*&---------------------------------------------------------------------*
*&  Include           ZXTRVU03
*&---------------------------------------------------------------------*
DATA: w_bukrs TYPE lfb1-bukrs,
      w_lifnr TYPE lfb1-lifnr,
      w_belnr TYPE bsis-belnr,
      w_lifnr2 TYPE bsak-lifnr,
      w_lifnr3 TYPE bsik-lifnr,
      w_lifnr4 TYPE bsik-lifnr.

*** Rollout - Eduardo Ruttkowski Tavares 03.11.2009 >>> INICIO
*** Valida se o usuário tem autorização para Aprovar

IF sy-tcode = 'TRIP' OR
   sy-tcode = 'TP04' OR
   sy-tcode = 'PR05'.
  FIELD-SYMBOLS: <code> TYPE ANY.
  IF sy-tcode = 'PR05'.
    ASSIGN ('(SAPMP56T)sa-code') TO <code>.
  ELSE.
    ASSIGN ('(SAPMP56T)sa_code') TO <code>.
  ENDIF.
  IF sy-subrc IS INITIAL.
    IF <code> = 'APPR'.
      DATA: w_kostl TYPE zfit0008-kostl.
      LOOP AT konti.
        SELECT SINGLE kostl FROM zfit0008
          INTO w_kostl
          WHERE kostl = konti-kostl AND
                aprovador = sy-uname.
        IF sy-subrc <> 0.
          MESSAGE e398(00) WITH  'usuário' sy-uname
          'não tem autorização para aprovar o centro de custo'
          konti-kostl.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDIF.
*** Rollout - Eduardo Ruttkowski Tavares 03.11.2009  <<< FIM

IF beleg[] IS INITIAL.

  CHECK sy-tcode = 'TRIP' OR
        sy-tcode = 'TP04' OR
        sy-tcode = 'PR05'.

  LOOP AT vsch.

    SELECT SINGLE bukrs
      FROM lfb1
    INTO w_bukrs
      WHERE pernr = trip_header-pernr.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE lifnr
        FROM lfb1
        INTO w_lifnr
        WHERE bukrs = w_bukrs AND
              pernr = trip_header-pernr.

      SELECT belnr
        FROM bsis
        INTO w_belnr
        WHERE bukrs = w_bukrs
        AND hkont = '0000113124'.

        CHECK sy-subrc IS INITIAL.

        SELECT SINGLE lifnr
        FROM bsik
        INTO w_lifnr2
        WHERE bukrs = w_bukrs
        AND belnr = w_belnr.

        IF w_lifnr2 EQ w_lifnr.
**          MESSAGE (colocR NOTICIrio
          MESSAGE e398(00) WITH  'Existem partidas em aberto para o fornecedor '
                                   w_lifnr
                                   ', procurar área de Liquidação'.
        ENDIF.

        SELECT SINGLE lifnr
        FROM bsak
        INTO w_lifnr2
        WHERE bukrs = w_bukrs
        AND belnr = w_belnr.

        IF w_lifnr2 EQ w_lifnr.
**          MESSAGE (colocR NOTICIrio
          MESSAGE e398(00) WITH  'Existem partidas em aberto para o fornecedor '
                                   w_lifnr
                                   ', procurar área de Liquidação'.
        ENDIF.
      ENDSELECT.
      SELECT SINGLE lifnr
      FROM bsik
      INTO w_lifnr3
      WHERE bukrs = w_bukrs
      AND lifnr = w_lifnr
      AND umskz NE space.

      IF w_lifnr3 EQ w_lifnr.
        MESSAGE e398(00) WITH  'Existem partidas em aberto para o fornecedor '
                                 w_lifnr
                                 ', procurar área de Liquidação'.
      ENDIF.



      SELECT SINGLE lifnr
        FROM bsik
        INTO w_lifnr4
        WHERE bukrs = w_bukrs
*Início Alteração Ricardo Furst.
        AND lifnr = w_lifnr
*Fim Alteração Ricardo Furst.
*      AND belnr = w_belnr
        AND umskz = 'L'.


      IF sy-subrc EQ 0.
        MESSAGE e398(00) WITH  'Existem partidas em aberto para o fornecedor '
                                 w_lifnr
                                 ', procurar área de Liquidação'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA: BEGIN OF t_data OCCURS 0.
          INCLUDE STRUCTURE  rke_dat.
  DATA: END OF t_data.

  DATA: w_date TYPE datum.
  w_date = trip_period-pdatv - 20.
  LOOP AT vsch.

    IF NOT vsch-datvs IS INITIAL.

      IF vsch-datvs < sy-datum.
        MESSAGE e398(00) WITH 'Data de soliticação de adiantamento'
                              ' no passado não permitido'.
      ENDIF.

      CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
        EXPORTING
          i_datab  = w_date
          i_datbi  = trip_period-pdatv
          i_factid = 'BR'
        TABLES
          eth_dats = t_data.

      SORT t_data BY periodat DESCENDING.

      DO 2 TIMES.
        DELETE t_data INDEX 1.
      ENDDO.

      READ TABLE t_data INDEX 1.

      IF vsch-datvs GE t_data-periodat.
        MESSAGE e398(00) WITH 'Data do Adiantamento tem que ser 72'
                              ' horas antes da data inicio da viagem'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDIF.

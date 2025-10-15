"Name: \PR:SAPLMLSR\FO:SAVE\SE:BEGIN\EI
ENHANCEMENT 0 Z_ML81N_SAVE.
*
  DATA: v1_bsart         TYPE ekko-bsart,
        v1_bedat         TYPE ekko-bedat,
        v1_bukrs         TYPE ekko-bukrs,
        vl_lifnr         TYPE ekko-lifnr,
        vl_zibt_nfse_006 TYPE boolean,
        vl_zibt_nfse_007 TYPE boolean,
        wa_zmmt0066      TYPE zmmt0066,
        wl_essr          TYPE essr,
        wl_ekpo          TYPE ekpo,
        wl_mara          TYPE mara,
        wl_mlan          TYPE mlan,
        wl_sab_dom_fer   TYPE TABLE OF iscal_day WITH HEADER LINE.

  "Controle data de lançamento Fiscal
  DATA: vg_last_day  TYPE sy-datum,
        vg_first_day TYPE sy-datum.

*  if essr-budat ne sy-datum.
*     MESSAGE E398(00) WITH 'Data de Lançamento deve ser a Atual!'.
*  endif.

*   CONCATENATE SY-DATUM(6) '01' INTO VG_FIRST_DAY.
*    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
*      EXPORTING
*        I_DATE = VG_FIRST_DAY
*      IMPORTING
*        E_DATE = VG_LAST_DAY.
*
*    REFRESH WL_SAB_DOM_FER.
*    CALL FUNCTION 'HOLIDAY_GET'
*      EXPORTING
*        FACTORY_CALENDAR           = 'ZF'
*        DATE_FROM                  = SY-DATUM
*        DATE_TO                    = VG_LAST_DAY
*      TABLES
*        HOLIDAYS                   = WL_SAB_DOM_FER
*      EXCEPTIONS
*        FACTORY_CALENDAR_NOT_FOUND = 1
*        HOLIDAY_CALENDAR_NOT_FOUND = 2
*        DATE_HAS_INVALID_FORMAT    = 3
*        DATE_INCONSISTENCY         = 4
*        OTHERS                     = 5.
*
*    READ TABLE WL_SAB_DOM_FER WITH KEY DATE = SY-DATUM.
*    IF SY-SUBRC = 0.
*      MESSAGE E398(00) WITH 'Lançamento não é dia útil'.
*    ELSE.
*      "Verifica ultimo dia útil
*      DO.
*        READ TABLE WL_SAB_DOM_FER WITH KEY DATE = VG_LAST_DAY.
*        IF SY-SUBRC NE 0.
*          EXIT.
*        ENDIF.
*        SUBTRACT 1 FROM VG_LAST_DAY.
*      ENDDO.
*      IF essr-budat EQ VG_LAST_DAY.
*        SELECT SINGLE *
*          FROM ZMMT0066
*          INTO WA_ZMMT0066
*          WHERE USNAM = SY-UNAME.
*        IF SY-SUBRC NE 0.
*            MESSAGE E398(00) WITH 'Lançamento é ultimo dia útil, procurar Depto fiscal'.
*        ENDIF.
*      ENDIF.
*    ENDIF.

*-------------------------------------------------------------------------------------
*-US 143691-09-07-2024-#143691-PANF-inicio
* impedir lançamentos em duplicidade.

  SELECT COUNT(*)
     FROM zibt_nfse_006.
  IF sy-subrc = 0.
    vl_zibt_nfse_006 = abap_true.
  ENDIF.

  SELECT COUNT(*)
     FROM zibt_nfse_007.
  IF sy-subrc = 0.
    vl_zibt_nfse_007 = abap_true.
  ENDIF.

  IF vl_zibt_nfse_006 = abap_false AND
     vl_zibt_nfse_007 = abap_false.
    "Nao realiza bloqueio
  ELSE.

    SELECT SINGLE bukrs, lifnr
     FROM ekko
     INTO (@v1_bukrs,@vl_lifnr)
     WHERE ebeln = @essr-ebeln.

    SELECT SINGLE werks
       FROM ekpo
       INTO @DATA(lv_werks)
       WHERE ebeln = @essr-ebeln.

    IF sy-tcode = 'ML81N'.

      SELECT COUNT(*)
      FROM zibt_nfse_007
      WHERE bukrs = @v1_bukrs
        AND werks = @lv_werks
        AND lifnr = @vl_lifnr.
      IF sy-subrc = 0.

      ELSE.

        SELECT COUNT(*)
        FROM zibt_nfse_006
        WHERE bukrs = @v1_bukrs
          AND werks = @lv_werks.
        IF sy-subrc = 0.
          MESSAGE e398(00) WITH 'Lçto. de nota de serviço é somente pela ZFIS63'.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.
*-US 143691-09-07-2024-#143691-PANF-fim
*-------------------------------------------------------------------------------------


  "IR147486 ALRS
  IF essr-lblni IS NOT INITIAL.
    SELECT SINGLE *
      FROM essr
      INTO wl_essr
      WHERE lblni = essr-lblni.

    IF sy-subrc NE 0.
      IF essr-bldat IS INITIAL.
        MESSAGE e398(00) WITH 'Informar a data do documento, na ABA Dados de Aceite.'.
      ENDIF.

*      "CS2016000714
*        IF ESSR-XBLNR is INITIAL.
*            MESSAGE E398(00) WITH 'Informar a referencia do documento.'.
*        ENDIF.

      IF essr-lblne IS INITIAL.
        MESSAGE e398(00) WITH 'Informar o número da Nota Fiscal no campo Nº Externo da aba Dados Básicos.'.
      ENDIF.

      SELECT SINGLE bukrs bsart bedat
       FROM ekko
        INTO (v1_bukrs,v1_bsart,v1_bedat)
       WHERE  ebeln EQ essr-ebeln.

      IF   v1_bsart EQ 'ZNB'.
        IF essr-bldat LT v1_bedat.
          MESSAGE e398(00) WITH 'Esta folha não pode ser lançada ' v1_bsart
                                'neste tipo de Pedido, lançar em'
                                'pedido  Regulariza/Outros.'.
        ENDIF.
      ENDIF.

      IF v1_bukrs NE '0100'.
        "Pedidos com material e serviço somente
        SELECT SINGLE *
          FROM ekpo
          INTO wl_ekpo
          WHERE ebeln EQ essr-ebeln
          AND   ebelp EQ essr-ebelp.

        IF wl_ekpo-matnr IS INITIAL.
          MESSAGE e398(00) WITH 'Esta folha não pode ser lançada '
                                 'o pedido deve ser para material/serviço'.
        ELSE.
          SELECT SINGLE *
          FROM mara
          INTO wl_mara
          WHERE matnr EQ wl_ekpo-matnr.
          IF wl_mara-mtart NE 'ZDIE'.
            MESSAGE e398(00) WITH 'O material associado ao serviço '
                                'deve ser do tipo PRESTAÇÃO DE SERVIÇOS (ZDIE)'.
          ELSE.
            SELECT SINGLE land1
                FROM t001
                INTO @DATA(vland)
                WHERE bukrs =  @v1_bukrs .
            SELECT SINGLE *
              FROM mlan
              INTO wl_mlan
              WHERE matnr EQ wl_ekpo-matnr
              AND   aland EQ vland.

            IF wl_mlan-taxm1 NE '2'.
              MESSAGE e398(00) WITH 'O material associado ao serviço '
                                  'deve ter classificação fiscal 2 (ISS) '.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

ENDENHANCEMENT.

*&--------------------------------------------------------------------&*
*& Report Name    : Validação das Ordens serviços GEO                 *&
*& Author         : Victor Hugo                                       *&
*& Date           : 03.01.2013                                        *&
*& Funcional Area : CO                                                *&
*&--------------------------------------------------------------------&*
REPORT  zcor14.

*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: coep, zmmt0029.

TYPES: BEGIN OF ty_coep,
         bukrs     TYPE coep-bukrs,
         gjahr     TYPE coep-gjahr,
         perio     TYPE coep-perio,
         kstar     TYPE coep-kstar,
         objnr     TYPE coep-objnr,
         ebeln     TYPE coep-ebeln,
         ebelp     TYPE coep-ebelp,
         matnr     TYPE coep-matnr,
         mbgbtr    TYPE coep-mbgbtr,
         mbfbtr    TYPE coep-mbfbtr,
         wtgbtr    TYPE coep-wtgbtr,
         wkgbtr    TYPE coep-wkgbtr,
         beknz     TYPE coep-beknz,
         rbest     TYPE coep-rbest,
         belnr     TYPE coep-belnr,
         buzei     TYPE coep-buzei,
         werks     TYPE coep-werks,
         objnr_aux TYPE zmmt0029-kostl,

       END OF ty_coep,

       BEGIN OF ty_zmmt0029,
         sakto TYPE zmmt0029-sakto,
         kostl TYPE zmmt0029-kostl,
       END OF ty_zmmt0029.

*&--------------------------------------------------------------------&*
*&  ESTRUTURA
*&--------------------------------------------------------------------&*
DATA: wa_return LIKE zmme_return_ordem_servico,
      it_return LIKE STANDARD TABLE OF wa_return.
*&--------------------------------------------------------------------&*
*& Internal Table
*&--------------------------------------------------------------------&*
DATA: it_coep     TYPE TABLE OF ty_coep,
      it_coep_aux TYPE TABLE OF ty_coep,
      it_zmmt0029 TYPE TABLE OF ty_zmmt0029.

*&--------------------------------------------------------------------&*
*& WORK AREA
*&--------------------------------------------------------------------&*
DATA: wa_coep     TYPE ty_coep,
      wa_coep_aux TYPE ty_coep,
      wa_zmmt0029 TYPE ty_zmmt0029,
      wa_ekpo     TYPE ekpo,
      wa_makt     TYPE makt,
      wa_zmmt0030 TYPE zmmt0030,
      wa_setleaf  TYPE setleaf.

DATA: v_rbest   TYPE co_rbest,
      belnr_ano TYPE c LENGTH 20,
      material  TYPE c LENGTH 10,
      item      TYPE c LENGTH 3.


*&--------------------------------------------------------------------&*
*& PARAMETROS
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR coep-bukrs NO INTERVALS OBLIGATORY NO-EXTENSION,
                  p_gjahr FOR coep-gjahr NO INTERVALS OBLIGATORY NO-EXTENSION,
                  p_perio FOR coep-perio NO INTERVALS OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK a.

START-OF-SELECTION.


  CALL FUNCTION 'CONVERSION_EXIT_REFBT_INPUT'
    EXPORTING
      input  = 'PED.'
    IMPORTING
      output = v_rbest.

  SELECT bukrs gjahr perio kstar objnr ebeln ebelp matnr mbgbtr mbfbtr wtgbtr wkgbtr beknz rbest belnr buzei kstar werks
    INTO TABLE it_coep_aux
    FROM coep
  WHERE bukrs IN p_bukrs
    AND gjahr IN p_gjahr
    AND perio IN p_perio
    AND rbest EQ v_rbest.

  LOOP AT it_coep_aux INTO wa_coep_aux.

    SELECT SINGLE * FROM setleaf
      INTO wa_setleaf
    WHERE setname EQ 'MAGGI_CENTROS_MODULO_PM'
      AND valfrom EQ wa_coep_aux-werks.

    IF ( sy-subrc EQ 0 ).
      CONTINUE.
    ELSE.


      wa_coep-bukrs      = wa_coep_aux-bukrs.
      wa_coep-gjahr      = wa_coep_aux-gjahr.
      wa_coep-perio      = wa_coep_aux-perio.
      wa_coep-kstar      = wa_coep_aux-kstar.
      wa_coep-objnr_aux  = wa_coep_aux-objnr+6(16).
      wa_coep-ebeln      = wa_coep_aux-ebeln.
      wa_coep-ebelp      = wa_coep_aux-ebelp.
      wa_coep-matnr      = wa_coep_aux-matnr.
      wa_coep-mbgbtr     = wa_coep_aux-mbgbtr.
      wa_coep-mbfbtr     = wa_coep_aux-mbfbtr.
      wa_coep-wtgbtr     = wa_coep_aux-wtgbtr.
      wa_coep-wkgbtr     = wa_coep_aux-wkgbtr.
      wa_coep-beknz      = wa_coep_aux-beknz.
      wa_coep-rbest      = wa_coep_aux-rbest.
      wa_coep-belnr      = wa_coep_aux-belnr.
      wa_coep-buzei      = wa_coep_aux-buzei.
      wa_coep-kstar      = wa_coep_aux-kstar.

      APPEND wa_coep TO it_coep.

    ENDIF.


  ENDLOOP.

  CHECK NOT it_coep[] IS INITIAL.

  SELECT sakto kostl
    INTO TABLE it_zmmt0029
    FROM zmmt0029
    FOR ALL ENTRIES IN it_coep
  WHERE sakto EQ it_coep-kstar
    AND kostl EQ it_coep-objnr_aux.

  CHECK NOT it_zmmt0029[] IS INITIAL.

  CLEAR: wa_coep.

  LOOP AT it_coep INTO wa_coep.


    READ TABLE it_zmmt0029 INTO wa_zmmt0029 WITH KEY sakto = wa_coep-kstar
                                                     kostl = wa_coep-objnr_aux.

    IF ( sy-subrc EQ 0 ).

      SELECT SINGLE * FROM makt INTO wa_makt WHERE matnr = wa_coep-matnr.
      wa_return-maktx     = wa_makt-maktx.


      CLEAR: belnr_ano.
      IF NOT ( wa_makt-matnr IS INITIAL ).
        CONCATENATE wa_coep-belnr wa_coep-buzei sy-datum(4) INTO belnr_ano.
      ELSE.


        CONCATENATE wa_coep-belnr wa_coep-buzei wa_coep-ebelp+1(4) INTO belnr_ano.

      ENDIF.

      wa_zmmt0030-docmat = belnr_ano.

      SELECT SINGLE * FROM ekpo INTO wa_ekpo WHERE ebeln = wa_coep-ebeln
                                               AND ebelp = wa_coep-ebelp.
      "filtra doc.de compra para ordem PM 08.05.2018
      IF wa_ekpo-knttp = 'F'.
        SELECT SINGLE *
          FROM ekkn
          INTO @DATA(_ekkn)
          WHERE ebeln = @wa_coep-ebeln
          AND   ebelp = @wa_coep-ebelp.
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM caufv
            INTO @DATA(_caufv)
            WHERE aufnr = @_ekkn-aufnr.
          IF _caufv-autyp = 30. "ordens de PM
            CLEAR: wa_return, wa_makt, wa_zmmt0030, wa_coep, belnr_ano.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      "
      wa_return-no_os_number = wa_ekpo-bednr.
      wa_return-aufnr     = ''.
      wa_return-kostl     = wa_coep-objnr_aux.
      wa_return-erdat     = sy-datum.
      wa_return-erzet     = sy-uzeit.
      wa_return-docmat    = belnr_ano.
      wa_return-buzei     = wa_coep-buzei.
      wa_return-hkont     = wa_coep-kstar.

      IF NOT ( wa_makt-matnr IS INITIAL ).

        CONCATENATE '000' wa_ekpo-matkl wa_makt-matnr+9(9) INTO wa_zmmt0030-matnr.
        wa_return-matnr = wa_zmmt0030-matnr.

      ELSE.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_coep-ebelp
          IMPORTING
            output = item.

        CONCATENATE '000' wa_coep-kstar item INTO wa_zmmt0030-matnr.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_zmmt0030-matnr
          IMPORTING
            output = wa_return-matnr.

        CLEAR: wa_zmmt0030-matnr.

        wa_zmmt0030-matnr = wa_return-matnr.

      ENDIF.

      SELECT SINGLE * FROM zmmt0030 INTO wa_zmmt0030 WHERE docmat EQ belnr_ano
                                                       AND matnr  EQ wa_return-matnr.

      IF ( sy-subrc EQ 0 ).
        CLEAR: wa_return, wa_makt, wa_zmmt0030, wa_coep, belnr_ano.
        CONTINUE.
      ELSE.

        INSERT INTO zmmt0030 VALUES wa_zmmt0030.

      ENDIF.

      IF ( wa_coep-mbgbtr < 0 ).
        wa_return-menge     = wa_coep-mbgbtr * -1.
      ELSEIF ( wa_coep-mbgbtr > 0 ).
        wa_return-menge     = wa_coep-mbgbtr.
      ELSE.
        wa_coep-mbgbtr  = 1.
        wa_return-menge = wa_coep-mbgbtr.
      ENDIF.

      IF ( wa_coep-wtgbtr < 0 ).
        wa_return-dmbtr     = wa_coep-wtgbtr * -1.
      ELSE.
        wa_return-dmbtr     = wa_coep-wtgbtr.
      ENDIF.

      IF ( wa_coep-wkgbtr < 0 ).
        wa_return-dmbe2     = wa_coep-wkgbtr * -1.
      ELSE.
        wa_return-dmbe2     = wa_coep-wkgbtr.
      ENDIF.




      wa_return-dmbe3     = ''.

      IF ( wa_makt-maktx IS INITIAL ).
        wa_return-maktx   = wa_ekpo-txz01.
      ENDIF.

      CASE wa_coep-beknz.
        WHEN: 'S'.
          wa_return-shkzg = '1'.
        WHEN: 'H'.
          wa_return-shkzg = '2'.
      ENDCASE.


      APPEND wa_return TO it_return.

      CLEAR: wa_return, wa_makt, wa_zmmt0030, wa_coep, belnr_ano.
    ENDIF.

  ENDLOOP.


  IF NOT ( it_return[] IS INITIAL ).
    PERFORM: call_rfc.
  ELSE.
    MESSAGE s000(z01) WITH 'Registro(s) não encontrados! '.
  ENDIF.

*&---------------------------------------------------------------------*
*&       text
*&---------------------------------------------------------------------*
FORM call_rfc .

  DATA: qtd TYPE sy-tabix.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_MM_OUTBOUND_ORDEM_SERVICO' IN BACKGROUND TASK
*    DESTINATION 'XI_GEO_ORDEM_SERVICO'
*    AS SEPARATE UNIT
*    TABLES
*      return_sucess = it_return.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_MM_OUTBOUND_ORDEM_SERVICO'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        return_sucess = it_return.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        return_sucess = it_return.
  ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  COMMIT WORK.

  CLEAR: qtd.
  DESCRIBE TABLE it_return LINES qtd.


  IF sy-subrc EQ 0.
    MESSAGE s000(z01) WITH qtd 'Registro(s) Gravados com Sucesso. '.
  ENDIF.


ENDFORM.                    " CALL_RFC

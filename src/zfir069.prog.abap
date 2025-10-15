*&---------------------------------------------------------------------*
*& Report  ZFIR069
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir069.

TABLES: bsis,sscrfields.

DATA: it_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_msg TYPE bdcmsgcoll,

      it_dta TYPE STANDARD TABLE OF bdcdata,
      wa_dta LIKE LINE OF it_dta,

      BEGIN OF it_msgtext OCCURS 0,
        texto TYPE t100-text,
      END OF it_msgtext.

DATA: gv_begda(10)  TYPE c,
      gv_ennda(10)  TYPE c,
      gv_event_low  TYPE hrpadbr_efd_event_type,
      gv_event_high TYPE hrpadbr_efd_event_type,
      gv_bukrs      TYPE bukrs,
      gv_mes        TYPE num2,
      gv_ano        TYPE num4,
      gv_logdet     TYPE c,
      gv_prod       TYPE c.

DATA: tela(4) TYPE c VALUE '0102',
      aut     TYPE c.



SELECTION-SCREEN BEGIN OF SCREEN 0102 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS:  p_bukrs FOR bsis-bukrs NO INTERVALS NO-EXTENSION,
                 p_mes   FOR bsis-monat NO INTERVALS NO-EXTENSION,
                 p_ano   FOR bsis-gjahr NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0102.



INITIALIZATION.

START-OF-SELECTION.

  CALL SCREEN 0100.

END-OF-SELECTION.

FORM f_bdc_data USING p_program p_dynpro p_start p_fnam p_fval.
  CLEAR wa_dta.
  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_start.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.

FORM f_call_transaction USING p_trans
                              p_mode
                              p_upd.

  REFRESH: it_msg, it_msgtext.

  CALL TRANSACTION p_trans USING it_dta
        MODE p_mode
        MESSAGES INTO it_msg
        UPDATE p_upd.

  IF it_msg[] IS NOT INITIAL.
    SELECT text
      FROM t100
      INTO TABLE it_msgtext
      FOR ALL ENTRIES IN it_msg
     WHERE arbgb  = it_msg-msgid AND
           msgnr  = it_msg-msgnr AND
           sprsl  = sy-langu.

    LOOP AT it_msgtext.
      TRANSLATE it_msgtext-texto USING '& '.
      CONDENSE it_msgtext-texto.
      MODIFY it_msgtext.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: dataini  TYPE sy-datum,
        datafim  TYPE sy-datum,
        msg(100) TYPE c.

  DATA: it_rsparams   TYPE TABLE OF rsparams,
        wa_rsparams   TYPE  rsparams,
        tl_parametros TYPE ustyp_t_parameters,
        wl_parametros TYPE ustyp_parameters.
  TYPES: ty_rg_budat TYPE RANGE OF bkpf-budat.



  CLEAR: gv_begda, gv_ennda,  gv_event_low,  gv_bukrs , dataini, datafim.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_MANU'.

      CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
        EXPORTING
          user_name           = sy-uname
        TABLES
          user_parameters     = tl_parametros
        EXCEPTIONS
          user_name_not_exist = 1
          OTHERS              = 2.

      READ TABLE tl_parametros INTO wl_parametros  WITH KEY parid = 'ZFIS51_ES_MD'.
      IF sy-subrc NE 0.
        CONCATENATE  'Falta permissão para o parâmetro "ZFIS51_ES_MD" para usuário ' sy-uname INTO msg SEPARATED BY space.
        MESSAGE msg TYPE 'I'.
      ELSE.

        IF p_bukrs[] IS INITIAL.
          MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
          CLEAR sy-ucomm.
        ELSEIF p_mes[] IS INITIAL.
          MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
          CLEAR sy-ucomm.
        ELSEIF p_ano[] IS INITIAL.
          MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
          CLEAR sy-ucomm.
        ELSE.
          CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

          CALL FUNCTION 'LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = dataini
            IMPORTING
              last_day_of_month = datafim.

          gv_begda = dataini.
          gv_ennda = datafim.

          CLEAR: it_rsparams[].

          wa_rsparams-selname = 'P_BUKRS'.
          wa_rsparams-kind    = 'S'.
          wa_rsparams-sign    = 'I'.
          wa_rsparams-option  = 'EQ'.
          wa_rsparams-low     = p_bukrs-low.
          wa_rsparams-high    = ' '.
          APPEND wa_rsparams TO it_rsparams.


          wa_rsparams-selname = 'P_BUDAT'.
          wa_rsparams-kind    = 'S'.
          wa_rsparams-sign    = 'I'.
          wa_rsparams-option  = 'BT'.
          wa_rsparams-low     = gv_begda.
          wa_rsparams-high    = gv_ennda.
          APPEND wa_rsparams TO it_rsparams.

          SUBMIT zfir070 WITH SELECTION-TABLE it_rsparams AND RETURN.
        ENDIF.

      ENDIF.
    WHEN 'BTN_GF'.

      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.

        CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = dataini
          IMPORTING
            last_day_of_month = datafim.

        CONCATENATE dataini+6(2) '.' dataini+4(2) '.' dataini+0(4) INTO gv_begda.
        CONCATENATE datafim+6(2) '.' datafim+4(2) '.' datafim+0(4) INTO gv_ennda.

        gv_bukrs     = p_bukrs-low.
        CLEAR it_dta[].
        PERFORM f_bdc_data USING:
               ''                  ''       'T'     'ZFIS49'         '',
               'ZFIR066'           '1000'   'X'     ''               '',
               ''                  ''       ''      'P_BUKRS-low'    gv_bukrs,
               ''                  ''       ''      'P_DATA-low'     gv_begda,
               ''                  ''       ''      'P_DATA-HIGH'    gv_ennda.


        PERFORM f_call_transaction USING 'ZFIS49'
                                         'E'
                                         'S'.
      ENDIF.

    WHEN 'BTN_REL'.

      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.
        CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = dataini
          IMPORTING
            last_day_of_month = datafim.

        CONCATENATE dataini+6(2) '.' dataini+4(2) '.' dataini+0(4) INTO gv_begda.
        CONCATENATE datafim+6(2) '.' datafim+4(2) '.' datafim+0(4) INTO gv_ennda.

        gv_bukrs     = p_bukrs-low.
        CLEAR it_dta[].
        PERFORM f_bdc_data USING:
               ''                  ''       'T'     'ZFIS48'             '',
               'ZFIR0079'          '1000'   'X'     ''                   '',
               ''                  ''       ''      'P_BUKRS-low'        gv_bukrs,
               ''                  ''       ''      'P_BRANCH-low'       '',
               ''                  ''       ''      'P_BUDAT-low'        gv_begda,
               ''                  ''       ''      'P_BUDAT-high'       gv_ennda,
               ''                  ''       ''      'P_BLDAT-low'        '',
               ''                  ''       ''      'P_LIFNR-low'        ''.

        PERFORM f_call_transaction USING 'ZFIS48'
                                         'E'
                                         'S'.
      ENDIF.
    WHEN 'BTN_EVENTOS'.
      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.

        CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = dataini
          IMPORTING
            last_day_of_month = datafim.

        CONCATENATE dataini+6(2) '.' dataini+4(2) '.' dataini+0(4) INTO gv_begda.
        CONCATENATE datafim+6(2) '.' datafim+4(2) '.' datafim+0(4) INTO gv_ennda.

        gv_event_low = 8050.
        gv_bukrs     = p_bukrs-low.

        CLEAR it_dta[].
        PERFORM f_bdc_data USING:
               ''                                 ''       'T'     'PC00_M37_EFD_PYERGEN'  '',
               'RPC_PAYBR_EFD_PAY_ER_GENERATOR'   '1000'   'X'     ''                      '',
               ''                                 ''       ''      's_bukrs-low'           gv_bukrs,
               ''                                 ''       ''      'p_begda'               gv_begda,
               ''                                 ''       ''      'p_endda'               gv_ennda,
               ''                                 ''       ''      's_events-low'          gv_event_low,
               ''                                 ''       ''      'p_logdet'              gv_logdet,
               ''                                 ''       ''      'p_prod'                gv_prod.

        PERFORM f_call_transaction USING 'PC00_M37_EFD_PYERGEN'
                                         'E'
                                         'S'.
      ENDIF.
    WHEN 'BTN_MANU02'.
      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.

        CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = dataini
          IMPORTING
            last_day_of_month = datafim.

        CONCATENATE dataini+6(2) '.' dataini+4(2) '.' dataini+0(4) INTO gv_begda.
        CONCATENATE datafim+6(2) '.' datafim+4(2) '.' datafim+0(4) INTO gv_ennda.

        gv_event_low = 8050.
        gv_bukrs     = p_bukrs-low.

        CLEAR it_dta[].
        PERFORM f_bdc_data USING:
               ''                                 ''       'T'     'ZHRST_EFD_AUTONOMOS'  '',
               'SAPMZHRST_EFD_AUTONOMOS'          '9000'   'X'     ''                      '',
               ''                                 ''       ''      'v_begda'               gv_begda,
               ''                                 ''       ''      'v_endda'               gv_ennda.
        PERFORM f_call_transaction USING 'ZHRST_EFD_AUTONOMOS'
                                         'E'
                                         'S'.
      ENDIF.
    WHEN 'BTN_RELCON'.
      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.
        gv_mes   = p_mes-low.
        gv_bukrs = p_bukrs-low.
        gv_ano   = p_ano-low.

        CLEAR it_dta[].
        PERFORM f_bdc_data USING:
               ''                      ''       'T'     'ZFIS50'            '',
               'ZFIR068'               '1000'   'X'     ''                   '',
               ''                      ''       ''      'P_MES-LOW'          gv_mes,
               ''                      ''       ''      'P_ANO-LOW'          gv_ano,
               ''                      ''       ''      'P_BUKRS-LOW'        gv_bukrs,
               ''                      ''       ''      'P_LIFNR-LOW'        '',
               ''                      ''       ''      'P_BELNR-LOW'        ''.


        PERFORM f_call_transaction USING 'ZFIS50'
                                         'E'
                                         'S'.
      ENDIF.
    WHEN 'BTN_PRI'.

      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.
        gv_mes   = p_mes-low.
        gv_bukrs = p_bukrs-low.
        gv_ano   = p_ano-low.

        CLEAR it_dta[].
        PERFORM f_bdc_data USING:
               ''                      ''       'T'     'ZHRST_EFD_EXAUTO'   '',
               'ZHRST_EFD_EXAUTO'      '1000'   'X'     ''                   '',
               ''                      ''       ''      'p_mes'              gv_mes,
               ''                      ''       ''      'p_ano'              gv_ano,
               ''                      ''       ''      'so_bukrs-low'       gv_bukrs,
               ''                      ''       ''      'so_lifnr-low'       '',
               ''                      ''       ''      'so_belnr-low'       '',
               ''                      ''       ''      'p_test'             '',
               ''                      ''       ''      'p_del'              ''.

        PERFORM f_call_transaction USING 'ZHRST_EFD_EXAUTO'
                                         'E'
                                         'S'.
        aut  = 'X'.
      ENDIF.

    WHEN 'BTN_SEG'.
      IF p_bukrs[] IS INITIAL.
        MESSAGE 'Campo Empresa Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.

        CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = dataini
          IMPORTING
            last_day_of_month = datafim.

        IF aut  = 'X'.

          CONCATENATE dataini+6(2) '.' dataini+4(2) '.' dataini+0(4) INTO gv_begda.
          CONCATENATE datafim+6(2) '.' datafim+4(2) '.' datafim+0(4) INTO gv_ennda.
          gv_bukrs      = p_bukrs-low.
          gv_event_low  = 8900.
          gv_event_high = 8910.

          CLEAR it_dta[].
          PERFORM f_bdc_data USING:
                 ''                                 ''       'T'     'PC00_M37_EFD_PYERGEN'  '',
                 'RPC_PAYBR_EFD_PAY_ER_GENERATOR'   '1000'   'X'     ''                      '',
                 ''                                 ''       ''      's_bukrs-low'           gv_bukrs,
                 ''                                 ''       ''      'p_begda'               gv_begda,
                 ''                                 ''       ''      'p_endda'               gv_ennda,
                 ''                                 ''       ''      's_events-low'          gv_event_low,
                 ''                                 ''       ''      's_events-high'         gv_event_high,
                 ''                                 ''       ''      'p_logdet'              gv_logdet,
                 ''                                 ''       ''      'p_prod'                gv_prod.

          PERFORM f_call_transaction USING 'PC00_M37_EFD_PYERGEN'
                                           'E'
                                           'S'.
        ELSE.
          MESSAGE 'Favor Atualiza os Registros p/ Geração de Evento' TYPE 'I'.
          EXIT.
        ENDIF.

      ENDIF.

    WHEN 'BTN_TXT'.
      IF p_mes[] IS INITIAL.
        MESSAGE 'Campo Mês Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSEIF p_ano[] IS INITIAL.
        MESSAGE 'Campo Ano Obrigatório!' TYPE 'I'.
        CLEAR sy-ucomm.
      ELSE.

        CLEAR sy-ucomm.

        CONCATENATE p_ano-low  p_mes-low '01' INTO dataini.

        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = dataini
          IMPORTING
            last_day_of_month = datafim.

        CONCATENATE dataini+6(2) '.' dataini+4(2) '.' dataini+0(4) INTO gv_begda.
        CONCATENATE datafim+6(2) '.' datafim+4(2) '.' datafim+0(4) INTO gv_ennda.

        DATA(p_budat) = VALUE ty_rg_budat( ( sign = 'I' option = 'EQ' low = dataini high = datafim  ) ).


        SUBMIT zfir085
        WITH p_budat IN p_budat
        WITH p_bukrs IN p_bukrs AND RETURN.

      ENDIF.


  ENDCASE.
ENDMODULE.

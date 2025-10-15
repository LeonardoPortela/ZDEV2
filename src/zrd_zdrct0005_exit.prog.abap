*&---------------------------------------------------------------------*
*&  Include  ZRD_ZDRCT0005_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zdrct0005_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zdrct0005_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0005 TYPE zdrct0005.

  CLEAR: wl_zdrct0005.

  wl_zdrct0005-data_reg    = sy-datum.
  wl_zdrct0005-hora_reg    = sy-uzeit.
  wl_zdrct0005-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zdrct0005 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0005_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zdrct0005   TYPE zdrct0005_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zdrct0005.

  IF w_zdrct0005-regio   IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Região Incorreta!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zdrct0005-model   IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Modelo NF Incorreto!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zdrct0005 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0005_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0005    TYPE zdrct0005.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zdrct0005.

  wl_zdrct0005-data_reg    = sy-datum.
  wl_zdrct0005-hora_reg    = sy-uzeit.
  wl_zdrct0005-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zdrct0005 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0005_0004 CHANGING p_saida TYPE any.

  DATA: wl_zdrct0005  TYPE zdrct0005_out.

  CLEAR: wl_zdrct0005.

  MOVE-CORRESPONDING p_saida  TO wl_zdrct0005.

  MOVE-CORRESPONDING wl_zdrct0005 TO p_saida.

ENDFORM.

FORM f_exit_zdrct0005_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0005    TYPE zdrct0005.

  CLEAR: wl_zdrct0005.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zdrct0005.
  MOVE-CORRESPONDING wl_zdrct0005 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0005_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zdrct0005_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.


  IF p_ref_tabname = 'ZDRCT0005_OUT' AND
     p_field       = 'REGIO'.
    p_scrtext_l = 'Região'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0005_OUT' AND
     p_field       = 'MODEL'.
    p_scrtext_l = 'Modelo NF'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0005_OUT' AND
     p_field       = 'USER_REG'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0005_OUT' AND
     p_field       = 'DATA_REG'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0005_OUT' AND
     p_field       = 'HORA_REG'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zdrct0005_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

  APPEND 'Modificar'    TO it_excl_toolbar.

ENDFORM.


FORM f_exit_zdrct0005_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZDRCT0005'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zdrct0005_0017 USING p_tipo.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include  ZRD_ZDRCT0006_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zdrct0006_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zdrct0006_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0006 TYPE zdrct0006.

  CLEAR: wl_zdrct0006.

  wl_zdrct0006-data_reg    = sy-datum.
  wl_zdrct0006-hora_reg    = sy-uzeit.
  wl_zdrct0006-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zdrct0006 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0006_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zdrct0006   TYPE zdrct0006_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zdrct0006.

  IF w_zdrct0006-branch  IS INITIAL AND
     w_zdrct0006-usuario IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Preencher uma das Informações!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zdrct0006-branch IS NOT INITIAL.
    SELECT SINGLE *
             FROM j_1bbranch
             INTO @DATA(_branch)
            WHERE branch = @w_zdrct0006-branch.
    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Filial Incorreta!'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF w_zdrct0006-usuario IS NOT INITIAL.
    SELECT SINGLE *
             FROM usr02
             INTO @DATA(_usr02)
            WHERE bname = @w_zdrct0006-usuario.
    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Usuario Incorreto!'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING w_zdrct0006 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0006_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0006    TYPE zdrct0006.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zdrct0006.

  wl_zdrct0006-data_reg    = sy-datum.
  wl_zdrct0006-hora_reg    = sy-uzeit.
  wl_zdrct0006-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zdrct0006 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0006_0004 CHANGING p_saida TYPE any.

  DATA: wl_zdrct0006  TYPE zdrct0006_out.

  CLEAR: wl_zdrct0006.

  MOVE-CORRESPONDING p_saida  TO wl_zdrct0006.

  MOVE-CORRESPONDING wl_zdrct0006 TO p_saida.

ENDFORM.

FORM f_exit_zdrct0006_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0006    TYPE zdrct0006.

  CLEAR: wl_zdrct0006.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zdrct0006.
  MOVE-CORRESPONDING wl_zdrct0006 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0006_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zdrct0006_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZDRCT0006_OUT' AND
     p_field       = 'BRANCH'.
    p_scrtext_l = 'Local de Negócio'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0006_OUT' AND
     p_field       = 'USUARIO'.
    p_scrtext_l = 'Usuário sem Contingência MDF-e'.
    p_outputlen = 32.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0006_OUT' AND
     p_field       = 'USER_REG'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0006_OUT' AND
     p_field       = 'DATA_REG'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0006_OUT' AND
     p_field       = 'HORA_REG'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zdrct0006_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

  APPEND 'Modificar'    TO it_excl_toolbar.

ENDFORM.


FORM f_exit_zdrct0006_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZDRCT0006'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zdrct0006_0017 USING p_tipo.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

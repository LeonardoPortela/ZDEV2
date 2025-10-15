*&---------------------------------------------------------------------*
*&  Include  ZRD_ZLEST0244_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zlest0244_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zlest0244_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0244 TYPE zlest0244.

  CLEAR: wl_zlest0244.

  wl_zlest0244-data_reg    = sy-datum.
  wl_zlest0244-hora_reg    = sy-uzeit.
  wl_zlest0244-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zlest0244 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0244_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zlest0244   TYPE zlest0244_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0244.

  IF w_zlest0244-matkl IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Grupo de Mercadoria Incorreto!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t023
           INTO @DATA(_t023)
          WHERE matkl = @w_zlest0244-matkl.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Grupo de Mercadoria Incorreto!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zlest0244 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0244_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0244    TYPE zlest0244.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0244.

  wl_zlest0244-data_reg    = sy-datum.
  wl_zlest0244-hora_reg    = sy-uzeit.
  wl_zlest0244-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zlest0244 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0244_0004 CHANGING p_saida TYPE any.

  DATA: wl_zlest0244  TYPE zlest0244_out.

  CLEAR: wl_zlest0244.

  MOVE-CORRESPONDING p_saida  TO wl_zlest0244.

  SELECT SINGLE *
           FROM t023t
           INTO @DATA(_t023t)
          WHERE spras = @sy-langu
            AND matkl = @wl_zlest0244-matkl.

  IF sy-subrc = 0.
    wl_zlest0244-descricao = _t023t-wgbez60.
  ENDIF.

  MOVE-CORRESPONDING wl_zlest0244 TO p_saida.

ENDFORM.

FORM f_exit_zlest0244_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0244    TYPE zlest0244.

  CLEAR: wl_zlest0244.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0244.
  MOVE-CORRESPONDING wl_zlest0244 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0244_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zlest0244_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZLEST0244_OUT' AND
     p_field       = 'MATKL'.
    p_scrtext_l = 'Grupo Mercadoria'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0244_OUT' AND
     p_field       = 'DESCRICAO'.
    p_scrtext_l = 'Descrição'.
    p_outputlen = 60.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0244_OUT' AND
     p_field       = 'USER_REG'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0244_OUT' AND
     p_field       = 'DATA_REG'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0244_OUT' AND
     p_field       = 'HORA_REG'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0244_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

  APPEND 'Modificar'    TO it_excl_toolbar.

ENDFORM.


FORM f_exit_zlest0244_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0244'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zlest0244_0017 USING p_tipo.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

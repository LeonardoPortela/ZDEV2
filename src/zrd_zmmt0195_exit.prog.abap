*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0195_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0195_exit.

FORM f_exit_zmmt0195_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0195 TYPE zmmt0195.

  CLEAR: wl_zmmt0195.

  wl_zmmt0195-user_reg  = sy-uname.
  wl_zmmt0195-data_reg  = sy-datum.
  wl_zmmt0195-hora_reg  = sy-uzeit.

  MOVE-CORRESPONDING wl_zmmt0195 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0195_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zmmt0195 TYPE zmmt0195.
  CLEAR:  wa_zmmt0195.

  DATA: var_answer TYPE c.

  DATA: lit_zmmt0195 TYPE TABLE OF zmmt0195.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0195.

  CLEAR: p_error.

  IF wa_zmmt0195-nome_marca IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Nome da Marca é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0195_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zmmt0195 TYPE zmmt0195.
  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0195.

  wa_zmmt0195-data_reg = sy-datum.
  wa_zmmt0195-hora_reg = sy-uzeit.
  wa_zmmt0195-user_reg = sy-uname.

  MOVE-CORRESPONDING wa_zmmt0195 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0195_0008  CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZMMT0195_OUT' AND
     p_field       = 'NOME_MARCA'.
    p_scrtext_l    = 'Nome da Marca'.
    p_outputlen    = 50.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0195_OUT' AND
     p_field       = 'USER_REG'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0195_OUT' AND
     p_field       = 'DATA_REG'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0195_OUT' AND
     p_field       = 'HORA_REG'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 12.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0195_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZMMT0195'
      tabfirst = 'X'.

ENDFORM.

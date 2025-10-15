*&---------------------------------------------------------------------*
*&  Include  ZRD_ZLEST0230_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zdrct0004_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zdrct0004_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0004 TYPE zdrct0004.

  CLEAR: wl_zdrct0004.

  wl_zdrct0004-dt_registro = sy-datum.
  wl_zdrct0004-hr_registro = sy-uzeit.
  wl_zdrct0004-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zdrct0004 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0004_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.
ENDFORM.

FORM f_exit_zdrct0004_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zdrct0004    TYPE zdrct0004.

  CLEAR: wl_zdrct0004.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zdrct0004.

  wl_zdrct0004-dt_registro = sy-datum.
  wl_zdrct0004-hr_registro = sy-uzeit.
  wl_zdrct0004-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zdrct0004 TO p_registro_manter.

ENDFORM.

FORM f_exit_zdrct0004_0004 CHANGING p_saida TYPE any.
ENDFORM.

FORM f_exit_zdrct0004_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zdrct0004_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zdrct0004_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZDRCT0004_OUT' AND
     p_field       = 'CAMPO'.
    p_scrtext_l = 'Nome Campo JSON'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0004_OUT' AND
     p_field       = 'GERAR_QUANDO_NULO'.
    p_scrtext_l = 'Gerar XML quando Valor Nulo'.
    p_check     = abap_true.
    p_outputlen = 30.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0004_OUT' AND
     p_field       = 'VALOR_QUANDO_NULO'.
    p_scrtext_l = 'Valor Saída do Campo quando Nulo'.
    p_outputlen = 35.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0004_OUT' AND
     p_field       = 'DT_REGISTRO'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0004_OUT' AND
     p_field       = 'HR_REGISTRO'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZDRCT0004_OUT' AND
     p_field       = 'US_REGISTRO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

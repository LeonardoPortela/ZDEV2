*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0218_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0218_exit.

FORM f_exit_zlest0218_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0218 TYPE zlest0218.

  CLEAR: wl_zlest0218.

  wl_zlest0218-dt_registro = sy-datum.
  wl_zlest0218-hr_registro = sy-uzeit.
  wl_zlest0218-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0218 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0218_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zlest0218 TYPE zlest0218_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0218.

  CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
    EXPORTING
      input     = w_zlest0218-stcd1
    IMPORTING
      output    = w_zlest0218-stcd1
    EXCEPTIONS
      not_valid = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'CNPJ informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0218_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0218    TYPE zlest0218.

  CLEAR: wl_zlest0218.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0218.

  wl_zlest0218-dt_registro = sy-datum.
  wl_zlest0218-hr_registro = sy-uzeit.
  wl_zlest0218-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0218 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0218_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zlest0218_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zlest0218_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zlest0218_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZLEST0218_OUT' AND
     p_field       = 'STCD1'.
    p_scrtext_l = 'CNPJ Fornecedor'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0218_OUT' AND
     p_field       = 'US_REGISTRO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0218_OUT' AND
     p_field       = 'DT_REGISTRO'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0218_OUT' AND
     p_field       = 'HR_REGISTRO'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0218_0009  TABLES pt_excl_toolbar
                             USING p_db_tab.

  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  CHECK p_db_tab = 'ZLEST0218'.

  FREE: it_excl_toolbar.

  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar  TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

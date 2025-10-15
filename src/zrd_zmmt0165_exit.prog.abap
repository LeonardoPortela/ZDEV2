*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0165_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0165_exit.

FORM f_exit_zmmt0165_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0165 TYPE zmmt0165.

  CLEAR: wl_zmmt0165.

  wl_zmmt0165-dt_registro = sy-datum.
  wl_zmmt0165-hr_registro = sy-uzeit.
  wl_zmmt0165-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zmmt0165 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0165_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0165 TYPE zmmt0165.

  CLEAR: wl_zmmt0165.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0165.
*
*  CLEAR: p_error.
*
*  IF wl_zmmt0165-class_aval IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Classe Avaliação é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF wl_zmmt0165-area_aval IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Área Avaliação é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF wl_zmmt0165-lgort IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Depósito é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.

ENDFORM.

FORM f_exit_zmmt0165_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0165 TYPE zmmt0165.

  CLEAR: wl_zmmt0165.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0165.

  wl_zmmt0165-dt_registro = sy-datum.
  wl_zmmt0165-hr_registro = sy-uzeit.
  wl_zmmt0165-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zmmt0165 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0165_0004 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0165_out TYPE zmmt0165_out.

  CLEAR: wl_zmmt0165_out.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0165_out.

  MOVE-CORRESPONDING wl_zmmt0165_out TO p_saida.

ENDFORM.

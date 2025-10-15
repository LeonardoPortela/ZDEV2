*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0171_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0171_exit.

FIELD-SYMBOLS: <fs_wa_registro_manter> TYPE zmmt0171.


FORM f_exit_zmmt0171_0001 USING p_registro_manter TYPE any.

  DATA: wl_zmmt0171 TYPE zmmt0171.

  CLEAR: wl_zmmt0171.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0171.

  IF wl_zmmt0171-us_criacao IS INITIAL.
    wl_zmmt0171-dt_criacao      = sy-datum.
    wl_zmmt0171-hr_criacao      = sy-uzeit.
    wl_zmmt0171-us_criacao      = sy-uname.
  ENDIF.



  MOVE-CORRESPONDING wl_zmmt0171 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0171_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.



ENDFORM.


FORM f_exit_zmmt0171_0005 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0171 TYPE zmmt0171.

  CLEAR: wl_zmmt0171.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0171.

  IF wl_zmmt0171-us_criacao IS INITIAL.
    wl_zmmt0171-dt_criacao      = sy-datum.
    wl_zmmt0171-hr_criacao      = sy-uzeit.
    wl_zmmt0171-us_criacao      = sy-uname.
  ELSE.
    wl_zmmt0171-dt_modif      = sy-datum.
    wl_zmmt0171-hr_modif      = sy-uzeit.
    wl_zmmt0171-us_modif      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0171 TO p_saida.


ENDFORM.

FORM f_exit_zmmt0171_0010 TABLES t_saida.


ENDFORM.

FORM  f_exit_zmmt0171_0016 USING p_ucomm  TYPE sy-ucomm CHANGING p_registro_manter TYPE any p_saida TYPE any.



ENDFORM.

FORM f_exit_zmmt0171_0017 USING p_tipo.




ENDFORM.

*&---------------------------------------------------------------------*
*& Report ZRD_ZMMT0179_EXIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0179_exit.

FORM f_exit_zmmt0179_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0179 TYPE zmmt0179.

  CLEAR: wl_zmmt0179.
  wl_zmmt0179-data  = sy-datum.
  wl_zmmt0179-hora  = sy-uzeit.

  MOVE-CORRESPONDING wl_zmmt0179 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0179_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.


FORM f_exit_zmmt0179_0003 CHANGING p_saida TYPE any.



ENDFORM.


FORM f_exit_zmmt0179_0004 CHANGING p_saida TYPE any.

ENDFORM.


FORM f_exit_zmmt0179_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.


FORM  f_exit_zmmt0179_0009 TABLES it_excl_toolbar
                           USING p_db_tab.


ENDFORM.

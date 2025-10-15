*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0226_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0162_exit.

FORM f_exit_ZMMT0162_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_ZMMT0162 TYPE ZMMT0162.

  CLEAR: wl_ZMMT0162.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZMMT0162.

  IF sy-ucomm = 'NOVO'.
    wl_ZMMT0162-data = sy-datum.
    wl_ZMMT0162-hora = sy-uzeit.
    wl_ZMMT0162-usuario    = sy-uname.
  ENDIF.


  MOVE-CORRESPONDING wl_ZMMT0162 TO p_registro_manter.

ENDFORM.

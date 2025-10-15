*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0226_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0163_exit.

FORM f_exit_ZMMT0163_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_ZMMT0163 TYPE ZMMT0163.

  CLEAR: wl_ZMMT0163.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZMMT0163.

  IF sy-ucomm = 'NOVO'.
    wl_ZMMT0163-data = sy-datum.
    wl_ZMMT0163-hora = sy-uzeit.
    wl_ZMMT0163-usuario    = sy-uname.
  ENDIF.


  MOVE-CORRESPONDING wl_ZMMT0163 TO p_registro_manter.

ENDFORM.

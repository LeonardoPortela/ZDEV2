*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0226_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0161_exit.

FORM f_exit_zmmt0161_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0161 TYPE zmmt0161.

  CLEAR: wl_zmmt0161.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0161.

  IF sy-ucomm = 'NOVO'.
    wl_zmmt0161-dt_criacao = sy-datum.
    wl_zmmt0161-hr_criacao = sy-uzeit.
    wl_zmmt0161-usuario    = sy-uname.
  ENDIF.


  MOVE-CORRESPONDING wl_zmmt0161 TO p_registro_manter.

ENDFORM.

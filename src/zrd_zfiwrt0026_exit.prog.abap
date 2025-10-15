*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIWRT0026_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfiwrt0026_exit.

FORM f_exit_zfit0150_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfiwrt0026 TYPE zfiwrt0026.

  CLEAR: wl_zfiwrt0026.

  wl_zfiwrt0026-dt_criacao = sy-datum.
  wl_zfiwrt0026-hr_criacao = sy-uzeit.

  MOVE-CORRESPONDING  wl_zfiwrt0026 TO p_registro_manter.

ENDFORM.



FORM  f_exit_zfiwrt0026_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfiwrt0026 TYPE zfiwrt0026.

  CLEAR: wl_zfiwrt0026.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt0026.

  wl_zfiwrt0026-dt_criacao = sy-datum.
  wl_zfiwrt0026-hr_criacao = sy-uzeit.

  MOVE-CORRESPONDING wl_zfiwrt0026 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfiwrt0026_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfiwrt0026_out TYPE zfiwrt0026_out.

  CLEAR: wl_zfiwrt0026_out.

  MOVE-CORRESPONDING p_saida TO wl_zfiwrt0026_out.


  MOVE-CORRESPONDING wl_zfiwrt0026_out TO p_saida.

ENDFORM.

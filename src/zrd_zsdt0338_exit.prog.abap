*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0104_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0338_exit.


FORM f_exit_zsdt0338_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0338 TYPE zsdt0338.

  CLEAR: wl_zsdt0338.

  wl_zsdt0338-data = sy-datum.
  wl_zsdt0338-hora = sy-uzeit.
  wl_zsdt0338-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0338 TO p_registro_manter.


ENDFORM.

FORM f_exit_zsdt0338_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

*  DATA: wl_zsdt0338 TYPE zsdt0338.
*
*  CLEAR: wl_zsdt0338.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0338.
*
*  CLEAR: p_error.
*
*  IF wl_zsdt0338-id_lms IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Informar o ID do curso!' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF wl_zsdt0338-nome_curso IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Informar o nome do curso!' TYPE 'S'.
*    EXIT.
*  ENDIF.




ENDFORM.

FORM f_exit_zsdt0338_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0338 TYPE zsdt0338.

  CLEAR: wl_zsdt0338.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0338.

  wl_zsdt0338-data = sy-datum.
  wl_zsdt0338-hora = sy-uzeit.
  wl_zsdt0338-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0338 TO p_registro_manter.

ENDFORM.

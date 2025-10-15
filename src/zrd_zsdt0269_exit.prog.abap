*&---------------------------------------------------------------------*
*& Report zrd_zsdt0269_exit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0269_exit.


*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0269_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0269 TYPE zsdt0269.

  CLEAR: wl_zsdt0269.
  MOVE-CORRESPONDING wl_zsdt0269 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0269_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0269 TYPE zsdt0269.

  CLEAR: wl_zsdt0269.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0269.

  wl_zsdt0269-date_create     = sy-datum.
  wl_zsdt0269-user_create     = sy-uname.
  wl_zsdt0269-time_create     = sy-uzeit.

  MOVE-CORRESPONDING   wl_zsdt0269 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0269_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: wl_zsdt0269 TYPE zsdt0269.
  CLEAR: wl_zsdt0269.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0269.

ENDFORM.

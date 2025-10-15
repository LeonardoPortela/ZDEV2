*&---------------------------------------------------------------------*
*& Report zrd_zsdt0394_exit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0394_exit.


*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0394_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0394 TYPE zsdt0394.

  CLEAR: wl_zsdt0394.
  MOVE-CORRESPONDING wl_zsdt0394 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0394_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0394 TYPE zsdt0394.

  CLEAR: wl_zsdt0394.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0394.

  wl_zsdt0394-date_create     = sy-datum.
  wl_zsdt0394-user_create     = sy-uname.
  wl_zsdt0394-time_create     = sy-uzeit.

  MOVE-CORRESPONDING   wl_zsdt0394 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0394_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: wl_zsdt0394 TYPE zsdt0394.
  CLEAR: wl_zsdt0394.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0394.

ENDFORM.

FORM  f_exit_zsdt0394_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
**
*  TYPES: BEGIN OF ty_excl_toolbar,
*           code TYPE ui_func.
*  TYPES: END OF ty_excl_toolbar.
*
*  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
*        wa_excl_toolbar TYPE ty_excl_toolbar.
*
*  FREE: it_excl_toolbar.
*
*
*  wa_excl_toolbar-code = 'Modificar'.
*  APPEND wa_excl_toolbar  TO it_excl_toolbar.
*
*  pt_excl_toolbar[] = it_excl_toolbar[].
ENDFORM.

*&---------------------------------------------------------------------*
*& Report zrd_zmmt0217_exit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0217_exit.


*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zmmt0217_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0217 TYPE zmmt0217.

  CLEAR: wl_zmmt0217.
  MOVE-CORRESPONDING wl_zmmt0217 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zmmt0217_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0217 TYPE zmmt0217.

  CLEAR: wl_zmmt0217.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0217.

  wl_zmmt0217-date_create     = sy-datum.
  wl_zmmt0217-user_create     = sy-uname.
  wl_zmmt0217-time_create     = sy-uzeit.

  MOVE-CORRESPONDING   wl_zmmt0217 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zmmt0217_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: wl_zmmt0217 TYPE zmmt0217.
  CLEAR: wl_zmmt0217.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0217.

ENDFORM.

FORM  f_exit_zmmt0217_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
*
  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.


  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar  TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].
ENDFORM.

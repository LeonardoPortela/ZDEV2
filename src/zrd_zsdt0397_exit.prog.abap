*&---------------------------------------------------------------------*
*& Report zrd_zsdt0397_exit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0397_exit.


*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0397_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0397 TYPE zsdt0397.

  CLEAR: wl_zsdt0397.

  wl_zsdt0397-user_create = sy-uname.
  wl_zsdt0397-date_create = sy-datum.
  wl_zsdt0397-time_create = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0397 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0397_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0397 TYPE zsdt0397.

  CLEAR: wl_zsdt0397.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0397.

  wl_zsdt0397-user_change = sy-uname.
  wl_zsdt0397-date_change = sy-datum.
  wl_zsdt0397-time_change = sy-uzeit.

  MOVE-CORRESPONDING   wl_zsdt0397 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0397_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: wl_zsdt0397 TYPE zsdt0397.
  CLEAR: wl_zsdt0397.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0397.

ENDFORM.

FORM  f_exit_zsdt0397_0009 TABLES pt_excl_toolbar
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


FORM f_exit_zsdt0397_0016 USING p_ucomm TYPE sy-ucomm
                             CHANGING p_registro_manter TYPE any
                                      p_saida TYPE any.

  DATA: wl_397 TYPE zsdt0397.

  MOVE-CORRESPONDING p_registro_manter TO wl_397.

  SELECT SINGLE vtext
    FROM tspat
    WHERE spras = 'P'
     AND  spart = @wl_397-setor_ativ
    INTO @wl_397-setor_ativ_desc.

  IF sy-subrc <> 0.

    CLEAR wl_397-setor_ativ_desc.

  ENDIF.


  MOVE-CORRESPONDING wl_397 TO p_registro_manter.
  MOVE-CORRESPONDING wl_397 TO p_saida.


ENDFORM.

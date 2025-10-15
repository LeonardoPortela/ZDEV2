*&---------------------------------------------------------------------*
*& Report  ZRD_ZPPT0024_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0255_exit.

*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0255_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0255 TYPE zsdt0255.

  CLEAR: wl_zsdt0255.
  MOVE-CORRESPONDING wl_zsdt0255 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0255_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: wl_zsdt0255 TYPE zsdt0255_out.
  CLEAR: wl_zsdt0255.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0255.

*&---------------------------------------------------------------------*
* Operação
*&---------------------------------------------------------------------*
  IF wl_zsdt0255-operacao IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Operação é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

*&---------------------------------------------------------------------*
*Chave de lançamento
*&---------------------------------------------------------------------*
  IF wl_zsdt0255-ch_deb IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Chave de lançamento é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

*&---------------------------------------------------------------------*
*Conta Débito
*&---------------------------------------------------------------------*
  IF wl_zsdt0255-conta_deb IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Conta Débito é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

*&---------------------------------------------------------------------*
*Nº do material
*&---------------------------------------------------------------------*
  IF wl_zsdt0255-deb_material IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Nº do material é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zsdt0255-deb_material IS NOT INITIAL.

    SELECT SINGLE maktx FROM makt
    INTO ( wl_zsdt0255-maktx )
     WHERE matnr = wl_zsdt0255-deb_material.

    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Nº do material não encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0255 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0255_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0255 TYPE zsdt0255.

  CLEAR: wl_zsdt0255.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0255.

  wl_zsdt0255-dt_registro     = sy-datum.
  wl_zsdt0255-us_registro     = sy-uname.
  wl_zsdt0255-hr_registro     = sy-uzeit.

  MOVE-CORRESPONDING   wl_zsdt0255 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Preenche campos de leitura
*&---------------------------------------------------------------------*
FORM f_exit_zsdt0255_0004 CHANGING p_registro_manter TYPE any.


  DATA: wl_zsdt0255 TYPE zsdt0255_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0255.

**********************************************************************
* 110527 ZSDT0168  - Botão contabilização - PSA (Foi comentado o código abaixo pois ele alterava os dados da tabela)
**********************************************************************
*  wl_zsdt0255-dt_registro     = sy-datum.
*  wl_zsdt0255-us_registro     = sy-uname.
*  wl_zsdt0255-hr_registro     = sy-uzeit.

  IF wl_zsdt0255-deb_material IS NOT INITIAL.

    SELECT SINGLE maktx FROM makt
      INTO ( wl_zsdt0255-maktx )
      WHERE matnr = wl_zsdt0255-deb_material.

  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0255 TO p_registro_manter.

ENDFORM.

**********************************************************************
* 110527 ZSDT0168  - Remove notão Alterar pois o campo chave Produto Predominate tem que ser inserido ou excluido e não alterado!
**********************************************************************

FORM  f_exit_zsdt0255_0009 TABLES pt_excl_toolbar
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


FORM f_exit_zsdt0255_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'zsdt0255'
      tabfirst = 'X'.

ENDFORM.

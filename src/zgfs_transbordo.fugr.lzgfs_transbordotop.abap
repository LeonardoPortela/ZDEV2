FUNCTION-POOL zgfs_transbordo.              "MESSAGE-ID ..

*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

**********************************************************************
* TABELAS
**********************************************************************
TABLES: t001, zsdt0225, ekko, ekpo, sscrfields, rsvar.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_saida.
         INCLUDE STRUCTURE zsds077.
       TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ekko-ebeln,
         ebelp TYPE ekpo-ebelp,
         etenr TYPE eket-etenr,
         charg TYPE eket-charg,
         loekz TYPE ekpo-loekz,
         aedat TYPE ekpo-aedat,
         bukrs TYPE ekpo-bukrs,
         werks TYPE ekpo-werks,
         lgort TYPE ekpo-lgort,
         matnr TYPE ekpo-matnr,
         matkl TYPE ekpo-matkl,
         txz01 TYPE ekpo-txz01,
         menge TYPE ekpo-menge,
         meins TYPE ekpo-meins,
         lifnr TYPE ekko-lifnr,
         name1 TYPE lfa1-name1,
         unsez TYPE ekko-unsez.
TYPES: END   OF ty_ekpo.

TYPES: BEGIN OF ty_filial,
         werks TYPE ekpo-werks,
         lgort TYPE ekpo-lgort,
         matnr TYPE ekpo-matnr.
TYPES: END   OF ty_filial.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
*
      g_id_seq             TYPE numc10,
      t_ekpo               TYPE TABLE OF ty_ekpo,
      w_ekpo               TYPE ty_ekpo,
      t_zsdt0306           TYPE TABLE OF zsdt0306,
      w_zsdt0306           TYPE zsdt0306,
      t_zsdt0225           TYPE TABLE OF zsdt0225,
      w_zsdt0225           TYPE zsdt0225,
      t_saida              TYPE TABLE OF ty_saida,
      w_saida              TYPE ty_saida,
      t_selecao            TYPE TABLE OF ty_saida,
      w_selecao            TYPE ty_saida,
      t_filial             TYPE TABLE OF ty_filial,
      w_filial             TYPE ty_filial,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_color              TYPE lvc_t_scol,
      w_color              TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl    VALUE 'XX',
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      r_matkl_sel          TYPE RANGE OF matkl WITH HEADER LINE,
*
      l_qtd                TYPE i,
      l_tabix              TYPE sy-tabix,
* ---> S4 Migration - 19/06/2023 - JS
*      l_cockpit            TYPE char02,
      l_cockpit(02)        TYPE c,
* <--- S4 Migration - 19/06/2023 - JS
      l_erro               TYPE c,
      l_editar             TYPE c,
      l_id_seq             TYPE zsdt0225-id_seq,
      l_resp               TYPE c,
      ok_code              TYPE sy-ucomm,
      ok_code2             TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util,
*
      l_sel_button         TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 0101 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-201 FOR FIELD s_bukrs.
SELECT-OPTIONS:  s_bukrs    FOR ekpo-bukrs           MODIF ID g1 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN POSITION 71.
SELECTION-SCREEN COMMENT (24) text-204 FOR FIELD s_werks.
SELECT-OPTIONS:  s_werks    FOR ekpo-werks           MODIF ID g1 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-202 FOR FIELD s_ebeln.
SELECT-OPTIONS:  s_ebeln    FOR ekko-ebeln           MODIF ID g1 NO INTERVALS.
SELECTION-SCREEN POSITION 71.
SELECTION-SCREEN COMMENT (18) text-205 FOR FIELD s_lifnr.
SELECT-OPTIONS:  s_lifnr    FOR ekko-lifnr           MODIF ID g1 NO INTERVALS.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-203 FOR FIELD s_data.
SELECT-OPTIONS:  s_data     FOR ekpo-aedat           MODIF ID g1 NO-EXTENSION.
SELECTION-SCREEN POSITION 71.
SELECTION-SCREEN PUSHBUTTON (14) but1 USER-COMMAND selec.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END   OF BLOCK b1.
SELECTION-SCREEN END   OF SCREEN 0101.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  but1 = icon_execute_object && 'Selecionar'.

  LOOP AT SCREEN.
    IF screen-name = 'S_BUKRS-LOW'.
      screen-required = '2'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  CLEAR ok_code.

  SET PF-STATUS 'ZLESR0152'.
  SET TITLEBAR 'ZLESR0152'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'SELEC'.
      PERFORM f_get_set.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'SALVAR'.
      PERFORM f_salvar_dados CHANGING l_erro.

      IF l_erro = abap_false.
        CALL METHOD g_custom_container->free.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

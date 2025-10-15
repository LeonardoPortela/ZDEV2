*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*
REPORT zsdr0145.

**********************************************************************
* TABELAS
**********************************************************************
TABLES: t001, zsdt0225, ekko, ekpo, sscrfields, rsvar.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_saida.
         INCLUDE STRUCTURE zsds077.
"TYPES    operacao TYPE zsdt0225-operacao.
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

TYPES: BEGIN OF ty_lote,
         id_seq TYPE numc10,
         ebeln  TYPE ekko-ebeln,
         ebelp  TYPE ekpo-ebelp.
TYPES: END   OF ty_lote.

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
      t_ekpo               TYPE TABLE OF ty_ekpo,
      w_ekpo               TYPE ty_ekpo,
      t_zsdt0306           TYPE TABLE OF zsdt0306,
      w_zsdt0306           TYPE zsdt0306,
      t_zsdt0307           TYPE TABLE OF zsdt0307,
      w_zsdt0307           TYPE zsdt0307,
      t_zsdt0225           TYPE TABLE OF zsdt0225,
      w_zsdt0225           TYPE zsdt0225,
      t_0225               TYPE TABLE OF zsdt0225,
      w_0225               TYPE zsdt0225,
      t_0225_aux           TYPE TABLE OF zsdt0225,
      w_0225_aux           TYPE zsdt0225,
      t_saida              TYPE TABLE OF ty_saida,
      w_saida              TYPE ty_saida,
      t_saida_aux          TYPE TABLE OF ty_saida,
      w_saida_aux          TYPE ty_saida,
      t_lote               TYPE TABLE OF ty_lote,
      w_lote               TYPE ty_lote,
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
      l_qtd                TYPE i,
      l_tabix              TYPE sy-tabix,
*---> 28/06/2023 - Migração S4 - JS
*     l_cockpit            TYPE char02,
      l_cockpit            TYPE zchar02,
*<--- 28/06/2023 - Migração S4 - JS
      l_erro               TYPE c,
      l_editar             TYPE c,
      l_werks              TYPE char10,
      l_lote_editado       TYPE numc10,
      l_id_seq             TYPE zsdt0225-id_seq,
      l_resp               TYPE c,
      ok_code              TYPE sy-ucomm,
      ok_code2             TYPE sy-ucomm,
      r_matkl_sel          TYPE RANGE OF matkl WITH HEADER LINE,
*
      zcl_util             TYPE REF TO zcl_util,
*
      l_sel_button         TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

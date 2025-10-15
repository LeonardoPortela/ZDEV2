*&---------------------------------------------------------------------*
*& Include          ZMMR0045_TOP
*&---------------------------------------------------------------------*
REPORT zsdr0230 MESSAGE-ID zsd.

************************************************************************
* tabelas
************************************************************************
TABLES: mara.

************************************************************************
* Types
************************************************************************
TYPES: BEGIN OF ty_error,
         statuscode TYPE string,
         message    TYPE string.
TYPES: END   OF ty_error.

TYPES: BEGIN OF ty_integra.
         INCLUDE TYPE zintegracao.
TYPES: END   OF ty_integra.

TYPES: BEGIN OF ty_product,
         matnr  TYPE mara-matnr,
         maktx  TYPE makt-maktx,
         laeda  TYPE mara-laeda,
         meins  TYPE mara-meins,
         brgew  TYPE mara-brgew,
         matnr2 TYPE zde_id_referencia.
TYPES: END   OF ty_product.

TYPES: BEGIN OF ty_saida_prod,
         status   TYPE icon_d,
         matnr    TYPE mara-matnr,
         maktx    TYPE makt-maktx,
         steuc    TYPE marc-steuc,
         meins    TYPE mara-meins,
         brgew    TYPE mara-brgew,
         mensagem TYPE char300,
         matnr2   TYPE zde_id_referencia.
TYPES: END   OF ty_saida_prod.

************************************************************************
* variaveis
************************************************************************
DATA: lv_tabix             TYPE sy-tabix,
      lv_erro              TYPE char01,
      lv_icon              TYPE icon_d,
      lv_data              TYPE sy-datum,
*
      t_product            TYPE TABLE OF ty_product,
      t_saida_prod         TYPE TABLE OF ty_saida_prod,
      t_integra            TYPE TABLE OF ty_integra,
      w_product            TYPE ty_product,
      w_saida_prod         TYPE ty_saida_prod,
      w_integra            TYPE ty_integra,
      w_error              TYPE ty_error,
*
      g_manager            TYPE REF TO cl_gos_manager,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
*
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
      ok_code              TYPE sy-ucomm,
      lc_object            TYPE borident,
      lc_ip_mode           TYPE sgs_rwmod,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_color              TYPE lvc_t_scol,
      w_color              TYPE lvc_s_scol,
      t_ucomm              TYPE TABLE OF syst_ucomm,
      w_ucomm              TYPE syst_ucomm,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl    VALUE 'XX',
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      t_rows_head          TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

************************************************************************
* parametros entrada
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr   FOR mara-matnr,
                  s_laeda   FOR mara-laeda.
SELECTION-SCREEN END   OF BLOCK b1.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

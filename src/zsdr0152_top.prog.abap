*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

**********************************************************************
* TABELAS
**********************************************************************
TABLES: zib_cte_dist_ter, zlest0019, lfa1, mara, bsak, sscrfields.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_chave_cte,
         chave TYPE zde_chave_doc_e.
TYPES: END   OF ty_chave_cte.

TYPES: BEGIN OF ty_zib_cte.
         INCLUDE STRUCTURE zib_cte_dist_ter.
         TYPES: awkey TYPE bkpf-awkey.
TYPES: END   OF ty_zib_cte.

TYPES: BEGIN OF ty_alv,
         filial        TYPE zlest0019-branch,
         transbordo    TYPE lfa1-ort01,
         vagao         TYPE zlest0019-idvagao,
         cidade_origem TYPE j_1bnfdoc-ort01,
         data_saida    TYPE zlest0019-dtadecarga,
         peso_saida    TYPE zlest0019-pesodvagao,
         dcl           TYPE zlest0019-dcl,
         produto       TYPE makt-maktx,
         valor_pago    TYPE bsak-dmbtr,
         data_pagto    TYPE bsak-budat.
TYPES: END   OF ty_alv.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: t_zib_cte            TYPE TABLE OF ty_zib_cte,
      w_zib_cte            TYPE ty_zib_cte,
      t_zlest0019          TYPE TABLE OF zlest0019,
      w_zlest0019          TYPE zlest0019,
      t_campos_nfe         TYPE TABLE OF zde_campos_nfe,
      w_campos_nfe         TYPE zde_campos_nfe,
      t_active             TYPE TABLE OF j_1bnfe_active,
      w_active             TYPE j_1bnfe_active,
      t_chave_cte          TYPE TABLE OF ty_chave_cte,
      w_chave_cte          TYPE ty_chave_cte,
      t_bkpf               TYPE TABLE OF bkpf,
      w_bkpf               TYPE bkpf,
      t_bsak               TYPE TABLE OF bsak,
      w_bsak               TYPE bsak,
      t_makt               TYPE TABLE OF makt,
      w_makt               TYPE makt,
      t_lfa1               TYPE TABLE OF lfa1,
      w_lfa1               TYPE lfa1,
      t_jdoc               TYPE TABLE OF j_1bnfdoc,
      w_jdoc               TYPE j_1bnfdoc,
*
      g_grid               TYPE REF TO cl_gui_alv_grid,
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
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_color              TYPE lvc_t_scol,
      w_color              TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_alv                TYPE TABLE OF ty_alv,
      w_alv                TYPE ty_alv,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      l_qtd                TYPE i,
      l_tabix              TYPE sy-tabix,
*---> 20.06.2023 - Migração S4 - DG
"      l_cockpit            TYPE char02,
      L_COCKPIT            TYPE ZCHAR02,
*<--- 20.06.2023 - Migração S4 - DG
      l_erro               TYPE c,
      l_resp               TYPE c,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util,
*
      l_sel_button         TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.


*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

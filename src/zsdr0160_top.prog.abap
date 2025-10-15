*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0160.

**********************************************************************
* TABELAS
**********************************************************************
TABLES: zsdt0330, zsdt0063, icon, sscrfields, zsdt0344.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_zsdt0330.
         INCLUDE STRUCTURE zsdt0330.
TYPES:   name1 TYPE t001w-name1.
TYPES: END   OF ty_zsdt0330.

TYPES: BEGIN OF ty_alv,
         werks          TYPE zsdt0330-werks,
         name1          TYPE t001w-name1,
         status         TYPE char50,
         qtd_penden     TYPE i,
         qtd_emproc     TYPE i,
         qtd_proces_err TYPE i,
         qtd_proces_suc TYPE i,
         total          TYPE i.
TYPES: END   OF ty_alv.

TYPES: BEGIN OF ty_alv_stat.
         INCLUDE STRUCTURE zsdt0330.
TYPES:   name1 TYPE t001w-name1.
TYPES: status_trace TYPE icon-id.
TYPES: desc_estorno TYPE char30.
TYPES: END   OF ty_alv_stat.

TYPES: BEGIN OF ty_alv_lote,
         status_trace     TYPE icon-id,
         status_email     TYPE icon-id,
         status_integra   TYPE icon-id,
         id_carga         TYPE zsdt0330-id_carga,
         matnr            TYPE zsdt0330-matnr,
         werks            TYPE zsdt0330-werks,
         lgort            TYPE zsdt0330-lgort,
         acharg           TYPE zsdt0330-acharg,
         safra            TYPE zsdt0330-safra,
         status_fardo     TYPE zsdt0330-status_fardo,
         status_gera_lote TYPE zsdt0330-status_gera_lote,
         nr_romaneio      TYPE zmmt0008-nr_romaneio,
         nfnum            TYPE zmmt0008-nfnum,
         vbeln            TYPE zmmt0008-vbeln,
         placa_cav        TYPE zmmt0008-placa_cav,
         motorista        TYPE zmmt0008-motorista,
         menge            TYPE zmmt0008-menge.
TYPES: END   OF ty_alv_lote.

TYPES: BEGIN OF ty_alv_nota,
         status_nf         TYPE icon-id,
         docnum            TYPE zsdt0330-docnum,
         docnum_cancelado  TYPE zsdt0330-docnum,
         id_carga          TYPE zsdt0330-id_carga,
         matnr             TYPE zsdt0330-matnr,
         werks             TYPE zsdt0330-werks,
         lgort             TYPE zsdt0330-lgort,
         acharg            TYPE zsdt0330-acharg,
         safra             TYPE zsdt0330-safra,
         status_nf_enviada TYPE zsdt0330-status_nf_enviada.
TYPES: END   OF ty_alv_nota.

TYPES: BEGIN OF ty_alv_reenv.
         INCLUDE STRUCTURE zsdt0213_integra.
TYPES:   status TYPE icon-id.
TYPES:   descr  TYPE char20.
TYPES: END   OF ty_alv_reenv.

"SD - Ganho Peso Automatico Algodao US #145369 - WPP
TYPES: BEGIN OF ty_alv_ganho_perda.
         INCLUDE STRUCTURE zsdt0344.
TYPES:   status TYPE icon-id.
TYPES:   descr     TYPE char20.
TYPES:   werks     TYPE zsdt0330-werks.
TYPES:   safra     TYPE zsdt0330-safra.
TYPES:   estornado TYPE c.
TYPES: END   OF ty_alv_ganho_perda.
"SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.

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
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_alv                TYPE TABLE OF ty_alv,
      w_alv                TYPE ty_alv,
      t_alv_stat           TYPE TABLE OF ty_alv_stat,
      w_alv_stat           TYPE ty_alv_stat,
      t_alv_lote           TYPE TABLE OF ty_alv_lote,
      w_alv_lote           TYPE ty_alv_lote,
      t_alv_nota           TYPE TABLE OF ty_alv_nota,
      w_alv_nota           TYPE ty_alv_nota,
      t_alv_esto           TYPE TABLE OF ty_alv_stat,
      w_alv_esto           TYPE ty_alv_stat,
      t_alv_reenv          TYPE TABLE OF ty_alv_reenv,
      t_alv_ganho_perda    TYPE TABLE OF ty_alv_ganho_perda, "SD - Ganho Peso Automatico Algodao US #145369 - WPP
      w_alv_reenv          TYPE ty_alv_reenv,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      t_zsdt0330_aux       TYPE TABLE OF ty_zsdt0330,
      t_zsdt0330           TYPE TABLE OF ty_zsdt0330,
      w_zsdt0330           TYPE ty_zsdt0330,
      t_zsdt0331           TYPE TABLE OF zsdt0331,
      w_zsdt0331           TYPE zsdt0331,
      t_zsdt0327           TYPE TABLE OF zsdt0327,
      w_zsdt0327           TYPE zsdt0327,
      t_zmmt0008           TYPE TABLE OF zmmt0008,
      w_zmmt0008           TYPE zmmt0008,
      t_zsdt0213_int       TYPE TABLE OF zsdt0213_integra,
      t_zsdt0344           TYPE TABLE OF zsdt0344, "SD - Ganho Peso Automatico Algodao US #145369 - WPP
      w_zsdt0213_int       TYPE zsdt0213_integra,
      t_icon               TYPE TABLE OF ty_icon,
      w_icon               TYPE ty_icon,
      w_acttab             TYPE j_1bnfe_active,
*
      l_tabix              TYPE sy-tabix,
      l_erro               TYPE char01,
      l_erro1              TYPE char01,
      l_erro2              TYPE char01,
      l_erro3              TYPE char01,
      l_cockpit            TYPE char02,
      l_icon_name          TYPE icon-name,
      ok_code              TYPE sy-ucomm,
      l_value              TYPE usr05-parva,
*
      zcl_util             TYPE REF TO zcl_util,
*
      l_sel_button         TYPE smp_dyntxt.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

**********************************************************************
* ranges
**********************************************************************
RANGES: r_name      FOR icon-name.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

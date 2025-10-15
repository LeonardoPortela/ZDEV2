*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

**********************************************************************
* TABELAS
**********************************************************************
TABLES: zppt0030, icon, sscrfields.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_zppt0030,
*        INCLUDE STRUCTURE zppt0030.
         id_referencia   TYPE zde_id_referencia,
         acharg          TYPE charg_d,
         werks           TYPE werks_d,
         status_registro TYPE char3,
         processamento   TYPE char02,
         emproc_normal   TYPE char01,
         proces_normal   TYPE char01,
         emproc_estorn   TYPE char01,
         proces_estorn   TYPE char01,
         name1           TYPE t001w-name1.
TYPES: END   OF ty_zppt0030.

TYPES: BEGIN OF ty_alv,
         werks      TYPE zppt0030-werks,
         name1      TYPE t001w-name1,
         status     TYPE char50,
         qtd_penden TYPE i,
         qtd_emproc TYPE i,
         qtd_proces TYPE i,
         total      TYPE i.
TYPES: END   OF ty_alv.

TYPES: BEGIN OF ty_alv_reen,
         status     TYPE icon-id,
         werks      TYPE zppt0006-werks,
         id_cotton  TYPE zppt0006-id_cotton,
         acharg     TYPE zppt0006-acharg,
         lgort      TYPE zppt0006-lgort,
         data       TYPE zppt0006-data,
         cd_safra   TYPE zppt0002-cd_safra,
         cd_sai     TYPE zppt0002-cd_sai,
         flag_envio TYPE zppt0006-flag_envio,
         mensagem   TYPE zppt0006-cd_mensagem.
TYPES: END   OF ty_alv_reen.

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
      t_alv_reen           TYPE TABLE OF ty_alv_reen,
      w_alv_reen           TYPE ty_alv_reen,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      t_zppt0030           TYPE TABLE OF ty_zppt0030,
      w_zppt0030           TYPE ty_zppt0030,
      t_zppt0002           TYPE TABLE OF zppt0002,
      w_zppt0002           TYPE zppt0002,
      t_zppt0006           TYPE TABLE OF zppt0006,
      w_zppt0006           TYPE zppt0006,
      t_zppt0006_grp       TYPE TABLE OF zppt0006,
      w_zppt0006_grp       TYPE zppt0006,
      t_zppt0006_atu       TYPE TABLE OF zppt0006,
      w_zppt0006_atu       TYPE zppt0006,
      t_zppt0006_pri       TYPE TABLE OF zppt0006,
      w_zppt0006_pri       TYPE zppt0006,
      t_icon               TYPE TABLE OF ty_icon,
      w_icon               TYPE ty_icon,
*
      l_tabix              TYPE sy-tabix,
*---> 28/06/2023 - Migração S4 - JS
*      l_cockpit            TYPE char02,
      l_cockpit            TYPE zchar02,
*<--- 28/06/2023 - Migração S4 - JS
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

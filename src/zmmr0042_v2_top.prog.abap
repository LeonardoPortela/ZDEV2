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

TYPES: BEGIN OF ty_zppt0002,
         id_referencia              TYPE zppt0002-id_referencia,
         id_sessao                  TYPE zppt0002-id_sessao,
         lgort                      TYPE zppt0002-lgort,
         werks                      TYPE zppt0002-werks,
         status_registro            TYPE zppt0002-status_registro,
         status_processamento       TYPE zppt0002-status_processamento,
         status_ret_sistema_origem  TYPE zppt0002-status_ret_sistema_origem,
         motivo_reprocessamento     TYPE zppt0002-motivo_reprocessamento,
         erro_ret_sistema_origem    TYPE zppt0002-erro_ret_sistema_origem,
         qtd_tentativa_ret_sis_orig TYPE zppt0002-qtd_tentativa_ret_sis_orig,
         cd_mensagem                TYPE zppt0002-cd_mensagem,
         name1                      TYPE t001w-name1,
         status_group               TYPE zppt0002-status_registro,
         ds_status                  TYPE char50.
TYPES: END OF ty_zppt0002.

TYPES: BEGIN OF ty_alv,
         werks            TYPE zppt0030-werks,
         name1            TYPE t001w-name1,
         status           TYPE char50,
         status_registro  TYPE zppt0002-status_registro,
         qtd_penden       TYPE i,
         qtd_emproc       TYPE i,
         qtd_proces       TYPE i,
         qtd_ret_pend     TYPE i,
         qtd_ret_proc     TYPE i,
         total            TYPE i,
         color            TYPE kkblo_specialcol OCCURS 0.
TYPES: END   OF ty_alv.

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

 DATA: GIT_ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
       GWA_ESTRUTURA    TYPE TY_ESTRUTURA.

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
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      git_zppt0002         TYPE TABLE OF ty_zppt0002,
      git_zppt0002_det     TYPE TABLE OF ty_zppt0002,
      git_zppt0002_tp_01   TYPE TABLE OF ty_zppt0002,
      git_zppt0002_tp_02   TYPE TABLE OF ty_zppt0002,
      git_zppt0002_tp_03   TYPE TABLE OF ty_zppt0002,
      git_zppt0002_wait    TYPE TABLE OF ty_zppt0002,
      t_icon               TYPE TABLE OF ty_icon,
      w_icon               TYPE ty_icon,
      l_tabix              TYPE sy-tabix,
      l_icon_name          TYPE icon-name,
      ok_code              TYPE sy-ucomm,
      l_value              TYPE usr05-parva,
      zcl_util             TYPE REF TO zcl_util,
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

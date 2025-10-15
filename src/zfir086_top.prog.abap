*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

**********************************************************************
* TABELAS
**********************************************************************
TABLES: zcarta_correcao,
        j_1bnfdoc.

**********************************************************************
* Tabelas
**********************************************************************

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_alv,
         bukrs        TYPE bukrs,
         docnum       TYPE j_1bdocnum,
         tp_authcod   TYPE j_1bnfedocstatus,
         tip_troca    TYPE char15,
         parc_orig    LIKE lfa1-lifnr,
         name1_orig   TYPE name1,
         parc_corr    LIKE lfa1-lifnr,
         name1_corr   TYPE name1,
         dt_authcod   TYPE j_1bnfe_action_date,
         tp_transf    TYPE char10,
         werks_o      TYPE werks_d,
         lgort_o      TYPE lgort_d,
         werks_d      TYPE werks_d,
         lgort_d      TYPE lgort_d,
         doc_material TYPE mblnr,
         ano_material TYPE mjahr,
         msg_correc1  TYPE zchar250.
TYPES: END   OF ty_alv.

TYPES: BEGIN OF ty_carta.
         INCLUDE STRUCTURE zcarta_correcao.
         TYPES:   bukrs TYPE bukrs.
TYPES: END   OF ty_carta.

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
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      t_selectedrow        TYPE lvc_t_row,
      w_selectedrow        TYPE lvc_s_row,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_alv                TYPE TABLE OF ty_alv,
      w_alv                TYPE ty_alv,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      t_carta_aux          TYPE TABLE OF ty_carta,
      t_carta              TYPE TABLE OF ty_carta,
      w_carta              TYPE ty_carta,
      t_jnad               TYPE TABLE OF j_1bnfnad,
      w_jnad               TYPE j_1bnfnad,
      t_lfa1               TYPE TABLE OF lfa1,
      w_lfa1               TYPE lfa1,
*
      l_tabix              TYPE sy-tabix,
      l_parceiro           TYPE char1,
      ok_code              TYPE sy-ucomm.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

FUNCTION-POOL zgfs_cct.                     "MESSAGE-ID ..

*******************************************************************************************
* tabelas
*******************************************************************************************
TABLES: zsdt0264.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA tree1 TYPE REF TO cl_hrpayna_gui_alv_tree. "cl_gui_alv_tree.
DATA mr_toolbar              TYPE REF TO cl_gui_toolbar.

DATA: g_container        TYPE scrfname VALUE 'TREE1',
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_grid             TYPE REF TO cl_gui_alv_grid.

*******************************************************************************************
* types
*******************************************************************************************
TYPES: BEGIN OF ty_alv.
         INCLUDE STRUCTURE zsds0051.
       TYPES: END   OF ty_alv.

*******************************************************************************************
* tabelas / works
*******************************************************************************************
DATA: t_fieldcatalog  TYPE lvc_t_fcat, "Fieldcatalog
      t_checked_items TYPE lvc_t_chit,
      w_checked_items TYPE LINE OF  lvc_t_chit,
      t_node_key      TYPE lvc_t_nkey,  "Saves top node key for expand nodes
      t_node_key_aux  TYPE lvc_t_nkey,  "Saves top node key for expand nodes
      w_node_key      TYPE lvc_nkey,
      w_node_key_aux  TYPE lvc_nkey,  "Saves top node key for expand nodes
*
      t_zsdt0264      TYPE TABLE OF zsdt0264,
      t_alv           TYPE TABLE OF ty_alv,
      gt_alv          TYPE TABLE OF ty_alv,
      t_exctab        TYPE slis_t_extab,
*
      w_zsdt0264      TYPE zsdt0264,
      w_alv           TYPE ty_alv,
      w_exctab        TYPE slis_extab,
      w_item_layout   TYPE lvc_s_laci,
      w_layout        TYPE lvc_s_layo,
      lt_sort         TYPE lvc_t_sort,
      wa_sort         LIKE LINE OF lt_sort.
*
DATA: t_rsparams TYPE TABLE OF rsparams,
      w_rsparams TYPE rsparams.

*******************************************************************************************
* data
*******************************************************************************************
DATA: ok_code        TYPE sy-ucomm,           "OK-Code
      l_tabix        TYPE sy-tabix,          "OK-Code
      l_erro         TYPE c,
      l_flag         TYPE c,
      l_init_tree    TYPE c,
      l_dias         TYPE i,
*
      g_integra      TYPE c,
      g_nao_integra  TYPE c,
      g_dt_lista_de  TYPE datum,
      g_dt_lista_ate TYPE datum,
      g_dt_baixa_de  TYPE datum,
      g_dt_baixa_ate TYPE datum.

RANGES:
      r_docdat        FOR j_1bnfdoc-docdat.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zsdf002_toolbar_event_receiver.
INCLUDE zsdf002_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver.

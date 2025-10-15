FUNCTION-POOL zgsd_embarque_fert.           "MESSAGE-ID ..

TYPE-POOLS : slis, icon, kkblo.

*******************************************************************************************
* types
*******************************************************************************************
TYPES: BEGIN OF ty_pedido,
         ebeln    TYPE ebeln,
         ebelp    TYPE ebelp,
         maktx    TYPE makt-maktx,
         qtd_disp TYPE bstmg.
TYPES: END   OF ty_pedido.

TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         matnr TYPE ekpo-matnr.
TYPES: END   OF ty_ekpo.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx.
TYPES: END   OF ty_makt.

TYPES: BEGIN OF ty_regs,
         row_id TYPE int4,
         ebeln  TYPE ebeln,
         ebelp  TYPE ebelp.
TYPES: END   OF ty_regs.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

*DATA: g_nro_sol           TYPE numc5, " RIM CS1029457 ANB 30.09.2022
DATA: g_nro_sol           TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
      g_seq               TYPE numc3,
      g_filial_resp       TYPE vkbur,
      g_selecao           TYPE c,
      g_edit              TYPE c,
      g_ebeln_sai         TYPE ebeln,
      g_ebelp_sai         TYPE ebelp,
*
      t_pedido            TYPE TABLE OF ty_pedido,
      w_pedido            TYPE ty_pedido,
      t_zsdt0265          TYPE TABLE OF zsdt0265,
      w_zsdt0265          TYPE zsdt0265,
      t_ekpo              TYPE TABLE OF ty_ekpo,
      w_ekpo              TYPE ty_ekpo,
      t_makt              TYPE TABLE OF ty_makt,
      w_makt              TYPE ty_makt,
      t_regs              TYPE TABLE OF ty_regs,
      w_regs              TYPE ty_regs,
*
      tree1               TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar          TYPE REF TO cl_gui_toolbar,
      g_container         TYPE scrfname VALUE 'CONTAINER',
      g_container2        TYPE scrfname VALUE 'CONTAINER2',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button,
      ok_code             TYPE sy-ucomm,
      l_ok                TYPE c,
      l_ped_imp           TYPE c,
*
      t_fieldcatalog      TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab            TYPE slis_t_extab,
      w_exctab            TYPE slis_extab,
      w_item_layout       TYPE lvc_s_laci,
      w_layout            TYPE lvc_s_layo,
      ls_fieldcatalog     TYPE lvc_s_fcat,
      ls_exclude          TYPE ui_func,
      pt_exclude          TYPE ui_functions,
      pt_exclude2         TYPE ui_functions,
      t_del_rows          TYPE lvc_t_row,
      w_del_rows          TYPE lvc_s_row,
      t_sel_cols          TYPE lvc_t_col,
      w_sel_cols          TYPE lvc_s_col,
      l_row_id            TYPE lvc_s_row,
      l_column_id         TYPE lvc_s_col,
      l_stable            TYPE lvc_s_stbl,

      t_fcat_lvc          TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb          TYPE kkblo_t_fieldcat.

*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event2 DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zsdfert_toolbar_event_receiver.
INCLUDE zsdfert_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler2       TYPE REF TO lcl_event2.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.
*    FREE e_object->mt_toolbar.
*
*    CLEAR w_tool.
*    w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    w_tool-text     = ''.
*    w_tool-icon     = '@17@'.
*    APPEND w_tool TO e_object->mt_toolbar.
*
*    CLEAR w_tool.
*    w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    w_tool-text     = ''.
*    w_tool-icon     = '@18@'.
*    APPEND w_tool TO e_object->mt_toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.
    CASE e_ucomm.

      WHEN 'INSERT'.

      WHEN 'DELETE'.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    l_stable-row = 'X'.
    l_stable-col = 'X'.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

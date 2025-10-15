FUNCTION-POOL zgfs_hvi.                     "MESSAGE-ID ..

TYPE-POOLS : slis, icon, kkblo.

*******************************************************************************************
* types
*******************************************************************************************
TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE zmmt0027.
       TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_zppt0033.
         INCLUDE TYPE zppt0033.
       TYPES: END   OF ty_zppt0033.

TYPES: BEGIN OF ty_zmmi0001.
         INCLUDE TYPE zmmi0001.
         TYPES: charg TYPE zmmt0027-charg.
TYPES: END   OF ty_zmmi0001.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: zcl_util            TYPE REF TO zcl_util,
*
      t_saida             TYPE TABLE OF ty_saida,
      w_saida             TYPE ty_saida,
      t_zppt0033          TYPE TABLE OF ty_zppt0033,
      w_zppt0033          TYPE ty_zppt0033,
      t_zmmi0001          TYPE TABLE OF ty_zmmi0001,
      w_zmmi0001          TYPE ty_zmmi0001,
      w_dados             TYPE zmmi0001,
      l_mesg1             TYPE char200,
      l_mesg2             TYPE char200,
      l_mesg3             TYPE char200,
      l_confirma          TYPE c,
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
      l_seleciona         TYPE c,
*
      t_sort              TYPE lvc_t_sort,
      w_sort              TYPE lvc_s_sort,
      t_fieldcatalog      TYPE lvc_t_fcat, "Fieldcatalog
      w_fieldcatalog      TYPE lvc_s_fcat, "Fieldcatalog
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
INCLUDE zsdhvio_toolbar_event_receiver.
INCLUDE zsdhvio_tree_event_receiver.

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

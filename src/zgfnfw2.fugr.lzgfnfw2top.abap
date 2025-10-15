FUNCTION-POOL zgfnfw2                    . "MESSAGE-ID ...
INCLUDE <icon>.
CONSTANTS: c_x           TYPE c VALUE 'X',
           c_clos_msg(8) TYPE c VALUE 'CLOS_MSG',
           c_info(8)     TYPE c VALUE 'INFO'.

*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
* dialogbox Container
DATA obg_dialogbox        TYPE REF TO cl_gui_dialogbox_container.
* Docking Container
DATA: obg_docking          TYPE REF TO cl_gui_docking_container,
      obg_grid1            TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      screen               TYPE sy-dynnr.

DATA g_dat(50).
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.

DATA: wg_mensagens(30) VALUE '@5C@ Messagens',
      tg_fieldcatalog  TYPE lvc_t_fcat,
      wg_fieldcatalog  TYPE lvc_s_fcat,
      wg_layout        TYPE lvc_s_layo,
      wg_stable        TYPE lvc_s_stbl,
      tg_msgs          TYPE TABLE OF zfiwrs0002,
      wg_msgs          TYPE zfiwrs0002,
      v_info           TYPE c,
      e_aba(50),
      e_field(30),
      wl_data          TYPE REF TO cl_gui_alv_grid.

DATA: tl_bdc TYPE TABLE OF bdcdata,
      wl_bdc TYPE bdcdata.

FIELD-SYMBOLS: <fs_aba>   TYPE any,
               <fs_field> TYPE any.

FIELD-SYMBOLS: "<fs_field> TYPE ANY,
  <fs_field2>   TYPE any,
  <fs_wa>       TYPE any,
  <fs_table>    TYPE table,
  <fs_set_cell> TYPE any,
  <fs_set_obj>  TYPE any,
  <fs_campo>    TYPE any,
  <fs_nfenum>   TYPE any,
  <fs_series>   TYPE any.

*                 <fs_table_aux>  TYPE TABLE.

DATA: goodsmvt_header  TYPE bapi2017_gm_head_01,
      goodsmvt_code    TYPE bapi2017_gm_code,
      goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE,
      return           TYPE TABLE OF bapiret2 WITH HEADER LINE,
      materialdocument TYPE	bapi2017_gm_head_ret-mat_doc,
      matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year,
      zfiwrt0008       TYPE zfiwrt0008,
      zfiwrt0009       TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
      zfiwrt0012       TYPE zfiwrt0012,
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Início de Alteração
      t_zfiwrt0010     TYPE TABLE OF zfiwrt0010,
      zfiwrs0005       TYPE zfiwrs0005.
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Fim de Alteração

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING
        io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar
        FOR EVENT toolbar
        OF  cl_gui_alv_grid
        IMPORTING
          e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      handle_close_box
        FOR EVENT close OF cl_gui_dialogbox_container.
*    CLASS-METHODS:
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                                   IMPORTING  E_ROW_ID E_COLUMN_ID.
*
*    CLASS-METHODS:
*     ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
* IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
*
*    CLASS-METHODS:
* ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV
*_GRID
*                      IMPORTING E_MODIFIED ET_GOOD_CELLS.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
*   Add customized toolbar buttons.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button1'.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    IF ( v_info = abap_true ).
      ty_toolbar-icon      =  icon_message_information_small.
      ty_toolbar-function  =  c_info.
      ty_toolbar-butn_type = 0.
*      ty_toolbar-text      = 'Inf'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
    ENDIF.


*    ty_toolbar-icon      =  icon_voice_output.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button2'.
*
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*
*    ty_toolbar-icon      =  icon_phone.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button3'.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*
*    ty_toolbar-icon      =  icon_mail.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button4'.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*
*    ty_toolbar-icon      =  icon_voice_input.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button5'.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
*   User Command Botões Incluidos
*    break abap.

    DATA function TYPE char10.

    CASE e_ucomm.
      WHEN c_clos_msg.
        IF ( obg_docking IS NOT INITIAL ).
          CALL METHOD obg_docking->set_visible
            EXPORTING
              visible = space.
        ELSEIF ( obg_dialogbox IS NOT INITIAL ).
          CALL METHOD obg_dialogbox->set_visible
            EXPORTING
              visible = space.
        ENDIF.

      WHEN c_info.
        CALL METHOD obg_grid1->get_selected_cells
          IMPORTING
            et_cell = DATA(selected_cell).

        IF <fs_field> IS ASSIGNED.
          <fs_field> = tg_msgs[ selected_cell[ 1 ]-row_id-index ]-field.
        ENDIF.
    ENDCASE.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = e_ucomm
      EXCEPTIONS
        function_not_supported = 1.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
    DATA: tl_cell TYPE lvc_t_cell,
          wl_cell TYPE lvc_s_cell.

    CLEAR: wl_cell.
    REFRESH: tl_cell.

    IF e_row GT 0.
      READ TABLE tg_msgs INTO wg_msgs INDEX e_row.
      IF sy-subrc IS INITIAL.
        IF <fs_aba> IS ASSIGNED.
          MOVE: wg_msgs-aba   TO <fs_aba>.
        ENDIF.

        IF <fs_field> IS ASSIGNED.
          MOVE: wg_msgs-field TO <fs_field>.
        ENDIF.

        IF <fs_set_cell> IS ASSIGNED.
*          MOVE: wg_msgs-field TO wl_cell-col_id-fieldname,
*                wg_msgs-tabix TO wl_cell-row_id-index.
*          BREAK-POINT.
*          ASSIGN ('(Z_DOC_CHECK_NEW)E_DB_CLICK') TO <fs_SET_CELL>.
*          break-point.
          ASSIGN ('<FS_SET_CELL>-COL_ID-FIELDNAME') TO <fs_campo>.
          IF <fs_campo> IS ASSIGNED.
            MOVE: wg_msgs-field TO <fs_campo>.
            UNASSIGN <fs_campo>.
          ENDIF.
          ASSIGN ('<FS_SET_CELL>-ROW_ID-INDEX') TO <fs_campo>.
          IF <fs_campo> IS ASSIGNED.
            MOVE:  wg_msgs-tabix TO <fs_campo>.
            UNASSIGN <fs_campo>.
          ENDIF.
*          APPEND wl_cell TO tl_cell.

*          BREAK-POINT.
**          CONCATENATE '('wg_msgs-obj
*          ASSIGN COMPONENT  ('(ZSDR015)GRID1->*') TO <FS_OO>.
*          GET REFERENCE OF ('(ZSDR015)GRID1') INTO WL_DATA.
*          CONCATENATE wg_msgs-obj '->SET_SELECTED_CELLS' INTO g_dat.
*          CALL METHOD <FS_OO>->SET_SELECTED_CELLS"(g_dat)          "(wg_msgs)=>set_selected_cells
*            EXPORTING
*              it_cellS = tl_cell[].
        ENDIF.

        IF <fs_set_obj> IS ASSIGNED.
          MOVE: wg_msgs-obj TO <fs_set_obj>.
        ENDIF.

*        LEAVE TO SCREEN SCREEN.
        CALL METHOD obg_toolbar->handle_user_command( e_ucomm = 'CLOS_MSG' ).

      ENDIF.

    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD handle_close_box.
    CALL METHOD obg_dialogbox->set_visible
      EXPORTING
        visible = space.
  ENDMETHOD.                    "handle_close_box
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

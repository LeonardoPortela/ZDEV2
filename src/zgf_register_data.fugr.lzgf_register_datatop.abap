FUNCTION-POOL zgf_register_data.            "MESSAGE-ID ..


TABLES: dd03l.

DATA: okcode TYPE sy-ucomm.

TYPES: BEGIN OF ty_excl_toolbar,
         code TYPE ui_func.
TYPES: END OF ty_excl_toolbar.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_field_screen_key,
         field_screen TYPE string.
TYPES END OF ty_field_screen_key.

DATA: p_db_tab    TYPE tabname,
      p_stcnam    TYPE tabname,
      p_title     TYPE cua_tit_tx,
      p_start_lin TYPE numc3,
      p_start_col TYPE numc3,
      p_end_lin   TYPE numc3,
      p_end_col   TYPE numc3.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

DATA: obj_alv       TYPE REF TO cl_gui_alv_grid,
      obj_container TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar TYPE REF TO lcl_alv_toolbar.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
      wa_excl_toolbar TYPE ty_excl_toolbar.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.


"Seleção Dinamica

DATA: d_it_saida               TYPE REF TO data,
      d_wa_saida               TYPE REF TO data,
      d_wa_saida_tmp           TYPE REF TO data,
      d_table                  TYPE REF TO data,
      d_table_wa               TYPE REF TO data,
      d_wa_registro_manter     TYPE REF TO data,
      d_wa_registro_manter_tmp TYPE REF TO data.

FIELD-SYMBOLS: <fs_it_saida>               TYPE table,
               <fs_wa_saida>               TYPE any,
               <fs_wa_saida_tmp>           TYPE any,
               <fs_table>                  TYPE STANDARD TABLE,
               <fs_wa_table>               TYPE any,
               <fs_wa_registro_manter>     TYPE any,
               <fs_wa_registro_manter_tmp> TYPE any.

DATA: vg_cond   TYPE rsds_where,
      cond_line TYPE rsdswhere.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: tg_field_screen_key TYPE TABLE OF ty_field_screen_key WITH HEADER LINE,
      tg_dd04t            TYPE TABLE OF dd04t WITH HEADER LINE,
      tg_dd03l            TYPE TABLE OF dd03l WITH HEADER LINE,
      tg_dd04t_out        TYPE TABLE OF dd04t WITH HEADER LINE,
      tg_dd03l_out        TYPE TABLE OF dd03l WITH HEADER LINE.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao TYPE c LENGTH 20,
      var_answer  TYPE c.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_novo    TYPE c VALUE 'NOVO'     LENGTH 4,
           c_del     TYPE c VALUE 'DEL'      LENGTH 4,
           c_change  TYPE c VALUE 'CHANGE'   LENGTH 6,
           c_func_01 TYPE c VALUE 'FUNC_01'  LENGTH 50,
           c_func_02 TYPE c VALUE 'FUNC_01'  LENGTH 50.

*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD catch_hotspot.

  ENDMETHOD.

ENDCLASS.

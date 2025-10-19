*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_TOP
*&---------------------------------------------------------------------*


TYPE-POOLS icon.

TYPES: BEGIN OF ty_saida,
         ativa_form_lote       TYPE ZSDT0361-ativa_form_lote ,
         data_ultima_modif     TYPE ZSDT0361-data_ultima_modif,
         hora_ultima_modif     TYPE ZSDT0361-hora_ultima_modif ,
         ultimo_modificador    TYPE ZSDT0361-ultimo_modificador ,
       END OF ty_saida.

DATA: BEGIN OF tg_tp_doc OCCURS 0,
        blart TYPE t003-blart,
      END OF tg_tp_doc,

      BEGIN OF tg_tp_ped OCCURS 0,
        bsart TYPE t161-bsart,
      END OF tg_tp_ped,

      BEGIN OF tg_tp_ov OCCURS 0,
        auart TYPE tvak-auart,
      END OF tg_tp_ov.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida        TYPE TABLE OF ty_saida,
      it_saida_final  TYPE TABLE OF ty_saida,
      wa_saida        TYPE ty_saida,
      it_ZSDT0361     TYPE TABLE OF ZSDT0361,
      wa_ZSDT0361     TYPE ZSDT0361,
      wa_ZSDT0361_aux TYPE ZSDT0361.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: obj_alv       TYPE REF TO cl_gui_alv_grid,
      obj_container TYPE REF TO cl_gui_custom_container.

DATA: gt_f4    TYPE lvc_t_f4 WITH HEADER LINE,
      gv_erro  TYPE c,
      gv_modif TYPE c.

**---------------------------------------------------------------------*
**  Inicio Implementação Classes
**---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0106 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.                                           "
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0103 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.                "

  METHOD on_f4.

  ENDMETHOD.                    "ON_F4

ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION



CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_data_changed
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING et_good_cells.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION


CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.

    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_data_changed.

  ENDMETHOD.

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

DATA: obj_toolbar      TYPE REF TO lcl_alv_toolbar.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

*-------------------------------------------------------------------
* Variaveis
*-------------------------------------------------------------------
DATA: vg_erro      TYPE c,
      v_prefix_ent TYPE zprefix,
      v_mensagem   TYPE bapi_msg,
      t_dir_loc_f  TYPE TABLE OF sdokpath,
      t_dir_local  TYPE TABLE OF sdokpath,
      t_dir_unix   TYPE TABLE OF epsfili,
      v_file_aux   TYPE draw-filep,
      v_file_aux2  TYPE draw-filep,
      it_xml_forn  TYPE TABLE OF zxml,
      wa_xml_forn  TYPE zxml.

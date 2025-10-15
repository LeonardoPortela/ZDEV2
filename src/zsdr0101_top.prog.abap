*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0101.

TABLES: zsdt0006, zsdt0294.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0006.
       TYPES END OF ty_saida_0100.

TYPES: BEGIN OF ty_saida_0120,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0294.
       TYPES END OF ty_saida_0120.

TYPES: BEGIN OF ty_saida_0130,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0287.
       TYPES END OF ty_saida_0130.

TYPES: BEGIN OF ty_saida_0140,
         ck_modify  TYPE c,
         estilo     TYPE lvc_t_styl,
         maktg      TYPE makt-maktg,
         wgbez      TYPE t023t-wgbez,   "*-CS2022000324-25.08.2022-#84903-JT-inicio
         observ_287 TYPE zsdt0287-observ.
         INCLUDE STRUCTURE zsdt0294.
       TYPES END OF ty_saida_0140.

TYPES: BEGIN OF ty_saida_0150,
         ck_modify  TYPE c,
         estilo     TYPE lvc_t_styl,
         maktg      TYPE makt-maktg,
         wgbez      TYPE t023t-wgbez,   "*-CS2022000324-25.08.2022-#84903-JT-inicio
         observ_287 TYPE zsdt0287-observ.
         INCLUDE STRUCTURE zsdt0294.
       TYPES END OF ty_saida_0150.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0100 DEFINITION.
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


CLASS lcl_alv_toolbar_0120 DEFINITION.
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

CLASS lcl_alv_toolbar_0130 DEFINITION.
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

CLASS lcl_alv_toolbar_0140 DEFINITION.
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

CLASS lcl_alv_toolbar_0150 DEFINITION.
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

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS lcl_event_handler_0120 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

CLASS lcl_event_handler_0130 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

CLASS lcl_event_handler_0140 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

CLASS lcl_event_handler_0150 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container,
      obj_alv_0120       TYPE REF TO cl_gui_alv_grid,
      obj_container_0120 TYPE REF TO cl_gui_custom_container,
      obj_alv_0130       TYPE REF TO cl_gui_alv_grid, "65582 CS2021000908 Ajuste Impressão NFe - materiais ASSEGURADO GMP+FSA - Etapa 1
      obj_container_0130 TYPE REF TO cl_gui_custom_container,
      obj_alv_0140       TYPE REF TO cl_gui_alv_grid, "65582 CS2021000908 Ajuste Impressão NFe - materiais ASSEGURADO GMP+FSA - Etapa 1
      obj_container_0140 TYPE REF TO cl_gui_custom_container,
      obj_alv_0150       TYPE REF TO cl_gui_alv_grid, "65582 CS2021000908 Ajuste Impressão NFe - materiais ASSEGURADO GMP+FSA - Etapa 1
      obj_container_0150 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100,
      obj_toolbar_0120 TYPE REF TO lcl_alv_toolbar_0120,
      obj_toolbar_0130 TYPE REF TO lcl_alv_toolbar_0130, "65582
      obj_toolbar_0140 TYPE REF TO lcl_alv_toolbar_0140, "65582
      obj_toolbar_0150 TYPE REF TO lcl_alv_toolbar_0150. "65582

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

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100     TYPE TABLE OF ty_saida_0100,
      wa_saida_0100     TYPE ty_saida_0100,
      it_saida_0120     TYPE TABLE OF ty_saida_0120,
      wa_saida_0120     TYPE ty_saida_0120,
      it_saida_0130     TYPE TABLE OF ty_saida_0130, "65582
      wa_saida_0130     TYPE ty_saida_0130, "65582
      it_saida_0140     TYPE TABLE OF ty_saida_0140, "65582
      wa_saida_0140     TYPE ty_saida_0140, "65582
      it_saida_0140_aux TYPE TABLE OF ty_saida_0140,
      it_saida_0150     TYPE TABLE OF ty_saida_0150, "65582
      wa_saida_0150     TYPE ty_saida_0150, "65582
      it_saida_0150_aux TYPE TABLE OF ty_saida_0150,
      tg_zsdt0006       TYPE TABLE OF zsdt0006 WITH HEADER LINE,
      tg_zsdt0294       TYPE TABLE OF zsdt0294 WITH HEADER LINE,
      tg_zsdt0287       TYPE TABLE OF zsdt0287 WITH HEADER LINE,
      it_zsdt0287_old   TYPE TABLE OF zsdt0287,
      it_zsdt0287_new   TYPE TABLE OF zsdt0287.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao TYPE c LENGTH 20,
      var_answer  TYPE c,
      l_erro      TYPE c.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_novo   TYPE c VALUE 'NOVO'   LENGTH 4,
           c_del    TYPE c VALUE 'DEL'    LENGTH 4,
           c_change TYPE c VALUE 'CHANGE' LENGTH 6.

INITIALIZATION.

  PERFORM: f_selecionar_dados,
           f_processa_dados.

  CALL SCREEN 0100.

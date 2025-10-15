*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_TOP
*&---------------------------------------------------------------------*

REPORT zfir073.

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
      d_table_search           TYPE REF TO data,
      d_table_wa               TYPE REF TO data,
      d_wa_registro_search     TYPE REF TO data,
      d_wa_registro_manter     TYPE REF TO data,
      d_wa_registro_manter_tmp TYPE REF TO data.

DATA: gwa_cond_search TYPE rsds_where.

FIELD-SYMBOLS: <fs_it_saida>               TYPE table,
               <fs_wa_saida>               TYPE any,
               <fs_wa_saida_tmp>           TYPE any,
               <fs_table>                  TYPE STANDARD TABLE,
               <fs_table_search>           TYPE STANDARD TABLE,
               <fs_wa_table>               TYPE any,
               <fs_wa_registro_search>     TYPE any,
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
CONSTANTS: c_novo      TYPE c VALUE 'NOVO'         LENGTH 4,
           c_del       TYPE c VALUE 'DEL'          LENGTH 4,
           c_car       TYPE c VALUE 'CAR'          LENGTH 4,
           c_change    TYPE c VALUE 'CHANGE'       LENGTH 6,
           c_func_01   TYPE c VALUE 'FUNC_01'      LENGTH 50,
           c_func_02   TYPE c VALUE 'FUNC_02'      LENGTH 50,
           c_func_03   TYPE c VALUE 'FUNC_03'      LENGTH 50, "US 172204 - Ajuste Sub Cadastro Zregister_data - WPP --->>
           c_action_01 TYPE c VALUE 'ACTION_01'    LENGTH 50,
           c_action_02 TYPE c VALUE 'ACTION_02'    LENGTH 50. "MMSILVA - 11.02.2025 - #163322

PARAMETERS: p_db_tab TYPE tabname,
            p_stcnam TYPE tabname,
            p_maxdel TYPE i,
            p_scmant TYPE c LENGTH 4,
            p_scpesq TYPE c LENGTH 4,
            p_stpesq TYPE tabname,
            p_title  TYPE cua_tit_tx,
            p_stcsai TYPE tabname,
            p_popup  TYPE char1,
            p_stalin TYPE numc3,
            p_stacol TYPE numc3,
            p_endlin TYPE numc3,
            p_endcol TYPE numc3.

PARAMETERS: p_db_01  TYPE tabname,
            p_stc_01 TYPE tabname,
            p_sc_01  TYPE c LENGTH 4,
            p_ti_01  TYPE cua_tit_tx.

"US 172204 - Ajuste Sub Cadastro Zregister_data - WPP --->>
PARAMETERS: p_db_03  TYPE tabname,
            p_stc_03 TYPE tabname,
            p_sc_03  TYPE c LENGTH 4,
            p_ti_03  TYPE cua_tit_tx.
"US 172204 - Ajuste Sub Cadastro Zregister_data - WPP <<---


"Exits Customers Action
PARAMETERS: p_act_01 TYPE cua_tit_tx,
            p_act_02 TYPE cua_tit_tx, "MMSILVA - 11.02.2025 - #163322
            p_nocwid TYPE char01,     "*-CS2025000249-17.04.2025-#173311-JT-inicio
            p_nosave TYPE char01.     "*-CS2025000249-17.04.2025-#173311-JT-inicio

START-OF-SELECTION.

*  P_DB_TAB  = 'ZFIT0150'.
*  P_STCNAM  = 'ZFIT0150_OUT'.
*  P_SCMANT  = '0110'.
*  P_TITLE   = 'oPS'.

  "Monta Estrutura Saida
  CREATE DATA d_it_saida TYPE TABLE OF (p_stcnam).
  ASSIGN d_it_saida->* TO <fs_it_saida>.

  CREATE DATA d_wa_saida LIKE LINE OF <fs_it_saida>.
  ASSIGN d_wa_saida->* TO <fs_wa_saida>.

  CREATE DATA d_wa_saida_tmp LIKE LINE OF <fs_it_saida>.
  ASSIGN d_wa_saida_tmp->* TO <fs_wa_saida_tmp>.
  "Fim Monta Estrutura Saida

  IF p_stcsai IS NOT INITIAL.
    CREATE DATA d_table TYPE STANDARD TABLE OF (p_stcsai).
  ELSE.
    CREATE DATA d_table TYPE STANDARD TABLE OF (p_db_tab).
  ENDIF.
  ASSIGN d_table->* TO <fs_table>.

  CREATE DATA d_table_wa LIKE LINE OF <fs_table>.
  ASSIGN d_table_wa->* TO <fs_wa_table>.

  CREATE DATA d_wa_registro_manter LIKE LINE OF <fs_table>.
  ASSIGN d_wa_registro_manter->* TO <fs_wa_registro_manter>.

  CREATE DATA d_wa_registro_manter_tmp LIKE LINE OF <fs_table>.
  ASSIGN d_wa_registro_manter_tmp->* TO <fs_wa_registro_manter_tmp>.

  "Instancias para variaveis de pesquisa - WPP 24-06-24
  IF p_scpesq IS NOT INITIAL.
    IF p_stpesq IS NOT INITIAL.
      CREATE DATA d_table_search TYPE STANDARD TABLE OF (p_stpesq).
    ELSE.
      CREATE DATA d_table_search TYPE STANDARD TABLE OF (p_db_tab).
    ENDIF.

    ASSIGN d_table_search->* TO <fs_table_search>.

    CREATE DATA d_wa_registro_search LIKE LINE OF <fs_table_search>.
    ASSIGN d_wa_registro_search->* TO <fs_wa_registro_search>.
  ENDIF.
  "Instancias para variaveis de pesquisa - WPP 24-06-24 - Fim

  IF p_scpesq IS NOT INITIAL. ""Instancias para variaveis de pesquisa - WPP 24-06-24
    CALL SCREEN p_scpesq STARTING AT 10 05.
  ELSE.
    PERFORM: f_selecionar_dados,
             f_processa_dados.

    CALL SCREEN 0001.
  ENDIF.

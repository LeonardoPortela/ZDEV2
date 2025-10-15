*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_TOP
*&---------------------------------------------------------------------*

REPORT zfir073.

TABLES: dd03l, sscrfields.

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

TYPES: BEGIN OF ty_alv,
         bukrs           TYPE zlest0222-bukrs,
         cnpj_filial     TYPE zlest0222-cnpj_filial,
         cnpj_transbordo TYPE zlest0222-cnpj_transbordo,
         nfps            TYPE zlest0222-nfps,
         data            TYPE zlest0222-data,
         valor_servico   TYPE zlest0222-valor_servico,
         datatransb_de   TYPE zlest0222-datatransb_de,
         datatransb_ate  TYPE zlest0222-datatransb_ate,
         dias_transito   TYPE zlest0222-dias_transito,
         matnr           TYPE zlest0222-matnr,
         valor_dia       TYPE zlest0222-valor_dia,
         pesochegada     TYPE zlest0222-pesochegada,
         vlr_tot_est     TYPE zlest0222-vlr_tot_est,
         ebeln           TYPE ekbe-ebeln,
         belnr           TYPE rbkp-belnr,
         augbl           TYPE bsak-augbl,
         augdt           TYPE bsak-augdt.
TYPES: END   OF ty_alv.

TYPES: BEGIN OF ty_select,
 line TYPE fieldname,
 END OF ty_select.

*DATA: d_it_saida               TYPE ty_alv,
*      d_wa_saida               TYPE ty_alv,
*      d_wa_saida_tmp           TYPE ty_alv,
*      d_table                  TYPE ty_alv,
*      d_table_wa               TYPE ty_alv,
*      d_wa_registro_manter     TYPE ty_alv,
*      d_wa_registro_manter_tmp TYPE ty_alv.

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
      cond_line TYPE rsdswhere,
      l_sel_button         TYPE smp_dyntxt.

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

Data: l_leave              TYPE syst_ucomm,
      l_opcao              TYPE char1.


data t_where  TYPE TABLE OF ty_select WITH HEADER LINE.

data: v_DT_DE TYPE CHAR8,
      V_DT_ATE TYPE CHAR8,
      V_DT_NF  TYPE CHAR10.
*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_novo       TYPE c VALUE 'NOVO'         LENGTH 4,
           c_del        TYPE c VALUE 'DEL'          LENGTH 4,
           c_change     TYPE c VALUE 'CHANGE'       LENGTH 6,
           c_func_01    TYPE c VALUE 'FUNC_01'      LENGTH 50,
           c_func_02    TYPE c VALUE 'FUNC_02'      LENGTH 50,
           c_action_01  TYPE c VALUE 'ACTION_01'    LENGTH 50.

PARAMETERS: p_db_tab TYPE tabname,
            p_stcnam TYPE tabname,
            p_scmant TYPE c LENGTH 4,
            p_title  TYPE cua_tit_tx,
            p_stcsai TYPE tabname  NO-DISPLAY,
            p_popup  TYPE char1  NO-DISPLAY,
            p_stalin TYPE numc3  NO-DISPLAY,
            p_stacol TYPE numc3  NO-DISPLAY,
            p_endlin TYPE numc3  NO-DISPLAY,
            p_endcol TYPE numc3  NO-DISPLAY,

            p_empres TYPE char4  NO-DISPLAY,
            p_CNPJ_F TYPE char20  NO-DISPLAY,
            p_CNPJ_P TYPE char20  NO-DISPLAY,
            P_NF TYPE CHAR10  NO-DISPLAY,
            p_D_DE TYPE zlest0222-datatransb_de  NO-DISPLAY,
            P_d_ate TYPE zlest0222-datatransb_de NO-DISPLAY,
            P_Dt_EM TYPE CHAR8  NO-DISPLAY,
            p_prod TYPE char18 NO-DISPLAY,
            P_MES TYPE CHAR2  NO-DISPLAY,
            P_VALOR TYPE CHAR18  NO-DISPLAY.

PARAMETERS: p_db_01  TYPE tabname NO-DISPLAY,
            p_stc_01 TYPE tabname NO-DISPLAY,
            p_sc_01  TYPE c LENGTH 4 NO-DISPLAY,
            p_ti_01  TYPE cua_tit_tx NO-DISPLAY.

"Exits Customers Action
PARAMETERS: p_act_01 TYPE cua_tit_tx.
PARAMETERS: p_submit TYPE char20 NO-DISPLAY.



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


  PERFORM: f_selecionar_dados,
           f_processa_dados.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&  Include           ZSDR0117_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0117.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_cab_boletim_prod,
         saldo_vincular  TYPE zsdt0249-qtde_vinc,
         saldo_vinculado TYPE zsdt0249-qtde_vinc.
         INCLUDE STRUCTURE zsdt0246.

       TYPES: END OF ty_cab_boletim_prod.

TYPES: BEGIN OF ty_status_detail_bol,
         status TYPE c LENGTH 10,
       END OF ty_status_detail_bol.

TYPES: BEGIN OF ty_saida_0100_01,
         id_boletim          TYPE zsdt0247-id_boletim,
         tp_produto_producao TYPE zsdt0247-tp_produto_producao,
         ds_prod_consumo     TYPE c LENGTH 200,
         qtde_consumo        TYPE zsdt0247-qtde_consumo,
         unid_consumo        TYPE zsdt0247-unid_consumo,
         check_mat_terceiro  TYPE char01,
       END OF ty_saida_0100_01.

TYPES: BEGIN OF ty_saida_0100_02,
         id_boletim          TYPE zsdt0248-id_boletim,
         tp_produto_producao TYPE zsdt0247-tp_produto_producao,
         ds_prod_rendimento  TYPE c LENGTH 200,
         qtde                TYPE zsdt0248-qtde,
         qtde_mi             TYPE zsdt0248-qtde,
         qtde_me             TYPE zsdt0248-qtde,
         unid                TYPE zsdt0248-unid,
         perc_rendimento     TYPE zsdt0248-perc_rendimento,
         order_view          TYPE zsdt0248-order_view,
         celltab             TYPE lvc_t_styl,
       END OF ty_saida_0100_02.

TYPES: BEGIN OF ty_saida_0120_01,
         branch           TYPE j_1bbranch-branch,
         ds_branch        TYPE j_1bbranch-name,
         charg            TYPE zsdt0251-charg,
         saldo            TYPE zsdt0251-qtde_saldo,
         notas            TYPE zsdt0251_t,
         qtde_nf_emi_lim  TYPE i,
         saldo_nf_emi_lim TYPE zsdt0251-qtde_saldo,
       END OF ty_saida_0120_01.

TYPES: BEGIN OF ty_saida_0120_02,
         id_boletim         TYPE zsdt0249-id_boletim,
         bukrs              TYPE j_1bbranch-bukrs,
         branch             TYPE j_1bbranch-branch,
         ds_branch          TYPE j_1bbranch-name,
         charg              TYPE zsdt0249-charg,
         id_agrp            TYPE zsdt0249-id_agrp,
         saldo_vinc         TYPE zsdt0249-qtde_vinc,

         "Nota Fiscal Devolução
         seqlcto_devol      TYPE zsdt0252-seqlcto_devol,
         docnum_devol       TYPE zfiwrt0008-docnum,
         nfenum_devol       TYPE zfiwrt0008-nfenum,

         "Nota Fiscal Entrada Devolução
         seqlcto_ent_dev    TYPE zsdt0252-seqlcto_ent_dev,
         docnum_ent_dev     TYPE zfiwrt0008-docnum,
         nfenum_ent_dev     TYPE zfiwrt0008-nfenum,

         "Nota Fiscal Industrialização
         seqlcto_ind        TYPE zsdt0252-seqlcto_ind,
         docnum_ind         TYPE zfiwrt0008-docnum,
         nfenum_ind         TYPE zfiwrt0008-nfenum,

         "Nota Fiscal Entrada Industrialização
         seqlcto_ent_ind    TYPE zsdt0252-seqlcto_ent_ind,
         docnum_ent_ind     TYPE zfiwrt0008-docnum,
         nfenum_ent_ind     TYPE zfiwrt0008-nfenum,

         qtde_si_doc_01     TYPE zsdt0252-qtde_si_doc_01,

         "Documentos de Produção
         doc_prod_01        TYPE zsdt0252-doc_prod_01,
         ano_doc_prod_01    TYPE zsdt0252-ano_doc_prod_01,

         doc_prod_02        TYPE zsdt0252-doc_prod_02,
         ano_doc_prod_02    TYPE zsdt0252-ano_doc_prod_02,

         doc_prod_03        TYPE zsdt0252-doc_prod_03,
         ano_doc_prod_03    TYPE zsdt0252-ano_doc_prod_03,

         doc_prod_04        TYPE zsdt0252-doc_prod_04,
         ano_doc_prod_04    TYPE zsdt0252-ano_doc_prod_04,

         doc_prod_05        TYPE zsdt0252-doc_prod_05,
         ano_doc_prod_05    TYPE zsdt0252-ano_doc_prod_05,

         "Nota Fiscal RFL 01
         seqlcto_rfl_01     TYPE zsdt0252-seqlcto_rfl_01,
         docnum_rfl_01      TYPE zfiwrt0008-docnum,
         nfenum_rfl_01      TYPE zfiwrt0008-nfenum,

         "Nota Fiscal RFL 02
         seqlcto_rfl_02     TYPE zsdt0252-seqlcto_rfl_02,
         docnum_rfl_02      TYPE zfiwrt0008-docnum,
         nfenum_rfl_02      TYPE zfiwrt0008-nfenum,

         "Nota Fiscal RFL 03
         seqlcto_rfl_03     TYPE zsdt0252-seqlcto_rfl_03,
         docnum_rfl_03      TYPE zfiwrt0008-docnum,
         nfenum_rfl_03      TYPE zfiwrt0008-nfenum,

         "Nota Fiscal RCO 01
         seqlcto_rco_01     TYPE zsdt0252-seqlcto_rco_01,
         docnum_rco_01      TYPE zfiwrt0008-docnum,
         nfenum_rco_01      TYPE zfiwrt0008-nfenum,

         "Nota Fiscal Entrada RCO 01
         seqlcto_ent_rco_01 TYPE zsdt0252-seqlcto_ent_rco_01,
         docnum_ent_rco_01  TYPE zfiwrt0008-docnum,
         nfenum_ent_rco_01  TYPE zfiwrt0008-nfenum,

         "Nota Fiscal RCO 02
         seqlcto_rco_02     TYPE zsdt0252-seqlcto_rco_02,
         docnum_rco_02      TYPE zfiwrt0008-docnum,
         nfenum_rco_02      TYPE zfiwrt0008-nfenum,

         "Nota Fiscal Entrada RCO 02
         seqlcto_ent_rco_02 TYPE zsdt0252-seqlcto_ent_rco_02,
         docnum_ent_rco_02  TYPE zfiwrt0008-docnum,
         nfenum_ent_rco_02  TYPE zfiwrt0008-nfenum,

         "Nota Fiscal RCO 03
         seqlcto_rco_03     TYPE zsdt0252-seqlcto_rco_03,
         docnum_rco_03      TYPE zfiwrt0008-docnum,
         nfenum_rco_03      TYPE zfiwrt0008-nfenum,

         "Nota Fiscal Entrada RCO 03
         seqlcto_ent_rco_03 TYPE zsdt0252-seqlcto_ent_rco_03,
         docnum_ent_rco_03  TYPE zfiwrt0008-docnum,
         nfenum_ent_rco_03  TYPE zfiwrt0008-nfenum,

         color              TYPE kkblo_specialcol OCCURS 0,

       END OF ty_saida_0120_02.

TYPES: BEGIN OF ty_saida_0121,
         docnum TYPE j_1bnfdoc-docnum,
         nfenum TYPE j_1bnfdoc-nfenum,
         docdat TYPE j_1bnfdoc-docdat,
         saldo  TYPE j_1bnflin-menge,
       END OF ty_saida_0121.

TYPES: BEGIN OF ty_inf_qtde_soja_ind,
         quantidade TYPE zsdt0249-qtde_vinc,
         branch     TYPE zsdt0252-branch.
TYPES: END OF ty_inf_qtde_soja_ind.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

DATA: sdate         TYPE bapi_rm_datgen-postdate,
      it_msg_return TYPE TABLE OF zfiwrs0002,
      wa_msg_return TYPE zfiwrs0002,
      wa_mensagem   TYPE char30.



*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS         lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_alv_toolbar_0100_01 DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0100_01 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_alv_toolbar_0100_02 DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0100_02 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: "Adicionado CS2020001194
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_ucomm er_data_changed.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_alv_toolbar_0120_01 DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0120_01 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_alv_toolbar_0120_02 DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0120_02 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_alv_toolbar_0121 DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0121 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


*-----------------------------------------------------------------------------*
* Tela 0100
*-----------------------------------------------------------------------------*

DATA: obj_container_0100_01 TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0100_01   TYPE REF TO lcl_alv_toolbar_0100_01,
      obj_alv_0100_01       TYPE REF TO cl_gui_alv_grid,

      obj_container_0100_02 TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0100_02   TYPE REF TO lcl_alv_toolbar_0100_02,
      obj_alv_0100_02       TYPE REF TO cl_gui_alv_grid.

*-----------------------------------------------------------------------------*
* Tela 0120
*-----------------------------------------------------------------------------*
DATA: obj_splitter_0120     TYPE REF TO cl_gui_splitter_container,
      obj_container_0120    TYPE REF TO cl_gui_custom_container,

      obj_container_0120_01 TYPE REF TO cl_gui_container,
      obj_toolbar_0120_01   TYPE REF TO lcl_alv_toolbar_0120_01,
      obj_alv_0120_01       TYPE REF TO cl_gui_alv_grid,

      obj_container_0120_02 TYPE REF TO cl_gui_container,
      obj_toolbar_0120_02   TYPE REF TO lcl_alv_toolbar_0120_02,
      obj_alv_0120_02       TYPE REF TO cl_gui_alv_grid.

*-----------------------------------------------------------------------------*
* Tela 0121
*-----------------------------------------------------------------------------*

DATA: obj_container_0121 TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0121   TYPE REF TO lcl_alv_toolbar_0121,
      obj_alv_0121       TYPE REF TO cl_gui_alv_grid.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.



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

DATA: gt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
      gt_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

DATA: go_alarm TYPE REF TO lcl_event_receiver,
      go_clock TYPE REF TO cl_gui_timer.

*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata,
      tl_bdc     TYPE TABLE OF bdcdata,
      wl_bdc     TYPE bdcdata,
      opt        TYPE ctu_params.

DATA: objeto TYPE REF TO zif_boletim_producao.


*----------------------------------------------------------------------*
* Variáveis Controle Tela
*----------------------------------------------------------------------*

DATA: vg_operacao_bol TYPE c LENGTH 100.

*----------------------------------------------------------------------*
* Variáveis de Telas
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Constantes Telas
*----------------------------------------------------------------------*
DATA: vg_dynnr_20xx TYPE sydynnr,
      vg_dynnr_30xx TYPE sydynnr,
      vg_dynnr_2001 TYPE sydynnr VALUE '2001',
      vg_dynnr_2002 TYPE sydynnr VALUE '2002',
      vg_dynnr_0120 TYPE sydynnr VALUE '0120'.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
DATA: c_max            TYPE c LENGTH 4 VALUE 'MAX',
      c_min            TYPE c LENGTH 4 VALUE 'MIN',

      c_new_bol        TYPE c LENGTH 50 VALUE 'NEW_BOL',
      c_edit_bol       TYPE c LENGTH 50 VALUE 'EDIT_BOL',
      c_save_bol       TYPE c LENGTH 50 VALUE 'SAVE_BOL',
      c_del_bol        TYPE c LENGTH 50 VALUE 'DEL_BOL',
      c_search_bol     TYPE c LENGTH 50 VALUE 'SEARCH_BOL',
      c_view_bol       TYPE c LENGTH 50 VALUE 'VIEW_BOL',
      c_canc_edit      TYPE c LENGTH 50 VALUE 'CANC_EDIT',
      c_atua_bol       TYPE c LENGTH 50 VALUE 'ATUA_BOL',
      c_aprov_bol      TYPE c LENGTH 50 VALUE 'APROV_BOL',
      c_desaprov_bol   TYPE c LENGTH 50 VALUE 'DESAP_BOL',
      c_disable_tim    TYPE c LENGTH 50 VALUE 'DISABL_TIM',
      c_enable_tim     TYPE c LENGTH 50 VALUE 'ENABLE_TIM',
      bt_dados_ads     TYPE c LENGTH 50 VALUE 'BT_DS',
      c_sel_tp_boletim TYPE c LENGTH 50 VALUE 'SEL_TP_B',
      c_sel_show_msg   TYPE c LENGTH 50 VALUE 'SHOW_MSG',
      c_time_interval  TYPE i VALUE 5,
      g_desc_titulo    TYPE char40.

*----------------------------------------------------------------------*
* Internal Tables and Workarea
*----------------------------------------------------------------------*

DATA: wg_cab_boletim_prod TYPE ty_cab_boletim_prod,

      wg_refresh_splitter TYPE c,

      it_saida_0100_01    TYPE TABLE OF ty_saida_0100_01,
      wa_saida_0100_01    TYPE ty_saida_0100_01,

      it_saida_0100_02    TYPE TABLE OF ty_saida_0100_02,
      wa_saida_0100_02    TYPE ty_saida_0100_02,

      it_saida_0120_01    TYPE TABLE OF ty_saida_0120_01,
      wa_saida_0120_01    TYPE ty_saida_0120_01,

      it_saida_0120_02    TYPE TABLE OF ty_saida_0120_02,
      wa_saida_0120_02    TYPE ty_saida_0120_02,

      it_saida_0121       TYPE TABLE OF ty_saida_0121,
      wa_saida_0121       TYPE ty_saida_0121.

DATA: t_celltab  TYPE lvc_t_styl,       "Adicionado CS2020001194
      t_celltab2 TYPE lvc_t_styl.       "Adicionado CS2020001194

DATA: wa_zsdt0246          TYPE zsdt0246,

      it_zsdt0247          TYPE zsdt0247_t,
      wa_zsdt0247          TYPE zsdt0247,

      it_zsdt0247_aux      TYPE zsdt0247_t,
      wa_zsdt0247_aux      TYPE zsdt0247,

      it_zsdt0248          TYPE zsdt0248_t,
      wa_zsdt0248          TYPE zsdt0248,

      it_zsdt0248_aux      TYPE zsdt0248_t,
      wa_zsdt0248_aux      TYPE zsdt0248,

      wa_zsdt0253          TYPE zsdt0253,

      it_zsdt0251          TYPE TABLE OF zsdt0251 WITH HEADER LINE,
      it_zsdt0251_agr      TYPE TABLE OF zsdt0251 WITH HEADER LINE,

      it_zsdt0249          TYPE TABLE OF zsdt0249 WITH HEADER LINE,
      it_zsdt0249_agr      TYPE TABLE OF zsdt0249 WITH HEADER LINE,

      it_zsdt0252          TYPE TABLE OF zsdt0252 WITH HEADER LINE,
      it_zsdt0252_aux      TYPE TABLE OF zsdt0252 WITH HEADER LINE,

      it_j_1bbranch        TYPE TABLE OF j_1bbranch WITH HEADER LINE,

      wg_inf_qtde_soja_ind TYPE ty_inf_qtde_soja_ind.

DATA: tg_exc_tcode_0100 TYPE TABLE OF sy-tcode,
      tg_exc_tcode_0120 TYPE TABLE OF sy-tcode.

DATA: l_saldo_ok              TYPE c,
      l_saldo_erro            TYPE c,
      tp_boletim              TYPE zsdt0246-tp_boletim,
      b_farelo                TYPE char01,
      b_biodiesel             TYPE char01,
      b_oleo_neutro           TYPE char01,
      _param                  TYPE  ustyp_t_parameters,
      r_parid                 TYPE RANGE OF memoryid,
      qtde_ent_soja_ind       TYPE char50,
      check_auth_b_farelo     TYPE char01,
      check_auth_b_biodiesel  TYPE char01,
      check_auth_b_oleo_netro TYPE char01,
      g_qtde_si_aux           TYPE zsdt0246-qtde_si.



START-OF-SELECTION.

  PERFORM fm_check_user.

*  CALL SCREEN 0100.

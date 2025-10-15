FUNCTION-POOL znfe_inbound MESSAGE-ID znfe_distri.

* INCLUDE LZNFE_INBOUNDD...                  " Local class definition

TABLES: zmmt0072,
        zmmt0073,
        zmmt0075,
        zmmt0076,
        zmmt0077,
        zmmt0119,
        zde_zmmt0073_alv,
        zde_zmmt0075_alv,
        zde_zmmt0076_alv,
        zde_zmmt0077_alv,
        zde_zmmt0119_alv,
        zib_nfe_dist_ter,
        zib_nfe_dist_itm,
        zde_nfe_dist_alv,
        zib_nfe_dist_lot,
        zde_nfe_inbound_alv,
        zde_nfe_dist_itm_alv,
        zib_nfe_dist_frt,
        zde_nfe_dist_itm_preco_ped.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1502 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_data_changed_1502 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS handle_f4_1502 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data et_bad_cells.
ENDCLASS.

TYPES: BEGIN OF ty_info_forne.
TYPES: texto TYPE char50,   "Fornecedor
       bankl TYPE char03, "Banco
       banka TYPE banka,  "Nome do Banco
       bankn TYPE bankn,  "Conta Corrente
       agenc TYPE char15. "Agencia
TYPES: END OF ty_info_forne.

TYPES: BEGIN OF ty_itens_alv.
         INCLUDE STRUCTURE zde_nfe_dist_itm_alv.
TYPES:   precos TYPE char04.
TYPES: line_color(4) TYPE c, "Used to store row color attributes
       color_cell    TYPE lvc_t_scol,  " Cell color
       style         TYPE lvc_t_styl,
       END OF ty_itens_alv.

TYPES: BEGIN OF ty_itens_lotes_alv.
         INCLUDE STRUCTURE zib_nfe_dist_lot.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         indea(4),
       END OF ty_itens_lotes_alv.

TYPES: BEGIN OF ty_itens_carac_alv.
         INCLUDE STRUCTURE zib_nfe_dist_lca.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
       END OF ty_itens_carac_alv.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.

CLASS lcl_alv_toolbar_1503 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_1603 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: event_receiver_1502 TYPE REF TO lcl_event_receiver_1502.

DATA: cl_grid_1503        TYPE REF TO cl_gui_alv_grid,
      "CL_CONTAINER_1503 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      container_1503      TYPE REF TO cl_gui_custom_container,
      it_function_1503    TYPE ui_functions,
      wa_layout_1503      TYPE lvc_s_layo,
      it_fieldcat_1503    TYPE lvc_t_fcat,
      it_sort_1503        TYPE lvc_t_sort,
      wa_variant_1503     TYPE disvariant,
      wa_stable_1503      TYPE lvc_s_stbl,
      it_selected_1503    TYPE lvc_t_row,
      wa_selected_1503    TYPE lvc_s_row,
      it_f4_1503          TYPE lvc_t_f4,
      wa_f4_1503          TYPE lvc_s_f4,
      obg_toolbar_1503    TYPE REF TO lcl_alv_toolbar_1503,
      toolbarmanager_1503 TYPE REF TO cl_alv_grid_toolbar_manager,
      it_itens_alv_sel    TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      tl_tlines           LIKE tline OCCURS 0 WITH HEADER LINE,
      longtext_tab        TYPE zde_tdline_t,
      longtext            TYPE zde_tdline,
      editor              TYPE REF TO cl_gui_textedit,
      container           TYPE REF TO cl_gui_custom_container,
      editor_18001        TYPE REF TO cl_gui_textedit,          "RJF
      container_18001     TYPE REF TO cl_gui_custom_container,  "RJF
      tl_tlines1          LIKE tline OCCURS 0 WITH HEADER LINE, "RJF
      longtext_tab1       TYPE zde_tdline_t,                    "RJF
      longtext1           TYPE zde_tdline,                      "RJF
      editor_18002        TYPE REF TO cl_gui_textedit,          "RJF
      container_18002     TYPE REF TO cl_gui_custom_container,  "RJF
      tl_tlines2          LIKE tline OCCURS 0 WITH HEADER LINE, "RJF
      longtext_tab2       TYPE zde_tdline_t,                    "RJF
      longtext2           TYPE zde_tdline,                      "RJF
      editor_18003        TYPE REF TO cl_gui_textedit,          "RJF
      container_18003     TYPE REF TO cl_gui_custom_container,  "RJF
      it_det              TYPE zintg_det,                       "RJF
      lv_infcpl           TYPE string,                          "RJF
      lv_infadfisco       TYPE string,                          "RJF
      tl_tlines3          LIKE tline OCCURS 0 WITH HEADER LINE, "RJF
      longtext_tab3       TYPE zde_tdline_t,                    "RJF
      longtext3           TYPE zde_tdline,                      "RJF
      custom_grid1        TYPE REF TO cl_gui_custom_container,  "RJF
      grid1               TYPE REF TO cl_gui_alv_grid,          "RJF
      gv_open_text        TYPE c,                               "RJF
      ex_nfe_inbound      TYPE REF TO zcx_nfe_inbound_exception,
      ex_pedido_compra    TYPE REF TO zcx_pedido_compra_exception,
      ex_cadastro         TYPE REF TO zcx_cadastro,
      ex_charg            TYPE REF TO zcx_charg_exception,
      ex_miro             TYPE REF TO zcx_miro_exception,
      event_handler       TYPE REF TO lcl_event_handler,
      ck_informado_preco  TYPE char01,
*      ck_nrosol           TYPE numc5, " RIM CS1029457 ANB 30.09.2022
      ck_nrosol           TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
      ck_seq              TYPE numc3.

DATA: lbl_item_sel TYPE c LENGTH 259.
"TASK
DATA: results_received(1) TYPE c,
      tsk_messagem_erro   TYPE  string,
      tsk_sucesso         TYPE  char01,
      tsk_belnr           TYPE  re_belnr,
      tsk_gjahr           TYPE  gjahr.
"TASK


*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1502 IMPLEMENTATION.
  METHOD handle_data_changed_1502.
    PERFORM data_changed_1502 USING er_data_changed.
  ENDMETHOD.

  METHOD handle_f4_1502.
    PERFORM f4_1502 USING e_fieldname es_row_no er_event_data et_bad_cells.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1601 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click_1601  FOR EVENT double_click   OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1603 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click_1603 FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
    METHODS handle_hotspot_click_1603 FOR EVENT  hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id.
ENDCLASS.

CLASS lcl_event_receiver_1602 DEFINITION.
  PUBLIC SECTION.
    DATA: error_in_data TYPE c.
    METHODS handle_data_changed_1602 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
*-CS2025000249-08.04.2025-#173180-JT-inicio
    METHODS on_f4_1602 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
*-CS2025000249-08.04.2025-#173180-JT-fim

  PRIVATE SECTION.
    METHODS: perform_semantic_checks IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.

DATA: event_receiver_1601 TYPE REF TO lcl_event_receiver_1601.
DATA: event_receiver_1602 TYPE REF TO lcl_event_receiver_1602.
DATA: event_receiver_1603 TYPE REF TO lcl_event_receiver_1603.

DATA: cl_grid_1601     TYPE REF TO cl_gui_alv_grid,
      container_1601   TYPE REF TO cl_gui_custom_container,
      it_function_1601 TYPE ui_functions,
      wa_layout_1601   TYPE lvc_s_layo,
      it_fieldcat_1601 TYPE lvc_t_fcat,
      it_sort_1601     TYPE lvc_t_sort,
      wa_variant_1601  TYPE disvariant,
      wa_stable_1601   TYPE lvc_s_stbl,
      it_f4_1601       TYPE lvc_t_f4,
      wa_f4_1601       TYPE lvc_s_f4.

DATA: cl_grid_1602     TYPE REF TO cl_gui_alv_grid,
      container_1602   TYPE REF TO cl_gui_custom_container,
      it_function_1602 TYPE ui_functions,
      wa_layout_1602   TYPE lvc_s_layo,
      it_fieldcat_1602 TYPE lvc_t_fcat,
      it_sort_1602     TYPE lvc_t_sort,
      wa_variant_1602  TYPE disvariant,
      wa_stable_1602   TYPE lvc_s_stbl,
      it_f4_1602       TYPE lvc_t_f4,
      wa_f4_1602       TYPE lvc_s_f4.

DATA: cl_grid_1603        TYPE REF TO cl_gui_alv_grid,
      container_1603      TYPE REF TO cl_gui_custom_container,
      it_function_1603    TYPE ui_functions,
      wa_layout_1603      TYPE lvc_s_layo,
      it_fieldcat_1603    TYPE lvc_t_fcat,
      it_sort_1603        TYPE lvc_t_sort,
      wa_variant_1603     TYPE disvariant,
      wa_stable_1603      TYPE lvc_s_stbl,
      it_f4_1603          TYPE lvc_t_f4,
      wa_f4_1603          TYPE lvc_s_f4,
      obg_toolbar_1603    TYPE REF TO lcl_alv_toolbar_1603,
      toolbarmanager_1603 TYPE REF TO cl_alv_grid_toolbar_manager,
      it_selected_1603    TYPE lvc_t_row,
      wa_selected_1603    TYPE lvc_s_row,
      lv_error            TYPE c,
      t_bdcdata           TYPE TABLE OF bdcdata.

DATA: it_zmmt0072_sel TYPE TABLE OF zmmt0072 WITH HEADER LINE,
      it_zmmt0073_sel TYPE TABLE OF zde_zmmt0073_alv WITH HEADER LINE,
      it_zmmt0075_sel TYPE TABLE OF zde_zmmt0075_alv WITH HEADER LINE,
      it_zmmt0076_sel TYPE TABLE OF zde_zmmt0076_alv WITH HEADER LINE,
      it_zmmt0077_sel TYPE TABLE OF zde_zmmt0077_alv WITH HEADER LINE,
      it_zmmt0119_sel TYPE TABLE OF zde_zmmt0119_alv WITH HEADER LINE.

CONSTANTS: ok_gravar      TYPE sy-ucomm VALUE 'GRAVAR',
           ok_cancelar    TYPE sy-ucomm VALUE 'CANCELAR',
           ok_desfazer    TYPE sy-ucomm VALUE 'DESFAZER',
           ok_confirmar   TYPE sy-ucomm VALUE 'CONFIRMAR',
           ok_cancel      TYPE sy-ucomm VALUE 'CANCEL',
           ok_pnovo       TYPE sy-ucomm VALUE 'PNOVO',
           ok_peditar     TYPE sy-ucomm VALUE 'PEDITAR',
           ok_pabrir      TYPE sy-ucomm VALUE 'PABRIR',
           ok_pexcluir    TYPE sy-ucomm VALUE 'PEXCLUIR',
           ok_atualizar   TYPE sy-ucomm VALUE 'ATUALIZAR',
           ok_vincula_mt  TYPE sy-ucomm VALUE 'VINCULA_MT',
           ok_aceite_can  TYPE sy-ucomm VALUE 'ACEITE_CAN',
           ok_aceite_doc  TYPE sy-ucomm VALUE 'ACEITE_DOC',
           ok_aceite_nao  TYPE sy-ucomm VALUE 'ACEITE_NAO',

           ok_fisico_can  TYPE sy-ucomm VALUE 'FISICO_CAN',
           ok_arma_fis    TYPE sy-ucomm VALUE 'ARMA_FIS',
           ok_arma_est    TYPE sy-ucomm VALUE 'ARMA_EST',
           ok_addtrans    TYPE sy-ucomm VALUE 'ADDTRANS',

           ok_devo_fis    TYPE sy-ucomm VALUE 'DEVO_FIS',
           ok_devo_est    TYPE sy-ucomm VALUE 'DEVO_EST',
           ok_destinacao  TYPE sy-ucomm VALUE 'DESTINACAO',
           ok_arma_fisp   TYPE sy-ucomm VALUE 'ARMA_FISP',
           ok_devo_fisp   TYPE sy-ucomm VALUE 'DEVO_FISP',

           ok_fisico_act  TYPE sy-ucomm VALUE 'ACEITE_FIS',
           ok_aviso_act   TYPE sy-ucomm VALUE 'AVISO_FIS',
           ok_aviso_can   TYPE sy-ucomm VALUE 'AVISO_CAN',
           ok_fatura_inf  TYPE sy-ucomm VALUE 'FATURA_INF',
           ok_fatura_can  TYPE sy-ucomm VALUE 'FATURA_CAN',
           ok_fatura_act  TYPE sy-ucomm VALUE 'FATURA_DOC',
           ok_reini_lote  TYPE sy-ucomm VALUE 'REINI_LOTE',  "*-CS2025000249-07.05.2025-#174157-JT

           ok_obs_fiscal  TYPE sy-ucomm VALUE 'OBS_FISCAL',
           ok_obs_contri  TYPE sy-ucomm VALUE 'OBS_CONTRI',
           ok_save        TYPE sy-ucomm VALUE 'SAVE',
           ok_gerar_trasp TYPE sy-ucomm VALUE 'GERAR_TRANSP'.

CONSTANTS: ok_aprovar TYPE sy-ucomm VALUE 'APROVAR',
           ok_recusar TYPE sy-ucomm VALUE 'RECUSAR'.

CONSTANTS: cs_line_color_selecionada TYPE c LENGTH 4 VALUE 'C710'.

DATA: obj_nfe_inbound  TYPE REF TO zcl_nfe_inbound,
      wa_nfe_inbound   TYPE znfe_inbound,
      obj_departamento TYPE REF TO zcl_mm_departamento,
      ck_consulta      TYPE c LENGTH 1,
      ck_gravado       TYPE c LENGTH 1,
      i_campo	         TYPE name_feld,
      gb_texto_obs     TYPE c LENGTH 40.

DATA: ok_code                    TYPE sy-ucomm,
      lc_filtro                  TYPE zde_zmmt0072_filtro,
      it_zmmt0072                TYPE TABLE OF zmmt0072,
      ck_novo_departamento       TYPE c LENGTH 1,
      wa_zmmt0072                TYPE zmmt0072,
      wa_zmmt0073                TYPE zmmt0073,
      wa_zmmt0075                TYPE zmmt0075,
      wa_zmmt0076                TYPE zmmt0076,
      wa_zmmt0077                TYPE zmmt0077,
      wa_zmmt0119                TYPE zmmt0119,
      it_zmmt0073                TYPE zde_zmmt0073_t,
      it_zmmt0075                TYPE zde_zmmt0075_t,
      it_zmmt0076                TYPE zde_zmmt0076_t,
      it_zmmt0077                TYPE zde_zmmt0077_t,
      it_zmmt0119                TYPE zde_zmmt0119_t,
      it_zmmt0073_alv            TYPE TABLE OF zde_zmmt0073_alv WITH HEADER LINE,
      it_zmmt0075_alv            TYPE TABLE OF zde_zmmt0075_alv WITH HEADER LINE,
      it_zmmt0076_alv            TYPE TABLE OF zde_zmmt0076_alv WITH HEADER LINE,
      it_zmmt0077_alv            TYPE TABLE OF zde_zmmt0077_alv WITH HEADER LINE,
      it_zmmt0119_alv            TYPE TABLE OF zde_zmmt0119_alv WITH HEADER LINE,
      it_dd07v                   TYPE TABLE OF dd07v,
      ck_selecionou_aprovacao    TYPE char01,
      ck_view_aprovacao          TYPE char01,
      wa_ultima_aprovacao        TYPE zib_nfe_dist_eap,
      sel_tp_autorizacao         TYPE zde_tp_autorizacao,
      wa_dd07v                   TYPE dd07v,
      ck_alterou_departamento    TYPE c LENGTH 1,
      ck_alterou_transportador   TYPE c LENGTH 1,
      ck_alterou_grupo           TYPE c LENGTH 1,
      ck_alterou_class           TYPE c LENGTH 1,
      ck_alterou_tipo            TYPE c LENGTH 1,
      ck_alterou_tipo_movimento  TYPE c LENGTH 1,
      ck_alterou_tipo_documento  TYPE c LENGTH 1,
      ck_alterou_form_bloqueio   TYPE c LENGTH 1,
      ck_alterou_armazem         TYPE c LENGTH 1,
      ck_alterou_lote            TYPE c LENGTH 1,
      ck_alterou_lote_info       TYPE c LENGTH 1,
      ck_alterou_cfop            TYPE c LENGTH 1,
      ck_alterou_cfop_retorno    TYPE c LENGTH 1,
      ck_alterou_nr_fase         TYPE c LENGTH 1,
      ck_alterou_dt_vencimento   TYPE c LENGTH 1,
      ck_alterou_zbvtyp          TYPE c LENGTH 1,
      ck_alterou_zlspr           TYPE c LENGTH 1,
      ck_alterou_pymt_meth       TYPE c LENGTH 1,
      ck_alterou_housebankid     TYPE c LENGTH 1,
      ck_alterou_vlr_desconto    TYPE c LENGTH 1,
      ck_alterou_obs_financeira  TYPE c LENGTH 1,
      ck_alterou_boleto          TYPE c LENGTH 1,
      ck_entrada_estoque         TYPE c LENGTH 1,
      ck_informado_grupo         TYPE c LENGTH 1,
      ck_informado_tipo          TYPE c LENGTH 1,
      ck_informado_armazem       TYPE c LENGTH 1,
      ck_informado_cfop          TYPE c LENGTH 1,
      ck_informado_cfop_retorno  TYPE c LENGTH 1,
      cl_informado_new_item      TYPE c LENGTH 1,
      ck_informado_vinc_retorno  TYPE c LENGTH 1,
      it_ucomm                   TYPE TABLE OF sy-ucomm,
      wa_itens                   TYPE zib_nfe_dist_itm,
      it_itens                   TYPE zib_nfe_dist_itm_t,
      it_lotes                   TYPE zib_nfe_dist_lot_t,
      it_lotes_c                 TYPE zib_nfe_dist_lca_t,
      it_itens_alv               TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      wa_itens_sel_lote          TYPE ty_itens_alv,
      it_lotes_alv_t             TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_lotes_alv_u             TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_lotes_alv_sel           TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_carac_alv               TYPE TABLE OF ty_itens_carac_alv WITH HEADER LINE,
      it_carac_alv_u             TYPE TABLE OF ty_itens_carac_alv WITH HEADER LINE,
      lc_info_forne              TYPE ty_info_forne,
      lt_texto_bloqueio_pagmento TYPE textl_008,
      lt_texto_forma_de_pagmento TYPE text1_042z.

CONSTANTS: tl_tela_1501 TYPE sy-dynnr VALUE '1501',
           tl_tela_1502 TYPE sy-dynnr VALUE '1502',
           tl_tela_1503 TYPE sy-dynnr VALUE '1503',
           tl_tela_1504 TYPE sy-dynnr VALUE '1504',
           tl_tela_1601 TYPE sy-dynnr VALUE '1601',
           tl_tela_1602 TYPE sy-dynnr VALUE '1602'.

DATA: sb_tela_1501 TYPE sy-dynnr,
      sb_tela_1502 TYPE sy-dynnr,
      sb_tela_1503 TYPE sy-dynnr,
      sb_tela_1504 TYPE sy-dynnr,
      sb_tela_1600 TYPE sy-dynnr,
      sb_tela_1601 TYPE sy-dynnr.

DATA: lc_txt_butxt               TYPE t001-butxt,
      ck_alterou_ck_possui_frete TYPE char01.

DATA: lc_transportador_cnpj  TYPE zde_arma_cnpj,
      lc_transportador_ie    TYPE zde_arma_insc_es,
      lc_transportador_razao TYPE zde_arma_razao,
      lc_transportador_regio TYPE regio,
      lc_fornecedor_regio    TYPE regio,
      lc_destinatario_regio  TYPE regio,
      lc_armazem_regio       TYPE regio.

CLASS lcl_alv_toolbar_1503 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1503
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    DATA(ck_permissao) = abap_true.

    IF zib_nfe_dist_ter-ck_fiscal NE abap_true.
      ck_permissao = abap_false.
    ENDIF.

*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1503->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_itens_alv_sel, it_itens_alv_sel[].

    CALL METHOD cl_grid_1503->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1503.

    LOOP AT it_selected_1503 INTO wa_selected_1503.
      READ TABLE it_itens_alv INTO DATA(lc_item_alv) INDEX wa_selected_1503-index.
      APPEND lc_item_alv TO it_itens_alv_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_item.
      WHEN 'DEL'.
        PERFORM deletar_item.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    IF e_column_id-fieldname = 'PRECOS'.
      READ TABLE it_itens_alv INDEX es_row_no-row_id INTO DATA(wa_item).
      IF wa_item-ebeln IS INITIAL OR wa_item-ebelp IS INITIAL.
        MESSAGE 'Deve ser inbformado o Pedido/Item da linha da nota fiscal' TYPE 'S'.
        EXIT.
      ELSE.

        SELECT SINGLE * INTO @DATA(wa_ekko)
          FROM ekko
         WHERE ebeln EQ @wa_item-ebeln.

        IF wa_ekko-waers EQ 'BRL'.
          MESSAGE 'Pedido deve ser difernte de BRL' TYPE 'S'.
          EXIT.
        ENDIF.

        ck_informado_preco = abap_false.

        IF wa_item-brtwr IS INITIAL.

          SELECT SINGLE * INTO @DATA(wa_zmmt0035)
            FROM zmmt0035
           WHERE ebeln EQ @wa_item-ebeln.

          IF sy-subrc IS INITIAL.
            SELECT SINGLE nro_sol_cp, ebelp, brtwr, netpr, bicms, picms INTO @DATA(wa_zmmt0037)
              FROM zmmt0037
             WHERE nro_sol_cp EQ @wa_zmmt0035-nro_sol_cp
               AND ebelp      EQ @wa_item-ebelp.
            IF sy-subrc IS INITIAL.
              wa_item-brtwr   = wa_zmmt0037-brtwr.
              wa_item-bicms   = wa_zmmt0037-bicms.
              wa_item-picms   = wa_zmmt0037-picms.
              "WA_ITEM-BPIS    = .
              "WA_ITEM-PPIS    = .
              "WA_ITEM-BCOFINS = .
              "WA_ITEM-PCOFINS = .
            ENDIF.
          ELSE.

            SELECT SINGLE * INTO @DATA(wa_item_informado)
              FROM zib_nfe_dist_itm
             WHERE ebeln EQ @wa_item-ebeln
               AND ebelp EQ @wa_item-ebelp
               AND brtwr LT 0.

            IF sy-subrc IS INITIAL.
              wa_item-brtwr   = wa_item_informado-brtwr.
              wa_item-bicms   = wa_item_informado-bicms.
              wa_item-picms   = wa_item_informado-picms.
              wa_item-bpis    = wa_item_informado-bpis.
              wa_item-ppis    = wa_item_informado-ppis.
              wa_item-bcofins = wa_item_informado-bcofins.
              wa_item-pcofins = wa_item_informado-pcofins.
            ENDIF.

          ENDIF.
        ENDIF.

        zde_nfe_dist_itm_preco_ped-brtwr    = wa_item-brtwr.
        zde_nfe_dist_itm_preco_ped-bicms    = wa_item-bicms.
        zde_nfe_dist_itm_preco_ped-picms    = wa_item-picms.
        zde_nfe_dist_itm_preco_ped-bpis     = wa_item-bpis.
        zde_nfe_dist_itm_preco_ped-ppis     = wa_item-ppis.
        zde_nfe_dist_itm_preco_ped-bcofins  = wa_item-bcofins.
        zde_nfe_dist_itm_preco_ped-pcofins  = wa_item-pcofins.

        zde_nfe_dist_itm_preco_ped-liquido = ( ( zde_nfe_dist_itm_preco_ped-brtwr -
                                (
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bicms   / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-picms   / 100 ) )  +
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bpis    / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-ppis    / 100 ) )  +
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bcofins / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-pcofins / 100 ) )
                                )
                              ) ).
        "Busca valor do pedido 04/04/2025 ALRS
        SELECT SINGLE * INTO @DATA(wa_item_ter)
              FROM zib_nfe_dist_ter
             WHERE ebeln EQ @wa_item_informado-chave_nfe.

        IF zde_nfe_dist_itm_preco_ped-brtwr  IS INITIAL.
          SELECT SINGLE netpr mwskz werks matnr
            INTO ( zde_nfe_dist_itm_preco_ped-brtwr, wa_item_ter-mwskz, wa_item_ter-f_tomadora, wa_item_informado-matnr )
            FROM ekpo
            WHERE ebeln EQ wa_item-ebeln
             AND ebelp  EQ wa_item-ebelp.

          IF wa_item_ter-p_emissor IS INITIAL.
            SELECT SINGLE lifnr
              FROM lfa1
              INTO wa_item_ter-p_emissor
              WHERE stcd1 = wa_item_ter-forne_cnpj.
          ENDIF.
          IF wa_item_ter-p_emissor IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(wa_lfa1)
               FROM lfa1
              WHERE lifnr EQ @wa_item_ter-p_emissor.

            DATA wa_kna1             TYPE kna1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_item_ter-f_tomadora
              IMPORTING
                output = wa_kna1-kunnr.

            SELECT SINGLE * INTO wa_kna1
              FROM kna1
             WHERE kunnr EQ wa_kna1-kunnr.

            CALL METHOD zcl_miro=>get_taxas_iva
              EXPORTING
                i_iva                   = wa_item_ter-mwskz
                i_data                  = wa_item_ter-dt_emissao
                i_bbranc                = wa_item_ter-f_tomadora
                i_shipfrom              = wa_lfa1-regio
                i_shipto                = wa_kna1-regio
                i_matnr                 = wa_item_informado-matnr
                i_icms_base             = wa_item_ter-vl_icms_base
                i_valor_icms            = wa_item_ter-vl_icms_total
                i_werks                 = wa_item_ter-f_tomadora
              IMPORTING
                e_rate_icms             = DATA(e_rate_icms)
                e_base_icms             = DATA(e_base_icms)
                e_rate_pis              = DATA(e_rate_pis)
                e_rate_cofins           = DATA(e_rate_cofins)
                e_rate_icms_diferencial = DATA(e_rate_icms_df).

            zde_nfe_dist_itm_preco_ped-bicms    = e_base_icms.
            zde_nfe_dist_itm_preco_ped-picms    = e_rate_icms.
            zde_nfe_dist_itm_preco_ped-bpis     = 0.
            zde_nfe_dist_itm_preco_ped-ppis     = e_rate_pis.
            zde_nfe_dist_itm_preco_ped-bcofins  = 0.
            zde_nfe_dist_itm_preco_ped-pcofins  = e_rate_cofins.
          ENDIF.
        ENDIF.
        "Busca valor do pedido 04/04/2025 ALRS
        CALL SCREEN 1504 STARTING AT 05 05.

        IF ck_informado_preco EQ abap_true.

          CLEAR: ck_informado_preco.

          LOOP AT it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>)
              WHERE ebeln EQ wa_item-ebeln
                AND ebelp EQ wa_item-ebelp.

            <fs_item>-brtwr    = zde_nfe_dist_itm_preco_ped-brtwr.
            <fs_item>-bicms    = zde_nfe_dist_itm_preco_ped-bicms.
            <fs_item>-picms    = zde_nfe_dist_itm_preco_ped-picms.
            <fs_item>-bpis     = zde_nfe_dist_itm_preco_ped-bpis.
            <fs_item>-ppis     = zde_nfe_dist_itm_preco_ped-ppis.
            <fs_item>-bcofins  = zde_nfe_dist_itm_preco_ped-bcofins.
            <fs_item>-pcofins  = zde_nfe_dist_itm_preco_ped-pcofins.
            <fs_item>-liquido  = ( ( zde_nfe_dist_itm_preco_ped-brtwr -
                                (
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bicms   / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-picms   / 100 ) )  +
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bpis    / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-ppis    / 100 ) )  +
                                  ( ( zde_nfe_dist_itm_preco_ped-brtwr * ( zde_nfe_dist_itm_preco_ped-bcofins / 100 ) ) * ( zde_nfe_dist_itm_preco_ped-pcofins / 100 ) )
                                  )
                                 ) ).

            CALL METHOD obj_nfe_inbound->set_item_material_preco
              EXPORTING
                i_prod_item    = <fs_item>-prod_item    " Nº item do documento
                i_preco_pedido = zde_nfe_dist_itm_preco_ped. " Inf. Pedido em Moeda Estrangeira

            UPDATE zib_nfe_dist_itm
               SET brtwr    = zde_nfe_dist_itm_preco_ped-brtwr
                   bicms    = zde_nfe_dist_itm_preco_ped-bicms
                   picms    = zde_nfe_dist_itm_preco_ped-picms
                   bpis     = zde_nfe_dist_itm_preco_ped-bpis
                   ppis     = zde_nfe_dist_itm_preco_ped-ppis
                   bcofins  = zde_nfe_dist_itm_preco_ped-bcofins
                   pcofins  = zde_nfe_dist_itm_preco_ped-pcofins
                   liquido  = zde_nfe_dist_itm_preco_ped-liquido
             WHERE chave_nfe EQ <fs_item>-chave_nfe
               AND prod_item EQ <fs_item>-prod_item.

          ENDLOOP.

          COMMIT WORK AND WAIT.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


CLASS lcl_alv_toolbar_1603 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1603
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    DATA(ck_permissao) = abap_false.

    IF zib_nfe_dist_ter-st_fisico NE zcl_nfe_inbound=>st_fisico_00.
      ck_permissao = abap_true.
    ENDIF.

*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1603->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_lotes_alv_sel, it_lotes_alv_sel[].

    CALL METHOD cl_grid_1603->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1603.

    LOOP AT it_selected_1603 INTO wa_selected_1603.
      READ TABLE it_lotes_alv_u INTO DATA(lc_lote_alv) INDEX wa_selected_1603-index.
      APPEND lc_lote_alv TO it_lotes_alv_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_lote.
      WHEN 'DEL'.
        PERFORM deletar_lote.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1601 IMPLEMENTATION.
  METHOD handle_double_click_1601.
    PERFORM double_click_1601 USING e_row e_column es_row_no.
  ENDMETHOD.


ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1603 IMPLEMENTATION.
  METHOD handle_double_click_1603.
    PERFORM double_click_1603 USING e_row e_column es_row_no.
  ENDMETHOD.

  METHOD  handle_hotspot_click_1603.

    DATA: t_carac TYPE zib_nfe_dist_lca_t,  "*-CS2025000249-17.04.2025-#173311-JT
          w_carac TYPE zib_nfe_dist_lca.    "*-CS2025000249-17.04.2025-#173311-JT

    DATA line_id TYPE zmmt0102-line_id.
    READ TABLE it_lotes_alv_u INDEX e_row_id-index INTO DATA(wa_lotes_alv_u).
*    LINE_ID = SY-TABIX.
    line_id = wa_lotes_alv_u-prod_item.
    IF wa_lotes_alv_u-charg  = zib_nfe_dist_lot-charg.
      READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY chave_nfe = wa_lotes_alv_u-chave_nfe
                                                                         prod_item = wa_lotes_alv_u-prod_item.
      SELECT SINGLE *
        FROM zib_nfe_dist_ter
        INTO @DATA(wzib_nfe_dist_ter)
        WHERE chave_nfe = @wa_lotes_alv_u-chave_nfe.

*-CS2025000249-17.04.2025-#173311-JT-inicio
      LOOP AT it_carac_alv_u        INTO DATA(_carac_alv).
        MOVE-CORRESPONDING _carac_alv TO w_carac.
        APPEND w_carac                TO t_carac.
      ENDLOOP.
*-CS2025000249-17.04.2025-#173311-JT-fim

      CALL FUNCTION 'Z_MM_INDEA_LOTE'
        EXPORTING
          i_ebeln   = <fs_item>-ebeln
          i_ebelp   = <fs_item>-ebelp
          i_matnr   = <fs_item>-matnr
          i_line_id = line_id
          i_mblnr   = wzib_nfe_dist_ter-mblnr
          i_mjahr   = wzib_nfe_dist_ter-mjahr
          i_charg   = wa_lotes_alv_u-charg
          i_menge   = wa_lotes_alv_u-menge
          i_btn     = 'X'
          t_carac   = t_carac   "*-CS2025000249-17.04.2025-#173311-JT
          i_tcode   = sy-tcode.
    ENDIF.
    "
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver_1602 IMPLEMENTATION.
  METHOD handle_data_changed_1602.
    error_in_data = abap_false.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data EQ abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ELSE.
      PERFORM data_changed_1602 USING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
    ENDIF.
  ENDMETHOD.

  METHOD perform_semantic_checks.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good) WHERE fieldname EQ 'ATWRT'.

      IF ls_good-value IS NOT INITIAL.
        READ TABLE it_carac_alv_u INTO DATA(wa_carac_alv_u) INDEX ls_good-row_id.
        TRY.
            zcl_charg=>valida_valor_caracteristica( i_class     = zib_nfe_dist_lot-class
                                                    i_classtype = zib_nfe_dist_lot-klart
                                                    i_atinn     = wa_carac_alv_u-atinn
                                                    i_atwrt     = CONV #( ls_good-value ) ).
          CATCH zcx_charg_exception INTO ex_charg.
            error_in_data = abap_true.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = ex_charg->if_t100_message~t100key-msgid
                i_msgno     = ex_charg->if_t100_message~t100key-msgno
                i_msgty     = ex_charg->msgty
                i_msgv1     = ex_charg->msgv1
                i_msgv2     = ex_charg->msgv2
                i_msgv3     = ex_charg->msgv3
                i_msgv4     = ex_charg->msgv4
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
        ENDTRY.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

*-CS2025000249-08.04.2025-#173180-JT-inicio
  METHOD on_f4_1602.

    TYPES: BEGIN OF ty_f4_marca,
             nome_marca TYPE zmmt0195-nome_marca,
             user_reg   TYPE zmmt0195-user_reg,
             data_reg   TYPE zmmt0195-data_reg,
             hora_reg   TYPE zmmt0195-hora_reg.
    TYPES: END OF ty_f4_marca.

    TYPES: BEGIN OF ty_f4_cat,
             id_cat_sementes TYPE zsdt0199-id_cat_sementes,
             nome            TYPE zsdt0199-nome,
             usname          TYPE zsdt0199-usname,
             data_atual      TYPE zsdt0199-data_atual,
             hora_atual      TYPE zsdt0199-hora_atual.
    TYPES: END OF ty_f4_cat.

    DATA: it_ret      TYPE STANDARD TABLE OF ddshretval,
          it_f4_marca TYPE STANDARD TABLE OF ty_f4_marca,
          it_f4_cat   TYPE STANDARD TABLE OF ty_f4_cat,
          wa_ret      TYPE ddshretval,
          ls_modi     TYPE lvc_s_modi.

    FIELD-SYMBOLS: <lt_f4> TYPE lvc_t_modi.

    READ TABLE it_fieldcat_1602 INTO DATA(w_fieldcat_1602) WITH KEY fieldname = 'ATWRT'.
    READ TABLE it_carac_alv_u INTO DATA(w_carac_alv_u) INDEX es_row_no-row_id.

    CHECK e_fieldname = 'ATWRT'.

    CASE w_carac_alv_u-atnam .
      WHEN 'SEMENTE_MARCAS'.
        SELECT *
          FROM zmmt0195
          INTO CORRESPONDING FIELDS OF TABLE it_f4_marca.

        SORT it_f4_marca BY nome_marca.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'NOME_MARCA'
            window_title    = 'Marcas Sementes'
            value_org       = 'S'
          TABLES
            value_tab       = it_f4_marca
            return_tab      = it_ret
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        READ TABLE it_ret INTO wa_ret INDEX 1.
        IF sy-subrc = 0 AND wa_ret-fieldval IS NOT INITIAL AND w_fieldcat_1602-edit = abap_true.
          er_event_data->m_event_handled = abap_true.
          ASSIGN er_event_data->m_data->* TO <lt_f4>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'ATWRT'.
          ls_modi-value     =  wa_ret-fieldval.
          APPEND ls_modi   TO <lt_f4>.
        ENDIF.

      WHEN 'SEMENTE_CATEGORIA'.
        SELECT *
          FROM zsdt0199
          INTO CORRESPONDING FIELDS OF TABLE it_f4_cat.

        SORT it_f4_cat BY id_cat_sementes.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ID_CAT_SEMENTES'
            window_title    = 'Categorias Sementes'
            value_org       = 'S'
          TABLES
            value_tab       = it_f4_cat
            return_tab      = it_ret
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        READ TABLE it_ret INTO wa_ret INDEX 1.
        IF sy-subrc = 0 AND wa_ret-fieldval IS NOT INITIAL AND w_fieldcat_1602-edit = abap_true.
          er_event_data->m_event_handled = abap_true.
          ASSIGN er_event_data->m_data->* TO <lt_f4>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'ATWRT'.
          ls_modi-value     =  wa_ret-fieldval.
          APPEND ls_modi   TO <lt_f4>.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
*-CS2025000249-08.04.2025-#173180-JT-fim

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_variaveis .
  CLEAR: obj_departamento,
         ck_alterou_departamento,
         ck_alterou_transportador,
         ck_alterou_grupo,
         ck_alterou_class,
         ck_alterou_tipo,
         ck_alterou_tipo_movimento,
         ck_alterou_armazem,
         ck_alterou_lote,
         ck_alterou_lote_info,
         ck_alterou_nr_fase,
         ck_alterou_dt_vencimento,
         ck_alterou_zbvtyp,
         ck_alterou_tipo_documento,
         ck_alterou_form_bloqueio,
         ck_alterou_zlspr,
         ck_alterou_pymt_meth,
         ck_alterou_housebankid,
         ck_informado_grupo,
         ck_informado_tipo,
         ck_informado_armazem,
         ck_informado_cfop,
         cl_informado_new_item,
         lt_texto_bloqueio_pagmento,
         lt_texto_forma_de_pagmento,
         lc_info_forne,
         it_zmmt0073_sel[],
         it_zmmt0075_sel[],
         it_zmmt0076_sel[],
         it_zmmt0077_sel[],
         it_zmmt0119_sel[],
         it_zmmt0073_sel,
         it_zmmt0075_sel,
         it_zmmt0076_sel,
         it_zmmt0077_sel,
         it_zmmt0119_sel,
         it_zmmt0073_alv[],
         it_zmmt0075_alv[],
         it_zmmt0076_alv[],
         it_zmmt0077_alv[],
         it_zmmt0119_alv[],
         it_zmmt0073_alv,
         it_zmmt0075_alv,
         it_zmmt0076_alv,
         it_zmmt0077_alv,
         it_zmmt0119_alv,
         it_zmmt0073[],
         it_zmmt0075[],
         it_zmmt0076[],
         it_zmmt0077[],
         it_zmmt0119[],
         it_zmmt0073,
         it_zmmt0075,
         it_zmmt0076,
         it_zmmt0077,
         it_zmmt0119,
         wa_zmmt0072,
         zmmt0072,
         zde_zmmt0073_alv,
         zde_zmmt0075_alv,
         zde_zmmt0076_alv,
         zde_zmmt0077_alv,
         zde_zmmt0119_alv,
         zib_nfe_dist_ter,
         zib_nfe_dist_lot,
         zde_nfe_inbound_alv,
         zde_nfe_dist_alv,
         zde_nfe_dist_itm_alv,
         it_itens[],
         it_itens,
         it_itens_alv[],
         it_itens_alv_sel,
         it_itens_alv_sel[],
         it_itens_alv,
         it_lotes_alv_t,
         it_lotes_alv_u,
         it_carac_alv,
         it_carac_alv_u,
         it_lotes_alv_t[],
         it_lotes_alv_u[],
         it_carac_alv[],
         it_carac_alv_u[],
         wa_nfe_inbound,
         wa_itens_sel_lote,
         lc_fornecedor_regio,
         lc_destinatario_regio ,
         lc_transportador_regio,
         lc_armazem_regio.

  CLEAR: event_receiver_1502.
  CLEAR: event_receiver_1601.
  CLEAR: event_receiver_1602.
  CLEAR: event_receiver_1603.

  IF cl_grid_1503 IS NOT INITIAL.
    cl_grid_1503->free( ).
  ENDIF.
  CLEAR: cl_grid_1503.
  CLEAR: toolbarmanager_1603, toolbarmanager_1503, obg_toolbar_1603, obg_toolbar_1503.

  IF cl_grid_1602 IS NOT INITIAL.
    cl_grid_1602->free( ).
  ENDIF.
  CLEAR: cl_grid_1602.

  IF cl_grid_1603 IS NOT INITIAL.
    cl_grid_1603->free( ).
  ENDIF.
  CLEAR: cl_grid_1603.

  IF cl_grid_1601 IS NOT INITIAL.
    cl_grid_1601->free( ).
  ENDIF.
  CLEAR: cl_grid_1601.

  IF container_1503 IS NOT INITIAL.
    container_1503->free( ).
  ENDIF.
  CLEAR: container_1503.

  IF container_1603 IS NOT INITIAL.
    container_1603->free( ).
  ENDIF.
  CLEAR: container_1603.

  IF container_1602 IS NOT INITIAL.
    container_1602->free( ).
  ENDIF.
  CLEAR: container_1602.

  IF container_1601 IS NOT INITIAL.
    container_1601->free( ).
  ENDIF.
  CLEAR: container_1601.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_tela .

  obj_nfe_inbound->get_pedido_massa( ).      "*-CS2025000249-08.04.2025-#173180-JT

  wa_nfe_inbound = obj_nfe_inbound->get_info_nota( ).
  MOVE-CORRESPONDING wa_nfe_inbound-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING wa_nfe_inbound-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING wa_nfe_inbound-nfe_alv  TO zde_nfe_dist_alv.
  MOVE wa_nfe_inbound-nfe_base-itens   TO it_itens.
  MOVE wa_nfe_inbound-nfe_base-lotes   TO it_lotes.
  MOVE wa_nfe_inbound-nfe_base-lotes_c TO it_lotes_c.


  IF it_itens IS NOT INITIAL.
    READ TABLE it_itens ASSIGNING FIELD-SYMBOL(<fs_itens>) INDEX 1.
    SELECT SINGLE ebeln,inco1
      FROM ekko
      INTO @DATA(ls_ekko)
      WHERE ebeln = @<fs_itens>-ebeln.
    IF sy-subrc IS INITIAL.
      IF ls_ekko-inco1 EQ 'FOB' AND zib_nfe_dist_ter-ebeln IS NOT INITIAL.
        ck_alterou_ck_possui_frete = abap_true.
        zde_nfe_dist_alv-ck_possui_frete = abap_true.

        SELECT *
          FROM zib_cte_dist_n55
          INTO @DATA(ls_cte_dist_n55)
          UP TO 1 ROWS
          WHERE n55_chave_acesso = @zde_nfe_dist_alv-chave_nfe.
        ENDSELECT.
        IF sy-subrc IS INITIAL.
          IF ls_cte_dist_n55-cd_chave_cte IS NOT INITIAL.
            SELECT SINGLE *
              FROM zib_cte_dist_ter
              INTO @DATA(ls_cte_dist_ter)
              WHERE cd_chave_cte = @ls_cte_dist_n55-cd_chave_cte.
            IF sy-subrc IS INITIAL.

              IF ls_cte_dist_ter IS NOT INITIAL.
                zde_nfe_dist_alv-f_transporte = ls_cte_dist_ter-p_emissor.
                ck_alterou_transportador = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        zde_nfe_dist_alv-ck_possui_frete = abap_false.
      ENDIF.

    ENDIF.

  ENDIF.

  FREE it_itens_alv.

  LOOP AT it_itens INTO DATA(wa_itens).
    MOVE-CORRESPONDING wa_itens TO it_itens_alv.
    it_itens_alv-precos = icon_price.
    APPEND it_itens_alv.
  ENDLOOP.

  LOOP AT it_lotes INTO DATA(wa_lotes).
    MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
    APPEND it_lotes_alv_t.
  ENDLOOP.

  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
    APPEND it_carac_alv.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bdcdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_bdcdata  USING p_prog
                      p_scr
                      p_new
                      p_field
                      p_val.

  APPEND INITIAL LINE TO t_bdcdata ASSIGNING FIELD-SYMBOL(<fs_bdcdata>).

  <fs_bdcdata>-program  = p_prog.
  <fs_bdcdata>-dynpro   = p_scr.
  <fs_bdcdata>-dynbegin = p_new.
  <fs_bdcdata>-fnam     = p_field.
  <fs_bdcdata>-fval     = p_val.

ENDFORM.

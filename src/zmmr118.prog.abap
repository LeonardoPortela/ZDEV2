*&---------------------------------------------------------------------*
*& Report  ZMMR118
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr118 MESSAGE-ID znfe_distri.

TABLES: zib_nfe_dist_ter,
        zib_cte_dist_ter, "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
        zib_nfe_forn,
        zde_nfe_dist_alv,
        zde_nfe_dist_itm_alv,
        zde_nfe_inbound_alv,
        zib_nfe_dist_lot,
        zib_nfe_dist_ped.

*DATA: wa_cte_inbound_alv  TYPE zde_cte_inbound_alv, "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
*      wa_cte_dist_ter_alv TYPE zde_cte_dist_ter_alv. "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA

DATA: lc_chave_nfe_texto TYPE c LENGTH 100,
      lc_chave_cte_texto TYPE c LENGTH 100.

DATA: BEGIN OF wa_log_alv,
        ic_message TYPE zde_icon_st,
        ic_texto   TYPE zde_icon_estrategia.
        INCLUDE STRUCTURE zib_nfe_dist_log.
DATA: END OF wa_log_alv.

DATA: it_log_alv     LIKE TABLE OF wa_log_alv WITH KEY chave_nfe dt_atualizacao hr_atualizacao nr_sequencia.

*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_log DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_event_handler_log DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.

CLASS c_service DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS  get_pic_tab IMPORTING mime_url TYPE csequence EXPORTING pic_tab  TYPE STANDARD TABLE.
ENDCLASS.                    "c_service DEFINITION

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click  FOR EVENT node_double_click  OF cl_gui_column_tree IMPORTING node_key,
      handle_expand_no_children FOR EVENT expand_no_children OF cl_gui_column_tree IMPORTING node_key,
      handle_item_double_click  FOR EVENT item_double_click  OF cl_gui_column_tree IMPORTING node_key item_name,
      handle_button_click       FOR EVENT button_click       OF cl_gui_column_tree IMPORTING node_key item_name,
      handle_link_click         FOR EVENT link_click         OF cl_gui_column_tree IMPORTING node_key item_name,
      handle_checkbox_change    FOR EVENT checkbox_change    OF cl_gui_column_tree IMPORTING node_key item_name checked.
ENDCLASS.                    "LCL_APPLICATION DEFINITION

CLASS lcl_alv_toolbar_0201 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_0204 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_event_receiver_0201 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click_0201 FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_button_click_0201 FOR EVENT button_click OF cl_gui_alv_grid IMPORTING es_col_id es_row_no.
    METHODS handle_ondrop_0201 FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no e_dragdropobj.
    METHODS handle_ondrag_0201 FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no e_dragdropobj.
    METHODS handle_data_changed_0201 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS handle_f4_0201 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data et_bad_cells.
ENDCLASS.

CLASS lcl_event_receiver_itens_pedi DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_button_click FOR EVENT button_click OF cl_gui_alv_grid IMPORTING es_col_id es_row_no.
    METHODS handle_ondrop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no e_dragdropobj.
    METHODS handle_ondrag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no e_dragdropobj.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS handle_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data et_bad_cells.
ENDCLASS.

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_REC_DRAGDROP                              *
*----------------------------------------------------------------------*
CLASS cl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT node_double_click OF cl_gui_column_tree IMPORTING node_key.
    METHODS handle_on_drag FOR EVENT on_drag OF cl_gui_column_tree IMPORTING node_key item_name drag_drop_object.
    METHODS handle_button_click FOR EVENT button_click OF cl_gui_column_tree IMPORTING node_key item_name.
    METHODS handle_link_click FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dragdrop_obj_tree DEFINITION.
  PUBLIC SECTION.
    DATA: node  TYPE tv_nodekey.
ENDCLASS.                    "lcl_dragdrop_obj_d0100 DEFINITION

CLASS lcl_event_receiver_9002_b DEFINITION.
  PUBLIC SECTION.
    DATA: error_in_data TYPE c.
    METHODS handle_data_changed_9002_b FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
  PRIVATE SECTION.
    METHODS: perform_semantic_checks IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.

*****************************************************
*              CLASS cl_myevent_handler             *
*****************************************************
CLASS cl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer IMPORTING action frame getdata postdata query_table.

ENDCLASS.
*CLASS LCL_ALV_TOOLBAR_9002_A DEFINITION.
*  PUBLIC SECTION.
**Constructor
*    METHODS:
*      CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
*      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
*ENDCLASS.                    "lcl_alv_toolbar DEFINITION

TYPES: BEGIN OF ty_itens_alv.
         INCLUDE STRUCTURE zde_nfe_dist_itm_alv.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         ico_func_lote TYPE char04,
       END OF ty_itens_alv.

TYPES: BEGIN OF ty_dados_cte_alv.
         INCLUDE STRUCTURE zde_cte_dist_t_alv.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         ico_func_lote TYPE char04,
       END OF ty_dados_cte_alv.


TYPES: BEGIN OF ty_pedi_itens_alv.
         INCLUDE STRUCTURE zde_nfe_dist_ped_alv.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         ico_func_lote TYPE char04,
       END OF ty_pedi_itens_alv.

TYPES: BEGIN OF ty_pedido_validado.
TYPES:   ebeln  TYPE ebeln.
TYPES:   ebelp  TYPE ebelp.
TYPES:   motivo TYPE string.
TYPES:   node_key	TYPE tv_nodekey.
TYPES: END OF ty_pedido_validado.

TYPES: BEGIN OF ty_itens_lotes_alv.
         INCLUDE STRUCTURE zib_nfe_dist_lot.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
       END OF ty_itens_lotes_alv.

TYPES: BEGIN OF ty_itens_carac_alv.
         INCLUDE STRUCTURE zib_nfe_dist_lca.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
       END OF ty_itens_carac_alv.

TYPES: BEGIN OF ty_info_forne.
TYPES: texto TYPE char50,   "Fornecedor
       bankl TYPE char03, "Banco
       banka TYPE banka,  "Nome do Banco
       bankn TYPE bankn,  "Conta Corrente
       agenc TYPE char15. "Agencia
TYPES: END OF ty_info_forne.

DATA: sb_tela_0200       TYPE sy-dynnr,
      sb_tela_0201       TYPE sy-dynnr,
      sb_tela_0203       TYPE sy-dynnr,
      sb_tela_0300       TYPE sy-dynnr,
      it_ucomm           TYPE TABLE OF sy-ucomm,
      ok_code            TYPE sy-ucomm,
      obj_nfe            TYPE REF TO zcl_nfe_inbound,
      "obj_cte            TYPE REF TO zcl_cte_inbound,
      obj_nfeclass       TYPE REF TO zcl_repository_classes,
      ex_nfe_inbound     TYPE REF TO zcx_nfe_inbound_exception,
      ex_webservice      TYPE REF TO zcx_webservice,
      ex_cadastro        TYPE REF TO zcx_cadastro,
      ex_pedido_compra   TYPE REF TO zcx_pedido_compra_exception,
      ex_charg           TYPE REF TO zcx_charg_exception,
      ex_miro            TYPE REF TO zcx_miro_exception,
      ex_erro            TYPE REF TO zcx_error,
      it_itens_alv       TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      it_dados_cte_alv   TYPE TABLE OF ty_dados_cte_alv WITH HEADER LINE, "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
      it_pedi_itens_alv  TYPE TABLE OF ty_pedi_itens_alv WITH HEADER LINE,
      it_itens_ped       TYPE TABLE OF zde_nfe_dist_ped_alv,
      wa_ekkn            TYPE ekkn,
      it_itens_alv_sel   TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      it_pedi_itens_sel  TYPE TABLE OF ty_pedi_itens_alv WITH HEADER LINE,
      wa_pedi_itens_sel  TYPE ty_pedi_itens_alv,
      it_lotes_alv_t     TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_lotes_alv_u     TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_lotes_alv_sel   TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_carac_alv       TYPE TABLE OF ty_itens_carac_alv WITH HEADER LINE,
      it_carac_alv_u     TYPE TABLE OF ty_itens_carac_alv WITH HEADER LINE,
      e_ekpo_t           TYPE zde_ekpo_help_saldo_t,
      evt_receiver       TYPE REF TO cl_myevent_handler,
      url_revisao        TYPE string,
      html_revisao       TYPE string,
      ck_informado       TYPE char01,
      ck_informa_lote    TYPE char01,
      ck_log_ativo       TYPE c LENGTH 1,
      ctl_cccontainer2   TYPE REF TO cl_gui_container,
      obg_toolbar_log    TYPE REF TO lcl_alv_toolbar_log,
      toolbarmanager_log TYPE REF TO cl_alv_grid_toolbar_manager,
      event_handler_log  TYPE REF TO lcl_event_handler_log.

DATA: qt_linha_pedido TYPE p,
      qt_linha_nfe    TYPE p,
      ws_ekko         TYPE ekko,
      it_lfa1         TYPE TABLE OF lfa1,
      it_item_nfe     TYPE TABLE OF zib_nfe_dist_itm,
      tg_tvarvc       TYPE TABLE OF tvarvc,
      wg_tvarvc       TYPE tvarvc,
      rg_regio        TYPE RANGE OF regio,
      _param          TYPE ustyp_t_parameters. "US #165366 - MMSILVA - 13.02.2025

CONSTANTS: c_regio TYPE rvari_vnam VALUE 'Z_MAGGI_ZMM0116_REG'.


CONSTANTS: tl_0201 TYPE sy-dynnr VALUE '0201',
           tl_0202 TYPE sy-dynnr VALUE '0202',
           tl_0203 TYPE sy-dynnr VALUE '0203',
           tl_0204 TYPE sy-dynnr VALUE '0204',
           tl_0205 TYPE sy-dynnr VALUE '0205',
           tl_0207 TYPE sy-dynnr VALUE '0207',
           tl_0301 TYPE sy-dynnr VALUE '0301',

           BEGIN OF c_tree,
             column1 TYPE tv_itmname VALUE 'Pedido',
             column2 TYPE tv_itmname VALUE 'Material',
             column3 TYPE tv_itmname VALUE 'Saldo',
             column4 TYPE tv_itmname VALUE 'Preco',
             column5 TYPE tv_itmname VALUE 'Usuario',
           END OF c_tree.

CONSTANTS: cs_line_color_selecionada TYPE c LENGTH 4 VALUE 'C710'.

****************************************************
*    cl_myevent_handler implementation             *
****************************************************
CLASS cl_myevent_handler IMPLEMENTATION.

  METHOD on_sapevent.
    SUBMIT zmmr118 WITH chaven EQ action AND RETURN.
    PERFORM limpar_tela_0001.
    LEAVE TO SCREEN 0001.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler_log IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_log USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


CLASS lcl_alv_toolbar_log IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_log
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_close.
    ty_toolbar-function  = 'OCULTAR'.
    ty_toolbar-quickinfo = TEXT-013.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = abap_false.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_log->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'OCULTAR'.
        PERFORM ocultar_log .
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION


INCLUDE zmmr118_0300.

DATA: ctl_cccontainer_html1   TYPE REF TO cl_gui_container,
      ctl_cccontainer_revisao TYPE REF TO cl_gui_container,
      html_control            TYPE REF TO cl_gui_html_viewer,
      myevent                 TYPE cntl_simple_event,
      myevent_tab             TYPE cntl_simple_events,
      splitter                TYPE REF TO cl_gui_splitter_container,
      html_control_html1      TYPE REF TO cl_gui_html_viewer,
      ck_entrada_por_pedido   TYPE char01,
      ck_entrada_por_cte      TYPE char01.

DATA: docking             TYPE REF TO cl_gui_docking_container,
      tree                TYPE REF TO cl_gui_column_tree,
      g_application       TYPE REF TO lcl_application,
      tree_event_receiver TYPE REF TO cl_tree_event_receiver,
      events              TYPE cntl_simple_events,
      g_dropeffect        TYPE i,
      dragdrop_tree       TYPE REF TO cl_dragdrop,
      g_handle_tree       TYPE i,
      g_handle_alv        TYPE i,
      dragdrop_alv        TYPE REF TO cl_dragdrop.

DATA: lc_txt_butxt                TYPE t001-butxt,
      ck_alterou_ck_possui_frete  TYPE c LENGTH 1,
      ck_alterou_transportador    TYPE c LENGTH 1,
      ck_alterou_armazem          TYPE c LENGTH 1,
      ck_alterou_nr_fase          TYPE c LENGTH 1,
      ck_alterou_dt_vencimento    TYPE c LENGTH 1,
      ck_alterou_departamento     TYPE c LENGTH 1,
      cl_informado_new_item       TYPE c LENGTH 1,
      ck_alterou_quantidade_split TYPE c LENGTH 1,
      ck_alterou_lote_info        TYPE c LENGTH 1,
      ck_alterou_lote             TYPE c LENGTH 1,
      ck_alterou_zbvtyp           TYPE c LENGTH 1,
      ck_alterou_ck_fpol          TYPE c LENGTH 1, "*-CS2024000243-05.06.2024-#136397-JT
      ck_alterou_zlspr            TYPE c LENGTH 1,
      ck_alterou_pymt_meth        TYPE c LENGTH 1,
      ck_alterou_boleto           TYPE c LENGTH 1,
      ck_alterou_housebankid      TYPE c LENGTH 1,
      ck_alterou_vlr_desconto     TYPE c LENGTH 1,
      ck_alterou_obs_financeira   TYPE c LENGTH 1,
      i_campo	                    TYPE name_feld,
      lc_transportador_cnpj       TYPE zde_arma_cnpj,
      lc_transportador_ie         TYPE zde_arma_insc_es,
      lc_transportador_razao      TYPE zde_arma_razao,
      lc_transportador_regio      TYPE regio,
      lc_fornecedor_regio         TYPE regio,
      lc_destinatario_regio       TYPE regio,
      lc_armazem_regio            TYPE regio,
      it_itens                    TYPE zib_nfe_dist_itm_t,
      it_pedidos                  TYPE zde_nfe_dist_ped_t,
      it_lotes                    TYPE zib_nfe_dist_lot_t,
      it_lotes_c                  TYPE zib_nfe_dist_lca_t,
      cl_grid_0201                TYPE REF TO cl_gui_alv_grid,
      container_0201              TYPE REF TO cl_gui_custom_container,
      ctl_alv_nfe_hist            TYPE REF TO cl_gui_alv_grid,
      obg_conteiner_log           TYPE REF TO cl_gui_custom_container,
      it_function_0201            TYPE ui_functions,
      it_fieldcatalog2            TYPE lvc_t_fcat,
      wa_fieldcatalog2            TYPE lvc_s_fcat,
      wa_layout_0201              TYPE lvc_s_layo,
      it_fieldcat_0201            TYPE lvc_t_fcat,
      it_sort_0201                TYPE lvc_t_sort,
      gs_variant2                 TYPE disvariant,
      gs_layout2                  TYPE lvc_s_layo,
      wa_stable                   TYPE lvc_s_stbl,
      wa_variant_0201             TYPE disvariant,
      wa_stable_0201              TYPE lvc_s_stbl,
      it_selected_0201            TYPE lvc_t_row,
      wa_selected_0201            TYPE lvc_s_row,
      it_f4_0201                  TYPE lvc_t_f4,
      wa_f4_0201                  TYPE lvc_s_f4,
      obg_toolbar_0201            TYPE REF TO lcl_alv_toolbar_0201,
      toolbarmanager_0201         TYPE REF TO cl_alv_grid_toolbar_manager,
      event_receiver_0201         TYPE REF TO lcl_event_receiver_0201,
      event_receiver_itens_pedi   TYPE REF TO lcl_event_receiver_itens_pedi,
      node_table                  TYPE treev_ntab,
      item_table                  TYPE STANDARD TABLE OF mtreeitm,
      gs_scroll_col               TYPE lvc_s_col,
      gs_scroll_row               TYPE lvc_s_roid,
      it_selected_rows            TYPE lvc_t_row,
      wa_selected_rows            TYPE lvc_s_row,
      "WA_ITENS_SEL_LOTE           TYPE TY_ITENS_ALV,
      lc_info_forne               TYPE ty_info_forne,
      lt_texto_bloqueio_pagmento  TYPE textl_008,
      lt_texto_forma_de_pagmento  TYPE text1_042z,
      it_pedido_validado          TYPE TABLE OF ty_pedido_validado.

DATA: cl_grid_9002_a        TYPE REF TO cl_gui_alv_grid,
      container_9002_a      TYPE REF TO cl_gui_custom_container,
      it_function_9002_a    TYPE ui_functions,
      wa_layout_9002_a      TYPE lvc_s_layo,
      it_fieldcat_9002_a    TYPE lvc_t_fcat,
      it_sort_9002_a        TYPE lvc_t_sort,
      wa_variant_9002_a     TYPE disvariant,
      wa_stable_9002_a      TYPE lvc_s_stbl,
      it_f4_9002_a          TYPE lvc_t_f4,
      wa_f4_9002_a          TYPE lvc_s_f4,
*      OBG_TOOLBAR_9002_A    TYPE REF TO LCL_ALV_TOOLBAR_9002_A,
      toolbarmanager_9002_a TYPE REF TO cl_alv_grid_toolbar_manager,
      it_selected_9002_a    TYPE lvc_t_row,
      wa_selected_9002_a    TYPE lvc_s_row,
      okcode                TYPE sy-ucomm.

DATA: t_texto_motivo TYPE catsxt_longtext_itab,
      w_texto_motivo TYPE txline,
      l_conf_retorno TYPE c.

DATA: cl_grid_9002_b        TYPE REF TO cl_gui_alv_grid,
      container_9002_b      TYPE REF TO cl_gui_custom_container,
      it_function_9002_b    TYPE ui_functions,
      wa_layout_9002_b      TYPE lvc_s_layo,
      it_fieldcat_9002_b    TYPE lvc_t_fcat,
      it_sort_9002_b        TYPE lvc_t_sort,
      wa_variant_9002_b     TYPE disvariant,
      wa_stable_9002_b      TYPE lvc_s_stbl,
      it_f4_9002_b          TYPE lvc_t_f4,
      wa_f4_9002_b          TYPE lvc_s_f4,
      event_receiver_9002_b TYPE REF TO lcl_event_receiver_9002_b.

CONTROLS: tab_itens TYPE TABSTRIP,
          tb001     TYPE TABLEVIEW USING SCREEN 0203,
          tb002     TYPE TABLEVIEW USING SCREEN 0203,
          tb003     TYPE TABLEVIEW USING SCREEN 0203.

DATA: nfe_container_0204  TYPE REF TO cl_gui_custom_container,
      obg_toolbar_0204    TYPE REF TO lcl_alv_toolbar_0204,
      toolbarmanager_0204 TYPE REF TO cl_alv_grid_toolbar_manager,
      nfe_container_0205  TYPE REF TO cl_gui_custom_container,
      nfe_container_0207  TYPE REF TO cl_gui_custom_container.

*CLASS DEMO DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS MAIN.
*  PRIVATE SECTION.
*    METHODS HANDLE_SAPEVENT FOR EVENT SAPEVENT OF CL_ABAP_BROWSER.
*ENDCLASS.
*
*CLASS DEMO IMPLEMENTATION.
*
*  METHOD MAIN.
*
*    SET HANDLER handle_sapevent.
*
*  ENDMETHOD.
*
*  METHOD HANDLE_SAPEVENT.
*    BREAK-POINT .
*
*  ENDMETHOD.
*
*ENDCLASS.


* PARAMETERS: NF-e Data
"Informações do Conhecimento de Transporte
PARAMETER: chaven TYPE zib_nfe_dist_ter-chave_nfe     NO-DISPLAY.
"PARAMETER: ch_cte TYPE zib_cte_dist_ter-cd_chave_cte  NO-DISPLAY. "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
PARAMETER: pvenct TYPE zib_nfe_dist_ter-dt_vencimento NO-DISPLAY.

START-OF-SELECTION.

  DATA: lc_texto TYPE c LENGTH 15, is_valid TYPE n VALUE 1.

  IF chaven IS INITIAL.

    CALL SCREEN 0001.

  ELSEIF NOT chaven IS INITIAL.

    PERFORM valida_bloqueio_excecao_user CHANGING is_valid.

    IF is_valid = 1.

      CREATE OBJECT obj_nfe.
      TRY.
          obj_nfe->zif_cadastro~set_registro( i_id_registro = chaven ).
          obj_nfe->set_info_sap( ).
          obj_nfe->free( ).
          obj_nfe->zif_cadastro~set_registro( i_id_registro = chaven ).
          obj_nfe->set_coleta_tudo( i_ck_coleta_tudo = abap_true ).
          PERFORM get_info_tela.
          PERFORM get_info_valores.
          PERFORM get_info_banco_parceiro.

          IF zib_nfe_dist_ter-p_emissor IS INITIAL.

            IF zib_nfe_dist_ter-forne_cnpj IS NOT INITIAL.
              lc_texto = zib_nfe_dist_ter-forne_cnpj.
            ELSE.
              lc_texto = zib_nfe_dist_ter-forne_cpf.
            ENDIF.

            MESSAGE i011 WITH lc_texto zib_nfe_dist_ter-forne_ie.
            LEAVE PROGRAM.
          ENDIF.

          IF zib_nfe_dist_ter-f_tomadora IS INITIAL.
            MESSAGE i009 WITH zib_nfe_dist_ter-destino_cnpj zib_nfe_dist_ter-destino_ie.
            LEAVE PROGRAM.
          ENDIF.

          ck_entrada_por_pedido = abap_true.
          "ZCL_NFE_INBOUND=>GET_CK_RECEBIMENTO_BURRO( I_FILIAL = ZIB_NFE_DIST_TER-F_TOMADORA ).
          CALL SCREEN 0200.

        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.
    ENDIF.

    CLEAR: zib_nfe_dist_ter.
    IF obj_nfe IS NOT INITIAL  .
      obj_nfe->free( ).
      CLEAR: obj_nfe.
    ENDIF.
    LEAVE PROGRAM.


*  ELSEIF NOT ch_cte IS INITIAL."DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
*
*    PERFORM valida_bloqueio_excecao_user CHANGING is_valid.
*
*    IF is_valid = 1.
*
*      CREATE OBJECT obj_cte.
*      TRY.
*          obj_cte->zif_cadastro~set_registro( i_id_registro = ch_cte ).
*          obj_cte->set_info_sap( ).
*          obj_cte->free( ).
*          obj_cte->zif_cadastro~set_registro( i_id_registro = ch_cte ).
*          obj_cte->set_coleta_tudo( i_ck_coleta_tudo = abap_true ).
*          PERFORM get_info_tela_cte.
*          "PERFORM get_info_valores_cte.
*          "PERFORM get_info_banco_parceiro_cte.
*
*          IF zib_cte_dist_ter-p_emissor IS INITIAL.
*
*            IF zib_cte_dist_ter-emit_cnpj IS NOT INITIAL.
*              lc_texto = zib_cte_dist_ter-emit_cnpj.
*            ELSE.
*              lc_texto = zib_cte_dist_ter-emit_cpf.
*            ENDIF.
*
*            MESSAGE i011 WITH lc_texto zib_cte_dist_ter-emit_ie.
*            LEAVE PROGRAM.
*          ENDIF.
*
**          IF zib_cte_dist_ter-f_tomadora IS INITIAL. "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
**            MESSAGE i009 WITH zib_cte_dist_ter-dest_cnpj zib_cte_dist_ter-dest_ie.
**            LEAVE PROGRAM.
**          ENDIF.
*
*          ck_entrada_por_cte = abap_true.
*          "ZCL_NFE_INBOUND=>GET_CK_RECEBIMENTO_BURRO( I_FILIAL = ZIB_NFE_DIST_TER-F_TOMADORA ).
*          CALL SCREEN 0200.
*
*        CATCH zcx_cadastro INTO ex_cadastro.
*          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*      ENDTRY.
*    ENDIF.
*
*    CLEAR: zib_cte_dist_ter.
*    IF obj_cte IS NOT INITIAL  .
*      obj_cte->free( ).
*      CLEAR: obj_cte.
*    ENDIF.
*    LEAVE PROGRAM.

  ENDIF.

INITIALIZATION.

*---------------------------------------------------------------------*
*       CLASS CL_TREE_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS cl_tree_event_receiver IMPLEMENTATION.
* handle double_click
  METHOD handle_double_click.
    CHECK NOT node_key IS INITIAL.

    READ TABLE item_table WITH KEY node_key = node_key item_name = c_tree-column1 INTO DATA(wa_item_table).

    CHECK sy-subrc IS INITIAL.

    READ TABLE e_ekpo_t WITH KEY ebeln = wa_item_table-text(10) ebelp = wa_item_table-text+11(5) INTO DATA(wa_ekpo_t).

    CHECK sy-subrc IS INITIAL.

    zcl_pedido_compra=>show_pedido( i_ebeln = wa_ekpo_t-ebeln ).

  ENDMETHOD.

* Drag & Drop
  METHOD handle_on_drag.
    CHECK NOT node_key IS INITIAL.

    DATA: l_obj     TYPE REF TO lcl_dragdrop_obj_tree.

    CREATE OBJECT l_obj.
    l_obj->node = node_key.
    drag_drop_object->object = l_obj.
  ENDMETHOD.

  METHOD handle_button_click.
    PERFORM add_pedido_manual.
  ENDMETHOD.

  METHOD handle_link_click.
    PERFORM chama_link_click USING node_key	item_name.
  ENDMETHOD.

ENDCLASS.

*CLASS LCL_EVENT_RECEIVER_9002_A IMPLEMENTATION.
*  METHOD HANDLE_DOUBLE_CLICK_9002_A.
*    PERFORM DOUBLE_CLICK_9002_A USING E_ROW E_COLUMN ES_ROW_NO.
*  ENDMETHOD.
*ENDCLASS.

CLASS lcl_alv_toolbar_0201 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_0201
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    DATA(ck_permissao) = abap_false.

    IF zib_nfe_dist_ter-ck_fiscal EQ abap_true.
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

    CALL METHOD toolbarmanager_0201->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_itens_alv_sel, it_itens_alv_sel[].

    CALL METHOD cl_grid_0201->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_0201.

    LOOP AT it_selected_0201 INTO wa_selected_0201.
      READ TABLE it_itens_alv INTO DATA(lc_item_alv) INDEX wa_selected_0201-index.
      APPEND lc_item_alv TO it_itens_alv_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_item.
      WHEN 'DEL'.
        PERFORM deletar_item.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION*---------------------------------------------------------------------*


CLASS lcl_alv_toolbar_0204 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_0204
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    DATA(ck_permissao) = abap_false.

    IF zib_nfe_dist_ter-ck_fiscal EQ abap_true.
      ck_permissao = abap_true.
    ENDIF.

*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_inspection_lot.
    ty_toolbar-function  = 'LOTE'.
    ty_toolbar-quickinfo = TEXT-008.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    "Separador
    ty_toolbar-butn_type = 3.
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

    CALL METHOD toolbarmanager_0204->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_pedi_itens_sel, it_pedi_itens_sel[].

    CALL METHOD ct_alv_itens_pedi->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_0201.

    LOOP AT it_selected_0201 INTO wa_selected_0201.
      READ TABLE it_pedi_itens_alv INTO DATA(lc_item_alv) INDEX wa_selected_0201-index.
      APPEND lc_item_alv TO it_pedi_itens_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'DEL'.
        PERFORM deletar_item_pedi.
      WHEN 'LOTE'.
        PERFORM lote_item_pedido.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS. "LCL_ALV_TOOLBAR_0204*


*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_0201 IMPLEMENTATION.

  METHOD handle_hotspot_click_0201.
*    PERFORM HANDLE_HOTSPOT_CLICK_0201 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_button_click_0201.
*    PERFORM HANDLE_HOTSPOT_CLICK_0201 USING ES_ROW_NO-ROW_ID ES_COL_ID-FIELDNAME.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_ondrop_0201.

    CHECK zib_nfe_dist_ter-ck_fiscal EQ abap_false.

    READ TABLE item_table WITH KEY node_key = CAST lcl_dragdrop_obj_tree( e_dragdropobj->object )->node item_name = c_tree-column1
          INTO DATA(wa_item_table).

    CHECK sy-subrc IS INITIAL.

    READ TABLE e_ekpo_t WITH KEY ebeln = wa_item_table-text(10) ebelp = wa_item_table-text+11(5) INTO DATA(wa_ekpo_t).

    CHECK sy-subrc IS INITIAL.

    READ TABLE it_pedido_validado INTO DATA(wa_erro_item_ped) WITH KEY ebeln = wa_ekpo_t-ebeln ebelp = wa_ekpo_t-ebelp.
    IF sy-subrc IS INITIAL.
      PERFORM mostra_motivo USING wa_erro_item_ped.
      EXIT.
    ENDIF.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_linha>) INDEX e_row-index.

    CHECK sy-subrc IS INITIAL.

    <fs_linha>-ebeln = wa_ekpo_t-ebeln.
    <fs_linha>-ebelp = wa_ekpo_t-ebelp.
    <fs_linha>-matnr = wa_ekpo_t-matnr.

    obj_nfe->get_material(
      EXPORTING
        i_emissor          = zib_nfe_dist_ter-p_emissor
        i_prod_codigo      = <fs_linha>-prod_codigo
        i_unidade_item     = <fs_linha>-prod_und_comerci
      RECEIVING
        r_001              = DATA(r_001)
      EXCEPTIONS
        nao_achou_deparada = 1
        OTHERS             = 2 ).

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE meins INTO <fs_linha>-meins FROM mara WHERE matnr EQ wa_ekpo_t-matnr.
    ELSE.
      <fs_linha>-meins = r_001-meins.
      <fs_linha>-menge = <fs_linha>-prod_qtd_comerci * r_001-fator.
    ENDIF.

    DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

    CHECK cl_grid_0201 IS NOT INITIAL.

    gs_alv_refres_cond-row = abap_true.
    gs_alv_refres_cond-col = abap_true.

    CALL METHOD obj_nfe->set_item_material
      EXPORTING
        i_prod_item = <fs_linha>-prod_item
        i_matnr     = <fs_linha>-matnr
        i_ebeln     = <fs_linha>-ebeln
        i_ebelp     = <fs_linha>-ebelp
        i_menge     = <fs_linha>-menge
        i_meins     = <fs_linha>-meins
        i_netpr     = <fs_linha>-netpr
        i_netwr     = <fs_linha>-netwr.

    IF obj_nfe->get_alterou_pedido_compra( ) EQ abap_true.
      PERFORM get_info_tela.
      LEAVE TO SCREEN 0200.
    ENDIF.

    CALL METHOD cl_grid_0201->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.

    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_ondrag_0201.

  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_data_changed_0201.
    PERFORM data_changed_0201 USING er_data_changed.
  ENDMETHOD.

  METHOD handle_f4_0201.
    PERFORM f4_0201 USING e_fieldname es_row_no er_event_data et_bad_cells.
  ENDMETHOD.

ENDCLASS.



*       CLASS LCL_EVENT_RECEIVER_ITENS_PEDI IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_itens_pedi IMPLEMENTATION.

  METHOD handle_hotspot_click.
*    PERFORM HANDLE_HOTSPOT_CLICK_0201 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_button_click.
*    PERFORM HANDLE_HOTSPOT_CLICK_0201 USING ES_ROW_NO-ROW_ID ES_COL_ID-FIELDNAME.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_ondrop.

    DATA: is_stable  	   TYPE lvc_s_stbl,
          i_soft_refresh TYPE char01.

    DATA: lc_ebeln TYPE ebeln,
          lc_ebelp TYPE ebelp,
          i_pedido TYPE zib_nfe_dist_ped.

    CHECK zib_nfe_dist_ter-ck_fiscal EQ abap_false.

    READ TABLE item_table WITH KEY node_key = CAST lcl_dragdrop_obj_tree( e_dragdropobj->object )->node item_name = c_tree-column1
          INTO DATA(wa_item_table).

    CHECK sy-subrc IS INITIAL.

    lc_ebeln = wa_item_table-text(10).
    lc_ebelp = wa_item_table-text+11(5).

    IF lc_ebelp IS NOT INITIAL.
      READ TABLE e_ekpo_t WITH KEY ebeln = lc_ebeln ebelp = lc_ebelp INTO DATA(wa_ekpo_t).
      CHECK sy-subrc IS INITIAL.

      READ TABLE it_pedi_itens_alv INTO DATA(wa_pedi_itens_alv)
      WITH KEY ebeln = wa_ekpo_t-ebeln
               ebelp = wa_ekpo_t-ebelp.

      CHECK sy-subrc IS NOT INITIAL.

      READ TABLE it_pedido_validado INTO DATA(wa_erro_item_ped) WITH KEY ebeln = wa_ekpo_t-ebeln ebelp = wa_ekpo_t-ebelp.
      IF sy-subrc IS INITIAL.
        PERFORM mostra_motivo USING wa_erro_item_ped.
        EXIT.
      ENDIF.

      CLEAR: i_pedido.
      i_pedido-chave_nfe     = zib_nfe_dist_ter-chave_nfe.
      i_pedido-ebeln         = wa_ekpo_t-ebeln.
      i_pedido-ebelp         = wa_ekpo_t-ebelp.
      i_pedido-matnr         = wa_ekpo_t-matnr.
      i_pedido-menge         = wa_ekpo_t-menge_saldo.
      i_pedido-meins         = wa_ekpo_t-meins.
      i_pedido-netpr         = wa_ekpo_t-netpr.

      SELECT SINGLE * INTO @DATA(wa_eket)
        FROM eket
       WHERE ebeln EQ	@wa_ekpo_t-ebeln
         AND ebelp EQ @wa_ekpo_t-ebelp
         AND charg NE @space.

      IF sy-subrc IS INITIAL.
        i_pedido-charg = wa_eket-charg.
      ENDIF.

      TRY .
          DATA(alv_pedido) = obj_nfe->add_pedido_nota( i_pedido = i_pedido ).
          MOVE-CORRESPONDING alv_pedido TO wa_pedi_itens_alv.
          wa_pedi_itens_alv-ico_func_lote = icon_batch.
          APPEND wa_pedi_itens_alv TO it_pedi_itens_alv.
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ENDTRY.
    ELSE.
      LOOP AT e_ekpo_t INTO wa_ekpo_t WHERE ebeln EQ lc_ebeln .

        READ TABLE it_pedido_validado INTO wa_erro_item_ped WITH KEY ebeln = wa_ekpo_t-ebeln ebelp = wa_ekpo_t-ebelp.
        IF sy-subrc IS INITIAL.
          PERFORM mostra_motivo USING wa_erro_item_ped.
          CONTINUE.
        ENDIF.

        CLEAR: i_pedido.
        i_pedido-chave_nfe     = zib_nfe_dist_ter-chave_nfe.
        i_pedido-ebeln         = wa_ekpo_t-ebeln.
        i_pedido-ebelp         = wa_ekpo_t-ebelp.
        i_pedido-matnr         = wa_ekpo_t-matnr.
        i_pedido-menge         = wa_ekpo_t-menge_saldo.
        i_pedido-meins         = wa_ekpo_t-meins.
        i_pedido-netpr         = wa_ekpo_t-netpr.

        SELECT SINGLE * INTO @wa_eket
          FROM eket
         WHERE ebeln EQ	@wa_ekpo_t-ebeln
           AND ebelp EQ @wa_ekpo_t-ebelp
           AND charg NE @space.

        IF sy-subrc IS INITIAL.
          i_pedido-charg = wa_eket-charg.
        ENDIF.

        TRY .
            alv_pedido = obj_nfe->add_pedido_nota( i_pedido = i_pedido ).
            MOVE-CORRESPONDING alv_pedido TO wa_pedi_itens_alv.
            wa_pedi_itens_alv-ico_func_lote = icon_batch.

            SELECT * FROM ekkn
              INTO wa_ekkn
              WHERE ebeln = wa_pedi_itens_alv-ebeln
               AND ebelp = wa_pedi_itens_alv-ebelp.
            ENDSELECT.

            IF sy-subrc = 0.
              wa_pedi_itens_alv-unload_pt    = wa_ekkn-wempf.
              wa_pedi_itens_alv-gr_rcpt    = wa_ekkn-ablad.
            ENDIF.

            APPEND wa_pedi_itens_alv TO it_pedi_itens_alv.
          CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
        ENDTRY.

      ENDLOOP.

    ENDIF.
    SORT it_pedi_itens_alv ASCENDING BY ebeln ebelp.

    IF obj_nfe->ck_alterou_iva EQ abap_true OR obj_nfe->ck_alterou_bloqueio_paga EQ abap_true.
      PERFORM get_info_tela.
      obj_nfe->ck_alterou_iva = abap_false.
      obj_nfe->ck_alterou_bloqueio_paga = abap_false.
      LEAVE TO SCREEN 0200.
    ENDIF.

    is_stable-col = abap_true.
    is_stable-row = abap_true.
    i_soft_refresh = abap_true.

    ct_alv_itens_pedi->refresh_table_display(
      EXPORTING
        is_stable      = is_stable
        i_soft_refresh = i_soft_refresh
    ).

    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_ondrag.

  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_data_changed.

    DATA: is_stable  	   TYPE lvc_s_stbl,
          i_soft_refresh TYPE char01.

    PERFORM data_changed_pedi USING er_data_changed.

    IF obj_nfe->get_ck_alterou_iva( ) EQ abap_true.
      PERFORM get_info_tela.
      LEAVE TO SCREEN 0200.
    ENDIF.

    SORT it_pedi_itens_alv ASCENDING BY ebeln ebelp.

    is_stable-col = abap_true.
    is_stable-row = abap_true.
    i_soft_refresh = abap_true.

    ct_alv_itens_pedi->refresh_table_display(
      EXPORTING
        is_stable      = is_stable
        i_soft_refresh = i_soft_refresh
    ).

    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.

  METHOD handle_f4.
    PERFORM f4_0201 USING e_fieldname es_row_no er_event_data et_bad_cells.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD  handle_node_double_click.

  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD  handle_item_double_click.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD  handle_link_click.

    PERFORM mostrar_node USING node_key item_name.

  ENDMETHOD.                    "HANDLE_LINK_CLICK

  METHOD  handle_button_click.

  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD  handle_checkbox_change.
  ENDMETHOD.                    "HANDLE_CHECKBOX_CHANGE

  METHOD handle_expand_no_children.
  ENDMETHOD.                    "HANDLE_EXPAND_NO_CHILDREN

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION


*CLASS LCL_ALV_TOOLBAR_9002_A IMPLEMENTATION.
*
*  METHOD CONSTRUCTOR.
**   Create ALV toolbar manager instance
*    CREATE OBJECT TOOLBARMANAGER_9002_A
*      EXPORTING
*        IO_ALV_GRID = IO_ALV_GRID.
*
*  ENDMETHOD.                    "constructor
*
*  METHOD ON_TOOLBAR.
*
*    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*
*    DATA(CK_PERMISSAO) = ABAP_FALSE.
*
*    IF ZIB_NFE_DIST_TER-ST_FISICO NE ZCL_NFE_INBOUND=>ST_FISICO_00.
*      CK_PERMISSAO = ABAP_TRUE.
*    ENDIF.
*
**    "Separador
*    TY_TOOLBAR-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
*
*    "Marcar Todos os Documentos
*    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
*    TY_TOOLBAR-FUNCTION  = 'ADD'.
*    TY_TOOLBAR-QUICKINFO = TEXT-005.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    TY_TOOLBAR-DISABLED  = CK_PERMISSAO.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
*
*    "Marcar Todos os Documentos
*    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
*    TY_TOOLBAR-FUNCTION  = 'DEL'.
*    TY_TOOLBAR-QUICKINFO = TEXT-006.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    TY_TOOLBAR-DISABLED  = CK_PERMISSAO.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
*
*    CALL METHOD TOOLBARMANAGER_9002_A->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.
*
*  ENDMETHOD.                    "on_toolbar
*
*  METHOD HANDLE_USER_COMMAND.
*
*    CLEAR: IT_LOTES_ALV_SEL, IT_LOTES_ALV_SEL[].
*
*    CALL METHOD CL_GRID_9002_A->GET_SELECTED_ROWS
*      IMPORTING
*        ET_INDEX_ROWS = IT_SELECTED_9002_A.
*
*    LOOP AT IT_SELECTED_9002_A INTO WA_SELECTED_9002_A.
*      READ TABLE IT_LOTES_ALV_U INTO DATA(LC_LOTE_ALV) INDEX WA_SELECTED_9002_A-INDEX.
*      APPEND LC_LOTE_ALV TO IT_LOTES_ALV_SEL.
*    ENDLOOP.
*
*    CASE E_UCOMM.
*      WHEN 'ADD'.
*        PERFORM INCLUIR_LOTE.
*        PERFORM ATUALIZA_TELA_9002_B.
*      WHEN 'DEL'.
*        PERFORM DELETAR_LOTE.
*        PERFORM ATUALIZA_TELA_9002_B.
*    ENDCASE.
*
*  ENDMETHOD. "zm_handle_user_command
*
*ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lc_chave_nfe_texto WITH ''. "US #172277 - MMSILVA - 28.03.2025
  DATA(qt_a) = strlen( lc_chave_nfe_texto ).
  CONDENSE lc_chave_nfe_texto NO-GAPS.
  DATA(qt_b) = strlen( lc_chave_nfe_texto ).



  CHECK qt_b EQ qt_a.

  zib_nfe_dist_ter-chave_nfe = lc_chave_nfe_texto.

  "Verificar se Nfe é de entrega futura.
  SELECT SINGLE * INTO @DATA(wa_nota) FROM zib_nfe_dist_ter WHERE chave_nfe EQ @zib_nfe_dist_ter-chave_nfe.
  IF wa_nota IS INITIAL.
    MESSAGE s103 WITH zib_nfe_dist_ter-chave_nfe DISPLAY LIKE 'E'.
    CLEAR: wa_nota.
    EXIT.
  ENDIF.

  CLEAR: wa_nota.
  SELECT SINGLE * INTO @DATA(wa_nota_itm) FROM zib_nfe_dist_itm WHERE chave_nfe EQ @zib_nfe_dist_ter-chave_nfe.
  IF wa_nota_itm IS NOT INITIAL.

    SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_ZMM0116_CFOP'
        AND valfrom EQ @wa_nota_itm-prod_cfop.

    IF i_data IS NOT INITIAL.
      MESSAGE s161 WITH zib_nfe_dist_ter-chave_nfe DISPLAY LIKE 'E'.
      CLEAR: wa_nota_itm, i_data.
      EXIT.
    ENDIF.

  ENDIF.
  CLEAR: wa_nota_itm, i_data.

  SUBMIT zmmr118 WITH chaven EQ zib_nfe_dist_ter-chave_nfe AND RETURN.
  CLEAR: zib_nfe_dist_ter-chave_nfe.

*  CREATE OBJECT OBJ_NFE.
*  OBJ_NFE->NFE_INBOUND( ).
*
*  TRY.
*      OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = ZIB_NFE_DIST_TER-CHAVE_NFE ).
*      OBJ_NFE->AT_NFE_INBOUND->SET_INFO_SAP( ).
*      OBJ_NFE->AT_NFE_INBOUND->FREE( ).
*      OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = ZIB_NFE_DIST_TER-CHAVE_NFE ).
*      OBJ_NFE->AT_NFE_INBOUND->SET_COLETA_TUDO( I_CK_COLETA_TUDO = ABAP_TRUE ).
*      PERFORM GET_INFO_TELA.
*      CALL SCREEN 0200.
*    CATCH ZCX_CADASTRO INTO EX_CADASTRO.
*      EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*  CLEAR: ZIB_NFE_DIST_TER.
*  IF OBJ_NFE IS NOT INITIAL.
*    OBJ_NFE->FREE( ).
*    CLEAR: OBJ_NFE.
*  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
*atualiza tabela interna.

*data: lo_grid TYPE REF TO cl_gui_alv_grid.
*
* CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*  IMPORTING
*    E_GRID   = lo_grid.
*
*
*if not lo_grid is INITIAL.
*
*  call METHOD lo_grid->check_changed_data.
*endif.




  CASE ok_code.
    WHEN 'LOG'.
      CALL SCREEN 9004  STARTING  AT 050 3
                         ENDING   AT 210 21.
    WHEN 'DANFE'.
      TRY.
          zcl_nfe_inbound=>danfe( i_chave_nfe = obj_nfe->get_cabecalho_nota( )-chave_nfe ).
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN 'DOCMATERIA'.
      DATA(lc_cabecalho) = obj_nfe->get_cabecalho_nota( ).
      PERFORM mostrar_doc_material USING lc_cabecalho-mblnr lc_cabecalho-mjahr.
    WHEN 'BUSCAR_PED'.
      CLEAR: ok_code.
      CLEAR e_ekpo_t.
      PERFORM pesquisa_pedidos USING abap_false e_ekpo_t.
      CLEAR: ok_code.

*-CS2021000662 - 19.11.2021 - JT - inicio
    WHEN 'RETORNAR'.
      PERFORM f_envia_retorno.
*-CS2021000662 - 19.11.2021 - JT - fim

    WHEN 'EXECUTAR'.
      CLEAR: ok_code.
***///[CS2021000555] Validação preço líquido/pedido ZMM0116 - Set/2021 - Inicio
      TRY.

          DATA: it_vlr TYPE RANGE OF j_1bnflin-netpr,
                wa_vlr LIKE LINE OF it_vlr,
                it_qtd TYPE RANGE OF j_1bnflin-menge,
                wa_qtd LIKE LINE OF it_qtd.

          DATA: it_set_qtd TYPE TABLE OF rgsb4,
                it_set_vlr TYPE TABLE OF rgsb4,
                wa_set     TYPE rgsb4.

          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              client        = sy-mandt
              setnr         = 'ZMMR118_TOLERANCIA_VLR'
              class         = '0000'
            TABLES
              set_values    = it_set_vlr
            EXCEPTIONS
              set_not_found = 1
              OTHERS        = 2.

          LOOP AT it_set_vlr INTO wa_set.
            wa_vlr-sign    = 'I'.
            wa_vlr-option  = 'GT'.

            CALL FUNCTION 'MOVE_CHAR_TO_NUM'
              EXPORTING
                chr             = wa_set-from
              IMPORTING
                num             = wa_vlr-low
              EXCEPTIONS
                convt_no_number = 1
                convt_overflow  = 2
                OTHERS          = 3.


*            wa_vlr-low     = wa_set-from.
            APPEND wa_vlr TO it_vlr.
            CLEAR: wa_vlr, wa_set.
          ENDLOOP.

          "itens pedido -> IT_PEDI_ITENS_ALV
          DATA: vl_total TYPE ty_pedi_itens_alv-total,
                vl_menge TYPE ty_pedi_itens_alv-menge.

          "itens Nf -> IT_ITENS_ALV
          DATA: vl_prod_vlr_total_b TYPE zde_nfe_dist_itm_alv-prod_vlr_total_b,
                vl_prod_qtd_comerci TYPE zde_nfe_dist_itm_alv-prod_qtd_comerci.


          CLEAR: vl_total,
                 vl_menge,
                 vl_prod_vlr_total_b,
                 vl_prod_qtd_comerci.


          LOOP AT it_pedi_itens_alv ASSIGNING FIELD-SYMBOL(<fs_itpd>).
            vl_total = vl_total + <fs_itpd>-total + <fs_itpd>-navnw.
            vl_menge = vl_menge + <fs_itpd>-menge.
          ENDLOOP.

          LOOP AT it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_itnf>).
            vl_prod_vlr_total_b = vl_prod_vlr_total_b + <fs_itnf>-prod_vlr_total_b.
            vl_prod_qtd_comerci = vl_prod_qtd_comerci + <fs_itnf>-prod_qtd_comerci.
          ENDLOOP.

          IF abs( vl_total - vl_prod_vlr_total_b ) IN it_vlr.
*        Preço líquido do pedido esta divergente do Itens Nota Fiscal
            RAISE EXCEPTION TYPE zcx_cadastro
              EXPORTING
                textid = VALUE #( msgid = 'ZNFE_DISTRI'
                                  msgno = '000'
                                  attr1 = 'Preço líquido do pedido'
                                  attr2 = 'esta divergente do'
                                  attr3 = 'Itens Nota Fiscal'
                                  attr4 = '' )
                msgid  = 'ZNFE_DISTRI'
                msgno  = '000'
                msgv1  = 'Preço líquido do pedido'
                msgv2  = 'esta divergente do'
                msgv3  = 'Itens Nota Fiscal'
                msgv4  = ''.
          ENDIF.

*          IF abs( vl_menge - vl_prod_qtd_comerci ) IN it_qtd.
**        Quantidade líquido do pedido esta divergente do Itens Nota Fiscal
*            RAISE EXCEPTION TYPE zcx_cadastro
*              EXPORTING
*                textid = VALUE #( msgid = 'ZNFE_DISTRI'
*                                  msgno = '000'
*                                  attr1 = 'Quantidade líquido do pedido'
*                                  attr2 = 'esta divergente do'
*                                  attr3 = 'Itens Nota Fiscal'
*                                  attr4 = '' )
*                msgid  = 'ZNFE_DISTRI'
*                msgno  = '000'
*                msgv1  = 'Quantidade líquido do pedido'
*                msgv2  = 'esta divergente do'
*                msgv3  = 'Itens Nota Fiscal'
*                msgv4  = ''.
*          ENDIF.
*        CATCH zcx_cadastro INTO ex_cadastro.
*          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*          RETURN.
      ENDTRY.

      "Check item da nota x quantidade linhas pedido.
**=======================================Inicio USER STORY 83807    / Anderson Oenning.

*atualiza tabela interna.


      CALL METHOD ct_alv_itens_pedi->check_changed_data.



      CLEAR: it_lfa1, ws_ekko, qt_linha_nfe, qt_linha_pedido.
      FREE: it_item_nfe.

      READ TABLE it_pedi_itens_alv INTO DATA(ws_pedido) INDEX 1.
      IF sy-subrc EQ 0.

        SELECT *
        FROM zmmt0161
        INTO TABLE @DATA(it_zmmt0161).
        IF sy-subrc EQ 0.
          CLEAR: ws_ekko.
          SELECT SINGLE * FROM ekko INTO ws_ekko WHERE ebeln EQ ws_pedido-ebeln.
          "Check região fornecedor.
          SELECT * FROM lfa1 AS a
          INTO TABLE it_lfa1
          FOR ALL ENTRIES IN it_zmmt0161
          WHERE regio EQ it_zmmt0161-regio
           AND  lifnr EQ ws_ekko-lifnr
           AND EXISTS ( SELECT * FROM lfb1 AS b WHERE b~lifnr EQ b~lifnr AND b~bukrs EQ it_zmmt0161-bukrs ).
          IF sy-subrc EQ 0.
            "Se for estado de Amazonas devera fazer a verificação quantidade de linhas pedido x NFe.
            SELECT * FROM zib_nfe_dist_itm INTO TABLE it_item_nfe WHERE chave_nfe EQ zib_nfe_dist_ter-chave_nfe.
            IF sy-subrc EQ 0.
              DESCRIBE TABLE it_item_nfe LINES qt_linha_nfe.
              DESCRIBE TABLE it_pedi_itens_alv LINES qt_linha_pedido.
              IF qt_linha_nfe NE qt_linha_pedido.
                MESSAGE s020(zmmped) DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
**=======================================Fim USER STORY 83807    / Anderson Oenning.

***///[CS2021000555] Validação preço líquido/pedido ZMM0116 - Set/2021 - Fim

      TRY.
*&--------------------------------------------------------Modifica dados do pedido/aoenning. *comentado 29092023
*          IF it_pedi_itens_alv IS NOT INITIAL.
*            MOVE-CORRESPONDING it_pedi_itens_alv[] TO it_itens_ped[].
*            obj_nfe->modify_dados_item_pedido( EXPORTING item_pedido = it_itens_ped[] ).
*          ENDIF.

*&--------------------------------------------------------Modifica dados do pedido.
*          OBJ_NFE->set_ck_politica( i_ck_pol = zde_nfe_dist_alv-ck_fpol ).
          IF zde_nfe_dist_alv-ck_fpol EQ abap_true.
            obj_nfe->set_fora_politica( i_ck_fpol = zde_nfe_dist_alv-ck_fpol ).
          ENDIF.

          obj_nfe->set_aceitar_documento( ).
          obj_nfe->set_aceitar_fisico( ).
          obj_nfe->set_aceitar_faturar( i_ck_somente_validar = abap_true ).
          DATA(i_gravou) = obj_nfe->zif_cadastro~gravar_registro( ).
          CHECK i_gravou EQ abap_true.
          lc_cabecalho = obj_nfe->get_cabecalho_nota( ).
          PERFORM limpar_tela.
          IF lc_cabecalho-mblnr IS NOT INITIAL.
            MESSAGE s001(zmigo) WITH lc_cabecalho-mblnr lc_cabecalho-mjahr.
          ENDIF.
          LEAVE TO SCREEN 0.
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
          ex_pedido_compra->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN 'ESTORNAR'.
      CLEAR: ok_code.
      TRY.
          lc_cabecalho = obj_nfe->get_cabecalho_nota( ).
          obj_nfe->nfe_inbound_cancela_fisico(
            IMPORTING
              e_mblnr = DATA(e_mblnr)
              e_mjahr = DATA(e_mjahr) ).

          obj_nfe->nfe_inbound_cancela_aceite( ).
          PERFORM limpar_tela.
          IF e_mblnr IS NOT INITIAL.
            MESSAGE s002(zmigo) WITH lc_cabecalho-mblnr lc_cabecalho-mjahr e_mblnr e_mjahr.
          ENDIF.
          LEAVE TO SCREEN 0.
        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
          ex_pedido_compra->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

*   US #165366 - MMSILVA - 13.02.2025 - Inicio
    WHEN 'DESV_SM'.
      CLEAR: ok_code.
      FREE: _param.

      CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
        EXPORTING
          user_name           = sy-uname
        TABLES
          user_parameters     = _param
        EXCEPTIONS
          user_name_not_exist = 1
          OTHERS              = 2.

      IF _param[] IS NOT INITIAL.

        READ TABLE _param INTO DATA(ws_param) WITH KEY parid = 'ZMM0116_DESV_SM'.

        IF sy-subrc EQ 0.

          DATA: resposta TYPE c.

          CLEAR resposta.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TITLEBAR              = 'Confirmar'
              TEXT_QUESTION         = 'Deseja confirmar a alteração?'
              TEXT_BUTTON_1         = 'Sim'
              TEXT_BUTTON_2         = 'Não'
              DEFAULT_BUTTON        = '2'
              DISPLAY_CANCEL_BUTTON = ''
            IMPORTING
              ANSWER                = RESPOSTA
            EXCEPTIONS
              TEXT_NOT_FOUND        = 1
              OTHERS                = 2.

          IF RESPOSTA = '1'.

            SELECT SINGLE se_recordid FROM zib_nfe_dist_ter WHERE chave_nfe = @zib_nfe_dist_ter-chave_nfe INTO @DATA(lr_checksm).

            IF lr_checksm IS NOT INITIAL.
              UPDATE zib_nfe_dist_ter SET se_recordid = '' WHERE chave_nfe EQ zib_nfe_dist_ter-chave_nfe.
              MESSAGE 'Realizado a desvinculação.' TYPE 'S'.
            ELSE.
              MESSAGE 'Não há SM vinculada.' TYPE 'E'.
            ENDIF.

          ELSE.

            EXIT.

          ENDIF.

        ELSE.

          MESSAGE 'Não possui permissão.' TYPE 'E'.

        ENDIF.

      ENDIF.
*   US #165366 - MMSILVA - 13.02.2025 - Fim

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Retorno sem ajuste
*&---------------------------------------------------------------------*
FORM f_envia_retorno.

  DATA: l_message_v1 TYPE syst_msgv,
        l_message_v2 TYPE syst_msgv,
        l_message_v3 TYPE syst_msgv,
        l_message_v4 TYPE syst_msgv,
        l_texto      TYPE bapi_msg.

  FREE: t_texto_motivo,
        l_texto.

  DATA(lc_cabec) = obj_nfe->get_cabecalho_nota( ).

  SELECT *
    FROM zib_nfe_dist_ter
    INTO @DATA(w_dist_ter)
      UP TO 1 ROWS
   WHERE chave_nfe = @lc_cabec-chave_nfe.
  ENDSELECT.

  CHECK sy-subrc = 0.

  IF w_dist_ter-mblnr IS INITIAL.
    MESSAGE s024(sd) WITH 'Ação não permitida!'
                          'Não existe MIGO gerada!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_dist_ter-belnr IS NOT INITIAL.
    MESSAGE s024(sd) WITH 'Ação não permitida!'
                          'MIRO gerada!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_dist_ter-ck_revisao = abap_false AND w_dist_ter-se_status NE 'SUCCESS_CRIA'.
    MESSAGE s024(sd) WITH 'Ação não permitida!'
                          'SM não está em Revisão!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*----------------------
* continua com processo?
*----------------------
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Retorno sem Ajuste'
      text_question         = 'Deseja continuar com o Processo?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
      default_button        = '2'
      start_column          = 60
      start_row             = 8
    IMPORTING
      answer                = l_conf_retorno.

  CHECK l_conf_retorno = '1'.

*----------------------
* informar motivo
*----------------------
  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Informar o Motivo do Retorno'
      im_start_column = 60
      im_start_row    = 8
    CHANGING
      ch_text         = t_texto_motivo.

*----------------------
* montar texto
*----------------------
  LOOP AT t_texto_motivo INTO w_texto_motivo..
    IF sy-tabix = 1.
      l_texto = w_texto_motivo.
    ELSE.
      l_texto = l_texto && '|' && w_texto_motivo.
    ENDIF.
  ENDLOOP.

  l_message_v1 = l_texto(50).
  l_message_v2 = l_texto+50(50).
  l_message_v3 = l_texto+100(50).
  l_message_v4 = l_texto+150(50).

*----------------------
* enviar confirmacao
*----------------------
  TRY.
      obj_nfe->set_aceitar_faturar( i_ck_somente_validar    = abap_true
                                    i_ck_retorno_sem_ajuste = abap_true ).
      obj_nfe->set_ck_retorno_sem_ajuste( i_message_v1 = l_message_v1
                                          i_message_v2 = l_message_v2
                                          i_message_v3 = l_message_v3
                                          i_message_v4 = l_message_v4 ).
      obj_nfe->zif_cadastro~gravar_registro( ).
      lc_cabecalho = obj_nfe->get_cabecalho_nota( ).
      PERFORM limpar_tela.
      LEAVE TO SCREEN 0.
    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_pedido_compra_exception INTO ex_pedido_compra.
      ex_pedido_compra->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.
  PERFORM limpar_tela.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  DATA: hierarchy_header TYPE treev_hhdr.

  CLEAR: it_ucomm, it_ucomm[].

  CASE zib_nfe_dist_ter-ck_fiscal.
    WHEN abap_true.
      APPEND 'EXECUTAR'   TO it_ucomm.
      APPEND 'BUSCAR_PED' TO it_ucomm.
    WHEN abap_false.
      APPEND 'ESTORNAR' TO it_ucomm.
      APPEND 'DOCMATERIA' TO it_ucomm.
  ENDCASE.

  SET PF-STATUS 'PF0200' EXCLUDING it_ucomm.
  SET TITLEBAR 'TL0200' WITH zib_nfe_dist_ter-chave_nfe zib_nfe_dist_ter-f_tomadora.

  IF sb_tela_0200 IS INITIAL.
    sb_tela_0200 = tl_0201.
  ENDIF.

  IF docking IS INITIAL.

    CREATE OBJECT docking
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking->dock_at_left
        extension = 420.

    hierarchy_header-heading = TEXT-001.
    hierarchy_header-width   = 31.

    CREATE OBJECT tree
      EXPORTING
        parent                = docking
        node_selection_mode   = tree->node_sel_mode_single
        item_selection        = 'X'
        hierarchy_column_name = c_tree-column1
        hierarchy_header      = hierarchy_header.

* define the events which will be passed to the backend
    CREATE OBJECT g_application.
    SET HANDLER g_application->handle_node_double_click  FOR tree.
    SET HANDLER g_application->handle_item_double_click  FOR tree.
    SET HANDLER g_application->handle_expand_no_children FOR tree.
    SET HANDLER g_application->handle_link_click         FOR tree.
    SET HANDLER g_application->handle_button_click       FOR tree.
    SET HANDLER g_application->handle_checkbox_change    FOR tree.

* Column2
    CALL METHOD tree->add_column
      EXPORTING
        name        = c_tree-column2
        width       = 30
        header_text = TEXT-002.

    CALL METHOD tree->add_column
      EXPORTING
        name        = c_tree-column3
        width       = 18
        header_text = TEXT-003.

    CALL METHOD tree->add_column
      EXPORTING
        name        = c_tree-column4
        width       = 18
        header_text = TEXT-004.

    CALL METHOD tree->add_column
      EXPORTING
        name        = c_tree-column5
        width       = 20
        header_text = TEXT-007.

* register events
    PERFORM register_events.
* set handler for tree1
    CREATE OBJECT tree_event_receiver.
    SET HANDLER tree_event_receiver->handle_double_click FOR tree.
    SET HANDLER tree_event_receiver->handle_on_drag FOR tree.
    SET HANDLER tree_event_receiver->handle_button_click FOR tree.
    SET HANDLER tree_event_receiver->handle_link_click FOR tree.

    g_dropeffect = cl_dragdrop=>move.

    CREATE OBJECT dragdrop_tree.

    CALL METHOD dragdrop_tree->add
      EXPORTING
        flavor     = 'LINE'
        dragsrc    = 'X'
        droptarget = ''
        effect     = g_dropeffect.

    CALL METHOD dragdrop_tree->get_handle
      IMPORTING
        handle = g_handle_tree.

    CREATE OBJECT dragdrop_alv.

    CALL METHOD dragdrop_alv->add
      EXPORTING
        flavor     = 'LINE'
        dragsrc    = ''
        droptarget = 'X'
        effect     = g_dropeffect.

    CALL METHOD dragdrop_alv->get_handle
      IMPORTING
        handle = g_handle_alv.

    CLEAR: e_ekpo_t.
    IF sy-tcode = 'ZNFW0010' AND it_pedi_itens_alv[] IS INITIAL.
      PERFORM add_pedido_manual.
    ELSE.
      PERFORM pesquisa_pedidos USING abap_true e_ekpo_t.
    ENDIF.


  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA: i_qtd TYPE i.

  SET TITLEBAR 'TL0001'.

  IF splitter IS INITIAL.

    PERFORM busca_revisoes.

    IF url_revisao IS NOT INITIAL.
      i_qtd = 2.
    ELSE.
      i_qtd = 1.
    ENDIF.

    CREATE OBJECT splitter
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = i_qtd.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = ctl_cccontainer_html1.

    CREATE OBJECT html_control_html1
      EXPORTING
        parent = ctl_cccontainer_html1.

    DATA: data_table TYPE STANDARD TABLE OF text255,
          i_url      TYPE c LENGTH 200.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = zcl_util=>get_html_fundo( )
        i_tabline_length = 255
      TABLES
        et_table         = data_table.

    html_control_html1->load_data(
      IMPORTING
        assigned_url           = i_url
      CHANGING
        data_table             = data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).

    html_control_html1->show_url(
      EXPORTING
        url                    = i_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5
    ).

    IF i_qtd EQ 2.
      PERFORM mostrar_revisoes.
    ENDIF.

  ENDIF.

  IF url_revisao IS INITIAL.
    CALL SCREEN 0100 STARTING AT 25 06.
  ELSE.
    SET PF-STATUS 'PF0001'.
  ENDIF.

ENDMODULE.

##PERF_NO_TYPE
FORM load_pic_from_db  USING  gui_picture TYPE REF TO cl_gui_picture.

  DATA url(255).
  TYPES pic_line(1022) TYPE x.
  DATA  pic_tab TYPE TABLE OF pic_line.

  CLEAR url.
  url = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  c_service=>get_pic_tab(
    EXPORTING
      mime_url = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
    IMPORTING
      pic_tab  = pic_tab ).

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type    = 'image'
      subtype = 'GIF'
    TABLES
      data    = pic_tab
    CHANGING
      url     = url
    EXCEPTIONS
      OTHERS  = 1.

  CALL METHOD gui_picture->load_picture_from_url
    EXPORTING
      url = url.

ENDFORM.                               " LOAD_PIC_FROM_DB

CLASS c_service IMPLEMENTATION.
  METHOD get_pic_tab.
    DATA pic_wa TYPE xstring.
    DATA length TYPE i.
    DATA mime_api TYPE REF TO if_mr_api.
    mime_api = cl_mime_repository_api=>get_api( ).
    mime_api->get( EXPORTING  i_url     = mime_url
                   IMPORTING  e_content = pic_wa
                   EXCEPTIONS OTHERS    = 4 ).
    IF sy-subrc = 4.
      RETURN.
    ENDIF.
    CLEAR pic_tab.
    length = xstrlen( pic_wa ).
    WHILE length >= 1022.
      APPEND pic_wa(1022) TO pic_tab.
      SHIFT pic_wa BY 1022 PLACES LEFT IN BYTE MODE.
      length = xstrlen( pic_wa ).
    ENDWHILE.
    IF length > 0.
      APPEND pic_wa TO pic_tab.
    ENDIF.
  ENDMETHOD.                    "get_pic_tab

ENDCLASS.                    "c_service IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0201 OUTPUT.

  IF sb_tela_0201 IS INITIAL.
    CASE ck_entrada_por_pedido.
      WHEN abap_false.
        sb_tela_0201 = tl_0202.
      WHEN abap_true.
        sb_tela_0201 = tl_0203.
    ENDCASE.
*    IF ck_entrada_por_cte = abap_true. "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
*      sb_tela_0201 = tl_0203.
*    ENDIF.
  ENDIF.

  IF ck_entrada_por_cte EQ abap_false."DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA

    IF zde_nfe_dist_alv-e_tomadora IS NOT INITIAL.
      SELECT SINGLE butxt INTO lc_txt_butxt
        FROM t001
       WHERE bukrs EQ zde_nfe_dist_alv-e_tomadora.
    ENDIF.

    IF ck_alterou_ck_possui_frete EQ abap_true.
      ck_alterou_ck_possui_frete = abap_false.
    ENDIF.

    IF zde_nfe_dist_alv-cd_departamento IS NOT INITIAL AND ck_alterou_departamento EQ abap_true.
      zde_nfe_dist_alv-ds_departamento = obj_nfe->get_info_departamento( i_cd_departamento = zde_nfe_dist_alv-cd_departamento ).
      ck_alterou_departamento = abap_false.
    ELSEIF zde_nfe_dist_alv-cd_departamento IS INITIAL AND ck_alterou_departamento EQ abap_true.
      CLEAR: zde_nfe_dist_alv-ds_departamento.
      ck_alterou_departamento = abap_false.
    ENDIF.

    LOOP AT SCREEN.

      IF zib_nfe_dist_ter-ck_fiscal EQ abap_true.
        IF screen-name(16) EQ 'ZDE_NFE_DIST_ALV'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-name(16) EQ 'ZDE_NFE_DIST_ALV'.
          SPLIT screen-name AT '-' INTO DATA(str1_1502) DATA(str2_1502).
          i_campo = str2_1502.
          IF obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ) EQ abap_true.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF zde_nfe_dist_alv-ck_possui_frete IS INITIAL.
      CLEAR: zde_nfe_dist_alv-f_transporte.
    ENDIF.

    IF zde_nfe_dist_alv-f_transporte IS INITIAL.
      CLEAR:
      lc_transportador_cnpj,
      lc_transportador_ie,
      lc_transportador_razao,
      lc_transportador_regio.
    ENDIF.

    IF zde_nfe_dist_alv-f_transporte IS NOT INITIAL AND ck_alterou_transportador EQ abap_true.
      DATA(wa_trans) = obj_nfe->set_transportadora( i_f_transporte = zde_nfe_dist_alv-f_transporte ).
      lc_transportador_cnpj  = wa_trans-stcd1.
      lc_transportador_ie    = wa_trans-stcd3.
      lc_transportador_razao = wa_trans-name1.
      lc_transportador_regio = wa_trans-regio.
      ck_alterou_transportador = abap_false.
    ELSEIF zde_nfe_dist_alv-f_transporte IS INITIAL AND ck_alterou_transportador EQ abap_true.
      obj_nfe->set_transportadora( i_f_transporte = zde_nfe_dist_alv-f_transporte ).
      CLEAR:
      lc_transportador_cnpj,
      lc_transportador_ie,
      lc_transportador_razao,
      lc_transportador_regio.
      ck_alterou_transportador = abap_false.
    ELSEIF zde_nfe_dist_alv-f_transporte IS NOT INITIAL AND ck_alterou_transportador EQ abap_false.
      DATA(wa_lfa1) = obj_nfe->get_info_fornecedor( i_lifnr = zde_nfe_dist_alv-f_transporte ).
      lc_transportador_cnpj  = wa_lfa1-stcd1.
      lc_transportador_ie    = wa_lfa1-stcd3.
      lc_transportador_razao = wa_lfa1-name1.
      lc_transportador_regio = wa_lfa1-regio.
    ENDIF.

    IF zde_nfe_dist_alv-f_armazem IS NOT INITIAL AND ck_alterou_armazem EQ abap_true.
      DATA(armazem) = obj_nfe->set_armazem( i_f_armazem = zde_nfe_dist_alv-f_armazem ).
      zde_nfe_dist_alv-armazem_cnpj  = armazem-stcd1.
      zde_nfe_dist_alv-armazem_ie    = armazem-stcd3.
      zde_nfe_dist_alv-armazem_razao = armazem-name1.
      lc_armazem_regio               = armazem-regio.
      ck_alterou_armazem = abap_false.
    ELSEIF zde_nfe_dist_alv-f_armazem IS INITIAL AND ck_alterou_armazem EQ abap_true.
      CLEAR:
      zde_nfe_dist_alv-armazem_cnpj,
      zde_nfe_dist_alv-armazem_ie,
      zde_nfe_dist_alv-armazem_razao,
      lc_armazem_regio.
      ck_alterou_armazem = abap_false.
    ENDIF.

    IF ck_alterou_nr_fase EQ abap_true.
      obj_nfe->set_nr_fase( i_nr_fase = zde_nfe_dist_alv-nr_fase ).
      ck_alterou_nr_fase = abap_false.
    ENDIF.

    IF ck_alterou_boleto EQ abap_true.
      obj_nfe->set_boleto( i_boleto = zde_nfe_dist_alv-boleto ).
      ck_alterou_boleto = abap_false.
    ENDIF.

    IF ck_alterou_dt_vencimento EQ abap_true.
      obj_nfe->set_dt_vencimento( i_dt_vencimento = zde_nfe_dist_alv-dt_vencimento ).
      ck_alterou_dt_vencimento = abap_false.
    ENDIF.

    IF ck_alterou_vlr_desconto EQ abap_true.
      obj_nfe->set_vlr_desconto( EXPORTING i_vlr_desconto = zde_nfe_dist_alv-vlr_desconto ).
    ENDIF.

    IF ck_alterou_obs_financeira EQ abap_true.
      obj_nfe->set_obs_financeira( EXPORTING i_obs_financeira = zde_nfe_dist_alv-obs_financeira ).
    ENDIF.

*-CS2024000243-05.06.2024-#136397-JT-inicio
    IF ck_alterou_ck_fpol EQ abap_true.
      obj_nfe->set_fora_politica( EXPORTING i_ck_fpol = zde_nfe_dist_alv-ck_fpol ).
    ENDIF.
*-CS2024000243-05.06.2024-#136397-JT-fim

    IF lc_fornecedor_regio IS INITIAL.
      SELECT SINGLE regio INTO lc_fornecedor_regio
        FROM lfa1
       WHERE lifnr EQ zde_nfe_dist_alv-p_emissor.
    ENDIF.

    IF lc_destinatario_regio IS INITIAL.
      wa_lfa1-lifnr = zde_nfe_dist_alv-f_tomadora.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_lfa1-lifnr
        IMPORTING
          output = wa_lfa1-lifnr.

      SELECT SINGLE regio INTO lc_fornecedor_regio
        FROM kna1
       WHERE kunnr EQ wa_lfa1-lifnr.
    ENDIF.

    IF ck_alterou_zlspr EQ abap_true AND zde_nfe_dist_alv-zlspr IS NOT INITIAL.

      SELECT SINGLE textl INTO lt_texto_bloqueio_pagmento
        FROM t008t
       WHERE spras EQ sy-langu
         AND zahls EQ zde_nfe_dist_alv-zlspr.

    ELSEIF zde_nfe_dist_alv-zlspr IS INITIAL.

      CLEAR: lt_texto_bloqueio_pagmento.

    ELSEIF zde_nfe_dist_alv-zlspr IS NOT INITIAL AND lt_texto_bloqueio_pagmento IS INITIAL.

      SELECT SINGLE textl INTO lt_texto_bloqueio_pagmento
        FROM t008t
       WHERE spras EQ sy-langu
         AND zahls EQ zde_nfe_dist_alv-zlspr.

    ENDIF.

    IF ck_alterou_pymt_meth EQ abap_true AND zde_nfe_dist_alv-pymt_meth IS NOT INITIAL.

      PERFORM get_info_banco_parceiro.
      SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
        FROM t042z
       WHERE land1 EQ zde_nfe_dist_alv-land1
         AND zlsch EQ zde_nfe_dist_alv-pymt_meth.

    ELSEIF zde_nfe_dist_alv-pymt_meth IS INITIAL.

      CLEAR: lt_texto_forma_de_pagmento.

    ELSEIF zde_nfe_dist_alv-pymt_meth IS NOT INITIAL AND lt_texto_forma_de_pagmento IS INITIAL.

      SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
        FROM t042z
       WHERE land1 EQ zde_nfe_dist_alv-land1
         AND zlsch EQ zde_nfe_dist_alv-pymt_meth.

    ENDIF.

    IF pvenct IS NOT INITIAL AND zde_nfe_dist_alv-dt_vencimento IS INITIAL.
      zde_nfe_dist_alv-dt_vencimento = pvenct.
    ENDIF.

    CLEAR: ck_alterou_zbvtyp, ck_alterou_zlspr, ck_alterou_pymt_meth, ck_alterou_housebankid.

  ENDIF.

*  "136397 CS2024000243 Ajuste ZMM0116 (Fluxo de lançamento fora da politica) PSA
*  LOOP AT SCREEN.
*    IF screen-name EQ 'ZDE_NFE_DIST_ALV-CK_FPOL'.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ARMAZEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_armazem INPUT.
  ck_alterou_armazem = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TRANSPORTADOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_transportador INPUT.
  ck_alterou_transportador = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_CK_POSSUI_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_ck_possui_frete INPUT.
  ck_alterou_ck_possui_frete = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_NR_FASE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_nr_fase INPUT.
  ck_alterou_nr_fase = abap_true.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALTEROU_DT_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_dt_vencimento INPUT.
  ck_alterou_dt_vencimento = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  data_changed_0201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_0201 USING  rr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells TYPE lvc_s_modi.
  DATA: ls_cells     TYPE lvc_s_modi.
  DATA: l_matnr      TYPE matnr.
  DATA: l_ebeln	TYPE ebeln.
  DATA: l_ebelp	TYPE ebelp.
  DATA: l_menge	TYPE j_1bnetqty.
  DATA: l_meins	TYPE j_1bnetunt.
  DATA: l_netpr	TYPE j_1bnetpri.
  DATA: l_netwr	TYPE j_1bnetval.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX ls_mod_cells-row_id.

    CASE ls_mod_cells-fieldname.
      WHEN  'MATNR'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_matnr.

        IF l_matnr IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wa_mara) FROM mara WHERE matnr EQ @l_matnr.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE s014 WITH l_matnr.

            CALL METHOD rr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = sy-msgid
                i_msgno     = sy-msgno
                i_msgty     = 'E'
                i_msgv1     = sy-msgv1
                i_msgv2     = sy-msgv2
                i_msgv3     = sy-msgv3
                i_msgv4     = sy-msgv4
                i_fieldname = ls_mod_cells-fieldname
                i_row_id    = ls_mod_cells-row_id.
          ELSE.
            SELECT SINGLE * INTO @DATA(wa_makt)
              FROM makt
             WHERE spras EQ @sy-langu
               AND matnr EQ @l_matnr.

            MESSAGE s015 WITH wa_makt-maktg.

            <fs_item>-matnr = wa_makt-matnr.

            CALL METHOD obj_nfe->set_item_material
              EXPORTING
                i_prod_item = <fs_item>-prod_item
                i_matnr     = <fs_item>-matnr
                i_ebeln     = <fs_item>-ebeln
                i_ebelp     = <fs_item>-ebelp
                i_menge     = <fs_item>-menge
                i_meins     = <fs_item>-meins
                i_netpr     = <fs_item>-netpr
                i_netwr     = <fs_item>-netwr.
          ENDIF.
        ELSE.
          CLEAR: <fs_item>-matnr.

          CALL METHOD obj_nfe->set_item_material
            EXPORTING
              i_prod_item = <fs_item>-prod_item
              i_matnr     = <fs_item>-matnr
              i_ebeln     = <fs_item>-ebeln
              i_ebelp     = <fs_item>-ebelp
              i_menge     = <fs_item>-menge
              i_meins     = <fs_item>-meins
              i_netpr     = <fs_item>-netpr
              i_netwr     = <fs_item>-netwr.
        ENDIF.
      WHEN 'MENGE'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_menge.

        <fs_item>-menge = l_menge.

        CALL METHOD obj_nfe->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

      WHEN 'MEINS'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_meins.

        <fs_item>-meins = l_meins.

        CALL METHOD obj_nfe->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

      WHEN 'EBELN'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebeln.

        <fs_item>-ebeln = l_ebeln.

        CALL METHOD obj_nfe->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

        IF obj_nfe->get_alterou_pedido_compra( ) EQ abap_true.
          PERFORM get_info_tela.
        ENDIF.

      WHEN 'EBELP'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebelp.

        <fs_item>-ebelp = l_ebelp.

        CALL METHOD obj_nfe->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

        IF obj_nfe->get_alterou_pedido_compra( ) EQ abap_true.
          PERFORM get_info_tela.
        ENDIF.

      WHEN 'NETPR'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_netpr.

        <fs_item>-netpr = l_netpr.

        CALL METHOD obj_nfe->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.


      WHEN 'NETWR'.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_netwr.

        <fs_item>-netwr = l_netwr.

        CALL METHOD obj_nfe->set_item_material
          EXPORTING
            i_prod_item = <fs_item>-prod_item
            i_matnr     = <fs_item>-matnr
            i_ebeln     = <fs_item>-ebeln
            i_ebelp     = <fs_item>-ebelp
            i_menge     = <fs_item>-menge
            i_meins     = <fs_item>-meins
            i_netpr     = <fs_item>-netpr
            i_netwr     = <fs_item>-netwr.

    ENDCASE.
  ENDLOOP.
ENDFORM.                               " data_changed_0201

*&---------------------------------------------------------------------*
*&      Form  F4_1502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_0201 USING r_fieldname TYPE lvc_fname rs_row_no TYPE lvc_s_roid rr_event_data TYPE REF TO cl_alv_event_data rt_bad_cells TYPE lvc_t_modi.

  FIELD-SYMBOLS: <lt_f4> TYPE lvc_t_modi.
  DATA: ls_f4  TYPE lvc_s_modi,
        i_nota TYPE zib_nfe_dist_ter,
        i_item TYPE zib_nfe_dist_itm.

  CASE r_fieldname.
    WHEN 'MATNR'.
      rr_event_data->m_event_handled = 'X'.
      ASSIGN rr_event_data->m_data->* TO <lt_f4>.
      MESSAGE 'Fazer Pesquisa material' TYPE 'I'.
    WHEN 'EBELN' OR 'EBELP'.

      "ZDE_NFE_DIST_ALV
      MOVE-CORRESPONDING zde_nfe_dist_alv TO i_nota.
      READ TABLE it_itens_alv INDEX rs_row_no-row_id INTO DATA(wa_itens_alv).
      MOVE-CORRESPONDING wa_itens_alv TO i_item.

      rr_event_data->m_event_handled = 'X'.
      ASSIGN rr_event_data->m_data->* TO <lt_f4>.

      CALL METHOD obj_nfe->get_pedido_compra_chave
        EXPORTING
          i_nota = i_nota
          i_item = i_item
        RECEIVING
          r_ekpo = DATA(wa_expo)
        EXCEPTIONS
          erro   = 1
          OTHERS = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      ELSE.
        ls_f4-row_id    = rs_row_no-row_id.
        ls_f4-fieldname = 'EBELN'.
        ls_f4-value     = wa_expo-ebeln.
        APPEND ls_f4 TO <lt_f4>.
        ls_f4-row_id    = rs_row_no-row_id.
        ls_f4-fieldname = 'EBELP'.
        ls_f4-value     = wa_expo-ebelp.
        APPEND ls_f4 TO <lt_f4>.
      ENDIF.

    WHEN 'MEINS'.
  ENDCASE.

  "ls_f4-fieldname = r_fieldname.
  "ls_f4-row_id = rs_row_no-row_id.
  "ls_f4-value = 'BLUBBER'.
  "append ls_f4 to <lt_f4>.

ENDFORM.                                                    " F4

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_item .
*
*071  Deve ser Selecionado um Item "BASE"!
*072  Deve ser Selecionado um Item "FAKE"!

  IF it_itens_alv_sel[] IS INITIAL.
    MESSAGE s071.
    EXIT.
  ENDIF.

  READ TABLE it_itens_alv_sel INDEX 1.

  "Informar Quantidade do Novo Item
  cl_informado_new_item = abap_false.
  MOVE-CORRESPONDING it_itens_alv_sel TO zde_nfe_dist_itm_alv.
  CALL SCREEN 9001 STARTING AT 02 02.
  "Se informado Quantidade
  CHECK cl_informado_new_item EQ abap_true.

  TRY.
      obj_nfe->add_item( i_prod_item_base = it_itens_alv_sel-prod_item i_prod_qtd_comerci = zde_nfe_dist_itm_alv-prod_qtd_comerci ).

      PERFORM get_info_tela.

      LEAVE TO SCREEN 0200.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_item .

*071  Deve ser Selecionado um Item "BASE"!
*072  Deve ser Selecionado um Item "FAKE"!

  IF it_itens_alv_sel[] IS INITIAL.
    MESSAGE s072.
    EXIT.
  ENDIF.

  READ TABLE it_itens_alv_sel INDEX 1.

  TRY.
      obj_nfe->excluir_item( i_prod_item_base = it_itens_alv_sel-prod_item ).

      PERFORM get_info_tela.

      LEAVE TO SCREEN 0200.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_ITEM_PEDI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_item_pedi .

  DATA: i_pedido TYPE zib_nfe_dist_ped.

*071  Deve ser Selecionado um Item "BASE"!
*072  Deve ser Selecionado um Item "FAKE"!

  IF it_pedi_itens_sel[] IS INITIAL.
    MESSAGE s120.
    EXIT.
  ENDIF.

  TRY.

      LOOP AT it_pedi_itens_sel INTO DATA(wa_excluir_pedi).
        MOVE-CORRESPONDING wa_excluir_pedi TO i_pedido.
        obj_nfe->add_pedido_nota( i_pedido = i_pedido i_excluir = abap_true ).
      ENDLOOP.

      PERFORM get_info_tela.

      LEAVE TO SCREEN 0200.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0201 .

  DATA: i_contador_2 TYPE lvc_colpos,
        wa_coluna    TYPE lvc_s_fcat.

  CLEAR: it_fieldcat_0201[], it_fieldcat_0201.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_NFE_DIST_ITM_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcat_0201.

  i_contador_2 = 19.

  wa_coluna-fieldname = 'ICO_FUNC_LOTE'.
  APPEND wa_coluna TO it_fieldcat_0201.

  LOOP AT it_fieldcat_0201 ASSIGNING FIELD-SYMBOL(<fs_0201>).
    <fs_0201>-edit = abap_false.
    CASE <fs_0201>-fieldname.
      WHEN 'PROD_ITEM'.
        <fs_0201>-col_pos   = 01.
        <fs_0201>-outputlen = 03.
      WHEN 'PROD_CODIGO'.
        <fs_0201>-col_pos   = 02.
        <fs_0201>-outputlen = 08.
      WHEN 'PROD_DESCRICAO'.
        <fs_0201>-col_pos   = 03.
        <fs_0201>-outputlen = 18.
      WHEN 'PROD_NCM'.
        <fs_0201>-col_pos   = 04.
        <fs_0201>-outputlen = 10.
      WHEN 'PROD_UND_COMERCI'.
        <fs_0201>-col_pos   = 05.
        <fs_0201>-outputlen = 04.
      WHEN 'PROD_QTD_COMERCI'.
        <fs_0201>-col_pos   = 06.
        <fs_0201>-do_sum    = abap_true.
        <fs_0201>-outputlen = 10.
      WHEN 'PROD_VLR_UND_COM'.
        <fs_0201>-col_pos   = 07.
        <fs_0201>-outputlen = 08.
      WHEN 'PROD_VLR_TOTAL_B'.
        <fs_0201>-col_pos   = 08.
        <fs_0201>-do_sum    = abap_true.
        <fs_0201>-outputlen = 10.
      WHEN 'COF_CST'.
        <fs_0201>-no_out    = abap_true.
        <fs_0201>-scrtext_l = 'COFINS'.
        <fs_0201>-scrtext_m = 'CO'.
        <fs_0201>-scrtext_s = 'CO'.
        <fs_0201>-col_pos   = 09.
        <fs_0201>-outputlen = 02.
      WHEN 'PIS_CST'.
        <fs_0201>-no_out    = abap_true.
        <fs_0201>-scrtext_l = 'PIS'.
        <fs_0201>-scrtext_m = 'PI'.
        <fs_0201>-scrtext_s = 'PI'.
        <fs_0201>-col_pos   = 10.
        <fs_0201>-outputlen = 02.
      WHEN 'IPI_CST'.
        <fs_0201>-no_out    = abap_true.
        <fs_0201>-scrtext_l = 'IPI'.
        <fs_0201>-scrtext_m = 'IP'.
        <fs_0201>-scrtext_s = 'IP'.
        <fs_0201>-col_pos   = 11.
        <fs_0201>-outputlen = 02.
      WHEN 'ICMS_CST'.
        <fs_0201>-scrtext_l = 'ICMS'.
        <fs_0201>-scrtext_m = 'IC'.
        <fs_0201>-scrtext_s = 'IC'.
        <fs_0201>-col_pos   = 12.
        <fs_0201>-outputlen = 02.
      WHEN 'MATNR'.
        i_campo = <fs_0201>-fieldname.
        <fs_0201>-edit      = obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_0201>-col_pos   = 13.
        <fs_0201>-outputlen = 08.
      WHEN 'MEINS'.
        i_campo = <fs_0201>-fieldname.
        <fs_0201>-edit  = obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_0201>-col_pos   = 14.
        <fs_0201>-outputlen = 04.
      WHEN 'MENGE'.
        i_campo = <fs_0201>-fieldname.
        <fs_0201>-edit      = obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_0201>-col_pos   = 15.
        <fs_0201>-do_sum    = abap_true.
        <fs_0201>-outputlen = 10.
      WHEN 'EBELN'.
        i_campo = <fs_0201>-fieldname.
        <fs_0201>-edit      = obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_0201>-col_pos   = 16.
        <fs_0201>-outputlen = 10.
      WHEN 'EBELP'.
        i_campo = <fs_0201>-fieldname.
        <fs_0201>-edit      = obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = i_campo ).
        <fs_0201>-col_pos   = 17.
        <fs_0201>-outputlen = 05.
      WHEN 'ICO_FUNC_LOTE'.
        <fs_0201>-style     = cl_gui_alv_grid=>mc_style_button.
        <fs_0201>-scrtext_l = 'Lote'.
        <fs_0201>-scrtext_m = 'Lote'.
        <fs_0201>-scrtext_s = 'Lote'.
        <fs_0201>-icon      = abap_true.
        <fs_0201>-just      = 'C'.
        <fs_0201>-col_pos   = 18.
        <fs_0201>-outputlen = 03.
      WHEN OTHERS.
        <fs_0201>-no_out  = abap_true.
        <fs_0201>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0201 .

  wa_variant_0201-report      = sy-repid.
  wa_variant_0201-handle      = '0201'.
  wa_variant_0201-log_group   = abap_false.
  wa_variant_0201-username    = abap_false.
  wa_variant_0201-variant     = abap_false.
  wa_variant_0201-text        = abap_false.
  wa_variant_0201-dependvars  = abap_false.

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

  CLEAR: it_itens, it_lotes, it_lotes_c, it_itens_alv[], it_itens_alv, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[],
         it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv[], it_pedi_itens_alv, it_pedi_itens_alv[].

  DATA(nfe_inbound) = obj_nfe->get_info_nota( ).
  MOVE-CORRESPONDING nfe_inbound-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING nfe_inbound-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING nfe_inbound-nfe_alv  TO zde_nfe_dist_alv.
  MOVE nfe_inbound-nfe_base-itens   TO it_itens.
  MOVE nfe_inbound-nfe_base-pedidos TO it_pedidos.
  MOVE nfe_inbound-nfe_base-lotes   TO it_lotes.
  MOVE nfe_inbound-nfe_base-lotes_c TO it_lotes_c.

  LOOP AT it_itens INTO DATA(wa_itens).

* SELECT * FROM ekkn
*              INTO wa_ekkn
*              WHERE ebeln = wa_itens-ebeln
*               AND ebelp = wa_itens-ebelp.
*    ENDSELECT.
*
*    IF sy-subrc = 0.
*      wa_itens-unload_pt    = wa_ekkn-wempf.
*      wa_itens-gr_rcpt    = wa_ekkn-ablad.
*    ENDIF.

    MOVE-CORRESPONDING wa_itens TO it_itens_alv.
    it_itens_alv-ico_func_lote = icon_batch.
    APPEND it_itens_alv.
  ENDLOOP.

  LOOP AT nfe_inbound-nfe_pedidos_alv INTO DATA(wa_pedidos).

    SELECT * FROM ekkn
              INTO wa_ekkn
              WHERE ebeln = wa_pedidos-ebeln
               AND ebelp = wa_pedidos-ebelp.
    ENDSELECT.

    IF sy-subrc = 0.
      wa_pedidos-unload_pt    = wa_ekkn-wempf.
      wa_pedidos-gr_rcpt    = wa_ekkn-ablad.
    ENDIF.

    MOVE-CORRESPONDING wa_pedidos TO it_pedi_itens_alv.
    it_pedi_itens_alv-ico_func_lote = icon_batch.

    APPEND it_pedi_itens_alv.
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

**&---------------------------------------------------------------------*
**&      Form  HANDLE_HOTSPOT_CLICK
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM HANDLE_HOTSPOT_CLICK_0201
*         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
*               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.
*
*  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.
*
*  READ TABLE IT_ITENS_ALV INDEX ROW_ID INTO DATA(WA_NFE_ALV).
*
*  CASE FIELDNAME.
*    WHEN 'ICO_FUNC_LOTE'.
*
*      IF WA_NFE_ALV-MATNR IS INITIAL OR WA_NFE_ALV-MEINS IS INITIAL OR WA_NFE_ALV-MENGE IS INITIAL.
*        MESSAGE S104.
*        EXIT.
*      ENDIF.
*
*      WA_ITENS_SEL_LOTE = WA_NFE_ALV.
*
*      READ TABLE IT_ITENS_ALV ASSIGNING FIELD-SYMBOL(<FS_ITEM>) INDEX ROW_ID.
*
*      LOOP AT IT_ITENS_ALV ASSIGNING FIELD-SYMBOL(<FS_LIMPAR>) WHERE PROD_ITEM NE <FS_ITEM>-PROD_ITEM.
*        CLEAR: <FS_LIMPAR>-LINE_COLOR.
*      ENDLOOP.
*
*      READ TABLE IT_ITENS_ALV ASSIGNING FIELD-SYMBOL(<FS_ITEM2>) INDEX ROW_ID.
*      IF ( SY-SUBRC IS INITIAL ) AND ( <FS_ITEM2>-LINE_COLOR NE CS_LINE_COLOR_SELECIONADA ).
*        <FS_ITEM2>-LINE_COLOR = CS_LINE_COLOR_SELECIONADA.
*        PERFORM SETAR_LOTES_LINHA USING <FS_ITEM2>.
*      ENDIF.
*
*      GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
*      GS_ALV_REFRES_COND-COL = ABAP_TRUE.
*
*      IF CL_GRID_0201 IS NOT INITIAL.
*        CALL METHOD CL_GRID_0201->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE      = GS_ALV_REFRES_COND
*            I_SOFT_REFRESH = ABAP_TRUE.
*      ENDIF.
*
*      CALL METHOD CL_GUI_CFW=>FLUSH.
*
*      CALL SCREEN 9002 STARTING AT 40 10.
*
*      LOOP AT IT_ITENS_ALV ASSIGNING <FS_LIMPAR>.
*        CLEAR: <FS_LIMPAR>-LINE_COLOR.
*      ENDLOOP.
*
*      GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
*      GS_ALV_REFRES_COND-COL = ABAP_TRUE.
*
*      IF CL_GRID_0201 IS NOT INITIAL.
*        CALL METHOD CL_GRID_0201->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE      = GS_ALV_REFRES_COND
*            I_SOFT_REFRESH = ABAP_TRUE.
*      ENDIF.
*
*      CALL METHOD CL_GUI_CFW=>FLUSH.
*
*  ENDCASE.
*
*ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_PEDIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pesquisa_pedidos USING add_botao TYPE char01 itens TYPE zde_ekpo_help_saldo_t.

  DATA: node           TYPE treev_node,
        node_pedido    TYPE treev_node,
        item           TYPE mtreeitm,
        qtd_itens      TYPE i,
        i_matnr_t	     TYPE ehs_matnr_t,
        lc_itens       TYPE zib_nfe_dist_itm_t,
        it_filtro_1    TYPE zde_ekpo_help_saldo_t,
        wa_filtro_1    TYPE zde_ekpo_help_saldo,
        lc_menge_ekbe  TYPE c LENGTH 18, "1 Tipo  ZDE_VOLUME_UTLIZADO QUAN  13  3 Volume Útilizado
        lc_menge_saldo TYPE c LENGTH 18, "1 Tipo ZDE_VOLUME_SALDO  QUAN  13  3 Saldo
        lc_preco       TYPE c LENGTH 17,
        wa_ekpo_v      TYPE zde_ekpo_help_saldo,
        lc_pedido      TYPE REF TO zcl_pedido_compra.

  tree->delete_all_nodes( ).

  IF itens IS INITIAL.

    CLEAR: i_matnr_t,
           node_table,
           item_table,
           node_table[],
           item_table[].

    qtd_itens = 1.

    node-node_key   = qtd_itens.
    node-hidden     = ' '.                 " The node is visible,
    node-disabled   = ' '.                 " selectable,
    node-isfolder   = ' '.                 " a folder.
    node-expander   = ' '.
    node-n_image    = icon_space.
    node-exp_image  = icon_space.
    APPEND node TO node_table.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree-column1.
    item-class     = cl_gui_list_tree=>item_class_button. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_intensified.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = 'Add Pedido'.
    APPEND item TO item_table.

    IF add_botao EQ abap_true.
      CALL METHOD tree->add_nodes_and_items
        EXPORTING
          node_table                     = node_table
          item_table                     = item_table
          item_table_structure_name      = 'MTREEITM'
        EXCEPTIONS
          failed                         = 1
          cntl_system_error              = 3
          error_in_tables                = 4
          dp_error                       = 5
          table_structure_name_not_found = 6.
    ENDIF.

    CHECK add_botao EQ abap_false.

    IF ck_entrada_por_pedido EQ abap_false.
      MOVE it_itens[] TO lc_itens[].
      DESCRIBE TABLE it_itens LINES DATA(qtd_itens_1).
      DELETE lc_itens WHERE matnr EQ space.
      DESCRIBE TABLE lc_itens LINES DATA(qtd_itens_2).

      IF qtd_itens_1 EQ qtd_itens_2.

        SELECT * INTO TABLE @DATA(it_mara)
          FROM mara
           FOR ALL ENTRIES IN @lc_itens
          WHERE matnr EQ @lc_itens-matnr.

        SORT lc_itens BY matnr.
        DELETE ADJACENT DUPLICATES FROM lc_itens COMPARING matnr.
        LOOP AT lc_itens INTO DATA(wa_itens).
          APPEND wa_itens-matnr TO i_matnr_t.
          wa_filtro_1-matnr       = wa_itens-matnr.
          wa_filtro_1-menge_saldo = 0.
          LOOP AT it_itens INTO DATA(wa_item) WHERE matnr EQ wa_itens-matnr.
            ADD wa_item-menge TO wa_filtro_1-menge_saldo.
          ENDLOOP.
          IF wa_item-menge IS NOT INITIAL.
            READ TABLE it_mara WITH KEY matnr = wa_itens-matnr INTO DATA(wa_mara) BINARY SEARCH.
            IF wa_mara-meins NE wa_itens-meins AND wa_itens-meins IS NOT INITIAL.
              CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                EXPORTING
                  i_matnr              = wa_itens-matnr
                  i_in_me              = wa_itens-meins
                  i_out_me             = wa_mara-meins
                  i_menge              = wa_item-menge
                IMPORTING
                  e_menge              = wa_item-menge
                EXCEPTIONS
                  error_in_application = 1
                  error                = 2
                  OTHERS               = 3.

              IF sy-subrc IS INITIAL.
                APPEND wa_filtro_1 TO it_filtro_1.
              ELSE.
                MESSAGE w005(zmmped) WITH wa_itens-matnr wa_itens-meins wa_mara-meins.
              ENDIF.
            ELSE.
              APPEND wa_filtro_1 TO it_filtro_1.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.
    ENDIF.

    CREATE OBJECT lc_pedido.

    lc_pedido->get_pedido_compra_chave_e(
      EXPORTING
        i_lifnr               = zib_nfe_dist_ter-p_emissor " Nº conta do fornecedor
        i_bukrs               = zib_nfe_dist_ter-e_tomadora    " Empresa
        i_werks               = zib_nfe_dist_ter-f_tomadora    " Centro
        i_abrir_tela          = abap_false
        i_matnr_t             = i_matnr_t
        i_filtro_1            = it_filtro_1
      IMPORTING
        e_ekpo_t              = e_ekpo_t
      EXCEPTIONS
        nao_encontrado_pedido = 1
        OTHERS                = 2
    ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    ADD 1 TO qtd_itens.

  ELSE.
    qtd_itens = 2.
    LOOP AT node_table INTO node.
      IF node-node_key GT qtd_itens.
        qtd_itens = node-node_key.
      ENDIF.
    ENDLOOP.
    ADD 1 TO qtd_itens.
  ENDIF.

  MOVE itens[] TO e_ekpo_t[].

  IF e_ekpo_t IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_makt)
      FROM makt
       FOR ALL ENTRIES IN @e_ekpo_t
     WHERE spras EQ @sy-langu
       AND matnr EQ @e_ekpo_t-matnr.

    SORT it_makt BY matnr.
  ENDIF.

  SORT e_ekpo_t ASCENDING BY ebeln ebelp.

  CLEAR: wa_ekpo_v.

  LOOP AT e_ekpo_t INTO DATA(wa_ekpo_t).

    DELETE it_pedido_validado
     WHERE ebeln = wa_ekpo_t-ebeln
       AND ebelp = wa_ekpo_t-ebelp.

    TRY .
        zcl_nfe_inbound=>get_valida_item_pedido(
          EXPORTING
            i_nfe      = zib_nfe_dist_ter
            i_ebeln    = wa_ekpo_t-ebeln
            i_ebelp    = wa_ekpo_t-ebelp
          IMPORTING
            e_validado = DATA(e_check)
            e_msg      = DATA(e_msg_ped_01) ).
      CATCH zcx_nfe_inbound_exception.
    ENDTRY.

    WRITE wa_ekpo_t-menge_ekbe  TO lc_menge_ekbe.
    WRITE wa_ekpo_t-menge_saldo TO lc_menge_saldo.
    WRITE wa_ekpo_t-netpr       TO lc_preco.

    IF ck_entrada_por_pedido EQ abap_true.

      IF wa_ekpo_v-ebeln NE wa_ekpo_t-ebeln.
        CLEAR: node_pedido.
        node_pedido-node_key   = qtd_itens.
        node_pedido-hidden     = ' '.                 " The node is visible,
        node_pedido-disabled   = ' '.                 " selectable,
        node_pedido-isfolder   = ' '.                 " a folder.
        node_pedido-expander   = ' '.
        node_pedido-n_image    = icon_delivery_complete.
        node_pedido-exp_image  = icon_delivery_complete.
        node_pedido-dragdropid = g_handle_tree.
        APPEND node_pedido TO node_table.

        CLEAR item.
        item-node_key  = node_pedido-node_key.
        item-item_name = c_tree-column1.
        item-class     = cl_gui_list_tree=>item_class_text. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_intensified.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = wa_ekpo_t-ebeln.
        APPEND item TO item_table.
        ADD 1 TO qtd_itens.

        wa_ekpo_v-ebeln = wa_ekpo_t-ebeln.
      ENDIF.
    ENDIF.

    "IF CK_ENTRADA_POR_PEDIDO EQ ABAP_TRUE.
    "  NODE-RELATKEY = NODE_PEDIDO-NODE_KEY.
    "  NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    "ENDIF.
    CLEAR node.
    node-node_key   = qtd_itens.
    node-hidden     = ' '.                 " The node is visible,
    node-disabled   = ' '.                 " selectable,
    node-isfolder   = ' '.                 " a folder.
    node-expander   = ' '.
    CASE e_check.
      WHEN abap_true.
        node-n_image    = icon_led_green.
        node-exp_image  = icon_led_green.
      WHEN abap_false.
        node-n_image    = icon_led_red.
        node-exp_image  = icon_led_red.
    ENDCASE.
    node-dragdropid = g_handle_tree.
    APPEND node TO node_table.

    IF e_check EQ abap_false.
      APPEND VALUE #( ebeln = wa_ekpo_t-ebeln ebelp = wa_ekpo_t-ebelp motivo = e_msg_ped_01 node_key = node-node_key ) TO it_pedido_validado.
    ENDIF.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree-column1.
    CASE e_check.
      WHEN abap_true.
        item-class     = cl_gui_list_tree=>item_class_text. " Text Item
      WHEN abap_false.
        item-class     = cl_gui_list_tree=>item_class_link.
    ENDCASE.
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_intensified.
    item-font      = cl_gui_list_tree=>item_font_prop.
    CONCATENATE wa_ekpo_t-ebeln wa_ekpo_t-ebelp INTO item-text SEPARATED BY '-'.
    APPEND item TO item_table.

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_ekpo_t-matnr BINARY SEARCH.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree-column2.
    item-class     = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = wa_makt-maktx.
    APPEND item TO item_table.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree-column3.
    item-class     = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_emphasized_positive.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = lc_menge_saldo.
    APPEND item TO item_table.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree-column4.
    item-class     = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_emphasized_positive.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = lc_preco.
    APPEND item TO item_table.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree-column5.
    item-class     = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = wa_ekpo_t-ernam.
    APPEND item TO item_table.

    CLEAR: wa_makt.
    ADD 1 TO qtd_itens.
  ENDLOOP.

  CALL METHOD tree->add_nodes_and_items
    EXPORTING
      node_table                     = node_table
      item_table                     = item_table
      item_table_structure_name      = 'MTREEITM'
    EXCEPTIONS
      failed                         = 1
      cntl_system_error              = 3
      error_in_tables                = 4
      dp_error                       = 5
      table_structure_name_not_found = 6.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_ITEM_NAME  text
*----------------------------------------------------------------------*
FORM mostrar_node  USING  p_node_key  TYPE  tv_nodekey
                          p_item_name TYPE  tv_itmname.


ENDFORM.

FORM limpar_tela.

  CLEAR: event_receiver_0201.
  CLEAR: event_receiver_itens_pedi.
  CLEAR: obg_toolbar_0201.
  CLEAR: toolbarmanager_0201.
  CLEAR: obg_toolbar_0204.
  CLEAR: toolbarmanager_0204.

  IF ct_alv_itens_nfe IS NOT INITIAL.
    ct_alv_itens_nfe->free( ).
  ENDIF.

  CLEAR: ct_alv_itens_nfe.

  IF ct_alv_itens_pedi IS NOT INITIAL.
    ct_alv_itens_pedi->free( ).
  ENDIF.

  CLEAR: ct_alv_itens_pedi.

  IF cl_grid_0201 IS NOT INITIAL.
    cl_grid_0201->free( ).
  ENDIF.

  CLEAR: cl_grid_0201.

  IF container_0201 IS NOT INITIAL.
    container_0201->free( ).
  ENDIF.

  CLEAR: container_0201.

  IF dragdrop_alv IS NOT INITIAL.
    dragdrop_alv->destroy( ).
  ENDIF.

  CLEAR: dragdrop_alv.

  IF dragdrop_tree IS NOT INITIAL.
    dragdrop_tree->destroy( ).
  ENDIF.

  CLEAR: dragdrop_tree.

  CLEAR: tree_event_receiver.

  CLEAR: g_application.

  IF tree IS NOT INITIAL.
    tree->free( ).
  ENDIF.
  CLEAR: tree.

  IF docking IS NOT INITIAL.
    docking->free( ).
  ENDIF.

  CLEAR: docking.
  IF obj_nfe IS NOT INITIAL.
    obj_nfe->free( ).
  ENDIF.
  CLEAR: obj_nfe.

  CLEAR: zib_nfe_dist_ter,
         zde_nfe_dist_alv,
         zde_nfe_dist_itm_alv,
         zde_nfe_inbound_alv,
         zib_nfe_dist_lot.

  CLEAR: lc_txt_butxt,
         ck_alterou_ck_possui_frete,
         ck_alterou_transportador,
         ck_alterou_armazem,
         ck_alterou_nr_fase,
         ck_alterou_boleto,
         ck_alterou_dt_vencimento,
         ck_alterou_departamento,
         cl_informado_new_item,
         ck_alterou_quantidade_split,
         ck_alterou_lote_info,
         ck_alterou_lote,
         i_campo,
         lc_transportador_cnpj,
         lc_transportador_ie,
         lc_transportador_razao,
         lc_transportador_regio,
         lc_fornecedor_regio,
         lc_destinatario_regio,
         lc_armazem_regio,
         it_itens,
         it_lotes,
         it_lotes_c,
         it_itens_alv,
         it_itens_alv_sel,
         it_lotes_alv_t,
         it_lotes_alv_u,
         it_lotes_alv_sel,
         it_carac_alv,
         it_carac_alv_u,
         e_ekpo_t,
         it_function_0201,
         wa_layout_0201,
         it_fieldcat_0201,
         it_sort_0201,
         wa_variant_0201,
         wa_stable_0201,
         it_selected_0201,
         wa_selected_0201,
         it_f4_0201,
         wa_f4_0201,
         node_table,
         item_table,
         gs_scroll_col,
         gs_scroll_row,
         it_selected_rows,
         wa_selected_rows,
         "WA_ITENS_SEL_LOTE,
         lc_info_forne,
         lt_texto_bloqueio_pagmento,
         lt_texto_forma_de_pagmento,
         ck_alterou_zbvtyp,
         ck_alterou_zlspr,
         ck_alterou_pymt_meth,
         ck_alterou_housebankid,
         ck_alterou_vlr_desconto,
         ck_alterou_obs_financeira,
         ck_alterou_ck_fpol, "*-CS2024000243-05.06.2024-#136397-JT
         it_pedido_validado.

  CLEAR: it_itens_alv[],
         it_itens_alv_sel[],
         it_lotes_alv_t[],
         it_lotes_alv_u[],
         it_lotes_alv_sel[],
         it_carac_alv[],
         it_carac_alv_u[],
         it_pedido_validado[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REGISTER_EVENTS
*&---------------------------------------------------------------------*
FORM register_events.
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.
* define the events which will be passed to the backend
  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.

  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.

  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_link_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.

  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.

  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_button_click.
  APPEND l_event TO lt_events.

* register events
  CALL METHOD tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE x534(0k).
  ENDIF.

ENDFORM.                               " REGISTER_EVENTS

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  IF cl_grid_0201 IS NOT INITIAL.
    CALL METHOD cl_grid_0201->get_scroll_info_via_id
      IMPORTING
        es_col_info = gs_scroll_col
        es_row_no   = gs_scroll_row.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  CLEAR it_selected_rows.

  CHECK cl_grid_0201 IS NOT INITIAL.

  CALL METHOD cl_grid_0201->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_itens_alv_sel[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_itens_alv INTO DATA(wa_itens_alv) INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING wa_itens_alv TO it_itens_alv_sel.
    APPEND it_itens_alv_sel.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.
  ck_alterou_quantidade_split = abap_false.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001_exit INPUT.
  CLEAR: ok_code.
  ck_alterou_quantidade_split = abap_false.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_QUANTIDADE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_quantidade INPUT.
  ck_alterou_quantidade_split = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CHECK ck_alterou_quantidade_split EQ abap_false.

  CASE ok_code.
    WHEN 'CONFIRMAR'.
      CLEAR: ok_code.
      cl_informado_new_item = abap_true.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

**&---------------------------------------------------------------------*
**&      Module  STATUS_9002  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE STATUS_9002 OUTPUT.
*
*  SET PF-STATUS 'PF9002'.
*  SET TITLEBAR 'TL9002'.
*
*  DATA: IT_LOTE_CARACT TYPE ZIB_NFE_DIST_LCA_T,
*        WA_LOTE_CARACT TYPE ZIB_NFE_DIST_LCA.
*
*  CLEAR: IT_LOTE_CARACT, WA_LOTE_CARACT.
*
*  IF CK_ALTEROU_LOTE_INFO EQ ABAP_TRUE OR CK_ALTEROU_LOTE EQ ABAP_TRUE.
*
*    CLEAR: IT_LOTE_CARACT.
*    LOOP AT IT_CARAC_ALV_U.
*      MOVE-CORRESPONDING IT_CARAC_ALV_U TO WA_LOTE_CARACT.
*      APPEND WA_LOTE_CARACT TO IT_LOTE_CARACT.
*    ENDLOOP.
*
*    OBJ_NFE->SET_LOTE_ITEM( CHANGING I_LOTE = ZIB_NFE_DIST_LOT I_LOTE_CARACT = IT_LOTE_CARACT ).
*
*    READ TABLE IT_LOTES_ALV_U ASSIGNING FIELD-SYMBOL(<FS_LOTE>) WITH KEY CD_LOTE_ITEM = ZIB_NFE_DIST_LOT-CD_LOTE_ITEM.
*    MOVE-CORRESPONDING ZIB_NFE_DIST_LOT TO <FS_LOTE>.
*
*    READ TABLE IT_LOTES_ALV_T ASSIGNING FIELD-SYMBOL(<FS_LOTEU>) WITH KEY CD_LOTE_ITEM = ZIB_NFE_DIST_LOT-CD_LOTE_ITEM.
*    MOVE-CORRESPONDING ZIB_NFE_DIST_LOT TO <FS_LOTEU>.
*
*    CLEAR: IT_CARAC_ALV_U[].
*
*    LOOP AT IT_LOTE_CARACT INTO DATA(RT_METODO).
*      MOVE-CORRESPONDING RT_METODO TO IT_CARAC_ALV_U.
*      READ TABLE IT_CARAC_ALV ASSIGNING FIELD-SYMBOL(<FS_CARAC>) WITH KEY CD_LOTE_ITEM = RT_METODO-CD_LOTE_ITEM
*                                                                          ATINN        = RT_METODO-ATINN.
*      MOVE-CORRESPONDING RT_METODO TO <FS_CARAC>.
*      APPEND IT_CARAC_ALV_U.
*    ENDLOOP.
*    CK_ALTEROU_LOTE_INFO = ABAP_FALSE.
*  ENDIF.
*
*  LOOP AT SCREEN.
*    IF SCREEN-NAME(16) EQ 'ZIB_NFE_DIST_LOT'.
*      SPLIT SCREEN-NAME AT '-' INTO DATA(STR1_9002_B) DATA(STR2_9002_B).
*      I_CAMPO = STR2_9002_B.
*      IF OBJ_NFE->VALIDA_ATRIBUTO_ALTERAVEL_LOTE( I_CAMPO = I_CAMPO I_PROD_ITEM = WA_ITENS_SEL_LOTE-PROD_ITEM  I_LOTE = ZIB_NFE_DIST_LOT-CHARG ) EQ ABAP_TRUE.
*        SCREEN-INPUT = 1.
*      ELSE.
*        SCREEN-INPUT = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*
*  IF CK_ALTEROU_LOTE EQ ABAP_TRUE.
*    PERFORM LIMPAR_TELA_LOTE.
*  ENDIF.
*
*  "Lotes
*  IF CONTAINER_9002_A IS INITIAL.
*    CLEAR WA_LAYOUT_9002_A.
*    WA_LAYOUT_9002_A-ZEBRA      = ABAP_TRUE.
*    WA_STABLE_9002_A-ROW        = ABAP_TRUE.
*    WA_STABLE_9002_A-COL        = ABAP_TRUE.
*    WA_LAYOUT_9002_A-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT_9002_A-SEL_MODE   = 'A'.
*    "WA_LAYOUT_9002_A-CWIDTH_OPT = 'X'.
*    "WA_LAYOUT_9002_A-COL_OPT    = 'X'.
*    WA_LAYOUT_9002_A-INFO_FNAME = 'LINE_COLOR'.
*    WA_LAYOUT_9002_A-CTAB_FNAME = 'COLOR_CELL'.
*    WA_LAYOUT_9002_A-NO_TOOLBAR = ABAP_FALSE.
*
*    IF ZIB_NFE_DIST_TER-ST_FISICO NE ZCL_NFE_INBOUND=>ST_FISICO_00.
*      WA_LAYOUT_9002_A-NO_TOOLBAR = ABAP_TRUE.
*    ENDIF.
*
*    CREATE OBJECT CONTAINER_9002_A
*      EXPORTING
*        CONTAINER_NAME = 'ALV_LOTES'.
*
*    PERFORM FILL_IT_FIELDCATALOG_9002_A.
*
**   Fill info for layout variant
*    PERFORM FILL_GS_VARIANT_9002_A.
*
*    CREATE OBJECT CL_GRID_9002_A
*      EXPORTING
*        I_PARENT = CONTAINER_9002_A.
*
*    CREATE OBJECT OBG_TOOLBAR_9002_A
*      EXPORTING
*        IO_ALV_GRID = CL_GRID_9002_A.
*
*    SET HANDLER OBG_TOOLBAR_9002_A->ON_TOOLBAR FOR CL_GRID_9002_A.
*    SET HANDLER OBG_TOOLBAR_9002_A->HANDLE_USER_COMMAND FOR CL_GRID_9002_A.
*
*    CREATE OBJECT EVENT_RECEIVER_9002_A.
*    SET HANDLER EVENT_RECEIVER_9002_A->HANDLE_DOUBLE_CLICK_9002_A FOR CL_GRID_9002_A.
*
*    CALL METHOD CL_GRID_9002_A->SET_TABLE_FOR_FIRST_DISPLAY
*      EXPORTING
*        IS_VARIANT                    = WA_VARIANT_9002_A
*        I_SAVE                        = 'A'
*        IS_LAYOUT                     = WA_LAYOUT_9002_A
*        IT_TOOLBAR_EXCLUDING          = IT_FUNCTION_9002_A
*      CHANGING
*        IT_OUTTAB                     = IT_LOTES_ALV_U[]
*        IT_FIELDCATALOG               = IT_FIELDCAT_9002_A
*        IT_SORT                       = IT_SORT_9002_A
**       IT_FILTER                     =
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*  ENDIF.
*
*  "Características
*  IF CONTAINER_9002_B IS INITIAL.
*    CLEAR WA_LAYOUT_9002_B.
*    WA_LAYOUT_9002_B-ZEBRA      = ABAP_TRUE.
*    WA_STABLE_9002_B-ROW        = ABAP_TRUE.
*    WA_STABLE_9002_B-COL        = ABAP_TRUE.
*    WA_LAYOUT_9002_B-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT_9002_B-SEL_MODE   = 'A'.
*    "WA_LAYOUT_9002_B-CWIDTH_OPT = 'X'.
*    "WA_LAYOUT_9002_B-COL_OPT    = 'X'.
*    WA_LAYOUT_9002_B-INFO_FNAME = 'LINE_COLOR'.
*    WA_LAYOUT_9002_B-CTAB_FNAME = 'COLOR_CELL'.
*    WA_LAYOUT_9002_B-NO_TOOLBAR = ABAP_TRUE.
*
*    CREATE OBJECT CONTAINER_9002_B
*      EXPORTING
*        CONTAINER_NAME = 'ALV_CARACTERISTICAS'.
*
*    PERFORM FILL_IT_FIELDCATALOG_9002_B.
*
**   Fill info for layout variant
*    PERFORM FILL_GS_VARIANT_9002_B.
*
*    CREATE OBJECT CL_GRID_9002_B
*      EXPORTING
*        I_PARENT = CONTAINER_9002_B.
*
*    CL_GRID_9002_B->REGISTER_EDIT_EVENT( EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).
*    CL_GRID_9002_B->REGISTER_EDIT_EVENT( EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
*
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW      TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO IT_FUNCTION_9002_B.
*    APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO IT_FUNCTION_9002_B.
*
*    CREATE OBJECT EVENT_RECEIVER_9002_B.
*    SET HANDLER EVENT_RECEIVER_9002_B->HANDLE_DATA_CHANGED_9002_B FOR CL_GRID_9002_B.
*
*    CALL METHOD CL_GRID_9002_B->SET_TABLE_FOR_FIRST_DISPLAY
*      EXPORTING
*        IS_VARIANT                    = WA_VARIANT_9002_B
*        I_SAVE                        = 'A'
*        IS_LAYOUT                     = WA_LAYOUT_9002_B
*        IT_TOOLBAR_EXCLUDING          = IT_FUNCTION_9002_B
*      CHANGING
*        IT_OUTTAB                     = IT_CARAC_ALV_U[]
*        IT_FIELDCATALOG               = IT_FIELDCAT_9002_B
*        IT_SORT                       = IT_SORT_9002_B
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*  ENDIF.
*
*  CALL METHOD CL_GRID_9002_A->REFRESH_TABLE_DISPLAY
*    EXPORTING
*      IS_STABLE = WA_STABLE_9002_A.
*
*  CALL METHOD CL_GRID_9002_B->REFRESH_TABLE_DISPLAY
*    EXPORTING
*      IS_STABLE = WA_STABLE_9002_B.
*
*
*ENDMODULE.

**&---------------------------------------------------------------------*
**&      Form  DOUBLE_CLICK_1601
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM DOUBLE_CLICK_9002_A  USING    P_E_ROW     TYPE LVC_S_ROW
*                                 P_E_COLUMN  TYPE LVC_S_COL
*                                 P_ES_ROW_NO TYPE LVC_S_ROID.
*  IF P_E_ROW-ROWTYPE IS INITIAL.
*    READ TABLE IT_LOTES_ALV_U INDEX P_E_ROW-INDEX.
*    PERFORM SELECIONAR_LOTE USING WA_ITENS_SEL_LOTE-PROD_ITEM IT_LOTES_ALV_U-CD_LOTE_ITEM ABAP_TRUE.
*  ENDIF.
*ENDFORM.

CLASS lcl_event_receiver_9002_b IMPLEMENTATION.
  METHOD handle_data_changed_9002_b.
    error_in_data = abap_false.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data EQ abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ELSE.
      PERFORM data_changed_9002_b USING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
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

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_9002_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_9002_b  USING  e_onf4           TYPE char01
                               e_onf4_after     TYPE char01
                               e_onf4_before    TYPE char01
                               e_ucomm          TYPE sy-ucomm
                               er_data_changed  TYPE REF TO cl_alv_changed_data_protocol.

  DATA: lc_atwrt  TYPE atwrt,
        i_caract  TYPE zib_nfe_dist_lca_t,
        wa_caract TYPE zib_nfe_dist_lca.

  CLEAR: i_caract.

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_mod_cells).

    READ TABLE it_carac_alv_u ASSIGNING FIELD-SYMBOL(<fs_caracu>) INDEX ls_mod_cells-row_id.
    READ TABLE it_carac_alv   ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = <fs_caracu>-cd_lote_item
                                                                          atinn        = <fs_caracu>-atinn.
    CASE ls_mod_cells-fieldname.
      WHEN  'ATWRT'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = lc_atwrt.

        IF lc_atwrt NE <fs_caracu>-atwrt.
          <fs_caracu>-atwrt = lc_atwrt.
          <fs_carac>-atwrt  = lc_atwrt.
          MOVE-CORRESPONDING <fs_carac> TO wa_caract.
          APPEND wa_caract TO i_caract.
        ENDIF.
    ENDCASE.

    IF i_caract IS NOT INITIAL.
      obj_nfe->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = i_caract ).
    ENDIF.

  ENDLOOP.

ENDFORM.

**&---------------------------------------------------------------------*
**&      Form  INCLUIR_LOTE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM INCLUIR_LOTE .
*
*  DATA: E_CARACTERISTICAS  TYPE ZIB_NFE_DIST_LCA_T,
*        WA_CARAC_ALV      TYPE TY_ITENS_CARAC_ALV,
*        WA_LOTES_ALV      TYPE TY_ITENS_LOTES_ALV,
*        P_INDEX           TYPE LVC_INDEX.
*
*  IF WA_ITENS_SEL_LOTE IS INITIAL.
*    MESSAGE S083.
*    EXIT.
*  ENDIF.
*
*  CHECK ZIB_NFE_DIST_TER-ST_FISICO EQ ZCL_NFE_INBOUND=>ST_FISICO_00.
*
*  TRY.
*      DATA(R_LOTE) = OBJ_NFE->ADD_LOTE_ITEM( EXPORTING I_PROD_ITEM = WA_ITENS_SEL_LOTE-PROD_ITEM
*                                             IMPORTING E_CARACTERISTICAS = E_CARACTERISTICAS ).
*      "Alimenta Tabelas com dados base
*      CLEAR: WA_LOTES_ALV.
*      MOVE-CORRESPONDING R_LOTE TO WA_LOTES_ALV.
*      APPEND WA_LOTES_ALV TO IT_LOTES_ALV_T.
*
*      LOOP AT E_CARACTERISTICAS INTO DATA(WA_CARACTERISTICAS) WHERE CD_LOTE_ITEM EQ R_LOTE-CD_LOTE_ITEM.
*        CLEAR: WA_CARAC_ALV.
*        MOVE-CORRESPONDING WA_CARACTERISTICAS TO WA_CARAC_ALV.
*        APPEND WA_CARAC_ALV TO IT_CARAC_ALV.
*      ENDLOOP.
*
*      READ TABLE IT_LOTES_ALV_T WITH KEY CD_LOTE_ITEM = WA_LOTES_ALV-CD_LOTE_ITEM.
*      MOVE SY-TABIX TO P_INDEX.
*      PERFORM SELECIONAR_LOTE USING WA_ITENS_SEL_LOTE-PROD_ITEM R_LOTE-CD_LOTE_ITEM ABAP_TRUE.
*
*    CATCH ZCX_NFE_INBOUND_EXCEPTION INTO EX_NFE_INBOUND.
*      EX_NFE_INBOUND->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E').
*      EXIT.
*    CATCH ZCX_CHARG_EXCEPTION INTO EX_CHARG.
*      EX_CHARG->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E').
*      EXIT.
*    CATCH ZCX_CADASTRO INTO EX_CADASTRO.
*      EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E').
*      EXIT.
*  ENDTRY.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_lote .

  DATA: p_index  TYPE lvc_index.

  IF it_lotes_alv_sel[] IS INITIAL.
    MESSAGE s082.
    EXIT.
  ENDIF.

  LOOP AT it_lotes_alv_sel.
    obj_nfe->excluir_lote_item( i_cd_lote_item = it_lotes_alv_sel-cd_lote_item ).
  ENDLOOP.

  CLEAR: it_lotes, it_lotes_c, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[], it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u.
  DATA(lc_info_nfe) = obj_nfe->get_info_nota( ).
  MOVE-CORRESPONDING lc_info_nfe-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_dist_alv.
  MOVE lc_info_nfe-nfe_base-lotes   TO it_lotes.
  MOVE lc_info_nfe-nfe_base-lotes_c TO it_lotes_c.

  LOOP AT it_lotes INTO DATA(wa_lotes).
    MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
    APPEND it_lotes_alv_t.
  ENDLOOP.

  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
    APPEND it_carac_alv.
  ENDLOOP.

  READ TABLE it_itens_alv INTO DATA(wa_item_selecionado) WITH KEY line_color = cs_line_color_selecionada.
  IF sy-subrc IS INITIAL.
    READ TABLE it_lotes_alv_t WITH KEY prod_item = wa_item_selecionado-prod_item.
    IF sy-subrc IS INITIAL.
      p_index = sy-tabix.
      PERFORM selecionar_lote USING wa_item_selecionado-prod_item it_lotes_alv_t-cd_lote_item abap_false.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 9002.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_lote_ped .

  DATA: p_index  TYPE lvc_index.

  IF it_lotes_alv_sel[] IS INITIAL.
    MESSAGE s082.
    EXIT.
  ENDIF.

  LOOP AT it_lotes_alv_sel.
    obj_nfe->excluir_lote_item( i_cd_lote_item = it_lotes_alv_sel-cd_lote_item ).
  ENDLOOP.

  CLEAR: it_lotes, it_lotes_c, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[], it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u.
  DATA(lc_info_nfe) = obj_nfe->get_info_nota( ).
  MOVE-CORRESPONDING lc_info_nfe-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_dist_alv.
  MOVE lc_info_nfe-nfe_base-lotes   TO it_lotes.
  MOVE lc_info_nfe-nfe_base-lotes_c TO it_lotes_c.

  LOOP AT it_lotes INTO DATA(wa_lotes).
    MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
    APPEND it_lotes_alv_t.
  ENDLOOP.

  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
    APPEND it_carac_alv.
  ENDLOOP.

  READ TABLE it_itens_alv INTO DATA(wa_item_selecionado) WITH KEY line_color = cs_line_color_selecionada.
  IF sy-subrc IS INITIAL.
    READ TABLE it_lotes_alv_t WITH KEY prod_item = wa_item_selecionado-prod_item.
    IF sy-subrc IS INITIAL.
      p_index = sy-tabix.
      PERFORM selecionar_lote USING wa_item_selecionado-prod_item it_lotes_alv_t-cd_lote_item abap_false.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 9002.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9002_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_9002_a .

  DATA: i_contador_2 TYPE lvc_colpos.

  CLEAR: it_fieldcat_9002_a[], it_fieldcat_9002_a.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LOT'
    CHANGING
      ct_fieldcat      = it_fieldcat_9002_a.

  i_contador_2 = 3.

  LOOP AT it_fieldcat_9002_a ASSIGNING FIELD-SYMBOL(<fs_9002_a>).
    <fs_9002_a>-edit = abap_false.
    CASE <fs_9002_a>-fieldname.
      WHEN 'CHARG'.
        <fs_9002_a>-col_pos   = 1.
        <fs_9002_a>-outputlen = 10.
      WHEN 'VFDAT'.
        <fs_9002_a>-col_pos   = 2.
        <fs_9002_a>-outputlen = 10.
      WHEN 'LICHA'.
        <fs_9002_a>-col_pos   = 3.
        <fs_9002_a>-outputlen = 20.
      WHEN 'HSDAT'.
        <fs_9002_a>-col_pos   = 4.
        <fs_9002_a>-outputlen = 10.
      WHEN 'MENGE'.
        <fs_9002_a>-edit      = abap_false.
        <fs_9002_a>-col_pos   = 5.
        <fs_9002_a>-outputlen = 10.
        <fs_9002_a>-do_sum    = abap_true.
      WHEN OTHERS.
        <fs_9002_a>-no_out  = abap_true.
        <fs_9002_a>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9002_A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_9002_a .

  wa_variant_9002_a-report      = sy-repid.
  wa_variant_9002_a-handle      = '9001'.
  wa_variant_9002_a-log_group   = abap_false.
  wa_variant_9002_a-username    = abap_false.
  wa_variant_9002_a-variant     = abap_false.
  wa_variant_9002_a-text        = abap_false.
  wa_variant_9002_a-dependvars  = abap_false.

ENDFORM.

**&---------------------------------------------------------------------*
**&      Form  FILL_IT_FIELDCATALOG_9002_B
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM FILL_IT_FIELDCATALOG_9002_B .
*
*  DATA: I_CONTADOR_2 TYPE LVC_COLPOS,
*        I_CAMPO       TYPE NAME_FELD,
*        I_PROD_ITEM   TYPE J_1BITMNUM,
*        I_LOTE       TYPE CHARG_D.
*
*  CLEAR: IT_FIELDCAT_9002_B[], IT_FIELDCAT_9002_B.
*
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      I_STRUCTURE_NAME = 'ZIB_NFE_DIST_LCA'
*    CHANGING
*      CT_FIELDCAT      = IT_FIELDCAT_9002_B.
*
*  I_CONTADOR_2 = 3.
*  I_CAMPO     = 'VFDAT'.
*  I_PROD_ITEM = WA_ITENS_SEL_LOTE-PROD_ITEM.
*  I_LOTE      = ZIB_NFE_DIST_LOT-CHARG.
*
*  DATA(CK_ALTERA) = OBJ_NFE->VALIDA_ATRIBUTO_ALTERAVEL_LOTE( I_CAMPO = I_CAMPO I_PROD_ITEM = WA_ITENS_SEL_LOTE-PROD_ITEM  I_LOTE = ZIB_NFE_DIST_LOT-CHARG ).
*
*  LOOP AT IT_FIELDCAT_9002_B ASSIGNING FIELD-SYMBOL(<FS_9002_B>).
*    <FS_9002_B>-EDIT = ABAP_FALSE.
*    CASE <FS_9002_B>-FIELDNAME.
*      WHEN 'SMBEZ'.
*        <FS_9002_B>-COL_POS   = 1.
*        <FS_9002_B>-OUTPUTLEN = 15.
*      WHEN 'ATWRT'.
*        <FS_9002_B>-EDIT      = CK_ALTERA.
*        <FS_9002_B>-OUTPUTLEN = 20.
*        <FS_9002_B>-COL_POS   = 2.
*        <FS_9002_B>-OUTPUTLEN = 20.
*      WHEN OTHERS.
*        <FS_9002_B>-NO_OUT  = ABAP_TRUE.
*        <FS_9002_B>-COL_POS = I_CONTADOR_2.
*        ADD 1 TO I_CONTADOR_2.
*    ENDCASE.
*  ENDLOOP.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9002_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_9002_b .

  wa_variant_9002_b-report      = sy-repid.
  wa_variant_9002_b-handle      = '9002'.
  wa_variant_9002_b-log_group   = abap_false.
  wa_variant_9002_b-username    = abap_false.
  wa_variant_9002_b-variant     = abap_false.
  wa_variant_9002_b-text        = abap_false.
  wa_variant_9002_b-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_E_ROW_INDEX  text
*      -->P_1002   text
*----------------------------------------------------------------------*
FORM selecionar_lote  USING p_prod_item	    TYPE j_1bitmnum
                            p_cd_lote_item  TYPE zde_cd_lote_item
                            p_atualiza_tela TYPE char01.

  CLEAR: it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u, it_lotes_alv_u, it_lotes_alv_u[].

  LOOP AT it_lotes_alv_t WHERE prod_item EQ p_prod_item.
    APPEND it_lotes_alv_t TO it_lotes_alv_u.
  ENDLOOP.

  READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lotes>) WITH KEY cd_lote_item = p_cd_lote_item.
  CHECK sy-subrc IS INITIAL.

  LOOP AT it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_limpar>) WHERE prod_item NE p_prod_item.
    CLEAR: <fs_limpar>-line_color.
  ENDLOOP.

  IF <fs_lotes>-line_color NE cs_line_color_selecionada.

    <fs_lotes>-line_color = cs_line_color_selecionada.

    "Tem um Lote
    MOVE-CORRESPONDING <fs_lotes> TO zib_nfe_dist_lot.

    LOOP AT it_carac_alv INTO DATA(wa_carac_alv) WHERE cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
      APPEND wa_carac_alv TO it_carac_alv_u.
    ENDLOOP.

  ENDIF.

  "PERFORM ATUALIZA_TELA_9002_B.

  "IF P_ATUALIZA_TELA EQ ABAP_TRUE.
  "  LEAVE TO SCREEN 9002.
  "ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_E_ROW_INDEX  text
*      -->P_1002   text
*----------------------------------------------------------------------*
FORM selecionar_lote_ped
  USING p_ebeln	        TYPE ebeln
        p_ebelp	        TYPE ebelp
        p_cd_lote_item  TYPE zde_cd_lote_item
        p_atualiza_tela TYPE char01.

  CLEAR: it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u, it_lotes_alv_u, it_lotes_alv_u[].

  LOOP AT it_lotes_alv_t
    WHERE ebeln EQ p_ebeln
      AND ebelp EQ p_ebelp.
    APPEND it_lotes_alv_t TO it_lotes_alv_u.
  ENDLOOP.

  READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lotes>) WITH KEY cd_lote_item = p_cd_lote_item.
  CHECK sy-subrc IS INITIAL.

  LOOP AT it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_limpar>) WHERE cd_lote_item NE p_cd_lote_item.
    CLEAR: <fs_limpar>-line_color.
  ENDLOOP.

  IF <fs_lotes>-line_color NE cs_line_color_selecionada.
    <fs_lotes>-line_color = cs_line_color_selecionada.
    MOVE-CORRESPONDING <fs_lotes> TO zib_nfe_dist_lot.

    LOOP AT it_carac_alv INTO DATA(wa_carac_alv) WHERE cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
      APPEND wa_carac_alv TO it_carac_alv_u.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_9002_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tela_9002_b .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  IF cl_grid_9002_b IS NOT INITIAL.
    CALL METHOD cl_grid_9002_b->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  IF cl_grid_9002_a IS NOT INITIAL.
    CALL METHOD cl_grid_9002_a->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " ATUALIZA_TELA

**&---------------------------------------------------------------------*
**&      Form  SETAR_LOTES_LINHA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_<FS_ITEM>  text
**----------------------------------------------------------------------*
*FORM SETAR_LOTES_LINHA  USING  P_ITEM TYPE TY_ITENS_ALV.
*
*  MOVE P_ITEM TO WA_ITENS_SEL_LOTE.
*
*  "Seleciona o primeiro lote do item
*  READ TABLE IT_LOTES_ALV_T WITH KEY CHAVE_NFE = P_ITEM-CHAVE_NFE
*                                     PROD_ITEM = P_ITEM-PROD_ITEM
*                          INTO DATA(WA_LOTES_ALV).
*  IF SY-SUBRC IS INITIAL.
*    PERFORM SELECIONAR_LOTE USING P_ITEM-PROD_ITEM WA_LOTES_ALV-CD_LOTE_ITEM ABAP_TRUE.
*  ELSE.
*    "Não tem um Lote
*    PERFORM INCLUIR_LOTE.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002_exit INPUT.
  PERFORM limpar_tela_lote.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  CASE ok_code.
    WHEN 'RETORNAR'.
      CLEAR: ok_code.
      PERFORM limpar_tela_lote.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tela_lote .

  IF cl_grid_9002_b IS NOT INITIAL.
    cl_grid_9002_b->free( ).
  ENDIF.
  CLEAR: cl_grid_9002_b.

  IF cl_grid_9002_a IS NOT INITIAL.
    cl_grid_9002_a->free( ).
  ENDIF.
  CLEAR: cl_grid_9002_a.

  IF container_9002_b IS NOT INITIAL.
    container_9002_b->free( ).
  ENDIF.
  CLEAR: container_9002_b.

  IF container_9002_a IS NOT INITIAL.
    container_9002_a->free( ).
  ENDIF.
  CLEAR: container_9002_a.

  ck_alterou_lote = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_lote INPUT.
  ck_alterou_lote = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_LOTE_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_lote_info INPUT.
  ck_alterou_lote_info = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_DEPARTAMEMTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_departamemto INPUT.
  ck_alterou_departamento = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERAR_BLOQUEIO_PAGAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_bloqueio_pagamento INPUT.

  IF zde_nfe_dist_alv-zlspr IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(wa_t008)
      FROM t008
     WHERE zahls EQ @zde_nfe_dist_alv-zlspr.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e079 WITH zde_nfe_dist_alv-zlspr.
    ENDIF.
  ENDIF.

  obj_nfe->set_bloqueio_pagamento( i_zlspr = zde_nfe_dist_alv-zlspr ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERAR_CTR_VALOR_TOTAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_ctr_valor_total INPUT.
  zde_nfe_dist_alv-ctr_wkurs = obj_nfe->set_ctr_valor_total( EXPORTING i_ctr_valor_total = zde_nfe_dist_alv-ctr_valor_total ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_DATA_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verificar_data_vencimento INPUT.

  obj_nfe->set_dt_vencimento( i_dt_vencimento = zde_nfe_dist_alv-dt_vencimento ).  "*-CS2024000243-05.06.2024-#136397-JT
  obj_nfe->set_fora_politica( EXPORTING i_ck_fpol = zde_nfe_dist_alv-ck_fpol ).    "*-CS2024000243-05.06.2024-#136397-JT
  obj_nfe->set_obs_financeira( EXPORTING i_obs_financeira = zde_nfe_dist_alv-obs_financeira ). "*-CS2024000243-05.06.2024-#136397-JT

  TRY.
      CALL METHOD zcl_miro=>verificar_vencimento_fatura
        EXPORTING
          i_data_vencimento = zde_nfe_dist_alv-dt_vencimento
          i_pymt_meth       = zde_nfe_dist_alv-pymt_meth
          i_data_se         = abap_true
          i_ck_revisao      = zde_nfe_dist_alv-ck_revisao
          i_valida_politica = abap_true                        "*-CS2024000243-05.06.2024-#136397-JT
          i_ck_fpol         = zde_nfe_dist_alv-ck_fpol         "*-CS2024000243-05.06.2024-#136397-JT
          i_obs_financeira  = zde_nfe_dist_alv-obs_financeira "*-CS2024000243-05.06.2024-#136397-JT
          i_bukrs           = zde_nfe_dist_alv-bukrs. "USER STORY 158527 - MMSILVA - 17.01.2025

    CATCH zcx_miro_exception INTO ex_miro.
      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
    CATCH zcx_error INTO ex_erro.
      ex_erro->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
  ENDTRY.

  obj_nfe->set_dt_vencimento( i_dt_vencimento = zde_nfe_dist_alv-dt_vencimento ).
  obj_nfe->set_fora_politica( EXPORTING i_ck_fpol = zde_nfe_dist_alv-ck_fpol ).  "*-CS2024000243-05.06.2024-#136397-JT
  obj_nfe->set_obs_financeira( EXPORTING i_obs_financeira = zde_nfe_dist_alv-obs_financeira ). "*-CS2024000243-05.06.2024-#136397-JT

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_BANCO_PARCEIRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_banco_parceiro INPUT.

  obj_nfe->set_banco_parceiro( i_bvtyp = zde_nfe_dist_alv-zbvtyp ).

  CLEAR: lc_info_forne.

  PERFORM get_info_banco_parceiro.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_BANCO_PARCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_banco_parceiro .

  DATA: lc_lfbk  TYPE lfbk,
        lc_bnka  TYPE bnka,
        lc_cte   TYPE zib_cte_dist_ter,
        lc_valor TYPE netwr_fp.

  "Tipo de banco do parceiro
  IF zde_nfe_dist_alv-zbvtyp IS NOT INITIAL.

    lc_cte-p_emissor = zde_nfe_dist_alv-p_emissor.
    lc_cte-zbvtyp    = zde_nfe_dist_alv-zbvtyp.

    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = lc_lfbk
        e_bnka     = lc_bnka
      CHANGING
        p_cte      = lc_cte
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      lc_info_forne-bankl = lc_bnka-bankl(3).
      lc_info_forne-banka = lc_bnka-banka.
      lc_info_forne-bankn = lc_lfbk-bankn.

      IF NOT lc_lfbk-bkont IS INITIAL.
        CONCATENATE lc_lfbk-bankl+4(11) '-' lc_lfbk-bkont INTO lc_info_forne-agenc.
      ELSE.
        lc_info_forne-agenc = lc_lfbk-bankl+4(11).
      ENDIF.

      CLEAR: lt_texto_forma_de_pagmento.

      IF zde_nfe_dist_alv-pymt_meth IS INITIAL AND zde_nfe_dist_alv-housebankid IS INITIAL.
        MOVE zde_nfe_dist_alv-ctr_valor_total TO lc_valor.

        CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
          EXPORTING
            p_bukrs           = zde_nfe_dist_alv-e_tomadora
            p_lifnr           = zde_nfe_dist_alv-p_emissor
            p_zlsch           = zde_nfe_dist_alv-pymt_meth
            p_valor           = lc_valor
            p_bvtyp           = zde_nfe_dist_alv-zbvtyp
            p_waers           = zde_nfe_dist_alv-ctr_waers
          IMPORTING
            p_forma_pagamento = zde_nfe_dist_alv-pymt_meth
            p_princ_bnc_emp   = zde_nfe_dist_alv-housebankid
          EXCEPTIONS
            nao_fornecedor    = 1
            fornecedor_conta  = 2
            fornecedor_banco  = 3
            faixa_valor       = 4
            banco_empresa     = 5
            OTHERS            = 6.

        IF sy-subrc IS INITIAL.
          obj_nfe->set_meio_de_pagamento( EXPORTING i_meio_pagamento = zde_nfe_dist_alv-pymt_meth ).
          obj_nfe->set_banco_empresa( EXPORTING i_banco_empresa = zde_nfe_dist_alv-housebankid ).
        ENDIF.
      ENDIF.

      IF zde_nfe_dist_alv-pymt_meth IS NOT INITIAL AND lt_texto_forma_de_pagmento IS INITIAL.
        SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
          FROM t042z
         WHERE land1 EQ zde_nfe_dist_alv-land1
           AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_MEIO_PAGAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_meio_pagamento INPUT.
  obj_nfe->set_meio_de_pagamento( EXPORTING i_meio_pagamento = zde_nfe_dist_alv-pymt_meth ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_BANCO_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_banco_empresa INPUT.
  obj_nfe->set_banco_empresa( EXPORTING i_banco_empresa = zde_nfe_dist_alv-housebankid ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ZBVTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_zbvtyp INPUT.
  ck_alterou_zbvtyp = abap_true.
ENDMODULE.

*-CS2024000243-05.06.2024-#136397-JT-inicio
MODULE alterou_ck_fpol INPUT.
  ck_alterou_ck_fpol = abap_true.
ENDMODULE.
*-CS2024000243-05.06.2024-#136397-JT-fim

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ZLSPR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_zlspr INPUT.
  ck_alterou_zlspr = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PYMT_METH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_pymt_meth INPUT.
  ck_alterou_pymt_meth = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_BOLETO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_boleto INPUT.
  ck_alterou_boleto = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_HOUSEBANKID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_housebankid INPUT.
  ck_alterou_housebankid = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VLR_DESCONTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_vlr_desconto INPUT.
  ck_alterou_vlr_desconto = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_OBS_FINANCEIRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_obs_financeira INPUT.
  ck_alterou_obs_financeira = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_CK_POSSUI_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_ck_possui_frete INPUT.
  obj_nfe->set_ck_possui_frete( i_ck_possui_frete = zde_nfe_dist_alv-ck_possui_frete ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_CD_DEPARTAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_cd_departamento INPUT.
  TRY .
      obj_nfe->set_departamento( i_cd_departamento = zde_nfe_dist_alv-cd_departamento ).

      IF obj_nfe->get_alterou_pedido_compra( ) EQ abap_true.
        PERFORM get_info_tela.
      ELSE.
        PERFORM get_info_valores.
      ENDIF.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_VALORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_valores .

  DATA: e_wkurs	      TYPE wkurs,
        e_valor_total	TYPE bapi_rmwwr.

  TRY .

      DATA(r_zmmt0075) = obj_nfe->get_config_tipo_pedido( ).

      obj_nfe->get_valor_nota_fiscal_fatura(
        IMPORTING
          e_waers       = zde_nfe_dist_alv-ctr_waers
          e_wkurs       = e_wkurs
          e_kufix       = zde_nfe_dist_alv-ctr_kufix
          e_sinal       = zde_nfe_dist_alv-ctr_sinal
          e_valor_total = e_valor_total
          e_zterm       = zde_nfe_dist_alv-ctr_zterm ).

      IF r_zmmt0075-ck_altera_valor EQ abap_false OR zde_nfe_dist_alv-ctr_wkurs IS INITIAL.
        zde_nfe_dist_alv-ctr_wkurs       = e_wkurs.
        zde_nfe_dist_alv-ctr_valor_total = e_valor_total.
        obj_nfe->set_ctr_valor_total( i_ctr_valor_total = zde_nfe_dist_alv-ctr_valor_total ).
      ENDIF.

      IF r_zmmt0075-zlspr IS NOT INITIAL AND zde_nfe_dist_alv-zlspr IS INITIAL.
        zde_nfe_dist_alv-zlspr = r_zmmt0075-zlspr.
        obj_nfe->set_bloqueio_pagamento( i_zlspr = r_zmmt0075-zlspr ).
      ENDIF.

      IF zde_nfe_dist_alv-zlspr IS NOT INITIAL.
        SELECT SINGLE textl INTO lt_texto_bloqueio_pagmento
          FROM t008t
         WHERE spras EQ sy-langu
           AND zahls EQ zde_nfe_dist_alv-zlspr.
      ENDIF.

      IF zde_nfe_dist_alv-pymt_meth IS NOT INITIAL.
        SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
          FROM t042z
         WHERE land1 EQ zde_nfe_dist_alv-land1
           AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
      ENDIF.
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0202 OUTPUT.

  IF container_0201 IS INITIAL.
    CLEAR wa_layout_0201.
    wa_layout_0201-zebra      = abap_true.
    wa_stable_0201-row        = abap_true.
    wa_stable_0201-col        = abap_true.
    wa_layout_0201-stylefname = 'STYLE'.
    wa_layout_0201-sel_mode   = 'A'.
    wa_layout_0201-info_fname = 'LINE_COLOR'.
    wa_layout_0201-ctab_fname = 'COLOR_CELL'.
    wa_layout_0201-s_dragdrop-row_ddid = g_handle_alv.

    CREATE OBJECT container_0201
      EXPORTING
        container_name = 'ALV_ITENS'.

    PERFORM fill_it_fieldcatalog_0201.

*   Fill info for layout variant
    PERFORM fill_gs_variant_0201.

    CREATE OBJECT cl_grid_0201
      EXPORTING
        i_parent = container_0201.

    CREATE OBJECT obg_toolbar_0201
      EXPORTING
        io_alv_grid = cl_grid_0201.

    SET HANDLER obg_toolbar_0201->on_toolbar FOR cl_grid_0201.
    SET HANDLER obg_toolbar_0201->handle_user_command FOR cl_grid_0201.

    cl_grid_0201->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_0201->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_0201.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_0201.

    wa_f4_0201-fieldname = 'EBELN'.
    wa_f4_0201-register  = 'X'.
    wa_f4_0201-getbefore = 'X'.
    INSERT wa_f4_0201 INTO TABLE it_f4_0201.
    wa_f4_0201-fieldname = 'EBELP'.
    INSERT wa_f4_0201 INTO TABLE it_f4_0201.
    wa_f4_0201-fieldname = 'MATNR'.
    INSERT wa_f4_0201 INTO TABLE it_f4_0201.

    CREATE OBJECT event_receiver_0201.
    SET HANDLER event_receiver_0201->handle_data_changed_0201 FOR cl_grid_0201.
    SET HANDLER event_receiver_0201->handle_hotspot_click_0201 FOR cl_grid_0201.
    SET HANDLER event_receiver_0201->handle_button_click_0201 FOR cl_grid_0201.
    SET HANDLER event_receiver_0201->handle_f4_0201 FOR cl_grid_0201.
    SET HANDLER event_receiver_0201->handle_ondrop_0201 FOR cl_grid_0201.
    SET HANDLER event_receiver_0201->handle_ondrag_0201 FOR cl_grid_0201.

    CALL METHOD cl_grid_0201->register_f4_for_fields EXPORTING it_f4 = it_f4_0201.

    CALL METHOD cl_grid_0201->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_0201
        i_save                        = 'A'
        is_layout                     = wa_layout_0201
        it_toolbar_excluding          = it_function_0201
      CHANGING
        it_outtab                     = it_itens_alv[]
        it_fieldcatalog               = it_fieldcat_0201
        it_sort                       = it_sort_0201
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL METHOD cl_grid_0201->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0201.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0203 INPUT.

  CASE ok_code.
    WHEN 'TB001'.
      sb_tela_0203 = tl_0204.
      tab_itens-activetab = 'TB001'.
      CLEAR: ok_code.
    WHEN 'TB002'.
      sb_tela_0203 = tl_0205.
      tab_itens-activetab = 'TB002'.
      CLEAR: ok_code.
    WHEN 'TB003'.
      sb_tela_0203 = tl_0207.
      tab_itens-activetab = 'TB003'.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0203  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0203 OUTPUT.

  IF sb_tela_0203 IS INITIAL.
    sb_tela_0203 = tl_0204.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0205  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0205 OUTPUT.

  DATA: wa_alv_itens      TYPE disvariant,
        wa_layout_itens   TYPE lvc_s_layo,
        wa_stbl_itens     TYPE lvc_s_stbl,
        it_fieldcat_itens TYPE lvc_t_fcat,
        is_stable  	      TYPE lvc_s_stbl,
        i_soft_refresh    TYPE char01.

  DATA: i_contador_2 TYPE lvc_colpos,
        wa_coluna    TYPE lvc_s_fcat.

  IF nfe_container_0205 IS INITIAL.

    CREATE OBJECT nfe_container_0205
      EXPORTING
        container_name = 'ALV_ITENS_NFE'.

    CREATE OBJECT ct_alv_itens_nfe
      EXPORTING
        i_parent = nfe_container_0205.

    "Variante
    wa_alv_itens-report      = sy-repid.
    wa_alv_itens-handle      = '0205'.
    wa_alv_itens-log_group   = abap_false.
    wa_alv_itens-username    = abap_false.
    wa_alv_itens-variant     = abap_false.
    wa_alv_itens-text        = abap_false.
    wa_alv_itens-dependvars  = abap_false.

    "LayOut
    wa_layout_itens-zebra      = abap_true.
    wa_layout_itens-stylefname = 'STYLE'.
    wa_layout_itens-sel_mode   = 'A'.
    wa_layout_itens-info_fname = 'LINE_COLOR'.
    wa_layout_itens-ctab_fname = 'COLOR_CELL'.
    "WA_LAYOUT_ITENS-S_DRAGDROP-ROW_DDID = G_HANDLE_ALV.

    "Controle VLA: estabilidade refresh
    wa_stbl_itens-row = abap_true.
    wa_stbl_itens-col = abap_true.

    "Catálogo de Itens de Nota Fiscal
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZDE_NFE_DIST_ITM_ALV'
      CHANGING
        ct_fieldcat      = it_fieldcat_itens.

    i_contador_2 = 14.

    LOOP AT it_fieldcat_itens ASSIGNING FIELD-SYMBOL(<fs_catalago_itens>).
      <fs_catalago_itens>-edit = abap_false.
      CASE <fs_catalago_itens>-fieldname.
        WHEN 'PROD_ITEM'.
          <fs_catalago_itens>-col_pos   = 01.
          <fs_catalago_itens>-outputlen = 03.
        WHEN 'PROD_CODIGO'.
          <fs_catalago_itens>-col_pos   = 02.
          <fs_catalago_itens>-outputlen = 08.
        WHEN 'PROD_DESCRICAO'.
          <fs_catalago_itens>-col_pos   = 03.
          <fs_catalago_itens>-outputlen = 18.
        WHEN 'PROD_NCM'.
          <fs_catalago_itens>-col_pos   = 04.
          <fs_catalago_itens>-outputlen = 10.
        WHEN 'PROD_UND_COMERCI'.
          <fs_catalago_itens>-col_pos   = 05.
          <fs_catalago_itens>-outputlen = 04.
        WHEN 'PROD_QTD_COMERCI'.
          <fs_catalago_itens>-col_pos   = 06.
          <fs_catalago_itens>-do_sum    = abap_true.
          <fs_catalago_itens>-outputlen = 10.
        WHEN 'PROD_VLR_UND_COM'.
          <fs_catalago_itens>-col_pos   = 07.
          <fs_catalago_itens>-outputlen = 08.
        WHEN 'PROD_VLR_TOTAL_B'.
          <fs_catalago_itens>-col_pos   = 08.
          <fs_catalago_itens>-do_sum    = abap_true.
          <fs_catalago_itens>-outputlen = 10.
        WHEN 'COF_CST'.
          <fs_catalago_itens>-scrtext_l = 'COFINS'.
          <fs_catalago_itens>-scrtext_m = 'CO'.
          <fs_catalago_itens>-scrtext_s = 'CO'.
          <fs_catalago_itens>-col_pos   = 09.
          <fs_catalago_itens>-outputlen = 02.
        WHEN 'PIS_CST'.
          <fs_catalago_itens>-scrtext_l = 'PIS'.
          <fs_catalago_itens>-scrtext_m = 'PI'.
          <fs_catalago_itens>-scrtext_s = 'PI'.
          <fs_catalago_itens>-col_pos   = 10.
          <fs_catalago_itens>-outputlen = 02.
        WHEN 'IPI_CST'.
          <fs_catalago_itens>-scrtext_l = 'IPI'.
          <fs_catalago_itens>-scrtext_m = 'IP'.
          <fs_catalago_itens>-scrtext_s = 'IP'.
          <fs_catalago_itens>-col_pos   = 11.
          <fs_catalago_itens>-outputlen = 02.
        WHEN 'ICMS_CST'.
          <fs_catalago_itens>-scrtext_l = 'ICMS'.
          <fs_catalago_itens>-scrtext_m = 'IC'.
          <fs_catalago_itens>-scrtext_s = 'IC'.
          <fs_catalago_itens>-col_pos   = 12.
          <fs_catalago_itens>-outputlen = 02.
        WHEN OTHERS.
          <fs_catalago_itens>-no_out  = abap_true.
          <fs_catalago_itens>-col_pos = i_contador_2.
          ADD 1 TO i_contador_2.
      ENDCASE.
    ENDLOOP.

    CALL METHOD ct_alv_itens_nfe->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_alv_itens
        i_save                        = 'A'
        is_layout                     = wa_layout_itens
      CHANGING
        it_outtab                     = it_itens_alv[]
        it_fieldcatalog               = it_fieldcat_itens
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  is_stable-col = abap_true.
  is_stable-row = abap_true.
  i_soft_refresh = abap_true.

  ct_alv_itens_nfe->refresh_table_display(
    EXPORTING
      is_stable      = is_stable
      i_soft_refresh = i_soft_refresh
  ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0204  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0204 OUTPUT.

  DATA: wa_alv_pedi      TYPE disvariant,
        wa_layout_pedi   TYPE lvc_s_layo,
        wa_stbl_pedi     TYPE lvc_s_stbl,
        it_fieldcat_pedi TYPE lvc_t_fcat,
        it_function_pedi TYPE ui_functions.

  IF nfe_container_0204 IS INITIAL.

    CREATE OBJECT nfe_container_0204
      EXPORTING
        container_name = 'ALV_ITENS_PEDIDOS'.

    CREATE OBJECT ct_alv_itens_pedi
      EXPORTING
        i_parent = nfe_container_0204.

    "Variante
    wa_alv_pedi-report      = sy-repid.
    wa_alv_pedi-handle      = '0204'.
    wa_alv_pedi-log_group   = abap_false.
    wa_alv_pedi-username    = abap_false.
    wa_alv_pedi-variant     = abap_false.
    wa_alv_pedi-text        = abap_false.
    wa_alv_pedi-dependvars  = abap_false.

    "LayOut
    wa_layout_pedi-zebra      = abap_true.
    wa_layout_pedi-stylefname = 'STYLE'.
    wa_layout_pedi-sel_mode   = 'A'.
    wa_layout_pedi-edit       = abap_false.
    wa_layout_pedi-info_fname = 'LINE_COLOR'.
    wa_layout_pedi-ctab_fname = 'COLOR_CELL'.
    wa_layout_pedi-s_dragdrop-cntr_ddid = g_handle_alv.

    "Controle VLA: estabilidade refresh
    wa_stbl_pedi-row = abap_true.
    wa_stbl_pedi-col = abap_true.

    "Catálogo de Itens de Nota Fiscal
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZDE_NFE_DIST_PED_ALV'
      CHANGING
        ct_fieldcat      = it_fieldcat_pedi.

    i_contador_2 = 2.

    DELETE it_fieldcat_pedi WHERE fieldname EQ 'CHAVE_NFE'.

    IF ck_entrada_por_cte = abap_false. "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA
      DATA(lc_edit_pedido) = obj_nfe->zif_cadastro~valida_atributo_alteravel( EXPORTING i_campo = 'EBELN' ).
    ENDIF.

    LOOP AT it_fieldcat_pedi ASSIGNING FIELD-SYMBOL(<fs_catalago_pedi>).
      <fs_catalago_pedi>-edit = abap_false.
      CASE <fs_catalago_pedi>-fieldname.
        WHEN 'ICO_FUNC_LOTE'.
          <fs_catalago_pedi>-style     = cl_gui_alv_grid=>mc_style_button.
          <fs_catalago_pedi>-scrtext_l = 'Lote'.
          <fs_catalago_pedi>-scrtext_m = 'Lote'.
          <fs_catalago_pedi>-scrtext_s = 'Lote'.
          <fs_catalago_pedi>-icon      = abap_true.
          <fs_catalago_pedi>-just      = 'C'.
          <fs_catalago_pedi>-col_pos   = 01.
          <fs_catalago_pedi>-outputlen = 03.
        WHEN 'VFDAT'.
          <fs_catalago_pedi>-edit    = lc_edit_pedido.
        WHEN 'GR_RCPT'.
          <fs_catalago_pedi>-edit    = lc_edit_pedido.
        WHEN 'UNLOAD_PT'.
          <fs_catalago_pedi>-edit    = lc_edit_pedido.
        WHEN 'ITEM_TEXT'.
          <fs_catalago_pedi>-edit    = lc_edit_pedido.
        WHEN 'HSDAT'.
          <fs_catalago_pedi>-edit    = lc_edit_pedido.
        WHEN 'MENGE'.
          <fs_catalago_pedi>-edit    = lc_edit_pedido.
          <fs_catalago_pedi>-col_pos = i_contador_2.
          ADD 1 TO i_contador_2.
        WHEN OTHERS.
          <fs_catalago_pedi>-col_pos = i_contador_2.
          ADD 1 TO i_contador_2.
      ENDCASE.
    ENDLOOP.

    ct_alv_itens_pedi->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    ct_alv_itens_pedi->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_pedi.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_pedi.

    CREATE OBJECT obg_toolbar_0204
      EXPORTING
        io_alv_grid = ct_alv_itens_pedi.

    SET HANDLER obg_toolbar_0204->on_toolbar FOR ct_alv_itens_pedi.
    SET HANDLER obg_toolbar_0204->handle_user_command FOR ct_alv_itens_pedi.

    CALL METHOD ct_alv_itens_pedi->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_alv_pedi
        i_save                        = 'A'
        is_layout                     = wa_layout_pedi
        it_toolbar_excluding          = it_function_pedi
      CHANGING
        it_outtab                     = it_pedi_itens_alv[]
        it_fieldcatalog               = it_fieldcat_pedi
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT event_receiver_itens_pedi.
    SET HANDLER event_receiver_itens_pedi->handle_data_changed FOR ct_alv_itens_pedi.
    SET HANDLER event_receiver_itens_pedi->handle_hotspot_click FOR ct_alv_itens_pedi.
    SET HANDLER event_receiver_itens_pedi->handle_button_click FOR ct_alv_itens_pedi.
    SET HANDLER event_receiver_itens_pedi->handle_f4 FOR ct_alv_itens_pedi.
    SET HANDLER event_receiver_itens_pedi->handle_ondrop FOR ct_alv_itens_pedi.
    SET HANDLER event_receiver_itens_pedi->handle_ondrag FOR ct_alv_itens_pedi.

  ENDIF.

  is_stable-col = abap_true.
  is_stable-row = abap_true.
  i_soft_refresh = abap_true.

  ct_alv_itens_pedi->refresh_table_display( EXPORTING is_stable = is_stable i_soft_refresh = i_soft_refresh ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ADD_PEDIDO_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_pedido_manual .

  DATA: e_pedido  TYPE ebeln,
        lc_pedido TYPE REF TO zcl_pedido_compra.

  DATA: c_reg TYPE lfa1-regio.

  IF sy-tcode = 'ZNFW0010'.
    SELECT SINGLE ebeln
      INTO e_pedido
      FROM zib_nfe_dist_ter
      WHERE chave_nfe = chaven.
  ELSE.
    CALL FUNCTION 'Z_MM_INFORMA_PEDIDO'
      IMPORTING
        e_pedido = e_pedido.
  ENDIF.

  CHECK e_pedido IS NOT INITIAL.

  CREATE OBJECT lc_pedido.

  lc_pedido->get_pedido_compra_chave_e(
    EXPORTING
      i_lifnr               = zib_nfe_dist_ter-p_emissor " Nº conta do fornecedor
      i_bukrs               = zib_nfe_dist_ter-e_tomadora    " Empresa
      i_werks               = zib_nfe_dist_ter-f_tomadora    " Centro
      i_ebeln               = e_pedido
      i_abrir_tela          = abap_false
    IMPORTING
      e_ekpo_t              = e_ekpo_t
    EXCEPTIONS
      nao_encontrado_pedido = 1
      OTHERS                = 2
  ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s010(zmmped) WITH e_pedido zib_nfe_dist_ter-p_emissor DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "DELETE E_EKPO_T WHERE WERKS NE ZIB_NFE_DIST_TER-F_TOMADORA.

*010  Pedido &1 não permitido para fornecedor &2!
*011  Pedido &1 não permitido para a filial &2!

  IF e_ekpo_t IS INITIAL.
    MESSAGE s011(zmmped) WITH e_pedido zib_nfe_dist_ter-f_tomadora DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


*  QTD1 TYPE CHAR3 VALUE '0',
*  e_ekpo_t

  "Add Pedido
  PERFORM pesquisa_pedidos USING abap_false e_ekpo_t.

ENDFORM.

FORM data_changed_pedi USING  rr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: l_menge	    TYPE j_1bnetqty,
        l_unload_pt TYPE zib_nfe_dist_ped-unload_pt,
        l_gr_rcpt   TYPE zib_nfe_dist_ped-gr_rcpt,
        l_item_text TYPE zib_nfe_dist_ped-item_text,
        l_total     TYPE zde_vlr_total,
        lv_value    TYPE lvc_value,
        i_pedido    TYPE zib_nfe_dist_ped.

  TRY .

      LOOP AT rr_data_changed->mt_good_cells INTO DATA(ls_mod_cells).

        READ TABLE it_pedi_itens_alv ASSIGNING FIELD-SYMBOL(<fs_pedi>) INDEX ls_mod_cells-row_id.

        CASE ls_mod_cells-fieldname.
          WHEN 'UNLOAD_PT'.
            CALL METHOD rr_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = ls_mod_cells-fieldname
              IMPORTING
                e_value     = l_unload_pt.
            <fs_pedi>-unload_pt = l_unload_pt.
            MOVE-CORRESPONDING <fs_pedi> TO i_pedido.
            obj_nfe->add_pedido_nota( i_pedido = i_pedido ).
          WHEN 'GR_RCPT'.
            CALL METHOD rr_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = ls_mod_cells-fieldname
              IMPORTING
                e_value     = l_gr_rcpt.
            <fs_pedi>-gr_rcpt = l_gr_rcpt.
            MOVE-CORRESPONDING <fs_pedi> TO i_pedido.
            obj_nfe->add_pedido_nota( i_pedido = i_pedido ).
          WHEN 'ITEM_TEXT'.
            CALL METHOD rr_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = ls_mod_cells-fieldname
              IMPORTING
                e_value     = l_item_text.
            <fs_pedi>-item_text = l_item_text.
            MOVE-CORRESPONDING <fs_pedi> TO i_pedido.
            obj_nfe->add_pedido_nota( i_pedido = i_pedido ).
          WHEN 'MENGE'.
            "ALRS
            CALL METHOD rr_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = ls_mod_cells-fieldname
              IMPORTING
                e_value     = l_menge.

            SELECT SINGLE *
              FROM ekpo
              INTO @DATA(wa_ekpo)
              WHERE ebeln = @<fs_pedi>-ebeln
              AND   ebelp = @<fs_pedi>-ebelp.
            "
*            l_total = l_menge * ( <fs_pedi>-netpr / wa_ekpo-peinh ).
            l_total = l_menge * ( <fs_pedi>-total / wa_ekpo-menge ).
            MOVE l_total TO lv_value.
            CALL METHOD rr_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = 'TOTAL'
                i_value     = lv_value.

            <fs_pedi>-menge = l_menge.
            <fs_pedi>-total = l_total.
            <fs_pedi>-navnw = l_menge * ( wa_ekpo-navnw / wa_ekpo-menge ).

            MOVE <fs_pedi>-navnw TO lv_value.
            CALL METHOD rr_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = 'NAVNW'
                i_value     = lv_value.
            "
            MOVE-CORRESPONDING <fs_pedi> TO i_pedido.
            obj_nfe->add_pedido_nota( i_pedido = i_pedido ).
        ENDCASE.
      ENDLOOP.
    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_doc_material  USING p_mblnr TYPE mblnr
                                 p_mjahr TYPE mjahr.

  IF p_mblnr IS NOT INITIAL.
    SET PARAMETER ID 'MBN' FIELD p_mblnr.
    SET PARAMETER ID 'MJA' FIELD p_mjahr.
    "CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
    CALL TRANSACTION 'MIGO_GO' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG2
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog2 .

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog2[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_NFE_DIST_LOG_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog2.

  LOOP AT it_fieldcatalog2 ASSIGNING <fs_cat>.
    CASE <fs_cat>-fieldname.
      WHEN 'MESSAGE'.
        <fs_cat>-outputlen = 50.
      WHEN 'MESSAGE_V1' OR 'MESSAGE_V2' OR 'MESSAGE_V3' OR 'MESSAGE_V4'.
        <fs_cat>-outputlen = 20.
      WHEN 'IC_MESSAGE'.
        <fs_cat>-just    = 'C'.
      WHEN 'IC_TEXTO'.
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-just    = 'C'.
      WHEN 'CK_ESTRATEGIA'.
        <fs_cat>-no_out  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG2

FORM fill_gs_variant2 .

  gs_variant2-report      = sy-repid.
  gs_variant2-handle      = '0002'.
  gs_variant2-log_group   = abap_false.
  gs_variant2-username    = abap_false.
  gs_variant2-variant     = abap_false.
  gs_variant2-text        = abap_false.
  gs_variant2-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT2

*&---------------------------------------------------------------------*
*&      Form  CHAMA_LINK_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM chama_link_click USING p_node_key TYPE tv_nodekey p_item_name TYPE tv_itmname.

  READ TABLE it_pedido_validado INTO DATA(wa_pedido_validado) WITH KEY node_key = p_node_key.
  IF sy-subrc IS INITIAL.
    PERFORM mostra_motivo USING wa_pedido_validado.
    "ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = WA_PEDIDO_VALIDADO-MOTIVO I_TITULO = 'Log de Erro do Item do Pedido' ).
  ENDIF.

ENDFORM.

FORM mostra_motivo USING p_pedido_item TYPE ty_pedido_validado.
  CONCATENATE 'Log de Erro do Item ' p_pedido_item-ebelp 'do Pedido' p_pedido_item-ebeln INTO DATA(lc_titulo) SEPARATED BY space.
  zcl_string=>show_texto( EXPORTING i_string = p_pedido_item-motivo i_titulo = lc_titulo ).
ENDFORM.


INCLUDE zmmr118_status_0206.

*&---------------------------------------------------------------------*
*&      Form  LOTE_ITEM_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lote_item_pedido .

  DATA: i_pedido TYPE zib_nfe_dist_ped.

  IF it_pedi_itens_sel[] IS INITIAL.
    MESSAGE s120.
    EXIT.
  ENDIF.

  READ TABLE it_pedi_itens_sel INTO lc_pedido_item_sel INDEX 1.
  IF sy-subrc IS INITIAL.

    SELECT SINGLE * INTO @DATA(wa_mara)
      FROM mara
     WHERE matnr EQ @lc_pedido_item_sel-matnr.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO @DATA(wa_ekpo)
        FROM ekpo
       WHERE ebeln EQ @lc_pedido_item_sel-ebeln
         AND ebelp EQ @lc_pedido_item_sel-ebelp.

    "Verificar se Material é Administrado por Lote mais Não tem Classe Definida """""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lc_ck_class) = abap_false.
    IF wa_mara-xchpf EQ abap_true.
      FIND REGEX 'C' IN wa_mara-pstat.
      IF sy-subrc IS INITIAL.
        lc_ck_class = abap_true.
      ENDIF.
    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF wa_mara-xchpf EQ abap_true AND lc_ck_class EQ abap_false.
      "Tem Controle Por Lote, Tem Classe de Custo e Sem Característica

      "Categoria de classificação contábil
      IF wa_ekpo-knttp IS INITIAL.
        DATA(obriga_lote) = abap_true.
      ELSE.
        obriga_lote = abap_false.
      ENDIF.

      PERFORM altera_campos_pedido_lote USING lc_pedido_item_sel obriga_lote.
      IF ck_informado EQ abap_true.
        ck_informado = abap_false.
        PERFORM get_info_tela.
        LEAVE TO SCREEN 0200.
      ENDIF.

*    ELSEIF WA_MARA-XCHPF EQ ABAP_TRUE AND WA_EKPO-KNTTP IS NOT INITIAL AND LC_CK_CLASS EQ ABAP_TRUE.
*      "Tem Controle Por Lote, Tem Classe de Custo e Com Característica
*
*      PERFORM ALTERA_CAMPOS_PEDIDO_LOTE USING LC_PEDIDO_ITEM_SEL LC_CK_CLASS.
*      IF CK_INFORMADO EQ ABAP_TRUE.
*        CK_INFORMADO = ABAP_FALSE.
*        PERFORM GET_INFO_TELA.
*        LEAVE TO SCREEN 0200.
*      ENDIF.

    ELSEIF wa_mara-xchpf EQ abap_true.
      "Controla Por Lote

      TRY.
          PERFORM get_info_tela.

          READ TABLE it_lotes_alv_t
            WITH KEY chave_nfe = lc_pedido_item_sel-chave_nfe ebeln = lc_pedido_item_sel-ebeln ebelp = lc_pedido_item_sel-ebelp
            INTO DATA(wa_lotes_alv).

          DATA(lc_sucesso) = abap_false.

          IF sy-subrc IS INITIAL.
            PERFORM selecionar_lote_ped USING lc_pedido_item_sel-ebeln lc_pedido_item_sel-ebelp wa_lotes_alv-cd_lote_item abap_true.
            lc_sucesso = abap_true.
          ELSE.
            PERFORM incluir_lote_ped CHANGING lc_sucesso.
          ENDIF.

          IF lc_sucesso EQ abap_true.
            PERFORM chamar_tela_lote USING lc_pedido_item_sel.
            LEAVE TO SCREEN 0200.
          ENDIF.

        CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
          ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_TELA_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EXCLUIR_PEDI  text
*----------------------------------------------------------------------*
FORM chamar_tela_lote  USING p_pedi TYPE ty_pedi_itens_alv.

  "IT_LOTES_ALV_T
  "IT_CARAC_ALV
  lc_pedido_item_sel = p_pedi.
  CALL SCREEN 0206 STARTING AT 07 30.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  INCLUIR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_lote_ped CHANGING lc_sucesso TYPE char01.

  DATA: e_caracteristicas	TYPE zib_nfe_dist_lca_t,
        wa_carac_alv      TYPE ty_itens_carac_alv,
        wa_lotes_alv      TYPE ty_itens_lotes_alv,
        p_index           TYPE lvc_index.

  CLEAR: lc_sucesso.

  IF lc_pedido_item_sel IS INITIAL.
    MESSAGE s083.
    EXIT.
  ENDIF.

  CHECK zib_nfe_dist_ter-st_fisico EQ zcl_nfe_inbound=>st_fisico_00.

  TRY.
      DATA(r_lote) = obj_nfe->add_lote_pedido_item( EXPORTING i_ebeln           = lc_pedido_item_sel-ebeln
                                                              i_ebelp           = lc_pedido_item_sel-ebelp
                                                    IMPORTING e_caracteristicas = e_caracteristicas ).
      "Alimenta Tabelas com dados base
      CLEAR: wa_lotes_alv.
      MOVE-CORRESPONDING r_lote TO wa_lotes_alv.
      APPEND wa_lotes_alv TO it_lotes_alv_t.

      LOOP AT e_caracteristicas INTO DATA(wa_caracteristicas) WHERE cd_lote_item EQ r_lote-cd_lote_item.
        CLEAR: wa_carac_alv.
        MOVE-CORRESPONDING wa_caracteristicas TO wa_carac_alv.
        APPEND wa_carac_alv TO it_carac_alv.
      ENDLOOP.

      READ TABLE it_lotes_alv_t WITH KEY cd_lote_item = wa_lotes_alv-cd_lote_item.
      MOVE sy-tabix TO p_index.

      PERFORM selecionar_lote_ped USING lc_pedido_item_sel-ebeln lc_pedido_item_sel-ebelp r_lote-cd_lote_item abap_true.

      lc_sucesso = abap_true.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
    CATCH zcx_charg_exception INTO ex_charg.
      ex_charg->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.

  SET PF-STATUS 'PF9002'.
  SET TITLEBAR 'TL9002'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0206  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0206 INPUT.


  CASE ok_code.
    WHEN 'RETORNAR'.
      CLEAR: ok_code.
      PERFORM limpar_tela_lote_0206.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM busca_revisoes.

  CALL FUNCTION 'ZNFE_INBOUND_REVISAO'
    EXPORTING
      i_user = sy-uname
    IMPORTING
      i_html = html_revisao
      i_url  = url_revisao.

ENDFORM.

FORM ocultar_log .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code.
    WHEN 'NFE'.
      CLEAR: ok_code.
      CALL SCREEN 0100 STARTING AT 25 06.
    WHEN 'CTE'.
      CLEAR: ok_code.
      CALL SCREEN 0101 STARTING AT 25 06.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REVISOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_revisoes .

  DATA: i_url  TYPE c LENGTH 200,
        i_url2 TYPE c LENGTH 200.

  IF ctl_cccontainer_revisao IS INITIAL.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = ctl_cccontainer_revisao.

    CREATE OBJECT html_control
      EXPORTING
        parent = ctl_cccontainer_revisao.

    DATA: data_table TYPE STANDARD TABLE OF text255.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = html_revisao
        i_tabline_length = 255
      TABLES
        et_table         = data_table.

    html_control->load_data(
      EXPORTING
        url                    = i_url
      IMPORTING
        assigned_url           = i_url2
      CHANGING
        data_table             = data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).

    myevent-eventid = html_control->m_id_sapevent.
    myevent-appl_event = abap_true.
    APPEND myevent TO myevent_tab.

    CALL METHOD html_control->set_registered_events
      EXPORTING
        events = myevent_tab.

    CREATE OBJECT evt_receiver.

    SET HANDLER evt_receiver->on_sapevent FOR html_control.

    html_control->show_url(
      EXPORTING
        url                    = i_url2
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5
    ).

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.

FORM handle_hotspot_click_log
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: i_cd_aprovacao  TYPE zde_est_aprovacao.

  READ TABLE it_log_alv INDEX row_id INTO wa_log_alv.

  CASE fieldname.
    WHEN 'IC_TEXTO'.
      IF wa_log_alv-ck_estrategia EQ abap_true.
        CALL FUNCTION 'ZNFE_SHOW_INFO_MOTIVO_ESTRATEG'
          EXPORTING
            i_cd_aprovacao = wa_log_alv-cd_aprovacao.
      ENDIF.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK


*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tela_0001 .

  CLEAR: evt_receiver.

  IF html_control IS NOT INITIAL.
    html_control->free( ).
  ENDIF.
  CLEAR: html_control.

  IF ctl_cccontainer_revisao IS NOT INITIAL.
    ctl_cccontainer_revisao->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_revisao.

  IF html_control_html1 IS NOT INITIAL.
    html_control_html1->free( ).
  ENDIF.
  CLEAR: html_control_html1.

  IF ctl_cccontainer_html1 IS NOT INITIAL.
    ctl_cccontainer_html1->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_html1.

  IF splitter IS NOT INITIAL.
    splitter->free( ).
  ENDIF.
  CLEAR: splitter.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALTERA_CAMPOS_PEDIDO_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LC_PEDIDO_ITEM_SEL  text
*----------------------------------------------------------------------*
FORM altera_campos_pedido_lote  USING  p_item TYPE ty_pedi_itens_alv
                                       p_informa_lote TYPE char01.

  TRY .

      READ TABLE it_pedi_itens_alv ASSIGNING FIELD-SYMBOL(<fs_pedi>)
      WITH KEY ebeln = p_item-ebeln
               ebelp = p_item-ebelp.

      CHECK sy-subrc IS INITIAL.

      CLEAR: zib_nfe_dist_ped.
      MOVE-CORRESPONDING <fs_pedi> TO zib_nfe_dist_ped.

      ck_informado = abap_false.
      ck_informa_lote = p_informa_lote.

      CALL SCREEN 9003 STARTING AT 30 04.

      IF ck_informado EQ abap_true.
        obj_nfe->add_pedido_nota( i_pedido = zib_nfe_dist_ped ).
      ENDIF.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003_exit INPUT.

  CLEAR: ck_informado.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.
      CLEAR: ok_code.
      ck_informado = abap_true.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9003 OUTPUT.

  SELECT SINGLE * INTO @DATA(wa_makt)
    FROM makt
   WHERE spras EQ @sy-langu
     AND matnr EQ @zib_nfe_dist_ped-matnr.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9003' WITH zib_nfe_dist_ped-ebeln zib_nfe_dist_ped-ebelp wa_makt-maktx.

  IF ck_informa_lote EQ abap_false.
    LOOP AT SCREEN.
      SPLIT screen-name AT '-' INTO: DATA(str1) DATA(str2).
      IF str1 EQ 'ZIB_NFE_DIST_PED' AND str2 EQ 'CHARG'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERAR_DATA_VENCIMENTO_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_data_vencimento_lote INPUT.

  IF zib_nfe_dist_ped-vfdat LT sy-datlo.
    MESSAGE e141.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERAR_DATA_FABRICACAO_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterar_data_fabricacao_lote INPUT.

  IF zib_nfe_dist_ped-hsdat GT sy-datlo.
    MESSAGE e142.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  VALIDAEXCECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_bloqueio_excecao_user  CHANGING is_valid.


  SELECT *
     INTO TABLE @DATA(it_zmmt0111)
  FROM zmmt0111 AS zt
  WHERE  zt~dt_bloqueio >= @sy-datum.

  IF sy-subrc  IS INITIAL.

    LOOP AT it_zmmt0111 INTO DATA(wa_zmmt0111).

      "Verifica o Mês das duas datas
*      IF  WA_ZMMT0111-DT_BLOQUEIO+4(2) < SY-DATUM+4(2).
      IF  wa_zmmt0111-dt_bloqueio < sy-datum.

        IF wa_zmmt0111-usuar_lib IS INITIAL OR  wa_zmmt0111-usuar_lib <> sy-uname.

          MESSAGE 'Período Bloqueado para Lançamento qualquer dúvida enviar e-mail para csc.fiscal' TYPE 'I'.
          is_valid = 0.

        ENDIF.
        IF wa_zmmt0111-usuar_lib = sy-uname .
          is_valid = 1.
          EXIT.
        ENDIF.

      ELSEIF wa_zmmt0111-dt_bloqueio = sy-datum. "" AND wa_zmmt0111-hr_bloqueio < sy-uzeit. "CS2020000457 BUG

        IF wa_zmmt0111-usuar_lib IS INITIAL OR  wa_zmmt0111-usuar_lib <> sy-uname.

          MESSAGE 'Período Bloqueado para Lançamento qualquer dúvida enviar e-mail para csc.fiscal' TYPE 'I'.
          is_valid = 0.

        ENDIF.
        IF wa_zmmt0111-usuar_lib = sy-uname .
          is_valid = 1.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.

INCLUDE zmmr118_status_log.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FORMA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_forma INPUT.

  DATA: tl_return_form TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselct      TYPE TABLE OF dselc      WITH HEADER LINE.


  DATA: BEGIN OF tl_zlsch OCCURS 0,
          zlsch TYPE t042z-zlsch,
          text1 TYPE t042z-text1,
        END OF tl_zlsch.

  REFRESH tl_zlsch.
  tl_zlsch-zlsch = 'E'.
  tl_zlsch-text1 = 'Boleto'.
  APPEND tl_zlsch.
  "
  tl_zlsch-zlsch = 'S'.
  tl_zlsch-text1 = 'TED STR'.
  APPEND tl_zlsch.
  "
  tl_zlsch-zlsch = 'U'.
  tl_zlsch-text1 = 'Transferência Bancária'.
  APPEND tl_zlsch.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZLSCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZDE_NFE_DIST_ALV-PYMT_METH'
      value_org       = 'S'
    TABLES
      value_tab       = tl_zlsch
      return_tab      = tl_return_form
      dynpfld_mapping = tl_dselct.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0101'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101_exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  DATA(qt_ct1) = strlen( lc_chave_cte_texto ).
  CONDENSE lc_chave_cte_texto NO-GAPS.
  DATA(qt_ct2) = strlen( lc_chave_cte_texto ).



  CHECK qt_ct1 EQ qt_ct2.

  zib_cte_dist_ter-cd_chave_cte = lc_chave_cte_texto.

  "Valida CT-e.
  SELECT SINGLE * INTO @DATA(wa_nota_cte) FROM zib_cte_dist_ter WHERE cd_chave_cte EQ @zib_cte_dist_ter-cd_chave_cte AND modelo = '57' OR modelo = '67'.
  IF wa_nota_cte IS INITIAL.
    MESSAGE s168 WITH zib_cte_dist_ter-cd_chave_cte DISPLAY LIKE 'E'.
    CLEAR: wa_nota_cte.
    EXIT.
  ENDIF.

  "Valida CT-e.
*  SELECT SINGLE * INTO @DATA(wa_zib_nfe_forn) FROM zib_nfe_forn WHERE nu_chave EQ @zib_cte_dist_ter-cd_chave_cte AND nu_chave_modelo = '67'.
*  IF wa_zib_nfe_forn IS INITIAL.
*    MESSAGE s168 WITH zib_cte_dist_ter-cd_chave_cte DISPLAY LIKE 'E'.
*    CLEAR: wa_zib_nfe_forn.
*    EXIT.
*  ENDIF.


*  CLEAR: wa_nota.
*  SELECT SINGLE * INTO @DATA(wa_nota_itm) FROM zib_nfe_dist_itm WHERE chave_nfe EQ @zib_nfe_dist_ter-chave_nfe.
*  IF wa_nota_itm IS NOT INITIAL.
*
*    SELECT SINGLE *
*    FROM setleaf
*    INTO @DATA(i_data)
*      WHERE setname EQ 'MAGI_ZMM0116_CFOP'
*        AND valfrom EQ @wa_nota_itm-prod_cfop.
*
*    IF i_data IS NOT INITIAL.
*      MESSAGE s161 WITH zib_nfe_dist_ter-chave_nfe DISPLAY LIKE 'E'.
*      CLEAR: wa_nota_itm, i_data.
*      EXIT.
*    ENDIF.
*
*  ENDIF.
*  CLEAR: wa_nota_itm, i_data.

  SUBMIT zmmr118 WITH ch_cte EQ zib_cte_dist_ter-cd_chave_cte AND RETURN.
  CLEAR: zib_cte_dist_ter-cd_chave_cte.




*  CREATE OBJECT OBJ_NFE.
*  OBJ_NFE->NFE_INBOUND( ).
*
*  TRY.
*      OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = ZIB_NFE_DIST_TER-CHAVE_NFE ).
*      OBJ_NFE->AT_NFE_INBOUND->SET_INFO_SAP( ).
*      OBJ_NFE->AT_NFE_INBOUND->FREE( ).
*      OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = ZIB_NFE_DIST_TER-CHAVE_NFE ).
*      OBJ_NFE->AT_NFE_INBOUND->SET_COLETA_TUDO( I_CK_COLETA_TUDO = ABAP_TRUE ).
*      PERFORM GET_INFO_TELA.
*      CALL SCREEN 0200.
*    CATCH ZCX_CADASTRO INTO EX_CADASTRO.
*      EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*  CLEAR: ZIB_NFE_DIST_TER.
*  IF OBJ_NFE IS NOT INITIAL.
*    OBJ_NFE->FREE( ).
*    CLEAR: OBJ_NFE.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0207 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0207 OUTPUT.

  "DEVK9A1XPD MM-050624-CS2024000243-Ajustes ZMM0116 #83815 RSA

  DATA: wa_alv_cte         TYPE disvariant,
        wa_layout_cte      TYPE lvc_s_layo,
        wa_stbl_cte        TYPE lvc_s_stbl,
        it_fieldcat_cte    TYPE lvc_t_fcat,
        is_stable_cte  	   TYPE lvc_s_stbl,
        i_soft_refresh_cte TYPE char01.

  DATA: i_contador_cte_2 TYPE lvc_colpos,
        wa_coluna_cte    TYPE lvc_s_fcat.

  IF nfe_container_0207 IS INITIAL.

    CREATE OBJECT nfe_container_0207
      EXPORTING
        container_name = 'ALV_DADOS_CTE'.

    CREATE OBJECT ct_alv_cte
      EXPORTING
        i_parent = nfe_container_0207.

    "Variante
    wa_alv_cte-report      = sy-repid.
    wa_alv_cte-handle      = '0207'.
    wa_alv_cte-log_group   = abap_false.
    wa_alv_cte-username    = abap_false.
    wa_alv_cte-variant     = abap_false.
    wa_alv_cte-text        = abap_false.
    wa_alv_cte-dependvars  = abap_false.

    "LayOut
    wa_layout_cte-zebra      = abap_true.
    wa_layout_cte-stylefname = 'STYLE'.
    wa_layout_cte-sel_mode   = 'A'.
    wa_layout_cte-info_fname = 'LINE_COLOR'.
    wa_layout_cte-ctab_fname = 'COLOR_CELL'.
    wa_layout_cte-cwidth_opt = abap_true.
    "WA_LAYOUT_ITENS-S_DRAGDROP-ROW_DDID = G_HANDLE_ALV.

    "Controle VLA: estabilidade refresh
    wa_stbl_cte-row = abap_true.
    wa_stbl_cte-col = abap_true.

    "Catálogo de Itens de Nota Fiscal
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZDE_CTE_DIST_T_ALV'
      CHANGING
        ct_fieldcat      = it_fieldcat_cte.

    i_contador_2 = 14.

*    LOOP AT it_fieldcat_itens ASSIGNING FIELD-SYMBOL(<fs_catalago_itens>).
*      <fs_catalago_itens>-edit = abap_false.
*      CASE <fs_catalago_itens>-fieldname.
*        WHEN 'PROD_ITEM'.
*          <fs_catalago_itens>-col_pos   = 01.
*          <fs_catalago_itens>-outputlen = 03.
*        WHEN 'PROD_CODIGO'.
*          <fs_catalago_itens>-col_pos   = 02.
*          <fs_catalago_itens>-outputlen = 08.
*        WHEN 'PROD_DESCRICAO'.
*          <fs_catalago_itens>-col_pos   = 03.
*          <fs_catalago_itens>-outputlen = 18.
*        WHEN 'PROD_NCM'.
*          <fs_catalago_itens>-col_pos   = 04.
*          <fs_catalago_itens>-outputlen = 10.
*        WHEN 'PROD_UND_COMERCI'.
*          <fs_catalago_itens>-col_pos   = 05.
*          <fs_catalago_itens>-outputlen = 04.
*        WHEN 'PROD_QTD_COMERCI'.
*          <fs_catalago_itens>-col_pos   = 06.
*          <fs_catalago_itens>-do_sum    = abap_true.
*          <fs_catalago_itens>-outputlen = 10.
*        WHEN 'PROD_VLR_UND_COM'.
*          <fs_catalago_itens>-col_pos   = 07.
*          <fs_catalago_itens>-outputlen = 08.
*        WHEN 'PROD_VLR_TOTAL_B'.
*          <fs_catalago_itens>-col_pos   = 08.
*          <fs_catalago_itens>-do_sum    = abap_true.
*          <fs_catalago_itens>-outputlen = 10.
*        WHEN 'COF_CST'.
*          <fs_catalago_itens>-scrtext_l = 'COFINS'.
*          <fs_catalago_itens>-scrtext_m = 'CO'.
*          <fs_catalago_itens>-scrtext_s = 'CO'.
*          <fs_catalago_itens>-col_pos   = 09.
*          <fs_catalago_itens>-outputlen = 02.
*        WHEN 'PIS_CST'.
*          <fs_catalago_itens>-scrtext_l = 'PIS'.
*          <fs_catalago_itens>-scrtext_m = 'PI'.
*          <fs_catalago_itens>-scrtext_s = 'PI'.
*          <fs_catalago_itens>-col_pos   = 10.
*          <fs_catalago_itens>-outputlen = 02.
*        WHEN 'IPI_CST'.
*          <fs_catalago_itens>-scrtext_l = 'IPI'.
*          <fs_catalago_itens>-scrtext_m = 'IP'.
*          <fs_catalago_itens>-scrtext_s = 'IP'.
*          <fs_catalago_itens>-col_pos   = 11.
*          <fs_catalago_itens>-outputlen = 02.
*        WHEN 'ICMS_CST'.
*          <fs_catalago_itens>-scrtext_l = 'ICMS'.
*          <fs_catalago_itens>-scrtext_m = 'IC'.
*          <fs_catalago_itens>-scrtext_s = 'IC'.
*          <fs_catalago_itens>-col_pos   = 12.
*          <fs_catalago_itens>-outputlen = 02.
*        WHEN OTHERS.
*          <fs_catalago_itens>-no_out  = abap_true.
*          <fs_catalago_itens>-col_pos = i_contador_2.
*          ADD 1 TO i_contador_2.
*      ENDCASE.
*    ENDLOOP.

    CALL METHOD ct_alv_cte->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_alv_cte
        i_save                        = 'A'
        is_layout                     = wa_layout_cte
      CHANGING
        it_outtab                     = it_dados_cte_alv[]
        it_fieldcatalog               = it_fieldcat_cte
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  is_stable_cte-col  = abap_true.
  is_stable_cte-row  = abap_true.
  i_soft_refresh_cte = abap_true.

  ct_alv_cte->refresh_table_display(
    EXPORTING
      is_stable      = is_stable_cte
      i_soft_refresh = i_soft_refresh_cte
  ).

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form get_info_tela_cte
*&---------------------------------------------------------------------*
*FORM get_info_tela_cte .
*
*  CLEAR: it_itens, it_lotes, it_lotes_c, it_itens_alv[], it_itens_alv, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[],
*         it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv[], it_pedi_itens_alv, it_pedi_itens_alv[].
*
*  DATA(nfe_inbound) = obj_cte->get_info_nota( ).
*  MOVE-CORRESPONDING nfe_inbound-cte_base TO zib_cte_dist_ter.
*  MOVE-CORRESPONDING nfe_inbound-cte_alv  TO wa_cte_inbound_alv.
*  MOVE-CORRESPONDING nfe_inbound-nfe_alv  TO wa_cte_dist_ter_alv.
*  MOVE nfe_inbound-cte_base-itens   TO it_itens.
*  MOVE nfe_inbound-cte_base-pedidos TO it_pedidos.
*  MOVE nfe_inbound-cte_base-lotes   TO it_lotes.
*  MOVE nfe_inbound-cte_base-lotes_c TO it_lotes_c.
*
*  MOVE-CORRESPONDING zib_cte_dist_ter TO it_dados_cte_alv.
*  APPEND it_dados_cte_alv.
*
**  LOOP AT it_itens INTO DATA(wa_itens).
**
*** SELECT * FROM ekkn
***              INTO wa_ekkn
***              WHERE ebeln = wa_itens-ebeln
***               AND ebelp = wa_itens-ebelp.
***    ENDSELECT.
***
***    IF sy-subrc = 0.
***      wa_itens-unload_pt    = wa_ekkn-wempf.
***      wa_itens-gr_rcpt    = wa_ekkn-ablad.
***    ENDIF.
**
**    MOVE-CORRESPONDING wa_itens TO it_itens_alv.
**    it_itens_alv-ico_func_lote = icon_batch.
**    APPEND it_itens_alv.
**  ENDLOOP.
*
*  LOOP AT nfe_inbound-nfe_pedidos_alv INTO DATA(wa_pedidos).
*
*    SELECT * FROM ekkn
*              INTO wa_ekkn
*              WHERE ebeln = wa_pedidos-ebeln
*               AND ebelp = wa_pedidos-ebelp.
*    ENDSELECT.
*
*    IF sy-subrc = 0.
*      wa_pedidos-unload_pt    = wa_ekkn-wempf.
*      wa_pedidos-gr_rcpt    = wa_ekkn-ablad.
*    ENDIF.
*
*    MOVE-CORRESPONDING wa_pedidos TO it_pedi_itens_alv.
*    it_pedi_itens_alv-ico_func_lote = icon_batch.
*
*    APPEND it_pedi_itens_alv.
*  ENDLOOP.
*
*  LOOP AT it_lotes INTO DATA(wa_lotes).
*    MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
*    APPEND it_lotes_alv_t.
*  ENDLOOP.
*
*  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
*    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
*    APPEND it_carac_alv.
*  ENDLOOP.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_info_valores_cte
*&---------------------------------------------------------------------*
*FORM get_info_valores_cte .
*
*  DATA: e_wkurs        TYPE wkurs,
*        e_valor_total  TYPE bapi_rmwwr.
*
*  TRY .
*
*      DATA(r_zmmt0075) = obj_cte->get_config_tipo_pedido( ).
*
*      obj_cte->get_valor_nota_fiscal_fatura(
*        IMPORTING
*          e_waers       = zde_nfe_dist_alv-ctr_waers
*          e_wkurs       = e_wkurs
*          e_kufix       = zde_nfe_dist_alv-ctr_kufix
*          e_sinal       = zde_nfe_dist_alv-ctr_sinal
*          e_valor_total = e_valor_total
*          e_zterm       = zde_nfe_dist_alv-ctr_zterm ).
*
*      IF r_zmmt0075-ck_altera_valor EQ abap_false OR zde_nfe_dist_alv-ctr_wkurs IS INITIAL.
*        zde_nfe_dist_alv-ctr_wkurs       = e_wkurs.
*        zde_nfe_dist_alv-ctr_valor_total = e_valor_total.
*        obj_nfe->set_ctr_valor_total( i_ctr_valor_total = zde_nfe_dist_alv-ctr_valor_total ).
*      ENDIF.
*
*      IF r_zmmt0075-zlspr IS NOT INITIAL AND zde_nfe_dist_alv-zlspr IS INITIAL.
*        zde_nfe_dist_alv-zlspr = r_zmmt0075-zlspr.
*        obj_nfe->set_bloqueio_pagamento( i_zlspr = r_zmmt0075-zlspr ).
*      ENDIF.
*
*      IF zde_nfe_dist_alv-zlspr IS NOT INITIAL.
*        SELECT SINGLE textl INTO lt_texto_bloqueio_pagmento
*          FROM t008t
*         WHERE spras EQ sy-langu
*           AND zahls EQ zde_nfe_dist_alv-zlspr.
*      ENDIF.
*
*      IF zde_nfe_dist_alv-pymt_meth IS NOT INITIAL.
*        SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
*          FROM t042z
*         WHERE land1 EQ zde_nfe_dist_alv-land1
*           AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
*      ENDIF.
*    CATCH zcx_cadastro INTO ex_cadastro.
*      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*  ENDTRY.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_info_banco_parceiro_cte
*&---------------------------------------------------------------------*
FORM get_info_banco_parceiro_cte .

  DATA: lc_lfbk  TYPE lfbk,
        lc_bnka  TYPE bnka,
        lc_cte   TYPE zib_cte_dist_ter,
        lc_valor TYPE netwr_fp.

  "Tipo de banco do parceiro
  IF zde_nfe_dist_alv-zbvtyp IS NOT INITIAL.

    lc_cte-p_emissor = zde_nfe_dist_alv-p_emissor.
    lc_cte-zbvtyp    = zde_nfe_dist_alv-zbvtyp.

    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = lc_lfbk
        e_bnka     = lc_bnka
      CHANGING
        p_cte      = lc_cte
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      lc_info_forne-bankl = lc_bnka-bankl(3).
      lc_info_forne-banka = lc_bnka-banka.
      lc_info_forne-bankn = lc_lfbk-bankn.

      IF NOT lc_lfbk-bkont IS INITIAL.
        CONCATENATE lc_lfbk-bankl+4(11) '-' lc_lfbk-bkont INTO lc_info_forne-agenc.
      ELSE.
        lc_info_forne-agenc = lc_lfbk-bankl+4(11).
      ENDIF.

      CLEAR: lt_texto_forma_de_pagmento.

      IF zde_nfe_dist_alv-pymt_meth IS INITIAL AND zde_nfe_dist_alv-housebankid IS INITIAL.
        MOVE zde_nfe_dist_alv-ctr_valor_total TO lc_valor.

        CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
          EXPORTING
            p_bukrs           = zde_nfe_dist_alv-e_tomadora
            p_lifnr           = zde_nfe_dist_alv-p_emissor
            p_zlsch           = zde_nfe_dist_alv-pymt_meth
            p_valor           = lc_valor
            p_bvtyp           = zde_nfe_dist_alv-zbvtyp
            p_waers           = zde_nfe_dist_alv-ctr_waers
          IMPORTING
            p_forma_pagamento = zde_nfe_dist_alv-pymt_meth
            p_princ_bnc_emp   = zde_nfe_dist_alv-housebankid
          EXCEPTIONS
            nao_fornecedor    = 1
            fornecedor_conta  = 2
            fornecedor_banco  = 3
            faixa_valor       = 4
            banco_empresa     = 5
            OTHERS            = 6.

        IF sy-subrc IS INITIAL.
          obj_nfe->set_meio_de_pagamento( EXPORTING i_meio_pagamento = zde_nfe_dist_alv-pymt_meth ).
          obj_nfe->set_banco_empresa( EXPORTING i_banco_empresa = zde_nfe_dist_alv-housebankid ).
        ENDIF.
      ENDIF.

      IF zde_nfe_dist_alv-pymt_meth IS NOT INITIAL AND lt_texto_forma_de_pagmento IS INITIAL.
        SELECT SINGLE text1 INTO lt_texto_forma_de_pagmento
          FROM t042z
         WHERE land1 EQ zde_nfe_dist_alv-land1
           AND zlsch EQ zde_nfe_dist_alv-pymt_meth.
      ENDIF.

    ENDIF.
  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&  Include           ZMMR153_0001
*&---------------------------------------------------------------------*

TABLES: zde_zsdt0001cg_alv,
        zde_zsdt0001od_alv,
        zde_zsdt0001nt_alv,
        zde_zsdt0001ov_alv,
        zde_zsdt0001acb_alv,
        zde_zsdt0001tk_alv_vinc,
        zlest0185.

CONSTANTS:
  BEGIN OF c_tree_0100,
    column1 TYPE tv_itmname VALUE 'USUARIO',
    column2 TYPE tv_itmname VALUE 'DATA',
    column3 TYPE tv_itmname VALUE 'HORA',
  END OF c_tree_0100.

CONSTANTS: cs_line_color_finalizado  TYPE c LENGTH 4 VALUE 'C500',
           cs_line_color_alterado    TYPE c LENGTH 4 VALUE 'C300',
           cs_line_color_selecionada TYPE c LENGTH 4 VALUE 'C601'.

TYPES: ty_valor TYPE p LENGTH 16 DECIMALS 2.

TYPES: BEGIN OF ty_node_info_logs.
TYPES: node_key    TYPE tv_nodekey,
       item_name   TYPE tv_itmname,
       registro    TYPE zde_log_registro,
       dt_registro TYPE char10,
       hr_registro TYPE char08,
       us_registro TYPE char30.
TYPES: END OF ty_node_info_logs.

TYPES: BEGIN OF ty_add_nfe.
TYPES: n55_chave_acesso	TYPE zde_chave_doc_e,
       docnum_nfe       TYPE j_1bdocnum,
       n55_stat_sefaz	  TYPE j_1bstatuscode,
       dt_emissao       TYPE zde_zsdt0001nt_alv-dt_emissao,
       numero           TYPE zde_zsdt0001nt_alv-nr_nota,
       serie            TYPE zde_zsdt0001nt_alv-nm_serie,
       bukrs            TYPE bukrs,
       branch	          TYPE j_1bbranc_,
       parid            TYPE j_1bparid, "Ajuda de Pesquisa DEBI_KRED
       parid_ie         TYPE zde_ie,
       butxt            TYPE butxt,
       name	            TYPE name1,
       name1            TYPE name1_gp,
       nftot            TYPE j_1bnftot,
       ntgew            TYPE ntgew_15,
       ck_incluir       TYPE char01,
       cfop             TYPE zde_zsdt0001nt_alv-cfop,
       nr_fardo         TYPE zde_nm_fardos,
       nm_pesol         TYPE zde_nm_pesol,
       nm_pesob         TYPE zde_nm_pesob.
TYPES: END OF ty_add_nfe.

TYPES: BEGIN OF ty_itens_alv.
         INCLUDE STRUCTURE zde_zsdt0001cg_alv.
         TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         ico_carga     TYPE char04,
       END OF ty_itens_alv.

CLASS lcl_application_0100 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_link_click  FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
ENDCLASS.                    "LCL_APPLICATION DEFINITION

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_link_click   FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name,
      handle_double_click FOR EVENT node_double_click OF cl_gui_column_tree IMPORTING node_key.
ENDCLASS.

DATA:
  objeto                  TYPE REF TO zif_carga,
  ex_carga                TYPE REF TO zcx_carga,
  ex_ordem_carregamento   TYPE REF TO zcx_ordem_carregamento,
  ex_parceiros            TYPE REF TO zcx_parceiros,
  ex_ordem_venda          TYPE REF TO zcx_ordem_venda,
  ex_soft_expert_workflow TYPE REF TO zcx_soft_expert_workflow,
  ex_ordem                TYPE REF TO zcx_ordem_carregamento,
  ex_romaneio             TYPE REF TO zcx_romaneio,
  ex_job                  TYPE REF TO zcx_job,
  ex_cadastro             TYPE REF TO zcx_cadastro,
  ex_pedido               TYPE REF TO zcx_pedido_compra_exception,
  ex_miro                 TYPE REF TO zcx_miro_exception,
  it_notas                TYPE TABLE OF zde_zsdt0001nt_alv WITH HEADER LINE,
  it_ordens_venda         TYPE TABLE OF zde_zsdt0001ov_alv WITH HEADER LINE,
  it_pedido_compra        TYPE TABLE OF zde_zsdt0001ek_alv WITH HEADER LINE,
  "IT_TAKES_VINCU          TYPE TABLE OF ZDE_ZSDT0001TK_ALV WITH HEADER LINE,
  it_blocos_vincu         TYPE TABLE OF zde_zsdt0001fd_alv WITH HEADER LINE,
  it_ordens_venda_alv     TYPE zsdt0001ov_algodao_alv_t,
  wa_ordens_venda_alv     TYPE zsdt0001ov_algodao_alv,
  it_takes_saldo          TYPE TABLE OF zde_zsdt0001tk_alv_vinc WITH HEADER LINE,
  ck_confer_carga         TYPE char01,
  lc_filtro               TYPE zde_filtro_zsdt0001cg,
  ok_code                 TYPE sy-ucomm,
  ok_code_suppress        TYPE sy-ucomm,
  ck_registro_log         TYPE char01,
  it_tree_info_log        TYPE TABLE OF ty_node_info_logs WITH HEADER LINE,
  docking_0100_se         TYPE REF TO cl_gui_docking_container,
  docking_0100            TYPE REF TO cl_gui_docking_container,
  dialogbox_0100          TYPE REF TO cl_gui_dialogbox_container,
  events_0100             TYPE cntl_simple_events,
  ck_mostrar_logs         TYPE char01,
  g_application_0100      TYPE REF TO lcl_application_0100,
  tree_0100               TYPE REF TO cl_gui_column_tree,
  events                  TYPE cntl_simple_events,
  node_table_0100         TYPE treev_ntab,
  item_table_0100         TYPE STANDARD TABLE OF mtreeitm,
  g_application           TYPE REF TO lcl_application,
  wa_carga_romaneio       TYPE ty_itens_alv,
  it_romaneio_entrada     TYPE TABLE OF zsdt0001 WITH HEADER LINE,
  it_romaneio_entrada_sel TYPE TABLE OF zsdt0001 WITH HEADER LINE,
  gb_id_carga             TYPE zde_id_carga,
  gb_st_carga             TYPE char01,
  gb_id_carga_est         TYPE zde_id_carga,
  gb_st_carga_est         TYPE char01,
  it_retorno_alv          TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
  wa_add_nfe_9002         TYPE ty_add_nfe,
  ck_alterado_nota        TYPE c LENGTH 1,
  ck_alterado_chave       TYPE c LENGTH 1,
  nm_field_set_nota       TYPE c LENGTH 50,
  pos                     TYPE i,
  ok_suppress_dialog      TYPE c,
  ck_conferiu             TYPE c,
  ck_conferiu_fardos      TYPE c,
  nm_field_set_carga      TYPE c LENGTH 50,
  it_tucomm               TYPE TABLE OF sy-ucomm.

DATA: ck_validacao_conferencia TYPE c LENGTH 1.
DATA: ck_validacao_saida_autom TYPE c LENGTH 1.
DATA: ck_efetuar_saida_autom   TYPE c LENGTH 1.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION_0300 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application_0100 IMPLEMENTATION.

  METHOD  handle_link_click.
    PERFORM mostrar_node_log USING node_key item_name.
  ENDMETHOD.                    "HANDLE_LINK_CLICK

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION

CLASS lcl_application IMPLEMENTATION.
  METHOD handle_link_click.
    PERFORM mostra_info_node_item USING node_key item_name .
  ENDMETHOD.

  METHOD handle_double_click.
    "PERFORM MOSTRA_INFO_NODE_CLICK USING NODE_KEY.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.

  ok_suppress_dialog = abap_true.
  ok_code = 'ADDNOTA'.
  SET PARAMETER ID 'ZIDCARGA' FIELD ''.

START-OF-SELECTION.

  IF pck_cad EQ abap_true.
    CLEAR: zde_zsdt0001cg_alv,
           zde_zsdt0001od_alv,
           zde_zsdt0001nt_alv.

    IF pidcarga IS NOT INITIAL.

      CLEAR: ok_suppress_dialog, ok_code.

      objeto =
      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga = pidcarga
        )->get_factory_objeto(
        ).

      TRY .
          IF pmanut EQ abap_false.
            objeto->set_registro( i_id_carga = pidcarga ).
          ELSE.
            IF pidsolic IS INITIAL.
              objeto->set_cria_manutencao( i_id_carga = pidcarga ).
            ELSE.
              objeto->set_registro_manutencao( i_id_solicitacao = pidsolic ).
            ENDIF.
          ENDIF.
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          LEAVE PROGRAM.
        CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
          ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          LEAVE PROGRAM.
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          LEAVE PROGRAM.
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          LEAVE PROGRAM.
        CATCH zcx_soft_expert_workflow INTO ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          LEAVE PROGRAM.
      ENDTRY.

      PERFORM atualizar_dados_tela.

    ELSE.

      DATA: e_algodao_alv      TYPE  zsdt0001ov_algodao_alv_t,
            i_charg            TYPE  charg_d,
            e_id_local_entrega TYPE  zde_id_local_entrega,
            e_ds_local_entrega TYPE  zde_ds_local_entrega.

      MOVE psafra TO i_charg.

      CALL FUNCTION 'ZMF_PESQ_INSTRU_OV_ALGO'
        EXPORTING
          i_vstel       = pfilia
          i_charg       = i_charg
        IMPORTING
          e_algodao_alv = e_algodao_alv.

      IF e_algodao_alv[] IS INITIAL AND sy-sysid NE 'DEV'.
        MESSAGE s038.
        LEAVE PROGRAM.
      ENDIF.

      CALL FUNCTION 'ZMF_PESQ_LOCAL_ENTRADA'
        EXPORTING
          i_vstel            = pfilia
          i_grupo            = zif_carga=>st_grupo_algodao_pluma
        IMPORTING
          e_id_local_entrega = e_id_local_entrega
          e_ds_local_entrega = e_ds_local_entrega.

      IF e_id_local_entrega IS INITIAL AND sy-sysid NE 'DEV'.
        MESSAGE s046.
        LEAVE PROGRAM.
      ENDIF.

      TRY .

          objeto =
          zcl_factory_carga=>zif_factory_carga~get_instance(
            )->set_factory_objeto( i_tp_carga = ptipca i_tp_produto = zif_carga=>st_tp_produto_carga_algodao
            )->get_factory_objeto(
            )->novo_registro(
            )->set_abrir( EXPORTING i_nr_safra          = psafra
                                    i_id_bukrs          = pempre
                                    i_id_branch         = pfilia
                                    i_tipo_produto      = zif_carga=>st_tp_produto_carga_algodao "*-CS2022000332-#78064-07.06.2022-JT
                          IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv
            ).

          DATA: i_ordem_venda	TYPE zde_zsdt0001ov_alv.

          LOOP AT e_algodao_alv INTO DATA(wa_algodao_alv).

            CLEAR: i_ordem_venda.
            i_ordem_venda-nr_ordem_venda = wa_algodao_alv-nr_ordem_venda.
            i_ordem_venda-qt_fardos      = wa_algodao_alv-qt_fardos.

            objeto->add_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda
                 )->set_volume_ordem_venda( EXPORTING i_vbeln = i_ordem_venda-nr_ordem_venda i_volume = CONV #( i_ordem_venda-qt_fardos )
                 ).

          ENDLOOP.

          IF sy-sysid NE 'DEV'.
            objeto->set_local_entrega( i_id_local_entrega = e_id_local_entrega ).
          ENDIF.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          "PERFORM LIMPAR_TELA.
          LEAVE PROGRAM.
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          "PERFORM LIMPAR_TELA.
          LEAVE PROGRAM.
      ENDTRY.
    ENDIF.
    CLEAR: ck_confer_carga.
    CALL SCREEN 0100.

    IF pidcarga IS INITIAL AND ck_confer_carga EQ abap_true AND pmanut EQ abap_false.
      TRY.
          CLEAR: lc_filtro.
          objeto->get_registro( IMPORTING e_registro = DATA(wa_carga) )->free( ).
          lc_filtro-iidcarga = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-id_carga  high = wa_carga-id_carga  ) ).
          lc_filtro-inrsafra = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-nr_safra  high = wa_carga-nr_safra  ) ).
          lc_filtro-iidbukrs = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-id_bukrs  high = wa_carga-id_bukrs  ) ).
          lc_filtro-iidbranc = VALUE #( option = 'EQ' sign = 'I' ( low = wa_carga-id_branch high = wa_carga-id_branch ) ).
          objeto->pesquisar(
                    EXPORTING
                      i_filtros = lc_filtro
                    IMPORTING
                      e_registros = DATA(it_retorno)
                      e_pesquisou = DATA(e_pesquisou) ).
          IF e_pesquisou EQ abap_true.
            CALL SCREEN 0200.
          ENDIF.
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.
    ENDIF.

    LEAVE PROGRAM.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  IF ok_suppress_dialog EQ abap_true.
    SUPPRESS DIALOG.
  ENDIF.

  DATA: html_pagina              TYPE string,
        lc_hierarchy_header_0100 TYPE treev_hhdr,
        lc_event_0100            TYPE cntl_simple_event.

  IF ck_registro_log EQ abap_true.
    SET TITLEBAR 'TL0101' WITH it_tree_info_log-dt_registro it_tree_info_log-hr_registro it_tree_info_log-us_registro.
  ELSEIF pmanut EQ abap_true.
    SET TITLEBAR 'TL0102'.
  ELSE.
    SET TITLEBAR 'TL0100'.
  ENDIF.

  PERFORM pf_satus.

  SET PF-STATUS 'PF0100' EXCLUDING it_tucomm.

  IF zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_aberto OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_cancelada OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_aprovado OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_enviado OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_recusada.

    PERFORM retornar_html_workflow USING zde_zsdt0001acb_alv CHANGING html_pagina.

    IF docking_0100_se IS INITIAL.
      CREATE OBJECT docking_0100_se
        EXPORTING
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = cl_gui_docking_container=>dock_at_right
          extension = 400.

      cl_abap_browser=>show_html(
       EXPORTING
         html_string = html_pagina
         modal       = abap_false
         format      = cl_abap_browser=>landscape
         size        = cl_abap_browser=>medium
         container   = docking_0100_se ).
    ELSE.
      cl_abap_browser=>close_browser( ).
      cl_abap_browser=>show_html(
       EXPORTING
         html_string = html_pagina
         modal       = abap_false
         format      = cl_abap_browser=>landscape
         size        = cl_abap_browser=>medium
         container   = docking_0100_se ).
    ENDIF.

  ELSEIF zde_zsdt0001cg_alv-id_carga IS NOT INITIAL AND ck_mostrar_logs EQ abap_false AND docking_0100 IS INITIAL
     AND zde_zsdt0001cg_alv-tp_status = zif_carga=>st_status_conferido.

    CREATE OBJECT docking_0100
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0100->dock_at_right
        extension = 400.

    PERFORM cria_alv_documentos USING 2 docking_0100->screen0 docking_0100.

    PERFORM atualiza_tree USING zde_zsdt0001cg_alv-id_carga.

  ELSEIF zde_zsdt0001cg_alv-id_carga IS NOT INITIAL AND ck_mostrar_logs EQ abap_true AND docking_0100 IS INITIAL.

    CREATE OBJECT docking_0100
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0100->dock_at_right
        extension = 400.

    lc_hierarchy_header_0100-heading = text-016.
    lc_hierarchy_header_0100-width   = 31.

    CLEAR: events_0100.
    lc_event_0100-eventid    = cl_gui_column_tree=>eventid_link_click.
    lc_event_0100-appl_event = 'X'.
    APPEND lc_event_0100 TO events_0100.

    CREATE OBJECT g_application_0100.

    CREATE OBJECT tree_0100
      EXPORTING
        parent                = docking_0100
        node_selection_mode   = tree_0100->node_sel_mode_single
        item_selection        = 'X'
        hierarchy_column_name = c_tree_0100-column1
        hierarchy_header      = lc_hierarchy_header_0100.

    tree_0100->set_registered_events( EXPORTING events = events_0100
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER g_application_0100->handle_link_click FOR tree_0100.

    CALL METHOD tree_0100->add_column
      EXPORTING
        name        = c_tree_0100-column2
        width       = 20
        header_text = text-017
        alignment   = cl_gui_column_tree=>align_center.

    CALL METHOD tree_0100->add_column
      EXPORTING
        name        = c_tree_0100-column3
        width       = 20
        header_text = text-018
        alignment   = cl_gui_column_tree=>align_center.

    PERFORM atualiza_tree_logs.

  ELSEIF docking_0100 IS INITIAL.

    CREATE OBJECT docking_0100
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0100->dock_at_right
        extension = 400.
  ENDIF.

  PERFORM view_docking.

*  IF DIALOGBOX_0100 IS NOT INITIAL.
*
*    CREATE OBJECT DIALOGBOX_0100
*      EXPORTING
*        WIDTH  = 540
*        HEIGHT = 100
*        TOP    = 150
*        LEFT   = 150
*        REPID  = SY-REPID
*        DYNNR  = SY-DYNNR.
*
*  ENDIF.

  IF ok_suppress_dialog EQ abap_true.
    LEAVE TO LIST-PROCESSING.
  ENDIF.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_NODE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_ITEM_NAME  text
*----------------------------------------------------------------------*
FORM mostrar_node_log
  USING  p_node_key  TYPE  tv_nodekey
         p_item_name TYPE  tv_itmname.

*  READ TABLE IT_TREE_INFO_LOG WITH KEY NODE_KEY = P_NODE_KEY ITEM_NAME = P_ITEM_NAME BINARY SEARCH.
*  IF SY-SUBRC IS INITIAL.
*    TRY .
*        OBJETO->GET_INFO_ALV_APRESENTACAO_LOG(
*          EXPORTING
*            I_DT_REGISTRO  = IT_TREE_INFO_LOG-REGISTRO-DT_REGISTRO
*            I_HR_REGISTRO  = IT_TREE_INFO_LOG-REGISTRO-HR_REGISTRO
*            I_US_REGISTRO  = IT_TREE_INFO_LOG-REGISTRO-US_REGISTRO
*          IMPORTING
*            E_APRESENTACAO = DATA(R_APRESENTACAO) ).
*
*        CK_REGISTRO_LOG    = ABAP_TRUE.
*        ZDE_ZSDT0001CG_ALV = R_APRESENTACAO-CARGA.
*        ZDE_ZSDT0001OD_ALV = R_APRESENTACAO-ORDEM_CARREGA.
*        IT_NOTAS[] = R_APRESENTACAO-NOTAS[].
*        IT_TAKES_VINCU[] = E_APRESENTACAO-TAKEUP[].
*
*        LEAVE TO SCREEN 0300.
*      CATCH ZCX_CARGA INTO EX_CARGA.
*        EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
*      CATCH ZCX_ORDEM_CARREGAMENTO INTO EX_ORDEM_CARREGAMENTO.
*        EX_ORDEM_CARREGAMENTO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
*    ENDTRY.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TREE_LOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_tree_logs .

  DATA: qtd_itens TYPE i,
        node      TYPE treev_node,
        item      TYPE mtreeitm.

  CHECK tree_0100 IS NOT INITIAL.

  tree_0100->delete_all_nodes( ).

  CLEAR: node_table_0100[], item_table_0100[], it_tree_info_log[], it_tree_info_log.

  TRY .
      objeto->get_logs_historico( IMPORTING e_logs = DATA(it_logs) ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

  CHECK it_logs[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_usuarios)
    FROM user_addr
     FOR ALL ENTRIES IN @it_logs
   WHERE bname EQ @it_logs-us_registro.

  SORT it_usuarios BY bname.

  DESCRIBE TABLE it_logs LINES DATA(qtd_linhas).

  LOOP AT it_logs INTO DATA(wa_logs).

    CLEAR: node.
    IF sy-tabix EQ qtd_linhas.
      node-n_image    = icon_customer.
      node-exp_image  = icon_customer.
    ELSE.
      node-n_image    = icon_hr_position.
      node-exp_image  = icon_hr_position.
    ENDIF.

    ADD 1 TO qtd_itens.
    node-node_key   = qtd_itens.
    CONDENSE node-node_key NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = node-node_key
      IMPORTING
        output = node-node_key.

    node-hidden     = abap_false. " The node is visible,
    node-disabled   = abap_false. " selectable,
    node-isfolder   = abap_true. " a folder.
    node-expander   = abap_false.
    APPEND node TO node_table_0100.

    it_tree_info_log-node_key    = node-node_key.
    it_tree_info_log-item_name   = c_tree_0100-column1.
    it_tree_info_log-registro    = wa_logs.
    CONCATENATE wa_logs-dt_registro+6(2) '.' wa_logs-dt_registro+4(2) '.' wa_logs-dt_registro(4) INTO it_tree_info_log-dt_registro.
    CONCATENATE wa_logs-hr_registro(2) ':' wa_logs-hr_registro+2(2) ':' wa_logs-hr_registro+4(2) INTO it_tree_info_log-hr_registro.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0100-column1.
    item-class     = cl_gui_list_tree=>item_class_link. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_intensifd_critical.
    item-font      = cl_gui_list_tree=>item_font_prop.
    READ TABLE it_usuarios INTO DATA(wa_usuarios) WITH KEY bname = wa_logs-us_registro.
    IF sy-subrc IS INITIAL.
      item-text = wa_usuarios-name_textc.
      it_tree_info_log-us_registro = wa_usuarios-name_textc.
    ELSE.
      item-text = wa_logs-us_registro.
      it_tree_info_log-us_registro = wa_logs-us_registro.
    ENDIF.
    APPEND item TO item_table_0100.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0100-column2.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_default.
    item-text      = it_tree_info_log-dt_registro.
    APPEND item TO item_table_0100.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0100-column3.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_default.
    item-text      = it_tree_info_log-hr_registro.
    APPEND item TO item_table_0100.

    APPEND it_tree_info_log.
  ENDLOOP.

  CALL METHOD tree_0100->add_nodes_and_items
    EXPORTING
      node_table                     = node_table_0100
      item_table                     = item_table_0100
      item_table_structure_name      = 'MTREEITM'
    EXCEPTIONS
      failed                         = 1
      cntl_system_error              = 3
      error_in_tables                = 4
      dp_error                       = 5
      table_structure_name_not_found = 6.

  SORT it_tree_info_log BY node_key item_name.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CL_GUI_CONTAINER  text
*----------------------------------------------------------------------*
FORM cria_alv_documentos  USING  p_numero    TYPE i
                                 p_container TYPE REF TO cl_gui_container
                                 p_docking   TYPE REF TO cl_gui_docking_container.

  DATA: event            TYPE cntl_simple_event,
        hierarchy_header TYPE treev_hhdr.

  "ALV DOCUMENTOS DO FLUXO
  CLEAR: events.

  " link click
  event-eventid    = cl_gui_column_tree=>eventid_link_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " link click
  event-eventid    = cl_gui_column_tree=>eventid_node_double_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  CREATE OBJECT g_application.
  hierarchy_header-heading = text-010.
  hierarchy_header-width   = 44.

  CASE p_numero.
    WHEN 1.
      CREATE OBJECT tree
        EXPORTING
          parent                = p_container
          node_selection_mode   = tree->node_sel_mode_single
          item_selection        = 'X'
          hierarchy_column_name = c_tree-column1
          hierarchy_header      = hierarchy_header.
    WHEN 2.
      CREATE OBJECT tree
        EXPORTING
          parent                = p_docking
          node_selection_mode   = tree->node_sel_mode_single
          item_selection        = 'X'
          hierarchy_column_name = c_tree-column1
          hierarchy_header      = hierarchy_header.
  ENDCASE.

  tree->set_registered_events( EXPORTING events = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3
      OTHERS                    = 4 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER g_application->handle_link_click FOR tree.
  SET HANDLER g_application->handle_double_click FOR tree.

  tree->add_column(
    EXPORTING
      name                         = c_tree-column2
      alignment                    = cl_gui_column_tree=>align_left
      width                        = 18
      header_image                 = 'ICON_PRINT'
      header_text                  = 'Imprimir'
    EXCEPTIONS
      column_exists                = 1
      illegal_column_name          = 2
      too_many_columns             = 3
      illegal_alignment            = 4
      different_column_types       = 5
      cntl_system_error            = 6
      failed                       = 7
      predecessor_column_not_found = 8
      OTHERS                       = 9 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  tree->add_column(
    EXPORTING
      name                         = c_tree-column3
      alignment                    = cl_gui_column_tree=>align_left
      width                        = 30
      header_image                 = 'ICON_PRINT'
      header_text                  = 'Descrição Documento'
    EXCEPTIONS
      column_exists                = 1
      illegal_column_name          = 2
      too_many_columns             = 3
      illegal_alignment            = 4
      different_column_types       = 5
      cntl_system_error            = 6
      failed                       = 7
      predecessor_column_not_found = 8
      OTHERS                       = 9 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RETORNAR_HTML_WORKFLOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_ZSDT0001ACB_ALV  text
*      <--P_HTML_PAGINA  text
*----------------------------------------------------------------------*
FORM retornar_html_workflow  USING    p_zsdt0001acb_alv TYPE zde_zsdt0001acb_alv
                             CHANGING p_html_pagina TYPE string.

  DATA: tx_status_filial    TYPE string,
        tx_status_fiscal    TYPE string,
        tx_status_comercial TYPE string.

  DATA: mt_status_filial    TYPE string,
        mt_status_fiscal    TYPE string,
        mt_status_comercial TYPE string.

  DATA: cl_status_filial    TYPE string,
        cl_status_fiscal    TYPE string,
        cl_status_comercial TYPE string.

  p_html_pagina = '<!DOCTYPE html>' && '<html>' && '<head>' && '<style>'.

*A  Solicitação Aprovada
*R  Solicitação Recusada
*W  Solicitação Em Espera de Aprovação
*S  Solicitação não gera Aprovação

  CASE p_zsdt0001acb_alv-tp_solicitacao_status.
    WHEN zif_carga=>st_status_manut_aberto.
      cl_status_filial    = 'inicial'.
      cl_status_fiscal    = 'inicial'.
      cl_status_comercial = 'inicial'.
      tx_status_filial    = 'Solicitação de Manutenção não enviada'.
      tx_status_fiscal    = 'Solicitação de Manutenção não enviada'.
      tx_status_comercial = 'Solicitação de Manutenção não enviada'.
    WHEN OTHERS.
      CASE p_zsdt0001acb_alv-rs_aceite_filial.
        WHEN zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_filial = 'sem_aprovacao'.
          tx_status_filial = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> da Filial'.
        WHEN zif_carga=>st_rs_aceite_manut_espera.
          cl_status_filial = 'enviado'.
          tx_status_filial = 'Solicitação de Manutenção <b>enviada para aprovação</b> pela Filial'.
        WHEN zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_filial = 'aprovado'.
          tx_status_filial = 'Solicitação de Manutenção <b>aprovada</b> pela Filial'.
        WHEN zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_filial = 'recusado'.
          tx_status_filial = 'Solicitação de Manutenção <b>recusada</b> pela Filial'.
          mt_status_filial = '<b>' && p_zsdt0001acb_alv-ds_aceite_filial && '</b>'.
      ENDCASE.

      CASE p_zsdt0001acb_alv-rs_aceite_fiscal.
        WHEN zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_fiscal = 'sem_aprovacao'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> do CSC Fiscal'.
        WHEN zif_carga=>st_rs_aceite_manut_espera.
          cl_status_fiscal = 'enviado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>enviada para aprovação</b> do CSC Fiscal'.
        WHEN zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_fiscal = 'aprovado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>aprovada</b> pelo CSC Fiscal'.
        WHEN zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_fiscal = 'recusado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>recusada</b> pelo CSC Fiscal'.
          mt_status_fiscal = '<b>' && p_zsdt0001acb_alv-ds_aceite_fiscal && '</b>'.
      ENDCASE.

      CASE p_zsdt0001acb_alv-rs_aceite_comercial.
        WHEN zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_comercial = 'sem_aprovacao'.
          tx_status_comercial = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> do CSC Financeiro'.
        WHEN zif_carga=>st_rs_aceite_manut_espera.
          cl_status_comercial = 'enviado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>enviada para aprovação</b> do CSC Financeiro'.
        WHEN zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_comercial = 'aprovado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>aprovada</b> pelo do CSC Financeiro'.
        WHEN zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_comercial = 'recusado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>recusada</b> pelo do CSC Financeiro'.
          mt_status_comercial = '<b>' && p_zsdt0001acb_alv-ds_aceite_comercial && '</b>'.
      ENDCASE.
  ENDCASE.

  p_html_pagina = p_html_pagina &&
 ' .inicial {' && ' background-color:LightGray;' && ' color:black;' &&' margin:10px;' && ' padding:10px;' && '}' &&
 ' .enviado {' && ' background-color:Orange;' &&' color:black;' &&' margin:10px;' && ' padding:10px;' && '}' &&
 ' .sem_aprovacao {' &&' background-color:DodgerBlue;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}' &&
 ' .aprovado {' &&' background-color:MediumSeaGreen;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}' &&
 ' .recusado {' &&' background-color:Tomato;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}'.

  p_html_pagina = p_html_pagina && '</style>' && '</head>' && '<body>'. " && '<table style="width:100%">' && '<tr>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_filial && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação Filial</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_filial && '</p>'.
  IF mt_status_filial IS NOT INITIAL.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_filial && '</b></p>'.
  ENDIF.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_fiscal && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação CSC Fiscal</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_fiscal && '</p>'.
  IF mt_status_fiscal IS NOT INITIAL.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_fiscal && '</b></p>'.
  ENDIF.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_comercial && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação CSC Financeira</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_comercial && '</p>'.
  IF mt_status_comercial IS NOT INITIAL.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_comercial && '</b></p>'.
  ENDIF.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '</body>' && '</html>'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  DATA: answer TYPE c.

  answer = '1'.
  IF objeto->ck_alterou EQ abap_true.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = text-001
        text_question         = text-002
        text_button_1         = text-003
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = text-004
        icon_button_2         = 'ICON_INCOMPLETE'
        default_button        = '2'
        display_cancel_button = ' '
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.
  CHECK answer EQ '1'.
  PERFORM limpa_tela_0100.
  PERFORM limpar_tela_0101.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPA_TELA_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_tela_0100 .

  IF tree IS NOT INITIAL.
    tree->free( ).
  ENDIF.
  CLEAR: tree.

  IF tree_0100 IS NOT INITIAL.
    tree_0100->free( ).
  ENDIF.
  CLEAR: tree_0100.

  CLEAR: g_application_0100.

  IF docking_0100_se IS NOT INITIAL.
    cl_abap_browser=>close_browser( ).
    docking_0100_se->free( ).
  ENDIF.
  CLEAR: docking_0100_se.

  IF docking_0100 IS NOT INITIAL.
    docking_0100->free( ).
  ENDIF.
  CLEAR: docking_0100.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUSCAR_INDO_NOTA_DIGITADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_indo_nota_digitada .

  DATA: qtd TYPE i,
        nfe TYPE REF TO zcl_nfe_inbound.

  qtd = strlen( wa_add_nfe_9002-n55_chave_acesso ).

  wa_add_nfe_9002-ck_incluir = abap_false.

  IF qtd NE 44.
    MESSAGE s034 WITH wa_add_nfe_9002-n55_chave_acesso.
    EXIT.
  ENDIF.

  TRY .
      CREATE OBJECT nfe
        EXPORTING
          i_chave_nfe = wa_add_nfe_9002-n55_chave_acesso.

      TRY .
          nfe->set_info_sap( ).
        CATCH zcx_nfe_inbound_exception.
        CATCH zcx_cadastro.
        CATCH zcx_pedido_compra_exception.
      ENDTRY.

      DATA(info_nota) = nfe->get_info_nota( ).
      nfe->free( ).
      CLEAR: nfe.
      wa_add_nfe_9002-branch     = info_nota-nfe_base-f_tomadora.
      wa_add_nfe_9002-bukrs      = info_nota-nfe_base-e_tomadora.
      wa_add_nfe_9002-docnum_nfe = info_nota-nfe_base-docnum_nfe.
      wa_add_nfe_9002-parid      = info_nota-nfe_base-p_emissor.
      SELECT SINGLE stcd3 INTO wa_add_nfe_9002-parid_ie
        FROM lfa1
       WHERE lifnr EQ info_nota-nfe_base-p_emissor.
      wa_add_nfe_9002-nftot      = info_nota-nfe_base-vl_total.
      wa_add_nfe_9002-dt_emissao = info_nota-nfe_base-dt_emissao.
      wa_add_nfe_9002-numero     = info_nota-nfe_base-numero.
      wa_add_nfe_9002-serie      = info_nota-nfe_base-serie.
      wa_add_nfe_9002-ntgew      = 0.

      LOOP AT info_nota-nfe_base-itens INTO DATA(wa_item).
        TRANSLATE wa_item-prod_und_comerci TO UPPER CASE.
        CASE wa_item-prod_und_comerci.
          WHEN 'KG'.
            ADD wa_item-prod_qtd_comerci TO wa_add_nfe_9002-ntgew.
          WHEN 'TO'.
            wa_item-prod_qtd_comerci = wa_item-prod_qtd_comerci * 1000.
            ADD wa_item-prod_qtd_comerci TO wa_add_nfe_9002-ntgew.
        ENDCASE.
        wa_add_nfe_9002-cfop = wa_item-prod_cfop.
      ENDLOOP.

      SELECT SINGLE butxt INTO wa_add_nfe_9002-butxt
        FROM t001
       WHERE bukrs EQ info_nota-nfe_base-e_tomadora.

      SELECT SINGLE name INTO wa_add_nfe_9002-name
        FROM j_1bbranch
       WHERE bukrs EQ info_nota-nfe_base-e_tomadora
         AND branch EQ info_nota-nfe_base-f_tomadora.

      SELECT SINGLE name1 INTO wa_add_nfe_9002-name1
        FROM lfa1
       WHERE lifnr EQ wa_add_nfe_9002-parid.

      wa_add_nfe_9002-ck_incluir = abap_true.

      "Fardos ----------------------------------------------------------------
      LOOP AT info_nota-nfe_base-volumes_transp INTO DATA(wa_volumes) WHERE ds_especie CS 'FARDO'.
        wa_add_nfe_9002-nr_fardo = wa_volumes-nm_qvol.
        wa_add_nfe_9002-nm_pesol = wa_volumes-nm_pesol.
        wa_add_nfe_9002-nm_pesob = wa_volumes-nm_pesob.
      ENDLOOP.

    CATCH zcx_nfe_inbound_exception INTO DATA(ex_nfe_inbound_exception).
      IF nfe IS NOT INITIAL.
        nfe->free( ).
      ENDIF.
      ex_nfe_inbound_exception->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).
      IF nfe IS NOT INITIAL.
        nfe->free( ).
      ENDIF.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      IF ex_cadastro->msgid = 'ZNFE_DISTRI' AND ex_cadastro->msgno = 103.
        MESSAGE i035.
      ENDIF.
  ENDTRY.

  "WA_ADD_NFE_9002-.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lc_titlebar      TYPE string,
        lc_text_question TYPE string.

  READ TABLE it_tucomm INTO DATA(wa_code) WITH KEY = ok_code.
  IF sy-subrc IS INITIAL.
    MESSAGE 'Função não permitida' TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR: ok_code.
    EXIT.
  ENDIF.

  CASE ok_code.

    WHEN 'SAVE'.
      "Salvar
      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = text-020.
          lc_text_question = text-021.
        WHEN abap_true.
          lc_titlebar      = text-026.
          lc_text_question = text-027.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .
          objeto->gravar_registro( ).

          PERFORM atualizar_dados_tela.
          PERFORM limpa_tela_0100.
          PERFORM limpar_tela_0101.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          "PERFORM SETA_CAMPO USING EX_CARGA->MSGID EX_CARGA->MSGNO.
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          "PERFORM SETA_CAMPO USING EX_PARCEIROS->MSGID EX_PARCEIROS->MSGNO.
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          "PERFORM SETA_CAMPO USING EX_ORDEM_VENDA->MSGID EX_ORDEM_VENDA->MSGNO.
      ENDTRY.

    WHEN 'ABRIR'.
      "Abrir

      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = text-022.
          lc_text_question = text-023.
        WHEN abap_true.
          lc_titlebar      = text-028.
          lc_text_question = text-029.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .
          objeto->set_abrir( IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).

          PERFORM atualizar_dados_tela.
          PERFORM limpa_tela_0100.
          PERFORM limpar_tela_0101.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_soft_expert_workflow INTO ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_job INTO ex_job.
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

    WHEN 'FECHAR'.
      "Fechar
      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = text-101.
          lc_text_question = text-102.
        WHEN abap_true.
          lc_titlebar      = text-032.
          lc_text_question = text-033.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .

          objeto->verif_saldo_ordem_venda( ).

          objeto->verif_ticket_pesagem( ).

          objeto->set_fechar( IMPORTING e_fechou = DATA(e_fechou) ).

          IF NOT e_fechou EQ abap_true.
            IF sy-msgid EQ 'ZCARGA'.
              CASE sy-msgno.
                WHEN 039.
                  SET CURSOR FIELD 'ZDE_ZSDT0001CG_ALV-ID_ENTRADA'.
              ENDCASE.
            ENDIF.
            EXIT.
          ELSE.

            zde_zsdt0001cg_alv-id_carga = objeto->carga-id_carga.

            IF pmanut EQ abap_true.
              zde_zsdt0001acb_alv-id_solicitacao = objeto->solicitacao_manutencao-id_solicitacao.
              objeto->free(
                )->limpar_registro(
                )->set_registro_manutencao( i_id_solicitacao = zde_zsdt0001acb_alv-id_solicitacao
                ).
            ELSE.

              objeto->free(
                )->limpar_registro(
                )->set_registro( i_id_carga = zde_zsdt0001cg_alv-id_carga
                ).
            ENDIF.

          ENDIF.

          PERFORM atualizar_dados_tela.
          PERFORM limpa_tela_0100.
          PERFORM limpar_tela_0101.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
        CATCH zcx_soft_expert_workflow INTO ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
        CATCH zcx_job INTO ex_job.
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
      ENDTRY.

    WHEN 'CONFERIR'.
      "Conferir

      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = text-103.
          lc_text_question = text-104.
        WHEN abap_true.
          lc_titlebar      = text-034.
          lc_text_question = text-035.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      PERFORM conferir_carga.

      PERFORM limpa_tela_0100.
      PERFORM limpar_tela_0101.

    WHEN 'CANCELAR'.

      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = text-024.
          lc_text_question = text-025.
        WHEN abap_true.
          lc_titlebar      = text-030.
          lc_text_question = text-031.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .
          objeto->set_cancelar( ).
          PERFORM atualizar_dados_tela.
          PERFORM limpa_tela_0100.
          PERFORM limpar_tela_0101.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  CONFERIR_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conferir_carga .

  DATA: ck_retorno TYPE sy-subrc.
  DATA: ck_saida_automatica TYPE char01.

  DATA: lc_peso_liquido TYPE zde_nm_peso_liquido.

  CLEAR: ck_conferiu.

  IF objeto->at_manutencao NE abap_true.
    IF NOT ( objeto->carga-ck_enviado_opus = abap_true AND objeto->carga-ck_recebido_opus EQ abap_false ).
      PERFORM confere_carga CHANGING ck_retorno ck_saida_automatica.
    ELSE.
      ck_retorno = 0.
    ENDIF.
  ELSE.
    ck_retorno = 0 .
  ENDIF.

  CHECK ck_retorno IS INITIAL.

  TRY .
      objeto->set_conferido(
         EXPORTING i_proximo_passo_automatico = ck_saida_automatica
         IMPORTING e_conferiu = DATA(e_conferiu) ).

      IF NOT e_conferiu EQ abap_true.
        EXIT.
      ENDIF.

    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_parceiros INTO ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_ordem_venda INTO ex_ordem_venda.
      ex_ordem_venda->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_job INTO ex_job.
      ex_job->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_pedido_compra_exception INTO ex_pedido.
      ex_pedido->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_miro_exception INTO ex_miro.
      ex_miro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

    CATCH zcx_ordem_carregamento INTO ex_ordem.
      ex_ordem->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      EXIT.

  ENDTRY.

  objeto->get_registro( IMPORTING e_registro = DATA(e_registro) ).

  PERFORM limpa_tela_0100.
  PERFORM limpar_tela_0101.

  "EXPORT E_REGISTRO-ID_CARGA TO MEMORY ID 'IDCARGA'.
  SET PARAMETER ID 'ZIDCARGA' FIELD e_registro-id_carga.
  "ZCL_UTIL=>AT_PARAM = E_REGISTRO-ID_CARGA.
  "WRITE E_REGISTRO-ID_CARGA.

  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_DADOS_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_dados_tela .

  TRY .

      objeto->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_apresentacao) ).
      zde_zsdt0001cg_alv    = e_apresentacao-carga.
      zde_zsdt0001od_alv    = e_apresentacao-ordem_carrega.
      zde_zsdt0001acb_alv   = e_apresentacao-manutencao.
      it_notas[]            = e_apresentacao-notas[].
      it_ordens_venda[]     = e_apresentacao-ordem_venda[].
      it_ordens_venda_alv[] = e_apresentacao-ordem_venda_algo[].
      it_pedido_compra[]    = e_apresentacao-pedido_compra[].
      it_blocos_vincu[]     = e_apresentacao-blocos[].

      READ TABLE it_notas INDEX 1 INTO zde_zsdt0001nt_alv.

    CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
      ex_ordem_carregamento->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).

    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SETA_CAMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EX_ORDEM_VENDA_>MSGID  text
*      -->P_EX_ORDEM_VENDA_>MSGNO  text
*----------------------------------------------------------------------*
FORM seta_campo  USING    p_msgid TYPE syst_msgid
                          p_msgno TYPE syst_msgno.

  CLEAR: nm_field_set_nota,
         nm_field_set_carga.

  CASE p_msgid.
    WHEN 'ZODVENDA'. "Ordem de Venda

      nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM_VENDA'.

    WHEN 'ZORDEMCA'. "Ordem de Carregamento

      "Local Entrega
      nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM'.

    WHEN 'ZCARGA'.
      CASE p_msgno.

          """" Cabeçalho """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        WHEN zcx_carga=>zcx_obg_inf_ordem_venda-msgno OR
             zcx_carga=>zcx_obg_inf_produto-msgno.

          "Ordem de Venda
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM_VENDA'.

        WHEN zcx_carga=>zcx_ordem_empresaag-msgno.

          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE'.

        WHEN zcx_carga=>zcx_obg_inf_lc_entrega-msgno OR
             zcx_carga=>zcx_le_sem_param_lc_negocio-msgno OR
             zcx_carga=>zcx_le_sem_param_material-msgno.

          "Local Entrega
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_ENTREGA'.

        WHEN zcx_carga=>zcx_obg_inf_lc_coleta-msgno.

          "Local de Coleta
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_COLETA'.

        WHEN zcx_carga=>zcx_obg_inf_lc_destino-msgno.

          "Destino
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESTINO'.

        WHEN zcx_carga=>zcx_obg_inf_lc_descarga-msgno.

          "Local de Descarga
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESCARGA'.

        WHEN zcx_carga=>zcx_ordem_vencida-msgno OR
             zcx_carga=>zcx_ordem_produto-msgno OR
             zcx_carga=>zcx_placa_trator-msgno OR
             zcx_carga=>zcx_placa_reboque1-msgno OR
             zcx_carga=>zcx_placa_reboque2-msgno OR
             zcx_carga=>zcx_placa_reboque3-msgno OR
             zcx_carga=>zcx_ordem_motorista-msgno OR
             zcx_carga=>zcx_ordem_proprietario-msgno OR
             zcx_carga=>zcx_ordem_destino-msgno OR
             zcx_carga=>zcx_ordem_descarga-msgno.

          "Ordem de Carregamento
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM'.

        WHEN zcx_carga=>zcx_obg_inf_motorista-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_MOTORISTA'.

        WHEN zcx_carga=>zcx_obg_inf_proprietario-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO'.

        WHEN zcx_carga=>zcx_obg_inf_trator-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR'.

        WHEN zcx_carga=>zcx_obg_inf_ticket-msgno OR
             zcx_carga=>zcx_erro_ticket_utilizado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_TICKET'.

        WHEN zcx_carga=>zcx_obg_inf_ps_bruto-msgno.

        WHEN zcx_carga=>zcx_obg_inf_ps_bruto-msgno OR
             zcx_carga=>zcx_tara_maior_bruto-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_BRUTO'.

        WHEN zcx_carga=>zcx_obg_inf_ps_tara-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_TARA'.

        WHEN zcx_carga=>zcx_obg_inf_ps_subtotal-msgno OR
             zcx_carga=>zcx_errp_ps_subtotal-msgno OR
             zcx_carga=>zcx_peso_liq_subtotal-msgno OR
             zcx_carga=>zcx_obg_inf_ps_liquido-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_SUBTOTAL'.

        WHEN zcx_carga=>zcx_obg_emp_classificadora-msgno OR
             zcx_carga=>zcx_fornecedor_nao_classifica-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_CLASSIFICADORA'.

        WHEN zcx_carga=>zcx_obg_class_umidade-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_UMI'.
        WHEN zcx_carga=>zcx_obg_class_impureza-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_IMP'.
        WHEN zcx_carga=>zcx_obg_class_avariado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA'.
        WHEN zcx_carga=>zcx_obg_class_ardido-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_ARD'.
        WHEN zcx_carga=>zcx_obg_class_quebrado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_QUE'.
        WHEN zcx_carga=>zcx_obg_class_esverdeado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_ESV'.

        WHEN zcx_carga=>zcx_nao_teste_amaggi_positivo-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-IN_GMO'.

        WHEN zcx_carga=>zcx_obg_inf_outro_part-msgno OR
             zcx_carga=>zcx_nao_inf_outro_part-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_OUTRO_PARTIC'.

          """" Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "Tipo de Entrada
        WHEN zcx_carga=>zcx_obg_inf_tp_entrada-msgno OR
             zcx_carga=>zcx_tp_entrada_nao_permitido-msgno OR
             zcx_carga=>zcx_te_md_fiscal_sem_param-msgno OR
             zcx_carga=>zcx_te_sem_ct_fiscal-msgno OR
             zcx_carga=>zcx_te_sem_tp_mov_merc-msgno OR
             zcx_carga=>zcx_te_sem_iva-msgno OR
             zcx_carga=>zcx_te_sem_form_pagamento-msgno OR
             zcx_carga=>zcx_te_sem_ch_bloqueio-msgno OR
             zcx_carga=>zcx_te_sem_bnc_empresa-msgno OR
             zcx_carga=>zcx_te_somente_fisica-msgno OR
             zcx_carga=>zcx_te_somente_juridica-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-ID_ENTRADA'.

          "Modelo de Documento Fiscal
        WHEN zcx_carga=>zcx_nao_permitido_md_fiscal-msgno OR
             zcx_carga=>zcx_pessoa_fis_nfe-msgno OR
             zcx_carga=>zcx_pessoa_jus_papel-msgno OR
             zcx_carga=>zcx_nf_propria_nfe-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL'.

          "Chave NF-e
        WHEN zcx_carga=>zcx_obg_inf_chave_nfe-msgno OR
             zcx_carga=>zcx_nfe_nao_distribuida-msgno OR
             zcx_carga=>zcx_nfe_item_nao_distribuido-msgno OR
             zcx_carga=>zcx_nfe_item_unidade-msgno OR
             zcx_carga=>zcx_xml_nfe_nao_recebido-msgno.

          nm_field_set_nota = 'BTN_CHAVE'.

          "IE do Fornecedor
        WHEN zcx_carga=>zcx_obg_inf_ie_prod-msgno OR
             zcx_carga=>zcx_obg_inf_fornecedor-msgno OR
             zcx_carga=>zcx_forn_sem_parametro-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE'.

          "Numero Documento
        WHEN zcx_carga=>zcx_obg_inf_nf_numero-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_NOTA'.

          "Série do Documento
        WHEN zcx_carga=>zcx_obg_inf_nf_serie-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NM_SERIE'.

          "Data de Emissão
        WHEN zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgno OR
             zcx_carga=>zcx_data_emissao_nf-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-DT_EMISSAO'.

          "Quantidade
        WHEN zcx_carga=>zcx_obg_inf_nf_quantidade-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_QUANTIDADE'.

          "Valor
        WHEN zcx_carga=>zcx_obg_inf_nf_valor_total-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_VALOR'.

          "Data de Vencimento
        WHEN zcx_carga=>zcx_obg_inf_dt_venc_form-msgno OR
             zcx_carga=>zcx_data_formulario_venc-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-DT_VENCIMENTO_FORM'.

          "CFOP
        WHEN zcx_carga=>zcx_obg_inf_cfop-msgno OR
             zcx_carga=>zcx_cfop_nao_permitido_te-msgno OR
             zcx_carga=>zcx_cfop_nao_permitido_forn-msgno OR
             zcx_carga=>zcx_xml_nfe_cfop_invalido-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-CFOP'.

        WHEN zcx_carga=>zcx_obg_inf_qt_fardo-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_FARDO'.

      ENDCASE.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VIEW_DOCKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_docking .

  PERFORM view_danfe.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ON_CTMENU_LOAD_GUI_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM on_ctmenu_load_gui_status .
  BREAK-POINT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_SATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_satus .

  CLEAR it_tucomm[].

  TRY .
      objeto->get_ck_saida_automatica( ).
    CATCH zcx_carga.
      APPEND 'FATURAMENT' TO it_tucomm.
  ENDTRY.

  IF objeto->ck_alterou EQ abap_false.
    APPEND 'SAVE' TO it_tucomm.
  ENDIF.

  IF objeto->at_manutencao EQ abap_true.
    TRY .
        objeto->get_ck_sol_ajuste_nao_proc( ).
      CATCH zcx_carga.    "
        APPEND 'PROCESSSOL' TO it_tucomm.
    ENDTRY.
  ELSE.
    APPEND 'PROCESSSOL' TO it_tucomm.
  ENDIF.
  IF ( zde_zsdt0001cg_alv-id_carga IS INITIAL ) OR ( objeto->at_manutencao EQ abap_true ).
    APPEND 'SHOWLOGS' TO it_tucomm.
  ENDIF.

  TRY .
      objeto->get_tp_status( IMPORTING e_tp_status = DATA(e_tp_status) ).
    CATCH zcx_carga.
  ENDTRY.

  CASE e_tp_status.
    WHEN zif_carga=>st_status_aberto.
      APPEND 'ABRIR'      TO it_tucomm.
      APPEND 'CONFERIR'   TO it_tucomm.
      APPEND 'FATURAMENT' TO it_tucomm.
      APPEND 'NOVA_SOLIC' TO it_tucomm.
      IF zde_zsdt0001cg_alv-id_carga IS INITIAL.
        APPEND 'CANCELAR' TO it_tucomm.
      ENDIF.
    WHEN zif_carga=>st_status_fechado.
      APPEND 'ADD_NOTA'   TO it_tucomm.
      APPEND 'FECHAR'     TO it_tucomm.
      APPEND 'NOVA_SOLIC' TO it_tucomm.
    WHEN zif_carga=>st_status_cancelada.
      APPEND 'ADD_NOTA'   TO it_tucomm.
      APPEND 'ADDNOTA' TO it_tucomm.
      APPEND 'ADDOC' TO it_tucomm.
      APPEND 'ADDTAKE' TO it_tucomm.
      APPEND 'ABRIR'      TO it_tucomm.
      APPEND 'FECHAR'     TO it_tucomm.
      APPEND 'CONFERIR'   TO it_tucomm.
      APPEND 'FATURAMENT' TO it_tucomm.
      APPEND 'CANCELAR'   TO it_tucomm.
      APPEND 'NOVA_SOLIC' TO it_tucomm.
    WHEN zif_carga=>st_status_conferido.
      APPEND 'ADD_NOTA' TO it_tucomm.
      APPEND 'ADDNOTA' TO it_tucomm.
      APPEND 'ADDOC' TO it_tucomm.
      APPEND 'ADDTAKE' TO it_tucomm.
      APPEND 'ABRIR'    TO it_tucomm.
      APPEND 'CONFERIR' TO it_tucomm.
      IF objeto->at_manutencao EQ abap_true.
        APPEND 'NOVA_SOLIC' TO it_tucomm.
      ELSE.
        APPEND 'CANCELAR' TO it_tucomm.
      ENDIF.
  ENDCASE.

  IF ck_registro_log EQ abap_true.
    APPEND 'ADD_NOTA' TO it_tucomm.
    APPEND 'ADDNOTA' TO it_tucomm.
    APPEND 'ADDOC' TO it_tucomm.
    APPEND 'ADDTAKE' TO it_tucomm.
    APPEND 'ABRIR'    TO it_tucomm.
    APPEND 'FECHAR'   TO it_tucomm.
    APPEND 'CONFERIR' TO it_tucomm.
    APPEND 'CANCELAR' TO it_tucomm.
    APPEND 'FATURAMENT' TO it_tucomm.
    "Registro Log: Data: &1 Hora: &2 Usuário: &3
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALCULA_MEDIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM calcula_media  CHANGING lc_peso_fardo_sem_emb TYPE ty_valor
                             lc_peso_fardo_com_emb TYPE ty_valor
                             lc_peso_embalagem     TYPE ty_valor.

  IF zde_zsdt0001nt_alv-nr_fardo IS INITIAL OR zde_zsdt0001cg_alv-nm_peso_subtotal IS INITIAL.
    lc_peso_embalagem = 0.
    lc_peso_fardo_sem_emb   = 0.
    lc_peso_fardo_com_emb   = 0.
  ELSE.
    DATA(ps_embalagem)    = zde_zsdt0001cg_alv-nm_peso_subtotal - zde_zsdt0001cg_alv-nm_peso_liquido.
    IF ps_embalagem GT 0.
      lc_peso_embalagem     = ps_embalagem / zde_zsdt0001nt_alv-nr_fardo.
      lc_peso_fardo_sem_emb = zde_zsdt0001cg_alv-nm_peso_liquido  / zde_zsdt0001nt_alv-nr_fardo.
      lc_peso_fardo_com_emb = zde_zsdt0001cg_alv-nm_peso_subtotal / zde_zsdt0001nt_alv-nr_fardo.
    ELSE.
      lc_peso_embalagem     = 0.
      lc_peso_fardo_sem_emb = 0.
      lc_peso_fardo_com_emb = 0.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONFERE_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM confere_carga  CHANGING  p_retorno TYPE sy-subrc p_automatico TYPE char01.

  DATA: lc_contador TYPE zde_sequencia_nota.

  p_retorno = 9.
  ck_validacao_conferencia = abap_false.
  ck_efetuar_saida_autom   = abap_false.

  CALL SCREEN 9003.

  IF ck_validacao_conferencia EQ abap_true AND ck_validacao_saida_autom EQ abap_true.
    p_retorno = 0.
  ELSE.
    p_retorno = 1.
  ENDIF.

  p_automatico = ck_efetuar_saida_autom.

  PERFORM limpar_tela_9003.

ENDFORM.

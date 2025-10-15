*&---------------------------------------------------------------------*
*& Report  ZMMR155
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr155 MESSAGE-ID zcarga.

TABLES: zde_info_frete_pedagio,
        zde_info_frete_frete,
        zde_info_frete,
        zsdt0001fe.

CLASS cl_tree_notas_event_receiver DEFINITION DEFERRED.
CLASS cl_tree_aviso_event_receiver DEFINITION DEFERRED.
CLASS cl_tree_local_event_receiver DEFINITION DEFERRED.
CLASS cl_tree_agrupa_event_receiver DEFINITION DEFERRED.
CLASS lcl_event_alarm_receiver DEFINITION DEFERRED.
CLASS lcl_event_receiver_9011 DEFINITION DEFERRED.

TYPES: BEGIN OF ty_ufs_alv.
         INCLUDE STRUCTURE zde_zsdt0001feufs_alv.
         TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
       END OF ty_ufs_alv.

TYPES BEGIN OF ty_entrega_coleta.
TYPES: id_local_descarga   TYPE zde_id_local_descarga,
       ds_local_descarga   TYPE zde_ds_local_descarga,
       id_entregue_por     TYPE zde_id_entregue_por,
       ds_entregue_por     TYPE zde_ds_entregue_por,
       id_agent_frete	     TYPE zde_id_agent_frete,
       ds_agent_frete	     TYPE zde_ds_agent_frete,
       preco_frete         TYPE kbetr_kond,
       quantidade_precos   TYPE i,
       po_number           TYPE ebeln,
       av_vbeln            TYPE vbeln_vl,
       message             TYPE string,
       erro                TYPE char01,
       placa_trator        TYPE zde_placa_trator,
       id_produto	         TYPE matnr,
       ds_produto	         TYPE maktx,
       dt_referecia        TYPE lddat,
       i_mensagem_causa    TYPE string,
       i_system_response   TYPE string,
       i_what_to_do        TYPE string,
       i_sys_admin         TYPE string,
       i_mensagem_completa TYPE string.
TYPES END OF ty_entrega_coleta.

TYPES BEGIN OF ty_mapa_node.
TYPES: node_key          TYPE tv_nodekey,
       tree_tipo         TYPE string,
       chave             TYPE string,
       node_key_anterior TYPE tv_nodekey,
       id_agrupamento    TYPE zde_id_agrupa_frete,
       vbeln             TYPE vbeln.
TYPES END OF ty_mapa_node.

TYPES BEGIN OF ty_agrupa.
TYPES: cont        TYPE REF TO cl_gui_container,
       tree        TYPE REF TO cl_gui_column_tree,
       event       TYPE REF TO cl_tree_agrupa_event_receiver,
       drop        TYPE REF TO cl_dragdrop,
       node_table  TYPE treev_ntab,
       item_table  TYPE etalv_mtree,
       drop_handle TYPE i.
TYPES END OF ty_agrupa.

DATA: ok_code TYPE sy-ucomm.

DATA: dg_splitter                TYPE REF TO cl_gui_splitter_container,
      "DG_SPLITTER     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      ctl_cccontainer_carga      TYPE REF TO cl_gui_container,
      dg_splitter_carga          TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer_carga_nt   TYPE REF TO cl_gui_container, "Container de Nota Fiscal
      tree_notas                 TYPE REF TO cl_gui_column_tree,
      tree_notas_event_receiver  TYPE REF TO cl_tree_notas_event_receiver,
      tree_avisos                TYPE REF TO cl_gui_column_tree,
      tree_avisos_event_receiver TYPE REF TO cl_tree_aviso_event_receiver,
      ctl_cccontainer_carga_av   TYPE REF TO cl_gui_container, "Container de Aviso de Recebimento.
      tree_notas_drag            TYPE REF TO cl_dragdrop,
      tree_notas_drag_handle     TYPE i,
      tree_notas_drop            TYPE REF TO cl_dragdrop,
      tree_notas_drop_handle     TYPE i,
      qtd_itens                  TYPE i VALUE 0,
      node_notas_table           TYPE treev_ntab,
      item_notas_table           TYPE STANDARD TABLE OF mtreeitm,
      node_avisos_table          TYPE treev_ntab,
      item_avisos_table          TYPE STANDARD TABLE OF mtreeitm,
      it_cargas                  TYPE TABLE OF zde_carga_apresentacao,
      it_avisos                  TYPE zde_zsdt0001nt_alv_t,
      it_mapa_node               TYPE TABLE OF ty_mapa_node.

DATA: ctl_cccontainer_frete      TYPE REF TO cl_gui_container,
      dg_splitter_frete          TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer_frete_dc   TYPE REF TO cl_gui_container, "Container de Documentos de Fatura de Frete.
      ctl_cccontainer_frete_lc   TYPE REF TO cl_gui_container, "Container de Locais e Preço de Pontos de Coleta e Entrega.
      tree_locais                TYPE REF TO cl_gui_column_tree,
      tree_locais_event_receiver TYPE REF TO cl_tree_local_event_receiver,
      node_locais_table          TYPE treev_ntab,
      item_locais_table          TYPE STANDARD TABLE OF mtreeitm,
      it_locais                  TYPE TABLE OF ty_entrega_coleta,
      it_locl_all                TYPE TABLE OF ty_entrega_coleta,
      tree_locais_drag           TYPE REF TO cl_dragdrop,
      tree_locais_drag_handle    TYPE i.

DATA: dg_splitter_agrupa      TYPE REF TO cl_gui_splitter_container,
      it_tree_agrupa          TYPE TABLE OF ty_agrupa,
      it_aviso_fretes         TYPE TABLE OF zlest0108,
      it_aviso_fretes_all     TYPE TABLE OF zlest0108,
      it_agrupa_fretes        TYPE TABLE OF zlest0108,
      go_clock                TYPE REF TO cl_gui_timer,
      go_alarm                TYPE REF TO lcl_event_alarm_receiver,
      ck_dado_frete_9011_ok   TYPE char01,
      ck_dado_frete_9011_cs   TYPE char01,
      ck_erro_pedagio_9011    TYPE char01,
      lc_msg_erro_pedagio     TYPE string,
      lc_tx_cid_origem        TYPE text60,
      lc_tx_cid_destino       TYPE text60,
      lc_tem_margem_adiant    TYPE char01,
      lc_tem_margem_zero      TYPE char01,
      ck_alterou_pedagio      TYPE char01,
      ck_alterou_itinerario   TYPE char01,
      lc_limpar_ck_pedagio    TYPE char01,
      ck_ped_repom_eletronico TYPE char01,
      ck_ped_tipfr_eletronico TYPE char01,
      ck_erro_frete_9011      TYPE char01,
      it_ufs_intermediarios   TYPE zde_zsdt0001feufs_t,
      wa_ufs_intermediarios   TYPE zsdt0001feufs,
      it_ufs_alv              TYPE TABLE OF ty_ufs_alv,
      wa_ufs_alv              TYPE ty_ufs_alv.

DATA: container_9011       TYPE REF TO cl_gui_custom_container,
      ctl_container_9011   TYPE REF TO cl_gui_custom_container,
      ctl_alv_9011         TYPE REF TO cl_gui_alv_grid,
      gs_layout_9011       TYPE lvc_s_layo,
      gs_variant_9011      TYPE disvariant,
      it_fieldcatalog_9011 TYPE lvc_t_fcat,
      event_handler_9011   TYPE REF TO lcl_event_receiver_9011.

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_REC_DRAGDROP                              *
*----------------------------------------------------------------------*
CLASS                    cl_tree_notas_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT node_double_click OF cl_gui_column_tree IMPORTING node_key.
    METHODS handle_on_drag FOR EVENT on_drag OF cl_gui_column_tree IMPORTING node_key item_name drag_drop_object.
    METHODS handle_button_click FOR EVENT button_click OF cl_gui_column_tree IMPORTING node_key item_name.
    METHODS handle_link_click FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
  PRIVATE SECTION.
ENDCLASS.

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_REC_DRAGDROP                              *
*----------------------------------------------------------------------*
CLASS cl_tree_aviso_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT node_double_click OF cl_gui_column_tree IMPORTING node_key.
    METHODS handle_on_drop FOR EVENT on_drop OF cl_gui_column_tree IMPORTING node_key drag_drop_object.
    METHODS handle_button_click FOR EVENT button_click OF cl_gui_column_tree IMPORTING node_key item_name.
    METHODS handle_link_click FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
  PRIVATE SECTION.
ENDCLASS.


*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_REC_DRAGDROP                              *
*----------------------------------------------------------------------*
CLASS cl_tree_local_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_link_click FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
    METHODS handle_on_drag FOR EVENT on_drag OF cl_gui_column_tree IMPORTING node_key item_name drag_drop_object.
  PRIVATE SECTION.
ENDCLASS.

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_REC_DRAGDROP                              *
*----------------------------------------------------------------------*
CLASS cl_tree_agrupa_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT node_double_click OF cl_gui_column_tree IMPORTING node_key.
    METHODS handle_on_drop FOR EVENT on_drop OF cl_gui_column_tree IMPORTING node_key drag_drop_object.
    METHODS handle_button_click FOR EVENT button_click OF cl_gui_column_tree IMPORTING node_key item_name.
    METHODS handle_link_click FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_event_receiver_9011 DEFINITION.
  PUBLIC SECTION.
    DATA: error_in_data TYPE c.
    METHODS: data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS: data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

  PRIVATE SECTION.
    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
    METHODS: atualiza_transferencia
      IMPORTING
        i_linha         TYPE lvc_s_modi
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.                    "lcl_event_receiver DEFINITION


CLASS lcl_dragdrop_obj_tree DEFINITION.
  PUBLIC SECTION.
    DATA: tipo TYPE string.
    DATA: node TYPE tv_nodekey.
ENDCLASS.

CLASS lcl_event_alarm_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.
  PRIVATE SECTION.
ENDCLASS.

CONSTANTS: cs_line_color_finalizado  TYPE c LENGTH 4 VALUE 'C500',
           cs_line_color_alterado    TYPE c LENGTH 4 VALUE 'C300',
           cs_line_color_selecionada TYPE c LENGTH 4 VALUE 'C601'.

CONSTANTS:

  c_time_interval TYPE i VALUE 5,

  BEGIN OF c_tree_tipo,
    tipo_all_carga  TYPE string VALUE 'AllCarga',
    tipo_carga      TYPE string VALUE 'Carga',
    tipo_carga_01   TYPE string VALUE 'CargaAviso',
    tipo_nota       TYPE string VALUE 'NotaFical',
    tipo_aviso      TYPE string VALUE 'AvisoRecebe',
    tipo_aviso_01   TYPE string VALUE 'AvisoDeletar',

    tipo_local      TYPE string VALUE 'ColetaEntrega',
    tipo_local_00   TYPE string VALUE 'ColetaEnt00', "Gerar Frete
    tipo_local_01   TYPE string VALUE 'ColetaEnt01', "Local de Entrega
    tipo_local_02   TYPE string VALUE 'ColetaEnt02', "Local de Coleta
    tipo_local_03   TYPE string VALUE 'ColetaEnt03', "Preço
    tipo_local_04   TYPE string VALUE 'ColetaEnt04', "Mensagem
    tipo_local_05   TYPE string VALUE 'ColetaEnt05', "Aviso de Recebimento
    tipo_local_06   TYPE string VALUE 'ColetaEnt06', "Pedido de Compra
    tipo_local_07   TYPE string VALUE 'ColetaEnt07', "Placa Trator
    tipo_local_08   TYPE string VALUE 'ColetaEnt08', "Agente de Frete
    tipo_local_09   TYPE string VALUE 'ColetaEnt09', "Produto
    tipo_local_10   TYPE string VALUE 'ColetaEnt10', "Data de Referência

    tipo_agrupa     TYPE string VALUE 'Agrupa',
    tipo_agrupa_00  TYPE string VALUE 'AgrupaGerar',
    tipo_agrupa_01  TYPE string VALUE 'AgrupaAviso',
    tipo_agrupa_02  TYPE string VALUE 'AgrupaTrans',
    tipo_agrupa_03  TYPE string VALUE 'AgrupaCusto',
    tipo_agrupa_03a TYPE string VALUE 'AgrupaCustoPed',
    tipo_agrupa_04  TYPE string VALUE 'AgrupaOrdem',
    tipo_agrupa_05  TYPE string VALUE 'AgrupaFatura',
    tipo_agrupa_06  TYPE string VALUE 'AgrupaCTe',
    tipo_agrupa_06a TYPE string VALUE 'AgrupaCTeTip',
    tipo_agrupa_06b TYPE string VALUE 'AgrupaCTeCIOT',
    tipo_agrupa_07  TYPE string VALUE 'AgrupaMDFe',
  END OF c_tree_tipo,

  BEGIN OF c_tree_nota,
    column1           TYPE tv_itmname VALUE c_tree_tipo-tipo_nota,
    column1_header    TYPE tv_heading VALUE 'Notas de Entrada',
    column1_01        TYPE tv_itmname VALUE c_tree_tipo-tipo_carga_01,
    column1_01_header TYPE tv_heading VALUE 'Gerar',

    column2           TYPE tv_itmname VALUE c_tree_tipo-tipo_aviso,
    column2_header    TYPE tv_heading VALUE 'Avisos de Recebimento',

    column2_01        TYPE tv_itmname VALUE c_tree_tipo-tipo_aviso_01,
    column2_01_header TYPE tv_heading VALUE 'Excluir',

    column3           TYPE tv_itmname VALUE c_tree_tipo-tipo_local,
    column3_header    TYPE tv_heading VALUE 'Local(is) de Coleta e Entrega',
    column3_00        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_00,
    column3_00_header TYPE tv_heading VALUE 'Gerar Frete',
    column3_01        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_01,
    column3_01_header TYPE tv_heading VALUE 'Local Entrega',
    column3_02        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_02,
    column3_02_header TYPE tv_heading VALUE 'Local Coleta',
    column3_03        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_03,
    column3_03_header TYPE tv_heading VALUE 'Preço Frete',
    column3_04        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_04,
    column3_04_header TYPE tv_heading VALUE 'Mensagem',
    column3_05        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_05,
    column3_05_header TYPE tv_heading VALUE 'Aviso',
    column3_06        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_06,
    column3_06_header TYPE tv_heading VALUE 'Pedido',
    column3_07        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_07,
    column3_07_header TYPE tv_heading VALUE 'Placa',
    column3_08        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_08,
    column3_08_header TYPE tv_heading VALUE 'Agente Frete',
    column3_09        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_09,
    column3_09_header TYPE tv_heading VALUE 'Produto',
    column3_10        TYPE tv_itmname VALUE c_tree_tipo-tipo_local_10,
    column3_10_header TYPE tv_heading VALUE 'Dt.Referência',

    column4           TYPE tv_itmname VALUE c_tree_tipo-tipo_agrupa,
    column4_header    TYPE tv_heading VALUE 'Agrupamentos para Emissão de Frete de Entrada',
    column4_01        TYPE tv_itmname VALUE c_tree_tipo-tipo_agrupa_00,
    column4_01_header TYPE tv_heading VALUE 'Gerar',


  END OF c_tree_nota.

CLASS cl_tree_notas_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
  ENDMETHOD.

  METHOD handle_on_drag.
    CHECK NOT node_key IS INITIAL.
    DATA: l_obj     TYPE REF TO lcl_dragdrop_obj_tree.

    CREATE OBJECT l_obj.
    l_obj->node = node_key.
    l_obj->tipo = c_tree_tipo-tipo_nota.
    drag_drop_object->object = l_obj.
  ENDMETHOD.

  METHOD handle_button_click.
  ENDMETHOD.

  METHOD handle_link_click.

    DATA: i_cod_loc_entrega TYPE kunnr.

    CASE item_name.
      WHEN c_tree_nota-column1_01.
        READ TABLE it_mapa_node INTO DATA(wa_mapa_node) WITH KEY node_key = node_key BINARY SEARCH.

        CASE wa_mapa_node-tree_tipo.
          WHEN c_tree_tipo-tipo_carga.
            "Gerar Automático, todos os avisos da carga.
            DATA: p_id_carga TYPE zde_id_carga.
            p_id_carga = wa_mapa_node-chave(10).
            PERFORM gerar_automatico_carga_entrada USING p_id_carga.

          WHEN c_tree_tipo-tipo_nota.

            "Gerar Aviso de Recebimento """""""""""""""""""""""""""""""""""
            READ TABLE it_cargas INTO DATA(wa_carga) WITH KEY carga-id_carga = wa_mapa_node-chave(10).

            DATA l_erro.
            PERFORM f_popup_ag_frete CHANGING wa_carga-carga-id_agent_frete l_erro.

            IF l_erro IS INITIAL.

              LOOP AT wa_carga-notas INTO DATA(wa_nota) WHERE id_nota EQ wa_mapa_node-chave+10(06).

                i_cod_loc_entrega = wa_carga-carga-id_branch.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = i_cod_loc_entrega
                  IMPORTING
                    output = i_cod_loc_entrega.

                TRY.
                    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~set_criar_aviso_nota_carga(
                      EXPORTING
                        i_agente_frete            = wa_carga-carga-id_agent_frete
                        i_cod_loc_entrega         = i_cod_loc_entrega
                        i_nota                    = wa_nota
                      IMPORTING
                        e_vbeln                   = DATA(e_vbeln)
                      RECEIVING
                        r_gerou                   = DATA(r_gerou)
                    ).

                    PERFORM atualiza_objetos_all USING abap_true.
                  CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).
                    PERFORM atualiza_objetos_all USING abap_true.
                    ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
                ENDTRY.
              ENDLOOP.

            ELSE.
              MESSAGE s000(z_mm) WITH 'Agente de Frete '
                        wa_carga-carga-id_agent_frete ' não é intercompany(ZFIC)!' DISPLAY LIKE 'E'.
            ENDIF.

            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        ENDCASE.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS cl_tree_aviso_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
  ENDMETHOD.

  METHOD handle_button_click.
  ENDMETHOD.

  METHOD handle_on_drop.

    DATA: l_obj    TYPE REF TO lcl_dragdrop_obj_tree,
          it_notas TYPE zde_zsdt0001nt_alv_t.
    l_obj = CAST #( drag_drop_object->object ).

    READ TABLE it_mapa_node INTO DATA(wa_mapa_node) WITH KEY node_key = l_obj->node BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    CASE wa_mapa_node-tree_tipo.
      WHEN c_tree_tipo-tipo_all_carga.

        "Ler todas os nós abaixo de Notas
        LOOP AT it_mapa_node INTO DATA(wa_mapa_node_carga) WHERE node_key_anterior EQ wa_mapa_node-node_key.
          READ TABLE it_cargas INTO DATA(wa_carga) WITH KEY carga-id_carga = wa_mapa_node_carga-chave.
          LOOP AT wa_carga-notas INTO DATA(wa_nota).
            APPEND wa_nota TO it_notas.
          ENDLOOP.
        ENDLOOP.

      WHEN c_tree_tipo-tipo_carga.

        "Ler todas os nós abaixo de Carga
        LOOP AT it_mapa_node INTO DATA(wa_mapa_node_nota) WHERE node_key_anterior EQ wa_mapa_node-node_key.
          READ TABLE it_cargas INTO wa_carga WITH KEY carga-id_carga = wa_mapa_node_nota-chave(10).
          LOOP AT wa_carga-notas INTO wa_nota WHERE id_nota EQ wa_mapa_node_nota-chave+10(06).
            APPEND wa_nota TO it_notas.
          ENDLOOP.
        ENDLOOP.

      WHEN c_tree_tipo-tipo_nota.

        READ TABLE it_cargas INTO wa_carga WITH KEY carga-id_carga = wa_mapa_node-chave(10).
        LOOP AT wa_carga-notas INTO wa_nota WHERE id_nota EQ wa_mapa_node-chave+10(06).
          APPEND wa_nota TO it_notas.
        ENDLOOP.

    ENDCASE.

    DATA: i_cod_loc_entrega TYPE kunnr.

    "Gerar Aviso de Recebimento das Notas que não possui Aviso de Recebimento """"""""""""""""""""""""""""""
    LOOP AT it_notas INTO wa_nota.

      READ TABLE it_cargas INTO wa_carga WITH KEY carga-id_carga = wa_nota-id_carga.

      i_cod_loc_entrega = wa_carga-carga-id_branch.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_cod_loc_entrega
        IMPORTING
          output = i_cod_loc_entrega.

      TRY.
          zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~set_criar_aviso_nota_carga(
            EXPORTING
              i_agente_frete            = wa_carga-carga-id_agent_frete
              i_cod_loc_entrega         = i_cod_loc_entrega
              i_nota                    = wa_nota
            IMPORTING
              e_vbeln                   = DATA(e_vbeln)
            RECEIVING
              r_gerou                   = DATA(r_gerou)
          ).

          PERFORM atualiza_objetos_all USING abap_true.
        CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).
          PERFORM atualiza_objetos_all USING abap_true.
          ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    ENDLOOP.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.

  METHOD handle_link_click.

    CASE item_name.
      WHEN c_tree_nota-column2_01.
        READ TABLE it_mapa_node INTO DATA(wa_mapa_node) WITH KEY node_key = node_key BINARY SEARCH.
        CASE wa_mapa_node-tree_tipo.
          WHEN c_tree_tipo-tipo_aviso_01.
            "Deletar Aviso de Recebimento """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            READ TABLE it_cargas INTO DATA(wa_carga) WITH KEY carga-id_carga = wa_mapa_node-chave(10).
            LOOP AT wa_carga-notas INTO DATA(wa_nota) WHERE id_nota EQ wa_mapa_node-chave+10(06).
              TRY.
                  zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~set_dele_aviso_nota_carga( i_nota = wa_nota ).
                CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).
                  ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
              ENDTRY.
            ENDLOOP.
            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            PERFORM atualiza_objetos_all USING abap_true.
        ENDCASE.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS cl_tree_local_event_receiver IMPLEMENTATION.
  METHOD handle_link_click.

    DATA: i_avisos TYPE t_vbeln.

    CASE item_name.
      WHEN c_tree_nota-column3_00.
        READ TABLE it_mapa_node INTO DATA(wa_mapa_node) WITH KEY node_key = node_key BINARY SEARCH.

        CASE wa_mapa_node-tree_tipo.
          WHEN c_tree_tipo-tipo_local_00.

            CLEAR: i_avisos.

            "Gerar Registro para Faturar
            LOOP AT it_mapa_node INTO DATA(wa_avisos) WHERE node_key_anterior = wa_mapa_node-node_key.
              APPEND VALUE #( vbeln = wa_avisos-chave(10) ) TO i_avisos.
            ENDLOOP.

            TRY.
                zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance( )->set_new_documento_agrupado( EXPORTING i_avisos = i_avisos ).
                PERFORM atualiza_objetos_all  USING abap_true.
              CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).    "
                PERFORM atualiza_objetos_all  USING abap_true.
                ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
            ENDTRY.


        ENDCASE.

      WHEN c_tree_nota-column3_04.
        READ TABLE it_mapa_node INTO wa_mapa_node WITH KEY node_key = node_key BINARY SEARCH.
        CASE wa_mapa_node-tree_tipo.
          WHEN c_tree_tipo-tipo_local.

            READ TABLE it_locl_all WITH KEY av_vbeln = wa_mapa_node-chave INTO DATA(wa_locl_all).

            CALL FUNCTION 'ZUTIL_MSG_TELA_INFO'
              EXPORTING
                i_mensagem_causa    = wa_locl_all-i_mensagem_causa
                i_mensagem_completa = wa_locl_all-i_mensagem_completa
                i_system_response   = wa_locl_all-i_system_response
                i_what_to_do        = wa_locl_all-i_what_to_do
                i_sys_admin         = wa_locl_all-i_sys_admin.

        ENDCASE.
    ENDCASE.


  ENDMETHOD.

  METHOD handle_on_drag.
    CHECK NOT node_key IS INITIAL.
    DATA: l_obj     TYPE REF TO lcl_dragdrop_obj_tree.
    CREATE OBJECT l_obj.
    l_obj->node = node_key.
    l_obj->tipo = c_tree_tipo-tipo_local.
    drag_drop_object->object = l_obj.
  ENDMETHOD.

ENDCLASS.

CLASS cl_tree_agrupa_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
  ENDMETHOD.

  METHOD handle_on_drop.

    DATA: l_obj    TYPE REF TO lcl_dragdrop_obj_tree,
          it_notas TYPE zde_zsdt0001nt_alv_t.
    l_obj = CAST #( drag_drop_object->object ).

    "Aviso de Recebimento (Drag)
    READ TABLE it_mapa_node INTO DATA(wa_mapa_drag) WITH KEY node_key = l_obj->node BINARY SEARCH.
    CHECK sy-subrc IS INITIAL.

    "Agrupamento (Drop)
    READ TABLE it_mapa_node INTO DATA(wa_mapa_drop) WITH KEY node_key = node_key BINARY SEARCH.
    CHECK sy-subrc IS INITIAL AND wa_mapa_drop-id_agrupamento IS NOT INITIAL.

    TRY.
        zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
           )->set_new_documento_agrupado(
               EXPORTING
                 i_avisos          = VALUE #( ( vbeln = wa_mapa_drag-chave(10) ) )
                 i_id_agrupa_frete = wa_mapa_drop-id_agrupamento
           ).
        PERFORM atualiza_objetos_all  USING abap_true.
      CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).    "
        PERFORM atualiza_objetos_all  USING abap_true.
        ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.


  ENDMETHOD.

  METHOD handle_button_click.
  ENDMETHOD.

  METHOD handle_link_click.

    DATA: p_fiscal TYPE j_1bdocnum.

    READ TABLE it_mapa_node INTO DATA(wa_mapa_node) WITH KEY node_key = node_key BINARY SEARCH.

    CASE item_name.
      WHEN c_tree_nota-column4_01.

        CASE wa_mapa_node-tree_tipo.
          WHEN c_tree_tipo-tipo_agrupa. "Gerar CT-e do Agrupamento

            READ TABLE it_aviso_fretes_all INTO DATA(wa_aviso_fretes) WITH KEY id_agrupa_frete = wa_mapa_node-id_agrupamento.

            CASE wa_aviso_fretes-doc_transp.
              WHEN space.
                TRY.
                    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
                      )->set_registro( EXPORTING i_vbeln = wa_mapa_node-vbeln    " Nº documento de vendas e distribuição
                      )->set_gerar_documento_transporte(
                      ).
                    PERFORM atualiza_objetos_all  USING abap_true.
                  CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).    "
                    PERFORM atualiza_objetos_all  USING abap_true.
                    ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
                ENDTRY.

              WHEN OTHERS.

                TRY.
                    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
                      )->set_registro( EXPORTING i_vbeln = wa_mapa_node-vbeln    " Nº documento de vendas e distribuição
                      )->set_estornar_documentos( i_estornar_aviso = abap_false
                      ).

                    PERFORM atualiza_objetos_all  USING abap_true.
                  CATCH zcx_doc_fiscal_ft_entrada INTO ex_entrada.    "
                    PERFORM atualiza_objetos_all  USING abap_true.
                    ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
                ENDTRY.

            ENDCASE.


          WHEN c_tree_tipo-tipo_agrupa_01. "Remessa
            CASE wa_aviso_fretes-doc_transp.
              WHEN space.

                "Apagar Transporte da Remessa Remessa """"""""""""""""""""""""""""""""""""""
                TRY.
                    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
                       )->set_registro( EXPORTING i_vbeln = wa_mapa_node-vbeln    " Nº documento de vendas e distribuição
                       )->set_estornar_documentos( i_estornar_aviso = abap_false
                       )->set_dele_aviso_frete( i_vbeln = CONV #( wa_mapa_node-chave(10) )
                       ).
                    PERFORM atualiza_objetos_all  USING abap_true.
                  CATCH zcx_doc_fiscal_ft_entrada INTO ex_entrada.    "
                    PERFORM atualiza_objetos_all  USING abap_true.
                    ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
                ENDTRY.

              WHEN OTHERS.

                TRY.
                    zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
                      )->set_registro( EXPORTING i_vbeln = wa_mapa_node-vbeln    " Nº documento de vendas e distribuição
                      )->set_estornar_documentos( i_estornar_aviso = abap_false
                      ).

                    PERFORM atualiza_objetos_all  USING abap_true.
                  CATCH zcx_doc_fiscal_ft_entrada INTO ex_entrada.    "
                    PERFORM atualiza_objetos_all  USING abap_true.
                    ex_entrada->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
                ENDTRY.

            ENDCASE.

          WHEN c_tree_tipo-tipo_agrupa_03a. "Pedágio
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM imprimir_pedagio_repom USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_06. "CTe
            p_fiscal = wa_mapa_node-chave(10).
            PERFORM imprimir_doc_fiscal USING p_fiscal.
          WHEN c_tree_tipo-tipo_agrupa_06a. "Contrato
            p_fiscal = wa_mapa_node-chave(10).
            PERFORM imprimir_ctr_viagem USING p_fiscal.
          WHEN c_tree_tipo-tipo_agrupa_06b. "CIOT
            p_fiscal = wa_mapa_node-chave(10).
            PERFORM imprimir_ctr_viagem USING p_fiscal.
          WHEN c_tree_tipo-tipo_agrupa_07. "MDFe
            p_fiscal = wa_mapa_node-chave(10).
            PERFORM imprimir_mdfe USING space p_fiscal.
        ENDCASE.

      WHEN c_tree_nota-column4.
        "Documento
        CASE wa_mapa_node-tree_tipo.
          WHEN c_tree_tipo-tipo_agrupa_01.  "Aviso de Recebimento
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM mostrar_aviso USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_02.  "Documento de Transporte
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM mostrar_doc_transporte USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_03.  "Documento de Custo
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM mostrar_doc_custo USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_03a. "Pedágio
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM mostrar_pedagio_repom USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_04.  "Ordem de Venda
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM mostrar_ordem_venda USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_05.  "Fatura
            wa_mapa_node-vbeln = wa_mapa_node-chave(10).
            PERFORM mostrar_fatura_vf USING wa_mapa_node-vbeln.
          WHEN c_tree_tipo-tipo_agrupa_06.  "CTe
            p_fiscal = wa_mapa_node-chave(10).
            PERFORM mostrar_monitor_eletronico USING p_fiscal.
          WHEN c_tree_tipo-tipo_agrupa_06a. "TipFrete
          WHEN c_tree_tipo-tipo_agrupa_06b. "ciot
          WHEN c_tree_tipo-tipo_agrupa_07.  "MDF-e
            p_fiscal = wa_mapa_node-chave(10).
            PERFORM mostrar_monitor_eletronico USING p_fiscal.
        ENDCASE.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_alarm_receiver IMPLEMENTATION.
  METHOD: on_finished.
    PERFORM atualiza_objetos_all USING abap_true.
  ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


CLASS lcl_event_receiver_9011 IMPLEMENTATION.

  METHOD perform_semantic_checks.

    DATA: lc_sequencia TYPE zde_nm_sequencia.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good) WHERE fieldname EQ 'NM_SEQUENCIA'.

      IF ls_good-value IS NOT INITIAL.
        lc_sequencia = ls_good-value.
      ENDIF.

      IF lc_sequencia IS INITIAL.
        error_in_data = abap_false.
        CALL METHOD atualiza_transferencia
          EXPORTING
            i_linha         = ls_good
            pr_data_changed = pr_data_changed.
        CONTINUE.
      ENDIF.

      error_in_data = abap_true.

      "Verificar Informação """"""""""""""""""""""""""""""""
      READ TABLE it_ufs_alv INTO wa_ufs_alv INDEX ls_good-row_id.
      IF wa_ufs_alv-bland = zde_info_frete_pedagio-cd_cid_origem(3).
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZCARGA'
            i_msgno     = '235'
            i_msgty     = 'E'
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.
        EXIT.
      ENDIF.
      IF wa_ufs_alv-bland = zde_info_frete_pedagio-cd_cid_destino(3).
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZCARGA'
            i_msgno     = '235'
            i_msgty     = 'E'
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.
        EXIT.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""

      error_in_data = abap_false.

      CALL METHOD atualiza_transferencia
        EXPORTING
          i_linha         = ls_good
          pr_data_changed = pr_data_changed.

    ENDLOOP.

  ENDMETHOD.

  METHOD atualiza_transferencia.

    DATA: lc_sequencia TYPE zde_nm_sequencia.

    IF i_linha-value IS NOT INITIAL.
      lc_sequencia = i_linha-value.
      IF lc_sequencia IS NOT INITIAL.
        DATA(lc_cor) = cs_line_color_alterado.
      ELSE.
        CLEAR: lc_cor.
      ENDIF.
    ELSE.
      CLEAR: lc_cor.
    ENDIF.

    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_row_id    = i_linha-row_id
        i_fieldname = 'LINE_COLOR'
        i_value     = lc_cor.

  ENDMETHOD.

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD data_changed_finished.

    DATA: lc_nm_sequencia	TYPE zde_nm_sequencia.

    CHECK e_modified IS NOT INITIAL.

    READ TABLE et_good_cells INTO DATA(wa_good_cells) INDEX 1.

    DATA(it_ufs_alv2) = it_ufs_alv[].

    SORT it_ufs_alv2 BY nm_sequencia ASCENDING.
    DELETE it_ufs_alv2 WHERE nm_sequencia IS INITIAL.

    READ TABLE it_ufs_alv INTO DATA(wa_ufs_alv) INDEX wa_good_cells-row_id.
    DELETE it_ufs_alv2 WHERE bland EQ wa_ufs_alv-bland.

    DATA(lc_nm_sequencia_new) = 1.
    lc_nm_sequencia = wa_good_cells-value.
    LOOP AT it_ufs_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE nm_sequencia IS NOT INITIAL AND nm_sequencia LT lc_nm_sequencia.
      <fs_alv>-nm_sequencia = lc_nm_sequencia_new.
      ADD 1 TO lc_nm_sequencia_new.
    ENDLOOP.

    lc_nm_sequencia_new = lc_nm_sequencia.
    ADD 1 TO lc_nm_sequencia_new.
    LOOP AT it_ufs_alv ASSIGNING <fs_alv> WHERE nm_sequencia IS NOT INITIAL AND nm_sequencia GE lc_nm_sequencia AND bland NE wa_ufs_alv-bland.
      <fs_alv>-nm_sequencia = lc_nm_sequencia_new.
      ADD 1 TO lc_nm_sequencia_new.
    ENDLOOP.

    IF wa_ufs_alv-nm_sequencia IS INITIAL.
      DELETE it_ufs_intermediarios WHERE bland = wa_ufs_alv-bland.
    ENDIF.

    CHECK wa_ufs_alv-nm_sequencia IS NOT INITIAL.

    READ TABLE it_ufs_intermediarios WITH KEY bland = wa_ufs_alv-bland TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      LOOP AT it_ufs_intermediarios ASSIGNING FIELD-SYMBOL(<fs_gravar>).
        IF wa_ufs_alv-bland EQ <fs_gravar>-bland.
          <fs_gravar>-id_carga     = zsdt0001fe-id_carga.
          <fs_gravar>-nm_sequencia = wa_ufs_alv-nm_sequencia.
        ELSE.
          READ TABLE it_ufs_alv INTO wa_ufs_alv WITH KEY bland = <fs_gravar>-bland.
          <fs_gravar>-nm_sequencia = wa_ufs_alv-nm_sequencia.
        ENDIF.
      ENDLOOP.
    ELSE.
      CLEAR: wa_ufs_intermediarios.
      wa_ufs_intermediarios-bland          = wa_ufs_alv-bland.
      wa_ufs_intermediarios-cd_cid_origem  = zde_info_frete_pedagio-cd_cid_origem.
      wa_ufs_intermediarios-cd_cid_destino = zde_info_frete_pedagio-cd_cid_destino.
      wa_ufs_intermediarios-id_carga       = zsdt0001fe-id_carga.
      wa_ufs_intermediarios-land1          = 'BR'.
      wa_ufs_intermediarios-route          = zde_info_frete-route.
      wa_ufs_intermediarios-nm_sequencia   = wa_ufs_alv-nm_sequencia.
      APPEND wa_ufs_intermediarios TO it_ufs_intermediarios.
    ENDIF.

    ck_alterou_itinerario = abap_true.

    ctl_alv_9011->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD.

ENDCLASS.                    "lcl_event_receiver DEFINITION

PARAMETERS: pidcarga TYPE zde_id_carga NO-DISPLAY,
            pautomat TYPE char01 NO-DISPLAY.


INITIALIZATION.


START-OF-SELECTION.

  IF pidcarga IS NOT INITIAL.

    CASE pautomat.
      WHEN abap_true.

        TRY .
            zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_instance(
              )->set_new_documento_if_carga(
                EXPORTING i_if_carga = zcl_factory_carga=>zif_factory_carga~get_instance(
                                          )->set_factory_objeto_id( i_id_carga = pidcarga
                                          )->get_factory_objeto(
                                          )->set_registro( i_id_carga = pidcarga i_no_enqueue = abap_true
                                          )
              ).
          CATCH zcx_carga INTO DATA(ex_carga).  "
            ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_frete).    "
            ex_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.

        LEAVE PROGRAM.

      WHEN abap_false.

        SELECT * INTO TABLE @DATA(it_zlest0108)
          FROM zlest0108
         WHERE id_carga EQ @pidcarga
           AND id_agrupa_frete NE @space.

        IF it_zlest0108[] IS NOT INITIAL.
          SELECT * APPENDING TABLE @DATA(it_zlest0108_all)
            FROM zlest0108
             FOR ALL ENTRIES IN @it_zlest0108
           WHERE id_agrupa_frete EQ @it_zlest0108-id_agrupa_frete.
        ENDIF.

        TRY .
            DATA(carga) = zcl_factory_carga=>zif_factory_carga~get_instance(
              )->set_factory_objeto_id( i_id_carga = pidcarga
              )->get_factory_objeto(
              )->set_registro( i_id_carga = pidcarga i_no_enqueue = abap_true
              ).

            carga->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_apresentacao) ).
            APPEND e_apresentacao TO it_cargas.
            CLEAR: carga.
          CATCH zcx_carga INTO ex_carga.  "
            ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH cx_root INTO DATA(ex_root).
            MESSAGE ex_root->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

        LOOP AT it_zlest0108_all INTO DATA(wa_agrupa) WHERE id_carga NE pidcarga.
          TRY .
              carga = zcl_factory_carga=>zif_factory_carga~get_instance(
                )->set_factory_objeto_id( i_id_carga = wa_agrupa-id_carga
                )->get_factory_objeto(
                )->set_registro( i_id_carga = wa_agrupa-id_carga i_no_enqueue = abap_true
                ).
              carga->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao ).
              APPEND e_apresentacao TO it_cargas.
              CLEAR: carga.
            CATCH zcx_carga INTO ex_carga.  "
              ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
            CATCH cx_root INTO ex_root.
              MESSAGE ex_root->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
          ENDTRY.
        ENDLOOP.

    ENDCASE.

  ENDIF.

END-OF-SELECTION.

  IF pautomat NE abap_true AND sy-batch NE abap_true.
    CALL SCREEN 0001.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  IF dg_splitter IS INITIAL.

    dg_splitter = NEW #( parent = cl_gui_container=>screen0 rows = 1 columns = 2 ).

    "Carga ----------------------------------------------------------------------
    dg_splitter->set_column_width( id = 1 width = 25 ).
    ctl_cccontainer_carga = dg_splitter->get_container( row = 1 column = 1 ).

    dg_splitter_carga = NEW #( parent = ctl_cccontainer_carga rows = 2 columns = 1 ).
    ctl_cccontainer_carga_nt = dg_splitter_carga->get_container( row = 1 column = 1 ).
    ctl_cccontainer_carga_av = dg_splitter_carga->get_container( row = 2 column = 1 ).

    tree_notas = NEW #(
      parent = ctl_cccontainer_carga_nt
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
      item_selection = abap_true
      hierarchy_column_name = c_tree_nota-column1
      hierarchy_header = VALUE #( width = '50' heading = c_tree_nota-column1_header )
    ).

    "Gerar Aviso
    tree_notas->add_column( EXPORTING name = c_tree_nota-column1_01 alignment = cl_gui_column_tree=>align_center width = 10 header_text = c_tree_nota-column1_01_header ).

    tree_notas->set_registered_events(
      events =  VALUE #( ( eventid = cl_gui_column_tree=>eventid_link_click appl_event = abap_true )
                         ( eventid = cl_gui_column_tree=>eventid_node_double_click appl_event = abap_true ) ) ).


    tree_notas_event_receiver = NEW #( ).
    SET HANDLER tree_notas_event_receiver->handle_double_click FOR tree_notas.
    SET HANDLER tree_notas_event_receiver->handle_on_drag FOR tree_notas.
    SET HANDLER tree_notas_event_receiver->handle_button_click FOR tree_notas.
    SET HANDLER tree_notas_event_receiver->handle_link_click FOR tree_notas.

    tree_notas_drag = NEW #( ).
    tree_notas_drag->add( EXPORTING flavor = 'Tree_move' dragsrc = abap_true droptarget = abap_false effect = cl_dragdrop=>move ).
    tree_notas_drag->get_handle( IMPORTING handle = tree_notas_drag_handle ).

    tree_notas_drop = NEW #( ).
    tree_notas_drop->add( EXPORTING flavor = 'Tree_move' dragsrc = abap_false droptarget = abap_true effect = cl_dragdrop=>move ).
    tree_notas_drop->get_handle( IMPORTING handle = tree_notas_drop_handle ).

    tree_avisos = NEW #(
      parent = ctl_cccontainer_carga_av
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
      item_selection = abap_true
      hierarchy_column_name = c_tree_nota-column2
      hierarchy_header = VALUE #( width = '50' heading = c_tree_nota-column2_header )
    ).

    "Delete Aviso
    tree_avisos->add_column( EXPORTING name = c_tree_nota-column2_01 alignment = cl_gui_column_tree=>align_center width = 10 header_text = c_tree_nota-column2_01_header ).

    tree_avisos->set_registered_events(
      events =  VALUE #( ( eventid = cl_gui_column_tree=>eventid_link_click appl_event = abap_true )
                         ( eventid = cl_gui_column_tree=>eventid_node_double_click appl_event = abap_true ) ) ).

    tree_avisos_event_receiver = NEW #( ).
    SET HANDLER tree_avisos_event_receiver->handle_on_drop FOR tree_avisos.
    SET HANDLER tree_avisos_event_receiver->handle_double_click FOR tree_avisos.
    SET HANDLER tree_avisos_event_receiver->handle_button_click FOR tree_avisos.
    SET HANDLER tree_avisos_event_receiver->handle_link_click FOR tree_avisos.

    "Frete -----------------------------------------------------------------------------------
    ctl_cccontainer_frete = dg_splitter->get_container( row = 1 column = 2 ).
    dg_splitter_frete = NEW #( parent = ctl_cccontainer_frete rows = 2 columns = 1 ).

    ctl_cccontainer_frete_lc = dg_splitter_frete->get_container( row = 1 column = 1 ).
    ctl_cccontainer_frete_dc = dg_splitter_frete->get_container( row = 2 column = 1 ).

    tree_locais = NEW #(
      parent = ctl_cccontainer_frete_lc
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
      item_selection = abap_true
      hierarchy_column_name = c_tree_nota-column3
      hierarchy_header = VALUE #( width = '60' heading = c_tree_nota-column3_header )
    ).

    "Gerar Frete
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_00 alignment = cl_gui_column_tree=>align_center width = 10 header_text = c_tree_nota-column3_00_header ).
    "Preço
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_03 alignment = cl_gui_column_tree=>align_right width = 20 header_text = c_tree_nota-column3_03_header ).
    "Mensagem
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_04 alignment = cl_gui_column_tree=>align_left  width = 30 header_text = c_tree_nota-column3_04_header ).
    "Pedido
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_06 alignment = cl_gui_column_tree=>align_left  width = 12 header_text = c_tree_nota-column3_06_header ).
    "Placa
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_07 alignment = cl_gui_column_tree=>align_left  width = 12 header_text = c_tree_nota-column3_07_header ).
    "Agente de Frete
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_08 alignment = cl_gui_column_tree=>align_left  width = 40 header_text = c_tree_nota-column3_08_header ).
    "Produto
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_09 alignment = cl_gui_column_tree=>align_left  width = 30 header_text = c_tree_nota-column3_09_header ).
    "Data de Referência
    tree_locais->add_column( EXPORTING name = c_tree_nota-column3_10 alignment = cl_gui_column_tree=>align_left  width = 20 header_text = c_tree_nota-column3_10_header ).

    tree_locais->set_registered_events(
      events =  VALUE #( ( eventid = cl_gui_column_tree=>eventid_link_click appl_event = abap_true )
                         ( eventid = cl_gui_column_tree=>eventid_node_double_click appl_event = abap_true ) ) ).

    tree_locais_event_receiver = NEW #( ).
    SET HANDLER tree_locais_event_receiver->handle_link_click FOR tree_locais.
    SET HANDLER tree_locais_event_receiver->handle_on_drag FOR tree_locais.

    tree_locais_drag = NEW #( ).
    tree_locais_drag->add( EXPORTING flavor = 'Tree_move' dragsrc = abap_true droptarget = abap_false effect = cl_dragdrop=>move ).
    tree_locais_drag->get_handle( IMPORTING handle = tree_locais_drag_handle ).

    PERFORM atualiza_objetos USING abap_true.
  ENDIF.

  SET PF-STATUS 'PF0100'.

  READ TABLE it_cargas INTO DATA(wa_carga) INDEX 1.
  SET TITLEBAR 'TL0100' WITH wa_carga-carga-ds_placa_trator wa_carga-carga-nr_ticket.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  PERFORM limpar_0100.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_0100 .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POPULA_NOTAS_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popula_notas_carga .

  DATA: node    TYPE treev_node,
        node_cg TYPE treev_node,
        node_nf TYPE treev_node,
        item    TYPE mtreeitm,
        it_node TYPE treev_nks.

  CLEAR: node_notas_table[],
         item_notas_table[],
         it_node[].

  tree_notas->delete_all_nodes( ).

  qtd_itens = 0.

  CLEAR: node.
  ADD 1 TO qtd_itens.
  node-node_key   = qtd_itens.
  CONDENSE node-node_key NO-GAPS.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = node-node_key
    IMPORTING
      output = node-node_key.

  node-hidden     = ' '. " The node is visible,
  node-disabled   = ' '. " selectable,
  node-isfolder   = abap_true. " a folder.
  node-expander   = abap_true.
  node-n_image    = icon_physical_sample.
  node-exp_image  = icon_physical_sample.
  node-dragdropid = tree_notas_drag_handle.
  APPEND node-node_key TO it_node.
  APPEND node TO node_notas_table.

  APPEND VALUE #( node_key = node-node_key tree_tipo = c_tree_tipo-tipo_all_carga ) TO it_mapa_node.

  CLEAR item.
  item-node_key  = node-node_key.
  item-item_name = c_tree_nota-column1.
  item-class     = cl_gui_list_tree=>item_font_default. " Text Item
  item-alignment = cl_gui_list_tree=>align_auto.
  item-style     = cl_gui_list_tree=>style_intensified.
  item-font      = cl_gui_list_tree=>item_font_prop.
  item-text      = |Nota(s)|.
  APPEND item TO item_notas_table.

  LOOP AT it_cargas INTO wa_carga.

    CLEAR: node_cg.
    ADD 1 TO qtd_itens.
    node_cg-node_key   = qtd_itens.
    CONDENSE node_cg-node_key NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = node_cg-node_key
      IMPORTING
        output = node_cg-node_key.

    node_cg-hidden     = ' '. " The node is visible,
    node_cg-disabled   = ' '. " selectable,
    node_cg-isfolder   = abap_true. " a folder.
    node_cg-expander   = abap_true.
    node_cg-n_image    = icon_biw_info_cube.
    node_cg-exp_image  = icon_biw_info_cube.
    node_cg-dragdropid = tree_notas_drag_handle.
    node_cg-relatkey   = node-node_key.
    node_cg-relatship  = cl_gui_column_tree=>relat_last_child.
    APPEND node_cg-node_key TO it_node.

    APPEND VALUE #( node_key = node_cg-node_key tree_tipo = c_tree_tipo-tipo_carga chave = wa_carga-carga-id_carga node_key_anterior = node_cg-relatkey ) TO it_mapa_node.

    CLEAR item.
    item-node_key  = node_cg-node_key.
    item-item_name = c_tree_nota-column1.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = |Ticket: { wa_carga-carga-nr_ticket } |.
    APPEND node_cg TO node_notas_table.
    APPEND item TO item_notas_table.

    DATA(ck_aviso_gerado) = abap_false.
    LOOP AT wa_carga-notas INTO DATA(wa_nota).
      IF wa_nota-av_vbeln IS NOT INITIAL.
        ck_aviso_gerado = abap_true.
      ENDIF.
    ENDLOOP.

    IF ck_aviso_gerado EQ abap_false.
      CLEAR item.
      item-node_key  = node_cg-node_key.
      item-item_name = c_tree_nota-column1_01.
      item-class     = cl_gui_list_tree=>item_class_link. " Text Item
      item-alignment = cl_gui_list_tree=>align_auto.
      item-style     = cl_gui_list_tree=>style_intensified.
      item-font      = cl_gui_list_tree=>item_font_prop.
      item-t_image   = icon_activity.
      APPEND item TO item_notas_table.
    ENDIF.

    LOOP AT wa_carga-notas INTO wa_nota.

      CLEAR: node_nf.

      ADD 1 TO qtd_itens.
      node_nf-node_key   = qtd_itens.
      CONDENSE node_nf-node_key NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = node_nf-node_key
        IMPORTING
          output = node_nf-node_key.

      node_nf-hidden     = ' '. " The node is visible,
      node_nf-disabled   = ' '. " selectable,
      node_nf-isfolder   = ' '. " a folder.
      node_nf-expander   = abap_false.
      node_nf-n_image    = icon_incoming_task.
      node_nf-exp_image  = icon_incoming_task.
      node_nf-dragdropid = tree_notas_drag_handle.
      node_nf-relatkey   = node_cg-node_key.
      node_nf-relatship  = cl_gui_column_tree=>relat_last_child.
      APPEND node_nf TO node_notas_table.

      APPEND VALUE #( node_key = node_nf-node_key tree_tipo = c_tree_tipo-tipo_nota chave = wa_nota-id_carga && wa_nota-id_nota node_key_anterior = node_nf-relatkey ) TO it_mapa_node.

      CLEAR item.
      item-node_key  = node_nf-node_key.
      item-item_name = c_tree_nota-column1.
      item-class     = cl_gui_list_tree=>item_font_default. " Text Item
      item-alignment = cl_gui_list_tree=>align_auto.
      item-style     = COND #( LET clet = wa_nota-av_vbeln IN WHEN clet IS INITIAL THEN cl_gui_list_tree=>style_intensified ELSE cl_gui_list_tree=>style_inactive ).
      item-font      = cl_gui_list_tree=>item_font_prop.
      item-text      = |Nota Fiscal { zcl_str=>trim( CONV #( wa_nota-nr_nota ) )->get( ) }-{ zcl_str=>trim( CONV #( wa_nota-nm_serie ) )->get( ) }|.
      APPEND item TO item_notas_table.

      IF wa_nota-av_vbeln IS INITIAL.
        CLEAR: item.
        item-node_key  = node_nf-node_key.
        item-item_name = c_tree_nota-column1_01.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_center.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-t_image   = icon_positive.
        APPEND item TO item_notas_table.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

  tree_notas->add_nodes_and_items(
    EXPORTING
      node_table                     = node_notas_table
      item_table                     = item_notas_table
      item_table_structure_name      = 'MTREEITM' ).

  tree_notas->expand_nodes( EXPORTING node_key_table = it_node ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code.
    WHEN 'INCCARGA'.
      CALL SCREEN 0200 STARTING AT 10 10.
      PERFORM atualiza_objetos_all USING abap_true.
*
*    WHEN 'AUTOMATICO'.
*
*      LOOP AT IT_CARGAS INTO WA_CARGA.
*
*      ENDLOOP.
*
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  POPULA_AVISOS_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popula_avisos_carga .

  DATA: node           TYPE treev_node,
        node_av        TYPE treev_node,
        item           TYPE mtreeitm,
        it_node_avisos TYPE treev_nks.

  CLEAR: it_node_avisos[],
         node_avisos_table[],
         item_avisos_table[].

  tree_avisos->delete_all_nodes( ).

  "Avisos"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CLEAR: it_avisos[].
  LOOP AT it_cargas INTO wa_carga.
    LOOP AT wa_carga-notas INTO DATA(wa_notas).
      APPEND wa_notas TO it_avisos.
    ENDLOOP.
  ENDLOOP.
  DELETE it_avisos WHERE av_vbeln EQ space.
  SORT it_avisos BY av_vbeln.
  DELETE ADJACENT DUPLICATES FROM it_avisos COMPARING av_vbeln.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CLEAR: node.
  ADD 1 TO qtd_itens.
  node-node_key   = qtd_itens.
  CONDENSE node-node_key NO-GAPS.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = node-node_key
    IMPORTING
      output = node-node_key.

  node-hidden     = ' '. " The node is visible,
  node-disabled   = ' '. " selectable,
  node-isfolder   = abap_true. " a folder.
  node-expander   = abap_true.
  node-n_image    = icon_delivery_inbound.
  node-exp_image  = icon_delivery_inbound.
  node-dragdropid = tree_notas_drop_handle.
  APPEND node-node_key TO it_node_avisos.

  CLEAR item.
  item-node_key  = node-node_key.
  item-item_name = c_tree_nota-column2.
  item-class     = cl_gui_list_tree=>item_font_default. " Text Item
  item-alignment = cl_gui_list_tree=>align_auto.
  item-style     = cl_gui_list_tree=>style_default.
  item-font      = cl_gui_list_tree=>item_font_prop.
  item-text      = |Aviso(s)|.
  APPEND node TO node_avisos_table.
  APPEND item TO item_avisos_table.

  LOOP AT it_avisos INTO DATA(wa_aviso).

    READ TABLE it_cargas INTO wa_carga WITH KEY carga-id_carga = wa_aviso-id_carga.

    ADD 1 TO qtd_itens.
    node_av-node_key   = qtd_itens.
    CONDENSE node_av-node_key NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = node_av-node_key
      IMPORTING
        output = node_av-node_key.

    node_av-hidden     = ' '. " The node is visible,
    node_av-disabled   = ' '. " selectable,
    node_av-isfolder   = abap_false. " a folder.
    node_av-expander   = abap_false.
    node_av-n_image    = icon_delivery_proposal.
    node_av-exp_image  = icon_delivery_proposal.
    node_av-dragdropid = tree_notas_drag_handle.
    node_av-relatkey   = node-node_key.
    node_av-relatship  = cl_gui_column_tree=>relat_last_child.
    APPEND node_av TO node_avisos_table.

    APPEND VALUE #( node_key = node_av-node_key tree_tipo = c_tree_tipo-tipo_aviso_01 chave = wa_aviso-id_carga && wa_aviso-id_nota ) TO it_mapa_node.

    CLEAR item.
    item-node_key  = node_av-node_key.
    item-item_name = c_tree_nota-column2.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = | { wa_aviso-av_vbeln } - Ticket: { wa_carga-carga-nr_ticket }|.
    APPEND item TO item_avisos_table.

    CLEAR: item.
    item-node_key  = node_av-node_key.
    item-item_name = c_tree_nota-column2_01.
    item-class     = cl_gui_list_tree=>item_class_link. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-t_image   = icon_delete.
    APPEND item TO item_avisos_table.


  ENDLOOP.

  tree_avisos->add_nodes_and_items(
    EXPORTING
      node_table                     = node_avisos_table
      item_table                     = item_avisos_table
      item_table_structure_name      = 'MTREEITM' ).

  tree_avisos->expand_nodes( EXPORTING node_key_table = it_node_avisos ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POPULA_LOCAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popula_locais .

  DATA: node          TYPE treev_node,
        node_cl       TYPE treev_node,
        node_lr       TYPE treev_node,
        item          TYPE mtreeitm,
        it_node_local TYPE treev_nks,
        wa_local      TYPE ty_entrega_coleta,
        lc_data       TYPE c LENGTH 10,
        lc_preco      TYPE c LENGTH 14.

  CLEAR: it_node_local[],
         node_locais_table[],
         item_locais_table[].

  tree_locais->delete_all_nodes( ).

  "Pontos de Coleta """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CLEAR: it_locais[].

  LOOP AT it_cargas INTO wa_carga.
    LOOP AT wa_carga-notas INTO DATA(wa_notas) WHERE av_vbeln IS NOT INITIAL.
      CLEAR: wa_local.

      SELECT SINGLE * INTO @DATA(wa_zlest0108)
        FROM zlest0108
       WHERE vbeln EQ @wa_notas-av_vbeln.

      "Buscar Parceiros do Aviso de Recebimento"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      SELECT * INTO TABLE @DATA(it_vbpa)
        FROM vbpa
       WHERE vbeln EQ @wa_notas-av_vbeln.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      READ TABLE it_vbpa INTO DATA(wa_vbpa) WITH KEY parvw = 'LR'.
      IF sy-subrc IS INITIAL.
        wa_local-id_local_descarga = wa_vbpa-kunnr.
        wa_local-ds_local_descarga = zcl_str=>initcap( i_texto = CONV #( CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_vbpa-kunnr ) )->at_kna1-name1 ) )->get(  ).
      ELSEIF wa_zlest0108-cod_loc_entrega IS NOT INITIAL.
        wa_local-id_local_descarga = wa_zlest0108-cod_loc_entrega.
        wa_local-ds_local_descarga = zcl_str=>initcap( i_texto = CONV #( CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_zlest0108-cod_loc_entrega ) )->at_kna1-name1 ) )->get(  ).
      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY parvw = 'PC'.
      IF sy-subrc IS INITIAL.
        wa_local-id_entregue_por   = wa_vbpa-lifnr.
        wa_local-ds_entregue_por   = zcl_str=>initcap( i_texto = CONV #( CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_vbpa-lifnr ) )->at_lfa1-name1 ) )->get(  ).
      ELSEIF wa_zlest0108-cod_loc_coleta IS NOT INITIAL.
        wa_local-id_entregue_por   = wa_zlest0108-cod_loc_coleta.
        wa_local-ds_entregue_por   = zcl_str=>initcap( i_texto = CONV #( CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_zlest0108-cod_loc_coleta ) )->at_lfa1-name1 ) )->get(  ).
      ENDIF.

      wa_local-po_number         = wa_notas-po_number.
      wa_local-av_vbeln          = wa_notas-av_vbeln.
      wa_local-placa_trator      = wa_carga-carga-ds_placa_trator.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY parvw = 'SP'.
      IF sy-subrc IS INITIAL.
        wa_local-id_agent_frete   = wa_vbpa-lifnr.
        wa_local-ds_agent_frete   = zcl_str=>initcap( i_texto = CONV #( CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_vbpa-lifnr ) )->at_lfa1-name1 ) )->get(  ).
      ELSEIF wa_zlest0108-agente_frete IS NOT INITIAL.
        wa_local-id_agent_frete   = wa_zlest0108-agente_frete.
        wa_local-ds_agent_frete   = zcl_str=>initcap( i_texto = CONV #( CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_zlest0108-agente_frete ) )->at_lfa1-name1 ) )->get(  ).
      ENDIF.

      wa_local-id_produto        = wa_carga-carga-id_produto.
      wa_local-ds_produto        = zcl_str=>initcap( i_texto = CONV #( wa_carga-carga-ds_produto ) )->get(  ).
      wa_local-dt_referecia      = sy-datum.
      APPEND wa_local TO it_locais.
      CLEAR: wa_zlest0108, it_vbpa[].
    ENDLOOP.
  ENDLOOP.
  DELETE it_avisos WHERE av_vbeln EQ space.
  SORT it_avisos BY av_vbeln.
  DELETE ADJACENT DUPLICATES FROM it_avisos COMPARING av_vbeln.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  SORT it_locais BY id_entregue_por id_local_descarga.
  DATA(it_locl_lr)  = it_locais[].
  it_locl_all = it_locais[].
  SORT it_locl_lr BY ds_local_descarga id_local_descarga.
  DELETE ADJACENT DUPLICATES FROM it_locl_lr COMPARING ds_local_descarga id_local_descarga.
  DELETE ADJACENT DUPLICATES FROM it_locais COMPARING id_entregue_por id_local_descarga.

  READ TABLE it_cargas INDEX 1 INTO DATA(wa_cargas).

*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio
  SELECT *
    FROM zlest0185
    INTO @DATA(wa_zlest0185)
      UP TO 1 ROWS
   WHERE id_ordem = @wa_cargas-ordem_carrega-id_ordem.
  ENDSELECT.
*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio

  LOOP AT it_locl_all ASSIGNING FIELD-SYMBOL(<fs_locais>).

    TRY .
        zcl_doc_fiscal_ft_entrada=>zif_doc_fiscal_ft_entrada~get_preco_frete_entrada(
          EXPORTING
            i_aviso                   = <fs_locais>-av_vbeln
            i_cod_loc_coleta          = <fs_locais>-id_entregue_por
            i_cod_loc_entrega         = <fs_locais>-id_local_descarga
            i_ebeln                   = <fs_locais>-po_number
            i_placa_cav               = wa_cargas-carga-ds_placa_trator
            i_agente_frete            = wa_cargas-carga-id_agent_frete
            i_material                = wa_cargas-carga-id_produto
            i_viagem_id               = wa_zlest0185-viagem_id
          IMPORTING
            e_tarifa                  = <fs_locais>-preco_frete
            e_qtd_precos_encontrados  = <fs_locais>-quantidade_precos
            e_data                    = <fs_locais>-dt_referecia
            e_message                 = <fs_locais>-message
        ).

        IF <fs_locais>-quantidade_precos EQ 1.
          <fs_locais>-i_mensagem_causa    = |Preço do Frete está cadastrado!|.
          <fs_locais>-i_system_response   = |Quando Gerado documento de custo será gerado valor de custo de frete|.
          <fs_locais>-i_mensagem_completa = |Encontrado preço de frete para as seguintes informações: { <fs_locais>-message }|.
        ELSEIF <fs_locais>-quantidade_precos GT 1.
          <fs_locais>-erro                = abap_true.
          <fs_locais>-i_mensagem_causa    = |Preço do Frete está cadastrado <b>mais de uma vez</b>!|.
          <fs_locais>-i_system_response   = |Sistema irá pegar o primeiro valor de frete encontrado, isso pode gerar problemas|.
          <fs_locais>-i_what_to_do        = |Solicitar ao Departamento de Logística, ajustar cadastro de Preço de Frete na <b>Transação TK12</b>|.
          <fs_locais>-i_sys_admin         = |Dever ser ajustado preço para somente existir um Preço cadastrado|.
          <fs_locais>-i_mensagem_completa = |Encontrado { <fs_locais>-quantidade_precos } preços; Encontrado preço de frete para as seguintes informações: { <fs_locais>-message }|.
        ELSEIF <fs_locais>-quantidade_precos EQ 0.
          <fs_locais>-erro                = abap_true.
          <fs_locais>-i_mensagem_causa    = |Preço do Frete <b>NÃO</b> está cadastrado!|.
          <fs_locais>-i_system_response   = |Quando Gerado documento de custo não será gerado valor de custo de frete|.
          <fs_locais>-i_what_to_do        = |Solicitar ao Departamento de Logística Cadastro de Preço de Frete na <b>Transação TK11</b>|.
          <fs_locais>-i_sys_admin         = |Preço do Frete deve ser Cadastrado|.
          <fs_locais>-i_mensagem_completa = |Não encontrado preço de frete para as seguintes informações: { <fs_locais>-message }|.
        ENDIF.

      CATCH zcx_doc_fiscal_ft_entrada INTO DATA(ex_entrada).

        <fs_locais>-erro = abap_true.

        IF ex_entrada->msgid = zcx_doc_fiscal_ft_entrada=>zcx_sem_tipo_pedido_transp-msgid AND
           ex_entrada->msgno = zcx_doc_fiscal_ft_entrada=>zcx_sem_tipo_pedido_transp-msgno.
          <fs_locais>-message = ex_entrada->get_longtext( ).
          <fs_locais>-i_mensagem_causa    = |Tipo de documento de compras <b>NÃO</b> está cadastrado!|.
          <fs_locais>-i_system_response   = |Sistema não encontrada o Tipo de transporte|.
          <fs_locais>-i_what_to_do        = |Solicitar ao Departamento de Tecnologia Parametrizar Tipo de documento de compras na <b>Transação ZSDT0022</b>|.
          <fs_locais>-i_sys_admin         = |Parametrizar Tipo de documento de compras|.
          <fs_locais>-i_mensagem_completa = |Erro: { ex_entrada->get_longtext( ) }|.
        ENDIF.

        IF ex_entrada->msgid = zcx_doc_fiscal_ft_entrada=>zcx_veiculo_sem_cadastro-msgid AND
           ex_entrada->msgno = zcx_doc_fiscal_ft_entrada=>zcx_veiculo_sem_cadastro-msgno.
          <fs_locais>-message = ex_entrada->get_longtext( ).
          <fs_locais>-i_mensagem_causa    = |Veículo <b>NÃO</b> está cadastrado!|.
          <fs_locais>-i_system_response   = |Sistema não encontrada o informações do Veículo|.
          <fs_locais>-i_what_to_do        = |Solicitar ao Departamento de Lógistica/Transportadora Cadastrar Veículo na <b>Transação ZLES0003</b>|.
          <fs_locais>-i_sys_admin         = |Cadastrar Veículo|.
          <fs_locais>-i_mensagem_completa = |Erro: { ex_entrada->get_longtext( ) }|.
        ENDIF.

      CATCH cx_root INTO DATA(ex_root).
        <fs_locais>-erro         = abap_true.
        <fs_locais>-message      = ex_root->get_longtext( ).
    ENDTRY.

  ENDLOOP.

  "Buscar Fretes já gerados
  SELECT * INTO TABLE @DATA(it_zlest0108)
    FROM zlest0108
     FOR ALL ENTRIES IN @it_locl_all
   WHERE vbeln EQ @it_locl_all-av_vbeln.

*-CS2021001045 - 15.02.2022 - JT - inicio
  IF sy-subrc <> 0.
    SELECT * INTO TABLE @DATA(it_zlest0211)
      FROM zlest0211
       FOR ALL ENTRIES IN @it_locl_all
     WHERE vbeln EQ @it_locl_all-av_vbeln.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_zlest0211[] TO it_zlest0108[].
    ENDIF.
  ENDIF.
*-CS2021001045 - 15.02.2022 - JT - fim

  SORT it_zlest0108 BY vbeln.

  LOOP AT it_locl_lr INTO DATA(wa_lc_entrega).

    CLEAR: node_lr.
    ADD 1 TO qtd_itens.
    node_lr-node_key   = qtd_itens.
    CONDENSE node_lr-node_key NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = node_lr-node_key
      IMPORTING
        output = node_lr-node_key.

    node_lr-hidden     = ' '. " The node is visible,
    node_lr-disabled   = ' '. " selectable,
    node_lr-isfolder   = abap_true. " a folder.
    node_lr-expander   = abap_true.
    node_lr-n_image    = icon_incoming_employee.
    node_lr-exp_image  = icon_incoming_employee.
    APPEND node_lr-node_key TO it_node_local.

    CLEAR item.
    item-node_key  = node_lr-node_key.
    item-item_name = c_tree_nota-column3.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = |{ wa_lc_entrega-id_local_descarga } - { wa_lc_entrega-ds_local_descarga }|.
    APPEND node_lr TO node_locais_table.
    APPEND item TO item_locais_table.

    LOOP AT it_locais INTO DATA(wa_locais) WHERE id_local_descarga EQ wa_lc_entrega-id_local_descarga.

      CLEAR: node_cl.
      ADD 1 TO qtd_itens.
      node_cl-node_key   = qtd_itens.
      CONDENSE node_cl-node_key NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = node_cl-node_key
        IMPORTING
          output = node_cl-node_key.

      node_cl-hidden     = ' '. " The node is visible,
      node_cl-disabled   = ' '. " selectable,
      node_cl-isfolder   = abap_true. " a folder.
      node_cl-expander   = abap_true.
      node_cl-n_image    = icon_outgoing_employee.
      node_cl-exp_image  = icon_outgoing_employee.
      node_cl-dragdropid = tree_locais_drag_handle.
      node_cl-relatkey   = node_lr-node_key.
      node_cl-relatship  = cl_gui_column_tree=>relat_last_child.
      APPEND node_cl-node_key TO it_node_local.
      APPEND VALUE #( node_key = node_cl-node_key tree_tipo = c_tree_tipo-tipo_local_00 chave = wa_lc_entrega-id_local_descarga && wa_locais-id_entregue_por node_key_anterior = node_lr-node_key ) TO it_mapa_node.

      CLEAR item.
      item-node_key  = node_cl-node_key.
      item-item_name = c_tree_nota-column3.
      item-class     = cl_gui_list_tree=>item_font_default. " Text Item
      item-alignment = cl_gui_list_tree=>align_auto.
      item-style     = cl_gui_list_tree=>style_default.
      item-font      = cl_gui_list_tree=>item_font_prop.
      item-text      = |{ wa_locais-id_entregue_por } - { wa_locais-ds_entregue_por }|.
      APPEND node_cl TO node_locais_table.
      APPEND item TO item_locais_table.

      CLEAR: item.
      item-node_key  = node_cl-node_key.
      item-item_name = c_tree_nota-column3_00.
      item-class     = cl_gui_list_tree=>item_class_link. " Text Item
      item-alignment = cl_gui_list_tree=>align_center.
      item-style     = cl_gui_list_tree=>style_default.
      item-font      = cl_gui_list_tree=>item_font_prop.
      item-t_image   = icon_positive.
      APPEND item TO item_locais_table.

      LOOP AT it_locl_all INTO DATA(wa_local_info) WHERE id_local_descarga EQ wa_locais-id_local_descarga AND id_entregue_por   EQ wa_locais-id_entregue_por.

        CLEAR: wa_zlest0108.

        READ TABLE it_zlest0108 INTO wa_zlest0108 WITH KEY vbeln = wa_local_info-av_vbeln.
        DATA(ck_possui_registro) = sy-subrc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_local_info-id_produto
          IMPORTING
            output = wa_local_info-id_produto.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_local_info-id_agent_frete
          IMPORTING
            output = wa_local_info-id_agent_frete.

        WRITE wa_local_info-dt_referecia TO lc_data.
        WRITE wa_local_info-preco_frete TO lc_preco.

        CLEAR: node.
        ADD 1 TO qtd_itens.
        node-node_key   = qtd_itens.
        CONDENSE node-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node-node_key
          IMPORTING
            output = node-node_key.

        node-hidden     = ' '. " The node is visible,
        node-disabled   = ' '. " selectable,
        node-isfolder   = abap_false. " a folder.
        node-expander   = abap_false.
        node-n_image    = icon_delivery_proposal.
        node-exp_image  = icon_delivery_proposal.
        node-relatkey   = node_cl-node_key.
        node-dragdropid = tree_locais_drag_handle.
        node-relatship  = cl_gui_column_tree=>relat_last_child.
        APPEND node TO node_locais_table.

        APPEND VALUE #( node_key = node-node_key tree_tipo = c_tree_tipo-tipo_local chave = wa_local_info-av_vbeln node_key_anterior = node-relatkey ) TO it_mapa_node.

        "Aviso
        CLEAR item.
        item-node_key  = node-node_key.
        item-item_name = c_tree_nota-column3.
        item-class     = cl_gui_list_tree=>item_font_default. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        IF ck_possui_registro IS INITIAL.
          item-style     = cl_gui_list_tree=>style_inactive.
          item-text      = |{ wa_local_info-av_vbeln } - { CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_zlest0108-agente_frete ) )->at_lfa1-name1 }|.
        ELSE.
          item-style     = cl_gui_list_tree=>style_intensified.
          item-text      = |{ wa_local_info-av_vbeln }|.
        ENDIF.
        item-font      = cl_gui_list_tree=>item_font_prop.
        APPEND item TO item_locais_table.

        "Preço
        item-item_name = c_tree_nota-column3_03.
        item-text      = |{ zcl_str=>trim_all( i_str = CONV #( lc_preco ) )->get( ) }|.
        item-style     = COND #( LET clet = wa_local_info-erro IN WHEN clet = abap_true THEN cl_gui_list_tree=>style_emphasized_negative ELSE cl_gui_list_tree=>style_emphasized_positive ).
        APPEND item TO item_locais_table.

        "Mensagem
        item-item_name = c_tree_nota-column3_04.
        item-text      = |{ wa_local_info-message }|.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-style     = cl_gui_list_tree=>style_emphasized.
        APPEND item TO item_locais_table.

        "Pedido
        item-item_name = c_tree_nota-column3_06.
        item-text      = |{ wa_local_info-po_number }|.
        item-style     = cl_gui_list_tree=>style_emphasized.
        APPEND item TO item_locais_table.

        "Placa
        item-item_name = c_tree_nota-column3_07.
        item-text      = |{ wa_local_info-placa_trator }|.
        item-style     = cl_gui_list_tree=>style_emphasized.
        APPEND item TO item_locais_table.

        "Agente de Frete
        item-item_name = c_tree_nota-column3_08.
        item-text      = |{ wa_local_info-id_agent_frete } - { wa_local_info-ds_agent_frete }|.
        item-style     = cl_gui_list_tree=>style_emphasized.
        APPEND item TO item_locais_table.

        "Produto
        item-item_name = c_tree_nota-column3_09.
        item-text      = |{ wa_local_info-id_produto } - { wa_local_info-ds_produto }|. "#EC CI_FLDEXT_OK[2215424]
        item-style     = cl_gui_list_tree=>style_emphasized.
        APPEND item TO item_locais_table.

        "Data de Referência
        item-item_name = c_tree_nota-column3_10.
        item-text      = |{ lc_data }|.
        item-style     = cl_gui_list_tree=>style_emphasized.
        APPEND item TO item_locais_table.

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

  tree_locais->add_nodes_and_items(
    EXPORTING
      node_table                = node_locais_table
      item_table                = item_locais_table
      item_table_structure_name = 'MTREEITM' ).

  tree_locais->expand_nodes( EXPORTING node_key_table = it_node_local ).

ENDFORM.

FORM atualiza_objetos_all USING ck_job TYPE char01.

  CLEAR: it_avisos[].

  LOOP AT it_cargas ASSIGNING FIELD-SYMBOL(<fs_apresentacao>).
    TRY.
        DATA(carga) = zcl_factory_carga=>zif_factory_carga~get_instance(
          )->set_factory_objeto_id( i_id_carga = <fs_apresentacao>-carga-id_carga
          )->get_factory_objeto(
          )->set_registro( i_id_carga = <fs_apresentacao>-carga-id_carga i_no_enqueue = abap_true
          ).

        carga->get_info_alv_apresentacao( IMPORTING e_apresentacao = <fs_apresentacao> ).

      CATCH zcx_carga INTO DATA(ex_carga).  "
        ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH cx_root INTO DATA(ex_root).
        MESSAGE ex_root->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDLOOP.

  PERFORM atualiza_objetos USING ck_job.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_objetos USING ck_job TYPE char01.

  qtd_itens = 0.
  CLEAR: it_mapa_node[], it_locl_all[].
  PERFORM popula_notas_carga.
  PERFORM popula_avisos_carga.
  PERFORM popula_locais.
  PERFORM popula_agrupamentos.
  IF ck_job EQ abap_true.
    PERFORM verificar_job USING abap_false.
  ENDIF.
  SORT it_mapa_node BY node_key.

ENDFORM.

FORM popula_agrupamentos.

  DATA: wa_linha          TYPE ty_agrupa,
        node              TYPE treev_node,
        node_item         TYPE treev_node,
        item              TYPE mtreeitm,
        it_node           TYPE treev_nks,
        st_ciot           TYPE RANGE OF zst_ciot,
        obj_pedafio_repom TYPE REF TO zcl_repom_viagem_vpr,
        wa_registro       TYPE zlest0123.

  CLEAR: it_aviso_fretes[], it_agrupa_fretes[], it_aviso_fretes_all[].

  IF dg_splitter_agrupa IS NOT INITIAL.

    "Matar Filhos"""""""""""""""""""""""""""""""""""

    LOOP AT it_tree_agrupa ASSIGNING FIELD-SYMBOL(<fs_tree_agrupa>).

      CLEAR: <fs_tree_agrupa>-drop.

      IF <fs_tree_agrupa>-tree IS NOT INITIAL.
        <fs_tree_agrupa>-tree->free(  ).
      ENDIF.
      CLEAR: <fs_tree_agrupa>-tree.

      IF <fs_tree_agrupa>-cont IS NOT INITIAL.
        <fs_tree_agrupa>-cont->free(  ).
      ENDIF.
      CLEAR: <fs_tree_agrupa>-cont.

      CLEAR: <fs_tree_agrupa>-event.

    ENDLOOP.

    CLEAR: it_tree_agrupa[].

    """"""""""""""""""""""""""""""""""""""""""""""""
    dg_splitter_agrupa->free(  ).
    CLEAR: dg_splitter_agrupa.


    ctl_cccontainer_frete_dc = dg_splitter_frete->get_container( row = 2 column = 1 ).

  ENDIF.

  IF it_avisos[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_todos)
      FROM zlest0108
       FOR ALL ENTRIES IN @it_avisos
     WHERE vbeln EQ @it_avisos-av_vbeln
       AND id_agrupa_frete NE @space.

    IF it_todos[] IS NOT INITIAL.
      SELECT * INTO TABLE @it_aviso_fretes
        FROM zlest0108
         FOR ALL ENTRIES IN @it_todos
       WHERE id_agrupa_frete EQ @it_todos-id_agrupa_frete.
    ENDIF.

    LOOP AT it_aviso_fretes INTO DATA(wa_).
      APPEND wa_ TO it_aviso_fretes_all.
    ENDLOOP.

    CHECK it_aviso_fretes[] IS NOT INITIAL.

    it_agrupa_fretes[] = it_aviso_fretes[].
    SORT it_agrupa_fretes BY id_agrupa_frete.
    DELETE ADJACENT DUPLICATES FROM it_agrupa_fretes COMPARING id_agrupa_frete.
    DESCRIBE TABLE it_agrupa_fretes LINES DATA(qt_linhas).

    dg_splitter_agrupa = NEW #( parent = ctl_cccontainer_frete_dc rows = 1 columns = qt_linhas ).

    DO qt_linhas TIMES.

      CLEAR: wa_linha.
      wa_linha-cont = dg_splitter_agrupa->get_container( row = 1 column = sy-index ).

      READ TABLE it_agrupa_fretes INDEX sy-index INTO DATA(wa_agrupa_fretes).

      wa_linha-tree = NEW #( parent = wa_linha-cont
                             node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
                             item_selection = abap_true
                             hierarchy_column_name = c_tree_nota-column4
                             hierarchy_header = VALUE #( width = '60' heading = c_tree_nota-column4_header ) ).

      wa_linha-tree->set_registered_events(
        events =  VALUE #( ( eventid = cl_gui_column_tree=>eventid_link_click appl_event = abap_true )
                           ( eventid = cl_gui_column_tree=>eventid_node_double_click appl_event = abap_true ) ) ).

      wa_linha-tree->add_column( EXPORTING name = c_tree_nota-column4_01 alignment = cl_gui_column_tree=>align_center width = 40 header_text = c_tree_nota-column4_01_header ).

      wa_linha-event = NEW #( ).
      SET HANDLER wa_linha-event->handle_double_click FOR wa_linha-tree.
      SET HANDLER wa_linha-event->handle_on_drop FOR wa_linha-tree.
      SET HANDLER wa_linha-event->handle_button_click FOR wa_linha-tree.
      SET HANDLER wa_linha-event->handle_link_click FOR wa_linha-tree.

      wa_linha-drop = NEW #( ).
      wa_linha-drop->add( EXPORTING flavor = 'Tree_move' dragsrc = abap_false droptarget = abap_true effect = cl_dragdrop=>move ).
      wa_linha-drop->get_handle( IMPORTING handle = wa_linha-drop_handle ).

      """""""""""""" Povoar Tree """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: node.
      ADD 1 TO qtd_itens.
      node-node_key   = qtd_itens.
      CONDENSE node-node_key NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = node-node_key
        IMPORTING
          output = node-node_key.

      CLEAR: it_node[].

      node-hidden     = ' '. " The node is visible,
      node-disabled   = ' '. " selectable,
      node-isfolder   = abap_true. " a folder.
      node-expander   = abap_true.
      node-n_image    = icon_usergroup.
      node-exp_image  = icon_usergroup.
      node-dragdropid = wa_linha-drop_handle.
      APPEND node-node_key TO it_node.
      APPEND VALUE #( node_key = node-node_key tree_tipo = c_tree_tipo-tipo_agrupa chave = wa_agrupa_fretes-id_agrupa_frete id_agrupamento = wa_agrupa_fretes-id_agrupa_frete vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

      CLEAR item.
      item-node_key  = node-node_key.
      item-item_name = c_tree_nota-column4.
      item-class     = cl_gui_list_tree=>item_font_default. " Text Item
      item-alignment = cl_gui_list_tree=>align_auto.
      item-style     = cl_gui_list_tree=>style_default.
      item-font      = cl_gui_list_tree=>item_font_prop.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_agrupa_fretes-id_agrupa_frete
        IMPORTING
          output = wa_agrupa_fretes-id_agrupa_frete.

      IF wa_agrupa_fretes-cod_loc_coleta IS NOT INITIAL.
        DATA(lc_lfa1) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(  )->set_parceiro( wa_agrupa_fretes-cod_loc_coleta ) )->at_lfa1.
        DATA(lc_coleta_texto) = zcl_str=>first_text( CONV #( lc_lfa1-name1 ) )->new_initcap(  )->get(  ).
      ELSE.
        CLEAR: lc_lfa1, lc_coleta_texto.
      ENDIF.

      IF wa_agrupa_fretes-cod_loc_entrega IS NOT INITIAL.
        DATA(lc_kna1) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance(  )->set_parceiro( wa_agrupa_fretes-cod_loc_entrega ) )->at_kna1.
        DATA(lc_entrega_texto) = zcl_str=>first_text( CONV #( lc_kna1-name1 ) )->new_initcap(  )->get(  ).
      ELSE.
        CLEAR: lc_kna1, lc_entrega_texto.
      ENDIF.

      IF wa_agrupa_fretes-nro_nf_frete IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(wa_cte)
          FROM j_1bnfe_active
         WHERE docnum EQ @wa_agrupa_fretes-nro_nf_frete.
        item-text = |{ wa_agrupa_fretes-id_agrupa_frete }->{ lc_coleta_texto }->{ lc_entrega_texto } - CT-e: { wa_cte-nfnum9 }-{ wa_cte-serie }|.
      ELSE.
        item-text = |{ wa_agrupa_fretes-id_agrupa_frete }->{ lc_coleta_texto }->{ lc_entrega_texto }|.
      ENDIF.
      APPEND node TO wa_linha-node_table.
      APPEND item TO wa_linha-item_table.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_agrupa_fretes-id_agrupa_frete
        IMPORTING
          output = wa_agrupa_fretes-id_agrupa_frete.

      CLEAR item.
      item-node_key  = node-node_key.
      item-item_name = c_tree_nota-column4_01.
      item-class     = cl_gui_list_tree=>item_class_link. " Text Item
      item-alignment = cl_gui_list_tree=>align_center.
      item-style     = cl_gui_list_tree=>style_default.
      item-font      = cl_gui_list_tree=>item_font_prop.
      IF wa_agrupa_fretes-doc_transp IS NOT INITIAL.
        item-t_image   = icon_delete.
        item-text      = 'Estornar Frete'.
      ELSE.
        item-t_image   = icon_import_all_requests.
        item-text      = 'Gerar Frete'.
      ENDIF.
      APPEND item TO wa_linha-item_table.

      "Remessas
      LOOP AT it_aviso_fretes INTO DATA(wa_aviso_fretes) WHERE id_agrupa_frete = wa_agrupa_fretes-id_agrupa_frete.

        CLEAR: node_item.
        ADD 1 TO qtd_itens.
        node_item-node_key   = qtd_itens.
        CONDENSE node_item-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node_item-node_key
          IMPORTING
            output = node_item-node_key.

        node_item-hidden     = ' '. " The node is visible,
        node_item-disabled   = ' '. " selectable,
        node_item-isfolder   = abap_false. " a folder.
        node_item-expander   = abap_false.
        node_item-n_image    = icon_order.
        node_item-exp_image  = icon_order.
        node_item-relatkey   = node-node_key.
        node_item-relatship  = cl_gui_column_tree=>relat_last_child.
        APPEND node_item TO wa_linha-node_table.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = |{ wa_aviso_fretes-vbeln } - Aviso de Recebimento|.
        APPEND item TO wa_linha-item_table.
        APPEND VALUE #(
            node_key = node_item-node_key
            tree_tipo = c_tree_tipo-tipo_agrupa_01
            chave = wa_aviso_fretes-vbeln
            node_key_anterior = node-node_key
            id_agrupamento = wa_aviso_fretes-id_agrupa_frete
            vbeln = wa_aviso_fretes-vbeln ) TO it_mapa_node.

        IF wa_agrupa_fretes-doc_transp IS INITIAL.
          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4_01.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_center.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-t_image   = icon_delete.
          APPEND item TO wa_linha-item_table.
        ENDIF.

      ENDLOOP.

      "Documento de Transporte
      IF wa_agrupa_fretes-doc_transp IS NOT INITIAL.

        CLEAR: node_item.
        ADD 1 TO qtd_itens.
        node_item-node_key   = qtd_itens.
        CONDENSE node_item-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node_item-node_key
          IMPORTING
            output = node_item-node_key.

        node_item-hidden     = ' '. " The node is visible,
        node_item-disabled   = ' '. " selectable,
        node_item-isfolder   = abap_false. " a folder.
        node_item-expander   = abap_false.
        node_item-n_image    = icon_order.
        node_item-exp_image  = icon_order.
        node_item-relatkey   = node-node_key.
        node_item-relatship  = cl_gui_column_tree=>relat_last_child.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = |{ wa_agrupa_fretes-doc_transp } - Documento de Transporte|.
        APPEND node_item TO wa_linha-node_table.
        APPEND item TO wa_linha-item_table.
        APPEND VALUE #(
            node_key = node_item-node_key
            tree_tipo = c_tree_tipo-tipo_agrupa_02
            chave = wa_agrupa_fretes-doc_transp
            node_key_anterior = node-node_key
            id_agrupamento = wa_aviso_fretes-id_agrupa_frete
            vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

        "Pedágio
        CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
          EXPORTING
            i_tknum           = wa_agrupa_fretes-doc_transp
          RECEIVING
            e_id_proc_cliente = DATA(lc_id_proc_cliente)
          EXCEPTIONS
            nao_encontrado    = 1
            OTHERS            = 2.

        IF sy-subrc IS INITIAL.
          obj_pedafio_repom = NEW #( i_id_proc_cliente = lc_id_proc_cliente ).
          obj_pedafio_repom->zif_cadastro~get_registro( IMPORTING e_registro = wa_registro ).
          CLEAR: obj_pedafio_repom.

          CLEAR: node_item.
          ADD 1 TO qtd_itens.
          node_item-node_key   = qtd_itens.
          CONDENSE node_item-node_key NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = node_item-node_key
            IMPORTING
              output = node_item-node_key.

          node_item-hidden     = ' '. " The node is visible,
          node_item-disabled   = ' '. " selectable,
          node_item-isfolder   = abap_false. " a folder.
          node_item-expander   = abap_false.
          node_item-n_image    = icon_order.
          node_item-exp_image  = icon_order.
          node_item-relatkey   = node-node_key.
          node_item-relatship  = cl_gui_column_tree=>relat_last_child.

          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_auto.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-text      = |{ wa_registro-id_viagem_codigo } - Pedágio|.
          APPEND node_item TO wa_linha-node_table.
          APPEND item TO wa_linha-item_table.
          APPEND VALUE #(
              node_key = node_item-node_key
              tree_tipo = c_tree_tipo-tipo_agrupa_03a
              chave = wa_agrupa_fretes-doc_transp
              node_key_anterior = node-node_key
              id_agrupamento = wa_aviso_fretes-id_agrupa_frete
              vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4_01.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_center.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-t_image   = icon_pdf.
          APPEND item TO wa_linha-item_table.

        ENDIF.

      ENDIF.

      "Documento de Custo
      IF wa_agrupa_fretes-fknum IS NOT INITIAL.

        CLEAR: node_item.
        ADD 1 TO qtd_itens.
        node_item-node_key   = qtd_itens.
        CONDENSE node_item-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node_item-node_key
          IMPORTING
            output = node_item-node_key.

        node_item-hidden     = ' '. " The node is visible,
        node_item-disabled   = ' '. " selectable,
        node_item-isfolder   = abap_false. " a folder.
        node_item-expander   = abap_false.
        node_item-n_image    = icon_order.
        node_item-exp_image  = icon_order.
        node_item-relatkey   = node-node_key.
        node_item-relatship  = cl_gui_column_tree=>relat_last_child.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = |{ wa_agrupa_fretes-fknum } - Documento de Custo|.
        APPEND node_item TO wa_linha-node_table.
        APPEND item TO wa_linha-item_table.
        APPEND VALUE #(
            node_key = node_item-node_key
            tree_tipo = c_tree_tipo-tipo_agrupa_03
            chave = wa_agrupa_fretes-fknum
            node_key_anterior = node-node_key
            id_agrupamento = wa_aviso_fretes-id_agrupa_frete
            vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

      ENDIF.

      "Ordem de Venda
      IF wa_agrupa_fretes-ov_frete IS NOT INITIAL.

        CLEAR: node_item.
        ADD 1 TO qtd_itens.
        node_item-node_key   = qtd_itens.
        CONDENSE node_item-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node_item-node_key
          IMPORTING
            output = node_item-node_key.

        node_item-hidden     = ' '. " The node is visible,
        node_item-disabled   = ' '. " selectable,
        node_item-isfolder   = abap_false. " a folder.
        node_item-expander   = abap_false.
        node_item-n_image    = icon_order.
        node_item-exp_image  = icon_order.
        node_item-relatkey   = node-node_key.
        node_item-relatship  = cl_gui_column_tree=>relat_last_child.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = |{ wa_agrupa_fretes-ov_frete } - Ordem de Venda|.
        APPEND node_item TO wa_linha-node_table.
        APPEND item TO wa_linha-item_table.
        APPEND VALUE #(
            node_key = node_item-node_key
            tree_tipo = c_tree_tipo-tipo_agrupa_04
            chave = wa_agrupa_fretes-ov_frete
            node_key_anterior = node-node_key
            id_agrupamento = wa_aviso_fretes-id_agrupa_frete
            vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

      ENDIF.

      "Fatura
      IF wa_agrupa_fretes-fatura_frete IS NOT INITIAL.

        CLEAR: node_item.
        ADD 1 TO qtd_itens.
        node_item-node_key   = qtd_itens.
        CONDENSE node_item-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node_item-node_key
          IMPORTING
            output = node_item-node_key.

        node_item-hidden     = ' '. " The node is visible,
        node_item-disabled   = ' '. " selectable,
        node_item-isfolder   = abap_false. " a folder.
        node_item-expander   = abap_false.
        node_item-n_image    = icon_order.
        node_item-exp_image  = icon_order.
        node_item-relatkey   = node-node_key.
        node_item-relatship  = cl_gui_column_tree=>relat_last_child.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = |{ wa_agrupa_fretes-fatura_frete } - Fatura|.
        APPEND node_item TO wa_linha-node_table.
        APPEND item TO wa_linha-item_table.
        APPEND VALUE #(
            node_key = node_item-node_key
            tree_tipo = c_tree_tipo-tipo_agrupa_05
            chave = wa_agrupa_fretes-fatura_frete
            node_key_anterior = node-node_key
            id_agrupamento = wa_aviso_fretes-id_agrupa_frete
            vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

      ENDIF.

      "CT-e
      IF wa_agrupa_fretes-nro_nf_frete IS NOT INITIAL.

        "Contrato TipFrete
        st_ciot = VALUE #( sign = 'I' option = 'EQ' ( low = '2' high = '2' ) ( low = '5' high = '5' ) ( low = '6' high = '6' ) ).
        SELECT SINGLE * INTO @DATA(wa_contrato)
          FROM zcte_ciot
         WHERE docnum  EQ @wa_agrupa_fretes-nro_nf_frete
           AND st_ciot IN @st_ciot.

        IF sy-subrc IS INITIAL.

          IF wa_contrato-nucontrato IS NOT INITIAL.
            CLEAR: node_item.
            ADD 1 TO qtd_itens.
            node_item-node_key   = qtd_itens.
            CONDENSE node_item-node_key NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = node_item-node_key
              IMPORTING
                output = node_item-node_key.

            node_item-hidden     = ' '. " The node is visible,
            node_item-disabled   = ' '. " selectable,
            node_item-isfolder   = abap_false. " a folder.
            node_item-expander   = abap_false.
            node_item-n_image    = icon_order.
            node_item-exp_image  = icon_order.
            node_item-relatkey   = node-node_key.
            node_item-relatship  = cl_gui_column_tree=>relat_last_child.

            CLEAR item.
            item-node_key  = node_item-node_key.
            item-item_name = c_tree_nota-column4.
            item-class     = cl_gui_list_tree=>item_class_link. " Text Item
            item-alignment = cl_gui_list_tree=>align_auto.
            item-style     = cl_gui_list_tree=>style_default.
            item-font      = cl_gui_list_tree=>item_font_prop.
            item-text      = |{ wa_contrato-nucontrato } - Contrato TipFrete|.
            APPEND node_item TO wa_linha-node_table.
            APPEND item TO wa_linha-item_table.
            APPEND VALUE #(
                node_key = node_item-node_key
                tree_tipo = c_tree_tipo-tipo_agrupa_06a
                chave = wa_agrupa_fretes-nro_nf_frete
                node_key_anterior = node-node_key
                id_agrupamento = wa_aviso_fretes-id_agrupa_frete
                vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

            CLEAR item.
            item-node_key  = node_item-node_key.
            item-item_name = c_tree_nota-column4_01.
            item-class     = cl_gui_list_tree=>item_class_link. " Text Item
            item-alignment = cl_gui_list_tree=>align_center.
            item-style     = cl_gui_list_tree=>style_default.
            item-font      = cl_gui_list_tree=>item_font_prop.
            item-t_image   = icon_pdf.
            APPEND item TO wa_linha-item_table.

          ENDIF.

          IF wa_contrato-nr_ciot IS NOT INITIAL.
            CLEAR: node_item.
            ADD 1 TO qtd_itens.
            node_item-node_key   = qtd_itens.
            CONDENSE node_item-node_key NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = node_item-node_key
              IMPORTING
                output = node_item-node_key.

            node_item-hidden     = ' '. " The node is visible,
            node_item-disabled   = ' '. " selectable,
            node_item-isfolder   = abap_false. " a folder.
            node_item-expander   = abap_false.
            node_item-n_image    = icon_order.
            node_item-exp_image  = icon_order.
            node_item-relatkey   = node-node_key.
            node_item-relatship  = cl_gui_column_tree=>relat_last_child.

            CLEAR item.
            item-node_key  = node_item-node_key.
            item-item_name = c_tree_nota-column4.
            item-class     = cl_gui_list_tree=>item_class_link. " Text Item
            item-alignment = cl_gui_list_tree=>align_auto.
            item-style     = cl_gui_list_tree=>style_default.
            item-font      = cl_gui_list_tree=>item_font_prop.
            item-text      = |{ wa_contrato-nr_ciot } - CIOT ANTT|.
            APPEND node_item TO wa_linha-node_table.
            APPEND item TO wa_linha-item_table.
            APPEND VALUE #(
                node_key = node_item-node_key
                tree_tipo = c_tree_tipo-tipo_agrupa_06b
                chave = wa_agrupa_fretes-nro_nf_frete
                node_key_anterior = node-node_key
                id_agrupamento = wa_aviso_fretes-id_agrupa_frete
                vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.
          ENDIF.

        ENDIF.

        CLEAR: node_item.
        ADD 1 TO qtd_itens.
        node_item-node_key   = qtd_itens.
        CONDENSE node_item-node_key NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = node_item-node_key
          IMPORTING
            output = node_item-node_key.

        node_item-hidden     = ' '. " The node is visible,
        node_item-disabled   = ' '. " selectable,
        node_item-isfolder   = abap_false. " a folder.
        node_item-expander   = abap_false.
        node_item-n_image    = icon_order.
        node_item-exp_image  = icon_order.
        node_item-relatkey   = node-node_key.
        node_item-relatship  = cl_gui_column_tree=>relat_last_child.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_auto.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-text      = |{ wa_agrupa_fretes-nro_nf_frete } - CT-e|.
        APPEND node_item TO wa_linha-node_table.
        APPEND item TO wa_linha-item_table.
        APPEND VALUE #(
            node_key = node_item-node_key
            tree_tipo = c_tree_tipo-tipo_agrupa_06
            chave = wa_agrupa_fretes-nro_nf_frete
            node_key_anterior = node-node_key
            id_agrupamento = wa_aviso_fretes-id_agrupa_frete
            vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

        CLEAR item.
        item-node_key  = node_item-node_key.
        item-item_name = c_tree_nota-column4_01.
        item-class     = cl_gui_list_tree=>item_class_link. " Text Item
        item-alignment = cl_gui_list_tree=>align_center.
        item-style     = cl_gui_list_tree=>style_default.
        item-font      = cl_gui_list_tree=>item_font_prop.
        item-t_image   = icon_pdf.
        APPEND item TO wa_linha-item_table.

        SELECT SINGLE vc~docnum_ref, vc~docnum INTO @DATA(wa_mdfe)
          FROM zsdt0105 AS vc
         INNER JOIN zsdt0102 AS mf ON mf~docnum EQ vc~docnum_ref AND mf~estornado EQ @abap_false
         WHERE vc~docnum EQ @wa_agrupa_fretes-nro_nf_frete
           AND EXISTS ( SELECT * FROM zsdt0102 AS md WHERE md~nmdfe EQ vc~nmdfe AND md~docnum EQ vc~docnum_ref AND md~estornado EQ @abap_false ).

        IF sy-subrc IS INITIAL.
          "'SMD -> "MDFE
          CLEAR: node_item.
          ADD 1 TO qtd_itens.
          node_item-node_key   = qtd_itens.
          CONDENSE node_item-node_key NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = node_item-node_key
            IMPORTING
              output = node_item-node_key.

          node_item-hidden     = ' '. " The node is visible,
          node_item-disabled   = ' '. " selectable,
          node_item-isfolder   = abap_false. " a folder.
          node_item-expander   = abap_false.
          node_item-n_image    = icon_order.
          node_item-exp_image  = icon_order.
          node_item-relatkey   = node-node_key.
          node_item-relatship  = cl_gui_column_tree=>relat_last_child.

          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_auto.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-text      = |{ wa_mdfe-docnum_ref } - MDFe|.
          APPEND node_item TO wa_linha-node_table.
          APPEND item TO wa_linha-item_table.
          APPEND VALUE #(
              node_key = node_item-node_key
              tree_tipo = c_tree_tipo-tipo_agrupa_07
              chave = wa_mdfe-docnum_ref
              node_key_anterior = node-node_key
              id_agrupamento = wa_aviso_fretes-id_agrupa_frete
              vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4_01.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_center.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-t_image   = icon_pdf.
          APPEND item TO wa_linha-item_table.

        ENDIF.

      ELSEIF wa_agrupa_fretes-nro_nf_mdfe IS NOT INITIAL.

        SELECT SINGLE mf~nmdfe, mf~docnum INTO @DATA(wa_mdfe_2)
          FROM zsdt0102 AS mf
         WHERE mf~docnum EQ @wa_agrupa_fretes-nro_nf_mdfe
           AND mf~estornado EQ @abap_false.

        IF sy-subrc IS INITIAL.
          "'SMD -> "MDFE
          CLEAR: node_item.
          ADD 1 TO qtd_itens.
          node_item-node_key   = qtd_itens.
          CONDENSE node_item-node_key NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = node_item-node_key
            IMPORTING
              output = node_item-node_key.

          node_item-hidden     = ' '. " The node is visible,
          node_item-disabled   = ' '. " selectable,
          node_item-isfolder   = abap_false. " a folder.
          node_item-expander   = abap_false.
          node_item-n_image    = icon_order.
          node_item-exp_image  = icon_order.
          node_item-relatkey   = node-node_key.
          node_item-relatship  = cl_gui_column_tree=>relat_last_child.

          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_auto.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-text      = |{ wa_mdfe_2-docnum } - MDFe|.
          APPEND node_item TO wa_linha-node_table.
          APPEND item TO wa_linha-item_table.
          APPEND VALUE #(
              node_key = node_item-node_key
              tree_tipo = c_tree_tipo-tipo_agrupa_07
              chave = wa_mdfe_2-docnum
              node_key_anterior = node-node_key
              id_agrupamento = wa_aviso_fretes-id_agrupa_frete
              vbeln = wa_agrupa_fretes-vbeln ) TO it_mapa_node.

          CLEAR item.
          item-node_key  = node_item-node_key.
          item-item_name = c_tree_nota-column4_01.
          item-class     = cl_gui_list_tree=>item_class_link. " Text Item
          item-alignment = cl_gui_list_tree=>align_center.
          item-style     = cl_gui_list_tree=>style_default.
          item-font      = cl_gui_list_tree=>item_font_prop.
          item-t_image   = icon_pdf.
          APPEND item TO wa_linha-item_table.

        ENDIF.

      ENDIF.

      APPEND wa_linha TO it_tree_agrupa.

      wa_linha-tree->add_nodes_and_items(
          EXPORTING
            node_table                = wa_linha-node_table
            item_table                = wa_linha-item_table
            item_table_structure_name = 'MTREEITM' ).

      wa_linha-tree->expand_nodes( EXPORTING node_key_table = it_node ).

    ENDDO.

  ELSE.
    "Painel Sem Agrupamento """"""""""""""""""""""""""""""



  ENDIF.

ENDFORM.

INCLUDE zmmr155_user_command_0200i01.

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_PEDAGIO_REPOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprimir_pedagio_repom USING p_id_proc_cliente TYPE  zde_id_proc_cliente.

  IF p_id_proc_cliente IS NOT INITIAL.

    zcl_repom_viagem_vpr=>imprimir_viagem(
      EXPORTING
        i_id_proc_cliente = p_id_proc_cliente
      EXCEPTIONS
        erro              = 1
        OTHERS            = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " IMPRIMIR_PEDAGIO_REPOM

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM imprimir_doc_fiscal  USING p_fiscal TYPE j_1bdocnum.

  CHECK p_fiscal IS NOT INITIAL.

  CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
    EXPORTING
      doc_numero     = p_fiscal
    EXCEPTIONS
      nao_localizado = 1
      OTHERS         = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_CTR_VIAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM imprimir_ctr_viagem USING p_fiscal TYPE j_1bdocnum.

  CHECK p_fiscal IS NOT INITIAL.

  CALL FUNCTION 'Z_SD_IMPRIMIR_CTR_CIOT'
    EXPORTING
      p_cte_avulso = p_fiscal
    EXCEPTIONS
      nao_ciot     = 1
      erro_status  = 2
      error        = 3
      OTHERS       = 4.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " IMPRIMIR_CTR_VIAGEM

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_aviso USING p_vbeln TYPE vbeln_vl.

  IF p_vbeln IS NOT INITIAL.
    SET PARAMETER ID 'VL'  FIELD p_vbeln.
    SET PARAMETER ID 'VLM' FIELD p_vbeln.
    CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_AVISO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA_VF
*&---------------------------------------------------------------------*
FORM mostrar_fatura_vf USING p_vbeln TYPE vbeln_vf.

  IF p_vbeln IS NOT INITIAL.
    SET PARAMETER ID 'VF'    FIELD p_vbeln.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_FATURA_VF

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_doc_transporte USING p_tknum TYPE tknum.

  IF p_tknum IS NOT INITIAL.
    SET PARAMETER ID 'TNR' FIELD p_tknum.
    CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_TRANSPORTE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_PEDAGIO_REPOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_pedagio_repom USING p_id_proc_cliente TYPE  zde_id_proc_cliente.

  IF p_id_proc_cliente IS NOT INITIAL.
    CALL FUNCTION 'Z_REPOM_CADASTRO_PEDAGIO'
      EXPORTING
        i_consulta        = abap_true
        i_id_proc_cliente = p_id_proc_cliente.
  ENDIF.

ENDFORM.                    " MOSTRAR_PEDAGIO_REPOM

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_doc_custo USING p_fknum TYPE fknum.

  IF p_fknum IS NOT INITIAL.
    SET PARAMETER ID 'FKK'    FIELD p_fknum.
    CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_CUSTO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ORDEM_VENDA
*&---------------------------------------------------------------------*
FORM mostrar_ordem_venda USING p_vbeln TYPE vbeln_va.
  zcl_ordem_venda=>zif_ordem_venda~open( p_vbeln ).
ENDFORM.                    " MOSTRAR_DOC_CUSTO

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_MDFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NMDFE  text
*      -->P_P_FISCAL  text
*----------------------------------------------------------------------*
FORM imprimir_mdfe  USING  p_nmdfe  TYPE j_1bnfnum9
                           p_fiscal TYPE j_1bdocnum.

  DATA: lc_mdfe TYPE REF TO zcl_mdfe.

  "CHECK P_NMDFE IS NOT INITIAL.
  CHECK p_fiscal IS NOT INITIAL.

  CREATE OBJECT lc_mdfe
    EXPORTING
      i_nmdfe  = p_nmdfe
      i_docnum = p_fiscal.

  lc_mdfe->print_mdfe( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM mostrar_monitor_eletronico  USING p_fiscal TYPE j_1bdocnum .

  DATA: gf_nfobjn LIKE j_1binterf-nfobjn.

  CHECK p_fiscal IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
    FROM j_1bnfdoc
   WHERE docnum EQ @p_fiscal.

  SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_j_1bnfdoc-docnum.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_j_1bnfdoc-bukrs.

  IF wa_j_1bnfdoc-form IS NOT INITIAL.
    CASE wa_j_1bnfdoc-model.
      WHEN '55'.
        CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
      WHEN '57'.
        CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
      WHEN '58'.
        CALL TRANSACTION 'ZMDFE' AND SKIP FIRST SCREEN.
    ENDCASE.
  ELSE.
    CASE wa_j_1bnfdoc-model.
      WHEN '55'.
        CALL TRANSACTION 'ZNFE_TERC' AND SKIP FIRST SCREEN.
      WHEN '57'.
        CALL TRANSACTION 'ZCTE_TERC' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

FORM emitir_frete_entrada_carga USING p_id_carga TYPE zde_id_carga.
  SUBMIT zmmr155 WITH pidcarga EQ p_id_carga AND RETURN.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_AUTOMATICO_CARGA_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MAPA_NODE_CHAVE(10)  text
*----------------------------------------------------------------------*
FORM gerar_automatico_carga_entrada USING p_id_carga TYPE zde_id_carga.

  DATA: number           TYPE tbtcjob-jobcount,
        name             TYPE tbtcjob-jobname,
        print_parameters TYPE pri_params.

  PERFORM faturamento_9011 USING p_id_carga.

  CHECK ck_dado_frete_9011_ok EQ abap_true.
  CLEAR: ck_dado_frete_9011_ok.

  IF 1 = 2.
    SUBMIT zmmr155 WITH pidcarga EQ p_id_carga WITH pautomat EQ abap_true AND RETURN.
  ELSE.

    DATA(lc_user_job) = zcl_job=>get_user_job( ).
    CONCATENATE 'JOB_FRETE_ENTRADA' p_id_carga INTO name SEPARATED BY '_'.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
      IMPORTING
        jobcount         = number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.

      SUBMIT zmmr155
          TO SAP-SPOOL SPOOL PARAMETERS print_parameters
        WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
        WITH pidcarga EQ p_id_carga
        WITH pautomat EQ abap_true
        USER lc_user_job
         AND RETURN.

      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = number
            jobname              = name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.

        IF sy-subrc IS NOT INITIAL.
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              jobcount                 = number
              jobname                  = name
            EXCEPTIONS
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              OTHERS                   = 18.
        ELSE.
          PERFORM atualiza_objetos_all USING abap_true.
        ENDIF.

      ELSE.
        CALL FUNCTION 'BP_JOB_DELETE'
          EXPORTING
            jobcount                 = number
            jobname                  = name
          EXCEPTIONS
            cant_delete_event_entry  = 1
            cant_delete_job          = 2
            cant_delete_joblog       = 3
            cant_delete_steps        = 4
            cant_delete_time_entry   = 5
            cant_derelease_successor = 6
            cant_enq_predecessor     = 7
            cant_enq_successor       = 8
            cant_enq_tbtco_entry     = 9
            cant_update_predecessor  = 10
            cant_update_successor    = 11
            commit_failed            = 12
            jobcount_missing         = 13
            jobname_missing          = 14
            job_does_not_exist       = 15
            job_is_already_running   = 16
            no_delete_authority      = 17
            OTHERS                   = 18.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verificar_job USING ck_all TYPE char01.

  DATA: name TYPE tbtcjob-jobname.
  DATA: name_await TYPE tbtcjob-jobname.

  DATA(ck_aguardar_job) = abap_false.
  CLEAR: name_await.

  LOOP AT it_cargas INTO DATA(lc_carga).

    CONCATENATE 'JOB_FRETE_ENTRADA' lc_carga-carga-id_carga INTO name SEPARATED BY '_'.

    TRY .
        zcl_job=>get_instance( )->get_job_execucao( i_job_name = name ).
        ck_aguardar_job = abap_true.
        name_await      = name.
      CATCH zcx_job.    "
        TRY .
            zcl_job=>get_instance( )->get_job_escalonado( i_job_name = name ).
            ck_aguardar_job = abap_true.
            name_await      = name.
          CATCH zcx_job.    "
        ENDTRY.
    ENDTRY.

  ENDLOOP.

  IF ck_aguardar_job IS NOT INITIAL.
    PERFORM ativar_time.
  ELSEIF ck_all EQ abap_true.
    PERFORM atualiza_objetos_all USING abap_false.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATIVAR_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME_AWAIT  text
*----------------------------------------------------------------------*
FORM ativar_time.

  IF go_clock IS INITIAL.
    go_clock = NEW #( ).
  ENDIF.

  IF go_alarm IS INITIAL.
    go_alarm = NEW #( ).
    SET HANDLER go_alarm->on_finished FOR go_clock.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = 'Aguardar Job Retornar de Processo Solicitado'.

  "PERFORM ATUALIZA_OBJETOS_ALL USING ABAP_FALSE.

  go_clock->interval = c_time_interval.
  go_clock->run( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FATURAMENTO_9011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM faturamento_9011 USING p_id_carga TYPE zde_id_carga.

  ck_dado_frete_9011_ok = abap_false.
  ck_dado_frete_9011_cs = abap_false.
  lc_tem_margem_adiant  = abap_false.
  lc_tem_margem_zero    = abap_false.
  ck_alterou_pedagio    = abap_false.
  ck_alterou_itinerario = abap_false.

  TRY .
      zcl_factory_carga=>zif_factory_carga~get_instance(
                                    )->set_factory_objeto_id( i_id_carga = p_id_carga
                                    )->get_factory_objeto(
                                    )->set_registro( i_id_carga = p_id_carga i_no_enqueue = abap_true
                                    )->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(info_carga)
                                    ).

    CATCH zcx_ordem_carregamento INTO DATA(ex_ordem).    "
      ex_ordem->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
    CATCH zcx_carga INTO DATA(ex_carga).  "
      ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      EXIT.
  ENDTRY.

  LOOP AT info_carga-notas INTO DATA(wa_nota).
    IF wa_nota-av_vbeln IS NOT INITIAL.
      ck_dado_frete_9011_cs = abap_true.
    ENDIF.
  ENDLOOP.

  SELECT SINGLE * INTO @DATA(wa_zsdt0001fe)
    FROM zsdt0001fe
   WHERE id_carga EQ @p_id_carga.

  zsdt0001fe-id_carga = p_id_carga.

  IF ck_dado_frete_9011_cs EQ abap_true.
    MOVE-CORRESPONDING wa_zsdt0001fe TO zde_info_frete.
    ck_alterou_pedagio = abap_false.
  ELSE.
    IF wa_zsdt0001fe IS NOT INITIAL AND wa_zsdt0001fe-id_agent_frete EQ info_carga-carga-id_agent_frete.
      MOVE-CORRESPONDING wa_zsdt0001fe TO zde_info_frete.
      ck_alterou_pedagio = abap_false.
    ELSE.

      READ TABLE info_carga-notas INDEX 1 INTO wa_nota.
      IF wa_nota-id_entregue_por IS NOT INITIAL.
        DATA(id_local_coleta) = wa_nota-id_entregue_por.
      ELSE.
        id_local_coleta = wa_nota-id_fornecedor.
      ENDIF.

      DATA: lc_quantidade TYPE lfimg.
      lc_quantidade = 0.

      LOOP AT info_carga-notas INTO DATA(wa_nota_1).

        ADD wa_nota_1-nr_quantidade TO lc_quantidade.

        IF wa_nota-id_entregue_por IS NOT INITIAL.
          DATA(id_local_coleta_1) = wa_nota-id_entregue_por.
        ELSE.
          id_local_coleta_1 = wa_nota-id_fornecedor.
        ENDIF.

        IF id_local_coleta NE id_local_coleta_1.
          MESSAGE s291 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDLOOP.

      zde_info_frete-id_local_descarga = info_carga-carga-id_branch.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = zde_info_frete-id_local_descarga
        IMPORTING
          output = zde_info_frete-id_local_descarga.

      DATA(local_descarga) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = zde_info_frete-id_local_descarga )->get_name( ) )->at_kna1.
      DATA(local_coleta)   = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = id_local_coleta )->get_name( ) )->at_lfa1.
      zde_info_frete-ds_local_descarga = local_descarga-name1.
      zde_info_frete-id_local_coleta   = id_local_coleta.
      zde_info_frete-ds_local_coleta   = local_coleta-name1.
      zde_info_frete-ds_placa_trator   = info_carga-carga-ds_placa_trator.
      zde_info_frete-id_proprietario   = info_carga-carga-id_proprietario.
      zde_info_frete-ds_proprietario   = info_carga-carga-ds_proprietario.
      zde_info_frete-ds_placa_reboq_1  = info_carga-carga-ds_placa_reboq_1.
      zde_info_frete-ds_placa_reboq_2  = info_carga-carga-ds_placa_reboq_2.
      zde_info_frete-ds_placa_reboq_3  = info_carga-carga-ds_placa_reboq_3.
      zde_info_frete-id_motorista      = info_carga-carga-id_motorista.
      zde_info_frete-ds_motorista      = info_carga-carga-ds_motorista.
      zde_info_frete-id_bukrs          = info_carga-carga-id_bukrs.
      zde_info_frete-id_branch         = info_carga-carga-id_branch.
      zde_info_frete-shtyp             = 'Z021'.
      zde_info_frete-tp_admim_frete    = '09'.
      zde_info_frete-lzonea            = local_descarga-lzone.
      zde_info_frete-lzonez            = local_coleta-lzone.
      zde_info_frete-matnr             = info_carga-carga-id_produto.
      zde_info_frete-id_agent_frete    = info_carga-carga-id_agent_frete.
      zde_info_frete-cd_cid_origem     = local_coleta-txjcd.
      zde_info_frete-cd_cid_destino    = local_descarga-txjcd.

      SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
        FROM j_1bbranch
       WHERE branch EQ @info_carga-carga-id_agent_frete+6(4).

      IF sy-subrc IS INITIAL.
        zde_info_frete-ag_bukrs  = wa_j_1bbranch-bukrs.
        zde_info_frete-ag_branch = wa_j_1bbranch-branch.
      ENDIF.

*-CS2019001158 - Jaime Tassoni - 19.11.2020 - inicio
      SELECT *
        FROM zlest0185
        INTO @DATA(wa_zlest0185)
          UP TO 1 ROWS
       WHERE id_ordem = @info_carga-ordem_carrega-id_ordem.
      ENDSELECT.
*-CS2019001158 - Jaime Tassoni - 19.11.2020 - fim

      TRY .

          zcl_itinerario=>zif_itinerario~get_instance(
            )->get_itinerario_relevante(
            EXPORTING
              i_cod_loc_coleta  = id_local_coleta
              i_cod_loc_entrega = zde_info_frete-id_local_descarga
            IMPORTING
              e_tvro            = DATA(e_tvro) ).

          zde_info_frete-route = e_tvro-route.

        CATCH zcx_itinerario INTO DATA(ex_itinerario).    " .
          MESSAGE s001(zitinerario) WITH id_local_coleta zde_info_frete-id_local_descarga.
          EXIT.
      ENDTRY.

      zcl_calc_frete=>get_valor_frete(
        EXPORTING
          i_route        = zde_info_frete-route           " Itinerário
          i_tdlnr        = zde_info_frete-id_agent_frete  " Nº do agente de frete
          i_shtyp        = zde_info_frete-shtyp           " Tipo de transporte
          i_lzonea       = zde_info_frete-lzonea          " Zona de partida  // PRODUTOR
          i_lzonez       = zde_info_frete-lzonez      " Zona de chegada  // FILIAL
          i_add01        = zde_info_frete-add01       " Suplem.1
          i_matnr        = zde_info_frete-matnr       " Nº do material
          i_placa_trator = zde_info_frete-ds_placa_trator " Placa Veículo Tração
          i_frete_entrada = abap_true
          i_viagem_id     = wa_zlest0185-viagem_id
        IMPORTING
          e_kbetr        = zde_info_frete-kbetr " Montante/porcentagem de condição no caso de não haver escala
          e_konwa        = zde_info_frete-konwa " Unidade de condição (moeda ou porcentagem)
          e_krech        = zde_info_frete-krech " Regra de cálculo de condição
          e_lzonea       = zde_info_frete-lzonea
          e_lzonez       = zde_info_frete-lzonez
          e_route        = zde_info_frete-route
        ).

      TRY .

          TRY .
              zcl_fornecedores=>zif_parceiros~get_instance(
                )->set_parceiro( i_parceiro = info_carga-carga-id_agent_frete
                )->ck_parceiro_local_negocio(
                ).

              DATA(r_margadto) = zcl_calc_frete=>get_valor_adiantamento( i_bukrs  = info_carga-carga-id_bukrs i_branch = info_carga-carga-id_branch i_lifnr  = info_carga-carga-id_proprietario ).
            CATCH zcx_parceiros.    " .
              r_margadto = 0.
          ENDTRY.

          zde_info_frete-vl_margadto = r_margadto.

          zde_info_frete-vl_frete = ( lc_quantidade / 1000 ) * zde_info_frete-kbetr.
          IF r_margadto IS INITIAL.
            lc_tem_margem_zero = abap_true.
            zde_info_frete-vl_adiantamento = 0.
          ELSE.
            zde_info_frete-vl_adiantamento = zde_info_frete-vl_frete * ( zde_info_frete-vl_margadto / 100 ).
          ENDIF.

        CATCH zcx_calc_frete INTO DATA(ex_calc_frete).
          ex_calc_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
      ENDTRY.

      SELECT SINGLE * INTO @DATA(wa_zlest0027)
        FROM zlest0027
       WHERE route EQ @zde_info_frete-route.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE s092(zles) DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        zde_info_frete-ck_tem_pedagio = wa_zlest0027-ck_pedagio.
        zde_info_frete-ck_credita_ped = wa_zlest0027-ck_pedagio.
      ENDIF.

      RANGES: rplaca FOR zlest0002-pc_veiculo.
      DATA: wplaca LIKE LINE OF rplaca.
      CLEAR: rplaca[], rplaca.
      wplaca-sign   = 'I'.
      wplaca-option = 'EQ'.

      IF info_carga-carga-ds_placa_trator IS NOT INITIAL.
        wplaca-low  = info_carga-carga-ds_placa_trator.
        wplaca-high = info_carga-carga-ds_placa_trator.
        APPEND wplaca TO rplaca.
      ENDIF.

      IF info_carga-carga-ds_placa_reboq_1 IS NOT INITIAL.
        wplaca-low  = info_carga-carga-ds_placa_reboq_1.
        wplaca-high = info_carga-carga-ds_placa_reboq_1.
        APPEND wplaca TO rplaca.
      ENDIF.

      IF info_carga-carga-ds_placa_reboq_2 IS NOT INITIAL.
        wplaca-low  = info_carga-carga-ds_placa_reboq_2.
        wplaca-high = info_carga-carga-ds_placa_reboq_2.
        APPEND wplaca TO rplaca.
      ENDIF.

      IF info_carga-carga-ds_placa_reboq_3 IS NOT INITIAL.
        wplaca-low  = info_carga-carga-ds_placa_reboq_3.
        wplaca-high = info_carga-carga-ds_placa_reboq_3.
        APPEND wplaca TO rplaca.
      ENDIF.

      IF rplaca[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(it_zlest0002)
          FROM zlest0002
         WHERE pc_veiculo IN @rplaca.
      ENDIF.

      CASE zde_info_frete-ck_tem_pedagio.
        WHEN abap_true.

          ck_alterou_pedagio = abap_true.

          zde_info_frete-dt_valor_ped = sy-datum.

          SELECT * INTO TABLE @DATA(it_zlest0090)
            FROM zlest0090
           WHERE werks      EQ @info_carga-carga-id_branch
             AND tp_servico EQ '1'.

          IF sy-subrc IS INITIAL.
            "DESCRIBE TABLE IT_ZLEST0090 LINES LC_QTD_LINHAS_PED.
            READ TABLE it_zlest0090 INTO DATA(wa_zlest0090) WITH KEY ck_default = abap_true.
            IF sy-subrc IS INITIAL.
              zde_info_frete-tp_admim_ped = wa_zlest0090-tp_adm.
            ELSE.
              READ TABLE it_zlest0090 INTO wa_zlest0090 INDEX 1.
              zde_info_frete-tp_admim_ped = wa_zlest0090-tp_adm.
            ENDIF.
          ENDIF.

          zde_info_frete-nm_qtd_eixos = 0.
          LOOP AT it_zlest0002 INTO DATA(wa_zlest0002).
            zde_info_frete-nm_qtd_eixos = zde_info_frete-nm_qtd_eixos + wa_zlest0002-qt_eixo.
          ENDLOOP.

        WHEN abap_false.

      ENDCASE.

      "Recupera Frete/Pedágio
      ck_alterou_itinerario = abap_true.

      CLEAR: it_ufs_intermediarios[], it_ufs_intermediarios.

      PERFORM recupera_ufs_intermediarias
        USING zde_info_frete-route zde_info_frete-cd_cid_origem zde_info_frete-cd_cid_destino
        CHANGING it_ufs_intermediarios
                 zsdt0001fe-id_carga_model.

      ck_alterou_pedagio = abap_true.

    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING zde_info_frete TO zde_info_frete_pedagio.
  MOVE-CORRESPONDING zde_info_frete TO zde_info_frete_frete.

  IF zde_info_frete-vl_margadto IS NOT INITIAL.
    lc_tem_margem_adiant = abap_true.
  ELSE.
    lc_tem_margem_adiant = abap_false.
  ENDIF.

  CALL SCREEN 9011 STARTING AT 55 09.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9011  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9011 OUTPUT.

  DATA: it_tucomm_9011 TYPE TABLE OF sy-ucomm.

*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF ( container_9011 IS INITIAL ).

    CLEAR: it_tucomm_9011[].

    IF ck_dado_frete_9011_cs EQ abap_true.
      APPEND 'VERIFICAR' TO it_tucomm_9011.
      APPEND 'CONFIRMAR' TO it_tucomm_9011.
    ENDIF.

    SET PF-STATUS 'PF9011' EXCLUDING it_tucomm_9011.
    SET TITLEBAR 'TL9011'.

    CREATE OBJECT container_9011
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    "Criar ALV """"""""""""""""""""""""""""""""""""""""""""""
    CREATE OBJECT ctl_container_9011
      EXPORTING
        container_name = 'ALV_UFS'.

    PERFORM fill_it_fieldcatalog_9011.

    "Hints
    PERFORM fill_it_hints_9011.

*   Fill info for layout variant
    PERFORM fill_gs_variant_9011.

    CREATE OBJECT ctl_alv_9011
      EXPORTING
        i_parent          = ctl_container_9011
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    gs_layout_9011-sel_mode   = 'A'.

    gs_layout_9011-info_fname = 'LINE_COLOR'.
    gs_layout_9011-stylefname = 'STYLE'.
    gs_layout_9011-ctab_fname = 'COLOR_CELL'.
    gs_layout_9011-zebra      = abap_false.
    gs_layout_9011-no_toolbar = abap_true.
    gs_layout_9011-no_rowmark = abap_true.
    gs_layout_9011-no_headers = abap_true.
    gs_layout_9011-edit_mode  = COND string( WHEN ck_dado_frete_9011_cs EQ abap_false THEN abap_true ELSE abap_false ).
    gs_layout_9011-grid_title = text-001.

    CALL METHOD ctl_alv_9011->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_9011
        is_variant      = gs_variant_9011
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_9011
        it_outtab       = it_ufs_alv[].

    CALL METHOD ctl_alv_9011->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_9011->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_9011.
    SET HANDLER event_handler_9011->data_changed_finished FOR ctl_alv_9011.
    SET HANDLER event_handler_9011->data_changed          FOR ctl_alv_9011.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDIF.

  IF lc_limpar_ck_pedagio = abap_true.
    CLEAR: zde_info_frete_pedagio-ck_credita_ped,
           zde_info_frete-ck_credita_ped.
  ENDIF.
  lc_limpar_ck_pedagio = abap_false.

  PERFORM ajusta_tela_pedagio.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  RECUPERA_UFS_INTERMEDIARIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM recupera_ufs_intermediarias  USING    p_route TYPE route
                                           p_cd_cid_origem TYPE j_1btxjcd
                                           p_cd_cid_destino TYPE j_1btxjcd
                                  CHANGING p_ufs TYPE zde_zsdt0001feufs_t
                                           p_id_carga_base TYPE zde_id_carga.

  DATA: wa_zsdt0001ftufs TYPE zsdt0001ftufs.

  CLEAR: p_ufs.

  IF p_id_carga_base IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_ufs_local)
      FROM zsdt0001feufs AS uf1
     WHERE uf1~id_carga EQ @p_id_carga_base.

  ELSE.
    SELECT * INTO TABLE @it_ufs_local
      FROM zsdt0001feufs AS uf1
     WHERE uf1~route          EQ @p_route
       AND uf1~cd_cid_origem  EQ @p_cd_cid_origem
       AND uf1~cd_cid_destino	EQ @p_cd_cid_destino
       AND uf1~id_carga       EQ ( SELECT MAX( uf2~id_carga )
                                     FROM zsdt0001fe AS uf2
                                    WHERE uf2~route          EQ uf1~route
                                      AND uf2~cd_cid_origem  EQ uf1~cd_cid_origem
                                      AND uf2~cd_cid_destino EQ uf1~cd_cid_destino ).

    IF sy-subrc IS NOT INITIAL.
      SELECT * INTO TABLE @it_ufs_local
        FROM zsdt0001feufs AS uf1
       WHERE uf1~route    EQ @p_route
         AND uf1~id_carga EQ ( SELECT MAX( uf2~id_carga )
                                 FROM zsdt0001fe AS uf2
                                WHERE uf2~route EQ uf1~route ).
    ENDIF.
  ENDIF.

  LOOP AT it_ufs_local INTO DATA(wa_ufs_local).
    APPEND wa_ufs_local TO p_ufs.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_9011 .

  CLEAR: it_fieldcatalog_9011[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0001FEUFS_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_9011.

  LOOP AT it_fieldcatalog_9011 ASSIGNING FIELD-SYMBOL(<fs_cat>).
    <fs_cat>-tabname = 'ZDE_ZSDT0001FEUFS_ALV'.
    CASE <fs_cat>-fieldname.
      WHEN 'NM_SEQUENCIA'.
        IF ck_dado_frete_9011_cs EQ abap_false.
          <fs_cat>-edit = abap_true.
        ENDIF.
      WHEN OTHERS.
        <fs_cat>-edit = abap_false.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS_9011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_hints_9011 .

*  DATA: IT_DD07V        TYPE TABLE OF DD07V WITH HEADER LINE,
*        WA_EXCEPT_QINFO LIKE LINE OF IT_EXCEPT_QINFO,
*        LC_TP_STATUS    TYPE ZDE_STATUS_CARGA,
*        LC_ICO_CARGA    TYPE CHAR04.
**
*  CLEAR: IT_EXCEPT_QINFO[].
**
*  "Informações Documento
*  CALL FUNCTION 'GET_DOMAIN_VALUES'
*    EXPORTING
*      DOMNAME    = 'ZDM_STATUS_CARGA'
*    TABLES
*      VALUES_TAB = IT_DD07V.
*
*  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
*    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
*    LC_TP_STATUS = CONV #( IT_DD07V-DOMVALUE_L ).
*    PERFORM SETA_ICONE_STATUS USING LC_TP_STATUS CHANGING LC_ICO_CARGA.
*    WA_EXCEPT_QINFO-VALUE = LC_ICO_CARGA.
*    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
*    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_ZSDT0001CG_ALV'.
*    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_CARGA'.
*    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_9011 .

  gs_variant_9011-report      = sy-repid.
  gs_variant_9011-handle      = '9011'.
  gs_variant_9011-log_group   = abap_false.
  gs_variant_9011-username    = abap_false.
  gs_variant_9011-variant     = abap_false.
  gs_variant_9011-text        = abap_false.
  gs_variant_9011-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9011_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9011_exit INPUT.

  CASE ok_code.
    WHEN 'CARD_PED'.
      CLEAR: ok_code.
      PERFORM informa_cartao_pedagio.

    WHEN OTHERS.
      PERFORM sair_9011.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  SAIR_9011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair_9011 .

  CLEAR: event_handler_9011.

  IF ctl_alv_9011 IS NOT INITIAL.
    ctl_alv_9011->free( ).
  ENDIF.
  CLEAR: ctl_alv_9011.

  IF ctl_container_9011 IS NOT INITIAL.
    ctl_container_9011->free( ).
  ENDIF.
  CLEAR: ctl_container_9011.

  IF container_9011 IS NOT INITIAL.
    container_9011->free( ).
  ENDIF.
  CLEAR: container_9011.

  CLEAR: zsdt0001fe,
         zde_info_frete_frete,
         zde_info_frete_pedagio,
         zde_info_frete,
         it_ufs_alv[],
         it_ufs_alv.

  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INFORMA_CARTAO_PEDAGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM informa_cartao_pedagio.

  CHECK zde_info_frete_pedagio-ck_credita_ped EQ abap_true AND
        zde_info_frete_pedagio-ck_tem_pedagio EQ abap_true.

  IF ck_ped_repom_eletronico EQ abap_true.
    "Cartão REPOM
    CLEAR zde_info_frete_pedagio-nr_cartao_ped_repom.
    PERFORM informa_cartao_pedagio_repom CHANGING zde_info_frete_pedagio-nr_cartao_ped_repom.
  ENDIF.

  IF ck_ped_tipfr_eletronico EQ abap_true.
    "Cartão TipFrete

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  AJUSTA_TELA_PEDAGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_tela_pedagio .

  IF ck_dado_frete_9011_cs EQ abap_true.
    LOOP AT SCREEN.
      IF screen-input = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.

    " >>> Valor de Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " >>> Valor de Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CASE lc_tem_margem_adiant.
      WHEN abap_true.

        CASE lc_tem_margem_zero.
          WHEN abap_true.
            "Bloquear todos os campo de entrada de adiantamento!
            "F1 - Campos Para Alteração de Adiantamento - Todos os campos
            LOOP AT SCREEN.
              IF screen-group1 = 'F1'.
                screen-input = '0'.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          WHEN abap_false.

            "Bloquear todos os campo de entrada de adiantamento!
            "F2 - Campos Para Alteração de Adiantamento - Campos Editáveis
            LOOP AT SCREEN.
              IF screen-group2 = 'F2'.
                screen-input = '1'.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
        ENDCASE.

      WHEN abap_false.
        "Bloquear todos os campo de entrada de adiantamento!
        "F1 - Campos Para Alteração de Adiantamento - Todos os campos
        LOOP AT SCREEN.
          IF screen-group1 = 'F1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.

  ENDIF.


  " >>> Pedágio """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " >>> Pedágio """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DATA: lc_informado  TYPE char01,
        lc_percurso   TYPE zlest0122,
        repom_roteiro TYPE REF TO zcl_repom_roteiro_vlr_vpr.

  ck_erro_pedagio_9011 = abap_false.
  CLEAR: lc_msg_erro_pedagio.

  CLEAR: lc_tx_cid_origem.
  IF zde_info_frete_pedagio-cd_cid_origem IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_origem
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ 'BR'
       AND taxjurcode EQ zde_info_frete_pedagio-cd_cid_origem.
  ENDIF.

  CLEAR: lc_tx_cid_destino.
  IF zde_info_frete_pedagio-cd_cid_destino IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_destino
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ 'BR'
       AND taxjurcode EQ zde_info_frete_pedagio-cd_cid_destino.
  ENDIF.

  IF ck_dado_frete_9011_cs EQ abap_false.

    ck_ped_repom_eletronico = abap_false.
    ck_ped_tipfr_eletronico = abap_false.

    IF zde_info_frete_pedagio-ck_tem_pedagio EQ abap_true.

      SELECT SINGLE * INTO @DATA(wa_zlest0090)
        FROM zlest0090
       WHERE werks      EQ @zde_info_frete-id_branch
         AND tp_servico EQ '1'
         AND tp_adm     EQ @zde_info_frete-tp_admim_ped.

      zde_info_frete-tp_adm_uso         = wa_zlest0090-tp_op.
      zde_info_frete_pedagio-tp_adm_uso = wa_zlest0090-tp_op.

      CASE wa_zlest0090-tp_op.
        WHEN 'E'. "Eletrônico

          "Repom Eletrônico
          IF wa_zlest0090-tp_adm = '03'.
            ck_ped_repom_eletronico = abap_true.
          ENDIF.

          "TipFrete Eletrônico
          IF wa_zlest0090-tp_adm = '09'.
            ck_ped_tipfr_eletronico = abap_true.
          ENDIF.

        WHEN 'M'. "Manual

      ENDCASE.
    ENDIF.


    CASE zde_info_frete_pedagio-ck_tem_pedagio.
      WHEN abap_true.

        IF zde_info_frete_pedagio-ck_credita_ped EQ abap_true.

          CASE zde_info_frete_pedagio-tp_admim_ped.
            WHEN '03'.

              CASE ck_ped_repom_eletronico.
                WHEN abap_true.
                  "REPOM Automático

                  "IF CK_ALTEROU_PEDAGIO EQ ABAP_TRUE.
                  CALL FUNCTION 'Z_REPOM_INFORMA_PERCURSO'
                    EXPORTING
                      i_branch         = zde_info_frete-ag_branch
                      i_bukrs          = zde_info_frete-ag_bukrs
                      i_cd_cid_origem  = zde_info_frete_pedagio-cd_cid_origem
                      i_cd_cid_destino = zde_info_frete_pedagio-cd_cid_destino
                    IMPORTING
                      e_informado      = lc_informado
                      e_percurso       = lc_percurso
                    EXCEPTIONS
                      sem_percurso     = 1
                      OTHERS           = 2.

                  IF sy-subrc IS INITIAL.
                    CREATE OBJECT repom_roteiro.
                    repom_roteiro->set_bukrs( EXPORTING i_bukrs = zde_info_frete-ag_bukrs ).
                    repom_roteiro->set_branch( EXPORTING i_branch = zde_info_frete-ag_branch ).
                    repom_roteiro->set_veiculo_eixos( EXPORTING i_veiculo_eixos = zde_info_frete_pedagio-nm_qtd_eixos ).
                    repom_roteiro->set_qtd_eixos_suspensos_ida( EXPORTING i_qtd_eixos_suspensos_ida = 0 ).
                    repom_roteiro->set_qtd_eixos_suspensos_volta( EXPORTING i_qtd_eixos_suspensos_volta = 0 ).
                    repom_roteiro->set_id_rota_repom( EXPORTING i_id_rota_repom = lc_percurso-id_rota_repom ).
                    repom_roteiro->set_id_percurso_repom( EXPORTING i_id_percurso_repom = lc_percurso-id_percurso_repom ).
                    repom_roteiro->set_id_rota( EXPORTING i_id_rota = lc_percurso-id_rota ).

                    CALL METHOD repom_roteiro->consultar_valor
                      IMPORTING
                        e_erros                    = DATA(lc_erros)
                      RECEIVING
                        i_retornou                 = DATA(p_retornou)
                      EXCEPTIONS
                        servico_nao_encontrado     = 1
                        http_communication_failure = 2
                        http_invalid_state         = 3
                        http_processing_failed     = 4
                        http_invalid_timeout       = 5
                        erro                       = 6
                        OTHERS                     = 7.

                    IF sy-subrc IS NOT INITIAL.
                      ck_erro_pedagio_9011 = abap_true.
                      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lc_msg_erro_pedagio.
                      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                    ENDIF.

                    IF p_retornou EQ abap_false.
                      LOOP AT lc_erros INTO DATA(lc_erro).
                        ck_erro_pedagio_9011 = abap_true.
                        MESSAGE s017(zrepom) WITH lc_erro-erro_codigo lc_erro-erro_descricao INTO lc_msg_erro_pedagio.
                        MESSAGE i017(zrepom) WITH lc_erro-erro_codigo lc_erro-erro_descricao DISPLAY LIKE 'E'.
                      ENDLOOP.
                    ELSE.
                      zde_info_frete_pedagio-vl_pedagio        = repom_roteiro->get_valor_total_vpr( ).
                      zde_info_frete_pedagio-id_rota           = lc_percurso-id_rota.
                      zde_info_frete_pedagio-id_rota_repom     = lc_percurso-id_rota_repom.
                      zde_info_frete_pedagio-id_percurso_repom = lc_percurso-id_percurso_repom.
                    ENDIF.
                    CLEAR: repom_roteiro.
                  ELSE.
                    ck_erro_pedagio_9011 = abap_true.
                    MESSAGE s040(zrepom) WITH zde_info_frete_pedagio-cd_cid_origem zde_info_frete_pedagio-cd_cid_destino INTO lc_msg_erro_pedagio.
                    MESSAGE i040(zrepom) WITH zde_info_frete_pedagio-cd_cid_origem zde_info_frete_pedagio-cd_cid_destino DISPLAY LIKE 'E'.
                  ENDIF.
                  "ENDIF.

                WHEN abap_false.
                  "REPOM Manual

              ENDCASE.

            WHEN '09'.

              CASE ck_ped_tipfr_eletronico.
                WHEN abap_true.
                  "TipFrete Automático
                  zde_info_frete_pedagio-tp_card_ped = 'S'.

                  SELECT SINGLE * INTO @DATA(wa_zlest0091)
                    FROM zlest0091
                   WHERE qtd_eixo EQ @zde_info_frete_pedagio-nm_qtd_eixos.

                  SELECT *
                    FROM zlest0084
                    INTO TABLE @DATA(it_zlest0084)
                   WHERE branch         EQ @zde_info_frete-ag_branch
                     AND munic_origem   EQ @zde_info_frete_pedagio-cd_cid_origem+3(7)
                     AND munic_destino  EQ @zde_info_frete_pedagio-cd_cid_destino+3(7)
                     AND cat_veiculo    EQ @wa_zlest0091-categoria
                     AND prioridade     EQ @abap_true.

                  IF sy-subrc IS INITIAL.
                    SORT it_zlest0084 BY vlr_pedagio ASCENDING.
                    READ TABLE it_zlest0084 INTO DATA(wa_zlest0084) INDEX 1.
                    IF ( zde_info_frete_pedagio-dt_valor_ped >= wa_zlest0084-dt_vigencia ).
                      IF NOT ( wa_zlest0084-vlr_pedagio IS INITIAL ).
                        zde_info_frete_pedagio-vl_pedagio  = wa_zlest0084-vlr_pedagio.
                        zde_info_frete_pedagio-id_rota_tip = wa_zlest0084-id_rota.
                      ENDIF.
                    ELSE.
                      ck_erro_frete_9011 = abap_true.
                      MESSAGE i000(fi) DISPLAY LIKE 'W' WITH 'Data de Vigência menor que a do transporte.'.
                    ENDIF.
                  ENDIF.

                WHEN abap_false.
                  "TipFrete Manual
              ENDCASE.

          ENDCASE.
        ENDIF.

        "Ativar Campos de Pedágio
        "P2 - Campos Para Alteração de Pedágio - Campos Editáveis
        LOOP AT SCREEN.
          IF screen-group2 = 'P2'.
            IF zde_info_frete_pedagio-ck_credita_ped EQ abap_false.

              "Se não é para creditar pedágio mesno tendo
              IF screen-name NE 'ZDE_INFO_FRETE_PEDAGIO-CK_CREDITA_PED'.
                "Desabilitar campos para entrada de dados
                screen-input = '0'.
                MODIFY SCREEN.
              ENDIF.

            ELSEIF screen-name EQ 'BTN_CARTAO_REPOM'.

              CASE ck_ped_repom_eletronico.
                WHEN abap_true.
                  "Habilitar campos para entrada de dados
                  screen-input = '1'.
                  MODIFY SCREEN.
                WHEN abap_false.
                  "Desabilitar campos para entrada de dados
                  screen-input = '0'.
                  MODIFY SCREEN.
              ENDCASE.

            ELSEIF screen-name EQ 'ZDE_INFO_FRETE_PEDAGIO-NR_CARTAO_PED_REPOM'.

              CASE ck_ped_repom_eletronico.
                WHEN abap_true.
                  "Habilitar campos para entrada de dados
                  screen-input = '1'.
                  MODIFY SCREEN.
                WHEN abap_false.
                  "Desabilitar campos para entrada de dados
                  screen-input = '0'.
                  MODIFY SCREEN.
              ENDCASE.

            ELSEIF screen-name EQ 'ZDE_INFO_FRETE_PEDAGIO-VL_PEDAGIO'.
              CASE ck_ped_repom_eletronico.
                WHEN abap_true.
                  "Desabilitar campos para entrada de dados
                  "Valor do Pedágio não é informado é retornado pelo webservice da REPOM
                  screen-input = '0'.
                  MODIFY SCREEN.
                WHEN abap_false.
                  "Habilitar campos para entrada de dados
                  "Valor do Pedágio deve ser informado
                  screen-input    = '1'.
                  MODIFY SCREEN.
              ENDCASE.
            ELSEIF screen-name EQ 'ZDE_INFO_FRETE_PEDAGIO-CK_CREDITA_PED'.
              CASE zde_info_frete_pedagio-ck_tem_pedagio.
                WHEN abap_true.
                  "Habilita campos para entrada de dados
                  screen-input = '1'.
                  MODIFY SCREEN.
                WHEN abap_false.
                  "Desabilitar campos para entrada de dados
                  screen-input = '0'.
                  MODIFY SCREEN.
              ENDCASE.
            ELSEIF screen-name EQ 'ZDE_INFO_FRETE_PEDAGIO-TP_ADMIM_PED'.
              "IF LC_QTD_LINHAS_PED GT 1.
              "Habilitar campo de Administradora de Pedágio
              screen-input = '1'.
              MODIFY SCREEN.
              "ELSE.
              "Desabilitar campo de Administradora de Pedágio
              "  SCREEN-INPUT = '0'.
              "  MODIFY SCREEN.
              "ENDIF.
            ELSE.
              "Habilitar campos para entrada de dados
              screen-input = '1'.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDLOOP.

      WHEN abap_false.
        "Bloquear todos os campo de entrada de pedágio!
        "P1 - Campos Para Alteração de Pedágio - Todos os campos
        LOOP AT SCREEN.
          IF screen-group1 = 'P1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    ck_alterou_pedagio = abap_false.

    IF ck_alterou_itinerario EQ abap_true.

      READ TABLE it_ufs_intermediarios WITH KEY id_carga = space TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE it_ufs_intermediarios WITH KEY id_carga = zsdt0001fe-id_carga TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: zsdt0001fe-id_carga_model.
          PERFORM recupera_ufs_intermediarias USING zde_info_frete-route zde_info_frete_pedagio-cd_cid_origem zde_info_frete_pedagio-cd_cid_destino
            CHANGING it_ufs_intermediarios
                     zsdt0001fe-id_carga_model.
        ENDIF.
      ENDIF.

      PERFORM carrega_estados_grid.

      ck_alterou_itinerario = abap_false.

      ctl_alv_9011->refresh_table_display( ).

    ENDIF.
  ELSE.
    PERFORM carrega_estados_grid.
    ctl_alv_9011->refresh_table_display( ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ESTADOS_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_estados_grid .

  CLEAR: it_ufs_alv.

  SORT it_ufs_intermediarios BY nm_sequencia ASCENDING.

  SELECT * INTO TABLE @DATA(it_t005u)
    FROM t005u
   WHERE spras EQ @sy-langu
     AND land1 EQ 'BR'
     AND bland GT '99'
     AND bland NE @zde_info_frete_pedagio-cd_cid_origem(3)
     AND bland NE @zde_info_frete_pedagio-cd_cid_destino(3).

  SORT it_t005u BY bland ASCENDING.

  LOOP AT it_t005u INTO DATA(wa_t005u).
    wa_ufs_alv-bland = wa_t005u-bland.
    wa_ufs_alv-bezei = wa_t005u-bezei.
    APPEND wa_ufs_alv TO it_ufs_alv.
  ENDLOOP.

  LOOP AT it_ufs_intermediarios INTO DATA(wa_ufs_intermediarios).
    READ TABLE it_ufs_alv ASSIGNING FIELD-SYMBOL(<fs_ufs>) WITH KEY bland = wa_ufs_intermediarios-bland.
    IF sy-subrc IS INITIAL.
      <fs_ufs>-nm_sequencia = wa_ufs_intermediarios-nm_sequencia.
      PERFORM ajusta_cores_ufs CHANGING <fs_ufs>.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_CORES_UFS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_UFS>  text
*----------------------------------------------------------------------*
FORM ajusta_cores_ufs  CHANGING p_ufs TYPE ty_ufs_alv.

  IF p_ufs-nm_sequencia IS INITIAL.
    CLEAR p_ufs-line_color.
  ELSE.
    p_ufs-line_color = cs_line_color_alterado.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  INFORMA_CARTAO_PEDAGIO_REPOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ZDE_INFO_FRETE_PEDAGIO_NR_CART  text
*----------------------------------------------------------------------*
FORM informa_cartao_pedagio_repom  CHANGING p_cartao TYPE zde_repom_cartao_ped.

  DATA: i_cartao    TYPE  zde_repom_cartao,
        ck_validado TYPE  char01,
        e_erros     TYPE  zde_repom_erros_t.

  CHECK p_cartao IS NOT INITIAL.

  i_cartao-bukrs     = zde_info_frete-ag_bukrs.
  i_cartao-branch    = zde_info_frete-ag_branch.
  i_cartao-nr_cartao = p_cartao.

  IF p_cartao IS INITIAL.
    CALL FUNCTION 'Z_REPOM_INFORMA_CARTAO_PED'
      IMPORTING
        ck_validado = ck_validado
      CHANGING
        i_cartao    = i_cartao.

    IF ck_validado EQ abap_true.
      p_cartao = i_cartao-nr_cartao.
    ELSE.
      MESSAGE s232 DISPLAY LIKE 'E'.
    ENDIF.

  ELSE.
    CALL FUNCTION 'Z_REPOM_VALIDA_CARTAO'
      EXPORTING
        i_cartao = i_cartao "WA_REPOM_CARTAO
      IMPORTING
        e_valido = ck_validado
        e_erros  = e_erros
      EXCEPTIONS
        erro     = 1
        OTHERS   = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSEIF ck_validado EQ abap_false.
      LOOP AT e_erros INTO DATA(wa_erro).
        MESSAGE e017 WITH wa_erro-erro_codigo wa_erro-erro_descricao.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  SET_VL_ADIANTAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_vl_adiantamento INPUT.

  DATA(vl_adiantamento_permitido) = zde_info_frete_frete-vl_frete * ( zde_info_frete_frete-vl_margadto / 100 ).
  IF zde_info_frete_frete-vl_adiantamento GT vl_adiantamento_permitido.
    MESSAGE e022(zles) WITH zde_info_frete_frete-vl_margadto.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_NR_CARTAO_PED_REPOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_nr_cartao_ped_repom INPUT.
  PERFORM informa_cartao_pedagio_repom CHANGING zde_info_frete_pedagio-nr_cartao_ped_repom.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_CREDITA_PEDAGIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_credita_pedagio INPUT.

  IF zde_info_frete_pedagio-ck_credita_ped EQ abap_false.
    CLEAR: zde_info_frete_pedagio-nr_cartao_ped_repom,
           zde_info_frete_pedagio-vl_pedagio,
           zde_info_frete_pedagio-cd_cid_destino,
           zde_info_frete_pedagio-cd_cid_origem.
  ENDIF.
  CHECK zde_info_frete_pedagio-ck_credita_ped EQ abap_true.
  ck_alterou_pedagio = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_ALTEROU_ITINERARIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alterou_itinerario INPUT.
  ck_alterou_itinerario = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_ALTEROU_PEDAGIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alterou_pedagio INPUT.
  ck_alterou_pedagio = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9011  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9011 INPUT.

  DATA: wl_header TYPE thead.

  CASE ok_code.
    WHEN 'CONFIRMAR'.

      CHECK ck_alterou_pedagio    EQ abap_false.
      CHECK ck_alterou_itinerario EQ abap_false.

      IF ck_erro_pedagio_9011 EQ abap_true.
        MESSAGE lc_msg_erro_pedagio TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF zde_info_frete_pedagio-ck_tem_pedagio EQ abap_true.

        IF ck_ped_repom_eletronico EQ abap_true AND
           zde_info_frete_pedagio-nr_cartao_ped_repom IS INITIAL AND
           zde_info_frete_pedagio-ck_credita_ped EQ abap_true.
          "233  Deve ser informado o Cartão de Pedágio da REPOM!
          MESSAGE i233 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF ck_ped_tipfr_eletronico EQ abap_true.
          "234  Deve ser informado o Cartão de Pedágio da TipFrete!
          MESSAGE i234 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      ENDIF.

      DATA(id_carga) = zsdt0001fe-id_carga.
      CLEAR: zsdt0001fe.
      MOVE-CORRESPONDING zde_info_frete TO zsdt0001fe.
      MOVE-CORRESPONDING zde_info_frete_pedagio TO zsdt0001fe.
      MOVE-CORRESPONDING zde_info_frete_frete TO zsdt0001fe.
      zsdt0001fe-id_carga = id_carga.

      DELETE it_ufs_intermediarios WHERE nm_sequencia IS INITIAL.
      IF it_ufs_intermediarios[] IS NOT INITIAL.
        READ TABLE it_ufs_intermediarios WITH KEY id_carga = id_carga TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE it_ufs_intermediarios INTO wa_ufs_intermediarios INDEX 1.
          zsdt0001fe-id_carga_model = wa_ufs_intermediarios-id_carga.
          CLEAR it_ufs_intermediarios[].
        ELSE.
          SORT it_ufs_intermediarios BY nm_sequencia ASCENDING.
          DATA(lc_sequencia) = 1.
          LOOP AT it_ufs_intermediarios ASSIGNING FIELD-SYMBOL(<fs_ufs>).
            <fs_ufs>-cd_cid_destino = zsdt0001fe-cd_cid_destino.
            <fs_ufs>-cd_cid_origem  = zsdt0001fe-cd_cid_origem.
            <fs_ufs>-route          = zsdt0001fe-route.
            <fs_ufs>-id_carga       = id_carga.
            <fs_ufs>-nm_sequencia   = lc_sequencia.
            ADD 1 TO lc_sequencia.
          ENDLOOP.
          DELETE FROM zsdt0001feufs WHERE id_carga = id_carga.
          zsdt0001fe-id_carga_model = id_carga.
        ENDIF.
      ENDIF.
      MODIFY zsdt0001fe.
      IF it_ufs_intermediarios[] IS NOT INITIAL.
        MODIFY zsdt0001feufs FROM TABLE it_ufs_intermediarios[].
      ENDIF.

      ck_dado_frete_9011_ok = abap_true.

      PERFORM sair_9011.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_AG_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_CARGA_CARGA_ID_AGENT_FRETE  text
*----------------------------------------------------------------------*
FORM f_popup_ag_frete  CHANGING p_agent_frete p_erro.

  DATA: l_lifnr TYPE lfa1-lifnr.

  DATA: BEGIN OF  it_tab OCCURS 0.
          INCLUDE STRUCTURE sval.
        DATA: END OF it_tab.

  DATA: it_return(1) TYPE c.

  "Preenche campo do popup com o valor inicial
  l_lifnr = p_agent_frete.

  CLEAR it_tab.
  it_tab-tabname    = 'LFA1'.
  it_tab-fieldname  = 'LIFNR'.
  it_tab-fieldtext  = 'Id. Agente Frete'.
  it_tab-value      =  l_lifnr.
  it_tab-comp_tab   = 'LFA1'.
  it_tab-comp_field = 'LIFNR'.
  it_tab-field_obl  = 'X'.
*  it_tab-novaluehlp = ''.
  APPEND it_tab.

  CLEAR:  it_tab, p_erro.
  "Abre popup para informar novo campo ou validar o informado
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Confirmar Agente de frete!'
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = it_return
    TABLES
      fields          = it_tab
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CLEAR: l_lifnr.
  LOOP AT it_tab.
    IF sy-tabix = 1 AND it_tab-value IS NOT INITIAL.
      l_lifnr = it_tab-value.
    ENDIF.
  ENDLOOP.

  IF l_lifnr IS NOT INITIAL.

    "Valida agende de frete
    SELECT SINGLE lifnr, ktokk
    FROM lfa1
    INTO @DATA(wlfa1_aux)
    WHERE lifnr = @l_lifnr
      AND   ktokk = 'ZFIC'.

    IF sy-subrc IS NOT INITIAL.
      p_erro = 'X'.
    ENDIF.

  ENDIF.

  " Preenche zeros a esquerda
  UNPACK l_lifnr TO p_agent_frete.

ENDFORM.

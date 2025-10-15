CLASS zcl_int_ib_fi_flux_cx_zfi0098 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_aedat,
        inicio TYPE dats,
        fim    TYPE dats,
      END OF ty_aedat .
    TYPES:
      BEGIN OF ty_bedat,
        inicio TYPE dats,
        fim    TYPE dats,
      END OF ty_bedat .
    TYPES:
      BEGIN OF ty_dados_fornecedor,
        nome               TYPE adrc-name1,
        endereco           TYPE adrc-street,
        municipio          TYPE adrc-city1,
        estado             TYPE adrc-region,
        bairro             TYPE adrc-city2,
        end_numero         TYPE adrc-house_num1,
        cnpj               TYPE lfa1-stcd1,
        cpf                TYPE lfa1-stcd2,
        inscricao_estadual TYPE lfa1-stcd3,
        pessoa_fisica      TYPE lfa1-stkzn,
      END OF ty_dados_fornecedor .
    TYPES:
      BEGIN OF ty_user_lan,
        cpf  TYPE adr3-fax_number,
        nome TYPE user_addr-name_textc,
      END OF ty_user_lan .
    TYPES:
      BEGIN OF tl_cabecalho,
        ekko                        TYPE ekko,
        dados_fornecedor            TYPE ty_dados_fornecedor,
        ds_empresa                  TYPE t001-butxt,
        usuario_lancamento          TYPE ty_user_lan,
        ds_usuario_aprovador_ultimo TYPE user_addr-name_textc,
      END OF tl_cabecalho .
    TYPES:
      BEGIN OF ty_dados_mat,
        tp_material         TYPE mtart,
        ds_material         TYPE maktx,
        ds_grupo_mercadoria TYPE wgbez,
      END OF ty_dados_mat .
    TYPES:
      BEGIN OF ty_dados_centro,
        endereco    TYPE t001w-stras,
        cidade      TYPE t001w-ort01,
        uf          TYPE t001w-regio,
        bairro      TYPE adrc-city2,
        endereco_nr TYPE adrc-house_num1,
        endereco_2  TYPE kna1-stras,
        cnpj        TYPE kna1-stcd1,
      END OF ty_dados_centro .
    TYPES:
      BEGIN OF it_itens,
        ekpo           TYPE ekpo,
        dados_material TYPE ty_dados_mat,
        dados_centro   TYPE ty_dados_centro,
      END OF it_itens .
    TYPES:
      BEGIN OF ty_historico,
        ekbe                  TYPE ekbe,
        doc_mat_estornado(1),
        doc_miro_estornado(1),
      END OF ty_historico .
    TYPES:
      BEGIN OF ty_class_contabil,
        ebelp                       TYPE  ekkn-ebelp,
        sakto                       TYPE  ekkn-sakto,
        gsber                       TYPE  ekkn-gsber,
        kostl                       TYPE  ekkn-kostl,
        kokrs                       TYPE  ekkn-kokrs,
        aufnr                       TYPE  ekkn-aufnr,
        anln1                       TYPE  ekkn-anln1,
        anln2                       TYPE  ekkn-anln2,
        ds_conta_contabil           TYPE skat-txt50,
        ds_divisao                  TYPE tgsbt-gtext,
        ds_centro_custo             TYPE cskt-ltext,
        ds_centro_custo_responsavel TYPE csks-verak,
      END OF ty_class_contabil .
    TYPES:
      BEGIN OF ty_cabecalho_miro,
        belnr TYPE rbkp-belnr,
        gjahr TYPE rbkp-gjahr,
        budat TYPE rbkp-budat,
        stblg TYPE rbkp-stblg,
      END OF ty_cabecalho_miro .
    TYPES:
      BEGIN OF ty_itens_miro,
        belnr TYPE rseg-belnr,
        gjahr TYPE rseg-gjahr,
        xblnr TYPE rseg-xblnr,
      END OF ty_itens_miro .
    TYPES:
      BEGIN OF ty_dados_miro,
        cabecalho TYPE ty_cabecalho_miro,
        itens     TYPE ty_itens_miro,
      END OF  ty_dados_miro .

    DATA:
      tb_ebeln    TYPE TABLE OF ebeln .
    DATA:
      tb_lifnr    TYPE TABLE OF elifn .
    DATA:
      tb_ebelp    TYPE TABLE OF ebelp .
    DATA:
      tb_werks    TYPE TABLE OF ewerk .
    DATA:
      tb_bewtp    TYPE TABLE OF bewtp .
    DATA:
      tb_vgabe    TYPE TABLE OF vgabe .
    DATA:
      tb_lifn2    TYPE TABLE OF lifn2 .
    DATA:
      tb_parvw    TYPE TABLE OF parvw .
    DATA:
      tb_bukrs    TYPE TABLE OF bukrs .
    DATA:
      tb_bsart    TYPE TABLE OF bsart .
    DATA:
      tb_loekz    TYPE TABLE OF loekz .
    DATA:
      tb_shkzg    TYPE TABLE OF shkzg .
    DATA:
      tb_matnr    TYPE TABLE OF matnr .
    DATA:
      tb_no_loekz TYPE TABLE OF loekz .
    DATA:
      BEGIN OF ty_filter_header,
        ebeln LIKE tb_ebeln,
        lifnr LIKE tb_lifnr,
        bukrs LIKE tb_bukrs,
        bsart LIKE tb_bsart,
        aedat TYPE  ty_aedat,
        bedat TYPE ty_bedat,
      END OF ty_filter_header .
    DATA:
      BEGIN OF ty_items_filters,
        ebelp    LIKE tb_ebelp,
        werks    LIKE tb_werks,
        matnr    LIKE tb_matnr,
        loekz    LIKE tb_loekz,
        no_loekz LIKE tb_no_loekz,
      END OF ty_items_filters .
    DATA:
      BEGIN OF ty_historic_filters,
        bewtp LIKE tb_bewtp,
        vgabe LIKE tb_vgabe,
        shkzg LIKE tb_shkzg,
      END OF ty_historic_filters .
    DATA:
      BEGIN OF ty_partners_filters,
        lifn2 LIKE tb_lifn2,
        parvw LIKE tb_parvw,
      END OF ty_partners_filters .
    DATA:
      tl_itens_miro    TYPE TABLE OF ty_itens_miro .
    DATA:
      tl_itens TYPE TABLE OF it_itens .
    DATA:
      tl_historico TYPE TABLE OF ty_historico .
    DATA:
      tl_class_contabil TYPE TABLE OF ty_class_contabil .
    DATA:
      BEGIN OF ty_pedidos,
        cabecalho      TYPE zde_saida_ekko,
        itens          TYPE TABLE OF zde_saida_ekpo, "tl_itens,
        historico      TYPE TABLE OF zde_saida_historico, "ty_historico,
        parceiros      TYPE zekpa_tab,
        class_contabil TYPE TABLE OF ty_class_contabil,
        dados_miro     TYPE TABLE OF ty_dados_miro,
      END OF ty_pedidos .
    DATA:
      BEGIN OF zde_data_request,
        header_filters       LIKE ty_filter_header,
        items_filters        LIKE ty_items_filters,
        historic_filters     LIKE ty_historic_filters,
        partners_filters     LIKE ty_partners_filters,
        dados_cabecalho      TYPE char1,
        dados_itens          TYPE char1,
        dados_historico      TYPE char1,
        dados_parceiro       TYPE char1,
        dados_class_contabil TYPE char1,
        dados_miro           TYPE char1,
        dados_complementares TYPE char1,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        pedidos LIKE TABLE OF ty_pedidos,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '191' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_FI_FLUX_CX_ZFI0098 IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~configure_server.

    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
       ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: lwa_data_request TYPE zfi_rel_zfi0098.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF lwa_data_request-empresa IS INITIAL.
      r_msg_erro = 'Empresa não Informada!'.
      e_status_code = '402'. "Payment Required
      RETURN.

    ELSEIF lwa_data_request-mes_ano IS INITIAL.
      r_msg_erro = 'Mes_Ano não Informado!'.
      e_status_code = '402'. "Payment Required
      RETURN.

    ELSEIF lwa_data_request-moeda IS INITIAL.
      r_msg_erro = 'Moeda não Informada!'.
      e_status_code = '402'. "Payment Required
      RETURN.

    ELSE.

    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.


    DATA: lwa_data_request  TYPE zfi_rel_zfi0098,
          lwa_data_response TYPE zfie0014_t.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound. "lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

    IF e_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.


    CALL FUNCTION 'ZFI_ZFI0098'
      EXPORTING
        i_empresa = lwa_data_request-empresa
        i_mes_ano = lwa_data_request-mes_ano
        i_moeda   = lwa_data_request-moeda
      IMPORTING
        it_saida  = lwa_data_response.


    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ). "enviar para essa tabela lwa_data_response


  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.

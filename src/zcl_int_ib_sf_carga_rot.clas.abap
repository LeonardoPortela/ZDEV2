CLASS zcl_int_ib_sf_carga_rot DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF z_itens,
        iditempedido   TYPE char100,
        quantidade     TYPE string,
        ordemdeentrega TYPE string,
        dataembarque   TYPE string,
      END OF z_itens .

    DATA:
      BEGIN OF  z_carga,
        identifiercargo TYPE string,
        datacriacao     TYPE string,
        itens           TYPE TABLE OF z_itens,
      END OF z_carga .
    DATA:
      BEGIN OF zde_data_request,
        carga LIKE z_carga,
      END OF zde_data_request .
*    DATA:
*      BEGIN OF ZDE_DATA_RESPONSE,
*
*      END OF ZDE_DATA_RESPONSE .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '294' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_SF_CARGA_ROT IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

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

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  METHOD zif_integracao_inbound~validar_dados_inbound.
*
    DATA: lwa_data_request LIKE zde_data_request.

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
    IF lwa_data_request IS INITIAL.
      r_msg_erro = 'Nenhum dado informado!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-carga-identifiercargo IS INITIAL.
      r_msg_erro = 'Identificador da Carga não informado!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-carga-datacriacao IS INITIAL.
      r_msg_erro = 'Data Criação da Carga não informado!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-carga-itens[] IS INITIAL.
      r_msg_erro = 'Itens da Carga não informado!'.
      RETURN.
    ENDIF.

    LOOP AT  lwa_data_request-carga-itens INTO DATA(lwa_item).
      IF lwa_item-iditempedido IS INITIAL.
        r_msg_erro = 'Identificador Item não informado!'.
        RETURN.
      ENDIF.

      IF lwa_item-quantidade IS INITIAL.
        r_msg_erro = 'Quantidade Item não informado!'.
        RETURN.
      ENDIF.

      IF lwa_item-ordemdeentrega IS INITIAL.
        r_msg_erro = 'Ordem de Entrega do Item Carga não informado!'.
        RETURN.
      ENDIF.

*      IF lwa_item-dataembarque IS INITIAL.
*        r_msg_erro = 'Data Embarque Carga não informado!'.
*        RETURN.
*      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_header_carga TYPE zsds382,
          lit_solicitacoes TYPE zsds381_t.

    DATA: lit_sol_psq TYPE TABLE OF zsdt0082.

    DATA: lit_solicitacoes_carga TYPE zsds384_t.

    DATA: lwa_data_request  LIKE zde_data_request.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound..

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

    IF e_msg_erro IS INITIAL.

      LOOP AT  lwa_data_request-carga-itens INTO DATA(lwa_item).
        APPEND INITIAL LINE TO lit_sol_psq  ASSIGNING FIELD-SYMBOL(<fs_sol_psq>).
        <fs_sol_psq>-nro_sol = lwa_item-iditempedido+00(10).
        <fs_sol_psq>-seq     = lwa_item-iditempedido+10(03).
      ENDLOOP.

      SELECT zsdt0082~nro_sol
             zsdt0082~seq
             zsdt0082~vbeln
             zsdt0082~posnr
             zsdt0082~seq_lib  INTO CORRESPONDING FIELDS OF TABLE lit_solicitacoes_carga
        FROM zsdt0082 INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND
                                         zsdt0082~posnr = vbap~posnr
                      INNER JOIN mara ON mara~matnr = vbap~matnr
                      INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
              FOR ALL ENTRIES IN lit_sol_psq
       WHERE zsdt0082~nro_sol    EQ lit_sol_psq-nro_sol
         AND zsdt0082~seq        EQ lit_sol_psq-seq
         AND zsdt0082~seq        NE 1
         AND zsdt0082~carga_automatica EQ abap_true
         AND ( zsdt0082~status EQ 2 OR
               zsdt0082~status EQ 5 ).

      IF lit_solicitacoes_carga[] IS INITIAL.
        e_msg_erro = 'Dados Solicitações não encontrados!'.
      ENDIF.

      IF e_msg_erro IS INITIAL AND lit_solicitacoes_carga[] IS NOT INITIAL.

        "Recuperar Dados Complementares Solicitação
        zcl_carga_saida_insumos=>get_dados_solitacoes(
          EXPORTING
            i_solicitacoes          = lit_solicitacoes_carga
          RECEIVING
            r_solicitacoes = DATA(lit_dados_comp_sol) ).

        IF lit_dados_comp_sol[] IS INITIAL.
          e_msg_erro = 'Dados Complementares das Solicitações não encontrados!'.
        ENDIF.

      ENDIF.

    ENDIF.

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

*--------------------------------------------------------------------------------------------------------------------*
*   Montagem Dados Carga Saida
*--------------------------------------------------------------------------------------------------------------------*

    "Montagem Dados Cabeçalho
    CLEAR: lwa_header_carga.

    lwa_header_carga-id_carga_safra_control  = lwa_data_request-carga-identifiercargo.
    "lwa_header_carga-inco1                   = 'CPT'.
    lwa_header_carga-carga_automatica        = abap_true.

    "Montagem Dados Solicitações
    CLEAR: lit_solicitacoes[].

    LOOP AT  lwa_data_request-carga-itens INTO lwa_item.

      READ TABLE lit_dados_comp_sol INTO DATA(lwa_dados_comp_sol) WITH KEY nro_sol = lwa_item-iditempedido+00(10)
                                                                           seq     = lwa_item-iditempedido+10(3).
      IF sy-subrc NE 0.
        e_msg_erro     = |Solicitação { lwa_item-iditempedido+00(10) } Item  { lwa_item-iditempedido+10(3) } não encontrado no SAP|.
        e_sucesso      = abap_true.
        e_nm_code      = '400'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
        RETURN.
      ENDIF.

      APPEND INITIAL LINE TO lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao_carga>).

      MOVE-CORRESPONDING lwa_dados_comp_sol TO <fs_solicitacao_carga>.

      <fs_solicitacao_carga>-qtd_vinc           = lwa_item-quantidade.
      <fs_solicitacao_carga>-qtd_vinc_kg        = <fs_solicitacao_carga>-qtd_vinc * <fs_solicitacao_carga>-brgew.
      <fs_solicitacao_carga>-seq_entrega        = lwa_item-ordemdeentrega.

      IF ( lwa_header_carga-dt_entrega IS INITIAL ) OR
         ( lwa_header_carga-dt_entrega LT <fs_solicitacao_carga>-dt_entrega ).
        lwa_header_carga-dt_entrega = <fs_solicitacao_carga>-dt_entrega.
      ENDIF.

      lwa_header_carga-roteiro_pc             = <fs_solicitacao_carga>-nr_rot_pc.
      lwa_header_carga-rot_desc               = <fs_solicitacao_carga>-local_embarq.
      lwa_header_carga-codigo_pc              = <fs_solicitacao_carga>-lifnr.
      lwa_header_carga-descricao_pc           = <fs_solicitacao_carga>-name1_lifnr.
      lwa_header_carga-frete_por_t            = abap_true.
      lwa_header_carga-integrar_carguero      = abap_true.

    ENDLOOP.

    "Gravar
    zcl_carga_saida_insumos=>gravar_carga(
      EXPORTING
        i_header                   = lwa_header_carga
        i_solicitacoes             = lit_solicitacoes
        i_criacao_carga_safra_ctrl = abap_true
      IMPORTING
        e_carga                    = DATA(_nro_carga)
        e_msg_erro                 = e_msg_erro ).

    IF e_msg_erro IS INITIAL AND _nro_carga IS NOT INITIAL.

      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = |Carga { _nro_carga } criada!|.
      e_msg_outbound = '{  "return": "'         && e_msg_erro && '" ,'  && cl_abap_char_utilities=>newline &&
                        '   "nr_carga_sap" : "'  && _nro_carga &&  '" ,' && cl_abap_char_utilities=>newline &&
                        '   "protocolo"   : "'  && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                        '   "status_code" : "'  && e_nm_code  && '" '   && cl_abap_char_utilities=>newline &&
                         '}'.
    ELSE.

      ROLLBACK WORK.

      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.

class ZCL_INT_OB_CARGUERO_TN_APR_TER definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_OB_CARGUERO_TN_APR_TER .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_OB_CARGUERO_TN_APR_TER IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_apr_car.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_apr_car.

    IF sy-subrc EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_int_ob_carguero_tn_apr_ter~set_ds_url( ).
*-#147361-31.07.2024-JT-fim

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

    SELECT SINGLE * INTO @DATA(wa_viagem)
      FROM zlest0185
     WHERE viagem_id EQ @i_integracao-id_referencia.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgid
                            msgno = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgno
                            attr1 = CONV #( i_integracao-id_referencia ) )
          msgid  = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgid
          msgno  = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_integracao-id_referencia ).
    ENDIF.

    TRY .
        CAST zcl_integracao_token(
               zcl_integracao_token=>zif_integracao_token~get_instance(
                 )->set_empresa_token( EXPORTING i_bukrs = wa_viagem-bukrs
                 )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_int_ob_carguero_tn_apr_ter~aprovar_viagem.

    DATA: lva_msg_erro TYPE string.

    r_if_integracao_carregar  = me.

    FREE: e_id_integracao.

    SELECT SINGLE * INTO @me->zif_int_ob_carguero_tn_apr_ter~at_viagem
      FROM zlest0185
     WHERE viagem_id EQ @i_viagem_id.

    CHECK sy-subrc IS INITIAL.

*    SELECT SINGLE *
*     FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
*    WHERE viagem_id EQ @i_viagem_id.
*
*    CHECK sy-subrc EQ 0.
*
*    SELECT SINGLE *
*      FROM zmmt0203 INTO @DATA(lwa_zmmt0203)
*     WHERE nro_cg EQ @lwa_zmmt0201-nro_cg.
*
*    CHECK sy-subrc EQ 0.

    me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro = i_nfe_terceiro.

    "Datas e Pesos Anteriores
    DATA(dt_carregado)    = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-dt_carregado.
    DATA(nm_peso_bruto)   = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_bruto.
    DATA(nm_peso_tara)    = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_tara.
    DATA(nm_peso_liquido) = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_liquido.

    me->zif_int_ob_carguero_tn_apr_ter~at_viagem-dt_carregado    = i_dt_carregamento.
    me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_bruto   = i_peso_bruto.
    me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_tara    = i_peso_tara.
    me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_liquido = i_peso_liquido.

    "Mudou Peso/Data/Documento Transporte
    IF dt_carregado IS INITIAL AND nm_peso_tara IS INITIAL AND nm_peso_liquido IS INITIAL.
      DATA(ck_alteracao) = abap_false.
    ELSEIF dt_carregado    NE me->zif_int_ob_carguero_tn_apr_ter~at_viagem-dt_carregado    OR
           nm_peso_tara    NE me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_tara    OR
           nm_peso_bruto   NE me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_bruto   OR
           nm_peso_liquido NE me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_liquido .
      ck_alteracao = abap_true.
    ENDIF.

*--------------------------------------------------
*-- Valida status e Envio aprovacao
*--------------------------------------------------
    TRY .
        zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_instance(
          )->valida_envio_aprovacao(
               EXPORTING
                 i_viagem_id    = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-viagem_id
               IMPORTING
                 e_erro          = DATA(l_erro)
                 e_status        = DATA(l_status)
                 e_msg_erro      = DATA(l_msg_erro)
                 e_id_integracao = e_id_integracao
          ).

      CATCH zcx_integracao.
      CATCH zcx_error.
    ENDTRY.

    IF l_erro = abap_true.
      lva_msg_erro = l_msg_erro.
      zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).
    ENDIF.

*----------------------------------------
*-- Envia aprovacao troca nota carguero
*----------------------------------------
    CASE l_status.

      WHEN '08'.
        TRY.
            me->zif_int_ob_carguero_tn_apr_ter~get_json( IMPORTING e_json = DATA(l_json_tn)
                  )->set_ds_data(  EXPORTING i_json          = l_json_tn
                  )->set_ds_url(
                  )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
                  ).

          CATCH zcx_integracao.
            lva_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
            zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).

          CATCH zcx_error.
            lva_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
            zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).
        ENDTRY.

    ENDCASE.

    SELECT SINGLE *
      FROM zintegracao INTO @DATA(lwa_integracao)
     WHERE id_integracao EQ @e_id_integracao.

    IF sy-subrc EQ 0.
      TRANSLATE lwa_integracao-ds_data_retorno TO UPPER CASE.

      IF lwa_integracao-ds_data_retorno = 'NULL'.
        lva_msg_erro = 'Carguero: Retorno informe de carregamento nao pode ser null!'.
        zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).
      ENDIF.
    ENDIF.

    me->zif_int_ob_carguero_tn_apr_ter~at_viagem-ck_carregado           = abap_true.
    me->zif_int_ob_carguero_tn_apr_ter~at_viagem-id_integracao_carregar = e_id_integracao.

    MODIFY zlest0185 FROM me->zif_int_ob_carguero_tn_apr_ter~at_viagem.
    COMMIT WORK.

  ENDMETHOD.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~GET_ID_REFERENCIA.

    r_if_integracao_carregar = me.

    e_referencia-tp_referencia = 'CARGUERO_TROCA_NOTA_APROVAR'.
    e_referencia-id_referencia = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-viagem_id.

  endmethod.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~GET_INSTANCE.

    IF zif_int_ob_carguero_tn_apr_ter~at_if_integracao_carregar IS NOT BOUND.
      CREATE OBJECT zif_int_ob_carguero_tn_apr_ter~at_if_integracao_carregar TYPE zcl_int_ob_carguero_tn_apr_ter.
    ENDIF.
    r_if_integracao_carregar = zif_int_ob_carguero_tn_apr_ter~at_if_integracao_carregar.

  endmethod.


  METHOD zif_int_ob_carguero_tn_apr_ter~get_json.

    DATA: l_dt_emis    TYPE string,
          l_qtde_faturada TYPE string,
          l_peso_tara  TYPE string,
          l_peso_bruto TYPE string,
          l_url        TYPE string.

    r_if_integracao_carregar = me.
*
*    SELECT SINGLE *
*      FROM zib_nfe_dist_ter INTO @DATA(lwa_zib_nfe_dist_ter)
*     WHERE chave_nfe EQ @me->zif_int_ob_carguero_tn_apr_ter~chave_nfe_embarcador.
*
*    CHECK sy-subrc EQ 0.
*
*    SELECT SUM( prod_qtd_trib )
*      FROM zib_nfe_dist_itm INTO @DATA(lva_qtde_trib)
*     WHERE chave_nfe EQ @me->zif_int_ob_carguero_tn_apr_ter~chave_nfe_embarcador.

    CONCATENATE me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-dt_emissao(4)   '-'
                me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-dt_emissao+4(2) '-'
                me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-dt_emissao+6(2) 'T'
                me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-dt_emissao(2)   ':'
                me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-dt_emissao+2(2) ':'
                me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-dt_emissao+4(2) 'Z'
           INTO l_dt_emis.

    l_qtde_faturada = me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-qtde_faturada.
    l_peso_tara     = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_tara.
    l_peso_bruto    = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-nm_peso_bruto .

    l_url = zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_url_documento( EXPORTING i_viagem_id  = me->zif_int_ob_carguero_tn_apr_ter~at_viagem-viagem_id ).

    CHECK l_url IS NOT INITIAL.

    CONCATENATE '{"nota_fiscal":{'
                '"chave_acesso":"'       me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-chave_nfe '",'
                '"data_emissao":"'       l_dt_emis '",'
                '"quantidade_faturada":' l_qtde_faturada ','
                '"url_documento":"'      l_url '",'
                '"numero":"'             me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-numero '",'
                '"serie":"'              me->zif_int_ob_carguero_tn_apr_ter~at_nfe_terceiro-serie '"},'
                '"peso_tara":{'
                '"unidade_medida":"KG' '",'
                '"quantidade":'          l_peso_tara '},'
                '"peso_bruto":{'
                '"unidade_medida":"KG' '",'
                '"quantidade":'          l_peso_bruto '}'
                '}'
           INTO e_json.


  ENDMETHOD.


  METHOD zif_int_ob_carguero_tn_apr_ter~get_url_documento.

    DATA: lva_nro_carga TYPE zmmt0201-nro_cg.

    CLEAR: r_url, lva_nro_carga.

    SELECT SINGLE *
      FROM zlest0185 INTO @DATA(lwa_zlest0185)
     WHERE viagem_id EQ @i_viagem_id.

    CHECK sy-subrc EQ 0.

    "Check se é viagem Carga Entrada Insumos
    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
     WHERE viagem_id EQ @i_viagem_id.

    IF sy-subrc EQ 0.
      lva_nro_carga = lwa_zmmt0201-nro_cg.
    ENDIF.

    "Check se é viagem Carga Saida Insumos
    IF lva_nro_carga IS INITIAL.
      SELECT SINGLE *
        FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
       WHERE viagem_id EQ @i_viagem_id.

      IF sy-subrc EQ 0.
        lva_nro_carga = lwa_zsdt0133-nro_cg.
      ENDIF.
    ENDIF.

    CHECK lva_nro_carga IS NOT INITIAL.

    r_url = lwa_zlest0185-blob_path && '/DocumentosCarga' && lva_nro_carga && '.pdf'.

  ENDMETHOD.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~SET_DS_DATA.

    "Incluir Texto JSON para integração
    r_if_integracao_carregar = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  endmethod.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~SET_DS_URL.

    r_if_integracao_carregar = me.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_apr_car.

    IF sy-subrc EQ 0.

      SELECT SINGLE * INTO @DATA(wa_webservice)
        FROM zauth_webservice
       WHERE service = 'CARGUERO_HOST'.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                              msgno = zcx_integracao=>zcx_servico_http_config-msgno
                              attr1 = 'O'
                              attr2 = '01' )
            msgid  = zcx_integracao=>zcx_servico_http_config-msgid
            msgno  = zcx_integracao=>zcx_servico_http_config-msgno
            msgty  = 'E'
            msgv1  = 'O'
            msgv2  = '01'.
      ENDIF.
    ELSE.
*-#147361-31.07.2024-JT-inicio
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_webservice "*-#147361-31.07.2024-JT
        FROM zciot_webservice
       WHERE tipo    EQ 'O'
         AND servico EQ '01'.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                              msgno = zcx_integracao=>zcx_servico_http_config-msgno
                              attr1 = 'O'
                              attr2 = '01' )
            msgid  = zcx_integracao=>zcx_servico_http_config-msgid
            msgno  = zcx_integracao=>zcx_servico_http_config-msgno
            msgty  = 'E'
            msgv1  = 'O'
            msgv2  = '01'.
      ENDIF.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_carregar=>at_fc_processar_viagem_carrega.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'PUT'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && 'viagens/' && me->zif_int_ob_carguero_tn_apr_ter~at_viagem-viagem_id && '/carregamento/aprovar'.
    me->zif_int_ob_carguero_tn_apr_ter~set_id_referencia( ).

  endmethod.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    r_if_integracao_carregar = me.
    me->zif_int_ob_carguero_tn_apr_ter~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~SET_ID_VIAGEM.
  endmethod.


  METHOD zif_int_ob_carguero_tn_apr_ter~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_carregar = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
       )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
       )->set_outbound_msg(
       )->set_processar_retorno(
       )->set_integrar_retorno(
       )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
       )->free(
       ).

    CLEAR: lc_integrar.


  ENDMETHOD.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~VALIDA_CHAVE_NFE.
  endmethod.


  METHOD zif_int_ob_carguero_tn_apr_ter~valida_envio_aprovacao.

    DATA: l_id_ordem             TYPE zsdt0001-id_ordem,
          l_doc_rem              TYPE zsdt0001-doc_rem,                   "#150020-27.08.2024-JT
          l_fat_contingencia_ecc TYPE zsdt0001-fat_contingencia_ecc.

    r_if_integracao_carregar = me.

    FREE: e_erro,
          e_status,
          e_msg_erro,
          e_id_integracao.

*---------------------------------------
*   Status
*---------------------------------------

    "01	Carregamento
    "02	Liberação do Embarcador
    "03	Carregado (Pode ser Reenviado os dados não dé erro)
    "04	Descarregado
    "05	Cancelado
    "06	Rejeitado pelo Embarcador
    "07	Emissão de Nota Fiscal de Trânsito
    "08	Liberação do Carregamento
    "09	Carregamento Rejeitado pelo Embarcador
    "10	Carregamento
    "11	Resposta da Solicitação de Cancelamento
    "12	Carregamento
    "13	Resposta da Solicitação de Cancelamento
    "14	Cancelado
    "15 A. Documentos de Trânsito

*---------------------------------------
*-- Verificar se Ainda está Carregado
*---------------------------------------
    TRY.
        zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
          )->set_viagem_status(
               EXPORTING
                 i_viagem_id = i_viagem_id
               IMPORTING
                 e_status    = e_status
          ).

      CATCH zcx_integracao.
        e_erro     = abap_true.
        e_msg_erro = 'Não foi possível obter Status do Carguero!'.
        RETURN.
      CATCH zcx_error.
        e_erro     = abap_true.
        e_msg_erro = 'Não foi possível obter Status do Carguero!'.
        RETURN.
    ENDTRY.

*----------------------------------------
*-- Status do Carguero
*----------------------------------------
    CASE e_status.

      WHEN '08'.
*----------------------------------------
*------ Envia aprovacao troca nota carguero
*----------------------------------------
*        TRY.
*            me->zif_integracao_trocant_aprovar~get_json( EXPORTING i_ch_referencia = i_ch_referencia
*                                                         IMPORTING e_json          = DATA(l_json_tn)
*                  )->set_ds_data(  EXPORTING i_json          = l_json_tn
*                  )->set_ds_url(
*                  )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
*                  ).
*
*          CATCH zcx_integracao.
*            e_erro     = abap_true.
*            e_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
*            RETURN.
*          CATCH zcx_error.
*            e_erro     = abap_true.
*            e_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
*            RETURN.
*        ENDTRY.

      WHEN '15'.
*----------------------------------------
*------ valida doc transito
*----------------------------------------
*        IF zif_integracao_trocant_aprovar~valida_chave_nfe(
*                 EXPORTING i_ch_referencia = i_ch_referencia ) = abap_true.
*          e_erro     = abap_true.
*          e_msg_erro = 'Nota Fiscal do Embarcador foi alterada. Cancelar Viagem no Carguero'.
*          RETURN.
*        ENDIF.

      WHEN OTHERS.
        e_erro     = abap_true.
        e_msg_erro = 'Viagem Carguero deve estar como Liberação do Carregamento ou A.Doctos de Trânsito'.
        RETURN.

    ENDCASE.


  ENDMETHOD.


  method ZIF_INT_OB_CARGUERO_TN_APR_TER~VALIDA_REMOCAO_DOCS_CARGUERO.
  endmethod.
ENDCLASS.

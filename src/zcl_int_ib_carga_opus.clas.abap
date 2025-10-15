class ZCL_INT_IB_CARGA_OPUS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '220' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  class-methods CHECK_DUPLICIDADE_RECEBIMENTO
    importing
      !I_ZSDT0001 type ZSDT0001
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CHECK_DADOS_OV_PED_ROMANEIO
    importing
      !I_ZSDT0001 type ZSDT0001
    returning
      value(R_MSG_ERROR) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CARGA_OPUS IMPLEMENTATION.


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


  method zif_integracao_inbound~validar_dados_inbound.

    constants: c_delete type c length 50 value 'DELETE',
               c_post   type c length 50 value 'POST'.


    data: wa_zsdt0001 type zsdt0001. "BUG SOLTO 170899 / AOENNING / 20-03-2025


    "**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025
    "DATA: lit_data_inbound  TYPE zde_zsdt0001_carga_saida_t.
    data: lit_data_inbound  type zde_zsdt0001_carga_t.
    "**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025

    clear: r_msg_erro.

    if me->zif_integracao_inject~at_info_request_http-ds_metodo ne c_post   and
       me->zif_integracao_inject~at_info_request_http-ds_metodo ne c_delete.
      r_msg_erro = 'Metodo informado não reconhecido!'.
      return.
    endif.

    if i_data_inbound is initial.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      return.
    endif.

    /ui2/cl_json=>deserialize( exporting json = i_data_inbound changing data = lit_data_inbound ).

*-----------------------------------------------------------------------------------------------------------------------*
*   Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    loop at lit_data_inbound into data(lwa_data_inbound).

      data(_tabix) = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização/Delete
      case me->zif_integracao_inject~at_info_request_http-ds_metodo.
        when c_post or c_delete.

          if lwa_data_inbound-id_carga is initial.
            r_msg_erro = 'Id. Carga é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            return.
          endif.
      endcase.

      case me->zif_integracao_inject~at_info_request_http-ds_metodo.
        when c_post.

          loop at lwa_data_inbound-classificacao into data(lwa_classificacao).
            if lwa_classificacao-id_classificacao is initial.
              r_msg_erro = 'No objeto Classificação, Id. Classificação é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
              return.
            endif.
          endloop.

          loop at lwa_data_inbound-classificacao_result into data(lwa_classificacao_result).

            if lwa_classificacao_result-id_classificacao is initial.
              r_msg_erro = 'No objeto Classificação Result, Id. Classificação é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
              return.
            endif.

            if lwa_classificacao_result-tp_caracteristica is initial.
              r_msg_erro = 'No objeto Classificação Result, Tp. Caracteristica é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
              return.
            endif.

          endloop.

          loop at lwa_data_inbound-ordens_venda into data(lwa_ordem_venda).

            if lwa_ordem_venda-nr_ordem_venda is initial.
              r_msg_erro = 'No objeto Ordens Venda, Nr. Ordem Venda é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
              return.
            endif.

          endloop.

          loop at lwa_data_inbound-ordens_venda_romaneio into data(lwa_ordem_venda_romaneio).

            if lwa_ordem_venda_romaneio-nr_ordem_venda is initial.
              r_msg_erro = 'No objeto Ordens Venda Romaneio, Nr. Ordem Venda é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
              return.
            endif.

            if lwa_ordem_venda_romaneio-ch_referencia_sai is initial.
              r_msg_erro = 'No objeto Ordens Venda Romaneio, Ch. Referencia. Sai. é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
              return.
            endif.

          endloop.

**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025
          loop at lwa_data_inbound-romaneios into data(lwa_romaneio_inbound).
            clear: wa_zsdt0001.
            move-corresponding lwa_romaneio_inbound to wa_zsdt0001.
            wa_zsdt0001-vbeln = lwa_romaneio_inbound-nr_ordem_venda.
            wa_zsdt0001-matnr = lwa_romaneio_inbound-cd_material.
            r_msg_erro = zcl_int_ib_carga_opus=>check_dados_ov_ped_romaneio( i_zsdt0001 = wa_zsdt0001 ).
            if r_msg_erro is not initial.
              return.
            endif.
          endloop.
**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025

      endcase.
    endloop.
  endmethod.


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


  method zif_integracao_inject~set_integrar_inbound.

    constants: c_delete type c length 50 value 'DELETE',
               c_post   type c length 50 value 'POST'.

    data: lit_ib_romaneio   type table of zsds001,
          lwa_ib_romaneio   type zsds001,
          lwa_zsdt0001      type zsdt0001, "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP
          lit_romaneio_bloq type table of zsdt0001_bloq.


    data: lit_data_inbound      type zde_zsdt0001_carga_t,
          lwa_zsdt0001cg_gravar type zsdt0001cg.

    r_if_integracao_inject = me.

    clear: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lit_ib_romaneio[].

    if i_msg_inbound is not initial.
      /ui2/cl_json=>deserialize( exporting json = i_msg_inbound changing data = lit_data_inbound ).
    endif.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound ).
    if e_msg_erro is not initial.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      return.
    endif.

    data(_change_bd) = abap_false.

    loop at lit_data_inbound into data(lwa_data_inbound).

      delete from zsdt0001cg   where id_carga = lwa_data_inbound-id_carga.
      delete from zsdt0001cl   where id_carga = lwa_data_inbound-id_carga.
      delete from zsdt0001rs   where id_carga = lwa_data_inbound-id_carga.
      delete from zsdt0001ov   where id_carga = lwa_data_inbound-id_carga.
      delete from zsdt0001ovro where id_carga = lwa_data_inbound-id_carga.

      _change_bd = abap_true.

      case me->zif_integracao_inject~at_info_request_http-ds_metodo.
        when c_post.

          move-corresponding lwa_data_inbound to lwa_zsdt0001cg_gravar.

          select single *
            from zsdt0001cg into @data(lwa_zsdt0001cg)
           where id_carga  eq @lwa_zsdt0001cg_gravar-id_carga.

          if sy-subrc eq 0.
            lwa_zsdt0001cg_gravar-dt_registro = lwa_zsdt0001cg-dt_registro.
            lwa_zsdt0001cg_gravar-hr_registro = lwa_zsdt0001cg-hr_registro.
            lwa_zsdt0001cg_gravar-dt_update   = sy-datum.
            lwa_zsdt0001cg_gravar-hr_update   = sy-uzeit.
          else.
            lwa_zsdt0001cg_gravar-dt_registro = sy-datum.
            lwa_zsdt0001cg_gravar-hr_registro = sy-uzeit.
          endif.

          lwa_zsdt0001cg_gravar-id_integracao = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração

          modify zsdt0001cg from lwa_zsdt0001cg_gravar.

          if sy-subrc eq 0.
            _change_bd = abap_true.
          else.
            e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
            exit.
          endif.

          "Classificação
          loop at lwa_data_inbound-classificacao into data(lwa_classificacao).

            lwa_classificacao-id_carga = lwa_zsdt0001cg_gravar-id_carga.

            modify zsdt0001cl from lwa_classificacao.

            if sy-subrc eq 0.
              _change_bd = abap_true.
            else.
              e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
              exit.
            endif.

          endloop.

          "Classificação Resultado
          loop at lwa_data_inbound-classificacao_result into data(lwa_classificacao_result).

            lwa_classificacao_result-id_carga = lwa_zsdt0001cg_gravar-id_carga.

            modify zsdt0001rs from lwa_classificacao_result.

            if sy-subrc eq 0.
              _change_bd = abap_true.
            else.
              e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
              exit.
            endif.

          endloop.

          "Ordens Venda
          loop at lwa_data_inbound-ordens_venda into data(lwa_ordem_venda).

            lwa_ordem_venda-id_carga = lwa_zsdt0001cg_gravar-id_carga.

            modify zsdt0001ov from lwa_ordem_venda.

            if sy-subrc eq 0.
              _change_bd = abap_true.
            else.
              e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
              exit.
            endif.

          endloop.

          "Ordens Venda Romaneio
          loop at lwa_data_inbound-ordens_venda_romaneio into data(lwa_ordem_venda_romaneio).

            lwa_ordem_venda_romaneio-id_carga = lwa_zsdt0001cg_gravar-id_carga.

            modify zsdt0001ovro from lwa_ordem_venda_romaneio.

            if sy-subrc eq 0.
              _change_bd = abap_true.
            else.
              e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
              exit.
            endif.

          endloop.

          if e_msg_erro is not initial.
            exit.
          endif.

          loop at lwa_data_inbound-romaneios into data(lwa_romaneio_inbound).
            clear: lwa_ib_romaneio.

            lwa_ib_romaneio-tp_movimento            = lwa_romaneio_inbound-tp_movimento.
            lwa_ib_romaneio-ch_referencia           = lwa_romaneio_inbound-ch_referencia.
            lwa_ib_romaneio-nr_romaneio             = lwa_romaneio_inbound-nr_romaneio.
            lwa_ib_romaneio-vbeln                   = lwa_romaneio_inbound-nr_ordem_venda.
            lwa_ib_romaneio-dt_movimento            = lwa_romaneio_inbound-dt_movimento.
            lwa_ib_romaneio-nr_safra                = lwa_romaneio_inbound-nr_safra.
            lwa_ib_romaneio-bukrs                   = lwa_romaneio_inbound-id_empresa.
            lwa_ib_romaneio-branch                  = lwa_romaneio_inbound-id_filial.
            lwa_ib_romaneio-parid                   = lwa_romaneio_inbound-id_parceiro.
            lwa_ib_romaneio-id_cli_dest             = lwa_romaneio_inbound-id_cliente_destino.
            lwa_ib_romaneio-tp_frete                = lwa_romaneio_inbound-tp_frete.
            lwa_ib_romaneio-matnr                   = lwa_romaneio_inbound-cd_material.
            lwa_ib_romaneio-peso_liq                = lwa_romaneio_inbound-nr_peso_liquido.
            lwa_ib_romaneio-peso_fiscal             = lwa_romaneio_inbound-nr_peso_fiscal.
            lwa_ib_romaneio-placa_cav               = lwa_romaneio_inbound-pc_placa_cavalo.
            lwa_ib_romaneio-placa_car1              = lwa_romaneio_inbound-pc1_placa_carreta.
            lwa_ib_romaneio-placa_car2              = lwa_romaneio_inbound-pc2_placa_carreta.
            lwa_ib_romaneio-placa_car3              = lwa_romaneio_inbound-pc3_placa_carreta.
            lwa_ib_romaneio-motorista               = lwa_romaneio_inbound-id_motorista.
            lwa_ib_romaneio-nr_ticket               = lwa_romaneio_inbound-nr_ticket.
            lwa_ib_romaneio-dt_fechamento           = lwa_romaneio_inbound-dt_fechamento.
            lwa_ib_romaneio-hr_fechamento           = lwa_romaneio_inbound-hr_fechamento.
            lwa_ib_romaneio-nr_perc_umidade         = lwa_romaneio_inbound-nr_perc_umidade.
            lwa_ib_romaneio-nr_qtd_umidade          = lwa_romaneio_inbound-nr_qtde_umidade.
            lwa_ib_romaneio-nr_perc_impureza        = lwa_romaneio_inbound-nr_perc_impureza.
            lwa_ib_romaneio-nr_qtd_impureza         = lwa_romaneio_inbound-nr_qtde_impureza.
            lwa_ib_romaneio-nr_perc_avaria          = lwa_romaneio_inbound-nr_perc_avariado.
            lwa_ib_romaneio-nr_qtd_avaria           = lwa_romaneio_inbound-nr_qtde_avariado.
            lwa_ib_romaneio-nr_perc_ardido          = lwa_romaneio_inbound-nr_perc_ardido.
            lwa_ib_romaneio-nr_qtd_ardido           = lwa_romaneio_inbound-nr_qtde_ardido.
            lwa_ib_romaneio-nr_perc_quebra          = lwa_romaneio_inbound-nr_perc_quebrado.
            lwa_ib_romaneio-nr_qtd_quebra           = lwa_romaneio_inbound-nr_qtde_quebrado.
            lwa_ib_romaneio-nr_perc_esverd          = lwa_romaneio_inbound-nr_perc_esverdeado.
            lwa_ib_romaneio-nr_qtd_esverd           = lwa_romaneio_inbound-nr_qtde_esverdeado.
            lwa_ib_romaneio-doc_rem                 = lwa_romaneio_inbound-nu_documento_remessa.
            lwa_ib_romaneio-id_interface            = lwa_romaneio_inbound-id_interface.
            lwa_ib_romaneio-nfe                     = lwa_romaneio_inbound-in_nota_eletronica.
            lwa_ib_romaneio-nfnum                   = lwa_romaneio_inbound-nr_nota.
            lwa_ib_romaneio-docdat                  = lwa_romaneio_inbound-dt_emissao_nf.
            lwa_ib_romaneio-series                  = lwa_romaneio_inbound-nr_serie.
            lwa_ib_romaneio-netwr                   = lwa_romaneio_inbound-nr_valor_nf.
            lwa_ib_romaneio-dt_chegada              = lwa_romaneio_inbound-dt_chegada.
            lwa_ib_romaneio-peso_subtotal           = lwa_romaneio_inbound-nr_peso_subtotal.
            lwa_ib_romaneio-nr_nota_conta_ordem     = lwa_romaneio_inbound-nr_nota_conta_ordem.
            lwa_ib_romaneio-nr_serie_conta_ordem    = lwa_romaneio_inbound-nr_serie_conta_ordem.
            lwa_ib_romaneio-id_referencia           = lwa_romaneio_inbound-id_referencia.
            lwa_ib_romaneio-tp_transgenia           = lwa_romaneio_inbound-tp_transgenia.
            lwa_ib_romaneio-dt_abertura             = lwa_romaneio_inbound-dt_abertura.
            lwa_ib_romaneio-hr_abertura             = lwa_romaneio_inbound-hr_abertura.
            lwa_ib_romaneio-local_descarga          = lwa_romaneio_inbound-local_descarga.
            lwa_ib_romaneio-tipo_entrada            = lwa_romaneio_inbound-tipo_entrada.
            lwa_ib_romaneio-ch_refer_ent            = lwa_romaneio_inbound-ch_refer_ent.
            lwa_ib_romaneio-chave_nfe               = lwa_romaneio_inbound-chave_nfe.
            lwa_ib_romaneio-modal                   = lwa_romaneio_inbound-modal.
            lwa_ib_romaneio-peso_rateio_origem      = lwa_romaneio_inbound-peso_rateio_origem.
            lwa_ib_romaneio-id_ordem                = lwa_romaneio_inbound-id_ordem.
            lwa_ib_romaneio-cfop                    = lwa_romaneio_inbound-nr_cfop.
            lwa_ib_romaneio-nr_tk_guardian          = lwa_romaneio_inbound-nr_ticket_veiculo_automacao.
            lwa_ib_romaneio-qtde_remessa            = lwa_romaneio_inbound-nr_peso_liq_remess.
            lwa_ib_romaneio-um_remessa              = lwa_romaneio_inbound-um_remessa.
            lwa_ib_romaneio-peso_bruto              = lwa_romaneio_inbound-nr_peso_bruto.
            lwa_ib_romaneio-peso_tara               = lwa_romaneio_inbound-nr_peso_tara.
            lwa_ib_romaneio-peso_liq_comercial      = lwa_romaneio_inbound-nr_peso_liq_comercial.
            lwa_ib_romaneio-romaneio_completo       = lwa_romaneio_inbound-romaneio_completo. "// US-162952 wbartbosa 07/01/2025 INCLUSÃO DE CAMPO PARA ENVIO DO OPUS
            lwa_ib_romaneio-eudr                    = lwa_romaneio_inbound-eudr. "153343- [Inbound] - Ajuste API de Integração Romaneio - BG
            lwa_ib_romaneio-protocolo_eudr          = lwa_romaneio_inbound-protocolo_eudr. "153343- [Inbound] - Ajuste API de Integração Romaneio - BG

            append lwa_ib_romaneio to lit_ib_romaneio.
          endloop.


      endcase.

    endloop.

    if ( _change_bd = abap_true ) and ( e_msg_erro is initial ).

      loop at lit_ib_romaneio into lwa_ib_romaneio.

        "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP -->>>
        clear lwa_zsdt0001.
        move-corresponding  lwa_ib_romaneio to lwa_zsdt0001.
        "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP <<---

*        clear: lit_romaneio_bloq[].
*        call function 'ZSD_STATUS_BLOQUEIO_ROMANEIO'
*          exporting
*            ch_referencia = lwa_ib_romaneio-ch_referencia
*          tables
*            it_bloqueio   = lit_romaneio_bloq.
*
*        read table lit_romaneio_bloq into data(lwa_romaneio_bloq) index 1.
*        if sy-subrc eq 0 and lit_romaneio_bloq[] is not initial and lwa_romaneio_bloq-bloquear = 'S'.
*          rollback work.
*
*          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
*            exporting
*              input  = lwa_ib_romaneio-nr_romaneio
*            importing
*              output = lwa_ib_romaneio-nr_romaneio.
*
*
*          e_sucesso      = abap_true.
*          e_nm_code      = '400'.
*          e_msg_erro     = |{ lwa_romaneio_bloq-mensagem_bloqueio } - Romaneio Nro: { lwa_ib_romaneio-nr_romaneio } Chv.Ref: { lwa_ib_romaneio-ch_referencia }!|.
*          e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
*                           '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
*                           '}'.
*          return.
*        endif.

        "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP -->>>
        data(r_msg_error) = zcl_int_ib_carga_opus=>check_duplicidade_recebimento( i_zsdt0001 = lwa_zsdt0001 ).
        if r_msg_error is not initial.

          rollback work.

          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = lwa_ib_romaneio-nr_romaneio
            importing
              output = lwa_ib_romaneio-nr_romaneio.


          e_sucesso      = abap_true.
          e_nm_code      = '400'.
          e_msg_erro     = |{ r_msg_error } - Romaneio Nro: { lwa_ib_romaneio-nr_romaneio } Chv.Ref: { lwa_ib_romaneio-ch_referencia }!|.
          e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                           '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                           '}'.
          return.
        endif.
        "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP <<---

      endloop.

      call function 'ZSD_INBOUND_REMESSA'
        tables
          ib_romaneio = lit_ib_romaneio.

      commit work.

      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                   &&  '" '  && cl_abap_char_utilities=>newline &&
                       ' }'.
    else.

      rollback work.

      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
    endif.


  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD check_duplicidade_recebimento.

    DATA: lit_zsdt0001_check TYPE TABLE OF zsdt0001,
          lit_romaneio_bloq  TYPE TABLE OF zsdt0001_bloq.

    CLEAR: r_msg_error , lit_zsdt0001_check[].

    CHECK i_zsdt0001 IS NOT INITIAL.

    IF i_zsdt0001-ch_referencia IS NOT INITIAL.
      CLEAR: lit_romaneio_bloq[].
      CALL FUNCTION 'ZSD_STATUS_BLOQUEIO_ROMANEIO'
        EXPORTING
          ch_referencia = i_zsdt0001-ch_referencia
        TABLES
          it_bloqueio   = lit_romaneio_bloq.

      READ TABLE lit_romaneio_bloq INTO DATA(lwa_romaneio_bloq) INDEX 1.
      IF sy-subrc EQ 0 AND lit_romaneio_bloq[] IS NOT INITIAL AND lwa_romaneio_bloq-bloquear = 'S'.
        r_msg_error = lwa_romaneio_bloq-mensagem_bloqueio.
        RETURN.
      ENDIF.
    ENDIF.


    SELECT *
      FROM zsdt0001 INTO TABLE lit_zsdt0001_check
     WHERE bukrs        EQ i_zsdt0001-bukrs
       AND branch       EQ i_zsdt0001-branch
       AND nr_safra     EQ i_zsdt0001-nr_safra
       AND nr_romaneio  EQ i_zsdt0001-nr_romaneio
       AND tp_movimento EQ i_zsdt0001-tp_movimento
       AND id_interface EQ i_zsdt0001-id_interface. "Ajuste validação romaneio duplicados #IR206778 / AOENNING

    LOOP AT lit_zsdt0001_check INTO DATA(lwa_zsdt0001_check).

      CLEAR: lit_romaneio_bloq[].

      CALL FUNCTION 'ZSD_STATUS_BLOQUEIO_ROMANEIO'
        EXPORTING
          ch_referencia = lwa_zsdt0001_check-ch_referencia
        TABLES
          it_bloqueio   = lit_romaneio_bloq.

      READ TABLE lit_romaneio_bloq INTO lwa_romaneio_bloq INDEX 1.
      IF sy-subrc EQ 0 AND lit_romaneio_bloq[] IS NOT INITIAL AND lwa_romaneio_bloq-bloquear = 'S'.
        r_msg_error = lwa_romaneio_bloq-mensagem_bloqueio.
        RETURN.
      ENDIF.

      CALL FUNCTION 'ENQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia  = lwa_zsdt0001_check-ch_referencia
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc NE 0.
        r_msg_error = |Romaneio { lwa_zsdt0001_check-ch_referencia } em processamento no SAP!|.
        RETURN.
      ENDIF.

      DELETE FROM zsdt0001 WHERE ch_referencia = lwa_zsdt0001_check-ch_referencia.

      CALL FUNCTION 'DEQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia = lwa_zsdt0001_check-ch_referencia.

    ENDLOOP.


  ENDMETHOD.


  method check_dados_ov_ped_romaneio.
    clear: r_msg_error.
**>>>>>bug solto 170899 / aoenning / 20-03-2025
    "Verifica OV Romaneio Saida.
**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025
    if i_zsdt0001-tp_movimento eq 'S'.
      select single * from vbak into @data(wa_vbak)
        where vbeln eq @i_zsdt0001-vbeln.
      if sy-subrc ne 0.
        "Verificar pedido.
        select single * from ekko into @data(wa_ekko)
        where ebeln eq @i_zsdt0001-vbeln.
        if sy-subrc eq 0.
          "Seleção item do pedido.
          select single * from ekpo into @data(wa_ekpo)
          where ebeln eq @wa_ekko-ebeln
            and loekz eq @space
            and matnr eq @i_zsdt0001-matnr.
          if sy-subrc ne 0.
            if i_zsdt0001-vbeln+0(2) ne '99'.
              r_msg_error = |Ordem de venda/pedido de compra { i_zsdt0001-vbeln } não encontrado no SAP!|.
              return.
            endif.
          endif.
        else.
          if i_zsdt0001-vbeln+0(2) ne '99'.
            r_msg_error = |Ordem de venda/pedido de compra { i_zsdt0001-vbeln } não encontrado no SAP!|.
            return.
          endif.
        endif.
      endif.
    endif.
**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025
  endmethod.
ENDCLASS.

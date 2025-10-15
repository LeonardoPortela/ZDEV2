class ZCL_INT_IB_PARAM_IMPOSTO_SIGAM definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '108' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_PARAM_IMPOSTO_SIGAM IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

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

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.


  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.

    CONSTANTS: C_DELETE TYPE C LENGTH 50 VALUE 'DELETE',
               C_POST   TYPE C LENGTH 50 VALUE 'POST'.

    DATA: lwa_data_inbound  TYPE zde_zsdt0001_impostos.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST'.
      r_msg_erro = 'Metodo informado não reconhecido!'.
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_inbound ).

*-----------------------------------------------------------------------------------------------------------------------*
*   Tipo de Entrada X Tipo de Nota Fiscal
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-te_tn INTO DATA(lwa_te_tn).

      DATA(_tabix) = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_te_tn-id_entrada IS INITIAL.
            r_msg_erro = 'Id. Entrada é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_te_tn-id_tipo_nota IS INITIAL.
            r_msg_erro = 'Id. Tp. Nota é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_te_tn-ds_tipo_nota IS INITIAL.
            r_msg_erro = 'Ds. Tp. Nota é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.

*-----------------------------------------------------------------------------------------------------------------------*
*   Informações de Material do SIGAM
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-info_material INTO DATA(lwa_info_material).

      _tabix = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_info_material-matnr IS INITIAL.
            r_msg_erro = 'Id. Entrada é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_info_material-tp_grupo_ctb IS INITIAL.
            r_msg_erro = 'Grupo Contabil é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.


*-----------------------------------------------------------------------------------------------------------------------*
*   Impostos SIGAM por Tn e Grupo de Mercadoria
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-imp_tn_grupo_merc INTO DATA(lwa_imp_tn_grupo_merc).

      _tabix = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_imp_tn_grupo_merc-id_lanc_imposto IS INITIAL.
            r_msg_erro = 'Id. Lcto Imposto é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-id_tipo_nota IS INITIAL.
            r_msg_erro = 'Id. Tp. Nota é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-tp_grupo_ctb IS INITIAL.
            r_msg_erro = 'Tp. Grupo Ctb. é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-regio IS INITIAL.
            r_msg_erro = 'Estado é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-dt_inicial IS INITIAL.
            r_msg_erro = 'Data Inicial é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-dt_final IS INITIAL.
            r_msg_erro = 'Data Final é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-lifnr IS INITIAL.
            r_msg_erro = 'Fornecedor é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-ds_imposto IS INITIAL.
            r_msg_erro = 'Data Imposto é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc-tp_tributo IS INITIAL.
            r_msg_erro = 'Tipo Tributo é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.


      ENDCASE.

    ENDLOOP.


*-----------------------------------------------------------------------------------------------------------------------*
*   Impostos SIGAM por Tn e Grupo de Mercadoria - % QTD
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-imp_tn_grupo_merc_qtde INTO DATA(lwa_imp_tn_grupo_merc_qtde).

      _tabix = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_imp_tn_grupo_merc_qtde-id_lanc_imposto IS INITIAL.
            r_msg_erro = 'Id. Lcto Imposto é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc_qtde-matnr IS INITIAL.
            r_msg_erro = 'Material é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc_qtde-dt_inicial IS INITIAL.
            r_msg_erro = 'Data Inicial é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_imp_tn_grupo_merc_qtde-dt_final IS INITIAL.
            r_msg_erro = 'Data Final é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.

*-----------------------------------------------------------------------------------------------------------------------*
*   Situação Tributária SIGAM
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-situacao_trib INTO DATA(lwa_situacao_trib).

      _tabix = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_situacao_trib-id_situacao_trib  IS INITIAL.
            r_msg_erro = 'Id. Situacao Trib. é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_situacao_trib-tp_tributacao IS INITIAL.
            r_msg_erro = 'Tp. Tributação é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.

*-----------------------------------------------------------------------------------------------------------------------*
*   Taxa de Imposto por Imposto SIGAM
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-impostos_taxas INTO DATA(lwa_impostos_taxas).

      _tabix = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_impostos_taxas-id_lanc_imposto  IS INITIAL.
            r_msg_erro = 'Id. Lcto Imposto é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_impostos_taxas-dt_inicial IS INITIAL.
            r_msg_erro = 'Data Inicial é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_impostos_taxas-dt_final IS INITIAL.
            r_msg_erro = 'Data Final é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_impostos_taxas-nm_aliquota IS INITIAL.
            r_msg_erro = 'Aliquota é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.

*-----------------------------------------------------------------------------------------------------------------------*
*   Iva Entrada de Estoque
*-----------------------------------------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-iva_ent_estoque INTO DATA(lwa_iva_ent_estoque).

      _tabix = sy-tabix.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_iva_ent_estoque-id_empresa  IS INITIAL.
            r_msg_erro = 'Id. Empresa é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_iva_ent_estoque-id_tipo_nota IS INITIAL.
            r_msg_erro = 'Id.Tp.Nota é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_iva_ent_estoque-tp_grupo_ctb IS INITIAL.
            r_msg_erro = 'Tp.Grupo Ctb é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

          IF lwa_iva_ent_estoque-id_iva IS INITIAL.
            r_msg_erro = 'Id. IVA é um campo obrigatório!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.


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


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_inbound  TYPE zde_zsdt0001_impostos.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_inbound ).
    ENDIF.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).
    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    DATA(_change_bd) = abap_false.

***==================================================Inicio #IR176823 / aoenning.
    IF lwa_data_inbound-te_tn IS NOT INITIAL.
      DELETE FROM zsdt0001tetn.
    ENDIF.

    IF lwa_data_inbound-impostos_taxas IS NOT INITIAL.
      DELETE FROM zsdt0001imtx.
    ENDIF.

    IF lwa_data_inbound-iva_ent_estoque IS NOT INITIAL.
      DELETE FROM zsdt0001tetniva.
    ENDIF.

    IF lwa_data_inbound-imp_tn_grupo_merc IS NOT INITIAL.
      DELETE FROM zsdt0001im.
    ENDIF.

    IF lwa_data_inbound-info_material IS NOT INITIAL.
      DELETE FROM zsdt0001mt.
    ENDIF.

    IF lwa_data_inbound-imp_tn_grupo_merc_qtde IS NOT INITIAL.
      DELETE FROM zsdt0001imqt.
    ENDIF.

    IF lwa_data_inbound-situacao_trib IS NOT INITIAL.
      DELETE FROM zsdt0001st.
    ENDIF.
***==================================================

*-------------------------------------------------------------------------------------*
*   Tipo de Entrada X Tipo de Nota Fiscal
*-------------------------------------------------------------------------------------*
    LOOP AT lwa_data_inbound-te_tn INTO DATA(lwa_te_tn).

      lwa_te_tn-id_integracao = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
      lwa_te_tn-dt_registro = sy-datum.
      lwa_te_tn-hr_registro = sy-uzeit.

      MODIFY zsdt0001tetn FROM lwa_te_tn.

      IF sy-subrc EQ 0.
        _change_bd = abap_true.
      ELSE.
        e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
        EXIT.
      ENDIF.

    ENDLOOP.

*-------------------------------------------------------------------------------------*
*   Informações de Material do SIGAM
*-------------------------------------------------------------------------------------*
    IF e_msg_erro IS INITIAL.

      LOOP AT lwa_data_inbound-info_material INTO DATA(lwa_info_material).

        lwa_info_material-id_integracao       = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
        lwa_info_material-dt_registro = sy-datum.
        lwa_info_material-hr_registro = sy-uzeit.

        MODIFY zsdt0001mt FROM lwa_info_material.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

*-------------------------------------------------------------------------------------*
*   Impostos SIGAM por Tn e Grupo de Mercadoria
*-------------------------------------------------------------------------------------*
    IF e_msg_erro IS INITIAL.

      LOOP AT lwa_data_inbound-imp_tn_grupo_merc INTO DATA(lwa_imp_tn_grupo_merc).

        lwa_imp_tn_grupo_merc-id_integracao       = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
        lwa_imp_tn_grupo_merc-dt_registro = sy-datum.
        lwa_imp_tn_grupo_merc-hr_registro = sy-uzeit.

        MODIFY zsdt0001im FROM lwa_imp_tn_grupo_merc.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

*-------------------------------------------------------------------------------------*
*   Impostos SIGAM por Tn e Grupo de Mercadoria - % QTD
*-------------------------------------------------------------------------------------*
    IF e_msg_erro IS INITIAL.

      LOOP AT lwa_data_inbound-imp_tn_grupo_merc_qtde INTO DATA(lwa_imp_tn_grupo_merc_qtde).

        lwa_imp_tn_grupo_merc_qtde-id_integracao  = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
        lwa_imp_tn_grupo_merc_qtde-dt_registro    = sy-datum.
        lwa_imp_tn_grupo_merc_qtde-hr_registro    = sy-uzeit.

        MODIFY zsdt0001imqt FROM lwa_imp_tn_grupo_merc_qtde.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

*-------------------------------------------------------------------------------------*
*   Situação Tributária SIGAM
*-------------------------------------------------------------------------------------*
    IF e_msg_erro IS INITIAL.

      LOOP AT lwa_data_inbound-situacao_trib INTO DATA(lwa_situacao_trib).

        lwa_situacao_trib-id_integracao  = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
        lwa_situacao_trib-dt_registro    = sy-datum.
        lwa_situacao_trib-hr_registro    = sy-uzeit.

        MODIFY zsdt0001st FROM lwa_situacao_trib.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

*-------------------------------------------------------------------------------------*
*   Taxa de Imposto por Imposto SIGAM
*-------------------------------------------------------------------------------------*
    IF e_msg_erro IS INITIAL.

      LOOP AT lwa_data_inbound-impostos_taxas INTO DATA(lwa_impostos_taxas).

        lwa_impostos_taxas-id_integracao  = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
        lwa_impostos_taxas-dt_registro    = sy-datum.
        lwa_impostos_taxas-hr_registro    = sy-uzeit.

        MODIFY zsdt0001imtx FROM lwa_impostos_taxas.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

*-------------------------------------------------------------------------------------*
*   Iva Entrada de Estoque
*-------------------------------------------------------------------------------------*
    IF e_msg_erro IS INITIAL.

      LOOP AT lwa_data_inbound-iva_ent_estoque INTO DATA(lwa_iva_ent_estoque).

        lwa_iva_ent_estoque-id_integracao  = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
        lwa_iva_ent_estoque-dt_registro    = sy-datum.
        lwa_iva_ent_estoque-hr_registro    = sy-uzeit.

        MODIFY zsdt0001tetniva FROM lwa_iva_ent_estoque.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.


    IF ( _change_bd = abap_true ) AND ( e_msg_erro IS INITIAL ).

      COMMIT WORK.

      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                   &&  '" '  && cl_abap_char_utilities=>newline &&
                       ' }'.
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


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.

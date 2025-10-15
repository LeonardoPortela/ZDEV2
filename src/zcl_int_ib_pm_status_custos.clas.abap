class ZCL_INT_IB_PM_STATUS_CUSTOS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '204' ##NO_TEXT.

  methods CONSTRUCTOR .
  methods CONSULTA_STATUS_E_CUSTOS
    importing
      !I_IMPORTA_DADOS type ZTPM_D_M_STATUS_CUSTO_T
    exporting
      !E_RETORNO type ZTPM_D_M_STATUS_CUST_RETORNO_T .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_PM_STATUS_CUSTOS IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD consulta_status_e_custos.

    DATA: lt_return       TYPE TABLE OF bapiret2,
          lv_msg          TYPE char255,
          lt_costs_detail TYPE TABLE OF  bapi_alm_order_costs_detail_e,
          ls_header       TYPE bapi_alm_order_header_e,
          lt_status       TYPE TABLE OF jstat.

    LOOP AT i_importa_dados INTO DATA(wa_dados).

        SELECT SINGLE iloan
        FROM iloa
        INTO @DATA(lv_iloan)
        WHERE eqfnr = @wa_dados-sas.

        IF sy-subrc = 0.

          SELECT SINGLE aufnr
          FROM afih
          INTO @DATA(lv_aufnr)
          WHERE iloan = @lv_iloan.

          IF sy-subrc <> 0.
            CLEAR lv_aufnr.
          ENDIF.

        ENDIF.

        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number           = lv_aufnr
          IMPORTING
            es_header        = ls_header
          TABLES
            et_costs_details = lt_costs_detail
            return           = lt_return.

        IF lt_return IS INITIAL.

          CALL FUNCTION 'STATUS_READ'
            EXPORTING
              objnr            = ls_header-object_no
              only_active      = 'X'
            TABLES
              status           = lt_status
            EXCEPTIONS
              object_not_found = 1
              OTHERS           = 2.
          IF sy-subrc <> 0.
            CLEAR lt_status.
          ENDIF.

          SELECT istat, txt04, txt30
          FROM tj02t
          INTO TABLE @DATA(lt_tj02t)
          WHERE spras = @sy-langu
            AND istat IN ('I0001', 'I0002', 'I0045', 'I0046' ).

          DELETE lt_status WHERE stat <> 'I0001' AND
                                 stat <> 'I0002' AND
                                 stat <> 'I0045' AND
                                 stat <> 'I0046'.

          LOOP AT lt_status INTO DATA(wa_status).

            READ TABLE lt_tj02t INTO DATA(wa_tj02t) WITH KEY istat = wa_status-stat.
            IF sy-subrc = 0.
              EXIT.
            ENDIF.
          ENDLOOP.

          READ TABLE lt_costs_detail INTO DATA(wa_costs) INDEX 1.
          APPEND VALUE #( success = abap_true
                          aufnr = lv_aufnr
                          status = wa_tj02t-txt04
                          descricao = wa_tj02t-txt30
                          custos_reais = wa_costs-costs_act
                          msg_error = space
                         ) TO e_retorno.

        ELSE.

          READ TABLE lt_return WITH KEY type = 'E' INTO DATA(wa_return).
          IF sy-subrc <> 0.
            CLEAR wa_return.
          ENDIF.

          APPEND VALUE #( success = abap_false
                          aufnr = lv_aufnr
                          status = space
                          descricao = space
                          custos_reais = space
                          msg_error = wa_return-message
                         ) TO e_retorno.
        ENDIF.

    ENDLOOP.

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

    DATA: lwa_data_request TYPE ztpm_d_m_status_custo_t.

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
*    IF lwa_data_request-empresa IS INITIAL.
*      r_msg_erro = 'Empresa não Informada!'.
*      e_status_code = '402'. "Payment Required
*      RETURN.
*
*      ELSEIF lwa_data_request-mes_ano IS INITIAL.
*      r_msg_erro = 'Mes_Ano não Informado!'.
*      e_status_code = '402'. "Payment Required
*      RETURN.
*
*     ELSEIF lwa_data_request-moeda IS INITIAL.
*      r_msg_erro = 'Moeda não Informada!'.
*      e_status_code = '402'. "Payment Required
*      RETURN.
*
*      else.
*
*    ENDIF.

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

    DATA: lwa_data_request  TYPE ztpm_d_m_status_custo_t,
          lwa_data_retorno  TYPE ztpm_d_m_status_cust_retorno_t,
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

    me->consulta_status_e_custos(
      EXPORTING
        i_importa_dados = lwa_data_request    " Número SAS
      IMPORTING
        e_retorno       = lwa_data_retorno   " Status da ordem e custos
    ).

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_retorno ). "enviar para essa tabela lwa_data_response

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

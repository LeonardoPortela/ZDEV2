class ZCL_INT_OB_SOBRA_MATERIAL_OPUS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

data:
BEGIN OF zde_data_response,
        QTDE_SOBRA_OPUS type BSTMG,
      END OF zde_data_response .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '125' ##NO_TEXT.
  data AT_DADOS_ZMMT0086 type TABLE OF ZDE_ZMMT0086 .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_SOBRA_MATERIAL_OPUS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

   me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_sim.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'interface'.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    DATA: lwa_data_response LIKE zde_data_response,
          tl_zmmt0086       TYPE TABLE OF zmmt0086.

    r_if_integracao_inject = me.
    "E_SUCESSO = ABAP_TRUE.

*    IF i_msg_inbound IS NOT INITIAL.
*      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING  data = lwa_data_response ).
*    ENDIF.

    "IF lwa_data_response IS NOT INITIAL.

      tl_zmmt0086 = me->at_dados_zmmt0086.

      LOOP AT tl_zmmt0086 INTO DATA(w_zmmt0086).
        UPDATE zmmt0086 SET st_envio_opus = '2'
               sobra_opus = w_zmmt0086-sobra
            WHERE ebeln = w_zmmt0086-ebeln
              AND ebelp = w_zmmt0086-ebelp.

      ENDLOOP.
   " ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    "e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.



    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    DATA: tl_zmmt0086       TYPE TABLE OF zmmt0086.

    r_if_integracao_inject = me.

      tl_zmmt0086 = me->at_dados_zmmt0086.

      LOOP AT tl_zmmt0086 INTO DATA(w_zmmt0086).
        UPDATE zmmt0086 SET st_envio_opus = '2'
               sobra_opus = w_zmmt0086-sobra
            WHERE ebeln = w_zmmt0086-ebeln
              AND ebelp = w_zmmt0086-ebelp.

      ENDLOOP.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.

    DATA: lwa_sobra_mat TYPE TABLE OF zmmt0086.
          "lwa_busca_sobra_mat TYPE zmmt0086.

    r_if_integracao_outbound = me.

   APPEND i_info_request TO lwa_sobra_mat.


*    select single * from zmmt0086 into lwa_sobra_mat
*      where ebelp = lwa_sobra_mat-ebelp
*      and ebeln = lwa_sobra_mat-ebeln.

      me->at_dados_zmmt0086 = lwa_sobra_mat.
      "me->at_dados_zmmt0086 = zcl_due=>zif_due~get_due( i_due = lwa_sobra_mat-ebeln ).


  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST( I_INFO_REQUEST = I_INFO_REQUEST
      )->GET_DATA( IMPORTING E_DATA = DATA(LC_DATA)
      )->SET_DATA( EXPORTING I_DATA = LC_DATA
      )->SET_URL(
      )->SET_ID_REFERENCIA(
      )->SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO	= E_INTEGRACAO
      ).

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_DATA.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    CALL METHOD /ui2/cl_json=>serialize
       EXPORTING
         data   = me->at_dados_zmmt0086
       RECEIVING
         r_json = e_data.


  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'SOBRA_MAT_OPUS'.
    "e_referencia-id_referencia = me->at_dados_zmmt0086-EBELN .
    "concatenate me->at_dados_zmmt0086-EBELN '-' me->at_dados_zmmt0086-EBELP INTO e_referencia-id_referencia.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE ZCL_INT_OB_SOBRA_MATERIAL_OPUS.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = ME.

    ME->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  METHOD zif_integracao_outbound~set_url.

    DATA: lva_url       TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'ENVIO_SOBRA_MATERIAL_OPUS'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'ENVIO_SOBRA_MATERIAL_OPUS' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'ENVIO_SOBRA_MATERIAL_OPUS'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    lva_url       = lwa_webservice-url.
    lva_url_token = lwa_webservice-url_token.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url              = lva_url.
    me->zif_integracao_inject~at_info_request_http-ds_url_token        = lva_url_token.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.
ENDCLASS.

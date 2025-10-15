class ZCL_INT_IB_FI_SIS_INDIC definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '200' ##NO_TEXT.
  data AT_DADOS_SIS type ZFIE0205 .
  data AT_CD_INDIC type ZSISCD_INDIC .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_FI_SIS_INDIC IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_nao.
    "me->zif_integracao_inject~at_autentica_module  = 'OPUS'.

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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
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

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.


    DATA: lwa_due TYPE zsdt0170.

    r_if_integracao_outbound = me.

    MOVE-CORRESPONDING i_info_request TO me->at_dados_sis.
    me->at_cd_indic = me->at_dados_sis-cd_indic.
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


  METHOD zif_integracao_outbound~get_data.
    r_if_integracao_outbound = me.
    data aux_indc type i.
        CLEAR: e_data,aux_indc.
    aux_indc = me->at_dados_sis-cd_indic.
    CASE aux_indc.

      WHEN 5683.
        TYPES: BEGIN OF ty_5683,
                 reference   TYPE string,
                 unit_id     TYPE i,
                 status      TYPE i,
                 fld11556318 TYPE string,
                 fld11556322 TYPE string,
                 fld11556324 TYPE string,
                 fld11556326 TYPE string,
               END OF ty_5683.

        DATA: lr_5683 TYPE ty_5683.

        lr_5683-reference = me->at_dados_sis-col_a.
        lr_5683-unit_id = ''.
        lr_5683-status = 6.
        lr_5683-fld11556318 = me->at_dados_sis-col_c.
        lr_5683-fld11556322 = me->at_dados_sis-col_d.
        lr_5683-fld11556322 = me->at_dados_sis-col_e.
        lr_5683-fld11556326 = me->at_dados_sis-col_f.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = lr_5683
      RECEIVING
        r_json = e_data.


      WHEN 5658.
        TYPES: BEGIN OF ty_5658,
                 reference   TYPE string,
                 unit_id     TYPE i,
                 status      TYPE i,
                 fld11563679 TYPE string,
                 fld11623133 TYPE string,
                 fld11623135 TYPE string,
                 fld11623137 TYPE string,
               END OF ty_5658.

        DATA: lr_5658 TYPE ty_5658.

        lr_5658-reference = me->at_dados_sis-col_a.
        lr_5658-unit_id = ''.
        lr_5658-status = 6.
        lr_5658-fld11563679 = me->at_dados_sis-col_c.
        lr_5658-fld11563679 = me->at_dados_sis-col_d.
        lr_5658-fld11623135 = me->at_dados_sis-col_e.
        lr_5658-fld11623137 = me->at_dados_sis-col_f.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = lr_5658
      RECEIVING
        r_json = e_data.

      WHEN 6710.
        TYPES: BEGIN OF ty_6710,
                 reference   TYPE string,
                 unit_id     TYPE i,
                 status      TYPE i,
                 fld11634177 TYPE string,
                 fld11634178 TYPE string,
                 fld11634179 TYPE string,
                 fld11634192 TYPE string,
               END OF ty_6710.

                 DATA: lr_6710 TYPE ty_6710.

        lr_6710-reference = me->at_dados_sis-col_a.
        lr_6710-unit_id = ''.
        lr_6710-status = 6.
        lr_6710-fld11634177 = me->at_dados_sis-col_c.
        lr_6710-fld11634178 = me->at_dados_sis-col_d.
        lr_6710-fld11634179 = me->at_dados_sis-col_e.
        lr_6710-fld11634192 = me->at_dados_sis-col_f.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = lr_6710
      RECEIVING
        r_json = e_data.

      WHEN 5638.
        TYPES: BEGIN OF ty_5638,
                 reference   TYPE string,
                 unit_id     TYPE i,
                 status      TYPE i,
                 fld11562061 TYPE string,
                 fld11562063 TYPE string,
                 fld11562065 TYPE string,
                 fld11562093 TYPE string,
               END OF ty_5638.

                 DATA: lr_5638 TYPE ty_5638.

        lr_5638-reference = me->at_dados_sis-col_a.
        lr_5638-unit_id = ''.
        lr_5638-status = 6.
        lr_5638-fld11562061 = me->at_dados_sis-col_c.
        lr_5638-fld11562063 = me->at_dados_sis-col_d.
        lr_5638-fld11562065 = me->at_dados_sis-col_e.
        lr_5638-fld11562093 = me->at_dados_sis-col_f.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = lr_5638
      RECEIVING
        r_json = e_data.

    ENDCASE.



  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'CRIAR_RESPOSTA_SIS'.
    "e_referencia-id_referencia = me->at_dados_SIS.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE ZCL_INT_IB_FI_SIS_INDIC.
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

    DATA: lva_val TYPE c,
          lva_url TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'CRIAR_RESPOSTA_SIS'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            "attr1 = ''
                            "attr2 = ''
                            )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'CRIAR_RESPOSTA_SIS'.
    ENDIF.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_header_fields =  VALUE #( ( name = 'Authorization' value = me->zif_integracao_outbound~at_auth_webservice-token ) ).
    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_url              = |{ lwa_webservice-url }{ me->at_cd_indic }{ lwa_webservice-url_token }|.

    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.
ENDCLASS.

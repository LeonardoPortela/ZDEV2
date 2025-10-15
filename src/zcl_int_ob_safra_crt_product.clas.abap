class ZCL_INT_OB_SAFRA_CRT_PRODUCT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '291' ##NO_TEXT.
  constants:
    lc_servico TYPE c LENGTH 21 value 'PRODUCT_SAFRA_CONTROL' ##NO_TEXT.
  constants:
    lc_authorization TYPE c LENGTH 13 value 'Authorization' ##NO_TEXT.
  constants:
    lc_erro          TYPE c LENGTH 01 value 'E' ##NO_TEXT.
  constants:
    lc_interface TYPE c LENGTH 09 value 'interface' ##NO_TEXT.
  constants:
*    DATA at_body TYPE zde_safra_control_ .
*    DATA at_externalid TYPE zde_safra_od_externalid .
    lc_content_type  TYPE c LENGTH 04 value 'JSON' ##NO_TEXT.
  data AT_BODY type ZDE_SAFRA_CONTROL_PRODUCTS .
  data AT_DS_BODY type STRING .
  data AT_EXTERNALID type ZDE_SAFRA_EXTID_PRD .
  data AT_MATNR type MATNR .
  data AT_ID_REFERENCIA type STRING .
  data AT_MARA type MARA .
  data AT_MAKT type MAKT .
  data AT_MARC type MARC .
  constants C_TRUE type STRING value 'true' ##NO_TEXT.
  constants C_FALSE type STRING value 'false' ##NO_TEXT.

  methods CONSTRUCTOR .
  methods SET_METODO_HTTP
    importing
      !I_METODO type STRING .
  methods GET_METODO_HTTP
    returning
      value(E_METODO) type STRING .
  methods SET_MONTAR_JSON
    returning
      value(R_DATA) type ZDE_SAFRA_CONTROL_PRODUCTS .
protected section.
private section.

  data AT_METODO_HTTP type STRING .

  methods SET_VALIDAR_REGISTRO
    returning
      value(R_VALIDADO) type CHAR01 .
ENDCLASS.



CLASS ZCL_INT_OB_SAFRA_CRT_PRODUCT IMPLEMENTATION.


  method CONSTRUCTOR.
    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = me->lc_interface.
  endmethod.


  method GET_METODO_HTTP.
    e_metodo = at_metodo_http.
  endmethod.


  method SET_METODO_HTTP.
    at_metodo_http = i_metodo.
  endmethod.


  METHOD set_montar_json.

    DATA: lv_peso   TYPE i.

    SELECT SINGLE wgbez
      INTO @DATA(_wgbez)
      FROM t023t
     WHERE spras = @sy-langu
       AND matkl = @me->at_mara-matkl.

    IF sy-subrc <> 0.
      CLEAR _wgbez.
    ENDIF.

    lv_peso                           = me->at_mara-brgew.

    me->at_body-externalid            = |{ me->at_mara-matnr ALPHA = OUT }|.
    me->at_body-name                  = me->at_makt-maktx.
    me->at_body-weight                = lv_peso.
    me->at_body-unit                  = me->at_mara-meins.
    me->at_body-conversionfactor      = 1000.
    me->at_body-multiple              = 1.
    me->at_body-ncm                   = me->at_marc-steuc.
    me->at_body-group-externalid      = me->at_mara-matkl.
    me->at_body-group-name            = _wgbez.
    me->at_body-group-enabled         = c_true.
    me->at_body-variety-externalid    = _wgbez. "me->at_mara-wrkst.
    me->at_body-variety-name          = _wgbez. "me->at_mara-wrkst.
    me->at_body-supplier-externalid   = me->at_mara-matnr.

    CONDENSE me->at_body-externalid NO-GAPS.

    r_data                            = me->at_body.

  ENDMETHOD.


  METHOD set_validar_registro.

    r_validado = abap_false.

    SELECT SINGLE matkl, mtart
      INTO @DATA(_mara)
      FROM mara
     WHERE matnr = @me->at_matnr.

    SELECT SINGLE werks
      INTO @DATA(_werks)
      FROM mard
     WHERE matnr = @me->at_matnr.

    SELECT SINGLE vkorg
      INTO @DATA(_vkorg)
      FROM t001w
     WHERE werks = @_werks.

    SELECT SINGLE guid
      INTO @DATA(_guid)
      FROM zsdt0417
     WHERE matkl  = @_mara-matkl
       AND bukrs  = @_vkorg
       AND cancel = @abap_off.

    IF sy-subrc <> 0.
      SELECT SINGLE guid
        INTO      @_guid
        FROM zsdt0417
       WHERE mtart  = @_mara-mtart
         AND bukrs  = @_vkorg
         AND cancel = @abap_off.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    r_validado = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.

    r_if_integracao_outbound = me.

*    IF at_metodo_http EQ zif_integracao_inject=>co_request_method_post.
*      MOVE-CORRESPONDING i_info_request TO at_body.
*    ELSE.
*      MOVE-CORRESPONDING i_info_request TO at_externalid.
*    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    DATA: ls_material TYPE zde_int_ob_product.

    r_if_integracao_outbound = me.

    ls_material          = i_info_request.
    me->at_matnr         = ls_material-matnr.
    me->at_metodo_http   = ls_material-metodo.
    me->at_id_referencia = |{ me->at_matnr ALPHA = OUT }|.

    CONDENSE me->at_id_referencia NO-GAPS.

    CHECK set_validar_registro( ) = abap_true.

    "// Inclui Json na Mensagem a Ser Enviada
    me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
      )->get_data( IMPORTING e_data = DATA(lc_data)
      )->set_data( EXPORTING i_data = lc_data
      )->set_url(
      )->set_id_referencia(
      )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).
  ENDMETHOD.


  METHOD zif_integracao_outbound~get_data.

    CLEAR: e_data.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      INTO me->at_mara
      FROM mara
     WHERE matnr = me->at_matnr.

    SELECT SINGLE *
      INTO me->at_makt
      FROM makt
     WHERE matnr = me->at_matnr
       AND spras = sy-langu.

    SELECT SINGLE *
      INTO me->at_marc
      FROM marc
     WHERE matnr = me->at_matnr.

    CHECK me->at_metodo_http <> zif_integracao_inject=>co_request_method_get.

    me->at_body = set_montar_json( ).

    /ui2/cl_json=>serialize( EXPORTING data   = me->at_body RECEIVING r_json = e_data ).

    TRANSLATE e_data TO LOWER CASE.

    REPLACE ALL OCCURRENCES OF 'externalid' IN e_data WITH 'externalId'.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound   = me.
    e_referencia-tp_referencia = 'OB_SAFRA_PRODUCT'.
    e_referencia-id_referencia = me->at_id_referencia.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_safra_crt_product.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.
  ENDMETHOD.


  METHOD zif_integracao_outbound~send_msg.
    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_outbound = me.

    CREATE OBJECT lc_integrar.

    "// Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.


  METHOD zif_integracao_outbound~set_data.

    r_if_integracao_outbound = me.

    FREE: me->at_ds_body.

    CASE me->at_metodo_http.

      WHEN zif_integracao_inject=>co_request_method_post.
        me->at_ds_body                = i_data.

      WHEN zif_integracao_inject=>co_request_method_put.
        me->at_ds_body                = i_data.
        me->at_externalid-external_id = |{ me->at_matnr ALPHA = OUT }|.
        me->at_externalid-upsert      = c_false.

      WHEN zif_integracao_inject=>co_request_method_get.
        me->at_externalid-external_id = |{ me->at_matnr ALPHA = OUT }|.
    ENDCASE.

    CONDENSE me->at_externalid-external_id NO-GAPS.

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_id_referencia.
    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

    r_if_integracao_outbound = me.

    DATA(lv_metodo)     = me->at_metodo_http.
    DATA(lv_externalid) = me->at_externalid-external_id.
    DATA(lv_upsert)     = me->at_externalid-upsert.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO @DATA(lwa_webservice)
     WHERE service EQ @me->lc_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = CONV #( TEXT-001 ) "// Serviço não configurado:
                            attr2 = CONV #( me->lc_servico ) )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = me->lc_erro "// E
          msgv1  = CONV #( TEXT-001 ) "// Serviço não configurado:
          msgv2  = CONV #( me->lc_servico ).
    ENDIF.

    CASE lv_metodo.
      WHEN zif_integracao_inject=>co_request_method_post.
*        https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/products
        DATA(v_url) = |{ lwa_webservice-url }|.

      WHEN zif_integracao_inject=>co_request_method_get.
*        https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/products/ab123cd
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.

      WHEN zif_integracao_inject=>co_request_method_put.
*        https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/products/ab123cd?upsert=false
        v_url = |{ lwa_webservice-url }/{ lv_externalid }?upsert={ lv_upsert }|.

      WHEN zif_integracao_inject=>co_request_method_delete.
*        https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/products/ab123cd
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.
    ENDCASE.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http =
    VALUE #(
                ds_formato            = me->lc_content_type
                ds_content_type       = lwa_webservice-content_type
                ds_url                = v_url
                ds_body               = me->at_ds_body
*               ds_url_token          = lwa_webservice-token
                ds_metodo             = lv_metodo
                ds_not_content_length = abap_false
    ).

    "// Informação do header
    APPEND VALUE #( name = me->lc_authorization value = lwa_webservice-token ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.

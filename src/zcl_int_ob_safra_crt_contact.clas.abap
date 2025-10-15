class ZCL_INT_OB_SAFRA_CRT_CONTACT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '290' ##NO_TEXT.
  constants:
    lc_servico TYPE c LENGTH 21 value 'CONTACT_SAFRA_CONTROL' ##NO_TEXT.
  constants:
    lc_authorization TYPE c LENGTH 13 value 'Authorization' ##NO_TEXT.
  constants:
    lc_erro          TYPE c LENGTH 01 value 'E' ##NO_TEXT.
  constants:
    lc_interface TYPE c LENGTH 09 value 'interface' ##NO_TEXT.
  constants LC_ESTRUTURA_JSON type TABNAME value 'ZDE_SAFRACONTROL_CONTATCS' ##NO_TEXT.
  data AT_TIPO_PARCEIRO type CHAR01 .
  data AT_NR_ROT type Z_NR_ROT .
  data AT_TXJCD type TXJCD .
  data AT_BODY type ZDE_SAFRACONTROL_CONTATCS .
  data AT_DS_BODY type STRING .
  data AT_TYPE_PUT type STRING .
  data AT_EXTERNALID type ZDE_SAFRA_EXTID_CONTROL .
  constants:
    lc_content_type  TYPE c LENGTH 04 value 'JSON' ##NO_TEXT.
  data AT_METODO_HTTP type STRING .
  data AT_COD_PARCEIRO type KUNNR .
  data AT_ID_REFERENCIA type STRING .
  constants C_TRUE type STRING value 'true' ##NO_TEXT.
  constants C_FALSE type STRING value 'false' ##NO_TEXT.
  data AT_ZI_PARCEIROS type ZI_SD_PARCEIROS_SAFRA_INT .
  data AT_T_PARCEIROS type ZSDE_PARCEIROS_SAFRA_T .
  data AT_ZI_ENDERECOS type ZI_SD_PARCEIROS_END_ADD .
  data AT_T_ZSDT0418 type ZSDT0418_T .
  data AT_ZSDT0132 type ZSDT0132 .

  methods CONSTRUCTOR .
  methods SET_METODO_HTTP
    importing
      !I_METODO type STRING .
  methods GET_METODO_HTTP
    returning
      value(E_METODO) type STRING .
  methods GET_EXTERNAL_ID
    importing
      !I_PARCEIRO type KUNNR
      !I_TIPO_PARCEIRO type CHAR01
      !I_NR_ROT type Z_NR_ROT optional
    exporting
      !E_INTEGRAR type CHAR01
    returning
      value(R_EXTERNAL_ID) type STRING .
protected section.
private section.

  data AT_PARCEIROS type ZSDE_PARCEIROS_SAFRA .

  methods SET_MONTAR_JSON
    returning
      value(R_DATA) type ZDE_SAFRACONTROL_CONTATCS .
  methods SET_TRATAR_TAGS
    changing
      !C_JSON type STRING .
  methods SET_TRATAMENTO_TAGS
    changing
      !C_JSON type STRING .
ENDCLASS.



CLASS ZCL_INT_OB_SAFRA_CRT_CONTACT IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = me->lc_interface.
  ENDMETHOD.


  METHOD get_metodo_http.
    e_metodo = at_metodo_http.
  ENDMETHOD.


  METHOD set_metodo_http.
    at_metodo_http = i_metodo.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
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

    DATA: ls_contato TYPE zde_int_ob_contact.

    r_if_integracao_outbound = me.

    FREE: me->at_tipo_parceiro, me->at_t_zsdt0418, me->at_zsdt0132.

    ls_contato           = i_info_request.

    me->at_cod_parceiro  = ls_contato-parceiro.
    me->at_metodo_http   = ls_contato-metodo.
    me->at_tipo_parceiro = ls_contato-tipo_parceiro.
    me->at_nr_rot        = ls_contato-nr_rot.
    me->at_id_referencia = me->get_external_id( i_parceiro = ls_contato-parceiro i_tipo_parceiro = ls_contato-tipo_parceiro i_nr_rot = ls_contato-nr_rot ).

    SELECT *
      INTO TABLE me->at_t_zsdt0418
      FROM zsdt0418.

    SELECT SINGLE tipo
      INTO @me->at_tipo_parceiro
      FROM zi_sd_parceiros_safra_int
     WHERE parceiro = @ls_contato-parceiro
       AND tipo     = @ls_contato-tipo_parceiro.

    CHECK sy-subrc = 0.

    IF me->at_nr_rot IS NOT INITIAL.
      SELECT SINGLE *
        INTO @me->at_zsdt0132
        FROM zsdt0132
       WHERE nr_rot   = @me->at_nr_rot.

      IF NOT ( ( me->at_zsdt0132-kunnr IS NOT INITIAL AND me->at_tipo_parceiro = 'C' ) OR
               ( me->at_zsdt0132-lifnr IS NOT INITIAL AND me->at_tipo_parceiro = 'F' ) ).
        RETURN.
      ENDIF.
    ENDIF.

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

    DATA: lv_id_referencia_low TYPE string.

    FREE: e_data, me->at_zi_parceiros, me->at_zi_enderecos.

    r_if_integracao_outbound = me.

    IF me->at_zsdt0132-lifnr_rot IS INITIAL.   "*-US190444-10.09.2025-#190444-JT-inicio
      SELECT SINGLE *
        INTO @me->at_zi_parceiros
        FROM zi_sd_parceiros_safra_int
       WHERE parceiro = @me->at_cod_parceiro
         AND tipo     = @me->at_tipo_parceiro.
*-US190444-10.09.2025-#190444-JT-inicio
    ELSE.
      SELECT SINGLE *
        INTO @me->at_zi_parceiros
        FROM zi_sd_parceiros_safra_int
       WHERE parceiro = @me->at_zsdt0132-lifnr_rot
         AND tipo     = @me->at_tipo_parceiro.
    ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

    SELECT SINGLE *
      FROM zi_sd_parceiros_end_add
      INTO @me->at_zi_enderecos
     WHERE parceiro  = @me->at_cod_parceiro
       AND numrota   = @me->at_nr_rot.

    CASE me->at_tipo_parceiro.
      WHEN 'C'.
        SELECT SINGLE  txjcd
          INTO @me->at_txjcd
          FROM kna1
         WHERE kunnr = @me->at_cod_parceiro.

      WHEN 'F'.
        IF me->at_zsdt0132-lifnr_rot IS INITIAL.   "*-US190444-10.09.2025-#190444-JT-inicio
          SELECT SINGLE  txjcd
            INTO @me->at_txjcd
            FROM lfa1
           WHERE lifnr = @me->at_cod_parceiro.
        ELSE.
          SELECT SINGLE  txjcd
            INTO @me->at_txjcd
            FROM lfa1
           WHERE lifnr = @me->at_zsdt0132-lifnr_rot.
        ENDIF.
*-US190444-10.09.2025-#190444-JT-fim
    ENDCASE.

    CHECK me->at_metodo_http <> zif_integracao_inject=>co_request_method_get AND
          me->at_metodo_http <> zif_integracao_inject=>co_request_method_delete.

    me->at_body = set_montar_json( ).

    /ui2/cl_json=>serialize( EXPORTING data   = me->at_body pretty_name = abap_true RECEIVING r_json = e_data ).

    me->set_tratamento_tags( CHANGING  c_json = e_data ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound   = me.
    e_referencia-tp_referencia = 'OB_SAFRA_CONTACT'.
    e_referencia-id_referencia = me->at_id_referencia.

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.
    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_safra_crt_contact.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.
  endmethod.


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
        me->at_externalid-external_id = me->at_id_referencia.

      WHEN zif_integracao_inject=>co_request_method_get.
        me->at_externalid-external_id = me->at_id_referencia.
    ENDCASE.

    CONDENSE me->at_externalid-external_id NO-GAPS.

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_id_referencia.
    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

    r_if_integracao_outbound = me.

    DATA(lv_metodo)     = at_metodo_http.
    DATA(lv_externalid) = at_externalid-external_id.
    DATA(lv_force)      = at_externalid-force.
    DATA(lv_type)       = at_externalid-type.
    DATA(lv_upsert)     = at_externalid-upsert.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO @DATA(lwa_webservice)
     WHERE service EQ @lc_servico.

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
*       Exemplo: https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/contacts
        DATA(v_url) = |{ lwa_webservice-url }|.

      WHEN zif_integracao_inject=>co_request_method_get.
*       Exemplo: https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/contacts/ab123ce?type=customer
*       v_url = |{ lwa_webservice-url }/{ lv_externalid }?type={ lv_type }|.
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.

      WHEN zif_integracao_inject=>co_request_method_put.
*       Exemplo: https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/contacts/ab123ce?upsert=false
*       v_url = |{ lwa_webservice-url }/{ lv_externalid }?upsert={ lv_upsert }|.
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.

      WHEN zif_integracao_inject=>co_request_method_delete.
*       Exemplo: https://amaggi.api.stg.services.souagrosolucoes.com.br/integration/contacts/ab123ce?type=customer&force=false
*       v_url = |{ lwa_webservice-url }/{ lv_externalid }?type={ lv_type }&force={ lv_force }|.
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.
    ENDCASE.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http = VALUE #( ds_formato            = me->lc_content_type
                                                              ds_content_type       = lwa_webservice-content_type
                                                              ds_url                = v_url
                                                              ds_body               = me->at_ds_body
*                                                             ds_url_token          = lwa_webservice-token
                                                              ds_metodo             = lv_metodo
                                                              ds_not_content_length = abap_false ).

    "// Informação do header
    APPEND VALUE #( name = me->lc_authorization value = lwa_webservice-token )     TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD get_external_id.

    DATA: lv_rota    TYPE zi_sd_parceiros_end_add-numrota.

    FREE: me->at_tipo_parceiro, lv_rota, e_integrar.

    SELECT SINGLE tipo
      INTO @me->at_tipo_parceiro
      FROM zi_sd_parceiros_safra_int
     WHERE parceiro = @i_parceiro.

    IF sy-subrc = 0.
      e_integrar = abap_true.
    ENDIF.

    me->at_tipo_parceiro = i_tipo_parceiro.

    r_external_id = COND #( WHEN i_nr_rot IS INITIAL THEN me->at_tipo_parceiro && '-' && |{ i_parceiro ALPHA = OUT }|
                                                     ELSE me->at_tipo_parceiro && '-' && |{ i_parceiro ALPHA = OUT }| && '-' && |{ i_nr_rot ALPHA = OUT }| ).

    CONDENSE r_external_id NO-GAPS.

  ENDMETHOD.


  METHOD set_montar_json.

    FREE: r_data, me->at_body.

    me->at_body-externalid                         = me->at_id_referencia.
    me->at_body-contact-name                       = COND #( WHEN me->at_zsdt0132-rot_desc IS INITIAL THEN me->at_zi_parceiros-nomecompl
                                                                                                      ELSE me->at_zsdt0132-rot_desc ).
    me->at_body-contact-aliasname                  = me->at_zi_parceiros-nomeparceiro.
    me->at_body-contact-person                     = COND #( WHEN me->at_zi_parceiros-tipopessoa = 'fisica' THEN 'natural'
                                                                                                            ELSE me->at_zi_parceiros-tipopessoa ).
    me->at_body-contact-description                = me->at_zi_parceiros-nomeend.
    me->at_body-contact-enabled                    = c_true.
    me->at_body-contact-identificationnumber       = me->at_zi_parceiros-identificacao.
    me->at_body-contact-secondidentificationnumber = abap_off.
*
    me->at_body-address-name                       = me->at_zi_parceiros-nomeend.
    me->at_body-address-description                = me->at_zsdt0132-rot_desc.
    me->at_body-address-externalid                 = me->at_id_referencia.
    me->at_body-address-postalcode                 = me->at_zi_parceiros-cep.
    me->at_body-address-street                     = COND #( WHEN me->at_zsdt0132-endereco IS INITIAL THEN me->at_zi_parceiros-rua
                                                                                                      ELSE me->at_zsdt0132-endereco ).
    me->at_body-address-district                   = me->at_zi_parceiros-cidade.
    me->at_body-address-complement                 = me->at_zi_parceiros-rua.
    me->at_body-address-city                       = COND #( WHEN me->at_zsdt0132-city1 IS INITIAL THEN me->at_zi_parceiros-cidade
                                                                                                   ELSE me->at_zsdt0132-city1 ).
    me->at_body-address-cityid                     = me->at_txjcd+3(12).
    me->at_body-address-state                      = COND #( WHEN me->at_zsdt0132-uf    IS INITIAL THEN me->at_zi_parceiros-estado
                                                                                                   ELSE me->at_zsdt0132-uf ).
    me->at_body-address-country                    = me->at_zi_parceiros-pais.
    me->at_body-address-latitude                   = COND #( WHEN me->at_zi_enderecos-latidude >= 0
                                                             THEN        zcl_util=>get_string_numeric( CONV #( me->at_zi_enderecos-latidude ) )
                                                             ELSE '-' && zcl_util=>get_string_numeric( CONV #( me->at_zi_enderecos-latidude ) ) ).
    me->at_body-address-longitude                  = COND #( WHEN me->at_zi_enderecos-longitude >= 0
                                                             THEN        zcl_util=>get_string_numeric( CONV #( me->at_zi_enderecos-longitude ) )
                                                             ELSE '-' && zcl_util=>get_string_numeric( CONV #( me->at_zi_enderecos-longitude ) ) ).
    me->at_body-address-primarycontactname         = me->at_zi_parceiros-nomecompl.
    me->at_body-address-addressroute               = abap_off. "me->at_zi_enderecos-numrota.

    CASE me->at_tipo_parceiro.
      WHEN 'F'.
        SELECT SINGLE ktokk
          INTO @DATA(_ktokk)
          FROM lfa1
         WHERE lifnr = @me->at_cod_parceiro.

        IF sy-subrc = 0.
          READ TABLE me->at_t_zsdt0418 INTO DATA(_zsdt0418) WITH KEY conta = _ktokk.
          IF sy-subrc = 0.
            IF _zsdt0418-fornecedor = abap_true.
              APPEND 'supplier'          TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-cliente = abap_true.
              APPEND 'customer'          TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-ponto_coleta = abap_true.
              APPEND 'withdrawalPlace'   TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-filial = abap_true.
              APPEND 'branchOffice'      TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-local_entrega = abap_true.
              APPEND 'deposit'           TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-transportadora = abap_true.
              APPEND 'transportCompany'  TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-motorista = abap_true.
              APPEND 'driver'            TO me->at_body-type.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'C'.
        SELECT SINGLE ktokd
          INTO @DATA(_ktokd)
          FROM kna1
         WHERE kunnr = @me->at_cod_parceiro.

        IF sy-subrc = 0.
          READ TABLE me->at_t_zsdt0418 INTO _zsdt0418 WITH KEY conta = _ktokd.
          IF sy-subrc = 0.
            IF _zsdt0418-fornecedor = abap_true.
              APPEND 'supplier'          TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-cliente = abap_true.
              APPEND 'customer'          TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-ponto_coleta = abap_true.
              APPEND 'withdrawalPlace'   TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-filial = abap_true.
              APPEND 'branchOffice'      TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-local_entrega = abap_true.
              APPEND 'deposit'           TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-transportadora = abap_true.
              APPEND 'transportCompany'  TO me->at_body-type.
            ENDIF.
            IF _zsdt0418-motorista = abap_true.
              APPEND 'driver'            TO me->at_body-type.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

*   IF me->at_metodo_http = zif_integracao_inject=>co_request_method_put.
*     READ TABLE me->at_body-type INTO DATA(_type) INDEX 1.
*     me->at_type_put = '"type":' && '"' && _type && '",'.
*     FREE: me->at_body-type.
*   ENDIF.

    CONDENSE me->at_body-externalid          NO-GAPS.
    CONDENSE me->at_body-contact-name.
    CONDENSE me->at_body-contact-aliasname.
    CONDENSE me->at_body-contact-person.
    CONDENSE me->at_body-contact-description.
    CONDENSE me->at_body-address-name.
    CONDENSE me->at_body-address-description.
    CONDENSE me->at_body-address-externalid.
    CONDENSE me->at_body-address-postalcode.
    CONDENSE me->at_body-address-street.
    CONDENSE me->at_body-address-district.
    CONDENSE me->at_body-address-complement.
    CONDENSE me->at_body-address-city.

    r_data = me->at_body.

  ENDMETHOD.


  METHOD set_tratamento_tags.

*   IF me->at_metodo_http = zif_integracao_inject=>co_request_method_put.
*     REPLACE ALL OCCURRENCES OF '"type":[],'               IN c_json WITH me->at_type_put.
*   ENDIF.

    REPLACE ALL OCCURRENCES OF 'externalid'                 IN c_json WITH 'externalId'.
    REPLACE ALL OCCURRENCES OF 'aliasname'                  IN c_json WITH 'aliasName'.
    REPLACE ALL OCCURRENCES OF 'identificationnumber'       IN c_json WITH 'identificationNumber'.
    REPLACE ALL OCCURRENCES OF 'secondidentificationnumber' IN c_json WITH 'secondIdentificationNumber'.
    REPLACE ALL OCCURRENCES OF 'postalcode'                 IN c_json WITH 'postalCode'.
    REPLACE ALL OCCURRENCES OF 'cityid'                     IN c_json WITH 'cityId'.
    REPLACE ALL OCCURRENCES OF 'primarycontactname'         IN c_json WITH 'primaryContactName'.
    REPLACE ALL OCCURRENCES OF 'addressroute'               IN c_json WITH 'addressRoute'.
    REPLACE ALL OCCURRENCES OF 'branchoffice'               IN c_json WITH 'branchOffice'.
    REPLACE ALL OCCURRENCES OF 'transportcompany'           IN c_json WITH 'transportCompany'.
    REPLACE ALL OCCURRENCES OF 'withdrawalplace'            IN c_json WITH 'withdrawalPlace'.
    REPLACE ALL OCCURRENCES OF '"true"'                     IN c_json WITH 'true'.
    REPLACE ALL OCCURRENCES OF '"false"'                    IN c_json WITH 'false'.
    REPLACE ALL OCCURRENCES OF '"null"'                     IN c_json WITH 'null'.

  ENDMETHOD.


  METHOD set_tratar_tags.

    TYPES: BEGIN OF ty_tabfield,
             fieldname TYPE tabname,
             comprim   TYPE i.
    TYPES: END OF ty_tabfield.

    DATA: t_colunas  TYPE TABLE OF dfies,
          t_tabfield TYPE TABLE OF ty_tabfield,
          w_tabfield TYPE ty_tabfield,
          lv_col_low TYPE string.

    FREE: t_colunas, t_tabfield.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = lc_estrutura_json
        all_types = abap_true
      TABLES
        dfies_tab = t_colunas.

    LOOP AT t_colunas    INTO DATA(_colunas).
      w_tabfield-fieldname  = _colunas-fieldname.
      w_tabfield-comprim    = strlen( _colunas-fieldname ).
      APPEND w_tabfield    TO t_tabfield.
    ENDLOOP.

    SORT t_tabfield BY comprim DESCENDING.

    IF me->at_metodo_http = zif_integracao_inject=>co_request_method_put.
      REPLACE ALL OCCURRENCES OF '"TYPE":[],'               IN c_json WITH me->at_type_put.
    ENDIF.

    LOOP AT t_tabfield   INTO DATA(_tabfield).
      lv_col_low            = _tabfield-fieldname.
      TRANSLATE lv_col_low TO LOWER CASE.
      REPLACE ALL OCCURRENCES OF _tabfield-fieldname        IN c_json WITH lv_col_low.
    ENDLOOP.

    REPLACE ALL OCCURRENCES OF 'externalid'                 IN c_json WITH 'externalId'.
    REPLACE ALL OCCURRENCES OF 'aliasname'                  IN c_json WITH 'aliasName'.
    REPLACE ALL OCCURRENCES OF 'identificationnumber'       IN c_json WITH 'identificationNumber'.
    REPLACE ALL OCCURRENCES OF 'secondidentificationnumber' IN c_json WITH 'secondIdentificationNumber'.
    REPLACE ALL OCCURRENCES OF 'postalcode'                 IN c_json WITH 'postalCode'.
    REPLACE ALL OCCURRENCES OF 'cityid'                     IN c_json WITH 'cityId'.
    REPLACE ALL OCCURRENCES OF 'primarycontactname'         IN c_json WITH 'primaryContactName'.
    REPLACE ALL OCCURRENCES OF 'addressroute'               IN c_json WITH 'addressRoute'.
    REPLACE ALL OCCURRENCES OF 'branchoffice'               IN c_json WITH 'branchOffice'.
    REPLACE ALL OCCURRENCES OF 'transportcompany'           IN c_json WITH 'transportCompany'.
    REPLACE ALL OCCURRENCES OF 'withdrawalplace'            IN c_json WITH 'withdrawalPlace'.
    REPLACE ALL OCCURRENCES OF '"true"'                     IN c_json WITH 'true'.
    REPLACE ALL OCCURRENCES OF '"false"'                    IN c_json WITH 'false'.
    REPLACE ALL OCCURRENCES OF '"null"'                     IN c_json WITH 'null'.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.
ENDCLASS.

CLASS zcl_due DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_due .

    ALIASES at_due
      FOR zif_due~at_due .
    ALIASES at_itens
      FOR zif_due~at_itens .
    ALIASES at_itens_faturas_ref
      FOR zif_due~at_itens_faturas_ref .
    ALIASES at_itens_lpco
      FOR zif_due~at_itens_lpco .
    ALIASES at_itens_paises_destino
      FOR zif_due~at_itens_paises_destino .
    ALIASES at_token
      FOR zif_due~at_token .
    ALIASES at_xmls_exportacao
      FOR zif_due~at_xmls_exportacao .
    ALIASES ck_alterou
      FOR zif_due~ck_alterou .
    ALIASES add_item
      FOR zif_due~add_item .
    ALIASES add_item_fatura_ref
      FOR zif_due~add_item_fatura_ref .
    ALIASES add_item_lpco
      FOR zif_due~add_item_lpco .
    ALIASES add_item_pais_destino
      FOR zif_due~add_item_pais_destino .
    ALIASES bloq_desbloq
      FOR zif_due~bloq_desbloq .
    ALIASES consultar_due
      FOR zif_due~consultar_due .
    ALIASES consulta_due_completa
      FOR zif_due~consulta_due_completa .
    ALIASES del_item
      FOR zif_due~del_item .
    ALIASES del_item_fatura_ref
      FOR zif_due~del_item_fatura_ref .
    ALIASES del_item_pais_destino
      FOR zif_due~del_item_pais_destino .
    ALIASES eliminar_registro
      FOR zif_due~eliminar_registro .
    ALIASES enviar_due
      FOR zif_due~enviar_due .
    ALIASES gerar_nr_ruc
      FOR zif_due~gerar_nr_ruc .
    ALIASES gerar_nr_ruc_with_screen
      FOR zif_due~gerar_nr_ruc_with_screen .
    ALIASES get_xml_due
      FOR zif_due~get_xml_due .
    ALIASES gravar_registro
      FOR zif_due~gravar_registro .
    ALIASES lib_leitura_opus
      FOR zif_due~lib_leitura_opus .
    ALIASES limpar_registro
      FOR zif_due~limpar_registro .
    ALIASES modify_region
      FOR zif_due~modify_region .
    ALIASES modify_tp_exportacao
      FOR zif_due~modify_tp_exportacao .
    ALIASES reclassificacao_eudr
      FOR zif_due~reclassificacao_eudr.
    ALIASES monta_xml
      FOR zif_due~monta_xml .
    ALIASES registrar_log
      FOR zif_due~registrar_log .
    ALIASES registro_due
      FOR zif_due~registro_due .
    ALIASES set_bukrs
      FOR zif_due~set_bukrs .
    ALIASES set_cabecalho
      FOR zif_due~set_cabecalho .
    ALIASES set_caso_especial_transporte
      FOR zif_due~set_caso_especial_transporte .
    ALIASES set_cnpj_cpf_resp_loc_desp
      FOR zif_due~set_cnpj_cpf_resp_loc_desp .
    ALIASES set_cnpj_declarante
      FOR zif_due~set_cnpj_declarante .
    ALIASES set_codigo_ra_despacho
      FOR zif_due~set_codigo_ra_despacho .
    ALIASES set_codigo_ra_embarque
      FOR zif_due~set_codigo_ra_embarque .
    ALIASES set_codigo_urf_despacho
      FOR zif_due~set_codigo_urf_despacho .
    ALIASES set_codigo_urf_embarque
      FOR zif_due~set_codigo_urf_embarque .
    ALIASES set_forma_exportacao
      FOR zif_due~set_forma_exportacao .
    ALIASES set_id_due
      FOR zif_due~set_id_due .
    ALIASES set_id_due_ref
      FOR zif_due~set_id_due_ref .
    ALIASES set_local_despacho_end
      FOR zif_due~set_local_despacho_end .
    ALIASES set_local_despacho_latitude
      FOR zif_due~set_local_despacho_latitude .
    ALIASES set_local_despacho_longitude
      FOR zif_due~set_local_despacho_longitude .
    ALIASES set_moeda_cambio
      FOR zif_due~set_moeda_cambio .
    ALIASES set_motivo
      FOR zif_due~set_motivo .
    ALIASES set_numero_due
      FOR zif_due~set_numero_due .
    ALIASES set_numero_ruc
      FOR zif_due~set_numero_ruc .
    ALIASES set_observacoes_gerais
      FOR zif_due~set_observacoes_gerais .
    ALIASES set_situacao_especial
      FOR zif_due~set_situacao_especial .
    ALIASES set_status
      FOR zif_due~set_status .
    ALIASES set_token
      FOR zif_due~set_token .
    ALIASES set_tp_cod_local_despacho
      FOR zif_due~set_tp_cod_local_despacho .
    ALIASES set_tp_due
      FOR zif_due~set_tp_due .
    ALIASES set_xmls_exportacao
      FOR zif_due~set_xmls_exportacao .
    ALIASES sol_modific_opus
      FOR zif_due~sol_modific_opus .
    ALIASES troca_due
      FOR zif_due~troca_due .
    ALIASES validar_registro
      FOR zif_due~validar_registro .
    ALIASES valida_preenchimento_auto
      FOR zif_due~valida_preenchimento_auto .
    ALIASES enviar_email_file_eudr
      FOR zif_due~enviar_email_file_eudr.


    METHODS constructor
      IMPORTING
        !i_id_due TYPE zde_id_due OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DUE IMPLEMENTATION.


  METHOD constructor.


    CLEAR: me->at_due, me->at_itens[], me->at_itens_faturas_ref[], me->at_itens_paises_destino[].

    CHECK ( i_id_due IS NOT INITIAL ).

    "Cabeçalho DU-e
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due = @i_id_due.

    CHECK sy-subrc = 0.

    "Itens DU-e
    SELECT *
      FROM zsdt0172 INTO TABLE @DATA(_tg_0172)
     WHERE id_due  = @i_id_due.

    IF _tg_0172[] IS NOT INITIAL.
      "Itens Faturas Referenciadas
      SELECT *
        FROM zsdt0173 INTO TABLE @DATA(_tg_0173)
         FOR ALL ENTRIES IN @_tg_0172
       WHERE id_due      = @_tg_0172-id_due
         AND id_due_item = @_tg_0172-id_due_item.

      "Paises Destino
      SELECT *
        FROM zsdt0174 INTO TABLE @DATA(_tg_0174)
         FOR ALL ENTRIES IN @_tg_0172
       WHERE id_due      = @_tg_0172-id_due
         AND id_due_item = @_tg_0172-id_due_item.

      "LPCO
      SELECT *
        FROM zsdt0190 INTO TABLE @DATA(_tg_0190)
         FOR ALL ENTRIES IN @_tg_0172
       WHERE id_due      = @_tg_0172-id_due
         AND id_due_item = @_tg_0172-id_due_item.

    ENDIF.

    me->set_cabecalho( i_zsdt0170 = _wl_0170 ).

*    ME->SET_ID_DUE(                         I_ID_DUE                           = _WL_0170-ID_DUE ).
*    ME->SET_BUKRS(                          I_BUKRS                            = _WL_0170-BUKRS ).
*    ME->SET_NUMERO_DUE(                     I_NUMERO_DUE                       = _WL_0170-NUMERO_DUE ).
*    ME->SET_NUMERO_RUC(                     I_NUMERO_RUC                       = _WL_0170-NUMERO_RUC ).
*    ME->SET_CODIGO_URF_DESPACHO(            I_CODIGO_URF_DESPACHO              = _WL_0170-CODIGO_URF_DESPACHO ).
*    ME->SET_CODIGO_RA_DESPACHO(             I_CODIGO_RA_DESPACHO               = _WL_0170-CODIGO_RA_DESPACHO ).
*    ME->SET_TP_COD_LOCAL_DESPACHO(          I_TP_COD_LOCAL_DESPACHO            = _WL_0170-TP_COD_LOCAL_DESPACHO ).
*    ME->SET_CNPJ_CPF_RESP_LOC_DESP(         I_CNPJ_CPF_RESP_LOC_DESP           = _WL_0170-CNPJ_CPF_RESP_LOC_DESP ).
*    ME->SET_LOCAL_DESPACHO_LONGITUDE(       I_LOCAL_DESPACHO_LONGITUDE         = _WL_0170-LOCAL_DESPACHO_LONGITUDE ).
*    ME->SET_LOCAL_DESPACHO_LATITUDE(        I_LOCAL_DESPACHO_LATITUDE          = _WL_0170-LOCAL_DESPACHO_LATITUDE ).
*    ME->SET_LOCAL_DESPACHO_END(             I_LOCAL_DESPACHO_END               = _WL_0170-LOCAL_DESPACHO_END ).
*    ME->SET_FORMA_EXPORTACAO(               I_FORMA_EXPORTACAO                 = _WL_0170-FORMA_EXPORTACAO ).
*    ME->SET_CASO_ESPECIAL_TRANSPORTE(       I_CASO_ESPECIAL_TRANSPORTE         = _WL_0170-CASO_ESPECIAL_TRANSPORTE ).
*    ME->SET_SITUACAO_ESPECIAL(              I_SITUACAO_ESPECIAL                = _WL_0170-SITUACAO_ESPECIAL ).
*    ME->SET_OBSERVACOES_GERAIS(             I_OBSERVACOES_GERAIS               = _WL_0170-OBSERVACOES_GERAIS ).
*    ME->SET_MOEDA_CAMBIO(                   I_MOEDA_CAMBIO                     = _WL_0170-MOEDA_CAMBIO ).
*    ME->SET_MOTIVO(                         I_MOTIVO                           = _WL_0170-MOTIVO ).
*    ME->SET_CNPJ_DECLARANTE(                I_CNPJ_DECLARANTE                  = _WL_0170-CNPJ_DECLARANTE ).
*    ME->SET_CODIGO_URF_EMBARQUE(            I_CODIGO_URF_EMBARQUE              = _WL_0170-CODIGO_URF_EMBARQUE ).
*    ME->SET_CODIGO_RA_EMBARQUE(             I_CODIGO_RA_EMBARQUE               = _WL_0170-CODIGO_RA_EMBARQUE ).
*    ME->SET_TP_DUE(                         I_TP_DUE                           = _WL_0170-TP_DUE ).
*    ME->SET_ID_DUE_REF(                     I_ID_DUE_REF                       = _WL_0170-ID_DUE_REF ).
*    ME->SET_STATUS(                         I_STATUS                           = _WL_0170-STATUS ).

    "Carrega Itens
    LOOP AT _tg_0172 INTO DATA(wl_0172).
      me->add_item( i_zsdt0172 = wl_0172 ).
    ENDLOOP.

    "Carrega Itens Faturas Ref.
    LOOP AT _tg_0173 INTO DATA(wl_0173).
      me->add_item_fatura_ref( i_zsdt0173 = wl_0173 ).
    ENDLOOP.

    "Carrega Itens Países Destino.
    LOOP AT _tg_0174 INTO DATA(wl_0174).
      me->add_item_pais_destino( i_zsdt0174 = wl_0174 ).
    ENDLOOP.

    "Carrega Itens LPCO.
    LOOP AT _tg_0190 INTO DATA(wl_0190).
      me->add_item_lpco( i_zsdt0190 = wl_0190 ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_due~add_item.

    CHECK i_zsdt0172 IS NOT INITIAL.

    APPEND i_zsdt0172 TO me->at_itens.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~add_item_fatura_ref.

    CHECK i_zsdt0173 IS NOT INITIAL.

    APPEND i_zsdt0173 TO me->at_itens_faturas_ref.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~add_item_lpco.

    CHECK i_zsdt0190 IS NOT INITIAL.

    APPEND i_zsdt0190 TO me->at_itens_lpco.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~add_item_pais_destino.

    CHECK i_zsdt0174 IS NOT INITIAL.

    APPEND i_zsdt0174 TO me->at_itens_paises_destino.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~bloq_desbloq.

    r_modificado = abap_false.

    CHECK me->at_due-id_due IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due EQ @me->at_due-id_due.

    CHECK sy-subrc EQ 0.

    "Se for Bloquear DU-e e mesma já tiver sido liberada para integração com Comex, deve Solicitar modificação primeiramente...
*    IF ( _WL_0170-BLOQUEIO_INTERNO EQ ABAP_FALSE ) AND
*       ( _WL_0170-LIB_LEITURA_OPUS EQ ABAP_TRUE  ).
*      ME->SOL_MODIFIC_OPUS( I_BLOQ_OPUS = ABAP_TRUE ).
*      RETURN.
*    ENDIF.

    IF ( _wl_0170-lib_leitura_opus       EQ abap_true ) OR
       ( _wl_0170-leitura_opus           EQ abap_true ) OR
       ( _wl_0170-solic_modificacao_opus EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_leitura_opus-msgid
                            msgno = zcx_due=>zcx_leitura_opus-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_leitura_opus-msgno
          msgid  = zcx_due=>zcx_leitura_opus-msgid.
    ENDIF.

    IF ( _wl_0170-vinc_opus EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_vinc_opus-msgid
                            msgno = zcx_due=>zcx_vinc_opus-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_vinc_opus-msgno
          msgid  = zcx_due=>zcx_vinc_opus-msgid.
    ENDIF.

    IF _wl_0170-bloqueio_interno EQ abap_true.
      _wl_0170-bloqueio_interno = abap_false.
      _wl_0170-bloq_opus        = abap_false.
    ELSE.
      _wl_0170-bloqueio_interno = abap_true.
    ENDIF.

    MODIFY zsdt0170 FROM _wl_0170.
    IF sy-subrc EQ 0.

      IF _wl_0170-bloqueio_interno EQ abap_true.
        MESSAGE s104.
      ELSE.
        MESSAGE s105.
      ENDIF.

      r_modificado = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD zif_due~consultar_due.

    DATA: http_client          TYPE REF TO if_http_client,
          xml_return           TYPE REF TO cl_xml_document,
          return_code          TYPE i,
          v_dt_registro_portal TYPE zsdt0170-dt_registro_portal,
          v_hr_registro_portal TYPE zsdt0170-hr_registro_portal,
          e_resultado          TYPE string,
          v_xml                TYPE string,
          v_authorization      TYPE string,
          v_x_csrf_token       TYPE string,
          v_url                TYPE ui_src_url,
          vl_ini_pos           TYPE i.

    DATA: js_prop_tab TYPE js_property_tab,
          js_obj      TYPE REF TO cl_java_script.

    CLEAR: v_authorization , v_x_csrf_token.

    CLEAR: r_zsdt0170.

    IF me->at_token IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_autenticacao_not_found-msgid
                            msgno = zcx_due=>zcx_autenticacao_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_autenticacao_not_found-msgno
          msgid  = zcx_due=>zcx_autenticacao_not_found-msgid.
    ENDIF.

    DATA(_id_token) = me->at_token->get_id_token( ).
    IF _id_token IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_autenticacao_not_found-msgid
                            msgno = zcx_due=>zcx_autenticacao_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_autenticacao_not_found-msgno
          msgid  = zcx_due=>zcx_autenticacao_not_found-msgid.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = TEXT-002.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    DATA(_due_webservice) = 'Consultar DU-e'.
    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(_auth_service)
     WHERE service = 'DUE_CONSULTAR'.

    _auth_service-url = _auth_service-url && '/?numero=' &&  me->at_due-numero_due.

    IF ( sy-subrc NE 0 ) OR ( _auth_service-url IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_webservice_not_found-msgid
                            msgno = zcx_due=>zcx_webservice_not_found-msgno
                            attr1 = CONV #( _due_webservice )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_webservice_not_found-msgno
          msgid  = zcx_due=>zcx_webservice_not_found-msgid
          msgv1  = CONV #( _due_webservice ).
    ELSE.
      v_url = _auth_service-url.
    ENDIF.

    "Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( v_url )
        ssl_id             = 'DFAULT' "ME->AT_PAR_AUTENTICACAO-SSL_ID
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/xml; charset=UTF-8'.

    v_authorization = me->at_token->get_token_jw( ).

    DATA(_token_opus) = me->at_token->get_token_opus( ).
    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Authorization'
        value = _token_opus.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'AuthorizationReceita'
        value = v_authorization.

    v_x_csrf_token  = me->at_token->get_token_csrf( ).

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'X-CSRF-Token'
        value = v_x_csrf_token.

    CALL METHOD http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_falha_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_falha_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 2.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_status_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_status_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_status_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_status_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 3.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_processamento_ws-msgid
                              msgno = zcx_due=>zcx_falha_processamento_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_processamento_ws-msgno
            msgid  = zcx_due=>zcx_falha_processamento_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 4.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_timeout_ws-msgid
                              msgno = zcx_due=>zcx_timeout_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_timeout_ws-msgno
            msgid  = zcx_due=>zcx_timeout_ws-msgid
            msgv1  = CONV #( v_url ).
    ENDCASE.

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_falha_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_falha_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 2.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_status_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_status_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_status_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_status_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 3.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_processamento_ws-msgid
                              msgno = zcx_due=>zcx_falha_processamento_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_processamento_ws-msgno
            msgid  = zcx_due=>zcx_falha_processamento_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 4.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_timeout_ws-msgid
                              msgno = zcx_due=>zcx_timeout_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_timeout_ws-msgno
            msgid  = zcx_due=>zcx_timeout_ws-msgid
            msgv1  = CONV #( v_url ).
    ENDCASE.

    "//Check return content
    CREATE OBJECT xml_return.

    CALL METHOD xml_return->parse_string
      EXPORTING
        stream = http_client->response->get_cdata( ).


    http_client->response->get_status( IMPORTING code = return_code ).

    e_resultado = http_client->response->get_cdata( ).

    IF return_code NE '200'.
      CLEAR: vl_ini_pos.

      IF vl_ini_pos IS INITIAL.
        FIND '(400)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '400'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(401)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '401'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(403)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '403'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(404)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '404'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(422)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '422'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(500)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '500'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(503)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '503'.
        ENDIF.
      ENDIF.

      CASE return_code.
        WHEN 400.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_400-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_400-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_400-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_400-msgid.
        WHEN 401.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_401-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_401-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_401-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_401-msgid.
        WHEN 403.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_403-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_403-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_403-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_403-msgid.
        WHEN 404.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_404-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_404-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_404-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_404-msgid.
        WHEN 422.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_422-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_422-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_422-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_422-msgid.
        WHEN 500.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_500-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_500-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_500-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_500-msgid.
        WHEN 503.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_503-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_503-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_503-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_503-msgid.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_others-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_others-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_others-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_others-msgid.
      ENDCASE.

      RETURN.
    ENDIF.

    CALL METHOD xml_return->find_node_table
      EXPORTING
        tabname = 'error'
      IMPORTING
        t_nodes = DATA(_nodes_error).

    IF _nodes_error[] IS NOT INITIAL.

      TRY.
          DATA(_message) = CAST if_ixml_node( _nodes_error[ 1 ]-node )->get_value( ).
          DATA(_code)    = CAST if_ixml_node( _nodes_error[ 2 ]-node )->get_value( ).
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      IF _message IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex-msgid
                              msgno = zcx_due=>zcx_retorno_siscomex-msgno
                              attr1 = CONV #( _message ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_retorno_siscomex-msgno
            msgid  = zcx_due=>zcx_retorno_siscomex-msgid
            msgv1  = CONV #( _message ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_desconhecido_siscomex-msgid
                              msgno = zcx_due=>zcx_erro_desconhecido_siscomex-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_erro_desconhecido_siscomex-msgno
            msgid  = zcx_due=>zcx_erro_desconhecido_siscomex-msgid.
      ENDIF.

    ELSE.

      CLEAR: js_prop_tab[].

      zcl_fmcall_handler=>json2abap( EXPORTING json_string    = e_resultado
                                     IMPORTING property_table = js_prop_tab
                                     CHANGING  js_object      = js_obj ).

      LOOP AT js_prop_tab ASSIGNING FIELD-SYMBOL(<js_prop>).

        CHECK ( <js_prop>-value IS NOT INITIAL ) AND ( <js_prop>-value NE 'null' ).

        CASE <js_prop>-name.
          WHEN 'numeroDUE'.
            r_zsdt0170-numero_due = <js_prop>-value.
          WHEN 'numeroRUC'.
            r_zsdt0170-numero_ruc = <js_prop>-value.
          WHEN 'situacaoDUE'.
            r_zsdt0170-situacao_due = <js_prop>-value.
          WHEN 'dataSituacaoDUE'.
            r_zsdt0170-dt_situacao  = <js_prop>-value(4) && <js_prop>-value+5(2) && <js_prop>-value+8(2).
            r_zsdt0170-hr_situacao  = <js_prop>-value+11(2) && <js_prop>-value+14(2) && <js_prop>-value+17(2).
          WHEN 'indicadorBloqueio'.
            r_zsdt0170-ind_bloqueio = <js_prop>-value.
          WHEN 'controleAdministrativo'.
            r_zsdt0170-controle_adm = <js_prop>-value.
          WHEN 'uaEmbarque'.
            r_zsdt0170-codigo_urf_embarque = <js_prop>-value.
          WHEN 'uaDespacho'.
            r_zsdt0170-codigo_urf_despacho = <js_prop>-value.
          WHEN 'responsavelUADespacho'.

          WHEN 'codigoRecintoAduaneiroDespacho'.
            r_zsdt0170-codigo_ra_despacho = <js_prop>-value.
          WHEN 'codigoRecintoAduaneiroEmbarque'.
            r_zsdt0170-codigo_ra_embarque = <js_prop>-value.
          WHEN 'latitudeDespacho'.
            r_zsdt0170-local_despacho_latitude = <js_prop>-value.
          WHEN 'longitudeDespacho'.
            r_zsdt0170-local_despacho_longitude = <js_prop>-value.
            "WHEN 'declarante'.
            "WHEN 'exportadores'.
          WHEN 'situacaoCarga'.
            r_zsdt0170-situacao_carga = <js_prop>-value.
        ENDCASE.
      ENDLOOP.

      IF ( i_atualiza_registro   IS NOT INITIAL ) AND
         ( r_zsdt0170-numero_due IS NOT INITIAL ) AND
         ( me->at_due-id_due     IS NOT INITIAL ).

        IF r_zsdt0170-situacao_due NE me->at_due-situacao_due.
          UPDATE zsdt0170 SET situacao_due = r_zsdt0170-situacao_due
           WHERE id_due = me->at_due-id_due.
        ENDIF.

        IF r_zsdt0170-dt_situacao NE me->at_due-dt_situacao.
          UPDATE zsdt0170 SET dt_situacao = r_zsdt0170-dt_situacao
           WHERE id_due = me->at_due-id_due.
        ENDIF.

        IF r_zsdt0170-hr_situacao NE me->at_due-hr_situacao.
          UPDATE zsdt0170 SET hr_situacao = r_zsdt0170-hr_situacao
           WHERE id_due = me->at_due-id_due.
        ENDIF.

        IF r_zsdt0170-ind_bloqueio NE me->at_due-ind_bloqueio.
          UPDATE zsdt0170 SET ind_bloqueio = r_zsdt0170-ind_bloqueio
           WHERE id_due = me->at_due-id_due.
        ENDIF.

        IF r_zsdt0170-situacao_carga NE me->at_due-situacao_carga.
          UPDATE zsdt0170 SET situacao_carga = r_zsdt0170-situacao_carga
           WHERE id_due = me->at_due-id_due.
        ENDIF.

        IF r_zsdt0170-controle_adm NE me->at_due-controle_adm.
          UPDATE zsdt0170 SET controle_adm = r_zsdt0170-controle_adm
           WHERE id_due = me->at_due-id_due.
        ENDIF.

        COMMIT WORK.

      ELSE.
        IF _message IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex-msgno
                                attr1 = CONV #( _message ) )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex-msgid
              msgv1  = CONV #( _message ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_erro_desconhecido_siscomex-msgid
                                msgno = zcx_due=>zcx_erro_desconhecido_siscomex-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_erro_desconhecido_siscomex-msgno
              msgid  = zcx_due=>zcx_erro_desconhecido_siscomex-msgid.
        ENDIF.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD zif_due~consulta_due_completa.

    DATA: http_client          TYPE REF TO if_http_client,
          xml_return           TYPE REF TO cl_xml_document,
          return_code          TYPE i,
          v_dt_registro_portal TYPE zsdt0170-dt_registro_portal,
          v_hr_registro_portal TYPE zsdt0170-hr_registro_portal,
          e_resultado          TYPE string,
          v_xml                TYPE string,
          v_authorization      TYPE string,
          v_x_csrf_token       TYPE string,
          v_url                TYPE string,
          vl_ini_pos           TYPE i.

    DATA: js_prop_tab TYPE js_property_tab,
          js_obj      TYPE REF TO cl_java_script.

    CLEAR: v_authorization , v_x_csrf_token, r_due_completa.

    IF me->at_token IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_autenticacao_not_found-msgid
                            msgno = zcx_due=>zcx_autenticacao_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_autenticacao_not_found-msgno
          msgid  = zcx_due=>zcx_autenticacao_not_found-msgid.
    ENDIF.

    DATA(_id_token) = me->at_token->get_id_token( ).
    IF _id_token IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_autenticacao_not_found-msgid
                            msgno = zcx_due=>zcx_autenticacao_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_autenticacao_not_found-msgno
          msgid  = zcx_due=>zcx_autenticacao_not_found-msgid.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = TEXT-002.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    IF i_numero_due IS NOT INITIAL.

      DATA(_due_webservice) = 'Consultar DU-e por Numero DU-e'.

      SELECT SINGLE *
        FROM zauth_webservice INTO @DATA(_auth_service)
       WHERE service = 'DUE_CONSULTA_COMPLETA_NR_DUE'.

    ELSEIF i_numero_ruc IS NOT INITIAL.

      _due_webservice = 'Consultar DU-e por Numero RUC'.

      SELECT SINGLE *
        FROM zauth_webservice INTO _auth_service
       WHERE service = 'DUE_CONSULTA_COMPLETA_NR_RUC'.

    ELSE.

      _due_webservice = 'Consultar DU-e'.

      sy-subrc = 4.
    ENDIF.

    IF ( sy-subrc NE 0 ) OR ( _auth_service-url IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_webservice_not_found-msgid
                            msgno = zcx_due=>zcx_webservice_not_found-msgno
                            attr1 = CONV #( _due_webservice )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_webservice_not_found-msgno
          msgid  = zcx_due=>zcx_webservice_not_found-msgid
          msgv1  = CONV #( _due_webservice ).
    ELSE.
      v_url = _auth_service-url.
    ENDIF.

    IF i_numero_due IS NOT INITIAL.

      v_url = _auth_service-url && '/' && i_numero_due.

    ELSEIF i_numero_ruc IS NOT INITIAL.

      v_url = _auth_service-url && '/' && i_numero_ruc.

    ENDIF.

    "Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = v_url
        ssl_id             = 'DFAULT' "ME->AT_PAR_AUTENTICACAO-SSL_ID
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/xml; charset=UTF-8'.

    v_authorization = me->at_token->get_token_jw( ).

    DATA(_token_opus) = me->at_token->get_token_opus( ).
    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Authorization'
        value = _token_opus.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'AuthorizationReceita'
        value = v_authorization.

    v_x_csrf_token  = me->at_token->get_token_csrf( ).

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'X-CSRF-Token'
        value = v_x_csrf_token.

    CALL METHOD http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_falha_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_falha_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 2.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_status_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_status_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_status_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_status_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 3.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_processamento_ws-msgid
                              msgno = zcx_due=>zcx_falha_processamento_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_processamento_ws-msgno
            msgid  = zcx_due=>zcx_falha_processamento_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 4.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_timeout_ws-msgid
                              msgno = zcx_due=>zcx_timeout_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_timeout_ws-msgno
            msgid  = zcx_due=>zcx_timeout_ws-msgid
            msgv1  = CONV #( v_url ).
    ENDCASE.

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_falha_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_falha_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 2.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_status_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_status_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_status_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_status_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 3.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_processamento_ws-msgid
                              msgno = zcx_due=>zcx_falha_processamento_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_processamento_ws-msgno
            msgid  = zcx_due=>zcx_falha_processamento_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 4.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_timeout_ws-msgid
                              msgno = zcx_due=>zcx_timeout_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_timeout_ws-msgno
            msgid  = zcx_due=>zcx_timeout_ws-msgid
            msgv1  = CONV #( v_url ).
    ENDCASE.

    "//Check return content
    CREATE OBJECT xml_return.

    CALL METHOD xml_return->parse_string
      EXPORTING
        stream = http_client->response->get_cdata( ).


    http_client->response->get_status( IMPORTING code = return_code ).

    e_resultado = http_client->response->get_cdata( ).

    IF return_code NE '200'.
      CLEAR: vl_ini_pos.

      IF vl_ini_pos IS INITIAL.
        FIND '(400)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '400'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(401)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '401'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(403)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '403'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(404)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '404'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(422)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '422'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(500)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '500'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(503)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '503'.
        ENDIF.
      ENDIF.

      CASE return_code.
        WHEN 400.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_400-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_400-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_400-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_400-msgid.
        WHEN 401.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_401-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_401-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_401-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_401-msgid.
        WHEN 403.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_403-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_403-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_403-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_403-msgid.
        WHEN 404.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_404-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_404-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_404-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_404-msgid.
        WHEN 422.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_422-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_422-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_422-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_422-msgid.
        WHEN 500.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_500-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_500-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_500-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_500-msgid.
        WHEN 503.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_503-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_503-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_503-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_503-msgid.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_others-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_others-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_others-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_others-msgid.
      ENDCASE.

      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES OF 'enderecoDoEstabelecimentoDoLocalDeDespacho'    IN e_resultado WITH 'endDoEstabDoLocalDeDespacho'.
    REPLACE ALL OCCURRENCES OF 'enderecoDoEstabelecimentoDoLocalDeEmbarque'    IN e_resultado WITH 'endDoEstabDoLocalDeEmbarque'.
    REPLACE ALL OCCURRENCES OF 'estabelecimentoDoLocalDeDespacho'              IN e_resultado WITH 'estabDoLocalDeDespacho'.
    REPLACE ALL OCCURRENCES OF 'quantidadeNaUnidadeEstatistica'                IN e_resultado WITH 'quantidadeNaUnidEstatistica'.
    REPLACE ALL OCCURRENCES OF 'valorDaMercadoriaNaCondicaoDeVenda'            IN e_resultado WITH 'valorDaMercNaCondicaoDeVenda'.
    REPLACE ALL OCCURRENCES OF 'valorDaMercadoriaNoLocalDeEmbarque'            IN e_resultado WITH 'valorDaMercNoLocalDeEmbarque'.
    REPLACE ALL OCCURRENCES OF 'valorDaMercadoriaNoLocalDeEmbarqueEmReais'     IN e_resultado WITH 'valorDaMercNoLocalEmbEmReais'.
    REPLACE ALL OCCURRENCES OF 'quantidadeNaUnidadeComercializada'             IN e_resultado WITH 'quantidadeNaUnidComercializada'.
    REPLACE ALL OCCURRENCES OF 'percentualDeReducaoDaBaseCalculado'            IN e_resultado WITH 'percentualDeReducaoDaBaseCalc'.
    REPLACE ALL OCCURRENCES OF 'referenciaDoEnderecoDoLocalDeDespacho'         IN e_resultado WITH 'refDoEndDoLocalDeDespacho'.
    REPLACE ALL OCCURRENCES OF 'referenciaDoEnderecoDoLocalDeEmbarque'         IN e_resultado WITH 'refDoEndDoLocalDeEmbarque'.
    REPLACE ALL OCCURRENCES OF 'situacaoDoTratamentoAdministrativo'            IN e_resultado WITH 'sitDoTratamentoAdministrativo'.
    REPLACE ALL OCCURRENCES OF 'justificativaDeDispensaDaNotaFiscal'           IN e_resultado WITH 'justificativaDeDispensaDaNF'.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = e_resultado
      CHANGING
        data = r_due_completa.


  ENDMETHOD.


  METHOD zif_due~del_item.

    CHECK i_zsdt0172 IS NOT INITIAL.

    READ TABLE me->at_itens INTO DATA(_wl_item) WITH KEY id_due      = i_zsdt0172-id_due
                                                         id_due_item = i_zsdt0172-id_due_item.
    IF sy-subrc EQ 0.
      DELETE me->at_itens WHERE id_due      = i_zsdt0172-id_due
                            AND id_due_item = i_zsdt0172-id_due_item.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_error_del_item-msgid
                              msgno = zcx_due=>zcx_error_del_item-msgno
                              attr1 = CONV #( i_zsdt0172-id_due_item ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_error_del_item-msgno
            msgid  = zcx_due=>zcx_error_del_item-msgid
            msgv1  = CONV #( i_zsdt0172-id_due_item ).
      ENDIF.
    ENDIF.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~del_item_fatura_ref.

    CHECK i_zsdt0173 IS NOT INITIAL.

    READ TABLE me->at_itens_faturas_ref INTO DATA(_wl_item_fat_ref) WITH KEY id_due      = i_zsdt0173-id_due
                                                                             id_due_item = i_zsdt0173-id_due_item
                                                                             id_fatura   = i_zsdt0173-id_fatura.
    IF sy-subrc EQ 0.
      DELETE me->at_itens_faturas_ref WHERE id_due      = i_zsdt0173-id_due
                                        AND id_due_item = i_zsdt0173-id_due_item
                                        AND id_fatura   = i_zsdt0173-id_fatura.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_error_del_item_ft_ref-msgid
                              msgno = zcx_due=>zcx_error_del_item_ft_ref-msgno
                              attr1 = CONV #( i_zsdt0173-id_fatura_ref )
                              attr2 = CONV #( i_zsdt0173-id_due_item   ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_error_del_item_ft_ref-msgno
            msgid  = zcx_due=>zcx_error_del_item_ft_ref-msgid
            msgv1  = CONV #( i_zsdt0173-id_fatura_ref )
            msgv2  = CONV #( i_zsdt0173-id_due_item ).
      ENDIF.
    ENDIF.
    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~del_item_pais_destino.

    CHECK i_zsdt0174 IS NOT INITIAL.

    READ TABLE me->at_itens_paises_destino INTO DATA(_wl_item_pais_dest) WITH KEY id_due          = i_zsdt0174-id_due
                                                                                  id_due_item     = i_zsdt0174-id_due_item
                                                                                  destino_country = i_zsdt0174-destino_country.
    IF sy-subrc EQ 0.
      DELETE me->at_itens_paises_destino WHERE id_due          = i_zsdt0174-id_due
                                           AND id_due_item     = i_zsdt0174-id_due_item
                                           AND destino_country = i_zsdt0174-destino_country.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_error_del_item_pais_dest-msgid
                              msgno = zcx_due=>zcx_error_del_item_pais_dest-msgno
                              attr1 = CONV #( i_zsdt0174-destino_country )
                              attr2 = CONV #( i_zsdt0174-id_due_item   ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_error_del_item_pais_dest-msgno
            msgid  = zcx_due=>zcx_error_del_item_pais_dest-msgid
            msgv1  = CONV #( i_zsdt0174-destino_country )
            msgv2  = CONV #( i_zsdt0174-id_due_item ).
      ENDIF.
    ENDIF.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~eliminar_registro.

    DATA: var_answer TYPE c.

    DATA: lit_zdoc_exp TYPE TABLE OF zdoc_exp.

    CHECK me->at_due-id_due IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due EQ @me->at_due-id_due.

    CHECK sy-subrc EQ 0.

    IF _wl_0170-lcto_avulso = abap_true.

      IF ( _wl_0170-lib_leitura_opus       EQ abap_true ) OR
         ( _wl_0170-leitura_opus           EQ abap_true ) OR
         ( _wl_0170-solic_modificacao_opus EQ abap_true ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_leitura_lib_opus-msgid
                              msgno = zcx_due=>zcx_leitura_lib_opus-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_leitura_lib_opus-msgno
            msgid  = zcx_due=>zcx_leitura_lib_opus-msgid.
      ENDIF.

    ELSE.

      IF _wl_0170-status = '1'.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_due_registrada_portal-msgid
                              msgno = zcx_due=>zcx_due_registrada_portal-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_due_registrada_portal-msgno
            msgid  = zcx_due=>zcx_due_registrada_portal-msgid.
      ENDIF.

    ENDIF.

    IF ( _wl_0170-vinc_opus EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_vinc_opus-msgid
                            msgno = zcx_due=>zcx_vinc_opus-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_vinc_opus-msgno
          msgid  = zcx_due=>zcx_vinc_opus-msgid.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente eliminar o registro?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK var_answer EQ '1'.

    UPDATE zsdt0170 SET loekz          = abap_true
                        usnam_loekz    = sy-uname
                        erdat_loekz    = sy-datum
                        erzet_loekz    = sy-uzeit
     WHERE id_due = _wl_0170-id_due.


    CLEAR: lit_zdoc_exp[].
    SELECT *
      FROM zdoc_exp INTO TABLE lit_zdoc_exp
     WHERE id_due EQ _wl_0170-id_due.

    LOOP AT lit_zdoc_exp INTO DATA(lwa_zdoc_exp).
      UPDATE zdoc_exp SET id_due     = '0000000000'
                          numero_due = space
       WHERE id_due EQ _wl_0170-id_due.
    ENDLOOP.

    MESSAGE s123.

  ENDMETHOD.


  METHOD zif_due~enviar_due.

    DATA: http_client          TYPE REF TO if_http_client,
          xml_return           TYPE REF TO cl_xml_document,
          return_code          TYPE i,
          v_dt_registro_portal TYPE zsdt0170-dt_registro_portal,
          v_hr_registro_portal TYPE zsdt0170-hr_registro_portal,
          e_resultado          TYPE string,
          v_xml                TYPE string,
          v_authorization      TYPE string,
          v_x_csrf_token       TYPE string,
          v_url                TYPE ui_src_url,
          v_msg_show           TYPE c LENGTH 200,
          vl_ini_pos           TYPE i.

    CLEAR: v_authorization , v_x_csrf_token.

    r_enviada = abap_false.

    DATA(_id_token) = me->at_token->get_id_token( ).
    IF _id_token IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_autenticacao_not_found-msgid
                            msgno = zcx_due=>zcx_autenticacao_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_autenticacao_not_found-msgno
          msgid  = zcx_due=>zcx_autenticacao_not_found-msgid.
    ENDIF.

    IF ( me->at_due-status = '1' ) AND ( i_retransmissao IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_registrada_portal-msgid
                            msgno = zcx_due=>zcx_due_registrada_portal-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_due_registrada_portal-msgno
          msgid  = zcx_due=>zcx_due_registrada_portal-msgid.
    ENDIF.

    IF me->at_due-loekz EQ abap_true.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_reg_eliminado-msgid
                            msgno = zcx_due=>zcx_reg_eliminado-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_reg_eliminado-msgno
          msgid  = zcx_due=>zcx_reg_eliminado-msgid.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = TEXT-001.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    IF me->at_due-id_due_ref IS INITIAL.
      DATA(_due_webservice) = 'Inserir DU-e'.
      SELECT SINGLE *
        FROM zauth_webservice INTO @DATA(_auth_service)
       WHERE service = 'DUE_INSERIR'.
    ELSE.

      SELECT SINGLE *
        FROM zsdt0170 INTO @DATA(_wl_0170)
       WHERE id_due = @me->at_due-id_due_ref.

      IF ( sy-subrc NE 0 ) OR ( _wl_0170-numero_due IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_due_ref_not_found-msgid
                              msgno = zcx_due=>zcx_due_ref_not_found-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_due_ref_not_found-msgno
            msgid  = zcx_due=>zcx_due_ref_not_found-msgid.
      ENDIF.

      _due_webservice = 'Retificar DU-e'.
      SELECT SINGLE *
        FROM zauth_webservice INTO _auth_service
       WHERE service = 'DUE_RETIFICAR'.

      _auth_service-url = _auth_service-url && '/' &&  _wl_0170-numero_due.

    ENDIF.

    IF ( sy-subrc NE 0 ) OR ( _auth_service-url IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_webservice_not_found-msgid
                            msgno = zcx_due=>zcx_webservice_not_found-msgno
                            attr1 = CONV #( _due_webservice )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_webservice_not_found-msgno
          msgid  = zcx_due=>zcx_webservice_not_found-msgid
          msgv1  = CONV #( _due_webservice ).
    ELSE.
      v_url = _auth_service-url.
    ENDIF.

    me->monta_xml( RECEIVING r_xml  = v_xml ).

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN v_xml WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN v_xml WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN v_xml WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN v_xml WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN v_xml WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN v_xml WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN v_xml WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN v_xml WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN v_xml WITH 'o' IGNORING CASE.

    IF v_xml IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_xml_due_not_found-msgid
                            msgno = zcx_due=>zcx_xml_due_not_found-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_xml_due_not_found-msgno
          msgid  = zcx_due=>zcx_xml_due_not_found-msgid.
    ENDIF.

    "Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( v_url )
        ssl_id             = 'DFAULT' "ME->AT_PAR_AUTENTICACAO-SSL_ID
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/xml; charset=UTF-8'.

    v_authorization = me->at_token->get_token_jw( ).

    DATA(_token_opus) = me->at_token->get_token_opus( ).
    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Authorization'
        value = _token_opus.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'AuthorizationReceita'
        value = v_authorization.

    v_x_csrf_token  = me->at_token->get_token_csrf( ).

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'X-CSRF-Token'
        value = v_x_csrf_token.

*  CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
*    EXPORTING
*      NAME  = 'certificado'
*      VALUE = CONV #( ME->AT_PAR_AUTENTICACAO-SSL_ID ).
*
*  CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
*    EXPORTING
*      NAME  = 'senha'
*      VALUE = CONV #( ME->AT_PAR_AUTENTICACAO-PASSWORD ).

    CALL METHOD http_client->request->set_cdata
      EXPORTING
        data   = v_xml
        offset = 0
        length = strlen( v_xml ).

    CALL METHOD http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_falha_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_falha_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 2.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_status_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_status_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_status_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_status_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 3.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_processamento_ws-msgid
                              msgno = zcx_due=>zcx_falha_processamento_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_processamento_ws-msgno
            msgid  = zcx_due=>zcx_falha_processamento_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 4.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_timeout_ws-msgid
                              msgno = zcx_due=>zcx_timeout_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_timeout_ws-msgno
            msgid  = zcx_due=>zcx_timeout_ws-msgid
            msgv1  = CONV #( v_url ).
    ENDCASE.

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_falha_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_falha_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 2.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_status_comunicacao_ws-msgid
                              msgno = zcx_due=>zcx_status_comunicacao_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_status_comunicacao_ws-msgno
            msgid  = zcx_due=>zcx_status_comunicacao_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 3.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_falha_processamento_ws-msgid
                              msgno = zcx_due=>zcx_falha_processamento_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_falha_processamento_ws-msgno
            msgid  = zcx_due=>zcx_falha_processamento_ws-msgid
            msgv1  = CONV #( v_url ).
      WHEN 4.
        http_client->close( ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_timeout_ws-msgid
                              msgno = zcx_due=>zcx_timeout_ws-msgno
                              attr1 = CONV #( v_url ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_timeout_ws-msgno
            msgid  = zcx_due=>zcx_timeout_ws-msgid
            msgv1  = CONV #( v_url ).
    ENDCASE.

    "//Check return content
    CREATE OBJECT xml_return.

    CALL METHOD xml_return->parse_string
      EXPORTING
        stream = http_client->response->get_cdata( ).


    http_client->response->get_status( IMPORTING code = return_code ).

    e_resultado = http_client->response->get_cdata( ).

    IF return_code NE '200'.
      CLEAR: vl_ini_pos.

      IF vl_ini_pos IS INITIAL.
        FIND '(400)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '400'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(401)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '401'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(403)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '403'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(404)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '404'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(422)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '422'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(500)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '500'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(503)' IN e_resultado MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '503'.
        ENDIF.
      ENDIF.

      CASE return_code.
        WHEN 400.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_400-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_400-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_400-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_400-msgid.
        WHEN 401.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_401-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_401-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_401-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_401-msgid.
        WHEN 403.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_403-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_403-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_403-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_403-msgid.
        WHEN 404.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_404-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_404-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_404-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_404-msgid.
        WHEN 422.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_422-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_422-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_422-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_422-msgid.
        WHEN 500.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_500-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_500-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_500-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_500-msgid.
        WHEN 503.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_503-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_503-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_503-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_503-msgid.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex_others-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex_others-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex_others-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex_others-msgid.
      ENDCASE.

      RETURN.
    ENDIF.

    CALL METHOD xml_return->find_node_table
      EXPORTING
        tabname = 'error'
      IMPORTING
        t_nodes = DATA(_nodes_error).

    IF _nodes_error[] IS NOT INITIAL.

      CALL FUNCTION 'ZDUE_SHOW_LOG_ERRO'
        EXPORTING
          i_xml = e_resultado.

      TRY.
          DATA(_message) = CAST if_ixml_node( _nodes_error[ 1 ]-node )->get_value( ).
          DATA(_code)    = CAST if_ixml_node( _nodes_error[ 2 ]-node )->get_value( ).
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      IF _message IS NOT INITIAL.

        v_msg_show = _code && _message.

        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex-msgid
                              msgno = zcx_due=>zcx_retorno_siscomex-msgno
                              attr1 = CONV #( v_msg_show+000(50) )
                              attr2 = CONV #( v_msg_show+050(50) )
                              attr3 = CONV #( v_msg_show+100(50) )
                              attr4 = CONV #( v_msg_show+150(50) ) )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_retorno_siscomex-msgno
            msgid  = zcx_due=>zcx_retorno_siscomex-msgid
            msgv1  = CONV #( v_msg_show+000(50) )
            msgv2  = CONV #( v_msg_show+050(50) )
            msgv3  = CONV #( v_msg_show+100(50) )
            msgv4  = CONV #( v_msg_show+150(50) ).

      ELSE.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_desconhecido_siscomex-msgid
                              msgno = zcx_due=>zcx_erro_desconhecido_siscomex-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_erro_desconhecido_siscomex-msgno
            msgid  = zcx_due=>zcx_erro_desconhecido_siscomex-msgid.
      ENDIF.

    ELSE.

      "Get Elemento error
      CALL METHOD xml_return->find_simple_element
        EXPORTING
          name  = 'error'
        RECEIVING
          value = DATA(_return_error).

      IF _return_error IS NOT INITIAL.
        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'message'
          RECEIVING
            value = DATA(_return_message).

        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'code'
          RECEIVING
            value = DATA(_return_code).

        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'tag'
          RECEIVING
            value = DATA(_return_tag).

        IF _return_code IS NOT INITIAL.
          _return_message = _return_message && ' - ' && _return_code && ' - ' && _return_tag.
        ENDIF.

        IF _return_message IS NOT INITIAL.

          MESSAGE _return_message TYPE 'I'.

          v_msg_show = 'DU-e não foi enviada ao Siscomex!'.

          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex-msgno
                                attr1 = CONV #( v_msg_show+000(50) )
                                attr2 = CONV #( v_msg_show+050(50) )
                                attr3 = CONV #( v_msg_show+100(50) )
                                attr4 = CONV #( v_msg_show+150(50) ) )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex-msgid
              msgv1  = CONV #( v_msg_show+000(50) )
              msgv2  = CONV #( v_msg_show+050(50) )
              msgv3  = CONV #( v_msg_show+100(50) )
              msgv4  = CONV #( v_msg_show+150(50) ).

        ENDIF.
      ENDIF.

      CLEAR: v_dt_registro_portal, v_hr_registro_portal.

      "Get Elemento Retorno
      CALL METHOD xml_return->find_simple_element
        EXPORTING
          name  = 'pucomexReturn'
        RECEIVING
          value = DATA(_return_pucomex).

      IF _return_pucomex IS NOT INITIAL.

        "Get Numero DU-e
        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'due'
          RECEIVING
            value = DATA(_due).

        "Get Numero Ruc
        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'ruc'
          RECEIVING
            value = DATA(_ruc).

        "Get Chave de Acesso
        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'chaveDeAcesso'
          RECEIVING
            value = DATA(_chv_acesso).

        "Get Data Registro
        IF me->at_due-id_due_ref IS INITIAL.
          CALL METHOD xml_return->find_simple_element
            EXPORTING
              name  = 'date'
            RECEIVING
              value = DATA(_date).

          IF _date IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF '.' IN _date WITH ''.
            REPLACE ALL OCCURRENCES OF '-' IN _date WITH ''.
            REPLACE ALL OCCURRENCES OF ':' IN _date WITH ''.

            IF _date(8) IS NOT INITIAL.
              v_dt_registro_portal = _date(8).
            ENDIF.

            IF _date+9(6) IS NOT INITIAL.
              v_hr_registro_portal   = _date+9(6).
            ENDIF.
          ENDIF.
        ENDIF.

        "Get CPF Registro
        CALL METHOD xml_return->find_simple_element
          EXPORTING
            name  = 'cpf'
          RECEIVING
            value = DATA(_cpf).

      ENDIF.

      IF ( _return_pucomex IS NOT INITIAL ) AND
         ( _due            IS NOT INITIAL ) AND
         ( _ruc            IS NOT INITIAL ).

        IF i_retransmissao EQ abap_true.
          r_enviada = abap_true.
          RETURN.
        ENDIF.

        me->set_status( '1' ). "Registrada no Portal.

        UPDATE zsdt0170 SET status                = '1'
                            numero_due            = _due
                            numero_ruc            = _ruc
                            chave_acesso          = _chv_acesso
                            cpf_registro_portal   = _cpf
                            dt_envio              = sy-datum
                            hr_envio              = sy-uzeit
                            dt_registro_portal    = v_dt_registro_portal
                            hr_registro_portal    = v_hr_registro_portal
         WHERE id_due = me->at_due-id_due.

        COMMIT WORK.

        r_enviada = abap_true.

        me->registrar_log( ).

        TRY.
            IF me->at_due-emb_container EQ abap_false.
              me->lib_leitura_opus( ). "Liberar/Enviar DU-e Comex
            ENDIF.
          CATCH zcx_due.
        ENDTRY.

        "WBARBOSA 23102024 - US-153330 --->>>
        zcl_eudr_utils=>get_dues_eudr_from_nomeacao(
          EXPORTING
            i_id_nomeacao = me->at_due-id_nomeacao_tran
          RECEIVING
            r_dues        = DATA(r_dues) ).

        IF r_dues[] IS NOT INITIAL.  "Possui DU-e EUDR na nomeação
          me->zif_due~check_generation_file_eudr( i_id_nomeacao_tran = me->at_due-id_nomeacao_tran ).
        ENDIF.
        "WBARBOSA 23102024 - US-153330 <<<---

      ELSE.
        IF _message IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_retorno_siscomex-msgid
                                msgno = zcx_due=>zcx_retorno_siscomex-msgno
                                attr1 = CONV #( _message ) )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_retorno_siscomex-msgno
              msgid  = zcx_due=>zcx_retorno_siscomex-msgid
              msgv1  = CONV #( _message ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_erro_desconhecido_siscomex-msgid
                                msgno = zcx_due=>zcx_erro_desconhecido_siscomex-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_erro_desconhecido_siscomex-msgno
              msgid  = zcx_due=>zcx_erro_desconhecido_siscomex-msgid.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_due~gerar_nr_ruc.

    DATA: lva_answer  TYPE c.

    DATA: lva_ano_ruc         TYPE c LENGTH 01,
          lva_dec_ruc         TYPE c LENGTH 01, "Decada
          lva_sigla_pais_ruc  TYPE c LENGTH 02,
          lva_cnpj_raiz_ruc   TYPE c LENGTH 08,
          lva_ds_porto_ruc    TYPE c LENGTH 12,
          lva_ds_material_ruc TYPE c LENGTH 07,
          lva_contador_ruc    TYPE c LENGTH 04.

    DATA: lva_matnr_out        TYPE mara-matnr.
    DATA: lva_matnr_18         TYPE matnr18.

    DATA: lwa_dados_geracao_ruc TYPE zde_dados_geracao_ruc.

    DATA: lwa_zsdt0289      TYPE zsdt0289,
          lwa_zsdt0289_proc TYPE zsdt0289_proc.

    CLEAR: r_numero_ruc.

    lwa_dados_geracao_ruc = i_dados_geracao_ruc.

    IF ( lwa_dados_geracao_ruc-ano IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = 'Ano não informado para geração da RUC!'
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = 'Ano não informado para geração da RUC!'
          msgty  = 'E'.
    ENDIF.

    IF ( lwa_dados_geracao_ruc-ano < '2019' ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = 'Ano inferior a 2019 não é permitido!'
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = 'Ano inferior a 2019 não é permitido!'
          msgty  = 'E'.
    ENDIF.


*--------------------------------------------------------------------------------------------------*
*  Conversion Exit Alpha Input
*--------------------------------------------------------------------------------------------------*

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_dados_geracao_ruc-kunnr_exp
      IMPORTING
        output = lwa_dados_geracao_ruc-kunnr_exp.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_dados_geracao_ruc-matnr
      IMPORTING
        output = lva_matnr_18.

    lwa_dados_geracao_ruc-matnr = lva_matnr_18.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_dados_geracao_ruc-id_nomeacao_tran
      IMPORTING
        output = lwa_dados_geracao_ruc-id_nomeacao_tran.

*--------------------------------------------------------------------------------------------------*
*  Bloqueio de Geração de Numero RUC Simultaneo - Controle por ANO
*--------------------------------------------------------------------------------------------------*

    lwa_zsdt0289_proc-ano = lwa_dados_geracao_ruc-ano.

    DO 3 TIMES.

      DATA(lva_ruc_in_proc) = abap_false.

      SELECT SINGLE *
        FROM zsdt0289_proc INTO @DATA(lwa_zsdt0289_proc_reg)
       WHERE ano EQ @lwa_zsdt0289_proc-ano.

      IF sy-subrc NE 0.
        INSERT zsdt0289_proc FROM lwa_zsdt0289_proc.
        COMMIT WORK.
      ENDIF.

      CALL FUNCTION 'ENQUEUE_EZSDT0289_PROC'
        EXPORTING
          ano            = lwa_zsdt0289_proc-ano
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        lva_ruc_in_proc = abap_true.
        WAIT UP TO 5 SECONDS.
      ENDIF.
    ENDDO.

    IF lva_ruc_in_proc EQ abap_true.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |RUC em processamento para o ano: { lwa_zsdt0289_proc-ano } !|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |RUC em processamento para o ano: { lwa_zsdt0289_proc-ano } !|
          msgty  = 'E'.
    ENDIF.

*--------------------------------------------------------------------------------------------------*
*  Fim - Bloqueio de Geração de Numero RUC Simultaneo - Controle por ANO
*--------------------------------------------------------------------------------------------------*

    IF lwa_dados_geracao_ruc-kunnr_exp IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = 'Cliente Exp. não informado para geração da RUC!'
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = 'Cliente Exp. não informado para geração da RUC!'
          msgty  = 'E'.
    ENDIF.


    SELECT SINGLE *
      FROM kna1 INTO @DATA(lwa_kna1)
     WHERE kunnr EQ @lwa_dados_geracao_ruc-kunnr_exp.

    IF ( sy-subrc NE 0 ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Cliente Exp. { lwa_dados_geracao_ruc-kunnr_exp } não encontrado na XD03|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Cliente Exp. { lwa_dados_geracao_ruc-kunnr_exp } não encontrado na XD03|
          msgty  = 'E'.
    ENDIF.

    IF ( lwa_kna1-stcd1 IS INITIAL ) OR ( strlen( lwa_kna1-stcd1 ) <> 14 ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |CNPJ Cliente Exp. { lwa_dados_geracao_ruc-kunnr_exp }|
                            attr2 = |não encontrado ou inválido na XD03!|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |CNPJ Cliente Exp. { lwa_dados_geracao_ruc-kunnr_exp }|
          msgv2  = |não encontrado ou inválido na XD03!|
          msgty  = 'E'.
    ENDIF.

    IF lwa_dados_geracao_ruc-codigo_ra IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = 'Codigo RA não informado para geração da RUC!'
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = 'Codigo RA não informado para geração da RUC!'
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0168 INTO @DATA(lwa_zsdt0168)
     WHERE codigo_ra EQ @lwa_dados_geracao_ruc-codigo_ra.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Codigo RA: { lwa_dados_geracao_ruc-codigo_ra }|
                            attr2 = |não encontrado na transação ZSDT0137|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Codigo RA: { lwa_dados_geracao_ruc-codigo_ra }|
          msgv2  = |não encontrado na transação ZSDT0137|
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0169 INTO @DATA(lwa_zsdt0169)
     WHERE codigo_ra EQ @lwa_dados_geracao_ruc-codigo_ra.

    IF ( sy-subrc NE 0 ) OR ( lwa_zsdt0169-codigo_urf IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Codigo RA: { lwa_dados_geracao_ruc-codigo_ra }|
                            attr2 = |sem depara na transação ZSDT0137|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Codigo RA: { lwa_dados_geracao_ruc-codigo_ra }|
          msgv2  = |sem depara na transação ZSDT0137|
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0167 INTO @DATA(lwa_zsdt0167)
     WHERE codigo_urf EQ @lwa_zsdt0169-codigo_urf.

    IF ( sy-subrc NE 0 ) OR ( lwa_zsdt0167-ds_urf_abrev IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Desc. Abrev. URF: { lwa_zsdt0169-codigo_urf }|
                            attr2 = |não encontrada na transação ZSDT0137|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Desc. Abrev. URF: { lwa_zsdt0169-codigo_urf }|
          msgv2  = |não encontrada na transação ZSDT0137|
          msgty  = 'E'.
    ENDIF.

    CONDENSE lwa_zsdt0167-ds_urf_abrev NO-GAPS. "Tirar Espaços para concantenar na DU-e

    IF strlen( lwa_zsdt0167-ds_urf_abrev ) > 12 .
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Tamanho Desc. Abrev. URF: { lwa_zsdt0169-codigo_urf }|
                            attr2 = |superior a 12 digitos na transação ZSDT0137|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Tamanho Desc. Abrev. URF: { lwa_zsdt0169-codigo_urf }|
          msgv2  = |superior a 12 digitos na transação ZSDT0137|
          msgty  = 'E'.
    ENDIF.

    IF lwa_dados_geracao_ruc-matnr IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = 'Material não informado para geração da RUC!'
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = 'Material não informado para geração da RUC!'
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM mara INTO @DATA(lwa_mara)
     WHERE matnr EQ @lwa_dados_geracao_ruc-matnr.

    IF ( sy-subrc NE 0 ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Material { lwa_dados_geracao_ruc-matnr } |
                            attr2 = |não encontrado na transação MM03!|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Material { lwa_dados_geracao_ruc-matnr } |
          msgv2  = |não encontrado na transação MM03!|
          msgty  = 'E'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lwa_dados_geracao_ruc-matnr
      IMPORTING
        output = lva_matnr_out.

    IF strlen( lva_matnr_out ) > 7.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Tamanho Codigo Material { lwa_dados_geracao_ruc-matnr }|
                            attr2 = |superior a 7 digitos!|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Tamanho Codigo Material { lwa_dados_geracao_ruc-matnr }|
          msgv2  = |superior a 7 digitos!|
          msgty  = 'E'.
    ENDIF.

    IF lwa_dados_geracao_ruc-id_nomeacao_tran IS INITIAL.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                           msgno = zcx_due=>zcx_erro_geral-msgno
                           attr1 = 'Id. Nomeação não informado para geração da RUC!'
                           )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = 'Id. Nomeação não informado para geração da RUC!'
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM znom_transporte INTO @DATA(lwa_znom_transporte)
     WHERE id_nomeacao_tran EQ @lwa_dados_geracao_ruc-id_nomeacao_tran.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Não encontrada Nomeação com o Id:{ lwa_dados_geracao_ruc-id_nomeacao_tran }! |
                           )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Não encontrada Nomeação com o Id:{ lwa_dados_geracao_ruc-id_nomeacao_tran }! |
          msgty  = 'E'.
    ENDIF.


    SELECT MAX( contador )
      FROM zsdt0289 INTO @DATA(lwa_max_contador)
     WHERE ano EQ @lwa_dados_geracao_ruc-ano.

    ADD 1 TO lwa_max_contador.

    lva_ano_ruc           = lwa_dados_geracao_ruc-ano+3(1).   "Utimo digito do Ano
    lva_dec_ruc           = lwa_dados_geracao_ruc-ano+2(1).   "Decada
    lva_sigla_pais_ruc    = 'BR'.
    lva_cnpj_raiz_ruc     = lwa_kna1-stcd1(8).
    lva_ds_porto_ruc      = lwa_zsdt0167-ds_urf_abrev.
    lva_ds_material_ruc   = lva_matnr_out.
    lva_contador_ruc      = lwa_max_contador.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lva_ds_material_ruc
      IMPORTING
        output = lva_ds_material_ruc.

    CLEAR: lwa_zsdt0289.

    lva_ds_porto_ruc = zcl_string=>rpad( i_str  = CONV #( lva_ds_porto_ruc )
                                         i_qtd  = 12
                                         i_char = '0' ).

    lwa_zsdt0289-numero_ruc         = lva_ano_ruc            && " 1  Digitos  "Ano
                                      lva_sigla_pais_ruc     && " 2  Digitos
                                      lva_cnpj_raiz_ruc      && " 8  Digitos
                                      lva_dec_ruc            && " 1  Digitos  "Decada
                                      lva_ds_porto_ruc       && " 12 Digitos
                                      lva_ds_material_ruc    && " 7  Digitos
                                      lva_contador_ruc.         " 4  Digitos

    IF strlen( lwa_zsdt0289-numero_ruc ) NE 35.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |Houve um erro ao gerar o numero da RUC!|
                            attr2 = |Quantidade de 35 caracteres não atingida!|
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |Houve um erro ao gerar o numero da RUC!|
          msgv2  = |Quantidade de 35 caracteres não atingida!|
          msgty  = 'E'.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente gerar um novo numero de RUC?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = lva_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK lva_answer EQ '1'.

    lwa_zsdt0289-ano                = lwa_dados_geracao_ruc-ano.
    lwa_zsdt0289-kunnr_exp          = lwa_dados_geracao_ruc-kunnr_exp.
    lwa_zsdt0289-matnr              = lwa_dados_geracao_ruc-matnr.
    lwa_zsdt0289-codigo_ra          = lwa_dados_geracao_ruc-codigo_ra.
    lwa_zsdt0289-id_nomeacao_tran   = lwa_dados_geracao_ruc-id_nomeacao_tran.
    lwa_zsdt0289-contador           = lwa_max_contador.
    lwa_zsdt0289-dt_registro        = sy-datum.
    lwa_zsdt0289-hr_registro        = sy-uzeit.
    lwa_zsdt0289-us_registro        = sy-uname.

    MODIFY zsdt0289 FROM lwa_zsdt0289.

    COMMIT WORK.

    r_numero_ruc = lwa_zsdt0289-numero_ruc.

    CALL FUNCTION 'DEQUEUE_EZSDT0289_PROC'
      EXPORTING
        ano = lwa_zsdt0289_proc-ano.

  ENDMETHOD.


  METHOD zif_due~gerar_nr_ruc_with_screen.

    CLEAR: r_numero_ruc.

    CALL FUNCTION 'ZDUE_GERAR_NR_RUC_WITH_SCREEN'
      EXPORTING
        i_dados_geracao_ruc = i_dados_geracao_ruc
      IMPORTING
        e_numero_ruc        = r_numero_ruc.

  ENDMETHOD.


  METHOD zif_due~get_due.

    DATA: lwa_due TYPE zde_due.

    CLEAR: r_due, lwa_due.

    SELECT SINGLE *
      FROM zsdt0170 INTO CORRESPONDING FIELDS OF lwa_due
     WHERE id_due EQ i_due.

    SELECT *
      FROM zsdt0172 INTO CORRESPONDING FIELDS OF TABLE lwa_due-itens
      WHERE id_due EQ i_due.

    LOOP AT lwa_due-itens ASSIGNING FIELD-SYMBOL(<fs_item_due>).

      <fs_item_due>-exportador_nome    = zcl_string=>tira_acentos( i_texto = CONV #( <fs_item_due>-exportador_nome ) ).
      <fs_item_due>-exportador_cpl_end = zcl_string=>tira_acentos( i_texto = CONV #( <fs_item_due>-exportador_cpl_end ) ).
      <fs_item_due>-importador_nome    = zcl_string=>tira_acentos( i_texto = CONV #( <fs_item_due>-importador_nome ) ).
      <fs_item_due>-importador_cpl_end = zcl_string=>tira_acentos( i_texto = CONV #( <fs_item_due>-importador_cpl_end )  ).

      SELECT *
        FROM zsdt0173 INTO CORRESPONDING FIELDS OF TABLE <fs_item_due>-notas_referenciadas
       WHERE id_due      = <fs_item_due>-id_due
         AND id_due_item = <fs_item_due>-id_due_item.

      SELECT *
        FROM zsdt0174 INTO CORRESPONDING FIELDS OF TABLE <fs_item_due>-paises_destino
       WHERE id_due      = <fs_item_due>-id_due
         AND id_due_item = <fs_item_due>-id_due_item.

    ENDLOOP.

    r_due = lwa_due.

  ENDMETHOD.


  METHOD zif_due~get_xml_due.


    TYPES: BEGIN OF ty_xml,
             xml TYPE string,
           END OF ty_xml.

    DATA: lv_filename TYPE string,
          lv_fullpath TYPE string,
          lv_path     TYPE string,
          lv_action   TYPE i,
          lv_file     TYPE string.

    DATA: v_xml         TYPE string,
          v_xml_rec_nfe TYPE string,
          v_name_file   TYPE string,
          wa_xml        TYPE ty_xml,
          it_xml        TYPE STANDARD TABLE OF ty_xml.

    me->monta_xml( RECEIVING r_xml = v_xml ).

    IF i_download IS NOT INITIAL.

      CLEAR: it_xml[].
      wa_xml-xml = v_xml.
      APPEND wa_xml TO it_xml.

      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'Selecione o dirétório'
          default_extension = 'xml'
          default_file_name = lv_file
          file_filter       = '*.XML'
        CHANGING
          filename          = lv_filename
          path              = lv_path
          fullpath          = lv_fullpath
          user_action       = lv_action
        EXCEPTIONS
          cntl_error        = 1
          error_no_gui      = 2
          OTHERS            = 3.

      CHECK sy-subrc = 0.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = lv_fullpath
        TABLES
          data_tab                = it_xml
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD zif_due~gravar_registro.

    DATA: v_id_due_item TYPE zsdt0172-id_due_item.

    r_gravou = abap_false.

    CLEAR: e_id_due, e_zsdt0170, e_zsdt0172[] , e_zsdt0173[].

    CHECK me->ck_alterou EQ abap_true.

    IF me->at_due-lcto_avulso EQ abap_true.
      me->at_due-status = '1'.
    ENDIF.

    CHECK me->validar_registro( ) EQ abap_true.

    IF me->at_due-id_due IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZID_DUE'
        IMPORTING
          number                  = me->at_due-id_due
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF ( sy-subrc IS NOT INITIAL ) OR ( me->at_due-id_due IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_obj_nro_due_not_found-msgid
                              msgno = zcx_due=>zcx_obj_nro_due_not_found-msgno
                              attr1 = CONV #( 'ZID_DUE' )
                              )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_obj_nro_due_not_found-msgno
            msgid  = zcx_due=>zcx_obj_nro_due_not_found-msgid
            msgv1  = CONV #( 'ZID_DUE' ).
      ENDIF.

      me->at_due-dt_registro     = sy-datum.
      me->at_due-hr_registro     = sy-uzeit.
      me->at_due-us_registro     = sy-uname.
    ELSE.
      me->at_due-dt_modificacao  = sy-datum.
      me->at_due-hr_modificacao  = sy-uzeit.
      me->at_due-us_modificacao  = sy-uname.
    ENDIF.

    LOOP AT me->at_itens ASSIGNING FIELD-SYMBOL(<fs_item>).
      IF <fs_item>-id_due IS INITIAL.
        <fs_item>-id_due = me->at_due-id_due.
      ENDIF.
    ENDLOOP.

    LOOP AT me->at_itens_faturas_ref ASSIGNING FIELD-SYMBOL(<fs_item_fat_ref>).
      IF <fs_item_fat_ref>-id_due IS INITIAL.
        <fs_item_fat_ref>-id_due = me->at_due-id_due.
      ENDIF.
    ENDLOOP.

    LOOP AT me->at_itens_paises_destino ASSIGNING FIELD-SYMBOL(<fs_item_pais_dest>).
      IF <fs_item_pais_dest>-id_due IS INITIAL.
        <fs_item_pais_dest>-id_due = me->at_due-id_due.
      ENDIF.
    ENDLOOP.

*"// WBARBOSA 23102024 - US-153330
    IF zcl_eudr_utils=>check_pais_destino_eudr( i_sdt0174 = me->at_itens_paises_destino ) EQ abap_true.
      me->at_due-eudr = zcl_eudr_utils=>lc_s_eudr.
    ELSE.
      me->at_due-eudr = zcl_eudr_utils=>lc_n_eudr.
    ENDIF.
*"// WBARBOSA 23102024 - US-153330

    LOOP AT me->at_itens_lpco ASSIGNING FIELD-SYMBOL(<fs_item_lpco>).
      IF <fs_item_lpco>-id_due IS INITIAL.
        <fs_item_lpco>-id_due = me->at_due-id_due.
      ENDIF.
    ENDLOOP.

*----------------------------------------------------------------------------------------------------*
*   Gravar Registros
*----------------------------------------------------------------------------------------------------*

    "Cabeçalho DU-e
    MODIFY zsdt0170 FROM me->at_due.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_gravar_cab_due-msgid
                            msgno = zcx_due=>zcx_erro_gravar_cab_due-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_erro_gravar_cab_due-msgno
          msgid  = zcx_due=>zcx_erro_gravar_cab_due-msgid.
    ENDIF.

    "Itens DU-e
    DELETE FROM zsdt0172 WHERE id_due = me->at_due-id_due.
    MODIFY zsdt0172 FROM TABLE me->at_itens.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_gravar_itm_due-msgid
                            msgno = zcx_due=>zcx_erro_gravar_itm_due-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_erro_gravar_itm_due-msgno
          msgid  = zcx_due=>zcx_erro_gravar_itm_due-msgid.
    ENDIF.

    "Itens Faturas Referenciadas
    DELETE FROM zsdt0173 WHERE id_due = me->at_due-id_due.
    IF me->at_itens_faturas_ref[] IS NOT INITIAL.
      MODIFY zsdt0173 FROM TABLE me->at_itens_faturas_ref.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_gravar_itm_fat_ref-msgid
                              msgno = zcx_due=>zcx_erro_gravar_itm_fat_ref-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_erro_gravar_itm_fat_ref-msgno
            msgid  = zcx_due=>zcx_erro_gravar_itm_fat_ref-msgid.
      ENDIF.
    ENDIF.

    "Itens Países Destino
    DELETE FROM zsdt0174 WHERE id_due = me->at_due-id_due.
    IF me->at_itens_paises_destino[] IS NOT INITIAL.
      MODIFY zsdt0174 FROM TABLE me->at_itens_paises_destino.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_gravar_itm_pais_dest-msgid
                              msgno = zcx_due=>zcx_erro_gravar_itm_pais_dest-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_erro_gravar_itm_pais_dest-msgno
            msgid  = zcx_due=>zcx_erro_gravar_itm_pais_dest-msgid.
      ENDIF.
    ENDIF.

    "Itens LPCO
    DELETE FROM zsdt0190 WHERE id_due = me->at_due-id_due.
    IF me->at_itens_lpco[] IS NOT INITIAL.
      MODIFY zsdt0190 FROM TABLE me->at_itens_lpco.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_error_gravar_lpco_item-msgid
                              msgno = zcx_due=>zcx_error_gravar_lpco_item-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_error_gravar_lpco_item-msgno
            msgid  = zcx_due=>zcx_error_gravar_lpco_item-msgid.
      ENDIF.
    ENDIF.

*----------------------------------------------------------------------------------------------------*
*   Carrega Registros Tabela Fisica e deleta caso não encontre o mesmo atribuido ao Objeto.
*----------------------------------------------------------------------------------------------------*
*    "Itens
*    SELECT *
*      FROM ZSDT0172 INTO TABLE @DATA(_TG_0172)
*     WHERE ID_DUE = @ME->AT_DUE-ID_DUE.
*
*    "Itens Faturas Referencias
*    SELECT *
*      FROM ZSDT0173 INTO TABLE @DATA(_TG_0173)
*     WHERE ID_DUE = @ME->AT_DUE-ID_DUE.
*
*    "Itens Paises Destino
*    SELECT *
*      FROM ZSDT0174 INTO TABLE @DATA(_TG_0174)
*     WHERE ID_DUE = @ME->AT_DUE-ID_DUE.
*
*    "Delete Itens
*    LOOP AT _TG_0172 INTO DATA(_WL_0172).
*      READ TABLE ME->AT_ITENS INTO DATA(_WL_ITEM) WITH KEY ID_DUE      = _WL_0172-ID_DUE
*                                                           ID_DUE_ITEM = _WL_0172-ID_DUE_ITEM.
*      IF SY-SUBRC NE 0.
*        DELETE FROM ZSDT0172 WHERE ID_DUE      = _WL_0172-ID_DUE
*                               AND ID_DUE_ITEM = _WL_0172-ID_DUE_ITEM.
*        IF SY-SUBRC NE 0.
*          RAISE EXCEPTION TYPE ZCX_DUE
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_ERROR_DEL_ITEM-MSGID
*                                MSGNO = ZCX_DUE=>ZCX_ERROR_DEL_ITEM-MSGNO
*                                ATTR1 = CONV #( _WL_0172-ID_DUE_ITEM ) )
*              MSGTY  = 'E'
*              MSGNO  = ZCX_DUE=>ZCX_ERROR_DEL_ITEM-MSGNO
*              MSGID  = ZCX_DUE=>ZCX_ERROR_DEL_ITEM-MSGID
*              MSGV1  = CONV #( _WL_0172-ID_DUE_ITEM ).
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    "Delete Itens Faturas Referencias
*    LOOP AT _TG_0173 INTO DATA(_WL_0173).
*      READ TABLE ME->AT_ITENS_FATURAS_REF INTO DATA(_WL_ITEM_FAT_REF) WITH KEY ID_DUE      = _WL_0173-ID_DUE
*                                                                               ID_DUE_ITEM = _WL_0173-ID_DUE_ITEM
*                                                                               ID_FATURA   = _WL_0173-ID_FATURA.
*      IF SY-SUBRC NE 0.
*        DELETE FROM ZSDT0173 WHERE ID_DUE      = _WL_0173-ID_DUE
*                               AND ID_DUE_ITEM = _WL_0173-ID_DUE_ITEM
*                               AND ID_FATURA   = _WL_0173-ID_FATURA.
*        IF SY-SUBRC NE 0.
*          RAISE EXCEPTION TYPE ZCX_DUE
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_FT_REF-MSGID
*                                MSGNO = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_FT_REF-MSGNO
*                                ATTR1 = CONV #( _WL_0173-ID_FATURA_REF )
*                                ATTR2 = CONV #( _WL_0173-ID_DUE_ITEM   ) )
*              MSGTY  = 'E'
*              MSGNO  = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_FT_REF-MSGNO
*              MSGID  = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_FT_REF-MSGID
*              MSGV1  = CONV #( _WL_0173-ID_FATURA_REF )
*              MSGV2  = CONV #( _WL_0173-ID_DUE_ITEM   ).
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    "Delete Itens Paises Destino
*    LOOP AT _TG_0174 INTO DATA(_WL_0174).
*      READ TABLE ME->AT_ITENS_PAISES_DESTINO INTO DATA(_WL_ITEM_PAIS_DEST) WITH KEY ID_DUE          = _WL_0174-ID_DUE
*                                                                                    ID_DUE_ITEM     = _WL_0174-ID_DUE_ITEM
*                                                                                    DESTINO_COUNTRY = _WL_0174-DESTINO_COUNTRY.
*      IF SY-SUBRC NE 0.
*        DELETE FROM ZSDT0174 WHERE ID_DUE          = _WL_0174-ID_DUE
*                               AND ID_DUE_ITEM     = _WL_0174-ID_DUE_ITEM
*                               AND DESTINO_COUNTRY = _WL_0174-DESTINO_COUNTRY.
*        IF SY-SUBRC NE 0.
*          RAISE EXCEPTION TYPE ZCX_DUE
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_PAIS_DEST-MSGID
*                                MSGNO = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_PAIS_DEST-MSGNO
*                                ATTR1 = CONV #( _WL_0174-DESTINO_COUNTRY )
*                                ATTR2 = CONV #( _WL_0174-ID_DUE_ITEM   ) )
*              MSGTY  = 'E'
*              MSGNO  = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_PAIS_DEST-MSGNO
*              MSGID  = ZCX_DUE=>ZCX_ERROR_DEL_ITEM_PAIS_DEST-MSGID
*              MSGV1  = CONV #( _WL_0174-DESTINO_COUNTRY )
*              MSGV2  = CONV #( _WL_0174-ID_DUE_ITEM   ).
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

*    SELECT SINGLE *
*      FROM SETLEAF INTO @DATA(WL_SETLEAF)
*     WHERE SETNAME = 'DUE_DOWN_XML'
*       AND VALFROM = @SY-UNAME.
*    IF SY-SUBRC = 0.
*      ME->GET_XML_DUE( EXPORTING I_DOWNLOAD = 'X'
*                       RECEIVING E_XML_DUE  = DATA(_XML_DUE) ).
*    ENDIF.

    COMMIT WORK.
    me->ck_alterou = abap_false.

    e_id_due = me->at_due-id_due.

    e_zsdt0170 = me->at_due.
    e_zsdt0172 = me->at_itens.
    e_zsdt0173 = me->at_itens_faturas_ref.

    r_gravou = abap_true.
    MESSAGE s023.

  ENDMETHOD.


  METHOD zif_due~lib_leitura_opus.

    CHECK me->at_due-id_due IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_due-id_due
      IMPORTING
        output = me->at_due-id_due.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due EQ @me->at_due-id_due.

    CHECK ( sy-subrc EQ 0 ).

    IF ( _wl_0170-lib_leitura_opus EQ abap_true ) OR
       ( _wl_0170-leitura_opus EQ abap_true ) .
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_leitura_opus-msgid
                            msgno = zcx_due=>zcx_leitura_opus-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_leitura_opus-msgno
          msgid  = zcx_due=>zcx_leitura_opus-msgid.
    ENDIF.

    IF ( _wl_0170-loekz EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_reg_eliminado-msgid
                            msgno = zcx_due=>zcx_reg_eliminado-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_reg_eliminado-msgno
          msgid  = zcx_due=>zcx_reg_eliminado-msgid.
    ENDIF.

    IF ( _wl_0170-status NE '1' ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_not_autorizada-msgid
                            msgno = zcx_due=>zcx_due_not_autorizada-msgno
                            attr1 = CONV #( _wl_0170-id_due ) )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_due_not_autorizada-msgno
          msgid  = zcx_due=>zcx_due_not_autorizada-msgid
          msgv1  = CONV #( _wl_0170-id_due ).
    ENDIF.

    IF ( _wl_0170-bloqueio_interno EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_bloq_interno-msgid
                            msgno = zcx_due=>zcx_bloq_interno-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_bloq_interno-msgno
          msgid  = zcx_due=>zcx_bloq_interno-msgid.
    ENDIF.

    IF ( _wl_0170-emb_container EQ abap_true ).
      MESSAGE i145.
      RETURN.
    ENDIF.

    TRY.
        zcl_int_ob_envia_due_comex=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = _wl_0170 ).
      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
        MESSAGE i173.
        RETURN.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
        MESSAGE i173.
        RETURN.
    ENDTRY.

    _wl_0170-lib_leitura_opus = abap_true.
    _wl_0170-leitura_opus     = abap_true.
    _wl_0170-dt_leitura_opus  = sy-datum.
    _wl_0170-hr_leitura_opus  = sy-uzeit.

    CLEAR: _wl_0170-msg_opus.

    MODIFY zsdt0170 FROM _wl_0170.

    MESSAGE s116.

    me->registrar_log( ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_due~modify_matnr.

    r_alterado = abap_false.

    CHECK i_id_due IS NOT INITIAL .

    UPDATE zsdt0172 SET matnr = i_matnr
     WHERE id_due = i_id_due.

    IF sy-subrc = 0.
      r_alterado = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_due~modify_region.

    r_alterado = abap_false.

    CHECK me->at_due-id_due IS NOT INITIAL.

    IF ( i_land1 IS NOT INITIAL ) AND ( i_regio IS NOT INITIAL ).

      SELECT SINGLE *
        FROM t005s INTO @DATA(_wl_t005s)
       WHERE land1 EQ @i_land1
         AND bland EQ @i_regio.

      IF ( sy-subrc NE 0 )       OR
         ( i_land1  IS INITIAL ) OR
         ( i_regio  IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_regio_invalid-msgid
                              msgno = zcx_due=>zcx_obg_inf_regio_invalid-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_obg_inf_regio_invalid-msgno
            msgid  = zcx_due=>zcx_obg_inf_regio_invalid-msgid.
      ENDIF.

    ENDIF.

*    SELECT SINGLE *
*      FROM ZNOM_REME_NOTAS INTO @DATA(_WL_REME_NOTAS)
*     WHERE ID_DUE EQ @ME->AT_DUE-ID_DUE.
*
*    IF SY-SUBRC EQ 0.
*      RAISE EXCEPTION TYPE ZCX_DUE
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGID
*                            MSGNO = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGNO
*                            ATTR1 = CONV #( _WL_REME_NOTAS-DOCNUM )
*                            ATTR2 = CONV #( _WL_REME_NOTAS-ID_REMETENTE )
*                            ATTR3 = CONV #( _WL_REME_NOTAS-ID_FILIAL )
*                            ATTR4 = CONV #( _WL_REME_NOTAS-GRP_RETORNO ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGNO
*          MSGID  = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGID
*          MSGV1 = CONV #( _WL_REME_NOTAS-DOCNUM )
*          MSGV2 = CONV #( _WL_REME_NOTAS-ID_REMETENTE )
*          MSGV3 = CONV #( _WL_REME_NOTAS-ID_FILIAL )
*          MSGV4 = CONV #( _WL_REME_NOTAS-GRP_RETORNO ).
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM ZNOM_REMETENTE INTO @DATA(_WL_REMETENTE)
*     WHERE ID_DUE EQ @ME->AT_DUE-ID_DUE.
*
*    IF SY-SUBRC EQ 0.
*      RAISE EXCEPTION TYPE ZCX_DUE
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGID
*                            MSGNO = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGNO
*                            ATTR1 = CONV #( _WL_REMETENTE-ID_FILIAL )
*                            ATTR2 = CONV #( _WL_REMETENTE-GRP_RETORNO ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGNO
*          MSGID  = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGID
*          MSGV1 = CONV #( _WL_REMETENTE-ID_FILIAL )
*          MSGV2 = CONV #( _WL_REMETENTE-GRP_RETORNO ).
*    ENDIF.

    UPDATE zsdt0170 SET land1 = i_land1
                        regio = i_regio
     WHERE id_due = me->at_due-id_due.

    IF sy-subrc = 0.
      MESSAGE s076.
      r_alterado = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD zif_due~modify_tp_exportacao.

    r_alterado = abap_false.

*    IF I_TP_EXPORTACAO IS INITIAL.
*      RAISE EXCEPTION TYPE ZCX_DUE
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGID
*                            MSGNO = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGNO )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGNO
*          MSGID  = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGID.
*    ENDIF.

    CHECK me->at_due-id_due IS NOT INITIAL.

*    SELECT SINGLE *
*      FROM ZNOM_REME_NOTAS INTO @DATA(_WL_REME_NOTAS)
*     WHERE ID_DUE EQ @ME->AT_DUE-ID_DUE.
*
*    IF SY-SUBRC EQ 0.
*      RAISE EXCEPTION TYPE ZCX_DUE
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGID
*                            MSGNO = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGNO
*                            ATTR1 = CONV #( _WL_REME_NOTAS-DOCNUM )
*                            ATTR2 = CONV #( _WL_REME_NOTAS-ID_REMETENTE )
*                            ATTR3 = CONV #( _WL_REME_NOTAS-ID_FILIAL )
*                            ATTR4 = CONV #( _WL_REME_NOTAS-GRP_RETORNO ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGNO
*          MSGID  = ZCX_DUE=>ZCX_DUE_VINC_NF_REMETENTE-MSGID
*          MSGV1 = CONV #( _WL_REME_NOTAS-DOCNUM )
*          MSGV2 = CONV #( _WL_REME_NOTAS-ID_REMETENTE )
*          MSGV3 = CONV #( _WL_REME_NOTAS-ID_FILIAL )
*          MSGV4 = CONV #( _WL_REME_NOTAS-GRP_RETORNO ).
*    ENDIF.

*    SELECT SINGLE *
*      FROM ZNOM_REMETENTE INTO @DATA(_WL_REMETENTE)
*     WHERE ID_DUE EQ @ME->AT_DUE-ID_DUE.
*
*    IF SY-SUBRC EQ 0.
*      RAISE EXCEPTION TYPE ZCX_DUE
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGID
*                            MSGNO = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGNO
*                            ATTR1 = CONV #( _WL_REMETENTE-ID_FILIAL )
*                            ATTR2 = CONV #( _WL_REMETENTE-GRP_RETORNO ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGNO
*          MSGID  = ZCX_DUE=>ZCX_DUE_VINC_REMETENTE-MSGID
*          MSGV1 = CONV #( _WL_REMETENTE-ID_FILIAL )
*          MSGV2 = CONV #( _WL_REMETENTE-GRP_RETORNO ).
*    ENDIF.

    UPDATE zsdt0170 SET tp_exportacao = i_tp_exportacao
     WHERE id_due = me->at_due-id_due.

    IF sy-subrc = 0.
      MESSAGE s096.
      r_alterado = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD zif_due~monta_xml.

    DATA: xvalor      TYPE string,
          v_xml       TYPE string,
          v_xml_drawn TYPE string,
          v_valor_aux TYPE string,
          v_matnr18   TYPE matnr18,
          v_id_c11    TYPE c LENGTH 11,
          v_id_c14    TYPE c LENGTH 14,
* Inicio - falheiros - 22.11.2022
          v_drawnback TYPE c.

    DATA: v_ncm        TYPE j1b_nf_xml_item-ncm,
          v_exp        TYPE zsdt0172-ue_exportada,
          v_meins_in   TYPE mara-meins,
          v_meins_out  TYPE mara-meins,
          v_qtde       TYPE j1b_nf_xml_item-qtrib,
          v_vlr        TYPE zsdt0172-vlr_local_embarque,
          v_dia        TYPE char2,
          v_mes        TYPE char2,
          v_ano        TYPE char4,
          v_data       TYPE char10,
          v_quantidade TYPE string,
          v_itens      TYPE i.

* Fim - falheiros - 22.11.2022

    CLEAR: r_xml, v_xml.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE v_xml xvalor INTO v_xml.
    END-OF-DEFINITION.

    DEFINE conc_xml_out.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.

* Inicio - falheiros - 22.11.2022
    DEFINE conc_xml_drawn.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE v_xml_drawn xvalor INTO v_xml_drawn.
    END-OF-DEFINITION.
* Fim - falheiros - 22.11.2022

    CASE me->at_due-tp_due.

      WHEN '1' OR '2'. " Sem  NF-e / Com NF-e

* Inicio - falheiros - 22.11.2022

        LOOP AT me->at_itens INTO DATA(wl_item_aux).
          SELECT SINGLE *
            FROM zsdt0305
            INTO @DATA(ls_zsdt0305)
            WHERE id_due      = @wl_item_aux-id_due AND
                  id_due_item = @wl_item_aux-id_due_item.
          IF sy-subrc IS INITIAL.
            v_drawnback = abap_true.
          ENDIF.
        ENDLOOP.
        IF v_drawnback IS NOT INITIAL.

          conc_xml_drawn         '<GoodsShipment>'.
          conc_xml_drawn         '<GovernmentAgencyGoodsItem>'.

          "      CONC_XML        <!-- Drawback Suspensão - Ato concessório comum terceiro (AC) - com NF de venda-->

          LOOP AT me->at_itens INTO DATA(wl_item_aux2).

            conc_xml_drawn         '<AdditionalDocument>'.

            SELECT SINGLE *
              FROM zsdt0305
              INTO @DATA(ls_zsdt0305_aux)
              WHERE id_due      = @wl_item_aux2-id_due AND
                    id_due_item = @wl_item_aux2-id_due_item.
            IF sy-subrc IS INITIAL.

              "      CONC_XML      <!-- tipo do ato -->
              conc_xml_drawn         '<CategoryCode>'.
              conc_xml_drawn         ls_zsdt0305_aux-tp_ato.
              conc_xml_drawn        '</CategoryCode>'. "

              "<!-- identificador do ato -->
              conc_xml_drawn        '<ID>'.
              conc_xml_drawn        ls_zsdt0305_aux-nr_drawback.
              conc_xml_drawn        '</ID>'.  "<< numero drawback preenchido na linha do item

              "<!-- ncm -->
              conc_xml_drawn        '<DrawbackHsClassification>' .
              conc_xml_drawn        wl_item_aux2-codigo_ncm.
              conc_xml_drawn        '</DrawbackHsClassification>'. " << ncm do material da linha do item


              "<!-- cnpj do beneficiário do ato -->
              "<!-- sendo este diferente do cnpj do declarante, deve ser informada nf de venda que possuem drawnback -- prencher com as notas que tem drawnback informado seu respectivo cnpj
              conc_xml_drawn        '<DrawbackRecipientId>'.
              conc_xml_drawn        wl_item_aux2-fatura_id+6(14).
              conc_xml_drawn        '</DrawbackRecipientId>'.

              "<!-- valor sem cobertura cambial do ato -->
              conc_xml_drawn        '<ValueWithoutExchangeCoverAmount>'.
              conc_xml_drawn        '0.0'.
              conc_xml_drawn        '</ValueWithoutExchangeCoverAmount>'." sempre zero

              "<!-- valor com cobertura cambial do ato -->
              conc_xml_drawn        '<ValueWithExchangeCoverAmount>'.

              v_quantidade = wl_item_aux2-vlr_cond_venda.
              CONDENSE v_quantidade.
              conc_xml_drawn        v_quantidade .
              conc_xml_drawn        '</ValueWithExchangeCoverAmount>'. "valor total da nfe

              "<!-- número do item correspondente do ato -->
              conc_xml_drawn        '<ItemID>'.
              v_itens = v_itens + 1.
              conc_xml_drawn        v_itens.
              conc_xml_drawn        '</ItemID>'.


*              v_ncm = wl_item_aux2-codigo_ncm.
*
*              REPLACE ALL OCCURRENCES OF '.' IN v_ncm WITH ''.
*
*              SELECT SINGLE *
*                FROM setleaf INTO @DATA(_wl_set_ncm_utrib)
*               WHERE setname = 'MAGGI_NCM_UTRIB_EXP'
*                 AND valfrom = @v_ncm.
*
*              IF sy-subrc EQ 0.
*                v_exp = 'TON'.
*              ELSE.
*                v_exp = 'KG'.
*              ENDIF.
*
*              v_meins_in  = 'KG'.
*              v_qtde      = wl_item_aux2-peso_liq_total.
*              v_meins_out = wl_item_aux2-ue_exportada.
*
*              IF v_meins_out = 'TON'.
*                v_meins_out = 'TO'.
*              ENDIF.
*
*              CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*                EXPORTING
*                  i_matnr              = zsdt0172-matnr
*                  i_in_me              = v_meins_in
*                  i_out_me             = v_meins_out
*                  i_menge              = v_qtde
*                IMPORTING
*                  e_menge              = v_qtde
*                EXCEPTIONS
*                  error_in_application = 1
*                  error                = 2
*                  OTHERS               = 3.


              "<!-- quantidade do ato-->
              conc_xml_drawn '<QuantityQuantity>'.
              v_quantidade = wl_item_aux2-qtde_ue_exportada.
              CONDENSE v_quantidade.
              conc_xml_drawn v_quantidade.
              conc_xml_drawn '</QuantityQuantity>'.    "  quantidade da nota de exportação

              "<!-- notas fiscais de venda -->
              conc_xml_drawn '<Invoice>'.
              "<!-- chave da nf de venda-->
              conc_xml_drawn '<ID>' .
              conc_xml_drawn wl_item_aux2-fatura_id.
              conc_xml_drawn '</ID>'.

              SELECT SINGLE credat
                  FROM j_1bnfe_active
                  INTO @DATA(l_credat)
                 WHERE regio   = @wl_item_aux2-fatura_id(2)
                   AND nfyear  = @wl_item_aux2-fatura_id+2(2)
                   AND nfmonth = @wl_item_aux2-fatura_id+4(2)
                   AND stcd1   = @wl_item_aux2-fatura_id+6(14)
                   AND model   = @wl_item_aux2-fatura_id+20(2)
                   AND serie   = @wl_item_aux2-fatura_id+22(3)
                   AND nfnum9  = @wl_item_aux2-fatura_id+25(9)
                   AND docnum9 = @wl_item_aux2-fatura_id+34(9)
                   AND cdv     = @wl_item_aux2-fatura_id+43(1)
                   AND form NE ''.
              IF sy-subrc IS INITIAL.

                v_dia = l_credat+6(2).
                v_mes = l_credat+4(2).
                v_ano = l_credat(4).

                CONCATENATE v_ano '-' v_mes '-' v_dia INTO v_data.
              ENDIF.

              "<!-- data de emissão da nf de venda-->
              conc_xml_drawn '<IssueDateTime>'.
              conc_xml_drawn v_data.
              conc_xml_drawn '</IssueDateTime>'.
              "<!-- valor da nf de venda-->
              conc_xml_drawn '<CustomsValueAmount>'.
              conc_xml_drawn wl_item_aux2-vlr_cond_venda .
              conc_xml_drawn '</CustomsValueAmount>'.

              "<!-- quantidade da nf de venda-->
              conc_xml_drawn '<QuantityQuantity>'.
              v_quantidade = wl_item_aux2-qtde_ue_exportada.
              CONDENSE v_quantidade.
              conc_xml_drawn v_quantidade.
              conc_xml_drawn '</QuantityQuantity>'.
              conc_xml_drawn '</Invoice>'.

            ENDIF.

            conc_xml_drawn '</AdditionalDocument>'.

          ENDLOOP.

          "<!-- enquadramentos - dbk suspensao = 81101 -->
          conc_xml_drawn '<GovernmentProcedure>'.
          conc_xml_drawn '<CurrentCode>'.
          conc_xml_drawn ls_zsdt0305-cod_enquadramento.
          conc_xml_drawn '</CurrentCode>' . "<< código enquadramento preenchido a linha do item
          conc_xml_drawn '</GovernmentProcedure>'.
          conc_xml_drawn '<SequenceNumeric>'.
          conc_xml_drawn ls_zsdt0305_aux-id_due_item.
          conc_xml_drawn '</SequenceNumeric>'.

          conc_xml_drawn '</GovernmentAgencyGoodsItem>'.
          "...
          conc_xml_drawn '</GoodsShipment>'.

        ENDIF.
* Fim - falheiros - 22.11.2022


        "Unidade Receita Federal e Recinto Alfandegado de Despacho
        conc_xml         '<DeclarationOffice>'.
        conc_xml          '<ID listID="token">'.
        conc_xml             me->at_due-codigo_urf_despacho.
        conc_xml          '</ID>'.
        conc_xml           '<Warehouse>'.

        IF me->at_due-tp_cod_local_despacho = '281'. "Recinto Alfandegado
          conc_xml          '<ID schemeID="token">'.
          conc_xml             me->at_due-codigo_ra_despacho.
          conc_xml          '</ID>'.
        ELSE. "Fora do Recinto
          conc_xml          '<ID schemeID="token">'.
          conc_xml             me->at_due-cnpj_cpf_resp_loc_desp.
          conc_xml          '</ID>'.

          conc_xml          '<LatitudeMeasure unitCode="">'.
          conc_xml             me->at_due-local_despacho_latitude.
          conc_xml          '</LatitudeMeasure>'.
          conc_xml          '<LongitudeMeasure unitCode="">'.
          conc_xml             me->at_due-local_despacho_longitude.
          conc_xml          '</LongitudeMeasure>'.
          conc_xml          '<Address>'.
          conc_xml            '<Line languageID="">'.
          conc_xml               me->at_due-local_despacho_end.
          conc_xml            '</Line>'.
          conc_xml          '</Address>'.
        ENDIF.

        conc_xml            '<TypeCode>'.
        conc_xml               me->at_due-tp_cod_local_despacho.
        conc_xml            '</TypeCode>'.
        conc_xml           '</Warehouse>'.
        conc_xml         '</DeclarationOffice>'.

        "CUS = Forma Exportação
        conc_xml         '<AdditionalInformation>'.
        conc_xml           '<StatementCode>'.
        conc_xml              me->at_due-forma_exportacao.
        conc_xml           '</StatementCode>'.
        conc_xml           '<StatementTypeCode>CUS</StatementTypeCode>'.
        conc_xml         '</AdditionalInformation>'.

        "TRA - Caso Especial Transporte
        IF me->at_due-caso_especial_transporte IS NOT INITIAL.
          conc_xml       '<AdditionalInformation>'.
          conc_xml         '<StatementCode>'.
          conc_xml            me->at_due-caso_especial_transporte.
          conc_xml         '</StatementCode>'.
          conc_xml         '<StatementTypeCode>TRA</StatementTypeCode>'.
          conc_xml       '</AdditionalInformation>'.
        ENDIF.

        "AHZ - Situação Especial
        conc_xml         '<AdditionalInformation>'.
        conc_xml           '<StatementCode>'.
        conc_xml              me->at_due-situacao_especial.
        conc_xml           '</StatementCode>'.
        conc_xml           '<StatementTypeCode>AHZ</StatementTypeCode>'.
        conc_xml         '</AdditionalInformation>'.

        "AAI - Observações Gerais
        IF me->at_due-observacoes_gerais IS NOT INITIAL.
          conc_xml       '<AdditionalInformation>'.
          conc_xml         '<StatementDescription>'.
          conc_xml            me->at_due-observacoes_gerais.
          conc_xml         '</StatementDescription>'.
          conc_xml         '<StatementTypeCode>AAI</StatementTypeCode>'.
          conc_xml       '</AdditionalInformation>'.
        ENDIF.

        "DEF - Motivo
        IF me->at_due-motivo IS NOT INITIAL.
          conc_xml       '<AdditionalInformation>'.
          conc_xml         '<StatementTypeCode>DEF</StatementTypeCode>'.
          conc_xml         '<StatementDescription>'.
          conc_xml            me->at_due-motivo.
          conc_xml         '</StatementDescription>'.
          conc_xml       '</AdditionalInformation>'.
        ENDIF.

        IF me->at_due-ciencia_alertas_rfb EQ abap_true. "Informe Ciencia Riscos RFB
          conc_xml      '<AdditionalInformation>'.
          conc_xml         '<StatementTypeCode>ACT</StatementTypeCode>'.
          conc_xml      '</AdditionalInformation>'.
        ENDIF.

        "Moeda Cambio
        conc_xml         '<CurrencyExchange>'.
        conc_xml           '<CurrencyTypeCode>'.
        conc_xml              me->at_due-moeda_cambio.
        conc_xml           '</CurrencyTypeCode>'.
        conc_xml         '</CurrencyExchange>'.

        "Declarante
        conc_xml         '<Declarant>'.
        conc_xml           '<ID schemeID="token">'.
        conc_xml              me->at_due-cnpj_declarante.
        conc_xml           '</ID>'.
        conc_xml         '</Declarant>'.

        "Unidade Receita Federal e Recinto Alfandegado de Embarque
        conc_xml         '<ExitOffice>'.
        conc_xml           '<ID schemeID="token">'.
        conc_xml              me->at_due-codigo_urf_embarque.
        conc_xml           '</ID>'.
        conc_xml           '<Warehouse>'.
        conc_xml            '<ID schemeID="token">'.
        conc_xml               me->at_due-codigo_ra_embarque.
        conc_xml            '</ID>'.
        conc_xml            '<TypeCode>'.
        conc_xml               '281'.          "Recinto Alfandegado
        conc_xml            '</TypeCode>'.
        conc_xml           '</Warehouse>'.
        conc_xml         '</ExitOffice>'.

        "Itens DU-e
        LOOP AT me->at_itens INTO DATA(wl_item).

          conc_xml       '<GoodsShipment>'.

          IF me->at_due-tp_due = '1'. "Sem NF-e.

            "Dados Exportador
            conc_xml       '<Exporter>'.
            conc_xml        '<Name languageID="">'.
            conc_xml           wl_item-exportador_nome.
            conc_xml        '</Name>'.
            conc_xml        '<ID schemeID="token">'.
            conc_xml           wl_item-exportador_cnpj.
            conc_xml        '</ID>'.
            conc_xml        '<Address>'.
            conc_xml          '<CountryCode>'.
            conc_xml             wl_item-exportador_country.
            conc_xml          '</CountryCode>'.
            conc_xml          '<CountrySubDivisionCode>'.
            CONCATENATE             wl_item-exportador_country '-' wl_item-exportador_region INTO v_valor_aux.
            conc_xml             v_valor_aux.
            conc_xml          '</CountrySubDivisionCode>'.

            "Dados Complementar Endereço
            IF wl_item-importador_cpl_end IS NOT INITIAL.
              conc_xml        '<Line languageID="">'.
              conc_xml           wl_item-importador_cpl_end.
              conc_xml        '</Line>'.
            ENDIF.

            conc_xml        '</Address>'.
            conc_xml       '</Exporter>'.
          ENDIF.

          "Dados Mercadoria
          conc_xml         '<GovernmentAgencyGoodsItem>'.

          conc_xml          '<CustomsValueAmount languageID="">'.
          conc_xml              wl_item-vlr_local_embarque.
          conc_xml          '</CustomsValueAmount>'.
          conc_xml          '<SequenceNumeric>'.
          conc_xml              wl_item-id_due_item.
          conc_xml          '</SequenceNumeric>'.

          "Países de Destino
          LOOP AT me->at_itens_paises_destino INTO DATA(wl_item_pais_destino) WHERE id_due_item EQ wl_item-id_due_item.
            conc_xml        '<Destination>'.
            conc_xml          '<CountryCode>'.
            conc_xml             wl_item_pais_destino-destino_country.
            conc_xml          '</CountryCode>'.
            conc_xml          '<GoodsMeasure>'.
            conc_xml            '<TariffQuantity unitCode="">'.
            conc_xml               wl_item_pais_destino-destino_qtde_ue_exportada.
            conc_xml            '</TariffQuantity>'.
            conc_xml          '</GoodsMeasure>'.
            conc_xml        '</Destination>'.
          ENDLOOP.

          "Mercadoria
          conc_xml          '<Commodity>'.

          IF wl_item-desc_complementar IS NOT INITIAL.
            conc_xml          '<Description languageID="">'.
            conc_xml             wl_item-desc_complementar.
            conc_xml          '</Description>'.
          ENDIF.

          conc_xml            '<ValueAmount schemeID="token">'.
          conc_xml               wl_item-vlr_cond_venda.
          conc_xml            '</ValueAmount>'.

          CASE me->at_due-tp_due.
            WHEN '1'. "Sem NF-e.

              IF wl_item-desc_mercadoria IS NOT INITIAL.
                conc_xml      '<CommercialDescription languageID="">'.
                conc_xml         wl_item-desc_mercadoria.
                conc_xml      '</CommercialDescription>'.
              ENDIF.

              "NCM
              conc_xml        '<Classification>'.

              IF wl_item-codigo_ncm IS NOT INITIAL.
                conc_xml        '<ID schemeID="token">'.
                v_valor_aux    = wl_item-codigo_ncm.
                REPLACE ALL OCCURRENCES OF '.' IN v_valor_aux WITH ''.
                conc_xml           v_valor_aux.
                conc_xml        '</ID>'.
              ENDIF.

              conc_xml          '<IdentificationTypeCode>'.
              conc_xml             'HS'.
              conc_xml          '</IdentificationTypeCode>'.
              conc_xml        '</Classification>'.

              "Unidade de Medida Estatística
              conc_xml        '<GoodsMeasure>'.
              conc_xml          '<TypeCode>'.
              conc_xml             'AAF'.
              conc_xml          '</TypeCode>'.
              conc_xml          '<TariffQuantity unitCode="">'.
              conc_xml             wl_item-qtde_ue_exportada.
              conc_xml          '</TariffQuantity>'.
              conc_xml        '</GoodsMeasure>'.

              "Unidade de Medida Comercial
              conc_xml        '<GoodsMeasure>'.
              conc_xml          '<UnitDescription languageID="">'.
              conc_xml             wl_item-uc_exportada.
              conc_xml          '</UnitDescription>'.
              conc_xml          '<TypeCode>'.
              conc_xml             'ABW'.
              conc_xml          '</TypeCode>'.
              conc_xml          '<TariffQuantity unitCode="">'.
              conc_xml             wl_item-qtde_uc_exportada.
              conc_xml          '</TariffQuantity>'.
              conc_xml        '</GoodsMeasure>'.

              "Produto
              conc_xml        '<Product>'.
              conc_xml          '<ID schemeID="token">'.

              CLEAR: v_valor_aux.
              v_matnr18 = wl_item-matnr.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = v_matnr18
                IMPORTING
                  output = v_valor_aux.

              conc_xml             v_valor_aux.
              conc_xml          '</ID>'.

              conc_xml          '<IdentifierTypeCode>'.
              conc_xml             'VN'.
              conc_xml          '</IdentifierTypeCode>'.
              conc_xml        '</Product>'.

              IF wl_item-atrib_code IS NOT INITIAL.
                conc_xml      '<ProductCharacteristics>'.
                conc_xml        '<TypeCode>'.
                conc_xml           wl_item-atrib_code.
                conc_xml        '</TypeCode>'.
                conc_xml        '<Description>'.
                conc_xml           wl_item-desc_atrib.
                conc_xml        '</Description>'.
                conc_xml      '</ProductCharacteristics>'.
              ENDIF.

            WHEN '2'. "Com NF-e

              "Itens da Fatura
              conc_xml        '<InvoiceLine>'.
              conc_xml          '<SequenceNumeric>1</SequenceNumeric>'.

              LOOP AT me->at_itens_faturas_ref INTO DATA(wl_item_fatura_ref) WHERE id_due_item  EQ wl_item-id_due_item.
                "AND REGISTRO_CCT EQ ABAP_TRUE.

                IF wl_item_fatura_ref-id_fatura IS INITIAL.
                  wl_item_fatura_ref-id_fatura = '1'.
                ENDIF.

                conc_xml          '<ReferencedInvoiceLine>'.
                conc_xml             '<SequenceNumeric>'.
                conc_xml                wl_item_fatura_ref-id_fatura.
                conc_xml             '</SequenceNumeric>'.
                conc_xml             '<InvoiceIdentificationID schemeID="token">'.
                conc_xml                wl_item_fatura_ref-id_fatura_ref.
                conc_xml             '</InvoiceIdentificationID>'.
                conc_xml             '<GoodsMeasure>'.

                IF wl_item_fatura_ref-complemento EQ abap_false.
                  conc_xml              '<TariffQuantity unitCode="">'.
                  conc_xml                 wl_item_fatura_ref-qtde_ue_exportada.
                  conc_xml              '</TariffQuantity>'.
                ENDIF.

                conc_xml             '</GoodsMeasure>'.
                conc_xml          '</ReferencedInvoiceLine>'.
              ENDLOOP.

              conc_xml        '</InvoiceLine>'.
          ENDCASE.

          conc_xml          '</Commodity>'.

          "Medidas e Medições
          conc_xml          '<GoodsMeasure>'.
          conc_xml            '<NetNetWeightMeasure unitCode="">'.
          conc_xml               wl_item-peso_liq_total.
          conc_xml            '</NetNetWeightMeasure>'.
          conc_xml          '</GoodsMeasure>'.

          "Enquadramento
          conc_xml          '<GovernmentProcedure>'.
          conc_xml            '<CurrentCode schemeID="token">'.
          conc_xml               wl_item-codigo_enquadramento.
          conc_xml            '</CurrentCode>'.
          conc_xml          '</GovernmentProcedure>'.

          LOOP AT me->at_itens_lpco INTO DATA(wl_item_lpco) WHERE id_due_item  EQ wl_item-id_due_item.
            conc_xml        '<AdditionalDocument>'.
            conc_xml           '<CategoryCode>LPCO</CategoryCode>'.
            conc_xml           '<ID>'.
            conc_xml               wl_item_lpco-nr_lpco.
            conc_xml           '</ID>'.
            conc_xml        '</AdditionalDocument>'.
          ENDLOOP.

          conc_xml         '</GovernmentAgencyGoodsItem>'.

          IF me->at_due-tp_due = '1'. "Sem NF-e.
            "Dados Importador
            conc_xml       '<Importer>'.
            conc_xml        '<Name languageID="">'.
            conc_xml           wl_item-importador_nome.
            conc_xml        '</Name>'.
            conc_xml        '<Address>'.
            conc_xml          '<CountryCode>'.
            conc_xml             wl_item-importador_country.
            conc_xml          '</CountryCode>'.
            IF wl_item-importador_cpl_end  IS NOT INITIAL.
              conc_xml        '<Line languageID="">'.
              conc_xml           wl_item-importador_cpl_end.
              conc_xml        '</Line>'.
            ENDIF.
            conc_xml        '</Address>'.
            conc_xml       '</Importer>'.
          ENDIF.

          "Fatura
          conc_xml         '<Invoice>'.

          IF me->at_due-tp_due = '2'. "Com NF-e.
            conc_xml        '<ID schemeID="token">'.
            conc_xml           wl_item-fatura_id.
            conc_xml        '</ID>'.
          ENDIF.

          conc_xml          '<TypeCode>'.
          conc_xml             wl_item-fatura_tp_codigo.
          conc_xml          '</TypeCode>'.

          CASE me->at_due-tp_due.
            WHEN '1'. "Sem NF-e

              "Motivo Dispensa nova Fiscal
              conc_xml      '<AdditionalInformation>'.
              conc_xml         '<StatementCode>'.
              conc_xml             wl_item-fatura_motivo_dispensa_nf.
              conc_xml         '</StatementCode>'.
              conc_xml         '<StatementTypeCode>'.
              conc_xml           'ACG'.
              conc_xml         '</StatementTypeCode>'.
              conc_xml      '</AdditionalInformation>'.

            WHEN '2'. "Com NF-e
              LOOP AT me->at_itens_faturas_ref INTO wl_item_fatura_ref WHERE id_due_item  EQ wl_item-id_due_item.
                conc_xml    '<ReferencedInvoice>'.
                conc_xml     '<ID schemeID="token">'.
                conc_xml        wl_item_fatura_ref-id_fatura_ref.
                conc_xml     '</ID>'.

                CASE wl_item_fatura_ref-complemento.
                  WHEN abap_true.
                    conc_xml '<TypeCode>'.
                    conc_xml    'COM'.
                    conc_xml '</TypeCode>'.
                  WHEN abap_false.
                    conc_xml '<TypeCode>'.
                    conc_xml    'REM'.
                    conc_xml '</TypeCode>'.
                ENDCASE.

                conc_xml     '<Submitter>'.
                conc_xml       '<ID schemeID="token">'.

                CLEAR: v_id_c14, v_id_c11.
                IF wl_item_fatura_ref-emissor_cnpj IS NOT INITIAL.
                  v_id_c14 = wl_item_fatura_ref-emissor_cnpj.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = v_id_c14
                    IMPORTING
                      output = v_id_c14.

                  conc_xml          v_id_c14.

                ELSEIF wl_item_fatura_ref-emissor_cpf IS NOT INITIAL.
                  v_id_c11 = wl_item_fatura_ref-emissor_cpf.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = v_id_c11
                    IMPORTING
                      output = v_id_c11.

                  conc_xml          v_id_c11.
                ENDIF.


                conc_xml       '</ID>'.
                conc_xml     '</Submitter>'.
                conc_xml    '</ReferencedInvoice>'.
              ENDLOOP.
          ENDCASE.

          conc_xml         '</Invoice>'.

          "Condição Venda
          conc_xml         '<TradeTerms>'.
          conc_xml          '<ConditionCode>'.
          conc_xml             wl_item-codigo_cond_venda.
          conc_xml          '</ConditionCode>'.
          conc_xml         '</TradeTerms>'.

          conc_xml       '</GoodsShipment>'.
        ENDLOOP.

        "Numero RUC
        IF me->at_due-numero_ruc IS NOT INITIAL.
          conc_xml       '<UCR>'.
          conc_xml         '<TraderAssignedReferenceID schemeID="token">'.
          conc_xml            me->at_due-numero_ruc.
          conc_xml         '</TraderAssignedReferenceID>'.
          conc_xml       '</UCR>'.
        ENDIF.

    ENDCASE.

    CASE me->at_due-tp_due.
      WHEN '1'. " Sem NF-e
        conc_xml_out     '<Declaration xsi:schemaLocation="urn:wco:datamodel:WCO:GoodsDeclaration:1 GoodsDeclaration_1p0_DUE.xsd"'.
        conc_xml_out       ' xmlns:ds="urn:wco:datamodel:WCO:GoodsDeclaration_DS:1"'.
        conc_xml_out       ' xmlns="urn:wco:datamodel:WCO:GoodsDeclaration:1"'.
        conc_xml_out       ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.

        conc_xml_out       '<DeclarationNoNF>'.
        conc_xml_out         '<ID schemeID="token"/>'.
        conc_xml_out             v_xml.
        conc_xml_out       '</DeclarationNoNF>'.
        "* Inicio - falheiros - 22.11.2022    ]
        IF v_xml_drawn IS NOT INITIAL.
          conc_xml_out       '<declarationDrawbackIsencao>'.
          conc_xml_out             v_xml_drawn.
          conc_xml_out       '</declarationDrawbackIsencao>'.
        ENDIF.
        conc_xml_out     '</Declaration>'.


* Fim - falheiros - 22.11.2022

      WHEN '2'. " Com NF-e

        conc_xml_out     '<Declaration xsi:schemaLocation="urn:wco:datamodel:WCO:GoodsDeclaration:1 GoodsDeclaration_1p0_DUE.xsd"'.
        conc_xml_out     	 ' xmlns:ds="urn:wco:datamodel:WCO:GoodsDeclaration_DS:1"'.
        conc_xml_out     	 ' xmlns="urn:wco:datamodel:WCO:GoodsDeclaration:1"'.
        conc_xml_out       ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.

        conc_xml_out       '<DeclarationNFe>'.
        conc_xml_out         '<ID/>'.
        conc_xml_out           v_xml.
        conc_xml_out       '</DeclarationNFe>'.
        "* Inicio - falheiros - 22.11.2022    ]
        IF v_xml_drawn IS NOT INITIAL.
          conc_xml_out       '<DeclarationDrawbackIsencao>'.
          conc_xml_out             v_xml_drawn.
          conc_xml_out       '</DeclarationDrawbackIsencao>'.
        ENDIF.
* Fim - falheiros - 22.11.2022
        conc_xml_out     '</Declaration>'.



    ENDCASE.

  ENDMETHOD.


  METHOD zif_due~registrar_log.

    DATA: wl_zsdt0170 TYPE zsdt0170,
          wl_zsdt0185 TYPE zsdt0185,
          v_id_log    TYPE zsdt0185-id_log.

    CHECK me->at_due-id_due IS NOT INITIAL.

    CLEAR: wl_zsdt0170, wl_zsdt0185, v_id_log.

    SELECT SINGLE *
      FROM zsdt0170 INTO wl_zsdt0170
     WHERE id_due = me->at_due-id_due.

    CHECK sy-subrc EQ 0.

    MOVE-CORRESPONDING wl_zsdt0170 TO wl_zsdt0185.

    SELECT MAX( id_log )
      FROM zsdt0185 INTO v_id_log
     WHERE id_due = wl_zsdt0170-id_due.

    ADD 1 TO v_id_log.

    MOVE-CORRESPONDING wl_zsdt0170 TO wl_zsdt0185.

    wl_zsdt0185-id_log                    =  v_id_log.
    wl_zsdt0185-sistema                   =  'SAP'.
    wl_zsdt0185-dt_registro               =  sy-datum.
    wl_zsdt0185-hr_registro               =  sy-uzeit.
    wl_zsdt0185-us_registro               =  sy-uname.

    MODIFY zsdt0185 FROM wl_zsdt0185.

  ENDMETHOD.


  METHOD zif_due~registro_due.

    CALL FUNCTION 'ZREGISTRO_DUE'
      EXPORTING
        i_registro_due = i_registro_due.

  ENDMETHOD.


  METHOD zif_due~set_bukrs.

    me->at_due-bukrs = i_bukrs.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_cabecalho.

    me->at_due = i_zsdt0170.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_caso_especial_transporte.

    me->at_due-caso_especial_transporte = i_caso_especial_transporte.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_cnpj_cpf_resp_loc_desp.

    me->at_due-cnpj_cpf_resp_loc_desp = i_cnpj_cpf_resp_loc_desp.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_cnpj_declarante.

    me->at_due-cnpj_declarante = i_cnpj_declarante.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_codigo_ra_despacho.

    me->at_due-codigo_ra_despacho = i_codigo_ra_despacho.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_codigo_ra_embarque.


    me->at_due-codigo_ra_embarque = i_codigo_ra_embarque.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_codigo_urf_despacho.

    me->at_due-codigo_urf_despacho = i_codigo_urf_despacho.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_codigo_urf_embarque.


    me->at_due-codigo_urf_embarque = i_codigo_urf_embarque.

    me->ck_alterou = abap_true.



  ENDMETHOD.


  METHOD zif_due~set_forma_exportacao.

    me->at_due-forma_exportacao = i_forma_exportacao.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_id_due.

    me->at_due-id_due = i_id_due.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_id_due_ref.


    me->at_due-id_due_ref = i_id_due_ref.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_local_despacho_end.

    me->at_due-local_despacho_end = i_local_despacho_end.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_local_despacho_latitude.

    me->at_due-local_despacho_latitude = i_local_despacho_latitude.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_local_despacho_longitude.

    me->at_due-local_despacho_longitude = i_local_despacho_longitude.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_moeda_cambio.

    me->at_due-moeda_cambio = i_moeda_cambio.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_motivo.

    me->at_due-motivo = i_motivo.

    me->ck_alterou = abap_true.


  ENDMETHOD.


  METHOD zif_due~set_numero_due.

    me->at_due-numero_due = i_numero_due.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_numero_ruc.

    me->at_due-numero_ruc = i_numero_ruc.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_observacoes_gerais.

    me->at_due-observacoes_gerais = i_observacoes_gerais.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_situacao_especial.

    me->at_due-situacao_especial = i_situacao_especial.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_status.

    me->at_due-status = i_status.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_token.

    FREE me->at_token.

    CHECK i_token IS NOT INITIAL.

*    IF ( I_TOKEN->GET_BUKRS( )  NE ME->AT_DUE-BUKRS  ).
*      MESSAGE S074 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.

    me->at_token = i_token.

  ENDMETHOD.


  METHOD zif_due~set_tp_cod_local_despacho.

    me->at_due-tp_cod_local_despacho = i_tp_cod_local_despacho.

    me->ck_alterou = abap_true.

  ENDMETHOD.


  METHOD zif_due~set_tp_due.


    me->at_due-tp_due = i_tp_due.

    me->ck_alterou = abap_true.



  ENDMETHOD.


  METHOD zif_due~set_xmls_exportacao.

    me->at_xmls_exportacao = i_xmls_exportacao.

  ENDMETHOD.


  METHOD zif_due~sol_modific_opus.

    CHECK me->at_due-id_due IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_due-id_due
      IMPORTING
        output = me->at_due-id_due.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due EQ @me->at_due-id_due.

    CHECK ( sy-subrc EQ 0 ).

    IF ( _wl_0170-leitura_opus EQ abap_false ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_leitura_nao_realizada_opus-msgid
                            msgno = zcx_due=>zcx_leitura_nao_realizada_opus-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_leitura_nao_realizada_opus-msgno
          msgid  = zcx_due=>zcx_leitura_nao_realizada_opus-msgid.
    ENDIF.

    IF ( _wl_0170-solic_modificacao_opus EQ abap_true ).
      MESSAGE s117.
      RETURN.
    ENDIF.

    _wl_0170-lib_leitura_opus       = abap_true.
    _wl_0170-leitura_opus           = abap_false.
    _wl_0170-solic_modificacao_opus = abap_true.
    _wl_0170-bloq_opus              = i_bloq_opus.

    IF ( _wl_0170-tp_due      EQ '1'       ) OR "DU-e Antecipada
       ( _wl_0170-performance EQ abap_true ).   "Performance (Não possui DU-e antecipada)
      _wl_0170-bloq_opus = abap_true. "Bloquear DU-e no Comex.
    ENDIF.

    CLEAR: _wl_0170-msg_opus,
           _wl_0170-dt_leitura_opus,
           _wl_0170-hr_leitura_opus.

    "3948394890384903
    TRY.
        zcl_int_ob_sol_alt_due_comex=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = _wl_0170 ).
      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
        MESSAGE i175.
        RETURN.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
        MESSAGE i175.
        RETURN.
    ENDTRY.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170_ret)
     WHERE id_due EQ @me->at_due-id_due.

    IF sy-subrc EQ 0 AND _wl_0170_ret-vinc_opus EQ abap_false.
      MESSAGE s174.
    ELSE.
      MESSAGE i176.
    ENDIF.

    me->registrar_log( ).

  ENDMETHOD.


  METHOD zif_due~troca_due.

    DATA: it_znom_remetente  TYPE TABLE OF znom_remetente,
          it_znom_reme_notas TYPE TABLE OF znom_reme_notas,
          it_znom_prog_reme  TYPE TABLE OF znom_prog_reme,
          it_zdoc_exp        TYPE TABLE OF zdoc_exp.

    DATA: wl_znom_remetente  TYPE znom_remetente,
          wl_znom_reme_notas TYPE znom_reme_notas,
          wl_znom_prog_reme  TYPE znom_prog_reme,
          wl_zdoc_exp        TYPE zdoc_exp.


    r_trocada = abap_false.

    CHECK ( me->at_due-id_due IS NOT INITIAL ) AND ( me->at_due-id_due NE i_id_due ).

*----------------------------------------------------------------------------------*
*   Carrega DU-e Atual
*----------------------------------------------------------------------------------*
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due EQ @me->at_due-id_due.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_registro_due_not_found-msgid
                            msgno = zcx_due=>zcx_registro_due_not_found-msgno
                            attr1 = CONV #( me->at_due-id_due )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_registro_due_not_found-msgno
          msgid  = zcx_due=>zcx_registro_due_not_found-msgid
          msgv1  = CONV #( me->at_due-id_due ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0172 INTO @DATA(_wl_0172)
     WHERE id_due EQ @me->at_due-id_due.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_registro_due_not_found-msgid
                            msgno = zcx_due=>zcx_registro_due_not_found-msgno
                            attr1 = CONV #( me->at_due-id_due )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_registro_due_not_found-msgno
          msgid  = zcx_due=>zcx_registro_due_not_found-msgid
          msgv1  = CONV #( me->at_due-id_due ).
    ENDIF.

*----------------------------------------------------------------------------------*
*   Carrega DU-e Nova
*----------------------------------------------------------------------------------*
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170_novo)
     WHERE id_due EQ @i_id_due.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_registro_due_not_found-msgid
                            msgno = zcx_due=>zcx_registro_due_not_found-msgno
                            attr1 = CONV #( i_id_due )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_registro_due_not_found-msgno
          msgid  = zcx_due=>zcx_registro_due_not_found-msgid
          msgv1  = CONV #( i_id_due ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0172 INTO @DATA(_wl_0172_novo)
     WHERE id_due EQ @i_id_due.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_registro_due_not_found-msgid
                            msgno = zcx_due=>zcx_registro_due_not_found-msgno
                            attr1 = CONV #( i_id_due )
                            )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_registro_due_not_found-msgno
          msgid  = zcx_due=>zcx_registro_due_not_found-msgid
          msgv1  = CONV #( i_id_due ).
    ENDIF.

*----------------------------------------------------------------------------------*
*   Validações
*----------------------------------------------------------------------------------*

    IF ( _wl_0170_novo-status NE '1' ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_not_autorizada-msgid
                            msgno = zcx_due=>zcx_due_not_autorizada-msgno
                            attr1 = CONV #( _wl_0170_novo-id_due ) )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_due_not_autorizada-msgno
          msgid  = zcx_due=>zcx_due_not_autorizada-msgid
          msgv1  = CONV #( _wl_0170_novo-id_due ).
    ENDIF.

    IF ( _wl_0170-lib_leitura_opus EQ abap_true ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_leitura_opus-msgid
                            msgno = zcx_due=>zcx_leitura_opus-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_leitura_opus-msgno
          msgid  = zcx_due=>zcx_leitura_opus-msgid.
    ENDIF.

    IF ( _wl_0170-bloqueio_interno EQ abap_false ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_sem_bloq_interno-msgid
                            msgno = zcx_due=>zcx_due_sem_bloq_interno-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_due_sem_bloq_interno-msgno
          msgid  = zcx_due=>zcx_due_sem_bloq_interno-msgid.
    ENDIF.

    IF _wl_0170-id_nomeacao_tran NE _wl_0170_novo-id_nomeacao_tran.
      MESSAGE i130.
      RETURN.
    ENDIF.

    IF _wl_0170-bukrs NE _wl_0170_novo-bukrs.
      MESSAGE i131.
      RETURN.
    ENDIF.

    IF _wl_0170-codigo_ra_embarque NE _wl_0170_novo-codigo_ra_embarque.
      MESSAGE i132.
      RETURN.
    ENDIF.

    IF _wl_0172-matnr NE _wl_0172_novo-matnr.
      MESSAGE i133.
      RETURN.
    ENDIF.

    "Checa Retificação DU-e Atual
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170_ret)
     WHERE id_due_ref EQ @me->at_due-id_due
       AND loekz      EQ @abap_false.

    IF sy-subrc EQ 0.
      MESSAGE i135 WITH _wl_0170_ret-id_due.
      RETURN.
    ENDIF.

    "Checa Retificação DU-e Nova
    SELECT SINGLE *
      FROM zsdt0170 INTO _wl_0170_ret
     WHERE id_due_ref EQ _wl_0170_novo-id_due
       AND loekz      EQ abap_false.

    IF sy-subrc EQ 0.
      MESSAGE i135 WITH _wl_0170_ret-id_due.
      RETURN.
    ENDIF.

*----------------------------------------------------------------------------------*
*   Processo de Troca
*----------------------------------------------------------------------------------*
    CLEAR: it_znom_remetente[], it_znom_reme_notas[], it_znom_prog_reme[], it_zdoc_exp[].

    "Carregar Tabelas de Referencia DU-e
    SELECT *
      FROM znom_remetente INTO TABLE it_znom_remetente
     WHERE id_due EQ _wl_0170-id_due.

    SELECT *
      FROM znom_reme_notas INTO TABLE it_znom_reme_notas
     WHERE id_due EQ _wl_0170-id_due.

    IF ( it_znom_reme_notas[] IS INITIAL ) AND ( it_znom_remetente[] IS INITIAL ).
      MESSAGE i136.
      RETURN.
    ENDIF.

    SELECT *
      FROM znom_prog_reme INTO TABLE it_znom_prog_reme
     WHERE id_due EQ _wl_0170-id_due.

    SELECT *
      FROM zdoc_exp INTO TABLE it_zdoc_exp
     WHERE id_due EQ _wl_0170-id_due.

    "Copiar Registros substituindo DU-e antiga pela Nova
    LOOP AT it_znom_remetente INTO DATA(_wl_znom_remetente).
      CLEAR: wl_znom_remetente.

      MOVE-CORRESPONDING _wl_znom_remetente TO wl_znom_remetente.

      wl_znom_remetente-id_due     = _wl_0170_novo-id_due.
      wl_znom_remetente-numero_due = _wl_0170_novo-numero_due.

      MODIFY znom_remetente FROM wl_znom_remetente.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE i143.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT it_znom_reme_notas INTO DATA(_wl_znom_reme_notas).
      CLEAR: wl_znom_reme_notas.

      MOVE-CORRESPONDING _wl_znom_reme_notas TO wl_znom_reme_notas.

      wl_znom_reme_notas-id_due     = _wl_0170_novo-id_due.
      wl_znom_reme_notas-numero_due = _wl_0170_novo-numero_due.

      MODIFY znom_reme_notas FROM wl_znom_reme_notas.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE i137.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT it_znom_prog_reme INTO DATA(_wl_znom_prog_reme).
      CLEAR: wl_znom_prog_reme.

      MOVE-CORRESPONDING _wl_znom_prog_reme TO wl_znom_prog_reme.

      wl_znom_prog_reme-id_due     = _wl_0170_novo-id_due.

      MODIFY znom_prog_reme FROM wl_znom_prog_reme.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE i138.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT it_zdoc_exp INTO DATA(_wl_doc_exp).
      CLEAR: wl_zdoc_exp.

      MOVE-CORRESPONDING _wl_doc_exp TO wl_zdoc_exp.

      wl_zdoc_exp-id_due     = _wl_0170_novo-id_due.
      wl_zdoc_exp-numero_due = _wl_0170_novo-numero_due.

      MODIFY zdoc_exp FROM wl_zdoc_exp.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE i139.
        RETURN.
      ENDIF.
    ENDLOOP.

    "Deleta Registros DU-e Antiga |----------------------------

    "Remetentes
    DELETE FROM znom_remetente WHERE id_due EQ _wl_0170-id_due.

    SELECT SINGLE *
      FROM znom_remetente INTO @DATA(_wl_znom_remetente_tmp)
     WHERE id_due EQ @_wl_0170-id_due.

    IF ( sy-subrc EQ 0 ) AND ( it_znom_remetente[] IS NOT INITIAL ).
      ROLLBACK WORK.
      MESSAGE i144.
      RETURN.
    ENDIF.

    "Notas Remetentes
    DELETE FROM znom_reme_notas WHERE id_due EQ _wl_0170-id_due.

    SELECT SINGLE *
      FROM znom_reme_notas INTO @DATA(_wl_znom_reme_notas_tmp)
     WHERE id_due EQ @_wl_0170-id_due.

    IF ( sy-subrc EQ 0 ) AND ( it_znom_reme_notas[] IS NOT INITIAL ).
      ROLLBACK WORK.
      MESSAGE i140.
      RETURN.
    ENDIF.

    "Remessas
    DELETE FROM znom_prog_reme WHERE id_due EQ _wl_0170-id_due.

    SELECT SINGLE *
      FROM znom_prog_reme INTO @DATA(_wl_znom_prog_reme_tmp)
     WHERE id_due EQ @_wl_0170-id_due.

    IF ( sy-subrc EQ 0 ) AND ( it_znom_prog_reme[] IS NOT INITIAL ).
      ROLLBACK WORK.
      MESSAGE i141.
      RETURN.
    ENDIF.

    "Documentos Exportação
    DELETE FROM zdoc_exp WHERE id_due EQ _wl_0170-id_due.

    SELECT SINGLE *
      FROM zdoc_exp INTO @DATA(_wl_zdoc_exp_tmp)
     WHERE id_due EQ @_wl_0170-id_due.

    IF ( sy-subrc EQ 0 ) AND ( it_zdoc_exp[] IS NOT INITIAL ).
      ROLLBACK WORK.
      MESSAGE i142.
      RETURN.
    ENDIF.

    UPDATE zsdt0170 SET id_due_troca = _wl_0170_novo-id_due
                        usnam_troca  = sy-uname
                        erdat_troca  = sy-datum
                        erzet_troca  = sy-uzeit
     WHERE id_due = _wl_0170-id_due.

    COMMIT WORK.

    MESSAGE s134.

    r_trocada = abap_true.


  ENDMETHOD.


  METHOD zif_due~validar_registro.

    DATA: tg_itens_aux                 TYPE TABLE OF zsdt0172,
          tg_paises_aux                TYPE TABLE OF zsdt0174,
          tg_itens_fat_ref_aux         TYPE TABLE OF zsdt0173,
          v_qtde_c_01                  TYPE c LENGTH 13,
          v_qtde_c_02                  TYPE c LENGTH 13,
          v_count                      TYPE i,
          tg_marc                      TYPE TABLE OF marc,
          v_vlr_loc_emb_calc           TYPE zsdt0172-vlr_local_embarque,
          v_vlr_margem                 TYPE zsdt0172-vlr_local_embarque,
          v_vlr_margem_positivo        TYPE zsdt0172-vlr_local_embarque,
          v_vlr_margem_negativo        TYPE zsdt0172-vlr_local_embarque,
          v_peso_liq_total_pais        TYPE zsdt0174-peso_liq_total,
          v_peso_liq_total_fat_sem_cct TYPE zsdt0174-peso_liq_total,
          v_peso_liq_total_fat_com_cct TYPE zsdt0174-peso_liq_total,
          v_qtde_ue_exportada          TYPE zsdt0174-destino_qtde_ue_exportada.

    DATA: var_answer TYPE c.

    DATA(_ciencia_embarque_nao_europeu) = abap_false.  "WBARBOSA 23102024 - US-153330

    r_validou = abap_false.

    DELETE me->at_itens_faturas_ref WHERE id_due_item IS INITIAL. "Descartar faturas referenciadas sem Id DUe Item.

    IF ( me->at_due-status      EQ '1'        ) AND "DU-e já registrada no Portal.
       ( me->at_due-lcto_avulso EQ abap_false ).

      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_registrada_portal-msgid
                            msgno = zcx_due=>zcx_due_registrada_portal-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_due_registrada_portal-msgno
          msgid  = zcx_due=>zcx_due_registrada_portal-msgid.
    ENDIF.

    IF me->at_due-id_due IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->at_due-id_due
        IMPORTING
          output = me->at_due-id_due.

      SELECT SINGLE *
        FROM zsdt0170 INTO @DATA(_wl_0170)
       WHERE id_due EQ @me->at_due-id_due.

      IF ( sy-subrc EQ 0 ) AND ( _wl_0170-lib_leitura_opus EQ abap_true ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_leitura_opus-msgid
                              msgno = zcx_due=>zcx_leitura_opus-msgno )
            msgty  = 'E'
            msgno  = zcx_due=>zcx_leitura_opus-msgno
            msgid  = zcx_due=>zcx_leitura_opus-msgid.
      ENDIF.
    ENDIF.

    IF ( me->at_due-tp_due NE '1' ) AND  "Sem NF-e
       ( me->at_due-tp_due NE '2' ).     "Com NF-e
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_tipo_due-msgid
                            msgno = zcx_due=>zcx_obg_inf_tipo_due-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_obg_inf_tipo_due-msgno
          msgid  = zcx_due=>zcx_obg_inf_tipo_due-msgid.
    ENDIF.


    IF ( me->at_due-id_nomeacao_tran IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgid
                            msgno = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgno
          msgid  = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgid.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_due-id_nomeacao_tran
      IMPORTING
        output = me->at_due-id_nomeacao_tran.

    SELECT SINGLE *
      FROM znom_transporte INTO @DATA(_wl_znom_transp)
     WHERE id_nomeacao_tran EQ @me->at_due-id_nomeacao_tran.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgid
                            msgno = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgno
          msgid  = zcx_due=>zcx_obg_atrib_nomeacao_trans-msgid.
    ENDIF.

    CASE me->at_due-tp_due.
      WHEN '1' OR '2'. "Sem NF-e/Com NF-e

        IF  ( me->at_due-performance EQ abap_false ).
          IF ( me->at_due-bukrs IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_bukrs-msgid
                                  msgno = zcx_due=>zcx_obg_inf_bukrs-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_bukrs-msgno
                msgid  = zcx_due=>zcx_obg_inf_bukrs-msgid.
          ENDIF.

          SELECT SINGLE *
            FROM t001 INTO @DATA(lwa_t001)
           WHERE bukrs EQ @me->at_due-bukrs.

          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_bukrs-msgid
                                  msgno = zcx_due=>zcx_obg_inf_bukrs-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_bukrs-msgno
                msgid  = zcx_due=>zcx_obg_inf_bukrs-msgid.
          ENDIF.

        ELSE.
          IF ( me->at_due-kunnr IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_kunnr-msgid
                                  msgno = zcx_due=>zcx_obg_inf_kunnr-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_kunnr-msgno
                msgid  = zcx_due=>zcx_obg_inf_kunnr-msgid.
          ENDIF.

          SELECT SINGLE *
            FROM kna1 INTO @DATA(lwa_kna1)
           WHERE kunnr EQ @me->at_due-kunnr.

          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_kunnr-msgid
                                  msgno = zcx_due=>zcx_obg_inf_kunnr-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_kunnr-msgno
                msgid  = zcx_due=>zcx_obg_inf_kunnr-msgid.
          ENDIF.

        ENDIF.

*        IF ( ME->AT_DUE-TP_EXPORTACAO NE 'D' ).
*          IF ( ME->AT_DUE-REGIO IS INITIAL ).
*            RAISE EXCEPTION TYPE ZCX_DUE
*              EXPORTING
*                TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_OBG_INF_REGIO-MSGID
*                                  MSGNO = ZCX_DUE=>ZCX_OBG_INF_REGIO-MSGNO )
*                MSGTY  = 'E'
*                MSGNO  = ZCX_DUE=>ZCX_OBG_INF_REGIO-MSGNO
*                MSGID  = ZCX_DUE=>ZCX_OBG_INF_REGIO-MSGID.
*          ENDIF.
*        ENDIF.

*        IF ME->AT_DUE-TP_EXPORTACAO IS INITIAL.
*          RAISE EXCEPTION TYPE ZCX_DUE
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGID
*                                MSGNO = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGNO )
*              MSGTY  = 'E'
*              MSGNO  = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGNO
*              MSGID  = ZCX_DUE=>ZCX_OBG_INF_TP_EXPORTACAO-MSGID.
*        ENDIF.

        IF ( me->at_due-regio IS NOT INITIAL ).
          SELECT SINGLE *
            FROM t005s INTO @DATA(_wl_t005s)
           WHERE land1 EQ @me->at_due-land1
             AND bland EQ @me->at_due-regio.

          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_regio_invalid-msgid
                                  msgno = zcx_due=>zcx_obg_inf_regio_invalid-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_regio_invalid-msgno
                msgid  = zcx_due=>zcx_obg_inf_regio_invalid-msgid.
          ENDIF.
        ENDIF.

        IF me->at_due-tp_cod_local_despacho IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_tp_loc_despacho-msgid
                                msgno = zcx_due=>zcx_obg_inf_tp_loc_despacho-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_tp_loc_despacho-msgno
              msgid  = zcx_due=>zcx_obg_inf_tp_loc_despacho-msgid.
        ENDIF.

        IF me->at_due-codigo_urf_despacho IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_cod_urf_despacho-msgid
                                msgno = zcx_due=>zcx_obg_inf_cod_urf_despacho-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_cod_urf_despacho-msgno
              msgid  = zcx_due=>zcx_obg_inf_cod_urf_despacho-msgid.
        ENDIF.

        CASE me->at_due-tp_cod_local_despacho.
          WHEN '19' OR "Fora de Recinto Alfandegado - Domiciliar
               '22' .  "Fora de Recinto Alfandegado - Não Domiciliar

            IF me->at_due-cnpj_cpf_resp_loc_desp IS INITIAL.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_obj_inf_cnpj_cpf_resp_ld-msgid
                                    msgno = zcx_due=>zcx_obj_inf_cnpj_cpf_resp_ld-msgno )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_obj_inf_cnpj_cpf_resp_ld-msgno
                  msgid  = zcx_due=>zcx_obj_inf_cnpj_cpf_resp_ld-msgid.
            ENDIF.

            IF me->at_due-local_despacho_longitude IS INITIAL.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_obj_inf_longitude_loc_desp-msgid
                                    msgno = zcx_due=>zcx_obj_inf_longitude_loc_desp-msgno )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_obj_inf_longitude_loc_desp-msgno
                  msgid  = zcx_due=>zcx_obj_inf_longitude_loc_desp-msgid.
            ENDIF.

            IF me->at_due-local_despacho_latitude IS INITIAL.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_obj_inf_latitude_loc_desp-msgid
                                    msgno = zcx_due=>zcx_obj_inf_latitude_loc_desp-msgno )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_obj_inf_latitude_loc_desp-msgno
                  msgid  = zcx_due=>zcx_obj_inf_latitude_loc_desp-msgid.
            ENDIF.


            IF me->at_due-local_despacho_end  IS INITIAL.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_obj_inf_end_loc_desp-msgid
                                    msgno = zcx_due=>zcx_obj_inf_end_loc_desp-msgno )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_obj_inf_end_loc_desp-msgno
                  msgid  = zcx_due=>zcx_obj_inf_end_loc_desp-msgid.
            ENDIF.

          WHEN '281'.  "Recinto Alfandegado
            IF me->at_due-codigo_ra_despacho IS INITIAL.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_cod_ra_despacho-msgid
                                    msgno = zcx_due=>zcx_obg_inf_cod_ra_despacho-msgno )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_obg_inf_cod_ra_despacho-msgno
                  msgid  = zcx_due=>zcx_obg_inf_cod_ra_despacho-msgid.
            ENDIF.
        ENDCASE.

*        IF ME->AT_DUE-BRANCH IS INITIAL.
*          RAISE EXCEPTION TYPE ZCX_DUE
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_DUE=>ZCX_OBG_INF_BRANCH-MSGID
*                                MSGNO = ZCX_DUE=>ZCX_OBG_INF_BRANCH-MSGNO )
*              MSGTY  = 'E'
*              MSGNO  = ZCX_DUE=>ZCX_OBG_INF_BRANCH-MSGNO
*              MSGID  = ZCX_DUE=>ZCX_OBG_INF_BRANCH-MSGID.
*        ENDIF.

        IF me->at_due-forma_exportacao IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_forma_exportacao-msgid
                                msgno = zcx_due=>zcx_obg_inf_forma_exportacao-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_forma_exportacao-msgno
              msgid  = zcx_due=>zcx_obg_inf_forma_exportacao-msgid.
        ENDIF.

        IF ( me->at_due-situacao_especial IS INITIAL ) AND (  me->at_due-emb_container EQ abap_false ) .
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_sit_especial-msgid
                                msgno = zcx_due=>zcx_obg_inf_sit_especial-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_sit_especial-msgno
              msgid  = zcx_due=>zcx_obg_inf_sit_especial-msgid.
        ENDIF.

        IF me->at_due-moeda_cambio IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_moeda_cambio-msgid
                                msgno = zcx_due=>zcx_obg_inf_moeda_cambio-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_moeda_cambio-msgno
              msgid  = zcx_due=>zcx_obg_inf_moeda_cambio-msgid.
        ENDIF.

        IF ( me->at_due-id_due_ref IS NOT INITIAL ) AND ( me->at_due-motivo IS INITIAL ).
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_motivo-msgid
                                msgno = zcx_due=>zcx_obg_inf_motivo-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_motivo-msgno
              msgid  = zcx_due=>zcx_obg_inf_motivo-msgid.
        ENDIF.

        IF me->at_due-cnpj_declarante IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_cnpj_declarante-msgid
                                msgno = zcx_due=>zcx_obg_inf_cnpj_declarante-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_cnpj_declarante-msgno
              msgid  = zcx_due=>zcx_obg_inf_cnpj_declarante-msgid.
        ENDIF.

        IF me->at_due-codigo_urf_embarque IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_cod_urf_embarque-msgid
                                msgno = zcx_due=>zcx_obg_inf_cod_urf_embarque-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_cod_urf_embarque-msgno
              msgid  = zcx_due=>zcx_obg_inf_cod_urf_embarque-msgid.
        ENDIF.

        IF me->at_due-codigo_ra_embarque IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_cod_ra_embarque-msgid
                                msgno = zcx_due=>zcx_obg_inf_cod_ra_embarque-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_cod_ra_embarque-msgno
              msgid  = zcx_due=>zcx_obg_inf_cod_ra_embarque-msgid.
        ENDIF.

        IF me->at_itens[] IS INITIAL.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_itens-msgid
                                msgno = zcx_due=>zcx_obg_inf_itens-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_obg_inf_itens-msgno
              msgid  = zcx_due=>zcx_obg_inf_itens-msgid.
        ENDIF.

        IF ( lines( me->at_itens[] ) > 1  ) AND
           ( me->at_due-tp_due = '1'      ). "Sem NF-e
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_n_itens_due_sem_nfe-msgid
                                msgno = zcx_due=>zcx_n_itens_due_sem_nfe-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_n_itens_due_sem_nfe-msgno
              msgid  = zcx_due=>zcx_n_itens_due_sem_nfe-msgid.
        ENDIF.

        IF ( me->at_due-tp_due = '2' ). "Com NF-e.
          IF ( me->at_due-id_due_ref IS NOT INITIAL ).

            SELECT SINGLE *
              FROM zsdt0170 INTO @DATA(_wl_0170_ref)
             WHERE id_due EQ @me->at_due-id_due_ref.

            IF sy-subrc NE 0.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_registro_due_not_found-msgid
                                    msgno = zcx_due=>zcx_registro_due_not_found-msgno
                                    attr1 = CONV #( me->at_due-id_due_ref )
                                    )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_registro_due_not_found-msgno
                  msgid  = zcx_due=>zcx_registro_due_not_found-msgid
                  msgv1  = CONV #( me->at_due-id_due_ref ).
            ENDIF.

            "Determinar
            me->at_due-dt_registro_portal = _wl_0170_ref-dt_registro_portal.
            me->at_due-chave_acesso       = _wl_0170_ref-chave_acesso.

            IF _wl_0170_ref-status NE '1'.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_due_not_autorizada-msgid
                                    msgno = zcx_due=>zcx_due_not_autorizada-msgno
                                    attr1 = CONV #( me->at_due-id_due_ref ) )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_due_not_autorizada-msgno
                  msgid  = zcx_due=>zcx_due_not_autorizada-msgid
                  msgv1  = CONV #( me->at_due-id_due_ref ).
            ENDIF.

            IF _wl_0170_ref-lib_leitura_opus IS INITIAL.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_registro_not_lib_opus-msgid
                                    msgno = zcx_due=>zcx_registro_not_lib_opus-msgno
                                    attr1 = CONV #( me->at_due-id_due_ref ) )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_registro_not_lib_opus-msgno
                  msgid  = zcx_due=>zcx_registro_not_lib_opus-msgid
                  msgv1  = CONV #( me->at_due-id_due_ref ).
            ENDIF.
          ENDIF.
        ENDIF.

        IF ( me->at_due-lcto_avulso  EQ abap_true ). "Lançamentos de DU-e Registradas pelo Despachante

          IF ( me->at_due-numero_due IS INITIAL   ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_numero_due-msgid
                                  msgno = zcx_due=>zcx_obg_inf_numero_due-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_numero_due-msgno
                msgid  = zcx_due=>zcx_obg_inf_numero_due-msgid.
          ENDIF.

          IF ( me->at_due-numero_ruc IS INITIAL   ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_numero_ruc-msgid
                                  msgno = zcx_due=>zcx_obg_inf_numero_ruc-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_numero_ruc-msgno
                msgid  = zcx_due=>zcx_obg_inf_numero_ruc-msgid.
          ENDIF.

          IF ( me->at_due-chave_acesso IS INITIAL   ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_chave_acesso-msgid
                                  msgno = zcx_due=>zcx_obg_inf_chave_acesso-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_chave_acesso-msgno
                msgid  = zcx_due=>zcx_obg_inf_chave_acesso-msgid.
          ENDIF.

          IF ( me->at_due-dt_registro_portal IS INITIAL ) AND ( me->at_due-emb_container EQ abap_true ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_dt_reg_portal-msgid
                                  msgno = zcx_due=>zcx_obg_inf_dt_reg_portal-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_dt_reg_portal-msgno
                msgid  = zcx_due=>zcx_obg_inf_dt_reg_portal-msgid.
          ENDIF.

          SELECT SINGLE *
            FROM zsdt0170 INTO @DATA(lwa_zst0170_exists)
           WHERE numero_due   EQ @me->at_due-numero_due
             AND tp_due       EQ @me->at_due-tp_due
             AND lcto_avulso  EQ @abap_true
             AND loekz        EQ @abap_false
             AND id_due       NE @me->at_due-id_due.

          IF sy-subrc EQ 0.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                  msgno = zcx_due=>zcx_erro_geral-msgno
                                  attr1 = |Já existe um registro para a DU-e { me->at_due-numero_due } !|
                                  attr2 = |Id: { lwa_zst0170_exists-id_due } |
                                  )
                msgid  = zcx_due=>zcx_erro_geral-msgid
                msgno  = zcx_due=>zcx_erro_geral-msgno
                msgv1  = |Já existe um registro para a DU-e { me->at_due-numero_due } !|
                msgv2  = |Id: { lwa_zst0170_exists-id_due } |
                msgty  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM zsdt0170 INTO lwa_zst0170_exists
           WHERE numero_due   EQ me->at_due-numero_ruc
             AND tp_due       EQ me->at_due-tp_due
             AND lcto_avulso  EQ abap_true
             AND loekz        EQ abap_false
             AND id_due       NE me->at_due-id_due.

          IF sy-subrc EQ 0.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                  msgno = zcx_due=>zcx_erro_geral-msgno
                                  attr1 = |Já existe um registro para a RUC { me->at_due-numero_ruc } !|
                                  attr2 = |Id: { lwa_zst0170_exists-id_due } |
                                  )
                msgid  = zcx_due=>zcx_erro_geral-msgid
                msgno  = zcx_due=>zcx_erro_geral-msgno
                msgv1  = |Já existe um registro para a RUC { me->at_due-numero_ruc } !|
                msgv2  = |Id: { lwa_zst0170_exists-id_due } |
                msgty  = 'E'.
          ENDIF.

        ENDIF.

*----------------------------------------------------------------------------------------------------*
*       Validação Itens
*----------------------------------------------------------------------------------------------------*
        LOOP AT me->at_itens INTO DATA(wl_item).

          IF ( wl_item-id_due_item IS INITIAL ) OR ( wl_item-id_due_item <= 0 ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_id_due_item-msgid
                                  msgno = zcx_due=>zcx_obg_inf_id_due_item-msgno )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_id_due_item-msgno
                msgid  = zcx_due=>zcx_obg_inf_id_due_item-msgid.
          ENDIF.

          IF ( wl_item-fatura_tp_codigo IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_tp_cod_fat-msgid
                                  msgno = zcx_due=>zcx_obg_inf_item_tp_cod_fat-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_item_tp_cod_fat-msgno
                msgid  = zcx_due=>zcx_obg_inf_item_tp_cod_fat-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          IF ( wl_item-vlr_local_embarque IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_vlr_loc_emb-msgid
                                  msgno = zcx_due=>zcx_obg_inf_item_vlr_loc_emb-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_item_vlr_loc_emb-msgno
                msgid  = zcx_due=>zcx_obg_inf_item_vlr_loc_emb-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          IF ( wl_item-vlr_cond_venda IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_vlr_cond_vend-msgid
                                  msgno = zcx_due=>zcx_obg_inf_item_vlr_cond_vend-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_item_vlr_cond_vend-msgno
                msgid  = zcx_due=>zcx_obg_inf_item_vlr_cond_vend-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          CASE me->at_due-tp_due.
            WHEN '1'. "Sem NF-e =========================================================================|

              IF ( wl_item-exportador_cnpj     IS INITIAL ) OR
                 ( wl_item-exportador_nome     IS INITIAL ) OR
                 ( wl_item-exportador_country  IS INITIAL ) OR
                 ( wl_item-exportador_region   IS INITIAL ) OR
                 ( wl_item-exportador_cpl_end  IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_dados_exp-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_dados_exp-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_dados_exp-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_dados_exp-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-importador_nome     IS INITIAL ) OR
                 ( wl_item-importador_country  IS INITIAL ) OR
                 ( wl_item-importador_cpl_end  IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_dados_imp-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_dados_imp-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_dados_imp-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_dados_imp-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-fatura_motivo_dispensa_nf IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_motivo_disp-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_motivo_disp-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_motivo_disp-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_motivo_disp-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-desc_mercadoria IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_desc_merc-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_desc_merc-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_desc_merc-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_desc_merc-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-codigo_ncm IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_ncm-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_ncm-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_ncm-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_ncm-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-qtde_ue_exportada LE 0 ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_qtde_ue_exp-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_qtde_ue_exp-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_qtde_ue_exp-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_qtde_ue_exp-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-uc_exportada IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_uc_exp-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_uc_exp-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_uc_exp-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_uc_exp-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-qtde_uc_exportada IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_qtde_uc_exp-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_qtde_uc_exp-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_qtde_uc_exp-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_qtde_uc_exp-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              IF ( wl_item-matnr IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_material-msgid
                                      msgno = zcx_due=>zcx_obg_inf_item_material-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_obg_inf_item_material-msgno
                    msgid  = zcx_due=>zcx_obg_inf_item_material-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

              CLEAR: tg_marc[].
              SELECT *
                FROM marc AS a INTO TABLE tg_marc
               WHERE a~matnr EQ wl_item-matnr
                 AND a~steuc EQ wl_item-codigo_ncm
                 AND EXISTS ( SELECT b~centrov_1
                                FROM zsdt_depara_cen AS b
                               WHERE b~centrov_1 EQ a~werks ).

              IF tg_marc[] IS INITIAL.
                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_ncm_invalido_item-msgid
                                      msgno = zcx_due=>zcx_ncm_invalido_item-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_ncm_invalido_item-msgno
                    msgid  = zcx_due=>zcx_ncm_invalido_item-msgid
                    msgv1  = CONV #( wl_item-id_due_item ).
              ENDIF.

            WHEN '2'. "Com NF-e =============================================================================================|

              IF me->at_due-performance EQ abap_true.
                IF ( wl_item-importador_nome     IS INITIAL ) OR
                   ( wl_item-importador_country  IS INITIAL ) OR
                   ( wl_item-importador_cpl_end  IS INITIAL ).

                  RAISE EXCEPTION TYPE zcx_due
                    EXPORTING
                      textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_dados_imp-msgid
                                        msgno = zcx_due=>zcx_obg_inf_item_dados_imp-msgno
                                        attr1 = CONV #( wl_item-id_due_item )
                                        )
                      msgty  = 'E'
                      msgno  = zcx_due=>zcx_obg_inf_item_dados_imp-msgno
                      msgid  = zcx_due=>zcx_obg_inf_item_dados_imp-msgid
                      msgv1  = CONV #( wl_item-id_due_item ).

                ENDIF.
              ENDIF.

              DATA(_enq_lpco) = abap_false.
              CALL FUNCTION 'ZDUE_CHECK_ENQUADRAMENTO_LPCO'
                EXPORTING
                  i_codigo_enquadramento = wl_item-codigo_enquadramento
                IMPORTING
                  e_enq_lpco             = _enq_lpco.

              IF _enq_lpco EQ abap_true.
                READ TABLE me->at_itens_lpco INTO DATA(_wl_item_lpco) WITH KEY id_due_item = wl_item-id_due_item.
                IF sy-subrc NE 0.

                  RAISE EXCEPTION TYPE zcx_due
                    EXPORTING
                      textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_lpco_item-msgid
                                        msgno = zcx_due=>zcx_obg_inf_lpco_item-msgno
                                        attr1 = CONV #( wl_item-id_due_item )
                                        )
                      msgty  = 'E'
                      msgno  = zcx_due=>zcx_obg_inf_lpco_item-msgno
                      msgid  = zcx_due=>zcx_obg_inf_lpco_item-msgid
                      msgv1  = CONV #( wl_item-id_due_item ).

                ENDIF.
              ENDIF.

          ENDCASE.

          IF ( wl_item-codigo_cond_venda IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_cod_cond_vda-msgid
                                  msgno = zcx_due=>zcx_obg_inf_item_cod_cond_vda-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_item_cod_cond_vda-msgno
                msgid  = zcx_due=>zcx_obg_inf_item_cod_cond_vda-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          IF ( wl_item-peso_liq_total IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_peso_liq_tot-msgid
                                  msgno = zcx_due=>zcx_obg_inf_item_peso_liq_tot-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_item_peso_liq_tot-msgno
                msgid  = zcx_due=>zcx_obg_inf_item_peso_liq_tot-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          IF ( wl_item-preco_ton <= 0 ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_preco_ton-msgid
                                  msgno = zcx_due=>zcx_obg_inf_preco_ton-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_preco_ton-msgno
                msgid  = zcx_due=>zcx_obg_inf_preco_ton-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          v_vlr_loc_emb_calc = ( wl_item-peso_liq_total / 1000 ) * wl_item-preco_ton .

*          Margem de liberação no valor de 1 dolar pra mais e para menos
          CLEAR v_vlr_margem.
          v_vlr_margem_positivo = 1.
          v_vlr_margem_negativo = v_vlr_margem_positivo * -1.

          v_vlr_margem = v_vlr_loc_emb_calc - wl_item-vlr_local_embarque.

          IF NOT v_vlr_margem BETWEEN v_vlr_margem_negativo AND v_vlr_margem_positivo.
*          IF v_vlr_loc_emb_calc NE wl_item-vlr_local_embarque.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_preco_ton_incorreto-msgid
                                  msgno = zcx_due=>zcx_preco_ton_incorreto-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_preco_ton_incorreto-msgno
                msgid  = zcx_due=>zcx_preco_ton_incorreto-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          IF ( wl_item-codigo_enquadramento IS INITIAL ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_cod_enq-msgid
                                  msgno = zcx_due=>zcx_obg_inf_item_cod_enq-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_item_cod_enq-msgno
                msgid  = zcx_due=>zcx_obg_inf_item_cod_enq-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          "----------------------------------------------------------------------*
          " Validações Paises Destino
          "----------------------------------------------------------------------*

          READ TABLE me->at_itens_paises_destino INTO DATA(_wl_pais_destino) WITH KEY id_due_item = wl_item-id_due_item.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_paises_destino-msgid
                                  msgno = zcx_due=>zcx_obg_inf_paises_destino-msgno
                                  attr1 = CONV #( wl_item-id_due_item ) )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_obg_inf_paises_destino-msgno
                msgid  = zcx_due=>zcx_obg_inf_paises_destino-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          CLEAR: v_qtde_ue_exportada, tg_paises_aux[], v_peso_liq_total_pais.

          DATA(_contem_pais_europeu) = abap_false.  "WBARBOSA 23102024 - US-153330
          LOOP AT me->at_itens_paises_destino INTO _wl_pais_destino WHERE id_due_item = wl_item-id_due_item.
            SELECT SINGLE *
              FROM t005 INTO @DATA(_wl_t005)
             WHERE land1 EQ @_wl_pais_destino-destino_country.

            IF sy-subrc NE 0.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_pais_invalido_item-msgid
                                    msgno = zcx_due=>zcx_pais_invalido_item-msgno
                                    attr1 = CONV #( _wl_pais_destino-destino_country )
                                    attr2 = CONV #( wl_item-id_due_item )
                                    )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_pais_invalido_item-msgno
                  msgid  = zcx_due=>zcx_pais_invalido_item-msgid
                  msgv1  = CONV #( _wl_pais_destino-destino_country )
                  msgv2  = CONV #( wl_item-id_due_item ).
            ENDIF.

            "WBARBOSA 23102024 - US-153330 --->>>
            SELECT COUNT(*)
               FROM zpais
              WHERE land1      EQ _wl_pais_destino-destino_country
                AND continente EQ zcl_eudr_utils=>lc_europa.
            IF sy-subrc IS INITIAL.
              _contem_pais_europeu = abap_true.
            ENDIF.
            "WBARBOSA 23102024 - US-153330 <<<---

            IF ( _wl_pais_destino-destino_qtde_ue_exportada LE 0 ) OR ( _wl_pais_destino-ue_exportada IS INITIAL ).
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_pais_qtde_item_not_found-msgid
                                    msgno = zcx_due=>zcx_pais_qtde_item_not_found-msgno
                                    attr1 = CONV #( _wl_pais_destino-destino_country )
                                    attr2 = CONV #( wl_item-id_due_item )
                                    )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_pais_qtde_item_not_found-msgno
                  msgid  = zcx_due=>zcx_pais_qtde_item_not_found-msgid
                  msgv1  = CONV #( _wl_pais_destino-destino_country )
                  msgv2  = CONV #( wl_item-id_due_item ).
            ENDIF.

            "Validar se Unid. Estatistica do Item confere com as dos Pais
            IF ( me->at_due-tp_due = '1' ) AND ( wl_item-ue_exportada NE _wl_pais_destino-ue_exportada ).
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_val_ue_item_pais-msgid
                                    msgno = zcx_due=>zcx_val_ue_item_pais-msgno
                                    attr1 = CONV #( wl_item-id_due_item )
                                    attr2 = CONV #( wl_item-ue_exportada )
                                    attr3 = CONV #( _wl_pais_destino-ue_exportada )
                                    )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_val_ue_item_pais-msgno
                  msgid  = zcx_due=>zcx_val_ue_item_pais-msgid
                  msgv1  = CONV #( wl_item-id_due_item )
                  msgv2  = CONV #( wl_item-ue_exportada )
                  msgv3  = CONV #( _wl_pais_destino-ue_exportada ).
            ENDIF.

            ADD _wl_pais_destino-destino_qtde_ue_exportada TO v_qtde_ue_exportada.
            ADD _wl_pais_destino-peso_liq_total            TO v_peso_liq_total_pais.

            APPEND _wl_pais_destino TO tg_paises_aux.

          ENDLOOP.

          "WBARBOSA 23102024 - US-153330 --->>>
          IF  me->at_due-tp_due = '2'.

            SELECT SINGLE *
              FROM tvarvc INTO @DATA(lwa_eudr_piloto)
             WHERE name EQ 'EUDR_MODO_PILOTO'
               AND low  EQ @abap_true.

            IF sy-subrc NE 0.

              IF _wl_0170_ref-eudr EQ zcl_eudr_utils=>lc_n_eudr AND _contem_pais_europeu = abap_true.

                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                      msgno = zcx_due=>zcx_erro_geral-msgno
                                      attr1 = CONV #( |DU-e { me->at_due-id_due }| )
                                      attr2 = CONV #( |NÃO EUDR! Nao pode ser retificada| )
                                      attr3 = CONV #( |com país de continente Europeu| )
                                      )
                    msgty  = cl_abap_aab_utilities=>category_error
                    msgno  = zcx_due=>zcx_erro_geral-msgno
                    msgid  = zcx_due=>zcx_erro_geral-msgid
                    msgv1  = CONV #( |DU-e { me->at_due-id_due }| )
                    msgv2  = CONV #( |NÃO EUDR! Nao pode ser retificada| )
                    msgv3  = CONV #( |com país de continente Europeu| ).

              ENDIF.

              IF _wl_0170_ref-eudr EQ zcl_eudr_utils=>lc_s_eudr AND _contem_pais_europeu EQ abap_false AND _ciencia_embarque_nao_europeu EQ abap_false.

                CALL FUNCTION 'POPUP_TO_CONFIRM'
                  EXPORTING
                    titlebar              = 'Atenção!'
                    text_question         = 'DU-e com produtos EUDR não esta sendo ser retificada com país de destino Europeu! Deseja continuar com a retificação?'
                    text_button_1         = 'Sim'
                    text_button_2         = 'Não'
                    default_button        = '1'
                    display_cancel_button = ''
                  IMPORTING
                    answer                = var_answer
                  EXCEPTIONS
                    text_not_found        = 1
                    OTHERS                = 2.

                IF var_answer NE '1'.
                  RAISE EXCEPTION TYPE zcx_due
                    EXPORTING
                      textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                        msgno = zcx_due=>zcx_erro_geral-msgno
                                        attr1 = CONV #( |DU-e { me->at_due-id_due }| )
                                        attr2 = CONV #( |Processo Interrompido.| )
                                        )
                      msgty  = cl_abap_aab_utilities=>category_error
                      msgno  = zcx_due=>zcx_erro_geral-msgno
                      msgid  = zcx_due=>zcx_erro_geral-msgid
                      msgv1  = CONV #( |DU-e { me->at_due-id_due }| )
                      msgv2  = CONV #( |Processo Interrompido.| ).
                ELSE.
                  _ciencia_embarque_nao_europeu = abap_true.
                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.
          "WBARBOSA 23102024 - US-153330 <<<---


          "Validar Duplicidade Paises
          v_count = lines( tg_paises_aux ).
          SORT tg_paises_aux BY destino_country.
          DELETE ADJACENT DUPLICATES FROM tg_paises_aux COMPARING destino_country.

          IF v_count NE lines( tg_paises_aux ).
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_val_dupl_pais_item-msgid
                                  msgno = zcx_due=>zcx_val_dupl_pais_item-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_val_dupl_pais_item-msgno
                msgid  = zcx_due=>zcx_val_dupl_pais_item-msgid
                msgv1  = CONV #( wl_item-id_due_item ).
          ENDIF.

          "Validar se Quantidade Unid. Estatistica do Item confere com as dos Paises
          IF ( me->at_due-tp_due = '1' ) AND "Sem NF-e
             ( wl_item-qtde_ue_exportada NE v_qtde_ue_exportada ).

            v_qtde_c_01 = wl_item-qtde_ue_exportada.
            v_qtde_c_02 = v_qtde_ue_exportada.

            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_val_qtde_ue_item_pais-msgid
                                  msgno = zcx_due=>zcx_val_qtde_ue_item_pais-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  attr2 = CONV #( v_qtde_c_01 )
                                  attr3 = CONV #( v_qtde_c_02 )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_val_qtde_ue_item_pais-msgno
                msgid  = zcx_due=>zcx_val_qtde_ue_item_pais-msgid
                msgv1  = CONV #( wl_item-id_due_item )
                msgv2  = CONV #( v_qtde_c_01 )
                msgv3  = CONV #( v_qtde_c_02 ).
          ENDIF.

          IF ( wl_item-peso_liq_total NE v_peso_liq_total_pais ).

            v_qtde_c_01 = wl_item-peso_liq_total.
            v_qtde_c_02 = v_peso_liq_total_pais.

            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_val_peso_liq_item_pais-msgid
                                  msgno = zcx_due=>zcx_val_peso_liq_item_pais-msgno
                                  attr1 = CONV #( wl_item-id_due_item )
                                  attr2 = CONV #( v_qtde_c_01 )
                                  attr3 = CONV #( v_qtde_c_02 )
                                  )
                msgty  = 'E'
                msgno  = zcx_due=>zcx_val_peso_liq_item_pais-msgno
                msgid  = zcx_due=>zcx_val_peso_liq_item_pais-msgid
                msgv1  = CONV #( wl_item-id_due_item )
                msgv2  = CONV #( v_qtde_c_01 )
                msgv3  = CONV #( v_qtde_c_02 ).

          ENDIF.

          IF ( me->at_due-tp_due = '2' )."Com NF-e

            "----------------------------------------------------------------------*
            " Validações Faturas
            "----------------------------------------------------------------------*
            READ TABLE me->at_itens_faturas_ref INTO DATA(_wl_fatura_ref) WITH KEY id_due_item = wl_item-id_due_item.
            IF sy-subrc NE 0.
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_item_nf-msgid
                                    msgno = zcx_due=>zcx_obg_inf_item_nf-msgno
                                    attr1 = CONV #( wl_item-id_due_item ) )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_obg_inf_item_nf-msgno
                  msgid  = zcx_due=>zcx_obg_inf_item_nf-msgid
                  msgv1  = CONV #( wl_item-id_due_item ).
            ENDIF.

            CLEAR: v_peso_liq_total_fat_sem_cct, v_peso_liq_total_fat_com_cct.

            LOOP AT me->at_itens_faturas_ref INTO _wl_fatura_ref WHERE id_due_item = wl_item-id_due_item.
              IF _wl_fatura_ref-registro_cct EQ abap_true.
                ADD _wl_fatura_ref-peso_liq_total TO v_peso_liq_total_fat_com_cct.
              ELSE.
                ADD _wl_fatura_ref-peso_liq_total TO v_peso_liq_total_fat_sem_cct.
              ENDIF.
            ENDLOOP.

            SELECT SINGLE * FROM zfit258 WHERE id_due = @me->at_due-numero_due INTO @DATA(wa_zfit258). "147506 CS2024000657 Salvar Due para retificação mesmo sem saldo em CTT ZMEMO00 - ZSDT0142 - PSA

            IF sy-subrc <> 0. "147506 CS2024000657 Salvar Due para retificação mesmo sem saldo em CTT ZMEMO00 - ZSDT0142 - PSA
              "Faturas Com CCT
              IF ( wl_item-peso_liq_total NE v_peso_liq_total_fat_com_cct ).
                v_qtde_c_01 = wl_item-peso_liq_total.
                v_qtde_c_02 = v_peso_liq_total_fat_com_cct.

                RAISE EXCEPTION TYPE zcx_due
                  EXPORTING
                    textid = VALUE #( msgid = zcx_due=>zcx_val_peso_liq_item_fat-msgid
                                      msgno = zcx_due=>zcx_val_peso_liq_item_fat-msgno
                                      attr1 = CONV #( wl_item-id_due_item )
                                      attr2 = CONV #( v_qtde_c_01 )
                                      attr3 = CONV #( v_qtde_c_02 )
                                      )
                    msgty  = 'E'
                    msgno  = zcx_due=>zcx_val_peso_liq_item_fat-msgno
                    msgid  = zcx_due=>zcx_val_peso_liq_item_fat-msgid
                    msgv1  = CONV #( wl_item-id_due_item )
                    msgv2  = CONV #( v_qtde_c_01 )
                    msgv3  = CONV #( v_qtde_c_02 ).
              ENDIF.
            ENDIF.

*** Stefanini - IR240290 - 25/06/2025 - FINC - Início de Alteração
*            "Faturas Sem CCT
*            IF ( wl_item-peso_liq_total NE v_peso_liq_total_fat_sem_cct AND me->at_due-tp_exportacao = 'I' ). "Exportação Indireta sempre comparar notas sem CCT
*              v_qtde_c_01 = wl_item-peso_liq_total.
*              v_qtde_c_02 = v_peso_liq_total_fat_sem_cct.
*
*              RAISE EXCEPTION TYPE zcx_due
*                EXPORTING
*                  textid = VALUE #( msgid = zcx_due=>zcx_val_peso_liq_item_fat_scct-msgid
*                                    msgno = zcx_due=>zcx_val_peso_liq_item_fat_scct-msgno
*                                    attr1 = CONV #( wl_item-id_due_item )
*                                    attr2 = CONV #( v_qtde_c_01 )
*                                    attr3 = CONV #( v_qtde_c_02 )
*                                    )
*                  msgty  = 'E'
*                  msgno  = zcx_due=>zcx_val_peso_liq_item_fat_scct-msgno
*                  msgid  = zcx_due=>zcx_val_peso_liq_item_fat_scct-msgid
*                  msgv1  = CONV #( wl_item-id_due_item )
*                  msgv2  = CONV #( v_qtde_c_01 )
*                  msgv3  = CONV #( v_qtde_c_02 ).
*            ENDIF.
*** Stefanini - IR240290 - 25/06/2025 - FINC - Fim de Alteração

            IF ( v_peso_liq_total_fat_sem_cct > 0 AND me->at_due-tp_exportacao = 'D' ). "Não pode existir Faturas sem CCT na Exportação Direta
              RAISE EXCEPTION TYPE zcx_due
                EXPORTING
                  textid = VALUE #( msgid = zcx_due=>zcx_val_fat_sem_cct_exp_dir-msgid
                                    msgno = zcx_due=>zcx_val_fat_sem_cct_exp_dir-msgno
                                    )
                  msgty  = 'E'
                  msgno  = zcx_due=>zcx_val_fat_sem_cct_exp_dir-msgno
                  msgid  = zcx_due=>zcx_val_fat_sem_cct_exp_dir-msgid.
            ENDIF.

          ENDIF.
        ENDLOOP. "LOOP AT ME->AT_ITENS INTO DATA(WL_ITEM).

        CLEAR: tg_itens_aux[].
        tg_itens_aux[] = me->at_itens[].
        SORT tg_itens_aux BY id_due_item.
        DELETE ADJACENT DUPLICATES FROM tg_itens_aux COMPARING id_due_item.

        IF lines( tg_itens_aux[] ) NE lines( me->at_itens[] ).
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_found_id_due_item_dupl-msgid
                                msgno = zcx_due=>zcx_found_id_due_item_dupl-msgno )
              msgty  = 'E'
              msgno  = zcx_due=>zcx_found_id_due_item_dupl-msgno
              msgid  = zcx_due=>zcx_found_id_due_item_dupl-msgid.
        ENDIF.

    ENDCASE.

    IF ( me->at_due-performance         EQ abap_true ) AND
       ( me->at_due-tp_due              EQ '2'       ) AND "Com NF-e
       ( me->at_due-preenchimento_auto  EQ abap_true ).    "Preenchimento Automatico com base nas NFs de Exportação

      me->valida_preenchimento_auto( ).

    ENDIF.

    r_validou = abap_true.

  ENDMETHOD.


  METHOD zif_due~valida_preenchimento_auto.

    CONSTANTS: c_vda TYPE c LENGTH 3 VALUE 'VDA',
               c_rfl TYPE c LENGTH 3 VALUE 'RFL'.

    TYPES: BEGIN OF ty_nfs_proc_ruc_sap,
             chave            TYPE zde_chave_doc_e,
             docnum           TYPE j_1bnfdoc-docnum,
             numero_ruc       TYPE zsdt0053-numero_ruc,
             codigo_ra        TYPE zsdt0053-codigo_ra,
             id_nomeacao_tran TYPE zsdt0053-id_nomeacao_tran,
             matnr            TYPE j_1bnflin-matnr,
             kunnr            TYPE kna1-kunnr,
             menge            TYPE j_1bnflin-menge,
             chave_exp        TYPE zde_chave_doc_e,
             tipo             TYPE c LENGTH 3, "VDA - Venda  / RFL - Formação de Lote
           END OF ty_nfs_proc_ruc_sap.

    DATA: lva_number_int1 TYPE char255,
          lva_number_int2 TYPE char255.

    DATA: lit_log_proc TYPE zde_due_log_proc_t.

    DATA: lit_nfs_proc_ruc_sap TYPE TABLE OF ty_nfs_proc_ruc_sap,
          lit_nfs_proc_ruc_xml TYPE TABLE OF ty_nfs_proc_ruc_sap,
          lit_zsdt_retlote     TYPE TABLE OF zsdt_retlote.

    DATA: it_zsdt0053 TYPE TABLE OF zsdt0053.

    DATA: lva_tot_qtde_rfl_sap   TYPE j_1bnflin-menge,
          lva_tot_qtde_venda_sap TYPE j_1bnflin-menge,
          lva_tot_qtde_exp_xml   TYPE j_1bnflin-menge,
          lva_quantidade_tmp     TYPE j_1bnflin-menge,
          lva_docnum             TYPE j_1bnflin-docnum,
          lva_chave_nfe          TYPE zde_chave_doc_e.

    IF ( me->at_due-numero_due IS INITIAL   ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_numero_due-msgid
                            msgno = zcx_due=>zcx_obg_inf_numero_due-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_obg_inf_numero_due-msgno
          msgid  = zcx_due=>zcx_obg_inf_numero_due-msgid.
    ENDIF.

    IF ( me->at_due-numero_ruc IS INITIAL   ).
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_obg_inf_numero_ruc-msgid
                            msgno = zcx_due=>zcx_obg_inf_numero_ruc-msgno )
          msgty  = 'E'
          msgno  = zcx_due=>zcx_obg_inf_numero_ruc-msgno
          msgid  = zcx_due=>zcx_obg_inf_numero_ruc-msgid.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------*
*   Identificar Notas de Venda e Formação de Lote no SAP para a RUC informada
*-----------------------------------------------------------------------------------------------------------*
    CLEAR: it_zsdt0053[], lit_nfs_proc_ruc_sap[], lva_tot_qtde_rfl_sap , lva_tot_qtde_venda_sap.

    SELECT *
      FROM zsdt0053 INTO TABLE it_zsdt0053
     WHERE numero_ruc EQ me->at_due-numero_ruc.


    LOOP AT it_zsdt0053 INTO DATA(lwa_zsdt0053).

      CLEAR: lit_zsdt_retlote[].

      IF lwa_zsdt0053-remessa_exp IS INITIAL.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Sol. OV { lwa_zsdt0053-nro_sol_ov } item: { lwa_zsdt0053-posnr }|
                              attr2 = |sem remessa gerada! Transação ZSDT0062!|
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Sol. OV { lwa_zsdt0053-nro_sol_ov } item: { lwa_zsdt0053-posnr }|
            msgv2  = |sem remessa gerada! Transação ZSDT0062!|
            msgty  = 'E'.
      ENDIF.

      SELECT SINGLE *
        FROM vbrp INTO @DATA(lwa_vbrp)
       WHERE vgbel EQ @lwa_zsdt0053-remessa_exp AND draft = @space .

      SELECT SINGLE a~vbeln
        FROM vbfa AS a INTO @DATA(vl_vbeln_fat)
       WHERE a~vbelv    EQ @lwa_zsdt0053-remessa_exp
         AND a~vbtyp_n  EQ 'M'
         AND a~vbtyp_v  EQ 'J'
         AND NOT EXISTS ( SELECT *
                            FROM vbfa AS b
                           WHERE b~vbelv   = a~vbeln
                             AND b~vbtyp_n = 'N' ). "ESTORNO

      IF ( sy-subrc NE 0 ) OR ( vl_vbeln_fat IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Não encontrada fatura|
                              attr2 = |para a remesssa { lwa_zsdt0053-remessa_exp } !|
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Não encontrada fatura|
            msgv2  = |para a remesssa { lwa_zsdt0053-remessa_exp } !|
            msgty  = 'E'.

      ENDIF.

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(lwa_lin_venda)
       WHERE refkey EQ @vl_vbeln_fat.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Não encontrada item documento fiscal|
                              attr2 = |fatura { vl_vbeln_fat } !|
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Não encontrada item documento fiscal|
            msgv2  = |fatura { vl_vbeln_fat } !|
            msgty  = 'E'.
      ENDIF.

      CASE lwa_lin_venda-meins.
        WHEN 'KG'.
          lva_quantidade_tmp = lwa_lin_venda-menge.
        WHEN 'TON' OR 'TO'.
          lva_quantidade_tmp = lwa_lin_venda-menge * 1000.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                msgno = zcx_due=>zcx_erro_geral-msgno
                                attr1 = |Unidade { lwa_lin_venda-meins } do documento: { lwa_lin_venda-docnum } |
                                attr2 = | é desconhecida! |
                                )
              msgid  = zcx_due=>zcx_erro_geral-msgid
              msgno  = zcx_due=>zcx_erro_geral-msgno
              msgv1  = |Unidade { lwa_lin_venda-meins } do documento: { lwa_lin_venda-docnum } |
              msgv2  = | é desconhecida! |
              msgty  = 'E'.
      ENDCASE.

      ADD lva_quantidade_tmp TO lva_tot_qtde_venda_sap.

      SELECT SINGLE *
        FROM j_1bnfe_active INTO @DATA(lwa_active_venda)
       WHERE docnum EQ @lwa_lin_venda-docnum.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Não encontrado cabeçalho documento fiscal |
                              attr2 = |{ lwa_lin_venda-docnum } !|
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Não encontrado cabeçalho documento fiscal |
            msgv2  = | { lwa_lin_venda-docnum } !|
            msgty  = 'E'.
      ENDIF.

      IF NOT ( ( lwa_active_venda-docsta = '1' ) AND ( lwa_active_venda-scssta NE '2' ) AND ( lwa_active_venda-cancel = abap_false ) ).
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Documento fiscal { lwa_lin_venda-docnum } não está autorizado! |
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Documento fiscal { lwa_lin_venda-docnum } não está autorizado! |
            msgty  = 'E'.
      ENDIF.

      IF lwa_zsdt0053-docnum_rt IS INITIAL.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Remessa { lwa_zsdt0053-remessa_exp } sem documento retorno! |
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Remessa { lwa_zsdt0053-remessa_exp } sem documento retorno! |
            msgty  = 'E'.
      ENDIF.

      SELECT *
        FROM zsdt_retlote INTO TABLE lit_zsdt_retlote
       WHERE docnum_ret  = lwa_zsdt0053-docnum_rt.

      IF lit_zsdt_retlote[] IS INITIAL.
        RAISE EXCEPTION TYPE zcx_due
          EXPORTING
            textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                              msgno = zcx_due=>zcx_erro_geral-msgno
                              attr1 = |Retorno { lwa_zsdt0053-docnum_rt } sem formação de lote! |
                              )
            msgid  = zcx_due=>zcx_erro_geral-msgid
            msgno  = zcx_due=>zcx_erro_geral-msgno
            msgv1  = |Retorno { lwa_zsdt0053-docnum_rt } sem formação de lote! |
            msgty  = 'E'.
      ENDIF.

      CONCATENATE lwa_active_venda-regio   "Região do emissor NF-e
                  lwa_active_venda-nfyear  "Ano da data do documento da NF-e
                  lwa_active_venda-nfmonth "Mês da data do documento da NF-e
                  lwa_active_venda-stcd1   "Nº CNPJ do emissor da NF-e
                  lwa_active_venda-model   "Modelo da nota fiscal
                  lwa_active_venda-serie   "SERIE
                  lwa_active_venda-nfnum9  "Nº NF-e de nove posições
                  lwa_active_venda-docnum9 "NF-e: nº aleatório
                  lwa_active_venda-cdv     "Dígito controle p/chave de acesso NF-e
             INTO DATA(lva_chave_nfe_venda).

      APPEND VALUE #( chave                = lva_chave_nfe_venda
                      docnum               = lwa_active_venda-docnum
                      numero_ruc           = lwa_zsdt0053-numero_ruc
                      codigo_ra            = lwa_zsdt0053-codigo_ra
                      id_nomeacao_tran     = lwa_zsdt0053-id_nomeacao_tran
                      matnr                = lwa_zsdt0053-matnr
                      kunnr                = lwa_active_venda-parid
                      tipo                 = c_vda  ) TO lit_nfs_proc_ruc_sap.

      LOOP AT lit_zsdt_retlote INTO DATA(lwa_zsdt_retlote).

        SELECT SINGLE *
          FROM j_1bnfe_active INTO @DATA(lwa_active_rfl)
         WHERE docnum EQ @lwa_zsdt_retlote-docnum.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                msgno = zcx_due=>zcx_erro_geral-msgno
                                attr1 = |Não encontrado cabeçalho documento fiscal |
                                attr2 = |{ lwa_zsdt_retlote-docnum } !|
                                )
              msgid  = zcx_due=>zcx_erro_geral-msgid
              msgno  = zcx_due=>zcx_erro_geral-msgno
              msgv1  = |Não encontrado cabeçalho documento fiscal |
              msgv2  = | { lwa_zsdt_retlote-docnum } !|
              msgty  = 'E'.
        ENDIF.

        IF NOT ( ( lwa_active_rfl-docsta = '1' ) AND ( lwa_active_rfl-scssta NE '2' ) AND ( lwa_active_rfl-cancel = abap_false ) ).
          RAISE EXCEPTION TYPE zcx_due
            EXPORTING
              textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                msgno = zcx_due=>zcx_erro_geral-msgno
                                attr1 = |Documento fiscal { lwa_active_rfl-docnum } não está autorizado! |
                                )
              msgid  = zcx_due=>zcx_erro_geral-msgid
              msgno  = zcx_due=>zcx_erro_geral-msgno
              msgv1  = |Documento fiscal { lwa_active_rfl-docnum } não está autorizado! |
              msgty  = 'E'.
        ENDIF.

        ADD lwa_zsdt_retlote-quant_vinc TO lva_tot_qtde_rfl_sap.

        CONCATENATE lwa_active_rfl-regio   "Região do emissor NF-e
                    lwa_active_rfl-nfyear  "Ano da data do documento da NF-e
                    lwa_active_rfl-nfmonth "Mês da data do documento da NF-e
                    lwa_active_rfl-stcd1   "Nº CNPJ do emissor da NF-e
                    lwa_active_rfl-model   "Modelo da nota fiscal
                    lwa_active_rfl-serie   "SERIE
                    lwa_active_rfl-nfnum9  "Nº NF-e de nove posições
                    lwa_active_rfl-docnum9 "NF-e: nº aleatório
                    lwa_active_rfl-cdv     "Dígito controle p/chave de acesso NF-e
               INTO DATA(lva_chave_nfe_rfl).

        APPEND VALUE #( chave                = lva_chave_nfe_rfl
                        docnum               = lwa_active_rfl-docnum
                        numero_ruc           = lwa_zsdt0053-numero_ruc
                        codigo_ra            = lwa_zsdt0053-codigo_ra
                        id_nomeacao_tran     = lwa_zsdt0053-id_nomeacao_tran
                        matnr                = lwa_zsdt0053-matnr
                        kunnr                = lwa_active_rfl-parid
                        tipo                 = c_rfl ) TO lit_nfs_proc_ruc_sap.

      ENDLOOP.


    ENDLOOP.

*-----------------------------------------------------------------------------------------------------------*
*   Identificar Notas de Venda e Formação de Lote nos XMLs de Exportação
*-----------------------------------------------------------------------------------------------------------*
    CLEAR: lit_nfs_proc_ruc_xml[],  lva_tot_qtde_exp_xml.

    LOOP AT me->at_xmls_exportacao INTO DATA(lwa_xml_exportacao).

      LOOP AT lwa_xml_exportacao-nfeproc-nfe-infnfe-ide-nfref INTO DATA(lwa_nfref).
        APPEND VALUE #( chave     = lwa_nfref-refnfe
                        chave_exp = lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe ) TO lit_nfs_proc_ruc_xml.
      ENDLOOP.


      "Preencher Quantidade e Dados Material com base no Material da NF-e Referenciada no XML de exportação
      LOOP AT lwa_xml_exportacao-nfeproc-nfe-infnfe-det INTO DATA(lwa_det).

        CLEAR: lva_quantidade_tmp.

        TRANSLATE lwa_det-prod-ucom TO UPPER CASE.

        CASE lwa_det-prod-ucom.
          WHEN 'KG'.
            lva_quantidade_tmp = lwa_det-prod-qcom.
          WHEN 'TON' OR 'TO'.
            lva_quantidade_tmp = lwa_det-prod-qcom * 1000.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                  msgno = zcx_due=>zcx_erro_geral-msgno
                                  attr1 = |Unidade { lwa_det-prod-ucom } do item da NF-e: |
                                  attr2 = |{ lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe } é desconhecida! |
                                  )
                msgid  = zcx_due=>zcx_erro_geral-msgid
                msgno  = zcx_due=>zcx_erro_geral-msgno
                msgv1  = |Unidade { lwa_det-prod-ucom } do item da NF-e: |
                msgv2  = |{ lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe } é desconhecida! |
                msgty  = 'E'.
        ENDCASE.

        ADD lva_quantidade_tmp TO lva_tot_qtde_exp_xml.

        LOOP AT lwa_det-prod-detexport INTO DATA(lwa_detexport).

          IF strlen( lwa_detexport-exportind-chnfe ) NE 44. "NF-e Venda
            RAISE EXCEPTION TYPE zcx_due
              EXPORTING
                textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                                  msgno = zcx_due=>zcx_erro_geral-msgno
                                  attr1 = |NF-e { lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe } referencia |
                                  attr2 = | notas inválidas em detalhes Exportação do Item! |
                                  )
                msgid  = zcx_due=>zcx_erro_geral-msgid
                msgno  = zcx_due=>zcx_erro_geral-msgno
                msgv1  = |NF-e { lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe } referencia |
                msgv2  = | notas inválidas em detalhes Exportação do Item! |
                msgty  = 'E'.
          ENDIF.

          SELECT SINGLE *
            FROM kna1 INTO @DATA(lwa_kna1_emissor)
           WHERE stcd1 EQ @lwa_detexport-exportind-chnfe+6(14).

          IF ( sy-subrc NE 0 ) .
            MESSAGE s171(zdue) WITH lwa_detexport-exportind-chnfe INTO DATA(lwa_mensagem).
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
            CONTINUE.
          ENDIF.

          CHECK lwa_kna1_emissor-ktokd EQ 'ZCIC'. "Não validar notas emitidas pelo Teceiro.

          lva_chave_nfe = lwa_detexport-exportind-chnfe.

          CALL FUNCTION 'ZDUE_GET_DOC_FISCAL'
            EXPORTING
              i_chave   = lva_chave_nfe
              i_direct  = '2' "Direção
              i_propria = 'X' "Propria
            IMPORTING
              e_docnum  = lva_docnum.

          IF lva_docnum IS INITIAL.
            MESSAGE s172(zdue) WITH lwa_detexport-exportind-chnfe INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
            CONTINUE.
          ENDIF.

          SELECT SINGLE *
            FROM zsdt_retlote INTO lwa_zsdt_retlote
           WHERE docnum EQ lva_docnum.

          CHECK ( sy-subrc NE 0 ). "Formaçao de Lote deve ignorar..

          APPEND VALUE #( chave                = lwa_detexport-exportind-chnfe
                          menge                = lva_quantidade_tmp
                          chave_exp            = lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe
                          tipo                 = c_vda ) TO lit_nfs_proc_ruc_xml.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.


*-----------------------------------------------------------------------------------------------------------*
*   Cruzamento e Validação de Informações
*-----------------------------------------------------------------------------------------------------------*

    CLEAR: lit_log_proc[].

    "Verificar se todas as remessas e vendas vinculadas a RUC no SAP, estao mencionadas devidamente nos XMLs de Exportação fornecidos pelo Performado
    LOOP AT lit_nfs_proc_ruc_sap INTO DATA(lwa_nf_proc_ruc_sap).

      CASE lwa_nf_proc_ruc_sap-tipo.
        WHEN c_rfl. "Formação de Lote

          READ TABLE lit_nfs_proc_ruc_xml INTO DATA(lwa_nf_proc_ruc_xml) WITH KEY chave = lwa_nf_proc_ruc_sap-chave
                                                                                  tipo  = space. "REFERENCIA NAS TAGS IDE->NFREF->REFNFE
          IF sy-subrc NE 0.
            MESSAGE s167(zdue) WITH lwa_nf_proc_ruc_sap-chave INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
          ENDIF.

        WHEN c_vda. "Venda

          READ TABLE lit_nfs_proc_ruc_xml INTO lwa_nf_proc_ruc_xml WITH KEY chave = lwa_nf_proc_ruc_sap-chave
                                                                            tipo  = space.   "REFERENCIA NAS TAGS IDE->NFREF->REFNFE
          IF sy-subrc NE 0.
            MESSAGE s167(zdue) WITH lwa_nf_proc_ruc_sap-chave INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
          ENDIF.

          READ TABLE lit_nfs_proc_ruc_xml INTO lwa_nf_proc_ruc_xml WITH KEY chave = lwa_nf_proc_ruc_sap-chave
                                                                            tipo  = c_vda.   "REFERENCIA NAS TAGS DET->PROD->DETEXPORT->EXPORTIND
          IF sy-subrc NE 0.
            MESSAGE s167(zdue) WITH lwa_nf_proc_ruc_sap-chave INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
          ENDIF.

      ENDCASE.

      DATA(lva_found_vinc_due)    = abap_false.
      DATA(lva_found_vinc_nf_exp) = abap_false.


      LOOP AT me->at_itens_faturas_ref INTO DATA(lwa_nf_fatura_ref) WHERE id_fatura_ref = lwa_nf_proc_ruc_sap-chave.
        lva_found_vinc_due = abap_true.
        READ TABLE me->at_itens INTO DATA(lwa_item_due) WITH KEY id_due_item = lwa_nf_fatura_ref-id_due_item.
        IF ( sy-subrc EQ 0 ) AND ( lwa_item_due-fatura_id EQ lwa_nf_proc_ruc_xml-chave_exp ).
          lva_found_vinc_nf_exp = abap_true.
        ENDIF.
      ENDLOOP.

      IF lva_found_vinc_due EQ abap_false.
        MESSAGE s167(zdue) WITH lwa_nf_proc_ruc_sap-chave INTO lwa_mensagem.
        APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
      ENDIF.

      IF lva_found_vinc_nf_exp EQ abap_false.
        MESSAGE s170(zdue) WITH lwa_nf_proc_ruc_sap-chave INTO lwa_mensagem.
        APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
      ENDIF.


    ENDLOOP.

    "Verificar se todas as remessas e vendas vinculadas nos XMLs de Exportação do performado, estao vinculadas a RUC no SAP
    LOOP AT lit_nfs_proc_ruc_xml INTO lwa_nf_proc_ruc_xml.

      CASE lwa_nf_proc_ruc_xml-tipo.
        WHEN space. "REFERENCIA nas TAGS IDE->NFREF->REFNFE

          SELECT SINGLE *
            FROM kna1 INTO lwa_kna1_emissor
           WHERE stcd1 EQ lwa_nf_proc_ruc_xml-chave+6(14).

          IF ( sy-subrc NE 0 ) OR ( lwa_nf_proc_ruc_xml-chave IS INITIAL ) .
            MESSAGE s171(zdue) WITH lwa_nf_proc_ruc_xml-chave INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
          ENDIF.

          CHECK lwa_kna1_emissor-ktokd EQ 'ZCIC'. "Não validar notas emitidas pelo Teceiro.

          READ TABLE lit_nfs_proc_ruc_sap INTO lwa_nf_proc_ruc_sap WITH KEY chave = lwa_nf_proc_ruc_xml-chave.
          IF sy-subrc NE 0.
            MESSAGE s168(zdue) WITH lwa_nf_proc_ruc_xml-chave me->at_due-numero_ruc INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
          ENDIF.

        WHEN c_vda. "Venda

          READ TABLE lit_nfs_proc_ruc_sap INTO lwa_nf_proc_ruc_sap WITH KEY chave = lwa_nf_proc_ruc_xml-chave
                                                                            tipo  = c_vda.
          IF sy-subrc NE 0.
            MESSAGE s168(zdue) WITH lwa_nf_proc_ruc_xml-chave me->at_due-numero_ruc INTO lwa_mensagem.
            APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
          ENDIF.

      ENDCASE.

    ENDLOOP.

    "Verificar se quantidade de Vendas no SAP corresponde a Qtde das NFs de Exportação
    IF ( lva_tot_qtde_venda_sap NE lva_tot_qtde_rfl_sap ) OR
       ( lva_tot_qtde_venda_sap NE lva_tot_qtde_exp_xml ).

      WRITE lva_tot_qtde_venda_sap TO lva_number_int1.
      WRITE lva_tot_qtde_exp_xml   TO lva_number_int2.

      MESSAGE s169(zdue) WITH lva_number_int1 lva_number_int2 INTO lwa_mensagem.
      APPEND VALUE #( mensagem = lwa_mensagem ) TO lit_log_proc.
    ENDIF.

    IF lit_log_proc[] IS NOT INITIAL.
      CALL FUNCTION 'ZDUE_SHOW_LOG_PROC'
        EXPORTING
          i_logs_proc = lit_log_proc.

      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = |DU-e não pode ser gravada! |
                            )
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgv1  = |DU-e não pode ser gravada|
          msgty  = 'E'.

    ENDIF.



  ENDMETHOD.


  METHOD zif_due~check_generation_file_eudr.

    zcl_eudr_utils=>check_generation_file_geojson(
      EXPORTING
        i_id_nomeacao = i_id_nomeacao_tran
      RECEIVING
        r_ok          = DATA(is_ok)
    ).

    CHECK is_ok EQ abap_true.

    zcl_eudr_utils=>create_arquivo_geojson(
      EXPORTING
        i_id_nomeacao  = i_id_nomeacao_tran
      RECEIVING
        e_file_geojson = DATA(e_file_geojson)
    ).

    me->enviar_email_file_eudr(
      EXPORTING
        i_file_geojson = e_file_geojson
        i_id_nomeacao  = i_id_nomeacao_tran
    ).

  ENDMETHOD.


  METHOD zif_due~enviar_email_file_eudr.

    DATA: send_request    TYPE REF TO cl_bcs,
          text            TYPE bcsy_text,
          document        TYPE REF TO cl_document_bcs,
          recipient       TYPE REF TO if_recipient_bcs,
          bcs_exception   TYPE REF TO cx_bcs,
          lv_contador     TYPE i,
          text_content    TYPE soli_tab,
          lv_contador_aux TYPE i.

    IF i_file_geojson IS NOT INITIAL.
      DATA(lv_comprimento_geojson) = strlen( i_file_geojson ).
      lv_contador = 0.
      lv_contador_aux = 0.

      DO.

        IF ( lv_contador + 1 ) > lv_comprimento_geojson.
          DATA(lv_final_geojson) = lv_contador - lv_contador_aux.
          APPEND i_file_geojson+lv_contador_aux(lv_final_geojson) TO text_content.
          EXIT.
        ENDIF.

        IF i_file_geojson+lv_contador(1) EQ cl_abap_char_utilities=>newline.
          lv_final_geojson = lv_contador - lv_contador_aux.
          APPEND i_file_geojson+lv_contador_aux(lv_final_geojson) TO text_content.
          lv_contador_aux = lv_contador.
        ENDIF.

        ADD 1 TO lv_contador.

      ENDDO.

      CALL FUNCTION 'SO_RAW_TO_RTF'
        TABLES
          objcont_old = text_content
          objcont_new = text_content
        EXCEPTIONS
          OTHERS      = 0.
    ENDIF.

    TRY.
        send_request = cl_bcs=>create_persistent( ).

        zcl_eudr_utils=>get_dues_eudr_from_nomeacao(
          EXPORTING
            i_id_nomeacao = i_id_nomeacao
          RECEIVING
            r_dues        = DATA(r_dues)
        ).

        SELECT SINGLE *
          from znom_transporte INTO @DATA(lwa_znom_transporte)
          WHERE id_nomeacao_tran eq @i_id_nomeacao.

        IF i_file_geojson IS NOT INITIAL.
          APPEND |Em anexo, segue os arquivo GeoJson para o navio { lwa_znom_transporte-ds_nome_transpor } referente as du-es abaixo:| TO text.
          LOOP AT r_dues INTO DATA(ls_dues).
            APPEND ls_dues-numero_due TO text.
          ENDLOOP.
        ELSE.
          APPEND |Não foi possivel gerar o arquivo GeoJson para o navio Navio { lwa_znom_transporte-ds_nome_transpor }| TO text.
        ENDIF.

        IF i_file_geojson IS NOT INITIAL.
          document = cl_document_bcs=>create_document(
            i_type    = 'RAW'
            i_text    = text
            i_subject = |EUDR - GeoJson Navio { lwa_znom_transporte-ds_nome_transpor }| ).


*"// Anexo
          document->add_attachment( i_attachment_type    = 'TXT'
                                    i_attachment_subject = 'Documento texto'
                                    i_att_content_text   = text_content ).

*"// Armazena o anexo
          send_request->set_document( document ).
        ENDIF.

*"// Recupera os e-mail cadastrados
        IF i_email IS INITIAL.
          SELECT email
            FROM zmail
            INTO TABLE @DATA(lt_email)
            WHERE tcode EQ @zcl_eudr_utils=>lc_tcode_email_geojson.
        ELSE.
          APPEND VALUE #( email = i_email ) TO lt_email.
        ENDIF.

*"// Adiciona E-mail
        LOOP AT lt_email INTO DATA(ls_email).
          recipient = cl_cam_address_bcs=>create_internet_address( CONV #( ls_email-email ) ).
          send_request->add_recipient( i_recipient = recipient ).
        ENDLOOP.

        send_request->set_send_immediately( abap_true ).

        DATA(sent_to_all) = send_request->send( i_with_error_screen = abap_true ).

        IF sent_to_all IS NOT INITIAL.
          MESSAGE 'E-mail GeoJson enviado com Sucesso!' TYPE 'S'.
          COMMIT WORK.
        ENDIF.

      CATCH cx_bcs INTO bcs_exception.
        MESSAGE e865 WITH bcs_exception->error_type.
    ENDTRY.


  ENDMETHOD.


  METHOD zif_due~reclassificacao_eudr.

    r_alterado = abap_false.

    CHECK me->at_due-id_due IS NOT INITIAL.

    SELECT SINGLE *
      FROM znom_reme_notas INTO @DATA(_wl_reme_notas)
     WHERE id_due EQ @me->at_due-id_due.

    IF sy-subrc EQ 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_vinc_nf_remetente-msgid
                            msgno = zcx_due=>zcx_due_vinc_nf_remetente-msgno
                            attr1 = CONV #( _wl_reme_notas-docnum )
                            attr2 = CONV #( _wl_reme_notas-id_remetente )
                            attr3 = CONV #( _wl_reme_notas-id_filial )
                            attr4 = CONV #( _wl_reme_notas-grp_retorno ) )
          msgty  = cl_abap_aab_utilities=>category_error
          msgno  = zcx_due=>zcx_due_vinc_nf_remetente-msgno
          msgid  = zcx_due=>zcx_due_vinc_nf_remetente-msgid
          msgv1  = CONV #( _wl_reme_notas-docnum )
          msgv2  = CONV #( _wl_reme_notas-id_remetente )
          msgv3  = CONV #( _wl_reme_notas-id_filial )
          msgv4  = CONV #( _wl_reme_notas-grp_retorno ).
    ENDIF.

    SELECT SINGLE *
      FROM znom_remetente INTO @DATA(_wl_remetente)
     WHERE id_due EQ @me->at_due-id_due.

    IF sy-subrc EQ 0.
      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_due_vinc_remetente-msgid
                            msgno = zcx_due=>zcx_due_vinc_remetente-msgno
                            attr1 = CONV #( _wl_remetente-id_filial )
                            attr2 = CONV #( _wl_remetente-grp_retorno ) )
          msgty  = cl_abap_aab_utilities=>category_error
          msgno  = zcx_due=>zcx_due_vinc_remetente-msgno
          msgid  = zcx_due=>zcx_due_vinc_remetente-msgid
          msgv1  = CONV #( _wl_remetente-id_filial )
          msgv2  = CONV #( _wl_remetente-grp_retorno ).
    ENDIF.

    IF i_eudr IS INITIAL.

      RAISE EXCEPTION TYPE zcx_due
        EXPORTING
          textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                            msgno = zcx_due=>zcx_erro_geral-msgno
                            attr1 = CONV #( TEXT-003 ) )
          msgty  = cl_abap_aab_utilities=>category_error
          msgno  = zcx_due=>zcx_erro_geral-msgno
          msgid  = zcx_due=>zcx_erro_geral-msgid
          msgv1  = CONV #( TEXT-003 ).

    ENDIF.

    UPDATE zsdt0170 SET eudr            = i_eudr
                        dt_reclass_eudr = sy-datum
                        hr_reclass_eudr = sy-uzeit
                        us_reclass_eudr = sy-uname
     WHERE id_due = me->at_due-id_due.

    IF sy-subrc IS INITIAL.
      MESSAGE s177.
      r_alterado = abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

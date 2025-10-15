CLASS zcl_rodoviario_chegada_ws DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_cnpj_grupo_neg
      IMPORTING
        VALUE(i_cnpj_grupo_neg) TYPE stcd1 .
    METHODS set_srv_integracao
      IMPORTING
        !i_srv_integracao TYPE zde_srv_integracao .
    METHODS set_dt_inicial
      IMPORTING
        !i_dt_inicial TYPE zde_dt_inicial .
    METHODS set_dt_final
      IMPORTING
        !i_dt_final TYPE zde_dt_final .
    METHODS set_dt_chegada
      IMPORTING
        !i_dt_chegada TYPE zde_dt_chegada .
    METHODS set_sigla_terminal
      IMPORTING
        !i_sigla_terminal TYPE zde_sigla_terminal .
    METHODS consultar
      IMPORTING
        !i_gravar   TYPE char01 OPTIONAL
      RETURNING
        VALUE(r_ok) TYPE char01
      EXCEPTIONS
        http_communication_failure
        http_invalid_state
        http_processing_failed
        http_invalid_timeout
        zcx_envio .
    METHODS gravar_dados
      RETURNING
        VALUE(r_gravou) TYPE char01 .
    METHODS get_dados_consulta
      EXPORTING
        !e_dados_descarga TYPE zlest0174_tmp_t
        !e_dados_nf       TYPE zlest0175_tmp_t .
    METHODS preparar_dados_gravacao
      RETURNING
        VALUE(r_ok) TYPE char01 .
    METHODS monta_xml_02
      RETURNING
        VALUE(r_xml) TYPE string .
    METHODS tratar_retorno_02
      IMPORTING
        !i_cdata_retorno TYPE string
      CHANGING
        !c_xml_return    TYPE REF TO cl_xml_document .
    METHODS tratar_retorno_03
      IMPORTING
        !i_cdata_retorno TYPE string
      CHANGING
        !c_xml_return    TYPE REF TO cl_xml_document
      RETURNING
        VALUE(r_ok)      TYPE char01 .
    METHODS tratar_retorno_06
      IMPORTING
        !i_cdata_retorno TYPE string
      CHANGING
        !c_xml_return    TYPE REF TO cl_xml_document .
    METHODS tratar_retorno_05
      IMPORTING
        !i_cdata_retorno TYPE string
      CHANGING
        !c_xml_return    TYPE REF TO cl_xml_document .
    METHODS tratar_retorno_04
      IMPORTING
        !i_cdata_retorno TYPE string
      CHANGING
        !c_xml_return    TYPE REF TO cl_xml_document .
    METHODS set_url_token .
  PROTECTED SECTION.
private section.

  data AT_SRV_INTEGRACAO type ZDE_SRV_INTEGRACAO .
  data AT_DT_CHEGADA type ZDE_DT_CHEGADA .
  data AT_SIGLA_TERMINAL type ZDE_SIGLA_TERMINAL .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_DADOS_DESCARGA type ZLEST0174_TMP_T .
  data AT_CNPJ_EMISSOR_NF type STCD1 .
  data AT_CNPJ_GRUPO_NEG type STCD1 .
  data AT_TOKEN type STRING .
  data AT_DT_INICIAL type ZDE_DT_INICIAL .
  data AT_DT_FINAL type ZDE_DT_FINAL .

  methods MONTA_XML
    returning
      value(R_XML) type STRING .
  methods MONTA_XML_01
    returning
      value(R_XML) type STRING .
  methods SET_URL
    returning
      value(R_URL) type CHAR255 .
  methods TRATAR_XML
    changing
      !C_XML type STRING .
  methods SET_HEADER_FIELDS
    changing
      !C_IF_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  methods TRATAR_RETORNO_CONSULTA
    importing
      !I_CDATA_RETORNO type STRING
    changing
      !C_XML_RETURN type ref to CL_XML_DOCUMENT .
  methods TRATAR_RETORNO_01
    importing
      !I_CDATA_RETORNO type STRING
    changing
      !C_XML_RETURN type ref to CL_XML_DOCUMENT .
  methods CONVERSION_CNPJ_INPUT
    changing
      !I_CNPJ type STRING .
  methods MONTA_XML_06
    returning
      value(R_XML) type STRING .
  methods MONTA_XML_04
    returning
      value(R_XML) type STRING .
  methods MONTA_XML_03
    returning
      value(R_XML) type STRING .
  methods MONTA_XML_05
    returning
      value(R_XML) type STRING .
  methods GET_TOKEN_OPUS .
  methods GET_LIST .
ENDCLASS.



CLASS ZCL_RODOVIARIO_CHEGADA_WS IMPLEMENTATION.


  METHOD consultar.

    DATA: http_client     TYPE REF TO if_http_client,
          xml_return      TYPE REF TO cl_xml_document,
          return_code     TYPE i,
          vl_ini_pos      TYPE i,
          v_cdata_retorno TYPE string,
          v_xml           TYPE string,
          v_url           TYPE char255,
          lw_rodo_vli     TYPE string,           "*-CS2024000593-18.11.2024-jt-#146078-inicio
          v_msg_show      TYPE c LENGTH 200.

    r_ok = abap_false.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = TEXT-001.

    v_url = me->set_url( ).

    CHECK v_url IS NOT INITIAL.

    v_xml = me->monta_xml( ).

*-CS2024000593-18.11.2024-jt-#146078-inicio
*   CHECK V_XML IS NOT INITIAL.
    IF v_xml IS NOT INITIAL.
      me->tratar_xml( CHANGING c_xml = v_xml ).
    ENDIF.
*-CS2024000593-18.11.2024-jt-#146078-fim

    "Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( v_url )
        ssl_id             = 'DFAULT'
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    me->set_header_fields( CHANGING  c_if_http_client = http_client ).

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
        MESSAGE s004 WITH | { v_url } (Send) | RAISING http_communication_failure.
        EXIT.
      WHEN 2.
        http_client->close( ).
        MESSAGE s005 WITH | { v_url } (Send) | RAISING http_invalid_state.
        EXIT.
      WHEN 3.
        http_client->close( ).
        MESSAGE s006 WITH | { v_url } (Send) | RAISING http_processing_failed.
        EXIT.
      WHEN 4.
        http_client->close( ).
        MESSAGE s007 WITH | { v_url } (Send) | RAISING http_invalid_timeout.
        EXIT.
    ENDCASE.

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        MESSAGE s004 WITH | { v_url } (Receive) |  DISPLAY LIKE 'E' RAISING http_communication_failure.
        EXIT.
      WHEN 2.
        http_client->close( ).
        MESSAGE s005 WITH | { v_url } (Receive) |  DISPLAY LIKE 'E' RAISING http_invalid_state.
        EXIT.
      WHEN 3.
        http_client->close( ).
        MESSAGE s006 WITH | { v_url } (Receive) |  DISPLAY LIKE 'E' RAISING http_processing_failed.
        EXIT.
    ENDCASE.

    "Temp
*  V_CDATA_RETORNO = '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && '.org/2001/XMLSchema-instance"><SOAP-ENV:Body><SispatDescarregamentoRodoviario><CabecalhoRetorno><MensagemDataHora>2018-11-05T16:31:52.255-03:00</Men'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'sagemDataHora><ProtocoloRecebimento>bcsoap:521E604B-4E9B-4D27-8E3F-3056DF207846</ProtocoloRecebimento><TipoEnvio>SISPAT_DESCARREGAMENTO_VEICULOS</Tip'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'oEnvio><Transacao>CONSULTAR</Transacao><DataHoraGeracao>2018-11-05T16:31:52.255-03:00</DataHoraGeracao><Recebedor><IdentificacaoEmpresa>77.294.254/005'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && '3-15</IdentificacaoEmpresa><RazaoSocial>AMAGGI - QUERENCIA</RazaoSocial></Recebedor></CabecalhoRetorno><VeiculosDescarregados><SispatVeiculo><IdVeicul'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'o>ADT-5559</IdVeiculo><CodigoTerminalTransbordo>TIA - TERMINAL INTEGRADOR DE ARAGUARI</CodigoTerminalTransbordo><DescricaoTerminalTransbordo>TIA - TERMINAL INTEGRADOR DE ARAGUARI'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && '</DescricaoTerminalTransbordo><CnpjTerminalTransbordo>42.276.907/0011-08</CnpjTerminalTransbordo><NomeFantasiaTerminalDest'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'ino>Complexo de Tubarão - Vitória - ES</NomeFantasiaTerminalDestino><RazaoSocialTerminalDestino>VITÓRIA</RazaoSocialTerminalDestino><CnpjTerminalDesti'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'no>33592510002106</CnpjTerminalDestino><DataChegada>2018-09-07T19:00:00-03:00</DataChegada><DataEntrada>2018-09-08T09:07:14-03:00</DataEntrada><DataSa'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'ida>2018-09-08T10:34:06.94-03:00</DataSaida><NumeroPlaca>ADT-5559</NumeroPlaca><NomeMotorista>GILVAN RODRIGUES</NomeMotorista><PesoBruto>56480</PesoBr'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'uto><PesoTara>19710</PesoTara><PesoLiquido>36770</PesoLiquido><NFe><ListaNfes><IdVeiculo>ADT-5559</IdVeiculo><NumeroNotaFiscal>0000036125</NumeroNotaF'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'iscal><SerieNotaFiscal>000</SerieNotaFiscal><DataEmissao>2018-09-06</DataEmissao><ChaveNfe>51180977294254005315550000000361251411287363</ChaveNfe><Pes'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'oDeclarado>36780</PesoDeclarado><PesoDescarregado>36770</PesoDescarregado></ListaNfes></NFe></SispatVeiculo><SispatVeiculo><IdVeiculo>OOM-7144</IdVeic'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'ulo><CodigoTerminalTransbordo>TIA - TERMINAL INTEGRADOR DE ARAGUARI</CodigoTerminalTransbordo><DescricaoTerminalTransbordo>TIA - TERMINAL INTEGRADOR D'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'E ARAGUARI</DescricaoTerminalTransbordo><CnpjTerminalTransbordo>42.276.907/0011-08</CnpjTerminalTransbordo><NomeFantasiaTerminalDestino>Complexo de Tu'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'barão - Vitória - ES</NomeFantasiaTerminalDestino><RazaoSocialTerminalDestino>VITÓRIA</RazaoSocialTerminalDestino><CnpjTerminalDestino>33592510002106<'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && '/CnpjTerminalDestino><DataChegada>2018-09-08T09:00:00-03:00</DataChegada><DataEntrada>2018-09-08T09:44:29-03:00</DataEntrada><DataSaida>2018-09-08T11:'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && '49:04.37-03:00</DataSaida><NumeroPlaca>OOM-7144</NumeroPlaca><NomeMotorista>EUROIDES DONIZETE ALVES</NomeMotorista><PesoBruto>56850</PesoBruto><PesoTa'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'ra>20800</PesoTara><PesoLiquido>36050</PesoLiquido><NFe><ListaNfes><IdVeiculo>OOM-7144</IdVeiculo><NumeroNotaFiscal>0000036119</NumeroNotaFiscal><Seri'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'eNotaFiscal>000</SerieNotaFiscal><DataEmissao>2018-09-06</DataEmissao><ChaveNfe>51180977294254005315550000000361191396812841</ChaveNfe><PesoDeclarado>'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && '36230</PesoDeclarado><PesoDescarregado>36050</PesoDescarregado></ListaNfes>    </NFe></SispatVeiculo></VeiculosDescarregados></SispatDescarregamentoRo'.
*  V_CDATA_RETORNO = V_CDATA_RETORNO  && 'doviario></SOAP-ENV:Body></SOAP-ENV:Envelope>'.
*
*  RETURN_CODE = '200'.
*
*  CALL METHOD HTTP_CLIENT->RESPONSE->SET_CDATA
*    EXPORTING
*      DATA   = V_CDATA_RETORNO
*      OFFSET = 0
*      LENGTH = STRLEN( V_CDATA_RETORNO  ).
*
*  HTTP_CLIENT->RESPONSE->set_STATUS( EXPORTING CODE = '200'
*                                               REASON = 'OK' ).
    "Temp fim

    "//Check return content
    CREATE OBJECT xml_return.

    CALL METHOD xml_return->parse_string
      EXPORTING
        stream = http_client->response->get_cdata( ).

    http_client->response->get_status( IMPORTING code = return_code ).

    v_cdata_retorno = http_client->response->get_cdata( ).

    "Temp fim
    "RETURN_CODE = '200'.
    "Temp fim

    IF return_code NE '200'.
      CLEAR: vl_ini_pos.

      IF vl_ini_pos IS INITIAL.
        FIND '(400)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '400'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(401)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '401'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(403)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '403'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(404)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '404'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(422)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '422'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(500)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '500'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(503)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '503'.
        ENDIF.
      ENDIF.

      CASE return_code.
        WHEN 400.
          MESSAGE '(WS)-Requisição mal formatada!' TYPE 'S' RAISING zcx_envio.
        WHEN 401.
          MESSAGE '(WS)-Requisição requer autenticação!' TYPE 'S' RAISING zcx_envio.
        WHEN 403.
          MESSAGE '(WS)-Requisição não autorizada!' TYPE 'S' RAISING zcx_envio.
        WHEN 404.
          MESSAGE '(WS)-Registro não encontrado!' TYPE 'S' RAISING zcx_envio.
        WHEN 422.
          MESSAGE '(WS)-Erro de negócio!' TYPE 'S' RAISING zcx_envio.
        WHEN 500.
          MESSAGE '(WS)-Erro interno do servidor!' TYPE 'S' RAISING zcx_envio.
        WHEN 503.
          MESSAGE '(WS)-Serviço indisponível!' TYPE 'S' RAISING zcx_envio.
        WHEN OTHERS.
          MESSAGE '(WS)-Consulta não realizada!' TYPE 'S' RAISING zcx_envio.
      ENDCASE.

      RETURN.
    ENDIF.

    me->tratar_retorno_consulta(
      EXPORTING
        i_cdata_retorno = v_cdata_retorno
      CHANGING
        c_xml_return    = xml_return
    ).

    r_ok = abap_true.

  ENDMETHOD.


  METHOD conversion_cnpj_input.

    REPLACE ALL OCCURRENCES OF '.' IN i_cnpj WITH space.
    REPLACE ALL OCCURRENCES OF '/' IN i_cnpj WITH space.
    REPLACE ALL OCCURRENCES OF '-' IN i_cnpj WITH space.
    CONDENSE i_cnpj NO-GAPS.

  ENDMETHOD.


  METHOD get_dados_consulta.

    e_dados_descarga = me->at_dados_descarga.

  ENDMETHOD.


  METHOD gravar_dados.

    DATA: wl_zlest0174 TYPE zlest0174,
          it_zlest0174 TYPE TABLE OF zlest0174,
          wl_zlest0175 TYPE zlest0175,
          it_zlest0175 TYPE TABLE OF zlest0175.

    DATA: it_zlest0175_aux TYPE TABLE OF zde_zlest0175_tmp,
          wa_zlest0175_aux TYPE zde_zlest0175_tmp,
          it_zlest0174_aux TYPE TABLE OF zlest0174,
          wa_zlest0174_aux TYPE zlest0174.

    DATA: vg_input TYPE char01.

    r_gravou = abap_false.

    IF ( me->at_dados_descarga[] IS INITIAL ).
      MESSAGE s009.
      RETURN.
    ENDIF.

    DATA(_ok) = me->preparar_dados_gravacao( ).

    CHECK _ok EQ abap_true.

    CLEAR: it_zlest0174[], it_zlest0175[].

    FREE: it_zlest0175_aux.
    LOOP AT me->at_dados_descarga ASSIGNING FIELD-SYMBOL(<ws_descarga>).
      APPEND LINES OF <ws_descarga>-notas[] TO it_zlest0175_aux[].
    ENDLOOP.

    SELECT * FROM zlest0175 AS a
    INTO TABLE @DATA(tg_zlest0175)
    FOR ALL ENTRIES IN @it_zlest0175_aux[]
    WHERE chave_nf EQ @it_zlest0175_aux-chave_nf.

    LOOP AT me->at_dados_descarga INTO DATA(_wl_descarga).
      CLEAR: vg_input.
      LOOP AT _wl_descarga-notas INTO DATA(_wl_nf) WHERE id_registro = _wl_descarga-id_registro.
        READ TABLE tg_zlest0175 INTO DATA(ws_zles00175) WITH KEY chave_nf = _wl_nf-chave_nf.
        IF sy-subrc NE 0.
          CLEAR: wl_zlest0175.
          MOVE-CORRESPONDING _wl_nf TO wl_zlest0175.
          APPEND wl_zlest0175 TO it_zlest0175.
          CLEAR: ws_zles00175.
          vg_input = abap_true.
        ENDIF.
      ENDLOOP.

      IF vg_input EQ abap_true.
        CLEAR: wl_zlest0174.
        MOVE-CORRESPONDING _wl_descarga TO wl_zlest0174.
        APPEND wl_zlest0174 TO it_zlest0174.
      ENDIF.
    ENDLOOP.

    IF ( it_zlest0174[] IS NOT INITIAL ).
      MODIFY zlest0174 FROM TABLE it_zlest0174.
    ENDIF.

    IF ( it_zlest0175[] IS NOT INITIAL ).
      MODIFY zlest0175 FROM TABLE it_zlest0175.
    ENDIF.

    r_gravou = abap_true.


  ENDMETHOD.


  METHOD monta_xml.

    CASE me->at_srv_integracao.
      WHEN '01'. "Terminais VLI
*       r_xml = me->monta_xml_01( ).  "*-CS2024000593-18.11.2024-jt-#146078-inicio
      WHEN '02'. "Terminais BUNGE
        r_xml = me->monta_xml_02( ).
      WHEN '03'. "Terminais TGG
        r_xml = me->monta_xml_03( ).
      WHEN '04'. "Terminais RUMO
        r_xml = me->monta_xml_04( ).
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
      WHEN '05'. "Terminais Brado
        r_xml = me->monta_xml_05( ).
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
**  Begin of " CS2022000810   #84550 FF   14.02.2023
      WHEN '06'. "Terminais ROCHA
        r_xml = me->monta_xml_06( ).
** End of FF  14.02.2023

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF r_xml IS INITIAL AND me->at_srv_integracao <> '01'. "*-CS2024000593-18.11.2024-jt-#146078-inicio
      MESSAGE s003.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD monta_xml_01.

    DATA: xvalor  TYPE string,
          v_c_aux TYPE string.

    CLEAR: r_xml.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.


    conc_xml    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'.
    conc_xml       '<SOAP-ENV:Body>'.
    conc_xml          '<Consulta xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
    conc_xml             '<Login>'.
    conc_xml                me->at_auth_ws-username.
    conc_xml             '</Login>'.
    conc_xml             '<Senha>'.
    conc_xml                me->at_auth_ws-password.
    conc_xml             '</Senha>'.
    conc_xml             '<CodigoTerminal>'.
    conc_xml                me->at_sigla_terminal.
    conc_xml             '</CodigoTerminal>'.

    conc_xml             '<Data>'.
    v_c_aux = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2).
    conc_xml                v_c_aux.
    conc_xml             '</Data>'.

    conc_xml          '</Consulta>'.
    conc_xml       '</SOAP-ENV:Body>'.
    conc_xml    '</SOAP-ENV:Envelope>'.

  ENDMETHOD.


  METHOD monta_xml_02.
    DATA: xvalor  TYPE string,
          v_c_aux TYPE string.

    CLEAR: r_xml.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.

    conc_xml '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">'.
    conc_xml '<soap:Header>'.
    conc_xml    '<WsAuthHeader xmlns="http://www.unisolution.com.br/">'.
    conc_xml        '<UserName>'.
    conc_xml        me->at_auth_ws-username.
    conc_xml        '</UserName>'.
    conc_xml        '<Password>'.
    conc_xml        me->at_auth_ws-password.
    conc_xml        '</Password>'.
    conc_xml    '</WsAuthHeader>'.
    conc_xml '</soap:Header>'.
    conc_xml '<soap:Body>'.
    conc_xml    '<ConsultaDescargaRodoviaria xmlns="http://www.unisolution.com.br/">'.
    conc_xml        '<dataIncial>'.
    v_c_aux = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2).
    conc_xml                v_c_aux.
    conc_xml       '</dataIncial>'.
    conc_xml       '<dataFinal>'.
    conc_xml                v_c_aux.
    conc_xml       '</dataFinal>'.
    conc_xml       '<chaveNfe></chaveNfe>'.
    conc_xml  '</ConsultaDescargaRodoviaria>'.
    conc_xml    '</soap:Body>'.
    conc_xml '</soap:Envelope>'.

  ENDMETHOD.


  METHOD monta_xml_04.

    DATA: xvalor  TYPE string,
          zvg_dt_inic TYPE ZDE_DT_CHEGADA,
          v_c_aux TYPE string.

    CLEAR: r_xml.

*--------------------------------------------------------------------------------Inicio ajuste CS2024000455 / AOENNING.
    clear: zvg_dt_inic.
    zvg_dt_inic = me->at_dt_chegada - 3. "Data de inicio com data retrotiva 3 dias.
*--------------------------------------------------------------------------------Fim ajuste CS2024000455 / AOENNING.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.



    conc_xml    '{'.
    conc_xml       '"cnpj": "'.
    conc_xml                 me->at_cnpj_grupo_neg.
    conc_xml                                      '" ,'.
    conc_xml             '"periodo": {'.
    conc_xml               '"data_inicio": "'.
*--------------------------------------------------------------------------------Inicio ajuste CS2024000455 / AOENNING.
    v_c_aux = zvg_dt_inic(4) && '-' && zvg_dt_inic+4(2) && '-' && zvg_dt_inic+6(2) && 'T00:00:00",'.
*--------------------------------------------------------------------------------Fim ajuste CS2024000455 / AOENNING.
    conc_xml                v_c_aux .
    conc_xml               '"data_fim": "'.
    v_c_aux = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2) && 'T23:59:59"'.
    conc_xml                v_c_aux .
    conc_xml              '},'.
    conc_xml       '"terminal": "'.
    conc_xml                     me->at_sigla_terminal.
    conc_xml                                         '" ,'.
    conc_xml       '"busca_grupo_negociador": "S"'.
    conc_xml    '}'.


  ENDMETHOD.


  METHOD preparar_dados_gravacao.

    DATA:wl_descarga TYPE zde_zlest0174_tmp.

    r_ok = abap_false.

    IF sy-batch = 'X'."SO FAZER SE FOR BACKGROUND
      LOOP AT me->at_dados_descarga INTO wl_descarga.

        LOOP AT wl_descarga-notas INTO DATA(ls_notas).

          SELECT * FROM zlest0175
            INTO @DATA(ls_zlest0175)
            WHERE chave_nf = @ls_notas-chave_nf."id_registro.
          ENDSELECT.

          IF sy-subrc = 0.

            SELECT * FROM zlest0174
              INTO @DATA(ls_zlest0174)
              WHERE id_registro = @ls_zlest0175-id_registro
                AND processado = 'X'.
            ENDSELECT.

            IF sy-subrc = 0.

              DELETE me->at_dados_descarga WHERE id_tmp = wl_descarga-id_tmp.

            ENDIF.

          ENDIF.

        ENDLOOP.
      ENDLOOP .


    ENDIF.

    LOOP AT me->at_dados_descarga ASSIGNING FIELD-SYMBOL(<fs_dados_descarga>).

      IF <fs_dados_descarga>-id_registro IS INITIAL.
        "Gerar ID registro
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZREG_L1_WS'
          IMPORTING
            number                  = <fs_dados_descarga>-id_registro
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        IF ( sy-subrc NE 0 ) OR ( <fs_dados_descarga>-id_registro IS INITIAL ).
          MESSAGE s008.
          RETURN.
        ENDIF.
      ENDIF.

      LOOP AT <fs_dados_descarga>-notas ASSIGNING FIELD-SYMBOL(<fs_dados_nf>).
        <fs_dados_nf>-id_registro = <fs_dados_descarga>-id_registro.
      ENDLOOP.

      <fs_dados_descarga>-srv_integracao = me->at_srv_integracao.
      <fs_dados_descarga>-dt_registro = sy-datum.
      <fs_dados_descarga>-hr_registro = sy-uzeit.

    ENDLOOP.

    r_ok = abap_true.

  ENDMETHOD.


  METHOD set_cnpj_grupo_neg.

    me->at_cnpj_grupo_neg  = i_cnpj_grupo_neg.

  ENDMETHOD.


  METHOD set_dt_chegada.

    me->at_dt_chegada = i_dt_chegada.

  ENDMETHOD.


  METHOD set_header_fields.

    DATA: lv_value       TYPE string.
    DATA: lv_value_user  TYPE string.
    DATA: lv_value_pass  TYPE string.
    DATA: lv_value_add01 TYPE string.
    DATA: lv_value_gest  TYPE string.
    DATA: v_url               TYPE ui_src_url.
    DATA: v_token             TYPE string.
    DATA: lw_data             TYPE string.        "*-CS2024000593-18.11.2024-jt-#146078-inicio
    DATA: lc_texto_vli        TYPE c LENGTH 200.  "*-CS2024000593-18.11.2024-jt-#146078-inicio

    DATA: ex_webservice_token TYPE REF TO zcl_webservice,
          lc_endereco         TYPE string.

    TYPES BEGIN OF ty_retorno_msg.
    TYPES message TYPE string.
    TYPES END OF ty_retorno_msg.

    TYPES BEGIN OF ty_json_retorno.
    TYPES: token        TYPE string.
    TYPES: ambiente     TYPE string.
    TYPES END OF ty_json_retorno.

    TYPES BEGIN OF ty_json_retorno_06.
    TYPES: access_token TYPE string.
    TYPES: token_type   TYPE string.
    TYPES END OF ty_json_retorno_06.

    DATA: lc_mensagem   TYPE ty_retorno_msg,
          lc_retorno_06 TYPE ty_json_retorno_06,
          lc_retorno    TYPE ty_json_retorno,
          lw_token      TYPE string,                         "*-CS2024000593-18.11.2024-jt-#146078-inicio
          lc_token_vli  TYPE REF TO zcl_token_vli_new.       "*-CS2024000593-18.11.2024-jt-#146078-inicio

    DATA: var_string TYPE string,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.

    CREATE OBJECT ex_webservice_token.

    CASE me->at_srv_integracao.

*-CS2024000593-18.11.2024-jt-#146078-inicio
      WHEN '01'.
        CREATE OBJECT lc_token_vli.

        TRY .
            lw_token = lc_token_vli->get_token( 'DESCARGA_RODOV_VLI' ).

          CATCH zcx_integracao INTO DATA(ex_integra).
            MESSAGE ID ex_integra->msgid TYPE 'S' NUMBER ex_integra->msgno WITH ex_integra->msgv1 ex_integra->msgv2 ex_integra->msgv3 ex_integra->msgv4 DISPLAY LIKE 'E'.
            RETURN.
          CATCH zcx_error INTO DATA(ex_error).
            MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'access_token'
            value = lw_token.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'client_id'
            value = CONV #( me->at_auth_ws-username ).

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'edi-token'
            value = CONV #( me->at_auth_ws-add01 ).

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'GET'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~server_protocol'
            value = 'HTTP/1.1'.
*-CS2024000593-18.11.2024-jt-#146078-fim

      WHEN '04'.

*-US 160340-04.12.2024-#160340-JT-inicio
        DATA(lt_header_auth) = zcl_gestao_token=>get_token_valido( i_id_token = '0009' ).
        IF lt_header_auth[] IS NOT INITIAL.
          READ TABLE lt_header_auth INTO DATA(lw_header_auth) INDEX 1.
          SPLIT lw_header_auth-value AT abap_off INTO DATA(lc_constant) v_token.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = '~request_method'
              value = 'POST'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = '~server_protocol'
              value = 'HTTP/1.1'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = 'Content-Type'
              value = 'application/json'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = 'Authorization'
              value = v_token.

          RETURN.
        ENDIF.
*-US 160340-04.12.2024-#160340-JT-fim

        SELECT SINGLE * INTO @DATA(ls_zauth_webservice)
             FROM zauth_webservice
           WHERE service = 'RUMO_TOKEN'.
        IF sy-subrc = 0.

          CREATE OBJECT ex_webservice_token.
          ex_webservice_token->at_url = ls_zauth_webservice-url.
          DATA(var_http) = ex_webservice_token->url( i_url = CONV #( ex_webservice_token->at_url ) ).  "*-CS2024000593-18.11.2024-jt-#146078-inicio
          ex_webservice_token->zif_webservice~abrir_conexao( i_http = var_http ).

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = '~request_method'
              value = 'POST'.

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = '~server_protocol'
              value = 'HTTP/1.1'.

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = 'Content-Type'
              value = 'application/json'.

          lv_value_user = ls_zauth_webservice-username.
          var_http->request->set_form_field( EXPORTING name = 'username' value = lv_value_user ).

          lv_value_pass = ls_zauth_webservice-password.
          var_http->request->set_form_field( EXPORTING name = 'password' value = lv_value_pass ).

          lv_value_gest = ls_zauth_webservice-add01.
          var_http->request->set_form_field( EXPORTING name = 'cod_gestao' value = lv_value_gest ).

          DATA text_form TYPE string.
          text_form = '{"username":"' && lv_value_user && '","password":"' && lv_value_pass && '","cod_gestao":' && lv_value_gest && '}'.

          ex_webservice_token->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http
              i_xml                      = text_form
            IMPORTING
              e_code                     = DATA(e_code)
              e_reason                   = DATA(e_reason)
            RECEIVING
              e_resultado                = DATA(token_retorno)
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5 ).

          CASE sy-subrc.
            WHEN 1 OR 5.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
            WHEN 2.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN 3.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN 4.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDCASE.

          IF e_code NE 200.

            DATA: lc_texto TYPE c LENGTH 200.

            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = token_retorno
              CHANGING
                data = lc_mensagem.

            lc_texto = lc_mensagem-message.
            sy-msgv1 = lc_texto+000(50).
            sy-msgv2 = lc_texto+050(50).
            sy-msgv3 = lc_texto+100(50).
            sy-msgv4 = lc_texto+150(50).

            MESSAGE i028(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lwa_message_token).

            CONCATENATE 'Rumo Token:' lwa_message_token INTO lwa_message_token SEPARATED BY space.

            MESSAGE lwa_message_token TYPE 'I'.

          ELSE.

            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = token_retorno
              CHANGING
                data = lc_retorno.

*           v_token = token_retorno.
            v_token = lc_retorno-token.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = '~request_method'
                value = 'POST'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = '~server_protocol'
                value = 'HTTP/1.1'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = 'Content-Type'
                value = 'application/json'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = 'Authorization'
                value = v_token.

*-US 160340-04.12.2024-#160340-JT-inicio
            zcl_gestao_token=>update_token( i_id_token = '0009' i_access_token = lc_retorno-token  i_token_type = 'Basic' ).
*-US 160340-04.12.2024-#160340-JT-fim

          ENDIF.

        ENDIF.

*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
      WHEN '05'.
        CLEAR ls_zauth_webservice.

*-US 160408-05.12.2024-#160408-JT-inicio
        lt_header_auth = zcl_gestao_token=>get_token_valido( i_id_token = '0010' ).
        IF lt_header_auth[] IS NOT INITIAL.
          READ TABLE lt_header_auth INTO lw_header_auth INDEX 1.
          v_token = lw_header_auth-value.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = '~request_method'
              value = 'POST'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = '~server_protocol'
              value = 'HTTP/1.1'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = 'Content-Type'
              value = 'application/json'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = 'Authorization'
              value = v_token.

          RETURN.
        ENDIF.
*-US 160408-05.12.2024-#160408-JT-fim

        SELECT SINGLE * INTO ls_zauth_webservice
             FROM zauth_webservice
           WHERE service = 'BRADO_TOKEN'.

        IF sy-subrc = 0.

          CREATE OBJECT ex_webservice_token.
          ex_webservice_token->at_url = ls_zauth_webservice-url.

          CLEAR var_http.
          var_http = ex_webservice_token->url( i_url = CONV #( ex_webservice_token->at_url ) ).
          ex_webservice_token->zif_webservice~abrir_conexao( i_http = var_http ).

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = '~request_method'
              value = 'POST'.

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = '~server_protocol'
              value = 'HTTP/1.1'.


          lv_value_user = ls_zauth_webservice-username.
          lv_value_pass = ls_zauth_webservice-password.

          CLEAR: var_string.
          var_string = |{ lv_value_user }:{ lv_value_pass }|.


          "Convert base64.
          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text   = var_string
            IMPORTING
              buffer = lv_xstring
            EXCEPTIONS
              failed = 1
              OTHERS = 2.
          IF sy-subrc EQ 0.
            "Converter XSTRING em BASE64.
            CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
              EXPORTING
                input  = lv_xstring
              IMPORTING
                output = lv_string.
            IF lv_string IS NOT INITIAL.
              "Dados convertido BASE64 com sucesso.
              CALL METHOD var_http->request->set_header_field
                EXPORTING
                  name  = 'Authorization'
                  value = |Basic { lv_string }|.
            ENDIF.
          ENDIF.


          ex_webservice_token->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http
*              i_xml                      = text_form
            IMPORTING
              e_code                     = e_code
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = token_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5 ).

          CASE sy-subrc.
            WHEN 1 OR 5.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
            WHEN 2.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN 3.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN 4.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDCASE.

          IF e_code NE 200.

            CLEAR lc_texto.
            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = token_retorno
              CHANGING
                data = lc_mensagem.

            lc_texto = lc_mensagem-message.
            sy-msgv1 = lc_texto+000(50).
            sy-msgv2 = lc_texto+050(50).
            sy-msgv3 = lc_texto+100(50).
            sy-msgv4 = lc_texto+150(50).

            CLEAR lwa_message_token.
            MESSAGE i028(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lwa_message_token.

            CONCATENATE 'Brado Token:' lwa_message_token INTO lwa_message_token SEPARATED BY space.

            MESSAGE lwa_message_token TYPE 'I'.

          ELSE.

            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = token_retorno
              CHANGING
                data = lc_retorno.

*           v_token = token_retorno.
***            v_token = lc_retorno-token.

            CONCATENATE 'Bearer' lc_retorno-token
                   INTO v_token SEPARATED BY space.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = '~request_method'
                value = 'POST'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = '~server_protocol'
                value = 'HTTP/1.1'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = 'Content-Type'
                value = 'application/json'.


            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = 'Authorization'
                value = v_token.

*-US 160408-05.12.2024-#160408-JT-inicio
            zcl_gestao_token=>update_token( i_id_token = '0010' i_access_token = lc_retorno-token  i_token_type = 'Bearer' ).
*-US 160408-05.12.2024-#160408-JT-fim

          ENDIF.
        ENDIF.
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575

**  Begin of " CS2022000810   #84550 FF   14.02.2023
      WHEN '06'. "Terminais Rocha

        CLEAR ls_zauth_webservice.

*-US 160408-05.12.2024-#160408-JT-inicio
        lt_header_auth = zcl_gestao_token=>get_token_valido( i_id_token = '0011' ).
        IF lt_header_auth[] IS NOT INITIAL.
          READ TABLE lt_header_auth INTO lw_header_auth INDEX 1.
          SPLIT lw_header_auth-value AT abap_off INTO lc_constant v_token.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = '~request_method'
              value = 'GET'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = '~server_protocol'
              value = 'HTTP/1.1'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = 'Content-Type'
              value = 'application/json'.

          CALL METHOD c_if_http_client->request->set_header_field
            EXPORTING
              name  = 'Authorization'
              value = v_token.

          RETURN.
        ENDIF.
*-US 160408-05.12.2024-#160408-JT-fim

        SELECT SINGLE * INTO ls_zauth_webservice
             FROM zauth_webservice
           WHERE service = 'ROCHA_TOKEN'.
        IF sy-subrc = 0.

          CONCATENATE ls_zauth_webservice-url 'token' INTO DATA(lv_post_url).

          CREATE OBJECT ex_webservice_token.
          ex_webservice_token->at_url = lv_post_url.
          var_http = ex_webservice_token->url( i_url = CONV #( ex_webservice_token->at_url ) ).
          ex_webservice_token->zif_webservice~abrir_conexao( i_http = var_http ).

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = '~request_method'
              value = 'POST'.

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = '~server_protocol'
              value = 'HTTP/1.1'.

          CALL METHOD var_http->request->set_header_field
            EXPORTING
              name  = 'Content-Type'
              value = 'application/x-www-form-urlencoded'.

          lv_value_add01 = ls_zauth_webservice-add01.
          TRANSLATE lv_value_add01 TO LOWER CASE.

          var_http->request->set_form_field( EXPORTING name = 'grant_type' value = lv_value_add01 ).

          lv_value_user = ls_zauth_webservice-username.
          var_http->request->set_form_field( EXPORTING name = 'username' value = lv_value_user ).

          lv_value_pass = ls_zauth_webservice-password.
          var_http->request->set_form_field( EXPORTING name = 'password' value = lv_value_pass ).

*           text_form = 'grant_type=password&username=' && lv_value_user && '&password=' && lv_value_pass.

          ex_webservice_token->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http
              i_xml                      = text_form
            IMPORTING
              e_code                     = e_code
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = token_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5 ).

          CASE sy-subrc.
            WHEN 1 OR 5.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
            WHEN 2.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN 3.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN 4.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDCASE.

          IF e_code NE 200.

            CLEAR lc_texto.

            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = token_retorno
              CHANGING
                data = lc_mensagem.

            lc_texto = lc_mensagem-message.
            sy-msgv1 = lc_texto+000(50).
            sy-msgv2 = lc_texto+050(50).
            sy-msgv3 = lc_texto+100(50).
            sy-msgv4 = lc_texto+150(50).

            CLEAR lwa_message_token.
            MESSAGE i028(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lwa_message_token.

            CONCATENATE 'Rocha Token:' lwa_message_token INTO lwa_message_token SEPARATED BY space.

            MESSAGE lwa_message_token TYPE 'I'.

          ELSE.

            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = token_retorno
              CHANGING
                data = lc_retorno_06.


*            DATA: lv_dt_inicial(10),
*                  lv_dt_final(10).
*
*            DATA: lv_data TYPE sy-datum.
*            lv_data = sy-datum - 1.
*            DATA: http_client          TYPE REF TO if_http_client.

*            lv_dt_inicial = lv_dt_final = |{ lv_data(4) }-{ lv_data+4(2) }-{ lv_data+6(2) }|.

*            CONCATENATE ls_zauth_webservice-url 'movimentacao/dadosVeiculo?dataInicial=' lv_dt_inicial '&dataFinal='
*                                                                                lv_dt_final INTO DATA(lv_get_url).

            v_token = lc_retorno_06-access_token.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = '~request_method'
                value = 'GET'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = '~server_protocol'
                value = 'HTTP/1.1'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = 'Content-Type'
                value = 'application/json'.

            CALL METHOD c_if_http_client->request->set_header_field
              EXPORTING
                name  = 'Authorization'
                value = v_token.

*-US 160408-05.12.2024-#160408-JT-inicio
            zcl_gestao_token=>update_token( i_id_token = '0011' i_access_token = lc_retorno_06-access_token  i_token_type = 'Bearer' ).
*-US 160408-05.12.2024-#160408-JT-fim

          ENDIF.
        ENDIF.
** End of FF  14.02.2023
      WHEN OTHERS.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'POST'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~server_protocol'
            value = 'HTTP/1.1'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'Content-Type'
            value = 'text/xml; charset=UTF-8'.

    ENDCASE.


  ENDMETHOD.


  METHOD set_sigla_terminal.

    me->at_sigla_terminal = i_sigla_terminal.

  ENDMETHOD.


  METHOD set_srv_integracao.

    me->at_srv_integracao = i_srv_integracao.

  ENDMETHOD.


  METHOD set_url.

    CLEAR: r_url.

    CASE me->at_srv_integracao.
      WHEN '01'. "Terminais VLI

        SELECT SINGLE *
          FROM zauth_webservice INTO me->at_auth_ws
         WHERE service = 'DESCARGA_RODO_SRV_INT_01'.

        IF sy-subrc EQ 0.
*-CS2024000593-18.11.2024-jt-#146078-inicio
          r_url  = me->at_auth_ws-url.
          DATA(lw_data) = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2).
          r_url = |{ r_url }?dataMovimento={ lw_data }&terminalDescarga={ me->at_sigla_terminal }|.
*-CS2024000593-18.11.2024-jt-#146078-fim
        ENDIF.

      WHEN '02'. "Terminais Bunge
        IF  me->at_sigla_terminal = 'BGESFS'.
          SELECT SINGLE *
            FROM zauth_webservice INTO me->at_auth_ws
           WHERE service = 'DESCARGA_RODO_SRV_INT_02'.
        ELSE.
          SELECT SINGLE *
            FROM zauth_webservice INTO me->at_auth_ws
           WHERE service = 'DESCARGA_RODO_SRV_INT_03'.
        ENDIF.


        IF sy-subrc EQ 0.
          r_url  = me->at_auth_ws-url.
        ENDIF.

      WHEN '03'. "Terminais TGG.
        SELECT SINGLE *
            FROM zauth_webservice INTO me->at_auth_ws
           WHERE service = 'DESCARGA_RODO_SRV_INT_03'.

        IF sy-subrc EQ 0.
          r_url  = me->at_auth_ws-url.
        ENDIF.
      WHEN '04'. "Terminais Rumo

        SELECT SINGLE *
          FROM zauth_webservice INTO me->at_auth_ws
         WHERE service = 'DESCARGA_RODO_SRV_INT_04'.

        IF sy-subrc EQ 0.
          r_url  = me->at_auth_ws-url.
        ENDIF.
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
      WHEN '05'. "Terminais BRADO

        SELECT SINGLE *
          FROM zauth_webservice INTO me->at_auth_ws
         WHERE service = 'DESCARGA_RODO_SRV_INT_05'.

        IF sy-subrc EQ 0.
          r_url  = me->at_auth_ws-url.
        ENDIF.
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575

**  Begin of " CS2022000810   #84550 FF   14.02.2023
      WHEN '06'. "Terminais Rocha
        SELECT SINGLE *
          FROM zauth_webservice INTO me->at_auth_ws
          WHERE service = 'ROCHA_TOKEN'.

        IF sy-subrc EQ 0.

          DATA: lv_dt_inicial(10),
                lv_dt_final(10).

          lv_dt_inicial = lv_dt_final = |{ me->at_dt_inicial(4) }-{ me->at_dt_inicial+4(2) }-{ me->at_dt_inicial+6(2) }|.

          CONCATENATE at_auth_ws-url 'movimentacao/dadosVeiculo?dataInicial=' lv_dt_inicial '&dataFinal='
                                                                              lv_dt_final INTO DATA(lv_get_url).
          r_url  = lv_get_url.
        ENDIF.
** End of FF  14.02.2023

      WHEN OTHERS.
        MESSAGE s002 WITH me->at_srv_integracao.
        RETURN.
    ENDCASE.

    IF r_url IS INITIAL.
      MESSAGE s001 WITH me->at_srv_integracao.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD tratar_retorno_01.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: v_node_dados_recebedor TYPE c,
          v_novo_veiculo         TYPE c,
          v_nova_nf              TYPE c.

    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          wl_zlest0174     TYPE zlest0174,
          wl_zlest0175     TYPE zlest0175,
          t_zlest0175      TYPE zlest0175_t,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name,

          lt_dados         TYPE zlese0260,  "*-CS2024000593-18.11.2024-jt-#146078-inicio
          lw_dados         TYPE zlese0261,   "*-CS2024000593-18.11.2024-jt-#146078-inicio
          lw_listanfe      TYPE zlese0262,   "*-CS2024000593-18.11.2024-jt-#146078-inicio
          lw_campos_nfe    TYPE zde_campos_nfe,
          zcl_util         TYPE REF TO zcl_util.

    CLEAR: wl_zlest0174, wl_zlest0175, t_zlest0175[], v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.

    CREATE OBJECT zcl_util.  "*-CS2024000593-18.11.2024-jt-#146078-inicio

*-CS2024000593-18.11.2024-jt-#146078-inicio
*--------------------------------------------------
*-- carregar dados
*--------------------------------------------------
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_cdata_retorno
      CHANGING
        data = lt_dados.

    CHECK lt_dados IS NOT INITIAL.

    LOOP AT lt_dados-data INTO lw_dados.
      CLEAR wl_zlest0174_tmp.

      v_cont_reg_tmp                                 = v_cont_reg_tmp + 1.

      wl_zlest0174_tmp-id_tmp                        = v_cont_reg_tmp.
      wl_zlest0174_tmp-srv_integracao                = '01' .
      wl_zlest0174_tmp-protocolo_rec                 = abap_off.
      wl_zlest0174_tmp-recebedor_cnpj                = abap_off.
      wl_zlest0174_tmp-recebedor_name                = abap_off.
      wl_zlest0174_tmp-sigla_terminal_transb         = lw_dados-codigoterminaltransbordo.
      wl_zlest0174_tmp-terminal_transb               = lw_dados-descricaoterminaltransbordo.
      wl_zlest0174_tmp-ds_terminal_transb            = lw_dados-descricaoterminaltransbordo.

      me->conversion_cnpj_input( CHANGING i_cnpj     = lw_dados-cnpjterminaltransbordo ).
      wl_zlest0174_tmp-cnpj_terminal_transb          = lw_dados-cnpjterminaltransbordo.

      wl_zlest0174_tmp-nm_fantasia_terminal_destino  = lw_dados-nomefantasiaterminaldestino.
      wl_zlest0174_tmp-rz_social_terminal_destino    = lw_dados-razaosocialterminaldestino.

      me->conversion_cnpj_input( CHANGING i_cnpj     = lw_dados-cnpjterminaldestino ).
      wl_zlest0174_tmp-cnpj_terminal_destino         = lw_dados-cnpjterminaldestino.

      wl_zlest0174_tmp-dt_chegada                    = lw_dados-datamovimento(4) && lw_dados-datamovimento+5(2) && lw_dados-datamovimento+8(2). "*-US159740-29.11.2024-JT
*     wl_zlest0174_tmp-dt_chegada                    = lw_dados-datachegada(4) && lw_dados-datachegada+5(2) && lw_dados-datachegada+8(2).
      wl_zlest0174_tmp-dt_entrada                    = lw_dados-dataentrada(4) && lw_dados-dataentrada+5(2) && lw_dados-dataentrada+8(2).
      wl_zlest0174_tmp-dt_saida                      = lw_dados-datasaida(4)   && lw_dados-datasaida+5(2)   && lw_dados-datasaida+8(2).
      wl_zlest0174_tmp-nome_motorista                = abap_off.
      wl_zlest0174_tmp-placa                         = lw_dados-numeroplaca.
      wl_zlest0174_tmp-peso_bruto                    = lw_dados-pesobruto * 1000.
      wl_zlest0174_tmp-peso_tara                     = lw_dados-pesotara * 1000.
      wl_zlest0174_tmp-peso_liquido                  = lw_dados-pesoliquido * 1000.
      wl_zlest0174_tmp-dt_registro                   = sy-datum.
      wl_zlest0174_tmp-hr_registro                   = sy-uzeit.

      APPEND wl_zlest0174_tmp                       TO it_zlest0174_tmp.

      LOOP AT lw_dados-listanfe INTO lw_listanfe.
        CLEAR wl_zlest0175_tmp.

        lw_campos_nfe                                = zcl_util->get_atributos_nfe( CONV #( lw_listanfe-chavenfe ) ).

        wl_zlest0175_tmp-id_tmp                      = v_cont_reg_tmp.
        wl_zlest0175_tmp-chave_nf                    = lw_listanfe-chavenfe.
        wl_zlest0175_tmp-model                       = lw_campos_nfe-model.
        wl_zlest0175_tmp-nfenum                      = lw_campos_nfe-nfnum9.
        wl_zlest0175_tmp-series                      = lw_campos_nfe-serie.
        wl_zlest0175_tmp-stcd1                       = lw_campos_nfe-stcd1.
        wl_zlest0175_tmp-docdat                      = lw_listanfe-dataemissao(4) && lw_listanfe-dataemissao+5(2) && lw_listanfe-dataemissao+8(2).
        wl_zlest0175_tmp-docnum                      = abap_off.
        wl_zlest0175_tmp-peso_declarado              = lw_listanfe-pesodeclarado * 1000.
        wl_zlest0175_tmp-peso_descarga               = lw_listanfe-pesodescarregado * 1000.

        APPEND wl_zlest0175_tmp                     TO it_zlest0175_tmp.
      ENDLOOP.
    ENDLOOP.
*-CS2024000593-18.11.2024-jt-#146078-fim

*-CS2024000593-18.11.2024-jt-#146078-inicio
*------------------------------------------------------------------------------------------------*
*   Ler Cabeçalho Retorno
*------------------------------------------------------------------------------------------------*
*
*    DATA(_xml_cab) = c_xml_return->find_node( EXPORTING name = 'CabecalhoRetorno' ).
*
*    IF _xml_cab IS NOT INITIAL.
*
*      DATA(_iterator_cab) = _xml_cab->create_iterator( ).
*      DATA(_xml_node_cab) = _iterator_cab->get_next( ).
*
*      CLEAR: v_node_dados_recebedor.
*
*      WHILE _xml_node_cab IS NOT INITIAL.
*
*        CASE _xml_node_cab->get_type( ).
*          WHEN: if_ixml_node=>co_node_element.
*
*            DATA(_node_name)  = _xml_node_cab->get_name( ).
*            DATA(_node_valor) = _xml_node_cab->get_value( ).
*
*            CASE _node_name.
*              WHEN 'ProtocoloRecebimento'.
*                v_protocolo_rec   = _node_valor.
*              WHEN 'Recebedor'.
*                v_node_dados_recebedor = abap_true.
*              WHEN 'IdentificacaoEmpresa'.
*
*                IF v_node_dados_recebedor EQ abap_true.
*
*                  me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
*
*                  v_recebedor_cnpj  = _node_valor.
*                ENDIF.
*
*              WHEN 'RazaoSocial'.
*
*                IF v_node_dados_recebedor EQ abap_true.
*                  v_recebedor_name  = _node_valor.
*                ENDIF.
*
*                CLEAR: v_node_dados_recebedor.
*
*            ENDCASE.
*        ENDCASE.
*
*        _xml_node_cab = _iterator_cab->get_next( ).
*      ENDWHILE.
*    ENDIF.
*
**------------------------------------------------------------------------------------------------*
**   Ler Veiculos Descarregados
**------------------------------------------------------------------------------------------------*
*
*    DATA(_xml_veic_descarregados) = c_xml_return->find_node( EXPORTING name = 'VeiculosDescarregados' ).
*
*    IF _xml_veic_descarregados IS NOT INITIAL.
*
*      DATA(_iterator_veic_descar) = _xml_veic_descarregados->create_iterator( ).
*      DATA(_xml_node_veic_descar) = _iterator_veic_descar->get_next( ).
*
*      CLEAR: v_novo_veiculo, v_cont_reg_tmp,
*             it_zlest0174_tmp[], wl_zlest0174_tmp,
*             it_zlest0175_tmp[], wl_zlest0175_tmp.
*
*      WHILE _xml_node_veic_descar IS NOT INITIAL.
*
*        CASE _xml_node_veic_descar->get_type( ).
*          WHEN: if_ixml_node=>co_node_element.
*
*            _node_name  = _xml_node_veic_descar->get_name( ).
*            _node_valor = _xml_node_veic_descar->get_value( ).
*
*            CASE _node_name.
*              WHEN 'SispatVeiculo'.
*
*                "Novo Veiculo
*                IF wl_zlest0174_tmp IS NOT INITIAL.
*                  APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
*                ENDIF.
*
*                CLEAR: wl_zlest0174_tmp.
*
*                "Gerar ID Temporario
*                ADD 1 TO v_cont_reg_tmp.
*
*                wl_zlest0174_tmp-id_tmp = v_cont_reg_tmp.
*
*                wl_zlest0174_tmp-sigla_terminal_transb = me->at_sigla_terminal.
*
**----------------------------------------------------------------------------*
**             Dados Descarga - Veiculo
**----------------------------------------------------------------------------*
*              WHEN 'IdVeiculo'.
*
*              WHEN 'CodigoTerminalTransbordo'.
*
*                wl_zlest0174_tmp-terminal_transb = _node_valor.
*
*              WHEN 'DescricaoTerminalTransbordo'.
*
*                wl_zlest0174_tmp-ds_terminal_transb = _node_valor.
*
*              WHEN 'CnpjTerminalTransbordo'.
*
*                me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
*                wl_zlest0174_tmp-cnpj_terminal_transb = _node_valor.
*
*              WHEN 'NomeFantasiaTerminalDestino'.
*                wl_zlest0174_tmp-nm_fantasia_terminal_destino = _node_valor.
*
*              WHEN 'RazaoSocialTerminalDestino'.
*                wl_zlest0174_tmp-rz_social_terminal_destino = _node_valor.
*              WHEN 'CnpjTerminalDestino'.
*
*                me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
*                wl_zlest0174_tmp-cnpj_terminal_destino = _node_valor.
*
*              WHEN 'DataChegada'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0174_tmp-dt_chegada = _node_valor.
*
*              WHEN 'DataEntrada'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0174_tmp-dt_entrada = _node_valor.
*
*              WHEN 'DataSaida'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0174_tmp-dt_saida = _node_valor.
*
*              WHEN 'NumeroPlaca'.
*
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0174_tmp-placa = _node_valor.
*
*              WHEN 'NomeMotorista'.
*
*                wl_zlest0174_tmp-nome_motorista = _node_valor.
*
*              WHEN 'PesoBruto'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0174_tmp-peso_bruto = _node_valor.
*
*              WHEN 'PesoTara'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0174_tmp-peso_tara = _node_valor.
*
*              WHEN 'PesoLiquido'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0174_tmp-peso_liquido = _node_valor.
*
**----------------------------------------------------------------------------*
**             Dados Descarga - Notas Fiscal
**----------------------------------------------------------------------------*
*
*              WHEN 'ListaNfes'.
*
*                "Nova NF
*                IF wl_zlest0175_tmp IS NOT INITIAL.
*                  APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
*                ENDIF.
*
*                CLEAR: wl_zlest0175_tmp.
*
*                wl_zlest0175_tmp-id_tmp = v_cont_reg_tmp.
*
*              WHEN 'NumeroNotaFiscal'.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = _node_valor
*                  IMPORTING
*                    output = wl_zlest0175_tmp-nfenum.
*
*              WHEN 'SerieNotaFiscal'.
*
*                wl_zlest0175_tmp-series = _node_valor.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = wl_zlest0175_tmp-series
*                  IMPORTING
*                    output = wl_zlest0175_tmp-series.
*
*
*              WHEN 'DataEmissao'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0175_tmp-docdat = _node_valor.
*
*              WHEN 'ChaveNfe'.
*
*                wl_zlest0175_tmp-chave_nf = _node_valor.
*                wl_zlest0175_tmp-model    = '55'.
*                wl_zlest0175_tmp-stcd1    = wl_zlest0175_tmp-chave_nf+6(14).
*
*              WHEN 'PesoDeclarado'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0175_tmp-peso_declarado = _node_valor.
*
*              WHEN 'PesoDescarregado'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0175_tmp-peso_descarga = _node_valor.
*
*            ENDCASE.
*        ENDCASE.
*
*        _xml_node_veic_descar = _iterator_veic_descar->get_next( ).
*      ENDWHILE.
*    ENDIF.
*
*    IF wl_zlest0174_tmp IS NOT INITIAL.
*      APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
*    ENDIF.
*
*    IF wl_zlest0175_tmp IS NOT INITIAL.
*      APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
*    ENDIF.
*-CS2024000593-18.11.2024-jt-#146078-fim

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0174_tmp>).
      <fs_zlest0174_tmp>-protocolo_rec  = v_protocolo_rec.
      <fs_zlest0174_tmp>-recebedor_cnpj = v_recebedor_cnpj.
      <fs_zlest0174_tmp>-recebedor_name = v_recebedor_name.

      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0175_tmp>) WHERE id_tmp EQ <fs_zlest0174_tmp>-id_tmp.
        APPEND <fs_zlest0175_tmp> TO <fs_zlest0174_tmp>-notas.
      ENDLOOP.
    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0174_tmp[].

  ENDMETHOD.


  METHOD tratar_retorno_02.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: v_node_dados_recebedor TYPE c,
          v_novo_veiculo         TYPE c,
          v_nova_nf              TYPE c.


    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          wl_zlest0174     TYPE zlest0174,
          wl_zlest0175     TYPE zlest0175,
          t_zlest0175      TYPE zlest0175_t,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name.

    CLEAR: wl_zlest0174, wl_zlest0175, t_zlest0175[], v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.

*------------------------------------------------------------------------------------------------*
*   Ler Cabeçalho Retorno
*------------------------------------------------------------------------------------------------*

    "XML
    DATA(_xml_rodo_descarregados) = c_xml_return->find_node( EXPORTING name = 'ConsultaDescargaRodoviariaResult' ).

    IF _xml_rodo_descarregados IS NOT INITIAL.

      DATA(_iterator_rodo_descar) = _xml_rodo_descarregados->create_iterator( ).
      DATA(_xml_node_rodo_descar) = _iterator_rodo_descar->get_next( ).

      CLEAR: v_novo_veiculo, v_cont_reg_tmp,
             it_zlest0174_tmp[], wl_zlest0174_tmp,
             it_zlest0175_tmp[], wl_zlest0175_tmp.

      WHILE _xml_node_rodo_descar IS NOT INITIAL.

        CASE _xml_node_rodo_descar->get_type( ).
          WHEN: if_ixml_node=>co_node_element.

            DATA(_node_name)  = _xml_node_rodo_descar->get_name( ).
            DATA(_node_valor) = _xml_node_rodo_descar->get_value( ).

            CASE _node_name.
              WHEN 'PlacaVeiculo'.
                "Novo Veiculo
                IF wl_zlest0174_tmp IS NOT INITIAL.
                  APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
                ENDIF.

                CLEAR: wl_zlest0174_tmp.

                "Gerar ID Temporario
                ADD 1 TO v_cont_reg_tmp.

                wl_zlest0174_tmp-id_tmp = v_cont_reg_tmp.
                wl_zlest0174_tmp-cnpj_terminal_transb         = ' '.
                wl_zlest0174_tmp-terminal_transb              = ' '.
                wl_zlest0174_tmp-ds_terminal_transb           = ' '.

                wl_zlest0174_tmp-sigla_terminal_transb = me->at_sigla_terminal.
                IF wl_zlest0174_tmp-sigla_terminal_transb       = 'BGESFS'.
                  "
                  wl_zlest0174_tmp-terminal_transb              = 'BUNGE ALIMENTOS S/A'.
                  wl_zlest0174_tmp-ds_terminal_transb           = 'São Francisco do Sul'.
                  wl_zlest0174_tmp-cnpj_terminal_transb         = '84046101000940'.
                  "
                  wl_zlest0174_tmp-nm_fantasia_terminal_destino = 'São Francisco do Sul'.
                  wl_zlest0174_tmp-rz_social_terminal_destino   = 'BUNGE ALIMENTOS S/A'.
                  wl_zlest0174_tmp-cnpj_terminal_destino        = '84046101000940'.
                ELSE.
                  "
                  wl_zlest0174_tmp-terminal_transb              = 'BUNGE ALIMENTOS S/A'.
                  wl_zlest0174_tmp-ds_terminal_transb           = 'Paranagua'.
                  wl_zlest0174_tmp-cnpj_terminal_transb         = '84046101028101'.
                  "
                  wl_zlest0174_tmp-nm_fantasia_terminal_destino = 'Paranagua'.
                  wl_zlest0174_tmp-rz_social_terminal_destino   = 'BUNGE ALIMENTOS S/A'.
                  wl_zlest0174_tmp-cnpj_terminal_destino        = '84046101028101'.
                ENDIF.

                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
                CONDENSE _node_valor NO-GAPS.

                wl_zlest0174_tmp-placa = _node_valor.

              WHEN 'CnpjCpfFornecedor'.
                "
                me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
                wl_zlest0174_tmp-recebedor_cnpj = _node_valor.
                SELECT SINGLE name1
                  INTO wl_zlest0174_tmp-recebedor_name
                  FROM lfa1
                  WHERE stcd1 = wl_zlest0174_tmp-recebedor_cnpj.

              WHEN 'DataChegada'.

                _node_valor = _node_valor(10).
                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.

                wl_zlest0174_tmp-dt_chegada = _node_valor.

              WHEN 'DataEntrada'.

                _node_valor = _node_valor(10).
                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.

                wl_zlest0174_tmp-dt_entrada = _node_valor.

              WHEN 'DataSaida'.

                _node_valor = _node_valor(10).
                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.

                wl_zlest0174_tmp-dt_saida = _node_valor.
                "
*              WHEN 'NomeMotorista'.
*                WL_ZLEST0174_TMP-NOME_MOTORISTA = _NODE_VALOR.

              WHEN 'PesoBruto'.
                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
                CONDENSE _node_valor NO-GAPS.

                wl_zlest0174_tmp-peso_bruto = _node_valor.

              WHEN 'PesoTara'.
                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
                CONDENSE _node_valor NO-GAPS.
                wl_zlest0174_tmp-peso_tara = _node_valor.

              WHEN 'PesoLiquido'.
                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
                CONDENSE _node_valor NO-GAPS.
                wl_zlest0174_tmp-peso_liquido = _node_valor.

              WHEN 'NumeroNF'.
                "Nova NF
                IF wl_zlest0175_tmp IS NOT INITIAL.
                  APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
                ENDIF.

                CLEAR: wl_zlest0175_tmp.

                wl_zlest0175_tmp-id_tmp = v_cont_reg_tmp.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = _node_valor
                  IMPORTING
                    output = wl_zlest0175_tmp-nfenum.

              WHEN 'SerieNF'.
                wl_zlest0175_tmp-series = _node_valor.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = wl_zlest0175_tmp-series
                  IMPORTING
                    output = wl_zlest0175_tmp-series.

              WHEN 'DataEmissaoNF'.
                _node_valor = _node_valor(10).
                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
                wl_zlest0175_tmp-docdat = _node_valor.
              WHEN 'ChaveNFe'.
                wl_zlest0175_tmp-chave_nf = _node_valor.
                wl_zlest0175_tmp-model    = '55'.
                wl_zlest0175_tmp-stcd1    = wl_zlest0175_tmp-chave_nf+6(14).

              WHEN 'PesoDeclarado'.
                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
                CONDENSE _node_valor NO-GAPS.

                wl_zlest0175_tmp-peso_declarado = _node_valor.
              WHEN 'PesoDescarregado'.
                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
                CONDENSE _node_valor NO-GAPS.

                wl_zlest0175_tmp-peso_descarga = _node_valor.
            ENDCASE.
        ENDCASE.

        _xml_node_rodo_descar = _iterator_rodo_descar->get_next( ).
      ENDWHILE.

    ENDIF.

    IF wl_zlest0174_tmp IS NOT INITIAL.
      APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
    ENDIF.

    IF wl_zlest0175_tmp IS NOT INITIAL.
      APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
    ENDIF.

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0174_tmp>).
*      <FS_ZLEST0174_TMP>-PROTOCOLO_REC  = V_PROTOCOLO_REC.
*      <FS_ZLEST0174_TMP>-RECEBEDOR_CNPJ = V_RECEBEDOR_CNPJ.
*      <FS_ZLEST0174_TMP>-RECEBEDOR_NAME = V_RECEBEDOR_NAME.

      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0175_tmp>) WHERE id_tmp EQ <fs_zlest0174_tmp>-id_tmp.
        APPEND <fs_zlest0175_tmp> TO <fs_zlest0174_tmp>-notas.
      ENDLOOP.
    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0174_tmp[].


  ENDMETHOD.


  METHOD tratar_retorno_03.

    TYPES: BEGIN OF ty_tgg,
             in_protocolo_integracao(100),
             nr_cnpj_empresa(14),
             dt_hora_chegada(08),
             dt_abertura(08),
             dt_fechamento(08),
             nr_cpf_motorista(11),
             ds_placa(7),
             nr_peso_bruto                TYPE zde_peso_bruto,
             nr_peso_tara                 TYPE zde_peso_tara,
             nr_peso_liquido              TYPE zde_peso_liq_total,
             nr_codigo_chave_nfe          TYPE zde_chave_nf,
             nr_nota_fiscal               TYPE j_1bnfnum9,
             id_serie_nota_fiscal         TYPE j_1bseries,
             dt_emissao_nota_fiscal(08),
             nr_quantidade_nota_fiscal    TYPE zde_peso_descarga,
             nr_peso_subtotal             TYPE zde_peso_descarga,
           END OF ty_tgg.
* opening the connection
    DATA it_tgg TYPE  TABLE OF ty_tgg.
    DATA wa_tgg TYPE  ty_tgg.
    DATA conexao(15).

    DATA: c TYPE cursor.

    r_ok = abap_false.


    "Inicio USER STORY 74070 - Anderson Oenning / 02/05/2022




    IF sy-sysid = 'PRD'.
      conexao = 'SAP_XI_PRD'.
    ELSE.
      conexao = 'SAPXIQAS'.
    ENDIF.

    SELECT SINGLE *
      FROM dbcon INTO @DATA(wl_dbcon)
     WHERE con_name EQ @conexao.

    CHECK sy-subrc EQ 0.

    EXEC SQL.
      CONNECT TO :CONEXAO
    ENDEXEC.

    EXEC SQL.
      OPEN C FOR
      SELECT IN_PROTOCOLO_INTEGRACAO,
             NR_CNPJ_EMPRESA,
             to_char(DT_HORA_CHEGADA,'yyyymmdd') as DT_HORA_CHEGADA,
             to_char(DT_ABERTURA,'yyyymmdd') as DT_ABERTURA,
             to_char(DT_FECHAMENTO,'yyyymmdd') as DT_FECHAMENTO,
             NR_CPF_MOTORISTA,
             DS_PLACA,
             NR_PESO_BRUTO,
             NR_PESO_TARA,
             NR_PESO_LIQUIDO,
             NR_CODIGO_CHAVE_NFE,
             NR_NOTA_FISCAL,
             ID_SERIE_NOTA_FISCAL,
             to_char(DT_EMISSAO_NOTA_FISCAL,'yyyymmdd') as DT_EMISSAO_NOTA_FISCAL,
             NR_QUANTIDADE_NOTA_FISCAL,
             NR_PESO_SUBTOTAL
             from opus.mr_integracao_terceiro
             where to_char(DT_ABERTURA,'yyyymmdd') = :ME->AT_DT_CHEGADA
    ENDEXEC.

    DO.
      EXEC SQL.
        FETCH NEXT C INTO :WA_TGG
      ENDEXEC.
      APPEND wa_tgg TO it_tgg.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE C
    ENDEXEC.

*closing the connection
    EXEC SQL.
      DISCONNECT :CONEXAO
    ENDEXEC.




    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174     TYPE TABLE OF zlest0174,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name.

    CLEAR:   v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.
    CLEAR:   v_cont_reg_tmp,
             it_zlest0174_tmp[], wl_zlest0174_tmp,
             it_zlest0175_tmp[], wl_zlest0175_tmp.

    IF it_tgg[] IS NOT INITIAL.
      SELECT *
        FROM zlest0174
        INTO TABLE it_zlest0174
        FOR ALL ENTRIES IN it_tgg
        WHERE protocolo_rec = it_tgg-in_protocolo_integracao.
      SORT it_zlest0174 BY protocolo_rec.
    ELSE.
      RETURN.
    ENDIF.

    LOOP AT it_tgg INTO wa_tgg.
      READ TABLE it_zlest0174 INTO DATA(w_0174) WITH KEY protocolo_rec = wa_tgg-in_protocolo_integracao BINARY SEARCH.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      "Gerar ID Temporario
      ADD 1 TO v_cont_reg_tmp.
      wl_zlest0174_tmp-id_tmp                               = v_cont_reg_tmp.
      "
      wl_zlest0174_tmp-srv_integracao                       = me->at_srv_integracao..
      wl_zlest0174_tmp-protocolo_rec                        = wa_tgg-in_protocolo_integracao.
*      WL_ZLEST0174_TMP-RECEBEDOR_CNPJ                       = WA_TGG-NR_CNPJ_EMPRESA.
*      SELECT SINGLE NAME1
*                  INTO WL_ZLEST0174_TMP-RECEBEDOR_NAME
*                  FROM LFA1
*                  WHERE STCD1 = WL_ZLEST0174_TMP-RECEBEDOR_CNPJ.

      wl_zlest0174_tmp-sigla_terminal_transb                = me->at_sigla_terminal.
      wl_zlest0174_tmp-terminal_transb                      = 'Terminal TGG'.
*      WL_ZLEST0174_TMP-DS_TERMINAL_TRANSB                   = 'Terminal TGG'.
*      WL_ZLEST0174_TMP-CNPJ_TERMINAL_TRANSB                 = ''.
      wl_zlest0174_tmp-cnpj_terminal_transb                  = wa_tgg-nr_cnpj_empresa.
      SELECT SINGLE name1
                  INTO wl_zlest0174_tmp-ds_terminal_transb
                  FROM lfa1
                  WHERE stcd1 = wl_zlest0174_tmp-cnpj_terminal_transb.
      wl_zlest0174_tmp-nm_fantasia_terminal_destino          = 'Terminal TGG'.
      wl_zlest0174_tmp-rz_social_terminal_destino            = 'Terminal TGG'.
      wl_zlest0174_tmp-cnpj_terminal_destino                = ' '.
      wl_zlest0174_tmp-dt_chegada                           = wa_tgg-dt_hora_chegada.
      wl_zlest0174_tmp-dt_entrada                           = wa_tgg-dt_abertura.
      wl_zlest0174_tmp-dt_saida                             = wa_tgg-dt_fechamento.
      wl_zlest0174_tmp-nome_motorista                       = wa_tgg-nr_cpf_motorista.
      wl_zlest0174_tmp-placa                                = wa_tgg-ds_placa.
      wl_zlest0174_tmp-peso_bruto                           = wa_tgg-nr_peso_bruto.
      wl_zlest0174_tmp-peso_tara                            = wa_tgg-nr_peso_tara.
      wl_zlest0174_tmp-peso_liquido                         = wa_tgg-nr_peso_liquido.
      APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
      "
      wl_zlest0175_tmp-id_tmp                               = v_cont_reg_tmp.
      wl_zlest0175_tmp-chave_nf                             = wa_tgg-nr_codigo_chave_nfe.
      wl_zlest0175_tmp-model                                = '55'.
      wl_zlest0175_tmp-nfenum                               = wa_tgg-nr_nota_fiscal.
      wl_zlest0175_tmp-series                               = wa_tgg-id_serie_nota_fiscal.
      wl_zlest0175_tmp-stcd1                                = wl_zlest0175_tmp-chave_nf+6(14).
      wl_zlest0175_tmp-docdat                               = wa_tgg-dt_emissao_nota_fiscal.
      wl_zlest0175_tmp-docnum                               = ''.
      wl_zlest0175_tmp-peso_declarado                       = wa_tgg-nr_quantidade_nota_fiscal.
      wl_zlest0175_tmp-peso_descarga                        = wa_tgg-nr_peso_subtotal.
      APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.

    ENDLOOP.

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0174_tmp>).
      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0175_tmp>) WHERE id_tmp EQ <fs_zlest0174_tmp>-id_tmp.
        APPEND <fs_zlest0175_tmp> TO <fs_zlest0174_tmp>-notas.
      ENDLOOP.
    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0174_tmp[].

    r_ok = abap_true.



    "Fim USER STORY 74070 - Anderson Oenning / 02/05/2022
  ENDMETHOD.


  METHOD tratar_retorno_04.

*----------------------------------------------------------------*
*   Types
*----------------------------------------------------------------*
    TYPES: BEGIN OF ty_nota_fiscal,
             numero                   TYPE string,
             serie                    TYPE string,
             data_emissao             TYPE string,
             chave                    TYPE string,
             peso_declarado           TYPE string,
             peso_fiscal              TYPE string,
             peso_fisico              TYPE string,
             cnpj_remetente           TYPE string,
             nome_remetente           TYPE string,
             cidade_remetente_ibge    TYPE string,
             cidade_remetente         TYPE string,
             cnpj_destinatario        TYPE string,
             nome_destinatario        TYPE string,
             cidade_destinatario_ibge TYPE string,
             cidade_destinatario      TYPE string,
             chave_cte                TYPE string.
    TYPES: END   OF ty_nota_fiscal.

    DATA: t_nota_fiscal   TYPE TABLE OF ty_nota_fiscal.

    TYPES: BEGIN OF ty_grupo_neg,
             nome TYPE string,
             cnpj TYPE string.
    TYPES: END   OF ty_grupo_neg.

    TYPES: BEGIN OF ty_detalhes,
             descarga_id   TYPE string,
             terminal      TYPE string,
             produto       TYPE string,
             placa         TYPE string,
             data_chegada  TYPE string,
             peso_liquido  TYPE string,
             coleta_ibge   TYPE string,
             coleta        TYPE string,
             praca_ibge    TYPE string,
             praca         TYPE string,
             contrato      TYPE string,
             destino_cnpj  TYPE string,
             destino       TYPE string,
             terminal_cnpj TYPE string,
             terminal_desc TYPE string,
             nota_fiscal   LIKE t_nota_fiscal.
    TYPES: END   OF ty_detalhes.

    DATA: t_grupo_neg TYPE TABLE OF ty_grupo_neg.
    DATA: t_detalhes  TYPE TABLE OF ty_detalhes.

    TYPES: BEGIN OF ty_xml,
             grupo_negociador LIKE t_grupo_neg,
             detalhes         LIKE t_detalhes.
    TYPES: END   OF ty_xml.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: t_xml TYPE ty_xml.

    DATA: v_node_dados_recededor TYPE c,
          v_novo_veiculo         TYPE c,
          v_nova_nf              TYPE c,
          v_nome_node            TYPE string.

    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          wl_zlest0174     TYPE zlest0174,
          wl_zlest0175     TYPE zlest0175,
          t_zlest0175      TYPE zlest0175_t,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name.

    CLEAR: wl_zlest0174, wl_zlest0175, t_zlest0175[], t_xml,
           v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.

*------------------------------------------------------------------------------------------------*
* descerializa json
*------------------------------------------------------------------------------------------------*
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_cdata_retorno
      CHANGING
        data = t_xml.

*------------------------------------------------------------------------------------------------*
*   Ler Cabeçalho Retorno
*------------------------------------------------------------------------------------------------*
    CLEAR: v_novo_veiculo, v_cont_reg_tmp,
           it_zlest0174_tmp[], wl_zlest0174_tmp,
           it_zlest0175_tmp[], wl_zlest0175_tmp.

    LOOP AT t_xml-detalhes INTO DATA(w_detalhes).

      CLEAR: wl_zlest0174_tmp.

      "Gerar ID Temporario
      ADD 1 TO v_cont_reg_tmp.

      IF ( w_detalhes-data_chegada IS NOT INITIAL ) AND ( strlen( w_detalhes-data_chegada ) >= 10 ).
        CONCATENATE w_detalhes-data_chegada+6(4)
                  w_detalhes-data_chegada+3(2)
                  w_detalhes-data_chegada(2)
             INTO wl_zlest0174_tmp-dt_chegada.
      ENDIF.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1_term)
       WHERE stcd1 EQ @w_detalhes-terminal_cnpj.

      IF sy-subrc EQ 0.
        wl_zlest0174_tmp-terminal_transb            = lwa_lfa1_term-name1.
      ENDIF.

      wl_zlest0174_tmp-id_tmp                       = v_cont_reg_tmp.
      wl_zlest0174_tmp-sigla_terminal_transb        = me->at_sigla_terminal.
      wl_zlest0174_tmp-ds_terminal_transb           = w_detalhes-terminal_desc.
      wl_zlest0174_tmp-cnpj_terminal_transb         = w_detalhes-terminal_cnpj.
      wl_zlest0174_tmp-nm_fantasia_terminal_destino = w_detalhes-destino.
      wl_zlest0174_tmp-rz_social_terminal_destino   = w_detalhes-destino.
      wl_zlest0174_tmp-cnpj_terminal_destino        = w_detalhes-destino_cnpj.
      wl_zlest0174_tmp-dt_entrada                   = wl_zlest0174_tmp-dt_chegada.
      wl_zlest0174_tmp-peso_liquido                 = w_detalhes-peso_liquido.

      t_nota_fiscal[]                               = w_detalhes-nota_fiscal[].

*-------------------------------
* --- notas fiscais
*-------------------------------
      LOOP AT t_nota_fiscal INTO DATA(w_nota_fiscal).
        CLEAR wl_zlest0175_tmp.

        wl_zlest0175_tmp-id_tmp                     = v_cont_reg_tmp.
        wl_zlest0175_tmp-nfenum                     = w_nota_fiscal-numero.
        wl_zlest0175_tmp-series                     = w_nota_fiscal-serie.
        wl_zlest0175_tmp-chave_nf                   = w_nota_fiscal-chave.
        wl_zlest0175_tmp-model                      = '55'.
        wl_zlest0175_tmp-stcd1                      = w_nota_fiscal-chave+6(14).
        wl_zlest0175_tmp-peso_declarado             = w_nota_fiscal-peso_declarado.
        wl_zlest0175_tmp-peso_descarga              = w_nota_fiscal-peso_fisico.

        CONCATENATE w_nota_fiscal-data_emissao+6(4)
                    w_nota_fiscal-data_emissao+3(2)
                    w_nota_fiscal-data_emissao(2)
               INTO wl_zlest0175_tmp-docdat.

        APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
      ENDLOOP.

      APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
    ENDLOOP.

*    "XML
**   DATA(_xml_rodo_descarregados) = c_xml_return->find_node( EXPORTING name = 'ConsultaDescargaRodoviariaResult' ).
*    DATA(_xml_rodo_descarregados) = c_xml_return->find_node( EXPORTING name = v_nome_node ).
*
*    IF _xml_rodo_descarregados IS NOT INITIAL.
*
*      DATA(_iterator_rodo_descar) = _xml_rodo_descarregados->create_iterator( ).
*      DATA(_xml_node_rodo_descar) = _iterator_rodo_descar->get_next( ).
*
*      CLEAR: v_novo_veiculo, v_cont_reg_tmp,
*             it_zlest0174_tmp[], wl_zlest0174_tmp,
*             it_zlest0175_tmp[], wl_zlest0175_tmp.
*
*      WHILE _xml_node_rodo_descar IS NOT INITIAL.
*
*        CASE _xml_node_rodo_descar->get_type( ).
*          WHEN: if_ixml_node=>co_node_element.
*
*            DATA(_node_name)  = _xml_node_rodo_descar->get_name( ).
*            DATA(_node_valor) = _xml_node_rodo_descar->get_value( ).
*
*            CASE _node_name.
*              WHEN 'PlacaVeiculo'.
*                "Novo Veiculo
*                IF wl_zlest0174_tmp IS NOT INITIAL.
*                  APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
*                ENDIF.
*
*                CLEAR: wl_zlest0174_tmp.
*
*                "Gerar ID Temporario
*                ADD 1 TO v_cont_reg_tmp.
*
*                wl_zlest0174_tmp-id_tmp = v_cont_reg_tmp.
*                wl_zlest0174_tmp-cnpj_terminal_transb         = ' '.
*                wl_zlest0174_tmp-terminal_transb              = ' '.
*                wl_zlest0174_tmp-ds_terminal_transb           = ' '.
*
*                wl_zlest0174_tmp-sigla_terminal_transb = me->at_sigla_terminal.
*                IF wl_zlest0174_tmp-sigla_terminal_transb       = 'TRO'.
*                  "
*                  wl_zlest0174_tmp-terminal_transb              = 'RUMO MALHA NORTE S A'.
*                  wl_zlest0174_tmp-ds_terminal_transb           = 'TERMROO´- RONDONOPOLIS'.
*                  wl_zlest0174_tmp-cnpj_terminal_transb         = '24962466000136'.
*                  "
*                  wl_zlest0174_tmp-nm_fantasia_terminal_destino = 'TGG'.
*                  wl_zlest0174_tmp-rz_social_terminal_destino   = 'TGG'.
*                  wl_zlest0174_tmp-cnpj_terminal_destino        = ' '.
*                ELSE.
*                  EXIT.
*                ENDIF.
*
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0174_tmp-placa = _node_valor.
*
*              WHEN 'CnpjCpfFornecedor'.
*                "
*                me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
*                wl_zlest0174_tmp-recebedor_cnpj = _node_valor.
*                SELECT SINGLE name1
*                  INTO wl_zlest0174_tmp-recebedor_name
*                  FROM lfa1
*                  WHERE stcd1 = wl_zlest0174_tmp-recebedor_cnpj.
*
*              WHEN 'DataChegada'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0174_tmp-dt_chegada = _node_valor.
*
*              WHEN 'DataEntrada'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0174_tmp-dt_entrada = _node_valor.
*
*              WHEN 'DataSaida'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0174_tmp-dt_saida = _node_valor.
*                "
**              WHEN 'NomeMotorista'.
**                WL_ZLEST0174_TMP-NOME_MOTORISTA = _NODE_VALOR.
*
*              WHEN 'PesoBruto'.
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0174_tmp-peso_bruto = _node_valor.
*
*              WHEN 'PesoTara'.
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*                wl_zlest0174_tmp-peso_tara = _node_valor.
*
*              WHEN 'PesoLiquido'.
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*                wl_zlest0174_tmp-peso_liquido = _node_valor.
*
*              WHEN 'NumeroNF'.
*                "Nova NF
*                IF wl_zlest0175_tmp IS NOT INITIAL.
*                  APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
*                ENDIF.
*
*                CLEAR: wl_zlest0175_tmp.
*
*                wl_zlest0175_tmp-id_tmp = v_cont_reg_tmp.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = _node_valor
*                  IMPORTING
*                    output = wl_zlest0175_tmp-nfenum.
*
*              WHEN 'SerieNF'.
*                wl_zlest0175_tmp-series = _node_valor.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = wl_zlest0175_tmp-series
*                  IMPORTING
*                    output = wl_zlest0175_tmp-series.
*
*              WHEN 'DataEmissaoNF'.
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*                wl_zlest0175_tmp-docdat = _node_valor.
*              WHEN 'ChaveNFe'.
*                wl_zlest0175_tmp-chave_nf = _node_valor.
*                wl_zlest0175_tmp-model    = '55'.
*                wl_zlest0175_tmp-stcd1    = wl_zlest0175_tmp-chave_nf+6(14).
*
*              WHEN 'PesoDeclarado'.
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0175_tmp-peso_declarado = _node_valor.
*              WHEN 'PesoDescarregado'.
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0175_tmp-peso_descarga = _node_valor.
*            ENDCASE.
*        ENDCASE.
*
*        _xml_node_rodo_descar = _iterator_rodo_descar->get_next( ).
*      ENDWHILE.
*
*    ENDIF.
*
*    IF wl_zlest0174_tmp IS NOT INITIAL.
*      APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
*    ENDIF.
*
*    IF wl_zlest0175_tmp IS NOT INITIAL.
*      APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
*    ENDIF.

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0174_tmp>).
*      <FS_ZLEST0174_TMP>-PROTOCOLO_REC  = V_PROTOCOLO_REC.
*      <FS_ZLEST0174_TMP>-RECEBEDOR_CNPJ = V_RECEBEDOR_CNPJ.
*      <FS_ZLEST0174_TMP>-RECEBEDOR_NAME = V_RECEBEDOR_NAME.

      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0175_tmp>) WHERE id_tmp EQ <fs_zlest0174_tmp>-id_tmp.
        APPEND <fs_zlest0175_tmp> TO <fs_zlest0174_tmp>-notas.
      ENDLOOP.
    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0174_tmp[].

  ENDMETHOD.


  METHOD tratar_retorno_consulta.

    CASE me->at_srv_integracao.
      WHEN '01'. "Terminais VLI

        me->tratar_retorno_01(
          EXPORTING
            i_cdata_retorno = i_cdata_retorno
          CHANGING
            c_xml_return    = c_xml_return
        ).
      WHEN '02'. "Terminais BUNGE

        me->tratar_retorno_02(
          EXPORTING
            i_cdata_retorno = i_cdata_retorno
          CHANGING
            c_xml_return    = c_xml_return
        ).

      WHEN '03'. "Terminais TGG

        me->tratar_retorno_03(
          EXPORTING
            i_cdata_retorno = i_cdata_retorno
          CHANGING
            c_xml_return    = c_xml_return
        ).

      WHEN '04'. "Terminais RUMO

        me->tratar_retorno_04(
          EXPORTING
            i_cdata_retorno = i_cdata_retorno
          CHANGING
            c_xml_return    = c_xml_return
        ).
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
      WHEN '05'. "Terminais BRADO

        me->tratar_retorno_05(
          EXPORTING
            i_cdata_retorno = i_cdata_retorno
          CHANGING
            c_xml_return    = c_xml_return
        ).
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
**  Begin of " CS2022000810   #84550 FF   14.02.2023
      WHEN '06'. "Terminais ROCHA

        me->tratar_retorno_06(
          EXPORTING
            i_cdata_retorno = i_cdata_retorno
          CHANGING
            c_xml_return    = c_xml_return
        ).
** End of FF  14.02.2023

      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD tratar_xml.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN c_xml WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN c_xml WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN c_xml WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN c_xml WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN c_xml WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN c_xml WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN c_xml WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN c_xml WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN c_xml WITH 'o' IGNORING CASE.

  ENDMETHOD.


  METHOD get_list.
  ENDMETHOD.


  METHOD get_token_opus.

  ENDMETHOD.


  METHOD monta_xml_03.

    DATA: xvalor  TYPE string,
          v_c_aux TYPE string.

    CLEAR: r_xml.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.


    conc_xml    '{'.
    conc_xml       '"cnpj": "'.
    conc_xml                 me->at_cnpj_grupo_neg.
    conc_xml                                      '" ,'.
    conc_xml             '"periodo": {'.
    conc_xml               '"data_inicio": "'.
    v_c_aux = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2) && 'T00:00:00",'.
    conc_xml                v_c_aux .
    conc_xml               '"data_fim": "'.
    v_c_aux = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2) && 'T23:59:59"'.
    conc_xml                v_c_aux .
    conc_xml              '},'.
    conc_xml       '"terminal": "'.
    conc_xml                     me->at_sigla_terminal.
    conc_xml                                         '" ,'.
    conc_xml       '"busca_grupo_negociador": "S"'.
    conc_xml    '}'.

  ENDMETHOD.


  METHOD monta_xml_05.


    DATA: xvalor  TYPE string,
          v_c_aux TYPE string.

    CLEAR: r_xml.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.


    conc_xml    '{'.
    conc_xml       '"cnpj": "'.
    conc_xml                 me->at_cnpj_grupo_neg.
    conc_xml                                      '" ,'.
    conc_xml             '"periodo": {'.
    conc_xml               '"data_inicio": "'.
    v_c_aux = me->at_dt_chegada+6(2) && '/' && me->at_dt_chegada+4(2) && '/' && me->at_dt_chegada(4) && ' 00:00:00",'.
    conc_xml                v_c_aux .
    conc_xml               '"data_fim": "'.
    v_c_aux = me->at_dt_chegada+6(2) && '/' && me->at_dt_chegada+4(2) && '/' && me->at_dt_chegada(4) && ' 23:59:59"'.
    conc_xml                v_c_aux .
    conc_xml              '},'.
    conc_xml       '"terminal": "'.
    conc_xml                     me->at_sigla_terminal.
    conc_xml                                         '" ,'.
    conc_xml       '"busca_grupo_negociador": "S"'.
    conc_xml    '}'.

  ENDMETHOD.


  METHOD monta_xml_06.

    DATA: xvalor  TYPE string,
          v_c_aux TYPE string.

    CLEAR: r_xml.

    DEFINE conc_xml.
      CLEAR: xvalor.
      xvalor = &1.
      CONCATENATE r_xml xvalor INTO r_xml.
    END-OF-DEFINITION.

    DATA: lv_dt_ini TYPE zde_dt_inicial,
          lv_dt_fim TYPE zde_dt_final.

    lv_dt_ini = me->at_dt_inicial.
    lv_dt_fim = me->at_dt_final.

    conc_xml    '{'.
    conc_xml               '"dataInicial": "'.
    v_c_aux = lv_dt_ini(4) && '-' && lv_dt_ini+4(2) && '-' && lv_dt_ini+6(2) && 'T00:00:00",'.
    conc_xml                v_c_aux .
    conc_xml               '"dataFinal": "'.
    v_c_aux = lv_dt_fim(4) && '-' && lv_dt_fim+4(2) && '-' && lv_dt_fim+6(2) && 'T23:59:59"'.
    conc_xml                v_c_aux .
    conc_xml    '}'.


  ENDMETHOD.


  METHOD set_dt_final.

    me->at_dt_final = i_dt_final.

  ENDMETHOD.


  METHOD set_dt_inicial.

    me->at_dt_inicial = i_dt_inicial.

  ENDMETHOD.


  METHOD set_url_token.

  ENDMETHOD.


  METHOD tratar_retorno_05.

*----------------------------------------------------------------*
*   Types
*----------------------------------------------------------------*
    TYPES: BEGIN OF ty_notas,
             numero                   TYPE string,
             serie                    TYPE string,
             data_emissao             TYPE string,
             chave                    TYPE string,
             peso_declarado           TYPE string,
             peso_fiscal              TYPE string,
             peso_fisico              TYPE string,
             cnpj_remetente           TYPE string,
             nome_remetente           TYPE string,
             cidade_remetente_ibge    TYPE string,
             cidade_remetente         TYPE string,
             cnpj_destinatario        TYPE string,
             nome_destinatario        TYPE string,
             cidade_destinatario_ibge TYPE string,
             cidade_destinatario      TYPE string,
             chave_cte                TYPE string.
    TYPES: END   OF ty_notas.

    DATA: t_notas   TYPE TABLE OF ty_notas.



    TYPES: BEGIN OF ty_motorista,
             nome TYPE string.
    TYPES: END   OF ty_motorista.


    TYPES: BEGIN OF ty_result,
             descarga_id           TYPE string,
             terminal              TYPE string,
             terminal_desc         TYPE string,
             terminal_cnpj         TYPE string,
             produto               TYPE string,
             placa                 TYPE string,
             data_chegada          TYPE string,
             data_entrada          TYPE string,
             data_saida            TYPE string,
             coleta_ibge           TYPE string,
             coleta                TYPE string,
             praca_ibge            TYPE string,
             praca                 TYPE string,
             contrato              TYPE string,
             destino_cnpj          TYPE string,
             destino               TYPE string,
             destino_nome_fastasia TYPE string,
             destino_razao_social  TYPE string,
             peso_liquido          TYPE string,
             peso_bruto            TYPE string,
             peso_tara             TYPE string,
             notas                 LIKE t_notas,
             motorista             TYPE ty_motorista.
    TYPES: END   OF ty_result.


    DATA: t_result  TYPE TABLE OF ty_result.

***    TYPES: BEGIN OF ty_xml,
***             result LIKE t_result.
***    TYPES: END   OF ty_xml.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: t_xml TYPE TABLE OF ty_result.

    DATA: v_node_dados_recededor TYPE c,
          v_novo_veiculo         TYPE c,
          v_nova_nf              TYPE c,
          v_nome_node            TYPE string.

    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          wl_zlest0174     TYPE zlest0174,
          wl_zlest0175     TYPE zlest0175,
          t_zlest0175      TYPE zlest0175_t,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name.

    CLEAR: wl_zlest0174, wl_zlest0175, t_zlest0175[], t_xml,
           v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.

*------------------------------------------------------------------------------------------------*
* descerializa json
*------------------------------------------------------------------------------------------------*
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_cdata_retorno
      CHANGING
        data = t_xml.

*------------------------------------------------------------------------------------------------*
*   Ler Cabeçalho Retorno
*------------------------------------------------------------------------------------------------*
    CLEAR: v_novo_veiculo, v_cont_reg_tmp,
           it_zlest0174_tmp[],
           it_zlest0175_tmp[].

    DATA:lt_set_values  TYPE STANDARD TABLE OF  rgsb4.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'TERMINAIS_BRADO_L1_WS'
        no_descriptions = ' '
      TABLES
        set_values      = lt_set_values
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    LOOP AT t_xml INTO DATA(w_result).

      "Gerar ID Temporario
      ADD 1 TO v_cont_reg_tmp.

      APPEND INITIAL LINE TO it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_174>).

      <fs_174>-id_tmp                = v_cont_reg_tmp.
      <fs_174>-srv_integracao        = '05'.
      <fs_174>-protocolo_rec         = ''.
      <fs_174>-recebedor_cnpj        = ''.
      <fs_174>-recebedor_name        = ''.
      <fs_174>-sigla_terminal_transb = me->at_sigla_terminal.
      <fs_174>-terminal_transb       = me->at_sigla_terminal.

      READ TABLE lt_set_values INTO DATA(wa_set_values) WITH KEY from = <fs_174>-terminal_transb.
      IF sy-subrc  EQ 0.
        <fs_174>-ds_terminal_transb  =  wa_set_values-from.
      ENDIF.

      <fs_174>-ds_terminal_transb           = w_result-terminal_desc.
      <fs_174>-cnpj_terminal_transb         = w_result-terminal_cnpj.
      <fs_174>-nm_fantasia_terminal_destino = w_result-destino_nome_fastasia.
      <fs_174>-rz_social_terminal_destino   = w_result-destino_razao_social.
      <fs_174>-cnpj_terminal_destino        = w_result-destino_cnpj.
      <fs_174>-nome_motorista               = w_result-motorista-nome.


*      CONCATENATE w_result-data_chegada+6(4)
*                  w_result-data_chegada+3(2)
*                  w_result-data_chegada(2)
*               INTO <fs_174>-dt_chegada. "AAAA/MM/DD
*
*      CONCATENATE w_result-data_entrada+6(4)
*                  w_result-data_entrada+3(2)
*                  w_result-data_entrada(2)
*               INTO <fs_174>-dt_entrada. "AAAA/MM/DD


      CONCATENATE w_result-data_saida+6(4)
                  w_result-data_saida+3(2)
                  w_result-data_saida(2)
               INTO <fs_174>-dt_chegada. "AAAA/MM/DD

      CONCATENATE w_result-data_saida+6(4)
                  w_result-data_saida+3(2)
                  w_result-data_saida(2)
               INTO <fs_174>-dt_entrada. "AAAA/MM/DD

      CONCATENATE w_result-data_saida+6(4)
                  w_result-data_saida+3(2)
                  w_result-data_saida(2)
               INTO <fs_174>-dt_saida. "AAAA/MM/DD


      TRANSLATE: w_result-peso_bruto USING ',.',
                 w_result-peso_tara USING ',.',
                 w_result-peso_liquido USING ',.'.

**      <fs_174>-nome_motorista = w_result-
      <fs_174>-placa          = w_result-placa.
      <fs_174>-peso_bruto     = w_result-peso_bruto.
      <fs_174>-peso_tara      = w_result-peso_tara.
      <fs_174>-peso_liquido   = w_result-peso_liquido.
      <fs_174>-dt_registro    = sy-datum.
      <fs_174>-hr_registro    = sy-uzeit.
      t_notas[]         = w_result-notas[].

*-------------------------------
* --- notas fiscais
*-------------------------------
      LOOP AT t_notas INTO DATA(w_nota).
        TRANSLATE: w_nota-peso_fiscal USING ',.'.

        APPEND INITIAL LINE TO it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_175>).
        <fs_175>-id_tmp         = <fs_174>-id_tmp.
        <fs_175>-chave_nf       = w_nota-chave.
        <fs_175>-model          = w_nota-chave+20(2).
        <fs_175>-nfenum         = w_nota-numero.
        <fs_175>-series         = w_nota-serie.
        <fs_175>-stcd1          = w_nota-cnpj_remetente.
        <fs_175>-docdat         = |{ w_nota-data_emissao+6(4) }{ w_nota-data_emissao+3(2) }{ w_nota-data_emissao+0(2) }|.
        <fs_175>-docnum         = ''.
        <fs_175>-peso_declarado = w_nota-peso_fiscal.
        <fs_175>-peso_descarga  = w_nota-peso_fiscal.
      ENDLOOP.

    ENDLOOP.

    SORT it_zlest0175_tmp BY chave_nf.
    DELETE ADJACENT DUPLICATES FROM it_zlest0175_tmp COMPARING chave_nf.

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING <fs_174>.

      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING <fs_175> WHERE id_tmp EQ <fs_174>-id_tmp.
        APPEND <fs_175> TO <fs_174>-notas.
      ENDLOOP.
    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0174_tmp[].
  ENDMETHOD.


  METHOD tratar_retorno_06.

*----------------------------------------------------------------*
*   Types
*----------------------------------------------------------------*
    TYPES: BEGIN OF ty_nota_fiscal,
             numero                   TYPE string,
             serie                    TYPE string,
             data_emissao             TYPE string,
             chave                    TYPE string,
             peso_declarado           TYPE string,
             peso_fiscal              TYPE string,
             peso_fisico              TYPE string,
             cnpj_remetente           TYPE string,
             nome_remetente           TYPE string,
             cidade_remetente_ibge    TYPE string,
             cidade_remetente         TYPE string,
             cnpj_destinatario        TYPE string,
             nome_destinatario        TYPE string,
             cidade_destinatario_ibge TYPE string,
             cidade_destinatario      TYPE string,
             chave_cte                TYPE string.
    TYPES: END   OF ty_nota_fiscal.

    DATA: t_nota_fiscal   TYPE TABLE OF ty_nota_fiscal.

    TYPES: BEGIN OF ty_status,
             statuscode     TYPE string,
             statusmessages TYPE string.
    TYPES: END   OF ty_status.

    TYPES: BEGIN OF ty_result,
             tipo             TYPE string,
             modal            TYPE string,
             ticket           TYPE string,
             chaveacesso      TYPE string,
             cnpjfilial       TYPE string,
             datadescarga     TYPE string,
             cnpjemissor      TYPE string,
             numeronf         TYPE string,
             serienf          TYPE string,
             pesofiscal       TYPE string,
             pesodescarregado TYPE string,
             placa            TYPE string,
             nota_fiscal      LIKE t_nota_fiscal.
    TYPES: END   OF ty_result.

    DATA: t_status TYPE TABLE OF ty_status.
    DATA: t_result  TYPE TABLE OF ty_result.

    TYPES: BEGIN OF ty_xml,
             status LIKE t_status,
             result LIKE t_result.
    TYPES: END   OF ty_xml.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: t_xml TYPE ty_xml.

    DATA: v_node_dados_recededor TYPE c,
          v_novo_veiculo         TYPE c,
          v_nova_nf              TYPE c,
          v_nome_node            TYPE string.

    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          wl_zlest0174     TYPE zlest0174,
          wl_zlest0175     TYPE zlest0175,
          t_zlest0175      TYPE zlest0175_t,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name.

    CLEAR: wl_zlest0174, wl_zlest0175, t_zlest0175[], t_xml,
           v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.

*------------------------------------------------------------------------------------------------*
* descerializa json
*------------------------------------------------------------------------------------------------*
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_cdata_retorno
      CHANGING
        data = t_xml.

*------------------------------------------------------------------------------------------------*
*   Ler Cabeçalho Retorno
*------------------------------------------------------------------------------------------------*
    CLEAR: v_novo_veiculo, v_cont_reg_tmp,
           it_zlest0174_tmp[],
           it_zlest0175_tmp[].

    DATA:lt_set_values  TYPE STANDARD TABLE OF  rgsb4.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'TERMINAIS_ROCHA_L1_WS'
        no_descriptions = ' '
      TABLES
        set_values      = lt_set_values
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    LOOP AT t_xml-result INTO DATA(w_result) WHERE tipo = 'DESCARGA'
                                               AND modal = 'R'.

      "Gerar ID Temporario
      ADD 1 TO v_cont_reg_tmp.

      APPEND INITIAL LINE TO it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_174>).

      <fs_174>-id_tmp                = v_cont_reg_tmp.
      <fs_174>-srv_integracao        = '06'.
      <fs_174>-protocolo_rec         = ''.
      <fs_174>-recebedor_cnpj        = ''.
      <fs_174>-recebedor_name        = ''.
      <fs_174>-sigla_terminal_transb = me->at_sigla_terminal.
      <fs_174>-terminal_transb       = me->at_sigla_terminal.

      READ TABLE lt_set_values INTO DATA(wa_set_values) WITH KEY from = <fs_174>-terminal_transb.
      IF sy-subrc  EQ 0.
        <fs_174>-ds_terminal_transb  =  wa_set_values-from.
      ENDIF.

      <fs_174>-cnpj_terminal_transb          = wa_set_values-title(14).
      <fs_174>-nm_fantasia_terminal_destino  = wa_set_values-from.
      <fs_174>-rz_social_terminal_destino    = wa_set_values-from.
      <fs_174>-cnpj_terminal_destino         = wa_set_values-title(14).
      CONCATENATE w_result-datadescarga(4)
                  w_result-datadescarga+5(2)
                  w_result-datadescarga+8(2)
               INTO <fs_174>-dt_chegada. "AAAA/MM/DD

      <fs_174>-dt_entrada     = <fs_174>-dt_chegada.
      <fs_174>-dt_saida       = ''.
      <fs_174>-nome_motorista = ''.
      <fs_174>-placa          = w_result-placa.
      <fs_174>-peso_bruto     = ''.
      <fs_174>-peso_tara      = ''.
      <fs_174>-peso_liquido   = w_result-pesodescarregado.
      <fs_174>-dt_registro    = sy-datum.
      <fs_174>-hr_registro    = sy-uzeit.
      t_nota_fiscal[]         = w_result-nota_fiscal[].

*-------------------------------
* --- notas fiscais
*-------------------------------
      APPEND INITIAL LINE TO it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_175>).
      <fs_175>-id_tmp         = <fs_174>-id_tmp.
      <fs_175>-chave_nf       = w_result-chaveacesso.
      <fs_175>-model          = w_result-chaveacesso+20(2).
      <fs_175>-nfenum         = w_result-numeronf.
      <fs_175>-series         = w_result-serienf.
      <fs_175>-stcd1          = w_result-cnpjemissor.
      <fs_175>-docdat         = ''.
      <fs_175>-docnum         = ''.
      <fs_175>-peso_declarado = w_result-pesofiscal.
      <fs_175>-peso_descarga  = w_result-pesodescarregado.

    ENDLOOP.

    SORT it_zlest0175_tmp BY chave_nf.
    DELETE ADJACENT DUPLICATES FROM it_zlest0175_tmp COMPARING chave_nf.

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING <fs_174>.

      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING <fs_175> WHERE id_tmp EQ <fs_174>-id_tmp.
        APPEND <fs_175> TO <fs_174>-notas.
      ENDLOOP.
    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0174_tmp[].

  ENDMETHOD.
ENDCLASS.

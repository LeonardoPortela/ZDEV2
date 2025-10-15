CLASS ZCL_FERROVIARIO_SAIDA_WS DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS SET_SRV_INTEGRACAO
      IMPORTING
        !I_SRV_INTEGRACAO TYPE ZDE_SRV_INTEGRACAO .
    METHODS SET_DT_SAIDA
      IMPORTING
        !I_DT_SAIDA TYPE ZDE_DT_SAIDA .
    METHODS SET_SIGLA_TERMINAL
      IMPORTING
        !I_SIGLA_TERMINAL TYPE ZDE_SIGLA_TERMINAL OPTIONAL .
    METHODS SET_OPERACAO
      IMPORTING
        !I_OPERACAO TYPE ZDE_OPERACAO_RUMO OPTIONAL .
    METHODS SET_TERMINAL
      IMPORTING
        !I_TERMINAL TYPE ZDE_CNPJ_TERM_RUMO OPTIONAL .
    METHODS SET_DATA
      IMPORTING
        !I_DATA TYPE ZDE_DT_RUMO OPTIONAL .
    METHODS SET_HORA
      IMPORTING
        !I_HORA TYPE ZDE_HORA_RUMO OPTIONAL .
    METHODS SET_ESTACAO
      IMPORTING
        !I_SIGLA_ESTACAO TYPE ZDE_ESTACAO_RUMO OPTIONAL .
    METHODS SET_DOCUMENTACAO
      IMPORTING
        !I_DOCUMENTACAO TYPE ZDE_DOC_RUMO OPTIONAL .
    METHODS SET_GP_NEGOCIADOR
      IMPORTING
        !I_GP_NEGOCIADOR TYPE ZDE_GPNG_RUMO OPTIONAL .
    METHODS CONSULTAR
      IMPORTING
        !I_GRAVAR   TYPE CHAR01 OPTIONAL
      RETURNING
        VALUE(R_OK) TYPE CHAR01
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE
        HTTP_INVALID_STATE
        HTTP_PROCESSING_FAILED
        HTTP_INVALID_TIMEOUT
        ZCX_ENVIO .
    METHODS GRAVAR_DADOS
      RETURNING
        VALUE(R_GRAVOU) TYPE CHAR01 .
    METHODS GET_DADOS_CONSULTA
      EXPORTING
        !E_DADOS_CARREGAMENTO TYPE ZLEST0177_TMP_T
        !E_DADOS_NF           TYPE ZLEST0178_TMP_T .
    METHODS PREPARAR_DADOS_GRAVACAO
      RETURNING
        VALUE(R_OK) TYPE CHAR01 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA AT_SRV_INTEGRACAO TYPE ZDE_SRV_INTEGRACAO .
    DATA AT_DT_SAIDA TYPE ZDE_DT_SAIDA .
    DATA AT_SIGLA_TERMINAL TYPE ZDE_SIGLA_TERMINAL .
    DATA AT_AUTH_WS TYPE ZAUTH_WEBSERVICE .
    DATA AT_DADOS_CARREGAMENTO TYPE ZLEST0177_TMP_T .
    DATA AT_OPERACAO TYPE ZDE_OPERACAO_RUMO .
    DATA AT_TERMINAL TYPE ZDE_CNPJ_TERM_RUMO .
    DATA AT_DATA TYPE ZDE_DT_RUMO .
    DATA AT_HORA TYPE ZDE_HORA_RUMO .
    DATA AT_ESTACAO TYPE ZDE_ESTACAO_RUMO .
    DATA AT_DOCUMENTACAO TYPE ZDE_DOC_RUMO .
    DATA AT_GP_NEGOCIADOR TYPE ZDE_GPNG_RUMO .

    METHODS MONTA_XML
      RETURNING
        VALUE(R_XML) TYPE STRING .
    METHODS MONTA_XML_01
      RETURNING
        VALUE(R_XML) TYPE STRING .
    METHODS MONTA_XML_04
      RETURNING
        VALUE(R_XML) TYPE STRING .
    METHODS SET_URL
      RETURNING
        VALUE(R_URL) TYPE UI_SRC_URL .
    METHODS TRATAR_XML
      CHANGING
        !C_XML TYPE STRING .
    METHODS SET_HEADER_FIELDS
      CHANGING
        !C_IF_HTTP_CLIENT TYPE REF TO IF_HTTP_CLIENT .
    METHODS TRATAR_RETORNO_CONSULTA
      IMPORTING
        !I_CDATA_RETORNO TYPE STRING
      CHANGING
        !C_XML_RETURN    TYPE REF TO CL_XML_DOCUMENT .
    METHODS TRATAR_RETORNO_01
      IMPORTING
        !I_CDATA_RETORNO TYPE STRING
      CHANGING
        !C_XML_RETURN    TYPE REF TO CL_XML_DOCUMENT .
    METHODS TRATAR_RETORNO_04
      IMPORTING
        !I_CDATA_RETORNO TYPE STRING
      CHANGING
        !C_XML_RETURN    TYPE REF TO CL_XML_DOCUMENT .
    METHODS CONVERSION_CNPJ_INPUT
      CHANGING
        !I_CNPJ TYPE STRING .
ENDCLASS.



CLASS ZCL_FERROVIARIO_SAIDA_WS IMPLEMENTATION.


  METHOD CONSULTAR.

    DATA: HTTP_CLIENT     TYPE REF TO IF_HTTP_CLIENT,
          XML_RETURN      TYPE REF TO CL_XML_DOCUMENT,
          RETURN_CODE     TYPE I,
          VL_INI_POS      TYPE I,
          V_CDATA_RETORNO TYPE STRING,
          V_XML           TYPE STRING,
          V_URL           TYPE UI_SRC_URL,
          LW_FERRO_VLI    TYPE STRING,           "*-CS2024000593-21.11.2024-JT-#157800-inicio
          V_MSG_SHOW      TYPE C LENGTH 200.

    R_OK = ABAP_FALSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = TEXT-001.

    V_URL = ME->SET_URL( ).

    CHECK V_URL IS NOT INITIAL.

    V_XML = ME->MONTA_XML( ).

*-CS2024000593-21.11.2024-JT-#157800-inicio
*   CHECK v_xml IS NOT INITIAL.
    IF V_XML IS NOT INITIAL.
      ME->TRATAR_XML( CHANGING C_XML = V_XML ).
    ENDIF.
*-CS2024000593-21.11.2024-JT-#157800-fim

    "Call service
    CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
      EXPORTING
        URL                = CONV #( V_URL )
        SSL_ID             = 'DFAULT'
      IMPORTING
        CLIENT             = HTTP_CLIENT
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4.

    ME->SET_HEADER_FIELDS(  CHANGING C_IF_HTTP_CLIENT = HTTP_CLIENT ).

    CALL METHOD HTTP_CLIENT->REQUEST->SET_CDATA
      EXPORTING
        DATA   = V_XML
        OFFSET = 0
        LENGTH = STRLEN( V_XML ).

    CALL METHOD HTTP_CLIENT->SEND
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4.

    CASE SY-SUBRC.
      WHEN 1.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S021 WITH | { V_URL } (Send) | RAISING HTTP_COMMUNICATION_FAILURE.
        EXIT.
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S022 WITH | { V_URL } (Send) | RAISING HTTP_INVALID_STATE.
        EXIT.
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S023 WITH | { V_URL } (Send) | RAISING HTTP_PROCESSING_FAILED.
        EXIT.
      WHEN 4.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S024 WITH | { V_URL } (Send) | RAISING HTTP_INVALID_TIMEOUT.
        EXIT.
    ENDCASE.

    CALL METHOD HTTP_CLIENT->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3.

    CASE SY-SUBRC.
      WHEN 1.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S021 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_COMMUNICATION_FAILURE.
        EXIT.
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S023 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_INVALID_STATE.
        EXIT.
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S024 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_PROCESSING_FAILED.
        EXIT.
    ENDCASE.

    "//Check return content
    CREATE OBJECT XML_RETURN.

    CALL METHOD XML_RETURN->PARSE_STRING
      EXPORTING
        STREAM = HTTP_CLIENT->RESPONSE->GET_CDATA( ).

    HTTP_CLIENT->RESPONSE->GET_STATUS( IMPORTING CODE = RETURN_CODE ).

    V_CDATA_RETORNO = HTTP_CLIENT->RESPONSE->GET_CDATA( ).

    IF RETURN_CODE NE '200'.
      CLEAR: VL_INI_POS.

      IF VL_INI_POS IS INITIAL.
        FIND '(400)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '400'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(401)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '401'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(403)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '403'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(404)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '404'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(422)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '422'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(500)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '500'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(503)' IN V_CDATA_RETORNO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '503'.
        ENDIF.
      ENDIF.

      CASE RETURN_CODE.
        WHEN 400.
          MESSAGE '(WS)-Requisição mal formatada!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 401.
          MESSAGE '(WS)-Requisição requer autenticação!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 403.
          MESSAGE '(WS)-Requisição não autorizada!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 404.
          MESSAGE '(WS)-Registro não encontrado!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 422.
          MESSAGE '(WS)-Erro de negócio!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 500.
          MESSAGE '(WS)-Erro interno do servidor!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 503.
          MESSAGE '(WS)-Serviço indisponível!' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN OTHERS.
          MESSAGE '(WS)-Consulta não realizada!' TYPE 'S' RAISING ZCX_ENVIO.
      ENDCASE.

      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '000Z' IN V_CDATA_RETORNO WITH '000M'. "// BUG-186206 WBARBOSA 25/07/2025

    ME->TRATAR_RETORNO_CONSULTA(
      EXPORTING
        I_CDATA_RETORNO = V_CDATA_RETORNO
      CHANGING
        C_XML_RETURN    = XML_RETURN
    ).

    R_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CONVERSION_CNPJ_INPUT.

    REPLACE ALL OCCURRENCES OF '.' IN I_CNPJ WITH SPACE.
    REPLACE ALL OCCURRENCES OF '/' IN I_CNPJ WITH SPACE.
    REPLACE ALL OCCURRENCES OF '-' IN I_CNPJ WITH SPACE.
    CONDENSE I_CNPJ NO-GAPS.

  ENDMETHOD.


  METHOD GET_DADOS_CONSULTA.

    E_DADOS_CARREGAMENTO = ME->AT_DADOS_CARREGAMENTO.

  ENDMETHOD.


  METHOD GRAVAR_DADOS.

    DATA: WL_ZLEST0177 TYPE ZLEST0177,
          IT_ZLEST0177 TYPE TABLE OF ZLEST0177,
          WL_ZLEST0178 TYPE ZLEST0178,
          IT_ZLEST0178 TYPE TABLE OF ZLEST0178.

    DATA: IT_ZLEST0178_AUX TYPE TABLE OF ZDE_ZLEST0178_TMP,
          WA_ZLEST0178_AUX TYPE ZDE_ZLEST0178_TMP,
          IT_ZLEST0177_AUX TYPE TABLE OF ZLEST0177,
          WA_ZLEST0177_AUX TYPE ZLEST0177.

    DATA: VG_INPUT TYPE CHAR01.

    R_GRAVOU = ABAP_FALSE.

    IF ( ME->AT_DADOS_CARREGAMENTO[] IS INITIAL ).
      MESSAGE S026.
      RETURN.
    ENDIF.

    DATA(_OK) = ME->PREPARAR_DADOS_GRAVACAO( ).

    CHECK _OK EQ ABAP_TRUE.

    CLEAR: IT_ZLEST0177[], IT_ZLEST0178[].

*-CS2024000593-21.11.2024-JT-#157800-inicio
    FREE: IT_ZLEST0178_AUX.
    LOOP AT ME->AT_DADOS_CARREGAMENTO ASSIGNING FIELD-SYMBOL(<WS_DESCARGA>).
      APPEND LINES OF <WS_DESCARGA>-NOTAS[] TO IT_ZLEST0178_AUX[].
    ENDLOOP.

    SELECT *
      FROM ZLEST0178 AS A
      INTO TABLE @DATA(TG_ZLEST0178)
       FOR ALL ENTRIES IN @IT_ZLEST0178_AUX[]
     WHERE CHAVE_NF EQ @IT_ZLEST0178_AUX-CHAVE_NF.

    LOOP AT ME->AT_DADOS_CARREGAMENTO INTO DATA(_WL_CARREGAMENTO).
      CLEAR: VG_INPUT.
      LOOP AT _WL_CARREGAMENTO-NOTAS INTO DATA(_WL_NF).
        READ TABLE TG_ZLEST0178 INTO DATA(WS_ZLES00178) WITH KEY CHAVE_NF = _WL_NF-CHAVE_NF.
        IF SY-SUBRC NE 0.
          CLEAR: WL_ZLEST0178.
          MOVE-CORRESPONDING _WL_NF TO WL_ZLEST0178.
          APPEND WL_ZLEST0178 TO IT_ZLEST0178.
          CLEAR: WS_ZLES00178.
          VG_INPUT = ABAP_TRUE.
        ENDIF.
      ENDLOOP.

      IF VG_INPUT EQ ABAP_TRUE.
        CLEAR: WL_ZLEST0177.
        MOVE-CORRESPONDING _WL_CARREGAMENTO TO WL_ZLEST0177.
        APPEND WL_ZLEST0177 TO IT_ZLEST0177.
      ENDIF.
    ENDLOOP.

*    LOOP AT me->at_dados_carregamento INTO DATA(_wl_carregamento).
*      CLEAR: wl_zlest0177.
*      MOVE-CORRESPONDING _wl_carregamento TO wl_zlest0177.
*      APPEND wl_zlest0177 TO it_zlest0177.
*
*      LOOP AT _wl_carregamento-notas INTO DATA(_wl_nf).
*        CLEAR: wl_zlest0178.
*        MOVE-CORRESPONDING _wl_nf TO wl_zlest0178.
*        APPEND wl_zlest0178 TO it_zlest0178.
*      ENDLOOP.
*    ENDLOOP.
*-CS2024000593-21.11.2024-JT-#157800-fim

    IF ( IT_ZLEST0177[] IS NOT INITIAL ).
      MODIFY ZLEST0177 FROM TABLE IT_ZLEST0177.
    ENDIF.

    IF ( IT_ZLEST0178[] IS NOT INITIAL ).
      MODIFY ZLEST0178 FROM TABLE IT_ZLEST0178.
    ENDIF.

    R_GRAVOU = ABAP_TRUE.


  ENDMETHOD.


  METHOD MONTA_XML.

    CASE ME->AT_SRV_INTEGRACAO.
      WHEN '01'. "Terminais VLI
*       r_xml = me->monta_xml_01( ).  "*-CS2024000593-21.11.2024-JT-#157800-inicio
*** PBI - 52204 - Inicio
      WHEN '04'. "Terminais RUMO
        R_XML = ME->MONTA_XML_04( ).
*** PBI - 52204 - Fim
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF R_XML IS INITIAL  AND ME->AT_SRV_INTEGRACAO <> '01'. "*-CS2024000593-21.11.2024-JT-#157800-inicio
      MESSAGE S020.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD MONTA_XML_01.

    DATA: XVALOR  TYPE STRING,
          V_C_AUX TYPE STRING.

    CLEAR: R_XML.

    DEFINE CONC_XML.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE R_XML XVALOR INTO R_XML.
    END-OF-DEFINITION.


    CONC_XML    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'.
    CONC_XML       '<SOAP-ENV:Body>'.
    CONC_XML          '<Consulta xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
    CONC_XML             '<Login>'.
    CONC_XML                ME->AT_AUTH_WS-USERNAME.
    CONC_XML             '</Login>'.
    CONC_XML             '<Senha>'.
    CONC_XML                ME->AT_AUTH_WS-PASSWORD.
    CONC_XML             '</Senha>'.
    CONC_XML             '<CodigoTerminal>'.
    CONC_XML                ME->AT_SIGLA_TERMINAL.
    CONC_XML             '</CodigoTerminal>'.

    CONC_XML             '<Data>'.
    V_C_AUX = ME->AT_DT_SAIDA(4) && '-' && ME->AT_DT_SAIDA+4(2) && '-' && ME->AT_DT_SAIDA+6(2).
    CONC_XML                V_C_AUX.
    CONC_XML             '</Data>'.

    CONC_XML          '</Consulta>'.
    CONC_XML       '</SOAP-ENV:Body>'.
    CONC_XML    '</SOAP-ENV:Envelope>'.

  ENDMETHOD.


  METHOD MONTA_XML_04.

    DATA: XVALOR  TYPE STRING,
          V_C_AUX TYPE STRING,
          V_DATA  TYPE STRING.

    CLEAR: R_XML.

    CONCATENATE ME->AT_DT_SAIDA(4)   '-'
                ME->AT_DT_SAIDA+4(2) '-'
                ME->AT_DT_SAIDA+6(2)
           INTO V_DATA.


    DEFINE CONC_CDATA.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE r_xml XVALOR INTO r_xml.
    END-OF-DEFINITION.

    CONC_CDATA '{'.
    CONC_CDATA '"operacao":"'.
    CONC_CDATA    ME->AT_OPERACAO.
    CONC_CDATA '",'.
    CONC_CDATA '"cnpj":"'.
    CONC_CDATA  ME->AT_TERMINAL.
    CONC_CDATA '",'.
    CONC_CDATA '"dia":"'.
    CONC_CDATA  V_DATA. "me->at_dt_saida.
    CONC_CDATA '",'.
    CONC_CDATA '"hora":"'.
    CONC_CDATA '",'.
    CONC_CDATA '"estacao":"'.
    CONC_CDATA  ME->AT_ESTACAO.
    CONC_CDATA '",'.
    CONC_CDATA '"com_documentacao":"'.
    CONC_CDATA  ME->AT_DOCUMENTACAO.
    CONC_CDATA '",'.
    CONC_CDATA '"busca_grupo_negociador":"'.
    CONC_CDATA  ME->AT_GP_NEGOCIADOR.
    CONC_CDATA '",'.
    CONC_CDATA '"data_selecao":"FATURAMENTO"'.
    CONC_CDATA  '}'.

  ENDMETHOD.


  METHOD PREPARAR_DADOS_GRAVACAO.

    R_OK = ABAP_FALSE.

    LOOP AT ME->AT_DADOS_CARREGAMENTO ASSIGNING FIELD-SYMBOL(<FS_DADOS_CARREGAMENTO>).

      IF <FS_DADOS_CARREGAMENTO>-ID_REGISTRO IS INITIAL.
        "Gerar ID registro
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZREG_L2_WS'
          IMPORTING
            NUMBER                  = <FS_DADOS_CARREGAMENTO>-ID_REGISTRO
          EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.

        IF ( SY-SUBRC NE 0 ) OR ( <FS_DADOS_CARREGAMENTO>-ID_REGISTRO IS INITIAL ).
          MESSAGE S025.
          RETURN.
        ENDIF.
      ENDIF.

      LOOP AT <FS_DADOS_CARREGAMENTO>-NOTAS ASSIGNING FIELD-SYMBOL(<FS_DADOS_NF>).
        <FS_DADOS_NF>-ID_REGISTRO = <FS_DADOS_CARREGAMENTO>-ID_REGISTRO.
      ENDLOOP.

      <FS_DADOS_CARREGAMENTO>-SRV_INTEGRACAO = ME->AT_SRV_INTEGRACAO.
      <FS_DADOS_CARREGAMENTO>-DT_REGISTRO = SY-DATUM.
      <FS_DADOS_CARREGAMENTO>-HR_REGISTRO = SY-UZEIT.

    ENDLOOP.

    R_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_DATA.
    ME->AT_DATA = I_DATA.
  ENDMETHOD.


  METHOD SET_DOCUMENTACAO.
    ME->AT_DOCUMENTACAO = I_DOCUMENTACAO.
  ENDMETHOD.


  METHOD SET_DT_SAIDA.

    ME->AT_DT_SAIDA = I_DT_SAIDA.

  ENDMETHOD.


  METHOD SET_ESTACAO.
    ME->AT_ESTACAO = I_SIGLA_ESTACAO.
  ENDMETHOD.


  METHOD SET_GP_NEGOCIADOR.
    ME->AT_GP_NEGOCIADOR = I_GP_NEGOCIADOR.
  ENDMETHOD.


  METHOD SET_HEADER_FIELDS.

    DATA: EX_WEBSERVICE_TOKEN TYPE REF TO ZCL_WEBSERVICE.

    DATA: LV_VALUE_USER TYPE STRING.
    DATA: LV_VALUE_PASS TYPE STRING.
    DATA: LV_VALUE_GEST TYPE STRING.
    DATA: LV_VALUE_AMB  TYPE STRING.

    DATA: V_URL               TYPE UI_SRC_URL.
    DATA: V_TOKEN             TYPE STRING.
    DATA: LW_DATA             TYPE STRING.        ""*-CS2024000593-21.11.2024-JT-#157800-inicio
    DATA: LC_TEXTO_VLI        TYPE C LENGTH 200.  "*-CS2024000593-21.11.2024-JT-#157800-inicio

    TYPES BEGIN OF TY_RETORNO_MSG.
    TYPES MESSAGE TYPE STRING.
    TYPES END OF TY_RETORNO_MSG.

    TYPES BEGIN OF TY_JSON_RETORNO.
    TYPES: TOKEN        TYPE STRING.
    TYPES: AMBIENTE     TYPE STRING.
    TYPES END OF TY_JSON_RETORNO.

    DATA: LC_MENSAGEM  TYPE TY_RETORNO_MSG,
          LC_RETORNO   TYPE TY_JSON_RETORNO,
          LW_TOKEN     TYPE STRING,                         "*-CS2024000593-21.11.2024-JT-#157800-inicio
          LC_TOKEN_VLI TYPE REF TO ZCL_TOKEN_VLI_NEW.       "*-CS2024000593-21.11.2024-JT-#157800-inicio

    CASE ME->AT_SRV_INTEGRACAO.

*-CS2024000593-21.11.2024-JT-#157800-inicio
      WHEN '01'.
        CREATE OBJECT LC_TOKEN_VLI.

        TRY .
            LW_TOKEN = LC_TOKEN_VLI->GET_TOKEN( 'CARGA_FERROVIA_VLI' ).

          CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
            MESSAGE ID EX_INTEGRA->MSGID TYPE 'S' NUMBER EX_INTEGRA->MSGNO WITH EX_INTEGRA->MSGV1 EX_INTEGRA->MSGV2 EX_INTEGRA->MSGV3 EX_INTEGRA->MSGV4 DISPLAY LIKE 'E'.
            RETURN.
          CATCH ZCX_ERROR INTO DATA(EX_ERROR).
            MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = 'access_token'
            VALUE = LW_TOKEN.

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = 'client_id'
            VALUE = CONV #( ME->AT_AUTH_WS-USERNAME ).

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = 'edi-token'
            VALUE = CONV #( ME->AT_AUTH_WS-ADD01 ).

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = '~request_method'
            VALUE = 'GET'.

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = '~server_protocol'
            VALUE = 'HTTP/1.1'.
*-CS2024000593-21.11.2024-JT-#157800-fim

*** PBI 52204 - Inicio
      WHEN '04'.
        DATA: LV_VALUE      TYPE STRING.

*-US 160340-04.12.2024-#160340-JT-inicio
        DATA(LT_HEADER_AUTH) = ZCL_GESTAO_TOKEN=>GET_TOKEN_VALIDO( I_ID_TOKEN = '0009' ).
        IF LT_HEADER_AUTH[] IS NOT INITIAL.
          READ TABLE LT_HEADER_AUTH INTO DATA(LW_HEADER_AUTH) INDEX 1.
          SPLIT LW_HEADER_AUTH-VALUE AT ABAP_OFF INTO DATA(LC_CONSTANT) V_TOKEN.

          CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = '~request_method'
              VALUE = 'POST'.

          CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = '~server_protocol'
              VALUE = 'HTTP/1.1'.

          CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = 'Content-Type'
              VALUE = 'application/json'.

          CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = 'Authorization'
              VALUE = V_TOKEN.

          RETURN.
        ENDIF.
*-US 160340-04.12.2024-#160340-JT-fim

        SELECT SINGLE * INTO @DATA(LS_ZAUTH_WEBSERVICE)
         FROM ZAUTH_WEBSERVICE
         WHERE SERVICE = 'RUMO_TOKEN_L2'.

        IF SY-SUBRC = 0.

          CREATE OBJECT EX_WEBSERVICE_TOKEN.
          EX_WEBSERVICE_TOKEN->AT_URL = LS_ZAUTH_WEBSERVICE-URL.
          DATA(VAR_HTTP) = EX_WEBSERVICE_TOKEN->URL( I_URL = CONV #( EX_WEBSERVICE_TOKEN->AT_URL ) ).
          EX_WEBSERVICE_TOKEN->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

          CALL METHOD VAR_HTTP->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = '~request_method'
              VALUE = 'POST'.

          CALL METHOD VAR_HTTP->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = '~server_protocol'
              VALUE = 'HTTP/1.1'.

          CALL METHOD VAR_HTTP->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = 'Content-Type'
              VALUE = 'application/json'.

          LV_VALUE_USER = LS_ZAUTH_WEBSERVICE-USERNAME.
          VAR_HTTP->REQUEST->SET_FORM_FIELD( EXPORTING NAME = 'username' VALUE = LV_VALUE_USER ).

          LV_VALUE_PASS = LS_ZAUTH_WEBSERVICE-PASSWORD.
          VAR_HTTP->REQUEST->SET_FORM_FIELD( EXPORTING NAME = 'password' VALUE = LV_VALUE_PASS ).

          LV_VALUE_GEST = LS_ZAUTH_WEBSERVICE-ADD01.
          VAR_HTTP->REQUEST->SET_FORM_FIELD( EXPORTING NAME = 'cod_gestao' VALUE = LV_VALUE_GEST ).

          DATA TEXT_FORM TYPE STRING.
          TEXT_FORM = '{"username":"' && LV_VALUE_USER && '","password":"' && LV_VALUE_PASS && '","cod_gestao":' && LV_VALUE_GEST && '}'.

          EX_WEBSERVICE_TOKEN->ZIF_WEBSERVICE~CONSULTAR(
            EXPORTING
              I_HTTP                     = VAR_HTTP
              I_XML                      = TEXT_FORM
            IMPORTING
              E_CODE                     = DATA(E_CODE)
              E_REASON                   = DATA(E_REASON)
            RECEIVING
              E_RESULTADO                = DATA(TOKEN_RETORNO)
            EXCEPTIONS
              HTTP_COMMUNICATION_FAILURE = 1
              HTTP_INVALID_STATE         = 2
              HTTP_PROCESSING_FAILED     = 3
              HTTP_INVALID_TIMEOUT       = 4
              OTHERS                     = 5 ).

          CASE SY-SUBRC.
            WHEN 1 OR 5.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
            WHEN 2.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            WHEN 3.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            WHEN 4.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDCASE.

          IF E_CODE NE 200.

            DATA: LC_TEXTO TYPE C LENGTH 200.

            CALL METHOD /UI2/CL_JSON=>DESERIALIZE
              EXPORTING
                JSON = TOKEN_RETORNO
              CHANGING
                DATA = LC_MENSAGEM.

            LC_TEXTO = LC_MENSAGEM-MESSAGE.
            SY-MSGV1 = LC_TEXTO+000(50).
            SY-MSGV2 = LC_TEXTO+050(50).
            SY-MSGV3 = LC_TEXTO+100(50).
            SY-MSGV4 = LC_TEXTO+150(50).

            MESSAGE I028(ZSIMETRYA) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO DATA(LWA_MESSAGE_TOKEN).

            CONCATENATE 'Rumo Token:' LWA_MESSAGE_TOKEN INTO LWA_MESSAGE_TOKEN SEPARATED BY SPACE.

            MESSAGE LWA_MESSAGE_TOKEN TYPE 'I'.

          ELSE.

            CALL METHOD /UI2/CL_JSON=>DESERIALIZE
              EXPORTING
                JSON = TOKEN_RETORNO
              CHANGING
                DATA = LC_RETORNO.

            V_TOKEN = LC_RETORNO-TOKEN.

            CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
              EXPORTING
                NAME  = '~request_method'
                VALUE = 'POST'.

            CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
              EXPORTING
                NAME  = '~server_protocol'
                VALUE = 'HTTP/1.1'.

            CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
              EXPORTING
                NAME  = 'Content-Type'
                VALUE = 'application/json'.

            CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
              EXPORTING
                NAME  = 'Authorization'
                VALUE = V_TOKEN.

*-US 160340-04.12.2024-#160340-JT-inicio
            ZCL_GESTAO_TOKEN=>UPDATE_TOKEN( I_ID_TOKEN = '0009' I_ACCESS_TOKEN = LC_RETORNO-TOKEN  I_TOKEN_TYPE = 'Basic' ).
*-US 160340-04.12.2024-#160340-JT-fim

          ENDIF.
        ENDIF.
*** PBI 52204 - Fim
      WHEN OTHERS.

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = '~request_method'
            VALUE = 'POST'.

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = '~server_protocol'
            VALUE = 'HTTP/1.1'.

        CALL METHOD C_IF_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = 'Content-Type'
            VALUE = 'text/xml; charset=UTF-8'.

    ENDCASE.
  ENDMETHOD.


  METHOD SET_HORA.
    ME->AT_HORA = I_HORA.
  ENDMETHOD.


  METHOD SET_OPERACAO.
    ME->AT_OPERACAO = I_OPERACAO.
  ENDMETHOD.


  METHOD SET_SIGLA_TERMINAL.

    ME->AT_SIGLA_TERMINAL = I_SIGLA_TERMINAL.

  ENDMETHOD.


  METHOD SET_SRV_INTEGRACAO.

    ME->AT_SRV_INTEGRACAO = I_SRV_INTEGRACAO.

  ENDMETHOD.


  METHOD SET_TERMINAL.
    ME->AT_TERMINAL = I_TERMINAL.
  ENDMETHOD.


  METHOD SET_URL.

    CLEAR: R_URL.

    CASE ME->AT_SRV_INTEGRACAO.
      WHEN '01'. "Terminais VLI

        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO ME->AT_AUTH_WS
         WHERE SERVICE = 'CARREGAMENTO_FERRO_SRV_INT_01'.

*-CS2024000593-18.11.2024-jt-#146078-inicio
        IF SY-SUBRC EQ 0.
          R_URL  = ME->AT_AUTH_WS-URL.
          DATA(LW_DATA) = ME->AT_DT_SAIDA(4) && '-' && ME->AT_DT_SAIDA+4(2) && '-' && ME->AT_DT_SAIDA+6(2).  "*-CS2024000593-18.11.2024-jt-#146078-inicio
          R_URL = |{ R_URL }?dataFaturamento={ LW_DATA }&terminal={ ME->AT_SIGLA_TERMINAL }|.
        ENDIF.
*-CS2024000593-18.11.2024-jt-#146078-fim

*** PBI - 52204 - Inicio
      WHEN '04'. "Terminais RUMO
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO ME->AT_AUTH_WS
         WHERE SERVICE = 'CARREGAMENTO_FERRO_SRV_INT_04'.

        IF SY-SUBRC EQ 0.
          R_URL  = ME->AT_AUTH_WS-URL.
        ENDIF.
*** PBI - 52204 - Fim
      WHEN OTHERS.
        MESSAGE S019 WITH ME->AT_SRV_INTEGRACAO.
        RETURN.
    ENDCASE.

    IF R_URL IS INITIAL.
      MESSAGE S018 WITH ME->AT_SRV_INTEGRACAO.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD TRATAR_RETORNO_01.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: V_NODE_DADOS_RECEBEDOR TYPE C,
          V_NOVO_VEICULO         TYPE C,
          V_NOVA_NF              TYPE C.


    DATA: RETURN_CODE      TYPE I,
          E_RESULTADO      TYPE STRING,
          V_NODE_NAME      TYPE STRING,
          V_NODE_VALUE     TYPE STRING,
          V_MSG_SHOW       TYPE C LENGTH 200,

          IT_ZLEST0177_TMP TYPE TABLE OF ZDE_ZLEST0177_TMP,
          WL_ZLEST0177_TMP TYPE ZDE_ZLEST0177_TMP,

          IT_ZLEST0178_TMP TYPE TABLE OF ZDE_ZLEST0178_TMP,
          WL_ZLEST0178_TMP TYPE ZDE_ZLEST0178_TMP,

          WL_ZLEST0177     TYPE ZLEST0177,
          WL_ZLEST0178     TYPE ZLEST0178,
          T_ZLEST0178      TYPE ZLEST0178_T,

          V_CONT_REG_TMP   TYPE ZLEST0177-ID_REGISTRO,

          V_PROTOCOLO_REC  TYPE ZLEST0177-PROTOCOLO_REC,
          V_RECEBEDOR_CNPJ TYPE ZLEST0177-RECEBEDOR_CNPJ,
          V_RECEBEDOR_NAME TYPE ZLEST0177-RECEBEDOR_NAME,

          LT_DADOS         TYPE ZLESE0263,   "*-CS2024000593-21.11.2024-JT-#157800-inicio
          LW_DADOS         TYPE ZLESE0265,   "*-CS2024000593-21.11.2024-JT-#157800-inicio
          LW_LISTANFE      TYPE ZLESE0266,   "*-CS2024000593-21.11.2024-JT-#157800-inicio
          LW_CAMPOS_NFE    TYPE ZDE_CAMPOS_NFE,
          ZCL_UTIL         TYPE REF TO ZCL_UTIL.

    CLEAR: WL_ZLEST0177, WL_ZLEST0178, T_ZLEST0178[], V_PROTOCOLO_REC, V_RECEBEDOR_CNPJ, V_RECEBEDOR_NAME.

    CREATE OBJECT ZCL_UTIL.  "*-CS2024000593-21.11.2024-JT-#157800-inicio

*-CS2024000593-21.11.2024-JT-#157800-inicio
*--------------------------------------------------
*-- carregar dados
*--------------------------------------------------
    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_CDATA_RETORNO
      CHANGING
        DATA = LT_DADOS.

    CHECK LT_DADOS IS NOT INITIAL.

    LOOP AT LT_DADOS-DATA-LISTACARREGAMENTOFERROVIARIO INTO LW_DADOS.
      CLEAR WL_ZLEST0177_TMP.

      V_CONT_REG_TMP                                = V_CONT_REG_TMP + 1.

      WL_ZLEST0177_TMP-ID_TMP                       = V_CONT_REG_TMP.
      WL_ZLEST0177_TMP-SRV_INTEGRACAO               = '01'.
      WL_ZLEST0177_TMP-PROTOCOLO_REC                = ABAP_OFF.
      WL_ZLEST0177_TMP-RECEBEDOR_CNPJ               = ABAP_OFF.
      WL_ZLEST0177_TMP-RECEBEDOR_NAME               = ABAP_OFF.
      WL_ZLEST0177_TMP-SIGLA_TERMINAL_TRANSB        = LW_DADOS-CODIGOTERMINALTRANSBORDO.
      WL_ZLEST0177_TMP-TERMINAL_TRANSB              = LW_DADOS-CODIGOTERMINALTRANSBORDO.
      WL_ZLEST0177_TMP-DS_TERMINAL_TRANSB           = LW_DADOS-DESCRICAOTERMINALTRANSBORDO.

      ME->CONVERSION_CNPJ_INPUT( CHANGING I_CNPJ    = LW_DADOS-CNPJTERMINALTRANSBORDO ).
      WL_ZLEST0177_TMP-CNPJ_TERMINAL_TRANSB         = LW_DADOS-CNPJTERMINALTRANSBORDO.

      WL_ZLEST0177_TMP-NM_FANTASIA_TERMINAL_DESTINO = LW_DADOS-NOMEFANTASIATERMINALDESTINO.
      WL_ZLEST0177_TMP-RZ_SOCIAL_TERMINAL_DESTINO   = LW_DADOS-NOMEFANTASIATERMINALDESTINO.

      ME->CONVERSION_CNPJ_INPUT( CHANGING I_CNPJ    = LW_DADOS-CNPJTERMINALDESTINO ).
      WL_ZLEST0177_TMP-CNPJ_TERMINAL_DESTINO        = LW_DADOS-CNPJTERMINALDESTINO.

      WL_ZLEST0177_TMP-DT_FATURAMENTO               = LW_DADOS-DATAFATURAMENTO(4) && LW_DADOS-DATAFATURAMENTO+5(2) && LW_DADOS-DATAFATURAMENTO+8(2).
      WL_ZLEST0177_TMP-DT_CARREGAMENTO              = LW_DADOS-DATACARREGAMENTO(4) && LW_DADOS-DATACARREGAMENTO+5(2) && LW_DADOS-DATACARREGAMENTO+8(2).
      WL_ZLEST0177_TMP-IDVAGAO                      = LW_DADOS-CODIGOVAGAO.
      WL_ZLEST0177_TMP-SERIE_VAGAO                  = LW_DADOS-SERIEVAGAO.
      WL_ZLEST0177_TMP-NR_DESPACHO                  = LW_DADOS-NUMERODESPACHO.
      WL_ZLEST0177_TMP-SERIE_DESPACHO               = LW_DADOS-SERIEDESPACHO.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LW_DADOS-NUMEROCTE
        IMPORTING
          OUTPUT = WL_ZLEST0177_TMP-NR_CTE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LW_DADOS-SERIECTE
        IMPORTING
          OUTPUT = WL_ZLEST0177_TMP-SERIE_CTE.

*     wl_zlest0177_tmp-dt_descarga_porto            = abap_off.
      WL_ZLEST0177_TMP-CHAVE_CTE                    = LW_DADOS-CHAVECTE.
      WL_ZLEST0177_TMP-PESO_BRUTO                   = LW_DADOS-PESOBRUTO.
      WL_ZLEST0177_TMP-PESO_TARA                    = LW_DADOS-PESOTARA.
      WL_ZLEST0177_TMP-PESO_LIQUIDO                 = LW_DADOS-PESOLIQUIDO.
      WL_ZLEST0177_TMP-DT_REGISTRO                  = SY-DATUM.
      WL_ZLEST0177_TMP-HR_REGISTRO                  = SY-UZEIT.

      APPEND WL_ZLEST0177_TMP                      TO IT_ZLEST0177_TMP.

      LOOP AT LW_DADOS-LISTANFE INTO LW_LISTANFE.
        CLEAR WL_ZLEST0178_TMP.

        LW_CAMPOS_NFE                               = ZCL_UTIL->GET_ATRIBUTOS_NFE( CONV #( LW_LISTANFE-CHAVENFE ) ).

        WL_ZLEST0178_TMP-ID_TMP                     = V_CONT_REG_TMP.
        WL_ZLEST0178_TMP-CHAVE_NF                   = LW_LISTANFE-CHAVENFE.
        WL_ZLEST0178_TMP-MODEL                      = LW_CAMPOS_NFE-MODEL.
        WL_ZLEST0178_TMP-NFENUM                     = LW_CAMPOS_NFE-NFNUM9.
        WL_ZLEST0178_TMP-SERIES                     = LW_CAMPOS_NFE-SERIE.
        WL_ZLEST0178_TMP-STCD1                      = LW_CAMPOS_NFE-STCD1.
        WL_ZLEST0178_TMP-DOCDAT                     = LW_LISTANFE-DATAEMISSAO(4) && LW_LISTANFE-DATAEMISSAO+5(2) && LW_LISTANFE-DATAEMISSAO+8(2).
        WL_ZLEST0178_TMP-DOCNUM                     = ABAP_OFF.
        WL_ZLEST0178_TMP-PESO_DECLARADO             = LW_LISTANFE-PESODECLARADO.
        WL_ZLEST0178_TMP-PESO_CARREGADO             = LW_LISTANFE-PESORATEADO.

        APPEND WL_ZLEST0178_TMP                    TO IT_ZLEST0178_TMP.
      ENDLOOP.
    ENDLOOP.
*-CS2024000593-21.11.2024-JT-#157800-fim

*-CS2024000593-21.11.2024-JT-#157800-inicio
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
**   Ler Vagoes Carregados
**------------------------------------------------------------------------------------------------*
*
*    DATA(_xml_vagoes_carregados) = c_xml_return->find_node( EXPORTING name = 'VagoesCarregados' ).
*
*    IF _xml_vagoes_carregados IS NOT INITIAL.
*
*      DATA(_iterator_vagao_carreg) = _xml_vagoes_carregados->create_iterator( ).
*      DATA(_xml_node_vagao_carreg) = _iterator_vagao_carreg->get_next( ).
*
*      CLEAR: v_novo_veiculo, v_cont_reg_tmp,
*             it_zlest0177_tmp[], wl_zlest0177_tmp,
*             it_zlest0178_tmp[], wl_zlest0178_tmp.
*
*      WHILE _xml_node_vagao_carreg IS NOT INITIAL.
*
*        CASE _xml_node_vagao_carreg->get_type( ).
*          WHEN: if_ixml_node=>co_node_element.
*
*            _node_name  = _xml_node_vagao_carreg->get_name( ).
*            _node_valor = _xml_node_vagao_carreg->get_value( ).
*
*            CASE _node_name.
*              WHEN 'SispatVagao'.
*
*                "Novo Vagao
*                IF wl_zlest0177_tmp IS NOT INITIAL.
*                  APPEND wl_zlest0177_tmp TO it_zlest0177_tmp.
*                ENDIF.
*
*                CLEAR: wl_zlest0177_tmp.
*
*                "Gerar ID Temporario
*                ADD 1 TO v_cont_reg_tmp.
*
*                wl_zlest0177_tmp-id_tmp = v_cont_reg_tmp.
*
*                wl_zlest0177_tmp-sigla_terminal_transb = me->at_sigla_terminal.
*
**----------------------------------------------------------------------------*
**             Dados Carregamento - Vagao
**----------------------------------------------------------------------------*
*              WHEN 'IdVagao'.
*
*              WHEN 'CodigoTerminalTransbordo'.
*
*                wl_zlest0177_tmp-terminal_transb = _node_valor.
*
*              WHEN 'DescricaoTerminalTransbordo'.
*
*                wl_zlest0177_tmp-ds_terminal_transb = _node_valor.
*
*              WHEN 'CnpjTerminalTransbordo'.
*
*                me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
*                wl_zlest0177_tmp-cnpj_terminal_transb = _node_valor.
*
*              WHEN 'NomeFantasiaTerminalDestino'.
*
*                wl_zlest0177_tmp-nm_fantasia_terminal_destino = _node_valor.
*
*              WHEN 'RazaoSocialTerminalDestino'.
*
*                wl_zlest0177_tmp-rz_social_terminal_destino = _node_valor.
*
*              WHEN 'CnpjTerminalDestino'.
*
*                me->conversion_cnpj_input( CHANGING i_cnpj = _node_valor ).
*                wl_zlest0177_tmp-cnpj_terminal_destino = _node_valor.
*
*              WHEN 'CodigoVagao'.
*
*                wl_zlest0177_tmp-idvagao = _node_valor.
*
*              WHEN 'SerieVagao'.
*
*                wl_zlest0177_tmp-serie_vagao = _node_valor.
*
*              WHEN 'PesoBruto'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0177_tmp-peso_bruto = _node_valor.
*
*              WHEN 'PesoTara'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0177_tmp-peso_tara = _node_valor.
*
*              WHEN 'PesoLiquido'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0177_tmp-peso_liquido = _node_valor.
*
*              WHEN 'DataCarregamento'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0177_tmp-dt_carregamento = _node_valor.
*
*              WHEN 'DataFaturamento'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0177_tmp-dt_faturamento = _node_valor.
*
*              WHEN 'NumeroDespacho'.
*
*                wl_zlest0177_tmp-nr_despacho = _node_valor.
*
*              WHEN 'SerieDespacho'.
*
*                wl_zlest0177_tmp-serie_despacho = _node_valor.
*
*              WHEN 'NumeroCte'.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = _node_valor
*                  IMPORTING
*                    output = wl_zlest0177_tmp-nr_cte.
*
*              WHEN 'SerieCte'.
*
*                wl_zlest0177_tmp-serie_cte = _node_valor.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = wl_zlest0177_tmp-serie_cte
*                  IMPORTING
*                    output = wl_zlest0177_tmp-serie_cte.
*
*              WHEN 'ChaveCte'.
*
*                wl_zlest0177_tmp-chave_cte = _node_valor.
*
*              WHEN 'DataDescarregamentoPorto'.
*
**----------------------------------------------------------------------------*
**             Dados Carregamento - Notas Fiscal
**----------------------------------------------------------------------------*
*
*              WHEN 'ListaNfes'.
*
*                "Nova NF
*                IF wl_zlest0178_tmp IS NOT INITIAL.
*                  APPEND wl_zlest0178_tmp TO it_zlest0178_tmp.
*                ENDIF.
*
*                CLEAR: wl_zlest0178_tmp.
*
*                wl_zlest0178_tmp-id_tmp = v_cont_reg_tmp.
*
*              WHEN 'NumeroNotaFiscal'.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = _node_valor
*                  IMPORTING
*                    output = wl_zlest0178_tmp-nfenum.
*
*              WHEN 'SerieNotaFiscal'.
*
*                wl_zlest0178_tmp-series = _node_valor.
*
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = wl_zlest0178_tmp-series
*                  IMPORTING
*                    output = wl_zlest0178_tmp-series.
*
*
*              WHEN 'DataEmissao'.
*
*                _node_valor = _node_valor(10).
*                REPLACE ALL OCCURRENCES OF '-' IN _node_valor WITH space.
*
*                wl_zlest0178_tmp-docdat = _node_valor.
*
*              WHEN 'ChaveNfe'.
*
*                wl_zlest0178_tmp-chave_nf = _node_valor.
*                wl_zlest0178_tmp-model    = '55'.
*                wl_zlest0178_tmp-stcd1    = wl_zlest0178_tmp-chave_nf+6(14).
*
*              WHEN 'PesoDeclarado'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0178_tmp-peso_declarado = _node_valor.
*
*              WHEN 'PesoCarregado'.
*
*                REPLACE ALL OCCURRENCES OF ',' IN _node_valor WITH '.'.
*                CONDENSE _node_valor NO-GAPS.
*
*                wl_zlest0178_tmp-peso_carregado = _node_valor.
*
*            ENDCASE.
*        ENDCASE.
*
*        _xml_node_vagao_carreg = _iterator_vagao_carreg->get_next( ).
*      ENDWHILE.
*    ENDIF.
*
*    IF wl_zlest0177_tmp IS NOT INITIAL.
*      APPEND wl_zlest0177_tmp TO it_zlest0177_tmp.
*    ENDIF.
*
*    IF wl_zlest0178_tmp IS NOT INITIAL.
*      APPEND wl_zlest0178_tmp TO it_zlest0178_tmp.
*    ENDIF.
*-CS2024000593-21.11.2024-JT-#157800-fim

    "Processar Dados
    LOOP AT IT_ZLEST0177_TMP ASSIGNING FIELD-SYMBOL(<FS_ZLEST0177_TMP>).
      <FS_ZLEST0177_TMP>-PROTOCOLO_REC  = V_PROTOCOLO_REC.
      <FS_ZLEST0177_TMP>-RECEBEDOR_CNPJ = V_RECEBEDOR_CNPJ.
      <FS_ZLEST0177_TMP>-RECEBEDOR_NAME = V_RECEBEDOR_NAME.

      "Carregar Notas
      LOOP AT IT_ZLEST0178_TMP ASSIGNING FIELD-SYMBOL(<FS_ZLEST0178_TMP>) WHERE ID_TMP EQ <FS_ZLEST0177_TMP>-ID_TMP.
        APPEND <FS_ZLEST0178_TMP> TO <FS_ZLEST0177_TMP>-NOTAS.
      ENDLOOP.
    ENDLOOP.

    ME->AT_DADOS_CARREGAMENTO[] = IT_ZLEST0177_TMP[].

  ENDMETHOD.


  METHOD TRATAR_RETORNO_04.
*------------------------------------------------------------------------------------------------*
*   Dados Carregamento - Vagao
*------------------------------------------------------------------------------------------------*

    TYPES: BEGIN OF TY_FINAL,
             DATA_CARREGAMENTO TYPE STRING,
             CODIGO            TYPE STRING,
             SERIAL            TYPE STRING,
           END OF TY_FINAL.

    TYPES: BEGIN OF TY_PESO_NF,
             CHAVE_NF   TYPE ZLEST0178-CHAVE_NF,
             PESO_CARGA TYPE ZLEST0178-PESO_CARREGADO,
             PESO_TOTAL TYPE ZLEST0178-PESO_CARREGADO.
    TYPES: END   OF TY_PESO_NF.

    DATA:T_USERMD            TYPE STANDARD TABLE OF  RGSB4.


    FIELD-SYMBOLS:
      <DATA>                         TYPE DATA,
      <DATA_DESCARGA>                TYPE DATA,
      <DATA_VAGAO>                   TYPE DATA,
      <DATA_VAGAO_INFO>              TYPE DATA,
      <DATA_LOCAL_CARREGAMENTO>      TYPE DATA,
      <DATA_DESTINATARIO>            TYPE DATA,
      <DATA_CEINFO>                  TYPE DATA,
      <DATA_PESO_TARA>               TYPE DATA,
      <DATA_PESO>                    TYPE DATA,
      <DATA_PESO_TOTAL>              TYPE DATA,
      <DATA_DOC_FISCAIS>             TYPE DATA,
      <DATA_NF_REFERENCIA>           TYPE DATA,
      <DATA_NF_PESO_CARGA>           TYPE DATA,
      <DATA_PESO_NOTA>               TYPE DATA,
      <DATA_PESO_CARGA>              TYPE DATA,
      <DATA_NOTA>                    TYPE DATA,
      <RESULTS>                      TYPE ANY,
      <RESULTS_NOTA>                 TYPE ANY,
      <STRUCTURE>                    TYPE ANY,
      <STRUCTURE_DESCARGA>           TYPE ANY,
      <STRUCTURE_VAGAO>              TYPE ANY,
      <STRUCTURE_VAGAO_INFO>         TYPE ANY,
      <STRUCTURE_LOCAL_CARREGAMENTO> TYPE ANY,
      <STRUCTURE_DESTINATARIO>       TYPE ANY,
      <STRUCTURE_NF_REFERENCIA>      TYPE ANY,
      <STRUCTURE_NF_PESO_CARGA>      TYPE ANY,
      <STRUCTURE_NOTA>               TYPE ANY,
      <TABLE>                        TYPE ANY TABLE,
      <TABLE_NOTA>                   TYPE ANY TABLE,
      <FIELD>                        TYPE ANY,
      <FIELD_VALUE>                  TYPE DATA,
      <RESULTS_DESCARGA>             TYPE ANY,
      <TABLE_DESCARGA>               TYPE ANY TABLE,
      <RESULTS_VAGAO>                TYPE ANY,
      <TABLE_VAGAO>                  TYPE ANY TABLE,
      <RESULTS_VAGAO_INFO>           TYPE ANY,
      <TABLE_VAGAO_INFO>             TYPE ANY TABLE,
      <TABLE_NF_REFERENCIA>          TYPE ANY TABLE,
      <TABLE_NF_PESO_CARGA>          TYPE ANY TABLE,
      <RESULTS_LOCAL_CARREGAMENTO>   TYPE ANY,
      <TABLE_LOCAL_CARREGAMENTO>     TYPE ANY TABLE,
      <TABLE_DESTINATARIO>           TYPE ANY TABLE,
      <RESULTS_DESTINATARIO>         TYPE ANY,
      <RESULTS_CEINFO>               TYPE ANY,
      <RESULTS_PESO_TARA>            TYPE ANY,
      <RESULTS_PESO>                 TYPE ANY,
      <RESULTS_PESO_TOTAL>           TYPE ANY,
      <RESULTS_DOC_FISCAIS>          TYPE ANY,
      <RESULTS_NF_REFERENCIA>        TYPE ANY,
      <RESULTS_NF_PESO_CARGA>        TYPE ANY,
      <RESULTS_PESO_NOTA>            TYPE ANY,
      <RESULTS_PESO_CARGA>           TYPE ANY.

    DATA: LR_DATA        TYPE REF TO DATA,
          LS_FINAL       TYPE TY_FINAL,
          LT_FINAL       TYPE TABLE OF TY_FINAL,
          V_CONT_REG_TMP TYPE ZLEST0177-ID_REGISTRO.

    DATA: IT_ZLEST0177_TMP TYPE TABLE OF ZDE_ZLEST0177_TMP,
          WL_ZLEST0177_TMP TYPE ZDE_ZLEST0177_TMP,

          IT_ZLEST0178_TMP TYPE TABLE OF ZDE_ZLEST0178_TMP,
          WL_ZLEST0178_TMP TYPE ZDE_ZLEST0178_TMP.

    DATA: IT_PESO_NF TYPE TABLE OF TY_PESO_NF,
          WL_PESO_NF TYPE TY_PESO_NF.

    DATA: LVA_PESO_ID     TYPE ZLEST0177-PESO_LIQUIDO,
          LVA_PESO_TOTAL  TYPE ZLEST0177-PESO_LIQUIDO,
          LVA_PESO_CARGA2 TYPE  ZLEST0177-PESO_LIQUIDO,
          LVA_PESO_CARGA  TYPE  ZLEST0177-PESO_LIQUIDO,
          LVA_CHAVE       TYPE ZLEST0178-CHAVE_NF,
          LVA_DOCNUM      TYPE NUMC10,
          LVA_NFENUM      TYPE NUMC09.


    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON         = I_CDATA_RETORNO
        PRETTY_NAME  = /UI2/CL_JSON=>PRETTY_MODE-USER
        ASSOC_ARRAYS = ABAP_TRUE
      CHANGING
        DATA         = LR_DATA.

    DATA: NR_DESPACHO    TYPE ZLEST0177-NR_DESPACHO,
          SERIE_DESPACHO TYPE ZLEST0177-SERIE_DESPACHO.


    ASSIGN LR_DATA->* TO <DATA>.

    ASSIGN COMPONENT 'DETALHES_PLANEJAMENTO' OF STRUCTURE <DATA> TO <RESULTS>.
    ASSIGN <RESULTS>->* TO <TABLE>.

    LOOP AT <TABLE> ASSIGNING <STRUCTURE>.

      ASSIGN <STRUCTURE>->* TO <DATA>.

      ASSIGN COMPONENT 'DETALHE_DESCARGA' OF STRUCTURE <DATA> TO <RESULTS_DESCARGA>.
      ASSIGN <RESULTS_DESCARGA>->* TO <DATA_DESCARGA>.

      ASSIGN COMPONENT 'DESCARGA_VAGAO' OF STRUCTURE <DATA_DESCARGA> TO <RESULTS_VAGAO>.
      ASSIGN <RESULTS_VAGAO>->* TO <DATA_VAGAO>.

      ASSIGN COMPONENT 'VAGAO_INFO' OF STRUCTURE <DATA_VAGAO> TO <RESULTS_VAGAO_INFO>.
      ASSIGN <RESULTS_VAGAO_INFO>->* TO <TABLE_VAGAO_INFO>.

      LOOP AT <TABLE_VAGAO_INFO> ASSIGNING <STRUCTURE_VAGAO_INFO>.

        ASSIGN <STRUCTURE_VAGAO_INFO>->* TO <DATA_VAGAO_INFO>.

        ASSIGN COMPONENT 'DATA_CARREGAMENTO' OF STRUCTURE <DATA_VAGAO_INFO> TO <FIELD>.

        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          "Gerar ID Temporario
          ADD 1 TO V_CONT_REG_TMP.

          WL_ZLEST0177_TMP-ID_TMP    = V_CONT_REG_TMP.
        ENDIF.

        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'CODIGO' OF STRUCTURE <DATA_VAGAO_INFO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-IDVAGAO = <FIELD_VALUE>.
        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'SERIAL' OF STRUCTURE <DATA_VAGAO_INFO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-SERIE_VAGAO = <FIELD_VALUE>.
        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'LOCAL_CARREGAMENTO' OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_LOCAL_CARREGAMENTO>.

        ASSIGN <RESULTS_LOCAL_CARREGAMENTO>->* TO  <DATA_LOCAL_CARREGAMENTO>.

        ASSIGN COMPONENT 'DOMINIO' OF STRUCTURE <DATA_LOCAL_CARREGAMENTO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-SIGLA_TERMINAL_TRANSB = <FIELD_VALUE>.
          WL_ZLEST0177_TMP-TERMINAL_TRANSB = <FIELD_VALUE>.

          IF WL_ZLEST0177_TMP-SIGLA_TERMINAL_TRANSB IS NOT INITIAL.
            "Seleciona CNPJ Terminal com base na segla terminal transb.
            SELECT SINGLE *
            FROM SETLEAF
            INTO @DATA(I_DATA)
            WHERE SETNAME EQ 'TERMINAIS_RUMO_L1_WS'
            AND VALFROM EQ @WL_ZLEST0177_TMP-SIGLA_TERMINAL_TRANSB.

            IF I_DATA IS NOT INITIAL.
              CALL FUNCTION 'G_SET_GET_ALL_VALUES'
                EXPORTING
                  CLASS           = '0000'
                  SETNR           = 'TERMINAIS_RUMO_L1_WS'
                  NO_DESCRIPTIONS = ' '
                TABLES
                  SET_VALUES      = T_USERMD
                EXCEPTIONS
                  SET_NOT_FOUND   = 1
                  OTHERS          = 2.

              READ TABLE T_USERMD INTO DATA(WA_USERMD) WITH KEY FROM = WL_ZLEST0177_TMP-SIGLA_TERMINAL_TRANSB.
              IF SY-SUBRC  EQ 0.
                WL_ZLEST0177_TMP-CNPJ_TERMINAL_TRANSB =  WA_USERMD-TITLE.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'DESCRICAO' OF STRUCTURE <DATA_LOCAL_CARREGAMENTO> TO <FIELD>.

        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          CONCATENATE  WL_ZLEST0177_TMP-SIGLA_TERMINAL_TRANSB  <FIELD_VALUE> INTO  WL_ZLEST0177_TMP-DS_TERMINAL_TRANSB SEPARATED BY SPACE.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.


        ASSIGN COMPONENT 'DESTINATARIO' OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_DESTINATARIO>.
        ASSIGN <RESULTS_DESTINATARIO>->* TO  <DATA_DESTINATARIO>.

        ASSIGN COMPONENT 'DOMINIO' OF STRUCTURE <DATA_DESTINATARIO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-NM_FANTASIA_TERMINAL_DESTINO = <FIELD_VALUE>.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'DESCRICAO' OF STRUCTURE <DATA_DESTINATARIO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          CONCATENATE WL_ZLEST0177_TMP-NM_FANTASIA_TERMINAL_DESTINO <FIELD_VALUE>
          INTO WL_ZLEST0177_TMP-RZ_SOCIAL_TERMINAL_DESTINO SEPARATED BY SPACE.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'CONHECIMENTO_ELETRONICO_INFO'  OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_CEINFO>.
        ASSIGN <RESULTS_CEINFO>->* TO  <DATA_CEINFO>.

        ASSIGN COMPONENT 'NUMERO' OF STRUCTURE <DATA_CEINFO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-NR_CTE = <FIELD_VALUE>.
          WL_ZLEST0177_TMP-NR_DESPACHO = <FIELD_VALUE>. "Incluido através do Bug  59336 - 02/06/2021 - AOENNING.
        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'SERIE' OF STRUCTURE <DATA_CEINFO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-SERIE_CTE       = <FIELD_VALUE>.
          WL_ZLEST0177_TMP-SERIE_DESPACHO  = <FIELD_VALUE>. "Incluido através do Bug  59336 - 02/06/2021 - AOENNING.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'CHAVE' OF STRUCTURE <DATA_CEINFO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-CHAVE_CTE  = <FIELD_VALUE>.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'DATA' OF STRUCTURE <DATA_CEINFO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          CONCATENATE <FIELD_VALUE>(4)
                      <FIELD_VALUE>+5(2)
                      <FIELD_VALUE>+8(2) INTO WL_ZLEST0177_TMP-DT_FATURAMENTO.

          WL_ZLEST0177_TMP-DT_CARREGAMENTO = WL_ZLEST0177_TMP-DT_FATURAMENTO.
        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'PESO_TARA' OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_PESO_TARA>.
        ASSIGN <RESULTS_PESO_TARA>->* TO  <DATA_PESO_TARA>.

        ASSIGN COMPONENT 'PESO_CARGA' OF STRUCTURE <DATA_PESO_TARA> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-PESO_TARA = ( <FIELD_VALUE>  * 1000 ).

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'PESO' OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_PESO>.
        ASSIGN <RESULTS_PESO>->* TO  <DATA_PESO>.

        ASSIGN COMPONENT 'PESO_CARGA' OF STRUCTURE <DATA_PESO> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-PESO_LIQUIDO = ( <FIELD_VALUE>  * 1000 ).
          LVA_PESO_CARGA = <FIELD_VALUE>.

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'PESO_TOTAL' OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_PESO_TOTAL>.
        ASSIGN <RESULTS_PESO_TOTAL>->* TO  <DATA_PESO_TOTAL>.

        ASSIGN COMPONENT 'PESO_CARGA' OF STRUCTURE <DATA_PESO_TOTAL> TO <FIELD>.
        IF <FIELD> IS ASSIGNED.
          LR_DATA = <FIELD>.
          ASSIGN LR_DATA->* TO <FIELD_VALUE>.

          WL_ZLEST0177_TMP-PESO_BRUTO = ( <FIELD_VALUE>  * 1000 ).

        ENDIF.
        UNASSIGN: <FIELD>, <FIELD_VALUE>.

        ASSIGN COMPONENT 'DOCUMENTOS_FISCAIS' OF STRUCTURE <DATA_VAGAO_INFO> TO <RESULTS_DOC_FISCAIS>.
        ASSIGN <RESULTS_DOC_FISCAIS>->* TO  <DATA_DOC_FISCAIS>.

        ASSIGN COMPONENT 'NOTAS_REFERENCIA' OF STRUCTURE <DATA_DOC_FISCAIS> TO <RESULTS_NF_REFERENCIA>.
        ASSIGN <RESULTS_NF_REFERENCIA>->* TO <TABLE_NF_REFERENCIA>.

        CLEAR: LVA_PESO_ID.

*----------------------------------------------------------------------------------------------------------
        FREE: IT_PESO_NF.

*          Primeiro ele soma os valores dos IDS desse vagão para verificar se da o total do vagão
        LOOP AT <TABLE_NF_REFERENCIA> ASSIGNING <STRUCTURE_NF_REFERENCIA>.
          ASSIGN <STRUCTURE_NF_REFERENCIA>->* TO <DATA_NF_REFERENCIA>.

          ASSIGN COMPONENT 'PESO_CARGA' OF STRUCTURE <DATA_NF_REFERENCIA> TO <RESULTS_NF_PESO_CARGA> .
          ASSIGN <RESULTS_NF_PESO_CARGA>->* TO <DATA_NF_PESO_CARGA>..

          ASSIGN COMPONENT 'PESO' OF STRUCTURE <DATA_NF_PESO_CARGA> TO <FIELD>.
          IF <FIELD> IS ASSIGNED.
            LR_DATA = <FIELD>.
            ASSIGN LR_DATA->* TO <FIELD_VALUE>.
            LVA_PESO_CARGA2  = <FIELD_VALUE>.
            LVA_PESO_ID = LVA_PESO_ID + <FIELD_VALUE>.
          ENDIF.

          UNASSIGN: <FIELD>, <FIELD_VALUE>.

*----------------------- JT
          ASSIGN COMPONENT 'PESO_TOTAL' OF STRUCTURE <DATA_NF_REFERENCIA> TO <RESULTS_NF_PESO_CARGA> .
          ASSIGN <RESULTS_NF_PESO_CARGA>->* TO <DATA_NF_PESO_CARGA>..

          ASSIGN COMPONENT 'PESO' OF STRUCTURE <DATA_NF_PESO_CARGA> TO <FIELD>.
          IF <FIELD> IS ASSIGNED.
            LR_DATA = <FIELD>.
            ASSIGN LR_DATA->* TO <FIELD_VALUE>.
            LVA_PESO_TOTAL  = <FIELD_VALUE>.
          ENDIF.

          UNASSIGN: <FIELD>, <FIELD_VALUE>.

          ASSIGN COMPONENT 'CHAVE' OF STRUCTURE <DATA_NF_REFERENCIA> TO <FIELD>.
          IF <FIELD> IS ASSIGNED.
            LR_DATA = <FIELD>.
            ASSIGN LR_DATA->* TO <FIELD_VALUE>.
            LVA_CHAVE = <FIELD_VALUE>.
          ENDIF.

          WL_PESO_NF-CHAVE_NF   =  LVA_CHAVE.
          WL_PESO_NF-PESO_CARGA =  LVA_PESO_CARGA2.
          WL_PESO_NF-PESO_TOTAL =  LVA_PESO_TOTAL.
          APPEND WL_PESO_NF    TO IT_PESO_NF.
*----------------------- JT

          UNASSIGN: <FIELD>, <FIELD_VALUE>.
        ENDLOOP.

*----------------------------------------------------------------------------------------------------------
*          Todos os IDs tem o Peso do vagão

        "IF lva_peso_id  = lva_peso_carga.

        LOOP AT <TABLE_NF_REFERENCIA> ASSIGNING <STRUCTURE_NF_REFERENCIA>.

          CLEAR: LVA_CHAVE.

          ASSIGN <STRUCTURE_NF_REFERENCIA>->* TO <DATA_NF_REFERENCIA>.
          ASSIGN COMPONENT 'CHAVE' OF STRUCTURE <DATA_NF_REFERENCIA> TO <FIELD>.
          IF <FIELD> IS ASSIGNED.
            LR_DATA = <FIELD>.
            ASSIGN LR_DATA->* TO <FIELD_VALUE>.

            LVA_CHAVE = <FIELD_VALUE>.

          ENDIF.
          UNASSIGN: <FIELD>, <FIELD_VALUE>.

*----------------------------------------------------------------------------------------------------------
*        Busca Informação da Nota.

          " DADOS NOTA FISCAL.
          ASSIGN COMPONENT 'NOTAS' OF STRUCTURE <DATA> TO <RESULTS_NOTA>.
          ASSIGN <RESULTS_NOTA>->* TO <TABLE_NOTA>.

          LOOP AT <TABLE_NOTA>  ASSIGNING <STRUCTURE_NOTA>.

            ASSIGN <STRUCTURE_NOTA>->* TO <DATA_NOTA>.
            ASSIGN COMPONENT 'CHAVE_NFE' OF STRUCTURE <DATA_NOTA> TO <FIELD>.

            IF <FIELD> IS ASSIGNED.
              LR_DATA = <FIELD>.
              ASSIGN LR_DATA->* TO <FIELD_VALUE>.

              IF <FIELD_VALUE> EQ LVA_CHAVE.

                WL_ZLEST0178_TMP-ID_TMP    = V_CONT_REG_TMP.
                WL_ZLEST0178_TMP-CHAVE_NF =  <FIELD_VALUE> .
                WL_ZLEST0178_TMP-MODEL =  WL_ZLEST0178_TMP-CHAVE_NF+20(2).

                "CONCATENATE wl_zlest0178_tmp-chave_nf+6(1) wl_zlest0178_tmp-chave_nf+20(1) INTO wl_zlest0178_tmp-stcd1.

                WL_ZLEST0178_TMP-STCD1 = WL_ZLEST0178_TMP-CHAVE_NF+6(14).

                UNASSIGN: <FIELD>, <FIELD_VALUE>.

                ASSIGN COMPONENT 'SERIE' OF STRUCTURE <DATA_NOTA> TO <FIELD>.
                IF <FIELD> IS ASSIGNED.
                  LR_DATA = <FIELD>.
                  ASSIGN LR_DATA->* TO <FIELD_VALUE>.

                  WL_ZLEST0178_TMP-SERIES = <FIELD_VALUE>.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = WL_ZLEST0178_TMP-SERIES
                    IMPORTING
                      OUTPUT = WL_ZLEST0178_TMP-SERIES.

                ENDIF.
                UNASSIGN: <FIELD>, <FIELD_VALUE>.

                ASSIGN COMPONENT 'DATA_EMISSAO' OF STRUCTURE <DATA_NOTA> TO <FIELD>.
                IF <FIELD> IS ASSIGNED.

                  LR_DATA = <FIELD>.
                  ASSIGN LR_DATA->* TO <FIELD_VALUE>.
                  CONCATENATE <FIELD_VALUE>+6(4)
                              <FIELD_VALUE>+3(2)
                              <FIELD_VALUE>(2)
                         INTO WL_ZLEST0178_TMP-DOCDAT.

                ENDIF.
                UNASSIGN: <FIELD>, <FIELD_VALUE>.

*----------------------- JT
                ASSIGN COMPONENT 'NUMERO' OF STRUCTURE <DATA_NOTA> TO <FIELD>.
                IF <FIELD> IS ASSIGNED.
                  LR_DATA = <FIELD>.
                  ASSIGN LR_DATA->* TO <FIELD_VALUE>.
                  LVA_NFENUM = <FIELD_VALUE>.
                  WL_ZLEST0178_TMP-NFENUM = LVA_NFENUM.
*                 wl_zlest0178_tmp-docnum = lva_docnum.
                ENDIF.

                UNASSIGN: <FIELD>, <FIELD_VALUE>.

                READ TABLE IT_PESO_NF INTO WL_PESO_NF WITH KEY CHAVE_NF = WL_ZLEST0178_TMP-CHAVE_NF.
                IF SY-SUBRC = 0.
                  WL_ZLEST0178_TMP-PESO_DECLARADO = WL_PESO_NF-PESO_TOTAL * 1000.
                  WL_ZLEST0178_TMP-PESO_CARREGADO = WL_PESO_NF-PESO_CARGA  * 1000.
                ENDIF.

                ASSIGN COMPONENT 'PESO_NOTA' OF STRUCTURE <DATA_NOTA> TO <RESULTS_PESO_NOTA>.
                ASSIGN <RESULTS_PESO_NOTA>->* TO  <DATA_PESO_NOTA>.

                ASSIGN COMPONENT 'PESO_CARGA' OF STRUCTURE <DATA_PESO_NOTA> TO <FIELD>.
                IF <FIELD> IS ASSIGNED.
                  LR_DATA = <FIELD>.
                  ASSIGN LR_DATA->* TO <FIELD_VALUE>.
*                 wl_zlest0178_tmp-peso_declarado = ( <field_value> * 1000 ).
*                 wl_zlest0178_tmp-peso_carregado = ( <field_value> * 1000 ).
                ENDIF.
*----------------------- JT
                UNASSIGN: <FIELD>, <FIELD_VALUE>.

                IF WL_ZLEST0178_TMP IS NOT INITIAL.
                  APPEND WL_ZLEST0178_TMP TO IT_ZLEST0178_TMP.
                ENDIF.
                CLEAR: WL_ZLEST0178_TMP.

                EXIT.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        IF WL_ZLEST0177_TMP IS NOT INITIAL.
          APPEND WL_ZLEST0177_TMP TO IT_ZLEST0177_TMP.
        ENDIF.
        CLEAR: WL_ZLEST0177_TMP.

        "ENDIF.

      ENDLOOP.
    ENDLOOP.

    "Processar Dados
    LOOP AT IT_ZLEST0177_TMP ASSIGNING FIELD-SYMBOL(<FS_ZLEST0177_TMP>).
*      <fs_zlest0177_tmp>-protocolo_rec  = v_protocolo_rec.
*      <fs_zlest0177_tmp>-recebedor_cnpj = v_recebedor_cnpj.
*      <fs_zlest0177_tmp>-recebedor_name = v_recebedor_name.

      "Carregar Notas
      LOOP AT IT_ZLEST0178_TMP ASSIGNING FIELD-SYMBOL(<FS_ZLEST0178_TMP>) WHERE ID_TMP EQ <FS_ZLEST0177_TMP>-ID_TMP.
        APPEND <FS_ZLEST0178_TMP> TO <FS_ZLEST0177_TMP>-NOTAS.
      ENDLOOP.
    ENDLOOP.

    ME->AT_DADOS_CARREGAMENTO[] = IT_ZLEST0177_TMP[].

  ENDMETHOD.


  METHOD TRATAR_RETORNO_CONSULTA.

    CASE ME->AT_SRV_INTEGRACAO.
      WHEN '01'. "Terminais VLI

        ME->TRATAR_RETORNO_01(
          EXPORTING
            I_CDATA_RETORNO = I_CDATA_RETORNO
          CHANGING
            C_XML_RETURN    = C_XML_RETURN
        ).
*** PBI 52204 - Inicio
      WHEN '04'. "Terminais RUMO

        ME->TRATAR_RETORNO_04(
          EXPORTING
            I_CDATA_RETORNO = I_CDATA_RETORNO
          CHANGING
            C_XML_RETURN    = C_XML_RETURN
        ).
*** PBI 52204 - Fim
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD TRATAR_XML.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN C_XML WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN C_XML WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN C_XML WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN C_XML WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN C_XML WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN C_XML WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN C_XML WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN C_XML WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN C_XML WITH 'o' IGNORING CASE.

  ENDMETHOD.
ENDCLASS.

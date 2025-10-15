class ZCL_CCT_ENTREGA_CARGA definition
  public
  final
  create public .

public section.

  interfaces ZIF_CCT_ENTREGA_CARGA .

  aliases AT_DOCUMENTOS
    for ZIF_CCT_ENTREGA_CARGA~AT_DOCUMENTOS .
  aliases AT_DOCUMENTOS_CARGA
    for ZIF_CCT_ENTREGA_CARGA~AT_DOCUMENTOS_CARGA .
  aliases AT_ENTREGA
    for ZIF_CCT_ENTREGA_CARGA~AT_ENTREGA .
  aliases AT_TOKEN
    for ZIF_CCT_ENTREGA_CARGA~AT_TOKEN .
  aliases CK_ALTEROU
    for ZIF_CCT_ENTREGA_CARGA~CK_ALTEROU .
  aliases ADD_DOCUMENTO
    for ZIF_CCT_ENTREGA_CARGA~ADD_DOCUMENTO .
  aliases ADD_DOCUMENTO_CARGA
    for ZIF_CCT_ENTREGA_CARGA~ADD_DOCUMENTO_CARGA .
  aliases CANCELAR_ENTREGA
    for ZIF_CCT_ENTREGA_CARGA~CANCELAR_ENTREGA .
  aliases ENVIAR_ENTREGA
    for ZIF_CCT_ENTREGA_CARGA~ENVIAR_ENTREGA .
  aliases GRAVAR_REGISTRO
    for ZIF_CCT_ENTREGA_CARGA~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CCT_ENTREGA_CARGA~LIMPAR_REGISTRO .
  aliases MONTA_XML
    for ZIF_CCT_ENTREGA_CARGA~MONTA_XML .
  aliases REGISTRO_ENTREGA
    for ZIF_CCT_ENTREGA_CARGA~REGISTRO_ENTREGA .
  aliases SET_CABECALHO
    for ZIF_CCT_ENTREGA_CARGA~SET_CABECALHO .
  aliases SET_CNPJ_RESPONSAVEL
    for ZIF_CCT_ENTREGA_CARGA~SET_CNPJ_RESPONSAVEL .
  aliases SET_TOKEN
    for ZIF_CCT_ENTREGA_CARGA~SET_TOKEN .
  aliases VALIDAR_REGISTRO
    for ZIF_CCT_ENTREGA_CARGA~VALIDAR_REGISTRO .

  methods CONSTRUCTOR
    importing
      !I_ID_ENTREGA type ZDE_ID_ENTREGA optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CCT_ENTREGA_CARGA IMPLEMENTATION.


  method CONSTRUCTOR.

    CLEAR: ME->AT_ENTREGA, ME->AT_DOCUMENTOS[].

    CHECK ( I_ID_ENTREGA IS NOT INITIAL ).

    "Cabeçalho Entrega
    SELECT SINGLE *
      FROM ZSDT0179 INTO @DATA(_WL_0179)
     WHERE ID_ENTREGA = @I_ID_ENTREGA.

    CHECK SY-SUBRC = 0.

    "Documentos
    SELECT *
      FROM ZSDT0180 INTO TABLE @DATA(_TG_0180)
     WHERE ID_ENTREGA = @I_ID_ENTREGA.

    IF _TG_0180[] IS NOT INITIAL.
      "Documentos de Carga
      SELECT *
        FROM ZSDT0181 INTO TABLE @DATA(_TG_0181)
         FOR ALL ENTRIES IN @_TG_0180
       WHERE ID_ENTREGA = @_TG_0180-ID_ENTREGA
         AND NUMERO_DUE = @_TG_0180-NUMERO_DUE.
    ENDIF.

    ME->SET_CABECALHO( I_ZSDT0179 = _WL_0179 ).

    "Carrega Documentos
    LOOP AT _TG_0180 INTO DATA(WL_0180).
      ME->ADD_DOCUMENTO( I_ZSDT0180 = WL_0180 ).
    ENDLOOP.

    "Carrega Documentos de Carga
    LOOP AT _TG_0181 INTO DATA(WL_0181).
      ME->ADD_DOCUMENTO_CARGA( I_ZSDT0181 = WL_0181 ).
    ENDLOOP.


  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~ADD_DOCUMENTO.

    CHECK I_ZSDT0180 IS NOT INITIAL.

    APPEND I_ZSDT0180 TO ME->AT_DOCUMENTOS.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~ADD_DOCUMENTO_CARGA.

    CHECK I_ZSDT0181 IS NOT INITIAL.

    APPEND I_ZSDT0181 TO ME->AT_DOCUMENTOS_CARGA.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~CANCELAR_ENTREGA.

    R_CANCELADA = ABAP_FALSE.

    SELECT SINGLE *
      FROM ZSDT0179 INTO @DATA(WL_0179)
     WHERE ID_ENTREGA = @ME->AT_ENTREGA-ID_ENTREGA.

    IF ( SY-SUBRC NE 0 ) OR ( ME->AT_ENTREGA-ID_ENTREGA IS INITIAL ).
      MESSAGE S108 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*    IF ( ME->AT_RECEPCAO-STATUS EQ '1' ).
*      MESSAGE S030 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.


    WL_0179-CANCEL    = 'X'.
    WL_0179-DT_CANCEL = SY-DATUM.
    WL_0179-HR_CANCEL = SY-UZEIT.
    WL_0179-US_CANCEL = SY-UNAME.

    MODIFY ZSDT0179 FROM WL_0179.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE S109 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    R_CANCELADA = ABAP_TRUE.

    MESSAGE S110 DISPLAY LIKE 'S'.



  endmethod.


  METHOD ZIF_CCT_ENTREGA_CARGA~ENVIAR_ENTREGA.

    DATA: HTTP_CLIENT     TYPE REF TO IF_HTTP_CLIENT,
          XML_RETURN      TYPE REF TO CL_XML_DOCUMENT,
          RETURN_CODE     TYPE I,
          E_RESULTADO     TYPE STRING,
          V_XML           TYPE STRING,
          V_AUTHORIZATION TYPE STRING,
          V_X_CSRF_TOKEN  TYPE STRING,
          V_URL           TYPE UI_SRC_URL,
          V_MSG_SHOW      TYPE C LENGTH 200,
          VL_INI_POS      TYPE I.

    CLEAR: V_AUTHORIZATION , V_X_CSRF_TOKEN.

    R_ENVIADA = ABAP_FALSE.

    IF ( ME->AT_TOKEN IS INITIAL ).
      MESSAGE S018 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DATA(_ID_TOKEN) = ME->AT_TOKEN->GET_ID_TOKEN( ).
    IF _ID_TOKEN IS INITIAL.
      MESSAGE S018 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->AT_ENTREGA-STATUS = '1'.
      MESSAGE S056 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = TEXT-001.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    CASE ME->AT_ENTREGA-TP_ENTREGA.
      WHEN '1'.  "DU-E/RUC
        DATA(_TIPO_REC) = 'DU-E/RUC'.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO @DATA(_AUTH_SERVICE)
         WHERE SERVICE = 'CCT_ENTREGA_CARGA_DUE_RUC'.
      WHEN '2'. "Contêiner
        _TIPO_REC = 'Contêiner'.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO _AUTH_SERVICE
         WHERE SERVICE = 'CCT_ENTREGA_CARGA_CONTEINER'.
      WHEN '3'. "Documento de Transporte
        _TIPO_REC = 'Documento de Transporte'.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO _AUTH_SERVICE
         WHERE SERVICE = 'CCT_ENTREGA_CARGA_DOC_TRANSP'.
    ENDCASE.

    IF ( SY-SUBRC NE 0 ) OR ( _AUTH_SERVICE-URL IS INITIAL ).
      MESSAGE S057 WITH _TIPO_REC DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      V_URL = _AUTH_SERVICE-URL.
    ENDIF.

    ME->MONTA_XML( RECEIVING R_XML  = V_XML ).

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN V_XML WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN V_XML WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN V_XML WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN V_XML WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN V_XML WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN V_XML WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN V_XML WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN V_XML WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN V_XML WITH 'o' IGNORING CASE.

    IF V_XML IS INITIAL.
      MESSAGE S058 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    "Call service
    CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
      EXPORTING
        URL                = CONV #( V_URL )
        SSL_ID             = 'DFAULT' "ME->AT_PAR_CCT-SSL_ID
      IMPORTING
        CLIENT             = HTTP_CLIENT
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~request_method'
        VALUE = 'POST'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~server_protocol'
        VALUE = 'HTTP/1.1'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Content-Type'
        VALUE = 'application/xml; charset=UTF-8'.

    V_AUTHORIZATION = ME->AT_TOKEN->GET_TOKEN_JW( ).

    DATA(_TOKEN_OPUS) = ME->AT_TOKEN->GET_TOKEN_OPUS( ).
    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Authorization'
        VALUE = _TOKEN_OPUS.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'AuthorizationReceita'
        VALUE = V_AUTHORIZATION.

    V_X_CSRF_TOKEN  = ME->AT_TOKEN->GET_TOKEN_CSRF( ).

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'X-CSRF-Token'
        VALUE = V_X_CSRF_TOKEN.

*  CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
*    EXPORTING
*      NAME  = 'certificado'
*      VALUE = CONV #( ME->AT_PAR_CCT-SSL_ID ).
*
*  CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
*    EXPORTING
*      NAME  = 'senha'
*      VALUE = CONV #( ME->AT_PAR_CCT-PASSWORD ).

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
        MESSAGE S024 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_COMMUNICATION_FAILURE.
        EXIT.
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S025 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_INVALID_STATE.
        EXIT.
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S026 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_PROCESSING_FAILED.
        EXIT.
      WHEN 4.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S027 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_INVALID_TIMEOUT.
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
        MESSAGE S024 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_COMMUNICATION_FAILURE.
        EXIT.
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S025 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_INVALID_STATE.
        EXIT.
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S026 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_PROCESSING_FAILED.
        EXIT.
    ENDCASE.

    "//Check return content
    CREATE OBJECT XML_RETURN.

    CALL METHOD XML_RETURN->PARSE_STRING
      EXPORTING
        STREAM = HTTP_CLIENT->RESPONSE->GET_CDATA( ).


    HTTP_CLIENT->RESPONSE->GET_STATUS( IMPORTING CODE = RETURN_CODE ).

    E_RESULTADO = HTTP_CLIENT->RESPONSE->GET_CDATA( ).

    IF RETURN_CODE NE '200'.
      CLEAR: VL_INI_POS.

      IF VL_INI_POS IS INITIAL.
        FIND '(400)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '400'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(401)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '401'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(403)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '403'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(404)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '404'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(422)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '422'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(500)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '500'.
        ENDIF.
      ENDIF.

      IF VL_INI_POS IS INITIAL.
        FIND '(503)' IN E_RESULTADO MATCH OFFSET VL_INI_POS.
        IF VL_INI_POS IS NOT INITIAL.
          RETURN_CODE = '503'.
        ENDIF.
      ENDIF.

      CASE RETURN_CODE.
        WHEN 400.
          MESSAGE 'Requisição mal formatada!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
          WHEN 401.
          MESSAGE 'Requisição requer autenticação!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 403.
          MESSAGE 'Requisição não autorizada!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 404.
          MESSAGE 'Registro não encontrado!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 422.
          MESSAGE 'Erro de negócio!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 500.
          MESSAGE 'Erro interno do servidor!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN 503.
          MESSAGE 'Serviço indisponível!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
        WHEN OTHERS.
          MESSAGE 'Recepção não processada!(Siscomex)' TYPE 'S' RAISING ZCX_ENVIO.
      ENDCASE.

      RETURN.
    ENDIF.

    CALL METHOD XML_RETURN->FIND_NODE_TABLE
      EXPORTING
        TABNAME = 'error'
      IMPORTING
        T_NODES = DATA(_NODES_ERROR).

    IF _NODES_ERROR[] IS NOT INITIAL.

      TRY.
          DATA(_CODE)    = CAST IF_IXML_NODE( _NODES_ERROR[ 1 ]-NODE )->GET_VALUE( ).
          DATA(_MESSAGE) = CAST IF_IXML_NODE( _NODES_ERROR[ 2 ]-NODE )->GET_VALUE( ).
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

      IF _MESSAGE IS NOT INITIAL.
        V_MSG_SHOW = _CODE && _MESSAGE.
        MESSAGE S000 WITH V_MSG_SHOW(50) V_MSG_SHOW+50(50) V_MSG_SHOW+100(50) V_MSG_SHOW+150(50)  DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        MESSAGE S021 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ELSE.

      "Get Elemento error
      CALL METHOD XML_RETURN->FIND_SIMPLE_ELEMENT
        EXPORTING
          NAME  = 'error'
        RECEIVING
          VALUE = DATA(_RETURN_ERROR).

      IF _RETURN_ERROR IS NOT INITIAL.
        CALL METHOD XML_RETURN->FIND_SIMPLE_ELEMENT
          EXPORTING
            NAME  = 'message'
          RECEIVING
            VALUE = DATA(_RETURN_MESSAGE).

        IF _RETURN_MESSAGE IS NOT INITIAL.
          V_MSG_SHOW = _RETURN_MESSAGE.
          MESSAGE S000 WITH V_MSG_SHOW(50) V_MSG_SHOW+50(50) V_MSG_SHOW+100(50) V_MSG_SHOW+150(50)  DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDIF.

      CALL METHOD XML_RETURN->FIND_NODE_TABLE
        EXPORTING
          TABNAME = 'mensagem'
        IMPORTING
          T_NODES = DATA(_NODES).

      TRY.
          _CODE    = CAST IF_IXML_NODE( _NODES[ 1 ]-NODE )->GET_VALUE( ).
          _MESSAGE = CAST IF_IXML_NODE( _NODES[ 2 ]-NODE )->GET_VALUE( ).
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

      IF ( _CODE EQ 'CCTR-IN0002' ).

        ME->AT_ENTREGA-STATUS = '1'. "Entrega Registrada no Portal.

        UPDATE ZSDT0179 SET STATUS = '1'
         WHERE ID_ENTREGA = ME->AT_ENTREGA-ID_ENTREGA.

        COMMIT WORK.

        R_ENVIADA = ABAP_TRUE.

      ELSE.
        IF _MESSAGE IS NOT INITIAL.
          MESSAGE S022 WITH _MESSAGE DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          MESSAGE S021 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  method ZIF_CCT_ENTREGA_CARGA~GRAVAR_REGISTRO.

    R_GRAVOU = ABAP_FALSE.

    CHECK ME->CK_ALTEROU EQ ABAP_TRUE.

    CHECK ME->VALIDAR_REGISTRO( ) EQ ABAP_TRUE.

    IF ME->AT_ENTREGA-ID_ENTREGA IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZCCT_ECG'
        IMPORTING
          NUMBER                  = ME->AT_ENTREGA-ID_ENTREGA
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.

      IF ( SY-SUBRC IS NOT INITIAL ) OR ( ME->AT_ENTREGA-ID_ENTREGA IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE S052 WITH 'ZCCT_ECG' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ENDIF.

    ME->AT_ENTREGA-DT_ENTREGA = SY-DATUM.
    ME->AT_ENTREGA-HR_ENTREGA = SY-UZEIT.
    ME->AT_ENTREGA-US_ENTREGA = SY-UNAME.

    MODIFY ZSDT0179 FROM ME->AT_ENTREGA.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE S053 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    LOOP AT ME->AT_DOCUMENTOS ASSIGNING FIELD-SYMBOL(<FS_DOC>).
      <FS_DOC>-ID_ENTREGA = ME->AT_ENTREGA-ID_ENTREGA.
    ENDLOOP.

    DELETE FROM ZSDT0180 WHERE ID_ENTREGA EQ ME->AT_ENTREGA-ID_ENTREGA.
    IF ME->AT_DOCUMENTOS[] IS NOT INITIAL.
      MODIFY ZSDT0180 FROM TABLE ME->AT_DOCUMENTOS.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE S054 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    LOOP AT ME->AT_DOCUMENTOS_CARGA ASSIGNING FIELD-SYMBOL(<FS_DOC_CARGA>).
      <FS_DOC_CARGA>-ID_ENTREGA = ME->AT_ENTREGA-ID_ENTREGA.
    ENDLOOP.

    DELETE FROM ZSDT0181 WHERE ID_ENTREGA EQ ME->AT_ENTREGA-ID_ENTREGA.
    IF ME->AT_DOCUMENTOS_CARGA[] IS NOT INITIAL.
      MODIFY ZSDT0181 FROM TABLE ME->AT_DOCUMENTOS_CARGA.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE S061 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

*    "Deletar Registros Irrelevantes
*    LOOP AT ME->AT_DOCUMENTOS_CARGA INTO DATA(_WL_DOC_CARGA).
*
*      "Check se linha de documento corresponde existe
*      READ TABLE ME->AT_DOCUMENTOS INTO DATA(_WL_DOC) WITH KEY ID_ENTREGA = _WL_DOC_CARGA-ID_ENTREGA
*                                                               NUMERO_DUE = _WL_DOC_CARGA-NUMERO_DUE.
*
*      IF SY-SUBRC NE 0.
*        DELETE FROM ZSDT0181 WHERE ID_ENTREGA = _WL_DOC_CARGA-ID_ENTREGA
*                               AND NUMERO_DUE = _WL_DOC_CARGA-NUMERO_DUE.
*        IF SY-SUBRC NE 0.
*          ROLLBACK WORK.
*          MESSAGE S062 DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

    ME->CK_ALTEROU = ABAP_FALSE.
    R_GRAVOU = ABAP_TRUE.
    MESSAGE S055.

  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~MONTA_XML.

    DATA: XVALOR            TYPE STRING,
          V_VALOR_AUX       TYPE STRING,
          V_XML_DOC         TYPE STRING,
          V_XML_ENTREGA     TYPE STRING,
          GT_DOCS_CARGA_AUX TYPE TABLE OF ZSDT0181.

    CLEAR: R_XML, V_XML_DOC, V_XML_ENTREGA.

    DEFINE CONC_XML.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE R_XML XVALOR INTO R_XML.
    END-OF-DEFINITION.

    DEFINE CONC_XML_DOC.
     CLEAR: XVALOR.
     XVALOR = &1.
     CONCATENATE V_XML_DOC XVALOR INTO V_XML_DOC .
    END-OF-DEFINITION.

    DEFINE CONC_XML_ENTREGA.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE V_XML_ENTREGA XVALOR INTO V_XML_ENTREGA.
    END-OF-DEFINITION.

    CASE ME->AT_ENTREGA-TP_ENTREGA.
      WHEN '1'. "DU-E/RUC

        "Monta XML Documentos.
        LOOP AT ME->AT_DOCUMENTOS INTO DATA(WL_DOC).
          CONC_XML_DOC   '<documento>'.
          CONC_XML_DOC     '<numeroDUE>'.
          CONC_XML_DOC        WL_DOC-NUMERO_DUE.
          CONC_XML_DOC     '</numeroDUE>'.

          "Fazer Quebra por Tipo de Carga
          GT_DOCS_CARGA_AUX[] = ME->AT_DOCUMENTOS_CARGA[].
          SORT GT_DOCS_CARGA_AUX                                   BY NUMERO_DUE TIPO_CARGA.
          DELETE ADJACENT DUPLICATES FROM GT_DOCS_CARGA_AUX COMPARING NUMERO_DUE TIPO_CARGA.

          LOOP AT GT_DOCS_CARGA_AUX INTO DATA(WL_DOCS_CARGA_AUX) WHERE NUMERO_DUE EQ WL_DOC-NUMERO_DUE.

            CASE WL_DOCS_CARGA_AUX-TIPO_CARGA.
              WHEN '1'. "Solta ou Veículo
*                CONC_XML_DOC    '<cargaSoltaVeiculo>'.
*                CONC_XML_DOC      '<carga>'.
*                CONC_XML_DOC        '<tipoEmbalagem>19</tipoEmbalagem>'.
*                CONC_XML_DOC        '<total>2000</total>'.
*                CONC_XML_DOC        '<quantidade>2000</quantidade>'.
*                CONC_XML_DOC      '</carga>'.
*                CONC_XML_DOC      '<carga>'.
*                CONC_XML_DOC        '<total>10</total>'.
*                CONC_XML_DOC        '<quantidade>2</quantidade>'.
*                CONC_XML_DOC      '</carga>'.
*                CONC_XML_DOC    '</cargaSoltaVeiculo>'.
              WHEN '2'. "Granel

                CONC_XML_DOC   	'<granel>'.

                LOOP AT ME->AT_DOCUMENTOS_CARGA INTO DATA(WL_DOC_CARGA) WHERE NUMERO_DUE EQ WL_DOCS_CARGA_AUX-NUMERO_DUE
                                                                          AND TIPO_CARGA EQ WL_DOCS_CARGA_AUX-TIPO_CARGA.
                  CONC_XML_DOC      '<carga>'.
                  CONC_XML_DOC        '<tipoGranel>'.
                  CONC_XML_DOC           WL_DOC_CARGA-TIPO_GRANEL.
                  CONC_XML_DOC        '</tipoGranel>'.
                  CONC_XML_DOC        '<unidademedida>'.

                  V_VALOR_AUX  = WL_DOC_CARGA-UNID_MEDIDA.
                  TRANSLATE V_VALOR_AUX TO LOWER CASE.

                  CONC_XML_DOC           V_VALOR_AUX.
                  CONC_XML_DOC        '</unidademedida>'.
                  CONC_XML_DOC        '<total>'.
                  CONC_XML_DOC           WL_DOC_CARGA-PESO_BRUTO_TOTAL.
                  CONC_XML_DOC        '</total>'.
                  CONC_XML_DOC        '<quantidade>'.
                  CONC_XML_DOC           WL_DOC_CARGA-PESO_BRUTO_ENTREGUE.
                  CONC_XML_DOC        '</quantidade>'.
                  CONC_XML_DOC      '</carga>'.

                ENDLOOP.

                CONC_XML_DOC    '</granel>'.
            ENDCASE.

          ENDLOOP.

          CONC_XML_DOC   '</documento>'.
        ENDLOOP.

        CONC_XML_ENTREGA    '<entregaDocumentoCarga>'.
        CONC_XML_ENTREGA        '<identificacaoEntrega>'.
        CONC_XML_ENTREGA            ME->AT_ENTREGA-ID_ENTREGA.
        CONC_XML_ENTREGA        '</identificacaoEntrega>'.
        CONC_XML_ENTREGA        '<identificacaoPessoaJuridica>'.
        CONC_XML_ENTREGA            ME->AT_ENTREGA-CNPJ_RESPONSAVEL.
        CONC_XML_ENTREGA        '</identificacaoPessoaJuridica>'.
        CONC_XML_ENTREGA        '<local>'.
        CONC_XML_ENTREGA            '<codigoURF>'.
        CONC_XML_ENTREGA               ME->AT_ENTREGA-LOCAL_CODIGO_URF.
        CONC_XML_ENTREGA            '</codigoURF>'.
        CONC_XML_ENTREGA            '<codigoRA>'.
        CONC_XML_ENTREGA               ME->AT_ENTREGA-LOCAL_CODIGO_RA.
        CONC_XML_ENTREGA            '</codigoRA>'.
        CONC_XML_ENTREGA        '</local>'.
        CONC_XML_ENTREGA        '<documentos>'.
        CONC_XML_ENTREGA            V_XML_DOC.
        CONC_XML_ENTREGA        '</documentos>'.
        CONC_XML_ENTREGA        '<recebedor>'.

        IF ME->AT_ENTREGA-REC_CNPJ IS NOT INITIAL.
          CONC_XML_ENTREGA          '<cnpj>'.
          CONC_XML_ENTREGA              ME->AT_ENTREGA-REC_CNPJ.
          CONC_XML_ENTREGA          '</cnpj>'.
        ENDIF.

        IF ME->AT_ENTREGA-REC_CPF IS NOT INITIAL.
          CONC_XML_ENTREGA          '<cpf>'.
          CONC_XML_ENTREGA              ME->AT_ENTREGA-REC_CPF.
          CONC_XML_ENTREGA          '</cpf>'.
        ENDIF.

        IF ME->AT_ENTREGA-REC_NOME_ESTRANGEIRO IS NOT INITIAL.
          CONC_XML_ENTREGA          '<nomeEstrangeiro>'.
          CONC_XML_ENTREGA              ME->AT_ENTREGA-REC_NOME_ESTRANGEIRO.
          CONC_XML_ENTREGA          '</nomeEstrangeiro>'.
        ENDIF.

        CONC_XML_ENTREGA            '<viaTransporte>'.
        CONC_XML_ENTREGA               ME->AT_ENTREGA-REC_VIA_TRANSPORTE.
        CONC_XML_ENTREGA            '</viaTransporte>'.
        CONC_XML_ENTREGA            '<baldeacaoOuTransbordo>'.
        CONC_XML_ENTREGA               ME->AT_ENTREGA-REC_BALDEACAO_TRANSBORDO.
        CONC_XML_ENTREGA            '</baldeacaoOuTransbordo>'.
        CONC_XML_ENTREGA        '</recebedor>'.
        CONC_XML_ENTREGA        '<pesoAferido>'.
        CONC_XML_ENTREGA            ME->AT_ENTREGA-PESO_AFERIDO.
        CONC_XML_ENTREGA        '</pesoAferido>'.
        CONC_XML_ENTREGA    '</entregaDocumentoCarga>'.

        CONC_XML  '<entregasDocumentoCarga xmlns="http://www.pucomex.serpro.gov.br/cct">'.
        CONC_XML      V_XML_ENTREGA.
        CONC_XML  '</entregasDocumentoCarga>'.

    ENDCASE.


  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~REGISTRO_ENTREGA.

    CALL FUNCTION 'ZCCT_REGISTRO_ENTREGA'
      EXPORTING
        I_REGISTRO_ENTREGA = I_REGISTRO_ENTREGA
        I_ZSDT0170         = I_ZSDT0170.

  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~SET_CABECALHO.

    ME->AT_ENTREGA = I_ZSDT0179.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~SET_CNPJ_RESPONSAVEL.

    DATA: WL_BRANCH_DETAIL TYPE BAPIBRANCH.

    IF I_CNPJ_RESPONSAVEL IS NOT INITIAL.

      ME->AT_ENTREGA-CNPJ_RESPONSAVEL = I_CNPJ_RESPONSAVEL.

    ELSEIF ( ME->AT_ENTREGA-BUKRS IS NOT INITIAL ) AND ( ME->AT_ENTREGA-BRANCH IS NOT INITIAL ).

      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          COMPANY         = ME->AT_ENTREGA-BUKRS
          BRANCH          = ME->AT_ENTREGA-BRANCH
        IMPORTING
          BRANCH_DETAIL   = WL_BRANCH_DETAIL.

      IF WL_BRANCH_DETAIL-CGC_NUMBER IS NOT INITIAL.
        ME->AT_ENTREGA-CNPJ_RESPONSAVEL = WL_BRANCH_DETAIL-CGC_NUMBER.
      ENDIF.

    ENDIF.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method ZIF_CCT_ENTREGA_CARGA~SET_TOKEN.

    FREE ME->AT_TOKEN.

    CHECK I_TOKEN IS NOT INITIAL.

    IF ( I_TOKEN->GET_BUKRS( )  NE ME->AT_ENTREGA-BUKRS  ).
      MESSAGE S028 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    ME->AT_TOKEN = I_TOKEN.


  endmethod.


  METHOD zif_cct_entrega_carga~validar_registro.

    DATA: gt_0181             TYPE TABLE OF zsdt0181,
          gt_0181_doc         TYPE TABLE OF zsdt0181,
          v_tot_peso_entregue TYPE zsdt0181-peso_bruto_entregue.

    r_validou = abap_false.

    IF ( me->at_entrega-tp_entrega NE '1' ) AND  "DU-E/RUC
       ( me->at_entrega-tp_entrega NE '2' ) AND  "Contêiner
       ( me->at_entrega-tp_entrega NE '3' ).     "Documento de Transporte
      MESSAGE s010 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE me->at_entrega-tp_entrega.
      WHEN '1'. "DU-E/RUC

        IF me->at_entrega-bukrs IS INITIAL.
          MESSAGE s007 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-branch IS INITIAL.
          MESSAGE s008 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-cnpj_responsavel IS INITIAL.
          MESSAGE s002 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF strlen( me->at_entrega-cnpj_responsavel ) NE 14.
          MESSAGE s154 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-local_codigo_urf IS INITIAL.
          MESSAGE s003 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-local_codigo_ra IS INITIAL.
          MESSAGE s004 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF ( me->at_entrega-rec_cnpj IS INITIAL ) AND ( me->at_entrega-rec_cpf IS INITIAL ) AND ( me->at_entrega-rec_nome_estrangeiro IS INITIAL  ).
          MESSAGE s041 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF ( me->at_entrega-rec_cnpj IS NOT INITIAL ).
          IF strlen( me->at_entrega-rec_cnpj ) NE 14.
            MESSAGE s152 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

        IF ( me->at_entrega-rec_cpf IS NOT INITIAL ).
          IF strlen( me->at_entrega-rec_cpf ) NE 11.
            MESSAGE s153 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

        IF ( ( me->at_entrega-rec_cnpj IS NOT INITIAL ) OR ( me->at_entrega-rec_cpf IS NOT INITIAL ) ) AND
             ( me->at_entrega-rec_nome_estrangeiro IS NOT INITIAL  ).
          MESSAGE s064 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-rec_via_transporte IS INITIAL.
          MESSAGE s042 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-rec_baldeacao_transbordo IS INITIAL.
          MESSAGE s043 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF me->at_entrega-peso_aferido IS INITIAL.
          MESSAGE s006 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        DELETE me->at_documentos WHERE numero_due IS INITIAL.

        IF me->at_documentos[] IS INITIAL.
          MESSAGE s044 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CLEAR: v_tot_peso_entregue.

        LOOP AT me->at_documentos INTO DATA(wl_doc).

          IF ( 1 = 2 ). "Se Recinto Alfandegado for proprio, check status DU-e.
            IF wl_doc-numero_due IS INITIAL.
              MESSAGE s046 DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            SELECT SINGLE *
              FROM zsdt0170 INTO @DATA(_wl_0170)
             WHERE numero_due EQ @wl_doc-numero_due
               AND tp_due EQ '2'. "Com NF-e

            IF sy-subrc NE 0.
              MESSAGE s045 WITH wl_doc-numero_due DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            IF _wl_0170-status NE '1'. "Registrada no Portal.
              MESSAGE s060 WITH wl_doc-numero_due DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            IF wl_doc-numero_due IS INITIAL.
              wl_doc-numero_due = _wl_0170-numero_due.
            ENDIF.

            IF wl_doc-numero_ruc IS INITIAL.
              wl_doc-numero_ruc = _wl_0170-numero_ruc.
            ENDIF.
          ENDIF.

*** CS2019001041 - Ajustes na ZLES0147 - Inicio - CBRAND
          SELECT SINGLE *
            FROM zsdt0170 INTO @DATA(_wl_0170_aux)
           WHERE numero_due EQ @wl_doc-numero_due.

          IF sy-subrc EQ 0.
            IF wl_doc-ds_nome_transpor IS NOT INITIAL OR wl_doc-cnpj_declarante  IS NOT INITIAL.
              MESSAGE s157 WITH wl_doc-numero_due DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
          ELSE.
            IF wl_doc-ds_nome_transpor IS INITIAL OR wl_doc-cnpj_declarante  IS  INITIAL.
              MESSAGE s158 WITH wl_doc-numero_due DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
          ENDIF.
***  CS2019001041 - Ajustes na ZLES0147 - Fim - CBRAND

          READ TABLE me->at_documentos_carga INTO DATA(wl_doc_carga) WITH KEY numero_due = wl_doc-numero_due.
          IF sy-subrc NE 0.
            MESSAGE s059 WITH wl_doc-numero_due DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          CLEAR: gt_0181[], gt_0181_doc[].
          LOOP AT me->at_documentos_carga INTO wl_doc_carga WHERE numero_due EQ wl_doc-numero_due.

            IF wl_doc_carga-tipo_carga NE '2'. "Granel
              MESSAGE s047 WITH wl_doc-numero_due DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            CASE wl_doc_carga-tipo_carga.
              WHEN '2'. "Granel

                IF wl_doc_carga-tipo_granel IS INITIAL.
                  MESSAGE s048 WITH wl_doc-numero_due  DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.

                IF wl_doc_carga-unid_medida IS INITIAL.
                  MESSAGE s049 WITH wl_doc-numero_due  DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.

                IF wl_doc_carga-peso_bruto_total IS INITIAL.
                  MESSAGE s050 WITH wl_doc-numero_due  DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.

                IF wl_doc_carga-peso_bruto_entregue IS INITIAL.
                  MESSAGE s051 WITH wl_doc-numero_due  DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.

            ENDCASE.

            APPEND wl_doc_carga TO gt_0181.
            APPEND wl_doc_carga TO gt_0181_doc.

            ADD wl_doc_carga-peso_bruto_entregue TO v_tot_peso_entregue.

          ENDLOOP.

          "Check Duplicidade Cargas
          SORT gt_0181                                   BY tipo_carga tipo_granel.
          DELETE ADJACENT DUPLICATES FROM gt_0181 COMPARING tipo_carga tipo_granel.
          IF lines( gt_0181 ) <> lines( gt_0181_doc ).
            MESSAGE s063 WITH wl_doc-numero_due  DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF  me->at_entrega-peso_aferido NE v_tot_peso_entregue.
            MESSAGE s111 WITH me->at_entrega-peso_aferido v_tot_peso_entregue DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

        ENDLOOP.

      WHEN '2'. "Contêiner
      WHEN '3'. "Documento de Transporte
    ENDCASE.

    r_validou = abap_true.

  ENDMETHOD.
ENDCLASS.

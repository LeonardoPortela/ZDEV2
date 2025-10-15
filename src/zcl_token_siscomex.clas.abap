class ZCL_TOKEN_SISCOMEX definition
  public
  final
  create public .

public section.

  interfaces ZIF_CADASTRO .

  aliases CK_ALTEROU
    for ZIF_CADASTRO~CK_ALTEROU .
  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR_REGISTRO
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  methods SET_ID_TOKEN
    importing
      value(I_ID_TOKEN) type ZID_TOKEN_CCT .
  methods GET_ID_TOKEN
    returning
      value(E_ID_TOKEN) type ZID_TOKEN_CCT .
  methods CONSTRUCTOR
    importing
      value(I_ID_TOKEN) type ZID_TOKEN_CCT optional .
  methods AUTENTICAR
    returning
      value(E_AUTENTICADO) type CHAR01
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT .
  methods SET_BUKRS
    importing
      value(I_BUKRS) type BUKRS .
  methods GET_BUKRS
    returning
      value(E_BUKRS) type BUKRS .
  methods SET_ROLE_TYPE
    importing
      !I_ROLE_TYPE type ZDE_ROLE_TYPE .
  methods GET_TOKEN_JW
    returning
      value(E_TOKEN_JW) type ZCHAR4000 .
  methods SET_TOKEN_JW
    importing
      value(I_TOKEN) type ZCHAR4000 .
  methods GET_TOKEN_CSRF
    returning
      value(E_TOKEN_CSRF) type ZCHAR250 .
  methods SET_TOKEN_CSRF
    importing
      value(I_TOKEN_CSRF) type ZCHAR250 .
  methods GET_CSRF_EXPIRATION
    returning
      value(E_CSRF_EXPIRATION) type ZTOKEN_EXPIRATION .
  methods SET_CSRF_EXPIRATION
    importing
      value(I_CSRF_EXPIRATION) type ZTOKEN_EXPIRATION .
  methods GET_TOKEN_OPUS
    returning
      value(E_TOKEN_OPUS) type STRING .
protected section.
private section.

  data AT_TOKEN type ZLEST0148 .
  data AT_PARAMETRO type ZAUTH_WS_0001 .
  data AT_TOKEN_OPUS type STRING .

  methods SET_PARAMETROS
    returning
      value(E_PAR_DEFINIDO) type CHAR01 .
ENDCLASS.



CLASS ZCL_TOKEN_SISCOMEX IMPLEMENTATION.


  METHOD AUTENTICAR.

    DATA: HTTP_CLIENT         TYPE REF TO IF_HTTP_CLIENT,
          RETURN_CODE         TYPE I,
          V_BUKRS_C           TYPE STRING,
          V_SET_TOKEN         TYPE STRING,
          V_X_CSRF_TOKEN      TYPE STRING,
          V_X_CSRF_EXPIRATION TYPE STRING,
          V_URL               TYPE UI_SRC_URL,
          WL_TOKEN_JW         TYPE ZCHAR4000,
          WL_TOKEN_CSRF       TYPE ZLEST0148-TOKEN_CSRF,
          WL_CSRF_EXPIRATION  TYPE ZLEST0148-CSRF_EXPIRATION.

    CLEAR: V_SET_TOKEN , V_X_CSRF_TOKEN, V_X_CSRF_EXPIRATION,
           WL_TOKEN_JW , WL_TOKEN_CSRF , WL_CSRF_EXPIRATION.

    E_AUTENTICADO = ABAP_FALSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = TEXT-001.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    SELECT SINGLE *
      FROM ZAUTH_WEBSERVICE INTO @DATA(_AUTH_SERVICE)
     WHERE SERVICE = 'PORTAL_SISCOMEX_AUTENTICAR'.

    IF ( SY-SUBRC NE 0 ) OR ( _AUTH_SERVICE-URL IS INITIAL ).
      MESSAGE S012 DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      V_URL = _AUTH_SERVICE-URL.
    ENDIF.

    "Definir parâmetros
    DATA(_PAR_DEFINIDO) = ME->SET_PARAMETROS( ).

    CHECK _PAR_DEFINIDO IS NOT INITIAL.

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

    ZCL_WEBSERVICE=>ZIF_WEBSERVICE~ADD_TOKEN_OPUS_HTTP_CLIENTE(
      EXPORTING
        I_URL_DESTINO              = CONV #( V_URL )
        I_URL_TOKEN                = CONV #( _AUTH_SERVICE-URL_TOKEN )
      CHANGING
        I_HTTP                     = HTTP_CLIENT
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5  ).

    CALL METHOD HTTP_CLIENT->REQUEST->GET_HEADER_FIELD
      EXPORTING
        NAME  = 'Authorization'
      RECEIVING
        VALUE = ME->AT_TOKEN_OPUS.

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

    IF ME->AT_PARAMETRO-ADD01 IS NOT INITIAL. "Perfil definido no parametro da Empresa
      CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = 'roletype'
          VALUE = CONV #( ME->AT_PARAMETRO-ADD01 ).
    ELSE.
      CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = 'roletype'
          VALUE = CONV #( ME->AT_TOKEN-ROLE_TYPE ).
    ENDIF.

    V_BUKRS_C = ME->AT_PARAMETRO-BUKRS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = V_BUKRS_C
      IMPORTING
        OUTPUT = V_BUKRS_C.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'certificado'
        VALUE = CONV #( V_BUKRS_C ).

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'senha'
        VALUE = CONV #( ME->AT_PARAMETRO-PASSWORD ).

    CALL METHOD HTTP_CLIENT->SEND
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4.

    CASE SY-SUBRC.
      WHEN 1.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S008 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_COMMUNICATION_FAILURE.
        EXIT.
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S009 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_INVALID_STATE.
        EXIT.
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S010 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_PROCESSING_FAILED.
        EXIT.
      WHEN 4.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S011 WITH | { V_URL } (Send) | DISPLAY LIKE 'E' RAISING HTTP_INVALID_TIMEOUT.
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
        MESSAGE S008 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_COMMUNICATION_FAILURE.
        EXIT.
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S009 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_INVALID_STATE.
        EXIT.
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        MESSAGE S010 WITH | { V_URL } (Receive) |  DISPLAY LIKE 'E' RAISING HTTP_PROCESSING_FAILED.
        EXIT.
    ENDCASE.

    "Return Header Response
    V_SET_TOKEN         = HTTP_CLIENT->RESPONSE->GET_HEADER_FIELD( 'Set-Token' ).
    V_X_CSRF_TOKEN      = HTTP_CLIENT->RESPONSE->GET_HEADER_FIELD( 'X-CSRF-Token' ).
    V_X_CSRF_EXPIRATION = HTTP_CLIENT->RESPONSE->GET_HEADER_FIELD( 'X-CSRF-Expiration' ).

    "Check return content
    HTTP_CLIENT->RESPONSE->GET_STATUS( IMPORTING CODE = RETURN_CODE ).

    HTTP_CLIENT->CLOSE( ).

    IF RETURN_CODE NE '200'.
      MESSAGE S007 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    WL_TOKEN_JW         = V_SET_TOKEN.
    WL_TOKEN_CSRF       = V_X_CSRF_TOKEN.
    WL_CSRF_EXPIRATION  = V_X_CSRF_EXPIRATION.

    ME->SET_TOKEN_JW(        WL_TOKEN_JW ).
    ME->SET_TOKEN_CSRF(      WL_TOKEN_CSRF ).
    ME->SET_CSRF_EXPIRATION( WL_CSRF_EXPIRATION ).

    E_AUTENTICADO = ABAP_TRUE.

  ENDMETHOD.


  method CONSTRUCTOR.

    DATA: V_TOKEN_JW TYPE ZCHAR4000.

    CLEAR: ME->AT_TOKEN.

    CHECK ( I_ID_TOKEN IS NOT INITIAL ).

    "Dados Token
    SELECT SINGLE *
      FROM ZLEST0148 INTO @DATA(_WL_ZLEST0148)
     WHERE ID_TOKEN = @I_ID_TOKEN.

    CHECK SY-SUBRC = 0.

    CONCATENATE _WL_ZLEST0148-TOKEN_JW1
                _WL_ZLEST0148-TOKEN_JW2
                _WL_ZLEST0148-TOKEN_JW3
                _WL_ZLEST0148-TOKEN_JW4
            INTO V_TOKEN_JW.

    ME->SET_ID_TOKEN(         I_ID_TOKEN            = _WL_ZLEST0148-ID_TOKEN ).
    ME->SET_BUKRS(            I_BUKRS               = _WL_ZLEST0148-BUKRS  ).
    ME->SET_TOKEN_JW(         I_TOKEN               =  V_TOKEN_JW ).
    ME->SET_TOKEN_CSRF(       I_TOKEN_CSRF          = _WL_ZLEST0148-TOKEN_CSRF ).
    ME->SET_CSRF_EXPIRATION(  I_CSRF_EXPIRATION     = _WL_ZLEST0148-CSRF_EXPIRATION ).


  endmethod.


  method GET_BUKRS.

    E_BUKRS = ME->AT_TOKEN-BUKRS.

  endmethod.


  method GET_CSRF_EXPIRATION.

    E_CSRF_EXPIRATION = ME->AT_TOKEN-CSRF_EXPIRATION.

  endmethod.


  method GET_ID_TOKEN.

    E_ID_TOKEN = ME->AT_TOKEN-ID_TOKEN.

  endmethod.


  method GET_TOKEN_CSRF.

    E_TOKEN_CSRF = ME->AT_TOKEN-TOKEN_CSRF.

  endmethod.


  method GET_TOKEN_JW.

    CONCATENATE ME->AT_TOKEN-TOKEN_JW1
                ME->AT_TOKEN-TOKEN_JW2
                ME->AT_TOKEN-TOKEN_JW3
                ME->AT_TOKEN-TOKEN_JW4
           INTO E_TOKEN_JW.

  endmethod.


  method GET_TOKEN_OPUS.

   E_TOKEN_OPUS = ME->AT_TOKEN_OPUS.

  endmethod.


  method SET_BUKRS.

    ME->AT_TOKEN-BUKRS = I_BUKRS.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method SET_CSRF_EXPIRATION.

    ME->AT_TOKEN-CSRF_EXPIRATION = I_CSRF_EXPIRATION.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method SET_ID_TOKEN.

    ME->AT_TOKEN-ID_TOKEN = I_ID_TOKEN.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method SET_PARAMETROS.

    E_PAR_DEFINIDO = ABAP_FALSE.

    IF ME->AT_TOKEN-BUKRS IS INITIAL.
      MESSAGE S002 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->AT_TOKEN-ROLE_TYPE IS INITIAL.
      MESSAGE S003 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM ZAUTH_WS_0001 INTO ME->AT_PARAMETRO
     WHERE SERVICE = 'PORTAL_SISCOMEX_AUTENTICAR'
       AND BUKRS   = ME->AT_TOKEN-BUKRS.

    IF ( SY-SUBRC NE 0 ).
      MESSAGE S013 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*    IF ME->AT_PARAMETRO-SSL_ID IS INITIAL.
*      MESSAGE S014 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.

    E_PAR_DEFINIDO = ABAP_TRUE.

  endmethod.


  method SET_ROLE_TYPE.

    ME->AT_TOKEN-ROLE_TYPE = I_ROLE_TYPE.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method SET_TOKEN_CSRF.

    ME->AT_TOKEN-TOKEN_CSRF = I_TOKEN_CSRF.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  method SET_TOKEN_JW.

    ME->AT_TOKEN-TOKEN_JW1 = I_TOKEN+0000(1000).
    ME->AT_TOKEN-TOKEN_JW2 = I_TOKEN+1000(1000).
    ME->AT_TOKEN-TOKEN_JW3 = I_TOKEN+2000(1000).
    ME->AT_TOKEN-TOKEN_JW4 = I_TOKEN+3000(0500).

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  METHOD zif_cadastro~gravar_registro.

    i_gravou = abap_false.

    CHECK me->ck_alterou EQ abap_true.

    "Autenticar no portal Siscomex
    DATA(_autenticado)  = me->autenticar( ).

    CHECK _autenticado IS NOT INITIAL.

    CHECK me->validar_registro( ) EQ abap_true.

    IF me->at_token-id_token IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZCCT_TKN'
        IMPORTING
          number                  = me->at_token-id_token
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE s015 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ENDIF.

    me->at_token-data_reg    = sy-datum.
    me->at_token-hora_reg    = sy-uzeit.
    me->at_token-usuario_reg = sy-uname.

    MODIFY zlest0148 FROM me->at_token.

    me->ck_alterou = abap_false.
    i_gravou = abap_true.

    IF i_job EQ abap_false.
      MESSAGE s001.
    ENDIF.


  ENDMETHOD.


  method ZIF_CADASTRO~LIMPAR_REGISTRO.

   CLEAR: ME->AT_TOKEN.

  endmethod.


  method ZIF_CADASTRO~NOVO_REGISTRO.
   ME->LIMPAR_REGISTRO( ).
  endmethod.


  method ZIF_CADASTRO~VALIDAR_REGISTRO.

    E_VALIDOU = ABAP_FALSE.

    IF ME->AT_TOKEN-BUKRS IS INITIAL.
      MESSAGE S002 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->AT_TOKEN-TOKEN_JW1 IS INITIAL.
      MESSAGE S004 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->AT_TOKEN-TOKEN_CSRF IS INITIAL.
      MESSAGE S005 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->AT_TOKEN-CSRF_EXPIRATION IS INITIAL.
      MESSAGE S006 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    E_VALIDOU = ABAP_TRUE.


  endmethod.
ENDCLASS.

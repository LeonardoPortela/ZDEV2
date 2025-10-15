*&---------------------------------------------------------------------*
*& Report  ZSDR023
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZSDR023.

*-CS2021000218-24.10.2022-#90707-JT-inicio
TABLES: ZSDT0259. "zsdt0218,  zsdt0219.
*-CS2021000218-24.10.2022-#90707-JT-fim
TYPES: BEGIN OF TY_ITENS,
         CODIGOMAPA                     TYPE STRING,
         CODIGOSIGACULTURA              TYPE STRING,
         CODIGOSIGADIAGNOSTICO          TYPE STRING,
         CODIGOSIGAPRODUTO              TYPE STRING,
         CODIGOSIGAUNIDADE              TYPE STRING,
         CODIGOSIGENAGROTOXICO          TYPE STRING,
         CODIGOSIGENPRAGA               TYPE STRING,
         CODSIGENUNIDADEMEDIDAADQUIRIDA TYPE STRING,
         CODIGOSIGENUNIDADEMEDIDAAREA   TYPE STRING,
         CODSIGENUNIDADEMEDIDADOSAGEM   TYPE STRING,
         CODIGOINDEACULTURA             TYPE STRING,
         CODIGOINDEAPRAGA               TYPE STRING,
         CODIGOINDEATIPOAPLICACAO       TYPE STRING,
         CODIGOINDEAUNIDADEMEDIDA       TYPE STRING,
         AREA                           TYPE P DECIMALS 2,
         DOSE                           TYPE P DECIMALS 2,
         QUANTIDADE                     TYPE KWMENG,
         QUANTIDADEAPLICACOES           TYPE P DECIMALS 2,
         QTDSEMENTES                    TYPE P DECIMALS 2,
         INTERVALOSEGURANCA             TYPE STRING,
         DIAGNOSTICO                    TYPE STRING,
         MODALIDADEAPLICACAO            TYPE STRING,
         RECEITAPRODUTOID               TYPE STRING,
         PRODUTOID                      TYPE STRING,
       END OF TY_ITENS,
       IT_ITENS TYPE TABLE OF TY_ITENS WITH DEFAULT KEY.

*-CS2021000218-07.12.2022-#97911-JT-inicio
TYPES: BEGIN OF TY_CABEC_RTC.
         INCLUDE TYPE ZSDE0082.
*         id        TYPE string,
*         nome      TYPE string,
*         sobrenome TYPE string,
*         email     TYPE string,
*         cpf       TYPE string,
*         token     TYPE string,
*         ativo     TYPE string,
TYPES: END OF TY_CABEC_RTC.
*-CS2021000218-07.12.2022-#97911-JT-fim

DATA:  IT_CABEC_RTC TYPE TABLE OF TY_CABEC_RTC WITH DEFAULT KEY.

TYPES: BEGIN OF TY_CABECALHO,
         CANCELADA              TYPE STRING,
         NUMERORECEITA          TYPE STRING,
         NUMEROPEDIDO           TYPE STRING,
         NUMERONF               TYPE STRING,
         DATAEMISSAO            TYPE STRING,
         NUMEROART              TYPE STRING,
         CREA                   TYPE STRING,
         CPFRT                  TYPE STRING,
         NOMECLIENTE            TYPE STRING,
         RAZAOSOCIAL            TYPE STRING,
         CPFCNPJCLIENTE         TYPE STRING,
         TIPOPESSOA             TYPE STRING,
         CODIGOMUNICIPIOIBGE    TYPE STRING,
         CODIGOSIGAMUNICIPIO    TYPE STRING,
         CODIGOSIGENLOCALIDADE  TYPE STRING,
         CODIGOINDEAPROPRIEDADE TYPE STRING,
         RECEITAPRODUTOS        TYPE IT_ITENS,
       END OF TY_CABECALHO,
       IT_CABECALHO TYPE TABLE OF TY_CABECALHO WITH DEFAULT KEY.

TYPES: BEGIN OF TY_RECEITAS,
         SUCCEEDED TYPE STRING,
         ERRORS    TYPE STRING,
         SUCCESSES TYPE STRING,
         ENTITY    TYPE STRING,
         DATA      TYPE IT_CABECALHO,
       END OF TY_RECEITAS.

TYPES: BEGIN OF TY_RTC,
         SUCCEEDED TYPE STRING,
*        errors    TYPE string,
*        successes TYPE string,
*        entity    TYPE string,
*        data TYPE it_cabec_rtc,
       END OF TY_RTC.

TYPES: BEGIN OF TY_ZAUTH_WEBSERVICE,
         SERVICE TYPE ZAUTH_WEBSERVICE-SERVICE,
         URL     TYPE ZAUTH_WEBSERVICE-URL,
       END OF TY_ZAUTH_WEBSERVICE.

TYPES: BEGIN OF TY_TOKEN,
         ACCESS_TOKEN TYPE STRING,
         EXPIRES_IN   TYPE I,
         TOKEN_TYPE   TYPE CHAR6,
       END OF   TY_TOKEN.


DATA: IT_ZSDT0218         TYPE TABLE OF ZSDT0218,
      WA_ZSDT0218         TYPE ZSDT0218,
      IT_ZSDT0219         TYPE TABLE OF ZSDT0219,
      WA_ZSDT0219         TYPE ZSDT0219,
      WA_ZSDT0259         TYPE ZSDT0259,
      IT_RTC              TYPE TABLE OF TY_CABEC_RTC, "ty_rtc,
      W_DADOS_RTC         TYPE ZSDE0081,  "*-CS2021000189-19.10.2023-#125787 -JT-fim
      IT_RECEITAS         TYPE TABLE OF TY_RECEITAS,
*     wa_receitas         TYPE ty_receitas,
      WA_RECEITAS         TYPE ZSDE0070,
      WA_RTC              TYPE TY_CABEC_RTC,
      IT_TOKEN            TYPE  TY_TOKEN,
      IT_ZAUTH_WEBSERVICE TYPE TABLE OF TY_ZAUTH_WEBSERVICE,
      WA_ZAUTH_WEBSERVICE TYPE TY_ZAUTH_WEBSERVICE,
      WA_WEBSERVICE       TYPE TY_ZAUTH_WEBSERVICE,
      IT_ZAUTH_WS_0001    TYPE TABLE OF ZAUTH_WS_0001,
      WA_ZAUTH_WS_0001    TYPE ZAUTH_WS_0001.

DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
DATA: E_REASON     TYPE STRING,
      JSON_RETORNO TYPE STRING,
      E_HTTP       TYPE REF TO  IF_HTTP_CLIENT,
      E_XML        TYPE STRING,
      LC_JSON      TYPE STRING.

DATA: V_URL            TYPE STRING,
      VURL             TYPE STRING,
      URL_RECEITAS     TYPE STRING,
      URL_RTC          TYPE STRING,
      V_USERNAME       TYPE STRING,
      V_PASSWORD       TYPE STRING,
      V_CLIENT_ID      TYPE STRING,
      V_CLIENTE_SECRET TYPE STRING.


DATA: DT_AUX     TYPE SY-DATUM,
      VDT_INICIO TYPE CHAR10,
      VDT_FINAL  TYPE CHAR10,
      L_SERVICO  TYPE CHAR30,           "*-CS2021000218-07.12.2022-#97911-JT
      T_SET      TYPE TABLE OF RGSB4,   "*-CS2021000218-07.12.2022-#97911-JT
      W_SET      TYPE RGSB4.            "*-CS2021000218-07.12.2022-#97911-JT

PARAMETERS: P_EVENTO TYPE CHAR1,
            P_DT_INI TYPE SY-DATUM NO-DISPLAY,
            P_DT_FIM TYPE SY-DATUM NO-DISPLAY.


CREATE OBJECT OB_WEB_SERVICE.


START-OF-SELECTION.

*-CS2021000218-07.12.2022-#97911-JT-inicio
  FREE: L_SERVICO.

*-----------------------------------------
*-- define qualservico sera utilizado
*-----------------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS           = '0000'
      SETNR           = 'ZSDR023_SERVICO'
      NO_DESCRIPTIONS = ABAP_OFF
    TABLES
      SET_VALUES      = T_SET
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  LOOP AT T_SET INTO W_SET.
    IF W_SET-TITLE = ABAP_TRUE.
      L_SERVICO = W_SET-FROM.
      EXIT.
    ENDIF.
  ENDLOOP.
*-CS2021000218-07.12.2022-#97911-JT-fim

  IF P_DT_INI IS NOT INITIAL AND P_DT_FIM IS NOT INITIAL.

    CONCATENATE P_DT_INI+0(4)  '-'   P_DT_INI+4(2)  '-' P_DT_INI+6(2)  INTO VDT_INICIO.
    CONCATENATE P_DT_FIM+0(4)  '-'   P_DT_FIM+4(2)  '-' P_DT_FIM+6(2)  INTO VDT_FINAL.

  ELSE.
    DT_AUX = SY-DATUM - 15.
    CONCATENATE DT_AUX+0(4)   '-'  DT_AUX+4(2)   '-' DT_AUX+6(2)   INTO VDT_INICIO.
    CONCATENATE SY-DATUM+0(4) '-'  SY-DATUM+4(2) '-' SY-DATUM+6(2) INTO VDT_FINAL.
  ENDIF.


  SELECT  SINGLE SERVICE  URL  FROM ZAUTH_WEBSERVICE
   INTO  WA_ZAUTH_WEBSERVICE
   WHERE SERVICE EQ 'SIAGRI_TOKEN'.


  SELECT  SINGLE URL FROM ZAUTH_WEBSERVICE INTO V_URL
    WHERE SERVICE EQ 'SIAGRI_PYTHON'.

  CASE P_EVENTO.
    WHEN '1'.
      SELECT SINGLE  URL  FROM ZAUTH_WEBSERVICE
       INTO URL_RECEITAS
       WHERE SERVICE = 'SIAGRI_RECEITAS'.

    WHEN '2'.
      SELECT SINGLE  URL  FROM ZAUTH_WEBSERVICE
       INTO URL_RTC
       WHERE SERVICE = 'SIAGRI_RTC'.
    WHEN OTHERS.
  ENDCASE.

  SELECT SINGLE *
    FROM  ZAUTH_WS_0001 INTO  WA_ZAUTH_WS_0001
   WHERE SERVICE EQ WA_ZAUTH_WEBSERVICE-SERVICE.

*-CS2021000218-07.12.2022-#97911-JT-inicio
  IF P_EVENTO = '1'.
    IF L_SERVICO = 'SIAGRI_RECEITAS'.
*     PERFORM: z_gerar_token.  "*-CS2021000218-25.05.2023-BUG #113456-JT
    ENDIF.
  ELSE.
    PERFORM: Z_GERAR_TOKEN.
  ENDIF.
*-CS2021000218-07.12.2022-#97911-JT-fim

  IF     P_EVENTO = '1'.

*-CS2021000218-07.12.2022-#97911-JT-inicio
    IF L_SERVICO = 'SIAGRI_RECEITAS'.

*-CS2021000218-25.05.2023-BUG #113456-JT-inicio
*     PERFORM z_consulta_siagri_receita USING it_token-access_token
*                                             vdt_inicio
*                                             vdt_final  CHANGING it_receitas.
      TRY .
          ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
             )->SET_CONSULTAR_RECEITA_SIAGRI( EXPORTING I_DATA_INICIO = P_DT_INI
                                                        I_DATA_FIM    = P_DT_FIM
                                              IMPORTING E_RECEITAS    = WA_RECEITAS ).

        CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
          MESSAGE S024(SD) WITH 'Não foram encontradas Receitas para atualização.' DISPLAY LIKE 'W'.

        CATCH ZCX_ERROR      INTO DATA(EX_ERROR).    "  "
          MESSAGE S024(SD) WITH 'Não foram encontradas Receitas para atualização.' DISPLAY LIKE 'W'.
      ENDTRY.
*-CS2021000218-25.05.2023-BUG #113456-JT-fim

      PERFORM Z_SALVA_DADOS.
    ELSE.
      TRY .
          ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
             )->SET_BAIXAR_RECEITAS_CONTA( EXPORTING I_DATA_INICIO  = P_DT_INI
                                                     I_DATA_FIM     = P_DT_FIM ).

        CATCH ZCX_INTEGRACAO INTO EX_INTEGRA.
          MESSAGE S024(SD) WITH 'Não foram encontradas Receitas para atualização.' DISPLAY LIKE 'W'.

        CATCH ZCX_ERROR      INTO EX_ERROR.    "  "
          MESSAGE S024(SD) WITH 'Não foram encontradas Receitas para atualização.' DISPLAY LIKE 'W'.
      ENDTRY.
    ENDIF.
*-CS2021000218-07.12.2022-#97911-JT-fim

  ELSEIF P_EVENTO = '2'.
    PERFORM Z_CONSULTA_SIAGRI_RTC     USING IT_TOKEN-ACCESS_TOKEN
                                   CHANGING IT_RTC.
    PERFORM Z_SALVA_DADOS_RTC.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_GERAR_TOKEN .

  "homologação :'http://172.12.12.158:6243/servico/sendHttp'.

  VURL       = WA_ZAUTH_WEBSERVICE-URL.
  V_USERNAME = WA_ZAUTH_WS_0001-USERNAME.
  V_PASSWORD = WA_ZAUTH_WS_0001-ADD01.


  CL_HTTP_CLIENT=>CREATE_BY_URL(
    EXPORTING
      URL                = |{ V_URL }|
    IMPORTING
      CLIENT             = E_HTTP
    EXCEPTIONS
      ARGUMENT_NOT_FOUND = 1
      PLUGIN_NOT_ACTIVE  = 2
      INTERNAL_ERROR     = 3 ).

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = '~request_method'
      VALUE = 'POST'.

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = '~server_protocol'
      VALUE = 'HTTP/1.1'.

  E_HTTP->REQUEST->SET_CONTENT_TYPE( CONTENT_TYPE = 'application/x-www-form-urlencoded' ).
  E_HTTP->REQUEST->SET_HEADER_FIELD( EXPORTING NAME = 'url_destino' VALUE = VURL ).
  E_HTTP->REQUEST->SET_FORM_FIELD( EXPORTING NAME = 'grant_type' VALUE = 'client_credentials' ).
  E_HTTP->REQUEST->SET_FORM_FIELD( EXPORTING NAME = 'modile' VALUE = 'apiV2' ).

  E_HTTP->AUTHENTICATE( USERNAME = V_USERNAME PASSWORD = V_PASSWORD ).

  CLEAR JSON_RETORNO.
  OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
    EXPORTING
      I_HTTP                     = E_HTTP
    IMPORTING
      E_REASON                   = E_REASON
    RECEIVING
      E_RESULTADO                = JSON_RETORNO
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      OTHERS                     = 5 ).

  IF SY-SUBRC <> 0.
    MESSAGE I000(FB) WITH TEXT-100.
  ENDIF.

  CHECK JSON_RETORNO IS NOT INITIAL.

  /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = IT_TOKEN ).

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_CONSULTA_SIAGRI_RTC     USING  P_TOKEN    TYPE STRING
                               CHANGING TABELA TYPE ANY TABLE.

*-CS2021000189-19.10.2023-#125787 -JT-inicio
*  DATA: v_token   TYPE string.
*
*  "VALUE 'https://homologintegracaoreceituario.azurewebsites.net/api/v2/Receita/Receitas'.
*
*  CLEAR:  v_token.
*
*  v_token       =  |{ 'Bearer' } { p_token }|.
*
*  cl_http_client=>create_by_url(
*  EXPORTING url   =  |{ v_url }|
*  IMPORTING client = e_http
*  EXCEPTIONS argument_not_found = 1
*           plugin_not_active  = 2
*           internal_error     = 3 ).
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = '~request_method'
*      value = 'GET'.
*
*
*  e_http->request->set_header_field( EXPORTING name  = 'url_destino'   value = url_rtc   ).
*  e_http->request->set_header_field( EXPORTING name  = 'Authorization' value = v_token ).
*
*
*  CLEAR json_retorno.
*  ob_web_service->zif_webservice~consultar(
*      EXPORTING
*        i_http                     = e_http
*      IMPORTING
*        e_reason                   = e_reason
*      RECEIVING
*        e_resultado                = json_retorno
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        http_invalid_timeout       = 4
*        OTHERS                     = 5  ).
*
*  IF sy-subrc <> 0.
*    MESSAGE i000(fb) WITH text-100.
*    EXIT.
*  ENDIF.
*
*  CHECK json_retorno IS NOT INITIAL.
*
*  /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = it_rtc[] ).

  FREE: IT_RTC.

  TRY .
      ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
         )->SET_CONSULTAR_RTC_SIAGRI( IMPORTING E_RTC = W_DADOS_RTC ).

    CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
      MESSAGE S024(SD) WITH 'Não foram encontrados RTC para atualização.' DISPLAY LIKE 'W'.
      EXIT.

    CATCH ZCX_ERROR      INTO DATA(EX_ERROR).    "  "
      MESSAGE S024(SD) WITH 'Não foram encontrados RTC para atualização.' DISPLAY LIKE 'W'.
      EXIT.
  ENDTRY.

  LOOP AT W_DADOS_RTC-DATA  INTO WA_RTC.
    APPEND WA_RTC             TO IT_RTC.
  ENDLOOP.

  TABELA[] = IT_RTC[].
*-CS2021000189-19.10.2023-#125787 -JT-fim

  MESSAGE S000(FB) WITH TEXT-101.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_CONSULTA_SIAGRI_RECEITA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_CONSULTA_SIAGRI_RECEITA USING  P_TOKEN    TYPE STRING
                                      P_DT_INI   TYPE CHAR10
                                      P_DT_FIM   TYPE CHAR10
                               CHANGING TABELA TYPE ANY TABLE.

  DATA: V_TOKEN   TYPE STRING,
        DT_INICIO TYPE CHAR10,
        DT_FINAL  TYPE CHAR10.

  "VALUE 'https://homologintegracaoreceituario.azurewebsites.net/api/v2/Receita/Receitas'.

  CLEAR:  V_TOKEN, DT_INICIO, DT_FINAL, DT_AUX.

  DT_INICIO = P_DT_INI.
  DT_FINAL  = P_DT_FIM.

  V_TOKEN       =  |{ 'Bearer' } { P_TOKEN }|.
  URL_RECEITAS  = URL_RECEITAS && '?dataInicial=' && DT_INICIO && '&dataFinal=' && DT_FINAL .


  CL_HTTP_CLIENT=>CREATE_BY_URL(
    EXPORTING
      URL                = |{ V_URL }|
    IMPORTING
      CLIENT             = E_HTTP
    EXCEPTIONS
      ARGUMENT_NOT_FOUND = 1
      PLUGIN_NOT_ACTIVE  = 2
      INTERNAL_ERROR     = 3 ).

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = '~request_method'
      VALUE = 'GET'.


  E_HTTP->REQUEST->SET_HEADER_FIELD( EXPORTING NAME = 'url_destino' VALUE = URL_RECEITAS ).
  E_HTTP->REQUEST->SET_HEADER_FIELD( EXPORTING NAME = 'Authorization' VALUE = V_TOKEN ).


  CLEAR JSON_RETORNO.
  OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
    EXPORTING
      I_HTTP                     = E_HTTP
    IMPORTING
      E_REASON                   = E_REASON
    RECEIVING
      E_RESULTADO                = JSON_RETORNO
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      OTHERS                     = 5 ).

  IF SY-SUBRC <> 0.
    MESSAGE I000(FB) WITH TEXT-100.
  ENDIF.

  CHECK JSON_RETORNO IS NOT INITIAL.

  /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = WA_RECEITAS ).
  APPEND WA_RECEITAS TO IT_RECEITAS.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_SALVA_DADOS_RTC.

  LOOP AT IT_RTC INTO WA_RTC.
    CLEAR WA_ZSDT0259.

    WA_ZSDT0259-MANDT    = SY-MANDT.
    WA_ZSDT0259-CPF      = WA_RTC-CPF.
    CONCATENATE WA_RTC-NOME
                WA_RTC-SOBRENOME
           INTO WA_ZSDT0259-NOME
           SEPARATED BY SPACE.
    WA_ZSDT0259-EMAIL    = WA_RTC-EMAIL.
    IF WA_RTC-ATIVO = ABAP_TRUE.
      WA_ZSDT0259-STATUS = 'S'.
    ELSE.
      WA_ZSDT0259-STATUS = 'N'.
    ENDIF.

*-CS2021000218-03.10.2022-#91289-JT-inicio
    SELECT SINGLE ASS_ELETRONICA
      INTO @DATA(L_ASS_ELETR)
      FROM ZSDT0259
     WHERE CPF = @WA_RTC-CPF.
    IF SY-SUBRC = 0.
      WA_ZSDT0259-ASS_ELETRONICA = L_ASS_ELETR.
    ENDIF.

    WA_ZSDT0259-USNAM            = SY-UNAME.
    WA_ZSDT0259-DATA             = SY-DATUM.
    WA_ZSDT0259-HORA             = SY-UZEIT.
*-CS2021000218-03.10.2022-#91289-JT-fim

    MODIFY ZSDT0259   FROM WA_ZSDT0259.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_SALVA_DADOS .


  LOOP AT WA_RECEITAS-DATA INTO DATA(WA_DADOS).

    WA_ZSDT0218-MANDT                   = SY-MANDT.
    WA_ZSDT0218-NUMERORECEITA           = WA_DADOS-NUMERORECEITA.
    WA_ZSDT0218-NUMEROPEDIDO            = |{ WA_DADOS-NUMEROPEDIDO ALPHA = IN }|.
    WA_ZSDT0218-CPFRT                   = WA_DADOS-CPFRT.
    WA_ZSDT0218-NUMERONF                = WA_DADOS-NUMERONF.
    CONCATENATE WA_DADOS-DATAEMISSAO+0(4) WA_DADOS-DATAEMISSAO+5(2) WA_DADOS-DATAEMISSAO+8(2) INTO WA_ZSDT0218-DATAEMISSAO.
    WA_ZSDT0218-NUMEROART               = WA_DADOS-NUMEROART.
    WA_ZSDT0218-CREA                    = WA_DADOS-CREA.
    WA_ZSDT0218-NOMECLIENTE             = WA_DADOS-NOMECLIENTE.
    WA_ZSDT0218-RAZAOSOCIAL             = WA_DADOS-RAZAOSOCIAL.
    WA_ZSDT0218-CPFCNPJCLIENTE          = WA_DADOS-CPFCNPJCLIENTE.
    WA_ZSDT0218-TIPOPESSOA              = WA_DADOS-TIPOPESSOA.
    WA_ZSDT0218-CODIGOINDEAPROPRIEDADE  = WA_DADOS-CODIGOINDEAPROPRIEDADE.
    WA_ZSDT0218-CANCELADA               = WA_DADOS-CANCELADA.
    WA_ZSDT0218-USNAM                   = SY-UNAME.
    WA_ZSDT0218-DATA_ATUAL              = SY-DATUM.
    WA_ZSDT0218-HORA_ATUAL              = SY-UZEIT.


    LOOP AT WA_DADOS-RECEITAPRODUTOS INTO DATA(WA_REC_PRODUTOS).

      WA_ZSDT0219-MANDT                      =  SY-MANDT.
      WA_ZSDT0219-NUMERORECEITA              =  WA_DADOS-NUMERORECEITA.
      WA_ZSDT0219-NUMEROPEDIDO               =  |{ WA_DADOS-NUMEROPEDIDO ALPHA = IN }|.
      WA_ZSDT0219-CPFRT                      =  WA_DADOS-CPFRT.
      WA_ZSDT0219-CODIGOMAPA                 =  |{ WA_REC_PRODUTOS-CODIGOMAPA ALPHA = OUT }|.
      WA_ZSDT0219-CODIGOINDEACULTURA         =  WA_REC_PRODUTOS-CODIGOINDEACULTURA.
      WA_ZSDT0219-CODIGOINDEAPRAGA           =  WA_REC_PRODUTOS-CODIGOINDEAPRAGA.
      WA_ZSDT0219-CODIGOINDEATIPOAPLICACAO   =  WA_REC_PRODUTOS-CODIGOINDEATIPOAPLICACAO.
      WA_ZSDT0219-CODIGOINDEAUNIDADEMEDIDA   =  WA_REC_PRODUTOS-CODIGOINDEAUNIDADEMEDIDA.
      WA_ZSDT0219-AREA                       =  WA_REC_PRODUTOS-AREA.
      WA_ZSDT0219-DOSE                       =  WA_REC_PRODUTOS-DOSE.
      WA_ZSDT0219-QUANTIDADE                 =  WA_REC_PRODUTOS-QUANTIDADE.
      WA_ZSDT0219-QUANTIDADEAPLICACOES       =  WA_REC_PRODUTOS-QUANTIDADEAPLICACOES.
      WA_ZSDT0219-RECEITAPRODUTOID           =  WA_REC_PRODUTOS-RECEITAPRODUTOID.
      WA_ZSDT0219-PRODUTOID                  =  WA_REC_PRODUTOS-PRODUTOID.

      TRY .
          WA_ZSDT0219-QTDSEMENTES =  WA_REC_PRODUTOS-QTDSEMENTES.
        CATCH CX_SY_CONVERSION_OVERFLOW.
          WA_ZSDT0219-QTDSEMENTES = 0.
      ENDTRY.


      WA_ZSDT0219-USNAM                      =  SY-UNAME.
      WA_ZSDT0219-DATA_ATUAL                 =  SY-DATUM.
      WA_ZSDT0219-HORA_ATUAL                 =  SY-UZEIT.

      APPEND WA_ZSDT0219 TO IT_ZSDT0219.
      CLEAR:  WA_REC_PRODUTOS, WA_ZSDT0219.
    ENDLOOP.

    APPEND WA_ZSDT0218 TO IT_ZSDT0218.
    CLEAR: WA_DADOS, WA_ZSDT0218.
  ENDLOOP.

  MODIFY ZSDT0218 FROM TABLE IT_ZSDT0218.
  COMMIT WORK.

  MODIFY ZSDT0219 FROM TABLE IT_ZSDT0219.
  COMMIT WORK.

  REFRESH: IT_ZSDT0218, IT_ZSDT0219, IT_RECEITAS.
  CLEAR: WA_ZSDT0218, WA_ZSDT0219, WA_RECEITAS.


ENDFORM.

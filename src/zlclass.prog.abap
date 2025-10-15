

DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
CREATE OBJECT OB_WEB_SERVICE.

CLASS ZCL_ZPM_GERAR_TOKEN DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF TY_ZTPM_L_AMOST,
        ID      TYPE ZTPM_L_AMOST-ID,
        CLIENTE TYPE ZTPM_L_AMOST-CLIENTE,
      END OF TY_ZTPM_L_AMOST.

    TYPES:
      BEGIN OF TY_ZTPM_R_AMOST,
        ID_COMP       TYPE ZTPM_R_AMOST-ID_COMP,
        COMPARTIMENTO TYPE ZTPM_R_AMOST-COMPARTIMENTO,
      END OF TY_ZTPM_R_AMOST.


    TYPES: BEGIN OF TY_ZTPM_L_AMOS.
             INCLUDE TYPE ZTPM_L_AMOST.
             TYPES: CHECK TYPE CHAR1,
           END OF TY_ZTPM_L_AMOS.


    DATA: LS_ZTPM_TOKEN_S360 TYPE ZTPM_TOKEN_S360,
          IT_ZTPM_R_AMOST    TYPE TABLE OF TY_ZTPM_R_AMOST,
          IT_ZTPM_L_AMOST    TYPE TABLE OF TY_ZTPM_L_AMOST.

    DATA:
      E_NUMEROPAGINA   TYPE CHAR5,
      E_TOTALPAGINAS   TYPE CHAR5,
      E_TOTALREGISTROS TYPE CHAR5,
      E_NOMEEMPRESA    TYPE CHAR30.


    TYPES BEGIN OF TY_RETORNO_TOKEN.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: USERNAME TYPE STRING.
    TYPES END OF TY_RETORNO_TOKEN.

    TYPES: BEGIN OF TY_RETORNO_RESTADO_AMOSTRA,
             FROTA             TYPE STRING,
             TIPOCOMPARTIMENTO TYPE STRING,
             RESPONSAVEL       TYPE STRING,
             DATARECEBIMENTO   TYPE STRING,
             NUMEROCAIXA       TYPE STRING,
             DATAFINALIZACAO   TYPE STRING,
             CLIENTE           TYPE STRING,
             NOME              TYPE STRING,
             ID                TYPE STRING,
             NOTAFISCALFRASCO  TYPE STRING,
             TIPOCOLETA        TYPE STRING,
             STATUSAMOSTRA     TYPE STRING,
             AVALIACAO         TYPE STRING,
           END OF TY_RETORNO_RESTADO_AMOSTRA.

    DATA: LC_JSON            TYPE STRING,
          WC_JSON            TYPE STRING,
*          LC_JSON_AUX        TYPE STRING,
          LC_DATA            TYPE C LENGTH 10,
          LC_HORA            TYPE C LENGTH 08,
          LC_DATA_HORA       TYPE C LENGTH 19,
          LC_NUMERO_ROM      TYPE ZSDT0001-NR_ROMANEIO,
          LC_INTEIRO         TYPE I,
          LC_PESO            TYPE C LENGTH 09,
          I_NAME_FILE        TYPE STRING,
          LC_RETORNO         TYPE ZDE_OPUS_CARGA,
          LC_DOCNUM	         TYPE J_1BDOCNUM,
          LC_NR_ROMANEIO_SAI TYPE ZDE_NR_ROMANEIO_SAI,
          E_REASON           TYPE STRING,
          JSON_RETORNO       TYPE STRING,
          JSON_RET_PDF       TYPE STRING,
          LS_RES_AMOSTRA     TYPE ZTPM_R_AMOSTRA,
          LS_RES_LST_AMOSTRA TYPE  ZTPM_LIS_AMOST,
          LS_RES_LST_PAG     TYPE ZTPM_LIS_AMOST_PAG,
          LS_LIS_AMOST       TYPE  ZTPM_LIS_RESULT_T,
          LS_ZTPM_L_AMOST    TYPE  ZTPM_L_AMOST,
          TS_ZTPM_L_AMOST    TYPE TABLE OF TY_ZTPM_L_AMOS,
          TS_ZTPM_R_AMOST    TYPE TABLE OF ZTPM_R_AMOST,
          LS_ZTPM_R_AMOST    TYPE ZTPM_R_AMOST,
          LS_RESULT          TYPE  ZTPM_LIS_RESULT_T,
          GT_RETURN          TYPE TABLE OF BAPIRET2,
          LS_GT_RETURN       TYPE BAPIRET2,
          LC_TOKEN           TYPE TY_RETORNO_TOKEN.


    DATA: LS_LAUDO TYPE ZTPM_R_AMOST-LAUDO.



*====================================================================
*Method gerar a chave de segurança para acessar as informações s360.
*====================================================================
    METHODS: ZGERAR_TOKEN
      IMPORTING
        TOKEN TYPE STRING.

*=================================================================
*Method consultar os resultados das amostras
*=================================================================
    METHODS: ZGERAR_RES_AMOSTRA
      EXPORTING
        TOKEN TYPE STRING.
*      RETURNING
*        VALUE(LS_ZTPM_R_AMOST) TYPE ZTPM_R_AMOST.

*=================================================================
*Method consultar os resultados das amostras
*=================================================================
    METHODS: ZGERAR_LIST_AMOSTRA
      EXPORTING
        TOKEN TYPE STRING.
*      RETURNING
*        VALUE(LS_RES_LST_AMOSTRA) TYPE ZTPM_LIS_AMOST.

*=================================================================
*Method gerar o laudo técnico em PDF
*=================================================================

    METHODS: ZGERAR_PDF_RESULT_AMOSTRA
      IMPORTING
        NUMEROAMOSTRA TYPE CHAR15
        TOKEN         TYPE STRING
      EXPORTING
        E_LAUDO       TYPE ZDE_SMARTFORMS_XSTRING.



*=================================================================
*Method verificar quantidade de paginas
*=================================================================

    METHODS: ZVERIF_QUANT_PAG
      IMPORTING
        I_EMP     TYPE CHAR30
      EXPORTING
        E_NPG     TYPE CHAR5
        E_TPG     TYPE CHAR5
        E_TRG     TYPE CHAR5
        E_EMPRESA TYPE CHAR30.




*=================================================================
*Method gerar listas de analises de óleo
*=================================================================

    METHODS: ZGERAR_LIST_AMOSTRAS
      IMPORTING
        I_NPG     TYPE CHAR5
        I_TPG     TYPE CHAR5
        I_TRG     TYPE CHAR5
        I_EMPRESA TYPE CHAR30.


ENDCLASS.


CLASS ZCL_ZPM_GERAR_TOKEN IMPLEMENTATION.

*====================================================================
*Method gerar a chave de segurança para acessar as informações s360.
*====================================================================
  METHOD: ZGERAR_TOKEN.
    DATA: LT_LS_ZTPM_TOKEN_S360 TYPE ZTPM_TOKEN_S360.
    DATA(USUARIO) = 'intergraco.als@amaggi.com.br'.
    DATA(SENHA)   = 'amaggi'.

    DATA: TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
    DATA: TL_TOKEN      TYPE STRING.

*    Verificar se existe chave valida para usuario.
    SELECT SINGLE *
    FROM ZTPM_TOKEN_S360
      INTO LT_LS_ZTPM_TOKEN_S360
      WHERE USERNAME EQ USUARIO.

    IF LT_LS_ZTPM_TOKEN_S360-TOKEN IS INITIAL.

*      Costruindo login de acesso, para gerar a chave de segurança.
      CLEAR: LC_JSON.
      LC_JSON = '{"username":' && '"' && USUARIO &&'",' && '"password":' && '"' && SENHA && '"}'.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
      TRY .
          OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'LS' ).
        CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
      ENDTRY.
      OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

      TRY .
          DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
          DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
        CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
      ENDTRY.

      OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).
      OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
          EXPORTING
            I_HTTP                     = VAR_HTTP
            I_XML                      = LC_JSON
          IMPORTING
*          E_CODE                     = E_CODE
            E_REASON                   = E_REASON
          RECEIVING
            E_RESULTADO                = JSON_RETORNO
          EXCEPTIONS
            HTTP_COMMUNICATION_FAILURE = 1
            HTTP_INVALID_STATE         = 2
            HTTP_PROCESSING_FAILED     = 3
            HTTP_INVALID_TIMEOUT       = 4
            OTHERS                     = 5
        ).

      CHECK JSON_RETORNO IS NOT INITIAL.
      /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = LC_TOKEN ).

      IF SY-SUBRC IS INITIAL.
        LS_ZTPM_TOKEN_S360 = VALUE #(
                            MANDT    = SY-MANDT
                            USERNAME = USUARIO
                            PASSWORD = SENHA
                            TOKEN    = LC_TOKEN-ACCESS_TOKEN
                            LAEDA    = SY-DATUM ).

        MODIFY ZTPM_TOKEN_S360 FROM LS_ZTPM_TOKEN_S360.
        COMMIT WORK.
      ENDIF.
    ELSE.

      IF LT_LS_ZTPM_TOKEN_S360-LAEDA NE SY-DATUM.

*    Costruindo login de acesso, para gerar a chave de segurança.
        CLEAR: LC_JSON.
        LC_JSON = '{"username":' && '"' && USUARIO &&'",' && '"password":' && '"' && SENHA && '"}'.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
        TRY .
            OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'LS' ).
          CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        ENDTRY.
        OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

        TRY .
            VAR_HTTP = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
            LC_URI = OB_WEB_SERVICE->GET_URI(  ).
          CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        ENDTRY.

        OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).
        OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
            EXPORTING
              I_HTTP                     = VAR_HTTP
              I_XML                      = LC_JSON
            IMPORTING
*          E_CODE                     = E_CODE
              E_REASON                   = E_REASON
            RECEIVING
              E_RESULTADO                = JSON_RETORNO
            EXCEPTIONS
              HTTP_COMMUNICATION_FAILURE = 1
              HTTP_INVALID_STATE         = 2
              HTTP_PROCESSING_FAILED     = 3
              HTTP_INVALID_TIMEOUT       = 4
              OTHERS                     = 5
          ).

*      TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
*      TL_TOKEN      TYPE STRING.

        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = LC_TOKEN ).

        IF SY-SUBRC IS INITIAL.
          DELETE FROM ZTPM_TOKEN_S360 WHERE USERNAME EQ USUARIO.
          COMMIT WORK.
          LS_ZTPM_TOKEN_S360 = VALUE #(
                              MANDT    = SY-MANDT
                              USERNAME = USUARIO
                              PASSWORD = SENHA
                              TOKEN    = LC_TOKEN-ACCESS_TOKEN
                              LAEDA    = SY-DATUM ).

          MODIFY ZTPM_TOKEN_S360 FROM LS_ZTPM_TOKEN_S360.
          COMMIT WORK.
        ENDIF.
      ELSE.

        CLEAR LC_TOKEN-ACCESS_TOKEN.
        LC_TOKEN-ACCESS_TOKEN = LT_LS_ZTPM_TOKEN_S360-TOKEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.

*=================================================================
*Method consultar os resultados das amostras
*=================================================================
  METHOD: ZGERAR_RES_AMOSTRA.

    FREE TS_ZTPM_L_AMOST.
    SELECT NUMEROAMOSTRA
    FROM ZTPM_L_AMOST
    INTO CORRESPONDING FIELDS OF TABLE TS_ZTPM_L_AMOST
      WHERE SITUACAO EQ 'FINALIZADA'.

    CHECK TS_ZTPM_L_AMOST IS NOT INITIAL.
    SORT TS_ZTPM_L_AMOST ASCENDING BY NUMEROAMOSTRA.

    SELECT NUMEROAMOSTRA
    FROM ZTPM_R_AMOST
     INTO TABLE @DATA(_ZTPM_R_AMOST)
      FOR ALL ENTRIES IN @TS_ZTPM_L_AMOST
      WHERE NUMEROAMOSTRA EQ @TS_ZTPM_L_AMOST-NUMEROAMOSTRA.
    SORT _ZTPM_R_AMOST ASCENDING BY NUMEROAMOSTRA.

    LOOP AT _ZTPM_R_AMOST ASSIGNING FIELD-SYMBOL(<TW_ZTPM_L_AMOST>).
      LOOP AT TS_ZTPM_L_AMOST ASSIGNING FIELD-SYMBOL(<W_AMOST>) WHERE NUMEROAMOSTRA EQ <TW_ZTPM_L_AMOST>-NUMEROAMOSTRA.
        <W_AMOST>-CHECK = ABAP_TRUE.
      ENDLOOP.
    ENDLOOP.

    DELETE TS_ZTPM_L_AMOST WHERE CHECK EQ ABAP_TRUE.

    CHECK TS_ZTPM_L_AMOST IS NOT INITIAL.

    LOOP AT TS_ZTPM_L_AMOST INTO DATA(LW_ZTPM_L_AMOST).

*>>> Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Inicio
*      DATA(URL_GET) = 'https://s360web.com' && '/api/v3/resultadoAmostra/view' && '?numeroAmostra=' && LW_ZTPM_L_AMOST-NUMEROAMOSTRA && '&token_type=Bearer&access_token=' && LC_TOKEN-ACCESS_TOKEN.
      DATA(URL_GET) = 'https://s360web.com' && '/api/v3/resultadoAmostra/view' && '?numeroAmostra=' && LW_ZTPM_L_AMOST-NUMEROAMOSTRA. " '&token_type=Bearer&access_token=' && LC_TOKEN-ACCESS_TOKEN.
*<<< Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Fim

      DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( EXPORTING I_URL = CONV #( URL_GET ) ).

      OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).

*>>> Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Inicio
*       Construindo bearer
    CONCATENATE 'Bearer' LC_TOKEN-ACCESS_TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
    CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Authorization'
        VALUE = BEARER_TOKEN.

*<<< Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Fim
      "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
      VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD( EXPORTING NAME  = '~request_method' VALUE = 'GET' ).

      CLEAR JSON_RETORNO.
      OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
          EXPORTING
            I_HTTP                     = VAR_HTTP_GET
          IMPORTING
*          E_CODE                     = E_CODE
              E_REASON                   = E_REASON
          RECEIVING
            E_RESULTADO                = JSON_RETORNO
          EXCEPTIONS
            HTTP_COMMUNICATION_FAILURE = 1
            HTTP_INVALID_STATE         = 2
            HTTP_PROCESSING_FAILED     = 3
            HTTP_INVALID_TIMEOUT       = 4
            OTHERS                     = 5
        ).

      IF SY-SUBRC = 0.
        CLEAR LS_RES_AMOSTRA.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = LS_RES_AMOSTRA ).
        IF LS_RES_AMOSTRA IS NOT INITIAL.

*          Gerar laudo técnico e converter em string para gravar no banco.
          CLEAR LS_LAUDO.
          ZGERAR_PDF_RESULT_AMOSTRA( EXPORTING
                                     NUMEROAMOSTRA = LW_ZTPM_L_AMOST-NUMEROAMOSTRA
                                     TOKEN         = LC_TOKEN-ACCESS_TOKEN
                                     IMPORTING
                                      E_LAUDO        = LS_LAUDO ).


          LS_ZTPM_R_AMOST = VALUE #(  MANDT            = SY-MANDT
                                      NUMEROAMOSTRA    = LS_RES_AMOSTRA-NUMEROAMOSTRA
                                      FROTA            = LS_RES_AMOSTRA-DADOSCOLETAEQUIPAMENTO-EQUIPAMENTO-FROTA
                                      CHASSISERIE      = LS_RES_AMOSTRA-DADOSCOLETAEQUIPAMENTO-EQUIPAMENTO-CHASSISERIE
                                      ID_COMP          = LS_RES_AMOSTRA-DADOSCOLETAEQUIPAMENTO-COMPARTIMENTO-ID
                                      COMPARTIMENTO    = LS_RES_AMOSTRA-DADOSCOLETAEQUIPAMENTO-COMPARTIMENTO-NOME
                                      RESPONSAVEL      = LS_RES_AMOSTRA-RESPONSAVEL
                                      DATARECEBIMENTO  = LS_RES_AMOSTRA-DATARECEBIMENTO
                                      AVALIACAO        = LS_RES_AMOSTRA-AVALIACAO
                                      DATAFINALIZACAO  = LS_RES_AMOSTRA-DATAFINALIZACAO
                                      CLIENTE          = LS_RES_AMOSTRA-CLIENTE-NOME
                                      OBRA             = LS_RES_AMOSTRA-OBRA-NOME
                                      TIPOCOLETA       = LS_RES_AMOSTRA-TIPOCOLETA
                                      STATUSAMOSTRA    = LS_RES_AMOSTRA-STATUSAMOSTRA
                                      LAUDO            = LS_LAUDO
                                      HR_KM            = LS_RES_AMOSTRA-DADOSCOLETAEQUIPAMENTO-EQUIPAMENTO-HORASEQUIPAMENTOCOLETA
                                      HORASOLEO        = LS_RES_AMOSTRA-DADOSCOLETAGERAL-HORASOLEO
                                      FABRICANTEOLEO   = LS_RES_AMOSTRA-DADOSCOLETAGERAL-OLEO-FABRICANTEOLEO-NOME
                                      VISCOSIDADE      = LS_RES_AMOSTRA-DADOSCOLETAGERAL-OLEO-VISCOSIDADE-NOME
                                      OLEOTROCADO      = LS_RES_AMOSTRA-DADOSCOLETAGERAL-OLEOTROCADO
                                      VOLUMEADICIONADO = LS_RES_AMOSTRA-DADOSCOLETAGERAL-VOLUMEADICIONADO
                                      DATACOLETA       = LS_RES_AMOSTRA-DADOSCOLETAGERAL-DATACOLETA
                                      ERDAT           = SY-DATUM ).

          IF LS_ZTPM_R_AMOST IS NOT INITIAL.
            APPEND LS_ZTPM_R_AMOST TO TS_ZTPM_R_AMOST.
            MODIFY ZTPM_R_AMOST FROM LS_ZTPM_R_AMOST.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

*=================================================================
*Method listar todas as amostras
*=================================================================

  METHOD: ZGERAR_LIST_AMOSTRA.

*Verificar empresas cadastradas.
    SELECT *
    FROM ZTPARAM
    INTO TABLE @DATA(LT_ZTPARAM)
      WHERE PARAM EQ 'INT_SAP_LS'.

    CHECK LT_ZTPARAM IS NOT INITIAL.

    LOOP AT LT_ZTPARAM INTO DATA(LS_ZTPARAM).
      CALL METHOD ZVERIF_QUANT_PAG
        EXPORTING
          I_EMP     = CONV #( LS_ZTPARAM-ZVAL )
        IMPORTING
          E_NPG     = E_NUMEROPAGINA
          E_TPG     = E_TOTALPAGINAS
          E_TRG     = E_TOTALREGISTROS
          E_EMPRESA = E_NOMEEMPRESA.

      CALL METHOD ZGERAR_LIST_AMOSTRAS
        EXPORTING
          I_NPG     = E_NUMEROPAGINA
          I_TPG     = E_TOTALPAGINAS
          I_TRG     = E_TOTALREGISTROS
          I_EMPRESA = E_NOMEEMPRESA.
    ENDLOOP.
  ENDMETHOD.


*=====================================================================
* https://s360.com.br/api//v1/resultadoAmostra/viewPdf?numeroAmostra=
* Method gerar o resultado em formato PDF
*=====================================================================

  METHOD: ZGERAR_PDF_RESULT_AMOSTRA.

    DATA: LR_CONV      TYPE REF TO CL_ABAP_CONV_IN_CE,
          LR_XSTRING   TYPE XSTRING,
          LV_URL       TYPE CHAR255,
          LR_STRING    TYPE STRING,
          BIN_FILESIZE TYPE I,
          IT_OTF       TYPE TABLE OF SSFCRESCL.

    DATA: PDF_TABLE TYPE  RCL_BAG_TLINE,
          PDF_FSIZE TYPE  I.

*>>> Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Inicio
*    DATA(URL_GET) = 'https://s360web.com' && '/api//v1/resultadoAmostra/viewPdf' && '?numeroAmostra=' && NUMEROAMOSTRA && '&token_type=Bearer&access_token=' && TOKEN.
    DATA(URL_GET) = 'https://s360web.com' && '/api//v1/resultadoAmostra/viewPdf' && '?numeroAmostra=' && NUMEROAMOSTRA.
*<<< Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Fim

    DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( EXPORTING I_URL = CONV #( URL_GET ) ).

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).

*>>> Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Inicio
*       Construindo bearer
    CONCATENATE 'Bearer' TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
    CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Authorization'
        VALUE = BEARER_TOKEN.

*<<< Ajuste IR207584 - Gabriel Galvão - 11/11/2024 - Fim

    "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
    VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD( EXPORTING NAME  = '~request_method' VALUE = 'GET' ).

    CLEAR: JSON_RET_PDF, E_LAUDO.
    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
        EXPORTING
          I_HTTP                     = VAR_HTTP_GET
        IMPORTING
*          E_CODE                     = E_CODE
            E_REASON                 = E_REASON
            E_DATA                   = E_LAUDO
        RECEIVING
          E_RESULTADO                = JSON_RET_PDF
        EXCEPTIONS
          HTTP_COMMUNICATION_FAILURE = 1
          HTTP_INVALID_STATE         = 2
          HTTP_PROCESSING_FAILED     = 3
          HTTP_INVALID_TIMEOUT       = 4
          OTHERS                     = 5
      ).
  ENDMETHOD.



*=====================================================================
*
* Method verificar quantidade de registro e quantidade de pagina
*=====================================================================

  METHOD: ZVERIF_QUANT_PAG.

    DATA(TPG) = 1.

    DATA: TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
    DATA: TL_TOKEN      TYPE STRING.


    CLEAR: WC_JSON.
    WC_JSON = '{ "numeroPagina":' && TPG && ','
            && ' "nomeCliente":'  && '"' && '%' && I_EMP && '%' && '" }'.

*        Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'AN' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

    TRY .
        DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

*       Construindo bearer
    CONCATENATE 'Bearer' LC_TOKEN-ACCESS_TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
    CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Authorization'
        VALUE = BEARER_TOKEN.


    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
    CLEAR JSON_RETORNO.
    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
        EXPORTING
          I_HTTP                     = VAR_HTTP_GET
          I_XML                      = WC_JSON
        IMPORTING
*          E_CODE                     = E_CODE
          E_REASON                   = E_REASON
        RECEIVING
          E_RESULTADO                = JSON_RETORNO
        EXCEPTIONS
          HTTP_COMMUNICATION_FAILURE = 1
          HTTP_INVALID_STATE         = 2
          HTTP_PROCESSING_FAILED     = 3
          HTTP_INVALID_TIMEOUT       = 4
          OTHERS                     = 5
      ).

    IF SY-SUBRC IS INITIAL.
      CLEAR LS_RES_LST_PAG.
      /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = LS_RES_LST_PAG ).

      E_NUMEROPAGINA   = LS_RES_LST_PAG-NUMEROPAGINA.
      CONDENSE E_NUMEROPAGINA.
      E_TOTALPAGINAS   = LS_RES_LST_PAG-TOTALPAGINAS.
      CONDENSE E_TOTALPAGINAS.
      E_TOTALREGISTROS = LS_RES_LST_PAG-TOTALREGISTROS.
      CONDENSE E_TOTALREGISTROS.
      E_NOMEEMPRESA = I_EMP.
    ENDIF.
  ENDMETHOD.


*=====================================================================
*
* Method coletando as listas de amostras realizadas
*=====================================================================



  METHOD: ZGERAR_LIST_AMOSTRAS.
    DATA: CONTPG TYPE P.
*    Method para listar todas as amostras realizadas.

    DATA WC_JSON TYPE STRING.

    DATA(E_NPG) = I_NPG.
    DATA(E_TPG) = I_TPG.
    DATA(E_TRG) = I_TRG.
    DATA(E_EMPRESA) = I_EMPRESA.

    FREE TS_ZTPM_L_AMOST.

    CLEAR CONTPG.
    DO.
      ADD 1 TO CONTPG.

      IF CONTPG > E_TPG.
        EXIT.
      ELSE.

        DATA: TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
        DATA: TL_TOKEN      TYPE STRING.
*        DATA(URL_GET) = 'https://s360web.com' && '/api/v1/amostra/list' && '?token_type=Bearer&access_token=' && LC_TOKEN-ACCESS_TOKEN && '&numeroPagina=' && CONTPG && '&nomeCliente=' && E_EMPRESA.
*        DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( EXPORTING I_URL = CONV #( URL_GET ) ).

        CLEAR: WC_JSON.
        WC_JSON = '{ "numeroPagina":' && CONTPG && ','
               && ' "nomeCliente":'  && '"' && '%' && E_EMPRESA && '%' && '" }'.

*        Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
        TRY .
            OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'AN' ).
          CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
        ENDTRY.

        OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

        TRY .
            DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
            OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
          CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        ENDTRY.

*       Construindo bearer
        CONCATENATE 'Bearer' LC_TOKEN-ACCESS_TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
        CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
          EXPORTING
            NAME  = 'Authorization'
            VALUE = BEARER_TOKEN.

*   Passando boby de acesso.
*        LC_JSON = '{"nomeObra":' && '"' && EMPRESA &&'"}'.

        OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
        CLEAR JSON_RETORNO.
        OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
            EXPORTING
              I_HTTP                     = VAR_HTTP_GET
              I_XML                      = WC_JSON
            IMPORTING
*          E_CODE                     = E_CODE
              E_REASON                   = E_REASON
            RECEIVING
              E_RESULTADO                = JSON_RETORNO
            EXCEPTIONS
              HTTP_COMMUNICATION_FAILURE = 1
              HTTP_INVALID_STATE         = 2
              HTTP_PROCESSING_FAILED     = 3
              HTTP_INVALID_TIMEOUT       = 4
              OTHERS                     = 5
          ).

        IF SY-SUBRC IS INITIAL.

          CLEAR LS_RES_LST_AMOSTRA.
          /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = LS_RES_LST_AMOSTRA ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).


          IF LS_RES_LST_AMOSTRA IS NOT INITIAL.
            TS_ZTPM_L_AMOST = VALUE #( FOR WL_ZTPM_LIS_RESULT IN LS_RES_LST_AMOSTRA-RESULTADOS
                            ( MANDT             = SY-MANDT
                              NUMEROAMOSTRA     = WL_ZTPM_LIS_RESULT-NUMEROAMOSTRA
                              CODIGOEXTERNO     = WL_ZTPM_LIS_RESULT-CODIGOEXTERNO
                              SITUACAO          = WL_ZTPM_LIS_RESULT-SITUACAO
                              DATAFINALIZACAO   = |{ WL_ZTPM_LIS_RESULT-DATAFINALIZACAO(4) }{ WL_ZTPM_LIS_RESULT-DATAFINALIZACAO+5(2) }{  WL_ZTPM_LIS_RESULT-DATAFINALIZACAO+8(2) }|
                              NOME              = WL_ZTPM_LIS_RESULT-COMPARTIMENTO-NOME
                              MODELO            = WL_ZTPM_LIS_RESULT-EQUIPAMENTO-MODELO
                              CHASSISERIE       = WL_ZTPM_LIS_RESULT-EQUIPAMENTO-CHASSISERIE
                              CLIENTE           = WL_ZTPM_LIS_RESULT-CLIENTE-NOME
                              ID                = WL_ZTPM_LIS_RESULT-CLIENTE-ID
                              FROTA             = WL_ZTPM_LIS_RESULT-EQUIPAMENTO-FROTA
                              EMPRESA           = WL_ZTPM_LIS_RESULT-OBRA-NOME
                              ERDAT             = SY-DATUM
                              LAEDA             = SY-DATUM ) ).
          ENDIF.
        ENDIF.

        IF TS_ZTPM_L_AMOST IS NOT INITIAL.
          LOOP AT TS_ZTPM_L_AMOST INTO DATA(LW_ZTPM_L_AMOST).

            SELECT SINGLE *
              FROM ZTPM_L_AMOST
              INTO @DATA(_ZTPM_L_AMOST)
              WHERE NUMEROAMOSTRA EQ @LW_ZTPM_L_AMOST-NUMEROAMOSTRA
                AND SITUACAO      EQ @LW_ZTPM_L_AMOST-SITUACAO.

            IF SY-SUBRC IS NOT INITIAL.
              MODIFY ZTPM_L_AMOST FROM LW_ZTPM_L_AMOST.
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

class ZCL_WEBSERVIC_ALS definition
  public
  final
  create public .

public section.

  class-methods GERAR_TOKEN
    exporting
      !TOKEN type STRING .
  class-methods LISTAR_COMPARTIMENTO
    importing
      !ID type CHAR10
      value(TOKEN) type STRING
    returning
      value(GT_COMPART) type ZPME0021_T .
  class-methods LISTAR_FABRICANTE
    importing
      !TOKEN type STRING .
  class-methods LIST_POSI_TOTAL_CONTADOR
    importing
      !POINT type IMRC_POINT
      !EQUNR type EQUNR
      !LOCAS type IMRC_LOCAS
    exporting
      !UNITC type IMRC_UNITC
      !CNTRC type IMRC_CNTRC
      !CDIFC type IMRC_CDIFC .
  class-methods LIST_EQUIPAMENTO
    importing
      !EQUNR type EQUNR
    exporting
      !ID type CHAR10
      !TOKEN type STRING .
  class-methods REGITRAR_AMOSTRA
    importing
      !GT_AMOSTRA type ZPME0017_T
    returning
      value(GT_RETURNG) type ZPME0022_T .
  class-methods LISTAR_VISCOSIDADE
    importing
      value(TOKEN) type STRING
    returning
      value(GT_VISC) type ZPME0023_T .
  class-methods CONVERT_DATA
    importing
      !DATA_CONV type CSEQUENCE
    exporting
      !DATA type CSEQUENCE .
  class-methods CADASTRAR_EQUIPAMENTO
    importing
      !TOKEN type STRING
      !GT_EQUP type ZPME0026_T
    returning
      value(GT_RETURNG) type ZPME0022_T .
  class-methods SELECIONAR_DADOS_EQUP
    importing
      !EQUNR type EQUNR
    returning
      value(GT_EQUP) type ZPME0026_T .
  class-methods LISTAR_FAMILIA_EQUI
    exporting
      !TOKEN type STRING
    returning
      value(GT_CLAS_EQUIP) type ZPME0028_T .
  class-methods LISTAR_FABRICANTE_EQUIP
    importing
      value(TOKEN) type STRING
    returning
      value(GT_FABR) type ZPME0028_T .
  class-methods LISTAR_TIPO_COMPATIMENTO
    importing
      value(TOKEN) type STRING
    returning
      value(GT_TIPCOMP) type ZPME0029_T .
  class-methods CADASTRAR_COMPARTIMENTO
    importing
      !TOKEN type STRING
      !GT_COMP type ZPME0031_T
    returning
      value(GT_RETURNG) type ZPME0022_T .
  class-methods LISTAR_AMOSTRA
    importing
      !NUMEROAMOSTRA type CHAR15 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WEBSERVIC_ALS IMPLEMENTATION.


  METHOD CADASTRAR_COMPARTIMENTO.
    DATA WC_JSON TYPE STRING.

    TYPES BEGIN OF TY_RETORNO_TOKEN.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: USERNAME TYPE STRING.
    TYPES END OF TY_RETORNO_TOKEN.

    DATA: TL_TEXTO     TYPE CATSXT_LONGTEXT_ITAB,
          TL_TOKEN     TYPE STRING,
          LC_TOKEN     TYPE TY_RETORNO_TOKEN,
          E_REASON     TYPE STRING,
          JSON_RETORNO TYPE STRING.

    DATA: GW_RETURN TYPE ZPME0022.
    DATA: W_RETURN TYPE ZPME0022.
    DATA: W_ZPME0031 TYPE ZPME0031.
    DATA: T_RETURN TYPE TABLE OF ZPME0022.
    DATA: EQUNR TYPE EQUNR.
    DATA: DATA TYPE CHAR10.
*      DATA(HEADERS) = 'application/json'.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
    CREATE OBJECT OB_WEB_SERVICE.

    CHECK GT_COMP IS NOT INITIAL.

    LOOP AT GT_COMP ASSIGNING FIELD-SYMBOL(<_COMP>).

*      Tirando zero a esquerda.
      CLEAR: EQUNR.
*      EQUNR = |{ <_EQUIP>-CODIGOEXTERNO ALPHA = OUT }|.

*       Construindo JSON.
      CLEAR: WC_JSON.
      WC_JSON = '{ "idEquipamento":' && '"' && <_COMP>-IDEQUIPAMENTO &&'",'
       && ' "nome":' && '"' && ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( <_COMP>-NOME ) ) ) &&'",'
       && ' "codigoExterno":' && '"' && <_COMP>-CODIGOEXTERNO &&'",'
       && ' "volume":' && <_COMP>-VOLUME && ','
       && ' "idTipoCompartimento":' && <_COMP>-IDTIPOCOMPARTIMENTO && ' }'.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
      TRY .
          OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'LC' ).
        CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
      ENDTRY.

      OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

      TRY .
          DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
          OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
        CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
      ENDTRY.

*      Construindo bearer
      CONCATENATE 'Bearer' TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
      CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = 'Authorization'
          VALUE = BEARER_TOKEN.

      CLEAR JSON_RETORNO.
      OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
          EXPORTING
            I_HTTP                     = VAR_HTTP_GET
            I_XML                      = WC_JSON
          IMPORTING
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
*
        FREE: GW_RETURN.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = GW_RETURN ).

        IF GW_RETURN IS NOT INITIAL.
          APPEND VALUE #(
                         ID            = GW_RETURN-ID
                         MENSAGEM      = GW_RETURN-MENSAGEM
                         CODIGOERRO    = GW_RETURN-CODIGOERRO ) TO GT_RETURNG.

          IF GT_RETURNG IS NOT INITIAL.
            READ TABLE GT_RETURNG INTO DATA(_RETURN) INDEX 1.
            IF _RETURN-CODIGOERRO EQ 0.
              W_RETURN-ID = _RETURN-ID.
              W_RETURN-CODIGOERRO = _RETURN-CODIGOERRO.
              W_RETURN-MENSAGEM = 'COMPARTIMENTO CADASTRADO COM SUCESSO' .
              APPEND W_RETURN TO T_RETURN.
              CLEAR: W_RETURN.
            ELSE.
              W_RETURN-CODIGOERRO = _RETURN-CODIGOERRO.
              W_RETURN-MENSAGEM = 'COMPARTIMENTO NÃO CADASTRADO' .
              APPEND W_RETURN TO T_RETURN.
              CLEAR: W_RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    FREE: GT_RETURNG.
    GT_RETURNG = T_RETURN.

  ENDMETHOD.


  METHOD CADASTRAR_EQUIPAMENTO.

    DATA WC_JSON TYPE STRING.

    TYPES BEGIN OF TY_RETORNO_TOKEN.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: USERNAME TYPE STRING.
    TYPES END OF TY_RETORNO_TOKEN.

    DATA: TL_TEXTO     TYPE CATSXT_LONGTEXT_ITAB,
          TL_TOKEN     TYPE STRING,
          LC_TOKEN     TYPE TY_RETORNO_TOKEN,
          E_REASON     TYPE STRING,
          JSON_RETORNO TYPE STRING.

    DATA: GW_RETURN TYPE ZPME0022.
    DATA: W_RETURN TYPE ZPME0022.
    DATA: W_ZPMT0023 TYPE ZPMT0023.
    DATA: T_RETURN TYPE TABLE OF ZPME0022.
    DATA: EQUNR TYPE EQUNR.
    DATA: DATA TYPE CHAR10.
*      DATA(HEADERS) = 'application/json'.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
    CREATE OBJECT OB_WEB_SERVICE.

    CHECK GT_EQUP IS NOT INITIAL.

    LOOP AT GT_EQUP ASSIGNING FIELD-SYMBOL(<_EQUIP>).

*      Tirando zero a esquerda.
      CLEAR: EQUNR.
      EQUNR = |{ <_EQUIP>-EQUNR ALPHA = OUT }|.

*       Construindo JSON.
      CLEAR: WC_JSON.
      WC_JSON = '{ "chassiSerie":' && '"' && <_EQUIP>-CHASSIS_NUM &&'",'
       && ' "tagFrota":' && '"' && EQUNR &&'",'
       && ' "codigoExterno":' && '"' && EQUNR &&'",'
       && ' "idObra":' && <_EQUIP>-IDOBRA &&','
       && ' "area":' && '"' && ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( <_EQUIP>-AREA ) ) ) &&'",'
       && ' "setor":' && '"' && ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( <_EQUIP>-SETOR ) ) ) &&'",'
       && ' "metricaControle":' && '"' && <_EQUIP>-METRICACONTROLE &&'",'
       && ' "tamanhoAtual":' && <_EQUIP>-TAMANHOATUAL &&','
       && ' "idFabricanteEquipamento":' && <_EQUIP>-IDFABR &&','
       && ' "idFamiliaEquipamento":' && <_EQUIP>-IDFAM &&','
       && ' "modelo":' && '"' && ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( <_EQUIP>-EQKTX ) ) ) && '" }'.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
      TRY .
          OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'LT' ).
        CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
      ENDTRY.

      OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

      TRY .
          DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
          OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
        CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
      ENDTRY.

*      Construindo bearer
      CONCATENATE 'Bearer' TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
      CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = 'Authorization'
          VALUE = BEARER_TOKEN.

      CLEAR JSON_RETORNO.
      OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
          EXPORTING
            I_HTTP                     = VAR_HTTP_GET
            I_XML                      = WC_JSON
          IMPORTING
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
*
        FREE: GW_RETURN.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = GW_RETURN ).

        IF GW_RETURN IS NOT INITIAL.
          APPEND VALUE #(
                         ID            = GW_RETURN-ID
                         MENSAGEM      = GW_RETURN-MENSAGEM
                         CODIGOERRO    = GW_RETURN-CODIGOERRO ) TO GT_RETURNG.

          IF GT_RETURNG IS NOT INITIAL.
            READ TABLE GT_RETURNG INTO DATA(_RETURN) INDEX 1.
            IF _RETURN-CODIGOERRO EQ 0.
              W_RETURN-ID = _RETURN-ID.
              W_RETURN-CODIGOERRO = _RETURN-CODIGOERRO.
              CONCATENATE 'EQUIPAMENTO' EQUNR 'CADASTRADO COM SUCESSO' INTO W_RETURN-MENSAGEM SEPARATED BY SPACE.
              APPEND W_RETURN TO T_RETURN.
              CLEAR: W_RETURN.
            ELSE.
              W_RETURN-CODIGOERRO = _RETURN-CODIGOERRO.
              CONCATENATE 'EQUIPAMENTO' EQUNR 'NÃO CADASTRADO' INTO W_RETURN-MENSAGEM SEPARATED BY SPACE.
              APPEND W_RETURN TO T_RETURN.
              CLEAR: W_RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    FREE: GT_RETURNG.
    GT_RETURNG = T_RETURN.
  ENDMETHOD.


  METHOD convert_data.

    DATA(data_cov) = data_conv.

    REPLACE ALL OCCURRENCES OF '-' IN data_cov WITH ' '.

    TRY.
        cl_abap_datfm=>conv_date_ext_to_int( EXPORTING im_datext = data_cov  IMPORTING ex_datint = DATA(data_convertida) ).
        data = data_convertida.
      CATCH cx_abap_datfm_no_date.
      CATCH cx_abap_datfm_invalid_date.
      CATCH cx_abap_datfm_format_unknown.
      CATCH cx_abap_datfm_ambiguous.
    ENDTRY.

  ENDMETHOD.


  METHOD GERAR_TOKEN.

    TYPES BEGIN OF TY_RETORNO_TOKEN.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: USERNAME TYPE STRING.
    TYPES END OF TY_RETORNO_TOKEN.



    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
    CREATE OBJECT OB_WEB_SERVICE.

    DATA: LC_JSON            TYPE STRING,
          WC_JSON            TYPE STRING,
          E_REASON           TYPE STRING,
          JSON_RETORNO       TYPE STRING,
          LC_TOKEN           TYPE TY_RETORNO_TOKEN,
          LS_ZTPM_TOKEN_S360 TYPE ZTPM_TOKEN_S360.

    DATA: LT_LS_ZTPM_TOKEN_S360 TYPE ZTPM_TOKEN_S360.
    DATA(USUARIO) = 'intergraco.als@amaggi.com.br'.
    DATA(SENHA)   = 'amaggi'.

    DATA: TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
    DATA: TL_TOKEN      TYPE STRING.

**    Verificar se existe chave valida para usuario.
*    SELECT SINGLE *
*    FROM ZTPM_TOKEN_S360
*      INTO LT_LS_ZTPM_TOKEN_S360
*      WHERE USERNAME EQ USUARIO.
*
*    IF LT_LS_ZTPM_TOKEN_S360-TOKEN IS INITIAL.

*      COSTRUINDO LOGIN DE ACESSO, PARA GERAR A CHAVE DE SEGURANÇA.
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
      TOKEN = LC_TOKEN-ACCESS_TOKEN.

*        LS_ZTPM_TOKEN_S360 = VALUE #(
*                            MANDT    = SY-MANDT
*                            USERNAME = USUARIO
*                            PASSWORD = SENHA
*                            TOKEN    = LC_TOKEN-ACCESS_TOKEN
*                            LAEDA    = SY-DATUM ).
*
*        MODIFY ZTPM_TOKEN_S360 FROM LS_ZTPM_TOKEN_S360.
*        COMMIT WORK.
    ENDIF.
*    ELSE.
*
*      IF LT_LS_ZTPM_TOKEN_S360-LAEDA NE SY-DATUM.
*
**    Costruindo login de acesso, para gerar a chave de segurança.
*        CLEAR: LC_JSON.
*        LC_JSON = '{"username":' && '"' && USUARIO &&'",' && '"password":' && '"' && SENHA && '"}'.
*
**   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
*        TRY .
*            OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'LS' ).
*          CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
*        ENDTRY.
*        OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).
*
*        TRY .
*            VAR_HTTP = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
*            LC_URI = OB_WEB_SERVICE->GET_URI(  ).
*          CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
*        ENDTRY.
*
*        OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).
*        OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
*            EXPORTING
*              I_HTTP                     = VAR_HTTP
*              I_XML                      = LC_JSON
*            IMPORTING
**          E_CODE                     = E_CODE
*              E_REASON                   = E_REASON
*            RECEIVING
*              E_RESULTADO                = JSON_RETORNO
*            EXCEPTIONS
*              HTTP_COMMUNICATION_FAILURE = 1
*              HTTP_INVALID_STATE         = 2
*              HTTP_PROCESSING_FAILED     = 3
*              HTTP_INVALID_TIMEOUT       = 4
*              OTHERS                     = 5
*          ).
*
**      TL_TEXTO      TYPE CATSXT_LONGTEXT_ITAB.
**      TL_TOKEN      TYPE STRING.
*
*        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = LC_TOKEN ).
*
*        IF SY-SUBRC IS INITIAL.
*          DELETE FROM ZTPM_TOKEN_S360 WHERE USERNAME EQ USUARIO.
*          COMMIT WORK.
*          LS_ZTPM_TOKEN_S360 = VALUE #(
*                              MANDT    = SY-MANDT
*                              USERNAME = USUARIO
*                              PASSWORD = SENHA
*                              TOKEN    = LC_TOKEN-ACCESS_TOKEN
*                              LAEDA    = SY-DATUM ).
*
*          MODIFY ZTPM_TOKEN_S360 FROM LS_ZTPM_TOKEN_S360.
*          COMMIT WORK.
*        ENDIF.
*      ELSE.
*
*        CLEAR LC_TOKEN-ACCESS_TOKEN.
*        LC_TOKEN-ACCESS_TOKEN = LT_LS_ZTPM_TOKEN_S360-TOKEN.
*        TOKEN = LC_TOKEN-ACCESS_TOKEN.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD LISTAR_AMOSTRA.

    TYPES: BEGIN OF TY_ZTPM_L_AMOS.
             INCLUDE TYPE ZTPM_L_AMOST.
             TYPES: CHECK TYPE CHAR1,
           END OF TY_ZTPM_L_AMOS.

    TYPES BEGIN OF TY_RETORNO_TOKEN.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: USERNAME TYPE STRING.
    TYPES END OF TY_RETORNO_TOKEN.

    DATA WC_JSON TYPE STRING.
    DATA(CONTPG) = 1.
    DATA: LC_TOKEN           TYPE TY_RETORNO_TOKEN.
    DATA:JSON_RETORNO       TYPE STRING.
    DATA: E_REASON           TYPE STRING.
    DATA: LS_RES_LST_AMOSTRA TYPE  ZTPM_LIS_AMOST.
    DATA: TS_ZTPM_L_AMOST    TYPE TABLE OF TY_ZTPM_L_AMOS.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
    CREATE OBJECT OB_WEB_SERVICE.

    CALL METHOD GERAR_TOKEN
      IMPORTING
        TOKEN = DATA(TOKEN).



    CLEAR: WC_JSON.
    WC_JSON = '{ "numeroPagina":' && CONTPG && ','
           && ' "numeroAmostra":'  && '"' && NUMEROAMOSTRA && '" }'.

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
    CONCATENATE 'Bearer' TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
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
  ENDMETHOD.


  METHOD listar_compartimento.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto     TYPE catsxt_longtext_itab,
          tl_token     TYPE string,
          lc_token     TYPE ty_retorno_token,
          e_reason     TYPE string,
          json_retorno TYPE string.

    DATA: gw_comp TYPE zpme0020.
    DATA: gt_comp TYPE TABLE OF zpme0020.


    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    IF token IS INITIAL.
      CALL METHOD gerar_token
        IMPORTING
          token = token.
    ENDIF.

    DATA(url_get) = 'https://api.s360web.com' && '/api/v1/equipamento/view' && '?token_type=Bearer&access_token=' && token && '&id=' && id.

    DATA(var_http_get) = ob_web_service->url( EXPORTING i_url = CONV #( url_get ) ).

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url_get
      IMPORTING
        client             = DATA(v_http_get)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
*              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).


    IF json_retorno IS NOT INITIAL.
      FREE: gw_comp.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = gw_comp ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).

      IF gw_comp IS NOT INITIAL.
        gt_comp = VALUE #( ( id                = gw_comp-id
                             tagfrota          = gw_comp-tagfrota
                             compartimentos    = VALUE #( FOR ls IN gw_comp-compartimentos
                                                        ( id   = ls-id
                                                          nome = ls-nome ) ) ) ).

      ENDIF.
    ENDIF.

    IF gt_comp IS NOT INITIAL.
      gt_compart = gt_comp.
    ENDIF.

  ENDMETHOD.


  method LISTAR_FABRICANTE.
  endmethod.


  METHOD listar_fabricante_equip.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto     TYPE catsxt_longtext_itab,
          tl_token     TYPE string,
          lc_token     TYPE ty_retorno_token,
          e_reason     TYPE string,
          json_retorno TYPE string.

    DATA: gw_fam TYPE zpme0027.
    DATA: gt_fam TYPE TABLE OF zpme0028.


    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    IF token IS INITIAL.
      CALL METHOD gerar_token
        IMPORTING
          token = token.
    ENDIF.

    DATA(url_get) = 'https://api.s360web.com' && '/api/v1/equipamento/listFabricante' && '?token_type=Bearer&access_token=' && token.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url_get
      IMPORTING
        client             = DATA(v_http_get)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
*              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).


    IF json_retorno IS NOT INITIAL.
      CLEAR: gw_fam.
      FREE: gt_fam.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = gw_fam ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).

      IF gw_fam IS NOT INITIAL.
        gt_fam = VALUE #( FOR ls IN gw_fam-resultados
                          (  id   = ls-id
                            nome  = ls-nome ) ).

      ENDIF.
    ENDIF.

    IF gt_fam IS NOT INITIAL.
      gt_fabr = gt_fam.
    ENDIF.

  ENDMETHOD.


  METHOD listar_familia_equi.
    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto     TYPE catsxt_longtext_itab,
          tl_token     TYPE string,
          lc_token     TYPE ty_retorno_token,
          e_reason     TYPE string,
          json_retorno TYPE string.

    DATA: gw_fam TYPE zpme0027.
    DATA: gt_fam TYPE TABLE OF zpme0028.


    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    CALL METHOD gerar_token
      IMPORTING
        token = token.

    DATA(url_get) = 'https://api.s360web.com' && '/api/v1/equipamento/listFamilia' && '?token_type=Bearer&access_token=' && token.


    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url_get
      IMPORTING
        client             = DATA(v_http_get)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
*              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).


    IF json_retorno IS NOT INITIAL.
      FREE: gw_fam.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = gw_fam ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).

      IF gw_fam IS NOT INITIAL.
        gt_fam = VALUE #( FOR ls IN gw_fam-resultados
                          (  id   = ls-id
                            nome  = ls-nome ) ).

      ENDIF.
    ENDIF.

    IF gt_fam IS NOT INITIAL.
      gt_clas_equip = gt_fam.
    ENDIF.

  ENDMETHOD.


  METHOD listar_tipo_compatimento.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto     TYPE catsxt_longtext_itab,
          tl_token     TYPE string,
          lc_token     TYPE ty_retorno_token,
          e_reason     TYPE string,
          json_retorno TYPE string.

    DATA: gw_com TYPE zpme0030.
    DATA: gt_com TYPE TABLE OF zpme0029.


    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    IF token IS INITIAL.
      CALL METHOD gerar_token
        IMPORTING
          token = token.
    ENDIF.


    DATA(url_get) = 'https://api.s360web.com' && '/api/v1/tipoCompartimento/list' && '?token_type=Bearer&access_token=' && token.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url_get
      IMPORTING
        client             = DATA(v_http_get)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
*              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).

    IF json_retorno IS NOT INITIAL.
      CLEAR: gw_com.
      FREE: gt_com.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = gw_com ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).

      IF gw_com IS NOT INITIAL.
        gt_com = VALUE #( FOR ls IN gw_com-listatipocompartimentos
                          (  id   = ls-id
                            nome  = ls-nome ) ).

      ENDIF.
    ENDIF.

    IF gt_com IS NOT INITIAL.
      gt_tipcomp = gt_com.
    ENDIF.

  ENDMETHOD.


  METHOD listar_viscosidade.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto     TYPE catsxt_longtext_itab,
          tl_token     TYPE string,
          lc_token     TYPE ty_retorno_token,
          e_reason     TYPE string,
          json_retorno TYPE string.

    DATA: gw_visc TYPE zpme0025.
    DATA: gt_viscos TYPE TABLE OF zpme0025.


    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    IF token IS INITIAL.
      CALL METHOD gerar_token
        IMPORTING
          token = token.
    ENDIF.

    DATA(url_get) = 'https://api.s360web.com' && '/api/v1/amostra/listViscosidadesOleo' && '?token_type=Bearer&access_token=' && token.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url_get
      IMPORTING
        client             = DATA(v_http_get)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
*              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).




    IF json_retorno IS NOT INITIAL.
      FREE: gt_visc.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = gw_visc ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).

      IF gw_visc IS NOT INITIAL.
        gt_visc = VALUE #( FOR ls IN gw_visc-resultados
                           (
                               nome        = ls-nome
                               id          = ls-id     ) ).

      ENDIF.
    ENDIF.

*    IF GT_VISCOS IS NOT INITIAL.
*      GT_VISC = GT_VISCOS.
*    ENDIF.
  ENDMETHOD.


  METHOD list_equipamento.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto     TYPE catsxt_longtext_itab,
          tl_token     TYPE string,
          lc_token     TYPE ty_retorno_token,
          e_reason     TYPE string,
          json_retorno TYPE string.

    DATA: gw_list_equip TYPE zpme0019.
    DATA: gt_list_equip TYPE TABLE OF zpme0018.
    DATA(contpg) = 1.
    DATA(maxpg) = 1.

    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    CALL METHOD gerar_token
      IMPORTING
        token = token.

    DATA(url_get) = 'https://api.s360web.com' && '/api/v1/equipamento/list' && '?token_type=Bearer&access_token=' && token && '&numeroPagina=' && contpg && '&maximoPorPagina=' && maxpg && '&frota=' && equnr.


    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url_get
      IMPORTING
        client             = DATA(v_http_get)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    CALL METHOD v_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
*              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).

    IF sy-subrc IS INITIAL.

      FREE: gw_list_equip.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = gw_list_equip ).
*      ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = JSON_RETORNO I_TITULO = 'Json Retorno' ).

      IF gw_list_equip IS NOT INITIAL.
        gt_list_equip = VALUE #( FOR ls IN gw_list_equip-resultados
                               ( id                = ls-id
                                 tagfrota             = ls-tagfrota ) ).


      ENDIF.
    ENDIF.

    READ TABLE gt_list_equip INTO DATA(w_equip) INDEX 1.
    IF sy-subrc EQ 0.
      MOVE w_equip-id TO id.
    ENDIF.
  ENDMETHOD.


  METHOD LIST_POSI_TOTAL_CONTADOR.
    DATA: GW_PONT TYPE IMPTT.
    DATA: GW_EQUIP TYPE EQUI.
    DATA: GT_IMRG TYPE TABLE OF IMRG.
    DATA: T_IMRG TYPE TABLE OF IMRG.
    DATA: QUANT_TOTAL TYPE IMRC_CNTRC.

    IF EQUNR IS NOT INITIAL AND LOCAS IS NOT INITIAL.
*Selecionando o objeto do equipamento.
      SELECT SINGLE *
      FROM EQUI
      INTO GW_EQUIP
        WHERE EQUNR EQ EQUNR.

      IF GW_EQUIP IS NOT INITIAL.
*Selecionando o ponto do equipamento compartimento óleo.
        SELECT SINGLE *
        FROM IMPTT
        INTO GW_PONT
          WHERE MPOBJ EQ GW_EQUIP-OBJNR
            AND LOCAS EQ LOCAS.


        IF GW_PONT IS NOT INITIAL.
          SELECT *
          FROM IMRG
          INTO TABLE @DATA(_IMRG)
          WHERE POINT EQ @GW_PONT-POINT
            AND VLCOD EQ '0010'.

          IF _IMRG IS NOT INITIAL.

            SORT _IMRG DESCENDING BY VLCOD IDATE ITIME.
            READ TABLE _IMRG INTO DATA(WA_IMRG) INDEX 1.

*     Selecionando as remontas após a ultima troca.
            SELECT *
           FROM IMRG
           INTO TABLE T_IMRG
             WHERE POINT EQ WA_IMRG-POINT
               AND VLCOD EQ '0020'
               AND IDATE > WA_IMRG-IDATE
               AND ITIME > WA_IMRG-ITIME.

          ELSE.
*       Selecionando as remontas após a ultima troca.
            SELECT *
           FROM IMRG
           INTO TABLE T_IMRG
             WHERE POINT EQ WA_IMRG-POINT
               AND VLCOD EQ '0020'.
          ENDIF.

          CLEAR QUANT_TOTAL.

          CHECK T_IMRG IS NOT INITIAL.

          LOOP AT T_IMRG ASSIGNING FIELD-SYMBOL(<REMONT>).

*          Converte posição total do contador.
            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
              EXPORTING
                CHAR_UNIT       = <REMONT>-RECDU
                DECIMALS        = 0
                EXPONENT        = 0
                FLTP_VALUE_SI   = <REMONT>-CDIFF
                INDICATOR_VALUE = 'X'
                MASC_SYMBOL     = ' '
              IMPORTING
                CHAR_VALUE      = QUANT_TOTAL.
            CONDENSE QUANT_TOTAL.

            ADD QUANT_TOTAL TO CDIFC.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.


*Selecionando documentos medição para buscar a posição do utimo documento.
    IF POINT IS NOT INITIAL.
      SELECT *
      FROM IMRG
      INTO TABLE GT_IMRG
        WHERE POINT EQ POINT
           AND IDATE EQ WA_IMRG-IDATE
           AND ITIME EQ WA_IMRG-ITIME.

      CHECK  GT_IMRG IS NOT INITIAL.

      SORT GT_IMRG DESCENDING BY VLCOD IDATE ITIME.

* Selecionando a posição do ultimo contador.
      READ TABLE GT_IMRG INTO DATA(W_IMRG) INDEX 1.
*
*          Converte posição total do contador.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = W_IMRG-RECDU
          DECIMALS        = 0
          EXPONENT        = 0
          FLTP_VALUE_SI   = W_IMRG-CNTRR
          INDICATOR_VALUE = 'X'
          MASC_SYMBOL     = ' '
        IMPORTING
          CHAR_VALUE      = CNTRC.
      CONDENSE CNTRC.
    ENDIF.











  ENDMETHOD.


  METHOD REGITRAR_AMOSTRA.

    DATA WC_JSON TYPE STRING.

    IF GT_AMOSTRA IS NOT INITIAL.

      TYPES BEGIN OF TY_RETORNO_TOKEN.
      TYPES: ACCESS_TOKEN TYPE STRING.
      TYPES: USERNAME TYPE STRING.
      TYPES END OF TY_RETORNO_TOKEN.

      DATA: TL_TEXTO     TYPE CATSXT_LONGTEXT_ITAB,
            TL_TOKEN     TYPE STRING,
            LC_TOKEN     TYPE TY_RETORNO_TOKEN,
            E_REASON     TYPE STRING,
            JSON_RETORNO TYPE STRING.

      DATA: GW_RETURN TYPE ZPME0022.
      DATA: W_RETURN TYPE ZPME0022.
      DATA: W_ZPMT0023 TYPE ZPMT0023.
      DATA: T_RETURN TYPE TABLE OF ZPME0022.
      DATA: EQUNR TYPE EQUNR.
      DATA: DATA TYPE CHAR10.
*      DATA(HEADERS) = 'application/json'.


      DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
      CREATE OBJECT OB_WEB_SERVICE.


      CALL METHOD GERAR_TOKEN
        IMPORTING
          TOKEN = DATA(TOKEN).


      LOOP AT GT_AMOSTRA ASSIGNING FIELD-SYMBOL(<_AMOSTR>).

        SELECT SINGLE *
        FROM ZPMT0023
        INTO @DATA(_ZPMT0023)
          WHERE NUMEROAMOSTRA EQ @<_AMOSTR>-NUMEROAMOSTRA
            AND STATUS_PROC EQ @ABAP_TRUE.

        IF _ZPMT0023 IS INITIAL.

          CLEAR: EQUNR.
          EQUNR = |{ <_AMOSTR>-CODIGOEXTERNO ALPHA = OUT }|.

*        Convertendo data.
          DATA =  <_AMOSTR>-DATACOLETA(4) && '-' &&  <_AMOSTR>-DATACOLETA+4(2) && '-' && <_AMOSTR>-DATACOLETA+6(2).

*       Construindo JSON.
          CLEAR: WC_JSON.
          WC_JSON = '{ "tipoColeta":' && '"' && <_AMOSTR>-TIPOCOLETA &&'",'
           && ' "numeroAmostra":' && '"' && <_AMOSTR>-NUMEROAMOSTRA &&'",'
           && ' "codigoExterno":' && '"' && EQUNR &&'",'
           && ' "idObraTipoColetaAvulsa":' && <_AMOSTR>-IDOBRATIPOCOLETAAVULSA &&','
           && ' "idViscosidade":' && <_AMOSTR>-IDVISCOSIDADE &&','
           && ' "idCompartimentoTipoColetaEquipamento":' && <_AMOSTR>-IDCOMPARTIMENTO &&','
           && ' "oleoTrocado":' && '"' && <_AMOSTR>-OLEOTROCADO &&'",'
           && ' "dataColeta":' && '"' && DATA &&'",'
           && ' "idFabricanteOleo":' && '" ",'
           && ' "detalhesOleo":' && '"' && ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( <_AMOSTR>-DETALHESOLEO ) ) ) &&'",'
           && ' "horasEquipamentoTipoColetaEquipamento":' && <_AMOSTR>-HORASEQUIPAMENTO &&','
           && ' "horasOleo":' && <_AMOSTR>-HORASOLEO  &&','
           && ' "volumeAdicionado":' && <_AMOSTR>-VOLUMEADICIONADO &&','
           && ' "comentario":' && '"' && <_AMOSTR>-COMENTARIO && '" }'.


*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
          TRY .
              OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'LW' ).
            CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
          ENDTRY.
          OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'L' ).

          TRY .
              DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
              OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
            CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
          ENDTRY.

*      Construindo bearer
          CONCATENATE 'Bearer' TOKEN INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.

*      Adicionando parametros.
          CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
            EXPORTING
              NAME  = 'Authorization'
              VALUE = BEARER_TOKEN.


          CLEAR JSON_RETORNO.
          OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
              EXPORTING
                I_HTTP                     = VAR_HTTP_GET
                I_XML                      = WC_JSON
              IMPORTING
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

            FREE: GW_RETURN, GT_RETURNG.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = GW_RETURN ).

            IF GW_RETURN IS NOT INITIAL.
              APPEND VALUE #(
                             MENSAGEM      = GW_RETURN-MENSAGEM
                             CODIGOERRO    = GW_RETURN-CODIGOERRO
                             NUMEROAMOSTRA =  <_AMOSTR>-NUMEROAMOSTRA ) TO GT_RETURNG.

              IF GT_RETURNG IS NOT INITIAL.
                READ TABLE GT_RETURNG INTO DATA(_RETURN) INDEX 1.
                IF _RETURN-CODIGOERRO EQ 0.
                  W_RETURN-CODIGOERRO = _RETURN-CODIGOERRO.
                  CONCATENATE 'AMOSTRA' _RETURN-NUMEROAMOSTRA 'REGISTRADO COM SUCESSO' INTO W_RETURN-MENSAGEM SEPARATED BY SPACE.
                  W_RETURN-NUMEROAMOSTRA = _RETURN-NUMEROAMOSTRA.
                  APPEND W_RETURN TO T_RETURN.
                  CLEAR: W_RETURN.

                  MOVE-CORRESPONDING <_AMOSTR> TO W_ZPMT0023.
                  W_ZPMT0023-STATUS_PROC = ABAP_TRUE.

                  MODIFY ZPMT0023 FROM W_ZPMT0023.
                  COMMIT WORK.
                ELSE.
                  W_RETURN-CODIGOERRO = _RETURN-CODIGOERRO.
                  CONCATENATE 'AMOSTRA' _RETURN-NUMEROAMOSTRA 'NÃO CADASTRADA' INTO W_RETURN-MENSAGEM SEPARATED BY SPACE.
                  W_RETURN-NUMEROAMOSTRA = _RETURN-NUMEROAMOSTRA.
                  APPEND W_RETURN TO T_RETURN.
                  CLEAR: W_RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          W_RETURN-CODIGOERRO = '999'.

          CONCATENATE 'AMOSTRA' _RETURN-NUMEROAMOSTRA 'JA ESTA REGISTRADA' INTO W_RETURN-MENSAGEM SEPARATED BY SPACE.
          W_RETURN-NUMEROAMOSTRA = _RETURN-NUMEROAMOSTRA.
          APPEND W_RETURN TO T_RETURN.
          CLEAR: W_RETURN.
        ENDIF.
      ENDLOOP.

      FREE: GT_RETURNG.
      GT_RETURNG = T_RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD SELECIONAR_DADOS_EQUP.
    DATA: T_USERMD            TYPE STANDARD TABLE OF  RGSB4 .

    IF EQUNR IS NOT INITIAL.

      DATA(V_EQUNR) = |{ EQUNR ALPHA = IN }|.
      SELECT *
      FROM V_EQUI AS A
      INNER JOIN  FLEET AS B ON B~OBJNR EQ A~OBJNR
      INNER JOIN EQKT AS C ON C~EQUNR EQ A~EQUNR
        INTO CORRESPONDING FIELDS OF TABLE GT_EQUP
        WHERE A~EQUNR EQ V_EQUNR.

      CHECK GT_EQUP IS NOT INITIAL.

      LOOP AT GT_EQUP ASSIGNING FIELD-SYMBOL(<_EQUI>).

*        Buscando empresa TABELA SETLEAF.
        SELECT SINGLE *
        FROM SETLEAF
        INTO @DATA(_OBRA)
          WHERE SETNAME EQ 'DPARA_ALS_SAP_PM'
            AND VALFROM EQ @<_EQUI>-SWERK.

        IF _OBRA IS INITIAL.
          MESSAGE 'Empresa não esta cadastrada no SET DPARA_ALS_SAP_PM' TYPE 'I' DISPLAY LIKE 'E'.

        ELSE.
          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              CLASS           = '0000'
              SETNR           = 'DPARA_ALS_SAP_PM'
              NO_DESCRIPTIONS = ' '
            TABLES
              SET_VALUES      = T_USERMD
            EXCEPTIONS
              SET_NOT_FOUND   = 1
              OTHERS          = 2.

          IF T_USERMD IS NOT INITIAL.
            READ TABLE T_USERMD INTO DATA(WA_USERMD) WITH KEY FROM = <_EQUI>-SWERK.
            <_EQUI>-IDOBRA = WA_USERMD-TITLE.
          ENDIF.
        ENDIF.

        <_EQUI>-CODIGOEXTERNO = EQUNR.

*Selecionado tipo de metrica de controle
        SELECT SINGLE *
        FROM IMPTT
        INTO @DATA(IMPT)
          WHERE MPOBJ EQ @<_EQUI>-OBJNR
            AND MPTYP EQ 'V'.

        IF IMPT-PSORT EQ 'HORIMETRO'.
          <_EQUI>-METRICACONTROLE = 'HORA'.
        ELSEIF IMPT-PSORT EQ 'ODOMETRO'.
          <_EQUI>-METRICACONTROLE = 'KILOMETRO'.
        ELSE.
          <_EQUI>-METRICACONTROLE = 'PERIODO'.
        ENDIF.

*       Selecionando a area.
        SELECT SINGLE *
        FROM TGSBT
        INTO @DATA(_AREA)
          WHERE GSBER EQ @<_EQUI>-SWERK
           AND SPRAS EQ @SY-LANGU.
        <_EQUI>-AREA = _AREA-GTEXT.

*        Selecionando setor.
        SELECT SINGLE *
        FROM CSKT
        INTO @DATA(_SETOR)
          WHERE KOSTL EQ @<_EQUI>-KOSTL
            AND SPRAS EQ @SY-LANGU.
        <_EQUI>-SETOR = _SETOR-KTEXT.
        <_EQUI>-TAMANHOATUAL = 1.
        CONDENSE <_EQUI>-TAMANHOATUAL.

*        <_EQUI>-IDFABR = 2659.
*        CONDENSE <_EQUI>-IDFABR.

*        <_EQUI>-IDFAM = 4288.
*        CONDENSE <_EQUI>-IDFAM.

      ENDLOOP.
    ENDIF.




  ENDMETHOD.
ENDCLASS.

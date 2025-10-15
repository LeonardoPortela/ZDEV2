class ZCL_CCT_RECEPCAO_CARGA definition
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

  methods CONSTRUCTOR
    importing
      value(I_ID_RECEPCAO) type ZID_RECEPCAO optional .
  methods GET_ID_RECEPCAO
    returning
      value(E_ID_RECEPCAO) type ZID_RECEPCAO .
  methods SET_ID_RECEPCAO
    importing
      value(I_ID_RECEPCAO) type ZID_RECEPCAO .
  methods SET_TP_RECEPCAO
    importing
      value(I_TP_RECEPCAO) type ZTP_RECEPCAO_CARGA_CCT .
  methods GET_TP_RECEPCAO
    returning
      value(E_TP_RECEPCAO) type ZTP_RECEPCAO_CARGA_CCT .
  methods SET_BUKRS
    importing
      value(I_BUKRS) type BUKRS .
  methods GET_BUKRS
    returning
      value(E_BUKRS) type BUKRS .
  methods SET_BRANCH
    importing
      value(I_BRANCH) type J_1BBRANC_ .
  methods GET_BRANCH
    returning
      value(E_BRANCH) type J_1BBRANC_ .
  methods GET_CNPJ_RESPONSAVEL
    returning
      value(E_CNPJ_RESPONSAVEL) type STCD1 .
  methods SET_CNPJ_RESPONSAVEL
    importing
      value(I_CNPJ_RESPONSAVEL) type STCD1 optional .
  methods GET_LOCAL_CODIGO_URF
    returning
      value(E_LOCAL_CODIGO_URF) type NUMC7 .
  methods SET_LOCAL_CODIGO_URF
    importing
      value(I_LOCAL_CODIGO_URF) type NUMC7 .
  methods GET_LOCAL_CODIGO_RA
    returning
      value(E_LOCAL_CODIGO_RA) type NUMC7 .
  methods SET_LOCAL_CODIGO_RA
    importing
      value(I_LOCAL_CODIGO_RA) type NUMC7 .
  methods GET_LOCAL_LATITUDE
    returning
      value(E_LOCAL_LATITUDE) type CHAR11 .
  methods SET_LOCAL_LATITUDE
    importing
      value(I_LOCAL_LATITUDE) type CHAR11 .
  methods GET_LOCAL_LONGITUDE
    returning
      value(E_LOCAL_LONGITUDE) type CHAR11 .
  methods SET_LOCAL_LONGITUDE
    importing
      value(I_LOCAL_LONGITUDE) type CHAR11 .
  methods GET_REFERENCIA_LOCAL_RECEPCAO
    returning
      value(E_REFERENCIA_LOCAL_RECEPCAO) type ZCHAR170 .
  methods SET_REFERENCIA_LOCAL_RECEPCAO
    importing
      value(I_REFERENCIA_LOCAL_RECEPCAO) type ZCHAR170 .
  methods ADD_NFE
    importing
      value(I_CHAVE_NFE) type CHAR44
      !I_COMPLEMENTO type CHAR01 optional .
  methods DEL_NFE
    importing
      value(I_CHAVE_NFE) type CHAR44 .
  methods ADD_NFF
    importing
      !I_CHAVE_NFF type ZDE_CHAVE_NFF
      !I_COMPLEMENTO type CHAR01 optional .
  methods GET_TRANSPORTADOR_CNPJ
    returning
      value(E_TRANSPORTADOR_CNPJ) type STCD1 .
  methods SET_TRANSPORTADOR_CNPJ
    importing
      value(I_TRANSPORTADOR_CNPJ) type STCD1 .
  methods SET_TRANSPORTADOR_CPF
    importing
      value(I_TRANSPORTADOR_CPF) type STCD2 .
  methods GET_TRANSPORTADOR_CPF
    returning
      value(E_TRANSPORTADOR_CPF) type STCD2 .
  methods GET_PESO_AFERIDO_RECEPCAO
    returning
      value(E_PESO_AFERIDO_RECEPCAO) type BRGEW .
  methods SET_PESO_AFERIDO_RECEPCAO
    importing
      value(I_PESO_AFERIDO_RECEPCAO) type BRGEW .
  methods GET_LOCAL_ARMAZENAMENTO
    returning
      value(E_LOCAL_ARMAZENAMENTO) type ZCHAR250 .
  methods SET_LOCAL_ARMAZENAMENTO
    importing
      value(I_LOCAL_ARMAZENAMENTO) type ZCHAR250 .
  methods GET_CODIGO_IDENT_CARGA
    returning
      value(E_CODIGO_IDENT_CARGA) type ZCHAR250 .
  methods SET_CODIGO_IDENT_CARGA
    importing
      value(I_CODIGO_IDENT_CARGA) type ZCHAR250 .
  methods GET_AVARIAS_IDENTIFICADAS
    returning
      value(E_AVARIAS_IDENTIFICADAS) type ZCHAR250 .
  methods SET_AVARIAS_IDENTIFICADAS
    importing
      value(I_AVARIAS_IDENTIFICADAS) type ZCHAR250 .
  methods GET_DIVERGENCIAS_IDENTIFICADAS
    returning
      value(E_DIVERGENCIAS_IDENTIFICADAS) type ZCHAR250 .
  methods SET_DIVERGENCIAS_IDENTIFICADAS
    importing
      value(I_DIVERGENCIAS_IDENTIFICADAS) type ZCHAR250 .
  methods GET_OBSERVACOES_GERAIS
    returning
      value(E_OBSERVACOES_GERAIS) type ZCHAR250 .
  methods SET_OBSERVACOES_GERAIS
    importing
      value(I_OBSERVACOES_GERAIS) type ZCHAR250 .
  methods SET_MOTIVO_NAO_PESAGEM
    importing
      !I_MOTIVO_NAO_PESAGEM type ZCHAR250 .
  methods SET_STATUS
    importing
      value(I_STATUS) type ZST_REC_CG_CCT .
  methods GET_STATUS
    returning
      value(E_STATUS) type ZST_REC_CG_CCT .
  methods SET_TOKEN
    importing
      value(I_TOKEN) type ref to ZCL_TOKEN_SISCOMEX .
  methods CANCELAR_RECEPCAO
    returning
      value(I_CANCELADA) type CHAR01 .
  methods RECEPCIONAR_CARGA
    returning
      value(I_RECEPCIONADA) type CHAR01
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT
      ZCX_ENVIO .
  methods GET_XML_RECEPCAO
    importing
      !I_DOWNLOAD type CHAR1 optional
    returning
      value(E_XML_REC_NF) type STRING .
  methods MONTA_XML
    importing
      value(I_TIPO) type CHAR01
    exporting
      value(E_ERRO) type CHAR01
    changing
      value(E_XML_REC_NF) type STRING optional
    returning
      value(E_XML) type STRING .
  methods CONSULTAR_ESTOQUE_PRE_ACD
    importing
      !I_CHAVES_NFE type ZDE_CHAVE_DOC_E_T
    exporting
      !E_ESTOQUE_NF_CCT_JS type ZDE_ESTOQUE_NF_CCT_JS
    raising
      ZCX_CCT .
  methods GERAR_RECEPCAO
    importing
      !I_ZLEST0146 type ZLEST0146
      !I_ZLEST0147 type ZLEST0147
    returning
      value(R_RETORNO_PROC) type ZDE_RETORNO_PROC .
  PROTECTED SECTION.
private section.

  data AT_RECEPCAO type ZLEST0146 .
  data AT_NF type ZLEST0147_T .
  data AT_TOKEN type ref to ZCL_TOKEN_SISCOMEX .

  methods CONFIRMAR_RECEPCAO
    importing
      !I_CONFIRMADO_RECEPCAO_PORTAL type CHAR01 optional .
ENDCLASS.



CLASS ZCL_CCT_RECEPCAO_CARGA IMPLEMENTATION.


  METHOD ADD_NFE.

    DATA: WL_0147 TYPE ZLEST0147.

    WL_0147-ID_RECEPCAO = ME->AT_RECEPCAO-ID_RECEPCAO.
    WL_0147-CHAVE_NFE   = I_CHAVE_NFE.
    WL_0147-COMPLEMENTO = I_COMPLEMENTO.

    APPEND WL_0147 TO ME->AT_NF.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  method ADD_NFF.

    DATA: WL_0147 TYPE ZLEST0147.

    WL_0147-ID_RECEPCAO = ME->AT_RECEPCAO-ID_RECEPCAO.
    WL_0147-CHAVE_NFF   = I_CHAVE_NFF.
    WL_0147-COMPLEMENTO = I_COMPLEMENTO.

    APPEND WL_0147 TO ME->AT_NF.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  METHOD cancelar_recepcao.

    DATA: v_chave TYPE c LENGTH 44.

    i_cancelada = abap_false.

    SELECT SINGLE *
      FROM zlest0146 INTO @DATA(wl_0146)
     WHERE id_recepcao = @me->at_recepcao-id_recepcao.

    IF ( sy-subrc NE 0 ) OR ( me->at_recepcao-id_recepcao IS INITIAL ).
      MESSAGE s015 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM zlest0147 INTO @DATA(wl_0147)
     WHERE id_recepcao EQ @wl_0146-id_recepcao.

    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM zsdt_retlote INTO @DATA(_wl_retlote)
       WHERE docnum = @wl_0147-docnum.

      IF sy-subrc EQ 0.
        MESSAGE s150 WITH _wl_retlote-docnum_ret DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

*    IF ( ME->AT_RECEPCAO-STATUS EQ '1' ).
*      MESSAGE S030 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.

    CASE me->at_recepcao-tp_recepcao.
      WHEN '1' OR "NF-e
           '2'.   "NF-f

        LOOP AT me->at_nf INTO DATA(wl_nf).

          IF wl_nf-chave_nfe IS NOT INITIAL.
            v_chave = wl_nf-chave_nfe.
          ELSEIF wl_nf-chave_nff IS NOT INITIAL.
            v_chave = wl_nf-chave_nff.
          ENDIF.

          SELECT SINGLE *
            FROM zlest0142 INTO @DATA(_wl_0142)
           WHERE chave_nfe = @wl_nf-chave_nfe
             AND chave_nff = @wl_nf-chave_nff.

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            MESSAGE s029 WITH v_chave DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          _wl_0142-id_recepcao = space.
          MODIFY zlest0142 FROM _wl_0142.

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            MESSAGE s032 WITH v_chave DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          "Atualizar Vinculos CCT
          DATA(_atualizado) = zcl_cct_control_nf=>atualiza_st_cct_registros( EXPORTING i_zlest0142  =  _wl_0142
                                                                                       i_st_cct     =  '01'  "Disponibilizado Registro CCT
                                                                            ).
          IF _atualizado EQ abap_false.
            ROLLBACK WORK.
            RETURN.
          ENDIF.

          DATA(_tg_0001) = zcl_cct_control_nf=>get_romaneios_nf( EXPORTING i_zlest0142 = _wl_0142 ).

          IF ( _tg_0001[] IS INITIAL ) AND ( _wl_0142-sem_romaneio EQ abap_false ).
            ROLLBACK WORK.
            MESSAGE s037 WITH v_chave DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          LOOP AT _tg_0001 INTO DATA(_wl_0001).

            "Desbloquear Romaneios Opus
            CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
              EXPORTING
                cd_referencia = _wl_0001-ch_referencia
                tp_bloqueio   = ''.
          ENDLOOP.


          "DELETAR o registro da tabela ZLEST0186 - CS2021000291- AOENNING.
          "===============================================================
          DELETE FROM zlest0186 WHERE chave EQ v_chave.
          "===============================================================

        CLEAR: v_chave.
        ENDLOOP.


      WHEN '3'. "DU-E/RUC
    ENDCASE.

    wl_0146-cancel    = 'X'.
    wl_0146-dt_cancel = sy-datum.
    wl_0146-hr_cancel = sy-uzeit.
    wl_0146-us_cancel = sy-uname.

    MODIFY zlest0146 FROM wl_0146.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s017 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    i_cancelada = abap_true.

    MESSAGE s016 DISPLAY LIKE 'S'.


  ENDMETHOD.


  method CONFIRMAR_RECEPCAO.

    ME->SET_STATUS( '1' ). "Recepcionada Portal.

    ME->AT_RECEPCAO-CONFIRMADO_RECEPCAO_PORTAL = I_CONFIRMADO_RECEPCAO_PORTAL.

    UPDATE ZLEST0146 SET STATUS = '1'
                         CONFIRMADO_RECEPCAO_PORTAL = I_CONFIRMADO_RECEPCAO_PORTAL
     WHERE ID_RECEPCAO = ME->AT_RECEPCAO-ID_RECEPCAO.

    COMMIT WORK.

  endmethod.


  METHOD CONSTRUCTOR.

    REFRESH ME->AT_NF.

    CHECK ( I_ID_RECEPCAO IS NOT INITIAL ).

    "Dados CCT
    SELECT SINGLE *
      FROM ZLEST0146 INTO @DATA(_WL_ZLEST0146)
     WHERE ID_RECEPCAO = @I_ID_RECEPCAO.

    CHECK SY-SUBRC = 0.

    "NF-e CCT.
    SELECT *
      FROM ZLEST0147 INTO TABLE @DATA(_IT_ZLEST0147)
     WHERE ID_RECEPCAO = @I_ID_RECEPCAO.

    ME->SET_ID_RECEPCAO(                I_ID_RECEPCAO                   = _WL_ZLEST0146-ID_RECEPCAO ).
    ME->SET_TP_RECEPCAO(                I_TP_RECEPCAO                   = _WL_ZLEST0146-TP_RECEPCAO ).
    ME->SET_STATUS(                     I_STATUS                        = _WL_ZLEST0146-STATUS ).
    ME->SET_BUKRS(                      I_BUKRS                         = _WL_ZLEST0146-BUKRS  ).
    ME->SET_BRANCH(                     I_BRANCH                        = _WL_ZLEST0146-BRANCH ).
    ME->SET_CNPJ_RESPONSAVEL(           I_CNPJ_RESPONSAVEL              = _WL_ZLEST0146-CNPJ_RESPONSAVEL ).
    ME->SET_LOCAL_CODIGO_URF(           I_LOCAL_CODIGO_URF              = _WL_ZLEST0146-LOCAL_CODIGO_URF ).
    ME->SET_LOCAL_CODIGO_RA(            I_LOCAL_CODIGO_RA               = _WL_ZLEST0146-LOCAL_CODIGO_RA ).
    ME->SET_LOCAL_LATITUDE(             I_LOCAL_LATITUDE                = _WL_ZLEST0146-LOCAL_LATITUDE ).
    ME->SET_LOCAL_LONGITUDE(            I_LOCAL_LONGITUDE               = _WL_ZLEST0146-LOCAL_LONGITUDE ).
    ME->SET_REFERENCIA_LOCAL_RECEPCAO(  I_REFERENCIA_LOCAL_RECEPCAO     = _WL_ZLEST0146-REFERENCIA_LOCAL_RECEPCAO ).
    ME->SET_TRANSPORTADOR_CNPJ(         I_TRANSPORTADOR_CNPJ            = _WL_ZLEST0146-TRANSPORTADOR_CNPJ ).
    ME->SET_TRANSPORTADOR_CPF(          I_TRANSPORTADOR_CPF             = _WL_ZLEST0146-TRANSPORTADOR_CPF ).
    ME->SET_PESO_AFERIDO_RECEPCAO(      I_PESO_AFERIDO_RECEPCAO         = _WL_ZLEST0146-PESO_AFERIDO_RECEPCAO ).
    ME->SET_LOCAL_ARMAZENAMENTO(        I_LOCAL_ARMAZENAMENTO           = _WL_ZLEST0146-LOCAL_ARMAZENAMENTO ).
    ME->SET_CODIGO_IDENT_CARGA(         I_CODIGO_IDENT_CARGA            = _WL_ZLEST0146-CODIGO_IDENT_CARGA ).
    ME->SET_AVARIAS_IDENTIFICADAS(      I_AVARIAS_IDENTIFICADAS         = _WL_ZLEST0146-AVARIAS_IDENTIFICADAS ).
    ME->SET_DIVERGENCIAS_IDENTIFICADAS( I_DIVERGENCIAS_IDENTIFICADAS    = _WL_ZLEST0146-DIVERGENCIAS_IDENTIFICADAS ).
    ME->SET_OBSERVACOES_GERAIS(         I_OBSERVACOES_GERAIS            = _WL_ZLEST0146-OBSERVACOES_GERAIS ).

    "Carrega NF Vinculadas
    LOOP AT _IT_ZLEST0147 INTO DATA(WL_ZLEST0147).

      IF WL_ZLEST0147-CHAVE_NFE IS NOT INITIAL.
        ME->ADD_NFE( I_CHAVE_NFE = WL_ZLEST0147-CHAVE_NFE ).
      ELSEIF WL_ZLEST0147-CHAVE_NFF IS NOT INITIAL.
        ME->ADD_NFF( I_CHAVE_NFF = WL_ZLEST0147-CHAVE_NFF ).
      ENDIF.

    ENDLOOP.




  ENDMETHOD.


  METHOD CONSULTAR_ESTOQUE_PRE_ACD.

    DATA: HTTP_CLIENT          TYPE REF TO IF_HTTP_CLIENT,
          XML_RETURN           TYPE REF TO CL_XML_DOCUMENT,
          RETURN_CODE          TYPE I,
          V_DT_REGISTRO_PORTAL TYPE ZSDT0170-DT_REGISTRO_PORTAL,
          V_HR_REGISTRO_PORTAL TYPE ZSDT0170-HR_REGISTRO_PORTAL,
          E_RESULTADO          TYPE STRING,
          V_XML                TYPE STRING,
          V_AUTHORIZATION      TYPE STRING,
          V_X_CSRF_TOKEN       TYPE STRING,
          V_URL                TYPE STRING,
          VL_INI_POS           TYPE I,
          WL_RETORNO_JS        TYPE ZDE_ESTOQUE_NF_CCT_JS,
          V_NFS_CONS           TYPE STRING.


    DATA: JS_PROP_TAB TYPE JS_PROPERTY_TAB,
          JS_OBJ      TYPE REF TO CL_JAVA_SCRIPT.

    CLEAR: V_AUTHORIZATION , V_X_CSRF_TOKEN, E_ESTOQUE_NF_CCT_JS.

    CHECK I_CHAVES_NFE[] IS NOT INITIAL.

    IF ME->AT_TOKEN IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CCT
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGID
                            MSGNO = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGNO
          MSGID  = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGID.
    ENDIF.

    DATA(_ID_TOKEN) = ME->AT_TOKEN->GET_ID_TOKEN( ).
    IF _ID_TOKEN IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CCT
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGID
                            MSGNO = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGNO )
          MSGTY  = 'E'
          MSGNO  = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGNO
          MSGID  = ZCX_CCT=>ZCX_AUTENTICACAO_NOT_FOUND-MSGID.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = TEXT-002.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    DATA(_SRV_CONSULTA_ESTOQUE_PRE_ACD) = 'Consulta Estoque Pré ACD'.
    SELECT SINGLE *
      FROM ZAUTH_WEBSERVICE INTO @DATA(_AUTH_SERVICE)
     WHERE SERVICE = 'CCT_CONSULTA_ESTOQUE_PRE_ACD'.

    IF ( SY-SUBRC NE 0 ) OR ( _AUTH_SERVICE-URL IS INITIAL ).
      RAISE EXCEPTION TYPE ZCX_CCT
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_WEBSERVICE_NOT_FOUND-MSGID
                            MSGNO = ZCX_CCT=>ZCX_WEBSERVICE_NOT_FOUND-MSGNO
                            ATTR1 = CONV #( _SRV_CONSULTA_ESTOQUE_PRE_ACD )
                            )
          MSGTY  = 'E'
          MSGNO  = ZCX_CCT=>ZCX_WEBSERVICE_NOT_FOUND-MSGNO
          MSGID  = ZCX_CCT=>ZCX_WEBSERVICE_NOT_FOUND-MSGID
          MSGV1  = CONV #( _SRV_CONSULTA_ESTOQUE_PRE_ACD ).
    ELSE.
      V_URL = _AUTH_SERVICE-URL.
    ENDIF.

    CLEAR: V_NFS_CONS.
    LOOP AT I_CHAVES_NFE INTO DATA(_CHAVE).
      IF V_NFS_CONS IS INITIAL.
        V_NFS_CONS = _CHAVE.
      ELSE.
        CONCATENATE V_NFS_CONS ',' _CHAVE INTO V_NFS_CONS.
      ENDIF.
    ENDLOOP.

    V_URL = V_URL && '/' && V_NFS_CONS.

    "Call service
    CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
      EXPORTING
        URL                = CONV #( V_URL )
        SSL_ID             = 'DFAULT' "ME->AT_PAR_AUTENTICACAO-SSL_ID
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

    CALL METHOD HTTP_CLIENT->REQUEST->SET_CDATA
      EXPORTING
        DATA   = V_NFS_CONS
        OFFSET = 0
        LENGTH = STRLEN( V_NFS_CONS ).

    CALL METHOD HTTP_CLIENT->SEND
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4.

    CASE SY-SUBRC.
      WHEN 1.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGID
            MSGV1  = CONV #( V_URL ).
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGID
            MSGV1  = CONV #( V_URL ).
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGID
            MSGV1  = CONV #( V_URL ).
      WHEN 4.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGID
            MSGV1  = CONV #( V_URL ).
    ENDCASE.

    CALL METHOD HTTP_CLIENT->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3.

    CASE SY-SUBRC.
      WHEN 1.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_FALHA_COMUNICACAO_WS-MSGID
            MSGV1  = CONV #( V_URL ).
      WHEN 2.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_STATUS_COMUNICACAO_WS-MSGID
            MSGV1  = CONV #( V_URL ).
      WHEN 3.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_FALHA_PROCESSAMENTO_WS-MSGID
            MSGV1  = CONV #( V_URL ).
      WHEN 4.
        HTTP_CLIENT->CLOSE( ).
        RAISE EXCEPTION TYPE ZCX_CCT
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGID
                              MSGNO = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGNO
                              ATTR1 = CONV #( V_URL ) )
            MSGTY  = 'E'
            MSGNO  = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGNO
            MSGID  = ZCX_CCT=>ZCX_TIMEOUT_WS-MSGID
            MSGV1  = CONV #( V_URL ).
    ENDCASE.


    "//Check return content
    CREATE OBJECT XML_RETURN.

    CALL METHOD XML_RETURN->PARSE_STRING
      EXPORTING
        STREAM = HTTP_CLIENT->RESPONSE->GET_CDATA( ).


    HTTP_CLIENT->RESPONSE->GET_STATUS( IMPORTING CODE = RETURN_CODE ).

    E_RESULTADO = HTTP_CLIENT->RESPONSE->GET_CDATA( ).

    "RETURN_CODE = '200'.

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
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_400-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_400-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_400-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_400-MSGID.
        WHEN 401.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_401-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_401-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_401-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_401-MSGID.
        WHEN 403.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_403-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_403-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_403-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_403-MSGID.
        WHEN 404.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_404-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_404-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_404-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_404-MSGID.
        WHEN 422.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_422-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_422-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_422-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_422-MSGID.
        WHEN 500.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_500-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_500-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_500-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_500-MSGID.
        WHEN 503.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_503-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_503-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_503-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_503-MSGID.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE ZCX_CCT
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_OTHERS-MSGID
                                MSGNO = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_OTHERS-MSGNO )
              MSGTY  = 'E'
              MSGNO  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_OTHERS-MSGNO
              MSGID  = ZCX_CCT=>ZCX_RETORNO_SISCOMEX_OTHERS-MSGID.
      ENDCASE.

      RETURN.
    ENDIF.


*  E_RESULTADO  =  E_RESULTADO && '{'.
*  E_RESULTADO  =  E_RESULTADO && '  "estoqueNotasFiscais": [                                                     ' .
*  E_RESULTADO  =  E_RESULTADO && '      {                                                                        ' .
*  E_RESULTADO  =  E_RESULTADO && '          "numero": "51190147067525016969550080000454761902973444",            ' .
*  E_RESULTADO  =  E_RESULTADO && '          "urf": "0320151",                                                    ' .
*  E_RESULTADO  =  E_RESULTADO && '          "recinto": "3932704",                                                ' .
*  E_RESULTADO  =  E_RESULTADO && '          "latitude": "-2.578333",                                             ' .
*  E_RESULTADO  =  E_RESULTADO && '          "longitude": "-44.368461",                                           ' .
*  E_RESULTADO  =  E_RESULTADO && '          "registro": "2019-02-01T12:05:19.131+0000",                          ' .
*  E_RESULTADO  =  E_RESULTADO && '          "responsavel": "15143827000202",                                     ' .
*  E_RESULTADO  =  E_RESULTADO && '          "itens": [                                                           ' .
*  E_RESULTADO  =  E_RESULTADO && '              {                                                                ' .
*  E_RESULTADO  =  E_RESULTADO && '                  "item": 1,                                                   ' .
*  E_RESULTADO  =  E_RESULTADO && '                  "saldo": 51.24                                               ' .
*  E_RESULTADO  =  E_RESULTADO && '              }                                                                ' .
*  E_RESULTADO  =  E_RESULTADO && '          ]                                                                    ' .
*  E_RESULTADO  =  E_RESULTADO && '      },                                                                       ' .
*  E_RESULTADO  =  E_RESULTADO && '      {                                                                        ' .
*  E_RESULTADO  =  E_RESULTADO && '          "numero": "51190147067525016969550080000454771903098803",            ' .
*  E_RESULTADO  =  E_RESULTADO && '          "urf": "0320151",                                                    ' .
*  E_RESULTADO  =  E_RESULTADO && '          "recinto": "3932704",                                                ' .
*  E_RESULTADO  =  E_RESULTADO && '          "latitude": "-2.578333",                                             ' .
*  E_RESULTADO  =  E_RESULTADO && '          "longitude": "-44.368461",                                           ' .
*  E_RESULTADO  =  E_RESULTADO && '          "registro": "2019-02-01T12:05:20.357+0000",                          ' .
*  E_RESULTADO  =  E_RESULTADO && '          "responsavel": "15143827000202",                                     ' .
*  E_RESULTADO  =  E_RESULTADO && '          "itens": [                                                           ' .
*  E_RESULTADO  =  E_RESULTADO && '              {                                                                ' .
*  E_RESULTADO  =  E_RESULTADO && '                  "item": 1,                                                   ' .
*  E_RESULTADO  =  E_RESULTADO && '                  "saldo": 50.15                                               ' .
*  E_RESULTADO  =  E_RESULTADO && '              }                                                                ' .
*  E_RESULTADO  =  E_RESULTADO && '          ]                                                                    ' .
*  E_RESULTADO  =  E_RESULTADO && '      }                                                                        ' .
*  E_RESULTADO  =  E_RESULTADO && '  ],                                                                           ' .
*  E_RESULTADO  =  E_RESULTADO && '  "mensagens": [                                                               ' .
*  E_RESULTADO  =  E_RESULTADO && '  ]                                                                            ' .
*  E_RESULTADO  =  E_RESULTADO && '  }                                                                            ' .

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = E_RESULTADO
      CHANGING
        DATA = WL_RETORNO_JS.

    E_ESTOQUE_NF_CCT_JS = WL_RETORNO_JS.

    HTTP_CLIENT->CLOSE( ).  "SKM / JF - RIMINI - IR122853 - 17.04.23

  ENDMETHOD.


  METHOD DEL_NFE.

    DELETE ME->AT_NF WHERE CHAVE_NFE = I_CHAVE_NFE.

    CHECK SY-SUBRC = 0.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  method GERAR_RECEPCAO.

    DATA: TG_0146  TYPE TABLE OF ZLEST0146.

    DATA: V_MSG TYPE STRING.

    DATA: WL_ZLEST0146    TYPE ZLEST0146,
          WL_ZLEST0147    TYPE ZLEST0147,
          IT_ZLEST0168    TYPE ZLEST0168_T,
          WL_RETORNO_PROC TYPE ZDE_RETORNO_PROC.

    CLEAR: IT_ZLEST0168[], R_RETORNO_PROC.

    WL_ZLEST0146 = I_ZLEST0146.
    WL_ZLEST0147 = I_ZLEST0147.

    IF WL_ZLEST0147-CHAVE_NFE IS NOT INITIAL.

      CLEAR: TG_0146[].
      SELECT *
        FROM ZLEST0146 AS A INTO CORRESPONDING FIELDS OF TABLE TG_0146
       WHERE A~CANCEL  EQ ''
         AND EXISTS ( SELECT *
                        FROM ZLEST0147 AS B
                       WHERE B~ID_RECEPCAO = A~ID_RECEPCAO
                         AND B~CHAVE_NFE   = WL_ZLEST0147-CHAVE_NFE ).

      IF TG_0146[] IS NOT INITIAL.

        READ TABLE TG_0146 INTO DATA(WL_0146) INDEX 1.

        MESSAGE S101 WITH  WL_ZLEST0147-CHAVE_NFE WL_0146-ID_RECEPCAO INTO V_MSG.

        R_RETORNO_PROC-TYPE     = 'W'.
        R_RETORNO_PROC-MSGNO    = SY-MSGNO.
        R_RETORNO_PROC-TEXTO    =  V_MSG.
        RETURN.
      ENDIF.

    ELSEIF WL_ZLEST0147-CHAVE_NFF IS NOT INITIAL.

      CLEAR: TG_0146[].
      SELECT *
        FROM ZLEST0146 AS A INTO CORRESPONDING FIELDS OF TABLE TG_0146
       WHERE A~CANCEL  EQ ''
         AND EXISTS ( SELECT *
                        FROM ZLEST0147 AS B
                       WHERE B~ID_RECEPCAO = A~ID_RECEPCAO
                         AND B~CHAVE_NFF   = WL_ZLEST0147-CHAVE_NFF ).

      IF TG_0146[] IS NOT INITIAL.
        READ TABLE TG_0146 INTO WL_0146 INDEX 1.

        MESSAGE S102 WITH WL_ZLEST0147-CHAVE_NFF WL_0146-ID_RECEPCAO INTO V_MSG.
        R_RETORNO_PROC-TYPE     = 'W'.
        R_RETORNO_PROC-MSGNO    = SY-MSGNO.
        R_RETORNO_PROC-TEXTO    =  V_MSG.
        RETURN.
      ENDIF.

    ELSE.
      RETURN.
    ENDIF.

    IF WL_ZLEST0146-ID_RECEPCAO IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZCCT_RCC'
        IMPORTING
          NUMBER                  = WL_ZLEST0146-ID_RECEPCAO
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.

      IF ( SY-SUBRC NE 0 ) OR ( WL_ZLEST0146-ID_RECEPCAO IS INITIAL ).
        RETURN.
      ENDIF.
    ENDIF.

    MODIFY ZLEST0146 FROM WL_ZLEST0146.

    WL_ZLEST0147-ID_RECEPCAO = WL_ZLEST0146-ID_RECEPCAO.
    MODIFY ZLEST0147 FROM WL_ZLEST0147.

    COMMIT WORK.

*------------------------------------------------------------------------*
*   Processar Recepção
*------------------------------------------------------------------------*

    CLEAR: WL_RETORNO_PROC.

    CALL FUNCTION 'ZCCT_PROC_RECEPCAO_CARGA'
      EXPORTING
        I_GRAVAR_REGISTRO       = ABAP_FALSE
      CHANGING
        C_ZLEST0146             = WL_ZLEST0146
        C_ZLEST0147             = WL_ZLEST0147
        C_ZLEST0168             = IT_ZLEST0168
        C_RETORNO               = WL_RETORNO_PROC.

    DELETE FROM ZLEST0168 WHERE ID_RECEPCAO = WL_ZLEST0147-ID_RECEPCAO.
    IF  IT_ZLEST0168[] IS NOT INITIAL.
      MODIFY ZLEST0168 FROM TABLE IT_ZLEST0168.
    ENDIF.



  endmethod.


  METHOD GET_AVARIAS_IDENTIFICADAS.

    E_AVARIAS_IDENTIFICADAS = ME->AT_RECEPCAO-AVARIAS_IDENTIFICADAS.

  ENDMETHOD.


  METHOD GET_BRANCH.

    E_BRANCH = ME->AT_RECEPCAO-BRANCH.

  ENDMETHOD.


  METHOD GET_BUKRS.

    E_BUKRS = ME->AT_RECEPCAO-BUKRS.

  ENDMETHOD.


  METHOD GET_CNPJ_RESPONSAVEL.
    E_CNPJ_RESPONSAVEL = ME->AT_RECEPCAO-CNPJ_RESPONSAVEL.
  ENDMETHOD.


  METHOD GET_CODIGO_IDENT_CARGA.

    E_CODIGO_IDENT_CARGA = ME->AT_RECEPCAO-CODIGO_IDENT_CARGA.

  ENDMETHOD.


  METHOD GET_DIVERGENCIAS_IDENTIFICADAS.

    E_DIVERGENCIAS_IDENTIFICADAS = ME->AT_RECEPCAO-DIVERGENCIAS_IDENTIFICADAS.

  ENDMETHOD.


  METHOD GET_ID_RECEPCAO.
    E_ID_RECEPCAO = ME->AT_RECEPCAO-ID_RECEPCAO.
  ENDMETHOD.


  METHOD GET_LOCAL_ARMAZENAMENTO.

    E_LOCAL_ARMAZENAMENTO = ME->AT_RECEPCAO-LOCAL_ARMAZENAMENTO.

  ENDMETHOD.


  METHOD GET_LOCAL_CODIGO_RA.

    E_LOCAL_CODIGO_RA = ME->AT_RECEPCAO-LOCAL_CODIGO_RA.

  ENDMETHOD.


  METHOD GET_LOCAL_CODIGO_URF.

    E_LOCAL_CODIGO_URF = ME->AT_RECEPCAO-LOCAL_CODIGO_URF.

  ENDMETHOD.


  METHOD GET_LOCAL_LATITUDE.

    E_LOCAL_LATITUDE = ME->AT_RECEPCAO-LOCAL_LATITUDE.

  ENDMETHOD.


  METHOD GET_LOCAL_LONGITUDE.

    E_LOCAL_LONGITUDE = ME->AT_RECEPCAO-LOCAL_LONGITUDE.

  ENDMETHOD.


  METHOD GET_OBSERVACOES_GERAIS.

    E_OBSERVACOES_GERAIS = ME->AT_RECEPCAO-OBSERVACOES_GERAIS.

  ENDMETHOD.


  METHOD GET_PESO_AFERIDO_RECEPCAO.

    E_PESO_AFERIDO_RECEPCAO = ME->AT_RECEPCAO-PESO_AFERIDO_RECEPCAO.

  ENDMETHOD.


  METHOD GET_REFERENCIA_LOCAL_RECEPCAO.

    E_REFERENCIA_LOCAL_RECEPCAO = ME->AT_RECEPCAO-REFERENCIA_LOCAL_RECEPCAO.

  ENDMETHOD.


  method GET_STATUS.

    E_STATUS = ME->AT_RECEPCAO-STATUS.

  endmethod.


  method GET_TP_RECEPCAO.

    E_TP_RECEPCAO = ME->AT_RECEPCAO-TP_RECEPCAO.

  endmethod.


  METHOD GET_TRANSPORTADOR_CNPJ.

    E_TRANSPORTADOR_CNPJ = ME->AT_RECEPCAO-TRANSPORTADOR_CNPJ.

  ENDMETHOD.


  method GET_TRANSPORTADOR_CPF.

     E_TRANSPORTADOR_CPF = ME->AT_RECEPCAO-TRANSPORTADOR_CPF.

  endmethod.


  method GET_XML_RECEPCAO.

    TYPES: BEGIN OF TY_XML,
             XML TYPE STRING,
           END OF TY_XML.

    DATA: LV_FILENAME   TYPE STRING,
          LV_FULLPATH   TYPE STRING,
          LV_PATH       TYPE STRING,
          LV_ACTION     TYPE I,
          LV_FILE       TYPE STRING.

    DATA: V_XML           TYPE STRING,
          V_XML_REC_NF    TYPE STRING,
          V_NAME_FILE     TYPE STRING,
          WA_XML          TYPE TY_XML,
          IT_XML          TYPE STANDARD TABLE OF TY_XML.

    ME->MONTA_XML( EXPORTING I_TIPO         = ME->AT_RECEPCAO-TP_RECEPCAO
                    CHANGING E_XML_REC_NF   = V_XML_REC_NF
                   RECEIVING E_XML          = V_XML ).

    IF I_DOWNLOAD IS NOT INITIAL.

      CLEAR: IT_XML[].
      WA_XML-XML = V_XML.
      APPEND WA_XML TO IT_XML.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
        EXPORTING
          WINDOW_TITLE      = 'Selecione o dirétório'
          DEFAULT_EXTENSION = 'xml'
          DEFAULT_FILE_NAME = LV_FILE
          FILE_FILTER       = '*.XML'
        CHANGING
          FILENAME          = LV_FILENAME
          PATH              = LV_PATH
          FULLPATH          = LV_FULLPATH
          USER_ACTION       = LV_ACTION
        EXCEPTIONS
          CNTL_ERROR        = 1
          ERROR_NO_GUI      = 2
          OTHERS            = 3.

      CHECK SY-SUBRC = 0.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME                = LV_FULLPATH
        TABLES
          DATA_TAB                = IT_XML
        EXCEPTIONS
          FILE_WRITE_ERROR        = 1
          NO_BATCH                = 2
          GUI_REFUSE_FILETRANSFER = 3
          INVALID_TYPE            = 4
          NO_AUTHORITY            = 5
          UNKNOWN_ERROR           = 6
          HEADER_NOT_ALLOWED      = 7
          SEPARATOR_NOT_ALLOWED   = 8
          FILESIZE_NOT_ALLOWED    = 9
          HEADER_TOO_LONG         = 10
          DP_ERROR_CREATE         = 11
          DP_ERROR_SEND           = 12
          DP_ERROR_WRITE          = 13
          UNKNOWN_DP_ERROR        = 14
          ACCESS_DENIED           = 15
          DP_OUT_OF_MEMORY        = 16
          DISK_FULL               = 17
          DP_TIMEOUT              = 18
          FILE_NOT_FOUND          = 19
          DATAPROVIDER_EXCEPTION  = 20
          CONTROL_FLUSH_ERROR     = 21
          OTHERS                  = 22.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDIF.

    E_XML_REC_NF = V_XML_REC_NF.

  endmethod.


  method MONTA_XML.

    DATA: XVALOR                  TYPE STRING,
          V_C_AUX                 TYPE STRING,
          V_XML_NF                TYPE STRING,
          V_XML_RC_NF             TYPE STRING,
          V_CFOP                  TYPE J_1BCFOP,
          WL_LFA1_DESTINATARIO    TYPE LFA1,
          WL_MAKT                 TYPE MAKT,
          WL_MARC                 TYPE MARC,
          V_LIFNR                 TYPE LFA1-LIFNR,
          V_MENGE_UTRIB           TYPE MENGE_D,
          V_MENGE_UTRIB_11_4      TYPE P DECIMALS 4.

    CLEAR: E_XML, E_XML_REC_NF , V_XML_NF, V_XML_RC_NF.

    DEFINE CONC_XML.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE E_XML XVALOR INTO E_XML.
    END-OF-DEFINITION.

    DEFINE CONC_XML_NF.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE V_XML_NF XVALOR INTO V_XML_NF.
    END-OF-DEFINITION.

    DEFINE CONC_XML_RC_NF.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE V_XML_RC_NF XVALOR INTO V_XML_RC_NF.
    END-OF-DEFINITION.

    CASE I_TIPO.
      WHEN '1' OR "NF-e
           '2'.   "NF-f

*-------------------------------------------------------------------------------------------*
*       Monta XML Notas Fiscais
*-------------------------------------------------------------------------------------------*
        CASE I_TIPO.
          WHEN '1'. "NF-e

            "Monta XML NF-e.
            LOOP AT ME->AT_NF INTO DATA(WL_NF) WHERE CHAVE_NFE IS NOT INITIAL.
              CONC_XML_NF              '<notaFiscalEletronica>'.
              CONC_XML_NF                  '<chaveAcesso>'.
              CONC_XML_NF                      WL_NF-CHAVE_NFE.
              CONC_XML_NF                  '</chaveAcesso>'.
              CONC_XML_NF              '</notaFiscalEletronica>'.
            ENDLOOP.

          WHEN '2'. "NF-f

            "Monta XML NF-f.
            LOOP AT ME->AT_NF INTO WL_NF WHERE CHAVE_NFF IS NOT INITIAL.

              CLEAR: WL_LFA1_DESTINATARIO, WL_MAKT, WL_MARC.

              SELECT SINGLE *
                FROM ZLEST0142 INTO @DATA(_WL_0142)
               WHERE CHAVE_NFF EQ @WL_NF-CHAVE_NFF.

              CHECK SY-SUBRC EQ 0.

              V_LIFNR = _WL_0142-BRANCH_ROM.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT       = V_LIFNR
                IMPORTING
                  OUTPUT      = V_LIFNR.

              SELECT SINGLE *
                FROM LFA1 INTO WL_LFA1_DESTINATARIO
               WHERE LIFNR EQ V_LIFNR.

              SELECT SINGLE *
                FROM MAKT INTO WL_MAKT
               WHERE MATNR  = _WL_0142-MATNR
                 AND SPRAS  = SY-LANGU.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT         = WL_MAKT-MATNR
                IMPORTING
                  OUTPUT        = WL_MAKT-MATNR.

              SELECT SINGLE *
                FROM MARC INTO WL_MARC
               WHERE MATNR = _WL_0142-MATNR
                 AND WERKS = _WL_0142-BRANCH_ROM.

              V_CFOP = _WL_0142-CFOP(4).

              "Converte Unidade Estatistica
              CALL FUNCTION 'ZCCT_GET_UTRIB'
                EXPORTING
                  I_CFOP         = V_CFOP
                  I_NCM          =  WL_MARC-STEUC
                  I_MENGE        = _WL_0142-PESO_CHEGADA
                  I_MATNR        = _WL_0142-MATNR
                  I_MEINS        = 'KG'
                IMPORTING
                  E_QTRIB        = V_MENGE_UTRIB.

              V_MENGE_UTRIB_11_4 = V_MENGE_UTRIB.

              IF V_MENGE_UTRIB_11_4 IS INITIAL.
                E_ERRO = ABAP_TRUE.
                MESSAGE S121 WITH WL_NF-CHAVE_NFF.
                RETURN.
              ENDIF.

              CONC_XML_NF              '<notaFiscalFormulario>'.
              CONC_XML_NF                 '<numeroNF>'.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    INPUT      = _WL_0142-NUMERO
                  IMPORTING
                    OUTPUT     =  V_C_AUX.

                CONDENSE V_C_AUX NO-GAPS.
                CONC_XML_NF                   V_C_AUX.
              CONC_XML_NF                 '</numeroNF>'.

              CONC_XML_NF                 '<serieNF>'.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    INPUT      = _WL_0142-SERIE
                  IMPORTING
                    OUTPUT     =  V_C_AUX.

                CONDENSE V_C_AUX NO-GAPS.
                CONC_XML_NF                    V_C_AUX.
              CONC_XML_NF                 '</serieNF>'.

              CONC_XML_NF                 '<modeloNF>'.
              CONC_XML_NF                    _WL_0142-MODEL.
              CONC_XML_NF                 '</modeloNF>'.

              CONC_XML_NF                 '<AAMM>'.
                V_C_AUX = _WL_0142-DT_EMISSAO+2(2) && _WL_0142-DT_EMISSAO+4(2).
                CONC_XML_NF                  V_C_AUX.
              CONC_XML_NF                 '</AAMM>'.

              CONC_XML_NF                 '<ufEmissor>'.
              CONC_XML_NF                    _WL_0142-SIGLA_UF_EMISSOR.
              CONC_XML_NF                 '</ufEmissor>'.

              CONC_XML_NF                 '<dataEmissao>'.
                V_C_AUX =  _WL_0142-DT_EMISSAO(4) && '-' && _WL_0142-DT_EMISSAO+4(2) && '-' && _WL_0142-DT_EMISSAO+6(2).
                CONC_XML_NF                  V_C_AUX.
              CONC_XML_NF                 '</dataEmissao>'.

              CONC_XML_NF                 '<identificacaoEmissor>'.
              CONC_XML_NF                    '<cpf>'.
              CONC_XML_NF                       _WL_0142-CPF_EMISSOR.
              CONC_XML_NF                    '</cpf>'.
              CONC_XML_NF                 '</identificacaoEmissor>'.

              CONC_XML_NF                 '<destinatario>'.
              CONC_XML_NF                     '<cnpj>'.
              CONC_XML_NF                        WL_LFA1_DESTINATARIO-STCD1.
              CONC_XML_NF                     '</cnpj>'.
              CONC_XML_NF                 '</destinatario>'.

              CONC_XML_NF                 '<itensNFF>'.
              CONC_XML_NF                     '<itemNFF>'.
              CONC_XML_NF                         '<numeroItem>001</numeroItem>'.

              CONC_XML_NF                         '<codigoProduto>'.
              CONC_XML_NF                            WL_MAKT-MATNR.
              CONC_XML_NF                         '</codigoProduto>'.


              CONC_XML_NF                         '<ncm>'.
                V_C_AUX =  WL_MARC-STEUC.
                REPLACE ALL OCCURRENCES OF '.' IN V_C_AUX WITH SPACE.
                CONDENSE V_C_AUX.
                CONC_XML_NF                          V_C_AUX.
              CONC_XML_NF                         '</ncm>'.

              CONC_XML_NF                         '<descricaoProduto>'.
              CONC_XML_NF                            WL_MAKT-MAKTX.
              CONC_XML_NF                         '</descricaoProduto>'.
              CONC_XML_NF                         '<cfop>'.
              CONC_XML_NF                            _WL_0142-CFOP(4).
              CONC_XML_NF                         '</cfop>'.

              CONC_XML_NF                         '<valorTotal>'.
                V_C_AUX = _WL_0142-NETWR.
                CONDENSE V_C_AUX NO-GAPS.
                CONC_XML_NF                          V_C_AUX.
              CONC_XML_NF                         '</valorTotal>'.


              CONC_XML_NF                         '<quantidadeMedidaEstatistica>'.
                V_C_AUX = V_MENGE_UTRIB_11_4.
                CONDENSE V_C_AUX NO-GAPS.
                CONC_XML_NF                             V_C_AUX.
              CONC_XML_NF                         '</quantidadeMedidaEstatistica>'.


              CONC_XML_NF                     '</itemNFF>'.
              CONC_XML_NF                 '</itensNFF>'.
              CONC_XML_NF              '</notaFiscalFormulario>'.

            ENDLOOP.

        ENDCASE.

*-------------------------------------------------------------------------------------------*
*       Monta XML Body Recepção Carga
*-------------------------------------------------------------------------------------------*

        IF I_TIPO EQ '1'. "NF-e
          CONC_XML_RC_NF  '<recepcaoNFE>'.
        ELSEIF I_TIPO EQ '2'. "NF-f
          CONC_XML_RC_NF  '<recepcaoNFF>'.
        ENDIF.

        CONC_XML_RC_NF       '<identificacaoRecepcao>'.
        CONC_XML_RC_NF            ME->AT_RECEPCAO-ID_RECEPCAO.
        CONC_XML_RC_NF       '</identificacaoRecepcao>'.
        CONC_XML_RC_NF       '<cnpjResp>'.
        CONC_XML_RC_NF           ME->AT_RECEPCAO-CNPJ_RESPONSAVEL.
        CONC_XML_RC_NF       '</cnpjResp>'.
        CONC_XML_RC_NF       '<local>'.
        CONC_XML_RC_NF           '<codigoURF>'.
        CONC_XML_RC_NF               ME->AT_RECEPCAO-LOCAL_CODIGO_URF.
        CONC_XML_RC_NF           '</codigoURF>'.
        CONC_XML_RC_NF           '<codigoRA>'.
        CONC_XML_RC_NF               ME->AT_RECEPCAO-LOCAL_CODIGO_RA.
        CONC_XML_RC_NF           '</codigoRA>'.
        CONC_XML_RC_NF       '</local>'.
        CONC_XML_RC_NF       '<notasFiscais>'.
        CONC_XML_RC_NF          V_XML_NF.
        CONC_XML_RC_NF       '</notasFiscais>'.
        CONC_XML_RC_NF       '<transportador>'.

        IF ME->AT_RECEPCAO-TRANSPORTADOR_CNPJ IS NOT INITIAL.
          CONC_XML_RC_NF          '<cnpj>'.
          CONC_XML_RC_NF              ME->AT_RECEPCAO-TRANSPORTADOR_CNPJ.
          CONC_XML_RC_NF          '</cnpj>'.
        ELSEIF ME->AT_RECEPCAO-TRANSPORTADOR_CPF IS NOT INITIAL.
          CONC_XML_RC_NF          '<cpf>'.
          CONC_XML_RC_NF              ME->AT_RECEPCAO-TRANSPORTADOR_CPF.
          CONC_XML_RC_NF          '</cpf>'.
        ENDIF.

        CONC_XML_RC_NF       '</transportador>'.

        IF ME->AT_RECEPCAO-PESO_AFERIDO_RECEPCAO NE 0.

          CONC_XML_RC_NF       '<pesoAferido>'.
          CONC_XML_RC_NF           ME->AT_RECEPCAO-PESO_AFERIDO_RECEPCAO.
          CONC_XML_RC_NF       '</pesoAferido>'.

        ELSEIF ME->AT_RECEPCAO-MOTIVO_NAO_PESAGEM IS NOT INITIAL.

          CONC_XML_RC_NF       '<motivoNaoPesagem>'.
          CONC_XML_RC_NF           ME->AT_RECEPCAO-MOTIVO_NAO_PESAGEM.
          CONC_XML_RC_NF       '</motivoNaoPesagem>'.

        ENDIF.

        IF ME->AT_RECEPCAO-OBSERVACOES_GERAIS IS NOT INITIAL.
          CONC_XML_RC_NF       '<observacoesGerais>'.
          CONC_XML_RC_NF           ME->AT_RECEPCAO-OBSERVACOES_GERAIS.
          CONC_XML_RC_NF       '</observacoesGerais>'.
        ENDIF.

        IF I_TIPO EQ '1'. "NF-e
          CONC_XML_RC_NF  '</recepcaoNFE>'.
        ELSEIF I_TIPO EQ '2'. "NF-f
          CONC_XML_RC_NF  '</recepcaoNFF>'.
        ENDIF.

*-------------------------------------------------------------------------------------------*
*       Monta XML Recepção Carga
*-------------------------------------------------------------------------------------------*

        IF I_TIPO EQ '1'. "NF-e

          CONC_XML  '<recepcoesNFE xsi:schemaLocation="http://www.pucomex.serpro.gov.br/cct RecepcaoNFE.xsd"'.
          CONC_XML               ' xmlns="http://www.pucomex.serpro.gov.br/cct"'.
          CONC_XML               ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
          CONC_XML      V_XML_RC_NF.
          CONC_XML  '</recepcoesNFE>'.

        ELSEIF I_TIPO EQ '2'. "NF-f

          CONC_XML  '<recepcoesNFF xsi:schemaLocation="http://www.pucomex.serpro.gov.br/cct RecepcaoNFF.xsd"'.
          CONC_XML               ' xmlns="http://www.pucomex.serpro.gov.br/cct"'.
          CONC_XML               ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
          CONC_XML      V_XML_RC_NF.
          CONC_XML  '</recepcoesNFF>'.

        ENDIF.


        E_XML_REC_NF = V_XML_RC_NF. "XML Recepção NF

      WHEN '3'. "DU-E/RUC



    ENDCASE.

  ENDMETHOD.


  METHOD RECEPCIONAR_CARGA.

    DATA: HTTP_CLIENT     TYPE REF TO IF_HTTP_CLIENT,
          XML_RETURN      TYPE REF TO CL_XML_DOCUMENT,
          RETURN_CODE     TYPE I,
          E_RESULTADO     TYPE STRING,
          V_XML           TYPE STRING,
          V_AUTHORIZATION TYPE STRING,
          V_X_CSRF_TOKEN  TYPE STRING,
          V_URL           TYPE UI_SRC_URL,
          V_MSG_SHOW      TYPE C LENGTH 200,
          VL_INI_POS      TYPE I,
          VAR_ANSWER      TYPE C.

    CLEAR: V_AUTHORIZATION , V_X_CSRF_TOKEN.

    I_RECEPCIONADA = ABAP_FALSE.

    IF ( ME->AT_TOKEN IS INITIAL ).
      MESSAGE S018 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DATA(_ID_TOKEN) = ME->AT_TOKEN->GET_ID_TOKEN( ).
    IF _ID_TOKEN IS INITIAL.
      MESSAGE S018 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->AT_RECEPCAO-STATUS = '1'.
      MESSAGE S034 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = SY-TABIX
        TEXT       = TEXT-001.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    CASE ME->AT_RECEPCAO-TP_RECEPCAO.
      WHEN '1'. "NF-e
        DATA(_TIPO_REC) = 'NF-e'.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO @DATA(_AUTH_SERVICE)
         WHERE SERVICE = 'CCT_RECEPCAO_CARGA_NFE'.
      WHEN '2'. "NF-f
        _TIPO_REC = 'NF-f'.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO _AUTH_SERVICE
         WHERE SERVICE = 'CCT_RECEPCAO_CARGA_NFF'.
      WHEN '3'. "DU-E/RUC
        _TIPO_REC = 'DU-E/RUC'.
        SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO _AUTH_SERVICE
         WHERE SERVICE = 'CCT_RECEPCAO_CARGA_DUE_RUC'.
    ENDCASE.

    IF ( SY-SUBRC NE 0 ) OR ( _AUTH_SERVICE-URL IS INITIAL ).
      MESSAGE S019 WITH _TIPO_REC DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      V_URL = _AUTH_SERVICE-URL.
    ENDIF.

    ME->MONTA_XML( EXPORTING I_TIPO = ME->AT_RECEPCAO-TP_RECEPCAO
                   IMPORTING E_ERRO = DATA(_ERRO)
                   RECEIVING E_XML  = V_XML ).

    CHECK _ERRO IS INITIAL.

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
      MESSAGE S020 DISPLAY LIKE 'E'.
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

        "Situações onde a nota já encontra-se recepcionada no portal.
        IF ( ( _CODE EQ 'CCTR-ER0512' ) OR ( _MESSAGE EQ 'CCTR-ER0512' ) ) OR
           (
             ( ( _CODE EQ 'CCTR-ER0501' ) OR ( _MESSAGE EQ 'CCTR-ER0501' ) ) AND ME->AT_RECEPCAO-TP_RECEPCAO = '2'
            ).

          MESSAGE I000 WITH V_MSG_SHOW(50) V_MSG_SHOW+50(50) V_MSG_SHOW+100(50) V_MSG_SHOW+150(50).

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TITLEBAR              = 'Confirmação'
              TEXT_QUESTION         = 'Essa nota já encontra-se recepcionada no Portal Siscomex! Deseja confirmar a recepção dessa NF no SAP?'
              TEXT_BUTTON_1         = 'Sim'
              TEXT_BUTTON_2         = 'Não'
              DEFAULT_BUTTON        = '1'
              DISPLAY_CANCEL_BUTTON = ''
            IMPORTING
              ANSWER                = VAR_ANSWER
            EXCEPTIONS
              TEXT_NOT_FOUND        = 1
              OTHERS                = 2.

          IF VAR_ANSWER EQ '1'.
            ME->CONFIRMAR_RECEPCAO( I_CONFIRMADO_RECEPCAO_PORTAL = ABAP_TRUE ).
            I_RECEPCIONADA = ABAP_TRUE.
            RETURN.
          ENDIF.
        ENDIF.

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
            NAME  = 'code'
          RECEIVING
            VALUE = DATA(_RETURN_CODE).

        CALL METHOD XML_RETURN->FIND_SIMPLE_ELEMENT
          EXPORTING
            NAME  = 'message'
          RECEIVING
            VALUE = DATA(_RETURN_MESSAGE).

        IF _RETURN_MESSAGE IS NOT INITIAL.

          V_MSG_SHOW = _RETURN_MESSAGE.

          "Situações onde a nota já encontra-se recepcionada no portal.
          IF ( _RETURN_CODE IS NOT INITIAL  AND _RETURN_CODE EQ 'CCTR-ER0512' ) OR
             ( _RETURN_CODE EQ 'CCTR-ER0501' AND ME->AT_RECEPCAO-TP_RECEPCAO = '2' ).

            MESSAGE I000 WITH V_MSG_SHOW(50) V_MSG_SHOW+50(50) V_MSG_SHOW+100(50) V_MSG_SHOW+150(50).

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                TITLEBAR              = 'Confirmação'
                TEXT_QUESTION         = 'Essa nota já encontra-se recepcionada no Portal Siscomex! Deseja confirmar a recepção dessa NF no SAP?'
                TEXT_BUTTON_1         = 'Sim'
                TEXT_BUTTON_2         = 'Não'
                DEFAULT_BUTTON        = '1'
                DISPLAY_CANCEL_BUTTON = ''
              IMPORTING
                ANSWER                = VAR_ANSWER
              EXCEPTIONS
                TEXT_NOT_FOUND        = 1
                OTHERS                = 2.

            IF VAR_ANSWER EQ '1'.
              ME->CONFIRMAR_RECEPCAO( I_CONFIRMADO_RECEPCAO_PORTAL = ABAP_TRUE ).
              I_RECEPCIONADA = ABAP_TRUE.
              RETURN.
            ENDIF.
          ENDIF.

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

      IF ( _CODE EQ 'CCTR-IN0001' ).

        ME->CONFIRMAR_RECEPCAO( ).
        I_RECEPCIONADA = ABAP_TRUE.

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


  METHOD SET_AVARIAS_IDENTIFICADAS.

    ME->AT_RECEPCAO-AVARIAS_IDENTIFICADAS = I_AVARIAS_IDENTIFICADAS.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_BRANCH.

    ME->AT_RECEPCAO-BRANCH = I_BRANCH.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_BUKRS.

    ME->AT_RECEPCAO-BUKRS = I_BUKRS.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_CNPJ_RESPONSAVEL.

    DATA: WL_BRANCH_DETAIL TYPE BAPIBRANCH.

    IF I_CNPJ_RESPONSAVEL IS NOT INITIAL.

      ME->AT_RECEPCAO-CNPJ_RESPONSAVEL = I_CNPJ_RESPONSAVEL.

    ELSEIF ( ME->AT_RECEPCAO-BUKRS IS NOT INITIAL ) AND ( ME->AT_RECEPCAO-BRANCH IS NOT INITIAL ).

      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          COMPANY         = ME->AT_RECEPCAO-BUKRS
          BRANCH          = ME->AT_RECEPCAO-BRANCH
        IMPORTING
          BRANCH_DETAIL   = WL_BRANCH_DETAIL.

      IF WL_BRANCH_DETAIL-CGC_NUMBER IS NOT INITIAL.
        ME->AT_RECEPCAO-CNPJ_RESPONSAVEL = WL_BRANCH_DETAIL-CGC_NUMBER.
      ENDIF.

    ENDIF.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_CODIGO_IDENT_CARGA.

    ME->AT_RECEPCAO-CODIGO_IDENT_CARGA = I_CODIGO_IDENT_CARGA.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_DIVERGENCIAS_IDENTIFICADAS.

    ME->AT_RECEPCAO-DIVERGENCIAS_IDENTIFICADAS = I_DIVERGENCIAS_IDENTIFICADAS.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_ID_RECEPCAO.

    ME->AT_RECEPCAO-ID_RECEPCAO = I_ID_RECEPCAO.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_LOCAL_ARMAZENAMENTO.

    ME->AT_RECEPCAO-LOCAL_ARMAZENAMENTO = I_LOCAL_ARMAZENAMENTO.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_LOCAL_CODIGO_RA.

    ME->AT_RECEPCAO-LOCAL_CODIGO_RA = I_LOCAL_CODIGO_RA.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_LOCAL_CODIGO_URF.

    ME->AT_RECEPCAO-LOCAL_CODIGO_URF = I_LOCAL_CODIGO_URF.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_LOCAL_LATITUDE.

    ME->AT_RECEPCAO-LOCAL_LATITUDE = I_LOCAL_LATITUDE.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_LOCAL_LONGITUDE.

    ME->AT_RECEPCAO-LOCAL_LONGITUDE = I_LOCAL_LONGITUDE.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  method SET_MOTIVO_NAO_PESAGEM.

    ME->AT_RECEPCAO-MOTIVO_NAO_PESAGEM = I_MOTIVO_NAO_PESAGEM.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  METHOD SET_OBSERVACOES_GERAIS.

    ME->AT_RECEPCAO-OBSERVACOES_GERAIS = I_OBSERVACOES_GERAIS.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_PESO_AFERIDO_RECEPCAO.

    ME->AT_RECEPCAO-PESO_AFERIDO_RECEPCAO = I_PESO_AFERIDO_RECEPCAO.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_REFERENCIA_LOCAL_RECEPCAO.

    ME->AT_RECEPCAO-REFERENCIA_LOCAL_RECEPCAO = I_REFERENCIA_LOCAL_RECEPCAO.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  method SET_STATUS.

    ME->AT_RECEPCAO-STATUS = I_STATUS.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  METHOD SET_TOKEN.

    FREE ME->AT_TOKEN.

    CHECK I_TOKEN IS NOT INITIAL.

*    IF ( I_TOKEN->GET_BUKRS( )  NE ME->AT_RECEPCAO-BUKRS  ).
*      MESSAGE S028 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.

    ME->AT_TOKEN = I_TOKEN.

  ENDMETHOD.


  method SET_TP_RECEPCAO.

   ME->AT_RECEPCAO-TP_RECEPCAO = I_TP_RECEPCAO.

   ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  METHOD SET_TRANSPORTADOR_CNPJ.

    ME->AT_RECEPCAO-TRANSPORTADOR_CNPJ = I_TRANSPORTADOR_CNPJ.

    ME->CK_ALTEROU = ABAP_TRUE.

  ENDMETHOD.


  method SET_TRANSPORTADOR_CPF.

    ME->AT_RECEPCAO-TRANSPORTADOR_CPF = I_TRANSPORTADOR_CPF.

    ME->CK_ALTEROU = ABAP_TRUE.

  endmethod.


  METHOD ZIF_CADASTRO~GET_REGISTRO.
  ENDMETHOD.


  METHOD zif_cadastro~gravar_registro.

    DATA: v_chave      TYPE c LENGTH 44,
          ws_zlest0186 TYPE zlest0186.

    i_gravou = abap_false.

    CHECK me->ck_alterou EQ abap_true.

    CHECK me->validar_registro( ) EQ abap_true.

    IF me->at_recepcao-id_recepcao IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZCCT_RCC'
        IMPORTING
          number                  = me->at_recepcao-id_recepcao
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
        ROLLBACK WORK.
        MESSAGE s011 WITH 'ZCCT_RCC' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ENDIF.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(wl_setleaf)
     WHERE setname = 'ZLES0147_DOWN_XML_RC'
       AND valfrom = @sy-uname.
    IF sy-subrc = 0.
      me->get_xml_recepcao( EXPORTING i_download    = 'X'
                            RECEIVING e_xml_rec_nf  = DATA(_xml_rc) ).
    ENDIF.

    "wpp 22102024 US-153342 --->>>>
    DATA(_force_recepcao_carga) = abap_false.

    IF sy-sysid EQ 'DEV' OR
       sy-sysid EQ 'QAS'.

      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_tvarvc_auth_rec)
       WHERE name = 'CCT_FORCE_RECEPCAO_CARGA'.

      IF sy-subrc EQ 0.
        _force_recepcao_carga = abap_true.
      ENDIF.
    ENDIF.
    "wpp 22102024 US-153342 <<<----

    "Recepcionar Carga
    IF _force_recepcao_carga EQ abap_false. "wpp 22102024 US-153342 <<<----
      me->recepcionar_carga( RECEIVING i_recepcionada = DATA(_recepcionada) ).
    ELSE.
      _recepcionada          = abap_true.
      me->at_recepcao-status =  '1'.
    ENDIF.

    CHECK _recepcionada EQ abap_true.

*-----------------------------------------------------------------------------------*
*     Gravar Cabeçalho Recepção Carga
*-----------------------------------------------------------------------------------*

    me->at_recepcao-dt_recepcao = sy-datum.
    me->at_recepcao-hr_recepcao = sy-uzeit.
    me->at_recepcao-us_recepcao = sy-uname.

    MODIFY zlest0146 FROM me->at_recepcao.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE s013 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CASE me->at_recepcao-tp_recepcao.
      WHEN '1' OR "NF-e
           '2'.   "NF-f

        LOOP AT me->at_nf INTO DATA(wl_nf).

          DATA(_tabix) = sy-tabix.

          IF wl_nf-chave_nfe IS NOT INITIAL.
            v_chave = wl_nf-chave_nfe.
          ELSEIF wl_nf-chave_nff IS NOT INITIAL.
            v_chave = wl_nf-chave_nff.
          ENDIF.

          wl_nf-id_recepcao = me->at_recepcao-id_recepcao.

          SELECT SINGLE *
            FROM zlest0142 INTO @DATA(_wl_0142)
           WHERE chave_nfe = @wl_nf-chave_nfe
             AND chave_nff = @wl_nf-chave_nff.

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            MESSAGE s029 WITH v_chave DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF _wl_0142-docnum IS NOT INITIAL.
            wl_nf-docnum = _wl_0142-docnum.
          ENDIF.

          _wl_0142-id_recepcao = me->at_recepcao-id_recepcao.
          MODIFY zlest0142 FROM _wl_0142.

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            MESSAGE s032 WITH v_chave DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          "Atualizar Vinculos CCT
          DATA(_atualizado) = zcl_cct_control_nf=>atualiza_st_cct_registros( EXPORTING i_zlest0142  =  _wl_0142
                                                                                       i_st_cct     =  '02' "Vinculado CCT
                                                                            ).
          IF _atualizado EQ abap_false.
            ROLLBACK WORK.
            RETURN.
          ENDIF.

          DATA(_tg_0001) = zcl_cct_control_nf=>get_romaneios_nf( EXPORTING i_zlest0142 = _wl_0142 ).

          LOOP AT _tg_0001 INTO DATA(_wl_0001).
            "Bloquear Romaneio Opus
            CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
              EXPORTING
                cd_referencia = _wl_0001-ch_referencia
                tp_bloqueio   = 'X'.
          ENDLOOP.

          MODIFY me->at_nf FROM wl_nf INDEX _tabix.

          "=======================================================CS2021000291 - AOENNNG.
          "Gravar dados da tabela ZLEST0186
          IF wl_nf-chave_nfe IS NOT INITIAL.
            ws_zlest0186 = VALUE #( mandt            =  at_recepcao-mandt
                                    chave            =  wl_nf-chave_nfe
                                    codigo_urf       =  at_recepcao-local_codigo_urf
                                    codigo_ra        =  at_recepcao-local_codigo_ra
                                    latitude         =  at_recepcao-local_latitude
                                    longitude        =  at_recepcao-local_longitude
                                    cnpj_responsavel =  at_recepcao-cnpj_responsavel
                                    dt_recepcao      =  at_recepcao-dt_recepcao
                                    peso_aferido     =  at_recepcao-peso_aferido_recepcao
*                              saldo =  ' '
                                    dt_registro      =  sy-datum
                                    hr_registro      =  sy-uzeit
                                    us_registro      =  sy-uname ).

            MODIFY zlest0186 FROM ws_zlest0186.
          ENDIF.

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            MESSAGE s032 WITH v_chave DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
          "=======================================================

          CLEAR: v_chave.
        ENDLOOP.

        MODIFY zlest0147 FROM TABLE me->at_nf.
        IF sy-subrc NE 0.
          ROLLBACK WORK.
          MESSAGE s014 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      WHEN '3'. "DU-E/RUC
    ENDCASE.

    COMMIT WORK.

    me->ck_alterou = abap_false.
    i_gravou = abap_true.
    MESSAGE s001.


  ENDMETHOD.


  METHOD ZIF_CADASTRO~LIMPAR_REGISTRO.

    CLEAR: ME->AT_RECEPCAO,
           ME->AT_TOKEN,
           ME->AT_NF[].

  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.
    ME->LIMPAR_REGISTRO( ).
  ENDMETHOD.


  method ZIF_CADASTRO~VALIDAR_EXCLUSAO.
  endmethod.


  METHOD zif_cadastro~validar_registro.

    DATA: it_active       TYPE TABLE OF j_1bnfe_active,
          v_candat        TYPE j_1bnfdoc-candat,
          v_qtde_vinc     TYPE zsdt_retlote-quant_vinc,
          v_menge_nf      TYPE ekpo-menge,
          tg_zsdt_retlote TYPE TABLE OF zsdt_retlote.

    DATA: var_answer      TYPE c,
          v_tol_permitida TYPE brgew,
          v_dif_peso      TYPE brgew.

    e_validou = abap_false.

    IF ( me->at_recepcao-tp_recepcao NE '1' ) AND  "NF-e
       ( me->at_recepcao-tp_recepcao NE '2' ) AND  "NF-f
       ( me->at_recepcao-tp_recepcao NE '3' ).     "DU-E/RUC
      MESSAGE s010 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    "Validar Dados Cabeçalho
    CASE me->at_recepcao-tp_recepcao.
      WHEN '1' OR "NF-e
           '2'.   "NF-f

        IF me->at_recepcao-bukrs IS INITIAL.
          MESSAGE s007 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF me->at_recepcao-branch IS INITIAL.
          MESSAGE s008 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF me->at_recepcao-cnpj_responsavel IS INITIAL.
          MESSAGE s002 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF strlen( me->at_recepcao-cnpj_responsavel ) NE 14.
          MESSAGE s154 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.


        IF me->at_recepcao-local_codigo_urf IS INITIAL.
          MESSAGE s003 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF me->at_recepcao-local_codigo_ra IS INITIAL.
          MESSAGE s004 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF ( me->at_recepcao-transportador_cnpj IS INITIAL ) AND ( me->at_recepcao-transportador_cpf IS INITIAL ).
          MESSAGE s005 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF ( me->at_recepcao-transportador_cnpj IS NOT INITIAL ) AND ( me->at_recepcao-transportador_cpf IS NOT INITIAL ).
          MESSAGE s005 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF me->at_recepcao-transportador_cnpj IS NOT INITIAL.
          IF strlen( me->at_recepcao-transportador_cnpj ) NE 14.
            MESSAGE s155 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

        IF me->at_recepcao-transportador_cpf IS NOT INITIAL.
          IF strlen( me->at_recepcao-transportador_cpf ) NE 11.
            MESSAGE s156 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

        DATA(_nf_comp)     = abap_false.
        DATA(_nf_not_comp) = abap_false.

        LOOP AT me->at_nf INTO DATA(wl_nf_vinc).
          IF wl_nf_vinc-complemento EQ abap_true.
            _nf_comp     = abap_true.
          ELSE.
            _nf_not_comp = abap_true.
          ENDIF.
        ENDLOOP.

        IF ( _nf_comp EQ abap_true ) AND ( _nf_not_comp EQ abap_true ).
          MESSAGE s149 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF _nf_comp EQ abap_true.

          IF ( me->at_recepcao-motivo_nao_pesagem IS INITIAL ).
            MESSAGE s144 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF ( me->at_recepcao-peso_aferido_recepcao IS NOT INITIAL ).
            MESSAGE s145 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

        ELSE.

          IF me->at_recepcao-peso_aferido_recepcao IS INITIAL.
            MESSAGE s006 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

        ENDIF.

      WHEN '3'. "DU-E/RUC

    ENDCASE.

    CASE me->at_recepcao-tp_recepcao.
      WHEN '1'. "NF-e

        DELETE me->at_nf WHERE chave_nfe IS INITIAL.

        IF me->at_nf[] IS INITIAL.
          MESSAGE s009 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT me->at_nf INTO DATA(wl_nf).

          DATA(_tabix) = sy-tabix.

          SELECT SINGLE *
            FROM zlest0142 INTO @DATA(_wl_0142)
           WHERE chave_nfe = @wl_nf-chave_nfe.

          IF sy-subrc NE 0.
            MESSAGE s029 WITH wl_nf-chave_nfe DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF _wl_0142-modal EQ '04'. "Modal Ferroviario
*** CS2019001041 - Inicio - CBRAND
*            IF ( _WL_0142-PESO_FISCAL <> _WL_0142-PESO_RATEIO_ORIGEM ) AND
*               ( _WL_0142-LIB_ENVIO_PARC IS INITIAL    ).

            IF ( _wl_0142-peso_transbordo <> _wl_0142-peso_rateio_origem ) AND
               ( _wl_0142-lib_envio_parc IS INITIAL    ).
*** CS2019001041 - Fim - CBRAND

              DATA(_ok) = ''.
              v_dif_peso = _wl_0142-peso_transbordo - _wl_0142-peso_rateio_origem.
              SELECT *
                FROM setleaf  INTO TABLE @DATA(_tg_setleaf)
               WHERE setname = 'ZLES0147_TOL_DIF_CARGA'.

              LOOP AT _tg_setleaf INTO DATA(_wl_setleaf).
                IF ( _wl_setleaf-valfrom(4) = _wl_0142-branch_rom ).
                  CLEAR: v_tol_permitida.
                  v_tol_permitida = _wl_setleaf-valfrom+4(10).

                  IF v_dif_peso < v_tol_permitida.
                    _ok = 'X'.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF _ok IS INITIAL.
                MESSAGE s036 WITH wl_nf-chave_nfe DISPLAY LIKE 'E'.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.

          DATA(_tg_0001) = zcl_cct_control_nf=>get_romaneios_nf( EXPORTING i_zlest0142 = _wl_0142 ).

          IF ( _tg_0001[] IS INITIAL ) AND ( _wl_0142-sem_romaneio EQ abap_false ).

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = |Romaneio da NF-e não encontrado ou foi cancelado! Deseja deletar o registro de envio de carga?|
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF var_answer EQ '1'..
              DELETE FROM zlest0142 WHERE chave_nfe = @wl_nf-chave_nfe.
            ENDIF.

            COMMIT WORK.
            "MESSAGE S035 WITH WL_NF-CHAVE_NFE DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          SELECT SINGLE a~*
            INTO @DATA(_wl_0147)
            FROM zlest0147 AS a INNER JOIN zlest0146 AS b ON  a~id_recepcao = b~id_recepcao
           WHERE a~chave_nfe EQ @wl_nf-chave_nfe
             AND b~cancel    EQ ''.

          IF sy-subrc = 0.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = _wl_0147-id_recepcao
              IMPORTING
                output = _wl_0147-id_recepcao.

            MESSAGE s012 WITH _wl_0147-chave_nfe _wl_0147-id_recepcao DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          "Get Docum.
          IF wl_nf-docnum IS INITIAL.
            CLEAR: it_active[], v_candat.

            SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @it_active
              FROM j_1bnfe_active AS a INNER JOIN j_1bnfdoc AS b ON a~docnum = b~docnum
             WHERE a~regio    EQ @_wl_0142-chave_nfe+00(02)
               AND a~nfyear   EQ @_wl_0142-chave_nfe+02(02)
               AND a~nfmonth  EQ @_wl_0142-chave_nfe+04(02)
               AND a~stcd1    EQ @_wl_0142-chave_nfe+06(14)
               AND a~model    EQ @_wl_0142-chave_nfe+20(02)
               AND a~serie    EQ @_wl_0142-chave_nfe+22(03)
               AND a~nfnum9   EQ @_wl_0142-chave_nfe+25(09)
               AND a~docnum9  EQ @_wl_0142-chave_nfe+34(09)
               AND a~cdv      EQ @_wl_0142-chave_nfe+43(01)
               AND a~direct   EQ '2'
               AND b~docdat   EQ @_wl_0142-dt_emissao
               AND a~form     NE ' '
               AND b~candat   EQ @v_candat
               AND b~cancel   EQ @space.

            DELETE it_active WHERE docnum IS INITIAL.
            READ TABLE it_active INTO DATA(_wl_active) INDEX 1.

            IF _wl_active-docnum IS NOT INITIAL.
              wl_nf-docnum = _wl_active-docnum.
            ENDIF.
          ENDIF.

          MODIFY me->at_nf FROM wl_nf INDEX _tabix.

          IF wl_nf-docnum IS NOT INITIAL.
            CLEAR: tg_zsdt_retlote[].
            SELECT * INTO CORRESPONDING FIELDS OF TABLE tg_zsdt_retlote
              FROM zsdt_retlote AS a INNER JOIN zsdt_export AS b ON a~docnum_ret = b~docnum
             WHERE a~docnum     EQ wl_nf-docnum
               AND b~finalidade EQ 'I'. "Industrialização

            IF ( tg_zsdt_retlote[] IS NOT INITIAL ).
              CLEAR: v_qtde_vinc, v_menge_nf.

              LOOP AT tg_zsdt_retlote INTO DATA(_wl_ret_lote).
                ADD _wl_ret_lote-quant_vinc TO v_qtde_vinc.
              ENDLOOP.

              SELECT SINGLE *
                FROM j_1bnflin INTO @DATA(_wl_lin)
               WHERE docnum EQ @wl_nf-docnum.

              IF sy-subrc EQ 0.
                v_menge_nf = _wl_lin-menge.

                IF _wl_lin-meins NE 'KG'.
                  CALL FUNCTION 'ME_CONVERSION_MEINS'
                    EXPORTING
                      i_matnr             = _wl_lin-matnr
                      i_mein1             = _wl_lin-meins
                      i_meins             = 'KG'
                      i_menge             = _wl_lin-menge
                    IMPORTING
                      menge               = v_menge_nf
                    EXCEPTIONS
                      error_in_conversion = 1
                      no_success          = 2
                      OTHERS              = 3.

                  IF sy-subrc IS NOT INITIAL.
                    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                    RETURN.
                  ENDIF.
                ENDIF.

                IF ( v_qtde_vinc > 0 ) AND ( v_qtde_vinc >= v_menge_nf ).
                  MESSAGE s112 WITH wl_nf-docnum _wl_ret_lote-docnum_ret DISPLAY LIKE 'E'.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.

      WHEN '2'. "NF-f

        DELETE me->at_nf WHERE chave_nff IS INITIAL.

        IF me->at_nf[] IS INITIAL.
          MESSAGE s009 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT me->at_nf INTO wl_nf.

          _tabix = sy-tabix.

          SELECT SINGLE *
            FROM zlest0142 INTO _wl_0142
           WHERE chave_nff = wl_nf-chave_nff.

          IF sy-subrc NE 0.
            MESSAGE s029 WITH wl_nf-chave_nff DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF _wl_0142-matnr IS INITIAL.
            MESSAGE s122 WITH wl_nf-chave_nff DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF _wl_0142-cfop IS INITIAL.
            MESSAGE s123 WITH wl_nf-chave_nff DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          _tg_0001 = zcl_cct_control_nf=>get_romaneios_nf( EXPORTING i_zlest0142 = _wl_0142 ).

          IF ( _tg_0001[] IS INITIAL ) AND ( _wl_0142-sem_romaneio EQ abap_false ).

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = |Romaneio da NF-f não encontrado ou foi cancelado! Deseja deletar o registro de envio de carga?|
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF var_answer EQ '1'..
              DELETE FROM zlest0142 WHERE chave_nff = @wl_nf-chave_nff.
            ENDIF.

            COMMIT WORK.
            RETURN.
          ENDIF.

          SELECT SINGLE a~*
            INTO @_wl_0147
            FROM zlest0147 AS a INNER JOIN zlest0146 AS b ON  a~id_recepcao = b~id_recepcao
           WHERE a~chave_nff EQ @wl_nf-chave_nff
             AND b~cancel    EQ ''.

          IF sy-subrc = 0.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = _wl_0147-id_recepcao
              IMPORTING
                output = _wl_0147-id_recepcao.

            MESSAGE s012 WITH _wl_0147-chave_nff _wl_0147-id_recepcao DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          MODIFY me->at_nf FROM wl_nf INDEX _tabix.

        ENDLOOP.

      WHEN '3'. "DU-E/RUC
    ENDCASE.


    e_validou = abap_true.

  ENDMETHOD.
ENDCLASS.

class ZCL_FORNECEDORES definition
  public
  final
  create public .

public section.

  interfaces ZIF_PARCEIROS .

  data AT_LFA1 type LFA1 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FORNECEDORES IMPLEMENTATION.


  METHOD ZIF_PARCEIROS~CK_ATIVO.

    R_PARCEIRO = ME.

    SELECT SINGLE * INTO @DATA(WA_LFA1)
      FROM LFA1
     WHERE LIFNR EQ @ME->ZIF_PARCEIROS~PARCEIRO.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

    IF WA_LFA1-SPERR EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

    IF WA_LFA1-LOEVM EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

    IF WA_LFA1-NODEL EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FOR_BLOQ_CENTRAL-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_FOR_BLOQ_CENTRAL-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_FOR_BLOQ_CENTRAL-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_FOR_BLOQ_CENTRAL-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_ATIVO_EMPRESA.

    R_PARCEIRO = ME.

    CHECK I_EMPRESA IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(WA_LFB1)
      FROM LFB1
     WHERE LIFNR EQ @ME->ZIF_PARCEIROS~PARCEIRO
       AND BUKRS EQ @I_EMPRESA.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FOR_NAO_EMPRESA-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_FOR_NAO_EMPRESA-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_FOR_NAO_EMPRESA-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_FOR_NAO_EMPRESA-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

    IF WA_LFB1-SPERR EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA_EMP-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA_EMP-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA_EMP-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_FOR_NAO_CONTABILIZA_EMP-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

    IF WA_LFB1-LOEVM EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO_EMP-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO_EMP-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO_EMP-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_FOR_MARC_ELIMINACAO_EMP-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_EMISSOR_NF_E.

    R_PARCEIRO = ME.

    CHECK ME->ZIF_PARCEIROS~SCACD NE '9999'.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGNO
                          ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
        MSGID  = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).

  ENDMETHOD.


  method ZIF_PARCEIROS~CK_PARCEIRO_EMP_DIFERENTE.

    R_PARCEIRO = ME.

    DATA(_EMP_IGUAIS) = ABAP_FALSE.

    TRY.

      ME->ZIF_PARCEIROS~CK_PARCEIRO_INTERCOMPANY(
        EXPORTING
          I_EMPRESA     = I_EMPRESA    " Empresa
        IMPORTING
          E_J_1BBRANCH  = E_J_1BBRANCH " Local de negócios
      ).

      _EMP_IGUAIS = ABAP_TRUE.

    CATCH ZCX_PARCEIROS.
    ENDTRY.

    IF _EMP_IGUAIS EQ ABAP_TRUE.

      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).

    ENDIF.

  endmethod.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_INTERCOMPANY.

    CLEAR: E_J_1BBRANCH.

    R_PARCEIRO = ME.

    DATA(R_LOCAL_NEGOCIO) =
       ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO(
          EXPORTING
            I_PARTINER = ME->ZIF_PARCEIROS~PARCEIRO
          IMPORTING
            E_J_1BBRANCH = E_J_1BBRANCH
       ).

    IF R_LOCAL_NEGOCIO EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

    IF I_EMPRESA NE E_J_1BBRANCH-BUKRS.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_LOCAL_NEGOCIO.

    R_PARCEIRO = ME.

    DATA(R_LOCAL_NEGOCIO) = ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO( EXPORTING I_PARTINER = ME->ZIF_PARCEIROS~PARCEIRO IMPORTING E_J_1BBRANCH = E_J_1BBRANCH ).

    IF R_LOCAL_NEGOCIO EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_TERCEIRO.

    R_PARCEIRO = ME.

    DATA(R_LOCAL_NEGOCIO) = ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO( I_PARTINER = ME->ZIF_PARCEIROS~PARCEIRO ).

    IF R_LOCAL_NEGOCIO EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_RESTRICAO_EMBARGO.

    DATA: S_RESTRICAO    TYPE ZDE_PES_REQUEST_RESTRICAO,
          OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.

    R_PARCEIRO = ME.

    CREATE OBJECT OB_WEB_SERVICE.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~AUTENTICA_OPUS = ABAP_TRUE.

    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'RT' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'O' ).

    DATA(I_URI) = OB_WEB_SERVICE->GET_URI( ).

    CONCATENATE I_URI ME->ZIF_PARCEIROS~PARCEIRO INTO I_URI.

    CL_HTTP_CLIENT=>CREATE_BY_URL(
      EXPORTING
        URL                = CONV #( I_URI )
      IMPORTING
        CLIENT             =  DATA(VAR_HTTP)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4 ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ADD_TOKEN_OPUS_HTTP_CLIENTE(
      EXPORTING
        I_URL_DESTINO              = CONV #( I_URI )
        I_URL_TOKEN                = OB_WEB_SERVICE->AT_URL_TOKEN
      CHANGING
        I_HTTP                     = VAR_HTTP
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5 ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL METHOD VAR_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~request_method'
        VALUE = 'GET'.

    CALL METHOD VAR_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~server_protocol'
        VALUE = 'HTTP/1.1'.

    CALL METHOD VAR_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Content-Type'
        VALUE = 'application/json; UTF-8'.

    CALL METHOD VAR_HTTP->SEND
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL METHOD VAR_HTTP->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    DATA(JSON_RETORNO) = VAR_HTTP->RESPONSE->GET_CDATA( ).

    VAR_HTTP->CLOSE( ).

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = JSON_RETORNO
      CHANGING
        DATA = E_RESULTADO.

    IF  E_RESULTADO-BLOQUEADO EQ ABAP_TRUE.
      CONCATENATE 'Tipo:' E_RESULTADO-TIPO 'Motivo:' E_RESULTADO-MOTIVO INTO E_RESULTADO-MOTIVO SEPARATED BY SPACE.
      IF I_GERA_ERRO EQ ABAP_TRUE.
        ME->ZIF_PARCEIROS~GERA_ERRO_GERAL( I_TEXTO = CONV #( E_RESULTADO-MOTIVO ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_SERVICO_FRETE.

    R_INSTANCE = ME.

    CHECK ME->ZIF_PARCEIROS~DLGRP IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO-MSGNO )
        MSGID  = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_SERVICO_FRETE_RODO.

    R_INSTANCE = ME.

    CHECK ME->ZIF_PARCEIROS~DLGRP NE '0001'.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO_RODO-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO_RODO-MSGNO )
        MSGID  = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO_RODO-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_FORNECEDOR_SERVICO_RODO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_DADOS_BANCARIOS.

    R_PARCEIRO = ME.

    SELECT * INTO TABLE E_BANCOS
      FROM LFBK
     WHERE LIFNR EQ ME->ZIF_PARCEIROS~PARCEIRO.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_EMAIL.

    R_PARCEIRO = ME.

    DATA(LC_CHAVE) = SY-MANDT && ME->ZIF_PARCEIROS~PARCEIRO.

    SELECT ADDRNUMBER INTO @DATA(LC_ADDRNUMBER)
      FROM ADRV UP TO 1 ROWS
     WHERE APPL_TABLE EQ 'LFA1'
       AND APPL_FIELD EQ 'ADRNR'
       AND APPL_KEY   EQ @LC_CHAVE   "Mandt && Id Fornecedor
       AND OWNER      EQ @ABAP_TRUE
     ORDER BY APPL_TABLE.
    ENDSELECT.

    CHECK LC_ADDRNUMBER IS NOT INITIAL.

    SELECT SMTP_ADDR INTO E_MAIL UP TO 1 ROWS
      FROM ADR6
     WHERE ADDRNUMBER EQ LC_ADDRNUMBER.
    ENDSELECT.

  ENDMETHOD.


  method ZIF_PARCEIROS~GET_ENDERECO.

    DATA: LC_RETORNO   TYPE AD_RETCODE,
          IT_ADDVALUE  TYPE TABLE OF ADDR1_VAL,
          IT_ADDSELECT TYPE TABLE OF ADDR1_SEL.

    R_PARCEIRO = ME.

    DATA(LC_CHAVE) = SY-MANDT && ME->ZIF_PARCEIROS~PARCEIRO.

    SELECT ADDRNUMBER FROM ADRV UP TO 1 ROWS
              INTO @DATA(LC_ADDRNUMBER)
             WHERE APPL_TABLE EQ 'LFA1'
               AND APPL_FIELD EQ 'ADRNR'
               AND APPL_KEY   EQ @LC_CHAVE   "Mandt && Id Fornecedor
               AND OWNER      EQ @ABAP_TRUE
             ORDER BY APPL_TABLE.
      APPEND VALUE #( ADDRNUMBER = LC_ADDRNUMBER ) TO IT_ADDSELECT.
    ENDSELECT.

    CHECK IT_ADDSELECT[] IS NOT INITIAL.

    CALL FUNCTION 'ADDR_GET_ARRAY'
      IMPORTING
        RETURNCODE        = LC_RETORNO
      TABLES
        ADDRESS_SELECTION = IT_ADDSELECT
        ADDRESS_VALUE     = IT_ADDVALUE
      EXCEPTIONS
        PARAMETER_ERROR   = 1
        INTERNAL_ERROR    = 2
        OTHERS            = 3.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_PARCEIROS=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    READ TABLE IT_ADDVALUE INTO E_ENDERECO INDEX 1.

  endmethod.


  METHOD ZIF_PARCEIROS~GET_ID_PARCEIRO.

    R_PARCEIRO = ME.
    E_PARCEIRO = ME->ZIF_PARCEIROS~PARCEIRO.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_INSTANCE.

    IF ZIF_PARCEIROS~AT_PARCEIROS IS NOT BOUND.
      CREATE OBJECT ZIF_PARCEIROS~AT_PARCEIROS TYPE ZCL_FORNECEDORES.
      PARCEIRO = ZIF_PARCEIROS~AT_PARCEIROS.
    ELSE.
      PARCEIRO = ZIF_PARCEIROS~AT_PARCEIROS.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_NAME.

    R_PARCEIRO = ME.

    E_NAME = ME->ZIF_PARCEIROS~NOME..

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_OUTBOUND_MSG.

    DATA: E_OUTBOUND_FORNE TYPE ZFIE_VENDOR.

    DATA: WA_ADDSELECT  TYPE ADDR1_SEL,
          IT_ADDSELECT  LIKE STANDARD TABLE OF WA_ADDSELECT,
          IT_ADDVALUE   TYPE TABLE OF ADDR1_VAL,
          LC_APPL_TABLE	TYPE AD_APPLTAB,
          LC_APPL_FIELD	TYPE AD_APPLFLD,
          LC_RETORNO    TYPE AD_RETCODE,
          LC_ZFIE_BANK  TYPE ZFIE_BANK.


    R_PARCEIRO = ME.


    DATA(LC_CHAVE) = SY-MANDT && ME->AT_LFA1-LIFNR.

    SELECT APPL_TABLE APPL_FIELD ADDRNUMBER FROM ADRV UP TO 1 ROWS
              INTO (LC_APPL_TABLE, LC_APPL_FIELD, WA_ADDSELECT-ADDRNUMBER)
             WHERE APPL_TABLE EQ 'LFA1'
               AND APPL_FIELD EQ 'ADRNR'
               AND APPL_KEY   EQ LC_CHAVE   "Mandt && Id Fornecedor
               AND OWNER      EQ ABAP_TRUE
             ORDER BY APPL_TABLE.
      APPEND WA_ADDSELECT TO IT_ADDSELECT.
    ENDSELECT.

    CALL FUNCTION 'ADDR_GET_ARRAY'
      IMPORTING
        RETURNCODE        = LC_RETORNO
      TABLES
        ADDRESS_SELECTION = IT_ADDSELECT
        ADDRESS_VALUE     = IT_ADDVALUE
      EXCEPTIONS
        PARAMETER_ERROR   = 1
        INTERNAL_ERROR    = 2
        OTHERS            = 3.

    E_OUTBOUND_FORNE-DT_ATUALIZACAO    = SY-DATUM.
    E_OUTBOUND_FORNE-HR_ATUALIZACAO    = SY-UZEIT.
    E_OUTBOUND_FORNE-CD_TRANSACAO      = 'CARGUERO'.
    E_OUTBOUND_FORNE-ID_FORNECEDOR     = ME->AT_LFA1-LIFNR.
    E_OUTBOUND_FORNE-GR_CONTA          = ME->AT_LFA1-KTOKK.
    E_OUTBOUND_FORNE-TX_TRATAMENTO     = ME->AT_LFA1-ANRED.
    E_OUTBOUND_FORNE-DESCRICAO         = ME->AT_LFA1-NAME1.
    E_OUTBOUND_FORNE-TR_PESQUISA       = ME->AT_LFA1-SORTL.
    E_OUTBOUND_FORNE-CNPJ              = ME->AT_LFA1-STCD1.
    E_OUTBOUND_FORNE-FL_PESSOA_FISICA  = ME->AT_LFA1-STKZN.
    E_OUTBOUND_FORNE-CPF               = ME->AT_LFA1-STCD2.
    E_OUTBOUND_FORNE-INSESTADUAL       = ME->AT_LFA1-STCD3.
    E_OUTBOUND_FORNE-INSMUNICIPAL      = ME->AT_LFA1-STCD4.
    E_OUTBOUND_FORNE-STCD5             = ME->AT_LFA1-STCD5.
    E_OUTBOUND_FORNE-ID_PIS            = ME->AT_LFA1-STENR.
    E_OUTBOUND_FORNE-NU_CELULAR        = ME->AT_LFA1-TELF2.
    E_OUTBOUND_FORNE-BL_TODA_EMPRESA   = ME->AT_LFA1-SPERR.
    E_OUTBOUND_FORNE-EL_TODA_AREA      = ME->AT_LFA1-LOEVM.
    E_OUTBOUND_FORNE-EL_DADOS          = ME->AT_LFA1-NODEL.
    E_OUTBOUND_FORNE-BL_QUALIDADE      = ME->AT_LFA1-SPERQ.
    E_OUTBOUND_FORNE-RNTRC             = ME->AT_LFA1-BAHNS.
    E_OUTBOUND_FORNE-SETOR_INDUSTRIAL  = ME->AT_LFA1-BRSCH.
    E_OUTBOUND_FORNE-VAT               = ME->AT_LFA1-STCEG.
    E_OUTBOUND_FORNE-PAIS              = ME->AT_LFA1-LAND1.
    E_OUTBOUND_FORNE-DESCRICAO2        = ME->AT_LFA1-NAME2.
    E_OUTBOUND_FORNE-FNC_BLOQUEIO      = ME->AT_LFA1-SPERQ.
    E_OUTBOUND_FORNE-GBORT             = ME->AT_LFA1-GBORT.
    E_OUTBOUND_FORNE-GBDAT             = ME->AT_LFA1-GBDAT.
    E_OUTBOUND_FORNE-SEXKZ             = ME->AT_LFA1-SEXKZ.

    SELECT SMTP_ADDR INTO E_OUTBOUND_FORNE-EMAIL UP TO 1 ROWS
      FROM ADR6
     WHERE ADDRNUMBER EQ WA_ADDSELECT-ADDRNUMBER.
    ENDSELECT.

    READ TABLE IT_ADDVALUE INTO DATA(WA_ADDVALUE) INDEX 1.
    IF SY-SUBRC IS INITIAL.
      E_OUTBOUND_FORNE-RUA               = WA_ADDVALUE-STREET.
      E_OUTBOUND_FORNE-NUMERO            = WA_ADDVALUE-HOUSE_NUM1.
      E_OUTBOUND_FORNE-BAIRRO            = WA_ADDVALUE-CITY2.
      E_OUTBOUND_FORNE-CD_POSTAL         = WA_ADDVALUE-POST_CODE1.
      E_OUTBOUND_FORNE-CIDADE            = WA_ADDVALUE-CITY1.
      E_OUTBOUND_FORNE-ESTADO            = WA_ADDVALUE-REGION.
      E_OUTBOUND_FORNE-CX_POSTAL         = WA_ADDVALUE-PO_BOX.
      E_OUTBOUND_FORNE-CD_CAIXA_POSTAL   = WA_ADDVALUE-POST_CODE2.
      E_OUTBOUND_FORNE-IDIOMA            = WA_ADDVALUE-LANGU.
      E_OUTBOUND_FORNE-NU_TELEFONE       = WA_ADDVALUE-TEL_NUMBER.
      E_OUTBOUND_FORNE-NU_RAMAL          = WA_ADDVALUE-TEL_EXTENS.
      E_OUTBOUND_FORNE-NU_FAX            = WA_ADDVALUE-FAX_NUMBER.
      E_OUTBOUND_FORNE-NU_RAMAL_FAX      = WA_ADDVALUE-FAX_EXTENS.
      E_OUTBOUND_FORNE-MO_COMUNICACAO    = WA_ADDVALUE-DEFLT_COMM.
      E_OUTBOUND_FORNE-OB_ENDERECO       = WA_ADDVALUE-REMARK.
      E_OUTBOUND_FORNE-SALA_EDIFICIO     = WA_ADDVALUE-BUILDING.
      E_OUTBOUND_FORNE-SALA              = WA_ADDVALUE-ROOMNUMBER.
      E_OUTBOUND_FORNE-ANDAR             = WA_ADDVALUE-FLOOR.
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_LFB1)
      FROM LFB1
     WHERE LIFNR EQ @ME->AT_LFA1-LIFNR
       AND BUKRS EQ @I_BUKRS.

    IF SY-SUBRC IS INITIAL.
      E_OUTBOUND_FORNE-EMPRESA           = WA_LFB1-BUKRS.
      E_OUTBOUND_FORNE-CN_RECONCILIACAO  = WA_LFB1-AKONT.
      E_OUTBOUND_FORNE-CH_ORDENACAO      = WA_LFB1-ZUAWA.
      E_OUTBOUND_FORNE-GR_ADMINISTRACAO  = WA_LFB1-FDGRV.
      E_OUTBOUND_FORNE-ID_SIGAM          = WA_LFB1-ALTKN.
      E_OUTBOUND_FORNE-CH_PAGAMENTO      = WA_LFB1-ZTERM.
      E_OUTBOUND_FORNE-FR_PAGAMENTO      = WA_LFB1-ZWELS.
      E_OUTBOUND_FORNE-BL_EMPRESA        = WA_LFB1-SPERR.
      E_OUTBOUND_FORNE-EL_EMPRESA        = WA_LFB1-LOEVM.
      E_OUTBOUND_FORNE-EL_EMPRESA_DADOS  = WA_LFB1-NODEL.
    ENDIF.

*              SELECT * INTO TABLE @DATA(IT_LFBK)
*                FROM LFBK
*               WHERE LIFNR EQ @ME->AT_LFA1-LIFNR.
*
*              LOOP AT IT_LFBK INTO DATA(WA_LFBK).
*                LC_ZFIE_BANK-ID_CLIENTE        = WA_LFBK-LIFNR.
*                LC_ZFIE_BANK-DT_ATUALIZACAO    = E_OUTBOUND_FORNE-DT_ATUALIZACAO.
*                LC_ZFIE_BANK-HR_ATUALIZACAO    = E_OUTBOUND_FORNE-HR_ATUALIZACAO.
*                LC_ZFIE_BANK-CD_PAIS_BANCO     = WA_LFBK-BANKS.
*                LC_ZFIE_BANK-CH_BANCO          = WA_LFBK-BANKL.
*                LC_ZFIE_BANK-NU_CONTA_BANCARIA = WA_LFBK-BANKN.
*                LC_ZFIE_BANK-DV_AGENCIA        = WA_LFBK-BKONT.
*                LC_ZFIE_BANK-BANCO_PARCEIRO    = WA_LFBK-BVTYP.
*                LC_ZFIE_BANK-ST_ATUALIZACAO    = WA_LFBK-KZ.
*                APPEND LC_ZFIE_BANK TO E_OUTBOUND_BANCO.
*              ENDLOOP.

    SELECT * INTO TABLE @DATA(IT_KNVK)
      FROM KNVK
     WHERE LIFNR EQ @ME->AT_LFA1-LIFNR.

    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_KNVK INTO DATA(WA_KNVK) INDEX 1.
      E_OUTBOUND_FORNE-NOME       = WA_KNVK-NAMEV.
      E_OUTBOUND_FORNE-DS_NOME    = WA_KNVK-NAME1.
      E_OUTBOUND_FORNE-DP_CONTATO = WA_KNVK-ABTNR.
      E_OUTBOUND_FORNE-FN_CONTATO = WA_KNVK-PAFKT.
    ENDIF.

    E_MSG  = E_OUTBOUND_FORNE.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_REGIO.

    R_PARCEIRO = ME.
    E_REGIO = ME->ZIF_PARCEIROS~REGIO.
    E_LAND1 = ME->ZIF_PARCEIROS~LAND1.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_TIPO_PARCEIRO.

    R_PARCEIRO = ME.
    CASE ME->ZIF_PARCEIROS~TIPO_PESSOA.
      WHEN ABAP_FALSE.
        E_TIPO = ZIF_PARCEIROS=>ST_PESSOA_JURIDICA.
      WHEN ABAP_TRUE.
        E_TIPO = ZIF_PARCEIROS=>ST_PESSOA_FISICA.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_TXJCD.

    R_PARCEIRO = ME.
    E_TXJCD = ME->ZIF_PARCEIROS~TXJCD.

  ENDMETHOD.


  METHOD zif_parceiros~set_create_partiner.

    DATA: restados TYPE RANGE OF j_1btxjcd.

    DATA: lc_dados   TYPE zde_info_vendor_create.
    DATA: i_ktokk	   TYPE zde_ktok_range_t.
    DATA: lv_name1   TYPE but000-name_org1.   "*-155550-28.11.2024-JT-inicio

    CLEAR: e_outbound_forne,
           e_outbound_banco,
           e_outbound_banco[].

    r_if_parceiros = me.

    lc_dados = i_data.

    lc_dados-name1      = CONV #( zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = CONV #( lc_dados-name1 ) ) ) ).
    lc_dados-title_medi = CONV #( zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = CONV #( lc_dados-title_medi ) ) ) ).
    lc_dados-profs      = CONV #( zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = CONV #( lc_dados-profs ) ) ) ).
    lc_dados-sortl      = CONV #( zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = CONV #( lc_dados-sortl ) ) ) ).
    lc_dados-street     = CONV #( zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = CONV #( lc_dados-street ) ) ) ).

    DATA(lc_cx_alterar)      = abap_false.
    DATA(lc_cx_alterar_base) = abap_true.

    DATA(lc_transaction) = 'XK01'.
    IF lc_dados-lifnr IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_lab)
        FROM lfb1
       WHERE lifnr EQ @lc_dados-lifnr
         AND bukrs EQ @lc_dados-bukrs.

      IF sy-subrc IS INITIAL.
        lc_transaction = 'XK02'.
        lc_cx_alterar_base = abap_false.
      ELSE.
        lc_transaction = 'XK01'.
      ENDIF.
    ENDIF.
    "DATA(I_MODE) = ZIF_SHDB=>ST_TIPO_MODE_COM_TELA.
    DATA(i_mode) = zif_shdb=>st_tipo_mode_sem_tela_sem_debu.

    restados[] = VALUE #( sign = 'I' option = 'CP'
                          ( low = 'TO 17*' ) ( low = 'MA 21*' ) ( low = 'PI 22*' ) ( low = 'CE 23*' ) ( low = 'RN 24*' )
                          ( low = 'PE 26*' ) ( low = 'AL 27*' ) ( low = 'SE 28*' ) ( low = 'BA 29*' ) ( low = 'MG 31*' )
                          ( low = 'ES 32*' ) ( low = 'RJ 33*' ) ( low = 'SP 35*' ) ( low = 'PR 41*' ) ( low = 'SC 42*' )
                          ( low = 'RS 43*' ) ( low = 'MS 50*' ) ( low = 'MT 51*' ) ( low = 'GO 52*' ) ( low = 'DF 53*' )
                          ( low = 'AC 12*' ) ( low = 'AM 13*' ) ( low = 'RR 14*' ) ( low = 'AP 16*' ) ( low = 'PA 15*' )
                          ( low = 'RO 11*' ) ( low = 'PB 25*' ) ).

    IF lc_dados-txjcd IS INITIAL.
      SELECT *
        FROM j_1btreg_city
        INTO TABLE @DATA(it_regcity)
       WHERE country    EQ @lc_dados-country
         AND region     EQ @lc_dados-region
         AND pstcd_from LE @lc_dados-post_code1
         AND pstcd_to   GE @lc_dados-post_code1
         AND taxjurcode IN @restados.
      READ TABLE it_regcity INTO DATA(wa_regcity) INDEX 1.

      DATA(lc_city1) = lc_dados-city1.
      IF lc_city1 IS INITIAL.
        SELECT SINGLE * INTO @DATA(wa_j_1btxjurt)
          FROM j_1btxjurt
         WHERE spras      EQ @lc_dados-langu
           AND country    EQ @wa_regcity-country
           AND taxjurcode EQ @wa_regcity-taxjurcode.
        IF sy-subrc IS INITIAL.
          lc_city1 = wa_j_1btxjurt-text.
        ENDIF.
      ENDIF.
    ELSE.

      lc_city1 = lc_dados-city1.

      SELECT SINGLE * INTO @wa_j_1btxjurt
        FROM j_1btxjurt
       WHERE spras      EQ @lc_dados-langu
         AND country    EQ 'BR'
         AND taxjurcode EQ @lc_dados-txjcd.

      IF sy-subrc IS INITIAL.
        wa_regcity-taxjurcode = wa_j_1btxjurt-taxjurcode.
        wa_regcity-country    = wa_j_1btxjurt-country.
        lc_city1 = wa_j_1btxjurt-text.
      ELSE.
        RAISE EXCEPTION TYPE zcx_parceiros
          EXPORTING
            textid = VALUE #( msgid = zcx_parceiros=>zcx_sem_cidade_idioma-msgid
                              msgno = zcx_parceiros=>zcx_sem_cidade_idioma-msgno
                              attr1 = CONV #( lc_dados-txjcd )
                              attr2 = 'BR'
                              attr3 = CONV #( lc_dados-langu ) )
            msgid  = zcx_parceiros=>zcx_sem_cidade_idioma-msgid
            msgno  = zcx_parceiros=>zcx_sem_cidade_idioma-msgno
            msgty  = 'E'
            msgv1  = CONV #( lc_dados-txjcd )
            msgv2  = 'BR'
            msgv3  = CONV #( lc_dados-langu ).
      ENDIF.

    ENDIF.


    TRY .
        IF lc_dados-lifnr IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(wa_lfa1)
            FROM lfa1
           WHERE lifnr EQ @lc_dados-lifnr.

          IF sy-subrc IS INITIAL.
*-155550-28.11.2024-JT-inicio
            lv_name1 = wa_lfa1-name1.

            SELECT SINGLE but000~name_org1,but000~name_org2,but000~name_org3,but000~name_last
              INTO @DATA(_but000)
              FROM ibpsupplier
             INNER JOIN but000 ON but000~partner = ibpsupplier~businesspartner
             WHERE ibpsupplier~supplier = @lc_dados-lifnr.

            IF sy-subrc = 0.
              lv_name1 = |{ _but000-name_org1 }| & | | & |{ _but000-name_last }|.
              CONDENSE lv_name1.
              IF lv_name1 IS INITIAL.
                lv_name1 = wa_lfa1-name1.
              ENDIF.
            ENDIF.
*-155550-28.11.2024-JT-fim

            IF wa_lfa1-ktokk EQ 'ZFIC'.

              me->zif_parceiros~set_parceiro( i_parceiro = lc_dados-lifnr
                          )->get_id_parceiro( IMPORTING e_parceiro = DATA(e_parceiro)
                          )->get_outbound_msg( EXPORTING i_bukrs = lc_dados-bukrs IMPORTING e_msg = e_outbound_forne
                          ).

              e_outbound_forne-st_atualizacao = 'A'.

              e_msg = 'Sem Alteração no Fornecedor'.
              CONCATENATE e_msg e_parceiro INTO e_msg SEPARATED BY space.
              EXIT.

            ENDIF.

            DATA(ck_lfa1) = abap_true.
            SELECT SINGLE * INTO @DATA(wa_adrc) FROM adrc WHERE addrnumber EQ @wa_lfa1-adrnr.
            IF sy-subrc IS INITIAL.
              DATA(ck_adrc) = abap_true.
            ELSE.
              ck_adrc = abap_false.
            ENDIF.

            SELECT SINGLE * INTO @DATA(wa_adrct) FROM adrct  WHERE addrnumber EQ @wa_lfa1-adrnr.
            IF sy-subrc IS INITIAL.
              DATA(ck_adrct) = abap_true.
            ELSE.
              ck_adrct = abap_false.
            ENDIF.

            SELECT SINGLE * INTO @DATA(wa_adr6) FROM adr6  WHERE addrnumber EQ @wa_lfa1-adrnr.
            IF sy-subrc IS INITIAL.
              DATA(ck_adr6) = abap_true.
            ELSE.
              ck_adr6 = abap_false.
            ENDIF.

            SELECT SINGLE * INTO @DATA(wa_lfb1) FROM lfb1 WHERE lifnr EQ @wa_lfa1-lifnr AND bukrs EQ @lc_dados-bukrs.
            IF sy-subrc IS INITIAL.
              DATA(ck_lfb1) = abap_true.
            ELSE.
              ck_lfb1 = abap_false.
            ENDIF.

            SELECT SINGLE * INTO @DATA(wa_lfm1) FROM lfm1 WHERE lifnr EQ @wa_lfa1-lifnr AND ekorg EQ @lc_dados-ekorg.
            IF sy-subrc IS INITIAL.
              DATA(ck_lfm1) = abap_true.
            ELSE.
              ck_lfm1 = abap_false.
            ENDIF.

          ELSE.
            ck_lfa1 = abap_false.
            ck_adrc = abap_false.
          ENDIF.

          IF ck_lfa1 = abap_true .
            lc_dados-title_medi = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-title_medi ) i_data_2 = CONV #( wa_lfa1-anred ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
*           lc_dados-name1      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-name1 )      i_data_2 = CONV #( wa_lfa1-name1 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_dados-name1      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-name1 )      i_data_2 = CONV #( lv_name1 ) IMPORTING e_diferentes = lc_cx_alterar ).  "*-155550-28.11.2024-JT
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-tel_number = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-tel_number ) i_data_2 = CONV #( wa_lfa1-telf1 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-kunnr      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-kunnr )      i_data_2 = CONV #( wa_lfa1-kunnr ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stcd1      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stcd1 )      i_data_2 = CONV #( wa_lfa1-stcd1 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stcd2      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stcd2 )      i_data_2 = CONV #( wa_lfa1-stcd2 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stcd3      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stcd3 )      i_data_2 = CONV #( wa_lfa1-stcd3 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stcd4      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stcd4 )      i_data_2 = CONV #( wa_lfa1-stcd4 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stcd5      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stcd5 )      i_data_2 = CONV #( wa_lfa1-stcd5 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stkzn      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stkzn )      i_data_2 = CONV #( wa_lfa1-stkzn ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-stenr      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-stenr )      i_data_2 = CONV #( wa_lfa1-stenr ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-dlgrp      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-dlgrp )      i_data_2 = CONV #( wa_lfa1-dlgrp ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-bahns      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-bahns )      i_data_2 = CONV #( wa_lfa1-bahns ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-scacd      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-scacd )      i_data_2 = CONV #( wa_lfa1-scacd ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).

            "*-155550-28.11.2024-JT-inicio
*           IF wa_lfa1-gbdat IS NOT INITIAL.
*             CONCATENATE wa_lfa1-gbdat+6(2) wa_lfa1-gbdat+4(2) wa_lfa1-gbdat(4) INTO wa_lfa1-gbdat.
*           ENDIF.
            "*-155550-28.11.2024-JT-fim

            lc_dados-gbdat      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-gbdat )       i_data_2 = CONV #( wa_lfa1-gbdat ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).

            lc_dados-profs      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-profs )       i_data_2 = CONV #( wa_lfa1-profs ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).

            lc_dados-sexkz      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-sexkz )       i_data_2 = CONV #( wa_lfa1-sexkz ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).

            lc_dados-gbort      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-gbort )       i_data_2 = CONV #( wa_lfa1-gbort ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
          ENDIF.

          IF ck_adrc = abap_true.
            lc_dados-sortl        = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-sortl(9) )     i_data_2 = CONV #( wa_adrc-sort1(9)   ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-street       = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-street )       i_data_2 = CONV #( wa_adrc-street     ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-house_num1   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-house_num1 )   i_data_2 = CONV #( wa_adrc-house_num1 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            "LC_DADOS-CITY1        = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_DADOS-CITY1 )        I_DATA_2 = CONV #( WA_ADRC-CITY1      ) IMPORTING E_DIFERENTES = LC_CX_ALTERAR ).
            "LC_CX_ALTERAR_BASE  = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_CX_ALTERAR ) I_DATA_2 = CONV #( LC_CX_ALTERAR_BASE ) ).
            "LC_DADOS-CITY2        = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_DADOS-CITY2 )        I_DATA_2 = CONV #( WA_ADRC-CITY2      ) IMPORTING E_DIFERENTES = LC_CX_ALTERAR ).
            "LC_CX_ALTERAR_BASE  = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_CX_ALTERAR ) I_DATA_2 = CONV #( LC_CX_ALTERAR_BASE ) ).

            lc_dados-post_code1   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-post_code1 )   i_data_2 = CONV #( wa_adrc-post_code1 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-country      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-country )      i_data_2 = CONV #( wa_adrc-country ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-region       = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-region )       i_data_2 = CONV #( wa_adrc-region ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-po_box       = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-po_box )       i_data_2 = CONV #( wa_adrc-po_box ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-post_code2   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-post_code2 )   i_data_2 = CONV #( wa_adrc-post_code2 ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-langu        = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-langu )        i_data_2 = CONV #( wa_adrc-langu ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-deflt_comm   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-deflt_comm )   i_data_2 = CONV #( wa_adrc-deflt_comm ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            wa_regcity-taxjurcode = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( wa_regcity-taxjurcode ) i_data_2 = CONV #( wa_adrc-taxjurcode ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-tel_extens   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-tel_extens )   i_data_2 = CONV #( wa_adrc-tel_extens ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).

            "LC_DADOS-MOB_NUMBER   = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_DADOS-MOB_NUMBER )   I_DATA_2 = CONV #( WA_ADRC-TEL_NUMBER ) IMPORTING E_DIFERENTES = LC_CX_ALTERAR ).
            "LC_CX_ALTERAR_BASE  = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_CX_ALTERAR ) I_DATA_2 = CONV #( LC_CX_ALTERAR_BASE ) ).

            lc_dados-fax_number   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-fax_number )   i_data_2 = CONV #( wa_adrc-fax_number ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-fax_extens   = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-fax_extens )   i_data_2 = CONV #( wa_adrc-fax_extens ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
          ENDIF.

          IF ck_adrct EQ abap_true.
            "Não altera pela XK02 em SHDB Mode 'N'
            "LC_DADOS-REMARK     = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_DADOS-REMARK )    I_DATA_2 = CONV #( WA_ADRCT-REMARK ) IMPORTING E_DIFERENTES = LC_CX_ALTERAR ).
            "LC_CX_ALTERAR_BASE  = ZCL_STRING=>NVL( EXPORTING I_DATA_1 = CONV #( LC_CX_ALTERAR ) I_DATA_2 = CONV #( LC_CX_ALTERAR_BASE ) ).
          ENDIF.

          IF ck_adr6 EQ abap_true.
            lc_dados-smtp_addr  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-smtp_addr ) i_data_2 = CONV #( wa_adr6-smtp_addr ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
          ENDIF.

          IF ck_lfm1 EQ abap_true.
            lc_dados-waers      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-waers )     i_data_2 = CONV #( wa_lfm1-waers ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
          ENDIF.

          IF ck_lfb1 EQ abap_true.
            lc_dados-akont      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-akont )     i_data_2 = CONV #( wa_lfb1-akont ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-fdgrv      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-fdgrv )     i_data_2 = CONV #( wa_lfb1-fdgrv ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-zterm      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-zterm )     i_data_2 = CONV #( wa_lfb1-zterm ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-eikto      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-eikto )     i_data_2 = CONV #( wa_lfb1-eikto ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-zsabe      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-zsabe )     i_data_2 = CONV #( wa_lfb1-zsabe ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-zuawa      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-zuawa )     i_data_2 = CONV #( wa_lfb1-zuawa ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-zwels      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-zwels )     i_data_2 = CONV #( wa_lfb1-zwels ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-zgrup      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-zgrup )     i_data_2 = CONV #( wa_lfb1-zgrup ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
            lc_dados-frgrp      = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_dados-frgrp )     i_data_2 = CONV #( wa_lfb1-frgrp ) IMPORTING e_diferentes = lc_cx_alterar ).
            lc_cx_alterar_base  = zcl_string=>nvl( EXPORTING i_data_1 = CONV #( lc_cx_alterar ) i_data_2 = CONV #( lc_cx_alterar_base ) ).
          ENDIF.

        ENDIF.

        IF lc_cx_alterar_base EQ abap_true.

          CASE lc_transaction.
            WHEN 'XK01'.
              SELECT SINGLE bu_group
                INTO @DATA(vbu_group)
                FROM tbc001
                WHERE ktokk = @lc_dados-ktokk.
              e_msg = 'Criado Fornecedor'.
              DATA(lc_shdb) = zcl_shdb=>zif_shdb~get_instance(
                )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0100' dynbegin = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE'  fval = '/00'          )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-LIFNR' fval = lc_dados-lifnr )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-BUKRS' fval = lc_dados-bukrs )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-EKORG' fval = lc_dados-ekorg )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFM1-EKORG' fval = lc_dados-ekorg ) "ALRS
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-KTOKK' fval = lc_dados-ktokk )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BUS_JOEL_MAIN-CREATION_GROUP' fval = vbu_group ) "ALRS
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'USE_ZAV'     fval = abap_true      )
                ).
            WHEN 'XK02'.
              e_msg = 'Alterado Fornecedor'.
              lc_shdb = zcl_shdb=>zif_shdb~get_instance(
                )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K'  dynpro = '0101' dynbegin = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE'   fval = '/00'          )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-LIFNR'  fval = lc_dados-lifnr )

*---> 23/08/2023 - Ticket MG-6301 - GT - Início
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-LIFNR'   fval = lc_dados-lifnr )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-LIFNR'   fval = lc_dados-lifnr )
*<--- 23/08/2023 - Ticket MG-6301 - GT - Fim

                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-BUKRS'  fval = lc_dados-bukrs )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-EKORG'  fval = lc_dados-ekorg )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFM1-EKORG' fval = lc_dados-ekorg ) "ALRS
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0110'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0120'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0130'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'WRF02K-D0380' fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0210'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0215'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0220'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0610'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'RF02K-D0310'  fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'WRF02K-D0320' fval = 'X' )
                )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'USE_ZAV'      fval = abap_true )
                ).
          ENDCASE.

          lc_shdb->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0111'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'SZA1_D0100-TITLE_MEDI' fval = lc_dados-title_medi   ) "Forma de Tratamento.
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-NAME1'      fval = lc_dados-name1        ) "Razão Social
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-SORT1'      fval = lc_dados-sortl        ) "Nome de Pesquisa
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-STREET'     fval = lc_dados-street       ) "Rua
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-HOUSE_NUM1' fval = lc_dados-house_num1   ) "Número da Rua
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-CITY1'      fval = lc_city1              ) "Cidade
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-CITY2'      fval = lc_dados-city2        ) "Bairro
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-POST_CODE1' fval = lc_dados-post_code1   ) "Código Postal
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-POST_CODE2' fval = lc_dados-post_code2   ) "Código Postal da CxPostal
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-COUNTRY'    fval = lc_dados-country      ) "País
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-REGION'     fval = lc_dados-region       ) "Estado
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-PO_BOX'     fval = lc_dados-po_box       ) "Caixa Postal
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-LANGU'      fval = lc_dados-langu        ) "Idioma
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'SZA1_D0100-TEL_NUMBER' fval = lc_dados-tel_number   ) "Telefone 1
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'SZA1_D0100-TEL_EXTENS' fval = lc_dados-tel_extens   ) "Telefone 2
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'SZA1_D0100-MOB_NUMBER' fval = lc_dados-mob_number   ) "Mobile
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'SZA1_D0100-FAX_NUMBER' fval = lc_dados-fax_number   ) "FAX
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'SZA1_D0100-SMTP_ADDR'  fval = lc_dados-smtp_addr    ) "E-mail
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-DEFLT_COMM' fval = lc_dados-deflt_comm   ) "Meio comunicação (chave) (administração endereços central)
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-REMARK'     fval = lc_dados-remark       ) "Observações sobre endereço
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'ADDR1_DATA-TAXJURCODE' fval = wa_regcity-taxjurcode ) "Domicílio Fiscal
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE'            fval = '=VW' )
           ).

          lc_shdb->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0120'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-KUNNR' fval = lc_dados-kunnr  ) ). "Código Cliente

          IF lc_dados-stcd1 IS NOT INITIAL.
            lc_shdb->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STCD1' fval = lc_dados-stcd1  ) ). "CNPJ
          ENDIF.

          DATA: lc_data TYPE c LENGTH 10.
          IF lc_dados-gbdat IS NOT INITIAL.
            WRITE lc_dados-gbdat TO lc_data.
          ELSE.
            CLEAR: lc_data.
          ENDIF.

          IF lc_dados-stcd2 IS NOT INITIAL.
            lc_shdb->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STCD2' fval = lc_dados-stcd2  ) "CPF
            )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STKZN' fval = lc_dados-stkzn  ) "Indica que é Pessoa Física
            )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-GBDAT' fval = lc_data ) "Data de Nascimento
            ).

          ENDIF.

          lc_shdb->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STCD3' fval = lc_dados-stcd3  ) "Insc. Estadual
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STCD4' fval = lc_dados-stcd4  ) "RG
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STCD5' fval = lc_dados-stcd5  ) "
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-BAHNS' fval = lc_dados-bahns  ) "RNTRC
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-STENR' fval = lc_dados-stenr  ) "PIS
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-DLGRP' fval = lc_dados-dlgrp  ) "Grupo de esquemas de fornecedores de serviços
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-PROFS' fval = lc_dados-profs  ) "Nome da Mãe
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-SEXKZ' fval = lc_dados-sexkz  ) "Sexo
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-GBORT' fval = lc_dados-gbort  ) "UF - Orgão Emissor do Registro Geral (RG)
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-SCACD' fval = lc_dados-scacd  ) "Standard Carrier Access Code
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFA1-KRAUS' fval = lc_dados-stenr  ) "Nº da informação - Comentado CS2023000160 - Publicar PRD depois
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=VW' )
           ).

          "Pagamentos
          lc_shdb->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0130'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=VW' )

           "Pessoa de Contato
           )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0380'  dynbegin = 'X' )
           ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'KNVK-NAMEV(01)' FVAL = LC_DADOS-NAMEV )
           ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'KNVK-NAME1(01)' FVAL = LC_DADOS-NAME1 )
           ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'KNVK-ABTNR(01)' FVAL = LC_DADOS-ABTNR )
           ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'KNVK-PAFKT(01)' FVAL = LC_DADOS-PAFKT )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=VW' )

           "Administração de Contas Contábeis
           )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0210'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-AKONT'   fval = lc_dados-akont ) "Cta.de reconciliação na contabilidade geral
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-FDGRV'   fval = lc_dados-fdgrv ) "Grupo de administração de tesouraria
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-ZUAWA'   fval = lc_dados-zuawa ) "Chave ordenação
           ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'LFB1-ALTKN'   FVAL = LC_DADOS-ALTKN ) "Id no legado
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-FRGRP'   fval = lc_dados-frgrp ) "Grupo de liberação
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE'   fval = '=VW' )

           "Pagamento Contábeis
           )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0215'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-ZTERM'   fval = lc_dados-zterm ) "Chv cond pagamento
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-ZGRUP'   fval = lc_dados-zgrup ) "Chv de agrupamento
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-ZWELS'   fval = lc_dados-zwels ) "Forma de pagamento
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=VW' )

           "Correspondência Contabilidade
           )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0220'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-EIKTO'   fval = lc_dados-eikto ) "Nosso nº conta no fornecedor
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFB1-ZSABE'   fval = lc_dados-zsabe ) "Encarreg. Fornec.
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=VW' )

           "IRF - Contabilidade
           )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0610'  dynbegin = 'X' )
           ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'LFB1-QLAND'   FVAL = LC_DADOS-QLAND ) "Código do país relativo ao imposto retido na fonte
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=VW' )

           "Dados de Compra
           )->set_add_bdcdata( i_bdcdata = VALUE #( program = 'SAPMF02K' dynpro = '0310'  dynbegin = 'X' )
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'LFM1-WAERS' fval = lc_dados-waers  ) "Moeda do Pedido de Compra

           "Salva Cadastro
           )->set_add_bdcdata( i_bdcdata = VALUE #( fnam = 'BDC_OKCODE' fval = '=UPDA' )

           "Chama SHDB
           )->set_transaction( i_tcode   = CONV #( lc_transaction )
           )->set_mode( i_mode = i_mode
           )->set_executar(
           )->get_ck_existe_msg_erro( IMPORTING e_msg = DATA(e_msg_erro) e_msg_tab = DATA(e_msg_tab)
           ).

          IF lc_transaction EQ 'XK01'.
            "Me com Erro Verificar Criação
            "Não deu Erro na Execução do SHDB
            TRY .
*-CS2021000253-06.02.2025-#165752-JT-inicio
                IF i_forne_frete = abap_true.
                  IF lc_dados-stcd1 IS NOT INITIAL.
                    "i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZFFJ' ktokk_high = 'ZFFJ' ) ).
                    i_ktokk = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ). "Temporario
                  ELSE.
                    i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZFFF' ktokk_high = 'ZFFF' ) ).
                  ENDIF.
                ELSE.
*-CS2021000253-06.02.2025-#165752-JT-fim
                  IF lc_dados-ktokk EQ 'ZMOT'.
                    i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ).
                  ELSE.
                    i_ktokk = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ).
                  ENDIF.
                ENDIF.

                WAIT UP TO 3 SECONDS.

                me->zif_parceiros~set_parceiro_cnpj_cpf_ie(
                   EXPORTING
                     i_cnpj             = CONV #( lc_dados-stcd1 )
                     i_cpf              = CONV #( lc_dados-stcd2 )
                     i_insc_estatual    = lc_dados-stcd3
                     i_ktokk            = i_ktokk
                     i_forne_frete      = i_forne_frete                                                            "*-CS2021000253-06.02.2025-#165752-JT
                     i_agnorar_bloqueio = COND #( WHEN i_forne_frete = abap_true THEN abap_false ELSE abap_true  ) "*-CS2021000253-06.02.2025-#165752-JT
*                    i_agnorar_bloqueio = abap_true                                                                "*-CS2021000253-06.02.2025-#165752-JT
                  )->get_id_parceiro( IMPORTING e_parceiro = e_parceiro
                  )->get_outbound_msg( EXPORTING i_bukrs = lc_dados-bukrs IMPORTING e_msg = e_outbound_forne
                  ).

                e_outbound_forne-st_atualizacao = 'I'.

                CONCATENATE e_msg e_parceiro INTO e_msg SEPARATED BY space.
                EXIT.
              CATCH zcx_parceiros.
            ENDTRY.
          ENDIF.

          RAISE EXCEPTION TYPE zcx_shdb
            EXPORTING
              textid = VALUE #( msgid = e_msg_erro-msgid
                                msgno = CONV #( e_msg_erro-msgnr )
                                attr1 = CONV #( e_msg_erro-msgv1 )
                                attr2 = CONV #( e_msg_erro-msgv2 )
                                attr3 = CONV #( e_msg_erro-msgv3 )
                                attr4 = CONV #( e_msg_erro-msgv4 ) )
              msgid  = e_msg_erro-msgid
              msgno  = CONV #( e_msg_erro-msgnr )
              msgty  = 'E'
              msgv1  = e_msg_erro-msgv1(50)
              msgv2  = e_msg_erro-msgv2(50)
              msgv3  = e_msg_erro-msgv3(50)
              msgv4  = e_msg_erro-msgv4(50).
        ELSE.
*-CS2021000253-06.02.2025-#165752-JT-inicio
          IF i_forne_frete = abap_true.
            IF lc_dados-stcd1 IS NOT INITIAL.
              "i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZFFJ' ktokk_high = 'ZFFJ' ) ).
              i_ktokk = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ). "Temporario
            ELSE.
              i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZFFF' ktokk_high = 'ZFFF' ) ).
            ENDIF.
          ELSE.
            IF lc_dados-ktokk EQ 'ZMOT'.
              i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ).
            ELSE.
              i_ktokk = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ).
            ENDIF.
          ENDIF.
*-CS2021000253-06.02.2025-#165752-JT-fim

          me->zif_parceiros~set_parceiro_cnpj_cpf_ie( EXPORTING i_cnpj = CONV #( lc_dados-stcd1 )
                                                                i_cpf = CONV #( lc_dados-stcd2 )
                                                                i_insc_estatual = lc_dados-stcd3
                                                                i_ktokk            = i_ktokk                                                                  "*-CS2021000253-06.02.2025-#165752-JT
                                                                i_forne_frete      = i_forne_frete                                                            "*-CS2021000253-06.02.2025-#165752-JT
                                                                i_agnorar_bloqueio = COND #( WHEN i_forne_frete = abap_true THEN abap_false ELSE abap_true  ) "*-CS2021000253-06.02.2025-#165752-JT
            )->get_id_parceiro( IMPORTING e_parceiro = e_parceiro
            )->get_outbound_msg( EXPORTING i_bukrs = lc_dados-bukrs IMPORTING e_msg = e_outbound_forne
            ).

          e_outbound_forne-st_atualizacao = 'A'.

          e_msg = 'Sem Alteração no Fornecedor'.
          CONCATENATE e_msg e_parceiro INTO e_msg SEPARATED BY space.
        ENDIF.

      CATCH zcx_shdb INTO DATA(ex_shdb).
        IF zcx_shdb=>zcx_sem_msg_erro-msgid NE ex_shdb->msgid OR
           zcx_shdb=>zcx_sem_msg_erro-msgno NE ex_shdb->msgno.
          "ZCX_PARCEIROS=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = EX_SHDB->ZIF_ERROR~GET_MSG_ERRO( ) ).

          RAISE EXCEPTION TYPE zcx_shdb
            EXPORTING
              textid = VALUE #( msgid = ex_shdb->msgid
                                msgno = ex_shdb->msgno
                                attr1 = CONV #( ex_shdb->msgv1 )
                                attr2 = CONV #( ex_shdb->msgv2 )
                                attr3 = CONV #( ex_shdb->msgv3 )
                                attr4 = CONV #( ex_shdb->msgv4 ) )
              msgid  = ex_shdb->msgid
              msgno  = ex_shdb->msgno
              msgty  = 'E'
              msgv1  = ex_shdb->msgv1
              msgv2  = ex_shdb->msgv2
              msgv3  = ex_shdb->msgv3
              msgv4  = ex_shdb->msgv4.

        ELSE.

*-CS2021000253-06.02.2025-#165752-JT-inicio
          IF i_forne_frete = abap_true.
            IF lc_dados-stcd1 IS NOT INITIAL.
              "i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZFFJ' ktokk_high = 'ZFFJ' ) ).
              i_ktokk = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ). "Temporario
            ELSE.
              i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZFFF' ktokk_high = 'ZFFF' ) ).
            ENDIF.
          ELSE.
*-CS2021000253-06.02.2025-#165752-JT-fim
            IF lc_dados-ktokk EQ 'ZMOT'.
              i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ).
            ELSE.
              i_ktokk = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ).
            ENDIF.
          ENDIF.

          WAIT UP TO 1 SECONDS.

          TRY .
              "Não deu Erro na Execução do SHDB
              me->zif_parceiros~set_parceiro_cnpj_cpf_ie(
                 EXPORTING
                   i_cnpj             = CONV #( lc_dados-stcd1 )
                   i_cpf              = CONV #( lc_dados-stcd2 )
                   i_insc_estatual    = lc_dados-stcd3
                   i_ktokk            = i_ktokk
                   i_forne_frete      = i_forne_frete                                                            "*-CS2021000253-06.02.2025-#165752-JT
                   i_agnorar_bloqueio = COND #( WHEN i_forne_frete = abap_true THEN abap_false ELSE abap_true  ) "*-CS2021000253-06.02.2025-#165752-JT
*                  i_agnorar_bloqueio = abap_true                                                                "*-CS2021000253-06.02.2025-#165752-JT
                )->get_id_parceiro( IMPORTING e_parceiro = e_parceiro
                )->get_outbound_msg( EXPORTING i_bukrs = lc_dados-bukrs IMPORTING e_msg = e_outbound_forne
                ).

              e_outbound_forne-st_atualizacao = 'I'.

              CONCATENATE e_msg e_parceiro INTO e_msg SEPARATED BY space.

            CATCH zcx_parceiros INTO DATA(ex_parceiros).

              "Não Achou Parceiro retorna primeira msg do Retorno do SHDB
              IF e_msg_tab[] IS NOT INITIAL AND
                 ex_parceiros->msgid EQ zcx_parceiros=>zcx_nao_fornecedor-msgid AND
                 ex_parceiros->msgno EQ zcx_parceiros=>zcx_nao_fornecedor-msgno.
                READ TABLE e_msg_tab INTO e_msg_erro INDEX 1.
                MESSAGE ID e_msg_erro-msgid TYPE e_msg_erro-msgtyp NUMBER e_msg_erro-msgnr INTO DATA(mtext) WITH e_msg_erro-msgv1 e_msg_erro-msgv2 e_msg_erro-msgv3 e_msg_erro-msgv4.
                e_msg = mtext.
                zcx_error=>zif_error~gera_erro_geral( i_texto = mtext ).
              ELSE.
                zcx_parceiros=>zif_error~gera_erro_geral( i_texto = ex_parceiros->zif_error~get_msg_erro( ) ).
              ENDIF.

          ENDTRY.
        ENDIF.
    ENDTRY.

    "Validate
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( PROGRAM = 'SAPLZYVALIDATEGF04' DYNPRO = '9000' DYNBEGIN = 'X'  )
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'BDC_OKCODE' FVAL = '=RADIO' )
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'BTN_CPF'    FVAL = COND STRING( WHEN LC_DADOS-STKZN = ABAP_TRUE  THEN 'X' ELSE ' ' ) )
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'BTN_CNPJ'   FVAL = COND STRING( WHEN LC_DADOS-STKZN = ABAP_FALSE THEN 'X' ELSE ' ' ) )

    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( PROGRAM = 'SAPLZYVALIDATEGF04' DYNPRO = '9000' DYNBEGIN = 'X'  )
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'BDC_OKCODE' FVAL = '=OK' )
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'GV_DOCUM'   FVAL = COND STRING( WHEN LC_DADOS-STKZN = ABAP_TRUE THEN LC_DADOS-STCD2 ELSE ' ' ) )
    ")->SET_ADD_BDCDATA( I_BDCDATA = VALUE #( FNAM = 'GV_DATAPF'  FVAL = COND STRING( WHEN LC_DADOS-STKZN = ABAP_TRUE THEN LC_DADOS-GBDAT ELSE ' ' ) )

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~SET_PARCEIRO.

    R_PARCEIRO = ME.

    SELECT SINGLE * INTO @DATA(WA_LFA1)
      FROM LFA1
     WHERE LIFNR EQ @I_PARCEIRO.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGNO
                            ATTR1 = CONV #( I_PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_FORNECEDOR-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_PARCEIRO ).
    ENDIF.

    ME->ZIF_PARCEIROS~PARCEIRO = I_PARCEIRO.
    ME->ZIF_PARCEIROS~SET_PARCEIRO_TABELA( EXPORTING I_TABELA = WA_LFA1 ).

  ENDMETHOD.


  METHOD zif_parceiros~set_parceiro_cnpj_cpf_ie.

    DATA: rg_cnpj TYPE RANGE OF stcd1,
          rg_cpf  TYPE RANGE OF stcd2,
          rg_ie   TYPE RANGE OF stcd3.

    DATA: wa_lfa1 TYPE lfa1.

    r_parceiro = me.

    wa_lfa1-stcd1 = i_cnpj.
    wa_lfa1-stcd2 = i_cpf.
    wa_lfa1-stcd3 = i_insc_estatual.

    IF wa_lfa1-stcd3 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_lfa1-stcd3
        IMPORTING
          output = wa_lfa1-stcd3.
    ENDIF.

    IF i_cnpj IS INITIAL AND i_cpf IS INITIAL.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_informar_cnpj_cpf-msgid
                            msgno = zcx_parceiros=>zcx_informar_cnpj_cpf-msgno )
          msgid  = zcx_parceiros=>zcx_informar_cnpj_cpf-msgid
          msgno  = zcx_parceiros=>zcx_informar_cnpj_cpf-msgno
          msgty  = 'E'.
    ENDIF.

    IF i_cnpj IS NOT INITIAL.
      rg_cnpj = VALUE #( sign = 'I' option = 'EQ' ( high = wa_lfa1-stcd1 low = wa_lfa1-stcd1 ) ).
    ENDIF.

    IF i_cpf IS NOT INITIAL.
      rg_cpf  = VALUE #( sign = 'I' option = 'EQ' ( high = wa_lfa1-stcd2 low = wa_lfa1-stcd2 ) ).
    ENDIF.

    IF i_insc_estatual IS NOT INITIAL.
      DATA(rg_ie_upper) = i_insc_estatual.
      TRANSLATE rg_ie_upper TO UPPER CASE.
      CONDENSE rg_ie_upper NO-GAPS.
      IF rg_ie_upper NE 'ISENTO'.
        rg_ie = VALUE #( sign = 'I' option = 'CP' ( low = '*' && wa_lfa1-stcd3 && '*' ) ).
      ELSEIF i_ck_ie EQ abap_true.
        rg_ie = VALUE #( sign = 'I' option = 'EQ' ( low = wa_lfa1-stcd3 ) ).
      ENDIF.
    ELSEIF i_ck_ie EQ abap_true.
      rg_ie = VALUE #( sign = 'I' option = 'EQ' ( low = space ) ).
    ENDIF.

*-CS2021000253-06.02.2025-#165752-JT-inicio
    IF i_forne_frete = abap_true.
      IF rg_cpf[] IS NOT INITIAL.
        SELECT SINGLE * INTO wa_lfa1
          FROM lfa1
         WHERE stcd2 IN rg_cpf
           AND stcd3 IN rg_ie
           AND loevm NE abap_true
           AND sperr NE abap_true
           AND sperm NE abap_true
           AND ktokk IN i_ktokk.
      ELSE.
        SELECT SINGLE * INTO wa_lfa1
          FROM lfa1
         WHERE stcd1 IN rg_cnpj
           AND stcd3 IN rg_ie
           AND loevm NE abap_true
           AND sperr NE abap_true
           AND sperm NE abap_true
           AND ktokk IN i_ktokk.
      ENDIF.
    ELSE.
*-CS2021000253-06.02.2025-#165752-JT-fim
      SELECT SINGLE * INTO wa_lfa1
        FROM lfa1
       WHERE stcd1 IN rg_cnpj
         AND stcd2 IN rg_cpf
         AND stcd3 IN rg_ie
         AND loevm NE abap_true
         AND sperr NE abap_true
         AND sperm NE abap_true
         AND ktokk IN i_ktokk.
    ENDIF.  "*-CS2021000253-06.02.2025-#165752-JT

    IF sy-subrc IS NOT INITIAL.

      IF i_agnorar_bloqueio EQ abap_true.
        SELECT SINGLE * INTO wa_lfa1
          FROM lfa1
         WHERE stcd1 IN rg_cnpj
           AND stcd2 IN rg_cpf
           AND stcd3 IN rg_ie
           AND ktokk IN i_ktokk.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_parceiros
          EXPORTING
            textid = VALUE #( msgid = zcx_parceiros=>zcx_nao_fornecedor-msgid
                              msgno = zcx_parceiros=>zcx_nao_fornecedor-msgno
                              attr1 = CONV #( me->zif_parceiros~parceiro ) )
            msgid  = zcx_parceiros=>zcx_nao_fornecedor-msgid
            msgno  = zcx_parceiros=>zcx_nao_fornecedor-msgno
            msgty  = 'E'
            msgv1  = CONV #( me->zif_parceiros~parceiro ).
      ENDIF.

    ENDIF.

    me->zif_parceiros~set_parceiro_tabela( EXPORTING i_tabela = wa_lfa1 ).

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~SET_PARCEIRO_IE.

    DATA: LC_STCD3 TYPE LFA1-STCD3.

    DATA: LC_STCD3_AUX1 TYPE LFA1-STCD3,
          LC_STCD3_AUX2 TYPE LFA1-STCD3.

    R_PARCEIRO = ME.

    CHECK I_INSC_ESTATUAL IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_INSC_ESTATUAL
      IMPORTING
        OUTPUT = LC_STCD3_AUX1.

    LC_STCD3 = '%' && LC_STCD3_AUX1.

    SELECT *
      FROM LFA1 INTO TABLE @DATA(TG_LFA1)
     WHERE STCD3 LIKE @LC_STCD3.

    LOOP AT TG_LFA1 INTO DATA(WA_LFA1).
      DATA(_TABIX) = SY-TABIX.
      DATA(_DEL)   = ''.

      CLEAR: LC_STCD3_AUX2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_LFA1-STCD3
        IMPORTING
          OUTPUT = LC_STCD3_AUX2.

      IF LC_STCD3_AUX1 NE LC_STCD3_AUX2.
        _DEL = 'X'.
      ENDIF.

      TRY .
          ME->ZIF_PARCEIROS~SET_PARCEIRO_TABELA( EXPORTING I_TABELA = WA_LFA1
            )->CK_ATIVO( ).
        CATCH ZCX_PARCEIROS.
          _DEL = 'X'.
      ENDTRY.

      IF _DEL IS NOT INITIAL.
        DELETE TG_LFA1 INDEX _TABIX.
      ENDIF.
    ENDLOOP.

    READ TABLE TG_LFA1 INTO WA_LFA1 INDEX 1.

    IF ( SY-SUBRC NE 0 ) OR ( TG_LFA1[] IS INITIAL ).
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGNO
                            ATTR1 = CONV #( I_INSC_ESTATUAL ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_INSC_ESTATUAL ).
    ENDIF.

    ME->ZIF_PARCEIROS~SET_PARCEIRO_TABELA( EXPORTING I_TABELA = WA_LFA1 ).

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~SET_PARCEIRO_TABELA.

    DATA: WA_LFA1 TYPE LFA1.

    R_INSTANCE = ME.

    MOVE-CORRESPONDING I_TABELA TO WA_LFA1.

    ME->ZIF_PARCEIROS~PARCEIRO      = WA_LFA1-LIFNR.
    ME->ZIF_PARCEIROS~NOME          = WA_LFA1-NAME1.
    ME->ZIF_PARCEIROS~TIPO_PESSOA   = WA_LFA1-STKZN.
    ME->ZIF_PARCEIROS~INSC_ESTADUAL = WA_LFA1-STCD3.
    ME->ZIF_PARCEIROS~REGIO         = WA_LFA1-REGIO.
    ME->ZIF_PARCEIROS~LAND1         = WA_LFA1-LAND1.
    ME->ZIF_PARCEIROS~DLGRP         = WA_LFA1-DLGRP.
    ME->ZIF_PARCEIROS~SCACD         = WA_LFA1-SCACD.
    ME->ZIF_PARCEIROS~TXJCD         = WA_LFA1-TXJCD.

    ME->AT_LFA1 = WA_LFA1.

  ENDMETHOD.
ENDCLASS.

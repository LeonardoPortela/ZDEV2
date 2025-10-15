class ZCL_API_OPUS definition
  public
  final
  create public .

public section.

  class-methods GET_STATUS_ROMANEIO
    importing
      !I_TP_MOVIMENTO type STRING
      !I_NR_SAFRA type STRING
      !I_BRANCH type STRING
      !I_NR_ROMANEIO type STRING
    exporting
      !E_URL type STRING
      !E_CODE type I
      !E_REASON type STRING
      !E_STATUS type ZDE_STATUS_CARGA
      !E_BLOQUEADO type STRING
      !E_MENSAGEM_BLOQUEIO type STRING
    returning
      value(R_RESULTADO) type STRING
    raising
      ZCX_ERROR .
  class-methods GET_SERIES_SIGAM
    importing
      !I_DESCR_RESUMIDA type STRING
    exporting
      !E_URL type STRING
      !E_CODE type I
      !E_REASON type STRING
      !E_SERIES type ZDE_API_OPUS_0003_T
    returning
      value(R_RESULTADO) type STRING
    raising
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_OPUS IMPLEMENTATION.


  METHOD GET_SERIES_SIGAM.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE,
          LC_MENSAGEM    TYPE ZDE_API_OPUS_0002.

    TRY .
        CREATE OBJECT OB_WEB_SERVICE.
        OB_WEB_SERVICE->ZIF_WEBSERVICE~AUTENTICA_OPUS = ABAP_TRUE.
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'BA' ).
        OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'O' ).
        E_URL = OB_WEB_SERVICE->GET_URI(  ).
        DATA(COMP_URL) = '/romaneiocompleto/sap/serie/' && ZCL_STRING=>TRIM( I_STR = I_DESCR_RESUMIDA ).
        CONCATENATE E_URL COMP_URL INTO E_URL.
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( I_URL = CONV #( E_URL ) ).

      CATCH ZCX_WEBSERVICE.

        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
                              MSGNO = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
                              ATTR1 = COMP_URL
                              ATTR2 = I_DESCR_RESUMIDA )
            MSGID  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
            MSGNO  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( I_DESCR_RESUMIDA ).

    ENDTRY.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

    VAR_HTTP->REQUEST->SET_METHOD( METHOD = IF_HTTP_ENTITY=>CO_REQUEST_METHOD_GET ).

    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
      EXPORTING
        I_HTTP                     = VAR_HTTP
      IMPORTING
        E_CODE                     = E_CODE
        E_REASON                   = E_REASON
      RECEIVING
        E_RESULTADO                = R_RESULTADO
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5
    ).

    IF SY-SUBRC IS NOT INITIAL.
      CLEAR: OB_WEB_SERVICE.
      RAISE EXCEPTION TYPE ZCX_ERROR
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

    CLEAR: OB_WEB_SERVICE.

    IF E_CODE NE 200.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = R_RESULTADO
        CHANGING
          DATA = LC_MENSAGEM.

      SY-MSGV1 = E_CODE.
      SY-MSGV2 = LC_MENSAGEM-MESSAGE.
      SY-MSGV3 = E_REASON.

      RAISE EXCEPTION TYPE ZCX_ERROR
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3.

    ELSE.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = R_RESULTADO
        CHANGING
          DATA = E_SERIES.

    ENDIF.

  ENDMETHOD.


  METHOD GET_STATUS_ROMANEIO.

    TYPES BEGIN OF TY_MESAGEM.
    TYPES: MESSAGE TYPE STRING.
    TYPES END OF TY_MESAGEM.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE,
          LC_RETORNO     TYPE ZDE_API_OPUS_0001,
          LC_MENSAGEM    TYPE TY_MESAGEM.

    CLEAR: E_CODE, E_REASON, R_RESULTADO, E_URL,
           E_STATUS,
           E_BLOQUEADO,
           E_MENSAGEM_BLOQUEIO.

    TRY .
        CREATE OBJECT OB_WEB_SERVICE.
        OB_WEB_SERVICE->ZIF_WEBSERVICE~AUTENTICA_OPUS = ABAP_TRUE.
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'BA' ).
        OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'O' ).
        E_URL = OB_WEB_SERVICE->GET_URI(  ).
        DATA(COMP_URL) = '/romaneiocompleto/sap/status/' &&
                            COND STRING( WHEN I_TP_MOVIMENTO EQ 'E' THEN 'entrada' WHEN I_TP_MOVIMENTO EQ 'S' THEN 'saida' ).

        CONCATENATE E_URL COMP_URL '/' I_NR_SAFRA '/' I_BRANCH '/' I_NR_ROMANEIO INTO E_URL.
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( I_URL = CONV #( E_URL ) ).

      CATCH ZCX_WEBSERVICE.
        CLEAR OB_WEB_SERVICE.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
                              MSGNO = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
                              ATTR1 = COMP_URL
                              ATTR2 = I_NR_SAFRA
                              ATTR3 = I_BRANCH
                              ATTR4 = I_NR_ROMANEIO )
            MSGID  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
            MSGNO  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( COMP_URL )
            MSGV2  = CONV #( I_NR_SAFRA )
            MSGV3  = CONV #( I_BRANCH )
            MSGV4  = CONV #( I_NR_ROMANEIO ).

    ENDTRY.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

    VAR_HTTP->REQUEST->SET_METHOD( METHOD = IF_HTTP_ENTITY=>CO_REQUEST_METHOD_GET ).

    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
      EXPORTING
        I_HTTP                     = VAR_HTTP
      IMPORTING
        E_CODE                     = E_CODE
        E_REASON                   = E_REASON
      RECEIVING
        E_RESULTADO                = R_RESULTADO
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5
    ).

    IF SY-SUBRC IS NOT INITIAL.
      CLEAR: OB_WEB_SERVICE.
      RAISE EXCEPTION TYPE ZCX_ERROR
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
    CLEAR: OB_WEB_SERVICE.

    IF E_CODE NE 200.

      CASE E_CODE.
        WHEN 500.

          CALL METHOD /UI2/CL_JSON=>DESERIALIZE
            EXPORTING
              JSON = R_RESULTADO
            CHANGING
              DATA = LC_MENSAGEM.

          IF LC_MENSAGEM-MESSAGE EQ 'Object reference not set to an instance of an object.'.
            E_STATUS = 'CA'.
          ELSE.
            RAISE EXCEPTION TYPE ZCX_ERROR
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
                                  MSGNO = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
                                  ATTR1 = I_NR_SAFRA
                                  ATTR2 = I_BRANCH
                                  ATTR3 = I_NR_ROMANEIO
                                  ATTR4 = E_URL )
                MSGID  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
                MSGNO  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
                MSGTY  = 'E'
                MSGV1  = CONV #( I_NR_SAFRA )
                MSGV2  = CONV #( I_BRANCH )
                MSGV3  = CONV #( I_NR_ROMANEIO )
                MSGV4  = CONV #( E_URL ).
          ENDIF.

        WHEN 204.

          RAISE EXCEPTION TYPE ZCX_ERROR
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_ROMANEIO=>ZCX_ERRO_ROM_NAO_ENCONTRADO-MSGID
                                MSGNO = ZCX_ROMANEIO=>ZCX_ERRO_ROM_NAO_ENCONTRADO-MSGNO
                                ATTR1 = I_NR_SAFRA
                                ATTR2 = I_BRANCH
                                ATTR3 = I_NR_ROMANEIO )
              MSGID  = ZCX_ROMANEIO=>ZCX_ERRO_ROM_NAO_ENCONTRADO-MSGID
              MSGNO  = ZCX_ROMANEIO=>ZCX_ERRO_ROM_NAO_ENCONTRADO-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( I_NR_SAFRA )
              MSGV2  = CONV #( I_BRANCH )
              MSGV3  = CONV #( I_NR_ROMANEIO ).

      ENDCASE.

    ELSE.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = R_RESULTADO
        CHANGING
          DATA = LC_RETORNO.

      IF LC_RETORNO-SEQPLAROMANEIO IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
                              MSGNO = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
                              ATTR1 = E_URL
                              ATTR2 = I_NR_SAFRA
                              ATTR3 = I_BRANCH
                              ATTR4 = I_NR_ROMANEIO )
            MSGID  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGID
            MSGNO  = ZCX_ROMANEIO=>ZCX_ERRO_CONSULTA_OPUS-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( E_URL )
            MSGV2  = CONV #( I_NR_SAFRA )
            MSGV3  = CONV #( I_BRANCH )
            MSGV4  = CONV #( I_NR_ROMANEIO ).
      ENDIF.

      E_STATUS = LC_RETORNO-STATUS.
      E_BLOQUEADO = LC_RETORNO-BLOQUEIOSIGAM-BLOQUEADO.
      E_MENSAGEM_BLOQUEIO = LC_RETORNO-BLOQUEIOSIGAM-MENSAGEM.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

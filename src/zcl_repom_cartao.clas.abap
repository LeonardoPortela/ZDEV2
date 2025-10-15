class ZCL_REPOM_CARTAO definition
  public
  inheriting from ZCL_REPOM
  final
  create public .

public section.

  interfaces ZIF_REPOM .

  aliases SET_BRANCH
    for ZIF_REPOM~SET_BRANCH .
  aliases SET_BUKRS
    for ZIF_REPOM~SET_BUKRS .

  data CK_SALVAR_XML_LOCAL type CHAR01 .

  methods CONSTRUCTOR
    importing
      !I_NR_CARTAO type ZDE_REPOM_CARTAO_PED optional .
  methods SET_NR_CARTAO_PEDAGIO
    importing
      !I_NR_CARTAO type ZDE_REPOM_CARTAO_PED .
  methods CONSULTAR_VALIDADE_CARTAO
    exporting
      !E_ERROS type ZDE_REPOM_ERROS_T
    returning
      value(I_VALIDOU) type CHAR01
    exceptions
      SERVICO_NAO_ENCONTRADO
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT
      ERRO .
  methods LIMPAR_CARTAO .
protected section.
private section.

  aliases BRANCH
    for ZIF_REPOM~BRANCH .
  aliases BUKRS
    for ZIF_REPOM~BUKRS .

  data NR_CARTAO type ZDE_REPOM_CARTAO_PED .
ENDCLASS.



CLASS ZCL_REPOM_CARTAO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    CALL METHOD SUPER->CONSTRUCTOR.

    ME->LIMPAR_CARTAO( ).

    IF I_NR_CARTAO IS   NOT INITIAL.
      ME->SET_NR_CARTAO_PEDAGIO( EXPORTING I_NR_CARTAO = I_NR_CARTAO ).
    ENDIF.

  ENDMETHOD.


  METHOD CONSULTAR_VALIDADE_CARTAO.

    DATA: LC_XML_AUTENTICA TYPE STRING,
          I_NAME_FILE      TYPE STRING,
          LC_EXCEPTION     TYPE REF TO ZCX_WEBSERVICE,
          LC_MSG           TYPE STRING,
          XML_INPUT        TYPE STRING,
          XML_RETORNO      TYPE STRING,
          VAR_HTTP         TYPE REF TO IF_HTTP_CLIENT,
          LC_ERRO          TYPE ZDE_REPOM_ERROS.

    DATA: LC_XML_RET   TYPE REF TO CL_XML_DOCUMENT,
          LC_XML_RET_E TYPE REF TO CL_XML_DOCUMENT,
          LC_TAMANHO   TYPE I,
          LC_XML_NODE  TYPE REF TO IF_IXML_NODE,
          LC_ITERATOR  TYPE REF TO IF_IXML_NODE_ITERATOR,
          LC_STRING    TYPE STRING.

    CHECK ME->NR_CARTAO IS NOT INITIAL.

    I_VALIDOU = ABAP_FALSE.

    ME->GET_AUTENTICA( EXPORTING I_OBJETO = ME RECEIVING XML = LC_XML_AUTENTICA ).

    CALL METHOD ME->LIMPAR.
    ME->CTNA(   EXPORTING TEXTO = '<?xml version="1.0" encoding="utf-8"?>' ).
    ME->CTNA(   EXPORTING TEXTO = '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' ).
    ME->CTNA(   EXPORTING TEXTO = '<soap12:Body>' ).
    ME->CTNAB(  EXPORTING TAG  = 'ValidaCartaoVPR xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao"' ).
    ME->CTNA(   EXPORTING TEXTO = LC_XML_AUTENTICA ).
    ME->CTNAV(  EXPORTING TAG = 'strCartao' VALOR = ME->NR_CARTAO ).
    ME->CTNAFE( EXPORTING TAG = 'strXmlErr' ).
    ME->CTNFE(  EXPORTING TAG = 'ValidaCartaoVPR' ).
    ME->CTNA(   EXPORTING TEXTO = '</soap12:Body>' ).
    ME->CTNA(   EXPORTING TEXTO = '</soap12:Envelope>' ).

    IF ME->CK_SALVAR_XML_LOCAL EQ ABAP_TRUE.
      CONCATENATE 'C:\Maggi\REPOM\ValidaCartaoVPR' ME->NR_CARTAO '.xml' INTO I_NAME_FILE.
      CALL METHOD ME->SALVA_XML
        EXPORTING
          I_NAME_FILE = I_NAME_FILE.
    ENDIF.

    TRY .
        ME->SET_SERVICO( I_SERVICO = 'R1' ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        LC_MSG = LC_EXCEPTION->GET_TEXT( ).
        MESSAGE E008 WITH 'R1 (ValidaCartaoVPR ME->SET_SERVICO)' RAISING SERVICO_NAO_ENCONTRADO.
    ENDTRY.

    ME->SET_TIPO( I_TIPO = 'I' ).

    TRY .
        VAR_HTTP = ME->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        LC_MSG = LC_EXCEPTION->GET_TEXT( ).
        MESSAGE E008 WITH 'R1 (ValidaCartaoVPR ME->URL)' RAISING SERVICO_NAO_ENCONTRADO.
    ENDTRY.

    ME->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

    CALL METHOD ME->GET_XML
      IMPORTING
        E_XML_TEXTO = XML_INPUT.

    CALL METHOD ME->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = XML_INPUT
      RECEIVING
        E_RESULTADO                = XML_RETORNO
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E009 RAISING HTTP_COMMUNICATION_FAILURE.
      WHEN 2.
        MESSAGE E010 RAISING HTTP_INVALID_STATE.
      WHEN 3.
        MESSAGE E011 RAISING HTTP_PROCESSING_FAILED.
      WHEN 4.
        MESSAGE E012 RAISING HTTP_INVALID_TIMEOUT.
      WHEN 5.
        MESSAGE E013 RAISING ERRO.
    ENDCASE.

    IF ME->CK_SALVAR_XML_LOCAL EQ ABAP_TRUE.
      CONCATENATE 'C:\Maggi\REPOM\ValidaCartaoVPRRet' ME->NR_CARTAO '.xml' INTO I_NAME_FILE.
      CALL METHOD ME->SALVA_XML
        EXPORTING
          I_NAME_FILE = I_NAME_FILE
          I_XML       = XML_RETORNO.
    ENDIF.

    CREATE OBJECT LC_XML_RET.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = XML_RETORNO
      RECEIVING
        RETCODE = LC_TAMANHO.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'ValidaCartaoVPRResult'
      RECEIVING
        NODE = LC_XML_NODE.

    IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).
      LC_STRING = LC_XML_NODE->GET_VALUE( ).
      TRANSLATE LC_STRING TO UPPER CASE.
      IF LC_STRING EQ 'TRUE'.
        MESSAGE S019 WITH ME->NR_CARTAO.
        I_VALIDOU = ABAP_TRUE.
      ELSE.
        CALL METHOD LC_XML_RET->FIND_NODE
          EXPORTING
            NAME = 'strXmlErr'
          RECEIVING
            NODE = LC_XML_NODE.

        IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).

          XML_RETORNO = LC_XML_NODE->GET_VALUE( ).

          CREATE OBJECT LC_XML_RET_E.

          CALL METHOD LC_XML_RET_E->PARSE_STRING
            EXPORTING
              STREAM  = XML_RETORNO
            RECEIVING
              RETCODE = LC_TAMANHO.

          CALL METHOD LC_XML_RET_E->FIND_NODE
            EXPORTING
              NAME = 'erros'
            RECEIVING
              NODE = LC_XML_NODE.

          IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).

            LC_ITERATOR = LC_XML_NODE->CREATE_ITERATOR( ).
            LC_XML_NODE = LC_ITERATOR->GET_NEXT( ).

            WHILE NOT LC_XML_NODE IS INITIAL.
              CASE LC_XML_NODE->GET_TYPE( ).
                WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.
                  LC_STRING = LC_XML_NODE->GET_NAME( ).
                  IF LC_STRING EQ 'erro_codigo'.
                    LC_STRING = LC_XML_NODE->GET_VALUE( ).
                    LC_ERRO-ERRO_CODIGO = LC_STRING.
                  ELSEIF LC_STRING EQ 'erro_descricao'.
                    LC_STRING = LC_XML_NODE->GET_VALUE( ).
                    LC_ERRO-ERRO_DESCRICAO = LC_STRING.
                    APPEND LC_ERRO TO E_ERROS.
                  ENDIF.
              ENDCASE.
              LC_XML_NODE = LC_ITERATOR->GET_NEXT( ).
            ENDWHILE.
          ENDIF.

        ELSE.
          MESSAGE E014 RAISING ERRO.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD LIMPAR_CARTAO.

    CLEAR: ME->NR_CARTAO,
           ME->BUKRS,
           ME->BRANCH.

  ENDMETHOD.


  METHOD SET_NR_CARTAO_PEDAGIO.
    ME->NR_CARTAO = I_NR_CARTAO.
  ENDMETHOD.


  method ZIF_REPOM~SET_BRANCH.
    ME->BRANCH = I_BRANCH.
  endmethod.


  METHOD ZIF_REPOM~SET_BUKRS.
    ME->BUKRS = I_BUKRS.
  ENDMETHOD.
ENDCLASS.

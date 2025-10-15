class ZCL_REPOM_ROTA definition
  public
  inheriting from ZCL_REPOM
  final
  create public .

public section.

  interfaces ZIF_REPOM .
  interfaces ZIF_CADASTRO .

  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases SET_BRANCH
    for ZIF_REPOM~SET_BRANCH .
  aliases SET_BUKRS
    for ZIF_REPOM~SET_BUKRS .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  data CK_SALVAR_XML_LOCAL type CHAR01 .

  methods CONSTRUCTOR
    importing
      !I_ID_ROTA type ZDE_ID_ROTA_CLIENTE optional .
  methods SET_ID_ROTA
    importing
      !I_ID_ROTA type ZDE_ID_ROTA_CLIENTE .
  methods SET_CD_PAIS
    importing
      !I_CD_PAIS type LAND1 .
  methods SET_CD_CID_ORIGEM
    importing
      !I_CD_CID_ORIGEM type ZDE_CIDADE_ORIGEM .
  methods SET_CD_CID_DESTINO
    importing
      !I_CD_CID_DESTINO type ZDE_CIDADE_DESTINO .
  methods SET_TP_PROC_TRANSP
    importing
      !I_TP_PROC_TRANSP type ZDE_TP_PROC_TRANSP_REPOM .
  methods SET_TP_IDA_VOLTA
    importing
      !I_TP_IDA_VOLTA type ZDE_TIPO_PERC .
  methods SET_DS_OBSERVACAO
    importing
      !I_DS_OBSERVACAO type ZDE_OBS_245 .
  methods SET_TP_ST_ROTA
    importing
      !I_TP_ST_ROTA type ZDE_ST_PED_REPOM .
  methods SET_PERCURSO
    importing
      !I_ZLEST0122 type ZLEST0122 .
  methods SOLICITAR_ROTA
    exporting
      !E_ERROS type ZDE_REPOM_ERROS_T
    returning
      value(I_SOLICITOU) type CHAR01
    exceptions
      SERVICO_NAO_ENCONTRADO
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT
      ERRO .
  methods CONSULTAR_ROTA
    exporting
      !E_ERROS type ZDE_REPOM_ERROS_T
    returning
      value(I_CONSULTOU) type CHAR01
    exceptions
      SERVICO_NAO_ENCONTRADO
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT
      ERRO .
  methods GET_PERCURSOS
    exporting
      !E_PERCURSOS type ZDE_ZLEST0122_T .
  methods DESATIVAR_ROTA
    returning
      value(I_DESATIVOU) type CHAR01 .
  methods ATIVAR_ROTA
    returning
      value(I_ATIVOU) type CHAR01 .
  methods EXCLUIR_PERCURSO
    importing
      !I_ID_ROTA_REPOM type ZDE_ID_ROTA_REPOM
      !I_ID_PERCURSO_REPOM type ZDE_ID_PERCURSO_REPOM .
  methods EDITAR_PERCURSO
    importing
      !I_ZLEST0122_ANT type ZLEST0122
      !I_ZLEST0122 type ZLEST0122 .
  methods INCLUIR_PERCURSO
    importing
      !I_ZLEST0122 type ZLEST0122 .
  methods SET_CK_ATIVO
    importing
      !I_CK_ATIVO type ZDE_REGISTRO_ATIVO .
  PROTECTED SECTION.
private section.

  aliases BRANCH
    for ZIF_REPOM~BRANCH .
  aliases BUKRS
    for ZIF_REPOM~BUKRS .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .

  data ID_ROTA type ZDE_ID_ROTA_CLIENTE .
  data CD_PAIS type LAND1 .
  data CD_CID_ORIGEM type ZDE_CIDADE_ORIGEM .
  data CD_CID_DESTINO type ZDE_CIDADE_DESTINO .
  data TP_PROC_TRANSP type ZDE_TP_PROC_TRANSP_REPOM .
  data TP_IDA_VOLTA type ZDE_TIPO_PERC .
  data DS_OBSERVACAO type ZDE_OBS_245 .
  data CK_ALTERADO type CHAR01 .
  data PERCURSOS_REPOM type ZDE_ZLEST0122_T .
  data TP_ST_ROTA type ZDE_ST_PED_REPOM .
  data CK_ATIVO type ZDE_REGISTRO_ATIVO .
  data US_ULTIMO_AJUSTE type ZDE_US_ULTIMO_AJUSTE .
  data DT_ULTIMO_AJUSTE type SYST_DATUM .
  data HR_ULTIMO_AJUSTE type SYST_UZEIT .

  methods SET_US_ULTIMO_AJUSTE
    importing
      !I_US_ULTIMO_AJUSTE type ZDE_US_ULTIMO_AJUSTE .
  methods SET_DT_ULTIMO_AJUSTE
    importing
      !I_DT_ULTIMO_AJUSTE type SYST_DATUM .
  methods SET_HR_ULTIMO_AJUSTE
    importing
      !I_HR_ULTIMO_AJUSTE type SYST_UZEIT .
ENDCLASS.



CLASS ZCL_REPOM_ROTA IMPLEMENTATION.


  METHOD ATIVAR_ROTA.
    I_ATIVOU = ABAP_FALSE.

    AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
                                    ID 'ZREPOMATV' FIELD '06'.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S063 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CHECK ME->CK_ATIVO EQ ABAP_FALSE.
    ME->SET_CK_ATIVO( EXPORTING I_CK_ATIVO = ABAP_TRUE ).
    IF ME->GRAVAR( ) EQ ABAP_TRUE.
      MESSAGE S038 WITH ME->ID_ROTA.
      I_ATIVOU = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    CALL METHOD SUPER->CONSTRUCTOR.

    ME->LIMPAR_REGISTRO( ).

    IF I_ID_ROTA IS NOT INITIAL.
      ME->SET_REGISTRO( EXPORTING I_ID_REGISTRO = I_ID_ROTA ).
    ENDIF.

    ME->CK_SALVAR_XML_LOCAL = ABAP_FALSE.

  ENDMETHOD.


  METHOD CONSULTAR_ROTA.

    DATA: I_NAME_FILE TYPE STRING.

    CHECK ME->CK_ALTERADO = ABAP_FALSE.

    DATA: VAR_HTTP     TYPE REF TO IF_HTTP_CLIENT,
          LC_EXCEPTION TYPE REF TO ZCX_WEBSERVICE,
          LC_MSG       TYPE STRING,
          XML_RETORNO  TYPE STRING,
          XML_INPUT    TYPE STRING.

    DATA: LC_XML_RET   TYPE REF TO CL_XML_DOCUMENT,
          LC_XML_RET_E TYPE REF TO CL_XML_DOCUMENT,
          LC_TAMANHO   TYPE I,
          LC_XML_NODE  TYPE REF TO IF_IXML_NODE,
          LC_ITERATOR  TYPE REF TO IF_IXML_NODE_ITERATOR,
          LC_STRING    TYPE STRING,
          LC_VALOR     TYPE STRING,
          LC_ERRO      TYPE ZDE_REPOM_ERROS.

    DATA: LC_CIDADE_ORIGEM_IBGE  TYPE C LENGTH 5,
          LC_CIDADE_DESTINO_IBGE TYPE C LENGTH 5,
          LC_UF_ORIGEM_IBGE      TYPE C LENGTH 5,
          LC_UF_DESTINO_IBGE     TYPE C LENGTH 5,
          LC_TEL_NUMBER          TYPE AD_TLNMBR1,
          LC_XML_AUTENTICA       TYPE STRING,
          WA_ZLEST0122           TYPE ZLEST0122,
          LC_DS_CIDADE_ORIGEM    TYPE C LENGTH 55,
          LC_DS_ESTADO_ORIGEM    TYPE C LENGTH 02,
          LC_DS_CIDADE_DESTINO   TYPE C LENGTH 55,
          LC_DS_ESTADO_DESTINO   TYPE C LENGTH 02.

    I_CONSULTOU = ABAP_FALSE.

    AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
                                    ID 'ZREPOMATV' FIELD '05'.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S063 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    LC_CIDADE_ORIGEM_IBGE  = ME->CD_CID_ORIGEM+5(5).
    LC_CIDADE_DESTINO_IBGE = ME->CD_CID_DESTINO+5(5).
    LC_UF_ORIGEM_IBGE      = ME->CD_CID_ORIGEM(2).
    LC_UF_DESTINO_IBGE     = ME->CD_CID_DESTINO(2).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LC_CIDADE_ORIGEM_IBGE
      IMPORTING
        OUTPUT = LC_CIDADE_ORIGEM_IBGE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LC_CIDADE_DESTINO_IBGE
      IMPORTING
        OUTPUT = LC_CIDADE_DESTINO_IBGE.

    ME->GET_AUTENTICA( EXPORTING I_OBJETO = ME RECEIVING XML = LC_XML_AUTENTICA ).
    CALL METHOD ME->LIMPAR.
    ME->CTNA(   EXPORTING TEXTO = '<?xml version="1.0" encoding="utf-8"?>' ).
    ME->CTNA(   EXPORTING TEXTO = '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' ).
    ME->CTNA(   EXPORTING TEXTO = '<soap12:Body>' ).
    ME->CTNAB(  EXPORTING TAG  = 'ConsultaRoteiros xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao"' ).
    ME->CTNA(   EXPORTING TEXTO = LC_XML_AUTENTICA ).

    ME->CTNAV(  EXPORTING TAG = 'strEstadoOrigem' VALOR = LC_UF_ORIGEM_IBGE ).
    ME->CTNAV(  EXPORTING TAG = 'strCodigoIBGEOrigem' VALOR = LC_CIDADE_ORIGEM_IBGE ).

    ME->CTNAV(  EXPORTING TAG = 'strEstadoDestino' VALOR = LC_UF_DESTINO_IBGE ).
    ME->CTNAV(  EXPORTING TAG = 'strCodigoIBGEDestino' VALOR = LC_CIDADE_DESTINO_IBGE ).

    ME->CTNFE(  EXPORTING TAG = 'ConsultaRoteiros' ).
    ME->CTNA(   EXPORTING TEXTO = '</soap12:Body>' ).
    ME->CTNA(   EXPORTING TEXTO = '</soap12:Envelope>' ).

    IF ME->CK_SALVAR_XML_LOCAL EQ ABAP_TRUE.
      CONCATENATE 'C:\Maggi\REPOM\ConsultaRota' ME->ID_ROTA '.xml' INTO I_NAME_FILE.
      CALL METHOD ME->SALVA_XML
        EXPORTING
          I_NAME_FILE = I_NAME_FILE.
    ENDIF.

    TRY .
        ME->SET_SERVICO( I_SERVICO = 'R1' ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        LC_MSG = LC_EXCEPTION->GET_TEXT( ).
        MESSAGE E008 WITH 'R1 (SolicitaRoteiro ME->SET_SERVICO)' RAISING SERVICO_NAO_ENCONTRADO.
    ENDTRY.

    ME->SET_TIPO( I_TIPO = 'I' ).

    TRY .
        VAR_HTTP = ME->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        LC_MSG = LC_EXCEPTION->GET_TEXT( ).
        MESSAGE E008 WITH 'R1 (SolicitaRoteiro ME->URL)' RAISING SERVICO_NAO_ENCONTRADO.
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
      CONCATENATE 'C:\Maggi\REPOM\ConsultaRotaRet' ME->ID_ROTA '.xml' INTO I_NAME_FILE.
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
        NAME = 'ConsultaRoteirosResult'
      RECEIVING
        NODE = LC_XML_NODE.

    IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).
      LC_STRING = LC_XML_NODE->GET_VALUE( ).
      TRANSLATE LC_STRING TO UPPER CASE.
      IF LC_STRING EQ 'TRUE'.

        CALL METHOD LC_XML_RET->FIND_NODE
          EXPORTING
            NAME = 'strXmlOut'
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
              NAME = 'roteiros'
            RECEIVING
              NODE = LC_XML_NODE.

          IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).
            LC_ITERATOR = LC_XML_NODE->CREATE_ITERATOR( ).
            LC_XML_NODE = LC_ITERATOR->GET_NEXT( ).

            CLEAR: WA_ZLEST0122, PERCURSOS_REPOM.

            WHILE NOT LC_XML_NODE IS INITIAL.
              CASE LC_XML_NODE->GET_TYPE( ).
                WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.
                  LC_STRING = LC_XML_NODE->GET_NAME( ).
                  LC_VALOR  = LC_XML_NODE->GET_VALUE( ).
                  CASE LC_STRING.
                    WHEN 'roteiro'.
                      IF WA_ZLEST0122 IS NOT INITIAL AND WA_ZLEST0122-ID_ROTA EQ ME->ID_ROTA.
                        WA_ZLEST0122-CD_PAIS        = ME->CD_PAIS.
                        WA_ZLEST0122-CD_CID_ORIGEM  = ME->CD_CID_ORIGEM .
                        WA_ZLEST0122-CD_CID_DESTINO = ME->CD_CID_DESTINO.
                        CONCATENATE LC_DS_ESTADO_ORIGEM '-' LC_DS_CIDADE_ORIGEM INTO WA_ZLEST0122-DS_CID_ORIGEM SEPARATED BY SPACE.
                        CONCATENATE LC_DS_ESTADO_DESTINO '-' LC_DS_CIDADE_DESTINO INTO WA_ZLEST0122-DS_CID_DESTINO SEPARATED BY SPACE.
                        APPEND WA_ZLEST0122 TO PERCURSOS_REPOM.
                        ME->CK_ALTERADO = ABAP_TRUE.
                        CLEAR: WA_ZLEST0122, LC_DS_CIDADE_ORIGEM, LC_DS_ESTADO_ORIGEM, LC_DS_CIDADE_DESTINO, LC_DS_ESTADO_DESTINO.
                      ENDIF.
                    WHEN 'roteiro_codigo'.
                      WA_ZLEST0122-ID_ROTA_REPOM = LC_VALOR.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          INPUT  = WA_ZLEST0122-ID_ROTA_REPOM
                        IMPORTING
                          OUTPUT = WA_ZLEST0122-ID_ROTA_REPOM.
                    WHEN 'percurso_codigo'.
                      WA_ZLEST0122-ID_PERCURSO_REPOM = LC_VALOR.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          INPUT  = WA_ZLEST0122-ID_PERCURSO_REPOM
                        IMPORTING
                          OUTPUT = WA_ZLEST0122-ID_PERCURSO_REPOM.
                    WHEN 'percurso_descricao'.
                      WA_ZLEST0122-DS_PERCURSO_REPOM = LC_VALOR.
                    WHEN 'cidade_origem'.
                      LC_DS_CIDADE_ORIGEM = LC_VALOR.
                    WHEN 'estado_origem'.
                      LC_DS_ESTADO_ORIGEM = LC_VALOR.
                    WHEN 'cidade_destino'.
                      LC_DS_CIDADE_DESTINO = LC_VALOR.
                    WHEN 'estado_destino'.
                      LC_DS_ESTADO_DESTINO = LC_VALOR.
                    WHEN 'km_ida'.
                      WA_ZLEST0122-NR_KM_IDA = LC_VALOR.
                    WHEN 'km_volta'.
                      WA_ZLEST0122-NR_KM_VOLTA = LC_VALOR.
                    WHEN 'processo_transporte_tipo'.
                      WA_ZLEST0122-TP_PROC_TRANSP = LC_VALOR.
                    WHEN 'roteiro_cliente_codigo'.
                      WA_ZLEST0122-ID_ROTA = LC_VALOR.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          INPUT  = WA_ZLEST0122-ID_ROTA
                        IMPORTING
                          OUTPUT = WA_ZLEST0122-ID_ROTA.
                  ENDCASE.
              ENDCASE.
              LC_XML_NODE = LC_ITERATOR->GET_NEXT( ).
            ENDWHILE.
            IF WA_ZLEST0122 IS NOT INITIAL AND WA_ZLEST0122-ID_ROTA EQ ME->ID_ROTA.
              WA_ZLEST0122-CD_PAIS        = ME->CD_PAIS.
              WA_ZLEST0122-CD_CID_ORIGEM  = ME->CD_CID_ORIGEM.
              WA_ZLEST0122-CD_CID_DESTINO = ME->CD_CID_DESTINO.
              CONCATENATE LC_DS_ESTADO_ORIGEM '-' LC_DS_CIDADE_ORIGEM INTO WA_ZLEST0122-DS_CID_ORIGEM SEPARATED BY SPACE.
              CONCATENATE LC_DS_ESTADO_DESTINO '-' LC_DS_CIDADE_DESTINO INTO WA_ZLEST0122-DS_CID_DESTINO SEPARATED BY SPACE.
              APPEND WA_ZLEST0122 TO PERCURSOS_REPOM.
              ME->CK_ALTERADO = ABAP_TRUE.
              CLEAR: WA_ZLEST0122, LC_DS_CIDADE_ORIGEM, LC_DS_ESTADO_ORIGEM, LC_DS_CIDADE_DESTINO, LC_DS_ESTADO_DESTINO.
            ENDIF.
          ENDIF.

          IF PERCURSOS_REPOM[] IS INITIAL.
            "072  Não Retornado Percurso para a Rota &1 Solicitada!
            MESSAGE E072 WITH ME->ID_ROTA RAISING ERRO.
          ENDIF.
          ME->SET_TP_ST_ROTA( EXPORTING I_TP_ST_ROTA = '3' ).
          ME->GRAVAR( ).
          I_CONSULTOU = ABAP_TRUE.
          MESSAGE S018.

        ELSE.
          MESSAGE E014 RAISING ERRO.
        ENDIF.

      ELSE.
        ME->SET_TP_ST_ROTA( EXPORTING I_TP_ST_ROTA = '4' ).
        ME->GRAVAR( ).

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

*POST /repomintegracaows/integracao/integracao.asmx HTTP/1.1
*Host: qa.repom.com.br
*Content-Type: application/soap+xml; charset=utf-8
*Content-Length: length
*
*<?xml version="1.0" encoding="utf-8"?>
*<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">
*  <soap12:Body>
*    <ConsultaRoteiros xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao">
*      <strCliente>0253</strCliente>
*      <strAssinaturaDigital>3343843794523796459</strAssinaturaDigital>
*      <strEstadoOrigem>MT</strEstadoOrigem>
*      <strCodigoIBGEOrigem>102</strCodigoIBGEOrigem>
*      <strEstadoDestino>MT</strEstadoDestino>
*      <strCodigoIBGEDestino>7602</strCodigoIBGEDestino>
*    </ConsultaRoteiros>
*  </soap12:Body>
*</soap12:Envelope>

*<?xml version="1.0" encoding="utf-8"?>
*<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
*    <soap:Body>
*        <ConsultaRoteirosResponse xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao">
*            <ConsultaRoteirosResult>true</ConsultaRoteirosResult>
*            <strXmlOut>&lt;roteiros /&gt;</strXmlOut>
*        </ConsultaRoteirosResponse>
*    </soap:Body>
*</soap:Envelope>

  ENDMETHOD.


  METHOD DESATIVAR_ROTA.

    I_DESATIVOU = ABAP_FALSE.

    AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
                                    ID 'ZREPOMATV' FIELD '07'.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S063 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CHECK ME->CK_ATIVO EQ ABAP_TRUE.

    ME->SET_CK_ATIVO( EXPORTING I_CK_ATIVO = ABAP_FALSE ).
    IF ME->GRAVAR( ) EQ ABAP_TRUE.
      MESSAGE S039 WITH ME->ID_ROTA.
      I_DESATIVOU = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD EDITAR_PERCURSO.

    READ TABLE ME->PERCURSOS_REPOM ASSIGNING FIELD-SYMBOL(<FS_PERCURSO>)
     WITH KEY ID_ROTA_REPOM = I_ZLEST0122_ANT-ID_ROTA_REPOM
              ID_PERCURSO_REPOM = I_ZLEST0122_ANT-ID_PERCURSO_REPOM.

    IF SY-SUBRC IS INITIAL.
      <FS_PERCURSO> = I_ZLEST0122.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD EXCLUIR_PERCURSO.

    DELETE ME->PERCURSOS_REPOM
     WHERE ID_ROTA_REPOM EQ I_ID_ROTA_REPOM
       AND ID_PERCURSO_REPOM EQ I_ID_PERCURSO_REPOM.

    ME->CK_ALTERADO = ABAP_TRUE.

  ENDMETHOD.


  METHOD GET_PERCURSOS.
    E_PERCURSOS = ME->PERCURSOS_REPOM.
  ENDMETHOD.


  METHOD INCLUIR_PERCURSO.

    APPEND I_ZLEST0122 TO ME->PERCURSOS_REPOM.
    ME->CK_ALTERADO = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_CD_CID_DESTINO.

    IF ME->CD_CID_DESTINO NE I_CD_CID_DESTINO.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->CD_CID_DESTINO = I_CD_CID_DESTINO.

  ENDMETHOD.


  METHOD SET_CD_CID_ORIGEM.

    IF ME->CD_CID_ORIGEM NE I_CD_CID_ORIGEM.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->CD_CID_ORIGEM = I_CD_CID_ORIGEM.

  ENDMETHOD.


  METHOD SET_CD_PAIS.

    IF ME->CD_PAIS NE I_CD_PAIS.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->CD_PAIS = I_CD_PAIS.

  ENDMETHOD.


  METHOD SET_CK_ATIVO.

    IF ME->CK_ATIVO NE I_CK_ATIVO.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->CK_ATIVO = I_CK_ATIVO.

  ENDMETHOD.


  METHOD SET_DS_OBSERVACAO.

    IF ME->DS_OBSERVACAO NE I_DS_OBSERVACAO.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->DS_OBSERVACAO = I_DS_OBSERVACAO.

  ENDMETHOD.


  METHOD SET_DT_ULTIMO_AJUSTE.

    IF ME->DT_ULTIMO_AJUSTE NE I_DT_ULTIMO_AJUSTE.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->DT_ULTIMO_AJUSTE = I_DT_ULTIMO_AJUSTE.

  ENDMETHOD.


  METHOD SET_HR_ULTIMO_AJUSTE.

    IF ME->HR_ULTIMO_AJUSTE NE I_HR_ULTIMO_AJUSTE.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->HR_ULTIMO_AJUSTE = I_HR_ULTIMO_AJUSTE.

  ENDMETHOD.


  METHOD SET_ID_ROTA.
    ME->ID_ROTA = I_ID_ROTA.
  ENDMETHOD.


  METHOD SET_PERCURSO.
    APPEND I_ZLEST0122 TO ME->PERCURSOS_REPOM.
  ENDMETHOD.


  METHOD SET_TP_IDA_VOLTA.

    IF ME->TP_IDA_VOLTA NE I_TP_IDA_VOLTA.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->TP_IDA_VOLTA = I_TP_IDA_VOLTA.

  ENDMETHOD.


  METHOD SET_TP_PROC_TRANSP.

    IF ME->TP_PROC_TRANSP NE I_TP_PROC_TRANSP.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->TP_PROC_TRANSP = I_TP_PROC_TRANSP.

  ENDMETHOD.


  METHOD SET_TP_ST_ROTA.

    IF ME->TP_ST_ROTA NE I_TP_ST_ROTA.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->TP_ST_ROTA = I_TP_ST_ROTA.

  ENDMETHOD.


  METHOD SET_US_ULTIMO_AJUSTE.

    IF ME->US_ULTIMO_AJUSTE NE I_US_ULTIMO_AJUSTE.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->US_ULTIMO_AJUSTE = I_US_ULTIMO_AJUSTE.


  ENDMETHOD.


  METHOD SOLICITAR_ROTA.

    DATA: I_NAME_FILE  TYPE STRING,
          VAR_HTTP     TYPE REF TO IF_HTTP_CLIENT,
          LC_EXCEPTION TYPE REF TO ZCX_WEBSERVICE,
          LC_MSG       TYPE STRING,
          XML_RETORNO  TYPE STRING,
          XML_INPUT    TYPE STRING.

    "Leitura da XML
    DATA: LC_XML_RET        TYPE REF TO CL_XML_DOCUMENT,
          LC_XML_RET_E      TYPE REF TO CL_XML_DOCUMENT,
          LC_TAMANHO        TYPE I,
          LC_XML_NODE       TYPE REF TO IF_IXML_NODE,
          LC_ITERATOR       TYPE REF TO IF_IXML_NODE_ITERATOR,
          LC_STRING         TYPE STRING,
          LC_EMAIL_ADDRESS  TYPE STRING,
*          LC_FILIAL_CLIENTE TYPE STRING,
          LC_XML_AUTENTICA  TYPE STRING.

    DATA: LC_ERRO                TYPE ZDE_REPOM_ERROS,
          LC_CIDADE_ORIGEM_IBGE  TYPE C LENGTH 5,
          LC_CIDADE_DESTINO_IBGE TYPE C LENGTH 5,
          LC_UF_ORIGEM_IBGE      TYPE C LENGTH 2,
          LC_UF_DESTINO_IBGE     TYPE C LENGTH 2,
          LC_TEL_NUMBER          TYPE AD_TLNMBR1.

    CHECK ME->CK_ALTERADO EQ ABAP_FALSE.

    CHECK ME->CK_ATIVO EQ ABAP_TRUE.

    IF ME->ID_ROTA(1) EQ 'A'.
      MESSAGE S069 DISPLAY LIKE 'E'.
    ENDIF.

    CHECK ME->ID_ROTA(1) NE 'A'.

    I_SOLICITOU = ABAP_FALSE.

    AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
                                    ID 'ZREPOMATV' FIELD '04'.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S063 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CLEAR: E_ERROS.

    CALL FUNCTION 'EFG_GEN_GET_USER_EMAIL'
      EXPORTING
        I_UNAME           = SY-UNAME
      IMPORTING
        E_EMAIL_ADDRESS   = LC_EMAIL_ADDRESS
      EXCEPTIONS
        NOT_QUALIFIED     = 1
        USER_NOT_FOUND    = 2
        ADDRESS_NOT_FOUND = 3
        OTHERS            = 4.

    LC_CIDADE_ORIGEM_IBGE  = ME->CD_CID_ORIGEM+5(5).
    LC_CIDADE_DESTINO_IBGE = ME->CD_CID_DESTINO+5(5).
    LC_UF_ORIGEM_IBGE      = ME->CD_CID_ORIGEM(2).
    LC_UF_DESTINO_IBGE     = ME->CD_CID_DESTINO(2).

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = ME->ZIF_REPOM~BRANCH
*      IMPORTING
*        OUTPUT = LC_FILIAL_CLIENTE.

*    LC_FILIAL_CLIENTE = ZIF_REPOM~BRANCH.

    SELECT SINGLE C~TEL_NUMBER INTO LC_TEL_NUMBER
      FROM USR21 AS E
     INNER JOIN ADRP AS N ON ( N~CLIENT EQ E~MANDT AND N~PERSNUMBER EQ E~PERSNUMBER )
     INNER JOIN ADCP AS C ON ( C~PERSNUMBER EQ N~PERSNUMBER AND C~NATION EQ N~NATION )
     WHERE E~BNAME      EQ SY-UNAME
       AND C~TEL_NUMBER NE SPACE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LC_CIDADE_ORIGEM_IBGE
      IMPORTING
        OUTPUT = LC_CIDADE_ORIGEM_IBGE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LC_CIDADE_DESTINO_IBGE
      IMPORTING
        OUTPUT = LC_CIDADE_DESTINO_IBGE.

    ME->GET_AUTENTICA( EXPORTING I_OBJETO = ME RECEIVING XML = LC_XML_AUTENTICA ).

    CALL METHOD ME->LIMPAR.
    ME->CTNA(   EXPORTING TEXTO = '<?xml version="1.0" encoding="utf-8"?>' ).
    ME->CTNA(   EXPORTING TEXTO = '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' ).
    ME->CTNA(   EXPORTING TEXTO = '<soap12:Body>' ).
    ME->CTNAB(  EXPORTING TAG  = 'SolicitaRoteiro xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao"' ).
    ME->CTNA(   EXPORTING TEXTO = LC_XML_AUTENTICA ).
    ME->CTNAB(  EXPORTING TAG  = 'strXmlIn' ).
    ME->CTNAB(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'solicita_roteiros' ).
    ME->CTNAB(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'roteiro' ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'roteiro_codigo_cliente' VALOR = ME->ID_ROTA ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'cidade_origem_ibge' VALOR = LC_CIDADE_ORIGEM_IBGE ).
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'cidade_origem_cep' ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'estado_origem' VALOR = LC_UF_ORIGEM_IBGE ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'cidade_destino_ibge' VALOR = LC_CIDADE_DESTINO_IBGE ).
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'cidade_destino_cep' ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'estado_destino' VALOR = LC_UF_DESTINO_IBGE ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'usuario_nome' VALOR = SY-UNAME ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'usuario_telefone' VALOR = LC_TEL_NUMBER ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'usuario_email' VALOR = LC_EMAIL_ADDRESS ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'filial_codigo_cliente' VALOR = ZIF_REPOM~BRANCH ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'tipo_processo_transporte' VALOR = ME->TP_PROC_TRANSP ).
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'tempo_previsto_viagem' ).
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'tipo_local_quitacao' ).
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'codigo_local_quitacao' ).
    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'ida_volta' VALOR = ME->TP_IDA_VOLTA ).

    " 1  Pendente / 2  Enviado / 3  Autorizado / 4  Erro
    "IF ME->TP_ST_ROTA EQ '1' OR ME->TP_ST_ROTA IS INITIAL.
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'altera_roteiro' ).
    "ELSE.
    "  ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'altera_roteiro' VALOR = 'sim' ).
    "ENDIF.

    ME->CTNAV(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'observacao' VALOR = ZCL_MONTA_XML=>CLEAR_STRANGE_CHARS( I_STRING = CONV #( ME->DS_OBSERVACAO ) ) ).
    ME->CTNAFE( EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'vias' ).
    ME->CTNFE(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'roteiro' ).
    ME->CTNFE(  EXPORTING ABRE = '&lt;' FECHA = '&gt;' TAG = 'solicita_roteiros' ).
    ME->CTNFE(  EXPORTING TAG = 'strXmlIn' ).
    ME->CTNAFE( EXPORTING TAG = 'strXmlErr' ).
    ME->CTNFE(  EXPORTING TAG = 'SolicitaRoteiro' ).
    ME->CTNA(   EXPORTING TEXTO = '</soap12:Body>' ).
    ME->CTNA(   EXPORTING TEXTO = '</soap12:Envelope>' ).

    IF ME->CK_SALVAR_XML_LOCAL EQ ABAP_TRUE.
      CONCATENATE 'C:\Maggi\REPOM\SolicitaRota' ME->ID_ROTA '.xml' INTO I_NAME_FILE.
      CALL METHOD ME->SALVA_XML
        EXPORTING
          I_NAME_FILE = I_NAME_FILE.
    ENDIF.

    TRY .
        ME->SET_SERVICO( I_SERVICO = 'R1' ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        LC_MSG = LC_EXCEPTION->GET_TEXT( ).
        MESSAGE E008 WITH 'R1 (SolicitaRoteiro ME->SET_SERVICO)' RAISING SERVICO_NAO_ENCONTRADO.
    ENDTRY.

    ME->SET_TIPO( I_TIPO = 'I' ).

    TRY .
        VAR_HTTP = ME->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        LC_MSG = LC_EXCEPTION->GET_TEXT( ).
        MESSAGE E008 WITH 'R1 (SolicitaRoteiro ME->URL)' RAISING SERVICO_NAO_ENCONTRADO.
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
      CONCATENATE 'C:\Maggi\REPOM\SolicitaRotaRet' ME->ID_ROTA '.xml' INTO I_NAME_FILE.
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
        NAME = 'SolicitaRoteiroResult'
      RECEIVING
        NODE = LC_XML_NODE.

    IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).
      LC_STRING = LC_XML_NODE->GET_VALUE( ).
      TRANSLATE LC_STRING TO UPPER CASE.
      IF LC_STRING EQ 'TRUE'.
        ME->SET_TP_ST_ROTA( EXPORTING I_TP_ST_ROTA = '2' ).
        ME->GRAVAR( ).
        I_SOLICITOU = ABAP_TRUE.
        MESSAGE S015.
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

* POST /repomintegracaows/integracao/integracao.asmx HTTP/1.1
* Host: qa.repom.com.br
* Content-Type: application/soap+xml; charset=utf-8
* Content-Length: length

*<?xml version="1.0" encoding="utf-8"?>
*<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
*  <soap:Body>
*    <SolicitaRoteiro xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao">
*      <strCliente>0253</strCliente>
*      <strAssinaturaDigital>3343843794523796459</strAssinaturaDigital>
*      <strXmlIn>
*        &lt;solicita_roteiros&gt;
*          &lt;roteiro&gt;
*            &lt;roteiro_codigo_cliente&gt;1001&lt;/roteiro_codigo_cliente&gt;
*            &lt;cidade_origem_ibge&gt;50308&lt;/cidade_origem_ibge&gt;
*            &lt;cidade_origem_cep/&gt;
*            &lt;estado_origem&gt;SP&lt;/estado_origem&gt;
*            &lt;cidade_destino_ibge&gt;4557&lt;/cidade_destino_ibge&gt;
*            &lt;cidade_destino_cep/&gt;
*            &lt;estado_destino&gt;RJ&lt;/estado_destino&gt;
*            &lt;usuario_nome&gt;Marcus Bárbara&lt;/usuario_nome&gt;
*            &lt;usuario_telefone&gt;(66) 99645-5500&lt;/usuario_telefone&gt;
*            &lt;usuario_email&gt;marcus.barbara@amaggi.com.br&lt;/usuario_email&gt;
*            &lt;filial_codigo_cliente&gt;&lt;/filial_codigo_cliente&gt;
*            &lt;tipo_processo_transporte&gt;4&lt;/tipo_processo_transporte&gt;
*            &lt;tempo_previsto_viagem/&gt;
*            &lt;tipo_local_quitacao/&gt;
*            &lt;codigo_local_quitacao/&gt;
*            &lt;ida_volta&gt;1&lt;/ida_volta&gt;
*            &lt;altera_roteiro/&gt;
*            &lt;observacao/&gt;
*            &lt;vias /&gt;
*          &lt;/roteiro&gt;
*        &lt;/solicita_roteiros&gt;
*      </strXmlIn>
*      <strXmlErr></strXmlErr>
*    </SolicitaRoteiro>
*  </soap:Body>
*</soap:Envelope>
*
*<?xml version="1.0" encoding="utf-8"?>
*<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
*    <soap:Body>
*        <SolicitaRoteiroResponse xmlns="http://www.repom.com.br/RepomIntegracaoWs/Integracao">
*            <SolicitaRoteiroResult>true</SolicitaRoteiroResult>
*            <strXmlErr />
*        </SolicitaRoteiroResponse>
*    </soap:Body>
*</soap:Envelope>

  ENDMETHOD.


  METHOD ZIF_CADASTRO~EXCLUIR_REGISTRO.

    I_EXCLUIU = ABAP_FALSE.

    IF ME->VALIDAR_EXCLUSAO( ) EQ ABAP_TRUE.

      "Percursos REPOM - Retorno de Solicitação
      DELETE FROM ZLEST0122 WHERE ID_ROTA EQ ME->ID_ROTA.

      "Rotas REPOM - Solicitação
      DELETE FROM ZLEST0121 WHERE ID_ROTA EQ ME->ID_ROTA.

      COMMIT WORK.

      I_EXCLUIU = ABAP_TRUE.
      MESSAGE S054.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GET_REGISTRO.

    DATA: E_ZLEST0121 TYPE ZLEST0121.

    E_ZLEST0121-MANDT          = SY-MANDT.
    E_ZLEST0121-ID_ROTA        = ME->ID_ROTA.
    E_ZLEST0121-BUKRS          = ME->BUKRS.
    E_ZLEST0121-BRANCH         = ME->BRANCH.
    E_ZLEST0121-CD_PAIS        = ME->CD_PAIS.
    E_ZLEST0121-CD_CID_ORIGEM  = ME->CD_CID_ORIGEM.
    E_ZLEST0121-CD_CID_DESTINO = ME->CD_CID_DESTINO.
    E_ZLEST0121-TP_PROC_TRANSP = ME->TP_PROC_TRANSP.
    E_ZLEST0121-TP_IDA_VOLTA   = ME->TP_IDA_VOLTA.
    E_ZLEST0121-DS_OBSERVACAO  = ME->DS_OBSERVACAO.
    E_ZLEST0121-TP_ST_ROTA     = ME->TP_ST_ROTA.
    E_ZLEST0121-CK_ATIVO       = ME->CK_ATIVO.
    E_ZLEST0121-US_ULTIMO_AJUSTE = ME->US_ULTIMO_AJUSTE.
    E_ZLEST0121-DT_ULTIMO_AJUSTE = ME->DT_ULTIMO_AJUSTE.
    E_ZLEST0121-HR_ULTIMO_AJUSTE = ME->HR_ULTIMO_AJUSTE.
    MOVE-CORRESPONDING E_ZLEST0121 TO E_REGISTRO.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GRAVAR_REGISTRO.

    DATA: WA_ZLEST0121 TYPE ZLEST0121.
    FIELD-SYMBOLS: <FS_22> TYPE ZLEST0122.

    I_GRAVOU = ABAP_FALSE.

    IF ME->CK_ALTERADO EQ ABAP_TRUE.

      IF ME->VALIDAR_REGISTRO( ) EQ ABAP_TRUE.
        IF ME->ID_ROTA IS INITIAL.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              NR_RANGE_NR = '01'
              OBJECT      = 'ZPEDREPOM'
            IMPORTING
              NUMBER      = ME->ID_ROTA.
        ELSE.
          DELETE FROM ZLEST0122 WHERE ID_ROTA = ME->ID_ROTA.
        ENDIF.

        CALL METHOD ME->SET_US_ULTIMO_AJUSTE( EXPORTING I_US_ULTIMO_AJUSTE = SY-UNAME ).
        CALL METHOD ME->SET_DT_ULTIMO_AJUSTE( EXPORTING I_DT_ULTIMO_AJUSTE = SY-DATUM ).
        CALL METHOD ME->SET_HR_ULTIMO_AJUSTE( EXPORTING I_HR_ULTIMO_AJUSTE = SY-UZEIT ).

        WA_ZLEST0121-ID_ROTA          = ME->ID_ROTA.
        WA_ZLEST0121-BUKRS            = ME->BUKRS.
        WA_ZLEST0121-BRANCH           = ME->BRANCH.
        WA_ZLEST0121-CD_PAIS          = ME->CD_PAIS.
        WA_ZLEST0121-CD_CID_ORIGEM    = ME->CD_CID_ORIGEM.
        WA_ZLEST0121-CD_CID_DESTINO   = ME->CD_CID_DESTINO.
        WA_ZLEST0121-TP_PROC_TRANSP   = ME->TP_PROC_TRANSP.
        WA_ZLEST0121-TP_IDA_VOLTA     = ME->TP_IDA_VOLTA.
        WA_ZLEST0121-DS_OBSERVACAO    = ME->DS_OBSERVACAO.
        WA_ZLEST0121-TP_ST_ROTA       = ME->TP_ST_ROTA.
        WA_ZLEST0121-CK_ATIVO         = ME->CK_ATIVO.
        WA_ZLEST0121-US_ULTIMO_AJUSTE = ME->US_ULTIMO_AJUSTE.
        WA_ZLEST0121-DT_ULTIMO_AJUSTE = ME->DT_ULTIMO_AJUSTE.
        WA_ZLEST0121-HR_ULTIMO_AJUSTE = ME->HR_ULTIMO_AJUSTE.
        MODIFY ZLEST0121 FROM WA_ZLEST0121.

        LOOP AT ME->PERCURSOS_REPOM ASSIGNING <FS_22>.
          IF <FS_22>-ID_ROTA IS INITIAL.
            <FS_22>-ID_ROTA = ME->ID_ROTA.
          ENDIF.
          MODIFY ZLEST0122 FROM <FS_22>.
        ENDLOOP.
        COMMIT WORK.

        ME->CK_ALTERADO = ABAP_FALSE.

        I_GRAVOU = ABAP_TRUE.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~LIMPAR_REGISTRO.

    CLEAR: ME->ID_ROTA,
           ME->BUKRS,
           ME->BRANCH,
           ME->CD_PAIS,
           ME->CD_CID_ORIGEM,
           ME->CD_CID_DESTINO,
           ME->TP_PROC_TRANSP,
           ME->TP_IDA_VOLTA,
           ME->DS_OBSERVACAO,
           ME->CK_ALTERADO,
           ME->PERCURSOS_REPOM,
           ME->TP_ST_ROTA,
           ME->CK_ATIVO,
           ME->US_ULTIMO_AJUSTE,
           ME->DT_ULTIMO_AJUSTE,
           ME->HR_ULTIMO_AJUSTE.

    ME->SET_CD_PAIS( EXPORTING I_CD_PAIS = 'BR' ).
    ME->SET_TP_PROC_TRANSP( EXPORTING I_TP_PROC_TRANSP = '4' ).
    ME->SET_TP_ST_ROTA( EXPORTING I_TP_ST_ROTA = '1' ).
    ME->SET_CK_ATIVO( EXPORTING I_CK_ATIVO = ABAP_TRUE ).

*1  Pendente
*2  Enviado
*3  Autorizado
*4  Erro
    ME->CK_SALVAR_XML_LOCAL = ABAP_FALSE.
    ME->CK_ALTERADO = ABAP_FALSE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.

    ME->LIMPAR_REGISTRO( ).

  ENDMETHOD.


  METHOD ZIF_CADASTRO~SET_REGISTRO.

    DATA: WA_ZLEST0121 TYPE ZLEST0121,
          IT_ZLEST0122 TYPE TABLE OF ZLEST0122,
          WA_ZLEST0122 TYPE ZLEST0122.

    ME->LIMPAR_REGISTRO( ).

    SELECT SINGLE * INTO WA_ZLEST0121 FROM ZLEST0121 WHERE ID_ROTA EQ I_ID_REGISTRO.
    IF SY-SUBRC IS INITIAL.
      CALL METHOD ME->SET_ID_ROTA( EXPORTING I_ID_ROTA = WA_ZLEST0121-ID_ROTA ).
      CALL METHOD ME->SET_BUKRS( EXPORTING I_BUKRS = WA_ZLEST0121-BUKRS ).
      CALL METHOD ME->SET_BRANCH( EXPORTING I_BRANCH = WA_ZLEST0121-BRANCH ).
      CALL METHOD ME->SET_CD_PAIS( EXPORTING I_CD_PAIS = WA_ZLEST0121-CD_PAIS ).
      CALL METHOD ME->SET_CD_CID_ORIGEM( EXPORTING I_CD_CID_ORIGEM = WA_ZLEST0121-CD_CID_ORIGEM ).
      CALL METHOD ME->SET_CD_CID_DESTINO( EXPORTING I_CD_CID_DESTINO = WA_ZLEST0121-CD_CID_DESTINO ).
      CALL METHOD ME->SET_TP_PROC_TRANSP( EXPORTING I_TP_PROC_TRANSP = WA_ZLEST0121-TP_PROC_TRANSP ).
      CALL METHOD ME->SET_TP_IDA_VOLTA( EXPORTING I_TP_IDA_VOLTA = WA_ZLEST0121-TP_IDA_VOLTA ).
      CALL METHOD ME->SET_DS_OBSERVACAO( EXPORTING I_DS_OBSERVACAO = WA_ZLEST0121-DS_OBSERVACAO ).
      CALL METHOD ME->SET_TP_ST_ROTA( EXPORTING I_TP_ST_ROTA = WA_ZLEST0121-TP_ST_ROTA ).
      CALL METHOD ME->SET_CK_ATIVO( EXPORTING I_CK_ATIVO = WA_ZLEST0121-CK_ATIVO ).
      CALL METHOD ME->SET_US_ULTIMO_AJUSTE( EXPORTING I_US_ULTIMO_AJUSTE = WA_ZLEST0121-US_ULTIMO_AJUSTE ).
      CALL METHOD ME->SET_DT_ULTIMO_AJUSTE( EXPORTING I_DT_ULTIMO_AJUSTE = WA_ZLEST0121-DT_ULTIMO_AJUSTE ).
      CALL METHOD ME->SET_HR_ULTIMO_AJUSTE( EXPORTING I_HR_ULTIMO_AJUSTE = WA_ZLEST0121-HR_ULTIMO_AJUSTE ).

      SELECT * INTO TABLE IT_ZLEST0122 FROM ZLEST0122 WHERE ID_ROTA EQ I_ID_REGISTRO.
      LOOP AT IT_ZLEST0122 INTO WA_ZLEST0122.
        CALL METHOD ME->SET_PERCURSO( EXPORTING I_ZLEST0122 = WA_ZLEST0122 ).
      ENDLOOP.
      ME->CK_ALTERADO = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

    AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
                                    ID 'ZREPOMATV' FIELD '03'.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S063 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CASE ME->TP_ST_ROTA.
      WHEN 2.
        MESSAGE S052 DISPLAY LIKE 'E'.
        EXIT.
      WHEN 3.
        MESSAGE S053 DISPLAY LIKE 'E'.
        EXIT.
    ENDCASE.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_REGISTRO.

    DATA: LC_ACAO TYPE ZDE_ACAO_PEDAGIO_REPOM.

    E_VALIDOU = ABAP_FALSE.

    IF ME->BUKRS IS INITIAL.
      MESSAGE S000 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->ID_ROTA IS INITIAL.
      LC_ACAO = '01'.
    ELSE.
      LC_ACAO = '02'.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
                                    ID 'ZREPOMATV' FIELD  LC_ACAO.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S063 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->BRANCH IS INITIAL.
      MESSAGE S001 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->CD_PAIS IS INITIAL.
      MESSAGE S002 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->CD_CID_ORIGEM IS INITIAL.
      MESSAGE S003 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->CD_CID_DESTINO IS INITIAL.
      MESSAGE S004 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->TP_PROC_TRANSP IS INITIAL.
      MESSAGE S005 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->TP_IDA_VOLTA IS INITIAL.
      MESSAGE S006 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->DS_OBSERVACAO IS INITIAL.
      MESSAGE S007 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL.

    R_PERMITIDO = ABAP_FALSE.

    IF ME->ID_ROTA IS NOT INITIAL.
      R_PERMITIDO = ABAP_FALSE.
    ELSE.
      R_PERMITIDO = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_REPOM~SET_BRANCH.
    ME->BRANCH = I_BRANCH.
  ENDMETHOD.


  METHOD ZIF_REPOM~SET_BUKRS.
    ME->BUKRS = I_BUKRS.
  ENDMETHOD.
ENDCLASS.

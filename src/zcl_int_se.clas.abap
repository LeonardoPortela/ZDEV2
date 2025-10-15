class ZCL_INT_SE definition
  public
  final
  create public .

public section.

  class-methods CREATE_WORKFLOW_SOFTEXPERT_SFC
    importing
      value(SOLICITACAO) type ZFIT0045
    exporting
      value(E_MSG) type STRING
      value(E_RECORDID) type CHAR30 .
  class-methods L_TOTALIZAR_VALOR_SOLIC
    importing
      value(E_NRO_SOL) type ZFIT0045-NRO_SOL
    exporting
      value(I_VLR_ADIANTAMENTO) type ZFIT0046-VLR_ADIANTAMENTO .
  class-methods L_CHECK_EMPRESA
    importing
      value(E_BUKRS) type BUKRS optional
    exporting
      !I_BUTXT type BUTXT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_SE IMPLEMENTATION.


  METHOD CREATE_WORKFLOW_SOFTEXPERT_SFC.



    DATA: OBJETO      TYPE REF TO ZCL_SOFT_EXPERT_WS_INJECT,
          WA_VALUES   TYPE ZDE_ZSEXPT00004_VALUES,
          WA_ANEXOS   TYPE ZDE_ZSEXPT00007,
          LC_VL_TOTAL TYPE C LENGTH 20,
          LC_CTE      TYPE ZIB_CTE_DIST_TER,
          LC_LFBK     TYPE LFBK,
          LC_BNKA     TYPE BNKA,
          LC_TEXT1    TYPE STRING,
          LC_TEXT2    TYPE STRING,
          LC_INFO     TYPE C LENGTH 4000.

*---> 20.06.2023 - Migração S4 - DG
"    DATA: ID TYPE CHAR02 VALUE '03'.
    DATA: ID TYPE ZCHAR02 VALUE '03'.
*<--- 20.06.2023 - Migração S4 - DG


    TRY .

        "Objeto Generico
        OBJETO = CAST #( ZCL_SOFT_EXPERT_WS_INJECT=>ZIF_SOFT_EXPERT_WS_INJECT~GET_INSTANCE(
                           I_WORKFLOW_SAP = ID
                        )->CLEAR(
                        ) ).

      CATCH ZCX_SOFT_EXPERT_WORKFLOW INTO DATA(EX_ERRO).
        EX_ERRO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        EXIT.
    ENDTRY.


    "Preenchendo os atributos.
**=========================================================================

    "Data da solicitação. --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'datadasolicit'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA
    VALUE       = SY-DATUM ).                               "'20200618'
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Numero da solicitação. --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'nmerosolicita'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-NRO_SOL ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Tipo de transação. --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'tipo'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = '2' ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Banco. --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'banco'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-HBKID_E ).
    IF SOLICITACAO-HBKID_E IS INITIAL.
      WA_VALUES-VALUE = '-'.
    ENDIF.
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Codigo da empresa --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'cdempresa'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-BUKRS ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Selecionar descrição da empresa.
    L_CHECK_EMPRESA(
      EXPORTING
        E_BUKRS =  SOLICITACAO-BUKRS  " Empresa
      IMPORTING
        I_BUTXT =  DATA(I_BUTXT)   " Denominação da firma ou empresa
    ).

    "Descrição da empresa --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'empresa'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = |{ SOLICITACAO-BUKRS } - { I_BUTXT }| ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Totalizar o valor da solicitação.
    L_TOTALIZAR_VALOR_SOLIC(
      EXPORTING
        E_NRO_SOL          =  SOLICITACAO-NRO_SOL   " Nº 10 posições
      IMPORTING
        I_VLR_ADIANTAMENTO =  DATA(I_VLR_ADIANTAMENTO)   " Valor do adiantamento

    ).

    DATA: LC_VALOR_ADIANTAMENTO TYPE C LENGTH 30.
    WRITE I_VLR_ADIANTAMENTO TO LC_VALOR_ADIANTAMENTO.
    CONDENSE LC_VALOR_ADIANTAMENTO NO-GAPS.
    LC_VALOR_ADIANTAMENTO = ZCL_STRING=>REPLACE( EXPORTING I_STR = CONV #( LC_VALOR_ADIANTAMENTO ) I_CHAR_OLD   = '.' ).
    LC_VALOR_ADIANTAMENTO = ZCL_STRING=>REPLACE( EXPORTING I_STR = CONV #( LC_VALOR_ADIANTAMENTO ) I_CHAR_OLD   = ',' I_CHAR_NEW = '.' ).

    "Valor da moeda
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'valormoedaest'
*    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_MOEDA
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_NUMERICO
    VALUE       = LC_VALOR_ADIANTAMENTO ).
*    REPLACE ALL OCCURRENCES OF '.' IN WA_VALUES-VALUE WITH ' '.
    CONDENSE WA_VALUES-VALUE NO-GAPS.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Data do crédito.--
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'datadocredd'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA
    VALUE       = SOLICITACAO-DT_PGTO ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Data do pagamento.--
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'datapgtodou'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA
    VALUE       = SOLICITACAO-DT_PGTO ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Documento SAP --
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'docsapn'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-BELNR ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Numero do pedido. --
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'pedido'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-EBELN ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).
**=============================================================================

    "Creditar / Debitar no banco. --
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'creditarnoban'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-HBKID ).
    IF SOLICITACAO-HBKID IS INITIAL.
      WA_VALUES-VALUE = '-'.
    ENDIF.
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Tipo de operação cambio --
    WA_VALUES = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'nomopr'
    FIELDNAME   = 'NOMOPR'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SWITCH #( SOLICITACAO-TP_OPER WHEN 1 THEN 'Compra-Importação' WHEN 2 THEN 'Compra-Serviço' )
    RELATIONSHIP = ABAP_TRUE
    RELATIONSHIPID = 'tipooprcambio'
     ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Tipo de moeda
    WA_VALUES   = VALUE #(
    KEY         = '1'
    ENTITYID    = 'SFC'
    TABNAME     = 'DYNTSFC'
    ATTRIBUTEID = 'nmmoeda'
    FIELDNAME   = 'TIPO_MOEDA'
    TYPE        = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
    VALUE       = SOLICITACAO-MOEDA_PGTO
    RELATIONSHIP = ABAP_TRUE
    RELATIONSHIPID = 'tipomoedaopr' ).
    CONDENSE WA_VALUES-VALUE.
    OBJETO->ADD_ENTITY_VALUE( I_ENTITY_VALUE = WA_VALUES ).

    "Add Anexo """"""""""""""""""""""""""""""""
*    ME->DANFE( EXPORTING I_CHAVE_NFE = ME->NOTA-CHAVE_NFE I_CHAMAR_BROWSER = ABAP_FALSE IMPORTING E_URL = DATA(E_URL) ).
*    WA_ANEXOS-ACTIVITYID = 'newAttachment'.
*    CONCATENATE ME->NOTA-CHAVE_NFE '.pdf' INTO WA_ANEXOS-FILENAME.
*    WA_ANEXOS-FILE_URL = E_URL.
*    OBJETO->ADD_ATTACHMENTS( I_ATTACHMENT = WA_ANEXOS ).

    DATA: IT_LISTA      TYPE TABLE OF BDN_CON,
          LC_OBJKEY     TYPE SWOTOBJID-OBJKEY,
          DOCUMENT_ID   TYPE SOFOLENTI1-DOC_ID,
          DOCUMENT_DATA TYPE SOFOLENTI1,
          CONT_HEX      TYPE TABLE OF SOLIX.

    LC_OBJKEY = SOLICITACAO-MANDT && SOLICITACAO-NRO_SOL.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        CLASSNAME       = 'ZFIR0031'
        OBJKEY          = LC_OBJKEY
      TABLES
        GOS_CONNECTIONS = IT_LISTA.

    LOOP AT IT_LISTA INTO DATA(WA_LISTA).

      DOCUMENT_ID = WA_LISTA-LOIO_ID.

      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
        EXPORTING
          DOCUMENT_ID                = DOCUMENT_ID
        IMPORTING
          DOCUMENT_DATA              = DOCUMENT_DATA
        TABLES
          CONTENTS_HEX               = CONT_HEX
        EXCEPTIONS
          DOCUMENT_ID_NOT_EXIST      = 1
          OPERATION_NO_AUTHORIZATION = 2
          X_ERROR                    = 3
          OTHERS                     = 4.

      CLEAR: WA_ANEXOS.

      SPLIT DOCUMENT_DATA-OBJ_DESCR AT '.' INTO TABLE DATA(LT_ACTION).
      READ TABLE LT_ACTION INTO DATA(WA_ACTION) INDEX 1.

      WA_ANEXOS-ACTIVITYID = 'ATV-01'.
      WA_ANEXOS-FILENAME   = WA_ACTION && '.' && DOCUMENT_DATA-OBJ_TYPE.

      DATA: E_XSTRING TYPE XSTRING.
      CLEAR: E_XSTRING.
      LOOP AT CONT_HEX INTO DATA(WA_CONT_HEX).
        E_XSTRING = E_XSTRING && WA_CONT_HEX-LINE.
      ENDLOOP.

      CALL FUNCTION 'SSFC_BASE64_ENCODE'
        EXPORTING
          BINDATA                  = E_XSTRING
*         BINLENG                  =
        IMPORTING
          B64DATA                  = WA_ANEXOS-FILE_CONTENT
        EXCEPTIONS
          SSF_KRN_ERROR            = 1
          SSF_KRN_NOOP             = 2
          SSF_KRN_NOMEMORY         = 3
          SSF_KRN_OPINV            = 4
          SSF_KRN_INPUT_DATA_ERROR = 5
          SSF_KRN_INVALID_PAR      = 6
          SSF_KRN_INVALID_PARLEN   = 7
          OTHERS                   = 8.

      APPEND WA_ANEXOS TO OBJETO->ATTACHMENTS.

    ENDLOOP.

    "Criar WorkFlowSE
    TRY .

        ZCL_SOFT_EXPERT_WORKFLOW=>ZIF_SOFT_EXPERT_WORKFLOW~GET_INSTANCE(
          )->SET_PROCESS_WORKFLOW_INJECT( I_INJECT = OBJETO
          )->SET_ATTACHMENT( I_ATTACHMENTS = OBJETO->ATTACHMENTS
          )->CREATE_NEW_WORKFLOW(
             EXPORTING I_NEW_WORKFLOW = VALUE #( STATUS    = ''
                                                 CODE      = ''
                                                 DETAIL    = ''
                                                 RECORDKEY = ''
                                                 RECORDID  = '' )
             IMPORTING E_NEW_WORKFLOW = DATA(E_NEW_WORKFLOW)
        ).

        "Retorno.
        IF  E_NEW_WORKFLOW IS NOT INITIAL.
          DATA(STATUS) =  E_NEW_WORKFLOW-STATUS.
          DATA(CODE) =  E_NEW_WORKFLOW-CODE.
          DATA(DETAIL) =  E_NEW_WORKFLOW-DETAIL.
          DATA(RECORDKEY) =  E_NEW_WORKFLOW-RECORDKEY.
          E_RECORDID =  E_NEW_WORKFLOW-RECORDID.
        ENDIF.

        IF E_RECORDID IS NOT INITIAL.
          E_MSG = 'Workflow SFC criado com sucesso'.
        ENDIF.

      CATCH ZCX_SOFT_EXPERT_WORKFLOW INTO EX_ERRO.
        EX_ERRO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.

  ENDMETHOD.


  METHOD L_CHECK_EMPRESA.

    "Selecionando descrição empresa.
    SELECT SINGLE BUTXT FROM T001 INTO I_BUTXT WHERE BUKRS EQ E_BUKRS.
  ENDMETHOD.


  METHOD L_TOTALIZAR_VALOR_SOLIC.

    "Selecionar informações.
    SELECT * FROM ZFIT0046 INTO TABLE @DATA(T_ZFIT0046) WHERE NRO_SOL EQ @E_NRO_SOL.

    IF T_ZFIT0046 IS NOT INITIAL.
      LOOP AT T_ZFIT0046 ASSIGNING FIELD-SYMBOL(<L_ZFIT0046>).
        ADD <L_ZFIT0046>-VLR_ADIANTAMENTO TO I_VLR_ADIANTAMENTO.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

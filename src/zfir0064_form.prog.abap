*----------------------------------------------------------------------*
***INCLUDE ZSDR0051_FORM .
*----------------------------------------------------------------------*


FORM SELECIONA_DADOS .

  PERFORM LIMPA_DADOS.
  PERFORM MONTA_RANGES.

  SELECT *
    FROM ZIB_NFE_FORN
    INTO TABLE IT_ZIB_NFE_FORN
   WHERE INC_MANUAL      EQ 'X'
     AND BUKRS           IN P_BUKRS
     AND BRANCH          IN P_BRANCH
     AND NU_CHAVE_CNPJ   IN P_CNPJ
     AND NU_CHAVE_MODELO IN P_MODELO
     AND DT_EMISSAO      IN P_DT_EMISSAO
     AND NU_CHAVE_NUMERO IN P_NUMERO.

  IF IT_ZIB_NFE_FORN[] IS INITIAL.
    MESSAGE 'Dados não encontrados!' TYPE 'S'.
    RETURN.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS

FORM PROCESSA_DADOS .

  LOOP AT IT_ZIB_NFE_FORN INTO WA_ZIB_NFE_FORN.

    CLEAR: WA_SAIDA_XML.

    MOVE-CORRESPONDING WA_ZIB_NFE_FORN TO WA_SAIDA_XML.

    APPEND WA_SAIDA_XML TO IT_SAIDA_XML.

  ENDLOOP.

ENDFORM.                    " PROCESSA_DADOS


FORM LIMPA_DADOS .

  REFRESH: IT_ZIB_NFE_FORN,
           IT_ZIB_NFE_DIST_TER,
           IT_SAIDA_XML.

  CLEAR: WA_ZIB_NFE_FORN,
         WA_ZIB_NFE_DIST_TER.

ENDFORM.                    " LIMPA_DADOS


FORM CRIAR_FIELD_CATALOG_XML .

  FREE: WA_FCAT, IT_FCAT.

  PERFORM ESTRUTURA_ALV USING:

      0  ''  ''   'IT_SAIDA_XML' 'NU_CHAVE_CNPJ'    'CNPJ Fornecedor'  '15'  ' '    '' ' ' ' ' ' ',
      1  ''  ''   'IT_SAIDA_XML' 'NU_CHAVE_MODELO'  'Modelo'           '06'  ' '    '' ' ' ' ' ' ',
      2  ''  ''   'IT_SAIDA_XML' 'NU_CHAVE_SERIE'   'Série'            '05'  ' '    '' ' ' ' ' ' ',
      3  ''  ''   'IT_SAIDA_XML' 'NU_CHAVE_NUMERO'  'Número'           '10'  ' '    '' ' ' ' ' ' ',
      3  ''  ''   'IT_SAIDA_XML' 'DT_EMISSAO'       'Data Emissão'     '12'  ' '    '' ' ' ' ' ' ',
      3  ''  ''   'IT_SAIDA_XML' 'NU_CHAVE'         'Chave'            '44'  ' '    '' ' ' ' ' ' ',
      3  ''  ''   'IT_SAIDA_XML' 'NU_IE'            'IE'               '16'  ' '    '' ' ' ' ' ' ',
      3  ''  ''   'IT_SAIDA_XML' 'BUKRS'            'Empresa'          '07'  ' '    '' ' ' ' ' ' ',
      3  ''  ''   'IT_SAIDA_XML' 'BRANCH'           'Filial'           '06'  ' '    '' ' ' ' ' ' '.


ENDFORM.                    " CRIAR_FIELD_CATALOG_XML

FORM ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
                         VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                         VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                         VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                         VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                         VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                         VALUE(P_OUTPUTLEN)
                         VALUE(P_EDIT)
                         VALUE(P_SUM)
                         VALUE(P_EMPHASIZE)
                         VALUE(P_JUST)
                         VALUE(P_HOTSPOT).

  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME   = P_FIELD.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-REF_TABLE   = P_REF_TABNAME.
  WA_FCAT-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCAT-KEY         = ' '.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-COL_POS     = P_COL_POS.
  WA_FCAT-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCAT-NO_OUT      = ' '.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV

FORM REFRESH_OBJETOS .
  CLEAR: GS_LAYOUT,
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.

ENDFORM.                    " REFRESH_OBJETOS


FORM PREENCHE_CAMINHO.

  WA_PATH-P_INPUT = 'C:\Amaggi\InputXMLForn\'.

ENDFORM.                    " PREENCHE_CAMINHO


FORM LER_DIRETORIO .

  DATA: V_INDEX         TYPE SY-TABIX,
        V_MASK_UNIX     TYPE EPSFILNAM,
        V_MASK_LOCL(60) TYPE C,
        V_IMPORTADO,
        V_MSG           TYPE TY_CHAVES_ERRO-MSG.

  CHECK: WA_PATH-P_INPUT IS NOT INITIAL.

  V_PREFIX_ENT = C_ALL.

  VG_BLOQ_FILIAL = 'X'.

** Processa arquivos de origem UNIX
*  IF R_UNIX = C_X.
*
*    CONCATENATE V_PREFIX_ENT C_MASK_UNIX INTO V_MASK_UNIX.
*
*    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
*      EXPORTING
*        DIR_NAME               = WA_PATH-P_INPUT
*        FILE_MASK              = V_MASK_UNIX
*      TABLES
*        DIR_LIST               = T_DIR_UNIX
*      EXCEPTIONS
*        INVALID_EPS_SUBDIR     = 1
*        SAPGPARAM_FAILED       = 2
*        BUILD_DIRECTORY_FAILED = 3
*        NO_AUTHORIZATION       = 4
*        READ_DIRECTORY_FAILED  = 5
*        TOO_MANY_READ_ERRORS   = 6
*        EMPTY_DIRECTORY_LIST   = 7
*        OTHERS                 = 8.
*
*    IF SY-SUBRC <> 0 OR T_DIR_UNIX[] IS INITIAL.
*
*      MESSAGE W899(FI)
*      WITH 'Diretório Unix: ' WA_PATH-P_INPUT
*           ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
*           V_MASK_UNIX
*      INTO V_MENSAGEM.
*
*      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
*
*    ELSE.
*
**     Consiste arquivo apto para processamento
*      LOOP AT T_DIR_UNIX INTO WA_FILES_UNIX.
*        PERFORM CARREGA_ARQ USING WA_FILES_UNIX-NAME C_U.
*        IF NOT VG_ERRO IS INITIAL.
*          V_ERRO_LOG = C_X.
*        ENDIF.
*      ENDLOOP.
*
*    ENDIF.
*
*  ELSEIF R_LOCAL = C_X.

  CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
    EXPORTING
      DIRECTORY  = WA_PATH-P_INPUT
      FILTER     = C_MASK_LOC
    TABLES
      FILE_TABLE = T_DIR_LOC_F
      DIR_TABLE  = T_DIR_LOCAL
    EXCEPTIONS
      CNTL_ERROR = 1
      OTHERS     = 2.

  IF SY-SUBRC <> 0 OR T_DIR_LOC_F[] IS INITIAL.

    MESSAGE W899(FI)
       WITH 'Diretório Local: ' WA_PATH-P_INPUT
            ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
            C_MASK_LOC
       INTO V_MENSAGEM.

    MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.

  ELSE.
*     Consiste arquivo apto para processamento
    CLEAR: TG_CHAVES_ERRO[].
    LOOP AT T_DIR_LOC_F INTO WA_FILES_DOC.
      CLEAR: V_FILE_AUX, V_FILE_AUX2, V_IMPORTADO.
      PERFORM CARREGA_ARQ USING WA_FILES_DOC-PATHNAME C_L.

      PERFORM LER_ARQUIVO USING IT_XML_FORN.
      PERFORM COMPLETA_CAMPOS.

      IF WA_PATH-P_IMP_TODOS IS NOT INITIAL.
        PERFORM GRAVAR_DOC CHANGING V_IMPORTADO
                                    V_MSG.
        IF ( V_IMPORTADO IS INITIAL ) AND
           ( WA_DOC_ELET-NU_CHAVE IS NOT INITIAL ).

          TG_CHAVES_ERRO-CHAVE = WA_DOC_ELET-NU_CHAVE.
          TG_CHAVES_ERRO-MSG   = V_MSG.
          APPEND TG_CHAVES_ERRO.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.

  "ENDIF.

  IF WA_PATH-P_IMP_TODOS IS INITIAL.

    MESSAGE S899(FI) WITH 'Arquivo processado!' DISPLAY LIKE C_S.

  ELSE.
    IF TG_CHAVES_ERRO[] IS INITIAL.
      MESSAGE S899(FI) WITH 'Arquivos processados!'
                         DISPLAY LIKE C_S.
      LEAVE TO SCREEN 0.
    ENDIF.

    WRITE: 'Registros não processados:'.

    LOOP AT TG_CHAVES_ERRO.
      WRITE: TG_CHAVES_ERRO-CHAVE.
    ENDLOOP.

    PERFORM EXIBE_NAO_PROCESSADOS.



  ENDIF.




ENDFORM.                    " LE_DIRETORIO


FORM CARREGA_ARQ  USING   V_FILE V_TIPO.
  DATA: V_SIZE       TYPE I,
        VL_ARQUIVO   TYPE STRING,
        T_XML        TYPE TABLE OF ZEXML,
        ST_XML       TYPE ZEXML,
        ST_XML_FERRO TYPE ZEXML_FERRO.


  CLEAR : ST_XML_FERRO, ST_XML.
  REFRESH : IT_XML_FORN.

  CONCATENATE WA_PATH-P_INPUT V_FILE INTO VL_ARQUIVO.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME                = VL_ARQUIVO
      FILETYPE                = 'BIN'
    IMPORTING
      FILELENGTH              = V_SIZE
    CHANGING
      DATA_TAB                = T_XML
    EXCEPTIONS
      FILE_READ_ERROR         = 3
      INVALID_TYPE            = 4
      NO_BATCH                = 5
      GUI_REFUSE_FILETRANSFER = 7
      OTHERS                  = 99.

  READ TABLE T_XML INTO ST_XML INDEX 1.
  WA_XML_FORN = ST_XML-XML.

  APPEND WA_XML_FORN TO IT_XML_FORN.

ENDFORM.                    " CARREGA_ARQ

FORM LER_ARQUIVO  USING  LT_DATA.


  DATA : LCL_XML_DOC TYPE REF TO CL_XML_DOCUMENT,
         V_NODE      TYPE REF TO IF_IXML_NODE,
         V_ITERATOR  TYPE REF TO IF_IXML_NODE_ITERATOR,
         V_NODEMAP   TYPE REF TO IF_IXML_NAMED_NODE_MAP,
         V_ATTR      TYPE REF TO IF_IXML_NODE,
         V_COUNT     TYPE I,
         V_INDEX     TYPE I,
         V_NAME      TYPE STRING,
         V_PREFIX    TYPE STRING,
         V_VALUE     TYPE STRING,
         V_TAG       TYPE STRING,
         V_SUB_TAG   TYPE STRING,
         V_SUB       TYPE CHAR1,
         V_CHAR      TYPE CHAR2,
         V_SUBRC     TYPE SYSUBRC,
         VL_PARENT   TYPE STRING,
         VL_DATA_FMT TYPE STRING,
         VL_HORA_FMT TYPE STRING,
         VL_CHAVE    TYPE STRING,
         VL_ITEM     TYPE I.

  CLEAR: WA_DOC_ELET, WA_DOC_ELET_ITM, VL_DATA_FMT, VL_HORA_FMT.
  REFRESH: IT_DOC_ELET_ITM.

  CREATE OBJECT LCL_XML_DOC.

  CALL METHOD LCL_XML_DOC->PARSE_TABLE
    EXPORTING
      TABLE   = LT_DATA
    RECEIVING
      RETCODE = V_SUBRC.

  CHECK V_SUBRC = 0.

  V_NODE = LCL_XML_DOC->M_DOCUMENT.

  CHECK NOT V_NODE IS INITIAL.

  V_ITERATOR = V_NODE->CREATE_ITERATOR( ).
  V_NODE = V_ITERATOR->GET_NEXT( ).

  WHILE NOT V_NODE IS INITIAL.

    CASE V_NODE->GET_TYPE( ).

      WHEN IF_IXML_NODE=>CO_NODE_ELEMENT.

        V_NAME = V_NODE->GET_NAME( ).
        V_NODEMAP = V_NODE->GET_ATTRIBUTES( ).

        CASE V_NAME.
            "Determinação de Parents para leitura
          WHEN: 'infNFe' OR 'infCte'  OR 'ide'     OR 'dest'   OR 'emit'  OR 'det' OR
                'prod'   OR 'imposto' OR 'ICMS'     OR 'IPI'   OR 'PIS'   OR
                'PISST'  OR 'COFINS'  OR 'COFINSST' OR 'total' OR 'fat'   OR 'retirada'.
            VL_PARENT = V_NAME.
        ENDCASE.

        IF NOT V_NODEMAP IS INITIAL.

          "Leitura de Atributos
          V_COUNT = V_NODEMAP->GET_LENGTH( ).

          DO V_COUNT TIMES.

            V_INDEX = SY-INDEX - 1.
            V_ATTR = V_NODEMAP->GET_ITEM( V_INDEX ).
            V_NAME = V_ATTR->GET_NAME( ).
            V_PREFIX = V_ATTR->GET_NAMESPACE_PREFIX( ).
            V_VALUE = V_ATTR->GET_VALUE( ).
            V_TAG = V_NODE->GET_NAME( ).

            CASE VL_PARENT.
              WHEN 'infNFe' OR 'infCte'.
                CASE V_NAME.
                  WHEN 'Id'.
                    IF STRLEN( V_VALUE ) >= 47.
                      VL_CHAVE = V_VALUE+3(44).
                      PERFORM ATRIB_VALOR USING WA_DOC_ELET-NU_CHAVE VL_CHAVE.
                    ENDIF.
                ENDCASE.
              WHEN 'det'.
                CASE V_NAME.
                  WHEN 'nItem'.
                    IF VL_ITEM IS INITIAL.
                      VL_ITEM = V_VALUE.
                      CLEAR: WA_DOC_ELET_ITM.
                      WA_DOC_ELET_ITM-CHAVE_NFE = WA_DOC_ELET-NU_CHAVE.
                      WA_DOC_ELET_ITM-PROD_ITEM = VL_ITEM.
                    ELSE.
                      APPEND WA_DOC_ELET_ITM TO IT_DOC_ELET_ITM.
                      VL_ITEM = V_VALUE.
                      CLEAR: WA_DOC_ELET_ITM.
                      WA_DOC_ELET_ITM-CHAVE_NFE = WA_DOC_ELET-NU_CHAVE.
                      WA_DOC_ELET_ITM-PROD_ITEM = VL_ITEM.
                    ENDIF.
                ENDCASE.
            ENDCASE.

          ENDDO.

          V_VALUE = V_NODE->GET_VALUE( ).

          "Leitura de Childs
          CASE VL_PARENT.
            WHEN 'ide'.
              CASE V_NAME.
                WHEN 'dhEmi' OR 'dEmi'.
                  IF WA_DOC_ELET-DT_EMISSAO IS INITIAL.

                    CONCATENATE V_VALUE(4)
                                V_VALUE+5(02)
                                V_VALUE+8(02) INTO VL_DATA_FMT.

                    PERFORM ATRIB_VALOR USING WA_DOC_ELET-DT_EMISSAO VL_DATA_FMT.

                    IF V_NAME EQ 'dhEmi'.
                      CONCATENATE V_VALUE+11(2)
                                  V_VALUE+14(02)
                                  V_VALUE+17(02) INTO VL_HORA_FMT.
                      PERFORM ATRIB_VALOR USING WA_DOC_ELET-HR_EMISSAO VL_HORA_FMT.
                    ENDIF.

                  ENDIF.
                WHEN 'natOp'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-DS_NAT_OPERACAO V_VALUE.
                WHEN 'indPag'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-CD_FORM_PAG V_VALUE.
                WHEN 'tpNF'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-CD_TIPO_DOC V_VALUE.
                WHEN 'tpEmis'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-CD_FORM_EMISSAO V_VALUE.
                WHEN 'dhSaiEnt' OR 'dSaiEnt'.
                  IF WA_DOC_ELET-DT_SAIDA IS INITIAL.

                    CONCATENATE V_VALUE(4)
                                V_VALUE+5(02)
                                V_VALUE+8(02) INTO VL_DATA_FMT.

                    PERFORM ATRIB_VALOR USING WA_DOC_ELET-DT_SAIDA VL_DATA_FMT.

                    IF V_NAME EQ 'dhSaiEnt'.
                      CONCATENATE V_VALUE+11(2)
                                  V_VALUE+14(02)
                                  V_VALUE+17(02) INTO VL_HORA_FMT.

                      PERFORM ATRIB_VALOR USING WA_DOC_ELET-HR_SAIDA VL_HORA_FMT.
                    ENDIF.

                  ENDIF.
                WHEN 'finNFe'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-CD_FINA_EMISSAO V_VALUE.
              ENDCASE.
            WHEN 'emit'.
              CASE V_NAME.
                WHEN 'CPF'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-FORNE_CPF V_VALUE.
                WHEN 'IE'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-NU_IE    V_VALUE.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-FORNE_IE V_VALUE.
                WHEN 'xNome'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-FORNE_RAZAO V_VALUE.
              ENDCASE.
            WHEN 'dest'.
              CASE V_NAME.
                WHEN 'CNPJ'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-NU_CNPJ_DEST V_VALUE.
                WHEN 'IE'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-DESTINO_IE V_VALUE.
                WHEN 'UF'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-UF_DESTINO V_VALUE.
              ENDCASE.
            WHEN 'prod'.
              CASE V_NAME.
                WHEN 'cProd'.

                WHEN 'cEAN'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_EAN V_VALUE.
                WHEN 'xProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_DESCRICAO V_VALUE.
                WHEN 'NCM'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_NCM V_VALUE.
                WHEN 'CFOP'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_CFOP V_VALUE.
                WHEN 'EXTIPI'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_EXTIPI V_VALUE.
                WHEN 'uCom'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_UND_COMERCI V_VALUE.
                WHEN 'qCom'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_QTD_COMERCI V_VALUE.
                WHEN 'vUnCom'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VLR_UND_COM V_VALUE.
                WHEN 'vProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VLR_TOTAL_B V_VALUE.
                WHEN 'cEANTrib'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_EAN_TRIB V_VALUE.
                WHEN 'uTrib'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_UND_TRIB V_VALUE.
                WHEN 'qTrib'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_QTD_TRIB V_VALUE.
                WHEN 'vUnTrib'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VLR_UND_TRI V_VALUE.
                WHEN 'vFrete'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VL_FRETE V_VALUE.
                WHEN 'vSeg'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VL_SEGURO V_VALUE.
                WHEN 'vDesc'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VL_DESCONTO V_VALUE.
                WHEN 'vOutro'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_VL_OUTRO V_VALUE.
                WHEN 'indTot'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_IND_TOTAL V_VALUE.
                WHEN 'xPed'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_PEDIDO_COMP V_VALUE.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-NR_PED_COMPRA V_VALUE.
                WHEN 'nItemPed'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PROD_NR_PED_COMP V_VALUE.
              ENDCASE.

            WHEN 'ICMS'.

              CASE V_NAME.
                WHEN 'orig'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ORIGEM_MEC V_VALUE.
                WHEN 'CST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_CST V_VALUE.
                WHEN 'modBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_MD_BASE V_VALUE.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_BASE V_VALUE.
                WHEN 'pICMS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_AQT V_VALUE.
                WHEN 'vICMS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_VALOR V_VALUE.
                WHEN 'pRedBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_RED_BASE V_VALUE.
                  "ICMS ST ----------------------------------------------------------
                WHEN 'modBCST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ST_MD_BASE V_VALUE.
                WHEN 'pMVAST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ST_MARGEM V_VALUE.
                WHEN 'pRedBCST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ST_RED_BASE V_VALUE.
                WHEN 'vBCST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ST_BASE V_VALUE.
                WHEN 'pICMSST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ST_AQT V_VALUE.
                WHEN 'vICMSST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_ST_VALOR V_VALUE.

                  "ICMS Desonerado
                WHEN 'vICMSDeson'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_VL_DESONERADO V_VALUE.
                WHEN 'motDesICMS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-ICMS_MT_DESONERA V_VALUE.

              ENDCASE.

            WHEN 'IPI'.
              CASE V_NAME.
                WHEN 'cEnq'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_COD_ENQUADRA  V_VALUE.
                WHEN 'clEnq'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_CLA_ENQUADRA  V_VALUE.
                WHEN 'CNPJProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_CNPJ_PROD  V_VALUE.
                WHEN 'cSelo'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_COD_SELO_CON  V_VALUE.
                WHEN 'qSelo'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_QTD_SELO_CON  V_VALUE.
                WHEN 'CST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_CST  V_VALUE.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_BASE  V_VALUE.
                WHEN 'qUnid'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_QTD_TRIBUTAD  V_VALUE.
                WHEN 'vUnid'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_VLR_UNITARIO  V_VALUE.
                WHEN 'pIPI'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_AQT  V_VALUE.
                WHEN 'vIPI'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-IPI_VALOR V_VALUE.
              ENDCASE.

            WHEN 'PIS'.
              CASE V_NAME.
                WHEN 'CST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_CST  V_VALUE.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_BASE  V_VALUE.
                WHEN 'pPIS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_AQT  V_VALUE.
                WHEN 'vPIS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_VALOR  V_VALUE.
                WHEN 'qBCProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_QTD_VENDIDA  V_VALUE.
                WHEN 'vAliqProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_AQT_REAIS V_VALUE.
              ENDCASE.
            WHEN 'PISST'.
              CASE V_NAME.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_ST_BASE V_VALUE.
                WHEN 'pPIS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_ST_AQT  V_VALUE.
                WHEN 'vPIS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_ST_VALOR  V_VALUE.
                WHEN 'qBCProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_ST_QTD_VENDI  V_VALUE.
                WHEN 'vAliqProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-PIS_ST_AQT_REAIS V_VALUE.
              ENDCASE.
            WHEN 'COFINS'.
              CASE V_NAME.
                WHEN 'CST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_CST V_VALUE.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_BASE V_VALUE.
                WHEN 'pCOFINS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_AQT V_VALUE.
                WHEN 'vCOFINS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_VALOR V_VALUE.
                WHEN 'qBCProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_QTD_VENDIDA V_VALUE.
                WHEN 'vAliqProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_AQT_REAIS V_VALUE.
              ENDCASE.
            WHEN 'COFINSST'.
              CASE V_NAME.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_ST_BASE V_VALUE.
                WHEN 'pCOFINS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_ST_AQT V_VALUE.
                WHEN 'qBCProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_ST_QTD_VENDI V_VALUE.
                WHEN 'vAliqProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_ST_AQT_REAIS V_VALUE.
                WHEN 'vCOFINS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET_ITM-COF_ST_VALOR V_VALUE.
              ENDCASE.
            WHEN 'total'.
              CASE V_NAME.
                WHEN 'vBC'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_ICMS_BASE V_VALUE.
                WHEN 'vICMSDeson'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_ICMS_DESONERADO V_VALUE.
                WHEN 'vICMS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_ICMS_TOTAL V_VALUE.
                WHEN 'vBCST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_ICMS_ST_BASE V_VALUE.
                WHEN 'vST'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_ICMS_ST_TOTAL V_VALUE.
                WHEN 'vProd'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_PRODUTOS V_VALUE.
                WHEN 'vFrete'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_FRETE V_VALUE.
                WHEN 'vSeg'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_SEGURO V_VALUE.
                WHEN 'vDesc'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_DESCONTO V_VALUE.
                WHEN 'vIPI'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_IPI_TOTAL V_VALUE.
                WHEN 'vPIS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_PIS_TOTAL V_VALUE.
                WHEN 'vCOFINS'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_COF_TOTAL V_VALUE.
                WHEN 'vNF'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_TOTAL V_VALUE.
              ENDCASE.

            WHEN 'fat'.
              CASE V_NAME.
                WHEN 'nFat'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-NR_FATURA V_VALUE.
                WHEN 'vOrig'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_TOTAL_FATURA V_VALUE.
                WHEN 'vDesc'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_DESCO_FATURA V_VALUE.
                WHEN 'vLiq'.
                  PERFORM ATRIB_VALOR USING WA_DOC_ELET-VL_LIQUIDO V_VALUE.

              ENDCASE.
          ENDCASE.

        ENDIF. "IF NOT V_NODEMAP IS INITIAL.

    ENDCASE.

    V_NODE = V_ITERATOR->GET_NEXT( ).

  ENDWHILE.

  IF WA_DOC_ELET_ITM IS NOT INITIAL.
    APPEND WA_DOC_ELET_ITM TO IT_DOC_ELET_ITM.
  ENDIF.

ENDFORM.                    " LER_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_XML_FORN  text
*----------------------------------------------------------------------*
FORM COMPLETA_CAMPOS.

  DATA: WL_BRANCH_DETAIL TYPE BAPIBRANCH.

  REFRESH: IT_J_1BBRANCH.

  IF WA_DOC_ELET-NU_CHAVE IS NOT INITIAL.

    WA_DOC_ELET-NU_CHAVE_REGIAO   = WA_DOC_ELET-NU_CHAVE(2).
    WA_DOC_ELET-NU_CHAVE_ANO      = WA_DOC_ELET-NU_CHAVE+2(2).
    WA_DOC_ELET-NU_CHAVE_MES      = WA_DOC_ELET-NU_CHAVE+4(2).
    WA_DOC_ELET-NU_CHAVE_CNPJ     = WA_DOC_ELET-NU_CHAVE+6(14).
    WA_DOC_ELET-NU_CHAVE_MODELO   = WA_DOC_ELET-NU_CHAVE+20(2).
    WA_DOC_ELET-NU_CHAVE_SERIE    = WA_DOC_ELET-NU_CHAVE+22(3).
    WA_DOC_ELET-NU_CHAVE_NUMERO   = WA_DOC_ELET-NU_CHAVE+25(9).
    WA_DOC_ELET-NU_CHAVE_ALEATOR  = WA_DOC_ELET-NU_CHAVE+34(9).
    WA_DOC_ELET-NU_CHAVE_DV       = WA_DOC_ELET-NU_CHAVE+43(1).

    "" Busca Local de Negócio """"""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF WA_DOC_ELET-NU_CNPJ_DEST IS NOT INITIAL.

      SELECT BRANCH STATE_INSC BUKRS
        INTO TABLE IT_J_1BBRANCH
        FROM J_1BBRANCH.

      LOOP AT IT_J_1BBRANCH.
        CLEAR: WL_BRANCH_DETAIL, IT_J_1BBRANCH-STCD1.

        CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
          EXPORTING
            COMPANY       = IT_J_1BBRANCH-BUKRS
            BRANCH        = IT_J_1BBRANCH-BRANCH
          IMPORTING
            BRANCH_DETAIL = WL_BRANCH_DETAIL.

        IF WL_BRANCH_DETAIL-CGC_NUMBER IS NOT INITIAL.
          IT_J_1BBRANCH-STCD1 = WL_BRANCH_DETAIL-CGC_NUMBER.
        ENDIF.

        MODIFY IT_J_1BBRANCH.
      ENDLOOP.

      DELETE IT_J_1BBRANCH WHERE STCD1 NE WA_DOC_ELET-NU_CNPJ_DEST.

      IF IT_J_1BBRANCH[] IS NOT INITIAL.

        READ TABLE IT_J_1BBRANCH INTO WA_J_1BBRANCH INDEX 1.

        IF LINES( IT_J_1BBRANCH ) = 1.
          WA_DOC_ELET-BUKRS       = WA_J_1BBRANCH-BUKRS.
          WA_DOC_ELET-DESTINO_IE  = WA_J_1BBRANCH-STATE_INSC.
          WA_DOC_ELET-BRANCH      = WA_J_1BBRANCH-BRANCH.
          VG_BLOQ_FILIAL = 'X'.
        ELSE.
          VG_BLOQ_FILIAL = ''.
        ENDIF.

      ENDIF.

    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  ENDIF.


ENDFORM.                    " COMPLETA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  STATUS_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0120   text
*----------------------------------------------------------------------*
FORM STATUS_FILIAL  USING P_BLOQUEIO.

*  LOOP AT SCREEN.
*
*    CASE SCREEN-NAME.
*      WHEN 'BTN_PSQ_FILIAL'.
*
*        IF P_BLOQUEIO IS NOT INITIAL.
*          SCREEN-ACTIVE = 0.
*        ELSE.
*          SCREEN-ACTIVE = 1.
*        ENDIF.
*
*        MODIFY SCREEN.
*
*    ENDCASE.
*
*  ENDLOOP.

ENDFORM.                    " STATUS_FILIAL
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVAR_DOC CHANGING P_IMPORTADO TYPE C
                         P_MESSAGE   TYPE TY_CHAVES_ERRO-MSG.

  DATA: WA_ZIB_NFE_FORN_AUX TYPE ZIB_NFE_FORN,
        WA_ZIB_DIST_TER_AUX TYPE ZIB_NFE_DIST_TER,
        WA_ZIB_DIST_ITM_AUX TYPE ZIB_NFE_DIST_ITM.

  CLEAR: P_IMPORTADO, P_MESSAGE.

  IF WA_DOC_ELET-NU_CHAVE IS INITIAL.
    P_MESSAGE = 'Chave não encontrada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_CNPJ IS INITIAL.
    P_MESSAGE = 'CNPJ Fornecedor não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_MODELO IS INITIAL.
    P_MESSAGE = 'Modelo não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_SERIE  IS INITIAL.
    P_MESSAGE = 'Série não encontrada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_NUMERO IS INITIAL.
    P_MESSAGE = 'Número não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-DT_EMISSAO IS INITIAL.
    P_MESSAGE = 'Data Emissão não encontrada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_REGIAO  IS INITIAL.
    P_MESSAGE = 'Região não encontrada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_ANO  IS INITIAL.
    P_MESSAGE = 'Ano não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_MES  IS INITIAL.
    P_MESSAGE = 'Mês não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_ALEATOR  IS INITIAL.
    P_MESSAGE = 'Aleatório não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_DV  IS INITIAL.
    P_MESSAGE = 'DV não encontrado!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( WA_DOC_ELET-BUKRS IS INITIAL ) AND ( WA_DOC_ELET-UF_DESTINO NE 'EX' ).
    P_MESSAGE = 'Empresa não encontrada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( WA_DOC_ELET-BRANCH IS INITIAL ) AND ( WA_DOC_ELET-UF_DESTINO NE 'EX' ).
    P_MESSAGE = 'Filial não encontrada/informada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_IE IS INITIAL.
    P_MESSAGE = 'Inscrição Estadual Fornecedor não encontrada/informada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( WA_DOC_ELET-DESTINO_IE IS INITIAL ) AND ( WA_DOC_ELET-UF_DESTINO NE 'EX' ).
    P_MESSAGE = 'Inscrição Estadual Destinatário não encontrada/informada!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE: WA_DOC_ELET-NU_CHAVE_CNPJ      TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_CNPJ,
        WA_DOC_ELET-NU_CHAVE_MODELO    TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_MODELO,
        WA_DOC_ELET-NU_CHAVE_SERIE     TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_SERIE,
        WA_DOC_ELET-NU_CHAVE_NUMERO    TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_NUMERO,
        '1'                            TO    WA_ZIB_NFE_FORN_AUX-ST_NOTA,
        WA_DOC_ELET-DT_EMISSAO         TO    WA_ZIB_NFE_FORN_AUX-DT_EMISSAO,
        WA_DOC_ELET-NU_CHAVE_REGIAO    TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_REGIAO,
        WA_DOC_ELET-NU_CHAVE_ANO       TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_ANO,
        WA_DOC_ELET-NU_CHAVE_MES       TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_MES,
        WA_DOC_ELET-NU_CHAVE_ALEATOR   TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_ALEATOR,
        WA_DOC_ELET-NU_CHAVE_DV        TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE_DV,
        WA_DOC_ELET-NU_CHAVE           TO    WA_ZIB_NFE_FORN_AUX-NU_CHAVE,
        '100'                          TO    WA_ZIB_NFE_FORN_AUX-NU_CODE,
        WA_DOC_ELET-NU_IE              TO    WA_ZIB_NFE_FORN_AUX-NU_IE,
        WA_DOC_ELET-BUKRS              TO    WA_ZIB_NFE_FORN_AUX-BUKRS,
        WA_DOC_ELET-BRANCH             TO    WA_ZIB_NFE_FORN_AUX-BRANCH,
        'X'                            TO    WA_ZIB_NFE_FORN_AUX-INC_MANUAL.

  MODIFY ZIB_NFE_FORN FROM WA_ZIB_NFE_FORN_AUX.

  IF SY-SUBRC <> 0.
    ROLLBACK WORK.
    P_MESSAGE = 'Houve um erro ao importar o XML!'.
    MESSAGE P_MESSAGE TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_DOC_ELET-NU_CHAVE_MODELO = '55'.

    LOOP AT IT_DOC_ELET_ITM INTO WA_DOC_ELET_ITM.

      WA_DOC_ELET_ITM-INC_MANUAL = 'X'.
      MODIFY IT_DOC_ELET_ITM FROM WA_DOC_ELET_ITM.

      MODIFY ZIB_NFE_DIST_ITM FROM WA_DOC_ELET_ITM.

      IF SY-SUBRC <> 0.
        ROLLBACK WORK.
        P_MESSAGE = 'Houve um erro ao importar o XML!'.
        MESSAGE P_MESSAGE TYPE 'S'.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF WA_DOC_ELET-FORNE_CPF IS INITIAL.
      MOVE: WA_DOC_ELET-NU_CHAVE_CNPJ TO WA_ZIB_DIST_TER_AUX-FORNE_CNPJ.
    ELSE.
      MOVE: WA_DOC_ELET-FORNE_CPF TO WA_ZIB_DIST_TER_AUX-FORNE_CPF.
    ENDIF.

    MOVE: WA_DOC_ELET-NU_CHAVE_MODELO       TO    WA_ZIB_DIST_TER_AUX-MODEL,
          WA_DOC_ELET-NU_CHAVE_SERIE        TO    WA_ZIB_DIST_TER_AUX-SERIE,
          WA_DOC_ELET-NU_CHAVE_NUMERO       TO    WA_ZIB_DIST_TER_AUX-NUMERO,
          WA_DOC_ELET-DT_EMISSAO            TO    WA_ZIB_DIST_TER_AUX-DT_EMISSAO,
          WA_DOC_ELET-HR_EMISSAO            TO    WA_ZIB_DIST_TER_AUX-HR_EMISSAO,
          WA_DOC_ELET-NU_CHAVE_REGIAO       TO    WA_ZIB_DIST_TER_AUX-REGIO,
          WA_DOC_ELET-NU_CHAVE_ANO          TO    WA_ZIB_DIST_TER_AUX-NFYEAR,
          WA_DOC_ELET-NU_CHAVE_MES          TO    WA_ZIB_DIST_TER_AUX-NFMONTH,
          WA_DOC_ELET-NU_CHAVE_ALEATOR      TO    WA_ZIB_DIST_TER_AUX-DOCNUM9,
          WA_DOC_ELET-NU_CHAVE_DV           TO    WA_ZIB_DIST_TER_AUX-CDV,
          WA_DOC_ELET-NU_CHAVE              TO    WA_ZIB_DIST_TER_AUX-CHAVE_NFE,
          WA_DOC_ELET-DESTINO_IE            TO    WA_ZIB_DIST_TER_AUX-DESTINO_IE,
          WA_DOC_ELET-NU_CNPJ_DEST          TO    WA_ZIB_DIST_TER_AUX-DESTINO_CNPJ,
          WA_DOC_ELET-BUKRS                 TO    WA_ZIB_DIST_TER_AUX-BUKRS,
          WA_DOC_ELET-BRANCH                TO    WA_ZIB_DIST_TER_AUX-BRANCH,
          '100'                             TO    WA_ZIB_DIST_TER_AUX-CD_MSG_SEFAZ,
          '1'                               TO    WA_ZIB_DIST_TER_AUX-DOCSTA,
          'X'                               TO    WA_ZIB_DIST_TER_AUX-INC_MANUAL,
          "Adicionais
          WA_DOC_ELET-FORNE_IE              TO    WA_ZIB_DIST_TER_AUX-FORNE_IE,
          WA_DOC_ELET-FORNE_RAZAO           TO    WA_ZIB_DIST_TER_AUX-FORNE_RAZAO,
          WA_DOC_ELET-DS_NAT_OPERACAO       TO    WA_ZIB_DIST_TER_AUX-DS_NAT_OPERACAO,
          WA_DOC_ELET-CD_FORM_PAG           TO    WA_ZIB_DIST_TER_AUX-CD_FORM_PAG,
          WA_DOC_ELET-CD_TIPO_DOC           TO    WA_ZIB_DIST_TER_AUX-CD_TIPO_DOC,
          WA_DOC_ELET-CD_FORM_EMISSAO       TO    WA_ZIB_DIST_TER_AUX-CD_FORM_EMISSAO,
          WA_DOC_ELET-DT_SAIDA              TO    WA_ZIB_DIST_TER_AUX-DT_SAIDA,
          WA_DOC_ELET-HR_SAIDA              TO    WA_ZIB_DIST_TER_AUX-HR_SAIDA,
          WA_DOC_ELET-CD_FINA_EMISSAO       TO    WA_ZIB_DIST_TER_AUX-CD_FINA_EMISSAO,
          WA_DOC_ELET-NR_PED_COMPRA         TO    WA_ZIB_DIST_TER_AUX-NR_PED_COMPRA,
          WA_DOC_ELET-NR_CTR_COMPRA         TO    WA_ZIB_DIST_TER_AUX-NR_CTR_COMPRA,
          WA_DOC_ELET-NR_FATURA             TO    WA_ZIB_DIST_TER_AUX-NR_FATURA,
          WA_DOC_ELET-VL_TOTAL_FATURA       TO    WA_ZIB_DIST_TER_AUX-VL_TOTAL_FATURA,
          WA_DOC_ELET-VL_DESCO_FATURA       TO    WA_ZIB_DIST_TER_AUX-VL_DESCO_FATURA,
          WA_DOC_ELET-VL_LIQUIDO            TO    WA_ZIB_DIST_TER_AUX-VL_LIQUIDO,
          WA_DOC_ELET-VL_ICMS_BASE          TO    WA_ZIB_DIST_TER_AUX-VL_ICMS_BASE,
          WA_DOC_ELET-VL_ICMS_TOTAL         TO    WA_ZIB_DIST_TER_AUX-VL_ICMS_TOTAL,
          WA_DOC_ELET-VL_ICMS_ST_BASE       TO    WA_ZIB_DIST_TER_AUX-VL_ICMS_ST_BASE,
          WA_DOC_ELET-VL_ICMS_ST_TOTAL      TO    WA_ZIB_DIST_TER_AUX-VL_ICMS_ST_TOTAL,
          WA_DOC_ELET-VL_PRODUTOS           TO    WA_ZIB_DIST_TER_AUX-VL_PRODUTOS,
          WA_DOC_ELET-VL_FRETE              TO    WA_ZIB_DIST_TER_AUX-VL_FRETE,
          WA_DOC_ELET-VL_SEGURO             TO    WA_ZIB_DIST_TER_AUX-VL_SEGURO,
          WA_DOC_ELET-VL_DESCONTO           TO    WA_ZIB_DIST_TER_AUX-VL_DESCONTO,
          WA_DOC_ELET-VL_II_TOTAL           TO    WA_ZIB_DIST_TER_AUX-VL_II_TOTAL,
          WA_DOC_ELET-VL_IPI_TOTAL          TO    WA_ZIB_DIST_TER_AUX-VL_IPI_TOTAL,
          WA_DOC_ELET-VL_PIS_TOTAL          TO    WA_ZIB_DIST_TER_AUX-VL_PIS_TOTAL,
          WA_DOC_ELET-VL_COF_TOTAL          TO    WA_ZIB_DIST_TER_AUX-VL_COF_TOTAL,
          WA_DOC_ELET-VL_DESPESAS           TO    WA_ZIB_DIST_TER_AUX-VL_DESPESAS,
          WA_DOC_ELET-VL_TOTAL              TO    WA_ZIB_DIST_TER_AUX-VL_TOTAL,
          WA_DOC_ELET-VL_ICMS_DESONERADO    to    WA_ZIB_DIST_TER_AUX-VL_ICMS_DESONERADO.

    MODIFY ZIB_NFE_DIST_TER FROM WA_ZIB_DIST_TER_AUX.

    IF SY-SUBRC <> 0.
      ROLLBACK WORK.
      P_MESSAGE = 'Houve um erro ao importar o XML!'.
      MESSAGE P_MESSAGE TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

  P_IMPORTADO = 'X'.

  COMMIT WORK.
  MESSAGE 'XML Importado com Sucesso!' TYPE 'S'.

  IF WA_PATH-P_IMP_TODOS IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

FORM MONTA_RANGES.

  REFRESH: P_BUKRS,
           P_BRANCH,
           P_CNPJ,
           P_MODELO,
           P_DT_EMISSAO,
           P_NUMERO.

  IF P_BUKRS-LOW IS NOT INITIAL.

    P_BUKRS-SIGN   = 'I'.
    P_BUKRS-OPTION = 'EQ'.
    P_BUKRS-HIGH   = P_BUKRS-LOW.
    APPEND P_BUKRS.

  ENDIF.

  IF P_BRANCH-LOW IS NOT INITIAL.

    P_BRANCH-SIGN   = 'I'.
    P_BRANCH-OPTION = 'EQ'.
    P_BRANCH-HIGH   = P_BRANCH-LOW.
    APPEND P_BRANCH.

  ENDIF.

  IF P_CNPJ-LOW IS NOT INITIAL.

    P_CNPJ-SIGN   = 'I'.
    P_CNPJ-OPTION = 'EQ'.
    P_CNPJ-HIGH   = P_CNPJ-LOW.
    APPEND P_CNPJ.

  ENDIF.

  IF P_MODELO-LOW IS NOT INITIAL.

    P_MODELO-SIGN   = 'I'.
    P_MODELO-OPTION = 'EQ'.
    P_MODELO-HIGH   = P_MODELO-LOW.
    APPEND P_MODELO.

  ENDIF.

  IF P_DT_EMISSAO-LOW IS NOT INITIAL.

    P_DT_EMISSAO-SIGN   = 'I'.
    P_DT_EMISSAO-OPTION = 'EQ'.
    P_DT_EMISSAO-HIGH   = P_DT_EMISSAO-LOW.
    APPEND P_DT_EMISSAO.

  ENDIF.

  IF P_NUMERO-LOW IS NOT INITIAL.

    P_NUMERO-SIGN   = 'I'.
    P_NUMERO-OPTION = 'EQ'.
    P_NUMERO-HIGH   = P_NUMERO-LOW.
    APPEND P_NUMERO.

  ENDIF.


ENDFORM.

FORM EXCLUIR_XML.

  DATA: VAR_ANSWER  TYPE C.

  DATA: IT_SEL_ROWS TYPE LVC_T_ROW,
        WA_SEL_ROWS TYPE LVC_S_ROW.

  DATA: WL_ZIB_NFE_FORN TYPE ZIB_NFE_FORN.


  CALL METHOD OBJ_ALV_XML->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK NOT IT_SEL_ROWS IS INITIAL.

  IF ( LINES( IT_SEL_ROWS ) NE 1 ).
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Deseja realmente excluir o registro?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = VAR_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  CHECK VAR_ANSWER EQ '1'.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
  READ TABLE IT_SAIDA_XML INTO  WA_SAIDA_XML INDEX WA_SEL_ROWS-INDEX.

  IF SY-SUBRC = 0.

    CLEAR: WL_ZIB_NFE_FORN.
    SELECT SINGLE *
      FROM ZIB_NFE_FORN INTO WL_ZIB_NFE_FORN
     WHERE INC_MANUAL       EQ 'X'
       AND NU_CHAVE_CNPJ    EQ  WA_SAIDA_XML-NU_CHAVE_CNPJ
       AND NU_CHAVE_MODELO  EQ  WA_SAIDA_XML-NU_CHAVE_MODELO
       AND NU_CHAVE_SERIE   EQ  WA_SAIDA_XML-NU_CHAVE_SERIE
       AND NU_CHAVE_NUMERO  EQ  WA_SAIDA_XML-NU_CHAVE_NUMERO.

    IF SY-SUBRC = 0.

      DELETE FROM ZIB_NFE_FORN WHERE INC_MANUAL       EQ 'X'
                                 AND NU_CHAVE         EQ WL_ZIB_NFE_FORN-NU_CHAVE.

      DELETE FROM ZIB_NFE_DIST_TER WHERE INC_MANUAL   EQ 'X'
                                     AND CHAVE_NFE    EQ WL_ZIB_NFE_FORN-NU_CHAVE.

      COMMIT WORK.

    ENDIF.

  ENDIF.

  PERFORM: SELECIONA_DADOS,
           PROCESSA_DADOS.

  CALL METHOD OBJ_ALV_XML->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

ENDFORM.

FORM ATRIB_VALOR USING P_FIELD
                       P_VALUE.

  IF P_FIELD IS INITIAL.
    P_FIELD = P_VALUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PSQ_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PSQ_FILIAL .

  DATA: LT_MAP    TYPE TABLE OF DSELC,
        LS_MAP    TYPE DSELC,
        LT_RETURN TYPE TABLE OF DDSHRETVAL,
        LS_RETURN TYPE DDSHRETVAL,
        LS_STABLE TYPE LVC_S_STBL.

  REFRESH: LT_MAP, LT_RETURN.

  "SET RETURN FIELD
  CLEAR LS_MAP.
  LS_MAP-FLDNAME = 'F0001'.
  LS_MAP-DYFLDNAME = 'BRANCH'.
  APPEND LS_MAP TO LT_MAP.

  LS_MAP-FLDNAME = 'F0003'.
  LS_MAP-DYFLDNAME = 'BUKRS'.
  APPEND LS_MAP TO LT_MAP.

  " CALL SEARCH HELP POPUP FUNCTION
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'BRANCH'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_J_1BBRANCH
      DYNPFLD_MAPPING = LT_MAP
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  CHECK LT_RETURN IS NOT INITIAL.

  " READ SELECTED F4 VALUE
  READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
  IF SY-SUBRC = 0.

    WA_DOC_ELET-BRANCH = LS_RETURN-FIELDVAL.

    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0003'.

    IF SY-SUBRC NE 0.
      CLEAR: WA_DOC_ELET-BRANCH.
      RETURN.
    ENDIF.

    WA_DOC_ELET-BUKRS = LS_RETURN-FIELDVAL.

    READ TABLE IT_J_1BBRANCH INTO WA_J_1BBRANCH WITH KEY BUKRS  = WA_DOC_ELET-BUKRS
                                                         BRANCH = WA_DOC_ELET-BRANCH.
    IF SY-SUBRC = 0.
      WA_DOC_ELET-DESTINO_IE = WA_J_1BBRANCH-STATE_INSC.
    ENDIF.

  ENDIF.

ENDFORM.

FORM EXIBE_NAO_PROCESSADOS.

  DATA: GT_OUTTAB       TYPE TABLE OF SFLIGHT,
        GR_TABLE        TYPE REF TO CL_SALV_TABLE,
        LR_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
        GR_DISPLAY      TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
        IT_CHAVES_ERRO  TYPE TABLE OF TY_CHAVES_ERRO.

  IT_CHAVES_ERRO[] = TG_CHAVES_ERRO[].
  "SELECT * FROM SFLIGHT INTO CORRESPONDING FIELDS OF TABLE GT_OUTTAB.

  CALL METHOD CL_SALV_TABLE=>FACTORY
    IMPORTING
      R_SALV_TABLE = GR_TABLE
    CHANGING
      T_TABLE      = IT_CHAVES_ERRO.

  LR_AGGREGATIONS = GR_TABLE->GET_AGGREGATIONS( ).

  GR_DISPLAY = GR_TABLE->GET_DISPLAY_SETTINGS( ).
  GR_DISPLAY->SET_LIST_HEADER( 'Registros não processados' ).

  LR_AGGREGATIONS->CLEAR( ).

  TRY.
      LR_AGGREGATIONS->ADD_AGGREGATION( COLUMNNAME = 'SEATSMAX' ).
    CATCH CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR CX_SALV_EXISTING.
  ENDTRY.

  GR_TABLE->DISPLAY( ).

ENDFORM.

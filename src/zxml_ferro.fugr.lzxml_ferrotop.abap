FUNCTION-POOL ZXML_FERRO MESSAGE-ID Z01.

TYPE-POOLS: ICON, SLIS, ZFTTE.
"---Tipos
TYPES: BEGIN OF TYPE_MSG,
         TP_MSG         TYPE BAPI_MTYPE,
         DACTE          TYPE ZLEST0006-NR_NF_ALL,
         SERIE_DESPACHO TYPE ZSRDEP,
         NR_DESPACHO    TYPE ZNRDESP,
         CGC_REMETENTE  TYPE ZCGC_REM,
         OV             TYPE VBELN_VA,
         NF             TYPE J_1BNFNUM9,
         MESSAGEM       TYPE BAPI_MSG,
         CHAVE_CTE      TYPE ZLEST0044-CHAVE_CTE,
         COD_FORNEC     TYPE LFA1-LIFNR,
       END   OF TYPE_MSG,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         STCD1 TYPE LFA1-STCD1,
       END OF TY_LFA1 ,

       BEGIN OF TY_VBFA,
         VBELV   TYPE VBFA-VBELV,
         POSNV   TYPE VBFA-POSNV,
         VBELN   TYPE VBFA-VBELN,
         POSNN   TYPE VBFA-POSNN,
         VBTYP_V TYPE VBFA-VBTYP_V,
         VBTYP_N TYPE VBFA-VBTYP_N,
       END OF TY_VBFA,

       BEGIN OF TY_DADOS,
         TDLNR            TYPE VTTK-TDLNR,                  "Fornecedor
         NAME1            TYPE LFA1-NAME1,                  "Nome do fornecedor
         SHTYP            TYPE VTTK-SHTYP,                  "Tipo de transporte
         EXTI1            TYPE VTTK-EXTI1,                  "Conhecimento de embarque
         EXTI2            TYPE VTTK-EXTI2,                  "Carta Frete
         TKNUM            TYPE ZLEST0032-TKNUM,             "Nr. doc. transporte
         FKNUM            TYPE ZLEST0032-FKNUM,             "Nr. custo de frete
         BUDAT            TYPE VFKK-BUDAT,                  "Data de Lançamento Contábil
         BUKRS            TYPE ZLEST0034-BUKRS,             "Empresa do custo de frete
         WERKS            TYPE ZLEST0034-WERKS,             "centro do custo de frete
         WAERS            TYPE ZLEST0034-WAERS,             "Moeda do custo de frete
         KBETR            TYPE ZLEST0034-KBETR,             "Montante ou porcentagem da condição
         KURST            TYPE ZLEST0034-KURST,             "Cotação da Moeda do Documento
         NFENUM           TYPE ZLEST0034-NFENUM,            "Nr. NFE
         SERIES           TYPE ZLEST0034-SERIES,            "Série conhecimento
         ZDT_MOV          TYPE ZLEST0034-ZDT_MOV,           "Data do movimento
         ZDT_VENCTO       TYPE ZLEST0034-ZDT_VENCTO,        "Data do vencimento
         NR_CONHEC        TYPE ZLEST0034-NR_CONHEC,         "Nr. do conhecimento
         ZDT_CONHEC       TYPE ZLEST0034-ZDT_CONHEC,        "Data do conhecimento
         ZPESO_DESTINO    TYPE ZLEST0034-ZPESO_DESTINO,     "Peso destino
         ZDT_CHEGADA      TYPE ZLEST0034-ZDT_CHEGADA,       "Peso chegada
         KALSM            TYPE ZLEST0034-KALSM,             "Esquema (determinação preço, mensagens, determ.contas, ...)
         IVA              TYPE ZLEST0034-IVA,               "Código do IVA
         NFE              TYPE ZLEST0034-NFE,               "Documento Eletrônico
         EBELN            TYPE ZLEST0034-EBELN,             "Pdido de compra
         EBELP            TYPE ZLEST0034-EBELP,             "iTEM Pdido de compra
         LBLNI            TYPE ZLEST0034-LBLNI,             "Folha de serviço
         LFGJA            TYPE ZLEST0034-LFGJA,             "Folha de serviço (ano)
         ZPESO_ORIGEM     TYPE ZLEST0034-ZPESO_ORIGEM,      "Peso original
         GEWEI            TYPE ZLEST0034-GEWEI,             "Unidade de medida
         DMBTR            TYPE ZLEST0034-DMBTR,             "Valor
         DMBTR_DOC        TYPE ZLEST0034-DMBTR_DOC,         "Valor Documento
         ZPESO_DIFERENCA  TYPE ZLEST0034-ZPESO_DIFERENCA,   "Diferença entre origem e destino
         ZQUEBRA          TYPE ZLEST0034-ZQUEBRA,           "Quebra do peso
         ZPERDA           TYPE ZLEST0034-ZPERDA,            "Perda de peso
         ZVLR_QUEBRA      TYPE ZLEST0034-ZVLR_QUEBRA,       "Valor da quebra
         ZVLR_PERDA       TYPE ZLEST0034-ZVLR_PERDA,        "Valor da perda
         ZVLR_LIQ_PAGAR   TYPE ZLEST0034-ZVLR_LIQ_PAGAR,    "Valor líquido a pagar
         MATNR            TYPE ZLEST0034-MATNR,             "Código do material
         BVTYP            TYPE ZLEST0034-BVTYP,             "Banco Pareiro
         MATNS            TYPE ZLEST0034-MATNS,             "Código do material Serciço
         RE_BELNR         TYPE ZLEST0034-RE_BELNR,          "Documento de Contas a Pagar MIRO
         RE_GJAHR         TYPE ZLEST0034-GJAHR,             "ANO de Contas a Pagar MIRO
         RE_ITEM          TYPE ZLEST0034-RE_ITEM,           "item de Contas a Pagar MIRO
         EN_DOCNUM        TYPE ZLEST0034-EN_DOCNUM,         "Docnum de Entrada fiscal
         REGIO_EMISSOR    TYPE ZLEST0034-REGIO_EMISSOR,
         REGIO_RECEPTOR   TYPE ZLEST0034-REGIO_RECEPTOR,
         BASE_ICMS        TYPE ZLEST0034-BASE_ICMS,
         BASE_PIS         TYPE ZLEST0034-BASE_PIS,
         BASE_COFINS      TYPE ZLEST0034-BASE_COFINS,
         RATE_ICMS        TYPE ZLEST0034-RATE_ICMS,
         RATE_PIS         TYPE ZLEST0034-RATE_PIS,
         RATE_COFINS      TYPE ZLEST0034-RATE_COFINS,
         VALOR_ICMS       TYPE ZLEST0034-VALOR_ICMS,
         VALOR_PIS        TYPE ZLEST0034-VALOR_PIS,
         VALOR_COFINS     TYPE ZLEST0034-VALOR_COFINS,
         VALOR_PEDAGIO    TYPE ZLEST0034-VALOR_PEDAGIO,
         DOCNUM           TYPE ZLEST0034-DOCNUM,
         VALOR_MERCADORIA TYPE ZLEST0034-VALOR_MERCADORIA,
       END OF TY_DADOS.

TYPES: BEGIN OF TY_BAPI_INCINV_CREATE_HEADER.
        INCLUDE STRUCTURE BAPI_INCINV_CREATE_HEADER.
TYPES:  NR_CONHEC       TYPE ZLEST0034-NR_CONHEC,            "Nr. NFE
        SERIES          TYPE ZLEST0034-SERIES.            "Série conhecimento
TYPES: END OF TY_BAPI_INCINV_CREATE_HEADER.

TYPES: BEGIN OF TY_BAPI_INCINV_CREATE_ITEM.
        INCLUDE STRUCTURE BAPI_INCINV_CREATE_ITEM.
TYPES:   REF_DOC_NO	   TYPE XBLNR,
         DIFF_INV	     TYPE LIFRE,
         ZVLR_QUEBRA   TYPE ZLEST0034-ZVLR_QUEBRA,
         ZVLR_PERDA    TYPE ZLEST0034-ZVLR_PERDA,
         DMBTR         TYPE ZLEST0034-DMBTR,
         VALOR_PEDAGIO TYPE ZLEST0034-VALOR_PEDAGIO.
TYPES: END OF TY_BAPI_INCINV_CREATE_ITEM.


TYPES: BEGIN OF TY_BAPI_INCINV_CREATE_GL_ACCO.
        INCLUDE STRUCTURE BAPI_INCINV_CREATE_GL_ACCOUNT.
TYPES:   REF_DOC_NO	 TYPE XBLNR.
TYPES: END OF TY_BAPI_INCINV_CREATE_GL_ACCO.

TYPES: BEGIN OF TY_BAPI_INCINV_CREATE_TAX.
        INCLUDE STRUCTURE BAPI_INCINV_CREATE_TAX.
TYPES:   REF_DOC_NO	 TYPE XBLNR.
TYPES: END OF TY_BAPI_INCINV_CREATE_TAX.

TYPES: BEGIN OF TY_BAPI_INCINV_CREATE_WITHTAX.
        INCLUDE STRUCTURE BAPI_INCINV_CREATE_WITHTAX.
TYPES:   REF_DOC_NO	 TYPE XBLNR.
TYPES: END OF TY_BAPI_INCINV_CREATE_WITHTAX.


"---Tabelas

DATA:  BEGIN OF TG_NODE_PATH  OCCURS 1,
          PATH TYPE STRING,
         END OF TG_NODE_PATH.

DATA: XML_TAB            TYPE TABLE OF ZXML_FERRO_TAB,
      T_ZLEST0035        TYPE TABLE OF ZLEST0035,
      T_ZLEST0044        TYPE TABLE OF ZLEST0044,
      T_ZLEST0045        TYPE TABLE OF ZLEST0045,
      T_MENSAGENS        TYPE TABLE OF TYPE_MSG,
      T_ITEMDATA         TYPE TABLE OF BAPISHIPMENTITEM,
      T_ITEMDATAACTION   TYPE TABLE OF BAPISHIPMENTITEMACTION WITH HEADER LINE,
      T_STAGEDATA        TYPE TABLE OF BAPISHIPMENTSTAGE,
      T_VBFA             TYPE TABLE OF TY_VBFA,
      T_VTTP             TYPE TABLE OF VTTP WITH HEADER LINE,
      T_VTTS             TYPE TABLE OF VTTS WITH HEADER LINE,
      T_VBPA             TYPE TABLE OF VBPA WITH HEADER LINE,
      T_ZLEST0008        TYPE TABLE OF ZLEST0008,
      T_RETURN           LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
      T_BDC              TYPE TABLE OF BDCDATA,
      T_MSG              TYPE TABLE OF BDCMSGCOLL,
      T_IMPOSTOS_RETIDOS TYPE TABLE OF ZLES0043_IMP_RET,
      T_IMPOSTOS_RET_B   TYPE TABLE OF TY_BAPI_INCINV_CREATE_WITHTAX,
      T_HEADERDATA_MIRO  TYPE TABLE OF TY_BAPI_INCINV_CREATE_HEADER,
      T_IMPOSTOS         TYPE TABLE OF TY_BAPI_INCINV_CREATE_TAX,
      T_DADOS            TYPE TABLE OF TY_DADOS,
      T_ZLEST0021        TYPE TABLE OF ZLEST0021 INITIAL SIZE 0 WITH HEADER LINE,
      T_ITEMDATA_MIRO    TYPE TABLE OF TY_BAPI_INCINV_CREATE_ITEM,
      T_GLACCOUNTDATA    TYPE TABLE OF TY_BAPI_INCINV_CREATE_GL_ACCO,
      T_EVENTOS          TYPE TABLE OF ZFTTE_EVENTOS,
      XML_TAB_NOTAS      TYPE TABLE OF ZXML_FERRO_TAB.

"---Work Areas
DATA: ST_XML_TAB          TYPE ZXML_FERRO_TAB,
      ST_ZLEST0035        TYPE ZLEST0035,
      ST_ZLEST0044        TYPE ZLEST0044,
      ST_ZLEST0045        TYPE ZLEST0045,
      ST_J_1BNFDOC        TYPE J_1BNFDOC,
      ST_MENSAGENS        TYPE TYPE_MSG ,
      ST_ZLEST0008        TYPE ZLEST0008,
      ST_LFA1             TYPE TY_LFA1,
      ST_INF_RECEB        TYPE TY_LFA1,
      ST_LFA2             TYPE TY_LFA1,
      ST_HEADERDATA       TYPE BAPISHIPMENTHEADER,
      ST_HEADERDATA_MIRO  TYPE TY_BAPI_INCINV_CREATE_HEADER,
      ST_IMPOSTOS         TYPE TY_BAPI_INCINV_CREATE_TAX,
      ST_STAGEDATA        TYPE BAPISHIPMENTSTAGE,
      ST_ZLEST0041        TYPE ZLEST0041,
      ST_ITEMDATA         TYPE BAPISHIPMENTITEM,
      ST_ITEMDATA_MIRO    TYPE TY_BAPI_INCINV_CREATE_ITEM,
      ST_VBFA             TYPE TY_VBFA,
      ST_RETURN           TYPE BAPIRET2,
      ST_MSG              TYPE BDCMSGCOLL,
      ST_BDC              TYPE BDCDATA,
      ST_VTFA             TYPE VTFA   ,
      ST_VFKP             TYPE VFKP   ,
      ST_HEADERDATA2      TYPE BAPISHIPMENTHEADER,
      ST_HEADERDATAACTION TYPE BAPISHIPMENTHEADERACTION,
      ST_IMPOSTOS_RETIDOS TYPE ZLES0043_IMP_RET,
      ST_ZLEST0021        TYPE ZLEST0021 ,
      ST_IMPOSTOS_RET_B   TYPE TY_BAPI_INCINV_CREATE_WITHTAX,
      ST_DADOS            TYPE TY_DADOS,
      ST_GLACCOUNTDATA    TYPE TY_BAPI_INCINV_CREATE_GL_ACCO,
      ST_EVENTOS          TYPE ZFTTE_EVENTOS,
      ST_VTTK             TYPE VTTK,
      ST_XML_TAB_NOTAS    TYPE ZXML_FERRO_TAB.


"---Variaveis
DATA: VCHAVE_CTE       TYPE CHAR44,
      V_CHAVE          TYPE ZLEST0045-CHAVE,
      V_VAGAO          TYPE ZLEST0045-NR_VAGAO,
      V_TP_VAGAO       TYPE ZLEST0045-TP_VAGAO,
      V_PESO_REAL      TYPE ZLEST0045-PESO_REAL,
      V_PESO_RATEADO   TYPE ZLEST0045-PESO_RATEADO,
      VCOD_FORNECEDOR  TYPE LFA1-LIFNR,
      V_CNPJ_ALL       TYPE J_1BCGC,
      V_CNPJ_CHAVE     TYPE J_1BCGC,
      V_NUM_NF_CHAVE   TYPE J_1BNFNUM9,
      V_SERIE_NF_CHAVE TYPE J_1BSERIES,
      V_MOD_NF_CHAVE   TYPE J_1BMODEL,
      V_ANO_MES_EMIS   TYPE C LENGTH 4,
      V_CD_UF_CHAVE    TYPE C LENGTH 2,
      V_DOCNUM         TYPE J_1BDOCNUM,
      V_BUKRS	         TYPE BUKRS,
      V_BRANCH         TYPE J_1BBRANC_,
      "v_ntgew          type j_1bnfdoc-ntgew,
      V_UTILIZADO      TYPE J_1BNFDOC-NTGEW,
      V_SALDO          TYPE ZLEST0035-SALDO,"j_1bnfdoc-ntgew,
      V_VBELN          TYPE VBFA-VBELN,
      V_POSNN          TYPE VBFA-POSNN,
      V_LIFNR          TYPE LFA1-LIFNR,
      V_TKNUM          TYPE VTTK-TKNUM,
      V_FKNUM          TYPE FKNUM,
      V_MSG            TYPE CHAR100,
      V_VLR_DIF        TYPE NETWR_P,
      V_MAXDIF         TYPE SETLEAF-VALFROM,
      V_MAXDIV         TYPE NETWR_P,
      V_MENSAGEM       TYPE STRING,
      V_MSG_PROC       TYPE BAPI_MSG,
      V_MENSAGEM2      TYPE STRING,
      V_VLR_FATURA     TYPE STRING,
      V_NETWR          TYPE STRING,
      V_DATA           TYPE C LENGTH 10,
      V_MODE           TYPE C LENGTH 1,
      V_1              TYPE I VALUE 1,
      V_ERRO           TYPE C LENGTH 1,
      WG_DATA_VI(10).

"---Constantes
CONSTANTS: C_L(1)           TYPE C              VALUE 'L',
           C_B(1)           TYPE C              VALUE 'B',
           C_E(1)           TYPE C              VALUE 'E',
           C_H(1)           TYPE C              VALUE 'H',
           C_N(1)           TYPE C              VALUE 'N',
           C_M(1)           TYPE C              VALUE 'M',
           C_S(1)           TYPE C              VALUE 'S',
           C_J(1)           TYPE C              VALUE 'J',
           C_R(1)           TYPE C              VALUE 'R',
           C_T(1)           TYPE C              VALUE 'T',
           C_W(1)           TYPE C              VALUE 'W',
           C_X(1)           TYPE C              VALUE 'X',
           C_BI(2)          TYPE C              VALUE 'BI',
           C_FT(2)          TYPE C              VALUE 'FT',
           C_VY(2)          TYPE C              VALUE 'VY',
           C_0000000001(10) TYPE C              VALUE '0000000001',
           C_000001(6)      TYPE C              VALUE '000001',
           C_0001(4)        TYPE C              VALUE '0001',
           C_0004(4)        TYPE C              VALUE '0004',
           C_Z003(4)        TYPE C              VALUE 'Z003',
           C_007(3)         TYPE C              VALUE '007',
           C_01             TYPE INRI-NRRANGENR VALUE '01',
           C_02(2)          TYPE C              VALUE '02',
           C_C1(2)          TYPE C              VALUE 'C1',
           C_TGG(3)         TYPE C              VALUE 'TGG',
           C_MGA(3)         TYPE C              VALUE 'MGA',
           C_VI01           TYPE SY-TCODE       VALUE 'VI01',
           C_VI02           TYPE SY-TCODE       VALUE 'VI02',
           C_VT02N          TYPE SY-TCODE       VALUE 'VT02N',
           C_MIRO(4)        TYPE C              VALUE 'MIRO',
           CN_ZEROS(9)      TYPE N              VALUE '000000000',
           C_1(1)           TYPE C              VALUE '1',
           C_2(1)           TYPE C              VALUE '2',
           C_15(2)          TYPE C              VALUE '15',
           C_14(2)          TYPE C              VALUE '14',
           C_16(2)          TYPE C              VALUE '16',
           C_17(2)          TYPE C              VALUE '17',
           C_18(2)          TYPE C              VALUE '18',
           C_19(2)          TYPE C              VALUE '19',
           C_20(2)          TYPE C              VALUE '20',
           C_30(2)          TYPE C              VALUE '30'.

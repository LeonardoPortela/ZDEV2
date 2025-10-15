*&---------------------------------------------------------------------*
*&  Include           ZLESR0014_TOP
*&---------------------------------------------------------------------*

* Tabela
TABLES:  ZLEST0034, LFA1, ESSR, T173. "Faturas de frete de terceiros

TYPE-POOLS: ICON, SLIS, ZFTTE.

* Tipo
TYPES: BEGIN OF Y_DADOS_TAB,
        TKNUM           TYPE ZLEST0032-TKNUM,             "Nr. doc. transporte
        FKNUM           TYPE ZLEST0032-FKNUM,             "Nr. custo de frete
        EXTI1           TYPE VTTK-EXTI1,                  "Conhecimento de embarque
        EXTI2           TYPE VTTK-EXTI2,                  "Carta Frete
        EBELN           TYPE ZLEST0034-EBELN,             "Pdido de compra
        EBELP           TYPE ZLEST0034-EBELP,             "Item Pdido de compra
        LBLNI           TYPE ZLEST0034-LBLNI,             "Folha de serviço
        SHTYP           TYPE VTTK-SHTYP,                  "Tipo de transporte
        TDLNR           TYPE VTTK-TDLNR,                  "Fornecedor
        GEWEI           TYPE ZLEST0034-GEWEI,             "Unidade de medida
        NAME1           TYPE LFA1-NAME1,                  "Nome do fornecedor
        LIFNR           TYPE EKKO-LIFNR,                  "Código Fornencedor
        VSART           TYPE VTTK-VSART,                  "Tipo de expedição
        ADD03           TYPE ZLEST0032-ADD03,             "Frota
        BUDAT           TYPE VFKK-BUDAT,                  "Data de Lançamento Contábil
       END OF Y_DADOS_TAB,

      BEGIN OF Y_TELA,
           TDLNR            TYPE ZLEST0034-TDLNR,
           NAME1            TYPE LFA1-NAME1,
           TKNUM            TYPE ZLEST0034-TKNUM,
           FKNUM            TYPE ZLEST0034-FKNUM,
           NFENUM           TYPE ZLEST0034-NFENUM,
           ZDT_MOV          TYPE ZLEST0034-ZDT_MOV,
           ZDT_VENCTO       TYPE ZLEST0034-ZDT_VENCTO,
           NR_CONHEC        TYPE ZLEST0034-NR_CONHEC,
           SERIES           TYPE ZLEST0034-SERIES,
           ZDT_CONHEC       TYPE ZLEST0034-ZDT_CONHEC,
           ZPESO_ORIGEM     TYPE ZLEST0034-ZPESO_ORIGEM,
           ZPESO_DESTINO    TYPE ZLEST0034-ZPESO_DESTINO,
           ZDT_CHEGADA      TYPE ZLEST0034-ZDT_CHEGADA,
           DMBTR            TYPE ZLEST0034-DMBTR,
           DMBTR_DOC        TYPE ZLEST0034-DMBTR_DOC,
           MATNR            TYPE ZLEST0034-MATNR,
           MAKTX            TYPE MAKT-MAKTX,
           MATNS            TYPE ZLEST0034-MATNS,
           VALOR_PEDAGIO    TYPE ZLEST0034-VALOR_PEDAGIO,
           IT_IMPOSTOS_RETIDOS TYPE ZLES0043_IMP_RETIDOS_T,
      END OF Y_TELA,

      BEGIN OF Y_EKBE,
        EBELN     TYPE EKBE-EBELN,
        EBELP     TYPE EKBE-EBELP,
        LFBNR     TYPE EKBE-LFBNR,
        BELNR     TYPE EKBE-BELNR,
*        gjahr     TYPE ekbe-gjahr,
        LFGJA     TYPE EKBE-LFGJA,
        MENGE     TYPE EKBE-MENGE,
      END OF Y_EKBE,

      BEGIN OF Y_NOTAS,
        TKNUM       TYPE ZLEST0032-TKNUM,
        VBELN_K     TYPE VBAK-VBELN,
        VBELN_F     TYPE VBFA-VBELN,
        POSNN       TYPE VBFA-POSNN,
        DOCNUM      TYPE J_1BNFLIN-DOCNUM,
        NFENUM      TYPE J_1BNFDOC-NFENUM,
      END OF Y_NOTAS,

      BEGIN OF Y_ZLEST0021,
        SHTYP       TYPE ZLEST0021-SHTYP,
        TCODE       TYPE ZLEST0021-TCODE,
        FATURA      TYPE ZLEST0021-FATURA,
        RAZAODEB    TYPE ZLEST0021-RAZAODEB,
        RAZAOCRED   TYPE ZLEST0021-RAZAOCRED,
        OPERFRETE   TYPE ZLEST0021-OPERFRETE,
        TP_VEICULO  TYPE ZLEST0021-TP_VEICULO,
      END OF Y_ZLEST0021.

* Campos auxiliares
DATA: VG_OKCODE           TYPE SY-UCOMM,
      VG_ERRO_S_N         TYPE C,
      VG_INDICE           TYPE SY-TABIX,
      VG_ULTIMO           TYPE SY-TABIX,
      VG_DT_MOV(10)       TYPE C,
      VG_DT_VENCTO(10)    TYPE C,
      VG_DT_CONHEC(10)    TYPE C,
      VG_DT_CHEGADA(10)   TYPE C,
      VG_PESO_DESTINO(19) TYPE C,
      VG_IVA              TYPE MWSKZ,
      TX_IVA              TYPE TEXT1_007S,
      VG_NFE              TYPE J_1BNFE,
      VG_FERRO_NOVO       TYPE C,
      VG_MULTIMODAL       TYPE CHAR01,
      VG_PRODUTO_NOME     TYPE MAKTG,
      VG_DATA             TYPE SY-DATUM,
      VG_DATA_MOV         TYPE SY-DATUM,
      VG_DATA_VENCTO      TYPE SY-DATUM,
      VG_DATA_CONHEC      TYPE SY-DATUM,
      VG_DATA_CHEGADA     TYPE SY-DATUM,
      P_KG_TRANSP         TYPE P DECIMALS 4,
      P_KG_MERCAD         TYPE P DECIMALS 2,
      PC_TOLERANCIA       TYPE P DECIMALS 2,
      VG_ALTEROU          TYPE C LENGTH 1,
      WA_FCODE            TYPE SY-UCOMM.

* Tabelas internas
DATA: TI_DADOS_TAB      TYPE TABLE OF Y_DADOS_TAB,
      TI_DADOS          TYPE TABLE OF ZFTTE_DADOS,
      TI_DADOS_MIRO     TYPE TABLE OF ZFTTE_DADOS,
      TI_NOTAS          TYPE TABLE OF Y_NOTAS,
      TI_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV,
      TI_EVENTS         TYPE SLIS_T_EVENT,
      TI_EKBE           TYPE TABLE OF EKBE,
      TI_TELA           TYPE TABLE OF Y_TELA,
      TI_EVENTOS        TYPE TABLE OF ZFTTE_EVENTOS,
      IT_FCODE          LIKE TABLE OF WA_FCODE,
      TI_ESTADIA        TYPE TABLE OF ZFTTE_DADOS,
      T_ZLEST0021       TYPE TABLE OF ZLEST0021,
      T_ZLEST0042       TYPE TABLE OF ZLEST0042.

* Estrutruas
DATA: ST_DADOS          TYPE ZFTTE_DADOS,
      ST_DADOS_AUX      TYPE ZFTTE_DADOS,
      ST_DADOS_TAB      TYPE Y_DADOS_TAB,
      ST_EKBE           TYPE Y_EKBE,
      ST_NOTAS          TYPE Y_NOTAS,
      ST_ZLEST0021      TYPE Y_ZLEST0021,
      ST_RETURN         TYPE BAPIRET2,
      ST_HEADER         TYPE J_1BNFDOC,
      ST_ITEM           TYPE J_1BNFLIN,
      ST_ITEM_TAX       TYPE J_1BNFSTX,
      ST_FIELDCAT       TYPE SLIS_FIELDCAT_ALV,
      ST_LAYOUT         TYPE SLIS_LAYOUT_ALV,
      ST_TOP_HEADER     TYPE SLIS_T_LISTHEADER,
      ST_TELA           TYPE Y_TELA,
      ST_EVENTOS        TYPE ZFTTE_EVENTOS.

* Constantes
CONSTANTS: C_BR                 TYPE LAND1 VALUE 'BR ',
           N_1                  TYPE I VALUE 1,
           C_0                  TYPE C LENGTH 1 VALUE '0',
           C_1                  TYPE C LENGTH 1 VALUE '1',
           C_2                  TYPE C LENGTH 1 VALUE '2',
           C_4                  TYPE C LENGTH 1 VALUE '4',
           C_5                  TYPE C LENGTH 1 VALUE '5',
           C_14                 TYPE C LENGTH 2 VALUE '14',
           C_15                 TYPE C LENGTH 2 VALUE '15',
           C_16                 TYPE C LENGTH 2 VALUE '16',
           C_17                 TYPE C LENGTH 2 VALUE '17',
           C_18                 TYPE C LENGTH 2 VALUE '18',
           C_19                 TYPE C LENGTH 2 VALUE '19',
           C_20                 TYPE C LENGTH 2 VALUE '20',
           C_21                 TYPE C LENGTH 2 VALUE '21',
           C_26                 TYPE C LENGTH 2 VALUE '26',
           C_08                 TYPE C LENGTH 2 VALUE '08',
           C_57                 TYPE C LENGTH 2 VALUE '57',
           C_9                  TYPE C LENGTH 1 VALUE '9',
           C_000001             TYPE C LENGTH 6 VALUE '000001',
           C_0000000002(10)     TYPE C VALUE '0000000002',
           C_0000000001(10)     TYPE C VALUE '0000000001',
           C_A                  TYPE C VALUE 'A',
           C_C                  TYPE C VALUE 'C',
           C_H                  TYPE C VALUE 'H',
           C_M                  TYPE C VALUE 'M',
           C_J                  TYPE C VALUE 'J',
           C_N                  TYPE C VALUE 'N',
           C_S                  TYPE C VALUE 'S',
           C_W                  TYPE C VALUE 'W',
           C_E                  TYPE C VALUE 'E',
           C_X                  TYPE C VALUE 'X',
           C_P                  TYPE C VALUE 'P',
           C_T                  TYPE C VALUE 'T',
           C_V                  TYPE C VALUE 'V',
           C_C1                 TYPE C LENGTH 2 VALUE 'C1',
           C_C2                 TYPE C LENGTH 2 VALUE 'C2',
           C_C6                 TYPE C LENGTH 2 VALUE 'C6',
           C_LI                 TYPE C LENGTH 2 VALUE 'LI',
           C_ZH                 TYPE C LENGTH 2 VALUE 'ZH',
           C_NT                 TYPE C LENGTH 2 VALUE 'NT',
           C_NS                 TYPE C LENGTH 2 VALUE 'NS',
           C_LF                 TYPE C LENGTH 2 VALUE 'LF',
           C_FT                 TYPE C LENGTH 2 VALUE 'FT',
           C_I7                 TYPE C LENGTH 2 VALUE 'I7',
           C_TX                 TYPE C LENGTH 2 VALUE 'TX',
           C_BI                 TYPE C LENGTH 2 VALUE 'BI',
           C_ATE(3)             TYPE C VALUE 'até',
           C_BOX(3)             TYPE C VALUE 'BOX',
           C_0004               TYPE C LENGTH 4 VALUE '0004',
           C_ZMRG               TYPE C LENGTH 4 VALUE 'ZMRG',
           C_ICMS               TYPE C LENGTH 4 VALUE 'ICMS',
           C_PIS                TYPE C LENGTH 4 VALUE 'PIS',
           C_COFINS             TYPE C LENGTH 4 VALUE 'COFI',
           C_IPI                TYPE C LENGTH 4 VALUE 'IPI',
           C_MIRO               TYPE C LENGTH 4 VALUE 'MIRO',
           C_F02                TYPE CHAR4      VALUE 'F-02',
           C_NF57               TYPE C LENGTH 4 VALUE 'NF57',
           C_ML81N              TYPE C LENGTH 5 VALUE 'ML81N',
           C_KSCHL              TYPE C LENGTH 5 VALUE 'KSCHL',
           C_TAXBRA             TYPE C LENGTH 6 VALUE 'TAXBRA',
           C_TI_DADOS(8)        TYPE C VALUE 'TI_DADOS',
* Botões da tela de complemento de dados.
           C_IMPOSTOS_R(10)     TYPE C VALUE 'IMPOSTOS_R',
           C_ANTERIOR(8)        TYPE C VALUE 'ANTERIOR',
           C_PROXIMO(7)         TYPE C VALUE 'PROXIMO',
           C_CANCELAR(8)        TYPE C VALUE 'CANCELAR',
           C_CONFIRMAR(9)       TYPE C VALUE 'CONFIRMAR',
* Constantes específicas para o ALV
           C_Z_TOP_OF_PAGE      TYPE SLIS_FORMNAME  VALUE 'Z_TOP_OF_PAGE',
           C_TDLNR              TYPE SLIS_FIELDNAME VALUE 'TDLNR',
           C_NAME1              TYPE SLIS_FIELDNAME VALUE 'NAME1',
           C_STATUS             TYPE SLIS_FIELDNAME VALUE 'STATUS',
           C_SHTYP              TYPE SLIS_FIELDNAME VALUE 'SHTYP',
           C_TKNUM              TYPE SLIS_FIELDNAME VALUE 'TKNUM',
           C_FKNUM              TYPE SLIS_FIELDNAME VALUE 'FKNUM',
           C_NFENUM             TYPE SLIS_FIELDNAME VALUE 'NFENUM',
           C_SERIES             TYPE SLIS_FIELDNAME VALUE 'SERIES',
           C_ZDT_MOV            TYPE SLIS_FIELDNAME VALUE 'ZDT_MOV',
           C_ZDT_VENCTO         TYPE SLIS_FIELDNAME VALUE 'ZDT_VENCTO',
           C_NR_CONHEC          TYPE SLIS_FIELDNAME VALUE 'NR_CONHEC',
           C_ZDT_CONHEC         TYPE SLIS_FIELDNAME VALUE 'ZDT_CONHEC',
           C_ZPESO_DESTINO      TYPE SLIS_FIELDNAME VALUE 'ZPESO_DESTINO',
           C_ZDT_CHEGADA        TYPE SLIS_FIELDNAME VALUE 'ZDT_CHEGADA',
           C_EBELN              TYPE SLIS_FIELDNAME VALUE 'EBELN',
           C_LBLNI              TYPE SLIS_FIELDNAME VALUE 'LBLNI',
           C_ZPESO_ORIGEM       TYPE SLIS_FIELDNAME VALUE 'ZPESO_ORIGEM',
           C_GEWEI              TYPE SLIS_FIELDNAME VALUE 'GEWEI',
           C_DMBTR              TYPE SLIS_FIELDNAME VALUE 'DMBTR',
           C_KBETR              TYPE SLIS_FIELDNAME VALUE 'KBETR',
           C_ZPESO_DIFERENCA    TYPE SLIS_FIELDNAME VALUE 'ZPESO_DIFERENCA',
           C_ZQUEBRA            TYPE SLIS_FIELDNAME VALUE 'ZQUEBRA',
           C_ZPERDA             TYPE SLIS_FIELDNAME VALUE 'ZPERDA',
           C_ZVLR_QUEBRA        TYPE SLIS_FIELDNAME VALUE 'ZVLR_QUEBRA',
           C_ZVLR_PERDA         TYPE SLIS_FIELDNAME VALUE 'ZVLR_PERDA',
           C_VALOR_PEDAGIO      TYPE SLIS_FIELDNAME VALUE 'VALOR_PEDAGIO',
           C_ZVLR_LIQ_PAGAR     TYPE SLIS_FIELDNAME VALUE 'ZVLR_LIQ_PAGAR',
           C_MATNR              TYPE SLIS_FIELDNAME VALUE 'MATNR',
           C_MAKTX              TYPE SLIS_FIELDNAME VALUE 'MAKTX',
           C_DMBTR_DOC          TYPE SLIS_FIELDNAME VALUE 'DMBTR_DOC',
           C_BVTYP              TYPE SLIS_FIELDNAME VALUE 'BVTYP',
           C_REGIO_EMISSOR      TYPE SLIS_FIELDNAME VALUE 'REGIO_EMISSOR',
           C_REGIO_RECEPTOR     TYPE SLIS_FIELDNAME VALUE 'REGIO_RECEPTOR',
           C_BASE_ICMS          TYPE SLIS_FIELDNAME VALUE 'BASE_ICMS',
           C_BASE_PIS           TYPE SLIS_FIELDNAME VALUE 'BASE_PIS',
           C_BASE_COFINS        TYPE SLIS_FIELDNAME VALUE 'BASE_COFINS',
           C_RATE_ICMS          TYPE SLIS_FIELDNAME VALUE 'RATE_ICMS',
           C_RATE_PIS           TYPE SLIS_FIELDNAME VALUE 'RATE_PIS',
           C_RATE_COFINS        TYPE SLIS_FIELDNAME VALUE 'RATE_COFINS',
           C_VALOR_ICMS         TYPE SLIS_FIELDNAME VALUE 'VALOR_ICMS',
           C_VALOR_PIS          TYPE SLIS_FIELDNAME VALUE 'VALOR_PIS',
           C_VALOR_COFINS       TYPE SLIS_FIELDNAME VALUE 'VALOR_COFINS',
           C_DOCNUM             TYPE SLIS_FIELDNAME VALUE 'DOCNUM',
           C_VALOR_MERCADORIA   TYPE SLIS_FIELDNAME VALUE 'VALOR_MERCADORIA'.

" Variáveis/Tabelas Internas/Work Areas para simulação
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

TYPES: BEGIN OF TY_INFO_FORNE.
TYPES:   BVTYP TYPE BVTYP,  "Tipo de banco do parceiro
         TEXTO TYPE CHAR50, "Fornecedor
         BANKL TYPE CHAR03, "Banco
         BANKA TYPE BANKA,  "Nome do Banco
         BANKN TYPE BANKN,  "Conta Corrente
         AGENC TYPE CHAR15. "Agencia
TYPES: END OF TY_INFO_FORNE.

DATA: IT_ZLEST0021      TYPE TABLE OF ZLEST0021 INITIAL SIZE 0 WITH HEADER LINE,
      WA_ZLEST0021      TYPE ZLEST0021.

DATA: IT_HEADERDATA       TYPE TABLE OF TY_BAPI_INCINV_CREATE_HEADER,
      IT_ITEMDATA         TYPE TABLE OF TY_BAPI_INCINV_CREATE_ITEM,
      IT_IMPOSTOS         TYPE TABLE OF TY_BAPI_INCINV_CREATE_TAX,
      IT_IMPOSTOS_RET_B   TYPE TABLE OF TY_BAPI_INCINV_CREATE_WITHTAX,
      IT_GLACCOUNTDATA    TYPE TABLE OF TY_BAPI_INCINV_CREATE_GL_ACCO,
      WA_HEADERDATA       TYPE TY_BAPI_INCINV_CREATE_HEADER,
      WA_IMPOSTOS         TYPE TY_BAPI_INCINV_CREATE_TAX,
      WA_IMPOSTOS_RET_B   TYPE TY_BAPI_INCINV_CREATE_WITHTAX,
      WA_ITEMDATA         TYPE TY_BAPI_INCINV_CREATE_ITEM,
      WA_GLACCOUNTDATA    TYPE TY_BAPI_INCINV_CREATE_GL_ACCO,
      IT_IMPOSTOS_RETIDOS TYPE TABLE OF ZLES0043_IMP_RETIDOS WITH HEADER LINE,
      INFO_FORNE          TYPE TY_INFO_FORNE.

"Controles de Tela
DATA: VG_TELA_0200 TYPE SY-DYNNR,
      OK_CODE      TYPE SY-UCOMM,
      IT_ITEMCAP   TYPE TABLE OF TY_BAPI_INCINV_CREATE_ITEM,
      IT_ITESCONTA TYPE TABLE OF BAPI_INCINV_CREATE_GL_ACCOUNT.

CONSTANTS: C_0201 TYPE SY-DYNNR VALUE '0201',
           C_0202 TYPE SY-DYNNR VALUE '0202',
           C_0203 TYPE SY-DYNNR VALUE '0203'.

CONSTANTS: C_EXIT     TYPE C LENGTH 4 VALUE 'EXIT',
           C_EDITAR   TYPE C LENGTH 6 VALUE 'EDITAR',
           C_GRAVAR   TYPE C LENGTH 6 VALUE 'GRAVAR',
           C_CANCEL   TYPE C LENGTH 6 VALUE 'CANCEL',
           C_SIMULAR  TYPE C LENGTH 7 VALUE 'SIMULAR',
           C_EVENTOS  TYPE C LENGTH 7 VALUE 'EVENTOS',
           C_BACK     TYPE C LENGTH 4 VALUE 'BACK',
           C_ITENS    TYPE C LENGTH 5 VALUE 'ITENS',
           C_GERAR    TYPE C LENGTH 5 VALUE 'GERAR',
           C_IMPO     TYPE C LENGTH 4 VALUE 'IMPO',
           C_ESTORNAR TYPE C LENGTH 8 VALUE 'ESTORNAR'.


DATA: V_RBKP        TYPE C.

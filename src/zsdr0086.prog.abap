*&---------------------------------------------------------------------*
*& Report  ZSDR0086
*& Relatório de Vinculações e Liberações de Embarque – Insumos
*& 14.02.2018
*& Jean Antunes
*&---------------------------------------------------------------------*


REPORT ZSDR0086.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON.
*----------------------------------------------------------------------*
* TABLES
*---------------------------------------------------------------------- *
TABLES: VBAK, VBAP, EKKO, KNA1, LFA1, T001W, ZSDT0062.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF TY_EKKO,
    " MM - COMPRAS - TABELA DE CABEÇALHO DO DOCUMENTO DE COMPRAS
    EBELN TYPE EKKO-EBELN, " Nº do documento de compras
    BSART TYPE EKKO-BSART, " Tipo de documento de compras
    LIFNR TYPE EKKO-LIFNR, " Nº conta do fornecedor
    BUKRS TYPE EKKO-BUKRS, " Empresa
    AEDAT TYPE EKKO-AEDAT, " Data de criação do registro
    WAERS TYPE EKKO-WAERS,
    UNSEZ TYPE EKKO-UNSEZ, " Safra
  END OF TY_EKKO,

  BEGIN OF TY_EKPO,
    EBELN TYPE EKPO-EBELN, " Nº do documento de compras
    EBELP TYPE EKPO-EBELP, " Nº item do documento de compra
    MATNR TYPE EKPO-MATNR, " Nº do material
    TXZ01 TYPE EKPO-TXZ01, " Texto breve
    MENGE TYPE EKPO-MENGE, " Quantidade do pedido
    NETWR TYPE EKPO-NETWR, " Valor líquido do pedido em moeda de pedido
    LOEKZ TYPE EKPO-LOEKZ, " Código de eliminação no documento de compras
    MEINS TYPE EKPO-MEINS, " Unidade de Medida
    NETPR TYPE EKPO-NETPR,
    WERKS TYPE EKPO-WERKS, " Centro
  END OF TY_EKPO,

  BEGIN OF TY_EKKO_T,
    " MM - TRANSFERÊNCIAS - TABELA DE CABEÇALHO DO DOCUMENTO DE TRANSFERÊNCIA
    EBELN TYPE EKKO-EBELN, " Nº do documento de compras
    BSART TYPE EKKO-BSART, " Tipo de documento de compras
    RESWK TYPE EKKO-RESWK, " Nº conta do fornecedor
    BUKRS TYPE EKKO-BUKRS, " Empresa
    AEDAT TYPE EKKO-AEDAT, " Data de criação do registro
    WAERS TYPE EKKO-WAERS,
    UNSEZ TYPE EKKO-UNSEZ, " Safra
  END OF TY_EKKO_T,

  BEGIN OF TY_EKPO_T,
    EBELN TYPE EKPO-EBELN, " Nº do documento de compras
    EBELP TYPE EKPO-EBELP, " Nº item do documento de compra
    MATNR TYPE EKPO-MATNR, " Nº do material
    TXZ01 TYPE EKPO-TXZ01, " Texto breve
    MENGE TYPE EKPO-MENGE, " Quantidade do pedido
    NETWR TYPE EKPO-NETWR, " Valor líquido do pedido em moeda de pedido
    LOEKZ TYPE EKPO-LOEKZ, " Código de eliminação no documento de compras
    MEINS TYPE EKPO-MEINS, " Unidade de Medida
    NETPR TYPE EKPO-NETPR,
  END OF TY_EKPO_T,

  BEGIN OF TY_VBAK,
    " SD - VENDAS - DOCUMENTO DE VENDAS: DADOS DO CABEÇALHO
    VBELN TYPE VBAK-VBELN, " Documento de vendas
    VKORG TYPE VBAK-VKORG, " Organização de vendas
    VTWEG TYPE VBAK-VTWEG, " Canal de distribuição
    SPART TYPE VBAK-SPART, " Setor de atividade
    AUART TYPE VBAK-AUART, " Tipo de documento de vendas
    VKBUR TYPE VBAK-VKBUR, " Escritório de vendas
    KUNNR TYPE VBAK-KUNNR, " Emissor da ordem
    WAERK TYPE VBAK-WAERK, " Moeda do documento SD
    ERDAT TYPE VBAK-ERDAT, " Data de criação do registro
    FAKSK TYPE VBAK-FAKSK, " Bloqueio tipos de doc.faturamento - documento SD
    LIFSK TYPE VBAK-LIFSK, " Bloqueio tipos de doc.faturamento - documento SD
    AUDAT TYPE VBAK-AUDAT, " Dada do documento (data de entrada / saída)
    KNUMV TYPE VBAK-KNUMV,
  END OF TY_VBAK,

  BEGIN OF TY_VBAP,
    " SD - VENDAS - Documento de vendas: dados de item
    VBELN  TYPE VBAP-VBELN,
    MATNR  TYPE VBAP-MATNR, " Nº do material
    ARKTX  TYPE VBAP-ARKTX, " Texto breve do item da ordem do cliente
    WERKS  TYPE VBAP-WERKS, " Centro (próprio ou externo)
    ZMENG  TYPE VBAP-ZMENG, " Qtd.prevista em UMV
    NETWR  TYPE VBAP-NETWR, " Valor líquido do item da ordem na moeda do documento
    KWMENG TYPE VBAP-KWMENG, " Quantidade da ordem acumulada em unidade de venda
    VRKME  TYPE VBAP-VRKME, " Unidade de venda
    POSNR  TYPE VBAP-POSNR, " Item
    NETPR  TYPE VBAP-NETPR, " Preço líquido
  END OF TY_VBAP,

  BEGIN OF TY_SAIDA,

    SOL_VINC        TYPE ZSDT0062-SOL_VINC,
    DT_VINC         TYPE ZSDT0062-DT_VINC,
    EBELN           TYPE ZSDT0062-EBELN,
    EBELP           TYPE ZSDT0062-EBELP,
    EBELN_T         TYPE ZSDT0062-EBELN_T,
    EBELP_T         TYPE ZSDT0062-EBELP_T,
    VBELN           TYPE ZSDT0062-VBELN,
    POSNR           TYPE ZSDT0062-POSNR,
    KUNNR           TYPE ZSDT0062-KUNNR,
    DESC_CLIENTE    TYPE KNA1-NAME1,
    LIFNR           TYPE ZSDT0062-LIFNR,
    DESC_FORN       TYPE LFA1-NAME1,
    RESWK           TYPE EKKO-RESWK,
    DESC_FORNT      TYPE T001W-NAME1,
    QTD_VINC        TYPE ZSDT0062-QTD_VINC,
    MATNR           TYPE ZSDT0062-MATNR,
    DESC_MATERIAL   TYPE MAKT-MAKTX,
    LOCAL_EMBARQUE  TYPE ZSDT0062-LOCAL_EMBARQUE,
    NRO_SOL         TYPE ZSDT0062-NRO_SOL,
    NRO_CG          TYPE ZSDT0062-NRO_CG,
    MBLNR           TYPE ZSDT0062-MBLNR,
    NR_FORN         TYPE ZSDT0062-NR_FORN,
    ENVIO_EMAIL(10) TYPE C,
    STATUS          TYPE ZSDT0062-STATUS,

    CELLCOLORS     TYPE LVC_T_SCOL,

  END OF TY_SAIDA,

  BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    NAME1 TYPE KNA1-NAME1,
  END OF TY_KNA1,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
  END OF TY_LFA1,

  BEGIN OF TY_VBFA,
    RFMNG   TYPE VBFA-RFMNG,
    VBELN   TYPE VBFA-VBELN,
    VBTYP_N TYPE VBFA-VBTYP_N,
    VBELV   TYPE VBFA-VBELV,
    VBTYP_V TYPE VBFA-VBTYP_V,
    MEINS   TYPE VBFA-MEINS,
    ERDAT   TYPE VBFA-ERDAT,
    MATNR   TYPE VBFA-MATNR,
    POSNV   TYPE VBFA-POSNV,
  END OF TY_VBFA,


  BEGIN OF TY_VBFA_AUX,
    RFMNG TYPE VBFA-RFMNG,
    VBELN TYPE VBFA-VBELN,
    VBELV TYPE VBFA-VBELV,
  END OF TY_VBFA_AUX,

  BEGIN OF TY_EKBE,
    EBELN TYPE EKBE-EBELN,
    VGABE TYPE EKBE-VGABE,
    MENGE TYPE EKBE-MENGE,
    SHKZG TYPE EKBE-SHKZG,
    BELNR TYPE EKBE-BELNR,
    BUDAT TYPE EKBE-BUDAT,
    DMBTR TYPE EKBE-DMBTR,
    EBELP TYPE EKBE-EBELP,
    MATNR TYPE EKBE-MATNR,
    GJAHR TYPE EKBE-GJAHR,
    BEWTP TYPE EKBE-BEWTP,

  END OF TY_EKBE,

  BEGIN OF TY_S225,
    SPTAG           TYPE S225-SPTAG,  "#EC CI_USAGE_OK[2268063]
    WERKS           TYPE S225-WERKS,  "#EC CI_USAGE_OK[2268063]
    MATNR           TYPE S225-MATNR,  "#EC CI_USAGE_OK[2268063]
    WEMNG           TYPE S225-WEMNG,  "#EC CI_USAGE_OK[2268063]
    AMEIN           TYPE S225-AMEIN,  "#EC CI_USAGE_OK[2268063]
    MENGE           TYPE EKPO-MENGE,
    TOTAL_PRODUZIDO TYPE S225-WEMNG,   "#EC CI_USAGE_OK[2268063]

  END OF TY_S225,

  BEGIN OF TY_KONV,
    KNUMV TYPE KONV-KNUMV,
    KPOSN TYPE KONV-KPOSN,
    KSCHL TYPE KONV-KSCHL,
    KBETR TYPE KONV-KBETR,
  END OF TY_KONV,

  BEGIN OF TY_RBKP,
    BELNR TYPE RBKP-BELNR,
    GJAHR TYPE RBKP-GJAHR,
    XBLNR TYPE RBKP-XBLNR,
    STBLG TYPE RBKP-STBLG,
  END OF TY_RBKP,

  BEGIN OF TY_MARA,
    MATKL TYPE MARA-MATKL,
    MATNR TYPE MARA-MATNR,
  END OF TY_MARA,

  BEGIN OF TY_SETLEAF,
    VALFROM_AUX TYPE MARA-MATKL,
  END OF TY_SETLEAF,

  BEGIN OF TY_MAKT,
    MANDT TYPE MAKT-MANDT,
    MATNR TYPE MAKT-MATNR,
    SPRAS TYPE MAKT-SPRAS,
    MAKTX TYPE MAKT-MAKTX,
    MAKTG TYPE MAKT-MAKTG,
  END OF TY_MAKT,

  BEGIN OF TY_T001W,
    WERKS TYPE T001W-WERKS,
    NAME1 TYPE T001W-NAME1,
  END OF TY_T001W,

  BEGIN OF TY_J_1BBRANCH,
    BUKRS  TYPE J_1BBRANCH-BUKRS,
    BRANCH TYPE J_1BBRANCH-BRANCH,
  END OF TY_J_1BBRANCH,

  BEGIN OF TY_VBEP,
    VBELN TYPE VBEP-VBELN,
    POSNR TYPE VBEP-POSNR,
    ETENR TYPE VBEP-ETENR,
    LIFSP TYPE VBEP-LIFSP,
  END OF TY_VBEP,

  BEGIN OF TY_ZIB_BSIK,
    BUKRS TYPE BSIK-BUKRS,
    BELNR TYPE BSIK-BELNR,
    GJAHR TYPE BSIK-GJAHR,
  END OF TY_ZIB_BSIK,

  BEGIN OF TY_ZSDT0062,
    SOL_VINC       TYPE ZSDT0062-SOL_VINC,
    EBELN          TYPE ZSDT0062-EBELN,
    EBELP          TYPE ZSDT0062-EBELP,
    EBELN_T        TYPE ZSDT0062-EBELN_T,
    EBELP_T        TYPE ZSDT0062-EBELP_T,
    RESWK          TYPE EKKO-RESWK,
    VBELN          TYPE ZSDT0062-VBELN,
    POSNR          TYPE ZSDT0062-POSNR,
    NRO_SOL        TYPE ZSDT0062-NRO_SOL,
    SEQ            TYPE ZSDT0062-SEQ,
    NRO_CG         TYPE ZSDT0062-NRO_CG,
    MBLNR          TYPE ZSDT0062-MBLNR,
    DT_VINC        TYPE ZSDT0062-DT_VINC,
    MATNR          TYPE ZSDT0062-MATNR,
    QTD_VINC       TYPE ZSDT0062-QTD_VINC,
    LOCAL_EMBARQUE TYPE ZSDT0062-LOCAL_EMBARQUE,
    USNAM          TYPE ZSDT0062-USNAM,
    DT_ATUAL       TYPE ZSDT0062-DT_ATUAL,
    HORA_ATUL      TYPE ZSDT0062-HORA_ATUL,
    ENVIO_EMAIL    TYPE ZSDT0062-ENVIO_EMAIL,
    LIFNR          TYPE ZSDT0062-LIFNR,
    KUNNR          TYPE ZSDT0062-KUNNR,
    NR_FORN        TYPE ZSDT0062-NR_FORN,
    STATUS         TYPE ZSDT0062-STATUS,

  END OF TY_ZSDT0062.

"--------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:
  IT_SAIDA    TYPE TABLE OF TY_SAIDA WITH HEADER LINE,    "
  IT_EKKO     TYPE TABLE OF TY_EKKO, "
  IT_EKPO     TYPE TABLE OF TY_EKPO, "
  IT_EKKO_T   TYPE TABLE OF TY_EKKO_T, "
  IT_EKPO_T   TYPE TABLE OF TY_EKPO, "
  IT_VBAK     TYPE TABLE OF TY_VBAK, "
  IT_VBAP     TYPE TABLE OF TY_VBAP, "
  IT_KNA1     TYPE TABLE OF TY_KNA1, "
  IT_LFA1     TYPE TABLE OF TY_LFA1, "
  IT_T001W    TYPE TABLE OF TY_T001W,
  IT_MAKT     TYPE TABLE OF TY_MAKT,
  IT_MARA     TYPE TABLE OF TY_MARA, "
  IT_ZSDT0062 TYPE TABLE OF TY_ZSDT0062.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:

  WA_ALV_0001       TYPE REF TO CL_GUI_ALV_GRID,
  WA_CONTAINER_0001 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

  WA_LAYOUT         TYPE LVC_S_LAYO,

  WA_SAIDA          TYPE TY_SAIDA,

  WA_EKKO           TYPE TY_EKKO, "
  WA_EKKO_T         TYPE TY_EKKO_T, "
  WA_EKPO           TYPE TY_EKPO, "
  WA_EKPO_T         TYPE TY_EKPO, "
  WA_VBAK           TYPE TY_VBAK, "
  WA_VBAP           TYPE TY_VBAP, "
  WA_KNA1           TYPE TY_KNA1, "
  WA_LFA1           TYPE TY_LFA1, "
  WA_T001W          TYPE TY_T001W,
  WA_MAKT           TYPE TY_MAKT,
  WA_MARA           TYPE TY_MARA,
  WA_ZSDT0062       TYPE TY_ZSDT0062.




*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: IT_FCAT      TYPE TABLE OF LVC_S_FCAT,
      GS_VARIANT_C TYPE DISVARIANT.

CLASS CL_GUI_CFW DEFINITION LOAD.

DATA: URL(255)                TYPE C,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
        VL_CONT                 TYPE I,
        VL_GTEXT                TYPE TGSBT-GTEXT,
        VL_LANDX                TYPE T005T-LANDX.

 DATA: DTVINC_I(10)     TYPE C,
        DTVINC_F(10)     TYPE C.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BLOCO1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_EBELN   FOR ZSDT0062-EBELN , "Pedido
                P_EBELNT  FOR ZSDT0062-EBELN_T, "Pedito de Transf.
                P_VBELN   FOR ZSDT0062-VBELN,  "Ordem de Venda
                P_MATNR   FOR ZSDT0062-MATNR NO-EXTENSION NO INTERVALS, "Material
                P_KUNNR   FOR ZSDT0062-KUNNR NO-EXTENSION NO INTERVALS, "Cliente
                P_LIFNR   FOR ZSDT0062-LIFNR NO INTERVALS, "Fornecedor
                P_RESWK   FOR T001W-WERKS NO INTERVALS, "ZSDT0062-RESWK NO INTERVALS, "Fornecedor Transf.
                P_LOCAL   FOR ZSDT0062-LOCAL_EMBARQUE NO INTERVALS NO-EXTENSION, "Local de Embarque
                P_STATUS  FOR ZSDT0062-STATUS NO INTERVALS OBLIGATORY, "status
                P_DTVINC FOR ZSDT0062-DT_VINC OBLIGATORY. " Data de Vinculação
SELECTION-SCREEN: END OF BLOCK BLOCO1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: F_SELECIONA_DADOS. " Form seleciona dados

  CALL SCREEN 0001.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .


  SELECT *
    FROM ZSDT0062
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0062
    WHERE EBELN           IN P_EBELN  AND
          EBELN_T         IN P_EBELNT AND
          VBELN           IN P_VBELN  AND
          MATNR           IN P_MATNR  AND
          KUNNR           IN P_KUNNR  AND
          LIFNR           IN P_LIFNR  AND
          RESWK           IN P_RESWK  AND
          LOCAL_EMBARQUE  IN P_LOCAL  AND
          STATUS          IN P_STATUS AND
          DT_VINC         IN P_DTVINC.

  " Mestre de fornecedores
  SELECT LIFNR NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_ZSDT0062
  WHERE LIFNR EQ IT_ZSDT0062-LIFNR.

  " Descrição do Material
  SELECT MANDT MATNR SPRAS MAKTX MAKTG
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_ZSDT0062
  WHERE MATNR EQ IT_ZSDT0062-MATNR.

  " Mestre de Clientes
  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_ZSDT0062
  WHERE KUNNR EQ IT_ZSDT0062-KUNNR.

  " Fornecedor Transf.
  SELECT WERKS
         NAME1
    FROM T001W
    INTO TABLE IT_T001W
    FOR ALL ENTRIES IN IT_ZSDT0062
  WHERE WERKS = IT_ZSDT0062-RESWK.

  SORT: IT_ZSDT0062 BY EBELN,
        IT_LFA1     BY LIFNR,
        IT_MAKT     BY MATNR,
        IT_KNA1     BY KUNNR.


  " ORGANIZAR OS DADOS SELECIONADOS
  PERFORM F_AGRUPA_DADOS.


ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_AGRUPA_DADOS
*&---------------------------------------------------------------------*
FORM F_AGRUPA_DADOS.

  LOOP AT IT_ZSDT0062 INTO WA_ZSDT0062.

    WA_SAIDA-SOL_VINC = WA_ZSDT0062-SOL_VINC.
    WA_SAIDA-DT_VINC  = WA_ZSDT0062-DT_VINC.
    WA_SAIDA-EBELN    = WA_ZSDT0062-EBELN.
    WA_SAIDA-EBELP    = WA_ZSDT0062-EBELP.
    WA_SAIDA-EBELN_T  = WA_ZSDT0062-EBELN_T.
    WA_SAIDA-EBELP_T  = WA_ZSDT0062-EBELP_T.
    WA_SAIDA-VBELN    = WA_ZSDT0062-VBELN.
    WA_SAIDA-POSNR    = WA_ZSDT0062-POSNR.


    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZSDT0062-KUNNR.
    WA_SAIDA-DESC_CLIENTE = WA_KNA1-NAME1.

      SHIFT WA_ZSDT0062-KUNNR LEFT DELETING LEADING '0'.
      WA_SAIDA-KUNNR    = WA_ZSDT0062-KUNNR.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZSDT0062-LIFNR.
    WA_SAIDA-DESC_FORN = WA_LFA1-NAME1.

      SHIFT WA_ZSDT0062-LIFNR LEFT DELETING LEADING '0'.
      WA_SAIDA-LIFNR    = WA_ZSDT0062-LIFNR.

    WA_SAIDA-RESWK    = WA_ZSDT0062-RESWK.

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_ZSDT0062-RESWK.
    WA_SAIDA-DESC_FORNT = WA_T001W-NAME1.

    WA_SAIDA-QTD_VINC = WA_ZSDT0062-QTD_VINC.



    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZSDT0062-MATNR.
    WA_SAIDA-DESC_MATERIAL = WA_MAKT-MAKTX.

      SHIFT WA_ZSDT0062-MATNR LEFT DELETING LEADING '0'.
      WA_SAIDA-MATNR    = WA_ZSDT0062-MATNR.


    WA_SAIDA-LOCAL_EMBARQUE = WA_ZSDT0062-LOCAL_EMBARQUE.
    WA_SAIDA-NRO_SOL        = WA_ZSDT0062-NRO_SOL.
    WA_SAIDA-NRO_CG         = WA_ZSDT0062-NRO_CG.
    WA_SAIDA-MBLNR          = WA_ZSDT0062-MBLNR.
    WA_SAIDA-NR_FORN        = WA_ZSDT0062-NR_FORN.

    IF WA_ZSDT0062-ENVIO_EMAIL = 'P'.
      WA_SAIDA-ENVIO_EMAIL    = 'Pendente'.
    ELSE.
      WA_SAIDA-ENVIO_EMAIL    = 'Enviado'.
    ENDIF.

    WA_SAIDA-STATUS         = WA_ZSDT0062-STATUS.

    APPEND WA_SAIDA TO IT_SAIDA.

    SORT IT_SAIDA BY SOL_VINC.

    CLEAR WA_SAIDA.     CLEAR WA_KNA1.  CLEAR WA_T001W.
    CLEAR WA_ZSDT0062.  CLEAR WA_LFA1.  CLEAR WA_MAKT.

  ENDLOOP.

  IF IT_SAIDA[] IS INITIAL.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Sem dados para o filtro informado!'.
    STOP.
  ENDIF.

ENDFORM.                    " F_AGRUPA_DADOS


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_REPORT
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT_REPORT.

    PERFORM Z_HOTSPOT_REPORT USING  E_ROW_ID E_COLUMN_ID ES_ROW_NO.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_0001
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_0001.
  REFRESH: IT_FCAT[].

  PERFORM ALV_CATALOG_0001  USING:

        'SOL_VINC'              'N° Vinculação  '             '08'       ''  'X' ''  ''      'MATN1' ,
        'DT_VINC'               'Data Vinculação'             '10'       ''  ''  ''  ''      ''      ,
        'EBELN'                 'N° Pedido de Compra'         '10'       'X'  ''  ''  ''      ''      ,
        'EBELP'                 'Item P.C.'                   '08'       ''  ''  ''  ''       ''      ,
        'EBELN_T'               'N° Pedido Transf.'           '10'       'X'  ''  ''  ''       ''      ,
        'EBELP_T'               'Item P.T.'                   '08'       ''  ''  ''  ''       ''      ,
        'VBELN'                 'N° Ordem de Venda'           '10'       'X'  ''  ''  ''      ''      ,
        'POSNR'                 'Item O.V.'                   '08'       ''  ''  ''  ''       ''      ,
        'KUNNR'                 'Cliente'                     '06'       ''  ''  ''  ''      ''      ,
        'DESC_CLIENTE'          'Desc. Cliente'               '15'       ''  ''  ''  ''       ''      ,
        'LIFNR'                 'Fornecedor'                  '08'       ''  ''  ''  ''      ''      ,
        'DESC_FORN'             'Desc. Fornecedor'            '15'       ''  ''  ''  ''       ''      ,
        'RESWK'                 'Forn. Transf.'               '08'       ''  ''  ''  ''      ''      ,
        'DESC_FORNT'            'Desc. Forn. Transf.'         '15'       ''  'X' ''  ''      'MATN1' ,
        'QTD_VINC'              'Qtd. Vinculada'              '08'       ''  ''  ''  ''      ''      ,
        'MATNR'                 'Material'                    '08'       ''  ''  ''  ''      ''      ,
        'DESC_MATERIAL'         'Desc. Material'              '10'       ''  ''  ''  ''       ''      ,
        'LOCAL_EMBARQUE'        'Local Embarque'              '10'       ''  ''  ''  ''       ''      ,
        'NRO_SOL'               'N° Sol. Entrega'             '05'       ''  ''  ''  ''       ''      ,
        'NRO_CG'                'N° Carga'                    '05'       ''  ''  ''  ''      ''      ,
        'MBLNR'                 'Doc. Material'               '05'       ''  ''  ''  ''       ''      ,
        'NR_FORN'               'N° Interno Forn.'            '05'       ''  ''  ''  ''      ''      ,
        'ENVIO_EMAIL'           'E-mail'                      '13'       ''  ''  ''  ''       ''      ,
        'STATUS'                'Status'                      '04'      ''  ''  ''  ''      ''      .



ENDFORM.                    " CRIAR_CATALOG_0200
*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_0001
*&---------------------------------------------------------------------*
FORM ALV_CATALOG_0001  USING   P_CAMPO    TYPE C
                               P_DESC     TYPE C
                               P_TAM      TYPE C
                               P_HOT      TYPE C
                               P_ZERO     TYPE C
                               P_SUM      TYPE C
                               P_COR      TYPE C
                               P_CONVEXIT TYPE C.


  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-DO_SUM   =  P_SUM.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-EMPHASIZE = P_COR.
  WL_FCAT-CONVEXIT  = P_CONVEXIT.

  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_CATALOG_0100


FORM CRIAR_ALV_0001.

  DATA: WA_EVENT_0001    TYPE REF TO LCL_EVENT_RECEIVER.


  DATA:   G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
          DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
          PICTURE            TYPE REF TO CL_GUI_PICTURE,
*          CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID,
          DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
          TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN             TYPE REF TO CL_DD_AREA,
          TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN_1           TYPE REF TO CL_DD_AREA,
          COLUMN_2           TYPE REF TO CL_DD_AREA,
          DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER.
*          GS_LAYOUT          TYPE LVC_S_LAYO,
*          GS_VARIANT         TYPE DISVARIANT,
*          IT_FIELDCATALOG    TYPE LVC_T_FCAT,
*          CK_CONFIRMADO      TYPE CHAR01.



  IF WA_CONTAINER_0001 IS INITIAL.

    CREATE OBJECT WA_CONTAINER_0001
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_PRINCIPAL'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = WA_CONTAINER_0001
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_1
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_2A.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 13.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 40.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_2A.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.


    CREATE OBJECT WA_ALV_0001
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    IF WA_EVENT_0001 IS INITIAL.
      CREATE OBJECT WA_EVENT_0001.
      SET HANDLER: WA_EVENT_0001->ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0001.
    ENDIF.

    "WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.
    WA_LAYOUT-CWIDTH_OPT = 'X'.
    WA_LAYOUT-SEL_MODE   = 'A'.

*
    CALL METHOD WA_ALV_0001->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        IS_VARIANT      = GS_VARIANT_C
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FCAT
        IT_OUTTAB       = IT_SAIDA[].

    CALL METHOD WA_ALV_0001->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0001.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.
*
    CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.
*
    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 1
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT.
*
    CALL METHOD TABLE_ELEMENT->ADD_COLUMN
      IMPORTING
        COLUMN = COLUMN.

    CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
      EXPORTING
        COL_NO    = 1
        "SAP_ALIGN = 'CENTER'
        SAP_STYLE = CL_DD_DOCUMENT=>HEADING.


    CLEAR: P_TEXT_TABLE.
    PERFORM ORGANIZA_CABECALHO.

    CALL METHOD COLUMN->ADD_TEXT
      EXPORTING
        TEXT      = P_TEXT
        SAP_STYLE = 'HEADING'.

    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 2
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT2.

    CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
      EXPORTING
        SAP_STYLE   = 'SAP_BOLD'
        STYLE_CLASS = 'SAP_BOLD'
      IMPORTING
        COLUMN      = COLUMN_1.


    CALL METHOD COLUMN_1->ADD_TEXT
      EXPORTING
        TEXT_TABLE = P_TEXT_TABLE
        FIX_LINES  = 'X'.

    CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_2.

    DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

    CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = DG_PARENT_2
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.
  ELSE.
    CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_0001


*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_REPORT
*&---------------------------------------------------------------------*
FORM Z_HOTSPOT_REPORT USING    P_E_ROW_ID    TYPE  LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO   TYPE  LVC_S_ROID.

  IF ( SY-SUBRC EQ 0 ).
    CASE P_E_COLUMN_ID.
      WHEN: 'EBELN'.
        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
        SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      WHEN: 'EBELN_T'.
        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
        SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN_T.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      WHEN: 'VEBELN'.
        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
        SET PARAMETER ID 'VL' FIELD WA_SAIDA-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.

ENDFORM.                    " Z_HOTSPOT_COMPRAS


MODULE STATUS_0001 OUTPUT.
  SET PF-STATUS 'ST001'.
  SET TITLEBAR 'T001'.
ENDMODULE.


MODULE USER_COMMAND_0001 INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANCEL'.
      CALL SELECTION-SCREEN 1000.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.


MODULE CRIAR_ALV_0001 OUTPUT.

  PERFORM CRIAR_CATALOG_0001.
  PERFORM CRIAR_ALV_0001.

ENDMODULE.


FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

  DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
          LINE(255) TYPE X,
        END OF GRAPHIC_TABLE.

  DATA: L_GRAPHIC_XSTR TYPE XSTRING.
  DATA: GRAPHIC_SIZE   TYPE I.
  DATA: L_GRAPHIC_CONV TYPE I.
  DATA: L_GRAPHIC_OFFS TYPE I.

  REFRESH GRAPHIC_TABLE.

  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.

  WHILE L_GRAPHIC_CONV > 255.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  ENDWHILE.

  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.

ENDFORM.                    " F_PEGA_IMAGEM


FORM ORGANIZA_CABECALHO.

  DTVINC_I = |{ P_DTVINC-LOW+6(2) }.{ P_DTVINC-LOW+4(2) }.{ P_DTVINC-LOW+0(4) }|.
  DTVINC_F = |{ P_DTVINC-HIGH+6(2) }.{ P_DTVINC-HIGH+4(2) }.{ P_DTVINC-HIGH+0(4) }|.

*  SDYDO_TEXT_ELEMENT = 'Relatório de Solicitação de Alteração de Preço de Frete: ____________________________________________________'.
*  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  CLEAR: SDYDO_TEXT_ELEMENT.
*
  PERFORM FORMA_CABECALHO USING P_EBELN  P_EBELN-OPTION  P_EBELN-LOW  P_EBELN-HIGH  TEXT-002.
  PERFORM FORMA_CABECALHO USING P_EBELNT P_EBELNT-OPTION P_EBELNT-LOW P_EBELNT-HIGH TEXT-003.
  PERFORM FORMA_CABECALHO USING P_VBELN  P_VBELN-OPTION  P_VBELN-LOW  P_VBELN-HIGH  TEXT-004.
  PERFORM FORMA_CABECALHO USING P_MATNR  P_MATNR-OPTION  P_MATNR-LOW  P_MATNR-HIGH  TEXT-005.
  PERFORM FORMA_CABECALHO USING P_KUNNR  P_KUNNR-OPTION  P_KUNNR-LOW  P_KUNNR-HIGH  TEXT-006.
  PERFORM FORMA_CABECALHO USING P_LIFNR  P_LIFNR-OPTION  P_LIFNR-LOW  P_LIFNR-HIGH  TEXT-007.
  PERFORM FORMA_CABECALHO USING P_RESWK  P_RESWK-OPTION  P_RESWK-LOW  P_RESWK-HIGH  TEXT-008.
  PERFORM FORMA_CABECALHO USING P_LOCAL  P_LOCAL-OPTION  P_LOCAL-LOW  P_LOCAL-HIGH  TEXT-009.
  PERFORM FORMA_CABECALHO USING P_STATUS P_STATUS-OPTION P_STATUS-LOW P_STATUS-HIGH TEXT-010.
  PERFORM FORMA_CABECALHO USING P_DTVINC P_DTVINC-OPTION DTVINC_I     DTVINC_F      TEXT-011.

ENDFORM.


FORM FORMA_CABECALHO  USING    P_STATUS
                               P_STATUS_OPTION
                               P_STATUS_LOW
                               P_STATUS_HIGH
                               P_TEXT_006.

  IF P_STATUS IS NOT INITIAL.
    IF P_STATUS_OPTION NE 'EQ' AND P_STATUS_OPTION NE 'BT'.
      SDYDO_TEXT_ELEMENT = P_TEXT_006.
      EXIT.
    ELSEIF P_STATUS_OPTION EQ 'BT'.
      SDYDO_TEXT_ELEMENT = | { P_TEXT_006 } { P_STATUS_LOW } - { P_STATUS_HIGH } |.
    ELSE.
      SDYDO_TEXT_ELEMENT = | { P_TEXT_006 } { P_STATUS_LOW } |.
    ENDIF.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR SDYDO_TEXT_ELEMENT.
*  ELSE.
*    SDYDO_TEXT_ELEMENT = P_TEXT_006.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*    CLEAR: SDYDO_TEXT_ELEMENT.
  ENDIF.

ENDFORM.

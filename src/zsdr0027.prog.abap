************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 17.04.2013                                          *
* Objetivo    ...: Liberação de Pedidos de Embarque - INSUMOS          *
* Transação   ...: ZSDT0067                                            *
************************************************************************
REPORT  ZSDR0027.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON.
*----------------------------------------------------------------------*
* TABLES
*---------------------------------------------------------------------- *
TABLES: VBAK, VBAP, EKKO, KNA1, LFA1, ZSDT0062.
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
    MATKL TYPE EKPO-MATKL, " Grupo de mercadorias
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

    MATNR             TYPE VBAP-MATNR,
    MAKTX             TYPE MAKT-MAKTX,

    " Compra
    EBELN_C           TYPE EKKO-EBELN,
    BSART_C           TYPE EKKO-BSART,
    BUKRS_C           TYPE EKKO-BUKRS,
    AEDAT_C           TYPE EKKO-AEDAT,
    WAERS_C           TYPE EKKO-WAERS,
    VGABE_C           TYPE EKBE-VGABE,
    MENGE_C           TYPE EKBE-MENGE,
    SHKZG_C           TYPE EKBE-SHKZG,
    BELNR_C           TYPE EKBE-BELNR,
    BUDAT_C           TYPE EKBE-BUDAT,
    DMBTR_C           TYPE EKBE-DMBTR,
    MATNR_C           TYPE EKBE-MATNR,
    EBELP_C           TYPE EKPO-EBELP,
    TXZ01_C           TYPE EKPO-TXZ01,
    NETWR_C           TYPE EKPO-NETWR,
    LOEKZ_C           TYPE EKPO-LOEKZ,
    MEINS_C           TYPE EKPO-MEINS,
    NETPR_C           TYPE EKPO-NETPR,
    GJAHR_C           TYPE RBKP-GJAHR,
    XBLNR_C           TYPE RBKP-XBLNR,
    LIFNR_C           TYPE LFA1-LIFNR,
    NAME1_C           TYPE LFA1-NAME1,


    " Venda
    VBELN_V           TYPE VBAK-VBELN,
    VKORG_V           TYPE VBAK-VKORG,
    VTWEG_V           TYPE VBAK-VTWEG,
    SPART_V           TYPE VBAK-SPART,
    AUART_V           TYPE VBAK-AUART,
    VKBUR_V           TYPE VBAK-VKBUR,
    WAERK_V           TYPE VBAK-WAERK,
    ERDAT_V           TYPE VBAK-ERDAT,
    FAKSK_V           TYPE VBAK-FAKSK,
    LIFSK_V           TYPE VBAK-LIFSK,
    AUDAT_V           TYPE VBAK-AUDAT,
    ARKTX_V           TYPE VBAP-ARKTX,
    WERKS_V           TYPE VBAP-WERKS,
    ZMENG_V           TYPE VBAP-ZMENG,
    NETWR_V           TYPE VBAP-NETWR,
    KWMENG_V          TYPE VBAP-KWMENG,
    VRKME_V           TYPE VBAP-VRKME,
    POSNR_V           TYPE VBAP-POSNR,
    RFMNG_V           TYPE VBFA-RFMNG,
    VBTYP_N_V         TYPE VBFA-VBTYP_N,
    VBELV_V           TYPE VBFA-VBELV,
    VBTYP_V_V         TYPE VBFA-VBTYP_V,
    MEINS_V           TYPE VBFA-MEINS,
    KUNNR_V           TYPE KNA1-KUNNR,
    NAME1_V           TYPE KNA1-NAME1,
    KBETR_V           TYPE KONV-KBETR,
    LIFSP_V           TYPE VBEP-LIFSP,

    " Transferência
    EBELN_T           TYPE EKKO-EBELN,
    BSART_T           TYPE EKKO-BSART,
    BUKRS_T           TYPE EKKO-BUKRS,
    AEDAT_T           TYPE EKKO-AEDAT,
    WAERS_T           TYPE EKKO-WAERS,
    VGABE_T           TYPE EKBE-VGABE,
    MENGE_T           TYPE EKBE-MENGE,
    SHKZG_T           TYPE EKBE-SHKZG,
    BELNR_T           TYPE EKBE-BELNR,
    BUDAT_T           TYPE EKBE-BUDAT,
    DMBTR_T           TYPE EKBE-DMBTR,
    MATNR_T           TYPE EKBE-MATNR,
    EBELP_T           TYPE EKPO-EBELP,
    TXZ01_T           TYPE EKPO-TXZ01,
    NETWR_T           TYPE EKPO-NETWR,
    LOEKZ_T           TYPE EKPO-LOEKZ,
    MEINS_T           TYPE EKPO-MEINS,
    NETPR_T           TYPE EKPO-NETPR,
    GJAHR_T           TYPE RBKP-GJAHR,
    XBLNR_T           TYPE RBKP-XBLNR,
    RESWK_T           TYPE EKKO-RESWK,
    NAME1_T           TYPE LFA1-NAME1,

    " Produzido
    SPTAG_P           TYPE S225-SPTAG,  "#EC CI_USAGE_OK[2268063]
    WERKS_P           TYPE S225-WERKS,  "#EC CI_USAGE_OK[2268063]
    MATNR_P           TYPE S225-MATNR,  "#EC CI_USAGE_OK[2268063]
    "wemng_p        type s225-wemng,
    WEMNG_P           TYPE EKPO-MENGE,

    TOTAL_VENDA       TYPE DB20199VP,
    TOTAL_COMPRA      TYPE DB20199VP,
    TOTAL_TRANSF      TYPE DB20199VP,
    "total_produzido_p type s225-wemng,
    TOTAL_PRODUZIDO_P TYPE DB20199VP,
    SALDO             TYPE DB20199VP,
    TOTAL_F_C         TYPE DB20199VP,
    SALDO_C           TYPE DB20199VP,
    TOTAL_F_V         TYPE DB20199VP,
    SALDO_V           TYPE DB20199VP,
    SALDO_T           TYPE DB20199VP,
    TOTAL_F_T         TYPE DB20199VP,
    CATEGORIA         TYPE C,

    CELLCOLORS        TYPE LVC_T_SCOL,

  END OF TY_SAIDA,

  BEGIN OF TY_SAIDA_MM,
    MATNR       TYPE  VBAP-MATNR,
    EBELN       TYPE  EKPO-EBELN,
    MENGE       TYPE  DB20199VP,
    NETWR       TYPE  DB20199VP,
    LIFNR       TYPE  LFA1-LIFNR,
    NAME1       TYPE  LFA1-NAME1,
    WAERS       TYPE  EKKO-WAERS,
    MEINS       TYPE  EKPO-MEINS,
    EBELP       TYPE  EKPO-EBELP,
    TXZ01       TYPE  EKPO-TXZ01,
    MAKTX       TYPE  MAKT-MAKTX,
    NETPR       TYPE EKPO-NETPR,
    VINCULAR_C  TYPE ZFIT0034-STATUS,
    QTD_VINC_MM TYPE DB20199VP,
    SALDO_MM    TYPE DB20199VP,
    WERKS       TYPE EKPO-WERKS,
  END OF TY_SAIDA_MM,

  BEGIN OF TY_SAIDA_SD,

    VINCULAR_V  TYPE ZFIT0034-STATUS,
    KUNNR       TYPE KNA1-KUNNR,
    NAME1       TYPE KNA1-NAME1,
    VBELN       TYPE VBAK-VBELN,
    ZMENG       TYPE VBAP-ZMENG,
    VRKME       TYPE VBAP-VRKME,

    STATUS,
    ZTERM(30),
    NETWR       TYPE VBAK-NETWR,
    WAERK       TYPE VBAK-WAERK,
    KWMENG      TYPE DB20199VP,
    LIFSK       TYPE VBAK-LIFSK,
    POSNR       TYPE VBAP-POSNR,
    MATNR       TYPE VBAP-MATNR,
    ARKTX       TYPE VBAP-ARKTX,
    VKBUR       TYPE VBAK-VKBUR,
    NETPR       TYPE VBAP-NETPR,
    KBETR       TYPE KONV-KBETR,
    QTD_VINC_SD TYPE DB20199VP,
    SALDO_SD    TYPE DB20199VP,
  END OF TY_SAIDA_SD,

  BEGIN OF TY_SAIDA_TRANSF,
    MATNR      TYPE  VBAP-MATNR,
    EBELN      TYPE  EKPO-EBELN,
    MENGE      TYPE  DB20199VP,
    NETWR      TYPE  DB20199VP,
    RESWK      TYPE  EKKO-RESWK,
    NAME1      TYPE  LFA1-NAME1,
    WAERS      TYPE  EKKO-WAERS,
    MEINS      TYPE  EKPO-MEINS,
    EBELP      TYPE  EKPO-EBELP,
    TXZ01      TYPE  EKPO-TXZ01,
    MAKTX      TYPE  MAKT-MAKTX,
    NETPR      TYPE EKPO-NETPR,
    VINCULAR_T TYPE ZFIT0034-STATUS,
    QTD_VINC_T TYPE DB20199VP,
    SALDO_T    TYPE DB20199VP,
  END OF TY_SAIDA_TRANSF,


  BEGIN OF TY_SAIDA_VINC,
    EBELN      TYPE EKPO-EBELN, "Número do Pedido
    EBELP      TYPE EKPO-EBELP, "Item do Pedido
    LIFNR      TYPE LFA1-LIFNR, "Código do Fornecedor
    NAME       TYPE LFA1-NAME1, "Descrição do Fornecedor
    EBELN_T    TYPE EKPO-EBELN, "Número do Pedido Transf
    EBELP_T    TYPE EKPO-EBELP, "Item do Pedido Transf
    RESWK_T    TYPE EKKO-RESWK, "Código do Fornecedor Transf
    NAME_T     TYPE LFA1-NAME1, "Descrição do Fornecedor Transf
    KUNNR      TYPE KNA1-KUNNR, "Código do Cliente
    NAME1      TYPE KNA1-NAME1, "Descrição do Cliente
    VKBUR      TYPE VBAK-VKBUR, "Escritorio de Vendas
    VBELN      TYPE VBAK-VBELN, "Ordem de Venda
    POSNR      TYPE VBAP-POSNR, "Item de Venda
    MATNR      TYPE VBAP-MATNR, "Material
    QTD_V(15)  TYPE P DECIMALS 2 , "Quantidade Vinculada
    DATA       TYPE SY-DATUM,  "Data da Vinculação
    LOCAL_EMB  TYPE CHAR25, "Local de Embarque
    STATUS     TYPE CHAR4, "Status da Operação
    STATUS_AUX TYPE C,
    SOL_VINC   TYPE ZSDT0062-SOL_VINC,
  END OF TY_SAIDA_VINC,

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
    SPTAG           TYPE S225-SPTAG,    "#EC CI_USAGE_OK[2268063]
    WERKS           TYPE S225-WERKS,    "#EC CI_USAGE_OK[2268063]
    MATNR           TYPE S225-MATNR,    "#EC CI_USAGE_OK[2268063]
    WEMNG           TYPE S225-WEMNG,    "#EC CI_USAGE_OK[2268063]
    AMEIN           TYPE S225-AMEIN,    "#EC CI_USAGE_OK[2268063]
    MENGE           TYPE EKPO-MENGE,
    TOTAL_PRODUZIDO TYPE S225-WEMNG,    "#EC CI_USAGE_OK[2268063]

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


  BEGIN OF TY_FATURADO,
    EBELN          TYPE EKKO-EBELN,
    VALOR_FATURADO TYPE EKBE-MENGE,
    MATNR          TYPE EKBE-MATNR,
    VALOR_TOTAL    TYPE EKKO-EBELN,
  END OF TY_FATURADO,

  BEGIN OF TY_FATURADO_VENDA,
    VBELV          TYPE VBFA-VBELV,
    VALOR_FATURADO TYPE EKBE-MENGE,
    MATNR          TYPE VBFA-MATNR,
    VALOR_TOTAL    TYPE VBFA-RFMNG,
  END OF TY_FATURADO_VENDA,

  BEGIN OF TY_FATURADO_T,
    EBELN          TYPE EKKO-EBELN,
    VALOR_FATURADO TYPE EKBE-MENGE,
    MATNR          TYPE EKBE-MATNR,
    VALOR_TOTAL    TYPE EKKO-EBELN,
  END OF TY_FATURADO_T,


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


  BEGIN OF TY_COMPRA,
    EBELN           TYPE EKKO-EBELN,
    BSART           TYPE EKKO-BSART,
    BUKRS           TYPE EKKO-BUKRS,
    AEDAT           TYPE EKKO-AEDAT,
    WAERS           TYPE EKKO-WAERS,
    VGABE           TYPE EKBE-VGABE,
    MENGE           TYPE EKBE-MENGE,
    MENGE_F         TYPE EKBE-MENGE,
    SHKZG           TYPE EKBE-SHKZG,
    BELNR           TYPE EKBE-BELNR,
    BUDAT           TYPE EKBE-BUDAT,
    DMBTR           TYPE EKBE-DMBTR,
    MATNR           TYPE EKBE-MATNR,
    EBELP           TYPE EKPO-EBELP,
    TXZ01           TYPE EKPO-TXZ01,
    NETWR           TYPE EKPO-NETWR,
    LOEKZ           TYPE EKPO-LOEKZ,
    MEINS           TYPE EKPO-MEINS,
    NETPR           TYPE EKPO-NETPR,
    GJAHR           TYPE RBKP-GJAHR,
    XBLNR           TYPE RBKP-XBLNR,
    LIFNR           TYPE LFA1-LIFNR,
    NAME1           TYPE LFA1-NAME1,
    MAKTX           TYPE MAKT-MAKTX,
    WERKS           TYPE EKPO-WERKS,

    TOTAL_COMPRA    TYPE EKBE-MENGE,
    TOTAL_PRODUZIDO TYPE EKBE-MENGE,
  END OF TY_COMPRA,

  BEGIN OF TY_VENDA,
    VBELN           TYPE VBAK-VBELN,
    VKORG           TYPE VBAK-VKORG,
    VTWEG           TYPE VBAK-VTWEG,
    SPART           TYPE VBAK-SPART,
    AUART           TYPE VBAK-AUART,
    VKBUR           TYPE VBAK-VKBUR,
    WAERK           TYPE VBAK-WAERK,
    ERDAT           TYPE VBAK-ERDAT,
    FAKSK           TYPE VBAK-FAKSK,
    LIFSK           TYPE VBAK-LIFSK,
    AUDAT           TYPE VBAK-AUDAT,
    MATNR           TYPE VBAP-MATNR,
    ARKTX           TYPE VBAP-ARKTX,
    WERKS           TYPE VBAP-WERKS,
    ZMENG           TYPE VBAP-ZMENG,
    NETWR           TYPE VBAP-NETWR,
    KWMENG          TYPE VBAP-KWMENG,
    VRKME           TYPE VBAP-VRKME,
    POSNR           TYPE VBAP-POSNR,
    NETPR           TYPE VBAP-NETPR,
    RFMNG           TYPE VBFA-RFMNG,
    VBTYP_N         TYPE VBFA-VBTYP_N,
    VBELV           TYPE VBFA-VBELV,
    VBTYP_V         TYPE VBFA-VBTYP_V,
    MEINS           TYPE VBFA-MEINS,
    KUNNR           TYPE KNA1-KUNNR,
    NAME1           TYPE KNA1-NAME1,
    MAKTX           TYPE MAKT-MAKTX,
    KBETR           TYPE KONV-KBETR,
    LIFSP           TYPE VBEP-LIFSP,

    TOTAL_VENDA     TYPE VBAP-ZMENG,
    TOTAL_PRODUZIDO TYPE VBAP-ZMENG,

  END OF TY_VENDA,

  BEGIN OF TY_TRANSF,
    EBELN           TYPE EKKO-EBELN,
    BSART           TYPE EKKO-BSART,
    BUKRS           TYPE EKKO-BUKRS,
    AEDAT           TYPE EKKO-AEDAT,
    WAERS           TYPE EKKO-WAERS,
    VGABE           TYPE EKBE-VGABE,
    MENGE           TYPE EKBE-MENGE,
    MENGE_F         TYPE EKBE-MENGE,
    SHKZG           TYPE EKBE-SHKZG,
    BELNR           TYPE EKBE-BELNR,
    BUDAT           TYPE EKBE-BUDAT,
    DMBTR           TYPE EKBE-DMBTR,
    MATNR           TYPE EKBE-MATNR,
    EBELP           TYPE EKPO-EBELP,
    TXZ01           TYPE EKPO-TXZ01,
    NETWR           TYPE EKPO-NETWR,
    LOEKZ           TYPE EKPO-LOEKZ,
    MEINS           TYPE EKPO-MEINS,
    NETPR           TYPE EKPO-NETPR,
    GJAHR           TYPE RBKP-GJAHR,
    XBLNR           TYPE RBKP-XBLNR,
    RESWK           TYPE EKKO-RESWK,
    NAME1           TYPE LFA1-NAME1,
    MAKTX           TYPE MAKT-MAKTX,

    TOTAL_COMPRA    TYPE EKBE-MENGE,
    TOTAL_PRODUZIDO TYPE EKBE-MENGE,
  END OF TY_TRANSF,

  BEGIN OF TY_ZSDT0041,
    VBELN         TYPE ZSDT0041-VBELN,
    DOC_SIMULACAO TYPE ZSDT0041-DOC_SIMULACAO,
  END OF TY_ZSDT0041,

  BEGIN OF TY_ZSDT0040,
    DOC_SIMULACAO TYPE ZSDT0040-DOC_SIMULACAO,
    SAFRA         TYPE ZSDT0040-SAFRA,
  END OF TY_ZSDT0040,

  BEGIN OF TY_ZSDT0062,
    SOL_VINC       TYPE ZSDT0062-SOL_VINC,
    EBELN          TYPE ZSDT0062-EBELN,
    EBELP          TYPE ZSDT0062-EBELP,
    EBELN_T        TYPE ZSDT0062-EBELN_T,
    EBELP_T        TYPE ZSDT0062-EBELP_T,
    RESWK          TYPE EKKO-RESWK,
    VBELN          TYPE ZSDT0062-VBELN,
    POSNR          TYPE ZSDT0062-POSNR,
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

  END OF TY_ZSDT0062.


"--------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:
  IT_SAIDA               TYPE TABLE OF TY_SAIDA WITH HEADER LINE,    "
  IT_SAIDA_AUX           TYPE TABLE OF TY_SAIDA WITH HEADER LINE,    "
  IT_SAIDA_SOMA          TYPE TABLE OF TY_SAIDA,    "

  IT_SAIDA_T             TYPE TABLE OF TY_SAIDA WITH HEADER LINE,    "
  IT_SAIDA_T_AUX         TYPE TABLE OF TY_SAIDA WITH HEADER LINE,    "
  IT_SAIDA_T_SOMA        TYPE TABLE OF TY_SAIDA,    "

  IT_SAIDA_MM            TYPE TABLE OF TY_SAIDA_MM, "
  IT_SAIDA_SD            TYPE TABLE OF TY_SAIDA_SD, "
  IT_SAIDA_TRANSF        TYPE TABLE OF TY_SAIDA_TRANSF,
  IT_SAIDA_VINC          TYPE TABLE OF TY_SAIDA_VINC,
  IT_SAIDA_VINCULADOS    TYPE TABLE OF TY_SAIDA_VINC,
  T_BDC                  TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
  T_MESSTAB              TYPE TABLE OF BDCMSGCOLL,

  IT_EKKO                TYPE TABLE OF TY_EKKO, "
  IT_EKKO_FORN           TYPE TABLE OF TY_EKKO, "
  IT_EKKO_AUX            TYPE TABLE OF TY_EKKO, "

  IT_EKKO_T              TYPE TABLE OF TY_EKKO_T, "
  IT_EKKO_T_FORN         TYPE TABLE OF TY_EKKO_T, "
  IT_EKKO_T_AUX          TYPE TABLE OF TY_EKKO_T, "

  IT_EKPO                TYPE TABLE OF TY_EKPO, "
  IT_EKPO_AUX            TYPE TABLE OF TY_EKPO, "

  IT_EKPO_T              TYPE TABLE OF TY_EKPO_T, "
  IT_EKPO_T_AUX          TYPE TABLE OF TY_EKPO_T, "

  IT_VBAK                TYPE TABLE OF TY_VBAK, "
  IT_VBAK_AUX            TYPE TABLE OF TY_VBAK, "
  IT_VBAP                TYPE TABLE OF TY_VBAP, "
  IT_0026                TYPE TABLE OF ZFIT0026 WITH HEADER LINE, "
  IT_ZIB                 TYPE TABLE OF ZIB_CONTABIL_CHV WITH HEADER LINE, "
  IT_BSIK                TYPE TABLE OF BSIK WITH HEADER LINE, "
  IT_VBKD                TYPE TABLE OF VBKD WITH HEADER LINE, "
  IT_T052U               TYPE TABLE OF T052U WITH HEADER LINE, "
  IT_ZIB_BSIK            TYPE TABLE OF TY_ZIB_BSIK WITH HEADER LINE, "
  IT_KNA1                TYPE TABLE OF TY_KNA1, "
  IT_LFA1                TYPE TABLE OF TY_LFA1, "
  IT_T001W               TYPE TABLE OF TY_T001W,
  IT_VBEP                TYPE TABLE OF TY_VBEP,

  IT_S225                TYPE TABLE OF TY_S225, "
  IT_S225_AUX            TYPE TABLE OF TY_S225, "
  IT_S225_SOMA           TYPE TABLE OF TY_S225, "

  IT_KONV                TYPE TABLE OF TY_KONV,

  IT_MAKT                TYPE TABLE OF TY_MAKT,
  IT_RBKP                TYPE TABLE OF TY_RBKP,
  IT_RBKP_T              TYPE TABLE OF TY_RBKP,

  IT_COMPRA              TYPE TABLE OF TY_COMPRA,
  IT_COMPRA_GROUP        TYPE TABLE OF TY_COMPRA,
  IT_COMPRA_AUX          TYPE TABLE OF TY_COMPRA,
  IT_COMPRA_SOMA         TYPE TABLE OF TY_COMPRA,
  IT_COMPRA_FATURADO     TYPE TABLE OF TY_COMPRA,

  IT_VENDA               TYPE TABLE OF TY_VENDA,
  IT_VENDA_AUX           TYPE TABLE OF TY_VENDA,
  IT_VENDA_SOMA          TYPE TABLE OF TY_VENDA,

  IT_TRANSF              TYPE TABLE OF TY_TRANSF,
  IT_TRANSF_AUX          TYPE TABLE OF TY_TRANSF,
  IT_TRANSF_SOMA         TYPE TABLE OF TY_TRANSF,


  IT_MARA                TYPE TABLE OF TY_MARA, "
  IT_MARA_TRANSF         TYPE TABLE OF TY_MARA, "
  IT_MARA_VENDA          TYPE TABLE OF TY_MARA, "
  IT_FATURADO            TYPE TABLE OF TY_FATURADO,
  IT_FATURADO_AUX        TYPE TABLE OF TY_FATURADO,
  IT_FATURADO_SOMA       TYPE TABLE OF TY_FATURADO,

  IT_FATURADO_VENDA      TYPE TABLE OF TY_FATURADO_VENDA,
  IT_FATURADO_VENDA_AUX  TYPE TABLE OF TY_FATURADO_VENDA,
  IT_FATURADO_VENDA_SOMA TYPE TABLE OF TY_FATURADO_VENDA,

  IT_FATURADO_T          TYPE TABLE OF TY_FATURADO_T,
  IT_FATURADO_T_AUX      TYPE TABLE OF TY_FATURADO_T,
  IT_FATURADO_T_SOMA     TYPE TABLE OF TY_FATURADO_T,


  IT_J_1BBRANCH          TYPE TABLE OF TY_J_1BBRANCH,

  IT_VBFA                TYPE TABLE OF TY_VBFA,     "
  IT_VBFA_SOMA           TYPE TABLE OF TY_VBFA,     "
  IT_VBFA_AUX            TYPE TABLE OF TY_VBFA, "
  IT_SETLEAF_AUX         LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  IT_SETLEAF             TYPE TABLE OF TY_SETLEAF,


  IT_EKBE                TYPE TABLE OF TY_EKBE, "
  IT_EKBE_AUX            TYPE TABLE OF TY_EKBE, "

  IT_EKBE_T              TYPE TABLE OF TY_EKBE, "
  IT_EKBE_T_AUX          TYPE TABLE OF TY_EKBE, "

  IT_ZSDT0041            TYPE TABLE OF TY_ZSDT0041,
  IT_ZSDT0040            TYPE TABLE OF TY_ZSDT0040,
  IT_ZSDT0062            TYPE TABLE OF TY_ZSDT0062,
  IT_ZSDT0074            TYPE TABLE OF ZSDT0074.



*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:

  "Tela 0100
  WA_ALV_0200            TYPE REF TO CL_GUI_ALV_GRID,
  WA_CONTAINER_0200      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,


  WA_ALV_COMPRAS         TYPE REF TO CL_GUI_ALV_GRID,
  WA_ALV_VENDAS          TYPE REF TO CL_GUI_ALV_GRID,
  WA_ALV_TRANSF          TYPE REF TO CL_GUI_ALV_GRID,
  WA_ALV_VINC            TYPE REF TO CL_GUI_ALV_GRID,
  WA_ALV_VINCULADOS      TYPE REF TO CL_GUI_ALV_GRID,
  WA_STABLE              TYPE LVC_S_STBL,


  WA_CONT_COMPRAS        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_CONT_VENDAS         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_CONT_TRANSF         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_CONT_VINC           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_CONT_VINCULADOS     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

  WA_LAYOUT              TYPE LVC_S_LAYO,


  WA_LAYOUT_VINC         TYPE LVC_S_LAYO,

  WA_SAIDA               TYPE TY_SAIDA,
  WA_SAIDA_SOMA          TYPE TY_SAIDA,
  WA_SAIDA_AUX           TYPE TY_SAIDA,

  WA_SAIDA_MM            TYPE TY_SAIDA_MM,
  WA_SAIDA_SD            TYPE TY_SAIDA_SD,
  WA_SAIDA_TRANSF        TYPE TY_SAIDA_TRANSF,
  WA_SAIDA_VINC          TYPE TY_SAIDA_VINC,
  WA_SAIDA_VINCULADOS    TYPE TY_SAIDA_VINC,

  WA_EKKO                TYPE TY_EKKO, "
  WA_EKKO_FORN           TYPE TY_EKKO, "
  WA_EKKO_AUX            TYPE TY_EKKO, "

  WA_EKKO_T              TYPE TY_EKKO_T, "
  WA_EKKO_T_FORN         TYPE TY_EKKO_T, "
  WA_EKKO_T_AUX          TYPE TY_EKKO_T, "

  WA_EKPO                TYPE TY_EKPO, "
  WA_EKPO_AUX            TYPE TY_EKPO, "

  WA_EKPO_T              TYPE TY_EKPO_T, "
  WA_EKPO_T_AUX          TYPE TY_EKPO_T, "

  WA_VBAK                TYPE TY_VBAK, "
  WA_VBAK_AUX            TYPE TY_VBAK, "
  WA_VBAP                TYPE TY_VBAP, "
  WA_KNA1                TYPE TY_KNA1, "
  WA_LFA1                TYPE TY_LFA1, "
  WA_T001W               TYPE TY_T001W,

  WA_S225                TYPE TY_S225, "
  WA_S225_AUX            TYPE TY_S225, "
  WA_S225_SOMA           TYPE TY_S225, "


  WA_KONV                TYPE TY_KONV,
  WA_VBEP                TYPE TY_VBEP,

  WA_MAKT                TYPE TY_MAKT,
  WA_RBKP                TYPE TY_RBKP,
  WA_RBKP_T              TYPE TY_RBKP,
  WA_FATURADO            TYPE TY_FATURADO,
  WA_FATURADO_AUX        TYPE TY_FATURADO,
  WA_FATURADO_SOMA       TYPE TY_FATURADO,

  WA_FATURADO_T          TYPE TY_FATURADO,
  WA_FATURADO_T_AUX      TYPE TY_FATURADO,
  WA_FATURADO_T_SOMA     TYPE TY_FATURADO,

  WA_FATURADO_VENDA      TYPE TY_FATURADO_VENDA,
  WA_FATURADO_VENDA_AUX  TYPE TY_FATURADO_VENDA,
  WA_FATURADO_VENDA_SOMA TYPE TY_FATURADO_VENDA,


  WA_J_1BBRANCH          TYPE TY_J_1BBRANCH,

  WA_COMPRA              TYPE TY_COMPRA,
  WA_COMPRA_GROUP        TYPE TY_COMPRA,
  WA_COMPRA_AUX          TYPE TY_COMPRA,
  WA_COMPRA_SOMA         TYPE TY_COMPRA,
  WA_COMPRA_FATURADO     TYPE TY_COMPRA,

  WA_VENDA               TYPE TY_VENDA,
  WA_VENDA_AUX           TYPE TY_VENDA,
  WA_VENDA_SOMA          TYPE TY_VENDA,

  WA_TRANSF              TYPE TY_TRANSF,
  WA_TRANSF_AUX          TYPE TY_TRANSF,
  WA_TRANSF_SOMA         TYPE TY_TRANSF,

  WA_SETLEAF_AUX         TYPE SETLEAF,
  WA_SETLEAF             TYPE TY_SETLEAF,
  WA_MARA                TYPE TY_MARA,

  WA_VBFA                TYPE TY_VBFA, " Documento de vendas: dados de item
  WA_VBFA_SOMA           TYPE TY_VBFA, " Documento de vendas: dados de item
  WA_VBFA_AUX            TYPE TY_VBFA, " Documento de vendas: dados de item

  WA_EKBE                TYPE TY_EKBE, " Documento de vendas: dados de item
  WA_EKBE_AUX            TYPE TY_EKBE, " Documento de vendas: dados de item

  WA_EKBE_T              TYPE TY_EKBE, " Documento de vendas: dados de item
  WA_EKBE_T_AUX          TYPE TY_EKBE, " Documento de vendas: dados de item

  WA_ZSDT0041            TYPE TY_ZSDT0041,
  WA_ZSDT0040            TYPE TY_ZSDT0040,
  WA_ZSDT0062            TYPE TY_ZSDT0062,
  WA_ZSDT0074            TYPE ZSDT0074.

DATA: VALOR_CONVERSION_FATURADO TYPE EKPO-MENGE,
      VALOR                     TYPE EKPO-MENGE.



*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: IT_FCAT            TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT_COMPRAS    TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT_TRANSF     TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT_VENDAS     TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT_VINC       TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT_VINCULADOS TYPE TABLE OF LVC_S_FCAT,
      R_EKPO_S           TYPE RANGE OF EKPO-LOEKZ,
      WA_STATUS          LIKE LINE OF  R_EKPO_S,
      GS_VARIANT_C       TYPE DISVARIANT.

*----------------------------------------------------------------------*
* Estrutura para as Vinculações
*----------------------------------------------------------------------*

"Icones
DATA: ICON_VINCULACAO TYPE C LENGTH 4,
      ICON_LIFNR      TYPE C LENGTH 4,
      ICON_MATNR      TYPE C LENGTH 4,
      ICON_COMPRAS    TYPE C LENGTH 4,
      ICON_VENDAS     TYPE C LENGTH 4,
      ICON_TOTAL_C    TYPE C LENGTH 4,
      ICON_TOTAL_V    TYPE C LENGTH 4,
      ICON_EMAIL      TYPE C LENGTH 4,
      ICON_ASSUNTO    TYPE C LENGTH 4.


" Campos de Informações
DATA:   WA_LIFNR TYPE EKKO-LIFNR,
        WA_NAME1 TYPE LFA1-NAME1,
        WA_MATNR TYPE MARA-MATNR,
        WA_MAKTX TYPE MAKT-MAKTX,
        WA_MEINS TYPE MARA-MEINS,
        WA_RESWK TYPE EKKO-RESWK.


" Campos de Vinculação
DATA: WA_DATA     TYPE SY-DATUM,
      WA_LOCAL    TYPE C LENGTH 20,
      WA_SALDO_C  TYPE DB20199VP,
      WA_SALDO_V  TYPE DB20199VP,
      WA_SALDO_T  TYPE DB20199VP,
      WA_QTD_VINC TYPE DB20199VP.


"Campos Compras
DATA: WA_QTD_VENDA  TYPE DB20199VP,
      WA_QTD_VINC_C TYPE DB20199VP,
      WA_SALDO_T_C  TYPE DB20199VP.


"Campos Vendas
*DATA: wa_qtd_compra TYPE db20199vp,
*      wa_qtd_vinc_v TYPE db20199vp,
*      wa_saldo_t_v  TYPE db20199vp.

"Campos Transf
DATA: WA_QTD_COMPRA TYPE DB20199VP,
      WA_QTD_VINC_T TYPE DB20199VP,
      WA_SALDO_T_T  TYPE DB20199VP.

"Campos do E-mail
DATA: WA_EMAIL   TYPE C LENGTH 500,
      WA_ASSUNTO TYPE C LENGTH 300.


"Variaveis Vinculacao
DATA: WA_EDIT_VINC TYPE C,
      OK_CODE      LIKE SY-UCOMM,
      WA_OPCAO     TYPE C.

"ALV Editor Texto E-mail - Vinculação
DATA: CL_EDITOR        TYPE REF TO CL_GUI_TEXTEDIT,
      CONTAINER_EDITOR TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      TL_ROWS          TYPE LVC_T_ROW,
      SL_ROWS          TYPE LVC_S_ROW,
      TL_ROWS_AUX      TYPE LVC_T_ROW,
      SL_ROWS_AUX      TYPE LVC_S_ROW,
      LINES            TYPE SY-TABIX,
      LT_TAB           TYPE STANDARD TABLE OF CHAR0241,
      LA_TAB           LIKE LINE OF LT_TAB.

DATA: VINCULADOS_EMAIL TYPE C.



CLASS CL_GUI_CFW DEFINITION LOAD.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BLOCO1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BSART   FOR EKKO-BSART NO INTERVALS OBLIGATORY," Tipo de Pedido
                P_BSARTT  FOR EKKO-BSART NO INTERVALS OBLIGATORY, " Tipo de Pedito de Transf.
                P_AUART   FOR VBAK-AUART NO INTERVALS OBLIGATORY, " Tipo de Ordem
                P_VKORG   FOR VBAK-VKORG NO-EXTENSION NO INTERVALS OBLIGATORY," Organização de Vendas
                P_VTWEG   FOR VBAK-VTWEG NO-EXTENSION NO INTERVALS OBLIGATORY," Canal de Distribuição
                P_SPART   FOR VBAK-SPART NO INTERVALS OBLIGATORY," Setor de Atividade
                P_SAFRA   FOR EKKO-UNSEZ NO INTERVALS, " Safra
                "p_vkbur   FOR vbak-vkbur NO INTERVALS, " Escritório de Vendas
                P_ERDAT   FOR VBAK-ERDAT             , " Data do Contrato
                P_FATUR   FOR VBAK-ERDAT             , " Data do Faturamento
                "p_werks   FOR vbap-werks NO INTERVALS, " Centro
                P_MATNR   FOR VBAP-MATNR NO INTERVALS, " Material
                P_BUKRS   FOR EKKO-BUKRS NO INTERVALS. " Material
SELECTION-SCREEN: END OF BLOCK BLOCO1.

INITIALIZATION.
  GS_VARIANT_C-REPORT      = SY-REPID.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: F_SELECIONA_DADOS. " Form seleciona dados


  CONTROLS TABSTRIP TYPE TABSTRIP.
  CALL SCREEN 0100.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  PERFORM: CONDICOES_STATUS.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CONDICOES_STATUS
*&---------------------------------------------------------------------*
FORM CONDICOES_STATUS .

  WA_STATUS-SIGN   = 'I'.
  WA_STATUS-OPTION = 'EQ'.
  WA_STATUS-LOW = ''.

  APPEND WA_STATUS TO R_EKPO_S.

  " Produzido
  PERFORM: SELECIONAR_PRODUZIDO.
  " Compras
  PERFORM: SELECIONA_DADOS_COMPRAS.
  " Vendas
  PERFORM SELECIONA_DADOS_VENDAS.
  " Transferências
  PERFORM SELECIONA_DADOS_TRANSF.
  " Agrupamento
  PERFORM: AGRUPAMENTO.


ENDFORM.                    " CONDICOES_STATUS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_PRODUZIDO
*&---------------------------------------------------------------------*
FORM SELECIONAR_PRODUZIDO .
  DATA: TOTAL TYPE S225-WEMNG.   "#EC CI_USAGE_OK[2268063]

  CLEAR: IT_MAKT[].

  IF ( P_SPART-LOW EQ '04' ).

    " Produzido
    SELECT *
       FROM SETLEAF
       INTO TABLE IT_SETLEAF_AUX
     WHERE SETNAME EQ 'MAGGI_ZSDT0041'.

    IF NOT ( IT_SETLEAF_AUX[] IS INITIAL ).

      LOOP AT IT_SETLEAF_AUX INTO WA_SETLEAF_AUX.
        WA_SETLEAF-VALFROM_AUX = WA_SETLEAF_AUX-VALFROM.
        APPEND WA_SETLEAF TO IT_SETLEAF.
      ENDLOOP.

      SELECT MATKL MATNR
        FROM MARA
        INTO TABLE IT_MARA
        FOR ALL ENTRIES IN IT_SETLEAF
      WHERE MATKL EQ IT_SETLEAF-VALFROM_AUX
        AND MATNR IN P_MATNR.

      CHECK NOT IT_MARA[] IS INITIAL.


      " Goods receipts: repetitive manufacturing
      SELECT SPTAG WERKS MATNR WEMNG AMEIN
        FROM S225      "#EC CI_USAGE_OK[2268063]
        INTO TABLE IT_S225_AUX
        FOR ALL ENTRIES IN IT_MARA
      WHERE MATNR EQ IT_MARA-MATNR
        AND SPTAG IN P_ERDAT.
      "AND werks IN p_werks.


      "Local de negócios
      SELECT BUKRS BRANCH
        FROM J_1BBRANCH
        INTO TABLE IT_J_1BBRANCH
        FOR ALL ENTRIES IN IT_S225_AUX
      WHERE BRANCH EQ IT_S225_AUX-WERKS
        AND BUKRS IN P_BUKRS.

      " Descrição do Material
      SELECT MANDT MATNR SPRAS MAKTX MAKTG
        FROM MAKT
        INTO TABLE IT_MAKT
        FOR ALL ENTRIES IN IT_S225_AUX
     WHERE MATNR EQ IT_S225_AUX-MATNR.

      REFRESH: IT_S225_SOMA[].
      IT_S225_SOMA[] = IT_S225_AUX[].

      SORT: IT_MAKT       BY MATNR,
            IT_J_1BBRANCH BY BRANCH,
            IT_S225_AUX   BY MATNR,
            IT_S225_SOMA  BY MATNR.

      CLEAR: WA_SAIDA_AUX-CATEGORIA.
      LOOP AT IT_S225_AUX INTO WA_S225_AUX.
        LOOP AT IT_S225_SOMA INTO WA_S225_SOMA WHERE MATNR EQ WA_S225_AUX-MATNR
                                                 AND WERKS EQ WA_S225_AUX-WERKS.
          READ TABLE IT_J_1BBRANCH INTO WA_J_1BBRANCH WITH KEY BRANCH = WA_S225_SOMA-WERKS BINARY SEARCH.
          IF ( SY-SUBRC EQ 0 ).

            IF ( WA_S225_SOMA-AMEIN EQ 'KG' ).

              WA_S225_SOMA-MENGE = WA_S225_SOMA-WEMNG.
              CALL FUNCTION 'ME_CONVERSION_BPRME'
                EXPORTING
                  I_MATNR             = WA_S225_SOMA-MATNR
                  I_MEIN1             = 'KG'
                  I_MEIN2             = 'BAG'
                  I_MEINS             = 'KG'
                  I_MENGE             = WA_S225_SOMA-MENGE
                IMPORTING
                  MENGE               = WA_SAIDA_AUX-WEMNG_P
                EXCEPTIONS
                  ERROR_IN_CONVERSION = 01
                  NO_SUCCESS          = 02.

            ELSE.
              WA_SAIDA_AUX-WEMNG_P  = WA_S225_SOMA-WEMNG.
            ENDIF.

            WA_SAIDA_AUX-CATEGORIA = 'P'.
            WA_SAIDA_AUX-SPTAG_P  = WA_S225_SOMA-SPTAG.
            WA_SAIDA_AUX-WERKS_P  = WA_S225_SOMA-WERKS.

            READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_S225_SOMA-MATNR BINARY SEARCH.
            WA_SAIDA_AUX-MATNR  = WA_MAKT-MATNR.
            WA_SAIDA_AUX-MAKTX  = WA_MAKT-MAKTX.

            TOTAL = TOTAL + WA_SAIDA_AUX-WEMNG_P.
          ENDIF.
          CLEAR: WA_S225_SOMA.
        ENDLOOP.

        IF ( WA_SAIDA_AUX-CATEGORIA EQ 'P' ).
          WA_SAIDA_AUX-TOTAL_PRODUZIDO_P = TOTAL.
          APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.

          IF NOT ( IT_S225 IS INITIAL ).
            DELETE IT_S225 WHERE MATNR EQ WA_S225_SOMA-MATNR.
          ENDIF.
        ENDIF.
        CLEAR: WA_SAIDA_AUX, WA_S225_AUX, WA_S225_SOMA, TOTAL.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " SELECIONAR_PRODUZIDO
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_COMPRAS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS_COMPRAS .

  DATA: TOTAL   TYPE EKBE-MENGE,
        TOTAL_F TYPE EKBE-MENGE.

  " Cabeçalho do documento de compra
  SELECT EBELN BSART LIFNR BUKRS AEDAT WAERS UNSEZ
   FROM EKKO
   INTO TABLE IT_EKKO
  WHERE  BSART IN P_BSART
     AND AEDAT IN P_ERDAT
     AND BUKRS IN P_BUKRS
     AND UNSEZ IN P_SAFRA.

  CHECK NOT IT_EKKO[] IS INITIAL.

  " Item do documento de compras
  SELECT EBELN EBELP MATNR TXZ01 MENGE NETWR LOEKZ MEINS NETPR WERKS
   FROM EKPO
   INTO TABLE IT_EKPO
   FOR ALL ENTRIES IN IT_EKKO
  WHERE EBELN EQ IT_EKKO-EBELN
    AND LOEKZ IN R_EKPO_S
    AND MATNR IN P_MATNR.

  CHECK NOT IT_EKPO[] IS INITIAL.

  " Mestre de fornecedores (parte geral)
  SELECT LIFNR NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_EKKO
  WHERE LIFNR EQ IT_EKKO-LIFNR.

  " Descrição do Material
  SELECT MANDT MATNR SPRAS MAKTX MAKTG
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_EKPO
 WHERE MATNR EQ IT_EKPO-MATNR.


  SORT: IT_EKBE BY EBELN,
        IT_EKPO BY EBELN,
        IT_RBKP BY BELNR GJAHR,
        IT_LFA1 BY LIFNR,
        IT_MAKT BY MATNR.

  LOOP AT IT_EKKO INTO WA_EKKO.

    WA_COMPRA_AUX-EBELN = WA_EKKO-EBELN.
    WA_COMPRA_AUX-BSART = WA_EKKO-BSART.
    WA_COMPRA_AUX-BUKRS = WA_EKKO-BUKRS.
    WA_COMPRA_AUX-AEDAT = WA_EKKO-AEDAT.
    WA_COMPRA_AUX-WAERS = WA_EKKO-WAERS.

    READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_EKKO-EBELN BINARY SEARCH.

    IF ( SY-SUBRC EQ 0 ).
      WA_COMPRA_AUX-MENGE = WA_EKPO-MENGE.
      WA_COMPRA_AUX-EBELP = WA_EKPO-EBELP.
      WA_COMPRA_AUX-TXZ01 = WA_EKPO-TXZ01.
      WA_COMPRA_AUX-NETWR = WA_EKPO-NETWR.
      WA_COMPRA_AUX-LOEKZ = WA_EKPO-LOEKZ.
      WA_COMPRA_AUX-MEINS = WA_EKPO-MEINS.
      WA_COMPRA_AUX-NETPR = WA_EKPO-NETPR.
      WA_COMPRA_AUX-WERKS = WA_EKPO-WERKS.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR BINARY SEARCH.
      WA_COMPRA_AUX-MATNR = WA_MAKT-MATNR.
      WA_COMPRA_AUX-MAKTX = WA_MAKT-MAKTX.


      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR BINARY SEARCH.
      WA_COMPRA_AUX-LIFNR = WA_LFA1-LIFNR.
      WA_COMPRA_AUX-NAME1 = WA_LFA1-NAME1.

      APPEND WA_COMPRA_AUX TO IT_COMPRA_AUX.

    ENDIF.

    CLEAR: WA_EKKO,
           WA_EKBE,
           WA_LFA1,
           WA_COMPRA_AUX.
  ENDLOOP.

  CLEAR: WA_COMPRA_AUX.

  " Cabeçalho do documento de compra
  SELECT EBELN BSART LIFNR BUKRS AEDAT WAERS UNSEZ
   FROM EKKO
   INTO TABLE IT_EKKO_FORN
   FOR ALL ENTRIES IN IT_EKPO
  WHERE EBELN EQ IT_EKPO-EBELN
    AND UNSEZ IN P_SAFRA.


  SORT: IT_COMPRA_AUX BY MATNR MENGE EBELP,
          IT_MAKT     BY MATNR,
          IT_LFA1     BY LIFNR,
          IT_EKBE     BY EBELN,
          IT_RBKP     BY BELNR.

*---> 05/07/2023 - Migração S4 - DL
  SORT IT_EKKO_FORN BY EBELN.
  SORT IT_EKKO BY EBELN.
  SORT IT_LFA1 BY LIFNR.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT IT_EKPO INTO WA_EKPO.
    READ TABLE IT_COMPRA_AUX INTO WA_COMPRA_AUX WITH KEY "matnr = wa_ekpo-matnr
                                                         "menge = wa_ekpo-menge
                                                         EBELN = WA_EKPO-EBELN
                                                         EBELP = WA_EKPO-EBELP.
    IF ( SY-SUBRC NE 0 ).
      WA_COMPRA_AUX-EBELN = WA_EKPO-EBELN.
      WA_COMPRA_AUX-MENGE = WA_EKPO-MENGE.
      WA_COMPRA_AUX-EBELP = WA_EKPO-EBELP.
      WA_COMPRA_AUX-TXZ01 = WA_EKPO-TXZ01.
      WA_COMPRA_AUX-NETWR = WA_EKPO-NETWR.
      WA_COMPRA_AUX-LOEKZ = WA_EKPO-LOEKZ.
      WA_COMPRA_AUX-MEINS = WA_EKPO-MEINS.
      WA_COMPRA_AUX-NETPR = WA_EKPO-NETPR.
      WA_COMPRA_AUX-WERKS = WA_EKPO-WERKS.

      READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 ).
        WA_COMPRA_AUX-WAERS = WA_EKKO-WAERS.
      ENDIF.

      READ TABLE IT_EKKO_FORN INTO WA_EKKO_FORN WITH KEY EBELN = WA_EKPO-EBELN BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 ).

        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO_FORN-LIFNR BINARY SEARCH.
        WA_COMPRA_AUX-LIFNR = WA_LFA1-LIFNR.
        WA_COMPRA_AUX-NAME1 = WA_LFA1-NAME1.
      ENDIF.


      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR BINARY SEARCH.
      WA_COMPRA_AUX-MATNR = WA_MAKT-MATNR.
      WA_COMPRA_AUX-MAKTX = WA_MAKT-MAKTX.


      APPEND WA_COMPRA_AUX TO IT_COMPRA_AUX.
    ENDIF.
    CLEAR: WA_COMPRA_AUX, WA_MAKT, WA_EKBE, WA_EKPO, WA_RBKP, WA_EKKO.
  ENDLOOP.


  " Tratamento para pegar o faturado
  IF NOT ( IT_COMPRA_AUX[] IS INITIAL ).

    " Histórico para o documento de compra
    SELECT EBELN VGABE MENGE SHKZG BELNR BUDAT DMBTR EBELP MATNR GJAHR BEWTP
      FROM EKBE
      INTO TABLE IT_EKBE
      FOR ALL ENTRIES IN IT_COMPRA_AUX
    WHERE EBELN EQ IT_COMPRA_AUX-EBELN
      AND VGABE = '2'
      AND MATNR EQ IT_COMPRA_AUX-MATNR
      "and shkzg ne 'H'
      AND BEWTP EQ 'Q'
      AND BUDAT IN P_FATUR.

    " Cabeçalho doc.da fatura recebida
    SELECT BELNR GJAHR XBLNR STBLG
      FROM RBKP
      INTO TABLE IT_RBKP
      FOR ALL ENTRIES IN IT_EKBE
    WHERE BELNR EQ IT_EKBE-BELNR
      AND GJAHR EQ IT_EKBE-GJAHR
      AND STBLG EQ ''.

    IT_EKBE_AUX[] = IT_EKBE[].


    SORT: IT_EKBE     BY MATNR,
          IT_EKBE_AUX BY MATNR.

    LOOP AT IT_EKBE_AUX INTO WA_EKBE_AUX.
      LOOP AT IT_EKBE INTO WA_EKBE WHERE EBELN EQ WA_EKBE_AUX-EBELN
                                     AND MATNR EQ WA_EKBE_AUX-MATNR.

        IF ( WA_EKBE-SHKZG EQ 'S' ).
          TOTAL_F = TOTAL_F + WA_EKBE-MENGE.
        ELSEIF ( WA_EKBE-SHKZG EQ 'H' )  .
          TOTAL_F = TOTAL_F - WA_EKBE-MENGE.
        ENDIF.
      ENDLOOP.

      IF NOT ( TOTAL_F IS INITIAL ).
        WA_FATURADO_AUX-EBELN          = WA_EKBE_AUX-EBELN.
        WA_FATURADO_AUX-MATNR          = WA_EKBE_AUX-MATNR.
        WA_FATURADO_AUX-VALOR_FATURADO = TOTAL_F.
        APPEND WA_FATURADO_AUX TO IT_FATURADO_AUX.
      ENDIF.

      DELETE IT_EKBE_AUX WHERE EBELN EQ WA_EKBE_AUX-EBELN
                           AND MATNR EQ WA_EKBE_AUX-MATNR.

      CLEAR: WA_EKBE, WA_EKBE_AUX, TOTAL_F.
    ENDLOOP.
  ENDIF.

  REFRESH: IT_FATURADO_SOMA[].
  IT_FATURADO_SOMA[] = IT_FATURADO_AUX[].


  SORT: IT_FATURADO_AUX  BY MATNR,
        IT_FATURADO_SOMA BY MATNR.

  LOOP AT IT_FATURADO_AUX INTO WA_FATURADO_AUX.
    LOOP AT IT_FATURADO_SOMA INTO WA_FATURADO_SOMA WHERE MATNR EQ WA_FATURADO_AUX-MATNR.
      WA_FATURADO-VALOR_TOTAL = WA_FATURADO-VALOR_TOTAL + WA_FATURADO_SOMA-VALOR_FATURADO.
      WA_FATURADO-MATNR       = WA_FATURADO_SOMA-MATNR.
    ENDLOOP.
    APPEND WA_FATURADO TO IT_FATURADO.
    DELETE IT_FATURADO_AUX WHERE MATNR EQ WA_FATURADO_SOMA-MATNR.
    CLEAR: WA_FATURADO_SOMA, WA_FATURADO.
  ENDLOOP.


  REFRESH: IT_COMPRA_SOMA[], IT_COMPRA[].
  IT_COMPRA_SOMA[] = IT_COMPRA_AUX[].
  IT_COMPRA[]      = IT_COMPRA_AUX[].

  SORT: IT_COMPRA_SOMA BY MATNR,
        IT_COMPRA      BY MATNR.

  LOOP AT IT_COMPRA_SOMA INTO WA_COMPRA_SOMA.
    LOOP AT IT_COMPRA INTO WA_COMPRA WHERE MATNR EQ WA_COMPRA_SOMA-MATNR
                                       AND EBELP EQ WA_COMPRA_SOMA-EBELP.

      WA_SAIDA_AUX-MATNR     = WA_COMPRA-MATNR.
      WA_SAIDA_AUX-MAKTX     = WA_COMPRA-MAKTX.
      WA_SAIDA_AUX-CATEGORIA = 'C'.

      WA_SAIDA_AUX-EBELN_C  = WA_COMPRA-EBELN.
      WA_SAIDA_AUX-BSART_C  = WA_COMPRA-BSART.
      WA_SAIDA_AUX-BUKRS_C  = WA_COMPRA-BUKRS.
      WA_SAIDA_AUX-AEDAT_C  = WA_COMPRA-AEDAT.
      WA_SAIDA_AUX-WAERS_C  = WA_COMPRA-WAERS.
      WA_SAIDA_AUX-VGABE_C  = WA_COMPRA-VGABE.
      WA_SAIDA_AUX-MENGE_C  = WA_COMPRA-MENGE.
      WA_SAIDA_AUX-SHKZG_C  = WA_COMPRA-SHKZG.
      WA_SAIDA_AUX-BELNR_C  = WA_COMPRA-BELNR.
      WA_SAIDA_AUX-BUDAT_C  = WA_COMPRA-BUDAT.
      WA_SAIDA_AUX-DMBTR_C  = WA_COMPRA-DMBTR.
      WA_SAIDA_AUX-EBELP_C  = WA_COMPRA-EBELP.
      WA_SAIDA_AUX-TXZ01_C  = WA_COMPRA-TXZ01.
      WA_SAIDA_AUX-NETWR_C  = WA_COMPRA-NETWR.
      WA_SAIDA_AUX-LOEKZ_C  = WA_COMPRA-LOEKZ.
      WA_SAIDA_AUX-MEINS_C  = WA_COMPRA-MEINS.
      WA_SAIDA_AUX-GJAHR_C  = WA_COMPRA-GJAHR.
      WA_SAIDA_AUX-XBLNR_C  = WA_COMPRA-XBLNR.
      WA_SAIDA_AUX-LIFNR_C  = WA_COMPRA-LIFNR.
      WA_SAIDA_AUX-NAME1_C  = WA_COMPRA-NAME1.
      WA_SAIDA_AUX-NETPR_C  = WA_COMPRA-NETPR.


      IF ( WA_COMPRA-SHKZG EQ 'S' ).
        TOTAL = TOTAL + WA_COMPRA-MENGE.
      ELSEIF ( WA_COMPRA-SHKZG EQ 'H' ).
        TOTAL = - ( TOTAL + WA_COMPRA-MENGE ).
      ELSEIF ( WA_COMPRA-SHKZG EQ '' ).
        TOTAL = TOTAL + WA_COMPRA-MENGE.
      ENDIF.

      WA_SAIDA_AUX-TOTAL_COMPRA = TOTAL.

    ENDLOOP.

    READ TABLE IT_SAIDA_AUX WITH KEY MATNR = WA_SAIDA_AUX-MATNR.
    IF NOT SY-SUBRC IS INITIAL.
      APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
    ELSE.
      WA_SAIDA_AUX-TOTAL_COMPRA = WA_SAIDA_AUX-TOTAL_COMPRA + IT_SAIDA_AUX-TOTAL_COMPRA.
      MODIFY IT_SAIDA_AUX FROM WA_SAIDA_AUX INDEX SY-TABIX TRANSPORTING TOTAL_COMPRA.
    ENDIF.

    IF NOT ( IT_SAIDA_AUX[] IS INITIAL ).
      DELETE IT_COMPRA_SOMA WHERE MATNR EQ WA_COMPRA-MATNR
                              AND EBELP EQ WA_COMPRA-EBELP.

    ENDIF.

    CLEAR: WA_SAIDA_AUX, WA_COMPRA, WA_COMPRA_SOMA, TOTAL, TOTAL_F, WA_EKBE, WA_RBKP.
  ENDLOOP.




ENDFORM.                    " SELECIONA_DADOS_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_VENDAS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS_VENDAS .
  DATA: TOTAL   TYPE VBFA-RFMNG,
        TOTAL_F TYPE VBFA-RFMNG.

  PERFORM: STATUS_PEDIDO.


  IF NOT ( P_SAFRA IS INITIAL ).

    CHECK NOT IT_VBAK[] IS INITIAL.
    CHECK NOT IT_ZSDT0041[] IS INITIAL.
    CHECK NOT IT_ZSDT0040[] IS INITIAL.

  ELSE.
    CHECK NOT IT_VBAK[] IS INITIAL.
  ENDIF.

  CLEAR: IT_MAKT.

  SELECT VBELN MATNR ARKTX WERKS ZMENG NETWR KWMENG VRKME POSNR NETPR
   FROM VBAP
   INTO TABLE IT_VBAP
   FOR ALL ENTRIES IN IT_VBAK
  WHERE VBELN EQ IT_VBAK-VBELN
      AND MATNR IN P_MATNR.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM ZFIT0026
      INTO TABLE IT_0026
      FOR ALL ENTRIES IN IT_VBAK
       WHERE VBELN EQ IT_VBAK-VBELN.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM ZIB_CONTABIL_CHV
        INTO TABLE IT_ZIB
         FOR ALL ENTRIES IN IT_0026
          WHERE OBJ_KEY EQ IT_0026-OBJ_KEY.

      IF SY-SUBRC IS INITIAL.
        LOOP AT IT_ZIB.
          READ TABLE IT_0026
            WITH KEY OBJ_KEY = IT_ZIB-OBJ_KEY.

          IF SY-SUBRC IS INITIAL.
            MOVE : IT_ZIB-BUKRS   TO IT_ZIB_BSIK-BUKRS,
                   IT_0026-DOCNUM TO IT_ZIB_BSIK-BELNR,
                   IT_ZIB-GJAHR   TO IT_ZIB_BSIK-GJAHR.

            APPEND IT_ZIB_BSIK.

          ENDIF.
          CLEAR: IT_ZIB_BSIK.
        ENDLOOP.

        IF SY-SUBRC IS INITIAL.
          SELECT *
            FROM BSIK
            INTO TABLE IT_BSIK
             FOR ALL ENTRIES IN IT_ZIB_BSIK
              WHERE BUKRS EQ IT_ZIB_BSIK-BUKRS
                AND BELNR EQ IT_ZIB_BSIK-BELNR
                AND GJAHR EQ IT_ZIB_BSIK-GJAHR.

        ENDIF.
      ENDIF.


    ENDIF.

  ENDIF.

  SELECT *
    FROM VBKD
    INTO TABLE IT_VBKD
     FOR ALL ENTRIES IN IT_VBAK
     WHERE VBELN EQ IT_VBAK-VBELN.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM T052U
      INTO TABLE IT_T052U
       FOR ALL ENTRIES IN IT_VBKD
        WHERE SPRAS EQ 'PT'
          AND ZTERM EQ IT_VBKD-ZTERM.
  ENDIF.

  " Descrição do Material
  SELECT MANDT MATNR SPRAS MAKTX MAKTG
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_VBAP
 WHERE MATNR EQ IT_VBAP-MATNR.


  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_VBAK
  WHERE KUNNR EQ IT_VBAK-KUNNR.

  SORT: IT_VBAK BY VBELN,
        IT_VBAP BY VBELN,
        IT_MAKT BY MATNR,
        IT_KNA1 BY KUNNR.


  LOOP AT IT_VBAP INTO WA_VBAP.

    WA_VENDA_AUX-MATNR   = WA_VBAP-MATNR.
    WA_VENDA_AUX-ARKTX   = WA_VBAP-ARKTX.
    WA_VENDA_AUX-WERKS   = WA_VBAP-WERKS.
    WA_VENDA_AUX-ZMENG   = WA_VBAP-ZMENG.
    WA_VENDA_AUX-NETWR   = WA_VBAP-NETWR.
    WA_VENDA_AUX-KWMENG  = WA_VBAP-KWMENG.
    WA_VENDA_AUX-VRKME   = WA_VBAP-VRKME.
    WA_VENDA_AUX-POSNR   = WA_VBAP-POSNR.
    WA_VENDA_AUX-NETPR   = WA_VBAP-NETPR.

    READ TABLE IT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_VBAP-VBELN BINARY SEARCH.
    WA_VENDA_AUX-VBELN = WA_VBAK-VBELN.
    WA_VENDA_AUX-VKORG = WA_VBAK-VKORG.
    WA_VENDA_AUX-VTWEG = WA_VBAK-VTWEG.
    WA_VENDA_AUX-SPART = WA_VBAK-SPART.
    WA_VENDA_AUX-AUART = WA_VBAK-AUART.
    WA_VENDA_AUX-VKBUR = WA_VBAK-VKBUR.
    WA_VENDA_AUX-WAERK = WA_VBAK-WAERK.
    WA_VENDA_AUX-ERDAT = WA_VBAK-ERDAT.
    WA_VENDA_AUX-FAKSK = WA_VBAK-FAKSK.
    WA_VENDA_AUX-LIFSK = WA_VBAK-LIFSK.
    WA_VENDA_AUX-AUDAT = WA_VBAK-AUDAT.

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
    WA_VENDA_AUX-MAKTX   = WA_MAKT-MAKTX.

    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR BINARY SEARCH.
    WA_VENDA_AUX-KUNNR = WA_KNA1-KUNNR.
    WA_VENDA_AUX-NAME1 = WA_KNA1-NAME1.

    READ TABLE IT_KONV INTO WA_KONV WITH KEY KPOSN = WA_VBAP-POSNR
                                             KNUMV = WA_VBAK-KNUMV.
    IF ( SY-SUBRC EQ 0 ).
      WA_VENDA_AUX-KBETR = WA_KONV-KBETR.
    ENDIF.

    APPEND WA_VENDA_AUX TO IT_VENDA_AUX.

    CLEAR: WA_VBAP, WA_VBAK, WA_VBAP, WA_MAKT, WA_KNA1, WA_KONV, WA_VENDA_AUX.

  ENDLOOP.

  " Fatura - Venda
  SELECT RFMNG VBELN VBTYP_N VBELV VBTYP_V MEINS ERDAT MATNR POSNV
    FROM VBFA
    INTO TABLE IT_VBFA
    FOR ALL ENTRIES IN IT_VENDA_AUX
  WHERE VBELV EQ IT_VENDA_AUX-VBELN
    AND VBTYP_V = 'C'
    AND ERDAT IN P_FATUR.

  IF NOT ( IT_VBFA[] IS INITIAL ).
    DELETE IT_VBFA WHERE VBTYP_N NE 'N'
                     AND VBTYP_N NE 'M'.
  ENDIF.


  SORT: IT_VENDA_AUX BY VBELN POSNR.

  LOOP AT IT_VBFA INTO WA_VBFA.

    "    wa_vbfa_aux-rfmng     = wa_vbfa-rfmng.
    WA_VBFA_AUX-VBELN     = WA_VBFA-VBELN.
    WA_VBFA_AUX-VBTYP_N   = WA_VBFA-VBTYP_N.
    WA_VBFA_AUX-VBELV     = WA_VBFA-VBELV.
    WA_VBFA_AUX-VBTYP_V   = WA_VBFA-VBTYP_V.
    WA_VBFA_AUX-MEINS     = WA_VBFA-MEINS.
    WA_VBFA_AUX-ERDAT     = WA_VBFA-ERDAT.
    WA_VBFA_AUX-POSNV     = WA_VBFA-POSNV.

    READ TABLE IT_VENDA_AUX INTO WA_VENDA_AUX WITH KEY VBELN = WA_VBFA_AUX-VBELV
                                                       POSNR = WA_VBFA_AUX-POSNV BINARY SEARCH.
    IF ( SY-SUBRC EQ 0 ).
      WA_VBFA_AUX-MATNR     = WA_VENDA_AUX-MATNR.

      PERFORM: CONVERSION_SAC USING WA_VENDA_AUX-MATNR
                                    WA_VBFA-RFMNG
                                    WA_VBFA-MEINS.

      IF NOT ( VALOR_CONVERSION_FATURADO IS INITIAL ).
        WA_VBFA_AUX-RFMNG = VALOR_CONVERSION_FATURADO.
      ENDIF.

    ENDIF.

    APPEND WA_VBFA_AUX TO IT_VBFA_AUX.

    CLEAR: WA_VBFA_AUX, WA_VBFA.
  ENDLOOP.

  REFRESH: IT_VBFA_SOMA[].
  IT_VBFA_SOMA[] = IT_VBFA_AUX[].

  SORT: IT_VBFA_AUX  BY MATNR,
        IT_VBFA_SOMA BY MATNR.

  LOOP AT IT_VBFA_AUX INTO WA_VBFA_AUX.
    LOOP AT IT_VBFA_SOMA INTO WA_VBFA_SOMA WHERE MATNR EQ WA_VBFA_AUX-MATNR
                                             AND VBELV EQ WA_VBFA_AUX-VBELV.

      IF ( ( WA_VBFA_SOMA-VBTYP_N EQ 'M' ) OR ( WA_VBFA_SOMA-VBTYP_N EQ 'S') ) .
        TOTAL_F = TOTAL_F + WA_VBFA_SOMA-RFMNG.
      ELSEIF ( ( WA_VBFA_SOMA-VBTYP_N EQ 'N' ) OR ( WA_VBFA_SOMA-VBTYP_N EQ 'O' ) ).
        TOTAL_F = TOTAL_F - WA_VBFA_SOMA-RFMNG.
      ENDIF.
    ENDLOOP.

    WA_FATURADO_VENDA-VBELV = WA_VBFA_SOMA-VBELN.
    WA_FATURADO_VENDA-MATNR = WA_VBFA_SOMA-MATNR.
    WA_FATURADO_VENDA-VALOR_FATURADO = TOTAL_F.


    APPEND WA_FATURADO_VENDA TO IT_FATURADO_VENDA.
    DELETE IT_VBFA_AUX WHERE VBELV EQ WA_VBFA_SOMA-VBELV
                         AND MATNR EQ WA_VBFA_SOMA-MATNR.

    CLEAR: WA_VBFA_AUX, WA_VBFA_SOMA, WA_FATURADO_VENDA, TOTAL_F, VALOR_CONVERSION_FATURADO .
  ENDLOOP.

  REFRESH: IT_FATURADO_VENDA_SOMA[].
  IT_FATURADO_VENDA_AUX[] = IT_FATURADO_VENDA[].


  SORT: IT_FATURADO_VENDA     BY MATNR,
        IT_FATURADO_VENDA_AUX BY MATNR.


  IF NOT ( IT_FATURADO_VENDA[] IS INITIAL ).

    DELETE IT_FATURADO_VENDA     WHERE MATNR EQ ''.
    DELETE IT_FATURADO_VENDA_AUX WHERE MATNR EQ ''.

    LOOP AT IT_FATURADO_VENDA INTO WA_FATURADO_VENDA.

      LOOP AT IT_FATURADO_VENDA_AUX INTO WA_FATURADO_VENDA_AUX WHERE MATNR EQ WA_FATURADO_VENDA-MATNR.
        WA_FATURADO_VENDA_SOMA-VALOR_TOTAL = WA_FATURADO_VENDA_SOMA-VALOR_TOTAL + WA_FATURADO_VENDA_AUX-VALOR_FATURADO.
        WA_FATURADO_VENDA_SOMA-MATNR       = WA_FATURADO_VENDA_AUX-MATNR.
      ENDLOOP.

      APPEND WA_FATURADO_VENDA_SOMA TO IT_FATURADO_VENDA_SOMA.
      DELETE IT_FATURADO_VENDA WHERE MATNR EQ WA_FATURADO_VENDA_AUX-MATNR.
      CLEAR: WA_FATURADO_VENDA_SOMA, WA_FATURADO_VENDA, WA_FATURADO_VENDA_AUX.

    ENDLOOP.

  ENDIF.

  REFRESH: IT_VENDA_SOMA[].
  IT_VENDA_SOMA[] = IT_VENDA_AUX[].
  IT_VENDA[] = IT_VENDA_AUX[].

  CLEAR: WA_VENDA_AUX.


  SORT: IT_VENDA_AUX  BY MATNR,
        IT_VENDA_SOMA BY MATNR.

  LOOP AT IT_VENDA_AUX INTO WA_VENDA_AUX.
    LOOP AT IT_VENDA_SOMA INTO WA_VENDA_SOMA WHERE VBELN EQ WA_VENDA_AUX-VBELN
                                               AND MATNR EQ WA_VENDA_AUX-MATNR.

      WA_SAIDA_AUX-MATNR     = WA_VENDA_SOMA-MATNR.
      WA_SAIDA_AUX-MAKTX     = WA_VENDA_SOMA-MAKTX.
      WA_SAIDA_AUX-CATEGORIA = 'V'.

      WA_SAIDA_AUX-VBELN_V    = WA_VENDA_SOMA-VBELN.
      WA_SAIDA_AUX-VKORG_V    = WA_VENDA_SOMA-VKORG.
      WA_SAIDA_AUX-VTWEG_V    = WA_VENDA_SOMA-VTWEG.
      WA_SAIDA_AUX-SPART_V    = WA_VENDA_SOMA-SPART.
      WA_SAIDA_AUX-AUART_V    = WA_VENDA_SOMA-AUART.
      WA_SAIDA_AUX-VKBUR_V    = WA_VENDA_SOMA-VKBUR.
      WA_SAIDA_AUX-WAERK_V    = WA_VENDA_SOMA-WAERK.
      WA_SAIDA_AUX-ERDAT_V    = WA_VENDA_SOMA-ERDAT.
      WA_SAIDA_AUX-FAKSK_V    = WA_VENDA_SOMA-FAKSK.
      WA_SAIDA_AUX-LIFSK_V    = WA_VENDA_SOMA-LIFSK.
      WA_SAIDA_AUX-AUDAT_V    = WA_VENDA_SOMA-AUDAT.
      WA_SAIDA_AUX-ARKTX_V    = WA_VENDA_SOMA-ARKTX.
      WA_SAIDA_AUX-WERKS_V    = WA_VENDA_SOMA-WERKS.
      WA_SAIDA_AUX-ZMENG_V    = WA_VENDA_SOMA-ZMENG.
      WA_SAIDA_AUX-NETWR_V    = WA_VENDA_SOMA-NETWR.
      WA_SAIDA_AUX-KWMENG_V   = WA_VENDA_SOMA-KWMENG.
      WA_SAIDA_AUX-VRKME_V    = WA_VENDA_SOMA-VRKME.
      WA_SAIDA_AUX-POSNR_V    = WA_VENDA_SOMA-POSNR.
      WA_SAIDA_AUX-RFMNG_V    = WA_VENDA_SOMA-RFMNG.
      WA_SAIDA_AUX-VBTYP_N_V  = WA_VENDA_SOMA-VBTYP_N.
      WA_SAIDA_AUX-VBELV_V    = WA_VENDA_SOMA-VBELV.
      WA_SAIDA_AUX-VBTYP_V_V  = WA_VENDA_SOMA-VBTYP_V.
      WA_SAIDA_AUX-MEINS_V    = WA_VENDA_SOMA-MEINS.
      WA_SAIDA_AUX-KUNNR_V    = WA_VENDA_SOMA-KUNNR.
      WA_SAIDA_AUX-NAME1_V    = WA_VENDA_SOMA-NAME1.
      WA_SAIDA_AUX-KBETR_V    = WA_VENDA_SOMA-KBETR.
      WA_SAIDA_AUX-LIFSP_V    = WA_VENDA_SOMA-LIFSP.

      IF ( WA_VENDA_SOMA-VBTYP_N EQ 'M' ).
        TOTAL = TOTAL + WA_VENDA_SOMA-KWMENG.
      ELSEIF ( WA_VENDA_SOMA-VBTYP_N EQ 'N' ).
        TOTAL = - ( TOTAL + WA_VENDA_SOMA-KWMENG ).
      ELSEIF ( WA_VENDA_SOMA-VBTYP_N EQ '' ).
        TOTAL = TOTAL + WA_VENDA_SOMA-KWMENG.
      ENDIF.

      WA_SAIDA_AUX-TOTAL_VENDA = TOTAL.

    ENDLOOP.

    APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.

    IF NOT ( IT_VENDA_AUX[] IS INITIAL ).
      DELETE IT_VENDA_AUX WHERE MATNR EQ WA_VENDA_SOMA-MATNR
                            AND VBELN EQ WA_VENDA_SOMA-VBELN.
    ENDIF.

    CLEAR: WA_VENDA_SOMA, WA_VENDA_AUX, WA_SAIDA_AUX, TOTAL, WA_FATURADO_VENDA.
  ENDLOOP.


ENDFORM.                    " SELECIONA_DADOS_VENDAS

FORM SELECIONA_DADOS_TRANSF.

  TYPES: BEGIN OF TY_SEL,
           SIGN(1),
           OPTION(2),
           LOW       TYPE EKPO-MATKL,
           HIGH      TYPE EKPO-MATKL,
         END OF TY_SEL.

  DATA: TOTAL_TRANSF   TYPE EKPO-MENGE,
        TOTAL_TRANSF_F TYPE EKPO-MENGE,
        WA_SEL         TYPE TY_SEL,
        WA_R_MATKL     TYPE EKPO-MATKL.

  RANGES R_MATKL FOR EKPO-MATKL.

  WA_SEL-SIGN = 'I'.
  WA_SEL-OPTION = 'EQ'.
  WA_SEL-LOW = '700130'.
  APPEND WA_SEL TO R_MATKL.
  WA_SEL-LOW = '700230'.
  APPEND WA_SEL TO R_MATKL.
  WA_SEL-LOW = '700240'.
  APPEND WA_SEL TO R_MATKL.
  WA_SEL-LOW = '700350'.
  APPEND WA_SEL TO R_MATKL.


  " Cabeçalho do documento de compra
  SELECT EBELN BSART RESWK BUKRS AEDAT WAERS UNSEZ
   FROM EKKO
   INTO TABLE IT_EKKO_T
  WHERE  BSART IN P_BSARTT
     AND AEDAT IN P_ERDAT
     AND BUKRS IN P_BUKRS
     AND UNSEZ IN P_SAFRA.

  CHECK NOT IT_EKKO_T[] IS INITIAL.

  " Item do documento de compras
  SELECT EBELN EBELP MATNR TXZ01 MENGE NETWR LOEKZ MEINS NETPR MATKL
   FROM EKPO
   INTO TABLE IT_EKPO_T
   FOR ALL ENTRIES IN IT_EKKO_T
  WHERE EBELN EQ IT_EKKO_T-EBELN
    AND LOEKZ IN R_EKPO_S
    AND MATNR IN P_MATNR
    AND PSTYP EQ 7.

  CHECK NOT IT_EKPO_T[] IS INITIAL.

  CASE P_SPART.
    WHEN 'IEQ02'.
      LOOP AT IT_EKPO_T INTO WA_EKPO_T.
        IF WA_EKPO_T-MATKL NE '700150'.
          DELETE IT_EKKO_T WHERE EBELN = WA_EKPO_T-EBELN.
          DELETE IT_EKPO_T WHERE EBELN = WA_EKPO_T-EBELN.
        ENDIF.
        CLEAR WA_EKPO_T.
      ENDLOOP.
    WHEN 'IEQ03'.
      LOOP AT IT_EKPO_T INTO WA_EKPO_T.
        IF WA_EKPO_T-MATKL NE '658445'.
          DELETE IT_EKKO_T WHERE EBELN = WA_EKPO_T-EBELN.
          DELETE IT_EKPO_T WHERE EBELN = WA_EKPO_T-EBELN.
        ENDIF.
        CLEAR WA_EKPO_T.
      ENDLOOP.
    WHEN 'IEQ04'.
      LOOP AT IT_EKPO_T INTO WA_EKPO_T.
        IF WA_EKPO_T-MATKL NOT IN R_MATKL.
          DELETE IT_EKKO_T WHERE EBELN = WA_EKPO_T-EBELN.
          DELETE IT_EKPO_T WHERE EBELN = WA_EKPO_T-EBELN.
        ENDIF.
        CLEAR WA_EKPO_T.
      ENDLOOP.
  ENDCASE.

  SELECT WERKS               "Seleção Nome da Filial
         NAME1
      FROM T001W
      INTO TABLE IT_T001W
      FOR ALL ENTRIES IN IT_EKKO_T
    WHERE WERKS = IT_EKKO_T-RESWK.

  " Descrição do Material
  SELECT MANDT MATNR SPRAS MAKTX MAKTG
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_EKPO_T
 WHERE MATNR EQ IT_EKPO_T-MATNR.


  SORT: IT_EKBE   BY EBELN,
        IT_EKPO_T BY EBELN,
        IT_RBKP   BY BELNR GJAHR,
        IT_T001W  BY WERKS,
        IT_MAKT   BY MATNR.

  LOOP AT IT_EKKO_T INTO WA_EKKO_T.

    WA_TRANSF_AUX-EBELN = WA_EKKO_T-EBELN.
    WA_TRANSF_AUX-BSART = WA_EKKO_T-BSART.
    WA_TRANSF_AUX-BUKRS = WA_EKKO_T-BUKRS.
    WA_TRANSF_AUX-AEDAT = WA_EKKO_T-AEDAT.
    WA_TRANSF_AUX-WAERS = WA_EKKO_T-WAERS.

    READ TABLE IT_EKPO_T INTO WA_EKPO_T WITH KEY EBELN = WA_EKKO_T-EBELN BINARY SEARCH.

    IF ( SY-SUBRC EQ 0 ).
      WA_TRANSF_AUX-MENGE = WA_EKPO_T-MENGE.
      WA_TRANSF_AUX-EBELP = WA_EKPO_T-EBELP.
      WA_TRANSF_AUX-TXZ01 = WA_EKPO_T-TXZ01.
      WA_TRANSF_AUX-NETWR = WA_EKPO_T-NETWR.
      WA_TRANSF_AUX-LOEKZ = WA_EKPO_T-LOEKZ.
      WA_TRANSF_AUX-MEINS = WA_EKPO_T-MEINS.
      WA_TRANSF_AUX-NETPR = WA_EKPO_T-NETPR.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO_T-MATNR BINARY SEARCH.
      WA_TRANSF_AUX-MATNR = WA_MAKT-MATNR.
      WA_TRANSF_AUX-MAKTX = WA_MAKT-MAKTX.


      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKKO_T-RESWK BINARY SEARCH.
      WA_TRANSF_AUX-RESWK = WA_T001W-WERKS.
      WA_TRANSF_AUX-NAME1 = WA_T001W-NAME1.

      APPEND WA_TRANSF_AUX TO IT_TRANSF_AUX.

    ENDIF.

    CLEAR: WA_EKKO_T,
           WA_EKBE,
           WA_T001W,
           WA_TRANSF_AUX.
  ENDLOOP.

  CLEAR: WA_TRANSF_AUX.

  " Cabeçalho do documento de transferência
  SELECT EBELN BSART LIFNR BUKRS AEDAT WAERS UNSEZ
   FROM EKKO
   INTO TABLE IT_EKKO_T_FORN
   FOR ALL ENTRIES IN IT_EKPO_T
  WHERE EBELN EQ IT_EKPO_T-EBELN
    AND UNSEZ IN P_SAFRA.


  SORT: IT_TRANSF_AUX BY MATNR MENGE EBELP,
          IT_MAKT     BY MATNR,
          IT_T001W    BY WERKS,
          IT_EKBE     BY EBELN,
          IT_RBKP     BY BELNR.

*---> 05/07/2023 - Migração S4 - DL
  SORT IT_EKKO_T BY EBELN.
  SORT IT_EKKO_T_FORN BY EBELN.
*<--- 05/07/2023 - Migração S4 - DL

  LOOP AT IT_EKPO_T INTO WA_EKPO_T.
    READ TABLE IT_TRANSF_AUX INTO WA_TRANSF_AUX WITH KEY MATNR = WA_EKPO_T-MATNR
                                                         "menge = wa_ekpo-menge
                                                         EBELN = WA_EKPO_T-EBELN
                                                         EBELP = WA_EKPO_T-EBELP.
    IF ( SY-SUBRC NE 0 ).
      WA_TRANSF_AUX-EBELN = WA_EKPO_T-EBELN.
      WA_TRANSF_AUX-MENGE = WA_EKPO_T-MENGE.
      WA_TRANSF_AUX-EBELP = WA_EKPO_T-EBELP.
      WA_TRANSF_AUX-TXZ01 = WA_EKPO_T-TXZ01.
      WA_TRANSF_AUX-NETWR = WA_EKPO_T-NETWR.
      WA_TRANSF_AUX-LOEKZ = WA_EKPO_T-LOEKZ.
      WA_TRANSF_AUX-MEINS = WA_EKPO_T-MEINS.
      WA_TRANSF_AUX-NETPR = WA_EKPO_T-NETPR.

      READ TABLE IT_EKKO_T INTO WA_EKKO_T WITH KEY EBELN = WA_EKPO_T-EBELN BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 ).
        WA_TRANSF_AUX-WAERS = WA_EKKO_T-WAERS.
      ENDIF.

      READ TABLE IT_EKKO_T_FORN INTO WA_EKKO_T_FORN WITH KEY EBELN = WA_EKPO_T-EBELN BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 ).

        READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKKO_T_FORN-RESWK BINARY SEARCH.
        WA_TRANSF_AUX-RESWK = WA_T001W-WERKS.
        WA_TRANSF_AUX-NAME1 = WA_T001W-NAME1.
      ENDIF.


      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO_T-MATNR BINARY SEARCH.
      WA_TRANSF_AUX-MATNR = WA_MAKT-MATNR.
      WA_TRANSF_AUX-MAKTX = WA_MAKT-MAKTX.


      APPEND WA_TRANSF_AUX TO IT_TRANSF_AUX.
    ENDIF.
    CLEAR: WA_TRANSF_AUX, WA_MAKT, WA_EKBE_T, WA_EKPO_T, WA_RBKP, WA_EKKO_T.
  ENDLOOP.


  " Tratamento para pegar o faturado
  IF NOT ( IT_TRANSF_AUX[] IS INITIAL ).

    " Histórico para o documento de transferência
    SELECT EBELN VGABE MENGE SHKZG BELNR BUDAT DMBTR EBELP MATNR GJAHR BEWTP
      FROM EKBE
      INTO TABLE IT_EKBE_T
      FOR ALL ENTRIES IN IT_TRANSF_AUX
    WHERE EBELN EQ IT_TRANSF_AUX-EBELN
      AND VGABE = '6'
      AND MATNR EQ IT_TRANSF_AUX-MATNR
      "and shkzg ne 'H'
      AND BEWTP EQ 'U'
      AND BUDAT IN P_FATUR.

    " Cabeçalho doc.da fatura recebida
    SELECT BELNR GJAHR XBLNR STBLG
      FROM RBKP
      INTO TABLE IT_RBKP_T
      FOR ALL ENTRIES IN IT_EKBE_T
    WHERE BELNR EQ IT_EKBE_T-BELNR
      AND GJAHR EQ IT_EKBE_T-GJAHR
      AND STBLG EQ ''.

    IT_EKBE_T_AUX[] = IT_EKBE_T[].


    SORT: IT_EKBE_T     BY MATNR,
          IT_EKBE_T_AUX BY MATNR.

    LOOP AT IT_EKBE_T_AUX INTO WA_EKBE_T_AUX.
      LOOP AT IT_EKBE_T INTO WA_EKBE_T WHERE EBELN EQ WA_EKBE_T_AUX-EBELN
                                     AND MATNR EQ WA_EKBE_T_AUX-MATNR.

        IF ( WA_EKBE_T-SHKZG EQ 'S' ).
          TOTAL_TRANSF_F = TOTAL_TRANSF_F + WA_EKBE_T-MENGE.
        ELSEIF ( WA_EKBE_T-SHKZG EQ 'H' )  .
          TOTAL_TRANSF_F = TOTAL_TRANSF_F - WA_EKBE_T-MENGE.
        ENDIF.
      ENDLOOP.

      IF NOT ( TOTAL_TRANSF_F IS INITIAL ).
        WA_FATURADO_T_AUX-EBELN          = WA_EKBE_T_AUX-EBELN.
        WA_FATURADO_T_AUX-MATNR          = WA_EKBE_T_AUX-MATNR.
        WA_FATURADO_T_AUX-VALOR_FATURADO = TOTAL_TRANSF_F.
        APPEND WA_FATURADO_T_AUX TO IT_FATURADO_T_AUX.
      ENDIF.

      DELETE IT_EKBE_T_AUX WHERE EBELN EQ WA_EKBE_T_AUX-EBELN
                           AND   MATNR EQ WA_EKBE_T_AUX-MATNR.

      CLEAR: WA_EKBE_T, WA_EKBE_T_AUX, TOTAL_TRANSF_F.
    ENDLOOP.
  ENDIF.

  REFRESH: IT_FATURADO_T_SOMA[].
  IT_FATURADO_T_SOMA[] = IT_FATURADO_T_AUX[].


  SORT: IT_FATURADO_T_AUX  BY MATNR,
        IT_FATURADO_T_SOMA BY MATNR.

  LOOP AT IT_FATURADO_T_AUX INTO WA_FATURADO_T_AUX.

    LOOP AT IT_FATURADO_T_SOMA INTO WA_FATURADO_T_SOMA WHERE MATNR EQ WA_FATURADO_T_AUX-MATNR.
      WA_FATURADO_T-VALOR_TOTAL = WA_FATURADO_T-VALOR_TOTAL + WA_FATURADO_T_SOMA-VALOR_FATURADO.
      WA_FATURADO_T-MATNR       = WA_FATURADO_T_SOMA-MATNR.
    ENDLOOP.

    APPEND WA_FATURADO_T TO IT_FATURADO_T.
    DELETE IT_FATURADO_T_AUX WHERE MATNR EQ WA_FATURADO_T_SOMA-MATNR.
    CLEAR: WA_FATURADO_T_SOMA, WA_FATURADO_T.
  ENDLOOP.


  REFRESH: IT_TRANSF_SOMA[], IT_TRANSF[].
  IT_TRANSF_SOMA[] = IT_TRANSF_AUX[].
  IT_TRANSF[]      = IT_TRANSF_AUX[].

  SORT: IT_TRANSF_SOMA BY MATNR,
        IT_TRANSF      BY MATNR.

  LOOP AT IT_TRANSF_SOMA INTO WA_TRANSF_SOMA.
    LOOP AT IT_TRANSF INTO WA_TRANSF WHERE MATNR EQ WA_TRANSF_SOMA-MATNR
                                       AND EBELP EQ WA_TRANSF_SOMA-EBELP.

      WA_SAIDA_AUX-MATNR     = WA_TRANSF-MATNR.
      WA_SAIDA_AUX-MAKTX     = WA_TRANSF-MAKTX.
      WA_SAIDA_AUX-CATEGORIA = 'T'.

      WA_SAIDA_AUX-EBELN_T  = WA_TRANSF-EBELN.
      WA_SAIDA_AUX-BSART_T  = WA_TRANSF-BSART.
      WA_SAIDA_AUX-BUKRS_T  = WA_TRANSF-BUKRS.
      WA_SAIDA_AUX-AEDAT_T  = WA_TRANSF-AEDAT.
      WA_SAIDA_AUX-WAERS_T  = WA_TRANSF-WAERS.
      WA_SAIDA_AUX-VGABE_T  = WA_TRANSF-VGABE.
      WA_SAIDA_AUX-MENGE_T  = WA_TRANSF-MENGE.
      WA_SAIDA_AUX-SHKZG_T  = WA_TRANSF-SHKZG.
      WA_SAIDA_AUX-BELNR_T  = WA_TRANSF-BELNR.
      WA_SAIDA_AUX-BUDAT_T  = WA_TRANSF-BUDAT.
      WA_SAIDA_AUX-DMBTR_T  = WA_TRANSF-DMBTR.
      WA_SAIDA_AUX-EBELP_T  = WA_TRANSF-EBELP.
      WA_SAIDA_AUX-TXZ01_T  = WA_TRANSF-TXZ01.
      WA_SAIDA_AUX-NETWR_T  = WA_TRANSF-NETWR.
      WA_SAIDA_AUX-LOEKZ_T  = WA_TRANSF-LOEKZ.
      WA_SAIDA_AUX-MEINS_T  = WA_TRANSF-MEINS.
      WA_SAIDA_AUX-GJAHR_T  = WA_TRANSF-GJAHR.
      WA_SAIDA_AUX-XBLNR_T  = WA_TRANSF-XBLNR.
      WA_SAIDA_AUX-RESWK_T  = WA_TRANSF-RESWK.
      WA_SAIDA_AUX-NAME1_T  = WA_TRANSF-NAME1.
      WA_SAIDA_AUX-NETPR_T  = WA_TRANSF-NETPR.


      IF ( WA_TRANSF-SHKZG EQ 'S' ).
        TOTAL_TRANSF = TOTAL_TRANSF + WA_TRANSF-MENGE.
      ELSEIF ( WA_TRANSF-SHKZG EQ 'H' ).
        TOTAL_TRANSF = - ( TOTAL_TRANSF + WA_TRANSF-MENGE ).
      ELSEIF ( WA_TRANSF-SHKZG EQ '' ).
        TOTAL_TRANSF = TOTAL_TRANSF + WA_TRANSF-MENGE.
      ENDIF.

      WA_SAIDA_AUX-TOTAL_TRANSF = TOTAL_TRANSF.

    ENDLOOP.

    READ TABLE IT_SAIDA_AUX WITH KEY MATNR = WA_SAIDA_AUX-MATNR.
    IF NOT SY-SUBRC IS INITIAL.
      APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
    ELSE.
      WA_SAIDA_AUX-TOTAL_TRANSF = WA_SAIDA_AUX-TOTAL_TRANSF + IT_SAIDA_AUX-TOTAL_TRANSF.
      MODIFY IT_SAIDA_AUX FROM WA_SAIDA_AUX INDEX SY-TABIX TRANSPORTING TOTAL_TRANSF.
    ENDIF.

    IF NOT ( IT_SAIDA_AUX[] IS INITIAL ).
      DELETE IT_TRANSF_SOMA WHERE MATNR EQ WA_TRANSF-MATNR
                              AND EBELP EQ WA_TRANSF-EBELP.

    ENDIF.

    CLEAR: WA_SAIDA_AUX, WA_TRANSF, WA_TRANSF_SOMA, TOTAL_TRANSF, TOTAL_TRANSF_F, WA_EKBE_T, WA_RBKP_T.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  STATUS_PEDIDO
*&---------------------------------------------------------------------*
FORM STATUS_PEDIDO .

  SELECT VBELN VKORG  VTWEG SPART AUART  VKBUR KUNNR WAERK ERDAT FAKSK LIFSK AUDAT KNUMV
   FROM VBAK
   INTO TABLE IT_VBAK
  WHERE  AUART IN P_AUART
     AND VKORG IN P_VKORG
     AND VTWEG IN P_VTWEG
     AND SPART IN P_SPART
     "AND vkbur IN p_vkbur
     AND ERDAT IN P_ERDAT
     AND LIFSK EQ SPACE.

  IF NOT ( IT_VBAK[] IS INITIAL ).


    SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , KBETR FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KNUMV AND KSCHL EQ 'PR00' INTO TABLE @IT_KONV .


    IF NOT ( P_SAFRA IS INITIAL ).

      SELECT VBELN DOC_SIMULACAO
        FROM ZSDT0041
        INTO TABLE IT_ZSDT0041
        FOR ALL ENTRIES IN IT_VBAK
      WHERE VBELN EQ IT_VBAK-VBELN.


      SELECT DOC_SIMULACAO SAFRA
        FROM ZSDT0040
        INTO TABLE IT_ZSDT0040
        FOR ALL ENTRIES IN IT_ZSDT0041
      WHERE DOC_SIMULACAO EQ IT_ZSDT0041-DOC_SIMULACAO
        AND SAFRA IN P_SAFRA.

      CLEAR: WA_VBAK.

      LOOP AT IT_VBAK INTO WA_VBAK.

        READ TABLE IT_ZSDT0041 INTO WA_ZSDT0041 WITH KEY VBELN = WA_VBAK-VBELN.
        IF ( SY-SUBRC EQ 0 ).
          READ TABLE IT_ZSDT0040 INTO WA_ZSDT0040 WITH KEY DOC_SIMULACAO = WA_ZSDT0041-DOC_SIMULACAO.

          IF ( SY-SUBRC EQ 0 ).
            CONTINUE.
          ELSE.
            DELETE IT_VBAK WHERE VBELN EQ WA_VBAK-VBELN.
          ENDIF.
        ELSE.
          DELETE IT_VBAK WHERE VBELN EQ WA_VBAK-VBELN.
        ENDIF.
        CLEAR:  WA_ZSDT0041, WA_ZSDT0040, WA_VBAK.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " STATUS_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  AGRUPAMENTO
*&---------------------------------------------------------------------*
FORM AGRUPAMENTO .
  DATA LS_CELLCOLOR TYPE LVC_S_SCOL .
  DATA: V_INDEX TYPE SY-TABIX.

  REFRESH: IT_SAIDA_SOMA[].

  IT_SAIDA_SOMA[] = IT_SAIDA_AUX[].


  SORT: IT_SAIDA_SOMA BY MATNR,
        IT_SAIDA_AUX  BY MATNR.


  LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.

    CLEAR: WA_SAIDA-TOTAL_PRODUZIDO_P,
           WA_SAIDA-TOTAL_COMPRA,
           WA_SAIDA-TOTAL_VENDA.

    LOOP AT IT_SAIDA_SOMA INTO WA_SAIDA_SOMA WHERE MATNR EQ WA_SAIDA_AUX-MATNR.
      WA_SAIDA-TOTAL_PRODUZIDO_P = WA_SAIDA-TOTAL_PRODUZIDO_P + WA_SAIDA_SOMA-TOTAL_PRODUZIDO_P.
      WA_SAIDA-TOTAL_COMPRA    = WA_SAIDA-TOTAL_COMPRA + WA_SAIDA_SOMA-TOTAL_COMPRA.
      WA_SAIDA-TOTAL_VENDA     = WA_SAIDA-TOTAL_VENDA  + WA_SAIDA_SOMA-TOTAL_VENDA.
      WA_SAIDA-TOTAL_TRANSF    = WA_SAIDA-TOTAL_TRANSF + WA_SAIDA_SOMA-TOTAL_TRANSF.
      CLEAR: WA_SAIDA_SOMA.
    ENDLOOP.

    READ TABLE IT_FATURADO INTO WA_FATURADO WITH KEY MATNR = WA_SAIDA_AUX-MATNR.
    IF ( SY-SUBRC EQ 0 ).
      WA_SAIDA-TOTAL_F_C       = WA_FATURADO-VALOR_TOTAL.
    ENDIF.

    READ TABLE IT_FATURADO_VENDA_SOMA INTO WA_FATURADO_VENDA_SOMA WITH KEY MATNR = WA_SAIDA_AUX-MATNR.
    IF ( SY-SUBRC EQ 0 ).
      WA_SAIDA-TOTAL_F_V       = WA_FATURADO_VENDA_SOMA-VALOR_TOTAL.
    ENDIF.

    READ TABLE IT_FATURADO_T INTO WA_FATURADO_T WITH KEY MATNR = WA_SAIDA_AUX-MATNR.
    IF ( SY-SUBRC EQ 0 ).
      WA_SAIDA-TOTAL_F_T       = WA_FATURADO_T-VALOR_TOTAL.
    ENDIF.



    WA_SAIDA-MATNR           = WA_SAIDA_AUX-MATNR.
    WA_SAIDA-MAKTX           = WA_SAIDA_AUX-MAKTX.
    WA_SAIDA-SALDO           = ( WA_SAIDA-TOTAL_COMPRA + WA_SAIDA-TOTAL_PRODUZIDO_P ) - ( WA_SAIDA-TOTAL_VENDA + WA_SAIDA-TOTAL_TRANSF ).
    WA_SAIDA-SALDO_C         = ( WA_SAIDA-TOTAL_COMPRA - WA_SAIDA-TOTAL_F_C ).
    WA_SAIDA-SALDO_V         = ( WA_SAIDA-TOTAL_VENDA - WA_SAIDA-TOTAL_F_V ).
    WA_SAIDA-SALDO_T         = ( WA_SAIDA-TOTAL_TRANSF - WA_SAIDA-TOTAL_F_T ).


    WA_SAIDA-EBELN_C         = WA_SAIDA_AUX-EBELN_C.

    APPEND WA_SAIDA TO IT_SAIDA.

    IF NOT ( IT_SAIDA[] IS INITIAL ).
      DELETE IT_SAIDA_AUX WHERE MATNR EQ WA_SAIDA-MATNR.
    ENDIF.

    CLEAR: WA_SAIDA, WA_SAIDA_SOMA.
  ENDLOOP.

  LOOP AT IT_SAIDA.

    IF ( IT_SAIDA-SALDO < 0 ).

      V_INDEX = SY-TABIX.
      LS_CELLCOLOR-FNAME = 'SALDO' .
      LS_CELLCOLOR-COLOR-COL = '6' .
      LS_CELLCOLOR-COLOR-INT = '0' .
      LS_CELLCOLOR-COLOR-INV = '0' .

      APPEND LS_CELLCOLOR TO IT_SAIDA-CELLCOLORS.
      MODIFY IT_SAIDA INDEX V_INDEX TRANSPORTING CELLCOLORS.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " AGRUPAMENTO

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SAC
*&---------------------------------------------------------------------*
FORM CONVERSION_SAC  USING    P_MATNR
                              P_VALOR
                              P_MEINS.

  IF ( ( P_MEINS EQ 'KG' ) AND ( P_SPART-LOW EQ '04' ) ).

    VALOR = P_VALOR.
    CALL FUNCTION 'ME_CONVERSION_BPRME'
      EXPORTING
        I_MATNR             = P_MATNR
        I_MEIN1             = 'KG'
        I_MEIN2             = 'BAG'
        I_MEINS             = 'KG'
        I_MENGE             = VALOR
      IMPORTING
        MENGE               = VALOR_CONVERSION_FATURADO
      EXCEPTIONS
        ERROR_IN_CONVERSION = 01
        NO_SUCCESS          = 02.

  ELSE.
    VALOR_CONVERSION_FATURADO  = P_VALOR.
  ENDIF.

ENDFORM.                    " CONVERSION_SAC
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS: ZM_HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_DOUBLE_CLICK.
    PERFORM Z_HANDLE_DOUBLE_CLICK USING  E_ROW E_COLUMN ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Principal
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Principal
*----------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  CASE SY-DYNNR.
    WHEN: '0100'.
      CASE SY-UCOMM.
        WHEN: 'BACK'.
          LEAVE TO SCREEN 0.
        WHEN: 'CANC'.
          LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
        WHEN: 'EXIT'.
          LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Agrupamento
*----------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.

ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIAR_ALV_0200  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Agrupamento
*----------------------------------------------------------------------*
MODULE CRIAR_ALV_0200 OUTPUT.
  PERFORM: CRIAR_CATALOG_0200,
           CRIAR_ALV_0200.
ENDMODULE.                 " CRIAR_ALV_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Agrupamento
*----------------------------------------------------------------------*
MODULE PAI_0200 INPUT.

  DATA: SUBRC_MM     TYPE SY-TABIX,
        SUBRC_TRANSF TYPE SY-TABIX,
        ERROR        TYPE SY-TABIX.

  CASE SY-UCOMM.

    WHEN: 'TAB_PRINCIPAL'.
      REFRESH: IT_SAIDA_MM[], IT_SAIDA_TRANSF[].
      CLEAR: WA_SAIDA_MM, WA_SAIDA_TRANSF, VINCULADOS_EMAIL.
      TABSTRIP-ACTIVETAB = 'TAB_PRINCIPAL'.
    WHEN: 'TAB_MM_TRANSF'.
      CLEAR: VINCULADOS_EMAIL.
      IF NOT ( WA_ALV_VINC IS INITIAL ).
        CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.
      ENDIF.

      IF NOT ( IT_SAIDA_TRANSF IS INITIAL ) OR ( NOT IT_SAIDA_MM IS INITIAL ).
        TABSTRIP-ACTIVETAB = 'TAB_MM_TRANSF'.

      ELSE.
        MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Executar duplo clique em um registro.'.
      ENDIF.
    WHEN: 'TAB_VINCULACAO'.
      CLEAR: WA_SAIDA_MM, WA_SAIDA_TRANSF, VINCULADOS_EMAIL.

      READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.
      SUBRC_MM = SY-SUBRC.
      READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.
      SUBRC_TRANSF = SY-SUBRC.

      IF WA_SAIDA_MM-WERKS NE WA_SAIDA_TRANSF-RESWK.
        MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Centro do Pedido de Compra difere do Fornecedor do Pedido de Transferência'.
      ELSE.

*      IF ( subrc_mm NE 0 ) OR ( subrc_sd NE 0 ).
*        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.
*      ELSE.
        PERFORM: TAB_VINCULACAO.
        TABSTRIP-ACTIVETAB = 'TAB_VINCULACAO'.
      ENDIF.

    WHEN: 'TAB_VINCULADOS'.
      CLEAR: WA_SAIDA_MM, WA_SAIDA_SD, WA_SAIDA_TRANSF, WA_ALV_VINCULADOS, WA_CONT_VINCULADOS, WA_SAIDA_VINCULADOS.
      REFRESH: IT_FCAT_VINCULADOS[], IT_SAIDA_VINCULADOS[].

      READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.
      IF ( SY-SUBRC NE 0 ).
        ERROR = 1.
      ENDIF.
      SUBRC_MM = SY-SUBRC.

      READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.
      SUBRC_TRANSF = SY-SUBRC.

      IF ( SY-SUBRC NE 0 ).
        ERROR = ERROR + 1.
      ENDIF.

      IF ( SUBRC_MM EQ 0 ) AND ( SUBRC_TRANSF EQ 0 ).
        MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.
      ELSEIF ( SUBRC_TRANSF EQ 0 ) AND ( SUBRC_MM EQ 0 ).
        MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.

      ELSEIF ( ERROR EQ 2 ).
        MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.
      ELSE.
        VINCULADOS_EMAIL = 'X'.
        PERFORM: SELECIONAR_VINCULADOS,
                 CRIAR_ALV_VINCULADOS.
        TABSTRIP-ACTIVETAB = 'TAB_VINCULADOS'.
      ENDIF.


  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT

*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_0200
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_0200 .
  REFRESH: IT_FCAT[].

  PERFORM ALV_CATALOG_0200  USING:
        'MATNR'             'Material'               '10'       ''  'X' ''  ''      'MATN1' , " COD. MATERIAL
        'MAKTX'             'Descrição do Material'  '45'       ''  ''  ''  ''      ''      , " MATERIAL
        'TOTAL_PRODUZIDO_P' 'Produzido'              '15'       ''  ''  ''  ''      ''      , " PRODUZIDO
        'TOTAL_COMPRA'      'Qtd. Pedido Compra'     '13'       ''  ''  ''  'C300'  ''      , " COMPRA
        'TOTAL_VENDA'       'Qtd. Ordem'             '13'       ''  ''  ''  'C300'  ''      , " VENDA
        'TOTAL_TRANSF'      'Qtd. Pedido Transf.'    '13'       ''  ''  ''  'C300'  ''      , " TRANSF
        'SALDO'             'Saldo'                  '13'       ''  ''  ''  ''      ''      , " SALDO
        'TOTAL_F_C'         'Faturado Compra'        '13'       ''  ''  ''  'C500'  ''      , " SALDO FATURADO
        'SALDO_C'           'Saldo Compra'           '13'       ''  ''  ''  ''      ''      , " SALDO FATURADO
        'TOTAL_F_V'         'Faturado Venda'         '13'       ''  ''  ''  'C500'  ''      , " SALDO FATURADO
        'SALDO_V'           'Saldo Venda'            '13'       ''  ''  ''  ''      ''      , " SALDO FATURADO
        'TOTAL_F_T'         'Faturado Transf.'       '13'       ''  ''  ''  'C500'  ''      , " SALDO FATURADO
        'SALDO_T'           'Saldo Transf'           '13'       ''  ''  ''  ''      ''      . " SALDO FATURADO


ENDFORM.                    " CRIAR_CATALOG_0200
*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_0100
*&---------------------------------------------------------------------*
FORM ALV_CATALOG_0200  USING   P_CAMPO    TYPE C
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
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_0200
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_0200 .

  DATA: WA_EVENT_0200    TYPE REF TO LCL_EVENT_RECEIVER.

  IF WA_CONTAINER_0200 IS INITIAL.

    CREATE OBJECT WA_CONTAINER_0200
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_0200'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
  ENDIF.

  IF ( WA_ALV_0200 IS INITIAL ) AND ( NOT WA_CONTAINER_0200 IS INITIAL ) .

    CREATE OBJECT WA_ALV_0200
      EXPORTING
        I_PARENT          = WA_CONTAINER_0200
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

  IF WA_EVENT_0200 IS INITIAL.
    CREATE OBJECT WA_EVENT_0200.
    SET HANDLER: WA_EVENT_0200->ZM_HANDLE_DOUBLE_CLICK FOR WA_ALV_0200.
  ENDIF.

  WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.

  CALL METHOD WA_ALV_0200->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = GS_VARIANT_C
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = IT_SAIDA[]
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_0200
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM Z_HANDLE_DOUBLE_CLICK  USING    P_ROW
                                     P_COLUMN
                                     P_ROW_NO.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ROW.


  PERFORM COMPRAS USING  WA_SAIDA-MATNR.

  IF NOT ( IT_SAIDA_MM[] IS INITIAL ).

    "PERFORM vendas  USING  wa_saida-matnr.
    PERFORM TRANSFERENCIAS USING WA_SAIDA-MATNR.

    IF NOT ( IT_SAIDA_TRANSF[] IS INITIAL ).

      PERFORM: CRIAR_CATALOG_COMPRAS,
               CRIAR_ALV_COMPRAS.

      PERFORM: CRIAR_CATALOG_TRANSF,
               CRIAR_ALV_TRANSF.

      TABSTRIP-ACTIVETAB = 'TAB_MM_TRANSF'.

    ELSE.
      MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Não Contêm dados de Transferências.'.


    ENDIF.

  ELSE.
    MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Não Contêm dados de Compras.'.


  ENDIF.



ENDFORM.                    " Z_HANDLE_DOUBLE_CLICK
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_COMPRAS DEFINITION.


  PUBLIC SECTION.
    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_COMPRAS FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_COMPRAS IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT_COMPRAS.
    PERFORM Z_HOTSPOT_COMPRAS USING  E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_COMPRAS
*&---------------------------------------------------------------------*
FORM Z_HOTSPOT_COMPRAS USING    P_E_ROW_ID    TYPE  LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO   TYPE  LVC_S_ROID.


  DATA: TABIX     TYPE SY-TABIX,
        TABIX_AUX TYPE SY-TABIX.

  DATA OPT TYPE CTU_PARAMS.


  CLEAR: TABIX, TABIX_AUX.

  READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM INDEX P_E_ROW_ID.
  TABIX = SY-TABIX.
  IF ( SY-SUBRC EQ 0 ).
    CASE P_E_COLUMN_ID.
      WHEN: 'VINCULAR_C'.

        READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.
        TABIX_AUX = SY-TABIX.

        IF ( SY-SUBRC EQ 0 ) AND ( TABIX_AUX NE TABIX ).
          MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
        ELSE.
          PERFORM: SELECIONAR_COMPRAS USING WA_SAIDA_MM
                                            P_E_ROW_ID.
        ENDIF.
      WHEN: 'EBELN'.


        READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM INDEX P_E_ROW_ID.
        CLEAR: T_BDC[], T_MESSTAB.
        PERFORM F_BDC_FIELD USING: 'X' 'SAPLMEGUI'           '0014'             ,
                                   ' ' 'BDC_OKCODE'           '=MECHOB'          ,
                                   ' ' 'DYN_6000-LIST'       '1'                ,
                                   'X' 'SAPLMEGUI'           '0002'             ,
                                   ' ' 'BDC_OKCODE'           '=MEOK'            ,
                                   ' ' 'BDC_SUBSCR'           'SAPLMEGUI'        ,
                                   ' ' 'BDC_CURSOR'           'MEPO_SELECT-EBELN',
                                   ' ' 'MEPO_SELECT-EBELN'   WA_SAIDA_MM-EBELN     ,
                                   ' ' 'MEPO_SELECT-BSTYP_F' 'X'.

        OPT-DISMODE = 'E'.
        OPT-DEFSIZE = 'X'.
        CALL TRANSACTION 'ME23N' USING T_BDC OPTIONS FROM OPT MESSAGES INTO T_MESSTAB.

      WHEN: ''.

    ENDCASE.
  ENDIF.


*  CLEAR: wa_saida, wa_saida_mm, wa_saida_sd.
*  DATA: tabix     TYPE sy-tabix,
*        tabix_aux TYPE sy-tabix.
*
*  CASE sy-dynnr.
*    WHEN: '0100'.
*      READ TABLE it_saida INTO wa_saida INDEX p_e_row_id.
*    WHEN: '0200'.
*      READ TABLE it_saida_mm INTO wa_saida_mm INDEX p_e_row_id.
*    WHEN: '0300'.
*      READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.
*  ENDCASE.
*
*  CASE p_e_column_id.
*    WHEN: 'TOTAL_COMPRA'.
*      "PERFORM: SELECIONA_DADOS_COMPRA USING WA_SAIDA-MATNR.
*    WHEN: 'TOTAL_VENDA'.
*      "  PERFORM: SELECIONA_DADOS_VENDA USING WA_SAIDA-MATNR.
*
*    WHEN: 'VINCULAR_C'.
*
*      CLEAR: wa_saida_mm, tabix, tabix_aux.
*
*      READ TABLE it_saida_mm INTO wa_saida_mm INDEX p_e_row_id.
*      tabix = sy-tabix.
*      IF ( sy-subrc EQ 0 ).
*        READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
*
*        tabix_aux = sy-tabix.
*        IF ( sy-subrc EQ 0 ) AND ( tabix_aux NE tabix ).
*          MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
*
*        ELSE.
*          "PERFORM: VINCULAR_MM USING WA_SAIDA_MM
*          "                          P_E_ROW_ID.
*        ENDIF.
*      ENDIF.
*
*    WHEN: 'VINCULAR_V'.
*
*      CLEAR: wa_saida_sd, tabix, tabix_aux.
*
*      READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.
*      tabix = sy-tabix.
*
*      IF ( sy-subrc EQ 0 ).
*        READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.
*        tabix_aux = sy-tabix.
*        IF ( sy-subrc EQ 0 ) AND ( tabix_aux NE tabix ).
*          MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
*        ELSE.
*          "PERFORM: VINCULAR_SD USING WA_SAIDA_SD
*          "                           P_E_ROW_ID.
*        ENDIF.
*      ENDIF.
*
*      "Compras
*    WHEN: 'EBELN'.
*
*      DATA opt TYPE ctu_params.
*
*      READ TABLE it_saida_mm INTO wa_saida_mm INDEX p_e_row_id.
*      CLEAR: t_bdc[], t_messtab.
*      PERFORM f_bdc_field USING: 'X' 'SAPLMEGUI'           '0014'             ,
*                                 ' ' 'BDC_OKCODE'           '=MECHOB'          ,
*                                 ' ' 'DYN_6000-LIST'       '1'                ,
*                                 'X' 'SAPLMEGUI'           '0002'             ,
*                                 ' ' 'BDC_OKCODE'           '=MEOK'            ,
*                                 ' ' 'BDC_SUBSCR'           'SAPLMEGUI'        ,
*                                 ' ' 'BDC_CURSOR'           'MEPO_SELECT-EBELN',
*                                 ' ' 'MEPO_SELECT-EBELN'   wa_saida_mm-ebeln     ,
*                                 ' ' 'MEPO_SELECT-BSTYP_F' 'X'.
*
*      opt-dismode = 'E'.
*      opt-defsize = 'X'.
*      CALL TRANSACTION 'ME23N' USING t_bdc OPTIONS FROM opt MESSAGES INTO t_messtab.
*
*      "Vendas
*    WHEN: 'VBELN'.
*      READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.
*      SET PARAMETER ID 'VL' FIELD wa_saida_sd-vbeln.
*      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*
*
*  ENDCASE.
ENDFORM.                    " Z_HOTSPOT_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_COMPRAS
*&---------------------------------------------------------------------*
FORM SELECIONAR_COMPRAS  USING  P_SAIDA_MM STRUCTURE WA_SAIDA_MM
                                P_E_ROW_ID  TYPE LVC_S_ROW.

  WA_STABLE-ROW        = 'X'.
  IF ( P_SAIDA_MM-VINCULAR_C NE 'X' ).
    P_SAIDA_MM-VINCULAR_C = 'X'.
    MODIFY IT_SAIDA_MM FROM P_SAIDA_MM INDEX P_E_ROW_ID.
    CALL METHOD WA_ALV_COMPRAS->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ELSE.
    CLEAR: P_SAIDA_MM-VINCULAR_C.
    MODIFY IT_SAIDA_MM FROM P_SAIDA_MM INDEX P_E_ROW_ID.
    CALL METHOD WA_ALV_COMPRAS->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.

ENDFORM.                    " SELECIONAR_COMPRAS

*&---------------------------------------------------------------------*
*&      Form  COMPRAS
*&---------------------------------------------------------------------*
FORM COMPRAS  USING    P_MATNR.

  DATA: WA_ZSDT0065 TYPE ZSDT0065.

  CLEAR: IT_SAIDA_MM[], WA_LIFNR, WA_NAME1, WA_MATNR, WA_MAKTX, WA_MEINS.

  LOOP AT IT_COMPRA_AUX INTO WA_COMPRA_AUX WHERE MATNR EQ P_MATNR.

    WA_SAIDA_MM-NAME1    = WA_COMPRA_AUX-NAME1.
    WA_SAIDA_MM-EBELN    = WA_COMPRA_AUX-EBELN.
    WA_SAIDA_MM-EBELP    = WA_COMPRA_AUX-EBELP.
    WA_SAIDA_MM-MATNR    = WA_COMPRA_AUX-MATNR.
    WA_SAIDA_MM-MAKTX    = WA_COMPRA_AUX-MAKTX.
    WA_SAIDA_MM-WERKS    = WA_COMPRA_AUX-WERKS.

    WA_SAIDA_MM-MENGE    = WA_COMPRA_AUX-MENGE.

    SELECT SINGLE * FROM ZSDT0065 INTO WA_ZSDT0065 WHERE EBELN EQ WA_COMPRA_AUX-EBELN
                                                     AND EBELP EQ WA_COMPRA_AUX-EBELP
                                                     AND TIPO  EQ 'C'.

    IF ( SY-SUBRC EQ 0 ).
      WA_SAIDA_MM-QTD_VINC_MM = WA_ZSDT0065-QTD_VINC.
      WA_SAIDA_MM-SALDO_MM    = WA_SAIDA_MM-MENGE - WA_ZSDT0065-QTD_VINC.

    ELSE.
      WA_SAIDA_MM-SALDO_MM    = WA_SAIDA_MM-MENGE.
    ENDIF.

    WA_SAIDA_MM-NETWR    = WA_COMPRA_AUX-NETWR.
    WA_SAIDA_MM-WAERS    = WA_COMPRA_AUX-WAERS.
    WA_SAIDA_MM-NETPR    = WA_COMPRA_AUX-NETPR.

    IF ( WA_COMPRA_AUX-MEINS EQ 'BAG' ).
      WA_SAIDA_MM-MEINS    = 'SAC'.
    ELSE.
      WA_SAIDA_MM-MEINS = WA_COMPRA_AUX-MEINS.
    ENDIF.

    WA_SAIDA_MM-VINCULAR_C = ''.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_COMPRA_AUX-LIFNR
      IMPORTING
        OUTPUT = WA_LIFNR.

    WA_SAIDA_MM-LIFNR = WA_LIFNR.
    WA_NAME1          = WA_COMPRA_AUX-NAME1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_COMPRA_AUX-MATNR
      IMPORTING
        OUTPUT = WA_MATNR.

    WA_MAKTX = WA_COMPRA_AUX-MAKTX.
    WA_MEINS = WA_COMPRA_AUX-MEINS.

    APPEND WA_SAIDA_MM TO IT_SAIDA_MM.

    CLEAR: WA_COMPRA_AUX, WA_SAIDA_MM, WA_ZSDT0065.
  ENDLOOP.



ENDFORM.                    " COMPRAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_COMPRAS
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_COMPRAS .

  CLEAR: IT_FCAT_COMPRAS[].

  PERFORM FCAT_COMPRAS USING:
          'VINCULAR_C'  'Seleção'        '7'   'X' ''   ''    ''     'X', " Seleção
          'LIFNR'       'Cod.Fornecedor' '10'  ''  ''   ''    ''     '' , " Fornecedor
          'NAME1'       'Fornecedor'     '15'  ''  ''   ''    ''     '',
          'EBELN'       'Nr. Pedido'     '10'  'X' ''   ''    ''     '' , " N. PEDIDO
          'EBELP'       'Item Pedido'    '5'   ''  ''   ''    ''     '' , " Item
          'WERKS'       'Centro'         '5'   ''  ''   ''    ''      '',  "Centro
          'MENGE'       'Quantidade'     '13'  ''  ''   'X'   'C500'  '' , " Quantidade
          'QTD_VINC_MM' 'Qtd. Vinculado' '13'  ''  ''   'X'   'C500'  '' , " Qtd. Vinculado
          'SALDO_MM'    'Saldo'          '11'  ''  ''   'X'   'C400'  '' . " Saldo

ENDFORM.                    " CRIAR_CATALOG_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  FCAT_COMPRAS
*&---------------------------------------------------------------------*
FORM FCAT_COMPRAS USING P_CAMPO TYPE C
                        P_DESC  TYPE C
                        P_TAM   TYPE C
                        P_HOT   TYPE C
                        P_ZERO  TYPE C
                        P_SUM   TYPE C
                        P_COR   TYPE C
                        P_CHECK TYPE C.

  DATA: WL_FCAT_COMPRAS TYPE LVC_S_FCAT.

  WL_FCAT_COMPRAS-TABNAME   = 'IT_SAIDA_MM'.
  WL_FCAT_COMPRAS-FIELDNAME = P_CAMPO.
  WL_FCAT_COMPRAS-SCRTEXT_L = P_DESC.
  WL_FCAT_COMPRAS-SCRTEXT_M = P_DESC.
  WL_FCAT_COMPRAS-SCRTEXT_S = P_DESC.
  WL_FCAT_COMPRAS-HOTSPOT   = P_HOT.
  WL_FCAT_COMPRAS-DO_SUM    = P_SUM.
  WL_FCAT_COMPRAS-OUTPUTLEN = P_TAM.
  WL_FCAT_COMPRAS-NO_ZERO   = P_ZERO.
  WL_FCAT_COMPRAS-EMPHASIZE = P_COR.
  WL_FCAT_COMPRAS-CHECKBOX  = P_CHECK.

  APPEND WL_FCAT_COMPRAS TO IT_FCAT_COMPRAS.

ENDFORM.                    " FCAT_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_COMPRAS
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_COMPRAS .

  DATA: WA_LAYOUT_COMPRAS      TYPE LVC_S_LAYO.

  "DATA: WA_EVENT_COMPRAS    TYPE REF TO LCL_EVENT_COMPRAS.

  IF ( WA_CONT_COMPRAS IS INITIAL ).

    CREATE OBJECT WA_CONT_COMPRAS
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_COMPRAS'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.


    IF ( WA_ALV_COMPRAS IS INITIAL )  AND ( NOT WA_CONT_COMPRAS IS INITIAL ).

      CREATE OBJECT WA_ALV_COMPRAS
        EXPORTING
          I_PARENT          = WA_CONT_COMPRAS
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.
    ENDIF.
  ENDIF.


  " CREATE OBJECT WA_EVENT_COMPRAS.

  CALL METHOD WA_ALV_COMPRAS->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT_COMPRAS
    CHANGING
      IT_OUTTAB                     = IT_SAIDA_MM
      IT_FIELDCATALOG               = IT_FCAT_COMPRAS
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD WA_ALV_COMPRAS->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  "SET HANDLER: WA_EVENT_COMPRAS->ZM_HANDLE_HOTSPOT_COMPRAS FOR WA_ALV_COMPRAS.
  SET HANDLER: LCL_EVENT_COMPRAS=>ZM_HANDLE_HOTSPOT_COMPRAS FOR WA_ALV_COMPRAS.


ENDFORM.                    " CRIAR_ALV_COMPRAS

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_VENDAS DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_VENDAS FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_VENDAS IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT_VENDAS.
    PERFORM Z_HOTSPOT_VENDAS USING  E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_VENDAS
*&---------------------------------------------------------------------*
FORM Z_HOTSPOT_VENDAS  USING    P_E_ROW_ID
                                P_E_COLUMN_ID
                                P_ES_ROW_NO.

  DATA: TABIX     TYPE SY-TABIX,
        TABIX_AUX TYPE SY-TABIX.

  CLEAR: WA_SAIDA_SD, TABIX, TABIX_AUX.

  READ TABLE IT_SAIDA_SD INTO WA_SAIDA_SD INDEX P_E_ROW_ID.
  TABIX = SY-TABIX.
  IF ( SY-SUBRC EQ 0 ).
    CASE P_E_COLUMN_ID.
      WHEN: 'VINCULAR_V'.
        READ TABLE IT_SAIDA_SD INTO WA_SAIDA_SD WITH KEY VINCULAR_V = 'X'.
        TABIX_AUX = SY-TABIX.
        IF ( SY-SUBRC EQ 0 ) AND ( TABIX_AUX NE TABIX ).
          MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
        ELSE.
          PERFORM: SELECIONAR_VENDAS USING  WA_SAIDA_SD
                                            P_E_ROW_ID.
        ENDIF.

      WHEN: 'VBELN'.
        READ TABLE IT_SAIDA_SD INTO WA_SAIDA_SD INDEX P_E_ROW_ID.
        SET PARAMETER ID 'VL' FIELD WA_SAIDA_SD-VBELN.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.
ENDFORM.                    " Z_HOTSPOT_VENDAS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VENDAS
*&---------------------------------------------------------------------*
FORM SELECIONAR_VENDAS  USING    P_SAIDA_VENDAS STRUCTURE WA_SAIDA_SD
                                 P_E_ROW_ID     TYPE LVC_S_ROW.

  IF ( P_SAIDA_VENDAS-VINCULAR_V NE 'X' ).

    P_SAIDA_VENDAS-VINCULAR_V = 'X'.
    MODIFY IT_SAIDA_SD FROM P_SAIDA_VENDAS INDEX P_E_ROW_ID.
    CALL METHOD WA_ALV_VENDAS->REFRESH_TABLE_DISPLAY.
    WA_EDIT_VINC = 'X'.

  ELSE.
    CLEAR: P_SAIDA_VENDAS-VINCULAR_V.
    MODIFY IT_SAIDA_SD FROM P_SAIDA_VENDAS INDEX P_E_ROW_ID.
    CALL METHOD WA_ALV_VENDAS->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    " SELECIONAR_VENDAS

*&---------------------------------------------------------------------*
*&      Form  VENDAS
*&---------------------------------------------------------------------*
FORM VENDAS  USING    P_MATNR.

  DATA: WA_ZSDT0065 TYPE ZSDT0065.

  CLEAR: IT_SAIDA_SD[].

  LOOP AT IT_VENDA INTO WA_VENDA WHERE MATNR EQ P_MATNR.

    CLEAR: WA_SAIDA_SD-STATUS.

    LOOP AT IT_0026 WHERE VBELN EQ WA_VENDA-VBELN.

      READ TABLE IT_ZIB_BSIK WITH KEY BELNR = IT_0026-DOCNUM.

      READ TABLE IT_BSIK WITH KEY BUKRS = IT_ZIB_BSIK-BUKRS
                                  BELNR = IT_ZIB_BSIK-BELNR
                                  GJAHR = IT_ZIB_BSIK-GJAHR.

      IF SY-SUBRC IS INITIAL.
        IF WA_SAIDA_SD-STATUS EQ 'Q'.
          WA_SAIDA_SD-STATUS = 'P'.
          SY-SUBRC = 0.
          CONTINUE.
        ELSE.
          SY-SUBRC = 8.
        ENDIF.

      ELSE.
        WA_SAIDA_SD-STATUS = 'Q'.
        SY-SUBRC = 0.

      ENDIF .

    ENDLOOP.
    IF SY-SUBRC IS NOT INITIAL.
      WA_SAIDA_SD-STATUS = 'A'.

    ENDIF.

    READ TABLE IT_VBKD WITH KEY VBELN = WA_VENDA-VBELN.

    IF SY-SUBRC IS INITIAL .
      READ TABLE IT_T052U WITH KEY ZTERM = IT_VBKD-ZTERM.

      IF SY-SUBRC IS INITIAL.
        CONCATENATE IT_VBKD-ZTERM '-' IT_T052U-TEXT1 INTO WA_SAIDA_SD-ZTERM SEPARATED BY SPACE.
      ENDIF.
    ENDIF.


    WA_SAIDA_SD-KUNNR    = WA_VENDA-KUNNR.
    WA_SAIDA_SD-NAME1    = WA_VENDA-NAME1.
    WA_SAIDA_SD-VBELN    = WA_VENDA-VBELN.
    WA_SAIDA_SD-POSNR    = WA_VENDA-POSNR.
    WA_SAIDA_SD-MATNR    = WA_VENDA-MATNR.
    WA_SAIDA_SD-ARKTX    = WA_VENDA-ARKTX.
    WA_SAIDA_SD-KWMENG   = WA_VENDA-KWMENG.

    SELECT SINGLE * FROM ZSDT0065 INTO WA_ZSDT0065 WHERE VBELN EQ WA_VENDA-VBELN
                                                     AND POSNR EQ WA_VENDA-POSNR
                                                     AND TIPO  EQ 'V'.

    IF ( SY-SUBRC EQ 0 ).
      WA_SAIDA_SD-QTD_VINC_SD = WA_ZSDT0065-QTD_VINC.
      WA_SAIDA_SD-SALDO_SD    = WA_VENDA-KWMENG - WA_ZSDT0065-QTD_VINC.
    ELSE.
      WA_SAIDA_SD-SALDO_SD    = WA_VENDA-KWMENG.
    ENDIF.

    WA_SAIDA_SD-NETWR    = WA_VENDA-NETWR.
    WA_SAIDA_SD-WAERK    = WA_VENDA-WAERK.
    WA_SAIDA_SD-KBETR    = WA_VENDA-KBETR.

    WA_SAIDA_SD-VKBUR    = WA_VENDA-VKBUR.

    IF ( WA_VENDA-VRKME EQ 'BAG' ).
      WA_SAIDA_SD-VRKME    = 'SAC'.
    ELSE.
      WA_SAIDA_SD-VRKME = WA_VENDA-VRKME.
    ENDIF.

    WA_SAIDA_SD-VINCULAR_V = ''.

    APPEND WA_SAIDA_SD TO IT_SAIDA_SD.
    CLEAR: WA_VENDA, WA_SAIDA_SD.
  ENDLOOP.

ENDFORM.                    " VENDAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VENDAS
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_VENDAS .

  REFRESH: IT_FCAT_VENDAS[].
  PERFORM FCAT_VENDAS USING:

        'VINCULAR_V'  'Seleção'             '7' 'X'   ''  ''   ''       'X', " Seleção
        'KUNNR'       'Código Cliente'      '10' ''   'X' ''  ''       '', " Codigo do Cliente
        'NAME1'       'Cliente'             '35' ''   ''  ''  ''       '', " Cliente
        'VKBUR'       'Escritorio de Venda' '10' ''   ''  ''  ''       '', " Contrato
        'VBELN'       'Ordem de Venda'      '10' 'X'  ''  ''  ''       '', " Contrato
        'POSNR'       'Item'                '7'  ''   ''  ''  ''       '', " Item
        'KWMENG'      'Quantidade'          '13' ''   ''  'X' 'C500'    '' , " Quantidade
        'QTD_VINC_SD' 'Qtd. Vinculada'      '13' ''   ''  'X' 'C500'    '' , " VALOR
        'SALDO_SD'    'Saldo'               '13' ''   ''  'X' 'C400'    '' .

ENDFORM.                    " CRIAR_CATALOG_VENDAS
*&---------------------------------------------------------------------*
*&      Form  FCAT_VENDAS
*&---------------------------------------------------------------------*
FORM FCAT_VENDAS USING   P_CAMPO TYPE C
                         P_DESC  TYPE C
                         P_TAM   TYPE C
                         P_HOT   TYPE C
                         P_ZERO  TYPE C
                         P_SUM   TYPE C
                         P_COR   TYPE C
                         P_CHECK TYPE C.

  DATA: WL_FCAT_VENDAS TYPE LVC_S_FCAT.

  WL_FCAT_VENDAS-TABNAME   = 'IT_SAIDA_SD'.
  WL_FCAT_VENDAS-FIELDNAME = P_CAMPO.
  WL_FCAT_VENDAS-SCRTEXT_L = P_DESC.
  WL_FCAT_VENDAS-SCRTEXT_M = P_DESC.
  WL_FCAT_VENDAS-SCRTEXT_S = P_DESC.
  WL_FCAT_VENDAS-HOTSPOT   = P_HOT.
  WL_FCAT_VENDAS-DO_SUM    = P_SUM.
  WL_FCAT_VENDAS-OUTPUTLEN = P_TAM.
  WL_FCAT_VENDAS-NO_ZERO   = P_ZERO.
  WL_FCAT_VENDAS-EMPHASIZE = P_COR.
  WL_FCAT_VENDAS-CHECKBOX  = P_CHECK.

  APPEND WL_FCAT_VENDAS TO IT_FCAT_VENDAS.

ENDFORM.                    " FCAT_VENDAS

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VENDAS
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_VENDAS .

  DATA: WA_LAYOUT_VENDAS     TYPE LVC_S_LAYO.

  "DATA: WA_EVENT_VENDAS    TYPE REF TO LCL_EVENT_VENDAS.


  IF WA_CONT_VENDAS IS INITIAL.

    CREATE OBJECT WA_CONT_VENDAS
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_VENDAS'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT WA_ALV_VENDAS
      EXPORTING
        I_PARENT          = WA_CONT_VENDAS
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

  "CREATE OBJECT WA_EVENT_VENDAS.

  CALL METHOD WA_ALV_VENDAS->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT_VENDAS
    CHANGING
      IT_OUTTAB                     = IT_SAIDA_SD
      IT_FIELDCATALOG               = IT_FCAT_VENDAS
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD WA_ALV_VENDAS->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

*  SET HANDLER: WA_EVENT_VENDAS->ZM_HANDLE_HOTSPOT_VENDAS FOR WA_ALV_VENDAS.
  SET HANDLER: LCL_EVENT_VENDAS=>ZM_HANDLE_HOTSPOT_VENDAS FOR WA_ALV_VENDAS.

ENDFORM.                    " CRIAR_ALV_VENDAS


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_TRANSF DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_TRANSF FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_TRANSF IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT_TRANSF.
    PERFORM Z_HOTSPOT_TRANSF USING  E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


FORM Z_HOTSPOT_TRANSF  USING    P_E_ROW_ID
                                P_E_COLUMN_ID
                                P_ES_ROW_NO.

  DATA: TABIX     TYPE SY-TABIX,
        TABIX_AUX TYPE SY-TABIX.

  DATA OPT TYPE CTU_PARAMS.

  CLEAR: TABIX, TABIX_AUX.

  READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF INDEX P_E_ROW_ID.
  TABIX = SY-TABIX.
  IF ( SY-SUBRC EQ 0 ).
    CASE P_E_COLUMN_ID.
      WHEN: 'VINCULAR_T'.
        READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.
        TABIX_AUX = SY-TABIX.
        IF ( SY-SUBRC EQ 0 ) AND ( TABIX_AUX NE TABIX ).
          MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
        ELSE.
          PERFORM: SELECIONAR_TRANSF USING  WA_SAIDA_TRANSF
                                            P_E_ROW_ID.
        ENDIF.

      WHEN: 'EBELN'.

        READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF INDEX P_E_ROW_ID.
        CLEAR: T_BDC[], T_MESSTAB.
        PERFORM F_BDC_FIELD USING: 'X' 'SAPLMEGUI'           '0014'             ,
                                   ' ' 'BDC_OKCODE'           '=MECHOB'          ,
                                   ' ' 'DYN_6000-LIST'       '1'                ,
                                   'X' 'SAPLMEGUI'           '0002'             ,
                                   ' ' 'BDC_OKCODE'           '=MEOK'            ,
                                   ' ' 'BDC_SUBSCR'           'SAPLMEGUI'        ,
                                   ' ' 'BDC_CURSOR'           'MEPO_SELECT-EBELN',
                                   ' ' 'MEPO_SELECT-EBELN'   WA_SAIDA_TRANSF-EBELN     ,
                                   ' ' 'MEPO_SELECT-BSTYP_F' 'X'.

        OPT-DISMODE = 'E'.
        OPT-DEFSIZE = 'X'.
        CALL TRANSACTION 'ME23N' USING T_BDC OPTIONS FROM OPT MESSAGES INTO T_MESSTAB.

    ENDCASE.
  ENDIF.
ENDFORM.                    " Z_HOTSPOT_transf

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_transf
*&---------------------------------------------------------------------*
FORM SELECIONAR_TRANSF  USING    P_SAIDA_TRANSF STRUCTURE WA_SAIDA_TRANSF
                                 P_E_ROW_ID     TYPE LVC_S_ROW.

  IF ( P_SAIDA_TRANSF-VINCULAR_T NE 'X' ).

    P_SAIDA_TRANSF-VINCULAR_T = 'X'.
    MODIFY IT_SAIDA_TRANSF FROM P_SAIDA_TRANSF INDEX P_E_ROW_ID.
    CALL METHOD WA_ALV_TRANSF->REFRESH_TABLE_DISPLAY.
    WA_EDIT_VINC = 'X'.

  ELSE.
    CLEAR: P_SAIDA_TRANSF-VINCULAR_T.
    MODIFY IT_SAIDA_TRANSF FROM P_SAIDA_TRANSF INDEX P_E_ROW_ID.
    CALL METHOD WA_ALV_TRANSF->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    " SELECIONAR_VENDAS

*&---------------------------------------------------------------------*
*&      Form  TRANSFERENCIAS
*&---------------------------------------------------------------------*
FORM TRANSFERENCIAS  USING    P_MATNR.

  DATA: WA_ZSDT0065 TYPE ZSDT0065.

  CLEAR: IT_SAIDA_TRANSF[], WA_T001W, WA_NAME1, WA_MATNR, WA_MAKTX, WA_MEINS.

  LOOP AT IT_TRANSF_AUX INTO WA_TRANSF_AUX WHERE MATNR EQ P_MATNR.

    WA_SAIDA_TRANSF-NAME1    = WA_TRANSF_AUX-NAME1.
    WA_SAIDA_TRANSF-EBELN    = WA_TRANSF_AUX-EBELN.
    WA_SAIDA_TRANSF-EBELP    = WA_TRANSF_AUX-EBELP.
    WA_SAIDA_TRANSF-MATNR    = WA_TRANSF_AUX-MATNR.
    WA_SAIDA_TRANSF-MAKTX    = WA_TRANSF_AUX-MAKTX.

    WA_SAIDA_TRANSF-MENGE    = WA_TRANSF_AUX-MENGE.

    SELECT SINGLE * FROM ZSDT0065 INTO WA_ZSDT0065 WHERE EBELN_T EQ WA_TRANSF_AUX-EBELN
                                                     AND EBELP_T EQ WA_TRANSF_AUX-EBELP
                                                     AND TIPO    EQ 'T'.

    IF ( SY-SUBRC EQ 0 ).
      WA_SAIDA_TRANSF-QTD_VINC_T = WA_ZSDT0065-QTD_VINC.
      WA_SAIDA_TRANSF-SALDO_T    = WA_SAIDA_TRANSF-MENGE - WA_ZSDT0065-QTD_VINC.

    ELSE.
      WA_SAIDA_TRANSF-SALDO_T    = WA_SAIDA_TRANSF-MENGE.
    ENDIF.

    WA_SAIDA_TRANSF-NETWR    = WA_TRANSF_AUX-NETWR.
    WA_SAIDA_TRANSF-WAERS    = WA_TRANSF_AUX-WAERS.
    WA_SAIDA_TRANSF-NETPR    = WA_TRANSF_AUX-NETPR.

    IF ( WA_TRANSF_AUX-MEINS EQ 'BAG' ).
      WA_SAIDA_TRANSF-MEINS    = 'SAC'.
    ELSE.
      WA_SAIDA_TRANSF-MEINS = WA_TRANSF_AUX-MEINS.
    ENDIF.

    WA_SAIDA_TRANSF-VINCULAR_T = ''.

    WA_SAIDA_TRANSF-RESWK = WA_TRANSF_AUX-RESWK.
    WA_NAME1              = WA_TRANSF_AUX-NAME1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_TRANSF_AUX-MATNR
      IMPORTING
        OUTPUT = WA_MATNR.

    WA_MAKTX = WA_TRANSF_AUX-MAKTX.
    WA_MEINS = WA_TRANSF_AUX-MEINS.

    APPEND WA_SAIDA_TRANSF TO IT_SAIDA_TRANSF.

    CLEAR: WA_TRANSF_AUX, WA_SAIDA_TRANSF, WA_ZSDT0065.
  ENDLOOP.



ENDFORM.                    " TRANSFERENCIAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_TRANSF
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_TRANSF.

  CLEAR: IT_FCAT_TRANSF[].

  PERFORM FCAT_TRANSF USING:
          'VINCULAR_T'  'Seleção'           '7'   'X' ''   ''    ''     'X', " Seleção
          'RESWK'       'Cod.Fornecedor'    '10'  ''  ''   ''    ''     '' , " Fornecedor
          'NAME1'       'Fornecedor'        '15'  ''  ''   ''    ''     '',
          'EBELN'       'Nr. Pedido Transf' '10'  'X' ''   ''    ''     '' , " N. PEDIDO
          'EBELP'       'Item Pedido'       '5'   ''  ''   ''    ''     '' , " Item
          'MENGE'       'Quantidade'        '13'  ''  ''   'X'   'C500'  '' , " Quantidade
          'QTD_VINC_T'  'Qtd. Vinculado'    '13'  ''  ''   'X'   'C500'  '' , " Qtd. Vinculado
          'SALDO_T'     'Saldo'             '11'  ''  ''   'X'   'C400'  '' . " Saldo

ENDFORM.                    " CRIAR_CATALOG_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  FCAT_COMPRAS
*&---------------------------------------------------------------------*
FORM FCAT_TRANSF  USING P_CAMPO TYPE C
                        P_DESC  TYPE C
                        P_TAM   TYPE C
                        P_HOT   TYPE C
                        P_ZERO  TYPE C
                        P_SUM   TYPE C
                        P_COR   TYPE C
                        P_CHECK TYPE C.

  DATA: WL_FCAT_TRANSF TYPE LVC_S_FCAT.

  WL_FCAT_TRANSF-TABNAME   = 'IT_SAIDA_MM'.
  WL_FCAT_TRANSF-FIELDNAME = P_CAMPO.
  WL_FCAT_TRANSF-SCRTEXT_L = P_DESC.
  WL_FCAT_TRANSF-SCRTEXT_M = P_DESC.
  WL_FCAT_TRANSF-SCRTEXT_S = P_DESC.
  WL_FCAT_TRANSF-HOTSPOT   = P_HOT.
  WL_FCAT_TRANSF-DO_SUM    = P_SUM.
  WL_FCAT_TRANSF-OUTPUTLEN = P_TAM.
  WL_FCAT_TRANSF-NO_ZERO   = P_ZERO.
  WL_FCAT_TRANSF-EMPHASIZE = P_COR.
  WL_FCAT_TRANSF-CHECKBOX  = P_CHECK.

  APPEND WL_FCAT_TRANSF TO IT_FCAT_TRANSF.

ENDFORM.                    " FCAT_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_TRANSF
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_TRANSF .

  DATA: WA_LAYOUT_TRANSF      TYPE LVC_S_LAYO.

  "DATA: WA_EVENT_COMPRAS    TYPE REF TO LCL_EVENT_COMPRAS.

  IF ( WA_CONT_TRANSF IS INITIAL ).

    CREATE OBJECT WA_CONT_TRANSF
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_TRANSFERENCIAS'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.


    IF ( WA_ALV_TRANSF IS INITIAL )  AND ( NOT WA_CONT_TRANSF IS INITIAL ).

      CREATE OBJECT WA_ALV_TRANSF
        EXPORTING
          I_PARENT          = WA_CONT_TRANSF
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.
    ENDIF.
  ENDIF.


  " CREATE OBJECT WA_EVENT_COMPRAS.

  CALL METHOD WA_ALV_TRANSF->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT_TRANSF
    CHANGING
      IT_OUTTAB                     = IT_SAIDA_TRANSF
      IT_FIELDCATALOG               = IT_FCAT_TRANSF
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD WA_ALV_COMPRAS->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  "SET HANDLER: WA_EVENT_COMPRAS->ZM_HANDLE_HOTSPOT_COMPRAS FOR WA_ALV_COMPRAS.
  SET HANDLER: LCL_EVENT_TRANSF=>ZM_HANDLE_HOTSPOT_TRANSF FOR WA_ALV_TRANSF.


ENDFORM.                    " CRIAR_ALV_COMPRAS



*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
FORM F_BDC_FIELD  USING    VALUE(P_FLAG)
                           VALUE(P_FNAM)
                           VALUE(P_FVAL).
  CLEAR T_BDC.
  IF NOT P_FLAG IS INITIAL.
    T_BDC-PROGRAM  = P_FNAM.
    T_BDC-DYNPRO   = P_FVAL.
    T_BDC-DYNBEGIN = 'X'.
  ELSE.
    T_BDC-FNAM = P_FNAM.
    T_BDC-FVAL = P_FVAL.
  ENDIF.
  APPEND T_BDC.

ENDFORM.                    "f_bdc_field






*&---------------------------------------------------------------------*
*&      Form  TAB_VINCULACAO
*&---------------------------------------------------------------------*
FORM TAB_VINCULACAO .

  CLEAR: WA_SAIDA_MM, WA_SAIDA_TRANSF.

  READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.

  IF NOT ( WA_SAIDA_MM-SALDO_MM IS INITIAL ) OR ( WA_SAIDA_MM-SALDO_MM EQ 0 ).
    WA_SALDO_C = WA_SAIDA_MM-SALDO_MM.
  ELSE.
    WA_SALDO_C = WA_SAIDA_MM-QTD_VINC_MM.
  ENDIF.

  READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.

  IF NOT ( WA_SAIDA_TRANSF-SALDO_T IS INITIAL ) OR ( WA_SAIDA_TRANSF-SALDO_T EQ 0 ).
    WA_SALDO_T = WA_SAIDA_TRANSF-SALDO_T.
  ELSE.
    WA_SALDO_T = WA_SAIDA_TRANSF-QTD_VINC_T.
  ENDIF.

  WA_DATA = SY-DATUM.

  PERFORM:   SELECIONAR_VINCULACAO,
             CRIAR_CATALOG_VINCULACAO,
             CRIAR_ALV_VINCULACAO.


ENDFORM.                    " TAB_VINCULACAO


*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Compra e Vendas - Vinculação
*----------------------------------------------------------------------*
MODULE PBO_0300 OUTPUT.

ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Compra e Vendas - Vinculação
*----------------------------------------------------------------------*
MODULE PAI_0300 INPUT.

ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Vinculação de Compra e Venda
*----------------------------------------------------------------------*
MODULE PBO_0400 OUTPUT.

  IF ( WA_OPCAO NE 'X' ).
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'BTN_VINCULAR' ).
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.

      IF ( SCREEN-NAME EQ 'BTN_SALVAR' ).
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'BTN_VINCULAR' ).
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '1'.
        MODIFY SCREEN.
      ENDIF.

      IF ( SCREEN-NAME EQ 'BTN_SALVAR' ).
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '1'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDMODULE.                 " PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0400  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Vinculação de Compra e Venda
*----------------------------------------------------------------------*
MODULE PAI_0400 INPUT.

  DATA: WA_ZSDT0062_EMAIL TYPE ZSDT0062,
        VAR_OK            TYPE C.


  CASE SY-UCOMM.
    WHEN: 'ENTER'.
      IF ( WA_DATA IS INITIAL ) OR ( WA_LOCAL IS INITIAL ) OR ( WA_QTD_VINC IS INITIAL ).
        MESSAGE W899(MM) WITH 'Informar os campos obrigatorios'.
      ELSE.
        IF ( WA_QTD_VINC > WA_SALDO_C  ) .
          MESSAGE W899(MM) WITH 'Qtd. maior que o saldo de compra'.
        ELSEIF ( WA_QTD_VINC > WA_SALDO_T  ) .
          MESSAGE W899(MM) WITH 'Qtd. maior que o saldo de Transferência'.
        ELSE.

          WA_OPCAO = 'X'.
        ENDIF.
      ENDIF.

    WHEN: 'BTN_VINCULAR'.
      PERFORM: PREENCHER_VINCULACAO.

    WHEN: 'BTN_SALVAR'.
      IF  NOT ( IT_SAIDA_VINC[] IS INITIAL ) .
        CLEAR: WA_OPCAO.
        PERFORM: SALVAR_VINCULACAO.
      ELSE.
        MESSAGE W899(MM) WITH 'Não existe vinculação.'.
      ENDIF.

    WHEN: 'BTN_EMAIL'.


      CALL METHOD WA_ALV_VINC->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TL_ROWS.


      LOOP AT TL_ROWS INTO SL_ROWS.

        READ TABLE IT_SAIDA_VINC INTO WA_SAIDA_VINC INDEX  SL_ROWS-INDEX.

        SELECT SINGLE * FROM ZSDT0062 INTO WA_ZSDT0062_EMAIL  WHERE EBELN    EQ WA_SAIDA_VINC-EBELN
                                                                AND EBELP    EQ WA_SAIDA_VINC-EBELP
                                                                AND VBELN    EQ WA_SAIDA_VINC-EBELN_T
                                                                "AND posnr    EQ wa_saida_vinc-posnr
                                                                AND SOL_VINC EQ WA_SAIDA_VINC-SOL_VINC.

        IF ( SY-SUBRC EQ 0 ).

          IF ( WA_ZSDT0062_EMAIL-ENVIO_EMAIL EQ 'G' ).
            MESSAGE W899(MM) DISPLAY LIKE 'E' WITH 'E-mail já enviado.'.
            CLEAR: VAR_OK.
          ELSE.
            VAR_OK = 'X'.
          ENDIF.
        ELSE.
          VAR_OK = 'X'.
        ENDIF.
      ENDLOOP.


      IF ( VAR_OK EQ 'X' ).
        PERFORM: CAIXA_EMAIL.
        CALL SCREEN 0500 STARTING AT 2 1 ENDING AT 95 25.
      ENDIF.


  ENDCASE.
ENDMODULE.                 " PAI_0400  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VINCULACAO
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_VINCULACAO .

  PERFORM PREENCHE_FCAT_VINCULACAO USING:

      'STATUS'    'Status'              '5'  ''  ''  ''  '' '' 'C',
      'EBELN'     'Nº.Pedido'           '10' ''  ''  ''  '' '' '',
      'EBELP'     'Item'                '3'  ''  ''  ''  '' '' '',
      'LIFNR'     'Cod.Forn.'           '6'  ''  ''  ''  '' '' '',
      'NAME'      'Desc.Forn.'          '10' ''  ''  ''  '' '' '',
      'EBELN_T'   'Pedido Transf'       '10'  ''  ''  ''  '' '' '',
      'EBELP_T'   'Item Transf'         '3' ''  ''  ''  '' '' '',
      'RESWK_T'   'Cod.Forn.Transf'     '6' ''  ''  ''  '' '' '',
      'NAME_T'    'Forn.Transf'         '10' ''  ''  ''  '' '' '',
      'MATNR'     'Material'            '12' ''  ''  ''  '' '' '',
      'QTD_V'     'Qtd.Vinc'            '10' ''  ''  ''  '' '' '',
      'LOCAL_EMB' 'Local de Embarque'   '10' ''  ''  ''  '' '' '',
      'DATA '     'Data.Vinc'           '10' ''  ''  ''  '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_FCAT_VINCULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREENCHE_FCAT_VINCULACAO   USING  P_CAMPO TYPE C
                                       P_DESC  TYPE C
                                       P_TAM   TYPE C
                                       P_HOT   TYPE C
                                       P_ZERO  TYPE C
                                       P_SUM   TYPE C
                                       P_COR   TYPE C
                                       P_CHECK TYPE C
                                       P_JUST  TYPE C.

  DATA: WL_FCAT_VINC TYPE LVC_S_FCAT.

  WL_FCAT_VINC-TABNAME   = 'IT_SAIDA_VINC'.
  WL_FCAT_VINC-FIELDNAME = P_CAMPO.
  WL_FCAT_VINC-SCRTEXT_L = P_DESC.
  WL_FCAT_VINC-SCRTEXT_M = P_DESC.
  WL_FCAT_VINC-SCRTEXT_S = P_DESC.
  WL_FCAT_VINC-HOTSPOT   = P_HOT.
  WL_FCAT_VINC-DO_SUM    = P_SUM.
  WL_FCAT_VINC-OUTPUTLEN = P_TAM.
  WL_FCAT_VINC-NO_ZERO   = P_ZERO.
  WL_FCAT_VINC-EMPHASIZE = P_COR.
  WL_FCAT_VINC-CHECKBOX  = P_CHECK.
  WL_FCAT_VINC-JUST      = P_JUST.


  APPEND WL_FCAT_VINC TO IT_FCAT_VINC.


ENDFORM.                    " PREENCHE_FCAT_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VINCULACAO
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_VINCULACAO .

  DATA: GS_VARIANT  TYPE DISVARIANT.
  DATA: WA_STABLE   TYPE LVC_S_STBL.
  GS_VARIANT-REPORT  = SY-REPID.

  IF ( WA_CONT_VINC IS INITIAL ).

    CREATE OBJECT WA_CONT_VINC
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_VINCULACAO'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT WA_ALV_VINC
      EXPORTING
        I_PARENT          = WA_CONT_VINC
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    WA_STABLE-ROW             = 'X'.
    WA_LAYOUT_VINC-SEL_MODE   = 'A'.

    CALL METHOD WA_ALV_VINC->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT_VINC
        IS_VARIANT                    = GS_VARIANT
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_VINC
        IT_FIELDCATALOG               = IT_FCAT_VINC
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_VINCULACAO
*&---------------------------------------------------------------------*
FORM PREENCHER_VINCULACAO .

  IF ( WA_DATA IS INITIAL ) OR ( WA_LOCAL IS INITIAL ) OR ( WA_QTD_VINC IS INITIAL ).
    MESSAGE W899(MM) WITH 'Informar os campos obrigatorios'.
  ELSE.

    IF ( WA_QTD_VINC > WA_SALDO_C  ) .
      MESSAGE W899(MM) WITH 'Qtd. maior que o saldo de compra'.

    ELSEIF ( WA_QTD_VINC > WA_SALDO_T ).
      MESSAGE W899(MM) WITH 'Qtd. maior que o saldo de transferência'.
    ELSE.
      CLEAR: WA_SAIDA_MM, WA_SAIDA_TRANSF.

      READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.

      IF ( SY-SUBRC EQ 0 ).

        READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.

        IF ( SY-SUBRC EQ 0 ).

          "Informações MM
          WA_SAIDA_VINC-EBELN = WA_SAIDA_MM-EBELN.
          WA_SAIDA_VINC-EBELP = WA_SAIDA_MM-EBELP.
          WA_SAIDA_VINC-LIFNR = WA_SAIDA_MM-LIFNR.
          WA_SAIDA_VINC-NAME  = WA_SAIDA_MM-NAME1.

          "Informações Transf
          WA_SAIDA_VINC-EBELN_T = WA_SAIDA_TRANSF-EBELN.
          WA_SAIDA_VINC-EBELP_T = WA_SAIDA_TRANSF-EBELP.
          WA_SAIDA_VINC-RESWK_T = WA_SAIDA_TRANSF-RESWK.
          WA_SAIDA_VINC-NAME_T = WA_SAIDA_TRANSF-NAME1.


          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_SAIDA_TRANSF-MATNR
            IMPORTING
              OUTPUT = WA_SAIDA_VINC-MATNR.

          WA_SAIDA_VINC-LOCAL_EMB = WA_LOCAL.
          WA_SAIDA_VINC-QTD_V = WA_QTD_VINC.
          WA_SALDO_C = WA_SALDO_C - WA_QTD_VINC.
          WA_SALDO_V = WA_SALDO_T - WA_QTD_VINC.
          WA_SAIDA_VINC-DATA = WA_DATA.

          WA_SAIDA_VINC-STATUS     = ICON_LED_RED.

          APPEND WA_SAIDA_VINC TO IT_SAIDA_VINC.

          IF NOT ( IT_SAIDA_VINC[] IS INITIAL ).

            IF NOT ( WA_SAIDA_MM-QTD_VINC_MM IS INITIAL ).
              WA_SAIDA_MM-QTD_VINC_MM = WA_SAIDA_MM-QTD_VINC_MM +   WA_QTD_VINC.
              WA_SAIDA_MM-SALDO_MM    = WA_SAIDA_MM-MENGE - WA_SAIDA_MM-QTD_VINC_MM.
            ELSE.
              WA_SAIDA_MM-QTD_VINC_MM = WA_QTD_VINC.
              WA_SAIDA_MM-SALDO_MM    = WA_SALDO_C.
            ENDIF.
            CLEAR: WA_DATA, WA_LOCAL, WA_QTD_VINC.
            CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.
          ENDIF.
        ELSE.
          MESSAGE W899(MM) WITH 'Selecionar um Item Transf.'.
        ENDIF.
      ELSE.
        MESSAGE W899(MM) WITH 'Selecionar um Item MM.'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " PREENCHER_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  SALVAR_VINCULACAO
*&---------------------------------------------------------------------*
FORM SALVAR_VINCULACAO .

  DATA: WA_ZSDT0062_INSR TYPE ZSDT0062,
        WA_ZSDT0065_INSR TYPE ZSDT0065,
        P_ZID            TYPE NUMC10,
        TABIX            TYPE SY-TABIX.

  DATA: IDX_MM TYPE SY-TABIX,
        IDX_SD TYPE SY-TABIX.


  CLEAR: WA_SAIDA_VINC.

  LOOP AT IT_SAIDA_VINC INTO WA_SAIDA_VINC.

    CLEAR: WA_ZSDT0062_INSR.
    SELECT SINGLE * FROM ZSDT0062 INTO WA_ZSDT0062_INSR WHERE EBELN       EQ WA_SAIDA_VINC-EBELN
                                                          AND EBELP       EQ WA_SAIDA_VINC-EBELP
                                                          AND EBELN_T     EQ WA_SAIDA_VINC-EBELN_T
                                                          "AND posnr       EQ wa_saida_vinc-posnr
                                                          AND ENVIO_EMAIL EQ WA_SAIDA_VINC-STATUS_AUX.

    IF ( SY-SUBRC EQ 0 ).
      CONTINUE.
    ENDIF.

    CLEAR: TABIX.
    TABIX = SY-TABIX.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR = '01'
        OBJECT      = 'ZSOL_VINC'
      IMPORTING
        NUMBER      = P_ZID.

    IF ( SY-SUBRC EQ 0 ).

      WA_ZSDT0062_INSR-SOL_VINC       = P_ZID.
      WA_ZSDT0062_INSR-EBELN          = WA_SAIDA_VINC-EBELN.
      WA_ZSDT0062_INSR-EBELP          = WA_SAIDA_VINC-EBELP.
      WA_ZSDT0062_INSR-EBELN_T        = WA_SAIDA_VINC-EBELN_T.
      WA_ZSDT0062_INSR-EBELP_T        = WA_SAIDA_VINC-EBELP_T.
      WA_ZSDT0062_INSR-RESWK          = WA_SAIDA_VINC-RESWK_T.
      "wa_zsdt0062_insr-posnr          = wa_saida_vinc-posnr.
      WA_ZSDT0062_INSR-DT_VINC        = WA_SAIDA_VINC-DATA.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINC-MATNR
        IMPORTING
          OUTPUT = WA_ZSDT0062_INSR-MATNR.

      WA_ZSDT0062_INSR-QTD_VINC       = WA_SAIDA_VINC-QTD_V.

      WA_ZSDT0062_INSR-LOCAL_EMBARQUE = WA_SAIDA_VINC-LOCAL_EMB.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINC-LIFNR
        IMPORTING
          OUTPUT = WA_ZSDT0062_INSR-LIFNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINC-KUNNR
        IMPORTING
          OUTPUT = WA_ZSDT0062_INSR-KUNNR.


      WA_ZSDT0062_INSR-USNAM          = SY-UNAME.
      WA_ZSDT0062_INSR-DT_ATUAL       = SY-DATUM.
      WA_ZSDT0062_INSR-HORA_ATUL      = SY-UZEIT.

      WA_ZSDT0062_INSR-ENVIO_EMAIL    = 'P'.
      WA_ZSDT0062_INSR-STATUS         = 'L'.

      INSERT INTO ZSDT0062 VALUES WA_ZSDT0062_INSR.

      IF ( SY-SUBRC EQ 0 ).

        WA_SAIDA_VINC-STATUS = ICON_MAIL.
        WA_SAIDA_VINC-STATUS_AUX = 'P'.
        WA_SAIDA_VINC-SOL_VINC   = P_ZID.
        MODIFY IT_SAIDA_VINC FROM WA_SAIDA_VINC INDEX TABIX.
        CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.

        CLEAR: WA_SAIDA_MM, WA_SAIDA_TRANSF.

        READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.
        IDX_MM = SY-TABIX.

        IF NOT ( WA_SAIDA_MM-QTD_VINC_MM IS INITIAL ).
          WA_SAIDA_MM-QTD_VINC_MM = WA_SAIDA_MM-QTD_VINC_MM + WA_SAIDA_VINC-QTD_V.
          WA_SAIDA_MM-SALDO_MM    = WA_SAIDA_MM-MENGE - WA_SAIDA_MM-QTD_VINC_MM.
        ELSE.
          WA_SAIDA_MM-QTD_VINC_MM = WA_SAIDA_VINC-QTD_V.
          WA_SAIDA_MM-SALDO_MM    = WA_SALDO_C.
        ENDIF.

        MODIFY IT_SAIDA_MM FROM WA_SAIDA_MM INDEX IDX_MM.
        IF ( SY-SUBRC EQ 0 ).
          CALL METHOD WA_ALV_COMPRAS->REFRESH_TABLE_DISPLAY.
        ENDIF.


        READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.
        IDX_SD = SY-TABIX.

        IF NOT ( WA_SAIDA_TRANSF-QTD_VINC_T IS INITIAL ).
          WA_SAIDA_TRANSF-QTD_VINC_T = WA_SAIDA_TRANSF-QTD_VINC_T + WA_SAIDA_VINC-QTD_V.
          WA_SAIDA_TRANSF-SALDO_T    = WA_SAIDA_TRANSF-MENGE - WA_SAIDA_TRANSF-QTD_VINC_T.
        ELSE.
          WA_SAIDA_TRANSF-QTD_VINC_T = WA_SAIDA_VINC-QTD_V.
          WA_SAIDA_TRANSF-SALDO_T    = WA_SALDO_T.
        ENDIF.
        MODIFY IT_SAIDA_TRANSF FROM WA_SAIDA_TRANSF INDEX IDX_SD.
        IF ( SY-SUBRC EQ 0 ).
          CALL METHOD WA_ALV_TRANSF->REFRESH_TABLE_DISPLAY.
        ENDIF.

        "Inserir os Itens Compra/Transferência
        "Compra

        SELECT SINGLE * FROM ZSDT0065 INTO WA_ZSDT0065_INSR WHERE EBELN EQ WA_SAIDA_MM-EBELN
                                                              AND EBELP EQ WA_SAIDA_MM-EBELP
                                                              AND TIPO  EQ 'C'.

        IF ( SY-SUBRC EQ 0 ).

          READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.

          UPDATE ZSDT0065 SET SOL_VINC = P_ZID
                              QTD_VINC = WA_SAIDA_MM-QTD_VINC_MM

                          WHERE EBELN EQ WA_ZSDT0062_INSR-EBELN
                            AND EBELP EQ WA_ZSDT0062_INSR-EBELP
                            AND TIPO  EQ 'C'.



        ELSE.

          READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.
          WA_ZSDT0065_INSR-SOL_VINC = P_ZID.
          WA_ZSDT0065_INSR-EBELN    = WA_SAIDA_MM-EBELN.
          WA_ZSDT0065_INSR-EBELP    = WA_SAIDA_MM-EBELP.
          WA_ZSDT0065_INSR-QTD_VINC = WA_SAIDA_MM-QTD_VINC_MM.


          WA_ZSDT0065_INSR-TIPO     = 'C'.

          INSERT INTO ZSDT0065 VALUES WA_ZSDT0065_INSR.

          CLEAR: WA_ZSDT0065_INSR.
        ENDIF.


        "Transferência
        SELECT SINGLE * FROM ZSDT0065 INTO WA_ZSDT0065_INSR WHERE EBELN_T EQ WA_SAIDA_TRANSF-EBELN
                                                            AND   EBELP_T EQ WA_SAIDA_TRANSF-EBELP
                                                              AND TIPO  EQ 'T'.
        IF ( SY-SUBRC EQ 0 ).

          READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.

          UPDATE ZSDT0065 SET SOL_VINC = P_ZID
                              QTD_VINC =  WA_SAIDA_TRANSF-QTD_VINC_T
                              WHERE EBELN_T EQ WA_ZSDT0065_INSR-EBELN_T
                                AND EBELP_T EQ WA_ZSDT0065_INSR-EBELP_T
                                AND TIPO    EQ 'T'.


        ELSE.

          READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.
          WA_ZSDT0065_INSR-SOL_VINC = P_ZID.
          WA_ZSDT0065_INSR-EBELN_T    = WA_SAIDA_TRANSF-EBELN.
          WA_ZSDT0065_INSR-EBELP_T    = WA_SAIDA_TRANSF-EBELP.
          WA_ZSDT0065_INSR-QTD_VINC   = WA_SAIDA_TRANSF-QTD_VINC_T.
          WA_ZSDT0065_INSR-TIPO     = 'T'.

          INSERT INTO ZSDT0065 VALUES WA_ZSDT0065_INSR.

          CLEAR: WA_ZSDT0065_INSR.
        ENDIF.
      ENDIF.


    ENDIF.
    CLEAR: WA_SAIDA_VINC, WA_ZSDT0062_INSR, P_ZID.
  ENDLOOP.
  CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.
  MESSAGE S899(MM) WITH 'Vinculação salva.'.

ENDFORM.                    " SALVAR_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VINCULACAO
*&---------------------------------------------------------------------*
FORM SELECIONAR_VINCULACAO .

  DATA: WA_LFA1 TYPE LFA1,
        WA_KNA1 TYPE KNA1,
        WA_VBAK TYPE VBAK.

  REFRESH: IT_ZSDT0062[], IT_SAIDA_VINC[].
  CLEAR: WA_ZSDT0062, WA_SAIDA_MM, WA_SAIDA_TRANSF.

  "Compras
  READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.
  "Transferências
  READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.

  SELECT  SOL_VINC EBELN EBELP EBELN_T EBELP_T RESWK VBELN POSNR DT_VINC MATNR QTD_VINC LOCAL_EMBARQUE USNAM DT_ATUAL HORA_ATUL ENVIO_EMAIL LIFNR KUNNR
    FROM ZSDT0062
    INTO TABLE IT_ZSDT0062
  WHERE EBELN   EQ WA_SAIDA_MM-EBELN
    AND EBELP   EQ WA_SAIDA_MM-EBELP
    AND EBELN_T EQ WA_SAIDA_TRANSF-EBELN
    AND EBELP_T EQ WA_SAIDA_TRANSF-EBELP
    AND STATUS EQ 'L'.

  LOOP AT IT_ZSDT0062 INTO WA_ZSDT0062.

    WA_SAIDA_VINC-SOL_VINC  = WA_ZSDT0062-SOL_VINC.
    WA_SAIDA_VINC-EBELN     = WA_ZSDT0062-EBELN.
    WA_SAIDA_VINC-EBELP     = WA_ZSDT0062-EBELP.
    SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_ZSDT0062-LIFNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZSDT0062-LIFNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINC-LIFNR.
    WA_SAIDA_VINC-NAME      = WA_LFA1-NAME1.

    WA_SAIDA_VINC-EBELN_T     = WA_ZSDT0062-EBELN_T.
    WA_SAIDA_VINC-EBELP_T     = WA_ZSDT0062-EBELP_T.
    SELECT SINGLE * FROM KNA1 INTO WA_KNA1 WHERE KUNNR EQ WA_ZSDT0062-KUNNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZSDT0062-KUNNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINC-KUNNR.

    WA_SAIDA_VINC-NAME1     = WA_KNA1-NAME1.

    WA_SAIDA_VINC-DATA      = WA_ZSDT0062-DT_VINC.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZSDT0062-MATNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINC-MATNR.

    WA_SAIDA_VINC-QTD_V     = WA_ZSDT0062-QTD_VINC.
    WA_SAIDA_VINC-LOCAL_EMB = WA_ZSDT0062-LOCAL_EMBARQUE.

    SELECT SINGLE * FROM VBAK INTO WA_VBAK WHERE VBELN EQ WA_ZSDT0062-VBELN.
    WA_SAIDA_VINC-VKBUR = WA_VBAK-VKBUR.


    CASE WA_ZSDT0062-ENVIO_EMAIL.
      WHEN: 'P'.
        WA_SAIDA_VINC-STATUS     = ICON_MAIL.
        WA_SAIDA_VINC-STATUS_AUX = 'P'.
      WHEN: 'G'.
        WA_SAIDA_VINC-STATUS     = ICON_OKAY.
        WA_SAIDA_VINC-STATUS_AUX = 'G'.
    ENDCASE.

    APPEND WA_SAIDA_VINC TO IT_SAIDA_VINC.
    CLEAR: WA_SAIDA_VINC, WA_ZSDT0062.

  ENDLOOP.
  IF ( WA_ALV_VINC IS INITIAL ).
    PERFORM:   CRIAR_CATALOG_VINCULACAO,
               CRIAR_ALV_VINCULACAO.

    CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.

  ELSE.
    CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    " SELECIONAR_VINCULACAO
**&---------------------------------------------------------------------*
**&      Form  CRIAR_EDITOR
**&---------------------------------------------------------------------*
FORM CRIAR_EDITOR .

  IF ( CONTAINER_EDITOR IS INITIAL ).

    CREATE OBJECT CONTAINER_EDITOR
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_EDITOR'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT CL_EDITOR
      EXPORTING
        PARENT                     = CONTAINER_EDITOR
        WORDWRAP_MODE              = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE
      EXCEPTIONS
        OTHERS                     = 1.

  ENDIF.
  CALL METHOD CL_EDITOR->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE           = LT_TAB
    EXCEPTIONS
      ERROR_DP        = 1
      ERROR_DP_CREATE = 2
      OTHERS          = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " CRIAR_EDITOR
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_VINCULADOS DEFINITION.




ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_VINCULADOS IMPLEMENTATION.




ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  PBO_0600  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0600 OUTPUT.
  IF WA_ZSDT0074-BNAME IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'BTN_ESTORNAR'.
        SCREEN-INPUT     = 0.
        SCREEN-INVISIBLE = 0.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " PBO_0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0600  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0600 INPUT.
  DATA W_ANSWER(1).
  CASE SY-UCOMM.
    WHEN: 'BTN_EMAIL_VINCULADOS'.
      CLEAR: TL_ROWS, SL_ROWS.
      CALL METHOD WA_ALV_VINCULADOS->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TL_ROWS.

      PERFORM: CAIXA_EMAIL.
      CALL SCREEN 0500 STARTING AT 2 1 ENDING AT 95 25.
    WHEN 'BTN_ESTORNAR'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          TEXT_QUESTION         = '“Deseja ESTORNAR ?'
          TEXT_BUTTON_1         = 'Sim'(100)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(101)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
*         USERDEFINED_F1_HELP   = ' '
          START_COLUMN          = 25
          START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = W_ANSWER
*               TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        PERFORM F_ESTORNO.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " PAI_0600  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VINCULADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_VINCULADOS .

  DATA: WA_LFA1 TYPE LFA1,
        WA_KNA1 TYPE KNA1,
        WA_VBAK TYPE VBAK.

  CLEAR: WA_SAIDA_MM, WA_SAIDA_TRANSF, WA_ZSDT0062.
  REFRESH: IT_ZSDT0062[].

  READ TABLE IT_SAIDA_MM INTO WA_SAIDA_MM WITH KEY VINCULAR_C = 'X'.

  IF ( SY-SUBRC EQ 0 ).

    SELECT  SOL_VINC EBELN EBELP EBELN_T EBELP_T RESWK VBELN POSNR DT_VINC MATNR QTD_VINC LOCAL_EMBARQUE USNAM DT_ATUAL HORA_ATUL ENVIO_EMAIL LIFNR KUNNR
      FROM ZSDT0062
      INTO TABLE IT_ZSDT0062
    WHERE EBELN EQ WA_SAIDA_MM-EBELN
      AND EBELP EQ WA_SAIDA_MM-EBELP
      AND STATUS EQ 'L'.

  ENDIF.

  SELECT SINGLE *
    FROM ZSDT0074
    INTO WA_ZSDT0074
    WHERE BNAME = SY-UNAME.

  READ TABLE IT_SAIDA_TRANSF INTO WA_SAIDA_TRANSF WITH KEY VINCULAR_T = 'X'.

  IF ( SY-SUBRC EQ 0 ).

    SELECT  SOL_VINC EBELN EBELP EBELN_T EBELP_T RESWK VBELN POSNR DT_VINC MATNR QTD_VINC LOCAL_EMBARQUE USNAM DT_ATUAL HORA_ATUL ENVIO_EMAIL LIFNR KUNNR
      FROM ZSDT0062
      INTO TABLE IT_ZSDT0062
    WHERE EBELN_T EQ WA_SAIDA_TRANSF-EBELN
      AND EBELP_T EQ WA_SAIDA_TRANSF-EBELP
      AND STATUS EQ 'L'.

  ENDIF.

  LOOP AT IT_ZSDT0062 INTO WA_ZSDT0062.

    WA_SAIDA_VINCULADOS-SOL_VINC  = WA_ZSDT0062-SOL_VINC.
    WA_SAIDA_VINCULADOS-EBELN     = WA_ZSDT0062-EBELN.
    WA_SAIDA_VINCULADOS-EBELP     = WA_ZSDT0062-EBELP.
    SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_ZSDT0062-LIFNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZSDT0062-LIFNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINCULADOS-LIFNR.
    WA_SAIDA_VINCULADOS-NAME      = WA_LFA1-NAME1.

    WA_SAIDA_VINCULADOS-VBELN     = WA_ZSDT0062-VBELN.
    WA_SAIDA_VINCULADOS-POSNR     = WA_ZSDT0062-POSNR.
    WA_SAIDA_VINCULADOS-EBELN_T   = WA_ZSDT0062-EBELN_T.
    WA_SAIDA_VINCULADOS-EBELP_T   = WA_ZSDT0062-EBELP_T.
    WA_SAIDA_VINCULADOS-RESWK_T   = WA_ZSDT0062-RESWK.

*    SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_zsdt0062-lifnr_t.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = wa_zsdt0062-lifnr_t
*      IMPORTING
*        output = wa_saida_vinculados-reswk_t.

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_SAIDA_VINCULADOS-RESWK_T.
    WA_SAIDA_VINCULADOS-NAME_T      = WA_T001W-NAME1.

    SELECT SINGLE * FROM KNA1 INTO WA_KNA1 WHERE KUNNR EQ WA_ZSDT0062-KUNNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZSDT0062-KUNNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINCULADOS-KUNNR.

    WA_SAIDA_VINCULADOS-NAME1     = WA_KNA1-NAME1.

    WA_SAIDA_VINCULADOS-DATA      = WA_ZSDT0062-DT_VINC.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZSDT0062-MATNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINCULADOS-MATNR.

    WA_SAIDA_VINCULADOS-QTD_V     = WA_ZSDT0062-QTD_VINC.
    WA_SAIDA_VINCULADOS-LOCAL_EMB = WA_ZSDT0062-LOCAL_EMBARQUE.

    SELECT SINGLE * FROM VBAK INTO WA_VBAK WHERE VBELN EQ WA_ZSDT0062-VBELN.
    WA_SAIDA_VINCULADOS-VKBUR = WA_VBAK-VKBUR.


    CASE WA_ZSDT0062-ENVIO_EMAIL.
      WHEN: 'P'.
        WA_SAIDA_VINCULADOS-STATUS     = ICON_MAIL.
        WA_SAIDA_VINCULADOS-STATUS_AUX = 'P'.
      WHEN: 'G'.
        WA_SAIDA_VINCULADOS-STATUS     = ICON_OKAY.
        WA_SAIDA_VINCULADOS-STATUS_AUX = 'G'.
    ENDCASE.

    APPEND WA_SAIDA_VINCULADOS TO IT_SAIDA_VINCULADOS.


  ENDLOOP.

ENDFORM.                    " SELECIONAR_VINCULADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VINCULADOS
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_VINCULADOS .

  DATA: WA_LAYOUT_VINCULADOS TYPE LVC_S_LAYO,
        WA_STABLE            TYPE LVC_S_STBL.


  IF WA_CONT_VINCULADOS IS INITIAL.

    CREATE OBJECT WA_CONT_VINCULADOS
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_VINCULADOS'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    PERFORM: CRIAR_FCAT_VINCULADOS.

    CREATE OBJECT WA_ALV_VINCULADOS
      EXPORTING
        I_PARENT          = WA_CONT_VINCULADOS
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

  WA_STABLE-ROW                   = 'X'.
  WA_LAYOUT_VINCULADOS-SEL_MODE   = 'A'.

  CALL METHOD WA_ALV_VINCULADOS->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT_VINCULADOS
      IS_VARIANT                    = GS_VARIANT_C
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = IT_SAIDA_VINCULADOS
      IT_FIELDCATALOG               = IT_FCAT_VINCULADOS
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_VINCULADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_FCAT_VINCULADOS
*&---------------------------------------------------------------------*
FORM CRIAR_FCAT_VINCULADOS .


  PERFORM PREENCHE_FCAT_VINCULADOS USING:

      'STATUS'    'Status'              '5'  ''  ''  ''  '' '' 'C',
      'EBELN'     'Nº.Pedido'           '10' ''  ''  ''  '' '' '',
      'EBELP'     'Item'                '3'  ''  ''  ''  '' '' '',
      'LIFNR'     'Cod.Forn.'           '6'  ''  ''  ''  '' '' '',
      'NAME'      'Desc.Forn.'          '10' ''  ''  ''  '' '' '',
      'KUNNR'     'Cliente'             '6'  ''  ''  ''  '' '' '',
      'NAME1'     'Nome'                '10' ''  ''  ''  '' '' '',
      'VKBUR'     'Escr.Venda'          '10' ''  ''  ''  '' '' '',
      'VBELN'     'Nº.OV.'              '10' ''  ''  ''  '' '' '',
      'POSNR'     'Item'                '3'  ''  ''  ''  '' '' '',
      'EBELN_T'   'Nº.Ped.Transf.'      '10' ''  ''  ''  '' '' '',
      'EBELP_T'   'Item Ped.Transf.'    '3'  ''  ''  ''  '' '' '',
      'RESWK_T'   'Cod.Forn.Transf.'    '6'  ''  ''  ''  '' '' '',
      'NAME_T'    'Desc.Forn.Transf.'   '10' ''  ''  ''  '' '' '',
      'MATNR'     'Material'            '12' ''  ''  ''  '' '' '',
      'QTD_V'     'Qtd.Vinc'            '10' ''  ''  ''  '' '' '',
      'LOCAL_EMB' 'Local de Embarque'   '10' ''  ''  ''  '' '' '',
      'DATA '     'Data.Vinc'           '10' ''  ''  ''  '' '' ''.



ENDFORM.                    " CRIAR_FCAT_VINCULADOS
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_FCAT_VINCULADOS
*&---------------------------------------------------------------------*
FORM PREENCHE_FCAT_VINCULADOS  USING   P_CAMPO TYPE C
                                       P_DESC  TYPE C
                                       P_TAM   TYPE C
                                       P_HOT   TYPE C
                                       P_ZERO  TYPE C
                                       P_SUM   TYPE C
                                       P_COR   TYPE C
                                       P_CHECK TYPE C
                                       P_JUST  TYPE C.

  DATA: WL_FCAT_VINCULADOS TYPE LVC_S_FCAT.

  WL_FCAT_VINCULADOS-TABNAME   = 'IT_SAIDA_VINCULADOS'.
  WL_FCAT_VINCULADOS-FIELDNAME = P_CAMPO.
  WL_FCAT_VINCULADOS-SCRTEXT_L = P_DESC.
  WL_FCAT_VINCULADOS-SCRTEXT_M = P_DESC.
  WL_FCAT_VINCULADOS-SCRTEXT_S = P_DESC.
  WL_FCAT_VINCULADOS-HOTSPOT   = P_HOT.
  WL_FCAT_VINCULADOS-DO_SUM    = P_SUM.
  WL_FCAT_VINCULADOS-OUTPUTLEN = P_TAM.
  WL_FCAT_VINCULADOS-NO_ZERO   = P_ZERO.
  WL_FCAT_VINCULADOS-EMPHASIZE = P_COR.
  WL_FCAT_VINCULADOS-CHECKBOX  = P_CHECK.
  WL_FCAT_VINCULADOS-JUST      = P_JUST.


  APPEND WL_FCAT_VINCULADOS TO IT_FCAT_VINCULADOS.

ENDFORM.                    " PREENCHE_FCAT_VINCULADOS

*&---------------------------------------------------------------------*
*&      Form  CAIXA_EMAIL
*&---------------------------------------------------------------------*
FORM CAIXA_EMAIL .

  DATA: BEGIN OF TL_MES OCCURS 0.
          INCLUDE STRUCTURE T247.
  DATA: END OF TL_MES.

  DATA: TL_LFA1 TYPE LFA1,
        TL_KNA1 TYPE KNA1,
        TL_VBKD TYPE VBKD.

  DATA: QTD_LINHAS TYPE SY-TABIX.

  DATA: MES     TYPE N LENGTH 2,
        TXT_MES TYPE C LENGTH 15.

  DATA: IT_SAIDA_VINCULADOS_AUX TYPE TABLE OF TY_SAIDA_VINC,
        WA_SAIDA_VINCULADOS_AUX TYPE TY_SAIDA_VINC.



  REFRESH: IT_SAIDA_VINCULADOS_AUX[].

  CHECK NOT TL_ROWS[] IS INITIAL.

  TL_ROWS_AUX[] = TL_ROWS[].

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = SY-LANGU
    TABLES
      MONTH_NAMES           = TL_MES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.


  CLEAR: WA_SAIDA_VINC, LA_TAB, LT_TAB.
  READ TABLE TL_ROWS INTO SL_ROWS INDEX 1.

  IF ( VINCULADOS_EMAIL EQ 'X' ).

    IT_SAIDA_VINCULADOS_AUX[] = IT_SAIDA_VINCULADOS[].


    READ TABLE IT_SAIDA_VINCULADOS INTO WA_SAIDA_VINCULADOS INDEX SL_ROWS-INDEX.

    CONCATENATE 'AMAGGI - Liberação de Embarque'  WA_SAIDA_VINCULADOS-VBELN  INTO WA_ASSUNTO SEPARATED BY SPACE.


    MES = SY-DATUM+4(2).
    READ TABLE TL_MES INDEX MES.

    CONCATENATE 'Cuiabá - MT,' SY-DATUM+6(2) 'de' TL_MES-LTX 'de' SY-DATUM(4) INTO LA_TAB SEPARATED BY SPACE.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = 'À'.
    APPEND LA_TAB TO LT_TAB.



    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_SAIDA_VINCULADOS-LIFNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINCULADOS-LIFNR.

    SELECT SINGLE * FROM LFA1 INTO TL_LFA1 WHERE LIFNR EQ WA_SAIDA_VINCULADOS-LIFNR.

    IF ( SY-SUBRC EQ 0 ).
      LA_TAB = TL_LFA1-NAME1.
      APPEND LA_TAB TO LT_TAB.
    ENDIF.


    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.

    LA_TAB = 'Referente ao(s) Pedido(s) de Compra Nro.'.
    APPEND LA_TAB TO LT_TAB.
    CLEAR: LA_TAB.
    DESCRIBE TABLE TL_ROWS LINES QTD_LINHAS.

    LOOP AT TL_ROWS_AUX INTO SL_ROWS_AUX.

      READ TABLE IT_SAIDA_VINCULADOS_AUX INTO WA_SAIDA_VINCULADOS_AUX INDEX SL_ROWS_AUX-INDEX.

      IF ( QTD_LINHAS > 1 ).
        CONCATENATE   WA_SAIDA_VINCULADOS_AUX-EBELN '|' LA_TAB   INTO LA_TAB SEPARATED BY SPACE.
      ELSE.
        LA_TAB = WA_SAIDA_VINCULADOS_AUX-EBELN.
      ENDIF.


*      IF ( QTD_LINHAS > 1 ).
*        CONCATENATE   WA_SAIDA_VINCULADOS-EBELN '|' LA_TAB   INTO LA_TAB SEPARATED BY SPACE.
*      ELSE.
*        LA_TAB = WA_SAIDA_VINCULADOS-EBELN.
*      ENDIF.
    ENDLOOP.
    APPEND LA_TAB TO LT_TAB.


    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.


    LA_TAB = 'Pela presente, autorizamos o embarque do pedido de venda, desmembrando para o nosso cliente conforme abaixo:'.
    APPEND LA_TAB TO LT_TAB.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_SAIDA_VINCULADOS-KUNNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINCULADOS-KUNNR.

    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.

    SELECT SINGLE * FROM KNA1 INTO TL_KNA1 WHERE KUNNR EQ WA_SAIDA_VINCULADOS-KUNNR.

    IF ( SY-SUBRC EQ 0 ).
      LA_TAB = TL_KNA1-NAME1.
      APPEND LA_TAB TO LT_TAB.

      CONCATENATE 'Fazenda:' TL_KNA1-ORT02 INTO LA_TAB SEPARATED BY SPACE.
      APPEND LA_TAB TO LT_TAB.

      IF NOT ( TL_KNA1-STCD1 IS INITIAL ).
        CONCATENATE 'CNPJ/CPF:' TL_KNA1-STCD1 INTO LA_TAB SEPARATED BY SPACE.
      ELSE.
        CONCATENATE 'CNPJ/CPF:' TL_KNA1-STCD2 INTO LA_TAB SEPARATED BY SPACE.

      ENDIF.

      APPEND LA_TAB TO LT_TAB.
      CONCATENATE 'Inscr. Estadual:' TL_KNA1-STCD3 INTO LA_TAB SEPARATED BY SPACE.
      APPEND LA_TAB TO LT_TAB.
      LA_TAB = TL_KNA1-STRAS.
      APPEND LA_TAB TO LT_TAB.
      LA_TAB = TL_KNA1-MCOD3.
      APPEND LA_TAB TO LT_TAB.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINCULADOS-VBELN
        IMPORTING
          OUTPUT = WA_SAIDA_VINCULADOS-VBELN.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINCULADOS-POSNR
        IMPORTING
          OUTPUT = WA_SAIDA_VINCULADOS-POSNR.

      SELECT SINGLE * FROM VBKD INTO TL_VBKD WHERE VBELN EQ WA_SAIDA_VINCULADOS-VBELN
                                               AND POSNR EQ WA_SAIDA_VINCULADOS-POSNR.

      LA_TAB = ''.
      APPEND LA_TAB TO LT_TAB.

      CONCATENATE 'FRETE:' TL_VBKD-INCO1 '-' TL_VBKD-INCO2 INTO LA_TAB SEPARATED BY SPACE.
      APPEND LA_TAB TO LT_TAB.


      LA_TAB = ''.
      APPEND LA_TAB TO LT_TAB.
    ENDIF.

*      LA_TAB = 'Amaggi Commodities'.
*      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'Insumos'.
*      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'Thamara Rodrigues  – (065) 3645-5232'.
*      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'Arianne Queiroz    – (065) 3645-5443'.
*      APPEND LA_TAB TO LT_TAB.
**      LA_TAB = 'Jaqueline          – (065) 3645-5093'.
**      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'Juliana Bortoluzzi – (065) 3645-5439'.
*      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'E-mail: insumos.fertilizantes@amaggi.com.br'.
*      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'Matriz – Cuiabá-MT'.
*      APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'www.amaggi.com.br'.
*      APPEND LA_TAB TO LT_TAB.

  ELSE.

    READ TABLE IT_SAIDA_VINC INTO WA_SAIDA_VINC INDEX SL_ROWS-INDEX.


    CONCATENATE 'AMAGGI - Liberação de Embarque'  WA_SAIDA_VINC-VBELN  INTO WA_ASSUNTO SEPARATED BY SPACE.

    MES = SY-DATUM+4(2).
    READ TABLE TL_MES INDEX MES.


    CONCATENATE 'Cuiabá - MT,' SY-DATUM+6(2) 'de' TL_MES-LTX 'de' SY-DATUM(4) INTO LA_TAB SEPARATED BY SPACE.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = 'À'.
    APPEND LA_TAB TO LT_TAB.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_SAIDA_VINC-LIFNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINC-LIFNR.

    SELECT SINGLE * FROM LFA1 INTO TL_LFA1 WHERE LIFNR EQ WA_SAIDA_VINC-LIFNR.

    IF ( SY-SUBRC EQ 0 ).
      LA_TAB = TL_LFA1-NAME1.
      APPEND LA_TAB TO LT_TAB.
    ENDIF.

    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.

    LA_TAB = 'Referente ao(s) Pedido(s) de Compra Nro.'.
    APPEND LA_TAB TO LT_TAB.
    CLEAR: LA_TAB.
    DESCRIBE TABLE TL_ROWS_AUX LINES QTD_LINHAS.

    LOOP AT TL_ROWS_AUX INTO SL_ROWS_AUX.
      IF ( QTD_LINHAS > 1 ).
        CONCATENATE   WA_SAIDA_VINC-EBELN '|' LA_TAB   INTO LA_TAB SEPARATED BY SPACE.
      ELSE.
        LA_TAB = WA_SAIDA_VINC-EBELN.
      ENDIF.
    ENDLOOP.
    APPEND LA_TAB TO LT_TAB.


    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.
    LA_TAB = 'Pela presente, autorizamos o embarque do pedido de venda, desmembrando para o nosso cliente conforme abaixo:'.
    APPEND LA_TAB TO LT_TAB.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_SAIDA_VINC-KUNNR
      IMPORTING
        OUTPUT = WA_SAIDA_VINC-KUNNR.

    LA_TAB = ''.
    APPEND LA_TAB TO LT_TAB.

    SELECT SINGLE * FROM KNA1 INTO TL_KNA1 WHERE KUNNR EQ WA_SAIDA_VINC-KUNNR.

    IF ( SY-SUBRC EQ 0 ).
      LA_TAB = TL_KNA1-NAME1.
      APPEND LA_TAB TO LT_TAB.

      CONCATENATE 'Fazenda:' TL_KNA1-ORT02 INTO LA_TAB SEPARATED BY SPACE.
      APPEND LA_TAB TO LT_TAB.
      IF NOT ( TL_KNA1-STCD1 IS INITIAL ).
        CONCATENATE 'CNPJ/CPF:' TL_KNA1-STCD1 INTO LA_TAB SEPARATED BY SPACE.
      ELSE.
        CONCATENATE 'CNPJ/CPF:' TL_KNA1-STCD2 INTO LA_TAB SEPARATED BY SPACE.

      ENDIF.
      APPEND LA_TAB TO LT_TAB.

      CONCATENATE 'Inscr. Estadual:' TL_KNA1-STCD3 INTO LA_TAB SEPARATED BY SPACE.
      APPEND LA_TAB TO LT_TAB.
      LA_TAB = TL_KNA1-STRAS.
      APPEND LA_TAB TO LT_TAB.
      LA_TAB = TL_KNA1-MCOD3.
      APPEND LA_TAB TO LT_TAB.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINC-VBELN
        IMPORTING
          OUTPUT = WA_SAIDA_VINC-VBELN.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_SAIDA_VINC-POSNR
        IMPORTING
          OUTPUT = WA_SAIDA_VINC-POSNR.

      SELECT SINGLE * FROM VBKD INTO TL_VBKD WHERE VBELN EQ WA_SAIDA_VINC-VBELN
                                               AND POSNR EQ WA_SAIDA_VINC-POSNR.

      LA_TAB = ''.
      APPEND LA_TAB TO LT_TAB.

      CONCATENATE 'FRETE:' TL_VBKD-INCO1 '-' TL_VBKD-INCO2 INTO LA_TAB SEPARATED BY SPACE.
      APPEND LA_TAB TO LT_TAB.

      LA_TAB = ''.
      APPEND LA_TAB TO LT_TAB.
    ENDIF.
  ENDIF.

  LA_TAB = 'Amaggi Commodities'.
  APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'Insumos'.
  APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'Thamara Rodrigues  – (065) 3645-5232'.
  APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'Arianne Queiroz    – (065) 3645-5443'.
  APPEND LA_TAB TO LT_TAB.
*      LA_TAB = 'Jaqueline          – (065) 3645-5093'.
*      APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'Juliana Bortoluzzi – (065) 3645-5439'.
  APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'E-mail: insumos.fertilizantes@amaggi.com.br'.
  APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'Matriz – Cuiabá-MT'.
  APPEND LA_TAB TO LT_TAB.
  LA_TAB = 'www.amaggi.com.br'.
  APPEND LA_TAB TO LT_TAB.

ENDFORM.                    " CAIXA_EMAIL
*&---------------------------------------------------------------------*
*&      Module  PBO_0500  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0500 OUTPUT.
  SET PF-STATUS 'PF0500'.
  SET TITLEBAR  'TB0500'.
  PERFORM: CRIAR_EDITOR.
ENDMODULE.                 " PBO_0500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0500  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0500 INPUT.

  CASE SY-UCOMM.
    WHEN 'OK'.
      PERFORM: ENVIAR_EMAIL.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " PAI_0500  INPUT
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
FORM ENVIAR_EMAIL .


  DATA: LT_EMAIL TYPE STANDARD TABLE OF CHAR0241,
        LA_EMAIL LIKE LINE OF LT_EMAIL.

  DATA: IT_DESTINATARIO TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE,
        IT_ASSUNTO      TYPE SODOCCHGI1,
        IT_TEXTO        TYPE STANDARD TABLE OF SOLI WITH HEADER LINE.

  DATA: VALOR_STR TYPE STRING,
        QTD_MENGE TYPE BRGEW_15.

  DATA: WA_MAKT  TYPE MAKT,
        WA_IHREZ TYPE EKKO.



  IF ( WA_EMAIL IS INITIAL ).
    MESSAGE W899(MM) DISPLAY LIKE 'E' WITH 'Informar um e-mail'.
  ELSE.


    CALL METHOD CL_EDITOR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE  = LT_EMAIL
      EXCEPTIONS
        OTHERS = 1.

    IF NOT ( LT_EMAIL IS INITIAL ).

      REFRESH: IT_DESTINATARIO[], IT_TEXTO[].
      CLEAR: IT_ASSUNTO.

      IT_DESTINATARIO-REC_TYPE = 'U'.
      IT_DESTINATARIO-RECEIVER = WA_EMAIL.
      APPEND IT_DESTINATARIO.

      IT_ASSUNTO-OBJ_NAME  = 'Liberação de Insumos - Maggi'.
      IT_ASSUNTO-OBJ_LANGU = SY-LANGU.
      IT_ASSUNTO-OBJ_DESCR = WA_ASSUNTO.

      IT_TEXTO = '<!DOCTYPE html>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<html>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<head>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<style type="text/css">'.
      APPEND IT_TEXTO.

      "Tabela do E-mail
      IT_TEXTO = '#tabela table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
      APPEND IT_TEXTO.
      IT_TEXTO = '#tabela th { width: 90px; font-size: 12px; background-color: #93DB70; color: #000000; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
      APPEND IT_TEXTO.
      IT_TEXTO = '#tabela td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
      APPEND IT_TEXTO.
      IT_TEXTO = '#msg { font-size: 14px; font-style: "Tahoma, Geneva, sans-serif"; }'.
      APPEND IT_TEXTO.


      IT_TEXTO = '</style>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '</head>'.

      APPEND IT_TEXTO.
      IT_TEXTO = '<body lang="pt-br">'.
      APPEND IT_TEXTO.

      IT_TEXTO = '<div id="msg">'.
      APPEND IT_TEXTO.

      LOOP AT LT_EMAIL INTO LA_EMAIL.

        CONCATENATE LA_EMAIL '<br/>' INTO IT_TEXTO.
        APPEND IT_TEXTO.
      ENDLOOP.

      IT_TEXTO = '</div>'.
      APPEND IT_TEXTO.

      IT_TEXTO = '<br/>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<br/>'.
      APPEND IT_TEXTO.

      IT_TEXTO = '<div id="tabela">'.
      APPEND IT_TEXTO.

      IT_TEXTO = '<table>'.
      APPEND IT_TEXTO.

      " Cabeçalho
      IT_TEXTO = '<tr>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Ped.Compra</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Item</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Nro.OV</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Item</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Material</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Descrição</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Quantidade</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Local Embarque</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '<th>Pedido de Compra</th>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '</tr>'.
      APPEND IT_TEXTO.

      CLEAR: WA_SAIDA_VINC, WA_SAIDA_VINCULADOS.
      "READ TABLE tl_rows INTO sl_rows INDEX 1.

      IF ( VINCULADOS_EMAIL EQ 'X' ).

        REFRESH: TL_ROWS[].
        CLEAR: SL_ROWS.

        CALL METHOD WA_ALV_VINCULADOS->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TL_ROWS.


        LOOP AT TL_ROWS INTO SL_ROWS.


          READ TABLE IT_SAIDA_VINCULADOS INTO WA_SAIDA_VINCULADOS INDEX SL_ROWS-INDEX.

          IT_TEXTO = '<tr>'.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINCULADOS-EBELN '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINCULADOS-EBELP '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINCULADOS-VBELN '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINCULADOS-POSNR '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINCULADOS-MATNR '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_SAIDA_VINCULADOS-MATNR
            IMPORTING
              OUTPUT = WA_SAIDA_VINCULADOS-MATNR.

          SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE MATNR EQ WA_SAIDA_VINCULADOS-MATNR.

          CONCATENATE '<td>' WA_MAKT-MAKTX '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_SAIDA_VINCULADOS-MATNR
            IMPORTING
              OUTPUT = WA_SAIDA_VINCULADOS-MATNR.

          IT_TEXTO = '<td>'.
          APPEND IT_TEXTO.
          IT_TEXTO = WA_SAIDA_VINCULADOS-QTD_V.
          APPEND IT_TEXTO.
          IT_TEXTO = '</td>'.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINCULADOS-LOCAL_EMB '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          SELECT SINGLE * FROM EKKO INTO WA_IHREZ WHERE EBELN EQ WA_SAIDA_VINCULADOS-EBELN.
          CONCATENATE '<td>' WA_IHREZ-IHREZ '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          IT_TEXTO = '</tr>'.
          APPEND IT_TEXTO.

        ENDLOOP.

      ELSE.

        REFRESH: TL_ROWS[].
        CLEAR: SL_ROWS.

        CALL METHOD WA_ALV_VINC->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TL_ROWS.

        LOOP AT TL_ROWS INTO SL_ROWS.

          READ TABLE IT_SAIDA_VINC INTO WA_SAIDA_VINC INDEX SL_ROWS-INDEX.

          IT_TEXTO = '<tr>'.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINC-EBELN '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINC-EBELP '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINC-VBELN '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINC-POSNR '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINC-MATNR '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_SAIDA_VINC-MATNR
            IMPORTING
              OUTPUT = WA_SAIDA_VINC-MATNR.

          SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE MATNR EQ WA_SAIDA_VINC-MATNR.

          CONCATENATE '<td>' WA_MAKT-MAKTX '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_SAIDA_VINC-MATNR
            IMPORTING
              OUTPUT = WA_SAIDA_VINC-MATNR.

          IT_TEXTO = '<td>'.
          APPEND IT_TEXTO.
          IT_TEXTO = WA_SAIDA_VINC-QTD_V.
          APPEND IT_TEXTO.
          IT_TEXTO = '</td>'.
          APPEND IT_TEXTO.
          CONCATENATE '<td>' WA_SAIDA_VINC-LOCAL_EMB '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          SELECT SINGLE * FROM EKKO INTO WA_IHREZ WHERE EBELN EQ WA_SAIDA_VINC-EBELN.
          CONCATENATE '<td>' WA_IHREZ-IHREZ '</td>' INTO IT_TEXTO.
          APPEND IT_TEXTO.

          IT_TEXTO = '</tr>'.
          APPEND IT_TEXTO.



        ENDLOOP.

      ENDIF.

      IT_TEXTO = '</table>'.
      APPEND IT_TEXTO.
      IT_TEXTO = '</body>'.
      APPEND IT_TEXTO.

      IT_TEXTO = '</html>'.
      APPEND IT_TEXTO.

      "Enviar E-mail
      CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
        EXPORTING
          DOCUMENT_DATA              = IT_ASSUNTO
          DOCUMENT_TYPE              = 'HTM'
        TABLES
          OBJECT_CONTENT             = IT_TEXTO
          RECEIVERS                  = IT_DESTINATARIO
        EXCEPTIONS
          TOO_MANY_RECEIVERS         = 1
          DOCUMENT_NOT_SENT          = 2
          DOCUMENT_TYPE_NOT_EXIST    = 3
          OPERATION_NO_AUTHORIZATION = 4
          PARAMETER_ERROR            = 5
          X_ERROR                    = 6
          ENQUEUE_ERROR              = 7
          OTHERS                     = 8.

      IF ( SY-SUBRC EQ 0 ).
        COMMIT WORK.
        SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.


        IF ( VINCULADOS_EMAIL EQ 'X' ).


          LOOP AT TL_ROWS INTO SL_ROWS.

            READ TABLE IT_SAIDA_VINCULADOS INTO WA_SAIDA_VINCULADOS INDEX SL_ROWS-INDEX.
            UPDATE ZSDT0062 SET ENVIO_EMAIL = 'G'
                            EMAIL           = WA_EMAIL
                            USERMAIL        = SY-UNAME
                            DT_EMAIL        = SY-DATUM
                            HR_EMAIL        = SY-UZEIT
                            WHERE EBELN       EQ WA_SAIDA_VINCULADOS-EBELN
                              AND EBELP       EQ WA_SAIDA_VINCULADOS-EBELP
                              AND VBELN       EQ WA_SAIDA_VINCULADOS-VBELN
                              AND POSNR       EQ WA_SAIDA_VINCULADOS-POSNR
                              AND ENVIO_EMAIL EQ 'P'
                              AND SOL_VINC    EQ WA_SAIDA_VINCULADOS-SOL_VINC.

            WA_SAIDA_VINCULADOS-STATUS = ICON_OKAY.
            MODIFY IT_SAIDA_VINCULADOS FROM WA_SAIDA_VINCULADOS INDEX SL_ROWS-INDEX.

          ENDLOOP.

          CALL METHOD WA_ALV_VINCULADOS->REFRESH_TABLE_DISPLAY.

          MESSAGE S899(MM) WITH 'E-mail enviado com sucesso.'.
        ELSE.

          LOOP AT TL_ROWS INTO SL_ROWS.

            READ TABLE IT_SAIDA_VINC INTO WA_SAIDA_VINC INDEX SL_ROWS-INDEX.
            UPDATE ZSDT0062 SET ENVIO_EMAIL = 'G'
                            EMAIL           = WA_EMAIL
                            USERMAIL        = SY-UNAME
                            DT_EMAIL        = SY-DATUM
                            HR_EMAIL        = SY-UZEIT
                            WHERE EBELN       EQ WA_SAIDA_VINC-EBELN
                              AND EBELP       EQ WA_SAIDA_VINC-EBELP
                              AND VBELN       EQ WA_SAIDA_VINC-VBELN
                              AND POSNR       EQ WA_SAIDA_VINC-POSNR
                              AND ENVIO_EMAIL EQ 'P'
                              AND SOL_VINC    EQ WA_SAIDA_VINC-SOL_VINC.

            WA_SAIDA_VINC-STATUS = ICON_OKAY.
            MODIFY IT_SAIDA_VINC FROM WA_SAIDA_VINC INDEX SL_ROWS-INDEX.

          ENDLOOP.

          CALL METHOD WA_ALV_VINC->REFRESH_TABLE_DISPLAY.
          MESSAGE S899(MM) WITH 'E-mail enviado com sucesso.'.

        ENDIF.

      ELSE.
        MESSAGE W899(MM) WITH 'E-mail não enviado'.
      ENDIF.
    ELSE.
      MESSAGE W899(MM) WITH 'Informar um texto' 'no corpo do e-mail'.
    ENDIF.
  ENDIF.


ENDFORM.                    " ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ESTORNO .
  CLEAR: TL_ROWS, SL_ROWS.
  DATA: QTD_LINHAS TYPE SY-TABIX.

  DATA: WL_ZSDT0062 TYPE ZSDT0062,
        V_MATNR     TYPE VBAP-MATNR. "Material

  CALL METHOD WA_ALV_VINCULADOS->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = TL_ROWS.
  DESCRIBE TABLE TL_ROWS LINES QTD_LINHAS.
  IF  QTD_LINHAS NE 1.
    MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Selecionar apenas uma linha.'.
    EXIT.
  ENDIF.

  LOOP AT TL_ROWS INTO SL_ROWS.

    READ TABLE IT_SAIDA_VINCULADOS INTO WA_SAIDA_VINCULADOS INDEX SL_ROWS-INDEX.
    SELECT SINGLE * FROM ZSDT0062 INTO WL_ZSDT0062 WHERE  SOL_VINC = WA_SAIDA_VINCULADOS-SOL_VINC.
    IF WL_ZSDT0062-STATUS = 'E'.
      MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Lançamento já estornado.'.
      EXIT.
    ENDIF.

    UPDATE ZSDT0065 SET QTD_VINC = QTD_VINC - WL_ZSDT0062-QTD_VINC
    WHERE EBELN = WL_ZSDT0062-EBELN
    AND   EBELP = WL_ZSDT0062-EBELP
    AND   TIPO = 'C'.

    UPDATE ZSDT0065 SET QTD_VINC = QTD_VINC - WL_ZSDT0062-QTD_VINC
    WHERE VBELN = WL_ZSDT0062-VBELN
    AND   POSNR = WL_ZSDT0062-POSNR
    AND   TIPO = 'V'.

    UPDATE ZSDT0062 SET STATUS    = 'E'
                        USNAM     = SY-UNAME
                        DT_ATUAL  = SY-DATUM
                        HORA_ATUL = SY-UZEIT
    WHERE  SOL_VINC = WA_SAIDA_VINCULADOS-SOL_VINC.

    REFRESH: IT_SAIDA_MM, IT_SAIDA_SD.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_SAIDA_VINCULADOS-MATNR
      IMPORTING
        OUTPUT = V_MATNR.

    PERFORM COMPRAS USING  V_MATNR.
    PERFORM VENDAS  USING  V_MATNR.
    DELETE IT_SAIDA_VINCULADOS INDEX SL_ROWS-INDEX.

  ENDLOOP.


  CALL METHOD WA_ALV_VINCULADOS->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  CALL METHOD WA_ALV_COMPRAS->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  CALL METHOD WA_ALV_VENDAS->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

ENDFORM.                    " F_ESTORNO

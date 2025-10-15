*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Report  ZSDR0005                                                     *
* Descrição  : Relatorio Consulta de Entregas - INSUMOS                *
* Módulo     : MM                               Transação: ZLES0050    *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                            Data: 21/03/2011*
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT zsdr0005.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  ekko,           " Cabeçalho do documento de compra
  ekbe,           " Histórico para o documento de compra
  ekpo,           " Item do documento de compras
  rbkp,           " Cabeçalho doc.da fatura recebida
  lfa1,           " Mestre de fornecedores (parte geral)
  j_1bnflin,      " Partidas individuais da nota fiscal
  vbak,           " Documento de vendas: dados de cabeçalho
  vbap,           " Documento de vendas: dados de item
  mara,
  vbfa,           " Fluxo de documentos de vendas e distribuição
  kna1,           " Mestre de clientes (parte geral)
  j_1bnfe_active, " Electronic Nota Fiscal: Actual Status
  zsdt0041.       " Simulador de Vendas - dados de itens

*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_ekko,        " Cabeçalho do documento de compra
    ebeln TYPE ekko-ebeln, " Nº do documento de compras
    bsart TYPE ekko-bsart, " Tipo de documento de compras
    lifnr TYPE ekko-lifnr, " Nº conta do fornecedor
    bukrs TYPE ekko-bukrs, " Empresa
    reswk TYPE ekko-reswk, " Centro de saída estoque em caso de pedido transferência
    bedat TYPE ekko-bedat,
    knumv TYPE ekko-knumv,
    zterm TYPE ekko-zterm,
  END OF ty_ekko,

  BEGIN OF ty_ekbe,        " Histórico para o documento de compra
    ebeln TYPE ekbe-ebeln, " Nº do documento de compras
    ebelp TYPE ekbe-ebelp, " Nº item do documento de compra
    belnr TYPE ekbe-belnr, " Nº documento de material
    budat TYPE ekbe-budat, " Data de lançamento no documento
    menge TYPE ekbe-menge, " Quantidade
    dmbtr TYPE ekbe-dmbtr, " Montante em moeda interna
    shkzg TYPE ekbe-shkzg, " Código débito/crédito
    gjahr TYPE ekbe-gjahr,
  END OF ty_ekbe,

  BEGIN OF ty_ekbe2,       " Histórico para o documento de compra
    ebeln TYPE ekbe-ebeln, " Nº do documento de compras
    ebelp TYPE ekbe-ebelp, " Nº item do documento de compra
    belnr TYPE ekbe-belnr, " Nº documento de material
    budat TYPE ekbe-budat, " Data de lançamento no documento
    menge TYPE ekbe-menge, " Quantidade
    dmbtr TYPE ekbe-dmbtr, " Montante em moeda interna
    shkzg TYPE ekbe-shkzg, " Código débito/crédito
    gjahr TYPE ekbe-gjahr,
  END OF ty_ekbe2,

  BEGIN OF ty_ekpo,        " Item do documento de compras
    ebeln TYPE ekpo-ebeln, " Nº do documento de compras
    ebelp TYPE ekpo-ebelp, " Nº item do documento de compra
    matnr TYPE ekpo-matnr, " Nº do material
    txz01 TYPE ekpo-txz01, " Texto breve
    menge TYPE ekpo-menge, " Quantidade do pedido
    meins TYPE ekpo-meins,
    loekz TYPE ekpo-loekz, " Código de eliminação no documento de compras
    bprme TYPE ekpo-bprme,
  END OF ty_ekpo,

  BEGIN OF ty_rbkp,        " Cabeçalho doc.da fatura recebida
    belnr TYPE rbkp-belnr, " Nº documento de material
    gjahr TYPE rbkp-gjahr, " Exercício
    xblnr TYPE rbkp-xblnr, " Nº documento de referência
    bldat TYPE rbkp-bldat, " Data de Emissão da Fatura
    rmwwr TYPE rbkp-rmwwr, " Valor da Nota
    waers TYPE rbkp-waers, " Tipo de moeda
  END OF ty_rbkp,

  BEGIN OF ty_lfa1,         " Mestre de fornecedores (parte geral)
    name1 TYPE lfa1-name1,                                    " Nome1
    lifnr TYPE lfa1-lifnr,  " Nº conta do fornecedor
  END OF  ty_lfa1,

  BEGIN OF ty_ekbe_aux,               " Histórico para o documento de compra
    bel_refkey TYPE j_1bnflin-refkey, " Referência ao documento de origem
  END OF ty_ekbe_aux,

  BEGIN OF ty_j_1bnflin,            " Partidas individuais da nota fiscal
    refkey   TYPE j_1bnflin-refkey, " Referência ao documento de origem
    docnum   TYPE j_1bnflin-docnum, " Nº documento
    mengelin TYPE j_1bnflin-menge,  " Quantidade
    meins    TYPE j_1bnflin-meins,  " Unidade de medida básica
    netwr    TYPE j_1bnflin-netwr,  " Valor líquido
    menge    TYPE j_1bnflin-menge, " Quantidade
    itmnum   TYPE j_1bnflin-itmnum, " Item
  END OF  ty_j_1bnflin,

  BEGIN OF ty_j_1bnfdoc,           " Partidas individuais da nota fiscal
    docnum TYPE j_1bnfdoc-docnum,  " Nº documento
    docdat TYPE j_1bnfdoc-docdat,  " Quantidade
    nfnum  TYPE j_1bnfdoc-nfnum,   " Cabeçalho da nota fiscal
    nfe    TYPE j_1bnfdoc-nfe,     " Nota Fiscal eletrônica
    nfenum TYPE j_1bnfdoc-nfenum,  " Nº NF-e de nove posições
  END OF  ty_j_1bnfdoc,

  BEGIN OF ty_tvzbt,
    spras TYPE tvzbt-spras,
    zterm TYPE tvzbt-zterm,
    vtext TYPE tvzbt-vtext,
  END OF ty_tvzbt,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    wrkst TYPE mara-wrkst,
    matkl TYPE mara-matkl,
  END OF ty_mara,

  BEGIN OF ty_saida,              " Saida
    name1        TYPE lfa1-name1,                                    " Nome 1
    ebeln        TYPE ekko-ebeln,        " Pedido
    ebelp        TYPE ekpo-ebelp,        " Item
    matnr        TYPE ekpo-matnr,        " Nº do material
    menge        TYPE db20199vp,        " Quantidade do pedido
    qtefaturado  TYPE ekpo-menge, " XTOT_FATURAMM (Habilitar link “Relatório 2”)
    saldo        TYPE db20199vp, " (Qte.Pedido – Qte.Faturado)
    descmat(100) TYPE c,
    qtd          TYPE db20199vp,
    unid         TYPE ekpo-meins,
    s_pedido(20) TYPE c,
    txz01        TYPE ekpo-txz01,
    waers        TYPE konv-waers,
    kbetr        TYPE konv-kbetr,
  END OF ty_saida,

  BEGIN OF ty_saida_sd,            " Saida
    name1          TYPE kna1-name1,        " Cliente 35
    vkbur          TYPE vbak-vkbur,         " Escr.Vendas 4
    vbeln          TYPE vbak-vbeln,         " Contrato 10
    matnr          TYPE vbap-matnr,         " Material 18
    descmat(60)    TYPE c,            " Descricao Material
    wrkst          TYPE mara-wrkst,
    status,
    zterm(30),
    vtext          TYPE tvzbt-vtext,  " Denominação da condição de pagamento
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 101103 - Ajuste_Relatório_ZSDT0027_ZSDT0051
* inserindo: VBKD-INCO1 / VBKD-INCO2
    inco1          TYPE vbkd-inco1,
    inco2          TYPE vbkd-inco2,
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END
    ntgew          TYPE vbap-ntgew,         " Qte.Contrato 15
*  qtefaturado TYPE vbap-zmeng,  " Quantidade Faturado
    qtefaturado    TYPE db20199vp,  " Quantidade Faturado 10
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 >>> INI
* CH 100102 - Ajuste_Relatório_ZSDT0027_ZSDT0051
    vlr_qtecont    TYPE konv-kbetr,   " Valor Qte. Contratada
    vlr_qtefat     TYPE konv-kbetr,   " Valor Qte. Faturada
    vlr_saldo      TYPE konv-kbetr,   " Valor Saldo
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 <<< END

    "saldo       TYPE vbap-zmeng, " Saldo
    saldo          TYPE db20199vp,                               " Saldo '10
    posnr          TYPE vbap-posnr,                              " Item. 6
    lifsp	         TYPE lifsp_ep,
    kwmeng         TYPE vbap-kwmeng, " Quantidade da ordem acumulada em unidade de venda 15
    qtd            TYPE db20199vp, " 10
    unid           TYPE vbap-vrkme, "3
    arktx          TYPE vbap-arktx,
    waers          TYPE konv-waers,
    kbetr          TYPE konv-kbetr,
    valdt          TYPE vbkd-valdt,
    kurrf          TYPE vbkd-kurrf,
    erdat          TYPE vbak-erdat,
    safra          TYPE ajahr,
    doc_simulacao  TYPE zsdt0041-doc_simulacao,
    auart          TYPE vbak-auart,
    cultura        TYPE acc_txtlg,
    safra_apl      TYPE ajahr,
    cultura_apl    TYPE acc_txtlg,
    regional       TYPE zsdt0270-regional,
    bezei          TYPE tvkbt-bezei,
    kbetr2         TYPE konv-kbetr,
    kbetr_tot      TYPE komp-netwr,
    qte_sol        TYPE db20199vp,  " Quantidade Faturado 10
    vlr_qtecon     TYPE konv-kbetr,
*    cod_regional   TYPE zcod_regional,
    augdt          TYPE bsad-augdt,      "Dt. Compensação
    nrpropopr(60)  TYPE c, "Nº Proposta Operação
    alcpropopr(60) TYPE c, "Alçada Prop. Operação
    efetivada(60)  TYPE c, "Efetivada
    nrproplmt(60)  TYPE c, "Nº Proposta Limite
    alcproplmt(60) TYPE c, "Alçada Prop. Limite
    j_1bcfop       TYPE vbap-j_1bcfop,
    j_1btxsdc      TYPE vbap-j_1btxsdc,
    werks          TYPE vbap-werks,  "RJF
    lgort          TYPE vbap-lgort,  "RJF
    vlr_frete      TYPE zsdt0041-vlr_frete,
    lifsp_t        TYPE tvlst-vtext, "Desc. Bloqueio
    matkl          TYPE mara-matkl,   "GR.Merc
    vlr_totbrt     TYPE konv-kbetr,     "Vlr Tot bruto
    vlr_totbrt_fat TYPE konv-kbetr,     "Vlr Tot br Fat
  END OF ty_saida_sd,

  BEGIN OF ty_saida_mm2,             " Saida
    name12  TYPE  lfa1-name1,      " Nº conta do fornecedo
    ebeln2  TYPE ekko-ebeln,       " Nº do documento de compras
    belnr2  TYPE ekbe-belnr ,      " Nº documento de material
    docnum2 TYPE j_1bnflin-docnum, " Nº documento
    nfenum2 TYPE j_1bnfdoc-nfenum, " Nº documento de referência
    budat   TYPE ekbe-budat, " Data de criação do registro
    menge   TYPE ekbe-menge,  " Quantidade referenciada em unidade medida básica
    dmbtr   TYPE ekbe-dmbtr,  " Valor de referência
    qtd     TYPE db20199vp,
    meins   TYPE ekpo-meins,
  END OF ty_saida_mm2,

  BEGIN OF ty_saida_sd2,                " Saida
    name12   TYPE kna1-name1,            " Cliente
    vbeln2   TYPE vbak-vbeln,            " Contrato
    vbeln3   TYPE vbak-vbeln,            " Contrato
    vbelnk   TYPE vbfa-vbeln,            " Doc.Fatura
    docnum2  TYPE j_1bnflin-docnum,      " Doc.Fiscal
    nfenum   TYPE j_1bnfdoc-nfenum,       " Nro.Nf.
    erdat2   TYPE vbfa-erdat,            " Dt.Emissão
    qtdfatur TYPE db20199vp,
    rfwrt2   TYPE vbfa-rfwrt,            " Valor Nota
    vrkme    TYPE vbap-vrkme,
  END OF ty_saida_sd2,

  BEGIN OF ty_vbak,        " Documento de vendas: dados de cabeçalho
    vbeln TYPE vbak-vbeln, " Documento de vendas
    vkorg TYPE vbak-vkorg, " Organização de vendas
    vtweg TYPE vbak-vtweg, " Canal de distribuição
    spart TYPE vbak-spart, " Setor de atividade
    auart TYPE vbak-auart, " Tipo de documento de vendas - Tipo de Contrato
    vkbur TYPE vbak-vkbur, " Escritório de vendas
    kunnr TYPE vbak-kunnr, " Emissor da ordem  - Cliente
    audat TYPE vbak-audat, " Data de Entrada.
    knumv TYPE vbak-knumv, " Nº condição do documento
    vgbel TYPE vbak-vgbel,
    erdat TYPE vbak-erdat, " Data de criação
    lifsk TYPE vbak-lifsk,
    faksk TYPE vbak-faksk,
  END OF ty_vbak,

  BEGIN OF ty_vbap,        " Documento de vendas: dados de item
    vbeln     TYPE vbap-vbeln, " Documento de vendas
    matnr     TYPE vbap-matnr, " Nº do material - (Material)
    arktx     TYPE vbap-arktx, " Texto breve do item da ordem do cliente -(Descrição do Material)
    werks     TYPE vbap-werks, " Centro (próprio ou externo)
    ntgew     TYPE vbap-ntgew, " Peso líquido do item
    gewei     TYPE vbap-gewei, " UM qtd.prevista
    posnr     TYPE vbap-posnr, " Item do documento de vendas
    kwmeng    TYPE vbap-kwmeng, " Quantidade da ordem acumulada em unidade de venda
    vrkme     TYPE vbap-vrkme,
    j_1bcfop  TYPE vbap-j_1bcfop, "CFOP
    j_1btxsdc TYPE vbap-j_1btxsdc, "Cód Imposto
    lgort     TYPE vbap-lgort,
    netwr     TYPE vbap-netwr,
    mwsbp     TYPE vbap-mwsbp,
  END OF ty_vbap,

  BEGIN OF ty_vbep,
    vbeln TYPE vbep-vbeln,
    posnr TYPE vbep-posnr,
    etenr TYPE vbep-etenr,
    lifsp TYPE vbep-lifsp,
  END OF ty_vbep,

  BEGIN OF ty_vbfa,            " Fluxo de documentos de vendas e distribuição
    vbelv   TYPE vbfa-vbelv,   " Documento de vendas e distribuição precedente
    vbeln   TYPE vbfa-vbeln,   " Documento de vendas e distribuição subseqüente
    erdat   TYPE vbfa-erdat,   " Data de criação do registro
    rfmng   TYPE vbfa-rfmng,   " Quantidade referenciada em unidade medida básica
    rfwrt   TYPE vbfa-rfwrt,   " Valor de referência
    vbtyp_n TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
    vbtyp_v TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
    posnn   TYPE vbfa-posnn,
    posnv   TYPE vbfa-posnv,
  END OF ty_vbfa,

  BEGIN OF ty_vbfa_aux,                 " Fluxo de documentos de vendas e distribuição
    vg_refkey TYPE j_1bnflin-refkey,   " Referência ao documento de origem
  END OF ty_vbfa_aux,

  BEGIN OF ty_vbfa_mm,            " Fluxo de documentos de vendas e distribuição
    vbeln_mm   TYPE vbfa-vbeln,   " Documento de vendas e distribuição subseqüente
    vbelv_mm   TYPE vbfa-vbelv,   " Documento de vendas e distribuição precedente
    erdat_mm   TYPE vbfa-erdat,   " Data de criação do registro
    rfmng_mm   TYPE vbfa-rfmng,   " Quantidade referenciada em unidade medida básica
    rfwrt_mm   TYPE vbfa-rfwrt,   " Valor de referência
    vbtyp_n_mm TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
    vbtyp_v_mm TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
  END OF ty_vbfa_mm,

  BEGIN OF ty_kna1,        " Mestre de clientes (parte geral)
    name1 TYPE kna1-name1,
    kunnr TYPE kna1-kunnr,
    stcd1 TYPE kna1-stcd1,
    stcd2 TYPE kna1-stcd2,
  END OF ty_kna1,

  BEGIN OF ty_j_1bnfe_active,          " Electronic Nota Fiscal: Actual Status
    docnum TYPE j_1bnfe_active-docnum, " Nº documento
    nfnum9 TYPE j_1bnfe_active-nfnum9, " Nº NF-e de nove posições
  END OF ty_j_1bnfe_active,

  BEGIN OF ty_vbfa_tot,           " Fluxo de documentos de vendas e distribuição
    vbelnrfmg      TYPE vbfa-vbelv, " Documento de vendas e distribuição precedente
    "  totalrfmng TYPE vbfa-rfmng, " Documento de vendas e distribuição subseqüente
    totalmenge     TYPE j_1bnflin-menge, " Documento de vendas e distribuição subseqüente
    posnn          TYPE vbfa-posnn,
    totaldim       TYPE j_1bnflin-menge, "Total de Recusa e Complemento
    vlr_totbrt_fat TYPE vbrk-netwr,
  END OF ty_vbfa_tot,

  BEGIN OF ty_konv,
    knumv TYPE konv-knumv, " Nº condição do documento
    kposn TYPE konv-kposn, " Nº item ao qual se aplicam as condições
    kschl TYPE konv-kschl, " Tipo de condição
    waers TYPE konv-waers, " Código da moeda
    kbetr TYPE konv-kbetr,
    kmein TYPE konv-kmein,
    kwert TYPE konv-kwert,
  END OF ty_konv,


  BEGIN OF ty_zib_bsik,
    bukrs TYPE bsik-bukrs,
    belnr TYPE bsik-belnr,
    gjahr TYPE bsik-gjahr,
  END OF ty_zib_bsik,

  BEGIN OF ty_zib_bsid,
    bukrs TYPE bsid-bukrs,
    belnr TYPE bsid-belnr,
    gjahr TYPE bsid-gjahr,
  END OF ty_zib_bsid,

  BEGIN OF ty_vbkd,
    vbeln TYPE vbkd-vbeln,
    valdt TYPE vbkd-valdt,
  END OF ty_vbkd,

  BEGIN OF ty_vbrk,
    vbeln TYPE vbrk-vbeln,
    fkart TYPE vbrk-fkart,
    sfakn TYPE vbrk-sfakn,
    netwr TYPE vbrk-netwr,
    mwsbk TYPE vbrk-mwsbk,
  END OF ty_vbrk,

  BEGIN OF ty_zsdt0040,
    doc_simulacao TYPE zsded003,
    cultura       TYPE zsded001,
    safra         TYPE ajahr,
  END OF ty_zsdt0040,

  BEGIN OF ty_zsdt0041,
    doc_simulacao TYPE zsded003,
    vbeln         TYPE vbeln,
    cultura_apl   TYPE zde_cultura_apl,
    safra_apl     TYPE zde_safra_apl,
  END OF ty_zsdt0041.


*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: t_bdc             TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      it_saida          TYPE TABLE OF ty_saida,
      it_ekko           TYPE TABLE OF ty_ekko,
      it_ekbe           TYPE TABLE OF ty_ekbe,
      it_ekbe2          TYPE TABLE OF ty_ekbe2,
      it_ekpo           TYPE TABLE OF ty_ekpo,
      it_rbkp           TYPE TABLE OF ty_rbkp,
      it_lfa1           TYPE TABLE OF ty_lfa1,
      it_ekbe_aux       TYPE TABLE OF ty_ekbe_aux,
      it_j_1bnflin      TYPE TABLE OF ty_j_1bnflin,
      it_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc,
      it_vbak           TYPE TABLE OF ty_vbak,
      it_vbak_est       TYPE TABLE OF ty_vbak,
      it_vbap           TYPE TABLE OF ty_vbap WITH HEADER LINE,
      it_vbap_aux       TYPE TABLE OF ty_vbap WITH HEADER LINE,
      it_vbep           TYPE TABLE OF ty_vbep    WITH HEADER LINE,
      it_vbep2          TYPE TABLE OF vbep       WITH HEADER LINE,
      it_vbfa           TYPE TABLE OF ty_vbfa,
      it_vbfa_est       TYPE TABLE OF ty_vbfa,
      it_vbrk           TYPE TABLE OF ty_vbrk,
      it_vbrk_aux       TYPE TABLE OF ty_vbrk,
      it_vbrk_est       TYPE TABLE OF ty_vbrk,
      it_tvzbt          TYPE TABLE OF ty_tvzbt,
      it_mara           TYPE TABLE OF ty_mara,
      it_0026           TYPE TABLE OF zfit0026 WITH HEADER LINE, "
      it_zib            TYPE TABLE OF zib_contabil_chv WITH HEADER LINE, "
      it_bsik           TYPE TABLE OF bsik WITH HEADER LINE, "
      it_bsid           TYPE TABLE OF bsid WITH HEADER LINE, "
      it_bsad           TYPE TABLE OF bsad WITH HEADER LINE,
      it_vbkd_aux       TYPE TABLE OF vbkd WITH HEADER LINE, "
      it_t052u          TYPE TABLE OF t052u WITH HEADER LINE, "
      it_zib_bsik       TYPE TABLE OF ty_zib_bsik WITH HEADER LINE, "
      it_zib_bsid       TYPE TABLE OF ty_zib_bsid WITH HEADER LINE, "
      it_vbfa_mm        TYPE TABLE OF ty_vbfa_mm,
      it_kna1           TYPE TABLE OF ty_kna1,
      it_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active,
      it_vbfa_tot       TYPE TABLE OF ty_vbfa_tot,
      it_vbfa_aux       TYPE TABLE OF ty_vbfa_aux,
      it_vbfa_aux2      TYPE TABLE OF ty_vbfa WITH HEADER LINE,
      it_vbfa_tot2      TYPE TABLE OF ty_vbfa_tot WITH HEADER LINE,
      it_saida_sd       TYPE TABLE OF ty_saida_sd,
      it_saida_mm2      TYPE TABLE OF ty_saida_mm2,
      it_konv           TYPE TABLE OF ty_konv,
      it_vbkd           TYPE TABLE OF ty_vbkd,
      it_saida_sd2      TYPE TABLE OF ty_saida_sd2,
      it_zsdt0040       TYPE TABLE OF ty_zsdt0040 WITH HEADER LINE,
      it_zsdt0041       TYPE TABLE OF ty_zsdt0041 WITH HEADER LINE,
      it_zsdt0038       TYPE TABLE OF zsdt0038 WITH HEADER LINE,
      it_zsdt0271       TYPE STANDARD TABLE OF zsdt0271 WITH HEADER LINE,
      it_zsdt0270       TYPE STANDARD TABLE OF zsdt0270 WITH HEADER LINE,
      it_zsdt0082       TYPE TABLE OF zsdt0082 WITH HEADER LINE,
      it_zsdt0272       TYPE STANDARD TABLE OF zsdt0272 WITH HEADER LINE,
      it_zsdt0273       TYPE STANDARD TABLE OF zsdt0273 WITH HEADER LINE,
      it_zsdt0274       TYPE TABLE OF zsdt0274 WITH HEADER LINE,
      it_zsdt0275       TYPE TABLE OF zsdt0275 WITH HEADER LINE,
      vl_cpf            TYPE kna1-stcd1,
      it_zsdt0041_frete TYPE STANDARD TABLE OF zsdt0041,
      it_zsdt0090_frete TYPE STANDARD TABLE OF zsdt0090,
      wa_zsdt0041_frete TYPE zsdt0041,
      wa_zsdt0090_frete TYPE zsdt0090,
      it_tvbur          TYPE TABLE OF tvbur WITH HEADER LINE,
      it_tvkbt          TYPE TABLE OF tvkbt WITH HEADER LINE,
      it_tvls           TYPE TABLE OF tvls WITH HEADER LINE,
      it_tvlst          TYPE TABLE OF tvlst WITH HEADER LINE,
      it_tvfs           TYPE TABLE OF tvfs WITH HEADER LINE,
      it_tvfst          TYPE TABLE OF tvfst WITH HEADER LINE,
      it_zsdt0090_safra TYPE TABLE OF zsdt0090 WITH HEADER LINE,
      it_zsdt0041_safra TYPE TABLE OF zsdt0041 WITH HEADER LINE,
      it_konv_imp       TYPE TABLE OF ty_konv.

RANGES:  rg_cpf_cnpj FOR  kna1-stcd1.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_cont           TYPE REF TO cl_gui_custom_container        , " Objeto Container
      wa_cont2          TYPE REF TO cl_gui_custom_container        , " Objeto Container
      wa_alv            TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
      wa_alv2           TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
      wa_layout         TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
      wa_layout2        TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
      wa_saida          TYPE  ty_saida,
      wa_ekko           TYPE ty_ekko,
      wa_ekbe           TYPE ty_ekbe,
      wa_kna1           TYPE ty_kna1,
      wa_zsdt0041_safra TYPE zsdt0041,
      wa_zsdt0090_safra TYPE zsdt0090,
      wa_vbep           TYPE ty_vbep,
      wa_ekbe2          TYPE ty_ekbe2,
      wa_ekpo           TYPE ty_ekpo,
      wa_rbkp           TYPE ty_rbkp,
      wa_lfa1           TYPE ty_lfa1,
      wa_ekbe_aux       TYPE ty_ekbe_aux,
      wa_j_1bnflin      TYPE ty_j_1bnflin,
      wa_j_1bnfdoc      TYPE ty_j_1bnfdoc,
      wa_vbak           TYPE ty_vbak,
      wa_vbap           TYPE ty_vbap,
      wa_vbfa           TYPE ty_vbfa,
*      WA_VBFA_tot2       TYPE TY_VBFA                               ,
      wa_vbrk           TYPE ty_vbrk,
      wa_vbrk_aux       TYPE ty_vbrk,
      wa_vbfa_mm        TYPE ty_vbfa_mm,
      wa_tvzbt          TYPE ty_tvzbt,
      wa_mara           TYPE ty_mara,
      wa_j_1bnfe_active TYPE ty_j_1bnfe_active,
      wa_vbfa_tot       TYPE ty_vbfa_tot,
      wa_saida_sd       TYPE ty_saida_sd,

      wa_saida_mm2      TYPE ty_saida_mm2,
      wa_saida_sd2      TYPE ty_saida_sd2,
      wa_konv           TYPE ty_konv,
      wa_vbkd           TYPE ty_vbkd,
      wa_vbfa_aux       TYPE ty_vbfa_aux,
      wa_bdc            TYPE bdcdata,
      wa_zsdt0041       TYPE ty_zsdt0041,
      wa_zsdt0271       TYPE zsdt0271,
      wa_tvkbt          TYPE tvkbt,
      wa_tvbur          TYPE tvbur.

DATA: vl_vbeln_frete TYPE vbap-vbeln,
      vl_posnr_frete TYPE vbap-posnr,
      vl_matnr_frete TYPE vbap-matnr,
      vl_achou_safra TYPE char1,
      vl_vbeln_safra TYPE vbap-vbeln,
      vl_achou_frete TYPE char1,
      vl_vbeln_exec  TYPE vbap-vbeln,
      vl_cont_exec   TYPE i,
      lc_safra_apl   TYPE zsdt0041-safra_apl,
      lc_cultura_apl TYPE zsdt0041-cultura_apl,
      vl_posnr_safra TYPE vbap-posnr.


*----------------------------------------------------------------------*
* ESTRUTURA ALV  - Tabela Estrutura Colunas do Relatório
*----------------------------------------------------------------------*
DATA: it_fcat      TYPE TABLE OF lvc_s_fcat,
      s_variant    TYPE disvariant,
      it_fcat2     TYPE TABLE OF lvc_s_fcat,
      r_ekpo_s     TYPE RANGE OF ekpo-loekz,
      wa_status    LIKE LINE OF  r_ekpo_s,
      gs_variant_c TYPE disvariant.

DATA: variante        LIKE disvariant.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: p_escven    FOR vbak-vkbur OBLIGATORY NO INTERVALS MODIF ID sd, " Escritório de vendas
                p_tpcont    FOR vbak-auart OBLIGATORY NO INTERVALS MODIF ID sd,  " Tipo de Contrato
                p_cont      FOR vbak-vbeln MODIF ID sd,               " Contrato
                p_simula    FOR vbak-vbeln MODIF ID sd,
                p_orgven    FOR vbak-vkorg OBLIGATORY NO INTERVALS NO-EXTENSION MODIF ID sd,  " Organização de vendas
                p_cdist     FOR vbak-vtweg OBLIGATORY  MODIF ID sd, " Canal distribuição
                p_sativ     FOR vbak-spart OBLIGATORY  MODIF ID sd, " Setor de atividade
                p_clien     FOR vbak-kunnr MODIF ID sd, " Cliente
                p_datent    FOR vbak-erdat OBLIGATORY MODIF ID sd, " Data de Entrada
                p_fatuv     FOR vbfa-erdat OBLIGATORY  NO-EXTENSION MODIF ID sd, " Data de criação do registro
                p_cent      FOR vbap-werks MODIF ID sd, " Centro
                p_mater     FOR vbap-matnr MODIF ID sd, " Material
                p_grupo     FOR mara-matkl MODIF ID sd, " Material
*                p_docs      FOR zsdt0041-doc_simulacao MODIF ID sd, "Numero do documento de simulação de venda
                p_waerks    FOR vbak-waerk NO INTERVALS NO-EXTENSION MODIF ID sd. " Moeda
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE text-000.
PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
  DATA: vg_repid   LIKE sy-repid,
        vg_variant TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

** Valida se usuário tem permissão
  PERFORM f_validar_esc_venda CHANGING sy-subrc.
  IF sy-subrc IS INITIAL.

    PERFORM: submit_zsdt0051.

*    PERFORM: form_seleciona_sd,      " Seleção de Dados - Formulário SD
*             form_saida_sd,          " Saída SD
*             form_alv_sd.            " Formulário ALV SD
*
*    CALL SCREEN 0100.

  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FORM_SELECIONA_SD
*&---------------------------------------------------------------------*
FORM form_seleciona_sd .

  DATA  :
    somar         TYPE j_1bnflin-menge,
    diminuir      TYPE j_1bnflin-menge,
    vbelnrfmg     TYPE vbfa-vbeln,
    vg_refkey_aux TYPE j_1bnflin-refkey,
    vg_refkey     TYPE j_1bnflin-refkey.

  IF  p_tpcont  IS INITIAL AND p_simula IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Tipo de Contrato !'.
    STOP.
  ENDIF.

  IF  p_orgven IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Organização de Vendas !'.
    STOP.
  ENDIF.

  IF p_cdist IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Canal Distribuição !'.
    STOP.
  ENDIF.

  IF p_sativ IS INITIAL AND p_simula IS INITIAL..
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Setor de Atividade !'.
    STOP.
  ENDIF.

  IF p_datent IS INITIAL AND p_simula IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data de lançamento !'.
    STOP.
  ENDIF.


  IF p_escven  IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Escritório de Venda !'.
    STOP.
  ENDIF.

  IF  p_fatuv-low  IS INITIAL
  OR p_fatuv-high IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data de Faturamento !'.
    STOP.
  ENDIF.

  IF p_simula IS NOT INITIAL.
    SELECT doc_simulacao vbeln
      INTO TABLE it_zsdt0041
      FROM zsdt0041
     WHERE doc_simulacao IN p_simula.

    SELECT doc_simulacao vbeln
 APPENDING TABLE it_zsdt0041
      FROM zsdt0090
     WHERE doc_simulacao IN p_simula.

    SELECT doc_simulacao vbelv
 APPENDING TABLE it_zsdt0041
      FROM zsdt0090
     WHERE doc_simulacao IN p_simula.

    CHECK it_zsdt0041[] IS NOT INITIAL.

    SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel erdat lifsk faksk
      FROM vbak
      INTO TABLE it_vbak
       FOR ALL ENTRIES IN it_zsdt0041
      WHERE auart IN  p_tpcont
      AND vbeln    =  it_zsdt0041-vbeln
*     AND vbeln   IN  p_cont
      AND vkorg   IN  p_orgven
      AND vtweg   IN  p_cdist
      AND spart   IN  p_sativ
      AND vkbur   IN  p_escven
      AND kunnr   IN  p_clien
      AND audat   IN  p_datent.

    DELETE it_vbak WHERE vbeln NOT IN p_cont.
  ELSE.
    SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel erdat lifsk faksk
      FROM vbak
      INTO TABLE it_vbak
      WHERE auart IN  p_tpcont
      AND vbeln   IN  p_cont
      AND vkorg   IN  p_orgven
      AND vtweg   IN  p_cdist
      AND spart   IN  p_sativ
      AND vkbur   IN  p_escven
      AND kunnr   IN  p_clien
      AND audat   IN  p_datent.

*   CHECK sy-subrc IS INITIAL.

  ENDIF.

*** PBI - 55017 - Inicio
  CHECK it_vbak[] IS NOT INITIAL.

  SELECT *
    APPENDING TABLE it_zsdt0041_safra
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT *
  FROM tvbur
  INTO TABLE it_tvbur
  FOR ALL ENTRIES IN it_vbak
  WHERE vkbur EQ it_vbak-vkbur.
  IF sy-subrc = 0.
    SELECT *
    FROM tvkbt
    INTO TABLE it_tvkbt
    FOR ALL ENTRIES IN it_tvbur
    WHERE vkbur = it_tvbur-vkbur
    AND spras EQ sy-langu.
  ENDIF.

  "=========================================
  SELECT *
FROM tvfs
  INTO TABLE it_tvfs
  FOR ALL ENTRIES IN it_vbak
WHERE faksp EQ it_vbak-lifsk.
  IF sy-subrc = 0.

    SELECT *
    FROM tvfst
      INTO TABLE it_tvfst
      FOR ALL ENTRIES IN it_tvfs
    WHERE faksp = it_tvfs-faksp
      AND  spras EQ sy-langu.
  ENDIF.

  SELECT *
  FROM tvls
    INTO TABLE it_tvls
    FOR ALL ENTRIES IN it_vbak
  WHERE lifsp EQ it_vbak-lifsk.

  IF sy-subrc = 0.
    SELECT *
    FROM tvlst
      INTO TABLE it_tvlst
      FOR ALL ENTRIES IN it_tvls
    WHERE lifsp = it_tvls-lifsp
       AND  spras EQ sy-langu.
  ENDIF.

  "=========================================================
  SELECT *
   FROM zsdt0271
     INTO TABLE it_zsdt0271
    FOR ALL ENTRIES IN it_vbak
   WHERE filial EQ it_vbak-vkbur.

  IF it_zsdt0271[] IS NOT INITIAL.
    SELECT *
     FROM zsdt0270
   INTO TABLE it_zsdt0270
     FOR ALL ENTRIES IN it_zsdt0271
   WHERE cod_regional EQ it_zsdt0271-cod_regional .
  ENDIF.

  SELECT doc_simulacao vbeln
    INTO TABLE it_zsdt0041
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT *
    INTO TABLE it_zsdt0041_frete
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT doc_simulacao vbeln
    APPENDING TABLE it_zsdt0041
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT doc_simulacao vbelv
    APPENDING TABLE it_zsdt0041
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbelv EQ it_vbak-vbeln.

  SELECT *
    INTO TABLE it_zsdt0090_frete
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbelv EQ it_vbak-vbeln
      OR vbeln EQ it_vbak-vbeln.

  SORT it_zsdt0041 BY vbeln doc_simulacao.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0041 COMPARING ALL FIELDS.

  IF it_zsdt0041[] IS NOT INITIAL.

    SELECT doc_simulacao cultura safra
      INTO TABLE it_zsdt0040
      FROM zsdt0040
      FOR ALL ENTRIES IN it_zsdt0041
     WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    SORT it_zsdt0040 BY doc_simulacao.

    SELECT * INTO TABLE it_zsdt0038
      FROM zsdt0038
       FOR ALL ENTRIES IN it_zsdt0040
     WHERE cultura EQ it_zsdt0040-cultura.

    SORT it_zsdt0038 BY cultura.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zfit0026
      INTO TABLE it_0026
       FOR ALL ENTRIES IN it_vbak
     WHERE vbeln EQ it_vbak-vbeln.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zib_contabil_chv
        INTO TABLE it_zib
         FOR ALL ENTRIES IN it_0026
       WHERE obj_key EQ it_0026-obj_key.

      IF sy-subrc IS INITIAL.
        LOOP AT it_zib.
          READ TABLE it_0026 WITH KEY obj_key = it_zib-obj_key.
          IF sy-subrc IS INITIAL.
            MOVE : it_zib-bukrs   TO it_zib_bsik-bukrs,
                   it_0026-docnum TO it_zib_bsik-belnr,
                   it_zib-gjahr   TO it_zib_bsik-gjahr.

            APPEND it_zib_bsik.

            MOVE : it_zib-bukrs   TO it_zib_bsid-bukrs,
                   it_0026-docnum TO it_zib_bsid-belnr,
                   it_zib-gjahr   TO it_zib_bsid-gjahr.

            IF it_0026-docnum = '0000000000'.
              MOVE it_zib-belnr TO it_zib_bsid-belnr.
            ENDIF.

            APPEND it_zib_bsid.

          ENDIF.

          CLEAR: it_zib_bsid.
          CLEAR: it_zib_bsik.

        ENDLOOP.

*        IF sy-subrc IS INITIAL.
*          SELECT *
*            FROM bsik
*            INTO TABLE it_bsik
*             FOR ALL ENTRIES IN it_zib_bsik
*           WHERE bukrs EQ it_zib_bsik-bukrs
*             AND belnr EQ it_zib_bsik-belnr
*             AND gjahr EQ it_zib_bsik-gjahr.
*
*        ENDIF.

        IF sy-subrc IS INITIAL.
          SELECT *
           FROM bsid
           INTO TABLE it_bsid
           FOR ALL ENTRIES IN it_zib_bsid
           WHERE bukrs EQ it_zib_bsid-bukrs
             AND belnr EQ it_zib_bsid-belnr
             AND gjahr EQ it_zib_bsid-gjahr.

          IF sy-subrc IS INITIAL.
            SELECT *
              FROM bsad
              INTO TABLE it_bsad
              FOR ALL ENTRIES IN it_bsid
              WHERE bukrs EQ it_bsid-bukrs
                AND belnr EQ it_bsid-belnr
                AND gjahr EQ it_bsid-gjahr.
          ENDIF.
*** PBI - 55017 - Fim
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  SELECT *
    FROM vbkd
    INTO TABLE it_vbkd_aux
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM t052u
      INTO TABLE it_t052u
       FOR ALL ENTRIES IN it_vbkd_aux
     WHERE spras EQ 'PT'
       AND zterm EQ it_vbkd_aux-zterm.


    SELECT   spras   zterm   vtext
      FROM tvzbt
      INTO TABLE it_tvzbt
       FOR ALL ENTRIES IN it_vbkd_aux
     WHERE spras EQ 'PT'
       AND zterm EQ it_vbkd_aux-zterm.
  ENDIF.

  SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , WAERS , KBETR , KMEIN , KWERT FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KNUMV AND KSCHL EQ 'PR00' AND WAERS IN @P_WAERKS INTO TABLE @IT_KONV .

  SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , WAERS , KBETR , KMEIN , KWERT FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KNUMV AND KSCHL EQ 'ICMI' AND WAERS IN @P_WAERKS INTO TABLE @IT_KONV_IMP .

  SELECT vbeln vbap~matnr arktx werks vbap~ntgew vbap~gewei posnr kwmeng vrkme j_1bcfop j_1btxsdc lgort
               vbap~netwr vbap~mwsbp
    FROM vbap
    INNER JOIN mara
    ON mara~matnr  = vbap~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_vbap
     FOR ALL ENTRIES IN it_vbak
   WHERE vbap~vbeln EQ it_vbak-vbeln
    AND vbap~matnr IN p_mater
    AND mara~matkl IN p_grupo
    AND vbap~werks IN p_cent.

  CHECK sy-subrc IS INITIAL.


  SELECT *
    FROM zsdt0082
  INTO TABLE it_zsdt0082
    FOR ALL ENTRIES IN it_vbap
  WHERE vbeln EQ it_vbap-vbeln
    AND posnr EQ it_vbap-posnr
    AND status = 1.

***Fim - USER STORY fim
  SELECT *
  FROM zsdt0275
INTO TABLE it_zsdt0275
  FOR ALL ENTRIES IN it_zsdt0041
WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

  SELECT *
    FROM zsdt0274
  INTO TABLE it_zsdt0274
    FOR ALL ENTRIES IN it_zsdt0040
  WHERE safra       EQ it_zsdt0040-safra
   AND cod_cultura  EQ it_zsdt0040-cultura.


  SELECT *
    FROM zsdt0273
  APPENDING TABLE it_zsdt0273
    FOR ALL ENTRIES IN it_zsdt0274
  WHERE nr_proposta = it_zsdt0274-nr_proposta.

  SELECT *
    FROM zsdt0273
  APPENDING TABLE it_zsdt0273
  WHERE cpf_cnpj IN rg_cpf_cnpj.

  IF it_zsdt0273[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0272
   APPENDING TABLE it_zsdt0272
      FOR ALL ENTRIES IN it_zsdt0273
    WHERE nr_proposta = it_zsdt0273-nr_proposta
    AND tp_proposta   = '2'.
  ENDIF.

  IF it_zsdt0274[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0272
   APPENDING TABLE it_zsdt0272
      FOR ALL ENTRIES IN it_zsdt0274
   WHERE nr_proposta = it_zsdt0274-nr_proposta
    AND tp_proposta   = '1'.
  ENDIF.
                                                            "BUG 63604

  DELETE it_zsdt0272 WHERE estagio EQ 'Cancelado'.
*** PBI - 55017 - Fim


  LOOP AT it_zsdt0273 .
    READ TABLE it_zsdt0272 WITH KEY nr_proposta = it_zsdt0273-nr_proposta.
    IF sy-subrc NE 0.
      DELETE it_zsdt0273 WHERE nr_proposta EQ it_zsdt0273-nr_proposta.
    ENDIF.


  ENDLOOP.
***Fim - USER STORY 61211

  SELECT matnr wrkst matkl
    FROM mara
    INTO TABLE it_mara
    FOR ALL ENTRIES IN it_vbap
    WHERE matnr EQ it_vbap-matnr.

  SELECT vbeln posnr etenr lifsp
  FROM vbep
  INTO TABLE it_vbep
   FOR ALL ENTRIES IN it_vbap
 WHERE vbeln EQ it_vbap-vbeln
   AND posnr EQ it_vbap-posnr
   AND etenr EQ 1.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbep2
    FROM vbep
    FOR ALL ENTRIES IN it_vbap
    WHERE vbeln EQ it_vbap-vbeln
      AND posnr EQ it_vbap-posnr.

  SELECT vbeln valdt
    FROM vbkd
    INTO TABLE it_vbkd
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT name1 kunnr stcd1 stcd2
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_vbak
    WHERE kunnr EQ it_vbak-kunnr.

  REFRESH rg_cpf_cnpj.
  rg_cpf_cnpj-sign = 'I'.
  rg_cpf_cnpj-option = 'EQ'.

  LOOP AT it_kna1 INTO wa_kna1.
    IF wa_kna1-stcd1 IS NOT INITIAL.
      rg_cpf_cnpj-low = wa_kna1-stcd1.
      APPEND rg_cpf_cnpj.
    ENDIF.
    IF wa_kna1-stcd2 IS NOT INITIAL.
      rg_cpf_cnpj-low = wa_kna1-stcd2.
      APPEND rg_cpf_cnpj.
    ENDIF.
  ENDLOOP.

  CHECK sy-subrc IS INITIAL.

  SELECT vbelv vbeln erdat rfmng rfwrt vbtyp_n vbtyp_v posnn posnv
    FROM vbfa
    INTO TABLE it_vbfa_aux2
     FOR ALL ENTRIES IN it_vbap
   WHERE vbelv   EQ it_vbap-vbeln
*     AND POSNN   EQ IT_VBAP-POSNR
     AND posnv   EQ it_vbap-posnr
     AND vbtyp_n IN ('M','O','N','R','S')
     AND vbtyp_v IN ('C','M','T').
*  AND ERDAT   IN P_FATUV.

*** Modificação - Eduardo Ruttkowski Tavares - 14.08.2013 >>> INI
* CH 100102 - Ajuste_Qte_faturada_ZSDT0027_ZSDT0051
  LOOP AT it_vbfa_aux2.
    IF it_vbfa_aux2-erdat IN p_fatuv.
      APPEND it_vbfa_aux2 TO it_vbfa.
      DELETE it_vbfa_aux2.
    ELSEIF it_vbfa_aux2-erdat GT p_fatuv-high.
      DELETE it_vbfa_aux2.
    ENDIF.
  ENDLOOP.
*** Modificação - Eduardo Ruttkowski Tavares - 14.08.2013 <<< END


*  CHECK SY-SUBRC IS INITIAL.
  IF it_vbfa[] IS NOT INITIAL.
    SELECT vbeln fkart sfakn netwr mwsbk
      FROM vbrk
      INTO TABLE it_vbrk
       FOR ALL ENTRIES IN it_vbfa
     WHERE vbeln = it_vbfa-vbeln.

    it_vbrk_aux[] = it_vbrk[].

** Pegando registro de estorno
    it_vbrk_est[] = it_vbrk[].
    DELETE it_vbrk_est WHERE fkart NE 'ZROB'.

    IF it_vbrk_est[] IS NOT INITIAL.
      SELECT vbelv vbeln erdat rfmng rfwrt vbtyp_n vbtyp_v posnn posnv
        INTO TABLE it_vbfa_est
        FROM vbfa
        FOR ALL ENTRIES IN it_vbrk_est
        WHERE vbeln EQ it_vbrk_est-vbeln
         AND  vbtyp_n EQ 'O'
         AND  vbtyp_v EQ 'H'.

      IF it_vbfa_est[] IS NOT INITIAL.
        SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel
          INTO TABLE it_vbak_est
          FROM vbak
          FOR ALL ENTRIES IN it_vbfa_est
          WHERE vbeln EQ it_vbfa_est-vbelv.

        IF it_vbak_est[] IS NOT INITIAL.
          LOOP AT it_vbak_est INTO wa_vbak.
            READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbak-vgbel
                                                     fkart = 'ZTRI'.
            IF sy-subrc IS INITIAL.
** Elimina estorno ZTRI
              DELETE it_vbrk WHERE vbeln EQ wa_vbak-vgbel.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

    "---

    DELETE it_vbrk_aux WHERE sfakn IS INITIAL.

    SORT it_vbrk BY vbeln.
    SORT it_vbfa BY vbeln.

    LOOP AT it_vbrk_aux INTO wa_vbrk_aux.
      "Elimina Estorno VBRK
      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-sfakn BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      "Elimina Estorno VBFA
*      READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELN = WA_VBRK_AUX-SFAKN BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        DELETE IT_VBFA INDEX SY-TABIX.
      DELETE it_vbfa WHERE vbeln EQ wa_vbrk_aux-sfakn.
*      ENDIF.
*
*      READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELN = WA_VBRK_AUX-VBELN BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        DELETE IT_VBFA INDEX SY-TABIX.
      DELETE it_vbfa WHERE vbeln EQ wa_vbrk_aux-vbeln.
*      ENDIF.

    ENDLOOP.


    "---
    LOOP AT it_vbfa INTO wa_vbfa.
      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
      APPEND wa_vbfa_aux TO it_vbfa_aux.

    ENDLOOP.

    SORT it_vbfa_aux BY vg_refkey.

    SELECT refkey docnum menge meins netwr menge itmnum
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
       FOR ALL ENTRIES IN it_vbfa_aux
     WHERE refkey  EQ it_vbfa_aux-vg_refkey.

    CHECK sy-subrc IS INITIAL.

    SELECT docnum docdat nfnum nfe nfenum
      FROM j_1bnfdoc
      INTO TABLE it_j_1bnfdoc
       FOR ALL ENTRIES IN it_j_1bnflin
     WHERE docnum EQ it_j_1bnflin-docnum.

    CHECK sy-subrc IS INITIAL.


    SORT: it_vbfa      BY vbeln posnn,
          it_j_1bnflin BY refkey itmnum,
          it_vbrk      BY vbeln.

    LOOP AT it_vbap INTO wa_vbap.

      CLEAR: somar, diminuir, wa_vbfa_tot.

*      LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV EQ WA_VBAP-VBELN AND POSNN EQ WA_VBAP-POSNR.
      LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnv EQ wa_vbap-posnr.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.
        IF wa_vbrk-fkart = 'ZTRI'.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vbfa-vbeln
          IMPORTING
            output = wa_vbfa-vbeln.

        vg_refkey = wa_vbfa-vbeln.

        READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = vg_refkey itmnum = wa_vbfa-posnn BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          IF wa_vbfa-vbtyp_n = 'M'.
            somar = somar + wa_j_1bnflin-menge.
          ELSEIF ( wa_vbfa-vbtyp_n = 'N') OR ( wa_vbfa-vbtyp_n = 'O')
            OR   ( wa_vbfa-vbtyp_n = 'R'  AND  wa_vbfa-vbtyp_v = 'T' ) .
            diminuir = diminuir + wa_j_1bnflin-menge.
          ENDIF.

          wa_vbfa_tot-totalmenge = somar - diminuir.
*          WA_VBFA_TOT-POSNN = WA_VBFA-POSNN.
          wa_vbfa_tot-posnn = wa_vbfa-posnv.
          wa_vbfa_tot-vbelnrfmg  = wa_vbfa-vbelv.
          wa_vbfa_tot-totaldim = diminuir.
          wa_vbfa_tot-vlr_totbrt_fat = wa_vbrk-netwr + wa_vbrk-mwsbk.
        ENDIF.

      ENDLOOP.

      IF wa_vbfa_tot-totalmenge GE 0.
        APPEND wa_vbfa_tot TO it_vbfa_tot.

        CLEAR: wa_j_1bnflin,
               wa_vbfa,
               wa_vbap.
      ENDIF.

    ENDLOOP.
  ENDIF.

*** Modificação - Eduardo Ruttkowski Tavares - 14.08.2013 >>> INI
* CH 100102 - Ajuste_Qte Faturada_ZSDT0027_ZSDT0051

  IF it_vbfa_aux2[] IS NOT INITIAL.
    SELECT vbeln fkart sfakn netwr mwsbk
      FROM vbrk
    APPENDING TABLE it_vbrk
       FOR ALL ENTRIES IN it_vbfa_aux2
     WHERE vbeln = it_vbfa_aux2-vbeln.

    it_vbrk_aux[] = it_vbrk[].
    DELETE it_vbrk_aux WHERE sfakn IS INITIAL.

    SORT it_vbrk      BY vbeln.
    SORT it_vbfa_aux2 BY vbeln.

    LOOP AT it_vbrk_aux INTO wa_vbrk_aux.
      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-sfakn BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      "Elimina Estorno VBFA
      READ TABLE it_vbfa_aux2 INTO wa_vbfa WITH KEY vbeln = wa_vbrk_aux-sfakn BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbfa_aux2 INDEX sy-tabix.
      ENDIF.

      READ TABLE it_vbfa_aux2 INTO wa_vbfa WITH KEY vbeln = wa_vbrk_aux-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbfa_aux2 INDEX sy-tabix.
      ENDIF.

    ENDLOOP.

    REFRESH it_vbfa_aux.

    LOOP AT it_vbfa_aux2 INTO wa_vbfa.
      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
      APPEND wa_vbfa_aux TO it_vbfa_aux.

    ENDLOOP.

    SORT it_vbfa_aux BY vg_refkey.

    SELECT refkey docnum menge meins netwr menge itmnum
      FROM j_1bnflin
    APPENDING TABLE it_j_1bnflin
       FOR ALL ENTRIES IN it_vbfa_aux
     WHERE refkey  EQ it_vbfa_aux-vg_refkey.

    SORT: it_vbfa_aux2 BY vbeln posnn,
          it_j_1bnflin BY refkey itmnum,
          it_vbrk      BY vbeln.

    LOOP AT it_vbap INTO wa_vbap.

      CLEAR: somar, diminuir, wa_vbfa_tot.

*      LOOP AT IT_VBFA_AUX2 INTO WA_VBFA WHERE VBELV EQ WA_VBAP-VBELN AND POSNN EQ WA_VBAP-POSNR.
      LOOP AT it_vbfa_aux2 INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnv EQ wa_vbap-posnr.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.
        IF wa_vbrk-fkart = 'ZTRI'.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vbfa-vbeln
          IMPORTING
            output = wa_vbfa-vbeln.

        vg_refkey = wa_vbfa-vbeln.

        READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = vg_refkey itmnum = wa_vbfa-posnn BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          "Somar/Diminuir
          IF wa_vbfa-vbtyp_n = 'M'.
            somar = somar + wa_j_1bnflin-menge.
          ELSEIF ( wa_vbfa-vbtyp_n = 'N' ) OR ( wa_vbfa-vbtyp_n = 'O' )
              OR ( wa_vbfa-vbtyp_n = 'R' AND wa_vbfa-vbtyp_v = 'T' ).
            diminuir = diminuir + wa_j_1bnflin-menge.
          ENDIF.

          wa_vbfa_tot-totalmenge = somar - diminuir.
*          WA_VBFA_TOT-POSNN = WA_VBFA-POSNN.
          wa_vbfa_tot-posnn = wa_vbfa-posnv.
          wa_vbfa_tot-vbelnrfmg  = wa_vbfa-vbelv.

        ENDIF.
      ENDLOOP.

      IF wa_vbfa_tot-totalmenge GE 0.

        APPEND wa_vbfa_tot TO it_vbfa_tot2.

        CLEAR: wa_j_1bnflin,
               wa_vbfa,
               wa_vbap.
      ENDIF.

    ENDLOOP.

  ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 14.08.2013 <<< END

** pbi - 55017 - inicio

  SELECT *
  APPENDING TABLE it_zsdt0090_safra
  FROM zsdt0090
   FOR ALL ENTRIES IN it_vbap
 WHERE vbeln EQ it_vbap-vbeln
    AND posnn EQ it_vbap-posnr.

  IF sy-subrc IS INITIAL.
    SELECT *
    APPENDING TABLE it_zsdt0041_safra
    FROM zsdt0041
      FOR ALL ENTRIES IN it_zsdt0090_safra
  WHERE vbeln EQ it_zsdt0090_safra-vbelv.
  ENDIF.

  SELECT *
    FROM zsdt0082
  INTO TABLE it_zsdt0082
    FOR ALL ENTRIES IN it_vbap
  WHERE vbeln EQ it_vbap-vbeln
    AND posnr EQ it_vbap-posnr
    AND status = 1.

  SELECT *
    FROM zsdt0275
  INTO TABLE it_zsdt0275
    FOR ALL ENTRIES IN it_zsdt0041
  WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

  SELECT *
    FROM zsdt0274
  INTO TABLE it_zsdt0274
    FOR ALL ENTRIES IN it_zsdt0040
  WHERE safra       EQ it_zsdt0040-safra
   AND cod_cultura  EQ it_zsdt0040-cultura.


  SELECT *
    FROM zsdt0273
  APPENDING TABLE it_zsdt0273
    FOR ALL ENTRIES IN it_zsdt0274
  WHERE nr_proposta = it_zsdt0274-nr_proposta.

  SELECT *
    FROM zsdt0273
  APPENDING TABLE it_zsdt0273
  WHERE cpf_cnpj IN rg_cpf_cnpj.

  IF it_zsdt0273[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0272
   APPENDING TABLE it_zsdt0272
      FOR ALL ENTRIES IN it_zsdt0273
    WHERE nr_proposta = it_zsdt0273-nr_proposta
    AND tp_proposta   = '2'.
  ENDIF.

  IF it_zsdt0274[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0272
   APPENDING TABLE it_zsdt0272
      FOR ALL ENTRIES IN it_zsdt0274
   WHERE nr_proposta = it_zsdt0274-nr_proposta
    AND tp_proposta   = '1'.
  ENDIF.

                                                            "BUG 63604

  DELETE it_zsdt0272 WHERE estagio EQ 'Cancelado'.
*** PBI - 55017 - Fim


  LOOP AT it_zsdt0273 .
    READ TABLE it_zsdt0272 WITH KEY nr_proposta = it_zsdt0273-nr_proposta.
    IF sy-subrc NE 0.
      DELETE it_zsdt0273 WHERE nr_proposta EQ it_zsdt0273-nr_proposta.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " FORM_SELECIONA_SD

*&---------------------------------------------------------------------*
*&      Form  FORM_SAIDA_SD
*&---------------------------------------------------------------------*
FORM form_saida_sd .
  DATA: x_saldo       TYPE vbap-zmeng,
        ntgewaux      TYPE bstmg,
        sac           TYPE vbap-vrkme,
        wl_xconversor TYPE vbap-kwmeng,
        aux(100)      TYPE c,
        wa_konv_imp   TYPE ty_konv.

  DATA: lc_safra         TYPE ajahr,
        lc_cultura       TYPE acc_txtlg,
        lc_doc_simulacao TYPE zsded003.

  REFRESH it_saida_sd.
  CLEAR: it_vbap_aux[].
  MOVE it_vbap[] TO it_vbap_aux[].
  SORT it_vbap_aux[] BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM it_vbap_aux COMPARING vbeln posnr.

*  IF NOT it_vbap_aux[] IS INITIAL.
*    SELECT * INTO TABLE it_vbep
*      FROM vbep
*       FOR ALL ENTRIES IN it_vbap_aux
*     WHERE vbeln EQ it_vbap_aux-vbeln
*       AND posnr EQ it_vbap_aux-posnr
*       AND etenr EQ 1.
*
*    SELECT * INTO TABLE it_vbep2
*      FROM vbep
*       FOR ALL ENTRIES IN it_vbap_aux
*     WHERE vbeln EQ it_vbap_aux-vbeln
*       AND posnr EQ it_vbap_aux-posnr.
*  ENDIF.


  SORT:
    it_vbap      BY vbeln,
    it_vbfa_tot  BY vbelnrfmg posnn,
    it_vbfa_tot2 BY vbelnrfmg posnn,
    it_kna1      BY kunnr,
    it_konv      BY knumv kposn,
    it_vbkd      BY vbeln,
    it_mara      BY matnr,
    it_tvzbt     BY zterm.

  LOOP AT it_vbak INTO wa_vbak.

    CLEAR : wa_vbfa,wa_vbap,wa_kna1, wa_saida_sd, wa_vbfa_tot, wl_xconversor, lc_safra, lc_cultura.

    READ TABLE it_zsdt0041 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      lc_doc_simulacao = it_zsdt0041-doc_simulacao.
      READ TABLE it_zsdt0040 WITH KEY doc_simulacao = it_zsdt0041-doc_simulacao BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lc_safra       = it_zsdt0040-safra.
        READ TABLE it_zsdt0038 WITH KEY cultura = it_zsdt0040-cultura BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lc_cultura   = it_zsdt0038-descricao.
        ENDIF.
      ENDIF.
    ENDIF.


    LOOP AT it_zsdt0274.
      TRANSLATE it_zsdt0274-cultura TO UPPER CASE.
      MODIFY it_zsdt0274.
      CLEAR  it_zsdt0274.
    ENDLOOP.

    TRANSLATE lc_cultura TO UPPER CASE.

*** PBI - 55017 - Inicio
    READ TABLE it_zsdt0275 WITH KEY doc_simulacao = lc_doc_simulacao.
    IF sy-subrc = 0.
      READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0275-nr_proposta
                                       safra       = lc_safra
                                       cultura     = lc_cultura.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
        CLEAR: vl_cpf.

        IF wa_kna1-stcd1 IS NOT INITIAL.
          vl_cpf = wa_kna1-stcd1.
        ELSE.
          vl_cpf = wa_kna1-stcd2.
        ENDIF.
        CLEAR: wa_kna1.

        READ TABLE it_zsdt0273 WITH KEY nr_proposta = it_zsdt0274-nr_proposta
                                        cpf_cnpj    = vl_cpf .

        IF sy-subrc = 0.
          LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0273-nr_proposta.
            IF wa_saida_sd-nrpropopr IS INITIAL.
              wa_saida_sd-nrpropopr = it_zsdt0272-nr_proposta.
            ELSE.
              CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrpropopr
                INTO wa_saida_sd-nrpropopr SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-alcpropopr IS INITIAL.
              wa_saida_sd-alcpropopr = it_zsdt0272-estagio.
            ELSE.
              CONCATENATE it_zsdt0272-estagio wa_saida_sd-alcpropopr
              INTO wa_saida_sd-alcpropopr SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-efetivada IS INITIAL.
              wa_saida_sd-efetivada = it_zsdt0272-efetivada.
            ELSE.
              CONCATENATE it_zsdt0272-efetivada wa_saida_sd-efetivada
              INTO wa_saida_sd-efetivada SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-nrpropopr IS NOT INITIAL.
              IF wa_saida_sd-nrproplmt  IS INITIAL.
                wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta_ref.
              ELSE.
                CONCATENATE it_zsdt0272-nr_proposta_ref wa_saida_sd-nrproplmt
                INTO wa_saida_sd-nrproplmt SEPARATED BY ','.
              ENDIF.
*** BUG  - 59327 - CSB
              IF wa_saida_sd-alcproplmt IS INITIAL.
                wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
              ELSE.
                CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
                INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
              ENDIF.
*** BUG  - 59327 - CSB
            ELSE.

              READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
              IF sy-subrc IS INITIAL.
                READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
                                 safra       = lc_safra
                                 cultura     = lc_cultura .
                IF sy-subrc = 0.
                  LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
                    AND tp_proposta = '1'.
                    IF wa_saida_sd-nrproplmt IS INITIAL.
                      wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta.
                    ELSE.
                      CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrproplmt INTO
                      wa_saida_sd-nrproplmt SEPARATED BY ','.
                    ENDIF.

                    IF wa_saida_sd-alcproplmt IS INITIAL.
                      wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
                    ELSE.
                      CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
                      INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_saida_sd-nrpropopr IS INITIAL.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      CLEAR: vl_cpf.

      IF wa_kna1-stcd1 IS NOT INITIAL.
        vl_cpf = wa_kna1-stcd1.
      ELSE.
        vl_cpf = wa_kna1-stcd2.
      ENDIF.
      CLEAR: wa_kna1.

      READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
      IF sy-subrc IS INITIAL.

        READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
                 safra       = lc_safra
                 cultura     = lc_cultura .
        IF sy-subrc = 0.
          LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
           AND tp_proposta = '1'.
            IF wa_saida_sd-nrproplmt IS INITIAL.
              wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta.
            ELSE.
              CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrproplmt INTO
              wa_saida_sd-nrproplmt SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-alcproplmt IS INITIAL.
              wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
            ELSE.
              CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
              INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
*** PBI - 55017 - fim


    LOOP AT it_vbap INTO wa_vbap WHERE vbeln EQ wa_vbak-vbeln.

      wa_saida_sd-j_1bcfop  = wa_vbap-j_1bcfop.
      wa_saida_sd-auart     = wa_vbak-auart.
      wa_saida_sd-j_1btxsdc = wa_vbap-j_1btxsdc.

      wa_saida_sd-werks = wa_vbap-werks. "RJF
      wa_saida_sd-lgort = wa_vbap-lgort.

      READ TABLE it_konv_imp INTO wa_konv_imp WITH KEY knumv = wa_vbak-knumv
                                                       kposn = wa_vbap-posnr BINARY SEARCH.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        IF wa_konv-kmein NE wa_konv_imp-kmein.
          IF wa_konv_imp-kmein EQ 'TO' AND wa_konv-kmein EQ 'KG'.
            IF wa_vbap-kwmeng IS INITIAL.
              wa_saida_sd-kbetr2 = 0.
            ELSE.
              wa_saida_sd-kbetr2 = wa_konv_imp-kwert / ( wa_vbap-kwmeng * 1000 ).
            ENDIF.
          ELSEIF wa_konv_imp-kmein EQ 'KG' AND wa_konv-kmein EQ 'TO'.
            IF wa_vbap-kwmeng IS INITIAL.
              wa_saida_sd-kbetr2 = 0.
            ELSE.
              wa_saida_sd-kbetr2 = wa_konv_imp-kwert / ( wa_vbap-kwmeng / 1000 ).
            ENDIF.
          ENDIF.
        ELSE.
          wa_saida_sd-kbetr2 = wa_konv_imp-kbetr.
        ENDIF.
      ENDIF.

      CLEAR: wa_konv.
******* Buscar Frete da Ordem: Se Ordem na ZSDT0041 busca o próprio frete, se na ZSDT0090 busca o frete da Ordem precedente inicial

      CLEAR: vl_vbeln_frete, vl_posnr_frete, vl_matnr_frete, vl_achou_frete.
      vl_vbeln_exec = wa_vbap-vbeln.
      vl_cont_exec = 0.

      vl_vbeln_frete = wa_vbap-vbeln.
      vl_posnr_frete = wa_vbap-posnr.
      vl_matnr_frete = wa_vbap-matnr.

      WHILE vl_achou_frete IS INITIAL.

        READ TABLE it_zsdt0041_frete INTO wa_zsdt0041_frete WITH KEY vbeln = vl_vbeln_frete
                                                                     matnr = vl_matnr_frete.
        IF sy-subrc IS INITIAL.
          wa_saida_sd-vlr_frete = wa_zsdt0041_frete-vlr_frete.
          vl_achou_frete = abap_true.
        ELSE.
          READ TABLE it_zsdt0090_frete INTO wa_zsdt0090_frete WITH KEY vbeln = vl_vbeln_frete
                                                                       posnn = vl_posnr_frete.
          IF sy-subrc IS INITIAL.
            vl_vbeln_frete = wa_zsdt0090_frete-vbelv.
            vl_posnr_frete = wa_zsdt0090_frete-posnv.
            vl_matnr_frete = wa_zsdt0090_frete-matnrv.
          ELSE.
            "Não tem ordem nem na 0090 nem na 0041  --> Mostra frete em Branco
            wa_saida_sd-vlr_frete = space.
            vl_achou_frete = abap_true.
          ENDIF.
        ENDIF.

      ENDWHILE.

*******

* PBI - 55017 - Inicio

      CLEAR: vl_vbeln_safra,
             vl_posnr_safra,
             lc_safra_apl,
             lc_cultura_apl,
             vl_achou_safra.

      vl_vbeln_safra = wa_vbap-vbeln.
      vl_posnr_safra = wa_vbap-posnr.

      WHILE vl_achou_safra IS INITIAL.

        READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = vl_vbeln_safra.
        IF sy-subrc IS INITIAL.
          lc_safra_apl     = wa_zsdt0041_safra-safra_apl.
          lc_cultura_apl   = wa_zsdt0041_safra-cultura_apl.
          vl_achou_safra = abap_true.
        ELSE.

          READ TABLE it_zsdt0090_safra INTO wa_zsdt0090_safra WITH KEY vbeln = vl_vbeln_safra
                                                                       posnn = vl_posnr_safra.
          IF sy-subrc IS INITIAL.
            vl_vbeln_safra = wa_zsdt0090_safra-vbelv.
            vl_posnr_safra = wa_zsdt0090_safra-posnv.
          ELSE.
            vl_achou_safra = abap_true.
          ENDIF.
        ENDIF.
      ENDWHILE.

      wa_saida_sd-safra_apl   =  lc_safra_apl.
      wa_saida_sd-cultura_apl =  lc_cultura_apl.


      LOOP AT it_zsdt0082 WHERE vbeln EQ wa_vbap-vbeln
                           AND  posnr = wa_vbap-posnr.
** BUG 59327
        wa_saida_sd-qte_sol = wa_saida_sd-qte_sol + it_zsdt0082-qte_sol.

      ENDLOOP.



* PBI - 55017 - Fim

      wa_saida_sd-erdat = wa_vbak-erdat.

      CLEAR: wa_saida_sd-status.

      wa_saida_sd-safra         = lc_safra.
      wa_saida_sd-cultura       = lc_cultura.
      wa_saida_sd-doc_simulacao = lc_doc_simulacao.

      LOOP AT it_0026 WHERE vbeln EQ wa_vbap-vbeln.
* PBI - 55017 - Inicio
*       READ TABLE it_zib_bsik WITH KEY belnr = it_0026-docnum.
*        READ TABLE it_bsik WITH KEY bukrs = it_zib_bsik-bukrs
*                                    belnr = it_zib_bsik-belnr
*                                    gjahr = it_zib_bsik-gjahr.
* PBI - 55017 - Fim
* PBI - 55017 - Inicio

        IF it_0026-docnum = '0000000000'.
          READ TABLE it_zib WITH  KEY obj_key = it_0026-obj_key.
          READ TABLE it_zib_bsid WITH KEY belnr = it_zib-belnr.
        ELSE.
          READ TABLE it_zib_bsid WITH KEY belnr = it_0026-docnum.
        ENDIF.

        READ TABLE it_bsid WITH KEY bukrs = it_zib_bsid-bukrs
                            belnr = it_zib_bsid-belnr
                            gjahr = it_zib_bsid-gjahr.
* PBI - 55017 - Fim
        IF sy-subrc IS INITIAL.
          IF wa_saida_sd-status EQ 'Q'.
            wa_saida_sd-status = 'P'.
            sy-subrc = 0.
            CONTINUE.
          ELSE.
            sy-subrc = 8.
          ENDIF.

        ELSE.
          wa_saida_sd-status = 'Q'.
          sy-subrc = 0.
*       continue.
        ENDIF .

        READ TABLE it_bsad WITH KEY bukrs = it_zib_bsid-bukrs
                                    belnr = it_zib_bsid-belnr
                                    gjahr = it_zib_bsid-gjahr.
        IF sy-subrc IS INITIAL.
          wa_saida_sd-augdt = it_bsad-augdt .
        ENDIF.

      ENDLOOP.

      IF sy-subrc IS NOT INITIAL.
        wa_saida_sd-status = 'A'.
      ENDIF.

      READ TABLE it_vbkd_aux WITH KEY vbeln = wa_vbap-vbeln.

      IF sy-subrc IS INITIAL .
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 101103 - Ajuste_Relatório_ZSDT0027_ZSDT0051
* inserindo: VBKD-INCO1 / VBKD-INCO2
        wa_saida_sd-inco1 = it_vbkd_aux-inco1.
        wa_saida_sd-inco2 = it_vbkd_aux-inco2.
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END

        wa_saida_sd-kurrf = it_vbkd_aux-kurrf.

        READ TABLE it_t052u WITH KEY zterm = it_vbkd_aux-zterm.
        IF sy-subrc IS INITIAL.
          CONCATENATE it_vbkd_aux-zterm '-' it_t052u-text1 INTO wa_saida_sd-zterm SEPARATED BY space.
        ENDIF.

      ENDIF.

      READ TABLE it_tvzbt INTO wa_tvzbt WITH KEY zterm = it_vbkd_aux-zterm.

      wa_saida_sd-vtext = wa_tvzbt-vtext.

      READ TABLE it_vbfa_tot INTO wa_vbfa_tot WITH KEY vbelnrfmg = wa_vbap-vbeln
                                                       posnn = wa_vbap-posnr   BINARY SEARCH.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr BINARY SEARCH.

      IF  p_waerks-low IS NOT INITIAL
      AND wa_konv-waers <> p_waerks-low.
        CONTINUE.
      ENDIF.

      READ TABLE it_vbep INTO wa_vbep WITH KEY vbeln = wa_vbap-vbeln
                                               posnr = wa_vbap-posnr
                                               etenr = 1.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      aux = ''.

      CLEAR: x_saldo.

      wa_saida_sd-lifsp = wa_vbep-lifsp.

      wa_saida_sd-name1 = wa_kna1-name1. " Cliente
      wa_saida_sd-vkbur = wa_vbak-vkbur. " Escr.Vendas

*** PBI - 55017 - Inicio
      READ TABLE it_tvbur WITH KEY vkbur =  wa_vbak-vkbur.
      IF sy-subrc = 0.
        READ TABLE it_tvkbt WITH KEY vkbur = it_tvbur-vkbur.
        wa_saida_sd-bezei = it_tvkbt-bezei.
      ENDIF.
      READ TABLE it_zsdt0271 WITH KEY filial =  wa_vbak-vkbur.
      IF sy-subrc = 0.
        READ TABLE it_zsdt0270 WITH KEY cod_regional = it_zsdt0271-cod_regional.
        wa_saida_sd-regional = it_zsdt0270-regional.
      ENDIF.

      IF wa_vbak-lifsk IS NOT INITIAL.
        READ TABLE it_tvls WITH KEY lifsp = wa_vbak-lifsk.
        READ TABLE it_tvlst WITH KEY lifsp = it_tvls-lifsp.
        wa_saida_sd-lifsp_t = it_tvlst-vtext.
      ENDIF.

      IF wa_vbak-faksk IS NOT INITIAL.
        READ TABLE it_tvfs  WITH  KEY faksp = wa_vbak-lifsk.
        READ TABLE it_tvfst WITH  KEY faksp = it_tvfs-faksp.
        wa_saida_sd-lifsp_t = it_tvfst-vtext.
      ENDIF.


*** PBI - 55017 - Fim

      wa_saida_sd-vbeln = wa_vbak-vbeln. " Contrato
      wa_saida_sd-posnr = wa_vbap-posnr. " Item
      wa_saida_sd-erdat = wa_vbak-erdat. "Data criaçâo

      wa_saida_sd-waers = wa_konv-waers.

*PERFORM BUSCA_IMPOSTO USING WA_VBAK-VBELN WA_VBAP-POSNR.

      wa_saida_sd-kbetr = wa_konv-kbetr.
      wa_saida_sd-valdt = wa_vbkd-valdt.

      wa_saida_sd-matnr = wa_vbap-matnr.
      wa_saida_sd-arktx = wa_vbap-arktx.

      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.
      wa_saida_sd-wrkst = wa_mara-wrkst.
      wa_saida_sd-matkl = wa_mara-matkl.

*      IF ( wa_vbap-vrkme EQ 'BAG' ).
*        wa_saida_sd-qtd  = wa_vbap-kwmeng.
*        wa_saida_sd-unid = 'SAC'.
*
*      ELSE.
        wa_saida_sd-qtd  = wa_vbap-kwmeng.
        wa_saida_sd-unid = wa_vbap-vrkme.

*      ENDIF.


      wa_saida_sd-qtd = wa_saida_sd-qtd - wa_vbfa_tot-totaldim.
      wa_saida_sd-vlr_totbrt  = wa_vbap-netwr   + wa_vbap-mwsbp.
      wa_saida_sd-vlr_qtecont = wa_vbap-netwr.

*** Modificação - Eduardo Ruttkowski Tavares - 13.08.2013 >>> INI
* CH 100102 - Ajuste_Qte Faturada_ZSDT0027_ZSDT0051
      READ TABLE it_vbfa_tot2 WITH KEY vbelnrfmg = wa_vbap-vbeln
                                       posnn     = wa_vbap-posnr   BINARY SEARCH.

      SUBTRACT it_vbfa_tot2-totalmenge FROM wa_saida_sd-qtd.

      wa_saida_sd-qtefaturado =  wa_vbfa_tot-totalmenge.
      x_saldo =  wa_saida_sd-qtd - wa_saida_sd-qtefaturado.
      wa_saida_sd-saldo =  x_saldo.
      wa_saida_sd-vlr_totbrt_fat = wa_vbfa_tot-vlr_totbrt_fat.

*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 >>> INI
* CH 100102 - Ajuste_Qte Faturada_ZSDT0027_ZSDT0051
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr.
      TRY.
          IF wa_konv-kmein NE wa_vbap-vrkme.
            IF  wa_konv-kmein EQ 'TO'
            AND wa_vbap-vrkme EQ 'KG'.
*              wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd / 1000 ) * wa_saida_sd-kbetr .
              wa_saida_sd-vlr_saldo   = ( wa_saida_sd-saldo / 1000 ) * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_qtefat  = ( wa_saida_sd-qtefaturado / 1000 ) * wa_saida_sd-kbetr.

            ELSEIF wa_konv-kmein EQ 'KG'
            AND    wa_vbap-vrkme EQ 'TO'.
*              wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd * 1000 ) * wa_saida_sd-kbetr .
              wa_saida_sd-vlr_saldo   = ( wa_saida_sd-saldo * 1000 ) * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_qtefat  = ( wa_saida_sd-qtefaturado * 1000 ) * wa_saida_sd-kbetr.

            ELSE.
*              wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * wa_saida_sd-kbetr .
              wa_saida_sd-vlr_saldo   = wa_saida_sd-saldo * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_qtefat  = wa_saida_sd-qtefaturado * wa_saida_sd-kbetr.

            ENDIF.

          ELSE.
*            wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * wa_saida_sd-kbetr .
            wa_saida_sd-vlr_saldo   = wa_saida_sd-saldo * wa_saida_sd-kbetr.
            wa_saida_sd-vlr_qtefat  = wa_saida_sd-qtefaturado * wa_saida_sd-kbetr.

          ENDIF.

        CATCH cx_root.
** Tratada excessões ocorrida por valores errados lançados
      ENDTRY.


      "Para não trazer ordens de transferência
      READ TABLE it_vbep2 WITH KEY vbeln = wa_vbak-vbeln
                                   posnr = wa_vbap-posnr
                                   lifsp = '12'.
      IF sy-subrc IS INITIAL.
        wa_saida_sd-qtd = 0.
      ENDIF.

*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 <<< END

      IF wa_saida_sd-qtd GT 0.
        APPEND wa_saida_sd TO it_saida_sd.
      ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 13.08.2013 <<< END
      CLEAR: wa_vbfa_tot,
             wa_vbfa_tot-totalmenge,
             it_vbfa_tot2-totalmenge,
             wa_vbap,
             wa_vbfa,
             wa_kna1,
             wa_mara,
             wa_tvzbt,
             wa_saida_sd.

    ENDLOOP.

    CLEAR: wa_vbap,
           wa_vbfa,
           wa_kna1,
           wa_konv,
           wa_vbkd,
           wa_saida_sd.

  ENDLOOP.

ENDFORM.                    " FORM_SAIDA_SD

*&---------------------------------------------------------------------*
*&      Form  FORM_ALV_SD 47
*&---------------------------------------------------------------------*
FORM form_alv_sd.
  PERFORM alv_preenche_cat USING:
       'NAME1'              text-011      '35'      ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Cliente
       'VKBUR'              text-012      '4'       ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Escritorio de vendas
       'BEZEI'              text-062      '31'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Desc. Esc. Vendas
       'REGIONAL'           text-061      '4'       ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Regional
       'DOC_SIMULACAO'      text-056      '13'      ' ' 'X'    'IT_SAIDA_SD'  ''  ''          ''    , " Doc de Simulação
       'LIFSP'              text-041      '4'       ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Codigo Bloqueio de Item de O.V.
       'LIFSP_T'            text-092      '30'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Desc. Bloqueio
       'AUART'              text-058      '31'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''    , " Tp. Ordem
       'VBELN'              text-013      '10'      ' ' 'X'    'IT_SAIDA_SD'  ''  ''          ''    , " Contrato
       'POSNR'              text-034      '6'       ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Contrato
       'MATNR'              text-014      '6'       'X' ' '    'IT_SAIDA_SD'  ''  ''          'MATN1' , " Material
       'WRKST'              text-044      '10'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Marca
       'MATKL'              text-101      '14'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Grp.Merc
       'STATUS'             text-042      '2'       'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Status
       'ZTERM'              text-043      '30'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Cond.Pgto.
       'VTEXT'              text-045      '30'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , "Den. cond.pagamento
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 101103 - Ajuste_Relatório_ZSDT0027_ZSDT0051
* inserindo: VBKD-INCO1 / VBKD-INCO2
       'INCO1'              text-046      '10'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , "INCO1
       'INCO2'              text-047      '28'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , "INCO2
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END
       'ARKTX'              text-037      '40'      'X' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Material
       'QTD'                text-015      '10'      ' ' ' '    'IT_SAIDA_SD'  'X' 'C500'      ''   , " Qte.Faturado
       'UNID'               text-035      '03'      ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Qte.Faturado
       'QTEFATURADO'        text-016      '10'      ' ' 'X'    'IT_SAIDA_SD'  'X' 'C500'      ''   , " Qte.Faturado
       'SALDO'              text-033      '10'      ' ' ' '    'IT_SAIDA_SD'  'X' 'C300'      ''   , " Qte.Faturado
       'QTE_SOL'            text-063      '31'      ''  ''     'IT_SAIDA_SD'  'X' 'C300'      ''   , " Vol. Liberado
       'WAERS'              text-038      '3'       ' ' ' '    'IT_SAIDA_SD'  ''  ''          ''    , " Moeda
       'KBETR'              text-039      ''        ''  ''     'IT_SAIDA_SD'  'X'  'C500'     ''   , " Valor Unitario.
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 >>> INI
* CH 100102 - Ajuste_Relatório_ZSDT0027_ZSDT0051
       'VLR_QTECONT'        text-049      ''        ''  ''     'IT_SAIDA_SD'  'X'  ''         '', " Valor Qte Contratada.
       'VLR_TOTBRT '        text-099      ''        ''  ''     'IT_SAIDA_SD'  'X'  ''         '', " Valor Tot.
       'VLR_QTEFAT'         text-050      ''        ''  ''     'IT_SAIDA_SD'  'X'  ''         '', " Valor Qte Faturada.
       'VLR_SALDO'          text-051      ''        ''  ''     'IT_SAIDA_SD'  'X'  ''         '', " Valor Saldo.
       'VLR_TOTBRT_FAT'     text-100      ''        ''  ''     'IT_SAIDA_SD'  'X'  ''         '', " Vlr.Total Bruto Faturado.
       'KURRF'              text-052      '13'      ''  ''     'IT_SAIDA_SD'  ''   ''         '', " Taxa Hedge
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 <<< END
       'VALDT'              text-040      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Data de Vencimento

       'AUGDT'              text-064      '10'     ' ' ' '     'IT_SAIDA_SD'  ''   ''         '', " Dt. Compensação        "Novo
       'NRPROPLMT'          text-065      '30'     ' ' ' '     'IT_SAIDA_SD'  ''   ''         '', " Nº Proposta Limite     "Novo
       'ALCPROPLMT'         text-066      '30'     ' ' ' '     'IT_SAIDA_SD'  ''   ''         '', " Alçada Prop. Limite    "Novo
       'NRPROPOPR'          text-067      '30'     ' ' ' '     'IT_SAIDA_SD'  ''   ''         '', " Nº Proposta Operação   "Novo
       'ALCPROPOPR'         text-068      '30'     ' ' ' '     'IT_SAIDA_SD'  ''   ''         '', " Alçada Prop. Operação  "Novo
       'EFETIVADA'          text-091      '30'     ' ' ' '     'IT_SAIDA_SD'  ''   ''         '', " Efetivada              "Novo

       'ERDAT'              text-053      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Data de Vencimento
       'SAFRA'              text-054      '06'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Safra
       'CULTURA'            text-055      '30'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Cultura
       'SAFRA_APL'          text-060      '31'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Safra de Aplicação
       'CULTURA_APL'        text-059      '31'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''   , " Cultura de Aplicação

       'VLR_FRETE'          text-093      '15'      ''  ''     'IT_SAIDA_SD'  ''   ''         '', " Vl. Frete
       'J_1BCFOP'           text-094      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         '', " CFOP
       'J_1BTXSDC'          text-095      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         '', " Cód Imp.
       'KBETR2'             text-096      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         '', " Vl. Unit Bt
       'WERKS'              text-097      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         '', " Centro
       'LGORT'              text-098      '10'      ''  ''     'IT_SAIDA_SD'  ''   ''         ''. " Deposito


ENDFORM.                    " FORM_ALV_SD

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    VALUE(p_flag)
                           VALUE(p_fnam)
                           VALUE(p_fval).
  CLEAR t_bdc.

  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.

  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD


*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT


*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
  wa_layout2-zebra = 'X'.
ENDFORM.                    " Z_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_zero  TYPE c
                               p_hot   TYPE c
                               p_saida TYPE c
                               p_soma  TYPE c
                               p_cor   TYPE c
                               p_convexit TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = p_saida .
  wl_fcat-fieldname = p_campo .
  wl_fcat-scrtext_l = p_desc  .
  wl_fcat-scrtext_m = p_desc  .
  wl_fcat-scrtext_s = p_desc  .
  wl_fcat-hotspot   = p_hot   .
  wl_fcat-no_zero   = p_zero  .
  wl_fcat-outputlen = p_tam   .
  wl_fcat-do_sum    = p_soma  .
  wl_fcat-emphasize = p_cor   .
  wl_fcat-convexit  = p_convexit.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA:wa_event  TYPE REF TO lcl_event_receiver,
     wa_event2 TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,

      zm_handle_hotspot2 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive                   ,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_receiver
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

  METHOD: zm_handle_hotspot2.
    PERFORM z_handle_hotspot2 USING    e_row_id
                                       e_column_id
                                       es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot2

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT .
  gs_variant_c-variant = p_varia.
  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.


  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  wa_layout-cwidth_opt = 'X'.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida_sd
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF ( sy-dynnr EQ '0100' ) OR ( sy-dynnr EQ '0200' ).
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS: c_button_normal           TYPE i VALUE 0,
             c_menu_and_default_button TYPE i VALUE 1,
             c_menu                    TYPE i VALUE 2,
             c_separator               TYPE i VALUE 3,
             c_radio_button            TYPE i VALUE 4,
             c_checkbox                TYPE i VALUE 5,
             c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm       .

  CASE p_ucomm.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD wa_alv->refresh_table_display .
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id
                                p_e_column_id
                                p_es_row_no.

  DATA opt TYPE ctu_params.

  READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.

  CASE p_e_column_id.

    WHEN 'VBELN'. " Contrato
      SET PARAMETER ID 'AUN' FIELD wa_saida_sd-vbeln.
      CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.

    WHEN 'DOC_SIMULACAO'. "Numero do documento de simulação de venda
      IF wa_saida_sd-doc_simulacao IS NOT INITIAL.
        CLEAR: t_bdc[].

        PERFORM z_preencher_dynpro USING:
                    'X' 'ZSDR016'                      '0100',
                    ' ' 'WG_HEADER-DOC_SIMULACAO'      wa_saida_sd-doc_simulacao,
                    ' ' 'BDC_OKCODE'                   'ATUAL'.

        opt-dismode = 'E'.
        opt-defsize = ' '.

        CALL TRANSACTION 'ZSDT0044' USING t_bdc OPTIONS FROM opt.

      ENDIF.
    WHEN 'QTEFATURADO'.


      CLEAR : wa_saida_sd2,
              it_saida_sd2,
              it_fcat2.

      DATA: day(2)    TYPE c,
            month(2)  TYPE c,
            year(4)   TYPE c,
            dta(8)    TYPE c,
            vg_refkey TYPE j_1bnflin-refkey,
            it_vbfa_2 TYPE STANDARD TABLE OF vbfa,
            wa_vbfa_2 TYPE vbfa.

      SORT:
            it_j_1bnflin BY refkey itmnum,
            it_j_1bnfdoc BY docnum,
            it_vbfa BY vbelv posnn,
            it_vbrk      BY vbeln.

      SELECT *
        FROM vbfa
        INTO TABLE it_vbfa_2
        FOR ALL ENTRIES IN it_vbfa
        WHERE vbeln EQ it_vbfa-vbeln.

*      LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV EQ WA_SAIDA_SD-VBELN AND POSNN EQ WA_SAIDA_SD-POSNR . " Dados da Fatura
      LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ wa_saida_sd-vbeln AND posnv EQ wa_saida_sd-posnr . " Dados da Fatura
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF wa_vbrk-fkart = 'ZTRI'.
          CONTINUE.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vbfa-vbeln
          IMPORTING
            output = wa_vbfa-vbeln.
        vg_refkey = wa_vbfa-vbeln.

        READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = vg_refkey
                                                           itmnum = wa_vbfa-posnn BINARY SEARCH.

        READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.

        IF NOT wa_j_1bnfdoc-nfe = 'X'.
          MOVE wa_j_1bnfdoc-nfnum TO wa_j_1bnfdoc-nfenum.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_j_1bnfdoc-nfenum
            IMPORTING
              output = wa_j_1bnfdoc-nfenum.
        ENDIF.

        CLEAR: wa_saida_sd2-vbeln3.

        IF wa_vbfa-vbtyp_n EQ 'O'.

          READ TABLE it_vbfa_2 INTO wa_vbfa_2 WITH KEY vbeln   = wa_vbfa-vbeln
                                                       posnn   = wa_vbfa-posnn
                                                       vbtyp_n = wa_vbfa-vbtyp_n
                                                       vbtyp_v = 'H'.

          IF sy-subrc IS INITIAL.
            wa_saida_sd2-vbeln3  = wa_vbfa_2-vbelv.
          ENDIF.

        ELSEIF wa_vbfa-vbtyp_n EQ 'N'.

          READ TABLE it_vbfa_2 INTO wa_vbfa_2 WITH KEY vbeln   = wa_vbfa-vbeln
                                                       posnn   = wa_vbfa-posnn
                                                       vbtyp_n = wa_vbfa-vbtyp_n
                                                       vbtyp_v = 'M'.

          IF sy-subrc IS INITIAL.
            wa_saida_sd2-vbeln3  = wa_vbfa_2-vbelv.
          ENDIF.

        ENDIF.

        wa_saida_sd2-name12  = wa_saida_sd-name1.
        wa_saida_sd2-vbeln2  = wa_vbfa-vbelv.
        wa_saida_sd2-vbelnk  = wa_vbfa-vbeln.
        wa_saida_sd2-docnum2 = wa_j_1bnflin-docnum.
        wa_saida_sd2-nfenum  = wa_j_1bnfdoc-nfenum.
        wa_saida_sd2-vrkme   =  wa_saida_sd-unid.

        day   = ''.
        month = ''.
        year  = ''.
        dta   = ''.

        day   =  wa_vbfa-erdat+6(2) .
        month =  wa_vbfa-erdat+4(2) .
        year  =  wa_vbfa-erdat(4) .

        CONCATENATE year month day INTO dta.

        wa_saida_sd2-erdat2 =    dta.       "Dt.Emissão


        IF wa_vbfa-vbtyp_n = 'M'.
          wa_saida_sd2-rfwrt2 =  wa_vbfa-rfwrt.   " Valor Nota
          wa_saida_sd2-qtdfatur =   wa_j_1bnflin-menge.      " Qte.Faturado
        ELSEIF ( wa_vbfa-vbtyp_n = 'N' ) OR ( wa_vbfa-vbtyp_n = 'O') .
          wa_saida_sd2-rfwrt2 =  - wa_vbfa-rfwrt.   " Valor Nota
          wa_saida_sd2-qtdfatur =  - wa_j_1bnflin-menge.      " Qte.Faturado
        ENDIF.

        APPEND wa_saida_sd2 TO it_saida_sd2.

      ENDLOOP.
      PERFORM form_alv_sd2.

      CALL SCREEN 0200.

  ENDCASE.
ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  z_handle_hotspot2
*&---------------------------------------------------------------------*
FORM z_handle_hotspot2  USING   p_e_row_id
                                p_e_column_id
                                p_es_row_no.

  READ TABLE it_saida_sd2 INTO wa_saida_sd2 INDEX p_e_row_id.
  CASE p_e_column_id.

    WHEN 'VBELN2'. " Contrato
      SET PARAMETER ID 'AUN' FIELD wa_saida_sd2-vbeln2.
      CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.

    WHEN 'VBELNK'. " Doc Fatura
      SET PARAMETER ID 'VF' FIELD wa_saida_sd2-vbelnk.
      CALL TRANSACTION  'VF03' AND SKIP FIRST SCREEN.

    WHEN 'DOCNUM2'. " doc. fiscal
      SET PARAMETER ID 'JEF' FIELD wa_saida_sd2-docnum2.
      CALL TRANSACTION  'J1B3N' AND SKIP FIRST SCREEN.

  ENDCASE.
ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  form_alv_sd2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM form_alv_sd2.

  PERFORM alv_preenche_cat_mm USING:

  'NAME12'     text-025      '35'      ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'VBELN2'     text-026      '10'      ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'VBELN3'     text-057      '10'      ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'VBELNK'     text-027      '10'      ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'DOCNUM2'    text-028      '10'      ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'NFENUM'     text-029      '9'       ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'ERDAT2'     text-030      '10'      ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'QTDFATUR'   text-031      '15'      ' ' ' '     'IT_SAIDA_SD2' 'X'   'C500',
  'VRKME'      text-035      '3'       ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'RFWRT2'     text-032      '15'      ' '  ''     'IT_SAIDA_SD2' 'X'   'C500'.

ENDFORM.                    " FORM_ALV_mm2


*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_exibe_alv_mm OUTPUT .

  IF wa_cont2 IS INITIAL.

    CREATE OBJECT wa_cont2
      EXPORTING
        container_name              = 'CC_ALV_MM2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.


  IF wa_alv2 IS INITIAL AND NOT
    wa_cont2 IS INITIAL.

    CREATE OBJECT wa_alv2
      EXPORTING
        i_parent          = wa_cont2
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event2 IS INITIAL.

    CREATE OBJECT wa_event2.
    SET HANDLER: wa_event2->zm_handle_toolbar  FOR wa_alv2.
    SET HANDLER: wa_event2->zm_handle_hotspot2 FOR wa_alv2.
    SET HANDLER: wa_event2->zm_handle_user_command FOR wa_alv2.

  ENDIF.

  CALL METHOD wa_alv2->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout2
*     is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida_sd2
      it_fieldcatalog               = it_fcat2
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.



  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv2 IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat_mm
*&---------------------------------------------------------------------*
FORM alv_preenche_cat_mm  USING   p_campo TYPE c
                                  p_desc  TYPE c
                                  p_tam   TYPE c
                                  p_zero  TYPE c
                                  p_hot   TYPE c
                                  p_saida TYPE c
                                  p_soma  TYPE c
                                  p_cor   TYPE c.

  DATA: wl_fcat2 TYPE lvc_s_fcat.

  wl_fcat2-tabname   = p_saida .
  wl_fcat2-fieldname = p_campo .
  wl_fcat2-scrtext_l = p_desc  .
  wl_fcat2-scrtext_m = p_desc  .
  wl_fcat2-scrtext_s = p_desc  .
  wl_fcat2-hotspot   = p_hot   .
  wl_fcat2-no_zero   = p_zero  .
  wl_fcat2-outputlen = p_tam   .
  wl_fcat2-do_sum = p_soma     .
  wl_fcat2-emphasize = p_cor.

  APPEND wl_fcat2 TO it_fcat2.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_ESC_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->C_ERRO     text
*----------------------------------------------------------------------*
FORM f_validar_esc_venda CHANGING c_erro  TYPE sy-subrc.
  DATA: tl_0060 TYPE TABLE OF zsdt0060 WITH HEADER LINE,
        lv_erro TYPE i.

  CLEAR c_erro.

  SELECT *
    INTO TABLE tl_0060
    FROM zsdt0060
   WHERE usnam    EQ sy-uname
     AND programa EQ 'ZSDR016'
     AND vkbur    IN p_escven.

  IF tl_0060[] IS NOT INITIAL.
    LOOP AT p_escven.
      READ TABLE tl_0060 WITH KEY vkbur = p_escven-low.
      IF sy-subrc IS NOT INITIAL.
        ADD 1 TO lv_erro.
      ENDIF.
    ENDLOOP.

  ELSE.
    ADD 1 TO lv_erro.
  ENDIF.

  CHECK lv_erro IS NOT INITIAL.
  MESSAGE 'Sem permissão para visualizar vendas do escritório' TYPE 'I'.
  c_erro = lv_erro.

ENDFORM.                    "F_VALIDAR_ESC_VENDA

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
FORM z_preencher_dynpro    USING l_start TYPE c l_name TYPE c l_value.
  MOVE l_start TO wa_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wa_bdc-program,
  l_value TO wa_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wa_bdc-fnam,
      l_value TO wa_bdc-fval.
  ENDIF.
  APPEND wa_bdc TO t_bdc.
  CLEAR: wa_bdc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_ZSDT0051
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_zsdt0051 .

  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.
*-------------------------------------------
* variaveis locais
*-------------------------------------------
  DATA: so_data            TYPE RANGE OF mkpf-budat,
        wa_data            LIKE LINE OF so_data,
        lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.

*-------------------------------------------
* execute ZSDT0051
*-------------------------------------------
  SUBMIT zsdr0018 WITH mm       EQ ''
                  WITH sd       EQ abap_true
                  WITH p_tpcont IN p_tpcont
                  WITH p_cont   IN p_cont
                  WITH p_simula IN p_simula
                  WITH p_orgven IN p_orgven
                  WITH p_cdist  IN p_cdist
                  WITH p_sativ  IN p_sativ
                  WITH p_escven IN p_escven
                  WITH p_clien  IN p_clien
                  WITH p_datent IN p_datent
                  WITH p_fatuv  IN p_fatuv
                  WITH p_cent   IN p_cent
                  WITH p_mater  IN p_mater
                  WITH p_grupo  IN p_grupo
                  WITH p_waerks IN p_waerks
                  WITH p_call   = abap_true
                  AND RETURN.

ENDFORM.

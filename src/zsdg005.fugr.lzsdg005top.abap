FUNCTION-POOL zsdg005.                      "MESSAGE-ID ..

* INCLUDE LZSDG005D...                       " Local class definition


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
  vbfa,           " Fluxo de documentos de vendas e distribuição
  kna1,           " Mestre de clientes (parte geral)
  mara,
  j_1bnfe_active, " Electronic Nota Fiscal: Actual Status

  indx.           " Tabela de sistema índice

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
    aedat TYPE ekko-aedat,
    angnr TYPE ekko-angnr,
    ihran TYPE ekko-ihran,
    inco1 TYPE ekko-inco1,
    ihrez TYPE ekko-ihrez,
    zterm TYPE ekko-zterm,
    submi TYPE ekko-submi,
  END OF ty_ekko,

  BEGIN OF ty_ekbe,        " Histórico para o documento de compra
    ebeln TYPE ekbe-ebeln, " Nº do documento de compras
    ebelp TYPE ekbe-ebelp, " Nº item do documento de compra
    zekkn TYPE ekbe-zekkn,
    vgabe TYPE ekbe-vgabe,
    gjahr TYPE ekbe-gjahr,
    belnr TYPE ekbe-belnr, " Nº documento de material
    buzei TYPE ekbe-buzei,
    budat TYPE ekbe-budat, " Data de lançamento no documento
    menge TYPE ekbe-menge, " Quantidade
    dmbtr TYPE ekbe-dmbtr, " Montante em moeda interna
    shkzg TYPE ekbe-shkzg, " Código débito/crédito
  END OF ty_ekbe,

  BEGIN OF ty_ekbe2,       " Histórico para o documento de compra
    ebeln   TYPE ekbe-ebeln, " Nº do documento de compras
    ebelp   TYPE ekbe-ebelp, " Nº item do documento de compra
    belnr   TYPE ekbe-belnr, " Nº documento de material
    budat   TYPE ekbe-budat, " Data de lançamento no documento
    menge   TYPE ekbe-menge, " Quantidade
    menge_a TYPE ekbe-menge, " Quantidade
    dmbtr   TYPE ekbe-dmbtr, " Montante em moeda interna
    shkzg   TYPE ekbe-shkzg, " Código débito/crédito
    gjahr   TYPE ekbe-gjahr,
  END OF ty_ekbe2,

  BEGIN OF ty_ekpo,        " Item do documento de compras
    ebeln TYPE ekpo-ebeln, " Nº do documento de compras
    ebelp TYPE ekpo-ebelp, " Nº item do documento de compra
    matnr TYPE ekpo-matnr, " Nº do material
    txz01 TYPE ekpo-txz01, " Texto breve
    menge TYPE ekpo-menge, " Quantidade do pedido
    meins TYPE ekpo-meins,
    loekz TYPE ekpo-loekz, " Código de eliminação no documento de compras
    matkl TYPE ekpo-matkl, " Grupo de mercadorias
    mwskz TYPE ekpo-mwskz,
    bprme TYPE ekpo-bprme,
    elikz TYPE ekpo-elikz,
    peinh TYPE ekpo-peinh,
    werks TYPE ekpo-werks,
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
  END OF ty_mara,

  BEGIN OF ty_saida,              " Saida
    lifnr        TYPE lfa1-lifnr,
    name1        TYPE lfa1-name1,                                    " Nome 1
    ebeln        TYPE ekko-ebeln,        " Pedido
    ebelp        TYPE ekpo-ebelp,        " Item
    matnr        TYPE ekpo-matnr,        " Nº do material
    angnr        TYPE ekko-angnr,        " Taxa Redge
    ihran        TYPE ekko-ihran,
    menge        TYPE ekpo-menge, " Quantidade do pedido
    qtefaturado  TYPE ekpo-menge, " XTOT_FATURAMM (Habilitar link “Relatório 2”)
    saldo        TYPE ekpo-menge, " (Qte.Pedido – Qte.Faturado)
    descmat(100) TYPE c,
    qtd          TYPE ekpo-menge,
    unid         TYPE ekpo-meins,
    s_pedido(20) TYPE c,
    txz01        TYPE ekpo-txz01,
    waers        TYPE konv-waers,
    kbetr        TYPE konv-kbetr,
    kbetr_tot    TYPE komp-netwr,
    kbetr_imp    TYPE komp-netwr,
    aedat        TYPE c LENGTH 10,
    erdat        TYPE c LENGTH 10,
    vbeln        TYPE vbak-vbeln,
    mwskz        TYPE ekpo-mwskz,
    inco1        TYPE ekko-inco1,
    werks        TYPE ekpo-werks,
    ihrez        TYPE ekko-ihrez,
    unid_p       TYPE ekpo-bprme,
    kbetr_ped    TYPE komp-netwr,
    kbetr_fat    TYPE komp-netwr,
    kbetr_sal    TYPE komp-netwr,
    matkl        TYPE ekpo-matkl,
    zterm        TYPE ekko-zterm,
    submi        TYPE ekko-submi,
    bsart        TYPE ekko-bsart,
    netpr_germ   TYPE zmmt0037-netpr_germ,
    netpr_roya   TYPE zmmt0037-netpr_roya,
  END OF ty_saida,

  BEGIN OF ty_0035,
    safra      TYPE zmmt0035-safra,
    lifnr      TYPE lfa1-lifnr,
    ebeln      TYPE zmmt0035-ebeln,
    werks      TYPE zmmt0035-werks,
    banfn      TYPE zmmt0035-banfn,
    nro_sol_cp TYPE zmmt0035-nro_sol_cp,
    ped_forn   TYPE zmmt0035-ped_forn,
    waers      TYPE zmmt0035-waers,
  END OF ty_0035,

  BEGIN OF ty_0036,
    nro_sol_cp TYPE zmmt0035-nro_sol_cp,
    cont       TYPE sy-tabix,
  END OF ty_0036,

  BEGIN OF ty_0037,
    nro_sol_cp TYPE zmmt0037-nro_sol_cp,
    ebelp      TYPE zmmt0037-ebelp,
    ebeln      TYPE zmmt0037-ebeln,
    netpr_germ TYPE zmmt0037-netpr_germ,
    netpr_roya TYPE zmmt0037-netpr_roya,
  END OF ty_0037,

  BEGIN OF ty_saida_sd,            " Saida
    name1          TYPE kna1-name1,         " Cliente 35
    vkbur          TYPE vbak-vkbur,         " Escr.Vendas 4
    regional       TYPE zsdt0270-regional,  " Regional
    vbeln          TYPE vbak-vbeln,         " Contrato 10
    matnr          TYPE vbap-matnr,         " Material 18
    angnr          TYPE ekko-angnr,         " Taxa Redge
    ihran          TYPE ekko-ihran,
    status,
    zterm(30),
    vtext          TYPE tvzbt-vtext,  " Denominação da condição de pagamento
    inco1          TYPE vbkd-inco1,
    inco2          TYPE vbkd-inco2,
    kurrf          TYPE vbkd-kurrf,
    descmat(60)    TYPE c,            " Descricao Material
    wrkst          TYPE mara-wrkst,
    ntgew          TYPE vbap-ntgew,         " Qte.Contrato 15
    qtefaturado    TYPE db20199vp,  " Quantidade Faturado 10
    vlr_qtecont    TYPE konv-kbetr,   " Valor Qte. Contratada
    vlr_qtefat     TYPE konv-kbetr,   " Valor Qte. Faturada
    vlr_saldo      TYPE konv-kbetr,   " Valor Saldo
    saldo          TYPE db20199vp,                               " Saldo '10
    posnr          TYPE vbap-posnr,                              " Item. 6
    kwmeng         TYPE vbap-kwmeng, " Quantidade da ordem acumulada em unidade de venda 15
    qtd            TYPE db20199vp, " 10
    unid           TYPE vbap-vrkme, "3
    arktx          TYPE vbap-arktx,
    waers          TYPE konv-waers,
    kbetr          TYPE konv-kbetr,
    valdt          TYPE vbkd-valdt,
    erdat          TYPE vbak-erdat,   "Data de criação
    safra          TYPE ajahr,
    cultura        TYPE acc_txtlg,
    safra_apl      TYPE ajahr,
    cultura_apl    TYPE acc_txtlg,
    bezei          TYPE tvkbt-bezei,     " Desc. Esc. Vendas
    qte_sol        TYPE db20199vp, "zsdt0082-qte_sol, "Vol. Liberado "bug 61203
    lifsp_t        TYPE tvlst-vtext, "Desc. Bloqueio
    augdt          TYPE bsad-augdt,      "Dt. Compensação
    nrpropopr(60)  TYPE c, "Nº Proposta Operação
    alcpropopr(60) TYPE c, "Alçada Prop. Operação
    efetivada(60)  TYPE c, "Efetivada
    nrproplmt(60)  TYPE c, "Nº Proposta Limite
    alcproplmt(60) TYPE c, "Alçada Prop. Limite
    lifsp          TYPE vbep-lifsp,
    doc_simulacao  TYPE zsdt0041-doc_simulacao,
    vlr_frete      TYPE zsdt0041-vlr_frete,
    auart          TYPE vbak-auart,
    j_1bcfop       TYPE vbap-j_1bcfop,
    kbetr2         TYPE konv-kbetr,
    j_1btxsdc      TYPE vbap-j_1btxsdc,
  END OF ty_saida_sd,

  BEGIN OF ty_saida_mm2,             " Saida
    name12  TYPE  lfa1-name1,      " Nº conta do fornecedo
    ebeln2  TYPE ekko-ebeln,       " Nº do documento de compras
    belnr2  TYPE ekbe-belnr ,      " Nº documento de material
    docnum2 TYPE j_1bnflin-docnum, " Nº documento
    nfenum2 TYPE j_1bnfdoc-nfenum, " Nº documento de referência
    budat   TYPE ekbe-budat, " Data de criação do registro
    menge   TYPE ekbe-menge,  " Quantidade referenciada em unidade medida básica
    angnr   TYPE ekko-angnr,        " Taxa Redge
    ihran   TYPE ekko-ihran,
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
    angnr    TYPE ekko-angnr,        " Taxa Redge
    ihran    TYPE ekko-ihran,
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
    vgbel TYPE vbak-vgbel, " Nº condição do documento
    erdat TYPE vbak-erdat,
    lifsk TYPE vbak-lifsk,
    faksk TYPE vbak-faksk,
  END OF ty_vbak,

  BEGIN OF ty_vbap,          " Documento de vendas: dados de item
    vbeln     TYPE vbap-vbeln,   " Documento de vendas
    matnr     TYPE vbap-matnr,   " Nº do material - (Material)
    arktx     TYPE vbap-arktx,   " Texto breve do item da ordem do cliente -(Descrição do Material)
    werks     TYPE vbap-werks,   " Centro (próprio ou externo)
    ntgew     TYPE vbap-ntgew,   " Peso líquido do item
    gewei     TYPE vbap-gewei,   " UM qtd.prevista
    posnr     TYPE vbap-posnr,   " Item do documento de vendas
    kwmeng    TYPE vbap-kwmeng,  " Quantidade da ordem acumulada em unidade de venda
    vrkme     TYPE vbap-vrkme,   "Data de criação
    j_1bcfop  TYPE vbap-j_1bcfop, "CFOP
    j_1btxsdc TYPE vbap-j_1btxsdc, "Cód Imposto
  END OF ty_vbap,

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
    name1 TYPE kna1-name1,                                   " Nome 1
    kunnr TYPE kna1-kunnr, " Nº cliente 1
    stcd1 TYPE kna1-stcd1,                                  "pbi 55017
    stcd2 TYPE kna1-stcd2,
    stcd3 TYPE kna1-stcd3,                                 "pbi 55017
  END OF ty_kna1,

  BEGIN OF ty_j_1bnfe_active,          " Electronic Nota Fiscal: Actual Status
    docnum TYPE j_1bnfe_active-docnum, " Nº documento
    nfnum9 TYPE j_1bnfe_active-nfnum9, " Nº NF-e de nove posições
  END OF ty_j_1bnfe_active,

  BEGIN OF ty_vbfa_tot,           " Fluxo de documentos de vendas e distribuição
    vbelnrfmg  TYPE vbfa-vbelv, " Documento de vendas e distribuição precedente
    "  totalrfmng TYPE vbfa-rfmng, " Documento de vendas e distribuição subseqüente
    totalmenge TYPE j_1bnflin-menge, " Documento de vendas e distribuição subseqüente
    posnn      TYPE vbfa-posnn,
    totaldim   TYPE j_1bnflin-menge, "Total de Recusa e Complemento
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

  BEGIN OF ty_vbep,
    vbeln TYPE vbep-vbeln,
    posnr TYPE vbep-posnr,
    etenr TYPE vbep-etenr,
    lifsp TYPE vbep-lifsp,
  END OF ty_vbep,

  BEGIN OF ty_vbkd,
    vbeln TYPE vbkd-vbeln,
    valdt TYPE vbkd-valdt,
  END OF ty_vbkd,

  BEGIN OF ty_vbrk,
    vbeln TYPE vbrk-vbeln,
    fkart TYPE vbrk-fkart,
    sfakn TYPE vbrk-sfakn,
  END OF ty_vbrk,

  BEGIN OF ty_zsdt0040,
    doc_simulacao TYPE zsded003,
    cultura       TYPE zsded001,
    safra         TYPE ajahr,
  END OF ty_zsdt0040,

  BEGIN OF ty_zsdt0041,
    doc_simulacao TYPE zsded003,
    vbeln         TYPE vbeln,
  END OF ty_zsdt0041,

  BEGIN OF ty_zsdt0041_safra,
    doc_simulacao TYPE zsded003,
    vbeln         TYPE vbeln,
    safra_apl     TYPE zsdt0041-safra_apl,
    cultura_apl   TYPE zsdt0041-cultura_apl,
  END OF ty_zsdt0041_safra,

  BEGIN OF ty_tvbur,
    vkbur TYPE tvbur-vkbur,
    vbeln TYPE vbak-vbeln,
    bezei TYPE tvkbt-bezei,
  END OF ty_tvbur,

  BEGIN OF ty_tvfs,
    faksp  TYPE tvfs-faksp,
    v_text TYPE tvfst-vtext,
  END OF ty_tvfs,

  BEGIN OF ty_tvls,
    lifsp  TYPE tvls-lifsp,
    v_text TYPE tvlst-vtext,
  END OF ty_tvls.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA:
  t_bdc             TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  it_saida          TYPE TABLE OF ty_saida,
  it_ekko           TYPE TABLE OF ty_ekko,
  it_0037           TYPE TABLE OF ty_0037,
  it_0035           TYPE TABLE OF ty_0035,
  it_0036           TYPE TABLE OF zmmt0036,
  it_0036_cont      TYPE TABLE OF ty_0036,
  it_ekko_aux       TYPE TABLE OF ty_ekko,
  it_ekbe           TYPE TABLE OF ty_ekbe,
  it_ekbe_zefi      TYPE TABLE OF ty_ekbe,
  it_ekbe2          TYPE TABLE OF ty_ekbe2,
  it_ekpo           TYPE TABLE OF ty_ekpo,
  it_rbkp           TYPE TABLE OF ty_rbkp,
  it_lfa1           TYPE TABLE OF ty_lfa1,
  it_ekbe_aux       TYPE TABLE OF ty_ekbe_aux,
  it_j_1bnflin      TYPE TABLE OF ty_j_1bnflin,
  it_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc,
  it_vbak           TYPE TABLE OF ty_vbak,
  it_tvbur          TYPE TABLE OF tvbur WITH HEADER LINE,
  it_tvkbt          TYPE TABLE OF tvkbt WITH HEADER LINE,
  it_tvls           TYPE TABLE OF tvls WITH HEADER LINE,
  it_tvlst          TYPE TABLE OF tvlst WITH HEADER LINE,
  it_tvfs           TYPE TABLE OF tvfs WITH HEADER LINE,
  it_tvfst          TYPE TABLE OF tvfst WITH HEADER LINE,
  it_vbak_est       TYPE TABLE OF ty_vbak,
  it_vbap           TYPE TABLE OF ty_vbap,
  it_zsdt0082       TYPE TABLE OF zsdt0082 WITH HEADER LINE,
  it_zsdt0270       TYPE STANDARD TABLE OF zsdt0270 WITH HEADER LINE,
  it_zsdt0271       TYPE STANDARD TABLE OF zsdt0271 WITH HEADER LINE,
  it_zsdt0272       TYPE STANDARD TABLE OF zsdt0272 WITH HEADER LINE,
  it_zsdt0273       TYPE STANDARD TABLE OF zsdt0273 WITH HEADER LINE,
  it_zsdt0274       TYPE TABLE OF zsdt0274 WITH HEADER LINE,
  it_zsdt0275       TYPE TABLE OF zsdt0275 WITH HEADER LINE,
  it_zsdt0090_safra TYPE TABLE OF zsdt0090 WITH HEADER LINE,
  it_zsdt0041_safra TYPE TABLE OF zsdt0041 WITH HEADER LINE,
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
  it_vbfa           TYPE TABLE OF ty_vbfa,
  it_vbfa_est       TYPE TABLE OF ty_vbfa,
  it_vbfa_aux2      TYPE TABLE OF ty_vbfa WITH HEADER LINE,
  it_vbrk           TYPE TABLE OF ty_vbrk,
  it_vbrk_est       TYPE TABLE OF ty_vbrk,
  it_vbrk_aux       TYPE TABLE OF ty_vbrk,
  it_vbfa_mm        TYPE TABLE OF ty_vbfa_mm,
  it_kna1           TYPE TABLE OF ty_kna1,
  it_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active,
  it_vbfa_tot       TYPE TABLE OF ty_vbfa_tot,
  it_vbfa_tot2      TYPE TABLE OF ty_vbfa_tot WITH HEADER LINE,
  it_vbfa_aux       TYPE TABLE OF ty_vbfa_aux,
  it_saida_sd       TYPE TABLE OF zsde009,
  it_saida_mm2      TYPE TABLE OF ty_saida_mm2,
  it_konv           TYPE TABLE OF ty_konv,
  it_konv_imp       TYPE TABLE OF ty_konv,
  it_vbkd           TYPE TABLE OF ty_vbkd,
  it_vbep           TYPE TABLE OF ty_vbep,
  it_vbep2          TYPE TABLE OF vbep    WITH HEADER LINE,
  it_saida_sd2      TYPE TABLE OF ty_saida_sd2,
  it_zsdt0040       TYPE TABLE OF ty_zsdt0040 WITH HEADER LINE,
  it_zsdt0041       TYPE TABLE OF ty_zsdt0041 WITH HEADER LINE,
  it_zsdt0041_frete TYPE STANDARD TABLE OF zsdt0041,
  it_zsdt0090_frete TYPE STANDARD TABLE OF zsdt0090,
  it_zsdt0038       TYPE TABLE OF zsdt0038 WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont           TYPE REF TO cl_gui_custom_container        , " Objeto Container
  wa_cont2          TYPE REF TO cl_gui_custom_container        , " Objeto Container
  wa_alv            TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
  wa_alv2           TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
  wa_layout         TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
  wa_layout2        TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
  wa_saida          TYPE ty_saida,
  wa_ekko           TYPE ty_ekko,
  wa_ekko_aux       TYPE ty_ekko,
  wa_0037           TYPE ty_0037,
  wa_0035           TYPE ty_0035,
  wa_0036           TYPE zmmt0036,
  wa_0036_cont      TYPE ty_0036,
  wa_ekbe           TYPE ty_ekbe,
  wa_ekbe_zefi      TYPE ty_ekbe,
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
  wa_vbrk           TYPE ty_vbrk,
  wa_vbrk_aux       TYPE ty_vbrk,
  wa_vbfa_mm        TYPE ty_vbfa_mm,
  wa_kna1           TYPE ty_kna1,
  wa_tvzbt          TYPE ty_tvzbt,
  wa_mara           TYPE ty_mara,
  wa_j_1bnfe_active TYPE ty_j_1bnfe_active,
  wa_vbfa_tot       TYPE ty_vbfa_tot,
  wa_saida_sd       TYPE ty_saida_sd,
  wa_saida_mm2      TYPE ty_saida_mm2,
  wa_saida_sd2      TYPE ty_saida_sd2,
  wa_konv           TYPE ty_konv,
  wa_vbkd           TYPE ty_vbkd,
  wa_vbep           TYPE ty_vbep,
  wa_vbfa_aux       TYPE ty_vbfa_aux,
  wa_bdc            TYPE bdcdata.

FIELD-SYMBOLS: <wmwst> TYPE any.
FIELD-SYMBOLS: <data> TYPE table.

*----------------------------------------------------------------------*
* ESTRUTURA ALV  - Tabela Estrutura Colunas do Relatório
*----------------------------------------------------------------------*
DATA:
  it_fcat      TYPE TABLE OF lvc_s_fcat,
  s_variant    TYPE disvariant,
  it_fcat2     TYPE TABLE OF lvc_s_fcat,
  r_ekpo_s     TYPE RANGE OF ekpo-loekz,
  wa_status    LIKE LINE OF  r_ekpo_s,
  gs_variant_c TYPE disvariant,
  verro(1).

DATA: vl_posnr TYPE vbap-posnr.
DATA: vl_vbeln TYPE vbap-vbeln.

RANGES:  rg_cpf_cnpj FOR  kna1-stcd1.

DATA: variante        LIKE disvariant.
DATA t_0035.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
               <fs_wa>,
               <fs_field>.
DATA: dyn_table TYPE REF TO data,
      dyn_line  TYPE REF TO data.

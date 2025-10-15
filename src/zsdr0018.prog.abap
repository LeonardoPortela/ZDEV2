*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Report  ZSDR0018                                                     *
* Descrição  : Relatorio Consulta de Entregas - INSUMOS x              *
* Módulo     : MM                               Transação: ZSDT0051    *
*                                                                      *
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2EK6 |07/02/2025 |Correção do DUMP em PRD no JOB &*
*&                                    |ZSDT0051 por conta de RANGES   &*
*&                                    |excedendo o limite máximo em   &*
*&                                    |cláusula de SELECT de dados.   &*
*&                                    |Chamado: 165866.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2EMI |07/02/2025 |Melhoria de performance de     &*
*&                                    |processamento de dados.        &*
*&                                    |Chamado: 160696.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MJ7 |11/06/2025 |Ajuste de DUMP em PRD. DUMP no &*
*&                                    |retorno do paralelismos que usa&*
*&                                    |uma parte da rotina que é para &*
*&                                    |processamento ONLINE.          &*
*&                                    |Chamado: 182012.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2PFX |24/07/2025 |Ajustes de Campos e Cálculos.  &*
*&                                    |Chamado: 184428.               &*
*&--------------------------------------------------------------------&*
REPORT zsdr0018_100925_jt.

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
  indx,           " Tabela de sistema índice
  zsdt0040,       " Doc Simulacao *-CS2021000218-14.09.2022-#90705-JT-inicio
  zfit0045.       " Solicitação de adiantamento

*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_ekko,        " Cabeçalho do documento de compra
    ebeln        TYPE ekko-ebeln, " Nº do documento de compras
    bsart        TYPE ekko-bsart, " Tipo de documento de compras
    lifnr        TYPE ekko-lifnr, " Nº conta do fornecedor
    bukrs        TYPE ekko-bukrs, " Empresa
    reswk        TYPE ekko-reswk, " Centro de saída estoque em caso de pedido transferência
    bedat        TYPE ekko-bedat,
    knumv        TYPE ekko-knumv,
    aedat        TYPE ekko-aedat,
    angnr        TYPE ekko-angnr,
    ihran        TYPE ekko-ihran,
    inco1        TYPE ekko-inco1,
    ihrez        TYPE ekko-ihrez,
    zterm        TYPE ekko-zterm,
    submi        TYPE ekko-submi,
    verkf        TYPE ekko-verkf,
    telf1        TYPE ekko-telf1,
    data_criacao TYPE zmmt0035-data_criacao,
    data_atual   TYPE zmmt0035-data_atual,
    nro_sol_cp   TYPE zmmt0035-nro_sol_cp,  "*-US192338-06.10.2025-#192338-JT
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
    rmwwr   TYPE rbkp-rmwwr,
  END OF ty_ekbe2,

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  BEGIN OF ty_rseg,
    belnr TYPE rseg-belnr,
    gjahr TYPE rseg-gjahr,
    ebeln TYPE rseg-ebeln,
    ebelp TYPE rseg-ebelp,
    shkzg TYPE rseg-shkzg,
    wrbtr TYPE rseg-wrbtr,
    buzei TYPE rseg-buzei,
  END OF ty_rseg,
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

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
    lgort TYPE ekpo-lgort,
    banfn TYPE ekpo-banfn,
    netpr TYPE ekpo-netpr,
  END OF ty_ekpo,

  BEGIN OF ty_ekpo_aux,        " Item do documento de compras
    ebeln TYPE ekpo-ebeln, " Nº do documento de compras
    ebelp TYPE ekpo-ebelp, " Nº item do documento de compra
    xped  TYPE j_1bnflin-xped,
  END OF ty_ekpo_aux,

  BEGIN OF ty_eban,
    banfn TYPE banfn,
    badat TYPE badat,
  END OF ty_eban,

  BEGIN OF ty_rbkp,        " Cabeçalho doc.da fatura recebida
    belnr  TYPE rbkp-belnr, " Nº documento de material
    gjahr  TYPE rbkp-gjahr, " Exercício
    xblnr  TYPE rbkp-xblnr, " Nº documento de referência
    bldat  TYPE rbkp-bldat, " Data de Emissão da Fatura
    rmwwr  TYPE rbkp-rmwwr, " Valor da Nota
    waers  TYPE rbkp-waers, " Tipo de moeda
    wmwst1 TYPE fwstev,    " Montante de imposto <<<------"160696 - NMS - FIM------>>>
  END OF ty_rbkp,

  BEGIN OF ty_lfa1,         " Mestre de fornecedores (parte geral)
    name1 TYPE lfa1-name1,                                    " Nome1
    lifnr TYPE lfa1-lifnr,  " Nº conta do fornecedor
    stcd1 TYPE lfa1-stcd1,  "CNPJ
    stcd2 TYPE lfa1-stcd2,  "CPF
    stkzn TYPE lfa1-stkzn,  "Pessoa física
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
    nfnet    TYPE j_1bnflin-nfnet,  " Valor do item
    xped     TYPE j_1bnflin-xped,  " Pedido
    nitemped TYPE j_1bnflin-nitemped, "Item pedido
  END OF  ty_j_1bnflin,

*<<<Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  BEGIN OF ty_lin_rseg,            " Partidas individuais da nota fiscal
    refkey TYPE j_1bnflin-refkey, " Referência ao documento de origem
    docnum TYPE j_1bnflin-docnum, " Nº documento
    itmnum TYPE j_1bnflin-itmnum, " Nº item do documento
  END OF  ty_lin_rseg,

  BEGIN OF ty_1bnfstx,
    docnum TYPE j_1bnfstx-docnum,
    itmnum TYPE j_1bnfstx-itmnum,
    taxval TYPE j_1bnfstx-taxval,
  END OF  ty_1bnfstx,

  BEGIN OF ty_rbkp_rseg,
    belnr TYPE rbkp-belnr,
    gjahr TYPE rbkp-gjahr,
    xblnr TYPE rbkp-xblnr,
    bldat TYPE rbkp-bldat,
    rmwwr TYPE rbkp-rmwwr,
    waers TYPE rbkp-waers,
    kursf TYPE rbkp-kursf,
  END OF  ty_rbkp_rseg,
*>>>End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

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
    matkl TYPE mara-matkl,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
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
    qtremfinal   TYPE ekpo-menge, " US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
    unid         TYPE ekpo-meins,
    s_pedido(20) TYPE c,
    txz01        TYPE ekpo-txz01,
    waers        TYPE konv-waers,
*    kbetr        TYPE konv-kbetr,
    kbetr        TYPE konv-kwert,
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
    vlremfinal   TYPE komp-netwr, " US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
    kbetr_sal    TYPE komp-netwr,
    matkl        TYPE ekpo-matkl,
    zterm        TYPE ekko-zterm,
    submi        TYPE ekko-submi,
    bsart        TYPE ekko-bsart,
    netpr_germ   TYPE zmmt0037-netpr_germ,
    netpr_roya   TYPE zmmt0037-netpr_roya,
    lgort        TYPE ekpo-lgort, "RJF
    verkf        TYPE ekko-verkf, "RJF
    telf1        TYPE ekko-telf1, "RSA
**  Begin of CS2022000771   #83814 FF   25.01.2023
    bedat        TYPE zsdt0090-dt_entrega, "c LENGTH 10, "Data pedido
    stcd1        TYPE lfa1-stcd1,
**<<<------"160696 - NMS - INI------>>>
*    adiantamento TYPE ekpo-netpr,
    adiantamento TYPE rlwrt,
**<<<------"160696 - NMS - FIM------>>>
    status       TYPE zfit0045-status,
    nro_sol      TYPE zfit0046-nro_sol,
    netpr_liq    TYPE ekpo-netpr,
    data_criacao TYPE c LENGTH 10,
    netpr_desc   TYPE zmmt0037-netpr_desc,
    netpr_supl   TYPE zmmt0037-netpr_supl,
    cultura      TYPE zmmt0035-cultura,       "**<<<------"184428 - NMS------>>>
    taxa_curva   TYPE zsdt0094-taxa_curva,    "**<<<------"184428 - NMS------>>>
    safra        TYPE zsdt0094-safra,         "**<<<------"184428 - NMS------>>>
    cultivar     TYPE char30,                 "**<<<------"184428 - NMS------>>> - Ajuste nome do campo GA - 18.09.2025
** End of FF  25.01.2023
  END OF ty_saida,

  BEGIN OF ty_0035,
    safra        TYPE zmmt0035-safra,
    lifnr        TYPE lfa1-lifnr,
    ebeln        TYPE zmmt0035-ebeln,
    werks        TYPE zmmt0035-werks,
    banfn        TYPE zmmt0035-banfn,
    nro_sol_cp   TYPE zmmt0035-nro_sol_cp,
    ped_forn     TYPE zmmt0035-ped_forn,
    waers        TYPE zmmt0035-waers,
    data_atual   TYPE zmmt0035-data_atual,
    data_criacao TYPE zmmt0035-data_criacao,
    cultura      TYPE zmmt0035-cultura,       "**<<<------"184428 - NMS------>>>
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
    netpr_desc TYPE zmmt0037-netpr_desc,
    netpr_supl TYPE zmmt0037-netpr_supl,
  END OF ty_0037,

  BEGIN OF ty_saida_sd,            " Saida
    name1             TYPE kna1-name1,         " Cliente 35
    vkbur             TYPE vbak-vkbur,         " Escr.Vendas 4
    regional          TYPE zsdt0270-regional,  " Regional
    " cod_regional   TYPE zsdt0270-cod_regional,  " Codigo Regionaç
    vbeln             TYPE vbak-vbeln,         " Contrato 10
    matnr             TYPE vbap-matnr,         " Material 18
    angnr             TYPE ekko-angnr,         " Taxa Pagamento
    ihran             TYPE ekko-ihran,
    txcontratada      TYPE vbkd-kurrf,         "Taxa Contratada
    status,
    zterm(30),
    vtext             TYPE tvzbt-vtext,  " Denominação da condição de pagamento
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 101103 - Ajuste_Relatório_ZSDT0027_ZSDT0051
* inserindo: VBKD-INCO1 / VBKD-INCO2
    inco1             TYPE vbkd-inco1,
    inco2             TYPE vbkd-inco2,
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END
    kurrf             TYPE vbkd-kurrf,
    descmat(60)       TYPE c,            " Descricao Material
    wrkst             TYPE mara-wrkst,
    ntgew             TYPE vbap-ntgew,         " Qte.Contrato 15
*  qtefaturado TYPE vbap-zmeng,  " Quantidade Faturado
    qtefaturado       TYPE zmenge, "db20199vp,  " Quantidade Faturado 10
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 >>> INI
* CH 100102 - Ajuste_Relatório_ZSDT0027_ZSDT0051
    vlr_qtecont       TYPE konv-kbetr,   " Valor Qte. Contratada
    vlr_qtefat        TYPE konv-kbetr,   " Valor Qte. Faturada
    vlr_saldo         TYPE konv-kbetr,   " Valor Saldo
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 <<< END
    "saldo       TYPE vbap-zmeng, " Saldo
    saldo             TYPE zmenge, "db20199vp,                               " Saldo '10
    posnr             TYPE vbap-posnr,                              " Item. 6
    kwmeng            TYPE vbap-kwmeng, " Quantidade da ordem acumulada em unidade de venda 15
    qtd               TYPE zmenge, "db20199vp, " 10
    unid              TYPE vbap-vrkme, "3
    arktx             TYPE vbap-arktx,
    waers             TYPE konv-waers,
    kbetr(15)         TYPE p DECIMALS 2, "konv-kbetr, "*-IR190416-10.09.2025-JT-inicio
    valdt             TYPE vbkd-valdt,
    erdat             TYPE vbak-erdat,   "Data de criação
    safra             TYPE ajahr,
    cultura           TYPE acc_txtlg,
* PBI - 55017 - Inicio
    safra_apl         TYPE ajahr,
    cultura_apl       TYPE acc_txtlg,
    bezei             TYPE tvkbt-bezei,     " Desc. Esc. Vendas
    qte_sol           TYPE db20199vp, "zsdt0082-qte_sol, "Vol. Liberado "bug 61203
    lifsp_t           TYPE tvlst-vtext, "Desc. Bloqueio
    augdt             TYPE bsad-augdt,      "Dt. Compensação
    nrpropopr(60)     TYPE c, "Nº Proposta Operação
    alcpropopr(60)    TYPE c, "Alçada Prop. Operação
    efetivada(60)     TYPE c, "Efetivada
    nrproplmt(60)     TYPE c, "Nº Proposta Limite
    alcproplmt(60)    TYPE c, "Alçada Prop. Limite
* PBI - 55017 - Fim
    lifsp             TYPE vbep-lifsp,
    doc_simulacao     TYPE zsdt0041-doc_simulacao,
    vlr_frete(15)     TYPE p DECIMALS 5,   "zsdt0041-vlr_frete,  "*-US192338-06.10.2025-#192338-JT
    vlr_frete_int(15) TYPE p DECIMALS 5,                         "*-US192338-06.10.2025-#192338-JT
    auart             TYPE vbak-auart,
    j_1bcfop          TYPE vbap-j_1bcfop,
    kbetr2(15)        TYPE p DECIMALS 2, "konv-kbetr, "*-IR190416-10.09.2025-JT-inicio
    j_1btxsdc         TYPE vbap-j_1btxsdc,
    werks             TYPE vbap-werks,  "RJF
    lgort             TYPE vbap-lgort,  "RJF
    dt_simul          TYPE zsdt0040-erdat, "Data Simulador "*-CS2021000218-14.09.2022-#90705-JT
    kunnr             TYPE kna1-kunnr,     "Cliente        "*-CS2021000218-14.09.2022-#90705-JT
    stcd1             TYPE kna1-stcd1,     "CPF/CNPJ       "*-CS2021000218-14.09.2022-#90705-JT
    stcd3             TYPE kna1-stcd3,     "IE             "*-CS2021000218-14.09.2022-#90705-JT
    ort01             TYPE kna1-ort01,     "Municipio      "*-CS2021000218-14.09.2022-#90705-JT
    regio             TYPE kna1-regio,     "UF             "*-CS2021000218-14.09.2022-#90705-JT
    matkl             TYPE mara-matkl,     "GR.Merc        "*-CS2021000218-14.09.2022-#90705-JT
    vlr_totbrt        TYPE konv-kbetr,     "Vlr Tot bruto  "*-CS2021000218-14.09.2022-#90705-JT
    vlr_totbrt_fat    TYPE konv-kbetr,     "Vlr Tot br Fat "*-CS2021000218-14.09.2022-#90705-JT
    dt_entrega        TYPE zsdt0090-dt_entrega,
    cod_vendedor      TYPE zsdt0040-vendedor,
    nome_vendedor     TYPE tvgrt-bezei,
    tipo_venda        TYPE char50,
    ped_ecomm         TYPE zsdt0040-id_order_ecommerce,
    meio_pago         TYPE char50,
    vlr_frete_tot(15) TYPE p DECIMALS 2, "zsdt0041-vlr_frete, "kbetr,   "*-US192338-06.10.2025-#192338-JT     "**<<<------"184428 - NMS------>>>
    cultivar          TYPE char30,
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

  BEGIN OF ty_zfiwrt0009,
    vbeln    TYPE zfiwrt0009-vbeln,
    posnr    TYPE zfiwrt0009-posnr,
    seq_lcto TYPE zfiwrt0009-seq_lcto,
    menge    TYPE zfiwrt0009-menge,
    meins    TYPE zfiwrt0009-meins,
    netwr    TYPE zfiwrt0009-netwr,
  END   OF ty_zfiwrt0009,

  BEGIN OF ty_zfiwrt0008,
    seq_lcto TYPE zfiwrt0008-seq_lcto,
    parid    TYPE zfiwrt0008-parid,
    docnum   TYPE zfiwrt0008-docnum,
    nfenum   TYPE zfiwrt0008-nfenum,
    bldat    TYPE zfiwrt0008-bldat,
    obj_key  TYPE zfiwrt0008-obj_key,
  END   OF ty_zfiwrt0008,

  BEGIN OF ty_parid,
    parid TYPE zfiwrt0008-parid,
  END   OF ty_parid,

  BEGIN OF ty_kunnr,
    kunnr TYPE kna1-kunnr,
  END   OF ty_kunnr,

  BEGIN OF ty_obj_key,
    obj_key TYPE zfiwrt0008-obj_key,
  END   OF ty_obj_key,

  BEGIN OF ty_zib_contabil_chv,
    obj_key TYPE zib_contabil_chv-obj_key,
    belnr   TYPE zib_contabil_chv-belnr,
  END   OF ty_zib_contabil_chv,

  BEGIN OF ty_clientes,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_clientes,

  BEGIN OF ty_tot_zfiwrt0009,
    vbeln       TYPE zfiwrt0009-vbeln,
    posnr       TYPE zfiwrt0009-posnr,
    total_menge TYPE zfiwrt0009-menge,
    total_netwr TYPE zfiwrt0009-netwr,
    total_dmbtr TYPE zfiwrt0011-dmbtr,
  END   OF ty_tot_zfiwrt0009,

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
    lgort     TYPE vbap-lgort,
    netwr     TYPE vbap-netwr,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
    mwsbp     TYPE vbap-mwsbp,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
    netpr     TYPE vbap-netpr,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
    kmein     TYPE vbap-kmein,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
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
    stcd2 TYPE kna1-stcd2,                                  "pbi 55017
    stcd3 TYPE kna1-stcd3,      "*-CS2021000218-14.09.2022-#90705-JT-inicio
    ort01 TYPE kna1-ort01,      "*-CS2021000218-14.09.2022-#90705-JT-inicio
    regio TYPE kna1-regio,      "*-CS2021000218-14.09.2022-#90705-JT-inicio
  END OF ty_kna1,

  BEGIN OF ty_j_1bnfe_active,          " Electronic Nota Fiscal: Actual Status
    docnum TYPE j_1bnfe_active-docnum, " Nº documento
    nfnum9 TYPE j_1bnfe_active-nfnum9, " Nº NF-e de nove posições
  END OF ty_j_1bnfe_active,

  BEGIN OF ty_vbfa_tot,           " Fluxo de documentos de vendas e distribuição
    vbelnrfmg      TYPE vbfa-vbelv, " Documento de vendas e distribuição precedente
    "  totalrfmng TYPE vbfa-rfmng, " Documento de vendas e distribuição subseqüente
    totalmenge     TYPE j_1bnflin-menge,
    posnn          TYPE vbfa-posnn,
    totaldim       TYPE j_1bnflin-menge, "Total de Recusa e Complemento
    vlr_totliq_fat TYPE vbrk-netwr,
    vlr_totbrt_fat TYPE vbrk-netwr,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
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
    kunnr TYPE bsid-kunnr, "**IR181516 informações N Proposta limite para todas as linhas - BG 20/05/2024
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
    netwr TYPE vbrk-netwr,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
    mwsbk TYPE vbrk-mwsbk,  "*-CS2021000218-14.09.2022-#90705-JT-inicio
  END OF ty_vbrk,


  BEGIN OF ty_vbrp,
    vbeln TYPE vbrp-vbeln,
    posnr TYPE vbrp-posnr,
    aubel TYPE vbrp-aubel,
    aupos TYPE vbrp-aupos,
    netwr TYPE vbrp-netwr,
    mwsbp TYPE vbrp-mwsbp,
  END OF ty_vbrp,


  BEGIN OF ty_zsdt0040,
    doc_simulacao      TYPE zsded003,
    vendedor           TYPE zsdt0040-vendedor,
    cultura            TYPE zsded001,
    safra              TYPE ajahr,
    erdat              TYPE zsdt0040-erdat,
    dt_entrega_def     TYPE zsdt0040-dt_entrega_def,
    dt_entrega_sem     TYPE zsdt0040-dt_entrega_sem,
    dt_entrega_fet     TYPE zsdt0040-dt_entrega_fet,
    meio_pago          TYPE zsdt0040-meio_pago,
    ecommerce          TYPE zsdt0040-ecommerce,
    id_order_ecommerce TYPE zsdt0040-id_order_ecommerce,
  END OF ty_zsdt0040,

  BEGIN OF ty_zsdt0041,
    doc_simulacao TYPE zsded003,
    vbeln         TYPE vbeln,
    dt_entrega    TYPE zsdt0090-dt_entrega,
  END OF ty_zsdt0041,

  BEGIN OF ty_dd07t,
    ddtext     TYPE dd07t-ddtext,
    domvalue_l TYPE dd07t-domvalue_l,
  END OF ty_dd07t,

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
  END OF ty_tvls,

  BEGIN OF ty_zfit0046.
    INCLUDE STRUCTURE zfit0046.
TYPES: status TYPE zfit0045-status,
  END OF ty_zfit0046.
**<<<------"160696 - NMS - INI------>>>
TYPES:
  BEGIN OF ty_zfit0046_s,
    ebeln            TYPE ebeln,
    ebelp            TYPE ebelp,
    vlr_adiantamento TYPE rlwrt,
  END   OF ty_zfit0046_s.
**<<<------"160696 - NMS - FIM------>>>
*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA:
  t_bdc               TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  it_saida            TYPE TABLE OF ty_saida,
  it_ekko             TYPE TABLE OF ty_ekko,
  it_0037             TYPE TABLE OF ty_0037,
  it_0035             TYPE TABLE OF ty_0035,
  it_0036             TYPE TABLE OF zmmt0036,
  it_0036_cont        TYPE TABLE OF ty_0036,
  it_ekko_aux         TYPE TABLE OF ty_ekko,
  it_ekbe             TYPE TABLE OF ty_ekbe,
  it_ekbe_zefi        TYPE TABLE OF ty_ekbe,
  it_ekbe2            TYPE TABLE OF ty_ekbe2,
  it_rseg             TYPE TABLE OF ty_rseg,     "US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  it_lin_rseg         TYPE TABLE OF ty_lin_rseg, "US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  it_j_1bnfstx        TYPE TABLE OF ty_1bnfstx,  "US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  it_rbkp_rseg        TYPE TABLE OF ty_rbkp_rseg, "US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  it_ekpo             TYPE TABLE OF ty_ekpo,
  it_ekpo_aux         TYPE TABLE OF ty_ekpo_aux,
  it_eban             TYPE TABLE OF ty_eban,
  it_zfit0045         TYPE TABLE OF zfit0045,
  it_zfit0045_aux     TYPE TABLE OF zfit0045,
  it_zfit0046         TYPE TABLE OF zfit0046,
  it_zfit0046_aux     TYPE TABLE OF ty_zfit0046,
**<<<------"160696 - NMS - INI------>>>
  it_zfit0046_sum     TYPE TABLE OF ty_zfit0046_s,
**<<<------"160696 - NMS - FIM------>>>
  ws_zfit0046_aux     TYPE ty_zfit0046,
  it_rbkp             TYPE TABLE OF ty_rbkp,
  it_lfa1             TYPE TABLE OF ty_lfa1,
  it_ekbe_aux         TYPE TABLE OF ty_ekbe_aux,
  it_j_1bnflin        TYPE TABLE OF ty_j_1bnflin,
  it_j_1bnflin_aux    TYPE TABLE OF ty_j_1bnflin,
  it_j_1bnfdoc        TYPE TABLE OF ty_j_1bnfdoc,
  it_vbak             TYPE TABLE OF ty_vbak,
  it_vbak_aux         TYPE TABLE OF ty_vbak, "*142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  it_zfiwrt0009       TYPE TABLE OF ty_zfiwrt0009,
  it_zfiwrt0008       TYPE TABLE OF ty_zfiwrt0008,
  it_tot_zfiwrt0009   TYPE TABLE OF ty_tot_zfiwrt0009,
  it_parid            TYPE TABLE OF ty_parid,
  it_kunnr            TYPE TABLE OF ty_kunnr,
  it_obj_key          TYPE TABLE OF ty_obj_key,
  it_zib_contabil_chv TYPE TABLE OF ty_zib_contabil_chv,
  it_clientes         TYPE TABLE OF ty_clientes,
  it_tvbur            TYPE TABLE OF tvbur WITH HEADER LINE,
  it_tvkbt            TYPE TABLE OF tvkbt WITH HEADER LINE,
  it_tvls             TYPE TABLE OF tvls WITH HEADER LINE,
  it_tvlst            TYPE TABLE OF tvlst WITH HEADER LINE,
  it_tvfs             TYPE TABLE OF tvfs WITH HEADER LINE,
  it_tvfst            TYPE TABLE OF tvfst WITH HEADER LINE,
  it_vbak_est         TYPE TABLE OF ty_vbak,
  it_vbap_aux         TYPE TABLE OF ty_vbap, "*142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  it_vbap             TYPE TABLE OF ty_vbap,
  it_zsdt0082         TYPE TABLE OF zsdt0082 WITH HEADER LINE,
  it_zsdt0270         TYPE STANDARD TABLE OF zsdt0270 WITH HEADER LINE,
  it_zsdt0271         TYPE STANDARD TABLE OF zsdt0271 WITH HEADER LINE,
  it_zsdt0272         TYPE STANDARD TABLE OF zsdt0272 WITH HEADER LINE,
  it_zsdt0273         TYPE STANDARD TABLE OF zsdt0273 WITH HEADER LINE,
  it_zsdt0274         TYPE TABLE OF zsdt0274 WITH HEADER LINE,
  it_zsdt0275         TYPE TABLE OF zsdt0275 WITH HEADER LINE,
  it_zsdt0090_safra   TYPE TABLE OF zsdt0090 WITH HEADER LINE,
  it_zsdt0041_safra   TYPE TABLE OF zsdt0041 WITH HEADER LINE,
  it_tvzbt            TYPE TABLE OF ty_tvzbt,
  it_mara             TYPE TABLE OF ty_mara,
  it_0026             TYPE TABLE OF zfit0026 WITH HEADER LINE, "
  it_zfit0026_aux     TYPE TABLE OF zfit0026 , "*142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  it_zib              TYPE TABLE OF zib_contabil_chv WITH HEADER LINE, "
  it_bsik             TYPE TABLE OF bsik WITH HEADER LINE, "
  it_bsid             TYPE TABLE OF bsid WITH HEADER LINE, "
  it_bsad             TYPE TABLE OF bsad WITH HEADER LINE,
  it_vbkd_aux         TYPE TABLE OF vbkd WITH HEADER LINE, "
  it_t052u            TYPE TABLE OF t052u WITH HEADER LINE, "
  it_zib_bsik         TYPE TABLE OF ty_zib_bsik WITH HEADER LINE, "
  it_zib_bsid         TYPE TABLE OF ty_zib_bsid WITH HEADER LINE, "
  it_vbfa             TYPE TABLE OF ty_vbfa,
  it_vbfa_est         TYPE TABLE OF ty_vbfa,
  it_vbfa_aux2        TYPE TABLE OF ty_vbfa WITH HEADER LINE,
  it_vbrk             TYPE TABLE OF ty_vbrk,
  it_vbrp             TYPE TABLE OF ty_vbrp,
  it_vbrk_est         TYPE TABLE OF ty_vbrk,
  it_vbrk_aux         TYPE TABLE OF ty_vbrk,
  it_vbfa_mm          TYPE TABLE OF ty_vbfa_mm,
  it_kna1             TYPE TABLE OF ty_kna1,
  it_j_1bnfe_active   TYPE TABLE OF ty_j_1bnfe_active,
  it_vbfa_tot         TYPE TABLE OF ty_vbfa_tot,
  it_vbfa_tot2        TYPE TABLE OF ty_vbfa_tot WITH HEADER LINE,
  it_vbfa_aux         TYPE TABLE OF ty_vbfa_aux,
  it_saida_sd         TYPE TABLE OF ty_saida_sd,
  it_saida_mm2        TYPE TABLE OF ty_saida_mm2,
  it_konv             TYPE TABLE OF ty_konv,
  it_konv_aux         TYPE TABLE OF ty_konv, "*142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  it_konv_imp         TYPE TABLE OF ty_konv,
*  it_konv_rb          TYPE TABLE OF ty_konv, "<<<------"160696 - NMS------>>> => Não está mais sendo usado.
  it_vbkd             TYPE TABLE OF ty_vbkd,
  it_dd07t            TYPE TABLE OF ty_dd07t,
  it_vbep             TYPE TABLE OF ty_vbep,
  it_vbep2            TYPE TABLE OF vbep    WITH HEADER LINE,
  it_vbep_aux         TYPE TABLE OF vbep    WITH HEADER LINE, "*142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  it_saida_sd2        TYPE TABLE OF ty_saida_sd2,
  it_zsdt0040         TYPE TABLE OF ty_zsdt0040 WITH HEADER LINE,
  it_zsdt0041         TYPE TABLE OF ty_zsdt0041 WITH HEADER LINE,
  it_zsdt0041_aux     TYPE TABLE OF ty_zsdt0041 ,      "*142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  it_zsdt0041_frete   TYPE STANDARD TABLE OF zsdt0041,
  it_zsdt0090_frete   TYPE STANDARD TABLE OF zsdt0090,
  it_zsdt0038         TYPE TABLE OF zsdt0038 WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont             TYPE REF TO cl_gui_custom_container        , " Objeto Container
  wa_cont2            TYPE REF TO cl_gui_custom_container        , " Objeto Container
  wa_alv              TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
  wa_alv2             TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
  wa_layout           TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
  wa_layout2          TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
  wa_saida            TYPE ty_saida,
  wa_ekko             TYPE ty_ekko,
  wa_ekko_aux         TYPE ty_ekko,
  wa_0037             TYPE ty_0037,
  wa_0035             TYPE ty_0035,
  wa_0036             TYPE zmmt0036,
  wa_0036_cont        TYPE ty_0036,
  wa_ekbe             TYPE ty_ekbe,
  wa_ekbe_zefi        TYPE ty_ekbe,
  wa_ekbe2            TYPE ty_ekbe2,
  wa_ekpo             TYPE ty_ekpo,
  wa_rbkp             TYPE ty_rbkp,
  wa_lfa1             TYPE ty_lfa1,
  wa_ekbe_aux         TYPE ty_ekbe_aux,
  wa_j_1bnflin        TYPE ty_j_1bnflin,
  wa_j_1bnfdoc        TYPE ty_j_1bnfdoc,
  wa_vbak             TYPE ty_vbak,
  wa_zfiwrt0009       TYPE ty_zfiwrt0009,
  wa_zfiwrt0008       TYPE ty_zfiwrt0008,
  wa_tot_zfiwrt0009   TYPE ty_tot_zfiwrt0009,
  wa_kunnr            TYPE ty_kunnr,
  wa_parid            TYPE ty_parid,
  wa_obj_key          TYPE ty_obj_key,
  wa_zib_contabil_chv TYPE ty_zib_contabil_chv,
  wa_clientes         TYPE ty_clientes,
  wa_vbap             TYPE ty_vbap,
  wa_vbfa             TYPE ty_vbfa,
  wa_vbrk             TYPE ty_vbrk,
  wa_vbrp             TYPE ty_vbrp,
  wa_vbrk_aux         TYPE ty_vbrk,
  wa_vbfa_mm          TYPE ty_vbfa_mm,
  wa_kna1             TYPE ty_kna1,
  wa_tvzbt            TYPE ty_tvzbt,
  wa_mara             TYPE ty_mara,
  wa_j_1bnfe_active   TYPE ty_j_1bnfe_active,
  wa_vbfa_tot         TYPE ty_vbfa_tot,
  wa_saida_sd         TYPE ty_saida_sd,
  wa_saida_mm2        TYPE ty_saida_mm2,
  wa_saida_sd2        TYPE ty_saida_sd2,
  wa_konv             TYPE ty_konv,
  wa_vbkd             TYPE ty_vbkd,
  wa_vbep             TYPE ty_vbep,
  wa_vbfa_aux         TYPE ty_vbfa_aux,
  wa_bdc              TYPE bdcdata.

FIELD-SYMBOLS: <wmwst> TYPE any.
FIELD-SYMBOLS: <data> TYPE table.

DATA: vg_vlr_conv TYPE vbrk-netwr.

*----------------------------------------------------------------------*
* ESTRUTURA ALV  - Tabela Estrutura Colunas do Relatório
*----------------------------------------------------------------------*
DATA:
  it_fcat          TYPE TABLE OF lvc_s_fcat,
  s_variant        TYPE disvariant,
  it_fcat2         TYPE TABLE OF lvc_s_fcat,
  r_ekpo_s         TYPE RANGE OF ekpo-loekz,
  wa_status        LIKE LINE OF  r_ekpo_s,
  gs_variant_c     TYPE disvariant,
  vg_ebeln         TYPE zfit0046-ebeln,
  vg_ebelp         TYPE zfit0046-ebelp,
  verro(1),
  vg_mont_rbdo_liq TYPE zfit0026-mont_rbdo.


DATA: linha_selecionada TYPE slis_selfield.
DATA: nr_solic TYPE num10.
DATA:    _exit             TYPE c.

DATA: l_netpr_liq     TYPE p LENGTH 16 DECIMALS 11,   "*-CS2021000218-14.09.2022-#90705-JT-inicio
      l_netpr_brt     TYPE p LENGTH 16 DECIMALS 11,   "*-CS2021000218-14.09.2022-#90705-JT-inicio
      l_netpr_liq_aux TYPE p LENGTH 16 DECIMALS 11,   "**US 142625
      l_netpr_brt_aux TYPE p LENGTH 16 DECIMALS 11.   "**US 142625

RANGES:  rg_cpf_cnpj FOR  kna1-stcd1.

DATA: rg_bschl TYPE RANGE OF zfiwrt0011-bschl,
      wa_bschl LIKE LINE  OF rg_bschl.

*DATA: r_belnr TYPE RANGE OF belnr_d. "<<<------"165866 - NMS"------>>>
DATA: r_operacao TYPE RANGE OF zfiwrt0008-operacao. "***IR181516 informações N Proposta limite para todas as linhas - BG 20/05/2024
**<<<------"165866 - NMS - INI------>>>
*DATA: r_bukrs TYPE RANGE OF bukrs. "****IR181516 informações N Proposta limite para todas as linhas - BG 20/05/2024
*DATA: r_kunnr TYPE RANGE OF kunnr. "**IR181516 informações N Proposta limite para todas as linhas - BG 20/05/2024
**<<<------"165866 - NMS - FIM------>>>
DATA: variante        LIKE disvariant.
DATA t_0035.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
               <fs_wa>,
               <fs_field>.
DATA: dyn_table TYPE REF TO data,
      dyn_line  TYPE REF TO data.
**<<<------"160696 - NMS - INI------>>>
DATA: vg_task_ativa TYPE n LENGTH 4,
      vg_tasks      TYPE i,
      vg_max_tasks  TYPE i.
**<<<------"160696 - NMS - FIM------>>>

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
" Seleção de Campos (TextField)
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: mm RADIOBUTTON GROUP rb1 USER-COMMAND sel DEFAULT 'X'.
  PARAMETERS: sd RADIOBUTTON GROUP rb1.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: SKIP 1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS:  p_pedido   FOR ekko-bsart NO INTERVALS MODIF ID mm,                  " Tipo Pedido
                   p_numped   FOR ekko-ebeln NO INTERVALS MODIF ID mm,                  " Numero do pedido
                   p_sol      FOR zfit0045-nro_sol NO INTERVALS MODIF ID mm,            " Numero da solicitação
                   p_emp      FOR ekko-bukrs NO INTERVALS MODIF ID mm,                  " Nome Empresa
                   p_forn     FOR ekko-lifnr MODIF ID mm,                               " Fornecedor
                   p_centro   FOR ekpo-werks MODIF ID mm,                               " Campo Centro
                   p_mat      FOR ekpo-matnr MODIF ID mm,                               " Material
                   p_matkl    FOR ekpo-matkl MODIF ID mm,                               " Grupo de Mercadoria
                   p_data     FOR ekko-bedat MODIF ID mm,                               " Data de Entrega
                   p_fatu     FOR ekbe-budat MODIF ID mm,                               " Data de criação do registro
                   p_waerkm   FOR vbak-waerk NO INTERVALS NO-EXTENSION MODIF ID mm.     " Moeda
SELECTION-SCREEN: END OF BLOCK b2.


SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-036.
  PARAMETERS:
    bloq_mm  TYPE char1  RADIOBUTTON GROUP rb02 MODIF ID mm,
    elim_mm  TYPE char1  RADIOBUTTON GROUP rb02 MODIF ID mm,
    ativo_mm TYPE char1  RADIOBUTTON GROUP rb02 MODIF ID mm,
    todos_mm TYPE char1  RADIOBUTTON GROUP rb02 DEFAULT 'X' MODIF ID mm.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS:  p_tpcont    FOR vbak-auart NO INTERVALS MODIF ID sd,                 " Tipo de Contrato
                   p_cont      FOR vbak-vbeln MODIF ID sd,                              " Contrato
                   p_simula    FOR vbak-vbeln MODIF ID sd,                              " Doc simulacao "*-CS2021000218-14.09.2022-#90705-JT-inicio
                   p_orgven    FOR vbak-vkorg NO INTERVALS NO-EXTENSION MODIF ID sd,    " Organização de vendas
                   p_cdist     FOR vbak-vtweg MODIF ID sd,                              " Canal distribuição
                   p_sativ     FOR vbak-spart MODIF ID sd,                              " Setor de atividade
                   p_escven    FOR vbak-vkbur MODIF ID sd,                              " Escritório de vendas
                   p_clien     FOR vbak-kunnr MODIF ID sd,                              " Cliente
                   p_datent    FOR vbak-erdat MODIF ID sd,                              " Data de Entrada
                   p_fatuv     FOR vbfa-erdat NO-EXTENSION MODIF ID sd,                 " Data de criação do registro
                   p_cent      FOR vbap-werks MODIF ID sd,                              " Centro
                   p_mater     FOR vbap-matnr MODIF ID sd,                              " Material
                   p_grupo     FOR mara-matkl MODIF ID sd ,                              " Material
                   p_waerks    FOR vbak-waerk NO INTERVALS NO-EXTENSION MODIF ID sd.    " Moeda
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-118.
  PARAMETERS:
    todos_sd TYPE char1  RADIOBUTTON GROUP rb03 DEFAULT 'X' MODIF ID sd,
    conv_sd  TYPE char1  RADIOBUTTON GROUP rb03 MODIF ID sd,
    ecom_sd  TYPE char1  RADIOBUTTON GROUP rb03 MODIF ID sd.
SELECTION-SCREEN: END OF BLOCK b6.


SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-000.
  PARAMETERS: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

PARAMETERS: p_t257 TYPE char1 NO-DISPLAY.

PARAMETERS: p_prod TYPE char1 NO-DISPLAY. "// BUG 160640 10-12-2024 - WBARBOSA

PARAMETERS: p_t189 TYPE char1 NO-DISPLAY. "-US 156984-07-11-2024-#156984-RJF

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

AT SELECTION-SCREEN OUTPUT.
  IF mm = 'X'.
    PERFORM hide_mm_options.
*    LOOP AT SCREEN.
*      CASE screen-name.
*        WHEN 'P_PEDIDO-LOW'.
*          screen-required = 1.
*          MODIFY SCREEN.
*      ENDCASE.
*    ENDLOOP.
  ELSE.
    PERFORM hide_sd_options.
*    LOOP AT SCREEN.
*      CASE screen-name.
*        WHEN 'P_PEDIDO-LOW'.
*          screen-required = 0.
*          MODIFY SCREEN.
*      ENDCASE.
*    ENDLOOP.
  ENDIF.

*  IF mm EQ 'X'.
*    LOOP AT SCREEN.
*      CASE screen-name.
*        WHEN 'P_PEDIDO-LOW'.
*          screen-required = 1.
*          MODIFY SCREEN.
*      ENDCASE.
*    ENDLOOP.
*  ELSE.
*   LOOP AT SCREEN.
*      CASE screen-name.
*        WHEN 'P_PEDIDO-LOW'.
*          screen-required = 0.
*          MODIFY SCREEN.
*      ENDCASE.
*    ENDLOOP.
*  ENDIF.





*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.



  DATA: lv_call_prog TYPE sy-repid.
  CLEAR verro.

  " Compras
  IF mm = 'X'.
    "CS2021000644
    LOOP AT p_emp .
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
         ID 'ACTVT' FIELD '03'
         ID 'BUKRS' FIELD p_emp-low.
      IF sy-subrc NE 0.
        MESSAGE | Sem acesso a empresa: { p_emp-low } | TYPE 'I'.
        verro = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    "CS2021000644

    IF verro NE 'X'.

*-US 156984-07-11-2024-#156984-RJF-Inicio
      " Compras (Chamada por JOB para gravar tabela ZMMT0189)
      IMPORT lv_call_prog FROM DATABASE indx(zp) ID 'ZMMT0189'.
      IF p_t189 IS NOT INITIAL.
        PERFORM: form_seleciona_mm,          " Seleção de Dados - Formulário MM
                 form_saida_m,              " Saída MM
                 f_armazena_tabela_zmmt0189. " Armazena dados na tabela ZMMT0189
      ELSE.
*-US 156984-07-11-2024-#156984-RJF-Fim

        PERFORM: form_seleciona_mm,      " Seleção de Dados - Formulário MM
                 form_alv_mm,            " Formulário ALV MM
                 form_saida_mm.          " Saída MM
        CALL SCREEN 0100.
      ENDIF.
*-US 156984-07-11-2024-#156984-RJF-Inicio
    ENDIF.
*-US 156984-07-11-2024-#156984-RJF-Fim

  ELSE.

    "// Inicio BUG 160640 10-12-2024 - WBARBOSA
    IF p_prod IS NOT INITIAL.
      IMPORT lv_call_prog FROM DATABASE indx(zp) ID 'GETAPPPRODUTOR'.
    ENDIF.
    "// Fim BUG 160640 10-12-2024 - WBARBOSA

* Início - Sara Oikawa - 10.07.2020 - 38883 - Adequação na ZSDT0051
    " Vendas (Chamada por JOB para gravar tabela ZSDT0257)
    IMPORT lv_call_prog FROM DATABASE indx(zp) ID 'ZSDT0257'.
    IF p_t257 IS NOT INITIAL OR p_prod IS NOT INITIAL." OR ( sy-calld IS NOT INITIAL AND lv_call_prog = 'ZSDR0223' ).
      PERFORM: form_seleciona_sd,          " Seleção de Dados - Formulário SD
               form_saida_sd,              " Saída SD
               f_armazena_tabela_zsdt0257. " Armazena dados na tabela ZSDT0257
    ELSE.
* Fim - Sara Oikawa - 10.07.2020 - 38883 - Adequação na ZSDT0051

      " Vendas
      PERFORM: form_seleciona_sd,      " Seleção de Dados - Formulário SD
               form_saida_sd,          " Saída SD
               form_alv_sd.            " Formulário ALV SD
      CALL SCREEN 0100.

    ENDIF.
  ENDIF.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FORM_SELECIONA_DADOS_mm
*&---------------------------------------------------------------------*
FORM form_seleciona_mm .

**<<<------"160696 - NMS - INI------>>>
  DATA: el_zfit0046_sum TYPE ty_zfit0046_s.
**<<<------"160696 - NMS - FIM------>>>
  DATA  : ebeln_aux          TYPE ekbe-ebeln,
          ebelp_aux          TYPE ekbe-ebelp,
          belnr_aux          TYPE ekbe-belnr,
          gjahr_aux          TYPE ekbe-gjahr,
          xtot_faturamm      TYPE ekbe-menge,
          xtot_faturaan      TYPE ekbe-menge,
          budat_aux          TYPE ekbe-budat,
          dmbtr_aux          TYPE ekbe-dmbtr,
          shkzg_aux          TYPE ekbe-shkzg,
*          bel_refkey         TYPE j_1bnflin-refkey, "<<<------"160696 - NMS------>>>
          x_vlr_tot_faturamm TYPE rbkp-rmwwr.


  IF  p_pedido IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Tipo de Pedido!'.
    STOP.
  ENDIF.

  IF  p_emp IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Empresa!'.
    STOP.
  ENDIF.

  "Check se usuário tem autorização para a Empresa
  IF p_emp IS NOT INITIAL .

    AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
    ID 'BUKRS' FIELD p_emp-low.

    IF sy-subrc <> 0.
      MESSAGE i000(z01) WITH 'Perfil do usuário sem acesso a esta Empresa'.
      STOP.
    ENDIF.

  ENDIF.

  IF  p_data  IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data!'.
    STOP.
  ENDIF.

  IF  p_matkl  IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Grupo de Mercadoria!'.
    STOP.
  ENDIF.

  PERFORM: condicoes_mm.

  IF p_sol IS INITIAL.
    SELECT ebeln bsart lifnr bukrs reswk bedat knumv aedat angnr ihran inco1 ihrez zterm submi verkf telf1
     FROM ekko
     INTO TABLE it_ekko
    WHERE ebeln   IN p_numped "CS2022000771   #83814 FF   26.01.2023
      AND bsart   IN p_pedido
      AND bukrs   IN p_emp
      AND lifnr   IN p_forn
      AND bedat   IN p_data
      AND waers   IN p_waerkm.
  ELSE.

    SELECT a~ebeln a~bsart a~lifnr a~bukrs a~reswk a~bedat a~knumv a~aedat a~angnr a~ihran a~inco1 a~ihrez a~zterm a~submi a~verkf a~telf1
    FROM ekko AS a
    INNER JOIN zmmt0035 AS b ON b~ebeln EQ a~ebeln
    INTO TABLE it_ekko
   WHERE a~ebeln   IN p_numped "CS2022000771   #83814 FF   26.01.2023
     AND a~bsart   IN p_pedido
     AND a~bukrs   IN p_emp
     AND a~lifnr   IN p_forn
     AND a~bedat   IN p_data
     AND a~waers   IN p_waerkm
     AND b~nro_sol_cp IN p_sol.

    SELECT a~ebeln a~bsart a~lifnr a~bukrs a~reswk a~bedat a~knumv a~aedat a~angnr a~ihran a~inco1 a~ihrez a~zterm a~submi a~verkf a~telf1
    FROM ekko AS a
    INNER JOIN zmmt0037 AS b ON b~ebeln EQ a~ebeln
    APPENDING TABLE it_ekko
   WHERE a~ebeln   IN p_numped "CS2022000771   #83814 FF   26.01.2023
     AND b~nro_sol_cp IN p_sol.

  ENDIF.


  CHECK it_ekko[] IS NOT INITIAL.

  it_ekko_aux[] = it_ekko[].

*  DELETE it_ekko WHERE bsart EQ 'ZEFI'.
*  DELETE it_ekko_aux WHERE bsart NE 'ZEFI'.

  SELECT knumv kposn kschl waers kbetr kmein kwert
    FROM prcd_elements "konv
    INTO TABLE it_konv
     FOR ALL ENTRIES IN it_ekko
   WHERE knumv EQ it_ekko-knumv
     AND kschl IN ('PBXX','PB00','RB00','ZB00').
**<<<------"160696 - NMS - INI------>>>
  DATA(tl_konv) = it_konv.
  CLEAR it_konv.
  LOOP AT tl_konv INTO DATA(el_konv).
    CASE el_konv-kschl.
      WHEN 'RB00' OR "Desconto Absoluto do item
           'ZB00'.   "Suplemento Absoluto do item
        CLEAR: el_konv-kbetr, el_konv-kmein.

      WHEN OTHERS.
        CLEAR: el_konv-kbetr.

    ENDCASE.

    COLLECT el_konv INTO it_konv.

  ENDLOOP.
**<<<------"160696 - NMS - FIM------>>>
  TRY .

      SELECT ebeln ebelp matnr txz01 menge meins loekz matkl mwskz bprme elikz peinh werks lgort banfn netpr
       FROM ekpo
       INTO TABLE it_ekpo
        FOR ALL ENTRIES IN it_ekko
      WHERE ebeln EQ it_ekko-ebeln
        AND matnr IN p_mat
        AND werks IN p_centro
        AND loekz IN r_ekpo_s
        AND matkl IN p_matkl.
*        AND banfn IN p_numreq. "CS2022000771   #83814 FF   26.01.2023

      IF it_ekpo[] IS NOT INITIAL.

**  Begin of CS2022000771   #83814 FF   25.01.2023"
        DATA: lr_ebeln TYPE RANGE OF ebeln.

        lr_ebeln = VALUE #( FOR ls_value IN it_ekpo ( sign = 'I'
                                                      option = 'EQ'
                                                      low = ls_value-ebeln ) ).
        DELETE it_ekko WHERE ebeln NOT IN lr_ebeln.
        CHECK it_ekko[] IS NOT INITIAL.

        SELECT banfn badat
          INTO TABLE it_eban
          FROM eban
          FOR ALL ENTRIES IN it_ekpo
          WHERE banfn = it_ekpo-banfn.
        IF sy-subrc <> 0.
          CLEAR it_eban[].
        ENDIF.

        SELECT * FROM zfit0046
          INTO TABLE it_zfit0046
          FOR ALL ENTRIES IN it_ekko
          WHERE ebeln = it_ekko-ebeln.
*          AND nro_sol IN p_sol.
        IF sy-subrc <> 0.
          CLEAR it_zfit0046.
**<<<------"160696 - NMS - INI------>>>
        ELSE.
          LOOP AT it_zfit0046 INTO DATA(es_it_zfit0046).
            MOVE-CORRESPONDING es_it_zfit0046 TO el_zfit0046_sum.
            COLLECT el_zfit0046_sum INTO it_zfit0046_sum.

          ENDLOOP.
**<<<------"160696 - NMS - FIM------>>>
        ENDIF.

        SELECT * FROM zfit0045
          INTO TABLE it_zfit0045
          FOR ALL ENTRIES IN it_ekko
          WHERE ebeln = it_ekko-ebeln.
*            AND nro_sol IN p_sol.

        IF sy-subrc <> 0.
          CLEAR it_zfit0045.
        ENDIF.
** End of FF  25.01.2023

        SELECT safra lifnr ebeln werks banfn nro_sol_cp ped_forn waers data_atual data_criacao
               cultura                                                                          "**<<<------"184428 - NMS------>>>
          FROM zmmt0035
          INTO TABLE it_0035
          FOR ALL ENTRIES IN it_ekpo
            WHERE ebeln = it_ekpo-ebeln
              AND werks = it_ekpo-werks.

*-US192338-06.10.2025-#192338-JT-inicio
        LOOP AT it_ekko  INTO DATA(_ekko).
          _ekko-nro_sol_cp  = _ekko-submi.
          MODIFY it_ekko FROM _ekko INDEX sy-tabix TRANSPORTING nro_sol_cp.
        ENDLOOP.

        SELECT safra lifnr ebeln werks banfn nro_sol_cp ped_forn waers data_atual data_criacao
               cultura                                                                          "**<<<------"184428 - NMS------>>>
          FROM zmmt0035
          APPENDING TABLE it_0035
           FOR ALL ENTRIES IN it_ekko
         WHERE nro_sol_cp = it_ekko-nro_sol_cp.

        SORT it_0035 BY ebeln.
        DELETE ADJACENT DUPLICATES FROM it_0035 COMPARING ebeln.
*-US192338-06.10.2025-#192338-JT-fim

        IF it_0035[] IS NOT INITIAL.
          SELECT nro_sol_cp
                 ebelp
                 ebeln
                 netpr_germ
                 netpr_roya
                 netpr_desc
                 netpr_supl
           FROM zmmt0037
           INTO TABLE it_0037
             FOR ALL ENTRIES IN it_0035
                WHERE nro_sol_cp = it_0035-nro_sol_cp.

          SELECT *
           FROM zmmt0036
           INTO TABLE it_0036
             FOR ALL ENTRIES IN it_0035
                WHERE nro_sol_cp = it_0035-nro_sol_cp.

          LOOP AT it_0036 INTO wa_0036.
            wa_0036_cont-nro_sol_cp = wa_0036-nro_sol_cp.
            wa_0036_cont-cont = 1.
            COLLECT wa_0036_cont INTO it_0036_cont.
          ENDLOOP.

          SORT it_0036_cont BY cont DESCENDING.
        ENDIF.
      ENDIF.

      SORT it_0035 BY ebeln.
      SORT it_0036 BY nro_sol_cp dt_vcto.
      SORT it_0037 BY nro_sol_cp.

    CATCH cx_sy_open_sql_db INTO DATA(err).
      DATA(msg) = err->get_text( ).
      MESSAGE msg TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.

  CHECK it_ekko[] IS NOT INITIAL.
*** Stefanini - IR216573 - 10/01/2024 - LAZAROSR - Início de Alteração
  CHECK it_ekpo[] IS NOT INITIAL.
*** Stefanini - IR216573 - 10/01/2024 - LAZAROSR - Fim de Alteração

  SELECT name1 lifnr stcd1 stcd2 stkzn
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_ekko
  WHERE lifnr EQ it_ekko-lifnr .

  SELECT ebeln ebelp zekkn vgabe gjahr belnr buzei budat menge dmbtr shkzg
    FROM ekbe
    INTO TABLE it_ekbe
    FOR ALL ENTRIES IN it_ekpo
  WHERE ebeln EQ it_ekpo-ebeln
    AND ebelp EQ it_ekpo-ebelp
    AND vgabe = '2'.
  " AND BUDAT IN P_FATU.


  CHECK sy-subrc IS INITIAL.

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

  DATA(lv_wrbtr_sum)      = CONV rseg-wrbtr( '' ).
  DATA(lv_wrbtr_subtract) = CONV rseg-wrbtr( '' ).
  DATA: lr_refkeyl TYPE RANGE OF j_1bnflin-refkey, "RJF
        ln_ref     LIKE LINE OF lr_refkeyl,        "RJF
        lr_refkey  LIKE TABLE OF ln_ref,           "RJF
        lr_buzei   TYPE RANGE OF rseg-buzei.
**<<<------"184428 - NMS - INI------>>>
*  SELECT BELNR GJAHR EBELN EBELP SHKZG WRBTR
  SELECT belnr gjahr ebeln ebelp shkzg wrbtr buzei
**<<<------"184428 - NMS - FIM------>>>
    FROM rseg
    INTO TABLE it_rseg
    FOR ALL ENTRIES IN it_ekbe
    WHERE belnr = it_ekbe-belnr
      AND gjahr = it_ekbe-gjahr
      AND ebeln = it_ekbe-ebeln
      AND ebelp = it_ekbe-ebelp.

  lr_refkey = VALUE #( FOR ls_rseg IN it_rseg
                       ( sign   = 'I'
                         option = 'EQ'
                         low    = |{ ls_rseg-belnr }{ ls_rseg-gjahr }|
                         high   = ' ' ) ).

  DELETE ADJACENT DUPLICATES FROM lr_refkey COMPARING ALL FIELDS. "RJF

  lr_buzei = VALUE #( FOR ls_rseg IN it_rseg
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = ls_rseg-buzei * 10 ) ).

  DELETE ADJACENT DUPLICATES FROM lr_buzei COMPARING ALL FIELDS. "RJF

  SELECT refkey docnum itmnum
    FROM j_1bnflin
    INTO TABLE it_lin_rseg
    FOR ALL ENTRIES IN lr_refkey "RJF
*    WHERE refkey IN lr_refkey   "RJF
     WHERE refkey EQ lr_refkey-low   "RJF
       AND itmnum IN lr_buzei.

  IF it_lin_rseg IS NOT INITIAL.

    SELECT docnum itmnum taxval
      FROM j_1bnfstx
      INTO TABLE it_j_1bnfstx
      FOR ALL ENTRIES IN it_lin_rseg
      WHERE docnum = it_lin_rseg-docnum
        AND itmnum = it_lin_rseg-itmnum.

  ENDIF.

  SELECT belnr gjahr xblnr bldat rmwwr waers kursf
    FROM rbkp
    INTO TABLE it_rbkp_rseg
    FOR ALL ENTRIES IN it_rseg
    WHERE belnr = it_rseg-belnr
    AND gjahr   = it_rseg-gjahr.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo2
**<<<------"160696 - NMS - INI------>>>
*  SELECT belnr gjahr xblnr bldat rmwwr waers
  SELECT belnr gjahr xblnr bldat rmwwr waers wmwst1
**<<<------"160696 - NMS - FIM------>>>
    FROM rbkp
    INTO TABLE it_rbkp
    FOR ALL ENTRIES IN it_ekbe
  WHERE belnr EQ it_ekbe-belnr
    AND gjahr   EQ it_ekbe-gjahr.

  CHECK sy-subrc IS INITIAL.

  it_ekpo_aux = VALUE #( FOR i IN it_ekpo ( xped = i-ebeln ebeln = i-ebeln ebelp = i-ebelp ) ).

  SELECT refkey docnum menge meins netwr nfnet xped nitemped
  FROM j_1bnflin
  INTO CORRESPONDING FIELDS OF TABLE it_j_1bnflin_aux
  FOR ALL ENTRIES IN it_ekpo_aux
  WHERE  xped EQ it_ekpo_aux-xped.

  ebeln_aux = ''.
  ebelp_aux = ''.
  belnr_aux = ''.
  gjahr_aux = ''.
  xtot_faturamm  = 0.
  xtot_faturaan = 0.
  x_vlr_tot_faturamm = 0.

  SORT it_ekbe BY ebeln ebelp.

  LOOP AT it_ekbe INTO wa_ekbe .
    IF  ebeln_aux  = '' AND ebelp_aux  = ''.
      ebeln_aux = wa_ekbe-ebeln.
      ebelp_aux = wa_ekbe-ebelp.
      belnr_aux = wa_ekbe-belnr.
      belnr_aux = wa_ekbe-belnr.
      budat_aux = wa_ekbe-budat.
      dmbtr_aux = wa_ekbe-dmbtr.
      shkzg_aux = wa_ekbe-shkzg.
      gjahr_aux = wa_ekbe-gjahr.
    ENDIF.

    IF wa_ekbe-ebeln = ebeln_aux AND ebelp_aux = wa_ekbe-ebelp.

      IF wa_ekbe-budat IN p_fatu.
        IF wa_ekbe-shkzg = 'S'.
          xtot_faturamm  = xtot_faturamm  + wa_ekbe-menge.
        ELSEIF wa_ekbe-shkzg = 'H'.
          xtot_faturamm  = xtot_faturamm  - wa_ekbe-menge.
        ENDIF.
      ELSE.
        IF wa_ekbe-shkzg = 'S'.
          xtot_faturaan  = xtot_faturaan  + wa_ekbe-menge.
        ELSEIF wa_ekbe-shkzg = 'H'.
          xtot_faturaan  = xtot_faturaan  - wa_ekbe-menge.
        ENDIF.
      ENDIF.

    ELSE.

      wa_ekbe2-menge = xtot_faturamm.
      wa_ekbe2-menge_a = xtot_faturaan.
      wa_ekbe2-ebeln = ebeln_aux.
      wa_ekbe2-ebelp = ebelp_aux.
      wa_ekbe2-belnr = belnr_aux.
      wa_ekbe2-budat = budat_aux.
      wa_ekbe2-dmbtr = dmbtr_aux.
      wa_ekbe2-shkzg = shkzg_aux.
      wa_ekbe2-gjahr = gjahr_aux.
      wa_ekbe2-rmwwr = x_vlr_tot_faturamm.

      APPEND wa_ekbe2 TO it_ekbe2.

      xtot_faturamm = 0.
      xtot_faturaan = 0.
      x_vlr_tot_faturamm = 0.

      IF wa_ekbe-budat IN p_fatu.
        IF wa_ekbe-shkzg = 'S'.
          xtot_faturamm  = xtot_faturamm  + wa_ekbe-menge.
        ELSEIF wa_ekbe-shkzg = 'H'.
          xtot_faturamm  = xtot_faturamm  - wa_ekbe-menge.
        ENDIF.
      ELSE.
        IF wa_ekbe-shkzg = 'S'.
          xtot_faturaan  = xtot_faturaan  + wa_ekbe-menge.
        ELSEIF wa_ekbe-shkzg = 'H'.
          xtot_faturaan  = xtot_faturaan  - wa_ekbe-menge.
        ENDIF.
      ENDIF.

      ebeln_aux     = wa_ekbe-ebeln.
      ebelp_aux     = wa_ekbe-ebelp.
      ebelp_aux     = wa_ekbe-ebelp.
      belnr_aux     = wa_ekbe-belnr.
      budat_aux     = wa_ekbe-budat.
      dmbtr_aux     = wa_ekbe-dmbtr.
      shkzg_aux     = wa_ekbe-shkzg.
      gjahr_aux     = wa_ekbe-gjahr.
**<<<------"160696 - NMS - INI------>>>
      wa_ekbe2-belnr         = | { wa_ekbe2-belnr ALPHA = IN WIDTH = 10 } |.
      wa_ekbe_aux-bel_refkey = wa_ekbe2-belnr && wa_ekbe2-gjahr.
      APPEND wa_ekbe_aux TO it_ekbe_aux.
**<<<------"160696 - NMS - FIM------>>>
    ENDIF.

    READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr = wa_ekbe-belnr gjahr = wa_ekbe-gjahr BINARY SEARCH. " Valor fatura
    IF ( sy-subrc EQ 0 ).
      x_vlr_tot_faturamm = x_vlr_tot_faturamm + wa_rbkp-rmwwr.
    ENDIF.

  ENDLOOP.

  wa_ekbe2-menge = xtot_faturamm.

  wa_ekbe2-ebeln = ebeln_aux.
  wa_ekbe2-ebelp = ebelp_aux.
  wa_ekbe2-belnr = belnr_aux.
  wa_ekbe2-budat = budat_aux.
  wa_ekbe2-dmbtr = dmbtr_aux.
  wa_ekbe2-shkzg = shkzg_aux.
  wa_ekbe2-gjahr = gjahr_aux.
  wa_ekbe2-rmwwr = x_vlr_tot_faturamm.

  APPEND wa_ekbe2 TO it_ekbe2.
**<<<------"160696 - NMS - INI------>>>
  wa_ekbe2-belnr         = | { wa_ekbe2-belnr ALPHA = IN WIDTH = 10 } |.
  wa_ekbe_aux-bel_refkey = wa_ekbe2-belnr && wa_ekbe2-gjahr.
  APPEND wa_ekbe_aux TO it_ekbe_aux.
*
*  LOOP AT it_ekbe2 INTO wa_ekbe2.
*    bel_refkey = 0.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wa_ekbe2-belnr
*      IMPORTING
*        output = wa_ekbe2-belnr.
*    CONCATENATE  wa_ekbe2-belnr wa_ekbe2-gjahr INTO bel_refkey .
*    wa_ekbe_aux-bel_refkey = bel_refkey.
*    APPEND wa_ekbe_aux TO it_ekbe_aux.
*
*  ENDLOOP.
**<<<------"160696 - NMS - FIM------>>>
  SORT it_ekbe_aux BY bel_refkey.

  SELECT refkey docnum menge meins netwr nfnet
    FROM j_1bnflin
    INTO TABLE it_j_1bnflin
    FOR ALL ENTRIES IN it_ekbe_aux
  WHERE  refkey EQ it_ekbe_aux-bel_refkey.

  CHECK sy-subrc IS INITIAL.

  SELECT docnum docdat nfnum nfe nfenum
    FROM j_1bnfdoc
    INTO TABLE it_j_1bnfdoc
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE docnum EQ it_j_1bnflin-docnum.

  CHECK sy-subrc IS INITIAL.

ENDFORM.                    " FORM_SELECIONA_MM

*&---------------------------------------------------------------------*
*&      Form  FORM_SAIDA
*&---------------------------------------------------------------------*
FORM form_saida_mm .
  REFRESH it_saida.

  DATA es_parametro TYPE zmme_paral_busca_imposto_param. "<<------"160696 - NMS------>>>

  DATA: vrefkey      TYPE j_1bnflin-refkey,
        aux(100)     TYPE c,
        bel_refkey   TYPE j_1bnflin-refkey,
        v_meng_ori   TYPE ekpo-menge,
        vlin         TYPE i,
        vtot         TYPE i,
        vfator       TYPE i,
        clin(6),
        ctot(6),
        vmsg(50),
        it_saida_aux LIKE it_saida.
**<<<------"184428 - NMS - INI------>>>
  DATA(cll_mm_util) = NEW zcl_mm_util( ).

  DATA: vl_object TYPE ausp-objek.

  CONSTANTS: cl_cultivar  TYPE atnam      VALUE 'NOVA_CULTIVAR',
             cl_class     TYPE klasse_d   VALUE 'SEMENTES_GERAL',
             cl_classtype TYPE klassenart VALUE '001'.
**<<<------"184428 - NMS - FIM------>>>
*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo2
  DATA(lv_wrbtr_sum)          = CONV rseg-wrbtr( '' ).
  DATA(lv_wrbtr_subtract)     = CONV rseg-wrbtr( '' ).
  DATA(lv_total_liq_faturado) = CONV komp-netwr( '' ).
  DATA(lv_imposto_nota_fiscal) = CONV komp-netwr( '' ).
  DATA(lv_refkey)             = CONV j_1bnflin-refkey( '' ).
  DATA(lv_buzei)              = CONV rseg-buzei( '' ).
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo2
**<<<------"160696 - NMS - INI------>>>
  DATA(tl_rbkp) = it_rbkp.
  DELETE tl_rbkp WHERE wmwst1 IS INITIAL.
  SORT tl_rbkp BY belnr gjahr DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_rbkp COMPARING belnr gjahr.
**<<<------"160696 - NMS - FIM------>>>
  SORT: it_ekko      BY ebeln,
        it_ekbe      BY ebeln ebelp,
        it_ekbe2     BY ebeln ebelp,
        it_ekpo      BY ebeln ebelp,
        it_rbkp      BY belnr gjahr ,
        it_lfa1      BY lifnr,
**<<<------"160696 - NMS - INI------>>>
        it_konv      BY knumv kposn kschl,
        it_zfit0045  BY ebeln,
        it_zfit0046  BY ebeln ebelp,
        it_zfit0046_sum BY ebeln ebelp,
**<<<------"160696 - NMS - FIM------>>>
        it_rseg      BY ebeln ebelp belnr gjahr buzei,   "**<<<------"184428 - NMS------>>>
        it_j_1bnflin BY refkey.

  DATA: lv_cont TYPE sy-tabix.
  DATA: lv_field TYPE c LENGTH 20.
  DATA: lv_text TYPE c LENGTH 20.
  DATA: lv_qtde TYPE ekpo-menge.
  DATA: lv_unit TYPE t006-msehi.
  DATA: lv_dec  TYPE p DECIMALS 3.
  DATA: lv_txt  TYPE c LENGTH 5.
  DATA: lv_vlr_tot_liq TYPE konv-kwert.
*  DATA: lv_vlr_unit_brt TYPE konv-kkurs.

  DATA: lv_vlr_unit_brt TYPE konv-kaqty.
  SELECT SINGLE msehi
         FROM t006
         WHERE decan = 0
         INTO (@lv_unit).
**<<<------"160696 - NMS - INI------>>>
  CLEAR: vg_task_ativa, vg_tasks, vg_max_tasks.
* Obter o número de sessões disponíveis e a máxima.
  CALL FUNCTION 'SPBT_INITIALIZE'
    IMPORTING
      free_pbt_wps                   = vg_tasks
      max_pbt_wps                    = vg_max_tasks
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.
**<<<------"160696 - NMS - FIM------>>>
  CLEAR: vlin,vtot.
  DESCRIBE TABLE it_ekko LINES vtot.
  ctot = vtot.
  LOOP AT it_ekko INTO wa_ekko. " Cabeçalho da fatura
    ADD 1 TO vlin.
    clin = vlin.
    CLEAR: wa_ekpo.
    CONCATENATE 'Pedido '  wa_ekko-ebeln 'Linha ' clin '/' ctot INTO vmsg SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = vmsg.

    wa_saida-verkf = wa_ekko-verkf. "RJF
    wa_saida-telf1 = wa_ekko-telf1. "RSA
    wa_saida-bedat = wa_ekko-bedat.
**<<<------"160696 - NMS - INI------>>>
*    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln EQ wa_ekko-ebeln. "Item
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekko-ebeln BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      DATA(vl_tabix) = sy-tabix.

    ELSE.
      CLEAR: wa_ekko, wa_saida.
      CONTINUE.

    ENDIF.

    LOOP AT it_ekpo INTO wa_ekpo FROM vl_tabix. "Item
      IF wa_ekko-ebeln NE wa_ekpo-ebeln.
        CLEAR: wa_lfa1, wa_ekbe2, wa_konv, lv_vlr_tot_liq, lv_vlr_unit_brt.
        EXIT.

      ENDIF.
**<<<------"160696 - NMS - FIM------>>>
      CLEAR: wa_lfa1, wa_ekbe2, wa_konv, lv_vlr_tot_liq, lv_vlr_unit_brt.

      v_meng_ori = wa_ekpo-menge.
      wa_saida-mwskz = wa_ekpo-mwskz.
      wa_saida-lgort = wa_ekpo-lgort. "RJF
      wa_saida-verkf = wa_ekko-verkf. "RJF
      wa_saida-telf1 = wa_ekko-telf1. "RSA

      READ TABLE it_lfa1  INTO wa_lfa1  WITH KEY lifnr = wa_ekko-lifnr BINARY SEARCH. " Fornecedor

      READ TABLE it_ekbe2 INTO wa_ekbe2 WITH KEY ebeln = wa_ekpo-ebeln
                                                 ebelp = wa_ekpo-ebelp BINARY SEARCH. " fatura
      IF ( sy-subrc EQ 0 ).

        wa_saida-qtd    = wa_ekbe2-menge.
        wa_ekpo-menge  = wa_ekpo-menge -  wa_ekbe2-menge_a. "atualiza qtde pedido sem a qtdo faturada periodo anterior

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
        IF wa_ekpo-elikz = 'X'. "REMESSA FINAL
          wa_saida-qtremfinal = wa_ekpo-menge - wa_ekbe2-menge.
          wa_saida-saldo      = wa_ekpo-menge - wa_ekbe2-menge - wa_saida-qtremfinal.
        ELSE.
          wa_saida-saldo  = wa_ekpo-menge - wa_ekbe2-menge.
        ENDIF.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
      ELSE.
                                                            "BUG 160661
        IF wa_ekpo-elikz = 'X'. "REMESSA FINAL
          CLEAR wa_saida-saldo.                             "BUG 160661
        ELSE.
          wa_saida-saldo = wa_ekpo-menge.
        ENDIF.
      ENDIF.

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo2
*      IF wa_ekpo-elikz = 'X'. "REMESSA FINAL
*        wa_saida-saldo = 0.
*      ENDIF.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo2

      IF wa_ekpo-menge = 0 AND wa_saida-saldo = 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_ekko-knumv
                                               kposn = wa_ekpo-ebelp
                                               kschl = 'RB00'         "--desconto Absoluto do item
                                               BINARY SEARCH.         "<<<------"160696 - NMS------>>>
      IF ( sy-subrc EQ 0 ).
        wa_saida-netpr_desc  = wa_konv-kwert.
      ENDIF.

      CLEAR: wa_konv.
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_ekko-knumv
                                               kposn = wa_ekpo-ebelp
                                               kschl = 'ZB00'         "--suplemento Absoluto do item
                                               BINARY SEARCH.         "<<<------"160696 - NMS------>>>
      IF ( sy-subrc EQ 0 ).
        wa_saida-netpr_supl  = wa_konv-kwert.
      ENDIF.

      CLEAR: wa_konv.
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_ekko-knumv
                                               kposn = wa_ekpo-ebelp
                                               kschl = 'PBXX'
                                               BINARY SEARCH.         "<<<------"160696 - NMS------>>>

      wa_saida-waers = wa_konv-waers.

      lv_vlr_tot_liq = wa_konv-kwert + wa_saida-netpr_desc + wa_saida-netpr_supl.

*      IF WA_EKPO-MWSKZ EQ 'I8'.
*        UNASSIGN <WMWST>.
*        PERFORM BUSCA_IMPOSTO USING WA_EKKO-EBELN WA_EKPO-EBELP.
*        IF V_MENG_ORI GT 0.
*          IF WA_EKPO-BPRME CS 'TO'.
*            WA_SAIDA-KBETR = ( ( <WMWST> + WA_KONV-KWERT ) / V_MENG_ORI ) * 1000.
*          ELSE.
*            WA_SAIDA-KBETR = ( ( <WMWST> + WA_KONV-KWERT ) / V_MENG_ORI  ).
*          ENDIF.
*        ENDIF.
*
*      ELSE.
*        WA_SAIDA-KBETR = WA_KONV-KBETR.
*      ENDIF.
**<<<------"160696 - NMS - INI------>>>
*      vfator = 1.
*      UNASSIGN <wmwst>.
*      PERFORM busca_imposto USING wa_ekko-ebeln wa_ekpo-ebelp.
*
*      IF v_meng_ori GT 0.
*        IF wa_ekpo-bprme CS 'TO'.
*          vfator = 1000.
*          lv_vlr_unit_brt = ( ( <wmwst> + lv_vlr_tot_liq ) / v_meng_ori ) * 1000.
*          wa_saida-kbetr = lv_vlr_unit_brt. "( ( <wmwst> + lv_vlr_tot_liq ) / v_meng_ori ) * 1000.
*        ELSE.
*      lv_vlr_unit_brt = ( ( <wmwst> + lv_vlr_tot_liq ) / v_meng_ori  ).8998
*          wa_saida-kbetr = lv_vlr_unit_brt.
*        ENDIF.
*      ENDIF.
*
*
**      wa_saida-kbetr_ped = ( lv_vlr_unit_brt * wa_ekpo-menge ) / vfator.
*      wa_saida-kbetr_imp = <wmwst>.
*      wa_saida-kbetr_tot = ( <wmwst> + lv_vlr_tot_liq ).
**      wa_saida-kbetr_fat = wa_ekbe2-RMWWR."( lv_vlr_unit_brt * wa_saida-qtd ) / vfator.
*
**<<<------"160696 - NMS - FIM------>>>
*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo2
**<<<------"160696 - NMS - INI------>>>
*      LOOP AT it_ekbe INTO DATA(ls_ekbe) WHERE ebeln = wa_ekpo-ebeln
*                                           AND ebelp = wa_ekpo-ebelp.
      READ TABLE it_ekbe TRANSPORTING NO FIELDS WITH KEY ebeln = wa_ekpo-ebeln
                                                         ebelp = wa_ekpo-ebelp
                                                BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        DATA(vl_tabix2) = sy-tabix.

        LOOP AT it_ekbe INTO DATA(ls_ekbe) FROM vl_tabix2.
          IF ls_ekbe-ebeln NE wa_ekpo-ebeln OR
             ls_ekbe-ebelp NE wa_ekpo-ebelp.
            EXIT.

          ENDIF.
**<<<------"160696 - NMS - FIM------>>>
          DATA(ls_rseg) = VALUE #( it_rseg[ ebeln = ls_ekbe-ebeln
                                            ebelp = ls_ekbe-ebelp
**<<<------"184428 - NMS - INI------>>>
*                                            BELNR = LS_EKBE-BELNR ] OPTIONAL ).
                                            belnr = ls_ekbe-belnr
                                            gjahr = ls_ekbe-gjahr
                                            buzei = ls_ekbe-buzei ] OPTIONAL ).
**<<<------"184428 - NMS - FIM------>>>
          IF ls_ekbe-shkzg = 'S'.
            lv_wrbtr_sum += ls_rseg-wrbtr.

          ELSEIF ls_ekbe-shkzg = 'H'.
            lv_wrbtr_subtract += ls_rseg-wrbtr.

          ENDIF.

***        lr_refkey = VALUE #( BASE lr_refkey
***                             ( sign   = 'I'
***                               option = 'EQ'
***                               low    = |{ ls_rseg-belnr }{ ls_rseg-gjahr }| ) ).
***
***        lr_buzei = VALUE #( BASE lr_refkey
***                            ( sign   = 'I'
***                              option = 'EQ'
***                              low    = ls_rseg-buzei * 10 ) ).

        ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
      ENDIF.
**<<<------"160696 - NMS - FIM------>>>
      lv_total_liq_faturado = CONV #( lv_wrbtr_sum - lv_wrbtr_subtract ).

      lv_refkey = |{ ls_rseg-belnr }{ ls_rseg-gjahr }|.
      lv_buzei  = ls_rseg-buzei * 10.

      DATA(ls_lin_rseg) = VALUE #( it_lin_rseg[ refkey = lv_refkey
                                                itmnum  = lv_buzei ] OPTIONAL ).

      IF ls_lin_rseg IS NOT INITIAL.

        lv_imposto_nota_fiscal = REDUCE #( INIT sum TYPE j_1bnfstx-taxval
                                             FOR ls_j_1bnfstx IN it_j_1bnfstx
                                             WHERE ( docnum = ls_lin_rseg-docnum   "*-US192338-06.10.2025-#192338-JT-inicio
                                               AND   itmnum = ls_lin_rseg-itmnum ) "*-US192338-06.10.2025-#192338-JT-inicio
                                              NEXT sum += ls_j_1bnfstx-taxval ).

        DATA(ls_rbkp_rseg) = VALUE #( it_rbkp_rseg[ belnr = ls_ekbe-belnr
                                                    gjahr = ls_ekbe-gjahr ] OPTIONAL ).

        IF ls_rbkp_rseg-waers = 'USD' .
          lv_imposto_nota_fiscal = CONV #( lv_imposto_nota_fiscal / ls_rbkp_rseg-kursf ).
        ENDIF.
      ENDIF.

***      wa_saida-kbetr_fat = ( wa_saida-kbetr * wa_saida-qtd ) / vfator.
      wa_saida-kbetr_fat = CONV #( lv_total_liq_faturado + lv_imposto_nota_fiscal ).
      CLEAR: lv_wrbtr_sum,lv_wrbtr_subtract.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

      "-----------------------------------"Ajuste BUG SOLTO 142625 / AOENNING------------------------------------------

*   clear: wa_ekbe, wa_rbkp, VG_VLR_CONV.
*   LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln eq wa_ekpo-ebeln and ebelp eq wa_ekpo-ebelp.
*    READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr = wa_ekbe-belnr gjahr = wa_ekbe-gjahr BINARY SEARCH. " Valor fatura
*    IF ( sy-subrc EQ 0 ).
*      if WA_SAIDA-WAERS EQ 'USD'.
*
*        IF wa_ekbe-shkzg = 'H'. " Qte.Faturado
*            VG_VLR_CONV = ( wa_ekbe-menge * WA_SAIDA-KBETR ).
*            VG_VLR_CONV = - VG_VLR_CONV.  "wa_j_1bnflin-netwr.         " Valor Nota
*       ELSEIF wa_ekbe-shkzg = 'S'.
*            wa_ekbe-DMBTR = wa_ekbe-DMBTR.  "wa_j_1bnflin-netwr.         " Valor Nota
*            VG_VLR_CONV = ( wa_ekbe-menge * WA_SAIDA-KBETR ).
*       ENDIF.
*
*      add  VG_VLR_CONV to wa_saida-kbetr_fat.
**      wa_saida-kbetr_fat = ( wa_saida-kbetr_fat + VG_VLR_CONV ).
*     ELSE.
*
*       IF wa_ekbe-shkzg = 'H'. " Qte.Faturado
*            wa_ekbe-DMBTR = - wa_ekbe-DMBTR.  "wa_j_1bnflin-netwr.         " Valor Nota
*       ELSEIF wa_ekbe-shkzg = 'S'.
*            wa_ekbe-DMBTR = wa_ekbe-DMBTR.  "wa_j_1bnflin-netwr.         " Valor Nota
*       ENDIF.
*       add  wa_ekbe-DMBTR to wa_saida-kbetr_fat.
**       wa_saida-kbetr_fat = ( wa_saida-kbetr_fat + wa_ekbe-DMBTR ).
*     endif.
*    ENDIF.
*    clear: VG_VLR_CONV, wa_rbkp.
*  ENDLOOP.




      "-----------------------------------"Ajuste BUG SOLTO 142625 / AOENNING------------------------------------------
**<<<------"160696 - NMS - INI------>>>
**>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
*      IF wa_ekpo-elikz = 'X'. "REMESSA FINAL
*
*        wa_saida-vlremfinal = wa_saida-kbetr_tot - wa_saida-kbetr_fat.
*        wa_saida-kbetr_sal  = wa_saida-kbetr_tot - wa_saida-kbetr_fat - wa_saida-vlremfinal.
*
*      ELSE.
*        wa_saida-kbetr_sal = wa_saida-kbetr_tot - wa_saida-kbetr_fat.
*      ENDIF.
*
**      wa_saida-kbetr_sal = wa_saida-kbetr_tot - wa_saida-kbetr_fat. "( lv_vlr_unit_brt * wa_saida-saldo ) / vfator.
**<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
      CLEAR es_parametro.
      es_parametro-ebeln       = wa_ekko-ebeln.
      es_parametro-ebelp       = wa_ekpo-ebelp.
      es_parametro-meng_ori    = v_meng_ori.
      es_parametro-qtde_fat    = wa_saida-qtd.         "*-US192338-06.10.2025-#192338-JT
      es_parametro-qtremfinal  = wa_saida-qtremfinal.  "*-US192338-06.10.2025-#192338-JT
      es_parametro-bprme       = wa_ekpo-bprme.
      es_parametro-kbetr       = wa_saida-kbetr.
      es_parametro-kbetr_imp   = wa_saida-kbetr_imp.
      es_parametro-vlr_tot_liq = lv_vlr_tot_liq.
      es_parametro-kbetr_tot   = wa_saida-kbetr_tot.   "*-US192338-06.10.2025-#192338-JT-inicio
      es_parametro-elikz       = wa_ekpo-elikz.
      es_parametro-kbetr_fat   = wa_saida-kbetr_fat.
      es_parametro-kbetr_sal   = wa_saida-kbetr_sal.
      es_parametro-vlremfinal  = wa_saida-vlremfinal.
* Parâmetros Func. Paral. Busca Imposto
      PERFORM busca_imposto USING es_parametro.
**<<<------"160696 - NMS - FIM------>>>
      wa_saida-unid_p = wa_ekpo-bprme.
      wa_saida-matkl  = wa_ekpo-matkl.
      wa_saida-aedat = wa_ekko-aedat.
      wa_saida-angnr = wa_ekko-angnr.
      wa_saida-ihran = wa_ekko-ihran.
      wa_saida-inco1 = wa_ekko-inco1.
      wa_saida-werks = wa_ekpo-werks.
      wa_saida-ihrez = wa_ekko-ihrez.
**<<<------"184428 - NMS - INI------>>>
* Busca a taxa de Curva.
      PERFORM zf_busca_taxa_curva USING    wa_ekko-ebeln
                                  CHANGING wa_saida-taxa_curva.
**<<<------"184428 - NMS - FIM------>>>
      CASE wa_ekpo-loekz.
        WHEN 'S'.
          CONCATENATE icon_locked ' - Bloqueado' INTO wa_saida-s_pedido SEPARATED BY space.
        WHEN 'L'.
          CONCATENATE icon_delete ' - Eliminado' INTO wa_saida-s_pedido SEPARATED BY space.
        WHEN ''.
          CONCATENATE icon_system_okay ' - Ativo' INTO wa_saida-s_pedido SEPARATED BY space.
      ENDCASE.

      wa_saida-menge  = wa_ekpo-menge.
      wa_saida-name1  = wa_lfa1-name1.
      wa_saida-ebeln  = wa_ekko-ebeln.
      wa_saida-ebelp  = wa_ekpo-ebelp.
      wa_saida-matnr  = wa_ekpo-matnr.
      wa_saida-txz01  = wa_ekpo-txz01.
**<<<------"184428 - NMS - INI------>>>
      vl_object = wa_saida-matnr.
      CALL METHOD cll_mm_util->get_caracteristica_geral
        EXPORTING
          i_class                = cl_class
          i_classtype            = cl_classtype
          i_object               = vl_object
          i_caracteristica       = cl_cultivar
        IMPORTING
          e_valor_caracteristica = DATA(vl_value).

      wa_saida-cultivar = CONV #( vl_value ).
**<<<------"184428 - NMS - FIM------>>>
**  Begin of CS2022000771   #83814 FF   25.01.2023
      IF wa_lfa1-stkzn IS NOT INITIAL.
        wa_saida-stcd1  = wa_lfa1-stcd2.
      ELSE.
        wa_saida-stcd1  = wa_lfa1-stcd1.
      ENDIF.

      READ TABLE it_eban INTO DATA(wa_eban) WITH KEY banfn =  ekpo-banfn.
      IF sy-subrc = 0.
*        wa_saida-badat = wa_eban-badat.
      ENDIF.

      "Valor do adiantamento por item.
**<<<------"160696 - NMS - INI------>>>
*      LOOP AT it_zfit0046 INTO DATA(wa_0046) WHERE ebeln = wa_ekpo-ebeln AND ebelp = wa_ekpo-ebelp.
**      IF sy-subrc = 0.
*        ADD  wa_0046-vlr_adiantamento TO wa_saida-adiantamento.
**        wa_saida-nro_sol      = wa_0046-nro_sol.
**      ENDIF.
*      ENDLOOP.
*
*      READ TABLE it_zfit0045 INTO DATA(wa_0045) WITH KEY ebeln = wa_ekpo-ebeln.
      READ TABLE it_zfit0046_sum INTO DATA(wa_0046) WITH KEY ebeln = wa_ekpo-ebeln
                                                             ebelp = wa_ekpo-ebelp
                                                    BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        wa_saida-adiantamento = wa_0046-vlr_adiantamento.

      ENDIF.

      READ TABLE it_zfit0045 INTO DATA(wa_0045) WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
      IF sy-subrc = 0.
        wa_saida-status = wa_0045-status.
*        wa_saida-data_atual  = wa_0045-dt_atual.
      ENDIF.

      IF wa_ekpo-peinh IS NOT INITIAL.
        wa_saida-netpr_liq = wa_ekpo-netpr / wa_ekpo-peinh.
      ENDIF.
** End of FF  25.01.2023

      IF ( wa_ekpo-meins EQ 'BAG' ).
        wa_saida-unid = 'SAC'.
      ELSEIF ( wa_ekpo-meins EQ 'BIG' ).
        wa_saida-unid = 'BAG'.
      ELSE.
        wa_saida-unid = wa_ekpo-meins.
      ENDIF.

      wa_saida-zterm  = wa_ekko-zterm.
      wa_saida-submi  = wa_ekko-submi.
      wa_saida-bsart  = wa_ekko-bsart.
      wa_saida-netpr_germ  = 0.
      wa_saida-netpr_roya  = 0.

      READ TABLE it_0035 INTO wa_0035
        WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.

*-US192338-06.10.2025-#192338-JT-inicio
      IF sy-subrc <> 0.
        READ TABLE it_0035 INTO wa_0035 WITH KEY nro_sol_cp = wa_ekko-nro_sol_cp.
      ENDIF.
*-US192338-06.10.2025-#192338-JT-fim

      IF sy-subrc = 0. "AND wa_ekko-bsart = 'ZSEM'.        READ TABLE it_0037 INTO wa_0037 WITH KEY nro_sol_cp = wa_0035-nro_sol_cp ebelp = wa_ekpo-ebelp.

        IF sy-subrc = 0.
          wa_saida-netpr_germ  = wa_0037-netpr_germ.
          wa_saida-netpr_roya  = wa_0037-netpr_roya.
          wa_saida-cultura     = wa_0035-cultura.     "**<<<------"184428 - NMS------>>>
          wa_saida-safra       = wa_0035-safra.       "**<<<------"184428 - NMS------>>>
*          wa_saida-netpr_desc  = wa_0037-netpr_desc.
*          wa_saida-netpr_supl  = wa_0037-netpr_supl.
        ENDIF.
      ENDIF.

      wa_saida-data_criacao  =  wa_0035-data_criacao.

*      "Desconto valor saldo Valor total pedido.
*      IF wa_saida-netpr_desc IS NOT INITIAL AND wa_saida-kbetr_tot IS NOT INITIAL.
*        wa_saida-kbetr_tot = wa_saida-kbetr_tot - wa_saida-netpr_desc.
*      ENDIF.
*
*      IF wa_saida-netpr_supl  IS NOT INITIAL AND wa_saida-kbetr_tot IS NOT INITIAL.
*        wa_saida-kbetr_tot = wa_saida-kbetr_tot + wa_saida-netpr_supl .
*      ENDIF.
*
*      "Desconto valor saldo Valor Saldo Bruto.
*      IF wa_saida-netpr_desc IS NOT INITIAL AND wa_saida-kbetr_sal IS NOT INITIAL.
*        wa_saida-kbetr_sal = wa_saida-kbetr_sal - wa_saida-netpr_desc.
*      ENDIF.
*
*      IF wa_saida-netpr_supl  IS NOT INITIAL AND wa_saida-kbetr_sal IS NOT INITIAL.
*        wa_saida-kbetr_sal = wa_saida-kbetr_sal + wa_saida-netpr_supl.
*      ENDIF.

      READ TABLE it_0036 INTO wa_0036 WITH KEY nro_sol_cp = wa_0035-nro_sol_cp BINARY SEARCH.
      IF sy-subrc = 0.
        lv_cont = 0.
        LOOP AT it_0036 INTO wa_0036 FROM sy-tabix.
          IF wa_0036-nro_sol_cp NE wa_0035-nro_sol_cp.
            EXIT.
          ENDIF.
          lv_cont = lv_cont + 1.
          lv_field = 'DT_VENC' && lv_cont.
          CONDENSE lv_field.
          .
          ASSIGN COMPONENT lv_field OF STRUCTURE <fs_wa>
                TO <fs_field>.
          IF sy-subrc = 0.
            <fs_field> = wa_0036-dt_vcto+6(2) && '.' && wa_0036-dt_vcto+4(2) && '.' && wa_0036-dt_vcto(4).
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE it_0036_cont INTO wa_0036_cont INDEX 1.
        IF sy-subrc = 0.
          DO.
            lv_cont = lv_cont + 1.
            IF lv_cont > wa_0036_cont-cont.
              EXIT.
            ELSE.
              lv_field = 'DT_VENC' && lv_cont.
              CONDENSE lv_field.

              ASSIGN COMPONENT lv_field OF STRUCTURE <fs_wa>
                    TO <fs_field>.
              IF sy-subrc = 0.
                <fs_field> = ''.
              ENDIF.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'QTD' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> =  wa_ekbe2-menge.
*        WRITE wa_ekbe2-menge TO <fs_field> UNIT lv_unit.
*        CONDENSE <fs_field> NO-GAPS.
*        FIND ',' IN <fs_field>.
*        IF sy-subrc = 0.
*          lv_dec = FRAC( wa_ekbe2-menge ).
*          wa_ekbe2-menge = wa_ekbe2-menge - lv_dec.
*          WRITE wa_ekbe2-menge TO <fs_field> UNIT lv_unit.
*          CONDENSE <fs_field> NO-GAPS.
*          lv_txt = lv_dec.
*          <fs_field> = <fs_field> && ',' && lv_txt+2(3).
*        else.
*          <fs_field> = <fs_field> && ',000'.
*        ENDIF.
      ENDIF.

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
      ASSIGN COMPONENT 'QTREMFINAL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-qtremfinal.
      ENDIF.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

      ASSIGN COMPONENT 'ZSALDO' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-saldo.
*        lv_qtde =  wa_saida-saldo.
*        WRITE lv_qtde TO <fs_field> UNIT lv_unit.
*        CONDENSE <fs_field> NO-GAPS.
*        FIND ',' IN <fs_field>.
*        IF sy-subrc = 0.
*          lv_dec = FRAC( lv_qtde ).
*          lv_qtde = lv_qtde - lv_dec.
*          WRITE lv_qtde TO <fs_field> UNIT lv_unit.
*          CONDENSE <fs_field> NO-GAPS.
*          lv_txt = lv_dec.
*          <fs_field> = <fs_field> && ',' && lv_txt+2(3).
*        else.
*          <fs_field> = <fs_field> && ',000'.
*        ENDIF.
      ENDIF.
      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-waers.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'MWSKZ' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-mwskz.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'KBETR' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-kbetr.
      ENDIF.
      ASSIGN COMPONENT 'KBETR_PED' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-kbetr_ped.
      ENDIF.
      ASSIGN COMPONENT 'KBETR_FAT' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-kbetr_fat.
      ENDIF.

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
      ASSIGN COMPONENT 'VLREMFINAL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-vlremfinal.
      ENDIF.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

      ASSIGN COMPONENT 'KBETR_SAL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-kbetr_sal.
      ENDIF.
      ASSIGN COMPONENT 'KBETR_IMP' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-kbetr_imp.
      ENDIF.
      ASSIGN COMPONENT 'KBETR_TOT' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-kbetr_tot.
      ENDIF.
      ASSIGN COMPONENT 'UNID_P' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-unid_p.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'MATKL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-matkl.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'ZAEDAT' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-aedat+6(2) && '.' && wa_saida-aedat+4(2) && '.' && wa_saida-aedat(4).
        CONDENSE <fs_field>.
      ENDIF.

      ASSIGN COMPONENT 'DATA_CRIACAO' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
*        <fs_field> = wa_saida-data_criacao.
        <fs_field> = wa_saida-data_criacao+6(2) && '.' && wa_saida-data_criacao+4(2) && '.' && wa_saida-data_criacao(4).
        CONDENSE <fs_field>.
      ENDIF.

      ASSIGN COMPONENT 'ANGNR' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-angnr.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'IHRAN' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-ihran+6(2) && '.'&& wa_saida-ihran+4(2) && '.'&& wa_saida-ihran(4).
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'INCO1' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-inco1.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-werks.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'IHREZ' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-ihrez.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'S_PEDIDO' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-s_pedido.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-menge.
*        lv_qtde = wa_saida-menge.
*        WRITE lv_qtde TO <fs_field> UNIT lv_unit.
*        CONDENSE <fs_field> NO-GAPS.
*        FIND ',' IN <fs_field>.
*        IF sy-subrc = 0.
*          lv_dec = FRAC( lv_qtde ).
*          lv_qtde = lv_qtde - lv_dec.
*          WRITE lv_qtde TO <fs_field> UNIT lv_unit.
*          CONDENSE <fs_field> NO-GAPS.
*          lv_txt = lv_dec.
*          <fs_field> = <fs_field> && ',' && lv_txt+2(3).
*        else.
*          <fs_field> = <fs_field> && ',000'.
*        ENDIF.
      ENDIF.
      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_lfa1-lifnr.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'NAME1' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-name1.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-ebeln.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-ebelp.
        CONDENSE <fs_field>.
        PACK <fs_field> TO <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-matnr.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'TXZ01' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-txz01.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'UNID' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-unid.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'ZTERM' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-zterm.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'SUBMI' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-submi.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'BSART' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-bsart.
        CONDENSE <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'NETPR_GERM' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-netpr_germ.
      ENDIF.
      ASSIGN COMPONENT 'NETPR_ROYA' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-netpr_roya.
      ENDIF.

      ASSIGN COMPONENT 'LGORT' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-lgort.
      ENDIF.

      ASSIGN COMPONENT 'VERKF' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-verkf.
      ENDIF.

      ASSIGN COMPONENT 'TELF1' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-telf1.
      ENDIF.

**  Begin of CS2022000771   #83814 FF   26.01.2023
      ASSIGN COMPONENT 'BEDAT' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0 AND wa_saida-bedat IS NOT INITIAL.
        <fs_field> = wa_saida-bedat+6(2) && '.' && wa_saida-bedat+4(2) && '.' && wa_saida-bedat(4).
*        <fs_field> = wa_saida-bedat.
        CONDENSE <fs_field>.
      ENDIF.

      ASSIGN COMPONENT 'STCD1' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-stcd1.
      ENDIF.

      ASSIGN COMPONENT 'ADIANTAMENTO' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0 AND wa_saida-adiantamento IS NOT INITIAL  .
        <fs_field> = wa_saida-adiantamento.
      ENDIF.

      ASSIGN COMPONENT 'STATUS' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-status.
      ENDIF.

      ASSIGN COMPONENT 'NRO_SOL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0 AND wa_saida-nro_sol IS NOT INITIAL.
        <fs_field> = wa_saida-nro_sol.
      ENDIF.

      ASSIGN COMPONENT 'NETPR_LIQ' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-netpr_liq.
      ENDIF.

      ASSIGN COMPONENT 'NETPR_DESC' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-netpr_desc.
      ENDIF.

      ASSIGN COMPONENT 'NETPR_SUPL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = wa_saida-netpr_supl.
      ENDIF.
** End of FF  26.01.2023
**<<<------"184428 - NMS - INI------>>>
      ASSIGN COMPONENT 'CULTURA' OF STRUCTURE <fs_wa> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.
        <fs_field> = wa_saida-cultura.

      ENDIF.

      ASSIGN COMPONENT 'TAXA_CURVA' OF STRUCTURE <fs_wa> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.
        <fs_field> = wa_saida-taxa_curva.

      ENDIF.

      ASSIGN COMPONENT 'SAFRA' OF STRUCTURE <fs_wa> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.
        <fs_field> = wa_saida-safra.

      ENDIF.

      ASSIGN COMPONENT 'CULTIVAR' OF STRUCTURE <fs_wa> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.
        <fs_field> = wa_saida-cultivar.

      ENDIF.
**<<<------"184428 - NMS - FIM------>>>
      APPEND wa_saida TO it_saida.
      APPEND <fs_wa> TO <fs_table>.


      CLEAR:
            wa_lfa1,
            wa_ekbe2,
            aux,
            wa_saida,
            <fs_wa>,
            wa_0045,
            wa_0046,
            wa_ekpo.

    ENDLOOP.
    CLEAR: wa_ekko, wa_0045, wa_0046.
  ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
* Esperar até que todas as sessões sejam finalizadas. O número é decrementado na subrtoina
* de retorno da função e incrementado antes da RFC ser chamada.
  WAIT UNTIL vg_task_ativa EQ 0.
**<<<------"160696 - NMS - FIM------>>>
  "===========================USER STORY 83814 / Anderson Oenning
*  IF  p_sol IS NOT INITIAL.
*    DELETE it_saida WHERE nro_sol NOT IN p_sol.
*
*    SORT <fs_table> BY ('NRO_SOL').
*    DELETE FROM (<fs_table>) WHERE ('NRO_SOL') NOT IN p_sol[].
*  ENDIF.

ENDFORM.                    " FORM_SAIDA


*&---------------------------------------------------------------------*
*&      Form  FORM_ALV
*&---------------------------------------------------------------------*

FORM form_alv_mm .

  PERFORM alv_preenche_cat USING:
        'S_PEDIDO'           TEXT-005      '12'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Status Pedido
        'LIFNR'              TEXT-079      '12'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Fornecedor
        'NAME1'              TEXT-004      '35'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Fornecedor
        'ZAEDAT'             TEXT-113      '10'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Data da Compra
        'EBELN'              TEXT-018      '10'     ' ' 'X'  '<FS_TABLE>'     ''    ''       ' ', " Pedido
        'EBELP'              TEXT-006      '5'      ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Item
        'MATNR'              TEXT-007      '18'     'X' ' '  '<FS_TABLE>'     ''    ''       ' ', " Material
        'CULTURA'            TEXT-127      '8'      ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Cultura            "**<<<------"184428 - NMS------>>>
        'SAFRA'              TEXT-129      '10'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Safra              "**<<<------"184428 - NMS------>>>
        'CULTIVAR'           TEXT-130      '15'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', " Cultivar           "**<<<------"184428 - NMS------>>>
        'TXZ01'              TEXT-037      '35'     'X' ' '  '<FS_TABLE>'     ''    ''       ' ', " Material
        'MENGE'              TEXT-008      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', " Qte.Pedido
        'UNID'               TEXT-035      '3'      ' ' ' '  '<FS_TABLE>'     ''    ''       'R', " Qte.Pedido
        'QTD'                TEXT-009      '13'     ' ' 'X'  '<FS_TABLE>'     'X'   'C500'   'R', " Qte.Faturado
*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
        'QTREMFINAL'         TEXT-125      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', " Qte. Rem. Final
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
        'ZSALDO'             TEXT-010      '18'     ' ' ' '  '<FS_TABLE>'     'X'   'C300'   'R', " SALDO
        'WAERS'              TEXT-038      '5'      ' ' ' '  '<FS_TABLE>'     ' '   ' '      ' ', " Moeda
        'KBETR'              ''            '13'     ' ' ' '  '<FS_TABLE>'     ''    'C500'   'R', " Vl. Unit Br
        'NETPR_LIQ'          TEXT-039       '13'    ' ' ' '  '<FS_TABLE>'     ''    'C500'   'R' , " Vl. Unit Lq
        'KBETR_IMP'          TEXT-058      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', " Valor imposto
        'NETPR_DESC'         TEXT-114       '10'    ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R' , " Valor desconto
        'NETPR_SUPL'         TEXT-115       '10'    ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R' , " Valor suplemento
*-US192338-06.10.2025-#192338-JT-inicio
       'KBETR_TOT'          TEXT-057      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', " Vl. Pedido Bt
       'KBETR_FAT'          TEXT-065      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R' , " Vl.Faturado
*      'KBETR_FAT'          TEXT-057      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', " Vl. Pedido Bt
*      'KBETR_TOT'          TEXT-065      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R' , " Vl.Faturado
*-US192338-06.10.2025-#192338-JT-inicio
*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
        'VLREMFINAL'         TEXT-126      '13'      ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R' ,  " Vl. Rem. Final.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
        'KBETR_SAL'          TEXT-066      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R' , " Vl. Saldo Bt
        'ADIANTAMENTO'       TEXT-109       '13'    ' ' 'X'  '<FS_TABLE>'     'X'   'C500'   'R' , "Adiantamento
        'ANGNR'              TEXT-053      '13'     ' ' ' '  '<FS_TABLE>'     ' '   ' '      'R', " Tx. Hedge
        'TAXA_CURVA'         TEXT-128      '13'     ' ' ' '  '<FS_TABLE>'     ' '   ' '      'R', " Tx. Curva
        'IHRAN'              TEXT-054      '10'     ' ' ' '  '<FS_TABLE>A'    ' '   ' '      'R', " Data Vencimento
        'MWSKZ'              'IMPOSTO'     '04'     ' ' ' '  '<FS_TABLE>'     ''    ''       'R',
        'INCO1'              TEXT-059      '10'     ' ' ' '  '<FS_TABLE>'     ' '   ''       ' ', " incoterms
        'WERKS'              TEXT-060      '10'     ' ' ' '  '<FS_TABLE>'     ' '   ''       ' ', " centro
        'IHREZ'              TEXT-061      '10'     ' ' ' '  '<FS_TABLE>'     ' '   ''       ' ', " nossa referencia
        'ZTERM'              TEXT-074      '10'     ' ' ' '  '<FS_TABLE>'     ' '   ''       ' ', " nossa referencia
        'UNID_P'             TEXT-062      '3'      ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', "
        'MATKL'              TEXT-063      '10'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', "
        'SUBMI'              TEXT-075      '10'     ' ' 'X'  '<FS_TABLE>A'    ''    ''       ' ', "
        'BSART'              TEXT-076      '07'     ' ' ' '  '<FS_TABLE>'     ''    ''       ' ', "
        'NETPR_GERM'         TEXT-077      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', "
        'NETPR_ROYA'         TEXT-078      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   'R', "
*        'KBETR_PED'          TEXT-064      '13'     ' ' ' '  '<FS_TABLE>'     'X'   'C500'   , " Valor total
        'LGORT'              TEXT-095      '13'     ' ' ' '  '<FS_TABLE>'     'X'   ''       'R' , " Depósito
        'VERKF'              TEXT-096      '30'     ' ' ' '  '<FS_TABLE>'     'X'   ''       'R' , " Vendedor c.
        'TELF1'              TEXT-124      '30'     ' ' ' '  '<FS_TABLE>'     'X'   ''       'R' , " Páis de Origem.
**  Begin of CS2022000771   #83814 FF   25.01.2023
        'DATA_CRIACAO'       TEXT-108       '15'    ' ' ' '  '<FS_TABLE>'     'X'   '   '    ' ' , "Data da solicitação de compras.
        'STCD1'              TEXT-101       '16'    ' ' ' '  '<FS_TABLE>'     'X'   '   '    ' ' , "CPF/CNPJ
        'STATUS'             TEXT-110       '05'    ' ' ' '  '<FS_TABLE>'     'X'   '   '    ' ' . "Status
*        'NRO_SOL'            text-112       '10'    ' ' 'X'  '<FS_TABLE>'     ' '   '    ' ' ' , " Nº da solicitação
*        'BEDAT     '         text-113       '10'    ' ' ' '  '<FS_TABLE>'     ' '   '    ' ' ' , " Data pedido de compra

** End of FF  25.01.2023

  DATA: lv_cont TYPE sy-tabix.
  DATA: lv_field TYPE c LENGTH 20.
  DATA: lv_text TYPE c LENGTH 20.

  READ TABLE it_0036_cont INTO wa_0036_cont INDEX 1.
  IF sy-subrc = 0.
    DO.
      lv_cont = lv_cont + 1.
      IF lv_cont > wa_0036_cont-cont.
        EXIT.
      ELSE.
        lv_field = 'DT_VENC' && lv_cont.
        lv_text = 'Vcto. ' && lv_cont.
        CONDENSE lv_field.
        PERFORM alv_preenche_cat USING:
             lv_field         lv_text      '10'     ' ' ' '  'IT_SAIDA'     ''    ''   ''    .
      ENDIF.
    ENDDO.
  ENDIF.
  PERFORM create_itab_dynamically.
ENDFORM.                    " FORM_ALV

*&---------------------------------------------------------------------*
*&      Form  FORM_SELECIONA_SD
*&---------------------------------------------------------------------*
FORM form_seleciona_sd .

  DATA  :
    somar            TYPE j_1bnflin-menge,
    diminuir         TYPE j_1bnflin-menge,
    somar_fat        TYPE vbrk-netwr,
    diminuir_fat     TYPE vbrk-netwr,
    somar_fat_liq    TYPE vbrk-netwr,
    diminuir_fat_liq TYPE vbrk-netwr,
    vbelnrfmg        TYPE vbfa-vbeln,
    vg_refkey_aux    TYPE j_1bnflin-refkey,
    vg_refkey        TYPE j_1bnflin-refkey.


  IF  p_tpcont  IS INITIAL AND p_simula IS INITIAL. "*-CS2021000218-14.09.2022-#90705-JT-inicio
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Tipo de Contrato !'.
    STOP.
  ENDIF.

  IF  p_orgven IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Organização de Vendas !'.
    STOP.
  ENDIF.

  "Check se usuário tem autorização para a Empresa
  IF p_orgven IS NOT INITIAL .

    AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
    ID 'BUKRS' FIELD p_orgven-low.

    IF sy-subrc <> 0.
      MESSAGE i000(z01) WITH 'Perfil do usuário sem acesso a esta Org. de Vendas'.
      STOP.
    ENDIF.

  ENDIF.

  IF  p_cdist IS INITIAL AND p_simula IS INITIAL. "*-CS2021000218-14.09.2022-#90705-JT-inicio
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Canal Distribuição !'.
    STOP.
  ENDIF.

  IF  p_sativ IS INITIAL AND p_simula IS INITIAL. "*-CS2021000218-14.09.2022-#90705-JT-inicio.
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Setor de Atividade !'.
    STOP.
  ENDIF.

  IF  p_datent IS INITIAL AND p_simula IS INITIAL. "*-CS2021000218-14.09.2022-#90705-JT-inicio.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data de Entrada !'.
    STOP.
  ENDIF.

  IF  p_fatuv-low  IS INITIAL
   OR p_fatuv-high IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data de Faturamento !'.
    STOP.
  ENDIF.

*-CS2021000218-14.09.2022-#90705-JT-inicio
  IF p_simula IS NOT INITIAL.
    SELECT doc_simulacao vbeln
      INTO TABLE it_zsdt0041
      FROM zsdt0041
     WHERE doc_simulacao IN p_simula.

    SELECT doc_simulacao vbeln dt_entrega
 APPENDING TABLE it_zsdt0041
      FROM zsdt0090
     WHERE doc_simulacao IN p_simula
       AND estorno        = abap_false. ""*-IR190416-10.09.2025-JT-inicio

    SELECT doc_simulacao vbelv dt_entrega
 APPENDING TABLE it_zsdt0041
      FROM zsdt0090
     WHERE doc_simulacao IN p_simula
       AND estorno        = abap_false. ""*-IR190416-10.09.2025-JT-inicio

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

*>>>Begin-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

    SELECT *
      FROM zfit0026
      INTO TABLE it_zfit0026_aux
      FOR ALL ENTRIES IN it_zsdt0041
      WHERE vbeln  =  it_zsdt0041-vbeln
        AND status IN ( 'G', 'A' ).
*        and docnum	 <> ''.

    DELETE it_zfit0026_aux WHERE docnum IS INITIAL AND ajuste IS INITIAL.

    SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel erdat lifsk faksk
      FROM vbak
      INTO TABLE it_vbak_aux
      FOR ALL ENTRIES IN it_zsdt0041
      WHERE vbeln = it_zsdt0041-vbeln.

    it_zsdt0041_aux[] = it_zsdt0041[].

*<<<End-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

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

  "DEVK9A1VQ7  - Melhorias - 137325 - RSA
*  IF NOT p_cont[] IS INITIAL.

  "ZSDT0051_OPERACAO_ZNFW

*ajuste busca operacao do set - alinhado 21/05/2024 com Quevedo -  BG - INICIO
  SELECT *
          FROM setleaf
          INTO TABLE @DATA(it_set)
         WHERE setname = 'ZSDT0051_OPERACAO_ZNFW'.

  LOOP AT it_set INTO DATA(wa_set).
    APPEND VALUE rsdsselopt( option = wa_set-valoption
                             sign   = wa_set-valsign
                             low    = wa_set-valfrom )
                        TO r_operacao.
  ENDLOOP.


  SELECT 09~vbeln 09~posnr 09~seq_lcto 09~menge 09~meins 09~netwr
         FROM zfiwrt0009 AS 09 INNER JOIN zfiwrt0008 AS 08
         ON 08~seq_lcto = 09~seq_lcto
         INTO TABLE it_zfiwrt0009
         FOR ALL ENTRIES IN it_vbak
*           WHERE 09~vbeln IN p_cont
         WHERE 09~vbeln EQ it_vbak-vbeln
         AND   08~bldat IN p_fatuv
         AND   08~operacao IN r_operacao "*ajuste busca operacao do set - alinhado 21/05/2024 com Quevedo -  BG
         AND   08~docs_estornados NE 'X'
         AND   09~vbeln_r NE space.
*ajuste busca operacao do set - alinhado 21/05/2024 com Quevedo -  BG - FIM
  SORT it_zfiwrt0009 BY vbeln posnr.
*  ENDIF.
  "DEVK9A1VQ7  - Melhorias - 137325 - RSA


  SELECT *
    APPENDING TABLE it_zsdt0041_safra
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

*-CS2021000218-14.09.2022-#90705-JT-fim

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
**<<<------"160696 - NMS - INI------>>>
    SORT: it_tvbur BY vkbur,
          it_tvkbt BY vkbur spras.
**<<<------"160696 - NMS - FIM------>>>
  ENDIF.

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
**<<<------"160696 - NMS - INI------>>>
    SORT: it_tvfs  BY faksp,
          it_tvfst BY faksp spras.
**<<<------"160696 - NMS - FIM------>>>
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
**<<<------"160696 - NMS - INI------>>>
    SORT: it_tvls  BY lifsp,
          it_tvlst BY lifsp spras.
**<<<------"160696 - NMS - FIM------>>>
  ENDIF.

  SELECT *
  FROM zsdt0271
    INTO TABLE it_zsdt0271
   FOR ALL ENTRIES IN it_vbak
  WHERE filial EQ it_vbak-vkbur .
  IF it_zsdt0271[] IS NOT INITIAL.
    SELECT *
     FROM zsdt0270
   INTO TABLE it_zsdt0270
     FOR ALL ENTRIES IN it_zsdt0271
   WHERE cod_regional EQ it_zsdt0271-cod_regional .
**<<<------"160696 - NMS - INI------>>>
    SORT: it_zsdt0271 BY filial,
          it_zsdt0270 BY cod_regional.
**<<<------"160696 - NMS - FIM------>>>
  ENDIF.

** PBI - 55017 - Fim

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

  SELECT doc_simulacao vbeln dt_entrega
    APPENDING TABLE it_zsdt0041
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln
     AND estorno   = abap_false. ""*-IR190416-10.09.2025-JT-inicio

  SELECT doc_simulacao vbelv dt_entrega
    APPENDING TABLE it_zsdt0041
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbelv EQ it_vbak-vbeln
     AND estorno   = abap_false. ""*-IR190416-10.09.2025-JT-inicio

*"// WBARBOSA BUG 183616 26/06/2025
  IF it_zsdt0041[] IS NOT INITIAL.
    SELECT *
       APPENDING TABLE it_zsdt0041_frete
      FROM zsdt0041
       FOR ALL ENTRIES IN it_zsdt0041
     WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.
  ENDIF.
*"// WBARBOSA BUG 183616 26/06/2025

*"// WBARBOSA BUG 183616 08/07/2025
  IF it_zsdt0041_frete[] IS NOT INITIAL.
    SELECT *
      INTO TABLE it_zsdt0090_frete
      FROM zsdt0090
       FOR ALL ENTRIES IN it_zsdt0041_frete
     WHERE doc_simulacao EQ it_zsdt0041_frete-doc_simulacao
       AND estorno        = abap_false. ""*-IR190416-10.09.2025-JT-inicio
  ENDIF.
*"// WBARBOSA BUG 183616 08/07/2025

  SELECT *
    APPENDING TABLE it_zsdt0090_frete
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE ( vbelv EQ it_vbak-vbeln
      OR   vbeln EQ it_vbak-vbeln )
     AND   estorno  = abap_false. ""*-IR190416-10.09.2025-JT-inicio


  SORT it_zsdt0041 BY vbeln doc_simulacao.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0041 COMPARING ALL FIELDS.

  SELECT ddtext domvalue_l
         FROM dd07t
         INTO TABLE it_dd07t
         WHERE domname    = 'ZSDD023'
         AND   ddlanguage = 'P'
         AND   as4local   = 'A'.
  SORT it_dd07t BY domvalue_l.

  IF it_zsdt0041[] IS NOT INITIAL.

    IF todos_sd    EQ abap_true." DEVK9A1PQG

      SELECT doc_simulacao vendedor cultura safra erdat dt_entrega_def dt_entrega_sem
             dt_entrega_fet meio_pago ecommerce id_order_ecommerce "*-CS2021000218-14.09.2022-#90705-JT-inicio
       INTO TABLE it_zsdt0040
       FROM zsdt0040
       FOR ALL ENTRIES IN it_zsdt0041
       WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    ELSEIF conv_sd EQ abap_true.

      SELECT doc_simulacao vendedor cultura safra erdat dt_entrega_def dt_entrega_sem
             dt_entrega_fet meio_pago ecommerce id_order_ecommerce "*-CS2021000218-14.09.2022-#90705-JT-inicio
      INTO TABLE it_zsdt0040
      FROM zsdt0040
      FOR ALL ENTRIES IN it_zsdt0041
      WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao
      AND   ecommerce     EQ abap_false.

    ELSEIF ecom_sd EQ abap_true.

      SELECT doc_simulacao vendedor cultura safra erdat dt_entrega_def dt_entrega_sem
             dt_entrega_fet meio_pago ecommerce id_order_ecommerce "*-CS2021000218-14.09.2022-#90705-JT-inicio
      INTO TABLE it_zsdt0040
      FROM zsdt0040
      FOR ALL ENTRIES IN it_zsdt0041
      WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao
      AND   ecommerce     EQ abap_true.

    ENDIF.

*>>>Begin-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

    IF it_zsdt0041_aux[] IS INITIAL.

      SELECT doc_simulacao vbeln
        FROM zsdt0041
        INTO TABLE it_zsdt0041_aux
        FOR ALL ENTRIES IN it_zsdt0041
        WHERE doc_simulacao = it_zsdt0041-doc_simulacao.

      SELECT doc_simulacao vbeln dt_entrega
        APPENDING TABLE it_zsdt0041_aux
        FROM zsdt0090
        FOR ALL ENTRIES IN it_zsdt0041
       WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao
         AND estorno        = abap_false. ""*-IR190416-10.09.2025-JT-inicio

      SELECT doc_simulacao vbelv dt_entrega
        APPENDING TABLE it_zsdt0041_aux
        FROM zsdt0090
        FOR ALL ENTRIES IN it_zsdt0041
       WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao
         AND estorno        = abap_false. ""*-IR190416-10.09.2025-JT-inicio

      SELECT *
        FROM zfit0026
        INTO TABLE it_zfit0026_aux
        FOR ALL ENTRIES IN it_zsdt0041_aux
        WHERE vbeln  =  it_zsdt0041_aux-vbeln
          AND status IN ( 'G', 'A' ).
*          and docnum <> ''.

      DELETE it_zfit0026_aux WHERE docnum IS INITIAL AND ajuste IS INITIAL.

      SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel erdat lifsk faksk
        FROM vbak
        INTO TABLE it_vbak_aux
        FOR ALL ENTRIES IN it_zsdt0041_aux
        WHERE vbeln = it_zsdt0041_aux-vbeln.

    ENDIF.

*<<<End-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

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
**<<<------"160696 - NMS - INI------>>>
        SORT: it_vbak BY vbeln,
              it_0026 BY obj_key,
              it_zib  BY obj_key.
**<<<------"160696 - NMS - FIM------>>>
        LOOP AT it_zib.
          READ TABLE it_0026
            WITH KEY obj_key = it_zib-obj_key
            BINARY SEARCH.                    "<<<------"160696 - NMS------>>>

          IF sy-subrc IS INITIAL.
**<<<------"160696 - NMS - INI------>>>
*            READ TABLE it_vbak INTO DATA(ws_vbak) WITH KEY vbeln = it_0026-vbeln.
            READ TABLE it_vbak INTO DATA(ws_vbak) WITH KEY vbeln = it_0026-vbeln BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
            MOVE : it_zib-bukrs   TO it_zib_bsid-bukrs,
                   it_0026-docnum TO it_zib_bsid-belnr,
                   ws_vbak-kunnr TO it_zib_bsid-kunnr,
                   it_zib-gjahr   TO it_zib_bsid-gjahr.

            IF it_0026-docnum = '0000000000'.
              MOVE it_zib-belnr TO it_zib_bsid-belnr.
            ENDIF.

            APPEND it_zib_bsid.
          ENDIF.
          CLEAR: it_zib_bsid, ws_vbak.
        ENDLOOP.

        SORT it_zib_bsid BY bukrs belnr gjahr.   "<<<------"160696 - NMS------>>>
*** PBI - 55017 - Inicio
*                  MOVE : it_zib-bukrs   TO it_zib_bsik-bukrs,
*                         it_0026-docnum TO it_zib_bsik-belnr,
*                         it_zib-gjahr   TO it_zib_bsik-gjahr.
*
*                  APPEND it_zib_bsik.
*          ENDIF.
**CLEAR: it_zib_bsik.
* ENDLOOP.
*** PBI - 55017 - Fim

*** PBI - 55017 - Inicio
*       IF sy-subrc IS INITIAL.
*                SELECT *
*                  FROM bsik
*                  INTO TABLE it_bsik
*                  FOR ALL ENTRIES IN it_zib_bsik
*                  WHERE bukrs EQ it_zib_bsik-bukrs
*                    AND belnr EQ it_zib_bsik-belnr
*                    AND gjahr EQ it_zib_bsik-gjahr.
*** PBI - 55017 - Fim

*** PBI - 55017 - Inicio
**<<<------"160696 - NMS - INI------>>>
*        IF sy-subrc IS INITIAL.
        IF NOT it_zib_bsid[] IS INITIAL.
**<<<------"160696 - NMS - FIM------>>>
          SELECT *
           FROM bsid
           INTO TABLE @it_bsid
           FOR ALL ENTRIES IN @it_zib_bsid
           WHERE bukrs EQ @it_zib_bsid-bukrs
             AND belnr EQ @it_zib_bsid-belnr
             AND gjahr EQ @it_zib_bsid-gjahr.

          IF sy-subrc IS INITIAL.
            SELECT *
              FROM bsad
              INTO TABLE @it_bsad
              FOR ALL ENTRIES IN @it_bsid
              WHERE bukrs EQ @it_bsid-bukrs
                AND belnr EQ @it_bsid-belnr
                AND gjahr EQ @it_bsid-gjahr.
          ENDIF.
          SORT it_zib_bsid BY belnr.   "<<<------"160696 - NMS------>>>
******IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024 - INICIO Ponto 2
          IF  it_bsad[] IS INITIAL.
**<<<------"165866 - NMS - INI------>>>
**            r_belnr =  VALUE #( FOR ls IN it_0026 ( sign = 'I' option = 'EQ' low = ls-docnum ) ).
**  r_belnr_aux =  VALUE #( FOR ls3 IN it_z0159 ( sign = 'I' option = 'EQ' low = ls3-adiant ) ).
**  APPEND LINES OF r_belnr_aux TO r_belnr.
**  r_belnr_aux =  VALUE #( FOR ls4 IN it_zibchv ( sign = 'I' option = 'EQ' low = ls4-belnr ) ).
**  APPEND LINES OF r_belnr_aux TO r_belnr.
*
*            r_bukrs =  VALUE #( FOR ls1 IN  it_vbak   ( sign = 'I' option = 'EQ' low = ls1-vkorg ) ).
*            r_kunnr =  VALUE #( FOR ls2 IN  it_vbak   ( sign = 'I' option = 'EQ' low = ls2-kunnr ) ).
*
*            IF NOT r_belnr[] IS INITIAL.
*
*              SELECT bukrs kunnr belnr augbl budat augdt dmbe2 dmbtr
*                 FROM bsad
*                 INTO CORRESPONDING FIELDS OF TABLE it_bsad
*               WHERE bukrs IN r_bukrs
*                 AND belnr IN r_belnr
*                 AND kunnr IN r_kunnr
*                 AND bschl IN ( '09', '01' ).
*
            DATA: BEGIN OF tl_fld_slc OCCURS 0,
                    bukrs TYPE bukrs,   "VKORG
                    belnr TYPE belnr_d, "DOCNUM
                    kunnr TYPE kunnr,   "KUNNR
                  END   OF tl_fld_slc.

            SELECT a~vkorg b~docnum a~kunnr
              FROM vbak AS a
              INNER JOIN zfit0026 AS b
               ON a~vbeln EQ b~vbeln
              INTO TABLE tl_fld_slc
              FOR ALL ENTRIES IN it_vbak
            WHERE a~vbeln EQ it_vbak-vbeln.

            IF sy-subrc IS INITIAL.
              SELECT bukrs kunnr belnr augbl budat augdt dmbe2 dmbtr
                FROM bsad
                INTO CORRESPONDING FIELDS OF TABLE it_bsad
               FOR ALL ENTRIES IN tl_fld_slc
              WHERE bukrs EQ tl_fld_slc-bukrs
                AND belnr EQ tl_fld_slc-belnr
                AND kunnr EQ tl_fld_slc-kunnr
                AND bschl IN ( '09', '01' ).

            ENDIF.
*            ENDIF.
**<<<------"165866 - NMS - FIM------>>>
          ENDIF.

******IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024 - FIM ponto 2

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
      WHERE spras EQ 'P'
       AND  zterm EQ it_vbkd_aux-zterm.


    SELECT   spras   zterm   vtext
      FROM tvzbt
      INTO TABLE it_tvzbt
      FOR ALL ENTRIES IN it_vbkd_aux
      WHERE spras EQ 'P'
       AND  zterm EQ it_vbkd_aux-zterm.
**<<<------"160696 - NMS - INI------>>>
    SORT: it_vbkd_aux BY vbeln,
          it_t052u    BY zterm,
          it_tvzbt    BY zterm.
**<<<------"160696 - NMS - FIM------>>>
  ENDIF.

  SELECT knumv kposn kschl waers kbetr kmein kwert
    FROM prcd_elements
    INTO TABLE it_konv
    FOR ALL ENTRIES IN it_vbak
  WHERE knumv EQ it_vbak-knumv
    AND kschl EQ 'PR00'
    AND waers IN  p_waerks.

  SELECT knumv kposn kschl waers kbetr kmein kwert
    FROM prcd_elements
    INTO TABLE it_konv_imp
    FOR ALL ENTRIES IN it_vbak
    WHERE knumv EQ it_vbak-knumv
      AND kschl EQ 'ICMI'
      AND waers IN  p_waerks.
**<<<------"160696 - NMS - INI------>>> => Não está mais sendo usado.
*  SELECT knumv kposn kschl waers kbetr kmein kwert
*    FROM prcd_elements
*    INTO TABLE it_konv_rb
*    FOR ALL ENTRIES IN it_vbak
*    WHERE knumv EQ it_vbak-knumv
*      AND kschl EQ 'RB00'
*      AND waers IN  p_waerks.
**<<<------"160696 - NMS - FIM------>>>
*>>>Begin-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

*** Stefanini - IR234653 - 23/04/2025 - GGARAUJO1 - Início de alteração
  IF it_vbak_aux[] IS NOT INITIAL.
*** Stefanini - IR234653 - 23/04/2025 - GGARAUJO1 - Fim de alteração

    SELECT knumv kposn kschl waers kbetr kmein kwert
          FROM prcd_elements
          INTO TABLE it_konv_aux
          FOR ALL ENTRIES IN it_vbak_aux
          WHERE knumv EQ it_vbak_aux-knumv
            AND kschl EQ 'ICMI'
            AND waers IN  p_waerks.

    SELECT vbeln vbap~matnr arktx werks vbap~ntgew vbap~gewei posnr kwmeng vrkme j_1bcfop j_1btxsdc lgort
                 vbap~netwr vbap~mwsbp  vbap~netpr vbap~kmein   "*-CS2021000218-14.09.2022-#90705-JT-inicio
      FROM vbap
      INNER JOIN mara
      ON mara~matnr  = vbap~matnr
      INTO CORRESPONDING FIELDS OF TABLE it_vbap_aux
      FOR ALL ENTRIES IN it_vbak_aux
      WHERE vbap~vbeln EQ it_vbak_aux-vbeln
      AND   vbap~matnr IN p_mater
      AND   mara~matkl IN p_grupo
      AND   vbap~werks IN p_cent.

*** Stefanini - IR234653 - 23/04/2025 - GGARAUJO1 - Início de alteração
  ENDIF.
*** Stefanini - IR234653 - 23/04/2025 - GGARAUJO1 - Fim de alteração
*<<<End-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

  SELECT vbeln vbap~matnr arktx werks vbap~ntgew vbap~gewei posnr kwmeng vrkme j_1bcfop j_1btxsdc lgort
               vbap~netwr vbap~mwsbp  vbap~netpr vbap~kmein   "*-CS2021000218-14.09.2022-#90705-JT-inicio
    FROM vbap
    INNER JOIN mara
    ON mara~matnr  = vbap~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_vbap
    FOR ALL ENTRIES IN it_vbak
    WHERE vbap~vbeln EQ it_vbak-vbeln
    AND   vbap~matnr IN p_mater
    AND   mara~matkl IN p_grupo
    AND   vbap~werks IN p_cent.

  CHECK sy-subrc IS INITIAL.

  "DEVK9A1VQ7  - Melhorias - 137325 - RSA

  CLEAR: wa_tot_zfiwrt0009, it_tot_zfiwrt0009[].
**<<<------"160696 - NMS - INI------>>>
  IF NOT it_zfiwrt0009[] IS INITIAL.
    DATA tl_str TYPE TABLE OF char26.

    DATA rl_seq_lcto TYPE RANGE OF zfiwed006.

    tl_str = it_zfiwrt0009.
    REPLACE ALL OCCURRENCES OF REGEX '\<................' IN TABLE tl_str WITH 'IEQ' IN CHARACTER MODE RESPECTING CASE.
    rl_seq_lcto = tl_str.
    SORT rl_seq_lcto BY low.
    DELETE ADJACENT DUPLICATES FROM rl_seq_lcto COMPARING low.

    SELECT seq_lcto, SUM( dmbtr ) AS dmbtr
     FROM zfiwrt0011
     INTO TABLE @DATA(tl_dmbtr)
    WHERE seq_lcto IN ( SELECT seq_lcto FROM zfiwrt0009 WHERE seq_lcto IN @rl_seq_lcto )
      AND bschl    EQ '40'
      AND estorno  NE @abap_true
    GROUP BY seq_lcto
    ORDER BY seq_lcto.

  ENDIF.
**<<<------"160696 - NMS - FIM------>>>
  LOOP AT it_zfiwrt0009 INTO wa_zfiwrt0009.

    wa_tot_zfiwrt0009-vbeln       = wa_zfiwrt0009-vbeln.
    wa_tot_zfiwrt0009-posnr       = wa_zfiwrt0009-posnr.
    wa_tot_zfiwrt0009-total_menge = wa_tot_zfiwrt0009-total_menge + wa_zfiwrt0009-menge.
*    wa_tot_zfiwrt0009-total_netwr = wa_tot_zfiwrt0009-total_netwr + wa_zfiwrt0009-netwr.
**<<<------"160696 - NMS - INI------>>>
*    SELECT SUM( dmbtr )
*           FROM zfiwrt0011
*           INTO @DATA(vl_dmbtr)
*           WHERE seq_lcto EQ @wa_zfiwrt0009-seq_lcto
*           AND   bschl    EQ '40'
*           AND   estorno  NE 'X'.
    READ TABLE tl_dmbtr INTO DATA(el_dmbtr) WITH KEY seq_lcto = wa_zfiwrt0009-seq_lcto BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      DATA(vl_dmbtr) = el_dmbtr-dmbtr.

    ELSE.
      CLEAR vl_dmbtr.

    ENDIF.
**<<<------"160696 - NMS - FIM------>>>
    wa_tot_zfiwrt0009-total_dmbtr = wa_tot_zfiwrt0009-total_dmbtr + wa_zfiwrt0009-netwr.

    wa_tot_zfiwrt0009-total_netwr = wa_tot_zfiwrt0009-total_netwr + ( wa_zfiwrt0009-netwr - vl_dmbtr ).

    AT END OF vbeln.
      APPEND wa_tot_zfiwrt0009 TO it_tot_zfiwrt0009.
      CLEAR wa_tot_zfiwrt0009.
    ENDAT.

  ENDLOOP.

  SORT it_tot_zfiwrt0009 BY vbeln posnr.
  "DEVK9A1VQ7  - Melhorias - 137325 - RSA


  SELECT matnr wrkst matkl "*-CS2021000218-14.09.2022-#90705-JT-inicio
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

  SELECT * INTO TABLE it_vbep2
    FROM vbep
    FOR ALL ENTRIES IN it_vbap
    WHERE vbeln EQ it_vbap-vbeln
      AND posnr EQ it_vbap-posnr.

*>>>Begin-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
  SELECT *
    INTO TABLE @it_vbep_aux
    FROM vbep
    FOR ALL ENTRIES IN @it_vbap_aux
    WHERE vbeln = @it_vbap_aux-vbeln
      AND posnr = @it_vbap_aux-posnr.
*<<<End-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

  SELECT vbeln valdt
    FROM vbkd
    INTO TABLE it_vbkd
    FOR ALL ENTRIES IN it_vbak
  WHERE vbeln EQ it_vbak-vbeln.

  SELECT name1 kunnr stcd1 stcd2
         stcd3 ort01 regio   "*-CS2021000218-14.09.2022-#90705-JT-inicio
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_vbak
    WHERE kunnr EQ it_vbak-kunnr .

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

*  SELECT VBELV VBELN ERDAT RFMNG RFWRT VBTYP_N VBTYP_V POSNN
  SELECT vbelv vbeln erdat rfmng rfwrt vbtyp_n vbtyp_v posnn posnv
   FROM vbfa
   INTO TABLE it_vbfa_aux2
   FOR ALL ENTRIES IN it_vbap
   WHERE vbelv EQ it_vbap-vbeln
    AND  posnv EQ it_vbap-posnr
*     AND  POSNN EQ IT_VBAP-POSNR
    AND  vbtyp_n IN ('M','O','N','R','S')
    AND  vbtyp_v IN ('C','M','T').
*    AND ERDAT   IN P_FATUV.

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

  IF it_vbfa[] IS NOT INITIAL.
    SELECT vbeln fkart sfakn netwr mwsbk "*-CS2021000218-14.09.2022-#90705-JT-inicio
      FROM vbrk
      INTO TABLE it_vbrk
      FOR ALL ENTRIES IN it_vbfa
      WHERE vbeln = it_vbfa-vbeln.

    IF sy-subrc IS INITIAL.
      SELECT vbeln posnr aubel aupos netwr mwsbp
        FROM vbrp
        INTO TABLE it_vbrp
        FOR ALL ENTRIES IN it_vbrk
        WHERE vbeln = it_vbrk-vbeln.
    ENDIF.

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
          SORT it_vbrk BY vbeln fkart. "<<<------"160696 - NMS------>>>
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
**<<<------"160696 - NMS - INI------>>>
*    LOOP AT it_vbfa INTO wa_vbfa.
*      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
*      APPEND wa_vbfa_aux TO it_vbfa_aux.
*
*    ENDLOOP.
*
*    SORT it_vbfa_aux BY vg_refkey.
    DATA tl_str2 TYPE TABLE OF char20.

    tl_str2 = it_vbfa.
    REPLACE ALL OCCURRENCES OF REGEX '\<..........' IN TABLE tl_str2 WITH '' IN CHARACTER MODE RESPECTING CASE.
    it_vbfa_aux = tl_str2.
    SORT it_vbfa_aux BY vg_refkey.
    DELETE ADJACENT DUPLICATES FROM it_vbfa_aux COMPARING vg_refkey.
**<<<------"160696 - NMS - FIM------>>>
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
**<<<------"160696 - NMS - INI------>>>
*    SORT: it_vbfa      BY vbeln posnn,
    SORT: it_vbfa      BY vbelv posnv,
**<<<------"160696 - NMS - FIM------>>>
          it_j_1bnflin BY refkey itmnum,
          it_vbrk      BY vbeln,
          it_vbrp      BY vbeln posnr.

    LOOP AT it_vbap INTO wa_vbap.

      CLEAR: somar, diminuir, somar_fat, diminuir_fat, wa_vbfa_tot, somar_fat_liq, diminuir_fat_liq.

*      LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV EQ WA_VBAP-VBELN AND POSNN EQ WA_VBAP-POSNR.
**<<<------"160696 - NMS - INI------>>>
*      LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnv EQ wa_vbap-posnr.
      READ TABLE it_vbfa TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbap-vbeln
                                                         posnv = wa_vbap-posnr
                                                BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        DATA(vl_tabix) = sy-tabix.

      ELSE.
        CONTINUE.

      ENDIF.

      LOOP AT it_vbfa INTO wa_vbfa FROM vl_tabix.
        IF wa_vbfa-vbelv NE wa_vbap-vbeln OR
           wa_vbfa-posnv NE wa_vbap-posnr.
          EXIT.

        ENDIF.
**<<<------"160696 - NMS - FIM------>>>
        CLEAR wa_vbrk.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.
*Inicio de alteração - FMARTINS - CS0965176 - Ajuste zsdt0051 ov - 24/03/2022
*        IF wa_vbrk-fkart = 'ZTRI'.
        IF wa_vbrk-fkart = 'ZTRI' OR sy-subrc IS NOT INITIAL.
*Fim de alteração - FMARTINS - CS0965176 - Ajuste zsdt0051 ov - 24/03/2022
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
          READ TABLE it_vbrp INTO wa_vbrp WITH KEY vbeln = vg_refkey posnr = wa_vbfa-posnn BINARY SEARCH.
          "Somar/Diminuir
          IF wa_vbfa-vbtyp_n = 'M'.
            somar     = somar     + wa_j_1bnflin-menge.
            somar_fat = somar_fat + wa_vbrp-netwr + wa_vbrp-mwsbp.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
            somar_fat_liq = somar_fat_liq + wa_vbrp-netwr.
          ELSEIF ( wa_vbfa-vbtyp_n = 'N' ) OR ( wa_vbfa-vbtyp_n = 'O' )
            OR   ( wa_vbfa-vbtyp_n = 'R'   AND  wa_vbfa-vbtyp_v = 'T' ) .
            diminuir     = diminuir     + wa_j_1bnflin-menge.
            diminuir_fat = diminuir_fat + wa_vbrp-netwr + wa_vbrp-mwsbp.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
            diminuir_fat_liq = diminuir_fat_liq + wa_vbrp-netwr.
          ENDIF.

          wa_vbfa_tot-totalmenge     = somar - diminuir.
*          WA_VBFA_TOT-POSNN         = WA_VBFA-POSNN.
          wa_vbfa_tot-posnn          = wa_vbfa-posnv.
          wa_vbfa_tot-vbelnrfmg      = wa_vbfa-vbelv.
          wa_vbfa_tot-totaldim       = diminuir.
          wa_vbfa_tot-vlr_totbrt_fat = somar_fat - diminuir_fat. "*-CS2021000218-14.09.2022-#90705-JT-inicio
          wa_vbfa_tot-vlr_totliq_fat = somar_fat_liq - diminuir_fat_liq. "*-CS2021000218-14.09.2022-#90705-JT-inicio
        ENDIF.
      ENDLOOP.

*      IF wa_vbfa_tot-totalmenge GE 0.       ">>RIM-SKM-IR130760-27.04.23
      IF NOT wa_vbfa_tot-posnn IS INITIAL.  ">>RIM-SKM-IR130760-27.04.23
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
    SELECT vbeln fkart sfakn netwr mwsbk "*-CS2021000218-14.09.2022-#90705-JT-inicio
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
**<<<------"160696 - NMS - INI------>>>
*    LOOP AT it_vbfa_aux2 INTO wa_vbfa.
*      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
*      APPEND wa_vbfa_aux TO it_vbfa_aux.
*    ENDLOOP.
*
*    SORT it_vbfa_aux BY vg_refkey.
    tl_str2 = it_vbfa_aux2[].
    REPLACE ALL OCCURRENCES OF REGEX '\<..........' IN TABLE tl_str2 WITH '' IN CHARACTER MODE RESPECTING CASE.
    it_vbfa_aux = tl_str2.
    SORT it_vbfa_aux BY vg_refkey.
    DELETE ADJACENT DUPLICATES FROM it_vbfa_aux COMPARING vg_refkey.
**<<<------"160696 - NMS - FIM------>>>
    SELECT refkey docnum menge meins netwr menge itmnum
      FROM j_1bnflin
      APPENDING TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_vbfa_aux
      WHERE refkey  EQ it_vbfa_aux-vg_refkey.
**<<<------"160696 - NMS - INI------>>>
*    SORT: it_vbfa_aux2 BY vbeln posnn,
    SORT: it_vbfa_aux2 BY vbelv posnv,
**<<<------"160696 - NMS - FIM------>>>
          it_j_1bnflin BY refkey itmnum,
          it_vbrk      BY vbeln.

    LOOP AT it_vbap INTO wa_vbap.

      CLEAR: somar, diminuir, wa_vbfa_tot.

*      LOOP AT IT_VBFA_AUX2 INTO WA_VBFA WHERE VBELV EQ WA_VBAP-VBELN AND POSNN EQ WA_VBAP-POSNR.
**<<<------"160696 - NMS - INI------>>>
*      LOOP AT it_vbfa_aux2 INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnv EQ wa_vbap-posnr.
      READ TABLE it_vbfa_aux2 TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbap-vbeln
                                                              posnv = wa_vbap-posnr
                                                     BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        vl_tabix = sy-tabix.

      ELSE.
        CONTINUE.

      ENDIF.

      LOOP AT it_vbfa_aux2 INTO wa_vbfa FROM vl_tabix.
        IF wa_vbfa-vbelv NE wa_vbap-vbeln OR
           wa_vbfa-posnv NE wa_vbap-posnr.
          EXIT.

        ENDIF.
**<<<------"160696 - NMS - FIM------>>>
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
*         comentado esta parte pois na totalização de tudo o que foi faturado anteriormente ao período de
*         Fatutamento informado no Filtro, o sistema não pode desconsiderar da soma as faturas de devolução/Recusa. CS2019000736
*          ELSEIF ( WA_VBFA-VBTYP_N = 'N' ) OR ( WA_VBFA-VBTYP_N = 'O' )
*            OR   ( WA_VBFA-VBTYP_N = 'R'   AND  WA_VBFA-VBTYP_V = 'T' ) .
*            DIMINUIR = DIMINUIR + WA_J_1BNFLIN-MENGE.
          ENDIF.

          wa_vbfa_tot-totalmenge = somar - diminuir.
*          WA_VBFA_TOT-POSNN = WA_VBFA-POSNN.
          wa_vbfa_tot-posnn = wa_vbfa-posnv.
          wa_vbfa_tot-vbelnrfmg  = wa_vbfa-vbelv.

        ENDIF.

      ENDLOOP.

*      IF wa_vbfa_tot-totalmenge GE 0.     ">>RIM-SKM-IR130760-27.04.23
      IF NOT wa_vbfa_tot-posnn IS INITIAL. "<<RIM-SKM-IR130760-27.04.23
        APPEND wa_vbfa_tot TO it_vbfa_tot2.

        CLEAR: wa_j_1bnflin,
               wa_vbfa,
               wa_vbap.
      ENDIF.

    ENDLOOP.

  ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 14.08.2013 <<< END

*** pbi - 55017 - inicio

  SELECT *
  APPENDING TABLE it_zsdt0090_safra
  FROM zsdt0090
   FOR ALL ENTRIES IN it_vbap
 WHERE vbeln EQ it_vbap-vbeln
    AND posnn EQ it_vbap-posnr
    AND estorno  = abap_false. ""*-IR190416-10.09.2025-JT-inicio

  IF sy-subrc IS INITIAL.
* Inclusão - RIM - SKM - IR122610 - 19.01.23 - Início
    SELECT *
    APPENDING TABLE it_zsdt0090_safra
    FROM zsdt0090
     FOR ALL ENTRIES IN it_zsdt0090_safra
      WHERE doc_simulacao EQ it_zsdt0090_safra-doc_simulacao
        AND estorno        = abap_false. ""*-IR190416-10.09.2025-JT-inicio

  ENDIF.
  IF NOT it_zsdt0090_safra[] IS  INITIAL.
* Inclusão - RIM - SKM - IR122610 - 19.01.23 - Fim
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
**<<<------"165866 - NMS - INI------>>>
*  WHERE cpf_cnpj IN rg_cpf_cnpj.
    FOR ALL ENTRIES IN rg_cpf_cnpj
  WHERE cpf_cnpj EQ rg_cpf_cnpj-low.
**<<<------"165866 - NMS - FIM------>>>
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
**<<<------"160696 - NMS - INI------>>>
    IF sy-subrc IS INITIAL.
      SORT it_zsdt0272 BY estagio.

    ENDIF.
**<<<------"160696 - NMS - FIM------>>>
  ENDIF.

                                                            "BUG 63604

  DELETE it_zsdt0272 WHERE estagio EQ 'Cancelado'.
*** PBI - 55017 - Fim
**<<<------"160696 - NMS - INI------>>>
  SORT it_zsdt0272 BY nr_proposta tp_proposta.
**<<<------"160696 - NMS - FIM------>>>
  LOOP AT it_zsdt0273 .
**<<<------"160696 - NMS - INI------>>>
*    READ TABLE it_zsdt0272 WITH KEY nr_proposta = it_zsdt0273-nr_proposta.
    READ TABLE it_zsdt0272 WITH KEY nr_proposta = it_zsdt0273-nr_proposta BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
    IF sy-subrc NE 0.
      DELETE it_zsdt0273 WHERE nr_proposta EQ it_zsdt0273-nr_proposta.
    ENDIF.
  ENDLOOP.




ENDFORM.                    " FORM_SELECIONA_SD

*&---------------------------------------------------------------------*
*&      Form  FORM_SAIDA_SD
*&---------------------------------------------------------------------*
FORM form_saida_sd .
  REFRESH it_saida_sd.

  DATA: vl_object TYPE ausp-objek.

  DATA(cll_mm_util) = NEW zcl_mm_util( ).

  CONSTANTS: cl_cultivar  TYPE atnam      VALUE 'NOVA_CULTIVAR',
             cl_class     TYPE klasse_d   VALUE 'SEMENTES_GERAL',
             cl_classtype TYPE klassenart VALUE '001'.

  DATA: x_saldo       TYPE vbap-zmeng,
        ntgewaux      TYPE bstmg,
        sac           TYPE vbap-vrkme,
        wl_xconversor TYPE konv-kwert,
*        WL_XVLR1      TYPE KONV-KWERT,
        aux(100)      TYPE c.

  DATA: lc_safra          TYPE ajahr,
        lc_cultura        TYPE acc_txtlg,
        lc_doc_simulacao  TYPE zsded003,
        lc_safra_apl      TYPE zsdt0041-safra_apl,
        lc_cultura_apl    TYPE zsdt0041-cultura_apl,
        lc_dt_simul       TYPE zsdt0040-erdat,
        vl_vbeln_exec     TYPE vbap-vbeln,
        vl_cont_exec      TYPE i,
        vl_achou_frete    TYPE char1,
        vl_vbeln_frete    TYPE vbap-vbeln,
        vl_posnr_frete    TYPE vbap-posnr,
        vl_matnr_frete    TYPE vbap-matnr,
        vl_achou_safra    TYPE char1,
        vl_achou_safra_ap TYPE char1,       "*- << IR12261-19.01.23-RIM-SKM
        vl_cont_safra     TYPE i,       "*- << IR12261-19.01.23-RIM-SKM
        wa_zsdt0090_aux   TYPE zsdt0090, "*- << IR12261-19.01.23-RIM-SKM
        vl_cpf            TYPE kna1-stcd1,
        vl_vbeln_safra    TYPE vbap-vbeln,
        vl_posnr_safra    TYPE vbap-posnr,
        wa_zsdt0041_safra TYPE zsdt0041,
        wa_zsdt0090_safra TYPE zsdt0090,
        wa_zsdt0041_frete TYPE zsdt0041,
        wa_zsdt0090_frete TYPE zsdt0090,
        wa_konv_imp       TYPE ty_konv,
        vl_vendedor       TYPE tvgrt-vkgrp,
        vl_meio_pag       TYPE dd07t-domvalue_l.
**<<<------"184428 - NMS - INI------>>>
  DATA: tl_values         TYPE TABLE OF rgsb4.
**<<<------"184428 - NMS - FIM------>>>
  SORT: it_vbap      BY vbeln,
        it_vbfa_tot  BY vbelnrfmg posnn,
        it_vbfa_tot2 BY vbelnrfmg posnn,
        it_kna1      BY kunnr,
        it_konv      BY knumv kposn,
        it_konv_imp  BY knumv kposn,
        it_vbkd      BY vbeln,
        it_mara      BY matnr,
        it_tvzbt     BY zterm,
        it_vbrk      BY vbeln,
        it_vbrp      BY vbeln posnr.

*  SORT it_bsad BY bukrs belnr gjahr augdt ASCENDING. "<<<------"160696 - NMS------>>>
  SORT it_0026 BY vbeln data_pgto docnum ASCENDING.
  SORT it_zfit0026_aux BY vbeln data_pgto docnum ASCENDING.
**<<<------"160696 - NMS - INI------>>>
  LOOP AT it_zsdt0274.
    TRANSLATE it_zsdt0274-cultura TO UPPER CASE.
    MODIFY it_zsdt0274.
    CLEAR  it_zsdt0274.

  ENDLOOP.

  SORT: it_zsdt0041_aux[] BY vbeln,
        it_vbep_aux[]     BY vbeln posnr.

  DELETE it_zsdt0041_aux[] WHERE vbeln IS INITIAL.
  DELETE it_vbep_aux[]     WHERE lifsp = '12'.

  DELETE ADJACENT DUPLICATES FROM it_zsdt0041_aux[] COMPARING vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vbep_aux[]     COMPARING vbeln posnr.
  DATA(it_zsdt0090_frete_2) = it_zsdt0090_frete.

  SORT: it_zsdt0041_aux[]   BY doc_simulacao,
        it_zsdt0275         BY doc_simulacao,
        it_zsdt0274         BY nr_proposta safra cultura,
        it_zsdt0090_frete   BY doc_simulacao vbelv categoria estorno,
        it_zsdt0090_frete_2 BY vbeln posnn,
        it_zsdt0041_frete   BY vbeln matnr,
        it_zsdt0041_safra   BY vbeln matnr,
        it_zsdt0090_safra   BY vbeln posnn,
        it_bsid             BY bukrs belnr gjahr,
        it_bsad             BY bukrs belnr kunnr gjahr augdt ASCENDING,
        it_vbep             BY vbeln posnr etenr,
        it_vbep2            BY vbeln posnr lifsp.
**<<<------"160696 - NMS - FIM------>>>
  LOOP AT it_vbak INTO wa_vbak.

    CLEAR : wa_vbfa,wa_vbap,wa_kna1, wa_saida_sd, wa_vbfa_tot, wl_xconversor, lc_safra, lc_cultura, lc_dt_simul,
            lc_doc_simulacao.

*    READ TABLE it_zsdt0090_frete INTO wa_zsdt0090_frete WITH KEY vbelv = wa_vbak-vbeln
*                                                                            categoria = 'B'
*                                                                            estorno = ' '.
*    IF sy-subrc IS INITIAL.
*      wa_saida_sd-dt_entrega = wa_zsdt0090_frete-dt_entrega.
*    ENDIF.

    READ TABLE it_zsdt0041 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      lc_doc_simulacao = it_zsdt0041-doc_simulacao.
      READ TABLE it_zsdt0040 WITH KEY doc_simulacao = it_zsdt0041-doc_simulacao BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lc_safra             = it_zsdt0040-safra.
        lc_dt_simul          = it_zsdt0040-erdat.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
        READ TABLE it_zsdt0038 WITH KEY cultura = it_zsdt0040-cultura BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lc_cultura   = it_zsdt0038-descricao.
        ENDIF.

        "Início - DEVK9A1PQE - 29.09.2023 SD - ZSDT0051 - Adicionar novos campos e filtro #123690 RSA

        wa_saida_sd-cod_vendedor = it_zsdt0040-vendedor.

        vl_vendedor = it_zsdt0040-vendedor.
        SELECT bezei UP TO 1 ROWS
               FROM tvgrt
               INTO wa_saida_sd-nome_vendedor
               WHERE vkgrp EQ vl_vendedor
          AND spras EQ sy-langu.
        ENDSELECT.



        IF it_zsdt0040-ecommerce EQ abap_true.
          wa_saida_sd-tipo_venda = 'E-Commerce'.
        ELSE.
          wa_saida_sd-tipo_venda = 'Convencional'.
        ENDIF.



        wa_saida_sd-ped_ecomm = it_zsdt0040-id_order_ecommerce.

        vl_meio_pag = it_zsdt0040-meio_pago.
        READ TABLE it_dd07t INTO DATA(wa_dd07t) WITH KEY domvalue_l = vl_meio_pag.
        IF sy-subrc EQ 0.
          CONCATENATE it_zsdt0040-meio_pago wa_dd07t-ddtext INTO wa_saida_sd-meio_pago SEPARATED BY space.
        ENDIF.

        "Fim - DEVK9A1PQE - 29.09.2023 SD - ZSDT0051 - Adicionar novos campos e filtro #123690 RSA

      ENDIF.
    ENDIF.
**<<<------"160696 - NMS - INI------>>>
*    LOOP AT it_zsdt0274.
*      TRANSLATE it_zsdt0274-cultura TO UPPER CASE.
*      MODIFY it_zsdt0274.
*      CLEAR  it_zsdt0274.
*    ENDLOOP.
**<<<------"160696 - NMS - FIM------>>>
    TRANSLATE lc_cultura TO UPPER CASE.
    "*******IR181516 informações N Proposta limite para todas as linhas - BG 20/05/2024 -  comentario para fazer essa busca para cada linha - INICIO
*** PBI - 55017 - Inicio
*    READ TABLE it_zsdt0275 WITH KEY doc_simulacao = lc_doc_simulacao.
*    IF sy-subrc = 0.
*      READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0275-nr_proposta
*                                       safra       = lc_safra
*                                       cultura     = lc_cultura.
*      IF sy-subrc = 0.
*        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
*        CLEAR: vl_cpf.
*
*        IF wa_kna1-stcd1 IS NOT INITIAL.
*          vl_cpf = wa_kna1-stcd1.
*        ELSE.
*          vl_cpf = wa_kna1-stcd2.
*        ENDIF.
*
**-CS2021000218-14.09.2022-#90705-JT-inicio
*        wa_saida_sd-stcd1 = vl_cpf.
*        wa_saida_sd-kunnr = wa_kna1-kunnr.
*        wa_saida_sd-stcd3 = wa_kna1-stcd3.
*        wa_saida_sd-ort01 = wa_kna1-ort01.
*        wa_saida_sd-regio = wa_kna1-regio.
**-CS2021000218-14.09.2022-#90705-JT-fim
*
*        CLEAR: wa_kna1.
*
*        READ TABLE it_zsdt0273 WITH KEY nr_proposta = it_zsdt0274-nr_proposta
*                                        cpf_cnpj    = vl_cpf .
*
*        IF sy-subrc = 0.
*          LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0273-nr_proposta.
*            IF wa_saida_sd-nrpropopr IS INITIAL.
*              wa_saida_sd-nrpropopr = it_zsdt0272-nr_proposta.
*            ELSE.
*              CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrpropopr
*                INTO wa_saida_sd-nrpropopr SEPARATED BY ','.
*            ENDIF.
*
*            IF wa_saida_sd-alcpropopr IS INITIAL.
*              wa_saida_sd-alcpropopr = it_zsdt0272-estagio.
*            ELSE.
*              CONCATENATE it_zsdt0272-estagio wa_saida_sd-alcpropopr
*              INTO wa_saida_sd-alcpropopr SEPARATED BY ','.
*            ENDIF.
*
*            IF wa_saida_sd-efetivada IS INITIAL.
*              wa_saida_sd-efetivada = it_zsdt0272-efetivada.
*            ELSE.
*              CONCATENATE it_zsdt0272-efetivada wa_saida_sd-efetivada
*              INTO wa_saida_sd-efetivada SEPARATED BY ','.
*            ENDIF.
*
*            IF wa_saida_sd-nrpropopr IS NOT INITIAL.
*              IF wa_saida_sd-nrproplmt  IS INITIAL.
*                wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta_ref.
*              ELSE.
*                CONCATENATE it_zsdt0272-nr_proposta_ref wa_saida_sd-nrproplmt
*                INTO wa_saida_sd-nrproplmt SEPARATED BY ','.
*              ENDIF.
**** BUG  - 59327 - CSB
*              IF wa_saida_sd-alcproplmt IS INITIAL.
*                wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
*              ELSE.
*                CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
*                INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
*              ENDIF.
**** BUG  - 59327 - CSB
*            ELSE.
*
*              READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
*              IF sy-subrc IS INITIAL.
*                READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
*                                 safra       = lc_safra
*                                 cultura     = lc_cultura .
*                IF sy-subrc = 0.
*                  LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
*                    AND tp_proposta = '1'.
*                    IF wa_saida_sd-nrproplmt IS INITIAL.
*                      wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta.
*                    ELSE.
*                      CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrproplmt INTO
*                      wa_saida_sd-nrproplmt SEPARATED BY ','.
*                    ENDIF.
*
*                    IF wa_saida_sd-alcproplmt IS INITIAL.
*                      wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
*                    ELSE.
*                      CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
*                      INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
*                    ENDIF.
*                  ENDLOOP.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.
*    ENDIF.

*    IF wa_saida_sd-nrpropopr IS INITIAL.
*
*      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
*      CLEAR: vl_cpf.
*
*      IF wa_kna1-stcd1 IS NOT INITIAL.
*        vl_cpf = wa_kna1-stcd1.
*      ELSE.
*        vl_cpf = wa_kna1-stcd2.
*      ENDIF.
*
**-CS2021000218-14.09.2022-#90705-JT-inicio
*      wa_saida_sd-stcd1 = vl_cpf.
*      wa_saida_sd-kunnr = wa_kna1-kunnr.
*      wa_saida_sd-stcd3 = wa_kna1-stcd3.
*      wa_saida_sd-ort01 = wa_kna1-ort01.
*      wa_saida_sd-regio = wa_kna1-regio.
**-CS2021000218-14.09.2022-#90705-JT-fim
*
*      CLEAR: wa_kna1.
*
*      READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
*      IF sy-subrc IS INITIAL.
*
*        READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
*                 safra       = lc_safra
*                 cultura     = lc_cultura .
*        IF sy-subrc = 0.
*          LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
*           AND tp_proposta = '1'.
*            IF wa_saida_sd-nrproplmt IS INITIAL.
*              wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta.
*            ELSE.
*              CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrproplmt INTO
*              wa_saida_sd-nrproplmt SEPARATED BY ','.
*            ENDIF.
*
*            IF wa_saida_sd-alcproplmt IS INITIAL.
*              wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
*            ELSE.
*              CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
*              INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*** PBI - 55017 - fim

    "*****IR181516 informações N Proposta limite para todas as linhas - BG 20/05/2024 -  comentario para fazer essa busca para cada linha - Fim
**<<<------"160696 - NMS - INI------>>>
*    LOOP AT it_vbap INTO wa_vbap WHERE vbeln EQ wa_vbak-vbeln.
    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln.
    IF sy-subrc IS INITIAL.
      DATA(vl_tabix) = sy-tabix.

    ELSE.
      CLEAR: wa_vbap, wa_vbfa, wa_kna1, wa_konv, wa_vbkd, wa_saida_sd.
      CONTINUE.

    ENDIF.

    LOOP AT it_vbap INTO wa_vbap FROM vl_tabix.
      IF wa_vbak-vbeln NE wa_vbap-vbeln.
        EXIT.

      ENDIF.
**<<<------"160696 - NMS - FIM------>>>
******IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024 - INICIO ponto 01

*** PBI - 55017 - Inicio
**<<<------"160696 - NMS - INI------>>>
*      READ TABLE it_zsdt0275 WITH KEY doc_simulacao = lc_doc_simulacao.
      READ TABLE it_zsdt0275 WITH KEY doc_simulacao = lc_doc_simulacao BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
      IF sy-subrc = 0.
        READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0275-nr_proposta
                                         safra       = lc_safra
                                         cultura     = lc_cultura
                               BINARY SEARCH.                                  "<<<------"160696 - NMS------>>>
        IF sy-subrc = 0.
          READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
          CLEAR: vl_cpf.

          IF wa_kna1-stcd1 IS NOT INITIAL.
            vl_cpf = wa_kna1-stcd1.
          ELSE.
            vl_cpf = wa_kna1-stcd2.
          ENDIF.

*-CS2021000218-14.09.2022-#90705-JT-inicio
          wa_saida_sd-stcd1 = vl_cpf.
          wa_saida_sd-kunnr = wa_kna1-kunnr.
          wa_saida_sd-stcd3 = wa_kna1-stcd3.
          wa_saida_sd-ort01 = wa_kna1-ort01.
          wa_saida_sd-regio = wa_kna1-regio.
*-CS2021000218-14.09.2022-#90705-JT-fim

          CLEAR: wa_kna1.

          READ TABLE it_zsdt0273 WITH KEY nr_proposta = it_zsdt0274-nr_proposta
                                          cpf_cnpj    = vl_cpf .

          IF sy-subrc = 0.
**<<<------"160696 - NMS - INI------>>>
*            LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0273-nr_proposta.
            READ TABLE it_zsdt0272 TRANSPORTING NO FIELDS WITH KEY nr_proposta = it_zsdt0273-nr_proposta BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              DATA(lv_tabix6) = sy-tabix.

              LOOP AT it_zsdt0272 FROM lv_tabix6.
                IF it_zsdt0272-nr_proposta NE it_zsdt0273-nr_proposta.
                  EXIT.

                ENDIF.
**<<<------"160696 - NMS - FIM------>>>
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
******IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024 - INICIO ponto 03
                  SELECT SINGLE *  FROM zsdt0272 INTO @DATA(wa_zsdt0272_limite) WHERE tp_proposta = '1' AND nr_proposta = @it_zsdt0272-nr_proposta_ref.
                  IF sy-subrc IS INITIAL.
*** BUG  - 59327 - CSB
                    IF wa_saida_sd-alcproplmt IS INITIAL.
                      wa_saida_sd-alcproplmt = wa_zsdt0272_limite-estagio.
                    ELSE.
                      CONCATENATE wa_zsdt0272_limite-estagio  wa_saida_sd-alcproplmt
                      INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
                    ENDIF.
*** BUG  - 59327 - CSB
                  ENDIF.
**** BUG  - 59327 - CSB
*              IF wa_saida_sd-alcproplmt IS INITIAL.
*                wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
*              ELSE.
*                CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
*                INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
*              ENDIF.
**** BUG  - 59327 - CSB
******IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024 - FIM ponto 03
                ELSE.

                  READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
                  IF sy-subrc IS INITIAL.
                    READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
                                     safra       = lc_safra
                                     cultura     = lc_cultura
                                     BINARY SEARCH.                                       "**<<<------"160696 - NMS------>>>
                    IF sy-subrc = 0.
**<<<------"160696 - NMS - INI------>>>
*                    LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
*                      AND tp_proposta = '1'.
                      READ TABLE it_zsdt0272 TRANSPORTING NO FIELDS WITH KEY nr_proposta = it_zsdt0274-nr_proposta
                                                                             tp_proposta = '1'
                                                                    BINARY SEARCH.

                      IF sy-subrc IS INITIAL.
                        DATA(lv_tabix7) = sy-tabix.

                        LOOP AT it_zsdt0272 FROM lv_tabix7.
                          IF it_zsdt0272-nr_proposta NE it_zsdt0274-nr_proposta OR
                             it_zsdt0272-tp_proposta NE '1'.
                            EXIT.

                          ENDIF.
**<<<------"160696 - NMS - FIM------>>>
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
**<<<------"160696 - NMS - INI------>>>
                      ENDIF.
**<<<------"160696 - NMS - FIM------>>>
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
            ENDIF.
**<<<------"160696 - NMS - FIM------>>>
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

*-CS2021000218-14.09.2022-#90705-JT-inicio
        wa_saida_sd-stcd1 = vl_cpf.
        wa_saida_sd-kunnr = wa_kna1-kunnr.
        wa_saida_sd-stcd3 = wa_kna1-stcd3.
        wa_saida_sd-ort01 = wa_kna1-ort01.
        wa_saida_sd-regio = wa_kna1-regio.
*-CS2021000218-14.09.2022-#90705-JT-fim

        CLEAR: wa_kna1.

        READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
        IF sy-subrc IS INITIAL.

          READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
                   safra       = lc_safra
                   cultura     = lc_cultura
                   BINARY SEARCH.                                                "<<<------"160696 - NMS------>>>
          IF sy-subrc = 0.
**<<<------"160696 - NMS - INI------>>>
*            LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
*             AND tp_proposta = '1'.
            READ TABLE it_zsdt0272 TRANSPORTING NO FIELDS WITH KEY nr_proposta = it_zsdt0274-nr_proposta
                                                                   tp_proposta = '1'
                                                          BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              lv_tabix6 = sy-tabix.

              LOOP AT it_zsdt0272 FROM lv_tabix6.
                IF it_zsdt0272-nr_proposta NE it_zsdt0274-nr_proposta OR
                   it_zsdt0272-tp_proposta NE '1'.
                  EXIT.

                ENDIF.
**<<<------"160696 - NMS - FIM------>>>
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
**<<<------"160696 - NMS - INI------>>>
            ENDIF.
**<<<------"160696 - NMS - FIM------>>>
          ENDIF.
        ENDIF.
      ENDIF.

******IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024 - FIM ponto 1


      wa_saida_sd-j_1bcfop  = wa_vbap-j_1bcfop.
      wa_saida_sd-auart     = wa_vbak-auart.
      wa_saida_sd-j_1btxsdc = wa_vbap-j_1btxsdc.

      wa_saida_sd-werks = wa_vbap-werks. "RJF
      wa_saida_sd-lgort = wa_vbap-lgort.


* CS2021000599 ZSDT0051 -  inserir campos na visão do relatório de compra e vendas. - US 60165 - BG Inicio
      READ TABLE it_zsdt0090_frete INTO wa_zsdt0090_frete WITH KEY  doc_simulacao = lc_doc_simulacao
                                                                    vbelv = wa_vbap-vbeln
                                                                    categoria = 'B'
                                                                    estorno = ' '
                                                          BINARY SEARCH.                              "<<<------"160696 - NMS------>>>
      IF sy-subrc IS INITIAL.
        wa_saida_sd-dt_entrega = wa_zsdt0090_frete-dt_entrega.
      ENDIF.
      IF  wa_saida_sd-dt_entrega IS INITIAL.
        READ TABLE it_zsdt0041 WITH KEY vbeln = wa_vbap-vbeln BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_zsdt0040 WITH KEY doc_simulacao = it_zsdt0041-doc_simulacao BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF wa_vbak-spart EQ '02'.
              wa_saida_sd-dt_entrega = it_zsdt0040-dt_entrega_fet.
            ELSEIF wa_vbak-spart EQ '03'.
              wa_saida_sd-dt_entrega = it_zsdt0040-dt_entrega_def.
            ELSEIF wa_vbak-spart EQ '04'.
              wa_saida_sd-dt_entrega = it_zsdt0040-dt_entrega_sem.
            ENDIF.

            "Início - DEVK9A1PQE - 29.09.2023 SD - ZSDT0051 - Adicionar novos campos e filtro #123690 RSA
            wa_saida_sd-cod_vendedor = it_zsdt0040-vendedor.

            vl_vendedor = it_zsdt0040-vendedor.
            SELECT bezei UP TO 1 ROWS
                   FROM tvgrt
                   INTO wa_saida_sd-nome_vendedor
                   WHERE vkgrp EQ vl_vendedor
              AND spras EQ sy-langu.
            ENDSELECT.

            IF it_zsdt0040-ecommerce EQ abap_true.
              wa_saida_sd-tipo_venda = 'E-Commerce'.
            ELSE.
              wa_saida_sd-tipo_venda = 'Convencional'.
            ENDIF.

            wa_saida_sd-ped_ecomm = it_zsdt0040-id_order_ecommerce.

            vl_meio_pag = it_zsdt0040-meio_pago.
            READ TABLE it_dd07t INTO DATA(wa_dd07t_itens) WITH KEY domvalue_l = vl_meio_pag.
            IF sy-subrc EQ 0.
              CONCATENATE it_zsdt0040-meio_pago wa_dd07t_itens-ddtext INTO wa_saida_sd-meio_pago SEPARATED BY space.
            ENDIF.
            "Fim - DEVK9A1PQE - 29.09.2023 SD - ZSDT0051 - Adicionar novos campos e filtro #123690 RSA

          ENDIF.
        ENDIF.
      ENDIF.
* CS2021000599 ZSDT0051 -  inserir campos na visão do relatório de compra e vendas. - US 60165 - BG  Fim

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
*-IR190416-10.09.2025-JT-inicio
              TRY.
                  wa_saida_sd-kbetr2 = wa_konv_imp-kwert / ( wa_vbap-kwmeng / 1000 ).
                CATCH cx_sy_arithmetic_overflow INTO DATA(ex_arithmetic_overflow_exc).
                  wa_saida_sd-kbetr2 = '999999.99'.
              ENDTRY.
*-IR190416-10.09.2025-JT-fim
            ENDIF.
          ELSEIF wa_konv_imp-kmein EQ 'KG' AND wa_konv-kmein EQ 'TO'.
            IF wa_vbap-kwmeng IS INITIAL.
              wa_saida_sd-kbetr2 = 0.
            ELSE.
*-IR190416-10.09.2025-JT-inicio
              TRY.
                  wa_saida_sd-kbetr2 = wa_konv_imp-kwert / ( wa_vbap-kwmeng / 1000 ).
                CATCH cx_sy_arithmetic_overflow INTO ex_arithmetic_overflow_exc.
                  wa_saida_sd-kbetr2 = '999999.99'.
              ENDTRY.
*-IR190416-10.09.2025-JT-fim
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
                                                                     matnr = vl_matnr_frete
                                                            BINARY SEARCH.                   "<<<------"160696 - NMS------>>>
        IF sy-subrc IS INITIAL.
          wa_saida_sd-vlr_frete = wa_zsdt0041_frete-vlr_frete.
          vl_achou_frete = abap_true.
        ELSE.
          CLEAR wa_zsdt0090_frete.
**<<<------"160696 - NMS - INI------>>>
*          READ TABLE it_zsdt0090_frete INTO wa_zsdt0090_frete WITH KEY vbeln = vl_vbeln_frete
*                                                                       posnn = vl_posnr_frete.
* Foi necessário fazer uma cópia da TI IT_ZSDT0090_FRETE_2 por conta que ele é lida em duas partes distintas
* com chaves diferente e nesta parte está dentro de um WHILE onde conven o BINARY SEARCH na leituta da TI.
          READ TABLE it_zsdt0090_frete_2 INTO wa_zsdt0090_frete WITH KEY vbeln = vl_vbeln_frete
                                                                         posnn = vl_posnr_frete
                                                                BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
          IF sy-subrc IS INITIAL.
*-IR190416-10.09.2025-JT-inicio
            IF wa_zsdt0090_frete-vbeln = wa_zsdt0090_frete-vbelv AND
               wa_zsdt0090_frete-posnn = wa_zsdt0090_frete-posnv.
              EXIT.
            ENDIF.
*-IR190416-10.09.2025-JT-fim
            vl_vbeln_frete = wa_zsdt0090_frete-vbelv.
            vl_posnr_frete = wa_zsdt0090_frete-posnv.
            vl_matnr_frete = wa_zsdt0090_frete-matnrv.
          ELSE.
            "Não tem ordem nem na 0090 nem na 0041  --> Mostra frete em Branco
            wa_saida_sd-vlr_frete = 0.   "**<<<------"184428 - NMS------>>>
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

        READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = vl_vbeln_safra
* RMI - CS1105375 - Adicionado o parametro de material na consulta pois se houvesse mais de um item com cultura diferente
* todos os outros itens teriam a mesma cultura. Não foi possível utilizar o item pois o item da ordem de venda não
* é o mesmo do item do simulador de venda
                                                                     matnr = wa_vbap-matnr
                                                            BINARY SEARCH.                   "<<<------"160696 - NMS------>>>
        IF sy-subrc IS INITIAL.
          lc_safra_apl     = wa_zsdt0041_safra-safra_apl.
          lc_cultura_apl   = wa_zsdt0041_safra-cultura_apl.
          vl_achou_safra = abap_true.
        ELSE.

          READ TABLE it_zsdt0090_safra INTO wa_zsdt0090_safra WITH KEY vbeln = vl_vbeln_safra
                                                                       posnn = vl_posnr_safra
                                                              BINARY SEARCH.                  "<<<------"160696 - NMS------>>>
          IF sy-subrc IS INITIAL.
*-IR190416-10.09.2025-JT-inicio
            IF wa_zsdt0090_safra-vbeln = wa_zsdt0090_safra-vbelv AND
               wa_zsdt0090_safra-posnn = wa_zsdt0090_safra-posnv.
              EXIT.
            ENDIF.
*-IR190416-10.09.2025-JT-fim

* Inclusão - RIM - SKM - IR122610 - 19.01.23 - Início
**<<<------"160696 - NMS - INI------>>>
*            READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = wa_zsdt0090_safra-vbelv.
            READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = wa_zsdt0090_safra-vbelv BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
            IF sy-subrc IS INITIAL.
              lc_safra_apl     = wa_zsdt0041_safra-safra_apl.
              lc_cultura_apl   = wa_zsdt0041_safra-cultura_apl.
              vl_achou_safra = abap_true.
            ELSE.
              CLEAR: vl_achou_safra_ap, vl_cont_safra.
              wa_zsdt0090_aux = wa_zsdt0090_safra.
              WHILE vl_achou_safra_ap IS INITIAL.
                vl_cont_safra = vl_cont_safra + 1.
                IF vl_cont_safra GE 5.
                  EXIT.
                ENDIF.
                READ TABLE it_zsdt0090_safra INTO wa_zsdt0090_aux WITH KEY vbeln = wa_zsdt0090_aux-vbelv
                                                                           posnn = wa_zsdt0090_aux-posnv
                                                                  BINARY SEARCH.                         "<<<------"160696 - NMS------>>>
                IF sy-subrc IS INITIAL.
*-IR190416-10.09.2025-JT-inicio
                  IF wa_zsdt0090_aux-vbeln = wa_zsdt0090_aux-vbelv AND
                     wa_zsdt0090_aux-posnn = wa_zsdt0090_aux-posnv.
                    EXIT.
                  ENDIF.
*-IR190416-10.09.2025-JT-fim

**<<<------"160696 - NMS - INI------>>>
*                  READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = wa_zsdt0090_aux-vbeln.
                  READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = wa_zsdt0090_aux-vbeln BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
                  IF sy-subrc IS INITIAL.
                    lc_safra_apl     = wa_zsdt0041_safra-safra_apl.
                    lc_cultura_apl   = wa_zsdt0041_safra-cultura_apl.
                    vl_achou_safra = abap_true.
                    vl_achou_safra_ap = abap_true.
                  ELSE.
**<<<------"160696 - NMS - INI------>>>
*                    READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = wa_zsdt0090_aux-vbelv.
                    READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = wa_zsdt0090_aux-vbelv BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
                    IF sy-subrc IS INITIAL.
                      lc_safra_apl     = wa_zsdt0041_safra-safra_apl.
                      lc_cultura_apl   = wa_zsdt0041_safra-cultura_apl.
                      vl_achou_safra = abap_true.
                      vl_achou_safra_ap = abap_true.
                    ENDIF.
                  ENDIF.
                ELSE.
                  vl_achou_safra = abap_true.
                  vl_achou_safra_ap = abap_true.
                ENDIF.
              ENDWHILE.
            ENDIF.
* Inclusão - RIM - SKM - IR122610 - 19.01.23 - Fim
            vl_vbeln_safra = wa_zsdt0090_safra-vbelv.
            vl_posnr_safra = wa_zsdt0090_safra-posnv.
          ELSE.
            vl_achou_safra = abap_true.
          ENDIF.
        ENDIF.
      ENDWHILE.

      wa_saida_sd-safra_apl   =  lc_safra_apl.
      wa_saida_sd-cultura_apl =  lc_cultura_apl.
      wa_saida_sd-dt_simul    =  lc_dt_simul.  "*-CS2021000218-14.09.2022-#90705-JT-inicio

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
      wa_saida_sd-dt_simul      = lc_dt_simul.  "*-CS2021000218-14.09.2022-#90705-JT-inicio

      LOOP AT it_0026 WHERE vbeln EQ wa_vbap-vbeln.
* PBI - 55017 - Inicio
*       READ TABLE it_zib_bsik WITH KEY belnr = it_0026-docnum.
*        READ TABLE it_bsik WITH KEY bukrs = it_zib_bsik-bukrs
*                                    belnr = it_zib_bsik-belnr
*                                    gjahr = it_zib_bsik-gjahr.
* PBI - 55017 - Fim
* PBI - 55017 - Inicio

        IF it_0026-docnum = '0000000000'.
**<<<------"160696 - NMS - INI------>>>
*          READ TABLE it_zib WITH  KEY obj_key = it_0026-obj_key.
*          READ TABLE it_zib_bsid WITH KEY belnr = it_zib-belnr.
          READ TABLE it_zib      WITH KEY obj_key = it_0026-obj_key BINARY SEARCH.
          READ TABLE it_zib_bsid WITH KEY belnr   = it_zib-belnr    BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
        ELSE.
**<<<------"160696 - NMS - INI------>>>
*          READ TABLE it_zib_bsid WITH KEY belnr = it_0026-docnum.
          READ TABLE it_zib_bsid WITH KEY belnr = it_0026-docnum BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
        ENDIF.

        READ TABLE it_bsid WITH KEY bukrs = it_zib_bsid-bukrs
                            belnr = it_zib_bsid-belnr
                            gjahr = it_zib_bsid-gjahr
                           BINARY SEARCH.                     "<<------"160696 - NMS------>>>
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
                                    kunnr = wa_saida_sd-kunnr "IR181516 informações N Proposta limite para todas as linhas -BG 20/05/2024
                           BINARY SEARCH.                     "<<------"160696 - NMS------>>>
        IF sy-subrc IS INITIAL.
          wa_saida_sd-augdt = it_bsad-augdt .
        ENDIF.

      ENDLOOP.

      IF sy-subrc IS NOT INITIAL.
        wa_saida_sd-status = 'A'.
      ENDIF.
**<<<------"160696 - NMS - INI------>>>
*      READ TABLE it_vbkd_aux WITH KEY vbeln = wa_vbap-vbeln.
      READ TABLE it_vbkd_aux WITH KEY vbeln = wa_vbap-vbeln BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
      IF sy-subrc IS INITIAL .
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 101103 - Ajuste_Relatório_ZSDT0027_ZSDT0051
* inserindo: VBKD-INCO1 / VBKD-INCO2
        wa_saida_sd-inco1 = it_vbkd_aux-inco1.
        wa_saida_sd-inco2 = it_vbkd_aux-inco2.
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END

        wa_saida_sd-kurrf = it_vbkd_aux-kurrf.
**<<<------"160696 - NMS - INI------>>>
*        READ TABLE it_t052u WITH KEY zterm = it_vbkd_aux-zterm.
        READ TABLE it_t052u WITH KEY zterm = it_vbkd_aux-zterm BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
        IF sy-subrc IS INITIAL.
          CONCATENATE it_vbkd_aux-zterm '-' it_t052u-text1 INTO wa_saida_sd-zterm SEPARATED BY space.
        ENDIF.

      ENDIF.
**<<<------"160696 - NMS - INI------>>>
*      READ TABLE it_tvzbt INTO wa_tvzbt WITH KEY zterm = it_vbkd_aux-zterm.
      READ TABLE it_tvzbt INTO wa_tvzbt WITH KEY zterm = it_vbkd_aux-zterm BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
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

      CLEAR wa_vbep. "<<<------"160696 - NMS------>>>
      READ TABLE it_vbep INTO wa_vbep WITH KEY vbeln = wa_vbap-vbeln
                                               posnr = wa_vbap-posnr
                                               etenr = 1
                                      BINARY SEARCH.                  "<<<------"160696 - NMS------>>>

      CLEAR wa_vbkd. "<<<------"160696 - NMS------>>>
      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      aux = ''.

      CLEAR: x_saldo.

      wa_saida_sd-lifsp = wa_vbep-lifsp.

      wa_saida_sd-name1 = wa_kna1-name1. " Cliente
      wa_saida_sd-vkbur = wa_vbak-vkbur. " Escr.Vendas

*-CS2021000218-14.09.2022-#90705-JT-inicio
      IF wa_kna1-stcd1 IS NOT INITIAL.
        vl_cpf = wa_kna1-stcd1.
      ELSE.
        vl_cpf = wa_kna1-stcd2.
      ENDIF.

      wa_saida_sd-stcd1 = vl_cpf.
      wa_saida_sd-kunnr = wa_kna1-kunnr.
      wa_saida_sd-stcd3 = wa_kna1-stcd3.
      wa_saida_sd-ort01 = wa_kna1-ort01.
      wa_saida_sd-regio = wa_kna1-regio.
*-CS2021000218-14.09.2022-#90705-JT-fim

*** PBI - 55017 - Inicio
**<<<------"160696 - NMS - INI------>>>
*      READ TABLE it_tvbur WITH KEY vkbur =  wa_vbak-vkbur.
      READ TABLE it_tvbur WITH KEY vkbur =  wa_vbak-vkbur BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
      IF sy-subrc = 0.
        READ TABLE it_tvkbt WITH KEY vkbur = it_tvbur-vkbur.
        wa_saida_sd-bezei = it_tvkbt-bezei.
      ENDIF.
**<<<------"160696 - NMS - INI------>>>
*      READ TABLE it_zsdt0271 WITH KEY filial =  wa_vbak-vkbur.
      READ TABLE it_zsdt0271 WITH KEY filial = wa_vbak-vkbur BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
      IF sy-subrc = 0.
**<<<------"160696 - NMS - INI------>>>
*        READ TABLE it_zsdt0270 WITH KEY cod_regional = it_zsdt0271-cod_regional.
        READ TABLE it_zsdt0270 WITH KEY cod_regional = it_zsdt0271-cod_regional BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
        wa_saida_sd-regional = it_zsdt0270-regional.
      ENDIF.

      IF wa_vbak-lifsk IS NOT INITIAL.
**<<<------"160696 - NMS - INI------>>>
*        READ TABLE it_tvls WITH KEY lifsp = wa_vbak-lifsk.
*        READ TABLE it_tvlst WITH KEY lifsp = it_tvls-lifsp.
        READ TABLE it_tvls  WITH KEY lifsp = wa_vbak-lifsk BINARY SEARCH.
        READ TABLE it_tvlst WITH KEY lifsp = it_tvls-lifsp BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
        wa_saida_sd-lifsp_t = it_tvlst-vtext.
      ENDIF.

      IF wa_vbak-faksk IS NOT INITIAL.
**<<<------"160696 - NMS - INI------>>>
*        READ TABLE it_tvfs  WITH  KEY faksp = wa_vbak-lifsk.
*        READ TABLE it_tvfst WITH  KEY faksp = it_tvfs-faksp.
        READ TABLE it_tvfs  WITH KEY faksp = wa_vbak-lifsk BINARY SEARCH.
        READ TABLE it_tvfst WITH KEY faksp = it_tvfs-faksp BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
        wa_saida_sd-lifsp_t = it_tvfst-vtext.
      ENDIF.


*** PBI - 55017 - Fim

      wa_saida_sd-vbeln = wa_vbak-vbeln. " Contrato
      wa_saida_sd-posnr = wa_vbap-posnr. " Item
      wa_saida_sd-erdat = wa_vbak-erdat. "Data criaçâo

      wa_saida_sd-waers = wa_konv-waers.

**********************************************************************
* CS2023000153 ZSDT0051 - Adicionar coluna com a taxa de cambio travada na ZSDT0087 - PSA
**********************************************************************

      IF wa_saida_sd-vbeln IS NOT INITIAL.
        SELECT SINGLE kurrf FROM zsdt0090
          WHERE vbelv = @wa_saida_sd-vbeln AND categoria = 'C' AND estorno <> 'X'
          INTO (@wa_saida_sd-txcontratada).
      ENDIF.
**********************************************************************
*PERFORM BUSCA_IMPOSTO USING WA_VBAK-VBELN WA_VBAP-POSNR.


      wa_saida_sd-valdt = wa_vbkd-valdt.

      wa_saida_sd-matnr = wa_vbap-matnr.
      wa_saida_sd-arktx = wa_vbap-arktx.

*-cs2025000249-us 192338 - Ajustes zsdt0051 - gabriel avila - inicio
      vl_object = wa_saida_sd-matnr.
      CALL METHOD cll_mm_util->get_caracteristica_geral
        EXPORTING
          i_class                = cl_class
          i_classtype            = cl_classtype
          i_object               = vl_object
          i_caracteristica       = cl_cultivar
        IMPORTING
          e_valor_caracteristica = DATA(vl_value).

      wa_saida_sd-cultivar = CONV #( vl_value ).
*-CS2025000249-US 192338 - Ajustes ZSDT0051 - GABRIEL AVILA - fim

      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.
      wa_saida_sd-wrkst = wa_mara-wrkst.
      wa_saida_sd-matkl = wa_mara-matkl.  "*-CS2021000218-14.09.2022-#90705-JT-inicio

      IF ( wa_vbap-vrkme EQ 'BAG' ).
        wa_saida_sd-qtd  = wa_vbap-kwmeng.
        wa_saida_sd-unid = 'SAC'.
      ELSEIF ( wa_vbap-vrkme EQ 'BIG' ).
        wa_saida_sd-qtd  = wa_vbap-kwmeng.
        wa_saida_sd-unid = 'BAG'.
      ELSE.
        wa_saida_sd-qtd  = wa_vbap-kwmeng.
        wa_saida_sd-unid = wa_vbap-vrkme.

      ENDIF.

      wa_saida_sd-qtd         = wa_saida_sd-qtd - wa_vbfa_tot-totaldim.
      wa_saida_sd-vlr_totbrt  = wa_vbap-netwr   + wa_vbap-mwsbp.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
*     wa_saida_sd-vlr_qtecont = wa_vbap-netwr.                    "*-CS2021000218-14.09.2022-#90705-JT-inicio


      IF wa_vbap-kwmeng IS INITIAL.                             "*- << IR118310-21.11.2022-RIM-SKM
        CLEAR: l_netpr_liq , l_netpr_brt, l_netpr_brt_aux.                       "*- << IR118310-21.11.2022-RIM-SKM
      ELSE.                                                     "*- << IR118310-21.11.2022-RIM-SKM
        l_netpr_liq = wa_vbap-netwr          / wa_vbap-kwmeng.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
        l_netpr_brt = wa_saida_sd-vlr_totbrt / wa_vbap-kwmeng.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
        l_netpr_brt_aux = l_netpr_brt.                          "**US 142625 - PQ E SMC
      ENDIF.                                                    "*- << IR118310-21.11.2022-RIM-SKM
*** Modificação - Eduardo Ruttkowski Tavares - 13.08.2013 >>> INI
* CH 100102 - Ajuste_Qte Faturada_ZSDT0027_ZSDT0051
      READ TABLE it_vbfa_tot2 WITH KEY vbelnrfmg = wa_vbap-vbeln
                                       posnn     = wa_vbap-posnr   BINARY SEARCH.

      SUBTRACT it_vbfa_tot2-totalmenge FROM wa_saida_sd-qtd.

      "DEVK9A1VQ7  - Melhorias - 137325 - RSA
      READ TABLE it_tot_zfiwrt0009 INTO wa_tot_zfiwrt0009 WITH KEY vbeln = wa_vbap-vbeln posnr = wa_vbap-posnr BINARY SEARCH.
      IF sy-subrc EQ 0.
**US 142625 - inicio - tratativa para situações onde o faturamento ocorreu pela MIC e a OV é em USD - PQ E SMC
        wa_saida_sd-qtefaturado    = wa_tot_zfiwrt0009-total_menge.
        IF wa_saida_sd-waers EQ 'USD'.
          wa_saida_sd-vlr_qtefat     = wa_tot_zfiwrt0009-total_menge * l_netpr_liq.
          wa_saida_sd-vlr_totbrt_fat = wa_tot_zfiwrt0009-total_menge * l_netpr_brt.
        ELSE.
          wa_saida_sd-vlr_qtefat     = wa_tot_zfiwrt0009-total_netwr.
          wa_saida_sd-vlr_totbrt_fat = wa_tot_zfiwrt0009-total_dmbtr.
        ENDIF.
**US 142625 - fim - tratativa para situações onde o faturamento ocorreu pela MIC e a OV é em USD - PQ E SMC
      ELSE.
        wa_saida_sd-qtefaturado =  wa_vbfa_tot-totalmenge.
        wa_saida_sd-vlr_qtefat = wa_vbfa_tot-vlr_totliq_fat.
        wa_saida_sd-vlr_totbrt_fat = wa_vbfa_tot-vlr_totbrt_fat.
      ENDIF.
      "DEVK9A1VQ7  - Melhorias - 137325 - RSA

      x_saldo =  wa_saida_sd-qtd - wa_saida_sd-qtefaturado.
      wa_saida_sd-saldo =  x_saldo.
      "wa_saida_sd-vlr_totbrt_fat = wa_vbfa_tot-vlr_totbrt_fat.  "*-CS2021000218-14.09.2022-#90705-JT-inicio
      "wa_saida_sd-vlr_qtefat = wa_vbfa_tot-vlr_totliq_fat.


*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 >>> INI
* CH 100102 - Ajuste_Qte Faturada_ZSDT0027_ZSDT0051
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr
                                      BINARY SEARCH.                 "<<<------"160696 - NMS------>>>
      TRY.
          IF wa_konv-kmein NE wa_vbap-vrkme.
            IF  wa_konv-kmein EQ 'TO'    "*-CS2021000218-14.09.2022-#90705-JT-inicio
            AND wa_vbap-vrkme EQ 'KG'.
              l_netpr_liq             = l_netpr_liq * 1000.
              l_netpr_brt             = l_netpr_brt * 1000.
*             wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd / 1000 ) * wa_saida_sd-kbetr . "*-CS2021000218-14.09.2022-#90705-JT-inicio
              wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd / 1000 ) * l_netpr_liq. "*-CS2021000218-14.09.2022-#90705-JT-inicio
**              wa_saida_sd-vlr_saldo   = ( wa_saida_sd-saldo / 1000 ) * wa_saida_sd-kbetr.
**              wa_saida_sd-vlr_qtefat  = ( wa_saida_sd-qtefaturado / 1000 ) * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_totbrt  = ( wa_saida_sd-qtd / 1000 ) * l_netpr_brt. "*-CS2021000218-14.09.2022-#90705-JT-inicio

            ELSEIF wa_konv-kmein EQ 'KG'   "*-CS2021000218-14.09.2022-#90705-JT-inicio
            AND    wa_vbap-vrkme EQ 'TO'.
              l_netpr_liq             = l_netpr_liq / 1000.
              l_netpr_brt             = l_netpr_brt / 1000.
*             wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd * 1000 ) * wa_saida_sd-kbetr . "*-CS2021000218-14.09.2022-#90705-JT-inicio
              wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd * 1000 ) * l_netpr_liq. "*-CS2021000218-14.09.2022-#90705-JT-inicio
**              wa_saida_sd-vlr_saldo   = ( wa_saida_sd-saldo * 1000 ) * wa_saida_sd-kbetr.
**              wa_saida_sd-vlr_qtefat  = ( wa_saida_sd-qtefaturado * 1000 ) * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_totbrt  = ( wa_saida_sd-qtd * 1000 ) * l_netpr_brt. "*-CS2021000218-14.09.2022-#90705-JT-inicio

            ELSE.
*             wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * wa_saida_sd-kbetr .            "*-CS2021000218-14.09.2022-#90705-JT-inicio
              wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * l_netpr_liq.            "*-CS2021000218-14.09.2022-#90705-JT-inicio
**              wa_saida_sd-vlr_saldo   = wa_saida_sd-saldo * wa_saida_sd-kbetr.
**              wa_saida_sd-vlr_qtefat  = wa_saida_sd-qtefaturado * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_totbrt  = wa_saida_sd-qtd * l_netpr_brt.                "**US 142625

            ENDIF.

          ELSE.
*           wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * wa_saida_sd-kbetr .              "*-CS2021000218-14.09.2022-#90705-JT-inicio
            wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * l_netpr_liq.              "*-CS2021000218-14.09.2022-#90705-JT-inicio
**            wa_saida_sd-vlr_saldo   = wa_saida_sd-saldo * wa_saida_sd-kbetr.
**            wa_saida_sd-vlr_qtefat  = wa_saida_sd-qtefaturado * wa_saida_sd-kbetr.
            wa_saida_sd-vlr_totbrt  = wa_saida_sd-qtd * l_netpr_brt.                "**US 142625

          ENDIF.

        CATCH cx_root.
** Tratada excessões ocorrida por valores errados lançados
      ENDTRY.

*-IR190416-10.09.2025-JT-inicio
      TRY.
          wa_saida_sd-kbetr = l_netpr_liq. "wa_konv-kbetr.
        CATCH cx_sy_conversion_overflow INTO DATA(ex_sy_conversion_overflow).
          wa_saida_sd-kbetr = '999999.99'.
      ENDTRY.
*-IR190416-10.09.2025-JT-fim

      wa_saida_sd-vlr_saldo = wa_saida_sd-vlr_totbrt - wa_saida_sd-vlr_totbrt_fat.




      "Aplicando o valor desconto da OV.
*      READ TABLE it_konv_rb INTO DATA(wa_konv_rb) WITH KEY knumv = wa_vbak-knumv
*                                               kposn = wa_vbap-posnr.
*
*      IF sy-subrc EQ 0.
*        IF wa_saida_sd-vlr_totbrt IS NOT INITIAL.
*          wa_saida_sd-vlr_totbrt = wa_saida_sd-vlr_totbrt - wa_konv_rb-kbetr.
*        ENDIF.
*
*        IF wa_saida_sd-vlr_totbrt_fat IS NOT INITIAL.
*          wa_saida_sd-vlr_totbrt_fat = wa_saida_sd-vlr_totbrt_fat - wa_konv_rb-kbetr.
*        ENDIF.
*
*        IF wa_saida_sd-vlr_qtecont IS NOT INITIAL.
*          wa_saida_sd-vlr_qtecont = wa_saida_sd-vlr_qtecont - wa_konv_rb-kbetr.
*        ENDIF.
*
*        IF wa_saida_sd-vlr_qtefat IS NOT INITIAL.
*          wa_saida_sd-vlr_qtefat = wa_saida_sd-vlr_qtefat - wa_konv_rb-kbetr.
*        ENDIF.
*
*        IF wa_saida_sd-vlr_saldo IS NOT INITIAL.
*          wa_saida_sd-vlr_saldo = wa_saida_sd-vlr_saldo - wa_konv_rb-kbetr.
*        ENDIF.
*      ENDIF.


      "Para não trazer ordens de transferência
      READ TABLE it_vbep2 WITH KEY vbeln = wa_vbak-vbeln
                                   posnr = wa_vbap-posnr
                                   lifsp = '12'
                          BINARY SEARCH.                 "<<<------"160696 - NMS------>>>
      IF sy-subrc IS INITIAL.
        wa_saida_sd-qtd = 0.
      ENDIF.

*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 <<< END

      IF wa_saida_sd-qtd GT 0.

*>>>Begin-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo

        DATA(lv_mont_rbdo_sum)  = CONV zfit0026-mont_rbdo( '' ).
        DATA(lv_konv_kwert_sum) = CONV konv-kwert( '' ).
        DATA(lv_data_pgto)      = CONV zfit0026-data_pgto( '' ).
        DATA(lv_auart_sum)      = CONV konv-kwert( '' ).

        CLEAR: lv_data_pgto,
               wa_saida_sd-augdt,
               lv_auart_sum.
**<<<------"160696 - NMS - INI------>>>
*        SORT: it_zsdt0041_aux[] BY vbeln,
*              it_vbep_aux[]     BY vbeln posnr.
*
*        DELETE it_zsdt0041_aux[] WHERE vbeln IS INITIAL.
*        DELETE it_vbep_aux[]     WHERE lifsp = '12'.
*
*        DELETE ADJACENT DUPLICATES FROM it_zsdt0041_aux[] COMPARING vbeln.
*        DELETE ADJACENT DUPLICATES FROM it_vbep_aux[]     COMPARING vbeln posnr. "Ajuste / retornou o campo POSNR estava comentado / MM - BUG SOLTO #142625 / AOENNING
*
*        LOOP AT it_zsdt0041_aux[] INTO DATA(ls_zsdt0041) WHERE doc_simulacao = lc_doc_simulacao.
*
*          LOOP AT it_zfit0026_aux INTO DATA(ls_0026) WHERE vbeln = ls_zsdt0041-vbeln.
        READ TABLE it_zsdt0041_aux TRANSPORTING NO FIELDS WITH KEY doc_simulacao = lc_doc_simulacao BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          DATA(vl_tabix2) = sy-tabix.

          LOOP AT it_zsdt0041_aux INTO DATA(ls_zsdt0041) FROM vl_tabix2.
            IF ls_zsdt0041-doc_simulacao NE lc_doc_simulacao.
              EXIT.

            ENDIF.

            READ TABLE it_zfit0026_aux TRANSPORTING NO FIELDS WITH KEY vbeln = ls_zsdt0041-vbeln BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              DATA(vl_tabix3) = sy-tabix.

              LOOP AT it_zfit0026_aux INTO DATA(ls_0026) FROM vl_tabix3.
                IF ls_0026-vbeln NE ls_zsdt0041-vbeln.
                  EXIT.

                ENDIF.
**<<<------"160696 - NMS - FIM------>>>
*          >>>>Inicio ajuste US 142091 / AOENNING.
                CLEAR: vg_mont_rbdo_liq.
                vg_mont_rbdo_liq = ls_0026-mont_rbdo.

                IF vg_mont_rbdo_liq > 0 AND ls_0026-vlr_multa_rbdo > 0.
                  vg_mont_rbdo_liq = vg_mont_rbdo_liq - ls_0026-vlr_multa_rbdo.
                ENDIF.

                IF vg_mont_rbdo_liq > 0 AND ls_0026-vlr_juros_rbdo > 0.
                  vg_mont_rbdo_liq = vg_mont_rbdo_liq - ls_0026-vlr_juros_rbdo.
                ENDIF.

                lv_mont_rbdo_sum += vg_mont_rbdo_liq.
*          >>>>Fim ajuste US 142091 / AOENNING.

                IF ls_0026-data_pgto > lv_data_pgto. " Rubenilson - 24.09.24 - #151588
                  lv_data_pgto      = ls_0026-data_pgto.
                ENDIF.

              ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
            ENDIF.
*          LOOP AT it_vbep_aux[] INTO DATA(ls_vbep) WHERE vbeln = ls_zsdt0041-vbeln.
            READ TABLE it_vbep_aux TRANSPORTING NO FIELDS WITH KEY vbeln = ls_zsdt0041-vbeln BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              DATA(vl_tabix4) = sy-tabix.

              LOOP AT it_vbep_aux INTO DATA(ls_vbep) FROM vl_tabix4.
                IF ls_vbep-vbeln NE ls_zsdt0041-vbeln.
                  EXIT.

                ENDIF.
**<<<------"160696 - NMS - FIM------>>>
                DATA(ls_vbak) = VALUE #( it_vbak_aux[ vbeln = ls_vbep-vbeln ] OPTIONAL ).
**<<<------"160696 - NMS - INI------>>>
*            LOOP AT it_konv_aux INTO DATA(ls_konv_aux) WHERE knumv EQ ls_vbak-knumv
*                                                         AND kposn EQ ls_vbep-posnr.
                READ TABLE it_konv_aux TRANSPORTING NO FIELDS WITH KEY knumv = ls_vbak-knumv
                                                                       kposn = ls_vbep-posnr
                                                              BINARY SEARCH.

                IF sy-subrc IS INITIAL.
                  DATA(vl_tabix5) = sy-tabix.

                  LOOP AT it_konv_aux INTO DATA(ls_konv_aux) FROM vl_tabix5.
                    IF ls_konv_aux-knumv NE ls_vbak-knumv OR
                       ls_konv_aux-kposn NE ls_vbep-posnr.
                      EXIT.

                    ENDIF.
**<<<------"160696 - NMS - FIM------>>>
*              if ls_konv_aux-knumv = ls_vbak-knumv and
*                 ls_konv_aux-kposn = ls_vbep-posnr.

                    IF ls_vbak-auart = 'ZTRI' OR
                       ls_vbak-auart = 'ZRPF' OR
                       ls_vbak-auart = 'ZROB' OR
                       ls_vbak-auart = 'ZREB' OR
                       ls_vbak-auart = 'ZFUT' OR
                       ls_vbak-auart = 'ZREM' . "Ajuste realizado 22-01-2025 BUG 164268/ aoenning.

                      lv_auart_sum += ls_konv_aux-kwert.

                      CONTINUE.
                    ENDIF.

                    lv_konv_kwert_sum += ls_konv_aux-kwert.

*              endif.

                  ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
                ENDIF.
**<<<------"160696 - NMS - FIM------>>>
              ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
            ENDIF.
**<<<------"160696 - NMS - FIM------>>>
          ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
        ENDIF.
**<<<------"160696 - NMS - FIM------>>>
        lv_konv_kwert_sum = lv_konv_kwert_sum -  lv_auart_sum .

        CLEAR: wa_saida_sd-augdt.
        IF lv_mont_rbdo_sum >= lv_konv_kwert_sum.

          wa_saida_sd-augdt = lv_data_pgto .

        ENDIF.
*<<<End-Stefanini-142091-ajustar regras da Data da Compensação - 21.06.2024 - Vitor Rienzo
**<<<------"184428 - NMS - INI------>>>
* Verifica o tipo do Frete.
        CASE wa_saida_sd-inco1.
          WHEN 'CPT' OR 'CIF'.
* Verifica o Grupo de mercadorias.
            CASE wa_saida_sd-matkl.
              WHEN '700230' OR '700240' OR '700280' OR '700350' OR '700150' OR '658440'.  "*-US192338-06.10.2025-#192338-JT
                DATA(vl_fator) = 1.

*                DATA(vl_unid) = |{ wa_saida_sd-unid CASE = UPPER }|.
*
*                CLEAR wa_saida_sd-vlr_frete.
*                SELECT SINGLE vlr_frete FROM zsdt0037
*                  INTO wa_saida_sd-vlr_frete
*                WHERE bukrs          EQ ls_vbak-vkorg
*                  AND matkl          EQ wa_saida_sd-matkl
*                  AND filial_origem  EQ wa_saida_sd-werks
*                  AND filial_destino EQ wa_saida_sd-vkbur.
*
*                IF sy-subrc IS INITIAL AND
*                   vl_unid  EQ 'KG'.
*                  DATA(vl_fator) = 1000.
*
*                ELSE.
*                  vl_fator = 1.
*
*                ENDIF.

              WHEN '700460' OR '658445'.
* SET % para valor do Hedge Frete Defensivos.
                CALL FUNCTION 'G_SET_GET_ALL_VALUES'
                  EXPORTING
                    setnr         = 'MAGGI_FRI_HEDGE'
                    table         = 'VBAP'
                    class         = '0000'
                    fieldname     = 'VBEAV'
                  TABLES
                    set_values    = tl_values
                  EXCEPTIONS
                    set_not_found = 1
                    OTHERS        = 2.

                IF sy-subrc IS INITIAL.
                  IF line_exists( tl_values[ 1 ] ).
                    DATA(vl_per_frete) = tl_values[ 1 ]-from.
                    TRANSLATE vl_per_frete USING ',.'.
                    vl_per_frete = vl_per_frete / 100.
                    CONDENSE vl_per_frete NO-GAPS.
                    wa_saida_sd-vlr_frete_int = vl_per_frete * wa_saida_sd-kbetr2.   "*-US192338-06.10.2025-#192338-JT
                    wa_saida_sd-vlr_frete     = wa_saida_sd-vlr_frete_int.           "*-US192338-06.10.2025-#192338-JT
                  ENDIF.

                ENDIF.

                vl_fator = 1.

              WHEN OTHERS.
                DATA(vl_unid) = |{ wa_saida_sd-unid CASE = UPPER }|.

                CLEAR wa_saida_sd-vlr_frete.
                SELECT SINGLE vlr_frete FROM zsdt0037
                  INTO wa_saida_sd-vlr_frete
                WHERE bukrs          EQ ls_vbak-vkorg
                  AND matkl          EQ wa_saida_sd-matkl
                  AND filial_origem  EQ wa_saida_sd-werks
                  AND filial_destino EQ wa_saida_sd-vkbur.

                IF sy-subrc IS INITIAL AND
                   vl_unid  EQ 'KG'.
                  vl_fator = 1000.
                ELSE.
                  vl_fator = 1.
                ENDIF.

*             Do nothing
            ENDCASE.

          WHEN 'CFR' OR 'FOB'.
            CLEAR wa_saida_sd-vlr_frete.
            vl_fator = 1.

          WHEN OTHERS.
*         Do nothing
        ENDCASE.
* Valor total do Frete.
        wa_saida_sd-vlr_frete_tot = ( wa_saida_sd-qtd / vl_fator ) * wa_saida_sd-vlr_frete.  "*-US192338-06.10.2025-#192338-JT
**<<<------"184428 - NMS - FIM------>>>
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

  "Início - DEVK9A1PQE - 29.09.2023 SD - ZSDT0051 - Adicionar novos campos e filtro #123690 RSA
  IF NOT it_saida_sd[] IS INITIAL.
    IF conv_sd EQ abap_true OR ecom_sd EQ abap_true.
      DELETE it_saida_sd WHERE tipo_venda IS INITIAL.
    ENDIF.
  ENDIF.
  "Fim - DEVK9A1PQE - 29.09.2023 SD - ZSDT0051 - Adicionar novos campos e filtro #123690 RSA


ENDFORM.                    " FORM_SAIDA_SD

*&---------------------------------------------------------------------*
*&      Form  FORM_ALV_SD
*&---------------------------------------------------------------------*
FORM form_alv_sd.
  PERFORM alv_preenche_cat USING:
       'KUNNR'              TEXT-100      '15'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Cliente "*-CS2021000218-14.09.2022-#90705-JT
       'NAME1'              TEXT-011      '35'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Cliente
       'STCD1'              TEXT-101      '15'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " STCD1   "*-CS2021000218-14.09.2022-#90705-JT
       'STCD3'              TEXT-102      '10'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " STCD3   "*-CS2021000218-14.09.2022-#90705-JT
       'ORT01'              TEXT-103      '30'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " ORT01   "*-CS2021000218-14.09.2022-#90705-JT
       'REGIO'              TEXT-104      '04'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " REGIO   "*-CS2021000218-14.09.2022-#90705-JT
*
       'VKBUR'              TEXT-012      '4'       ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Escritorio de vendas
       'BEZEI'              TEXT-082      '10'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Descrição Escritorio de vendas
       'REGIONAL'           TEXT-083      '10'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Regional
       'DOC_SIMULACAO'      TEXT-067      '10'      ' ' 'X'    'IT_SAIDA_SD'  ''  '' '', " Doc de Simulação
       'DT_SIMUL'           TEXT-098      '12'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Data Simulação  "*-CS2021000218-14.09.2022-#90705-JT
       'LIFSP'              TEXT-041      '4'       ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Escritorio de vendas (Bloq)
       'LIFSP_T'            TEXT-085      '10'      'X' ' '    'IT_SAIDA_SD'  ''  '' '', " Desc. Bloqueio
       'AUART'              TEXT-070      '08'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Tp. Ordem
       'VBELN'              TEXT-013      '10'      ' ' 'X'    'IT_SAIDA_SD'  ''  '' '', " Contrato
       'POSNR'              TEXT-034      '6'       ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Item Contrato
       'MATNR'              TEXT-014      '6'       'X' ' '    'IT_SAIDA_SD'  ''  '' '', " Material
       'WRKST'              TEXT-044      '10'      'X' ' '    'IT_SAIDA_SD'  ''  '' '', " Marca
       'CULTIVAR'           TEXT-130      '15'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Cultivar           "**<<<------"192338 - GA------>>>
       'MATKL'              TEXT-105      '10'      ' ' ' '    'IT_SAIDA_SD'  ''  '' '', " Grp.Merc "*-CS2021000218-14.09.2022-#90705-JT
       'STATUS'             TEXT-042      '2'       'X' ' '    'IT_SAIDA_SD'  ''  '' '', "
       'ZTERM'              TEXT-043      '20'      'X' ' '    'IT_SAIDA_SD'  ''  '' '', "
       'VTEXT'              TEXT-045      '20'      'X' ' '    'IT_SAIDA_SD'  ''  '' '', " Denominação da condição de pagamento
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 101103 - Ajuste_Relatório_ZSDT0027_ZSDT0051
* inserindo: VBKD-INCO1 / VBKD-INCO2
       'INCO1'              TEXT-047      '6'      'X' ' '    'IT_SAIDA_SD'  ''  ''   ''          , "INCO1
       'INCO2'              TEXT-048      '6'      'X' ' '    'IT_SAIDA_SD'  ''  ''   ''           , "INCO2
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END
       'ARKTX'              TEXT-037      '40'      'X' ' '    'IT_SAIDA_SD'  ''  '' '',
       'QTD'                TEXT-015      '10'      ' ' ' '    'IT_SAIDA_SD'  'X' 'C500' '', " Qte.Contrato
       'UNID'               TEXT-035      '03'      ' ' ' '    'IT_SAIDA_SD'  ''  ''     '',     " U.M da venda
       'QTEFATURADO'        TEXT-016      '10'      ' ' 'X'    'IT_SAIDA_SD'  'X' 'C500' '', " Qte.Faturado
       'SALDO'              TEXT-033      '13'      ' ' ' '    'IT_SAIDA_SD'  'X' 'C300' '', " Saldo Qte. Contrato
       'QTE_SOL'            TEXT-084      '13'      ' ' ' '    'IT_SAIDA_SD'  'X' 'C500' '', " Vol. Liberado em Solicitação de entrega
*       'ANGNR'              TEXT-053      '13'      ' ' ' '    'IT-SAIDA_SD'     ' '   ' ' , " Taxa Pagamento
       'WAERS'              TEXT-038      '3'       ' ' ' '    'IT_SAIDA_SD'  ''  ''     ''  , " Moeda
       'KBETR'              TEXT-039      ''        ''  ''     'IT_SAIDA_SD'  ''  ''     '', " Valor Unitario Liquido.
       'KBETR2'             TEXT-072      ''        ''  ''     'IT_SAIDA_SD'  ''  ''     '', " Vl. Unit Bt
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 >>> INI
* CH 100102 - Ajuste_Relatório_ZSDT0027_ZSDT0051
*
       'VLR_QTECONT'        TEXT-049      '15'      ''  ''     'IT_SAIDA_SD'  'X'  'C500' '', " Valor total liquido do Contrato.
       'VLR_TOTBRT'         TEXT-106      '15'      ''  ''     'IT_SAIDA_SD'  'X'  'C500' '', " Valor Total Bruto do Contrato   "*-CS2021000218-14.09.2022-#90705-JT
*
       'VLR_QTEFAT'         TEXT-050      '15'      ''  ''     'IT_SAIDA_SD'  'X'  'C500' '', " Valor total Liq Faturado.
       'VLR_TOTBRT_FAT'     TEXT-107      '15'      ''  ''     'IT_SAIDA_SD'  'X'  'C500' '', " Valor Total Bruto Faturado"*-CS2021000218-14.09.2022-#90705-JT
*
       'VLR_SALDO'          TEXT-051      '15'      ''  ''     'IT_SAIDA_SD'  'X'  'C300' '', " Valor Saldo bruto do contrato.
       'KURRF'              TEXT-053      '6'       ''  ''     'IT_SAIDA_SD'  ''   ''     '', " Taxa Pagamento
*** Modificação - Eduardo Ruttkowski Tavares - 15.08.2013 <<< END
       'VALDT'              TEXT-040      '10'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Data de Vencimento
       'AUGDT'              TEXT-086      '10'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', " Dt. Compensação
       'NRPROPLMT'          TEXT-087      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', " Nº Proposta Limite
       'ALCPROPLMT'         TEXT-088      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', " Alçada Prop. Limite
       'NRPROPOPR'          TEXT-089      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', " Nº Proposta Operação
       'ALCPROPOPR'         TEXT-090      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', " Alçada Prop. Operação
       'EFETIVADA'          TEXT-091      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', " Efetivada
       'ERDAT'              TEXT-052      '10'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Data de Vencimento
       'SAFRA'              TEXT-055      '06'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Safra
       'CULTURA'            TEXT-056      '10'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Cultura
* PBI - 55017 - Inicio
       'SAFRA_APL'          TEXT-080      '06'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Safra Aplicação
       'CULTURA_APL'        TEXT-081      '10'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Cultura Aplicação
* PBI - 55017 - Fim
       'VLR_FRETE'          TEXT-068      '10'     'X'  ''     'IT_SAIDA_SD'  ''   '' '', " Vl. Frete          "**<<<------"184428 - NMS------>>>
       'VLR_FRETE_TOT'      TEXT-131      '10'     'X'  ''     'IT_SAIDA_SD'  ''   '' '', " Vl. Tot. Frete     "**<<<------"184428 - NMS------>>>
       'J_1BCFOP'           TEXT-071      '10'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " CFOP
       'J_1BTXSDC'          TEXT-073      '6'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Cód Imp.

       'WERKS'              TEXT-094      '10'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Centro
       'LGORT'              TEXT-095      '6'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Deposito
       'TXCONTRATADA'       TEXT-116      '6'      ''  ''     'IT_SAIDA_SD'  ''   '' '', " Taxa Contratada
       'DT_ENTREGA'         TEXT-097      '10'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', "Data de Entrega
*       'KBETR     '         text-117      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' ''. "Desc.Abs
       'COD_VENDEDOR'       TEXT-119      '6'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', "Cod. vendedor
       'NOME_VENDEDOR'      TEXT-120      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', "Nome vendedor
       'TIPO_VENDA'         TEXT-121      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', "Tipo venda
       'PED_ECOMM'          TEXT-122      '15'     ' ' ' '     'IT_SAIDA_SD'  ''   '' '', "Ped. ecommerce.
       'MEIO_PAGO'          TEXT-123      '10'     ' ' ' '     'IT_SAIDA_SD'  ''   '' ''. "Meio pagamento

ENDFORM.                    " FORM_ALV_SD
**<<<------"160696 - NMS - INI------>>>
**&---------------------------------------------------------------------*
**&      Form  F_BDC_FIELD
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_field  USING    VALUE(p_flag)
*                           VALUE(p_fnam)
*                           VALUE(p_fval).
*  CLEAR t_bdc.
*
*  IF NOT p_flag IS INITIAL.
*    t_bdc-program  = p_fnam.
*    t_bdc-dynpro   = p_fval.
*    t_bdc-dynbegin = 'X'.
*  ELSE.
*    t_bdc-fnam = p_fnam.
*    t_bdc-fval = p_fval.
*  ENDIF.
*
*  APPEND t_bdc.
*
*ENDFORM.                    " F_BDC_FIELD
**<<<------"160696 - NMS - FIM------>>>

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
                               p_just  TYPE c.

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
  wl_fcat-just = p_just.


**  Begin of CS2022000771   #83814 FF   25.01.2023 14:25:40
  IF wl_fcat-fieldname = 'BEDAT'. "Data do pedido de compras
*    wl_fcat-scrtext_l = text-108.
    wl_fcat-scrtext_m = TEXT-113.
    wl_fcat-scrtext_s = TEXT-113.
  ENDIF.

  IF wl_fcat-fieldname = 'DATA_ATUAL'. "Data do pedido de compras
*    wl_fcat-scrtext_l = text-108.
  ENDIF.


  IF mm IS NOT INITIAL.
    IF wl_fcat-fieldname = 'KBETR_TOT' AND wl_fcat-fieldname = 'KBETR_SAL'.
      "Não alterar os parâmetros....
    ELSE.
** End of FF  25.01.2023 14:25:40
      IF wl_fcat-fieldname(5) = 'KBETR' OR wl_fcat-fieldname(5) = 'NETPR' OR wl_fcat-fieldname = 'ADIANTAMENTO'.
        wl_fcat-ref_field = 'NETPR'.
        wl_fcat-ref_table = 'KOMP'.


**  Begin of CS2022000771   #83814 FF   25.01.2023
        IF wl_fcat-fieldname = 'KBETR'. "Vl. Unit br
          wl_fcat-scrtext_l = TEXT-072.
          wl_fcat-scrtext_m = TEXT-072.
          wl_fcat-scrtext_s = TEXT-072.

** End of FF  25.01.2023
        ENDIF.

      ENDIF.
    ENDIF.

    IF wl_fcat-fieldname = 'MENGE' OR
       wl_fcat-fieldname = 'QTD' .
      wl_fcat-ref_field = 'MENGE'.
      wl_fcat-ref_table = 'EKPO'.
    ENDIF.
  ENDIF.

  IF  wl_fcat-fieldname = 'ZSALDO'.
    wl_fcat-ref_field = 'NETPR'.
    wl_fcat-ref_table = 'KOMP'.
  ENDIF.

*>>>Begin-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo
  IF  wl_fcat-fieldname = 'QTREMFINAL' OR
      wl_fcat-fieldname = 'VLREMFINAL'.
    wl_fcat-ref_field = 'NETPR'.
    wl_fcat-ref_table = 'KOMP'.
  ENDIF.
*<<<End-US144007-Correção logica coluna Vl faturado - 08.07.2024 - Vitor Rienzo

  IF wl_fcat-fieldname = 'VLR_QTECONT'    OR
     wl_fcat-fieldname = 'VLR_TOTBRT'     OR
     wl_fcat-fieldname = 'VLR_QTEFAT'     OR
     wl_fcat-fieldname = 'VLR_TOTBRT_FAT' OR
     wl_fcat-fieldname = 'VLR_SALDO'   .
    wl_fcat-scrtext_l = abap_off.
    wl_fcat-scrtext_m = abap_off.
    wl_fcat-scrtext_s = abap_off.
    wl_fcat-reptext   = p_desc  .
    wl_fcat-col_opt   = abap_true.
  ENDIF.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

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
    CHECK e_row_id-rowtype IS INITIAL.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

  METHOD: zm_handle_hotspot2.
    CHECK e_row_id-rowtype IS INITIAL.
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
  gs_variant_c-handle = '0100'.

  IF mm = 'X'.
    CALL METHOD wa_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        is_variant                    = gs_variant_c
        i_save                        = 'A'
      CHANGING
        it_outtab                     = <fs_table>
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
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
  ENDIF.


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
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
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
      CALL METHOD wa_alv->refresh_table_display.
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*&      Form  HIDE_MM_OPTIONS
*&---------------------------------------------------------------------*
FORM hide_mm_options .
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      WHEN 'SD'.
        screen-active = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " HIDE_MM_OPTIONS

*&---------------------------------------------------------------------*
*&      Form  HIDE_SD_OPTIONS
*&---------------------------------------------------------------------*
FORM hide_sd_options .
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'SD'.
        screen-active = 1.
        MODIFY SCREEN.
      WHEN 'MM'.
        screen-active = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " HIDE_SD_OPTIONS

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id
                                p_e_column_id
                                p_es_row_no.


  IF mm = 'X'.

*    READ TABLE it_saida INTO wa_saida INDEX p_e_row_id.

    READ TABLE <fs_table> INTO <fs_wa>  INDEX p_e_row_id.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa>
            TO <fs_field>.
      IF sy-subrc = 0.
        wa_saida-ebeln = <fs_field>.
      ENDIF.
      ASSIGN COMPONENT 'SUBMI' OF STRUCTURE <fs_wa>
           TO <fs_field>.
      IF sy-subrc = 0.
        wa_saida-submi = <fs_field>.
      ENDIF.
    ENDIF.

    CASE p_e_column_id.

      WHEN 'SUBMI'.
        SET PARAMETER ID 'SOLI_51' FIELD  wa_saida-submi.
        CALL TRANSACTION 'ZMM0149' AND SKIP FIRST SCREEN.
      WHEN 'EBELN'.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
        CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
**  Begin of CS2022000771   #83814 FF   26.01.2023
      WHEN 'NRO_SOL'.

        DATA: i_nro_sol TYPE  num10.

        ASSIGN COMPONENT 'NRO_SOL' OF STRUCTURE <fs_wa>
            TO <fs_field>.
        IF <fs_field> IS ASSIGNED.

          i_nro_sol = <fs_field>.

          CALL FUNCTION 'Z_FUC_EXIBIR_ZFI0025'
            EXPORTING
              i_nro_sol = i_nro_sol.
        ENDIF.
** End of FF  26.01.2023
      WHEN 'QTD'.

        CLEAR : wa_saida_mm2,
                it_saida_mm2,
                it_fcat2.

        DATA: bel_refkey TYPE j_1bnflin-refkey,
              vrefkey    TYPE j_1bnflin-refkey.

        DATA: wa_lin TYPE j_1bnflin,
              it_lin TYPE TABLE OF j_1bnflin,
              wa_doc TYPE j_1bnfdoc.



        SORT: it_rbkp      BY belnr,
              it_j_1bnflin BY refkey,
              it_j_1bnfdoc BY docnum.

        ASSIGN COMPONENT 'NAME1' OF STRUCTURE <fs_wa>
              TO <fs_field>.
        IF sy-subrc = 0.
          wa_saida-name1 = <fs_field>.
        ENDIF.

        wa_saida_mm2-name12     = wa_saida-name1  . " Fornecedor
        wa_saida_mm2-ebeln2     = wa_saida-ebeln  . " Pedido

        ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_wa>
              TO <fs_field>.
        IF sy-subrc = 0.
          wa_saida-ebelp = <fs_field>.
        ENDIF.

        LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln EQ wa_saida-ebeln AND ebelp EQ wa_saida-ebelp . " Dados da Fatura

          IF NOT sy-subrc IS INITIAL.
            CONTINUE.
          ENDIF.

          IF NOT wa_ekbe-budat IN p_fatu.
            CONTINUE.
          ENDIF.

          READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr = wa_ekbe-belnr gjahr = wa_ekbe-gjahr BINARY SEARCH. " Valor fatura

          bel_refkey = 0.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_ekbe-belnr
            IMPORTING
              output = wa_ekbe-belnr.
          vrefkey = wa_ekbe-belnr.

          CONCATENATE  wa_ekbe-belnr wa_ekbe-gjahr INTO bel_refkey .

          CLEAR: wa_lin, wa_doc.

          SELECT SINGLE * FROM  j_1bnflin INTO wa_lin WHERE refkey EQ bel_refkey.
          SELECT SINGLE * FROM j_1bnfdoc INTO wa_doc WHERE docnum EQ wa_lin-docnum.

          IF ( wa_lin-docnum IS INITIAL ).

            MOVE wa_rbkp-bldat  TO wa_doc-docdat.
            MOVE wa_rbkp-rmwwr  TO wa_lin-netwr.
            MOVE wa_ekbe2-menge TO wa_lin-menge.


            CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
              EXPORTING
                ref_number   = wa_rbkp-xblnr
              IMPORTING
                nf_number    = wa_doc-nfnum
*               series       = wa_j_1bnfdoc-series
              EXCEPTIONS
                number_error = 1
                OTHERS       = 2.

            MOVE wa_doc-nfnum TO wa_doc-nfenum.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_doc-nfenum
              IMPORTING
                output = wa_doc-nfenum.

          ELSE.

            IF NOT wa_doc-nfe = 'X'.
              MOVE wa_doc-nfnum TO wa_doc-nfenum.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_doc-nfenum
                IMPORTING
                  output = wa_doc-nfenum.
            ENDIF.


          ENDIF.


          wa_saida_mm2-belnr2     = wa_ekbe-belnr.             " Doc Fatura
          wa_saida_mm2-budat      = wa_ekbe-budat. "wa_j_1bnfdoc-docdat.        " Dt.Emissão

          wa_saida_mm2-docnum2    = wa_lin-docnum.        " Doc.Fiscal
          wa_saida_mm2-nfenum2    = wa_doc-nfenum.        " Nro.Nf.


          IF wa_ekbe-shkzg = 'H'. " Qte.Faturado
            wa_saida_mm2-qtd = - wa_ekbe-menge.
            wa_saida_mm2-dmbtr = - wa_ekbe-dmbtr.  "wa_j_1bnflin-netwr.         " Valor Nota
          ELSEIF wa_ekbe-shkzg = 'S'.
            wa_saida_mm2-qtd =  wa_ekbe-menge.
            wa_saida_mm2-dmbtr = wa_ekbe-dmbtr.  "wa_j_1bnflin-netwr.         " Valor Nota
          ENDIF.
          ASSIGN COMPONENT 'UNID' OF STRUCTURE <fs_wa>
                TO <fs_field>.
          IF sy-subrc = 0.
            wa_saida-unid = <fs_field>.
          ENDIF.

          wa_saida_mm2-meins  = wa_saida-unid.

          APPEND wa_saida_mm2 TO it_saida_mm2.

        ENDLOOP.

        PERFORM form_alv_mm2.

        CALL SCREEN 0200.

      WHEN 'ADIANTAMENTO'.

        CLEAR: vg_ebeln, vg_ebelp.
        FREE: linha_selecionada,_exit, it_zfit0046_aux.

        IF sy-subrc = 0.
          ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa>
                TO <fs_field>.
          IF sy-subrc = 0.
            vg_ebeln = <fs_field>.
          ENDIF.

          ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_wa>
               TO <fs_field>.
          IF sy-subrc = 0.
            vg_ebelp = <fs_field>.
          ENDIF.
        ENDIF.

        LOOP AT it_zfit0046 INTO DATA(ws_zfit0046) WHERE ebeln EQ vg_ebeln AND ebelp EQ vg_ebelp.
          MOVE-CORRESPONDING ws_zfit0046 TO ws_zfit0046_aux.
          READ TABLE it_zfit0045 INTO DATA(ws_zfit0045) WITH KEY nro_sol = ws_zfit0046-nro_sol.
          IF sy-subrc EQ 0.
            ws_zfit0046_aux-status = ws_zfit0045-status.
          ENDIF.

          IF ws_zfit0046_aux IS NOT INITIAL.
            APPEND ws_zfit0046_aux TO it_zfit0046_aux.
          ENDIF.
          CLEAR: ws_zfit0046_aux, ws_zfit0045.
        ENDLOOP.


        "Construindo ALV.
        DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(

          ( fieldname = 'NRO_SOL   '        seltext_m = 'Nr solicitação    '  outputlen = '15' )
          ( fieldname = 'EBELN     '        seltext_m = 'Pedido            '  outputlen = '13' )
          ( fieldname = 'EBELP     '        seltext_m = 'Item              '  outputlen = '10' )
          ( fieldname = 'VLR_ADIANTAMENTO'  seltext_m = 'Valor adiantamento'  outputlen = '10' do_sum = 'X')
          ( fieldname = 'STATUS    '        seltext_m = 'Status            '  outputlen = '10' )
          ).

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            i_title     = 'Status solicitação adiantamento pedido'
            i_selection = 'X'
            i_tabname   = 'IT_ZFIT0046_AUX'
            i_zebra     = 'X'
            it_fieldcat = tl_fieldcat
          IMPORTING
            es_selfield = linha_selecionada
            e_exit      = _exit
          TABLES
            t_outtab    = it_zfit0046_aux.


        CASE linha_selecionada-fieldname.
          WHEN 'NRO_SOL'.
            CLEAR: nr_solic.
            nr_solic = linha_selecionada-value.
            CALL FUNCTION 'Z_FUC_EXIBIR_ZFI0025'
              EXPORTING
                i_nro_sol = nr_solic.
        ENDCASE.


    ENDCASE.

  ENDIF.

  IF sd = 'X'.
    DATA opt TYPE ctu_params.

    READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.

    CASE p_e_column_id.

      WHEN 'VBELN'. " Contrato
        SET PARAMETER ID 'AUN' FIELD wa_saida_sd-vbeln.
        CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.

      WHEN 'QTEFATURADO'.

        CLEAR : wa_saida_sd2,
                it_saida_sd2,
                it_fcat2.

        DATA: day(2)    TYPE c,
              month(2)  TYPE c,
              year(4)   TYPE c,
              dta(8)    TYPE c,
              vg_refkey TYPE j_1bnflin-refkey,
              vg_refitm TYPE j_1bnflin-refitm,  "*-BUG 78045-11.05.2022-JT-inicio
              it_vbfa_2 TYPE STANDARD TABLE OF vbfa,
              wa_vbfa_2 TYPE vbfa.

        SORT:
              it_j_1bnflin BY refkey itmnum,
              it_j_1bnfdoc BY docnum,
              it_vbfa      BY vbelv posnn,
              it_vbrk      BY vbeln.

        IF NOT it_vbfa[] IS INITIAL.
          SELECT *
                 FROM vbfa
                 INTO TABLE it_vbfa_2
                 FOR ALL ENTRIES IN it_vbfa
                 WHERE vbeln EQ it_vbfa-vbeln.
        ENDIF.

        "LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV EQ WA_SAIDA_SD-VBELN AND POSNN EQ WA_SAIDA_SD-POSNR . " Dados da Fatura
        LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ wa_saida_sd-vbeln AND posnv EQ wa_saida_sd-posnr . " Dados da Fatura

          READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.

          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          IF wa_vbrk-fkart = 'ZTRI'.
            CONTINUE.
          ENDIF.

          wa_vbfa-vbeln = |{ wa_vbfa-vbeln ALPHA = IN }|.
          vg_refkey = wa_vbfa-vbeln.
          vg_refitm = wa_vbfa-posnn.  "*-BUG 78045-11.05.2022-JT-inicio

          CLEAR: wa_lin, wa_doc.
          FREE it_lin.

*          SELECT SINGLE * FROM J_1BNFLIN INTO WA_LIN WHERE REFKEY EQ VG_REFKEY
*                                                       AND ITMNUM EQ WA_VBFA-POSNN.
          SELECT * FROM j_1bnflin INTO TABLE it_lin WHERE refkey EQ vg_refkey
                                                      AND refitm EQ vg_refitm. "*-BUG 78045-11.05.2022-JT-inicio

          LOOP AT it_lin INTO wa_lin.

            SELECT SINGLE *
              FROM j_1bnfdoc
              INTO wa_doc
              WHERE docnum EQ wa_lin-docnum.

            IF NOT wa_doc-nfe = 'X'.
              MOVE wa_doc-nfnum TO wa_doc-nfenum.
              wa_doc-nfenum = |{ wa_doc-nfenum ALPHA = IN }|.
            ENDIF.

            CLEAR: wa_saida_sd2-vbeln3.

            IF wa_vbfa-vbtyp_n EQ 'O'.

*              READ TABLE IT_VBFA_2 INTO WA_VBFA_2 WITH KEY VBELN   = WA_VBFA-VBELN
*                                                           POSNN   = WA_VBFA-POSNN
*                                                           VBTYP_N = WA_VBFA-VBTYP_N
*                                |                           VBTYP_V = 'H'.
              READ TABLE it_vbfa_2 INTO wa_vbfa_2 WITH KEY vbeln   = wa_lin-refkey
                                                           posnn   = wa_lin-refitm
                                                           vbtyp_n = wa_vbfa-vbtyp_n
                                                           vbtyp_v = 'H'.

              IF sy-subrc IS INITIAL.
                wa_saida_sd2-vbeln3  = wa_vbfa_2-vbelv.
              ENDIF.

            ELSEIF wa_vbfa-vbtyp_n EQ 'N'.

*              READ TABLE IT_VBFA_2 INTO WA_VBFA_2 WITH KEY VBELN   = WA_VBFA-VBELN
*                                                           POSNN   = WA_VBFA-POSNN
*                                                           VBTYP_N = WA_VBFA-VBTYP_N
*                                                           VBTYP_V = 'M'.
              READ TABLE it_vbfa_2 INTO wa_vbfa_2 WITH KEY vbeln   = wa_lin-refkey
                                                           posnn   = wa_lin-refitm
                                                           vbtyp_n = wa_vbfa-vbtyp_n
                                                           vbtyp_v = 'M'.

              IF sy-subrc IS INITIAL.
                wa_saida_sd2-vbeln3  = wa_vbfa_2-vbelv.
              ENDIF.

            ENDIF.

            wa_saida_sd2-name12  = wa_saida_sd-name1.
            wa_saida_sd2-vbeln2  = wa_vbfa-vbelv.
            wa_saida_sd2-vbelnk  = wa_vbfa-vbeln.

            wa_saida_sd2-docnum2 = wa_lin-docnum.
            wa_saida_sd2-nfenum  = wa_doc-nfenum.

            wa_saida_sd2-vrkme   =  wa_saida_sd-unid.

            day   = ''.
            month = ''.
            year  = ''.
            dta   = ''.

            day   =  wa_vbfa-erdat+6(2) .
            month =  wa_vbfa-erdat+4(2) .
            year  =  wa_vbfa-erdat(4) .

            dta = |{ year }{ month }{ day  }|.

            wa_saida_sd2-erdat2 =    dta.       "Dt.Emissão


            IF wa_vbfa-vbtyp_n = 'M'.

*              ADD wa_vbfa-rfwrt TO wa_saida_sd2-rfwrt2.       " Valor Nota
              ADD wa_lin-netwr  TO wa_saida_sd2-rfwrt2.       " Valor Nota
              ADD wa_lin-menge  TO wa_saida_sd2-qtdfatur.     " Qte. Faturado

            ELSEIF ( wa_vbfa-vbtyp_n = 'N' ) OR ( wa_vbfa-vbtyp_n = 'O' ).

*              MULTIPLY WA_VBFA-RFWRT BY -1.
*              ADD WA_VBFA-RFWRT TO WA_SAIDA_SD2-RFWRT2.   " Valor Nota
*              MULTIPLY wa_vbfa_2-rfwrt BY -1.
*              ADD wa_vbfa_2-rfwrt TO wa_saida_sd2-rfwrt2.   " Valor Nota
              MULTIPLY wa_lin-netwr BY -1.
              ADD wa_lin-netwr TO wa_saida_sd2-rfwrt2.   " Valor Nota

              MULTIPLY wa_lin-menge BY -1.
              ADD wa_lin-menge TO wa_saida_sd2-qtdfatur.  " Qte. Faturado

            ENDIF.

          ENDLOOP.

          APPEND wa_saida_sd2 TO it_saida_sd2.
          CLEAR: wa_saida_sd2.

        ENDLOOP.

        "DEVK9A1VQ7  - Melhorias - 137325 - RSA
        IF it_saida_sd2[] IS INITIAL.

          IF NOT it_zfiwrt0009[] IS INITIAL.
            CLEAR: it_zfiwrt0008[], it_parid[], it_kna1[], it_zib_contabil_chv[].
            SORT it_zfiwrt0009 BY seq_lcto.

            SELECT seq_lcto parid docnum nfenum bldat obj_key
                   FROM zfiwrt0008
                   INTO TABLE it_zfiwrt0008
                   FOR ALL ENTRIES IN it_zfiwrt0009
                   WHERE seq_lcto EQ it_zfiwrt0009-seq_lcto
                   AND   docs_estornados NE 'X'.

            SORT it_zfiwrt0008 BY seq_lcto.

            LOOP AT it_zfiwrt0008 INTO wa_zfiwrt0008.
              IF NOT wa_zfiwrt0008-parid IS INITIAL.
                wa_parid-parid = wa_zfiwrt0008-parid.
                APPEND wa_parid TO it_parid.
              ENDIF.

              IF NOT wa_zfiwrt0008-obj_key IS INITIAL.
                wa_obj_key-obj_key = wa_zfiwrt0008-obj_key.
                APPEND wa_obj_key TO it_obj_key.
              ENDIF.
            ENDLOOP.

            SORT it_parid BY parid.
            DELETE ADJACENT DUPLICATES FROM it_parid COMPARING parid.

            SORT it_obj_key BY obj_key.
            DELETE ADJACENT DUPLICATES FROM it_obj_key COMPARING obj_key.

            IF NOT it_parid[] IS INITIAL.
              SELECT kunnr name1
                     FROM kna1
                     INTO TABLE it_clientes
                     FOR ALL ENTRIES IN it_parid
                     WHERE kunnr = it_parid-parid.
              SORT it_clientes BY kunnr.
            ENDIF.

            IF NOT it_obj_key[] IS INITIAL.
              SELECT obj_key belnr
                     FROM zib_contabil_chv
                     INTO TABLE it_zib_contabil_chv
                     FOR ALL ENTRIES IN it_obj_key
                     WHERE obj_key = it_obj_key-obj_key.
              SORT it_zib_contabil_chv BY obj_key.
            ENDIF.

            LOOP AT it_zfiwrt0009 INTO wa_zfiwrt0009 WHERE vbeln EQ wa_saida_sd-vbeln AND posnr EQ wa_saida_sd-posnr .
              READ TABLE it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY seq_lcto = wa_zfiwrt0009-seq_lcto BINARY SEARCH.
              IF sy-subrc EQ 0.
                READ TABLE it_clientes INTO wa_clientes WITH KEY kunnr = wa_zfiwrt0008-parid BINARY SEARCH.
                IF sy-subrc EQ 0.
                  wa_saida_sd2-name12 = wa_clientes-name1.
                ENDIF.

                wa_saida_sd2-vbeln2   = wa_zfiwrt0009-vbeln.

                READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key =  wa_zfiwrt0008-obj_key BINARY SEARCH.
                IF sy-subrc EQ 0.
                  wa_saida_sd2-vbelnk = wa_zib_contabil_chv-belnr.
                ENDIF.

                wa_saida_sd2-docnum2 = wa_zfiwrt0008-docnum.

                wa_saida_sd2-nfenum  = wa_zfiwrt0008-nfenum.

                wa_saida_sd2-erdat2  = wa_zfiwrt0008-bldat.

                wa_saida_sd2-qtdfatur = wa_zfiwrt0009-menge.

                wa_saida_sd2-vrkme    = wa_zfiwrt0009-meins.

                SELECT SUM( dmbtr )
                       FROM zfiwrt0011
                       INTO @DATA(vl_valor_nota)
                       WHERE seq_lcto EQ @wa_zfiwrt0009-seq_lcto
                       AND   bschl    EQ '01'
                       AND   estorno  NE 'X'.

                IF wa_saida_sd-waers = 'USD'.
                  wa_saida_sd2-rfwrt2   = vl_valor_nota * l_netpr_brt_aux.
                ELSE.
                  wa_saida_sd2-rfwrt2   = vl_valor_nota.
                ENDIF.
                APPEND wa_saida_sd2 TO it_saida_sd2.
                CLEAR wa_saida_sd2.
              ENDIF.
            ENDLOOP.
          ENDIF.

        ENDIF.
        "DEVK9A1VQ7  - Melhorias - 137325 - RSA

        PERFORM form_alv_sd2.

        CALL SCREEN 0200.

      WHEN 'DOC_SIMULACAO'. " Doc Simulação - ZSDT0044

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





    ENDCASE.
  ENDIF.
ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  z_handle_hotspot2
*&---------------------------------------------------------------------*
FORM z_handle_hotspot2  USING   p_e_row_id
                                p_e_column_id
                                p_es_row_no.

  IF sd = 'X'.
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
  ELSE.
    READ TABLE it_saida_mm2 INTO wa_saida_mm2 INDEX p_e_row_id.
    CASE p_e_column_id.
      WHEN 'EBELN2'. " Pedido
        SET PARAMETER ID 'BES' FIELD wa_saida_mm2-ebeln2.
        CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.

      WHEN 'BELNR2'. " Doc Fatura
        SET PARAMETER ID 'RBN' FIELD wa_saida_mm2-belnr2.
        CALL TRANSACTION  'MIR4' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM2'. " doc. fiscal
        SET PARAMETER ID 'JEF' FIELD wa_saida_mm2-docnum2.
        CALL TRANSACTION  'J1B3N' AND SKIP FIRST SCREEN.

    ENDCASE.
  ENDIF.
ENDFORM.                    " Z_HANDLE_HOTSPOT


*&---------------------------------------------------------------------*
*&      Form  form_alv_mm2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM form_alv_mm2.

  PERFORM alv_preenche_cat_mm USING:

   'NAME12'     TEXT-017      '35'      ' ' ''      'IT_SAIDA_MM2' ''   '' ,
   'EBELN2'     TEXT-018      '10'      ' ' 'X'     'IT_SAIDA_MM2' ''   '' ,
   'BELNR2'     TEXT-019      '10'      ' ' 'X'     'IT_SAIDA_MM2' ''   '' ,
   'DOCNUM2'    TEXT-020      '10'      ' ' 'X'     'IT_SAIDA_MM2' ''   '' ,
   'NFENUM2'    TEXT-021      '9'       'X' ' '     'IT_SAIDA_MM2' ''   '' ,
   'BUDAT'      TEXT-022      '10'      ' ' ' '     'IT_SAIDA_MM2' ''   '' ,
   'QTD'        TEXT-023      '15'      ' ' ' '     'IT_SAIDA_MM2' 'X'  'C500',
*   'ANGNR'      TEXT-053      '13'      ' ' ' '     'IT-SAIDA_MM2' ' '  ' ', " Taxa Pagamento
   'MEINS'      TEXT-035      '3'       ' ' ' '     'IT_SAIDA_MM2' ''   '' ,
   'DMBTR'      TEXT-024      '15'      ' ' ' '     'IT_SAIDA_MM2' 'X'  'C500'.


ENDFORM.                    " FORM_ALV_mm2

*&---------------------------------------------------------------------*
*&      Form  form_alv_sd2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM form_alv_sd2.

  PERFORM alv_preenche_cat_mm USING:

  'NAME12'     TEXT-025      '35'      ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'VBELN2'     TEXT-026      '10'      ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'VBELN3'     TEXT-069      '10'      ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'VBELNK'     TEXT-027      '10'      ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'DOCNUM2'    TEXT-028      '10'      ' ' 'X'     'IT_SAIDA_SD2' ''    '',
*  'ANGNR'      TEXT-053      '13'      ' ' ' '     'IT-SAIDA_SD2' ''    '', " Taxa Pagamento
  'NFENUM'     TEXT-029      '9'       ' ' 'X'     'IT_SAIDA_SD2' ''    '',
  'ERDAT2'     TEXT-030      '10'      ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'QTDFATUR'   TEXT-031      '15'      ' ' ' '     'IT_SAIDA_SD2' 'X'   'C500',
  'VRKME'      TEXT-035      '3'       ' ' ' '     'IT_SAIDA_SD2' ''    '',
  'RFWRT2'     TEXT-032      '15'      ' '  ''     'IT_SAIDA_SD2' 'X'   'C500'.

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

  IF mm = 'X'.
    CALL METHOD wa_alv2->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout2
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_saida_mm2
        it_fieldcatalog               = it_fcat2
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD wa_alv2->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout2
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_saida_sd2
        it_fieldcatalog               = it_fcat2
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv2 IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  CONDICOES_MM
*&---------------------------------------------------------------------*
FORM condicoes_mm .

  wa_status-sign   = 'I'.
  wa_status-option = 'EQ'.

  IF ( bloq_mm = 'X' ).
    wa_status-low = 'S'.
    APPEND wa_status TO r_ekpo_s.
  ELSEIF ( elim_mm = 'X' ).
    wa_status-low = 'L'.
    APPEND wa_status TO r_ekpo_s.
  ELSEIF ( ativo_mm = 'X' ).
    wa_status-low = ''.
    APPEND wa_status TO r_ekpo_s.
  ELSE.

    CLEAR: wa_status-sign,
           wa_status-option.

    wa_status-sign   = 'I'.
    wa_status-option = 'EQ'.

    wa_status-low  = 'S'.
    wa_status-high = 'S'.
    APPEND wa_status TO r_ekpo_s.

    wa_status-low  = 'L'.
    wa_status-high = 'L'.
    APPEND wa_status TO r_ekpo_s.

    wa_status-low  = ''.
    wa_status-high = ''.
    APPEND wa_status TO r_ekpo_s.

  ENDIF.
ENDFORM.                    " CONDICOES_MM
**<<<------"160696 - NMS - INI------>>>
**&---------------------------------------------------------------------*
**&      Form  BUSCA_IMPOSTO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_WA_EKKO_EBELN  text
**      -->P_WA_EKPO_EBELP  text
**----------------------------------------------------------------------*
*FORM busca_imposto  USING    w_ebeln
*                             w_ebelp.
*
*  DATA: wa_ite  LIKE mepoitem.
*  CLEAR wa_ite.
*
*  CALL FUNCTION 'MEPO_DOC_ITEM_GET'
*    EXPORTING
*      im_ebelp = w_ebelp                                    "'00010'
*    IMPORTING
*      ex_item  = wa_ite
*    EXCEPTIONS
*      failure  = 1
*      OTHERS   = 2.
*  IF sy-subrc <> 0.
*  ENDIF.
*
*  DATA: BEGIN OF t_konv OCCURS 0.
*          INCLUDE STRUCTURE prcd_elements.
*  DATA: END OF t_konv.
*
*  TYPES: ty_konv TYPE TABLE OF komv.
*
*  FIELD-SYMBOLS: <lfa1>  TYPE lfa1,
*                 <ekpo>  TYPE ekpo,
*                 <ek2>   TYPE ekpo,
*                 <ekko>  TYPE ekko,
*                 <vorga> TYPE any,
*                 <konv>  TYPE ty_konv,
*                 <cva>   TYPE any.
*
*  ASSIGN ('(SAPLMEPO)ekpo') TO <ekpo>.
*  ASSIGN ('(SAPLMEPO)ekko') TO <ekko>.
*  ASSIGN ('(SAPLMEPO)lfa1') TO <lfa1>.
*
*
*  SELECT SINGLE * FROM ekpo INTO <ekpo>
*    WHERE ebeln = w_ebeln
*      AND ebelp = w_ebelp
*      AND loekz = space.
*
*  SELECT SINGLE * FROM ekko INTO <ekko>
*    WHERE ebeln = w_ebeln.
*
*  IF <ekpo>-bednr IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = <ekpo>-bednr
*      IMPORTING
*        output = <ekpo>-bednr.
*
*    SELECT SINGLE * FROM lfa1 INTO <lfa1>
*      WHERE lifnr = <ekpo>-bednr.
*
*    IF sy-subrc NE 0. "Se não existir fornecedor por linha
*      SELECT SINGLE * FROM lfa1 INTO <lfa1>
*        WHERE lifnr = <ekko>-lifnr.
*    ENDIF.
*  ELSE.
*    SELECT SINGLE * FROM lfa1 INTO <lfa1>
*       WHERE lifnr = <ekko>-lifnr.
*  ENDIF.
*
*
*  SELECT * FROM prcd_elements INTO TABLE t_konv
*    WHERE knumv = <ekko>-knumv.
*
*  ASSIGN ('(SAPLMEPO)fc_vorga') TO <vorga>.
*  ASSIGN ('(SAPLMEPO)cva_en') TO <cva>.
*  ASSIGN ('(SAPLMEPO)tkomv[]') TO <konv>.
*
*  <vorga> = <cva>.
*
*  PERFORM kond_taxes(saplmepo) USING 'D' 'X'.
*
*  CHECK <ekpo>-loekz = space.
*  ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <wmwst>.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_IMPOSTO
*&---------------------------------------------------------------------*
*       Cálculo do imposto por item de documento de compra.
*----------------------------------------------------------------------*
*      -->UE_PARAMETRO Parâmetros Func. Paral. Busca Imposto
*----------------------------------------------------------------------*
FORM busca_imposto USING ue_parametro TYPE zmme_paral_busca_imposto_param.

  DATA: vl_dest     TYPE char10 VALUE 'NONE',
        vl_taskname TYPE char12.

* Aguarda enquanto verifica se a quantidade de sessões solicitadas é superior ao número de sessões disponíveis
* Segurança de solicitações de sessões para não travar o servidor.
  WAIT UNTIL vg_task_ativa GT vg_tasks.
  ADD 1 TO vg_task_ativa.
  CONCATENATE 'PR' ue_parametro-ebeln INTO vl_taskname.

  CALL FUNCTION 'ZMMF_PARAL_BUSCA_IMPOSTO'
    DESTINATION vl_dest
    STARTING NEW TASK vl_taskname
    PERFORMING zf_retur_paral_busca_imposto ON END OF TASK
    EXPORTING
      ie_param = ue_parametro.
**<<<------"160696 - NMS - FIM------>>>
ENDFORM.

* Início - Sara Oikawa - 10.07.2020 - 38883 - Adequação na ZSDT0051
*&---------------------------------------------------------------------*
*&      Form  F_ARMAZENA_TABELA_ZSDT0257
*&---------------------------------------------------------------------*
FORM f_armazena_tabela_zsdt0257 .

  DATA: it_zsdt0257 TYPE TABLE OF zsdt0257,
        wa_zsdt0257 TYPE zsdt0257,
        lv_cont     TYPE sy-tfill.

  SORT it_vbak BY vbeln.
  SORT it_vbap BY vbeln posnr.

  REFRESH it_zsdt0257.

  LOOP AT it_saida_sd INTO wa_saida_sd.

    MOVE-CORRESPONDING wa_saida_sd TO wa_zsdt0257.

    wa_zsdt0257-qtdefaturado = wa_saida_sd-qtefaturado.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_saida_sd-vbeln
                                    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zsdt0257-vkorg       = wa_vbak-vkorg.
      wa_zsdt0257-spart       = wa_vbak-spart.
      wa_zsdt0257-kunnr       = wa_vbak-kunnr.
    ENDIF.

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_saida_sd-vbeln
                                             posnr = wa_saida_sd-posnr
                                    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zsdt0257-werks       = wa_vbap-werks.
    ENDIF.

    wa_zsdt0257-usnam       = sy-uname.
    wa_zsdt0257-data_atual  = sy-datum.
    wa_zsdt0257-hora_atual  = sy-uzeit.


    APPEND wa_zsdt0257 TO it_zsdt0257.

  ENDLOOP.

  EXPORT it_zsdt0257 TO DATABASE indx(zk) ID 'ZSDT0257'.

  "// Inicio BUG 160640 10-12-2024 - WBARBOSA
  IF p_prod IS NOT INITIAL.
    EXPORT it_zsdt0257 TO DATABASE indx(zk) ID 'GETAPPPRODUTOR'.
  ENDIF.
  "// Fim BUG 160640 10-12-2024 - WBARBOSA

ENDFORM.
* Fim - Sara Oikawa - 10.07.2020 - 38883 - Adequação na ZSDT0051

*&---------------------------------------------------------------------*
*&      Form  create_itab_dynamically
*&---------------------------------------------------------------------*
*       Create internal table dynamically
*----------------------------------------------------------------------*
FORM create_itab_dynamically.
* Create dynamic internal table and assign to Field-Symbol
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fcat
    IMPORTING
      ep_table        = dyn_table.
  ASSIGN dyn_table->* TO <fs_table>.
* Create dynamic work area and assign to Field Symbol
  CREATE DATA dyn_line LIKE LINE OF <fs_table>.
  ASSIGN dyn_line->* TO <fs_wa>.
ENDFORM.                    "create_itab_dynamically
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

"PSA KONV PARA PRCD_ELEMENTS
*&---------------------------------------------------------------------*
*& Form f_armazena_tabela_zmmt0189
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_armazena_tabela_zmmt0189 .

  DATA: it_zmmt0189 TYPE TABLE OF zmmt0189,
        wa_zmmt0189 TYPE zmmt0189,
        lv_cont     TYPE sy-tfill.

  SORT it_ekko BY ebeln.
  SORT it_ekpo BY ebeln ebelp.

  REFRESH it_zmmt0189.

  LOOP AT it_saida INTO wa_saida.

    MOVE-CORRESPONDING wa_saida TO wa_zmmt0189.

    wa_zmmt0189-qtdefaturado = wa_saida-qtefaturado.

    READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_saida-ebeln
                                    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
*      wa_zmmt0189-vkorg       = wa_ekko-vkorg.
*      wa_zmmt0189-spart       = wa_ekko-spart.
      wa_zmmt0189-lifnr       = wa_ekko-lifnr.
    ENDIF.

    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_saida-ebeln
                                             ebelp = wa_saida-ebelp
                                    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_zmmt0189-werks       = wa_ekpo-werks.
    ENDIF.

    wa_zmmt0189-usnam       = sy-uname.
    wa_zmmt0189-data_atual  = sy-datum.
    wa_zmmt0189-hora_atual  = sy-uzeit.


    APPEND wa_zmmt0189 TO it_zmmt0189.

  ENDLOOP.

  EXPORT it_zmmt0189 TO DATABASE indx(zk) ID 'ZMMT0189'.

ENDFORM.
*-US 156984-07-11-2024-#156984-RJF-Inicio
*&---------------------------------------------------------------------*
*&      Form  FORM_SAIDA
*&---------------------------------------------------------------------*
FORM form_saida_m .
  REFRESH it_saida.

  DATA es_parametro TYPE zmme_paral_busca_imposto_param. "<<------"160696 - NMS------>>>

  DATA: vrefkey      TYPE j_1bnflin-refkey,
        aux(100)     TYPE c,
        bel_refkey   TYPE j_1bnflin-refkey,
        v_meng_ori   TYPE ekpo-menge,
        vlin         TYPE i,
        vtot         TYPE i,
        vfator       TYPE i,
        clin(6),
        ctot(6),
        vmsg(50),
        it_saida_aux LIKE it_saida.
**<<<------"184428 - NMS - INI------>>>
  DATA(cll_mm_util) = NEW zcl_mm_util( ).

  DATA: vl_object TYPE ausp-objek.

  CONSTANTS: cl_cultivar  TYPE atnam      VALUE 'NOVA_CULTIVAR',
             cl_class     TYPE klasse_d   VALUE 'SEMENTES_GERAL',
             cl_classtype TYPE klassenart VALUE '001'.
**<<<------"184428 - NMS - FIM------>>>
  DATA(lv_wrbtr_sum)          = CONV rseg-wrbtr( '' ).
  DATA(lv_wrbtr_subtract)     = CONV rseg-wrbtr( '' ).
  DATA(lv_total_liq_faturado) = CONV komp-netwr( '' ).
  DATA(lv_imposto_nota_fiscal) = CONV komp-netwr( '' ).
  DATA(lv_refkey)             = CONV j_1bnflin-refkey( '' ).
  DATA(lv_buzei)              = CONV rseg-buzei( '' ).
**<<<------"160696 - NMS - INI------>>>
  DATA(tl_rbkp) = it_rbkp.
  DELETE tl_rbkp WHERE wmwst1 IS INITIAL.
  SORT tl_rbkp BY belnr gjahr DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_rbkp COMPARING belnr gjahr.
**<<<------"160696 - NMS - FIM------>>>
  SORT: it_ekko      BY ebeln,
        it_ekbe      BY ebeln ebelp,
        it_ekbe2     BY ebeln ebelp,
        it_ekpo      BY ebeln ebelp,
        it_rbkp      BY belnr gjahr ,
        it_lfa1      BY lifnr,
**<<<------"160696 - NMS - INI------>>>
        it_konv      BY knumv kposn kschl,
        it_zfit0045  BY ebeln,
        it_zfit0046  BY ebeln ebelp,
        it_zfit0046_sum BY ebeln ebelp,
**<<<------"160696 - NMS - FIM------>>>
        it_rseg      BY ebeln ebelp belnr gjahr buzei,   "**<<<------"184428 - NMS------>>>
        it_j_1bnflin BY refkey.

  DATA: lv_cont TYPE sy-tabix.
  DATA: lv_field TYPE c LENGTH 20.
  DATA: lv_text TYPE c LENGTH 20.
  DATA: lv_qtde TYPE ekpo-menge.
  DATA: lv_unit TYPE t006-msehi.
  DATA: lv_dec  TYPE p DECIMALS 3.
  DATA: lv_txt  TYPE c LENGTH 5.
  DATA: lv_vlr_tot_liq TYPE konv-kwert.

  DATA: lv_vlr_unit_brt TYPE konv-kaqty.
  SELECT SINGLE msehi
         FROM t006
         WHERE decan = 0
         INTO (@lv_unit).
**<<<------"160696 - NMS - INI------>>>
  CLEAR: vg_task_ativa, vg_tasks, vg_max_tasks.
* Obter o número de sessões disponíveis e a máxima.
  CALL FUNCTION 'SPBT_INITIALIZE'
    IMPORTING
      free_pbt_wps                   = vg_tasks
      max_pbt_wps                    = vg_max_tasks
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.
**<<<------"160696 - NMS - FIM------>>>
  CLEAR: vlin,vtot.
  DESCRIBE TABLE it_ekko LINES vtot.
  ctot = vtot.
  LOOP AT it_ekko INTO wa_ekko. " Cabeçalho da fatura
    ADD 1 TO vlin.
    clin = vlin.
    CLEAR: wa_ekpo.
    CONCATENATE 'Pedido '  wa_ekko-ebeln 'Linha ' clin '/' ctot INTO vmsg SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = vmsg.

    wa_saida-verkf = wa_ekko-verkf.
    wa_saida-telf1 = wa_ekko-telf1.
    wa_saida-bedat = wa_ekko-bedat.
**<<<------"160696 - NMS - INI------>>>
*    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln EQ wa_ekko-ebeln. "Item
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekko-ebeln BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      DATA(vl_tabix) = sy-tabix.

    ELSE.
      CLEAR: wa_ekko, wa_saida.
      CONTINUE.

    ENDIF.

    LOOP AT it_ekpo INTO wa_ekpo FROM vl_tabix. "Item
      IF wa_ekko-ebeln NE wa_ekpo-ebeln.
        CLEAR: wa_lfa1, wa_ekbe2, wa_konv, lv_vlr_tot_liq, lv_vlr_unit_brt.
        EXIT.

      ENDIF.
**<<<------"160696 - NMS - FIM------>>>
      CLEAR: wa_lfa1, wa_ekbe2, wa_konv, lv_vlr_tot_liq, lv_vlr_unit_brt.

      v_meng_ori = wa_ekpo-menge.
      wa_saida-mwskz = wa_ekpo-mwskz.
      wa_saida-lgort = wa_ekpo-lgort.
      wa_saida-verkf = wa_ekko-verkf.
      wa_saida-telf1 = wa_ekko-telf1.

      READ TABLE it_lfa1  INTO wa_lfa1  WITH KEY lifnr = wa_ekko-lifnr BINARY SEARCH. " Fornecedor

      READ TABLE it_ekbe2 INTO wa_ekbe2 WITH KEY ebeln = wa_ekpo-ebeln
                                                 ebelp = wa_ekpo-ebelp BINARY SEARCH. " fatura
      IF ( sy-subrc EQ 0 ).

        wa_saida-qtd    = wa_ekbe2-menge.
        wa_ekpo-menge  = wa_ekpo-menge -  wa_ekbe2-menge_a. "atualiza qtde pedido sem a qtdo faturada periodo anterior

        IF wa_ekpo-elikz = 'X'. "REMESSA FINAL
          wa_saida-qtremfinal = wa_ekpo-menge - wa_ekbe2-menge.
          wa_saida-saldo      = wa_ekpo-menge - wa_ekbe2-menge - wa_saida-qtremfinal.
        ELSE.
          wa_saida-saldo  = wa_ekpo-menge - wa_ekbe2-menge.
        ENDIF.
      ELSE.
        wa_saida-saldo = wa_ekpo-menge.
      ENDIF.

      IF wa_ekpo-menge = 0 AND wa_saida-saldo = 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_ekko-knumv
                                               kposn = wa_ekpo-ebelp
                                               kschl = 'RB00'         "--desconto Absoluto do item
                                               BINARY SEARCH.         "<<<------"160696 - NMS------>>>
      IF ( sy-subrc EQ 0 ).
        wa_saida-netpr_desc  = wa_konv-kwert.
      ENDIF.

      CLEAR: wa_konv.
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_ekko-knumv
                                               kposn = wa_ekpo-ebelp
                                               kschl = 'ZB00'         "--suplemento Absoluto do item
                                               BINARY SEARCH.         "<<<------"160696 - NMS------>>>
      IF ( sy-subrc EQ 0 ).
        wa_saida-netpr_supl  = wa_konv-kwert.
      ENDIF.

      CLEAR: wa_konv.
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_ekko-knumv
                                               kposn = wa_ekpo-ebelp
                                               kschl = 'PBXX'
                                               BINARY SEARCH.         "<<<------"160696 - NMS------>>>

      wa_saida-waers = wa_konv-waers.

      lv_vlr_tot_liq = wa_konv-kwert + wa_saida-netpr_desc + wa_saida-netpr_supl.
**<<<------"160696 - NMS - INI------>>>
*      vfator = 1.
*      UNASSIGN <wmwst>.
*      PERFORM busca_imposto USING wa_ekko-ebeln wa_ekpo-ebelp.
*
*      IF v_meng_ori GT 0.
*        IF wa_ekpo-bprme CS 'TO'.
*          vfator = 1000.
*          lv_vlr_unit_brt = ( ( <wmwst> + lv_vlr_tot_liq ) / v_meng_ori ) * 1000.
*          wa_saida-kbetr = lv_vlr_unit_brt.
*        ELSE.
*          lv_vlr_unit_brt = ( ( <wmwst> + lv_vlr_tot_liq ) / v_meng_ori  ).
*          wa_saida-kbetr = lv_vlr_unit_brt.
*        ENDIF.
*      ENDIF.
*
*      wa_saida-kbetr_imp = <wmwst>.
*      wa_saida-kbetr_tot = ( <wmwst> + lv_vlr_tot_liq ).
*
*      LOOP AT it_ekbe INTO DATA(ls_ekbe) WHERE ebeln = wa_ekpo-ebeln
*                                           AND ebelp = wa_ekpo-ebelp.
      READ TABLE it_ekbe TRANSPORTING NO FIELDS WITH KEY ebeln = wa_ekpo-ebeln
                                                         ebelp = wa_ekpo-ebelp
                                                BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        DATA(vl_tabix2) = sy-tabix.

        LOOP AT it_ekbe INTO DATA(ls_ekbe) FROM vl_tabix2.
          IF ls_ekbe-ebeln NE wa_ekpo-ebeln OR
             ls_ekbe-ebelp NE wa_ekpo-ebelp.
            EXIT.

          ENDIF.
**<<<------"160696 - NMS - FIM------>>>
          DATA(ls_rseg) = VALUE #( it_rseg[ ebeln = ls_ekbe-ebeln
                                            ebelp = ls_ekbe-ebelp
**<<<------"184428 - NMS - INI------>>>
*                                            BELNR = LS_EKBE-BELNR ] OPTIONAL ).
                                            belnr = ls_ekbe-belnr
                                            gjahr = ls_ekbe-gjahr
                                            buzei = ls_ekbe-buzei ] OPTIONAL ).
**<<<------"184428 - NMS - FIM------>>>
          IF ls_ekbe-shkzg = 'S'.
            lv_wrbtr_sum += ls_rseg-wrbtr.

          ELSEIF ls_ekbe-shkzg = 'H'.
            lv_wrbtr_subtract += ls_rseg-wrbtr.

          ENDIF.
        ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
      ENDIF.
**<<<------"160696 - NMS - FIM------>>>
      lv_total_liq_faturado = CONV #( lv_wrbtr_sum - lv_wrbtr_subtract ).

      lv_refkey = |{ ls_rseg-belnr }{ ls_rseg-gjahr }|.
      lv_buzei  = ls_rseg-buzei * 10.

      DATA(ls_lin_rseg) = VALUE #( it_lin_rseg[ refkey = lv_refkey
                                                itmnum  = lv_buzei ] OPTIONAL ).

      IF ls_lin_rseg IS NOT INITIAL.

        lv_imposto_nota_fiscal = REDUCE #( INIT sum TYPE j_1bnfstx-taxval
                                             FOR ls_j_1bnfstx IN it_j_1bnfstx
                                             NEXT sum += ls_j_1bnfstx-taxval ).

        DATA(ls_rbkp_rseg) = VALUE #( it_rbkp_rseg[ belnr = ls_ekbe-belnr
                                                    gjahr = ls_ekbe-gjahr ] OPTIONAL ).

        IF ls_rbkp_rseg-waers = 'USD' .
          lv_imposto_nota_fiscal = CONV #( lv_imposto_nota_fiscal / ls_rbkp_rseg-kursf ).
        ENDIF.
      ENDIF.

      wa_saida-kbetr_fat = CONV #( lv_total_liq_faturado + lv_imposto_nota_fiscal ).
      CLEAR: lv_wrbtr_sum,lv_wrbtr_subtract.
**<<<------"160696 - NMS - INI------>>>
*      IF wa_ekpo-elikz = 'X'. "REMESSA FINAL
*
*        wa_saida-vlremfinal = wa_saida-kbetr_tot - wa_saida-kbetr_fat.
*        wa_saida-kbetr_sal  = wa_saida-kbetr_tot - wa_saida-kbetr_fat - wa_saida-vlremfinal.
*
*      ELSE.
*        wa_saida-kbetr_sal = wa_saida-kbetr_tot - wa_saida-kbetr_fat.
*      ENDIF.
      CLEAR es_parametro.
      es_parametro-ebeln       = wa_ekko-ebeln.
      es_parametro-ebelp       = wa_ekpo-ebelp.
      es_parametro-meng_ori    = v_meng_ori.
      es_parametro-qtde_fat    = wa_saida-qtd.  "*-US192338-06.10.2025-#192338-JT
      es_parametro-qtremfinal  = wa_saida-qtremfinal.  "*-US192338-06.10.2025-#192338-JT
      es_parametro-bprme       = wa_ekpo-bprme.
      es_parametro-kbetr       = wa_saida-kbetr.
      es_parametro-kbetr_imp   = wa_saida-kbetr_imp.
      es_parametro-vlr_tot_liq = lv_vlr_tot_liq.
      es_parametro-kbetr_tot   = wa_saida-kbetr_tot.  "*-US192338-06.10.2025-#192338-JT-inicio
      es_parametro-elikz       = wa_ekpo-elikz.
      es_parametro-kbetr_fat   = wa_saida-kbetr_fat.
      es_parametro-kbetr_sal   = wa_saida-kbetr_sal.
      es_parametro-vlremfinal  = wa_saida-vlremfinal.
* Parâmetros Func. Paral. Busca Imposto
      PERFORM busca_imposto USING es_parametro.
**<<<------"160696 - NMS - FIM------>>>
      wa_saida-unid_p = wa_ekpo-bprme.
      wa_saida-matkl  = wa_ekpo-matkl.
      wa_saida-aedat = wa_ekko-aedat.
      wa_saida-angnr = wa_ekko-angnr.
      wa_saida-ihran = wa_ekko-ihran.
      wa_saida-inco1 = wa_ekko-inco1.
      wa_saida-werks = wa_ekpo-werks.
      wa_saida-ihrez = wa_ekko-ihrez.
**<<<------"184428 - NMS - INI------>>>
* Busca a taxa de Curva.
      PERFORM zf_busca_taxa_curva USING    wa_ekko-ebeln
                                  CHANGING wa_saida-taxa_curva.
**<<<------"184428 - NMS - FIM------>>>
      CASE wa_ekpo-loekz.
        WHEN 'S'.
          CONCATENATE icon_locked ' - Bloqueado' INTO wa_saida-s_pedido SEPARATED BY space.
        WHEN 'L'.
          CONCATENATE icon_delete ' - Eliminado' INTO wa_saida-s_pedido SEPARATED BY space.
        WHEN ''.
          CONCATENATE icon_system_okay ' - Ativo' INTO wa_saida-s_pedido SEPARATED BY space.
      ENDCASE.

      wa_saida-menge  = wa_ekpo-menge.
      wa_saida-name1  = wa_lfa1-name1.
      wa_saida-ebeln  = wa_ekko-ebeln.
      wa_saida-ebelp  = wa_ekpo-ebelp.
      wa_saida-matnr  = wa_ekpo-matnr.
      wa_saida-txz01  = wa_ekpo-txz01.
**<<<------"184428 - NMS - INI------>>>
      vl_object = wa_saida-matnr.
      CALL METHOD cll_mm_util->get_caracteristica_geral
        EXPORTING
          i_class                = cl_class
          i_classtype            = cl_classtype
          i_object               = vl_object
          i_caracteristica       = cl_cultivar
        IMPORTING
          e_valor_caracteristica = DATA(vl_value).

      CONDENSE vl_value NO-GAPS.
      wa_saida-cultivar = CONV #( vl_value ).
**<<<------"184428 - NMS - FIM------>>>
      IF wa_lfa1-stkzn IS NOT INITIAL.
        wa_saida-stcd1  = wa_lfa1-stcd2.
      ELSE.
        wa_saida-stcd1  = wa_lfa1-stcd1.
      ENDIF.

      READ TABLE it_eban INTO DATA(wa_eban) WITH KEY banfn =  ekpo-banfn.
      IF sy-subrc = 0.
*        wa_saida-badat = wa_eban-badat.
      ENDIF.

      "Valor do adiantamento por item.
**<<<------"160696 - NMS - INI------>>>
*      LOOP AT it_zfit0046 INTO DATA(wa_0046) WHERE ebeln = wa_ekpo-ebeln AND ebelp = wa_ekpo-ebelp.
*        ADD  wa_0046-vlr_adiantamento TO wa_saida-adiantamento.
*      ENDLOOP.
*
*      READ TABLE it_zfit0045 INTO DATA(wa_0045) WITH KEY ebeln = wa_ekpo-ebeln.
      READ TABLE it_zfit0046_sum INTO DATA(wa_0046) WITH KEY ebeln = wa_ekpo-ebeln
                                                             ebelp = wa_ekpo-ebelp
                                                    BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        wa_saida-adiantamento = wa_0046-vlr_adiantamento.

      ENDIF.

      READ TABLE it_zfit0045 INTO DATA(wa_0045) WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.
**<<<------"160696 - NMS - FIM------>>>
      IF sy-subrc = 0.
        wa_saida-status = wa_0045-status.
      ENDIF.

      IF wa_ekpo-peinh IS NOT INITIAL.
        wa_saida-netpr_liq = wa_ekpo-netpr / wa_ekpo-peinh.
      ENDIF.

      IF ( wa_ekpo-meins EQ 'BAG' ).
        wa_saida-unid = 'SAC'.
      ELSEIF ( wa_ekpo-meins EQ 'BIG' ).
        wa_saida-unid = 'BAG'.
      ELSE.
        wa_saida-unid = wa_ekpo-meins.
      ENDIF.

      wa_saida-zterm  = wa_ekko-zterm.
      wa_saida-submi  = wa_ekko-submi.
      wa_saida-bsart  = wa_ekko-bsart.
      wa_saida-netpr_germ  = 0.
      wa_saida-netpr_roya  = 0.

      READ TABLE it_0035 INTO wa_0035
        WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.

*-US192338-06.10.2025-#192338-JT-inicio
      IF sy-subrc <> 0.
        READ TABLE it_0035 INTO wa_0035 WITH KEY nro_sol_cp = wa_ekko-nro_sol_cp.
      ENDIF.
*-US192338-06.10.2025-#192338-JT-fim

      IF sy-subrc = 0.
        IF sy-subrc = 0.
          wa_saida-netpr_germ  = wa_0037-netpr_germ.
          wa_saida-netpr_roya  = wa_0037-netpr_roya.
          wa_saida-cultura     = wa_0035-cultura.     "**<<<------"184428 - NMS------>>>
          wa_saida-safra       = wa_0035-safra.       "**<<<------"184428 - NMS------>>>
        ENDIF.
      ENDIF.

      wa_saida-data_criacao  =  wa_0035-data_criacao.

      READ TABLE it_0036 INTO wa_0036 WITH KEY nro_sol_cp = wa_0035-nro_sol_cp BINARY SEARCH.
      IF sy-subrc = 0.
        lv_cont = 0.
        LOOP AT it_0036 INTO wa_0036 FROM sy-tabix.
          IF wa_0036-nro_sol_cp NE wa_0035-nro_sol_cp.
            EXIT.
          ENDIF.
          lv_cont = lv_cont + 1.
          lv_field = 'DT_VENC' && lv_cont.
          CONDENSE lv_field.
          .
          ASSIGN COMPONENT lv_field OF STRUCTURE <fs_wa>
                TO <fs_field>.
          IF sy-subrc = 0.
            <fs_field> = wa_0036-dt_vcto+6(2) && '.' && wa_0036-dt_vcto+4(2) && '.' && wa_0036-dt_vcto(4).
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE it_0036_cont INTO wa_0036_cont INDEX 1.
        IF sy-subrc = 0.
          DO.
            lv_cont = lv_cont + 1.
            IF lv_cont > wa_0036_cont-cont.
              EXIT.
            ELSE.
              lv_field = 'DT_VENC' && lv_cont.
              CONDENSE lv_field.

              ASSIGN COMPONENT lv_field OF STRUCTURE <fs_wa>
                    TO <fs_field>.
              IF sy-subrc = 0.
                <fs_field> = ''.
              ENDIF.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.
**<<<------"182012 - NMS - INI------>>>
*      ASSIGN COMPONENT 'QTD' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> =  wa_ekbe2-menge.
*      ENDIF.
*
*      ASSIGN COMPONENT 'QTREMFINAL' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-qtremfinal.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZSALDO' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-saldo.
*      ENDIF.
*      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-waers.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'MWSKZ' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-mwskz.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'KBETR' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-kbetr.
*      ENDIF.
*      ASSIGN COMPONENT 'KBETR_PED' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-kbetr_ped.
*      ENDIF.
*      ASSIGN COMPONENT 'KBETR_FAT' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-kbetr_fat.
*      ENDIF.
*
*      ASSIGN COMPONENT 'VLREMFINAL' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-vlremfinal.
*      ENDIF.
*
*      ASSIGN COMPONENT 'KBETR_SAL' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-kbetr_sal.
*      ENDIF.
*      ASSIGN COMPONENT 'KBETR_IMP' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-kbetr_imp.
*      ENDIF.
*      ASSIGN COMPONENT 'KBETR_TOT' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-kbetr_tot.
*      ENDIF.
*      ASSIGN COMPONENT 'UNID_P' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-unid_p.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'MATKL' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-matkl.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'ZAEDAT' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-aedat+6(2) && '.' && wa_saida-aedat+4(2) && '.' && wa_saida-aedat(4).
*        CONDENSE <fs_field>.
*      ENDIF.
*
*      ASSIGN COMPONENT 'DATA_CRIACAO' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-data_criacao+6(2) && '.' && wa_saida-data_criacao+4(2) && '.' && wa_saida-data_criacao(4).
*        CONDENSE <fs_field>.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ANGNR' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-angnr.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'IHRAN' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-ihran+6(2) && '.'&& wa_saida-ihran+4(2) && '.'&& wa_saida-ihran(4).
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'INCO1' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-inco1.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-werks.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'IHREZ' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-ihrez.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'S_PEDIDO' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-s_pedido.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-menge.
*      ENDIF.
*      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_lfa1-lifnr.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'NAME1' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-name1.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-ebeln.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-ebelp.
*        CONDENSE <fs_field>.
*        PACK <fs_field> TO <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-matnr.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'TXZ01' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-txz01.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'UNID' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-unid.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'ZTERM' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-zterm.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'SUBMI' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-submi.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'BSART' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-bsart.
*        CONDENSE <fs_field>.
*      ENDIF.
*      ASSIGN COMPONENT 'NETPR_GERM' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-netpr_germ.
*      ENDIF.
*      ASSIGN COMPONENT 'NETPR_ROYA' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-netpr_roya.
*      ENDIF.
*
*      ASSIGN COMPONENT 'LGORT' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-lgort.
*      ENDIF.
*
*      ASSIGN COMPONENT 'VERKF' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-verkf.
*      ENDIF.
*
*      ASSIGN COMPONENT 'TELF1' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-telf1.
*      ENDIF.
*
*      ASSIGN COMPONENT 'BEDAT' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0 AND wa_saida-bedat IS NOT INITIAL.
*        <fs_field> = wa_saida-bedat+6(2) && '.' && wa_saida-bedat+4(2) && '.' && wa_saida-bedat(4).
*        CONDENSE <fs_field>.
*      ENDIF.
*
*      ASSIGN COMPONENT 'STCD1' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-stcd1.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ADIANTAMENTO' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0 AND wa_saida-adiantamento IS NOT INITIAL  .
*        <fs_field> = wa_saida-adiantamento.
*      ENDIF.
*
*      ASSIGN COMPONENT 'STATUS' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-status.
*      ENDIF.
*
*      ASSIGN COMPONENT 'NRO_SOL' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0 AND wa_saida-nro_sol IS NOT INITIAL.
*        <fs_field> = wa_saida-nro_sol.
*      ENDIF.
*
*      ASSIGN COMPONENT 'NETPR_LIQ' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-netpr_liq.
*      ENDIF.
*
*      ASSIGN COMPONENT 'NETPR_DESC' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-netpr_desc.
*      ENDIF.
*
*      ASSIGN COMPONENT 'NETPR_SUPL' OF STRUCTURE <fs_wa>
*            TO <fs_field>.
*      IF sy-subrc = 0.
*        <fs_field> = wa_saida-netpr_supl.
*      ENDIF.
**<<<------"182012 - NMS - FIM------>>>
      APPEND wa_saida TO it_saida.

      CLEAR:
            wa_lfa1,
            wa_ekbe2,
            aux,
            wa_saida,
            wa_0045,
            wa_0046,
            wa_ekpo.

    ENDLOOP.
    CLEAR: wa_ekko, wa_0045, wa_0046.
  ENDLOOP.
**<<<------"160696 - NMS - INI------>>>
* Esperar até que todas as sessões sejam finalizadas. O número é decrementado na subrtoina
* de retorno da função e incrementado antes da RFC ser chamada.
  WAIT UNTIL vg_task_ativa EQ 0.
**<<<------"160696 - NMS - FIM------>>>
ENDFORM.                    " FORM_SAIDA
*-US 156984-07-11-2024-#156984-RJF-Fim
**<<<------"160696 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& FORM ZF_RETUR_PARAL_BUSCA_IMPOSTO
*&---------------------------------------------------------------------*
*& Retorno da Função de Paralelismo.
*&---------------------------------------------------------------------*
*& -->P_TASKNAME Nome (ID) da Tarefa ativa
*&---------------------------------------------------------------------*
FORM zf_retur_paral_busca_imposto USING p_taskname.

  TABLES: zmme_paral_busca_imposto_param.

  FIELD-SYMBOLS: <fs_ebeln>,
                 <fs_ebelp>,
                 <fs_wa_local>.

  RECEIVE RESULTS FROM FUNCTION 'ZLABRFI_RFC_PARALL_SYCH'
     IMPORTING
      ee_param = zmme_paral_busca_imposto_param.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WITH KEY ebeln = zmme_paral_busca_imposto_param-ebeln
                                                                  ebelp = zmme_paral_busca_imposto_param-ebelp.

  IF sy-subrc IS INITIAL.
*   zmme_paral_busca_imposto_param-kbetr_fat  = zmme_paral_busca_imposto_param-kbetr     * zmme_paral_busca_imposto_param-qtde_fat.  "*-US192338-06.10.2025-#192338-JT-inicio
    zmme_paral_busca_imposto_param-kbetr_sal  = zmme_paral_busca_imposto_param-kbetr_tot - zmme_paral_busca_imposto_param-kbetr_fat. "*-US192338-06.10.2025-#192338-JT-inicio
    zmme_paral_busca_imposto_param-vlremfinal = zmme_paral_busca_imposto_param-kbetr     * zmme_paral_busca_imposto_param-qtremfinal."*-US192338-06.10.2025-#192338-JT-inicio
*
    <fs_saida>-kbetr      = zmme_paral_busca_imposto_param-kbetr.
    <fs_saida>-kbetr_imp  = zmme_paral_busca_imposto_param-kbetr_imp.
    <fs_saida>-kbetr_tot  = zmme_paral_busca_imposto_param-kbetr_tot.
    <fs_saida>-kbetr_fat  = zmme_paral_busca_imposto_param-kbetr_fat.
    <fs_saida>-kbetr_sal  = zmme_paral_busca_imposto_param-kbetr_sal.
    <fs_saida>-vlremfinal = zmme_paral_busca_imposto_param-vlremfinal.
  ENDIF.
**<<<------"182012 - NMS - INI------>>>
* Verifica se o processamento não é via SUBMIT.
  IF p_t189 IS INITIAL.
**<<<------"182012 - NMS - FIM------>>>
    ASSIGN <fs_wa> TO <fs_wa_local>.

    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa_local> TO <fs_ebeln>.

    IF sy-subrc IS INITIAL.
      <fs_ebeln> = zmme_paral_busca_imposto_param-ebeln.
      CONDENSE <fs_ebeln>.

    ENDIF.

    ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_wa_local> TO <fs_ebelp>.

    IF sy-subrc IS INITIAL.
      <fs_ebelp> = zmme_paral_busca_imposto_param-ebelp.
      CONDENSE <fs_ebelp>.
      PACK <fs_ebelp> TO <fs_ebelp>.

    ENDIF.

    READ TABLE <fs_table> ASSIGNING <fs_wa_local> WITH KEY ('EBELN') = <fs_ebeln>
                                                           ('EBELP') = <fs_ebelp>.

    IF sy-subrc IS INITIAL.
      <fs_wa_local>-('KBETR')      = zmme_paral_busca_imposto_param-kbetr.
      <fs_wa_local>-('KBETR_IMP')  = zmme_paral_busca_imposto_param-kbetr_imp.
      <fs_wa_local>-('KBETR_TOT')  = zmme_paral_busca_imposto_param-kbetr_tot.
      <fs_wa_local>-('KBETR_FAT')  = zmme_paral_busca_imposto_param-kbetr_fat.
      <fs_wa_local>-('KBETR_SAL')  = zmme_paral_busca_imposto_param-kbetr_sal.
      <fs_wa_local>-('VLREMFINAL') = zmme_paral_busca_imposto_param-vlremfinal.

    ENDIF.

  ENDIF.   "<<<------"182012 - NMS------>>>

  SUBTRACT 1 FROM vg_task_ativa.

ENDFORM.
**<<<------"160696 - NMS - FIM------>>>
*&---------------------------------------------------------------------*
*& Form zf_busca_taxa_curva
*&---------------------------------------------------------------------*
*& Busca a taxa de Curva
*&---------------------------------------------------------------------*
*&      --> UV_NRO_SOL_OV  Numero de Solicitação de Ordem de Venda
*&      <-- CV_TAXA_CURVA  Tx câmbio para lançtos. na contab. financeira
*&---------------------------------------------------------------------*
FORM zf_busca_taxa_curva  USING    uv_nro_sol_ov TYPE zsded013
                          CHANGING cv_taxa_curva TYPE kurrf.

  CONSTANTS: cl_pdi TYPE char03 VALUE 'PDI'.

  SELECT * FROM zsdt0094
    INTO TABLE @DATA(tl_taxa_curva)
  WHERE nro_sol_ov EQ @uv_nro_sol_ov
    AND tipo       EQ @cl_pdi
  ORDER BY data_registro DESCENDING, hora_registro DESCENDING.

  IF sy-subrc IS INITIAL.
    IF line_exists( tl_taxa_curva[ 1 ] ).
      cv_taxa_curva = tl_taxa_curva[ 1 ]-taxa_curva.

    ELSE.
      CLEAR cv_taxa_curva.

    ENDIF.

  ENDIF.

ENDFORM.

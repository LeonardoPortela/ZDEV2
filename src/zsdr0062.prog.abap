************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...:                                                     *
* Objetivo    ...: Liberação de Pedidos de Embarque - INSUMOS          *
* Transação   ...:                                                     *
************************************************************************
REPORT  zsdr0062.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: vbak, vbap, ekko, kna1, lfa1, zsdt0062, zsdt0133, zsdt0082, ekpo.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_ekko,
    " MM - COMPRAS - TABELA DE CABEÇALHO DO DOCUMENTO DE COMPRAS
    ebeln TYPE ekko-ebeln, " Nº do documento de compras
    bsart TYPE ekko-bsart, " Tipo de documento de compras
    lifnr TYPE ekko-lifnr, " Nº conta do fornecedor
    bukrs TYPE ekko-bukrs, " Empresa
    aedat TYPE ekko-aedat, " Data de criação do registro
    waers TYPE ekko-waers,
    unsez TYPE ekko-unsez, " Safra
    submi TYPE ekko-submi,
  END OF ty_ekko,

  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln, " Nº do documento de compras
    ebelp TYPE ekpo-ebelp, " Nº item do documento de compra
    matnr TYPE ekpo-matnr, " Nº do material
    txz01 TYPE ekpo-txz01, " Texto breve
    menge TYPE ekpo-menge, " Quantidade do pedido
    netwr TYPE ekpo-netwr, " Valor líquido do pedido em moeda de pedido
    loekz TYPE ekpo-loekz, " Código de eliminação no documento de compras
    meins TYPE ekpo-meins, " Unidade de Medida
    netpr TYPE ekpo-netpr,
    werks TYPE ekpo-werks,
  END OF ty_ekpo,

  BEGIN OF ty_vbak,
    " SD - VENDAS - DOCUMENTO DE VENDAS: DADOS DO CABEÇALHO
    vbeln TYPE vbak-vbeln, " Documento de vendas
    vkorg TYPE vbak-vkorg, " Organização de vendas
    vtweg TYPE vbak-vtweg, " Canal de distribuição
    spart TYPE vbak-spart, " Setor de atividade
    auart TYPE vbak-auart, " Tipo de documento de vendas
    vkbur TYPE vbak-vkbur, " Escritório de vendas
    kunnr TYPE vbak-kunnr, " Emissor da ordem
    waerk TYPE vbak-waerk, " Moeda do documento SD
    erdat TYPE vbak-erdat, " Data de criação do registro
    faksk TYPE vbak-faksk, " Bloqueio tipos de doc.faturamento - documento SD
    lifsk TYPE vbak-lifsk, " Bloqueio tipos de doc.faturamento - documento SD
    audat TYPE vbak-audat, " Dada do documento (data de entrada / saída)
    knumv TYPE vbak-knumv,
  END OF ty_vbak,

  BEGIN OF ty_vbap,
    " SD - VENDAS - Documento de vendas: dados de item
    vbeln  TYPE vbap-vbeln,
    matnr  TYPE vbap-matnr, " Nº do material
    arktx  TYPE vbap-arktx, " Texto breve do item da ordem do cliente
    werks  TYPE vbap-werks, " Centro (próprio ou externo)
    zmeng  TYPE vbap-zmeng, " Qtd.prevista em UMV
    netwr  TYPE vbap-netwr, " Valor líquido do item da ordem na moeda do documento
    kwmeng TYPE vbap-kwmeng, " Quantidade da ordem acumulada em unidade de venda
    vrkme  TYPE vbap-vrkme, " Unidade de venda
    posnr  TYPE vbap-posnr, " Item
    netpr  TYPE vbap-netpr, " Preço líquido
    lgort  TYPE vbap-lgort,
    charg  TYPE vbap-charg,
  END OF ty_vbap,

  BEGIN OF ty_saida,

    matnr             TYPE vbap-matnr,
    maktx             TYPE makt-maktx,

    " Compra
    ebeln_c           TYPE ekko-ebeln,
    bsart_c           TYPE ekko-bsart,
    bukrs_c           TYPE ekko-bukrs,
    aedat_c           TYPE ekko-aedat,
    waers_c           TYPE ekko-waers,
    vgabe_c           TYPE ekbe-vgabe,
    menge_c           TYPE ekbe-menge,
    shkzg_c           TYPE ekbe-shkzg,
    belnr_c           TYPE ekbe-belnr,
    budat_c           TYPE ekbe-budat,
    dmbtr_c           TYPE ekbe-dmbtr,
    matnr_c           TYPE ekbe-matnr,
    ebelp_c           TYPE ekpo-ebelp,
    txz01_c           TYPE ekpo-txz01,
    netwr_c           TYPE ekpo-netwr,
    loekz_c           TYPE ekpo-loekz,
    meins_c           TYPE ekpo-meins,
    netpr_c           TYPE ekpo-netpr,
    gjahr_c           TYPE rbkp-gjahr,
    xblnr_c           TYPE rbkp-xblnr,
    lifnr_c           TYPE lfa1-lifnr,
    name1_c           TYPE lfa1-name1,


    " Venda
    vbeln_v           TYPE vbak-vbeln,
    vkorg_v           TYPE vbak-vkorg,
    vtweg_v           TYPE vbak-vtweg,
    spart_v           TYPE vbak-spart,
    auart_v           TYPE vbak-auart,
    vkbur_v           TYPE vbak-vkbur,
    waerk_v           TYPE vbak-waerk,
    erdat_v           TYPE vbak-erdat,
    faksk_v           TYPE vbak-faksk,
    lifsk_v           TYPE vbak-lifsk,
    audat_v           TYPE vbak-audat,
    arktx_v           TYPE vbap-arktx,
    werks_v           TYPE vbap-werks,
    zmeng_v           TYPE vbap-zmeng,
    netwr_v           TYPE vbap-netwr,
    kwmeng_v          TYPE vbap-kwmeng,
    vrkme_v           TYPE vbap-vrkme,
    posnr_v           TYPE vbap-posnr,
    rfmng_v           TYPE vbfa-rfmng,
    vbtyp_n_v         TYPE vbfa-vbtyp_n,
    vbelv_v           TYPE vbfa-vbelv,
    vbtyp_v_v         TYPE vbfa-vbtyp_v,
    meins_v           TYPE vbfa-meins,
    kunnr_v           TYPE kna1-kunnr,
    name1_v           TYPE kna1-name1,
    kbetr_v           TYPE konv-kbetr,
    lifsp_v           TYPE vbep-lifsp,

    " Produzido
    sptag_p           TYPE s225-sptag,  "#EC CI_USAGE_OK[2268063]
    werks_p           TYPE s225-werks,  "#EC CI_USAGE_OK[2268063]
    matnr_p           TYPE s225-matnr,  "#EC CI_USAGE_OK[2268063]
    "wemng_p        type s225-wemng,
    wemng_p           TYPE ekpo-menge,

    total_venda       TYPE db20199vp,
    total_compra      TYPE db20199vp,
    "total_produzido_p type s225-wemng,
    total_produzido_p TYPE db20199vp,
    saldo             TYPE db20199vp,
    total_f_c         TYPE db20199vp,
    saldo_c           TYPE db20199vp,
    total_f_v         TYPE db20199vp,
    saldo_v           TYPE db20199vp,
    categoria         TYPE c,

    cellcolors        TYPE lvc_t_scol,
    qtd_vinc          TYPE zsdt0062-qtd_vinc,
  END OF ty_saida,

  BEGIN OF ty_saida_mm,
    matnr       TYPE  matnr,
    ebeln       TYPE  ekpo-ebeln,
    menge       TYPE  db20199vp,
    netwr       TYPE  ekpo-netwr,
    lifnr       TYPE  lfa1-lifnr,
    name1       TYPE  lfa1-name1,
    waers       TYPE  ekko-waers,
    meins       TYPE  ekpo-meins,
    ebelp       TYPE  ekpo-ebelp,
    txz01       TYPE  ekpo-txz01,
    maktx       TYPE  makt-maktx,
    netpr       TYPE ekpo-netpr,
    vincular_c  TYPE zfit0034-status,
    qtd_vinc_mm TYPE db20199vp,
    saldo_mm    TYPE db20199vp,
    saldo_entr  TYPE db20199vp,
    werks       TYPE vbap-werks,
    bsart       TYPE ekko-bsart,
    submi       TYPE ekko-submi,
  END OF ty_saida_mm,

  BEGIN OF ty_saida_sd,

    vincular_v  TYPE zfit0034-status,
    kunnr       TYPE kna1-kunnr,
    name1       TYPE kna1-name1,
    vbeln       TYPE vbak-vbeln,
    zmeng       TYPE vbap-zmeng,
    vrkme       TYPE vbap-vrkme,

    status,
    zterm(30),
    netwr       TYPE vbak-netwr,
    waerk       TYPE vbak-waerk,
    kwmeng      TYPE db20199vp,
    lifsk       TYPE vbak-lifsk,
    posnr       TYPE vbap-posnr,
    matnr       TYPE vbap-matnr,
    arktx       TYPE vbap-arktx,
    vkbur       TYPE vbak-vkbur,
    netpr       TYPE vbap-netpr,
    kbetr       TYPE konv-kbetr,
    qtd_vinc_sd TYPE db20199vp,
    saldo_sd    TYPE db20199vp,
    werks       TYPE vbap-werks,
    lgort       TYPE vbap-lgort,
    charg       TYPE vbap-charg,
    nro_sol     TYPE zsdt0131-nro_sol,
    seq         TYPE zsdt0131-seq,
  END OF ty_saida_sd,


  BEGIN OF ty_saida_vinc,
    ebeln      TYPE ekpo-ebeln, "Número do Pedido
    ebelp      TYPE ekpo-ebelp, "Item do Pedido
    lifnr      TYPE lfa1-lifnr, "Código do Fornecedor
    name       TYPE lfa1-name1, "Descrição do Fornecedor
    kunnr      TYPE kna1-kunnr, "Código do Cliente
    name1      TYPE kna1-name1, "Descrição do Cliente
    vkbur      TYPE vbak-vkbur, "Escritorio de Vendas
    vbeln      TYPE vbak-vbeln, "Ordem de Venda
    posnr      TYPE vbap-posnr, "Item de Venda
    matnr      TYPE vbap-matnr, "Material
    matnr_c    TYPE zsdt0062-ematn, "Material de compras
    charg_orig TYPE mchb-charg, "Lote de origem
    lgort_orig TYPE mchb-lgort, "Deposito de origem
    mblnr      TYPE zsdt0062-mblnr,  "Documento de transferência
    mjahr      TYPE zsdt0062-mjahr,
    qtd_v(15)  TYPE p DECIMALS 2 , "Quantidade Vinculada
    data       TYPE sy-datum,  "Data da Vinculação
    local_emb  TYPE char25, "Local de Embarque
    status     TYPE char4, "Status da Operação
    status_aux TYPE c,
    sol_vinc   TYPE zsdt0062-sol_vinc,
    nr_forn    TYPE zsdt0062-nr_forn,
    nro_sol    TYPE zsdt0131-nro_sol,
    seq        TYPE zsdt0131-seq,
  END OF ty_saida_vinc,

  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END OF ty_lfa1,

  BEGIN OF ty_vbfa,
    rfmng   TYPE vbfa-rfmng,
    vbeln   TYPE vbfa-vbeln,
    vbtyp_n TYPE vbfa-vbtyp_n,
    vbelv   TYPE vbfa-vbelv,
    vbtyp_v TYPE vbfa-vbtyp_v,
    meins   TYPE vbfa-meins,
    erdat   TYPE vbfa-erdat,
    matnr   TYPE vbfa-matnr,
    posnv   TYPE vbfa-posnv,
  END OF ty_vbfa,


  BEGIN OF ty_vbfa_aux,
    rfmng TYPE vbfa-rfmng,
    vbeln TYPE vbfa-vbeln,
    vbelv TYPE vbfa-vbelv,
  END OF ty_vbfa_aux,

  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    vgabe TYPE ekbe-vgabe,
    menge TYPE ekbe-menge,
    shkzg TYPE ekbe-shkzg,
    belnr TYPE ekbe-belnr,
    budat TYPE ekbe-budat,
    dmbtr TYPE ekbe-dmbtr,
    ebelp TYPE ekbe-ebelp,
    matnr TYPE ekbe-matnr,
    gjahr TYPE ekbe-gjahr,
    bewtp TYPE ekbe-bewtp,
  END OF ty_ekbe,

  BEGIN OF ty_ekbe2,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
    zekkn TYPE ekbe-zekkn,
    vgabe TYPE ekbe-vgabe,
    gjahr TYPE ekbe-gjahr,
    belnr TYPE ekbe-belnr,
    buzei TYPE ekbe-buzei,
    menge TYPE ekbe-menge,
  END   OF ty_ekbe2,

  BEGIN OF ty_s225,
    sptag           TYPE s225-sptag,  "#EC CI_USAGE_OK[2268063]
    werks           TYPE s225-werks,  "#EC CI_USAGE_OK[2268063]
    matnr           TYPE s225-matnr,  "#EC CI_USAGE_OK[2268063]
    wemng           TYPE s225-wemng,  "#EC CI_USAGE_OK[2268063]
    amein           TYPE s225-amein,  "#EC CI_USAGE_OK[2268063]
    menge           TYPE ekpo-menge,
    total_produzido TYPE s225-wemng,  "#EC CI_USAGE_OK[2268063]

  END OF ty_s225,

  BEGIN OF ty_konv,
    knumv TYPE konv-knumv,
    kposn TYPE konv-kposn,
    kschl TYPE konv-kschl,
    kbetr TYPE konv-kbetr,
  END OF ty_konv,

  BEGIN OF ty_rbkp,
    belnr TYPE rbkp-belnr,
    gjahr TYPE rbkp-gjahr,
    xblnr TYPE rbkp-xblnr,
    stblg TYPE rbkp-stblg,
  END OF ty_rbkp,

  BEGIN OF ty_mara,
    matkl TYPE mara-matkl,
    matnr TYPE mara-matnr,
  END OF ty_mara,

  BEGIN OF ty_setleaf,
    valfrom_aux TYPE mara-matkl,
  END OF ty_setleaf,

  BEGIN OF ty_makt,
    mandt TYPE makt-mandt,
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
    maktg TYPE makt-maktg,
  END OF ty_makt,


  BEGIN OF ty_faturado,
    ebeln          TYPE ekko-ebeln,
    valor_faturado TYPE ekbe-menge,
    matnr          TYPE ekbe-matnr,
    valor_total    TYPE ekko-ebeln,
  END OF ty_faturado,

  BEGIN OF ty_faturado_venda,
    vbelv          TYPE vbfa-vbelv,
    valor_faturado TYPE ekbe-menge,
    matnr          TYPE vbfa-matnr,
    valor_total    TYPE vbfa-rfmng,
  END OF ty_faturado_venda,


  BEGIN OF ty_j_1bbranch,
    bukrs  TYPE j_1bbranch-bukrs,
    branch TYPE j_1bbranch-branch,
  END OF ty_j_1bbranch,

  BEGIN OF ty_vbep,
    vbeln TYPE vbep-vbeln,
    posnr TYPE vbep-posnr,
    etenr TYPE vbep-etenr,
    lifsp TYPE vbep-lifsp,
  END OF ty_vbep,

  BEGIN OF ty_zib_bsik,
    bukrs TYPE bsik-bukrs,
    belnr TYPE bsik-belnr,
    gjahr TYPE bsik-gjahr,
  END OF ty_zib_bsik,


  BEGIN OF ty_compra,
    ebeln           TYPE ekko-ebeln,
    bsart           TYPE ekko-bsart,
    bukrs           TYPE ekko-bukrs,
    aedat           TYPE ekko-aedat,
    waers           TYPE ekko-waers,
    vgabe           TYPE ekbe-vgabe,
    menge           TYPE ekbe-menge,
    menge_f         TYPE ekbe-menge,
    shkzg           TYPE ekbe-shkzg,
    belnr           TYPE ekbe-belnr,
    budat           TYPE ekbe-budat,
    dmbtr           TYPE ekbe-dmbtr,
    matnr           TYPE ekbe-matnr,
    ebelp           TYPE ekpo-ebelp,
    txz01           TYPE ekpo-txz01,
    netwr           TYPE ekpo-netwr,
    loekz           TYPE ekpo-loekz,
    meins           TYPE ekpo-meins,
    netpr           TYPE ekpo-netpr,
    gjahr           TYPE rbkp-gjahr,
    xblnr           TYPE rbkp-xblnr,
    lifnr           TYPE lfa1-lifnr,
    name1           TYPE lfa1-name1,
    maktx           TYPE makt-maktx,

    total_compra    TYPE ekbe-menge,
    total_produzido TYPE ekbe-menge,
    werks           TYPE ekpo-werks,
    submi           TYPE ekko-submi,
  END OF ty_compra,

  BEGIN OF ty_venda,
    vbeln           TYPE vbak-vbeln,
    vkorg           TYPE vbak-vkorg,
    nro_sol         TYPE zsdt0131-nro_sol,
    seq             TYPE zsdt0131-seq,
    vtweg           TYPE vbak-vtweg,
    spart           TYPE vbak-spart,
    auart           TYPE vbak-auart,
    vkbur           TYPE vbak-vkbur,
    waerk           TYPE vbak-waerk,
    erdat           TYPE vbak-erdat,
    faksk           TYPE vbak-faksk,
    lifsk           TYPE vbak-lifsk,
    audat           TYPE vbak-audat,
    matnr           TYPE vbap-matnr,
    arktx           TYPE vbap-arktx,
    werks           TYPE vbap-werks,
    zmeng           TYPE vbap-zmeng,
    netwr           TYPE vbap-netwr,
    kwmeng          TYPE vbap-kwmeng,
    vrkme           TYPE vbap-vrkme,
    posnr           TYPE vbap-posnr,
    netpr           TYPE vbap-netpr,
    rfmng           TYPE vbfa-rfmng,
    vbtyp_n         TYPE vbfa-vbtyp_n,
    vbelv           TYPE vbfa-vbelv,
    vbtyp_v         TYPE vbfa-vbtyp_v,
    meins           TYPE vbfa-meins,
    kunnr           TYPE kna1-kunnr,
    lgort           TYPE vbap-lgort,
    charg           TYPE vbap-charg,
    name1           TYPE kna1-name1,
    maktx           TYPE makt-maktx,
    kbetr           TYPE konv-kbetr,
    lifsp           TYPE vbep-lifsp,
    total_venda     TYPE vbap-zmeng,
    total_produzido TYPE vbap-zmeng,
    qtd_vinc        TYPE zsdt0062-qtd_vinc,
  END OF ty_venda,

  BEGIN OF ty_zsdt0041,
    vbeln         TYPE zsdt0041-vbeln,
    doc_simulacao TYPE zsdt0041-doc_simulacao,
  END OF ty_zsdt0041,

  BEGIN OF ty_zsdt0040,
    doc_simulacao TYPE zsdt0040-doc_simulacao,
    safra         TYPE zsdt0040-safra,
  END OF ty_zsdt0040,

  BEGIN OF ty_zsdt0062,
    sol_vinc       TYPE zsdt0062-sol_vinc,
    ebeln          TYPE zsdt0062-ebeln,
    ebelp          TYPE zsdt0062-ebelp,
    vbeln          TYPE zsdt0062-vbeln,
    posnr          TYPE zsdt0062-posnr,
    dt_vinc        TYPE zsdt0062-dt_vinc,
    matnr          TYPE zsdt0062-matnr,
    qtd_vinc       TYPE zsdt0062-qtd_vinc,
    local_embarque TYPE zsdt0062-local_embarque,
    usnam          TYPE zsdt0062-usnam,
    dt_atual       TYPE zsdt0062-dt_atual,
    hora_atul      TYPE zsdt0062-hora_atul,
    envio_email    TYPE zsdt0062-envio_email,
    lifnr          TYPE zsdt0062-lifnr,
    kunnr          TYPE zsdt0062-kunnr,
    nr_forn        TYPE zsdt0062-nr_forn,
    mblnr          TYPE zsdt0062-mblnr,
    mjahr          TYPE zsdt0062-mjahr,
    lgort          TYPE zsdt0062-lgort,
    charg          TYPE zsdt0062-charg,
  END OF ty_zsdt0062.


"--------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:
  it_saida                    TYPE TABLE OF ty_saida WITH HEADER LINE,    "
  it_saida_aux                TYPE TABLE OF ty_saida WITH HEADER LINE,    "
  it_saida_soma               TYPE TABLE OF ty_saida,    "


  it_saida_mm                 TYPE TABLE OF ty_saida_mm, "
  it_saida_sd                 TYPE TABLE OF ty_saida_sd, "
  it_saida_vinc               TYPE TABLE OF ty_saida_vinc,
  it_saida_vinculados         TYPE TABLE OF ty_saida_vinc,
  it_saida_vinculados_nr_forn TYPE STANDARD TABLE OF ty_saida_vinc,
  it_saida_vinculados_qtde    TYPE STANDARD TABLE OF ty_saida_vinc,
  t_bdc                       TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  t_messtab                   TYPE TABLE OF bdcmsgcoll,

  it_ekko                     TYPE TABLE OF ty_ekko, "
  it_ekko_forn                TYPE TABLE OF ty_ekko, "
  it_ekko_aux                 TYPE TABLE OF ty_ekko, "

  it_ekpo                     TYPE TABLE OF ty_ekpo, "
  it_ekpo_aux                 TYPE TABLE OF ty_ekpo, "

  it_vbak                     TYPE TABLE OF ty_vbak, "
  it_vbak_aux                 TYPE TABLE OF ty_vbak, "
  it_vbap                     TYPE TABLE OF ty_vbap, "
  it_0026                     TYPE TABLE OF zfit0026 WITH HEADER LINE, "
  it_zib                      TYPE TABLE OF zib_contabil_chv WITH HEADER LINE, "
  it_bsik                     TYPE TABLE OF bsik WITH HEADER LINE, "
  it_vbkd                     TYPE TABLE OF vbkd WITH HEADER LINE, "
  it_t052u                    TYPE TABLE OF t052u WITH HEADER LINE, "
  it_zib_bsik                 TYPE TABLE OF ty_zib_bsik WITH HEADER LINE, "
  it_kna1                     TYPE TABLE OF ty_kna1, "
  it_lfa1                     TYPE TABLE OF ty_lfa1, "
  it_vbep                     TYPE TABLE OF ty_vbep,

  it_s225                     TYPE TABLE OF ty_s225, "
  it_s225_aux                 TYPE TABLE OF ty_s225, "
  it_s225_soma                TYPE TABLE OF ty_s225, "

  it_konv                     TYPE TABLE OF ty_konv,

  it_makt                     TYPE TABLE OF ty_makt,
  it_rbkp                     TYPE TABLE OF ty_rbkp,

  it_compra                   TYPE TABLE OF ty_compra,
  it_compra_group             TYPE TABLE OF ty_compra,
  it_compra_aux               TYPE TABLE OF ty_compra,
  it_compra_soma              TYPE TABLE OF ty_compra,
  it_compra_faturado          TYPE TABLE OF ty_compra,

  it_venda                    TYPE TABLE OF ty_venda,
  it_venda_aux                TYPE TABLE OF ty_venda,
  it_venda_soma               TYPE TABLE OF ty_venda,


  it_mara                     TYPE TABLE OF ty_mara, "
  it_mara_venda               TYPE TABLE OF ty_mara, "
  it_faturado                 TYPE TABLE OF ty_faturado,
  it_faturado_aux             TYPE TABLE OF ty_faturado,
  it_faturado_soma            TYPE TABLE OF ty_faturado,


  it_faturado_venda           TYPE TABLE OF ty_faturado_venda,
  it_faturado_venda_aux       TYPE TABLE OF ty_faturado_venda,
  it_faturado_venda_soma      TYPE TABLE OF ty_faturado_venda,


  it_j_1bbranch               TYPE TABLE OF ty_j_1bbranch,

  it_vbfa                     TYPE TABLE OF ty_vbfa,     "
  it_vbfa_soma                TYPE TABLE OF ty_vbfa,     "
  it_vbfa_aux                 TYPE TABLE OF ty_vbfa, "
  it_setleaf_aux              LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  it_setleaf                  TYPE TABLE OF ty_setleaf,


  it_ekbe                     TYPE TABLE OF ty_ekbe, "
  it_ekbe_aux                 TYPE TABLE OF ty_ekbe, "
  it_ekbe2                    TYPE TABLE OF ty_ekbe2, "
  wa_ekbe2                    TYPE ty_ekbe2, "

  it_zsdt0041                 TYPE TABLE OF ty_zsdt0041,
  it_zsdt0040                 TYPE TABLE OF ty_zsdt0040,
  it_zsdt0062                 TYPE TABLE OF ty_zsdt0062,
  it_zsdt0074                 TYPE TABLE OF zsdt0074,
  it_zsdt0082                 TYPE STANDARD TABLE OF zsdt0082,
  it_zsdt0131                 TYPE STANDARD TABLE OF zsdt0131,
  it_zsdt0062_soma            TYPE STANDARD TABLE OF zsdt0062.



*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:

  "Tela 0100
  wa_alv_0200            TYPE REF TO cl_gui_alv_grid,
  wa_container_0200      TYPE REF TO cl_gui_custom_container,


  wa_alv_compras         TYPE REF TO cl_gui_alv_grid,
  wa_alv_vendas          TYPE REF TO cl_gui_alv_grid,
  wa_alv_vinc            TYPE REF TO cl_gui_alv_grid,
  wa_alv_vinculados      TYPE REF TO cl_gui_alv_grid,
  wa_stable              TYPE lvc_s_stbl,


  wa_cont_compras        TYPE REF TO cl_gui_custom_container,
  wa_cont_vendas         TYPE REF TO cl_gui_custom_container,
  wa_cont_vinc           TYPE REF TO cl_gui_custom_container,
  wa_cont_vinculados     TYPE REF TO cl_gui_custom_container,

  wa_layout              TYPE lvc_s_layo,


  wa_layout_vinc         TYPE lvc_s_layo,

  wa_saida               TYPE ty_saida,
  wa_saida_soma          TYPE ty_saida,
  wa_saida_aux           TYPE ty_saida,

  wa_saida_mm            TYPE ty_saida_mm,
  wa_saida_sd            TYPE ty_saida_sd,
  wa_saida_vinc          TYPE ty_saida_vinc,
  wa_saida_vinculados    TYPE ty_saida_vinc,

  wa_ekko                TYPE ty_ekko, "
  wa_ekko_forn           TYPE ty_ekko, "
  wa_ekko_aux            TYPE ty_ekko, "

  wa_ekpo                TYPE ty_ekpo, "
  wa_ekpo_aux            TYPE ty_ekpo, "

  wa_vbak                TYPE ty_vbak, "
  wa_vbak_aux            TYPE ty_vbak, "
  wa_vbap                TYPE ty_vbap, "
  wa_kna1                TYPE ty_kna1, "
  wa_lfa1                TYPE ty_lfa1, "

  wa_s225                TYPE ty_s225, "
  wa_s225_aux            TYPE ty_s225, "
  wa_s225_soma           TYPE ty_s225, "


  wa_konv                TYPE ty_konv,
  wa_vbep                TYPE ty_vbep,

  wa_makt                TYPE ty_makt,
  wa_rbkp                TYPE ty_rbkp,
  wa_faturado            TYPE ty_faturado,
  wa_faturado_aux        TYPE ty_faturado,
  wa_faturado_soma       TYPE ty_faturado,

  wa_faturado_venda      TYPE ty_faturado_venda,
  wa_faturado_venda_aux  TYPE ty_faturado_venda,
  wa_faturado_venda_soma TYPE ty_faturado_venda,


  wa_j_1bbranch          TYPE ty_j_1bbranch,

  wa_compra              TYPE ty_compra,
  wa_compra_group        TYPE ty_compra,
  wa_compra_aux          TYPE ty_compra,
  wa_compra_soma         TYPE ty_compra,
  wa_compra_faturado     TYPE ty_compra,

  wa_venda               TYPE ty_venda,
  wa_venda_aux           TYPE ty_venda,
  wa_venda_soma          TYPE ty_venda,

  wa_setleaf_aux         TYPE setleaf,
  wa_setleaf             TYPE ty_setleaf,
  wa_mara                TYPE ty_mara,

  wa_vbfa                TYPE ty_vbfa, " Documento de vendas: dados de item
  wa_vbfa_soma           TYPE ty_vbfa, " Documento de vendas: dados de item
  wa_vbfa_aux            TYPE ty_vbfa, " Documento de vendas: dados de item

  wa_ekbe                TYPE ty_ekbe, " Documento de vendas: dados de item
  wa_ekbe_aux            TYPE ty_ekbe, " Documento de vendas: dados de item

  wa_zsdt0041            TYPE ty_zsdt0041,
  wa_zsdt0040            TYPE ty_zsdt0040,
  wa_zsdt0062            TYPE ty_zsdt0062,
  wa_zsdt0074            TYPE zsdt0074.

DATA: valor_conversion_faturado TYPE ekpo-menge,
      valor                     TYPE ekpo-menge,
      vg_nr_forn                TYPE char10,
      vg_qt_vinc                TYPE zsdt0062-qtd_vinc,
      l_lines_qtvinc            TYPE i,
      ckc_pedido                TYPE char01 VALUE 'X',
      l_saldo_entr              TYPE db20199vp.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: it_fcat            TYPE TABLE OF lvc_s_fcat,
      it_fcat_compras    TYPE TABLE OF lvc_s_fcat,
      it_fcat_vendas     TYPE TABLE OF lvc_s_fcat,
      it_fcat_vinc       TYPE TABLE OF lvc_s_fcat,
      it_fcat_vinculados TYPE TABLE OF lvc_s_fcat,
      r_ekpo_s           TYPE RANGE OF ekpo-loekz,
      wa_status          LIKE LINE OF  r_ekpo_s,
      gs_variant_c       TYPE disvariant.

*----------------------------------------------------------------------*
* Estrutura para as Vinculações
*----------------------------------------------------------------------*

"Icones
DATA: icon_vinculacao TYPE c LENGTH 4,
      icon_lifnr      TYPE c LENGTH 4,
      icon_matnr      TYPE c LENGTH 4,
      icon_compras    TYPE c LENGTH 4,
      icon_vendas     TYPE c LENGTH 4,
      icon_total_c    TYPE c LENGTH 4,
      icon_total_v    TYPE c LENGTH 4,
      icon_email      TYPE c LENGTH 4,
      icon_assunto    TYPE c LENGTH 4.


" Campos de Informações
DATA: wa_lifnr TYPE ekko-lifnr,
      wa_name1 TYPE lfa1-name1,
      wa_matnr TYPE mara-matnr,
      wa_maktx TYPE makt-maktx,
      wa_meins TYPE mara-meins.


" Campos de Vinculação
DATA: wa_data            TYPE sy-datum,
      wa_local           TYPE c LENGTH 20,
      wa_saldo_c         TYPE db20199vp,
      wa_saldo_v         TYPE db20199vp,
      wa_saldo_e         TYPE db20199vp,
      wa_deposito_origem TYPE mchb-lgort,
      wa_lote_origem     TYPE mchb-charg,
      wa_qtd_vinc        TYPE db20199vp.


"Campos Compras
DATA: wa_qtd_venda  TYPE db20199vp,
      wa_qtd_vinc_c TYPE db20199vp,
      wa_saldo_t_c  TYPE db20199vp.


"Campos Vendas
DATA: wa_qtd_compra TYPE db20199vp,
      wa_qtd_vinc_v TYPE db20199vp,
      wa_saldo_t_v  TYPE db20199vp.

"Campos do E-mail
DATA: wa_email   TYPE c LENGTH 500,
      wa_assunto TYPE c LENGTH 300.


"Variaveis Vinculacao
DATA: wa_edit_vinc TYPE c,
      ok_code      LIKE sy-ucomm,
      wa_opcao     TYPE c.

"ALV Editor Texto E-mail - Vinculação
DATA: cl_editor        TYPE REF TO cl_gui_textedit,
      container_editor TYPE REF TO cl_gui_custom_container,
      tl_rows          TYPE lvc_t_row,
      sl_rows          TYPE lvc_s_row,
      tl_rows_aux      TYPE lvc_t_row,
      sl_rows_aux      TYPE lvc_s_row,
      lines            TYPE sy-tabix,
      lt_tab           TYPE STANDARD TABLE OF char0241,
      la_tab           LIKE LINE OF lt_tab.

DATA: vinculados_email TYPE c.

DATA: gv_screen1 TYPE n LENGTH 4.
DATA: gv_screen2 TYPE n LENGTH 4.
DATA: gv_screen3 TYPE n LENGTH 4.
DATA: gv_screen4 TYPE n LENGTH 4.

CLASS cl_gui_cfw DEFINITION LOAD.


*---------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK bloco1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_nrocg FOR zsdt0133-nro_cg NO INTERVALS NO-EXTENSION, " Carga
                  p_nrosl FOR zsdt0082-nro_sol NO INTERVALS NO-EXTENSION, "Nr. Solicitação
                  p_seqsl FOR zsdt0082-seq NO INTERVALS NO-EXTENSION, "Seq Solicitação
                  p_bsart   FOR ekko-bsart NO INTERVALS OBLIGATORY," Tipo de Pedido
                  "P_AUART   FOR VBAK-AUART NO INTERVALS OBLIGATORY, " Tipo de Ordem
                  "P_VKORG   FOR VBAK-VKORG NO-EXTENSION NO INTERVALS OBLIGATORY," Organização de Vendas
                  "P_VTWEG   FOR VBAK-VTWEG NO-EXTENSION NO INTERVALS OBLIGATORY," Canal de Distribuição
                  p_spart   FOR vbak-spart NO INTERVALS OBLIGATORY," Setor de Atividade
                  p_safra   FOR ekko-unsez NO INTERVALS, " Safra
                  "P_VKBUR   FOR VBAK-VKBUR NO INTERVALS, " Escritório de Vendas
                  p_erdat   FOR vbak-erdat             , " Data do Contrato
                  p_fatur   FOR vbak-erdat             , " Data do Faturamento
                  p_werks   FOR vbap-werks NO INTERVALS, " Centro
                  p_matnr   FOR vbap-matnr NO INTERVALS,
                  p_matnrp  FOR ekpo-matnr NO INTERVALS,
                  p_ebeln   FOR ekpo-ebeln NO INTERVALS, " Material
                  p_bukrs   FOR ekko-bukrs NO INTERVALS. " Empresa

  "//Indica que o material do pedido que está sendo passado
  "//é diferente do material da solicitação;
  PARAMETERS chk_mat TYPE abap_bool NO-DISPLAY.

SELECTION-SCREEN: END OF BLOCK bloco1.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: f_seleciona_dados. " Form seleciona dados


  CONTROLS tabstrip TYPE TABSTRIP.
  CALL SCREEN 0100.

END-OF-SELECTION.


CLASS lcl_screen_fields DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_fields,
        "FIELD TYPE CHAR132,
        group TYPE char3,
        value TYPE num1,
      END OF ty_fields,

      ty_t_fields TYPE TABLE OF ty_fields WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_status,
        active    TYPE num1 VALUE 1,
        inactive  TYPE num1 VALUE 0,
        invisible TYPE num1 VALUE 1,
        visible   TYPE num1 VALUE 0,
      END OF c_status.

    CLASS-DATA fields TYPE TABLE OF ty_fields.

    "//It can be implemented;
*    CLASS-METHODS SET_FIELD_STATUS
*      IMPORTING
*        FIELD TYPE TY_FIELDS-FIELD
*        VALUE TYPE TY_FIELDS-VALUE DEFAULT LCL_SCREEN_FIELDS=>C_STATUS-INATIVO.

    CLASS-METHODS set_group_status
      IMPORTING
        group TYPE ty_fields-group
        value TYPE ty_fields-value.

    CLASS-METHODS is_active
      IMPORTING
        group        TYPE ty_fields-group
      RETURNING
        VALUE(value) TYPE abap_bool.

    CLASS-METHODS get_screen_fields
      RETURNING VALUE(fields) TYPE ty_t_fields.

ENDCLASS.

CLASS lcl_screen_fields IMPLEMENTATION.
  METHOD set_group_status.
    APPEND VALUE #( group = group value = value ) TO lcl_screen_fields=>fields.
  ENDMETHOD.

  METHOD is_active.
    TRY.
        value = COND #( WHEN lcl_screen_fields=>fields[ group = group ]-value = lcl_screen_fields=>c_status-active THEN abap_true ELSE abap_false ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_screen_fields.
    "//You can take LCL_SCREEN_FIELDS=>FIELDS directelY;
    MOVE lcl_screen_fields=>fields TO fields.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_vinculados DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    CLASS-METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_vinculados IMPLEMENTATION.
  METHOD handle_set_toolbar.
    CHECK NOT line_exists( e_object->mt_toolbar[ function = 'CREATE_DOCUMENT' ] )
          AND lcl_screen_fields=>is_active( 'GR1' ) = abap_true.

    APPEND VALUE #( butn_type = cntb_btype_sep ) TO e_object->mt_toolbar.
    APPEND VALUE
      #( butn_type = cntb_btype_button
         function  = 'CREATE_DOCUMENT'
         icon      = icon_generate
         text      = 'Gerar documento'
       ) TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.
    CALL METHOD wa_alv_vinc->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_rows).

    CASE e_ucomm.
      WHEN 'CREATE_DOCUMENT'.
        DATA errors   TYPE TABLE OF bapiret2.
        DATA document TYPE bapi2017_gm_head_ret.

        LOOP AT selected_rows INTO DATA(_row).

          TRY.
              ASSIGN it_saida_vinc[ _row-index ] TO FIELD-SYMBOL(<fs_vinculacao>).

              CHECK ( <fs_vinculacao>-mblnr IS INITIAL ).

              READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
              READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.

              "//Realiza transferência de material do pedido p/
              "//o material da venda;
              PERFORM material_movement_output
                USING
                  sy-datum
                  ''
                  wa_saida_mm-matnr
                  wa_saida_mm-werks
                  <fs_vinculacao>-lgort_orig
                  <fs_vinculacao>-charg_orig
                  <fs_vinculacao>-qtd_v
                  '311'
                  wa_saida_sd-werks
                  wa_saida_sd-lgort
                  wa_saida_sd-matnr
                  wa_saida_sd-charg
                CHANGING
                  document
                  errors.

              "//Set created document into table;
              UPDATE zsdt0062 SET mblnr = document-mat_doc mjahr = document-doc_year
               WHERE ebeln EQ <fs_vinculacao>-ebeln
                 AND ebelp EQ <fs_vinculacao>-ebelp
                 AND vbeln EQ <fs_vinculacao>-vbeln
                 AND posnr EQ <fs_vinculacao>-posnr.

              <fs_vinculacao>-mblnr = document-mat_doc.

            CATCH cx_sy_itab_line_not_found cx_abap_util_exception.
          ENDTRY.
        ENDLOOP.

        wa_alv_vinc->refresh_table_display( ).

        IF NOT ( errors IS INITIAL ).
          CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
            EXPORTING
              it_message = errors.
        ENDIF.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .
  PERFORM: condicoes_status.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CONDICOES_STATUS
*&---------------------------------------------------------------------*
FORM condicoes_status .

  wa_status-sign   = 'I'.
  wa_status-option = 'EQ'.
  wa_status-low = ''.

  APPEND wa_status TO r_ekpo_s.

  " Produzido
  PERFORM selecionar_produzido.
  " Compras
  PERFORM: seleciona_dados_compras.
  " Vendas
  PERFORM seleciona_dados_vendas.
  " Agrupamento
  PERFORM: agrupamento.


ENDFORM.                    " CONDICOES_STATUS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_PRODUZIDO
*&---------------------------------------------------------------------*
FORM selecionar_produzido.

  DATA: total TYPE s225-wemng.   "#EC CI_USAGE_OK[2268063]

  CLEAR: it_makt[].

  IF ( p_spart-low EQ '04' ).

    " Produzido
    SELECT *
       FROM setleaf
       INTO TABLE it_setleaf_aux
     WHERE setname EQ 'MAGGI_ZSDT0041'.

    IF NOT ( it_setleaf_aux[] IS INITIAL ).

      LOOP AT it_setleaf_aux INTO wa_setleaf_aux.
        wa_setleaf-valfrom_aux = wa_setleaf_aux-valfrom.
        APPEND wa_setleaf TO it_setleaf.
      ENDLOOP.

      SELECT matkl matnr
        FROM mara
        INTO TABLE it_mara
        FOR ALL ENTRIES IN it_setleaf
      WHERE matkl EQ it_setleaf-valfrom_aux
        AND matnr IN p_matnr.

      CHECK NOT it_mara[] IS INITIAL.


      " Goods receipts: repetitive manufacturing
      SELECT sptag werks matnr wemng amein
        FROM s225     "#EC CI_USAGE_OK[2268063]
        INTO TABLE it_s225_aux
        FOR ALL ENTRIES IN it_mara
      WHERE matnr EQ it_mara-matnr
        AND sptag IN p_erdat
        AND werks IN p_werks.


      "Local de negócios
      SELECT bukrs branch
        FROM j_1bbranch
        INTO TABLE it_j_1bbranch
        FOR ALL ENTRIES IN it_s225_aux
      WHERE branch EQ it_s225_aux-werks
        AND bukrs IN p_bukrs.

      " Descrição do Material
      SELECT mandt matnr spras maktx maktg
        FROM makt
        INTO TABLE it_makt
        FOR ALL ENTRIES IN it_s225_aux
     WHERE matnr EQ it_s225_aux-matnr.

      REFRESH: it_s225_soma[].
      it_s225_soma[] = it_s225_aux[].

      SORT: it_makt       BY matnr,
            it_j_1bbranch BY branch,
            it_s225_aux   BY matnr,
            it_s225_soma  BY matnr.

      CLEAR: wa_saida_aux-categoria.
      LOOP AT it_s225_aux INTO wa_s225_aux.
        LOOP AT it_s225_soma INTO wa_s225_soma WHERE matnr EQ wa_s225_aux-matnr
                                                 AND werks EQ wa_s225_aux-werks.
          READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY branch = wa_s225_soma-werks BINARY SEARCH.
          IF ( sy-subrc EQ 0 ).

            IF ( wa_s225_soma-amein EQ 'KG' ).

              wa_s225_soma-menge = wa_s225_soma-wemng.
              CALL FUNCTION 'ME_CONVERSION_BPRME'
                EXPORTING
                  i_matnr             = wa_s225_soma-matnr
                  i_mein1             = 'KG'
                  i_mein2             = 'BAG'
                  i_meins             = 'KG'
                  i_menge             = wa_s225_soma-menge
                IMPORTING
                  menge               = wa_saida_aux-wemng_p
                EXCEPTIONS
                  error_in_conversion = 01
                  no_success          = 02.

            ELSE.
              wa_saida_aux-wemng_p  = wa_s225_soma-wemng.
            ENDIF.

            wa_saida_aux-categoria = 'P'.
            wa_saida_aux-sptag_p  = wa_s225_soma-sptag.
            wa_saida_aux-werks_p  = wa_s225_soma-werks.

            READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_s225_soma-matnr BINARY SEARCH.
            wa_saida_aux-matnr  = wa_makt-matnr.
            wa_saida_aux-maktx  = wa_makt-maktx.

            total = total + wa_saida_aux-wemng_p.
          ENDIF.
          CLEAR: wa_s225_soma.
        ENDLOOP.

        IF ( wa_saida_aux-categoria EQ 'P' ).
          wa_saida_aux-total_produzido_p = total.
          APPEND wa_saida_aux TO it_saida_aux.

          IF NOT ( it_s225 IS INITIAL ).
            DELETE it_s225 WHERE matnr EQ wa_s225_soma-matnr.
          ENDIF.
        ENDIF.
        CLEAR: wa_saida_aux, wa_s225_aux, wa_s225_soma, total.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " SELECIONAR_PRODUZIDO
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_COMPRAS
*&---------------------------------------------------------------------*
FORM seleciona_dados_compras .

  DATA: total   TYPE ekbe-menge,
        total_f TYPE ekbe-menge.

*  " Cabeçalho do documento de compra
*  SELECT EBELN BSART LIFNR BUKRS AEDAT WAERS UNSEZ
*   FROM EKKO
*   INTO TABLE IT_EKKO
*  WHERE  BSART IN P_BSART
*     AND AEDAT IN P_ERDAT
*     AND BUKRS IN P_BUKRS
*     AND UNSEZ IN P_SAFRA.
*
*  CHECK NOT IT_EKKO[] IS INITIAL.
*
*  " Item do documento de compras
*  SELECT EBELN EBELP MATNR TXZ01 MENGE NETWR LOEKZ MEINS NETPR WERKS
*   FROM EKPO
*   INTO TABLE IT_EKPO
*   FOR ALL ENTRIES IN IT_EKKO
*  WHERE EBELN EQ IT_EKKO-EBELN
*    AND LOEKZ IN R_EKPO_S
*    AND MATNR IN P_MATNR.

  IF p_matnrp IS INITIAL.
    p_matnrp[] = p_matnr[].
  ENDIF.

  TRY.
      DATA(_purchasing_number) = p_ebeln[ 1 ]-low.
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  IF ( _purchasing_number IS NOT INITIAL ).

    SELECT *
      FROM ekpo AS a
     INNER JOIN ekko AS b ON b~ebeln = a~ebeln
      INTO CORRESPONDING FIELDS OF TABLE it_ekpo
     WHERE b~bsart IN p_bsart
       AND b~aedat IN p_erdat
       AND b~bukrs IN p_bukrs
       AND b~unsez IN p_safra
       AND a~ebeln IN p_ebeln
       AND a~loekz IN r_ekpo_s.

  ELSE.
    SELECT *
      FROM ekpo AS a
     INNER JOIN ekko AS b ON b~ebeln = a~ebeln
      INTO CORRESPONDING FIELDS OF TABLE it_ekpo
     WHERE b~bsart IN p_bsart
       AND b~aedat IN p_erdat
       AND b~bukrs IN p_bukrs
       AND b~unsez IN p_safra
       AND a~loekz IN r_ekpo_s
       AND a~matnr IN p_matnrp.
  ENDIF.

  SELECT ebeln bsart lifnr bukrs aedat waers unsez submi
    FROM ekko
    INTO TABLE it_ekko
 FOR ALL ENTRIES IN it_ekpo
   WHERE ebeln = it_ekpo-ebeln.

  CHECK NOT it_ekpo[] IS INITIAL.

  " Mestre de fornecedores (parte geral)
  SELECT lifnr name1
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_ekko
  WHERE lifnr EQ it_ekko-lifnr.

  " Descrição do Material
  SELECT mandt matnr spras maktx maktg
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_ekpo
 WHERE matnr EQ it_ekpo-matnr.


  SORT: it_ekbe BY ebeln,
        it_ekpo BY ebeln,
        it_rbkp BY belnr gjahr,
        it_lfa1 BY lifnr,
        it_makt BY matnr.

  LOOP AT it_ekko INTO wa_ekko.

    wa_compra_aux-ebeln = wa_ekko-ebeln.
    wa_compra_aux-bsart = wa_ekko-bsart.
    wa_compra_aux-bukrs = wa_ekko-bukrs.
    wa_compra_aux-aedat = wa_ekko-aedat.
    wa_compra_aux-waers = wa_ekko-waers.
    wa_compra_aux-bsart = wa_ekko-bsart.
    wa_compra_aux-submi = wa_ekko-submi.

    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekko-ebeln BINARY SEARCH.

    IF ( sy-subrc EQ 0 ).
      wa_compra_aux-menge = wa_ekpo-menge.
      wa_compra_aux-ebelp = wa_ekpo-ebelp.
      wa_compra_aux-txz01 = wa_ekpo-txz01.
      wa_compra_aux-netwr = wa_ekpo-netwr.
      wa_compra_aux-loekz = wa_ekpo-loekz.
      wa_compra_aux-meins = wa_ekpo-meins.
      wa_compra_aux-netpr = wa_ekpo-netpr.
      wa_compra_aux-werks = wa_ekpo-werks.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ekpo-matnr BINARY SEARCH.
      wa_compra_aux-matnr = wa_makt-matnr.
      wa_compra_aux-maktx = wa_makt-maktx.


      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekko-lifnr BINARY SEARCH.
      wa_compra_aux-lifnr = wa_lfa1-lifnr.
      wa_compra_aux-name1 = wa_lfa1-name1.

      APPEND wa_compra_aux TO it_compra_aux.

    ENDIF.

    CLEAR: wa_ekko,
           wa_ekbe,
           wa_lfa1,
           wa_compra_aux.
  ENDLOOP.

  CLEAR: wa_compra_aux.

  " Cabeçalho do documento de compra
  SELECT ebeln bsart lifnr bukrs aedat waers unsez
   FROM ekko
   INTO TABLE it_ekko_forn
   FOR ALL ENTRIES IN it_ekpo
  WHERE ebeln EQ it_ekpo-ebeln
    AND unsez IN p_safra.


  SORT: it_compra_aux BY matnr menge ebelp,
          it_makt     BY matnr,
          it_lfa1     BY lifnr,
          it_ekbe     BY ebeln,
          it_rbkp     BY belnr.
*---> 05/07/2023 - Migração S4 - DL
  SORT it_ekko BY ebeln.
  SORT it_ekko_forn BY ebeln.
*<--- 05/07/2023 - Migração S4 - DL

  LOOP AT it_ekpo INTO wa_ekpo.
    READ TABLE it_compra_aux INTO wa_compra_aux WITH KEY "matnr = wa_ekpo-matnr
                                                         "menge = wa_ekpo-menge
                                                         ebeln = wa_ekpo-ebeln
                                                         ebelp = wa_ekpo-ebelp.
    IF ( sy-subrc NE 0 ).
      wa_compra_aux-ebeln = wa_ekpo-ebeln.
      wa_compra_aux-menge = wa_ekpo-menge.
      wa_compra_aux-ebelp = wa_ekpo-ebelp.
      wa_compra_aux-txz01 = wa_ekpo-txz01.
      wa_compra_aux-netwr = wa_ekpo-netwr.
      wa_compra_aux-loekz = wa_ekpo-loekz.
      wa_compra_aux-meins = wa_ekpo-meins.
      wa_compra_aux-netpr = wa_ekpo-netpr.
      wa_compra_aux-werks = wa_ekpo-werks.

      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.
      IF ( sy-subrc EQ 0 ).
        wa_compra_aux-waers = wa_ekko-waers.
        wa_compra_aux-bsart = wa_ekko-bsart.
        wa_compra_aux-submi = wa_ekko-submi.
      ENDIF.

      READ TABLE it_ekko_forn INTO wa_ekko_forn WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.
      IF ( sy-subrc EQ 0 ).

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekko_forn-lifnr BINARY SEARCH.
        wa_compra_aux-lifnr = wa_lfa1-lifnr.
        wa_compra_aux-name1 = wa_lfa1-name1.
      ENDIF.


      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ekpo-matnr BINARY SEARCH.
      wa_compra_aux-matnr = wa_makt-matnr.
      wa_compra_aux-maktx = wa_makt-maktx.


      APPEND wa_compra_aux TO it_compra_aux.
    ENDIF.
    CLEAR: wa_compra_aux, wa_makt, wa_ekbe, wa_ekpo, wa_rbkp, wa_ekko.
  ENDLOOP.


  " Tratamento para pegar o faturado
  IF NOT ( it_compra_aux[] IS INITIAL ).

    " Histórico para o documento de compra
    SELECT ebeln vgabe menge shkzg belnr budat dmbtr ebelp matnr gjahr bewtp
      FROM ekbe
      INTO TABLE it_ekbe
      FOR ALL ENTRIES IN it_compra_aux
    WHERE ebeln EQ it_compra_aux-ebeln
      AND vgabe = '2'
      AND matnr EQ it_compra_aux-matnr
      "and shkzg ne 'H'
      AND bewtp EQ 'Q'
      AND budat IN p_fatur.

    " Cabeçalho doc.da fatura recebida
    SELECT belnr gjahr xblnr stblg
      FROM rbkp
      INTO TABLE it_rbkp
      FOR ALL ENTRIES IN it_ekbe
    WHERE belnr EQ it_ekbe-belnr
      AND gjahr EQ it_ekbe-gjahr
      AND stblg EQ ''.

    it_ekbe_aux[] = it_ekbe[].


    SORT: it_ekbe     BY matnr,
          it_ekbe_aux BY matnr.

    LOOP AT it_ekbe_aux INTO wa_ekbe_aux.
      LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln EQ wa_ekbe_aux-ebeln
                                     AND matnr EQ wa_ekbe_aux-matnr.

        IF ( wa_ekbe-shkzg EQ 'S' ).
          total_f = total_f + wa_ekbe-menge.
        ELSEIF ( wa_ekbe-shkzg EQ 'H' )  .
          total_f = total_f - wa_ekbe-menge.
        ENDIF.
      ENDLOOP.

      IF NOT ( total_f IS INITIAL ).
        wa_faturado_aux-ebeln          = wa_ekbe_aux-ebeln.
        wa_faturado_aux-matnr          = wa_ekbe_aux-matnr.
        wa_faturado_aux-valor_faturado = total_f.
        APPEND wa_faturado_aux TO it_faturado_aux.
      ENDIF.

      DELETE it_ekbe_aux WHERE ebeln EQ wa_ekbe_aux-ebeln
                           AND matnr EQ wa_ekbe_aux-matnr.

      CLEAR: wa_ekbe, wa_ekbe_aux, total_f.
    ENDLOOP.
  ENDIF.

  REFRESH: it_faturado_soma[].
  it_faturado_soma[] = it_faturado_aux[].


  SORT: it_faturado_aux  BY matnr,
        it_faturado_soma BY matnr.

  LOOP AT it_faturado_aux INTO wa_faturado_aux.
    LOOP AT it_faturado_soma INTO wa_faturado_soma WHERE matnr EQ wa_faturado_aux-matnr.
      wa_faturado-valor_total = wa_faturado-valor_total + wa_faturado_soma-valor_faturado.
      wa_faturado-matnr       = wa_faturado_soma-matnr.
    ENDLOOP.
    APPEND wa_faturado TO it_faturado.
    DELETE it_faturado_aux WHERE matnr EQ wa_faturado_soma-matnr.
    CLEAR: wa_faturado_soma, wa_faturado.
  ENDLOOP.


  REFRESH: it_compra_soma[], it_compra[].
  it_compra_soma[] = it_compra_aux[].
  it_compra[]      = it_compra_aux[].

  SORT: it_compra_soma BY matnr,
        it_compra      BY matnr.

  LOOP AT it_compra_soma INTO wa_compra_soma.
    LOOP AT it_compra INTO wa_compra WHERE matnr EQ wa_compra_soma-matnr
                                       AND ebelp EQ wa_compra_soma-ebelp.

      wa_saida_aux-matnr     = wa_compra-matnr.
      wa_saida_aux-maktx     = wa_compra-maktx.
      wa_saida_aux-categoria = 'C'.

      wa_saida_aux-ebeln_c  = wa_compra-ebeln.
      wa_saida_aux-bsart_c  = wa_compra-bsart.
      wa_saida_aux-bukrs_c  = wa_compra-bukrs.
      wa_saida_aux-aedat_c  = wa_compra-aedat.
      wa_saida_aux-waers_c  = wa_compra-waers.
      wa_saida_aux-vgabe_c  = wa_compra-vgabe.
      wa_saida_aux-menge_c  = wa_compra-menge.
      wa_saida_aux-shkzg_c  = wa_compra-shkzg.
      wa_saida_aux-belnr_c  = wa_compra-belnr.
      wa_saida_aux-budat_c  = wa_compra-budat.
      wa_saida_aux-dmbtr_c  = wa_compra-dmbtr.
      wa_saida_aux-ebelp_c  = wa_compra-ebelp.
      wa_saida_aux-txz01_c  = wa_compra-txz01.
      wa_saida_aux-netwr_c  = wa_compra-netwr.
      wa_saida_aux-loekz_c  = wa_compra-loekz.
      wa_saida_aux-meins_c  = wa_compra-meins.
      wa_saida_aux-gjahr_c  = wa_compra-gjahr.
      wa_saida_aux-xblnr_c  = wa_compra-xblnr.
      wa_saida_aux-lifnr_c  = wa_compra-lifnr.
      wa_saida_aux-name1_c  = wa_compra-name1.
      wa_saida_aux-netpr_c  = wa_compra-netpr.


      IF ( wa_compra-shkzg EQ 'S' ).
        total = total + wa_compra-menge.
      ELSEIF ( wa_compra-shkzg EQ 'H' ).
        total = - ( total + wa_compra-menge ).
      ELSEIF ( wa_compra-shkzg EQ '' ).
        total = total + wa_compra-menge.
      ENDIF.

      wa_saida_aux-total_compra = total.

    ENDLOOP.

    READ TABLE it_saida_aux WITH KEY matnr = wa_saida_aux-matnr.
    IF NOT sy-subrc IS INITIAL.
      APPEND wa_saida_aux TO it_saida_aux.
    ELSE.
      wa_saida_aux-total_compra = wa_saida_aux-total_compra + it_saida_aux-total_compra.
      MODIFY it_saida_aux FROM wa_saida_aux INDEX sy-tabix TRANSPORTING total_compra.
    ENDIF.

    IF NOT ( it_saida_aux[] IS INITIAL ).
      DELETE it_compra_soma WHERE matnr EQ wa_compra-matnr
                              AND ebelp EQ wa_compra-ebelp.

    ENDIF.

    CLEAR: wa_saida_aux, wa_compra, wa_compra_soma, total, total_f, wa_ekbe, wa_rbkp.
  ENDLOOP.




ENDFORM.                    " SELECIONA_DADOS_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_VENDAS
*&---------------------------------------------------------------------*
FORM seleciona_dados_vendas .
  DATA: total   TYPE vbfa-rfmng,
        total_f TYPE vbfa-rfmng.

  DATA: "IT_ZSDT0131 TYPE STANDARD TABLE OF ZSDT0131,
    "IT_ZSDT0082 TYPE STANDARD TABLE OF ZSDT0082,
    wa_zsdt0131 TYPE zsdt0131,
    wa_zsdt0082 TYPE zsdt0082,
    wa_zsdt0062 TYPE zsdt0062.

  PERFORM: status_pedido.


  IF NOT ( p_safra IS INITIAL ).

    CHECK NOT it_vbak[] IS INITIAL.
    CHECK NOT it_zsdt0041[] IS INITIAL.
    CHECK NOT it_zsdt0040[] IS INITIAL.

  ELSE.
    CHECK NOT it_vbak[] IS INITIAL.
  ENDIF.

  CLEAR: it_makt.

*  SELECT VBELN MATNR ARKTX WERKS ZMENG NETWR KWMENG VRKME POSNR NETPR
*   FROM VBAP
*   INTO TABLE IT_VBAP
*   FOR ALL ENTRIES IN IT_VBAK
*  WHERE VBELN EQ IT_VBAK-VBELN
*     AND MATNR IN P_MATNR.

* Modificação Insumos

  IF p_nrocg IS NOT INITIAL.
    PERFORM tabela_qtd_via_carga.
  ELSEIF p_nrosl IS NOT INITIAL.
    PERFORM tabela_qtd_via_solic.
  ENDIF.

*  SELECT VBELN MATNR ARKTX WERKS ZMENG NETWR KWMENG VRKME POSNR NETPR
*   FROM VBAP
*   INTO TABLE IT_VBAP
*   FOR ALL ENTRIES IN IT_VBAK
*   WHERE VBELN EQ IT_VBAK-VBELN
*     AND MATNR IN P_MATNR
*     AND EXISTS ( SELECT *
*                    FROM ZSDT0131
*                    WHERE VBELN  EQ VBAP~VBELN
*                      AND POSNR  EQ VBAP~POSNR
*                      AND STATUS NE 'X' ).
*
*  IF IT_VBAP IS NOT INITIAL.
*    SELECT *
*      FROM ZSDT0131
*      INTO TABLE IT_ZSDT0131
*      FOR ALL ENTRIES IN IT_VBAP
*      WHERE VBELN  EQ IT_VBAP-VBELN
*        AND POSNR  EQ IT_VBAP-POSNR
*        AND EXISTS ( SELECT *
*                       FROM ZSDT0129
*                       WHERE NRO_LOTE EQ ZSDT0131~NRO_LOTE
*                         AND NRO_CG   IN P_NROCG
*                         AND STATUS   NE 'X' )
*        AND STATUS NE 'X'.
*  ENDIF.

* Modificação Insumos

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
          READ TABLE it_0026
            WITH KEY obj_key = it_zib-obj_key.

          IF sy-subrc IS INITIAL.
            MOVE : it_zib-bukrs   TO it_zib_bsik-bukrs,
                   it_0026-docnum TO it_zib_bsik-belnr,
                   it_zib-gjahr   TO it_zib_bsik-gjahr.

            APPEND it_zib_bsik.

          ENDIF.
          CLEAR: it_zib_bsik.
        ENDLOOP.

        IF sy-subrc IS INITIAL.
          SELECT *
            FROM bsik
            INTO TABLE it_bsik
             FOR ALL ENTRIES IN it_zib_bsik
              WHERE bukrs EQ it_zib_bsik-bukrs
                AND belnr EQ it_zib_bsik-belnr
                AND gjahr EQ it_zib_bsik-gjahr.

        ENDIF.
      ENDIF.


    ENDIF.

  ENDIF.

  SELECT *
    FROM vbkd
    INTO TABLE it_vbkd
     FOR ALL ENTRIES IN it_vbak
     WHERE vbeln EQ it_vbak-vbeln.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM t052u
      INTO TABLE it_t052u
       FOR ALL ENTRIES IN it_vbkd
        WHERE spras EQ 'PT'
          AND zterm EQ it_vbkd-zterm.
  ENDIF.

  " Descrição do Material
  SELECT mandt matnr spras maktx maktg
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_vbap
 WHERE matnr EQ it_vbap-matnr.


  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_vbak
  WHERE kunnr EQ it_vbak-kunnr.

  SORT: it_vbak BY vbeln,
        it_vbap BY vbeln,
        it_makt BY matnr,
        it_kna1 BY kunnr.


  LOOP AT it_vbap INTO wa_vbap.

    wa_venda_aux-matnr   = wa_vbap-matnr.
    wa_venda_aux-arktx   = wa_vbap-arktx.
    wa_venda_aux-werks   = wa_vbap-werks.
    wa_venda_aux-zmeng   = wa_vbap-zmeng.
    wa_venda_aux-netwr   = wa_vbap-netwr.
* Modificação Insumos
*   WA_VENDA_AUX-KWMENG  = WA_VBAP-KWMENG.
    IF p_nrocg IS NOT INITIAL.
      LOOP AT it_zsdt0131 INTO wa_zsdt0131 WHERE vbeln EQ wa_vbap-vbeln
                                                 AND posnr EQ wa_vbap-posnr.

        wa_venda_aux-nro_sol = wa_zsdt0131-nro_sol.
        wa_venda_aux-seq     = wa_zsdt0131-seq.
        wa_venda_aux-kwmeng  = wa_venda_aux-kwmeng + wa_zsdt0131-qtd_vinc.
      ENDLOOP.
      LOOP AT it_zsdt0062_soma INTO wa_zsdt0062 WHERE matnr  EQ wa_vbap-matnr
                                                  AND vbeln  EQ wa_vbap-vbeln
                                                  AND posnr  EQ wa_vbap-posnr
                                                  AND nro_cg EQ p_nrocg-low.

        wa_venda_aux-qtd_vinc = wa_venda_aux-qtd_vinc + wa_zsdt0062-qtd_vinc.
      ENDLOOP.
    ELSEIF p_nrosl IS NOT INITIAL.
      LOOP AT it_zsdt0082 INTO wa_zsdt0082.
        wa_venda_aux-kwmeng  = wa_venda_aux-kwmeng + wa_zsdt0082-qte_lib.
      ENDLOOP.
      LOOP AT it_zsdt0062_soma INTO wa_zsdt0062 WHERE matnr   EQ wa_vbap-matnr
                                                  AND vbeln   EQ wa_vbap-vbeln
                                                  AND posnr   EQ wa_vbap-posnr
                                                  AND nro_sol EQ p_nrosl-low
                                                  AND seq     EQ p_seqsl-low.

        wa_venda_aux-qtd_vinc = wa_venda_aux-qtd_vinc + wa_zsdt0062-qtd_vinc.
      ENDLOOP.
    ENDIF.
* Modificação Insumos

    wa_venda_aux-vrkme   = wa_vbap-vrkme.
    wa_venda_aux-posnr   = wa_vbap-posnr.
    wa_venda_aux-netpr   = wa_vbap-netpr.
    wa_venda_aux-werks   = wa_vbap-werks.
    wa_venda_aux-lgort   = wa_vbap-lgort.
    wa_venda_aux-charg   = wa_vbap-charg.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_vbap-vbeln BINARY SEARCH.
    wa_venda_aux-vbeln = wa_vbak-vbeln.
    wa_venda_aux-vkorg = wa_vbak-vkorg.
    wa_venda_aux-vtweg = wa_vbak-vtweg.
    wa_venda_aux-spart = wa_vbak-spart.
    wa_venda_aux-auart = wa_vbak-auart.
    wa_venda_aux-vkbur = wa_vbak-vkbur.
    wa_venda_aux-waerk = wa_vbak-waerk.
    wa_venda_aux-erdat = wa_vbak-erdat.
    wa_venda_aux-faksk = wa_vbak-faksk.
    wa_venda_aux-lifsk = wa_vbak-lifsk.
    wa_venda_aux-audat = wa_vbak-audat.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.
    wa_venda_aux-maktx   = wa_makt-maktx.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
    wa_venda_aux-kunnr = wa_kna1-kunnr.
    wa_venda_aux-name1 = wa_kna1-name1.

    READ TABLE it_konv INTO wa_konv WITH KEY kposn = wa_vbap-posnr
                                             knumv = wa_vbak-knumv.
    IF ( sy-subrc EQ 0 ).
      wa_venda_aux-kbetr = wa_konv-kbetr.
    ENDIF.

    APPEND wa_venda_aux TO it_venda_aux.

    CLEAR: wa_vbap, wa_vbak, wa_vbap, wa_makt, wa_kna1, wa_konv, wa_venda_aux.

  ENDLOOP.

  " Fatura - Venda
  SELECT rfmng vbeln vbtyp_n vbelv vbtyp_v meins erdat matnr posnv
    FROM vbfa
    INTO TABLE it_vbfa
    FOR ALL ENTRIES IN it_venda_aux
  WHERE vbelv EQ it_venda_aux-vbeln
    AND vbtyp_v = 'C'
    AND erdat IN p_fatur.

  IF NOT ( it_vbfa[] IS INITIAL ).
    DELETE it_vbfa WHERE vbtyp_n NE 'N'
                     AND vbtyp_n NE 'M'.
  ENDIF.


  SORT: it_venda_aux BY vbeln posnr.

  LOOP AT it_vbfa INTO wa_vbfa.

    "    wa_vbfa_aux-rfmng     = wa_vbfa-rfmng.
    wa_vbfa_aux-vbeln     = wa_vbfa-vbeln.
    wa_vbfa_aux-vbtyp_n   = wa_vbfa-vbtyp_n.
    wa_vbfa_aux-vbelv     = wa_vbfa-vbelv.
    wa_vbfa_aux-vbtyp_v   = wa_vbfa-vbtyp_v.
    wa_vbfa_aux-meins     = wa_vbfa-meins.
    wa_vbfa_aux-erdat     = wa_vbfa-erdat.
    wa_vbfa_aux-posnv     = wa_vbfa-posnv.

    READ TABLE it_venda_aux INTO wa_venda_aux WITH KEY vbeln = wa_vbfa_aux-vbelv
                                                       posnr = wa_vbfa_aux-posnv BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      wa_vbfa_aux-matnr     = wa_venda_aux-matnr.

      PERFORM: conversion_sac USING wa_venda_aux-matnr
                                    wa_vbfa-rfmng
                                    wa_vbfa-meins.

      IF NOT ( valor_conversion_faturado IS INITIAL ).
        wa_vbfa_aux-rfmng = valor_conversion_faturado.
      ENDIF.

    ENDIF.

    APPEND wa_vbfa_aux TO it_vbfa_aux.

    CLEAR: wa_vbfa_aux, wa_vbfa.
  ENDLOOP.

  REFRESH: it_vbfa_soma[].
  it_vbfa_soma[] = it_vbfa_aux[].

  SORT: it_vbfa_aux  BY matnr,
        it_vbfa_soma BY matnr.

  LOOP AT it_vbfa_aux INTO wa_vbfa_aux.
    LOOP AT it_vbfa_soma INTO wa_vbfa_soma WHERE matnr EQ wa_vbfa_aux-matnr
                                             AND vbelv EQ wa_vbfa_aux-vbelv.

      IF ( ( wa_vbfa_soma-vbtyp_n EQ 'M' ) OR ( wa_vbfa_soma-vbtyp_n EQ 'S') ) .
        total_f = total_f + wa_vbfa_soma-rfmng.
      ELSEIF ( ( wa_vbfa_soma-vbtyp_n EQ 'N' ) OR ( wa_vbfa_soma-vbtyp_n EQ 'O' ) ).
        total_f = total_f - wa_vbfa_soma-rfmng.
      ENDIF.
    ENDLOOP.

    wa_faturado_venda-vbelv = wa_vbfa_soma-vbeln.
    wa_faturado_venda-matnr = wa_vbfa_soma-matnr.
    wa_faturado_venda-valor_faturado = total_f.


    APPEND wa_faturado_venda TO it_faturado_venda.
    DELETE it_vbfa_aux WHERE vbelv EQ wa_vbfa_soma-vbelv
                         AND matnr EQ wa_vbfa_soma-matnr.

    CLEAR: wa_vbfa_aux, wa_vbfa_soma, wa_faturado_venda, total_f, valor_conversion_faturado .
  ENDLOOP.

  REFRESH: it_faturado_venda_soma[].
  it_faturado_venda_aux[] = it_faturado_venda[].


  SORT: it_faturado_venda     BY matnr,
        it_faturado_venda_aux BY matnr.


  IF NOT ( it_faturado_venda[] IS INITIAL ).

    DELETE it_faturado_venda     WHERE matnr EQ ''.
    DELETE it_faturado_venda_aux WHERE matnr EQ ''.

    LOOP AT it_faturado_venda INTO wa_faturado_venda.

      LOOP AT it_faturado_venda_aux INTO wa_faturado_venda_aux WHERE matnr EQ wa_faturado_venda-matnr.
        wa_faturado_venda_soma-valor_total = wa_faturado_venda_soma-valor_total + wa_faturado_venda_aux-valor_faturado.
        wa_faturado_venda_soma-matnr       = wa_faturado_venda_aux-matnr.
      ENDLOOP.

      APPEND wa_faturado_venda_soma TO it_faturado_venda_soma.
      DELETE it_faturado_venda WHERE matnr EQ wa_faturado_venda_aux-matnr.
      CLEAR: wa_faturado_venda_soma, wa_faturado_venda, wa_faturado_venda_aux.

    ENDLOOP.

  ENDIF.

  REFRESH: it_venda_soma[].
  it_venda_soma[] = it_venda_aux[].
  it_venda[] = it_venda_aux[].

  CLEAR: wa_venda_aux.


  SORT: it_venda_aux  BY matnr,
        it_venda_soma BY matnr.

  LOOP AT it_venda_aux INTO wa_venda_aux.
    LOOP AT it_venda_soma INTO wa_venda_soma WHERE vbeln EQ wa_venda_aux-vbeln
                                               AND matnr EQ wa_venda_aux-matnr.

      wa_saida_aux-matnr     = wa_venda_soma-matnr.
      wa_saida_aux-maktx     = wa_venda_soma-maktx.
      wa_saida_aux-categoria = 'V'.

      wa_saida_aux-vbeln_v    = wa_venda_soma-vbeln.
      wa_saida_aux-vkorg_v    = wa_venda_soma-vkorg.
      wa_saida_aux-vtweg_v    = wa_venda_soma-vtweg.
      wa_saida_aux-spart_v    = wa_venda_soma-spart.
      wa_saida_aux-auart_v    = wa_venda_soma-auart.
      wa_saida_aux-vkbur_v    = wa_venda_soma-vkbur.
      wa_saida_aux-waerk_v    = wa_venda_soma-waerk.
      wa_saida_aux-erdat_v    = wa_venda_soma-erdat.
      wa_saida_aux-faksk_v    = wa_venda_soma-faksk.
      wa_saida_aux-lifsk_v    = wa_venda_soma-lifsk.
      wa_saida_aux-audat_v    = wa_venda_soma-audat.
      wa_saida_aux-arktx_v    = wa_venda_soma-arktx.
      wa_saida_aux-werks_v    = wa_venda_soma-werks.
      wa_saida_aux-zmeng_v    = wa_venda_soma-zmeng.
      wa_saida_aux-netwr_v    = wa_venda_soma-netwr.
      wa_saida_aux-kwmeng_v   = wa_venda_soma-kwmeng.
      wa_saida_aux-vrkme_v    = wa_venda_soma-vrkme.
      wa_saida_aux-posnr_v    = wa_venda_soma-posnr.
      wa_saida_aux-rfmng_v    = wa_venda_soma-rfmng.
      wa_saida_aux-vbtyp_n_v  = wa_venda_soma-vbtyp_n.
      wa_saida_aux-vbelv_v    = wa_venda_soma-vbelv.
      wa_saida_aux-vbtyp_v_v  = wa_venda_soma-vbtyp_v.
      wa_saida_aux-meins_v    = wa_venda_soma-meins.
      wa_saida_aux-kunnr_v    = wa_venda_soma-kunnr.
      wa_saida_aux-name1_v    = wa_venda_soma-name1.
      wa_saida_aux-kbetr_v    = wa_venda_soma-kbetr.
      wa_saida_aux-lifsp_v    = wa_venda_soma-lifsp.
      wa_saida_aux-qtd_vinc   = wa_venda_soma-qtd_vinc.

      IF ( wa_venda_soma-vbtyp_n EQ 'M' ).
        total = total + wa_venda_soma-kwmeng.
      ELSEIF ( wa_venda_soma-vbtyp_n EQ 'N' ).
        total = - ( total + wa_venda_soma-kwmeng ).
      ELSEIF ( wa_venda_soma-vbtyp_n EQ '' ).
        total = total + wa_venda_soma-kwmeng.
      ENDIF.

      wa_saida_aux-total_venda = total.

    ENDLOOP.

    APPEND wa_saida_aux TO it_saida_aux.

    IF NOT ( it_venda_aux[] IS INITIAL ).
      DELETE it_venda_aux WHERE matnr EQ wa_venda_soma-matnr
                            AND vbeln EQ wa_venda_soma-vbeln.
    ENDIF.

    CLEAR: wa_venda_soma, wa_venda_aux, wa_saida_aux, total, wa_faturado_venda.
  ENDLOOP.


ENDFORM.                    " SELECIONA_DADOS_VENDAS
*&---------------------------------------------------------------------*
*&      Form  STATUS_PEDIDO
*&---------------------------------------------------------------------*
FORM status_pedido .

*  SELECT VBELN VKORG  VTWEG SPART AUART  VKBUR KUNNR WAERK ERDAT FAKSK LIFSK AUDAT KNUMV
*   FROM VBAK
*   INTO TABLE IT_VBAK
*  WHERE  AUART IN P_AUART
*     AND VKORG IN P_VKORG
*     AND VTWEG IN P_VTWEG
*     AND SPART IN P_SPART
*     AND VKBUR IN P_VKBUR
*     AND ERDAT IN P_ERDAT
*     AND LIFSK EQ SPACE.
* Modificação Insumos

  IF p_nrocg IS NOT INITIAL.
    PERFORM it_vbak_via_carga.
  ELSEIF p_nrosl IS NOT INITIAL.
    PERFORM it_vbak_via_solic.
  ENDIF.

*  DATA: WA_ZSDT0133 TYPE ZSDT0133.
*  DATA: IT_ZSDT0131 TYPE STANDARD TABLE OF ZSDT0131,
*        IT_ZSDT0129 TYPE STANDARD TABLE OF ZSDT0129.
*
*  SELECT SINGLE *
*    FROM ZSDT0133
*    INTO WA_ZSDT0133
*    WHERE NRO_CG IN P_NROCG.
*
*  IF WA_ZSDT0133 IS NOT INITIAL.
*
*    SELECT *
*      FROM ZSDT0129
*      INTO TABLE IT_ZSDT0129
*      WHERE NRO_CG EQ WA_ZSDT0133-NRO_CG
*        AND STATUS NE 'X'.
*
*    IF IT_ZSDT0129 IS NOT INITIAL.
*
*      SELECT *
*        FROM ZSDT0131
*        INTO TABLE IT_ZSDT0131
*        FOR ALL ENTRIES IN IT_ZSDT0129
*        WHERE NRO_LOTE EQ IT_ZSDT0129-NRO_LOTE
*          AND STATUS NE 'X'.
*
*      IF IT_ZSDT0131 IS NOT INITIAL.
*
*        SELECT VBELN VKORG  VTWEG SPART AUART  VKBUR KUNNR WAERK ERDAT FAKSK LIFSK AUDAT KNUMV
*         FROM VBAK
*         INTO TABLE IT_VBAK
*         FOR ALL ENTRIES IN IT_ZSDT0131
*         WHERE VBELN EQ IT_ZSDT0131-VBELN
*           AND LIFSK EQ SPACE.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.

  IF NOT ( it_vbak[] IS INITIAL ).


    SELECT FROM v_konv FIELDS knumv , kposn , kschl , kbetr FOR ALL ENTRIES IN @it_vbak WHERE knumv EQ @it_vbak-knumv AND kschl EQ 'PR00' INTO TABLE @it_konv .


    IF NOT ( p_safra IS INITIAL ).

      SELECT vbeln doc_simulacao
        FROM zsdt0041
        INTO TABLE it_zsdt0041
        FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln.


      SELECT doc_simulacao safra
        FROM zsdt0040
        INTO TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0041
      WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao
        AND safra IN p_safra.

      CLEAR: wa_vbak.

      LOOP AT it_vbak INTO wa_vbak.

        READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_vbak-vbeln.
        IF ( sy-subrc EQ 0 ).
          READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.

          IF ( sy-subrc EQ 0 ).
            CONTINUE.
          ELSE.
            DELETE it_vbak WHERE vbeln EQ wa_vbak-vbeln.
          ENDIF.
        ELSE.
          DELETE it_vbak WHERE vbeln EQ wa_vbak-vbeln.
        ENDIF.
        CLEAR:  wa_zsdt0041, wa_zsdt0040, wa_vbak.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " STATUS_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  AGRUPAMENTO
*&---------------------------------------------------------------------*
FORM agrupamento .
  DATA ls_cellcolor TYPE lvc_s_scol .
  DATA: v_index TYPE sy-tabix.

  REFRESH: it_saida_soma[].

  it_saida_soma[] = it_saida_aux[].


  SORT: it_saida_soma BY matnr,
        it_saida_aux  BY matnr.


  LOOP AT it_saida_aux INTO wa_saida_aux WHERE vbeln_v IS  NOT INITIAL.

    CLEAR: wa_saida-total_produzido_p,
           wa_saida-total_compra,
           wa_saida-total_venda.

    IF p_matnrp[] <> p_matnr[] OR p_ebeln[] IS NOT INITIAL.

      LOOP AT it_saida_soma INTO wa_saida_soma WHERE ebeln_c IS NOT INITIAL.
        wa_saida-total_produzido_p = wa_saida-total_produzido_p + wa_saida_soma-total_produzido_p.
        wa_saida-total_compra    = wa_saida-total_compra + wa_saida_soma-total_compra.
*        WA_SAIDA-TOTAL_VENDA     = WA_SAIDA-TOTAL_VENDA  + WA_SAIDA_SOMA-TOTAL_VENDA.
        wa_saida-qtd_vinc        = wa_saida-qtd_vinc     + wa_saida_soma-qtd_vinc.
        CLEAR: wa_saida_soma.
      ENDLOOP.
*    ENDIF.

      wa_saida-total_venda = wa_saida_aux-total_venda.

    ELSE.

      LOOP AT it_saida_soma INTO wa_saida_soma WHERE matnr EQ wa_saida_aux-matnr.
        wa_saida-total_produzido_p = wa_saida-total_produzido_p + wa_saida_soma-total_produzido_p.
        wa_saida-total_compra    = wa_saida-total_compra + wa_saida_soma-total_compra.
        wa_saida-total_venda     = wa_saida-total_venda  + wa_saida_soma-total_venda.
        wa_saida-qtd_vinc        = wa_saida-qtd_vinc     + wa_saida_soma-qtd_vinc.
        CLEAR: wa_saida_soma.
      ENDLOOP.

    ENDIF.

    READ TABLE it_faturado INTO wa_faturado WITH KEY matnr = wa_saida_aux-matnr.
    IF ( sy-subrc EQ 0 ).
      wa_saida-total_f_c       = wa_faturado-valor_total.
    ENDIF.

    READ TABLE it_faturado_venda_soma INTO wa_faturado_venda_soma WITH KEY matnr = wa_saida_aux-matnr.
    IF ( sy-subrc EQ 0 ).
      wa_saida-total_f_v       = wa_faturado_venda_soma-valor_total.
    ENDIF.

    wa_saida-matnr           = wa_saida_aux-matnr.
    wa_saida-maktx           = wa_saida_aux-maktx.
    wa_saida-saldo           = ( wa_saida-total_compra + wa_saida-total_produzido_p ) - wa_saida-total_venda.
    wa_saida-saldo_c         = ( wa_saida-total_compra - wa_saida-total_f_c ).
    wa_saida-saldo_v         = ( wa_saida-total_venda - wa_saida-total_f_v ).
    wa_saida-ebeln_c         = wa_saida_aux-ebeln_c.

    APPEND wa_saida TO it_saida.

    IF NOT ( it_saida[] IS INITIAL ).
      IF p_matnrp[] <> p_matnr[] OR p_ebeln[] IS NOT INITIAL.
        DELETE it_saida_aux WHERE vbeln_v IS NOT INITIAL.
      ELSE.
        DELETE it_saida_aux WHERE matnr = wa_saida_aux-matnr.
      ENDIF.
    ENDIF.

    CLEAR: wa_saida, wa_saida_soma.
  ENDLOOP.

  LOOP AT it_saida.

    IF ( it_saida-saldo < 0 ).

      v_index = sy-tabix.
      ls_cellcolor-fname = 'SALDO' .
      ls_cellcolor-color-col = '6' .
      ls_cellcolor-color-int = '0' .
      ls_cellcolor-color-inv = '0' .

      APPEND ls_cellcolor TO it_saida-cellcolors.
      MODIFY it_saida INDEX v_index TRANSPORTING cellcolors.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " AGRUPAMENTO

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SAC
*&---------------------------------------------------------------------*
FORM conversion_sac  USING    p_matnr
                              p_valor
                              p_meins.

  IF ( ( p_meins EQ 'KG' ) AND ( p_spart-low EQ '04' ) ).

    valor = p_valor.
    CALL FUNCTION 'ME_CONVERSION_BPRME'
      EXPORTING
        i_matnr             = p_matnr
        i_mein1             = 'KG'
        i_mein2             = 'BAG'
        i_meins             = 'KG'
        i_menge             = valor
      IMPORTING
        menge               = valor_conversion_faturado
      EXCEPTIONS
        error_in_conversion = 01
        no_success          = 02.

  ELSE.
    valor_conversion_faturado  = p_valor.
  ENDIF.

ENDFORM.                    " CONVERSION_SAC
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS: zm_handle_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_double_click.
    PERFORM z_handle_double_click USING  e_row e_column es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Principal
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

  gv_screen1 = '0200'.
  gv_screen2 = '0300'.
  IF sy-ucomm = 'TAB_VINCULACAO'.
    gv_screen3 = '0401'.
  ELSE.
    gv_screen3 = '0400'.
  ENDIF.
  gv_screen4 = '0600'.

ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Principal
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  CASE sy-dynnr.
    WHEN: '0100'.
      CASE sy-ucomm.
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
MODULE pbo_0200 OUTPUT.

ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIAR_ALV_0200  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Agrupamento
*----------------------------------------------------------------------*
MODULE criar_alv_0200 OUTPUT.
  PERFORM: criar_catalog_0200,
           criar_alv_0200.
ENDMODULE.                 " CRIAR_ALV_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Agrupamento
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.

  DATA: subrc_mm TYPE sy-tabix,
        subrc_sd TYPE sy-tabix,
        error    TYPE sy-tabix.

  CASE sy-ucomm.

    WHEN: 'TAB_PRINCIPAL'.
      REFRESH: it_saida_mm[], it_saida_sd[].
      CLEAR: wa_saida_mm, wa_saida_sd, vinculados_email.
      tabstrip-activetab = 'TAB_PRINCIPAL'.
    WHEN: 'TAB_MM_SD'.
      CLEAR: vinculados_email.
      IF NOT ( wa_alv_vinc IS INITIAL ).
        CALL METHOD wa_alv_vinc->refresh_table_display.
      ENDIF.

      IF NOT ( it_saida_sd IS INITIAL ) OR ( NOT it_saida_mm IS INITIAL ).
        tabstrip-activetab = 'TAB_MM_SD'.

      ELSE.
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Executar duplo clique em um registro.'.
      ENDIF.
    WHEN: 'TAB_VINCULACAO'.
      CLEAR: wa_saida_mm, wa_saida_sd, vinculados_email,
             lcl_screen_fields=>fields.

      READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
      subrc_mm = sy-subrc.
      READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.
      subrc_sd = sy-subrc.

      IF ( subrc_mm NE 0 ) OR ( subrc_sd NE 0 ).
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.
      ELSEIF wa_saida_mm-saldo_mm LT 0 OR wa_saida_sd-saldo_sd LT 0.
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Saldo Negativo'.
      ELSEIF wa_saida_mm-werks NE wa_saida_sd-werks.
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Centros Divergentes'.
      ELSE.

        IF ( wa_saida_mm-matnr <> wa_saida_sd-matnr ).
          "//Set visible - screen 0400
          CALL METHOD lcl_screen_fields=>set_group_status
            EXPORTING
              group = 'GR1'
              value = lcl_screen_fields=>c_status-active.
        ELSE.
          "//Set invisible - screen 0400;
          CALL METHOD lcl_screen_fields=>set_group_status
            EXPORTING
              group = 'GR1'
              value = lcl_screen_fields=>c_status-inactive.
        ENDIF.

        PERFORM: tab_vinculacao.
        tabstrip-activetab = 'TAB_VINCULACAO'.
      ENDIF.

    WHEN: 'TAB_VINCULADOS'.
      CLEAR: wa_saida_mm, wa_saida_sd, wa_alv_vinculados, wa_cont_vinculados, wa_saida_vinculados.
      REFRESH: it_fcat_vinculados[], it_saida_vinculados[].

      READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
      IF ( sy-subrc NE 0 ).
        error = 1.
      ENDIF.
      subrc_mm = sy-subrc.

      READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.
      subrc_sd = sy-subrc.

      IF ( sy-subrc NE 0 ).
        error = error + 1.
      ENDIF.

      IF ( subrc_mm EQ 0 ) AND ( subrc_sd EQ 0 ).
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.
      ELSEIF ( subrc_sd EQ 0 ) AND ( subrc_mm EQ 0 ).
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.

      ELSEIF ( error EQ 2 ).
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecionar uma vinculação'.
      ELSE.
        vinculados_email = 'X'.
        PERFORM: selecionar_vinculados,
                 criar_alv_vinculados.
        tabstrip-activetab = 'TAB_VINCULADOS'.
      ENDIF.


  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT

*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_0200
*&---------------------------------------------------------------------*
FORM criar_catalog_0200 .
  REFRESH: it_fcat[].

  PERFORM alv_catalog_0200  USING:
        'MATNR'             'Material'               '10'       ''  'X' ''  ''      'MATN1' , " COD. MATERIAL
        'MAKTX'             'Descrição do Material'  '45'       ''  ''  ''  ''      ''      , " MATERIAL
        'TOTAL_PRODUZIDO_P' 'Produzido'              '15'       ''  ''  ''  ''      ''      , " PRODUZIDO
        'TOTAL_COMPRA'      'Qtd. Pedido Compra'     '13'       ''  ''  ''  'C300'  ''      , " COMPRA
        'TOTAL_VENDA'       'Qtd. Ordem'             '13'       ''  ''  ''  'C300'  ''      , " VENDA
        'SALDO'             'Saldo'                  '13'       ''  ''  ''  ''      ''      , " SALDO
        'TOTAL_F_C'         'Faturado Compra'        '13'       ''  ''  ''  'C500'  ''      , " SALDO FATURADO
        'SALDO_C'           'Saldo Compra'           '13'       ''  ''  ''  ''      ''      , " SALDO FATURADO
        'TOTAL_F_V'         'Faturado Venda'         '13'       ''  ''  ''  'C500'  ''      , " SALDO FATURADO
        'SALDO_V'           'Saldo Venda'            '13'       ''  ''  ''  ''      ''      , " SALDO FATURADO
        'QTD_VINC'          'Qtd. Vinculada'         '13'       ''  ''  ''  ''      ''      . " QTD Vinculada

ENDFORM.                   " CRIAR_CATALOG_0200
*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_0100
*&---------------------------------------------------------------------*
FORM alv_catalog_0200  USING   p_campo    TYPE c
                               p_desc     TYPE c
                               p_tam      TYPE c
                               p_hot      TYPE c
                               p_zero     TYPE c
                               p_sum      TYPE c
                               p_cor      TYPE c
                               p_convexit TYPE c.


  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum   =  p_sum.
  wl_fcat-outputlen = p_tam.
  wl_fcat-emphasize = p_cor.
  wl_fcat-convexit  = p_convexit.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_CATALOG_0100
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_0200
*&---------------------------------------------------------------------*
FORM criar_alv_0200 .

  DATA: wa_event_0200    TYPE REF TO lcl_event_receiver.

  IF wa_container_0200 IS INITIAL.

    CREATE OBJECT wa_container_0200
      EXPORTING
        container_name              = 'CONTAINER_0200'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.

  IF ( wa_alv_0200 IS INITIAL ) AND ( NOT wa_container_0200 IS INITIAL ) .

    CREATE OBJECT wa_alv_0200
      EXPORTING
        i_parent          = wa_container_0200
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event_0200 IS INITIAL.
    CREATE OBJECT wa_event_0200.
    SET HANDLER: wa_event_0200->zm_handle_double_click FOR wa_alv_0200.
  ENDIF.

  wa_layout-ctab_fname = 'CELLCOLORS'.

  CALL METHOD wa_alv_0200->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida[]
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

ENDFORM.                    " CRIAR_ALV_0200
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM z_handle_double_click  USING    p_row
                                     p_column
                                     p_row_no.

  READ TABLE it_saida INTO wa_saida INDEX p_row.


  PERFORM compras USING  wa_saida-matnr.

  IF NOT ( it_saida_mm[] IS INITIAL ).

    PERFORM vendas  USING  wa_saida-matnr.

    IF NOT ( it_saida_sd[] IS INITIAL ).

      PERFORM: criar_catalog_compras,
               criar_alv_compras.

      PERFORM: criar_catalog_vendas,
               criar_alv_vendas.

      tabstrip-activetab = 'TAB_MM_SD'.

    ELSE.
      MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Não Contêm dados de Vendas.'.


    ENDIF.

  ELSE.
    MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Não Contêm dados de Compras.'.


  ENDIF.



ENDFORM.                    " Z_HANDLE_DOUBLE_CLICK
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_compras DEFINITION.


  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_compras FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_compras IMPLEMENTATION.

  METHOD: zm_handle_hotspot_compras.
    PERFORM z_hotspot_compras USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_COMPRAS
*&---------------------------------------------------------------------*
FORM z_hotspot_compras USING    p_e_row_id    TYPE  lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no   TYPE  lvc_s_roid.


  DATA: tabix     TYPE sy-tabix,
        tabix_aux TYPE sy-tabix.

  DATA opt TYPE ctu_params.


  CLEAR: tabix, tabix_aux.

  CHECK p_e_row_id-index IS NOT INITIAL.

  READ TABLE it_saida_mm INTO wa_saida_mm INDEX p_e_row_id.
  tabix = sy-tabix.
  IF ( sy-subrc EQ 0 ).
    CASE p_e_column_id.
      WHEN: 'VINCULAR_C'.

        READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
        tabix_aux = sy-tabix.

        IF ( sy-subrc EQ 0 ) AND ( tabix_aux NE tabix ).
          MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
        ELSE.
          PERFORM: selecionar_compras USING wa_saida_mm
                                            p_e_row_id.
        ENDIF.
      WHEN: 'EBELN'.


        READ TABLE it_saida_mm INTO wa_saida_mm INDEX p_e_row_id.
        CLEAR: t_bdc[], t_messtab.
        PERFORM f_bdc_field USING: 'X' 'SAPLMEGUI'           '0014'             ,
                                   ' ' 'BDC_OKCODE'           '=MECHOB'          ,
                                   ' ' 'DYN_6000-LIST'       '1'                ,
                                   'X' 'SAPLMEGUI'           '0002'             ,
                                   ' ' 'BDC_OKCODE'           '=MEOK'            ,
                                   ' ' 'BDC_SUBSCR'           'SAPLMEGUI'        ,
                                   ' ' 'BDC_CURSOR'           'MEPO_SELECT-EBELN',
                                   ' ' 'MEPO_SELECT-EBELN'   wa_saida_mm-ebeln     ,
                                   ' ' 'MEPO_SELECT-BSTYP_F' 'X'.

        opt-dismode = 'E'.
        opt-defsize = 'X'.
        CALL TRANSACTION 'ME23N' USING t_bdc OPTIONS FROM opt MESSAGES INTO t_messtab.

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
FORM selecionar_compras  USING  p_saida_mm STRUCTURE wa_saida_mm
                                p_e_row_id  TYPE lvc_s_row.

  wa_stable-row        = 'X'.
  IF ( p_saida_mm-vincular_c NE 'X' ).
    p_saida_mm-vincular_c = 'X'.
    MODIFY it_saida_mm FROM p_saida_mm INDEX p_e_row_id.
    CALL METHOD wa_alv_compras->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.
    CLEAR: p_saida_mm-vincular_c.
    MODIFY it_saida_mm FROM p_saida_mm INDEX p_e_row_id.
    CALL METHOD wa_alv_compras->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDFORM.                    " SELECIONAR_COMPRAS



*&---------------------------------------------------------------------*
*&      Form  COMPRAS
*&---------------------------------------------------------------------*
FORM compras  USING  i_matnr.

  DATA: wa_zsdt0065 TYPE zsdt0065.

  CLEAR: it_saida_mm[], wa_lifnr, wa_name1, wa_matnr, wa_maktx, wa_meins,
         it_ekbe2[].

  TRY.
      DATA(_pedido) = p_ebeln[ 1 ]-low.
    CATCH cx_sy_itab_line_not_found.
      CLEAR _pedido.
  ENDTRY.

*-CS2019001891 - JT - 02.02.2021 - inicio
  IF it_compra_aux[] IS NOT INITIAL.
    SELECT ebeln ebelp zekkn vgabe
           gjahr belnr buzei menge
      FROM ekbe
      INTO TABLE it_ekbe2
       FOR ALL ENTRIES IN it_compra_aux
     WHERE ebeln = it_compra_aux-ebeln
       AND ebelp = it_compra_aux-ebelp
       AND bewtp = 'E'
       AND bwart = '101'.

    SORT it_ekbe2 BY ebeln ebelp.
  ENDIF.
*-CS2019001891 - JT - 02.02.2021 - fim

  IF p_matnrp[] <> p_matnr[] OR _pedido IS NOT INITIAL.
    LOOP AT it_compra_aux INTO wa_compra_aux. "WHERE MATNR EQ P_MATNR.

      "SELECT SINGLE * FROM ZSDT0065 INTO WA_ZSDT0065 WHERE EBELN EQ WA_COMPRA_AUX-EBELN
      "                                                     AND EBELP EQ WA_COMPRA_AUX-EBELP
      "                                                    AND TIPO  EQ 'C'.
*INICIO - AS - 02/10/2020 - CS2020001072
*      IF ( SY-SUBRC EQ 0 )  AND WA_SAIDA_MM-MENGE - WA_ZSDT0065-QTD_VINC LE 0.
*        CONTINUE.
*      ENDIF.
*FIM - AS - 02/10/2020 - CS2020001072
      wa_saida_mm-name1    = wa_compra_aux-name1.
      wa_saida_mm-ebeln    = wa_compra_aux-ebeln.
      wa_saida_mm-ebelp    = wa_compra_aux-ebelp.
      wa_saida_mm-matnr    = wa_compra_aux-matnr.
      wa_saida_mm-maktx    = wa_compra_aux-maktx.
      wa_saida_mm-werks    = wa_compra_aux-werks.
      wa_saida_mm-bsart    = wa_compra_aux-bsart.
      wa_saida_mm-submi    = wa_compra_aux-submi.
      wa_saida_mm-menge    = wa_compra_aux-menge.

      SELECT SINGLE * FROM zsdt0065 INTO wa_zsdt0065 WHERE ebeln EQ wa_compra_aux-ebeln
                                                       AND ebelp EQ wa_compra_aux-ebelp
                                                       AND tipo  EQ 'C'.

      IF ( sy-subrc EQ 0 ).
*INICIO - AS - 02/10/2020 - CS2020001072
        IF wa_saida_mm-menge - wa_zsdt0065-qtd_vinc LE 0.
          CONTINUE.
        ENDIF.
*FIM - AS - 02/10/2020 - CS2020001072
        wa_saida_mm-qtd_vinc_mm = wa_zsdt0065-qtd_vinc.
        wa_saida_mm-saldo_mm    = wa_saida_mm-menge - wa_zsdt0065-qtd_vinc.

      ELSE.
        wa_saida_mm-saldo_mm    = wa_saida_mm-menge.
      ENDIF.

*-CS2019001891 - JT - 02.02.2021 - inicio
      CLEAR l_saldo_entr.
      LOOP AT it_ekbe2 INTO wa_ekbe2 WHERE ebeln = wa_compra_aux-ebeln
                                       AND ebelp = wa_compra_aux-ebelp.
        l_saldo_entr = l_saldo_entr + wa_ekbe2-menge.
      ENDLOOP.
      wa_saida_mm-saldo_entr = wa_compra_aux-menge - l_saldo_entr.
*-CS2019001891 - JT - 02.02.2021 - fim

      wa_saida_mm-netwr    = wa_compra_aux-netwr.
      wa_saida_mm-waers    = wa_compra_aux-waers.
      wa_saida_mm-netpr    = wa_compra_aux-netpr.

      IF ( wa_compra_aux-meins EQ 'BAG' ).
        wa_saida_mm-meins    = 'SAC'.
      ELSE.
        wa_saida_mm-meins = wa_compra_aux-meins.
      ENDIF.

      wa_saida_mm-vincular_c = ''.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_compra_aux-lifnr
        IMPORTING
          output = wa_lifnr.

      wa_saida_mm-lifnr = wa_lifnr.
      wa_name1          = wa_compra_aux-name1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_compra_aux-matnr
        IMPORTING
          output = wa_matnr.

      wa_maktx = wa_compra_aux-maktx.
      wa_meins = wa_compra_aux-meins.

      APPEND wa_saida_mm TO it_saida_mm.
      CLEAR wa_saida_mm.
    ENDLOOP.

  ELSE.

    LOOP AT it_compra_aux INTO wa_compra_aux WHERE matnr EQ i_matnr.

      wa_saida_mm-name1    = wa_compra_aux-name1.
      wa_saida_mm-ebeln    = wa_compra_aux-ebeln.
      wa_saida_mm-ebelp    = wa_compra_aux-ebelp.
      wa_saida_mm-matnr    = wa_compra_aux-matnr.
      wa_saida_mm-maktx    = wa_compra_aux-maktx.
      wa_saida_mm-werks    = wa_compra_aux-werks.
      wa_saida_mm-bsart    = wa_compra_aux-bsart.
      wa_saida_mm-submi    = wa_compra_aux-submi.
      wa_saida_mm-menge    = wa_compra_aux-menge.

      SELECT SINGLE * FROM zsdt0065 INTO wa_zsdt0065 WHERE ebeln EQ wa_compra_aux-ebeln
                                                       AND ebelp EQ wa_compra_aux-ebelp
                                                       AND tipo  EQ 'C'.

      IF ( sy-subrc EQ 0 ).
        wa_saida_mm-qtd_vinc_mm = wa_zsdt0065-qtd_vinc.
        wa_saida_mm-saldo_mm    = wa_saida_mm-menge - wa_zsdt0065-qtd_vinc.

      ELSE.
        wa_saida_mm-saldo_mm    = wa_saida_mm-menge.
      ENDIF.

*-CS2019001891 - JT - 02.02.2021 - inicio
      CLEAR l_saldo_entr.
      LOOP AT it_ekbe2 INTO wa_ekbe2 WHERE ebeln = wa_compra_aux-ebeln
                                       AND ebelp = wa_compra_aux-ebelp.
        l_saldo_entr = l_saldo_entr + wa_ekbe2-menge.
      ENDLOOP.
      wa_saida_mm-saldo_entr = wa_compra_aux-menge - l_saldo_entr.
*-CS2019001891 - JT - 02.02.2021 - fim

      wa_saida_mm-netwr    = wa_compra_aux-netwr.
      wa_saida_mm-waers    = wa_compra_aux-waers.
      wa_saida_mm-netpr    = wa_compra_aux-netpr.

      IF ( wa_compra_aux-meins EQ 'BAG' ).
        wa_saida_mm-meins    = 'SAC'.
      ELSE.
        wa_saida_mm-meins = wa_compra_aux-meins.
      ENDIF.

      wa_saida_mm-vincular_c = ''.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_compra_aux-lifnr
        IMPORTING
          output = wa_lifnr.

      wa_saida_mm-lifnr = wa_lifnr.
      wa_name1          = wa_compra_aux-name1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_compra_aux-matnr
        IMPORTING
          output = wa_matnr.

      wa_maktx = wa_compra_aux-maktx.
      wa_meins = wa_compra_aux-meins.

      IF ( wa_saida_mm-saldo_mm EQ 0 AND ckc_pedido = abap_false ) OR wa_saida_mm-saldo_mm GT 0.
        APPEND wa_saida_mm TO it_saida_mm.
      ENDIF.
      CLEAR wa_saida_mm.
    ENDLOOP.
  ENDIF.

  CLEAR: wa_compra_aux, wa_saida_mm, wa_zsdt0065.
ENDFORM.                    " COMPRAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_COMPRAS
*&---------------------------------------------------------------------*
FORM criar_catalog_compras .

  CLEAR: it_fcat_compras[].

  PERFORM fcat_compras USING:
          'VINCULAR_C'  'Seleção'        '7'   'X' ''   ''    ''     'X', " Seleção
          'EBELN'       'Nr. Pedido'     '10'  'X' ''   ''    ''     '' , " N. PEDIDO
          'EBELP'       'Item Pedido'    '5'   ''  ''   ''    ''     '' , " Item
          'MATNR'       'Material'       '10'  ''  'X'  ''    ''     '',  " Material
          'MAKTX'       'Descr.'         '10'  ''  ''   ''    ''     '',  " Descrição
          'WERKS'       'Centro F.'      '9'   ''  ''   ''    ''     '' , " Centro Fornecedor
          'BSART'       'Tp Pedido'      '9'   ''  ''   ''    ''     '' , "Tipo do pedido
          'SUBMI'       'RFQ - Coletiva' '13'  ''  ''   ''    ''     '' , "RFQ - Coletiva
          'MENGE'       'Quantidade'     '13'  ''  ''   'X'   'C500'  '' , " Quantidade
          'QTD_VINC_MM' 'Qtd. Vinculado' '13'  ''  ''   'X'   'C500'  '' , " Qtd. Vinculado
          'SALDO_MM'    'Saldo'          '11'  ''  ''   'X'   'C400'  '' , " Saldo
          'SALDO_ENTR'  'Sdo Entrada'    '11'  ''  ''   'X'   'C400'  '' . " Saldo entrada

ENDFORM.                    " CRIAR_CATALOG_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  FCAT_COMPRAS
*&---------------------------------------------------------------------*
FORM fcat_compras USING p_campo TYPE c
                        p_desc  TYPE c
                        p_tam   TYPE c
                        p_hot   TYPE c
                        p_zero  TYPE c
                        p_sum   TYPE c
                        p_cor   TYPE c
                        p_check TYPE c.

  DATA: wl_fcat_compras TYPE lvc_s_fcat.

  wl_fcat_compras-tabname   = 'IT_SAIDA_MM'.
  wl_fcat_compras-fieldname = p_campo.
  wl_fcat_compras-scrtext_l = p_desc.
  wl_fcat_compras-scrtext_m = p_desc.
  wl_fcat_compras-scrtext_s = p_desc.
  wl_fcat_compras-hotspot   = p_hot.
  wl_fcat_compras-do_sum    = p_sum.
  wl_fcat_compras-outputlen = p_tam.
  wl_fcat_compras-no_zero   = p_zero.
  wl_fcat_compras-emphasize = p_cor.
  wl_fcat_compras-checkbox  = p_check.

  APPEND wl_fcat_compras TO it_fcat_compras.

ENDFORM.                    " FCAT_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_COMPRAS
*&---------------------------------------------------------------------*
FORM criar_alv_compras .

  DATA: wa_layout_compras      TYPE lvc_s_layo.

  "DATA: WA_EVENT_COMPRAS    TYPE REF TO LCL_EVENT_COMPRAS.

  IF ( wa_cont_compras IS INITIAL ).

    CREATE OBJECT wa_cont_compras
      EXPORTING
        container_name              = 'CONTAINER_COMPRAS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF ( wa_alv_compras IS INITIAL )  AND ( NOT wa_cont_compras IS INITIAL ).

      CREATE OBJECT wa_alv_compras
        EXPORTING
          i_parent          = wa_cont_compras
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.
  ENDIF.


  " CREATE OBJECT WA_EVENT_COMPRAS.

  CALL METHOD wa_alv_compras->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_compras
    CHANGING
      it_outtab                     = it_saida_mm
      it_fieldcatalog               = it_fcat_compras
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD wa_alv_compras->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  "SET HANDLER: WA_EVENT_COMPRAS->ZM_HANDLE_HOTSPOT_COMPRAS FOR WA_ALV_COMPRAS.
  SET HANDLER: lcl_event_compras=>zm_handle_hotspot_compras FOR wa_alv_compras.


ENDFORM.                    " CRIAR_ALV_COMPRAS

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_vendas DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      zm_handle_hotspot_vendas FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_compras DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_vendas IMPLEMENTATION.

  METHOD: zm_handle_hotspot_vendas.
    PERFORM z_hotspot_vendas USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_VENDAS
*&---------------------------------------------------------------------*
FORM z_hotspot_vendas  USING    p_e_row_id TYPE  lvc_s_row
                                p_e_column_id TYPE lvc_s_col
                                p_es_row_no TYPE lvc_s_roid.

  DATA: tabix     TYPE sy-tabix,
        tabix_aux TYPE sy-tabix.

  CLEAR: wa_saida_sd, tabix, tabix_aux.

  CHECK p_e_row_id-index IS NOT INITIAL.

  READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.
  tabix = sy-tabix.
  IF ( sy-subrc EQ 0 ).
    CASE p_e_column_id.
      WHEN: 'VINCULAR_V'.
        READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.
        tabix_aux = sy-tabix.
        IF ( sy-subrc EQ 0 ) AND ( tabix_aux NE tabix ).
          MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Existe registro marcado.'.
        ELSE.
          PERFORM: selecionar_vendas USING  wa_saida_sd
                                            p_e_row_id.
        ENDIF.

      WHEN: 'VBELN'.
        READ TABLE it_saida_sd INTO wa_saida_sd INDEX p_e_row_id.
        SET PARAMETER ID 'AUN' FIELD wa_saida_sd-vbeln."'VL' FIELD WA_SAIDA_SD-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN."'VL03N' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.
ENDFORM.                    " Z_HOTSPOT_VENDAS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VENDAS
*&---------------------------------------------------------------------*
FORM selecionar_vendas  USING    p_saida_vendas STRUCTURE wa_saida_sd
                                 p_e_row_id     TYPE lvc_s_row.

  IF ( p_saida_vendas-vincular_v NE 'X' ).

    p_saida_vendas-vincular_v = 'X'.
    MODIFY it_saida_sd FROM p_saida_vendas INDEX p_e_row_id.
    CALL METHOD wa_alv_vendas->refresh_table_display.
    wa_edit_vinc = 'X'.

  ELSE.
    CLEAR: p_saida_vendas-vincular_v.
    MODIFY it_saida_sd FROM p_saida_vendas INDEX p_e_row_id.
    CALL METHOD wa_alv_vendas->refresh_table_display.
  ENDIF.

ENDFORM.                    " SELECIONAR_VENDAS

*&---------------------------------------------------------------------*
*&      Form  VENDAS
*&---------------------------------------------------------------------*
FORM vendas  USING    p_matnr.

  DATA: wa_zsdt0065 TYPE zsdt0065.

  CLEAR: it_saida_sd[].

  LOOP AT it_venda INTO wa_venda WHERE matnr EQ p_matnr.

    CLEAR: wa_saida_sd-status.

    LOOP AT it_0026 WHERE vbeln EQ wa_venda-vbeln.

      READ TABLE it_zib_bsik WITH KEY belnr = it_0026-docnum.

      READ TABLE it_bsik WITH KEY bukrs = it_zib_bsik-bukrs
                                  belnr = it_zib_bsik-belnr
                                  gjahr = it_zib_bsik-gjahr.

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

      ENDIF .

    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      wa_saida_sd-status = 'A'.

    ENDIF.

    READ TABLE it_vbkd WITH KEY vbeln = wa_venda-vbeln.

    IF sy-subrc IS INITIAL .
      READ TABLE it_t052u WITH KEY zterm = it_vbkd-zterm.

      IF sy-subrc IS INITIAL.
        CONCATENATE it_vbkd-zterm '-' it_t052u-text1 INTO wa_saida_sd-zterm SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF p_nrocg IS NOT INITIAL.
      wa_saida_sd-nro_sol  = wa_venda-nro_sol.
      wa_saida_sd-seq      = wa_venda-seq.
    ELSEIF p_nrosl IS NOT INITIAL.
      wa_saida_sd-nro_sol = p_nrosl-low.
      wa_saida_sd-seq     = p_seqsl-low.
    ENDIF.


    wa_saida_sd-kunnr    = wa_venda-kunnr.
    wa_saida_sd-name1    = wa_venda-name1.
    wa_saida_sd-vbeln    = wa_venda-vbeln.
    wa_saida_sd-posnr    = wa_venda-posnr.
    wa_saida_sd-matnr    = wa_venda-matnr.
    wa_saida_sd-arktx    = wa_venda-arktx.
    wa_saida_sd-kwmeng   = wa_venda-kwmeng.
    wa_saida_sd-werks    = wa_venda-werks.
    wa_saida_sd-lgort    = wa_venda-lgort.
    wa_saida_sd-charg    = wa_venda-charg.


    SELECT SINGLE * FROM zsdt0065 INTO wa_zsdt0065 WHERE vbeln    EQ wa_venda-vbeln
                                                     AND posnr    EQ wa_venda-posnr
                                                     AND tipo     EQ 'V'
                                                     AND nro_cg   EQ p_nrocg-low
                                                     AND nro_sol  EQ wa_saida_sd-nro_sol
                                                     AND seq      EQ wa_saida_sd-seq.

    IF ( sy-subrc EQ 0 ).
      wa_saida_sd-qtd_vinc_sd = wa_zsdt0065-qtd_vinc.
      wa_saida_sd-saldo_sd    = wa_venda-kwmeng - wa_zsdt0065-qtd_vinc.
    ELSE.
      wa_saida_sd-saldo_sd    = wa_venda-kwmeng.
    ENDIF.

    wa_saida_sd-netwr    = wa_venda-netwr.
    wa_saida_sd-waerk    = wa_venda-waerk.
    wa_saida_sd-kbetr    = wa_venda-kbetr.

    wa_saida_sd-vkbur    = wa_venda-vkbur.

    IF ( wa_venda-vrkme EQ 'BAG' ).
      wa_saida_sd-vrkme    = 'SAC'.
    ELSE.
      wa_saida_sd-vrkme = wa_venda-vrkme.
    ENDIF.

    wa_saida_sd-vincular_v = ''.

    APPEND wa_saida_sd TO it_saida_sd.
    CLEAR: wa_venda, wa_saida_sd.
  ENDLOOP.

ENDFORM.                    " VENDAS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VENDAS
*&---------------------------------------------------------------------*
FORM criar_catalog_vendas .

  REFRESH: it_fcat_vendas[].
  PERFORM fcat_vendas USING:

        'VINCULAR_V'  'Seleção'             '7' 'X'   ''  ''   ''       'X', " Seleção
        'KUNNR'       'Código Cliente'      '10' ''   'X' ''  ''        '', " Codigo do Cliente
        'NAME1'       'Cliente'             '35' ''   ''  ''  ''        '', " Cliente
        'VKBUR'       'Escritorio de Venda' '10' ''   ''  ''  ''        '', " Contrato
        'VBELN'       'Ordem de Venda'      '10' 'X'  ''  ''  ''        '', " Contrato
        'POSNR'       'Item'                '7'  ''   ''  ''  ''        '', " Item
        'WERKS'       'Centro F.'           '9'  ''   ''  ''  ''        '', " Item
        'KWMENG'      'Quantidade'          '13' ''   ''  'X' 'C500'    '' , " Quantidade
        'QTD_VINC_SD' 'Qtd. Vinculada'      '13' ''   ''  'X' 'C500'    '' , " VALOR
        'SALDO_SD'    'Saldo'               '13' ''   ''  'X' 'C400'    '' .

ENDFORM.                    " CRIAR_CATALOG_VENDAS
*&---------------------------------------------------------------------*
*&      Form  FCAT_VENDAS
*&---------------------------------------------------------------------*
FORM fcat_vendas USING   p_campo TYPE c
                         p_desc  TYPE c
                         p_tam   TYPE c
                         p_hot   TYPE c
                         p_zero  TYPE c
                         p_sum   TYPE c
                         p_cor   TYPE c
                         p_check TYPE c.

  DATA: wl_fcat_vendas TYPE lvc_s_fcat.

  wl_fcat_vendas-tabname   = 'IT_SAIDA_SD'.
  wl_fcat_vendas-fieldname = p_campo.
  wl_fcat_vendas-scrtext_l = p_desc.
  wl_fcat_vendas-scrtext_m = p_desc.
  wl_fcat_vendas-scrtext_s = p_desc.
  wl_fcat_vendas-hotspot   = p_hot.
  wl_fcat_vendas-do_sum    = p_sum.
  wl_fcat_vendas-outputlen = p_tam.
  wl_fcat_vendas-no_zero   = p_zero.
  wl_fcat_vendas-emphasize = p_cor.
  wl_fcat_vendas-checkbox  = p_check.

  APPEND wl_fcat_vendas TO it_fcat_vendas.

ENDFORM.                    " FCAT_VENDAS

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VENDAS
*&---------------------------------------------------------------------*
FORM criar_alv_vendas .

  DATA: wa_layout_vendas     TYPE lvc_s_layo.

  "DATA: WA_EVENT_VENDAS    TYPE REF TO LCL_EVENT_VENDAS.


  IF wa_cont_vendas IS INITIAL.

    CREATE OBJECT wa_cont_vendas
      EXPORTING
        container_name              = 'CONTAINER_VENDAS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT wa_alv_vendas
      EXPORTING
        i_parent          = wa_cont_vendas
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  "CREATE OBJECT WA_EVENT_VENDAS.

  CALL METHOD wa_alv_vendas->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_vendas
    CHANGING
      it_outtab                     = it_saida_sd
      it_fieldcatalog               = it_fcat_vendas
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD wa_alv_vendas->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  SET HANDLER: WA_EVENT_VENDAS->ZM_HANDLE_HOTSPOT_VENDAS FOR WA_ALV_VENDAS.
  SET HANDLER: lcl_event_vendas=>zm_handle_hotspot_vendas FOR wa_alv_vendas.

ENDFORM.                    " CRIAR_ALV_VENDAS
*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
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

ENDFORM.                    "f_bdc_field
*&---------------------------------------------------------------------*
*&      Form  TAB_VINCULACAO
*&---------------------------------------------------------------------*
FORM tab_vinculacao .

  CLEAR: wa_saida_mm, wa_saida_sd.

  READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.

  IF NOT ( wa_saida_mm-saldo_mm IS INITIAL ) OR ( wa_saida_mm-saldo_mm EQ 0 ).
    wa_saldo_c = wa_saida_mm-saldo_mm.
  ELSE.
    wa_saldo_c = wa_saida_mm-qtd_vinc_mm.
  ENDIF.

*-CS2019001891 - JT - 02.02.2021 - inicio
  CLEAR l_saldo_entr.
  LOOP AT it_ekbe2 INTO wa_ekbe2 WHERE ebeln = wa_saida_mm-ebeln
                                   AND ebelp = wa_saida_mm-ebelp.
    l_saldo_entr = l_saldo_entr + wa_ekbe2-menge.
  ENDLOOP.
*  wa_saldo_e = wa_saldo_c - l_saldo_entr. "Comentado -  15/06/2021 - AOENNING / IR063432
  wa_saldo_e = wa_saida_mm-menge - l_saldo_entr. "AOENNING / IR063432


  IF wa_saldo_e < 0.
    wa_saldo_e = 0.
  ENDIF.
*-CS2019001891 - JT - 02.02.2021 - fim

  READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.

  IF NOT ( wa_saida_sd-saldo_sd IS INITIAL ) OR ( wa_saida_sd-saldo_sd EQ 0 ).
    wa_saldo_v = wa_saida_sd-saldo_sd.
  ELSE.
    wa_saldo_v = wa_saida_sd-qtd_vinc_sd.
  ENDIF.

  wa_data = sy-datum.

  PERFORM:   selecionar_vinculacao,
             criar_catalog_vinculacao,
             criar_alv_vinculacao.


ENDFORM.                    " TAB_VINCULACAO


*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Compra e Vendas - Vinculação
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.

ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Compra e Vendas - Vinculação
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.

ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       Liberação de Embarque - Insumos - Tela Vinculação de Compra e Venda
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.

  DATA(_fields) = lcl_screen_fields=>get_screen_fields( ).

  LOOP AT _fields INTO DATA(_field).
    LOOP AT SCREEN .

      "//Group 1 tratatives;
      IF ( screen-group1 = _field-group ).
        screen-active = _field-value.
      ENDIF.

      "//Group 2 tratatives;
      "//...

      MODIFY SCREEN.
    ENDLOOP.
  ENDLOOP.

  IF ( wa_opcao NE 'X' ).
    LOOP AT SCREEN.
      IF ( screen-name EQ 'BTN_VINCULAR' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.

      IF ( screen-name EQ 'BTN_SALVAR' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT SCREEN.
      IF ( screen-name EQ 'BTN_VINCULAR' ).
        screen-output = '1'.
        screen-input  = '1'.
        MODIFY SCREEN.
      ENDIF.

      IF ( screen-name EQ 'BTN_SALVAR' ).
        screen-output = '1'.
        screen-input  = '1'.
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
MODULE pai_0400 INPUT.

  DATA: wa_zsdt0062_email TYPE zsdt0062,
        var_ok            TYPE c.


  CASE sy-ucomm.
    WHEN: 'ENTER'.
      IF ( wa_data     IS INITIAL ) OR
         ( wa_qtd_vinc IS INITIAL ).
        MESSAGE w899(mm) WITH 'Informar os campos obrigatorios'.
      ELSE.
        IF ( wa_qtd_vinc > wa_saldo_c  ) .
          MESSAGE w899(mm) WITH 'Qtd. maior que o saldo de compra'.
        ELSEIF ( wa_qtd_vinc > wa_saldo_v  ) .
          MESSAGE w899(mm) WITH 'Qtd. maior que o saldo de venda'.
*-CS2019001891 - JT - 02.02.2021 - inicio
*        ELSEIF ( wa_qtd_vinc > wa_saldo_e  ) .
*          MESSAGE w899(mm) WITH 'Qtd. de vinculação está superior ao saldo '
*                                'disponível para entrada no Pedido/Item selecionado.'.
*-CS2019001891 - JT - 02.02.2021 - fim
        ELSE.
          IF lcl_screen_fields=>is_active( 'GR1' ) = abap_true.

            IF ( wa_lote_origem IS INITIAL ) OR ( wa_deposito_origem IS INITIAL ).
              MESSAGE w899(mm) WITH 'Informar os campos obrigatorios'.
            ELSE.
              wa_opcao = 'X'.
            ENDIF.

          ELSE.
            wa_opcao = 'X'.
          ENDIF.

        ENDIF.
      ENDIF.

    WHEN: 'BTN_VINCULAR'.
      PERFORM: preencher_vinculacao.

    WHEN: 'BTN_SALVAR'.
      IF  NOT ( it_saida_vinc[] IS INITIAL ) .
        CLEAR: wa_opcao.
        PERFORM: salvar_vinculacao.
      ELSE.
        MESSAGE w899(mm) WITH 'Não existe vinculação.'.
      ENDIF.

    WHEN: 'BTN_EMAIL'.


      CALL METHOD wa_alv_vinc->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.


      LOOP AT tl_rows INTO sl_rows.

        READ TABLE it_saida_vinc INTO wa_saida_vinc INDEX  sl_rows-index.

        SELECT SINGLE * FROM zsdt0062 INTO wa_zsdt0062_email  WHERE ebeln    EQ wa_saida_vinc-ebeln
                                                                AND ebelp    EQ wa_saida_vinc-ebelp
                                                                AND vbeln    EQ wa_saida_vinc-vbeln
                                                                AND posnr    EQ wa_saida_vinc-posnr
                                                                AND sol_vinc EQ wa_saida_vinc-sol_vinc.

        IF ( sy-subrc EQ 0 ).

          IF ( wa_zsdt0062_email-envio_email EQ 'G' ).
            MESSAGE w899(mm) DISPLAY LIKE 'E' WITH 'E-mail já enviado.'.
            CLEAR: var_ok.
          ELSE.
            var_ok = 'X'.
          ENDIF.
        ELSE.
          var_ok = 'X'.
        ENDIF.
      ENDLOOP.


      IF ( var_ok EQ 'X' ).
        PERFORM: caixa_email.
        CALL SCREEN 0500 STARTING AT 2 1 ENDING AT 95 25.
      ENDIF.


  ENDCASE.
ENDMODULE.                 " PAI_0400  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VINCULACAO
*&---------------------------------------------------------------------*
FORM criar_catalog_vinculacao .

  PERFORM preenche_fcat_vinculacao USING:

      'STATUS'    'Status'              '5'  ''  ''  ''  '' '' 'C',
      'EBELN'     'Nº.Pedido'           '10' ''  ''  ''  '' '' '',
      'EBELP'     'Item'                '3'  ''  ''  ''  '' '' ''.

  IF lcl_screen_fields=>is_active( 'GR1' ) = abap_true.
    PERFORM preenche_fcat_vinculacao USING:
      'LGORT_ORIG' 'Dep. Origem'        '8'  ''  ''  ''  '' '' '',
      'CHARG_ORIG' 'Lote Origem'        '8'  ''  ''  ''  '' '' '',
      'MBLNR'      'Documento Mat.'     '9'  ''  ''  ''  '' '' ''.
  ENDIF.

  PERFORM preenche_fcat_vinculacao USING:
        'LIFNR'     'Cod.Forn.'           '6'  ''  ''  ''  '' '' '',
        'NAME'      'Desc.Forn.'          '10' ''  ''  ''  '' '' '',
        'KUNNR'     'Cliente'             '6'  ''  ''  ''  '' '' '',
        'NAME1'     'Nome'                '10' ''  ''  ''  '' '' '',
        'VKBUR'     'Escr.Venda'          '10' ''  ''  ''  '' '' '',
        'VBELN'     'Nº.OV.'              '10' ''  ''  ''  '' '' '',
        'POSNR'     'Item'                '3'  ''  ''  ''  '' '' '',
        'MATNR'     'Material'            '12' ''  ''  ''  '' '' '',
        'QTD_V'     'Qtd.Vinc'            '10' ''  ''  ''  '' '' '',
*      'LOCAL_EMB' 'Local de Embarque'   '10' ''  ''  ''  '' '' '',
        'DATA '     'Data.Vinc'           '10' ''  ''  ''  '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_FCAT_VINCULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM preenche_fcat_vinculacao   USING  p_campo TYPE c
                                       p_desc  TYPE c
                                       p_tam   TYPE c
                                       p_hot   TYPE c
                                       p_zero  TYPE c
                                       p_sum   TYPE c
                                       p_cor   TYPE c
                                       p_check TYPE c
                                       p_just  TYPE c.

  DATA: wl_fcat_vinc TYPE lvc_s_fcat.

  wl_fcat_vinc-tabname   = 'IT_SAIDA_VINC'.
  wl_fcat_vinc-fieldname = p_campo.
  wl_fcat_vinc-scrtext_l = p_desc.
  wl_fcat_vinc-scrtext_m = p_desc.
  wl_fcat_vinc-scrtext_s = p_desc.
  wl_fcat_vinc-hotspot   = p_hot.
  wl_fcat_vinc-do_sum    = p_sum.
  wl_fcat_vinc-outputlen = p_tam.
  wl_fcat_vinc-no_zero   = p_zero.
  wl_fcat_vinc-emphasize = p_cor.
  wl_fcat_vinc-checkbox  = p_check.
  wl_fcat_vinc-just      = p_just.


  APPEND wl_fcat_vinc TO it_fcat_vinc.


ENDFORM.                    " PREENCHE_FCAT_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VINCULACAO
*&---------------------------------------------------------------------*
FORM criar_alv_vinculacao .

  DATA: gs_variant  TYPE disvariant.
  DATA: wa_stable   TYPE lvc_s_stbl.
  gs_variant-report  = sy-repid.

  IF ( wa_cont_vinc IS INITIAL ).

    CREATE OBJECT wa_cont_vinc
      EXPORTING
        container_name              = 'CONTAINER_VINCULACAO'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT wa_alv_vinc
      EXPORTING
        i_parent          = wa_cont_vinc
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    wa_stable-row             = 'X'.
    wa_layout_vinc-sel_mode   = 'A'.

    CALL METHOD wa_alv_vinc->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_vinc
        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = it_saida_vinc
        it_fieldcatalog               = it_fcat_vinc
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER: lcl_event_vinculados=>handle_set_toolbar  FOR wa_alv_vinc,
                 lcl_event_vinculados=>handle_user_command FOR wa_alv_vinc.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_VINCULACAO
*&---------------------------------------------------------------------*
FORM preencher_vinculacao .

*  IF ( WA_DATA IS INITIAL ) OR ( WA_LOCAL IS INITIAL ) OR ( WA_QTD_VINC IS INITIAL ).
  IF ( wa_data IS INITIAL     ) OR
     ( wa_qtd_vinc IS INITIAL ).
    MESSAGE w899(mm) WITH 'Informar os campos obrigatorios'.
  ELSE.

    IF lcl_screen_fields=>is_active( 'GR1' ) = abap_true.
      IF ( wa_lote_origem IS INITIAL OR wa_deposito_origem IS INITIAL ).
        MESSAGE w899(mm) WITH 'Informar os campos obrigatorios'.
        EXIT.
      ENDIF.
    ENDIF.

    IF ( wa_qtd_vinc > wa_saldo_c  ) .
      MESSAGE w899(mm) WITH 'Qtd. maior que o saldo de compra'.

    ELSEIF ( wa_qtd_vinc > wa_saldo_v ).
      MESSAGE w899(mm) WITH 'Qtd. maior que o saldo de venda'.

*-CS2019001891 - JT - 02.02.2021 - inicio
*    ELSEIF ( wa_qtd_vinc > wa_saldo_e  ) .
*      MESSAGE w899(mm) WITH 'Qtd. de vinculação está superior ao saldo '
*                            'disponível para entrada no Pedido/Item selecionado.'.
*-CS2019001891 - JT - 02.02.2021 - fim

    ELSE.
      CLEAR: wa_saida_mm, wa_saida_sd.

      READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.

      IF ( sy-subrc EQ 0 ).

        READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.

        IF ( sy-subrc EQ 0 ).

          "Informações MM
          wa_saida_vinc-matnr_c = wa_saida_mm-matnr.
          wa_saida_vinc-ebeln   = wa_saida_mm-ebeln.
          wa_saida_vinc-ebelp   = wa_saida_mm-ebelp.
          wa_saida_vinc-lifnr   = wa_saida_mm-lifnr.
          wa_saida_vinc-name    = wa_saida_mm-name1.
          wa_saida_vinc-lgort_orig = wa_deposito_origem.
          wa_saida_vinc-charg_orig = wa_lote_origem.

          "Informações SD
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_saida_sd-kunnr
            IMPORTING
              output = wa_saida_vinc-kunnr.

          wa_saida_vinc-nro_sol = wa_saida_sd-nro_sol.
          wa_saida_vinc-seq     = wa_saida_sd-seq.

          wa_saida_vinc-name1 = wa_saida_sd-name1.
          wa_saida_vinc-vkbur = wa_saida_sd-vkbur.


          wa_saida_vinc-vbeln = wa_saida_sd-vbeln.
          wa_saida_vinc-posnr = wa_saida_sd-posnr.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_saida_sd-matnr
            IMPORTING
              output = wa_saida_vinc-matnr.

          wa_saida_vinc-local_emb = wa_local.
          wa_saida_vinc-qtd_v = wa_qtd_vinc.
          wa_saldo_c = wa_saldo_c - wa_qtd_vinc.
          wa_saldo_v = wa_saldo_v - wa_qtd_vinc.
          wa_saida_vinc-data = wa_data.

          wa_saida_vinc-status     = icon_led_red.

          APPEND wa_saida_vinc TO it_saida_vinc.

          IF NOT ( it_saida_vinc[] IS INITIAL ).

            IF NOT ( wa_saida_mm-qtd_vinc_mm IS INITIAL ).
              wa_saida_mm-qtd_vinc_mm = wa_saida_mm-qtd_vinc_mm +   wa_qtd_vinc.
              wa_saida_mm-saldo_mm    = wa_saida_mm-menge - wa_saida_mm-qtd_vinc_mm.
            ELSE.
              wa_saida_mm-qtd_vinc_mm = wa_qtd_vinc.
              wa_saida_mm-saldo_mm    = wa_saldo_c.
            ENDIF.
            CLEAR: wa_data, wa_local, wa_qtd_vinc, wa_deposito_origem, wa_lote_origem.
            CALL METHOD wa_alv_vinc->refresh_table_display.
          ENDIF.
        ELSE.
          MESSAGE w899(mm) WITH 'Selecionar um Item SD.'.
        ENDIF.
      ELSE.
        MESSAGE w899(mm) WITH 'Selecionar um Item MM.'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " PREENCHER_VINCULACAO

FORM check_material_balance USING
                              material
                              quantidade
                              deposito
                              lote
                              centro
                             CHANGING
                               return TYPE bapiret2_t
                             RAISING
                              cx_abap_util_exception.


  SELECT SINGLE clabs
    FROM mchb
    INTO @DATA(_quantidade_livre)
   WHERE matnr = @material
     AND werks = @centro
     AND lgort = @deposito
     AND charg = @lote.


  IF ( quantidade > _quantidade_livre ).
    APPEND VALUE #( type   = 'E' id = 'Z_MM' number = 004 message_v1 = deposito message_v2 = lote ) TO return.
    RAISE EXCEPTION TYPE cx_abap_util_exception.
  ENDIF.
ENDFORM.

FORM material_movement_output USING
                                data_documento
                                descricao
                                material
                                centro
                                deposito
                                lote
                                quantidade
                                tipo_movimento
                                centro_destino
                                deposito_destino
                                material_destino
                                lote_destino
                              CHANGING
                                document TYPE bapi2017_gm_head_ret
                                return   TYPE bapiret2_t
                               RAISING
                                cx_abap_util_exception.

  DATA movement_header TYPE bapi2017_gm_head_01.
  DATA movement_items  TYPE bapi2017_gm_item_create_t.
  DATA errors          TYPE TABLE OF bapiret2.


  TRY.
      "//Check available balance in material;
      PERFORM check_material_balance
        USING
          material
          quantidade
          deposito
          lote
          centro
        CHANGING
          return.

      movement_header = VALUE #(
          pstng_date = data_documento
          doc_date   = data_documento
          "HEADER_TXT = DESCRICAO
       ).

*---> 19/06/2023 - Migração S4 - DG
      DATA: lv_MATERIAL_long TYPE mara-matnr,
            lv_MATERIAL(18)  TYPE c.
      DATA(v_len) = strlen( material ).

      IF v_len > 18.
        lv_MATERIAL_long = material.
      ELSE.
        lv_MATERIAL       = material. "#EC CI_FLDEXT_OK[2215424]
      ENDIF.
*<--- 19/06/2023 - Migração S4 - DG

      movement_items = VALUE #( (
*---> 19/06/2023 - Migração S4 - DG
*           material   = material
          material      = lv_MATERIAL
          MATERIAL_long = lv_MATERIAL_long
*<--- 19/06/2023 - Migração S4 - DG

          plant      = centro
          stge_loc   = deposito
          batch      = lote
          move_type  = tipo_movimento
          entry_qnt  = quantidade
          move_plant = centro_destino
          move_stloc = deposito_destino
          move_mat   = material_destino
          move_batch = lote_destino
          "VENDOR     = FORNECEDOR
      ) ).

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = movement_header
          goodsmvt_code    = '04'
        IMPORTING
          materialdocument = document-mat_doc
          matdocumentyear  = document-doc_year
        TABLES
          goodsmvt_item    = movement_items
          return           = errors.

      DELETE errors WHERE type <> 'E'.

      IF ( errors IS INITIAL ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        APPEND LINES OF errors TO return.
        RAISE EXCEPTION TYPE cx_abap_util_exception.
      ENDIF.

    CATCH cx_abap_util_exception.
      RAISE EXCEPTION TYPE cx_abap_util_exception.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVAR_VINCULACAO
*&---------------------------------------------------------------------*
FORM salvar_vinculacao .

  DATA: wa_zsdt0062_insr TYPE zsdt0062,
        wa_zsdt0065_insr TYPE zsdt0065,
        p_zid            TYPE numc10,
        tabix            TYPE sy-tabix.

  DATA errors   TYPE TABLE OF bapiret2.
  DATA idx_mm   TYPE sy-tabix.
  DATA idx_sd   TYPE sy-tabix.
  DATA document TYPE bapi2017_gm_head_ret.

  CLEAR wa_saida_vinc.
  LOOP AT it_saida_vinc INTO wa_saida_vinc.

    CLEAR: wa_zsdt0062_insr.
    SELECT SINGLE * FROM zsdt0062 INTO wa_zsdt0062_insr WHERE ebeln       EQ wa_saida_vinc-ebeln
                                                          AND ebelp       EQ wa_saida_vinc-ebelp
                                                          AND vbeln       EQ wa_saida_vinc-vbeln
                                                          AND posnr       EQ wa_saida_vinc-posnr
                                                          AND envio_email EQ wa_saida_vinc-status_aux.

    IF ( sy-subrc EQ 0 ).
      CONTINUE.
    ENDIF.

    CLEAR: tabix.
    tabix = sy-tabix.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZSOL_VINC'
      IMPORTING
        number      = p_zid.

    IF ( sy-subrc EQ 0 ).

      wa_zsdt0062_insr-sol_vinc       = p_zid.
      wa_zsdt0062_insr-ebeln          = wa_saida_vinc-ebeln.
      wa_zsdt0062_insr-ebelp          = wa_saida_vinc-ebelp.
      wa_zsdt0062_insr-vbeln          = wa_saida_vinc-vbeln.
      wa_zsdt0062_insr-posnr          = wa_saida_vinc-posnr.
      wa_zsdt0062_insr-dt_vinc        = wa_saida_vinc-data.

      IF p_nrocg IS NOT INITIAL.
        wa_zsdt0062_insr-nro_cg     = p_nrocg-low.
        wa_zsdt0062_insr-nro_sol    = wa_saida_vinc-nro_sol.
        wa_zsdt0062_insr-seq        = wa_saida_vinc-seq.
      ELSEIF p_nrosl IS NOT INITIAL.
        wa_zsdt0062_insr-nro_sol        = p_nrosl-low.
        wa_zsdt0062_insr-seq        = p_seqsl-low.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinc-matnr
        IMPORTING
          output = wa_zsdt0062_insr-matnr.

      wa_zsdt0062_insr-qtd_vinc       = wa_saida_vinc-qtd_v.

      wa_zsdt0062_insr-local_embarque = wa_saida_vinc-local_emb.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinc-lifnr
        IMPORTING
          output = wa_zsdt0062_insr-lifnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinc-kunnr
        IMPORTING
          output = wa_zsdt0062_insr-kunnr.


      wa_zsdt0062_insr-usnam          = sy-uname.
      wa_zsdt0062_insr-dt_atual       = sy-datum.
      wa_zsdt0062_insr-hora_atul      = sy-uzeit.
      wa_zsdt0062_insr-envio_email    = 'P'.
      wa_zsdt0062_insr-status         = 'L'.
      wa_zsdt0062_insr-lgort          = wa_saida_vinc-lgort_orig.
      wa_zsdt0062_insr-charg          = wa_saida_vinc-charg_orig.
      wa_zsdt0062_insr-ematn          = wa_saida_vinc-matnr_c.

      "//Realiza transferência de material do pedido p/
      "//o material da venda;
      "// comentado para atendimento do IR065329 alinhamento com Quevedo e Arianne
*      IF ( wa_saida_vinc-lgort_orig IS NOT INITIAL )
*     AND ( wa_saida_vinc-charg_orig IS NOT INITIAL )
*     and ( wa_saida_vinc-lgort_orig = 'ITAC' ).
*
*        TRY.
*            PERFORM material_movement_output
*              USING
*                sy-datum
*                ''
*                wa_saida_mm-matnr
*                wa_saida_mm-werks
*                wa_saida_vinc-lgort_orig
*                wa_saida_vinc-charg_orig
*                wa_saida_vinc-qtd_v
*                '311'
*                wa_saida_sd-werks
*                wa_saida_sd-lgort
*                wa_saida_sd-matnr
*                wa_saida_sd-charg
*              CHANGING
*                document
*                errors.
*
*            wa_zsdt0062_insr-mblnr  = document-mat_doc.
*            wa_zsdt0062_insr-mjahr  = document-doc_year.
*
*          CATCH cx_abap_util_exception.
*        ENDTRY.
*
*      ENDIF.

      INSERT INTO zsdt0062 VALUES wa_zsdt0062_insr.

      IF ( sy-subrc EQ 0 ).
        wa_saida_vinc-status     = icon_mail.
        wa_saida_vinc-status_aux = 'P'.
        wa_saida_vinc-sol_vinc   = p_zid.
        wa_saida_vinc-mblnr      = document-mat_doc.

        MODIFY it_saida_vinc FROM wa_saida_vinc INDEX tabix.
        CALL METHOD wa_alv_vinc->refresh_table_display.

        CLEAR: wa_saida_mm, wa_saida_sd.

        READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
        idx_mm = sy-tabix.

        IF NOT ( wa_saida_mm-qtd_vinc_mm IS INITIAL ).
          wa_saida_mm-qtd_vinc_mm = wa_saida_mm-qtd_vinc_mm + wa_saida_vinc-qtd_v.
          wa_saida_mm-saldo_mm    = wa_saida_mm-menge - wa_saida_mm-qtd_vinc_mm.
        ELSE.
          wa_saida_mm-qtd_vinc_mm = wa_saida_vinc-qtd_v.
          wa_saida_mm-saldo_mm    = wa_saldo_c.
        ENDIF.

        MODIFY it_saida_mm FROM wa_saida_mm INDEX idx_mm.
        IF ( sy-subrc EQ 0 ).
          CALL METHOD wa_alv_compras->refresh_table_display.
        ENDIF.


        READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.
        idx_sd = sy-tabix.

        IF NOT ( wa_saida_sd-qtd_vinc_sd IS INITIAL ).
          wa_saida_sd-qtd_vinc_sd = wa_saida_sd-qtd_vinc_sd + wa_saida_vinc-qtd_v.
          wa_saida_sd-saldo_sd    = wa_saida_sd-kwmeng - wa_saida_sd-qtd_vinc_sd.
        ELSE.
          wa_saida_sd-qtd_vinc_sd = wa_saida_vinc-qtd_v.
          wa_saida_sd-saldo_sd    = wa_saldo_v.
        ENDIF.
        MODIFY it_saida_sd FROM wa_saida_sd INDEX idx_sd.
        IF ( sy-subrc EQ 0 ).
          CALL METHOD wa_alv_vendas->refresh_table_display.
        ENDIF.

        "Inserir os Itens Compra/Venda
        "Compra
        SELECT SINGLE * FROM zsdt0065 INTO wa_zsdt0065_insr WHERE ebeln EQ wa_saida_mm-ebeln
                                                              AND ebelp EQ wa_saida_mm-ebelp
                                                              AND tipo  EQ 'C'.

        IF ( sy-subrc EQ 0 ).

          READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.

          UPDATE zsdt0065 SET sol_vinc = p_zid
                              qtd_vinc = wa_saida_mm-qtd_vinc_mm

                          WHERE ebeln EQ wa_zsdt0062_insr-ebeln
                            AND ebelp EQ wa_zsdt0062_insr-ebelp
                            AND tipo  EQ 'C'.



        ELSE.

          READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
          wa_zsdt0065_insr-sol_vinc = p_zid.
          wa_zsdt0065_insr-ebeln    = wa_saida_mm-ebeln.
          wa_zsdt0065_insr-ebelp    = wa_saida_mm-ebelp.
          wa_zsdt0065_insr-qtd_vinc = wa_saida_mm-qtd_vinc_mm.
          wa_zsdt0065_insr-tipo     = 'C'.

          INSERT INTO zsdt0065 VALUES wa_zsdt0065_insr.
          CLEAR wa_zsdt0065_insr.
        ENDIF.

        "Vendas

        IF p_nrocg IS NOT INITIAL.
          DATA(nro_carga_a) = p_nrocg-low.
          DATA(nro_sol_a) = wa_saida_vinc-nro_sol.
          DATA(seq_a)     = wa_saida_vinc-seq.
        ELSEIF p_nrosl IS NOT INITIAL.
          nro_sol_a   = p_nrosl-low.
          seq_a       = p_seqsl-low.
        ENDIF.


        SELECT SINGLE * FROM zsdt0065 INTO wa_zsdt0065_insr WHERE vbeln EQ wa_saida_sd-vbeln
                                                              AND posnr EQ wa_saida_sd-posnr
                                                              AND tipo  EQ 'V'
                                                              AND nro_cg  EQ nro_carga_a
                                                              AND nro_sol EQ nro_sol_a
                                                              AND seq     EQ seq_a.
        IF ( sy-subrc EQ 0 ).
          READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.



          UPDATE zsdt0065 SET sol_vinc = p_zid
                              qtd_vinc =  wa_saida_sd-qtd_vinc_sd
                              WHERE vbeln EQ wa_zsdt0065_insr-vbeln
                                AND posnr EQ wa_zsdt0065_insr-posnr
                                AND tipo  EQ 'V'
                                AND nro_cg  EQ nro_carga_a
                                AND nro_sol EQ nro_sol_a
                                AND seq     EQ seq_a.


        ELSE.

          READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.
          wa_zsdt0065_insr-sol_vinc = p_zid.
          wa_zsdt0065_insr-vbeln    = wa_saida_sd-vbeln.
          wa_zsdt0065_insr-posnr    = wa_saida_sd-posnr.
          wa_zsdt0065_insr-qtd_vinc = wa_saida_sd-qtd_vinc_sd.
          wa_zsdt0065_insr-tipo     = 'V'.
          wa_zsdt0065_insr-nro_cg     = nro_carga_a.
          wa_zsdt0065_insr-nro_sol    = nro_sol_a.
          wa_zsdt0065_insr-seq        = seq_a.

          INSERT INTO zsdt0065 VALUES wa_zsdt0065_insr.

          CLEAR: wa_zsdt0065_insr.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: wa_saida_vinc, wa_zsdt0062_insr, p_zid.
  ENDLOOP.

  IF NOT errors IS INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = errors.
  ENDIF.

  CALL METHOD wa_alv_vinc->refresh_table_display.
  MESSAGE s899(mm) WITH 'Vinculação salva.'.

ENDFORM.                    " SALVAR_VINCULACAO
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VINCULACAO
*&---------------------------------------------------------------------*
FORM selecionar_vinculacao .

  DATA: wa_lfa1 TYPE lfa1,
        wa_kna1 TYPE kna1,
        wa_vbak TYPE vbak.

  REFRESH: it_zsdt0062[], it_saida_vinc[].
  CLEAR: wa_zsdt0062, wa_saida_mm, wa_saida_sd.

  "Compras
  READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.
  "Vendas
  READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.

  SELECT * " SOL_VINC EBELN EBELP VBELN POSNR DT_VINC MATNR QTD_VINC LOCAL_EMBARQUE USNAM DT_ATUAL HORA_ATUL ENVIO_EMAIL LIFNR KUNNR MJAHR MBLNR
    FROM zsdt0062
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0062
  WHERE ebeln   EQ wa_saida_mm-ebeln
    AND ebelp   EQ wa_saida_mm-ebelp
    AND vbeln   EQ wa_saida_sd-vbeln
    AND posnr   EQ wa_saida_sd-posnr
    AND nro_cg  EQ p_nrocg-low
    AND nro_sol EQ p_nrosl-low
    AND seq     EQ p_seqsl-low
    AND status  EQ 'L'.

  LOOP AT it_zsdt0062 INTO wa_zsdt0062.

    wa_saida_vinc-sol_vinc  = wa_zsdt0062-sol_vinc.
    wa_saida_vinc-ebeln     = wa_zsdt0062-ebeln.
    wa_saida_vinc-ebelp     = wa_zsdt0062-ebelp.
    wa_saida_vinc-mblnr     = wa_zsdt0062-mblnr.
    wa_saida_vinc-lgort_orig = wa_zsdt0062-lgort.
    wa_saida_vinc-charg_orig = wa_zsdt0062-charg.

    SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_zsdt0062-lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0062-lifnr
      IMPORTING
        output = wa_saida_vinc-lifnr.
    wa_saida_vinc-name      = wa_lfa1-name1.

    wa_saida_vinc-vbeln     = wa_zsdt0062-vbeln.
    wa_saida_vinc-posnr     = wa_zsdt0062-posnr.
    SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ wa_zsdt0062-kunnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0062-kunnr
      IMPORTING
        output = wa_saida_vinc-kunnr.

    wa_saida_vinc-name1     = wa_kna1-name1.

    wa_saida_vinc-data      = wa_zsdt0062-dt_vinc.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0062-matnr
      IMPORTING
        output = wa_saida_vinc-matnr.

    wa_saida_vinc-qtd_v     = wa_zsdt0062-qtd_vinc.
    wa_saida_vinc-local_emb = wa_zsdt0062-local_embarque.

    SELECT SINGLE * FROM vbak INTO wa_vbak WHERE vbeln EQ wa_zsdt0062-vbeln.
    wa_saida_vinc-vkbur = wa_vbak-vkbur.


    CASE wa_zsdt0062-envio_email.
      WHEN: 'P'.
        wa_saida_vinc-status     = icon_mail.
        wa_saida_vinc-status_aux = 'P'.
      WHEN: 'G'.
        wa_saida_vinc-status     = icon_okay.
        wa_saida_vinc-status_aux = 'G'.
    ENDCASE.

    APPEND wa_saida_vinc TO it_saida_vinc.
    CLEAR: wa_saida_vinc, wa_zsdt0062.

  ENDLOOP.
  IF ( wa_alv_vinc IS INITIAL ).
    PERFORM:   criar_catalog_vinculacao,
               criar_alv_vinculacao.

    CALL METHOD wa_alv_vinc->refresh_table_display.

  ELSE.
    CALL METHOD wa_alv_vinc->refresh_table_display.
  ENDIF.

ENDFORM.                    " SELECIONAR_VINCULACAO
**&---------------------------------------------------------------------*
**&      Form  CRIAR_EDITOR
**&---------------------------------------------------------------------*
FORM criar_editor .

  IF ( container_editor IS INITIAL ).

    CREATE OBJECT container_editor
      EXPORTING
        container_name              = 'CONTAINER_EDITOR'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT cl_editor
      EXPORTING
        parent                     = container_editor
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.

  ENDIF.
  CALL METHOD cl_editor->set_text_as_r3table
    EXPORTING
      table           = lt_tab
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " CRIAR_EDITOR

*&---------------------------------------------------------------------*
*&      Module  PBO_0600  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0600 OUTPUT.
  IF wa_zsdt0074-bname IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'BTN_ESTORNAR' OR
*--------CS2019001891 - JT - 01.02.2021 - inicio
         screen-name = 'BTN_EDITAR'.
*--------CS2019001891 - JT - 01.02.2021 - fim
        screen-input     = 0.
        screen-invisible = 0.
        MODIFY SCREEN.
*       EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDMODULE.                 " PBO_0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0600  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0600 INPUT.
  DATA: w_answer(1).

  CASE sy-ucomm.
    WHEN: 'BTN_EMAIL_VINCULADOS'.
      CLEAR: tl_rows, sl_rows.
      CALL METHOD wa_alv_vinculados->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      PERFORM: caixa_email.
      CALL SCREEN 0500 STARTING AT 2 1 ENDING AT 95 25.
    WHEN 'BTN_ESTORNAR'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          text_question         = 'Tem certeza que deseja ESTORNAR?'
          text_button_1         = 'Sim'(100)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(101)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
*         USERDEFINED_F1_HELP   = ' '
          start_column          = 25
          start_row             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          answer                = w_answer
*               TABLES
*         PARAMETER             =
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.
        PERFORM f_estorno.
      ENDIF.

    WHEN 'BTN_NR_FORNECEDOR'.

      CLEAR: tl_rows, sl_rows, vg_nr_forn.

      CALL METHOD wa_alv_vinculados->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      IF tl_rows IS NOT INITIAL.
        LOOP AT tl_rows INTO sl_rows.
          READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.
          APPEND wa_saida_vinculados TO it_saida_vinculados_nr_forn.
        ENDLOOP.
      ENDIF.

      IF it_saida_vinculados_nr_forn IS NOT INITIAL.
        CALL SCREEN 0700 STARTING AT 5 5 ENDING AT 56 9.
      ELSE.
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecione uma linha.'.
      ENDIF.

*---CS2019001891 - JT - 01.02.2021 - inicio
    WHEN 'BTN_EDITAR'.

      FREE: it_saida_vinculados_qtde.
      CLEAR: tl_rows, sl_rows, vg_qt_vinc.

      CALL METHOD wa_alv_vinculados->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      IF tl_rows IS NOT INITIAL.
        LOOP AT tl_rows INTO sl_rows.
          READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.
          APPEND wa_saida_vinculados TO it_saida_vinculados_qtde.
        ENDLOOP.
      ENDIF.

      DESCRIBE TABLE it_saida_vinculados_qtde LINES l_lines_qtvinc.

      vg_qt_vinc = wa_saida_vinculados-qtd_v.
*     CONDENSE vg_qt_vinc.

      IF l_lines_qtvinc = 1.
        CALL SCREEN 0800 STARTING AT 5 5 ENDING AT 56 9.
      ELSE.
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecione uma linha.'.
      ENDIF.
*---CS2019001891 - JT - 01.02.2021 - fim

  ENDCASE.

ENDMODULE.                 " PAI_0600  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VINCULADOS
*&---------------------------------------------------------------------*
FORM selecionar_vinculados .

  DATA: wa_lfa1 TYPE lfa1,
        wa_kna1 TYPE kna1,
        wa_vbak TYPE vbak.

  CLEAR: wa_saida_mm, wa_saida_sd, wa_zsdt0062.
  REFRESH: it_zsdt0062[].

  READ TABLE it_saida_mm INTO wa_saida_mm WITH KEY vincular_c = 'X'.

  IF ( sy-subrc EQ 0 ).

    SELECT * "SOL_VINC EBELN EBELP VBELN POSNR DT_VINC MATNR QTD_VINC LOCAL_EMBARQUE USNAM DT_ATUAL HORA_ATUL ENVIO_EMAIL LIFNR KUNNR
      FROM zsdt0062
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0062
    WHERE ebeln EQ wa_saida_mm-ebeln
      AND ebelp EQ wa_saida_mm-ebelp
      AND status EQ 'L'.

  ENDIF.

  SELECT SINGLE *
    FROM zsdt0074
    INTO wa_zsdt0074
    WHERE bname = sy-uname.

  READ TABLE it_saida_sd INTO wa_saida_sd WITH KEY vincular_v = 'X'.

  IF ( sy-subrc EQ 0 ).

    SELECT * "SOL_VINC EBELN EBELP VBELN POSNR DT_VINC MATNR QTD_VINC LOCAL_EMBARQUE USNAM DT_ATUAL HORA_ATUL ENVIO_EMAIL LIFNR KUNNR NR_FORN
      FROM zsdt0062
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0062
    WHERE vbeln EQ wa_saida_sd-vbeln
      AND posnr EQ wa_saida_sd-posnr
      AND nro_cg  EQ p_nrocg-low
      AND nro_sol EQ wa_saida_sd-nro_sol
      AND seq     EQ wa_saida_sd-seq
      AND status EQ 'L'.

  ENDIF.

  LOOP AT it_zsdt0062 INTO wa_zsdt0062.

    wa_saida_vinculados-sol_vinc   = wa_zsdt0062-sol_vinc.
    wa_saida_vinculados-ebeln      = wa_zsdt0062-ebeln.
    wa_saida_vinculados-ebelp      = wa_zsdt0062-ebelp.
    wa_saida_vinculados-mblnr      = wa_zsdt0062-mblnr.
    wa_saida_vinculados-mjahr      = wa_zsdt0062-mjahr.

    wa_saida_vinculados-nro_sol    = wa_saida_sd-nro_sol.
    wa_saida_vinculados-seq        = wa_saida_sd-seq.

    SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_zsdt0062-lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0062-lifnr
      IMPORTING
        output = wa_saida_vinculados-lifnr.
    wa_saida_vinculados-name      = wa_lfa1-name1.

    wa_saida_vinculados-vbeln     = wa_zsdt0062-vbeln.
    wa_saida_vinculados-posnr     = wa_zsdt0062-posnr.
    SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ wa_zsdt0062-kunnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0062-kunnr
      IMPORTING
        output = wa_saida_vinculados-kunnr.

    wa_saida_vinculados-name1     = wa_kna1-name1.
    wa_saida_vinculados-nr_forn   = wa_zsdt0062-nr_forn.
    wa_saida_vinculados-data      = wa_zsdt0062-dt_vinc.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0062-matnr
      IMPORTING
        output = wa_saida_vinculados-matnr.

    wa_saida_vinculados-qtd_v     = wa_zsdt0062-qtd_vinc.
    wa_saida_vinculados-local_emb = wa_zsdt0062-local_embarque.

    SELECT SINGLE * FROM vbak INTO wa_vbak WHERE vbeln EQ wa_zsdt0062-vbeln.
    wa_saida_vinculados-vkbur = wa_vbak-vkbur.


    CASE wa_zsdt0062-envio_email.
      WHEN: 'P'.
        wa_saida_vinculados-status     = icon_mail.
        wa_saida_vinculados-status_aux = 'P'.
      WHEN: 'G'.
        wa_saida_vinculados-status     = icon_okay.
        wa_saida_vinculados-status_aux = 'G'.
    ENDCASE.

    APPEND wa_saida_vinculados TO it_saida_vinculados.


  ENDLOOP.

ENDFORM.                    " SELECIONAR_VINCULADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VINCULADOS
*&---------------------------------------------------------------------*
FORM criar_alv_vinculados .

  DATA: wa_layout_vinculados TYPE lvc_s_layo,
        wa_stable            TYPE lvc_s_stbl.


  IF wa_cont_vinculados IS INITIAL.

    CREATE OBJECT wa_cont_vinculados
      EXPORTING
        container_name              = 'CONTAINER_VINCULADOS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    PERFORM: criar_fcat_vinculados.

    CREATE OBJECT wa_alv_vinculados
      EXPORTING
        i_parent          = wa_cont_vinculados
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  wa_stable-row                   = 'X'.
  wa_layout_vinculados-sel_mode   = 'A'.

  CALL METHOD wa_alv_vinculados->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_vinculados
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida_vinculados
      it_fieldcatalog               = it_fcat_vinculados
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_VINCULADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_FCAT_VINCULADOS
*&---------------------------------------------------------------------*
FORM criar_fcat_vinculados .


  PERFORM preenche_fcat_vinculados USING:

      'STATUS'    'Status'              '5'  ''  ''  ''  '' '' 'C',
      'EBELN'     'Nº.Pedido'           '10' ''  ''  ''  '' '' '',
      'EBELP'     'Item'                '3'  ''  ''  ''  '' '' '',
      'MBLNR'     'Documento Mat.'      '10' ''  ''  ''  '' '' '',
      'LIFNR'     'Cod.Forn.'           '6'  ''  ''  ''  '' '' '',
      'NAME'      'Desc.Forn.'          '10' ''  ''  ''  '' '' '',
      'KUNNR'     'Cliente'             '6'  ''  ''  ''  '' '' '',
      'NAME1'     'Nome'                '10' ''  ''  ''  '' '' '',
      'VKBUR'     'Escr.Venda'          '10' ''  ''  ''  '' '' '',
      'VBELN'     'Nº.OV.'              '10' ''  ''  ''  '' '' '',
      'POSNR'     'Item'                '3'  ''  ''  ''  '' '' '',
      'MATNR'     'Material'            '12' ''  ''  ''  '' '' '',
      'QTD_V'     'Qtd.Vinc'            '10' ''  ''  ''  '' '' '',
*      'LOCAL_EMB' 'Local de Embarque'   '10' ''  ''  ''  '' '' '',
      'DATA '     'Data.Vinc'           '10' ''  ''  ''  '' '' '',
      'NR_FORN'   'Nr. Int. Fornecedor' '10' ''  ''  ''  '' '' ''.
ENDFORM.                    " CRIAR_FCAT_VINCULADOS
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_FCAT_VINCULADOS
*&---------------------------------------------------------------------*
FORM preenche_fcat_vinculados  USING   p_campo TYPE c
                                       p_desc  TYPE c
                                       p_tam   TYPE c
                                       p_hot   TYPE c
                                       p_zero  TYPE c
                                       p_sum   TYPE c
                                       p_cor   TYPE c
                                       p_check TYPE c
                                       p_just  TYPE c.

  DATA: wl_fcat_vinculados TYPE lvc_s_fcat.

  wl_fcat_vinculados-tabname   = 'IT_SAIDA_VINCULADOS'.
  wl_fcat_vinculados-fieldname = p_campo.
  wl_fcat_vinculados-scrtext_l = p_desc.
  wl_fcat_vinculados-scrtext_m = p_desc.
  wl_fcat_vinculados-scrtext_s = p_desc.
  wl_fcat_vinculados-hotspot   = p_hot.
  wl_fcat_vinculados-do_sum    = p_sum.
  wl_fcat_vinculados-outputlen = p_tam.
  wl_fcat_vinculados-no_zero   = p_zero.
  wl_fcat_vinculados-emphasize = p_cor.
  wl_fcat_vinculados-checkbox  = p_check.
  wl_fcat_vinculados-just      = p_just.


  APPEND wl_fcat_vinculados TO it_fcat_vinculados.

ENDFORM.                    " PREENCHE_FCAT_VINCULADOS

*&---------------------------------------------------------------------*
*&      Form  CAIXA_EMAIL
*&---------------------------------------------------------------------*
FORM caixa_email .

  DATA: BEGIN OF tl_mes OCCURS 0.
          INCLUDE STRUCTURE t247.
  DATA: END OF tl_mes.

  DATA: tl_lfa1 TYPE lfa1,
        tl_kna1 TYPE kna1,
        tl_vbkd TYPE vbkd.

  DATA: qtd_linhas TYPE sy-tabix.

  DATA: mes     TYPE n LENGTH 2,
        txt_mes TYPE c LENGTH 15.

  DATA: it_saida_vinculados_aux TYPE TABLE OF ty_saida_vinc,
        wa_saida_vinculados_aux TYPE ty_saida_vinc.



  REFRESH: it_saida_vinculados_aux[].

  CHECK NOT tl_rows[] IS INITIAL.

  tl_rows_aux[] = tl_rows[].

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language              = sy-langu
    TABLES
      month_names           = tl_mes
    EXCEPTIONS
      month_names_not_found = 1
      OTHERS                = 2.


  CLEAR: wa_saida_vinc, la_tab, lt_tab.
  READ TABLE tl_rows INTO sl_rows INDEX 1.

  IF ( vinculados_email EQ 'X' ).

    it_saida_vinculados_aux[] = it_saida_vinculados[].


    READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.

    CONCATENATE 'AMAGGI - Liberação de Embarque'  wa_saida_vinculados-vbeln  INTO wa_assunto SEPARATED BY space.


    mes = sy-datum+4(2).
    READ TABLE tl_mes INDEX mes.

    CONCATENATE 'Cuiabá - MT,' sy-datum+6(2) 'de' tl_mes-ltx 'de' sy-datum(4) INTO la_tab SEPARATED BY space.
    APPEND la_tab TO lt_tab.
    la_tab = ''.
    APPEND la_tab TO lt_tab.
    la_tab = ''.
    APPEND la_tab TO lt_tab.
    la_tab = 'À'.
    APPEND la_tab TO lt_tab.



    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_vinculados-lifnr
      IMPORTING
        output = wa_saida_vinculados-lifnr.

    SELECT SINGLE * FROM lfa1 INTO tl_lfa1 WHERE lifnr EQ wa_saida_vinculados-lifnr.

    IF ( sy-subrc EQ 0 ).
      la_tab = tl_lfa1-name1.
      APPEND la_tab TO lt_tab.
    ENDIF.


    la_tab = ''.
    APPEND la_tab TO lt_tab.

    la_tab = 'Referente ao(s) Pedido(s) de Compra Nro.'.
    APPEND la_tab TO lt_tab.
    CLEAR: la_tab.
    DESCRIBE TABLE tl_rows LINES qtd_linhas.

    LOOP AT tl_rows_aux INTO sl_rows_aux.

      READ TABLE it_saida_vinculados_aux INTO wa_saida_vinculados_aux INDEX sl_rows_aux-index.

      IF ( qtd_linhas > 1 ).
        CONCATENATE   wa_saida_vinculados_aux-ebeln '|' la_tab   INTO la_tab SEPARATED BY space.
      ELSE.
        la_tab = wa_saida_vinculados_aux-ebeln.
      ENDIF.


*      IF ( QTD_LINHAS > 1 ).
*        CONCATENATE   WA_SAIDA_VINCULADOS-EBELN '|' LA_TAB   INTO LA_TAB SEPARATED BY SPACE.
*      ELSE.
*        LA_TAB = WA_SAIDA_VINCULADOS-EBELN.
*      ENDIF.
    ENDLOOP.
    APPEND la_tab TO lt_tab.


    la_tab = ''.
    APPEND la_tab TO lt_tab.


    la_tab = 'Pela presente, autorizamos o embarque do pedido de venda, desmembrando para o nosso cliente conforme abaixo:'.
    APPEND la_tab TO lt_tab.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_vinculados-kunnr
      IMPORTING
        output = wa_saida_vinculados-kunnr.

    la_tab = ''.
    APPEND la_tab TO lt_tab.

    SELECT SINGLE * FROM kna1 INTO tl_kna1 WHERE kunnr EQ wa_saida_vinculados-kunnr.

    IF ( sy-subrc EQ 0 ).
      la_tab = tl_kna1-name1.
      APPEND la_tab TO lt_tab.

      CONCATENATE 'Fazenda:' tl_kna1-ort02 INTO la_tab SEPARATED BY space.
      APPEND la_tab TO lt_tab.

      IF NOT ( tl_kna1-stcd1 IS INITIAL ).
        CONCATENATE 'CNPJ/CPF:' tl_kna1-stcd1 INTO la_tab SEPARATED BY space.
      ELSE.
        CONCATENATE 'CNPJ/CPF:' tl_kna1-stcd2 INTO la_tab SEPARATED BY space.

      ENDIF.

      APPEND la_tab TO lt_tab.
      CONCATENATE 'Inscr. Estadual:' tl_kna1-stcd3 INTO la_tab SEPARATED BY space.
      APPEND la_tab TO lt_tab.
      la_tab = tl_kna1-stras.
      APPEND la_tab TO lt_tab.
      la_tab = tl_kna1-mcod3.
      APPEND la_tab TO lt_tab.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinculados-vbeln
        IMPORTING
          output = wa_saida_vinculados-vbeln.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinculados-posnr
        IMPORTING
          output = wa_saida_vinculados-posnr.

      SELECT SINGLE * FROM vbkd INTO tl_vbkd WHERE vbeln EQ wa_saida_vinculados-vbeln
                                               AND posnr EQ wa_saida_vinculados-posnr.

      la_tab = ''.
      APPEND la_tab TO lt_tab.

      CONCATENATE 'FRETE:' tl_vbkd-inco1 '-' tl_vbkd-inco2 INTO la_tab SEPARATED BY space.
      APPEND la_tab TO lt_tab.


      la_tab = ''.
      APPEND la_tab TO lt_tab.

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
    ENDIF.

  ELSE.

    READ TABLE it_saida_vinc INTO wa_saida_vinc INDEX sl_rows-index.


    CONCATENATE 'AMAGGI - Liberação de Embarque'  wa_saida_vinc-vbeln  INTO wa_assunto SEPARATED BY space.

    mes = sy-datum+4(2).
    READ TABLE tl_mes INDEX mes.


    CONCATENATE 'Cuiabá - MT,' sy-datum+6(2) 'de' tl_mes-ltx 'de' sy-datum(4) INTO la_tab SEPARATED BY space.
    APPEND la_tab TO lt_tab.
    la_tab = ''.
    APPEND la_tab TO lt_tab.
    la_tab = ''.
    APPEND la_tab TO lt_tab.
    la_tab = 'À'.
    APPEND la_tab TO lt_tab.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_vinc-lifnr
      IMPORTING
        output = wa_saida_vinc-lifnr.

    SELECT SINGLE * FROM lfa1 INTO tl_lfa1 WHERE lifnr EQ wa_saida_vinc-lifnr.

    IF ( sy-subrc EQ 0 ).
      la_tab = tl_lfa1-name1.
      APPEND la_tab TO lt_tab.
    ENDIF.

    la_tab = ''.
    APPEND la_tab TO lt_tab.

    la_tab = 'Referente ao(s) Pedido(s) de Compra Nro.'.
    APPEND la_tab TO lt_tab.
    CLEAR: la_tab.
    DESCRIBE TABLE tl_rows_aux LINES qtd_linhas.

    LOOP AT tl_rows_aux INTO sl_rows_aux.
      IF ( qtd_linhas > 1 ).
        CONCATENATE   wa_saida_vinc-ebeln '|' la_tab   INTO la_tab SEPARATED BY space.
      ELSE.
        la_tab = wa_saida_vinc-ebeln.
      ENDIF.
    ENDLOOP.
    APPEND la_tab TO lt_tab.


    la_tab = ''.
    APPEND la_tab TO lt_tab.
    la_tab = 'Pela presente, autorizamos o embarque do pedido de venda, desmembrando para o nosso cliente conforme abaixo:'.
    APPEND la_tab TO lt_tab.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_vinc-kunnr
      IMPORTING
        output = wa_saida_vinc-kunnr.

    la_tab = ''.
    APPEND la_tab TO lt_tab.

    SELECT SINGLE * FROM kna1 INTO tl_kna1 WHERE kunnr EQ wa_saida_vinc-kunnr.

    IF ( sy-subrc EQ 0 ).
      la_tab = tl_kna1-name1.
      APPEND la_tab TO lt_tab.

      CONCATENATE 'Fazenda:' tl_kna1-ort02 INTO la_tab SEPARATED BY space.
      APPEND la_tab TO lt_tab.
      IF NOT ( tl_kna1-stcd1 IS INITIAL ).
        CONCATENATE 'CNPJ/CPF:' tl_kna1-stcd1 INTO la_tab SEPARATED BY space.
      ELSE.
        CONCATENATE 'CNPJ/CPF:' tl_kna1-stcd2 INTO la_tab SEPARATED BY space.

      ENDIF.
      APPEND la_tab TO lt_tab.

      CONCATENATE 'Inscr. Estadual:' tl_kna1-stcd3 INTO la_tab SEPARATED BY space.
      APPEND la_tab TO lt_tab.
      la_tab = tl_kna1-stras.
      APPEND la_tab TO lt_tab.
      la_tab = tl_kna1-mcod3.
      APPEND la_tab TO lt_tab.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinc-vbeln
        IMPORTING
          output = wa_saida_vinc-vbeln.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_vinc-posnr
        IMPORTING
          output = wa_saida_vinc-posnr.

      SELECT SINGLE * FROM vbkd INTO tl_vbkd WHERE vbeln EQ wa_saida_vinc-vbeln
                                               AND posnr EQ wa_saida_vinc-posnr.

      la_tab = ''.
      APPEND la_tab TO lt_tab.

      CONCATENATE 'FRETE:' tl_vbkd-inco1 '-' tl_vbkd-inco2 INTO la_tab SEPARATED BY space.
      APPEND la_tab TO lt_tab.

      la_tab = ''.
      APPEND la_tab TO lt_tab.
    ENDIF.
  ENDIF.

  la_tab = 'Amaggi Commodities'.
  APPEND la_tab TO lt_tab.
  la_tab = 'Insumos'.
  APPEND la_tab TO lt_tab.
  la_tab = 'Thamara Rodrigues  – (065) 3645-5232'.
  APPEND la_tab TO lt_tab.
  la_tab = 'Arianne Queiroz    – (065) 3645-5443'.
  APPEND la_tab TO lt_tab.
*      LA_TAB = 'Jaqueline          – (065) 3645-5093'.
*      APPEND LA_TAB TO LT_TAB.
  la_tab = 'Juliana Bortoluzzi – (065) 3645-5439'.
  APPEND la_tab TO lt_tab.
  la_tab = 'E-mail: insumos.fertilizantes@amaggi.com.br'.
  APPEND la_tab TO lt_tab.
  la_tab = 'Matriz – Cuiabá-MT'.
  APPEND la_tab TO lt_tab.
  la_tab = 'www.amaggi.com.br'.
  APPEND la_tab TO lt_tab.

ENDFORM.                    " CAIXA_EMAIL
*&---------------------------------------------------------------------*
*&      Module  PBO_0500  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0500 OUTPUT.
  SET PF-STATUS 'PF0500'.
  SET TITLEBAR  'TB0500'.
  PERFORM: criar_editor.
ENDMODULE.                 " PBO_0500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0500  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0500 INPUT.

  CASE sy-ucomm.
    WHEN 'OK'.
      PERFORM: enviar_email.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " PAI_0500  INPUT
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
FORM enviar_email .


  DATA: lt_email TYPE STANDARD TABLE OF char0241,
        la_email LIKE LINE OF lt_email.

  DATA: it_destinatario TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE,
        it_assunto      TYPE sodocchgi1,
        it_texto        TYPE STANDARD TABLE OF soli WITH HEADER LINE,
        tg_texto        TYPE TABLE OF tline,
        wg_texto        TYPE tline.

  DATA: valor_str TYPE string,
        qtd_menge TYPE brgew_15,
        l_name    TYPE thead-tdname.

  DATA: wa_makt    TYPE makt,
        wa_ihrez   TYPE ekko,
        w_zsdt0062 TYPE zsdt0062,
        w_zsdt0082 TYPE zsdt0082.

  IF ( wa_email IS INITIAL ).
    MESSAGE w899(mm) DISPLAY LIKE 'E' WITH 'Informar um e-mail'.
  ELSE.


    CALL METHOD cl_editor->get_text_as_r3table
      IMPORTING
        table  = lt_email
      EXCEPTIONS
        OTHERS = 1.

    IF NOT ( lt_email IS INITIAL ).

      REFRESH: it_destinatario[], it_texto[].
      CLEAR: it_assunto.

      it_destinatario-rec_type = 'U'.
      it_destinatario-receiver = wa_email.
      APPEND it_destinatario.

      it_assunto-obj_name  = 'Liberação de Insumos - Maggi'.
      it_assunto-obj_langu = sy-langu.
      it_assunto-obj_descr = wa_assunto.

      it_texto = '<!DOCTYPE html>'.
      APPEND it_texto.
      it_texto = '<html>'.
      APPEND it_texto.
      it_texto = '<head>'.
      APPEND it_texto.
      it_texto = '<style type="text/css">'.
      APPEND it_texto.

      "Tabela do E-mail
      it_texto = '#tabela table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
      APPEND it_texto.
      it_texto = '#tabela th { width: 90px; font-size: 12px; background-color: #93DB70; color: #000000; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
      APPEND it_texto.
      it_texto = '#tabela td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
      APPEND it_texto.
      it_texto = '#msg { font-size: 14px; font-style: "Tahoma, Geneva, sans-serif"; }'.
      APPEND it_texto.


      it_texto = '</style>'.
      APPEND it_texto.
      it_texto = '</head>'.

      APPEND it_texto.
      it_texto = '<body lang="pt-br">'.
      APPEND it_texto.

      it_texto = '<div id="msg">'.
      APPEND it_texto.

      LOOP AT lt_email INTO la_email.

        CONCATENATE la_email '<br/>' INTO it_texto.
        APPEND it_texto.
      ENDLOOP.

      it_texto = '</div>'.
      APPEND it_texto.

      it_texto = '<br/>'.
      APPEND it_texto.
      it_texto = '<br/>'.
      APPEND it_texto.

*-CS2019001891 - JT - 02.02.2021 - inicio
      REFRESH: tl_rows[].
      CLEAR: sl_rows, w_zsdt0062, w_zsdt0082.

      IF wa_alv_vinculados IS NOT INITIAL.
        CALL METHOD wa_alv_vinculados->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.
      ELSE.
        CALL METHOD wa_alv_vinc->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.
      ENDIF.

      READ TABLE tl_rows             INTO sl_rows INDEX 1.
      READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.

      SELECT SINGLE *
               FROM zsdt0062
               INTO w_zsdt0062
              WHERE sol_vinc = wa_saida_vinculados-sol_vinc
                AND ebeln    = wa_saida_vinculados-ebeln
                AND ebelp    = wa_saida_vinculados-ebelp
                AND vbeln    = wa_saida_vinculados-vbeln
                AND posnr    = wa_saida_vinculados-posnr.

      IF sy-subrc = 0.
        SELECT SINGLE nr_rot
                 FROM zsdt0082
                 INTO w_zsdt0082-nr_rot
                WHERE nro_sol = w_zsdt0062-nro_sol
                  AND seq     = w_zsdt0062-seq
                  AND vbeln   = w_zsdt0062-vbeln
                  AND posnr   = w_zsdt0062-posnr.

        FREE: tg_texto.

        l_name = w_zsdt0082-nr_rot.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'ZROT'
            language                = sy-langu
            name                    = l_name
            object                  = 'ZSDROTEIRO'
          TABLES
            lines                   = tg_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF tg_texto[] IS NOT INITIAL.
          CONCATENATE 'ROTEIRO:' '<br/>' INTO it_texto.
          APPEND it_texto.
          LOOP AT tg_texto INTO wg_texto.
            CONCATENATE wg_texto-tdline '<br/>'
                   INTO it_texto.
            APPEND it_texto.
          ENDLOOP.
          it_texto = '<br/>'.
          APPEND it_texto.
          it_texto = '<br/>'.
          APPEND it_texto.
        ENDIF.
      ENDIF.
*-CS2019001891 - JT - 02.02.2021 - inicio

      it_texto = '<div id="tabela">'.
      APPEND it_texto.

      it_texto = '<table>'.
      APPEND it_texto.

      " Cabeçalho
      it_texto = '<tr>'.
      APPEND it_texto.
      it_texto = '<th>Ped.Compra</th>'.
      APPEND it_texto.
      it_texto = '<th>Item</th>'.
      APPEND it_texto.
      it_texto = '<th>Nro.OV</th>'.
      APPEND it_texto.
      it_texto = '<th>Item</th>'.
      APPEND it_texto.
      it_texto = '<th>Material</th>'.
      APPEND it_texto.
      it_texto = '<th>Descrição</th>'.
      APPEND it_texto.
      it_texto = '<th>Quantidade</th>'.
      APPEND it_texto.
      it_texto = '<th>Local Embarque</th>'.
      APPEND it_texto.
      it_texto = '<th>Pedido de Compra</th>'.
      APPEND it_texto.
      it_texto = '</tr>'.
      APPEND it_texto.

      CLEAR: wa_saida_vinc, wa_saida_vinculados.
      "READ TABLE tl_rows INTO sl_rows INDEX 1.

      IF ( vinculados_email EQ 'X' ).

        REFRESH: tl_rows[].
        CLEAR: sl_rows.

        CALL METHOD wa_alv_vinculados->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.


        LOOP AT tl_rows INTO sl_rows.


          READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.

          it_texto = '<tr>'.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinculados-ebeln '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinculados-ebelp '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinculados-vbeln '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinculados-posnr '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinculados-matnr '</td>' INTO it_texto.
          APPEND it_texto.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_saida_vinculados-matnr
            IMPORTING
              output = wa_saida_vinculados-matnr.

          SELECT SINGLE * FROM makt INTO wa_makt WHERE matnr EQ wa_saida_vinculados-matnr.

          CONCATENATE '<td>' wa_makt-maktx '</td>' INTO it_texto.
          APPEND it_texto.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_saida_vinculados-matnr
            IMPORTING
              output = wa_saida_vinculados-matnr.

          it_texto = '<td>'.
          APPEND it_texto.
          it_texto = wa_saida_vinculados-qtd_v.
          APPEND it_texto.
          it_texto = '</td>'.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinculados-local_emb '</td>' INTO it_texto.
          APPEND it_texto.

          SELECT SINGLE * FROM ekko INTO wa_ihrez WHERE ebeln EQ wa_saida_vinculados-ebeln.
          CONCATENATE '<td>' wa_ihrez-ihrez '</td>' INTO it_texto.
          APPEND it_texto.

          it_texto = '</tr>'.
          APPEND it_texto.

        ENDLOOP.

      ELSE.

        REFRESH: tl_rows[].
        CLEAR: sl_rows.

        CALL METHOD wa_alv_vinc->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.

        LOOP AT tl_rows INTO sl_rows.

          READ TABLE it_saida_vinc INTO wa_saida_vinc INDEX sl_rows-index.

          it_texto = '<tr>'.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinc-ebeln '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinc-ebelp '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinc-vbeln '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinc-posnr '</td>' INTO it_texto.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinc-matnr '</td>' INTO it_texto.
          APPEND it_texto.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_saida_vinc-matnr
            IMPORTING
              output = wa_saida_vinc-matnr.

          SELECT SINGLE * FROM makt INTO wa_makt WHERE matnr EQ wa_saida_vinc-matnr.

          CONCATENATE '<td>' wa_makt-maktx '</td>' INTO it_texto.
          APPEND it_texto.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_saida_vinc-matnr
            IMPORTING
              output = wa_saida_vinc-matnr.

          it_texto = '<td>'.
          APPEND it_texto.
          it_texto = wa_saida_vinc-qtd_v.
          APPEND it_texto.
          it_texto = '</td>'.
          APPEND it_texto.
          CONCATENATE '<td>' wa_saida_vinc-local_emb '</td>' INTO it_texto.
          APPEND it_texto.

          SELECT SINGLE * FROM ekko INTO wa_ihrez WHERE ebeln EQ wa_saida_vinc-ebeln.
          CONCATENATE '<td>' wa_ihrez-ihrez '</td>' INTO it_texto.
          APPEND it_texto.

          it_texto = '</tr>'.
          APPEND it_texto.



        ENDLOOP.

      ENDIF.

      it_texto = '</table>'.
      APPEND it_texto.
      it_texto = '</body>'.
      APPEND it_texto.

      it_texto = '</html>'.
      APPEND it_texto.

      "Enviar E-mail
      CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
        EXPORTING
          document_data              = it_assunto
          document_type              = 'HTM'
        TABLES
          object_content             = it_texto
          receivers                  = it_destinatario
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.

      IF ( sy-subrc EQ 0 ).
        COMMIT WORK.
        SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.


        IF ( vinculados_email EQ 'X' ).


          LOOP AT tl_rows INTO sl_rows.

            READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.
            UPDATE zsdt0062 SET envio_email = 'G'
                            email           = wa_email
                            usermail        = sy-uname
                            dt_email        = sy-datum
                            hr_email        = sy-uzeit
                            WHERE ebeln       EQ wa_saida_vinculados-ebeln
                              AND ebelp       EQ wa_saida_vinculados-ebelp
                              AND vbeln       EQ wa_saida_vinculados-vbeln
                              AND posnr       EQ wa_saida_vinculados-posnr
                              AND envio_email EQ 'P'
                              AND sol_vinc    EQ wa_saida_vinculados-sol_vinc.

            wa_saida_vinculados-status = icon_okay.
            MODIFY it_saida_vinculados FROM wa_saida_vinculados INDEX sl_rows-index.

          ENDLOOP.

          CALL METHOD wa_alv_vinculados->refresh_table_display.

          MESSAGE s899(mm) WITH 'E-mail enviado com sucesso.'.
        ELSE.

          LOOP AT tl_rows INTO sl_rows.

            READ TABLE it_saida_vinc INTO wa_saida_vinc INDEX sl_rows-index.
            UPDATE zsdt0062 SET envio_email = 'G'
                            email           = wa_email
                            usermail        = sy-uname
                            dt_email        = sy-datum
                            hr_email        = sy-uzeit
                            WHERE ebeln       EQ wa_saida_vinc-ebeln
                              AND ebelp       EQ wa_saida_vinc-ebelp
                              AND vbeln       EQ wa_saida_vinc-vbeln
                              AND posnr       EQ wa_saida_vinc-posnr
                              AND envio_email EQ 'P'
                              AND sol_vinc    EQ wa_saida_vinc-sol_vinc.

            wa_saida_vinc-status = icon_okay.
            MODIFY it_saida_vinc FROM wa_saida_vinc INDEX sl_rows-index.

          ENDLOOP.

          CALL METHOD wa_alv_vinc->refresh_table_display.
          MESSAGE s899(mm) WITH 'E-mail enviado com sucesso.'.

        ENDIF.

      ELSE.
        MESSAGE w899(mm) WITH 'E-mail não enviado'.
      ENDIF.
    ELSE.
      MESSAGE w899(mm) WITH 'Informar um texto' 'no corpo do e-mail'.
    ENDIF.
  ENDIF.


ENDFORM.                    " ENVIAR_EMAIL

FORM estorno_material_movement USING document doc_year CHANGING return TYPE bapiret2_t.
  DATA document_storno TYPE bapi2017_gm_head_ret.

  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument = document
      matdocumentyear  = doc_year
*     GOODSMVT_PSTNG_DATE       =
*     GOODSMVT_PR_UNAME         =
    IMPORTING
      goodsmvt_headret = document_storno
    TABLES
      return           = return
*     GOODSMVT_MATDOCITEM       =
    .

  DELETE return WHERE type <> 'E'.

  IF NOT return IS INITIAL
  OR document_storno-mat_doc IS INITIAL.
    RAISE EXCEPTION TYPE cx_abap_util_exception.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno .
  CLEAR: tl_rows, sl_rows.
  DATA qtd_linhas TYPE sy-tabix.
  DATA return     TYPE TABLE OF bapiret2.

  DATA: wl_zsdt0062 TYPE zsdt0062,
        wl_zsdt0138 TYPE zsdt0138,
        v_matnr     TYPE vbap-matnr. "Material

  CALL METHOD wa_alv_vinculados->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.
  DESCRIBE TABLE tl_rows LINES qtd_linhas.
  IF  qtd_linhas NE 1.
    MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Selecionar apenas uma linha.'.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.

    CLEAR: wl_zsdt0138.

    READ TABLE it_saida_vinculados INTO wa_saida_vinculados INDEX sl_rows-index.
    SELECT SINGLE * FROM zsdt0062 INTO wl_zsdt0062 WHERE  sol_vinc  EQ wa_saida_vinculados-sol_vinc
                                                     AND  nro_cg    EQ p_nrocg-low
                                                     AND  nro_sol   EQ wa_saida_vinculados-nro_sol
                                                     AND  seq       EQ wa_saida_vinculados-seq.
    IF sy-subrc IS INITIAL.
      IF wl_zsdt0062-status = 'E'.
        MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Lançamento já estornado.'.
        EXIT.
      ELSE.

        SELECT SINGLE * FROM zsdt0138 INTO wl_zsdt0138 WHERE  nro_sol EQ wl_zsdt0062-nro_sol
                                                         AND  seq     EQ wl_zsdt0062-seq
                                                         AND  ebeln   EQ wl_zsdt0062-ebeln
                                                         AND  ebelp   EQ wl_zsdt0062-ebelp
                                                         AND  status  NE 'X'.

        IF sy-subrc IS INITIAL.
          MESSAGE s899(fi) DISPLAY LIKE 'E' WITH 'Pedido já está associado a um embarque!'.
          EXIT.
        ELSE.
          IF ( wl_zsdt0062-mblnr IS NOT INITIAL ).

            TRY.
                PERFORM estorno_material_movement
                  USING
                    wl_zsdt0062-mblnr
                    wl_zsdt0062-mjahr
                  CHANGING
                    return.

              CATCH cx_sy_itab_line_not_found.
                CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
                  EXPORTING
                    it_message = return.

                EXIT.
            ENDTRY.
          ENDIF.

*          UPDATE ZSDT0065 SET
*                QTD_VINC = QTD_VINC - WL_ZSDT0062-QTD_VINC
*          WHERE SOL_VINC = WL_ZSDT0062-SOL_VINC
*          AND   EBELN    = WL_ZSDT0062-EBELN
*          AND   EBELP    = WL_ZSDT0062-EBELP
*          AND   TIPO     = 'C'.

          UPDATE zsdt0065 SET
                  qtd_vinc = qtd_vinc - wl_zsdt0062-qtd_vinc
            WHERE ebeln    = wl_zsdt0062-ebeln
            AND   ebelp    = wl_zsdt0062-ebelp
            AND   tipo     = 'C'.

          IF sy-subrc IS INITIAL.

            UPDATE zsdt0065 SET
                  qtd_vinc = qtd_vinc - wl_zsdt0062-qtd_vinc
            WHERE vbeln   = wl_zsdt0062-vbeln
            AND   posnr   = wl_zsdt0062-posnr
            AND   nro_cg  = wl_zsdt0062-nro_cg
            AND   nro_sol = wl_zsdt0062-nro_sol
            AND   seq     = wl_zsdt0062-seq
            AND   tipo    = 'V'.

            IF sy-subrc IS INITIAL.

              UPDATE zsdt0062 SET status    = 'E'
                                  usnam     = sy-uname
                                  dt_atual  = sy-datum
                                  hora_atul = sy-uzeit
              WHERE  sol_vinc = wa_saida_vinculados-sol_vinc.
            ENDIF.
          ENDIF.


          REFRESH: it_saida_mm, it_saida_sd.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_saida_vinculados-matnr
            IMPORTING
              output = v_matnr.

          PERFORM compras USING  v_matnr.
          PERFORM vendas  USING  v_matnr.
          DELETE it_saida_vinculados INDEX sl_rows-index.
        ENDIF.
      ENDIF.

    ELSE.
      MESSAGE s899(fi) DISPLAY LIKE 'W' WITH 'Lançamento pertece a outra Solicitação/Carga'.
      EXIT.
    ENDIF.

  ENDLOOP.


  CALL METHOD wa_alv_vinculados->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD wa_alv_compras->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD wa_alv_vendas->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.                    " F_ESTORNO
*&---------------------------------------------------------------------*
*&      Form  IT_VBAK_VIA_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_vbak_via_carga .

  DATA: wa_zsdt0133 TYPE zsdt0133.
  DATA: it_zsdt0131 TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0129 TYPE STANDARD TABLE OF zsdt0129.

  SELECT SINGLE *
    FROM zsdt0133
    INTO wa_zsdt0133
    WHERE nro_cg IN p_nrocg.

  IF wa_zsdt0133 IS NOT INITIAL.

    SELECT *
      FROM zsdt0129
      INTO TABLE it_zsdt0129
      WHERE nro_cg EQ wa_zsdt0133-nro_cg
        AND status NE 'X'.

    IF it_zsdt0129 IS NOT INITIAL.

      SELECT *
        FROM zsdt0131
        INTO TABLE it_zsdt0131
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE 'X'.

      IF it_zsdt0131 IS NOT INITIAL.

        SELECT vbeln vkorg  vtweg spart auart  vkbur kunnr waerk erdat faksk lifsk audat knumv
         FROM vbak
         INTO TABLE it_vbak
         FOR ALL ENTRIES IN it_zsdt0131
         WHERE vbeln EQ it_zsdt0131-vbeln
           AND lifsk EQ space.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IT_VBAK_VIA_SOLIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_vbak_via_solic .

  DATA: wa_zsdt0082 TYPE zsdt0082.

  SELECT SINGLE *
    FROM zsdt0082
    INTO wa_zsdt0082
    WHERE nro_sol IN p_nrosl.

  IF wa_zsdt0082 IS NOT INITIAL.

    SELECT vbeln vkorg  vtweg spart auart  vkbur kunnr waerk erdat faksk lifsk audat knumv
      FROM vbak
      INTO TABLE it_vbak
      WHERE vbeln EQ wa_zsdt0082-vbeln
        AND lifsk EQ space.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TABELA_QTD_VIA_SOLIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tabela_qtd_via_solic .

  SELECT *
   FROM vbap
   INTO CORRESPONDING FIELDS OF TABLE it_vbap
   FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln
     AND matnr IN p_matnr
     AND EXISTS ( SELECT *
                    FROM zsdt0082
                    WHERE vbeln   EQ vbap~vbeln
                      AND posnr   EQ vbap~posnr
                      AND nro_sol IN p_nrosl ).

  IF it_vbap IS NOT INITIAL.

    SELECT *
      FROM zsdt0082
      INTO TABLE it_zsdt0082
      FOR ALL ENTRIES IN it_vbap
      WHERE vbeln   EQ it_vbap-vbeln
        AND posnr   EQ it_vbap-posnr
        AND nro_sol IN p_nrosl
        AND seq     IN p_seqsl.

    SELECT *
      FROM zsdt0062
      INTO TABLE it_zsdt0062_soma
      FOR ALL ENTRIES IN it_vbap
      WHERE matnr   EQ it_vbap-matnr
        AND vbeln   EQ it_vbap-vbeln
        AND nro_sol EQ p_nrosl-low
        AND seq     EQ p_seqsl-low
        AND status  NE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TABELA_QTD_VIA_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tabela_qtd_via_carga .

  SELECT vbeln matnr arktx werks zmeng netwr kwmeng vrkme posnr netpr
     FROM vbap
     INTO TABLE it_vbap
     FOR ALL ENTRIES IN it_vbak
     WHERE vbeln EQ it_vbak-vbeln
       AND matnr IN p_matnr
       AND EXISTS ( SELECT *
                      FROM zsdt0131
                      WHERE vbeln  EQ vbap~vbeln
                        AND posnr  EQ vbap~posnr
                        AND status NE 'X' ).

  IF it_vbap IS NOT INITIAL.
    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131
      FOR ALL ENTRIES IN it_vbap
      WHERE vbeln  EQ it_vbap-vbeln
        AND posnr  EQ it_vbap-posnr
        AND EXISTS ( SELECT *
                       FROM zsdt0129
                       WHERE nro_lote EQ zsdt0131~nro_lote
                         AND nro_cg   IN p_nrocg
                         AND status   NE 'X' )
        AND status NE 'X'.

    SELECT *
      FROM zsdt0062
      INTO TABLE it_zsdt0062_soma
      FOR ALL ENTRIES IN it_vbap
      WHERE matnr  EQ it_vbap-matnr
        AND vbeln  EQ it_vbap-vbeln
        AND nro_cg EQ p_nrocg-low
        AND status NE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0700 OUTPUT.
  SET PF-STATUS 'PF0500'.
  SET TITLEBAR  'TB0700'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'PF0800'.
  SET TITLEBAR  'TB0800'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0700 INPUT.

  CASE sy-ucomm.
    WHEN 'OK'.
      PERFORM: adicionar_nr_fornecedor.
      CLEAR: it_saida_vinculados_nr_forn.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      CLEAR: it_saida_vinculados_nr_forn.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.

  DATA: l_erro   TYPE c.

  CASE sy-ucomm.
    WHEN 'OK'.
      CLEAR l_erro.

      PERFORM: f_alterar_quantidade CHANGING l_erro.
      IF l_erro = abap_false.
        CLEAR: it_saida_vinculados_qtde.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN: 'CANC'.
      CLEAR: it_saida_vinculados_qtde.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM f_alterar_quantidade CHANGING p_erro.

  DATA: wl_zsdt0062  TYPE zsdt0062,
        wl_zsdt0065  TYPE zsdt0065,
        vl_menge     TYPE ekpo-menge,
        vl_diferenca TYPE ekpo-menge,
        vl_cont      TYPE i.

  FREE: p_erro,
        vl_menge,
        vl_diferenca.

  SELECT SINGLE menge
           INTO vl_menge
           FROM ekpo
          WHERE ebeln = wa_saida_vinculados-ebeln
            AND ebelp = wa_saida_vinculados-ebelp.

  IF vg_qt_vinc > wa_saida_vinculados-qtd_v. "vl_menge.
    MESSAGE s024(sd) WITH 'Não é possível aumentar a Quantidade Vinculada!' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0062
    INTO wl_zsdt0062
   WHERE sol_vinc = wa_saida_vinculados-sol_vinc
     AND ebeln    = wa_saida_vinculados-ebeln
     AND ebelp    = wa_saida_vinculados-ebelp.

  IF sy-subrc = 0.
    vl_diferenca         = wa_saida_vinculados-qtd_v - vg_qt_vinc.
    wl_zsdt0062-qtd_vinc = vg_qt_vinc.
    MODIFY zsdt0062 FROM wl_zsdt0062.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0065
    INTO wl_zsdt0065
   WHERE ebeln    = wa_saida_vinculados-ebeln
     AND ebelp    = wa_saida_vinculados-ebelp
     AND tipo     = 'C'.

  IF sy-subrc = 0.
    wl_zsdt0065-qtd_vinc = wl_zsdt0065-qtd_vinc - vl_diferenca.
    MODIFY zsdt0065 FROM wl_zsdt0065.
  ENDIF.

  "Para atualizar na tela após alteração
  LOOP AT it_saida_vinculados INTO wa_saida_vinculados.
    vl_cont = vl_cont + 1.
    READ TABLE it_saida_vinculados_qtde WITH KEY sol_vinc = wa_saida_vinculados-sol_vinc TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      wa_saida_vinculados-qtd_v = vg_qt_vinc.
      MODIFY it_saida_vinculados FROM wa_saida_vinculados INDEX vl_cont.
    ENDIF.
  ENDLOOP.

  CALL METHOD wa_alv_vinculados->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

FORM adicionar_nr_fornecedor.

  DATA: wl_zsdt0062 TYPE zsdt0062,
        vl_cont     TYPE i.

  LOOP AT it_saida_vinculados_nr_forn INTO wa_saida_vinculados.

    SELECT SINGLE *
      FROM zsdt0062 INTO wl_zsdt0062
      WHERE sol_vinc = wa_saida_vinculados-sol_vinc
        AND  nro_cg = p_nrocg-low
        AND  nro_sol = p_nrosl-low
        AND  seq = p_seqsl-low.

    wl_zsdt0062-nr_forn = vg_nr_forn.
    MODIFY zsdt0062 FROM wl_zsdt0062.

  ENDLOOP.

  "Para atualizar na tela após alteração
  LOOP AT it_saida_vinculados INTO wa_saida_vinculados.
    vl_cont = vl_cont + 1.
    READ TABLE it_saida_vinculados_nr_forn WITH KEY sol_vinc = wa_saida_vinculados-sol_vinc TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      wa_saida_vinculados-nr_forn = vg_nr_forn.
      MODIFY it_saida_vinculados FROM wa_saida_vinculados INDEX vl_cont.
    ENDIF.
  ENDLOOP.

  CALL METHOD wa_alv_vinculados->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

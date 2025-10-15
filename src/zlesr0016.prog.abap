
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 01.03.2011                                          *
* Objetivo    ...: Relatório de Comparativo de Saída e Chegada         *
* Transação   ...: ZLES0050                                            *
* Autor       ...: Victor Hugo                                         *
************************************************************************
* Histórico de alterações                                              *
*----------------------------------------------------------------------*
* Autor          Request       Data          Descrição                 *
* Marcos Faneli  DEVK937726    20.05.2014    Chamado - 126346          *
*----------------------------------------------------------------------*

REPORT  zlesr0016 MESSAGE-ID sd.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zlest0039,
        likp,
        t001w,
        kna1,
        vbap,
        eket,
        vbak,
        j_1bagnt,
        zlest0041,
        vbfa.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_zlest0039 ,
    mandt             TYPE zlest0039-mandt,            " Mandante
    docnum            TYPE zlest0039-docnum,           " Nº documento
    cnpj              TYPE zlest0039-cnpj,             " CNPJ do cliente
    nfenum            TYPE zlest0039-nfenum,           " Nº NF-e de nove posições
    serie             TYPE zlest0039-serie,            " Código da série
    vbeln             TYPE zlest0039-vbeln,            " Fornecimento
    bukrs             TYPE zlest0039-bukrs,            " Empresa
    werks             TYPE zlest0039-werks,            " Centro
    pontocoleta       TYPE zlest0039-pontocoleta,      " Nº conta do fornecedor
    pontotransb       TYPE zlest0039-pontotransb,      " Nº cliente 1
    pontoentrega      TYPE zlest0039-pontoentrega,     " Nº cliente 1
    datasaida         TYPE zlest0039-datasaida,        " Data de criação do registro
    pesosaida         TYPE zlest0039-pesosaida,        " Peso líquido
    unidadesaida      TYPE zlest0039-unidadesaida,     " Unidade de medida básica
    datatransb        TYPE zlest0039-datatransb,       " Data de criação do registro
    pesotransb        TYPE zlest0039-pesotransb,       " Peso destinado ao vagão
    unidadetransb     TYPE zlest0039-unidadetransb,    " Unidade de medida básica
    dataterminal      TYPE zlest0039-dataterminal,     " Data de criação do registro
    pesoterminal      TYPE zlest0039-pesoterminal,     " Peso destinado ao vagão
    unidadeterminal   TYPE zlest0039-unidadeterminal,  " Unidade de medida básica
    datachegada       TYPE zlest0039-datachegada,      " Data de chegada
    pesochegada       TYPE zlest0039-pesochegada,      " Peso destinado ao vagão
    unidadechegada    TYPE zlest0039-unidadechegada,   " Unidade de medida básica
    ebeln             TYPE zlest0039-ebeln,            " Nº documento de vendas e distribuição
    matnr             TYPE zlest0039-matnr,            " Nº do material
    kunnr             TYPE zlest0039-kunnr,            " Nº cliente 1
    placa_cav         TYPE zlest0039-placa_cav,        " Placa do Veículo
    formaconfirmacao  TYPE zlest0039-formaconfirmacao, " Código de uma posição
    status            TYPE zlest0039-status,           " Status comparativo saidas e chegadas
    data              TYPE zlest0039-data,             " Data de criação do registro
    hora              TYPE zlest0039-hora,             " Hora
    reftyp            TYPE zlest0039-reftyp,           " Tipo referencia
    observacao        TYPE zlest0039-observacao,       " Observação
    status_placa      TYPE zlest0039-status_placa,     " Status da placa
    dias_transito     TYPE zlest0039-dias_transito,    " Dias em Transito
    pesoliquido       TYPE zlest0039-pesoliquido,      " Peso Liquido
    transb_efetivo    TYPE zlest0039-transb_efetivo,   " Local Transb. Efetivo
    ck_estornar_trans TYPE zlest0039-ck_estornar_trans, " Estorno de Transito da Mercadoria
    tp_transgenia     TYPE zlest0039-tp_transgenia,    "Tipo de Transgenia
    chave_nfe         TYPE zlest0039-chave_nfe,        "Chave NFe
    dt_atualizacao    TYPE zlest0039-dt_atualizacao, "Data atualização
    us_atualizacao    TYPE zlest0039-us_atualizacao, "Usuario atualização
    tp_importacao_l1  TYPE zlest0039-tp_importacao_l1, "Tipo de importação L1
    eudr              TYPE zlest0039-eudr, "Atende a EUDR
  END OF ty_zlest0039,

  BEGIN OF ty_zlest0041,
    docnum           TYPE zlest0041-docnum,
    nr_nf_propria    TYPE zlest0041-nr_nf_propria,
    nr_nf            TYPE zlest0041-nr_nf,
    serie            TYPE zlest0041-serie,
    centro_comprador TYPE zlest0041-centro_comprador,
    cod_cliente      TYPE zlest0041-cod_cliente,
  END OF ty_zlest0041,

  BEGIN OF ty_t001w,
    name1 TYPE t001w-name1, " Nome
    werks TYPE t001w-werks, " Centro
    kunnr TYPE t001w-kunnr, " Cliente
  END OF ty_t001w,

  BEGIN OF ty_kna1,
    name1 TYPE kna1-name1,                                " Nome 1
    kunnr TYPE kna1-kunnr, " Nº cliente 1
    stras TYPE kna1-stras, " Rua e nº
    ort01 TYPE kna1-ort01, " Local
    lifnr TYPE kna1-lifnr, " Fornecedor
  END OF ty_kna1,

  BEGIN OF ty_zdco_vinculo,
    nr_dco TYPE zdco_vinculo-nr_dco, " Número do DCO
    vbeln  TYPE zdco_vinculo-vbeln, " Documento de vendas
  END OF ty_zdco_vinculo,

  BEGIN OF ty_vbap,
    charg TYPE vbap-charg, " Número do lote
    vbeln TYPE vbap-vbeln, " Documento de venda
  END OF ty_vbap,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr, " Nº do material
    maktx TYPE makt-maktx, " Texto breve de material
  END OF ty_makt,

  BEGIN OF ty_j_1bnflin,
    docnum TYPE j_1bnflin-docnum, " Nº documento
    cfop   TYPE j_1bnflin-cfop, " Código CFOP e extensão
    menge  TYPE j_1bnflin-menge,
    meins  TYPE j_1bnflin-meins,
    matnr  TYPE j_1bnflin-matnr,
  END OF ty_j_1bnflin,

  BEGIN OF ty_j_1bagnt,
    cfop    TYPE j_1bagnt-cfop, " Descrição do CFOP
    version TYPE j_1bagnt-version, " Nº da versão CFOP
    cfotxt  TYPE j_1bagnt-cfotxt, " Descrição do CFOP
  END OF ty_j_1bagnt,

  BEGIN OF ty_lfa1,
    name1 TYPE lfa1-name1,                               " Nome 1
    lifnr TYPE lfa1-lifnr, " Nº conta do fornecedor
    ort01 TYPE lfa1-ort01, " Local
    ort02 TYPE lfa1-ort02, " Bairro
    stras TYPE lfa1-stras, " Rua e nº
  END OF ty_lfa1,

  BEGIN OF type_retlote,
    docnum       TYPE zsdt_retlote-docnum,
    nfenum       TYPE zsdt_retlote-nfenum,
    werks        TYPE zsdt_retlote-werks,
    nf_retorno   TYPE zsdt_retlote-nf_retorno,
    docnum_ret   TYPE zsdt_retlote-docnum_ret,
    quant_vinc   TYPE zsdt_retlote-quant_vinc,
    data_criacao TYPE zsdt_retlote-data_criacao,
    id_export    TYPE zsdt_retlote-id_export, "81360
  END   OF type_retlote,

  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln,
    kunnr TYPE vbak-kunnr,
    kvgr3 TYPE vbak-kvgr3,
  END OF ty_vbak,

  BEGIN OF ty_vbfa,
    vbelv   TYPE vbfa-vbelv,
    vbtyp_n TYPE vbfa-vbtyp_n,
    vbtyp_v TYPE vbfa-vbtyp_v,
    rfwrt   TYPE vbfa-rfwrt,
  END OF ty_vbfa,

  BEGIN OF y_lin,
    docnum TYPE j_1bnflin-docnum,
    refkey TYPE j_1bnflin-refkey,
    refitm TYPE zdoc_nf_produtor-itmnum_prod,
  END OF y_lin,

  BEGIN OF y_nfdoc,
    docnum TYPE j_1bnfdoc-docnum,
    cancel TYPE j_1bnfdoc-cancel,
  END OF y_nfdoc,

  BEGIN OF y_vbfa,
    vbelv TYPE vbfa-vbelv,
    vbeln TYPE j_1bnflin-refkey, "VBFA-VBELN,
    posnn TYPE vbfa-posnn,
  END OF y_vbfa,

  BEGIN OF y_doc,
    docnum  TYPE j_1bnfdoc-docnum,
    regio   TYPE j_1bregio,
    nfyear  TYPE j_1byear,
    nfmonth TYPE j_1bmonth,
    stcd1   TYPE j_1bstcd1,
    model   TYPE j_1bmodel,
    serie   TYPE j_1bseries,
    nfnum9  TYPE j_1bnfnum9,
    docnum9 TYPE j_1bdocnum9,
    cdv     TYPE j_1bcheckdigit,
  END OF y_doc,


  BEGIN OF y_active,
    docnum  TYPE j_1bnfdoc-docnum,
    regio   TYPE  j_1bregio,
    nfyear  TYPE j_1byear,
    nfmonth TYPE j_1bmonth,
    stcd1   TYPE j_1bstcd1,
    model   TYPE j_1bmodel,
    serie   TYPE j_1bseries,
    nfnum9  TYPE j_1bnfnum9,
    docnum9 TYPE j_1bdocnum9,
    cdv     TYPE j_1bcheckdigit,
  END OF y_active,

  BEGIN OF ty_zsdt0276,
    docnum TYPE zsdt0276-docnum,
    itmnum TYPE zsdt0276-itmnum,
    menge  TYPE zsdt0276-menge,
  END OF ty_zsdt0276,

  BEGIN OF ty_j_1bnfdoc,
    docnum    TYPE j_1bnflin-docnum,
    itmnum    TYPE j_1bnflin-itmnum,
    credat    TYPE j_1bnfdoc-credat,
    docdat    TYPE j_1bnfdoc-docdat,
    model     TYPE j_1bnfdoc-model,
    series    TYPE j_1bnfdoc-series,
    nfnum     TYPE j_1bnfdoc-nfnum,
    bukrs     TYPE j_1bnfdoc-bukrs,
    branch    TYPE j_1bnfdoc-branch,
    parvw     TYPE j_1bnfdoc-parvw,
    parid     TYPE j_1bnfdoc-parid,
    partyp    TYPE j_1bnfdoc-partyp,
    nfenum    TYPE j_1bnfdoc-nfenum,
    matnr     TYPE j_1bnflin-matnr,
    matkl     TYPE j_1bnflin-matkl,
    charg     TYPE j_1bnflin-charg,
    cfop      TYPE j_1bnflin-cfop,
    nbm       TYPE j_1bnflin-nbm,
    menge     TYPE j_1bnflin-menge,
    meins     TYPE j_1bnflin-meins,
    nfe       TYPE j_1bnfdoc-nfe,
    entrad    TYPE j_1bnfdoc-entrad,
    parid_b   TYPE j_1bnfdoc-parid,
    chave_nfe TYPE zib_nfe_dist_itm-chave_nfe,
    cancel    TYPE j_1bnfdoc-cancel.
TYPES: END OF ty_j_1bnfdoc,

BEGIN OF ty_zsdt01ex,
  nro_nf_prod TYPE zsdt0001-nro_nf_prod,
  id_carga    TYPE zsdt0001-id_carga,
END OF ty_zsdt01ex,

BEGIN OF ty_edicao,
  in_werks          TYPE zlest0039-werks,        " Centro
  in_name1          TYPE t001w-name1,            " Remetente
  in_ebeln          TYPE zlest0039-ebeln,        " Ordem de Venda
  in_vbeln          TYPE zlest0039-vbeln,        " Documento de Remessa
  in_nfenum         TYPE zlest0039-nfenum,       " Nota Fiscal
  in_placa_cav      TYPE zlest0039-placa_cav,    " Placa do Cavalo
  in_datasaida      TYPE zlest0039-datasaida,    " Data Saída
  in_pesosaida      TYPE zlest0039-pesosaida,    " Peso Saída
  in_datatransb     TYPE zlest0039-datatransb,   " Data de Descarga
  in_pesotransb     TYPE zlest0039-pesotransb,   " Peso de Descarga
  in_name1_transb   TYPE kna1-name1,             " Local de Entrega Z1
  in_datachegada    TYPE zlest0039-datachegada,  " Data Chegada
  in_pesochegada    TYPE zlest0039-pesochegada,  " Peso Chegada
  in_name1_desti    TYPE lfa1-name1,                      " Nome 1
  in_observacao     TYPE zlest0039-observacao,   " Observação
  in_ort01_dst      TYPE lfa1-ort01,
  in_ort01_transb   TYPE kna1-ort01,
  in_docnum         TYPE zlest0039-docnum,
  in_pontotransb    TYPE zlest0039-pontotransb,
  in_estornar_trans TYPE zlest0039-ck_estornar_trans,
  tp_transgenia     TYPE zlest0039-tp_transgenia,
END OF ty_edicao,

BEGIN OF ty_cte,
  "Informações do CT-e Rodoviário
  cte_docnum_nf       TYPE zcte_info_nota-docnum_nf,
  cte_docnum          TYPE zcte_info_nota-docnum,
  cte_numero          TYPE zcte_identifica-nct,
  cte_valor           TYPE zcte_identifica-vrec,
  cte_vlr_unitario    TYPE zcte_identifica-vlr_unit_frete,
  cte_unidade_valor   TYPE zcte_identifica-unid_vlr_frete,
  cte_cd_contratado   TYPE zcte_ciot-tr_codigo,
  cte_ds_contratado   TYPE zcte_ciot-tr_nome,
  cte_cnpj_contratado TYPE zcte_ciot-tr_cnpj,
END OF ty_cte,

BEGIN OF ty_saida,

  mark,
  cfotxt                TYPE j_1bagnt-cfotxt,        " Natureza da Operação
  cfop                  TYPE j_1bagnt-cfop,        " CFOP

  werks                 TYPE zlest0039-werks,        " Centro
  pontocoleta           TYPE zlest0039-pontocoleta,  " Ponto de Coleta
  name1                 TYPE t001w-name1,            " Remetente
  name1_pontc           TYPE lfa1-name1,             " Nome do Ponto de Coleta
  stras                 TYPE lfa1-stras,             " Endereço
  ort01                 TYPE lfa1-ort01,             " Municipio
  charg                 TYPE vbap-charg,             " Lote
  matnr                 TYPE zlest0039-matnr,        " Material
  maktx                 TYPE makt-maktx,             " Produto
  ebeln                 TYPE zlest0039-ebeln,        " Ordem de Venda
  vbeln                 TYPE zlest0039-vbeln,        " Documento de Remessa
  nfenum                TYPE zlest0039-nfenum,       " Nota Fiscal
  nr_dco                TYPE zdco_vinculo-nr_dco,    " DCO
  placa_cav             TYPE zlest0039-placa_cav,    " Placa do Cavalo
  status                TYPE zlest0039-status,       " Status

  "       datasaida    TYPE zlest0039-datasaida,    " Data Saída
  datasaida(10)         TYPE c,    " Data Saída

  "pesosaida    TYPE zlest0039-pesosaida,    " Peso Saída
  pesosaida             TYPE db20199vp,    " Peso Saída

  "      datatransb      TYPE zlest0039-datatransb,   " Data de Descarga
  datatransb(10)        TYPE c,   " Data de Descarga

  "pesotransb   TYPE zlest0039-pesotransb,   " Peso de Descarga
  pesotransb            TYPE db20199vp,   " Peso de Descarga

  "      datachegada     TYPE zlest0039-datachegada,  " Data Chegada
  datachegada(10)       TYPE c,  " Data Chegada
  pesochegada           TYPE zlest0039-pesochegada,  " Peso Chegada

  "difer        TYPE zlest0039-pesosaida,    " Peso de Descarga - Peso Saída
  difer                 TYPE db20199vp,    " Peso de Descarga - Peso Saída

  diatras(10)           TYPE n,   " Data Chegada - Data Saída

  reftyp                TYPE zlest0039-reftyp,
  name1_transb          TYPE kna1-name1,             " Local de Entrega Z1
  ort01_transb          TYPE kna1-ort01,             " Municipio Transbordo
  cod_dest              TYPE kna1-kunnr,
  name1_desti           TYPE lfa1-name1,              " Destinatário
  ort01_dst             TYPE lfa1-ort01,             " Municipio Destinario
  obs                   TYPE zlest0039-observacao,            " Observação
  docnum                TYPE zlest0039-docnum,
  itmnum                TYPE j_1bnflin-itmnum,
  pontotransb           TYPE zlest0039-pontotransb,
  pontoentrega          TYPE zlest0039-pontoentrega,

  dataterminal          TYPE zlest0039-dataterminal,

  peso_liq              TYPE  db20199vp,   " Peso Liquido

  " Variaveis Auxiliares para Data
  datasaida_aux         TYPE zlest0039-datasaida,
  datachegada_aux       TYPE zlest0039-datachegada,
  datachegada_aux_b     TYPE zlest0039-datachegada,
  datatransb_aux        TYPE zlest0039-datatransb,
  datatransb_aux_b      TYPE zlest0039-datatransb,
  dataterminal_aux      TYPE zlest0039-dataterminal,
  transb_efetivo        TYPE kna1-name1, "ZLEST0039-TRANSB_EFETIVO,
  ort01_transb_ef       TYPE kna1-ort01, "Municipio Transbordo Efetivo

  kvgr3                 TYPE c LENGTH 50,
  rfwrt                 TYPE vbfa-rfwrt,

  " Icone
  duplic(4)             TYPE c,
*"// WBARBOSA 16102024 - US-154985
  icon_eudr(4)          TYPE c,
*"// WBARBOSA 16102024 - US-154985
  dias_trans(4)         TYPE c,

  "Nota Fiscal de Terceiros(Faneli)
  nfe_terc              TYPE zlest0041-nr_nf,
  ck_estornar_trans     TYPE zlest0039-ck_estornar_trans,
  tp_transgenia(30),

  "Informações do CT-e Rodoviário
  cte_docnum            TYPE zcte_info_nota-docnum,
  cte_numero            TYPE zcte_identifica-nct,
  cte_valor             TYPE zcte_identifica-vrec,
  cte_vlr_unitario      TYPE zcte_identifica-vlr_unit_frete,
  cte_unidade_valor     TYPE zcte_identifica-unid_vlr_frete,
  cte_cd_contratado     TYPE zcte_ciot-tr_codigo,
  cte_ds_contratado     TYPE zcte_ciot-tr_nome,
  cte_cnpj_contratado   TYPE zcte_ciot-tr_cnpj,
  chave_nfe_pro         TYPE zlest0039-chave_nfe,
  docnum_nfe_pro        TYPE zcte_info_nota-docnum,
  chave_nfe_ter         TYPE zlest0039-chave_nfe,
  docnum_nfe_ter        TYPE zlest0039-docnum,
  nr_romaneio_s         TYPE zsdt0001-nr_romaneio,
  id_carta_cor          TYPE zcarta_correcao-authcode,
  data_carta_cor        TYPE zcarta_correcao-dt_authcod,
  hora_carta_cor        TYPE zcarta_correcao-hr_authcod,
  doc_material_cc       TYPE zcarta_correcao-doc_material,
  dt_atualizacao        TYPE zlest0039-dt_atualizacao,
  us_atualizacao        TYPE zlest0039-us_atualizacao,
  tp_importacao_l1      TYPE zlest0039-tp_importacao_l1,
  term_cct_portal       TYPE zsdt0168-lifnr,
  dt_recepcao_portal    TYPE zlest0186-dt_recepcao,
  saldo                 TYPE ccm_quant,

  ds_term_cct_portal    TYPE lfa1-name1,
  dt_recepcao_portal_p  TYPE zlest0186-dt_recepcao,
  saldo_exportar        TYPE zsdt0173-peso_liq_total,
  menge                 TYPE ekpo-menge,
  conf_cct_portal       TYPE c LENGTH 4,
  conf_cct_portal_p     TYPE c LENGTH 4,
  peso_aferido_recepcao TYPE zlest0146-peso_aferido_recepcao,
  qtde_vinc_due         TYPE zsdt0173-peso_liq_total,
  qtde_baixada          TYPE zsdt0276-menge,
  peso_fiscal_cct       TYPE zlest0146-peso_aferido_recepcao,
  peso_cct              TYPE zlest0146-peso_aferido_recepcao,
  dif_peso_cct_nf       TYPE zlest0146-peso_aferido_recepcao,
  term_cct              TYPE zsdt0168-lifnr,
  ds_term_cct           TYPE lfa1-name1,
  dt_recepcao_cct       TYPE zlest0146-dt_recepcao,
  check_cfop_delet      TYPE char01,
*  nfnum_c               TYPE zsdt0001-nfnum,
  lgort_de              TYPE mseg-lgort, "User Story 153789 // MMSILVA - 03.10.2024 // Depósito DE
  lgort_para            TYPE mseg-lgort, "User Story 153789 // MMSILVA - 03.10.2024 // Depósito PARA
END OF ty_saida,

BEGIN OF tl_likp ,
  vbeln TYPE likp-vbeln,
  xblnr TYPE likp-xblnr,
END OF tl_likp ,

BEGIN OF tl_zsdt0001 ,
  ch_referencia TYPE zsdt0001-ch_referencia,
  nr_romaneio   TYPE zsdt0001-nr_romaneio,
END OF tl_zsdt0001 ,

BEGIN OF tl_ch_ref ,
  ch_referencia TYPE zch_ref,
END OF tl_ch_ref ,

BEGIN OF tl_zlest0146,
  codigo_ra TYPE zlest0146-local_codigo_ra,
END OF tl_zlest0146,

BEGIN OF tl_zsdt_retlote,
  docnum_ret TYPE zsdt_retlote-docnum_ret,
  docnum     TYPE zsdt_retlote-docnum,
END OF tl_zsdt_retlote,

BEGIN OF ty_j_1bnfdoc_aux,
  docnum TYPE j_1bnfdoc-docnum,
  nfenum TYPE j_1bnfdoc-nfenum,
  series TYPE j_1bnfdoc-series,
  parid  TYPE j_1bnfdoc-parid,
  bukrs  TYPE j_1bnfdoc-bukrs,
  branch TYPE j_1bnfdoc-branch,
END OF ty_j_1bnfdoc_aux.



*nro_nf_prod EQ it_zsdt0001c-id_carga

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA:
  it_zlest0039            TYPE TABLE OF ty_zlest0039,
  ws_zlest0039            TYPE ty_zlest0039,
  it_zsdt0001e            TYPE TABLE OF zsdt0001,
  it_zsdt01ex             TYPE TABLE OF ty_zsdt01ex,
  wa_zsdt01ex             TYPE ty_zsdt01ex,
  it_zsdt0001c            TYPE TABLE OF zsdt0001,
  it_zsdt_export          TYPE TABLE OF zsdt_export,
  it_zsdt_retlote         TYPE TABLE OF tl_zsdt_retlote,  "projHANA - ajuste consulta Bug #119365 - BG
  it_t001w                TYPE TABLE OF ty_t001w,
  tg_j_1bnfdoc            TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
  it_t001w_remet          TYPE TABLE OF ty_t001w,
  it_j_1bnfdoc            TYPE TABLE OF  j_1bnfdoc,
  tg_zlest0146            TYPE TABLE OF tl_zlest0146, "zlest0146  WITH HEADER LINE, "projHANA - ajuste consulta Bug #119365 - BG
  tg_zlest0147            TYPE TABLE OF zlest0147  WITH HEADER LINE,
  tg_zlest0186            TYPE TABLE OF zlest0186  WITH HEADER LINE,
  t_retlote               TYPE TABLE OF type_retlote,
  sl_retlote              TYPE type_retlote,
  t_sumret                TYPE TABLE OF type_retlote,
  sl_sumret               TYPE type_retlote,
  it_j_1bnfe_active       TYPE TABLE OF  j_1bnfe_active,
  it_zsdt0001             TYPE TABLE OF tl_zsdt0001, "projHANA - ajuste consulta Bug #119365 - BG
  it_likp                 TYPE TABLE OF tl_likp, "projHANA - ajuste consulta Bug #119365 - BG

  it_likp_aux             TYPE TABLE OF likp,
  it_zcarta_correcao      TYPE TABLE OF zcarta_correcao,
  it_lfa1_coleta          TYPE TABLE OF ty_lfa1,
  tg_zsdt0168             TYPE TABLE OF zsdt0168   WITH HEADER LINE,
  tg_znom_reme_notas_vinc TYPE TABLE OF znom_reme_notas WITH HEADER LINE,

  it_lfa1_pontoentrega    TYPE TABLE OF ty_lfa1,
  it_kna1_transb          TYPE TABLE OF ty_kna1,
  it_kna1_transb_ef       TYPE TABLE OF ty_kna1,

  it_zdco_vinculo         TYPE TABLE OF ty_zdco_vinculo,
  it_vbap                 TYPE TABLE OF ty_vbap,
  it_makt                 TYPE TABLE OF ty_makt,
  it_j_1bnflin            TYPE TABLE OF ty_j_1bnflin,
  t_doc                   TYPE TABLE OF j_1bnfdoc,
  t_j_1bnfnad             TYPE TABLE OF j_1bnfnad,
  it_j_1bagnt             TYPE TABLE OF ty_j_1bagnt,
  it_vbak                 TYPE TABLE OF ty_vbak,
  it_vbfa                 TYPE TABLE OF ty_vbfa,
  it_edicao               TYPE TABLE OF ty_edicao,
  it_zlest0041            TYPE TABLE OF ty_zlest0041,
  it_ctes                 TYPE TABLE OF ty_cte,
  it_saida                TYPE TABLE OF ty_saida,
  tg_znom_transporte      TYPE TABLE OF znom_transporte WITH HEADER LINE,
  tg_znom_reme_notas      TYPE TABLE OF znom_reme_notas WITH HEADER LINE,
  tg_nf_produtor          TYPE TABLE OF zdoc_nf_produtor,
  tg_nf_produtor_algodao  TYPE TABLE OF zdoc_nf_produtor,
  tg_zsdt0276             TYPE TABLE OF zsdt0276   WITH HEADER LINE,
  tg_zsdt0276_aux         TYPE TABLE OF zsdt0276   WITH HEADER LINE,
  tg_zsdt0276_tot         TYPE TABLE OF ty_zsdt0276 WITH HEADER LINE,
  tg_vbfa                 TYPE TABLE OF y_vbfa,
  tg_nfoc                 TYPE TABLE OF y_nfdoc,
  tg_lin                  TYPE TABLE OF y_lin,
  tg_nfdoc                TYPE TABLE OF y_nfdoc,
  tg_active               TYPE TABLE OF y_active,
  tg_zsdt0170             TYPE TABLE OF zsdt0170   WITH HEADER LINE,
  tg_zsdt0170_ret         TYPE TABLE OF zsdt0170   WITH HEADER LINE,
  r_docnum                TYPE RANGE OF j_1bdocnum,
  it_bdc                  TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  tg_tvarvc               TYPE TABLE OF tvarvc,
  rg_final                TYPE RANGE OF zsdt_export-finalidade,
  it_messtab              TYPE TABLE OF bdcmsgcoll,
  it_ch_referencia        TYPE TABLE OF tl_ch_ref, "projHANA - ajuste consulta Bug #119365 - BG
  it_j_1bnfdoc_aux        TYPE TABLE OF ty_j_1bnfdoc_aux,
  it_mseg                 TYPE TABLE OF mseg.

DATA: wa_nf_produtor LIKE LINE OF tg_nf_produtor,
      wa_vbfa        TYPE ty_vbfa.

*-CS2023000070-21.03.2023-#103477-JT-inicio
DATA: t_parametros TYPE ustyp_t_parameters,
      w_parametros TYPE ustyp_parameters,
      ok_code      TYPE syst_ucomm.
*-CS2023000070-21.03.2023-#103477-JT-fim

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_j_1bnfdoc_aux     TYPE ty_j_1bnfdoc_aux,
  wa_zlest0039         TYPE ty_zlest0039,
  wa_zlest0041         TYPE ty_zlest0041,
  wa_t001w             TYPE ty_t001w,

  wa_lfa1_coleta       TYPE ty_lfa1,
  wa_lfa1_pontoentrega TYPE ty_lfa1,
  wa_kna1_transb       TYPE ty_kna1,
  wa_j_1bnfe_active    TYPE j_1bnfe_active,

  wa_zdco_vinculo      TYPE ty_zdco_vinculo,
  wa_vbap              TYPE ty_vbap,
  wa_makt              TYPE ty_makt,
  wa_j_1bnflin         TYPE ty_j_1bnflin,
  wa_j_1bagnt          TYPE ty_j_1bagnt,
  wa_vbak              TYPE ty_vbak,
  wa_edicao            TYPE ty_edicao,
  wa_active            TYPE j_1bnfe_active,
  wa_saida             TYPE ty_saida,
  wa_saida_aux         TYPE ty_saida,

  wa_cont              TYPE REF TO cl_gui_custom_container,
  wa_alv               TYPE REF TO  cl_gui_alv_grid,
  ls_stable            TYPE lvc_s_stbl,
  wa_layout            TYPE lvc_s_layo,
  index_aux            TYPE lvc_s_row,
  wa_stable            TYPE lvc_s_stbl,

  wa_zlest0077         TYPE zlest0077,
  gw_tabix             TYPE sy-tabix,
  index                TYPE sy-index,
  index_doc            TYPE sy-index,
  retlot               TYPE c LENGTH 1,
  p_nfe_conve          TYPE zlest0039-nfenum.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: it_fcat TYPE TABLE OF lvc_s_fcat.

DATA: wl_zlest0146      TYPE zlest0146,
      lt_zlest0147      TYPE zlest0147_t,
      v_count_dues      TYPE i,
      v_count_nf_prod   TYPE i,
      v_count_saida_nf  TYPE i,
      v_count_saida_doc TYPE i,
      tg_lfa1           TYPE TABLE OF lfa1       WITH HEADER LINE,
      v_doc_rateio      TYPE char01.

DATA:
  r_zlest0039         TYPE RANGE OF zlest0039-formaconfirmacao,
  wa_formaconfirmacao LIKE LINE OF r_zlest0039,
  r_zlest0039_s       TYPE RANGE OF zlest0039-status,
  wa_status           LIKE LINE OF r_zlest0039_s,
  dias_transito(4)    TYPE p,
  v_menge             TYPE ekpo-menge,
  gv_doc_p            TYPE j_1bdocnum,
  vl_kunnr            TYPE kna1-kunnr.

*"// WBARBOSA 16102024 - US-154985
CONSTANTS: gc_atende_eudr TYPE c LENGTH 01 VALUE 'S'.
*"// WBARBOSA 16102024 - US-154985

*DATA: tg_zlest0186 TYPE TABLE OF zlest0186  WITH HEADER LINE,
*      tg_lfa1      TYPE TABLE OF lfa1       WITH HEADER LINE,
*      tg_zsdt0168  TYPE TABLE OF zsdt0168   WITH HEADER LINE.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

" Seleção de Campos (TextField)
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                   p_lote     FOR vbap-charg             NO INTERVALS, " Campo Lote
                   p_cfop     FOR j_1bagnt-cfop          NO INTERVALS, " Natureza de Operação - CFOP
                   p_remes    FOR zlest0039-pontoentrega NO INTERVALS, " Remessa
                   p_emp      FOR zlest0039-bukrs        NO INTERVALS, " Campo Empresa
                   p_centro   FOR t001w-werks            , " Campo Centro
                   p_data_s   FOR zlest0039-datasaida    NO-EXTENSION, " Campo Data Período da Saída
                   p_data_c   FOR zlest0039-datachegada  NO-EXTENSION, " Campo Data Período da Chegada
                   p_ov       FOR zlest0039-ebeln        NO INTERVALS, " Campo O.V (Ordem de Venda)
                   p_mat      FOR zlest0039-matnr        NO INTERVALS, " Campo Material
                   p_nfe      FOR zlest0039-nfenum       NO INTERVALS, " Campo Material
                   p_nf_ter   FOR zlest0041-nr_nf        NO INTERVALS, " Nota de terceiro
                   p_veic     FOR zlest0039-placa_cav    NO INTERVALS, " Campo Veiculo
                   p_pontoc   FOR zlest0039-pontocoleta  NO INTERVALS, " Campo Ponto Coleta
                   p_pontot   FOR zlest0039-pontotransb  NO INTERVALS, " Campo Cod.Transb
                   p_term     FOR zlest0039-pontoentrega NO INTERVALS, " Destinatario
**Inicio  CS2022000139 - 03/03/2022 - ABAP - Anderson Oenning.
                   p_chv_np  FOR  zlest0039-chave_nfe   NO INTERVALS, " Chave nota fiscal proprio
                   p_doc_p   FOR  zlest0039-docnum      NO INTERVALS, " Nº documento nota proprio
                   p_chv_nt  FOR  zlest0039-chave_nfe   NO INTERVALS, " Chave nota fiscal Terceiro
                   p_doc_t   FOR  zlest0039-docnum      NO INTERVALS. " Nº documento nota terceiro
**Fim  CS2022000139 - 03/03/2022 - ABAP - Anderson Oenning.

  DATA: _param    TYPE  ustyp_t_parameters,
        acesso(1) TYPE c.


  "p_client  FOR vbak-kunnr             NO INTERVALS. " Campo Comprador (Contrato de Venda)
SELECTION-SCREEN: END OF BLOCK b1.

" Seleção de Campos (RadioButton) - FC - Formas de Confirmação
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    fc_digi  TYPE char1  RADIOBUTTON GROUP rb02            , " Peso Digitado
    fc_auto  TYPE char1  RADIOBUTTON GROUP rb02            , " Automática
    fc_ambos TYPE char1  RADIOBUTTON GROUP rb02 DEFAULT 'X'. " Ambos
SELECTION-SCREEN: END OF BLOCK b3.

" Seleção de Campos (RadioButton) - S - Status
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-003.
  PARAMETERS:
    s_trans  TYPE char1 RADIOBUTTON GROUP rb03            , " Em transito
    s_transb TYPE char1 RADIOBUTTON GROUP rb03            , " Terminal Transbordo
    s_todos  TYPE char1 RADIOBUTTON GROUP rb03 DEFAULT 'X'. " Todos
SELECTION-SCREEN: END OF BLOCK b4.

INITIALIZATION.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:
            f_seleciona_dados     , " Form seleciona dados
            f_saida               , " Form de saida
            f_alv                 . " ALV

  CALL SCREEN 0100.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*   Form  F_SELECIONA_DADOS
*---------------------------------------------------------------------*
FORM f_seleciona_dados.

  DATA: lt_j_1bnfdoc TYPE TABLE OF ty_j_1bnfdoc_aux.
  DATA: r_ch_referencia TYPE RANGE OF zch_ref,
        lv_count        TYPE i.

  DATA: BEGIN OF t_referencia OCCURS 0,
          ch_referencia TYPE zch_ref,
        END OF t_referencia.

  FREE: it_zlest0039, it_ctes, it_zlest0041, it_t001w, it_lfa1_coleta, it_lfa1_pontoentrega, it_kna1_transb, it_kna1_transb_ef, it_zdco_vinculo, it_vbap,
        it_makt, it_j_1bnflin, it_j_1bagnt, it_vbak, it_vbfa.

*-CS2023000070-21.03.2023-#103477-JT-inicio
  FREE: t_parametros.

*-----------------------------------
* busca parametros do perfil do usuario
*-----------------------------------
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
*     WITH_TEXT           =
    TABLES
      user_parameters     = t_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
*-CS2023000070-21.03.2023-#103477-JT-fim

  "Inicio=======================================CS2019001140 - 06.07.2022 - RJF
  IF p_doc_p IS INITIAL.
    GET PARAMETER ID 'JEF' FIELD gv_doc_p.
    IF gv_doc_p IS NOT INITIAL.
      p_doc_p-sign = 'I'.
      p_doc_p-option = 'EQ'.
      p_doc_p-low = gv_doc_p.
      APPEND p_doc_p.
    ENDIF.
  ENDIF.
  "Fim=======================================CS2019001140 - 06.07.2022 - RJF

  PERFORM condicoes.

  IF ( s_trans EQ 'X' ).
    PERFORM p_transito.
  ELSEIF ( s_transb EQ 'X' ).
    PERFORM p_transbordo.
  ELSE.
    PERFORM p_todos.
  ENDIF.

  CHECK NOT it_zlest0039[] IS INITIAL.

  "Buscar Informações do Frete Rodoviário """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT n~docnum_nf       AS cte_docnum_nf
         n~docnum          AS cte_docnum
         i~nct             AS cte_numero
         i~vrec            AS cte_valor
         i~vlr_unit_frete  AS cte_vlr_unitario
         i~unid_vlr_frete  AS cte_unidade_valor
         c~tr_codigo       AS cte_cd_contratado
         c~tr_nome         AS cte_ds_contratado
         c~tr_cnpj         AS cte_cnpj_contratado
    INTO TABLE it_ctes
    FROM zcte_info_nota AS n
   INNER JOIN zcte_identifica AS i ON i~docnum EQ n~docnum
   INNER JOIN j_1bnfdoc       AS d ON d~docnum EQ n~docnum
   INNER JOIN zcte_ciot       AS c ON c~docnum EQ n~docnum
     FOR ALL ENTRIES IN it_zlest0039
   WHERE n~docnum_nf EQ it_zlest0039-docnum
     AND i~modal     EQ '01'
     AND d~cancel    EQ ' '.

  SORT it_ctes BY cte_docnum_nf.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  " Nota fiscal de terceiros
  SELECT docnum nr_nf_propria nr_nf serie centro_comprador cod_cliente
    FROM zlest0041
    INTO TABLE it_zlest0041
    FOR ALL ENTRIES IN it_zlest0039
    WHERE docnum = it_zlest0039-docnum
    AND nr_nf_propria = it_zlest0039-nfenum
    AND nr_nf         IN p_nf_ter.  "Í

*    IF p_nf_ter IS NOT INITIAL.
*      LOOP AT it_zlest0039 INTO DATA(wa_zlest0039).
*        READ TABLE it_zlest0041 TRANSPORTING NO FIELDS WITH KEY docnum        = wa_zlest0039-docnum
*                                         nr_nf_propria = wa_zlest0039-nfenum.
*        IF sy-subrc NE 0.
*          DELETE TABLE it_zlest0039 FROM wa_zlest0039.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

  "Inicio=======================================CS2022000139 - 03.03.2022 - Anderson Oenning
  IF p_chv_nt IS INITIAL OR p_doc_t IS INITIAL.
    IF it_zlest0041 IS NOT INITIAL.

      LOOP AT it_zlest0041 ASSIGNING FIELD-SYMBOL(<ws_zlest0041>).
        <ws_zlest0041>-serie = |{ <ws_zlest0041>-serie ALPHA = OUT }|.
      ENDLOOP.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
      FROM j_1bnfdoc AS dc INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
        FOR ALL ENTRIES IN it_zlest0041
      WHERE dc~series EQ it_zlest0041-serie
      AND  dc~branch  EQ it_zlest0041-centro_comprador
      AND  dc~parid   EQ it_zlest0041-cod_cliente
      AND  dc~nfenum  EQ it_zlest0041-nr_nf
      AND  cancel     EQ space.


      FREE: it_j_1bnfdoc.
      SELECT * FROM j_1bnfdoc
        INTO TABLE it_j_1bnfdoc
        FOR ALL ENTRIES IN it_zlest0041
        WHERE series    EQ it_zlest0041-serie
         AND  branch  EQ it_zlest0041-centro_comprador
         AND  parid   EQ it_zlest0041-cod_cliente
         AND  nfenum  EQ it_zlest0041-nr_nf
         AND  cancel  EQ space
         AND  doctyp  EQ '1'. "US #182782 - MMSILVA - 20.06.2025




      IF it_j_1bnfdoc IS NOT INITIAL.
        FREE: it_j_1bnfe_active.
        SELECT * FROM j_1bnfe_active
          INTO TABLE it_j_1bnfe_active
          FOR ALL ENTRIES IN it_j_1bnfdoc
          WHERE docnum EQ it_j_1bnfdoc-docnum.
        IF it_j_1bnfe_active IS NOT INITIAL.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT vbeln xblnr FROM likp INTO TABLE it_likp
  FOR ALL ENTRIES IN it_zlest0039
    WHERE vbeln EQ it_zlest0039-vbeln.

  r_ch_referencia = VALUE #( FOR l IN it_likp ( option = 'EQ' sign = 'I' low = l-xblnr ) ).




  MOVE-CORRESPONDING it_likp TO it_ch_referencia.





  IF r_ch_referencia IS NOT INITIAL.
    SELECT ch_referencia nr_romaneio FROM zsdt0001
      INTO TABLE it_zsdt0001
      FOR ALL ENTRIES IN it_ch_referencia
      WHERE ch_referencia EQ it_ch_referencia-ch_referencia.
  ENDIF.
*  r_ch_referencia = VALUE #( FOR l IN it_likp_aux ( option = 'EQ' sign = 'I' low = l-xblnr ) ).
*  IF r_ch_referencia IS NOT INITIAL.
*    SELECT * FROM zsdt0001
*      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0001
*      WHERE ch_referencia IN r_ch_referencia.
*  ENDIF.

  SELECT *  FROM zcarta_correcao
  INTO TABLE it_zcarta_correcao
  FOR ALL ENTRIES IN it_zlest0039
  WHERE docnum EQ it_zlest0039-docnum
   "  AND doc_material NE space.  "BUG SOLTO 75826 LPORTELA
   AND code EQ 135
    AND novo_terminal  NE space.

  "
  "Fim=======================================CS2022000139 - 03.03.2022 - Anderson Oenning

  " Centros/filiais
  SELECT name1 werks kunnr
    FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_zlest0039
  WHERE werks EQ it_zlest0039-werks.
  "AND WERKS IN P_CENTRO.

  " Buscar o Remetente. Ponto de Coleta - PC
  SELECT name1 lifnr ort01 ort02 stras
    FROM lfa1
    INTO TABLE it_lfa1_coleta
    FOR ALL ENTRIES IN it_zlest0039
  WHERE lifnr EQ it_zlest0039-pontocoleta.

  " Ponto Transbordo. Ponto de Coleta - Z1
  SELECT name1 lifnr ort01 ort02 stras
    FROM lfa1
    INTO TABLE it_lfa1_pontoentrega
    FOR ALL ENTRIES IN it_zlest0039
  WHERE lifnr EQ it_zlest0039-pontoentrega.

  " Busca informações do destinatário.
  SELECT name1 kunnr stras ort01 lifnr
    FROM kna1
    INTO TABLE it_kna1_transb
    FOR ALL ENTRIES IN it_zlest0039
  WHERE kunnr EQ it_zlest0039-pontotransb
    AND kunnr NE space.

  " Busca informações do  transb. efetivo
  SELECT name1 kunnr stras ort01 lifnr
    FROM kna1
    INTO TABLE it_kna1_transb_ef
    FOR ALL ENTRIES IN it_zlest0039
  WHERE kunnr EQ it_zlest0039-transb_efetivo
    AND kunnr NE space.

  " Vinculo de DCO com fornecimento
  SELECT nr_dco vbeln
    FROM zdco_vinculo
    INTO TABLE it_zdco_vinculo
    FOR ALL ENTRIES IN it_zlest0039
  WHERE vbeln EQ it_zlest0039-vbeln.

  " Documento de vendas: dados de item
  SELECT charg vbeln
    FROM vbap
    INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_zlest0039
  WHERE vbeln EQ it_zlest0039-ebeln
    AND charg IN p_lote.

  " Textos breves de material
  SELECT matnr maktx
    FROM  makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_zlest0039
  WHERE spras EQ sy-langu
    AND matnr EQ it_zlest0039-matnr.

  " Partidas individuais da nota fiscal
  SELECT docnum cfop menge meins matnr
        FROM j_1bnflin
    INTO TABLE it_j_1bnflin
    FOR ALL ENTRIES IN it_zlest0039
  WHERE docnum EQ it_zlest0039-docnum.

  CHECK NOT it_j_1bnflin[] IS INITIAL.

  " CFOP com versões: descrição
  SELECT cfop version cfotxt
    FROM j_1bagnt
    INTO TABLE it_j_1bagnt
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE cfop EQ it_j_1bnflin-cfop
    AND version = '02'
    AND cfop IN p_cfop.

  SELECT vbeln kunnr kvgr3
     FROM vbak
    INTO TABLE it_vbak
    FOR ALL ENTRIES IN it_zlest0039
  WHERE vbeln EQ it_zlest0039-ebeln.

  SELECT vbelv vbtyp_n vbtyp_v rfwrt
     FROM vbfa
    INTO TABLE it_vbfa
    FOR ALL ENTRIES IN it_zlest0039
 WHERE vbelv EQ it_zlest0039-vbeln
   AND vbtyp_n = 'M'
   AND vbtyp_v = 'J'.

  PERFORM: z_seleciona_doc,
           z_seleciona_retlote ,
           f_get_due,
           f_get_cct,
           f_get_znom_transporte,
           f_get_nfe_exportacao,
           f_get_nfe_exportacao_algodao,
           f_get_qnt_baixada.

  PERFORM zf_seleciona_tvarv.

*CS2022000854 Automatização Estorno Trânsito da Mercadoria / Anderson Oenning
  IF it_zlest0039 IS NOT INITIAL.
    SELECT docnum_ret docnum FROM zsdt_retlote INTO TABLE it_zsdt_retlote
      FOR ALL ENTRIES IN it_zlest0039
       WHERE docnum EQ it_zlest0039-docnum.

    IF sy-subrc EQ 0.
      SELECT * FROM zsdt_export INTO TABLE it_zsdt_export
      FOR ALL ENTRIES IN it_zsdt_retlote
      WHERE docnum EQ it_zsdt_retlote-docnum_ret AND finalidade IN rg_final.
      IF sy-subrc EQ 0.
        LOOP AT it_zsdt_export ASSIGNING FIELD-SYMBOL(<ls_zsdt_export>).
          READ TABLE it_zsdt_retlote INTO DATA(ws_retlote) WITH KEY docnum_ret = <ls_zsdt_export>-docnum.
          IF sy-subrc EQ 0.
            READ TABLE it_zlest0039 INTO ws_zlest0039 WITH KEY docnum = ws_retlote-docnum.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
                EXPORTING
                  docnum         = ws_retlote-docnum
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

                UPDATE zlest0039
                  SET ck_estornar_trans = abap_true
                WHERE docnum EQ ws_retlote-docnum.
                CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
                  EXPORTING
                    docnum = ws_retlote-docnum.

              ENDIF.
            ENDIF.
          ENDIF.
          CLEAR: ws_retlote, ws_zlest0039.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
*  cs2022000854 automatização estorno trânsito da mercadoria / anderson oenning


*A coluna deverá fazer a seleção abaixo para a seleção do filtro da tela inicial:
*
*Selecionar
*ZSDT0001- NRO_NF_PROD com zlest0039-Docnum
*ZSDT0001-tp_movimento = ‘S’
*
*Pegar ZSDT0001-ID_CARGA (Guardar na variável X_ID_CARGA)
*
*                Fazer nova seleção na ZSDT0001
*Selecionar
*
*ZSDT0001-ID_Carga = X_ID_CARGA
*ZSDT0001-tp_movimento = ‘E’
*                Pegar ZSDT0001-NFNUM (salvar na nova coluna criada no ALV)

  SELECT * FROM zsdt0001
  INTO TABLE it_zsdt0001c
  FOR ALL ENTRIES IN it_zlest0039
  WHERE nro_nf_prod EQ it_zlest0039-docnum
    AND tp_movimento  EQ 'S'.

  IF sy-subrc IS INITIAL.

    LOOP AT it_zsdt0001c INTO DATA(ls_zsdt0001c).
      DO 2 TIMES.
        ADD 1 TO lv_count.
        wa_j_1bnfdoc_aux-nfenum = ls_zsdt0001c-nfnum   .
        wa_j_1bnfdoc_aux-parid  = ls_zsdt0001c-parid   .
        wa_j_1bnfdoc_aux-bukrs  = ls_zsdt0001c-bukrs   .
        wa_j_1bnfdoc_aux-branch = ls_zsdt0001c-branch  .
        wa_j_1bnfdoc_aux-series = COND #( WHEN lv_count EQ 1 THEN |{ ls_zsdt0001c-series ALPHA = IN }|
                                          ELSE |{ ls_zsdt0001c-series ALPHA = OUT }| )  .
        APPEND wa_j_1bnfdoc_aux TO lt_j_1bnfdoc.
      ENDDO.
      CLEAR lv_count.
    ENDLOOP.

*    lt_j_1bnfdoc = VALUE #( BASE lt_j_1bnfdoc FOR ls_zsdt0001c IN it_zsdt0001c (
*                        nfenum = ls_zsdt0001c-nfnum
*                        series = ls_zsdt0001c-series
*                        parid  = ls_zsdt0001c-parid
*                        bukrs  = ls_zsdt0001c-bukrs
*                        branch = ls_zsdt0001c-branch
*                        ) ) .

    LOOP AT it_zsdt0001c INTO DATA(wa_zsdt0001c).
      IF wa_zsdt0001c-id_carga IS NOT INITIAL.
        wa_zsdt01ex-id_carga    = wa_zsdt0001c-id_carga.
        wa_zsdt01ex-nro_nf_prod = wa_zsdt0001c-nro_nf_prod.
        APPEND wa_zsdt01ex TO it_zsdt01ex.
      ENDIF.
      CLEAR wa_zsdt01ex.
    ENDLOOP.

    IF it_zsdt01ex IS NOT INITIAL.
      SORT it_zsdt01ex BY id_carga.
*      DELETE it_zsdt01ex WHERE id_carga IS INITIAL.
      SORT it_zsdt01ex BY nro_nf_prod.
      SELECT * FROM zsdt0001
      INTO TABLE it_zsdt0001e
      FOR ALL ENTRIES IN it_zsdt01ex
      WHERE id_carga  EQ it_zsdt01ex-id_carga
        AND tp_movimento EQ 'E'.

*BUG - 136092 - Remover coluna NF_COMPRA e ajustar nfe terceiro | ITSOUZA
      IF sy-subrc EQ 0.

        LOOP AT it_zsdt0001e INTO DATA(ls_zsdt0001e).
          DO 2 TIMES.
            ADD 1 TO lv_count.
            wa_j_1bnfdoc_aux-nfenum = ls_zsdt0001e-nfnum   .
            wa_j_1bnfdoc_aux-parid  = ls_zsdt0001e-parid   .
            wa_j_1bnfdoc_aux-bukrs  = ls_zsdt0001e-bukrs   .
            wa_j_1bnfdoc_aux-branch = ls_zsdt0001e-branch  .
            wa_j_1bnfdoc_aux-series = COND #( WHEN lv_count EQ 1 THEN |{ ls_zsdt0001e-series ALPHA = IN }|
                                              ELSE |{ ls_zsdt0001e-series ALPHA = OUT }| )  .
            APPEND wa_j_1bnfdoc_aux TO lt_j_1bnfdoc.
          ENDDO.
          CLEAR lv_count.
        ENDLOOP.

*        lt_j_1bnfdoc = VALUE #( BASE lt_j_1bnfdoc FOR ls_zsdt0001e IN it_zsdt0001e (
*                                nfenum = ls_zsdt0001e-nfnum
*                                series = ls_zsdt0001e-series
*                                parid  = ls_zsdt0001e-parid
*                                bukrs  = ls_zsdt0001e-bukrs
*                                branch = ls_zsdt0001e-branch
*                                ) ).

        SELECT docnum nfenum series parid bukrs branch FROM j_1bnfdoc
        INTO TABLE it_j_1bnfdoc_aux
        FOR ALL ENTRIES IN lt_j_1bnfdoc
        WHERE nfenum   EQ lt_j_1bnfdoc-nfenum
          AND series  EQ lt_j_1bnfdoc-series
          AND parid   EQ lt_j_1bnfdoc-parid
          AND bukrs   EQ lt_j_1bnfdoc-bukrs
          AND branch  EQ lt_j_1bnfdoc-branch
          AND cancel  EQ space.
      ENDIF.
    ENDIF.
  ENDIF.

  "Inicio user Story 153789 // MMSILVA - 03.10.2024
  IF it_zcarta_correcao IS NOT INITIAL.
    SELECT *
      FROM mseg
      INTO TABLE it_mseg
      FOR ALL ENTRIES IN it_zcarta_correcao
      WHERE mblnr EQ it_zcarta_correcao-doc_material.
  ENDIF.

  "Fim User Story 153789 // MMSILVA - 03.10.2024 //

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  P_TRANSITO
*&---------------------------------------------------------------------*
FORM p_transito .
  FREE: r_docnum, it_zlest0041, it_j_1bnfdoc.
  "Inicio=======================================CS2022000139 - 03.03.2022 - Anderson Oenning
  IF p_chv_nt IS NOT INITIAL OR p_doc_t IS NOT INITIAL.
    LOOP AT p_chv_nt ASSIGNING FIELD-SYMBOL(<w_chave>).
      wa_active-regio     = <w_chave>-low(2).
      wa_active-nfyear    = <w_chave>-low+2(2).
      wa_active-nfmonth   = <w_chave>-low+4(2).
      wa_active-stcd1     = <w_chave>-low+6(14).
      wa_active-model     = <w_chave>-low+20(2).
      wa_active-serie     = <w_chave>-low+22(3).
      wa_active-nfnum9    = <w_chave>-low+25(9).
      wa_active-docnum9   = <w_chave>-low+34(9).
      wa_active-cdv       = <w_chave>-low+43(1).

      CLEAR: wa_j_1bnfe_active.
      SELECT SINGLE *
        FROM j_1bnfe_active
        INTO wa_j_1bnfe_active
       WHERE regio    EQ wa_active-regio
         AND nfyear   EQ wa_active-nfyear
         AND nfmonth  EQ wa_active-nfmonth
         AND stcd1    EQ wa_active-stcd1
         AND model    EQ wa_active-model
         AND serie    EQ wa_active-serie
         AND nfnum9   EQ wa_active-nfnum9
         AND docnum9  EQ wa_active-docnum9
         AND cdv      EQ wa_active-cdv.

      IF wa_j_1bnfe_active IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_j_1bnfe_active-docnum ) TO r_docnum.
        APPEND LINES OF r_docnum TO p_doc_t.
        REFRESH r_docnum.   "<<SKM-29.09.21-IR110139
      ENDIF.
    ENDLOOP.

    CHECK p_doc_t[] IS NOT INITIAL.

    SELECT * FROM j_1bnfdoc
    INTO TABLE it_j_1bnfdoc
    WHERE docnum IN p_doc_t.

    " Nota fiscal de terceiros
    IF it_j_1bnfdoc IS NOT INITIAL.
      LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<ws_1bnfdoc>).
        <ws_1bnfdoc>-series = |{ <ws_1bnfdoc>-series ALPHA = IN }|.
      ENDLOOP.

      SELECT docnum nr_nf_propria nr_nf serie centro_comprador cod_cliente
      FROM zlest0041
      INTO TABLE it_zlest0041
      FOR ALL ENTRIES IN it_j_1bnfdoc
      WHERE  serie             EQ it_j_1bnfdoc-series
        AND  centro_comprador  EQ it_j_1bnfdoc-branch
        AND  cod_cliente       EQ it_j_1bnfdoc-parid
        AND  nr_nf             EQ it_j_1bnfdoc-nfenum.
    ENDIF.

    CHECK it_zlest0041 IS NOT INITIAL.

    " Comparativo de saidas e chegadas
    SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
           datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
           pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
           placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
           ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr
      FROM zlest0039
      INTO TABLE it_zlest0039
      FOR ALL ENTRIES IN it_zlest0041
    WHERE docnum EQ it_zlest0041-docnum
      AND bukrs            IN p_emp
      AND datasaida        IN p_data_s
      AND ebeln            IN p_ov
      AND pontoentrega     IN p_term
      AND matnr            IN p_mat
      AND placa_cav        IN p_veic
      AND pontocoleta      IN p_pontoc
      AND pontotransb      IN p_pontot
      AND nfenum           IN p_nfe
      AND formaconfirmacao IN r_zlest0039
      AND pesotransb       EQ space
      AND pesochegada      EQ space
      AND werks            IN p_centro
      AND vbeln            IN p_remes
      AND chave_nfe        IN p_chv_np
      AND docnum           IN p_doc_p.

  ELSE.

    " Comparativo de saidas e chegadas
    SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
           datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
           pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
           placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
           ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr
      FROM zlest0039
      INTO TABLE it_zlest0039
    WHERE bukrs            IN p_emp
      AND datasaida        IN p_data_s
      AND ebeln            IN p_ov
      AND pontoentrega     IN p_term
      AND matnr            IN p_mat
      AND placa_cav        IN p_veic
      AND pontocoleta      IN p_pontoc
      AND pontotransb      IN p_pontot
      AND nfenum           IN p_nfe
      AND formaconfirmacao IN r_zlest0039
      AND pesotransb       EQ space
      AND pesochegada      EQ space
      AND werks            IN p_centro
      AND vbeln            IN p_remes
      AND chave_nfe        IN p_chv_np
      AND docnum           IN p_doc_p.
  ENDIF.

ENDFORM.                    " P_TRANSITO
*&---------------------------------------------------------------------*
*&      Form  P_TRANSBORDO
*&---------------------------------------------------------------------*
FORM p_transbordo .


  FREE: r_docnum, it_zlest0041, it_j_1bnfdoc.
  "Inicio=======================================CS2022000139 - 03.03.2022 - Anderson Oenning
  IF p_chv_nt IS NOT INITIAL OR p_doc_t IS NOT INITIAL.
    LOOP AT p_chv_nt ASSIGNING FIELD-SYMBOL(<w_chave>).
      wa_active-regio     = <w_chave>-low(2).
      wa_active-nfyear    = <w_chave>-low+2(2).
      wa_active-nfmonth   = <w_chave>-low+4(2).
      wa_active-stcd1     = <w_chave>-low+6(14).
      wa_active-model     = <w_chave>-low+20(2).
      wa_active-serie     = <w_chave>-low+22(3).
      wa_active-nfnum9    = <w_chave>-low+25(9).
      wa_active-docnum9   = <w_chave>-low+34(9).
      wa_active-cdv       = <w_chave>-low+43(1).

      CLEAR: wa_j_1bnfe_active.
      SELECT SINGLE *
        FROM j_1bnfe_active
        INTO wa_j_1bnfe_active
       WHERE regio    EQ wa_active-regio
         AND nfyear   EQ wa_active-nfyear
         AND nfmonth  EQ wa_active-nfmonth
         AND stcd1    EQ wa_active-stcd1
         AND model    EQ wa_active-model
         AND serie    EQ wa_active-serie
         AND nfnum9   EQ wa_active-nfnum9
         AND docnum9  EQ wa_active-docnum9
         AND cdv      EQ wa_active-cdv.


      IF wa_j_1bnfe_active IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_j_1bnfe_active-docnum ) TO r_docnum.
        APPEND LINES OF r_docnum TO p_doc_t.
        REFRESH r_docnum.   "<<SKM-29.09.21-IR110139
      ENDIF.
    ENDLOOP.

    CHECK p_doc_t[] IS NOT INITIAL.

    SELECT * FROM j_1bnfdoc
    INTO TABLE it_j_1bnfdoc
    WHERE docnum IN p_doc_t.

    " Nota fiscal de terceiros
    IF it_j_1bnfdoc IS NOT INITIAL.

      LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<ws_1bnfdoc>).
        <ws_1bnfdoc>-series = |{ <ws_1bnfdoc>-series ALPHA = IN }|.
      ENDLOOP.

      SELECT docnum nr_nf_propria nr_nf serie centro_comprador cod_cliente
      FROM zlest0041
      INTO TABLE it_zlest0041
      FOR ALL ENTRIES IN it_j_1bnfdoc
      WHERE  serie             EQ it_j_1bnfdoc-series
        AND  centro_comprador  EQ it_j_1bnfdoc-branch
        AND  cod_cliente       EQ it_j_1bnfdoc-parid
        AND  nr_nf             EQ it_j_1bnfdoc-nfenum.
    ENDIF.

    CHECK it_zlest0041 IS NOT INITIAL.

    IF ( NOT p_data_c IS INITIAL ).

      " Comparativo de saidas e chegadas
      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
             pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
             placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
             ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr
        FROM zlest0039
        INTO TABLE it_zlest0039
        FOR ALL ENTRIES IN it_zlest0041
      WHERE docnum           EQ it_zlest0041-docnum
        AND bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        "AND unidadetransb    NE space.
        AND pontotransb      NE space
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.

    ELSE.
      " Comparativo de saidas e chegadas
*---> 02/06/2023 - Migração S4 - JS
*      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
*             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
*            pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
*            placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
*            ck_estornar_trans chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1
      SELECT mandt
            docnum
            cnpj
            nfenum
            serie
            vbeln
            bukrs
            werks
            pontocoleta
            pontotransb
            pontoentrega
            datasaida
            pesosaida
            unidadesaida
            datatransb
            pesotransb
            unidadetransb
            dataterminal
            pesoterminal
            unidadeterminal
            datachegada
            pesochegada
            unidadechegada
            ebeln
            matnr
            kunnr
            placa_cav
            formaconfirmacao
            status
            data
            hora
            reftyp
            observacao
            status_placa
            dias_transito
            pesoliquido
            inco1
            chave_nfe
            transb_efetivo
            ck_estornar_trans
            dt_atualizacao
            us_atualizacao
            tp_importacao_l1
            eudr
*<--- 02/06/2023 - Migração S4 - JS
        FROM zlest0039
        INTO TABLE it_zlest0039
       FOR ALL ENTRIES IN it_zlest0041
        WHERE docnum         EQ it_zlest0041-docnum
        AND bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        "and datachegada      in p_data_c
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        "AND unidadetransb    NE space.
        AND pontotransb      NE space
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.
    ENDIF.

  ELSE.
    IF ( NOT p_data_c IS INITIAL ).

      " Comparativo de saidas e chegadas
      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
             pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
             placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
             ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr


        FROM zlest0039
        INTO TABLE it_zlest0039
      WHERE bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        "AND unidadetransb    NE space.
        AND pontotransb      NE space
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.

    ELSE.
      " Comparativo de saidas e chegadas
*---> 02/06/2023 - Migração S4 - JS
*      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
*             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
*            pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
*            placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
*            ck_estornar_trans chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1
      SELECT mandt
            docnum
            cnpj
            nfenum
            serie
            vbeln
            bukrs
            werks
            pontocoleta
            pontotransb
            pontoentrega
            datasaida
            pesosaida
            unidadesaida
            datatransb
            pesotransb
            unidadetransb
            dataterminal
            pesoterminal
            unidadeterminal
            datachegada
            pesochegada
            unidadechegada
            ebeln
            matnr
            kunnr
            placa_cav
            formaconfirmacao
            status
            data
            hora
            reftyp
            observacao
            status_placa
            dias_transito
            pesoliquido
            inco1
            chave_nfe
            transb_efetivo
            ck_estornar_trans
            dt_atualizacao
            us_atualizacao
            tp_importacao_l1
            eudr
*<--- 02/06/2023 - Migração S4 - JS
        FROM zlest0039
        INTO TABLE it_zlest0039
      WHERE bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        "and datachegada      in p_data_c
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        "AND unidadetransb    NE space.
        AND pontotransb      NE space
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.

    ENDIF.
  ENDIF.



ENDFORM.                    " P_TRANSBORDO
*&---------------------------------------------------------------------*
*&      Form  P_TODOS
*&---------------------------------------------------------------------*
FORM p_todos .


  FREE: r_docnum, it_zlest0041, it_j_1bnfdoc.
  "Inicio=======================================CS2022000139 - 03.03.2022 - Anderson Oenning
  IF p_chv_nt IS NOT INITIAL OR p_doc_t IS NOT INITIAL.
    LOOP AT p_chv_nt ASSIGNING FIELD-SYMBOL(<w_chave>).
      wa_active-regio     = <w_chave>-low(2).
      wa_active-nfyear    = <w_chave>-low+2(2).
      wa_active-nfmonth   = <w_chave>-low+4(2).
      wa_active-stcd1     = <w_chave>-low+6(14).
      wa_active-model     = <w_chave>-low+20(2).
      wa_active-serie     = <w_chave>-low+22(3).
      wa_active-nfnum9    = <w_chave>-low+25(9).
      wa_active-docnum9   = <w_chave>-low+34(9).
      wa_active-cdv       = <w_chave>-low+43(1).

      CLEAR: wa_j_1bnfe_active.
      SELECT SINGLE *
        FROM j_1bnfe_active
        INTO wa_j_1bnfe_active
       WHERE regio    EQ wa_active-regio
         AND nfyear   EQ wa_active-nfyear
         AND nfmonth  EQ wa_active-nfmonth
         AND stcd1    EQ wa_active-stcd1
         AND model    EQ wa_active-model
         AND serie    EQ wa_active-serie
         AND nfnum9   EQ wa_active-nfnum9
         AND docnum9  EQ wa_active-docnum9
         AND cdv      EQ wa_active-cdv.

      IF wa_j_1bnfe_active IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_j_1bnfe_active-docnum ) TO r_docnum.
        APPEND LINES OF r_docnum TO p_doc_t.
        REFRESH r_docnum.   "<<SKM-29.09.21-IR110139
      ENDIF.
    ENDLOOP.

    CHECK p_doc_t[] IS NOT INITIAL.
    SELECT * FROM j_1bnfdoc
    INTO TABLE it_j_1bnfdoc
    WHERE docnum IN p_doc_t.

    " Nota fiscal de terceiros
    IF it_j_1bnfdoc IS NOT INITIAL.

      LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<ws_1bnfdoc>).
        <ws_1bnfdoc>-series = |{ <ws_1bnfdoc>-series ALPHA = IN }|.
      ENDLOOP.

      SELECT docnum nr_nf_propria nr_nf serie centro_comprador cod_cliente
      FROM zlest0041
      INTO TABLE it_zlest0041
      FOR ALL ENTRIES IN it_j_1bnfdoc
      WHERE  serie             EQ it_j_1bnfdoc-series
        AND  centro_comprador  EQ it_j_1bnfdoc-branch
        AND  cod_cliente       EQ it_j_1bnfdoc-parid
        AND  nr_nf             EQ it_j_1bnfdoc-nfenum.
    ENDIF.

    CHECK it_zlest0041 IS NOT INITIAL.

    IF ( NOT p_data_c IS INITIAL ).

      " Comparativo de saidas e chegadas
      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
             pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
            placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
             ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr

         FROM zlest0039
*---> 02/06/2023 - Migração S4 - JS
*       INTO TABLE it_zlest0041
         INTO CORRESPONDING FIELDS OF TABLE it_zlest0041
*<--- 02/06/2023 - Migração S4 - JS
          FOR ALL ENTRIES IN it_zlest0041
       WHERE docnum           EQ it_zlest0041-docnum
         AND bukrs            IN p_emp
         AND datasaida        IN p_data_s
         AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
         "and datatransb       in p_data_c
         AND ebeln            IN p_ov
         AND pontoentrega     IN p_term
         AND matnr            IN p_mat
         AND placa_cav        IN p_veic
         AND pontocoleta      IN p_pontoc
         AND pontotransb      IN p_pontot
         AND nfenum           IN p_nfe
         AND formaconfirmacao IN r_zlest0039
         AND status           IN r_zlest0039_s
         AND werks            IN p_centro
         AND vbeln            IN p_remes
         AND chave_nfe        IN p_chv_np
         AND docnum           IN p_doc_p.

    ELSE.
      " Comparativo de saidas e chegadas
      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
             pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
             placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
             ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr
        FROM zlest0039
        INTO TABLE it_zlest0039
        FOR ALL ENTRIES IN it_zlest0041
      WHERE docnum           EQ it_zlest0041-docnum
        AND bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        "and datachegada       in p_data_c
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        AND status           IN r_zlest0039_s
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.
    ENDIF.

  ELSE.
    IF ( NOT p_data_c IS INITIAL ).

      " Comparativo de saidas e chegadas
      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
             pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
             placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
             ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr
        FROM zlest0039
        INTO TABLE it_zlest0039
      WHERE bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        "and datatransb       in p_data_c
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        AND status           IN r_zlest0039_s
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.

    ELSE.
      " Comparativo de saidas e chegadas
      SELECT mandt docnum cnpj nfenum serie vbeln bukrs werks pontocoleta pontotransb pontoentrega
             datasaida pesosaida unidadesaida datatransb pesotransb unidadetransb dataterminal
             pesoterminal unidadeterminal datachegada pesochegada unidadechegada ebeln matnr kunnr
             placa_cav formaconfirmacao status data hora reftyp observacao status_placa dias_transito pesoliquido transb_efetivo
             ck_estornar_trans tp_transgenia chave_nfe dt_atualizacao us_atualizacao tp_importacao_l1 eudr
        FROM zlest0039
        INTO TABLE it_zlest0039
      WHERE bukrs            IN p_emp
        AND datasaida        IN p_data_s
        AND ( ( datatransb   IN p_data_c ) OR  ( datachegada IN p_data_c ) )
        "and datachegada       in p_data_c
        AND ebeln            IN p_ov
        AND pontoentrega     IN p_term
        AND matnr            IN p_mat
        AND placa_cav        IN p_veic
        AND pontocoleta      IN p_pontoc
        AND pontotransb      IN p_pontot
        AND nfenum           IN p_nfe
        AND formaconfirmacao IN r_zlest0039
        AND status           IN r_zlest0039_s
        AND werks            IN p_centro
        AND vbeln            IN p_remes
        AND chave_nfe        IN p_chv_np
        AND docnum           IN p_doc_p.

    ENDIF.
  ENDIF.
ENDFORM.                    " P_TODOS

*---------------------------------------------------------------------*
* Form  CONDICOES
*---------------------------------------------------------------------*
FORM condicoes.

  DATA: wa_nfe     LIKE p_nfe,
        wa_nf_terc LIKE p_nf_ter,
        vg_tabix   TYPE sy-tabix.

  FREE: r_zlest0039, r_zlest0039_s.

  IF ( NOT p_nfe IS INITIAL ).
    LOOP AT p_nfe INTO wa_nfe.
      vg_tabix = sy-tabix.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_nfe-low
        IMPORTING
          output = wa_nfe-low.
      MODIFY p_nfe FROM wa_nfe INDEX vg_tabix TRANSPORTING low.
    ENDLOOP.
  ENDIF.


  IF ( NOT p_nf_ter IS INITIAL ).

    LOOP AT p_nf_ter INTO wa_nf_terc.
      vg_tabix = sy-tabix.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_nf_terc-low
        IMPORTING
          output = wa_nf_terc-low.

      MODIFY p_nf_ter FROM wa_nf_terc INDEX vg_tabix TRANSPORTING low.

    ENDLOOP.

  ENDIF.



  wa_formaconfirmacao-sign   = 'I'.
  wa_formaconfirmacao-option = 'EQ'.

  IF ( fc_digi = 'X' ).
    wa_formaconfirmacao-low = 'M'.
    APPEND wa_formaconfirmacao TO r_zlest0039.
  ELSEIF ( fc_auto = 'X' ).
    wa_formaconfirmacao-low = 'A'.
    APPEND wa_formaconfirmacao TO r_zlest0039.
  ELSE.

    CLEAR: wa_formaconfirmacao-sign,
           wa_formaconfirmacao-option.

    wa_formaconfirmacao-sign   = 'I'.
    wa_formaconfirmacao-option = 'EQ'.

    wa_formaconfirmacao-low  = 'M'.
    wa_formaconfirmacao-high = 'M'.
    APPEND wa_formaconfirmacao TO r_zlest0039.

    wa_formaconfirmacao-low  = 'A'.
    wa_formaconfirmacao-high = 'A'.
    APPEND wa_formaconfirmacao TO r_zlest0039.
  ENDIF.

  wa_status-sign   = 'I'.
  wa_status-option = 'EQ'.

  IF ( s_trans = 'X' ).
    wa_status-low = 'ET'.
    APPEND wa_status TO r_zlest0039_s.
  ELSEIF ( s_transb = 'X' ).
    wa_status-low = 'L1'.
    APPEND wa_status TO r_zlest0039_s.
  ELSE.

    CLEAR: wa_status-sign,
           wa_status-option.

    wa_status-sign   = 'I'.
    wa_status-option = 'EQ'.

    wa_status-low  = 'ET'.
    wa_status-high = 'ET'.
    APPEND wa_status TO r_zlest0039_s.

    wa_status-low  = 'L1'.
    wa_status-high = 'L1'.
    APPEND wa_status TO r_zlest0039_s.

    wa_status-low  = 'L2'.
    wa_status-high = 'L2'.
    APPEND wa_status TO r_zlest0039_s.

    wa_status-low  = 'L3'.
    wa_status-high = 'L3'.
    APPEND wa_status TO r_zlest0039_s.

  ENDIF.

ENDFORM.                    " CONDICOES

*----------------------------------------------------------------------*
*   Form  F_SAIDA
*----------------------------------------------------------------------*
FORM f_saida.

  DATA: day(2)   TYPE c,
        month(2) TYPE c,
        year(4)  TYPE c.


  SORT: it_zlest0039         BY vbeln,
        it_t001w             BY werks,
        it_lfa1_coleta       BY lifnr,
        it_lfa1_pontoentrega BY lifnr,
        it_kna1_transb       BY kunnr,
        it_kna1_transb_ef    BY kunnr,
        it_zdco_vinculo      BY vbeln,
        it_vbap              BY vbeln,
        it_makt              BY matnr,
        it_j_1bnflin         BY docnum,
        it_j_1bagnt          BY cfop,
        it_vbak              BY vbeln,
        it_vbfa              BY vbelv.

  "SORT it_zcarta_correcao DESCENDING BY dt_atualizado hr_atualizado.
  SORT it_zcarta_correcao DESCENDING BY dt_authcod hr_authcod.

  LOOP AT it_zlest0039 INTO wa_zlest0039.

*    AUTHORITY-CHECK OBJECT 'ZCENT_WERK'
*             ID 'ZCENT_WERK' FIELD wa_zlest0039-werks.
*
*    IF  ( sy-subrc EQ 0 ).

    CLEAR: wa_t001w,
           wa_lfa1_coleta,
           wa_lfa1_pontoentrega,
           wa_kna1_transb,
           wa_zdco_vinculo,
           wa_vbap,
           wa_makt,
           wa_j_1bnflin,
           wa_j_1bagnt,
           wa_zlest0041,
           wa_saida.

    "Nota fiscal de terceiros
    READ TABLE it_zlest0041 INTO wa_zlest0041 WITH KEY docnum        = wa_zlest0039-docnum
                                                       nr_nf_propria = wa_zlest0039-nfenum.
    IF sy-subrc = 0.
      wa_saida-nfe_terc = wa_zlest0041-nr_nf.
    ENDIF.

    READ TABLE it_zsdt0001c INTO DATA(wa_zsdt01c) WITH KEY nro_nf_prod = wa_zlest0039-docnum.
    IF sy-subrc IS INITIAL.
      READ TABLE it_zsdt01ex INTO DATA(wa_zsdt01ex) WITH KEY id_carga = wa_zsdt01c-id_carga.
      IF sy-subrc IS INITIAL.
        READ TABLE it_zsdt0001e INTO DATA(wa_zsdt01e) WITH KEY id_carga = wa_zsdt01ex-id_carga.
        IF sy-subrc IS INITIAL.
          IF wa_saida-nfe_terc IS INITIAL.
            wa_saida-nfe_terc = wa_zsdt01e-nfnum.
          ENDIF.
          wa_zsdt01e-series = |{ wa_zsdt01e-series ALPHA = IN }|.
          READ TABLE it_j_1bnfdoc_aux INTO DATA(wa_j_1bnfdoc_aux) WITH KEY nfenum = wa_zsdt01e-nfnum
                                                                           series = wa_zsdt01e-series
                                                                           parid  = wa_zsdt01e-parid
                                                                           bukrs  = wa_zsdt01e-bukrs
                                                                           branch = wa_zsdt01e-branch.
          IF sy-subrc EQ 0.
            wa_saida-docnum_nfe_ter = wa_j_1bnfdoc_aux-docnum.
            IF wa_saida-nfe_terc IS INITIAL.
              wa_saida-nfe_terc = wa_j_1bnfdoc_aux-nfenum.
            ENDIF.
          ELSE.
            wa_zsdt01e-series = |{ wa_zsdt01e-series ALPHA = OUT }|.
            READ TABLE it_j_1bnfdoc_aux INTO wa_j_1bnfdoc_aux WITH KEY nfenum = wa_zsdt01e-nfnum
                                                                       series = wa_zsdt01e-series
                                                                       parid  = wa_zsdt01e-parid
                                                                       bukrs  = wa_zsdt01e-bukrs
                                                                       branch = wa_zsdt01e-branch.
            IF sy-subrc EQ 0.
              wa_saida-docnum_nfe_ter = wa_j_1bnfdoc_aux-docnum.
              IF wa_saida-nfe_terc IS INITIAL.
                wa_saida-nfe_terc = wa_j_1bnfdoc_aux-nfenum.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_nf_ter IS NOT INITIAL AND wa_saida-nfe_terc NOT IN p_nf_ter.
      CONTINUE.
    ENDIF.
    "==========================================================================================================Inicio CS2022000139 / 03.03.2022 / Anderson Oenning

    wa_saida-chave_nfe_pro    = wa_zlest0039-chave_nfe.
    wa_saida-docnum_nfe_pro   = wa_zlest0039-docnum.
    wa_saida-dt_atualizacao   = wa_zlest0039-dt_atualizacao.
    wa_saida-us_atualizacao   = wa_zlest0039-us_atualizacao.
    wa_saida-tp_importacao_l1 = wa_zlest0039-tp_importacao_l1.
*    wa_saida-ck_estornar_trans = wa_zlest0039-ck_estornar_trans.

** Ini - RJF
*    READ TABLE it_zsdt0001c INTO DATA(wa_zsdt01c) WITH KEY nro_nf_prod = wa_zlest0039-docnum.
*    IF sy-subrc IS INITIAL.
*      READ TABLE it_zsdt01ex INTO DATA(wa_zsdt01ex) WITH KEY id_carga = wa_zsdt01c-id_carga.
*      IF sy-subrc IS INITIAL.
*        READ TABLE it_zsdt0001e INTO DATA(wa_zsdt01e) WITH KEY id_carga = wa_zsdt01ex-id_carga.
*        IF sy-subrc IS INITIAL.
*          wa_saida-nfnum_c = wa_zsdt01e-nfnum.
*        ENDIF.
*      ENDIF.
*    ENDIF.
** Fim - RJF

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY docnum = wa_zlest0039-docnum  BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      READ TABLE it_j_1bagnt  INTO wa_j_1bagnt  WITH KEY cfop   = wa_j_1bnflin-cfop BINARY SEARCH.
      IF  ( NOT p_cfop IS INITIAL ) AND ( wa_j_1bagnt-cfop NOT IN p_cfop ).
        CONTINUE.
      ENDIF.
      wa_saida-cfotxt = wa_j_1bagnt-cfotxt. " Natureza da Operação
      wa_saida-cfop   = wa_j_1bagnt-cfop. " cfop

    ENDIF.

    READ TABLE it_j_1bnfdoc INTO DATA(ws_j_1bnfdoc) WITH KEY series  = wa_zlest0041-serie
                                                              branch = wa_zlest0041-centro_comprador
                                                              parid  = wa_zlest0041-cod_cliente
                                                              nfenum = wa_zlest0041-nr_nf.

    IF sy-subrc EQ 0.
      wa_saida-docnum_nfe_ter = ws_j_1bnfdoc-docnum.

      READ TABLE it_j_1bnfe_active INTO DATA(ws_active) WITH KEY docnum  = ws_j_1bnfdoc-docnum.
      IF sy-subrc EQ 0.
        "Concatena dados chave.
        wa_saida-chave_nfe_ter = ws_active-regio
        && ws_active-nfyear
        && ws_active-nfmonth
        && ws_active-stcd1
        && ws_active-model
        && ws_active-serie
        && ws_active-nfnum9
        && ws_active-docnum9
        && ws_active-cdv.
      ENDIF.

      IF wa_j_1bnflin-meins NE 'KG'.
        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = wa_j_1bnflin-matnr
            i_mein1             = wa_j_1bnflin-meins
            i_meins             = 'KG'
            i_menge             = wa_j_1bnflin-menge
          IMPORTING
            menge               = v_menge
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          wa_saida-menge = v_menge.
        ENDIF.
      ELSE.

      ENDIF.
    ENDIF.

    READ TABLE it_likp INTO DATA(ws_likp) WITH KEY vbeln = wa_zlest0039-vbeln.
    IF sy-subrc EQ 0.
      READ TABLE it_zsdt0001 INTO DATA(ws_zsdt0001) WITH KEY ch_referencia = ws_likp-xblnr.
      IF sy-subrc EQ 0.
        wa_saida-nr_romaneio_s = ws_zsdt0001-nr_romaneio.
      ENDIF.
    ENDIF.
    READ TABLE it_zcarta_correcao INTO DATA(ws_zcarta_correcao) WITH KEY docnum = wa_zlest0039-docnum.
    IF sy-subrc EQ 0.
      wa_saida-id_carta_cor      = ws_zcarta_correcao-authcode.
      wa_saida-data_carta_cor    = ws_zcarta_correcao-dt_authcod.
      wa_saida-hora_carta_cor    = ws_zcarta_correcao-hr_authcod.
      wa_saida-doc_material_cc   = ws_zcarta_correcao-doc_material.

      "User Story 153789 // MMSILVA - 03.10.2024
      LOOP AT it_mseg INTO DATA(ls_mseg) WHERE mblnr = ws_zcarta_correcao-doc_material.
        IF ls_mseg-shkzg EQ 'H'.
          wa_saida-lgort_de = ls_mseg-lgort.
        ELSEIF ls_mseg-shkzg EQ 'S'.
          wa_saida-lgort_para = ls_mseg-lgort.
        ENDIF.
      ENDLOOP.
      "User Story 153789 // MMSILVA - 03.10.2024
    ENDIF.
    "==========================================================================================================Fim CS2022000139 / 03.03.2022 / Anderson Oenning


*    IF NOT p_nf_ter IS INITIAL AND wa_saida-nfe_terc NOT IN p_nf_ter.
*      CONTINUE.
*    ENDIF.

    wa_saida-werks       = wa_zlest0039-werks.       " Centro
    wa_saida-pontocoleta = wa_zlest0039-pontocoleta. " Ponto Coleta
    wa_saida-matnr       = wa_zlest0039-matnr.       " Material
    wa_saida-ebeln       = wa_zlest0039-ebeln.       " Ordem de Venda
    wa_saida-vbeln       = wa_zlest0039-vbeln.       " Documento de Remessa
    wa_saida-nfenum      = wa_zlest0039-nfenum.      " Nota Fiscal
    wa_saida-placa_cav   = wa_zlest0039-placa_cav.   " Placa Cavalo
    wa_saida-reftyp      = wa_zlest0039-reftyp.
    wa_saida-docnum      = wa_zlest0039-docnum.
    wa_saida-pontotransb = wa_zlest0039-pontotransb.
    wa_saida-obs         = wa_zlest0039-observacao.
    wa_saida-peso_liq    = wa_zlest0039-pesoliquido.
    wa_saida-status      = wa_zlest0039-status.
    wa_saida-ck_estornar_trans = wa_zlest0039-ck_estornar_trans.


    "WA_SAIDA-TRANSB_EFETIVO = WA_ZLEST0039-TRANSB_EFETIVO.
    CLEAR wa_kna1_transb.
    READ TABLE it_kna1_transb_ef INTO wa_kna1_transb WITH KEY kunnr = wa_zlest0039-transb_efetivo BINARY SEARCH.
    wa_saida-transb_efetivo  = wa_kna1_transb-name1.
    wa_saida-ort01_transb_ef = wa_kna1_transb-ort01.

    CLEAR: day, month, year.
    day   =  wa_zlest0039-datasaida+6(2) .
    month =  wa_zlest0039-datasaida+4(2) .
    year  =  wa_zlest0039-datasaida(4)   .
    CONCATENATE day '/' month '/' year INTO wa_saida-datasaida.


    wa_saida-datasaida_aux   = wa_zlest0039-datasaida.   " Data Saída
    wa_saida-pesosaida       = wa_zlest0039-pesosaida.   " Peso Saída

    IF ( wa_zlest0039-pontotransb NE space ).

      wa_saida-datatransb_aux   = wa_zlest0039-datatransb.
      wa_saida-datatransb_aux_b = wa_zlest0039-datatransb.

      CLEAR: day, month, year.
      day   =  wa_zlest0039-datatransb+6(2) .
      month =  wa_zlest0039-datatransb+4(2) .
      year  =  wa_zlest0039-datatransb(4) .
      CONCATENATE day '/' month '/' year INTO wa_saida-datatransb.


      wa_saida-pesotransb  = wa_zlest0039-pesotransb.  " Peso Transbordo

      " Calculo para diferenca de peso, Peso de Transbordo (Descarga) - Peso Saída = diferenca
      wa_saida-difer       = wa_zlest0039-pesotransb - wa_zlest0039-pesosaida.

      " Calculo para diferença de dias
      IF ( wa_zlest0039-datatransb IS INITIAL ).
        wa_saida-diatras     = sy-datum - wa_zlest0039-datasaida.
      ELSE.
        wa_saida-diatras     = wa_zlest0039-datatransb - wa_zlest0039-datasaida.
      ENDIF.

      IF ( wa_zlest0039-datatransb EQ wa_zlest0039-datasaida ).
        wa_saida-diatras = 0.
      ENDIF.

    ELSE.

      " Senão existir transbordo utilizar a data de chegada.
      wa_saida-datatransb_aux    = wa_zlest0039-datachegada. " Data Chegada
      wa_saida-datatransb_aux_b  = wa_zlest0039-datachegada. " Data Chegada

      CLEAR: day, month, year.
      day   =  wa_zlest0039-datachegada+6(2) .
      month =  wa_zlest0039-datachegada+4(2) .
      year  =  wa_zlest0039-datachegada(4) .
      CONCATENATE day '/' month '/' year INTO wa_saida-datatransb.

      wa_saida-pesotransb = wa_zlest0039-pesochegada. " Peso Chegada

      wa_saida-datachegada_aux = wa_zlest0039-datachegada. " Data Chegada
      wa_saida-datachegada_aux_b = wa_zlest0039-datachegada. " Data Chegada

      CLEAR: day, month, year.
      day   =  wa_zlest0039-datachegada+6(2) .
      month =  wa_zlest0039-datachegada+4(2) .
      year  =  wa_zlest0039-datachegada(4)   .
      CONCATENATE day '/' month '/' year INTO wa_saida-datachegada.

      wa_saida-pesochegada = wa_zlest0039-pesochegada. " Peso Chegada

      " Caso o peso chegada não seja iniciado, colocar o peso de saída com o valor negativo no lugar da chegada.
      IF ( wa_zlest0039-pesochegada IS INITIAL ).
        wa_saida-pesochegada = ( wa_zlest0039-pesosaida ) * ( -1 ).
      ENDIF.

      " Calculo para diferenca de peso, Peso de Transbordo (Descarga) - Peso Saída = diferenca
      wa_saida-difer = wa_zlest0039-pesochegada - wa_zlest0039-pesosaida.

      " Calculo para diferença de dias
      IF wa_zlest0039-datachegada IS INITIAL.
        wa_saida-diatras = sy-datum - wa_zlest0039-datasaida.
      ELSE.
        wa_saida-diatras = wa_zlest0039-datachegada - wa_zlest0039-datasaida.
      ENDIF.

      IF ( wa_zlest0039-datachegada EQ wa_zlest0039-datasaida ).
        wa_saida-diatras = 0.
      ENDIF.

    ENDIF.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zlest0039-werks BINARY SEARCH.
    IF  ( NOT p_centro IS INITIAL ) AND ( wa_t001w-werks NOT IN p_centro ).
      CONTINUE.
    ENDIF.
    wa_saida-name1 = wa_t001w-name1. " Centro

    READ TABLE it_lfa1_coleta INTO wa_lfa1_coleta WITH KEY lifnr = wa_zlest0039-pontocoleta BINARY SEARCH.
    wa_saida-name1_pontc = wa_lfa1_coleta-name1. " Nome do Remetente
    wa_saida-stras       = wa_lfa1_coleta-stras. " Endereço do Remetente
    wa_saida-ort01       = wa_lfa1_coleta-ort01. " Municipio Remetente

    CLEAR wa_kna1_transb.
    READ TABLE it_kna1_transb INTO wa_kna1_transb WITH KEY kunnr = wa_zlest0039-pontotransb BINARY SEARCH.
    wa_saida-name1_transb = wa_kna1_transb-name1.
    wa_saida-ort01_transb = wa_kna1_transb-ort01.

    READ TABLE it_lfa1_pontoentrega INTO wa_lfa1_pontoentrega WITH KEY lifnr = wa_zlest0039-pontoentrega BINARY SEARCH.

    IF ( wa_zlest0039-pontotransb EQ space ).

      CLEAR: wa_kna1_transb.
      SELECT SINGLE * FROM kna1 WHERE kunnr EQ wa_zlest0039-pontoentrega.

      wa_saida-cod_dest    = kna1-kunnr.
      wa_saida-name1_desti = kna1-name1.
      wa_saida-ort01_dst   = kna1-ort01.

    ELSE.

      wa_saida-cod_dest    = wa_lfa1_pontoentrega-lifnr.
      wa_saida-name1_desti = wa_lfa1_pontoentrega-name1.
      wa_saida-ort01_dst   = wa_lfa1_pontoentrega-ort01.
    ENDIF.

    wa_saida-pontoentrega = wa_zlest0039-pontoentrega.

    READ TABLE it_zdco_vinculo INTO wa_zdco_vinculo WITH KEY vbeln = wa_zlest0039-vbeln BINARY SEARCH.
    wa_saida-nr_dco = wa_zdco_vinculo-nr_dco. " Número do DCO

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_zlest0039-ebeln BINARY SEARCH.

    IF ( wa_zlest0039-reftyp NE 'BI' ).
      SELECT SINGLE * FROM eket WHERE ebeln = wa_zlest0039-ebeln.
      IF  ( NOT p_lote IS INITIAL ) AND ( eket-charg NOT IN p_lote ).
        CONTINUE.
      ENDIF.
      "Saida
      wa_saida-charg = eket-charg. " Lote - Safra ( somente ano )
    ELSE.
      IF  ( NOT p_lote IS INITIAL ) AND ( wa_vbap-charg NOT IN p_lote ).
        CONTINUE.
      ENDIF.
      "Saida
      wa_saida-charg = wa_vbap-charg. " Lote - Safra ( somente ano )
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zlest0039-matnr BINARY SEARCH.
    wa_saida-maktx = wa_makt-maktx. " Produto - Descrição do Material


    " Verificação da duplicação da placa do cavalo.
    CASE wa_zlest0039-status_placa.
      WHEN 1.
        wa_saida-duplic = icon_green_light.
      WHEN 2.
        wa_saida-duplic = icon_red_light.
      WHEN 3.
        wa_saida-duplic = icon_yellow_light.
    ENDCASE.

*"// WBARBOSA 16102024 - US-154985
    IF wa_zlest0039-eudr EQ gc_atende_eudr.
      wa_saida-icon_eudr = icon_checked.
    ENDIF.
*"// WBARBOSA 16102024 - US-154985

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zlest0039-ebeln.
    CASE wa_vbak-kvgr3.
      WHEN: 'R'.
        wa_saida-kvgr3 = 'R-RR'.
      WHEN: 'C'.
        wa_saida-kvgr3 = 'C-Convencional'.
    ENDCASE.

    CASE wa_zlest0039-tp_transgenia.
      WHEN: 'R' OR 'T'.
        wa_saida-tp_transgenia = 'R-RR'.
      WHEN: 'C'.
        wa_saida-tp_transgenia = 'C-Convencional'.
    ENDCASE.

    READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbelv = wa_zlest0039-vbeln.
    wa_saida-rfwrt = wa_vbfa-rfwrt.

    READ TABLE it_ctes WITH KEY cte_docnum_nf = wa_zlest0039-docnum INTO DATA(wa_cte) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-cte_docnum           = wa_cte-cte_docnum.
      wa_saida-cte_numero           = wa_cte-cte_numero.
      wa_saida-cte_valor            = wa_cte-cte_valor.
      wa_saida-cte_vlr_unitario     = wa_cte-cte_vlr_unitario.
      wa_saida-cte_unidade_valor    = wa_cte-cte_unidade_valor.
      wa_saida-cte_cd_contratado    = wa_cte-cte_cd_contratado.
      wa_saida-cte_ds_contratado    = wa_cte-cte_ds_contratado.
      wa_saida-cte_cnpj_contratado  = wa_cte-cte_cnpj_contratado.
    ENDIF.


*-------------------------------------------------------------------------------*
*   Dados CCT
*-------------------------------------------------------------------------------*

    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = wa_saida-docnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      wa_saida-peso_aferido_recepcao = wl_zlest0146-peso_aferido_recepcao.

      READ TABLE lt_zlest0147 INTO DATA(wl_0147) INDEX 1.
      IF sy-subrc EQ 0.
        wa_saida-peso_fiscal_cct = wl_0147-peso_fiscal.

        IF wl_0147-complemento EQ abap_true.
          wa_saida-peso_cct = wl_0147-peso_fiscal.
        ELSE.
          wa_saida-peso_cct = wl_zlest0146-peso_aferido_recepcao.
        ENDIF.

      ENDIF.
*
      wa_saida-dt_recepcao_cct       = wl_zlest0146-dt_recepcao.
      wa_saida-dif_peso_cct_nf       = wa_saida-menge - wa_saida-peso_cct.


      SELECT SINGLE *
        FROM zsdt0168 INTO @DATA(_wl_0168)
       WHERE codigo_ra EQ @wl_zlest0146-local_codigo_ra.

      IF ( sy-subrc EQ 0 ) AND ( wl_zlest0146-local_codigo_ra IS NOT INITIAL ).
        wa_saida-term_cct = _wl_0168-lifnr.

        IF _wl_0168-lifnr IS NOT INITIAL.
          SELECT SINGLE name1
            FROM lfa1 INTO wa_saida-ds_term_cct
           WHERE lifnr = _wl_0168-lifnr.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE t_sumret INTO sl_sumret
       WITH KEY docnum = wa_zlest0039-docnum
       BINARY SEARCH.


**** RETLOTE WSB INICIO
****Conta quantos arquivos de Lotes o documento tem
    CLEAR: index, index_doc.
    LOOP AT t_retlote INTO sl_retlote WHERE docnum EQ wa_j_1bnflin-docnum.
      index = index + 1.
    ENDLOOP.

    "Verifica saldo.

    LOOP AT t_retlote INTO sl_retlote WHERE docnum EQ wa_j_1bnflin-docnum.
      index_doc = sy-tabix.
      retlot = 'X'.
****** no Loop retlote o index é comparado ao numero de registro da quantidade de
****** DocRetorno para ser impresso a Diferença somente no ultimo Documento.
      IF index EQ index_doc.
        wa_saida-menge = wa_j_1bnflin-menge.
        wa_saida-saldo = wa_j_1bnflin-menge - sl_sumret-quant_vinc.  "Original
      ELSE.
        wa_saida-saldo = 0.
        wa_saida-menge = 0.
      ENDIF.
    ENDLOOP.

    READ TABLE t_retlote TRANSPORTING NO FIELDS WITH KEY docnum = wa_zlest0039-docnum.
    IF sy-subrc IS NOT INITIAL.
      wa_saida-saldo = wa_j_1bnflin-menge.
    ENDIF.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida,
           wa_zlest0039,
           wa_vbfa,
           wa_j_1bnflin,
           wa_vbak.

  ENDLOOP.




  "Deletar o CFOP que encontrar na tabela ZLEST0077(transação ZLES0093)
  CLEAR: wa_saida.
  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<ls_saida>).


    "Nota de terceiro.
    IF <ls_saida>-nfe_terc IS NOT INITIAL.
*-------------------------------------------------------------------------------*
*   Caso nota estiver vinculada em uma ou mais DU-e's, saida deve ser gerada por DU-e
*-------------------------------------------------------------------------------*
      CLEAR: v_count_dues.
      LOOP AT tg_znom_reme_notas WHERE docnum EQ <ls_saida>-docnum
                                   AND id_due IS NOT INITIAL.
        READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
        CHECK sy-subrc EQ 0.
        ADD 1 TO v_count_dues.
      ENDLOOP.


**************************************************************************************
* Processo especifico para Algodão IR047064
**************************************************************************************
      CLEAR v_count_nf_prod.
      LOOP AT tg_nf_produtor_algodao INTO wa_nf_produtor WHERE docnum_prod EQ <ls_saida>-docnum.
        ADD 1 TO v_count_nf_prod.
      ENDLOOP.


      READ TABLE it_zlest0041 INTO wa_zlest0041 WITH KEY docnum        = <ls_saida>-docnum
                                                         nr_nf_propria = <ls_saida>-nfenum.

      READ TABLE tg_j_1bnfdoc WITH KEY series  = wa_zlest0041-serie
                                        branch = wa_zlest0041-centro_comprador
                                        parid  = wa_zlest0041-cod_cliente
                                        nfenum = wa_zlest0041-nr_nf.


      <ls_saida>-itmnum     = tg_j_1bnfdoc-itmnum.
      IF tg_j_1bnfdoc-matkl EQ '700140'.

        v_count_saida_nf = 0.
        v_count_dues = 0.
        LOOP AT tg_nf_produtor_algodao INTO wa_nf_produtor WHERE docnum_prod EQ <ls_saida>-docnum.

          READ TABLE tg_znom_reme_notas WITH KEY docnum = wa_nf_produtor-docnum_prod id_nomeacao_tran = wa_nf_produtor-id_nomeacao_tran.
          IF sy-subrc IS INITIAL AND tg_znom_reme_notas-id_due IS NOT INITIAL.
            READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
            CHECK sy-subrc EQ 0.
            <ls_saida>-qtde_vinc_due     = tg_znom_reme_notas-nr_quantidade.
          ELSE.
            CLEAR:
              <ls_saida>-qtde_vinc_due,
              tg_zsdt0170.
          ENDIF.

          ADD 1 TO v_count_saida_nf.
          <ls_saida>-qtde_vinc_due     = wa_nf_produtor-menge.
*

          tg_znom_reme_notas-docnum = wa_nf_produtor-docnum_prod.
          tg_znom_reme_notas-nr_quantidade = wa_nf_produtor-menge.
          APPEND tg_znom_reme_notas TO tg_znom_reme_notas_vinc.

          IF v_count_nf_prod EQ v_count_saida_nf.

            IF ( <ls_saida>-peso_cct > 0 ) AND ( <ls_saida>-peso_cct <= <ls_saida>-menge ).
              <ls_saida>-saldo_exportar = <ls_saida>-peso_cct.
            ELSE.
              <ls_saida>-saldo_exportar = <ls_saida>-menge.
            ENDIF.

            LOOP AT tg_znom_reme_notas_vinc WHERE docnum EQ <ls_saida>-docnum.
              SUBTRACT tg_znom_reme_notas_vinc-nr_quantidade FROM <ls_saida>-saldo_exportar.
            ENDLOOP.
          ENDIF.

          READ TABLE tg_zsdt0276_tot WITH  KEY docnum = <ls_saida>-docnum
                                               itmnum = <ls_saida>-itmnum.
          IF sy-subrc EQ 0.
            <ls_saida>-qtde_baixada = tg_zsdt0276_tot-menge.
          ENDIF.

          <ls_saida>-saldo_exportar = <ls_saida>-saldo_exportar - <ls_saida>-qtde_baixada.

        ENDLOOP.
      ENDIF.

      IF v_count_dues EQ 0.

        CHECK tg_j_1bnfdoc-matkl NE '700140'.

        IF ( <ls_saida>-peso_cct > 0 ) AND ( <ls_saida>-peso_cct <= <ls_saida>-menge ).
          <ls_saida>-saldo_exportar = <ls_saida>-peso_cct.
        ELSE.
          <ls_saida>-saldo_exportar = wa_saida-menge.
        ENDIF.

*** PBI 54906 - Inicio
        READ TABLE tg_zsdt0276_tot WITH  KEY docnum = <ls_saida>-docnum
                                             itmnum = <ls_saida>-itmnum.
        IF sy-subrc EQ 0.
          <ls_saida>-qtde_baixada = tg_zsdt0276_tot-menge.
        ENDIF.

        <ls_saida>-saldo_exportar = <ls_saida>-saldo_exportar - <ls_saida>-qtde_baixada.

      ELSE.

        v_count_saida_doc = 0.

        LOOP AT tg_znom_reme_notas WHERE docnum EQ <ls_saida>-docnum
                                     AND id_due IS NOT INITIAL.

          READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
          CHECK sy-subrc EQ 0.

          ADD 1 TO v_count_saida_doc.

          wa_saida-qtde_vinc_due     = tg_znom_reme_notas-nr_quantidade.

          APPEND tg_znom_reme_notas TO tg_znom_reme_notas_vinc.


          IF v_count_dues = v_count_saida_doc.
            IF ( <ls_saida>-peso_cct > 0 ) AND ( <ls_saida>-peso_cct <= <ls_saida>-menge ).
              <ls_saida>-saldo_exportar = wa_saida-peso_cct.
            ELSE.
              <ls_saida>-saldo_exportar = <ls_saida>-menge.
            ENDIF.

            LOOP AT tg_znom_reme_notas_vinc WHERE docnum EQ <ls_saida>-docnum.
              SUBTRACT tg_znom_reme_notas_vinc-nr_quantidade FROM <ls_saida>-saldo_exportar.
            ENDLOOP.
          ENDIF.

          READ TABLE tg_zsdt0276_tot WITH  KEY docnum = <ls_saida>-docnum
                                               itmnum = <ls_saida>-itmnum.
          IF sy-subrc EQ 0.
            <ls_saida>-qtde_baixada = tg_zsdt0276_tot-menge.
          ENDIF.

          <ls_saida>-saldo_exportar = <ls_saida>-saldo_exportar - <ls_saida>-qtde_baixada.

          CLEAR: tg_zsdt0170, tg_zsdt0276_tot.
        ENDLOOP.
      ENDIF.
    ENDIF.

    gw_tabix = sy-tabix.
    SELECT SINGLE * FROM zlest0077 INTO wa_zlest0077 WHERE cfop EQ <ls_saida>-cfop.
    IF ( sy-subrc EQ 0 ).
      <ls_saida>-check_cfop_delet = abap_true.
    ENDIF.
    CLEAR: gw_tabix, wa_zlest0041.
  ENDLOOP.

  SORT it_saida BY check_cfop_delet.
  DELETE it_saida WHERE check_cfop_delet EQ abap_true.

  SORT it_saida BY docnum.

  "Inicio=======================================CS2019001140 - 06.07.2022 - RJF
  IF gv_doc_p IS NOT INITIAL AND it_saida IS INITIAL.
    " Messagem
    FREE gv_doc_p.
    SET PARAMETER ID 'JEF' FIELD gv_doc_p.
    FREE MEMORY ID 'JEF'.
    MESSAGE i903.
    LEAVE PROGRAM.
  ELSEIF gv_doc_p IS NOT INITIAL.
    FREE gv_doc_p.
    SET PARAMETER ID 'JEF' FIELD gv_doc_p.
    FREE MEMORY ID 'JEF'.
  ENDIF.
  "Fim=======================================CS2019001140 - 06.07.2022 - RJF


  PERFORM: z_atrib_conf_cct_portal.
  PERFORM: z_atrib_conf_cct_portal_p.
*  PERFORM: z_ajusta_saldo_cct_portal_prod.


ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv.

  PERFORM alv_preenche_cat USING:

          'CFOTXT'              TEXT-011   '050'   ' '  ' ' ' '     ''           ''                   '', " Natureza da Operação
          'WERKS'               TEXT-012   '005'   ' '  'X' ' '     'WERKS'      'ZLEST0039'          '', " Centro
          'PONTOCOLETA'         TEXT-013   '010'   ' '  ' ' ' '     ''           ''                   '', " Ponto de Coleta
          'NAME1_PONTC'         TEXT-036   '035'   ' '  ' ' ' '     ''           ''                   '', " Nome Ponto de Coleta
          'NAME1'               TEXT-014   '030'   ' '  ' ' ' '     ''           ''                   '', " Remente
          'STRAS'               TEXT-015   '035'   ' '  ' ' ' '     ''           ''                   '', " Endereço
          'ORT01'               TEXT-016   '035'   ' '  ' ' ' '     ''           ''                   '', " Municipio
          'CHARG'               TEXT-017   '010'   ' '  ' ' ' '     ''           ''                   '', " Lote
          'MATNR'               TEXT-018   '018'   ' '  'X' ' '     'MATNR'      'ZLEST0039'          '', " Material
          'MAKTX'               TEXT-019   '040'   ' '  ' ' ' '     ''           ''                   '', " Produto
          'EBELN'               TEXT-020   '010'   'X'  'X' ' '     ''           ''                   '', " Ordem de Venda
          'KVGR3'               TEXT-052   '020'   ' '  ' ' ' '     ''           ''                   '',  "
          'VBELN'               TEXT-021   '010'   'X'  'X' ' '     ''           ''                   '', " Documento de Remessa
          'RFWRT'               TEXT-053   '020'   ' '  ' ' ' '     ''           ''                   '', " Valor de referência
          'NFENUM'              TEXT-022   '010'   ' '  'X' ' '     'NUMERO'     'ZIB_NFE_DIST_TER'   '', " Nota Fiscal
          'NFE_TERC'            TEXT-051   '010'   ' '  ' ' ' '     ''           ''                   '', " Nfe. Terceiro
          'NR_DCO'              TEXT-023   '020'   ' '  ' ' ' '     ''           ''                   '', " DCO
          'DUPLIC'              TEXT-044   '010'   ' '  ' ' ' '     ''           ''                   '', " Icone para o status de duplicidade
*"// WBARBOSA 16102024 - US-154985
          'ICON_EUDR'           TEXT-089   '012'   ' '  ' ' ' '     ''           ''                   'C', " Atende EUDR
*"// WBARBOSA 16102024 - US-154985
          'PLACA_CAV'           TEXT-024   '010'   ' '  ' ' ' '     ''           ''                   '', " Placa do Cavalo
          'DATASAIDA'           TEXT-025   '010'   ' '  ' ' ' '     ''           ''                   '', " Data Saída
          'PESOSAIDA'           TEXT-026   '013'   ' '  ' ' ' '     ''           ''                   '', " Peso Saída
          'DATATRANSB'          TEXT-027   '010'   ' '  ' ' ' '     ''           ''                   '', " Data de Descarga
          'PESOTRANSB'          TEXT-028   '015'   ' '  ' ' ' '     ''           ''                   '', " Peso de Descarga
          'DIFER'               TEXT-029   '013'   ' '  ' ' ' '     ''           ''                   '', " Peso de Descarga - Peso Sa ída
          'DIATRAS'             TEXT-030   '010'   ' '  'X' ' '     ''           ''                   '', " Data Chegada - Data Saída
          'PESO_LIQ'            TEXT-046   '010'   ' '  ' ' ' '     ''           ''                   '', " Peso Liquido
          'PONTOTRANSB'         TEXT-050   '010'   ' '  ' ' ' '     ''           ''                   '', " Cod.Transb
          'NAME1_TRANSB'        TEXT-031   '035'   ' '  ' ' ' '     ''           ''                   '', " Local de Entrega
          'ORT01_TRANSB'        TEXT-032   '035'   ' '  ' ' ' '     ''           ''                   '', " Municipio Transbordo
          'COD_DEST'            TEXT-043   '010'   ' '  'X' ' '     ''           ''                   '', " Código do Destinatário cod_dest
          'NAME1_DESTI'         TEXT-033   '035'   ' '  ' ' ' '     ''           ''                   '', " Destinatário
          'ORT01_DST'           TEXT-034   '035'   ' '  ' ' ' '     ''           ''                   '', " Municipio Destinario
          'TRANSB_EFETIVO'      TEXT-054   '035'   ' '  ' ' ' '     ''           ''                   '', " Local Transb. Efetivo
          'ORT01_TRANSB_EF'     TEXT-055   '035'   ' '  ' ' ' '     ''           ''                   '', " Munic. Transb. Efetivo
          'STATUS'              TEXT-048   '005'   ' '  ' ' ' '     ''           ''                   '', " Status
          'OBS'                 TEXT-035   '132'   ' '  ' ' ' '     ''           ''                   '', " Observação
          'CK_ESTORNAR_TRANS'   TEXT-056   '132'   ' '  ' ' ' '     ''           ''                   '', " Estornar Transito da Mercadoria
          'TP_TRANSGENIA'       TEXT-057   '020'   ' '  ' ' ' '     ''           ''                   '', " Tipo de Trangenia
          'CTE_DOCNUM'          TEXT-058   '020'   ' '  ' ' 'ALPHA' 'DOCNUM'     'ZCTE_INFO_NOTA '    '', " Docnum da CT-e
          'CTE_NUMERO'          TEXT-059   '020'   'X'  ' ' 'ALPHA' 'NCT'        'ZCTE_IDENTIFICA'    '', " Número da CT-e
          'CTE_VALOR'           TEXT-060   '020'   ' '  ' ' ' '     ''           ''                   '', " Valor do Frete
          'CTE_VLR_UNITARIO'    TEXT-061   '020'   ' '  ' ' ' '     ''           ''                   '', " Valor do Frete Peso
          'CTE_UNIDADE_VALOR'   TEXT-062   '020'   ' '  ' ' ' '     ''           ''                   '', " Und. Vlr. Frete Peso
          'CTE_CD_CONTRATADO'   TEXT-063   '020'   ' '  ' ' 'ALPHA' ''           ''                   '', " Código do SubContratado
          'CTE_DS_CONTRATADO'   TEXT-064   '020'   ' '  ' ' ' '     ''           ''                   '', " Nome do SubContratado
          'CTE_CNPJ_CONTRATADO' TEXT-065   '020'   ' '  ' ' ' '     ''           ''                   '', " CNPJ do SubContratado.

          'CHAVE_NFE_PRO     ' TEXT-067    '015'   ' '  ' ' ' '     'CHAVE_NFE'  'ZIB_NFE_DIST_TER'   '', " Chave Nfe Próprio.
          'DOCNUM_NFE_PRO    ' TEXT-068    '015'   ' '  ' ' ' '     'DOCNUM   '  'ZLEST0039'          '', " Docnum Nfe Próprio.
          'CHAVE_NFE_TER     ' TEXT-069    '015'   ' '  ' ' ' '     'CHAVE_NFE'  'ZIB_NFE_DIST_TER'   '', " Chave Nfe Terceiro.
          'DOCNUM_NFE_TER    ' TEXT-070    '015'   ' '  ' ' ' '     'DOCNUM   '  'ZLEST0039'          '', " Docnum Nfe Terceiro.

          'NR_ROMANEIO_S     ' TEXT-071    '015'   ' '  ' ' ' '     ''           ''                   '', " Nrº Romaneio.
          'ID_CARTA_COR      ' TEXT-072    '025'   ' '  ' ' ' '     ''           ''                   '', " Id Carta de Correção.
          'DATA_CARTA_COR    ' TEXT-073    '015'   ' '  ' ' ' '     ''           ''                   '', " Data Carta de Correção.
          'HORA_CARTA_COR    ' TEXT-075    '015'   ' '  ' ' ' '     ''           ''                   '', " Data Carta de Correção.
          'DOC_MATERIAL_CC   ' TEXT-074    '015'   ' '  ' ' ' '     ''           ''                   '', " Doc Material.

          'LGORT_DE          ' TEXT-087    '015'   ' '  ' ' ' '     ''           ''                   '', "User Story 153789 // MMSILVA - 03.10.2024 // DE depósito.
          'LGORT_PARA        ' TEXT-088    '015'   ' '  ' ' ' '     ''           ''                   '', "User Story 153789 // MMSILVA - 03.10.2024 // DE depósito.

          'DT_ATUALIZACAO    ' TEXT-076    '010'   ' '  ' ' ' '     ''           ''                   '', " Data Atualização.
          'US_ATUALIZACAO    ' TEXT-077    '020'   ' '  ' ' ' '     ''           ''                   '', " Usuario Atualização.
          'TP_IMPORTACAO_L1  ' TEXT-078    '020'   ' '  ' ' ' '     ''           ''                   ''. " Tp Importação L1.

*          'ck_estornar_trans ' text-079   '030'   ' '  ' ' ' '. " Fleg Estornar Transito Carga SPL.

  PERFORM alv_preenche_cat USING:
  "CS2022000834 Conferência do Status Fiscal   / Anderson Oenning
            'TERM_CCT_PORTAL    ' TEXT-080   '010'   ' '  ' ' ' '  ''  '' '', " Term.CCT Portal
            'DT_RECEPCAO_PORTAL ' TEXT-081   '020'   ' '  ' ' ' '  ''  '' '', " Dt.CCT Portal.
            'SALDO              ' TEXT-082   '020'   ' '  ' ' ' '  ''  '' '', " Saldo a Exportar.

            'DS_TERM_CCT_PORTAL ' TEXT-083   '010'   ' '  ' ' ' '  ''  '' '', "  Ds.Term.CCT Portal produtor
            'DT_RECEPCAO_PORTAL_P ' TEXT-084 '020'   ' '  ' ' ' '  ''  '' '', "  Dt.CCT Portal produtor
            'SALDO_EXPORTAR     ' TEXT-085   '020'   ' '  ' ' ' '  ''  '' ''. "  Saldo Exportar produtor

  "CS2022000834 Conferência do Status Fiscal   / Anderson Oenning

*  PERFORM alv_preenche_cat USING:
*  "CS2022000834 Conferência do Status Fiscal   / Anderson Oenning
*            'NFNUM_C    ' TEXT-086   '010'   ' '  ' ' ' '  ''  ''. " ZSDT0001-NFNUM Compra

ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat USING p_campo        TYPE c
                               p_desc      TYPE c
                               p_tam       TYPE c
                               p_hot       TYPE c
                               p_zero      TYPE c
                               p_convexit  TYPE c
                               p_ref_field TYPE c
                               p_ref_table TYPE c
                               p_just      TYPE c.



  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-convexit  = p_convexit.
  wl_fcat-ref_field = p_ref_field.
  wl_fcat-ref_table = p_ref_table.
  wl_fcat-just = p_just.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA wa_event TYPE REF TO lcl_event_receiver.
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

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

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
MODULE z_exibe_alv OUTPUT.


  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

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
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.


  wa_stable-row  = abap_true.
  wa_layout-zebra = abap_true.      "Código Zebrado
*  wa_layout-no_rowmark = abap_true. "Exclui barra standard de flag a esquerda
  wa_layout-cwidth_opt = 'X'. "Ajusta tamanho na coluna
  wa_layout-col_opt    = 'X'. "Ajusta tamanho na coluna

  wa_layout-sel_mode   = 'A'.
  wa_layout-box_fname  = 'SELECTED'.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida
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

  CALL METHOD wa_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
      WHEN 'COMP'.

        AUTHORITY-CHECK OBJECT 'ZEDIT_COMP' ID 'ACTVT' FIELD '16'.
        IF ( sy-subrc EQ 0 ).
          PERFORM z_handle_command USING sy-ucomm.
        ENDIF.

    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

  DATA opt TYPE ctu_params.

  READ TABLE it_saida INTO wa_saida INDEX p_e_row_id.

  CASE p_e_column_id.
    WHEN 'CTE_NUMERO'.
      PERFORM mostrar_doc_fiscal USING wa_saida-cte_docnum.
    WHEN 'EBELN'.
      IF wa_saida-reftyp NE 'BI'.
        CLEAR: it_bdc[], it_messtab.
        PERFORM f_bdc_field USING: 'X' 'SAPLMEGUI'           '0014'             ,
                                   ' ' 'BDC_OKCODE'	         '=MECHOB'          ,
                                   ' ' 'DYN_6000-LIST'       '1'                ,
                                   'X' 'SAPLMEGUI'           '0002'             ,
                                   ' ' 'BDC_OKCODE'	         '=MEOK'            ,
                                   ' ' 'BDC_SUBSCR'	         'SAPLMEGUI'        ,
                                   ' ' 'BDC_CURSOR'	         'MEPO_SELECT-EBELN',
                                   ' ' 'MEPO_SELECT-EBELN'   wa_saida-ebeln     ,
                                   ' ' 'MEPO_SELECT-BSTYP_F' 'X'.

        opt-dismode = 'E'.
        opt-defsize = 'X'.
        CALL TRANSACTION 'ME23N' USING it_bdc OPTIONS FROM opt MESSAGES INTO it_messtab.
      ELSE.

        SET PARAMETER ID 'AUN' FIELD wa_saida-ebeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      ENDIF.

    WHEN 'VBELN'.
      SET PARAMETER ID 'VL' FIELD wa_saida-vbeln.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
FORM f_bdc_field   USING    VALUE(p_flag)
                            VALUE(p_fnam)
                            VALUE(p_fval).

  CLEAR it_bdc.
  IF NOT p_flag IS INITIAL.
    it_bdc-program  = p_fnam.
    it_bdc-dynpro   = p_fval.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam = p_fnam.
    it_bdc-fval = p_fval.
  ENDIF.
  APPEND it_bdc.

ENDFORM.                    " F_BDC_FIELD

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

  CLEAR sl_toolbar.
  MOVE: 'COMP'                 TO sl_toolbar-function ,
         icon_change           TO sl_toolbar-icon     ,
         TEXT-039              TO sl_toolbar-quickinfo,
         TEXT-039              TO sl_toolbar-text     ,
         space                 TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

  CLEAR sl_toolbar.

  MOVE: 'DANF'                 TO sl_toolbar-function,
         icon_print            TO sl_toolbar-icon     ,
         TEXT-041              TO sl_toolbar-quickinfo,
         TEXT-041              TO sl_toolbar-text     ,
         space                 TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

  CLEAR sl_toolbar.

  MOVE: 'REPROCESSAR'          TO sl_toolbar-function,
         icon_replace          TO sl_toolbar-icon     ,
         TEXT-047              TO sl_toolbar-quickinfo,
         TEXT-047              TO sl_toolbar-text     ,
         space                 TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.


  MOVE: 'IMPORTAR'              TO sl_toolbar-function,
        icon_interchange        TO sl_toolbar-icon,
        TEXT-049                TO sl_toolbar-quickinfo,
        TEXT-049                TO sl_toolbar-text,
        space                   TO sl_toolbar-disabled.

  APPEND sl_toolbar TO p_object->mt_toolbar.


  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

  CLEAR sl_toolbar.

  MOVE: 'REFRESH'          TO sl_toolbar-function,
         icon_view_refresh TO sl_toolbar-icon     ,
         TEXT-066          TO sl_toolbar-quickinfo,
         TEXT-066          TO sl_toolbar-text     ,
         space             TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

*-CS2023000070-21.03.2023-#103477-JT-inicio
*-----------------------------------------
*-----Check Permissão botao Recusa
*-----------------------------------------
  READ TABLE t_parametros INTO w_parametros WITH KEY parid = 'ZLES0050_RECUSA'.

  IF sy-subrc = 0.
    MOVE: 'RECUSA'          TO sl_toolbar-function,
           icon_storno      TO sl_toolbar-icon     ,
           TEXT-086         TO sl_toolbar-quickinfo,
           TEXT-086         TO sl_toolbar-text     ,
           space            TO sl_toolbar-disabled .
    APPEND sl_toolbar       TO p_object->mt_toolbar.
  ENDIF.
*-CS2023000070-21.03.2023-#103477-JT-fim

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm.

  DATA: wa_reprocessar TYPE tbtco.
  CASE p_ucomm.
    WHEN 'COMP'.

      AUTHORITY-CHECK OBJECT 'ZEDIT_COMP' ID 'ACTVT' FIELD '16'.
      IF ( sy-subrc EQ 0 ).
        PERFORM: z_edita_comparativo.
      ENDIF.

    WHEN 'DANF'.
      PERFORM: z_invoca_danfe.

    WHEN 'REPROCESSAR'.

      AUTHORITY-CHECK OBJECT 'ZEDIT_COMP' ID 'ACTVT' FIELD '16'.

      IF ( sy-subrc EQ 0 ).

        SELECT SINGLE *
            INTO wa_reprocessar
            FROM tbtco
           WHERE jobname EQ 'ATUALIZA_COMP_J2'
             AND status EQ 'R'.

        IF ( sy-subrc NE 0 ).
          PERFORM: z_reprocessar.
        ELSE.
          MESSAGE s000(z01) WITH 'Já existe um processo em andamento!'.
          STOP.
        ENDIF.

      ENDIF.

    WHEN 'IMPORTAR'.

      AUTHORITY-CHECK OBJECT 'ZEDIT_COMP' ID 'ACTVT' FIELD '16'.

      IF ( sy-subrc EQ 0 ).
        PERFORM: z_importar.
      ENDIF.

*-CS2023000070-21.03.2023-#103477-JT-inicio
    WHEN 'RECUSA'.
      PERFORM z_recusa_modal.
*-CS2023000070-21.03.2023-#103477-JT-fim

    WHEN 'REFRESH'.

      FREE it_saida.

      PERFORM: f_seleciona_dados     , " Form seleciona dados
               f_saida               . " Form de saida

      CALL METHOD wa_alv->refresh_table_display
        EXPORTING
          is_stable = ls_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND

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
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_RECUSA_MODAL
*&---------------------------------------------------------------------*
FORM z_recusa_modal.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  " Verifica Seleção de Linhas
  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[]      IS INITIAL OR
    lines( tl_rows ) > 1.
    MESSAGE i836 WITH TEXT-087 TEXT-088.
    EXIT.
  ENDIF.

  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  zlest0039-chave_nfe      = wa_saida-chave_nfe_pro.
  zlest0039-nfenum         = wa_saida-nfenum.
  zlest0039-werks          = wa_saida-werks.
  zlest0039-observacao     = wa_saida-obs.
  zlest0039-dt_atualizacao = sy-datum.
  zlest0039-us_atualizacao = sy-uname.

  CALL SCREEN 0300 STARTING AT 18  06
                     ENDING AT 168 12.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_EDITA_COMPARATIVO
*&---------------------------------------------------------------------*
FORM z_edita_comparativo .

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  " Verifica Seleção de Linhas
  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-040.
    EXIT.
  ENDIF.

  CLEAR: it_edicao[], wa_edicao, index_aux. "WA_SAIDA.
  SORT: it_t001w             BY werks,
        it_kna1_transb       BY kunnr,
        it_lfa1_pontoentrega BY lifnr.

  "READ TABLE TL_ROWS INTO SL_ROWS INDEX 1.

  LOOP AT tl_rows INTO sl_rows.
    READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

    index_aux                    = sl_rows-index.

    wa_edicao-in_werks            = wa_saida-werks.
    wa_edicao-in_ebeln            = wa_saida-ebeln.
    wa_edicao-in_vbeln            = wa_saida-vbeln.
    wa_edicao-in_nfenum           = wa_saida-nfenum.
    wa_edicao-in_placa_cav        = wa_saida-placa_cav.
    wa_edicao-in_datasaida        = wa_saida-datasaida_aux.
    wa_edicao-in_pesosaida        = wa_saida-pesosaida.
    wa_edicao-in_estornar_trans   = wa_saida-ck_estornar_trans.
    zlest0039-tp_transgenia       = wa_saida-tp_transgenia.

    IF ( wa_saida-pontotransb NE space ).
      wa_edicao-in_datatransb  = wa_saida-datatransb_aux_b.
      wa_edicao-in_pesotransb  = wa_saida-pesotransb.
    ELSE.
      wa_edicao-in_datatransb  = wa_saida-datachegada_aux_b.
      wa_edicao-in_pesotransb  = wa_saida-pesochegada.
    ENDIF.

    wa_edicao-in_observacao  = wa_saida-obs.
    wa_edicao-in_docnum      = wa_saida-docnum.
    wa_edicao-in_pontotransb = wa_saida-pontotransb.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_saida-werks BINARY SEARCH.
    wa_edicao-in_name1       = wa_t001w-name1.

    READ TABLE it_kna1_transb INTO wa_kna1_transb WITH KEY kunnr = wa_saida-pontotransb BINARY SEARCH.
    wa_edicao-in_name1_transb = wa_kna1_transb-name1.
    wa_edicao-in_ort01_transb = wa_kna1_transb-ort01.

    IF ( wa_saida-pontotransb EQ space ).
      SELECT SINGLE * FROM kna1 WHERE kunnr EQ wa_saida-pontoentrega.
      wa_edicao-in_name1_desti    = kna1-name1.
      wa_edicao-in_ort01_dst      = kna1-ort01.
    ELSE.
      READ TABLE it_lfa1_pontoentrega INTO wa_lfa1_pontoentrega WITH KEY lifnr = wa_saida-pontoentrega BINARY SEARCH.
      wa_edicao-in_name1_desti = wa_lfa1_pontoentrega-name1.
      wa_edicao-in_ort01_dst   = wa_lfa1_pontoentrega-ort01.
    ENDIF.

    IF ( wa_saida-transb_efetivo EQ space ).
      SELECT SINGLE * FROM kna1 WHERE kunnr EQ wa_saida-pontoentrega.
      wa_edicao-in_name1_desti    = kna1-name1.
      wa_edicao-in_ort01_dst      = kna1-ort01.
    ELSE.
      READ TABLE it_lfa1_pontoentrega INTO wa_lfa1_pontoentrega WITH KEY lifnr = wa_saida-pontoentrega BINARY SEARCH.
      wa_edicao-in_name1_desti = wa_lfa1_pontoentrega-name1.
      wa_edicao-in_ort01_dst   = wa_lfa1_pontoentrega-ort01.
    ENDIF.


    CALL SCREEN 0200 STARTING AT 15 10 ENDING AT 85 30.


    CLEAR: sl_rows,
           wa_zlest0039,
           wa_t001w,
           wa_kna1_transb,
           wa_zlest0039-pontotransb,
           wa_lfa1_pontoentrega.

  ENDLOOP.

ENDFORM.                    " Z_EDITA_COMPARATIVO
*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_TABELA
*&---------------------------------------------------------------------*
FORM z_atualizar_tabela.

  " Conversão de data para BR
  DATA: dia(2) TYPE c,
        mes(2) TYPE c,
        ano(4) TYPE c.

  wa_saida_aux-tp_transgenia  = zlest0039-tp_transgenia.
  wa_saida_aux-datatransb  = wa_edicao-in_datatransb.
  wa_saida_aux-pesotransb  = wa_edicao-in_pesotransb.

  wa_saida_aux-datachegada  = wa_edicao-in_datatransb.
  wa_saida_aux-pesochegada  = wa_edicao-in_pesotransb.

  IF wa_edicao-in_pesotransb IS NOT INITIAL.
    wa_saida_aux-difer        = ( wa_edicao-in_pesotransb - wa_edicao-in_pesosaida ).
  ENDIF.

  wa_saida_aux-obs         = wa_edicao-in_observacao.

  wa_saida_aux-docnum      = wa_edicao-in_docnum.
  wa_saida_aux-pontotransb = wa_edicao-in_pontotransb.
  wa_saida_aux-ck_estornar_trans = wa_edicao-in_estornar_trans.

  IF ( wa_saida_aux-pontotransb NE space ).

    CLEAR: dia, mes, ano.

    dia = wa_edicao-in_datatransb+6(2).
    mes = wa_edicao-in_datatransb+4(2).
    ano = wa_edicao-in_datatransb(4).
    CONCATENATE ano mes dia  INTO wa_saida_aux-datatransb_aux_b.

    CONCATENATE dia '/' mes '/' ano INTO wa_saida_aux-datatransb.

    MODIFY it_saida INDEX index_aux FROM wa_saida_aux
    TRANSPORTING datatransb pesotransb difer obs ck_estornar_trans.

    SELECT SINGLE * INTO @DATA(wa_zlest0039)
      FROM zlest0039
     WHERE docnum EQ @wa_saida_aux-docnum.

    IF sy-subrc IS INITIAL.
      DATA(lw_zlest0039_ajuste) = wa_zlest0039.
      lw_zlest0039_ajuste-datatransb = wa_saida_aux-datatransb_aux_b.
      lw_zlest0039_ajuste-pesotransb = wa_saida_aux-pesotransb.
      PERFORM verificar_carguero IN PROGRAM zlesi0005 USING wa_zlest0039 lw_zlest0039_ajuste IF FOUND.
      CLEAR: lw_zlest0039_ajuste.
    ENDIF.


***Ajuste USER STORY 93771  / Anderson Oenning / 14-10-2022
    IF wa_saida_aux-pesotransb IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum         = wa_saida_aux-docnum
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

        UPDATE zlest0039
          SET datatransb        = wa_saida_aux-datatransb_aux_b
              pesotransb        = wa_saida_aux-pesotransb
              unidadetransb     = wa_zlest0039-unidadesaida
              status            = 'L1'
              transb_efetivo    = vl_kunnr
              observacao        = wa_saida_aux-obs
              ck_estornar_trans = wa_saida_aux-ck_estornar_trans
              dt_atualizacao    = sy-datum
              us_atualizacao    = sy-uname
              tp_importacao_l1  = 'M'
              tp_transgenia     = wa_saida_aux-tp_transgenia
        WHERE docnum EQ wa_saida_aux-docnum.

        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = wa_saida_aux-docnum.

      ENDIF.
    ELSE.

      IF wa_saida_aux-obs IS NOT INITIAL.
        UPDATE zlest0039
          SET observacao        = wa_saida_aux-obs
          ck_estornar_trans     = wa_saida_aux-ck_estornar_trans
        WHERE docnum EQ wa_saida_aux-docnum.
      ENDIF.
    ENDIF.
***Ajuste USER STORY 93771  / Anderson Oenning / 14-10-2022

  ELSE.

    CLEAR: dia, mes, ano.

    dia = wa_edicao-in_datatransb+6(2).
    mes = wa_edicao-in_datatransb+4(2).
    ano = wa_edicao-in_datatransb(4).
    CONCATENATE dia '/' mes '/' ano INTO wa_saida_aux-datatransb.
    MODIFY it_saida INDEX index_aux FROM wa_saida_aux
    TRANSPORTING datatransb pesotransb obs ck_estornar_trans.

    wa_saida_aux-datachegada_aux_b = wa_edicao-in_datatransb.
    dia = wa_saida_aux-datachegada+6(2).
    mes = wa_saida_aux-datachegada+4(2).
    ano = wa_saida_aux-datachegada(4).
    CONCATENATE dia '/' mes '/' ano INTO wa_saida_aux-datachegada.

    MODIFY it_saida INDEX index_aux FROM wa_saida_aux
    TRANSPORTING datachegada pesochegada obs ck_estornar_trans.

    SELECT SINGLE * INTO @wa_zlest0039
      FROM zlest0039
     WHERE docnum EQ @wa_saida_aux-docnum.

    IF sy-subrc IS INITIAL.
      lw_zlest0039_ajuste = wa_zlest0039.
      lw_zlest0039_ajuste-datachegada = wa_saida_aux-datachegada_aux_b.
      lw_zlest0039_ajuste-pesochegada = wa_saida_aux-pesochegada.
      PERFORM verificar_carguero IN PROGRAM zlesi0005 USING wa_zlest0039 lw_zlest0039_ajuste IF FOUND.
      CLEAR: lw_zlest0039_ajuste.
    ENDIF.

***Ajuste USER STORY 93771  / Anderson Oenning / 14-10-2022
    IF wa_saida_aux-pesotransb IS NOT INITIAL.

      CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum         = wa_saida_aux-docnum
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

        UPDATE zlest0039
          SET datachegada       = wa_saida_aux-datachegada_aux_b
              pesochegada       = wa_saida_aux-pesochegada
              unidadetransb     = wa_zlest0039-unidadesaida
              status            = 'L1'
              transb_efetivo    = vl_kunnr
              observacao        = wa_saida_aux-obs
              ck_estornar_trans = wa_saida_aux-ck_estornar_trans
              dt_atualizacao    = sy-datum
              us_atualizacao    = sy-uname
              tp_importacao_l1  = 'M'
              tp_transgenia     = wa_saida_aux-tp_transgenia
        WHERE docnum EQ wa_saida_aux-docnum.

        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = wa_saida_aux-docnum.

      ENDIF.
    ELSE.
      IF wa_saida_aux-obs IS NOT INITIAL.
        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_saida_aux-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          UPDATE zlest0039
            SET observacao        = wa_saida_aux-obs
            ck_estornar_trans     = wa_saida_aux-ck_estornar_trans
          WHERE docnum EQ wa_saida_aux-docnum.
          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = wa_saida_aux-docnum.

        ENDIF.
      ENDIF.
    ENDIF.
***Ajuste USER STORY 93771  / Anderson Oenning / 14-10-2022
  ENDIF.

  CLEAR: wa_saida_aux, wa_edicao.

  CALL METHOD wa_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


ENDFORM.                    " ATUALIZAR_TABELA
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS_EDICAO  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_status_edicao OUTPUT.
  SET PF-STATUS 'FF0200'.
  SET TITLEBAR  'TB0200'.
ENDMODULE.                 " Z_STATUS_EDICAO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE z_user_command_edicao INPUT.

  IF sy-dynnr EQ '0200'.
    CASE sy-ucomm.
      WHEN 'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0.
      WHEN 'OK'.

        PERFORM z_atualizar_tabela.

        FREE it_saida.
        PERFORM: f_seleciona_dados     , " Form seleciona dados
                 f_saida               . " Form de saida

        CALL METHOD wa_alv->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        LEAVE TO SCREEN 0.


    ENDCASE.
  ENDIF.
ENDMODULE.                    "z_user_command_edicao INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_INVOCA_DANFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_invoca_danfe .

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  " Verifica Seleção de Linhas
  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-042.
    EXIT.
  ENDIF.

  CLEAR: wa_saida.

  SORT: it_t001w             BY werks,
        it_kna1_transb       BY kunnr,
        it_lfa1_pontoentrega BY lifnr.

  READ TABLE tl_rows INTO sl_rows INDEX 1.


  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
    EXPORTING
      doc_numero     = wa_saida-docnum
    EXCEPTIONS
      nao_localizado = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " Z_INVOCA_DANFE
*&---------------------------------------------------------------------*
*&      Form  Z_REPROCESSAR
*&---------------------------------------------------------------------*
FORM z_reprocessar .

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  " Verifica Seleção de Linhas
  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.


  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-040.
    EXIT.
  ENDIF.

  CLEAR: wa_saida.

  LOOP AT tl_rows INTO sl_rows.

    READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.


    IF ( wa_saida-pesotransb EQ 0 ).
      CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum         = wa_saida-docnum
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        UPDATE zlest0039
          SET status = 'ET'
          WHERE vbeln  EQ wa_saida-vbeln
            AND nfenum EQ wa_saida-nfenum.
        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = wa_saida-docnum.

      ENDIF.
    ENDIF.

    CLEAR: wa_saida,
           sl_rows.
  ENDLOOP.

  CALL METHOD wa_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " Z_REPROCESSAR
*&---------------------------------------------------------------------*
*&      Form  Z_IMPORTAR
*&---------------------------------------------------------------------*
FORM z_importar .

ENDFORM.                    " Z_IMPORTAR

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM mostrar_doc_fiscal  USING p_fiscal TYPE j_1bdocnum.

  DATA: gf_nfobjn LIKE j_1binterf-nfobjn.

  CHECK p_fiscal IS NOT INITIAL.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      doc_number         = p_fiscal
    IMPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      object_not_found   = 1
      scr_ctrl_not_found = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
    EXPORTING
      obj_number       = gf_nfobjn
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

ENDFORM.                    " MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_TVARV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_seleciona_tvarv .

  DATA: w_final LIKE LINE OF rg_final.

  CONSTANTS: c_name TYPE rvari_vnam VALUE 'Z_FIN_ESTORN_ZLES0050'.

  SELECT * INTO TABLE tg_tvarvc
    FROM tvarvc
    WHERE name = c_name.
  IF sy-subrc EQ 0.
    rg_final = VALUE #( FOR l IN tg_tvarvc ( sign = 'I' option = 'EQ' low = l-low ) ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ATRIB_CONF_CCT_PORTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_atrib_conf_cct_portal.

  DATA: tg_zlest0186 TYPE TABLE OF zlest0186  WITH HEADER LINE,
        tg_lfa1      TYPE TABLE OF lfa1       WITH HEADER LINE,
        tg_zsdt0168  TYPE TABLE OF zsdt0168   WITH HEADER LINE,
        t_saida_aux  TYPE TABLE OF ty_saida.


  t_saida_aux[] = it_saida[].
  DELETE t_saida_aux WHERE chave_nfe_pro IS INITIAL.

  IF t_saida_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0186 APPENDING TABLE tg_zlest0186
       FOR ALL ENTRIES IN t_saida_aux
     WHERE chave = t_saida_aux-chave_nfe_pro.
  ENDIF.

  SORT tg_zlest0186 BY chave.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0186 COMPARING chave.

  CHECK tg_zlest0186[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0168 APPENDING TABLE tg_zsdt0168
     FOR ALL ENTRIES IN tg_zlest0186
   WHERE codigo_ra EQ tg_zlest0186-codigo_ra.

  IF tg_zsdt0168[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 APPENDING TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zsdt0168
     WHERE lifnr EQ tg_zsdt0168-lifnr.
  ENDIF.

  SORT tg_lfa1 BY lifnr.
  DELETE ADJACENT DUPLICATES FROM tg_lfa1 COMPARING lifnr.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    READ TABLE tg_zlest0186 WITH KEY chave = <fs_saida>-chave_nfe_pro.

    IF sy-subrc EQ 0.
      <fs_saida>-conf_cct_portal    = icon_okay.
      <fs_saida>-dt_recepcao_portal = tg_zlest0186-dt_recepcao.

      READ TABLE tg_zsdt0168 WITH KEY codigo_ra = tg_zlest0186-codigo_ra.
      IF sy-subrc EQ 0.
        <fs_saida>-term_cct_portal   = tg_zsdt0168-lifnr.

*        READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0168-lifnr.
*        IF sy-subrc EQ 0.
*          <fs_saida>-ds_term_cct_portal = tg_lfa1-name1.
*        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: tg_zsdt0168, tg_zlest0186.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SOMA_QTD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_soma_qtd .

  DATA: sl_retlote TYPE type_retlote,
        sl_sumret  TYPE type_retlote.

  REFRESH t_sumret.

  LOOP AT t_retlote INTO sl_retlote.
    sl_sumret-docnum     = sl_retlote-docnum.
    sl_sumret-quant_vinc = sl_retlote-quant_vinc.
    COLLECT sl_sumret INTO t_sumret.
    CLEAR: sl_retlote,
           sl_sumret .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ATRIB_CONF_CCT_PORTAL_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_atrib_conf_cct_portal_p .
  DATA: it_saida_0100_aux LIKE it_saida.

  it_saida_0100_aux[] = it_saida[].
  DELETE it_saida_0100_aux WHERE chave_nfe_ter IS INITIAL.

  IF it_saida_0100_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0186 APPENDING TABLE tg_zlest0186
       FOR ALL ENTRIES IN it_saida_0100_aux
     WHERE chave = it_saida_0100_aux-chave_nfe_ter.
  ENDIF.


  SORT tg_zlest0186 BY chave.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0186 COMPARING chave.

  CHECK tg_zlest0186[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0168 APPENDING TABLE tg_zsdt0168
     FOR ALL ENTRIES IN tg_zlest0186
   WHERE codigo_ra EQ tg_zlest0186-codigo_ra.

  IF tg_zsdt0168[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 APPENDING TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zsdt0168
     WHERE lifnr EQ tg_zsdt0168-lifnr.
  ENDIF.

  SORT tg_lfa1 BY lifnr.
  DELETE ADJACENT DUPLICATES FROM tg_lfa1 COMPARING lifnr.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_0100>).

    READ TABLE tg_zlest0186 WITH KEY chave = <fs_saida_0100>-chave_nfe_ter.

    IF ( sy-subrc EQ 0 ) AND ( tg_zlest0186-chave IS NOT INITIAL ).
      <fs_saida_0100>-conf_cct_portal    = icon_okay.
      <fs_saida_0100>-dt_recepcao_portal_p = tg_zlest0186-dt_recepcao.

      READ TABLE tg_zsdt0168 WITH KEY codigo_ra = tg_zlest0186-codigo_ra.
      IF sy-subrc EQ 0.
*        <fs_saida_0100>-term_cct_portal   = tg_zsdt0168-lifnr.

        READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0168-lifnr.
        IF sy-subrc EQ 0.
          <fs_saida_0100>-ds_term_cct_portal = tg_lfa1-lifnr.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: tg_zlest0186, tg_zsdt0168.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_AJUSTA_SALDO_CCT_PORTAL_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_ajusta_saldo_cct_portal_prod .
  DATA:
  qtde_total  TYPE zsdt0173-peso_liq_total.

  "Ajustar saldo.
  DATA(it_saida_0100_aux) = it_saida.
  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<ls_saida>).
    IF <ls_saida>-menge IS NOT INITIAL.
      CLEAR: qtde_total.

      IF <ls_saida>-conf_cct_portal_p EQ icon_okay.
        IF <ls_saida>-peso_aferido_recepcao > <ls_saida>-menge.
          qtde_total = <ls_saida>-menge.
        ELSE.
          qtde_total = <ls_saida>-peso_aferido_recepcao.
        ENDIF.
      ELSE.
        qtde_total = <ls_saida>-menge.
      ENDIF.

      LOOP AT it_saida_0100_aux ASSIGNING FIELD-SYMBOL(<ls_saida_aux>) WHERE docnum EQ <ls_saida>-docnum.
        qtde_total = ( qtde_total - <ls_saida_aux>-qtde_vinc_due - <ls_saida_aux>-qtde_baixada ).
      ENDLOOP.
      <ls_saida>-saldo_exportar = qtde_total.
    ELSE.
      <ls_saida>-saldo_exportar = ' '.
    ENDIF.
    CLEAR: qtde_total.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_due .

  CHECK it_j_1bnfdoc[] IS NOT INITIAL.

  SELECT *
    FROM znom_reme_notas INTO TABLE tg_znom_reme_notas
     FOR ALL ENTRIES IN it_j_1bnfdoc
   WHERE docnum    EQ it_j_1bnfdoc-docnum.


  DATA(tg_znom_reme_notas_aux) = tg_znom_reme_notas[].
  DELETE tg_znom_reme_notas_aux WHERE id_due IS INITIAL.

  CHECK tg_znom_reme_notas_aux[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0170 INTO TABLE tg_zsdt0170
     FOR ALL ENTRIES IN tg_znom_reme_notas_aux
   WHERE id_due = tg_znom_reme_notas_aux-id_due
     AND status = '1'.

  SELECT *
    FROM zsdt0170 INTO TABLE tg_zsdt0170_ret
     FOR ALL ENTRIES IN tg_znom_reme_notas_aux
   WHERE id_due_ref  EQ tg_znom_reme_notas_aux-id_due
     AND loekz       EQ abap_false
     AND status      EQ '1'.

  DELETE tg_zsdt0170 WHERE loekz            IS NOT INITIAL OR
                           bloqueio_interno IS NOT INITIAL.

  SORT tg_zsdt0170 BY id_due.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0170 COMPARING id_due.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_CCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_cct .

  CHECK it_j_1bnfdoc[] IS NOT INITIAL.

  SELECT *
    FROM zlest0147 AS a INTO TABLE tg_zlest0147
     FOR ALL ENTRIES IN it_j_1bnfdoc
   WHERE docnum EQ it_j_1bnfdoc-docnum
     AND EXISTS ( SELECT id_recepcao
                    FROM zlest0146 AS b
                   WHERE b~id_recepcao EQ a~id_recepcao
                     AND b~cancel      EQ abap_false ).

  CHECK tg_zlest0147[] IS NOT INITIAL.

  SELECT local_codigo_ra
    FROM zlest0146 INTO TABLE tg_zlest0146
     FOR ALL ENTRIES IN tg_zlest0147
   WHERE id_recepcao EQ tg_zlest0147-id_recepcao
     AND cancel      EQ abap_false.

  CHECK tg_zlest0146[] IS NOT INITIAL.

  SELECT *
     FROM zsdt0168 INTO TABLE tg_zsdt0168
      FOR ALL ENTRIES IN tg_zlest0146
    WHERE codigo_ra = tg_zlest0146-codigo_ra.

  CHECK tg_zsdt0168[] IS NOT INITIAL.

  SELECT *
    FROM lfa1 APPENDING TABLE tg_lfa1
     FOR ALL ENTRIES IN tg_zsdt0168
   WHERE lifnr = tg_zsdt0168-lifnr.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ZNOM_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_znom_transporte .
  CHECK tg_zsdt0170[] IS NOT INITIAL.

  SELECT *
    FROM znom_transporte INTO TABLE tg_znom_transporte
     FOR ALL ENTRIES IN tg_zsdt0170
   WHERE id_nomeacao_tran = tg_zsdt0170-id_nomeacao_tran.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_NFE_EXPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_nfe_exportacao .
  CHECK tg_znom_reme_notas[] IS NOT INITIAL.

* Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT * FROM zdoc_nf_produtor
    INTO TABLE tg_nf_produtor
    FOR ALL ENTRIES IN tg_znom_reme_notas
    WHERE docnum_prod      = tg_znom_reme_notas-docnum
      AND itmnum_prod      = tg_znom_reme_notas-itmnum
      AND id_nomeacao_tran = tg_znom_reme_notas-id_nomeacao_tran
      AND grp_retorno      = tg_znom_reme_notas-grp_retorno.

* Fluxo de documentos de vendas e distribuição
  IF tg_nf_produtor[] IS NOT INITIAL.

* Fluxo de documentos de vendas e distribuição
    SELECT vbelv vbeln posnn FROM vbfa
          INTO TABLE tg_vbfa
          FOR ALL ENTRIES IN tg_nf_produtor
          WHERE vbelv   = tg_nf_produtor-vbeln
            AND vbtyp_n = 'M'.

  ENDIF.

* Partidas individuais da nota fiscal
  IF tg_vbfa[] IS NOT INITIAL.

* Item nota fiscal
    SELECT docnum refkey refitm FROM j_1bnflin
      INTO TABLE tg_lin
      FOR ALL ENTRIES IN tg_vbfa
      WHERE refkey = tg_vbfa-vbeln
        AND refitm = tg_vbfa-posnn.

  ENDIF.

  IF tg_lin[] IS NOT INITIAL.

* cabeçalho nota fiscal
    SELECT docnum cancel FROM j_1bnfdoc
      INTO TABLE tg_nfdoc
      FOR ALL ENTRIES IN tg_lin
      WHERE docnum = tg_lin-docnum
        AND cancel = space.

  ENDIF.

* Electronic Nota Fiscal: Actual Status
  IF tg_lin[] IS NOT INITIAL.

    SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv FROM j_1bnfe_active
      INTO TABLE tg_active
      FOR ALL ENTRIES IN tg_nfdoc
      WHERE docnum = tg_nfdoc-docnum.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_NFE_EXPORTACAO_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_nfe_exportacao_algodao .
  CHECK tg_znom_reme_notas[] IS NOT INITIAL.

* Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT * FROM zdoc_nf_produtor
    INTO TABLE tg_nf_produtor_algodao
    FOR ALL ENTRIES IN tg_znom_reme_notas
    WHERE docnum_prod      = tg_znom_reme_notas-docnum
      AND itmnum_prod      = tg_znom_reme_notas-itmnum.

* Fluxo de documentos de vendas e distribuição
  IF tg_nf_produtor_algodao[] IS NOT INITIAL.

* Fluxo de documentos de vendas e distribuição
    SELECT vbelv vbeln posnn FROM vbfa
          APPENDING TABLE tg_vbfa
          FOR ALL ENTRIES IN tg_nf_produtor_algodao
          WHERE vbelv   = tg_nf_produtor_algodao-vbeln
            AND vbtyp_n = 'M'.

  ENDIF.

* Partidas individuais da nota fiscal
  IF tg_vbfa[] IS NOT INITIAL.

* Item nota fiscal
    SELECT docnum refkey refitm FROM j_1bnflin
      APPENDING TABLE tg_lin
      FOR ALL ENTRIES IN tg_vbfa
      WHERE refkey = tg_vbfa-vbeln
        AND refitm = tg_vbfa-posnn.

  ENDIF.

  IF tg_lin[] IS NOT INITIAL.

* cabeçalho nota fiscal
    SELECT docnum cancel FROM j_1bnfdoc
      APPENDING TABLE tg_nfdoc
      FOR ALL ENTRIES IN tg_lin
      WHERE docnum = tg_lin-docnum
        AND cancel = space.

  ENDIF.

* Electronic Nota Fiscal: Actual Status
  IF tg_lin[] IS NOT INITIAL.

    SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv FROM j_1bnfe_active
      APPENDING TABLE tg_active
      FOR ALL ENTRIES IN tg_nfdoc
      WHERE docnum = tg_nfdoc-docnum.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_QNT_BAIXADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_qnt_baixada .
  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.

  FREE: tg_zsdt0276_tot.

  SELECT *
    FROM zsdt0276 INTO TABLE tg_zsdt0276
     FOR ALL ENTRIES IN tg_j_1bnfdoc
   WHERE docnum  = tg_j_1bnfdoc-docnum
     AND itmnum  = tg_j_1bnfdoc-itmnum
     AND baixar  = 'X'
     AND status IN ('','A').

  LOOP AT tg_zsdt0276.
    MOVE-CORRESPONDING tg_zsdt0276 TO tg_zsdt0276_tot.
    COLLECT tg_zsdt0276_tot.
  ENDLOOP.

  SORT tg_zsdt0276_tot BY docnum itmnum.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_doc .
*  DATA: tl_lin LIKE it_j_1bnflin,
*        wa_doc TYPE j_1bnfdoc,
*        wa_nad TYPE ty_j_1bnfnad.
*
*  REFRESH t_doc.
*
*  CHECK NOT it_j_1bnflin[] IS INITIAL.
*  tl_lin[] = it_j_1bnflin[].
*  SORT tl_lin BY docnum ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.
*
*  IF p_doc_p IS NOT INITIAL AND tl_lin IS NOT INITIAL.
*    DELETE tl_lin WHERE docnum NOT IN p_doc_p.
*  ENDIF.
*
*  SELECT docnum docdat bukrs
*         branch parid  series
*         nfnum  nfenum
*    FROM j_1bnfdoc
*    INTO TABLE t_doc
*     FOR ALL ENTRIES IN tl_lin
*   WHERE docnum EQ tl_lin-docnum
*     AND cancel <> 'X'
*     AND nftype <> 'ZJ'.
*
*
*  SORT: t_doc       BY docnum ASCENDING,
*        t_j_1bnfnad BY docnum ASCENDING.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_RETLOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_retlote .
  REFRESH t_retlote.

  CHECK NOT it_zlest0039[] IS INITIAL.

  SELECT docnum     nfenum     werks
         nf_retorno docnum_ret quant_vinc data_criacao id_export
    FROM zsdt_retlote
    APPENDING TABLE t_retlote
    FOR ALL ENTRIES IN it_zlest0039
  WHERE  docnum EQ it_zlest0039-docnum.

  SORT t_retlote BY docnum ASCENDING.

* Soma Quantidades
  PERFORM z_soma_qtd.

ENDFORM.

*&---------------------------------------------------------------------*
*&  atualizar ZLES0T0039
*&---------------------------------------------------------------------*
FORM z_atualiza_zlest0039.

  UPDATE zlest0039 SET tp_baixa       = '1'
                       dt_atualizacao = sy-datum
                       us_atualizacao = sy-uname
                       observacao     = zlest0039-observacao
                 WHERE docnum         = wa_saida-docnum.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  CLEAR ok_code.

  SET PF-STATUS 'ZLESR0016'.
  SET TITLEBAR 'ZLESR0016'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE ok_code.

    WHEN 'CONFIRMA'.

      PERFORM z_atualiza_zlest0039.

      FREE it_saida.
      PERFORM: f_seleciona_dados     , " Form seleciona dados
               f_saida               . " Form de saida

      CALL METHOD wa_alv->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      LEAVE TO SCREEN 0.

    WHEN 'VOLTA' OR 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

FORM modifica_field_estorno.

  "128321 CS2023000632 Parâmetro de autorização para habilitar campo psa

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  READ TABLE  _param INTO acesso WITH KEY parid = 'ZLES0050_EDITAR' parva = 'X'.

  IF sy-subrc <> 0.

    LOOP AT SCREEN .
      "Desativa
      IF screen-name = 'WA_EDICAO-IN_ESTORNAR_TRANS'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT SCREEN .
      "Ativa
      IF screen-name = 'WA_EDICAO-IN_ESTORNAR_TRANS'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  PERFORM modifica_field_estorno.

ENDMODULE.

FUNCTION-POOL zles_report.                  "MESSAGE-ID ..

TYPES:
  BEGIN OF ty_bsak,
    bukrs    TYPE bsak-bukrs,
    augdt    TYPE bsak-augdt,
    blart    TYPE bsak-blart,
    shkzg    TYPE bsak-shkzg,
    augbl    TYPE bsak-augbl,
    belnr    TYPE bsak-belnr,
    gjahr    TYPE bsak-gjahr,
    dmbtr    TYPE bsak-dmbtr,
    dmbe2    TYPE bsak-dmbe2,
    monat    TYPE bsak-monat,
    xblnr    TYPE bsak-xblnr,
    lifnr    TYPE bsak-lifnr,
    zfbdt    TYPE bsak-zfbdt,
    zbd1t    TYPE bsak-zbd1t,
    sgtxt    TYPE bsak-sgtxt,
    budat    TYPE bsak-budat,
    umsks    TYPE bsak-umsks,
    ebeln    TYPE bsak-ebeln,
    gsber    TYPE bsak-gsber,
    zuonr    TYPE bsak-zuonr,
    nm_lote  TYPE zpfe_lote_item-nm_lote,
    waers    TYPE bsak-waers,
    ebeln_fr TYPE zlest0032-ebeln,
    auggj    TYPE bsak-auggj,
*-CS2022000256 - 24.03.2022 - JT - inicio
    kostl    TYPE bsak-kostl,
    prctr    TYPE bsak-prctr,
    matnr    TYPE bseg-matnr,
    stcd1    TYPE lfa1-stcd1,
*-CS2022000256 - 24.03.2022 - JT - fim
  END OF ty_bsak,

  BEGIN OF ty_bsik,
    bukrs   TYPE bsik-bukrs,
    augdt   TYPE bsik-augdt,
    blart   TYPE bsik-blart,
    shkzg   TYPE bsik-shkzg,
    augbl   TYPE bsik-augbl,
    belnr   TYPE bsik-belnr,
    gjahr   TYPE bsik-gjahr,
    dmbtr   TYPE bsik-dmbtr,
    dmbe2   TYPE bsik-dmbe2,
    monat   TYPE bsik-monat,
    xblnr   TYPE bsik-xblnr,
    lifnr   TYPE bsik-lifnr,
    zfbdt   TYPE bsik-zfbdt,
    zbd1t   TYPE bsik-zbd1t,
    sgtxt   TYPE bsik-sgtxt,
    budat   TYPE bsik-budat,
    umsks   TYPE bsik-umsks,
    ebeln   TYPE bsik-ebeln,
    gsber   TYPE bsik-gsber,
    zuonr   TYPE bsik-zuonr,
    nm_lote TYPE zpfe_lote_item-nm_lote,
    waers   TYPE bsik-waers,
*-CS2022000256 - 24.03.2022 - JT - inicio
    kostl   TYPE bsak-kostl,
    prctr   TYPE bsak-prctr,
    matnr   TYPE bseg-matnr,
    stcd1   TYPE lfa1-stcd1,
*-CS2022000256 - 24.03.2022 - JT - fim
  END OF ty_bsik,

  BEGIN OF ty_zlest0032,
    belnr  TYPE zlest0032-belnr,
    tknum  TYPE zlest0032-tknum,
    add03  TYPE zlest0032-add03,
    fknum  TYPE zlest0032-fknum,
    docnum TYPE zlest0032-docnum,
    refkey TYPE j_1bnflin-refkey,
  END OF ty_zlest0032,

  BEGIN OF ty_j_1bnfdoc,
    docnum    TYPE j_1bnfdoc-docnum,
    nfenum    TYPE j_1bnfdoc-nfenum,
    nfnum     TYPE j_1bnfdoc-nfnum,
    series    TYPE j_1bnfdoc-series,
    nfe       TYPE j_1bnfdoc-nfe,
    nfe_serie TYPE string,
  END OF ty_j_1bnfdoc,

  BEGIN OF ty_bkpf,
    awkey    TYPE bkpf-awkey,
    bktxt    TYPE bkpf-bktxt,
    bukrs    TYPE bkpf-bukrs,
    belnr    TYPE bkpf-belnr,
    gjahr    TYPE bkpf-gjahr,
    blart    TYPE bkpf-blart,
    stblg    TYPE bkpf-stblg,
    tcode    TYPE bkpf-tcode,
    waers    TYPE bkpf-waers,
    budat    TYPE bkpf-budat,
    re_belnr TYPE zlest0034-re_belnr,
    re_gjahr TYPE zlest0034-re_gjahr,
    bldat    TYPE bkpf-bldat,
  END OF ty_bkpf,

  BEGIN OF ty_zglt035,
    bukrs    TYPE zglt035-bukrs,
    tp_lcto  TYPE zglt035-tp_lcto,
    budat    TYPE zglt035-budat,
    doc_lcto TYPE zglt035-doc_lcto,
    obj_key  TYPE zib_contabil_chv-obj_key,
  END OF ty_zglt035,

  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    gjahr TYPE ekbe-gjahr,
    belnr TYPE ekbe-belnr,
  END OF ty_ekbe,

  BEGIN OF ty_saida,
    bukrs            TYPE bsak-bukrs,
    augdt            TYPE bsak-augdt,
    monat            TYPE bsak-monat,
    gjahr            TYPE bsak-gjahr,
    augbl            TYPE bsak-augbl,
    belnr            TYPE bsak-belnr,
    blart            TYPE bsak-blart,
    miro             TYPE bkpf-awkey,
    lifnr            TYPE bsak-lifnr,
    doc_fiscal       TYPE bsak-xblnr,
    docnum           TYPE j_1bnflin-docnum,
    tknum            TYPE zpfe_lote_item-tknum,
    fknum            TYPE vfkp-fknum,
    modal            TYPE vttk-vsart,
    add03            TYPE zlest0032-add03,
    matnr            TYPE lips-matnr,
    maktx            TYPE makt-maktx,
    matkl            TYPE mara-matkl,
    wgbez            TYPE t023t-wgbez,
    peso             TYPE zpfe_lote_item-peso_chegada,
    zpeso_origem     TYPE zlest0034-zpeso_origem,
    gewei            TYPE lips-gewei,
    vl_pago_lote     TYPE zpfe_lote_item-vl_pago_lote,
    dmbe2            TYPE bsak-dmbe2,
    xblnr            TYPE bsak-xblnr,
    chvid            TYPE zpfe_lote_item-chvid,
    bktxt            TYPE bkpf-bktxt,
    zfbdt            TYPE bsak-zfbdt,
    ebeln            TYPE zfit0045-ebeln,
    name1            TYPE lfa1-name1,
    budat            TYPE bsak-budat,
    dmbtr            TYPE bsak-dmbtr,
    sgtxt            TYPE bsak-sgtxt,
    belnr_fat        TYPE bsak-belnr,
    augbl_fat        TYPE bsak-augbl,
    budat_fat        TYPE bsak-budat,
    dmbtr_fat        TYPE bsak-dmbtr,
    dmbe2_fat        TYPE bsak-dmbe2,
    banco_liq        TYPE skat-txt50,
    gsber            TYPE bsak-gsber,
    bsart            TYPE ekko-bsart,
    charg            TYPE lips-charg,
    tx_camb          TYPE zlest0061-tax_dolar,
    exc_est          TYPE char04,
    tipo             TYPE zchar02,
    inc_manual       TYPE zlest0105-inc_manual,
    bezei            TYPE c LENGTH 40,
    vlr_cte          TYPE j_1bnfdoc-nftot,
    vlr_quebra       TYPE zpfe_lote_item-vl_transacao,
    vlr_perda        TYPE zpfe_lote_item-vl_transacao,
    vlr_quebra_perda TYPE zpfe_lote_item-vl_transacao,
    vlr_tarifa_pgto  TYPE konp-kbetr,
    tp_frete         TYPE c,
    ctenum           TYPE j_1bnfdoc-nfenum,
    ebeln_origem     TYPE zlest0105-ebeln_origem,
    bsart_origem     TYPE zlest0105-bsart_origem,
    ktokk            TYPE lfa1-ktokk,
*-CS2022000256 - 24.03.2022 - JT - inicio
    kostl            TYPE zlest0217-kostl,
    prctr            TYPE zlest0217-prctr,
    matnr_serv       TYPE zlest0217-matnr_serv,
    stcd1            TYPE zlest0218-stcd1,
*-CS2022000256 - 24.03.2022 - JT - fim
  END OF ty_saida.

*-CS2022000256 - 24.03.2022 - JT - inicio
TYPES: BEGIN OF ty_info_port,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         lifnr TYPE lfa1-lifnr,
         ebeln TYPE ekpo-ebeln.
TYPES: END   OF ty_info_port.

TYPES: BEGIN OF ty_bkpf_port,
         bukrs    TYPE bkpf-bukrs,
         belnr    TYPE bkpf-belnr,
         gjahr    TYPE bkpf-gjahr,
         waers    TYPE bkpf-waers,
         awkey    TYPE bkpf-awkey,
         re_belnr TYPE ekbe-belnr,
         re_gjahr TYPE ekbe-gjahr.
TYPES: END   OF ty_bkpf_port.

TYPES: BEGIN OF ty_bseg_port,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         buzei TYPE bseg-buzei,
         kostl TYPE bseg-kostl,
         prctr TYPE bseg-prctr.
TYPES: END   OF ty_bseg_port.

TYPES: BEGIN OF ty_bseg_port_kostl,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         kostl TYPE bseg-kostl.
TYPES: END   OF ty_bseg_port_kostl.

TYPES: BEGIN OF ty_bseg_port_prctr,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         prctr TYPE bseg-prctr.
TYPES: END   OF ty_bseg_port_prctr.

TYPES: BEGIN OF ty_lfa1_port,
         lifnr TYPE lfa1-lifnr,
         stcd1 TYPE lfa1-stcd1.
TYPES: END   OF ty_lfa1_port.

TYPES: BEGIN OF ty_ekpo_port,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         matnr TYPE ekpo-matnr.
TYPES: END   OF ty_ekpo_port.

TYPES: BEGIN OF ty_ekbe_port,
         belnr TYPE ekbe-belnr,
         gjahr TYPE ekbe-gjahr,
         ebeln TYPE ekbe-ebeln.
TYPES: END   OF ty_ekbe_port.
*-CS2022000256 - 24.03.2022 - JT - fim

DATA: BEGIN OF tg_setleaf_eleva OCCURS 0,
        descript TYPE setlinet-descript.
        INCLUDE STRUCTURE setleaf.
      DATA: END OF tg_setleaf_eleva.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: gt_bsak             TYPE TABLE OF ty_bsak,
      gt_bsak_aux02       TYPE TABLE OF ty_bsak WITH HEADER LINE,
      gt_bsik             TYPE TABLE OF ty_bsik,
      gt_bsak_adt_dev     TYPE TABLE OF ty_bsak,
      gt_bsak_aux         TYPE TABLE OF ty_bsak,
      gt_bsak_adt_liq     TYPE TABLE OF ty_bsak,
      gt_bsak_pedagio     TYPE TABLE OF ty_bsak,
      gt_bsak_insumos     TYPE TABLE OF ty_bsak,
      gt_bsis_aux         TYPE TABLE OF bsis WITH HEADER LINE,
      gt_bsis             TYPE TABLE OF bsis,
      gt_bkpf             TYPE TABLE OF ty_bkpf,
      gt_rbkp             TYPE TABLE OF rbkp,
      gt_zpfe_lote_item   TYPE TABLE OF zpfe_lote_item,
      gt_zlest0032        TYPE TABLE OF ty_zlest0032,
      gt_zlest0034        TYPE TABLE OF zlest0034,
      gt_zlest0042        TYPE TABLE OF zlest0042,
      gt_zlest0141        TYPE TABLE OF zlest0141,
      gt_j_1bnfdoc        TYPE TABLE OF j_1bnfdoc,
      gt_j_1bnflin        TYPE TABLE OF j_1bnflin,
      gt_vfkp             TYPE TABLE OF vfkp,
      gt_vttk             TYPE TABLE OF vttk,
      gt_zcte_ciot        TYPE TABLE OF zcte_ciot WITH HEADER LINE,
      gt_zib_cte_dist_n55 TYPE TABLE OF zib_cte_dist_n55 WITH HEADER LINE,
      gt_zib_cte_dist_n01 TYPE TABLE OF zib_cte_dist_n01 WITH HEADER LINE,
      gt_t173             TYPE TABLE OF t173,
      gt_t173t            TYPE TABLE OF t173t,
      gt_vttp             TYPE TABLE OF vttp,
      gt_lips             TYPE TABLE OF lips,
      gt_mara             TYPE TABLE OF mara,
      gt_makt             TYPE TABLE OF makt,
      gt_t023             TYPE TABLE OF t023,
      gt_t023t            TYPE TABLE OF t023t,
      gt_ska1             TYPE TABLE OF ska1,
      gt_skat             TYPE TABLE OF skat,
      gt_ekko             TYPE TABLE OF ekko,
      gt_zglt035          TYPE TABLE OF ty_zglt035,
      gt_zib_contabil_chv TYPE TABLE OF zib_contabil_chv,
      gt_saida            TYPE TABLE OF ty_saida,
      gt_saida_aux        TYPE TABLE OF ty_saida,
      gt_zlest0105        TYPE TABLE OF zlest0105,
      gt_zlest0105_mn     TYPE TABLE OF zlest0105 WITH HEADER LINE,
      gt_zlest0105_aux    TYPE TABLE OF zlest0105,
      gt_tvtkt_grv        TYPE TABLE OF tvtkt WITH HEADER LINE,
      gt_j_1bnfdoc_grv    TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
      gt_vttk_grv         TYPE TABLE OF vttk WITH HEADER LINE,
      gt_vttp_grv         TYPE TABLE OF vttp WITH HEADER LINE,
      gt_bkpf_grv         TYPE TABLE OF ty_bkpf WITH HEADER LINE,
      gt_lfa1_ktokk       TYPE TABLE OF lfa1 WITH HEADER LINE,
*-CS2022000256 - 24.03.2022 - JT - inicio
      t_info_port         TYPE TABLE OF ty_info_port,
      t_info_port_aux     TYPE TABLE OF ty_info_port,
      t_bkpf_port         TYPE TABLE OF ty_bkpf_port,
*
      t_bseg_port_kostl   TYPE SORTED TABLE OF ty_bseg_port_kostl
                               WITH UNIQUE KEY bukrs belnr gjahr kostl,
      t_bseg_port_prctr   TYPE SORTED TABLE OF ty_bseg_port_prctr
                               WITH UNIQUE KEY bukrs belnr gjahr prctr,
      t_bseg_port         TYPE SORTED TABLE OF ty_bseg_port
                               WITH UNIQUE KEY bukrs belnr gjahr buzei,
      t_bseg_port_aux     TYPE TABLE OF ty_bseg_port,
*
      t_lfa1_port         TYPE TABLE OF ty_lfa1_port,
      t_ekpo_port         TYPE SORTED TABLE OF ty_ekpo_port
                               WITH UNIQUE KEY ebeln ebelp,
      t_ekpo_port1        TYPE TABLE OF ty_ekpo_port,
      t_ekbe_port         TYPE TABLE OF ty_ekbe_port,
      w_info_port         TYPE ty_info_port,
      w_bkpf_port         TYPE ty_bkpf_port,
*
      w_bseg_port         TYPE ty_bseg_port,
      w_bseg_port_aux     TYPE ty_bseg_port,
      w_bseg_port_kostl   TYPE ty_bseg_port_kostl,
      w_bseg_port_prctr   TYPE ty_bseg_port_prctr,
*
      w_lfa1_port         TYPE ty_lfa1_port,
      w_ekpo_port         TYPE ty_ekpo_port,
      w_ekbe_port         TYPE ty_ekbe_port.
*-CS2022000256 - 24.03.2022 - JT - fim

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: gw_bsak             TYPE ty_bsak,
      gw_bsik             TYPE ty_bsik,
      gw_bsak_adt_dev     TYPE ty_bsak,
      gw_bsak_aux         TYPE ty_bsak,
      gw_bsak_adt_liq     TYPE ty_bsak,
      gw_bsak_pedagio     TYPE ty_bsak,
      gw_bsak_insumos     TYPE ty_bsak,
      gw_bsis             TYPE bsis,
      gw_bkpf             TYPE ty_bkpf,
      gw_bkpf_aux         TYPE bkpf,
      gw_rbkp             TYPE rbkp,
      gw_zpfe_lote_item   TYPE zpfe_lote_item,
      gw_zlest0032        TYPE ty_zlest0032,
      gw_zlest0034        TYPE zlest0034,
      gw_zlest0042        TYPE zlest0042,
      gw_zlest0141        TYPE zlest0141,
      gw_j_1bnfdoc        TYPE j_1bnfdoc,
      gw_j_1bnflin        TYPE j_1bnflin,
      gw_vttk             TYPE vttk,
      gw_vfkp             TYPE vfkp,
      gw_t173             TYPE t173,
      gw_t173t            TYPE t173t,
      gw_vttp             TYPE vttp,
      gw_lips             TYPE lips,
      gw_mara             TYPE mara,
      gw_makt             TYPE makt,
      gw_t023             TYPE t023,
      gw_t023t            TYPE t023t,
      gw_ska1             TYPE ska1,
      gw_skat             TYPE skat,
      gw_ekko             TYPE ekko,
      gw_zglt035          TYPE ty_zglt035,
      gw_zib_contabil_chv TYPE zib_contabil_chv,
      gw_zlest0105        TYPE zlest0105,
      gw_zlest0105_aux    TYPE zlest0105,
      gw_lfa1             TYPE lfa1,
      gw_saida            TYPE ty_saida,
      gw_saida_aux        TYPE ty_saida.

*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: var_belnr_gjahr TYPE c LENGTH 14.
DATA: var_linhas      TYPE sy-tabix.
DATA: var_tipo_reg(10) TYPE c.
DATA: var_doc_estorno TYPE c.

*-----------------------------------
* ESTRUTURA
*-----------------------------------
DATA: gw_empresa  TYPE t001,
      gw_data     TYPE bsad,
      gw_gjahr    TYPE gjahr,
*
*-CS2022000256 - 24.03.2022 - JT - inicio
      t_zlest0217 TYPE TABLE OF zlest0217,
      t_zlest0218 TYPE TABLE OF zlest0218,
      w_zlest0217 TYPE zlest0217,
      w_zlest0218 TYPE zlest0218.

RANGES: r_0217_prctr  FOR zlest0217-prctr,
        r_0217_kostl  FOR zlest0217-kostl.
*-CS2022000256 - 24.03.2022 - JT - fim

*-----------------------------------
* RANGES
*-----------------------------------
RANGES it_bukrs  FOR t001-bukrs.
RANGES it_augdt  FOR bsad-augdt.
RANGES it_gjahr  FOR bsak-gjahr.

RANGES: r_auggj  FOR bsak-auggj,
        r_bsart  FOR ekko-bsart,
        r_blart  FOR bsak-blart,
        r_blart1 FOR bsak-blart,
        r_waers  FOR bsak-waers,
        r_lifnr  FOR bsak-lifnr,
        r_lifnr1 FOR bsak-lifnr,
        r_lifnr2 FOR bsak-lifnr.

*------------------------------------
* CONSTANTE PARA GAMBETA
*------------------------------------
DATA: cs_tax_fix   TYPE zlest0061-tax_dolar VALUE '3.7870'.

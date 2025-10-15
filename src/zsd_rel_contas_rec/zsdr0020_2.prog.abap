************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 24.09.2102                                          *
* Objetivo    ...: Relatório de Contas a Receber - INSUMOS             *
* Transação   ...: ZSDR0020                                            *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 24.09.2012   Antonio Luiz   Criação       16:17:00      DEVK924159   *
* 22.07.2015   Welgem Barbosa Modificação                              *
* 16.09.2024   Nilton Segantin Melhoria     13:00:00      DEVK9A26WH   *
************************************************************************


REPORT  zsdr0020_2.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: vbak,
        t001,
        kna1,
        zfit0026,
        zsdt0090, vbrk, bsad.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:


  " Documento de vendas: dados de cabeçalho
  BEGIN OF ty_vbak,
    vbeln     TYPE vbak-vbeln,
    erdat     TYPE vbak-erdat,
    auart     TYPE vbak-auart,
    vkbur     TYPE vbak-vkbur,
    kunnr     TYPE vbak-kunnr,
    waerk     TYPE vbak-waerk,
    vkorg     TYPE vbak-vkorg,
    vkgrp(30) TYPE c,
  END OF ty_vbak,

* Documento Principal
  BEGIN OF ty_0041,
    vbeln         TYPE vbeln,
    doc_simulacao TYPE zsded003,
    " 28.07.2023 - RAMON 98680 -->
*    vbelv_agp     TYPE vbeln,
    " 28.07.2023 - RAMON 98680 --<
  END OF ty_0041,

  BEGIN OF ty_0090,
    vbeln         TYPE vbeln_nach,
    vbelv         TYPE vbeln_von,
    doc_simulacao TYPE zsded003,
  END OF ty_0090,

  BEGIN OF ty_vbel,
    vbeln TYPE vbeln_nach,
    vbelv TYPE vbelv,
  END OF ty_vbel,

* Documento de vendas: dados comerciais
  BEGIN OF ty_vbkd,
    vbeln TYPE vbkd-vbeln,
    zterm TYPE vbkd-zterm,
    kurrf TYPE vbkd-kurrf,
    valdt TYPE vbkd-valdt,
  END OF ty_vbkd,

* Documento de vendas: dados de item
  BEGIN OF ty_vbap,
    vbeln  TYPE vbap-vbeln,
    posnr  TYPE vbap-posnr,
    kwmeng TYPE vbap-kwmeng,
    netwr  TYPE vbap-netwr,
    werks  TYPE vbap-werks,
    mwsbp  TYPE vbap-mwsbp,
    zmeng  TYPE vbap-zmeng,
  END OF ty_vbap,


* Mestre de clientes (parte geral)
  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

* Mestre de clientes (parte geral)Contab.financ.: índice secundário p/clientes (partida liq.)
  BEGIN OF ty_bsad,
    bukrs TYPE bsad-bukrs,
    kunnr TYPE bsad-kunnr,
    belnr TYPE bsad-belnr,
    augbl TYPE bsad-augbl,
    budat TYPE bsad-budat,
    augdt TYPE bsad-augdt,
    dmbe2 TYPE bsad-dmbe2,
    dmbtr TYPE bsad-dmbtr,
  END OF ty_bsad,

* Explicações próprias relativas às condições de pagamento
  BEGIN OF ty_t052u,
    spras TYPE t052u-spras,
    zterm TYPE t052u-zterm,
    text1 TYPE t052u-text1,
  END OF ty_t052u,

* Tabela de Controle de Lançamentos Insumos
  BEGIN OF ty_zfit0026,
    zid_lanc        TYPE zfit0026-zid_lanc,
    data_venc       TYPE zfit0026-data_venc,
    vbeln           TYPE zfit0026-vbeln,
    docnum          TYPE zfit0026-docnum,
    moeda           TYPE zfit0026-moeda,
    mont_moeda      TYPE zfit0026-mont_moeda,
    mont_mi         TYPE zfit0026-mont_mi,
    taxa            TYPE zfit0026-taxa,
    forma_pag       TYPE zfit0026-forma_pag,
    observacao      TYPE zfit0026-observacao,
    obj_key         TYPE zfit0026-obj_key,
    docnum_conv     TYPE char10,
    doc_fatura      TYPE zfit0026-doc_fatura,
    vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
    vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
    vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
    vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
    vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
    vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
    mont_rbdo       TYPE zfit0026-mont_rbdo,
    data_pgto       TYPE zfit0026-data_pgto,
    ajuste          TYPE zfit0026-ajuste,
    num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
  END OF ty_zfit0026,

  BEGIN OF ty_saida,
    bukrs              TYPE t001-bukrs,
    zid_lanc           TYPE zfit0026-zid_lanc,
    werks              TYPE vbap-werks,
    " 09.12.2024 - RAMON - 160480 -->
    "vkgrp              TYPE char40,
    vkgrp              TYPE vkgrp,
    bezei              TYPE bezei20,
    " 09.12.2024 - RAMON - 160480 --<
    vkbur              TYPE vbak-vkbur,
    kunnr              TYPE vbak-kunnr,
    name1              TYPE kna1-name1,
    auart              TYPE vbak-auart,
    zterm              TYPE vbkd-zterm,
    text1              TYPE t052u-text1,
    vbeln_s            TYPE vbak-vbeln,
    vbeln_p            TYPE vbak-vbeln,
    " 28.07.2023 - RAMON 98680 -->
*    vbelv_agp        TYPE vbak-vbeln,
    " 28.07.2023 - RAMON 98680 --<
    vbeln              TYPE vbak-vbeln,
    vbeln_g            TYPE vbak-vbeln,
    erdat              TYPE vbak-erdat,
    waerk              TYPE vbak-waerk,
*    TOTALQ_OV     TYPE N LENGTH 15,
*    TOTVL_OV      TYPE N LENGTH 15,
    totalq_ov          TYPE zfit0026-mont_moeda,
    totvl_ov           TYPE zfit0026-mont_moeda,
    netwr_l            TYPE vbap-netwr,
    mwsbp              TYPE vbap-mwsbp,
    data_venc          TYPE zfit0026-data_venc,
    data_venc_2        TYPE zfit0026-data_venc,
    forma_pag          TYPE zfit0026-forma_pag,
    taxa               TYPE zfit0026-taxa,
    mont_moeda         TYPE zfit0026-mont_moeda,
    mont_mi            TYPE zfit0026-mont_mi,
    docnum             TYPE zfit0026-docnum,
    moeda_forte        TYPE zfit0026-mont_moeda,
    moeda_inter        TYPE zfit0026-mont_mi,
    augbl              TYPE bsad-augbl,
    budat              TYPE bsad-budat,
    dmbe2              TYPE bsad-dmbe2,
    dmbtr              TYPE bsad-dmbtr,
    salus              TYPE bsad-dmbe2,
    salre              TYPE bsad-dmbtr,
    rfmng              TYPE rfmng,
    observacao         TYPE zfit0026-observacao,
    safra              TYPE zsdt0040-safra,
    id_order_ecommerce TYPE zsdt0040-id_order_ecommerce, "SMC #123881 - EQUALIZAÇÃO ECC X HANA
    cultura            TYPE zsdt0038-descricao,
    line_color(4)      TYPE c, "Used to store row color attributes
    color_cell         TYPE lvc_t_scol,  " Cell color,
    pgto_ant           TYPE char30,
    tx_multa           TYPE zsdt0051-tx_multa,
    tx_juros           TYPE zsdt0051-tx_juros,
    fkimg              TYPE vbrp-fkimg,
    ptax               TYPE zfit0026-taxa,
    vlr_multa_calc     TYPE zfit0026-vlr_multa_calc,
    vlr_multa_rbdo     TYPE zfit0026-vlr_multa_calc,
    vlr_juros_calc     TYPE zfit0026-vlr_juros_calc,
    vlr_juros_rbdo     TYPE zfit0026-vlr_juros_rbdo,
    vlr_desc_mult      TYPE zfit0026-vlr_desc_mult,
    vlr_desc_jros      TYPE zfit0026-vlr_desc_jros,
    referencia_nfe     TYPE j_1bnfdoc-nfenum,
    vlr_sald_fin_brl   TYPE zfit0026-mont_rbdo,
    vlr_sald_fin       TYPE zfit0026-mont_rbdo,
    data_pgto          TYPE zfit0026-data_pgto,
    mont_rbdo          TYPE zfit0026-mont_rbdo,
    vlr_sal_fat        TYPE zfit0026-mont_rbdo,
    vlr_referencia     TYPE vbrp-netwr,
    vlr_tot_ref        TYPE vbrp-netwr,
    vlr_total_ov       TYPE zfit0026-mont_moeda,
    sald_referencia    TYPE vbrp-netwr,
    ind_rec_total      TYPE c,
    ind_rec_parc       TYPE c,
    num_comp_adiant    TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
**<<<------"152483 - NMS - INI------>>>
    tprel              TYPE char2,                    "Tipo de Relatório (IN = Insumos e MI = Mercado Interno)
    tplin              TYPE c,                        "Tipo de Linha (H = Header I = Item)
**<<<------"152483 - NMS - FIM------>>>
    status             TYPE char20,
  END OF ty_saida,


  BEGIN OF ty_saida_memoria_calculo,
    desc_vlr_multa                 TYPE  char200,
    desc_vlr_multa_legenda         TYPE  char200,
    desc_vlr_juros_dia_ultil       TYPE  char200,
    desc_vlr_juros_dia_subsequente TYPE  char200,
    desc_dias_atraso               TYPE  char200,
    desc_vlr_juros_dia             TYPE  char200,
    moeda                          TYPE vbak-waerk,
  END OF ty_saida_memoria_calculo,

  BEGIN OF ty_saida_calculadora,
    fator            TYPE p DECIMALS 9,
    data_pagamento   TYPE zfit0026-data_pgto,
    vlr_dolar_final  TYPE ukurs_curr,
    vlr_original_usd TYPE netwr,
    vlr_juros_usd    TYPE netwr,
    vlr_multa_usd    TYPE netwr,
    vlr_total_usd    TYPE netwr,
    vlr_original_brl TYPE netwr,
    vlr_juros_brl    TYPE netwr,
    vlr_multa_brl    TYPE netwr,
    vlr_total_brl    TYPE netwr,
    moeda            TYPE vbak-waerk,
    dia_atraso       TYPE char10,
    tx_multa         TYPE zsdt0051-tx_multa,
    tx_juros         TYPE zsdt0051-tx_juros,
    vlr_saldo_ov     TYPE zfit0026-mont_moeda,
    vlr_total_ov     TYPE zfit0026-mont_moeda,
    tx_multa_prop    TYPE zsdt0051-tx_multa,
    tx_juros_prop    TYPE zsdt0051-tx_juros,

  END OF ty_saida_calculadora,

  BEGIN OF ty_saida_listar_faturas,
    xblnr           TYPE vbrk-xblnr,
    vbeln           TYPE vbfa-vbeln,
    doc_fatura      TYPE  zfit0026-doc_fatura,
    doc_contabil    TYPE  bkpf-belnr,
    nfe             TYPE j_1bnfdoc-nfenum,
    data_vencimento TYPE char10,
    valdt           TYPE vbrk-valdt,
    valor           TYPE vbrk-netwr,
    netwr           TYPE vbrk-netwr,
    mwsbk           TYPE  mwsbp,
  END OF ty_saida_listar_faturas,

  BEGIN OF ty_del,
    vbeln_s     TYPE vbak-vbeln,
    vbeln_p     TYPE vbak-vbeln,
    moeda_forte TYPE zfit0026-mont_moeda,
    moeda_inter TYPE zfit0026-mont_mi,
  END OF ty_del,

  BEGIN OF ty_cabec,
    ov        TYPE vbak-vbeln,
    data_venc TYPE datum,
    moeda     TYPE vbap-waerk,
    saldo     TYPE vbap-netwr,
    ptax      TYPE c LENGTH 15,
    sald_jbrl TYPE vbap-netwr,
  END OF ty_cabec.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_vbfa_auxi,
         vbeln  TYPE vbak-vbeln,
         refkey TYPE j_1bnflin-refkey.
TYPES: END OF ty_vbfa_auxi.

TYPES: BEGIN OF ty_j_1bnfdoc.
         INCLUDE TYPE j_1bnfdoc.
TYPES:   vbeln  TYPE vbak-vbeln,
         refkey TYPE j_1bnflin-refkey.
TYPES: END OF ty_j_1bnfdoc.

TYPES: BEGIN OF ty_vbrk.
         INCLUDE TYPE vbrk.
TYPES:   ind_venc TYPE c.
TYPES: END OF ty_vbrk.

TYPES:
  " Documento de vendas: dados de cabeçalho
  BEGIN OF ty_vbrk_,
    vbeln TYPE bkpf-awkey,
    fkart TYPE vbrk-fkart,
    fktyp TYPE vbrk-fktyp,
    zterm TYPE vbrk-zterm,
    vbelv TYPE vbfa-vbelv,
**    ind_venc TYPE c,
    valdt TYPE valdt,
  END OF ty_vbrk_.

TYPES: BEGIN OF ty_ov_primari,
         nro_sol_ov TYPE zsdt0100-nro_sol_ov,
         vbeln      TYPE zsdt0100-vbeln,
         vbelv      TYPE vbfa-vbelv,
         auart      TYPE zsdt0100-auart,
         status     TYPE zsdt0100-status.
TYPES: END OF ty_ov_primari.

TYPES: BEGIN OF ty_edit_juros,
         vbeln        TYPE vbeln,
         doc_fatura   TYPE vbeln,
         data_venc    TYPE zfit0026-data_venc,
         data_pgto    TYPE  zfit0026-data_pgto,
         dias_atraso  TYPE p,
         fator        TYPE p DECIMALS 9,
         juros        TYPE zsdt0053-vlrtot,
         jros         TYPE zsdt0053-vlrtot,
         multa        TYPE zsdt0053-vlrtot,
         mult         TYPE zsdt0053-vlrtot,
         porc_juros   TYPE p DECIMALS 9,
         porc_multa   TYPE p DECIMALS 9,
         juros_parc   TYPE zsdt0053-vlrtot,
         multa_parc   TYPE zsdt0053-vlrtot,
         tx_jros      TYPE zsdt0051-tx_multa,
         tx_multa     TYPE zsdt0051-tx_multa,
         vlr_total_ov TYPE zfit0026-mont_moeda,
         vlr_rbdo     TYPE zfit0026-mont_moeda,
         vlr_t_ov     TYPE zfit0026-mont_moeda,
         dias_ano     TYPE char4,
         txt_doc      TYPE char100,
         porc         TYPE char3,
         por          TYPE char3.
TYPES:       END OF ty_edit_juros.


TYPES: BEGIN OF ty_cad_ordem,
         vbeln       TYPE vbeln,
         org_vendas  TYPE vkorg,
         valor       TYPE netwr,
         filial      TYPE werks_d,
         solicitante TYPE string,
       END OF ty_cad_ordem.

DATA: r_vbeln TYPE RANGE OF vbeln.
DATA: r_vbeln_aux TYPE RANGE OF vbeln.
DATA: r_vbelv TYPE RANGE OF vbelv.

DATA: obj_alv_0200       TYPE REF TO cl_gui_alv_grid,
      obj_container_0200 TYPE REF TO cl_gui_custom_container.


*DATA: IT_FCAT TYPE LVC_T_FCAT,
*      WA_FCAT TYPE LVC_S_FCAT.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

DATA: it_fca      TYPE lvc_t_fcat,
      wa_fca      TYPE lvc_s_fcat,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      tg_estrat   TYPE TABLE OF zsds019.



*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: t_bdc                   TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_ov_primari            TYPE TABLE OF ty_ov_primari,
      it_vbrk_mi              TYPE TABLE OF ty_vbrk,
      lt_vbrk_mi              TYPE TABLE OF ty_vbrk,
      t_edit                  TYPE TABLE OF ty_edit_juros,
      wa_edit                 TYPE ty_edit_juros,
      w_edit                  TYPE ty_edit_juros,
      t_messtab               TYPE TABLE OF bdcmsgcoll,
      it_j_1bnfdoc            TYPE TABLE OF ty_j_1bnfdoc,
      it_vbak                 TYPE TABLE OF ty_vbak,
**********************************************************************
** 151268 Verificar OV`s devolução - PANF - 28.08.24 - INICIO
      gt_ov_devolucao         TYPE TABLE OF tvarvc,
** 151268 Verificar OV`s devolução - PANF - 28.08.24 - Fim
**********************************************************************
      t_vbak                  TYPE TABLE OF ty_vbak,
      it_zsdt0090             TYPE STANDARD TABLE OF zsdt0090,
      it_zsdt0041             TYPE TABLE OF zsdt0041,
      wa_zsdt0041             TYPE zsdt0041,
      it_zsdt0040             TYPE TABLE OF zsdt0040,
      wa_zsdt0040             TYPE zsdt0040,
      ts_vbrk                 TYPE TABLE OF zpme0053,
      t_vbrk                  TYPE TABLE OF vbrk,
      t_bseg                  TYPE TABLE OF bseg,
      it_vbfa                 TYPE TABLE OF vbfa WITH HEADER LINE,
      it_remessa              TYPE TABLE OF vbfa,
      it_estreme              TYPE TABLE OF vbfa,
      it_vbak_1               TYPE TABLE OF ty_vbak,
      it_vbak_aux             TYPE TABLE OF vbak,
      it_vbel                 TYPE TABLE OF ty_vbel,
      it_vbelx                TYPE TABLE OF ty_vbel,
      it_vbel2                TYPE TABLE OF ty_vbel,
      it_0090                 TYPE TABLE OF ty_0090,
      it_0090x                TYPE TABLE OF ty_0090,
      it_0041                 TYPE TABLE OF ty_0041,
      it_0040                 TYPE TABLE OF zsdt0040 WITH HEADER LINE,
      it_zsdt0038             TYPE TABLE OF zsdt0038,
      it_vbkd                 TYPE TABLE OF ty_vbkd,
      it_vbap                 TYPE TABLE OF ty_vbap WITH HEADER LINE,
      it_kna1                 TYPE TABLE OF ty_kna1,
      it_bsad                 TYPE TABLE OF ty_bsad,
      t_bsad                  TYPE TABLE OF ty_bsad,
      it_zsdt0053             TYPE TABLE OF zsdt0053,
      it_zsdt0051             TYPE TABLE OF zsdt0051,
      it_t052u                TYPE TABLE OF ty_t052u,
      it_zfit0026             TYPE TABLE OF ty_zfit0026 WITH HEADER LINE,
      i_zfit0026              TYPE TABLE OF ty_zfit0026,
      it_z0159                TYPE STANDARD TABLE OF zsdt0159,
      it_zibchv               TYPE STANDARD TABLE OF zib_contabil_chv,
      it_z0054                TYPE STANDARD TABLE OF zsdt0054,
      it_saida                TYPE TABLE OF ty_saida,
      lt_saida                TYPE TABLE OF ty_saida,
      it_delete               TYPE TABLE OF ty_del,
      t_cor                   TYPE TABLE OF lvc_s_scol,
      it_color                TYPE TABLE OF lvc_s_scol,
      it_vbfa_auxi            TYPE TABLE OF ty_vbfa_auxi,
      it_saida_listar_faturas TYPE TABLE OF ty_saida_listar_faturas,
      it_vbfa_auxi_1          TYPE TABLE OF ty_saida_listar_faturas,
      wa_cabec                TYPE ty_cabec,
      go_container            TYPE REF TO cl_gui_custom_container,
      go_textedit             TYPE REF TO cl_gui_textedit,
      gv_justif               TYPE char255.

DATA: ind_rec_total            TYPE c.
DATA: ind_rec_parc             TYPE c.
DATA: ind_doc_fatura           TYPE c.
DATA: tot_saldo                TYPE zfit0026-mont_moeda.
DATA: vlr_saldo_parcial        TYPE zfit0026-mont_moeda.
DATA: vlr_saldo_finc           TYPE zfit0026-mont_moeda.
DATA: tot_saldo_fin            TYPE zfit0026-mont_moeda.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont                  TYPE REF TO cl_gui_custom_container,
  wa_alv                   TYPE REF TO cl_gui_alv_grid,
  wa_layout                TYPE lvc_s_layo,
  wa_vbak                  TYPE ty_vbak,
  wa_vbak_1                TYPE ty_vbak,
  wa_vbel                  TYPE ty_vbel,
  wa_vbelx                 TYPE ty_vbel,
  wa_vbel2                 TYPE ty_vbel,
  wa_0090                  TYPE ty_0090,
  wa_0090x                 TYPE ty_0090,
  wa_0041                  TYPE ty_0041,
  wa_vbkd                  TYPE ty_vbkd,
  wa_vbap                  TYPE ty_vbap,
  wa_kna1                  TYPE ty_kna1,
  wa_bsad                  TYPE ty_bsad,
  wa_t052u                 TYPE ty_t052u,
  wa_zfit0026              TYPE ty_zfit0026,
  wa_saida                 TYPE ty_saida,
*  it_vbrk_mi               TYPE TABLE OF vbrk,
  wa_delete                TYPE ty_del,
  wa_color                 TYPE lvc_s_scol,
  wa_z0159                 TYPE zsdt0159,
  wa_zibchv                TYPE zib_contabil_chv,
  wa_saida_memoria_calculo TYPE ty_saida_memoria_calculo,
  wa_saida_calculadora     TYPE ty_saida_calculadora,
  wa_saida_listar_faturas  TYPE ty_saida_listar_faturas,
  lw_zfit0026              TYPE zfit0026,
  w_bseg                   TYPE bseg.

DATA: vg_periodo_base  TYPE char6,
      vg_periodo_atual TYPE char6.

DATA: lw_zfit0026-fator TYPE p DECIMALS 9.
DATA: ptax TYPE p DECIMALS 5.
DATA: juros_multa TYPE bsad-dmbe2.
DATA: r_docnum TYPE RANGE OF j_1bdocnum.
DATA: git_zfit0026 TYPE TABLE OF zfit0026.

TYPES: ty_saida_tab TYPE TABLE OF ty_saida.
TYPES: ty_agreg_02 TYPE TABLE OF zi_in_agreg_02. " RAMON 10.02.2025
*----------------------------------------------------------------------*
*  SHDB
*----------------------------------------------------------------------*
DATA: it_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF it_bdcdata.
DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat         TYPE TABLE OF ty_estrutura,
  it_fcat_faturas TYPE TABLE OF ty_estrutura,
  s_variant       TYPE disvariant           , " Tabela Estrutura co
  t_top           TYPE slis_t_listheader,
  xs_events       TYPE slis_alv_event,
  events          TYPE slis_t_event,
  gd_layout       TYPE slis_layout_alv,
  t_print         TYPE slis_print_alv,
  v_report        LIKE sy-repid,
  t_sort          TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  it_setleaf      LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  estrutura       TYPE TABLE OF ty_estrutura,
  vl_ukurs        TYPE ukurs_curr, "TAXA
  vg_i            TYPE i.

DEFINE mc_preenche_class.
  vg_i = vg_i + 1.
  CLEAR t_sort.
  t_sort-spos      = vg_i.
  t_sort-fieldname = &1.
  t_sort-group     = &2.
  t_sort-up        = &3.
  t_sort-subtot    = &4.
  APPEND t_sort.
END-OF-DEFINITION.


*******************Adptar alv tree para informações Mercado Interno -> 23/01/2020 - AOENNING.

TYPES: BEGIN OF ty_s_zfit0026.
         INCLUDE TYPE zfit0026. "AS DATA.
TYPES:   nkey   TYPE lvc_nkey,
         fatura TYPE zde_fatura.
TYPES: parent_key TYPE lvc_nkey.
TYPES: END OF ty_s_zfit0026.
TYPES: ty_t_zfit0026 TYPE STANDARD TABLE OF ty_s_zfit0026
                      WITH DEFAULT KEY.

TYPES: BEGIN OF ty_report.
         INCLUDE TYPE zpme0039.
TYPES: END OF ty_report.

TYPES: BEGIN OF ty_no,
         node_key TYPE lvc_nkey,
         fatura   TYPE zde_fatura,
       END OF ty_no.

*TYPES:
*  " Documento de vendas: dados de cabeçalho
*  BEGIN OF TY_VBRK,
*    VBELN TYPE BKPF-AWKEY,
*    FKART TYPE VBRK-FKART,
*    FKTYP TYPE VBRK-FKTYP,
*    IND_VENC TYPE C,
*  END OF TY_VBRK.

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  t_fatura     TYPE TABLE OF zpmt0025,
  t_zpmt0024   TYPE TABLE OF zpmt0024,
  t_zpmt0025   TYPE TABLE OF zpmt0025,
  t_zpmt0032   TYPE TABLE OF zpmt0032,
  t_zpmt0034   TYPE TABLE OF zpmt0034,
  t_obs        TYPE TABLE OF zpme0045,
  t_zfit0026   TYPE ty_t_zfit0026,
  t_zfit0026_2 TYPE ty_t_zfit0026,
  t_fcat       TYPE lvc_t_fcat,
  t_fcat2      TYPE slis_t_fieldcat_alv,
  t_zpmt0024_2 TYPE TABLE OF zpmt0024,
  t_zpmt0026_2 TYPE TABLE OF zpmt0026,
  t_report     TYPE TABLE OF zpme0039,
  gs_variant_c TYPE disvariant,
  variante     LIKE disvariant.

*DATA: W_SAIDA TYPE ZPME0053.
DATA: ti_vbrk TYPE TABLE OF ty_vbrk_.

*----------------------------------------------------------------------*
* Variaveis -----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  v_no_unico TYPE c,
  v_node_key TYPE lvc_nkey,
  v_ucomm    TYPE sy-ucomm,
  v_tabix    TYPE sy-tabix,
  v_erro     TYPE c,
  v_obrig    TYPE c,
  go_docking TYPE REF TO cl_gui_docking_container,
  go_tree    TYPE REF TO cl_gui_alv_tree.

**********************************************************************
* PARAMETROS - ZFIS26
**********************************************************************
DATA: lr_werks  TYPE RANGE OF j_1bbranch-branch,
      lr_bukrs  TYPE RANGE OF j_1bbranch-bukrs,           "Empresa
      lr_vkbur  TYPE RANGE OF vbak-vkbur,           "Escritorio de Venda
      lr_kunnr  TYPE RANGE OF kna1-kunnr,           "Cliente
      lr_vbeln  TYPE RANGE OF vbak-vbeln,           "Nr OV
      lr_erdat  TYPE RANGE OF vbak-erdat,           "data da Criação
      lr_ins    TYPE c,                    "Radio Insumos
      lr_mi     TYPE c,                    "Radio Mercado Interno
      l_rb7     TYPE c,                    "Radio Insumos
      l_rb8     TYPE c,                    "Radio Mercado Interno
**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
      lr_contas TYPE RANGE OF bseg-hkont.                    "Conta Juros
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************

*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:

      handle_node_double_click
        FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key,

      handle_item_double_click
        FOR EVENT item_double_click OF cl_gui_alv_tree
        IMPORTING node_key
                  fieldname.

ENDCLASS.                    "lcl_eventhandler DEFINITION

*---------------------------------------------------------------------
*
* Classes locais (Definição)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
*...User Command
    METHODS handle_command_grid
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.


ENDCLASS. "LCL_GRID_EVENT DEFINITION

*---------------------------------------------------------------------
*
* Classes locais (Implementação)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event IMPLEMENTATION.
  METHOD handle_toolbar.
*...Barra de Ferramentas
*    PERFORM F_TOOLBAR_GRID CHANGING E_OBJECT.
  ENDMETHOD. "handle_toolba

  METHOD handle_command_grid.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
*    PERFORM F_COMMAND USING E_UCOMM.
  ENDMETHOD. "handle_command_grid


ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD handle_node_double_click.

  ENDMETHOD.                    "handle_node_double_click

  METHOD handle_item_double_click.

    CLEAR: v_node_key.
    v_node_key = node_key.
    CONDENSE v_node_key.
*    V_NODE_KEY = V_NODE_KEY - 1.
    CONDENSE v_node_key.

*    DELETE T_ZPMT0029 WHERE FATURA = SPACE.

*    IF T_ZPMT0029[] IS NOT INITIAL.
*      CALL SCREEN '0200'.
*    ENDIF.

*    IF V_UCOMM = 'APROVAR' OR V_UCOMM = 'REPROVAR'.
*
*      CALL METHOD GO_TREE->DELETE_SUBTREE
*        EXPORTING
*          I_NODE_KEY                = NODE_KEY
*          I_UPDATE_PARENTS_EXPANDER = ABAP_TRUE.
*
*      CALL METHOD CL_GUI_CFW=>FLUSH.
*    ENDIF.

  ENDMETHOD.                    "handle_item_double_click

ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION


*
*&*******************************************************************************************

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-038.
  PARAMETERS: rb7 RADIOBUTTON GROUP rad3 DEFAULT 'X' USER-COMMAND gb4,
              rb8 RADIOBUTTON GROUP rad3.
  "RB3 RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: rb1 RADIOBUTTON GROUP rad1 USER-COMMAND gb2,
              rb2 RADIOBUTTON GROUP rad1,
              rb3 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b2.
*
*SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-031.
*PARAMETERS: RB4 RADIOBUTTON GROUP RAD2 USER-COMMAND GB3,
*            RB5 RADIOBUTTON GROUP RAD2,
*            RB6 RADIOBUTTON GROUP RAD2 DEFAULT 'X'.
*SELECTION-SCREEN: END OF BLOCK B3.

DATA rb6 TYPE boolean   VALUE 'X'.
DATA: def_variant TYPE disvariant,
      variant     TYPE disvariant,
      v_save(1)   TYPE c VALUE 'A'.
**<<<------"152483 - NMS - INI------>>>
CONSTANTS: gc_mi TYPE char2 VALUE 'MI', "MI = Mercado Interno
           gc_in TYPE char2 VALUE 'IN'. "IN = Insumos
**<<<------"152483 - NMS - FIM------>>>

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-065.
  PARAMETERS: rb9  RADIOBUTTON GROUP rad2 USER-COMMAND gb3 DEFAULT 'X',
              rb10 RADIOBUTTON GROUP rad2,
              rb11 RADIOBUTTON GROUP rad2.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                  p_bukrs FOR t001-bukrs, " Empresa
*                P_AUART FOR VBAK-AUART,            " Tipo de Ordem Venda
                  p_vkbur FOR vbak-vkbur,            " Escritorio venda
                  p_kunnr FOR kna1-kunnr,            " Cliente
                  p_docsi FOR zsdt0090-doc_simulacao, " Doc Simulação
                  p_vbeln FOR vbak-vbeln,            " ordem de venda
                  p_erdat FOR vbak-erdat,            " Data criação da OV
                  p_venc  FOR zfit0026-data_venc,    " Data Vencimento
                  p_pgto  FOR bsad-augdt,
                  p_moeda FOR vbak-waerk,            " Moeda
                  p_vtweg FOR vbak-vtweg,            " Canal de distribuição
                  p_spart FOR vbak-spart,            " Setor de atividade
                  p_auart FOR vbak-auart.            " Tipo da OV
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-000.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

INITIALIZATION.
*  GS_VARIANT_C-REPORT      = SY-REPID.

  variant-report = sy-repid.
  def_variant-report = sy-repid.

* Verificar se existe uma variante default
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    p_varia = def_variant-variant.
  ENDIF.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
  DATA: vg_repid   LIKE sy-repid,
        vg_variant TYPE disvariant.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
  PERFORM f_f4_variant.

AT SELECTION-SCREEN OUTPUT.
*  IF rb2 EQ abap_true.
*
*    LOOP AT SCREEN.
*      IF screen-name CS 'P_ERDAT'.
*
*        FREE: p_erdat.
*        screen-active = '0'.
*
*        MODIFY SCREEN.
*        CONTINUE.
*
*      ENDIF.
*    ENDLOOP.
*  ENDIF.


******************************Class dados da OV.**********************************************


CLASS dados_merc_interno DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: selec_dados_ov.
    CLASS-METHODS: selec_dados_fatura IMPORTING i_saida TYPE ty_saida.
    CLASS-METHODS: chkec_cond_ov  IMPORTING i_saida TYPE ty_saida
                                  EXPORTING e_zdart TYPE t052-zdart.

    CLASS-METHODS: check_nfe_ov.
    CLASS-METHODS: check_recebimentos IMPORTING i_saida TYPE ty_saida.

ENDCLASS.


CLASS dados_merc_interno IMPLEMENTATION.

  METHOD check_recebimentos.

    DATA: saldo_ov TYPE bsad-dmbtr.
    DATA: nr_ov TYPE zfit0026-vbeln.

    DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
          xz_vlr_fortet   TYPE zfit0026-mont_moeda,
          xz_vlr_interna  TYPE zfit0026-mont_mi,
          xz_vlr_internat TYPE zfit0026-mont_mi,
          xtotalq_ov      TYPE vbap-kwmeng,
          xtotalvl_ov     TYPE vbap-netwr,
          vvalor          TYPE zfit0026-mont_moeda,
          pare            TYPE n LENGTH 3,
          vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

    SORT:  it_vbak          BY vbeln,
           it_vbap          BY vbeln,
           it_vbkd          BY vbeln,
           it_zfit0026      BY vbeln,
           it_kna1          BY kunnr,
           it_bsad          BY bukrs kunnr belnr ,
           it_t052u         BY zterm.

    DATA: saldo_ov_brl TYPE zfit0026-mont_moeda.
    DATA: saldo_ov_dol TYPE zfit0026-mont_moeda.
    DATA: saldo_fatura TYPE zfit0026-mont_moeda.
    FREE: it_color, t_cor.


**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
    IF t_ov_primari IS NOT INITIAL.

*    ***    Verficando se existe OV filho com base no simulador.
      SELECT *
      FROM zsdt0053
      INTO CORRESPONDING FIELDS OF TABLE t_ov_primari
        WHERE nro_sol_ov EQ i_saida-vbeln_s
        AND  status IN ( 'Y', 'W' ).

      SELECT *
      FROM zsdt0100
      APPENDING CORRESPONDING FIELDS OF TABLE  t_ov_primari
        WHERE nro_sol_ov EQ i_saida-vbeln_s
         AND  status IN ( 'Y', 'W' ).

      IF t_ov_primari IS NOT INITIAL.
        SORT t_ov_primari BY vbeln.
        DELETE ADJACENT DUPLICATES FROM t_ov_primari COMPARING vbeln.
      ENDIF.


      SORT it_zfit0026 BY vbeln.

*    WA_SAIDA-VBELN_P = I_SAIDA-VBELN_P.
*    WA_SAIDA-VBELN_S = I_SAIDA-VBELN_S.
*    WA_SAIDA-VBELN_G = I_SAIDA-VBELN_G.
*    WA_SAIDA-SAFRA   = I_SAIDA-SAFRA.   "Safra "Comentado AOENNING

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
      SELECT vbeln, vbelv
      FROM vbfa
      INTO TABLE @DATA(lt_vbfa_hc)
        FOR ALL ENTRIES IN @t_ov_primari
        WHERE vbeln = @t_ov_primari-vbeln
          AND vbtyp_n EQ 'H'
          AND vbtyp_v EQ 'C'.
      IF sy-subrc = 0.
        SORT: lt_vbfa_hc BY vbeln.
      ENDIF.


      SELECT vbeln, vbelv
       FROM vbfa
       INTO TABLE @DATA(lt_vbfa_cc)
         FOR ALL ENTRIES IN @t_ov_primari
         WHERE vbeln = @t_ov_primari-vbeln
           AND vbtyp_n EQ 'C'
           AND vbtyp_v EQ 'C'.
      IF sy-subrc = 0.
        SORT: lt_vbfa_cc BY vbeln.
      ENDIF.

    ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************


    READ TABLE t_ov_primari ASSIGNING FIELD-SYMBOL(<w_ov>) WITH KEY vbeln =  i_saida-vbeln.
    IF sy-subrc EQ 0.
      IF <w_ov>-status EQ 'Y'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
**        SELECT SINGLE *
**        FROM vbfa
**        INTO @DATA(w_vbfa)
**          WHERE vbeln = @<w_ov>-vbeln
**            AND vbtyp_n EQ 'H'
**            AND vbtyp_v EQ 'C'.
        READ TABLE lt_vbfa_hc ASSIGNING FIELD-SYMBOL(<fs_vbfa_hc>)
                              WITH KEY vbeln = <w_ov>-vbeln
                              BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = <fs_vbfa_hc>-vbeln.
          wa_saida-vbeln_p = <fs_vbfa_hc>-vbelv.
        ELSE.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = i_saida-vbeln_g.
          wa_saida-vbeln_p = i_saida-vbeln_p.
        ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

      ELSEIF <w_ov>-status EQ 'W'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
***      CLEAR: w_vbfa.
***      SELECT SINGLE *
***      FROM vbfa
***      INTO w_vbfa
***        WHERE vbeln = <w_ov>-vbeln
***          AND vbtyp_n EQ 'C'
***          AND vbtyp_v EQ 'C'.

        READ TABLE lt_vbfa_hc ASSIGNING FIELD-SYMBOL(<fs_vbfa_cc>)
                        WITH KEY vbeln = <w_ov>-vbeln
                        BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = <fs_vbfa_cc>-vbeln.
          wa_saida-vbeln_p = <fs_vbfa_cc>-vbelv.
        ELSE.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = i_saida-vbeln_g.
          wa_saida-vbeln_p = i_saida-vbeln_p.
        ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

      ENDIF.
    ELSE.
      wa_saida-vbeln_s = i_saida-vbeln_s.
      wa_saida-vbeln_g = i_saida-vbeln_g.
      wa_saida-vbeln_p = i_saida-vbeln_p.
    ENDIF.
*
*    "PAGAMENTO ANTECIPADO
*    SELECT SINGLE *
*      FROM ZSDT0052 INTO @DATA(WA_ZSDT0052)
*      WHERE NRO_SOL_OV = @I_SAIDA-VBELN_P.
*
*    CASE WA_ZSDT0052-PGTO_ANT .
*
*      WHEN 'X' .
*        WA_SAIDA-PGTO_ANT = ' Com Boleto '.
*
*      WHEN 'N' .
*        WA_SAIDA-PGTO_ANT = ' Sem Boleto '.
*
*      WHEN ' ' .
*        WA_SAIDA-PGTO_ANT = ' Não Antecipado '.
*
*    ENDCASE.
*
*    " TAXA MULTA
*
*    SELECT SINGLE *
*      FROM ZSDT0051 INTO @DATA(WA_ZSDT0051)
*      WHERE NRO_SOL_OV = @I_SAIDA-VBELN_P.
*
*    WA_SAIDA-TX_MULTA = WA_ZSDT0051-TX_MULTA.
*    WA_SAIDA-TX_JUROS = WA_ZSDT0051-TX_JUROS.
*
*
** CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<
*    READ TABLE IT_VBKD  INTO WA_VBKD  WITH KEY VBELN = I_SAIDA-VBELN BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      WA_SAIDA-DATA_VENC  = WA_VBKD-VALDT.
*      WA_SAIDA-DATA_VENC_2  = WA_VBKD-VALDT.
*    ENDIF.
*
*    READ TABLE IT_T052U INTO WA_T052U WITH KEY ZTERM = WA_VBKD-ZTERM BINARY SEARCH.
*
**    SELECT SINGLE BEZEI
**  FROM TVGRT
**  INTO @DATA(Z_BEZEI)
**   WHERE SPRAS EQ @SY-LANGU
**     AND VKGRP EQ @I_SAIDA-VKGRP.
*    WA_SAIDA-VKGRP      = I_SAIDA-VKGRP.
*    WA_SAIDA-WERKS      = I_SAIDA-WERKS. "Centro
**      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
*    WA_SAIDA-VKBUR      = I_SAIDA-VKBUR.  "Equip venda
*    WA_SAIDA-KUNNR      = I_SAIDA-KUNNR.  "Emissor da ordem
*    WA_SAIDA-NAME1      = WA_KNA1-NAME1.  "Nome 1
*    WA_SAIDA-AUART      = I_SAIDA-AUART.  "Tipo de ordem
*    WA_SAIDA-ZTERM      = WA_VBKD-ZTERM.  "Chave de condições de pagamento
*    WA_SAIDA-TEXT1      = WA_T052U-TEXT1.  "Explicação própria para as condições de pagamento
*
*    WA_SAIDA-VBELN      = I_SAIDA-VBELN.  "Documento de vendas
*    WA_SAIDA-VBELN_G    = I_SAIDA-VBELN.  "Documento de vendas
*
*    WA_SAIDA-ERDAT      = I_SAIDA-ERDAT.  "Data de criação do registro
    wa_saida-waerk      = i_saida-waerk.  "Moeda do documento SD


    "QTDE FATURADO NFe
    DATA(t_zfit0026_group_by_doc_fatura) = it_zfit0026[].

    SORT t_zfit0026_group_by_doc_fatura BY doc_fatura.
    DELETE ADJACENT DUPLICATES FROM t_zfit0026_group_by_doc_fatura COMPARING doc_fatura.



    LOOP AT t_zfit0026_group_by_doc_fatura INTO  DATA(wa_zfit26_group_by_doc_fatura) WHERE  vbeln = i_saida-vbeln.

      IF wa_zfit26_group_by_doc_fatura-doc_fatura IS NOT INITIAL .
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO

**        SELECT SUM( fkimg )
**        FROM vbrp
**        INTO @DATA(qtde_faturado_nfe)
**       WHERE vbeln = @wa_zfit26_group_by_doc_fatura-doc_fatura.

        SELECT SINGLE qtd_fat
        FROM zi_sd_sum_fat
        INTO @DATA(qtde_faturado_nfe)
        WHERE vbeln = @wa_zfit26_group_by_doc_fatura-doc_fatura.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

      ENDIF.

      ADD qtde_faturado_nfe TO wa_saida-fkimg.
      CLEAR: qtde_faturado_nfe.
    ENDLOOP.
    "END QTDE FATURA.

    DATA: vflag TYPE i,
          vcont TYPE i.
    vflag = 0.
    vcont = 0.

    CLEAR: saldo_ov, nr_ov.

    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = i_saida-vbeln.


*********************************


      "PAGAMENTO ANTECIPADO
      SELECT SINGLE *
        FROM zsdt0052 INTO @DATA(wa_zsdt0052)
        WHERE nro_sol_ov = @i_saida-vbeln_p.

*      CASE WA_ZSDT0052-PGTO_ANT .             "Comentado Aoenning
*
*        WHEN 'X' .
*          WA_SAIDA-PGTO_ANT = ' Com Boleto '.
*
*        WHEN 'N' .
*          WA_SAIDA-PGTO_ANT = ' Sem Boleto '.
*
*        WHEN ' ' .
*          WA_SAIDA-PGTO_ANT = ' Não Antecipado '.
*
*      ENDCASE.

      " TAXA MULTA

      SELECT SINGLE *
        FROM zsdt0051 INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov = @i_saida-vbeln_p.

*      WA_SAIDA-TX_MULTA = WA_ZSDT0051-TX_MULTA. "Comentado Aoenning
*      WA_SAIDA-TX_JUROS = WA_ZSDT0051-TX_JUROS. "Comentado Aoenning


* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<
      READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = i_saida-vbeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida-data_venc    = wa_vbkd-valdt.
        wa_saida-data_venc_2  = wa_vbkd-valdt.
      ENDIF.

      READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm BINARY SEARCH.

*    SELECT SINGLE BEZEI
*  FROM TVGRT
*  INTO @DATA(Z_BEZEI)
*   WHERE SPRAS EQ @SY-LANGU
*     AND VKGRP EQ @I_SAIDA-VKGRP.
*      WA_SAIDA-VKGRP      = I_SAIDA-VKGRP. "Centro            "Comentado Aoenning
*      WA_SAIDA-WERKS      = I_SAIDA-WERKS. "Centro            "Comentado Aoenning
*      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
*      WA_SAIDA-VKBUR      = I_SAIDA-VKBUR.  "Equip venda      "Comentado Aoenning
*      WA_SAIDA-KUNNR      = I_SAIDA-KUNNR.  "Emissor da ordem "Comentado Aoenning
*      WA_SAIDA-NAME1      = WA_KNA1-NAME1.  "Nome 1           "Comentado Aoenning
*      WA_SAIDA-AUART      = I_SAIDA-AUART.  "Tipo de ordem    "Comentado Aoenning
*      WA_SAIDA-ZTERM      = WA_VBKD-ZTERM.  "Chave de condições de pagamento  "Comentado Aoenning
*      WA_SAIDA-TEXT1      = WA_T052U-TEXT1.  "Cond de pagamento "Comentado Aoenning

      wa_saida-vbeln      = i_saida-vbeln.  "Documento de vendas
      wa_saida-vbeln_g    = i_saida-vbeln.  "Documento de vendas

*      WA_SAIDA-ERDAT      = I_SAIDA-ERDAT.  "Data de criação do registro "Comentado Aoenning
      wa_saida-waerk      = i_saida-waerk.  "Moeda do documento SD  "Comentado Aoenning


*********************************

*      READ TABLE IT_ZSDT0053 INTO DATA(W_ZSDT0053) WITH KEY VBELN = WA_ZFIT0026-VBELN.
*      IF SY-SUBRC EQ 0.
**        WA_SAIDA-VBELN_S = W_ZSDT0053-NRO_SOL_OV.
*      ENDIF.

      READ TABLE t_ov_primari ASSIGNING <w_ov> WITH KEY vbeln =  wa_zfit0026-vbeln.
      IF sy-subrc EQ 0.
        IF <w_ov>-status EQ 'Y'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO

***          SELECT SINGLE *
***          FROM vbfa
***          INTO w_vbfa
***            WHERE vbeln = <w_ov>-vbeln
***              AND vbtyp_n EQ 'H'
***              AND vbtyp_v EQ 'C'.

          READ TABLE lt_vbfa_hc ASSIGNING <fs_vbfa_hc>
                                WITH KEY vbeln = <w_ov>-vbeln
                                BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = <fs_vbfa_hc>-vbeln.
            wa_saida-vbeln_p = <fs_vbfa_hc>-vbelv.
          ELSE.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = i_saida-vbeln_g.
            wa_saida-vbeln_p = i_saida-vbeln_p.
          ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************


        ELSEIF <w_ov>-status EQ 'W'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
**          CLEAR: w_vbfa.
**          SELECT SINGLE *
**          FROM vbfa
**          INTO w_vbfa
**            WHERE vbeln = <w_ov>-vbeln
**              AND vbtyp_n EQ 'C'
**              AND vbtyp_v EQ 'C'.

          READ TABLE lt_vbfa_cc ASSIGNING <fs_vbfa_cc>
                                WITH KEY vbeln = <w_ov>-vbeln
                                BINARY SEARCH.

          IF sy-subrc = 0.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = <fs_vbfa_cc>-vbeln.
            wa_saida-vbeln_p = <fs_vbfa_cc>-vbelv.
          ELSE.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = i_saida-vbeln_g.
            wa_saida-vbeln_p = i_saida-vbeln_p.
          ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

        ENDIF.

      ELSE.
        wa_saida-vbeln_s = i_saida-vbeln_s.
        wa_saida-vbeln_g = i_saida-vbeln_g.
        wa_saida-vbeln_p = i_saida-vbeln_p.
      ENDIF.

      READ TABLE it_zsdt0051 INTO DATA(w_zsdt0051) WITH KEY nro_sol_ov = i_saida-vbeln_s.
*      CLEAR: Z_BEZEI.
      IF sy-subrc EQ 0.
        SELECT SINGLE bezei
      FROM tvgrt
      INTO @DATA(z_bezei)
       WHERE spras EQ @sy-langu
         AND vkgrp EQ @w_zsdt0051-vkgrp.

*        WA_SAIDA-VKBUR = W_ZSDT0051-VKBUR.  " Escr.Venda                      "Comentado Aoenning
*        WA_SAIDA-VKGRP = W_ZSDT0051-VKGRP && '-' && Z_BEZEI. " Equipe Vendas  "Comentado Aoenning
      ENDIF.


      wa_saida-vlr_multa_calc = wa_zfit0026-vlr_multa_calc.  "MULTA CALCULADO
      wa_saida-vlr_multa_rbdo = wa_zfit0026-vlr_multa_rbdo.  "MULTA RECEBIDA

      wa_saida-vlr_desc_mult  = wa_zfit0026-vlr_desc_mult.   "Desconto Multa
      wa_saida-vlr_desc_jros  = wa_zfit0026-vlr_desc_jros.   "Desconto Juros

      wa_saida-vlr_juros_calc = wa_zfit0026-vlr_juros_calc.  "JUROS CALCULADO
      wa_saida-vlr_juros_rbdo = wa_zfit0026-vlr_juros_rbdo. "JUROS RECEBIDO

      t_cor =
      VALUE #(
               ( fname = 'VLR_MULTA_CALC'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_MULTA_RBDO'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_JUROS_CALC'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_JUROS_RBDO'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_DESC_MULT'     color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_DESC_JROS'     color-col = '5' color-int = '1' color-inv = '1' ) ).
      APPEND LINES OF t_cor TO it_color.

*        WA_SAIDA-VLR_SALD_FIN = ( WA_ZFIT0026-VLR_JUROS_CALC - WA_ZFIT0026-VLR_JUROS_RBDO - WA_ZFIT0026-VLR_DESC_JROS ) + ( WA_ZFIT0026-VLR_MULTA_CALC - WA_ZFIT0026-VLR_MULTA_RBDO - WA_ZFIT0026-VLR_DESC_MULT ).

      IF i_saida-waerk EQ 'USD'.
        wa_saida-vlr_sald_fin = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ELSE.
        wa_saida-vlr_sald_fin_brl = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ENDIF.

      wa_saida-zid_lanc       = wa_zfit0026-zid_lanc.
      wa_saida-vlr_total_ov   = i_saida-totvl_ov.
      wa_saida-ptax = wa_zfit0026-taxa. "PTAX
      wa_saida-data_pgto = wa_zfit0026-data_pgto.
      wa_saida-mont_rbdo = wa_zfit0026-mont_rbdo.
      wa_saida-data_venc = wa_zfit0026-data_venc.


      MOVE: wa_zfit0026-observacao TO wa_saida-observacao.

      ADD 1 TO vcont.

*****Preencher as informações de referente recebimento caso for uma ajuste, senão preencher os valores do documento.
*      IF wa_zfit0026-ajuste IS NOT INITIAL.
*        wa_saida-budat     = wa_zfit0026-data_pgto.
*
*        IF i_saida-waerk EQ 'USD'.
*          wa_saida-dmbtr     = wa_zfit0026-mont_mi.
*          wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
*        ELSE.
*          wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
*          wa_saida-dmbe2     = wa_zfit0026-mont_mi.
*        ENDIF.

      IF wa_zfit0026-ajuste IS NOT INITIAL.
        wa_saida-budat     = wa_zfit0026-data_pgto.

        IF i_saida-waerk EQ 'USD'.
          IF wa_zfit0026-mont_moeda IS NOT INITIAL.
            wa_saida-dmbtr     = ( wa_zfit0026-mont_moeda * wa_zfit0026-taxa ).
            wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          ELSE.
            wa_saida-dmbtr     = wa_zfit0026-mont_mi..
            wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          ENDIF.
        ELSE.
          IF wa_zfit0026-mont_moeda IS NOT INITIAL.
            wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
            wa_saida-dmbe2     = ( wa_zfit0026-mont_moeda / wa_zfit0026-taxa ).
          ELSE.
            wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
            wa_saida-dmbe2     = wa_zfit0026-mont_mi.
          ENDIF.
        ENDIF.

        t_cor =
          VALUE #(
                   ( fname = 'AUGBL          ' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'BUDAT          ' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBE2          ' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBTR          ' color-col = '5' color-int = '1' color-inv = '1' )
                 ).
        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.
      ELSE.

        READ TABLE it_bsad  INTO wa_bsad  WITH KEY kunnr = i_saida-kunnr
                                                   belnr = wa_zfit0026-docnum.
        IF sy-subrc IS NOT INITIAL.

*          READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.

*          READ TABLE it_bsad  INTO wa_bsad  WITH KEY  kunnr = i_saida-kunnr
*                                                      belnr = wa_z0159-adiant.

          READ TABLE it_zibchv INTO wa_zibchv WITH KEY obj_key = wa_zfit0026-obj_key.

          READ TABLE it_bsad  INTO wa_bsad  WITH KEY  kunnr = i_saida-kunnr
                                                      belnr = wa_zibchv-belnr.
        ENDIF.

        FREE: t_cor.

        IF sy-subrc IS INITIAL.

          wa_saida-augbl     = wa_bsad-augbl.
*        WA_SAIDA-BUDAT     = WA_BSAD-BUDAT.
          wa_saida-budat     = wa_bsad-augdt.
*        WA_SAIDA-DMBE2     = WA_BSAD-DMBE2.
*        WA_SAIDA-DMBTR     = WA_BSAD-DMBTR.
*        WA_SAIDA-DMBTR     = ZFIT0026-MONT_MOEDA.

          IF i_saida-waerk EQ 'BRL'.
***     Calculando o valor recebido liquido.
            wa_saida-dmbtr = wa_bsad-dmbtr - ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
            CLEAR: ptax.
            ptax = ( wa_bsad-dmbtr / wa_bsad-dmbe2 ).
            IF ptax IS NOT INITIAL.
              juros_multa  = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) / ptax.
              wa_saida-dmbe2  = wa_bsad-dmbe2 - juros_multa.
            ENDIF.
            CLEAR: juros_multa.
          ELSE.
*** Calculando o valor recebido liquido.
            wa_saida-dmbe2 = wa_bsad-dmbe2 - ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
            CLEAR: ptax.
            ptax = ( wa_bsad-dmbtr / wa_bsad-dmbe2 ).
            IF ptax IS NOT INITIAL.
              juros_multa = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) * ptax.
              wa_saida-dmbtr     = wa_bsad-dmbtr - juros_multa.
            ENDIF.
            CLEAR: juros_multa.
          ENDIF.

          t_cor =
          VALUE #(
                   ( fname = 'AUGBL' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'BUDAT' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBE2' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBTR' color-col = '5' color-int = '1' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.
        ENDIF.
      ENDIF.

      wa_saida-forma_pag  = wa_zfit0026-forma_pag.
      wa_saida-taxa       = wa_zfit0026-taxa.
      wa_saida-mont_moeda = COND #( WHEN i_saida-waerk EQ 'USD' THEN wa_zfit0026-mont_moeda ELSE 0 ).
      wa_saida-mont_mi = COND #( WHEN i_saida-waerk EQ 'BRL' THEN wa_zfit0026-mont_mi ELSE 0 ).

      wa_saida-docnum     = wa_zfit0026-docnum.

      IF wa_saida-docnum IS INITIAL.
*        wa_saida-docnum = wa_z0159-adiant.
        wa_saida-docnum = wa_zibchv-belnr.
      ENDIF.

      FREE: t_cor.

      t_cor =
      VALUE #(
               ( fname = 'FORMA_PAG'  color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'TAXA'       color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'MONT_MOEDA' color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'MONT_MI'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'DOCNUM'     color-col = '5' color-int = '1' color-inv = '1' )
             ).
      APPEND LINES OF t_cor TO it_color.

      IF vcont NE 0.
        IF nr_ov NE i_saida-vbeln.
*          WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA. "Comentado porque o valor esta repetindo.
          t_cor = VALUE  #(
                          ( fname = 'MOEDA_INTER     ' color-col = '6' color-int = '1' color-inv = '1' )
                          ( fname = 'MOEDA_FORTE     ' color-col = '6' color-int = '1' color-inv = '1' )
                          ( fname = 'SALD_REFERENCIA ' color-col = '6' color-int = '1' color-inv = '1' )
                          ( fname = 'VLR_SALD_FIN    ' color-col = '6' color-int = '1' color-inv = '1' )
                          ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' ) ).
          APPEND LINES OF t_cor TO it_color.
          FREE  t_cor.
        ENDIF.

        t_cor = VALUE  #(
                        ( fname = 'MOEDA_FORTE     ' color-col = '6' color-int = '1' color-inv = '1' )
                        ( fname = 'MOEDA_INTER     ' color-col = '6' color-int = '1' color-inv = '1' )
                        ( fname = 'SALD_REFERENCIA ' color-col = '6' color-int = '1' color-inv = '1' )
                        ( fname = 'VLR_SALD_FIN    ' color-col = '6' color-int = '1' color-inv = '1' )
                        ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' ) ).
        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.
        CLEAR wa_saida-line_color.
      ELSE.

        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.

        t_cor =
        VALUE #(
                 ( fname = 'VLR_SALD_FIN_BRL' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'VLR_SALD_FIN    ' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'SALD_REFERENCIA ' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'MOEDA_FORTE     ' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'MOEDA_INTER     ' color-col = '2' color-int = '0' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.
        CLEAR wa_saida-line_color.
      ENDIF.

      wa_saida-color_cell[] = it_color[].


*      IF RB1 = 'X'.
*        IF WA_SAIDA-AUGBL IS NOT INITIAL.
*          APPEND WA_SAIDA TO IT_SAIDA.
*          VFLAG = 1.
*        ENDIF.
*      ELSEIF RB2 = 'X'.
*        IF ( WA_SAIDA-WAERK = 'USD' AND XZ_VLR_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND XZ_VLR_INTERNA NE 0 ).
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.
*      ELSE.
**<<<------"152483 - NMS - INI------>>>
      wa_saida-tprel = gc_mi.         "MI = Mercado Interno
      wa_saida-tplin = sy-abcde+8(1). "I - Item
**<<<------"152483 - NMS - FIM------>>>
      APPEND wa_saida TO it_saida.
*      ENDIF.
      wa_saida-augbl     = ''.

      CLEAR: wa_saida.
*      CLEAR: WA_SAIDA-DMBE2, WA_SAIDA-DMBTR, WA_SAIDA-BUDAT, WA_SAIDA-FORMA_PAG, WA_SAIDA-TAXA, WA_SAIDA-DOCNUM, WA_SAIDA-AUGBL, WA_SAIDA-COLOR_CELL[].
      CLEAR: wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_color, wa_0090, wa_0090x, wa_0041, wa_z0159, wa_bsad, wa_zibchv.
      REFRESH it_color.

      IF vcont = 0.

*        WA_SAIDA-VBELN_S = I_SAIDA-VBELN_S. "Numero de Solicitação de Ordem de Venda
*
*
*        READ TABLE IT_ZSDT0051 INTO W_ZSDT0051 WITH KEY NRO_SOL_OV = I_SAIDA-VBELN_S.
*        IF SY-SUBRC EQ 0.
*          SELECT SINGLE BEZEI
*        FROM TVGRT
*        INTO Z_BEZEI
*         WHERE SPRAS EQ SY-LANGU
*           AND VKGRP EQ W_ZSDT0051-VKGRP.
*
*          WA_SAIDA-VKBUR = W_ZSDT0051-VKBUR.
*          WA_SAIDA-VKGRP = W_ZSDT0051-VKGRP && '-' && Z_BEZEI. "Equipe de venda.
*        ENDIF.
*
*        WA_SAIDA-AUGBL       = WA_BSAD-AUGBL. "Nº documento de compensação
*        IF RB1 = 'X'.
*          IF VFLAG = 1.
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSEIF RB2 = 'X'.
*          IF ( WA_SAIDA-WAERK = 'USD' AND WA_SAIDA-MOEDA_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND WA_SAIDA-MOEDA_INTER NE 0 ).
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSE.
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.


*        CLEAR: WA_VBAK, WA_VBAP, WA_KNA1, WA_VBKD, WA_ZFIT0026, WA_BSAD, WA_T052U, WA_COLOR, WA_0090, WA_0090X, WA_0041.
*        FREE IT_COLOR.

      ENDIF.
    ENDLOOP.


**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
** Loop sem nenhuma utilidade - Comentando *
*****ATUALIZA VALORES DA IT_SAIDA
****    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
*****      <F_SAIDA>-RFMNG = REDUCE RFMNG( INIT X TYPE RFMNG FOR LS IN IT_REMESSA WHERE ( VBELV EQ <F_SAIDA>-VBELN_G ) NEXT X = X + LS-RFMNG ).
****    ENDLOOP.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

* ATUALIZA saldo da OV.
    saldo_ov_brl = i_saida-totvl_ov.
    saldo_ov_dol = i_saida-totvl_ov.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>) WHERE vbeln EQ i_saida-vbeln.
*      IF saldo_ov_brl IS NOT INITIAL.
      saldo_ov_brl  = ( saldo_ov_brl - <f_saida>-dmbtr ).
*      ENDIF.

*      IF saldo_ov_dol IS NOT INITIAL.
      saldo_ov_dol = ( saldo_ov_dol - <f_saida>-dmbe2 ).
*      ENDIF.
    ENDLOOP.

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<w_saida>) WITH KEY vbeln = i_saida-vbeln.
    IF sy-subrc EQ 0.
      IF <w_saida>-waerk <> 'BRL'.
        <w_saida>-moeda_forte = saldo_ov_dol.
      ELSE.
        <w_saida>-moeda_inter = saldo_ov_brl.
      ENDIF.
    ENDIF.
    CLEAR: saldo_ov_brl, saldo_ov_dol.

*    CHECK RB2 EQ ABAP_TRUE.
*
*    LOOP AT IT_SAIDA INTO WA_SAIDA.
*
*      WA_DELETE =
*      VALUE #(
*               VBELN_S     = WA_SAIDA-VBELN_S
*               VBELN_P     = WA_SAIDA-VBELN_P
*               MOEDA_FORTE = WA_SAIDA-MOEDA_FORTE
*               MOEDA_INTER = WA_SAIDA-MOEDA_INTER
*             ).
*
*      COLLECT WA_DELETE INTO IT_DELETE.
*      CLEAR WA_DELETE.
*
*    ENDLOOP.
*
*    DELETE IT_DELETE WHERE MOEDA_FORTE > 10 OR  MOEDA_INTER > 10.
*
*    LOOP AT IT_DELETE INTO WA_DELETE.
*      DELETE IT_SAIDA WHERE VBELN_S EQ WA_DELETE-VBELN_S AND
*                            VBELN_P EQ WA_DELETE-VBELN_P.
*    ENDLOOP.

    CLEAR: wa_saida.
  ENDMETHOD.


  METHOD chkec_cond_ov.

*   Verifca a condição se OV ou Fatura
    SELECT SINGLE t~zdart
     FROM vbkd AS dk
     INNER JOIN vbak AS ak ON ak~vbeln = dk~vbeln
     INNER JOIN t052 AS t  ON t~zterm  = dk~zterm
     INTO @DATA(v_zdart)
    WHERE dk~vbeln = @i_saida-vbeln.

    e_zdart = v_zdart.
  ENDMETHOD.

  METHOD selec_dados_ov.
    FREE: it_saida.
    CLEAR: wa_saida.

    DATA: saldo_ov TYPE bsad-dmbtr.
    DATA: nr_ov TYPE zfit0026-vbeln.

    DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
          xz_vlr_fortet   TYPE zfit0026-mont_moeda,
          xz_vlr_interna  TYPE zfit0026-mont_mi,
          xz_vlr_internat TYPE zfit0026-mont_mi,
          xtotalq_ov      TYPE vbap-kwmeng,
          xtotalvl_ov     TYPE vbap-netwr,
          vvalor          TYPE zfit0026-mont_moeda,
          pare            TYPE n LENGTH 3,
          vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

    SORT:  it_vbak          BY vbeln,
           it_vbap          BY vbeln,
           it_vbkd          BY vbeln,
           it_zfit0026      BY vbeln,
           it_kna1          BY kunnr,
           it_bsad          BY bukrs kunnr belnr,
           it_t052u         BY zterm.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO

    IF it_vbak IS NOT INITIAL.

      SELECT vbeln, nro_sol_ov, charg
        FROM zsdt0053 INTO TABLE @DATA(lt_zsdt0053)
        FOR ALL ENTRIES IN @it_vbak
        WHERE vbeln = @it_vbak-vbeln.
      IF sy-subrc = 0.
        SORT lt_zsdt0053 BY vbeln.

        SELECT nro_sol_ov , pgto_ant
          FROM zsdt0052
          INTO TABLE @DATA(lt_zsdt0052)
          FOR ALL ENTRIES IN @lt_zsdt0053
          WHERE nro_sol_ov = @lt_zsdt0053-nro_sol_ov.

        IF sy-subrc = 0.
          SORT lt_zsdt0052 BY nro_sol_ov.
        ENDIF.

        SELECT nro_sol_ov, tx_multa, tx_juros
        FROM zsdt0051
        INTO TABLE @DATA(lt_zsdt0051)
        FOR ALL ENTRIES IN @lt_zsdt0053
          WHERE nro_sol_ov = @lt_zsdt0053-nro_sol_ov.
        IF sy-subrc = 0.
          SORT lt_zsdt0051 BY nro_sol_ov.
        ENDIF.

      ENDIF.
    ENDIF.

    DATA lr_vkgrp TYPE RANGE OF tvgrt-vkgrp.

    lr_vkgrp = VALUE #( FOR ls_vkgrp IN it_vbak
                        ( sign = 'I'
                          option = 'EQ'
                          low = ls_vkgrp-vkgrp ) ).

    SELECT vkgrp , bezei
     FROM tvgrt
     INTO TABLE @DATA(lt_bezei)
     WHERE spras = @sy-langu
       AND vkgrp IN @lr_vkgrp.
    IF sy-subrc = 0.
      SORT lt_bezei BY vkgrp.
    ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************


    SORT it_zfit0026 BY vbeln.
    DATA: linha_princ_ov.
    LOOP AT it_vbak INTO wa_vbak.


      xtotalq_ov  = 0.
      xtotalvl_ov = 0.


      CLEAR vbeln_aux.
      pare = 0.
      vbeln_aux = wa_vbak-vbeln.
**<<<------"152483 - NMS - INI------>>>
      wa_saida-tprel = gc_mi. "MI = Mercado Interno
**<<<------"152483 - NMS - FIM------>>>
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
** SELECT DENTRO DE LOOP

***        SELECT *
***          FROM zsdt0053 INTO TABLE @DATA(it_zsdt0053)
***         WHERE vbeln = @vbeln_aux.
***
***        LOOP AT it_zsdt0053 INTO DATA(wa_zsdt0053).
***          wa_saida-vbeln_p =   wa_zsdt0053-vbeln.
***          wa_saida-vbeln_s =   wa_zsdt0053-nro_sol_ov.
***          wa_saida-vbeln_g =   wa_zsdt0053-vbeln.
***          wa_saida-safra   =   wa_zsdt0053-charg.
***        ENDLOOP.

      READ TABLE lt_zsdt0053 ASSIGNING FIELD-SYMBOL(<fs_zsdt0053>)
                             WITH KEY vbeln = wa_vbak-vbeln
                             BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-vbeln_p = <fs_zsdt0053>-vbeln.
        wa_saida-vbeln_s = <fs_zsdt0053>-nro_sol_ov.
        wa_saida-vbeln_g = <fs_zsdt0053>-vbeln.
        wa_saida-safra   = <fs_zsdt0053>-charg.


        "PAGAMENTO ANTECIPADO
****        SELECT SINGLE *
****          FROM zsdt0052 INTO @DATA(wa_zsdt0052)
****          WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.
        READ TABLE lt_zsdt0052 ASSIGNING FIELD-SYMBOL(<fs_zsdt0052>)
                               WITH KEY nro_sol_ov = <fs_zsdt0053>-nro_sol_ov
                               BINARY SEARCH.
        IF sy-subrc = 0.
          CASE <fs_zsdt0052>-pgto_ant .
            WHEN 'X' .
              wa_saida-pgto_ant = ' Com Boleto '.
            WHEN 'N' .
              wa_saida-pgto_ant = ' Sem Boleto '.
            WHEN ' ' .
              wa_saida-pgto_ant = ' Não Antecipado '.
          ENDCASE.
        ENDIF.

        " TAXA MULTA
****        SELECT SINGLE *
****          FROM zsdt0051 INTO @DATA(wa_zsdt0051)
****          WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.
        READ TABLE lt_zsdt0051 ASSIGNING FIELD-SYMBOL(<fs_zsdt0051>)
                               WITH KEY nro_sol_ov = <fs_zsdt0053>-nro_sol_ov
                               BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-tx_multa = <fs_zsdt0051>-tx_multa.
          wa_saida-tx_juros = <fs_zsdt0051>-tx_juros.
        ENDIF.


      ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

      LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.
        IF wa_vbak-auart EQ 'ZFUT'  OR  wa_vbak-auart EQ 'ZTRI'.
          ADD wa_vbap-zmeng TO xtotalq_ov.
          ADD wa_vbap-netwr TO xtotalvl_ov.
          ADD wa_vbap-mwsbp TO xtotalvl_ov.
          ADD wa_vbap-netwr TO wa_saida-netwr_l.
          ADD wa_vbap-mwsbp TO wa_saida-mwsbp.

        ELSE.
          ADD wa_vbap-kwmeng TO xtotalq_ov.
          ADD wa_vbap-netwr TO xtotalvl_ov.
          ADD wa_vbap-mwsbp TO xtotalvl_ov.
          ADD wa_vbap-netwr TO wa_saida-netwr_l.
          ADD wa_vbap-mwsbp TO wa_saida-mwsbp.
        ENDIF.
      ENDLOOP.

*&------------Inicio ajuste CS2024000717 / AOENNING &*
      "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
      READ TABLE gt_ov_devolucao ASSIGNING FIELD-SYMBOL(<fs_ov_devolucao>)
          WITH KEY low = wa_vbak-auart
          BINARY SEARCH.
**      SELECT SINGLE * FROM tvarvc INTO  @DATA(wa_ov_devolucao)
**        WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
**         AND  low  EQ @wa_vbak-auart.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
*      IF wa_vbak-auart EQ 'ZRPF'  OR  wa_vbak-auart EQ 'ZROB'  OR  wa_vbak-auart EQ 'ZREB' . "'ZRPF' 'ZROB' 'ZREB' Considera T SE FOR NE considerar J
      IF sy-subrc EQ 0.
*&------------Inicio ajuste CS2024000717 / AOENNING &*
        xtotalq_ov  = xtotalq_ov  * -1.
        xtotalvl_ov = xtotalvl_ov * -1.

        wa_saida-netwr_l = wa_saida-netwr_l * -1.
        wa_saida-mwsbp = wa_saida-mwsbp * -1.

      ELSEIF  wa_vbak-auart EQ 'ZREM' OR  wa_vbak-auart EQ 'ZRFU'.
        xtotalq_ov  = 0.
        xtotalvl_ov = 0.
      ENDIF.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<

      READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      READ TABLE it_kna1  INTO wa_kna1  WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
*      READ TABLE IT_VBFA_AUXI INTO W_VBFA_AUXI WITH KEY  VBELN = WA_VBRK-VBELN


*      READ TABLE IT_J_1BNFDOC INTO DATA(W_J_1BNFDOC) WITH KEY REFKEY = W_VBFA_AUXI-REFKEY
      wa_saida-data_venc  = wa_vbkd-valdt.
      wa_saida-data_venc_2  = wa_vbkd-valdt.

      READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm BINARY SEARCH.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO

***        SELECT SINGLE bezei
***        FROM tvgrt
***        INTO @DATA(z_bezei)
***        WHERE spras EQ @sy-langu
***          AND vkgrp EQ @wa_vbak-vkgrp.

      READ TABLE lt_bezei ASSIGNING FIELD-SYMBOL(<fs_bezei>)
            WITH KEY vkgrp = wa_vbak-vkgrp
            BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-vkgrp = wa_vbak-vkgrp && '-' && <fs_bezei>-bezei.
      ELSE.
        wa_saida-vkgrp = wa_vbak-vkgrp .
      ENDIF.


** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

      wa_saida-werks      = wa_vbap-werks. "Centro
*      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.

      wa_saida-vkbur      = wa_vbak-vkbur.  "Equip venda
      wa_saida-kunnr      = wa_vbak-kunnr.  "Emissor da ordem
      wa_saida-name1      = wa_kna1-name1.  "Nome 1
      wa_saida-auart      = wa_vbak-auart.  "Tipo de ordem
      wa_saida-zterm      = wa_vbkd-zterm.  "Chave de condições de pagamento
      wa_saida-text1      = wa_t052u-text1.  "Explicação própria para as condições de pagamento

      wa_saida-vbeln      = wa_vbak-vbeln.  "Documento de vendas
      wa_saida-vbeln_g    = wa_vbak-vbeln.  "Documento de vendas

      wa_saida-erdat      = wa_vbak-erdat.  "Data de criação do registro
      wa_saida-waerk      = wa_vbak-waerk.  "Moeda do documento SD

      xz_vlr_forte   = 0.
      xz_vlr_interna = 0.

      LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln.

*        MOVE: WA_ZFIT0026-OBSERVACAO TO WA_SAIDA-OBSERVACAO.
        ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
        ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.

      ENDLOOP.

      "QTDE FATURADO NFe
*      DATA(T_ZFIT0026_GROUP_BY_DOC_FATURA) = IT_ZFIT0026[].
*
*      SORT T_ZFIT0026_GROUP_BY_DOC_FATURA BY DOC_FATURA.
*      DELETE ADJACENT DUPLICATES FROM T_ZFIT0026_GROUP_BY_DOC_FATURA COMPARING DOC_FATURA.
**
**
*      LOOP AT T_ZFIT0026_GROUP_BY_DOC_FATURA INTO  DATA(WA_ZFIT26_GROUP_BY_DOC_FATURA) WHERE  VBELN = WA_VBAK-VBELN.
*
*        IF WA_ZFIT26_GROUP_BY_DOC_FATURA-DOC_FATURA IS NOT INITIAL .
*          SELECT SUM( FKIMG )
*          FROM VBRP
*          INTO @DATA(QTDE_FATURADO_NFE)
*         WHERE VBELN = @WA_ZFIT26_GROUP_BY_DOC_FATURA-DOC_FATURA.
*        ENDIF.
*
*        ADD QTDE_FATURADO_NFE TO WA_SAIDA-FKIMG.
*        CLEAR: QTDE_FATURADO_NFE.
*      ENDLOOP.
      "END QTDE FATURA.

* Salva totais
      xz_vlr_fortet   = xz_vlr_forte.
      xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
      IF wa_vbak-waerk = 'USD'.
        xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
        xz_vlr_interna = 0.
      ELSE.
        xz_vlr_forte = 0.
        xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
      ENDIF.

      vvalor = xtotalq_ov.
      wa_saida-totalq_ov = vvalor.

      vvalor = xtotalvl_ov.
      wa_saida-totvl_ov = vvalor.

      DATA: vflag TYPE i,
            vcont TYPE i.
      vflag = 0.
      vcont = 0.

      CLEAR: saldo_ov, nr_ov.


      CLEAR: wa_saida-forma_pag, wa_saida-taxa, wa_saida-docnum, wa_saida-augbl, wa_saida-color_cell[].
      REFRESH it_color.

      IF vcont = 0.

        READ TABLE it_zsdt0053 INTO DATA(w_zsdt0053) WITH KEY vbeln = wa_vbak-vbeln.
        IF sy-subrc EQ 0.
          wa_saida-vbeln_s = w_zsdt0053-nro_sol_ov. "Numero de Solicitação de Ordem de Venda
        ENDIF.

        READ TABLE it_zsdt0051 INTO DATA(w_zsdt0051) WITH KEY nro_sol_ov = w_zsdt0053-nro_sol_ov.
        IF sy-subrc EQ 0.


          SELECT SINGLE bezei
          FROM tvgrt
          INTO @DATA(z_bezei)
           WHERE spras EQ @sy-langu
             AND vkgrp EQ @w_zsdt0051-vkgrp.

          wa_saida-vkbur = w_zsdt0051-vkbur.
          wa_saida-vkgrp = w_zsdt0051-vkgrp && '-' && z_bezei. "Equipe de venda.
        ENDIF.


        wa_saida-mont_moeda  = 0.
        wa_saida-mont_mi     = 0.
        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.
        wa_saida-augbl       = wa_bsad-augbl. "Nº documento de compensação
*        WA_SAIDA-MOEDA_FORTE = XZ_VLR_FORTE.  "Valor líquido na moeda do documento
*        IF NR_OV NE WA_VBAK-VBELN.
*        WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA. "Valor líquido na moeda do documento
*        ENDIF.

        IF wa_saida-waerk <> 'BRL'.
          wa_saida-moeda_forte = wa_saida-totvl_ov.
        ELSE.
          wa_saida-moeda_inter = wa_saida-totvl_ov.
        ENDIF.

        t_cor =
        VALUE #(
                ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'SALD_REFERENCIA' color-col = '6' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        wa_saida-color_cell[] = it_color[].
        wa_saida-line_color  = 'C310'.


        READ TABLE it_zfit0026 INTO wa_zfit0026 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.

          wa_saida-salus = wa_saida-moeda_forte. "Valor líquido na moeda do documento
*          IF NR_OV NE WA_VBAK-VBELN.
          wa_saida-salre = wa_saida-moeda_inter. "Valor líquido na moeda do documento
          "WA_SAIDA-NUM_COMP_ADIANT = WA_ZFIT0026-NUM_COMP_ADIANT. "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
*          ENDIF.
*          NR_OV = NR_OV.
        ENDIF.

*        IF RB1 = 'X'.
*          IF VFLAG = 1.
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSEIF RB2 = 'X'.
*          IF ( WA_SAIDA-WAERK = 'USD' AND WA_SAIDA-MOEDA_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND WA_SAIDA-MOEDA_INTER NE 0 ).
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSE.
        APPEND wa_saida TO it_saida.
*        ENDIF.
      ENDIF.

      CLEAR: linha_princ_ov, wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_saida, wa_color, wa_0090, wa_0090x, wa_0041.
      FREE it_color.
    ENDLOOP.

    CHECK NOT it_saida IS INITIAL.
***    *    ***    Verficando se existe OV filho com base no simulador.
    SELECT *
    FROM zsdt0053
    INTO CORRESPONDING FIELDS OF TABLE t_ov_primari
      FOR ALL ENTRIES IN it_saida
      WHERE nro_sol_ov EQ it_saida-vbeln_s
            AND  status IN ( 'Y', 'W' ).

    SELECT *
    FROM zsdt0100
    APPENDING CORRESPONDING FIELDS OF TABLE  t_ov_primari
      FOR ALL ENTRIES IN it_saida
      WHERE nro_sol_ov EQ it_saida-vbeln_s
       AND  status IN ( 'Y', 'W' ).

    IF t_ov_primari IS NOT INITIAL.
      SORT t_ov_primari BY vbeln.
      DELETE ADJACENT DUPLICATES FROM t_ov_primari COMPARING vbeln.
    ENDIF.


**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
    IF t_ov_primari IS NOT INITIAL.

      SELECT vbeln, vbelv
       FROM vbfa
       INTO TABLE @DATA(lt_vbfa_hc)
         FOR ALL ENTRIES IN @t_ov_primari
         WHERE vbeln = @t_ov_primari-vbeln
           AND vbtyp_n EQ 'H'
           AND vbtyp_v EQ 'C'.

      IF sy-subrc = 0.
        SORT lt_vbfa_hc BY vbeln.
      ENDIF.


      SELECT vbeln, vbelv
       FROM vbfa
       INTO TABLE @DATA(lt_vbfa_cc)
         FOR ALL ENTRIES IN @t_ov_primari
         WHERE vbeln = @t_ov_primari-vbeln
           AND vbtyp_n EQ 'C'
           AND vbtyp_v EQ 'C'.

      IF sy-subrc = 0.
        SORT lt_vbfa_cc BY vbeln.
      ENDIF.

    ENDIF.

**********************************************************************
** Buscar OV principal e Nr Simulador - PANF - 28.08.24 - INICIO
    SELECT vbeln, vbelv
     FROM vbfa
       INTO TABLE @DATA(lt_vbfa_hc2)
         FOR ALL ENTRIES IN @it_saida
         WHERE vbeln = @it_saida-vbeln
           AND vbtyp_n EQ 'H'
           AND vbtyp_v EQ 'C'.

    SELECT vbeln, vbelv
     FROM vbfa
       APPENDING TABLE @lt_vbfa_hc2
         FOR ALL ENTRIES IN @it_saida
         WHERE vbeln = @it_saida-vbeln
           AND vbtyp_n EQ 'C'
           AND vbtyp_v EQ 'C'.

    SELECT vbeln, vbelv
      FROM vbfa
      APPENDING TABLE @lt_vbfa_hc2
       FOR ALL ENTRIES IN @it_saida
       WHERE vbeln = @it_saida-vbeln
         AND vbtyp_n EQ 'L'
         AND vbtyp_v EQ 'L'.

**********************************************************************
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO
    SELECT vbeln, vbelv
     FROM vbfa
     APPENDING TABLE @lt_vbfa_hc2
      FOR ALL ENTRIES IN @it_saida
      WHERE vbeln = @it_saida-vbeln
        AND vbtyp_n EQ 'L'
        AND vbtyp_v EQ 'C'.
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO
**********************************************************************


    IF lt_vbfa_hc2 IS NOT INITIAL.
      SORT lt_vbfa_hc2 BY vbeln.

      SELECT *
      FROM zsdt0053
      INTO CORRESPONDING FIELDS OF TABLE t_ov_primari
        FOR ALL ENTRIES IN lt_vbfa_hc2
        WHERE vbeln EQ lt_vbfa_hc2-vbelv.

    ENDIF.
** Buscar OV principal e Nr Simulador - PANF - 28.08.24 - Fim
**********************************************************************


** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

*
*ATUALIZA VALORES DA IT_SAIDA
    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
*      <F_SAIDA>-RFMNG = REDUCE RFMNG( INIT X TYPE RFMNG Q TYPE AUART FOR LS IN IT_REMESSA
*      WHERE ( VBELV EQ <F_SAIDA>-VBELN_G AND
*            ( AUART EQ 'ZRPF' OR AUART EQ 'ZROB' OR AUART EQ 'ZREB' )
*      )  NEXT X =  X + LS-RFMNG ) .

*&------------Inicio ajuste cs2024000717 / aoenning &*
      "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO

**      CLEAR: wa_ov_devolucao.
**      SELECT SINGLE * FROM tvarvc INTO wa_ov_devolucao
**        WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
**         AND  low  EQ <f_saida>-auart.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

      LOOP AT it_remessa ASSIGNING FIELD-SYMBOL(<_remessa>) WHERE vbelv EQ <f_saida>-vbeln_g.
*              if <f_saida>-auart eq 'ZRPF' or <f_saida>-auart eq 'ZROB' or <f_saida>-auart eq 'ZREB' and <_remessa>-vbtyp_n eq 'T'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
        READ TABLE gt_ov_devolucao TRANSPORTING NO FIELDS
        WITH KEY low = <f_saida>-auart
        BINARY SEARCH.

**        IF <f_saida>-auart EQ wa_ov_devolucao-low AND <_remessa>-vbtyp_n EQ 'T'.
        IF sy-subrc = 0 AND <_remessa>-vbtyp_n EQ 'T'.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
*&------------Fim ajuste cs2024000717 / aoenning &*
          <f_saida>-rfmng = <f_saida>-rfmng + <_remessa>-rfmng.
        ELSE.
          IF <_remessa>-vbtyp_n EQ 'J'
**********************************************************************
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO
            OR ( <_remessa>-vbtyp_n EQ 'M' AND <_remessa>-vbtyp_v EQ 'C' )
            OR ( <_remessa>-vbtyp_n EQ 'O' AND <_remessa>-vbtyp_v EQ 'H' )
            OR ( <_remessa>-vbtyp_n EQ 'P' AND <_remessa>-vbtyp_v EQ 'L' )
            OR ( <_remessa>-vbtyp_n EQ 'M' AND <_remessa>-vbtyp_v EQ 'L' ).

            <f_saida>-rfmng = <f_saida>-rfmng + <_remessa>-rfmng.
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO
**********************************************************************
** 152099 - busca dos estorno da faturas
*          ELSEIF ( <_remessa>-vbtyp_n EQ 'N' AND <_remessa>-vbtyp_v EQ 'C' )
*              OR ( <_remessa>-vbtyp_n EQ 'N' AND <_remessa>-vbtyp_v EQ 'L' )
*              OR ( <_remessa>-vbtyp_n EQ 'S' AND <_remessa>-vbtyp_v EQ 'H' ).
*
*               <f_saida>-rfmng = <f_saida>-rfmng - <_remessa>-rfmng.
** 152099 - busca dos estorno da faturas
          ENDIF.
        ENDIF.

        READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <f_saida>-vbeln.
        IF sy-subrc EQ 0.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
          READ TABLE gt_ov_devolucao TRANSPORTING NO FIELDS
          WITH KEY low = wa_vbak-auart
          BINARY SEARCH.

          IF sy-subrc = 0.
            <f_saida>-rfmng = <f_saida>-totalq_ov.
          ENDIF.

***          IF wa_vbak-auart EQ wa_ov_devolucao-low.
***            <f_saida>-rfmng = <f_saida>-totalq_ov.
***          ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
        ENDIF.
      ENDLOOP.

***Verifica as ordem principal se existem.
      READ TABLE t_ov_primari ASSIGNING FIELD-SYMBOL(<w_ov>) WITH KEY vbeln = <f_saida>-vbeln.
      IF sy-subrc EQ 0.
        IF <w_ov>-status EQ 'Y'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
          READ TABLE lt_vbfa_hc ASSIGNING FIELD-SYMBOL(<fs_vbfa_hc>)
                                WITH KEY vbeln = <w_ov>-vbeln
                                BINARY SEARCH.
          IF sy-subrc = 0.
            <f_saida>-vbeln_g = <fs_vbfa_hc>-vbeln.
            <f_saida>-vbeln_p = <fs_vbfa_hc>-vbelv.
          ENDIF.

***          SELECT SINGLE *
***          FROM vbfa
***          INTO @DATA(w_vbfa)
***            WHERE vbeln = @<w_ov>-vbeln
***              AND vbtyp_n EQ 'H'
***              AND vbtyp_v EQ 'C'.
***
***          IF w_vbfa IS NOT INITIAL.
****              <F_SAIDA>-VBELN_S = I_SAIDA-VBELN_S.
***            <f_saida>-vbeln_g = w_vbfa-vbeln.
***            <f_saida>-vbeln_p = w_vbfa-vbelv.
***          ELSE.
****              <F_SAIDA>-VBELN_S = I_SAIDA-VBELN_S.
****              <F_SAIDA>-VBELN_G = I_SAIDA-VBELN_G.
****              <F_SAIDA>-VBELN_P = I_SAIDA-VBELN_P.
***          ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

        ELSEIF <w_ov>-status EQ 'W'.
*

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
          READ TABLE lt_vbfa_cc ASSIGNING FIELD-SYMBOL(<fs_vbfa_cc>)
                                WITH KEY vbeln = <w_ov>-vbeln
                                BINARY SEARCH.
          IF sy-subrc = 0.
            <f_saida>-vbeln_g = <fs_vbfa_cc>-vbeln.
            <f_saida>-vbeln_p = <fs_vbfa_cc>-vbelv.
          ENDIF.

***          CLEAR: w_vbfa.
***          SELECT SINGLE *
***          FROM vbfa
***          INTO w_vbfa
***            WHERE vbeln = <w_ov>-vbeln
***              AND vbtyp_n EQ 'C'
***              AND vbtyp_v EQ 'C'.
***
***          IF w_vbfa IS NOT INITIAL.
****              <F_SAIDA>-VBELN_S = I_SAIDA-VBELN_S.
***            <f_saida>-vbeln_g = w_vbfa-vbeln.
***            <f_saida>-vbeln_p = w_vbfa-vbelv.
***          ELSE.
****              <F_SAIDA>-VBELN_S = I_SAIDA-VBELN_S.
****              <F_SAIDA>-VBELN_G = I_SAIDA-VBELN_G.
****              <F_SAIDA>-VBELN_P = I_SAIDA-VBELN_P.
***          ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
        ELSE.
**********************************************************************
** Buscar OV principal e Nr Simulador - PANF - 28.08.24 - INICIO
          READ TABLE lt_vbfa_hc ASSIGNING <fs_vbfa_hc>
                      WITH KEY vbeln = <w_ov>-vbeln
                      BINARY SEARCH.
          IF sy-subrc = 0.
            <f_saida>-vbeln_g = <fs_vbfa_hc>-vbeln.
            <f_saida>-vbeln_p = <fs_vbfa_hc>-vbelv.
          ENDIF.
**********************************************************************
** Buscar OV principal e Nr Simulador - PANF - 28.08.24 - Fim

        ENDIF.
      ELSE.

**********************************************************************
** Buscar OV principal e Nr Simulador - PANF - 28.08.24 - INICIO
** Tenta localizar a OV principal pelo fluxo standard sem precisar das
** tabelas Z. Quando encontrar, verificar se OV principal esta na ZSDT0053

        READ TABLE lt_vbfa_hc2 ASSIGNING FIELD-SYMBOL(<fs_vbfa_hc2>)
                    WITH KEY vbeln = <f_saida>-vbeln
                    BINARY SEARCH.
        IF sy-subrc = 0.

          READ TABLE t_ov_primari ASSIGNING <w_ov> WITH KEY vbeln = <fs_vbfa_hc2>-vbelv.
          IF sy-subrc = 0.
            <f_saida>-vbeln_s = <w_ov>-nro_sol_ov.
            <f_saida>-vbeln_g = <fs_vbfa_hc2>-vbeln.
            <f_saida>-vbeln_p = <fs_vbfa_hc2>-vbelv.
          ENDIF.

        ENDIF.

** Buscar OV principal e Nr Simulador - PANF - 28.08.24 - INICIO
**********************************************************************

*          <F_SAIDA>-VBELN_S = I_SAIDA-VBELN_S.
*          <F_SAIDA>-VBELN_G = I_SAIDA-VBELN_G.
*          <F_SAIDA>-VBELN_P = I_SAIDA-VBELN_P.
      ENDIF.
    ENDLOOP.

*    CHECK RB2 EQ ABAP_TRUE.
*
*    LOOP AT IT_SAIDA INTO WA_SAIDA.
*
*      WA_DELETE =
*      VALUE #(
*               VBELN_S     = WA_SAIDA-VBELN_S
*               VBELN_P     = WA_SAIDA-VBELN_P
*               MOEDA_FORTE = WA_SAIDA-MOEDA_FORTE
*               MOEDA_INTER = WA_SAIDA-MOEDA_INTER
*             ).
*
*      COLLECT WA_DELETE INTO IT_DELETE.
*      CLEAR WA_DELETE.
*
*    ENDLOOP.
*
*    DELETE IT_DELETE WHERE MOEDA_FORTE > 10 OR  MOEDA_INTER > 10.
*
*    LOOP AT IT_DELETE INTO WA_DELETE.
*      DELETE IT_SAIDA WHERE VBELN_S EQ WA_DELETE-VBELN_S AND
*                            VBELN_P EQ WA_DELETE-VBELN_P.
*    ENDLOOP.
  ENDMETHOD.


  METHOD check_nfe_ov.

    IF it_saida IS NOT INITIAL.



    ENDIF.

  ENDMETHOD.

  METHOD selec_dados_fatura.

*    IF V_ZDART = 'B'.

    "carrega dados da faturas quando a fatura é baseada na remessa.
    FREE: it_vbrk_mi.
    SELECT *
    FROM   vbrk  AS rk
    INNER JOIN vbfa AS fa  ON fa~vbeln = rk~vbeln
    INTO CORRESPONDING FIELDS OF TABLE it_vbrk_mi
    WHERE fa~vbelv = i_saida-vbeln
    AND  rk~vbeln IN r_vbeln
    AND  fa~vbtyp_n  = 'M'
    AND  fa~vbtyp_v  = 'C'
    AND   stufe      = '01'.
**********************************************************************
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO

    IF sy-subrc <> 0.

      "Busca fatura da Nota de credito - Devolucao
      SELECT *
        FROM   vbrk  AS rk
        INNER JOIN vbfa AS fa  ON fa~vbeln = rk~vbeln
        INTO CORRESPONDING FIELDS OF TABLE it_vbrk_mi
        WHERE fa~vbelv = i_saida-vbeln
        AND  rk~vbeln IN r_vbeln
        AND  fa~vbtyp_n  = 'O'
        AND  fa~vbtyp_v  = 'H'
        AND   stufe      = '00'.

      IF sy-subrc <> 0.

        "Busca fatura da Nota de debito - Solic Nota de debito
        SELECT *
          FROM   vbrk  AS rk
          INNER JOIN vbfa AS fa  ON fa~vbeln = rk~vbeln
          INTO CORRESPONDING FIELDS OF TABLE it_vbrk_mi
          WHERE fa~vbelv = i_saida-vbeln
          AND  rk~vbeln IN r_vbeln
          AND  fa~vbtyp_n  = 'P'
          AND  fa~vbtyp_v  = 'L'
          AND   stufe      = '00'.

      ENDIF.

    ENDIF.
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO
**********************************************************************

*    IF P_VENC IS NOT INITIAL.
*      LOOP AT  IT_VBRK_MI ASSIGNING FIELD-SYMBOL(<LS_VBRK>).
*        LOOP AT TS_VBRK INTO DATA(W_VBRK) WHERE DOC_FATURA EQ <LS_VBRK>-VBELN AND VBELN EQ I_SAIDA-VBELN.
*          IF SY-SUBRC EQ 0.
*            <LS_VBRK>-IND_VENC = ABAP_TRUE.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.

*      SORT IT_VBRK_MI BY IND_VENC.
*      DELETE IT_VBRK_MI WHERE IND_VENC EQ ABAP_FALSE.
*    ENDIF.

***    Verficando se existe OV filho com base no simulador.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
**    SELECT *
    SELECT vbeln, status
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
    FROM zsdt0053
    INTO CORRESPONDING FIELDS OF TABLE @t_ov_primari
    WHERE nro_sol_ov EQ @i_saida-vbeln_s
    AND  status IN ( 'Y', 'W' ).

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
**    SELECT *
    SELECT vbeln, status
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
    FROM zsdt0100
    APPENDING CORRESPONDING FIELDS OF TABLE @t_ov_primari
      WHERE nro_sol_ov EQ @i_saida-vbeln_s
       AND  status IN ( 'Y', 'W' ).

    IF t_ov_primari IS NOT INITIAL.
      SORT t_ov_primari BY vbeln.
      DELETE ADJACENT DUPLICATES FROM t_ov_primari COMPARING vbeln.
    ENDIF.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
    IF t_ov_primari IS NOT INITIAL.

      SELECT vbeln, vbelv
             FROM vbfa
             INTO TABLE @DATA(lt_vbfa_hc)
             FOR ALL ENTRIES IN @t_ov_primari
               WHERE vbeln = @t_ov_primari-vbeln
                 AND vbtyp_n EQ 'H'
                 AND vbtyp_v EQ 'C'.
      IF sy-subrc = 0.
        SORT lt_vbfa_hc BY vbeln.
      ENDIF.

      SELECT vbeln, vbelv
            FROM vbfa
            INTO TABLE @DATA(lt_vbfa_cc)
            FOR ALL ENTRIES IN @t_ov_primari
              WHERE vbeln = @t_ov_primari-vbeln
                AND vbtyp_n EQ 'C'
                AND vbtyp_v EQ 'C'.
      IF sy-subrc = 0.
        SORT lt_vbfa_cc BY vbeln.
      ENDIF.

    ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

    IF it_vbrk_mi IS INITIAL.

*      * *    **    Buscando OV principal, caso não tenha faturas para preencher a OV principal do cabecalho da OV filho.
      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<w_saida>) WHERE vbeln EQ i_saida-vbeln.
**<<<------"152483 - NMS - INI------>>>
        <w_saida>-tprel = i_saida-tprel.
        <w_saida>-tplin = i_saida-tplin.
**<<<------"152483 - NMS - FIM------>>>
        READ TABLE t_ov_primari ASSIGNING FIELD-SYMBOL(<w_ov>) WITH KEY vbeln =  <w_saida>-vbeln.
        IF sy-subrc EQ 0.
          IF <w_ov>-status EQ 'Y'.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
**            SELECT SINGLE *
**            FROM vbfa
**            INTO @DATA(w_vbfa)
**              WHERE vbeln = @<w_ov>-vbeln
**                AND vbtyp_n EQ 'H'
**                AND vbtyp_v EQ 'C'.

            READ TABLE lt_vbfa_hc ASSIGNING FIELD-SYMBOL(<fs_vbfa_hc>)
                                  WITH KEY vbeln = <w_ov>-vbeln
                                  BINARY SEARCH.

            IF sy-subrc = 0.
              <w_saida>-vbeln_s = i_saida-vbeln_s.
              <w_saida>-vbeln_g = <fs_vbfa_hc>-vbeln.
              <w_saida>-vbeln_p = <fs_vbfa_hc>-vbelv.
            ELSE.
              <w_saida>-vbeln_s = i_saida-vbeln_s.
              <w_saida>-vbeln_g = i_saida-vbeln_g.
              <w_saida>-vbeln_p = i_saida-vbeln_p.
            ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
*
*
          ELSEIF <w_ov>-status EQ 'W'.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
**            CLEAR: w_vbfa.
**            SELECT SINGLE *
**            FROM vbfa
**            INTO w_vbfa
**              WHERE vbeln = <w_ov>-vbeln
**                AND vbtyp_n EQ 'C'
**                AND vbtyp_v EQ 'C'.

            READ TABLE lt_vbfa_hc ASSIGNING FIELD-SYMBOL(<fs_vbfa_cc>)
                                  WITH KEY vbeln = <w_ov>-vbeln
                                  BINARY SEARCH.

            IF sy-subrc = 0.
              <w_saida>-vbeln_s = i_saida-vbeln_s.
              <w_saida>-vbeln_g = <fs_vbfa_cc>-vbeln.
              <w_saida>-vbeln_p = <fs_vbfa_cc>-vbelv.
            ELSE.
              <w_saida>-vbeln_s = i_saida-vbeln_s.
              <w_saida>-vbeln_g = i_saida-vbeln_g.
              <w_saida>-vbeln_p = i_saida-vbeln_p.
            ENDIF.
          ENDIF.

** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

        ELSE.
          <w_saida>-vbeln_s = i_saida-vbeln_s.
          <w_saida>-vbeln_g = i_saida-vbeln_g.
          <w_saida>-vbeln_p = i_saida-vbeln_p.
        ENDIF.
      ENDLOOP.

    ELSE.
****   Faturas da OV.
      PERFORM f_construir_saida_faturas_mi TABLES it_vbrk_mi USING i_saida.

      CLEAR: wa_saida-forma_pag, wa_saida-taxa, wa_saida-docnum, wa_saida-augbl, wa_saida-color_cell[].
      REFRESH it_color.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

********************************************************************
*
*                         Class dados insumo.
********************************************************************

CLASS dados_insumo DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: selec_dados_ov.
    CLASS-METHODS: selec_dados_fatura IMPORTING i_saida TYPE ty_saida.
    CLASS-METHODS: chkec_cond_ov  IMPORTING i_saida TYPE ty_saida
                                  EXPORTING e_zdart TYPE t052-zdart.

    CLASS-METHODS: check_nfe_ov.
    CLASS-METHODS: check_recebimentos IMPORTING i_saida TYPE ty_saida.

ENDCLASS.

CLASS dados_insumo IMPLEMENTATION.

*****  Selecionar dados da OV.
  METHOD selec_dados_ov.

    FREE: it_saida.
    CLEAR: wa_saida.

    DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
          xz_vlr_fortet   TYPE zfit0026-mont_moeda,
          xz_vlr_interna  TYPE zfit0026-mont_mi,
          xz_vlr_internat TYPE zfit0026-mont_mi,
          xtotalq_ov      TYPE vbap-kwmeng,
          xtotalvl_ov     TYPE vbap-netwr,
          vvalor          TYPE zfit0026-mont_moeda,
          pare            TYPE n LENGTH 3,
          vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

    SORT:  it_vbak          BY vbeln,
           it_vbap          BY vbeln,
           it_vbkd          BY vbeln,
           it_zfit0026      BY vbeln,
           it_kna1          BY kunnr,
           it_bsad          BY bukrs kunnr belnr ,
           it_t052u         BY zterm.


*  DATA(OBJ_AUART) = NEW ZCL_TAXA_CURVA( ).
*  R_DEVO_RECU = OBJ_AUART->GET_AUART( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa

    LOOP AT it_vbak INTO wa_vbak.
* Verifca a condição se OV ou Fatura
*      SELECT SINGLE T~ZDART
*       FROM VBKD AS DK
*       INNER JOIN VBAK AS AK ON AK~VBELN = DK~VBELN
*       INNER JOIN T052 AS T  ON T~ZTERM  = DK~ZTERM
*       INTO @DATA(V_ZDART)
*      WHERE DK~VBELN = @WA_VBAK-VBELN.
*
*
*      IF V_ZDART = 'B'.
*
*        "carrega dados da faturas.
*        SELECT RK~*
*          FROM   VBRK  AS RK
*          INNER JOIN VBFA AS FA  ON FA~VBELN = RK~VBELN
*          INTO TABLE @DATA(IT_VBRK)
*         WHERE FA~VBELV = @WA_VBAK-VBELN
*          AND  FA~VBTYP_N  = 'M'
*          AND  FA~VBTYP_V  = 'C'.
**        AND  RK~VBELN    = FA~VBELV.
*
*        PERFORM F_CONSTRUIR_SAIDA_FATURAS   TABLES IT_VBRK USING  WA_VBAK-VBELN.
*
*      ENDIF.

*      IF V_ZDART <> 'B'.

      xtotalq_ov  = 0.
      xtotalvl_ov = 0.
**<<<------"152483 - NMS - INI------>>>
      wa_saida-tprel = gc_in. "IN = Insumos
**<<<------"152483 - NMS - FIM------>>>
*     INICIO MODIFICAÇÃO WELGEM
      CLEAR vbeln_aux.
      pare = 0.
      vbeln_aux = wa_vbak-vbeln.

      READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
      IF sy-subrc IS INITIAL.
        wa_saida-vbeln_p = wa_0041-vbeln.
        wa_saida-vbeln_s = wa_0041-doc_simulacao.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                       AND vbelv NE vbeln_aux.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.   "<<RIM-SKM-IR107590-12.08.22
          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
          ENDIF.
        ENDIF.              "<<RIM-SKM-IR107590-12.08.22
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        WHILE pare IS INITIAL.

          pare = 4.
          LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                         AND vbelv NE vbeln_aux.
            pare = 0.
            EXIT.
          ENDLOOP.

          IF pare IS INITIAL.
            vbeln_aux = wa_0090-vbelv.
          ENDIF.

          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
          ENDIF.

        ENDWHILE.
      ENDIF.

* INCLUINDO AS REMESSAS CS2016000023 >>>>>
      READ TABLE it_vbfa INTO DATA(w_vbfa) WITH KEY vbeln = vbeln_aux.
      IF sy-subrc IS INITIAL.
        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = w_vbfa-vbelv.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ELSE.
          READ TABLE it_0090 INTO wa_0090 WITH KEY vbeln = w_vbfa-vbelv.
          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
*** Alteracao ARM - CHAMADO CS1124692 - IR146189 - 03.08.2023

          ELSE.
            DATA v_vbelnf TYPE vbfa-vbelv.
            v_vbelnf = wa_0090-vbelv.
            READ TABLE it_0090 INTO wa_0090 WITH KEY vbeln = v_vbelnf.
            READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
            IF sy-subrc IS INITIAL.
              wa_saida-vbeln_p = wa_0041-vbeln.
              wa_saida-vbeln_s = wa_0041-doc_simulacao.
            ELSE.
              v_vbelnf = wa_0090-vbelv.
              READ TABLE it_0090 INTO wa_0090 WITH KEY vbeln = v_vbelnf.
              READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
              IF sy-subrc IS INITIAL.
                wa_saida-vbeln_p = wa_0041-vbeln.
                wa_saida-vbeln_s = wa_0041-doc_simulacao.
              ENDIF.
            ENDIF.

*** Fim Altecarao ARM - CHAMADO CS1124692 - IR146189 - 03.08.2023
          ENDIF.
        ENDIF.
      ENDIF.
* INCLUINDO AS REMESSAS CS2016000023 <<<<<

* INCLUINDO O CAMPO SAFRA CS2016000023 >>>>>
      READ TABLE it_0040 INTO DATA(w_0040) WITH KEY doc_simulacao = wa_saida-vbeln_s.
      IF sy-subrc IS INITIAL.
        wa_saida-safra = w_0040-safra.
        wa_saida-id_order_ecommerce = w_0040-id_order_ecommerce. "SMC #123881 - EQUALIZAÇÃO ECC X HANA

        "PAGAMENTO ANTECIPADO
        CASE w_0040-meio_pago .
          WHEN 'D' .
            wa_saida-pgto_ant = 'Deposito em Conta'.
          WHEN 'A' .
            wa_saida-pgto_ant = 'Acerto'.
          WHEN 'B' .
            wa_saida-pgto_ant = 'Boleto Bancário'.
          WHEN ' ' .
            wa_saida-pgto_ant = 'Não Atencipado'.
        ENDCASE.


        " TAXA MULTA

*      WA_SAIDA-TX_MULTA = IT_0040-JUROS_ANO . " Comentado porque não tem esse campo no simulador.
        wa_saida-tx_juros = w_0040-juros_ano .

        "CS CS2020000381
        READ TABLE it_zsdt0038 WITH KEY cultura = w_0040-cultura INTO DATA(wa_cultura) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-cultura = wa_cultura-descricao.
        ENDIF.

      ENDIF.

      " END TAXA MULTA


      CLEAR it_0040.
* INCLUINDO O CAMPO SAFRA CS2016000023 <<<<<<

*      FIM MODIFICAÇÃO WELGEM
      LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.
        IF wa_vbak-auart EQ 'ZFUT'  OR  wa_vbak-auart EQ 'ZTRI'.
          ADD wa_vbap-zmeng TO xtotalq_ov.
          ADD wa_vbap-netwr TO xtotalvl_ov.
          ADD wa_vbap-mwsbp TO xtotalvl_ov.
          ADD wa_vbap-netwr TO wa_saida-netwr_l.
          ADD wa_vbap-mwsbp TO wa_saida-mwsbp.

        ELSE.
          ADD wa_vbap-kwmeng TO xtotalq_ov.
          ADD wa_vbap-netwr TO xtotalvl_ov.
          ADD wa_vbap-mwsbp TO xtotalvl_ov.
          ADD wa_vbap-netwr TO wa_saida-netwr_l.
          ADD wa_vbap-mwsbp TO wa_saida-mwsbp.
        ENDIF.
      ENDLOOP.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 >>>>>>
*    CASE WA_VBAK-AUART.
*      WHEN 'ZRPF' OR 'ZROB' OR 'ZREB'.
*        XTOTALQ_OV  = XTOTALQ_OV  * -1.
*        XTOTALVL_OV = XTOTALVL_OV * -1.
*        WA_SAIDA-NETWR_L = WA_SAIDA-NETWR_L * -1.
*        WA_SAIDA-MWSBP = WA_SAIDA-MWSBP * -1.
*
*      WHEN 'ZREM' OR 'ZRFU'.
*        XTOTALQ_OV  = 0.
*        XTOTALVL_OV = 0.
*    ENDCASE.

*    IF WA_VBAK-AUART IN R_DEVO_RECU.

*&------------Inicio ajuste cs2024000717 / aoenning &*
      "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.

      SELECT SINGLE * FROM tvarvc INTO @DATA(wa_ov_devolucao)
        WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
         AND  low  EQ @wa_vbak-auart.

      IF wa_vbak-auart EQ wa_ov_devolucao-low.
        xtotalq_ov  = xtotalq_ov  * -1.
        xtotalvl_ov = xtotalvl_ov * -1.
        wa_saida-netwr_l = wa_saida-netwr_l * -1.
        wa_saida-mwsbp = wa_saida-mwsbp * -1.

      ELSEIF  wa_vbak-auart EQ 'ZREM' OR  wa_vbak-auart EQ 'ZRFU'.
        xtotalq_ov  = 0.
        xtotalvl_ov = 0.
      ENDIF.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<
      READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      READ TABLE it_kna1  INTO wa_kna1  WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.

      wa_saida-data_venc  = wa_vbkd-valdt.

      READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm BINARY SEARCH.

      wa_saida-werks      = wa_vbap-werks.

      IF wa_vbak-vkgrp IS NOT INITIAL.
        SELECT SINGLE bezei
      FROM tvgrt
      INTO @DATA(z_bezei)
       WHERE spras EQ @sy-langu
         AND vkgrp EQ @wa_vbak-vkgrp.
        wa_saida-vkgrp = wa_vbak-vkgrp && '-' && z_bezei.
      ENDIF.

*        WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
      wa_saida-vkbur      = wa_vbak-vkbur.
      wa_saida-kunnr      = wa_vbak-kunnr.
      wa_saida-name1      = wa_kna1-name1.
      wa_saida-auart      = wa_vbak-auart.
      wa_saida-zterm      = wa_vbkd-zterm.
      wa_saida-text1      = wa_t052u-text1.
      wa_saida-vbeln      = wa_vbak-vbeln.
      wa_saida-vbeln_g    = wa_vbak-vbeln.
      wa_saida-erdat      = wa_vbak-erdat.
      wa_saida-waerk      = wa_vbak-waerk.

      xz_vlr_forte   = 0.
      xz_vlr_interna = 0.

      LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln.
        "MOVE: WA_ZFIT0026-OBSERVACAO TO WA_SAIDA-OBSERVACAO. "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
        ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
        ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.
      ENDLOOP.

      "QTDE FATURADO NFe
      DATA(t_zfit0026_group_by_doc_fatura) = it_zfit0026[].

      SORT t_zfit0026_group_by_doc_fatura BY doc_fatura.
      DELETE ADJACENT DUPLICATES FROM t_zfit0026_group_by_doc_fatura COMPARING doc_fatura.

      LOOP AT t_zfit0026_group_by_doc_fatura INTO  DATA(wa_zfit26_group_by_doc_fatura) WHERE  vbeln = wa_vbak-vbeln.
        IF wa_zfit26_group_by_doc_fatura-doc_fatura IS NOT INITIAL .
          SELECT SUM( fkimg )
          FROM vbrp
          INTO @DATA(qtde_faturado_nfe)
         WHERE vbeln = @wa_zfit26_group_by_doc_fatura-doc_fatura.
        ENDIF.

        ADD qtde_faturado_nfe TO wa_saida-fkimg.

      ENDLOOP.
      "END QTDE FATURA.

* Salva totais
      xz_vlr_fortet   = xz_vlr_forte.
      xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
      IF wa_vbak-waerk = 'USD'.
        xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
        xz_vlr_interna = 0.
      ELSE.
        xz_vlr_forte = 0.
        xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
      ENDIF.

      vvalor = xtotalq_ov.
      wa_saida-totalq_ov = vvalor.
      vvalor = xtotalvl_ov.
      wa_saida-totvl_ov = vvalor.

      DATA: vflag TYPE i,
            vcont TYPE i.
      vflag = 0.
      vcont = 0.


*      LOOP AT IT_ZFIT0026 INTO WA_ZFIT0026 WHERE VBELN = WA_VBAK-VBELN.
*        WA_SAIDA-VLR_MULTA_CALC = WA_ZFIT0026-VLR_MULTA_CALC.  "MULTA CALCULADO
*        WA_SAIDA-VLR_MULTA_RBDO = WA_ZFIT0026-VLR_MULTA_RBDO.  "MULTA RECEBIDA
*        WA_SAIDA-VLR_JUROS_CALC = WA_ZFIT0026-VLR_JUROS_CALC.  "JUROS CALCULADO
*        WA_SAIDA-VLR_JUROS_RBDO = WA_ZFIT0026-VLR_JUROS_RBDO. "JUROS RECEBIDO
*        WA_SAIDA-VLR_DESC_MULT  = WA_ZFIT0026-VLR_DESC_MULT.
*        WA_SAIDA-VLR_DESC_JROS  = WA_ZFIT0026-VLR_DESC_JROS.
*
*        IF WA_SAIDA-WAERK EQ 'USD'.
*          WA_SAIDA-VLR_SALD_FIN = ( WA_ZFIT0026-VLR_JUROS_CALC - WA_ZFIT0026-VLR_JUROS_RBDO - WA_ZFIT0026-VLR_DESC_JROS ) + ( WA_ZFIT0026-VLR_MULTA_CALC - WA_ZFIT0026-VLR_MULTA_RBDO - WA_ZFIT0026-VLR_DESC_MULT ).
*        ELSE.
*          WA_SAIDA-VLR_SALD_FIN_BRL = ( WA_ZFIT0026-VLR_JUROS_CALC - WA_ZFIT0026-VLR_JUROS_RBDO - WA_ZFIT0026-VLR_DESC_JROS ) + ( WA_ZFIT0026-VLR_MULTA_CALC - WA_ZFIT0026-VLR_MULTA_RBDO - WA_ZFIT0026-VLR_DESC_MULT ).
*        ENDIF.
*
*        WA_SAIDA-PTAX = WA_ZFIT0026-TAXA. "PTAX
*        MOVE: WA_ZFIT0026-OBSERVACAO TO WA_SAIDA-OBSERVACAO.
*
*        ADD 1 TO VCONT.
*        READ TABLE IT_BSAD  INTO WA_BSAD  WITH KEY BUKRS = WA_VBAK-VKORG
*                                                   KUNNR = WA_VBAK-KUNNR
*                                                   BELNR = WA_ZFIT0026-DOCNUM.
*        IF SY-SUBRC IS NOT INITIAL.
*          READ TABLE IT_Z0159 INTO WA_Z0159 WITH KEY OBJ_KEY = WA_ZFIT0026-OBJ_KEY.
*          READ TABLE IT_BSAD  INTO WA_BSAD  WITH KEY BUKRS = WA_VBAK-VKORG
*                                         KUNNR = WA_VBAK-KUNNR
*                                         BELNR = WA_Z0159-ADIANT.
*        ENDIF.
*
*        FREE: IT_COLOR, T_COR.
*
*        IF SY-SUBRC IS INITIAL.
*          WA_SAIDA-AUGBL     = WA_BSAD-AUGBL.
**        WA_SAIDA-BUDAT     = WA_BSAD-BUDAT.
*          WA_SAIDA-BUDAT     = WA_BSAD-AUGDT.
**          WA_SAIDA-DMBE2     = WA_BSAD-DMBE2.
**          WA_SAIDA-DMBTR     = WA_BSAD-DMBTR.
*          WA_SAIDA-DMBE2     = WA_ZFIT0026-MONT_MOEDA.
*          WA_SAIDA-DMBTR      = ( WA_ZFIT0026-MONT_MOEDA * WA_ZFIT0026-TAXA ).
*
*          T_COR =
*          VALUE #(
*                   ( FNAME = 'AUGBL' COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                   ( FNAME = 'BUDAT' COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                   ( FNAME = 'DMBE2' COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                   ( FNAME = 'DMBTR' COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                 ).
*          APPEND LINES OF T_COR TO IT_COLOR.
*
*        ELSE.
*          WA_SAIDA-DMBE2     = 0.
*          WA_SAIDA-DMBTR     = 0.
*          WA_SAIDA-AUGBL     = ''.
*        ENDIF.
*
*        WA_SAIDA-FORMA_PAG  = WA_ZFIT0026-FORMA_PAG.
*        WA_SAIDA-TAXA       = WA_ZFIT0026-TAXA.
*        WA_SAIDA-MONT_MOEDA = COND #( WHEN WA_VBAK-WAERK EQ 'USD' THEN WA_ZFIT0026-MONT_MOEDA ELSE 0 ).
*        WA_SAIDA-MONT_MI = COND #( WHEN WA_VBAK-WAERK EQ 'BRL' THEN WA_ZFIT0026-MONT_MI ELSE 0 ).
*        WA_SAIDA-DOCNUM     = WA_ZFIT0026-DOCNUM.
*
*        IF WA_SAIDA-DOCNUM IS INITIAL.
*          WA_SAIDA-DOCNUM = WA_Z0159-ADIANT.
*        ENDIF.
*
*        T_COR =
*        VALUE #(
*                 ( FNAME = 'FORMA_PAG'  COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                 ( FNAME = 'TAXA'       COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                 ( FNAME = 'MONT_MOEDA' COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                 ( FNAME = 'MONT_MI'    COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*                 ( FNAME = 'DOCNUM'     COLOR-COL = '5' COLOR-INT = '1' COLOR-INV = '1' )
*               ).
*        APPEND LINES OF T_COR TO IT_COLOR.
*
*        IF VCONT = 1.
*          WA_SAIDA-MOEDA_FORTE = XZ_VLR_FORTE.
*          WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA.
*
*          T_COR =
*          VALUE #(
*                   ( FNAME = 'VLR_SALD_FIN' COLOR-COL = '6' COLOR-INT = '1' COLOR-INV = '1' )
*                   ( FNAME = 'MOEDA_FORTE' COLOR-COL = '6' COLOR-INT = '1' COLOR-INV = '1' )
*                   ( FNAME = 'MOEDA_INTER' COLOR-COL = '6' COLOR-INT = '1' COLOR-INV = '1' )
*                 ).
*          APPEND LINES OF T_COR TO IT_COLOR.
*
*        ELSE.
*          WA_SAIDA-MOEDA_FORTE = 0.
*          WA_SAIDA-MOEDA_INTER = 0.
*
*          T_COR =
*          VALUE #(
*                   ( FNAME = 'VLR_SALD_FIN' COLOR-COL = '2' COLOR-INT = '0' COLOR-INV = '1' )
*                   ( FNAME = 'MOEDA_FORTE' COLOR-COL = '2' COLOR-INT = '0' COLOR-INV = '1' )
*                   ( FNAME = 'MOEDA_INTER' COLOR-COL = '2' COLOR-INT = '0' COLOR-INV = '1' )
*                 ).
*          APPEND LINES OF T_COR TO IT_COLOR.
*
*          CLEAR WA_SAIDA-LINE_COLOR.
*        ENDIF.
*
*        WA_SAIDA-SALUS = ( WA_SAIDA-MONT_MOEDA + WA_SAIDA-MOEDA_FORTE ) - WA_SAIDA-DMBE2.
*        WA_SAIDA-SALRE = ( WA_SAIDA-MONT_MI    + WA_SAIDA-MOEDA_INTER ) - WA_SAIDA-DMBTR.
*        WA_SAIDA-COLOR_CELL[] = IT_COLOR[].
*
*        IF RB1 = 'X'.
*          IF WA_SAIDA-AUGBL IS NOT INITIAL.
*            APPEND WA_SAIDA TO IT_SAIDA.
*            VFLAG = 1.
*          ENDIF.
*        ELSEIF RB2 = 'X'.
*          IF ( WA_SAIDA-WAERK = 'USD' AND XZ_VLR_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND XZ_VLR_INTERNA NE 0 ).
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSE.
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.
*        WA_SAIDA-AUGBL     = ''.
*      ENDLOOP.

      CLEAR: wa_saida-forma_pag, wa_saida-taxa, wa_saida-docnum, wa_saida-augbl, wa_saida-color_cell[].
      REFRESH it_color.

      IF vcont = 0.
        wa_saida-mont_moeda  = 0.
        wa_saida-mont_mi     = 0.
        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.
        wa_saida-augbl     = wa_bsad-augbl.
*          WA_SAIDA-MOEDA_FORTE = XZ_VLR_FORTE.
*          WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA.

        IF wa_saida-waerk <> 'BRL'.
          wa_saida-moeda_forte = wa_saida-totvl_ov.
        ELSE.
          wa_saida-moeda_inter = wa_saida-totvl_ov.
        ENDIF.

        t_cor =
        VALUE #(
                ( fname = 'VLR_SALD_FIN'     color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'SALD_REFERENCIA'  color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'MOEDA_FORTE'      color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'MOEDA_INTER'      color-col = '6' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        wa_saida-color_cell[] = it_color[].
        wa_saida-line_color  = 'C310'.

        READ TABLE it_zfit0026 INTO wa_zfit0026 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          wa_saida-salus = wa_saida-moeda_forte.
          wa_saida-salre = wa_saida-moeda_inter.
          "ELSE.
          " WA_SAIDA-NUM_COMP_ADIANT = WA_ZFIT0026-NUM_COMP_ADIANT. "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
        ENDIF.

*          IF RB1 = 'X'.
*            IF VFLAG = 1.
*              APPEND WA_SAIDA TO IT_SAIDA.
*            ENDIF.
*          ELSEIF RB2 = 'X'.
*            IF ( WA_SAIDA-WAERK = 'USD' AND WA_SAIDA-MOEDA_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND WA_SAIDA-MOEDA_INTER NE 0 ).
*              APPEND WA_SAIDA TO IT_SAIDA.
*            ENDIF.
*          ELSE.
        APPEND wa_saida TO it_saida.
*          ENDIF.
      ENDIF.

      CLEAR: wa_ov_devolucao, wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_saida, wa_color, wa_0090, wa_0090x, wa_0041.
      FREE it_color.
*      ENDIF.
    ENDLOOP.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
*      <F_SAIDA>-RFMNG = REDUCE RFMNG( INIT X TYPE RFMNG FOR LS IN IT_REMESSA WHERE ( VBELV EQ <F_SAIDA>-VBELN_G ) NEXT X = X + LS-RFMNG ).

*&------------Inicio ajuste cs2024000717 / aoenning &*
      "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.

      CLEAR: wa_ov_devolucao.
      SELECT SINGLE * FROM tvarvc INTO wa_ov_devolucao
        WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
         AND  low  EQ <f_saida>-auart.

      LOOP AT it_remessa ASSIGNING FIELD-SYMBOL(<_remessa>) WHERE vbelv EQ <f_saida>-vbeln_g.
        IF <f_saida>-auart EQ wa_ov_devolucao-low AND <_remessa>-vbtyp_n EQ 'T'.
          <f_saida>-rfmng = <f_saida>-rfmng + <_remessa>-rfmng.
        ELSE.
*          IF <_remessa>-vbtyp_n EQ 'J'.                                 ">> RIM-SKM-IR113139-03.10.22
          IF <_remessa>-vbtyp_n EQ 'J' AND <_remessa>-bwart NE space.    "<< RIM-SKM-IR113139-03.10.22
            <f_saida>-rfmng = <f_saida>-rfmng + <_remessa>-rfmng.
          ENDIF.
        ENDIF.
      ENDLOOP.

      READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <f_saida>-vbeln.
      IF sy-subrc EQ 0.
        IF wa_vbak-auart EQ wa_ov_devolucao-low.
          <f_saida>-rfmng = <f_saida>-totalq_ov.
        ENDIF.
      ENDIF.

    ENDLOOP.

*    CHECK RB2 EQ ABAP_TRUE.
*
*    LOOP AT IT_SAIDA INTO WA_SAIDA.
*      WA_DELETE =
*      VALUE #(
*               VBELN_S     = WA_SAIDA-VBELN_S
*               VBELN_P     = WA_SAIDA-VBELN_P
*               MOEDA_FORTE = WA_SAIDA-MOEDA_FORTE
*               MOEDA_INTER = WA_SAIDA-MOEDA_INTER
*             ).
*
*      COLLECT WA_DELETE INTO IT_DELETE.
*      CLEAR WA_DELETE.
*
*    ENDLOOP.
*
*    DELETE IT_DELETE WHERE MOEDA_FORTE > 10 OR  MOEDA_INTER > 10.
*
*    LOOP AT IT_DELETE INTO WA_DELETE.
*      DELETE IT_SAIDA WHERE VBELN_S EQ WA_DELETE-VBELN_S AND
*                            VBELN_P EQ WA_DELETE-VBELN_P.
*    ENDLOOP.


  ENDMETHOD.

*****  Selecionar dados da fatura.
  METHOD selec_dados_fatura.

  ENDMETHOD.

*****  Selecionar dados da fatura.
  METHOD chkec_cond_ov.

*   Verifca a condição se OV ou Fatura
    SELECT SINGLE t~zdart
     FROM vbkd AS dk
     INNER JOIN vbak AS ak ON ak~vbeln = dk~vbeln
     INNER JOIN t052 AS t  ON t~zterm  = dk~zterm
     INTO @DATA(v_zdart)
    WHERE dk~vbeln = @i_saida-vbeln.

    e_zdart = v_zdart.

  ENDMETHOD.


*****  Selecionar nfe ov (Referencia).
  METHOD check_nfe_ov.

  ENDMETHOD.

*****  Selecionar dados da fatura.
  METHOD check_recebimentos.

    DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
          xz_vlr_fortet   TYPE zfit0026-mont_moeda,
          xz_vlr_interna  TYPE zfit0026-mont_mi,
          xz_vlr_internat TYPE zfit0026-mont_mi,
          xtotalq_ov      TYPE vbap-kwmeng,
          xtotalvl_ov     TYPE vbap-netwr,
          vvalor          TYPE zfit0026-mont_moeda,
          pare            TYPE n LENGTH 3,
          qt_fatura       TYPE p DECIMALS 2,
          vbeln_aux       TYPE vbeln.

*         R_DEVO_RECU     TYPE RANGE OF AUART.

    SORT:  it_vbak          BY vbeln,
           it_vbap          BY vbeln,
           it_vbkd          BY vbeln,
           it_zfit0026      BY vbeln,
           it_kna1          BY kunnr,
           it_bsad          BY bukrs kunnr belnr ,
           it_t052u         BY zterm.

    DATA: saldo_ov TYPE bsad-dmbtr.
    DATA: nr_ov TYPE zfit0026-vbeln.
    DATA: saldo_ov_brl TYPE zfit0026-mont_moeda.
    DATA: saldo_ov_dol TYPE zfit0026-mont_moeda.
    DATA: saldo_fatura TYPE zfit0026-mont_moeda.
    CLEAR: qt_fatura.


*    WA_SAIDA-DATA_VENC  = I_SAIDA-DATA_VENC.
*    WA_SAIDA-WERKS      = I_SAIDA-WERKS.
*    WA_SAIDA-VKGRP = I_SAIDA-VKGRP.
**      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
*    WA_SAIDA-VKBUR      = I_SAIDA-VKBUR.
*    WA_SAIDA-KUNNR      = I_SAIDA-KUNNR.
*    WA_SAIDA-NAME1      = I_SAIDA-NAME1.
*    WA_SAIDA-AUART      = I_SAIDA-AUART.
*    WA_SAIDA-ZTERM      = I_SAIDA-ZTERM.
*    WA_SAIDA-TEXT1      = I_SAIDA-TEXT1.
*    WA_SAIDA-VBELN      = I_SAIDA-VBELN.
*    WA_SAIDA-VBELN_G    = I_SAIDA-VBELN.
*    WA_SAIDA-ERDAT      = I_SAIDA-ERDAT.
*    WA_SAIDA-WAERK      = I_SAIDA-WAERK.

*    XZ_VLR_FORTE   = 0.
*    XZ_VLR_INTERNA = 0.

    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = i_saida-vbeln.
      MOVE: wa_zfit0026-observacao TO wa_saida-observacao.
      ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
      ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.
    ENDLOOP.

    "QTDE FATURADO NFe
*    DATA(T_ZFIT0026_GROUP_BY_DOC_FATURA) = IT_ZFIT0026[].

*    SORT T_ZFIT0026_GROUP_BY_DOC_FATURA BY DOC_FATURA.
*    DELETE ADJACENT DUPLICATES FROM T_ZFIT0026_GROUP_BY_DOC_FATURA COMPARING DOC_FATURA.
*
*    LOOP AT T_ZFIT0026_GROUP_BY_DOC_FATURA INTO  DATA(WA_ZFIT26_GROUP_BY_DOC_FATURA) WHERE  VBELN = I_SAIDA-VBELN.
*      IF WA_ZFIT26_GROUP_BY_DOC_FATURA-DOC_FATURA IS NOT INITIAL .
*        SELECT SUM( FKIMG )
*        FROM VBRP
*        INTO @DATA(QTDE_FATURADO_NFE)
*       WHERE VBELN = @WA_ZFIT26_GROUP_BY_DOC_FATURA-DOC_FATURA.
*      ENDIF.
*
*      ADD QTDE_FATURADO_NFE TO QT_FATURA.
*
*    ENDLOOP.
*    I_SAIDA-FKIMG = QT_FATURA.
    "END QTDE FATURA.

* Salva totais
    xz_vlr_fortet   = xz_vlr_forte.
    xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
    IF wa_vbak-waerk = 'USD'.
      xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
      xz_vlr_interna = 0.
    ELSE.
      xz_vlr_forte = 0.
      xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
    ENDIF.

    vvalor = xtotalq_ov.
    wa_saida-totalq_ov = vvalor.
    vvalor = xtotalvl_ov.
    wa_saida-totvl_ov = vvalor.

    DATA: vflag TYPE i,
          vcont TYPE i.
    vflag = 0.
    vcont = 0.


    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = i_saida-vbeln.

****************************

      wa_saida-data_venc  = i_saida-data_venc.
      wa_saida-werks      = i_saida-werks.
      wa_saida-vkgrp      = i_saida-vkgrp.
*      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
      wa_saida-vkbur      = i_saida-vkbur.
      wa_saida-kunnr      = i_saida-kunnr.
      wa_saida-name1      = i_saida-name1.
      wa_saida-auart      = i_saida-auart.
      wa_saida-zterm      = i_saida-zterm.
      wa_saida-text1      = i_saida-text1.
      wa_saida-vbeln      = i_saida-vbeln.
      wa_saida-vbeln_g    = i_saida-vbeln.
      wa_saida-erdat      = i_saida-erdat.
      wa_saida-waerk      = i_saida-waerk.
      wa_saida-num_comp_adiant = wa_zfit0026-num_comp_adiant. "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
****************************

      CLEAR vbeln_aux.
      pare = 0.
      vbeln_aux = i_saida-vbeln.

      READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
      IF sy-subrc IS INITIAL.
        wa_saida-vbeln_p = wa_0041-vbeln.
        wa_saida-vbeln_s = wa_0041-doc_simulacao.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                       AND vbelv NE vbeln_aux.
          EXIT.
        ENDLOOP.

        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ENDIF.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        WHILE pare IS INITIAL.

          pare = 4.
          LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                         AND vbelv NE vbeln_aux.
            pare = 0.
            EXIT.
          ENDLOOP.

          IF pare IS INITIAL.
            vbeln_aux = wa_0090-vbelv.
          ENDIF.

          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
          ENDIF.

        ENDWHILE.
      ENDIF.

* INCLUINDO AS REMESSAS CS2016000023 >>>>>
      READ TABLE it_vbfa INTO DATA(w_vbfa) WITH KEY vbeln = vbeln_aux.
      IF sy-subrc IS INITIAL.
        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = w_vbfa-vbelv.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ELSE.
          READ TABLE it_0090 INTO wa_0090 WITH KEY vbeln = w_vbfa-vbelv.
          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
          ENDIF.
        ENDIF.
      ENDIF.

      wa_saida-zid_lanc       = wa_zfit0026-zid_lanc.
      wa_saida-vlr_multa_calc = wa_zfit0026-vlr_multa_calc.  "MULTA CALCULADO
      wa_saida-vlr_multa_rbdo = wa_zfit0026-vlr_multa_rbdo.  "MULTA RECEBIDA
      wa_saida-vlr_juros_calc = wa_zfit0026-vlr_juros_calc.  "JUROS CALCULADO
      wa_saida-vlr_juros_rbdo = wa_zfit0026-vlr_juros_rbdo. "JUROS RECEBIDO
      wa_saida-vlr_desc_mult  = wa_zfit0026-vlr_desc_mult.
      wa_saida-vlr_desc_jros  = wa_zfit0026-vlr_desc_jros.


      t_cor =
      VALUE #(
               ( fname = 'VLR_MULTA_CALC'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_MULTA_RBDO'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_JUROS_CALC'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_JUROS_RBDO'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_DESC_MULT'     color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'VLR_DESC_JROS'     color-col = '5' color-int = '1' color-inv = '1' ) ).
      APPEND LINES OF t_cor TO it_color.

      IF i_saida-waerk EQ 'USD'.
        wa_saida-vlr_sald_fin = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ELSE.
        wa_saida-vlr_sald_fin_brl = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ENDIF.

      wa_saida-ptax = wa_zfit0026-taxa. "PTAX
      MOVE: wa_zfit0026-observacao TO wa_saida-observacao.

      ADD 1 TO vcont.

*****Preencher as informações de referente recebimento caso for uma ajuste, senão preencher os valores do documento.
*      IF wa_zfit0026-ajuste IS NOT INITIAL.
*        wa_saida-budat     = wa_zfit0026-data_pgto.

*        IF i_saida-waerk EQ 'USD'.
*          wa_saida-dmbtr     = ( wa_zfit0026-mont_rbdo * wa_zfit0026-taxa ).
*          wa_saida-dmbe2     = wa_zfit0026-mont_rbdo.
*        ELSE.
*          wa_saida-dmbtr     = wa_zfit0026-mont_rbdo.
*          wa_saida-dmbe2     = ( wa_zfit0026-mont_rbdo / wa_zfit0026-taxa ).
*        ENDIF.

      IF wa_zfit0026-ajuste IS NOT INITIAL.
        wa_saida-budat         = wa_zfit0026-data_pgto.

        IF i_saida-waerk EQ 'USD'.
          IF wa_zfit0026-mont_moeda IS NOT INITIAL.
            wa_saida-dmbtr     = ( wa_zfit0026-mont_moeda * wa_zfit0026-taxa ).
            wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          ELSE.
            wa_saida-dmbtr     = wa_zfit0026-mont_mi.
            wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          ENDIF.
        ELSE.
          IF wa_zfit0026-mont_moeda IS NOT INITIAL.
            wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
            wa_saida-dmbe2     = ( wa_zfit0026-mont_moeda / wa_zfit0026-taxa ).
          ELSE.
            wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
            wa_saida-dmbe2     = wa_zfit0026-mont_mi.
          ENDIF.
        ENDIF.


      ELSE.

*        READ TABLE IT_BSAD  INTO WA_BSAD  WITH KEY BUKRS = WA_VBAK-VKORG
*                                                   KUNNR = WA_VBAK-KUNNR
*                                                   BELNR = WA_ZFIT0026-DOCNUM.
        READ TABLE it_bsad  INTO wa_bsad  WITH KEY kunnr = i_saida-kunnr
                                                   belnr = wa_zfit0026-docnum.


        IF sy-subrc IS NOT INITIAL.

          READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.

          READ TABLE it_bsad  INTO wa_bsad  WITH KEY  kunnr = i_saida-kunnr
                                                        belnr = wa_z0159-adiant.
          IF sy-subrc IS NOT INITIAL.

            READ TABLE it_zibchv INTO wa_zibchv WITH KEY obj_key = wa_zfit0026-obj_key.

            READ TABLE it_bsad  INTO wa_bsad  WITH KEY  kunnr = i_saida-kunnr
                                                        belnr = wa_zibchv-belnr.
          ENDIF.
        ENDIF.


        FREE: t_cor.

        IF sy-subrc IS INITIAL.
          wa_saida-augbl     = wa_bsad-augbl.
*        WA_SAIDA-BUDAT     = WA_BSAD-BUDAT.
          wa_saida-budat     = wa_bsad-augdt.
*          WA_SAIDA-DMBE2     = WA_BSAD-DMBE2.
**          WA_SAIDA-DMBTR     = WA_BSAD-DMBTR.
*          WA_SAIDA-DMBE2     = WA_ZFIT0026-MONT_MOEDA.
*          WA_SAIDA-DMBTR      = ( WA_ZFIT0026-MONT_MOEDA * WA_ZFIT0026-TAXA ).

          IF i_saida-waerk EQ 'BRL'.
***     Calculando o valor recebido liquido.
            wa_saida-dmbtr = wa_bsad-dmbtr - ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
            CLEAR: ptax.
            ptax = ( wa_bsad-dmbtr / wa_bsad-dmbe2 ).
            IF ptax IS NOT INITIAL.
              juros_multa = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) / ptax.
              wa_saida-dmbe2     = wa_bsad-dmbe2 - juros_multa.
            ENDIF.
            CLEAR: juros_multa.
          ELSE.

*** Calculando o valor recebido liquido.
            wa_saida-dmbe2 = wa_bsad-dmbe2 - ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
            CLEAR: ptax.
            ptax = ( wa_bsad-dmbtr / wa_bsad-dmbe2 ).
            IF ptax IS NOT INITIAL.
              juros_multa = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) * ptax.
              wa_saida-dmbtr     = wa_bsad-dmbtr - juros_multa.
            ENDIF.
            CLEAR: juros_multa.
          ENDIF.

          t_cor =
          VALUE #(
                   ( fname = 'AUGBL' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'BUDAT' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBE2' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBTR' color-col = '5' color-int = '1' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.

        ELSE.
          wa_saida-dmbe2     = 0.
          wa_saida-dmbtr     = 0.
          wa_saida-augbl     = ''.
        ENDIF.
      ENDIF.

      wa_saida-forma_pag  = wa_zfit0026-forma_pag.
      wa_saida-taxa       = wa_zfit0026-taxa.
      wa_saida-mont_moeda = COND #( WHEN i_saida-waerk EQ 'USD' THEN wa_zfit0026-mont_moeda ELSE 0 ).
      wa_saida-mont_mi = COND #( WHEN i_saida-waerk EQ 'BRL' THEN wa_zfit0026-mont_mi ELSE 0 ).
      wa_saida-docnum     = wa_zfit0026-docnum.

      IF wa_saida-docnum IS INITIAL.
        IF wa_z0159-adiant IS NOT INITIAL.
          wa_saida-docnum = wa_z0159-adiant.
        ELSE.
          wa_saida-docnum = wa_zibchv-belnr.
        ENDIF.
      ENDIF.


      t_cor =
      VALUE #(
               ( fname = 'FORMA_PAG'  color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'TAXA'       color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'MONT_MOEDA' color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'MONT_MI'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'DOCNUM'     color-col = '5' color-int = '1' color-inv = '1' )
             ).
      APPEND LINES OF t_cor TO it_color.

      IF vcont NE 0.
*        WA_SAIDA-MOEDA_FORTE = XZ_VLR_FORTE.
*        WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA.

        t_cor =
        VALUE #(
                 ( fname = 'VLR_SALD_FIN'     color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'MOEDA_FORTE'      color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'SALD_REFERENCIA'  color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'MOEDA_INTER'      color-col = '6' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.
        FREE t_cor.

      ELSE.
        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.

        t_cor =
        VALUE #(
                 ( fname = 'VLR_SALD_FIN'     color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'MOEDA_FORTE'      color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'SALD_REFERENCIA'  color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'VLR_SALD_FIN_BRL' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'MOEDA_INTER'      color-col = '2' color-int = '0' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        CLEAR wa_saida-line_color.
      ENDIF.

      wa_saida-salus = ( wa_saida-mont_moeda + wa_saida-moeda_forte ) - wa_saida-dmbe2.
      wa_saida-salre = ( wa_saida-mont_mi    + wa_saida-moeda_inter ) - wa_saida-dmbtr.
      wa_saida-color_cell[] = it_color[].

*      IF RB1 = 'X'.
*        IF WA_SAIDA-AUGBL IS NOT INITIAL.
*          APPEND WA_SAIDA TO IT_SAIDA.
*          VFLAG = 1.
*        ENDIF.
*      ELSEIF RB2 = 'X'.
*        IF ( WA_SAIDA-WAERK = 'USD' AND XZ_VLR_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND XZ_VLR_INTERNA NE 0 ).
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.
*      ELSE.
**<<<------"152483 - NMS - INI------>>>
      wa_saida-tprel = gc_in.         "IN = Insumos
      wa_saida-tplin = sy-abcde+8(1). "I - Item
**<<<------"152483 - NMS - FIM------>>>
      APPEND wa_saida TO it_saida.
*      ENDIF.
      wa_saida-augbl     = ''.
      CLEAR: wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_saida, wa_bsad, wa_z0159, wa_zibchv.
    ENDLOOP.


*    CLEAR: WA_SAIDA-FORMA_PAG, WA_SAIDA-TAXA, WA_SAIDA-DOCNUM, WA_SAIDA-AUGBL, WA_SAIDA-COLOR_CELL[].
    REFRESH it_color.

*    LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<F_SAIDA>).
**    <F_SAIDA>-RFMNG = REDUCE RFMNG( INIT X TYPE RFMNG FOR LS IN IT_REMESSA WHERE ( VBELV EQ <F_SAIDA>-VBELN_G ) NEXT X = X + LS-RFMNG ).
*    ENDLOOP.

* ATUALIZA saldo da OV.
    saldo_ov_brl = i_saida-totvl_ov.
    saldo_ov_dol = i_saida-totvl_ov.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>) WHERE vbeln EQ i_saida-vbeln.
*      IF saldo_ov_brl IS NOT INITIAL.
      saldo_ov_brl  = ( saldo_ov_brl - <f_saida>-dmbtr ).
*      ENDIF.

*      IF saldo_ov_dol IS NOT INITIAL.
      saldo_ov_dol = ( saldo_ov_dol - <f_saida>-dmbe2 ).
*      ENDIF.
    ENDLOOP.

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<w_saida>) WITH KEY vbeln = i_saida-vbeln.
    IF sy-subrc EQ 0.
      IF <w_saida>-waerk <> 'BRL'.
        <w_saida>-moeda_forte = saldo_ov_dol.
      ELSE.
        <w_saida>-moeda_inter = saldo_ov_brl.
      ENDIF.
    ENDIF.
    CLEAR: saldo_ov_brl, saldo_ov_dol, wa_saida.

*    CHECK RB2 EQ ABAP_TRUE.
*
*    LOOP AT IT_SAIDA INTO WA_SAIDA.
*      WA_DELETE =
*      VALUE #(
*               VBELN_S     = WA_SAIDA-VBELN_S
*               VBELN_P     = WA_SAIDA-VBELN_P
*               MOEDA_FORTE = WA_SAIDA-MOEDA_FORTE
*               MOEDA_INTER = WA_SAIDA-MOEDA_INTER
*             ).
*
*      COLLECT WA_DELETE INTO IT_DELETE.
*      CLEAR WA_DELETE.
*
*    ENDLOOP.
*
*    DELETE IT_DELETE WHERE MOEDA_FORTE > 10 OR  MOEDA_INTER > 10.
*
*    LOOP AT IT_DELETE INTO WA_DELETE.
*      DELETE IT_SAIDA WHERE VBELN_S EQ WA_DELETE-VBELN_S AND
*                            VBELN_P EQ WA_DELETE-VBELN_P.
*    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


********************************************************************

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
**<<<------"152483 - NMS - INI------>>>
* Veridica se o processamento é background.
  IF NOT sy-batch IS INITIAL.
    SELECT SINGLE low
    FROM tvarvc
    INTO @DATA(lv_jobname)
    WHERE name EQ 'ZSD_CARGA_TBL_CONTAS_RECEBER'.

    SELECT SINGLE COUNT(*)
      FROM tbtco
      INTO @DATA(gv_job)
    WHERE jobname EQ @lv_jobname
      AND status  EQ @sy-abcde+17(1). "R - Running

    IF gv_job EQ 1.
      DATA(gv_extract) = abap_on.

    ELSE.
* &2 job atualmente em execução.
      MESSAGE s001(finoc) WITH lv_jobname.
      EXIT.

    ENDIF.

    CLEAR gv_job.

  ENDIF.

  IF rb8 IS NOT INITIAL AND ( rb10 IS NOT INITIAL OR rb11 IS NOT INITIAL ).
    MESSAGE s000(z01) WITH 'Tipo Convencional/E-commerce' 'deve ser utilizado somente para Insumos' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
**<<<------"152483 - NMS - FIM------>>>
  IF p_bukrs IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório o Preenchimento do Campo Empresa!'.
    STOP.
  ENDIF.

  IF p_pgto IS NOT INITIAL.
    IF p_venc IS NOT INITIAL.
*    IF p_erdat IS NOT INITIAL OR p_venc IS NOT INITIAL.
*      MESSAGE i000(z01) WITH 'Não é permitido preencher data venc ou data criação OV!'.
      MESSAGE 'Deve ser informado apenas uma das Datas (Vencimento ou Pagamento)!' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

*  IF P_VENC IS INITIAL AND P_ERDAT IS INITIAL AND P_VBELN IS INITIAL.
*    MESSAGE I000(Z01) WITH 'Informe o vencimento ou data criação.'.
*    STOP.
*  ENDIF.

*  IF P_VENC IS NOT INITIAL AND P_VBELN IS NOT INITIAL.
*    MESSAGE 'Ao informar vencimento não pode ser informado o numero da ordem!' TYPE 'I'.
*    STOP.
*  ENDIF.

  IF p_docsi IS INITIAL AND p_vbeln IS INITIAL.

    IF p_pgto  IS INITIAL AND
       p_venc  IS INITIAL AND
       p_erdat IS INITIAL.

      MESSAGE 'Para essa busca é obrigatório informar pelo menos umas das datas do filtro!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM f_variant.

  CASE abap_true.
    WHEN rb8.

      DATA lv_fat_vbeln TYPE vbeln_vf.
      DATA: gt_docs_finan      TYPE TABLE OF zi_mi_ov_fat_principal,
            gt_ovs             TYPE TABLE OF zi_mi_ov_principal_3,
*** Início - Rubenilson Pereira - 25.08.2025 #175147
            lv_saldo_brl       TYPE zi_mi_ov_fat_principal-saldojuros,
            lv_saldo_usd       TYPE zi_mi_ov_fat_principal-saldojuros,
            lv_saldo_juros_brl TYPE zi_mi_ov_fat_principal-saldojuros,
            lv_saldo_juros_usd TYPE zi_mi_ov_fat_principal-saldojuros.
*** Início - Rubenilson Pereira - 25.08.2025 #175147

      PERFORM:f_iniciar_variaves. " Cabeçalho

      CLEAR: it_saida.

      "Busca primeiro pelos dados de data
      IF ( p_pgto IS NOT INITIAL OR p_venc IS NOT INITIAL ).

        "AND p_vbeln IS INITIAL AND p_docsi IS INITIAL. " 17.02.2025 - RAMON, buscar pela data independente do simulador ou OV

        CLEAR lr_vbeln[].

        IF p_vbeln IS NOT INITIAL.

          SELECT 'I' AS sign, 'EQ' AS option, vbeln2 AS low FROM zi_mi_filtro_zsdt0053
            INTO TABLE @lr_vbeln
              WHERE vbeln IN @p_vbeln.

        ENDIF.

        " seleciona lançamentos dentro da data de vencimento
        SELECT DISTINCT ordem
          FROM zi_mi_ov_fat_principal
          WHERE dtpagto IN @p_pgto
            AND dtvenc IN @p_venc
            AND ordem IN @lr_vbeln "@p_vbeln. 06.08.2025
            INTO TABLE @DATA(gt_ov_venci).
        " seleciona ovs dentro da data de vencimento
**        SELECT DISTINCT ordem
**          FROM ZI_MI_OV_PRINCIPAL_3
**          APPENDING TABLE @gt_ov_venci
**          WHERE data_venc IN @p_venc
**            AND ordem IN @p_vbeln
**            and NumDocs = 0.

        SELECT DISTINCT ordem
          FROM zi_mi_ov_principal_3
          WHERE data_venc IN @p_venc
            AND ordemderivada IN @lr_vbeln "@p_vbeln. 06.08.2025
            AND numdocs = 0
            "AND simulador IS NOT INITIAL """"" teste 03.02.2025
      APPENDING TABLE @gt_ov_venci.

        IF gt_ov_venci IS NOT INITIAL.

          " 05.08.2025 -->
          SELECT DISTINCT ordem
            FROM zi_mi_ov_principal_3
            INTO TABLE @DATA(gt_ov_venci_aux)
            FOR ALL ENTRIES IN @gt_ov_venci
            WHERE ordemderivada = @gt_ov_venci-ordem.

          APPEND LINES OF gt_ov_venci_aux TO gt_ov_venci.

          " 05.08.2025 --<
          SELECT vbeln_va AS ordem FROM zi_mi_ov_vbkd_vbak
            FOR ALL ENTRIES IN @gt_ov_venci
              WHERE vbeln = @gt_ov_venci-ordem
                AND valdt IN @p_venc
            APPENDING TABLE @gt_ov_venci.

        ENDIF.

        " 25.02.2025 - RAMON -->

        SELECT vbeln AS ordem FROM zi_mi_ov_vbkd_vbak_2
            WHERE valdt IN @p_venc
              AND vbeln IN @lr_vbeln "@p_vbeln. 06.08.2025
              AND bukrs IN @p_bukrs
              AND kunnr IN @p_kunnr
              AND vkbur IN @p_vkbur
          APPENDING TABLE @gt_ov_venci.

        " 25.02.2025 - RAMON --<

        SORT gt_ov_venci BY ordem ASCENDING.
        DELETE ADJACENT DUPLICATES FROM gt_ov_venci COMPARING ordem.

        IF gt_ov_venci IS NOT INITIAL.

          SELECT *
            FROM zi_mi_ov_principal_3
            FOR ALL ENTRIES IN @gt_ov_venci
            WHERE empresa IN @p_bukrs
               AND cliente IN @p_kunnr
               AND escvendas IN @p_vkbur
               AND simulador IN @p_docsi
            " 25.02.2025 - RAMON -->
               AND ( ordem = @gt_ov_venci-ordem OR ordemderivada = @gt_ov_venci-ordem )
            AND datacriacao IN @p_erdat
            AND moeda IN @p_moeda
            AND vtweg IN @p_vtweg
            AND spart IN @p_spart
            AND tpov  IN @p_auart
             INTO TABLE @gt_ovs.

          DELETE ADJACENT DUPLICATES FROM gt_ovs COMPARING ALL FIELDS.

*****          " 06.08.2025 -->
*****          IF p_vbeln IS NOT INITIAL.
*****
*****            SELECT * FROM zi_mi_filtro_zsdt0053_p
*****              INTO TABLE @DATA(lt_vbeln)
*****                WHERE vbeln IN @p_vbeln.
*****
*****            LOOP AT gt_ovs ASSIGNING FIELD-SYMBOL(<fs_ovs1>).
*****
*****              READ TABLE lt_vbeln ASSIGNING FIELD-SYMBOL(<fs_vbeln>)
*****                WITH KEY vbeln = <fs_ovs1>-ordem.
*****
*****              IF sy-subrc NE 0.
*****
*****                DELETE gt_ovs WHERE ordem = <fs_ovs1>-ordem.
*****
*****              ENDIF.
*****
*****
*****            ENDLOOP.
*****
*****            "delete gt_ovs where
*****
*****          ENDIF.
*****          " 06.08.2025 --<

          IF gt_ovs IS NOT INITIAL.

            SELECT *
             FROM zi_mi_ov_fat_principal
             INTO TABLE @gt_docs_finan
             FOR ALL ENTRIES IN @gt_ovs
             WHERE ( ordem = @gt_ovs-ordem OR ordem = @gt_ovs-ordemderivada ).
          ENDIF.


        ENDIF.


        "Seleciona dados por dados da Ordem/Simulador
      ELSE.

        " 16.01.2025 - teste de performance -->

        IF p_vbeln IS NOT INITIAL.

          SELECT ordem,empresa,ordemfilha FROM zi_mi_ov_filha_fluxo
            INTO TABLE @DATA(lt_fluxo)
            WHERE empresa IN @p_bukrs
              AND ordemfilha IN @p_vbeln.

          " 04.08.2025 --->
          SELECT ordemfilha AS ordem,empresa,ordem AS ordemfilha FROM zi_mi_ov_filha_fluxo
            APPENDING TABLE @lt_fluxo
            WHERE empresa IN @p_bukrs
              AND ordem IN @p_vbeln.

          SORT lt_fluxo BY ordem empresa ordemfilha ASCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_fluxo COMPARING ALL FIELDS.
          " 04.08.2025 ---<

          IF lt_fluxo IS NOT INITIAL.

            SELECT *
              FROM zi_mi_ov_principal_3
              INTO TABLE @gt_ovs
                FOR ALL ENTRIES IN @lt_fluxo
                  WHERE empresa IN @p_bukrs
                     AND cliente IN @p_kunnr
                     AND eqvendas IN @p_vkbur
                     AND simulador IN @p_docsi
                     AND ordem = @lt_fluxo-ordem
                     AND datacriacao IN @p_erdat.

          ENDIF.

        ELSE.
          " 16.01.2025 - teste de performance --<

          SELECT *
              FROM zi_mi_ov_principal_3
              INTO TABLE @gt_ovs
              WHERE empresa IN @p_bukrs
                 AND cliente IN @p_kunnr
                 AND eqvendas IN @p_vkbur
                 AND simulador IN @p_docsi
                 AND ordem IN @p_vbeln
                 AND datacriacao IN @p_erdat.



        ENDIF.



        " 16.01.2025 - teste de performance --<


        IF sy-subrc = 0.

          SELECT *
          FROM zi_mi_ov_fat_principal "!zi_mi_ov_finan_uni
          INTO TABLE @gt_docs_finan
          FOR ALL ENTRIES IN @gt_ovs
          WHERE ordem = @gt_ovs-ordemderivada.

        ENDIF.

      ENDIF.

      IF gt_ovs IS NOT INITIAL AND gt_docs_finan IS INITIAL.

        SELECT *
          FROM zi_mi_ov_fat_principal "!zi_mi_ov_finan_uni
          INTO TABLE @gt_docs_finan
          FOR ALL ENTRIES IN @gt_ovs
          WHERE ordem = @gt_ovs-ordemderivada.


      ENDIF.

      " 20.01.2025 - RAMON - BUG 163213 -->

      IF rb3 IS INITIAL.

        IF gt_ovs IS NOT INITIAL.

          SELECT * FROM zi_mi_ov_saldo_2
            INTO TABLE @DATA(lt_saldo)
              FOR ALL ENTRIES IN @gt_ovs
                "WHERE simulador = @gt_ovs-simulador. ""18.02.2025
                WHERE ordem = @gt_ovs-ordem. ""18.02.2025

        ENDIF.

      ENDIF.

      CASE abap_true.

          " ----Recebido
        WHEN rb1.
*** Início - Rubenilson Pereira - 25.08.2025 #175147
*          LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>) WHERE saldoov > 10 OR saldojurosfiltro > 10.
*
*            DELETE gt_ovs WHERE ordem = <fs_saldo>-ordem.
*
*          ENDLOOP.

          "DELETE gt_ovs WHERE sem_saldo = abap_false.
          " ----Saldo
*** Fim - Rubenilson Pereira - 25.08.2025 #175147
        WHEN rb2.
*** Inicio - Rubenilson Pereira - 25.08.2025 #175147
          " se é para visualizar saldo, então retira as que tem saldo zerado, ou saldo de juros zerado ou saldo de juros negativo
*          LOOP AT lt_saldo ASSIGNING <fs_saldo>
*              WHERE ( ( saldoov BETWEEN 0 AND 10 ) OR saldoov < 0 )
*                AND ( ( saldojurosfiltro BETWEEN -10 AND 10 ) OR saldojurosfiltro < -10 ).
*
*            DELETE gt_ovs WHERE ordem = <fs_saldo>-ordem.
*
*          ENDLOOP.
*** Fim - Rubenilson Pereira - 25.08.2025 #175147
          " 25.02.2025 - 2 - RAMON -->

          DATA: lr_tp_descarte TYPE RANGE OF vbak-auart.

          SELECT *
            FROM setleaf
            INTO TABLE @DATA(lt_set)
           WHERE setname = 'Z_MI_OV_DESCARTAR'.
          IF sy-subrc IS INITIAL.
            lr_tp_descarte = VALUE #( FOR ls_set IN lt_set
                                      sign = 'I'
                                      option = 'EQ'
                                      ( low = ls_set-valfrom )
                                      ).

            IF lr_tp_descarte IS NOT INITIAL.
              DELETE gt_ovs WHERE tpov IN lr_tp_Descarte.
            ENDIF.
          ENDIF.

          LOOP AT gt_ovs ASSIGNING FIELD-SYMBOL(<fs_ovs>) WHERE tpdevvenda = 'D'.

            READ TABLE gt_ovs TRANSPORTING NO FIELDS
              WITH KEY ordem = <fs_ovs>-ordem
                       tpdevvenda = 'V'.

            CHECK sy-subrc NE 0.

            DELETE gt_ovs WHERE ordem = <fs_ovs>-ordem.

          ENDLOOP.


*          LOOP AT lt_saldo ASSIGNING <fs_saldo>.
*
*            READ TABLE gt_ovs ASSIGNING <fs_ov> WITH KEY ordem = <fs_saldo>-ordem.
*
*            CHECK sy-subrc eq 0.
*
*            data(lv_vbeln) = <fs_saldo>-ordem.
*
*            DELETE gt_ovs WHERE ordem = <fs_saldo>-ordem.
*
*
*          ENDLOOP.

          " 25.02.2025 - 2 - RAMON --<

          "DELETE gt_ovs WHERE sem_saldo = abap_true.

        WHEN rb3.

      ENDCASE.

      " 20.01.2025 - RAMON - BUG 163213 --<

      DATA lt_collect TYPE TABLE OF zsde_mi_simulador.
      DATA ls_collect TYPE zsde_mi_simulador.

      DATA(gt_ov_princ) = gt_ovs.
      SORT gt_ov_princ BY escvendas eqvendas ordem ordemderivada.

      "SORT: gt_docs_finan BY ordem.
      SORT: gt_docs_finan BY ordem faturamento doccont doccomp ASCENDING. " 14.01.2025 - RAMON - 163213

      LOOP AT gt_ov_princ ASSIGNING FIELD-SYMBOL(<fs_ov>).

        APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

        CLEAR ls_collect.
        ls_collect-simulador = <fs_ov>-simulador. " 19.02.2025

        <fs_saida>-bukrs = <fs_ov>-empresa.
        <fs_saida>-vkgrp = <fs_ov>-eqvendas.

        " 09.12.2024 - RAMON - 160480 -->
        <fs_saida>-bezei = <fs_ov>-eqvendas_txt.
        " 09.12.2024 - RAMON - 160480 --<

        <fs_saida>-vkbur = <fs_ov>-escvendas.
        <fs_saida>-auart = <fs_ov>-tpov.
        <fs_saida>-safra = <fs_ov>-safra.
        <fs_saida>-pgto_ant = <fs_ov>-pgto_ant.
        <fs_saida>-tx_juros = <fs_ov>-tx_juros.
        <fs_saida>-tx_multa = <fs_ov>-tx_multa.
        <fs_saida>-kunnr = <fs_ov>-cliente.
        <fs_saida>-name1 = <fs_ov>-nomecliente.
        <fs_saida>-zterm = <fs_ov>-condpagto.
        <fs_saida>-text1 = <fs_ov>-desccondpagto.
        <fs_saida>-vbeln_s = <fs_ov>-simulador.
        <fs_saida>-vbeln_p = <fs_ov>-ordem.
        <fs_saida>-vbeln_g = <fs_ov>-ordemderivada.
        <fs_saida>-erdat = <fs_ov>-datacriacao.
        <fs_saida>-waerk = <fs_ov>-moeda.
        <fs_saida>-status = <fs_ov>-status.

        "04.02.2025 - RAMON - Chamada tela zfis26 -->
        <fs_saida>-vbeln = <fs_ov>-ordem.
        "04.02.2025 - RAMON - Chamada tela zfis26 --<

        <fs_saida>-totalq_ov = <fs_ov>-qtd.
        <fs_saida>-rfmng = <fs_ov>-qtdfat.
        <fs_saida>-netwr_l = <fs_ov>-valorliq.
        <fs_saida>-mwsbp = <fs_ov>-valorimp.
        <fs_saida>-totvl_ov = <fs_ov>-valortotal.

        "Campos Saldo OV
        " 15.01.2025 - RAMON - 163213 -->
        <fs_saida>-moeda_inter = <fs_ov>-saldoov.
        <fs_saida>-moeda_forte = <fs_ov>-saldoovusd.

        " 27.01.2025
*        <fs_saida>-moeda_inter = <fs_ov>-saldorefbrl.
*        <fs_saida>-moeda_forte = <fs_ov>-saldorefusd.

        " 15.01.2025 - RAMON - 163213 --<

        <fs_saida>-line_color = 'C310'.

        " 05.08.2025 -->
        <fs_saida>-sald_referencia = <fs_ov>-sald_referencia.
        " 05.08.2025 --<

        " 25.02.2025 ---> data de vencimento da ov principal -->
*      IF p_venc IS NOT INITIAL.
*
*        IF <fs_ov>-data_venc IN p_venc.
*          ls_collect-docs_in = 1.
*        ENDIF.
*      ENDIF.
        " 25.02.2025 ---> data de vencimento da ov principal --<


        t_cor =
              VALUE #(
                       "Vermelho
                       ( fname = 'MOEDA_FORTE'  color-col = '6' color-int = '1' color-inv = '1' )
                       ( fname = 'MOEDA_INTER'  color-col = '6' color-int = '1' color-inv = '1' )
                       ( fname = 'SALD_REFERENCIA' color-col = '6' color-int = '1' color-inv = '1' )
                       ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                       ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
                        ).
        APPEND LINES OF t_cor TO <fs_saida>-color_cell.
        FREE  t_cor.


        READ TABLE gt_docs_finan TRANSPORTING NO FIELDS
                            WITH KEY ordem = <fs_ov>-ordemderivada.
        "BINARY SEARCH.
        IF sy-subrc = 0.

          LOOP AT gt_docs_finan ASSIGNING FIELD-SYMBOL(<fs_doc>) FROM sy-tabix.

            IF <fs_doc>-ordem <> <fs_ov>-ordemderivada.
              EXIT.
            ENDIF.

            " 19.02.2025
            IF p_venc IS NOT INITIAL.

              IF <fs_doc>-dtvenc IN p_venc.
                ls_collect-docs_in = 1.
              ELSE.

                " 25.02.2025 -->
                IF <fs_ov>-data_venc IN p_venc AND <fs_ov>-data_venc IS NOT INITIAL.
                  ls_collect-docs_in = 1.
                ENDIF.
                " 25.02.2025 --<

                ls_collect-docs_out = 1.

              ENDIF.

            ENDIF.

            APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_doc>).

            " id do lançamento zfit0026
            <fs_saida_doc>-zid_lanc = <fs_doc>-id26.

            "Dados da OV header
            <fs_saida_doc>-vbeln_s = <fs_ov>-simulador.
            <fs_saida_doc>-vbeln_p = <fs_ov>-ordem.
            <fs_saida_doc>-vbeln_g = <fs_ov>-ordemderivada.
            <fs_saida_doc>-waerk = <fs_saida>-waerk.

            "04.02.2025 - RAMON - Chamada tela zfis26 -->
            <fs_saida_doc>-vbeln =  <fs_ov>-ordemderivada.
            "04.02.2025 - RAMON - Chamada tela zfis26 --<

            " Referencia
            <fs_saida_doc>-referencia_nfe = <fs_doc>-nfe.


***            IF lv_fat_vbeln NE <fs_doc>-faturamento.

            " Quantidade de Faturado NFe
            <fs_saida_doc>-fkimg = <fs_doc>-qtdfatref.

            " Vlr.Total Ref
            <fs_saida_doc>-vlr_tot_ref = <fs_doc>-vlrtotref.

            " Saldo da referencia
            <fs_saida_doc>-sald_referencia = <fs_doc>-saldoref.

****            ELSE.
****
****              IF lv_fat_vbeln IS NOT INITIAL.
****
****                IF <fs_saida_doc>-waerk = 'USD'.
****                  SUBTRACT <fs_ov>-saldorefusd FROM <fs_saida>-moeda_forte.
****                ENDIF.
****
****                IF <fs_saida_doc>-waerk = 'BRL'.
****                  SUBTRACT <fs_ov>-saldorefbrl FROM <fs_saida>-moeda_inter.
****                ENDIF.
****
****              ENDIF.
****
****            ENDIF.

            " data Vencimento
            <fs_saida_doc>-data_venc = <fs_doc>-dtvenc.

            " Forma de pagamento
            <fs_saida_doc>-budat = <fs_doc>-dtpagto.

            " Taxa (Ptax)
            <fs_saida_doc>-taxa = <fs_doc>-ptax.
            <fs_saida_doc>-forma_pag = <fs_doc>-forpagto.

            " N. Doc Contabil
            <fs_saida_doc>-docnum = <fs_doc>-doccont.

            " Doc.Compens
            <fs_saida_doc>-augbl = <fs_doc>-doccomp.

            " Vlr.Comp.US$ / Vlr Recebido US$
            <fs_saida_doc>-dmbe2 = <fs_doc>-valorrecusd.

            " Vlr.Comp.R$ / Vlr Recebido R$
            <fs_saida_doc>-dmbtr = <fs_doc>-valorrec.

            " Multa Calculado
            <fs_saida_doc>-vlr_multa_calc = <fs_doc>-vlr_multa_calc.
            " Multa Recebido
            <fs_saida_doc>-vlr_multa_rbdo = <fs_doc>-vlr_multa_rbdo.
            " Multa Calculado
            <fs_saida_doc>-vlr_desc_mult = <fs_doc>-vlr_desc_mult.
            " Juros Calculado
            <fs_saida_doc>-vlr_juros_calc = <fs_doc>-vlr_juros_calc.
            " Juros Recebido
            <fs_saida_doc>-vlr_juros_rbdo = <fs_doc>-vlr_juros_rbdo.
            " Multa Recebido
            <fs_saida_doc>-vlr_desc_jros = <fs_doc>-vlr_desc_jros.

            " Saldo Juros USD
            <fs_saida_doc>-vlr_sald_fin = <fs_doc>-saldojurosusd.

            "Saldo Juros R$
            <fs_saida_doc>-vlr_sald_fin_brl = <fs_doc>-saldojuros.

            " Saldo US$
*            <fs_saida_doc>-MOEDA_FORTE = 0.

            " Saldo R$
*            <fs_saida_doc>-MOEDA_INTER = 0.

            " Observação
            <fs_saida_doc>-observacao = <fs_doc>-observacao.

            IF <fs_doc>-id26 IS NOT INITIAL.

              t_cor = VALUE #(
                           "Verde
                           ( fname = 'AUGBL'          color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'DOCNUM'         color-col = '5' color-int = '1' color-inv = '1' )
                           "( fname = 'DATA_VENC'      color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'BUDAT'          color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'DMBE2'          color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'DMBTR'          color-col = '5' color-int = '1' color-inv = '1' )
                          ( fname = 'FORMA_PAG'      color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'TAXA'           color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'MONT_MOEDA'     color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'MONT_MI'        color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'VLR_MULTA_CALC' color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'VLR_MULTA_RBDO' color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'VLR_JUROS_CALC' color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'VLR_JUROS_RBDO' color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'VLR_DESC_MULT'  color-col = '5' color-int = '1' color-inv = '1' )
                           ( fname = 'VLR_DESC_JROS'  color-col = '5' color-int = '1' color-inv = '1' ) ).

              APPEND LINES OF t_cor TO <fs_saida_doc>-color_cell.
              CLEAR t_cor.

            ENDIF.

            t_cor = VALUE #(
                   "Vermelho
                   ( fname = 'MOEDA_FORTE'  color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'MOEDA_INTER'  color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'SALD_REFERENCIA' color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
                    ).

            APPEND LINES OF t_cor TO <fs_saida_doc>-color_cell.
            FREE  t_cor.

            lv_fat_vbeln = <fs_doc>-faturamento.

            COLLECT ls_collect INTO lt_collect. " 19.02.2025

          ENDLOOP.

        ENDIF.

      ENDLOOP.

*** Início - Rubenilson Pereira - 25.08.2025 #175147
      DATA(lt_simu) = it_saida.
      SORT lt_simu BY vbeln_p.

      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_select>).

        CLEAR: lv_saldo_brl,
               lv_saldo_usd,
               lv_saldo_juros_brl,
               lv_saldo_juros_usd.

        READ TABLE lt_simu TRANSPORTING NO FIELDS
        WITH KEY vbeln_p = <fs_select>-vbeln_p
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>) FROM sy-tabix.
            IF <fs_simu>-vbeln_p <> <fs_select>-vbeln_p.
              EXIT.
            ENDIF.

            lv_saldo_brl = lv_saldo_brl + <fs_simu>-moeda_inter.
            lv_saldo_usd = lv_saldo_usd + <fs_simu>-moeda_forte.

            lv_saldo_juros_brl = lv_saldo_juros_brl + <fs_simu>-vlr_sald_fin_brl.
            lv_saldo_juros_usd = lv_saldo_juros_usd + <fs_simu>-vlr_sald_fin.
          ENDLOOP.

        ENDIF.

        CASE abap_true.

          WHEN rb1.

            IF     ( lv_saldo_brl > 10 ) AND
                   ( lv_saldo_usd > 10 )  AND
                   ( lv_saldo_juros_brl > 10 ) AND
                   ( lv_saldo_juros_usd > 10 ) .
              DELETE it_saida WHERE vbeln_p = <fs_select>-vbeln_p.
            ENDIF.

          WHEN rb2.

            IF ( ( ( lv_saldo_brl BETWEEN 0 AND 10 ) OR
               ( lv_saldo_brl < 0 ) ) AND
             ( ( lv_saldo_usd BETWEEN 0 AND 10 ) OR
               ( lv_saldo_usd < 0 ) ) ) AND
             ( ( lv_saldo_juros_brl BETWEEN -10 AND 10 ) OR
               ( lv_saldo_juros_brl < -10 ) ) AND
             ( ( lv_saldo_juros_usd BETWEEN -10 AND 10 ) OR
               ( lv_saldo_juros_usd < -10 ) ).
              DELETE it_saida WHERE vbeln_p = <fs_select>-vbeln_p.
            ENDIF.

          WHEN rb3.
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.
*** Fim - Rubenilson Pereira - 25.08.2025 #175147

      " 19.02.2025
      IF p_venc IS NOT INITIAL.

        LOOP AT lt_collect INTO ls_collect WHERE docs_in = 0.

          DELETE it_saida WHERE vbeln_s = ls_collect-simulador.

        ENDLOOP.

      ENDIF.



      " 03.02.2025 -->
      LOOP AT it_saida ASSIGNING <fs_saida> WHERE vbeln_s = '0000000000'.
        CLEAR <fs_saida>-vbeln_s.
        CLEAR <fs_saida>-vbeln_p.
      ENDLOOP.
      " 03.02.2025 --<


      PERFORM: f_classificacao,
               f_build_layout,
               f_imprime_dados.

    WHEN rb7.

*      IF sy-uname NE 'RBLIMA'.
*
*        MESSAGE 'Visão insumos em contrução' TYPE 'I' DISPLAY LIKE 'E'.
*        EXIT.
*
*
*      ELSE.

      PERFORM f_iniciar_variaves.

      PERFORM f_visao_insumos_select.

      PERFORM f_classificacao.
      PERFORM f_build_layout.
      PERFORM f_imprime_dados.

      EXIT.

*      ENDIF.

      PERFORM:
*            F_SHDB,
      f_iniciar_variaves, " Cabeçalho
      f_seleciona_dados. " Form seleciona dados Insumos
*          F_SAIDA. " Form de saida

      DATA(t_saida) = it_saida.

*********************"Selecionando dados da OV*************************************************
      dados_insumo=>selec_dados_ov( ).


      IF it_saida IS NOT INITIAL.  "Verifica se os dados da OV foi carregado.
        FREE: t_saida, lt_saida.
        t_saida = it_saida.
        lt_saida = it_saida.

        FREE: it_saida.

        SORT lt_saida BY vbeln_p.
        DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING vbeln_p.

****        Verificar saldo da OV.
        LOOP AT lt_saida ASSIGNING FIELD-SYMBOL(<ls_saida>).
          LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<l_saida>) WHERE vbeln_p EQ <ls_saida>-vbeln_p.
            zcl_dados_ov=>i_vlr_ov(
              EXPORTING
                i_vbeln       = <l_saida>-vbeln    " Documento de vendas e distribuição precedente
              IMPORTING
*               E_VLR_TOTAL   = DATA(VLR_SALDO_TOTAL)   " Valor líquido na moeda do documento
                e_vlr_parcial = vlr_saldo_parcial " Valor líquido na moeda do documento
            ).
            ADD vlr_saldo_parcial TO tot_saldo.
            CLEAR: vlr_saldo_parcial.

            zcl_dados_ov=>i_vlr_saldo_financ(
              EXPORTING
                i_vbeln     = <l_saida>-vbeln    " Documento de faturamento
              IMPORTING
                e_vlr_total = vlr_saldo_finc   " Valor líquido na moeda do documento
            ).

            ADD vlr_saldo_finc TO tot_saldo_fin.
            CLEAR: vlr_saldo_finc.
          ENDLOOP.

          IF tot_saldo EQ 0 OR tot_saldo < 0 OR tot_saldo =< 10.
            IF tot_saldo_fin EQ 0 OR tot_saldo_fin < 0 OR tot_saldo_fin =< 10.
              <ls_saida>-ind_rec_total  = abap_true.
            ENDIF.
          ENDIF.
          CLEAR: tot_saldo, tot_saldo_fin, vlr_saldo_parcial, vlr_saldo_finc.
        ENDLOOP.

        SORT t_saida BY ind_rec_total.

*        IF rb1 IS NOT INITIAL.
*          DELETE t_saida WHERE ind_rec_total EQ abap_false.
*        ELSEIF rb2 IS NOT INITIAL.
*          DELETE t_saida WHERE ind_rec_total EQ abap_true.
*        ENDIF.

        SORT t_saida BY vbeln_p.

**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
*   Verifca a condição se OV ou Fatura
        SELECT dk~vbeln, t~zdart
         FROM vbkd AS dk
         INNER JOIN vbak AS ak ON ak~vbeln = dk~vbeln
         INNER JOIN t052 AS t  ON t~zterm  = dk~zterm
         INTO TABLE @DATA(lt_zdart)
         FOR ALL ENTRIES IN @t_saida
        WHERE dk~vbeln = @t_saida-vbeln.

        IF sy-subrc = 0.
          SORT lt_zdart BY vbeln.
        ENDIF.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************

        LOOP AT t_saida INTO DATA(w_saida).
          READ TABLE lt_saida INTO DATA(ls_saida) WITH KEY vbeln_p = w_saida-vbeln_p.
          IF sy-subrc EQ 0.
            IF rb1 IS NOT INITIAL. "Recebido.
              IF ls_saida-ind_rec_total EQ abap_true.
              ELSE.
                CONTINUE.
              ENDIF.
            ELSEIF rb2 IS NOT INITIAL. "Saldo.
              IF ls_saida-ind_rec_total EQ abap_true. "25/05/2021
                CONTINUE.
              ELSE.

              ENDIF.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
**<<<------"152483 - NMS - INI------>>>
          w_saida-tplin = sy-abcde+7(1). "H - Header
**<<<------"152483 - NMS - FIM------>>>
          APPEND w_saida TO it_saida.

****************"Vericando a codição de pagamento da OV.**************************************
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
***          dados_merc_interno=>chkec_cond_ov( EXPORTING i_saida = w_saida
***                                             IMPORTING e_zdart = DATA(z_zdart) ).

          READ TABLE lt_zdart ASSIGNING FIELD-SYMBOL(<fs_zdart>)
                      WITH KEY vbeln = w_saida-vbeln
                      BINARY SEARCH.
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
          IF sy-subrc = 0.
            IF <fs_zdart>-zdart EQ 'B'.
****************Selecionando recebimento com base na faturas da OV****************************************************
              dados_insumo=>selec_dados_fatura( EXPORTING i_saida = w_saida ).
            ELSE.
****************"Selecionando recebimento com base na OV*************************************
              dados_insumo=>check_recebimentos( EXPORTING i_saida = w_saida ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
**<<<------"152483 - NMS - INI------>>>
      IF gv_extract IS INITIAL.
**<<<------"152483 - NMS - FIM------>>>
        PERFORM:  f_classificacao,
                  f_build_layout,
                  f_imprime_dados.
**<<<------"152483 - NMS - INI------>>>
      ELSE.
* Prepara dados para exportar para a tabela.
        PERFORM zf_export_data_to_table USING gc_in. "IN = Insumos

      ENDIF.
**<<<------"152483 - NMS - FIM------>>>
  ENDCASE.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves .
  DATA:
    w_texto1(10),
    w_texto2(10),
    w_texto3(60),

    w_empresa_texto(40),
    w_tipo_texto(40),
    w_esc_texto(40),
    w_cli_texto(40),
    w_ov_texto(40),
    w_dtcri_texto(40),
    w_dtve_texto(40),

    empresa             TYPE c LENGTH 50,
    tipo                TYPE c LENGTH 50,
    escritorio          TYPE c LENGTH 50,
    cliente             TYPE c LENGTH 50,
    ordem               TYPE c LENGTH 50,
    dtcriacao           TYPE c LENGTH 200,
    dtvencto            TYPE c LENGTH 200,
    dtpgto              TYPE c LENGTH 200.

  FREE: t_top.
*  CLEAR: LS_LINE.
  v_report = sy-repid.

  IF rb7 IS NOT INITIAL.
    w_texto3 = 'Relatório de Contas a Receber Insumo'.
    "PERFORM f_construir_cabecalho USING 'H' w_texto3.
  ELSEIF rb8 IS NOT INITIAL.
    w_texto3 = 'Relatório de Contas a Receber Mercado Interno'.
    "PERFORM f_construir_cabecalho USING 'H' w_texto3.
  ENDIF.

  " ramon -->
  "IF sy-uname = 'RBLIMA'.
*  w_texto3 = w_texto3 && '( novo ) '. *** Início - SMC - 26.08.2025 #175147 - COMENTADO PARA SUBIR PARA PRD. NAO PRECISA MAIS IDENTIFICAR COMO NOVO.
  "ENDIF.

  PERFORM f_construir_cabecalho USING 'H' w_texto3.

  "ramon <----

  IF p_bukrs IS NOT INITIAL.
    w_empresa_texto = 'Empresa    :'.
    IF ( p_bukrs-low IS NOT INITIAL ) AND ( p_bukrs-high IS NOT INITIAL ).
      CONCATENATE w_empresa_texto p_bukrs-low 'á' p_bukrs-high INTO empresa SEPARATED BY space.
    ELSEIF ( p_bukrs-low IS NOT INITIAL ).
      CONCATENATE w_empresa_texto p_bukrs-low  INTO empresa SEPARATED BY space.
    ELSE.
      CONCATENATE w_empresa_texto 'Todas'  INTO empresa SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' empresa.
  ENDIF.

*  IF P_AUART IS NOT INITIAL.
*    W_TIPO_TEXTO = 'Tipo de O.V:'.
*    IF ( P_AUART-LOW IS NOT INITIAL ) AND ( P_AUART-HIGH IS NOT INITIAL ).
*      CONCATENATE W_TIPO_TEXTO  P_AUART-LOW 'á' P_AUART-HIGH INTO TIPO SEPARATED BY SPACE.
*    ELSEIF ( P_AUART-LOW IS NOT INITIAL ).
*      CONCATENATE W_TIPO_TEXTO P_AUART-LOW  INTO TIPO SEPARATED BY SPACE.
*    ELSE.
*      CONCATENATE W_TIPO_TEXTO 'Todas'  INTO TIPO SEPARATED BY SPACE.
*    ENDIF.
*    PERFORM F_CONSTRUIR_CABECALHO USING 'S' TIPO.
*  ENDIF.

  IF p_vkbur IS NOT INITIAL.
    w_esc_texto = 'Escr.Vendas:'.
    IF ( p_vkbur-low IS NOT INITIAL ) AND ( p_vkbur-high IS NOT INITIAL ).
      CONCATENATE w_esc_texto  p_vkbur-low 'á' p_vkbur-high INTO escritorio SEPARATED BY space.
    ELSEIF ( p_vkbur-low IS NOT INITIAL ).
      CONCATENATE w_esc_texto p_vkbur-low  INTO escritorio SEPARATED BY space.
    ELSE.
      CONCATENATE w_esc_texto 'Todas'  INTO escritorio SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' escritorio.
  ENDIF.

  IF p_kunnr IS NOT INITIAL.
    w_cli_texto = 'Cliente    :'.
    IF ( p_kunnr-low IS NOT INITIAL ) AND ( p_kunnr-high IS NOT INITIAL ).
      CONCATENATE w_cli_texto  p_kunnr-low 'á' p_kunnr-high INTO cliente SEPARATED BY space.
    ELSEIF ( p_kunnr-low IS NOT INITIAL ).
      CONCATENATE w_cli_texto p_kunnr-low  INTO cliente SEPARATED BY space.
    ELSE.
      CONCATENATE w_cli_texto 'Todas'  INTO cliente SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' cliente.
  ENDIF.

  IF p_vbeln IS NOT INITIAL.
    w_ov_texto = 'Ordem      :'.
    IF ( p_vbeln-low IS NOT INITIAL ) AND ( p_vbeln-high IS NOT INITIAL ).
      CONCATENATE w_ov_texto  p_vbeln-low 'á' p_vbeln-high INTO cliente SEPARATED BY space.
    ELSEIF ( p_vbeln-low IS NOT INITIAL ).
      CONCATENATE w_ov_texto p_vbeln-low  INTO ordem SEPARATED BY space.
    ELSE.
      CONCATENATE w_ov_texto  'Todas'  INTO ordem SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' ordem.
  ENDIF.

  IF ( NOT  p_erdat IS INITIAL ).
    w_dtcri_texto = 'Dt.Criação :'.
    CONCATENATE p_erdat-low+6(2)   '.' p_erdat-low+4(2)  '.' p_erdat-low(4)  INTO w_texto1.
    CONCATENATE p_erdat-high+6(2)  '.' p_erdat-high+4(2) '.' p_erdat-high(4) INTO w_texto2.
    CONCATENATE w_dtcri_texto w_texto1 ' - ' w_texto2 INTO dtcriacao  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' dtcriacao.
  ENDIF.

  IF ( NOT  p_venc  IS INITIAL ).
    w_dtve_texto = 'Dt.Vencto  :'.
    CONCATENATE p_venc-low+6(2)   '.' p_venc-low+4(2)  '.' p_venc-low(4)  INTO w_texto1.
    CONCATENATE p_venc-high+6(2)  '.' p_venc-high+4(2) '.' p_venc-high(4) INTO w_texto2.
    CONCATENATE w_dtve_texto w_texto1 ' - ' w_texto2 INTO dtvencto  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' dtvencto.
  ENDIF.

  IF ( NOT  p_pgto  IS INITIAL ).
    w_dtve_texto = 'Dt.pagamento  :'.
    CONCATENATE p_pgto-low+6(2)   '.' p_pgto-low+4(2)  '.' p_pgto-low(4)  INTO w_texto1.
    CONCATENATE p_pgto-high+6(2)  '.' p_pgto-high+4(2) '.' p_pgto-high(4) INTO w_texto2.
    CONCATENATE w_dtve_texto w_texto1 ' - ' w_texto2 INTO dtpgto  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' dtpgto.
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_construir_cabecalho    USING typ text.


  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.


ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

***  DATA: r_docnum TYPE RANGE OF j_1bdocnum.

  DATA: c_waerk TYPE RANGE OF vbak-waerk WITH HEADER LINE.
  DATA: r_belnr TYPE RANGE OF belnr_d.
  DATA: r_belnr_aux TYPE RANGE OF belnr_d.
  DATA: r_bukrs TYPE RANGE OF bukrs.
  DATA: r_kunnr TYPE RANGE OF kunnr.


  FREE:c_waerk, it_vbfa, it_vbkd, it_vbak, it_vbel, it_0041, it_0040,
       it_0090, it_zfit0026, it_vbkd, it_vbap, it_kna1, it_t052u, it_bsad,
       it_zsdt0038[], it_zsdt0038.

  c_waerk-sign   = 'I'.
  c_waerk-option = 'EQ'.

*  CASE ABAP_TRUE.
*    WHEN RB4.
*      C_WAERK-LOW    = 'USD'.
*      C_WAERK-HIGH   = 'USD'.
*      APPEND C_WAERK.
*    WHEN RB5.
*      C_WAERK-LOW    = 'BRL'.
*      C_WAERK-HIGH   = 'BRL'.
*      APPEND C_WAERK.
*  ENDCASE.

  DATA(obj_auart) = NEW zcl_taxa_curva( ).
  DATA(set_in) = obj_auart->get_auart( 'ZFIS26_SA_IN' ).

*    CASE ABAP_TRUE.
*      WHEN RB8.
*        SET_IN_MI = OBJ_AUART->GET_AUART( 'ZFIS26_SA_MI' ).
*    ENDCASE.

  IF p_venc IS NOT INITIAL.

    SELECT a~vbeln a~zterm a~kurrf a~valdt
        FROM vbkd AS a
        INNER JOIN vbak AS b ON a~vbeln	=	b~vbeln
        INTO TABLE it_vbkd
        WHERE a~valdt	IN p_venc
          "AND A~ZTERM NE 'I006'
          AND b~erdat IN p_erdat
          AND b~bukrs_vf IN p_bukrs
*          AND b~auart IN p_auart
          AND b~spart IN set_in
          AND b~vbeln IN p_vbeln
          AND b~vkbur IN p_vkbur
          AND b~kunnr IN p_kunnr
          AND b~waerk IN c_waerk.


    CHECK it_vbkd[] IS NOT INITIAL.

    IF p_docsi IS NOT INITIAL.
      SELECT  vbeln erdat auart vkbur kunnr waerk vkorg vkgrp
        FROM vbak
        INTO TABLE it_vbak
        FOR ALL ENTRIES IN it_vbkd
        WHERE vbeln	=	it_vbkd-vbeln
          AND erdat IN p_erdat
          AND bukrs_vf IN p_bukrs
          "AND AUART IN P_AUART
          AND spart IN set_in
          AND vkbur IN p_vkbur
          AND kunnr IN p_kunnr
          AND waerk IN c_waerk
          AND ( EXISTS ( SELECT *
                      FROM zsdt0041
                     WHERE vbeln EQ vbak~vbeln
                       AND doc_simulacao IN p_docsi ) OR
           EXISTS ( SELECT *
                      FROM zsdt0090
                     WHERE vbeln EQ vbak~vbeln
                       AND doc_simulacao IN p_docsi ) ).
    ELSE.
      SELECT  vbeln erdat auart vkbur kunnr waerk vkorg vkgrp
        FROM vbak
        INTO TABLE it_vbak
        FOR ALL ENTRIES IN it_vbkd
        WHERE vbeln	=	it_vbkd-vbeln
          AND erdat IN p_erdat
          AND bukrs_vf IN p_bukrs
          "AND AUART IN P_AUART
          AND spart IN set_in
          AND vkbur IN p_vkbur
          AND kunnr IN p_kunnr
          AND waerk IN c_waerk.
    ENDIF.


  ELSE.
    "Verifica se pesquisa por data de pgamento foi preenchida.
    "==============================================INICIO CS2020001117 - AO
    FREE: t_bsad.

    IF p_pgto IS NOT INITIAL.

      SELECT * FROM bsad AS a
        INTO CORRESPONDING FIELDS OF TABLE t_bsad
        WHERE augdt IN p_pgto
          AND bukrs IN p_bukrs
          AND bschl IN ( '09', '01' ).

      IF t_bsad IS NOT INITIAL.
***        r_docnum = VALUE #( FOR l IN t_bsad ( sign = 'I' option = 'EQ' low = l-belnr ) ).
***        SORT r_docnum[] BY low ASCENDING.
***        DELETE ADJACENT DUPLICATES FROM r_docnum COMPARING low.

        DATA(lva_count) = 0.
        LOOP AT t_bsad[] ASSIGNING FIELD-SYMBOL(<lfs_bsad>).

          IF NOT ( line_exists( r_docnum[ low = <lfs_bsad>-belnr ] ) ).
            lva_count = lva_count + 1.
            APPEND INITIAL LINE TO r_docnum[] ASSIGNING FIELD-SYMBOL(<lfs_docnum>).
            <lfs_docnum>-sign = 'I'.
            <lfs_docnum>-option = 'EQ'.
            <lfs_docnum>-low = <lfs_bsad>-belnr.
            <lfs_docnum>-high = <lfs_bsad>-belnr.
          ENDIF.

          "Quantidade de registros muito grande, limitando à 1000 por select
          IF  ( lva_count = 1000 ).
            PERFORM fm_select_zfit0026.
            CLEAR: lva_count.
          ENDIF.

        ENDLOOP.

        "Caso ainda restem documentos (<1000), buscar o restante
        IF  ( r_docnum[] IS NOT INITIAL ).
          PERFORM fm_select_zfit0026.
        ENDIF.

***        SELECT  zid_lanc vbeln data_venc docnum mont_rbdo moeda mont_moeda mont_mi taxa forma_pag observacao obj_key doc_fatura vlr_multa_calc
***        vlr_juros_calc vlr_multa_rbdo vlr_juros_rbdo vlr_desc_mult vlr_desc_jros ajuste rec_vlr_total
***        FROM zfit0026
***        INTO  CORRESPONDING FIELDS OF TABLE  it_zfit0026
***        WHERE docnum IN r_docnum.

        IF ( git_zfit0026[] IS NOT INITIAL ).
          it_zfit0026[] = CORRESPONDING #( git_zfit0026[] ).
        ENDIF.

        IF it_zfit0026[] IS NOT INITIAL.

          IF p_docsi IS NOT INITIAL.
            SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg a~vkgrp
             FROM vbak AS a
*             INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln  "Comentado 07/05/2021 - CS2020001117 - AO
             INTO TABLE it_vbak
             FOR ALL ENTRIES IN it_zfit0026
             WHERE a~vbeln EQ it_zfit0026-vbeln
             AND a~erdat IN p_erdat
*          AND B~ZTERM NE 'I006'
             AND a~bukrs_vf IN p_bukrs
             "AND A~AUART IN P_AUART
             AND a~spart IN set_in
             AND a~vkbur IN p_vkbur
             AND a~kunnr IN p_kunnr
             AND a~vbeln IN p_vbeln
             AND a~waerk IN c_waerk
             AND ( EXISTS ( SELECT *
                                FROM zsdt0041
                               WHERE vbeln EQ a~vbeln
                                 AND doc_simulacao IN p_docsi ) OR
                     EXISTS ( SELECT *
                                FROM zsdt0090
                               WHERE vbeln EQ a~vbeln
                                 AND doc_simulacao IN p_docsi ) ).
          ELSE.

            SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg a~vkgrp
             FROM vbak AS a
*               INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln  "Comentado 07/05/2021 - CS2020001117 - AO
             INTO TABLE it_vbak
              FOR ALL ENTRIES IN it_zfit0026
             WHERE a~vbeln EQ it_zfit0026-vbeln
             AND a~erdat IN p_erdat
             AND a~bukrs_vf IN p_bukrs
             "AND A~AUART IN P_AUART
             AND a~spart IN set_in
             AND a~vkbur IN p_vkbur
             AND a~kunnr IN p_kunnr
             AND a~vbeln IN p_vbeln
             AND a~waerk IN c_waerk.
          ENDIF.
        ENDIF.
      ENDIF.

      "========================================FIM CS2020001117 - AO
    ELSE.

      IF p_docsi IS NOT INITIAL.
        SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg a~vkgrp
              FROM vbak AS a
*              INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln "Comentado 07/05/2021 - CS2020001117 - AO
              INTO TABLE it_vbak
              WHERE a~erdat IN p_erdat
*          AND B~ZTERM NE 'I006'
              AND a~bukrs_vf IN p_bukrs
              "AND A~AUART IN P_AUART
              AND a~spart IN set_in
              AND a~vkbur IN p_vkbur
              AND a~kunnr IN p_kunnr
              AND a~vbeln IN p_vbeln
              AND a~waerk IN c_waerk
              AND ( EXISTS ( SELECT *
                                 FROM zsdt0041
                                WHERE vbeln EQ a~vbeln
                                  AND doc_simulacao IN p_docsi ) OR
                      EXISTS ( SELECT *
                                 FROM zsdt0090
                                WHERE vbeln EQ a~vbeln
                                  AND doc_simulacao IN p_docsi ) ).
      ELSE.

        SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg a~vkgrp
         FROM vbak AS a
*         INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln "Comentado 07/05/2021 - CS2020001117 - AO
         INTO TABLE it_vbak
         WHERE a~erdat IN p_erdat
*          AND B~ZTERM NE 'I006'
         AND a~bukrs_vf IN p_bukrs
         "AND A~AUART IN P_AUART
         AND a~spart IN set_in
         AND a~vkbur IN p_vkbur
         AND a~kunnr IN p_kunnr
         AND a~vbeln IN p_vbeln
         AND a~waerk IN c_waerk.

* Inclusão - RIM - SKM - IR107590 -Inicio
        IF NOT it_vbak[] IS INITIAL.
          DATA:
            it_z41_aux TYPE TABLE OF zsdt0041,
            it_z90_aux TYPE TABLE OF zsdt0090.

          SELECT *  FROM zsdt0041 INTO TABLE it_z41_aux
            FOR ALL ENTRIES IN it_vbak
            WHERE vbeln EQ it_vbak-vbeln.

          SELECT *  FROM zsdt0090 INTO TABLE it_z90_aux
            FOR ALL ENTRIES IN it_vbak
            WHERE vbeln EQ it_vbak-vbeln.
          IF NOT it_z41_aux[] IS INITIAL."<<RIM-SKM-IR107590-12.08.22

            " 127510 - Transação ZSDT0060 - Melhorar Performance de Select - PANF - Inicio
**            SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg a~vkgrp
**                   FROM vbak AS a
***              INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln "Comentado 07/05/2021 - CS2020001117 - AO
**                   APPENDING  TABLE it_vbak
**               FOR ALL ENTRIES IN it_z41_aux
**                   WHERE a~bukrs_vf IN p_bukrs
**                   "
**                   AND ( EXISTS ( SELECT *
**                                      FROM zsdt0041
**                                     WHERE vbeln EQ a~vbeln
**                                       AND doc_simulacao EQ it_z41_aux-doc_simulacao ) OR
**                           EXISTS ( SELECT *
**                                      FROM zsdt0090
**                                     WHERE vbeln EQ a~vbeln
**                                       AND doc_simulacao EQ it_z41_aux-doc_simulacao ) ).
            SELECT *
             FROM zi_salesorder_simul_agrp
             APPENDING CORRESPONDING FIELDS OF TABLE @it_vbak
             FOR ALL ENTRIES IN @it_z41_aux
             WHERE bukrs_vf IN @p_bukrs
               AND docsimu = @it_z41_aux-doc_simulacao.

          ENDIF.
          IF NOT it_z90_aux[] IS INITIAL.
**            SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg a~vkgrp
**                   FROM vbak AS a
***              INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln "Comentado 07/05/2021 - CS2020001117 - AO
**                   APPENDING  TABLE it_vbak
**               FOR ALL ENTRIES IN it_z90_aux
**                   WHERE a~bukrs_vf IN p_bukrs
**                   "
**                   AND ( EXISTS ( SELECT *
**                                      FROM zsdt0041
**                                     WHERE vbeln EQ a~vbeln
**                                       AND doc_simulacao EQ it_z90_aux-doc_simulacao ) OR
**                           EXISTS ( SELECT *
**                                      FROM zsdt0090
**                                     WHERE vbeln EQ a~vbeln
**                                       AND doc_simulacao EQ it_z90_aux-doc_simulacao ) ).
            SELECT *
            FROM zi_salesorder_simul_agrp
                         APPENDING CORRESPONDING FIELDS OF TABLE @it_vbak
                         FOR ALL ENTRIES IN @it_z90_aux
                         WHERE bukrs_vf IN @p_bukrs
                           AND docsimu = @it_z90_aux-doc_simulacao.

            " 127510 - Transação ZSDT0060 - Melhorar Performance de Select - PANF - Fim
          ENDIF.
        ENDIF.
* Inclusão - RIM - SKM - IR107590 - Fim
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK NOT it_vbak IS INITIAL.

*  " Dados de Taxa Travada
*  SELECT *
*    FROM ZSDT0090
*    INTO TABLE IT_ZSDT0090
*    FOR ALL ENTRIES IN IT_VBAK
*    WHERE VBELV EQ IT_VBAK-VBELN.
*
*  IF IT_ZSDT0090[] IS NOT INITIAL.
*
*    SELECT *
*      FROM ZSDT0040 INTO TABLE IT_ZSDT0040
*      FOR ALL ENTRIES IN IT_ZSDT0090
*     WHERE DOC_SIMULACAO EQ IT_ZSDT0090-DOC_SIMULACAO.
*
*  ELSE.
*
*    SELECT *
*      FROM ZSDT0041 INTO TABLE IT_ZSDT0041
*      FOR ALL ENTRIES IN IT_VBAK
*      WHERE VBELN EQ IT_VBAK-VBELN.
*
*    IF IT_ZSDT0041[] IS NOT INITIAL.
*
*      SELECT *
*        FROM ZSDT0040 INTO TABLE IT_ZSDT0040
*        FOR ALL ENTRIES IN IT_ZSDT0041
*       WHERE DOC_SIMULACAO EQ IT_ZSDT0041-DOC_SIMULACAO.
*
*    ENDIF.
*  ENDIF.

**********************************************************************
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
  DATA: r_vbeln TYPE RANGE OF vbeln.
  DATA: g_vbeln TYPE RANGE OF vbeln.

  FREE r_vbeln.
  r_vbeln = VALUE #( FOR ls_vbak IN it_vbak ( sign = 'I' option = 'EQ' low = ls_vbak-vbeln ) ).

  DELETE r_vbeln WHERE low IS INITIAL.
  SORT r_vbeln BY low.
  DELETE ADJACENT DUPLICATES FROM r_vbeln COMPARING low.

  SELECT vbeln vbelv
    FROM zsdt0090
      APPENDING TABLE it_vbel
          WHERE ( vbeln IN r_vbeln OR
                  vbelv IN r_vbeln )
            AND doc_simulacao IN p_docsi
            AND estorno NE abap_true.

*****  SELECT vbeln, vbelv
*****    FROM zsdt0090
*****   APPENDING TABLE @it_vbel
*****       FOR ALL ENTRIES IN @it_vbak
*****       WHERE vbeln = @it_vbak-vbeln
*****         AND doc_simulacao IN @p_docsi
*****         AND estorno NE @abap_true.
*****
*****  SELECT vbeln, vbelv
*****     FROM zsdt0090
*****     APPENDING TABLE @it_vbel
*****      FOR ALL ENTRIES IN @it_vbak
*****      WHERE vbelv = @it_vbak-vbeln
*****        AND doc_simulacao IN @p_docsi
*****        AND estorno NE @abap_true.

** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
**********************************************************************

  MOVE it_vbel TO it_vbelx.

  IF it_vbelx IS NOT INITIAL.

**********************************************************************
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
    FREE: r_vbeln, g_vbeln.
    r_vbeln = VALUE #( FOR ls_vbelx IN it_vbelx ( sign = 'I' option = 'EQ' low = ls_vbelx-vbeln ) ).
    APPEND LINES OF r_vbeln TO g_vbeln.

    r_vbeln = VALUE #( FOR ls_vbelx IN it_vbelx ( sign = 'I' option = 'EQ' low = ls_vbelx-vbelv ) ).
    APPEND LINES OF r_vbeln TO g_vbeln.

    DELETE g_vbeln WHERE low IS INITIAL.
    SORT g_vbeln BY low.
    DELETE ADJACENT DUPLICATES FROM g_vbeln COMPARING low.

    SELECT vbeln vbelv
      FROM zsdt0090
        APPENDING TABLE it_vbel
            WHERE ( vbelv IN g_vbeln OR vbeln IN g_vbeln )
              AND estorno NE abap_true.

****    SELECT vbeln, vbelv
****     FROM zsdt0090
****       APPENDING TABLE @it_vbel
****           FOR ALL ENTRIES IN @it_vbelx
****           WHERE vbelv = @it_vbelx-vbeln
****             AND estorno NE @abap_true.
****
****    SELECT vbeln, vbelv
****   FROM zsdt0090
****     APPENDING TABLE @it_vbel
****         FOR ALL ENTRIES IN @it_vbelx
****         WHERE vbeln = @it_vbelx-vbeln
****           AND estorno NE @abap_true.
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
**********************************************************************

    SORT it_vbel BY vbeln vbelv.
    DELETE ADJACENT DUPLICATES FROM it_vbel COMPARING vbeln vbelv.

    FREE: r_vbeln, g_vbeln, it_vbelx.

    r_vbeln = VALUE #( FOR ls_vbel_aux IN it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbeln ) ).
    APPEND LINES OF r_vbeln TO g_vbeln.

    r_vbeln = VALUE #( FOR ls_vbel_aux IN it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbelv ) ).
    APPEND LINES OF r_vbeln TO g_vbeln.

    DELETE g_vbeln WHERE low IS INITIAL.
    SORT g_vbeln BY low.
    DELETE ADJACENT DUPLICATES FROM g_vbeln COMPARING low.

    IF g_vbeln IS NOT INITIAL.
**    IF it_vbel IS NOT INITIAL.

      SELECT vbeln vbelv
        FROM zsdt0090
      APPENDING TABLE it_vbel
      WHERE ( vbelv IN g_vbeln OR vbeln IN g_vbeln )
        AND doc_simulacao IN p_docsi
        AND estorno NE abap_true.

***      SELECT vbeln, vbelv
***  FROM zsdt0090
***APPENDING TABLE @it_vbel
***        FOR ALL ENTRIES IN @it_vbel
***      WHERE vbelv = @it_vbel-vbeln
***        AND doc_simulacao IN @p_docsi
***        AND estorno NE @abap_true.
***
***      SELECT vbeln, vbelv
***     FROM zsdt0090
***   APPENDING TABLE @it_vbel
***        FOR ALL ENTRIES IN @it_vbel
***   WHERE vbeln = @it_vbel-vbeln
***     AND doc_simulacao IN @p_docsi
***     AND estorno NE @abap_true.

** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
**********************************************************************

**********************************************************************
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
      FREE: r_vbeln, g_vbeln.
      r_vbeln = VALUE #( FOR ls_vbel_aux IN it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbeln ) ).
      APPEND LINES OF r_vbeln TO g_vbeln.

      r_vbeln = VALUE #( FOR ls_vbel_aux IN it_vbel ( sign = 'I' option = 'EQ' low = ls_vbel_aux-vbelv ) ).
      APPEND LINES OF r_vbeln TO g_vbeln.

      DELETE g_vbeln WHERE low IS INITIAL.
      SORT g_vbeln BY low.
      DELETE ADJACENT DUPLICATES FROM g_vbeln COMPARING low.
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
**********************************************************************

    ENDIF.

*    WHILE IT_VBEL NE IT_VBELX.
*
*      MOVE IT_VBEL TO IT_VBELX.
*
*      LOOP AT IT_VBELX INTO WA_VBELX.
*
*        "Ajuste Chamado CS2017000602 para não buscar na ZSDT0090 com campos VBELN e VBELV Zerados
*        IF WA_VBELX-VBELN IS INITIAL.
*          WA_VBELX-VBELN = WA_VBELX-VBELV.
*        ELSEIF WA_VBELX-VBELV IS INITIAL.
*          WA_VBELX-VBELV = WA_VBELX-VBELN.
*        ENDIF.
*
*        SELECT VBELN VBELV
*        FROM ZSDT0090
*        APPENDING TABLE IT_VBEL
*        WHERE ( VBELV EQ WA_VBELX-VBELN
*             OR VBELV EQ WA_VBELX-VBELV
*             OR VBELN EQ WA_VBELX-VBELN
*             OR VBELN EQ WA_VBELX-VBELV )
*          AND DOC_SIMULACAO IN P_DOCSI.
*
**        SORT IT_VBEL BY VBELN.
**        DELETE ADJACENT DUPLICATES FROM IT_VBEL COMPARING VBELN.
*        SORT IT_VBEL BY VBELN VBELV.
*        DELETE ADJACENT DUPLICATES FROM IT_VBEL COMPARING VBELN VBELV.
*
*
*      ENDLOOP.
*
*    ENDWHILE.

*    SORT IT_VBEL BY VBELN.
*    DELETE ADJACENT DUPLICATES FROM IT_VBEL COMPARING VBELN.
*    SORT it_vbel BY vbeln vbelv.
*    DELETE ADJACENT DUPLICATES FROM it_vbel COMPARING vbeln vbelv.

**********************************************************************
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
    IF g_vbeln IS NOT INITIAL.
**    IF it_vbel IS NOT INITIAL.

***      SELECT vbeln, erdat, auart, vkbur, kunnr, waerk, vkorg, vkgrp
***        FROM vbak
***        APPENDING CORRESPONDING FIELDS OF TABLE @it_vbak
***        FOR ALL ENTRIES IN @it_vbel
***        WHERE vbeln = @it_vbel-vbeln.

      SELECT vbeln erdat auart vkbur kunnr waerk vkorg vkgrp
     FROM vbak
     APPENDING CORRESPONDING FIELDS OF TABLE it_vbak
     WHERE vbeln IN g_vbeln.
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
**********************************************************************

    ENDIF.
  ENDIF.

  SELECT  *
     FROM vbfa
       INTO TABLE it_remessa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n IN ( 'J', 'T' )
             AND vbtyp_v IN ( 'C', 'H' ).

  IF it_remessa IS NOT INITIAL.
    SELECT *
      FROM vbfa
      INTO TABLE it_estreme
       FOR ALL ENTRIES IN it_remessa
       WHERE vbeln EQ it_remessa-vbeln
         AND vbtyp_n EQ 'J'
         AND vbtyp_v EQ 'J'.
    LOOP AT it_estreme INTO DATA(wa_estreme).
      DELETE it_remessa WHERE vbeln EQ wa_estreme-vbeln AND posnn = wa_estreme-posnn.
    ENDLOOP.
  ENDIF.

* INCLUINDO A Recusa e Devolução CS2016000023 >>>>>
  SELECT  *
     FROM vbfa
       INTO TABLE it_vbfa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n EQ 'H'
             AND vbtyp_v EQ 'C'.

  SELECT  *
     FROM vbfa
       APPENDING TABLE it_vbfa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n IN ('L', 'C')
             AND vbtyp_v EQ 'C'.

* Inclusão - RIM - SKM - IR107590
  SELECT  *
     FROM vbfa
       APPENDING TABLE it_vbfa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbeln EQ it_vbak-vbeln
             AND vbtyp_n IN ('L', 'C')
             AND vbtyp_v EQ 'C'.
* Inclusão - RIM - SKM - IR107590

  IF NOT it_vbfa[] IS INITIAL.

    SELECT  vbeln erdat auart vkbur kunnr waerk vkorg
    FROM vbak
    APPENDING TABLE it_vbak
      FOR ALL ENTRIES IN it_vbfa
    WHERE vbeln EQ it_vbfa-vbeln.

  ENDIF.
* INCLUINDO A Recusa e Devolução CS2016000023 <<<<<

  SORT it_vbak BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vbak COMPARING vbeln.

  IF it_vbak IS NOT INITIAL.

    SELECT vbeln doc_simulacao "vbelv_agp
      FROM zsdt0041
      INTO TABLE it_0041
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln         EQ it_vbak-vbeln
        AND doc_simulacao IN p_docsi.

* Inclusão - RIM - SKM - IR107590
    SELECT vbeln doc_simulacao
      FROM zsdt0041
      APPENDING TABLE it_0041
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln         EQ it_vbak-vbeln
        AND doc_simulacao IN p_docsi.
* Inclusão - RIM - SKM - IR107590


* INCLUINDO O CAMPO SAFRA CS2016000023 >>>>>
    IF NOT it_0041 IS INITIAL.

      SELECT *
         FROM zsdt0040
         INTO TABLE it_0040
         FOR ALL ENTRIES IN it_0041
         WHERE doc_simulacao EQ it_0041-doc_simulacao.

    ENDIF.
* INCLUINDO O CAMPO SAFRA CS2016000023 <<<<<

    SELECT vbeln vbelv doc_simulacao
      FROM zsdt0090
      INTO TABLE it_0090
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln
      AND estorno NE 'X'.

* INCLUINDO O CAMPO SAFRA CS2016000023 >>>>>
    IF  NOT it_0090 IS INITIAL.

      SELECT *
        FROM zsdt0040
        APPENDING TABLE it_0040
        FOR ALL ENTRIES IN it_0090
        WHERE doc_simulacao EQ it_0090-doc_simulacao.

* Inclusão - RIM - SKM - IR107590 - INICIO
      IF NOT it_vbfa[] IS INITIAL.
        SELECT vbeln doc_simulacao
          FROM zsdt0041
          APPENDING TABLE it_0041
            FOR ALL ENTRIES IN it_vbfa
            WHERE vbeln EQ it_vbfa-vbelv.
      ENDIF.
* Inclusão - RIM - SKM - IR107590 - FIM

    ENDIF.
* INCLUINDO O CAMPO SAFRA CS2016000023 <<<<<<

    "CS2020000381
    IF it_0040[] IS NOT INITIAL.

      SELECT * INTO TABLE @it_zsdt0038
        FROM zsdt0038
        FOR ALL ENTRIES IN @it_0040
       WHERE cultura EQ @it_0040-cultura.

      SORT it_zsdt0038 BY cultura.

    ENDIF.

    FREE: it_zfit0026.
    SELECT  zid_lanc vbeln data_venc docnum mont_rbdo moeda mont_moeda mont_mi taxa forma_pag observacao obj_key doc_fatura vlr_multa_calc
    vlr_juros_calc vlr_multa_rbdo vlr_juros_rbdo vlr_desc_mult vlr_desc_jros ajuste rec_vlr_total  num_comp_adiant "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    FROM zfit0026
    INTO  CORRESPONDING FIELDS OF TABLE  it_zfit0026
    FOR ALL ENTRIES IN it_vbak
    WHERE vbeln	=	it_vbak-vbeln.

    IF it_zfit0026[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0159
        INTO TABLE it_z0159
        FOR ALL ENTRIES IN it_zfit0026
        WHERE obj_key EQ it_zfit0026-obj_key
        AND estorno NE 'X'.

      SELECT *
        FROM zib_contabil_chv
        INTO TABLE it_zibchv
        FOR ALL ENTRIES IN it_zfit0026
        WHERE obj_key EQ it_zfit0026-obj_key.

    ENDIF.

    SELECT   vbeln zterm  kurrf valdt
      FROM vbkd
      INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln	= it_vbak-vbeln.

  ENDIF.

*   FIM MODIFICAÇÃO WELGEM

  SELECT vbeln posnr kwmeng netwr werks mwsbp zmeng
    FROM vbap
    INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_vbak
    WHERE vbeln  = it_vbak-vbeln
      AND NOT EXISTS ( SELECT *
                         FROM vbep
                        WHERE vbeln EQ vbap~vbeln
                          AND posnr EQ vbap~posnr
                          AND lifsp EQ '12' ).

  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
   FOR ALL ENTRIES IN it_vbak
   WHERE kunnr  = it_vbak-kunnr.

  SELECT  spras zterm text1
    FROM t052u
    INTO TABLE it_t052u
   FOR ALL ENTRIES IN it_vbkd
   WHERE zterm  = it_vbkd-zterm
    AND spras = 'PT'.

  r_belnr =  VALUE #( FOR ls IN it_zfit0026 ( sign = 'I' option = 'EQ' low = ls-docnum ) ).
  r_belnr_aux =  VALUE #( FOR ls3 IN it_z0159 ( sign = 'I' option = 'EQ' low = ls3-adiant ) ).
  APPEND LINES OF r_belnr_aux TO r_belnr.
  r_belnr_aux =  VALUE #( FOR ls4 IN it_zibchv ( sign = 'I' option = 'EQ' low = ls4-belnr ) ).
  APPEND LINES OF r_belnr_aux TO r_belnr.

  r_bukrs =  VALUE #( FOR ls1 IN  it_vbak   ( sign = 'I' option = 'EQ' low = ls1-vkorg ) ).
  r_kunnr =  VALUE #( FOR ls2 IN  it_vbak   ( sign = 'I' option = 'EQ' low = ls2-kunnr ) ).
**********************************************************************
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
  SORT: r_belnr BY low,
        r_bukrs BY low,
        r_kunnr BY low.
  DELETE ADJACENT DUPLICATES FROM r_belnr COMPARING low.
  DELETE ADJACENT DUPLICATES FROM r_bukrs COMPARING low.
  DELETE ADJACENT DUPLICATES FROM r_kunnr COMPARING low.
** 152099 - Ajustes DUMP - PANF - 10.09.24 - INICIO
**********************************************************************


  IF NOT r_belnr[] IS INITIAL.            "<<RIM-SKM-IR107590-12.08.22
    SELECT bukrs kunnr belnr augbl budat augdt dmbe2 dmbtr
      FROM bsad
      INTO CORRESPONDING FIELDS OF TABLE it_bsad
    WHERE bukrs IN r_bukrs
      AND belnr IN r_belnr
      AND kunnr IN r_kunnr
*    AND augdt IN p_pgto
      AND bschl IN ( '09', '01' ).
  ENDIF.                                   "<<RIM-SKM-IR107590-12.08.22

*      SELECT  BS~BUKRS BS~KUNNR BS~BELNR BS~AUGBL BS~BUDAT BS~DMBE2 BS~DMBTR
*        FROM BSAD AS BS
*        INNER JOIN ZFIT0026 AS ZF ON ZF~DOCNUM  = BS~BELNR
*        INTO TABLE IT_BSAD
*      FOR ALL ENTRIES IN IT_VBAK
*      WHERE  BS~BUKRS  = IT_VBAK-VKORG
*      AND BS~KUNNR  = IT_VBAK-KUNNR.

  IF it_vbfa_auxi IS NOT INITIAL.
    SELECT doc~nfenum refkey
    FROM j_1bnflin AS lin
    INNER JOIN j_1bnfdoc AS doc ON  doc~docnum = lin~docnum
    INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfdoc
    FOR ALL ENTRIES IN it_vbfa_auxi
    WHERE lin~refkey = it_vbfa_auxi-refkey.
  ENDIF.

  IF it_vbak IS NOT INITIAL.
    DELETE it_vbak WHERE auart EQ 'ZFNT'.
  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS

FORM f_seleciona_dados_mi.
  DATA: r_docnum TYPE RANGE OF j_1bdocnum.
  DATA: c_waerk TYPE RANGE OF vbak-waerk WITH HEADER LINE.
  DATA: r_belnr TYPE RANGE OF belnr_d.
  DATA: r_belnr_aux TYPE RANGE OF belnr_d.
  DATA: r_bukrs TYPE RANGE OF bukrs.
  DATA: r_vbeln TYPE RANGE OF vbeln.
  DATA: r_vbeln_aux TYPE RANGE OF vbeln.
  DATA: r_kunnr TYPE RANGE OF kunnr.
  DATA: dt_venc TYPE bseg-zfbdt.
  DATA: zvbeln TYPE vbeln,
        zzterm TYPE dzterm.

  FREE:c_waerk, it_vbfa, it_vbkd, it_vbak, it_vbel, it_0041, it_0040,
       it_0090, it_zfit0026, it_vbkd, it_vbap, it_kna1, it_t052u, it_bsad, ts_vbrk.

  c_waerk-sign   = 'I'.
  c_waerk-option = 'EQ'.

  DATA(obj_auart) = NEW zcl_taxa_curva( ).
  DATA(set_mi) = obj_auart->get_auart( 'ZFIS26_SA_MI' ).

*    CASE ABAP_TRUE.
*      WHEN RB8.
*        SET_IN_MI = OBJ_AUART->GET_AUART( 'ZFIS26_SA_MI' ).
*    ENDCASE.
  "=====================================================CS2020001117
  IF p_venc IS NOT INITIAL.

    "Selecionar dados com data de vencimento por OV
    IF p_docsi IS NOT INITIAL.
      SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
      FROM vbak AS a
        INNER JOIN vbkd AS b ON a~vbeln EQ  b~vbeln
        INNER JOIN t052 AS c ON c~zterm EQ b~zterm
      INTO TABLE it_vbak
      WHERE a~erdat IN p_erdat
      AND b~valdt	IN p_venc
      AND c~zdart NE 'B'
*      AND b~zterm NE 'I006'
      AND a~bukrs_vf IN p_bukrs
      AND a~spart IN set_mi
      AND a~vkbur IN p_vkbur
      AND a~kunnr IN p_kunnr
      AND a~vbeln IN p_vbeln
      AND a~waerk IN c_waerk
      AND ( EXISTS ( SELECT *
                        FROM zsdt0053
                          WHERE vbeln EQ a~vbeln
                            AND nro_sol_ov IN p_docsi ) OR
                         EXISTS ( SELECT *
                                   FROM zsdt0100
                                  WHERE vbeln EQ a~vbeln
                                    AND nro_sol_ov IN p_docsi ) ).


      "Selecionar todas as OV que a data de vencimento é por fatura.
      SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
      FROM vbak AS a
        INNER JOIN vbkd AS b ON a~vbeln EQ  b~vbeln
        INNER JOIN t052 AS c ON c~zterm EQ b~zterm
      INTO TABLE t_vbak
      WHERE a~erdat IN p_erdat
      AND c~zdart EQ 'B'
*      AND b~zterm NE 'I006'
      AND a~bukrs_vf IN p_bukrs
      AND a~spart IN set_mi
      AND a~vkbur IN p_vkbur
      AND a~kunnr IN p_kunnr
      AND a~vbeln IN p_vbeln
      AND a~waerk IN c_waerk
      AND ( EXISTS ( SELECT *
                         FROM zsdt0053
                        WHERE vbeln EQ a~vbeln
                          AND nro_sol_ov IN p_docsi ) OR
                       EXISTS ( SELECT *
                                 FROM zsdt0100
                                WHERE vbeln EQ a~vbeln
                                  AND nro_sol_ov IN p_docsi ) ).


    ELSE.

      "Selecionar dados com data de vencimento por OV
      SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
      FROM vbak AS a
      INNER JOIN vbkd AS b ON a~vbeln EQ b~vbeln
      INNER JOIN t052 AS c ON c~zterm EQ b~zterm
      INTO TABLE it_vbak
      WHERE a~erdat IN p_erdat
      AND c~zdart NE 'B'
*       AND b~zterm NE 'I006'
      AND b~valdt  IN p_venc
      AND a~bukrs_vf IN p_bukrs
      AND a~spart IN set_mi
      AND a~vkbur IN p_vkbur
      AND a~kunnr IN p_kunnr
      AND a~vbeln IN p_vbeln
      AND a~waerk IN c_waerk.


      "Selecionar todas as OV que a data de vencimento é por fatura.
      SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
       FROM vbak AS a
       INNER JOIN vbkd AS b ON a~vbeln  EQ b~vbeln
       INNER JOIN t052 AS c ON c~zterm EQ b~zterm
       INTO TABLE t_vbak
       WHERE a~erdat IN p_erdat
       AND c~zdart EQ 'B'
*       AND b~zterm NE 'I006'
       AND a~bukrs_vf IN p_bukrs
       AND a~spart IN set_mi
       AND a~vkbur IN p_vkbur
       AND a~kunnr IN p_kunnr
       AND a~vbeln IN p_vbeln
       AND a~waerk IN c_waerk.
    ENDIF.

    IF t_vbak IS NOT INITIAL.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO

      "Selecionar as faturas da OVs.
      SELECT rk~vbeln, rk~fkart, rk~fktyp, rk~zterm, fa~vbelv, venc~datavenc
      FROM   vbrk  AS rk
      INNER JOIN vbfa AS fa  ON fa~vbeln = rk~vbeln
      INNER JOIN zi_sd_dtvenci_fat_ov AS venc ON rk~vbeln = venc~ref
      FOR ALL ENTRIES IN @t_vbak
      WHERE fa~vbelv = @t_vbak-vbeln
      AND  fa~vbtyp_n  = 'M'
      AND  fa~vbtyp_v  = 'C'
      AND  stufe      = '01'
      INTO TABLE @ti_vbrk.

      IF ti_vbrk IS NOT INITIAL.
****        LOOP AT ti_vbrk ASSIGNING FIELD-SYMBOL(<w_vbrk>).
****          CLEAR: dt_venc, zvbeln, zzterm.
****
****
****          zvbeln = CONV #( <w_vbrk>-vbeln ).
****          zzterm = CONV #( <w_vbrk>-zterm ).
****          CALL FUNCTION 'J_1A_SD_CI_DUEDATE_CHECK'
****            EXPORTING
****              iv_vbeln                 = zvbeln
****              iv_zterm                 = zzterm
****              iv_ratnr                 = '01'
****            IMPORTING
****              ev_netdate               = dt_venc
****            EXCEPTIONS
****              time_below_limit         = 1
****              fi_document_not_found    = 2
****              payment_terms_incomplete = 3
****              invoice_not_found        = 4
****              OTHERS                   = 5.
****          IF dt_venc IS NOT INITIAL.
****            <w_vbrk>-valdt = dt_venc.
****          ENDIF.
****        ENDLOOP.

*  Ajustes Performance - PANF - 28.08.24 - FIM
**********************************************************************

        "Deletar as faturas diferente da data de vencimento selecionada.
        SORT ti_vbrk ASCENDING BY valdt.
        DELETE ti_vbrk WHERE valdt NOT IN p_venc.
        SORT ti_vbrk ASCENDING BY vbelv.
        r_vbeln = VALUE #( FOR s IN ti_vbrk ( sign = 'I' option = 'EQ' low = s-vbelv ) ).

        IF r_vbeln IS NOT INITIAL.
          DELETE t_vbak WHERE vbeln NOT IN r_vbeln.
        ENDIF.
      ENDIF.

      "Incluir as OVs com vencimento de fatura junto com as vencimento por OV.
      IF t_vbak IS NOT INITIAL.
        APPEND LINES OF t_vbak TO it_vbak.
      ENDIF.
    ENDIF.
  ELSE.

    "Verifica se o filtro de pagamento foi preenchido.
    "----------------------------------"CS2020001117
    IF p_pgto IS NOT INITIAL.

      SELECT * FROM bsad AS a
        INTO CORRESPONDING FIELDS OF TABLE t_bsad
        WHERE augdt IN p_pgto
          AND bukrs IN p_bukrs
          AND bschl IN ( '09', '01' ).

      IF t_bsad IS NOT INITIAL.
        r_docnum = VALUE #( FOR l IN t_bsad ( sign = 'I' option = 'EQ' low = l-belnr ) ).
        FREE: it_zfit0026.

        SELECT  zid_lanc vbeln data_venc docnum mont_rbdo moeda mont_moeda mont_mi taxa forma_pag observacao obj_key doc_fatura vlr_multa_calc
        vlr_juros_calc vlr_multa_rbdo vlr_juros_rbdo vlr_desc_mult vlr_desc_jros ajuste rec_vlr_total
        FROM zfit0026
        INTO  CORRESPONDING FIELDS OF TABLE  it_zfit0026
        WHERE docnum IN r_docnum.

        IF it_zfit0026[] IS NOT INITIAL.
          "Verifica se o documento simulação for preenchido.
          IF p_docsi IS NOT INITIAL.

            SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
              FROM vbak AS a
*                INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln "Comentado 07/05/2021 - CS2020001117 - AO
              INTO TABLE it_vbak
              FOR ALL ENTRIES IN it_zfit0026
              WHERE a~vbeln EQ it_zfit0026-vbeln
              AND a~erdat IN p_erdat
*              AND b~zterm NE 'I006'
              AND a~bukrs_vf IN p_bukrs
              AND a~spart IN set_mi
              AND a~vkbur IN p_vkbur
              AND a~kunnr IN p_kunnr
              AND a~vbeln IN p_vbeln
              AND a~waerk IN c_waerk
              AND ( EXISTS ( SELECT *
                                 FROM zsdt0053
                                WHERE vbeln EQ a~vbeln
                                  AND nro_sol_ov IN p_docsi ) OR
                               EXISTS ( SELECT *
                                         FROM zsdt0100
                                        WHERE vbeln EQ a~vbeln
                                          AND nro_sol_ov IN p_docsi ) ).

          ELSE.

            SELECT a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
              FROM vbak AS a
*                INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln "Comentado 07/05/2021 - CS2020001117 - AO
              INTO TABLE it_vbak
              FOR ALL ENTRIES IN it_zfit0026
              WHERE a~vbeln EQ it_zfit0026-vbeln
              AND  a~erdat IN p_erdat
*              AND b~zterm NE 'I006'
              AND a~bukrs_vf IN p_bukrs
              AND a~spart IN set_mi
              AND a~vkbur IN p_vkbur
              AND a~kunnr IN p_kunnr
              AND a~vbeln IN p_vbeln
              AND a~waerk IN c_waerk.
          ENDIF.
        ENDIF.
      ENDIF.
      "===========================================FIM CS2020001117
    ELSE.

      IF p_docsi IS NOT INITIAL.
        SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
        FROM vbak AS a
*        INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln
        INTO TABLE it_vbak
        WHERE a~erdat IN p_erdat
*      AND b~zterm NE 'I006'
        AND a~bukrs_vf IN p_bukrs
        AND a~spart IN set_mi
        AND a~vkbur IN p_vkbur
        AND a~kunnr IN p_kunnr
        AND a~vbeln IN p_vbeln
        AND a~waerk IN c_waerk
        AND ( EXISTS ( SELECT *
                           FROM zsdt0053
                          WHERE vbeln EQ a~vbeln
                            AND nro_sol_ov IN p_docsi ) OR
                         EXISTS ( SELECT *
                                   FROM zsdt0100
                                  WHERE vbeln EQ a~vbeln
                                    AND nro_sol_ov IN p_docsi ) ).
      ELSE.
        SELECT  a~vbeln a~erdat a~auart a~vkbur a~kunnr a~waerk a~vkorg
        FROM vbak AS a
*        INNER JOIN vbkd AS b ON a~vbeln  = b~vbeln
       INTO TABLE it_vbak
       WHERE a~erdat IN p_erdat
*      AND b~zterm NE 'I006'
       AND a~bukrs_vf IN p_bukrs
       AND a~spart IN set_mi
       AND a~vkbur IN p_vkbur
       AND a~kunnr IN p_kunnr
       AND a~vbeln IN p_vbeln
       AND a~waerk IN c_waerk.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK NOT it_vbak IS INITIAL.

  SELECT  *
     FROM vbfa
       INTO TABLE it_remessa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n IN ( 'J', 'T' )
             AND vbtyp_v IN ( 'C', 'H' ).

  IF it_remessa IS NOT INITIAL.
    SELECT *
      FROM vbfa
      INTO TABLE it_estreme
       FOR ALL ENTRIES IN it_remessa
       WHERE vbeln EQ it_remessa-vbeln
         AND vbtyp_n EQ 'J'
         AND vbtyp_v EQ 'J'.
    LOOP AT it_estreme INTO DATA(wa_estreme).
      DELETE it_remessa WHERE vbeln EQ wa_estreme-vbeln AND posnn = wa_estreme-posnn.
    ENDLOOP.
  ENDIF.

* INCLUINDO A Recusa e Devolução CS2016000023 >>>>>
  SELECT  *
     FROM vbfa
       INTO TABLE it_vbfa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n EQ 'H'
             AND vbtyp_v EQ 'C'.

  SELECT  *
     FROM vbfa
       APPENDING TABLE it_vbfa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n IN ('L', 'C')
             AND vbtyp_v IN ('C', 'L'). " INCLUINDO CONDIÇÃO PARA COMTEMPLAR AS OV DE COMPLEMENTO E REMESSA DE ENTREGA FUTURA 03.09.24 - PQ

  IF NOT it_vbfa[] IS INITIAL.

    SELECT  vbeln erdat auart vkbur kunnr waerk vkorg vkgrp
    FROM vbak
    APPENDING TABLE it_vbak
      FOR ALL ENTRIES IN it_vbfa
    WHERE vbeln EQ it_vbfa-vbeln.

**********************************************************************
** Valor qtd faturada por doc de simulação - PANF - 28.08.24 - INICIO

    IF sy-subrc = 0.
      SELECT  *
       FROM vbfa
       INTO TABLE it_remessa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
**********************************************************************
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - INICIO
             AND vbtyp_n IN ( 'M' )
             AND vbtyp_v IN ( 'C' )
             AND stufe EQ '01'.
**             AND vbtyp_n IN ( 'J', 'T' )
**             AND vbtyp_v IN ( 'C', 'H' ).

      SELECT  *
       FROM vbfa
       APPENDING TABLE it_remessa
         FOR ALL ENTRIES IN it_vbak
           WHERE vbelv EQ it_vbak-vbeln
             AND vbtyp_n IN ( 'O', 'P', 'M' )
             AND vbtyp_v IN ( 'H', 'L' )
             AND stufe EQ '00'.

** 152099 - busca dos estorno da faturas
*      SELECT  *
*       FROM vbfa
*       APPENDING TABLE it_remessa
*         FOR ALL ENTRIES IN it_vbak
*           WHERE vbelv EQ it_vbak-vbeln
*             AND vbtyp_n IN ( 'N' )
*             AND vbtyp_v IN ( 'C' )
*             AND stufe EQ '01'.
*
*      SELECT  *
*       FROM vbfa
*       APPENDING TABLE it_remessa
*         FOR ALL ENTRIES IN it_vbak
*           WHERE vbelv EQ it_vbak-vbeln
*             AND vbtyp_n IN ( 'N', 'S' )
*             AND vbtyp_v IN ( 'L', 'H', 'C' )
*             AND stufe EQ '00'.
** 152099 - busca dos estorno da faturas
**********************************************************************
** 152099 - Ajustes fluxo doc - PANF - 11.09.24 - FIM

      IF it_remessa IS NOT INITIAL.
        SELECT *
          FROM vbfa
          INTO TABLE it_estreme
           FOR ALL ENTRIES IN it_remessa
           WHERE vbeln EQ it_remessa-vbeln
             AND vbtyp_n EQ 'J'
             AND vbtyp_v EQ 'J'.
        LOOP AT it_estreme INTO wa_estreme.
          DELETE it_remessa WHERE vbeln EQ wa_estreme-vbeln AND posnn = wa_estreme-posnn.
        ENDLOOP.
      ENDIF.
    ENDIF.
** Valor qtd faturada por doc de simulação - PANF - 28.08.24 - Fim
**********************************************************************

  ENDIF.

* INCLUINDO A Recusa e Devolução CS2016000023 <<<<<
  SORT it_vbak BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vbak COMPARING vbeln.

  IF it_vbak IS NOT INITIAL.

* INCLUINDO O CAMPO SAFRA CS2016000023 <<<<<<

    FREE: it_zfit0026.
    SELECT
      zid_lanc
      vbeln
      data_venc
      docnum
      moeda
      mont_rbdo
      mont_moeda
      mont_mi taxa
      forma_pag
      observacao
      obj_key
      doc_fatura
      vlr_multa_calc
      vlr_juros_calc
      vlr_multa_rbdo
      vlr_juros_rbdo
      mont_rbdo
      data_pgto
      vlr_desc_mult
      vlr_desc_jros
      ajuste
      rec_vlr_total
      FROM zfit0026
      INTO  CORRESPONDING FIELDS OF TABLE  it_zfit0026
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln	=	it_vbak-vbeln.

    IF it_zfit0026[] IS NOT INITIAL.

      LOOP AT it_zfit0026  ASSIGNING FIELD-SYMBOL(<fs_zfit0026>).
        <fs_zfit0026>-docnum_conv = <fs_zfit0026>-docnum.
      ENDLOOP.

      SELECT *
      FROM zsdt0054
      INTO TABLE it_z0054
      FOR ALL ENTRIES IN it_zfit0026
      WHERE adiant EQ  it_zfit0026-docnum_conv.
    ENDIF.

*    SELECT VBELN ZTERM  KURRF VALDT
*      FROM VBKD
*      INTO TABLE IT_VBKD
*      FOR ALL ENTRIES IN IT_VBAK
*      WHERE VBELN  = IT_VBAK-VBELN.
  ENDIF.

*   FIM MODIFICAÇÃO WELGEM

  SELECT vbeln posnr kwmeng netwr werks mwsbp zmeng
    FROM vbap
    INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_vbak
    WHERE vbeln  = it_vbak-vbeln
      AND NOT EXISTS ( SELECT *
                         FROM vbep
                        WHERE vbeln EQ vbap~vbeln
                          AND posnr EQ vbap~posnr
                          AND lifsp EQ '12' ).

  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
   FOR ALL ENTRIES IN it_vbak
   WHERE kunnr  = it_vbak-kunnr.

  SELECT  spras zterm text1
    FROM t052u
    INTO TABLE it_t052u
   FOR ALL ENTRIES IN it_vbkd
   WHERE zterm  = it_vbkd-zterm
    AND spras = 'PT'.

  r_belnr =  VALUE #( FOR ls IN it_zfit0026 ( sign = 'I' option = 'EQ' low = ls-docnum ) ).

  APPEND LINES OF r_belnr_aux TO r_belnr.

  r_bukrs =  VALUE #( FOR ls1 IN  it_vbak   ( sign = 'I' option = 'EQ' low = ls1-vkorg ) ).
  r_kunnr =  VALUE #( FOR ls2 IN  it_vbak   ( sign = 'I' option = 'EQ' low = ls2-kunnr ) ).

  SORT r_bukrs BY low.
  SORT r_kunnr BY low.
  SORT r_belnr BY low.

  DELETE ADJACENT DUPLICATES FROM r_bukrs COMPARING low.
  DELETE ADJACENT DUPLICATES FROM r_kunnr COMPARING low.
  DELETE ADJACENT DUPLICATES FROM r_belnr COMPARING low.

  SELECT bukrs kunnr belnr augbl budat augdt dmbe2 dmbtr
    FROM bsad
    INTO CORRESPONDING FIELDS OF TABLE it_bsad
  WHERE bukrs IN r_bukrs
    AND belnr IN r_belnr
    AND kunnr IN r_kunnr
*    AND augdt IN p_pgto
    AND bschl IN ( '09', '01' ).

  SELECT *
  FROM zsdt0053
  INTO TABLE it_zsdt0053
    FOR ALL ENTRIES IN it_vbak
    WHERE vbeln EQ it_vbak-vbeln.


  SELECT *
  FROM zsdt0051
  INTO TABLE it_zsdt0051
    FOR ALL ENTRIES IN it_zsdt0053
    WHERE nro_sol_ov EQ it_zsdt0053-nro_sol_ov.

**********************************************************************
** 151268 Verificar OV`s devolução - PANF - 28.08.24 - INICIO
  SELECT *
   FROM tvarvc
   INTO TABLE @gt_ov_devolucao
   WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'.

  IF sy-subrc = 0..
    SORT: gt_ov_devolucao BY low.
  ENDIF.

** 151268 Verificar OV`s devolução - PANF - 28.08.24 - Fim
**********************************************************************

**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
  DATA: lv_hkont_aux TYPE bseg-hkont,
        lv_htype     TYPE dd01v-datatype.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(lt_tvarvc)
    WHERE name EQ 'CONTA_JUROS'.

  IF sy-subrc EQ 0.
    CLEAR: lr_contas.
    LOOP AT lt_tvarvc INTO DATA(lw_tvarvc).
      CLEAR: lv_hkont_aux, lv_htype.
      lv_hkont_aux = lw_tvarvc-low.
      CALL FUNCTION 'NUMERIC_CHECK'
        EXPORTING
          string_in  = lv_hkont_aux
        IMPORTING
          string_out = lv_hkont_aux
          htype      = lv_htype.

      IF lv_htype EQ 'NUMC'.
        APPEND INITIAL LINE TO lr_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
        <fs_conta>-sign   = lw_tvarvc-sign.
        <fs_conta>-option = lw_tvarvc-opti.
        <fs_conta>-low    = lv_hkont_aux.
      ENDIF.
    ENDLOOP.
  ENDIF.
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************

*    IF p_venc IS NOT INITIAL.
*      SELECT rk~vbeln rk~fkart rk~fktyp
*      FROM   vbrk  AS rk
*      INNER JOIN vbfa AS fa  ON fa~vbeln = rk~vbeln
*      INTO TABLE ti_vbrk
*      FOR ALL ENTRIES IN it_vbak
*      WHERE fa~vbelv = it_vbak-vbeln
*      AND  fa~vbtyp_n  = 'M'
*      AND  fa~vbtyp_v  = 'C'
*      AND   stufe      = '01'.
*
*
*      IF ti_vbrk IS NOT INITIAL.
*        IF p_venc-low IS NOT INITIAL.
*          DATA(z_ano) = |{ p_venc-low(4) }|.
*
*          DATA: log_system TYPE t000-logsys.
*          SELECT *
*          FROM bkpf
*          INTO TABLE @DATA(t_bkpf)
*          FOR ALL ENTRIES IN @ti_vbrk
*          WHERE awkey EQ @ti_vbrk-vbeln
**          AND AWSYS EQ @LOG_SYSTEM
*            AND gjahr EQ @z_ano
*            AND bukrs IN @p_bukrs.
*        ELSE.
*
*          SELECT *
*          FROM bkpf
*          INTO TABLE t_bkpf
*          FOR ALL ENTRIES IN ti_vbrk
*          WHERE awkey EQ ti_vbrk-vbeln
**          AND AWSYS EQ LOG_SYSTEM
*            AND bukrs IN p_bukrs.
*        ENDIF.
*
*        IF t_bkpf IS NOT INITIAL.
*          SELECT *
*           FROM bseg
*           INTO TABLE @DATA(t_bseg)
*             FOR ALL ENTRIES IN @t_bkpf
*             WHERE belnr EQ @t_bkpf-belnr
*               AND bukrs EQ @t_bkpf-bukrs
*               AND gjahr EQ @t_bkpf-gjahr
*               AND fdtag IN @p_venc
*               AND buzei EQ '01'.
*
*
*          IF t_bseg IS NOT INITIAL.
*            LOOP AT t_bseg INTO DATA(w_bseg).
*              READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY belnr = w_bseg-belnr
*                                                           bukrs = w_bseg-bukrs
*                                                           gjahr = w_bseg-gjahr.
*
*
*              IF sy-subrc EQ 0.
*                READ TABLE ti_vbrk INTO DATA(ws_vbrk) WITH KEY vbeln = w_bkpf-awkey.
*                IF sy-subrc EQ 0.
*                  APPEND VALUE #( sign = 'I' option = 'EQ' low = ws_vbrk-vbeln ) TO r_vbeln.
*                ENDIF.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      DELETE ADJACENT DUPLICATES FROM r_vbeln COMPARING low.
*
*      IF r_vbeln IS NOT INITIAL.
*        SELECT *
*        FROM vbfa
*        INTO TABLE @DATA(tl_vbfa)
*          WHERE vbeln IN @r_vbeln.
*
*        IF tl_vbfa IS NOT INITIAL.
**          r_vbelv =  VALUE #( FOR s IN tl_vbfa ( sign = 'I' option = 'EQ' low = s-vbelv ) ).
*        ENDIF.
*      ENDIF.
*
*      SORT r_vbelv BY low.
*      DELETE ADJACENT DUPLICATES FROM r_vbelv COMPARING low.
*
*      SELECT a~vbeln a~zterm a~kurrf a~valdt
*      FROM vbkd AS a
*      INNER JOIN vbak AS b ON a~vbeln  = b~vbeln
*      INTO TABLE it_vbkd
*        FOR ALL ENTRIES IN it_vbak
*        WHERE a~vbeln EQ it_vbak-vbeln
*        AND a~valdt  IN p_venc
*        AND a~zterm NE 'I006'
*        AND b~bukrs_vf IN p_bukrs
*        AND b~spart IN set_mi
*        AND b~vbeln IN p_vbeln
*        AND b~vkbur IN p_vkbur
*        AND b~kunnr IN p_kunnr
*        AND b~waerk IN c_waerk.
*
*      r_vbeln_aux =  VALUE #( FOR i IN it_vbkd ( sign = 'I' option = 'EQ' low = i-vbeln ) ).
*      DELETE ADJACENT DUPLICATES FROM r_vbeln_aux COMPARING low.
*      APPEND LINES OF r_vbeln_aux TO r_vbelv.
*
*      DELETE it_vbak WHERE vbeln NOT IN r_vbelv.
*    ELSE.
*
*      SELECT a~vbeln a~zterm a~kurrf a~valdt
*    FROM vbkd AS a
*    INNER JOIN vbak AS b ON a~vbeln  = b~vbeln
*    INTO TABLE it_vbkd
*      FOR ALL ENTRIES IN it_vbak
*      WHERE a~vbeln EQ it_vbak-vbeln
**      AND A~VALDT  IN P_VENC
*      AND a~zterm NE 'I006'
*      AND b~bukrs_vf IN p_bukrs
*      AND b~spart IN set_mi
*      AND b~vbeln IN p_vbeln
*      AND b~vkbur IN p_vkbur
*      AND b~kunnr IN p_kunnr
*      AND b~waerk IN c_waerk.
*    ENDIF.





ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
        xz_vlr_fortet   TYPE zfit0026-mont_moeda,
        xz_vlr_interna  TYPE zfit0026-mont_mi,
        xz_vlr_internat TYPE zfit0026-mont_mi,
        xtotalq_ov      TYPE vbap-kwmeng,
        xtotalvl_ov     TYPE vbap-netwr,
        vvalor          TYPE zfit0026-mont_moeda,
        pare            TYPE n LENGTH 3,
        vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

  SORT:  it_vbak          BY vbeln,
         it_vbap          BY vbeln,
         it_vbkd          BY vbeln,
         it_zfit0026      BY vbeln,
         it_kna1          BY kunnr,
         it_bsad          BY bukrs kunnr belnr ,
         it_t052u         BY zterm.


*  DATA(OBJ_AUART) = NEW ZCL_TAXA_CURVA( ).
*  R_DEVO_RECU = OBJ_AUART->GET_AUART( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa

  LOOP AT it_vbak INTO wa_vbak.
* Verifca a condição se OV ou Fatura
    SELECT SINGLE t~zdart
     FROM vbkd AS dk
     INNER JOIN vbak AS ak ON ak~vbeln = dk~vbeln
     INNER JOIN t052 AS t  ON t~zterm  = dk~zterm
     INTO @DATA(v_zdart)
    WHERE dk~vbeln = @wa_vbak-vbeln.


    IF v_zdart = 'B'.

      "carrega dados da faturas.
      SELECT rk~*
        FROM   vbrk  AS rk
        INNER JOIN vbfa AS fa  ON fa~vbeln = rk~vbeln
        INTO TABLE @DATA(it_vbrk)
       WHERE fa~vbelv = @wa_vbak-vbeln
        AND  fa~vbtyp_n  = 'M'
        AND  fa~vbtyp_v  = 'C'.
*        AND  RK~VBELN    = FA~VBELV.

      PERFORM f_construir_saida_faturas   TABLES it_vbrk USING  wa_vbak-vbeln.

    ENDIF.

    IF v_zdart <> 'B'.

      xtotalq_ov  = 0.
      xtotalvl_ov = 0.

*     INICIO MODIFICAÇÃO WELGEM
      CLEAR vbeln_aux.
      pare = 0.
      vbeln_aux = wa_vbak-vbeln.

      READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
      IF sy-subrc IS INITIAL.
        wa_saida-vbeln_p = wa_0041-vbeln.
        wa_saida-vbeln_s = wa_0041-doc_simulacao.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                       AND vbelv NE vbeln_aux.
          EXIT.
        ENDLOOP.

        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ENDIF.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        WHILE pare IS INITIAL.

          pare = 4.
          LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                         AND vbelv NE vbeln_aux.
            pare = 0.
            EXIT.
          ENDLOOP.

          IF pare IS INITIAL.
            vbeln_aux = wa_0090-vbelv.
          ENDIF.

          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
          ENDIF.

        ENDWHILE.
      ENDIF.

* INCLUINDO AS REMESSAS CS2016000023 >>>>>
      READ TABLE it_vbfa WITH KEY vbeln = vbeln_aux.
      IF sy-subrc IS INITIAL.
        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = it_vbfa-vbelv.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ELSE.
          READ TABLE it_0090 INTO wa_0090 WITH KEY vbeln = it_vbfa-vbelv.
          READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
          IF sy-subrc IS INITIAL.
            wa_saida-vbeln_p = wa_0041-vbeln.
            wa_saida-vbeln_s = wa_0041-doc_simulacao.
          ENDIF.
        ENDIF.
      ENDIF.
* INCLUINDO AS REMESSAS CS2016000023 <<<<<

* INCLUINDO O CAMPO SAFRA CS2016000023 >>>>>
      READ TABLE it_0040 WITH KEY doc_simulacao = wa_saida-vbeln_s.
      IF sy-subrc IS INITIAL.
        wa_saida-safra = it_0040-safra.
        wa_saida-id_order_ecommerce = it_0040-id_order_ecommerce."SMC #123881 - EQUALIZAÇÃO ECC X HANA

        "PAGAMENTO ANTECIPADO
        CASE it_0040-meio_pago .
          WHEN 'D' .
            wa_saida-pgto_ant = 'Deposito em Conta'.
          WHEN 'A' .
            wa_saida-pgto_ant = 'Acerto'.
          WHEN 'B' .
            wa_saida-pgto_ant = 'Boleto Bancário'.
          WHEN ' ' .
            wa_saida-pgto_ant = 'Não Atencipado'.
        ENDCASE.

        "CS CS2020000381
        READ TABLE it_zsdt0038 WITH KEY cultura = it_0040-cultura INTO DATA(wa_cultura) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-cultura = wa_cultura-descricao.
        ENDIF.

      ENDIF.

      " TAXA MULTA

*      WA_SAIDA-TX_MULTA = IT_0040-JUROS_ANO . " Comentado porque não tem esse campo no simulador.
      wa_saida-tx_juros = it_0040-juros_ano .

      " END TAXA MULTA


      CLEAR it_0040.
* INCLUINDO O CAMPO SAFRA CS2016000023 <<<<<<

*      FIM MODIFICAÇÃO WELGEM
      LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.
        ADD wa_vbap-kwmeng TO xtotalq_ov.
        ADD wa_vbap-netwr TO xtotalvl_ov.
        ADD wa_vbap-mwsbp TO xtotalvl_ov.
        ADD wa_vbap-netwr TO wa_saida-netwr_l.
        ADD wa_vbap-mwsbp TO wa_saida-mwsbp.
      ENDLOOP.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 >>>>>>
*    CASE WA_VBAK-AUART.
*      WHEN 'ZRPF' OR 'ZROB' OR 'ZREB'.
*        XTOTALQ_OV  = XTOTALQ_OV  * -1.
*        XTOTALVL_OV = XTOTALVL_OV * -1.
*        WA_SAIDA-NETWR_L = WA_SAIDA-NETWR_L * -1.
*        WA_SAIDA-MWSBP = WA_SAIDA-MWSBP * -1.
*
*      WHEN 'ZREM' OR 'ZRFU'.
*        XTOTALQ_OV  = 0.
*        XTOTALVL_OV = 0.
*    ENDCASE.

*    IF WA_VBAK-AUART IN R_DEVO_RECU.
*&------------Inicio ajuste CS2024000717 / AOENNING &*
      "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.
      SELECT SINGLE * FROM tvarvc INTO  @DATA(wa_ov_devolucao)
        WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
         AND  low  EQ @wa_vbak-auart.
*      IF wa_vbak-auart EQ 'ZRPF'  OR  wa_vbak-auart EQ 'ZROB'  OR  wa_vbak-auart EQ 'ZREB'.
      IF sy-subrc EQ 0.
*&------------Inicio ajuste CS2024000717 / AOENNING &*
        xtotalq_ov  = xtotalq_ov  * -1.
        xtotalvl_ov = xtotalvl_ov * -1.
        wa_saida-netwr_l = wa_saida-netwr_l * -1.
        wa_saida-mwsbp = wa_saida-mwsbp * -1.

      ELSEIF  wa_vbak-auart EQ 'ZREM' OR  wa_vbak-auart EQ 'ZRFU'.
        xtotalq_ov  = 0.
        xtotalvl_ov = 0.
      ENDIF.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<
      READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      READ TABLE it_kna1  INTO wa_kna1  WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.

      wa_saida-data_venc  = wa_vbkd-valdt.

      READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm BINARY SEARCH.

      wa_saida-werks      = wa_vbap-werks.

      IF wa_vbak-vkgrp IS NOT INITIAL.
        SELECT SINGLE bezei
      FROM tvgrt
      INTO @DATA(z_bezei)
       WHERE spras EQ @sy-langu
         AND vkgrp EQ @wa_vbak-vkgrp.
        wa_saida-vkgrp = wa_vbak-vkgrp && '-' && z_bezei.
      ENDIF.

*      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
      wa_saida-vkbur      = wa_vbak-vkbur.
      wa_saida-kunnr      = wa_vbak-kunnr.
      wa_saida-name1      = wa_kna1-name1.
      wa_saida-auart      = wa_vbak-auart.
      wa_saida-zterm      = wa_vbkd-zterm.
      wa_saida-text1      = wa_t052u-text1.
      wa_saida-vbeln      = wa_vbak-vbeln.
      wa_saida-vbeln_g    = wa_vbak-vbeln.
      wa_saida-erdat      = wa_vbak-erdat.
      wa_saida-waerk      = wa_vbak-waerk.

      xz_vlr_forte   = 0.
      xz_vlr_interna = 0.

      LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln.
        MOVE: wa_zfit0026-observacao TO wa_saida-observacao.
        ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
        ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.
      ENDLOOP.

      "QTDE FATURADO NFe
      DATA(t_zfit0026_group_by_doc_fatura) = it_zfit0026[].

      SORT t_zfit0026_group_by_doc_fatura BY doc_fatura.
      DELETE ADJACENT DUPLICATES FROM t_zfit0026_group_by_doc_fatura COMPARING doc_fatura.

      LOOP AT t_zfit0026_group_by_doc_fatura INTO  DATA(wa_zfit26_group_by_doc_fatura) WHERE  vbeln = wa_vbak-vbeln.
        IF wa_zfit26_group_by_doc_fatura-doc_fatura IS NOT INITIAL .
          SELECT SUM( fkimg )
          FROM vbrp
          INTO @DATA(qtde_faturado_nfe)
         WHERE vbeln = @wa_zfit26_group_by_doc_fatura-doc_fatura.
        ENDIF.

        ADD qtde_faturado_nfe TO wa_saida-fkimg.

      ENDLOOP.
      "END QTDE FATURA.

* Salva totais
      xz_vlr_fortet   = xz_vlr_forte.
      xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
      IF wa_vbak-waerk = 'USD'.
        xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
        xz_vlr_interna = 0.
      ELSE.
        xz_vlr_forte = 0.
        xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
      ENDIF.

      vvalor = xtotalq_ov.
      wa_saida-totalq_ov = vvalor.
      vvalor = xtotalvl_ov.
      wa_saida-totvl_ov = vvalor.

      DATA: vflag TYPE i,
            vcont TYPE i.
      vflag = 0.
      vcont = 0.


      LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln.
        wa_saida-vlr_multa_calc = wa_zfit0026-vlr_multa_calc.  "MULTA CALCULADO
        wa_saida-vlr_multa_rbdo = wa_zfit0026-vlr_multa_rbdo.  "MULTA RECEBIDA
        wa_saida-vlr_juros_calc = wa_zfit0026-vlr_juros_calc.  "JUROS CALCULADO
        wa_saida-vlr_juros_rbdo = wa_zfit0026-vlr_juros_rbdo. "JUROS RECEBIDO
        wa_saida-vlr_desc_mult  = wa_zfit0026-vlr_desc_mult.
        wa_saida-vlr_desc_jros  = wa_zfit0026-vlr_desc_jros.

        IF wa_saida-waerk EQ 'USD'.
          wa_saida-vlr_sald_fin = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
        ELSE.
          wa_saida-vlr_sald_fin_brl = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
        ENDIF.

        wa_saida-ptax = wa_zfit0026-taxa. "PTAX
        MOVE: wa_zfit0026-observacao TO wa_saida-observacao.

        ADD 1 TO vcont.
        READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                   kunnr = wa_vbak-kunnr
                                                   belnr = wa_zfit0026-docnum.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.

          READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                     kunnr = wa_vbak-kunnr
                                                     belnr = wa_z0159-adiant.

          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_zibchv INTO wa_zibchv WITH KEY obj_key = wa_zfit0026-obj_key.

            READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                       kunnr = wa_vbak-kunnr
                                                       belnr = wa_zibchv-belnr.
          ENDIF.

        ENDIF.

        FREE: it_color, t_cor.

        IF sy-subrc IS INITIAL.
          wa_saida-augbl     = wa_bsad-augbl.
*        WA_SAIDA-BUDAT     = WA_BSAD-BUDAT.
          wa_saida-budat     = wa_bsad-augdt.
*          WA_SAIDA-DMBE2     = WA_BSAD-DMBE2.
*          WA_SAIDA-DMBTR     = WA_BSAD-DMBTR.
          wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          wa_saida-dmbtr      = ( wa_zfit0026-mont_moeda * wa_zfit0026-taxa ).

          t_cor =
          VALUE #(
                   ( fname = 'AUGBL' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'BUDAT' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBE2' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBTR' color-col = '5' color-int = '1' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.

        ELSE.
          wa_saida-dmbe2     = 0.
          wa_saida-dmbtr     = 0.
          wa_saida-augbl     = ''.
        ENDIF.

        wa_saida-forma_pag  = wa_zfit0026-forma_pag.
        wa_saida-taxa       = wa_zfit0026-taxa.
        wa_saida-mont_moeda = COND #( WHEN wa_vbak-waerk EQ 'USD' THEN wa_zfit0026-mont_moeda ELSE 0 ).
        wa_saida-mont_mi = COND #( WHEN wa_vbak-waerk EQ 'BRL' THEN wa_zfit0026-mont_mi ELSE 0 ).
        wa_saida-docnum     = wa_zfit0026-docnum.

        IF wa_saida-docnum IS INITIAL.
          IF wa_z0159-adiant IS NOT INITIAL.
            wa_saida-docnum = wa_z0159-adiant.
          ELSE.
            wa_saida-docnum = wa_zibchv-belnr.
          ENDIF.
        ENDIF.

        t_cor =
        VALUE #(
                 ( fname = 'FORMA_PAG'  color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'TAXA'       color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'MONT_MOEDA' color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'MONT_MI'    color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'DOCNUM'     color-col = '5' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        IF vcont = 1.
          wa_saida-moeda_forte = xz_vlr_forte.
          wa_saida-moeda_inter = xz_vlr_interna.

          t_cor =
          VALUE #(
                   ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.

        ELSE.
          wa_saida-moeda_forte = 0.
          wa_saida-moeda_inter = 0.

          t_cor =
          VALUE #(
                   ( fname = 'VLR_SALD_FIN' color-col = '2' color-int = '0' color-inv = '1' )
                   ( fname = 'MOEDA_FORTE' color-col = '2' color-int = '0' color-inv = '1' )
                   ( fname = 'MOEDA_INTER' color-col = '2' color-int = '0' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.

          CLEAR wa_saida-line_color.
        ENDIF.

        wa_saida-salus = ( wa_saida-mont_moeda + wa_saida-moeda_forte ) - wa_saida-dmbe2.
        wa_saida-salre = ( wa_saida-mont_mi    + wa_saida-moeda_inter ) - wa_saida-dmbtr.
        wa_saida-color_cell[] = it_color[].

        IF rb1 = 'X'.
          IF wa_saida-augbl IS NOT INITIAL.
            APPEND wa_saida TO it_saida.
            vflag = 1.
          ENDIF.
        ELSEIF rb2 = 'X'.
          IF ( wa_saida-waerk = 'USD' AND xz_vlr_forte NE 0 ) OR ( wa_saida-waerk = 'BRL' AND xz_vlr_interna NE 0 ).
            APPEND wa_saida TO it_saida.
          ENDIF.
        ELSE.
          APPEND wa_saida TO it_saida.
        ENDIF.
        wa_saida-augbl     = ''.

        CLEAR: wa_z0159, wa_bsad, wa_zibchv.
      ENDLOOP.

      CLEAR: wa_saida-forma_pag, wa_saida-taxa, wa_saida-docnum, wa_saida-augbl, wa_saida-color_cell[].
      REFRESH it_color.

      IF vcont = 0.
        wa_saida-mont_moeda  = 0.
        wa_saida-mont_mi     = 0.
        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.
        wa_saida-augbl     = wa_bsad-augbl.
        wa_saida-moeda_forte = xz_vlr_forte.
        wa_saida-moeda_inter = xz_vlr_interna.

        t_cor =
        VALUE #(
               ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
                ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        wa_saida-color_cell[] = it_color[].
        wa_saida-line_color  = 'C310'.

        READ TABLE it_zfit0026 INTO wa_zfit0026 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          wa_saida-salus = wa_saida-moeda_forte.
          wa_saida-salre = wa_saida-moeda_inter.
        ENDIF.

*        IF RB1 = 'X'.
*          IF VFLAG = 1.
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSEIF RB2 = 'X'.
*          IF ( WA_SAIDA-WAERK = 'USD' AND WA_SAIDA-MOEDA_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND WA_SAIDA-MOEDA_INTER NE 0 ).
*            APPEND WA_SAIDA TO IT_SAIDA.
*          ENDIF.
*        ELSE.
        APPEND wa_saida TO it_saida.
*        ENDIF.
      ENDIF.

      CLEAR: wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_saida, wa_color, wa_0090, wa_0090x, wa_0041.
      FREE it_color.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
    <f_saida>-rfmng = REDUCE rfmng( INIT x TYPE rfmng FOR ls IN it_remessa WHERE ( vbelv EQ <f_saida>-vbeln_g ) NEXT x = x + ls-rfmng ).
  ENDLOOP.

  CHECK rb2 EQ abap_true.

  LOOP AT it_saida INTO wa_saida.
    wa_delete =
    VALUE #(
             vbeln_s     = wa_saida-vbeln_s
             vbeln_p     = wa_saida-vbeln_p
             moeda_forte = wa_saida-moeda_forte
             moeda_inter = wa_saida-moeda_inter
           ).

    COLLECT wa_delete INTO it_delete.
    CLEAR wa_delete.

  ENDLOOP.

  DELETE it_delete WHERE moeda_forte > 10 OR  moeda_inter > 10.

  LOOP AT it_delete INTO wa_delete.
    DELETE it_saida WHERE vbeln_s EQ wa_delete-vbeln_s AND
                          vbeln_p EQ wa_delete-vbeln_p.
  ENDLOOP.


ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida_mi .

  DATA: saldo_ov TYPE bsad-dmbtr.
  DATA: nr_ov TYPE zfit0026-vbeln.

  DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
        xz_vlr_fortet   TYPE zfit0026-mont_moeda,
        xz_vlr_interna  TYPE zfit0026-mont_mi,
        xz_vlr_internat TYPE zfit0026-mont_mi,
        xtotalq_ov      TYPE vbap-kwmeng,
        xtotalvl_ov     TYPE vbap-netwr,
        vvalor          TYPE zfit0026-mont_moeda,
        pare            TYPE n LENGTH 3,
        vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

  SORT:  it_vbak          BY vbeln,
         it_vbap          BY vbeln,
         it_vbkd          BY vbeln,
         it_zfit0026      BY vbeln,
         it_kna1          BY kunnr,
         it_bsad          BY bukrs kunnr belnr ,
         it_t052u         BY zterm.


  SORT it_zfit0026 BY vbeln.
  DATA: linha_princ_ov.
  LOOP AT it_vbak INTO wa_vbak.

    IF wa_vbak-vbeln IS NOT INITIAL.
      linha_princ_ov = abap_true. "Indicando linha principal com informações da OV.
    ENDIF.

* Verifca a condição se OV ou Fatura
    SELECT SINGLE t~zdart
     FROM vbkd AS dk
     INNER JOIN vbak AS ak ON ak~vbeln = dk~vbeln
     INNER JOIN t052 AS t  ON t~zterm  = dk~zterm
     INTO @DATA(v_zdart)
    WHERE dk~vbeln = @wa_vbak-vbeln.

*
*    IF V_ZDART = 'B'.
*
*      "carrega dados da faturas.
*      SELECT RK~*
*        FROM   VBRK  AS RK
*        INNER JOIN VBFA AS FA  ON FA~VBELN = RK~VBELN
*        INTO TABLE @DATA(IT_VBRK_MI)
*       WHERE FA~VBELV = @WA_VBAK-VBELN
*        AND  FA~VBTYP_N  = 'M'
*        AND  FA~VBTYP_V  = 'C'.
*
*      PERFORM F_CONSTRUIR_SAIDA_FATURAS_MI.  "TABLES IT_VBRK_MI  USING  WA_VBAK-VBELN .
*
*      CLEAR: WA_SAIDA-FORMA_PAG, WA_SAIDA-TAXA, WA_SAIDA-DOCNUM, WA_SAIDA-AUGBL, WA_SAIDA-COLOR_CELL[].
*      REFRESH IT_COLOR.
*    ENDIF.

*    IF V_ZDART <> 'B'.

    xtotalq_ov  = 0.
    xtotalvl_ov = 0.


    CLEAR vbeln_aux.
    pare = 0.
    vbeln_aux = wa_vbak-vbeln.


    SELECT *
      FROM zsdt0053 INTO TABLE @DATA(it_zsdt0053)
     WHERE vbeln = @vbeln_aux.

    LOOP AT it_zsdt0053 INTO DATA(wa_zsdt0053).

      wa_saida-vbeln_p =   wa_zsdt0053-vbeln.
      wa_saida-vbeln_s =   wa_zsdt0053-nro_sol_ov.
      wa_saida-vbeln_g =   wa_zsdt0053-vbeln.
      wa_saida-safra   =   wa_zsdt0053-charg.

    ENDLOOP.

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.

      ADD wa_vbap-kwmeng TO xtotalq_ov.

      ADD wa_vbap-netwr TO xtotalvl_ov.
      ADD wa_vbap-mwsbp TO xtotalvl_ov.

      ADD wa_vbap-netwr TO wa_saida-netwr_l.
      ADD wa_vbap-mwsbp TO wa_saida-mwsbp.

    ENDLOOP.

    "PAGAMENTO ANTECIPADO
    SELECT SINGLE *
      FROM zsdt0052 INTO @DATA(wa_zsdt0052)
      WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

    CASE wa_zsdt0052-pgto_ant .

      WHEN 'X' .
        wa_saida-pgto_ant = ' Com Boleto '.

      WHEN 'N' .
        wa_saida-pgto_ant = ' Sem Boleto '.

      WHEN ' ' .
        wa_saida-pgto_ant = ' Não Antecipado '.

    ENDCASE.

    " TAXA MULTA

    SELECT SINGLE *
      FROM zsdt0051 INTO @DATA(wa_zsdt0051)
      WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

    wa_saida-tx_multa = wa_zsdt0051-tx_multa.
    wa_saida-tx_juros = wa_zsdt0051-tx_juros.

*&------------Inicio ajuste CS2024000717 / AOENNING &*
    "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.
    SELECT SINGLE * FROM tvarvc INTO  @DATA(wa_ov_devolucao)
      WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
       AND  low  EQ @wa_vbak-auart.
*      IF wa_vbak-auart EQ 'ZRPF'  OR  wa_vbak-auart EQ 'ZROB'  OR  wa_vbak-auart EQ 'ZREB'.
    IF sy-subrc EQ 0.
*&------------Fim ajuste CS2024000717 / AOENNING &*
      xtotalq_ov  = xtotalq_ov  * -1.
      xtotalvl_ov = xtotalvl_ov * -1.

      wa_saida-netwr_l = wa_saida-netwr_l * -1.
      wa_saida-mwsbp = wa_saida-mwsbp * -1.

    ELSEIF  wa_vbak-auart EQ 'ZREM' OR  wa_vbak-auart EQ 'ZRFU'.
      xtotalq_ov  = 0.
      xtotalvl_ov = 0.
    ENDIF.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<

    READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    READ TABLE it_kna1  INTO wa_kna1  WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
    READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
*      READ TABLE IT_VBFA_AUXI INTO W_VBFA_AUXI WITH KEY  VBELN = WA_VBRK-VBELN


*      READ TABLE IT_J_1BNFDOC INTO DATA(W_J_1BNFDOC) WITH KEY REFKEY = W_VBFA_AUXI-REFKEY
    wa_saida-data_venc  = wa_vbkd-valdt.
    wa_saida-data_venc_2  = wa_vbkd-valdt.

    READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm BINARY SEARCH.

    SELECT SINGLE bezei
  FROM tvgrt
  INTO @DATA(z_bezei)
   WHERE spras EQ @sy-langu
     AND vkgrp EQ @wa_vbak-vkgrp.
    wa_saida-vkgrp = wa_vbak-vkgrp && '-' && z_bezei.

    wa_saida-werks      = wa_vbap-werks. "Centro
*      WA_SAIDA-VKGRP      = WA_VBAK-VKGRP.
    wa_saida-vkbur      = wa_vbak-vkbur.  "Equip venda
    wa_saida-kunnr      = wa_vbak-kunnr.  "Emissor da ordem
    wa_saida-name1      = wa_kna1-name1.  "Nome 1
    wa_saida-auart      = wa_vbak-auart.  "Tipo de ordem
    wa_saida-zterm      = wa_vbkd-zterm.  "Chave de condições de pagamento
    wa_saida-text1      = wa_t052u-text1.  "Explicação própria para as condições de pagamento

    wa_saida-vbeln      = wa_vbak-vbeln.  "Documento de vendas
    wa_saida-vbeln_g    = wa_vbak-vbeln.  "Documento de vendas

    wa_saida-erdat      = wa_vbak-erdat.  "Data de criação do registro
    wa_saida-waerk      = wa_vbak-waerk.  "Moeda do documento SD

    xz_vlr_forte   = 0.
    xz_vlr_interna = 0.


    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln.

      MOVE: wa_zfit0026-observacao TO wa_saida-observacao.
      ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
      ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.

    ENDLOOP.

    "QTDE FATURADO NFe
    DATA(t_zfit0026_group_by_doc_fatura) = it_zfit0026[].

    SORT t_zfit0026_group_by_doc_fatura BY doc_fatura.
    DELETE ADJACENT DUPLICATES FROM t_zfit0026_group_by_doc_fatura COMPARING doc_fatura.


    LOOP AT t_zfit0026_group_by_doc_fatura INTO  DATA(wa_zfit26_group_by_doc_fatura) WHERE  vbeln = wa_vbak-vbeln.

      IF wa_zfit26_group_by_doc_fatura-doc_fatura IS NOT INITIAL .
        SELECT SUM( fkimg )
        FROM vbrp
        INTO @DATA(qtde_faturado_nfe)
       WHERE vbeln = @wa_zfit26_group_by_doc_fatura-doc_fatura.
      ENDIF.

      ADD qtde_faturado_nfe TO wa_saida-fkimg.
      CLEAR: qtde_faturado_nfe.
    ENDLOOP.
    "END QTDE FATURA.

* Salva totais
    xz_vlr_fortet   = xz_vlr_forte.
    xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
    IF wa_vbak-waerk = 'USD'.
      xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
      xz_vlr_interna = 0.
    ELSE.
      xz_vlr_forte = 0.
      xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
    ENDIF.

    vvalor = xtotalq_ov.
    wa_saida-totalq_ov = vvalor.

    vvalor = xtotalvl_ov.
    wa_saida-totvl_ov = vvalor.

    DATA: vflag TYPE i,
          vcont TYPE i.
    vflag = 0.
    vcont = 0.

    CLEAR: saldo_ov, nr_ov.

    IF linha_princ_ov NE abap_true.

      LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln  .

        READ TABLE it_zsdt0053 INTO DATA(w_zsdt0053) WITH KEY vbeln = wa_zfit0026-vbeln.
        IF sy-subrc EQ 0.
          wa_saida-vbeln_s = w_zsdt0053-nro_sol_ov.
        ENDIF.

        READ TABLE it_zsdt0051 INTO DATA(w_zsdt0051) WITH KEY nro_sol_ov = w_zsdt0053-nro_sol_ov.
        CLEAR: z_bezei.
        IF sy-subrc EQ 0.
          SELECT SINGLE bezei
        FROM tvgrt
        INTO z_bezei
         WHERE spras EQ sy-langu
           AND vkgrp EQ w_zsdt0051-vkgrp.

          wa_saida-vkbur = w_zsdt0051-vkbur.
          wa_saida-vkgrp = w_zsdt0051-vkgrp && '-' && z_bezei.
        ENDIF.


        wa_saida-vlr_multa_calc = wa_zfit0026-vlr_multa_calc.  "MULTA CALCULADO
        wa_saida-vlr_multa_rbdo = wa_zfit0026-vlr_multa_rbdo.  "MULTA RECEBIDA
        wa_saida-vlr_desc_mult  = wa_zfit0026-vlr_desc_mult.
        wa_saida-vlr_desc_jros  = wa_zfit0026-vlr_desc_jros.

        wa_saida-vlr_juros_calc = wa_zfit0026-vlr_juros_calc.  "JUROS CALCULADO
        wa_saida-vlr_juros_rbdo = wa_zfit0026-vlr_juros_rbdo. "JUROS RECEBIDO

*        WA_SAIDA-VLR_SALD_FIN = ( WA_ZFIT0026-VLR_JUROS_CALC - WA_ZFIT0026-VLR_JUROS_RBDO - WA_ZFIT0026-VLR_DESC_JROS ) + ( WA_ZFIT0026-VLR_MULTA_CALC - WA_ZFIT0026-VLR_MULTA_RBDO - WA_ZFIT0026-VLR_DESC_MULT ).

        IF wa_saida-waerk EQ 'USD'.
          wa_saida-vlr_sald_fin = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
        ELSE.
          wa_saida-vlr_sald_fin_brl = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
        ENDIF.


        wa_saida-ptax = wa_zfit0026-taxa. "PTAX
        wa_saida-data_pgto = wa_zfit0026-data_pgto.
        wa_saida-mont_rbdo = wa_zfit0026-mont_rbdo.
        wa_saida-data_venc = wa_zfit0026-data_venc.

        MOVE: wa_zfit0026-observacao TO wa_saida-observacao.

        ADD 1 TO vcont.
        READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                   kunnr = wa_vbak-kunnr
                                                   belnr = wa_zfit0026-docnum.
        IF sy-subrc IS NOT INITIAL.

*          READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.
*
*          READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
*                                         kunnr = wa_vbak-kunnr
*                                         belnr = wa_z0159-adiant.

          READ TABLE it_zibchv INTO wa_zibchv WITH KEY obj_key = wa_zfit0026-obj_key.

          READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                     kunnr = wa_vbak-kunnr
                                                     belnr = wa_zibchv-belnr.



        ENDIF.

        FREE: it_color, t_cor.

        IF sy-subrc IS INITIAL.

          wa_saida-augbl     = wa_bsad-augbl.
*        WA_SAIDA-BUDAT     = WA_BSAD-BUDAT.
          wa_saida-budat     = wa_bsad-augdt.
          wa_saida-dmbe2     = wa_bsad-dmbe2.
          wa_saida-dmbtr     = wa_bsad-dmbtr.
*          WA_SAIDA-DMBTR     = ZFIT0026-MONT_MOEDA.

          t_cor =
          VALUE #(
                   ( fname = 'AUGBL' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'BUDAT' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBE2' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DMBTR' color-col = '5' color-int = '1' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.

        ELSE.
          wa_saida-dmbe2     = 0.
          wa_saida-dmbtr     = 0.
          wa_saida-augbl     = ''.
        ENDIF.

        wa_saida-forma_pag  = wa_zfit0026-forma_pag.
        wa_saida-taxa       = wa_zfit0026-taxa.
        wa_saida-mont_moeda = COND #( WHEN wa_vbak-waerk EQ 'USD' THEN wa_zfit0026-mont_moeda ELSE 0 ).
        wa_saida-mont_mi = COND #( WHEN wa_vbak-waerk EQ 'BRL' THEN wa_zfit0026-mont_mi ELSE 0 ).

        wa_saida-docnum     = wa_zfit0026-docnum.

        IF wa_saida-docnum IS INITIAL.
*          wa_saida-docnum = wa_z0159-adiant.
          wa_saida-docnum = wa_zibchv-belnr.
        ENDIF.

        t_cor =
        VALUE #(
                 ( fname = 'FORMA_PAG'  color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'TAXA'       color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'MONT_MOEDA' color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'MONT_MI'    color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'DOCNUM'     color-col = '5' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        IF vcont = 1.

          wa_saida-moeda_forte = xz_vlr_forte.
          wa_saida-moeda_inter = xz_vlr_interna.



          t_cor =
          VALUE #(
                   ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
                   ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.
        ELSE.

          wa_saida-moeda_forte = 0.
          wa_saida-moeda_inter = 0.

          t_cor =
          VALUE #(
                   ( fname = 'MOEDA_FORTE' color-col = '2' color-int = '0' color-inv = '1' )
                   ( fname = 'MOEDA_INTER' color-col = '2' color-int = '0' color-inv = '1' )
                 ).
          APPEND LINES OF t_cor TO it_color.

          CLEAR wa_saida-line_color.
        ENDIF.


        wa_saida-salus = ( wa_saida-mont_moeda + wa_saida-moeda_forte ) - wa_saida-dmbe2.
        wa_saida-salre = ( wa_saida-mont_mi    + wa_saida-moeda_inter ) - wa_saida-dmbtr.

        wa_saida-color_cell[] = it_color[].

        IF rb1 = 'X'.
          IF wa_saida-augbl IS NOT INITIAL.
            APPEND wa_saida TO it_saida.
            vflag = 1.
          ENDIF.
        ELSEIF rb2 = 'X'.
          IF ( wa_saida-waerk = 'USD' AND xz_vlr_forte NE 0 ) OR ( wa_saida-waerk = 'BRL' AND xz_vlr_interna NE 0 ).
            APPEND wa_saida TO it_saida.
          ENDIF.
        ELSE.
          APPEND wa_saida TO it_saida.
        ENDIF.
        wa_saida-augbl     = ''.

        CLEAR: wa_z0159, wa_bsad, wa_zibchv.
      ENDLOOP.
    ENDIF.

    CLEAR: wa_saida-forma_pag, wa_saida-taxa, wa_saida-docnum, wa_saida-augbl, wa_saida-color_cell[].
    REFRESH it_color.

    IF vcont = 0.

      READ TABLE it_zsdt0053 INTO w_zsdt0053 WITH KEY vbeln = wa_vbak-vbeln.
      IF sy-subrc EQ 0.
        wa_saida-vbeln_s = w_zsdt0053-nro_sol_ov. "Numero de Solicitação de Ordem de Venda
      ENDIF.

      READ TABLE it_zsdt0051 INTO w_zsdt0051 WITH KEY nro_sol_ov = w_zsdt0053-nro_sol_ov.
      IF sy-subrc EQ 0.
        SELECT SINGLE bezei
      FROM tvgrt
      INTO z_bezei
       WHERE spras EQ sy-langu
         AND vkgrp EQ w_zsdt0051-vkgrp.

        wa_saida-vkbur = w_zsdt0051-vkbur.
        wa_saida-vkgrp = w_zsdt0051-vkgrp && '-' && z_bezei. "Equipe de venda.
      ENDIF.


      wa_saida-mont_moeda  = 0.
      wa_saida-mont_mi     = 0.
      wa_saida-moeda_forte = 0.
      wa_saida-moeda_inter = 0.
      wa_saida-augbl       = wa_bsad-augbl. "Nº documento de compensação
      wa_saida-moeda_forte = xz_vlr_forte.  "Valor líquido na moeda do documento
*        IF NR_OV NE WA_VBAK-VBELN.
      wa_saida-moeda_inter = xz_vlr_interna. "Valor líquido na moeda do documento
*        ENDIF.

      t_cor =
      VALUE #(
              ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
              ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
             ).
      APPEND LINES OF t_cor TO it_color.

      wa_saida-color_cell[] = it_color[].
      wa_saida-line_color  = 'C310'.


      READ TABLE it_zfit0026 INTO wa_zfit0026 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.

        wa_saida-salus = wa_saida-moeda_forte. "Valor líquido na moeda do documento
*          IF NR_OV NE WA_VBAK-VBELN.
        wa_saida-salre = wa_saida-moeda_inter. "Valor líquido na moeda do documento
*          ENDIF.
*          NR_OV = NR_OV.
      ENDIF.

      IF rb1 = 'X'.
        IF vflag = 1.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ELSEIF rb2 = 'X'.
        IF ( wa_saida-waerk = 'USD' AND wa_saida-moeda_forte NE 0 ) OR ( wa_saida-waerk = 'BRL' AND wa_saida-moeda_inter NE 0 ).
          APPEND wa_saida TO it_saida.
        ENDIF.
      ELSE.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ENDIF.

    CLEAR: linha_princ_ov, wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_saida, wa_color, wa_0090, wa_0090x, wa_0041.
    FREE it_color.

*    ENDIF.
  ENDLOOP.


*
*ATUALIZA VALORES DA IT_SAIDA
  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
    <f_saida>-rfmng = REDUCE rfmng( INIT x TYPE rfmng FOR ls IN it_remessa WHERE ( vbelv EQ <f_saida>-vbeln_g ) NEXT x = x + ls-rfmng ).
  ENDLOOP.

  CHECK rb2 EQ abap_true.

  LOOP AT it_saida INTO wa_saida.

    wa_delete =
    VALUE #(
             vbeln_s     = wa_saida-vbeln_s
             vbeln_p     = wa_saida-vbeln_p
             moeda_forte = wa_saida-moeda_forte
             moeda_inter = wa_saida-moeda_inter
           ).

    COLLECT wa_delete INTO it_delete.
    CLEAR wa_delete.

  ENDLOOP.

  DELETE it_delete WHERE moeda_forte > 10 OR  moeda_inter > 10.

  LOOP AT it_delete INTO wa_delete.
    DELETE it_saida WHERE vbeln_s EQ wa_delete-vbeln_s AND
                          vbeln_p EQ wa_delete-vbeln_p.
  ENDLOOP.



ENDFORM.                    " F_SAIDA_MI
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  FREE: it_fcat.

  IF it_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.



  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

  gs_variant_c-variant = p_varia.

  IF gs_variant_c-variant IS INITIAL.
    gs_variant_c-variant = '/'.
  ENDIF.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_variant               = gs_variant_c
      i_callback_user_command  = 'USER_COMMAND1'
      i_callback_pf_status_set = 'F_PF_STANDARD_FULLSCREEN'
      is_layout                = gd_layout
      it_fieldcat              = it_fcat[]
      it_sort                  = t_sort[]
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
*     IS_VARIANT               = VG_VARIANT
    TABLES
      t_outtab                 = it_saida.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  "check sy-uname ne 'RBLIMA'. """#teste apagar ramon

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.

ENDFORM. "X_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_1553   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .
  READ TABLE it_saida INTO DATA(w_saida) INDEX 1.

  PERFORM alv_preenche_cat USING:
           'VKBUR'                TEXT-004        '06'       ' '     ' '    ' ' , " Escr.Venda
           'VKGRP'                TEXT-039        '06'       ' '     ' '    ' ' , " Equipe Vendas
           " 09.12.2024 - RAMON - 160480 -->
           'BEZEI'                TEXT-t39        '06'       ' '     ' '    ' ' , " Equipe Vendas Descrição
           " 09.12.2024 - RAMON - 160480 --<
           'VBELN_S'              TEXT-028        '12'       'X'     ' '    ' ' , " Nr Simulador
           'id_order_ecommerce'   TEXT-060        '12'       ''      ' '    ' ' , " E-commerce - "SMC #123881 - EQUALIZAÇÃO ECC X HANA
           'VBELN_P'              TEXT-027        '12'       ' '     ' '    ' ' , " Nro OV Principal / N° OV Principal
           'VBELN_G'              TEXT-009        '14'       'X'     ' '    ' ' , " Nro OV  /   N° OV Desmembrada

           " 28.07.2023 - RAMON 98680 -->
*           'VBELV_AGP'            text-059        '14'       'X'     ' '    ' ' , " VBELV_AGP
           " 28.07.2023 - RAMON 98680 --<

           'AUART'                TEXT-007        '10'       ' '     ' '    ' ' , " Tipo O.V.
           'SAFRA'                TEXT-034        '10'       ' '     ' '    ' ' . " Safra

  IF rb7 EQ abap_true.
    PERFORM alv_preenche_cat USING:
             'CULTURA'            TEXT-058        '10'       ' '     ' '    ' ' , "Cultura
             'NUM_COMP_ADIANT'    TEXT-061        '10'       ' '     ' '    ' '. "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
  ENDIF.

  PERFORM alv_preenche_cat USING:
           'KUNNR'                TEXT-005        '10'       ' '     ' '    ' ' , " Cod.cliente
           'NAME1'                TEXT-006        '30'       ' '     ' '    ' ' , " Cliente
           'PGTO_ANT'             TEXT-040        '30'       ' '     ' '    ' ' , " Pgto Antecipado
           'TEXT1'                TEXT-008        '20'       ' '     ' '    ' ' , " Cond. Pgto
           'TX_MULTA'             TEXT-041        '20'       ' '     ' '    ' ' , " Taxa Multa
           'TX_JUROS'             TEXT-042        '20'       ' '     ' '    ' ' , " Taza Juros a.a
           'ERDAT'                TEXT-010        '11'       ' '     ' '    ' ' , " Data da OV
           'WAERK'                TEXT-011        '06'       ' '     ' '    ' ' , " Moeda
           'TOTALQ_OV'            TEXT-012        '13'       ' '     ' '    'X' , " Quantidade                  **
           'RFMNG'                TEXT-037        '13'       ' '     ' '    'X' , "Quantidade de Faturado       **
           'REFERENCIA_NFE'       TEXT-049        '13'       ' '     ' '    '' ,  "Referencia Nfe
*           'VLR_REFERENCIA'       TEXT-050        '18'       ' '     ' '    '' ,  "Valor Referencia Nfe
           'FKIMG'                TEXT-043        '13'       ' '     ' '    'X' , "Quantidade de Faturado NFe
           'NETWR_L'              TEXT-035        '13'       ' '     ' '    'X' , " Vlr Liquido                 **
           'MWSBP'                TEXT-036        '13'       ' '     ' '    ' ' , " Vlr IMposto                 **
           'TOTVL_OV'             TEXT-013        '15'       ' '     ' '    'X' , " Vlr.Total OV                **
           'VLR_TOT_REF'          TEXT-055        '15'       ' '     ' '    'X' , " Vlr.Total Ref                **
           'DATA_VENC'            TEXT-014        '11'       ' '     ' '    ' ' , " Dt.Vcto. / Dt. Vencimento
           'BUDAT'                TEXT-032        '15'       ' '     ' '    ' ' , " Dt Pagamento
           'TAXA'                 TEXT-016        '13'       ' '     ' '    ' ' , " TAXA / Ptax
           'FORMA_PAG'            TEXT-015        '06'       ' '     ' '    ' ' , " Form.Pgto
*           'PTAX'                 TEXT-044        '06'       ' '     ' '    ' ' , " TPAX
           'DOCNUM'               TEXT-019        '12'       ' '     ' '    ' ' , " N. Doc
           'AUGBL'                TEXT-022        '15'       ' '     ' '    ' ' . " Doc.Compens

*  CASE ABAP_TRUE.
*    WHEN RB4. "Dollar
*      PERFORM ALV_PREENCHE_CAT USING:
*             'DMBE2'       TEXT-023        '15'       ' '     ' '    'X' . " Vlr.Comp.US$
*    WHEN RB5. "Real
*      PERFORM ALV_PREENCHE_CAT USING:
*             'DMBTR'       TEXT-024        '15'       ' '     ' '    'X' . " Vlr.Comp.R$
*  ENDCASE.

  CASE abap_true.
*    WHEN RB4. "Dollar
*      PERFORM ALV_PREENCHE_CAT USING:
*             'MOEDA_FORTE' TEXT-020        '15'       ' '     ' '    'X' . " Saldo US$
*    WHEN RB5. "Real
*      PERFORM ALV_PREENCHE_CAT USING:
*             'MOEDA_INTER' TEXT-021        '15'       ' '     ' '    'X' . " Saldo R$
    WHEN rb6.
      PERFORM alv_preenche_cat USING:
       'DMBE2'       TEXT-023              '15'       ' '     ' '    'X' , " Vlr.Comp.US$ / Vlr Recebido US$
       'DMBTR'       TEXT-024              '15'       ' '     ' '    'X',  " Vlr.Comp.R$ / Vlr Recebido R$


       'VLR_MULTA_CALC' TEXT-045           '15'       'X'     ' '    '',  " Multa Calculado
       'VLR_MULTA_RBDO' TEXT-046           '15'       ' '     ' '    '',  " Multa Recebido
       'VLR_DESC_MULT'  TEXT-051           '15'       ' '     ' '    '',  " Multa Calculado
       'VLR_JUROS_CALC' TEXT-047           '15'       'X'     ' '    '',  " Juros Calculado
       'VLR_JUROS_RBDO' TEXT-048           '15'       ' '     ' '    '',  " Juros Recebido
       'VLR_DESC_JROS'  TEXT-052           '15'       ' '     ' '    ''.  " Multa Recebido

*      IF W_SAIDA-WAERK EQ 'USD'.
      PERFORM alv_preenche_cat USING:
     'VLR_SALD_FIN' TEXT-053              '15'       ' '     ' '    'X' . " Saldo US$ / Saldo Juros US$
*      ELSE.
      PERFORM alv_preenche_cat USING:
      'VLR_SALD_FIN_BRL' TEXT-054         '15'       ' '     ' '    'X' . " Saldo R$ / 	Saldo Juros R$
*      ENDIF.

      PERFORM alv_preenche_cat USING:
      'SALD_REFERENCIA' TEXT-057               '15'        ' '     ' '    'X' , " Saldo da referencia
      'MOEDA_FORTE'     TEXT-020               '15'        ' '     ' '    'X' , " Saldo US$
      'MOEDA_INTER'     TEXT-021               '15'        ' '     ' '    'X' . " Saldo R$


  ENDCASE.

  PERFORM alv_preenche_cat USING:
         'OBSERVACAO'  TEXT-033        '16'       ' '     ' '    ' ', " Observação
         'TPREL'       TEXT-062        '16'       ' '     ' '    ' ', " Tp. Rep.
         'TPLIN'       TEXT-063        '16'       ' '     ' '    ' ', " Tp. Lin
         'STATUS'      TEXT-064        '20'       ' '     ' '    ' '.

ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c.


  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.
*  WL_FCAT-SP_GROUP = 'A'.
*  IF P_CAMPO = 'TOTALQ_OV' OR P_CAMPO = 'TOTVL_OV'.
*    WL_FCAT-JUST  = 'R'.
*  ENDIF.
**<<<------"152483 - NMS - INI------>>>
  IF p_campo EQ 'TPREL' OR
     p_campo EQ 'TPLIN'.
    wl_fcat-no_out = abap_on.

  ENDIF.
**<<<------"152483 - NMS - FIM------>>>
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fatura .
  FREE: wa_fca, it_fca.
  PERFORM alv_preenche_cat_fatura USING:
        ' DOC_FATURA     '     'Doc.Fatura'         '12'    ''    ''   ''   ''    '',
        ' DOC_CONTABIL   '     'Doc.Contábil'       '12'    ''    ''   ''   ''    '',
        ' NFE            '     'Nfe'                '12'    ''    ''   ''   ''    '',
        ' DATA_VENCIMENTO'     'Data Vencimento'    '12'    ''    ''   ''   ''    '',
        ' VALOR          '     'Valor'              '14'    ''    ''   ''   ''    ''.

ENDFORM.                    " F_ALV
FORM alv_preenche_cat_fatura  USING  VALUE(p_fieldname)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_no_zero)
                            VALUE(p_hotspot)
                            VALUE(p_cor)
                            VALUE(p_just)
                            VALUE(p_sum).

  CLEAR wa_fca.

  wa_fca-fieldname = p_fieldname.
  wa_fca-scrtext_l = p_desc.
  wa_fca-scrtext_m = p_desc.
  wa_fca-scrtext_s = p_desc.
  wa_fca-outputlen = p_tam.
  wa_fca-no_zero   = p_no_zero.
  wa_fca-hotspot   = p_hotspot.
  wa_fca-emphasize = p_cor.
  wa_fca-just      = p_just.
  wa_fca-do_sum    = p_sum.

*  WA_FCA-REPTEXT     = P_SCRTEXT_L.
*  WA_FCA-SCRTEXT_S   = P_SCRTEXT_L.
*  WA_FCA-SCRTEXT_M   = P_SCRTEXT_L.
*  WA_FCA-SCRTEXT_L   = P_SCRTEXT_L.
*  WA_FCA-EMPHASIZE   = P_EMPHASIZE.
*  WA_FCA-STYLE       =
*  WA_FCA-JUST        = P_JUST.
*  WA_FCA-HOTSPOT     = P_HOTSPOT.
*  WA_FCA-F4AVAILABL  = P_F4.
*  WA_FCA-CHECKBOX    = P_CHECK.

  APPEND wa_fca TO it_fca.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_CLASSIFICACAO
*&---------------------------------------------------------------------*
FORM f_classificacao .
  CLEAR vg_i.
  REFRESH t_sort.

  mc_preenche_class:
*                     'WERKS'       '' 'X' ' ',
*                     'VKBUR'       '' 'X' ' ',
*                'REFERENCIA_NFE'   '' 'X' ' ',
                      'VBELN_P'     '' 'X' 'X',  "**
                      'VBELN_S'     '' ' ' ' ', "**
                      'VBELN_G'     '' ' ' ' '. "**
*                     'AUART'       '' 'X' ' ',
*                     'KUNNR'       '' 'X' ' ',
*                     'NAME1'       '' 'X' ' '
*                     'TEXT1'       '' 'X' ' ',
*                     'ERDAT'       '' 'X' ' ',
*                     'WAERK'       '' 'X' ' ',
*                     'TOTALQ_OV'   '' 'X' ' ',
*                     'TOTVL_OV'    '' 'X' ' '
  .




ENDFORM.                    " F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_layout .
  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-totals_text       = 'Totals'(201).
  gd_layout-info_fieldname    = 'LINE_COLOR'.
  gd_layout-coltab_fieldname  = 'COLOR_CELL'.

ENDFORM.                    " F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command1 USING ucomm TYPE sy-ucomm
                         selfield TYPE slis_selfield.

  TYPES:
    BEGIN OF ty_log,
      vbeln      TYPE zsdt0337-vbeln,
      nivel      TYPE zsdt0337-nivel,
      valor_de   TYPE zsdt0337-valor_de,
      valor_ate  TYPE zsdt0337-valor_ate,
      aprovador  TYPE zsdt0337-aprovador,
      status_apr TYPE char50,
      valor_apr  TYPE zfit186-vl_moeda_doc,                 "146630 RGA
      data_atual TYPE zsdt0337-data_atual,
      hora_atual TYPE zsdt0337-hora_atual,
    END OF ty_log.

  DATA: lv_juros_calc TYPE vbap-netwr,
        lv_juros_rbdo TYPE vbap-netwr,
        lv_desc_jros  TYPE vbap-netwr,
        lt_log        TYPE TABLE OF ty_log,
        lt_fieldcat   TYPE slis_t_fieldcat_alv,
        lt_value      TYPE TABLE OF dd07v.

  READ TABLE it_saida INTO wa_saida INDEX selfield-tabindex.

  REFRESH it_bdcdata.


  IF rb7 = abap_true.

    PERFORM f_bdc_data USING:
            'ZSDR016'   '0100'  'X'  ''                           ' ',
            ''          ''      ''   'BDC_CURSOR'	                'WG_HEADER-DOC_SIMULACAO',
            ''          ''      ''   'BDC_OKCODE'	                'ATUAL',
            ''          ''      ''   'WG_HEADER-DOC_SIMULACAO'     wa_saida-vbeln_s.
  ELSEIF rb8 = abap_true.

    PERFORM f_bdc_data USING:
            'ZSDR0022'  '0050'  'X'  ''                           ' ',
            ''          ''      ''   'BDC_CURSOR'	                'WG_HEADER-NRO_SOL_OV',
            ''          ''      ''   'BDC_OKCODE'	                'ATUAL',
            ''          ''      ''   'WG_HEADER-NRO_SOL_OV'       wa_saida-vbeln_s.
  ENDIF.

  REFRESH it_msg.

  wl_mode = 'E'.

  CASE ucomm.
    WHEN '&IC1'.
      IF sy-subrc EQ 0.
        IF selfield-fieldname EQ 'VBELN_S' AND wa_saida-vbeln_s IS NOT INITIAL.

          IF rb7 = abap_true.
            CALL TRANSACTION 'ZSDT0044' USING it_bdcdata
                                         MODE wl_mode
                                MESSAGES INTO it_msg.

          ELSEIF rb8 = abap_true.

            CALL TRANSACTION 'ZSDT0062' USING it_bdcdata
                                         MODE wl_mode
                                MESSAGES INTO it_msg.
          ENDIF.

        ELSEIF selfield-fieldname EQ 'VBELN_G' AND wa_saida-vbeln_g IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD wa_saida-vbeln_g.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.

        IF selfield-fieldname EQ 'VLR_JUROS_CALC' .

          CASE abap_true.

            WHEN rb7 OR rb8.
*              BREAK-POINT.
*              READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
              PERFORM f_saida_memoria_calculo USING wa_saida.

              CALL SCREEN 1001 STARTING AT 1 1 ENDING AT 95 25.

          ENDCASE.


        ELSEIF selfield-fieldname EQ 'VLR_MULTA_CALC'.

          PERFORM f_saida_memoria_calculo_mult USING wa_saida.

          CALL SCREEN 1003 STARTING AT 1 1 ENDING AT 85 15.

        ENDIF.


      ENDIF.



    WHEN 'REFRESH'.

      CASE abap_true.
        WHEN rb8.

          PERFORM:f_iniciar_variaves, " Cabeçalho
                  f_seleciona_dados_mi. " Form seleciona dados Mercado Interno
*              F_SHDB.

*      PERFORM:  F_SAIDA_MI. " Form de saida

*********************"Selecionando dados da OV*************************************************
          dados_merc_interno=>selec_dados_ov( ).

          IF it_saida IS NOT INITIAL.  "Verifica se os dados da OV foi carregado.
            DATA(t_saida) = it_saida.

            lt_saida = it_saida.
            FREE: it_saida.

            SORT lt_saida BY vbeln_p.
            DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING vbeln_p.

****        Verificar saldo da OV.
            LOOP AT lt_saida ASSIGNING FIELD-SYMBOL(<ls_saida>).
              LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<l_saida>) WHERE vbeln_p EQ <ls_saida>-vbeln_p.
                zcl_dados_ov=>i_vlr_ov(
                  EXPORTING
                    i_vbeln       = <l_saida>-vbeln    " Documento de vendas e distribuição precedente
                  IMPORTING
*                   E_VLR_TOTAL   = DATA(VLR_SALDO_TOTAL)   " Valor líquido na moeda do documento
                    e_vlr_parcial = vlr_saldo_parcial " Valor líquido na moeda do documento
                ).

                ADD vlr_saldo_parcial TO tot_saldo.
                CLEAR: vlr_saldo_parcial.

                zcl_dados_ov=>i_vlr_saldo_financ(
                  EXPORTING
                    i_vbeln     = <l_saida>-vbeln    " Documento de faturamento
                  IMPORTING
                    e_vlr_total = vlr_saldo_finc   " Valor líquido na moeda do documento
                ).

                ADD vlr_saldo_finc TO tot_saldo_fin.
                CLEAR: vlr_saldo_finc.
              ENDLOOP.

              IF tot_saldo EQ 0 OR tot_saldo < 0 OR tot_saldo =< 10.
                IF tot_saldo_fin EQ 0 OR tot_saldo_fin < 0 OR tot_saldo_fin =< 10.
                  <ls_saida>-ind_rec_total  = abap_true.
                ENDIF.
              ENDIF.

              CLEAR: tot_saldo, vlr_saldo_parcial, vlr_saldo_finc, tot_saldo_fin.
            ENDLOOP.

            SORT t_saida BY ind_rec_total.

*        IF rb1 IS NOT INITIAL.
*          DELETE t_saida WHERE ind_rec_total EQ abap_false.
*        ELSEIF rb2 IS NOT INITIAL.
*          DELETE t_saida WHERE ind_rec_total EQ abap_true.
*        ENDIF.

            SORT t_saida BY vbeln_p.

            LOOP AT t_saida INTO DATA(w_saida).
              READ TABLE lt_saida INTO DATA(ls_saida) WITH KEY vbeln_p = w_saida-vbeln_p.
              IF sy-subrc EQ 0.
                IF rb1 IS NOT INITIAL. "Recebido.
                  IF ls_saida-ind_rec_total EQ abap_true.
                  ELSE.
                    CONTINUE.
                  ENDIF.
                ELSEIF rb2 IS NOT INITIAL. "Saldo.
                  IF ls_saida-ind_rec_total EQ abap_true. "25/05/2021 - aoenning
                    CONTINUE.
                  ELSE.

                  ENDIF.
                ENDIF.
              ELSE.
                CONTINUE.
              ENDIF.

              APPEND w_saida TO it_saida.
****************"Vericando a codição de pagamento da OV.**************************************
              dados_merc_interno=>chkec_cond_ov( EXPORTING i_saida = w_saida
                                                 IMPORTING e_zdart = DATA(z_zdart) ).

              IF z_zdart EQ 'B'.
****************Selecionando recebimento com base na faturas da OV****************************************************
                dados_merc_interno=>selec_dados_fatura( EXPORTING i_saida = w_saida ).
              ELSE.
****************"Selecionando recebimento com base na OV*************************************
                dados_merc_interno=>check_recebimentos( EXPORTING i_saida = w_saida ).
              ENDIF.
            ENDLOOP.
          ENDIF.

          PERFORM: f_classificacao,
                   f_build_layout,
                   f_imprime_dados.

        WHEN rb7.
          PERFORM:
*            F_SHDB,
              f_iniciar_variaves, " Cabeçalho
              f_seleciona_dados. " Form seleciona dados Insumos
*          F_SAIDA, " Form de saida


*********************"Selecionando dados da OV*************************************************
          dados_insumo=>selec_dados_ov( ).


          IF it_saida IS NOT INITIAL.  "Verifica se os dados da OV foi carregado.
            FREE: t_saida, lt_saida.
            t_saida = it_saida.
            lt_saida = it_saida.

            FREE: it_saida.

            SORT lt_saida BY vbeln_p.
            DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING vbeln_p.

****        Verificar saldo da OV.
            LOOP AT lt_saida ASSIGNING <ls_saida>.
              LOOP AT t_saida ASSIGNING <l_saida> WHERE vbeln_p EQ <ls_saida>-vbeln_p.
                zcl_dados_ov=>i_vlr_ov(
                  EXPORTING
                    i_vbeln       = <l_saida>-vbeln    " Documento de vendas e distribuição precedente
                  IMPORTING
*                   E_VLR_TOTAL   = DATA(VLR_SALDO_TOTAL)   " Valor líquido na moeda do documento
                    e_vlr_parcial = vlr_saldo_parcial " Valor líquido na moeda do documento
                ).
                ADD vlr_saldo_parcial TO tot_saldo.
                CLEAR: vlr_saldo_parcial.

                zcl_dados_ov=>i_vlr_saldo_financ(
                  EXPORTING
                    i_vbeln     = <l_saida>-vbeln    " Documento de faturamento
                  IMPORTING
                    e_vlr_total = vlr_saldo_finc   " Valor líquido na moeda do documento
                ).

                ADD vlr_saldo_finc TO tot_saldo_fin.
                CLEAR: vlr_saldo_finc.
              ENDLOOP.

              IF tot_saldo EQ 0 OR tot_saldo < 0 OR tot_saldo =< 10.
                IF tot_saldo_fin EQ 0 OR tot_saldo_fin < 0 OR tot_saldo_fin =< 10.
                  <ls_saida>-ind_rec_total  = abap_true.
                ENDIF.
              ENDIF.
              CLEAR: tot_saldo, tot_saldo_fin, vlr_saldo_parcial, vlr_saldo_finc.
            ENDLOOP.

            SORT t_saida BY ind_rec_total.

*        IF rb1 IS NOT INITIAL.
*          DELETE t_saida WHERE ind_rec_total EQ abap_false.
*        ELSEIF rb2 IS NOT INITIAL.
*          DELETE t_saida WHERE ind_rec_total EQ abap_true.
*        ENDIF.

            SORT t_saida BY vbeln_p.

            LOOP AT t_saida INTO w_saida.
              READ TABLE lt_saida INTO ls_saida WITH KEY vbeln_p = w_saida-vbeln_p.
              IF sy-subrc EQ 0.
                IF rb1 IS NOT INITIAL. "Recebido.
                  IF ls_saida-ind_rec_total EQ abap_true.
                  ELSE.
                    CONTINUE.
                  ENDIF.
                ELSEIF rb2 IS NOT INITIAL. "Saldo.
                  IF ls_saida-ind_rec_total EQ abap_true. "25/05/2021
                    CONTINUE.
                  ELSE.

                  ENDIF.
                ENDIF.
              ELSE.
                CONTINUE.
              ENDIF.

              APPEND w_saida TO it_saida.

****************"Vericando a codição de pagamento da OV.**************************************
              CLEAR: z_zdart.
              dados_insumo=>chkec_cond_ov( EXPORTING i_saida = w_saida
                                           IMPORTING e_zdart = z_zdart ).

              IF z_zdart EQ 'B'.
****************Selecionando recebimento com base na faturas da OV****************************************************
                dados_insumo=>selec_dados_fatura( EXPORTING i_saida = w_saida ).
              ELSE.
****************"Selecionando recebimento com base na OV*************************************
                dados_insumo=>check_recebimentos( EXPORTING i_saida = w_saida ).
              ENDIF.
            ENDLOOP.
          ENDIF.


          PERFORM:  f_classificacao,
                    f_build_layout,
                    f_imprime_dados.
      ENDCASE.


    WHEN  'CALCULAR'.



      IF wa_saida-data_venc IS INITIAL .
*        MESSAGE E888(SABAPDOCU) WITH 'O registro selecionado não possui Data de Vencimento, favor selecionar um registro que possui vencimento.'.
        MESSAGE 'O registro selecionado não possui data de vencimento, favor selecionar um registro que possui vencimento.' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        IF wa_saida-moeda_inter IS INITIAL AND wa_saida-sald_referencia IS INITIAL AND wa_saida-moeda_forte IS INITIAL.
          MESSAGE s888(sabapdocu) WITH 'O registro selecionado não possui saldo.'.
          EXIT.
        ELSE.

**        Limpar os campos
          CLEAR: wa_saida_calculadora.

          lw_zfit0026-data_venc      = wa_saida-data_venc.
          wa_saida_calculadora-moeda = wa_saida-waerk.

          IF wa_saida-waerk EQ 'BRL'.
            IF wa_saida-moeda_inter IS NOT INITIAL.
              wa_saida_calculadora-vlr_total_ov  = wa_saida-totvl_ov.
              wa_saida_calculadora-vlr_saldo_ov  = wa_saida-moeda_inter.
            ELSE.
              IF wa_saida-sald_referencia IS NOT INITIAL.
                wa_saida_calculadora-vlr_total_ov  = wa_saida-vlr_tot_ref.
                wa_saida_calculadora-vlr_saldo_ov  = wa_saida-sald_referencia.
              ENDIF.
            ENDIF.
          ELSE.
            IF wa_saida-moeda_forte IS NOT INITIAL.
              wa_saida_calculadora-vlr_total_ov  = wa_saida-totvl_ov.
              wa_saida_calculadora-vlr_saldo_ov  = wa_saida-moeda_forte.
            ELSE.
              IF wa_saida-sald_referencia IS NOT INITIAL.
                wa_saida_calculadora-vlr_total_ov  = wa_saida-vlr_tot_ref.
                wa_saida_calculadora-vlr_saldo_ov  = wa_saida-sald_referencia.
              ENDIF.
            ENDIF.
          ENDIF.

          lw_zfit0026-vbeln      = wa_saida-vbeln.

          CALL SCREEN 1002 STARTING AT 5 5 ENDING AT 80  25.
        ENDIF.
      ENDIF.

    WHEN 'LISTA'.

      PERFORM f_listar_faturas USING wa_saida.

    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE TO SCREEN 0.

**********************************************************************
* Função botão ir pra transação zfis26 / 110302 CS2023000291 Melhoria Simples ZSDT0060 - PSA
**********************************************************************

    WHEN '&ZFIS26'. "Programa -> ZFIR0022

      AUTHORITY-CHECK OBJECT 'S_TCODE'
      ID 'TCD' FIELD 'ZFIS26'.
      IF sy-subrc = 0.

        PERFORM parametros_zfis26.

        IF rb7 EQ abap_true.
          l_rb7 = 'X'.
        ELSE.
          l_rb7 = space .
        ENDIF.

        IF rb8 EQ abap_true.
          l_rb8 = 'X'.
        ELSE.
          l_rb8 = space .
        ENDIF.

        SUBMIT zfir0022
            WITH p_bukrs CP '*' "IN lr_bukrs            "Empresa
            WITH p_vkbur CP '*' "IN lr_vkbur            "Escritorio de Venda
            "WITH p_kunnr IN lr_kunnr            "Cliente
            WITH p_vbeln IN lr_vbeln            "Nr OV
           "WITH p_erdat IN lr_erdat            "data da Criação
            WITH p_ins   =  l_rb7               "Insumos
            WITH p_mi    =  l_rb8               "Mercado interno
           AND RETURN.

      ELSE.
        MESSAGE e208(00) WITH 'Você não possui autorização à Transação ZFIS26' .
      ENDIF.

    WHEN 'ISENCAO_JUROS'.

      "146630-CS2024000604 Isenção de Juros Insumos - SMC - RETIRAR ESSE PONTO APÓS HOMOLOGADO PARA SUBIR PARA PRD
      IF sy-sysid <> 'QAS'.
        MESSAGE 'Funcionalidade em manutenção' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      "146630-CS2024000604 Isenção de Juros Insumos - SMC - RETIRAR ESSE PONTO APÓS HOMOLOGADO PARA SUBIR PARA PRD

      IF selfield-tabindex EQ 0.
        MESSAGE 'Favor selecionar uma linha' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF wa_saida-vbeln_p IS INITIAL.
        MESSAGE 'Favor selecionar uma linha com OV principal' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      wa_cabec-ov   = wa_saida-vbeln_p.
      wa_cabec-data_venc = wa_saida-data_venc.
      wa_cabec-moeda = wa_saida-waerk.

      "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - ini
      IF wa_cabec-moeda NE 'BRL'.

        CLEAR wa_cabec-ptax.

        SELECT SINGLE ukurs
          FROM zi_est_tcurr
          INTO @DATA(lv_ptax)
          WHERE kurst EQ 'G'
          AND fcurr   EQ @wa_cabec-moeda
          AND tcurr   EQ 'BRL'
          AND gdatu   EQ @sy-datum.
        IF sy-subrc EQ 0.
          WRITE lv_ptax TO wa_cabec-ptax CURRENCY 'BRL'.
        ENDIF.
      ELSE.
        CLEAR wa_cabec-ptax.
      ENDIF.
      "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - fim


      DATA(lt_saida) = it_saida.
      SORT lt_saida BY vbeln.
      IF wa_saida-vbeln_p IS NOT INITIAL.

        DELETE lt_saida WHERE vbeln_p <> wa_saida-vbeln_p.
      ELSE.
        DELETE lt_saida WHERE vbeln_p <> space.
      ENDIF.

      IF lt_saida IS NOT INITIAL.

        SELECT *
          FROM zfit0026
          INTO TABLE @DATA(lt_zfit0026)
          FOR ALL ENTRIES IN @lt_saida
          WHERE vbeln = @lt_saida-vbeln_g
            OR  vbeln = @lt_saida-vbeln_p.
        IF sy-subrc IS INITIAL.

          LOOP AT lt_zfit0026 ASSIGNING FIELD-SYMBOL(<fs_zfit0026>).
            lv_juros_calc = lv_juros_calc + <fs_zfit0026>-vlr_juros_calc.
            lv_juros_rbdo = lv_juros_rbdo + <fs_zfit0026>-vlr_juros_rbdo.
            lv_desc_jros  = lv_desc_jros + <fs_zfit0026>-vlr_desc_jros.
          ENDLOOP.

          lv_juros_calc = lv_juros_calc - lv_juros_rbdo - lv_desc_jros.

          IF lv_juros_calc <= 0.
            MESSAGE 'Não existe saldo de juros disponível' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            wa_cabec-saldo = lv_juros_calc.

            CLEAR wa_cabec-sald_jbrl.

            IF wa_cabec-moeda EQ 'BRL'.                     "146630-RGA
              wa_cabec-sald_jbrl =  wa_cabec-saldo.
            ELSE.
              wa_cabec-sald_jbrl = wa_cabec-saldo * lv_ptax.
            ENDIF.

          ENDIF.

        ELSE.

          MESSAGE 'Não existe saldo de juros disponível' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.

      ENDIF.

      SELECT SINGLE *
        FROM zfit186
        INTO @DATA(ls_zfit186)
        WHERE ov_principal = @wa_cabec-ov
          AND status_solicit NOT IN ( '2' , '5' ).          "146630-RGA
      IF sy-subrc IS INITIAL.
        MESSAGE 'Já existe solicitação de isenção de juros para a OV selecionada' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CALL SCREEN '1005' STARTING AT 1 1 ENDING AT 80 20.

    WHEN 'LOG_ISENCAO'.

      "146630-CS2024000604 Isenção de Juros Insumos - SMC - RETIRAR ESSE PONTO APÓS HOMOLOGADO PARA SUBIR PARA PRD
      IF sy-sysid <> 'QAS'.
        MESSAGE 'Funcionalidade em manutenção' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      "146630-CS2024000604 Isenção de Juros Insumos - SMC - RETIRAR ESSE PONTO APÓS HOMOLOGADO PARA SUBIR PARA PRD


      "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - ini
*      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*        EXPORTING
*          i_program_name         = sy-repid
*          i_structure_name       = 'ZFIE_ALV_LOG_JUROS'
*        CHANGING
*          ct_fieldcat            = lt_fieldcat
*        EXCEPTIONS
*          inconsistent_interface = 1
*          program_error          = 2
*          OTHERS                 = 3.
*      IF sy-subrc = 0.
*        LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
*
*          CASE <fs_fieldcat>-fieldname.
*            WHEN 'VALOR_APR'.
*              <fs_fieldcat>-seltext_l = 'Valor Aprovação'.
*              <fs_fieldcat>-seltext_m = 'Valor Aprovação'.
*              <fs_fieldcat>-seltext_s = 'Valor Aprovação'.
*
**            WHEN 'DATA_ATUAL'.
**              <fs_fieldcat>-seltext_l = 'Data Aprovação'.
**              <fs_fieldcat>-seltext_m = 'Data Aprovação'.
**              <fs_fieldcat>-seltext_s = 'Data Aprovação'.
*
*            WHEN 'VALOR_DE'.
*              <fs_fieldcat>-seltext_l = 'Valor DE'.
*              <fs_fieldcat>-seltext_m = 'Valor DE'.
*              <fs_fieldcat>-seltext_s = 'Valor DE'.
*
*            WHEN 'VALOR_ATE'.
*              <fs_fieldcat>-seltext_l = 'Valor ATÉ'.
*              <fs_fieldcat>-seltext_m = 'Valor ATÉ'.
*              <fs_fieldcat>-seltext_s = 'Valor ATÉ'.
*
*            WHEN 'APROVADOR'.
*              <fs_fieldcat>-seltext_l = 'Aprovador'.
*              <fs_fieldcat>-seltext_m = 'Aprovador'.
*              <fs_fieldcat>-seltext_s = 'Aprovador'.
*
*            WHEN 'STATUS'.
*              <fs_fieldcat>-seltext_l = 'Status Aprovação'.
*              <fs_fieldcat>-seltext_m = 'Status Aprovação'.
*              <fs_fieldcat>-seltext_s = 'Status Aprovação'.
*            WHEN OTHERS.
*          ENDCASE.
*
*        ENDLOOP.
*      ENDIF.

      SELECT SINGLE status_solicit, vl_moeda_doc "146630 - RGA
              FROM zfit186
              INTO @DATA(ls_zfit1866)
              WHERE ov_principal = @wa_saida-vbeln_p.

      IF sy-subrc IS INITIAL.

        SUBMIT zfir0126
           WITH s_bukrs  INCL wa_saida-bukrs
           WITH s_vkbur  INCL wa_saida-vkbur
           WITH s_ovprin INCL wa_saida-vbeln_p
           WITH s_kunnr  INCL wa_saida-kunnr

           AND RETURN.
*        SELECT *
*          FROM zsdt0337
*          INTO CORRESPONDING FIELDS OF TABLE lt_log
*          WHERE vbeln = wa_saida-vbeln_p.
*        IF sy-subrc IS INITIAL.
*
*          CALL FUNCTION 'GET_DOMAIN_VALUES'
*            EXPORTING
*              domname         = 'ZDOFI_STAT_SOLIC'
**             TEXT            = 'X'
**             FILL_DD07L_TAB  = ' '
*            TABLES
*              values_tab      = lt_value
**             VALUES_DD07L    =
*            EXCEPTIONS
*              no_values_found = 1
*              OTHERS          = 2.
*          IF sy-subrc = 0.
*            SORT lt_value BY domvalue_l.
*          ENDIF.
*
*          LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).
*
*            READ TABLE lt_value ASSIGNING FIELD-SYMBOL(<fs_value>)
*            WITH KEY domvalue_l = <fs_log>-status_apr(1)
*            BINARY SEARCH.
*            IF sy-subrc IS INITIAL.
*              <fs_log>-status_apr = <fs_value>-ddtext.
*            ENDIF.
*
*          ENDLOOP.
*
*        ELSE.
*
*          APPEND INITIAL LINE TO lt_log ASSIGNING <fs_log>.
*          <fs_log>-vbeln  = wa_saida-vbeln_p.
*
*          <fs_log>-data_atual = sy-datum.
*
*        ENDIF.

      ELSE.
        MESSAGE 'Não existe histórico para a OV selecionada' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

*      SORT lt_log BY vbeln data_atual hora_atual nivel.
*
*      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*        EXPORTING
*          i_callback_program = sy-repid
*          it_fieldcat        = lt_fieldcat
*        TABLES
*          t_outtab           = lt_log
*        EXCEPTIONS
*          program_error      = 1
*          OTHERS             = 2.
*      IF sy-subrc <> 0.
*
*      ENDIF.
      "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - fim
  ENDCASE.
ENDFORM.                    "USER_COMMAND1

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " F_BDC_DATA

"INCLUDE zsdr0020_construir_saidaf01.
*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_VBRK  text
*----------------------------------------------------------------------*
FORM f_construir_saida_faturas  TABLES   p_it_vbrk STRUCTURE vbrk USING  v_vbeln.

  DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
        xz_vlr_fortet   TYPE zfit0026-mont_moeda,
        xz_vlr_interna  TYPE zfit0026-mont_mi,
        xz_vlr_internat TYPE zfit0026-mont_mi,
        xtotalq_ov      TYPE vbap-kwmeng,
        xtotalvl_ov     TYPE vbap-netwr,
        vvalor          TYPE zfit0026-mont_moeda,
        pare            TYPE n LENGTH 3,
        vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

  SORT:  it_vbak          BY vbeln,
         it_vbap          BY vbeln,
         it_vbkd          BY vbeln,
         it_zfit0026      BY vbeln,
         it_kna1          BY kunnr,
         it_bsad          BY bukrs kunnr belnr ,
         it_t052u         BY zterm.



  LOOP AT p_it_vbrk INTO DATA(wa_vbrk) .
    " Referencia NFe
    SELECT *
       FROM vbfa
       INTO CORRESPONDING FIELDS OF  TABLE it_vbfa_auxi
       WHERE vbeln = wa_vbrk-vbeln
       AND  vbtyp_n  = 'M'
       AND  vbtyp_v  = 'C'.

    LOOP AT it_vbfa_auxi  ASSIGNING FIELD-SYMBOL(<wa_vbfa_auxi>).
      <wa_vbfa_auxi>-refkey =  <wa_vbfa_auxi>-vbeln.
    ENDLOOP.

*    LOOP AT IT_J_1BNFDOC INTO DATA(WA_J_1BNFDOC).
*      WA_SAIDA-REFERENCIA_NFE = WA_J_1BNFDOC-NFENUM.
*    ENDLOOP.

    SELECT * FROM vbrp INTO TABLE @DATA(it_vbrp)
      WHERE vbeln EQ @wa_vbrk-vbeln.

    LOOP AT it_vbrp INTO DATA(wa_vbrp).
      wa_saida-vlr_referencia = ( wa_vbrp-netwr + wa_vbrp-mwsbp ).
    ENDLOOP.

    xtotalq_ov  = 0.
    xtotalvl_ov = 0.


    CLEAR vbeln_aux.
    pare = 0.
    vbeln_aux = wa_vbak-vbeln.

    READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
    IF sy-subrc IS INITIAL.
      wa_saida-vbeln_p = wa_0041-vbeln.
      wa_saida-vbeln_s = wa_0041-doc_simulacao.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                     AND vbelv NE vbeln_aux.
        EXIT.
      ENDLOOP.

      READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
      IF sy-subrc IS INITIAL.
        wa_saida-vbeln_p = wa_0041-vbeln.
        wa_saida-vbeln_s = wa_0041-doc_simulacao.
      ENDIF.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      WHILE pare IS INITIAL.

        pare = 4.
        LOOP AT it_0090 INTO wa_0090 WHERE vbeln EQ vbeln_aux
                                       AND vbelv NE vbeln_aux.
          pare = 0.
          EXIT.
        ENDLOOP.

        IF pare IS INITIAL.
          vbeln_aux = wa_0090-vbelv.
        ENDIF.
        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = vbeln_aux.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ENDIF.
      ENDWHILE.
    ENDIF.

* INCLUINDO AS REMESSAS CS2016000023 >>>>>
    READ TABLE it_vbfa WITH KEY vbeln = vbeln_aux.
    IF sy-subrc IS INITIAL.

      READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = it_vbfa-vbelv.

      IF sy-subrc IS INITIAL.

        wa_saida-vbeln_p = wa_0041-vbeln.
        wa_saida-vbeln_s = wa_0041-doc_simulacao.

        " 28.07.2023 - RAMON 98680 -->
*        wa_saida-vbelv_agp = wa_0041-vbelv_agp.
        " 28.07.2023 - RAMON 98680 --<

      ELSE.
        READ TABLE it_0090 INTO wa_0090 WITH KEY vbeln = it_vbfa-vbelv.
        READ TABLE it_0041 INTO wa_0041 WITH KEY vbeln = wa_0090-vbelv.
        IF sy-subrc IS INITIAL.
          wa_saida-vbeln_p = wa_0041-vbeln.
          wa_saida-vbeln_s = wa_0041-doc_simulacao.
        ENDIF.
      ENDIF.
    ENDIF.
* INCLUINDO AS REMESSAS CS2016000023 <<<<<

* INCLUINDO O CAMPO SAFRA CS2016000023 >>>>>
    READ TABLE it_0040 WITH KEY doc_simulacao = wa_saida-vbeln_s.
    IF sy-subrc IS INITIAL.



      wa_saida-safra = it_0040-safra.
      wa_saida-id_order_ecommerce = it_0040-id_order_ecommerce. "SMC #123881 - EQUALIZAÇÃO ECC X HANA

      "PAGAMENTO ANTECIPADO
      CASE it_0040-meio_pago .
        WHEN 'D' .
          wa_saida-pgto_ant = 'Deposito em Conta'.
        WHEN 'A' .
          wa_saida-pgto_ant = 'Acerto'.
        WHEN 'B' .
          wa_saida-pgto_ant = 'Boleto Bancário'.
        WHEN ' ' .
          wa_saida-pgto_ant = 'Não Atencipado'.
      ENDCASE.


    ENDIF.

    " TAXA MULTA

*    WA_SAIDA-TX_MULTA = IT_0040-JUROS_ANO .  " Comentado porque não tem esse campo no simulador.
    wa_saida-tx_juros = it_0040-juros_ano .

    " END TAXA MULTA


    CLEAR it_0040.
* INCLUINDO O CAMPO SAFRA CS2016000023 <<<<<<

*      FIM MODIFICAÇÃO WELGEM
    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.
      ADD wa_vbap-kwmeng TO xtotalq_ov.
      ADD wa_vbap-netwr TO xtotalvl_ov.
      ADD wa_vbap-mwsbp TO xtotalvl_ov.
      ADD wa_vbap-netwr TO wa_saida-netwr_l.
      ADD wa_vbap-mwsbp TO wa_saida-mwsbp.
    ENDLOOP.

*&------------Inicio ajuste CS2024000717 / AOENNING &*
    "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.
    SELECT SINGLE * FROM tvarvc INTO  @DATA(wa_ov_devolucao)
      WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
       AND  low  EQ @wa_vbak-auart.
*      IF wa_vbak-auart EQ 'ZRPF'  OR  wa_vbak-auart EQ 'ZROB'  OR  wa_vbak-auart EQ 'ZREB'.
    IF sy-subrc EQ 0.
*&------------Fim ajuste CS2024000717 / AOENNING &*
      xtotalq_ov  = xtotalq_ov  * -1.
      xtotalvl_ov = xtotalvl_ov * -1.
      wa_saida-netwr_l = wa_saida-netwr_l * -1.
      wa_saida-mwsbp = wa_saida-mwsbp * -1.
    ELSEIF  wa_vbak-auart EQ 'ZREM' OR  wa_vbak-auart EQ 'ZRFU'.
      xtotalq_ov  = 0.
      xtotalvl_ov = 0.
    ENDIF.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<
    READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln = wa_vbak-vbeln. "BINARY SEARCH.
    READ TABLE it_kna1  INTO wa_kna1  WITH KEY kunnr = wa_vbak-kunnr. " BINARY SEARCH.
    READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = wa_vbak-vbeln. " BINARY SEARCH.

    wa_saida-data_venc  = wa_vbkd-valdt.

    READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm. " BINARY SEARCH.

    wa_saida-werks      = wa_vbap-werks.

    SELECT SINGLE bezei
    FROM tvgrt
    INTO @DATA(z_bezei)
     WHERE spras EQ @sy-langu
       AND vkgrp EQ @wa_vbak-vkgrp.
    wa_saida-vkgrp = wa_vbak-vkgrp && '-' && z_bezei.

    wa_saida-vkbur      = wa_vbak-vkbur.
    wa_saida-kunnr      = wa_vbak-kunnr.
    wa_saida-name1      = wa_kna1-name1.
    wa_saida-auart      = wa_vbak-auart.
    wa_saida-zterm      = wa_vbkd-zterm.
    wa_saida-text1      = wa_t052u-text1.

    wa_saida-vbeln      = wa_vbak-vbeln.
    wa_saida-vbeln_g    = wa_vbak-vbeln.

    wa_saida-erdat      = wa_vbak-erdat.
    wa_saida-waerk      = wa_vbak-waerk.

    xz_vlr_forte   = 0.
    xz_vlr_interna = 0.

    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln.

      MOVE: wa_zfit0026-observacao TO wa_saida-observacao.

      "Verifcar se o documento é boleto
      SELECT COUNT(*)
        FROM zsdt0159
        INTO @DATA(is_boleto)
        WHERE adiant =  @wa_zfit0026-docnum.

      " É boleto
      IF is_boleto > 0 .

        READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                  kunnr = wa_vbak-kunnr
                                                  belnr = wa_zfit0026-docnum.
        "Verifica se está compensado.
        IF wa_bsad-augdt IS NOT INITIAL.
          ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
          ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.

        ENDIF.

      ELSE.
        ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
        ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.
      ENDIF.
    ENDLOOP.

    "QTDE FATURADO NFe
    DATA(t_zfit0026_group_by_doc_fatura) = it_zfit0026[].

    SORT t_zfit0026_group_by_doc_fatura BY doc_fatura.
    DELETE ADJACENT DUPLICATES FROM t_zfit0026_group_by_doc_fatura COMPARING doc_fatura.

    LOOP AT t_zfit0026_group_by_doc_fatura INTO  DATA(wa_zfit26_group_by_doc_fatura) WHERE  vbeln = wa_vbak-vbeln
                                                                                       AND  doc_fatura = wa_vbrk-vbeln.
      IF wa_zfit26_group_by_doc_fatura-doc_fatura IS NOT INITIAL .
        SELECT SUM( fkimg )
        FROM vbrp
        INTO @DATA(qtde_faturado_nfe)
       WHERE vbeln = @wa_zfit26_group_by_doc_fatura-doc_fatura.
      ENDIF.

      ADD qtde_faturado_nfe TO wa_saida-fkimg.
    ENDLOOP.
    "END QTDE FATURADO NFe.

* Salva totais
    xz_vlr_fortet   = xz_vlr_forte.
    xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
    IF wa_vbak-waerk = 'USD'.
      xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
      xz_vlr_interna = 0.
    ELSE.
      xz_vlr_forte = 0.
      xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
    ENDIF.

    vvalor = xtotalq_ov.
    wa_saida-totalq_ov = vvalor.

    vvalor = xtotalvl_ov.
    wa_saida-totvl_ov = vvalor.

    DATA: vflag TYPE i,
          vcont TYPE i.
    vflag = 0.
*    VCONT = 0.

    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = wa_vbak-vbeln. "AND DOC_FATURA = WA_VBRK-VBELN .
      wa_saida-vlr_multa_calc = wa_zfit0026-vlr_multa_calc.  "MULTA CALCULADO
      wa_saida-vlr_multa_rbdo = wa_zfit0026-vlr_multa_rbdo.  "MULTA RECEBIDA
      wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
      wa_saida-vlr_juros_calc = wa_zfit0026-vlr_juros_calc.  "JUROS CALCULADO
      wa_saida-vlr_juros_rbdo = wa_zfit0026-vlr_juros_rbdo. "JUROS RECEBIDO
      wa_saida-vlr_desc_mult  = wa_zfit0026-vlr_desc_mult.
      wa_saida-vlr_desc_jros  = wa_zfit0026-vlr_desc_jros.

      IF wa_saida-waerk EQ 'USD'.
        wa_saida-vlr_sald_fin = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ELSE.
        wa_saida-vlr_sald_fin_brl = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ENDIF.

      wa_saida-ptax = wa_zfit0026-taxa. "PTAX
      wa_saida-data_pgto = wa_zfit0026-data_pgto.
      wa_saida-mont_rbdo = wa_zfit0026-mont_rbdo.
      MOVE: wa_zfit0026-observacao TO wa_saida-observacao.

      ADD 1 TO vcont.

      READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
                                                 kunnr = wa_vbak-kunnr
                                                 belnr = wa_zfit0026-docnum.
*      IF sy-subrc IS NOT INITIAL.
*
*        READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.
*
*        READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbak-vkorg
*                                       kunnr = wa_vbak-kunnr
*                                       belnr = wa_z0159-adiant.
*      ENDIF.

      IF sy-subrc IS NOT INITIAL.

        READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.

        READ TABLE it_bsad  INTO wa_bsad  WITH KEY  bukrs = wa_vbak-vkorg
                                                    kunnr = wa_vbak-kunnr
                                                    belnr = wa_z0159-adiant.
        IF sy-subrc IS NOT INITIAL.

          READ TABLE it_zibchv INTO wa_zibchv WITH KEY obj_key = wa_zfit0026-obj_key.

          READ TABLE it_bsad  INTO wa_bsad  WITH KEY  bukrs = wa_vbak-vkorg
                                                      kunnr = wa_vbak-kunnr
                                                      belnr = wa_zibchv-belnr.
        ENDIF.
      ENDIF.




      FREE: it_color, t_cor.

      IF sy-subrc IS INITIAL.
        wa_saida-augbl     = wa_bsad-augbl.
*        WA_SAIDA-BUDAT     = WA_BSAD-BUDAT.
        wa_saida-budat     = wa_bsad-augdt.
*        WA_SAIDA-DMBE2     = WA_BSAD-DMBE2.
*        WA_SAIDA-DMBTR     = WA_BSAD-DMBTR.
        wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
        wa_saida-dmbtr      = ( wa_zfit0026-mont_moeda * wa_zfit0026-taxa ).

        t_cor =
        VALUE #(
                 ( fname = 'AUGBL' color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'BUDAT' color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'VLR_SALD_FIN' color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'DMBE2' color-col = '5' color-int = '1' color-inv = '1' )
                 ( fname = 'DMBTR' color-col = '5' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

      ELSE.
        wa_saida-dmbe2     = 0.
        wa_saida-dmbtr     = 0.
        wa_saida-augbl     = ''.
      ENDIF.

      wa_saida-forma_pag  = wa_zfit0026-forma_pag.
      wa_saida-taxa       = wa_zfit0026-taxa.
      wa_saida-mont_moeda = COND #( WHEN wa_vbak-waerk EQ 'USD' THEN wa_zfit0026-mont_moeda ELSE 0 ).
      wa_saida-mont_mi = COND #( WHEN wa_vbak-waerk EQ 'BRL' THEN wa_zfit0026-mont_mi ELSE 0 ).
      wa_saida-docnum     = wa_zfit0026-docnum.

*      IF wa_saida-docnum IS INITIAL.
*        wa_saida-docnum = wa_z0159-adiant.
*      ENDIF.

      IF wa_saida-docnum IS INITIAL.
        IF wa_z0159-adiant IS NOT INITIAL.
          wa_saida-docnum = wa_z0159-adiant.
        ELSE.
          wa_saida-docnum = wa_zibchv-belnr.
        ENDIF.
      ENDIF.

      t_cor =
      VALUE #(
               ( fname = 'FORMA_PAG'  color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'TAXA'       color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'MONT_MOEDA' color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'MONT_MI'    color-col = '5' color-int = '1' color-inv = '1' )
               ( fname = 'DOCNUM'     color-col = '5' color-int = '1' color-inv = '1' )
             ).
      APPEND LINES OF t_cor TO it_color.

      IF vcont = 1.
        wa_saida-moeda_forte = xz_vlr_forte.
        wa_saida-moeda_inter = xz_vlr_interna.

        t_cor =
        VALUE #(
                 ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

      ELSE.
        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.

        t_cor =
        VALUE #(
                 ( fname = 'VLR_SALD_FIN' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'MOEDA_FORTE' color-col = '2' color-int = '0' color-inv = '1' )
                 ( fname = 'MOEDA_INTER' color-col = '2' color-int = '0' color-inv = '1' )
               ).
        APPEND LINES OF t_cor TO it_color.

        CLEAR wa_saida-line_color.
      ENDIF.

      wa_saida-salus = ( wa_saida-mont_moeda + wa_saida-moeda_forte ) - wa_saida-dmbe2.
      wa_saida-salre = ( wa_saida-mont_mi    + wa_saida-moeda_inter ) - wa_saida-dmbtr.
      wa_saida-color_cell[] = it_color[].

      IF rb1 = 'X'.
        IF wa_saida-augbl IS NOT INITIAL.
          APPEND wa_saida TO it_saida.
          vflag = 1.
        ENDIF.
      ELSEIF rb2 = 'X'.
        IF ( wa_saida-waerk = 'USD' AND xz_vlr_forte NE 0 ) OR ( wa_saida-waerk = 'BRL' AND xz_vlr_interna NE 0 ).
          APPEND wa_saida TO it_saida.
        ENDIF.
      ELSE.
        APPEND wa_saida TO it_saida.
      ENDIF.
      wa_saida-augbl     = ''.

      CLEAR: wa_z0159, wa_bsad, wa_zibchv.
    ENDLOOP.

    CLEAR: wa_saida-forma_pag, wa_saida-taxa, wa_saida-docnum, wa_saida-augbl, wa_saida-color_cell[].
    REFRESH it_color.

    IF vcont = 0.
      wa_saida-mont_moeda  = 0.
      wa_saida-mont_mi     = 0.

      wa_saida-moeda_forte = 0.
      wa_saida-moeda_inter = 0.
      wa_saida-augbl     = wa_bsad-augbl.

      wa_saida-moeda_forte = xz_vlr_forte.
      wa_saida-moeda_inter = xz_vlr_interna.

      t_cor =
      VALUE #(
              ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
              ( fname = 'MOEDA_FORTE' color-col = '6' color-int = '1' color-inv = '1' )
              ( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' )
             ).
      APPEND LINES OF t_cor TO it_color.

      wa_saida-color_cell[] = it_color[].
      wa_saida-line_color  = 'C310'.

      READ TABLE it_zfit0026 INTO wa_zfit0026 WITH KEY vbeln = wa_vbak-vbeln. " BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        wa_saida-salus = wa_saida-moeda_forte.
        wa_saida-salre = wa_saida-moeda_inter.
      ENDIF.

      IF rb1 = 'X'.
        IF vflag = 1.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ELSEIF rb2 = 'X'.
        IF ( wa_saida-waerk = 'USD' AND wa_saida-moeda_forte NE 0 ) OR ( wa_saida-waerk = 'BRL' AND wa_saida-moeda_inter NE 0 ).
          APPEND wa_saida TO it_saida.
        ENDIF.
      ELSE.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ENDIF.

    CLEAR: wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_saida, wa_color, wa_0090, wa_0090x, wa_0041.
    FREE it_color.
  ENDLOOP.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).
    <f_saida>-rfmng = REDUCE rfmng( INIT x TYPE rfmng FOR ls IN it_remessa WHERE ( vbelv EQ <f_saida>-vbeln_g ) NEXT x = x + ls-rfmng ).
  ENDLOOP.

  CHECK rb2 EQ abap_true.

  LOOP AT it_saida INTO wa_saida.
    wa_delete =
    VALUE #(
             vbeln_s     = wa_saida-vbeln_s
             vbeln_p     = wa_saida-vbeln_p
             moeda_forte = wa_saida-moeda_forte
             moeda_inter = wa_saida-moeda_inter
           ).

    COLLECT wa_delete INTO it_delete.
    CLEAR wa_delete.
  ENDLOOP.

  DELETE it_delete WHERE moeda_forte > 10 OR  moeda_inter > 10.

  LOOP AT it_delete INTO wa_delete.
    DELETE it_saida WHERE vbeln_s EQ wa_delete-vbeln_s AND
                          vbeln_p EQ wa_delete-vbeln_p.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_SAIDA_FATURAS_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_VBRK_MI  text
*----------------------------------------------------------------------*
FORM f_construir_saida_faturas_mi TABLES it_vbrk_mi STRUCTURE vbrk USING i_saida TYPE ty_saida.
  DATA: xz_vlr_forte    TYPE zfit0026-mont_moeda,
        xz_vlr_fortet   TYPE zfit0026-mont_moeda,
        xz_vlr_interna  TYPE zfit0026-mont_mi,
        xz_vlr_internat TYPE zfit0026-mont_mi,
        xtotalq_ov      TYPE vbap-kwmeng,
        xtotalvl_ov     TYPE vbap-netwr,
        vvalor          TYPE zfit0026-mont_moeda,
        pare            TYPE n LENGTH 3,
        vbeln_aux       TYPE vbeln.
*         R_DEVO_RECU     TYPE RANGE OF AUART.

  SORT:  it_vbak          BY vbeln,
         it_vbap          BY vbeln,
         it_vbkd          BY vbeln,
         it_zfit0026      BY vbeln,
         it_kna1          BY kunnr,
         it_bsad          BY bukrs kunnr belnr ,
         it_t052u         BY zterm.

  DATA:  wa_zfit26_group_by_doc_fatura TYPE ty_zfit0026.
  DATA:  doc_fatura TYPE vbrk-vbeln.

  DATA: qtde_faturado_nfe TYPE fkimg.
  DATA: saldo_ov TYPE zfit0026-mont_moeda.
  DATA: saldo_ov_dol TYPE zfit0026-mont_moeda.
  DATA: vlr_rec_fatura TYPE zfit0026-mont_moeda.
  FREE: it_color, t_cor.

**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - Início
**  RANGES: lr_hkont FOR bseg-hkont.
**
**  DATA: lv_hkont_aux TYPE bseg-hkont,
**        lv_htype     TYPE dd01v-datatype.
**
**  SELECT *
**    FROM tvarvc
**    INTO TABLE @DATA(lt_tvarvc)
**    WHERE name EQ 'CONTA_JUROS'.
**
**  IF sy-subrc EQ 0.
**    LOOP AT lt_tvarvc INTO DATA(lw_tvarvc).
**      CLEAR: lv_hkont_aux, lv_htype, lr_hkont.
**
**      lv_hkont_aux = lw_tvarvc-low.
**
**      CALL FUNCTION 'NUMERIC_CHECK'
**        EXPORTING
**          string_in  = lv_hkont_aux
**        IMPORTING
**          string_out = lv_hkont_aux
**          htype      = lv_htype.
**
**      IF lv_htype EQ 'NUMC'.
**        lr_hkont-sign   = lw_tvarvc-sign.
**        lr_hkont-option = lw_tvarvc-opti.
**        lr_hkont-low    = lv_hkont_aux.
**        APPEND lr_hkont.
**      ENDIF.
**    ENDLOOP.
**  ENDIF.
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - Fim
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************


  LOOP AT it_vbrk_mi INTO DATA(wa_vbrk).

    xtotalq_ov  = 0.
    xtotalvl_ov = 0.


    CLEAR vbeln_aux.
    pare = 0.
    vbeln_aux = i_saida-vbeln.

***    Pegando a data de vencimento da fatura.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
*    SELECT SINGLE *
    SELECT SINGLE bukrs, belnr, gjahr
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
      FROM bkpf
      INTO @DATA(w_bkpf)
        WHERE awkey EQ @wa_vbrk-vbeln.

    IF w_bkpf IS NOT INITIAL.

      DATA etl6964c6r9108 TYPE TABLE OF bseg.
      DATA rldnr_l6964c6r5690 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l6964c6r5690
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr   = rldnr_l6964c6r5690
            i_bukrs   = w_bkpf-bukrs
            i_belnr   = w_bkpf-belnr
            i_gjahr   = w_bkpf-gjahr
            i_buzei   = '01'
          IMPORTING
            et_bseg   = etl6964c6r9108
          EXCEPTIONS
            not_found = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl6964c6r9108 ) = 1.
        w_bseg = etl6964c6r9108[ 1 ].
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.

    ENDIF.

*    WA_SAIDA-DATA_VENC = W_BSEG-FDTAG.

    CALL METHOD zcl_solicitacao_ov=>get_t052_calc
      EXPORTING
        data_in  = w_bseg-zfbdt
        zterm    = w_bseg-zterm
      RECEIVING
        data_out = wa_saida-data_venc.

    " Referencia NFe
**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
*      SELECT *
    SELECT vbeln
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************
     FROM vbfa
     INTO CORRESPONDING FIELDS OF  TABLE it_vbfa_auxi
     WHERE vbeln = wa_vbrk-vbeln
     AND  vbtyp_n  = 'M'
     AND  vbtyp_v  = 'C'.

    LOOP AT it_vbfa_auxi  ASSIGNING FIELD-SYMBOL(<wa_vbfa_auxi>).
      <wa_vbfa_auxi>-refkey =  <wa_vbfa_auxi>-vbeln.
    ENDLOOP.

    IF it_vbfa_auxi IS NOT INITIAL.
      SELECT doc~nfenum
         FROM j_1bnflin AS lin
         INNER JOIN j_1bnfdoc AS doc ON  doc~docnum = lin~docnum
         INTO   TABLE @DATA(it_j_1bnfdoc)
         FOR ALL ENTRIES IN @it_vbfa_auxi
        WHERE lin~refkey = @it_vbfa_auxi-refkey.
    ENDIF.

    LOOP AT it_j_1bnfdoc INTO DATA(wa_j_1bnfdoc).
      wa_saida-referencia_nfe = wa_j_1bnfdoc-nfenum.
    ENDLOOP.


**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
*     SELECT *
    SELECT vbeln, nro_sol_ov
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************
     FROM zsdt0053 INTO TABLE @DATA(it_zsdt0053)
    WHERE vbeln = @vbeln_aux.

    LOOP AT it_zsdt0053 INTO DATA(wa_zsdt0053).
      wa_saida-vbeln_p =   wa_zsdt0053-vbeln.
      wa_saida-vbeln_s =   wa_zsdt0053-nro_sol_ov.
      wa_saida-vbeln_g =   wa_zsdt0053-vbeln.
*      WA_SAIDA-SAFRA   =   WA_ZSDT0053-CHARG.  "Safra "Comentado Aoenning
    ENDLOOP.

*    **    Buscando OV principal para preencher o cabecalho da fatura, caso não tenha recebimento.
    READ TABLE t_ov_primari ASSIGNING FIELD-SYMBOL(<w_ov>) WITH KEY vbeln = i_saida-vbeln.
    IF sy-subrc EQ 0.
      IF <w_ov>-status EQ 'Y'.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
*        SELECT SINGLE *
        SELECT SINGLE vbeln, vbelv
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
        FROM vbfa
        INTO @DATA(w_vbfa)
          WHERE vbeln = @<w_ov>-vbeln
            AND vbtyp_n EQ 'H'
            AND vbtyp_v EQ 'C'.

        IF w_vbfa IS NOT INITIAL.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = w_vbfa-vbeln.
          wa_saida-vbeln_p = w_vbfa-vbelv.
        ELSE.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = i_saida-vbeln_g.
          wa_saida-vbeln_p = i_saida-vbeln_p.
        ENDIF.
*
*
      ELSEIF <w_ov>-status EQ 'W'.
*
        CLEAR: w_vbfa.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
*        SELECT SINGLE *
        SELECT SINGLE vbeln, vbelv
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
        FROM vbfa
        INTO @w_vbfa
          WHERE vbeln = @<w_ov>-vbeln
            AND vbtyp_n EQ 'C'
            AND vbtyp_v EQ 'C'.

        IF w_vbfa IS NOT INITIAL.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = w_vbfa-vbeln.
          wa_saida-vbeln_p = w_vbfa-vbelv.
        ELSE.
          wa_saida-vbeln_s = i_saida-vbeln_s.
          wa_saida-vbeln_g = i_saida-vbeln_g.
          wa_saida-vbeln_p = i_saida-vbeln_p.
        ENDIF.
      ENDIF.
    ELSE.
      wa_saida-vbeln_s = i_saida-vbeln_s.
      wa_saida-vbeln_g = i_saida-vbeln_g.
      wa_saida-vbeln_p = i_saida-vbeln_p.
    ENDIF.


    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = i_saida-vbeln.
      ADD wa_vbap-kwmeng TO xtotalq_ov.
      ADD wa_vbap-netwr TO xtotalvl_ov.
      ADD wa_vbap-mwsbp TO xtotalvl_ov.
*      ADD WA_VBAP-NETWR TO WA_SAIDA-NETWR_L.
*      ADD WA_VBAP-MWSBP TO WA_SAIDA-MWSBP.
      CLEAR wa_vbap.
    ENDLOOP.

*    WA_SAIDA-NETWR_L       = WA_VBRK-NETWR. "Alteração nova
*    WA_SAIDA-MWSBP         = WA_VBRK-MWSBK. "Alteração nova
    wa_saida-vlr_tot_ref   = ( wa_vbrk-netwr + wa_vbrk-mwsbk ).

    "PAGAMENTO ANTECIPADO
**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
***    SELECT SINGLE *
***      FROM zsdt0052 INTO @DATA(wa_zsdt0052)
***      WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************
*    CASE WA_ZSDT0052-PGTO_ANT .             "Comentado Aoenning
*
*      WHEN 'X' .
*        WA_SAIDA-PGTO_ANT = ' Com Boleto '.
*
*      WHEN 'N' .
*        WA_SAIDA-PGTO_ANT = ' Sem Boleto '.
*
*      WHEN ' ' .
*        WA_SAIDA-PGTO_ANT = ' Não Antecipado '.
*
*    ENDCASE.

    " TAXA MULTA
**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
***    SELECT SINGLE *
***      FROM zsdt0051 INTO @DATA(wa_zsdt0051)
***      WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************
*    WA_SAIDA-TX_MULTA = WA_ZSDT0051-TX_MULTA. "Comentado Aoenning
*    WA_SAIDA-TX_JUROS = WA_ZSDT0051-TX_JUROS. "Comentado Aoenning

*&------------Inicio ajuste cs2024000717 / aoenning &*
    "Seleciona TVARVC ZSDT0060_TP_OV_DEVOLUCAO / Tipos de ordem de venda de devolução.
**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
    READ TABLE gt_ov_devolucao ASSIGNING FIELD-SYMBOL(<fs_ov_devolucao>)
     WITH KEY low = wa_vbak-auart
     BINARY SEARCH.
***    SELECT SINGLE * FROM tvarvc INTO  @DATA(wa_ov_devolucao)
***      WHERE name EQ 'ZSDT0060_TP_OV_DEVOLUCAO'
***       AND  low  EQ @wa_vbak-auart.
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************
*      IF wa_vbak-auart EQ 'ZRPF'  OR  wa_vbak-auart EQ 'ZROB'  OR  wa_vbak-auart EQ 'ZREB'.
    IF sy-subrc EQ 0.
*&------------Fim ajuste CS2024000717 / AOENNING &*
      xtotalq_ov  = xtotalq_ov  * -1.
      xtotalvl_ov = xtotalvl_ov * -1.

*      WA_SAIDA-NETWR_L = WA_SAIDA-NETWR_L * -1.  "Corrigido.
*      WA_SAIDA-MWSBP = WA_SAIDA-MWSBP * -1.      "Corrigido.

      IF wa_vbrk-vbeln NE doc_fatura. "Caso seja a mesma fatura não preencher a informação quantidade faturada.
*        WA_SAIDA-NETWR_L       = WA_VBRK-NETWR.
*        WA_SAIDA-MWSBP         = WA_VBRK-MWSBK.
        wa_saida-vlr_tot_ref   = ( wa_vbrk-netwr + wa_vbrk-mwsbk ).
      ENDIF.

    ELSEIF  i_saida-auart EQ 'ZREM' OR  i_saida-auart EQ 'ZRFU'.
      xtotalq_ov  = 0.
      xtotalvl_ov = 0.
    ENDIF.

* CONVERTE PARA NEGATIVO OS TIPOS ABAIXO CS2016000023 <<<<<<

    READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln = i_saida-vbeln BINARY SEARCH.
    READ TABLE it_kna1  INTO wa_kna1  WITH KEY kunnr = i_saida-kunnr BINARY SEARCH.
    READ TABLE it_vbkd  INTO wa_vbkd  WITH KEY vbeln = i_saida-vbeln_g BINARY SEARCH.

    READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm BINARY SEARCH.

*    SELECT SINGLE BEZEI
*    FROM TVGRT
*    INTO @DATA(Z_BEZEI)
*     WHERE SPRAS EQ @SY-LANGU
*       AND VKGRP EQ @I_SAIDA-VKGRP.

*    WA_SAIDA-VKGRP = I_SAIDA-VKGRP.      "Comentado Aoenning

*    WA_SAIDA-WERKS      = WA_VBAP-WERKS. "Comentado Aoenning
*    WA_SAIDA-VKGRP      = WA_VBAK-VKGRP. " Equipe Vendas
*    WA_SAIDA-VKBUR      = I_SAIDA-VKBUR. "Escr.Venda           "Comentado Aoenning
*    WA_SAIDA-KUNNR      = I_SAIDA-KUNNR. "Cod.cliente          "Comentado Aoenning
*    WA_SAIDA-NAME1      = WA_KNA1-NAME1. "Cliente              "Comentado Aoenning
*    WA_SAIDA-AUART      = I_SAIDA-AUART. "Tipo O.V.            "Comentado Aoenning
*    WA_SAIDA-ZTERM      = WA_VBKD-ZTERM. "Chave cond pagamento "Comentado Aoenning
*    WA_SAIDA-TEXT1      = WA_T052U-TEXT1. " Cond. Pgto "Comentado Aoenning

    wa_saida-vbeln      = i_saida-vbeln."
    wa_saida-vbeln_g    = i_saida-vbeln_g.
    wa_saida-vbeln_s    = i_saida-vbeln_s.
*    WA_SAIDA-ERDAT      = I_SAIDA-ERDAT. "Data de criação do registro  "Comentado Aoenning
    wa_saida-waerk      = i_saida-waerk. "Moeda do documento SD  "Comentado Aoenning
*    WA_SAIDA-SALD_REFERENCIA = WA_SAIDA-VLR_TOT_REF.

    xz_vlr_forte   = 0.
    xz_vlr_interna = 0.

    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln = i_saida-vbeln.

*      MOVE: WA_ZFIT0026-OBSERVACAO TO WA_SAIDA-OBSERVACAO.

      "Verifcar se o documento é boleto e se está compensado
      SELECT COUNT(*)
        FROM zsdt0054
        INTO @DATA(is_boleto)
        WHERE adiant =  @wa_zfit0026-docnum.

      " É boleto
      IF is_boleto > 0 .

        READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbrk-bukrs
                                                   kunnr = i_saida-kunnr
                                                   belnr = wa_zfit0026-docnum.

        IF wa_bsad-augdt IS NOT INITIAL.
          ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
          ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.
        ENDIF.
      ELSE.

        ADD wa_zfit0026-mont_moeda TO xz_vlr_forte.
        ADD wa_zfit0026-mont_mi    TO xz_vlr_interna.
      ENDIF.
    ENDLOOP.

    "QTDE FATURADO NFe
    DATA(t_zfit0026_group_by_doc_fatura) = it_zfit0026[].


    SORT t_zfit0026_group_by_doc_fatura BY doc_fatura.
    DELETE ADJACENT DUPLICATES FROM t_zfit0026_group_by_doc_fatura COMPARING doc_fatura.

*    LOOP AT T_ZFIT0026_GROUP_BY_DOC_FATURA INTO  DATA(WA_ZFIT26_GROUP_BY_DOC_FATURA) WHERE  VBELN = WA_VBAK-VBELN.
*    READ TABLE T_ZFIT0026_GROUP_BY_DOC_FATURA INTO WA_ZFIT26_GROUP_BY_DOC_FATURA WITH KEY VBELN = I_SAIDA-VBELN
*    DOC_FATURA = WA_VBRK-VBELN.


*    IF SY-SUBRC EQ 0.
*      IF WA_ZFIT26_GROUP_BY_DOC_FATURA-DOC_FATURA IS NOT INITIAL .
*      IF QTDE_FATURADO_NFE IS INITIAL.
    SELECT SUM( fkimg )
    FROM vbrp
    INTO qtde_faturado_nfe
   WHERE vbeln = wa_vbrk-vbeln.


    IF wa_vbrk-vbeln NE doc_fatura. "Caso seja a mesma fatura não preencher a informação quantidade faturada.
      wa_saida-fkimg = qtde_faturado_nfe.
    ENDIF.
*      ENDIF.
*    ENDIF.
    doc_fatura = wa_vbrk-vbeln.
*    ENDLOOP.
    "END QTDE FATURA.

* Salva totais
    xz_vlr_fortet   = xz_vlr_forte.
    xz_vlr_internat = xz_vlr_interna.
*
* Saldo moeda
    IF wa_vbak-waerk = 'USD'.
      xz_vlr_forte =  xtotalvl_ov -  xz_vlr_forte.
      xz_vlr_interna = 0.
    ELSE.
      xz_vlr_forte = 0.
      xz_vlr_interna = xtotalvl_ov - xz_vlr_interna.
    ENDIF.

    vvalor = xtotalq_ov.
*    WA_SAIDA-TOTALQ_OV = VVALOR. "Comentado porque o valor esta repetindo.

    vvalor = xtotalvl_ov.
*    WA_SAIDA-TOTVL_OV = VVALOR. "Comentado porque o valor esta repetindo.

    DATA: vflag TYPE i,
          vcont TYPE i.

    DATA: nr_ov TYPE vbak-vbeln.

    vflag = 0.
    vcont = 0.


    LOOP AT it_zfit0026 INTO wa_zfit0026 WHERE vbeln      = i_saida-vbeln
                                           AND doc_fatura = wa_vbrk-vbeln.

      READ TABLE it_zsdt0053 INTO DATA(w_zsdt0053) WITH KEY vbeln = wa_zfit0026-vbeln.
      IF sy-subrc EQ 0.
        wa_saida-vbeln_s = w_zsdt0053-nro_sol_ov.
      ENDIF.


**    Buscando OV principal para preencher as informações da OV principal no recebimento.
      READ TABLE t_ov_primari ASSIGNING <w_ov> WITH KEY vbeln = wa_zfit0026-vbeln.
      IF sy-subrc EQ 0.
        IF <w_ov>-status EQ 'Y'.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
*        SELECT SINGLE *
          SELECT SINGLE vbeln, vbelv
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
            FROM vbfa
            INTO @w_vbfa
              WHERE vbeln = @<w_ov>-vbeln
                AND vbtyp_n EQ 'H'
                AND vbtyp_v EQ 'C'.

          IF w_vbfa IS NOT INITIAL.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = w_vbfa-vbeln.
            wa_saida-vbeln_p = w_vbfa-vbelv.
          ELSE.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = i_saida-vbeln_g.
            wa_saida-vbeln_p = i_saida-vbeln_p.
          ENDIF.
*
*
        ELSEIF <w_ov>-status EQ 'W'.
*
          CLEAR: w_vbfa.
**********************************************************************
** Ajustes Performance - PANF - 28.08.24 - INICIO
*        SELECT SINGLE *
          SELECT SINGLE vbeln, vbelv
** Ajustes Performance - PANF - 28.08.24 - Fim
**********************************************************************
            FROM vbfa
            INTO @w_vbfa
              WHERE vbeln = @<w_ov>-vbeln
                AND vbtyp_n EQ 'C'
                AND vbtyp_v EQ 'C'.

          IF w_vbfa IS NOT INITIAL.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = w_vbfa-vbeln.
            wa_saida-vbeln_p = w_vbfa-vbelv.
          ELSE.
            wa_saida-vbeln_s = i_saida-vbeln_s.
            wa_saida-vbeln_g = i_saida-vbeln_g.
            wa_saida-vbeln_p = i_saida-vbeln_p.
          ENDIF.
        ENDIF.
      ELSE.
        wa_saida-vbeln_s = i_saida-vbeln_s.
        wa_saida-vbeln_g = i_saida-vbeln_g.
        wa_saida-vbeln_p = i_saida-vbeln_p.
      ENDIF.

      READ TABLE it_zsdt0051 INTO DATA(w_zsdt0051) WITH KEY nro_sol_ov = w_zsdt0053-nro_sol_ov.
      IF sy-subrc EQ 0.

        SELECT SINGLE bezei
        FROM tvgrt
        INTO @DATA(z_bezei)
         WHERE spras EQ @sy-langu
           AND vkgrp EQ @w_zsdt0051-vkgrp.

*        WA_SAIDA-VKBUR = W_ZSDT0051-VKBUR. "Escritório de venda  "Comentado Aoenning
*        WA_SAIDA-VKGRP = W_ZSDT0051-VKGRP && '-' && Z_BEZEI. "Equipe de venda "Comentado Aoenning
      ENDIF.
      wa_saida-waerk          = i_saida-waerk. "Moeda do documento SD  "Comentado Aoenning
      wa_saida-zid_lanc       = wa_zfit0026-zid_lanc.
      wa_saida-vlr_total_ov   = i_saida-totvl_ov.
      wa_saida-observacao     = wa_zfit0026-observacao.
      wa_saida-vlr_multa_calc = wa_zfit0026-vlr_multa_calc.  "MULTA CALCULADO
      wa_saida-vlr_multa_rbdo = wa_zfit0026-vlr_multa_rbdo.  "MULTA RECEBIDA

      wa_saida-vlr_juros_calc = wa_zfit0026-vlr_juros_calc.  "JUROS CALCULADO
      wa_saida-vlr_juros_rbdo = wa_zfit0026-vlr_juros_rbdo. "JUROS RECEBIDO

      wa_saida-vlr_desc_mult  = wa_zfit0026-vlr_desc_mult.
      wa_saida-vlr_desc_jros  = wa_zfit0026-vlr_desc_jros.


*      WA_SAIDA-VLR_SALD_FIN = ( WA_ZFIT0026-VLR_JUROS_CALC - WA_ZFIT0026-VLR_JUROS_RBDO - WA_ZFIT0026-VLR_DESC_JROS ) + ( WA_ZFIT0026-VLR_MULTA_CALC - WA_ZFIT0026-VLR_MULTA_RBDO - WA_ZFIT0026-VLR_DESC_MULT ).

      IF i_saida-waerk EQ 'USD'.
        wa_saida-vlr_sald_fin = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ELSE.
        wa_saida-vlr_sald_fin_brl = ( wa_zfit0026-vlr_juros_calc - wa_zfit0026-vlr_juros_rbdo - wa_zfit0026-vlr_desc_jros ) + ( wa_zfit0026-vlr_multa_calc - wa_zfit0026-vlr_multa_rbdo - wa_zfit0026-vlr_desc_mult ).
      ENDIF.

      wa_saida-ptax = wa_zfit0026-taxa. "PTAX
      wa_saida-data_pgto = wa_zfit0026-data_pgto.
      wa_saida-mont_rbdo = wa_zfit0026-mont_rbdo.


      ADD 1 TO vcont.

*****Preencher as informações de referente recebimento caso for uma ajuste, senão preencher os valores do documento.
      IF wa_zfit0026-ajuste IS NOT INITIAL.
        wa_saida-budat     = wa_zfit0026-data_pgto.

        IF i_saida-waerk EQ 'USD'.
          IF wa_zfit0026-mont_moeda IS NOT INITIAL.
            wa_saida-dmbtr     = ( wa_zfit0026-mont_moeda * wa_zfit0026-taxa ).
            wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          ELSE.
            wa_saida-dmbtr     = wa_zfit0026-mont_mi..
            wa_saida-dmbe2     = wa_zfit0026-mont_moeda.
          ENDIF.
        ELSE.
          IF wa_zfit0026-mont_moeda IS NOT INITIAL.
            wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
            wa_saida-dmbe2     = ( wa_zfit0026-mont_moeda / wa_zfit0026-taxa ).
          ELSE.
            wa_saida-dmbtr     = wa_zfit0026-mont_moeda.
            wa_saida-dmbe2     = wa_zfit0026-mont_mi.
          ENDIF.
        ENDIF.



        APPEND VALUE #( fname = 'AUGBL'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'BUDAT'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'DMBE2'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'DMBTR'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'VLR_MULTA_CALC'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'VLR_MULTA_RBDO'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'VLR_JUROS_CALC'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'VLR_JUROS_RBDO'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'VLR_DESC_MULT'     color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE #( fname = 'VLR_DESC_JROS'     color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.

        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.

      ELSE.

        READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbrk-bukrs
                                                   kunnr = i_saida-kunnr
                                                   belnr = wa_zfit0026-docnum.
        IF sy-subrc IS NOT INITIAL.

*          READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_zfit0026-obj_key.
*
*          READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbrk-bukrs
*                                                     kunnr = wa_vbak-kunnr
*                                                     belnr = wa_z0159-adiant.
          READ TABLE it_zibchv INTO wa_zibchv WITH KEY obj_key = wa_zfit0026-obj_key.

          READ TABLE it_bsad  INTO wa_bsad  WITH KEY bukrs = wa_vbrk-bukrs
                                                     kunnr = wa_vbak-kunnr
                                                     belnr = wa_zibchv-belnr.



        ENDIF.


        IF sy-subrc IS INITIAL.

          wa_saida-augbl     = wa_bsad-augbl.
          wa_saida-budat     = wa_bsad-augdt.

**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - Início
**          TABLES bseg.
**          SELECT SINGLE *              "#EC CI_DB_OPERATION_OK[2431747]
          SELECT COUNT(*)
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************
            FROM bseg
*            INTO TABLE @DATA(lt_bseg)
            WHERE bukrs = wa_bsad-bukrs
              AND belnr = wa_bsad-augbl
              AND gjahr = w_bseg-gjahr
**********************************************************************
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
*              AND hkont IN lr_hkont.
              AND hkont IN lr_contas.
** 152099 - performance ZSDT0060 - PANF - 28.08.24 - INICIO
**********************************************************************

          IF sy-subrc IS INITIAL.
            DATA(lv_contas_chk) = abap_true.
          ENDIF.
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - End

          IF i_saida-waerk EQ 'BRL'.
***     Calculando o valor recebido liquido.
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - Início
            IF lv_contas_chk IS NOT INITIAL.
              wa_saida-dmbtr = wa_bsad-dmbtr.
            ELSE.
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - Fim
              wa_saida-dmbtr = wa_bsad-dmbtr - ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - Início
            ENDIF.
*** RMNI - CS1001799 - Verificação de contas -  01.11.2022 - End
            CLEAR: ptax.
            ptax = ( wa_bsad-dmbtr / wa_bsad-dmbe2 ).
            IF ptax IS NOT INITIAL AND wa_bsad-dmbtr IS NOT INITIAL.
              juros_multa = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) / ptax.
              wa_saida-dmbe2    = wa_bsad-dmbe2 - juros_multa.
            ENDIF.
            CLEAR: juros_multa.
          ELSE.

*** Calculando o valor recebido liquido.
            wa_saida-dmbe2 = wa_bsad-dmbe2 - ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
            CLEAR: ptax.
            ptax = ( wa_bsad-dmbtr / wa_bsad-dmbe2 ).
            IF ptax IS NOT INITIAL.
              juros_multa = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) * ptax.
              wa_saida-dmbtr     = wa_bsad-dmbtr - juros_multa.
            ENDIF.
            CLEAR: juros_multa.
          ENDIF.


          APPEND VALUE #( fname = 'AUGBL'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'BUDAT'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'DMBE2'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'DMBTR'             color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'VLR_MULTA_CALC'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'VLR_MULTA_RBDO'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'VLR_JUROS_CALC'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'VLR_JUROS_RBDO'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'VLR_DESC_MULT'     color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE #( fname = 'VLR_DESC_JROS'     color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.

          APPEND LINES OF t_cor TO it_color.
          FREE  t_cor.
        ENDIF.
      ENDIF.

      wa_saida-forma_pag  = wa_zfit0026-forma_pag. "Comentado Aoenning
      wa_saida-taxa       = wa_zfit0026-taxa.      "Comentado Aoenning
      wa_saida-observacao = wa_zfit0026-observacao.
*      WA_SAIDA-DATA_PGTO  = WA_ZFIT0026-DATA_PGTO.

      wa_saida-mont_moeda = COND #( WHEN i_saida-waerk EQ 'USD' THEN wa_zfit0026-mont_moeda ELSE 0 ).
      wa_saida-mont_mi = COND #( WHEN i_saida-waerk EQ 'BRL' THEN wa_zfit0026-mont_mi ELSE 0 ).

      wa_saida-docnum     = wa_zfit0026-docnum.

      IF wa_saida-docnum IS INITIAL.
*        wa_saida-docnum = wa_z0159-adiant.
        wa_saida-docnum = wa_zibchv-belnr.
      ENDIF.

      APPEND VALUE #( fname = 'FORMA_PAG'  color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE #( fname = 'TAXA'       color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE #( fname = 'MONT_MOEDA' color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE #( fname = 'MONT_MI'    color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE #( fname = 'DOCNUM'     color-col = '5' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND LINES OF t_cor TO it_color.
      FREE  t_cor.

      IF vcont NE 0.

*        WA_SAIDA-MOEDA_FORTE = XZ_VLR_FORTE. "Comentado porque o valor esta repetindo.

*****************Verificando se ja existe lançamento saldo da OV.
        IF nr_ov NE i_saida-vbeln.
*          WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA. "Comentado porque o valor esta repetindo.
          APPEND VALUE  #( fname = 'MOEDA_INTER'      color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE  #( fname = 'MOEDA_FORTE'      color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE  #( fname = 'VLR_SALD_FIN'     color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE  #( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND VALUE  #( fname = 'SALD_REFERENCIA ' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
          APPEND LINES OF t_cor TO it_color.
          FREE  t_cor.
        ENDIF.


        APPEND VALUE  #( fname = 'MOEDA_FORTE'      color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'MOEDA_INTER'      color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'VLR_SALD_FIN'     color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'SALD_REFERENCIA ' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.
      ELSE.

        wa_saida-moeda_forte = 0.
        wa_saida-moeda_inter = 0.

        APPEND VALUE  #( fname = 'VLR_SALD_FIN_BRL' color-col = '2' color-int = '0' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'VLR_SALD_FIN'     color-col = '2' color-int = '0' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'MOEDA_FORTE'      color-col = '2' color-int = '0' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'MOEDA_INTER'      color-col = '2' color-int = '0' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'SALD_REFERENCIA ' color-col = '2' color-int = '0' color-inv = '1' ) TO t_cor.
        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.
        CLEAR wa_saida-line_color.
      ENDIF.

      wa_saida-salus = ( wa_saida-mont_moeda + wa_saida-moeda_forte ) - wa_saida-dmbe2.
      IF nr_ov NE i_saida-vbeln.
        wa_saida-salre = ( wa_saida-mont_mi    + wa_saida-moeda_inter ) - wa_saida-dmbtr.
      ENDIF.
      nr_ov = i_saida-vbeln.


      wa_saida-color_cell[] = it_color[].

*      IF RB1 = 'X'.
*        IF I_SAIDA-AUGBL IS NOT INITIAL.
*          APPEND WA_SAIDA TO IT_SAIDA.
*          VFLAG = 1.
*        ENDIF.
*      ELSEIF RB2 = 'X'.
*        IF ( I_SAIDA-WAERK = 'USD' AND XZ_VLR_FORTE NE 0 ) OR ( I_SAIDA-WAERK = 'BRL' AND XZ_VLR_INTERNA NE 0 ).
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.
*      ELSE.
**<<<------"152483 - NMS - INI------>>>
      wa_saida-tprel = gc_mi.         "MI = Mercado Interno
      wa_saida-tplin = sy-abcde+8(1). "I - Item
**<<<------"152483 - NMS - FIM------>>>
      APPEND wa_saida TO it_saida.
*      ENDIF.
      wa_saida-augbl     = ''.
      CLEAR: wa_saida-fkimg, wa_saida-netwr_l, wa_saida-mwsbp, wa_saida-vlr_tot_ref, wa_zfit0026, wa_bsad,
      wa_saida-dmbtr, wa_saida-dmbe2, wa_saida-vlr_sald_fin, wa_saida-vlr_sald_fin_brl, wa_saida-waerk, wa_z0159, wa_zibchv.
    ENDLOOP.

    IF vcont = 0.

      READ TABLE it_zsdt0053 INTO w_zsdt0053 WITH KEY vbeln = i_saida-vbeln_g.
      IF sy-subrc EQ 0.
        wa_saida-vbeln_s = w_zsdt0053-nro_sol_ov.
      ENDIF.

      READ TABLE it_zsdt0051 INTO w_zsdt0051 WITH KEY nro_sol_ov = w_zsdt0053-nro_sol_ov.
      IF sy-subrc EQ 0.
        SELECT SINGLE bezei
        FROM tvgrt
        INTO z_bezei
         WHERE spras EQ sy-langu
           AND vkgrp EQ w_zsdt0051-vkgrp.

*        WA_SAIDA-VKBUR = W_ZSDT0051-VKBUR.   "Comentado Aoenning
*        WA_SAIDA-VKGRP = W_ZSDT0051-VKGRP && '-' && Z_BEZEI.  "Comentado Aoenning
      ENDIF.

      wa_saida-mont_moeda  = 0.
      wa_saida-mont_mi     = 0.

      wa_saida-moeda_forte = 0.
      wa_saida-moeda_inter = 0.
      wa_saida-augbl     = wa_bsad-augbl.

*      WA_SAIDA-MOEDA_FORTE = XZ_VLR_FORTE. "Comentado porque o valor esta repetindo.

      IF nr_ov NE i_saida-vbeln.
*        WA_SAIDA-MOEDA_INTER = XZ_VLR_INTERNA.  "Comentado porque o valor esta repetindo.

        APPEND VALUE  #( fname = 'MOEDA_FORTE' color-col       = '6' color-int = '1' color-inv = '1' ) TO t_cor. "Comentado porque o valor esta repetindo.
        APPEND VALUE  #( fname = 'MOEDA_INTER' color-col       = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'VLR_SALD_FIN_BRL' color-col  = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'VLR_SALD_FIN' color-col      = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND VALUE  #( fname = 'SALD_REFERENCIA ' color-col  = '6' color-int = '1' color-inv = '1' ) TO t_cor.
        APPEND LINES OF t_cor TO it_color.
        FREE  t_cor.

      ENDIF.
      nr_ov = i_saida-vbeln.

      APPEND VALUE  #( fname = 'MOEDA_FORTE' color-col      = '6' color-int = '1' color-inv = '1' ) TO t_cor. "Comentado porque o valor esta repetindo.
      APPEND VALUE  #( fname = 'MOEDA_INTER' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE  #( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE  #( fname = 'VLR_SALD_FIN' color-col     = '6' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND VALUE  #( fname = 'SALD_REFERENCIA ' color-col = '6' color-int = '1' color-inv = '1' ) TO t_cor.
      APPEND LINES OF t_cor TO it_color.
      FREE  t_cor.

      wa_saida-color_cell[] = it_color[].
*      WA_SAIDA-LINE_COLOR  = 'C310'.

      READ TABLE it_zfit0026 INTO wa_zfit0026 WITH KEY vbeln = i_saida-vbeln BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
*        WA_SAIDA-SALUS = WA_SAIDA-MOEDA_FORTE. "Comentado porque o valor esta repetindo.
*        WA_SAIDA-SALRE = WA_SAIDA-MOEDA_INTER.  "Comentado porque o valor esta repetindo.
      ENDIF.

****  Atualizada saldo da referencia.
*      WA_SAIDA-SALD_REFERENCIA = ( WA_SAIDA-SALD_REFERENCIA - .

*      IF RB1 = 'X'.
*        IF VFLAG = 1.
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.
*      ELSEIF RB2 = 'X'.
*        IF ( WA_SAIDA-WAERK = 'USD' AND WA_SAIDA-MOEDA_FORTE NE 0 ) OR ( WA_SAIDA-WAERK = 'BRL' AND WA_SAIDA-MOEDA_INTER NE 0 ).
*          APPEND WA_SAIDA TO IT_SAIDA.
*        ENDIF.
*      ELSE.
**<<<------"152483 - NMS - INI------>>>
      wa_saida-tprel = gc_mi.         "MI = Mercado Interno
      wa_saida-tplin = sy-abcde+8(1). "I - Item
**<<<------"152483 - NMS - FIM------>>>
      APPEND wa_saida TO it_saida.
*      ENDIF.
    ENDIF.

*    CLEAR: WA_SAIDA.

    CLEAR: wa_vbak, wa_vbap, wa_kna1, wa_vbkd, wa_zfit0026, wa_bsad, wa_t052u, wa_saida, wa_color, wa_0090, wa_0090x, wa_0041.
    FREE it_color.
  ENDLOOP.


*
*ATUALIZA VALORES DA IT_SAIDA
*  LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<F_SAIDA>).
**    <F_SAIDA>-RFMNG = REDUCE RFMNG( INIT X TYPE RFMNG FOR LS IN IT_REMESSA WHERE ( VBELV EQ <F_SAIDA>-VBELN_G ) NEXT X = X + LS-RFMNG ).
*  ENDLOOP.

* ATUALIZA saldo da OV.
  saldo_ov = i_saida-totvl_ov.
  saldo_ov_dol = i_saida-totvl_ov.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>) WHERE vbeln EQ i_saida-vbeln.
*    IF saldo_ov IS NOT INITIAL.
    saldo_ov  = ( saldo_ov - <f_saida>-dmbtr ).
*    ENDIF.

*    IF saldo_ov_dol IS NOT INITIAL.
    saldo_ov_dol = ( saldo_ov_dol - <f_saida>-dmbe2 ).
*    ENDIF.
  ENDLOOP.

  DATA(t_faturas) = it_saida.
  SORT t_faturas ASCENDING BY referencia_nfe.
  DELETE ADJACENT DUPLICATES FROM t_faturas  COMPARING referencia_nfe.


****Atualizando o saldo da fatura.
  LOOP AT t_faturas ASSIGNING FIELD-SYMBOL(<w_fatura>).
    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_said>) WHERE vbeln EQ i_saida-vbeln AND referencia_nfe EQ <w_fatura>-referencia_nfe.
      IF <wa_said>-waerk <> 'BRL'.
        ADD <wa_said>-dmbe2 TO vlr_rec_fatura.
      ELSE.
        ADD <wa_said>-dmbtr TO vlr_rec_fatura.
      ENDIF.
    ENDLOOP.

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<l_saida>) WITH KEY vbeln = i_saida-vbeln
                                                          referencia_nfe = <w_fatura>-referencia_nfe.
    IF sy-subrc EQ 0.
      <l_saida>-sald_referencia = ( <l_saida>-vlr_tot_ref - vlr_rec_fatura ).
      CLEAR: vlr_rec_fatura.
    ENDIF.
    CLEAR: vlr_rec_fatura.
  ENDLOOP.



  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<w_saida>) WITH KEY vbeln = i_saida-vbeln.
  IF sy-subrc EQ 0.
    IF <w_saida>-waerk <> 'BRL'.
      <w_saida>-moeda_forte = saldo_ov_dol.
    ELSE.
      <w_saida>-moeda_inter = saldo_ov.
    ENDIF.

*    <W_SAIDA>-MOEDA_INTER = SALDO_OV.
*    <W_SAIDA>-MOEDA_FORTE = SALDO_OV_DOL.
  ENDIF.
  CLEAR: saldo_ov, saldo_ov_dol.

****  Atualizada saldo da referencia.
*      WA_SAIDA-SALD_REFERENCIA = VLR_TOT_REF.


*  CHECK RB2 EQ ABAP_TRUE.

*  LOOP AT IT_SAIDA INTO WA_SAIDA.
*
*    WA_DELETE =
*    VALUE #(
*             VBELN_S     = WA_SAIDA-VBELN_S
*             VBELN_P     = WA_SAIDA-VBELN_P
*             MOEDA_FORTE = WA_SAIDA-MOEDA_FORTE
*             MOEDA_INTER = WA_SAIDA-MOEDA_INTER
*           ).
*
*    COLLECT WA_DELETE INTO IT_DELETE.
*    CLEAR WA_DELETE.
*
*  ENDLOOP.
*
*  DELETE IT_DELETE WHERE MOEDA_FORTE > 10 OR  MOEDA_INTER > 10.
*
*  LOOP AT IT_DELETE INTO WA_DELETE.
*    DELETE IT_SAIDA WHERE VBELN_S EQ WA_DELETE-VBELN_S AND
*                          VBELN_P EQ WA_DELETE-VBELN_P.
*  ENDLOOP.

ENDFORM.

**********************************************************************
* pegra os paramentros para a Transação zfis26 / 110302 CS2023000291 Melhoria Simples ZSDT0060 - PSA
**********************************************************************
FORM parametros_zfis26.
  FREE lr_vbeln.
*  FREE: lr_werks,lr_bukrs,lr_vkbur,lr_kunnr,lr_vbeln,lr_erdat.
*
*  lr_werks[] = VALUE #( FOR wa_params1 IN it_saida WHERE ( werks <> space ) ( option = 'EQ' sign = 'I' low = wa_params1-werks ) ).
*  SORT lr_werks BY low.
*  DELETE ADJACENT DUPLICATES FROM lr_werks COMPARING ALL FIELDS.
*
*  SELECT DISTINCT 'I', 'EQ', bukrs FROM j_1bbranch
*    INTO TABLE @lr_bukrs
*    WHERE branch IN @lr_werks.
*  lr_vkbur[] = VALUE #( FOR wa_params2 IN it_saida WHERE ( vkbur <> space ) ( option = 'EQ' sign = 'I' low = wa_params2-vkbur ) ).
*  lr_kunnr[] = VALUE #( FOR wa_params3 IN it_saida WHERE ( kunnr <> space ) ( option = 'EQ' sign = 'I' low = wa_params3-kunnr ) ).
  lr_vbeln[] = VALUE #( FOR wa_params4 IN it_saida WHERE ( vbeln <> space ) ( option = 'EQ' sign = 'I' low = wa_params4-vbeln ) ).


*  SORT it_saida ASCENDING BY erdat.
*  READ TABLE it_saida INTO DATA(wa_params5) INDEX 1.
*
*  SORT it_saida DESCENDING BY erdat.
*  READ TABLE it_saida INTO DATA(wa_params6) INDEX 1.
*
*  lr_erdat[] = VALUE #( ( option = 'BT' sign = 'I' low = wa_params5-erdat high = wa_params6-erdat ) ).

  SORT: lr_vbeln BY low.
*        lr_bukrs BY low,
*        lr_vkbur BY low,
*        lr_kunnr BY low,
*

*  DELETE ADJACENT DUPLICATES FROM lr_bukrs COMPARING ALL FIELDS.
*  DELETE ADJACENT DUPLICATES FROM lr_vkbur COMPARING ALL FIELDS.
*  DELETE ADJACENT DUPLICATES FROM lr_kunnr COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM lr_vbeln COMPARING ALL FIELDS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.

  CASE sy-ucomm.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN  0.

    WHEN 'SAIR'.
      LEAVE TO SCREEN  0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.

  CASE sy-ucomm.
    WHEN 'SAIR'.
      LEAVE TO SCREEN  0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET PF-STATUS 'STATUS_1001'.
  SET TITLEBAR 'TITLEBAR_1001'.

***      Ocutar campos de recebimento parcial.
  PERFORM ocutar_campos_juros.
  PERFORM ocutar_campos_fatura.

  IF ind_rec_parc IS NOT INITIAL.
    PERFORM ocutar_campo_rec_parc.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1002 OUTPUT.
  SET PF-STATUS 'STATUS_1002'.
  SET TITLEBAR 'TITLEBAR_1002'.

  PERFORM oculta_campo_dolar.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_MEMORIA_CALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida_memoria_calculo USING wa_saida_p TYPE ty_saida.

  DATA qtde_dias_atraso TYPE int4.
  DATA data_venc_format TYPE char20.
  DATA data_pgto_format TYPE char20.
  DATA: total_ov TYPE vbap-netwr.
  DATA: vlr_total TYPE vbap-netwr.
  DATA: total_ov_parc TYPE vbap-netwr.
  CLEAR: wa_edit, w_edit, total_ov, total_ov_parc, ind_rec_total, ind_doc_fatura, ind_rec_parc.

*  DATA_VENC_FORMAT = | { WA_SAIDA_P-DATA_VENC+6(2) }.{ WA_SAIDA_P-DATA_VENC+4(2) }.{ WA_SAIDA_P-DATA_VENC(4) }|.
*  DATA_PGTO_FORMAT = | { WA_SAIDA_P-DATA_PGTO+6(2) }.{ WA_SAIDA_P-DATA_PGTO+4(2) }.{ WA_SAIDA_P-DATA_PGTO(4) }|.
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_MULTA =  | Valor Multa = { WA_SAIDA_P-MONT_RBDO }(Mont.RBDO) * { WA_SAIDA_P-TX_MULTA }(Tx.Multa)%  = { WA_SAIDA_P-VLR_MULTA_CALC }|.
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA_ULTIL = |Se a data de vencimento { DATA_VENC_FORMAT } for dia não útil e a data de pagamento { DATA_PGTO_FORMAT } for primeiro dia útil subsequente a data de vencimento { DATA_VENC_FORMAT }  |.



*  CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*    EXPORTING
*      I_DATUM_BIS = WA_SAIDA_P-DATA_VENC  " Data Maior
*      I_DATUM_VON = WA_SAIDA_P-DATA_PGTO  " Data Menor
*    IMPORTING
*      E_TAGE      = QTDE_DIAS_ATRASO. " Dieferença em dias
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_DIAS_ATRASO = |Quantidade de dias em atraso: {  COND #( WHEN  QTDE_DIAS_ATRASO = 0 OR QTDE_DIAS_ATRASO IS INITIAL THEN 0 ELSE QTDE_DIAS_ATRASO   ) } dias. | .
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA = 0.
*
*  IF  WA_SAIDA-TX_JUROS IS NOT INITIAL AND WA_SAIDA-TX_JUROS <> 0 .
*
*    WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA  =  | Valor de Juros ao dia: { WA_SAIDA-MONT_RBDO *  QTDE_DIAS_ATRASO *  ( WA_SAIDA-TX_JUROS / 360 ) }%|.

*  ENDIF.

*===============================================================================

*  SELECT *  FROM VBAP INTO TABLE @DATA(T_VBAP)
*          WHERE VBELN EQ @WA_SAIDA-VBELN.
*
*  CLEAR TOTAL_OV.
*  LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN = WA_SAIDA-VBELN.
*    ADD WA_VBAP-NETWR TO TOTAL_OV.
*    ADD WA_VBAP-MWSBP TO TOTAL_OV.
*  ENDLOOP.

****  Selecionar informações zfit0026.
  SELECT SINGLE *
  FROM zfit0026
    INTO @DATA(w_zfit0026)
     WHERE zid_lanc EQ @wa_saida-zid_lanc.

  CHECK w_zfit0026 IS NOT INITIAL.
*  W_EDIT-DOC_FATURA = W_ZFIT0026-DOC_FATURA.
  w_edit-doc_fatura = wa_saida-referencia_nfe.
  w_edit-vbeln = w_zfit0026-vbeln.



  IF w_zfit0026-doc_fatura IS INITIAL.
****   Buscar o saldo total e parcial da OV.
    zcl_dados_ov=>i_vlr_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc
        e_lifsp_12    = DATA(lv_lifsp_12) ).


    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).

  ELSE.
****     Buscar o saldo total e parcial da referencia.
    zcl_dados_ov=>i_vlr_referencia_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
        i_vbelnn      = w_zfit0026-doc_fatura
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).
*    W_EDIT-TXT_DOC = 'Calculo com base na referencia'.

    CLEAR: total_ov.
    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).
    ind_doc_fatura = abap_true.
  ENDIF.

***   Achar o % fator

  IF rb8 IS NOT INITIAL. "Se mercado interno for selecionado.

    IF wa_saida-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        wa_edit-dias_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).
        w_edit-dias_atraso = wa_edit-dias_atraso.

        SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov EQ @wa_saida-vbeln_s.

        wa_edit-data_venc    =    w_zfit0026-data_venc.
        wa_edit-data_pgto    =    w_zfit0026-data_pgto.
        wa_edit-fator      = ( wa_zsdt0051-tx_juros / 360 ) * wa_edit-dias_atraso.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
          DATA(t_ov)              = ( wa_edit-juros + total_ov + wa_edit-multa ). "Total da OV + juros.

        ELSE.

          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
          t_ov               = ( wa_edit-juros + vlr_total + wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / t_ov )  * 100. " Porcentagem proporcional ao valor do juros
          DATA(prop_multa)   = ( wa_edit-multa / t_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          DATA(vlr_multa)    = ( prop_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

        wa_edit-tx_jros      = wa_zsdt0051-tx_juros.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        wa_edit-dias_ano      = '360'.
        w_edit-fator          = wa_edit-fator.
        w_edit-juros          = wa_edit-juros.
        wa_edit-jros          = wa_edit-juros.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_juros     = wa_edit-porc_juros.
        wa_edit-porc          = '100'.
        w_edit-porc           = '100'.
        w_edit-por            = '100'.


        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_juros = ' '.
          wa_edit-vlr_rbdo = ' '.
          w_edit-por = ' '.
          wa_edit-juros_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSEIF rb7 IS NOT INITIAL. "Se insumo interno for selecionado.
*    CLEAR: D_ATRASO.
    IF wa_saida-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        wa_edit-dias_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).
        w_edit-dias_atraso = wa_edit-dias_atraso.

        SELECT SINGLE *  FROM zsdt0040 INTO @DATA(wa_zsdt0040)
        WHERE doc_simulacao EQ @wa_saida-vbeln_s.

        wa_edit-data_venc    =    w_zfit0026-data_venc.
        wa_edit-data_pgto    =    w_zfit0026-data_pgto.
        wa_edit-fator      = ( wa_zsdt0040-juros_ano / 360 ) * wa_edit-dias_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

        IF lv_lifsp_12 IS INITIAL.

          IF w_zfit0026-rec_vlr_total IS NOT INITIAL.

            wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
            wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
            DATA(to_ov)        = ( wa_edit-juros + total_ov + wa_edit-multa ). "Total da OV + juros.

          ELSE.
            wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
            wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
            to_ov        = ( wa_edit-juros + vlr_total + wa_edit-multa ). "Total da OV + juros.

            wa_edit-porc_juros = ( wa_edit-juros / to_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa         = ( wa_edit-multa / to_ov )  * 100. " Porcentagem proporcional da multa.

            wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
            vlr_multa          = ( prop_multa * w_zfit0026-mont_rbdo ) / 100.
            total_ov = vlr_total.
          ENDIF.

        ELSE.

          wa_edit-porc_juros  = wa_saida-vlr_juros_calc / w_zfit0026-mont_rbdo * 100.
          wa_edit-juros_parc  = wa_saida-vlr_juros_calc.

        ENDIF.

        wa_edit-tx_jros       = wa_zsdt0040-juros_ano.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        wa_edit-dias_ano      = '360'.
        w_edit-fator          = wa_edit-fator.
        w_edit-juros          = wa_edit-juros.
        wa_edit-jros          = wa_edit-juros.
*        WA_EDIT-MULTA         = VLR_MULT.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_juros     = wa_edit-porc_juros.
        wa_edit-porc          = '100'.
        w_edit-porc           = '100'.
        w_edit-por            = '100'.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_juros = ' '.
          wa_edit-vlr_rbdo = ' '.
          w_edit-por = ' '.
          wa_edit-juros_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: w_zfit0026, total_ov, to_ov, vlr_total.
  ENDIF.

ENDFORM.

FORM f_pf_standard_fullscreen USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.                    "f01_alv_event_pf_status_set
*&---------------------------------------------------------------------*
*&      Module  F_CALCULAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_calcular INPUT.
  DATA qtde_dias_atraso TYPE int4.
  DATA: maior_data TYPE zfit0026-data_venc,
        menor_data TYPE zfit0026-data_venc.

  DATA: fator      TYPE kurrf,
        vlr_juros  TYPE netwr,
        vlr_mult   TYPE netwr,
        vlr_tot_ov TYPE netwr,
        prop_juros TYPE kurrf,
        prop_multa TYPE kurrf.


***Limpando os campos da tela.
  CLEAR:
  wa_saida_calculadora-vlr_dolar_final,
  wa_saida_calculadora-vlr_juros_usd,
  wa_saida_calculadora-vlr_multa_usd,
  wa_saida_calculadora-vlr_total_usd,
  wa_saida_calculadora-vlr_original_brl,
  wa_saida_calculadora-vlr_juros_brl,
  wa_saida_calculadora-vlr_multa_brl,
  wa_saida_calculadora-vlr_total_brl,
*WA_SAIDA_CALCULADORA-MOEDA,
  wa_saida_calculadora-dia_atraso,
  wa_saida_calculadora-tx_multa,
  wa_saida_calculadora-tx_juros,
  wa_saida_calculadora-tx_multa_prop,
  wa_saida_calculadora-tx_juros_prop.


  IF wa_saida_calculadora-data_pagamento IS INITIAL.
    MESSAGE 'Informada a data de pagamento' TYPE 'I'.
    EXIT.
  ENDIF.


  IF wa_saida_calculadora-data_pagamento < lw_zfit0026-data_venc.
    MESSAGE TEXT-056 TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = wa_saida_calculadora-data_pagamento
                                IMPORTING e_subrc      = DATA(is_dia_util)
                                         ).

  IF is_dia_util <> 4 AND wa_saida_calculadora-data_pagamento IS NOT INITIAL.

    MESSAGE 'Data informada não é dia útil' TYPE 'I'.

***Limpando os campos da tela.
    CLEAR:
   wa_saida_calculadora-vlr_dolar_final,
   wa_saida_calculadora-vlr_juros_usd,
   wa_saida_calculadora-vlr_multa_usd,
   wa_saida_calculadora-vlr_total_usd,
   wa_saida_calculadora-vlr_original_brl,
   wa_saida_calculadora-vlr_juros_brl,
   wa_saida_calculadora-vlr_multa_brl,
   wa_saida_calculadora-vlr_total_brl,
*WA_SAIDA_CALCULADORA-MOEDA,
   wa_saida_calculadora-dia_atraso,
   wa_saida_calculadora-tx_multa,
   wa_saida_calculadora-tx_juros,
   wa_saida_calculadora-tx_multa_prop,
   wa_saida_calculadora-tx_juros_prop.
    EXIT.
  ELSE.

*    IF wa_saida_calculadora-data_pagamento < sy-datum.

    CLEAR: vg_periodo_base, vg_periodo_atual.
    vg_periodo_base = |{ wa_saida_calculadora-data_pagamento+0(4) }{ wa_saida_calculadora-data_pagamento+4(2) }|.
    vg_periodo_atual = |{ sy-datum+0(4) }{ sy-datum+4(2) }|.


    IF vg_periodo_base < vg_periodo_atual.
*      MESSAGE 'Data informada não pode ser menor que a data atual.' TYPE 'I' DISPLAY LIKE 'E'.
      MESSAGE 'Data informada é menor que o ultimo dia do mês vigente atual.' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
    CREATE OBJECT obj_zcl_util_sd.

    DATA: vl_gdatu     TYPE gdatu_inv,
*          VL_UKURS     TYPE UKURS_CURR, "TAXA
          tx_juros_dia TYPE netwr.

    " BUSCA O PRÓXIMO DIA UTIL DA DATA DE VENCIMENTO.

    zcl_miro=>get_proximo_dia_util(
      EXPORTING
        i_data_base        = wa_saida-data_venc
        i_signum           = '+'
        i_ck_data_zles0145 = abap_true
      RECEIVING
        r_data             = DATA(v_dt_proximo_dia_util)
      EXCEPTIONS
        erro               = 1 ).


***    Check se data de vencimento caiu num feriado e verificar se a data de pagamento é a mesma data de pagamento se for diferente considera a data de vencimento.
    IF wa_saida-data_venc NE v_dt_proximo_dia_util.
      IF v_dt_proximo_dia_util EQ wa_saida_calculadora-data_pagamento.

      ELSE.
        v_dt_proximo_dia_util =  wa_saida-data_venc.
      ENDIF.
    ENDIF.




*================================================================================================================
    IF rb8 IS INITIAL. "Se mercado interno não for selecionado, trazer informações referente insumo.

      IF wa_saida-waerk  <> 'BRL' . "Se for moeda em Dolar.

***  Calculo para moeda em Dolar.

        "BUSCA A TAXA
        CLEAR: vl_ukurs.
        obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
        obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).


        MOVE  wa_saida_calculadora-data_pagamento TO vl_gdatu.
        obj_zcl_util_sd->set_data( vl_gdatu ).
        vl_ukurs = obj_zcl_util_sd->taxa_cambio_dia( ).


***       Selecionar a taxa de juros.
        CLEAR: wa_zsdt0040.
        SELECT SINGLE *  FROM zsdt0040 INTO wa_zsdt0040
        WHERE doc_simulacao EQ wa_saida-vbeln_s.

        wa_saida_calculadora-vlr_dolar_final = vl_ukurs.

        " AQUI CALCULA O JUROS
*          IF  WA_SAIDA_CALCULADORA-DATA_PAGAMENTO > V_DT_PROXIMO_DIA_UTIL .
        IF wa_saida_calculadora-data_pagamento > v_dt_proximo_dia_util.

          maior_data = wa_saida_calculadora-data_pagamento.
          menor_data = v_dt_proximo_dia_util.

        ELSE.
          maior_data = v_dt_proximo_dia_util .
          menor_data =  wa_saida_calculadora-data_pagamento.
        ENDIF.

*        CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*          EXPORTING
*            I_DATUM_BIS = MAIOR_DATA  " Data Maior
*            I_DATUM_VON = MENOR_DATA  " Data Menor
*          IMPORTING
*            E_TAGE      = QTDE_DIAS_ATRASO. " Dieferença em dias

        IF wa_zsdt0040-juros_ano IS INITIAL.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH | Não existem taxa de juros cadastrada. |.
          EXIT.
        ENDIF.

        IF wa_saida-moeda_forte IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov = wa_saida-moeda_forte.
          wa_saida_calculadora-vlr_total_ov = wa_saida-totvl_ov.
        ELSEIF wa_saida-sald_referencia IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov = wa_saida-sald_referencia.
          wa_saida_calculadora-vlr_total_ov = wa_saida-vlr_tot_ref.
        ENDIF.

        qtde_dias_atraso = wa_saida_calculadora-data_pagamento - v_dt_proximo_dia_util.
        wa_saida_calculadora-fator  = ( wa_zsdt0040-juros_ano / 360 ) * qtde_dias_atraso. " Fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).
        vlr_juros        = ( wa_saida_calculadora-vlr_saldo_ov * wa_saida_calculadora-fator ) / 100. "Valor dos juros com base no valor total da OV.
        vlr_mult         = ' '.
        vlr_tot_ov       = ( wa_saida_calculadora-vlr_saldo_ov + vlr_juros + vlr_mult ).

        wa_saida_calculadora-tx_juros_prop =  ( vlr_juros / vlr_tot_ov ) * 100.
        wa_saida_calculadora-tx_multa_prop =  ( vlr_mult  / vlr_tot_ov ) * 100.


        wa_saida_calculadora-tx_multa  = ' '.
        wa_saida_calculadora-tx_juros  = wa_zsdt0040-juros_ano.

        wa_saida_calculadora-dia_atraso       =   qtde_dias_atraso.
        wa_saida_calculadora-vlr_original_usd =   wa_saida_calculadora-vlr_saldo_ov. "Saldo da OV.
*          WA_SAIDA_CALCULADORA-VLR_JUROS_USD    =   ( WA_SAIDA_CALCULADORA-TX_JUROS_PROP *  WA_SAIDA-MOEDA_FORTE ) / 100.  "Valor do juros com base no valor total da OV.
*          WA_SAIDA_CALCULADORA-VLR_MULTA_USD    =   ( WA_SAIDA_CALCULADORA-TX_MULTA_PROP *  WA_SAIDA-MOEDA_FORTE ) / 100.

        wa_saida_calculadora-vlr_juros_usd    =   vlr_juros.
        wa_saida_calculadora-vlr_multa_usd    =   vlr_mult.
        wa_saida_calculadora-vlr_total_usd    =   ( wa_saida_calculadora-vlr_original_usd + wa_saida_calculadora-vlr_multa_usd + wa_saida_calculadora-vlr_juros_usd ).

        IF vl_ukurs IS NOT INITIAL.
          PERFORM oculta_campo_dolar.
          wa_saida_calculadora-vlr_dolar_final = vl_ukurs.
          wa_saida_calculadora-vlr_original_brl = ( wa_saida_calculadora-vlr_saldo_ov * vl_ukurs ).
          wa_saida_calculadora-vlr_juros_brl    = ( wa_saida_calculadora-vlr_juros_usd * vl_ukurs )."Valor do juros com base no valor total da OV.
          wa_saida_calculadora-vlr_multa_brl    = ( wa_saida_calculadora-vlr_multa_usd * vl_ukurs ).
          wa_saida_calculadora-vlr_total_brl    = ( wa_saida_calculadora-vlr_original_brl + wa_saida_calculadora-vlr_juros_brl + wa_saida_calculadora-vlr_multa_brl ).

          CLEAR: qtde_dias_atraso, fator, vlr_juros, vlr_mult, vlr_tot_ov, prop_juros, prop_multa.

*        ELSE.
*          MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH | Não existem taxa de cambio cadastrada para esse periodo. |.
        ENDIF.

      ELSE. "Se for moeda em BLR.

**         Calcular os dias em atraso, com base em dias úteis.
        qtde_dias_atraso = wa_saida_calculadora-data_pagamento - v_dt_proximo_dia_util.

        wa_saida_calculadora-vlr_original_usd = 0.
        wa_saida_calculadora-vlr_juros_usd    = 0.
        wa_saida_calculadora-vlr_multa_usd    = 0.
        wa_saida_calculadora-vlr_total_usd    = 0.


***       Selecionar a taxa de juros.
        SELECT SINGLE *  FROM zsdt0040 INTO wa_zsdt0040
        WHERE doc_simulacao EQ wa_saida-vbeln_s.

        IF wa_zsdt0040-juros_ano IS INITIAL.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH | Não existem taxa de juros cadastrada. |.
          EXIT.
        ENDIF.

        IF wa_saida-moeda_forte IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov  =  wa_saida-moeda_inter.
          wa_saida_calculadora-vlr_total_ov   = wa_saida-totvl_ov.
        ELSEIF wa_saida-sald_referencia IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov = wa_saida-sald_referencia.
          wa_saida_calculadora-vlr_total_ov = wa_saida-vlr_tot_ref.
        ENDIF.

        qtde_dias_atraso  = wa_saida_calculadora-data_pagamento - v_dt_proximo_dia_util.
        wa_saida_calculadora-fator = ( wa_zsdt0040-juros_ano / 360 ) * qtde_dias_atraso. " Fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).
        vlr_juros         = ( wa_saida_calculadora-vlr_saldo_ov * wa_saida_calculadora-fator ) / 100. "Valor dos juros com base no valor total da OV.
        vlr_mult          = ' '.
        vlr_tot_ov        = ( wa_saida_calculadora-vlr_saldo_ov + vlr_juros + vlr_mult ).

        wa_saida_calculadora-tx_juros_prop =  ( vlr_juros / vlr_tot_ov ) * 100.
        wa_saida_calculadora-tx_multa_prop =  ( vlr_mult  / vlr_tot_ov ) * 100.


        wa_saida_calculadora-tx_multa  = ' '.
        wa_saida_calculadora-tx_juros  = wa_zsdt0040-juros_ano.

        wa_saida_calculadora-dia_atraso       = qtde_dias_atraso.
        wa_saida_calculadora-vlr_original_brl = wa_saida_calculadora-vlr_saldo_ov.
*        WA_SAIDA_CALCULADORA-VLR_JUROS_BRL    = ( WA_SAIDA-MOEDA_INTER * WA_SAIDA_CALCULADORA-TX_JUROS_PROP ) / 100.  "Valor do juros com base no valor total da OV.
*        WA_SAIDA_CALCULADORA-VLR_MULTA_BRL    = ( WA_SAIDA-MOEDA_INTER * WA_SAIDA_CALCULADORA-TX_MULTA_PROP ) / 100.

        wa_saida_calculadora-vlr_juros_brl    = vlr_juros.
        wa_saida_calculadora-vlr_multa_brl    = vlr_mult.
        wa_saida_calculadora-vlr_total_brl    = ( wa_saida_calculadora-vlr_original_brl + wa_saida_calculadora-vlr_multa_brl + wa_saida_calculadora-vlr_juros_brl ).

        CLEAR: qtde_dias_atraso, fator, vlr_juros, vlr_mult, vlr_tot_ov, prop_juros, prop_multa.
      ENDIF.
    ELSE. "Selecionar informações relacionada ao mercado interno.
      CLEAR: fator, vlr_juros, vlr_mult, vlr_tot_ov, prop_juros, prop_multa.
      IF wa_saida-waerk  <> 'BRL' . "Se for moeda em Dolar.

***  Calculo para moeda em Dolar.

*        "BUSCA A TAXA
        CLEAR: vl_ukurs.
        obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
        obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).


        MOVE  wa_saida_calculadora-data_pagamento TO vl_gdatu.
        obj_zcl_util_sd->set_data( vl_gdatu ).
        vl_ukurs = obj_zcl_util_sd->taxa_cambio_dia( ).

***       Selecionar a taxa de juros.
        SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov EQ @wa_saida-vbeln_s.

        IF wa_zsdt0051-tx_juros IS INITIAL.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH | Não existem taxa de juros cadastrada. |.
          EXIT.
        ENDIF.


        wa_saida_calculadora-vlr_dolar_final = vl_ukurs.

        " AQUI CALCULA O JUROS
*          IF  WA_SAIDA_CALCULADORA-DATA_PAGAMENTO > V_DT_PROXIMO_DIA_UTIL .
        IF wa_saida_calculadora-data_pagamento > v_dt_proximo_dia_util.

          maior_data = wa_saida_calculadora-data_pagamento.
          menor_data = v_dt_proximo_dia_util.

        ELSE.
          maior_data = v_dt_proximo_dia_util .
          menor_data =  wa_saida_calculadora-data_pagamento.
        ENDIF.

        IF wa_saida-moeda_forte IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov  = wa_saida-moeda_forte.
          wa_saida_calculadora-vlr_total_ov = wa_saida-totvl_ov.
        ELSEIF wa_saida-sald_referencia IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov = wa_saida-sald_referencia.
          wa_saida_calculadora-vlr_total_ov = wa_saida-vlr_tot_ref.
        ENDIF.



        qtde_dias_atraso = wa_saida_calculadora-data_pagamento - v_dt_proximo_dia_util.
        wa_saida_calculadora-fator = ( wa_zsdt0051-tx_juros / 360 ) * qtde_dias_atraso. " Fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).
        vlr_juros   = ( wa_saida_calculadora-vlr_saldo_ov * wa_saida_calculadora-fator ) / 100. "Valor dos juros com base no valor total da OV.
        IF qtde_dias_atraso NE 0.
          vlr_mult    = ( wa_saida_calculadora-vlr_saldo_ov * wa_zsdt0051-tx_multa ) / 100.
        ENDIF.
        vlr_tot_ov  = ( wa_saida_calculadora-vlr_saldo_ov + vlr_juros + vlr_mult ).

        wa_saida_calculadora-tx_juros_prop =  ( vlr_juros / vlr_tot_ov ) * 100.
        wa_saida_calculadora-tx_multa_prop =  ( vlr_mult  / vlr_tot_ov ) * 100.


        wa_saida_calculadora-tx_multa  = wa_zsdt0051-tx_multa.
        wa_saida_calculadora-tx_juros  = wa_zsdt0051-tx_juros.

        wa_saida_calculadora-dia_atraso       = qtde_dias_atraso.
        wa_saida_calculadora-vlr_original_usd =     wa_saida_calculadora-vlr_saldo_ov. "Saldo da OV.
*          WA_SAIDA_CALCULADORA-VLR_JUROS_USD    =   ( WA_SAIDA_CALCULADORA-TX_JUROS_PROP *  WA_SAIDA-MOEDA_FORTE ) / 100.  "Valor do juros com base no valor total da OV.
*          WA_SAIDA_CALCULADORA-VLR_MULTA_USD    =   ( WA_SAIDA_CALCULADORA-TX_MULTA_PROP *  WA_SAIDA-MOEDA_FORTE ) / 100.

        wa_saida_calculadora-vlr_juros_usd    =   vlr_juros.
        wa_saida_calculadora-vlr_multa_usd    =   vlr_mult .
        wa_saida_calculadora-vlr_total_usd    =   ( wa_saida_calculadora-vlr_original_usd + wa_saida_calculadora-vlr_multa_usd + wa_saida_calculadora-vlr_juros_usd ).

        IF vl_ukurs IS NOT INITIAL.
          PERFORM oculta_campo_dolar.
          wa_saida_calculadora-vlr_dolar_final = vl_ukurs.
          wa_saida_calculadora-vlr_original_brl = ( wa_saida_calculadora-vlr_saldo_ov * vl_ukurs ).
          wa_saida_calculadora-vlr_juros_brl    = ( vlr_juros * vl_ukurs ).
          wa_saida_calculadora-vlr_multa_brl    = ( vlr_mult * vl_ukurs ).
          wa_saida_calculadora-vlr_total_brl    = ( wa_saida_calculadora-vlr_original_brl + wa_saida_calculadora-vlr_multa_brl + wa_saida_calculadora-vlr_juros_brl ).

          CLEAR: qtde_dias_atraso, fator, vlr_juros, vlr_mult, vlr_tot_ov, prop_juros, prop_multa.
*        ELSE.
*          MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH | Não existem taxa de cambio cadastrada para esse periodo. |.
        ENDIF.

      ELSE. "Se for moeda em BLR.

**         Calcular os dias em atraso, com base em dias úteis.
        qtde_dias_atraso = wa_saida_calculadora-data_pagamento - v_dt_proximo_dia_util.

        wa_saida_calculadora-vlr_original_usd = 0.
        wa_saida_calculadora-vlr_juros_usd    = 0.
        wa_saida_calculadora-vlr_multa_usd    = 0.
        wa_saida_calculadora-vlr_total_usd    = 0.


***       Selecionar a taxa de juros.
        SELECT SINGLE *  FROM zsdt0051 INTO wa_zsdt0051
        WHERE nro_sol_ov EQ wa_saida-vbeln_s.

        IF wa_zsdt0051-tx_juros IS INITIAL.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH | Não existem taxa de juros cadastrada. |.
          EXIT.
        ENDIF.

        IF wa_saida-moeda_forte IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov  = wa_saida-moeda_inter.
          wa_saida_calculadora-vlr_total_ov = wa_saida-totvl_ov.
        ELSEIF wa_saida-sald_referencia IS NOT INITIAL.
          wa_saida_calculadora-vlr_saldo_ov = wa_saida-sald_referencia.
          wa_saida_calculadora-vlr_total_ov = wa_saida-vlr_tot_ref.
        ENDIF.

        wa_saida_calculadora-fator = ( wa_zsdt0051-tx_juros / 360 ) * qtde_dias_atraso. " Fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).
        vlr_juros         = ( wa_saida_calculadora-vlr_saldo_ov * wa_saida_calculadora-fator ) / 100. "Valor dos juros com base no valor total da OV.
        IF qtde_dias_atraso NE 0.
          vlr_mult          = ( wa_saida_calculadora-vlr_saldo_ov * wa_zsdt0051-tx_multa ) / 100.
        ENDIF.
        vlr_tot_ov        = ( wa_saida_calculadora-vlr_saldo_ov + vlr_juros + vlr_mult ).

        wa_saida_calculadora-tx_juros_prop =  ( vlr_juros / vlr_tot_ov ) * 100.
        wa_saida_calculadora-tx_multa_prop =  ( vlr_mult  / vlr_tot_ov ) * 100.

        wa_saida_calculadora-tx_multa  = wa_zsdt0051-tx_multa.
        wa_saida_calculadora-tx_juros  = wa_zsdt0051-tx_juros.

        wa_saida_calculadora-dia_atraso = qtde_dias_atraso.
        wa_saida_calculadora-vlr_original_brl = wa_saida_calculadora-vlr_saldo_ov.
*        WA_SAIDA_CALCULADORA-VLR_JUROS_BRL    = ( WA_SAIDA-MOEDA_INTER * WA_SAIDA_CALCULADORA-TX_JUROS_PROP ) / 100.  "Valor do juros com base no valor total da OV.
*        WA_SAIDA_CALCULADORA-VLR_MULTA_BRL    = ( WA_SAIDA-MOEDA_INTER * WA_SAIDA_CALCULADORA-TX_MULTA_PROP ) / 100.
        wa_saida_calculadora-vlr_juros_brl    = vlr_juros.
        wa_saida_calculadora-vlr_multa_brl    = vlr_mult .
        wa_saida_calculadora-vlr_total_brl    = ( wa_saida_calculadora-vlr_original_brl + wa_saida_calculadora-vlr_multa_brl + wa_saida_calculadora-vlr_juros_brl ).

        CLEAR: qtde_dias_atraso, fator, vlr_juros, vlr_mult, vlr_tot_ov, prop_juros, prop_multa.
      ENDIF.
    ENDIF.
  ENDIF.
*ENDIF.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  LISTAR_FATURAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_listar_faturas  USING    p_wa_saida TYPE ty_saida.
  FREE: it_saida_listar_faturas, it_fcat_faturas[].

  DATA: w_vbrk TYPE vbrk.
  DATA: awkey_auxi  TYPE awkey,
        refkey_auxi TYPE j_1bnflin-refkey.


  SELECT  *  FROM vbfa INTO TABLE @DATA(it_vbfa)
    WHERE vbelv   EQ @p_wa_saida-vbeln_p
    AND   vbtyp_n EQ 'M'
    AND   vbtyp_v EQ 'C'.

  IF it_vbfa IS NOT INITIAL.
    SELECT *  FROM vbrk INTO TABLE @DATA(it_vbrk)
     FOR ALL ENTRIES IN @it_vbfa
      WHERE vbeln EQ @it_vbfa-vbeln
      AND   fksto NE 'X'.
  ENDIF.


  LOOP AT it_vbfa INTO DATA(wa_vbfa).

    READ TABLE it_vbrk INTO DATA(wa_vbrk) WITH KEY vbeln = wa_vbfa-vbeln.
    IF sy-subrc = 0.
      wa_saida_listar_faturas-doc_fatura = wa_vbrk-vbeln.
      wa_saida_listar_faturas-valor      = ( wa_vbrk-netwr + wa_vbrk-mwsbk ).
    ENDIF.

    SELECT SINGLE * FROM j_1bnflin INTO @DATA(wa_lin)
      WHERE refkey EQ @wa_vbfa-vbeln.
    IF wa_lin IS NOT INITIAL.
      SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc)
        WHERE docnum EQ @wa_lin-docnum.
    ENDIF.
    wa_saida_listar_faturas-nfe        =  wa_doc-nfenum.

    SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf)
         WHERE awkey EQ @wa_vbfa-vbeln.
    wa_saida_listar_faturas-doc_contabil =  wa_bkpf-belnr.

*      ***   Pegando a data de vencimento.
* ---> S4 Migration - 19/06/2023 - JS
*    SELECT SINGLE * FROM bseg INTO @DATA(w_bseg) WHERE belnr EQ @wa_bkpf-belnr
*    AND bukrs EQ @wa_bkpf-bukrs AND gjahr EQ @wa_bkpf-gjahr AND bschl = '01'.

    DATA: lt_bseg TYPE fagl_t_bseg.

    CALL FUNCTION 'FAGL_GET_BSEG'
      EXPORTING
        i_bukrs = wa_bkpf-bukrs
        i_belnr = wa_bkpf-belnr
        i_gjahr = wa_bkpf-gjahr
      IMPORTING
        et_bseg = lt_bseg
      EXCEPTIONS
        OTHERS  = 2.

    DELETE lt_bseg WHERE bschl NE '01'.

    READ TABLE lt_bseg INTO DATA(wa_bseg) INDEX 1.

    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING wa_bseg TO w_bseg.
    ENDIF.
* <--- S4 Migration - 19/06/2023 - JS
    IF sy-subrc EQ 0.
      wa_saida_listar_faturas-data_vencimento = |{ w_bseg-fdtag+6(2) }.{ w_bseg-fdtag+4(2) }.{ w_bseg-fdtag(4) }| . "20200202
    ENDIF.

    APPEND wa_saida_listar_faturas TO it_saida_listar_faturas.
    CLEAR:wa_saida_listar_faturas.

  ENDLOOP.
  SORT it_saida_listar_faturas ASCENDING BY doc_fatura.

  CALL SCREEN 1004 STARTING AT 5 5 ENDING AT 100 20.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM    = V_REPORT
*      IT_FIELDCAT           = IT_FCAT_FATURAS
*      I_SAVE                = 'X'
*      I_SCREEN_START_COLUMN = 1
*      I_SCREEN_START_LINE   = 1
*      I_SCREEN_END_COLUMN   = 80
*      I_SCREEN_END_LINE     = 20
*    TABLES
*      T_OUTTAB              = IT_SAIDA_LISTAR_FATURAS.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR 'TITULO_0100'.

  PERFORM f_init_controls.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_CONTROLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_controls .

* create Hierarchy-header
  DATA ls_hierarchy_header TYPE treev_hhdr.

* Create docking container
  CREATE OBJECT go_docking
    EXPORTING
      parent = cl_gui_container=>screen0
      ratio  = 90
    EXCEPTIONS
      OTHERS = 6.

* create tree control
  CREATE OBJECT go_tree
    EXPORTING
      parent                      = go_docking
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'  " required for double-click event on item
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

  PERFORM f_build_hierarchy_header CHANGING ls_hierarchy_header.

  PERFORM f_build_fieldcatalog.

* MOVE-CORRESPONDING IT_ZFIT0026[] TO T_ZFIT0026.

* create emty tree-control
  CALL METHOD go_tree->set_table_for_first_display
    EXPORTING
      i_structure_name    = 'ZFIT0026'
      i_default           = 'X'
      is_hierarchy_header = ls_hierarchy_header
    CHANGING
      it_outtab           = t_zfit0026[]
      it_fieldcatalog     = t_fcat.

* create hierarchy
  PERFORM f_create_hierarchy.

  IF t_zfit0026[] IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-001 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.

* register events
  PERFORM f_register_events.

* adjust column_width
  CALL METHOD go_tree->column_optimize.


ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM f_build_hierarchy_header  CHANGING p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = 'Tipo Documento'.            "#EC NOTEXT
  p_hierarchy_header-tooltip = 'Tipo Documento'.            "#EC NOTEXT
  p_hierarchy_header-width = 30.
  p_hierarchy_header-width_pix = ''.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_fieldcatalog .

  REFRESH: t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZFIT0026'
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_create_hierarchy .

  DATA: ld_fatura_key TYPE lvc_nkey,
        ld_item_key   TYPE lvc_nkey,
        ld_last_key   TYPE lvc_nkey.
  DATA: it_node_key TYPE lvc_t_nkey,
        t_aux_25    TYPE TABLE OF ty_s_zfit0026.

  BREAK rfilipsick.

*  SELECT *
*    FROM ZFIT0026
*    INTO TABLE IT_ZFIT0026
*   WHERE USUARIO = SY-UNAME.

*  IF SY-SUBRC NE 0.
* Usuário não cadastrado - transação ZPM0068 !
*    MESSAGE S000(Z_LES) WITH TEXT-011 DISPLAY LIKE 'S'.
*    LEAVE TO SCREEN 0.
*  ENDIF.

*  BREAK-POINT.
  SELECT vbeln data_venc docnum moeda mont_moeda mont_mi taxa
    forma_pag observacao obj_key doc_fatura vlr_multa_calc vlr_juros_calc vlr_multa_rbdo vlr_juros_rbdo
    FROM zfit0026
    INTO  CORRESPONDING FIELDS OF TABLE  t_zfit0026
    FOR ALL ENTRIES IN it_vbak
    WHERE vbeln	=	it_vbak-vbeln.

*  SELECT *
*    FROM ZPMT0025
*    INTO CORRESPONDING FIELDS OF TABLE T_ZPMT0031
*     FOR ALL ENTRIES IN  T_ZPMT0034
*   WHERE COD_MATERIAL EQ T_ZPMT0034-COD_MATERIAL
*     AND CENTRO EQ  T_ZPMT0034-WERKS
*     AND COD_STATUS = '1'.

  SORT t_zfit0026[] BY vbeln.

***
  LOOP AT t_zfit0026 ASSIGNING FIELD-SYMBOL(<fs_fatura>).
    COLLECT <fs_fatura> INTO t_aux_25.
  ENDLOOP.

  CLEAR: t_zfit0026.
  MOVE-CORRESPONDING t_aux_25[] TO t_zfit0026[].
***

  DELETE ADJACENT DUPLICATES FROM t_zfit0026 COMPARING vbeln.
  MOVE-CORRESPONDING t_zfit0026[] TO t_zfit0026_2[].

  LOOP AT t_zfit0026_2 ASSIGNING FIELD-SYMBOL(<fs_zfit0026>).
    PERFORM f_add_customer_line USING  <fs_zfit0026> "-DATA
                                         ''
                              CHANGING ld_fatura_key.

    IF ld_fatura_key  IS NOT INITIAL.
      APPEND ld_fatura_key TO it_node_key.
    ENDIF.

    ON CHANGE OF <fs_zfit0026>-fatura.
      PERFORM f_add_item_line USING <fs_zfit0026> "-DATA
                                    <fs_zfit0026>-vbeln
                                    ld_fatura_key
                           CHANGING ld_item_key.
    ENDON.
    DELETE t_zfit0026_2 WHERE vbeln = <fs_zfit0026>-vbeln.
  ENDLOOP.

*" calculate totals
  CALL METHOD go_tree->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD go_tree->frontend_update.

  DELETE ADJACENT DUPLICATES FROM it_node_key COMPARING ALL FIELDS.
  CALL METHOD go_tree->expand_nodes( it_node_key = it_node_key ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_register_events.
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD go_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

* set Handler
  SET HANDLER:
    lcl_eventhandler=>handle_node_double_click FOR go_tree,
    lcl_eventhandler=>handle_item_double_click FOR go_tree.

ENDFORM.                               " register_events

*&---------------------------------------------------------------------*
*&      Form  F_ADD_CUSTOMER_LINE
*&---------------------------------------------------------------------*
FORM f_add_customer_line  USING us_data      TYPE ty_s_zfit0026 "-DATA
                                ud_relat_key TYPE lvc_nkey
                       CHANGING cd_node_key  TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  IF v_no_unico = space.
    l_node_text =  'OV'.

    DATA: ls_node TYPE lvc_s_layn.

    CALL METHOD go_tree->add_node
      EXPORTING
        i_relat_node_key = ud_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
*       IS_OUTTAB_LINE   = US_DATA
        is_node_layout   = ls_node
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = cd_node_key.
  ENDIF.

  v_no_unico = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_ITEM_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB_DATA  text
*      -->P_LD_FATURA_KEY  text
*      <--P_LD_ITEM_KEY  text
*----------------------------------------------------------------------*
FORM f_add_item_line  USING  us_data      TYPE ty_s_zfit0026 "-DATA
                             vbeln       TYPE vbeln
                             ud_relat_key TYPE lvc_nkey
                    CHANGING cd_node_key  TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.
  DATA: ls_node TYPE lvc_s_layn.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text       =  vbeln.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = ud_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = us_data
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = cd_node_key.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_MEMORIA_CALCULO_MULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_saida_memoria_calculo_mult  USING    p_wa_saida.
  DATA: total_ov TYPE vbap-netwr.
  DATA: total_ov_parc TYPE vbap-netwr.
  DATA: vlr_total TYPE vbap-netwr.
  CLEAR: wa_edit.

*  SELECT *  FROM VBAP INTO TABLE @DATA(T_VBAP)
*          WHERE VBELN EQ @WA_SAIDA-VBELN.
*
*  CLEAR TOTAL_OV.
*  LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN = WA_SAIDA-VBELN.
*    ADD WA_VBAP-NETWR TO TOTAL_OV.
*    ADD WA_VBAP-MWSBP TO TOTAL_OV.
*  ENDLOOP.

  CLEAR: total_ov, total_ov_parc, ind_rec_total, vlr_total, ind_rec_parc.

*  ****  Selecionar informações zfit0026.
  SELECT SINGLE *
  FROM zfit0026
    INTO @DATA(w_zfit0026)
     WHERE zid_lanc EQ @wa_saida-zid_lanc.

  CHECK w_zfit0026 IS NOT INITIAL.

*  W_EDIT-DOC_FATURA = W_ZFIT0026-DOC_FATURA.
  w_edit-doc_fatura = wa_saida-referencia_nfe.
  w_edit-vbeln = w_zfit0026-vbeln.

  IF w_zfit0026-doc_fatura IS INITIAL.
****   Buscar o saldo total e parcial da OV.
    zcl_dados_ov=>i_vlr_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).

    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).
  ELSE.
****     Buscar o saldo total e parcial da referencia.
    zcl_dados_ov=>i_vlr_referencia_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
        i_vbelnn      = w_zfit0026-doc_fatura
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).

    CLEAR: total_ov.
    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).
    ind_doc_fatura = abap_true.
  ENDIF.


***   Achar o % fator
  IF rb8 IS NOT INITIAL. "Se mercado interno for selecionado.

    IF wa_saida-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        DATA(d_atraso) = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).

        SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov EQ @wa_saida-vbeln_s.

        wa_edit-fator      = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso.
        wa_edit-tx_multa   = wa_zsdt0051-tx_multa.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
          DATA(t_ov)         = ( wa_edit-juros + total_ov + wa_edit-multa ). "Total da OV + juros.

        ELSE.
          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
          t_ov               = ( wa_edit-juros + vlr_total + wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / t_ov )  * 100. " Porcentagem proporcional ao valor do juros
          wa_edit-porc_multa = ( wa_edit-multa / t_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          wa_edit-multa_parc = ( wa_edit-porc_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

*        WA_EDIT-FATOR         = WA_ZSDT0051-TX_MULTA.
*        WA_EDIT-TX_MULTA      = WA_ZSDT0051-TX_MULTA.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        w_edit-tx_multa       = wa_edit-tx_multa.
        w_edit-multa          = wa_edit-multa.
        w_edit-mult           = wa_edit-multa.
        wa_edit-juros         = wa_edit-juros.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_multa     = wa_edit-porc_multa.
        w_edit-por            = '100'.
        w_edit-porc           = '100'.
        wa_edit-porc          = '100'.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_multa  = ' '.
          wa_edit-vlr_rbdo   = ' '.
          w_edit-por         = ' '.
          wa_edit-multa_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.

      ENDIF.
    ENDIF.

  ELSEIF rb7 IS NOT INITIAL. "Se insumo interno for selecionado.
    CLEAR: d_atraso.
    IF wa_saida-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        d_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).

        SELECT SINGLE *  FROM zsdt0040 INTO @DATA(wa_zsdt0040)
        WHERE doc_simulacao EQ @wa_saida-vbeln_s.

        wa_edit-fator      = ( wa_zsdt0040-juros_ano / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).
        wa_edit-tx_multa   = wa_zsdt0051-tx_multa.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100. "Valor da multa com base no valor total da OV.
          DATA(to_ov)        = ( wa_edit-juros + total_ov + wa_edit-multa ). "Total da OV + juros.

        ELSE.
          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100. "Valor da multa com base no valor total da OV.
          to_ov              = ( wa_edit-juros + vlr_total + wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / to_ov )  * 100. " Porcentagem proporcional ao valor do juros
          wa_edit-porc_multa = ( wa_edit-multa / to_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          wa_edit-multa_parc = ( wa_edit-porc_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

*        WA_EDIT-FATOR        = WA_ZSDT0051-TX_MULTA.

        wa_edit-tx_jros       = wa_zsdt0040-juros_ano.
        wa_edit-tx_multa      = wa_zsdt0051-tx_multa.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        w_edit-tx_multa       = wa_edit-tx_multa.
        w_edit-multa          = wa_edit-multa.
        w_edit-mult           = wa_edit-multa.
        wa_edit-juros         = wa_edit-juros.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_multa     = wa_edit-porc_multa.
        w_edit-por            = '100'.
        w_edit-porc           = '100'.
        wa_edit-porc          = '100'.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_multa  = ' '.
          wa_edit-vlr_rbdo   = ' '.
          w_edit-por         = ' '.
          wa_edit-multa_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: w_zfit0026, total_ov, to_ov, vlr_total.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1003 OUTPUT.
  SET PF-STATUS 'STATUS_1003'.
  SET TITLEBAR 'TITLEBAR_1003'.


  PERFORM ocutar_campos_multa.
  PERFORM ocutar_campos_fatura.
  PERFORM ocutar_campo_rec_parc_mult.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1003 INPUT.

  CASE sy-ucomm.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  OCULTA_CAMPO_DOLAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM oculta_campo_dolar .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'WA_SAIDA_CALCULADORA-VLR_DOLAR_FINAL'.
        IF vl_ukurs IS INITIAL.
          screen-invisible = 1.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'TXT_DOLAR'.
        IF vl_ukurs IS INITIAL.
          screen-invisible = 1.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_JUROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_juros .


  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXT_JUROS'                   OR 'TXTTAXA_JUROS'
        OR 'JUROS'                       OR 'TXT_MULT'
        OR 'TXT_TOT_OV'                  OR '100'
        OR 'TXT_PORC_JUROS'              OR 'TXTVLR_JUROS_PARC'
        OR 'TXT_JROS'                    OR 'TXT_SALD_OV'
        OR '%#AUTOTEXT003'               OR '%#AUTOTEXT007'
        OR 'TXTCAL_VALR_TAXA_JUROS_PARC' OR '%#AUTOTEXT006'
        OR 'TXT_SUB'                     OR 'TXT_MAIS'
        OR 'TXT_MAISS'                   OR 'TXT_SUB'
        OR 'TXT_SUBB'                    OR '%#AUTOTEXT001'
        OR '%#AUTOTEXT002'               OR 'TXT_P'
        OR 'TXT_PO'                      OR 'WA_EDIT-JROS'
        OR 'W_EDIT-JUROS'                OR '%#AUTOTEXT008'
        OR 'WA_EDIT-PORC_JUROS'          OR 'W_EDIT-PORC_JUROS'
        OR 'WA_EDIT-MULTA'               OR 'WA_EDIT-VLR_RBDO'
        OR 'W_EDIT-VLR_TOTAL_OV'         OR 'W_EDIT-POR'
        OR 'W_EDIT-PORC'                 OR 'WA_EDIT-JUROS_PARC'.

        IF NOT ind_rec_total IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_MULTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_multa .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXTTAXA_JUROS'                 OR '%#AUTOTEXT001'
        OR 'TXT_MULT'                      OR '%#AUTOTEXT006'
        OR 'TXT_MUL'                       OR 'TXT_JUROS'
        OR 'TOTAL_OV'                      OR '%#AUTOTEXT005'
        OR 'TXTCAL_TAXA_JUROS'             OR '%#AUTOTEXT002'
        OR 'TXTVLR_JUROS_PARC'             OR 'TXT_PORC_MULT'
        OR 'TXTCAL_VALR_TAXA_JUROS_PARC'   OR '%#AUTOTEXT008'
        OR '%#AUTOTEXT007'                 OR 'TXT_PORC_MUL'
        OR 'W_EDIT-PORC_MULTA'             OR 'WA_EDIT-VLR_RBDO'
        OR 'W_EDIT-POR'                    OR 'WA_EDIT-MULTA_PARC'
        OR 'TXT_MAIS'                      OR 'W_EDIT-MULT'
        OR 'W_EDIT-MULTA'                  OR'W_EDIT-VLR_TOTAL_OV'
        OR 'WA_EDIT-JUROS'                 OR'WA_EDIT-PORC_MULTA'
        OR 'W_EDIT-PORC'                   OR'TXT_P'
        OR 'TXT_PP'                        OR 'TXT_MAISS'
        OR 'TXT_SUB'                       OR 'TXT_SUBB'.

        IF NOT ind_rec_total IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_fatura .
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'W_EDIT-DOC_FATURA' OR 'TXT_FATURA'.
        IF ind_doc_fatura IS NOT INITIAL.
          screen-invisible = 0. "Campo Fechado
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_f4_variant .

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


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_variant .

  IF NOT p_varia IS INITIAL.

    MOVE variant TO def_variant.
    MOVE p_varia TO def_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = def_variant.

    variant = def_variant.

  ELSE.
    CLEAR variant.
    variant-report = sy-repid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPO_REC_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campo_rec_parc .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXTVLR_JUROS' OR
           'TXT_FAT' OR
           'TXT_T_OV' OR
           'TXT_DV' OR
           '%#AUTOTEXT009' OR
           'TXT_100' OR
           '%#AUTOTEXT005' OR
           'TXTCAL_VALR_' OR
           'W_EDIT-FATOR' OR
           'WA_EDIT-VLR_TOTAL_OV' OR
           'WA_EDIT-PORC' OR
           'WA_EDIT-JUROS' OR
           '%#AUTOTEXT001' OR
           'TXTTAXA_JUROS' OR
           '%#AUTOTEXT002' OR
           'TXTINDICADOR' OR
           'TXT_JUROS' OR
           '%#AUTOTEXT007' OR
           'JUROS' OR
           'TXT_MAIS' OR
           'TXT_MULT' OR
           'TXT_MAISS' OR
           'TXT_TOT_OV' OR
           'TXT_SUB' OR
           'TXT_PO' OR
           '%#AUTOTEXT006' OR
           'TXT_PORC_JUROS' OR
           'WA_EDIT-JROS' OR
           'W_EDIT-JUROS' OR
           'WA_EDIT-MULTA' OR
           'W_EDIT-VLR_TOTAL_OV' OR
           'W_EDIT-PORC' OR
           'WA_EDIT-PORC_JUROS'.

        IF NOT ind_rec_parc IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPO_REC_PARC_MULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campo_rec_parc_mult .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXTVLR_JUROS'             OR '%#AUTOTEXT001'      OR
           'TXT_CAL_MULTA'            OR
           'TXT_TOT_OV'               OR '%#AUTOTEXT009'      OR
           'TXT_PORC'                 OR '%#AUTOTEXT004'      OR
           'TXT_MULTA'                OR 'W_EDIT-TX_MULTA'    OR
           'WA_EDIT-VLR_TOTAL_OV'     OR 'WA_EDIT-PORC'       OR
           'WA_EDIT-MULTA'            OR '%#AUTOTEXT001'      OR
           'TXTTAXA_JUROS'            OR 'TXT_MULT'           OR
           'W_EDIT-MULTA'             OR '%#AUTOTEXT006'      OR
           'TXT_MUL'                  OR 'W_EDIT-MULT'        OR
           '%#AUTOTEXT002'            OR 'TXT_MAIS'           OR
           'TXT_JUROS'                OR 'WA_EDIT-JUROS'      OR
           'TXT_MAISS'                OR 'TOTAL_OV'           OR
           'W_EDIT-VLR_TOTAL_OV'      OR 'TXT_P'              OR
           'W_EDIT-PORC'              OR '%#AUTOTEXT005'      OR
           'TXTCAL_TAXA_JUROS'        OR 'WA_EDIT-PORC_MULTA' OR
           'TXT_SUBT'                   OR 'TXT_SUB'.



        IF NOT ind_rec_parc IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1004 OUTPUT.
  SET PF-STATUS 'ST1004'.
  SET TITLEBAR 'SET1004'.

  PERFORM f_alv_fatura.

  IF ( obj_alv_0200 IS INITIAL ).
*    PERFORM F_CRIAR_CAT USING '0200'.


    CREATE OBJECT obj_container_0200
      EXPORTING
        container_name = 'CONTAINER_LISTAR'.


    CREATE OBJECT obj_alv_0200
      EXPORTING
        i_parent = obj_container_0200.

*    GS_LAYOUT-SEL_MODE    = 'A'.

    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.


*    PERFORM F_EXCLUDE_FCODE USING '1004'.

    CALL METHOD obj_alv_0200->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        i_save                        = 'A'
        it_toolbar_excluding          = it_exclude_fcode
*        IMPORTING
*       et_fieldcat                   = rt_fieldcat
      CHANGING
        it_fieldcatalog               = it_fca
        it_outtab                     = it_saida_listar_faturas
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.



    CALL METHOD obj_alv_0200->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD obj_alv_0200->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1004 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.


FORM fm_select_zfit0026.

  SELECT * FROM zfit0026
        APPENDING TABLE git_zfit0026
      WHERE docnum IN r_docnum[].

  CLEAR: r_docnum[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_1005 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1005 OUTPUT.

  SET PF-STATUS 'STATUS_1005'.

  IF go_container IS NOT BOUND.

*   create control container
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CC_JUST'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc = 0.

      CREATE OBJECT go_textedit
        EXPORTING
          max_number_chars           = 255
          parent                     = go_container
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 60
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1005 INPUT.
  CASE sy-ucomm.
    WHEN 'BT_CONF'.

      PERFORM get_text_editor.

    WHEN 'BT_SAIR' OR 'EXIT'.

      CALL METHOD go_textedit->delete_text.

      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'BT_EST_LIB'.

      PERFORM f_exibe_estrategia.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.


FORM get_text_editor .

  DATA: lt_text     TYPE TABLE OF as4text,
        lt_zfit186  TYPE TABLE OF zfit186,
        lt_zsdt0337 TYPE TABLE OF zsdt0337,
        lv_id       TYPE numc12,
        lt_estra    TYPE TABLE OF zsds019,
        ls_cadastro TYPE ty_cad_ordem,
        lv_tp_neg   TYPE zsdt0336-tp_negocio_ate. "INICIO BUG SOLTO 145061 - RU


  DATA lv_total_chars TYPE i.
  DATA lv_line_chars  TYPE i.

  CALL METHOD go_textedit->get_text_as_stream
    EXPORTING
      only_when_modified     = cl_gui_textedit=>true
    IMPORTING
      text                   = lt_text
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      OTHERS                 = 3.


  "INICIO BUG SOLTO 145061 - RU
  IF rb8 IS NOT INITIAL.
    lv_tp_neg      = '1'.
  ELSEIF rb7 IS NOT INITIAL.
    lv_tp_neg      = '2'.
  ENDIF.
  "FIM BUG SOLTO 145061 - RU

  PERFORM f_verifica_estrategia . "--------------#146630-CS2024000604 Isenção de Juros Insumos---SMC

  "BUG SOLTO 145061 - SMC
  IF ( tg_estrat[] IS INITIAL ).
    MESSAGE 'Não foi encontrado estratégia para solicitação!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  "BUG SOLTO 145061 - SMC

  CLEAR: lv_total_chars.

  " Loop na tabela para contar caracteres
  LOOP AT lt_text INTO DATA(wa_text).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN wa_text WITH ''.  "
    lv_line_chars = strlen( wa_text ).
    lv_total_chars = lv_total_chars + lv_line_chars.
  ENDLOOP.

  " Verificar se total é maior que 255
  IF lv_total_chars > 255.
    MESSAGE 'Texto excede 255 caracteres' TYPE 'S' DISPLAY LIKE 'E'.
    DATA(v_erro) = 'X'.

  ELSE.
    CLEAR v_erro.
  ENDIF.

  IF v_erro IS NOT INITIAL.
    EXIT.
  ENDIF.

  APPEND INITIAL LINE TO lt_zfit186 ASSIGNING FIELD-SYMBOL(<fs_zfit186>).


*  "146630 - RGA - início
  DATA lv_cod_sol_jur TYPE zfit186-cd_sol_isen.

  CONSTANTS c_obj TYPE nrobj VALUE 'ZCODISEN'.

  CLEAR lv_cod_sol_jur.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = c_obj
    IMPORTING
      number                  = lv_cod_sol_jur
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF lv_cod_sol_jur IS NOT INITIAL.
    <fs_zfit186>-cd_sol_isen = lv_cod_sol_jur.
  ENDIF.
  "146630 - RGA - fim

  <fs_zfit186>-ov_principal    = wa_cabec-ov.
  <fs_zfit186>-data_venc       = wa_cabec-data_venc.
  <fs_zfit186>-moeda           = wa_cabec-moeda.
  <fs_zfit186>-vl_moeda_doc    = wa_cabec-saldo.            "146630 RGA
  <fs_zfit186>-data            = sy-datum.
  <fs_zfit186>-hora            = sy-uzeit.
  <fs_zfit186>-usuario_solicit = sy-uname.
  <fs_zfit186>-status_solicit  = '1'.                       "146630 RGA
  IF rb8 IS NOT INITIAL.
    <fs_zfit186>-tipo_negocio      = '1'.
  ELSEIF rb7 IS NOT INITIAL.
    <fs_zfit186>-tipo_negocio      = '2'.
  ENDIF.

  LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
    REPLACE ALL OCCURRENCES OF '#' IN <fs_text> WITH space.
    <fs_zfit186>-justificativa = <fs_zfit186>-justificativa && space && <fs_text>.
  ENDLOOP.

  MODIFY zfit186 FROM TABLE lt_zfit186.
  IF sy-subrc IS INITIAL.
    COMMIT WORK.
  ENDIF.

  SELECT SINGLE *
    FROM vbak
    INTO @DATA(ls_vbak)
    WHERE vbeln = @wa_cabec-ov.

  SELECT *
     FROM zsdt0336
     INTO TABLE @DATA(lt_0336)
    WHERE  bukrs      <= @ls_vbak-vkorg
       AND bukrs_ate  >= @ls_vbak-vkorg
       AND vkbur      <= @ls_vbak-vkbur
       AND vkbur_ate  >= @ls_vbak-vkbur
"INICIO BUG SOLTO 145061 - RU
       AND tp_negocio_de <= @lv_tp_neg
       AND tp_negocio_ate >= @lv_tp_neg
*       AND waers      EQ @ls_vbak-waerk "146630 - RGA
"FIM BUG SOLTO 145061 - RU
       AND valor_de   <= @wa_cabec-sald_jbrl "146630 - RGA
       AND valor_ate  >= @wa_cabec-sald_jbrl "146630 - RGA
       AND dt_val_de  <= @sy-datum
       AND dt_val_ate >= @sy-datum.

  IF sy-subrc IS INITIAL.

    DELETE lt_0336 WHERE dt_val_de  = sy-datum AND hr_val_de > sy-uzeit.
    DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.

    SORT lt_0336 BY nivel.
    READ TABLE lt_0336 TRANSPORTING NO FIELDS
    WITH KEY nivel = '1'
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      SORT lt_0336 BY nivel DESCENDING.

      READ TABLE lt_0336 ASSIGNING FIELD-SYMBOL(<fs_0336>) INDEX 1.

      SELECT *
         FROM zsdt0336
         APPENDING TABLE lt_0336
        WHERE  bukrs     <= ls_vbak-vkorg
           AND bukrs_ate >= ls_vbak-vkorg
           AND vkbur     <= ls_vbak-vkbur
           AND vkbur_ate >= ls_vbak-vkbur
"INICIO BUG SOLTO 145061 - RU
           AND tp_negocio_de <= lv_tp_neg
           AND tp_negocio_ate >= lv_tp_neg
*           AND waers     = ls_vbak-waerk "146630 - RGA
"FIM BUG SOLTO 145061 - RU
           AND nivel < <fs_0336>-nivel
           AND dt_val_de  <= sy-datum
           AND dt_val_ate >= sy-datum.
      IF sy-subrc IS INITIAL.
        DELETE lt_0336 WHERE dt_val_de  = sy-datum AND hr_val_de > sy-uzeit.
        DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.
      ENDIF.

    ENDIF.

    SORT lt_0336 BY nivel.

    LOOP AT lt_0336 ASSIGNING <fs_0336>.
      APPEND INITIAL LINE TO lt_zsdt0337 ASSIGNING FIELD-SYMBOL(<fs_zsdt0337>).
      <fs_zsdt0337>-bukrs      = ls_vbak-vkorg.
      <fs_zsdt0337>-vbeln      = wa_cabec-ov.
      <fs_zsdt0337>-status_apr = '3'. "146630 - RGA
      <fs_zsdt0337>-valor_de   = <fs_0336>-valor_de.
      <fs_zsdt0337>-valor_ate  = <fs_0336>-valor_ate.
      <fs_zsdt0337>-nivel      = <fs_0336>-nivel.
      <fs_zsdt0337>-data_atual = sy-datum.
      <fs_zsdt0337>-hora_atual = sy-uzeit.
      <fs_zsdt0337>-usuario    = sy-uname.
      <fs_zsdt0337>-aprovador  = <fs_0336>-aprovador.
      <fs_zsdt0337>-valor_moeda_doc  = wa_cabec-saldo.      "146630 RGA

      "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - ini
*** Rubenilson - 18.04.2024 - 131863 - Envio email

*      APPEND INITIAL LINE TO lt_estra ASSIGNING FIELD-SYMBOL(<fs_estra>).
*      <fs_estra>-nivel     = <fs_0336>-nivel.
*      <fs_estra>-aprovador = <fs_0336>-aprovador.

*** Rubenilson - 18.04.2024 - 131863 - Envio email
      "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - fim

    ENDLOOP.

    CLEAR lv_id.

  ENDIF.
  "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - ini
*  IF lt_zsdt0337 IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM lt_zsdt0337 COMPARING ALL FIELDS.
*    MODIFY zsdt0337 FROM TABLE lt_zsdt0337.
*    IF sy-subrc IS INITIAL.
*      COMMIT WORK.
*    ENDIF.
*  ENDIF.
  "146630-CS2024000604 Isenção de Juros Insumos - Parte 1 - RGA - fim

  CALL METHOD go_textedit->delete_text.

  MESSAGE 'Isenção liberada com sucesso' TYPE 'S'.

*** Rubenilson - 18.04.2024 - 131863 - Envio email

  ls_cadastro-vbeln      = wa_cabec-ov.
  ls_cadastro-valor      = wa_cabec-saldo.
  ls_cadastro-org_vendas = ls_vbak-vkorg.
  ls_cadastro-solicitante = sy-uname.
  ls_cadastro-filial      = ls_vbak-vkbur.

  DELETE ADJACENT DUPLICATES FROM lt_estra COMPARING ALL FIELDS.

  SORT lt_estra BY nivel.

  PERFORM envia_email TABLES lt_estra
                       USING ls_cadastro
                             '1'.

*** Rubenilson - 18.04.2024 - 131863 - Envio email

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  DATA: x_contador   TYPE string,
        wa_estrutura TYPE slis_fieldcat_alv.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-edit          = p_edit.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-reptext_ddic  = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.

  IF p_field EQ 'SALDO'.
    wa_estrutura-do_sum = abap_true.
  ENDIF.

  IF ( p_field EQ 'NIVEL' ) OR ( p_field EQ 'APROVADOR' ).
    wa_estrutura-no_zero = abap_true.
    wa_estrutura-just    = 'C'.
  ENDIF.


  APPEND wa_estrutura TO gt_fieldcat.

ENDFORM.                    " montar_estrutura

*&---------------------------------------------------------------------*
*&      Form  SHOW_ESTRAT
*&---------------------------------------------------------------------*
*       Exibe a estratégia de aprovação para a solicitação
*----------------------------------------------------------------------*
FORM show_estrat .

  IF ( tg_estrat[] IS INITIAL ).
    MESSAGE 'Não foi encontrado estratégia para solicitação!' TYPE 'I'.
    EXIT.
  ENDIF.

  DATA: wl_layout TYPE  slis_layout_alv.

  wl_layout-zebra = abap_true.
  wl_layout-colwidth_optimize = abap_true.
  wl_layout-window_titlebar = 'Estratégia de liberação'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
      is_layout             = wl_layout
      it_fieldcat           = gt_fieldcat[]
      i_default             = ' '
      i_save                = ' '
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_estrat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_estrategia
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_estrategia .

  DATA: lv_tp_neg TYPE zsdt0336-tp_negocio_de. "INICIO BUG SOLTO 145061 - RU

  REFRESH: estrutura.

  FREE: gt_fieldcat.

  PERFORM montar_estrutura_alv USING:
     1  'ZSDT0161'    'NIVEL'       'TG_ESTRAT' 'NIVEL'      ''           ' ' ' '  ' ' ' ',
     2  'ZSDT0161'    'APROVADOR'   'TG_ESTRAT' 'APROVADOR'  ''           ' ' ' '  ' ' ' ',
     3  'ZSDT0161'    'VALOR_DE'    'TG_ESTRAT' 'VALOR_DE'   'Valor De'   ' ' ' '  ' ' ' ',
     4  'ZSDT0161'    'VALOR_ATE'   'TG_ESTRAT' 'VALOR_ATE'  'Valor Até'  ' ' ' '  ' ' ' '.

  FREE tg_estrat.

  SELECT SINGLE *
      FROM vbak
      INTO @DATA(ls_vbak)
      WHERE vbeln = @wa_cabec-ov.

  "INICIO BUG SOLTO 145061 - RU
  IF rb8 IS NOT INITIAL.
    lv_tp_neg      = '1'.
  ELSEIF rb7 IS NOT INITIAL.
    lv_tp_neg      = '2'.
  ENDIF.
  "FIM BUG SOLTO 145061 - RU

  SELECT *
    FROM zsdt0336
    INTO TABLE @DATA(lt_0336)
   WHERE  bukrs     <= @ls_vbak-vkorg
      AND bukrs_ate >= @ls_vbak-vkorg
      AND vkbur     <= @ls_vbak-vkbur
      AND vkbur_ate >= @ls_vbak-vkbur
"INICIO BUG SOLTO 145061 - RU
      AND tp_negocio_de <= @lv_tp_neg
      AND tp_negocio_ate >= @lv_tp_neg
*      AND waers     = @ls_vbak-waerk "146630 - RGA
"FIM BUG SOLTO 145061 - RU
      AND valor_de <= @wa_cabec-sald_jbrl   "146630 - RGA
      AND valor_ate >= @wa_cabec-sald_jbrl  "146630 - RGA
      AND dt_val_de  <= @sy-datum
      AND dt_val_ate >= @sy-datum.
*      AND hr_val_de  <= @sy-uzeit
*      AND hr_val_ate >= @sy-uzeit.
  IF sy-subrc IS INITIAL.

    DELETE lt_0336 WHERE dt_val_de = sy-datum AND hr_val_de > sy-uzeit.
    DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.

    SORT lt_0336 BY nivel.
    READ TABLE lt_0336 TRANSPORTING NO FIELDS
    WITH KEY nivel = '1'
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      SORT lt_0336 BY nivel DESCENDING.

      READ TABLE lt_0336 ASSIGNING FIELD-SYMBOL(<fs_0336>) INDEX 1.

      SELECT *
         FROM zsdt0336
         APPENDING TABLE lt_0336
        WHERE  bukrs     <= ls_vbak-vkorg
           AND bukrs_ate >= ls_vbak-vkorg
           AND vkbur     <= ls_vbak-vkbur
           AND vkbur_ate >= ls_vbak-vkbur
"INICIO BUG SOLTO 145061 - RU
           AND tp_negocio_de <= lv_tp_neg
           AND tp_negocio_ate >= lv_tp_neg
*           AND waers     EQ ls_vbak-waerk "146630 - RGA
"FIM BUG SOLTO 145061 - RU
           AND nivel     < <fs_0336>-nivel
           AND dt_val_de  <= sy-datum
           AND dt_val_ate >= sy-datum.
      IF sy-subrc IS INITIAL.
        DELETE lt_0336 WHERE dt_val_de = sy-datum AND hr_val_de > sy-uzeit.
        DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.
      ENDIF.

    ENDIF.


    SORT lt_0336 BY nivel.
    LOOP AT lt_0336 ASSIGNING <fs_0336>.
      APPEND INITIAL LINE TO tg_estrat ASSIGNING FIELD-SYMBOL(<fs_estrat>).
      <fs_estrat>-aprovador = <fs_0336>-aprovador.
      <fs_estrat>-nivel     = <fs_0336>-nivel.
      <fs_estrat>-valor_de  = <fs_0336>-valor_de.
      <fs_estrat>-valor_ate = <fs_0336>-valor_ate.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM tg_estrat COMPARING ALL FIELDS.
  ENDIF.

  SORT tg_estrat BY nivel.

  PERFORM show_estrat .
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_1006 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1006 OUTPUT.
  SET PF-STATUS 'STATUS_1006'..
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1006 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

FORM envia_email TABLES tg_estra
                  USING VALUE(wg_cad_ordem) TYPE ty_cad_ordem
                              plinha  .

  FIELD-SYMBOLS: <fs_solix> TYPE solix.

* Objetos para enviar email
  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin1   TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: l_anex      TYPE string.
  DATA: l_leng      TYPE i.
  DATA: l_arq       TYPE string.
  DATA: l_tam       TYPE i.
  DATA: l_tam_ord   TYPE i.
  DATA: l_tam_log   TYPE i.
  DATA: l_email(300) TYPE c.
  DATA: vlinha      TYPE i.
  DATA: vuser       TYPE sy-uname.
  DATA: it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE.
  DATA: content  TYPE string,
        wa_estra TYPE zsds019.
  DATA: bsmtp_addr TYPE adr6-smtp_addr,
        lv_valor   TYPE string.

  CONSTANTS c_cockpit TYPE sy-sysid VALUE 'ZFIR0125'.       "146630 RGA

*  ** Pass the required parameters and create the shortcut
  CLEAR it_shortcut_param.
  REFRESH it_shortcut_param.

  DELETE ADJACENT DUPLICATES FROM tg_estra COMPARING ALL FIELDS.

*  IF plinha > 0.

*    vlinha = plinha.
*
*    READ TABLE tg_estra INTO wa_estra INDEX vlinha .
*
*    SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
*      FROM usr21
*        INNER JOIN adr6
*           ON  usr21~addrnumber = adr6~addrnumber
*          AND usr21~persnumber = adr6~persnumber
*              WHERE usr21~bname = wa_estra-aprovador.

*  ELSE.

*    LOOP AT tg_estra INTO wa_estra.
*      SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
*        FROM usr21
*       INNER JOIN adr6
*             ON  usr21~addrnumber = adr6~addrnumber
*            AND usr21~persnumber = adr6~persnumber
*          WHERE usr21~bname = wa_estra-aprovador.
*      IF sy-subrc IS INITIAL.
*        reclist-receiver = bsmtp_addr.
*        reclist-rec_type = 'U'.                    "Define email externo
*        APPEND reclist.
*      ENDIF.
*    ENDLOOP.

*  ENDIF.

  SELECT *
    FROM zsdt0060
    INTO TABLE @DATA(lt_0060)
    WHERE vkbur = @wg_cad_ordem-filial
    AND   programa = @c_cockpit.                            "146630 RGA


  IF sy-subrc EQ 0.

    LOOP AT lt_0060 INTO DATA(ls_0060).                     "146630 RGA
      SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
        FROM usr21
       INNER JOIN adr6
             ON  usr21~addrnumber = adr6~addrnumber
            AND usr21~persnumber = adr6~persnumber
          WHERE usr21~bname = ls_0060-usnam.
      IF sy-subrc IS INITIAL.
        reclist-receiver = bsmtp_addr.
        reclist-rec_type = 'U'.                    "Define email externo
        APPEND reclist.
      ENDIF.
    ENDLOOP.

  ELSE.

    MESSAGE 'Não existe aprovador definido na ZSDT0065 para o Esc. Venda selecionado. Solicitação realizada com sucesso' TYPE 'I'.
    EXIT.

  ENDIF.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_ESTRA'.

* Assunto do Email
  doc_chng-obj_descr = 'Solicitação de Isenção de Juros OV: ' && wg_cad_ordem-vbeln.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP:'.
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = 'Aprovação de Isenção de Juros OV: ' &&  wg_cad_ordem-vbeln.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT SINGLE butxt
    FROM t001
    INTO @DATA(lv_butxt)
    WHERE bukrs = @wg_cad_ordem-org_vendas.

  CONCATENATE 'Organização de Venda: ' wg_cad_ordem-org_vendas '-' lv_butxt INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT SINGLE name1
    FROM t001w
    INTO @DATA(lv_name1)
    WHERE werks = @wg_cad_ordem-filial.

  CONCATENATE 'Escr. Vendas: ' wg_cad_ordem-filial '-' lv_name1 INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  lv_valor = wg_cad_ordem-valor.
  CONCATENATE  'Valor: ' lv_valor INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT name_first, name_last
    FROM user_addr
    INTO @DATA(ls_nome)
    UP TO 1 ROWS
    WHERE bname = @wg_cad_ordem-solicitante.
  ENDSELECT.
  IF sy-subrc IS INITIAL.

    IF ls_nome-name_first IS INITIAL AND ls_nome-name_last IS INITIAL.
      ls_nome-name_first = wg_cad_ordem-solicitante.
    ENDIF.

  ENDIF.
  CONCATENATE 'Solicitante: ' ls_nome-name_first ls_nome-name_last INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = 'Para aprovar clique no link "COCKPIT" em anexo' . "146630 RGA
  APPEND objtxt.
  CLEAR objtxt.

  DATA: ctotal(20),
        vdata(10).

  WRITE wg_cad_ordem-valor TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.

  SELECT SINGLE waerk
     FROM vbak INTO @DATA(_waerk)
    WHERE vbeln = @wg_cad_ordem-vbeln.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = wa_estra-aprovador
      transaction       = 'ZFI0174' "146630 RGA
    IMPORTING
      content           = content
    TABLES
      shortcut_param    = it_shortcut_param.

  CLEAR : tab_lines, objbin.
  CONCATENATE content wa_objbin-line INTO wa_objbin-line.
  APPEND  wa_objbin TO objbin.

  DESCRIBE TABLE objbin LINES tab_lines.
  objhead = 'COCKPIT.SAP'.                                  "146630 RGA
  APPEND objhead.

** Creation of the entry for the compressed attachment
  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 1.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'EXT'." SAP
  objpack-obj_name   = 'SAPSHORTCUTMAIL'.
  objpack-obj_descr  = 'COCKPIT.SAP'.                       "146630 RGA
  objpack-doc_size   = tab_lines * 255.
  APPEND objpack.

* Alimentar destinatários do email
  IF bsmtp_addr IS INITIAL.
    MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' TYPE 'I'.
    EXIT.
  ENDIF.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  APPEND reclist.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.

  sy-uname = vuser.

ENDFORM.                    " ENVIA_EMAIL
*&---------------------------------------------------------------------*
*& Form f_verifica_estrategia
*&---------------------------------------------------------------------*
*& *--------------#146630-CS2024000604 Isenção de Juros Insumos---SMC
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verifica_estrategia .

  DATA: lv_tp_neg TYPE zsdt0336-tp_negocio_de. "INICIO BUG SOLTO 145061 - RU

  REFRESH: estrutura.

  FREE: gt_fieldcat.

  PERFORM montar_estrutura_alv USING:
     1  'ZSDT0161'    'NIVEL'       'TG_ESTRAT' 'NIVEL'      ''           ' ' ' '  ' ' ' ',
     2  'ZSDT0161'    'APROVADOR'   'TG_ESTRAT' 'APROVADOR'  ''           ' ' ' '  ' ' ' ',
     3  'ZSDT0161'    'VALOR_DE'    'TG_ESTRAT' 'VALOR_DE'   'Valor De'   ' ' ' '  ' ' ' ',
     4  'ZSDT0161'    'VALOR_ATE'   'TG_ESTRAT' 'VALOR_ATE'  'Valor Até'  ' ' ' '  ' ' ' '.

  FREE tg_estrat.

  SELECT SINGLE *
      FROM vbak
      INTO @DATA(ls_vbak)
      WHERE vbeln = @wa_cabec-ov.

  "INICIO BUG SOLTO 145061 - RU
  IF rb8 IS NOT INITIAL.
    lv_tp_neg      = '1'.
  ELSEIF rb7 IS NOT INITIAL.
    lv_tp_neg      = '2'.
  ENDIF.
  "FIM BUG SOLTO 145061 - RU

  SELECT *
    FROM zsdt0336
    INTO TABLE @DATA(lt_0336)
   WHERE  bukrs     <= @ls_vbak-vkorg
      AND bukrs_ate >= @ls_vbak-vkorg
      AND vkbur     <= @ls_vbak-vkbur
      AND vkbur_ate >= @ls_vbak-vkbur
"INICIO BUG SOLTO 145061 - RU
      AND tp_negocio_de <= @lv_tp_neg
      AND tp_negocio_ate >= @lv_tp_neg
*      AND waers     = @ls_vbak-waerk
"FIM BUG SOLTO 145061 - RU
      AND valor_de <= @wa_cabec-saldo
      AND valor_ate >= @wa_cabec-saldo
      AND dt_val_de  <= @sy-datum
      AND dt_val_ate >= @sy-datum.
*      AND hr_val_de  <= @sy-uzeit
*      AND hr_val_ate >= @sy-uzeit.
  IF sy-subrc IS INITIAL.

    DELETE lt_0336 WHERE dt_val_de = sy-datum AND hr_val_de > sy-uzeit.
    DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.

    SORT lt_0336 BY nivel.
    READ TABLE lt_0336 TRANSPORTING NO FIELDS
    WITH KEY nivel = '1'
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      SORT lt_0336 BY nivel DESCENDING.

      READ TABLE lt_0336 ASSIGNING FIELD-SYMBOL(<fs_0336>) INDEX 1.

      SELECT *
         FROM zsdt0336
         APPENDING TABLE lt_0336
        WHERE  bukrs     <= ls_vbak-vkorg
           AND bukrs_ate >= ls_vbak-vkorg
           AND vkbur     <= ls_vbak-vkbur
           AND vkbur_ate >= ls_vbak-vkbur
"INICIO BUG SOLTO 145061 - RU
           AND tp_negocio_de <= lv_tp_neg
           AND tp_negocio_ate >= lv_tp_neg
*           AND waers     EQ ls_vbak-waerk
"FIM BUG SOLTO 145061 - RU
           AND nivel     < <fs_0336>-nivel
           AND dt_val_de  <= sy-datum
           AND dt_val_ate >= sy-datum.
      IF sy-subrc IS INITIAL.
        DELETE lt_0336 WHERE dt_val_de = sy-datum AND hr_val_de > sy-uzeit.
        DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.
      ENDIF.

    ENDIF.


    SORT lt_0336 BY nivel.
    LOOP AT lt_0336 ASSIGNING <fs_0336>.
      APPEND INITIAL LINE TO tg_estrat ASSIGNING FIELD-SYMBOL(<fs_estrat>).
      <fs_estrat>-aprovador = <fs_0336>-aprovador.
      <fs_estrat>-nivel     = <fs_0336>-nivel.
      <fs_estrat>-valor_de  = <fs_0336>-valor_de.
      <fs_estrat>-valor_ate = <fs_0336>-valor_ate.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM tg_estrat COMPARING ALL FIELDS.
  ENDIF.

  SORT tg_estrat BY nivel.


ENDFORM.
**<<<------"152483 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_export_data_to_table
*&---------------------------------------------------------------------*
*& Prepara dados para exportar para a tabela.
*&---------------------------------------------------------------------*
*  UV_TPREL --> Tipo de Relatório (IN = Insumos e MI = Mercado Interno)
*&---------------------------------------------------------------------*
FORM zf_export_data_to_table USING uv_tprel TYPE char2.

  DATA: lit_extract TYPE TABLE OF zsdt0349,
        lit_0349_in TYPE TABLE OF zsdt0349,
        lit_0349_up TYPE TABLE OF zsdt0349.

  IF it_saida IS NOT INITIAL.  "Verifica se os dados para Extraír
    MOVE-CORRESPONDING it_saida TO lit_extract.
* Busca todos os dados da tabela pelo tipo do relatório.
    SELECT * FROM zsdt0349 INTO TABLE @DATA(lit_0349) WHERE tprel EQ @uv_tprel.

    IF sy-subrc IS INITIAL.
      SORT lit_0349 BY tprel vbeln_s vbeln_p vbeln referencia_nfe zid_lanc.
      LOOP AT lit_extract INTO DATA(ls_extract).
        DATA(lv_tabix_ex) = sy-tabix.
        READ TABLE lit_0349 INTO DATA(ls_0349) WITH KEY tprel          = ls_extract-tprel
                                                        vbeln_s        = ls_extract-vbeln_s
                                                        vbeln_p        = ls_extract-vbeln_p
                                                        vbeln          = ls_extract-vbeln
                                                        referencia_nfe = ls_extract-referencia_nfe
                                                        zid_lanc       = ls_extract-zid_lanc
                                               BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          CLEAR ls_0349-mandt.
          DATA(lv_tabix_tb) = sy-tabix.
* Verifica se o registro do processado é igual ao que está na tabela.
          IF ls_extract EQ ls_0349.
            DELETE lit_0349    INDEX lv_tabix_tb.
            DELETE lit_extract INDEX lv_tabix_ex.
            CLEAR: ls_0349.

          ELSE.
            APPEND ls_extract TO lit_0349_up.
            DELETE lit_extract INDEX lv_tabix_ex.
            CLEAR ls_extract.

          ENDIF.

        ELSE.
          APPEND ls_extract TO lit_0349_in.
          DELETE lit_extract INDEX lv_tabix_ex.
          CLEAR ls_extract.

        ENDIF.

      ENDLOOP.

      IF NOT lit_0349[] IS INITIAL.
        SORT lit_0349 BY vbeln_s vbeln_p vbeln.
        DELETE zsdt0349 FROM TABLE lit_0349.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
          DATA(lv_rc_ok) = abap_on.

        ENDIF.

      ENDIF.

      IF NOT lit_0349_up[] IS INITIAL.
        SORT lit_0349_up BY vbeln_s vbeln_p vbeln.
        UPDATE zsdt0349 FROM TABLE lit_0349_up.
        IF sy-subrc IS INITIAL.
          lv_rc_ok = abap_on.

        ENDIF.

      ENDIF.

      IF NOT lit_0349_in[] IS INITIAL.
        SORT lit_0349_in BY vbeln_s vbeln_p vbeln.
        INSERT zsdt0349 FROM TABLE lit_0349_in.
        IF sy-subrc IS INITIAL.
          lv_rc_ok = abap_on.

        ENDIF.

      ENDIF.

      IF NOT lv_rc_ok IS INITIAL.
        COMMIT WORK.
* Dados gravados com sucesso.
        MESSAGE s001(zhcm_ehs).

      ELSE.
        ROLLBACK WORK.
* Dados não foram gravados& &
        MESSAGE i499(5a) DISPLAY LIKE 'E'.

      ENDIF.

    ELSE.
      IF NOT lit_extract[] IS INITIAL.
        INSERT zsdt0349 FROM TABLE lit_extract.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
* Dados gravados com sucesso.
          MESSAGE s001(zhcm_ehs).

        ELSE.
          ROLLBACK WORK.
* Ocorreu erro ao gravar dados
          MESSAGE i073(gd) DISPLAY LIKE 'E'.

        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.
* Não há dados para gravar
    MESSAGE s536(scpr).

  ENDIF.

ENDFORM.
**<<<------"152483 - NMS - FIM------>>>
*&---------------------------------------------------------------------*
*& Form f_visao_insumos_new
*&---------------------------------------------------------------------*
FORM f_visao_insumos_select .
  TYPES:
    BEGIN OF ty_ov_doc,
      vbeln_p       TYPE zsdt0090-vbeln,
      doc_simulacao TYPE zsdt0090-doc_simulacao,
    END OF ty_ov_doc.

  DATA lt_simu TYPE TABLE OF zi_in_agreg_02.
  DATA lv_lines TYPE i.
  DATA lv_index TYPE i VALUE 1.
  DATA lr_sim TYPE RANGE OF zsdt0090-doc_simulacao.
  DATA: lr_vbeln_p TYPE RANGE OF zsdt0090-vbeln,
        lt_ov_doc  TYPE TABLE OF ty_ov_doc.

  IF p_venc IS NOT INITIAL OR p_docsi[] IS NOT INITIAL.

*    IF ( p_docsi[] IS INITIAL AND p_vbeln IS NOT INITIAL ).
    IF ( p_docsi[] IS INITIAL ).

      SELECT 'I' AS sign, 'EQ' AS option,  vbeln_p AS low
        FROM zi_in_filtro_dt_venc
            WHERE bukrs IN @p_bukrs
              AND kunnr IN @p_kunnr
              AND vkbur IN @p_vkbur
              "AND valdt IN @p_venc
              AND ( vbeln_p IN @p_vbeln OR vbeln IN @p_vbeln )
              AND auart IN @p_auart
              AND vtweg IN @p_vtweg
              AND spart IN @p_spart
              AND waerk IN @p_moeda
              AND erdat IN @p_erdat
              AND data_pgto IN @p_pgto
              AND doc_simulacao IS NOT INITIAL
         INTO TABLE @lr_vbeln_p.
*** Inicio - Rubenilson Pereira - 25.08.25 #175147
      LOOP AT lr_vbeln_p ASSIGNING FIELD-SYMBOL(<fs_vbeln_p>).
        APPEND INITIAL LINE TO lt_ov_doc ASSIGNING FIELD-SYMBOL(<fs_ov_doc>).
        MOVE <fs_vbeln_p>-low TO <fs_ov_doc>-vbeln_p.
      ENDLOOP.

      IF lt_ov_doc IS NOT INITIAL.

        SELECT * FROM zi_in_agreg_02
          INTO TABLE  @lt_simu
          FOR ALL ENTRIES IN @lt_ov_doc
            WHERE bukrs IN @p_bukrs
              AND vbeln_p = @lt_ov_doc-vbeln_p.
*              AND erdat IN @p_erdat
*              AND data_pgto IN @p_pgto.
        IF sy-subrc IS INITIAL.
          IF rb11 IS NOT INITIAL.
            DELETE lt_simu WHERE ecommerce = ''.
          ELSEIF rb10 IS NOT INITIAL  .
            DELETE lt_simu WHERE ecommerce = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.
*** Fim - Rubenilson Pereira - 25.08.25 #175147
    ELSE.

*      SELECT 'I' AS sign, 'EQ' AS option,  doc_simulacao AS low
      SELECT 'I' AS sign, 'EQ' AS option,  vbeln_p AS low
        FROM zi_in_filtro_dt_venc
            WHERE bukrs IN @p_bukrs
              AND kunnr IN @p_kunnr
              AND vkbur IN @p_vkbur
              AND valdt IN @p_venc
              AND auart IN @p_auart
              AND vtweg IN @p_vtweg
              AND spart IN @p_spart
              AND waerk IN @p_moeda
              AND erdat IN @p_erdat
              AND data_pgto IN @p_pgto
              AND ( vbeln_p IN @p_vbeln OR vbeln IN @p_vbeln )
              AND doc_simulacao IN @p_docsi[]
         INTO TABLE @lr_sim.
*** Inicio - Rubenilson Pereira - 25.08.25 #175147
*      LOOP AT lr_sim ASSIGNING FIELD-SYMBOL(<fs_sim>).
*        APPEND INITIAL LINE TO lt_ov_doc ASSIGNING <fs_ov_doc>.
*        MOVE <fs_sim>-low TO <fs_ov_doc>-doc_simulacao.
*      ENDLOOP.
      LOOP AT lr_sim ASSIGNING FIELD-SYMBOL(<fs_sim>).
        APPEND INITIAL LINE TO lt_ov_doc ASSIGNING <fs_ov_doc>.
        MOVE <fs_sim>-low TO <fs_ov_doc>-vbeln_p.
      ENDLOOP.
      IF lt_ov_doc IS NOT INITIAL.

        SELECT * FROM zi_in_agreg_02
       INTO TABLE  @lt_simu
       FOR ALL ENTRIES IN @lt_ov_doc
         WHERE bukrs IN @p_bukrs
*           AND doc_simulacao = @lt_ov_doc-doc_simulacao
           AND vbeln_p = @lt_ov_doc-vbeln_p.
*           AND erdat IN @p_erdat
*           AND data_pgto IN @p_pgto.
        IF sy-subrc IS INITIAL.
          IF rb11 IS NOT INITIAL.
            DELETE lt_simu WHERE ecommerce = ''.
          ELSEIF rb10 IS NOT INITIAL  .
            DELETE lt_simu WHERE ecommerce = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

*** Fim - Rubenilson Pereira - 25.08.25 #175147
    ENDIF.

    IF p_docsi[] IS INITIAL.

      " 07.03.2025 - ramon -->

*      SELECT * FROM zi_in_agreg_02
*          WHERE bukrs IN @p_bukrs
*            "AND kunnr IN @p_kunnr """07.03.2025
*            AND vkbur IN @p_vkbur
*            AND ( vbeln_p IN @p_vbeln OR vbeln IN @p_vbeln )
*            AND erdat IN @p_erdat
*            AND doc_simulacao IS INITIAL
*            AND valdt IN @p_venc " 24.02.2025 -
*            APPENDING TABLE @lt_simu.

      SELECT * FROM zi_in_agreg_02
          WHERE bukrs IN @p_bukrs
            AND kunnr IN @p_kunnr """07.03.2025  --- descomentado 20.05.2025 - bug: 179809
            AND vkbur IN @p_vkbur
            AND ( vbeln_p IN @p_vbeln OR vbeln IN @p_vbeln )
            AND erdat IN @p_erdat
            AND doc_simulacao IS INITIAL
            AND valdt IN @p_venc " 24.02.2025 -
            AND auart IN @p_auart
            AND vtweg IN @p_vtweg
*            AND data_pgto IN @p_pgto
            INTO TABLE @DATA(lt_simu_sem).
      IF sy-subrc IS INITIAL.
        IF rb11 IS NOT INITIAL.
          DELETE lt_simu_sem WHERE ecommerce = ''.
        ELSEIF rb10 IS NOT INITIAL  .
          DELETE lt_simu_sem WHERE ecommerce = 'X'.
        ENDIF.
      ENDIF.
      LOOP AT lt_simu_sem ASSIGNING FIELD-SYMBOL(<fs_sem>).

        READ TABLE lt_simu TRANSPORTING NO FIELDS
          WITH KEY vbeln_p = <fs_sem>-vbeln_p
                   vbeln = <fs_sem>-vbeln.

        CHECK sy-subrc NE 0.

        APPEND <fs_sem> TO lt_simu.

      ENDLOOP.
      " 07.03.2025 - ramon --<




    ENDIF.

    " 27.02.2025 - RAMON -->]
    LOOP AT lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu1>) WHERE valdt NOT IN p_venc.

      DATA(lv_count) = REDUCE i( INIT x = 0 FOR wa IN lt_simu WHERE ( vbeln_p = <fs_simu1>-vbeln_p AND valdt IN p_venc ) NEXT x = x + 1 ).

      CHECK lv_count IS INITIAL.

      DELETE lt_simu WHERE vbeln_p = <fs_simu1>-vbeln_p.

    ENDLOOP.

    " 27.02.2025 - RAMON --<

  ELSE.

    IF ( p_docsi[] IS INITIAL AND p_vbeln[] IS NOT INITIAL ) OR p_erdat IS NOT INITIAL
        OR p_pgto IS NOT INITIAL.


      IF p_docsi[] IS INITIAL AND p_vbeln[] IS NOT INITIAL OR p_erdat IS NOT INITIAL OR
         p_pgto IS NOT INITIAL.

        SELECT 'I' AS sign, 'EQ' AS option,  vbeln_p AS low
          FROM zi_in_filtro_dt_venc
              WHERE bukrs IN @p_bukrs
                AND kunnr IN @p_kunnr
                AND vkbur IN @p_vkbur
                AND auart IN @p_auart
                AND vtweg IN @p_vtweg
                AND spart IN @p_spart
                AND waerk IN @p_moeda
                AND erdat IN @p_erdat
                AND data_pgto IN @p_pgto
                AND ( vbeln_p IN @p_vbeln OR vbeln IN @p_vbeln )
                AND doc_simulacao IS NOT INITIAL
           INTO TABLE @lr_vbeln_p.
      ELSE.
        sy-subrc = 0.
      ENDIF.

      IF sy-subrc EQ 0.

        SELECT * FROM zi_in_agreg_02
          INTO TABLE  @lt_simu
          FOR ALL ENTRIES IN @lr_vbeln_p
            WHERE bukrs IN @p_bukrs
              AND kunnr IN @p_kunnr
              AND vkbur IN @p_vkbur
              AND vbeln_p = @lr_vbeln_p-low.
*              AND erdat IN @p_erdat
*              AND data_pgto IN @p_pgto.


        " 28.02.2025 --- filtro ov principal --<
        IF p_vbeln IS NOT INITIAL.

          SELECT * FROM zi_in_agreg_02
              WHERE bukrs IN @p_bukrs
                AND kunnr IN @p_kunnr
                AND vkbur IN @p_vkbur
                AND ( vbeln_p IN @p_vbeln OR vbeln IN @p_vbeln )
*              AND erdat IN @p_erdat
*                AND data_pgto IN @p_pgto
                AND doc_simulacao IS INITIAL
          APPENDING TABLE @lt_simu.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

*  PERFORM f_visao_insumos_filtro CHANGING lt_simu. " Rubenilson Pereira - 25.08.25 #175147

  lv_lines = lines( lt_simu ).

  DO.

    READ TABLE lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>) INDEX lv_index.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    " preenche cab
    PERFORM f_preenche_insumos_cab USING <fs_simu> CHANGING it_saida.

    IF <fs_simu>-num_reg > 0.

      "preenche item
      PERFORM f_preenche_insumos_item
        USING lt_simu
              <fs_simu>
     CHANGING lv_index
              it_saida.

    ELSE.

      ADD 1 TO lv_index.

    ENDIF.

  ENDDO.

  PERFORM f_visao_insumos_filtro CHANGING it_saida." Rubenilson Pereira - 25.08.25 #175147
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_insumos_cab
*&---------------------------------------------------------------------*
FORM f_preenche_insumos_cab USING us_dados TYPE zi_in_agreg_02
                         CHANGING ct_saida TYPE ty_saida_tab.


  APPEND INITIAL LINE TO ct_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

  <fs_saida>-id_order_ecommerce = us_dados-id_ecomm.
  <fs_saida>-bukrs = us_dados-bukrs.
  <fs_saida>-vkgrp = us_dados-vkgrp.
  <fs_saida>-bezei = us_dados-vkgrp_name.
  <fs_saida>-vkbur = us_dados-vkbur.
  <fs_saida>-auart = us_dados-auart.
  <fs_saida>-safra = us_dados-safra.
  <fs_saida>-cultura = us_dados-cultura_desc.
  <fs_saida>-pgto_ant = us_dados-pgto_ant.
  <fs_saida>-tx_juros = us_dados-tx_juros.
  "<fs_saida>-tx_multa = us_dados-tx_multa.
  <fs_saida>-kunnr = us_dados-kunnr.
  <fs_saida>-name1 = us_dados-name1.
  <fs_saida>-zterm = us_dados-zterm.
  <fs_saida>-text1 = us_dados-zterm_name.

  IF us_dados-doc_simulacao IS NOT INITIAL.
    <fs_saida>-vbeln_s = us_dados-doc_simulacao.
  ENDIF.

  <fs_saida>-vbeln_p = us_dados-vbeln_p.
  <fs_saida>-vbeln_g = us_dados-vbeln.
  <fs_saida>-vbeln = us_dados-vbeln.

  <fs_saida>-data_venc = us_dados-valdt.
  <fs_saida>-erdat = us_dados-erdat.
  <fs_saida>-waerk = us_dados-waerk.

  <fs_saida>-totalq_ov = us_dados-qtde.
  <fs_saida>-rfmng = us_dados-qtde_fat.
  <fs_saida>-netwr_l = us_dados-vlr_liq.
  <fs_saida>-mwsbp = us_dados-vlr_imp.
  <fs_saida>-totvl_ov = us_dados-vlr_tot_ov.

  <fs_saida>-moeda_inter = us_dados-saldo_ov_brl.
  <fs_saida>-moeda_forte = us_dados-saldo_ov_usd.
  <fs_saida>-line_color = 'C310'.


  t_cor =
        VALUE #(
                 "Vermelho
                 ( fname = 'MOEDA_FORTE'  color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'MOEDA_INTER'  color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'SALD_REFERENCIA' color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
                 ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
                  ).
  APPEND LINES OF t_cor TO <fs_saida>-color_cell.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_insumos_item
*&---------------------------------------------------------------------*
FORM f_preenche_insumos_item USING t_simu TYPE ty_agreg_02
                                    us_dados TYPE zi_in_agreg_02
                           CHANGING uv_indice TYPE i
                                    ct_saida TYPE ty_saida_tab.

  DATA(lv_index_ret) = uv_indice.

  LOOP AT t_simu ASSIGNING FIELD-SYMBOL(<fs_lcto>) FROM uv_indice WHERE vbeln = us_dados-vbeln.

    APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_doc>).

    " id do lançamento zfit0026
    <fs_saida_doc>-zid_lanc = <fs_lcto>-zid_lanc.

    <fs_saida_doc>-vkgrp = us_dados-vkgrp.
    <fs_saida_doc>-bezei = us_dados-vkgrp_name.
    <fs_saida_doc>-vkbur = us_dados-vkbur.

    <fs_saida_doc>-vbeln_s = us_dados-doc_simulacao.
    <fs_saida_doc>-vbeln_p = us_dados-vbeln_p.
    <fs_saida_doc>-vbeln_g = us_dados-vbeln.
    <fs_saida_doc>-vbeln =  us_dados-vbeln.

    <fs_saida_doc>-auart = us_dados-auart.

    <fs_saida_doc>-kunnr = us_dados-kunnr.
    <fs_saida_doc>-name1 = us_dados-name1.
    <fs_saida_doc>-zterm = us_dados-zterm.
    <fs_saida_doc>-text1 = us_dados-zterm_name.

    <fs_saida_doc>-erdat = us_dados-erdat.
    <fs_saida_doc>-waerk = us_dados-waerk.

    " data Vencimento
    <fs_saida_doc>-data_venc = <fs_lcto>-valdt.

    " data de pagamento
    <fs_saida_doc>-budat = <fs_lcto>-data_pgto.

    " Taxa (Ptax)
    <fs_saida_doc>-taxa = <fs_lcto>-taxa.

    " Forma de pagamento
    <fs_saida_doc>-forma_pag = <fs_lcto>-forma_pag.

    " N. Doc Contabil
    <fs_saida_doc>-docnum = <fs_lcto>-belnr.

    " Doc.Compens
    <fs_saida_doc>-augbl = <fs_lcto>-augbl.

    " Vlr.Comp.US$ / Vlr Recebido US$
    <fs_saida_doc>-dmbe2 = <fs_lcto>-vlr_rec_usd.

    " Vlr.Comp.R$ / Vlr Recebido R$
    <fs_saida_doc>-dmbtr = <fs_lcto>-vlr_rec_brl.

    " Multa Calculado
    <fs_saida_doc>-vlr_multa_calc = <fs_lcto>-multa_calc.
    " Multa Recebido
    <fs_saida_doc>-vlr_multa_rbdo = <fs_lcto>-multa_rec.
    " Multa Calculado
    <fs_saida_doc>-vlr_desc_mult = <fs_lcto>-desc_multa.
    " Juros Calculado
    <fs_saida_doc>-vlr_juros_calc = <fs_lcto>-juros_calc.
    " Juros Recebido
    <fs_saida_doc>-vlr_juros_rbdo = <fs_lcto>-juros_rec.
    " Multa Recebido
    <fs_saida_doc>-vlr_desc_jros = <fs_lcto>-desc_juros.

    " Saldo Juros USD
    <fs_saida_doc>-vlr_sald_fin = <fs_lcto>-saldo_juros_usd.

    "Saldo Juros R$
    <fs_saida_doc>-vlr_sald_fin_brl = <fs_lcto>-saldo_juros_brl.

    "N° Compra/Adiantamento
    <fs_saida_doc>-num_comp_adiant = <fs_lcto>-num_comp_adiant.

    " Observação
    <fs_saida_doc>-observacao = <fs_lcto>-observacao.

    IF <fs_saida_doc>-zid_lanc IS NOT INITIAL.

      t_cor = VALUE #(
                   "Verde
                   "( fname = 'AUGBL'          color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'DOCNUM'         color-col = '5' color-int = '1' color-inv = '1' )
                   "( fname = 'DATA_VENC'      color-col = '5' color-int = '1' color-inv = '1' )
                   "( fname = 'BUDAT'          color-col = '5' color-int = '1' color-inv = '1' )
                   "( fname = 'DMBE2'          color-col = '5' color-int = '1' color-inv = '1' )
                   "( fname = 'DMBTR'          color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'FORMA_PAG'      color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'TAXA'           color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'MONT_MOEDA'     color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'MONT_MI'        color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_MULTA_CALC' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_MULTA_RBDO' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_JUROS_CALC' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_JUROS_RBDO' color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_DESC_MULT'  color-col = '5' color-int = '1' color-inv = '1' )
                   ( fname = 'VLR_DESC_JROS'  color-col = '5' color-int = '1' color-inv = '1' ) ).

      APPEND LINES OF t_cor TO <fs_saida_doc>-color_cell.
      CLEAR t_cor.

    ENDIF.

    IF <fs_saida_doc>-augbl IS NOT INITIAL.
      t_cor = VALUE #( ( fname = 'BUDAT'          color-col = '5' color-int = '1' color-inv = '1' )
                       ( fname = 'AUGBL'          color-col = '5' color-int = '1' color-inv = '1' )
                       ( fname = 'DMBE2'          color-col = '5' color-int = '1' color-inv = '1' )
                       ( fname = 'DMBTR'          color-col = '5' color-int = '1' color-inv = '1' ) ).

      APPEND LINES OF t_cor TO <fs_saida_doc>-color_cell.
      CLEAR t_cor.
    ENDIF.


    t_cor = VALUE #(
           "Vermelho
           ( fname = 'MOEDA_FORTE'  color-col = '6' color-int = '1' color-inv = '1' )
           ( fname = 'MOEDA_INTER'  color-col = '6' color-int = '1' color-inv = '1' )
           ( fname = 'SALD_REFERENCIA' color-col = '6' color-int = '1' color-inv = '1' )
           ( fname = 'VLR_SALD_FIN' color-col = '6' color-int = '1' color-inv = '1' )
           ( fname = 'VLR_SALD_FIN_BRL' color-col = '6' color-int = '1' color-inv = '1' )
            ).

    APPEND LINES OF t_cor TO <fs_saida_doc>-color_cell.
    FREE  t_cor.

    ADD 1 TO lv_index_ret.

  ENDLOOP.

  uv_indice = lv_index_ret.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_visao_insumos_filtro
*&---------------------------------------------------------------------*
FORM f_visao_insumos_filtro CHANGING ct_select TYPE ty_saida_tab.

  CHECK ct_select IS NOT INITIAL.


  IF rb3 IS INITIAL.

    SELECT * FROM zi_in_filtro_saldo
     INTO TABLE @DATA(lt_saldo)
       FOR ALL ENTRIES IN @ct_select
         WHERE vbeln_p = @ct_select-vbeln_p.

  ENDIF.

*** Inicio - Rubenilson Pereira - 25.08.25 #175147
  DATA: lv_saldo_brl       TYPE zi_in_agreg_02-saldo_ov_brl,
        lv_saldo_usd       TYPE zi_in_agreg_02-saldo_ov_usd,
        lv_saldo_juros_brl TYPE zi_in_agreg_02-saldo_juros_brl,
        lv_saldo_juros_usd TYPE zi_in_agreg_02-saldo_juros_usd.

  DATA(lt_simu) = ct_select.
  SORT lt_simu BY vbeln_p.

  LOOP AT ct_select ASSIGNING FIELD-SYMBOL(<fs_select>).

    CLEAR: lv_saldo_brl,
           lv_saldo_usd,
           lv_saldo_juros_brl,
           lv_saldo_juros_usd.

    READ TABLE lt_simu TRANSPORTING NO FIELDS
    WITH KEY vbeln_p = <fs_select>-vbeln_p
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>) FROM sy-tabix.
        IF <fs_simu>-vbeln_p <> <fs_select>-vbeln_p.
          EXIT.
        ENDIF.

        lv_saldo_brl = lv_saldo_brl + <fs_simu>-moeda_inter.
        lv_saldo_usd = lv_saldo_usd + <fs_simu>-moeda_forte.

        lv_saldo_juros_brl = lv_saldo_juros_brl + <fs_simu>-vlr_sald_fin_brl.
        lv_saldo_juros_usd = lv_saldo_juros_usd + <fs_simu>-vlr_sald_fin.
      ENDLOOP.

      CASE abap_true.
        WHEN rb1.
          IF
         ( lv_saldo_brl > 10 ) OR
         ( lv_saldo_usd > 10 ) OR
         ( lv_saldo_juros_brl > 10 ) OR
         ( lv_saldo_juros_usd > 10 ).
            DELETE ct_select WHERE vbeln_p = <fs_select>-vbeln_p.
          ENDIF.
        WHEN rb2.
          IF ( ( ( lv_saldo_brl BETWEEN 0 AND 10 ) OR
                ( lv_saldo_brl < 0 ) ) AND
              ( ( lv_saldo_usd BETWEEN 0 AND 10 ) OR
                ( lv_saldo_usd < 0 ) ) ) AND
              ( ( lv_saldo_juros_brl BETWEEN -10 AND 10 ) OR
                ( lv_saldo_juros_brl < -10 ) ) AND
              ( ( lv_saldo_juros_usd BETWEEN -10 AND 10 ) OR
                ( lv_saldo_juros_usd < -10 ) ).
            DELETE ct_select WHERE vbeln_p = <fs_select>-vbeln_p.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.


    ENDIF.

  ENDLOOP.
*** Fim - Rubenilson Pereira - 25.08.25 #175147

  CASE abap_true.

      " -----------------------------------------------------Recebido
    WHEN rb1.
*** Inicio - Rubenilson Pereira - 25.08.25 #175147
*      LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>) WHERE saldoov > 10 OR saldojurosfiltro > 10.
*
*        DELETE ct_select WHERE vbeln_p = <fs_saldo>-vbeln_p.
*
*      ENDLOOP.
*** Fim - Rubenilson Pereira - 25.08.25 #175147
      " ---------------------------------------------------------Saldo
    WHEN rb2.
*** Inicio - Rubenilson Pereira - 25.08.25 #175147
*      " se é para visualizar saldo, então retira as que tem saldo zerado, ou saldo de juros zerado ou saldo de juros negativo
*      LOOP AT lt_saldo ASSIGNING <fs_saldo>
*          WHERE ( ( saldoov BETWEEN 0 AND 10 ) OR saldoov < 0 )
*            "AND   ( saldojurosfiltro BETWEEN -10 AND 10 ). " OR saldojurosfiltro < -10 ). " retirado para insumos 26.02.2025 --- 08.05.2025
*            AND ( saldojurosfiltro < 10 ).
*
*        DELETE ct_select WHERE vbeln_p = <fs_saldo>-vbeln_p.
*
*      ENDLOOP.
*** Fim - Rubenilson Pereira - 25.08.25 #175147
    WHEN rb3.

  ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*& Report  ZFIR0025
*&
*&---------------------------------------------------------------------*
*&TITULO: INVOICE - CONTAS A PAGAR
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 26.03.2013
*TRANSACAO: ZFI0017
*&---------------------------------------------------------------------*

report  zfir0025.

include <cl_alv_control>.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: icon,
            slis.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: zib_contabil , zfit0036, zfit0037, sscrfields, tvarvc.



*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.
data: wl_mode(1).


types: begin of ty_zib_contabil.
         include structure zib_contabil.
types:   mark   type c,
         data   type sy-datum,
         del(1),
       end of ty_zib_contabil.

types: begin of ty_zib_contabil_err.
         include structure zib_contabil_err.
types:   mark type c,
       end of ty_zib_contabil_err.

types: begin of ty_zfit0037.
         include structure zfit0037.
types:   mark type c,
       end of ty_zfit0037.

types:
  begin of ty_cdhdr,
    objectclas type cdhdr-objectclas,
    objectid   type cdhdr-objectid,
    changenr   type cdhdr-changenr,
    tcode      type cdhdr-tcode,
    username   type cdhdr-username,
    udate      type cdhdr-udate,
  end of ty_cdhdr,

  begin of ty_cdpos,
    objectclas type cdpos-objectclas,
    objectid   type cdpos-objectid,
    changenr   type cdpos-changenr,
    fname      type cdpos-fname,
    value_new  type cdpos-value_new,
  end of ty_cdpos,

  begin of ty_cadger,
    dt_lct        type zfit0036-dt_pgto,
    waers         type zimp_cad_imposto-waers,
    question(1)                                 ,
    tp_operacao   type zfit0043-tp_operacao,
    ds_operacao   type zfit0043-ds_operacao,
    observacao    type zfit0036-observacao,
    motivo        type zfit0036-motivo,
    descricao     type zfit0040-descricao,
    documento(10),
    icon(4),
    obj_key       type zfit0036-obj_key,
    referencia    type zfit0036-referencia,
    liquidar      type zfit0043-liquidar,
    navio         type zfit0036-navio,                      "bug 53981
  end of ty_cadger,

  begin of ty_cadest,
    augbl       type rf05r-augbl,
    question(1)                       ,
    dt_pgto     type zfit0036-dt_pgto,
    invoice     type zfit0036-invoice,
    flag(1),
  end of ty_cadest,

  begin of ty_cadcom,
    dt_pgto    type zfit0036-dt_pgto,
    observacao type zfit0036-observacao,
    kunnr      type kna1-kunnr,
  end of ty_cadcom,

  begin of ty_cadadt,
    dt_pgto    type zfit0036-dt_pgto,
    observacao type zfit0036-observacao,
    lifnr      type lfa1-lifnr,
    name1      type lfa1-name1,
  end of ty_cadadt,


  begin of ty_cadinvo,
    bukrs      type zfit0036-bukrs,                         "BUG 53981
    lifnr      type zib_contabil-hkont,       "Fornecedor
    name1      type lfa1-name1,
    lnrza      type lfa1-lnrza,
    waers      type zimp_cad_imposto-waers,
    dt_pgto    type zfit0036-dt_pgto,
    tx_cambio  type zfit0036-tx_cambio,
    observacao type zfit0036-observacao,
    referencia type zfit0036-referencia,
    vlr_pgto   type zfit0036-vlr_pgto,
  end of ty_cadinvo,

  begin of ty_cadbrasil,
    augdt      type bsid_view-augdt,
    augbl      type bsid_view-augbl,
    nro_sol_ov type zsdt0053-nro_sol_ov,
    observacao type zfit0036-observacao,
    kunnr      type kna1-kunnr,
    name1      type kna1-name1,
  end of ty_cadbrasil,

  begin of ty_cadliq,
    hbkid         type t012-hbkid,
    laufi         type zfit0037-laufi,
    motivo_transf type zfit0037-motivo_transf,
    nome_arq(20),
    path(250),
    local(1),
    servidor(1),
    checkarq(1),
    checkpa(1),
    moeda         type zfit0036-moeda_pgto,
  end of ty_cadliq,

  begin of ty_zib_contabil_chv,
    obj_key type zib_contabil_chv-obj_key,
    belnr   type zib_contabil_chv-belnr,
    bukrs   type zib_contabil_chv-bukrs,
    hkont   type zib_contabil-hkont,
    gjahr   type zib_contabil_chv-gjahr,
  end of ty_zib_contabil_chv,

  begin of ty_zfit0036,
    obj_key         type zfit0036-obj_key,
    belnr36         type zfit0036-belnr,
    buzei36         type zfit0036-buzei,
    bukrs           type zfit0036-bukrs,
    invoice         type zfit0036-invoice,
    navio           type zfit0036-navio,
    lote            type zfit0036-lote,
    lote_cp         type zfit0036-lote_cp,
    dt_pgto         type zfit0036-dt_pgto,
    tx_cambio       type zfit0036-tx_cambio,
    moeda_pgto      type zfit0036-moeda_pgto,
    vlr_pgto        type zfit0036-vlr_pgto,
    hbkid           type zfit0036-hbkid,
    observacao      type zfit0036-observacao,
    status          type zfit0036-status,
    forma_pg        type zfit0036-forma_pg,
    motivo          type zfit0036-motivo,
    rg_atualizado   type zfit0036-rg_atualizado,
    bvtyp           type zfit0036-bvtyp,
    operacao        type zfit0036-operacao,
    id_tipo_invoice type zfit0036-id_tipo_invoice,
    banka_1         type zfit0036-banka_1,
    banks_1         type zfit0036-banks_1, "US - 76596 - CSB
    invoice_terc    type zfit0036-invoice_terc,
    matnr           type zfit0036-matnr,
    lifnr           type lfa1-lifnr,
    referencia      type zfit0036-referencia,
    belnr_adt_c     type zfit0036-belnr_adt_c,
    belnr_adt_g     type zfit0036-belnr_adt_g,
    usuario         type zfit0036-usuario,
    user_create     type zfit0036-user_create,
    data_atual      type zfit0036-data_atual,
    belnr           type bsik_view-belnr,
    buzei           type zfit0036-buzei,
    nro_sol_ov      type zsdt0053-nro_sol_ov,
* Ini - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual
*    bvtyp           TYPE zfit0036-bvtyp,
*    banks_1         TYPE zfit0036-banks_1,
    bankl_1         type zfit0036-bankl_1,
    bankn_1         type zfit0036-bankn_1,
    iban_1          type zfit0036-iban_1,
    swift_1         type zfit0036-swift_1,
*    banka_1         TYPE zfit0036-banka_1,
    bvtyp_2         type zfit0036-bvtyp_2,
    banks_2         type zfit0036-banks_2,
    bankl_2         type zfit0036-bankl_2,
    bankn_2         type zfit0036-bankn_2,
    iban_2          type zfit0036-iban_2,
    swift_2         type zfit0036-swift_2,
    banka_2         type zfit0036-banka_2,
    desp_tar        type zfit0036-desp_tar,
* Fim - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual
  end of ty_zfit0036,

  begin of ty_makt,
    matnr type makt-matnr,
    maktx type makt-maktx,
  end of ty_makt,

  begin of ty_zfit0047,
    id_tipo_invoice type zfit0047-id_tipo_invoice,
    descricao       type zfit0047-descricao,
    pgto            type zfit0047-pgto,
  end of ty_zfit0047,

  begin of ty_bsak,
    bukrs type bsak_view-bukrs,
    lifnr type bsak_view-lifnr,
    belnr type bsak_view-belnr,
    augdt type bsak_view-augdt,
    augbl type bsak_view-augbl,
    dmbtr type bsak_view-dmbtr,
    dmbe2 type bsak_view-dmbe2,
    budat type bsak_view-budat,
    buzei type bsak_view-buzei,
  end of ty_bsak,

  begin of ty_bsik,
    bukrs type bsik_view-bukrs,
    lifnr type bsik_view-lifnr,
    belnr type bsik_view-belnr,
    dmbtr type bsik_view-dmbtr,
    dmbe2 type bsik_view-dmbe2,
    wrbtr type bsik_view-wrbtr,
    budat type bsik_view-budat,
    buzei type bsik_view-buzei,
    gsber type bsik_view-gsber,
    shkzg type bsik_view-shkzg,
    umskz type bsik_view-umskz,
    zfbdt type bsik_view-zfbdt,
    zbd1t type bsik_view-zbd1t,
  end of ty_bsik,

  begin of ty_bsik_comp,
    bukrs type bsik_view-bukrs,
    belnr type bsik_view-belnr,
    gjahr type bsik_view-gjahr,
    dmbtr type bsik_view-dmbtr,
    dmbe2 type bsik_view-dmbe2,
    augbl type bsik_view-augbl,
    budat type bsik_view-budat,
    shkzg type bsik_view-shkzg,
  end of ty_bsik_comp,

  begin of ty_bsid_comp,
    bukrs type bsid_view-bukrs,
    belnr type bsid_view-belnr,
    gjahr type bsid_view-gjahr,
    dmbtr type bsid_view-dmbtr,
    dmbe2 type bsid_view-dmbe2,
    augbl type bsid_view-augbl,
    budat type bsid_view-budat,
    shkzg type bsid_view-shkzg,
  end of ty_bsid_comp,

  begin of ty_lfa1,
    lifnr type lfa1-lifnr,
    name1 type lfa1-name1,
    stras type lfa1-stras,
    regio type lfa1-regio,
    land1 type lfa1-land1,
    ktokk type lfa1-ktokk,
    lnrza type lfa1-lnrza,
    ort01 type lfa1-ort01,
  end of ty_lfa1,

  begin of ty_kna1,
    kunnr type kna1-kunnr,
    name1 type kna1-name1,
  end of ty_kna1,

  begin of ty_lfbk,
    lifnr  type lfbk-lifnr,
    banks  type lfbk-banks,
    bankl  type lfbk-bankl,
    bankn  type lfbk-bankn,
    bvtyp  type lfbk-bvtyp,
    del(1),
  end of ty_lfbk,


  begin of ty_bnka,
    bankl type bnka-bankl,
    swift type bnka-swift,
    stras type bnka-stras,
    banka type bnka-banka,
    provz type bnka-provz,
    banks type bnka-banks,
    brnch type bnka-brnch,
  end of ty_bnka,

  begin of ty_tiban,
    banks  type tiban-banks,
    bankl  type tiban-bankl,
    tabkey type tiban-tabkey,
    iban   type tiban-iban,
  end of ty_tiban,


  begin of ty_t012k,
    bukrs type t012k-bukrs,
    hbkid type t012k-hbkid,
    bankn type t012k-bankn,
    hkont type t012k-hkont,
    waers type t012k-waers,
  end of ty_t012k,

  begin of ty_bkpf2,
    bukrs type bkpf-bukrs,
    belnr type bkpf-belnr,
    gjahr type bkpf-gjahr,
    stblg type bkpf-stblg,
  end of ty_bkpf2,

  begin of ty_atach,
    down(1),
    updl(1),
*     NOME_ARQ(25),
*     DIR_SRV TYPE ZFIT0036-PATH,
    path(100),
  end of ty_atach,

  begin of ty_linha,
    nro_ref(20)          type c,
    pontov1(1),
    um_ext_resp_cta(3)   type c,
    pontov2(1),
    nro_cta_cliente(40)  type c,
    pontov3(1),
    moeda_cta_client(3)  type c,
    pontov4(1),
    moeda_transf(3)      type c,
    pontov5(1),
    dt_transf(10)        type c,
    pontov6(1),
    vlr_pagto(12)        type c,
    pontov7(1),
    motivo_transf(2)     type c,
    pontov8(1),
    ordem_priorid(4)     type c,
    pontov9(1),
    nro_cta_benef(40)    type c,
    pontov10(1),
    nome_benef(35)       type c,
    pontov11(1), "168
    end_benef(105)       type c,
    pontov12(1), "274
    swift_banco(12)      type c,
    pontov13(1),
    benef_cod_clear(2)   type c,
    pontov14(1),
    benef_conta(31)      type c,
    pontov15(1),
    benef_nome_bco(35)   type c,
    pontov16(1),
    benef_end_banco(105) type c,
    pontov17(1),
    inter_swift_bco(12)  type c,
    pontov18(1),
    interm_cod_clear(2)  type c,
    pontov19(1),
    interm_conta(31)     type c,
    pontov20(1),
    interm_nome_bco(35)  type c,
    pontov21(1),
    interm_end_bco(105)  type c,
    pontov22(1),
    det_pgto(140)        type c,
    pontov23(1),
    det_deb(3)           type c,
    pontov24(1),
    inform_adicio(105)   type c,
  end of ty_linha,

  " Layout EDI sem NAS
  begin of ty_linha2,
    nro_ref(20)          type c,
    pontov1(1),
    nro_cta_cliente(40)  type c,
    pontov3(1),
    moeda_cta_client(3)  type c,
    pontov4(1),
    moeda_transf(3)      type c,
    pontov5(1),
    dt_transf(10)        type c,
    pontov6(1),
    vlr_pagto(12)        type c,
    pontov7(1),
    motivo_transf(2)     type c,
    pontov8(1),
    ordem_priorid(4)     type c,
    pontov9(1),
    nro_cta_benef(40)    type c,
    pontov10(1),
    nome_benef(35)       type c,
    pontov11(1), "168
    end_benef(105)       type c,
    pontov12(1), "274
    swift_banco(12)      type c,
    pontov13(1),
    benef_cod_clear(2)   type c,
    pontov14(1),
    benef_conta(31)      type c,
    pontov15(1),
    benef_nome_bco(35)   type c,
    pontov16(1),
    benef_end_banco(105) type c,
    pontov17(1),
    inter_swift_bco(12)  type c,
    pontov18(1),
    interm_cod_clear(2)  type c,
    pontov19(1),
    interm_conta(31)     type c,
    pontov20(1),
    interm_nome_bco(35)  type c,
    pontov21(1),
    interm_end_bco(105)  type c,
    pontov22(1),
    det_pgto(140)        type c,
    pontov23(1),
    det_deb(3)           type c,
    pontov24(1),
    inform_adicio(105)   type c,
  end of ty_linha2,

  begin of ty_cadbai,
    tipo(1)                                   ,
    de_tipo(15)                               ,
    opera(2)                                  ,
    de_opera(15)                              ,
    moeda         type zfit0036-moeda_pgto,
    dt_lct        type dats,
    dt_pgt        type dats,
    motivo        type zfit0040-motivo,
    descricao     type zfit0040-descricao,
    status        type zfit0036-status,
    observacao    type zfit0036-observacao,
    referencia    type zfit0036-referencia,
    vlr_pgto      type zfit0036-vlr_pgto,
    vlr_sldo      type zfit0036-vlr_pgto,
    rg_atualizado type zfit0036-rg_atualizado,
  end of ty_cadbai,

  begin of ty_pagar ,
    lifnr    type zib_contabil-hkont,       "Fornecedor
    name1    type lfa1-name1,
    budat    type zib_contabil-budat,       "Dt.Lcto
    belnr    type bsik_view-belnr,               "Doc.Cta
    invoice  type zfit0036-invoice,         "Invoice
    vlr_pgto type zfit0036-vlr_pgto,
    vlr_comp type zfit0036-vlr_pgto,
    vlr_sld  type zfit0036-vlr_pgto,
    obj_key  type zfit0036-obj_key,
    navio    type zfit0036-navio,
    belnr36  type zfit0036-belnr,
    buzei36  type zfit0036-buzei,
    fg_ok(1),
  end of ty_pagar,


  begin of ty_adiant ,
    lifnr    type zib_contabil-hkont,       "Fornecedor
    name1    type lfa1-name1,
    budat    type zib_contabil-budat,       "Dt.Lcto
    belnr    type bsik_view-belnr,               "Doc.Cta
    invoice  type zfit0036-invoice,         "Invoice
    vlr_pgto type zfit0036-vlr_pgto,
    vlr_comp type zfit0036-vlr_pgto,
    vlr_sld  type zfit0036-vlr_pgto,
    obj_key  type zfit0036-obj_key,
    navio    type zfit0036-navio,
    belnr36  type zfit0036-belnr,
    buzei36  type zfit0036-buzei,
    augbl    type bsak_view-augbl,
    fg_ok(1),
  end of ty_adiant,

  begin of ty_adiantbra,
    augbl    type bsid_view-augbl,
    augdt    type bsid_view-augdt,
    waers    type bsid_view-waers,
    vlr_doc  type bsid_view-dmbe2,
    vlr_comp type bsid_view-dmbe2,
    vlr_sld  type bsid_view-dmbe2,
    blart    type bsid_view-blart,
    umskz    type bsid_view-umskz,
    obj_key  type zfit0036-obj_key,
    fg_ok(1),
  end of ty_adiantbra,

  begin of ty_notas,
    augbl    type bsid_view-augbl,
    augdt    type bsid_view-augdt,
    vbeln    type vbfa-vbeln,
    waers    type bsid_view-waers,
    vlr_doc  type bsid_view-dmbe2,
    vlr_comp type bsid_view-dmbe2,
    vlr_sld  type bsid_view-dmbe2,
    fg_ok(1),
  end of ty_notas,

  begin of ty_receber ,
    checkbox(1),
    budat       type bsid_view-budat,
    belnr       type bsid_view-belnr,
    invoice     type zfit0036-invoice	,
    vlr_doc     type bsid_view-dmbe2,
    vlr_comp    type bsid_view-dmbe2,
    vlr_sld     type bsid_view-dmbe2,
    obj_key     type zfit0036-obj_key,
  end of ty_receber,

  begin of ty_invoice ,
    checkbox(1),
    budat       type bsid_view-budat,
    belnr       type bsid_view-belnr,
    invoice     type zfit0036-invoice	,
    vlr_doc     type bsid_view-dmbe2,
    vlr_comp    type bsid_view-dmbe2,
    vlr_sld     type bsid_view-dmbe2,
    obj_key     type zfit0036-obj_key,
    navio       type zfit0036-navio,
  end of ty_invoice,


  begin of ty_compe ,
    belnr_cp type bsik_view-belnr,               "Doc.Cta
    belnr_cr type bsik_view-belnr,               "Doc.Cta
    vlr_comp type zfit0036-vlr_pgto,
  end of ty_compe,


  begin of ty_externo ,
    mark(1),
    belnr         type zib_contabil_chv-belnr,
    invoice       type zfit0036-invoice,
    vlr_doc       type bsid_view-dmbe2,
    vlr_pgt       type bsid_view-dmbe2,
    vlr_cre       type bsid_view-dmbe2,
    vlr_cre_b     type bsid_view-dmbe2,
    vlr_sld       type bsid_view-dmbe2,
    vlr_real      type bsid_view-dmbe2,
    dt_cred       type zfit0036-dt_pgto,
    tx_cambio     type zfit0036-tx_cambio,
    obj_key       type zfit0036-obj_key,
    navio         type zfit0036-navio,
    bukrs         type zib_contabil-bukrs,
    budat         type zib_contabil-budat,
    belnr36       type zfit0036-belnr,
    buzei36       type zfit0036-buzei,
    status        type zfit0036-status,
    lote          type zfit0036-lote,
    rg_atualizado type zfit0036-rg_atualizado,
    bvtyp         type zfit0036-bvtyp,
    lnrza         type zfit0036-lifnr,
    lifnr         type zfit0036-lifnr,
    bukrs_bra     type zib_contabil-bukrs,
    fg_ok(1),
    line_color(4) type c,
    style         type lvc_t_styl,
  end of ty_externo,

  begin of ty_brasil ,
    mark(1),
    checkbox(1),
    vbelv        type vbfa-vbelv,
    vbeln        type vbfa-vbeln,
    belnr        type bkpf-belnr,
    gjahr        type bsid_view-gjahr,
    buzei        type bkpf-belnr,
    invoice      type zfit0036-invoice,
    nr_re        type zfit0041-nr_re,
    numero_due   type zdoc_exp-numero_due,
    situacao_due type zsdt0170-situacao_due,
    vlr_usd      type bsid_view-dmbe2,
    vlr_bai      type bsid_view-dmbe2,
    vlr_sld      type bsid_view-dmbe2,
    navio        type znom_transporte-ds_nome_transpor,
    c_pais       type zsdt0174-destino_country,
    x_pais       type t005t-landx,
    bldat        type bkpf-bldat,
    blart        type bkpf-blart,
    bukrs        type bkpf-bukrs,
    budat        type bkpf-budat,
    waers        type bkpf-waers,
    kursf        type bkpf-kursf,
    xblnr        type bkpf-xblnr,
    kunnr        type bsid_view-kunnr,
    belnr36      type zfit0036-belnr,
    belnr2       type bsid_view-belnr,
    gjahr2       type bsid_view-gjahr,
    buzei2       type bsid_view-belnr,
    ds_porto     type znom_transporte-ds_porto,
    fg_ok(1),
  end of ty_brasil,

  begin of ty_zfit0041,
    nr_invoice type zfit0041-nr_invoice,
    nr_re      type zfit0041-nr_re,
  end of ty_zfit0041,

  begin of ty_zdoc_exp,
    numero_due       type zdoc_exp-numero_due,
    id_due           type zdoc_exp-id_due,
    nr_registro_expo type zdoc_exp-nr_registro_expo,
    id_nomeacao_tran type zdoc_exp-id_nomeacao_tran,
    vbeln            type zdoc_exp-vbeln,
  end of ty_zdoc_exp,

  begin of ty_vbfa,
    vbeln type vbfa-vbeln,
    vbelv type vbfa-vbelv,
    erdat type vbfa-erdat,
    gjahr type bkpf-gjahr,
    awkey type bkpf-awkey,
  end of ty_vbfa,

  begin of ty_due,
    id_due       type zsdt0170-id_due,
    numero_due   type zsdt0170-numero_due,
    situacao_due type zsdt0170-situacao_due,

  end of ty_due,

  begin of ty_vbrk,
    vbeln type vbrk-vbeln,
    vkorg type vbrk-vkorg,
    fkdat type vbrk-fkdat,
    gjahr type bkpf-gjahr,
    awkey type bkpf-awkey,
  end of ty_vbrk,

  begin of ty_bkpf,
    bukrs type bkpf-bukrs,
    gjahr type bkpf-gjahr,
    awkey type bkpf-awkey,
    belnr type bkpf-belnr,
    blart type bkpf-blart,
    budat type bkpf-budat,
    bldat type bkpf-bldat,
    xblnr type bkpf-xblnr,
    waers type bkpf-waers,
    kursf type bkpf-kursf,
  end of ty_bkpf,

  begin of ty_bsad,
    belnr  type bsad_view-belnr,
    bukrs  type bsad_view-bukrs,
    gjahr  type bsad_view-gjahr,
    augbl  type bsad_view-augbl,
    augdt  type bsad_view-augdt,
    dmbe2  type bsad_view-dmbe2,
    kunnr  type bsad_view-kunnr,
    gjahr2 type bsad_view-gjahr,
  end of ty_bsad,

  begin of ty_bsid,
    bukrs type bsid_view-bukrs,
    belnr type bsid_view-belnr,
    gjahr type bsid_view-gjahr,
    dmbtr type bsid_view-dmbtr,
    dmbe2 type bsid_view-dmbe2,
    kunnr type bsid_view-kunnr,
    buzei type bsid_view-buzei,
    waers type bsid_view-waers,
    budat type bsid_view-budat,
    blart type bsid_view-blart,
    augbl type bsid_view-augbl,
    augdt type bsid_view-augdt,
    umskz type bsid_view-umskz,
    xblnr type bsid_view-xblnr,
    shkzg type bsid_view-shkzg,
    vbeln type vbfa-vbeln,
  end of ty_bsid,

  begin of ty_zsdt0053,
    nro_sol_ov type zsdt0053-nro_sol_ov,
    vbeln      type zsdt0053-vbeln,
  end of ty_zsdt0053,

  ty_arquivo(904) type c,


  begin of ty_estra ,
    bukrs     type zinv_lotes_aprov-bukrs,
    lote      type zinv_lotes_aprov-lote,
    valor_de  type zinv_aprovador-valor_de,
    valor_ate type zinv_aprovador-valor_ate,
    aprovador type zinv_aprovador-aprovador,
    nivel     type zinv_aprovador-nivel,
    estado(4),
    opcoes(4),
  end of ty_estra,

  begin of ty_zinv_aprovador,
    bukrs     type zinv_aprovador-bukrs,
    bukrs_ate type zinv_aprovador-bukrs_ate,
    tipo      type zinv_aprovador-tipo,
    nivel     type zinv_aprovador-nivel,
    waers     type zinv_aprovador-waers,
    aprovador type zinv_aprovador-aprovador,
    valor_de  type zinv_aprovador-valor_de,
    valor_ate type zinv_aprovador-valor_ate,
  end of ty_zinv_aprovador,

  begin of ty_zinv_lotes_aprov,
    bukrs      type zinv_lotes_aprov-bukrs,
    lote       type zinv_lotes_aprov-lote,
    nivel      type zinv_lotes_aprov-nivel,
    aprovador  type zinv_lotes_aprov-aprovador,
    valor_de   type zinv_lotes_aprov-valor_de,
    valor_ate  type zinv_lotes_aprov-valor_ate,
    data_atual type zinv_lotes_aprov-data_atual,
    hora_atual type zinv_lotes_aprov-hora_atual,
    usuario    type zinv_lotes_aprov-usuario,
  end of ty_zinv_lotes_aprov,


  begin of ty_saida,
    mark(1),
    checkbox(1),
    icon0(4)            type c,                        "Attachment
    icon1(4)            type c,                        "St.Liquid
    icon2(4)            type c,                        "St.Lib
    lote                type zfit0036-lote,            "Lote Pg
    lote_cp             type zfit0036-lote_cp,
    tipo(20),                                     "Adto. Performance
    invoice             type zfit0036-invoice,         "Invoice
    belnr               type zib_contabil_chv-belnr,   "Doc.Cta
    buzei               type bsik-buzei,   "Doc.Cta
    belnr_o             type zib_contabil_chv-belnr,   "Doc.Cta Original
    budat               type zib_contabil-budat,       "Dt.Lcto
    lifnr               type zib_contabil-hkont,       "Fornecedor
    name1               type lfa1-name1,
    lnrza               type lfa1-lnrza,
    dmbtr               type anlp-nafaz,
    dmbe2               type anlp-nafaz,
    navio               type zfit0036-navio,
    augbl               type bsak_view-augbl,
    gjahr               type bsak_view-gjahr,
    augdt               type bsak_view-augdt,
    hbkid               type zfit0036-hbkid,
    banks_1             type zfit0036-banks_1, "US - 76596 - CSB
    observacao          type zfit0036-observacao,
    referencia          type zfit0036-referencia,
    obj_key             type zfit0036-obj_key,
    moeda_pgto          type zfit0036-moeda_pgto,
    dt_pgto             type zfit0036-dt_pgto,
    zfbdt               type bsik-zfbdt,
    vlr_pgto            type zfit0036-vlr_pgto,
    status              type zfit0036-status,
    waers               type zib_contabil-waers,
    gsber               type zib_contabil-gsber,
    hkont               type zib_contabil-hkont,
    rg_atualizado       type zfit0036-rg_atualizado,
    forma_pg(20), "       TYPE ZFIT0036-FORMA_PG,
    desp_tar(20),
    motivo              type zfit0036-motivo,
    bukrs               type zfit0036-bukrs,
    icon3(4)            type c,     "Lista Contas Bancaria (C.Bank)
    bvtyp               type lfbk-bvtyp,
    lifnr_z             type zib_contabil-hkont,
    belnr36             type zfit0036-belnr,
    buzei36             type zfit0036-buzei,
    belnr_adt_c         type zfit0036-belnr_adt_c,
    nr_solov            type zfit0036-invoice,         "Sol.OV
    vbeln               type vbfa-vbeln,
    kunnr               type bsid_view-kunnr,
    blart               type bsid_view-blart,
    umskz               type bsid_view-umskz,
    opera               type zfit0036-operacao,
    invoice_terc        type zfit0036-invoice_terc,
    matnr               type zfit0036-matnr,
    maktx               type makt-maktx,
    id_tipo_invoice(50),
    line_color(4)       type c, "Used to store row color attributes
    user_create         type zfit0036-user_create,
    usuario_arq         type zfit0037-usuario_arq,
    nome_arquivo        type zfit0037-nome_arquivo,

* Ini - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual
*    bvtyp           TYPE zfit0036-bvtyp,
*    banks_1         TYPE zfit0036-banks_1,
    bankl_1             type zfit0036-bankl_1,
    bankn_1             type zfit0036-bankn_1,
    iban_1              type zfit0036-iban_1,
    swift_1             type zfit0036-swift_1,
    banka_1             type zfit0036-banka_1,
*    bvtyp               TYPE zfit0036-bvtyp,
    bvtyp_2             type zfit0036-bvtyp_2,
    banks_2             type zfit0036-banks_2,
    bankl_2             type zfit0036-bankl_2,
    bankn_2             type zfit0036-bankn_2,
    iban_2              type zfit0036-iban_2,
    swift_2             type zfit0036-swift_2,
    banka_2             type zfit0036-banka_2,
    del                 type char1,
    style               type lvc_t_styl,
* Fim - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual

  end of ty_saida,


  begin of ty_arq_bancos,
    bukrs        type  zfit0036-bukrs,
    belnr        type  zfit0037-belnr,
    augbl        type  zfit0037-augbl,
    nome_benef   type  zfit0037-nome_benef,
    dt_transf    type  zfit0037-dt_transf,
    lote         type  zfit0037-lote,
    vlr_pagto    type  zfit0037-vlr_pagto,
    moeda_transf type  zfit0037-moeda_transf,
    nro_ref      type  zfit0037-nro_ref,
    invoice      type  zfit0036-invoice	,
    nome_arquivo type  zfit0037-nome_arquivo,
    usuario_arq  type  zfit0037-usuario_arq	,
    dt_ger_arq   type  zfit0037-dt_ger_arq,
    hr_ger_arq   type  zfit0037-hr_ger_arq,
    status(30)   type c,
  end of ty_arq_bancos,

  begin of ty_data,
    bvtyp type lfbk-bvtyp,
    banks type lfbk-banks,
    bankl type lfbk-bankl,
    bankn type lfbk-bankn,
    bkont type lfbk-bkont,
    iban  type tiban-iban,
    swift type bnka-swift,
    banka type bnka-banka,
  end of ty_data.


*** US 76100 - CBRAND - Inicio
data: begin of tg_bsis_cbanco occurs 0,
        bukrs type bsis_view-bukrs,
        belnr type bsis_view-belnr,
        gjahr type bsis_view-gjahr,
        hkont type bsis_view-hkont,
        dmbtr type bsis_view-dmbtr,
        dmbe2 type bsis_view-dmbe2,
        waers type bsis_view-waers,
        wrbtr type bsis_view-wrbtr,
        fdlev type skb1-fdlev,
      end of tg_bsis_cbanco.
*** US 76100 - CBRAND - Fim

types:begin of ty_estrutura.
        include type slis_fieldcat_main.
        include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
data:
  tg_selectedcell type lvc_t_cell,
  wg_selectedcell type lvc_s_cell,
  tl_index_rows   type lvc_t_row,
  wl_index_rows   type lvc_s_row,
  x_field(30),

  begin of tg_status  occurs 0,
    tipo(20),
    invoice  type zfit0036-invoice,         "Invoice
    belnr    type zib_contabil_chv-belnr,   "Doc.Cta
    budat    type zib_contabil-budat,       "Dt.Lcto
    lifnr    type zib_contabil-hkont,       "Fornecedor
    name1    type lfa1-name1,
    dmbe2    type zib_contabil-dmbe2,
    status   type zfit0036-status,
    obj_key  type zfit0036-obj_key,
    belnr36  type zfit0036-belnr,
    buzei36  type zfit0036-buzei,
  end of tg_status,

  begin of tg_criaadt  occurs 0,
    bschl     type tbsl-bschl,
    dc(1),
    saknr     type skat-saknr,
    txt50     type skat-txt50,
    umskz     type t074t-shbkz,
    wrbtr     type bseg-wrbtr,
    icon(4)   type c,     "Lista Contas Bancaria (C.Bank)
    bvtyp     type lfbk-bvtyp,
    kostl     type csks-kostl,
    saknrz    type skat-saknr,
    gsber     type bseg-gsber,
    lifnr     type lfa1-lifnr,
    belnr     type bsik_view-belnr,
    saknr_ba  type skat-saknr,
    saknrz_ba type skat-saknr,
*** RMNI - CS1028997 - Validação bloqueio p/ chave de lançamento - 26.10.2022 - Início
    cellstyle type lvc_t_styl,
*** RMNI - CS1028997 - Validação bloqueio p/ chave de lançamento - 26.10.2022 - Fim
  end of tg_criaadt,

  begin of tg_conta occurs 0,
    bvtyp type lfbk-bvtyp,
    banks type lfbk-banks,
    bankl type lfbk-bankl,
    bankn type lfbk-bankn,
    bkont type lfbk-bkont,
    iban  type tiban-iban,
    swift type bnka-swift,
    banka type bnka-banka,
    lifnr type lfbk-lifnr,
  end of tg_conta.

data: ti_bdcdata              type standard table of bdcdata ,   "Guarda o mapeamento
      t_messtab               type table of bdcmsgcoll,

      it_saida_pa             type table of zfir0001,
      wa_saida_pa             type zfir0001,
      it_zfit0036_det         type table of zfit0036, "RJF
      it_saida                type table of ty_saida,
      it_saida_aux            type table of ty_saida,
      it_saida_aux2           type table of ty_saida,
      t_usermd                type standard table of  rgsb4 with header line,
      t_forint                type standard table of  rgsb4 with header line,
      it_zfit0076             type table of zfit0076,
      it_zfit0036             type table of ty_zfit0036,
      it_zfit0036_pgt         type table of ty_zfit0036,
      it_zfit0036_aux         type table of ty_zfit0036,
      it_zfit0036_adt         type table of ty_zfit0036,
      it_zfit0036_com         type table of ty_zfit0036,
      it_zfit0036_com36       type table of ty_zfit0036,
      it_zfit0036_per         type table of ty_zfit0036,
      it_zfit0037             type table of ty_zfit0037,
      it_zfit0037_arq         type table of zfit0037,
      it_zib_contabil         type table of ty_zib_contabil,
      it_zib_contabil_err     type table of ty_zib_contabil_err,
      it_zib_contabil_per     type table of ty_zib_contabil,
      it_zib_contabil_chv     type table of ty_zib_contabil_chv,
      it_zib_contabil_chv_com type table of ty_zib_contabil_chv,
      it_zib_contabil_chv_per type table of ty_zib_contabil_chv,
      it_bsak                 type table of ty_bsak,
      it_bsak_adt             type table of ty_bsak,
      it_bsak_36              type table of ty_bsak,
      it_bsik                 type table of ty_bsik,
      it_bsik_shdb            type table of ty_bsik,
      it_bsik_36              type table of ty_bsik,
      it_bsik_adt             type table of ty_bsik,
      it_bsik_aux             type table of ty_bsik,
      it_bsik_per             type table of ty_bsik,
      it_bsik_comp            type table of ty_bsik_comp,
      it_bsid_comp            type table of ty_bsid_comp,
      it_kna1                 type table of ty_kna1,
      it_lfa1                 type table of ty_lfa1,
      it_lfa1x                type table of ty_lfa1,
      it_lfa1_aux             type table of ty_lfa1,
      it_lfbk                 type table of ty_lfbk,
      it_lfbk_cria            type table of ty_lfbk,
      it_bnka                 type table of ty_bnka,
      it_tiban                type table of ty_tiban,
      it_zfit0047             type table of ty_zfit0047,
      it_zfit0041             type table of ty_zfit0041,
      it_zfit0042             type table of zfit0042,
      it_zfit0042_2           type table of zfit0042,
      it_zfit0042_cre         type table of zfit0042,
      it_zfit0042_bsad        type table of zfit0042,
      it_zfit0042_taxa        type table of zfit0042,
      it_zdoc_exp             type table of ty_zdoc_exp,
      it_due                  type table of ty_due,
      it_vbfa_brasil          type table of ty_vbfa,
      it_vbfa                 type table of ty_vbfa,
      it_vbfa_2               type table of ty_vbfa,
      it_vbrk                 type table of ty_vbrk,
      it_bkpf_brasil          type table of ty_bkpf,
      it_bkpf                 type table of ty_bkpf,
      it_bkpf2                type table of ty_bkpf2,
      it_bsad                 type table of ty_bsad,
      it_bsad_brasil2         type table of bsad,
      it_bsad_brasil          type table of ty_bsad,
      it_bsad_braadt          type table of ty_bsad,
      it_bsad_augbl           type table of ty_bsad,
      it_bsad_augbl2          type table of ty_bsad,
      it_bsid                 type table of ty_bsid,
      it_bsid_brasil          type table of ty_bsid,
      it_bsid_braadt          type table of ty_bsid,
      it_bsid_branfi          type table of ty_bsid,
      it_bsid_branfr          type table of ty_bsid,
      it_bsid_com             type table of ty_bsid,
      it_bsid_com_aux         type table of ty_bsid,
      it_bsid_shdb            type table of ty_bsid,
      it_bsid_aux             type table of ty_bsid,
      it_bsid_42              type table of ty_bsid,
      it_bsis                 type table of bsis,
      tg_bsik_aux             type table of bsik,
      it_makt                 type table of ty_makt,
      it_zsdt0053             type table of ty_zsdt0053,
      t_arquivo               type table of ty_arquivo,
      tg_externo              type table of ty_externo,
      tg_externo_aux          type table of ty_externo,
      tg_pagar                type table of ty_pagar,
      tg_adiant               type table of ty_adiant,
      tg_adiantbra            type table of ty_adiantbra,
      tg_notas                type table of ty_notas,
      tg_invoice              type table of ty_invoice,
      tg_compe                type table of ty_compe,
      tg_compe_aux            type table of ty_compe,
      tg_receber              type table of ty_receber,
      tg_externo_aux2         type table of ty_externo,
      tg_brasil               type table of ty_brasil,
      it_zinv_aprovador       type table of ty_zinv_aprovador,
      it_zinv_lotes_aprov     type table of ty_zinv_lotes_aprov,
      it_zglt038_2            type table of zglt038,
      it_zadt_sol_aprov       type table of zadt_sol_aprov,
      tg_estra                type table of ty_estra,
      it_color                type table of lvc_s_scol,
      it_arq_bancos           type table of ty_arq_bancos,
      it_remessa              type table of epsfili,
      it_backup               type table of epsfili,
      gt_data                 type table of ty_data.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
data:
  wg_criaadt          like line of tg_criaadt,
  wg_status           like line of tg_status,
  wa_cont             type ref to cl_gui_custom_container,
  wa_alv              type ref to cl_gui_alv_grid,
  wa_bdcdata          like line of ti_bdcdata,

  wa_saida            type ty_saida,
  wa_saida_aux        type ty_saida,
  wa_zib_contabil     type ty_zib_contabil,
  wa_zib_contabil_chv type ty_zib_contabil_chv,
  wa_zib_contabil_err type ty_zib_contabil_err,
  wa_zfit0076         type zfit0076,
  wa_zfit0036         type ty_zfit0036,
  wa_zfit0036_ins     type zfit0036,
  wa_zfit0042         type zfit0042,
  wa_zfit0047         type ty_zfit0047,
  wa_zfit0037         type ty_zfit0037,
  wa_zfit0037_arq     type zfit0037,
  wa_zfit0040         type zfit0040,
  wa_bsak             type ty_bsak,
  wa_bsik             type ty_bsik,
  wa_kna1             type ty_kna1,
  wa_lfa1             type ty_lfa1,
  wa_lfbk             type ty_lfbk,
  wa_bnka             type ty_bnka,
  wa_tiban            type ty_tiban,
  wa_t012k            type ty_t012k,
  wa_color            type lvc_s_scol,
  wg_cadinvo          type ty_cadinvo,
  wg_cadbai           type ty_cadbai,
  wg_externo          type ty_externo,
  wg_pagar            type ty_pagar,
  wg_cadadt           type ty_cadadt,
  wg_compe            type ty_compe,
  wg_receber          type ty_receber,
  wg_adiant           type ty_adiant,
  wg_adiantbra        type ty_adiantbra,
  wg_notas            type ty_notas,
  wg_invoice          type ty_invoice,
  wg_brasil           type ty_brasil,
  wg_conta            like line of tg_conta,
  wg_cadliq           type ty_cadliq,
  wg_cadger           type ty_cadger,
  wg_cadest           type ty_cadest,
  wg_cadcom           type ty_cadcom,
  wg_cadbrasil        type ty_cadbrasil,
  wg_atach            type ty_atach,
  wa_zfit0041         type ty_zfit0041,
  wa_zdoc_exp         type ty_zdoc_exp,
  wa_due              type ty_due,
  wa_vbfa             type ty_vbfa,
  wa_vbfa_brasil      type vbfa,
  wa_vbfa_2           type ty_vbfa,
  wa_vbrk             type ty_vbrk,
  wa_bkpf             type ty_bkpf,
  wa_bkpf2            type ty_bkpf2,

  wa_bkpf_2           type bkpf,
  wa_bseg_2           type bseg,
  wa_bseg_aux         type bseg,
  wa_bsik_aux         type bsik,
  wa_t012k_aux        type t012k,
  wa_zglt035_2        type zglt035,
  wa_zglt038_2        type zglt038,


  wa_bsad             type ty_bsad,
  wa_bsad2            type ty_bsad,
  wa_bsad_brasil2     type bsad,
  wa_bsid             type ty_bsid,
  wa_bsis             type bsis,
  wa_bsik_comp        type ty_bsik_comp,
  wa_bsid_comp        type ty_bsid_comp,
  wa_makt             type ty_makt,
  w_arquivo           type ty_arquivo,
  lv_nome_arquivo     type string,
  wa_zinv_aprovador   type ty_zinv_aprovador,
  wa_zinv_lotes_aprov type ty_zinv_lotes_aprov,
  wa_zsdt0053         type ty_zsdt0053,
  wa_estra            type ty_estra,
  "WA_ZFIT0040         TYPE ZFIT0040,
  wa_zfit0043         type zfit0043,
  w_linha             type ty_linha,
  w_linha2            type ty_linha2,
  vfilename           type string,
  vfilename_srv       type rfpdo-rfbifile,
  vdirname_srv        type rfpdo-rfbifile,
  wa_arq_bancos       type ty_arq_bancos,
  wa_remessa          type epsfili,
  wa_backup           type epsfili,
  wa_data             type ty_data.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
data:
  it_fcat    type table of ty_estrutura,
  s_variant  type disvariant           , " Tabela Estrutura co
  t_top      type slis_t_listheader,
  xs_events  type slis_alv_event,
  events     type slis_t_event,
  gd_layout  type slis_layout_alv,
  t_print    type slis_print_alv,
  v_report   like sy-repid,
  t_sort     type slis_t_sortinfo_alv with header line,
  it_setleaf like table of setleaf initial size 0 with header line,
  estrutura  type table of ty_estrutura,
  vg_i       type i,
  wa_style   type lvc_s_styl,
  style      type lvc_t_styl with header line.

data: ok-code           type sy-ucomm,
      wg_mensagem(25),
      vnum(10)          type c,
      vnumb(5)          type c,
      vnr_range(2),
      vlote_ad(10)      type c,
      vnum2(11)         type c,
      vfor(06)          type c,
      vlifnr_9000       type lfa1-lifnr,
      vseq(10)          type p,
      vseqb(5)          type p,
      wl_erro(1),
      wl_belnr          type bsik-belnr,
      wl_chk(1),
      wg_documento(10),
      vstatus(1),
      vmudar(1),
      indrow            type lvc_t_row,
      w_ind             type lvc_t_row with header line,
      w_cont            type i,
      w_contc(5),
      w_mensagem(50),
      w_flag(1)         value '',
      vtentativas       type i,
      gw_choice         type sy-tabix,
      vl_form           type tdsfname,
      vl_name           type rs38l_fnam,
      wg_zfit0173       type zfit0173,
      wg_zfit0168       type zfit0168,                      " BUG51556
      wg_zfit01682      type zfit0168,                      " BUG51556
      wg_savelocal(250) type c,                             " BUG51556
      wg_savesrv(250)   type c,                             " BUG51556
      gva_hbkid         type t012t-hbkid.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
constants: begin of c_tab_strip_imp,
             tab1 like sy-ucomm value 'TAB_STRIP_IMP_FC1',
             tab2 like sy-ucomm value 'TAB_STRIP_IMP_FC2',
             tab3 like sy-ucomm value 'TAB_STRIP_IMP_FC3',
           end of c_tab_strip_imp.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
controls:  tab_strip_imp type tabstrip.
data: begin of g_tab_strip_imp,
        subscreen   like sy-dynnr,
        prog        like sy-repid value 'ZFIR0025',
        pressed_tab like sy-ucomm value c_tab_strip_imp-tab1,
      end of g_tab_strip_imp.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
*Class definition for ALV toolbar
class:      lcl_alv_toolbar   definition deferred.

data: editcontainer        type ref to cl_gui_custom_container,
      cl_container         type ref to cl_gui_custom_container,
      g_custom_container   type ref to cl_gui_custom_container,

      container_1          type ref to cl_gui_container,
      container_2          type ref to cl_gui_container,
      splitter             type ref to cl_gui_splitter_container,

      editor               type ref to cl_gui_textedit,
      cl_container_95      type ref to cl_gui_docking_container,
      cl_container_05      type ref to cl_gui_docking_container,
      obj_dyndoc_id        type ref to cl_dd_document,
      cl_grid              type ref to cl_gui_alv_grid,
      wa_stable            type lvc_s_stbl,
      wa_afield            type lvc_s_fcat,
      it_fieldcat          type lvc_t_fcat,
      w_fieldcat           type lvc_s_fcat,
      i_sort               type lvc_t_sort,
      wa_layout            type lvc_s_layo,
      is_stable            type lvc_s_stbl value 'XX',
      wg_repname           like sy-repid,
      wg_x_variant         like disvariant,
      wg_exit(1)           type c,
      wg_save(1)           type c,
      wg_variant           like disvariant,
      wg_atualiza(1),
      wg_atualiza_shdb(1),
      xvlrpg               type bsid_view-dmbe2,
      vsub_tela            type sy-dynnr,

      grid1                type ref to cl_gui_alv_grid,
      grid2                type ref to cl_gui_alv_grid,
      grid3                type ref to cl_gui_alv_grid,
      grid4                type ref to cl_gui_alv_grid,
      grid5                type ref to cl_gui_alv_grid,
      grid6                type ref to cl_gui_alv_grid,
      grid7                type ref to cl_gui_alv_grid,
      grid8                type ref to cl_gui_alv_grid,
      grid9                type ref to cl_gui_alv_grid,
      grid10               type ref to cl_gui_alv_grid,
      grid11               type ref to cl_gui_alv_grid,
      grid14               type ref to cl_gui_alv_grid,
      grid15               type ref to cl_gui_alv_grid,

      obg_toolbar          type ref to lcl_alv_toolbar,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      obg_conteiner_cta    type ref to cl_gui_custom_container,
      obg_conteiner_ext    type ref to cl_gui_custom_container,
      obg_conteiner_bra    type ref to cl_gui_custom_container,


      g_cc_cta             type scrfname value 'CC_CONTAS',
      g_cc_ext             type scrfname value 'CC_EXTERIOR',
      g_cc_bra             type scrfname value 'CC_BRASIL',
      obg_conteiner_cp     type ref to cl_gui_custom_container,
      obg_conteiner_cpa    type ref to cl_gui_custom_container,
      obg_conteiner_cr     type ref to cl_gui_custom_container,
      g_cc_pagar           type scrfname value 'CC_PAGAR',
      g_cc_receber         type scrfname value 'CC_RECEBER',
      obg_conteiner_adt    type ref to cl_gui_custom_container,
      obg_conteiner_adta   type ref to cl_gui_custom_container,
      obg_conteiner_inv    type ref to cl_gui_custom_container,
      g_cc_adiant          type scrfname value 'CC_ADIANTAMENTO',
      g_cc_invoice         type scrfname value 'CC_INVOICE',
      obg_conteiner_adtbra type ref to cl_gui_custom_container,
      obg_conteiner_notas  type ref to cl_gui_custom_container,
      g_cc_adiantbra       type scrfname value 'CC_ADIANTBRASIL',
      g_cc_notas           type scrfname value 'CC_NOTAS',
      obg_conteiner_cria   type ref to cl_gui_custom_container,
      g_cc_criaadiant      type scrfname value 'CC_LANCAMENTO',
      obg_conteiner_st     type ref to cl_gui_custom_container,
      g_cc_status          type scrfname value 'CC_STATUS'.

data:
  grid12              type ref to cl_gui_alv_grid,
  obg_conteiner_estra type ref to cl_gui_custom_container,
  g_cc_estra          type scrfname value 'CC_ESTRA'.


data: grid13             type ref to cl_gui_alv_grid,
      editcontainer_bnco type ref to cl_gui_custom_container,
      obg_conteiner_bnco type ref to cl_gui_custom_container,
      obj_dyndoc_bnco    type ref to cl_dd_document.



*Declaration for toolbar buttons
data : ty_toolbar type stb_button.
*** TREE DE MENSAGENS.
data node_itab like node_str occurs 0.
data node like node_str.

data container type ref to cl_gui_custom_container.
data splitter_msg type ref to cl_gui_easy_splitter_container.
data right type ref to cl_gui_container.
data left  type ref to cl_gui_container.

data: p_dt_trans type range of zfit0037-dt_transf,
      w_dt_trans like line of p_dt_trans.



data tree type ref to cl_gui_simple_tree.

data behaviour_left type ref to cl_dragdrop.
data behaviour_right type ref to cl_dragdrop.

data handle_tree type i.
data num_row type i value 0.

** Criação de tabela dinamica
data: t_fieldcatalog type lvc_t_fcat,
      w_fieldcatalog type lvc_s_fcat,
      gt_f4          type lvc_t_f4 with header line.
"WA_LAYOUT             TYPE LVC_S_LAYO,
"WA_STABLE             TYPE LVC_S_STBL.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
constants: c_0               type c value '0',
           c_1               type c value '1',
           c_2               type c value '2',
           c_b               type c value 'B',
           c_s               type c value 'S',
           c_l               type c value 'L',
           c_x               type c value 'X',
           c_d               type c value 'D',
           c_k               type c value 'K',
           c_w               type c value 'W',
           c_f               type c value 'F',
           c_t               type c value 'T',
           c_i               type c value 'I',
           c_n               type c value 'N',
           c_h               type c value 'H',
           c_ag(2)           type c value 'AG',
           c_ne(2)           type c value 'NE',
           c_01(2)           type c value '01',
           c_30(2)           type c value '30',
           c_40(2)           type c value '40',
           c_50(4)           type c value '0050',
           c_76(2)           type c value '76',
           c_71(2)           type c value '71',
           c_72(2)           type c value '72',
           c_br(2)           type c value 'BR',
           c_lf(2)           type c value 'LF',
           c_lr(2)           type c value 'LR',
           c_z1(2)           type c value 'Z1',
           c_add(3)          type c value 'ADD',
           c_del(3)          type c value 'DEL',
           c_dg1(3)          type c value 'DG1',
           c_dg2(3)          type c value 'DG2',
           c_dummy_header(3) type c value '099',
           c_dummy_itens(3)  type c value '098',
           c_exit(4)         type c value 'EXIT',
           c_root(4)         type c value 'ROOT',
           c_minimizar(4)    type c value '@K2@',
           c_maximizar(4)    type c value '@K1@',
           c_back(4)         type c value 'BACK',
           c_save(4)         type c value 'SAVE',
           c_desat(5)        type c value 'DESAT',
           c_dmbtr(5)        type c value 'DMBTR',
           c_modif(5)        type c value 'MODIF',
           c_cancel(6)       type c value 'CANCEL',
           c_deldoc(6)       type c value 'DELDOC',
           c_dclick(6)       type c value 'DCLICK',
           c_search(6)       type c value 'SEARCH',
           c_atuali(6)       type c value 'ATUALI',
           c_add_msg(7)      type c value 'ADD_MSG',
           c_del_msg(7)      type c value 'DEL_MSG',
           c_clos_msg(8)     type c value 'CLOS_MSG',
           c_save_msg(8)     type c value 'SAVE_MSG',
           c_show_msgre(10)  type c value 'SHOW_MSGRE'.

data: zva_tabix       type sy-tabix.

*ALRS
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar definition.
  public section.
*Constructor
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm,

      handle_user_command_cta for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.                    "lcl_alv_toolbar DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "constructor

  method on_toolbar.
    data: wl_desactive.


    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.


    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    if vsub_tela = '4100'.
      ty_toolbar-icon      = icon_copy_object.
      ty_toolbar-function  = 'COPIAR'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = 'Copiar'.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.
    elseif  vsub_tela = '4200'.
      ty_toolbar-icon      = icon_copy_object.
      ty_toolbar-function  = 'MARCAR'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = 'Marcar'.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.
    endif.


    ty_toolbar-butn_type = 3.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.


**   Call reorganize method of toolbar manager to
**   display the toolbar
    call method c_alv_toolbarmanager->reorganize
      exporting
        io_alv_toolbar = e_object.
  endmethod.                    "on_toolbar

  method handle_user_command.
    data: tl_criaadt_aux like table of tg_criaadt,
          wl_criaadt     like line of tg_criaadt,
          wl_lines       type sy-tabix.

    data: vdt_cred   type zfit0036-dt_pgto,
          vtx_cambio type zfit0036-tx_cambio.

    refresh: tl_criaadt_aux.

    if sy-dynnr ne '4000'.
      case e_ucomm.
        when c_add.
          tl_criaadt_aux[] = tg_criaadt[].
          refresh: tg_criaadt.
          loop at tl_criaadt_aux into wl_criaadt.
            append wl_criaadt to tg_criaadt.
          endloop.
          clear: wl_criaadt.
          wl_criaadt-icon = icon_message_question_small.
          append wl_criaadt to tg_criaadt.

          call method grid9->refresh_table_display
            exporting
              is_stable = wa_stable.
        when c_del.
          call method grid9->get_selected_cells
            importing
              et_cell = tg_selectedcell.

          loop at tg_selectedcell into wg_selectedcell.
            delete tg_criaadt index wg_selectedcell-row_id-index.
          endloop.

          call method grid9->refresh_table_display
            exporting
              is_stable = wa_stable.
      endcase.
    elseif sy-dynnr = '4000'.
      if vsub_tela = '4100'.
        loop at tg_externo into wg_externo.
          if wg_externo-dt_cred is not initial.
            vdt_cred = wg_externo-dt_cred.
            vtx_cambio = wg_externo-tx_cambio.
            exit.
          endif.
        endloop.
        loop at tg_externo into wg_externo.
          wg_externo-dt_cred = vdt_cred.
          wg_externo-tx_cambio = vtx_cambio.
          modify tg_externo from wg_externo index sy-tabix transporting dt_cred tx_cambio.
        endloop.
        call method grid1->refresh_table_display
          exporting
            is_stable = wa_stable.
      else.
        loop at tg_brasil into wg_brasil.
          if wg_brasil-checkbox = 'X'.
            clear wg_brasil-checkbox.
          else.
            wg_brasil-checkbox = 'X'.
          endif.
          modify tg_brasil from wg_brasil index sy-tabix transporting checkbox.
        endloop.
      endif.
    endif.

  endmethod.                    "zm_handle_user_command

  method handle_user_command_cta.
    data: tl_conta_aux like table of tg_conta,
          wl_conta     like line of tg_conta,
          wl_lines     type sy-tabix.

    refresh: tl_conta_aux.

    case e_ucomm.
      when c_add.
        "TL_CONTA_AUX[] = TG_CONTA[].
        refresh: tg_conta.
        loop at tl_conta_aux into wl_conta.
          append wl_conta to tg_conta.
        endloop.
        clear: wl_conta.
        append wl_conta to tg_conta.

        call method grid10->refresh_table_display
          exporting
            is_stable = wa_stable.
      when c_del.
        call method grid10->get_selected_cells
          importing
            et_cell = tg_selectedcell.

        refresh: tg_conta.
        call method grid10->refresh_table_display
          exporting
            is_stable = wa_stable.
    endcase.

  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_alv_toolbar IMPLEMENTATION

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
class lcl_event_handler definition.

  public section.
    class-methods:
      catch_hotspot_cria
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no.

    class-methods:
      catch_hotspot_4
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no.
    class-methods:
      catch_hotspot_41
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no.
    class-methods:
      on_data_changed_4 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_5 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_cp for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_adt for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_adtbra for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_cr for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_cria for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_st for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
    class-methods:
      on_data_changed_cta for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
    class-methods:
      on_data_changed_finished_4 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished_5 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished_cp for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished_adt for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished_cr for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.
    class-methods:
      on_data_changed_finished_bra for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished_cria for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished_st for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.
    class-methods:
      on_data_changed_finished_cta for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_f4                      for event onf4                 of cl_gui_alv_grid
        importing e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

endclass.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_event_handler implementation.
  method catch_hotspot_cria.
    data wl_tbsl type tbsl.
    data vlifnr_cria type lfa1-lifnr.

    read table tg_criaadt into wg_criaadt index e_row_id-index.
    if sy-subrc = 0.
      if e_column_id = 'ICON' and  ( wg_criaadt-bschl is not initial or wg_cadger-ds_operacao is not  initial ).
        select single *
        from tbsl
        into wl_tbsl
        where bschl = wg_criaadt-bschl.
        if ( wl_tbsl-koart = 'K' or  wg_cadger-question is initial ) or
           ( wg_criaadt-lifnr is not initial ).
          if ( wl_tbsl-koart = 'K' or  wg_cadger-question is initial ) and  wg_criaadt-saknrz is not initial.
            select lifnr banks bankl bankn bvtyp
                 from lfbk
                 into table it_lfbk_cria
                 where lifnr = wg_criaadt-saknrz.
            vlifnr_cria = wg_criaadt-saknrz.
          endif.
          if it_lfbk_cria[] is initial.
            if wg_criaadt-lifnr is not initial .
              select lifnr banks bankl bankn bvtyp
                  from lfbk
                  into table it_lfbk_cria
                  where lifnr = wg_criaadt-lifnr.
              vlifnr_cria = wg_criaadt-lifnr.
            endif.
          endif.
          delete   it_lfbk where bvtyp+3(1) ne '1' and bvtyp+3(1) ne '2'.
          types: begin of ty_itab ,
                   value    type lfbk-bvtyp,
                   name(80) type c,
                 end of ty_itab.

          data: msg_alv  type char80,
                itab_msg type table of ty_itab,
                wtab_msg type  ty_itab.
          msg_alv = 'Lista contas bancárias'.
          clear wtab_msg.
          wtab_msg-name    = 'Pais|ChBanco        |Cta Banc          |Seq.Cta'.
          append wtab_msg to itab_msg .
          clear wtab_msg.
          wtab_msg-name    = '-----------------------------------------------'.
          append wtab_msg to itab_msg .
          clear wtab_msg.
          loop at it_lfbk_cria  into wa_lfbk where lifnr = vlifnr_cria .
            if wa_lfbk-bvtyp+3(1)  ne '2'.
              wtab_msg-value   = wa_lfbk-bvtyp.
              wtab_msg-name+0  = wa_lfbk-banks.
              wtab_msg-name+4  = '|'.
              wtab_msg-name+5  = wa_lfbk-bankl.
              wtab_msg-name+20 = '|'.
              wtab_msg-name+21 = wa_lfbk-bankn.
              wtab_msg-name+39 = '|'.
              wtab_msg-name+40 = wa_lfbk-bvtyp.
              append wtab_msg to itab_msg .
              clear wtab_msg.
            endif.
          endloop.
          call function 'POPUP_WITH_TABLE_DISPLAY'
            exporting
              endpos_col   = 190
              endpos_row   = 16
              startpos_col = 135
              startpos_row = 12
              titletext    = msg_alv
            importing
              choise       = gw_choice
            tables
              valuetab     = itab_msg
            exceptions
              break_off    = 1
              others       = 2.
          check gw_choice > 2.
          read table itab_msg into wtab_msg index gw_choice.
          if wtab_msg-value is not initial.
            wg_criaadt-bvtyp = wtab_msg-value.
            modify tg_criaadt from wg_criaadt index e_row_id-index transporting bvtyp.
            call method grid9->refresh_table_display.
            if sy-subrc <> 0.

            endif.
          endif.
        endif.
      endif.
    endif.
  endmethod.                    "catch_hotspot_4

  method catch_hotspot_4.
    read table tg_brasil into wg_brasil index e_row_id-index.
    if sy-subrc = 0.
      if e_column_id = 'BELNR'.
        set parameter id 'BLN' field wg_brasil-belnr.
        set parameter id 'BUK' field wg_brasil-bukrs.
        set parameter id 'GJR' field wg_brasil-budat+0(4).
        call transaction 'FB03' and skip first screen.
      elseif e_column_id = 'VBELV'.
        set parameter id 'AUN' field wg_brasil-vbelv.
        call transaction 'VA03' and skip first screen.
      elseif e_column_id = 'VBELN'.
        set parameter id 'VF'  field wg_brasil-vbeln.
        call transaction 'VF03' and skip first screen.
      endif.
    endif.
  endmethod.                    "catch_hotspot_4
  method catch_hotspot_41.
    read table tg_externo into wg_externo index e_row_id-index.
    if sy-subrc = 0.
      if e_column_id = 'BELNR'.
        set parameter id 'BLN' field wg_externo-belnr.
        set parameter id 'BUK' field wg_externo-bukrs.
        set parameter id 'GJR' field wg_externo-budat+6(4).
        call transaction 'FB03' and skip first screen.
      endif.
    endif.
  endmethod.                    "catch_hotspot_41

  method on_data_changed_4.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          v_pgt    type bsik_view-dmbe2,
          v_sld    type bsik_view-dmbe2,
          v_rea    type bsik_view-dmbe2,
          v_tax    type bkpf-kursf.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_PGT'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_pgt.

      read table tg_externo into wg_externo index ls_good-row_id.
      v_sld = wg_externo-vlr_doc - v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.

    endloop.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_CRE'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_pgt.

      read table tg_externo into wg_externo index ls_good-row_id.
      v_sld = wg_externo-vlr_pgt - wg_externo-vlr_cre_b -  v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.

      v_sld = wg_externo-tx_cambio * v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_REAL'
          i_value     = lv_value.
    endloop.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'TX_CAMBIO'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_tax.

      read table tg_externo into wg_externo index ls_good-row_id.
      v_sld = wg_externo-vlr_cre * v_tax.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_REAL'
          i_value     = lv_value.
    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_5.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          v_sld    type bsik_view-dmbe2,
          v_bai    type bsik_view-dmbe2.


    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_BAI'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_bai.

      read table tg_brasil into wg_brasil index ls_good-row_id.

      v_sld = wg_brasil-vlr_usd - v_bai.
      if v_sld lt 0.
        v_sld = 0.
        v_bai = 0.
        move v_bai to lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_BAI'
            i_value     = lv_value.
      endif.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.

    endloop.
  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_st.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'STATUS'.

      lv_value = ls_good-value.
      condense lv_value no-gaps.
      if lv_value ne '' and lv_value ne 'B'.
        read table tg_status into wg_status index ls_good-row_id.
        lv_value = wg_status-status.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'STATUS'
            i_value     = lv_value.
        if sy-langu = 'P'.
          message s836(sd) display like 'E' with  'Status só pode ser branco ou "B"'.
        else.
          message s836(sd) display like 'E' with  'Status only can be null or "B"'.
        endif.

      endif.
    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_cp.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          v_pgt    type bsik_view-dmbe2,
          v_sld    type bsik_view-dmbe2.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_COMP'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_pgt.

      read table tg_pagar into wg_pagar index ls_good-row_id.
      v_sld = wg_pagar-vlr_pgto - v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.
    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_adt.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          v_pgt    type bsik_view-dmbe2,
          v_sld    type bsik_view-dmbe2.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_COMP'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_pgt.

      read table tg_adiant into wg_adiant index ls_good-row_id.
      v_sld = wg_adiant-vlr_pgto - v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.
    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_adtbra.
    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          v_pgt    type bsik_view-dmbe2,
          v_sld    type bsik_view-dmbe2.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_COMP'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_pgt.

      read table tg_adiantbra into wg_adiantbra index ls_good-row_id.
      v_sld = wg_adiantbra-vlr_doc - v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.
    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_cr.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          v_pgt    type bsik_view-dmbe2,
          v_sld    type bsik_view-dmbe2.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_COMP'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      move lv_value to v_pgt.

      read table tg_receber into wg_receber index ls_good-row_id.
      v_sld = wg_receber-vlr_doc - v_pgt.
      move v_sld to lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_SLD'
          i_value     = lv_value.
    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_cria.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          wl_tbsl  type tbsl,
          wl_name1 type skat-txt50,
*** RMNI - CS1028997 - Validação bloqueio p/ chave de lançamento - 26.10.2022 - Início
          lv_fname type lvc_fname,
          lv_style type raw4.
*** RMNI - CS1028997 - Validação bloqueio p/ chave de lançamento - 26.10.2022 - Fim

    loop at er_data_changed->mt_good_cells
                         into ls_good
                         where fieldname = 'BSCHL'.

      lv_value = ls_good-value.
      select single *
       from tbsl
       into wl_tbsl
       where bschl = lv_value.

      if wl_tbsl-shkzg = 'H'.
        lv_value = 'C'.
      else.
        lv_value = 'D'.
      endif.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DC'
          i_value     = lv_value.

      clear  lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNR'
          i_value     = lv_value.

*** RMNI - CS1028997 - Validação bloqueio p/ chave de lançamento - 26.10.2022 - Início
      if ls_good-value eq '29'.
        lv_fname = 'SAKNR'.
        lv_style = cl_gui_alv_grid=>mc_style_disabled.

        call method er_data_changed->modify_style
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = lv_fname
            i_style     = lv_style.

        clear: lv_fname, lv_style.
      elseif ls_good-value eq '50'.
        lv_fname = 'LIFNR'.
        lv_style = cl_gui_alv_grid=>mc_style_disabled.

        call method er_data_changed->modify_style
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = lv_fname
            i_style     = lv_style.

        clear: lv_fname, lv_style.
      endif.
*** RMNI - CS1028997 - Validação bloqueio p/ chave de lançamento - 26.10.2022 - Fim

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TXT50'
          i_value     = lv_value.


    endloop.

    loop at er_data_changed->mt_good_cells
                           into ls_good
                           where fieldname = 'SAKNR'.
      lv_value = ls_good-value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNRZ'
          i_value     = lv_value.

      vl_value = ls_good-value.
      condense lv_value no-gaps.
      condense vl_value no-gaps.

      if wg_cadger-question = 'X'.
        clear wg_criaadt.
        read table tg_criaadt into wg_criaadt index  ls_good-row_id.
        if sy-subrc ne 0.
          exit.
        elseif wg_criaadt-bschl is initial.
          exit.
        endif.

        select single *
          from tbsl
          into wl_tbsl
          where bschl = wg_criaadt-bschl.

        if wl_tbsl-koart = 'K'.
          select single name1
            from lfa1
            into wl_name1
            where lifnr = lv_value.
        elseif wl_tbsl-koart = 'D'.
          select single name1
            from kna1
            into wl_name1
            where kunnr = lv_value.
        elseif wl_tbsl-koart = 'S'.
          select single  txt50
            from skat
            into wl_name1
            where spras = sy-langu
            and ktopl = '0050'
            and saknr = lv_value.
        endif.
      else.
        select single name1
            from lfa1
            into wl_name1
            where lifnr = lv_value.
      endif.

      if sy-subrc = 0.
        move wl_name1 to lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'TXT50'
            i_value     = lv_value.

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = vl_value
          importing
            output = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'SAKNR'
            i_value     = lv_value.

      endif.
    endloop.


    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'SAKNR_BA'.
      lv_value = ls_good-value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNRZ_BA'
          i_value     = lv_value.

      vl_value = ls_good-value.
      condense lv_value no-gaps.
      condense vl_value no-gaps.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = vl_value
        importing
          output = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNR_BA'
          i_value     = lv_value.

    endloop.

    "BUG 53981 - Inicio
    loop at er_data_changed->mt_good_cells
                       into ls_good
                       where fieldname = 'LIFNR'.

      vl_value = ls_good-value.


      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNR'
          i_value     = lv_value.

      condense lv_value no-gaps.
      condense vl_value no-gaps.


      if lv_value is initial and vl_value is not initial.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'SAKNRZ'
            i_value     = vl_value.


        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = vl_value
          importing
            output = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'SAKNR'
            i_value     = lv_value.


        if wg_cadger-question = 'X'.
          clear wg_criaadt.
          read table tg_criaadt into wg_criaadt index  ls_good-row_id.
          if sy-subrc ne 0.
            exit.
          elseif wg_criaadt-bschl is initial.
            exit.
          endif.

          select single *
            from tbsl
            into wl_tbsl
            where bschl = wg_criaadt-bschl.

          if wl_tbsl-koart = 'K'.
            select single name1
              from lfa1
              into wl_name1
              where lifnr = vl_value.
          elseif wl_tbsl-koart = 'D'.
            select single name1
              from kna1
              into wl_name1
              where kunnr = vl_value.
          elseif wl_tbsl-koart = 'S'.
            select single  txt50
              from skat
              into wl_name1
              where spras = sy-langu
              and ktopl = '0050'
              and saknr = vl_value.
          endif.
        else.
          select single name1
              from lfa1
              into wl_name1
              where lifnr = vl_value.
        endif.

        if sy-subrc = 0.
          move wl_name1 to lv_value.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'TXT50'
              i_value     = lv_value.
        endif.

      endif.
    endloop.
    "BUG 53981 - Fim
  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_cta.

    data: ls_good     type lvc_s_modi,
          lv_value    type lvc_value,
          vl_value    type lvc_value,
          wl_lfbk     type lfbk,
          tl_tiban    type table of tiban,
          wl_tiban    type tiban,
          wl_bnka     type bnka,
          vlifnr      type lfbk-lifnr,
          vbvtyp      type lfbk-bvtyp,
          vbvtyp2     type lfbk-bvtyp,
          vtabkey     type tiban-tabkey,
          vbanks      type lfbk-banks,
          vbankl      type lfbk-bankl,
          vbankn      type lfbk-bankn,
          vbkont      type lfbk-bkont,
          viban       type tiban-iban,
          vlines      type sy-tabix,
          tabix       type sy-tabix,
          w_answer(1).


    loop at er_data_changed->mt_good_cells
                                     into ls_good
                                     where fieldname = 'BANKL'.
      vbankl =  ls_good-value.
      if sy-dynnr = 2000.
        if wg_cadinvo-lnrza is not initial.
          vlifnr = wg_cadinvo-lnrza.
        else.
          vlifnr = wg_cadinvo-lifnr.
        endif.
      elseif sy-dynnr = 4000.
        if tg_externo[] is not initial.
          read table tg_externo into wg_externo index 1.
          if wg_externo-lnrza is not initial.
            vlifnr = wg_externo-lnrza.
          else.
            vlifnr = wg_externo-lifnr.
          endif.
        endif.
      elseif sy-dynnr = 9000.
        loop at tg_criaadt into wg_criaadt.
          if wg_criaadt-lifnr is not initial and ( wg_criaadt-bschl <> 40 and wg_criaadt-bschl <> 50 ). "BUG 51556
            vlifnr = wg_criaadt-lifnr.
          endif.
        endloop.
      else.
        clear vlifnr.
      endif.


      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vlifnr
        importing
          output = vlifnr.

      select single *
        from lfbk
        into wl_lfbk
        where lifnr = vlifnr
        and bankl  = vbankl.

      clear lv_value.
      if sy-subrc = 0.
        lv_value = wl_lfbk-banks.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'BANKS'
          i_value     = lv_value.
    endloop.


    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'IBAN'
                                 or    fieldname = 'BANKS'
                                 or    fieldname = 'BANKL'
                                 or    fieldname = 'BANKN' .
      read table tg_conta into wg_conta index  ls_good-row_id.

      if ls_good-row_id = 2.
        lv_value = wg_conta-banks.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'BANKS'
            i_value     = lv_value.

        lv_value = wg_conta-bankl.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'BANKL'
            i_value     = lv_value.

        lv_value = wg_conta-bankn.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'BANKN'
            i_value     = lv_value.

        lv_value = wg_conta-iban.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'IBAN'
            i_value     = lv_value.
        exit.
      endif.

      move: wg_conta-banks to vbanks,
            wg_conta-bankl to vbankl,
            wg_conta-bankn to vbankn,
*            WG_CONTA-BKONT TO VBKONT,
            wg_conta-iban  to viban.

      if ls_good-fieldname  = 'IBAN'.
        viban =  ls_good-value.
      elseif ls_good-fieldname  = 'BANKS'.
        vbanks =  ls_good-value.
      elseif ls_good-fieldname  = 'BANKL'.
        vbankl =  ls_good-value.
      elseif ls_good-fieldname  = 'BANKN'.
        vbankn =  ls_good-value.
      endif.

      if vbanks is  not initial and
         vbankl is  not initial and
        ( vbankn is  not initial or viban is not initial ) .
        if sy-dynnr = 2000.
          if wg_cadinvo-lnrza is not initial.
            vlifnr = wg_cadinvo-lnrza.
          else.
            vlifnr = wg_cadinvo-lifnr.
          endif.
        elseif sy-dynnr = 4000.
          if tg_externo[] is not initial.
            read table tg_externo into wg_externo index 1.
            if wg_externo-lnrza is not initial.
              vlifnr = wg_externo-lnrza.
            else.
              vlifnr = wg_externo-lifnr.
            endif.
          endif.
        elseif sy-dynnr = 9000.
          loop at tg_criaadt into wg_criaadt.
            if wg_criaadt-lifnr is not initial and ( wg_criaadt-bschl <> 40 and wg_criaadt-bschl <> 50 ). "BUG 51556
              vlifnr = wg_criaadt-lifnr.
            endif.
          endloop.
        else.
          clear vlifnr.
        endif.

        if vlifnr is initial.
          if sy-langu eq 'P'.
            message  'Fornecedor não encontrados' type 'I'.
            exit.
          else.
            message  'Supplier not found' type 'I'.
            exit.
          endif.
        endif.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vlifnr
          importing
            output = vlifnr.

        lv_value = vlifnr.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'LIFNR'
            i_value     = lv_value.

        refresh tl_tiban.
        move vlifnr to vtabkey.
        if vbankn is not initial.
          select single *
            from lfbk
            into wl_lfbk
            where lifnr = vlifnr
            and banks  = vbanks
            and bankl  = vbankl
            and bankn  = vbankn.
        else.
          " Acesso o TIBAN sem tabkey para pegar a data mais recente
          select  *
           from tiban
           into table tl_tiban
           where banks   = vbanks
           and   bankl   = vbankl
           and   tabname in ('LFBK', 'BUT0BK')
           and   iban    = viban
           order by erdat descending.
        endif.

        if sy-subrc eq 0 or tl_tiban[] is not initial.
          if vbankn is not initial.
            "ponto
            lv_value = wl_lfbk-bkont.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'BKONT'
                i_value     = lv_value.

            lv_value = wl_lfbk-bvtyp.
            vbvtyp2 = wl_lfbk-bvtyp.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'BVTYP'
                i_value     = lv_value.

            " acesso sem tabkey
            select single *
             from tiban
             into wl_tiban
             where banks   =  vbanks
             and   bankl   =  vbankl
             and   bankn   =  vbankn
             "AND   TABKEY  =  VTABKEY
             and   tabname  in ('LFBK', 'BUT0BK').
            if sy-subrc ne 0.
              clear lv_value.
            else.
              lv_value = wl_tiban-iban.
            endif.

            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'IBAN'
                i_value     = lv_value.

          else.
            read table tl_tiban into wl_tiban index 1.
            lv_value = wl_tiban-bankn.
            vbankn   = wl_tiban-bankn..
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'BANKN'
                i_value     = lv_value.

            select single *
            from lfbk
            into wl_lfbk
            where lifnr = vlifnr
            and banks  = vbanks
            and bankl  = vbankl
            and bankn  = vbankn.
            if sy-subrc = 0.
              "ponto
              lv_value = wl_lfbk-bkont.
              call method er_data_changed->modify_cell
                exporting
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'BKONT'
                  i_value     = lv_value.

              lv_value = wl_lfbk-bvtyp.
              vbvtyp2 = wl_lfbk-bvtyp.
              call method er_data_changed->modify_cell
                exporting
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'BVTYP'
                  i_value     = lv_value.
            else.
              if sy-langu eq 'P'.
                message 'Dados informados não encontrado no dado mestre do fornecedor' type 'I'.
                exit.
              else.
                message 'Data informed not found in the supplier master data' type 'I'.
                exit.
              endif.
            endif.
          endif.

          select single *
            from bnka
            into wl_bnka
            where banks	=	vbanks
            and   bankl	=	vbankl.

          clear lv_value.
          if sy-subrc = 0.
            lv_value = wl_bnka-swift.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'SWIFT'
                i_value     = lv_value.
            lv_value = wl_bnka-banka.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'BANKA'
                i_value     = lv_value.
          else.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'SWIFT'
                i_value     = lv_value.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'BANKA'
                i_value     = lv_value.
            exit.
          endif.
          describe table tg_conta lines vlines.
          concatenate vbvtyp2+0(3) '2' into vbvtyp.
          select single *
           from lfbk
           into wl_lfbk
           where lifnr = vlifnr
           and bvtyp  = vbvtyp.

          if sy-subrc = 0.
            move vlifnr to vtabkey.
            select single *
               from tiban
               into wl_tiban
               where banks   =  wl_lfbk-banks
               and   bankl   =  wl_lfbk-bankl
               and   bankn   =  wl_lfbk-bankn
               and   tabkey  =  vtabkey
               and   tabname in ('LFBK', 'BUT0BK').
            if sy-subrc ne 0.
              clear wl_tiban.
            endif.
            select single *
               from bnka
               into wl_bnka
               where banks  = wl_lfbk-banks
               and   bankl  = wl_lfbk-bankl.
            if sy-subrc ne 0.
              clear wl_bnka.
            endif.

            wg_conta-bvtyp    = vbvtyp.
            wg_conta-banks    = wl_lfbk-banks.
            wg_conta-bankl    = wl_lfbk-bankl.
            wg_conta-bankn    = wl_lfbk-bankn.
            wg_conta-bkont    = wl_lfbk-bkont.
            wg_conta-iban     = wl_tiban-iban.
            wg_conta-swift    = wl_bnka-swift.
            wg_conta-banka    = wl_bnka-banka.
            wg_conta-lifnr    = vlifnr.

            if vlines = 2.
              delete tg_conta index 2.
            endif.
            append wg_conta to tg_conta.

          else.
            if sy-langu = 'P'.
              message  'Dados da SEGUNDA conta não encontrados' type 'I'.
            else.
              message  'SECOND Account data not found' type 'I'.
            endif.
          endif.
        else.
          if sy-langu = 'P'.
            message 'Dados informados não encontrado no dado mestre do fornecedor' type 'I'.
            exit.
          else.
            message 'Informed data not found in the supplier master data' type 'I'.
            exit.
          endif.
        endif.
      endif.

    endloop.

  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_finished_4.
    data: vtotal           type zfit0036-vlr_pgto,
          wvalor(16),
          wflag_externo(1).

    data: l_dynpfields type table of dynpread,
          w_dynpfields type dynpread.

    loop at tg_externo into wg_externo.
      if wg_externo-rg_atualizado = 'T'.
        wflag_externo = 'X'.
        exit.
      endif.
    endloop.
    vtotal = 0.
    loop at tg_externo into wg_externo.
      if wflag_externo = 'X'.
        add wg_externo-vlr_cre to vtotal.
      else.
        add wg_externo-vlr_pgt to vtotal.
      endif.
    endloop.

    "LÊ VARIÁVEL
    refresh l_dynpfields.
    clear   w_dynpfields.
    w_dynpfields-fieldname  = 'WG_CADBAI-VLR_PGTO'.
    append w_dynpfields to l_dynpfields.

    call function 'DYNP_VALUES_READ'
      exporting
        dyname     = sy-repid
        dynumb     = sy-dynnr
      tables
        dynpfields = l_dynpfields.

    read table l_dynpfields into w_dynpfields index 1.
    translate w_dynpfields-fieldvalue using '. '.
    condense w_dynpfields-fieldvalue no-gaps.
    translate w_dynpfields-fieldvalue using ',.'.
    if w_dynpfields-fieldvalue is initial.
      clear  wg_cadbai-vlr_pgto.
    else.
      move w_dynpfields-fieldvalue to wg_cadbai-vlr_pgto.
    endif.

    wg_cadbai-vlr_sldo = wg_cadbai-vlr_pgto - vtotal.
    if wg_cadbai-vlr_sldo lt 0.
      wg_cadbai-vlr_sldo = 0.
    endif.

*    "ATUALIZA VARIÁVEL
*    DATA: STEP_LINE LIKE SY-STEPL.
*    CALL FUNCTION 'DYNP_GET_STEPL'
*      IMPORTING
*        POVSTEPL        = STEP_LINE
*      EXCEPTIONS
*        STEPL_NOT_FOUND = 1
*        OTHERS          = 2.
*
*    REFRESH L_DYNPFIELDS.
*    CLEAR   W_DYNPFIELDS.
*    W_DYNPFIELDS-FIELDNAME  = 'WG_CADBAI-VLR_SLDO'.
*    WRITE WG_CADBAI-VLR_SLDO TO WVALOR.
*    W_DYNPFIELDS-FIELDVALUE = WVALOR.
*    W_DYNPFIELDS-STEPL      = STEP_LINE.
*    APPEND W_DYNPFIELDS TO L_DYNPFIELDS.
*
**    CALL FUNCTION 'DYNP_UPDATE_FIELDS'
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        DYNAME               = SY-REPID
*        DYNUMB               = SY-DYNNR
*      TABLES
*        DYNPFIELDS           = L_DYNPFIELDS
*      EXCEPTIONS
*        INVALID_ABAPWORKAREA = 1
*        INVALID_DYNPROFIELD  = 2
*        INVALID_DYNPRONAME   = 3
*        INVALID_DYNPRONUMMER = 4
*        INVALID_REQUEST      = 5
*        NO_FIELDDESCRIPTION  = 6
*        UNDEFIND_ERROR       = 7
*        OTHERS               = 8.
*    IF SY-SUBRC <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*
**** Método de atualização de dados na Tela
*    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.



  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_5.

****** Método de atualização de dados na Tela
*    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_cp.

**** Método de atualização de dados na Tela
*    CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_adt.

**** Método de atualização de dados na Tela
*    CALL METHOD GRID5->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_cria.

*** Método de atualização de dados na Tela
    call method grid9->refresh_table_display
      exporting
        is_stable = wa_stable.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_cta.
    data: tabix       type sy-tabix,
          w_china(1)  value '',
          w_answer(1).

    loop at tg_conta into wg_conta .
      if wg_conta-banks = 'CN'.
        w_china = 'X'.
      endif.
      if wg_conta-iban is not initial.
        tabix = sy-tabix.
        call function 'POPUP_TO_CONFIRM'
          exporting
            text_question         = 'Conta tem IBAN, mantem ?'(102)
            text_button_1         = 'Sim'(100)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(101)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
            start_column          = 25
            start_row             = 6
          importing
            answer                = w_answer
          exceptions
            text_not_found        = 1
            others                = 2.

        if w_answer = '2'. "não
          clear wg_conta-iban.
          modify tg_conta from wg_conta index tabix transporting iban.
        endif.
      endif.
    endloop.

    if w_china = 'X'.
      message 'Confirma os dados bancários de pagamento para CHINA?'(103) type 'I'.
    endif.

*** Método de atualização de dados na Tela
    call method grid10->refresh_table_display
      exporting
        is_stable = wa_stable.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_st.

*** Método de atualização de dados na Tela
    call method grid11->refresh_table_display
      exporting
        is_stable = wa_stable.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_bra.

*** Método de atualização de dados na Tela
    call method grid7->refresh_table_display
      exporting
        is_stable = wa_stable.


  endmethod.                    "on_data_changed_finisheD

  method on_data_changed_finished_cr.

*** Método de atualização de dados na Tela
    call method grid4->refresh_table_display
      exporting
        is_stable = wa_stable.


  endmethod.                    "on_data_changed_finisheD

  method on_f4.
    types: begin of t_f4_structure,
             fieldtext type dfies-fieldtext,
             fieldname type dfies-fieldname,
           end of t_f4_structure.

    field-symbols: <itab> type lvc_t_modi.

    data: ls_modi type lvc_s_modi.

    types : begin of ty_conta,
              saknr type skat-saknr,
              txt50 type skat-txt50,
            end of ty_conta.

    data: wl_return_lg type  ddshretval,
          wl_dselclg   type  dselc,
          tl_conta     type table of ty_conta,
          wl_conta     type ty_conta,
          tl_return_lg type table of ddshretval,
          tl_dselclg   type table of dselc,
          wl_tbsl      type tbsl,
          vlifnr       type lfa1-lifnr.

    case e_fieldname.
      when 'BANKL'.
        types : begin of ty_lfbk,
                  banks type lfbk-banks,
                  bankl type lfbk-bankl,
                  banka type bnka-banka,
                end of ty_lfbk.


        data: wl_return_chvl type  ddshretval,
              wl_dselcchvl   type  dselc,
              tl_lfbk        type table of ty_lfbk,
              wl_lfbk        type ty_lfbk,
              tl_return_chvl type table of ddshretval,
              tl_dselcchvl   type table of dselc,
              tabix_lfbk     type sy-tabix.

        if sy-dynnr = 2000.
          if wg_cadinvo-lnrza is not initial.
            vlifnr = wg_cadinvo-lnrza.
          else.
            vlifnr = wg_cadinvo-lifnr.
          endif.
        elseif sy-dynnr = 4000.
          if tg_externo[] is not initial.
            read table tg_externo into wg_externo index 1.
            if wg_externo-lnrza is not initial.
              vlifnr = wg_externo-lnrza.
            else.
              vlifnr = wg_externo-lifnr.
            endif.
          endif.
        elseif sy-dynnr = 9000.
          loop at tg_criaadt into wg_criaadt.
            if wg_criaadt-lifnr is not initial and ( wg_criaadt-bschl <> 40 and wg_criaadt-bschl <> 50 ). "BUG 51556
              vlifnr = wg_criaadt-lifnr.
            endif.
          endloop.
        else.
          clear vlifnr.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vlifnr
          importing
            output = vlifnr.


        select distinct lfbk~banks lfbk~bankl bnka~banka
          from lfbk
          inner join bnka
          on    bnka~banks  = lfbk~banks
          and   bnka~bankl  = lfbk~bankl
          into table tl_lfbk
          where lfbk~lifnr = vlifnr.

        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'BANKL'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          tables
            value_tab       = tl_lfbk
            return_tab      = tl_return_chvl
            dynpfld_mapping = tl_dselcchvl.

        read table tl_return_chvl into wl_return_chvl index 1.
        if sy-subrc = 0 and wl_return_chvl-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'BANKL'.
          ls_modi-value     = wl_return_chvl-fieldval.
          append ls_modi to <itab>.

          er_event_data->m_event_handled = 'X'.
        endif.
      when 'UMSKZ'.
        types : begin of ty_t074u,
                  koart type t074u-koart,
                  umskz type t074u-umskz,
                  ltext type t074t-ltext,
                end of ty_t074u.


        data: wl_return_chvu type  ddshretval,
              wl_dselcchvu   type  dselc,
              tl_074u        type table of ty_t074u,
              wl_074u        type ty_t074u,
              tl_return_chvu type table of ddshretval,
              tl_dselcchvu   type table of dselc,
              tabix_074u     type sy-tabix.

        select koart shbkz ltext
          from t074t
          into table tl_074u
          where spras = 'P'.

        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'UMSKZ'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          tables
            value_tab       = tl_074u
            return_tab      = tl_return_chvu
            dynpfld_mapping = tl_dselcchvu.

        read table tl_return_chvu into wl_return_chvu index 1.
        if sy-subrc = 0 and wl_return_chvu-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'UMSKZ'.
          ls_modi-value     = wl_return_chvu-fieldval.
          append ls_modi to <itab>.

          er_event_data->m_event_handled = 'X'.
        endif.

      when 'BSCHL'.
        types : begin of ty_chave,
                  bschl type tbsl-bschl,
                  koart type tbsl-koart,
                  ltext type tbslt-ltext,
                end of ty_chave,

                begin of ty_texto,
                  bschl type tbsl-bschl,
                  ltext type tbslt-ltext,
                end of ty_texto.

        data: wl_return_chv type  ddshretval,
              wl_dselcchv   type  dselc,
              tl_chave      type table of ty_chave,
              tl_texto      type table of ty_texto,
              wl_texto      type ty_texto,
              wl_chave      type ty_chave,
              tl_return_chv type table of ddshretval,
              tl_dselcchv   type table of dselc,
              tabix_tbsl    type sy-tabix.

        select bschl koart
          from tbsl
          into table tl_chave.

        select bschl ltext
          from tbslt
          into table tl_texto
          for all entries in tl_chave
          where bschl = tl_chave-bschl
          and spras = 'P'.

        sort tl_texto by bschl.

        loop at tl_chave into wl_chave.
          tabix_tbsl = sy-tabix.
          read table tl_texto into wl_texto with key bschl = wl_chave-bschl binary search.
          wl_chave-ltext = wl_texto-ltext.
          modify tl_chave from wl_chave index tabix_tbsl transporting ltext.
        endloop.

        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'BSCHL'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          tables
            value_tab       = tl_chave
            return_tab      = tl_return_chv
            dynpfld_mapping = tl_dselcchv.

        read table tl_return_chv into wl_return_chv index 1.
        if sy-subrc = 0 and wl_return_chv-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'BSCHL'.
          ls_modi-value     = wl_return_chv-fieldval.
          append ls_modi to <itab>.

          er_event_data->m_event_handled = 'X'.
        endif.

      when 'SAKNR'.
        if wg_cadger-question = 'X'.
          read table tg_criaadt into wg_criaadt index es_row_no-row_id.
          if sy-subrc ne 0.
            exit.
          elseif wg_criaadt-bschl is initial.
            exit.
          endif.
          select single *
            from tbsl
            into wl_tbsl
            where bschl = wg_criaadt-bschl.

          if wl_tbsl-koart = 'K'.
            select lifnr name1
              from lfa1
              into table tl_conta.
          elseif wl_tbsl-koart = 'D'.
            select kunnr name1
              from kna1
              into table tl_conta.
          elseif wl_tbsl-koart = 'S'.
            select saknr txt50
              from skat
              into table tl_conta
              where spras = sy-langu
              and ktopl = '0050'.
          endif.
        else.
          select lifnr name1
              from lfa1
              into table tl_conta.
        endif.
        sort tl_conta by saknr.


        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'SAKNR'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          tables
            value_tab       = tl_conta
            return_tab      = tl_return_lg
            dynpfld_mapping = tl_dselclg.

        read table tl_return_lg into wl_return_lg index 1.
        if sy-subrc = 0 and wl_return_lg-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'SAKNR'.
          ls_modi-value     = wl_return_lg-fieldval.
          append ls_modi to <itab>.

          read table tl_conta into wl_conta with key saknr = wl_return_lg-fieldval binary search.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'TXT50'.
          ls_modi-value     = wl_conta-txt50.
          append ls_modi to <itab>.

          er_event_data->m_event_handled = 'X'.
        endif.
      when 'SAKNR_BA'.
        read table tg_criaadt into wg_criaadt index es_row_no-row_id.
        select saknr txt50
          from skat
          into table tl_conta
          where spras = sy-langu
          and ktopl = '0050'.
        sort tl_conta by saknr.

        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'SAKNR'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          tables
            value_tab       = tl_conta
            return_tab      = tl_return_lg
            dynpfld_mapping = tl_dselclg.

        read table tl_return_lg into wl_return_lg index 1.
        if sy-subrc = 0 and wl_return_lg-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'SAKNR_BA'.
          ls_modi-value     = wl_return_lg-fieldval.
          append ls_modi to <itab>.

*          READ TABLE TL_CONTA INTO WL_CONTA WITH KEY SAKNR = WL_RETURN_LG-FIELDVAL BINARY SEARCH.
*          LS_MODI-ROW_ID    = ES_ROW_NO-ROW_ID.
*          LS_MODI-FIELDNAME = 'TXT50'.
*          LS_MODI-VALUE     = WL_CONTA-TXT50.
*          APPEND LS_MODI TO <ITAB>.

          er_event_data->m_event_handled = 'X'.
        endif.
      when 'KOSTL'.
        types : begin of ty_csks,
                  kostl type cskt-kostl,
                  ktext type cskt-ktext,
                end of ty_csks.


        data: wl_return_chvk type  ddshretval,
              wl_dselcchvk   type  dselc,
              tl_csks        type table of ty_csks,
              wl_csks        type ty_csks,
              tl_return_chvk type table of ddshretval,
              tl_dselcchvk   type table of dselc,
              tabix_csks     type sy-tabix.

        select kostl ktext
          from cskt
          into table tl_csks
          where spras = 'P'
          and kokrs = 'MAGI'
          and datbi gt sy-datum.

        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'KOSTL'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          tables
            value_tab       = tl_csks
            return_tab      = tl_return_chvk
            dynpfld_mapping = tl_dselcchvk.

        read table tl_return_chvk into wl_return_chvk index 1.
        if sy-subrc = 0 and wl_return_chvk-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'KOSTL'.
          ls_modi-value     = wl_return_chvk-fieldval.
          append ls_modi to <itab>.

          er_event_data->m_event_handled = 'X'.
        endif.

    endcase.



  endmethod. "on_f4


endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

*ALRS fim

************************************************************************
* D E F I N I T I O N
************************************************************************
class lcl_event_receiver definition.
  public section.
    methods:
      catch_hotspot
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no.

    methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .


    methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    methods:
      on_f4                      for event onf4                 of cl_gui_alv_grid
        importing e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

  private section.
endclass.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
class lcl_event_receiver implementation.

  method on_f4.

    field-symbols: <itab> type lvc_t_modi.
    data: tl_return_tab type table of ddshretval,
          tl_dselc      type table of dselc,
          wl_return_tab type  ddshretval,
          wl_dselc      type  dselc.

    types:
      begin  of ty_tipoo,
        tp_operacao type zfit0043-tp_operacao,
        ds_operacao type zfit0043-ds_operacao,
        status_ctb  type zfit0043-status_ctb,
        liquidar    type zfit0043-liquidar,
      end of ty_tipoo.

    data tl_tipoo type table of ty_tipoo.
    data: ls_modi type lvc_s_modi.

    case e_fieldname.
      when 'OPERA'.

        select tp_operacao ds_operacao status_ctb liquidar
          from zfit0043
          into table tl_tipoo
           where spras = sy-langu.

        sort tl_tipoo by tp_operacao.
        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'TP_OPERACAO'
            dynpprog        = sy-repid                            "'ZFINR018'
            dynpnr          = sy-dynnr
            dynprofield     = 'ZFIT0043-TP_OPERACAO'
            value_org       = 'S'
          tables
            value_tab       = tl_tipoo
            return_tab      = tl_return_tab
            dynpfld_mapping = tl_dselc.


        read table tl_return_tab into wl_return_tab   index 1.
        if sy-subrc = 0 and wl_return_tab-fieldval <> ''.
          assign er_event_data->m_data->* to <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'OPERA'.
          ls_modi-value     = wl_return_tab-fieldval.
          append ls_modi to <itab>.
          er_event_data->m_event_handled = 'X'.
        endif.
    endcase.

  endmethod.

  method on_data_changed.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'AUGBL'.

      read table it_saida into wa_saida index ls_good-row_id.
      if wa_saida-augbl is not initial.
        lv_value = wa_saida-augbl.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'AUGBL'
            i_value     = lv_value.
        message s836(sd) display like 'E' with 'Não altere Doc. Compensação, somente vazios!'(104).
      endif.

    endloop.

    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'VLR_PGTO'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      wa_saida-vlr_pgto = lv_value.
      read table it_saida into wa_saida index ls_good-row_id.
      if wa_saida-vlr_pgto gt wa_saida-dmbe2.
        message s836(sd) display like 'E' with 'Valor de pagamento inválido!'(373).
      endif.

    endloop.

*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'CHECKBOX'.
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
*      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX LS_GOOD-ROW_ID.
*      IF WA_SAIDA-ICON1 = ICON_LOCKED.
*        CLEAR LV_VALUE.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'CHECKBOX'
*            I_VALUE     = LV_VALUE.
*        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não altere Doc. BLOQUEADO'.
*      ENDIF.

*  ENDLOOP.

    loop at er_data_changed->mt_good_cells
                                  into ls_good
                                  where fieldname = 'OPERA'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      select single *
      from zfit0043
      into wa_zfit0043
      where tp_operacao = lv_value
      and   spras       = sy-langu.
      if sy-subrc ne 0.
        clear: lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'OPERA'
            i_value     = lv_value.
        message s836(sd) display like 'E' with 'Operation code not found!'.
      endif.

    endloop.
    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'MOTIVO'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      select single *
      from zfit0040
      into wa_zfit0040
      where motivo = lv_value.
      if sy-subrc ne 0.
        clear: lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'MOTIVO'
            i_value     = lv_value.
        message s836(sd) display like 'E' with 'Código de motivo não foi encontrado!'(105).
      endif.

    endloop.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'FORMA_PG'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      if not 'C_P_A' cs lv_value+0(1).
        clear: lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'FORMA_PG'
            i_value     = lv_value.
        message s836(sd) display like 'E' with 'Forma de pagto deve ser C-Compensação ou P-Pagamento!'(106).
      else.
        if lv_value+0(1) = 'C'.
          move 'C-Compensação/Receber'(107) to lv_value.
          concatenate icon_release 'Compensar INVOICE'(108) into wg_mensagem.
        elseif  lv_value+0(1) = 'A'.
          move 'A-Compensação/Adto'(109) to lv_value.
          concatenate icon_release 'Compensar INVOICE Adto'(110) into wg_mensagem.
        else.
          move 'P-Pagamento'(111) to lv_value.
          concatenate icon_history 'Criar Pagamento'(112) into wg_mensagem.
        endif.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'FORMA_PG'
            i_value     = lv_value.
      endif.

    endloop.

  endmethod.                    "ON_DATA_CHANGED
  method on_data_changed_finished.

**** Método de atualização de dados na Tela
*    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.

  endmethod.                    "on_data_changed_finisheD
  method catch_hotspot.

    read table it_saida into wa_saida index e_row_id-index.
    if sy-subrc = 0.
      if e_column_id =  'ICON0'.
        refresh it_zfit0076.
        clear wg_atach.
        wg_atach-updl = 'X'.
        clear wg_atach-down.
        select *
          from zfit0076
          into table it_zfit0076
          where obj_key = wa_saida-obj_key
          and   belnr   = wa_saida-belnr36
          and   buzei   = wa_saida-buzei36
          order by pos.
        if it_zfit0076[] is not initial.
          wg_atach-down = 'X'.
          clear wg_atach-updl.
        endif.

        call screen 0300 starting at 050 3
                         ending   at 165 13.

      elseif e_column_id = 'BELNR' and wa_saida-belnr is not initial.
        set parameter id 'BLN' field wa_saida-belnr.
        set parameter id 'BUK' field wa_saida-bukrs.
        set parameter id 'GJR' field wa_saida-budat+6(4).
        call transaction 'FB03' and skip first screen.
      elseif e_column_id = 'BELNR_O' and wa_saida-belnr_o is not initial.
        set parameter id 'BLN' field wa_saida-belnr_o.
        set parameter id 'BUK' field wa_saida-bukrs.
        set parameter id 'GJR' field wa_saida-budat+6(4).
        call transaction 'FB03' and skip first screen.
      elseif e_column_id = 'AUGBL' and wa_saida-augbl is not initial.
        set parameter id 'BLN' field wa_saida-augbl.
        set parameter id 'BUK' field wa_saida-bukrs.
        set parameter id 'GJR' field wa_saida-budat+6(4).
        call transaction 'FB03' and skip first screen.
      elseif e_column_id = 'LOTE'  and wa_saida-lote is not initial..
        call screen 200 starting at 20  1
                 ending   at 120 13.
      elseif e_column_id = 'NR_SOLOV'.
        call transaction 'ZSDT0062'.
      elseif e_column_id = 'ICON3'.
        data  wl_zfit0036 type zfit0036.
        refresh: gt_data.

        select single *
             from zfit0036
             into wl_zfit0036
             where obj_key = wa_saida-obj_key
             and   belnr   = wa_saida-belnr36
             and   buzei   = wa_saida-buzei36.

        select single *
        from lfbk
        into @data(w_lfbk)
        where lifnr = @wa_saida-lifnr_z
        and   banks = @wl_zfit0036-banks_1
        and   bankl = @wl_zfit0036-bankl_1
        and   bankn = @wl_zfit0036-bankn_1.

        wa_data-bvtyp = wl_zfit0036-bvtyp.
        wa_data-banks = wl_zfit0036-banks_1.
        wa_data-bankl = wl_zfit0036-bankl_1.
        wa_data-bankn = wl_zfit0036-bankn_1.
        wa_data-bkont = w_lfbk-bkont.
        wa_data-iban  = wl_zfit0036-iban_1.
        wa_data-swift = wl_zfit0036-swift_1.
        wa_data-banka = wl_zfit0036-banka_1.
        append wa_data to gt_data.

        if wl_zfit0036-bvtyp_2 is not initial.
          select single *
          from lfbk
          into @data(w_lfbk2)
          where lifnr = @wa_saida-lifnr_z
          and   banks = @wl_zfit0036-banks_2
          and   bankl = @wl_zfit0036-bankl_2
          and   bankn = @wl_zfit0036-bankn_2.

          clear wa_data.
          wa_data-bvtyp = wl_zfit0036-bvtyp_2.
          wa_data-banks = wl_zfit0036-banks_2.
          wa_data-bankl = wl_zfit0036-bankl_2.
          wa_data-bankn = wl_zfit0036-bankn_2.
          wa_data-bkont = w_lfbk2-bkont.
          wa_data-iban  = wl_zfit0036-iban_2.
          wa_data-swift = wl_zfit0036-swift_2.
          wa_data-banka = wl_zfit0036-banka_2.
          append wa_data to gt_data.
        endif.

        data : lo_table type ref to cl_salv_table.

* Create ALV
        try.
            cl_salv_table=>factory( importing r_salv_table = lo_table
                                     changing t_table      = gt_data ).
          catch cx_salv_msg.
        endtry.

        data: lr_functions type ref to cl_salv_functions_list.

        lr_functions = lo_table->get_functions( ).
        lr_functions->set_all( 'X' ).

        if lo_table is bound.
          lo_table->set_screen_popup(
            start_column = 20
            end_column   = 180
            start_line   = 10
            end_line     = 16 ).
        endif.


* Display ALV grid
        lo_table->display( ).



      endif.
    endif.
  endmethod.                    "CATCH_HOTSPOT



endclass.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

data: event_receiver   type ref to lcl_event_receiver.
data: gs_variant_c type disvariant.
data: gs_variant_bra type disvariant.
data: variante         like disvariant.
data: def_variante     like disvariant.
data: t_set type standard table of setleaf  with header line.
data: t_lay type standard table of setlinet with header line.
data: d_butt1(4). "teste
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.
  parameters    : p_bukrs type zfit0036-bukrs obligatory.
  select-options: p_forn  for zib_contabil-hkont,
                  p_invo  for zfit0036-invoice  ,
                  p_gsber for zib_contabil-gsber,
                  p_data  for sy-datum ,
                  p_datap  for sy-datum modif id a.
selection-screen: end of block b1.

selection-screen: begin of block b2 with frame title text-002.
  parameters:
    r_st_g radiobutton group rad1 user-command act,
    r_st_a radiobutton group rad1 default 'X'.
*            R_ST_B  RADIOBUTTON GROUP RAD1  ,
*            R_INV   LIKE BSID-UMSKZ AS CHECKBOX  DEFAULT ' '.
  selection-screen begin of line.
    parameters:  r_st_b  radiobutton group rad1  .
    selection-screen comment 4(25) text-su1 for field r_st_b.
    parameters: r_inv   like bsid-umskz as checkbox  default ' '.
    selection-screen comment (20) text-su2 for field r_inv.
  selection-screen end of line.

  parameters: r_st_e  radiobutton group rad1,
              r_st_l  radiobutton group rad1,
              r_st_p  radiobutton group rad1,
              r_st_c  radiobutton group rad1,
              r_st_cb radiobutton group rad1,
              r_arq_b radiobutton group rad1.

selection-screen: end of block b2.

selection-screen begin of block b6  with frame title text-000.
  select-options: p_dt_p for sy-datum modif id b,
                  p_lote for zfit0037-lote modif id b.
selection-screen end of block b6.


selection-screen: begin of block b5 with frame title text-000.
  parameter: p_varia type disvariant-variant.
selection-screen: end of block b5.

selection-screen function key 1.

initialization.
  gs_variant_c-report      = sy-repid.
  data  _pos      type i.

*** - US 76596- Inicio - CBRAND
  authority-check object 'ZFI0017_FERIADO'
  id 'USERNAME' field sy-uname.
  if sy-subrc = 0.
    move 'Cadastro Feriados' to sscrfields-functxt_01.
    move 'Cadastro Feriados' to sscrfields-functxt_02.
    move 'CAD_FER'   to sscrfields-ucomm.

  else.
    move '' to sscrfields-functxt_01.
    move '' to sscrfields-functxt_02.
    move '' to sscrfields-ucomm.
  endif.

  select single *
    from usr21
    into @data(_usr21)
    where bname = @sy-uname.
  if sy-subrc = 0.
    select single *
      from adr6
      into @data(_adr6)
      where addrnumber = @_usr21-addrnumber
      and   persnumber = @_usr21-persnumber.
    if sy-subrc = 0.
      if _adr6-smtp_addr is not initial.
        if p_bukrs is initial.
          data(_len) = strlen( _adr6-smtp_addr ).
          _pos = _len - 3.
          if  _adr6-smtp_addr+_pos(3) = '.ch' or
              _adr6-smtp_addr+_pos(3) = '.CH'.  "suiça
            p_bukrs = '0200'.
          endif.
        endif.
      endif.
    endif.


  endif.
**** - US 76596- Fim - CBRAND

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
  data: vg_repid   like sy-repid,
        vg_variant type disvariant.

at selection-screen on value-request for p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  if ( not p_varia is initial ).
    vg_variant-variant = p_varia.

  endif.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = variante
      i_save        = 'A'
    importing
      es_variant    = variante
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

  if ( sy-subrc ne 0 ).
    message s000(z01) with 'Não existe variante'(114).
    stop.
  else.
    move variante-variant to p_varia.
    move variante-variant to gs_variant_c-variant.
  endif.

at selection-screen output.

  "CS2021001160 CAMPO EMPRESA ZFI0017
  select single *
  into @data(w_t001_)
  from t001
  where bukrs = @p_bukrs.

  if sy-subrc ne 0.
    message 'Empresa não cadastrada.' type 'I'.
    set cursor field 'P_BUKRS' .
  endif.
  "CS2021001160 CAMPO EMPRESA ZFI0017

*  if  p_data is initial and r_st_g = '' and r_arq_b = ''.
  if  p_data[] is initial and ( r_st_e = ' ' and r_st_l = ' ' and r_st_p = ' ' ).
    message 'Informe a Data de Lançamento.'(115) type 'I'.
    set cursor field 'P_DATA-LOW' .
  endif.
  if  p_datap[] is initial and ( r_st_e = 'X' or r_st_l = 'X' or r_st_p = 'X' ).
    message 'Informe a Data de Pagamento.'(146) type 'I'.
    set cursor field 'P_DATA-LOW' .
  endif.
  loop at screen.
    if r_st_e = 'X' or r_st_l = 'X' or r_st_p = 'X'.
      if screen-group1 = 'A'.
        screen-active = 1.
      endif.
    else.
      if screen-group1 = 'A'.
        refresh p_datap.
        screen-active = 0.
      endif.
    endif.
    modify screen.

    if r_arq_b  = 'X'.
      if screen-group1 = 'B'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        modify screen.
        continue.

      endif.
    else.
      if screen-group1 = 'B'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        modify screen.
        continue.
      endif.
    endif.
  endloop.

*
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      CLASS         = '0000'
*      SETNR         = 'MAGGI_ZFI0017'
*    TABLES
*      SET_VALUES    = T_LAY
*    EXCEPTIONS
*      SET_NOT_FOUND = 1
*      OTHERS        = 2.

  select *
    from  setleaf
    into table t_set
    where setclass      = '0000'
    and   setname        = 'MAGGI_ZFI0017'.

  select *
    from setlinet
    into table t_lay
    for all entries in t_set
    where setclass   = t_set-setclass
    and subclass     = t_set-subclass
    and setname      = t_set-setname
    and langu        = 'P'
    and lineid       = t_set-lineid.

  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  if r_st_g = 'X'.
    read table t_set with key valfrom = 'R_ST_G'.
  elseif  r_st_a = 'X'.
    read table t_set with key valfrom = 'R_ST_A'.
  elseif r_st_b = 'X'.
    read table t_set with key valfrom = 'R_ST_B'.
  elseif r_st_e = 'X'.
    read table t_set with key valfrom = 'R_ST_E'.
  elseif r_st_l = 'X'.
    read table t_set with key valfrom = 'R_ST_L'.
  elseif r_st_p = 'X'.
    read table t_set with key valfrom = 'R_ST_P'.
  elseif r_st_c = 'X'.
    read table t_set with key valfrom = 'R_ST_C'.
  elseif r_st_cb = 'X'.
    read table t_set with key valfrom = 'R_ST_CB'.
  else.
    clear p_varia.
    move p_varia to gs_variant_c-variant.
  endif.
  if sy-subrc = 0.
    read table t_lay with key  setclass   = t_set-setclass
                               subclass   = t_set-subclass
                               setname    = t_set-setname
                               lineid     = t_set-lineid.
    if sy-subrc = 0.
      p_varia = t_lay-descript.
      move p_varia to gs_variant_c-variant.
    else.
      clear p_varia.
      move p_varia to gs_variant_c-variant.
    endif.
  else.
    clear p_varia.
    move p_varia to gs_variant_c-variant.
  endif.

**** - US 76596- Inicio - CBRAND
at selection-screen.
  case sscrfields-ucomm.
    when'FC01'.
      call transaction 'ZFI0137'.
  endcase.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
start-of-selection.

  "CS2021001160 CAMPO EMPRESA ZFI0017
  select single *
     into @data(_w_t001)
     from t001
     where bukrs = @p_bukrs.

  if sy-subrc eq 0.

    if r_st_g = 'X'.
      clear wg_cadger.
      refresh tg_criaadt.
      refresh tg_conta.
      call screen 9000.
*     CALL SCREEN 9000 STARTING AT 060 2
*             ENDING   AT 170 35.

    elseif r_arq_b = 'X'.

      perform z_rel_banco.
      call screen 9400.

    else.
      if p_datap[] is not initial and p_data[] is initial.
        p_data-sign    = 'I'.
        p_data-option  = 'BT'.            "
        p_data-low     = '20000101'.
        p_data-high    = '21000101'.
        append p_data.
      endif.
      if p_data[] is not initial.
        perform:
                f_seleciona_dados, " Form seleciona dados
                f_saida, " Form de saida
                f_imprime_dados.
      endif.
    endif.
  endif. "CS2021001160 CAMPO EMPRESA ZFI0017

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados .

  data: tabix       type sy-tabix,
        vnro_sol_ov type  zsdt0053-nro_sol_ov,
        vinvoice    type  zfit0036-invoice.

  if r_st_a  = 'X'.
    vstatus = ' '.
  endif.
  if r_st_e = 'X'.
    vstatus = 'A'.
  endif.
  if r_st_l = 'X'.
    vstatus = 'L'.
  endif.
  if r_st_p = 'X'.
    vstatus = 'P'.
  endif.
  if r_st_b = 'X'.
    vstatus = ' '.
  endif.

  if r_st_c = 'X'.
    vstatus = 'T'.
  endif.

  if r_st_cb = 'X'.
    vstatus = 'Z'.
  endif.
*
  if r_st_c = 'X'. " Performance
*    select   obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc  matnr lifnr referencia belnr_adt_c
*      belnr_adt_g usuario user_create data_atual bankl_1 bankn_1 iban_1 swift_1 bvtyp_2 banks_2 bankl_2 bankn_2 iban_2 swift_2 banka_2 desp_tar   "bvtyp banks_1 banka_1
    select *
      from zfit0036
      into  corresponding fields of table it_zfit0036
      where in_performance  eq 'S'
      and   status ne 'C'
      and   bukrs eq p_bukrs
      and   dt_pgto in p_datap
      and   invoice in p_invo
      and   eliminar = ''.

*    select   obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc  matnr lifnr referencia belnr_adt_c
*      belnr_adt_g usuario user_create data_atual bankl_1 bankn_1 iban_1 swift_1 bvtyp_2 banks_2 bankl_2 bankn_2 iban_2 swift_2 banka_2 desp_tar
    select *
     from zfit0036
     appending corresponding fields of table it_zfit0036
     where obj_key like 'P%'
     and   status = 'P'
     and   rg_atualizado = 'S'
     and   bukrs eq p_bukrs
     and   dt_pgto in p_datap
     and   invoice in p_invo
     and   eliminar = ''.
    delete it_zfit0036 where obj_key+0(2) = 'PB%'.
  elseif r_st_cb = 'X'. " Performance Brasil
*    select   obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc  matnr lifnr referencia belnr_adt_c
*      belnr_adt_g usuario user_create data_atual bankl_1 bankn_1 iban_1 swift_1 bvtyp_2 banks_2 bankl_2 bankn_2 iban_2 swift_2 banka_2 desp_tar
    select *
     from zfit0036
     appending corresponding fields of table it_zfit0036
     where obj_key like 'PB%'
     and   status = ' '
     and   bukrs eq p_bukrs
     and   dt_pgto in p_datap
     and   invoice in p_invo
     and   eliminar = ''.
  else.
*    select   obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc  matnr lifnr referencia belnr_adt_c
*      belnr_adt_g usuario user_create data_atual bankl_1 bankn_1 iban_1 swift_1 bvtyp_2 banks_2 bankl_2 bankn_2 iban_2 swift_2 banka_2 desp_tar
    select *
      from zfit0036
      into corresponding fields of table it_zfit0036
      where status  = vstatus
      and  in_performance  in (' ','N')
      and  bukrs eq p_bukrs
      and  dt_pgto in p_datap
      and  invoice in p_invo

      and  eliminar = ''.

    if vstatus = ' '. " Adiciona os Bloqueados
*      select  obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc  matnr lifnr referencia belnr_adt_c
*        belnr_adt_g usuario user_create data_atual bankl_1 bankn_1 iban_1 swift_1 bvtyp_2 banks_2 bankl_2 bankn_2 iban_2 swift_2 banka_2  desp_tar
      select *
       from zfit0036
       appending corresponding fields of table it_zfit0036
       where status  = 'B'
       and  in_performance  in (' ','N')
       and  bukrs eq p_bukrs
       and  dt_pgto in p_datap
       and  invoice in p_invo
       and  eliminar = ''.
    endif.
    if vstatus = 'P'. " Adiciona os compensados também
      select *
        from zfit0036
        appending corresponding fields of table it_zfit0036
        where status  = 'C'
        and  in_performance  in (' ','N')
        and  bukrs eq p_bukrs
        and  dt_pgto in p_datap
        and  invoice in p_invo
        and  eliminar = ''.
    endif.
  endif.

  delete it_zfit0036 where obj_key+0(2) = 'AC' and data_atual  not in p_data.
  delete it_zfit0036 where obj_key+0(2) = 'AC' and bukrs  ne p_bukrs.
  if r_st_b = 'X'.
    delete it_zfit0036 where obj_key+0(1) = 'P' . "elimina Adto performance
  endif.

  " Usuários que podem mudar o Status
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'MAGGI_ZFI0017_MS'
    tables
      set_values    = t_usermd
    exceptions
      set_not_found = 1
      others        = 2.
  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  sort t_usermd by from.


  " fornecedores Intercompany tratados com terceiro
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'MAGGI_ZFI0017_F'
    tables
      set_values    = t_forint
    exceptions
      set_not_found = 1
      others        = 2.
  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  sort t_forint by from.

  if r_st_b = 'X'.
    "insere também registros que faltam Taxa
*    SELECT   OBJ_KEY BELNR BUZEI BUKRS INVOICE NAVIO LOTE DT_PGTO TX_CAMBIO MOEDA_PGTO VLR_PGTO HBKID OBSERVACAO STATUS FORMA_PG MOTIVO RG_ATUALIZADO BVTYP OPERACAO
    select * "obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc  matnr lifnr referencia belnr_adt_c
*    usuario user_create
    from zfit0036
    into corresponding fields of  table it_zfit0036_aux
    where rg_atualizado  = 'T'
    and  invoice in p_invo
    and  dt_pgto in p_datap.
    "AND  STATUS  NE 'P'.
    loop at it_zfit0036_aux into wa_zfit0036.
      append wa_zfit0036 to it_zfit0036.
    endloop.
  endif.

  if r_st_b = 'X'.
    vstatus = 'B'.
  endif.

  check it_zfit0036[] is not initial.
  loop at it_zfit0036 into wa_zfit0036.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zfit0036-matnr
      importing
        output = wa_zfit0036-matnr.
    modify it_zfit0036 from wa_zfit0036 index sy-tabix transporting matnr.
  endloop.

*** PBI - 74866 - Inicio - CBRAND
  select *
    from zfit0037
    into table it_zfit0037_arq
    for all entries in it_zfit0036
    where lote = it_zfit0036-lote
    and obj_key = it_zfit0036-obj_key.

*** PBI - 74866 - Fim - CBRAND

  select matnr maktx
    from makt
    into table it_makt
    for all entries in it_zfit0036
    where matnr = it_zfit0036-matnr
    and spras = 'P'.

  select id_tipo_invoice  descricao pgto
    from zfit0047
    into table it_zfit0047
    for all entries in it_zfit0036
    where id_tipo_invoice = it_zfit0036-id_tipo_invoice.

  select  bukrs bukrs_ate tipo waers nivel aprovador valor_de valor_ate
   from zinv_aprovador
   into table it_zinv_aprovador
   where bukrs     le p_bukrs
   and   bukrs_ate ge p_bukrs.

  select bukrs lote nivel aprovador valor_de valor_ate data_atual hora_atual usuario
    from zinv_lotes_aprov
    into table it_zinv_lotes_aprov
    for all entries in it_zfit0036
    where bukrs eq it_zfit0036-bukrs
    and   lote  eq it_zfit0036-lote.


  loop at it_zfit0036 into wa_zfit0036.
    if wa_zfit0036-belnr is not initial.
      wa_zfit0036-belnr36 = wa_zfit0036-belnr.
      wa_zfit0036-buzei36 = wa_zfit0036-buzei.
      wa_zfit0036-belnr   = ''.
      modify it_zfit0036 from  wa_zfit0036 index sy-tabix transporting belnr36 buzei36 belnr.
    endif.
    if 'P_A' cs wa_zfit0036-obj_key+0(1) .
      if  wa_zfit0036-belnr36 is not initial.
        wa_zfit0036-belnr = wa_zfit0036-belnr36.
      else.
        if  wa_zfit0036-obj_key+0(2) = 'PB'.
          wa_zfit0036-belnr = wa_zfit0036-obj_key+2(10).
          vinvoice    = wa_zfit0036-invoice.
          condense vinvoice no-gaps.
          vnro_sol_ov = vinvoice.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = vnro_sol_ov
            importing
              output = vnro_sol_ov.
          wa_zfit0036-nro_sol_ov = vnro_sol_ov.
        elseif wa_zfit0036-obj_key+0(2) eq 'AC'  and wa_zfit0036-operacao ne '08'.
          wa_zfit0036-lifnr = wa_zfit0036-obj_key+13(6).
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_zfit0036-lifnr
            importing
              output = wa_zfit0036-lifnr.
        elseif  wa_zfit0036-obj_key+0(2) ne 'AC'. "Não é contábil
          wa_zfit0036-belnr = wa_zfit0036-obj_key+1(10).
        endif.
      endif.
      modify it_zfit0036 from  wa_zfit0036 index sy-tabix transporting belnr nro_sol_ov lifnr.
    endif.
  endloop.


  " Adiantamento doc esta contido no obj_key
  it_zfit0036_adt[] = it_zfit0036[].
  delete it_zfit0036_adt[] where obj_key+0(1) ne 'P' and obj_key+0(1) ne 'A'.
  loop at it_zfit0036_adt into wa_zfit0036.
    if  wa_zfit0036-belnr36 is not initial.
      wa_zfit0036-belnr = wa_zfit0036-belnr36.
    else.
      if  wa_zfit0036-obj_key+0(2) = 'PB'.
        wa_zfit0036-belnr      = wa_zfit0036-obj_key+2(10).
        vinvoice    = wa_zfit0036-invoice.
        condense vinvoice no-gaps.
        vnro_sol_ov = vinvoice.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vnro_sol_ov
          importing
            output = vnro_sol_ov.
        wa_zfit0036-nro_sol_ov = vnro_sol_ov.
      elseif  wa_zfit0036-obj_key+0(2) ne 'AC'. "Não é contábil
        wa_zfit0036-belnr = wa_zfit0036-obj_key+1(10).
      endif.
    endif.
    modify it_zfit0036_adt from  wa_zfit0036 index sy-tabix transporting belnr nro_sol_ov .
  endloop.

  if it_zfit0036_adt[] is not initial.
    " Alrs 20.08.13
    select *
      from bsis
      into table it_bsis
      for all entries in it_zfit0036_adt
              where bukrs eq it_zfit0036_adt-bukrs
        and   belnr eq it_zfit0036_adt-belnr.

    if r_st_cb eq 'X'. " performance Brasil.
      "Busca Adiantamento
      select   belnr bukrs gjahr augbl augdt dmbe2 kunnr
        from bsad
        into table it_bsad_brasil
        for all entries in it_zfit0036_adt
        where bukrs eq it_zfit0036_adt-bukrs
        and   belnr eq it_zfit0036_adt-belnr.

      if it_bsad_brasil[] is not initial.
        select  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei waers budat blart augbl augdt umskz
        from bsid
        into table it_bsid_braadt
         for all entries in it_bsad_brasil
          where bukrs eq it_bsad_brasil-bukrs
          and   belnr eq it_bsad_brasil-augbl.

        select kunnr name1
          from kna1
          into table it_kna1
          for all entries in it_bsad_brasil
          where kunnr eq it_bsad_brasil-kunnr .
      endif.

      " Busca Nota Fiscal
      select nro_sol_ov vbeln
       from zsdt0053
        into table it_zsdt0053
      for all entries in it_zfit0036_adt
      where nro_sol_ov eq it_zfit0036_adt-nro_sol_ov.

      if it_zsdt0053[] is not initial.
        select  vbeln vbelv erdat
          from vbfa
          into table it_vbfa_brasil
          for all entries in it_zsdt0053
          where vbelv	=	it_zsdt0053-vbeln
          and   vbtyp_n	=	'M'
          and   vbtyp_v	=	'C'.

        if it_vbfa_brasil[] is not initial.
          loop at it_vbfa_brasil into wa_vbfa.
            wa_vbfa-gjahr = wa_vbfa-erdat+0(4).
            wa_vbfa-awkey = wa_vbfa-vbeln.
            "CONCATENATE '00' wa_vbfa-vbeln  INTO wa_vbfa-awkey.
            modify it_vbfa_brasil from wa_vbfa index sy-tabix transporting gjahr awkey.
          endloop.
          select   bukrs gjahr awkey belnr blart budat bldat xblnr waers kursf
          from bkpf
          into table it_bkpf_brasil
          for all entries in it_vbfa_brasil
          where bukrs eq  p_bukrs
          and   gjahr eq  it_vbfa_brasil-gjahr
          and   awkey eq  it_vbfa_brasil-awkey.

          if it_bkpf_brasil[] is not initial.
            select  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei waers budat blart augbl augdt
             from bsid
             into table it_bsid_branfi
              for all entries in it_bkpf_brasil
               where bukrs eq it_bkpf_brasil-bukrs
               and   belnr eq it_bkpf_brasil-belnr
               and   gjahr eq it_bkpf_brasil-gjahr .
          endif.
        endif.

      endif.

    else.
      if r_st_b ne 'X'.
        select bsak~bukrs bsak~lifnr bsak~belnr bsak~augdt bsak~augbl bsak~dmbtr bsak~wrbtr bsak~budat
         from bsak
         into table it_bsak_adt
         for all entries in it_zfit0036_adt
         where bsak~bukrs eq it_zfit0036_adt-bukrs
         and   bsak~belnr eq it_zfit0036_adt-belnr.
      else.
        select bsak~bukrs bsak~lifnr bsak~belnr bsak~augdt bsak~augbl bsak~dmbtr bsak~wrbtr bsak~budat
        from bsak
        inner join lfa1 on lfa1~lifnr eq bsak~lifnr
        and lfa1~ktokk eq 'ZFIC'
        into table it_bsak_adt
        for all entries in it_zfit0036_adt
        where bsak~bukrs eq it_zfit0036_adt-bukrs
        and   bsak~belnr eq it_zfit0036_adt-belnr.
      endif.

      if it_bsak_adt[] is not initial.
        refresh it_lfa1_aux.
        select lifnr name1 stras regio land1 ktokk lnrza ort01
        from lfa1
        into table it_lfa1_aux
        for all entries in it_bsak_adt
        where lifnr = it_bsak_adt-lifnr.
      endif.
      loop at it_lfa1_aux into wa_lfa1.
        append wa_lfa1 to it_lfa1.
      endloop.

      if r_st_b ne 'X'.
        select * "bsik~bukrs bsik~lifnr bsik~belnr bsik~dmbtr bsik~wrbtr bsik~budat bsik~buzei bsik~gsber bsik~shkzg
         from bsik
         into corresponding fields of table it_bsik_adt
         for all entries in it_zfit0036_adt
         where bsik~bukrs eq it_zfit0036_adt-bukrs
         and   bsik~belnr eq it_zfit0036_adt-belnr.
      else.
        select * "bsik~bukrs bsik~lifnr bsik~belnr bsik~dmbtr bsik~wrbtr bsik~budat bsik~buzei bsik~gsber bsik~shkzg
        from bsik
        inner join lfa1 on lfa1~lifnr eq bsik~lifnr
        and lfa1~ktokk eq 'ZFIC'
        into corresponding fields of table it_bsik_adt
        for all entries in it_zfit0036_adt
        where bsik~bukrs eq it_zfit0036_adt-bukrs
        and   bsik~belnr eq it_zfit0036_adt-belnr.
      endif.

      if it_bsik_adt[] is not initial.
        refresh it_lfa1_aux.
        select lifnr name1 stras regio land1 ktokk lnrza ort01
        from lfa1
        into table it_lfa1_aux
        for all entries in it_bsik_adt
        where lifnr = it_bsik_adt-lifnr.
      endif.
      loop at it_lfa1_aux into wa_lfa1.
        append wa_lfa1 to it_lfa1.
      endloop.

    endif.
  endif.

  " 05.07.2013 mudei para fora do "IF".
  select *
    from zib_contabil
    into table it_zib_contabil
    for all entries in it_zfit0036
    where obj_key eq it_zfit0036-obj_key
    and   gsber   in p_gsber
    and   bschl   in ('31','21','29','39')
    and   rg_atualizado eq  'S'
    and   hkont in p_forn
    and   bukrs eq p_bukrs.

  " Obtem valores documento compensado para substituição no relatório 12.07.13
  select * "bukrs lifnr belnr dmbtr wrbtr budat buzei gsber shkzg
     from bsik
     into corresponding fields of table it_bsik_36
     for all entries in it_zfit0036
     where bukrs eq it_zfit0036-bukrs
     and   belnr eq it_zfit0036-belnr36.

  select bukrs lifnr belnr augdt augbl dmbtr wrbtr budat buzei
    from bsak
    into table it_bsak_36
    for all entries in it_zfit0036
    where bukrs eq it_zfit0036-bukrs
    and   belnr eq it_zfit0036-belnr36.

  if not it_zib_contabil[] is initial.
    refresh it_lfa1_aux.
    select lifnr name1 stras regio land1 ktokk lnrza ort01
      from lfa1
      into table it_lfa1_aux
      for all entries in it_zib_contabil
      where lifnr = it_zib_contabil-hkont.

    loop at it_lfa1_aux into wa_lfa1.
      append wa_lfa1 to it_lfa1.
    endloop.
  endif.

  sort it_lfa1 by lifnr.
  loop at it_zib_contabil into wa_zib_contabil.
    tabix = sy-tabix.
    concatenate wa_zib_contabil-budat+6(4) wa_zib_contabil-budat+3(2) wa_zib_contabil-budat+0(2) into  wa_zib_contabil-data .
    if r_st_b = 'X'.
      read table it_lfa1 into wa_lfa1 with key lifnr = wa_zib_contabil-hkont binary search.
      if sy-subrc = 0.
        if wa_lfa1-ktokk ne 'ZFIC'.
          wa_zib_contabil-del = 'X'.
          modify it_zib_contabil from wa_zib_contabil index tabix  transporting del.
        endif.
      endif.
    elseif r_st_a = 'X'.
      read table it_lfa1 into wa_lfa1 with key lifnr = wa_zib_contabil-hkont binary search.
      if sy-subrc = 0.
        if wa_lfa1-ktokk eq 'ZFIC'.
          read table t_forint with key from = wa_zib_contabil-hkont.
          if sy-subrc ne 0.
            wa_zib_contabil-del = 'X'.
          else.
            wa_zib_contabil-del = ''.
          endif.
          modify it_zib_contabil from wa_zib_contabil index tabix  transporting del.
        endif.
      endif.
    endif.
    modify it_zib_contabil from wa_zib_contabil index tabix  transporting data.
  endloop.
  "DELETE IT_ZIB_CONTABIL WHERE DATA  NOT IN P_DATA. Não deletar aqui, alterado para dentro F-SAIDA
  delete it_zib_contabil where del = 'X'.

  if not it_zib_contabil[] is initial.
    refresh it_lfa1_aux.
    select lifnr name1 stras regio land1 ktokk lnrza ort01
      from lfa1
      into table it_lfa1_aux
      for all entries in it_zib_contabil
      where lifnr = it_zib_contabil-hkont.

    loop at it_lfa1_aux into wa_lfa1.
      append wa_lfa1 to it_lfa1.
    endloop.

    select zib_contabil_chv~obj_key zib_contabil_chv~belnr zib_contabil_chv~bukrs zib_contabil~hkont
      from zib_contabil_chv
      inner join zib_contabil
      on  zib_contabil~obj_key eq zib_contabil_chv~obj_key
      and zib_contabil~bschl   in ('31','21','29','39')
      into table  it_zib_contabil_chv
      for all entries in it_zib_contabil
      where zib_contabil_chv~obj_key eq it_zib_contabil-obj_key.

    if it_zib_contabil_chv[] is not initial.

      select bukrs lifnr belnr augdt augbl dmbtr wrbtr
        from bsak
        into table it_bsak
        for all entries in it_zib_contabil_chv
        where bukrs eq it_zib_contabil_chv-bukrs
        and   lifnr eq it_zib_contabil_chv-hkont
        and   belnr eq it_zib_contabil_chv-belnr.

      select * "bukrs lifnr belnr dmbtr wrbtr budat buzei gsber shkzg
      from bsik
      into corresponding fields of table it_bsik
      for all entries in it_zib_contabil_chv
      where bukrs eq it_zib_contabil_chv-bukrs
      and   lifnr eq it_zib_contabil_chv-hkont
      and   belnr eq it_zib_contabil_chv-belnr.

    endif.
  endif.

  "fornecedores adiantamento tipo AC
  select lifnr name1 stras regio land1 ktokk
      from lfa1
      appending table it_lfa1
      for all entries in it_zfit0036
      where lifnr = it_zfit0036-lifnr.


  if it_lfa1[] is not initial.
    it_lfa1_aux[] = it_lfa1[].
    delete it_lfa1_aux where lnrza is not  initial.

    if it_lfa1_aux[] is not initial.
      select lifnr banks bankl bankn bvtyp
        from lfbk
        into table it_lfbk
        for all entries in it_lfa1_aux
        where lifnr = it_lfa1_aux-lifnr.
    endif.

    select lifnr banks bankl bankn bvtyp
      from lfbk
      appending table it_lfbk
      for all entries in it_lfa1
      where lifnr = it_lfa1-lnrza. " alternativa

    it_lfa1_aux[] = it_lfa1[].
    delete it_lfa1_aux where lnrza is  initial.
    select lifnr name1 stras regio land1 ktokk lnrza ort01
      from lfa1
      appending table it_lfa1
      for all entries in it_lfa1_aux
      where lifnr = it_lfa1_aux-lnrza.

    sort it_lfa1 by lifnr.
    delete adjacent duplicates from it_lfa1 comparing lifnr.

    delete   it_lfbk where lifnr is initial.
    delete   it_lfbk where bvtyp+3(1) ne '1' and bvtyp+3(1) ne '2'.
    sort it_lfbk by lifnr banks bankl bankn bvtyp.
    delete adjacent duplicates from it_lfbk comparing all fields.
    sort it_lfa1 by lifnr.
    if not it_lfbk[] is initial.
      select bankl swift stras banka provz banks
        from bnka
        into table it_bnka
        for all entries in it_lfbk
        where bankl = it_lfbk-bankl.

      select banks bankl tabkey iban
        from tiban
        into table it_tiban
        for all entries in it_lfbk
        where banks   eq it_lfbk-banks
        and bankl     eq it_lfbk-bankl.
    endif.
  endif.


endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_saida .
  sort: it_zib_contabil      by obj_key,
        it_zib_contabil_chv  by obj_key,
        it_bsis              by bukrs belnr,
        it_bsad_brasil       by bukrs belnr,
        it_bsid_braadt       by bukrs belnr,
        it_bsak              by bukrs lifnr belnr,
        it_bsik              by bukrs lifnr belnr,
        it_bsak_adt          by bukrs belnr,
        it_bsik_adt          by bukrs belnr,
        it_bsik_36           by bukrs belnr buzei,
        it_bsak_36           by bukrs belnr buzei,
        it_lfa1              by lifnr,
        it_lfbk              by lifnr bvtyp,
        it_bnka              by bankl,
        it_tiban             by banks bankl,
        it_zsdt0053          by nro_sol_ov,
        it_vbfa_brasil       by vbelv,
        it_bkpf_brasil       by awkey gjahr,
        it_bsid_branfi       by bukrs belnr gjahr,
        it_kna1              by kunnr,
        it_makt              by matnr,
        it_zfit0047          by id_tipo_invoice,
        it_zfit0037_arq      by lote laufi descending.

  delete adjacent duplicates from it_zib_contabil comparing  obj_key.

  data: v_num(10)        type c,
        vflag(1),
        vflag_p(1),
        vqtde            type i,
        vdata            type sy-datum,
        vbvtyp           type lfbk-bvtyp,
        vlifnr           type lfa1-lifnr,
        vaugbl           type bsid_view-augbl,
        vid_tipo_invoice type zfit0036-id_tipo_invoice.


* Ini - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual #128356 - RJF
  if it_zfit0036[] is not initial.
    select *
        from zfit0036
        into table it_zfit0036_det
      for all entries in it_zfit0036
        where obj_key = it_zfit0036-obj_key
        and   belnr   = it_zfit0036-belnr36
        and   buzei   = it_zfit0036-buzei36.
  endif.
* Fim - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual #128356 - RJF

  loop at it_zfit0036 into wa_zfit0036.
    wa_saida-icon0    =  icon_attachment.
    if wa_zfit0036-rg_atualizado = 'T'.
      wa_saida-line_color = 'C610'.
    endif.
    wa_saida-obj_key = wa_zfit0036-obj_key.
    wa_saida-belnr36 = wa_zfit0036-belnr36.
    wa_saida-buzei36 = wa_zfit0036-buzei36.
    wa_saida-belnr_adt_c = wa_zfit0036-belnr_adt_c.


    read table it_zfit0036_det into data(wa_zfit36_det) with key obj_key = wa_zfit0036-obj_key.
    if sy-subrc is initial.
* Ini - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual
      wa_saida-bankl_1 = wa_zfit36_det-bankl_1.
      wa_saida-bankn_1 = wa_zfit36_det-bankn_1.
      wa_saida-iban_1  = wa_zfit36_det-iban_1.
      wa_saida-swift_1 = wa_zfit36_det-swift_1.
      wa_saida-banka_1 = wa_zfit36_det-banka_1.
      wa_saida-bvtyp_2 = wa_zfit36_det-bvtyp_2.
      wa_saida-banks_2 = wa_zfit36_det-banks_2.
      wa_saida-bankl_2 = wa_zfit36_det-bankl_2.
      wa_saida-bankn_2 = wa_zfit36_det-bankn_2.
      wa_saida-iban_2  = wa_zfit36_det-iban_2.
      wa_saida-swift_2 = wa_zfit36_det-swift_2.
      wa_saida-banka_2 = wa_zfit36_det-banka_2.
* Fim - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual
    endif.

*** PBI - 74866 - Inicio - CBRAND
    read table it_zfit0037_arq into wa_zfit0037_arq with key lote = wa_zfit0036-lote  obj_key = wa_zfit0036-obj_key binary search.
    if sy-subrc = 0.
      wa_saida-usuario_arq  =  wa_zfit0037_arq-usuario_arq.
      wa_saida-nome_arquivo =  wa_zfit0037_arq-nome_arquivo.
    endif.
*** PBI - 74866 - Fim - CBRAND

    read table it_zfit0047 into wa_zfit0047 with key id_tipo_invoice = wa_zfit0036-id_tipo_invoice binary search.
    if sy-subrc = 0.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_zfit0036-id_tipo_invoice
        importing
          output = vid_tipo_invoice.
      concatenate vid_tipo_invoice '-' wa_zfit0047-descricao into  wa_saida-id_tipo_invoice.
      if wa_zfit0047-pgto = 'S' and r_st_b = 'X'.
        if r_inv = 'X'.
          wa_saida-line_color = 'C310'.
        else.
          continue.
        endif.
      endif.
    else.
      wa_saida-id_tipo_invoice = ''.
    endif.

    if wa_zfit0036-banka_1 is not initial.
      wa_saida-icon3   =   icon_display.
    else.
      wa_saida-icon3   =   icon_message_question_small.
    endif.


    if wa_zfit0036-status ne 'P'.
      wa_saida-icon1    =   icon_message_warning.
    else.
      wa_saida-icon1    =   icon_system_okay.
    endif.
    if 'P_L' cs wa_zfit0036-status.
      wa_saida-icon2    =   icon_release.
    else.
      wa_saida-icon2    =   icon_initial.
    endif.

    if wa_zfit0036-status eq 'L' and wa_zfit0036-rg_atualizado = 'S'.
      wa_saida-icon1 = icon_activity.
    endif.

    if wa_zfit0036-status eq 'B' .
      wa_saida-icon1 = icon_locked.
    endif.
    wa_saida-invoice        = wa_zfit0036-invoice.
    wa_saida-moeda_pgto     = wa_zfit0036-moeda_pgto.
    wa_saida-waers          = wa_saida-moeda_pgto.
    wa_saida-dt_pgto        = wa_zfit0036-dt_pgto.
    wa_saida-augdt          = wa_saida-dt_pgto.
    wa_saida-vlr_pgto       = wa_zfit0036-vlr_pgto.
    wa_saida-status         = wa_zfit0036-status.
    wa_saida-rg_atualizado  = wa_zfit0036-rg_atualizado.
    wa_saida-navio          = wa_zfit0036-navio.
    wa_saida-lote           = wa_zfit0036-lote.
    wa_saida-lote_cp        = wa_zfit0036-lote_cp.
    wa_saida-hbkid          = wa_zfit0036-hbkid.
    wa_saida-banks_1        = wa_zfit0036-banks_1. "US - 76596 - CSB
    wa_saida-observacao     = wa_zfit0036-observacao.
    wa_saida-referencia     = wa_zfit0036-referencia.
    wa_saida-opera          = wa_zfit0036-operacao.
    wa_saida-desp_tar       = wa_zfit0036-desp_tar.
    if  wa_zfit0036-status is not initial.
      refresh: style.
      clear: wa_style.
      wa_style-fieldname = 'OPERA'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
      delete wa_saida-style where fieldname eq 'OPERA'.
      insert  wa_style into table style .
      wa_saida-style[] = style[].
    endif.

    if vstatus = 'P' and wa_zfit0036-status = 'C'.
      wa_saida-forma_pg   = 'C-Compensação'(116).
    elseif wa_zfit0036-forma_pg = 'P'.
      wa_saida-forma_pg   = 'P-Pagamento'(117).
    elseif wa_zfit0036-forma_pg = 'C'.
      wa_saida-forma_pg   = 'C-Compensação'(116).
    else.
      clear wa_saida-forma_pg.
    endif.
    wa_saida-motivo     = wa_zfit0036-motivo.
    vflag = ''.
    vflag_p = ''.
    clear: wa_saida-matnr, wa_saida-matnr.
    if wa_zfit0036-matnr is not initial.
      wa_saida-matnr = wa_zfit0036-matnr.
      read table it_makt into wa_makt with key matnr = wa_zfit0036-matnr binary search.
      if sy-subrc = 0.
        wa_saida-maktx = wa_makt-maktx.
      endif.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_saida-matnr
        importing
          output = wa_saida-matnr.
    endif.


    wa_saida-invoice_terc = wa_zfit0036-invoice_terc.
*** PBI - 74866 - Inicio - CBRAND
    wa_saida-user_create = wa_zfit0036-user_create.
*** PBI - 74866 - Fim - CBRAND

    if wa_zfit0036-obj_key+0(2) = 'AC'. "Adiantamento sem lançamento contábil.
      wa_saida-augdt = wa_zfit0036-dt_pgto.
*      CONCATENATE wa_zfit0036-dt_pgto+6(2) '.' wa_zfit0036-dt_pgto+4(2) '.' wa_zfit0036-dt_pgto+0(4) INTO wa_saida-budat.
      concatenate wa_zfit0036-data_atual+6(2) '.' wa_zfit0036-data_atual+4(2) '.' wa_zfit0036-data_atual+0(4) into wa_saida-budat.
      wa_saida-hkont    = wa_zfit0036-lifnr.
      wa_saida-lifnr_z  = wa_zfit0036-lifnr.
      v_num = wa_zfit0036-lifnr.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = v_num
        importing
          output = v_num.
      wa_saida-lifnr = v_num.
      read table it_lfa1 into wa_lfa1 with key lifnr = wa_zfit0036-lifnr binary search.
      if sy-subrc = 0.
        wa_saida-name1    = wa_lfa1-name1.
        wa_saida-lnrza    = wa_lfa1-lnrza.
      else.
        clear: wa_saida-name1, wa_saida-lnrza .
      endif.
      wa_saida-tipo     = wa_zfit0036-invoice.
      wa_saida-vlr_pgto = wa_zfit0036-vlr_pgto.
      wa_saida-dmbtr    = 0.
      wa_saida-dmbe2    = 0.
      if wa_zfit0036-moeda_pgto ne 'BRL'.
        wa_saida-dmbe2    = wa_zfit0036-vlr_pgto.
      else.
        wa_saida-dmbtr    = wa_zfit0036-vlr_pgto.
      endif.
      wa_saida-bvtyp    = wa_zfit0036-bvtyp.
      vflag_p = 'X'.
    endif.

    wa_saida-bukrs    = wa_zfit0036-bukrs.

    if 'P_A' cs wa_zfit0036-obj_key+0(1) and not 'PB_AC' cs wa_zfit0036-obj_key+0(2).
      clear wa_saida-invoice .
      wa_saida-bukrs    = wa_zfit0036-bukrs.
      wa_saida-nr_solov = wa_zfit0036-invoice.
      if wa_zfit0036-obj_key+0(1) = 'A'.
        wa_saida-tipo = wa_zfit0036-invoice.
      else.
        wa_saida-tipo = 'Adto Performance'(117).
      endif.

      if wa_saida-moeda_pgto is not initial.
        wa_saida-waers = wa_saida-moeda_pgto.
      endif.
      read table it_bsak_adt into wa_bsak with key  bukrs = wa_zfit0036-bukrs
                                                    belnr = wa_zfit0036-belnr binary search.
      if sy-subrc = 0.
        concatenate wa_bsak-augdt+6(2) '.' wa_bsak-augdt+4(2) '.' wa_bsak-augdt+0(4) into wa_saida-budat.
        wa_saida-augbl    = wa_bsak-augbl.
        wa_saida-augdt    = wa_bsak-augdt.
        wa_saida-dmbtr    = wa_bsak-dmbtr.
        wa_saida-dmbe2    = wa_bsak-dmbe2.
        wa_saida-hkont    = wa_bsak-lifnr.
        wa_saida-lifnr_z  = wa_bsak-lifnr.
        v_num = wa_bsak-lifnr.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = v_num
          importing
            output = v_num.
        wa_saida-lifnr = v_num.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_bsak-lifnr binary search.
        if sy-subrc = 0.
          wa_saida-name1    = wa_lfa1-name1.
          wa_saida-lnrza    = wa_lfa1-lnrza.
        else.
          clear: wa_saida-name1,wa_saida-lnrza .
        endif.
        vflag_p = 'X'.
      else.
        vflag_p = ' '.
        clear:  wa_saida-augbl, wa_saida-augdt,  wa_saida-dmbtr, wa_saida-dmbe2.
        wa_saida-augdt    = wa_saida-dt_pgto.
      endif.
      read table it_bsik_adt into wa_bsik with key  bukrs = wa_zfit0036-bukrs
                                                    belnr = wa_zfit0036-belnr binary search.
      if sy-subrc = 0.
        if wa_bsik-budat lt p_data-low or wa_bsik-budat gt p_data-high.
          continue.
        endif.
        vflag_p = 'X'.
        concatenate wa_bsik-budat+6(2) '.' wa_bsik-budat+4(2) '.' wa_bsik-budat+0(4) into wa_saida-budat.
        wa_saida-belnr    = wa_bsik-belnr.
        wa_saida-buzei    = wa_bsik-buzei.
        wa_saida-zfbdt    = wa_bsik-zfbdt.

        wa_saida-dmbtr    = wa_bsik-dmbtr.
        wa_saida-dmbe2    = wa_bsik-wrbtr.
        if ( r_st_c = 'X' or r_st_cb = 'X' ).
          if wa_bsik-shkzg = 'H'.
            wa_saida-dmbtr    = wa_saida-dmbtr * -1.
            wa_saida-dmbe2    = wa_saida-dmbe2 * -1.
          endif.
        endif.
        wa_saida-gsber    = wa_bsik-gsber.
        wa_saida-hkont    = wa_bsik-lifnr.
        wa_saida-lifnr_z  = wa_bsik-lifnr.
        v_num = wa_bsik-lifnr.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = v_num
          importing
            output = v_num.
        wa_saida-lifnr = v_num.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_bsik-lifnr binary search.
        if sy-subrc = 0.
          wa_saida-name1    = wa_lfa1-name1.
          wa_saida-lnrza    = wa_lfa1-lnrza.
        else.
          clear wa_saida-name1 .
        endif.
      else.
        wa_saida-belnr    = wa_zfit0036-obj_key+1(10).
        "alrs 20.08.13
        read table it_bsis into wa_bsis with key  bukrs = wa_zfit0036-bukrs
                                                  belnr = wa_zfit0036-belnr binary search.
        if sy-subrc = 0.
          if wa_bsis-budat lt p_data-low or wa_bsik-budat gt p_data-high.
            continue.
          endif.
          vflag_p = 'X'.
          concatenate wa_bsis-budat+6(2) '.' wa_bsis-budat+4(2) '.' wa_bsis-budat+0(4) into wa_saida-budat.
          wa_saida-belnr    = wa_bsis-belnr.
          wa_saida-dmbtr    = wa_bsis-dmbtr.
          wa_saida-dmbe2    = wa_bsis-dmbe2.
          wa_saida-gsber    = wa_bsis-gsber.
          wa_saida-hkont    = wa_zfit0036-lifnr.
          wa_saida-lifnr_z  = wa_zfit0036-lifnr.
          v_num = wa_zfit0036-lifnr.
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = v_num
            importing
              output = v_num.
          wa_saida-lifnr = v_num.
          read table it_lfa1 into wa_lfa1 with key lifnr = wa_zfit0036-lifnr binary search.
          if sy-subrc = 0.
            wa_saida-name1    = wa_lfa1-name1.
            wa_saida-lnrza    = wa_lfa1-lnrza.
          else.
            clear wa_saida-name1 .
          endif.
        endif.

      endif.

      if wa_zfit0036-obj_key+0(1) = 'A'.
        wa_saida-dmbe2 = wa_zfit0036-vlr_pgto. " Moeda externa será a mesma informada no adiantamento e valor tambem
      endif.

      if wa_zfit0036-bvtyp is initial. "aberto
*        VQTDE = 0.
*        CLEAR: VBVTYP, WA_LFBK-BVTYP.
*        IF WA_SAIDA-LNRZA IS NOT INITIAL.
*          VLIFNR = WA_SAIDA-LNRZA.
*        ELSE.
*          VLIFNR = WA_SAIDA-LIFNR_Z.
*        ENDIF.
*        LOOP AT IT_LFBK INTO WA_LFBK WHERE LIFNR = VLIFNR.
*          IF WA_LFBK-BVTYP+3(1)  NE '2'.
*            ADD 1 TO VQTDE.
*            VBVTYP = WA_LFBK-BVTYP.
*          ENDIF.
*        ENDLOOP.
*        IF VQTDE = 1.
*          WA_SAIDA-BVTYP = VBVTYP.
*        ELSE.
*          CLEAR WA_SAIDA-BVTYP.
*        ENDIF.
      else.
        wa_saida-bvtyp = wa_zfit0036-bvtyp .
      endif.
    elseif wa_zfit0036-obj_key+0(2) = 'AC'.
      wa_saida-belnr    = wa_zfit0036-belnr_adt_c.
      wa_saida-augbl    = wa_zfit0036-belnr_adt_g.
    endif.

    if vstatus = 'Z'. " Performance Brasil.
      wa_saida-tipo     = 'Adiantamento'(118).
      wa_saida-bukrs    = wa_zfit0036-bukrs.
      wa_saida-nr_solov = wa_zfit0036-invoice.
      wa_saida-belnr    = wa_zfit0036-belnr.
      read table it_bsad_brasil into wa_bsad with key  bukrs = wa_zfit0036-bukrs
                                                       belnr = wa_zfit0036-belnr binary search.
      if sy-subrc = 0.
        read table it_bsid_braadt into wa_bsid with key  bukrs = wa_bsad-bukrs
                                                         belnr = wa_bsad-augbl binary search.
        if sy-subrc = 0.
          concatenate wa_bsid-budat+6(2) '.' wa_bsid-budat+4(2) '.' wa_bsid-budat+0(4) into wa_saida-budat.
          wa_saida-kunnr = wa_bsid-kunnr.
          read table it_kna1 into wa_kna1 with key kunnr = wa_bsad-kunnr binary search.
          wa_saida-name1 = wa_kna1-name1.
          if wa_bsid-waers is not initial.
            wa_saida-waers = wa_bsid-waers.
          endif.
          wa_saida-dmbtr = wa_bsid-dmbtr.
          wa_saida-dmbe2 = wa_bsid-dmbe2.
          wa_saida-blart = wa_bsid-blart.
          wa_saida-umskz = wa_bsid-umskz.
          wa_saida-augbl = wa_bsad-augbl.
          wa_saida-augdt = wa_bsad-augdt.
          wa_saida-gjahr = wa_saida-budat+6(4).
          append wa_saida to it_saida.
        endif.
      endif.
      clear: wa_saida-tipo  ,
             wa_saida-bukrs ,
             wa_saida-nr_solov ,
             wa_saida-belnr ,
             wa_saida-budat ,
             wa_saida-kunnr ,
             wa_saida-name1 ,
*             wa_saida-waers ,
             wa_saida-dmbtr ,
             wa_saida-dmbe2 ,
             wa_saida-blart ,
             wa_saida-umskz ,
             wa_saida-augbl ,
             wa_saida-augdt .
      wa_saida-tipo     = 'Nota Fiscal'(176).
      wa_saida-bukrs    = wa_zfit0036-bukrs.
      wa_saida-nr_solov = wa_zfit0036-invoice.
      wa_saida-belnr    = wa_zfit0036-belnr.
      read table it_zsdt0053  into wa_zsdt0053 with key  nro_sol_ov = wa_zfit0036-nro_sol_ov binary search.
      if sy-subrc = 0.
        read table it_vbfa_brasil into wa_vbfa with key vbelv = wa_zsdt0053-vbeln binary search.
        if sy-subrc = 0.
          wa_saida-vbeln = wa_vbfa-vbeln.
          read table it_bkpf_brasil into wa_bkpf with key  gjahr =  wa_vbfa-gjahr
                                                           awkey =  wa_vbfa-awkey binary search.
          if sy-subrc = 0.

            if wa_bkpf-waers is not initial.
              wa_saida-waers = wa_bkpf-waers.
            endif.

            read table it_bsid_branfi into wa_bsid with key  bukrs = wa_bkpf-bukrs
                                                             belnr = wa_bkpf-belnr
                                                             gjahr = wa_bkpf-gjahr binary search.
            if sy-subrc = 0.
              wa_saida-belnr = wa_bsid-belnr.
              concatenate wa_bsid-budat+6(2) '.' wa_bsid-budat+4(2) '.' wa_bsid-budat+0(4) into wa_saida-budat.
              wa_saida-kunnr = wa_bsid-kunnr.
              wa_saida-dmbtr = wa_bsid-dmbtr.
              wa_saida-dmbe2 = wa_bsid-dmbe2.
              clear wa_saida-augbl .
              wa_saida-gjahr = wa_saida-budat+6(4).
              append wa_saida to it_saida.
              vaugbl = wa_bsid-augbl.
              while vaugbl is not initial.
                select single  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei waers budat blart augbl augdt
                from bsid
                into  wa_bsid
                where bukrs eq wa_bsid-bukrs
                and   belnr eq wa_bsid-augbl
                and   gjahr eq wa_bsid-gjahr .
                if sy-subrc = 0.
                  wa_saida-belnr = wa_bsid-belnr.
                  if wa_bsid-waers is not initial.
                    wa_saida-waers = wa_bsid-waers.
                  endif.
                  concatenate wa_bsid-budat+6(2) '.' wa_bsid-budat+4(2) '.' wa_bsid-budat+0(4) into wa_saida-budat.
                  wa_saida-kunnr = wa_bsid-kunnr.
                  wa_saida-dmbtr = wa_bsid-dmbtr.
                  wa_saida-dmbe2 = wa_bsid-dmbe2.
                  clear wa_saida-augbl .
                  wa_saida-gjahr = wa_saida-budat+6(4).
                  append wa_saida to it_saida.
                  vaugbl = wa_bsid-augbl.
                endif.
              endwhile.
            endif.
          endif.

        endif.
      endif.
    endif.

    loop at it_zib_contabil into wa_zib_contabil where obj_key = wa_zfit0036-obj_key .
      vflag = 'X'.
      wa_saida-gsber = wa_zib_contabil-gsber.
      read table it_zib_contabil_chv into wa_zib_contabil_chv with key obj_key = wa_zib_contabil-obj_key binary search.
      if sy-subrc = 0.
        wa_saida-belnr    = wa_zib_contabil_chv-belnr.
        wa_saida-bukrs    = wa_zib_contabil_chv-bukrs.
      else.
        clear wa_saida.
        continue.
        clear wa_saida-belnr.
      endif.

      wa_saida-budat    = wa_zib_contabil-budat.
      v_num = wa_zib_contabil-hkont.
      wa_saida-lifnr_z = wa_zib_contabil-hkont.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = v_num
        importing
          output = v_num.

      wa_saida-lifnr = v_num.

      read table it_lfa1 into wa_lfa1 with key lifnr = wa_zib_contabil-hkont binary search.
      if sy-subrc = 0.
        wa_saida-name1    = wa_lfa1-name1.
        wa_saida-lnrza    = wa_lfa1-lnrza.
      else.
        clear wa_saida-name1 .
      endif.

      if wa_zfit0036-obj_key+0(1) ne 'P'.
        read table it_bsak into wa_bsak with key  bukrs = wa_zib_contabil-bukrs
                                                  lifnr = wa_zib_contabil-hkont
                                                  belnr = wa_zib_contabil_chv-belnr binary search.
        if sy-subrc = 0.
*** BUG 51556 - Inicio - Camila Brand
          if  wa_zib_contabil-bukrs = '0200' and  wa_zib_contabil-waers <> 'USD'
             and ( r_st_a = 'X' or r_st_e = 'X' or r_st_l = 'X' or r_st_p = 'X') .
            wa_saida-augbl    = wa_bsak-augbl.
            wa_saida-augdt    = wa_bsak-augdt.
            wa_saida-dmbtr    = wa_bsak-dmbtr.
            wa_saida-dmbe2    = wa_zib_contabil-wrbtr.

          else.
*** BUG 51556 - Fim
            wa_saida-augbl    = wa_bsak-augbl.
            wa_saida-augdt    = wa_bsak-augdt.
            wa_saida-dmbtr    = wa_bsak-dmbtr.
            wa_saida-dmbe2    = wa_bsak-dmbe2.
          endif.
        else.
          clear:  wa_saida-augbl, wa_saida-augdt,  wa_saida-dmbtr, wa_saida-dmbe2.
          wa_saida-augdt    = wa_saida-dt_pgto.
        endif.
        read table it_bsik into wa_bsik with key  bukrs = wa_zib_contabil-bukrs
                                                  lifnr = wa_zib_contabil-hkont
                                                  belnr = wa_zib_contabil_chv-belnr binary search.
        if sy-subrc = 0.
          wa_saida-belnr    = wa_bsik-belnr.
          wa_saida-buzei    = wa_bsik-buzei.
          wa_saida-zfbdt    = wa_bsik-zfbdt.
          clear:  wa_saida-dmbtr ,  wa_saida-dmbe2.
*** BUG 51556 - inicio
          if  wa_zib_contabil-bukrs = '0200' and  wa_zib_contabil-waers <> 'USD'
             and ( r_st_a = 'X' or r_st_e = 'X' or r_st_l = 'X' or r_st_p = 'X').
            wa_saida-dmbe2 =  wa_zib_contabil-wrbtr.
          endif.
*** BUG 51556 - Fim

        endif.

        loop at it_bsik into wa_bsik where bukrs = wa_zib_contabil-bukrs
                                     and   lifnr = wa_zib_contabil-hkont
                                     and   belnr = wa_zib_contabil_chv-belnr.
*** BUG 51556 - inicio
          if  wa_zib_contabil-bukrs = '0200' and  wa_zib_contabil-waers <> 'USD'
            and ( r_st_a = 'X' or r_st_e = 'X' or r_st_l = 'X' or r_st_p = 'X').
            add wa_bsik-dmbtr to wa_saida-dmbtr.
          else.
*** BUG 51556 - Fim
            add wa_bsik-dmbtr to wa_saida-dmbtr.
            add wa_bsik-wrbtr to wa_saida-dmbe2.
          endif.
          if ( r_st_c = 'X' or r_st_cb = 'X' ).
            if wa_bsik-shkzg = 'H'.
              wa_saida-dmbtr    = wa_saida-dmbtr * -1.
              wa_saida-dmbe2    = wa_saida-dmbe2 * -1.
            endif.
          endif.
        endloop.


      endif.

      "DOCUMENTO NOVO
      "IF R_ST_B NE 'X'.
      if wa_zfit0036-belnr36 is not initial and wa_zfit0036-buzei36 is not initial.
        read table it_bsik_36 into wa_bsik with key  bukrs = wa_zfit0036-bukrs
                                                     belnr = wa_zfit0036-belnr36
                                                     buzei = wa_zfit0036-buzei36 binary search.
      elseif  wa_zfit0036-belnr36 is not initial  .
        read table it_bsik_36 into wa_bsik with key  bukrs = wa_zfit0036-bukrs
                                                     belnr = wa_zfit0036-belnr36 binary search.
      endif.
      if  wa_zfit0036-belnr36 is not initial.
        if sy-subrc = 0.
          wa_saida-augdt    = wa_zfit0036-dt_pgto. " Troca a Data de Pagamento
          wa_saida-belnr    = wa_bsik-belnr.
          wa_saida-buzei    = wa_bsik-buzei.
          wa_saida-zfbdt    = wa_bsik-zfbdt.
*          wa_saida-gjahr    = wa_bsik-gjhar.
          concatenate wa_bsik-budat+6(2) '.' wa_bsik-budat+4(2) '.' wa_bsik-budat+0(4) into wa_saida-budat.
          wa_saida-dmbtr    = wa_bsik-dmbtr.
          wa_saida-dmbe2    = wa_bsik-wrbtr.
          if ( r_st_c = 'X' or r_st_cb = 'X' ).
            if wa_bsik-shkzg = 'H'.
              wa_saida-dmbtr    = wa_saida-dmbtr * -1.
              wa_saida-dmbe2    = wa_saida-dmbe2 * -1.
            endif.
          endif.
          clear: wa_saida-augbl.
        else.
          if wa_zfit0036-belnr36 is not initial and wa_zfit0036-buzei36 is not initial.
            read table it_bsak_36 into wa_bsak with key  bukrs = wa_zfit0036-bukrs
                                                         belnr = wa_zfit0036-belnr36
                                                         buzei = wa_zfit0036-buzei36 binary search.
          elseif  wa_zfit0036-belnr36 is not initial  .
            read table it_bsak_36 into wa_bsak with key  bukrs = wa_zfit0036-bukrs
                                                         belnr = wa_zfit0036-belnr36 binary search.
          endif.
          if sy-subrc = 0.
            "WA_SAIDA-AUGDT    = WA_ZFIT0036-DT_PGTO. " Troca a Data de Pagamento
            wa_saida-augdt    = wa_bsak-augdt.
            wa_saida-augbl    = wa_bsak-augbl.
            wa_saida-belnr    = wa_bsak-belnr.
            concatenate wa_bsak-budat+6(2) '.' wa_bsak-budat+4(2) '.' wa_bsak-budat+0(4) into wa_saida-budat.
            wa_saida-dmbtr    = wa_bsak-dmbtr.
            wa_saida-dmbe2    = wa_bsak-dmbe2.
            "CLEAR: WA_SAIDA-AUGBL.
          endif.
        endif.
      endif.

      if wa_zib_contabil-waers is not initial.
        wa_saida-waers          = wa_zib_contabil-waers.
      endif.
      wa_saida-gsber          = wa_zib_contabil-gsber.
      wa_saida-hkont          = wa_zib_contabil-hkont.
      wa_saida-bvtyp          = wa_zfit0036-bvtyp.
      if wa_zfit0036-bvtyp is initial. "aberto
*        VQTDE = 0.
*        CLEAR: VBVTYP, WA_LFBK-BVTYP.
*        IF WA_SAIDA-LNRZA IS NOT INITIAL.
*          VLIFNR = WA_SAIDA-LNRZA.
*        ELSE.
*          VLIFNR = WA_SAIDA-LIFNR_Z.
*        ENDIF.
*        LOOP AT IT_LFBK INTO WA_LFBK WHERE LIFNR = VLIFNR.
*          IF WA_LFBK-BVTYP+3(1)  NE '2'.
*            ADD 1 TO VQTDE.
*            VBVTYP = WA_LFBK-BVTYP.
*          ENDIF.
*        ENDLOOP.
*        IF VQTDE = 1.
*          WA_SAIDA-BVTYP = VBVTYP.
*        ELSE.
*          CLEAR WA_SAIDA-BVTYP.
*        ENDIF.
      else.
        wa_saida-bvtyp = wa_zfit0036-bvtyp .
      endif.
      "somente incluir se a data estiver dentro do parametro p_data
      concatenate wa_saida-budat+6(4) wa_saida-budat+3(2) wa_saida-budat+0(2) into  vdata .
      wa_saida-gjahr = wa_saida-budat+6(4).
      if p_data-high is not initial.
        if vdata ge p_data-low and vdata le p_data-high.
          append wa_saida to it_saida.
        endif.
      else.
        if vdata eq p_data-low.
          append wa_saida to it_saida.
        endif.
      endif.
      clear: wa_saida-belnr,
             wa_saida-bukrs,
             wa_saida-budat,
             wa_saida-lifnr_z,
             wa_saida-lifnr,
             wa_saida-name1,
             wa_saida-augbl,
             wa_saida-augdt,
             wa_saida-dmbtr,
             wa_saida-dmbe2,
             wa_saida-waers,
             wa_saida-gsber,
             wa_saida-hkont,
             wa_saida-vbeln,
             wa_saida-kunnr,
             wa_saida-blart,
             wa_saida-umskz .

    endloop.
    if vflag = '' and vflag_p = 'X' and 'P_A' cs wa_zfit0036-obj_key+0(1).
      wa_saida-gjahr = wa_saida-budat+6(4).
      append wa_saida to it_saida.
    endif.
    clear wa_saida.
  endloop.

  " Substitui documentos compensados para Performance compensação.
  if it_saida[] is not initial and ( r_st_c = 'X' or r_st_cb = 'X' ).
    select * "bukrs belnr gjahr dmbtr dmbe2 augbl budat shkzg
      from bsik
      into corresponding fields of table it_bsik_comp
      for all entries in it_saida
      where bukrs      = it_saida-bukrs
      and   belnr      = it_saida-augbl
      and   gjahr      = it_saida-gjahr.

    select bukrs belnr gjahr dmbtr dmbe2 augbl budat shkzg
      from bsid
      into table it_bsid_comp
      for all entries in it_saida
      where bukrs      = it_saida-bukrs
      and   belnr      = it_saida-augbl
      and   gjahr      = it_saida-gjahr.
  endif.
  sort: it_bsik_comp by bukrs belnr gjahr,
        it_bsid_comp by bukrs belnr gjahr.

  " ELIMINA DOCUMENTOS ESTORNADOS e/ou substitui doc compensados
  data tabix type sy-tabix.
  loop at it_saida into wa_saida.
    tabix = sy-tabix.
    clear wa_bkpf2.
    select single  bukrs belnr gjahr stblg
      from bkpf
      into wa_bkpf2
      where belnr eq wa_saida-belnr
      and   bukrs eq wa_saida-bukrs
      and   gjahr eq wa_saida-budat+6(4).

    if sy-subrc = 0.
      if wa_bkpf2-stblg is not initial.
        delete it_saida index tabix.
      endif.
    endif.

    if ( r_st_c = 'X' or r_st_cb = 'X' ) and wa_saida-augbl is not initial.
      read table it_bsik_comp into wa_bsik_comp with key bukrs = wa_saida-bukrs
                                                         belnr = wa_saida-augbl
                                                         gjahr = wa_saida-gjahr binary search .
      if sy-subrc ne 0.
        read table it_bsid_comp into wa_bsid_comp with key bukrs = wa_saida-bukrs
                                                         belnr = wa_saida-augbl
                                                         gjahr = wa_saida-gjahr binary search .
        if sy-subrc ne 0.
          delete it_saida index tabix.
        else.
          wa_saida-belnr_o = wa_saida-belnr.
          wa_saida-belnr = wa_saida-augbl.
          wa_saida-augbl = wa_bsid_comp-augbl.
          wa_saida-dmbe2 = wa_bsid_comp-dmbe2.
          wa_saida-dmbtr = wa_bsid_comp-dmbtr.
          concatenate wa_bsid_comp-budat+6(2) '.' wa_bsid_comp-budat+4(2) '.' wa_bsid_comp-budat+0(4) into wa_saida-budat.
          if  wa_bsid_comp-shkzg = 'H'. " negativo
            wa_saida-dmbe2 = wa_saida-dmbe2 * -1.
            wa_saida-dmbtr = wa_saida-dmbtr * -1.
          endif.
          clear: wa_saida-invoice,
                 wa_saida-augdt,
                 wa_saida-navio.
          modify it_saida from wa_saida index tabix transporting belnr_o belnr augbl dmbe2 dmbtr budat invoice augdt navio.
        endif.
      else.
        wa_saida-belnr_o = wa_saida-belnr.
        wa_saida-belnr = wa_saida-augbl.
        wa_saida-augbl = wa_bsik_comp-augbl.
        wa_saida-dmbe2 = wa_bsik_comp-dmbe2.
        wa_saida-dmbtr = wa_bsik_comp-dmbtr.
        concatenate wa_bsik_comp-budat+6(2) '.' wa_bsik_comp-budat+4(2) '.' wa_bsik_comp-budat+0(4) into wa_saida-budat.
        if  wa_bsik_comp-shkzg = 'H'. " negativo
          wa_saida-dmbe2 = wa_saida-dmbe2 * -1.
          wa_saida-dmbtr = wa_saida-dmbtr * -1.
        endif.
        clear: wa_saida-invoice,
               wa_saida-augdt,
               wa_saida-navio.
        modify it_saida from wa_saida index tabix transporting belnr_o belnr augbl dmbe2 dmbtr budat invoice augdt navio.
      endif.

    endif.
  endloop.

  if ( r_st_c = 'X' or r_st_cb = 'X' ).
    data v_duplicado type i.
    refresh it_saida_aux2.
    it_saida_aux[] = it_saida[].
    sort: it_saida     by belnr,
          it_saida_aux by belnr.

    clear wa_saida_aux.
    loop at it_saida into wa_saida.
      if wa_saida_aux-belnr = wa_saida-belnr.
        continue.
      endif.
      v_duplicado = 0.
      loop at it_saida_aux into wa_saida_aux where belnr = wa_saida-belnr.
        add 1 to v_duplicado.
      endloop.
      if v_duplicado gt 1.
        clear: wa_saida-invoice,
               wa_saida-augdt,
               wa_saida-navio.
      endif.
      append wa_saida to it_saida_aux2 .
    endloop.
    refresh it_saida.
    it_saida[] = it_saida_aux2[].
    refresh: it_saida_aux, it_saida_aux2[].
  endif.

  if ( r_st_a  = 'X' or r_st_b  = 'X' ).
*    DELETE IT_SAIDA WHERE AUGBL IS NOT INITIAL AND RG_ATUALIZADO NE 'T'.
    loop at it_saida into wa_saida.
      tabix = sy-tabix.
      if wa_saida-augbl is not initial and wa_saida-rg_atualizado ne 'T' and ( wa_saida-belnr ne wa_saida-augbl ).
        wa_saida-rg_atualizado = 'D'.
        modify it_saida from wa_saida index tabix transporting rg_atualizado.
      endif.
    endloop.
    delete it_saida where rg_atualizado = 'D'.
  endif.

  if p_forn is not initial.
    delete it_saida where lifnr_z not in p_forn.
  endif.

  "  IF R_ARQ_B IS NOT INITIAL.

  " ENDIF.

*-US 164319-03-03-2025-#164319-RJF-Inicio
  if it_saida[] is not initial and p_bukrs = '0200'.
    data: lv_datcorte type zib_contabil-budat,
          lv_tbbudat  type datum,
          lv_dtcorte  type datum.

    select single *
      from tvarvc
      where name eq 'DT_CORTE_INV_SUICA'.

    if sy-subrc is initial and tvarvc-low is not initial.
      lv_datcorte = tvarvc-low.
      lv_dtcorte = lv_datcorte+6(4) && lv_datcorte+3(2) && lv_datcorte(2).

      loop at it_saida into wa_saida.
        tabix = sy-tabix.
* Verifica data corte...
        if wa_saida-budat is not initial.
          lv_tbbudat = wa_saida-budat+6(4) && wa_saida-budat+3(2) && wa_saida-budat(2).
        endif.

        if lv_tbbudat le lv_dtcorte.
          wa_saida-del = abap_true.
          modify it_saida from wa_saida index tabix transporting del.
        endif.
      endloop.

      delete it_saida where del is not initial.
    endif.
  endif.
*-US 164319-03-03-2025-#164319-RJF-Fim

endform.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_imprime_dados .

  vmudar = 'S'.
  if vstatus is not initial and
     vstatus ne 'A' and
     vstatus ne 'B' and
     vstatus ne 'L'.
    vmudar = 'N'.
  else.
    read table t_usermd with key from = sy-uname.
    if sy-subrc ne 0.
      vmudar = 'N'.
    endif.
  endif.

  if vstatus = 'Z'.
    perform f_alv_fieldcat_brasil.
  else.
    perform f_alv_fieldcat.
  endif.


  wa_layout-zebra      = 'X'.
*  wa_layout-cell_merge          = 'X'.    "Desneccessário
  wa_layout-no_rowmove = 'X'.
  wa_layout-no_rowins  = 'X'.
  wa_layout-no_rowmark = space.
  if vstatus = ' '.
    wa_layout-grid_title = 'Em Aberto'(119).
  elseif vstatus = 'A'.
    wa_layout-grid_title = 'Aguardando Aprovação'(120).
  elseif vstatus = 'L'.
    wa_layout-grid_title = 'Liberadas'(121).
  elseif vstatus = 'P'.
    wa_layout-grid_title = 'Pagas'(122).
  elseif vstatus = 'B'.
    wa_layout-grid_title = 'Baixa empresas do Grupo'(123).
  else.
    clear wa_layout-grid_title .
  endif.
  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt = 'X'.
  wa_layout-box_fname  = 'MARK'.
  wa_layout-stylefname = 'STYLE'.
  call screen 0100.


endform.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_alv_fieldcat .
  refresh it_fieldcat.
  data i type i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON0'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'Attach'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = 'X'.
  append wa_afield to it_fieldcat.


  if vstatus = ' ' or vstatus = 'L' or vstatus = 'B' or vstatus = 'P' or vstatus = 'T' or vstatus = 'A'.
    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'CHECKBOX'.
    wa_afield-checkbox      = 'X'.
    wa_afield-scrtext_s = 'Chk'.
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    wa_afield-edit          = 'X'.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.
  endif.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON1'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'St.Liquid'(124).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  if vstatus ne 'T'.
    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'ICON2'.
    wa_afield-icon          = 'X'.
    wa_afield-scrtext_s = 'St.Lib'(125).
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'LOTE'.
    wa_afield-scrtext_s = 'Lote'(126).
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    wa_afield-hotspot       = 'X'.
    append wa_afield to it_fieldcat.
  endif.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TIPO'.
  wa_afield-scrtext_s = 'Tipo'(127).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'GSBER'.
  wa_afield-scrtext_s = 'Divisão'(128).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  if vstatus = 'T'.
    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'NR_SOLOV'.
    wa_afield-scrtext_s = 'Sol.OV.'(129).
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    wa_afield-hotspot  = 'X'.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.
  endif.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'INVOICE'.
  wa_afield-scrtext_s = text-003.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_s = text-004.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-hotspot  = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUZEI36'.
  wa_afield-scrtext_s = text-358.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR_O'.
  wa_afield-scrtext_m = 'Doc.Cta.Orig'(130).
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-hotspot  = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.


  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUDAT'.
  wa_afield-scrtext_s = text-005.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.


  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ZFBDT'.
  wa_afield-scrtext_s = text-375.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LIFNR'.
  wa_afield-scrtext_s = text-006.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_s = text-007.
  wa_afield-scrtext_l = text-007. "wa_afield-scrtext_s.
  wa_afield-scrtext_m = text-007. "wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 40.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBTR'.
  wa_afield-scrtext_s = text-008.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBE2'.
  wa_afield-scrtext_s = text-009.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-ref_table     = 'ZFIT0036'.
  wa_afield-ref_field     = 'VLR_PGTO'.
  wa_afield-fieldname     = 'VLR_PGTO'.
  wa_afield-scrtext_s = text-373.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  if vstatus = ''.
    wa_afield-edit          = 'X'.
  endif.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WAERS'.
  wa_afield-scrtext_s = 'Moeda'(131).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.


  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAVIO'.
  wa_afield-scrtext_s = text-010.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  if vstatus ne 'T'.
    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'ICON3'.
    wa_afield-icon          = 'X'.
    wa_afield-scrtext_s = 'C.Bank'.
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    wa_afield-hotspot       = 'X'.
    append wa_afield to it_fieldcat.

*    I = I + 1.
*    CLEAR WA_AFIELD.
*    WA_AFIELD-COL_POS    = I.
*    WA_AFIELD-FIELDNAME  = 'BVTYP'.
*    WA_AFIELD-SCRTEXT_S  = 'Seq - Beneficiário'.
*    WA_AFIELD-SCRTEXT_L  = 'Seq - Beneficiário'.
*    WA_AFIELD-SCRTEXT_M  = 'Seq - Beneficiário'.
*    WA_AFIELD-EDIT       = ''.
*    WA_AFIELD-KEY        = ''.
*    APPEND WA_AFIELD TO IT_FIELDCAT.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'FORMA_PG'.
    wa_afield-scrtext_s = 'Form.Pgto'(132).
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    if  vstatus = ''.
      wa_afield-edit       = 'X'.
      wa_afield-drdn_hndl = '1'.
      wa_afield-checktable = '!'.
    endif.
    wa_afield-outputlen     = 30.
    append wa_afield to it_fieldcat.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'DESP_TAR'.
    wa_afield-scrtext_s = 'bank charges'.
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    if  vstatus = ''.
      wa_afield-edit       = 'X'.
      wa_afield-drdn_hndl = '2'.
      wa_afield-checktable = '!'.
    endif.
    wa_afield-outputlen     = 30.
    append wa_afield to it_fieldcat.


    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos   = i.
    wa_afield-fieldname = 'MOTIVO'.
    "WA_AFIELD-TABNAME   = 'IT_SAIDA'.
    wa_afield-scrtext_s = 'Motivo'(133).
    wa_afield-ref_table = 'ZFIT0040'.
    wa_afield-ref_field = 'MOTIVO'.
    wa_afield-scrtext_l = wa_afield-scrtext_s.
    wa_afield-scrtext_m = wa_afield-scrtext_s.
    if  vstatus = ''.
      wa_afield-edit       = 'X'.
      wa_afield-f4availabl = 'X'.
    endif.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.
  endif.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'OPERA'.
  wa_afield-scrtext_s = 'Operation'.
  "wa_afield-ref_table = 'ZFIT0043'.
  "wa_afield-ref_field = 'TP_OPERACAO'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  if  vstatus = ''.
    wa_afield-edit       = 'X'.
    wa_afield-f4availabl = 'X'.
  endif.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 30.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'OBSERVACAO'.
  wa_afield-scrtext_s = 'Observação'(134).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 30.
  append wa_afield to it_fieldcat.


  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUGBL'.
  wa_afield-scrtext_s = text-011.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-hotspot   = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 12.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUGDT'.
  wa_afield-scrtext_s = text-012.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  if r_st_e = 'X' or r_st_l = 'X'. "Aguardando aprovação / Liberadas
    wa_afield-edit          = 'X'.
  else.
    wa_afield-edit          = ''.
  endif.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 10.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'HBKID'.
  wa_afield-scrtext_s = 'Banco'(135).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  "*** PAIS do BANCO
**** US - 76596 - Inicio - CBRAND
  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKS_1'.
  wa_afield-scrtext_s = 'Pais - Beneficiário'."(359).
  wa_afield-scrtext_l = 'Pais - Beneficiário'."(359).
  wa_afield-scrtext_m = 'Pais - Beneficiário'."(359).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.
**** US - 76596 - Fim - CBRAND

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ID_TIPO_INVOICE'.
  wa_afield-scrtext_s = 'Tp.INVOICE'(136).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'INVOICE_TERC'.
  wa_afield-scrtext_s = 'INVOICE Terc0'(137).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MATNR'.
  wa_afield-scrtext_s = 'Material'(138).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MAKTX'.
  wa_afield-scrtext_m = 'Descrição Material'(139).
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'REFERENCIA'.
  wa_afield-scrtext_m = 'Referência'(140).
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

*** PBI - 74866 - Inicio
  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'USER_CREATE'.
  wa_afield-scrtext_m     = 'Usuário Lançamento'(355).
  wa_afield-scrtext_l     = 'Usuário Lançamento'(355).
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  if vstatus = 'P'.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'USUARIO_ARQ'.
    wa_afield-scrtext_m     = 'Usuário Arquivo Pagamento'(356).
    wa_afield-scrtext_l     = 'Usuário Arquivo Pagamento'(356).
    wa_afield-scrtext_s     = wa_afield-scrtext_m.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'NOME_ARQUIVO'.
    wa_afield-scrtext_m     = 'Nome Arquivo'(357).
    wa_afield-scrtext_l     = wa_afield-scrtext_m.
    wa_afield-scrtext_s     = wa_afield-scrtext_m.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.

  endif.

*** PBI - 74866 - Fim

* Ini - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos    = i.
  wa_afield-fieldname  = 'BVTYP'.
  wa_afield-scrtext_s  = 'Seq - Beneficiário'.
  wa_afield-scrtext_l  = 'Seq - Beneficiário'.
  wa_afield-scrtext_m  = 'Seq - Beneficiário'.
  wa_afield-edit       = ''.
  wa_afield-key        = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKL_1'.
  wa_afield-scrtext_s = 'Ch Bc Benef.'.
  wa_afield-scrtext_l = 'Ch Banco - Beneficiário'(361).
  wa_afield-scrtext_m = 'Ch Banco - Beneficiário'(361).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKN_1'.
  wa_afield-scrtext_s = 'Cta Banco - Beneficiário'(362).
  wa_afield-scrtext_l = 'Cta Banco - Beneficiário'(362).
  wa_afield-scrtext_m = 'Cta Banco - Beneficiário'(362).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'IBAN_1'.
  wa_afield-scrtext_s = 'IBAN - Beneficiário'(363).
  wa_afield-scrtext_l = 'IBAN - Beneficiário'(363).
  wa_afield-scrtext_m = 'IBAN - Beneficiário'(363).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SWIFT_1'.
  wa_afield-scrtext_s = 'SWIFT - Beneficiário'(364).
  wa_afield-scrtext_l = 'SWIFT - Beneficiário'(364).
  wa_afield-scrtext_m = 'SWIFT - Beneficiário'(364).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKA_1'.
  wa_afield-scrtext_s = 'Nome Banco - Beneficiário'(365).
  wa_afield-scrtext_l = 'Nome Banco - Beneficiário'(365).
  wa_afield-scrtext_m = 'Nome Banco - Beneficiário'(365).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BVTYP_2'.
  wa_afield-scrtext_s = 'Seq - Intermediário'(366).
  wa_afield-scrtext_l = 'Seq - Intermediário'(366).
  wa_afield-scrtext_m = 'Seq - Intermediário'(366).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKS_2'.
  wa_afield-scrtext_s = 'Pais - Intermediário'(367).
  wa_afield-scrtext_l = 'Pais - Intermediário'(367).
  wa_afield-scrtext_m = 'Pais - Intermediário'(367).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKL_2'.
  wa_afield-scrtext_s = 'Ch Banco - Intermediário'(368).
  wa_afield-scrtext_l = 'Ch Banco - Intermediário'(368).
  wa_afield-scrtext_m = 'Ch Banco - Intermediário'(368).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKN_2'.
  wa_afield-scrtext_s = 'Cta Banco - Intermediário'(369).
  wa_afield-scrtext_l = 'Cta Banco - Intermediário'(369).
  wa_afield-scrtext_m = 'Cta Banco - Intermediário'(369).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'IBAN_2'.
  wa_afield-scrtext_s = 'IBAN - Intermediário'(370).
  wa_afield-scrtext_l = 'IBAN - Intermediário'(370).
  wa_afield-scrtext_m = 'IBAN - Intermediário'(370).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SWIFT_2'.
  wa_afield-scrtext_s = 'SWIFT - Intermediário'(371).
  wa_afield-scrtext_l = 'SWIFT - Intermediário'(371).
  wa_afield-scrtext_m = 'SWIFT - Intermediário'(371).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKA_2'.
  wa_afield-scrtext_s = 'Nome Banco - Intermediário'(372).
  wa_afield-scrtext_l = 'Nome Banco - Intermediário'(372).
  wa_afield-scrtext_m = 'Nome Banco - Intermediário'(372).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOEDA_PGTO'.
  wa_afield-scrtext_s = 'Moeda Pagamento'(373).
  wa_afield-scrtext_l = 'Moeda Pagamento'(373).
  wa_afield-scrtext_m = 'Moeda Pagamento'(373).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

* Fim - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual

endform.                    " F_ALV_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_alv_fieldcat_brasil.
  refresh it_fieldcat.
  data i type i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.


  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CHECKBOX'.
  wa_afield-checkbox      = 'X'.
  wa_afield-scrtext_s = 'Chk'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = 'X'.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.


  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TIPO'.
  wa_afield-scrtext_s = 'Tipo'(127).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'GSBER'.
  wa_afield-scrtext_s = 'Divisão'(128).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NR_SOLOV'.
  wa_afield-scrtext_s = 'Sol.OV.'(129).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-hotspot  = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_s = text-004.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-hotspot  = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VBELN'.
  wa_afield-scrtext_s = 'Doc.Fatura'(141).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUDAT'.
  wa_afield-scrtext_s = text-005.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KUNNR'.
  wa_afield-scrtext_s = 'Cliente'(142).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_s = 'Nome Cliente'(143).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 40.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBTR'.
  wa_afield-scrtext_s = text-008.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBE2'.
  wa_afield-scrtext_s = text-009.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WAERS'.
  wa_afield-scrtext_s = 'Moeda'(131).
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUGBL'.
  wa_afield-scrtext_s = text-011.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-hotspot   = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 12.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'REFERENCIA'.
  wa_afield-scrtext_m = 'Referência'(140).
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

*** PBI - 74866 - Inicio
  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'USER_CREATE'.
  wa_afield-scrtext_m = 'Usuário Lançamento'(355).
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  if vstatus = 'P'.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'USUARIO_ARQ'.
    wa_afield-scrtext_m     = 'Usuário Arquivo Pagamento'(356).
    wa_afield-scrtext_l     = wa_afield-scrtext_m.
    wa_afield-scrtext_s     = wa_afield-scrtext_m.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.

    i = i + 1.
    clear wa_afield.
    wa_afield-col_pos       = i.
    wa_afield-fieldname     = 'NOME_ARQUIVO'.
    wa_afield-scrtext_m     = 'Nome Arquivo'(357).
    wa_afield-scrtext_l     = wa_afield-scrtext_m.
    wa_afield-scrtext_s     = wa_afield-scrtext_m.
    wa_afield-edit          = ''.
    wa_afield-key           = ''.
    append wa_afield to it_fieldcat.

  endif.

*** PBI - 74866 - Fim

* Ini - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKL_1'.
  wa_afield-scrtext_s = 'Ch Banco - Beneficiário'(361).
  wa_afield-scrtext_l = 'Ch Banco - Beneficiário'(361).
  wa_afield-scrtext_m = 'Ch Banco - Beneficiário'(361).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKN_1'.
  wa_afield-scrtext_s = 'Cta Banco - Beneficiário'(362).
  wa_afield-scrtext_l = 'Cta Banco - Beneficiário'(362).
  wa_afield-scrtext_m = 'Cta Banco - Beneficiário'(362).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'IBAN_1'.
  wa_afield-scrtext_s = 'IBAN - Beneficiário'(363).
  wa_afield-scrtext_l = 'IBAN - Beneficiário'(363).
  wa_afield-scrtext_m = 'IBAN - Beneficiário'(363).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SWIFT_1'.
  wa_afield-scrtext_s = 'SWIFT - Beneficiário'(364).
  wa_afield-scrtext_l = 'SWIFT - Beneficiário'(364).
  wa_afield-scrtext_m = 'SWIFT - Beneficiário'(364).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKA_1'.
  wa_afield-scrtext_s = 'Nome Banco - Beneficiário'(365).
  wa_afield-scrtext_l = 'Nome Banco - Beneficiário'(365).
  wa_afield-scrtext_m = 'Nome Banco - Beneficiário'(365).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BVTYP_2'.
  wa_afield-scrtext_s = 'Seq - Intermediário'(366).
  wa_afield-scrtext_l = 'Seq - Intermediário'(366).
  wa_afield-scrtext_m = 'Seq - Intermediário'(366).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKS_2'.
  wa_afield-scrtext_s = 'Pais - Intermediário'(367).
  wa_afield-scrtext_l = 'Pais - Intermediário'(367).
  wa_afield-scrtext_m = 'Pais - Intermediário'(367).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKL_2'.
  wa_afield-scrtext_s = 'Ch Banco - Intermediário'(368).
  wa_afield-scrtext_l = 'Ch Banco - Intermediário'(368).
  wa_afield-scrtext_m = 'Ch Banco - Intermediário'(368).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKN_2'.
  wa_afield-scrtext_s = 'Cta Banco - Intermediário'(369).
  wa_afield-scrtext_l = 'Cta Banco - Intermediário'(369).
  wa_afield-scrtext_m = 'Cta Banco - Intermediário'(369).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'IBAN_2'.
  wa_afield-scrtext_s = 'IBAN - Intermediário'(370).
  wa_afield-scrtext_l = 'IBAN - Intermediário'(370).
  wa_afield-scrtext_m = 'IBAN - Intermediário'(370).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SWIFT_2'.
  wa_afield-scrtext_s = 'SWIFT - Intermediário'(371).
  wa_afield-scrtext_l = 'SWIFT - Intermediário'(371).
  wa_afield-scrtext_m = 'SWIFT - Intermediário'(371).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BANKA_2'.
  wa_afield-scrtext_s = 'Nome Banco - Intermediário'(372).
  wa_afield-scrtext_l = 'Nome Banco - Intermediário'(372).
  wa_afield-scrtext_m = 'Nome Banco - Intermediário'(372).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

  i = i + 1.
  clear wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOEDA_PGTO'.
  wa_afield-scrtext_s = 'Moeda Pagamento'(373).
  wa_afield-scrtext_l = 'Moeda Pagamento'(373).
  wa_afield-scrtext_m = 'Moeda Pagamento'(373).
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  append wa_afield to it_fieldcat.

* Fim - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual

endform.                    " F_ALV_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_2000 output.
  set pf-status 'Z001'.
*  CALL METHOD CL_GUI_CFW=>DISPATCH.
  set titlebar '2000'.


endmodule.                 " STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_exit input.
  case ok-code.
    when c_back.
      set screen 0.
    when c_cancel.
      set screen 0.

    when c_exit.
      leave program.
  endcase.
endmodule.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_2000 input.
  data: vlines  type sy-tabix,
        wl_lfbk type lfbk.

  case ok-code.
    when 'SEARCH'.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wg_cadinvo-lifnr
        importing
          output = wg_cadinvo-lifnr.

      select single name1 into wg_cadinvo-name1
        from lfa1
        where lifnr = wg_cadinvo-lifnr.
    when 'GERAR'.
      data: vg_data_pgt type sy-datum.
      "Check data de pagamento CS2022000843 Bloqueio pagamento internacional fora do prazo de 48H / Anderson Oenning.
      select single *
      from zfit0179
      into @data(i_data)
      where usname_acesso eq @sy-uname.
      if sy-subrc ne 0.

        zcl_solicitacao_ov=>dia_util(
                                      exporting p_vencimento = wg_cadinvo-dt_pgto
*                                                i_bukrs      = wg_cadinvo-bukrs "USER STORY 158527 - MMSILVA - 17.01.2025
                                      importing e_subrc      = data(is_dia_util)
                                               ).

        if is_dia_util <> 4 and wg_cadinvo-dt_pgto is not initial.
          message s000(zwrm001) display like 'E' with 'Data informada não é dia útil'.
          clear: is_dia_util.
          exit.
        endif.

        clear: vg_data_pgt, i_data.
        vg_data_pgt = sy-datum + 2.

        if wg_cadinvo-dt_pgto < vg_data_pgt .
          message s000(zwrm001) display like 'E' with 'Data de pagamento está fora do prazo de 48h'(475).
          exit.
        endif.
      endif.
*      "Fim CS2022000843.

      if wg_cadinvo-waers is initial.
        message s000(zwrm001) display like 'E' with 'Moeda não informada.'(144).
        exit.
      elseif wg_cadinvo-waers ne 'USD' and wg_cadinvo-tx_cambio le 0.
        if wg_cadinvo-bukrs ne '0200'.                      "bug 53981
          message s000(zwrm001) display like 'E' with 'Valor da taxa de câmbio não informado.'(145).
          exit.
        endif.
      endif.

      if wg_cadinvo-dt_pgto is initial.
        message s000(zwrm001) display like 'E' with 'Data de Pagamento não informada.'(146).
        exit.
      elseif wg_cadinvo-dt_pgto lt sy-datum.
        message s000(zwrm001) display like 'E' with 'Data de Pagamento inválida'(147).
        exit.
      endif.

      describe table tg_conta lines vlines.
      if vlines eq 0.
        message s000(zwrm001) display like 'E' with 'Informe dados bancários'(148).
        exit.
      endif.  " US 164319-26-02-2025-#164319-RJF
      clear wl_erro.
      loop at tg_conta into wg_conta.
        select single *
              from lfbk
              into wl_lfbk
              where lifnr = wg_conta-lifnr
              and banks   = wg_conta-banks
              and bankl   = wg_conta-bankl
              and bankn   = wg_conta-bankn.
        if sy-subrc ne 0.
          message s000(zwrm001) display like 'E' with 'Dados bancários incorretos'(149).
          wl_erro = 'X'.
          exit.
        endif.
      endloop.

      if wl_erro = 'X'.
        exit.
      endif.

      clear w_flag.
      w_cont = 0.
      loop at it_saida into wa_saida.
        if wa_saida-checkbox = 'X'.
          add 1 to w_cont.
          if wa_saida-forma_pg is initial or wa_saida-motivo is initial.
            w_flag = 'X'.
            exit.
          endif.
          if wa_saida-opera is initial and p_bukrs eq '0200'. " US 164319-26-02-2025-#164319-RJF.
            w_flag = 'S'.
            exit.
          endif.

          if wa_saida-desp_tar is initial and p_bukrs eq '0200'. " US 164319-26-02-2025-#164319-RJF.
            w_flag = 'T'.
            exit.
          endif.

** US - 76596 - CBRAND - Inicio
          if wa_saida-forma_pg+0(1) = 'P'.
            perform verifica_feriado using wg_cadinvo-dt_pgto wg_cadinvo-waers '' '2000'.
            if wl_erro = 'X'.
              exit.
            endif.
          endif.
** US - 76596 - CBRAND - Fim

        endif.
      endloop.

      if w_flag = 'T'.
        message 'Enter a value for the bank charges field' type 'I'.
      elseif w_flag = 'S'.
        message 'Enter a value for the operation field' type 'I'.
      elseif w_flag = 'X'.
        message 'Informe a forma de pagto e motivo.'(150) type 'I'.
      elseif w_cont gt 0 and  wl_erro ne 'X'.
        call function 'NUMBER_GET_NEXT'
          exporting
            nr_range_nr = '01'
            object      = 'ZID_INV'
          importing
            number      = vseq.
        vnum = vseq .

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vnum
          importing
            output = vnum.
        w_cont = 0.
        loop at it_saida into wa_saida where checkbox = 'X'.
          if wa_saida-icon1 ne icon_activity.
            add 1 to w_cont.
            if wa_saida-vlr_pgto = 0.
              if wg_cadinvo-waers = 'USD' or ( p_bukrs eq '0200' and wg_cadinvo-waers = 'EUR' ).
                wa_saida-vlr_pgto = conv #( wa_saida-dmbe2 ).
              else.
                wa_saida-vlr_pgto = conv #( wa_saida-dmbtr ).
              endif.
            endif.

            update zfit0036 set lote        = vnum
                                dt_pgto     = wg_cadinvo-dt_pgto
                                moeda_pgto  = wg_cadinvo-waers
                                tx_cambio   = wg_cadinvo-tx_cambio
                                observacao  = wg_cadinvo-observacao
                                referencia  = wg_cadinvo-referencia
                                operacao    = wa_saida-opera  "novo
                                desp_tar    = wa_saida-desp_tar+0(3)  "novo
                                status      = 'A'
                                vlr_pgto    = wa_saida-vlr_pgto
                                forma_pg    = wa_saida-forma_pg+0(1)
                                motivo      = wa_saida-motivo
                                bvtyp       = wa_saida-bvtyp
                                lifnr       = wa_saida-lnrza
                                usuario     = sy-uname
                                data_atual  = sy-datum
                                hora_atual  = sy-uzeit
                                user_create = sy-uname " BUG - 75841 - CBRAND
            where obj_key = wa_saida-obj_key
            and   belnr = wa_saida-belnr36
            and   buzei = wa_saida-buzei36.
            " GRAVA DADOS BANCÁRIOS.
            loop at tg_conta into wg_conta.
              if sy-tabix = 1.
                update zfit0036 set bvtyp   = wg_conta-bvtyp
                                    swift_1 = wg_conta-swift
                                    banks_1 = wg_conta-banks
                                    bankl_1 = wg_conta-bankl
                                    banka_1 = wg_conta-banka
                                    bankn_1 = wg_conta-bankn
                                    iban_1  = wg_conta-iban
                 where obj_key = wa_saida-obj_key
                 and   belnr = wa_saida-belnr36
                 and   buzei = wa_saida-buzei36.
                "
                update zfit0036 set  bvtyp_2 = ''
                                     swift_2 = ''
                                     banks_2 = ''
                                     bankl_2 = ''
                                     banka_2 = ''
                                     bankn_2 = ''
                                     iban_2  = ''
                  where obj_key = wa_saida-obj_key
                  and   belnr = wa_saida-belnr36
                  and   buzei = wa_saida-buzei36.
              else.
                update zfit0036 set bvtyp_2   = wg_conta-bvtyp
                                   swift_2 = wg_conta-swift
                                   banks_2 = wg_conta-banks
                                   bankl_2 = wg_conta-bankl
                                   banka_2 = wg_conta-banka
                                   bankn_2 = '' "WG_CONTA-BANKN'
                                   iban_2  = wg_conta-iban
                where obj_key = wa_saida-obj_key
                and   belnr = wa_saida-belnr36
                and   buzei = wa_saida-buzei36.
              endif.
            endloop.
            wa_saida-icon1 = icon_activity.
            modify it_saida from wa_saida index sy-tabix transporting icon1 vlr_pgto.
            perform envia_email using sy-tabix.
          endif.
        endloop.
        w_contc = w_cont.
        concatenate 'Processados'(151) w_contc 'documentos'(152) into w_mensagem separated by space.
        message w_mensagem type 'I'.
        "limpa
        delete  it_saida where checkbox = 'X'.
        set screen 0.
      else.
        message 'Não foram selecionados Documentos'(153) type 'I'.
      endif.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields output.
  loop at screen.
    if screen-name = 'WG_CADINVO-TX_CAMBIO'.
      if  wg_cadinvo-waers = 'USD'.
        screen-input     = 0.
        screen-invisible = 0.
        clear wg_cadinvo-tx_cambio.
      else.
        screen-input     = 1.
        screen-invisible = 0.
      endif.
      modify screen.
    endif.
    if screen-name = 'WG_CADINVO-LIFNR' and p_bukrs = '0200'.
      screen-input     = 1.
      screen-invisible = 0.
      modify screen.
    endif.
  endloop.

endmodule.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_3000 output.
  set pf-status 'Z002'.
*  CALL METHOD CL_GUI_CFW=>DISPATCH.
  set titlebar '3000'.
endmodule.                 " STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_3000 input.
  case ok-code.
    when 'GERAR'.

*** BUG 55284 - CSB - Inicio
      if  p_bukrs = '0200'.
        clear: wl_erro.
        perform verifica_banco_empresa.
        if wl_erro = 'X'.
          exit.
        endif.
      endif.
*** BUG 55284 - CSB - Fim
      select single *
        from zfit0037
        into wa_zfit0037
        where laufi = wg_cadliq-laufi.
      if sy-subrc = 0.
        message s000(zwrm001) display like 'E' with 'Identificação já utilizada.'(154).
        exit.
      elseif wg_cadliq-hbkid is initial.
        message s000(zwrm001) display like 'E' with 'Banco não informado.'(155).
        exit.
      elseif wg_cadliq-laufi is initial.
        message s000(zwrm001) display like 'E' with 'Identificação não informada.'(156).
        exit.
      elseif wg_cadliq-path is initial and wg_cadliq-checkarq = 'X'.
        message s000(zwrm001) display like 'E' with 'Arquivo não informado.'(157).
        exit.
      endif.

      data tabix type sy-tabix.
      clear w_flag.
      w_cont = 0.
      loop at it_saida into wa_saida where checkbox = 'X'.
        add 1 to w_cont.
      endloop.

      if w_cont gt 0.
        if  wg_cadliq-checkarq = 'X'.
          clear wl_erro.
          perform zf_grava_arquivo using 'C' changing wl_erro.
          if wl_erro = 'X'.
            exit.
          endif.
        endif.
        select  single bukrs hbkid bankn hkont waers
          from t012k
          into wa_t012k
          where bukrs = p_bukrs
          and   hbkid = wg_cadliq-hbkid.
        if sy-subrc = 0.
          if not wa_t012k-hkont is initial.
            w_cont = 0.
            loop at it_saida into wa_saida where checkbox = 'X'.
              if wa_saida-rg_atualizado ne 'S'.
                tabix = sy-tabix.
                if wa_saida-obj_key+0(1) ne 'A' or wa_saida-opera eq '08'. "Adiantamento "A" e "AC" não gera SHDB
                  if ( wa_t012k-waers ne wa_saida-waers ) or ( wa_saida-opera eq '08' and wg_cadliq-moeda <> 'USD' ).
                    " Não gera SHDB
                  elseif p_bukrs = '0200'. " and wa_saida-vlr_pgto  ne wa_saida-dmbe2. " Gera Saldo.
                    clear: wg_documento, wl_belnr.

                    if wa_saida-vlr_pgto gt 0 and wa_saida-vlr_pgto ne wa_saida-dmbe2. " Gera Saldo
                      perform f_bapi_f51 changing wa_saida
                                                  wl_belnr wl_erro.
                    else.
                      wl_belnr = '9999999999'.
                    endif.
                    if wl_erro is initial.
                      perform f_bapi_f51 changing wa_saida
                                                  wl_belnr wl_erro.

                    endif.
                  else.
                    perform f_shdb       changing wa_saida wl_erro.
                  endif.
                else.
                  clear  wl_erro .
                  wg_documento = ''.
                  if wa_saida-obj_key+0(1) eq 'A' and wa_saida-opera = '05'.
                    perform f_post_adto using wa_saida  changing  wl_erro.
                  endif.
                  if wa_saida-belnr_adt_c is not initial.
                    clear: wa_bseg_aux.
                    select single *
                       from bseg
                       into wa_bseg_aux
                       where bukrs eq wa_saida-bukrs
                       and   belnr eq wa_saida-belnr_adt_c
                       and   bschl in ('31','39').
                  endif.

                endif.
                if wl_erro ne 'X'.
                  perform f_grava_liq  changing wa_saida wl_erro.
                  " Processado corretamente
                  if wa_saida-rg_atualizado eq 'T'.
                    update zfit0036 set status = 'P'
                                        hbkid = wg_cadliq-hbkid
                                        usuario    = sy-uname
                                        data_atual = sy-datum
                                        hora_atual = sy-uzeit
                    where obj_key = wa_saida-obj_key
                    and   belnr   = wa_saida-belnr36
                    and   buzei   = wa_saida-buzei36.
                  else.
                    update zfit0036 set rg_atualizado = 'S'
                                     status = 'P'
                                     hbkid = wg_cadliq-hbkid
                                     usuario    = sy-uname
                                     data_atual = sy-datum
                                     hora_atual = sy-uzeit
                 where obj_key = wa_saida-obj_key
                 and   belnr   = wa_saida-belnr36
                 and   buzei   = wa_saida-buzei36.
                  endif.

                  wa_saida-rg_atualizado = 'S'.
                  wa_saida-icon1    =   icon_system_okay.
                  wa_saida-augbl    = wg_documento.
                  modify it_saida from wa_saida index tabix transporting rg_atualizado icon1 augbl.
                  add 1 to w_cont.
                endif.
              endif.
            endloop.
            if w_cont > 0.
              if wg_cadliq-checkarq = 'X'.
                perform f_gerar_arq.
              endif.
              w_contc = w_cont.
              concatenate 'Processados'(151) w_contc 'documentos'(152) into w_mensagem separated by space.
              message w_mensagem type 'I'.
            else.
              message 'Houve um erro de processamento'(158) type 'I'.
            endif.
            set screen 0.
          else.
            message 'Conta cadastrada para este banco está em branco'(159) type 'I'.
          endif.
        else.
          message 'Não existe conta cadastrada para este banco'(160) type 'I'.
        endif.
      else.
        message 'Não foram selecionados Documentos'(153) type 'I'.
      endif.

    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_3000  INPUT


module search_fornecedor input.
  data: tl_return_tabf type table of ddshretval with header line,
        tl_dselcf      type table of dselc      with header line.

  data: begin of tl_fornec occurs 0,
          lifnr type lfa1-lifnr,
          name1 type lfa1-name1,
        end of tl_fornec.

  refresh tl_fornec.
  loop at it_saida into wa_saida where checkbox = 'X'.
    tl_fornec-lifnr = wa_saida-lifnr.
    tl_fornec-name1 = wa_saida-name1.
    append tl_fornec.
  endloop.


  sort tl_fornec by lifnr name1.
  delete adjacent duplicates from tl_fornec comparing all fields.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'LIFNR'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'LFA1-LIFNR'
      value_org       = 'S'
    tables
      value_tab       = tl_fornec
      return_tab      = tl_return_tabf
      dynpfld_mapping = tl_dselcf.

endmodule.                 " SEARCH_BANCO  INPUT

*&---------------------------------------------------------------------*
*&      Module  SEARCH_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_banco input.
  data: tl_return_tab type table of ddshretval with header line,
        tl_dselc      type table of dselc      with header line.

  data: begin of tl_banco occurs 0,
          hbkid type zimp_cad_imposto-hbkid,
          text1 type t012t-text1,
        end of tl_banco.

  select distinct hbkid text1
    from t012t
    into table tl_banco
    where bukrs eq p_bukrs.
*    and   spras eq sy-langu.

  sort tl_banco by hbkid.
  delete adjacent duplicates from tl_banco comparing all fields.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'HBKID'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_IMPOSTO-HBKID'
      value_org       = 'S'
    tables
      value_tab       = tl_banco
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

  clear: wg_zfit0168 ,wg_savesrv.
  select single *
     from zfit0168
  into wg_zfit0168
  where bukrs eq p_bukrs
   and hbkid  eq wg_cadliq-hbkid.

  wg_savesrv = wg_zfit0168-end_arquivo.

*** BUG 55284 - CSB - Inicio
  if  p_bukrs = '0200'.
    clear: gva_hbkid.
    data: wl_return_tab like line of tl_return_tab.
    read table tl_return_tab into wl_return_tab index 1.

    gva_hbkid = wl_return_tab-fieldval.

    perform verifica_banco_empresa.
  endif.
*** BUG 55284 - CSB - Fim


endmodule.                 " SEARCH_BANCO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PATH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_path input.

  data: l_dynpfields like dynpread occurs 0 with header line.
  refresh l_dynpfields.
  clear   l_dynpfields.

*** BUG 51576 -  Inicio
*  IF sy-sysid = 'QAS'.
*
*    wg_cadliq-local = 'X'.
**** BUG 51576 - Fim

* Comentei isso aqui
*    l_dynpfields-fieldname  = 'WG_CADLIQ-LOCAL'.
*    APPEND l_dynpfields.
*
*    CALL FUNCTION 'DYNP_VALUES_READ'
*      EXPORTING
*        dyname     = sy-repid
*        dynumb     = sy-dynnr
*      TABLES
*        dynpfields = l_dynpfields.
*    READ TABLE l_dynpfields INDEX 1.
*    MOVE l_dynpfields-fieldvalue TO wg_cadliq-local.

  if wg_cadliq-local = 'X'.
    call function 'WS_FILENAME_GET'
      exporting
        def_filename     = ' '
        def_path         = 'C:\'
        mask             = '*.TXT'
        mode             = 'S'
        title            = 'Busca de Arquivo'(161)
      importing
        filename         = wg_cadliq-path
      exceptions
        inv_winsys       = 1
        no_batch         = 2
        selection_cancel = 3
        selection_error  = 4
        others           = 5.
  else.
    data: wl_path type dxlpath.
    call function 'F4_DXFILENAME_TOPRECURSION'
      exporting
        i_location_flag = ' '
*       i_server        = lv_servername
        i_path          = '//'
        filemask        = '*.*'
        fileoperation   = 'R'
      importing
*       O_LOCATION_FLAG =
*       O_SERVER        =
        o_path          = wl_path
*       ABEND_FLAG      =
      exceptions
        rfc_error       = 1
        others          = 2.
    move wl_path to wg_cadliq-path.
  endif.
*  ENDIF.
endmodule.                 " SEARCH_PATH  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_shdb changing p_alv like wa_saida p_erro.
  data: vxblnr     type bkpf-xblnr,
        vdata(10),
        wl_vlr(16),
        wcontb     type i.


  data: lc_datat     type char08,
        lc_gdatu     type scurr-gdatu,
        lc_lastdate  type scurr-gdatu,
        wl_tcurr_usd type tcurr.


  if p_alv-moeda_pgto = 'USD' and p_bukrs ne '0200'.
    concatenate p_alv-dt_pgto+6(2) p_alv-dt_pgto+4(2) p_alv-dt_pgto(4) into lc_datat.

    call function 'CONVERSION_EXIT_INVDT_INPUT'
      exporting
        input  = lc_datat
      importing
        output = lc_gdatu.

    select single *
        from tcurr
        into wl_tcurr_usd
        where kurst eq 'B'
              and fcurr eq 'USD'
              and tcurr eq 'BRL'
              and gdatu eq lc_gdatu.
    "
    if sy-subrc ne 0.
      message 'Ainda não existe taxa de cambio (USD/BRL) para esta data!' type 'I'.
      p_erro = 'X'.
      exit.
    endif.

  endif.

  refresh ti_bdcdata.
  if p_alv-opera = '08'.
    vxblnr =  p_alv-referencia.
  else.
    concatenate 'INV-' p_alv-invoice into vxblnr.
  endif.

  concatenate  p_alv-dt_pgto+6(2) p_alv-dt_pgto+4(2) p_alv-dt_pgto(4) into vdata separated by '.'.

  select * "bukrs lifnr belnr dmbtr dmbe2 budat buzei gsber shkzg  umskz
          from bsik
          into corresponding fields of table it_bsik_aux
          where bukrs eq p_bukrs
          and   belnr eq p_alv-belnr.

  wcontb = 0.
  loop at it_bsik_aux into wa_bsik.
    add 1 to wcontb.
    if p_alv-opera = '08'.
      p_alv-gsber = wa_bsik-gsber.
    endif.
  endloop.

  write: p_alv-dmbe2 to wl_vlr.
  if p_alv-waers is initial.
    p_alv-waers = p_alv-moeda_pgto.
  endif.

  if  wg_cadliq-checkpa = 'X'.
    wa_t012k-hkont = '561000'.
  endif.
  perform f_bdc_data using:
    'SAPMF05A'  '0103'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'KZ',
    ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       p_alv-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       p_alv-waers,
    ''          ''      ''   'BKPF-XBLNR'       vxblnr,
    ''          ''      ''   'RF05A-KONTO'      wa_t012k-hkont,
    ''          ''      ''   'BSEG-GSBER'       p_alv-gsber,
    ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
    ''          ''      ''   'RF05A-AGKON'      p_alv-hkont,
    ''          ''      ''   'BSEG-ZUONR'       p_alv-lifnr_z,
    ''          ''      ''   'RF05A-AGKOA'      'K'.
  if p_alv-obj_key+0(1) = 'P' or p_alv-opera = '08'.
    perform f_bdc_data using:
      ''          ''      ''   'RF05A-AGUMS'      'F'.
  endif.

  perform f_bdc_data using:
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  '',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA',
    ''          ''      ''   'RF05A-SEL01(01)'  p_alv-belnr.

  if wcontb gt 1.
    perform f_bdc_data using:
    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PI',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6102PAGE',
    ''          ''      ''   'BDC_CURSOR'	      'DF05B-PSBET(01)',
    ''          ''      ''   'RF05A-ABPOS'      '1'.
  endif.

  perform f_bdc_data using:
  'SAPDF05X'  '3100'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=BU',
  ''          ''      ''   'RF05A-ABPOS'      '1'.

  clear p_erro.
  perform zf_call_transaction using 'F-53' changing p_erro.

  wait up to 5 seconds.

  if p_erro = 'X'.
    rollback work.
  else.
    if p_alv-opera = '08'.
      update zfit0036 set belnr_adt_g = wg_documento
      where obj_key = p_alv-obj_key.
    endif.
    commit work.
  endif.
endform.                    " F_SHDB

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  append wa_bdcdata to ti_bdcdata.

endform.                    " F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
form zf_call_transaction using p_trans changing p_erro.
  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  refresh it_msg.

  wl_mode = 'E'.
  if p_trans = 'F-53'.
    call transaction p_trans using ti_bdcdata
      mode wl_mode
      messages into it_msg
      update 'S'.
  else.
    call transaction p_trans using ti_bdcdata
          mode wl_mode
          messages into it_msg.
  endif.

  read table it_msg with key msgtyp = 'A'.
  if sy-subrc = 0.
    p_erro = 'X'.
  else.
    read table it_msg with key msgtyp = 'E'.
    if sy-subrc = 0.
      p_erro = 'X'.
    endif.
  endif.

  clear wg_documento.
  if p_trans = 'FBRA'.
    read table it_msg with key msgid = c_msgid
                           msgnr = c_msgne
                           msgtyp = 'S'.
  else.
    read table it_msg with key msgid = c_msgid
                               msgnr = c_msgnr
                               msgtyp = 'S'.
  endif.
  if sy-subrc = 0.
    move it_msg-msgv1 to wg_documento.
  endif.

  if  wg_documento is initial.
    p_erro = 'X'.
  else.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_documento
      importing
        output = wg_documento.
  endif.


endform.                    "ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_LIQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_grava_liq changing p_alv like wa_saida p_erro.
  data: vtext(40),
        vtam        type i,
        vloop       type i,
        vbvtyp      type lfbk-bvtyp,
        vlifnr      type lfa1-lifnr,
        wl_zfit0036 type zfit0036.

  wa_zfit0037-lote              = p_alv-lote.
  wa_zfit0037-laufi             = wg_cadliq-laufi.
  wa_zfit0037-belnr             = p_alv-belnr.
  wa_zfit0037-augbl             = wg_documento.
  wa_zfit0037-obj_key           = p_alv-obj_key.
  if p_alv-belnr is not initial.
    concatenate wg_cadliq-laufi p_alv-belnr into wa_zfit0037-nro_ref.
    condense wa_zfit0037-nro_ref no-gaps.
  else.
*    WA_ZFIT0037-NRO_REF           = P_ALV-OBJ_KEY.
    concatenate wg_cadliq-laufi p_alv-obj_key+0(14) into wa_zfit0037-nro_ref.
    condense wa_zfit0037-nro_ref no-gaps.
  endif.
  wa_zfit0037-um_ext_resp_cta   = 'NAS'.

  vtext                         = wa_t012k-bankn.
  condense vtext no-gaps.
  vtam = strlen( vtext ).
  vloop = 15 - vtam.
  while vloop > 0.
    concatenate '0' vtext into vtext.
    subtract 1 from vloop.
  endwhile.
  "wa_zfit0037-nro_cta_cliente   = vtext.
  write vtext to wa_zfit0037-nro_cta_cliente right-justified.

  wa_zfit0037-moeda_cta_client 	=	'USD'.
  wa_zfit0037-moeda_transf      = p_alv-moeda_pgto.

  concatenate  p_alv-dt_pgto+0(4) '-' p_alv-dt_pgto+4(2) '-' p_alv-dt_pgto+6(2)  into wa_zfit0037-dt_transf         .
  if p_alv-moeda_pgto = 'USD'.
*---> 08/06/2023 - Migração S4 - JS
*    WA_ZFIT0037-VLR_PAGTO         = P_ALV-DMBE2.
    wa_zfit0037-vlr_pagto = conv #( p_alv-dmbe2 ).
*<--- 08/06/2023 - Migração S4 - JS

  else.
*---> 08/06/2023 - Migração S4 - JS
*            WA_ZFIT0037-VLR_PAGTO         = P_ALV-DMBTR.
    wa_zfit0037-vlr_pagto = conv #( p_alv-dmbtr ).
*<--- 08/06/2023 - Migração S4 - JS

  endif.
  if p_alv-obj_key+0(2) = 'AC'.
    if p_alv-moeda_pgto ne 'BRL'.
*---> 08/06/2023 - Migração S4 - JS
*              WA_ZFIT0037-VLR_PAGTO         = P_ALV-DMBE2.
      wa_zfit0037-vlr_pagto  = conv #( p_alv-dmbe2 ).
*<--- 08/06/2023 - Migração S4 - JS

    else.
*---> 08/06/2023 - Migração S4 - JS
*             WA_ZFIT0037-VLR_PAGTO         = P_ALV-DMBTR.
      wa_zfit0037-vlr_pagto = conv #( p_alv-dmbtr ).
*<--- 08/06/2023 - Migração S4 - JS

    endif.
  endif.

  wa_zfit0037-motivo_transf     = p_alv-motivo.
  wa_zfit0037-ordem_priorid     = 'CRED'.


  clear: wa_lfa1,wa_lfbk,wa_bnka.
  if p_alv-lnrza is not initial.
    vlifnr = p_alv-lnrza.
  else.
    vlifnr = p_alv-lifnr_z.
  endif.
  read table it_lfa1 into wa_lfa1 with key lifnr = vlifnr  binary search.
  wa_zfit0037-nome_benef        = wa_lfa1-name1.

  wa_zfit0037-end_benef+0(35) = wa_lfa1-stras.
  wa_zfit0037-end_benef+35(35) = wa_lfa1-regio.
  wa_zfit0037-end_benef+70(30) = wa_lfa1-land1.

  " Substitui dados bancários pelos novos campos da ZFIT0036
  select single *
    from zfit0036
    into wl_zfit0036
    where obj_key = wa_saida-obj_key
    and   belnr   = wa_saida-belnr36
    and   buzei   = wa_saida-buzei36.

  clear wa_zfit0037-det_pgto.
*  IF P_ALV-OBJ_KEY+0(1) NE 'A' AND ( WL_ZFIT0036-FORMA_PG NE 'P' AND WL_ZFIT0036-REFERENCIA IS NOT INITIAL ).
  if p_alv-obj_key+0(1) ne 'A' and (  wl_zfit0036-referencia is initial ).
    concatenate '/RFB/' p_alv-invoice_terc into wa_zfit0037-det_pgto.
  else.
    concatenate '/RFB/' wl_zfit0036-referencia into wa_zfit0037-det_pgto.
  endif.
  wa_zfit0037-det_deb             = 'OUR'.

  wa_zfit0037-inform_adicio       = ''.

  if wl_zfit0036-iban_1 is not initial.
    write wl_zfit0036-iban_1  to wa_zfit0037-nro_cta_benef  right-justified.
  else.
    select single *
            from lfbk
            into wl_lfbk
            where lifnr = wa_saida-lifnr_z
            and banks  = wl_zfit0036-banks_1
            and bankl  = wl_zfit0036-bankl_1
            and bankn  = wl_zfit0036-bankn_1.
    if sy-subrc = 0.
      concatenate wl_zfit0036-bankn_1 wl_lfbk-bkont wl_lfbk-bkref into wa_zfit0037-nro_cta_benef. "ALRS 30.05.2023
      write wa_zfit0037-nro_cta_benef to wa_zfit0037-nro_cta_benef  right-justified.
    else.
      write wl_zfit0036-bankn_1 to wa_zfit0037-nro_cta_benef  right-justified.
    endif.

  endif.
  wa_zfit0037-swift_banco       = wl_zfit0036-swift_1.
  wa_zfit0037-benef_nome_bco    = wl_zfit0036-banka_1.

  "ABA ao invés de SWIFT
  clear wa_bnka.
  select single bankl swift stras banka provz banks brnch
      from bnka
    into wa_bnka
    where banks	=	wl_zfit0036-banks_1
    and   bankl	=	wl_zfit0036-bankl_1
    and   bgrup = 99.
  if sy-subrc = 0.
    wa_zfit0037-benef_cod_clear   = 'FW'.
    wa_zfit0037-benef_conta       = wa_bnka-brnch.
  endif.

  clear wa_bnka.
  select single bankl swift stras banka provz banks
        from bnka
        into wa_bnka
        where bankl = wl_zfit0036-bankl_1.

  if sy-subrc = 0.
    wa_zfit0037-benef_end_banco+0(35)  = wa_bnka-stras.
    wa_zfit0037-benef_end_banco+35(35) = wa_bnka-provz.
  else.
    clear: wa_zfit0037-benef_end_banco.
  endif.
  wa_zfit0037-benef_end_banco+70(30) =  wl_zfit0036-banks_1.

  clear wa_zfit0037-interm_conta.
  wa_zfit0037-inter_swift_bco   = wl_zfit0036-swift_2.
  wa_zfit0037-interm_nome_bco   = wl_zfit0036-banka_2.


  clear wa_bnka.
  select single bankl swift stras banka provz banks
      from bnka
      into wa_bnka
      where bankl = wl_zfit0036-bankl_2.
  if sy-subrc = 0.
    wa_zfit0037-interm_end_bco+0(35)  = wa_bnka-stras.
    wa_zfit0037-interm_end_bco+35(35) = wa_bnka-provz.
  else.
    clear: wa_zfit0037-interm_end_bco.
  endif.

  wa_zfit0037-interm_end_bco+70(30) = wl_zfit0036-banks_2.
  wa_zfit0037-nome_arquivo = wg_cadliq-path.
  wa_zfit0037-dt_ger_arq   = sy-datum.
  wa_zfit0037-hr_ger_arq   = sy-uzeit.
  wa_zfit0037-usuario_arq  = sy-uname.


  insert into  zfit0037 values wa_zfit0037.
  if sy-subrc ne 0.
    rollback work.
    p_erro = 'X'.
  else.
    commit work.
  endif.

  clear: wa_zfit0037,wa_lfa1,wa_lfbk,wa_bnka.
endform.                    " F_GRAVA_LIQ
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_gerar_arq .
  data: w_proc          type i,
        wvlr_pagto(12)  type n,
        it_zfit0037_aux type table of zfit0037,
        wa_zfit0037_aux type zfit0037,
        v_vlr_pagto     type zfit0037-vlr_pagto.

  select *
    from zfit0037
    into table it_zfit0037
    where laufi = wg_cadliq-laufi.

  it_zfit0037_aux[] = it_zfit0037[].
  sort it_zfit0037_aux by nome_benef nro_cta_benef.
  delete adjacent duplicates from it_zfit0037_aux comparing nome_benef nro_cta_benef.


  w_proc = 0.
  loop at it_zfit0037_aux into wa_zfit0037_aux.
    v_vlr_pagto = 0.
    clear w_linha.
    clear w_linha-det_pgto.
    loop at it_zfit0037 into wa_zfit0037 where nome_benef      = wa_zfit0037_aux-nome_benef
                                         and   nro_cta_benef   = wa_zfit0037_aux-nro_cta_benef.
      add wa_zfit0037-vlr_pagto to v_vlr_pagto .
      concatenate w_linha-det_pgto wa_zfit0037-det_pgto into w_linha-det_pgto separated by space.
    endloop.

    move-corresponding wa_zfit0037_aux to wa_zfit0037.
    move v_vlr_pagto to wa_zfit0037-vlr_pagto.

    w_linha-pontov1 = w_linha-pontov2 = w_linha-pontov3 = w_linha-pontov4 = w_linha-pontov5 = w_linha-pontov6 = ';'.
    w_linha-pontov7 = w_linha-pontov8 = w_linha-pontov9 = w_linha-pontov10 = w_linha-pontov11 = w_linha-pontov12 = ';'.
    w_linha-pontov13 = w_linha-pontov14 = w_linha-pontov15 = w_linha-pontov16 = w_linha-pontov17 = w_linha-pontov18 = ';'.
    w_linha-pontov19 = w_linha-pontov20 = w_linha-pontov21 = w_linha-pontov22 = w_linha-pontov23 = w_linha-pontov24 = ';'.
    w_linha-nro_ref           =  wa_zfit0037-nro_ref.
    w_linha-um_ext_resp_cta   =  wa_zfit0037-um_ext_resp_cta.
    w_linha-nro_cta_cliente   =  wa_zfit0037-nro_cta_cliente.
    w_linha-moeda_cta_client  =  wa_zfit0037-moeda_cta_client.
    w_linha-moeda_transf      =  wa_zfit0037-moeda_transf.
    w_linha-dt_transf         =  wa_zfit0037-dt_transf.

    wvlr_pagto                =  wa_zfit0037-vlr_pagto * 100.
    concatenate wvlr_pagto+1(9) '.' wvlr_pagto+10(2) into w_linha-vlr_pagto.
    w_linha-motivo_transf     =  wa_zfit0037-motivo_transf.
    w_linha-ordem_priorid     =  wa_zfit0037-ordem_priorid.
    w_linha-nro_cta_benef     =  wa_zfit0037-nro_cta_benef.
    w_linha-nome_benef        =  wa_zfit0037-nome_benef .
    w_linha-end_benef         =  wa_zfit0037-end_benef.
    if wa_zfit0037-benef_cod_clear ne 'FW'. "ABA
      w_linha-swift_banco       =  wa_zfit0037-swift_banco.
    endif.
    "
    w_linha-benef_cod_clear   =  wa_zfit0037-benef_cod_clear.
    "
    if wa_zfit0037-benef_cod_clear ne 'FW'. "ABA
      w_linha-benef_conta       =  wa_zfit0037-benef_conta.
    else.
      w_linha-benef_conta       =  wa_zfit0037-benef_conta+0(30).
    endif.
    w_linha-benef_nome_bco    =  wa_zfit0037-benef_nome_bco.
    w_linha-benef_end_banco   =  wa_zfit0037-benef_end_banco.
    w_linha-inter_swift_bco   =  wa_zfit0037-inter_swift_bco.
    w_linha-interm_cod_clear  =  wa_zfit0037-interm_cod_clear.
    w_linha-interm_conta      =  wa_zfit0037-interm_conta.
    w_linha-interm_nome_bco   =  wa_zfit0037-interm_nome_bco.
    w_linha-interm_end_bco    =  wa_zfit0037-interm_end_bco.

    "W_LINHA-DET_PGTO          =  WA_ZFIT0037-DET_PGTO.

    w_linha-det_deb           =  wa_zfit0037-det_deb.
    w_linha-inform_adicio     =  wa_zfit0037-inform_adicio.

    call function 'SCP_REPLACE_STRANGE_CHARS'
      exporting
        intext            = w_linha
      importing
        outtext           = w_linha
      exceptions
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        others            = 6.

    w_arquivo = w_linha.



    move-corresponding w_linha to w_linha2.
    w_arquivo = w_linha2.
    append w_arquivo to t_arquivo.
    clear w_linha.
    add 1 to w_proc.


  endloop.

  if w_proc > 0.
    clear wl_erro.
    perform zf_grava_arquivo using 'G' changing wl_erro.
  endif.
endform.                    " F_GERAR_ARQ
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_grava_arquivo using p_tipo changing p_erro.

******* AL11

* Empresas: 0035, 0036, 0037 : /usr/sap/LDCQAS/Remessa/ - QAS
* Empresas: 0035, 0036, 0037 : /usr/sap/LDCPRD/Remessa/ - PRD
* OUTRAS: /usr/sap/EDITC/Remessa/ - PRD
* OUTRAS: /usr/sap/trans/ - QAS

  data: wl_tabname(15),
        wl_index(2).

  clear: lv_nome_arquivo,
         wg_zfit0168.

  clear: wg_zfit0168.
  select single *
    from zfit0168
    into wg_zfit0168
    where bukrs eq p_bukrs
      and hbkid eq wg_cadliq-hbkid
      and status eq '01'. "Habilitado


  concatenate wg_zfit0168-end_arquivo wg_cadliq-path into lv_nome_arquivo.
  condense lv_nome_arquivo no-gaps.

  field-symbols <fs_tab> type any table.

  wl_tabname = 't_arquivo[]'.
  assign (wl_tabname) to <fs_tab>.

  if sy-subrc = 0.

    if wg_zfit0168-baixa_arq is not initial.

      data: wl_filename type rlgrap-filename.

      move <fs_tab>        to t_arquivo[].

      call function 'WS_FILENAME_GET'
        exporting
          def_filename     = ' '
          def_path         = 'C:\'
          mask             = '*.TXT'
          mode             = 'S'
          title            = 'Busca de Arquivo'(161)
        importing
          filename         = wl_filename
        exceptions
          inv_winsys       = 1
          no_batch         = 2
          selection_cancel = 3
          selection_error  = 4
          others           = 5.

      condense wl_filename no-gaps.

      call function 'WS_DOWNLOAD'
        exporting
          filename                = wl_filename
        tables
          data_tab                = t_arquivo
        exceptions
          file_open_error         = 1
          file_write_error        = 2
          invalid_filesize        = 3
          invalid_type            = 4
          no_batch                = 5
          unknown_error           = 6
          invalid_table_width     = 7
          gui_refuse_filetransfer = 8
          customer_error          = 9
          no_authority            = 10
          others                  = 11.
      if sy-subrc <> 0.
        message id '00' type 'E' number '398' with
           'Erro ao criar o arquivo'(166)
           'na pasta'(167)
           wl_filename .
      endif.

    else.

      if not <fs_tab> is initial or p_tipo = 'C'.

        move <fs_tab>        to t_arquivo[].

        open dataset  lv_nome_arquivo for output in text mode    "smart: 11/01/10 E111
         encoding non-unicode with windows linefeed.
        if sy-subrc <> 0.
          if p_tipo = 'G'.
            message 'Caminho ou nome de arquivo inválido. Impossível continuar!'(162)  type 'A'.
          else.
            message i000(z01)
              with 'Não foi possivel gerar o arquivo no diretorio '(163)
                    lv_nome_arquivo
                   'acionar o suporte Infra para verificar se '(164)
                   'está configurado'(165).
            p_erro = 'X'.
            exit.
          endif.
        endif.
        if p_tipo = 'G'.
          loop at t_arquivo into w_arquivo.
            transfer w_arquivo to lv_nome_arquivo.
          endloop.
        endif.
        close dataset lv_nome_arquivo.
        if p_tipo = 'C'.
          delete dataset lv_nome_arquivo.
          exit.
        endif.

      endif.
    endif.
*** BUG 54488 - XML SWIFT - inicio
    if wg_zfit0168-tp_comunica = '01'.
      perform f_gera_xml_swift using wg_zfit0168-end_arquivo wg_cadliq-path .
    endif.
*** BUG 54488 - XML SWIFT - fim
  endif.
******* Código abaixo Original.
*  DATA:
*    wl_tabname(15),
*    wl_index(2).
*  FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.
**** BUG 51576 -  Inicio
*  IF sy-sysid = 'QAS'.
*    wg_cadliq-local = 'X'.
*  ELSE.
**** BUG 51576 -  Fim
*    CLEAR wg_cadliq-local.
*  ENDIF.
*  wl_tabname = 't_arquivo[]'.
*  ASSIGN (wl_tabname) TO <fs_tab>.
*  IF sy-subrc = 0.
*    IF NOT <fs_tab> IS INITIAL OR p_tipo = 'C'.
*      CONCATENATE '/usr/sap/EDITC/Remessa/' wg_cadliq-path INTO lv_nome_arquivo.
*      IF sy-sysid = 'QAS'.
*        CONCATENATE '/usr/sap/trans/' wg_cadliq-path INTO lv_nome_arquivo.
*      ENDIF.
*      IF p_bukrs = '0035' OR p_bukrs = '0038' OR p_bukrs = '0037'.
*        IF sy-sysid = 'QAS'.
*          CONCATENATE '/usr/sap/LDCQAS/Remessa/' wg_cadliq-path INTO lv_nome_arquivo.
*        ELSE.
*          CONCATENATE '/usr/sap/LDCPRD/Remessa/' wg_cadliq-path INTO lv_nome_arquivo.
*        ENDIF.
*      ENDIF.
*      "
*      MOVE <fs_tab>        TO t_arquivo[].
*      IF wg_cadliq-local IS INITIAL.
*        OPEN DATASET  lv_nome_arquivo FOR OUTPUT IN TEXT MODE    "smart: 11/01/10 E111
*         ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
*        IF sy-subrc <> 0.
*          IF p_tipo = 'G'.
*            MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!'(162)  TYPE 'A'.
*          ELSE.
*            MESSAGE i000(z01)
*              WITH 'Não foi possivel gerar o arquivo no diretorio '(163)
*                    lv_nome_arquivo
*                   'acionar o suporte Infra para verificar se '(164)
*                   'está configurado'(165).
*            p_erro = 'X'.
*            EXIT.
*          ENDIF.
*
*        ENDIF.
*        IF p_tipo = 'G'.
*          LOOP AT t_arquivo INTO w_arquivo.
*            TRANSFER w_arquivo TO lv_nome_arquivo.
*          ENDLOOP.
*        ENDIF.
*
*        CLOSE DATASET lv_nome_arquivo.
*        IF p_tipo = 'C'.
*          DELETE DATASET lv_nome_arquivo.
*          EXIT.
*        ENDIF.
*      ELSE.
*        IF p_tipo = 'C'.
*          EXIT.
*        ENDIF.
*
*        DATA: wl_filename TYPE rlgrap-filename.
*
**** BUG 51576 -  Inicio
*        IF sy-sysid = 'QAS'.
*          MOVE wg_cadliq-path TO wl_filename.
*        ELSE.
**** BUG 51576 -  Fim
*          MOVE: lv_nome_arquivo TO wl_filename.
*        ENDIF.
*
*        CALL FUNCTION 'WS_DOWNLOAD'
*          EXPORTING
*            filename                = wl_filename
*          TABLES
*            data_tab                = t_arquivo
*          EXCEPTIONS
*            file_open_error         = 1
*            file_write_error        = 2
*            invalid_filesize        = 3
*            invalid_type            = 4
*            no_batch                = 5
*            unknown_error           = 6
*            invalid_table_width     = 7
*            gui_refuse_filetransfer = 8
*            customer_error          = 9
*            no_authority            = 10
*            OTHERS                  = 11.
*        IF sy-subrc <> 0.
*          MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH
*             'Erro ao criar o arquivo'(166)
*             'na pasta'(167)
*             wg_cadliq-path .
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*
*  WRITE:/ sy-uline.
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
*
*
*  WRITE:/1 sy-vline,
*         2 'Arquivo Gerado:'(168),
*         19 lv_nome_arquivo,
*         202 sy-vline.
*
*
*  WRITE:/1 sy-uline.
endform.                    " ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: fcode type table of sy-ucomm.
  refresh: fcode.

  if vstatus  is initial.
    append 'SAVE' to fcode.
    append '&GERAR' to fcode.
    append '&BAIXA' to fcode.
    append '&ESTORNAR' to fcode.
    append '&PERFOR' to fcode.
    append '&SMART' to fcode.
  elseif vstatus  = 'L'.
    append '&CRIAR' to fcode.
    append '&BAIXA' to fcode.
    append '&ESTORNAR' to fcode.
    append '&PERFOR' to fcode.
    append '&SMART' to fcode.
    append '&ESTOR_AD' to fcode.
  elseif vstatus  = 'B'.
    append 'SAVE' to fcode.
    append '&GERAR' to fcode.
    append '&CRIAR' to fcode..
    append '&ESTORNAR' to fcode.
    append '&PERFOR' to fcode.
    append '&SMART' to fcode.
    append '&ESTOR_AD' to fcode.
  elseif vstatus  = 'P'.
    append 'SAVE' to fcode.
    append '&GERAR' to fcode.
    append '&CRIAR' to fcode..
    append '&BAIXA' to fcode.
    append '&PERFOR' to fcode.
    append '&ESTOR_AD' to fcode.
  elseif vstatus  = 'T' or vstatus  = 'Z' .
    append 'SAVE' to fcode.
    append '&GERAR' to fcode.
    append '&CRIAR' to fcode..
    append '&BAIXA' to fcode.
    append '&ESTORNAR' to fcode.
    append '&SMART' to fcode.
    append '&ESTOR_AD' to fcode.
  else.
    if not 'A_L' cs vstatus.
      append 'SAVE' to fcode.
    endif.
    append '&GERAR' to fcode.
    append '&CRIAR' to fcode.
    append '&BAIXA' to fcode.
    append '&ESTORNAR' to fcode.
    append '&PERFOR' to fcode.
    append '&SMART' to fcode.
    append '&ESTOR_AD' to fcode.
  endif.

  if vmudar = 'N'.
    append '&MUDAR' to fcode.
    append '&ELIMINA' to fcode.
  endif.

  set pf-status 'F_SET_PF' excluding fcode.
  set titlebar  'ZFTITLE'.



  if cl_container_95 is initial.
    create object cl_container_95
      exporting
        side  = '4'
        ratio = '80'.
  endif.

  if not cl_grid is initial.

    call method cl_grid->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = it_fieldcat[].
    loop at it_fieldcat into w_fieldcat
     where fieldname eq 'FORMA_PG'.
      w_fieldcat-outputlen = '20'.
      modify it_fieldcat from w_fieldcat.
    endloop.
    call method cl_grid->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = it_fieldcat[].

    loop at it_saida into wa_saida where checkbox = 'X'.
      if wa_saida-rg_atualizado = 'S' and vstatus = ''.
        wa_saida-icon1 = icon_activity.
        modify it_saida from wa_saida index sy-tabix transporting icon1.
      endif.
    endloop.
    if wg_atualiza_shdb = 'X'. " atualizado SHDB baixa grupo empresas
      sort: tg_externo by invoice belnr36,
            tg_brasil  by invoice belnr36,
            tg_pagar   by obj_key belnr36,
            tg_adiant  by obj_key,
            tg_adiantbra by obj_key.
      loop at it_saida into wa_saida.
        clear wl_erro.
        tabix = sy-tabix .
        if tg_externo[] is not initial.
          loop at tg_externo into wg_externo where invoice = wa_saida-invoice
                                             and   belnr36 = wa_saida-belnr36.
            if wg_externo-fg_ok ne 'X'.
              wl_erro = 'X'.
            endif.
          endloop.
          loop at tg_brasil into wg_brasil  where invoice = wa_saida-invoice
                                            and   belnr36 = wa_saida-belnr36.
            if wg_externo-fg_ok ne 'X'.
              wl_erro = 'X'.
            endif.
          endloop.
          if wl_erro eq 'X'.
            wa_saida-icon1 = icon_incomplete.
          else.
            wa_saida-icon1 = icon_okay.
          endif.
        endif.
        if tg_pagar[] is not initial.
          read table tg_pagar into wg_pagar with key obj_key = wa_saida-obj_key
                                                     belnr36 = wa_saida-belnr36 binary search.
          if sy-subrc = 0.
            if wg_pagar-fg_ok = 'X'.
              wa_saida-icon1 = icon_okay.
            else.
              wa_saida-icon1 =  icon_incomplete.
            endif.
          endif.
        endif.
        if tg_adiant[] is not initial.
          read table tg_adiant into wg_adiant with key obj_key = wa_saida-obj_key binary search.

          if sy-subrc = 0.
            if wg_adiant-fg_ok = 'X'.
              wa_saida-icon1 = icon_okay.
            else.
              wa_saida-icon1 =  icon_incomplete.
            endif.
          endif.
        endif.
        if tg_adiantbra[] is not initial.
          read table tg_adiantbra into wg_adiantbra with key obj_key = wa_saida-obj_key binary search.

          if sy-subrc = 0.
            if wg_adiantbra-fg_ok = 'X'.
              wa_saida-icon1 = icon_okay.
            else.
              wa_saida-icon1 =  icon_incomplete.
            endif.
          endif.
        endif.
        modify it_saida from wa_saida index tabix transporting icon1.
      endloop.
      refresh: tg_externo,tg_brasil, tg_pagar, tg_adiant.
    endif.
    perform zf_alv_header.
    call method cl_grid->refresh_table_display.
    if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

  else.
    create object obj_dyndoc_id
      exporting
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        no_margins = 'X'.

    perform zf_alv_header .


    if editcontainer is initial .
      create object editcontainer
        exporting
          container_name = 'HEADER'.
    endif .

    call method obj_dyndoc_id->merge_document.

    call method obj_dyndoc_id->display_document
      exporting
        reuse_control      = 'X'
        parent             = editcontainer
      exceptions
        html_display_error = 1.


    create object cl_grid
      exporting
        i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    call method cl_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    data: lt_dropdown type lvc_t_drop,
          ls_dropdown type lvc_s_drop.

* First listbox (handle '1').
    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'C-Compensação'(116).
    append ls_dropdown to lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'A-Comp.Adto'(169).
    append ls_dropdown to lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'P-Pagamento'(117).
    append ls_dropdown to lt_dropdown.

    ls_dropdown-handle = '2'.
    ls_dropdown-value = 'BEN'.
    append ls_dropdown to lt_dropdown.

    ls_dropdown-handle = '2'.
    ls_dropdown-value = 'SHA'.
    append ls_dropdown to lt_dropdown.

    ls_dropdown-handle = '2'.
    ls_dropdown-value = 'OUR'.
    append ls_dropdown to lt_dropdown.


    call method cl_grid->set_drop_down_table
      exporting
        it_drop_down = lt_dropdown.

    wa_stable-row        = c_x.

    wg_save = 'X'.
    wa_layout-info_fname    = 'LINE_COLOR'.

    wg_x_variant-report = sy-repid. "Enable users save own LAYOUTs

    wa_layout-box_fname  = 'MARK'.
    wa_layout-sel_mode   = 'B'.

    call method cl_grid->set_table_for_first_display
      exporting
        is_variant      = gs_variant_c "WG_X_VARIANT
        is_layout       = wa_layout
        i_save          = wg_save
        i_default       = 'X'
      changing
        it_fieldcatalog = it_fieldcat[]
        it_sort         = i_sort[]
        it_outtab       = it_saida[].

    refresh gt_f4.
    gt_f4-fieldname = 'OPERA'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    append gt_f4.

    call method cl_grid->register_f4_for_fields
      exporting
        it_f4 = gt_f4[].
    create object event_receiver.
    set handler event_receiver->catch_hotspot              for cl_grid.
    set handler event_receiver->on_data_changed            for cl_grid.
    set handler event_receiver->on_data_changed_finished   for cl_grid.
    set handler event_receiver->on_f4                      for cl_grid.



  endif.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  data: vforma_pg(1),
        vlifnr(10)   type n,
        vlifnc(10)   type c,
        vlinha       type i,
        vqtde        type i,
        w_answer(1),
        wlin(3),
        wmsg(50),
        vobjectid2   type cdhdr-objectid,
        wl_zfit0036  type  zfit0036.

  data: t_cdhdr2  type table of ty_cdhdr,
        t_cdpos2  type table of ty_cdpos,
        wa_cdhdr2 type ty_cdhdr,
        wa_cdpos2 type ty_cdpos,
        wa_bseg   type bseg.


  if not cl_grid is initial.
    call method cl_grid->dispatch
      exporting
        cargo         = sy-ucomm
        eventid       = 19
        is_shellevent = ' '.

    if sy-ucomm is initial.
      call method cl_grid->refresh_table_display
        exporting
          is_stable = is_stable.
    endif.
  endif.

  case sy-ucomm.
    when 'BACK' or 'UP'.
      refresh it_saida.
      call method cl_grid->refresh_table_display.
      leave to screen 0.
    when '&REFRESH'.
      if p_data[] is not initial.
        refresh it_saida.
        perform:
                f_seleciona_dados, " Form seleciona dados
                f_saida, " Form de saida
                f_imprime_dados.
      endif.
    when 'CANCEL'.
      leave program.
    when '&CHECK'.
      call method cl_grid->get_selected_rows
        importing
          et_index_rows = tl_index_rows.

      loop at tl_index_rows into wl_index_rows.
        read table it_saida into wa_saida index wl_index_rows-index.
        if wa_saida-checkbox = ''.
          wa_saida-checkbox = 'X'.
        else.
          wa_saida-checkbox = ' '.
        endif.
        modify it_saida from wa_saida index wl_index_rows-index  transporting  checkbox.
      endloop.

* Ini - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual
    when '&EMI_MANUAL'.

      data: lv_dmbe2      type dmbe2,
            ls_spell      type spell,
            lv_cc         type char50,
            lv_end_comp   type char50,
            lv_end_comp1  type char50,
            lv_end_comp2  type char50,
            lv_desc_land1 type  bezei20,
            lv_erro(1).

      read table it_saida into data(wa_saidac) with key checkbox = abap_on.
      if sy-subrc is initial.
        select single * from zfit0036
          into @data(wa_zfit0036c)
          where obj_key eq @wa_saidac-obj_key.
      endif.

      clear: lv_dmbe2, lv_erro.
      loop at it_saida into wa_saida  where checkbox = 'X'.
        select single * from zfit0036
          into @data(wa_zfit0036a)
          where obj_key eq @wa_saida-obj_key.
        if wa_zfit0036a-dt_pgto ne wa_zfit0036c-dt_pgto or
           wa_zfit0036a-swift_1 ne wa_zfit0036c-swift_1 or
           wa_zfit0036a-banks_1 ne wa_zfit0036c-banks_1 or
           wa_zfit0036a-bankl_1 ne wa_zfit0036c-bankl_1 or
           wa_zfit0036a-banka_1 ne wa_zfit0036c-banka_1 or
           wa_zfit0036a-bankn_1 ne wa_zfit0036c-bankn_1 or
           wa_zfit0036a-iban_1  ne wa_zfit0036c-iban_1  or
           wa_zfit0036a-bvtyp_2 ne wa_zfit0036c-bvtyp_2 or
           wa_zfit0036a-swift_2 ne wa_zfit0036c-swift_2 or
           wa_zfit0036a-banks_2 ne wa_zfit0036c-banks_2 or
           wa_zfit0036a-bankl_2 ne wa_zfit0036c-bankl_2 or
           wa_zfit0036a-banka_2 ne wa_zfit0036c-banka_2 or
           wa_zfit0036a-bankn_2 ne wa_zfit0036c-bankn_2 or
           wa_zfit0036a-iban_2  ne wa_zfit0036c-iban_2.
          lv_erro = 'X'.
          data(lv_msg) = 'Data de pagamento e dados bancarios tem que ser identicos.'. "&& wa_saida-lote.
          message lv_msg type 'I'.
          exit.
        endif.
        if  wa_saida-dmbe2 gt 0.
          lv_dmbe2 = ( ( lv_dmbe2 ) + ( wa_saida-dmbe2 ) ).
        else.
          lv_dmbe2 = ( ( lv_dmbe2 ) + ( wa_saida-vlr_pgto ) ).
        endif.
      endloop.
      if lv_erro = 'X'.
        exit.
      endif.
      read table it_saida into data(wa_saidax) with key checkbox = abap_on.
      if sy-subrc is initial.
        select * from zfit0036 " ZFIT0036-moeda_pgto
          up to 1 rows
          into @data(wa_zfit0036x)
          where obj_key eq @wa_saidax-obj_key.
        endselect.

        if wa_zfit0036x-bankl_2 is not initial.
          select single bankl, swift, stras, banka, provz, banks
          from bnka
          into @data(wa_bnkax)
          where bankl = @wa_zfit0036x-bankl_2.
          if sy-subrc = 0.
            lv_end_comp = wa_bnkax-stras && '-' && wa_bnkax-provz.
          endif.
        endif.

        if wa_zfit0036x-bankl_1 is not initial.
          select single bankl, swift, stras, banka, provz, banks
          from bnka
          into @data(wa_bnka1)
          where bankl = @wa_zfit0036x-bankl_1.
          if sy-subrc = 0.
            lv_end_comp1 = wa_bnka1-stras && '-' &&  wa_bnka1-provz.
          endif.
        endif.

*       Beneficiário: - Nº de conta ou IBAN
        if wa_zfit0036x-bankl_1 is not initial.
          if wa_zfit0036x-iban_1 is not initial.
            write wa_zfit0036x-iban_1  to wa_zfit0037-nro_cta_benef  right-justified.
          else.
*            wa_zfit0037-nro_cta_benef = |{ wa_zfit0036x-bankl_1 } / { wa_zfit0036x-bankn_1 }|.
            wa_zfit0037-nro_cta_benef =  wa_zfit0036x-bankn_1.
          endif.
        else.
          select single *
                  from lfbk
                  into wl_lfbk
                  where lifnr = wa_saidax-lifnr_z
                  and banks  = wa_zfit0036x-banks_1
                  and bankl  = wa_zfit0036x-bankl_1
                  and bankn  = wa_zfit0036x-bankn_1.
          if sy-subrc = 0.
            concatenate wa_zfit0036x-bankn_1 wl_lfbk-bkont wl_lfbk-bkref into wa_zfit0037-nro_cta_benef.
            write wa_zfit0037-nro_cta_benef to wa_zfit0037-nro_cta_benef  right-justified.
          else.
            write wa_zfit0036x-bankn_1 to wa_zfit0037-nro_cta_benef  right-justified.
          endif.

        endif.
*       Nome e Endereço
        if wa_saidax-lnrza is not initial.
          vlifnr = wa_saidax-lnrza.
        else.
          vlifnr = wa_saidax-lifnr.
        endif.

        sort it_lfa1 by lifnr.
*        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = vlifnr BINARY SEARCH.
        vlifnc = vlifnr.
        loop at it_lfa1 into wa_lfa1 where lifnr = vlifnc and ort01 is not initial.
          exit.
        endloop.
        if sy-subrc is initial.
          lv_end_comp2 = wa_lfa1-stras && '-' && wa_lfa1-regio.
        endif.
        select single * from t001
        into @data(wa_t001)
        where bukrs eq @wa_saidax-bukrs.


        "Selecionar descrição do pais.
        select single landx from t005t into lv_desc_land1
          where land1 eq wa_lfa1-land1 "wa_saida-banks_1
            and spras eq sy-langu.
        if sy-subrc eq 0.
*          lv_desc_land1 = |{ wa_saida-banks_1 } - { lv_desc_land1 }|.
          lv_desc_land1 = |{ wa_lfa1-land1 } - { lv_desc_land1 }|.
        endif.

      endif.

      call function 'SPELL_AMOUNT'
        exporting
          amount    = lv_dmbe2
          currency  = wa_zfit0036x-moeda_pgto
          filler    = ' '
          language  = sy-langu
        importing
          in_words  = ls_spell
        exceptions
          not_found = 1
          too_large = 2
          others    = 3.

      if ls_spell-word is not initial and ls_spell-decword ne 'ZERO' and wa_zfit0036x-moeda_pgto eq 'BRL'. "sy-langu EQ 'P'.
*        ls_spell-word = ls_spell-word && | E | && ls_spell-decword && | CENTAVO(S) |.
        ls_spell-word = ls_spell-word  && | REAIS | && | E | && ls_spell-decword && | CENTAVO(S) |.
      elseif ls_spell-word is not initial and ls_spell-word ne 'ZERO' and wa_zfit0036x-moeda_pgto eq 'USD'."sy-langu EQ 'E'.
        if ls_spell-decword eq 'ZERO'.
          clear ls_spell-decword.
        endif.
        if ls_spell-decword is not initial.
          ls_spell-word = ls_spell-word  && | DOLAR(ES) | && | E | && ls_spell-decword && | CENTAVO(S) |.
        else.
          ls_spell-word = ls_spell-word && ls_spell-decword && | DOLAR(ES) |.
        endif.
      else.
        ls_spell-word = ls_spell-word.
      endif.

      data: vg_fm_name type rs38l_fnam. "Nome da função smart form

*     Chamada de relatório SMARTFORMS
      call function 'SSF_FUNCTION_MODULE_NAME'
        exporting
          formname = 'ZFIR0025_SMART_FORM'
        importing
          fm_name  = vg_fm_name
        exceptions
          others   = 3.

      check sy-subrc is initial.

      case wa_saida-bukrs.
        when '0004'.
          lv_cc = 'ITAU = 0004 - Amaggi International C/C 1891-0'.
        when '0015'.
          lv_cc = 'ITA15 = 0015 - AGROPECUARIA MAGGI C/C/ 1894-5'.
        when '0001'.
          lv_cc = 'ITA01 = 0001 - AMAGGI EXPORTAÇÃO C/C/ 1897-0'.
        when '0010'.
          lv_cc = 'ITA10 = 0010 - HERMASA C/C/ 90021-1'.
        when '0050'.
          lv_cc = 'ITA50 = 0050 O TELHAR C/C/ 96033-1'.
        when '0039'.
          lv_cc = 'ITA39 = 0039 nAVEGAÇÇOES C/C 139986-1'.
        when '0203'.
          lv_cc = 'ITALE = 0203 - LUXEMBOURG C/C 160985-1'.
      endcase.

      call function vg_fm_name
        exporting
          bukrs         = wa_saida-bukrs
          contad        = lv_cc
          moeda_pgto    = wa_zfit0036x-moeda_pgto
          dmbe2         = lv_dmbe2
          word          = ls_spell-word
          dt_pgto       = wa_zfit0036x-dt_pgto
          swift_2       = wa_zfit0036x-swift_2 "
          banka_2       = wa_zfit0036x-banka_2
          endcom        = lv_end_comp
          bankn_2       = wa_zfit0036x-bankn_2
          swift_1       = wa_zfit0036x-swift_1 "
          banka_1       = wa_zfit0036x-banka_1
          endcom1       = lv_end_comp1
          nro_cta_benef = wa_zfit0037-nro_cta_benef "
          name1         = wa_lfa1-name1
          endcom2       = lv_end_comp2
          ort01         = wa_lfa1-ort01
          land1         = wa_lfa1-land1
          referencia    = wa_zfit0036x-referencia
          butxt         = wa_t001-butxt
          lv_desc_land1 = lv_desc_land1.
* Fim - RJF - CS2023000844 ZFI0017 - Extração de relatório para pagamento manual

    when '&ELIMINA'.
      call function 'POPUP_TO_CONFIRM'
        exporting
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          text_question         = '“Deseja marcar para eliminação sim ou não?'(170)
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
        importing
          answer                = w_answer
*       TABLES
*         PARAMETER             =
        exceptions
          text_not_found        = 1
          others                = 2.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
      if w_answer = '1'.
        clear wlin.
        loop at it_saida into wa_saida where checkbox = 'X'.
          wlin = sy-tabix.
          update zfit0036 set   eliminar = 'X'
                 where belnr    eq wa_saida-belnr36
                 and   buzei    eq wa_saida-buzei36
                 and   obj_key  eq wa_saida-obj_key.
          if sy-subrc = 0.
            concatenate 'Documento eliminado com Sucesso,linha'(171) wlin into wmsg separated by space.
            message wmsg type 'I'.
          else.
            concatenate 'Ocorreu erro na eliminação,linha'(172) wlin into wmsg separated by space.
            message wmsg type 'I'.
          endif.
        endloop.
        if wlin is initial.
          message 'Nenhum documento foi eliminado'(173) type 'I'.
        endif.
      endif.
    when '&SMART'.
      refresh it_saida_pa.
      sort  it_zinv_lotes_aprov by lote data_atual hora_atual descending.
      loop at it_saida into wa_saida where checkbox = 'X'.
        move-corresponding wa_saida to wa_saida_pa.
        if wa_saida-moeda_pgto ne 'USD'.
          wa_saida_pa-vlr_pgto = wa_saida-dmbtr.
        else.
          wa_saida_pa-vlr_pgto = wa_saida-dmbe2.
        endif.
        if wa_saida-obj_key+0(2) = 'AC'.
          if wa_saida-moeda_pgto ne 'BRL'.
            wa_saida_pa-vlr_pgto         = wa_saida-dmbe2.
          else.
            wa_saida_pa-vlr_pgto         = wa_saida-dmbtr.
          endif.
        endif.

        read table it_zinv_lotes_aprov into wa_zinv_lotes_aprov with key lote = wa_saida-lote binary search.
        if sy-subrc = 0.
          wa_saida_pa-aprovador  = wa_zinv_lotes_aprov-aprovador.
          wa_saida_pa-data_atual = wa_zinv_lotes_aprov-data_atual.
          wa_saida_pa-hora_atual = wa_zinv_lotes_aprov-hora_atual.
        else.
          if wa_saida-obj_key+0(2) = 'AC' and wa_saida-opera ne '08'.
            select single *
                from bkpf
                into wa_bkpf_2
                where bukrs = wa_saida-bukrs
                and   belnr = wa_saida-belnr_adt_c.

            if wa_bkpf_2-awkey+0(5) = 'ZGL17'. "pega a estrategia da ZGL
              select single *
                from zglt035
                into wa_zglt035_2
               where doc_lcto = wa_bkpf_2-awkey+5(10).
              if sy-subrc = 0.
                select *
                  from zglt038
                   into table it_zglt038_2
                  where lote = wa_zglt035_2-lote.
                sort  it_zglt038_2 by data_atual hora_atual  descending.

                if it_zglt038_2[] is not initial.
                  read table it_zglt038_2 into wa_zglt038_2 index 1.
                  wa_saida_pa-aprovador  = wa_zglt038_2-aprovador.
                  wa_saida_pa-data_atual = wa_zglt038_2-data_atual.
                  wa_saida_pa-hora_atual = wa_zglt038_2-hora_atual.
                endif.

              endif.

            else. "pedido
* ---> S4 Migration - 15/06/2023 - MA
*Não tem os campos chaves
              select single *
                 from bseg
                 into wa_bseg
                 where bukrs eq wa_saida-bukrs
                 and   belnr eq wa_saida-belnr_adt_c
                 and   ebeln ne ''.    "#EC CI_DB_OPERATION_OK[2431747]

*<--- S4 Migration - 15/06/2023 - MA
              if sy-subrc = 0.
                vobjectid2 = wa_bseg-ebeln.
                select objectclas objectid changenr tcode username udate
                  from cdhdr
                    into table t_cdhdr2
                       where objectid   eq vobjectid2
                         and objectclas eq 'EINKBELEG'
                         and tcode      eq 'ME29N'.

                if sy-subrc is initial.
                  select  objectclas objectid changenr fname value_new
                    from cdpos
                    into table t_cdpos2
                    for all entries in t_cdhdr2
                    where objectclas eq t_cdhdr2-objectclas
                    and   objectid   eq t_cdhdr2-objectid
                    and   changenr   eq t_cdhdr2-changenr
                    and   fname      eq 'FRGKE'
                    and   value_new  eq '2'.
                endif.
                sort:  t_cdhdr2 by objectid ascending changenr descending,
                       t_cdpos2 by objectclas objectid changenr.

                read table t_cdhdr2 into wa_cdhdr2
                              with key objectid   = vobjectid2.
                read table t_cdpos2 into wa_cdpos2 with key  objectclas = wa_cdhdr2-objectclas
                                                              objectid   = wa_cdhdr2-objectid
                                                              changenr   = wa_cdhdr2-changenr binary search.
                if sy-subrc = 0.
                  wa_saida_pa-aprovador  = wa_cdhdr2-username.
                  wa_saida_pa-data_atual = wa_cdhdr2-udate.
                  wa_saida_pa-hora_atual = ''.
                endif.
              endif.

            endif.
          endif.
        endif.
        select single *
            from zfit0036
            into wl_zfit0036
            where obj_key = wa_saida-obj_key
            and   belnr   = wa_saida-belnr36
            and   buzei   = wa_saida-buzei36.
        wa_saida_pa-banks_1 = wl_zfit0036-banks_1.
        wa_saida_pa-bankl_1 = wl_zfit0036-bankl_1.
        wa_saida_pa-bankn_1 = wl_zfit0036-bankn_1.
        wa_saida_pa-iban_1  = wl_zfit0036-iban_1.
        append wa_saida_pa to it_saida_pa.
      endloop.
      perform f_imprime_smart using it_saida_pa.
    when 'SAVE'.
      loop at it_saida into wa_saida where checkbox = 'X'.

** US - 76596 - CBRAND - Inicio
        perform verifica_feriado using wa_saida-augdt wa_saida-moeda_pgto wa_saida-banks_1 '0100'.
        if wl_erro ne 'X' .
** US - 76596 - CBRAND - Fim
          update zfit0036 set   dt_pgto    = wa_saida-augdt
          where belnr    eq wa_saida-belnr36
          and   buzei    eq wa_saida-buzei36
          and   obj_key  eq wa_saida-obj_key.
        endif.
      endloop.
    when '&MUDAR'.
      refresh tg_status.
      loop at it_saida into wa_saida where checkbox = 'X'.
        if wa_saida-augbl is initial.
          wg_status-tipo      = wa_saida-tipo.
          wg_status-invoice   = wa_saida-invoice.
          wg_status-belnr     = wa_saida-belnr.
          wg_status-budat     = wa_saida-budat.
          wg_status-lifnr     = wa_saida-lifnr.
          wg_status-name1     = wa_saida-name1.
          wg_status-dmbe2     = wa_saida-dmbe2.
          wg_status-status    = wa_saida-status.
          wg_status-obj_key   = wa_saida-obj_key.
          wg_status-belnr36   = wa_saida-belnr36.
          wg_status-buzei36   = wa_saida-buzei36.
          append wg_status to tg_status.
        endif.
      endloop.
      if tg_status[] is not initial.
        call screen 9300     starting at 070 3
                             ending   at 170 12.
      else.
        message 'Não foi selecionada nenhuma linha'(174) type 'I'.
      endif.
    when  '&PERFOR'.
      if vstatus = 'Z'.
        clear wg_cadbrasil.
        clear  wl_erro.
        vlinha = 0.
        loop at it_saida into wa_saida where checkbox = 'X'.
          if wa_saida-tipo ne 'Adiantamento'(118).
            message 'Selecione apenas Adiantamento'(175) type 'I'.
            wl_erro = 'X'.
            exit.
          endif.
          add 1 to vlinha.
        endloop.
        if vlinha eq 1.
          if wl_erro ne 'X' .
            refresh tg_adiantbra.
            clear wg_adiantbra.
            loop at it_saida into wa_saida where checkbox = 'X'.
              wg_adiantbra-augbl       = wa_saida-augbl.
              wg_adiantbra-augdt       = wa_saida-augdt.
              wg_adiantbra-waers       = wa_saida-waers.
              if wa_saida-waers = 'USD'.
                wg_adiantbra-vlr_doc   = wa_saida-dmbe2.
              else.
                wg_adiantbra-vlr_doc  = wa_saida-dmbtr.
              endif.
              wg_adiantbra-vlr_comp    = 0.
              wg_adiantbra-vlr_sld     = wg_adiantbra-vlr_doc.
              wg_adiantbra-obj_key     = wa_saida-obj_key.
              wg_adiantbra-blart       = wa_saida-blart.
              wg_adiantbra-umskz       = wa_saida-umskz.
              append wg_adiantbra to tg_adiantbra.
              wg_cadbrasil-nro_sol_ov = wa_saida-nr_solov.
              wg_cadbrasil-kunnr = wa_saida-kunnr.
              wg_cadbrasil-name1 = wa_saida-name1.
            endloop.
            clear:  wg_atualiza_shdb.
            refresh tg_notas.
            loop at it_saida into wa_saida where nr_solov = wg_cadbrasil-nro_sol_ov.
              if wa_saida-tipo ne 'Adiantamento'(118).
                wg_notas-augbl      = wa_saida-belnr.
                concatenate wa_saida-budat+6(4) wa_saida-budat+3(2) wa_saida-budat+0(2) into  wg_notas-augdt.
                wg_notas-vbeln      = wa_saida-vbeln.
                wg_notas-waers      = wa_saida-waers.
                if wa_saida-waers = 'USD'.
                  wg_notas-vlr_doc    = wa_saida-dmbe2.
                else.
                  wg_notas-vlr_doc   = wa_saida-dmbtr.
                endif.
                wg_notas-vlr_comp   = 0.
                wg_notas-vlr_sld    = wg_notas-vlr_doc.
                append wg_notas to tg_notas.
              endif.
            endloop.
            call screen 8000 starting at 060 2
             ending   at 170 35.
          endif.
        else.
          message 'Selecione um documento por vez'(177) type 'I'.
        endif.

      else.
        clear wg_cadadt.
        clear  wl_erro.
        vlinha = 0.
        clear:  vlifnr.
        loop at it_saida into wa_saida where checkbox = 'X'.
          if wa_saida-tipo ne 'Adto Performance'(117).
            message 'Selecione apenas Adto Performance'(178) type 'I'.
            wl_erro = 'X'.
            exit.
          endif.
          if vlinha gt 0 and vlifnr  ne wa_saida-lifnr.
            message 'Selecione linhas com o mesmo fornecedor'(179) type 'I'.
            wl_erro = 'X'.
            exit.
          endif.
          add 1 to vlinha.
          vlifnr    = wa_saida-lifnr_z.
        endloop.
        if vlinha eq 1.
          if wl_erro ne 'X' .
            refresh tg_adiant.
            clear wg_cadadt.
            loop at it_saida into wa_saida where checkbox = 'X'.
              wg_adiant-lifnr      =  wa_saida-lifnr.
              wg_adiant-name1      =  wa_saida-name1.
              wg_adiant-budat      =  wa_saida-budat.
              wg_adiant-belnr      =  wa_saida-augbl. "wa_saida-belnr.
              "WG_ADIANT-INVOICE    =  WA_SAIDA-INVOICE.
              wg_adiant-invoice    =  wa_saida-nr_solov.
              "IF wa_saida-moeda_pgto = 'USD'.
*---> 08/06/2023 - Migração S4 - JS
*            WG_ADIANT-VLR_PGTO  = WA_SAIDA-DMBE2.
              wg_adiant-vlr_pgto = conv #( wa_saida-dmbe2 ).
*<--- 08/06/2023 - Migração S4 - JS

              "ELSE.
              "  wg_adiant-vlr_pgto = wa_saida-dmbtr.
              "ENDIF.

              wg_adiant-vlr_comp   = 0.
              wg_adiant-vlr_sld    = wg_adiant-vlr_pgto.
              wg_adiant-obj_key    = wa_saida-obj_key.
              wg_adiant-navio      = wa_saida-navio.
              wg_adiant-belnr36    = wa_saida-belnr36.
              wg_adiant-buzei36    = wa_saida-buzei36.
              wg_adiant-augbl      = wa_saida-augbl.
              append wg_adiant to tg_adiant.
              wg_cadadt-lifnr = wa_saida-lifnr.
              wg_cadadt-name1 = wa_saida-name1.
            endloop.
            clear:  wg_atualiza_shdb.
            refresh: tg_invoice, tg_compe.
            " obter INVOICES abertas
            select   * "obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao
            from zfit0036
            into corresponding fields of table it_zfit0036_per
            where status  = ''.
            delete it_zfit0036_per where obj_key+0(1) = 'P' and obj_key+0(2) = 'PB'.

            select *
             from zib_contabil
             into table it_zib_contabil_per
             for all entries in it_zfit0036_per
             where obj_key eq it_zfit0036_per-obj_key
             and   bschl   in ('31','21','29','39')
             and   rg_atualizado eq  'S'
             and   hkont eq vlifnr
             and   bukrs eq p_bukrs.

            if it_zib_contabil_per[] is not initial.
              select zib_contabil_chv~obj_key zib_contabil_chv~belnr zib_contabil_chv~bukrs zib_contabil~hkont
              from zib_contabil_chv
              inner join zib_contabil
              on zib_contabil~obj_key eq zib_contabil_chv~obj_key
              and  zib_contabil~hkont = vlifnr
              into table  it_zib_contabil_chv_per
              for all entries in it_zib_contabil_per
              where zib_contabil_chv~obj_key eq it_zib_contabil_per-obj_key
              and zib_contabil~bschl  in ('31','21','29','39').

              if it_zib_contabil_chv_per[] is not initial.
                select * "bukrs lifnr belnr dmbtr dmbe2
                from bsik
                into corresponding fields of table it_bsik_per
                for all entries in it_zib_contabil_chv_per
                where bukrs eq it_zib_contabil_chv_per-bukrs
                and   lifnr eq it_zib_contabil_chv_per-hkont
                and   belnr eq it_zib_contabil_chv_per-belnr.

                sort: it_zib_contabil_chv_per   by bukrs hkont belnr,
                      it_zfit0036_per           by obj_key.
                loop at it_bsik_per into wa_bsik.
                  wg_invoice-budat      = wa_bsik-budat.
                  wg_invoice-belnr      = wa_bsik-belnr.
                  wg_invoice-vlr_doc    = wa_bsik-wrbtr.
                  wg_invoice-vlr_comp   = 0.
                  wg_invoice-vlr_sld    = wa_bsik-wrbtr.
                  read table it_zib_contabil_chv_per into wa_zib_contabil_chv with key bukrs = wa_bsik-bukrs
                                                                                       belnr = wa_bsik-belnr
                                                                                       hkont = wa_bsik-lifnr binary search.
                  read table it_zfit0036_per into wa_zfit0036 with key obj_key = wa_zib_contabil_chv-obj_key binary search.
                  if sy-subrc = 0.
                    wg_invoice-invoice    = wa_zfit0036-invoice.
                    wg_invoice-navio      = wa_zfit0036-navio.
                    wg_invoice-obj_key    = wa_zfit0036-obj_key.
                  endif.
                  append wg_invoice to tg_invoice.
                endloop.
                sort tg_invoice by vlr_doc.
              endif.
            endif.

            call screen 7000 starting at 060 2
                 ending   at 175 34.

          endif.
        else.
          message 'Selecione um documento por vez'(177) type 'I'.
        endif.
      endif.
    when  '&CRIAR'.

      clear: wg_cadinvo, wg_savesrv.
      wg_cadinvo-waers = 'USD'.
      clear  wl_erro.
      vlinha = 0.
      clear: vforma_pg, vlifnr.
      loop at it_saida into wa_saida where checkbox = 'X'.

        if vlinha gt 0 and vforma_pg  ne wa_saida-forma_pg+0(1).
          message 'Selecione linhas com a mesma forma de pagto'(180) type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
        if vforma_pg = 'C' or vforma_pg = 'A'.
          if vlinha gt 0 and vlifnr  ne wa_saida-lifnr.
            message 'Selecione linhas com o mesmo fornecedor'(179) type 'I'.
            wl_erro = 'X'.
            exit.
          endif.
        endif.

        if wa_saida-icon1 = icon_locked.
          message 'Documento está BLOQUEADO'(181) type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
*
*        if wa_saida-opera is initial and p_bukrs eq '0200'. " US 164319-26-02-2025-#164319-RJF.
*          message 'Enter a value for the operation field' type 'I'.
*          exit.
*        endif.


        add 1 to vlinha.
        wg_cadinvo-bukrs = wa_saida-bukrs.                  "bug 53981
        wg_cadinvo-lifnr = wa_saida-lifnr.
        wg_cadinvo-name1 = wa_saida-name1.
        wg_cadinvo-lnrza = wa_saida-lnrza.
        wg_cadinvo-vlr_pgto = wa_saida-dmbe2.
        vforma_pg = wa_saida-forma_pg+0(1).
        vlifnr    = wa_saida-lifnr.
        if wg_cadinvo-bukrs = '0200'.                       "bug 53981
          wg_cadinvo-waers = wa_saida-waers.
        endif.

*        IF WA_SAIDA-BVTYP IS INITIAL.
*          MESSAGE 'Informe a Seq. Conta para todas as INVOICES selecionadas' TYPE 'I'.
*          WL_ERRO = 'X'.
*          EXIT.
*        ENDIF.
      endloop.
      if vlinha gt 0.
        if wl_erro ne 'X' and vforma_pg eq 'P'.
          refresh tg_conta.
          " Substitui dados bancários pelos novos campos da ZFIT0036
          loop at it_saida into wa_saida where checkbox = 'X'.
          endloop.
          if wa_saida-obj_key is not initial.
            select single *
              from zfit0036
              into wl_zfit0036
              where obj_key = wa_saida-obj_key
              and   belnr   = wa_saida-belnr36
              and   buzei   = wa_saida-buzei36.
            if wl_zfit0036-banka_1 is not initial.
              wg_conta-bvtyp = wl_zfit0036-bvtyp.
              wg_conta-banks = wl_zfit0036-banks_1.
              wg_conta-bankl = wl_zfit0036-bankl_1.
              wg_conta-bankn = wl_zfit0036-bankn_1.
              wg_conta-iban  = wl_zfit0036-iban_1.
              wg_conta-swift = wl_zfit0036-swift_1.
              wg_conta-banka = wl_zfit0036-banka_1.
              append wg_conta to tg_conta.
              wg_conta-bvtyp = wl_zfit0036-bvtyp_2.
              wg_conta-banks = wl_zfit0036-banks_2.
              wg_conta-bankl = wl_zfit0036-bankl_2.
              wg_conta-bankn = wl_zfit0036-bankn_2.
              wg_conta-iban  = wl_zfit0036-iban_2.
              wg_conta-swift = wl_zfit0036-swift_2.
              wg_conta-banka = wl_zfit0036-banka_2.
              append wg_conta to tg_conta.
            endif.
          endif.
          call screen 2000 starting at 070 3
                           ending   at 170 20.
        elseif wl_erro ne 'X' and ( vforma_pg eq 'C' or vforma_pg eq 'A' ).
          refresh tg_pagar.
          refresh tg_receber.
          loop at it_saida into wa_saida where checkbox = 'X'.
            wg_pagar-lifnr      =  wa_saida-lifnr.
            wg_pagar-name1      =  wa_saida-name1.
            wg_pagar-budat      =  wa_saida-budat.
            wg_pagar-belnr      =  wa_saida-belnr.
            wg_pagar-invoice    =  wa_saida-invoice.
*---> 08/06/2023 - Migração S4 - JS
*            WG_PAGAR-VLR_PGTO   = WA_SAIDA-DMBE2.
            wg_pagar-vlr_pgto = conv #( wa_saida-dmbe2 ).
*<--- 08/06/2023 - Migração S4 - JS

            wg_pagar-vlr_comp   = 0.
            wg_pagar-vlr_sld    = wg_pagar-vlr_pgto.
            wg_pagar-obj_key    = wa_saida-obj_key.
            wg_pagar-navio      = wa_saida-navio.
            wg_pagar-belnr36    = wa_saida-belnr36.
            wg_pagar-buzei36    = wa_saida-buzei36.
            append wg_pagar to tg_pagar.
          endloop.

          clear: wg_cadcom, wg_atualiza_shdb.

          refresh: tg_adiant, tg_compe.
          w_cont = 0.
          loop at it_saida into wa_saida where checkbox = 'X'.
            add 1 to w_cont.
          endloop.
          if w_cont ne 1.
            message 'Selecione um documento por vez'(177) type 'I'.
          else.
            if vforma_pg eq 'C'.
              call screen 6000 starting at 060 2
                               ending   at 175 34.
            else.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = wa_saida-lifnr
                importing
                  output = vlifnr.
              select *
                from bsik
                into table @data(it_bsik_)
                where bukrs = @p_bukrs
                and   lifnr = @vlifnr
                and   umsks = 'A'
                and   waers = @wg_cadinvo-waers.
              loop at it_bsik_ into data(wa_bsik_).
                wg_adiant-vlr_comp   = 0.
*---> 08/06/2023 - Migração S4 - JS
*                WG_ADIANT-VLR_PGTO   = WA_BSIK_-WRBTR.
                wg_adiant-vlr_pgto = conv #( wa_bsik_-wrbtr ).
*<--- 08/06/2023 - Migração S4 - JS
                wg_adiant-vlr_sld    = 0.
                wg_adiant-belnr      = wa_bsik_-belnr.
                wg_adiant-navio      = wa_saida-navio.
                wg_adiant-belnr36    = wa_saida-belnr36.
                wg_adiant-buzei36    = wa_saida-buzei36.
                append wg_adiant to tg_adiant.
                clear wg_adiant.
              endloop.
              refresh tg_compe.
              call screen 6500 starting at 060 2
                              ending   at 175 34.
            endif.
          endif.
        endif.
      endif.
    when '&GERAR'.
      clear wg_cadliq.
      if p_bukrs ne '0200'. " US 164319-26-02-2025-#164319-RJF
        wg_cadliq-checkarq = 'X'.
      endif. " US 164319-26-02-2025-#164319-RJF
      clear  wl_erro.

      loop at it_saida into wa_saida where checkbox = 'X'.
        wg_cadliq-moeda = wa_saida-moeda_pgto.              "bug 53981

** US - 76596 - CBRAND - Inicio
        perform verifica_feriado using wa_saida-augdt wa_saida-moeda_pgto '' '4000'.
** US - 76596 - CBRAND - Fim

        if wa_saida-bukrs ne '0200'. " US 164319-26-02-2025-#164319-RJF
          if wa_saida-bvtyp is initial.
            message 'Informe a Seq. Conta para todas as INVOICES selecionadas'(182) type 'I'.
            wl_erro = 'X'.
            exit.
          endif.
        else." US 164319-26-02-2025-#164319-RJF
          wg_cadliq-checkarq = abap_off.   " US 164319-26-02-2025-#164319-RJF
        endif. " US 164319-26-02-2025-#164319-RJF
      endloop.
      if wl_erro ne 'X'.
        wg_cadliq-servidor = 'X'.

*        vseqb = sy-datum+0(4).
*        vseqb = ( vseqb - 2016 ) + 1.

        vnr_range = 1.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vnr_range
          importing
            output = vnr_range.

        call function 'NUMBER_GET_NEXT'
          exporting
            nr_range_nr = vnr_range
            object      = 'ZID_BCO2'
          importing
            number      = vseqb.

        vnumb = vseqb.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vnumb
          importing
            output = vnumb.

        concatenate 'B' vnumb into wg_cadliq-laufi.

        if wa_saida-opera = '08'.
          wg_cadliq-hbkid  = wa_saida-hbkid.
        endif.
        call screen 3000 starting at 050 3
                         ending   at 165 13.
      endif.
    when '&BAIXA'.
      perform carrega_baixas.
    when '&ESTORNAR'.
      w_cont = 0.
      clear wl_erro.
      loop at it_saida into wa_saida where checkbox = 'X'.
        add 1 to w_cont.

        if wa_saida-augbl is initial and  wa_saida-obj_key+0(2) ne 'AC'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.

      if wl_erro = 'X'.
        message 'Não existe compensação a ser estornada'(183) type 'I'.
      elseif w_cont = 1.
        clear wg_cadest.
        wg_cadest-augbl   = wa_saida-augbl.
        wg_cadest-invoice = p_invo-low.
        call screen 5000 starting at 050 3
                         ending   at 115 7.
      else.
        message 'Informe apenas um documento para estornar de cada vez'(184) type 'I'.
      endif.
    when '&ESTOR_AD'.
      clear wg_cadest.
      w_cont = 0.
      clear wl_erro.
      loop at it_saida into wa_saida where checkbox = 'X'.
        add 1 to w_cont.
        select single *
          from zfit0036
          into @data(_z36)
          where obj_key = @wa_saida-obj_key
            and   belnr = @wa_saida-belnr36
            and   buzei = @wa_saida-buzei36.

        if _z36-status_arq_inv ne 'A'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        message 'Este documento não foi compensado com adiantamento'(185) type 'I'.
      elseif w_cont <= 1.
        wg_cadest-augbl = wa_saida-belnr.
        if w_cont = 0.
          wg_cadest-flag = 'X'.
        endif.
        call screen 5000 starting at 050 3
                         ending   at 115 7.
      else.
        message 'Informe apenas um documento para estornar de cada vez'(184) type 'I'.
      endif.
    when others.

  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3521   text
*----------------------------------------------------------------------*
form zf_alv_header .
  data:   wl_data(10),
          wl_hora(8),
          wl_linha(60),
          wl_text type sdydo_text_element.

  if vstatus = ''.
    wl_text = 'Em Aberto'(119).
  elseif vstatus = 'A'.
    wl_text = 'Aguardando Aprovação'(120).
  elseif vstatus = 'L'.
    wl_text = 'Liberadas'(121).
  elseif vstatus = 'P'.
    wl_text = 'Pagas'(122).
  endif.

  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.


  concatenate  'Empresa:'(186) p_bukrs
          into wl_linha separated by space.
  wl_text = wl_linha.
  call method obj_dyndoc_id->new_line.

  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  if p_forn is not initial.
    if p_forn-high is initial.
      concatenate 'Fornecedor  :'(187) p_forn-low
       into wl_linha separated by space.
    else.
      concatenate 'Fornecedor  :'(187) p_forn-low 'à'(189) p_forn-high
      into wl_linha separated by space.
    endif.
    wl_text = wl_linha.
    call method obj_dyndoc_id->new_line.

    call method obj_dyndoc_id->add_text
      exporting
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  endif.


  if p_invo is not initial.
    if p_invo-high is initial.
      concatenate 'Invoice  :'(188) p_invo-low
      into wl_linha separated by space.
    else.
      concatenate 'Invoice  :'(188)  p_invo-low 'à'(189) p_invo-high
      into wl_linha separated by space.
    endif.
    wl_text = wl_linha.
    call method obj_dyndoc_id->new_line.

    call method obj_dyndoc_id->add_text
      exporting
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  endif.

  if p_data is not initial.
    concatenate p_data-low+6(2) p_data-low+4(2) p_data-low+0(4) into  wl_data separated by '.'.
    if p_data-high is initial.
      concatenate 'Data Lançamento  :'(190) wl_data
      into wl_linha separated by space.
    else.
      concatenate 'Data Lançamento  :'(190) wl_data  into wl_linha separated by space.
      concatenate p_data-high+6(2) p_data-high+4(2) p_data-high+0(4) into  wl_data separated by '.'.
      concatenate wl_linha 'à'(189) wl_data  into wl_linha separated by space.
    endif.
    wl_text = wl_linha.
    call method obj_dyndoc_id->new_line.

    call method obj_dyndoc_id->add_text
      exporting
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  endif.



endform.                    " F_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  STATUS_4000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_4000 output.
  set pf-status 'Z001'.
*  CALL METHOD CL_GUI_CFW=>DISPATCH.
  set titlebar '4000'.
endmodule.                 " STATUS_4000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_4000 input.
  case ok-code.
    when 'BT_SLD'.
      call method grid1->refresh_table_display
        exporting
          is_stable = wa_stable.

      call method grid2->refresh_table_display
        exporting
          is_stable = wa_stable.
    when 'GERAR'.
      if wg_atualiza = 'X'.
        clear: wl_erro, wg_atualiza.
        perform verifica_erros.
** US - 76596 - CBRAND - Inicio
        if wg_cadbai-dt_pgt <> wg_cadbai-dt_lct.
          perform verifica_feriado using wg_cadbai-dt_pgt wg_cadbai-moeda wg_conta-banks '4000'.
          perform verifica_feriado using wg_cadbai-dt_lct wg_cadbai-moeda wg_conta-banks '4000'. "Verifica também data de lançamento
        else.
          perform verifica_feriado using wg_cadbai-dt_pgt wg_cadbai-moeda wg_conta-banks '4000'.
        endif.
** US - 76596 - CBRAND - Fim

        if wl_erro = 'X'.
          exit.
        endif.
        wg_atualiza_shdb = 'X'.
        clear vnum. " Numero do lote unico para processamento
        " GRAVA DADOS BANCÁRIOS.
        loop at tg_externo into wg_externo.
          if wg_externo-tx_cambio gt 0.
            loop at tg_conta into wg_conta.
              if sy-tabix = 1.
                update zfit0036 set bvtyp   = wg_conta-bvtyp
                                    swift_1 = wg_conta-swift
                                    banks_1 = wg_conta-banks
                                    bankl_1 = wg_conta-bankl
                                    banka_1 = wg_conta-banka
                                    bankn_1 = wg_conta-bankn
                                    iban_1  = wg_conta-iban
                where obj_key = wg_externo-obj_key
                and   belnr = wg_externo-belnr36
                and   buzei = wg_externo-buzei36.
              else.
                update zfit0036 set bvtyp_2   = wg_conta-bvtyp
                                   swift_2 = wg_conta-swift
                                   banks_2 = wg_conta-banks
                                   bankl_2 = wg_conta-bankl
                                   banka_2 = wg_conta-banka
                                   bankn_2 =  '' "WG_CONTA-BANKN
                                   iban_2  = wg_conta-iban
                where obj_key = wg_externo-obj_key
                and   belnr = wg_externo-belnr36
                and   buzei = wg_externo-buzei36.
              endif.
            endloop.
          endif.
        endloop.
        perform rot_baixa_ext.
        if wl_erro ne 'X'.
          perform rot_baixa_bra.
          if wl_erro ne 'X'.
            message 'Processamento realizado com sucesso.'(191) type 'I'.
            set screen 0.
          else.
            message 'Erro no processamento, empresa BRASIL.'(192) type 'I'.
            exit.
          endif.
        else.
          message 'Erro no processamento, empresa EXTERNA.'(193) type 'I'.
          exit.
        endif.
        call method grid2->set_ready_for_input
          exporting
            i_ready_for_input = 1.
      else.
        message 'Não foram validados valores de baixa.'(194) type 'I'.
      endif.

    when 'ATU_CHK'.
      call method grid2->get_selected_rows
        importing
          et_index_rows = tl_index_rows.

      loop at tl_index_rows into wl_index_rows.
        if wl_index_rows-rowtype is not initial.
          continue.
        endif.
        read table tg_brasil into wg_brasil index wl_index_rows-index.
        if wg_brasil-checkbox = ''.
          wg_brasil-checkbox = 'X'.
        else.
          wg_brasil-checkbox = ' '.
        endif.
        modify tg_brasil from wg_brasil index wl_index_rows-index transporting  checkbox.
      endloop.

    when 'ATU_LCT'.
      clear wl_erro.
      data: t_vlr_bai     type bsid_view-dmbe2 value 0,
            t_vlr_res     type anlp-nafaz,
            t_vlr_clc     type anlp-nafaz,
            vtx_cambio    type zfit0036-tx_cambio,
            vdt_cred      type zfit0036-dt_pgto,
            vdt_budat     type zfit0036-dt_pgto,
            wmensagem(50),
            wlinha(3).

      perform verifica_erros.
      if wl_erro = 'X'.
        exit.
      endif.

      xvlrpg = 0.
      " Pega Primeira taxa de cambio informada e comprar com as outras
      clear: vtx_cambio, vdt_cred.
      loop at tg_externo into wg_externo.
        if wg_externo-tx_cambio gt 0.
          vtx_cambio  = wg_externo-tx_cambio.
          vdt_cred    = wg_externo-dt_cred.
          exit.
        endif.
      endloop.

      loop at tg_externo into wg_externo.
        wlinha = sy-tabix.
        if wg_externo-rg_atualizado = 'T'.
          add wg_externo-vlr_cre to xvlrpg.
        else.
          add wg_externo-vlr_pgt to xvlrpg.
        endif.
        if wg_externo-tx_cambio gt 0.
          if vtx_cambio  ne wg_externo-tx_cambio or
             vdt_cred    ne wg_externo-dt_cred.
            concatenate 'Tx Câmbio/Data devem ser iguais, p/ único processamento'(195) '' into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          endif.
        endif.
      endloop.

      if wl_erro = 'X'.
        exit.
      endif.

      if xvlrpg le 0.
        concatenate 'Informe o Valor Pagamento/Crédito'(196) '' into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      t_vlr_bai = 0.
      loop at tg_brasil into wg_brasil where checkbox = 'X'.
        if vtx_cambio le 0.
          wg_brasil-vlr_bai = 0.
          wg_brasil-vlr_sld = 0.
          wg_brasil-checkbox = ''.
          modify tg_brasil from wg_brasil index sy-tabix transporting vlr_bai vlr_sld checkbox.
        endif.
        if wg_brasil-budat gt vdt_cred .
          concatenate 'Documento '(197) wg_brasil-belnr ' está maior que a data de crédito'(198) into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.

        if wg_brasil-numero_due is not initial and wg_brasil-situacao_due ne '70'.
          concatenate 'DUE '(199) wg_brasil-numero_due ' não averbada'(200) into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.

        add wg_brasil-vlr_bai to t_vlr_bai .
      endloop.
      if t_vlr_bai gt xvlrpg.
*        CONCATENATE 'Valor da Baixa maior que a INVOICE ' '' INTO WMENSAGEM  SEPARATED BY SPACE.
*        MESSAGE WMENSAGEM TYPE 'I'.
*        WL_ERRO = 'X'.
      elseif vtx_cambio gt 0 and t_vlr_bai eq 0.
        concatenate 'Informe o valor de baixa INVOICE '(201) '' into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
      endif.

      if wl_erro = 'X'.
        exit.
      endif.

      call method grid2->set_ready_for_input
        exporting
          i_ready_for_input = 0.

      wg_atualiza = 'X'.
      call method grid2->refresh_table_display
        exporting
          is_stable = wa_stable.

    when 'SAIR'.
      clear vsub_tela.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_4000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.
  data: event       type cntl_simple_event,
        tl_filter   type lvc_t_filt,
        wl_filter   type lvc_s_filt,
        tl_function type ui_functions,
        wl_function like tl_function with header line.

  clear wa_layout.
  gs_variant_bra-report = sy-repid. "Enable users save own LAYOUTs
  clear wa_layout.

  wa_layout-cwidth_opt  = c_x.
  wa_layout-zebra       = c_x.
  wa_layout-no_rowmark  = ' '. "C_X.
  wa_layout-sel_mode    = 'B'.
  wa_layout-box_fname   = 'MARK'.
  wa_layout-grid_title  = ' '.

  wa_stable-row         = c_x.
  wa_stable-col         = c_x.


  if g_custom_container is initial.
    create object g_custom_container
      exporting
        container_name = g_cc_ext.

    create object splitter
      exporting
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    call method splitter->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = container_1.

    create object grid1
      exporting
        i_parent = container_1.


    perform montar_layout_ext.

    create object obg_toolbar
      exporting
        io_alv_grid = grid1.

*
*** Register event handler
    vsub_tela = '4100'.
    set handler obg_toolbar->on_toolbar for grid1.
    set handler obg_toolbar->handle_user_command for grid1.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = ''.


    call method grid1->set_table_for_first_display
      exporting
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      changing
*       IT_FILTER            = TL_FILTER
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_externo[].

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>catch_hotspot_41           for grid1,
              lcl_event_handler=>on_data_changed_finished_4 for grid1,
              lcl_event_handler=>on_data_changed_4          for grid1.

*    posiciona spliter na altura x
    call method splitter->set_row_height
      exporting
        id     = 1
        height = 100.

  else.
    perform montar_layout_ext.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  "GRID2
  if obg_conteiner_bra is initial.
    create object obg_conteiner_bra
      exporting
        container_name = g_cc_bra.


    create object grid2
      exporting
        i_parent = obg_conteiner_bra.


    perform montar_layout_bra.
*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = GRID1.
*
**
**** Register event handler
*    VSUB_TELA = '4200'.
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID2.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID2.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

    wa_layout-no_toolbar = space.
    wa_layout-grid_title = ''.

    wg_save = 'X'.
    gs_variant_bra-report = sy-repid.
    call method grid2->set_table_for_first_display
      exporting
        is_variant           = gs_variant_bra
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_save               = wg_save
        i_default            = 'X'
      changing
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_brasil[].

    call method grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
     lcl_event_handler=>catch_hotspot_4 for grid2,
     lcl_event_handler=>on_data_changed_finished_5 for grid2,
     lcl_event_handler=>on_data_changed_5 for grid2.

    call method grid2->set_ready_for_input
      exporting
        i_ready_for_input = 1.

  else.
    perform montar_layout_bra.
    call method grid2->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
  clear vsub_tela.

endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_EXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_ext .
  refresh t_fieldcatalog.
  data wflag_externo(1).
  loop at tg_externo into wg_externo.
    if wg_externo-rg_atualizado = 'T'.
      wflag_externo = 'X'.
      exit.
    endif.
  endloop.
  perform montar_estrutura using:
        1 'ZIB_CONTABIL' 'BELNR'        'TG_EXTERNO' 'BELNR'            'Documento'(202)        '11' ' ' ' ' ' ',
        2 'ZFIT0036'     'INVOICE'      'TG_EXTERNO' 'INVOICE'          'INVOICE'(203)          '11' ' ' ' ' ' ',
        2 'ZFIT0036'     'INVOICE'      'TG_EXTERNO' 'NAVIO'            'NAVIO'(204)            '19' ' ' ' ' ' ',
        3 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_DOC'          'Valor Documento'(205)  '15' ' ' 'X' ' '.

  if wflag_externo = 'X'.
    perform montar_estrutura using:
          4 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_PGT'          'Vlr.Pagamento'(206)    '15' ' ' 'X' ' ',
          5 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_CRE'          'Vlr.Crédito'(207)      '15' 'X' 'X' ' '.
  else.
    perform montar_estrutura using:
          4 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_PGT'          'Vlr.Pagamento'(206)    '15' 'X' 'X' ' ',
          5 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_CRE'          'Vlr.Crédito'(207)      '15' ' ' 'X' ' '.
  endif.
  perform montar_estrutura using:
  6 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_CRE_B'        'Vlr.Cré Baixa'(208)    '15' ' ' ' ' ' ',
  7 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_SLD'          'Saldo Residual'(209)   '15' ' ' 'X' ' ',
  8 'ZFIT0036'     'TX_CAMBIO'    'TG_EXTERNO' 'TX_CAMBIO'        'Tx.Câmbio'(210)        '12' 'X' ' ' ' ',
  9 'ZFIT0036'     'DT_PGTO '     'TG_EXTERNO' 'DT_CRED'          'Dt.Credito'(211)       '15' 'X' ' ' ' ',
 10 'BSID'         'DMBE2'        'TG_EXTERNO' 'VLR_REAL'         'Vlr. Cré Real'(212)    '15' ' ' 'X' ' '.

endform.                    " MONTAR_LAYOUT_EXT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_edit)
                            value(p_sum)
                            value(p_emphasize).

  clear w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  if p_outputlen is not initial.
    w_fieldcatalog-outputlen      = p_outputlen.
  endif.

  if p_tabname ne 'TG_PAGAR' and p_tabname ne 'TG_RECEBER' and p_tabname ne 'TG_CRIAADT' and  p_tabname ne 'IT_ARQ_BANCOS'.
    if p_field = 'VBELV' or
      p_field = 'VBELN' or
      p_field = 'BELNR'.
      w_fieldcatalog-hotspot = 'X'.
    endif.
  endif.
  if p_field = 'CHECKBOX'.
    w_fieldcatalog-checkbox = 'X'.
  endif.

  if p_field eq 'SAKNR' or p_field eq 'BSCHL' or p_field eq 'UMSKZ' or p_field eq 'SAKNR_BA'  or p_field eq 'KOSTL' or p_field eq 'BANKL' or p_field eq 'GSBER'.
    w_fieldcatalog-f4availabl = c_x.
  endif.

  if p_tabname eq 'TG_CRIAADT'.
    if p_field eq 'ICON'.
      w_fieldcatalog-icon      = c_x.
      w_fieldcatalog-hotspot   = c_x.
    endif.
  endif.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  append w_fieldcatalog to t_fieldcatalog.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_BRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_bra .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 '  '          ' '        'TG_BRASIL' 'CHECKBOX'        'Chk'(213)             '05' 'X' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'VBELV'           'Ordem Venda'(214)     '10' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'VBELN'           'Fatura'(215)          '10' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'BELNR'           'Doc.Contabil'(216)    '10' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'BUZEI'           'Linha'(217)           '05' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'BUDAT'           'Data'(218)            '08' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'NR_RE'           'RE'(219)              '10' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'NUMERO_DUE'      'DUE'(199)             '10' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'SITUACAO_DUE'    'Sit.DUE'(220)         '08' ' ' ' ' ' ',
        1 'BSID'        'DMBE2'    'TG_BRASIL' 'VLR_USD'         'Valor US$'(221)       '15' ' ' 'X' ' ',
        1 'BSID'        'DMBE2'    'TG_BRASIL' 'VLR_BAI'         'Valor Baixa US$'(222) '15' 'X' 'X' ' ',
        1 ' '           ' '        'TG_BRASIL' 'VLR_SLD'         'Saldo Residual'(209)  '15' ' ' 'X' ' ',
        1 ' '           ' '        'TG_BRASIL' 'NAVIO'           'Navio'(010)           '20' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'X_PAIS'          'País'(050)            '50' ' ' ' ' ' ',
        1 ' '           ' '        'TG_BRASIL' 'DS_PORTO'        'Des.Porto'(050)       '50' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_BRA

*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_tipo input.
  data: tl_return_tab_t type table of ddshretval with header line,
        tl_dselc_t      type table of dselc      with header line.

  data: begin of tl_tipo occurs 0,
          tipo type zfit0036-forma_pg,
          text type t012t-text1,
        end of tl_tipo.

  refresh tl_tipo.
  tl_tipo-tipo = 'C'.
  tl_tipo-text = 'Compensação'(223).
  append tl_tipo.

  tl_tipo-tipo = 'P'.
  tl_tipo-text = 'Pagamento'(224).
  append tl_tipo.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'TIPO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0036-FORMA_PG'
      value_org       = 'S'
    tables
      value_tab       = tl_tipo
      return_tab      = tl_return_tab_t
      dynpfld_mapping = tl_dselc_t.
endmodule.                 " SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_oper input.
  data: tl_return_tab_o type table of ddshretval with header line,
        tl_dselc_o      type table of dselc      with header line.

  data: begin of tl_opera occurs 0,
          opera type zfit0042-tp_operacao,
          text  type t012t-text1,
        end of tl_opera.

  refresh tl_opera.
  tl_opera-opera = '01'.
  tl_opera-text  = 'Cambio'(225).
  append tl_opera.

  tl_opera-opera = '02'.
  tl_opera-text  = 'Captações'(226).
  append tl_opera.

  tl_opera-opera = '03'.
  tl_opera-text  = 'Baixa PA'(227).
  append tl_opera.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'OPERA'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0042-TP_OPERACAO'
      value_org       = 'S'
    tables
      value_tab       = tl_opera
      return_tab      = tl_return_tab_o
      dynpfld_mapping = tl_dselc_o.
endmodule.                 " SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*&      Form  CARREGA_BAIXAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form carrega_baixas .
  refresh: tg_externo, tg_brasil.

  data: vstatus_saida_ant(1),
        vtotal_pgt           type zfit0036-vlr_pgto,
        vtotal_cre           type zfit0036-vlr_pgto,
        vbelnr2              type zfit0042-belnr2,
        wl_zfit0036          type zfit0036,
        v_vbeln              type bsid_view-vbeln.

  clear: wg_atualiza,wg_atualiza_shdb, wg_cadbai, w_flag.
  w_cont = 0.
  loop at it_saida into wa_saida where checkbox = 'X'.
    if wa_saida-icon1 eq icon_incomplete or
       wa_saida-icon1 eq icon_message_warning or
       wa_saida-rg_atualizado = 'T' .

      if vstatus_saida_ant ne wa_saida-status and  w_cont  gt 0.
        vstatus_saida_ant = 'Z'.
        exit.
      endif.
      vstatus_saida_ant = wa_saida-status.

      add 1 to w_cont.
    endif.
  endloop.
  if vstatus_saida_ant = 'Z'.
    message 'Selecione Linhas com a mesma cor'(228) type 'I'.
    exit.
  endif.
  if w_cont gt 0.
    select *
      from zfit0042
      into table it_zfit0042_cre
      for all entries in it_saida
      where lote  = it_saida-lote.
    sort it_zfit0042_cre by lote belnr belnr2.
    wg_cadbai-vlr_pgto = 0.
    loop at it_saida into wa_saida where checkbox = 'X'.
      if wa_saida-icon1 eq icon_incomplete or
        wa_saida-icon1 eq icon_message_warning or
        wa_saida-rg_atualizado = 'T' .

        wg_externo-lote     = wa_saida-lote.
        wg_externo-belnr     = wa_saida-belnr.
        wg_externo-invoice   = wa_saida-invoice.
        wg_externo-vlr_doc   = wa_saida-dmbe2.
        "
        if wa_saida-rg_atualizado = 'T'.
*          "Desabilita entrada
*          REFRESH: STYLE.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VLR_CRE'.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
*          DELETE WG_EXTERNO-STYLE WHERE FIELDNAME EQ 'VLR_CRE'.
*          INSERT  WA_STYLE INTO TABLE STYLE .
*          WG_EXTERNO-STYLE[] = STYLE[].
        else.
          add wa_saida-dmbe2 to wg_cadbai-vlr_pgto.
        endif.
        "
        vtotal_pgt = 0.
        vtotal_cre = 0.
        if wa_saida-lote gt 0.
          select  * "obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado
           from zfit0036
           into corresponding fields of table it_zfit0036_pgt
           where obj_key = wa_saida-obj_key
           and   lote    = wa_saida-lote
           and   belnr   = wa_saida-belnr36
           and   buzei   = wa_saida-buzei36
           and   status  in  ('L','C','A','P').
          loop at it_zfit0036_pgt into wa_zfit0036.
            add wa_zfit0036-vlr_pgto to vtotal_pgt.
          endloop.

          loop at it_zfit0042_cre into wa_zfit0042 where lote = wa_saida-lote.
            if wa_zfit0042-lote = wa_saida-lote.
              add wa_zfit0042-dmbe2 to vtotal_cre.
            endif.
          endloop.
        endif.
        wg_externo-vlr_pgt   = vtotal_pgt.
        wg_externo-vlr_cre   = 0.
        wg_externo-vlr_cre_b = vtotal_cre.
        if wa_saida-rg_atualizado = 'T'.
          wg_externo-vlr_sld   = vtotal_pgt - vtotal_cre.
        else.
          wg_externo-vlr_sld   = wa_saida-dmbe2.
        endif.

        wg_externo-obj_key   = wa_saida-obj_key.
        wg_externo-navio     = wa_saida-navio.
        wg_externo-bukrs     = wa_saida-bukrs.
        wg_externo-budat     = wa_saida-budat.
        wg_externo-belnr36   = wa_saida-belnr36.
        wg_externo-buzei36   = wa_saida-buzei36.
        wg_externo-status    = wa_saida-status.
        wg_externo-rg_atualizado    = wa_saida-rg_atualizado.
        wg_externo-bvtyp     = wa_saida-bvtyp.
        wg_externo-lnrza     = wa_saida-lnrza.
        wg_externo-lifnr     = wa_saida-lifnr.
        wg_externo-line_color     = wa_saida-line_color.
        if wg_externo-lifnr = 101.
          wg_externo-bukrs_bra = '0001'.
        elseif wg_externo-lifnr = 1501.
          wg_externo-bukrs_bra = '0015'.
        elseif wg_externo-lifnr = 1801.
          wg_externo-bukrs_bra = '0018'.
        elseif wg_externo-lifnr = 5001.
          wg_externo-bukrs_bra = '0050'.
        endif.
        append wg_externo to tg_externo .
        vstatus_saida_ant = wa_saida-status.
        wg_cadbai-dt_pgt  = wa_saida-augdt.
        wg_cadbai-opera   = wa_saida-opera.
        wg_cadbai-tipo    = wa_saida-forma_pg+0(1).
        wg_cadbai-moeda   = wa_saida-moeda_pgto.
        wg_cadbai-status  = wa_saida-status.
        wg_cadbai-observacao = wa_saida-observacao.
        wg_cadbai-referencia = wa_saida-referencia.
        wg_cadbai-rg_atualizado  = wa_saida-rg_atualizado.
      endif.
    endloop.
    "
    if wg_cadbai-rg_atualizado eq 'T'.
      tg_externo_aux2[] = tg_externo[].
      sort tg_externo_aux by lote.
      delete adjacent duplicates from tg_externo_aux2 comparing lote.
      loop at tg_externo_aux2 into wg_externo.
        add wg_externo-vlr_sld to  wg_cadbai-vlr_pgto.
      endloop.
    endif.

    refresh tg_conta.
    " Substitui dados bancários pelos novos campos da ZFIT0036
    if tg_externo[] is not initial.
      read table tg_externo into wg_externo index 1.
      select single *
        from zfit0036
        into wl_zfit0036
        where obj_key = wg_externo-obj_key
        and   belnr   = wg_externo-belnr36
        and   buzei   = wg_externo-buzei36.
      if wl_zfit0036-banka_1 is not initial.
        wg_conta-bvtyp = wl_zfit0036-bvtyp.
        wg_conta-banks = wl_zfit0036-banks_1.
        wg_conta-bankl = wl_zfit0036-bankl_1.
        wg_conta-bankn = wl_zfit0036-bankn_1.
        wg_conta-iban  = wl_zfit0036-iban_1.
        wg_conta-swift = wl_zfit0036-swift_1.
        wg_conta-banka = wl_zfit0036-banka_1.
        append wg_conta to tg_conta.
        wg_conta-bvtyp = wl_zfit0036-bvtyp_2.
        wg_conta-banks = wl_zfit0036-banks_2.
        wg_conta-bankl = wl_zfit0036-bankl_2.
        wg_conta-bankn = wl_zfit0036-bankn_2.
        wg_conta-iban  = wl_zfit0036-iban_2.
        wg_conta-swift = wl_zfit0036-swift_2.
        wg_conta-banka = wl_zfit0036-banka_2.
        append wg_conta to tg_conta.
      endif.
    endif.

    data: it_znom_transporte type table of znom_transporte,
          wa_znom_transporte type          znom_transporte.

    "BRASIL (Novo) 02.10.2013

    select  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei waers budat blart augbl augdt umskz xblnr shkzg vbeln
             from bsid
             into table it_bsid
             for all entries in tg_externo
             where bukrs eq tg_externo-bukrs_bra
             and   kunnr eq  '0000300279'
             and   umsks eq  ''
             and   blart ne  'VC'.


    loop at it_bsid into wa_bsid.
      if wa_bsid-vbeln is initial.
        select single vbeln
          into v_vbeln
          from bsid
           where bukrs = wa_bsid-bukrs
           and   belnr = wa_bsid-belnr
           and   gjahr = wa_bsid-gjahr
           and   vbeln ne ''.

        if sy-subrc = 0.
          select single vbeln vbelv
            from vbfa
            into wa_vbfa
            where vbeln  eq  v_vbeln
            and vbtyp_n  = 'M'
            and vbtyp_v  = 'C'.
          if sy-subrc = 0.
            wa_bsid-vbeln =  v_vbeln.
            modify it_bsid from wa_bsid index sy-tabix transporting vbeln.
          else.
            select single *
               from bkpf
               into wa_bkpf_2
               where bukrs = wa_bsid-bukrs
               and   belnr = wa_bsid-belnr
               and   gjahr = wa_bsid-gjahr.

            wa_bsid-vbeln = wa_bkpf_2-awkey.

            select single vbeln
               from vbrk
               into v_vbeln
               where vbeln  eq  wa_bsid-vbeln.
            if sy-subrc = 0.
              modify it_bsid from wa_bsid index sy-tabix transporting vbeln.
            endif.
          endif.

        endif.
      endif.

    endloop.

    if it_bsid[] is not initial.
      " Selecionar anteriores
      select *
        from zfit0042
        into table it_zfit0042
        for all entries in it_bsid
        where bukrs  eq it_bsid-bukrs
        and   belnr eq it_bsid-belnr
        and   buzei eq it_bsid-buzei.

      " Selecionar anteriores
      select *
        from zfit0042
        into table it_zfit0042_2
        for all entries in it_bsid
        where bukrs  eq it_bsid-bukrs
        and   belnr2 eq it_bsid-belnr
        and   buzei2 eq it_bsid-buzei.

      " Selecionar  BKPF
      select   bukrs gjahr awkey belnr blart budat bldat xblnr waers kursf
              from bkpf
              into table it_bkpf
              for all entries in it_bsid
              where bukrs eq  it_bsid-bukrs
              and   belnr eq  it_bsid-belnr
              and   gjahr eq  it_bsid-gjahr.

      "Selecionar OV
      select vbeln vbelv
           from vbfa
           into table it_vbfa
           for all entries in it_bsid
           where vbeln  eq  it_bsid-vbeln
           and vbtyp_n  = 'M'
           and vbtyp_v  = 'C'.

      if it_vbfa[] is not initial.
        "Selecionar REMESSA
        select vbeln vbelv
             from vbfa
             into table it_vbfa_2
             for all entries in it_vbfa
             where vbeln  eq  it_vbfa-vbeln "Fatura
             and vbtyp_n  = 'M'
             and vbtyp_v  = 'J'.

        if it_vbfa_2[] is not initial.
          "Selecionar RE
          select numero_due id_due nr_registro_expo id_nomeacao_tran vbeln
             from zdoc_exp
             into table it_zdoc_exp
             for all entries in it_vbfa_2
             where vbeln eq it_vbfa_2-vbelv.

          if it_zdoc_exp[] is not initial.
            "Selecionar NAVIO
            select *
              from znom_transporte
              into table it_znom_transporte
              for all entries in it_zdoc_exp
              where id_nomeacao_tran  = it_zdoc_exp-id_nomeacao_tran.

            select id_due numero_due situacao_due
              from zsdt0170
              into table it_due
              for all entries in it_zdoc_exp
              where id_due     = it_zdoc_exp-id_due
              and   numero_due = it_zdoc_exp-numero_due
              and   loekz      = ' '.
          endif.
        endif.
      endif.

      sort: it_vbfa   by vbeln,
            it_vbfa_2 by vbeln,
            it_bkpf   by bukrs belnr gjahr,
            it_zdoc_exp by vbeln,
            it_znom_transporte by id_nomeacao_tran,
            it_zfit0042 by bukrs belnr buzei,
            it_zfit0042_2 by bukrs belnr2 buzei2,
            it_zfit0041 by nr_re,
            it_due      by id_due numero_due.

      loop at it_bsid into wa_bsid.
        read table it_zfit0042 into wa_zfit0042 with key bukrs = wa_bsid-bukrs
                                                         belnr = wa_bsid-belnr
                                                         buzei = wa_bsid-buzei binary search.
        if sy-subrc = 0.
          continue.
        endif.

        read table it_zfit0042_2 into wa_zfit0042 with key bukrs = wa_bsid-bukrs
                                                           belnr2 = wa_bsid-belnr
                                                           buzei2 = wa_bsid-buzei binary search.
        if sy-subrc = 0.
          continue.
        endif.

        clear wa_vbfa.

        read table it_vbfa into wa_vbfa with key vbeln = wa_bsid-vbeln binary search.
        if sy-subrc = 0.
          wg_brasil-vbelv    = wa_vbfa-vbelv. " Ordem de Venda
          wg_brasil-vbelv    = wa_vbfa-vbelv. " Ordem de Venda
          wg_brasil-vbeln    = wa_vbfa-vbeln. " Fatura
        else.
          clear: wg_brasil-vbelv,
                 wg_brasil-vbeln,
                 wa_vbfa.
        endif.
        wg_brasil-bukrs    = wa_bsid-bukrs. " Bukrs
        wg_brasil-budat    = wa_bsid-budat. "
        wg_brasil-kunnr    = wa_bsid-kunnr. "
        wg_brasil-belnr    = wa_bsid-belnr. " doc. contabil
        wg_brasil-belnr2   = wa_bsid-belnr. " doc. contabil
        wg_brasil-buzei    = wa_bsid-buzei. " linha doc. contabil
        wg_brasil-buzei2   = wa_bsid-buzei. " linha doc. contabil
        wg_brasil-gjahr    = wa_bsid-gjahr. "
        wg_brasil-gjahr2   = wa_bsid-gjahr. "
        wg_brasil-vlr_usd  = wa_bsid-dmbe2.


        if wa_bsid-vbeln is not initial.
          read table it_vbfa_2 into wa_vbfa_2 with key vbeln = wa_bsid-vbeln  binary search.
          if sy-subrc = 0.
            read table it_zdoc_exp into wa_zdoc_exp  with key vbeln = wa_vbfa_2-vbelv.
            if sy-subrc = 0.
              wg_brasil-nr_re        = wa_zdoc_exp-nr_registro_expo.
              wg_brasil-numero_due   = wa_zdoc_exp-numero_due.

              if wg_brasil-numero_due is not initial.
                read table it_due into wa_due with key id_due = wa_zdoc_exp-id_due binary search.
                if sy-subrc = 0.
                  wg_brasil-situacao_due = wa_due-situacao_due.
                endif.
              endif.

              read table it_znom_transporte into wa_znom_transporte with key id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran binary search.
              if sy-subrc = 0.
                wg_brasil-navio    = wa_znom_transporte-ds_nome_transpor.
                wg_brasil-ds_porto = wa_znom_transporte-ds_porto.
              endif.
            endif.
          endif.
        endif.


        read table it_bkpf into wa_bkpf with key bukrs = wa_bsid-bukrs
                                                 belnr = wa_bsid-belnr
                                                 gjahr = wa_bsid-gjahr binary search.
        if sy-subrc = 0.
          wg_brasil-bldat    =  wa_bkpf-bldat.
          wg_brasil-blart    =  wa_bkpf-blart.
          wg_brasil-bukrs    =  wa_bkpf-bukrs.
          wg_brasil-budat    =  wa_bkpf-budat.
          wg_brasil-waers    =  wa_bkpf-waers.
          wg_brasil-kursf    =  wa_bkpf-kursf.
          wg_brasil-xblnr    =  wa_bkpf-xblnr.
        endif.
        append wg_brasil to tg_brasil.
        clear wg_brasil.
      endloop.

      data: xachou(1),
            xlimite type i.
      " Reprocessar abertos parciais
      loop at tg_brasil into wg_brasil.
        if wg_brasil-vbeln is initial.
          tabix = sy-tabix.
          clear xachou.
          xlimite = 0.
          while xachou = ''.
            add 1 to xlimite.
            if xlimite gt 10.
              xachou = 'X'.
            endif.
            select single *
              from bsad
              into wa_bsad_brasil2
              where bukrs   = wg_brasil-bukrs
              and   augbl   = wg_brasil-belnr
              and   bsad~augbl  ne bsad~belnr
              and   bsad~vbeln  ne ''.
            if sy-subrc ne 0.
              select *
               from bsad
               into table @it_bsad_brasil2
               where bukrs   = @wg_brasil-bukrs
               and   augbl   = @wg_brasil-belnr
               and   bsad~augbl  ne bsad~belnr.
              delete it_bsad_brasil2 where belnr+0(1) = '1'.
              if it_bsad_brasil2[] is not initial.
                read table  it_bsad_brasil2 into wa_bsad_brasil2 index 1.
                wg_brasil-belnr  = wa_bsad_brasil2-belnr. "PEGA O PROXIMO BELNR
                continue.
              else.
                xachou = 'X'.
                continue.
              endif.
            endif.

            if sy-subrc = 0.
              if wa_bsad_brasil2-vbeln is initial. "fatura em branco, busca na referencia
                select single vbeln
                    into v_vbeln
                    from bsad
                    where bukrs = wa_bsad_brasil2-bukrs
                    and   belnr = wa_bsad_brasil2-belnr
                    and   gjahr = wa_bsad_brasil2-gjahr
                    and   vbeln ne ''.
                if sy-subrc = 0.
                  wa_bsad_brasil2-vbeln = v_vbeln.
                endif.
                if wa_bsad_brasil2-vbeln is initial.
                  select single *
                     from bkpf
                     into wa_bkpf_2
                     where bukrs = wa_bsad_brasil2-bukrs
                     and   belnr = wa_bsad_brasil2-belnr
                     and   gjahr = wa_bsad_brasil2-gjahr.

                  wa_bsad_brasil2-vbeln = wa_bkpf_2-awkey.
                endif.

              endif.

              select single *
                  from vbfa
                  into  wa_vbfa_brasil
                  where vbeln  eq  wa_bsad_brasil2-vbeln "Fatura
                  and vbtyp_n  = 'M'
                  and vbtyp_v  = 'C'.

              if sy-subrc = 0.
*                "Selecionar REMESSA
                select single vbeln vbelv
                     from vbfa
                     into wa_vbfa_2
                     where vbeln  eq  wa_bsad_brasil2-vbeln "Fatura
                     and vbtyp_n  eq 'M'
                     and vbtyp_v  eq 'J'.

                check sy-subrc = 0.

                select single numero_due id_due nr_registro_expo id_nomeacao_tran vbeln
                   from   zdoc_exp
                   into wa_zdoc_exp
                   where vbeln eq wa_vbfa_2-vbelv.

                check sy-subrc = 0.

                xachou = 'X'.
                "Selecionar NAVIO
                clear wa_znom_transporte.
                select single *
                  from znom_transporte
                  into wa_znom_transporte
                  where id_nomeacao_tran  = wa_zdoc_exp-id_nomeacao_tran.

                wg_brasil-vbelv    = wa_vbfa_brasil-vbelv. " Ordem de Venda
                wg_brasil-vbeln    = wa_vbfa_brasil-vbeln. " Fatura
                wg_brasil-nr_re    = wa_zdoc_exp-nr_registro_expo.
                wg_brasil-numero_due   = wa_zdoc_exp-numero_due.
                "
                select single id_due numero_due situacao_due
                 from zsdt0170
                 into wa_due
                 where id_due     = wa_zdoc_exp-id_due
                 and   numero_due = wa_zdoc_exp-numero_due
                 and   loekz      = ' '.
                if sy-subrc = 0.
                  wg_brasil-situacao_due = wa_due-situacao_due.
                endif.

                wg_brasil-ds_porto    = wa_znom_transporte-ds_porto.
                wg_brasil-navio       = wa_znom_transporte-ds_nome_transpor.
                modify tg_brasil from wg_brasil index tabix transporting vbelv vbeln nr_re numero_due navio ds_porto situacao_due.
              else.
                wg_brasil-belnr  = wa_bsad_brasil2-belnr. "PEGA O PROXIMO BELNR
              endif.
            endif.



          endwhile.
        endif.


      endloop.


      "USER STORY 78022 -LPC
      clear :wg_brasil.
      loop at tg_brasil into wg_brasil.

        select single id_due
                from zsdt0170
                into @data(lv_iddue)
                where numero_due = @wg_brasil-numero_due
                and   loekz      = ' '.

        if lv_iddue is not initial.

          select single destino_country
            from zsdt0174
            into @data(lv_dcountry)
            where  id_due = @lv_iddue.
          if lv_dcountry is not initial.

            select single landx
              from t005t
              into @wg_brasil-x_pais
               where land1 eq @lv_dcountry
               and spras eq @sy-langu.
            modify tg_brasil from wg_brasil transporting x_pais  where  numero_due eq wg_brasil-numero_due.
          endif.
        endif.
      endloop.

      delete tg_brasil where vbeln is initial.
    endif.

    sort: tg_brasil  by  invoice ,
          tg_externo by invoice.

    if wg_cadbai-vlr_pgto lt 0.
      wg_cadbai-vlr_pgto = 0.
    endif.

    call screen 4000. " STARTING AT 20  1
    " ENDING   AT 200 32.
  else.
    message 'Não foram selecionados Documentos'(153) type 'I'.
  endif.
endform.                    " CARREGA_BAIXAS

*&---------------------------------------------------------------------*
*&      Form  ROT_BAIXA_EXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rot_baixa_ext .
  clear wl_erro.
  data v_status_proc type zfit0036-status.
  v_status_proc = 'A'.
  if wg_cadbai-tipo = 'C'.
    v_status_proc = 'L'.
  endif.
  loop at tg_externo into wg_externo.
    if wg_externo-rg_atualizado = 'T'.
      continue. " Se for 'T' é porque já fez esta parte no processo anterior, fazer apenas a parte BRASIL.
*      WL_ERRO = 'X' .
*      MESSAGE 'Lançamentos não permitidos, somente atualizar taxa de câmbio' TYPE 'I'.
*      EXIT.
    endif.
    tabix = sy-tabix.
    if vnum is initial.
      call function 'NUMBER_GET_NEXT'
        exporting
          nr_range_nr = '01'
          object      = 'ZID_INV'
        importing
          number      = vseq.
      vnum = vseq .

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vnum
        importing
          output = vnum.
    endif.

    if wg_externo-vlr_sld = 0.

* ---> S4 Migration - 10/06/2023 - DG
      data: lv_vlr_pgto type zfit0036-vlr_pgto.

      lv_vlr_pgto   = conv #( wg_externo-vlr_pgt ).
* <--- S4 Migration - 10/06/2023 - DG




      if wg_externo-tx_cambio gt 0 .
        update zfit0036 set moeda_pgto = wg_cadbai-moeda
                            dt_pgto    = wg_cadbai-dt_pgt
                            vlr_pgto   = lv_vlr_pgto "WG_EXTERNO-VLR_PGT  ---> S4 Migration - 10/06/2023 - DG
                            forma_pg   = wg_cadbai-tipo
                            motivo     = wg_cadbai-motivo
                            operacao   = wg_cadbai-opera
                            observacao = wg_cadbai-observacao
                            referencia = wg_cadbai-referencia
                            lote       = vnum
                            status     = v_status_proc
                            rg_atualizado  = ''
        where invoice  eq wg_externo-invoice
        and   belnr    eq wg_externo-belnr36
        and   buzei    eq wg_externo-buzei36
        and   obj_key  eq wg_externo-obj_key.
      else.
        update zfit0036 set moeda_pgto = wg_cadbai-moeda
                           dt_pgto    = wg_cadbai-dt_pgt
                           vlr_pgto   = lv_vlr_pgto "WG_EXTERNO-VLR_PGT  ---> S4 Migration - 10/06/2023 - DG
                           forma_pg   = wg_cadbai-tipo
                           motivo     = wg_cadbai-motivo
                           operacao   = wg_cadbai-opera
                           observacao = wg_cadbai-observacao
                           referencia = wg_cadbai-referencia
                           lote       = vnum
                           status     = v_status_proc
                           rg_atualizado     = 'T'
       where invoice  eq wg_externo-invoice
       and   belnr    eq wg_externo-belnr36
       and   buzei    eq wg_externo-buzei36
       and   obj_key  eq wg_externo-obj_key.

        loop at tg_conta into wg_conta.
          if sy-tabix = 1.
            update zfit0036 set bvtyp   = wg_conta-bvtyp
                                swift_1 = wg_conta-swift
                                banks_1 = wg_conta-banks
                                bankl_1 = wg_conta-bankl
                                banka_1 = wg_conta-banka
                                bankn_1 = wg_conta-bankn
                                iban_1  = wg_conta-iban
            where obj_key = wg_externo-obj_key
            and   belnr = wg_externo-belnr36
            and   buzei = wg_externo-buzei36.
          else.
            update zfit0036 set bvtyp_2   = wg_conta-bvtyp
                               swift_2 = wg_conta-swift
                               banks_2 = wg_conta-banks
                               bankl_2 = wg_conta-bankl
                               banka_2 = wg_conta-banka
                               bankn_2 = ''
                               iban_2  = wg_conta-iban
            where obj_key = wg_externo-obj_key
            and   belnr = wg_externo-belnr36
            and   buzei = wg_externo-buzei36.
          endif.
        endloop.
      endif.

      wg_externo-fg_ok = 'X'.
      modify tg_externo from wg_externo index tabix transporting fg_ok.
    else.
      clear wl_erro.
      perform f_shdb_51e changing wg_externo wl_erro.
      if wl_erro ne 'X' .
        refresh it_bsik_shdb.
        vtentativas = 0.
        while vtentativas lt 10.
          wait up to 1 seconds.
          select * "bukrs lifnr belnr dmbtr dmbe2 budat buzei
           from bsik
           into corresponding fields of table it_bsik_shdb
           where bukrs eq wg_externo-bukrs
           and   belnr eq wg_documento.
          add 1 to vtentativas.
          if not it_bsik_shdb[] is initial.
            exit.
          endif.
        endwhile.
        if  it_bsik_shdb[] is initial.
          wl_erro = 'X' .
          exit.
        endif.
        clear wa_zfit0036_ins.
        loop at it_bsik_shdb into wa_bsik.
          wa_zfit0036_ins-obj_key	=	wg_externo-obj_key .
          wa_zfit0036_ins-belnr   = wg_documento.
          wa_zfit0036_ins-buzei   = wa_bsik-buzei.
          wa_zfit0036_ins-bukrs   = wg_externo-bukrs.
          wa_zfit0036_ins-invoice	=	wg_externo-invoice.
          wa_zfit0036_ins-navio   = wg_externo-navio.
          wa_zfit0036_ins-operacao = wg_cadbai-opera.
          wa_zfit0036_ins-observacao = wg_cadbai-observacao.
          wa_zfit0036_ins-referencia = wg_cadbai-referencia.
          wa_zfit0036_ins-usuario     = sy-uname.
          wa_zfit0036_ins-data_atual  = sy-datum.
          wa_zfit0036_ins-hora_atual  = sy-uzeit.
          wa_zfit0036_ins-user_create = sy-uname. "BUG - 75841 - CBRAND
          if wa_bsik-dmbe2 = wg_externo-vlr_pgt.
            wa_zfit0036_ins-moeda_pgto = wg_cadbai-moeda.
            wa_zfit0036_ins-forma_pg = wg_cadbai-tipo.
            wa_zfit0036_ins-motivo   = wg_cadbai-motivo.
            wa_zfit0036_ins-bvtyp    = wg_externo-bvtyp.
            wa_zfit0036_ins-lifnr    = wg_externo-lnrza.
            wa_zfit0036_ins-lote     =  vnum.
            wa_zfit0036_ins-status   = v_status_proc .
            wa_zfit0036_ins-rg_atualizado   = 'T'.

*---> 08/06/2023 - Migração S4 - JS
*            WA_ZFIT0036_INS-VLR_PGTO = WA_BSIK-DMBE2.
            wa_zfit0036_ins-vlr_pgto = conv #( wa_bsik-dmbe2 ).
*<--- 08/06/2023 - Migração S4 - JS

            wa_zfit0036_ins-dt_pgto  = wg_cadbai-dt_pgt.
            " GRAVA DADOS BANCÁRIOS.
            loop at tg_conta into wg_conta.
              if sy-tabix = 1.
                wa_zfit0036_ins-bvtyp   = wg_conta-bvtyp.
                wa_zfit0036_ins-swift_1 = wg_conta-swift.
                wa_zfit0036_ins-banks_1 = wg_conta-banks.
                wa_zfit0036_ins-bankl_1 = wg_conta-bankl.
                wa_zfit0036_ins-banka_1 = wg_conta-banka.
                wa_zfit0036_ins-bankn_1 = wg_conta-bankn.
                wa_zfit0036_ins-iban_1  = wg_conta-iban.

              else.
                wa_zfit0036_ins-bvtyp_2   = wg_conta-bvtyp.
                wa_zfit0036_ins-swift_2 = wg_conta-swift.
                wa_zfit0036_ins-banks_2 = wg_conta-banks.
                wa_zfit0036_ins-bankl_2 = wg_conta-bankl.
                wa_zfit0036_ins-banka_2 = wg_conta-banka.
                wa_zfit0036_ins-bankn_2 = ''. "WG_CONTA-BANKN.
                wa_zfit0036_ins-iban_2  = wg_conta-iban.
              endif.
            endloop.
          endif.
          insert into  zfit0036 values wa_zfit0036_ins.
          if sy-subrc ne 0.
            rollback work.
          else.
            commit work.
          endif.
          clear wa_zfit0036_ins.
        endloop.
        update zfit0036 set status     = 'C'
                            lote       = vnum
                            rg_atualizado  = ''
         where invoice  eq wg_externo-invoice
         and   belnr    eq wg_externo-belnr36
         and   buzei    eq wg_externo-buzei36
         and   obj_key  eq wg_externo-obj_key.
        clear wg_externo-lote.
        modify tg_externo from wg_externo index tabix transporting lote.

        wg_externo-fg_ok = 'X'.
        modify tg_externo from wg_externo index tabix transporting fg_ok.
      else.
        exit.
      endif.

    endif.

  endloop.

endform.                    " ROT_BAIXA_EXT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_51E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_shdb_51e changing p_alv like wg_externo p_erro.
  data: vdata(10),
        wl_vlr(16),
        wl_vlrs(16),
        wl_taxa(16),
        v_pgt       type df05b-psdif,
        v_sld       type rf05a-akobt,
        v_belnr     type bkpf-belnr,
        v_gsber     type bseg-gsber.

  concatenate p_bukrs+2(2) '01' into v_gsber.

  refresh ti_bdcdata.
  concatenate  wg_cadbai-dt_lct+6(2) wg_cadbai-dt_lct+4(2) wg_cadbai-dt_lct(4) into vdata separated by '.'.

  v_pgt = p_alv-vlr_pgt * -1.
  write: v_pgt                to wl_vlr.
  translate wl_vlr using '. ,'.
  condense wl_vlr no-gaps.
  v_sld = p_alv-vlr_sld.
  write: v_sld        to wl_vlrs.


  select single  *
    from zib_contabil
    into wa_zib_contabil
    where obj_key eq wg_externo-obj_key.

  select single obj_key belnr  bukrs    bukrs gjahr
  from zib_contabil_chv
  into wa_zib_contabil_chv
  where obj_key eq wg_externo-obj_key.

  if wg_externo-belnr36 is initial.
    v_belnr = wa_zib_contabil_chv-belnr.
  else.
    v_belnr = wg_externo-belnr36.
  endif.

  select single bukrs gjahr awkey belnr blart budat bldat xblnr waers kursf
    from bkpf
    into wa_bkpf
    where bukrs	=	wa_zib_contabil_chv-bukrs
    and   gjahr	=	wa_zib_contabil_chv-gjahr
    and   belnr	=	v_belnr.



  write: wa_bkpf-kursf  to wl_taxa.

  perform f_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SL',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       wa_zib_contabil-blart,
    ''          ''      ''   'BKPF-BUKRS'       wa_zib_contabil-bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadbai-dt_lct+4(2),
    ''          ''      ''   'BKPF-WAERS'       wg_cadbai-moeda,
    ''          ''      ''   'BKPF-KURSF'       wl_taxa,

    'SAPMF05A'  '0710'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'RF05A-AGBUK'      wa_zib_contabil-bukrs,
    ''          ''      ''   'RF05A-AGKON'      wa_zib_contabil-hkont,
    ''          ''      ''   'RF05A-AGKOA'      'K',
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',

    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA',
    ''          ''      ''   'RF05A-SEL01(01)'  v_belnr,


    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=REST',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6102PAGE',
    ''          ''      ''   'RF05A-ABPOS'      '1',
    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PI',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
    ''          ''      ''   'RF05A-ABPOS'      '1',

    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
    ''          ''      ''   'RF05A-ABPOS'      '1',
    ''          ''      ''   'DF05B-PSDIF(01)'  wl_vlr,

    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PI',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
    ''          ''      ''   'RF05A-ABPOS'      '1',

    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
    ''          ''      ''   'RF05A-ABPOS'      '1',
    ''          ''      ''   'DF05B-PSDIF(01)'  wl_vlr.

  if wg_externo-belnr36 is not initial. "desmarca a segunda linha que aparecer de acordo com buzei
    perform f_bdc_data using:
    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PI',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
    ''          ''      ''   'BDC_CURSOR'	      'DF05B-PSBET(02)',
    ''          ''      ''   'RF05A-ABPOS'      '1'.
  endif.

  perform f_bdc_data using:
  'SAPDF05X'  '3100'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=BS',
  ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
  ''          ''      ''   'RF05A-ABPOS'       '1',
  ''          ''      ''   'RF05A-AKOBT'       wl_vlrs,

  'SAPMF05A'  '0700'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_CURSOR'	      'RF05A-AZEI1(01)',
  ''          ''      ''   'BDC_OKCODE'	      '=PI',

  'SAPMF05A'  '0302'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=S+',
  ''          ''      ''   'BSEG-BUPLA'	      v_gsber,
  ''          ''      ''   'BSEG-GSBER'	      v_gsber,
  ''          ''      ''   'BSEG-ZTERM'	      '0004',
  ''          ''      ''   'BSEG-ZFBDT'	       vdata,
  ''          ''      ''   'BSEG-ZUONR'	      wa_zib_contabil-zuonr,
  ''          ''      ''   'BSEG-SGTXT'	      wa_zib_contabil-sgtxt,

  'SAPMF05A'  '0302'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=S+',
  ''          ''      ''   'BSEG-BUPLA'	      v_gsber,
  ''          ''      ''   'BSEG-GSBER'	      v_gsber,
  ''          ''      ''   'BSEG-ZTERM'	      '0004',
  ''          ''      ''   'BSEG-ZFBDT'	       vdata,
  ''          ''      ''   'BSEG-ZUONR'	      wa_zib_contabil-zuonr,
  ''          ''      ''   'BSEG-SGTXT'	      wa_zib_contabil-sgtxt,

  'SAPMF05A'  '0302'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=AB',
  ''          ''      ''   'BSEG-ZUONR'	      wa_zib_contabil-zuonr,
  ''          ''      ''   'BSEG-SGTXT'	      wa_zib_contabil-sgtxt,

  'SAPMF05A'  '0700'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=BU'.


  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.
  if p_erro = 'X'.
    rollback work.
  else.
    commit work.
  endif.
endform.                    " F_SHDB_51E
*&---------------------------------------------------------------------*
*&      Form  ROT_BAIXA_BRA
*&---------------------------------------------------------------------*
*       text
**----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rot_baixa_bra .
  data  vsaldo_cre type zfit0036-vlr_pgto.
  " Executa SHDB
  clear wl_erro.
  loop at tg_brasil into wg_brasil where checkbox = 'X'.
    tabix = sy-tabix.
    if wg_brasil-vlr_sld ne 0 and wg_brasil-vlr_bai ne 0.
      perform f_shdb_51b changing wg_brasil wl_erro.
      if wl_erro ne 'X'.
        refresh it_bsid_shdb.
        vtentativas = 0.
        while vtentativas lt 10.
          wait up to 1 seconds.
          select bukrs belnr gjahr dmbtr dmbe2 kunnr buzei
           from bsid
           into table it_bsid_shdb
           where bukrs eq wg_brasil-bukrs
           and   belnr eq wg_documento.
          add 1 to vtentativas.
          if not it_bsid_shdb[] is initial.
            exit.
          endif.
        endwhile.
        if  it_bsid_shdb[] is initial.
          wl_erro = 'X' .
          exit.
        endif.
        loop at it_bsid_shdb into wa_bsid.
          if wa_bsid-dmbe2 = wg_brasil-vlr_bai.
            wg_brasil-belnr2 = wa_bsid-belnr.
            wg_brasil-gjahr2 = wa_bsid-gjahr.
            wg_brasil-buzei2 = wa_bsid-buzei.
            modify tg_brasil from wg_brasil index tabix transporting belnr2 gjahr2 buzei2.
          endif.
        endloop.
      else.
        exit.
      endif.
    endif.
  endloop.
  " Se sucesso no SHDB ou se não houve SHDB para executar, então gravar na tabela
  if wl_erro ne 'X'.
    loop at tg_brasil into wg_brasil where checkbox = 'X'.
      tabix = sy-tabix.
      clear wa_zfit0042.
      " leitura taxa dolar
      " Pega Primeira taxa de cambio informada e comprar com as outras

      loop at tg_externo into wg_externo.
        if wg_externo-tx_cambio gt 0.
          exit.
        endif.
      endloop.

      if  wg_externo-tx_cambio eq 0 or wg_externo-dt_cred is initial.
        continue.
      endif.

      if wg_brasil-vlr_bai ne 0.
        if wg_externo-lote is not initial. " Pega o lote do processo feito em partes
          vnum = wg_externo-lote.
        endif.
        if vnum is initial.
          call function 'NUMBER_GET_NEXT'
            exporting
              nr_range_nr = '01'
              object      = 'ZID_INV'
            importing
              number      = vseq.
          vnum = vseq .

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = vnum
            importing
              output = vnum.
        endif.

        wa_zfit0042-bukrs         = wg_brasil-bukrs.
        wa_zfit0042-numero_due    = wg_brasil-numero_due.
        wa_zfit0042-nr_re         = wg_brasil-nr_re.
        wa_zfit0042-nr_invoice    = wg_brasil-invoice.
        wa_zfit0042-belnr         = wg_brasil-belnr.
        wa_zfit0042-buzei         = wg_brasil-buzei.
        wa_zfit0042-gjahr         = wg_brasil-gjahr.
        if wg_brasil-belnr2 is not initial.
          wa_zfit0042-belnr2        = wg_brasil-belnr2.
          wa_zfit0042-gjahr2        = wg_brasil-gjahr2.
          wa_zfit0042-buzei2        = wg_brasil-buzei2.
        else.
          wa_zfit0042-belnr2        = wg_brasil-belnr.
          wa_zfit0042-gjahr2        = wg_brasil-gjahr.
          wa_zfit0042-buzei2        = wg_brasil-buzei.
        endif.
        wa_zfit0042-kunnr         = wg_brasil-kunnr.
        wa_zfit0042-tp_operacao   = wg_cadbai-opera.
        wa_zfit0042-nro_ov        = wg_brasil-vbelv.
        wa_zfit0042-nro_fatura    = wg_brasil-vbeln.
        wa_zfit0042-lote          = vnum.
        wa_zfit0042-dt_venc       = wg_externo-dt_cred.
        wa_zfit0042-tx_cambio     = wg_externo-tx_cambio.
        wa_zfit0042-dmbe2         = wg_brasil-vlr_bai.
        wa_zfit0042-dmbtr         = wg_brasil-vlr_bai * wg_externo-tx_cambio.
        wa_zfit0042-usnam         = sy-uname.
        wa_zfit0042-dt_atual      = sy-datum.
        wa_zfit0042-hr_atual      = sy-uzeit.
        insert into  zfit0042 values wa_zfit0042.
        if sy-subrc ne 0.
          rollback work.
        else.
          commit work.
        endif.

        if wg_brasil-vlr_sld eq 0.
          update zfit0041 set rg_atualizado = 'S'
          where nr_re = wa_zfit0042-nr_re.
*          AND   NUMERO_DUE = WA_ZFIT0042-NUMERO_DUE.
        endif.
        clear wa_zfit0042.
        wg_brasil-fg_ok = 'X'.
        modify tg_brasil from wg_brasil index tabix transporting fg_ok.
      endif.
    endloop.

    loop at tg_externo into wg_externo.
      if wg_externo-tx_cambio gt 0.
        if wg_externo-rg_atualizado = 'T'.
          vsaldo_cre = ( wg_externo-vlr_pgt - wg_externo-vlr_cre - wg_externo-vlr_cre_b ).
          if vsaldo_cre = 0 . " já foi baixado todo o pagamento creditado
            update zfit0036 set
                                rg_atualizado = ''
              where invoice  eq wg_externo-invoice
              and   belnr    eq wg_externo-belnr36
              and   buzei    eq wg_externo-buzei36
              and   obj_key  eq wg_externo-obj_key.
          endif.
        else.
*          UPDATE ZFIT0036 SET RG_ATUALIZADO = 'S'
*            WHERE INVOICE  EQ WG_EXTERNO-INVOICE
*            AND   BELNR    EQ WG_EXTERNO-BELNR36
*            AND   BUZEI    EQ WG_EXTERNO-BUZEI36
*            AND   OBJ_KEY  EQ WG_EXTERNO-OBJ_KEY.
        endif.
      endif.
    endloop.
  endif.
endform.                    " ROT_BAIXA_BRA
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_51B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_BRASIL  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_51b   changing p_alv like wg_brasil p_erro.
  data: vdata(10),
        vdata2(10),
        wl_vlr(16),
        wl_vlrb(16),
        wl_kursf(16),
        v_bai        type rf05a-akobt,
        v_sld        type df05b-psdif,
        v_kur        type bkpf-kursf,
        cnum_seq(2),
        vnum_seq     type i,
        vcampo(15),
        vqtd_c       type i,
        xachou(1),
        v_belnr      type bkpf-belnr.

  " pega data de Credito
  loop at tg_externo into wg_externo.
    if wg_externo-tx_cambio gt 0 and wg_externo-dt_cred is not initial.
      exit.
    endif.
  endloop.

  refresh ti_bdcdata.
  concatenate  wg_externo-dt_cred+6(2) wg_externo-dt_cred+4(2) wg_externo-dt_cred(4) into vdata  separated by '.'.

  v_bai = p_alv-vlr_bai * -1.
  write: v_bai  to wl_vlrb.

  v_sld =  p_alv-vlr_sld.
  write: v_sld  to wl_vlr.
  v_kur = p_alv-kursf.
  write: v_kur   to wl_kursf.

  refresh it_bsid_42.


  perform f_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SL',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       p_alv-blart,
    ''          ''      ''   'BKPF-BUKRS'       p_alv-bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_externo-dt_cred+4(2),
    ''          ''      ''   'BKPF-WAERS'       p_alv-waers,
    ''          ''      ''   'BKPF-KURSF'       wl_kursf,
    ''          ''      ''   'BKPF-XBLNR'       p_alv-xblnr,

    'SAPMF05A'  '0710'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'RF05A-AGBUK'      p_alv-bukrs,
    ''          ''      ''   'RF05A-AGKON'      p_alv-kunnr,
    ''          ''      ''   'RF05A-AGKOA'      'D',
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X'.


  perform f_bdc_data using:
     'SAPMF05A'  '0731'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '=PA',
     ''          ''      ''   'RF05A-SEL01(01)'  p_alv-belnr2.



  perform f_bdc_data using:
      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=REST',
      ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6102PAGE',
      ''          ''      ''   'RF05A-ABPOS'      '1'.


  read table it_zfit0042_cre into wa_zfit0042 with key belnr      = wg_brasil-belnr
                                                       belnr2     = wg_brasil-belnr2.
  if sy-subrc = 0.
    select  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei
    from bsid
    into  table it_bsid_42
    where bukrs	eq  wa_zfit0042-bukrs
    and   belnr eq  wa_zfit0042-belnr2
    "AND   buzei EQ  wa_zfit0042-buzei2
    and   gjahr eq  wa_zfit0042-dt_venc+0(4).
  endif.


  vnum_seq = 0.
  loop at it_bsid_42 into wa_bsid.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    if  wa_bsid-dmbe2 ne wa_zfit0042-dmbe2. " Valor diferente da Bsid, então gravar saldo
      concatenate 'DF05B-PSDIF(' cnum_seq ')' into vcampo.
      perform f_bdc_data using:
        'SAPDF05X'  '3100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '/00',
        ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
        ''          ''      ''   'BDC_CURSOR'	      vcampo,
        ''          ''      ''   'RF05A-ABPOS'      '1',
        ''          ''      ''   vcampo             wl_vlr.
    else. " desmarca linha
      concatenate 'DF05B-PSBET(' cnum_seq ')' into vcampo.
      perform f_bdc_data using:
        'SAPDF05X'  '3100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=PI',
        ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
        ''          ''      ''   'BDC_CURSOR'	      vcampo,
        ''          ''      ''   'RF05A-ABPOS'      '1'.
    endif.

  endloop.
  if vnum_seq = 0. " Não tem documentos compensados anteriormente, processa linha 01
    perform f_bdc_data using:
        'SAPDF05X'  '3100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '/00',
        ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
        ''          ''      ''   'BDC_CURSOR'	      'DF05B-PSDIF(01)',
        ''          ''      ''   'RF05A-ABPOS'      '1',
        ''          ''      ''   'DF05B-PSDIF(01)'  wl_vlr.
  endif.

  perform f_bdc_data using:
      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BS',
      ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6106PAGE',
      ''          ''      ''   'BDC_CURSOR'	      'RF05A-AKOBT',
      ''          ''      ''   'RF05A-ABPOS'       '1',
      ''          ''      ''   'RF05A-AKOBT'       wl_vlrb,

      'SAPMF05A'  '0700'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BU',
      ''          ''      ''   'BKPF-XBLNR'	       p_alv-xblnr.


  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.

endform.                    " F_SHDB_51B
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS_4000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields_4000 output.
  loop at screen.
    if screen-name = 'WG_CADBAI-TIPO'.
      clear wg_cadbai-de_tipo.
      if wg_cadbai-tipo = 'C'.
        wg_cadbai-de_tipo = 'Compensação'(223).
      elseif wg_cadbai-tipo = 'P'.
        wg_cadbai-de_tipo = 'Pagamento'(224).
      endif.
      modify screen.
    endif.
    if screen-name = 'WG_CADBAI-OPERA'.
      clear wg_cadbai-de_opera.
      if wg_cadbai-opera = '01'.
        wg_cadbai-de_opera = 'Cambio'(225).
      elseif wg_cadbai-opera = '02'.
        wg_cadbai-de_opera = 'Captações'(226).
      elseif wg_cadbai-opera = '03'.
        wg_cadbai-de_opera = 'Baixa PA'(227).
      endif.
      modify screen.
    endif.
    if screen-name = 'WG_CADBAI-MOTIVO'.
      clear wg_cadbai-descricao.
      select single *
        from zfit0040
        into wa_zfit0040
        where motivo = wg_cadbai-motivo.
      if sy-subrc = 0.
        wg_cadbai-descricao = wa_zfit0040-descricao.
      endif.
      modify screen.
    endif.
    if wg_cadbai-rg_atualizado = 'T'.
      if screen-name = 'WG_CADBAI-TIPO' or
         screen-name = 'WG_CADBAI-OPERA' or
         screen-name = 'WG_CADBAI-DT_PGT' or
         screen-name = 'WG_CADBAI-OBSERVACAO' or
         screen-name = 'WG_CADBAI-REFERENCIA' or
         screen-name = 'WG_CADBAI-MOEDA'.
        screen-input     = 0.
        screen-invisible = 0.
        modify screen.
      endif.
      if screen-name = 'WG_CADBAI-MOTIVO' or
         screen-name = 'WG_CADBAI-DESCRICAO' or
         screen-name = 'TXTMOTIVO' or
         screen-name = 'WG_CADBAI-DT_LCT' or
         screen-name = 'TXTDTLCT'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endif.
  endloop.
endmodule.                 " TRATA_FIELDS_4000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form verifica_erros_cria .
  data: tl_tbsl       type table of tbsl,
        wl_tbsl       type          tbsl,
        wl_tka02      type tka02,
        wl_cskb       type cskb,
        wl_bsik       type bsik,
        wl_name1      type skat-txt50,
        wl_kostl      type csks-kostl,
        wv_valor      type bseg-wrbtr,
        wlinha(3),
        vkokrs        type tka02-kokrs,
        wcont         type i,
        w_div1(1),
        w_div2(1),
        t_divisao     type standard table of  rgsb4  with header line,
        wmensagem(50).

*  DATA: vg_data_pgt TYPE sy-datum.

  data: wa_tcurc    type tcurc.
*
  select single *
  from tka02
  into wl_tka02
  where bukrs  = p_bukrs.
  move wl_tka02-kokrs to vkokrs.

  if  wg_cadger-obj_key is not initial.
    message 'Adiantamentos gravado'(258) type 'I'.
    wl_erro = 'X'.
    exit.
  endif.


*  "Check data de pagamento CS2022000843 Bloqueio pagamento internacional fora do prazo de 48H / Anderson Oenning.
  select single *
  from zfit0179
  into @data(i_data)
  where usname_acesso eq @sy-uname.
  if sy-subrc ne 0.

    zcl_solicitacao_ov=>dia_util(
                                  exporting p_vencimento = wg_cadger-dt_lct
*                                            i_bukrs      = p_bukrs "USER STORY 158527 - MMSILVA - 17.01.2025
                                  importing e_subrc      = data(is_dia_util)
                                           ).

    if is_dia_util <> 4 and wg_cadger-dt_lct is not initial.
      message 'Data informada não é dia útil' type 'I'.
      wl_erro = 'X'.
      clear: is_dia_util.
      exit.
    endif.

    clear: vg_data_pgt, i_data.
    vg_data_pgt = sy-datum + 2.

    if wg_cadger-dt_lct < vg_data_pgt .
      message 'Data de pagamento está fora do prazo de 48h'(475) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
  endif.
*  "Fim CS2022000843.



  if wg_cadger-dt_lct is initial.
    message 'Data de Lançamento deve ser informada'(229) type 'I'.
    wl_erro = 'X'.
  elseif wg_cadger-dt_lct lt sy-datum.
    message 'Data de Lançamento inválida'(230) type 'I'.
    wl_erro = 'X'.
  endif.
  if wg_cadger-waers is initial.
    message 'Moeda deve ser informada'(231) type 'I'.
    wl_erro = 'X'.
  else.
    select single *
      from tcurc
      into wa_tcurc
      where waers = wg_cadger-waers.
    if sy-subrc ne 0.
      message 'Moeda não existe'(232) type 'I'.
      wl_erro = 'X'.
    endif.
  endif.

  if  wg_cadger-tp_operacao is initial.
    message 'Operação deve ser informada'(233) type 'I'.
    wl_erro = 'X'.
  else.
    select single *
    from zfit0043
    into wa_zfit0043
    where tp_operacao = wg_cadger-tp_operacao
     and spras = sy-langu.
    if sy-subrc ne 0.
      message 'Operação de adiantamento não existe'(234) type 'I'.
      wl_erro = 'X'.
    endif.
  endif.

  if  wg_cadger-motivo is initial.
    message 'Informar Motivo'(235) type 'I'.
    wl_erro = 'X'.
  else.
    select single *
    from zfit0040
    into wa_zfit0040
    where motivo = wg_cadger-motivo.
    if sy-subrc ne 0.
      message 'Motivo não existe'(236) type 'I'.
      wl_erro = 'X'.
    endif.
  endif.

  describe table tg_conta lines vlines.
  if p_bukrs ne '0200'. " US 164319-26-02-2025-#164319-RJF
    if vlines eq 0.
      message 'Informe dados bancários'(148) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
  endif. " US 164319-26-02-2025-#164319-RJF
  loop at tg_conta into wg_conta.
    select single *
          from lfbk
          into wl_lfbk
          where lifnr = wg_conta-lifnr
          and banks   = wg_conta-banks
          and bankl   = wg_conta-bankl
          and bankn   = wg_conta-bankn.
    if sy-subrc ne 0.
      message  'Dados bancários incorretos'(149) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
  endloop.

  if tg_criaadt[] is not initial.
    select *
      from tbsl
      into table tl_tbsl
      for all entries in tg_criaadt
      where bschl = tg_criaadt-bschl.
    sort tl_tbsl by bschl.
  else.
    message 'Não há adiantamentos lançados'(237) type 'I'.
    wl_erro = 'X'.
  endif  .
  if wg_cadger-question = ''.
    wcont = 0.
    wv_valor = 0.
    loop at tg_criaadt into wg_criaadt.
      add 1 to wcont.
      add wg_criaadt-wrbtr to wv_valor.

      if wg_criaadt-saknrz is initial.
        concatenate 'Conta fornecedor obrigatória fornecedor, linha'(238) wlinha into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
      else.
        select single name1
                from lfa1
                into wl_name1
                where lifnr = wg_criaadt-saknrz.
        if sy-subrc ne 0.
          concatenate 'Código conta não cadastrado, linha'(239) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.
      endif.

*      IF WG_CRIAADT-BVTYP IS INITIAL.
*        CONCATENATE 'Conta bancária obrigatória fornecedor, linha' WLINHA INTO WMENSAGEM  SEPARATED BY SPACE.
*        MESSAGE WMENSAGEM TYPE 'I'.
*        WL_ERRO = 'X'.
*      ENDIF.

      if  wg_criaadt-lifnr is initial.
*        CONCATENATE 'Fornecedor obrigatório, linha' WLINHA INTO WMENSAGEM  SEPARATED BY SPACE.
*        MESSAGE WMENSAGEM TYPE 'I'.
*        WL_ERRO = 'X'.
      else.
        select single name1
         from lfa1
         into wl_name1
         where lifnr = wg_criaadt-lifnr.
        if sy-subrc ne 0.
          concatenate 'Código fornecedor não cadastrado, linha'(240) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.
      endif.

      if wg_cadger-liquidar = 'S'.
        if wg_criaadt-belnr is initial.
          concatenate 'Doc.Liquidação obrigatório, linha'(241) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        else.
          select single *
            from bsik
            into wl_bsik
            where bukrs = p_bukrs
            and   belnr = wg_criaadt-belnr.
          if sy-subrc ne 0.
            concatenate 'Doc.Liquidação não existe, linha'(242) wlinha into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          else.
                                                            "IR161571
            select single *
              from zfit0036
              into @data(wl36)
              where belnr_adt_c =  @wg_criaadt-belnr
              and   eliminar    <> 'X'.
            if sy-subrc = 0.
              concatenate 'Documento ja vinculado no lote ' wl36-lote ', linha' wlinha into wmensagem  separated by space.
              message wmensagem type 'I'.
              wl_erro = 'X'.
            endif.
                                                            "IR161571
          endif.

        endif.

        if wg_criaadt-saknr_ba is initial.
          concatenate 'Informe a conta banco, linha'(243) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        else.
          select single txt50
            from skat
            into wl_name1
            where spras = sy-langu
            and ktopl = '0050'
            and saknr = wg_criaadt-saknrz_ba.

          if sy-subrc ne 0.
            concatenate 'Código conta banco não cadastrado, linha'(244) wlinha into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          endif.
        endif.
      endif.
    endloop.
*    IF  wcont NE 1.
*      MESSAGE 'Permitido somente uma linha, para esta operação' TYPE 'I'.
*      wl_erro = 'X'.
*      EXIT.
*    ENDIF.
    if  wv_valor = 0.
      message 'Lançar o valor'(245) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
  else.
    " Divisões de empresas
    call function 'G_SET_GET_ALL_VALUES'
      exporting
        class         = '0000'
        setnr         = 'MAGGI_ZGL0016_DIV'
      tables
        set_values    = t_divisao
      exceptions
        set_not_found = 1
        others        = 2.
    if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
    sort t_divisao by from.
    wcont = 0.
    wv_valor = 0.
    loop at tg_criaadt into wg_criaadt.
      add 1 to wcont.
      wlinha = sy-tabix.

      if  wg_criaadt-gsber is initial.
        concatenate 'Divisão obrigatória, linha'(246) wlinha into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
      else.
        select single gsber
         from tgsb
         into wl_name1
         where gsber = wg_criaadt-gsber.
        if sy-subrc ne 0.
          concatenate 'Divisão não cadastrada, linha'(247) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.

        clear: w_div1, w_div2.
        loop at t_divisao.
          if t_divisao-from+0(4) = p_bukrs.
            w_div1 = 'X'.
            if t_divisao-from+5(4) = wg_criaadt-gsber.
              w_div2 = 'X'.
            endif.
          endif.
        endloop.
        if w_div1 = 'X' and w_div2 is initial.
          concatenate 'Divisão para empresa não cadastrada SET, linha'(248) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        elseif w_div1 is initial.
          concatenate 'Empresa sem Divisão SET, linha'(249) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.
      endif.

      if wg_criaadt-wrbtr is initial.
        concatenate 'Valor não informado, linha'(250) wlinha into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
      endif.

      if wg_criaadt-kostl is initial.








* ---> S4 Migration - 06/07/2023 - DG
*        select single * "#EC CI_DB_OPERATION_OK[2431747]
*            from CSKB
*            into WL_CSKB
*            where  KOKRS  = VKOKRS
*            and    KSTAR  = WG_CRIAADT-SAKNRZ
*            and    DATAB  le SY-DATUM
*            and    DATBI  ge SY-DATUM.

*   if SY-SUBRC = 0.

        data: lt_returns type table of bapiret2,
              ls_coeldes type bapi1030_ceoutputlist.

        data: wa_cskb            type  cskb,
              lv_controllingarea type  bapi1030_gen-co_area,
              lv_costelement     type  bapi1030_gen-cost_elem,
              lv_keydate         type  bapi1030_gen-some_date.

        lv_controllingarea  = vkokrs.
        lv_costelement      = wg_criaadt-saknrz.
        lv_keydate          = sy-datum.

        clear: lt_returns[], ls_coeldes.

        call function 'K_COSTELEM_BAPI_GETDETAIL'
          exporting
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
          importing
            costelementdetail = ls_coeldes
          tables
            return            = lt_returns.

        read table lt_returns transporting no fields with key type = 'E'.

        if sy-subrc ne 0.
          wl_cskb-kokrs = lv_controllingarea.
          wl_cskb-kstar = lv_costelement .
          wl_cskb-datbi = ls_coeldes-valid_to.
          wl_cskb-katyp = ls_coeldes-celem_category.
        endif.

        if wl_cskb is not initial.
* <--- S4 Migration - 06/07/2023 - DG





































          concatenate 'Centro de custo obrigatório , linha'(251) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.
      else.
        select single kostl
                from csks
                into wl_kostl
                where kokrs = vkokrs
                and   kostl = wg_criaadt-kostl.
        if sy-subrc ne 0.
          concatenate 'Centro Custo não cadastrado, linha'(252) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.
      endif.

      if  wg_criaadt-lifnr is initial.
*        CONCATENATE 'Fornecedor obrigatório, linha' WLINHA INTO WMENSAGEM  SEPARATED BY SPACE.
*        MESSAGE WMENSAGEM TYPE 'I'.
*        WL_ERRO = 'X'.
      else.
        select single name1
         from lfa1
         into wl_name1
         where lifnr = wg_criaadt-lifnr.
        if sy-subrc ne 0.
          concatenate 'Código fornecedor não cadastrado, linha'(240) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        endif.
      endif.

      if wg_cadger-liquidar = 'S'.
        if wg_criaadt-belnr is initial.
          concatenate 'Doc.Liquidação obrigatório, linha'(241) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        else.
          select single *
            from bsik
            into wl_bsik
            where bukrs = p_bukrs
            and   belnr = wg_criaadt-belnr.
          if sy-subrc ne 0.
            concatenate 'Doc.Liquidação não existe, linha'(242) wlinha into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          endif.
        endif.

        if wg_criaadt-saknr_ba is initial.
          concatenate 'Informe a conta banco, linha'(243) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        else.
          select single txt50
            from skat
            into wl_name1
            where spras = sy-langu
            and ktopl = '0050'
            and saknr = wg_criaadt-saknrz_ba.

          if sy-subrc ne 0.
            concatenate 'Código conta banco não cadastrado, linha'(244) wlinha into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          endif.

        endif.
      endif.

      if wg_criaadt-bschl is initial.
        concatenate 'Chave de lançamento obrigatório, linha'(253) wlinha into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
      else.
        read table tl_tbsl into wl_tbsl with key bschl = wg_criaadt-bschl binary search.
        if sy-subrc ne 0.
          concatenate 'Chave de lançamento não existe, linha'(254) wlinha into wmensagem  separated by space.
          message wmensagem type 'I'.
          wl_erro = 'X'.
        else.
          if wl_tbsl-shkzg = 'H'.
            add wg_criaadt-wrbtr to wv_valor.
          else.
            subtract  wg_criaadt-wrbtr from wv_valor.
          endif.
          if wl_tbsl-xsonu = 'X' and wg_criaadt-umskz is initial.
            concatenate 'Código de razão especial obrigatório, linha'(255) wlinha into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          endif.
          if wl_tbsl-koart = 'K'.
            select single name1
              from lfa1
              into wl_name1
              where lifnr = wg_criaadt-saknrz.
          elseif wl_tbsl-koart = 'D'.
            select single name1
              from kna1
              into wl_name1
              where kunnr = wg_criaadt-saknrz.
          elseif wl_tbsl-koart = 'S'.
            select single txt50
              from skat
              into wl_name1
              where spras = sy-langu
              and ktopl = '0050'
              and saknr = wg_criaadt-saknrz.
          endif.
          if sy-subrc ne 0.
            concatenate 'Código conta não cadastrado, linha'(239) wlinha into wmensagem  separated by space.
            message wmensagem type 'I'.
            wl_erro = 'X'.
          endif.

        endif.
      endif.

    endloop.
    if  wcont ne 1 and wg_cadger-liquidar eq 'S'.
      message 'Permitido somente uma linha, para esta operação'(256) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
    if wv_valor ne 0 and wg_cadger-liquidar ne 'S'.
      message 'Total DEB/CRE deve zerar'(257) type 'I'.
      wl_erro = 'X'.
    endif.
  endif.
*** US - 76596 - Inicio - CBRAND
* Se pais = 'LU' - Verificar qualquer moeda.
  perform verifica_feriado using wg_cadger-dt_lct wg_cadger-waers ''  '1000'.
*** US - 76596 - Fim - CBRAND
endform.                    "verifica_erros_cria

*&---------------------------------------------------------------------*
*&      Form  verifica_erros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form verifica_erros .
  data: wlinha(3),
        wmensagem(50),
        vsaldo_cre type zfit0036-vlr_pgto.

*  DATA: vg_data_pgt TYPE sy-datum.

  if not 'C_P' cs wg_cadbai-tipo.
    message 'Tipo inválido.'(259) type 'I'.
    wl_erro = 'X'.
    exit.
  elseif not '01_02_03' cs wg_cadbai-opera.
    message 'Operação inválida.'(260) type 'I'.
    wl_erro = 'X'.
    exit.
  endif.



  if wg_cadbai-rg_atualizado ne 'T'.
    if wg_cadbai-tipo       is initial or
        wg_cadbai-opera      is initial or
        wg_cadbai-moeda      is initial or
        wg_cadbai-motivo     is initial or
        wg_cadbai-descricao  is initial.
      message 'Informe os parâmetros.'(261) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
    if wg_cadbai-dt_lct     is initial.
      message 'Informe a data de lançamento'(262) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.

    if  wg_cadbai-dt_pgt     is initial.
      message 'Informe a data de pagamento'(263) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
  endif.


*  "Check data de pagamento CS2022000843 Bloqueio pagamento internacional fora do prazo de 48H / Anderson Oenning.
  select single *
  from zfit0179
  into @data(i_data)
  where usname_acesso eq @sy-uname.
  if sy-subrc ne 0.

    zcl_solicitacao_ov=>dia_util(
                                  exporting p_vencimento = wg_cadbai-dt_pgt
*                                            i_bukrs      = p_bukrs "USER STORY 158527 - MMSILVA - 17.01.2025
                                  importing e_subrc      = data(is_dia_util)
                                           ).

    if is_dia_util <> 4 and wg_cadbai-dt_pgt is not initial.
      message 'Data informada não é dia útil' type 'I'.
      wl_erro = 'X'.
      clear: is_dia_util.
      exit.
    endif.

    clear: vg_data_pgt, i_data.
    vg_data_pgt = sy-datum + 2.

    if wg_cadbai-dt_pgt < vg_data_pgt .
      message 'Data de pagamento está fora do prazo de 48h'(475) type 'I'.
      wl_erro = 'X'.
      exit.
    endif.
  endif.
*  "Fim CS2022000843.

  describe table tg_conta lines vlines.
  if wg_cadbai-rg_atualizado ne 'T'.
    if p_bukrs ne '0200'. " US 164319-26-02-2025-#164319-RJF
      if vlines eq 0.
        message 'Informe dados bancários'(264) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
    endif. " US 164319-26-02-2025-#164319-RJF

    loop at tg_conta into wg_conta.
      select single *
            from lfbk
            into wl_lfbk
            where lifnr = wg_conta-lifnr
            and banks   = wg_conta-banks
            and bankl   = wg_conta-bankl
            and bankn   = wg_conta-bankn.
      if sy-subrc ne 0.
        message s000(zwrm001) display like 'E' with 'Dados bancários incorretos'(149).
        wl_erro = 'X'.
        exit.
      endif.
    endloop.
  endif.


  loop at tg_externo into wg_externo.
    wlinha = sy-tabix.
*    IF WG_EXTERNO-DT_CRED IS NOT INITIAL AND WG_EXTERNO-DT_CRED LT SY-DATUM.
*      CONCATENATE 'Data Crédito inválida, linha' WLINHA INTO WMENSAGEM  SEPARATED BY SPACE.
*      MESSAGE WMENSAGEM TYPE 'I'.
*      WL_ERRO = 'X'.
*    ENDIF.
    if wg_cadbai-tipo = 'P' and wg_externo-line_color = 'C310'.
      concatenate 'INVOICE somente pode ser compensada, linha'(265) wlinha into wmensagem  separated by space.
      message wmensagem type 'I'.
      wl_erro = 'X'.
    endif.
    if ( wg_externo-dt_cred is initial and wg_externo-tx_cambio is not initial ).
      concatenate 'Data Crédito obrigatória, linha'(266) wlinha into wmensagem  separated by space.
      message wmensagem type 'I'.
      wl_erro = 'X'.
    elseif  ( wg_externo-dt_cred is not initial and wg_externo-tx_cambio is initial ).
      concatenate 'Taxa de câmbio obrigatória,  linha'(267) wlinha into wmensagem  separated by space.
      message wmensagem type 'I'.
      wl_erro = 'X'.
    elseif wg_externo-rg_atualizado eq 'T'.
*      VSALDO_CRE = ( WG_EXTERNO-VLR_PGT - WG_EXTERNO-VLR_CRE_B ).
*      IF WG_EXTERNO-VLR_CRE GT VSALDO_CRE .
*        CONCATENATE 'Valor de crédito inválido, linha' WLINHA INTO WMENSAGEM  SEPARATED BY SPACE.
*        MESSAGE WMENSAGEM TYPE 'I'.
*        WL_ERRO = 'X'.
*      ENDIF.
    elseif wg_externo-rg_atualizado ne 'T'.
      if wg_externo-vlr_pgt gt wg_externo-vlr_doc.
        concatenate 'Valor de pagamento inválido, linha'(268) wlinha into wmensagem  separated by space.
        message wmensagem type 'I'.
        wl_erro = 'X'.
      endif.
    endif.
  endloop.

endform.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form envia_email using plinha.

  field-symbols: <fs_solix> type solix.

* Objetos para enviar email
  data: objpack     like sopcklsti1 occurs  2 with header line.
  data: objhead     like solisti1   occurs  1 with header line.
  data: objbin_ord  like solisti1   occurs 10 with header line.
  data: objbin_log  like solisti1   occurs 10 with header line.
  data: objbin_ann  type solisti1.
  data: objbin    like solisti1   occurs 10 with header line,
        objbin1   type soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin like line of objbin.
  data: content_hex type standard table of solix with header line.
  data: objtxt      like solisti1   occurs 10 with header line.
  data: reclist     like somlreci1  occurs  5 with header line.
  data: doc_chng    like sodocchgi1.
  data: tab_lines   like sy-tabix.
  data: l_anex      type string.
  data: l_leng      type i.
  data: l_arq       type string.
  data: l_tam       type i.
  data: l_tam_ord   type i.
  data: l_tam_log   type i.
  data: l_email(300) type c.
  data: vlinha      type i.
  data: vuser       type sy-uname.
  data: it_shortcut_param like zst_shortcut_par occurs 0 with header line.
  data: content type string.
  data v_msg              type char50.


  data: t_lotes  type table of ziv_lotes_imp,
        t_lotes2 type table of zfi_lotes_imp,
        t_estra  type table of zfi_estrategia_imp,
        w_estra  type          zfi_estrategia_imp,
        t_docs   type table of ziv_docs_imp,
        w_docs   type          ziv_docs_imp.

*  ** Pass the required parameters and create the shortcut
  clear it_shortcut_param.
  refresh it_shortcut_param.


  "Estratégia salva
  data:   vflg_ico(1),
          vtipo(2),
          vvalor_ate type zinv_lotes_aprov-valor_ate.

  read table it_saida into wa_saida index plinha.

  refresh: tg_estra.

  "alrs

  call function 'Z_IV_ESTRATEGIA_LISTA'
    exporting
      v_usuario = sy-uname
      lote      = wa_saida-lote "PBI - 74866 - CBRAND
    importing
      msg       = v_msg
    tables
      t_lotes   = t_lotes
      t_estra   = t_estra
      t_docs    = t_docs.
  sort t_estra by nivel aprovador.
  delete adjacent duplicates from t_estra comparing nivel aprovador.
  loop at t_estra into w_estra.
    if w_estra-lote = wa_saida-lote.
      move-corresponding w_estra to wa_estra.
      append wa_estra to tg_estra.
    endif.
  endloop.

  "alrs

*  sort it_zinv_aprovador by bukrs bukrs_ate tipo nivel.
*  if wa_saida-obj_key+0(1) = 'P'.
*    vtipo = '02'.
*  else.
*    vtipo = '01'.
*  endif.
*  vflg_ico = 'N'.
*
*  vvalor_ate = 0.
*
*  loop at it_zinv_aprovador into wa_zinv_aprovador.
*    if  wa_zinv_aprovador-bukrs_ate is initial.
*      if  wa_zinv_aprovador-bukrs ne wa_saida-bukrs.
*        continue.
*      endif.
*    elseif wa_zinv_aprovador-bukrs     gt wa_saida-bukrs or
*           wa_zinv_aprovador-bukrs_ate lt wa_saida-bukrs.
*      continue.
*    endif.
*    if vtipo = wa_zinv_aprovador-tipo.
*      if wa_saida-vlr_pgto > vvalor_ate.
*        vvalor_ate = wa_zinv_aprovador-valor_ate.
*      endif.
*    endif.
*  endloop.
*
*  loop at it_zinv_aprovador into wa_zinv_aprovador.
*    if  wa_zinv_aprovador-bukrs_ate is initial.
*      if  wa_zinv_aprovador-bukrs ne wa_saida-bukrs .
*        continue.
*      endif.
*    elseif wa_zinv_aprovador-bukrs     gt wa_saida-bukrs  or
*           wa_zinv_aprovador-bukrs_ate lt wa_saida-bukrs .
*      continue.
*    endif.
*    if wa_zinv_aprovador-valor_ate <= vvalor_ate.
*      wa_estra-bukrs        = wa_saida-bukrs.
*      wa_estra-lote         = wa_saida-lote.
*      wa_estra-valor_de     = wa_zinv_aprovador-valor_de.
*      wa_estra-valor_ate    = wa_zinv_aprovador-valor_ate.
*      wa_estra-aprovador    = wa_zinv_aprovador-aprovador.
*      wa_estra-nivel        = wa_zinv_aprovador-nivel.
*      if vtipo = wa_zinv_aprovador-tipo.
*        append wa_estra to tg_estra.
*      endif.
*    endif.
*  endloop.

  if tg_estra[] is initial.
    exit.
  endif.
  read table tg_estra into wa_estra index 1 . " Manda e-mail para o primeiro aprovador

  data: bsmtp_addr type adr6-smtp_addr.

  select single adr6~smtp_addr into bsmtp_addr
    from usr21
      inner join adr6
         on  usr21~addrnumber = adr6~addrnumber
        and usr21~persnumber = adr6~persnumber
            where usr21~bname = wa_estra-aprovador.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_ESTRA'.

* Assunto do Email
  doc_chng-obj_descr = 'Aprovação INVOICE - Contas a Pagar'(269).

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP, o lote  abaixo.'(270).
  append objtxt.
  clear objtxt.
  append objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo.'(271) .
  append objtxt.
  clear objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  append objtxt.
  clear objtxt.

  data: ctotal(20),
        vdata(10).
  read table it_zfit0036 into wa_zfit0036 with key lote = wa_estra-lote binary search.
  concatenate wa_zfit0036-dt_pgto+6(2) wa_zfit0036-dt_pgto+4(2) wa_zfit0036-dt_pgto+0(4) into vdata separated by '.'.

  write wa_saida-vlr_pgto to ctotal currency 'USD'.

  condense ctotal no-gaps.
  concatenate 'Empresa:'(186)  wa_saida-bukrs ' Lote:'(272) wa_saida-lote ' R$'(273) ctotal ' Venc.'(274)  vdata into objtxt separated by space.
  append objtxt.
  clear objtxt.

* Setar tamanho da mensagem
  describe table objtxt lines tab_lines.
  read table objtxt index tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  clear objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  append objpack.


  call function 'ZFM_CREATE_SHORTCUT'
    exporting
      recipient_user_id = wa_estra-aprovador
      transaction       = 'ZFI0019'
    importing
      content           = content
    tables
      shortcut_param    = it_shortcut_param.

  clear : tab_lines, objbin.
  concatenate content wa_objbin-line into wa_objbin-line.
  append  wa_objbin to objbin.

  describe table objbin lines tab_lines.
  objhead = 'ESTRATEGIA.SAP'.
  append objhead.


** Creation of the entry for the compressed attachment
  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 1.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'EXT'." SAP
  objpack-obj_name   = 'SAPSHORTCUTMAIL'.
  objpack-obj_descr  = 'ESTRATEGIA.SAP'.
  objpack-doc_size   = tab_lines * 255.
  append objpack.


* Alimentar destinatários do email
  if bsmtp_addr is initial.
    message 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.'(275) type 'I'.
    exit.
  endif.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  append reclist.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  call function 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    exporting
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    tables
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      others                     = 99.

  sy-uname = vuser.


endform.                    " ENVIA_EMAIL
*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_5000 output.
  set pf-status 'Z002'.
*  CALL METHOD CL_GUI_CFW=>DISPATCH.
  set titlebar '5000'.

*  IF WG_CADEST-FLAG = 'X'.
*    LOOP AT SCREEN.
*      IF SCREEN-NAME = 'WG_CADEST-INVOICE'.
*        SCREEN-INVISIBLE = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.


endmodule.                 " STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_5000 input.
  case ok-code.
    when 'GERAR'.

      if wg_cadest-question = ' ' and wg_cadest-dt_pgto is initial.
        message s000(zwrm001) display like 'E' with 'Informe a nova data de pagamento'(276).
        exit.
      endif.

      clear w_flag.
      w_cont = 0.
      data tabix5000 type sy-tabix.
      clear wl_erro.
      clear wa_saida .
      loop at it_saida into wa_saida where checkbox = 'X'.
        tabix5000  = sy-tabix.
        exit.
      endloop.
      clear wl_erro.
      if wa_saida is initial. "invoice baixa total adto
        if wg_cadest-augbl is initial.
          message s000(zwrm001) display like 'E' with 'Informe a compensação por adiantamento'(277).
          exit.
        endif.
        if wg_cadest-invoice is initial and wg_cadest-flag = 'X'.
          message s000(zwrm001) display like 'E' with 'Informe a invoice compensada!'(278).
          exit.
        elseif wg_cadest-flag = 'X'.
          select single *
            from zfit0036
            into @data(wzfit36)
            where bukrs   = @p_bukrs
            and   invoice = @wg_cadest-invoice
            and   status  = 'C'.
          if wg_cadest-augbl is initial.
            message s000(zwrm001) display like 'E' with 'Informe a invoice por adiantamento'(279).
            exit.
          else.
            wa_saida-lote = wzfit36-lote_cp.
          endif.
        endif.
      else.
        select single *
        from zfit0036
        into @data(_zfi36)
        where obj_key = @wa_saida-obj_key
        and   belnr   = @wa_saida-belnr36
        and   buzei   = @wa_saida-buzei36.
        "
      endif.

      if wa_saida-obj_key+0(2) ne 'AC'.
        perform f_shdb_est changing wa_saida wl_erro.
      elseif wg_cadest-question = 'X' and  wa_saida-lote is not initial.
        update zfit0036 set   status      = ''
                              lote        = ''
                              dt_pgto     = ''
                              moeda_pgto  = ''
                              tx_cambio   = 0
                              observacao  = ''
                              referencia  = ''
                              operacao    = ''
                              desp_tar    = ''
                              vlr_pgto    = 0
                              forma_pg    = ''
                              motivo      = ''
                              bvtyp       = ''
                              lifnr       = ''
                              swift_1     = ''
                              banks_1     = ''
                              bankl_1     = ''
                              banka_1     = ''
                              bankn_1     = ''
                              iban_1      = ''
                              bvtyp_2     = ''
                              swift_2     = ''
                              banks_2     = ''
                              bankl_2     = ''
                              banka_2     = ''
                              bankn_2     = ''
                              iban_2      = ''
        where lote = wa_saida-lote.
        commit work.

      else.
        "CS2021001169  Melhoria ZFI0017 - Estorno de pagamentos
        if wa_saida-forma_pg+0(1) = 'P' and  wa_saida-lote is not initial.
          update zfit0036 set status = 'A'
                           dt_pgto = wg_cadest-dt_pgto
                           rg_atualizado = ''
          where lote = wa_saida-lote.
          "CS2021001169  Melhoria ZFI0017 - Estorno de pagamentos
        elseif  wa_saida-lote is not initial.
          update zfit0036 set status = 'L'
                              dt_pgto = wg_cadest-dt_pgto
                              rg_atualizado = ''
          where lote = wa_saida-lote.
        endif.
        commit work.
      endif.

      if wl_erro ne 'X'.
        clear wa_saida-augbl.
        modify it_saida from wa_saida index tabix5000 transporting augbl.
        message 'Estorno realizado com sucesso!'(280) type 'I'.
        " VERIFICA SE HOUVE SPLIT E ESTORNA
        select single *
          from bsak
          into @data(wbsak)
          where bukrs = @wa_saida-bukrs
          and   belnr = @wa_saida-belnr
          and   augbl between 1500000000 and 1590000000.
        if sy-subrc ne 0. "NAO TEM PARC PAGAS
          select single *
            from zfit0036
            into @data(w36)
            where belnr = @wa_saida-belnr
            and   buzei = 2.
          if sy-subrc = 0.
            wg_cadest-flag = 'S'. "SPLIT
            select single *
                  from bsak
                  into wbsak
                  where bukrs eq wa_saida-bukrs
                  and   augbl eq wa_saida-belnr
                  and   belnr ne wa_saida-belnr.
            perform f_shdb_est changing wa_saida wl_erro.
            if wl_erro ne 'X'.
              update zfit0036 set status      = ''
                                  lote        = ''
                                  dt_pgto     = ''
                                  moeda_pgto  = ''
                                  tx_cambio   = 0
                                  observacao  = ''
                                  referencia  = ''
                                  operacao    = ''
                                  desp_tar    = ''
                                  vlr_pgto    = 0
                                  forma_pg    = ''
                                  motivo      = ''
                                  bvtyp       = ''
                                  lifnr       = ''
                                  swift_1     = ''
                                  banks_1     = ''
                                  bankl_1     = ''
                                  banka_1     = ''
                                  bankn_1     = ''
                                  iban_1      = ''
                                  bvtyp_2     = ''
                                  swift_2     = ''
                                  banks_2     = ''
                                  bankl_2     = ''
                                  banka_2     = ''
                                  bankn_2     = ''
                                  iban_2      = ''
              where obj_key = w36-obj_key
              and   belnr   = wbsak-belnr
              and   buzei   = wbsak-buzei.
              if sy-subrc ne 0.
                update zfit0036 set
                          status      = ''
                          lote        = ''
                          dt_pgto     = ''
                          moeda_pgto  = ''
                          tx_cambio   = 0
                          observacao  = ''
                          referencia  = ''
                          operacao    = ''
                          desp_tar    = ''
                          vlr_pgto    = 0
                          forma_pg    = ''
                          motivo      = ''
                          bvtyp       = ''
                          lifnr       = ''
                          swift_1     = ''
                          banks_1     = ''
                          bankl_1     = ''
                          banka_1     = ''
                          bankn_1     = ''
                          iban_1      = ''
                          bvtyp_2     = ''
                          swift_2     = ''
                          banks_2     = ''
                          bankl_2     = ''
                          banka_2     = ''
                          bankn_2     = ''
                          iban_2      = ''
                 where obj_key = w36-obj_key
                 and   belnr   = ''
                 and   buzei   = 0.
              endif.
              if sy-subrc eq 0.
                commit work.
                message 'Estorno(SPLIT) realizado com sucesso!'(374) type 'I'.
              endif.

            endif.
          endif.

        endif.
        " VERIFICA SE HOUVE SPLIT E ESTORNA
        delete  it_saida where checkbox = 'X'.
        set screen 0.
      else.
        message s000(zwrm001) display like 'E' with 'Erro ao estornar compensação'(281).
      endif.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_EST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_SAIDA  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_est  changing p_alv like wa_saida p_erro.
  refresh ti_bdcdata.
  data vgjahr  type rf05r-gjahr.
  vgjahr = p_alv-dt_pgto+0(4).
  if wg_cadest-flag = 'S'.
    vgjahr      = wa_saida-budat+6(4).
    p_alv-bukrs = p_bukrs.
    p_alv-augbl = wa_saida-belnr.
  elseif wg_cadest-flag = 'X'.
    vgjahr      = wg_cadest-dt_pgto+0(4).
    p_alv-bukrs = p_bukrs.
    p_alv-augbl = wg_cadest-augbl.
  else.
    select single *
          from zfit0036
          into @data(_zfi36)
          where obj_key = @p_alv-obj_key
          and   belnr   = @p_alv-belnr36
          and   buzei   = @p_alv-buzei36.

    if _z36-status_arq_inv eq 'A'.
      if vgjahr is initial.
        vgjahr = p_alv-gjahr.
      endif.
      p_alv-lote  = _zfi36-lote_cp.
      p_alv-augbl = p_alv-belnr.
    endif.
  endif.


  perform f_bdc_data using:
    'SAPMF05R'  '0100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=RAGL',
    ''          ''      ''   'RF05R-AUGBL'      p_alv-augbl,
    ''          ''      ''   'RF05R-BUKRS'      p_alv-bukrs,
    ''          ''      ''   'RF05R-GJAHR'      vgjahr ,
    'SAPLSPO2'  '0100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=OPT2',
    'SAPMF05R'  '0300'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=ENTR',
    ''          ''      ''   'RF05R-STGRD'      '01',
    'SAPMF05R'  '0100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=EZUR',
    ''          ''      ''   'RF05R-BUKRS'      p_alv-bukrs,
    ''          ''      ''   'RF05R-GJAHR'      vgjahr ,
    'SAPMF05R'  '0100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '/EEEND'.
  clear p_erro.
  perform zf_call_transaction using 'FBRA' changing p_erro.
  if p_erro = 'X'.
    rollback work.
  else.
    commit work.
    wait up to 3 seconds. " espera
    if p_alv-forma_pg+0(1) = 'C' and  p_alv-lote_cp is not initial. " se for compensacao volta inicial sempre
      update zfit0036 set forma_pg = ''
               motivo   = ''
               status   = ''
               status_ctas_rec = ''
               rg_atualizado = ''
               dt_pgto     = ''
               moeda_pgto  = ''
               tx_cambio   = 0
               observacao  = ''
               referencia  = ''
               operacao    = ''
               desp_tar    = ''
               vlr_pgto    = 0
               bvtyp       = ''
               lifnr       = ''
               swift_1     = ''
               banks_1     = ''
               bankl_1     = ''
               banka_1     = ''
               bankn_1     = ''
               iban_1      = ''
               bvtyp_2     = ''
               swift_2     = ''
               banks_2     = ''
               bankl_2     = ''
               banka_2     = ''
               bankn_2     = ''
               iban_2      = ''
               lote        = 0
               lote_cp     = 0
     where lote_cp  = p_alv-lote_cp.
    elseif _z36-status_arq_inv eq 'A' or wg_cadest-flag = 'X' and p_alv-lote is not initial.
      update zfit0036 set forma_pg = ''
                motivo   = ''
                status   = ''
                status_ctas_rec = ''
                rg_atualizado = ''
                dt_pgto     = ''
                moeda_pgto  = ''
                tx_cambio   = 0
                observacao  = ''
                referencia  = ''
                operacao    = ''
                desp_tar    = ''
                vlr_pgto    = 0
                bvtyp       = ''
                lifnr       = ''
                swift_1     = ''
                banks_1     = ''
                bankl_1     = ''
                banka_1     = ''
                bankn_1     = ''
                iban_1      = ''
                bvtyp_2     = ''
                swift_2     = ''
                banks_2     = ''
                bankl_2     = ''
                banka_2     = ''
                bankn_2     = ''
                iban_2      = ''
      where lote_cp  = p_alv-lote
      and   status   = 'C'.
      "
      "
      if wg_cadest-flag ne 'X'.
        update zfit0036 set eliminar = 'X'
        where lote_cp  = p_alv-lote
        and   belnr    = p_alv-belnr36
        and   status  <>  'C'.
      endif.
      commit work.
      commit work.
    elseif wg_cadest-question = 'X' and  p_alv-lote is not initial.
      update zfit0036 set forma_pg = ''
               motivo   = ''
               status   = ''
               status_ctas_rec = ''
               rg_atualizado = ''
               dt_pgto     = ''
               moeda_pgto  = ''
               tx_cambio   = 0
               observacao  = ''
               referencia  = ''
               operacao    = ''
               desp_tar    = ''
               vlr_pgto    = 0
               bvtyp       = ''
               lifnr       = ''
               swift_1     = ''
               banks_1     = ''
               bankl_1     = ''
               banka_1     = ''
               bankn_1     = ''
               iban_1      = ''
               bvtyp_2     = ''
               swift_2     = ''
               banks_2     = ''
               bankl_2     = ''
               banka_2     = ''
               bankn_2     = ''
               iban_2      = ''
               lote        = 0
               lote_cp     = 0
     where lote = p_alv-lote.
    elseif p_alv-lote is not initial.
      update zfit0036 set status = 'L'
                          dt_pgto = wg_cadest-dt_pgto
                          rg_atualizado = ''
      where lote = p_alv-lote.
    endif.
    commit work.
    refresh ti_bdcdata.
    perform f_bdc_data using:
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=BU',
          ''          ''      ''   'RF05A-BELNS'      p_alv-augbl,
          ''          ''      ''   'BKPF-BUKRS'       p_alv-bukrs,
          ''          ''      ''   'RF05A-GJAHS'      vgjahr,
          ''          ''      ''   'UF05A-STGRD'      '01'.

    clear p_erro.
    perform zf_call_transaction using 'FB08' changing p_erro.
    if p_erro = 'X'.
      rollback work.
      message 'Erro ao estornar FB08'(282) type 'I'.
    else.
      commit work.
    endif.
  endif.
endform.                    " F_SHDB_EST
*&---------------------------------------------------------------------*
*&      Module  STATUS_6000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_6000 output.
  set pf-status 'Z001'.
  set titlebar '6000'.
endmodule.                 " STATUS_6000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS_6000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields_6000 output.

endmodule.                 " TRATA_FIELDS_6000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_6000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_6000 output.
  "DATA: EVENT TYPE CNTL_SIMPLE_EVENT,

  "TL_FILTER           TYPE LVC_T_FILT,
  "WL_FILTER           TYPE LVC_S_FILT,
  "TL_FUNCTION         TYPE UI_FUNCTIONS,
  "WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE.
  clear wa_layout.
  wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
  wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
  wa_stable-row        = c_x.
  wa_layout-info_fname = 'COLOR'.

  wa_layout-no_toolbar = c_x.
  wa_layout-stylefname = 'STYLE2'.
  wa_layout-grid_title = ' '.
  "GRID3
  if obg_conteiner_cp is initial.
    create object obg_conteiner_cp
      exporting
        container_name = g_cc_pagar.


    create object grid3
      exporting
        i_parent = obg_conteiner_cp.


    perform montar_layout_cp.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = c_x.


    call method grid3->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_pagar[].

    call method grid3->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>on_data_changed_finished_cp for grid3,
              lcl_event_handler=>on_data_changed_cp for grid3.

  else.
    call method grid3->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = t_fieldcatalog[].
    loop at t_fieldcatalog into w_fieldcatalog
     where fieldname eq 'VLR_PGTO'
        or fieldname eq 'VLR_COMP'
        or fieldname eq 'VLR_SLD'.
      w_fieldcatalog-outputlen = '15'.
      modify t_fieldcatalog from w_fieldcatalog.
    endloop.
    call method grid3->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid3->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  "GRID2
  if obg_conteiner_cr is initial.
    create object obg_conteiner_cr
      exporting
        container_name = g_cc_receber.


    create object grid4
      exporting
        i_parent = obg_conteiner_cr.


    perform montar_layout_cr.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = c_x.


    call method grid4->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_receber[].

    call method grid4->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>on_data_changed_finished_cr for grid4,
              lcl_event_handler=>on_data_changed_cr for grid4.

  else.
    call method grid4->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = t_fieldcatalog[].
    loop at t_fieldcatalog into w_fieldcatalog
     where fieldname eq 'VLR_DOC'
        or fieldname eq 'VLR_COMP'
        or fieldname eq 'VLR_SLD'.
      w_fieldcatalog-outputlen = '15'.
      modify t_fieldcatalog from w_fieldcatalog.
    endloop.
    call method grid4->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid4->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

endmodule.                 " CRIA_OBJETOS_6000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_6000 input.
  clear wl_erro.
  data: vlinhacp(3),
        vmsgcp(50),
        vqtdecp     type i,

* ---> S4 Migration - 10/06/2023 - DG
*        VTOTAL_CP   type ZFIT0036-VLR_PGTO,
*        VTOTAL_CR   type ZFIT0036-VLR_PGTO,
*        VTOTAL_AD   type ZFIT0036-VLR_PGTO.
        vtotal_cp   type bsid_view-dmbe2,
        vtotal_cr   type bsid_view-dmbe2,
        vtotal_ad   type bsid_view-dmbe2,
        vwaerscp    type zib_contabil-waers.
* <--- S4 Migration - 10/06/2023 - DG

  case ok-code.
    when 'GERAR'.
      vtotal_cp = 0.
      vqtdecp = 0.
** US - 76596 - CBRAND - Inicio
      perform verifica_feriado using wg_cadcom-dt_pgto '' '' '6000'.
** US - 76596 - CBRAND - Fim
      if wg_cadcom-dt_pgto is initial.
        message 'Informe a data de pagamento'(263) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      loop at tg_pagar into wg_pagar.
        if wg_pagar-vlr_comp gt 0.
          add 1 to vqtdecp.
          add wg_pagar-vlr_comp to vtotal_cp .
        endif.
        if wg_pagar-vlr_sld lt 0.
          vlinhacp = sy-tabix.
          concatenate 'Saldo negativo PAGAR, linha'(283) vlinhacp into vmsgcp separated by space.
          message vmsgcp type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        exit.
      endif.
      if vtotal_cp eq 0.
        message 'Informe valor compensação CONTAS A PAGAR '(284) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      if vqtdecp ne 1.
        message 'Informe valor compensação CONTAS A PAGAR, um de cada vez '(285) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      vtotal_cr = 0.
      loop at tg_receber into wg_receber. " WHERE CHECKBOX = 'X' .
        add wg_receber-vlr_doc to vtotal_cr.
      endloop.

      if vtotal_cp gt vtotal_cr.
        message 'Total compensação CONTAS A PAGAR maior que total CONTAS A RECEBER '(286) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      if vtotal_cr eq 0.
        message 'CONTAS A RECEBER sem valores a compensar'(287) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      clear wl_erro.
      perform f_shdb_iva changing wg_pagar wl_erro.
      if wl_erro eq 'X' .
        message 'Erro na execução do processo (SHDB)'(288) type 'I'.
        set screen 0.
        exit.
      endif.
      wg_atualiza_shdb = 'X'.
      " Marca status compensado 'C' em todas a INVOICE PAGAR e RECEBER que tiveram Valores compensados.
      sort: tg_receber by belnr,
            tg_pagar   by belnr.
      tg_compe_aux[] = tg_compe[].
      "
      call function 'NUMBER_GET_NEXT'
        exporting
          nr_range_nr = '01'
          object      = 'ZID_INV'
        importing
          number      = vseq.
      vnum = vseq .

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vnum
        importing
          output = vnum.
      "
      sort tg_compe_aux by belnr_cp belnr_cr.
      delete adjacent duplicates from tg_compe_aux comparing belnr_cp belnr_cr.
      loop at tg_compe_aux into wg_compe.
        read table tg_receber into wg_receber with key belnr = wg_compe-belnr_cr binary search.
        update zfit0036 set status_ctas_rec = 'C'
                            lote_cp = vnum
        where obj_key = wg_receber-obj_key.

        read table tg_pagar into wg_pagar with key belnr = wg_compe-belnr_cp binary search.
        if sy-subrc = 0.
          wg_pagar-fg_ok = 'X'.
          modify tg_pagar from wg_pagar index sy-tabix  transporting fg_ok.
          update zfit0036 set status = 'C'
                              lote_cp = vnum
          where obj_key = wg_pagar-obj_key
          and belnr   = wg_pagar-belnr36
          and buzei   = wg_pagar-buzei36.
        endif.

      endloop.
      commit work.

      if wl_erro ne 'Z'. " Sem saldo - não executou o SHDB - se executou gravar na ZFIT0036
        loop at tg_pagar into wg_pagar.
          if wg_pagar-vlr_comp gt 0  and wg_pagar-vlr_sld gt 0.
            clear wa_zfit0036_ins.
            wa_zfit0036_ins-obj_key  = wg_pagar-obj_key .
            wa_zfit0036_ins-bukrs    = p_bukrs.
            wa_zfit0036_ins-belnr    = wg_documento.
            wa_zfit0036_ins-invoice  = wg_pagar-invoice.
            wa_zfit0036_ins-navio    = wg_pagar-navio.
            wa_zfit0036_ins-lote_cp  = vnum.

            insert into  zfit0036 values wa_zfit0036_ins.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
          endif.
        endloop.

        loop at tg_receber into wg_receber.
          if wg_receber-vlr_comp gt 0  and wg_receber-vlr_sld gt 0.
            clear wa_zfit0036_ins.
            wa_zfit0036_ins-obj_key  = wg_receber-obj_key .
            wa_zfit0036_ins-bukrs    = p_bukrs.
            wa_zfit0036_ins-belnr    = wg_documento.
            wa_zfit0036_ins-invoice  = wg_receber-invoice.
            wa_zfit0036_ins-lote_cp  = vnum.
            "WA_ZFIT0036_INS-NAVIO    = WG_receber-NAVIO.

            insert into  zfit0036 values wa_zfit0036_ins.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
          endif.
        endloop.
      endif.
      message 'Processo realizado com sucesso'(290) type 'I'.
      set screen 0.

    when 'BTN_COMP'.
      vtotal_cp = 0.
      loop at tg_pagar into wg_pagar.
        add wg_pagar-vlr_comp to vtotal_cp .
        if wg_pagar-vlr_sld lt 0.
          vlinhacp = sy-tabix.
          concatenate 'Saldo negativo PAGAR, linha'(283) vlinhacp into vmsgcp separated by space.
          message vmsgcp type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        exit.
      endif.
      if vtotal_cp eq 0.
        message 'Informe valor compensação CONTAS A PAGAR '(284) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      vtotal_cr = 0.
      loop at tg_receber into wg_receber where checkbox = 'X' .
        add wg_receber-vlr_doc to vtotal_cr.
      endloop.

      if vtotal_cp gt vtotal_cr.
        message 'Total compensação CONTAS A PAGAR maior que total CONTAS A RECEBER '(286) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      refresh tg_compe.
      sort tg_receber by vlr_doc.
      loop at tg_receber into wg_receber.
        wg_receber-vlr_comp = 0.
        wg_receber-vlr_sld  = 0.
        modify tg_receber from wg_receber index sy-tabix transporting vlr_comp vlr_sld .
      endloop.
      loop at tg_pagar into wg_pagar.
        if wg_pagar-vlr_comp eq 0.
          continue.
        endif.
        xvlrpg = wg_pagar-vlr_comp.
        loop at tg_receber into wg_receber where checkbox = 'X' .
          vtotal_cr = wg_receber-vlr_doc - wg_receber-vlr_comp.
          if vtotal_cr gt 0 .
            if xvlrpg gt vtotal_cr.
              subtract vtotal_cr from xvlrpg.
              if wg_receber-vlr_comp eq 0.
                wg_receber-vlr_comp = vtotal_cr.
              else.
                add vtotal_cr to wg_receber-vlr_comp.
              endif.
            else.
              if wg_receber-vlr_comp eq 0.
                wg_receber-vlr_comp = xvlrpg.
              else.
                add xvlrpg to wg_receber-vlr_comp.
              endif.
              xvlrpg = 0.
            endif.
            wg_receber-vlr_sld  =  wg_receber-vlr_doc  - wg_receber-vlr_comp.
            modify tg_receber from wg_receber index sy-tabix transporting vlr_comp vlr_sld .
            wg_compe-belnr_cp = wg_pagar-belnr.
            wg_compe-belnr_cr = wg_receber-belnr.
*---> 08/06/2023 - Migração S4 - JS
*            WG_COMPE-VLR_COMP = WG_RECEBER-VLR_COMP.
            wg_compe-vlr_comp = conv #( wg_receber-vlr_comp ).
*<--- 08/06/2023 - Migração S4 - JS

            append wg_compe to tg_compe.
            "WG_ATUALIZA = 'X'.
          endif.
        endloop.
        delete tg_compe where vlr_comp = 0.
      endloop.

      call method grid4->refresh_table_display
        exporting
          is_stable = wa_stable.

    when 'BTNC'.
      if wg_cadcom-kunnr is not initial.
        select   * "obj_key belnr buzei bukrs invoice navio lote dt_pgto tx_cambio moeda_pgto vlr_pgto hbkid observacao status forma_pg motivo rg_atualizado bvtyp operacao
        from zfit0036
        into corresponding fields of table it_zfit0036_com
        where status_ctas_rec	= ''
        and   bukrs = p_bukrs.


        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wg_cadcom-kunnr
          importing
            output = wg_cadcom-kunnr.

        if it_zfit0036_com[] is not initial.
          loop at tg_pagar into wg_pagar.
*            if wg_pagar-vlr_comp gt 0.
            select single waers
              into vwaerscp
              from zib_contabil
              where obj_key = wg_pagar-obj_key.
            exit.
*            endif.
          endloop.

          select zib_contabil_chv~obj_key zib_contabil_chv~belnr zib_contabil_chv~bukrs zib_contabil~hkont zib_contabil_chv~gjahr
           from zib_contabil_chv
           inner join zib_contabil
           on zib_contabil~obj_key eq zib_contabil_chv~obj_key
           and zib_contabil~hkont = wg_cadcom-kunnr
           and zib_contabil~waers = vwaerscp "somente filtra docs mesma moeda pagar/receber US164319
           into table  it_zib_contabil_chv_com
           for all entries in it_zfit0036_com
           where zib_contabil_chv~obj_key eq it_zfit0036_com-obj_key.

          if it_zib_contabil_chv_com[] is not initial.
*"US172348
*            SELECT  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei waers budat blart
            select  bukrs belnr gjahr dmbtr wrbtr kunnr buzei waers budat blart
                from bsid
                into table it_bsid_com
                for all entries in it_zib_contabil_chv_com
                where bukrs eq  it_zib_contabil_chv_com-bukrs
                and   belnr eq  it_zib_contabil_chv_com-belnr
                and   gjahr eq  it_zib_contabil_chv_com-gjahr.

*"US172348
*            select  bukrs belnr gjahr dmbtr dmbe2 kunnr buzei waers budat blart
            select  bukrs belnr gjahr dmbtr wrbtr kunnr buzei waers budat blart
               from bsid
               appending table it_bsid_com
               for all entries in it_zfit0036_com
               where bukrs eq  it_zfit0036_com-bukrs
               and   belnr eq  it_zfit0036_com-belnr36
               and   kunnr eq  wg_cadcom-kunnr.
          endif.
        endif.

        refresh tg_receber.
        if it_bsid_com[] is initial.
          message 'Não existem documentos deste cliente a compensar'(289) type 'I'.
        else.
          it_zfit0036_com36[] = it_zfit0036_com[].
          sort: it_zib_contabil_chv_com   by bukrs belnr gjahr,
                it_zfit0036_com           by obj_key,
                it_zfit0036_com36         by bukrs belnr36.
          clear wg_receber.
          loop at it_bsid_com into wa_bsid.
            wg_receber-budat      = wa_bsid-budat.
            wg_receber-belnr      = wa_bsid-belnr.

            read table it_zib_contabil_chv_com into wa_zib_contabil_chv with key bukrs = wa_bsid-bukrs
                                                                                 belnr = wa_bsid-belnr
                                                                                 gjahr = wa_bsid-gjahr binary search.
            if sy-subrc = 0.
              read table it_zfit0036_com into wa_zfit0036 with key obj_key = wa_zib_contabil_chv-obj_key binary search.
              wg_receber-invoice    = wa_zfit0036-invoice.
            else.
              read table it_zfit0036_com36 into wa_zfit0036 with key bukrs   =  wa_bsid-bukrs
                                                                     belnr36 = wa_bsid-belnr binary search.
              if sy-subrc  = 0.
                wg_receber-invoice    = wa_zfit0036-invoice.
              endif.
            endif.
            wg_receber-vlr_doc    = wa_bsid-dmbe2.
            wg_receber-vlr_comp   = 0.
            wg_receber-vlr_sld    = wa_bsid-dmbe2.
            wg_receber-obj_key    = wa_zfit0036-obj_key.
            append wg_receber to tg_receber.
            clear wg_receber.
          endloop.

          sort tg_receber by vlr_doc.
        endif.
      endif.

    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_6000  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_cp .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 ' LIFNR'           'LFA1'        'TG_PAGAR' 'LIFNR'           'Fornecedor'(006)       '12' ' ' ' ' ' ',
        2 ' NAME1'           'LFA1'        'TG_PAGAR' 'NAME1'           'Nome Fornecedor'(007)  '30' ' ' ' ' ' ',
        3 ' '                ' '           'TG_PAGAR' 'BUDAT'           'Dt.Lcto'(005)          '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_PAGAR' 'BELNR'           'Documento'(202)        '12' ' ' ' ' ' ',
        5 ' '                ' '           'TG_PAGAR' 'INVOICE'         'Invoice'(003)          '12' ' ' ' ' ' ',
        6 ' '                ' '           'TG_PAGAR' 'VLR_PGTO'        'Valor Documento'(205)  '15' ' ' 'X' ' ',
        7 'BSID'             'DMBE2'       'TG_PAGAR' 'VLR_COMP'        'Vlr.Compensação'(291)  '15' 'X' 'X' ' ',
        8 ' '                ' '           'TG_PAGAR' 'VLR_SLD'         'Saldo Residual'(209)   '15' ' ' 'X' ' '.

endform.                    " MONTAR_LAYOUT_CP
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_cr .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 '  '           ' '              'TG_RECEBER' 'CHECKBOX'      'Chk'(213)                '05' 'X' ' ' ' ',
        1 '  '           ' '              'TG_RECEBER' 'BUDAT'         'Dt.Lcto'(005)            '10' ' ' ' ' ' ',
        2 '  '           ' '              'TG_RECEBER' 'BELNR'         'Documento'(202)          '12' ' ' ' ' ' ',
        3 '  '           ' '              'TG_RECEBER' 'INVOICE'       'Invoice'(003)            '12' ' ' ' ' ' ',
        4 ' '            ' '              'TG_RECEBER' 'VLR_DOC'       'Valor Documento'(205)    '15' ' ' 'X' ' ',
        5 'BSID'         'DMBE2'          'TG_RECEBER' 'VLR_COMP'      'Vlr.Compensação'(291)    '15' ' ' 'X' ' ',
        6 ' '            ' '              'TG_RECEBER' 'VLR_SLD'       'Saldo Residual'(209)     '15' ' ' 'X' ' '.
endform.                    " MONTAR_LAYOUT_CR
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_IVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_EXTERNO  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_iva   changing p_alv like wg_pagar p_erro.
  data: vdata(10),
        vcampo(15),
        cnum_seq(2),
        vnum_seq    type i,
        vsaldo      type bsid_view-dmbe2,
        wl_vlr(16).


  " Verifica se o saldo é no contas a pagar
  vtotal_cp = 0.
  loop at tg_pagar into wg_pagar.
    if wg_pagar-vlr_comp gt 0  and wg_pagar-vlr_sld gt 0.
      add wg_pagar-vlr_sld to vtotal_cp.
    endif.
  endloop.

  " Verifica se o saldo é no contas a receber
  vtotal_cr = 0.
  loop at tg_receber into wg_receber.
    if wg_receber-vlr_comp gt 0  and wg_receber-vlr_sld gt 0.
      add wg_receber-vlr_sld to vtotal_cr.
    endif.
  endloop.


  if p_bukrs ne '0200'.
    if vtotal_cp = 0 and vtotal_cr = 0 .
      p_erro = 'Z'.
      exit.
    endif.
  endif.

  refresh ti_bdcdata.
  concatenate  wg_cadcom-dt_pgto+6(2) wg_cadcom-dt_pgto+4(2) wg_cadcom-dt_pgto(4) into vdata  separated by '.'.

  if vtotal_cp gt 0.
    vtotal_cp = vtotal_cp * -1.
  endif.

  vsaldo = vtotal_cp + vtotal_cr.
  write: vsaldo  to wl_vlr.

  condense wl_vlr no-gaps.

  select single *
    from zib_contabil
    inner join tbsl
    on  tbsl~bschl = zib_contabil~bschl
    and tbsl~koart = 'K'
    into corresponding fields of wa_zib_contabil
    where obj_key eq p_alv-obj_key.


  perform f_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SL',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       wa_zib_contabil-blart,
    ''          ''      ''   'BKPF-BUKRS'       wa_zib_contabil-bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadcom-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       wa_zib_contabil-waers,

    'SAPMF05A'  '0710'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'RF05A-AGBUK'      wa_zib_contabil-bukrs,
    ''          ''      ''   'RF05A-AGKON'      wa_zib_contabil-hkont,
    ''          ''      ''   'RF05A-AGKOA'      'K',
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SLK'.

  vnum_seq = 0.
  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cp.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cp.

  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cp.
  endloop.

  perform f_bdc_data using:
     'SAPMF05A'  '0710'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'RF05A-AGBUK'      wa_zib_contabil-bukrs,
      ''          ''      ''   'RF05A-AGKON'      wg_cadcom-kunnr,
      ''          ''      ''   'RF05A-AGKOA'      'D',
      ''          ''      ''   'RF05A-XNOPS'      'X',
      ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
      ''          ''      ''   'RF05A-XPOS1(03)'  'X',

      'SAPMF05A'  '0731'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PA'.

  vnum_seq = 0.
  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cr.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cr.
  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cr.
  endloop.

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=REST',
         ''          ''      ''   'BDC_SUBSCR'        'SAPDF05X                                6102PAGE',
         ''          ''      ''   'RF05A-ABPOS'      '1'.

  if vsaldo lt 0.
    perform f_bdc_data using:
           'SAPDF05X'  '3100'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'        '=PI',
           ''          ''      ''   'BDC_SUBSCR'        'SAPDF05X                                6106PAGE',
           ''          ''      ''   'RF05A-ABPOS'      '1',
           ''          ''      ''   'DF05B-PSDIF(01)'  wl_vlr.
  else.
    perform f_bdc_data using:
           'SAPDF05X'  '3100'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/00',
           ''          ''      ''   'BDC_SUBSCR'        'SAPDF05X                                6106PAGE',
           ''          ''      ''   'RF05A-ABPOS'      '1',
           ''          ''      ''   'DF05B-PSDIF(02)'  wl_vlr.
  endif.

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=BS',

         'SAPMF05A'  '0700'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=BU'.
  "ENDLOOP.


  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.

endform.                    " F_SHDB_IVA
*&---------------------------------------------------------------------*
*&      Module  STATUS_7000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_7000 output.
  set pf-status 'Z001'.
  set titlebar '7000'.
endmodule.                 " STATUS_7000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_7000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_7000 output.
  clear wa_layout.
  wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
  wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
  wa_stable-row        = c_x.
  wa_layout-info_fname = 'COLOR'.

  wa_layout-no_toolbar = c_x.
  wa_layout-stylefname = 'STYLE2'.
  wa_layout-grid_title = ' '.
  "GRID3
  if obg_conteiner_adt is initial.
    create object obg_conteiner_adt
      exporting
        container_name = g_cc_adiant.


    create object grid5
      exporting
        i_parent = obg_conteiner_adt.


    perform montar_layout_adt.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = c_x.


    call method grid5->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_adiant[].

    call method grid5->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>on_data_changed_finished_adt for grid5,
              lcl_event_handler=>on_data_changed_adt for grid5.

  else.
    call method grid5->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = t_fieldcatalog[].
    loop at t_fieldcatalog into w_fieldcatalog
     where fieldname eq 'VLR_PGTO'
        or fieldname eq 'VLR_COMP'
        or fieldname eq 'VLR_SLD'.
      w_fieldcatalog-outputlen = '15'.
      modify t_fieldcatalog from w_fieldcatalog.
    endloop.
    call method grid5->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid5->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  "GRID2
  if obg_conteiner_inv is initial.
    create object obg_conteiner_inv
      exporting
        container_name = g_cc_invoice.


    create object grid6
      exporting
        i_parent = obg_conteiner_inv.


    perform montar_layout_inv.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = c_x.


    call method grid6->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_invoice[].

    call method grid6->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  else.
    call method grid6->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = t_fieldcatalog[].
    loop at t_fieldcatalog into w_fieldcatalog
     where fieldname eq 'VLR_DOC'
        or fieldname eq 'VLR_COMP'
        or fieldname eq 'VLR_SLD'.
      w_fieldcatalog-outputlen = '15'.
      modify t_fieldcatalog from w_fieldcatalog.
    endloop.
    call method grid6->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid6->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

endmodule.                 " CRIA_OBJETOS_7000  OUTPUT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_7000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_7000 input.
  clear wl_erro.
  data: vlinhaadt(3),
        vqtdeadt     type i,
        vmsgadt(50),
* ---> S4 Migration - 10/06/2023 - DG
*        VTOTAL_ADT   type ZFIT0036-VLR_PGTO,
*        VTOTAL_INV   type ZFIT0036-VLR_PGTO.
        vtotal_adt   type bsid_view-dmbe2,
        vtotal_inv   type bsid_view-dmbe2.
* <--- S4 Migration - 10/06/2023 - DG







  case ok-code.
    when 'GERAR'.
      vtotal_adt = 0.
      vqtdeadt = 0.
      clear wl_erro .
      if wg_cadadt-dt_pgto is initial.
        message 'Informe a data de pagamento'(263) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      loop at tg_adiant into wg_adiant.
        if wg_adiant-vlr_comp gt 0.
          add wg_adiant-vlr_comp to vtotal_adt.
          add 1 to vqtdeadt.
        endif.
        if wg_adiant-vlr_sld lt 0.
          vlinhaadt = sy-tabix.
          concatenate 'Saldo negativo ADIANTAMENTO, linha'(292) vlinhaadt into vmsgadt separated by space.
          message vmsgadt type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        exit.
      endif.
      if vtotal_adt eq 0.
        message 'Informe valor compensação ADIANTAMENTO '(293) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      if vqtdeadt ne 1.
        message 'Informe valor compensação ADIANTAMENTO, um de cada vez '(294) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      vtotal_inv = 0.
      loop at tg_invoice into wg_invoice where checkbox = 'X' .
        add wg_invoice-vlr_doc to vtotal_inv.
      endloop.

      if vtotal_adt gt vtotal_inv.
        message 'Total compensação ADIANTAMENTO maior que total INVOICE '(295) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      if vtotal_inv eq 0.
        message 'INVOICE sem valores a compensar'(296) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      wg_atualiza_shdb = 'X'.
      clear wl_erro.
      perform f_shdb_adt changing wg_adiant wl_erro.
      if wl_erro eq 'X' .
        message 'Erro na execução do processo (SHDB)'(288) type 'I'.
        set screen 0.
        exit.
      endif.

      " Marca status compensado 'C' em todas a INVOICE PAGAR e RECEBER que tiveram Valores compensados.
      sort: tg_invoice by belnr,
            tg_adiant  by belnr.
      tg_compe_aux[] = tg_compe[].
      sort tg_compe_aux by belnr_cp belnr_cr.
      delete adjacent duplicates from tg_compe_aux comparing belnr_cp belnr_cr.
      loop at tg_compe_aux into wg_compe.
        read table tg_invoice into wg_invoice with key belnr = wg_compe-belnr_cr binary search.
        update zfit0036 set status = 'C'
        where obj_key = wg_invoice-obj_key.

        read table tg_adiant into wg_adiant with key belnr = wg_compe-belnr_cp binary search.
        if sy-subrc = 0.
          wg_adiant-fg_ok = 'X'.
          modify tg_adiant from wg_adiant index sy-tabix  transporting fg_ok.
          update zfit0036 set status = 'C'
          where obj_key = wg_adiant-obj_key.

        endif.

      endloop.

      if wl_erro ne 'Z'. " Sem saldo - não executou o SHDB - se executou gravar na ZFIT0036
        loop at tg_adiant into wg_adiant.
          if wg_adiant-vlr_comp gt 0  .
            clear wa_zfit0036_ins.
            wa_zfit0036_ins-obj_key  = wg_adiant-obj_key .
            wa_zfit0036_ins-bukrs    = p_bukrs .
            wa_zfit0036_ins-belnr    = wg_documento.
            wa_zfit0036_ins-invoice  = wg_adiant-invoice.
            wa_zfit0036_ins-navio    = wg_adiant-navio.
            wa_zfit0036_ins-moeda_pgto    = 'USD'.
            wa_zfit0036_ins-status    = 'P'.
            wa_zfit0036_ins-rg_atualizado = 'S'.

            insert into  zfit0036 values wa_zfit0036_ins.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
          endif.
        endloop.
      endif.
      message 'Processo realizado com sucesso'(290) type 'I'.
      set screen 0.

    when 'BTN_COMP'.
      vtotal_adt = 0.
      clear wl_erro .
      loop at tg_adiant into wg_adiant.
        add wg_adiant-vlr_comp to vtotal_adt .
        if wg_adiant-vlr_sld lt 0.
          vlinhaadt = sy-tabix.
          concatenate 'Saldo negativo ADIANTAMENTO, linha'(292) vlinhaadt into vmsgadt separated by space.
          message vmsgadt type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        exit.
      endif.
      if vtotal_adt eq 0.
        message 'Informe valor compensação ADIANTAMENTO'(293) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      vtotal_inv = 0.
      loop at tg_invoice into wg_invoice where checkbox = 'X' .
        add wg_invoice-vlr_doc to vtotal_inv.
      endloop.

      if vtotal_adt gt vtotal_inv.
        message 'Total compensação ADIANTAMENTO maior que total INVOICE'(295) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      refresh tg_compe.
      sort tg_invoice by vlr_doc.
      loop at tg_invoice into wg_invoice.
        wg_invoice-vlr_comp = 0.
        wg_invoice-vlr_sld  = 0.
        modify tg_invoice from wg_invoice index sy-tabix transporting vlr_comp vlr_sld .
      endloop.
      loop at tg_adiant into wg_adiant.
        if wg_adiant-vlr_comp eq 0.
          continue.
        endif.
        xvlrpg = wg_adiant-vlr_comp.
        loop at tg_invoice into wg_invoice where checkbox = 'X' .
          vtotal_inv = wg_invoice-vlr_doc - wg_invoice-vlr_comp.
          if vtotal_inv gt 0 .
            if xvlrpg gt vtotal_inv.
              subtract vtotal_inv from xvlrpg.
              if wg_invoice-vlr_comp eq 0.
                wg_invoice-vlr_comp = vtotal_inv.
              else.
                add vtotal_inv to wg_invoice-vlr_comp.
              endif.
            else.
              if wg_invoice-vlr_comp eq 0.
                wg_invoice-vlr_comp = xvlrpg.
              else.
                add xvlrpg to wg_invoice-vlr_comp.
              endif.
              xvlrpg = 0.
            endif.
            wg_invoice-vlr_sld  =  wg_invoice-vlr_doc  - wg_invoice-vlr_comp.
            modify tg_invoice from wg_invoice index sy-tabix transporting vlr_comp vlr_sld .
            wg_compe-belnr_cp = wg_adiant-augbl.
            wg_compe-belnr_cr = wg_invoice-belnr.
*---> 08/06/2023 - Migração S4 - JS
*            WG_COMPE-VLR_COMP = WG_INVOICE-VLR_COMP.
            wg_compe-vlr_comp = conv #( wg_invoice-vlr_comp ).
*<--- 08/06/2023 - Migração S4 - JS
            append wg_compe to tg_compe.
            "WG_ATUALIZA = 'X'.
          endif.
        endloop.
        delete tg_compe where vlr_comp = 0.
      endloop.

      call method grid6->refresh_table_display
        exporting
          is_stable = wa_stable.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_7000  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ADT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_adt .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        "1 ' LIFNR'           'LFA1'        'TG_ADIANT' 'LIFNR'           'Fornecedor'          '12' ' ' ' ' ' ',
        "2 ' NAME1'           'LFA1'        'TG_ADIANT' 'NAME1'           'Nome Fornecedor'     '30' ' ' ' ' ' ',
        3 ' '                ' '           'TG_ADIANT' 'BUDAT'           'Dt.Lcto'(005)             '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_ADIANT' 'BELNR'           'Documento'(202)           '12' ' ' ' ' ' ',
        5 ' '                ' '           'TG_ADIANT' 'INVOICE'         'Sol.OV.'(129)             '12' ' ' ' ' ' ',
        6 ' '                ' '           'TG_ADIANT' 'VLR_PGTO'        'Valor Documento'(205)     '15' ' ' ' ' ' ',
        7 'BSID'             'DMBE2'       'TG_ADIANT' 'VLR_COMP'        'Vlr.Compensação'(291)     '15' 'X' ' ' ' ',
        8 ' '                ' '           'TG_ADIANT' 'VLR_SLD'         'Saldo Residual'(209)      '15' ' ' ' ' ' '.

endform.                    " MONTAR_LAYOUT_ADT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_INV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_inv .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 '  '           ' '              'TG_INVOICE' 'CHECKBOX'      'Chk'(213)                '05' 'X' ' ' ' ',
        1 '  '           ' '              'TG_INVOICE' 'BUDAT'         'Dt.Lcto'(005)            '10' ' ' ' ' ' ',
        2 '  '           ' '              'TG_INVOICE' 'BELNR'         'Documento'(202)          '12' ' ' ' ' ' ',
        2 '  '           ' '              'TG_INVOICE' 'NAVIO'         'Navio'(010)              '20' ' ' ' ' ' ',
        3 '  '           ' '              'TG_INVOICE' 'INVOICE'       'Invoice'(003)            '12' ' ' ' ' ' ',
        4 ' '            ' '              'TG_INVOICE' 'VLR_DOC'       'Valor Documento'(205)    '15' ' ' ' ' ' ',
        5 'BSID'         'DMBE2'          'TG_INVOICE' 'VLR_COMP'      'Vlr.Compensação'(291)    '15' ' ' ' ' ' ',
        6 ' '            ' '              'TG_INVOICE' 'VLR_SLD'       'Saldo Residual'(209)     '15' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_INV
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_ADT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_ADT  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_adt  changing p_alv like wg_adiant p_erro.
  data: vdata(10),
        vcampo(15),
        cnum_seq(2),
        vnum_seq    type i,
        vsaldo      type bsid_view-dmbe2,
        wl_vlr(16),
        wl_vlrk(16).


  select single bukrs gjahr awkey belnr blart budat bldat xblnr waers kursf
     from bkpf
     into wa_bkpf
     where bukrs  = p_bukrs
     and   gjahr  = p_alv-budat+6(4)
     and   belnr  = p_alv-augbl.

  write: wa_bkpf-kursf   to wl_vlrk.
  condense wl_vlrk no-gaps.

  " Verifica se o saldo é ADIANTAMENTO
  vtotal_adt = 0.
  clear wa_bkpf.
  loop at tg_adiant into wg_adiant.
    if wg_adiant-vlr_comp gt 0 .
      add wg_adiant-vlr_sld to vtotal_adt.
    endif.
  endloop.

  " Verifica se o saldo é no contas a receber
  vtotal_inv = 0.
  loop at tg_invoice into wg_invoice where checkbox = 'X' .
    if wg_invoice-vlr_comp gt 0.
      add wg_invoice-vlr_sld to vtotal_inv.
    endif.
  endloop.

  if vtotal_adt = 0 .
    p_erro = 'Z'.
    exit.
  endif.

  refresh ti_bdcdata.
  concatenate  wg_cadadt-dt_pgto+6(2) wg_cadadt-dt_pgto+4(2) wg_cadadt-dt_pgto(4) into vdata  separated by '.'.

*  IF vtotal_adt GT 0.
*    vtotal_adt = vtotal_adt * -1.
*  ENDIF.

  vsaldo = vtotal_adt .
  write: vsaldo  to wl_vlr.
  condense wl_vlr no-gaps.

  select single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key eq p_alv-obj_key.

  perform f_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'AB',
    ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadadt-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       'USD',

    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SL',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'AB',
    ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadadt-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       'USD',
    ''          ''      ''   'BKPF-KURSF'       wl_vlrk,
    ''          ''      ''   'BKPF-WWERT'       vdata,

    'SAPMF05A'  '0710'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'RF05A-AGBUK'      p_bukrs,
    ''          ''      ''   'RF05A-AGKON'      p_alv-lifnr,
    ''          ''      ''   'RF05A-AGKOA'      'K',
    ''          ''      ''   'RF05A-AGUMS'      'A',
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SLK'.

  vnum_seq = 0.
  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cp.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cp.
  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cp.

  endloop.

  perform f_bdc_data using:
     'SAPMF05A'  '0710'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'RF05A-AGBUK'      p_bukrs,
      ''          ''      ''   'RF05A-AGKON'      p_alv-lifnr,
      ''          ''      ''   'RF05A-AGKOA'      'K',
      ''          ''      ''   'RF05A-AGUMS'      ' ',
      ''          ''      ''   'RF05A-XNOPS'      'X',
      ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
      ''          ''      ''   'RF05A-XPOS1(03)'  'X',

      'SAPMF05A'  '0731'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PA'.

  vnum_seq = 0.
  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cr.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cr.
  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cr.
  endloop.

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=REST',
         ''          ''      ''   'BDC_SUBSCR'        'SAPDF05X                                6102PAGE',
         ''          ''      ''   'RF05A-ABPOS'      '1'.
  " Posicionar no documento que ficará com saldo

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=PI',
         ''          ''      ''   'BDC_SUBSCR'        'SAPDF05X                                6106PAGE',
         ''          ''      ''   'RF05A-ABPOS'      '1',
         ''          ''      ''   'DF05B-PSDIF(01)'  wl_vlr.

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=BS',

         'SAPMF05A'  '0700'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=BU'.
  "ENDLOOP.


  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.

endform.                    " F_SHDB_ADT
*&---------------------------------------------------------------------*
*&      Module  STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_8000 output.
  set pf-status 'Z001'.
  set titlebar '8000'.
endmodule.                 " STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_8000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_8000 output.
  clear wa_layout.
  wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
  wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
  wa_stable-row        = c_x.
  wa_layout-info_fname = 'COLOR'.

  wa_layout-no_toolbar = c_x.
  wa_layout-stylefname = 'STYLE2'.
  wa_layout-grid_title = ' '.
  "GRID3
  if obg_conteiner_adtbra is initial.
    create object obg_conteiner_adtbra
      exporting
        container_name = g_cc_adiantbra.


    create object grid7
      exporting
        i_parent = obg_conteiner_adtbra.


    perform montar_layout_adtbra.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = c_x.


    call method grid7->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_adiantbra[].

    call method grid7->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>on_data_changed_finished_bra for grid7,
              lcl_event_handler=>on_data_changed_adtbra for grid7.

  else.
    call method grid7->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = t_fieldcatalog[].
    loop at t_fieldcatalog into w_fieldcatalog
     where fieldname eq 'VLR_PGTO'
        or fieldname eq 'VLR_COMP'
        or fieldname eq 'VLR_SLD'.
      w_fieldcatalog-outputlen = '15'.
      modify t_fieldcatalog from w_fieldcatalog.
    endloop.
    call method grid7->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid7->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  "GRID2
  if obg_conteiner_notas is initial.
    create object obg_conteiner_notas
      exporting
        container_name = g_cc_notas.


    create object grid8
      exporting
        i_parent = obg_conteiner_notas.


    perform montar_layout_notas.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = c_x.


    call method grid8->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_notas[].

    call method grid8->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  else.
    call method grid8->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = t_fieldcatalog[].
    loop at t_fieldcatalog into w_fieldcatalog
     where fieldname eq 'VLR_DOC'
        or fieldname eq 'VLR_COMP'
        or fieldname eq 'VLR_SLD'.
      w_fieldcatalog-outputlen = '15'.
      modify t_fieldcatalog from w_fieldcatalog.
    endloop.
    call method grid8->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid8->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS_8000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ADTBRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_adtbra .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        3 ' '                ' '           'TG_ADIANTBRA' 'AUGDT'           'Dt.Lcto'(005)             '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_ADIANTBRA' 'AUGBL'           'Doc.Cont.'(297)           '12' ' ' ' ' ' ',
        5 ' '                ' '           'TG_ADIANTBRA' 'WAERS'           'Moeda'(131)               '12' ' ' ' ' ' ',
        6 ' '                ' '           'TG_ADIANTBRA' 'VLR_DOC'         'Valor Documento'(205)     '15' ' ' 'X' ' ',
        7 'BSID'             'DMBE2'       'TG_ADIANTBRA' 'VLR_COMP'        'Vlr.Compensação'(291)     '15' 'X' 'X' ' ',
        8 ' '                ' '           'TG_ADIANTBRA' 'VLR_SLD'         'Saldo Residual'(209)      '15' ' ' 'X' ' '.
endform.                    " MONTAR_LAYOUT_ADTBRA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_notas .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        3 ' '                ' '           'TG_NOTAS' 'AUGDT'           'Dt.Lcto'(005)             '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_NOTAS' 'AUGBL'           'Doc.Cont.'(297)           '12' ' ' ' ' ' ',
        4 ' '                ' '           'TG_NOTAS' 'VBELN'           'Doc.Fatura'(141)          '12' ' ' ' ' ' ',
        5 ' '                ' '           'TG_NOTAS' 'WAERS'           'Moeda'(131)               '12' ' ' ' ' ' ',
        6 ' '                ' '           'TG_NOTAS' 'VLR_DOC'         'Valor Documento'(205)     '15' ' ' 'X' ' ',
        7 'BSID'             'DMBE2'       'TG_NOTAS' 'VLR_COMP'        'Vlr.Compensação'(291)     '15' ' ' 'X' ' ',
        8 ' '                ' '           'TG_NOTAS' 'VLR_SLD'         'Saldo Residual'(209)      '15' ' ' 'X' ' '.
endform.                    " MONTAR_LAYOUT_NOTAS
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_8000 input.
  case ok-code.
    when 'GERAR'.
      clear wl_erro .
      if wg_cadbrasil-augdt is initial.
        message 'Informe a data de pagamento'(263) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      vtotal_adt = 0.
      loop at tg_adiantbra into wg_adiantbra.
        add wg_adiantbra-vlr_comp to vtotal_adt .
        if wg_adiantbra-vlr_sld lt 0.
          vlinhaadt = sy-tabix.
          concatenate 'Saldo negativo ADIANTAMENTO, linha'(292) vlinhaadt into vmsgadt separated by space.
          message vmsgadt type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        exit.
      endif.
      if vtotal_adt eq 0.
        message 'Informe valor compensação ADIANTAMENTO'(293) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      vtotal_inv = 0.
      loop at tg_notas into wg_notas. " WHERE checkbox = 'X' .
        add wg_notas-vlr_doc to vtotal_inv.
      endloop.

      if vtotal_adt gt vtotal_inv.
        message 'Total compensação ADIANTAMENTO maior que total NOTAS'(298) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.

      wg_atualiza_shdb = 'X'.
      clear wl_erro.
      perform f_shdb_adtbra changing wg_adiantbra wl_erro.
      if wl_erro eq 'X' .
        message 'Erro na execução do processo (SHDB)'(288) type 'I'.
        set screen 0.
        exit.
      endif.
      " Marca status compensado 'C' em todas a INVOICE PAGAR e RECEBER que tiveram Valores compensados.
      sort:  tg_adiantbra  by augbl.
      tg_compe_aux[] = tg_compe[].
      sort tg_compe_aux by belnr_cp belnr_cr.
      delete adjacent duplicates from tg_compe_aux comparing belnr_cp.
      loop at tg_compe_aux into wg_compe.

        read table tg_adiantbra into wg_adiantbra with key augbl = wg_compe-belnr_cp binary search.
        if sy-subrc = 0.
          wg_adiant-fg_ok = 'X'.
          modify tg_adiantbra from wg_adiantbra index sy-tabix  transporting fg_ok.
          update zfit0036 set status = 'C'
          where obj_key = wg_adiantbra-obj_key.

        endif.

      endloop.

      if wl_erro ne 'Z'. " Sem saldo - não executou o SHDB - se executou gravar na ZFIT0036
        loop at tg_adiant into wg_adiant.
          if wg_adiant-vlr_comp gt 0  .
            clear wa_zfit0036_ins.
            wa_zfit0036_ins-obj_key  = wg_adiant-obj_key .
            wa_zfit0036_ins-bukrs    = p_bukrs .
            wa_zfit0036_ins-belnr    = wg_documento.
            wa_zfit0036_ins-invoice  = wg_adiant-invoice.
            wa_zfit0036_ins-navio    = wg_adiant-navio.
            wa_zfit0036_ins-moeda_pgto    = 'USD'.
            wa_zfit0036_ins-status    = 'P'.
            wa_zfit0036_ins-rg_atualizado = 'S'.

            insert into  zfit0036 values wa_zfit0036_ins.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
          endif.
        endloop.
      endif.
      message 'Processo realizado com sucesso'(290) type 'I'.
      set screen 0.
    when 'BTNV'.
      vtotal_adt = 0.
      clear wl_erro .
      loop at tg_adiantbra into wg_adiantbra.
        add wg_adiantbra-vlr_comp to vtotal_adt .
        if wg_adiantbra-vlr_sld lt 0.
          vlinhaadt = sy-tabix.
          concatenate 'Saldo negativo ADIANTAMENTO, linha'(292) vlinhaadt into vmsgadt separated by space.
          message vmsgadt type 'I'.
          wl_erro = 'X'.
          exit.
        endif.
      endloop.
      if wl_erro = 'X'.
        exit.
      endif.
      if vtotal_adt eq 0.
        message 'Informe valor compensação ADIANTAMENTO'(293) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      vtotal_inv = 0.
      loop at tg_notas into wg_notas. " WHERE checkbox = 'X' .
        add wg_notas-vlr_doc to vtotal_inv.
      endloop.

      if vtotal_adt gt vtotal_inv.
        message 'Total compensação ADIANTAMENTO maior que total NOTAS'(298) type 'I'.
        wl_erro = 'X'.
        exit.
      endif.
      refresh tg_compe.
      sort tg_notas by vlr_doc.
      loop at tg_notas into wg_notas.
        wg_invoice-vlr_comp = 0.
        wg_invoice-vlr_sld  = 0.
        modify tg_notas from wg_notas index sy-tabix transporting vlr_comp vlr_sld .
      endloop.
      loop at tg_adiantbra into wg_adiantbra.
        if wg_adiantbra-vlr_comp eq 0.
          continue.
        endif.
        xvlrpg = wg_adiantbra-vlr_comp.
        loop at tg_notas into wg_notas. " WHERE checkbox = 'X' .
          vtotal_inv = wg_notas-vlr_doc - wg_notas-vlr_comp.
          if vtotal_inv gt 0 .
            if xvlrpg gt vtotal_inv.
              subtract vtotal_inv from xvlrpg.
              if wg_notas-vlr_comp eq 0.
                wg_notas-vlr_comp = vtotal_inv.
              else.
                add vtotal_inv to wg_notas-vlr_comp.
              endif.
            else.
              if wg_notas-vlr_comp eq 0.
                wg_notas-vlr_comp = xvlrpg.
              else.
                add xvlrpg to wg_notas-vlr_comp.
              endif.
              xvlrpg = 0.
            endif.
            wg_notas-vlr_sld  =  wg_notas-vlr_doc  - wg_notas-vlr_comp.
            modify tg_notas from wg_notas index sy-tabix transporting vlr_comp vlr_sld .
            wg_compe-belnr_cp = wg_adiantbra-augbl.
            wg_compe-belnr_cr = wg_notas-augbl.
*---> 08/06/2023 - Migração S4 - JS
*            WG_COMPE-VLR_COMP = WG_NOTAS-VLR_COMP.
            wg_compe-vlr_comp = conv #( wg_notas-vlr_comp ).
*<--- 08/06/2023 - Migração S4 - JS
            append wg_compe to tg_compe.
            "WG_ATUALIZA = 'X'.
          endif.
        endloop.
        delete tg_compe where vlr_comp = 0.
      endloop.

      call method grid8->refresh_table_display
        exporting
          is_stable = wa_stable.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_ADTBRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_ADIANTBRA  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_adtbra  changing p_alv like wg_adiantbra p_erro.
  data: vdata(10),
        vcampo(15),
        cnum_seq(2),
        vnum_seq    type i,
        vsaldo      type bsid_view-dmbe2,
        wl_vlr(16),
        wl_vlrk(16).


  select single bukrs gjahr awkey belnr blart budat bldat xblnr waers kursf
     from bkpf
     into wa_bkpf
     where bukrs  = p_bukrs
     and   gjahr  = p_alv-augbl+0(4)
     and   belnr  = p_alv-augbl.

  write: wa_bkpf-kursf   to wl_vlrk.
  condense wl_vlrk no-gaps.

  " Verifica se o saldo é ADIANTAMENTO
  vtotal_adt = 0.
  clear wa_bkpf.
  loop at tg_adiantbra into wg_adiantbra.
    if wg_adiantbra-vlr_comp gt 0 .
      add wg_adiantbra-vlr_sld to vtotal_adt.
    endif.
  endloop.

  " Verifica se o saldo é no contas a receber
  vtotal_inv = 0.
  loop at tg_notas into wg_notas. " WHERE CHECKBOX = 'X' .
    if wg_notas-vlr_comp gt 0.
      add wg_notas-vlr_sld to vtotal_inv.
    endif.
  endloop.

  if vtotal_adt = 0 .
    p_erro = 'Z'.
    exit.
  endif.

  refresh ti_bdcdata.
  concatenate  wg_cadbrasil-augdt+6(2) wg_cadbrasil-augdt+4(2) wg_cadbrasil-augdt(4) into vdata  separated by '.'.


  vsaldo = vtotal_adt .
  write: vsaldo  to wl_vlr.
  condense wl_vlr no-gaps.

*  SELECT SINGLE *
*    FROM ZIB_CONTABIL
*    INTO WA_ZIB_CONTABIL
*    WHERE OBJ_KEY EQ P_ALV-OBJ_KEY.

  perform f_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SL',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       p_alv-blart,
    ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadbrasil-augdt+4(2),
    ''          ''      ''   'BKPF-WAERS'       p_alv-waers,
    'SAPMF05A'  '0710'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'RF05A-AGBUK'      p_bukrs,
    ''          ''      ''   'RF05A-AGKON'      wg_cadbrasil-kunnr,
    ''          ''      ''   'RF05A-AGKOA'      'D',
    ''          ''      ''   'RF05A-AGUMS'      p_alv-umskz,
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA',
    ''          ''      ''   'RF05A-SEL01(01)'  p_alv-augbl.
  vnum_seq = 1.
  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cr.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cr.
  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cr.
  endloop.


  perform f_bdc_data using:
      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=REST',
      ''          ''      ''   'BDC_SUBSCR'       'SAPDF05X                                5102PAGE',
      ''          ''      ''   'BDC_CURSOR'       'RF05A-PSCOM(01)',
      ''          ''      ''   'RF05A-ABPOS'      '1',

      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PI',
      ''          ''      ''   'BDC_SUBSCR'       'SAPDF05X                                5106PAGE',
      ''          ''      ''   'BDC_CURSOR'       'DF05B-PSBET(01)',
      ''          ''      ''   'RF05A-ABPOS'      '1',

      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PI',
      ''          ''      ''   'BDC_SUBSCR'       'SAPDF05X                                5106PAGE',
      ''          ''      ''   'BDC_CURSOR'       'DF05B-PSBET(02)',
      ''          ''      ''   'RF05A-ABPOS'      '1',

      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PI',
      ''          ''      ''   'BDC_SUBSCR'       'SAPDF05X                                5106PAGE',
      ''          ''      ''   'BDC_CURSOR'       'DF05B-PSDIF(01)',
      ''          ''      ''   'RF05A-ABPOS'      '1',

      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BU',
      ''          ''      ''   'BDC_SUBSCR'       'SAPDF05X                                5106PAGE',
      ''          ''      ''   'BDC_CURSOR'       'DF05B-PSDIF(01)',
      ''          ''      ''   'RF05A-ABPOS'      '1'.

  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.

endform.                    " F_SHDB_ADTBRA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9000 output.

  refresh: fcode.
  append c_save to fcode.
  append c_exit to fcode.
  append c_back to fcode.
  append c_cancel to fcode.
  set pf-status 'Z003' excluding fcode.
  set titlebar '9000'.

  if wg_cadger-tp_operacao is not initial.
    clear: wg_cadger-question, wg_cadger-ds_operacao.
    select single *
     from zfit0043
     into   wa_zfit0043
     where tp_operacao = wg_cadger-tp_operacao
      and spras = sy-langu.
    if sy-subrc = 0.
      wg_cadger-ds_operacao =  wa_zfit0043-ds_operacao.
      if wa_zfit0043-status_ctb = 'S'.
        wg_cadger-question = 'X'.
      else.
        wg_cadger-question = ' '.
      endif.
      wg_cadger-liquidar = wa_zfit0043-liquidar.
    endif.
    select single *
     from zfit0040
     into   wa_zfit0040
     where motivo = wg_cadger-motivo.
    if sy-subrc = 0.
      wg_cadger-descricao =  wa_zfit0040-descricao.
    endif.
  endif.

  loop at screen.
    if wg_cadger-obj_key is not  initial.
      if screen-name eq 'WG_CADGER-TP_OPERACAO' or
         screen-name eq 'WG_CADGER-BUKRS'    or
         screen-name eq 'WG_CADGER-DT_LCT'   or
         screen-name eq 'WG_CADGER-WAERS'    or
         screen-name eq 'WG_CADGER-OBSERVACAO'.
        screen-input     = 0.
      endif.
    else.
      if screen-name eq 'WG_CADGER-TP_OPERACAO' or
         screen-name eq 'WG_CADGER-BUKRS'    or
         screen-name eq 'WG_CADGER-DT_LCT'   or
         screen-name eq 'WG_CADGER-WAERS'    or
         screen-name eq 'WG_CADGER-OBSERVACAO'.
        screen-input     = 1.
      endif.
    endif.
    modify screen.
  endloop.

endmodule.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_9000 output.
  if wg_cadger-ds_operacao is initial.
    exit.
  endif.
  clear wa_layout.
  "wa_layout-no_f4       = c_x.
  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
  wa_layout-col_opt    = c_x.
  wa_stable-row        = c_x.
  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt   = 'X'.
  "WA_LAYOUT-BOX_FNAME    = 'MARK'.
  wa_layout-no_toolbar = space.
  "GRID8
  if obg_conteiner_cria is initial.
    create object obg_conteiner_cria
      exporting
        container_name = g_cc_criaadiant.


    create object grid9
      exporting
        i_parent = obg_conteiner_cria.


    perform montar_layout_cria.

    create object obg_toolbar
      exporting
        io_alv_grid = grid9.

*      * Register event handler
    set handler obg_toolbar->on_toolbar for grid9.
    set handler obg_toolbar->handle_user_command for grid9.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.


    wa_layout-no_toolbar = space.

    wa_layout-grid_title = 'Adiantamentos'(299).

    wa_layout-stylefname = 'CELLSTYLE'.

    call method grid9->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter        "I_CONSISTENCY_CHECK = 'X'
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_criaadt[].

*
    call method grid9->set_ready_for_input
      exporting
        i_ready_for_input = 1.            " 1 for edit, 0 for display

    call method grid9->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid9->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    perform build_f4_cat.

    set handler:
              lcl_event_handler=>catch_hotspot_cria for grid9,
              lcl_event_handler=>on_data_changed_cria for grid9,
              lcl_event_handler=>on_f4 for grid9,
              lcl_event_handler=>on_data_changed_finished_cria for grid9.


  else.
    perform montar_layout_cria.
    call method grid9->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid9->refresh_table_display
      exporting
        is_stable = wa_stable.

*    CALL METHOD grid9->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.            " 1 for edit, 0 for display


  endif.

endmodule.                 " CRIA_OBJETOS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  clear wl_erro.
  data: wl_linha  type sy-tabix,
        vseqitem  type zib_contabil-seqitem,
        vobj_key  type zib_contabil-obj_key,
        wv_wrbtr  type bseg-wrbtr,
        wv_bvtyp  type lfbk-bvtyp,
        vdata(10).
  case ok-code.
    when '&NOVO'.
      clear wg_cadger.
      refresh tg_criaadt.
      refresh tg_conta.
      if obg_conteiner_cria is not initial.
        call method grid9->refresh_table_display
          exporting
            is_stable = wa_stable.

        call method grid9->set_ready_for_input
          exporting
            i_ready_for_input = 1.
      endif.
    when '&ATUADT'.
      check tg_criaadt[] is not initial.
      if wg_cadger-question = 'X'.
        if  wg_cadger-obj_key is not initial and wg_cadger-documento is initial.
          select single *
          from zfit0043
          into wa_zfit0043
          where tp_operacao = wg_cadger-tp_operacao
            and spras = sy-langu.

          select single obj_key belnr bukrs
           from zib_contabil_chv
           into wa_zib_contabil_chv
           where obj_key =  wg_cadger-obj_key.

          if sy-subrc = 0.
            wg_cadger-icon      =  icon_okay.
            wg_cadger-documento = wa_zib_contabil_chv-belnr.
            wg_documento = wa_zib_contabil_chv-belnr.

            wv_wrbtr = 0.
            clear: vnum,wv_bvtyp,vlifnr_9000.
            loop at tg_criaadt into wg_criaadt.
              if wg_criaadt-dc = 'C'.
                add wg_criaadt-wrbtr to wv_wrbtr.
              endif.
              if wg_criaadt-bvtyp is not initial.
                wv_bvtyp = wg_criaadt-bvtyp.
              endif.
              if wg_criaadt-lifnr is not initial.
                vlifnr_9000 = wg_criaadt-lifnr.
              endif.
            endloop.
            " Gera numero do lote
            call function 'NUMBER_GET_NEXT'
              exporting
                nr_range_nr = '01'
                object      = 'ZID_INV'
              importing
                number      = vseq.
            vnum = vseq .

            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = vnum
              importing
                output = vnum.
            clear wa_zfit0036_ins.
            concatenate 'A' wg_documento into wa_zfit0036_ins-obj_key    .
            wa_zfit0036_ins-bukrs          = p_bukrs.
            wa_zfit0036_ins-lote           = vnum.
            wa_zfit0036_ins-invoice        = wa_zfit0043-ds_operacao.
            wa_zfit0036_ins-dt_pgto        = wg_cadger-dt_lct.
            wa_zfit0036_ins-moeda_pgto     = wg_cadger-waers.
*---> 08/06/2023 - Migração S4 - JS
*            wa_zfit0036_ins-vlr_pgto       = wv_wrbtr.
            wa_zfit0036_ins-vlr_pgto = conv #( wv_wrbtr ).
*<--- 08/06/2023 - Migração S4 - JS

            wa_zfit0036_ins-bvtyp          = wv_bvtyp.
            wa_zfit0036_ins-hbkid          = ''.

            if wg_cadger-liquidar = 'S'.
              wa_zfit0036_ins-status         = 'L'. "Liberadas automaticamente pedido/ZGL
            else.
              wa_zfit0036_ins-status         = 'A'.
            endif.
            wa_zfit0036_ins-forma_pg       = 'P'.
            wa_zfit0036_ins-motivo         = wg_cadger-motivo.
            wa_zfit0036_ins-referencia     = wg_cadger-referencia.
            wa_zfit0036_ins-observacao     = wg_cadger-observacao.
            wa_zfit0036_ins-operacao       = wg_cadger-tp_operacao.
            wa_zfit0036_ins-navio          = wg_cadger-navio. " BUG 53981
            wa_zfit0036_ins-lifnr          = vlifnr_9000.
            wa_zfit0036_ins-usuario        = sy-uname.
            wa_zfit0036_ins-data_atual     = sy-datum.
            wa_zfit0036_ins-hora_atual     = sy-uzeit.
            wa_zfit0036_ins-user_create    = sy-uname. " BUG - 75841 - CBRAND
            " GRAVA DADOS BANCÁRIOS.
            loop at tg_conta into wg_conta.
              if sy-tabix = 1.
                wa_zfit0036_ins-bvtyp   = wg_conta-bvtyp.
                wa_zfit0036_ins-swift_1 = wg_conta-swift.
                wa_zfit0036_ins-banks_1 = wg_conta-banks.
                wa_zfit0036_ins-bankl_1 = wg_conta-bankl.
                wa_zfit0036_ins-banka_1 = wg_conta-banka.
                wa_zfit0036_ins-bankn_1 = wg_conta-bankn.
                wa_zfit0036_ins-iban_1  = wg_conta-iban.
              else.
                wa_zfit0036_ins-bvtyp_2   = wg_conta-bvtyp.
                wa_zfit0036_ins-swift_2 = wg_conta-swift.
                wa_zfit0036_ins-banks_2 = wg_conta-banks.
                wa_zfit0036_ins-bankl_2 = wg_conta-bankl.
                wa_zfit0036_ins-banka_2 = wg_conta-banka.
                wa_zfit0036_ins-bankn_2 = ''. "WG_CONTA-BANKN.
                wa_zfit0036_ins-iban_2  = wg_conta-iban.
              endif.
            endloop.
            insert into  zfit0036 values wa_zfit0036_ins.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
            clear wa_zfit0036_ins.

          else.
            select single *
             from zib_contabil_err
             into wa_zib_contabil_err
             where obj_key =  wg_cadger-obj_key..
            if sy-subrc = 0.
              wg_cadger-icon      =  icon_incomplete.
              wg_cadger-documento = '999999999'.
            endif.
          endif.

          call method grid9->refresh_table_display
            exporting
              is_stable = wa_stable.
        endif.
      else.
        message 'Não será gerado lançamento contábil para esta operação'(300) type 'I'.
      endif.

    when 'GERAR'.
      concatenate  wg_cadger-dt_lct+6(2) wg_cadger-dt_lct+4(2) wg_cadger-dt_lct(4) into vdata separated by '.'.
*
      perform verifica_erros_cria.
      if wl_erro is initial.
        if wg_cadger-question = ' '.
          wg_cadger-icon      =  icon_okay.
          wg_cadger-documento = 0.
          wv_wrbtr = 0.
          clear vnum.
          clear: wv_bvtyp, vlifnr_9000.
          loop at tg_criaadt into wg_criaadt.
            add wg_criaadt-wrbtr to wv_wrbtr.
            if wg_criaadt-bvtyp is not initial.
              wv_bvtyp = wg_criaadt-bvtyp.
            endif.
            if wg_criaadt-lifnr is not initial.
              vlifnr_9000 = wg_criaadt-lifnr.
            endif.
            vnum = wg_criaadt-saknrz.
          endloop.
          "loop aqui 29.09.2016

          " Gera numero do lote
          call function 'NUMBER_GET_NEXT'
            exporting
              nr_range_nr = '01'
              object      = 'ZID_INV'
            importing
              number      = vseq.
          vlote_ad = vseq .

          loop at tg_criaadt into wg_criaadt.
            if wg_cadger-liquidar eq 'S'.
              vlifnr_9000 = wg_criaadt-lifnr.
              vnum        = wg_criaadt-saknrz.
              wv_bvtyp    = wg_criaadt-bvtyp.
              wv_wrbtr    = wg_criaadt-wrbtr.
            endif.
            "
            call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
              exporting
                input  = vnum
              importing
                output = vnum.
            condense vnum no-gaps.
            vfor = vnum.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = vfor
              importing
                output = vfor.
            "
            " Sequencia "AC"
            call function 'NUMBER_GET_NEXT'
              exporting
                nr_range_nr = '01'
                object      = 'ZID_FI17'
              importing
                number      = vseq.
            vnum2 = vseq .
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = vnum2
              importing
                output = vnum2.
            "

            select single *
            from zfit0043
            into wa_zfit0043
            where tp_operacao = wg_cadger-tp_operacao
               and spras = sy-langu.

            clear wa_zfit0036_ins.
            concatenate 'AC' vnum2 vfor into wa_zfit0036_ins-obj_key.
            wg_cadger-obj_key              = wa_zfit0036_ins-obj_key.
            wa_zfit0036_ins-bukrs          = p_bukrs.
            wa_zfit0036_ins-lote           = vlote_ad.
            wa_zfit0036_ins-invoice        = wa_zfit0043-ds_operacao.
            wa_zfit0036_ins-dt_pgto        = wg_cadger-dt_lct.
            wa_zfit0036_ins-moeda_pgto     = wg_cadger-waers.
*---> 08/06/2023 - Migração S4 - JS
*            WA_ZFIT0036_INS-VLR_PGTO       = WV_WRBTR.
            wa_zfit0036_ins-vlr_pgto = conv #( wv_wrbtr ).
*<--- 08/06/2023 - Migração S4 - JS
            wa_zfit0036_ins-bvtyp          = wv_bvtyp.
            wa_zfit0036_ins-hbkid          = ''.
            if wg_cadger-liquidar = 'S'.
              wa_zfit0036_ins-status         = 'L'. "Liberadas automaticamente pedido/ZGL
            else.
              wa_zfit0036_ins-status         = 'A'.
            endif.
            wa_zfit0036_ins-forma_pg       = 'P'.
            wa_zfit0036_ins-motivo         = wg_cadger-motivo.
            wa_zfit0036_ins-referencia     = wg_cadger-referencia.
            wa_zfit0036_ins-navio          = wg_cadger-navio. "BUG 53981
            wa_zfit0036_ins-observacao     = wg_cadger-observacao.
            wa_zfit0036_ins-operacao       = wg_cadger-tp_operacao.
            wa_zfit0036_ins-lifnr          = vlifnr_9000.
            wa_zfit0036_ins-usuario        = sy-uname.
            wa_zfit0036_ins-data_atual     = sy-datum.
            wa_zfit0036_ins-hora_atual     = sy-uzeit.
            wa_zfit0036_ins-user_create    = sy-uname. "BUG 75841 - CBRAND
            " GRAVA DADOS BANCÁRIOS.
            loop at tg_conta into wg_conta.
              if sy-tabix = 1.
                wa_zfit0036_ins-bvtyp   = wg_conta-bvtyp.
                wa_zfit0036_ins-swift_1 = wg_conta-swift.
                wa_zfit0036_ins-banks_1 = wg_conta-banks.
                wa_zfit0036_ins-bankl_1 = wg_conta-bankl.
                wa_zfit0036_ins-banka_1 = wg_conta-banka.
                wa_zfit0036_ins-bankn_1 = wg_conta-bankn.
                wa_zfit0036_ins-iban_1  = wg_conta-iban.
              else.
                wa_zfit0036_ins-bvtyp_2   = wg_conta-bvtyp.
                wa_zfit0036_ins-swift_2 = wg_conta-swift.
                wa_zfit0036_ins-banks_2 = wg_conta-banks.
                wa_zfit0036_ins-bankl_2 = wg_conta-bankl.
                wa_zfit0036_ins-banka_2 = wg_conta-banka.
                wa_zfit0036_ins-bankn_2 = ''. "WG_CONTA-BANKN.
                wa_zfit0036_ins-iban_2  = wg_conta-iban.
              endif.
            endloop.
            "SHDB 1-1
            if wg_criaadt-belnr is not initial.
              wa_zfit0036_ins-belnr_adt_c = wg_criaadt-belnr.
              clear: wg_documento, wl_erro.
              if wg_cadger-tp_operacao ne '08'.
                perform f_shdb_geradt   changing wg_criaadt wl_erro.
              endif.
              if wl_erro = 'X'.
                message 'Erro ao gerar compensação'(301) type 'I'.
              else.
                wa_zfit0036_ins-belnr_adt_g = wg_documento.

                message s836(sd) with 'Documento'(202)
                          wg_documento
                          ', criado com sucesso!'(302).
              endif.
            endif.
            insert into  zfit0036 values wa_zfit0036_ins.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
            clear wa_zfit0036_ins.
            if wg_cadger-liquidar ne 'S'.
              exit. "Sommente docd liquidação fazem mais de uma vez
            endif.
          endloop.
        else.
          call function 'NUMBER_GET_NEXT'
            exporting
              nr_range_nr = '01'
              object      = 'ZID_FI17'
            importing
              number      = vseq.
          vnum2 = vseq .
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = vnum2
            importing
              output = vnum2.
          vseqitem = 0.
*** BUG - 55284 - Inicio - CSB
          if p_bukrs = '0200' and wg_cadger-waers = 'GBP'.
            perform f_cria_adt_gbp_0200.
          else.
            loop at tg_criaadt into wg_criaadt.
              add 1 to vseqitem.
              concatenate 'ZFI17' vnum2  wg_cadger-dt_lct+0(4) into wa_zib_contabil-obj_key.
              wg_cadger-icon      =  icon_message_warning .
              wg_cadger-obj_key   =  wa_zib_contabil-obj_key.
              wa_zib_contabil-seqitem   = vseqitem.
              wa_zib_contabil-bschl     = wg_criaadt-bschl.
              "CONCATENATE P_BUKRS+2(2) '01' INTO WA_ZIB_CONTABIL-GSBER.
              wa_zib_contabil-gsber     = wg_criaadt-gsber.
              wa_zib_contabil-bukrs     = p_bukrs.
              wa_zib_contabil-interface	=	''.
              wa_zib_contabil-bldat     = vdata.
              wa_zib_contabil-budat     = vdata.
              wa_zib_contabil-gjahr	    =	 wg_cadger-dt_lct+0(4).
              wa_zib_contabil-monat     = wg_cadger-dt_lct+4(2).
              wa_zib_contabil-blart     = 'SA'.
              wa_zib_contabil-xblnr     = wa_zfit0043-ds_operacao.
              wa_zib_contabil-hkont     = wg_criaadt-saknrz.
              wa_zib_contabil-kostl     = wg_criaadt-kostl.
              wa_zib_contabil-wrbtr     = wg_criaadt-wrbtr.
              wa_zib_contabil-waers     = wg_cadger-waers.
              wa_zib_contabil-sgtxt     = wg_cadger-observacao.
              wa_zib_contabil-umskz     = wg_criaadt-umskz.
              wa_zib_contabil-rg_atualizado	=	'N'.

              insert into  zib_contabil values wa_zib_contabil.
              if sy-subrc ne 0.
                rollback work.
                clear vobj_key.
              else.
                commit work.
              endif.
              clear  wa_zib_contabil.
            endloop.
          endif.
        endif.

        call method grid9->set_ready_for_input
          exporting
            i_ready_for_input = 0.            " 1 for edit, 0 for display
      endif.

    when 'SAIR'.
      if tg_criaadt[] is initial or wg_cadger-icon = icon_okay.
        set screen 0.
      else.
        if wg_cadger-documento is not initial or  wg_cadger-obj_key is initial.
          set screen 0.
        else.
          message 'Documento não foi atualizado, pressione "Atualizar Documento"'(303) type 'I'.
        endif.
      endif.

    when 'SAVE_MOD'.
      perform gravar_dados_modelo.
    when 'USE_MOD'.
      perform buscar_dados_modelo.
    when 'DEL_MOD'.
      perform deletar_modelo.
  endcase.
endmodule.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CRIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_cria .
  refresh t_fieldcatalog.

  if  wg_cadger-question = 'X'.
    perform montar_estrutura using:
*** BUG 55284 - CSB ( Mudei a ordem do fornecedor. )
          1 ' '            ' '              'TG_CRIAADT' 'BSCHL'      'Chv.Lcto'(304)                '08' 'X' ' ' ' ',
          2 ' '            ' '              'TG_CRIAADT' 'DC'         'D/C'(305)                     '05' ' ' ' ' ' ',
          3 'LFA1'         'LIFNR'          'TG_CRIAADT' 'LIFNR'      'Fornecedor'(006)              '12' 'X' ' ' ' ',
          4 ' '            ' '              'TG_CRIAADT' 'SAKNR'      'Conta'(306)                   '12' 'X' ' ' ' ',
          5 ' '            ' '              'TG_CRIAADT' 'TXT50'      'Descrição'(307)               '30' ' ' ' ' ' ',
          6 ' '            ' '              'TG_CRIAADT' 'UMSKZ'      'Razão Especial'(308)          '05' 'X' ' ' ' ',
          7 'BSEG'         'WRBTR'          'TG_CRIAADT' 'WRBTR'      'Valor'(309)                   '15' 'X' ' ' ' ',
          8 ' '            ' '              'TG_CRIAADT' 'KOSTL'      'Centro Custo'(310)            '15' 'X ' ' ' ' ',
          9 'BSEG'         'GSBER'          'TG_CRIAADT' 'GSBER'      'Divisão'(128)                 '15' 'X ' ' ' ' '.

*          1 ' '            ' '              'TG_CRIAADT' 'BSCHL'      'Chv.Lcto'(304)                '08' 'X' ' ' ' ',
*          2 ' '            ' '              'TG_CRIAADT' 'DC'         'D/C'(305)                     '05' ' ' ' ' ' ',
*          2 ' '            ' '              'TG_CRIAADT' 'SAKNR'      'Conta'(306)                   '12' 'X' ' ' ' ',
*          3 ' '            ' '              'TG_CRIAADT' 'TXT50'      'Descrição'(307)               '30' ' ' ' ' ' ',
*          4 ' '            ' '              'TG_CRIAADT' 'UMSKZ'      'Razão Especial'(308)          '05' 'X' ' ' ' ',
*          5 'BSEG'         'WRBTR'          'TG_CRIAADT' 'WRBTR'      'Valor'(309)                   '15' 'X' ' ' ' ',
*          6 'LFA1'         'LIFNR'          'TG_CRIAADT' 'LIFNR'      'Fornecedor'(006)              '12' 'X' ' ' ' ',
*          7 ' '            ' '              'TG_CRIAADT' 'KOSTL'      'Centro Custo'(310)            '15' 'X ' ' ' ' ',
*          8 'BSEG'         'GSBER'          'TG_CRIAADT' 'GSBER'      'Divisão'(128)                 '15' 'X ' ' ' ' '.
    if wg_cadger-liquidar eq 'S'.
      perform montar_estrutura using:
          9 ' '            ' '              'TG_CRIAADT' 'SAKNR_BA'   'Conta Banco'(311)             '12' 'X' ' ' ' ',
         10 'BSIK'         'BELNR'          'TG_CRIAADT' 'BELNR'      'Doc.Liquidação'(312)          '12' 'X' ' ' ' '.
    endif.
  else.
    perform montar_estrutura using:
*** BUG 55284 - Forncedor antes da conta - CSB
          1 'LFA1'         'LIFNR'          'TG_CRIAADT' 'LIFNR'      'Fornecedor'(006)              '12' 'X' ' ' ' ',
          2 ' '            ' '              'TG_CRIAADT' 'SAKNR'      'Conta'(306)                   '12' 'X' ' ' ' ',
          3 ' '            ' '              'TG_CRIAADT' 'TXT50'      'Descrição'(307)               '30' ' ' ' ' ' ',
          4 'BSEG'         'WRBTR'          'TG_CRIAADT' 'WRBTR'      'Valor'(309)                   '15' 'X' ' ' ' ',
          5 ' '            ' '              'TG_CRIAADT' 'ICON'       'C.Bank'(313)                  '05' ' ' ' ' ' '.
    "6 ' '            ' '              'TG_CRIAADT' 'BVTYP'      'Seq.Conta'               '05' ' ' ' ' ' '.
    if wg_cadger-liquidar eq 'S'.
      perform montar_estrutura using:
         7 ' '            ' '              'TG_CRIAADT' 'SAKNR_BA'   'Conta Banco'(311)             '12' 'X' ' ' ' ',
         8 'BSIK'         'BELNR'          'TG_CRIAADT' 'BELNR'      'Doc.Liquidação'(312)          '12' 'X' ' ' ' '.
    endif.

  endif.




endform.                    " MONTAR_LAYOUT_CRIA
*&---------------------------------------------------------------------*
*&      Form  BUILD_F4_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_f4_cat .
  refresh gt_f4.
  gt_f4-fieldname = 'BSCHL'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  append gt_f4.

  gt_f4-fieldname = 'GSBER'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  append gt_f4.

  gt_f4-fieldname = 'KOSTL'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  append gt_f4.


  gt_f4-fieldname = 'SAKNR'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  append gt_f4.

  gt_f4-fieldname = 'SAKNR_BA'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  append gt_f4.

  gt_f4-fieldname = 'UMSKZ'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  append gt_f4.

  call method grid9->register_f4_for_fields
    exporting
      it_f4 = gt_f4[].

endform.                    " BUILD_F4_CAT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO_OPERACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_tipo_operacao input.
  data: tl_return_tab_tp type table of ddshretval with header line,
        tl_dselc_tp      type table of dselc      with header line.

  data: begin of tl_tipoo occurs 0,
          tp_operacao type zfit0043-tp_operacao,
          ds_operacao type zfit0043-ds_operacao,
          status_ctb  type zfit0043-status_ctb,
          liquidar    type zfit0043-liquidar,
        end of tl_tipoo.

  select tp_operacao ds_operacao status_ctb liquidar
    from zfit0043
    into table tl_tipoo
     where spras = sy-langu.

  sort tl_tipoo by tp_operacao.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'TP_OPERACAO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0043-TP_OPERACAO'
      value_org       = 'S'
    tables
      value_tab       = tl_tipoo
      return_tab      = tl_return_tab_tp
      dynpfld_mapping = tl_dselc_tp.

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENT'
    exceptions
      function_not_supported = 1
      others                 = 2.
endmodule.                 " SEARCH_TIPO_OPERACAO  INPUT
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_REL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pf_status_rel output.
  set pf-status 'ZREL'.
  set titlebar '9000'.
endmodule.                 " PF_STATUS_REL  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_CRIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_CRIAADT  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_cria  changing p_alv like wg_criaadt p_erro.
  data: vdata(10),
        wl_vlr(16),
        wl_umskz   type t074t-shbkz,
        v_gsber    type bseg-gsber.

  concatenate p_bukrs+2(2) '01' into v_gsber.

  refresh ti_bdcdata.
  concatenate  wg_cadger-dt_lct+6(2) wg_cadger-dt_lct+4(2) wg_cadger-dt_lct(4) into vdata separated by '.'.

  clear wl_umskz.
  sort tg_criaadt by umskz descending.
  read table tg_criaadt into wg_criaadt index 1.
  if wg_criaadt-umskz is initial.
    read table tg_criaadt into wg_criaadt index 2.
    if wg_criaadt-umskz is not initial.
      wl_umskz = wg_criaadt-umskz.
    endif.
  else.
    wl_umskz = wg_criaadt-umskz.
  endif.

  if wl_umskz  is initial.
    read table tg_criaadt into wg_criaadt index 1.
    write: wg_criaadt-wrbtr to wl_vlr.
    perform f_bdc_data using:
      'SAPMF05A'  '0100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'BKPF-BLDAT'       vdata ,
      ''          ''      ''   'BKPF-BLART'       'SA',
      ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
      ''          ''      ''   'BKPF-BUDAT'       vdata ,
      ''          ''      ''   'BKPF-MONAT'       wg_cadger-dt_lct+4(2),
      ''          ''      ''   'BKPF-WAERS'       wg_cadger-waers,
      ''          ''      ''   'BKPF-XBLNR'       wa_zfit0043-ds_operacao,
      ''          ''      ''   'RF05A-NEWBS'      wg_criaadt-bschl,
      ''          ''      ''   'RF05A-NEWKO'      wg_criaadt-saknrz,
      ''          ''      ''   'BDC_SUBSCR'       'SAPMF05A                                1300APPL_SUB_T',
      ''          ''      ''   'BDC_SUBSCR'       'SAPLSEXM                                0200APPL_SUB'.
    read table tg_criaadt into wg_criaadt index 2.
    perform f_bdc_data using:
      'SAPMF05A'  '0302'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'BSEG-WRBTR'	      wl_vlr,
      ''          ''      ''   'BSEG-GSBER'	      v_gsber,
      ''          ''      ''   'BSEG-ZTERM'	      '0004',
      ''          ''      ''   'BSEG-ZFBDT'	      vdata ,
      ''          ''      ''   'BSEG-SGTXT'	      wg_cadger-observacao,
      ''          ''      ''   'RF05A-NEWBS'      wg_criaadt-bschl,
      ''          ''      ''   'RF05A-NEWKO'      wg_criaadt-saknrz,
      'SAPMF05A'  '0300'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BU',
      ''          ''      ''   'BSEG-WRBTR'	      wl_vlr,
      ''          ''      ''   'BSEG-SGTXT'	      wg_cadger-observacao,
      'SAPLKACB'  '0002'  'X'  ''	                ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=ENTE',
      ''          ''      ''   'COBL-GSBER'	      v_gsber.
  else.
    read table tg_criaadt into wg_criaadt index 1.
    write: wg_criaadt-wrbtr to wl_vlr.
    perform f_bdc_data using:
      'SAPMF05A'  '0100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'BKPF-BLDAT'       vdata ,
      ''          ''      ''   'BKPF-BLART'       'SA',
      ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
      ''          ''      ''   'BKPF-BUDAT'       vdata ,
      ''          ''      ''   'BKPF-MONAT'       wg_cadger-dt_lct+4(2),
      ''          ''      ''   'BKPF-WAERS'       wg_cadger-waers,
      ''          ''      ''   'RF05A-NEWBS'      wg_criaadt-bschl,
      ''          ''      ''   'RF05A-NEWKO'      wg_criaadt-saknrz,
      ''          ''      ''   'RF05A-NEWUM'      wg_criaadt-umskz,
      ''          ''      ''   'BDC_SUBSCR'       'SAPMF05A                                1300APPL_SUB_T',
      ''          ''      ''   'BDC_SUBSCR'       'SAPLSEXM                                0200APPL_SUB'.
    read table tg_criaadt into wg_criaadt index 2.
    perform f_bdc_data using:
      'SAPMF05A'  '0304'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'BSEG-WRBTR'	      wl_vlr,
      ''          ''      ''   'BSEG-GSBER'	      v_gsber,
      ''          ''      ''   'BSEG-ZFBDT'	      vdata ,
      ''          ''      ''   'BSEG-SGTXT'	      wg_cadger-observacao,
      ''          ''      ''   'RF05A-NEWBS'      wg_criaadt-bschl,
      ''          ''      ''   'RF05A-NEWKO'      wg_criaadt-saknrz,
      'SAPMF05A'  '0300'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BU',
      ''          ''      ''   'BSEG-WRBTR'	      wl_vlr,
      ''          ''      ''   'BSEG-SGTXT'	      wg_cadger-observacao,
      'SAPLKACB'  '0002'  'X'  ''	                ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=ENTE',
      ''          ''      ''   'COBL-GSBER'	      v_gsber.
  endif.

  clear p_erro.
  perform zf_call_transaction using 'F-02' changing p_erro.
endform.                    " F_SHDB_CRIA
*&---------------------------------------------------------------------*
*&      Module  VALIDA_PARAMETROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module valida_parametros input.
  if wg_cadger-tp_operacao is initial or  obg_conteiner_cria is initial.
    exit.
  endif.

  select single *
  from zfit0043
  into wa_zfit0043
  where tp_operacao = wg_cadger-tp_operacao
     and spras = sy-langu.
  if sy-subrc = 0.
    if wa_zfit0043-status_ctb = 'S'.
      wg_cadger-question = 'X'.
    else.
      wg_cadger-question = ' '.
    endif.
    wg_cadger-liquidar = wa_zfit0043-liquidar.
    perform montar_layout_cria.

    call method grid9->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid9->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

endmodule.                 " VALIDA_PARAMETROS  INPUT

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV      text
*----------------------------------------------------------------------*
form f_imprime_smart using p_alv like it_saida_pa.
  data: ls_control        type ssfctrlop,
        ls_options        type ssfcompop,
        job_output_info   type ssfcrescl,
        ls_xsfparam_line  type ssfxsfp,
        v_bin_filesize    type i,
        it_docs           type standard table of docs,
        it_lines          type standard table of tline,
        lv_fname          type rs38l_fnam,
        lv_mail_recipient type swotobjid,
        lv_mail_sender    type swotobjid,
        lv_control        type ssfctrlop,
        lv_name           type so_name,
        lv_output         type ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20).

  data: i_otf       type itcoo occurs 0 with header line,
        i_tline     type table of tline with header line,
        i_receivers type table of somlreci1 with header line,
        i_record    like solisti1 occurs 0 with header line,
* Objects to send mail.
        i_objpack   like sopcklsti1 occurs 0 with header line,
        i_objtxt    like solisti1 occurs 0 with header line,
        i_objbin    like solisti1 occurs 0 with header line,
        i_reclist   like somlreci1 occurs 0 with header line,
* Work Area declarations
        wa_objhead  type soli_tab,
        w_ctrlop    type ssfctrlop,
        w_compop    type ssfcompop,
        w_return    type ssfcrescl,
        wa_doc_chng type sodocchgi1,
        w_data      type sodocchgi1,
        wa_buffer   type string, "To convert from 132 to 255
* Variables declarations
        v_form_name type rs38l_fnam,
        v_len_in    like sood-objlen,
        v_len_out   like sood-objlen,
        v_len_outn  type i,
        v_lines_txt type i,
        v_lines_bin type i.

  vl_form = 'ZFIR0003'.
*
  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = vl_form
    importing
      fm_name            = vl_name
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

*  Impresora
  ls_control-no_dialog = ' '. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = ' '.

  clear:job_output_info.
  call function vl_name
    exporting
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
    importing
      job_output_info    = job_output_info
    tables
      it_saida           = p_alv
    exceptions
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      others             = 5.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_GERADT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_SAIDA  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_geradt  changing p_alv like wg_criaadt p_erro.
  data: vxblnr     type bkpf-xblnr,
        vdata(10),
        wl_vlr(16),
        wcontb     type i,
        vsaknr     type skat-saknr,
        wl_bsik    type ty_bsik.

  refresh ti_bdcdata.
  vxblnr = wg_cadger-referencia.
  concatenate  wg_cadger-dt_lct+6(2) wg_cadger-dt_lct+4(2) wg_cadger-dt_lct(4) into vdata separated by '.'.
  select single * " bukrs lifnr belnr dmbtr dmbe2 budat buzei gsber shkzg  umskz
          from bsik
          into corresponding fields of wl_bsik
          where bukrs eq p_bukrs
          and   belnr eq p_alv-belnr.

  write: p_alv-wrbtr to wl_vlr.

  select * "bukrs lifnr belnr dmbtr dmbe2 budat buzei  gsber shkzg  umskz
            from bsik
            into corresponding fields of table it_bsik_aux
            where bukrs eq p_bukrs
            and   belnr eq p_alv-belnr.

  wcontb = 0.
  loop at it_bsik_aux into wa_bsik.
    add 1 to wcontb.
  endloop.

  if p_alv-saknrz_ba is not initial.
    vsaknr = p_alv-saknrz_ba.
  else.
    vsaknr = p_alv-saknrz.
  endif.

  perform f_bdc_data using:
    'SAPMF05A'  '0103'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'KZ',
    ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadger-dt_lct+4(2),
    ''          ''      ''   'BKPF-WAERS'       wg_cadger-waers,
    ''          ''      ''   'BKPF-XBLNR'       vxblnr,
    ''          ''      ''   'RF05A-KONTO'      vsaknr,
    ''          ''      ''   'BSEG-GSBER'       wl_bsik-gsber,
    ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
    ''          ''      ''   'RF05A-AGKON'      p_alv-lifnr,
    ''          ''      ''   'RF05A-AGKOA'      'K',
    ''          ''      ''   'RF05A-AGUMS'      wl_bsik-umskz.
  perform f_bdc_data using:
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  '',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA',
    ''          ''      ''   'RF05A-SEL01(01)'  p_alv-belnr.

  if wcontb gt 1.
    perform f_bdc_data using:
    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PI',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6102PAGE',
    ''          ''      ''   'BDC_CURSOR'	      'DF05B-PSBET(01)',
    ''          ''      ''   'RF05A-ABPOS'      '1'.
  endif.

  perform f_bdc_data using:
  'SAPDF05X'  '3100'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=BU',
  ''          ''      ''   'RF05A-ABPOS'      '1'.

  clear p_erro.
  perform zf_call_transaction using 'F-53' changing p_erro.

  wait up to 5 seconds.

  if p_erro = 'X'.
    rollback work.
  else.
    commit work.
  endif.

endform.                    " F_SHDB_GERADT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_2000 output.
*  DATA: "EVENT TYPE CNTL_SIMPLE_EVENT,
*                 "EVENTS TYPE CNTL_SIMPLE_EVENTS,
*                 TL_FILTER           TYPE LVC_T_FILT,
*                 WL_FILTER           TYPE LVC_S_FILT,
*                 TL_FUNCTION         TYPE UI_FUNCTIONS,
*                 WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE.

  clear wa_layout.
  wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
  wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
  wa_stable-row        = c_x.
  wa_layout-info_fname = 'COLOR'.

  wa_layout-no_toolbar = c_x.

  wa_layout-grid_title = ' '.
  "GRID1
  if obg_conteiner_cta is initial.
    create object obg_conteiner_cta
      exporting
        container_name = g_cc_cta.


    create object grid10
      exporting
        i_parent = obg_conteiner_cta.


    perform montar_layout_cta.
    create object obg_toolbar
      exporting
        io_alv_grid = grid10.

*      * Register event handler
    set handler obg_toolbar->on_toolbar for grid10.
    set handler obg_toolbar->handle_user_command_cta for grid10.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = ''.


    call method grid10->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter        "I_CONSISTENCY_CHECK = 'X'
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_conta[].

    call method grid10->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    refresh gt_f4.
    gt_f4-fieldname = 'BANKL'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    append gt_f4.

    call method grid10->register_f4_for_fields
      exporting
        it_f4 = gt_f4[].

    set handler:
             "LCL_EVENT_HANDLER=>CATCH_HOTSPOT_CTA FOR GRID10,
             lcl_event_handler=>on_data_changed_cta for grid10,
             lcl_event_handler=>on_f4 for grid10,
             lcl_event_handler=>on_data_changed_finished_cta for grid10.


  else.
    perform montar_layout_cta.
    call method grid10->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid10->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_cta .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
         1 ' '           ' '        'TG_CONTA' 'BVTYP'           'Seq.'(314)         '08' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKS'           'País'(315)         '10' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKL'           'Banco'(135)        '10' 'X' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKN'           'Cta.Bancária'(316) '18' 'X' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BKONT'           'CC'(317)           '02' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'IBAN'            'IBAN'(318)         '30' 'X' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'SWIFT'           'SWIFT'(319)        '10' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKA'           'Nome Banco'(320)   '20' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_CTA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9300 output.
  set pf-status 'Z004'.
  set titlebar '9300'.
endmodule.                 " STATUS_9300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_9300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_9300 output.
  clear wa_layout.
  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
  wa_layout-col_opt    = c_x.
  wa_stable-row        = c_x.
  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt   = 'X'.
  wa_layout-no_toolbar = space.

  if obg_conteiner_st is initial.
    create object obg_conteiner_st
      exporting
        container_name = g_cc_status.


    create object grid11
      exporting
        i_parent = obg_conteiner_st.


    perform montar_layout_st.


    wa_layout-no_toolbar = 'X'.

    wa_layout-grid_title = 'Mudar Status'(321).

    call method grid11->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter        "I_CONSISTENCY_CHECK = 'X'
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_status[].

*

    call method grid11->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid11->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    set handler:
              lcl_event_handler=>on_data_changed_st for grid11,
              lcl_event_handler=>on_data_changed_finished_st for grid11.


  else.
    perform montar_layout_st.
    call method grid11->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid11->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.

endmodule.                 " CRIA_OBJETOS_9300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_st .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 ' '                ' '           'TG_STATUS' 'TIPO'            'Tipo'(127)              '20' ' ' ' ' ' ',
        2 ' '                ' '           'TG_STATUS' 'BELNR'           'Doc.Cta'(322)           '12' ' ' ' ' ' ',
        3 ' '                ' '           'TG_STATUS' 'BUDAT'           'Dt.Lcto'(005)           '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_STATUS' 'LIFNR'           'Fornecedor'(006)        '10' ' ' ' ' ' ',
        5 ' '                ' '           'TG_STATUS' 'NAME1'           'Nome'(323)              '20' ' ' ' ' ' ',
        6 ' '                ' '           'TG_STATUS' 'DMBE2'           'Valor US$'(221)         '15' ' ' ' ' ' ',
        7 ' '                ' '           'TG_STATUS' 'STATUS'          'Status'(324)            '05' 'X' ' ' ' '.

endform.                    " MONTAR_LAYOUT_ST
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9300 input.
  case ok-code.
    when 'SALVAR'.
      loop at tg_status into wg_status.
        if wg_status-status = '' or wg_status-status = 'B'.
          update zfit0036 set status        = wg_status-status
                              rg_atualizado = ''
                              lote          = ''
                              dt_pgto     = ''
                              moeda_pgto  = ''
                              tx_cambio   = 0
                              observacao  = ''
                              referencia  = ''
                              operacao    = ''
                              desp_tar    = ''
                              vlr_pgto    = 0
                              forma_pg    = ''
                              motivo      = ''
                              bvtyp       = ''
                              lifnr       = ''
                              swift_1     = ''
                              banks_1     = ''
                              bankl_1     = ''
                              banka_1     = ''
                              bankn_1     = ''
                              iban_1      = ''
                              bvtyp_2     = ''
                              swift_2     = ''
                              banks_2     = ''
                              bankl_2     = ''
                              banka_2     = ''
                              bankn_2     = ''
                              iban_2      = ''
          where obj_key = wg_status-obj_key
          and   belnr   = wg_status-belnr36
          and   buzei   = wg_status-buzei36.
          commit work.
          message 'Alteração de STATUS efetutada'(325) type 'I'.
          delete it_saida where checkbox = 'X'.
        endif.
      endloop.

    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_9300  INPUTWA_SAIDA-BUDAT+6(4)
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  refresh: fcode.
  append 'SALVAR' to fcode.
  set pf-status 'Z004' excluding fcode.
  call method cl_gui_cfw=>dispatch.
  set titlebar '0200'.

endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module carrega_lotes output.

  data: v_msg              type char50,
        vobjectid          type cdhdr-objectid,
        t_lotes            type table of ziv_lotes_imp,
        t_lotes2           type table of zfi_lotes_imp,
        w_lotes            type          ziv_lotes_imp,
        t_estra            type table of zfi_estrategia_imp,
        t_estra2           type table of zfi_estrategia_zgl,
        w_estra            type          zfi_estrategia_imp,
        w_estra2           type          zfi_estrategia_zgl,
        t_docs             type table of ziv_docs_imp,
        t_docs2            type table of zgl_docs_imp,
        w_docs             type          ziv_docs_imp,
        t_zinv_lotes_aprov type table of zinv_lotes_aprov,
        w_zinv_lotes_aprov type zinv_lotes_aprov,
        w_bkpf             type bkpf,
        w_bseg             type bseg,
        w_zglt035          type zglt035.

  data: t_cdhdr  type table of ty_cdhdr,
        t_cdpos  type table of ty_cdpos,
        wa_cdhdr type ty_cdhdr,
        wa_cdpos type ty_cdpos.


  refresh: t_lotes,t_estra,t_docs,tg_estra.

  select single *
    from bkpf
    into w_bkpf
    where bukrs = wa_saida-bukrs
    and   belnr = wa_saida-belnr_adt_c.

  if wa_saida-obj_key+0(2) = 'AC'  and wa_saida-opera ne '08' and sy-subrc = 0. "PBI - 74866 - CBRAND
    if w_bkpf-awkey+0(5) = 'ZGL17'. "pega a estrategia da ZGL
      select single *
        from zglt035
        into w_zglt035
       where doc_lcto = w_bkpf-awkey+5(10).

      call function 'Z_GL_ESTRATEGIA_LISTA'
        exporting
          v_usuario = sy-uname
          v_lote    = w_zglt035-lote
        importing
          msg       = v_msg
        tables
          t_lotes   = t_lotes2
          t_estra   = t_estra2
          t_docs    = t_docs2.

      refresh tg_estra.
      sort t_estra2 by nivel aprovador.
      delete adjacent duplicates from t_estra2 comparing nivel aprovador.
      loop at t_estra2 into w_estra2.
        move-corresponding w_estra2 to wa_estra.
        append wa_estra to tg_estra.
      endloop.
    else.
* ---> S4 Migration - 15/06/2023 - MA
*Não tem os campos chaves
      select single *
       from bseg
       into w_bseg
       where bukrs eq wa_saida-bukrs
       and   belnr eq wa_saida-belnr_adt_c
       and   ebeln ne ''.              "#EC CI_DB_OPERATION_OK[2431747]

*<--- S4 Migration - 15/06/2023 - MA


      if sy-subrc = 0.
        vobjectid = w_bseg-ebeln.
        select objectclas objectid changenr tcode username udate
          from cdhdr
            into table t_cdhdr
               where objectid   eq vobjectid
                 and objectclas eq 'EINKBELEG'
                 and tcode      eq 'ME29N'.

        if sy-subrc is initial.
          select  objectclas objectid changenr fname value_new
            from cdpos
            into table t_cdpos
            for all entries in t_cdhdr
            where objectclas eq t_cdhdr-objectclas
            and   objectid   eq t_cdhdr-objectid
            and   changenr   eq t_cdhdr-changenr
            and   fname      eq 'FRGKE'
            and   value_new  eq '2'.
        endif.
        sort:  t_cdhdr by objectid ascending changenr descending,
               t_cdpos by objectclas objectid changenr.

        refresh tg_estra.
        read table t_cdhdr into wa_cdhdr
                      with key objectid   = vobjectid.
        read table t_cdpos into wa_cdpos with key  objectclas = wa_cdhdr-objectclas
                                                   objectid   = wa_cdhdr-objectid
                                                   changenr   = wa_cdhdr-changenr binary search.
        if sy-subrc = 0.
          wa_estra-bukrs      = wa_saida-bukrs.
          wa_estra-lote       = vobjectid.
          wa_estra-valor_de   = 0.
          wa_estra-valor_ate  = 0.
          wa_estra-aprovador  = wa_cdhdr-username.
          wa_estra-nivel      = 0.
          append wa_estra to tg_estra.
        endif.
      endif.
    endif.
  elseif   wa_saida-opera = '08' and sy-subrc = 0.
    refresh it_zadt_sol_aprov.
    select *
       from zadt_sol_aprov
       into table it_zadt_sol_aprov
       where  nro_sol  =  wa_saida-referencia+4(10).

    loop at it_zadt_sol_aprov into data(w_zadt_sol_aprov).
      move-corresponding w_zadt_sol_aprov to wa_estra.
*      WA_ESTRA-ESTADO       = ICON_CHECKED .
*      WA_ESTRA-OPCOES       = ICON_SYSTEM_UNDO .
      wa_estra-estado       = icon_led_yellow .
      wa_estra-opcoes       = icon_set_state  .
      append wa_estra to tg_estra.
    endloop.

  else.

    call function 'Z_IV_ESTRATEGIA_LISTA'
      exporting
        v_usuario = sy-uname
        lote      = wa_saida-lote "PBI - 74866 - CBRAND
      importing
        msg       = v_msg
      tables
        t_lotes   = t_lotes
        t_estra   = t_estra
        t_docs    = t_docs.

    delete t_estra where lote ne wa_saida-lote.
    refresh tg_estra.

    sort t_estra by nivel aprovador.
    delete adjacent duplicates from t_estra comparing nivel aprovador.
    loop at t_estra into w_estra.
      if w_estra-lote = wa_saida-lote.
        move-corresponding w_estra to wa_estra.
        append wa_estra to tg_estra.
      endif.
    endloop.

    if tg_estra[] is initial.
      select *
        from zinv_lotes_aprov
        into table t_zinv_lotes_aprov
        where bukrs = wa_saida-bukrs
        and   lote  = wa_saida-lote.

      loop at t_zinv_lotes_aprov into w_zinv_lotes_aprov.
        move-corresponding w_zinv_lotes_aprov to wa_estra.
*      WA_ESTRA-ESTADO       = ICON_CHECKED .
*      WA_ESTRA-OPCOES       = ICON_SYSTEM_UNDO .
        wa_estra-estado       = icon_led_yellow .
        wa_estra-opcoes       = icon_set_state  .
        append wa_estra to tg_estra.
      endloop.

    endif.
  endif.




endmodule.                 " CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_200 output.
*  DATA: EVENT TYPE CNTL_SIMPLE_EVENT,
*              TL_FILTER           TYPE LVC_T_FILT,
*              WL_FILTER           TYPE LVC_S_FILT,
*              TL_FUNCTION         TYPE UI_FUNCTIONS,
*              WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE.
  "GRID12
  if obg_conteiner_estra is initial.
    create object obg_conteiner_estra
      exporting
        container_name = g_cc_estra.


    create object grid12
      exporting
        i_parent = obg_conteiner_estra.


    perform montar_layout_estra.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

    clear wa_layout.
*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Estratégia de Liberação'(326).
    wa_layout-no_toolbar = c_x.
    perform montar_layout_estra.

    call method grid12->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

  else.
    perform montar_layout_estra.
    call method grid12->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = t_fieldcatalog[].

    call method grid12->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_estra .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZADTO_APROVADOR'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         'Valor de'(327)      '15' ' ' ' ' ' ',
        1 'ZADTO_APROVADOR'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        'Valor ate'(328)     '15' ' ' ' ' ' ',
        1 'ZADTO_APROVADOR'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        'Aprovador'(329)     '20' ' ' ' ' ' ',
        1 ' '                         ' '               'TG_ESTRA' 'ESTADO'           'Estado'(330)        '10' ' ' ' ' ' ',
        1 ' '                         ' '               'TG_ESTRA' 'OPCOES'           'Opções Liber.'(331) '12' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  case ok-code.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0300 output.
  set pf-status 'Z005'.
*  CALL METHOD CL_GUI_CFW=>DISPATCH.
  set titlebar '0300'.

endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0300 input.
  case ok-code.
    when 'ARQ'.
      if wg_atach-path is initial and  wg_atach-updl is not initial.
        message 'Informe o caminho do arquivo .PDF (Invoice)'(332) type 'I'.
      else.
        perform zf_grava_invoice.
        set screen 0.
      endif.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_grava_invoice .
  data: begin of itab occurs 0,
          field(256),
        end of itab.

  data: length  like sy-tabix,
        lengthn like sy-tabix,
        varqtmp type string value 'c:\temp\invoice.pdf'. "visualizar

  vfilename     = wg_atach-path. " Diretorio Windows

  if wg_atach-updl is not initial.
    call function 'GUI_UPLOAD'
      exporting
        filename   = vfilename
        filetype   = 'BIN'
      importing
        filelength = length
      tables
        data_tab   = itab.

    move: wa_saida-obj_key to wa_zfit0076-obj_key,
          wa_saida-belnr36 to wa_zfit0076-belnr,
          wa_saida-buzei36 to wa_zfit0076-buzei.

    refresh it_zfit0076.
    loop at itab.
      move: sy-tabix   to wa_zfit0076-pos,
            itab-field+0(128)   to wa_zfit0076-linha,
            itab-field+128(128) to wa_zfit0076-linha2.
      append wa_zfit0076 to it_zfit0076.
    endloop.

    clear itab.
    refresh itab.
    delete from zfit0076 where obj_key = wa_saida-obj_key
                         and   belnr   = wa_saida-belnr36
                         and   buzei   = wa_saida-buzei36.

    modify zfit0076 from table it_zfit0076.

*    "Grava Local
    update zfit0036 set path = vfilename
      where obj_key = wa_saida-obj_key
      and   belnr   = wa_saida-belnr36
      and   buzei   = wa_saida-buzei36.

    message 'Upload efetuado com sucesso '(333) type 'I'.
  else.
    clear itab.
    loop at it_zfit0076 into wa_zfit0076.
      move wa_zfit0076-linha to itab-field.
      concatenate itab-field wa_zfit0076-linha2 into itab-field.
      append itab.
      clear itab.
    endloop.

    call function 'GUI_DOWNLOAD'
      exporting
        filename     = varqtmp
        filetype     = 'BIN'
        bin_filesize = length
      importing
        filelength   = lengthn
      tables
        data_tab     = itab.

    call method cl_gui_frontend_services=>execute
      exporting
        document = varqtmp.
*    minimized = 'X'.
  endif.
endform.                    " ZF_GRAVA_INVOICE
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PATH_A  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_path_a input.
  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = 'C:\'
      mode             = 'S'       " MASK = '*.TXT'
      title            = 'Busca de Arquivo'(161)
    importing
      filename         = wg_atach-path
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.

endmodule.                 " SEARCH_PATH_A  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields_0300 output.
  loop at screen.
    if wg_atach-down = 'X'.
      if screen-name eq 'WG_ATACH-UPDL'.
        clear wg_atach-updl.
        modify screen.
        exit.
      endif.
    elseif screen-name eq 'WG_ATACH-DOWN'.
      clear wg_atach-down.
      modify screen.
      exit.
    endif.
  endloop.
endmodule.                 " TRATA_FIELDS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9400 output.

  data: wa_linha(60) type c,
        data_ini(10) type c,
        data_fim(10) type c,
        wa_text      type sdydo_text_element.


  set pf-status 'ST_9400'.
  set titlebar 'TL_9400'.

  if obg_conteiner_bnco is initial.

    create object obg_conteiner_bnco
      exporting
        container_name = 'CONTAINER_9400'.


    create object grid13
      exporting
        i_parent = obg_conteiner_bnco.


    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.

    clear wa_layout.
    wa_layout-no_toolbar = space.
    wa_layout-no_toolbar = c_x.

    perform montar_layout_banco .

    call method grid13->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_arq_bancos[].

    create object obj_dyndoc_bnco
      exporting
        no_margins = 'X'.

    wa_linha = 'Relatório Arquivos Bancos'(334).
    wa_text = wa_linha.
    call method obj_dyndoc_bnco->new_line.

    call method obj_dyndoc_bnco->add_text
      exporting
        text      = wa_text
        sap_style = 'HEADING'.


    if  p_dt_p[] is not initial.
      concatenate  p_dt_p-low+6(2)  '-' p_dt_p-low+4(2)  '-' p_dt_p-low+0(4) into data_ini.
      concatenate  p_dt_p-high+6(2) '-' p_dt_p-high+4(2) '-' p_dt_p-high+0(4)  into data_fim.


      concatenate 'Data Lançamento: '(190) data_ini   into wa_linha separated by space.
      if data_fim <> '00.00.0000' .
        concatenate  wa_linha  'até'(335)  data_fim into wa_linha separated by space.
      endif.

      wa_text = wa_linha.
      call method obj_dyndoc_bnco->new_line.

      call method obj_dyndoc_bnco->add_text
        exporting
          text         = wa_text
          sap_fontsize = cl_dd_area=>list_normal.
    endif.



    if p_lote is not initial.
      concatenate 'Lote: ' p_lote-low into wa_linha separated by space.

      wa_text = wa_linha.
      call method obj_dyndoc_bnco->new_line.

      call method obj_dyndoc_bnco->add_text
        exporting
          text         = wa_text
          sap_fontsize = cl_dd_area=>list_normal.
    endif.

    if editcontainer_bnco is initial.

      create object editcontainer_bnco
        exporting
          container_name = 'HEADER'.

    endif .

    call method obj_dyndoc_bnco->merge_document.

    call method obj_dyndoc_bnco->display_document
      exporting
        reuse_control      = 'X'
        parent             = editcontainer_bnco
      exceptions
        html_display_error = 1.

  else.
    call method grid12->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9400 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
  endcase.

endmodule.


form montar_layout_banco .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1   ' '      ' '           'IT_ARQ_BANCOS' 'BUKRS'              'Empresa'(336)                '07' ' ' ' ' ' ',
        2   ' '      ' '           'IT_ARQ_BANCOS' 'BELNR'              'Doc.Contabil'(216)           '12' ' ' ' ' ' ',
        3   ' '      ' '           'IT_ARQ_BANCOS' 'AUGBL'              'Doc.Comp.'(337)              '12' ' ' ' ' ' ',
        4   ' '      ' '           'IT_ARQ_BANCOS' 'NOME_BENEF'         'Nome Favorecido'(338)        '25' ' ' ' ' ' ',
        5   ' '      ' '           'IT_ARQ_BANCOS' 'DT_TRANSF'          'Dt. Pgto'(339)              '10' ' ' ' ' ' ',
        6   ' '      ' '           'IT_ARQ_BANCOS' 'LOTE'               'Lote'(126)                   '10' ' ' ' ' ' ',
        7   ' '      ' '           'IT_ARQ_BANCOS' 'VLR_PAGTO'          'Valor Pgto'(340)             '12' ' ' ' ' ' ',
        8   ' '      ' '           'IT_ARQ_BANCOS' 'MOEDA_TRANSF'       'Moeda'(131)                  '06' ' ' ' ' ' ',
        9   ' '      ' '           'IT_ARQ_BANCOS' 'NRO_REF'            'Referencia'(140)             '10' ' ' ' ' ' ',
        10  ' '      ' '           'IT_ARQ_BANCOS' 'INVOICE'            'Invoice'(003)                '10' ' ' ' ' ' ',
        11  ' '      ' '           'IT_ARQ_BANCOS' 'NOME_ARQUIVO'       'Nome Arquivo'(341)           '30' ' ' ' ' ' ',
        12  ' '      ' '           'IT_ARQ_BANCOS' 'USUARIO_ARQ'        'Usuario'(342)                '10' ' ' ' ' ' ',
        13  ' '      ' '           'IT_ARQ_BANCOS' 'DT_GER_ARQ'         'Dt.Ger.Arq'(343)             '10' ' ' ' ' ' ',
        14  ' '      ' '           'IT_ARQ_BANCOS' 'HR_GER_ARQ'         'Hora Ger.Arq'(344)           '10' ' ' ' ' ' ',
        15  ' '      ' '           'IT_ARQ_BANCOS' 'STATUS'             'Status Arquivo'(345)         '15' ' ' ' ' ' '.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_REL_BANCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_rel_banco .

  clear p_dt_trans.

  loop at p_dt_p into data(w_data).
    w_dt_trans-sign    = w_data-sign.
    w_dt_trans-option  = w_data-option.
    concatenate w_data-low+0(4)  '-' w_data-low+4(2)  '-' w_data-low+6(2) into  w_dt_trans-low.
    concatenate w_data-high+0(4) '-' w_data-high+4(2) '-' w_data-high+6(2) into  w_dt_trans-high.
    append w_dt_trans to p_dt_trans.
  endloop.


  select *
    from zfit0037 into table  it_zfit0037
   where dt_transf in p_dt_trans
    and  lote      in p_lote.

  if it_zfit0037[] is not initial.
*    select   obj_key    belnr      buzei     bukrs invoice    navio  lote dt_pgto
*             tx_cambio  moeda_pgto vlr_pgto  hbkid observacao status forma_pg
*             motivo rg_atualizado bvtyp operacao id_tipo_invoice banka_1 banks_1 invoice_terc
*             matnr lifnr referencia belnr_adt_c
    select *
     from zfit0036
      into corresponding fields of table it_zfit0036
       for all entries in it_zfit0037
     where lote  eq it_zfit0037-lote
     and   bukrs eq p_bukrs.

  endif.

  refresh: it_remessa, it_backup.
  wa_arq_bancos-bukrs  = p_bukrs.

  if ( wa_arq_bancos-bukrs ne '0035' )  and ( wa_arq_bancos-bukrs ne '0038' ).

    call function 'EPS_GET_DIRECTORY_LISTING'
      exporting
        dir_name               = '/usr/sap/EDITC/remessa/'
        file_mask              = '*.*'
      tables
        dir_list               = it_remessa
      exceptions
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        others                 = 8.

    call function 'EPS_GET_DIRECTORY_LISTING'
      exporting
        dir_name               = '/usr/sap/EDITC/backup/'
        file_mask              = '*.*'
      tables
        dir_list               = it_backup
      exceptions
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        others                 = 8.

  elseif   ( wa_arq_bancos-bukrs eq '0035' )  and ( wa_arq_bancos-bukrs eq '0038' ).
    call function 'EPS_GET_DIRECTORY_LISTING'
      exporting
        dir_name               = '/usr/sap/LDCPRD/remessa/'
        file_mask              = '*.*'
      tables
        dir_list               = it_remessa
      exceptions
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        others                 = 8.

    call function 'EPS_GET_DIRECTORY_LISTING'
      exporting
        dir_name               = '/usr/sap/LDCPRD/backup/'
        file_mask              = '*.*'
      tables
        dir_list               = it_backup
      exceptions
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        others                 = 8.

  endif.

  loop at it_zfit0037 into wa_zfit0037.
    if wa_zfit0037-obj_key is not initial.
      read table it_zfit0036 into wa_zfit0036   with key  lote    = wa_zfit0037-lote
                                                          obj_key = wa_zfit0037-obj_key.
    else.
      read table it_zfit0036 into wa_zfit0036   with key  lote    = wa_zfit0037-lote.
    endif.
    if sy-subrc = 0.
      wa_arq_bancos-bukrs         = wa_zfit0036-bukrs.
      wa_arq_bancos-invoice       = wa_zfit0036-invoice.
    else.
      continue.
    endif.

    wa_arq_bancos-status = 'Em processamento'(346).
    read table it_remessa into wa_remessa with key name = wa_zfit0037-nome_arquivo.
    if sy-subrc = 0.
      wa_arq_bancos-status = 'Aguardando Transmissão'(347).
    else.
      read table it_backup into wa_backup with key name = wa_zfit0037-nome_arquivo.
      if sy-subrc = 0.
        wa_arq_bancos-status = 'Transmitido'(348).
      endif.
    endif.
    wa_arq_bancos-belnr          = wa_zfit0037-belnr.
    wa_arq_bancos-augbl          = wa_zfit0037-augbl.
    wa_arq_bancos-nome_benef     = wa_zfit0037-nome_benef.
    wa_arq_bancos-dt_transf      = wa_zfit0037-dt_transf.
    wa_arq_bancos-lote           = wa_zfit0037-lote.
    wa_arq_bancos-vlr_pagto      = wa_zfit0037-vlr_pagto.
    wa_arq_bancos-moeda_transf   = wa_zfit0037-moeda_transf.
    wa_arq_bancos-nro_ref        = wa_zfit0037-nro_ref.
    wa_arq_bancos-nome_arquivo   = wa_zfit0037-nome_arquivo.
    wa_arq_bancos-usuario_arq    = wa_zfit0037-usuario_arq.
    wa_arq_bancos-dt_ger_arq     = wa_zfit0037-dt_ger_arq.
    wa_arq_bancos-hr_ger_arq     = wa_zfit0037-hr_ger_arq.

    append wa_arq_bancos to it_arq_bancos.
    clear: wa_arq_bancos, wa_zfit0037, wa_zfit0036, wa_remessa, wa_backup.

  endloop.

endform.

include zfir0025_pbo.

include zfir0025_forms.

include zfir0025_pai.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_XML_SWIFT
*&---------------------------------------------------------------------*
form f_gera_xml_swift using p_local p_nome_arq .
  types: begin of ty_swift,
           revision          type c length 30,
           sender_ref        type c length 30,
           messageidentifier type c length 30,
           format            type c length 30,
           dn_empresa        type c length 30,
           dn_banco          type c length 30,
           bc_empresa        type c length 30,
           bc_banco          type c length 30,
           notreque          type c length 30,
           service           type c length 30,
           ntresponder       type c length 30,
           filedescription   type c length 30,
           fileinfo          type c length 35,
           filedigest        type c length 30,
           filelgname        type c length 30,
           body              type c length 30,

         end of ty_swift.

  data: lv_newline type c value cl_abap_char_utilities=>newline,
        result_tab type match_result_tab,
        lv_c       type i,
        lv_i       type i.


  field-symbols: <fs_swift> type ty_swift.

  data: lt_output type standard table of ty_swift,
        lt_input  type xstring.

  data: lt_result_xml type standard table of abap_trans_resbind,
        ls_result_xml type abap_trans_resbind.

  data: lva_localsave type string,
        lva_file_name type rlgrap-filename,
        lva_file_path type rlgrap-filename.

  data: t_file       type table of zarq_bloomberg with header line,
        wa_t_file    type zarq_bloomberg,
        lv_size      type i,
        lv_size_c(6) type c.


  clear: wg_zfit0173.
  select single *
     from zfit0173
  into wg_zfit0173
  where bukrs eq p_bukrs
    and hbkid eq wg_cadliq-hbkid.

* Try-catch variables
  data: ls_rif_ex   type ref to cx_root, ls_var_text type string.
  append initial line to lt_output assigning <fs_swift>.

  translate wg_zfit0173-dn_empresa to lower case.
  translate wg_zfit0173-dn_banco to lower case.

  split p_nome_arq at '.' into lva_file_name lva_file_path.

  if sy-subrc eq 0.
    <fs_swift>-revision          = '2.0.6'.
    concatenate 'Arquivo_' wg_cadliq-hbkid into <fs_swift>-sender_ref. condense <fs_swift>-sender_ref no-gaps.
    <fs_swift>-messageidentifier = 'pain.xxx.international'.
    <fs_swift>-format            = 'File'.
    concatenate  wg_zfit0173-dn_empresa  ',' 'o=swift' into <fs_swift>-dn_empresa. condense <fs_swift>-dn_empresa no-gaps.
    concatenate  wg_zfit0173-dn_banco    ',' 'o=swift' into <fs_swift>-dn_banco.   condense <fs_swift>-dn_banco no-gaps.
    <fs_swift>-bc_banco          = wg_zfit0173-bc_banco.
    <fs_swift>-bc_empresa        = wg_zfit0173-bc_empresa.
    <fs_swift>-notreque          = 'true'.
    <fs_swift>-service           = 'swift.corp.fa'. "'swift.corp.fa!p'.Ambiente teste.
    <fs_swift>-ntresponder       = 'o=amxibrdf,o=swift'.
    concatenate p_bukrs wg_cadliq-hbkid  lva_file_name into  <fs_swift>-filedescription. condense <fs_swift>-filedescription no-gaps.
    "<fs_swift>-filedescription   = 'AMXIISO20022[ISO[AL2[TEST'. "Verificar onde buscar.
    <fs_swift>-fileinfo          = 'swcompression=none,characterset=utf-8'.
    <fs_swift>-filedigest        = 'SHA-256'.
    <fs_swift>-filelgname        = p_nome_arq.
    <fs_swift>-body              = p_nome_arq.
  endif.

  get reference of lt_output into ls_result_xml-value.
  ls_result_xml-name = 'ISWIFT'.
  append ls_result_xml to lt_result_xml.

* Perform the XSLT stylesheet
  try.
      call transformation zfi_swift_001
      source (lt_result_xml)
      result xml lt_input.

    catch cx_root into ls_rif_ex.
      ls_var_text = ls_rif_ex->get_text( ).
      message ls_var_text type 'E'.
  endtry.

  concatenate p_local lva_file_name '.fa' into lva_localsave.

  open dataset lva_localsave for output in binary mode.
  if ( syst-subrc = 0 ).

    transfer lt_input to lva_localsave.
    close dataset lva_localsave.


***** tratar arquivo XML

    open dataset lva_localsave in text mode for input encoding utf-8 .
    clear: t_file[].
    do.
      read dataset lva_localsave into t_file.
      if sy-subrc  is initial.
        lv_size = lv_size + strlen( t_file-linha ).
        append t_file.
      else.
        exit.
      endif.
    enddo.
    close dataset lva_localsave.

    cl_bcs_convert=>xstring_to_string(
      exporting
        iv_xstr   = lt_input
        iv_cp     = 1100                " SAP character set identification
      receiving
        rv_string = data(lv_string)
    ).

    find all occurrences of lv_newline in lv_string results result_tab.
    describe table result_tab lines lv_i.
    lv_c = lv_c + lv_i.
    "lv_c = lv_c * 2.

    loop at t_file into wa_t_file .
      if sy-tabix = 1.
        lv_size = lv_size + lv_c + 24." + 2.
        move lv_size to lv_size_c.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_size_c
          importing
            output = lv_size_c.
*** TESTE US
        types:
           char01     type c length 1.

        data:
          s    type string,
          hex  type x length 4,
          char type char01.

        field-symbols:
          <pc>.

        hex = '1F'.

        assign hex to <pc> casting type char01.
        char = <pc>.
*** TESTE US

        concatenate  char lv_size_c space space space space space space space space space space space space space space space
                                    space space space space space space space space space  wa_t_file-linha into wa_t_file-linha respecting blanks.


        modify  t_file  from  wa_t_file index sy-tabix transporting linha.
      endif.
    endloop.

    delete dataset lva_localsave .


    data: lv_content type xstring,
          xml_string type string.

    loop at t_file into wa_t_file.
      describe table t_file lines data(v_linhas).

      if wa_t_file-linha is not initial.
        if sy-tabix = 1 .
          concatenate xml_string wa_t_file-linha into  xml_string.
        else.
          concatenate xml_string wa_t_file-linha into  xml_string separated by lv_newline.
        endif.
      endif.
    endloop.

    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text     = xml_string
        mimetype = 'text/xml; charset=utf-8'
      importing
        buffer   = lv_content.

*    OPEN DATASET lva_localsave  FOR OUTPUT IN TEXT MODE ENCODING DEFAULT. "NON-UNICODE. "WITH WINDOWS LINEFEED.
*    LOOP AT t_file.
*      TRANSFER t_file TO lva_localsave.
*    ENDLOOP.

    open dataset lva_localsave for output in binary mode.

    if sy-subrc = 0.
      transfer lv_content to lva_localsave.
    endif.

    close dataset lva_localsave.

  else.
    message id '00' type 'E' number '398' with
   'Erro ao criar o arquivo'(166)
   'na pasta'(167)
   p_local .
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_BANCO_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form verifica_banco_empresa .

  data: wl_t012k    type t012k,
        wl_bsis_aux type bsis.

  if gva_hbkid is initial.
    gva_hbkid = wg_cadliq-hbkid.
  endif.

  loop at it_saida into wa_saida where checkbox = 'X'.
    if wa_saida-obj_key+0(1) = 'A'.

      select single *
        from bsis
        into wl_bsis_aux
        where bukrs eq wa_saida-bukrs
              and gjahr eq wa_saida-dt_pgto(4)
              and belnr eq wa_saida-obj_key+1
              and bschl  = 50.

      if wl_bsis_aux is not initial.

        select single *
            from t012k
            into wl_t012k
            where bukrs eq wa_saida-bukrs
              and  hbkid eq gva_hbkid.

        if wl_t012k-hkont <> wl_bsis_aux-hkont.
          message i000(z01)
             with 'Para o doc. Contábil:'(352)
                   wl_bsis_aux-hkont
                   'a conta do Banco está diferente'(353)
                   'da conta do Banco Empresa'(354).
          wl_erro = 'X'.
          exit.
        endif.
      endif.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_CRIA_ADT_GBP_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_cria_adt_gbp_0200 .

  data: wl_tcurr_usd type tcurr,
        wl_tcurr_gbp type tcurr.


  data: lc_datat    type char08,
        lc_gdatu    type scurr-gdatu,
        lc_lastdate type scurr-gdatu.


  concatenate wg_cadger-dt_lct+6(2) wg_cadger-dt_lct+4(2) wg_cadger-dt_lct(4) into lc_datat.

  call function 'CONVERSION_EXIT_INVDT_INPUT'
    exporting
      input  = lc_datat
    importing
      output = lc_gdatu.


  select single *
      from tcurr
      into wl_tcurr_usd
      where kurst eq 'B'
            and fcurr eq 'USD'
            and tcurr eq 'CHF'
            and gdatu eq lc_gdatu.

  if wl_tcurr_usd is initial.
    clear:lc_lastdate.
    select min( gdatu ) into lc_lastdate from tcurr
        where kurst eq 'B'
              and fcurr eq 'USD'
              and tcurr eq 'CHF'.

    select single *
        from tcurr
        into wl_tcurr_usd
        where kurst eq 'B'
              and fcurr eq 'USD'
              and tcurr eq 'CHF'
              and gdatu eq lc_lastdate.
  endif.

  select single *
    from tcurr
    into wl_tcurr_gbp
    where kurst eq 'B'
          and fcurr eq 'GBP'
          and tcurr eq 'CHF'
          and gdatu eq lc_gdatu.

  if wl_tcurr_gbp is initial.
    clear: lc_lastdate.
    select min( gdatu ) into lc_lastdate from tcurr
       where kurst eq 'B'
            and fcurr eq 'GBP'
            and tcurr eq 'CHF'.

    select single *
      from tcurr
      into wl_tcurr_gbp
      where kurst eq 'B'
            and fcurr eq 'GBP'
            and tcurr eq 'CHF'
            and gdatu eq  lc_lastdate.

  endif.


  loop at tg_criaadt into wg_criaadt.

    add 1 to vseqitem.
    concatenate 'ZFI17' vnum2  wg_cadger-dt_lct+0(4) into wa_zib_contabil-obj_key.
    wg_cadger-icon      =  icon_message_warning .
    wg_cadger-obj_key   =  wa_zib_contabil-obj_key.
    wa_zib_contabil-seqitem   = vseqitem.
    wa_zib_contabil-bschl     = wg_criaadt-bschl.
    wa_zib_contabil-gsber     = wg_criaadt-gsber.
    wa_zib_contabil-bukrs     = p_bukrs.
    wa_zib_contabil-interface	=	''.
    wa_zib_contabil-bldat     = vdata.
    wa_zib_contabil-budat     = vdata.
    wa_zib_contabil-gjahr	    =	 wg_cadger-dt_lct+0(4).
    wa_zib_contabil-monat     = wg_cadger-dt_lct+4(2).
    wa_zib_contabil-blart     = 'SA'.
    wa_zib_contabil-xblnr     = wa_zfit0043-ds_operacao.
    wa_zib_contabil-hkont     = wg_criaadt-saknrz.
    wa_zib_contabil-kostl     = wg_criaadt-kostl.
    wa_zib_contabil-wrbtr     = wg_criaadt-wrbtr.
    wa_zib_contabil-waers     = wg_cadger-waers.
    wa_zib_contabil-sgtxt     = wg_cadger-observacao.
    wa_zib_contabil-umskz     = wg_criaadt-umskz.
    wa_zib_contabil-rg_atualizado	=	'N'.

    wa_zib_contabil-waers_i = 'CHF'.
    wa_zib_contabil-waers_f = 'USD'.
    wa_zib_contabil-dmbtr   = wa_zib_contabil-wrbtr  * wl_tcurr_gbp-ukurs.
    wa_zib_contabil-dmbe2   = wa_zib_contabil-dmbtr / wl_tcurr_usd-ukurs.


    insert into  zib_contabil values wa_zib_contabil.
    if sy-subrc ne 0.
      rollback work.
      clear vobj_key.
    else.
      commit work.
    endif.
    clear  wa_zib_contabil.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_POST_ADTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form f_bapi_f51 changing wa_saida type ty_saida
                         p_belnr
                         p_erro.

  data: l_auglv   type t041a-auglv   value 'UMBUCHNG', "Posting with Clearing
        l_tcode   type sy-tcode      value 'FB05',     "You get an error with any other value
        l_sgfunct type rfipi-sgfunct value 'C'.        "Post immediately

  data: lt_blntab  type standard table of blntab  with header line,
        lt_ftclear type standard table of ftclear with header line,
        lt_ftpost  type standard table of ftpost  with header line,
        lt_fttax   type standard table of fttax   with header line,
        lds_return type bapiret2.

  data: lc_datat     type char08,
        lc_gdatu     type scurr-gdatu,
        lc_lastdate  type scurr-gdatu,
        wl_tcurr_usd type tcurr.

  data: vdata(10),
        vdata_venc(10),
        count_ft       type ftpost-count,
        wl_taxa(16),
        v_kursf_banco  type bkpf-kursf,
        v_xsimu        type char1,
        msg_no         type t100-msgnr,
        msg_text       type string,
        p_mode         like rfpdo-allgazmd,
        vl_saldo       type bsik-wrbtr,
        wl_vlrn        type p decimals 2,
        wl_vlrc(16),
        vxblnr         type bkpf-xblnr.

  if wa_saida-moeda_pgto = 'USD' and p_bukrs ne '0200'.
    concatenate wa_saida-dt_pgto+6(2) wa_saida-dt_pgto+4(2) wa_saida-dt_pgto(4) into lc_datat.

    call function 'CONVERSION_EXIT_INVDT_INPUT'
      exporting
        input  = lc_datat
      importing
        output = lc_gdatu.

    select single *
        from tcurr
        into wl_tcurr_usd
        where kurst eq 'B'
              and fcurr eq 'USD'
              and tcurr eq 'BRL'
              and gdatu eq lc_gdatu.
    "
    if sy-subrc ne 0.
      message 'Ainda não existe taxa de cambio (USD/BRL) para esta data!' type 'I'.
      p_erro = 'X'.
      exit.
    endif.

  endif.

  clear: wa_bsik_aux, tg_bsik_aux.
  if p_belnr is not initial and p_belnr ne '9999999999'.
    wa_saida-belnr = p_belnr.
    wa_saida-buzei = 1.
  endif.
  select single *
     from bsik
     into wa_bsik_aux
     where bukrs eq wa_saida-bukrs
     and   belnr eq wa_saida-belnr
     and   buzei eq wa_saida-buzei.

  select single *
     from bseg
     into wa_bseg_aux
     where bukrs eq wa_saida-bukrs
     and   belnr eq wa_saida-belnr
     and   buzei eq wa_saida-buzei.

  select single *
    from bkpf
    into corresponding fields of wa_bkpf
    where bukrs eq wa_bseg_aux-bukrs
    and   belnr eq wa_bseg_aux-belnr
    and   gjahr eq wa_bseg_aux-gjahr.

  clear: wa_t012k_aux.
  select single *
    from t012k
    into wa_t012k_aux
    where bukrs eq wa_saida-bukrs
      and  hbkid eq wg_cadliq-hbkid.

  vl_saldo = wa_saida-dmbe2 - wa_saida-vlr_pgto.

  p_mode = 'N'.

  call function 'POSTING_INTERFACE_START'
    exporting
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    exceptions
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      others             = 6.

  if sy-subrc ne 0.
    wl_erro = 'X'.
    rollback work.
    message 'Houve ao efeturar a compensação' type 'S'.
    return.
  endif.

  clear: lt_blntab, lt_blntab[],
       lt_ftclear,  lt_ftclear[],
       lt_ftpost,   lt_ftpost[],
       lt_fttax,    lt_fttax[],

       lds_return.
  if p_belnr is not initial.
    if wa_saida-opera = '08'.
      vxblnr =  wa_saida-referencia.
    else.
      concatenate 'INV-' wa_saida-invoice into vxblnr.
    endif.
  else.
    vxblnr = wa_bkpf-xblnr.
  endif.

  concatenate  wa_saida-dt_pgto+6(2) wa_saida-dt_pgto+4(2) wa_saida-dt_pgto(4) into vdata separated by '.'.

  count_ft = 1.

  lt_ftpost-stype = 'K'.        "Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = wa_bsik_aux-bukrs.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = wa_bsik_aux-waers.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  wa_saida-dt_pgto+4(2).
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-XBLNR'.
  lt_ftpost-fval =  vxblnr.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  if p_belnr is initial.
    lt_ftpost-fval = wa_bkpf-blart.
  else.
    lt_ftpost-fval = 'KZ'.
  endif.
  append lt_ftpost.


  if p_belnr is initial. "Split
    do 2 times.
      if sy-index = 1.
        wl_vlrn = conv #( wa_saida-vlr_pgto ).
      else.
        wl_vlrn = conv #( vl_saldo ).
      endif.

      write: wl_vlrn to wl_vlrc.

      add 1 to count_ft.
      lt_ftpost-stype = 'P'.
      lt_ftpost-count = count_ft .

      lt_ftpost-fnam = 'RF05A-NEWBS'.
      lt_ftpost-fval =  wa_bseg_aux-bschl. "'31'.
      append lt_ftpost.

      if wa_bseg_aux-bschl = '29'.
        lt_ftpost-fnam = 'RF05A-NEWUM'.
        lt_ftpost-fval = wa_bseg_aux-umskz.
        append lt_ftpost.
      endif.

      lt_ftpost-fnam = 'BSEG-HKONT'.
      lt_ftpost-fval =  wa_bseg_aux-lifnr.
      append lt_ftpost.

      lt_ftpost-fnam = 'BSEG-SGTXT'.
      lt_ftpost-fval = wa_bseg_aux-sgtxt. "'Split fatura'.
      append lt_ftpost.

      lt_ftpost-fnam = 'BSEG-ZUONR'.
      lt_ftpost-fval = wa_bseg_aux-zuonr.
      append lt_ftpost.

      if wa_bseg_aux-bschl = '31'.
        lt_ftpost-fnam = 'BSEG-KIDNO'.
        lt_ftpost-fval = wa_bseg_aux-kidno.
        append lt_ftpost.
      endif.

      lt_ftpost-fnam = 'BSEG-GSBER'.
      lt_ftpost-fval = wa_bseg_aux-gsber.
      append lt_ftpost.

      concatenate  wa_bseg_aux-zfbdt+6(2) wa_bseg_aux-zfbdt+4(2) wa_bseg_aux-zfbdt(4) into vdata_venc separated by '.'.
      lt_ftpost-fnam = 'BSEG-ZFBDT'.
      lt_ftpost-fval = vdata_venc.
      append lt_ftpost.

      if wa_bseg_aux-bschl ne '29'.
        lt_ftpost-fnam = 'BSEG-ZBD1T'.
        lt_ftpost-fval = wa_bseg_aux-zbd1t.
        condense lt_ftpost-fval no-gaps.
        append lt_ftpost.
      endif.

      lt_ftpost-fnam = 'BSEG-WRBTR'.
      lt_ftpost-fval =  wl_vlrc.
      append lt_ftpost.
    enddo.
  else. "Banco
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'RF05A-NEWBS'.

    if wa_bsik_aux-umsks is not initial.
      lt_ftpost-fval =  '40'.
    else.
      lt_ftpost-fval =  '50'.
    endif.

    append lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval = wa_t012k_aux-hkont.
    append lt_ftpost.

    wl_vlrn = abs( wa_bsik_aux-wrbtr ).

    write: wl_vlrn to wl_vlrc.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    append lt_ftpost.

    lt_ftpost-fnam = 'COBL-GSBER'.
    lt_ftpost-fval = wa_bsik_aux-gsber.
    append lt_ftpost.
  endif.

*******************************************
  lt_ftclear-agkoa  = 'K'.
  lt_ftclear-agkon  = wa_bsik_aux-lifnr.
  lt_ftclear-agums  = wa_bsik_aux-umskz.
  lt_ftclear-agbuk  = wa_bsik_aux-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  concatenate wa_bsik_aux-belnr wa_bsik_aux-budat(4) wa_bsik_aux-buzei into lt_ftclear-selvon.
  append lt_ftclear.

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
      i_xsimu                    = v_xsimu
    importing
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    tables
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    exceptions
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      others                     = 10.

  if lt_blntab[] is initial.
    p_erro = 'X'.
    write lds_return-number to msg_no.
    call function 'MESSAGE_PREPARE'
      exporting
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      importing
        msg_text               = msg_text
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.

    message msg_text type 'I'.
    clear : p_erro, p_belnr.
  else.
    "busca dados originais do pagamento para copiar nos 2 lancamentos de saldo e pagamentoi
    select single *
    from zfit0036
    into wa_zfit0036_ins
    where obj_key = wa_saida-obj_key
    and   belnr   = wa_saida-belnr36
    and   buzei   = wa_saida-buzei36.
    "

    read table lt_blntab index 1.
    if  p_belnr is initial.
      update zfit0036 set status = 'C' "compernsado o original
                          usuario    = sy-uname
                          data_atual = sy-datum
                          hora_atual = sy-uzeit
                   where obj_key = wa_saida-obj_key
                   and   belnr   = wa_saida-belnr36
                   and   buzei   = wa_saida-buzei36.
      wg_documento     = lt_blntab-belnr.
      wa_saida-belnr36 = lt_blntab-belnr.
      wa_saida-buzei36 = 1.


      p_belnr = lt_blntab-belnr.
      "valor a ser pago
      wa_zfit0036_ins-belnr       = p_belnr.
      wa_zfit0036_ins-buzei       = 1.
      wa_zfit0036_ins-usuario     = sy-uname.
      wa_zfit0036_ins-data_atual  = sy-datum.
      wa_zfit0036_ins-hora_atual  = sy-uzeit.
      wa_zfit0036_ins-user_create = sy-uname.
      insert into  zfit0036 values wa_zfit0036_ins.
      if sy-subrc ne 0.
        rollback work.
      else.
        commit work.
      endif.
      "valor saldo
      wa_zfit0036_ins-belnr   = p_belnr.
      wa_zfit0036_ins-buzei = 2.
      clear: wa_zfit0036_ins-lote,
             wa_zfit0036_ins-dt_pgto,
             wa_zfit0036_ins-moeda_pgto,
             wa_zfit0036_ins-tx_cambio,
             wa_zfit0036_ins-observacao,
             wa_zfit0036_ins-referencia,
             wa_zfit0036_ins-operacao,
             wa_zfit0036_ins-desp_tar,
             wa_zfit0036_ins-status,
             wa_zfit0036_ins-vlr_pgto,
             wa_zfit0036_ins-forma_pg,
             wa_zfit0036_ins-motivo,
             wa_zfit0036_ins-bvtyp,
             wa_zfit0036_ins-lifnr,
             wa_zfit0036_ins-bvtyp,
             wa_zfit0036_ins-swift_1,
             wa_zfit0036_ins-banks_1,
             wa_zfit0036_ins-bankl_1,
             wa_zfit0036_ins-banka_1,
             wa_zfit0036_ins-bankn_1,
             wa_zfit0036_ins-iban_1,
             wa_zfit0036_ins-bvtyp_2,
             wa_zfit0036_ins-swift_2,
             wa_zfit0036_ins-banks_2,
             wa_zfit0036_ins-bankl_2,
             wa_zfit0036_ins-banka_2,
             wa_zfit0036_ins-bankn_2,
             wa_zfit0036_ins-iban_2.
      insert into  zfit0036 values wa_zfit0036_ins.
      if sy-subrc ne 0.
        rollback work.
      else.
        commit work.
      endif.
    endif.
  endif.
endform.

form f_post_adto using wa_saida type ty_saida changing wl_erro.

  data: l_auglv   type t041a-auglv   value 'UMBUCHNG', "Posting with Clearing
        l_tcode   type sy-tcode      value 'FB05',     "You get an error with any other value
        l_sgfunct type rfipi-sgfunct value 'C'.        "Post immediately

  data: lt_blntab  type standard table of blntab  with header line,
        lt_ftclear type standard table of ftclear with header line,
        lt_ftpost  type standard table of ftpost  with header line,
        lt_fttax   type standard table of fttax   with header line,
        lds_return type bapiret2.


  data: vdata(10),
        vdata_venc(10),
        count_ft       type ftpost-count,
        wl_taxa(16),
        v_kursf_banco  type bkpf-kursf,
        v_xsimu        type char1,
        msg_no         type t100-msgnr,
        msg_text       type string,
        p_mode         like rfpdo-allgazmd,
        wl_vlrn        type p decimals 2,
        wl_vlrc(16).

  clear: wa_bsik_aux, tg_bsik_aux.
  select single *
     from bsik
     into wa_bsik_aux
     where bukrs eq wa_saida-bukrs
     and   belnr eq wa_saida-obj_key+1(10).

  clear: wa_t012k_aux.
  select single *
    from t012k
    into wa_t012k_aux
    where bukrs eq wa_saida-bukrs
      and  hbkid eq wg_cadliq-hbkid.

  p_mode = 'N'.

  call function 'POSTING_INTERFACE_START'
    exporting
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    exceptions
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      others             = 6.

  if sy-subrc ne 0.
    wl_erro = 'X'.
    rollback work.
    message 'Houve ao efeturar a compensação' type 'S'.
    return.
  endif.

  clear: lt_blntab, lt_blntab[],
       lt_ftclear,  lt_ftclear[],
       lt_ftpost,   lt_ftpost[],
       lt_fttax,    lt_fttax[],
       lds_return.

  concatenate  wa_saida-dt_pgto+6(2) wa_saida-dt_pgto+4(2) wa_saida-dt_pgto(4) into vdata separated by '.'.

  count_ft = 1.

  lt_ftpost-stype = 'K'.        "Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = wa_bsik_aux-bukrs.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = wa_bsik_aux-waers.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  wa_saida-dt_pgto+4(2).
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  lt_ftpost-fval = 'KZ'.
  append lt_ftpost.
***************************************

  lt_ftpost-stype = 'P'.
  lt_ftpost-count = count_ft .

  lt_ftpost-fnam = 'RF05A-NEWBS'.
                                                            "IR134493
  if wa_bsik_aux-umsks is not initial.
    lt_ftpost-fval =  '40'.
  else.
    lt_ftpost-fval =  '50'.
  endif.
                                                            "IR134493

  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-HKONT'.
  lt_ftpost-fval = wa_t012k_aux-hkont.
  append lt_ftpost.

  wl_vlrn = abs( wa_bsik_aux-wrbtr ).

  write: wl_vlrn to wl_vlrc.

  lt_ftpost-fnam = 'BSEG-WRBTR'.
  lt_ftpost-fval =  wl_vlrc.
  append lt_ftpost.

  lt_ftpost-fnam = 'COBL-GSBER'.
  lt_ftpost-fval = wa_bsik_aux-gsber.
  append lt_ftpost.

*******************************************
  lt_ftclear-agkoa  = 'K'.
  lt_ftclear-agkon  = wa_bsik_aux-lifnr.
  lt_ftclear-agums  = wa_bsik_aux-umskz.
  lt_ftclear-agbuk  = wa_bsik_aux-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  concatenate wa_bsik_aux-belnr wa_bsik_aux-budat(4) wa_bsik_aux-buzei into lt_ftclear-selvon.
  append lt_ftclear.

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
      i_xsimu                    = v_xsimu
    importing
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    tables
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    exceptions
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      others                     = 10.

  if lt_blntab[] is initial.
    write lds_return-number to msg_no.
    call function 'MESSAGE_PREPARE'
      exporting
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      importing
        msg_text               = msg_text
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.

    message msg_text type 'I'.
    wl_erro = 'X'.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_FERIADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_CADGER_DT_LCT  text
*----------------------------------------------------------------------*
form verifica_feriado  using  p_dt_lct p_moeda p_pais p_tipo.

  data: lit_zfit0178 type table of zfit0178,
        lwa_zfit0178 like line of lit_zfit0178,
        wl_zfit0177  type zfit0177,
        lva_x        type i.


  if p_tipo = '1000'.
    loop at tg_conta into wg_conta.

      select *
        from zfit0178
      into table lit_zfit0178
        where data_feriado = p_dt_lct.

      if sy-subrc = 0.

        loop at lit_zfit0178 into lwa_zfit0178.

          if lwa_zfit0178-pais  eq 'LU' and lwa_zfit0178-pais ne wg_conta-banks .
            data(lv_luxe) = 'X'.
          endif.

          select single *
             from zfit0177
            into wl_zfit0177
             where pais = lwa_zfit0178-pais.

          if sy-subrc = 0 and lv_luxe ne 'X' .

            find first occurrence of p_moeda in wl_zfit0177-moeda match count lva_x .
            if sy-subrc eq 0.
              select single *
               from t005t into @data(w_t005)
              where land1 = @lwa_zfit0178-pais .

              clear: wmensagem.
              concatenate 'Na data do Lançamento é feriado'(360) '(' w_t005-landx ')' into wmensagem  separated by space.
              message wmensagem type 'I'.
              wl_erro = 'X'.
            endif.

          endif.
          clear: lv_luxe.
        endloop.
      endif.
    endloop.
  else.
    if p_tipo = '4000' or  p_tipo = '0100' or  p_tipo = '2000' .

      clear: lit_zfit0178.


      select *
       from zfit0178
       into table lit_zfit0178
       where data_feriado = p_dt_lct.

      if sy-subrc = 0.

        loop at lit_zfit0178 into lwa_zfit0178.

          if lwa_zfit0178-pais  eq 'LU' and lwa_zfit0178-pais ne wg_conta-banks .
            lv_luxe = 'X'.
          endif.

          select single *
             from zfit0177
            into wl_zfit0177
             where pais = lwa_zfit0178-pais.

          if sy-subrc = 0 and lv_luxe ne 'X' ..

            find first occurrence of p_moeda in wl_zfit0177-moeda match count lva_x .
            if sy-subrc eq 0.

              select single *
               from t005t into w_t005
              where land1 = lwa_zfit0178-pais
              and spras eq 'P'.
              clear: wmensagem.
              concatenate 'Na data do Lançamento é feriado'(360) '(' w_t005-landx ')' into wmensagem  separated by space.
              message wmensagem type 'I'.
              wl_erro = 'X'.
            endif.

          endif.
          clear: lv_luxe.
        endloop.
      endif.
    endif.
  endif.
  clear: w_t005.
endform.

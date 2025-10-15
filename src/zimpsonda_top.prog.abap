*&---------------------------------------------------------------------*
*& Include          ZIMPSONDA_TOP
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_zib_contabil_chv,
    obj_key TYPE zib_contabil_chv-obj_key,
    belnr   TYPE zib_contabil_chv-belnr,
    bukrs   TYPE zib_contabil_chv-bukrs,
    gjahr   TYPE zib_contabil_chv-gjahr,
  END OF ty_zib_contabil_chv,

  BEGIN OF ty_zimp_lanc_impost,
    doc_imposto  TYPE  zimp_lanc_impost-doc_imposto,
    bukrs        TYPE  zimp_lanc_impost-bukrs,
    gsber        TYPE  zimp_lanc_impost-gsber,
    lote         TYPE  zimp_lanc_impost-lote,
    dt_venc      TYPE  zimp_lanc_impost-dt_venc,
    dt_apuracao  TYPE  zimp_lanc_impost-dt_apuracao,
    mes_apuracao TYPE  zimp_lanc_impost-mes_apuracao,
    ano_apuracao TYPE  zimp_lanc_impost-ano_apuracao,
    observacao   TYPE  zimp_lanc_impost-observacao,
    cod_imposto  TYPE  zimp_lanc_impost-cod_imposto,
    ref_imposto  TYPE  zimp_lanc_impost-ref_imposto,
    tp_imposto   TYPE  zimp_lanc_impost-tp_imposto,
    cod_pgto     TYPE  zimp_lanc_impost-cod_pgto,
    conv_banco   TYPE  zimp_lanc_impost-conv_banco,
    hbkid        TYPE  zimp_lanc_impost-hbkid,
    data_atual   TYPE  zimp_lanc_impost-data_atual,
    hora_atual   TYPE  zimp_lanc_impost-hora_atual,
    usuario      TYPE  zimp_lanc_impost-usuario,
    loekz        TYPE  zimp_lanc_impost-loekz,
    cod_barras   TYPE  zimp_lanc_impost-cod_barras,
    doc_contabil TYPE  zib_contabil_chv-belnr, " contabil
    gjahr        TYPE  zib_contabil_chv-gjahr,
    budat        TYPE  bkpf-budat,
    augbl        TYPE  bsak-augbl,
  END OF ty_zimp_lanc_impost,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
    budat TYPE bkpf-budat,
  END OF ty_bkpf,

  BEGIN OF ty_bsak,
    bukrs TYPE bsak-bukrs,
    belnr TYPE bsak-belnr,
    gjahr TYPE bsak-gjahr,
    augbl TYPE bsak-augbl,
  END OF ty_bsak.



DATA: it_ZIMP_LANC_SONDA      TYPE TABLE OF zimp_lanc_sonda.
DATA: it_zimp_guia_sonda      TYPE TABLE OF zimp_guia_sonda.
DATA: it_zimp_imp             TYPE TABLE OF zimp_cad_imposto.

DATA: it_zimp_lanc_impost TYPE TABLE OF ty_zimp_lanc_impost,
      it_zimp_lanc_imp_ct TYPE TABLE OF zimp_lanc_imp_ct,
      it_zimp_cad_lote    TYPE TABLE OF zimp_cad_lote.


DATA: wa_zimp_cad_lote    TYPE zimp_cad_lote,
      wa_zimp_lanc_impost TYPE ty_zimp_lanc_impost.

DATA: wa_zimp_lanc_sonda TYPE zimp_lanc_sonda.
DATA: wa_zimp_guia_sonda TYPE zimp_guia_sonda.

DATA: wa_zib_contabil_chv TYPE ty_zib_contabil_chv.

DATA: wa_bkpf TYPE ty_bkpf,
      wa_bsak TYPE ty_bsak.

DATA: it_zimpcodimp   TYPE TABLE OF zimpcodimp.
DATA: it_zimpcodobrig TYPE TABLE OF zimpcodobrig.
DATA: it_zimptpajuste TYPE TABLE OF zimptpajuste.


DATA: vobj_key TYPE zib_contabil_err-obj_key.
DATA: go_alv   TYPE REF TO cl_salv_table.

DATA: v1 TYPE c LENGTH 50,
      v2 TYPE c LENGTH 50.
DATA: lv_ref TYPE dats.

CONSTANTS:

c_s TYPE c VALUE 'S'.

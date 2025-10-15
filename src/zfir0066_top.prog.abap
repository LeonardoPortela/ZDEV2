*&---------------------------------------------------------------------*
*&  Include           ZFIR0066_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_bukrs_prev,
         bukrs      TYPE zfit0094-bukrs,
         bukrs_prev TYPE zfit0094-bukrs,
       END   OF ty_bukrs_prev.

DATA: BEGIN OF tg_bsik OCCURS 0,
        bukrs TYPE bsik-bukrs,
        lifnr TYPE bsik-lifnr,
        umsks TYPE bsik-umsks,
        umskz TYPE bsik-umskz,
        augdt TYPE bsik-augdt,
        augbl TYPE bsik-augbl,
        zuonr TYPE bsik-zuonr,
        gjahr TYPE bsik-gjahr,
        belnr TYPE bsik-belnr,
        buzei TYPE bsik-buzei,
        budat TYPE bsik-budat,
        bldat TYPE bsik-bldat,
        waers TYPE bsik-waers,
        xblnr TYPE bsik-xblnr,
        blart TYPE bsik-blart,
        gsber TYPE bsik-gsber,
        ebeln TYPE bsik-ebeln,
        ebelp TYPE bsik-ebelp,
        bschl TYPE bsik-bschl,
        shkzg TYPE bsik-shkzg,
        zlsch TYPE bsik-zlsch,
        zlspr TYPE bsik-zlspr,
        hkont TYPE bsik-hkont,
        sgtxt TYPE bsik-sgtxt,
        hbkid TYPE bsik-hbkid,
        zfbdt TYPE bsik-zfbdt,
        zbd1t TYPE bsik-zbd1t,
        dmbtr TYPE bsik-dmbtr,
        dmbe2 TYPE bsik-dmbe2,
        wrbtr TYPE bsik-wrbtr,
        vbund TYPE bsik-vbund, "*-CS2022000133-#74201-05.05.2022-JT-inicio
        venci TYPE bsik-zfbdt, "Calculado
        xvlr  TYPE bsik-dmbtr, "Calculado
        xvlu  TYPE bsik-dmbe2, "Calculado
      END OF tg_bsik,

      BEGIN OF tg_bsik_for_bsid OCCURS 0,
        bukrs        TYPE bsik-bukrs,
        bukrs_p      TYPE bsik-bukrs,
        lifnr        TYPE bsik-lifnr,
        umsks        TYPE bsik-umsks,
        umskz        TYPE bsik-umskz,
        augdt        TYPE bsik-augdt,
        augbl        TYPE bsik-augbl,
        zuonr        TYPE bsik-zuonr,
        gjahr        TYPE bsik-gjahr,
        belnr        TYPE bsik-belnr,
        buzei        TYPE bsik-buzei,
        budat        TYPE bsik-budat,
        bldat        TYPE bsik-bldat,
        waers        TYPE bsik-waers,
        xblnr        TYPE bsik-xblnr,
        blart        TYPE bsik-blart,
        gsber        TYPE bsik-gsber,
        ebeln        TYPE bsik-ebeln,
        ebelp        TYPE bsik-ebelp,
        bschl        TYPE bsik-bschl,
        shkzg        TYPE bsik-shkzg,
        zlsch        TYPE bsik-zlsch,
        zlspr        TYPE bsik-zlspr,
        hkont        TYPE bsik-hkont,
        sgtxt        TYPE bsik-sgtxt,
        hbkid        TYPE bsik-hbkid,
        zfbdt        TYPE bsik-zfbdt,
        zbd1t        TYPE bsik-zbd1t,
        dmbtr        TYPE bsik-dmbtr,
        dmbe2        TYPE bsik-dmbe2,
        wrbtr        TYPE bsik-wrbtr,
        vbund        TYPE bsik-vbund, "*-CS2022000133-#74201-05.05.2022-JT-inicio
        venci        TYPE bsik-zfbdt, "Calculado
        xvlr         TYPE bsik-dmbtr, "Calculado
        xvlu         TYPE bsik-dmbe2, "Calculado
        processo_esp TYPE zfit0109-processo_esp,
        sistema_orig TYPE zfit0109-sistema_orig,
        certo(1)     TYPE c,
        bsart        TYPE ekko-bsart,
      END OF tg_bsik_for_bsid,

      BEGIN OF tg_bsak OCCURS 0,
        bukrs TYPE bsak-bukrs,
        lifnr TYPE bsak-lifnr,
        umsks TYPE bsak-umsks,
        umskz TYPE bsak-umskz,
        augdt TYPE bsak-augdt,
        augbl TYPE bsak-augbl,
        zuonr TYPE bsak-zuonr,
        gjahr TYPE bsak-gjahr,
        belnr TYPE bsak-belnr,
        buzei TYPE bsak-buzei,
        budat TYPE bsak-budat,
        bldat TYPE bsak-bldat,
        waers TYPE bsak-waers,
        xblnr TYPE bsak-xblnr,
        blart TYPE bsak-blart,
        gsber TYPE bsak-gsber,
        ebeln TYPE bsak-ebeln,
        ebelp TYPE bsak-ebelp,
        bschl TYPE bsak-bschl,
        shkzg TYPE bsak-shkzg,
        zlsch TYPE bsak-zlsch,
        zlspr TYPE bsak-zlspr,
        hkont TYPE bsak-hkont,
        sgtxt TYPE bsak-sgtxt,
        hbkid TYPE bsak-hbkid,
        zfbdt TYPE bsak-zfbdt,
        zbd1t TYPE bsak-zbd1t,
        dmbtr TYPE bsak-dmbtr,
        dmbe2 TYPE bsak-dmbe2,
        wrbtr TYPE bsak-wrbtr,
        vbund TYPE bsak-vbund, "*-CS2022000133-#74201-05.05.2022-JT-inicio
        venci TYPE bsak-zfbdt, "Calculado
        xvlr  TYPE bsak-dmbtr, "Calculado
        xvlu  TYPE bsak-dmbe2, "Calculado
      END OF tg_bsak,

      BEGIN OF tg_vbsegk OCCURS 0,
        bukrs          TYPE vbsegk-bukrs,
        lifnr          TYPE vbsegk-lifnr,
        umsks          TYPE vbsegk-umsks,
        umskz          TYPE vbsegk-umskz,
        zuonr          TYPE vbsegk-zuonr,
        gjahr          TYPE vbsegk-gjahr,
        belnr          TYPE vbsegk-belnr,
        buzei          TYPE vbsegk-buzei,
        swaer          TYPE vbsegk-swaer,
        gsber          TYPE vbsegk-gsber,
        bschl          TYPE vbsegk-bschl,
        shkzg          TYPE vbsegk-shkzg,
        zlsch          TYPE vbsegk-zlsch,
        zlspr          TYPE vbsegk-zlspr,
        hkont          TYPE vbsegk-hkont,
        sgtxt          TYPE vbsegk-sgtxt,
        hbkid          TYPE vbsegk-hbkid,
        zfbdt          TYPE vbsegk-zfbdt,
        zbd1t          TYPE vbsegk-zbd1t,
        dmbtr          TYPE vbsegk-dmbtr,
        dmbe2          TYPE vbsegk-dmbe2,
        wrbtr          TYPE vbsegk-wrbtr,
        venci          TYPE vbsegk-zfbdt, "Calculado
        xvlr           TYPE vbsegk-dmbtr, "Calculado
        xvlu           TYPE vbsegk-dmbe2, "Calculado
        processo_esp   TYPE zfit0109-processo_esp,
        processo_esp02 TYPE zfit0109-processo_esp,
        sistema_orig   TYPE zfit0109-sistema_orig,
      END OF tg_vbsegk,

      BEGIN OF tg_bsid OCCURS 0,
        bukrs TYPE bsid-bukrs,
        kunnr TYPE bsid-kunnr,
        umsks TYPE bsid-umsks,
        umskz TYPE bsid-umskz,
        zuonr TYPE bsid-zuonr,
        gjahr TYPE bsid-gjahr,
        belnr TYPE bsid-belnr,
        buzei TYPE bsid-buzei,
        budat TYPE bsid-budat,
        bldat TYPE bsid-bldat,
        waers TYPE bsid-waers,
        xblnr TYPE bsid-xblnr,
        blart TYPE bsid-blart,
        gsber TYPE bsid-gsber,
        vbel2 TYPE bsid-vbel2,
        vpos2 TYPE bsid-vpos2,
        hkont TYPE bsid-hkont,
        sgtxt TYPE bsid-sgtxt,
        hbkid TYPE bsid-hbkid,
        bschl TYPE bsid-bschl,
        shkzg TYPE bsid-shkzg,
        zlsch TYPE bsid-zlsch,
        zlspr TYPE bsid-zlspr,
        zfbdt TYPE bsid-zfbdt,
        zbd1t TYPE bsid-zbd1t,
        dmbtr TYPE bsid-dmbtr,
        dmbe2 TYPE bsid-dmbe2,
        wrbtr TYPE bsid-wrbtr,
        vbund TYPE bsid-vbund, "*-CS2022000133-#74201-05.05.2022-JT-inicio
        venci TYPE bsid-zfbdt, "Calculado
        xvlr  TYPE bsid-dmbtr, "Calculado
        xvlu  TYPE bsid-dmbe2, "Calculado
      END OF tg_bsid,

*      BEGIN OF TG_0109 OCCURS 0,
*        CODIGO       TYPE ZFIT0109-CODIGO,
*        CLAS_FLX     TYPE ZFIT0109-CLAS_FLX,
*        SEQ          TYPE ZFIT0109-SEQ,
*        DESCRICAO    TYPE ZFIT0109-DESCRICAO,
*        TIPO_DOC     TYPE ZFIT0109-TIPO_DOC,
*        TIPO_PED     TYPE ZFIT0109-TIPO_PED,
*        TIPO_OV      TYPE ZFIT0109-TIPO_OV,
*        BLOQ_PGTO    TYPE ZFIT0109-BLOQ_PGTO,
*        FORMA_PGTO   TYPE ZFIT0109-FORMA_PGTO,
*        BCO_EMPRESA  TYPE ZFIT0109-BCO_EMPRESA,
*        PROCESSO_ESP TYPE ZFIT0109-PROCESSO_ESP,
*        OCULTAR      TYPE ZFIT0109-OCULTAR,
*        ST_CALC_SDO  TYPE ZFIT0109-ST_CALC_SDO,
*        TP_PREV      TYPE ZFIT0109-TP_PREV,
*      END OF TG_0109,

      BEGIN OF tg_0079_resumo OCCURS 0,
        bukrs    TYPE zfit0079-bukrs,
        codigo   TYPE zfit0079-codigo,
        clas_flx TYPE zfit0079-clas_flx,
        seq      TYPE zfit0079-seq,
        zfbdt    TYPE zfit0079-zfbdt,
        dmbtr    TYPE zfit0079-dmbtr,
        dmbe2    TYPE zfit0079-dmbe2,
      END OF tg_0079_resumo,


      BEGIN OF tg_bkpf OCCURS 0,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        usnam TYPE bkpf-usnam,
      END OF tg_bkpf,

      BEGIN OF tg_ekko OCCURS 0,
        ebeln TYPE ekko-ebeln,
        bsart TYPE ekko-bsart,
      END OF tg_ekko,

      BEGIN OF tg_ekko_aux OCCURS 0,
        ebeln TYPE ekko-ebeln,
        bsart TYPE ekko-bsart,
      END OF tg_ekko_aux,

      BEGIN OF tg_ekpo OCCURS 0,
        ebeln TYPE ekpo-ebeln,
        matnr TYPE ekpo-matnr,
        matkl TYPE ekpo-matkl,
      END OF tg_ekpo,

      BEGIN OF tg_vbak OCCURS 0,
        vbeln TYPE vbak-vbeln,
        auart TYPE vbak-auart,
      END OF tg_vbak,

      BEGIN OF tg_zimp_cad_lote OCCURS 0,
        status_lote TYPE zimp_cad_lote-status_lote,
        lote        TYPE zimp_cad_lote-lote,
        bukrs       TYPE zimp_cad_lote-bukrs,
      END OF tg_zimp_cad_lote,

      BEGIN OF tg_zimp_lanc_impost OCCURS 0,
        lote        TYPE zimp_lanc_impost-lote,
        bukrs       TYPE zimp_lanc_impost-bukrs,
        doc_imposto TYPE zimp_lanc_impost-doc_imposto,
        dt_venc     TYPE zimp_lanc_impost-dt_venc,
        waers       TYPE zimp_lanc_impost-waers,
        usuario     TYPE zimp_lanc_impost-usuario,
      END OF tg_zimp_lanc_impost,

      BEGIN OF tg_zimp_lanc_imp_ct OCCURS 0,
        doc_imposto  TYPE zimp_lanc_imp_ct-doc_imposto,
        bukrs        TYPE zimp_lanc_imp_ct-bukrs,
        seqitem      TYPE zimp_lanc_imp_ct-seqitem,
        bschl        TYPE zimp_lanc_imp_ct-bschl,
        lifnr        TYPE zimp_lanc_imp_ct-lifnr,
        valor_imp    TYPE zimp_lanc_imp_ct-valor_imp,
        valor_for    TYPE zimp_lanc_imp_ct-valor_for,
        gsber        TYPE zimp_lanc_imp_ct-gsber,
        xvlr         TYPE bsid-dmbtr, "Calculado
        xvlu         TYPE bsid-dmbe2, "Calculado
        processo_esp TYPE zfit0109-processo_esp,
        sistema_orig TYPE zfit0109-sistema_orig,
      END OF tg_zimp_lanc_imp_ct,

      BEGIN OF tg_0110 OCCURS 0,
        bukrs        TYPE zfit0110-bukrs,
        planilha     TYPE zfit0110-planilha,
        planilha_itm TYPE zfit0110-planilha_itm,
        lifnr        TYPE zfit0110-lifnr,
        waers        TYPE zfit0110-waers,
        dt_vcto      TYPE zfit0110-dt_vcto,
        gsber        TYPE zfit0110-gsber,
        dmbtr        TYPE zfit0110-dmbtr,
        dmbe2        TYPE zfit0110-dmbe2,
        tipo         TYPE zfit0110-tipo,
        id_invoice   TYPE zfit0110-id_invoice,
        ds_porto     TYPE zfit0110-ds_porto,
        kursf        TYPE zfit0110-kursf,
        processo_esp TYPE zfit0109-processo_esp,
        sistema_orig TYPE zfit0109-sistema_orig,
        xvlr         TYPE zfit0110-dmbtr, "Calculado
        xvlu         TYPE zfit0110-dmbe2, "Calculado
      END OF tg_0110,

      BEGIN OF tg_0112 OCCURS 0,
        bukrs           TYPE zfit0112-bukrs,
        opr_numero      TYPE zfit0112-opr_numero,
        con_codigo      TYPE zfit0112-con_codigo,
        data_vencimento TYPE zfit0112-data_vencimento,
        mdo_codigo      TYPE zfit0112-mdo_codigo,
        par_tipo        TYPE zfit0112-par_tipo,
        mdo_tipo        TYPE zfit0112-mdo_tipo,
        dmbtr           TYPE zfit0112-dmbtr,
        dmbe2           TYPE zfit0112-dmbe2,
        waers           TYPE zfit0112-waers,

        bukrs_opr	      TYPE zfit0112-bukrs_opr,
        agente          TYPE zfit0112-agente,
        regra_val	      TYPE zfit0112-regra_val,

        xvlr            TYPE zfit0110-dmbtr, "Calculado
        xvlu            TYPE zfit0110-dmbe2, "Calculado
        processo_esp    TYPE zfit0109-processo_esp,
        processo_esp02  TYPE zfit0109-processo_esp,
        processo_esp03  TYPE zfit0109-processo_esp,
        processo_esp04  TYPE zfit0109-processo_esp,
        sistema_orig    TYPE zfit0109-sistema_orig,
      END OF tg_0112,

      BEGIN OF tg_0112_sld_aplic OCCURS 0,
        bukrs           TYPE zfit0112-bukrs,
        opr_numero      TYPE zfit0112-opr_numero,
        con_codigo      TYPE zfit0112-con_codigo,
        data_vencimento TYPE zfit0112-data_vencimento,
        mdo_codigo      TYPE zfit0112-mdo_codigo,
        par_tipo        TYPE zfit0112-par_tipo,
        mdo_tipo        TYPE zfit0112-mdo_tipo,
        dmbtr           TYPE zfit0112-dmbtr,
        dmbe2           TYPE zfit0112-dmbe2,
        waers           TYPE zfit0112-waers,

        bukrs_opr	      TYPE zfit0112-bukrs_opr,
        agente          TYPE zfit0112-agente,
        regra_val	      TYPE zfit0112-regra_val,

        xvlr            TYPE zfit0110-dmbtr, "Calculado
        xvlu            TYPE zfit0110-dmbe2, "Calculado
        processo_esp    TYPE zfit0109-processo_esp,
        sistema_orig    TYPE zfit0109-sistema_orig,
      END OF tg_0112_sld_aplic,

      BEGIN OF tg_0115 OCCURS 0.
        INCLUDE STRUCTURE zfit0115.
DATA  END OF tg_0115.


TYPES:
  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    land1 TYPE lfa1-land1,
  END OF ty_lfa1.

DATA  BEGIN OF tg_0045 OCCURS 0.
INCLUDE STRUCTURE zfit0045.
DATA END OF tg_0045.

DATA: BEGIN OF tg_zlest0140 OCCURS 0,
        xvlr         TYPE zfit0110-dmbtr,
        xvlu         TYPE zfit0110-dmbe2,
        processo_esp TYPE zfit0109-processo_esp,
        sistema_orig TYPE zfit0109-sistema_orig.
        INCLUDE STRUCTURE zlest0140.
DATA END OF tg_zlest0140.

DATA  BEGIN OF tg_zlest0141 OCCURS 0.
INCLUDE STRUCTURE zlest0141.
DATA END OF tg_zlest0141.

DATA  BEGIN OF tg_curr_inf OCCURS 0.
INCLUDE STRUCTURE zfi_curr_inf_flx.
DATA  END OF tg_curr_inf.

DATA  BEGIN OF tg_0141_sld_fre OCCURS 0.
INCLUDE STRUCTURE zlest0141_resumo.
DATA END OF tg_0141_sld_fre.

DATA:  BEGIN OF tg_0141_outros OCCURS 0,
         cd_adiministra TYPE zlest0141_lote-cd_adiministra,
         bukrs          TYPE zlest0141_lote-bukrs.
         INCLUDE STRUCTURE zlest0141_l_item.
DATA END OF tg_0141_outros.


DATA: BEGIN OF tg_0141_sld_fre_grp OCCURS 0,
        xvlr         TYPE zfit0110-dmbtr,
        xvlu         TYPE zfit0110-dmbe2,
        processo_esp TYPE zfit0109-processo_esp,
        sistema_orig TYPE zfit0109-sistema_orig.
        INCLUDE STRUCTURE zlest0141_resumo.
DATA END OF tg_0141_sld_fre_grp.

DATA: BEGIN OF tg_0141_outros_grp OCCURS 0,
        cd_adiministra TYPE zlest0141_lote-cd_adiministra,
        bukrs          TYPE zlest0141_lote-bukrs,
        xvlr           TYPE zfit0110-dmbtr,
        xvlu           TYPE zfit0110-dmbe2,
        processo_esp   TYPE zfit0109-processo_esp,
        sistema_orig   TYPE zfit0109-sistema_orig.
        INCLUDE STRUCTURE zlest0141_l_item.
DATA END OF tg_0141_outros_grp.

DATA: BEGIN OF tg_0046 OCCURS 0,
        nro_sol          TYPE zfit0046-nro_sol,
        ebeln            TYPE zfit0046-ebeln,
        ebelp            TYPE zfit0046-ebelp,
        vlr_adiantamento TYPE zfit0046-vlr_adiantamento,
        xvlr             TYPE zfit0110-dmbtr, "Calculado
        xvlu             TYPE zfit0110-dmbe2, "Calculado
        processo_esp     TYPE zfit0109-processo_esp,
        sistema_orig     TYPE zfit0109-sistema_orig.
.
DATA END OF tg_0046.

DATA: BEGIN OF tg_0083 OCCURS 0,
        bukrs            TYPE zfit0083-bukrs,
        date_period_1    TYPE zfit0083-date_period_1,
        trade_id         TYPE zfit0083-trade_id,
        amount_dealt     TYPE zfit0083-amount_dealt,
        counter_amount   TYPE zfit0083-counter_amount,
        cont_part_deal_c TYPE zfit0083-cont_part_deal_c,
        exch_rat_period1 TYPE zfit0083-exch_rat_period1,
        currency_1       TYPE zfit0083-currency_1,
        side             TYPE zfit0083-side,
        rev_trade        TYPE zfit0083-rev_trade,
        processo_esp     TYPE zfit0109-processo_esp,
        sistema_orig     TYPE zfit0109-sistema_orig,
        xv1              TYPE dmbtr,
        xv2              TYPE dmbtr,
        xv3              TYPE dmbtr,
        xv4              TYPE dmbtr,
        xv5              TYPE dmbtr,
        xv6              TYPE dmbtr,
      END OF tg_0083,

      BEGIN OF tg_0096 OCCURS 0,
        bukrs            TYPE zfit0096-bukrs,
        date_period_1    TYPE zfit0096-date_period_1,
        trade_id         TYPE zfit0096-trade_id,
        amount_dealt     TYPE zfit0083-amount_dealt,
        counter_amount   TYPE zfit0083-counter_amount,
        cont_part_deal_c TYPE zfit0083-cont_part_deal_c,
        exch_rat_period1 TYPE zfit0083-exch_rat_period1,
        currency_1       TYPE zfit0083-currency_1,
        side             TYPE zfit0083-side,
        rev_trade        TYPE zfit0083-rev_trade,
        tx_ob08          TYPE zfit0096-tx_ob08,
        aj_bruto_brl     TYPE zfit0096-aj_bruto_brl,
        processo_esp     TYPE zfit0109-processo_esp,
        sistema_orig     TYPE zfit0109-sistema_orig,
        xv1              TYPE dmbtr,
        xv2              TYPE dmbtr,
        xv3              TYPE dmbtr,
        xv4              TYPE dmbtr,
        xv5              TYPE dmbtr,
        xv6              TYPE dmbtr,
      END OF tg_0096,

      BEGIN OF tg_0094 OCCURS 0,
        bukrs       TYPE zfit0094-bukrs,
        banco_bloom TYPE zfit0094-banco_bloom,
        kunnr       TYPE zfit0094-kunnr,
        lifnr       TYPE zfit0094-lifnr,
        hkont       TYPE zfit0094-hkont,
        bco_bloom_1 TYPE char04,
      END OF tg_0094.

TYPES:
  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    land1 TYPE kna1-land1,
  END OF ty_kna1,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    land1 TYPE t001-land1,
  END OF ty_t001.

DATA: tg_bukrs TYPE TABLE OF ty_t001 WITH HEADER LINE,
      tg_0079  LIKE zfit0079 OCCURS 0 WITH HEADER LINE,
      tg_0119  LIKE zfit0119 OCCURS 0 WITH HEADER LINE,
      "TG_0079_AUX LIKE ZFIT0079 OCCURS 0 WITH HEADER LINE,
      tg_0109  LIKE zfit0109 OCCURS 0 WITH HEADER LINE,
      tg_0175  LIKE zfit0175 OCCURS 0 WITH HEADER LINE,
      t_lfa1   TYPE TABLE of ty_lfa1,
      t_kna1   TYPE TABLE OF ty_kna1.
"TG_0111     LIKE ZFIT0111 OCCURS 0 WITH HEADER LINE,
"TG_0111_AUX LIKE ZFIT0111 OCCURS 0 WITH HEADER LINE.

*---------------------------------------------------------------------*
* Váriaveis
*---------------------------------------------------------------------*

DATA: vg_tx_usd_brl   TYPE ukurs_curr,
      vg_tx_usd_ars   TYPE ukurs_curr,
      vg_tx_eur_brl   TYPE ukurs_curr,
      vg_tx_eur_usd   TYPE ukurs_curr,
      vg_err_consulta TYPE c,
      c_bukrs         TYPE string,
      error_text      TYPE string,
      vg_error_prc    TYPE string,
      tg_bukrs_prev   TYPE TABLE OF ty_bukrs_prev,
      wa_bukrs_prev   TYPE ty_bukrs_prev.

*---------------------------------------------------------------------*
* Constantes
*---------------------------------------------------------------------*

CONSTANTS: c_ars VALUE 'ARS' TYPE c LENGTH 3,
           c_brl VALUE 'BRL' TYPE c LENGTH 3,
           c_usd VALUE 'USD' TYPE c LENGTH 3.

*---------------------------------------------------------------------*
* Classe
*---------------------------------------------------------------------*

DATA: exc_ref TYPE REF TO cx_sy_native_sql_error.

*---------------------------------------------------------------------*
* Ranges
*---------------------------------------------------------------------*
RANGES:  it_tp_doc  FOR zfit0109-tipo_doc,
         it_tp_ped  FOR zfit0109-tipo_ped,
         it_tp_ov   FOR zfit0109-tipo_ov,
         it_dt_venc FOR zfit0079-zfbdt,
         rg_bukrs_proc FOR t001-bukrs.

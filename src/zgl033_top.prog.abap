TABLES: zglt064,zglt050,zglt068.



TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida,
         nro_apolice        TYPE zglt050-nro_apolice,
         vig_de             TYPE zglt050-vig_de,
         vig_ate            TYPE zglt050-vig_ate,
         seq_lcto           TYPE zglt050-seq_lcto,
         seq_tipo           TYPE zglt050-seq_tipo,
         descr_tipo         TYPE zglt064-descr,
         tp_opr             TYPE zglt050-tp_opr,
         nr_item            TYPE zglt068-nr_item,
         bukrs              TYPE zglt050-bukrs,
         werks              TYPE zglt068-werks,
         anln1              TYPE zglt068-anln1,
         anln2              TYPE zglt068-anln2,
         invnr              TYPE zglt068-invnr,
         sernr              TYPE zglt068-sernr,
         matnr              TYPE zglt068-matnr,
         descr_bens         TYPE zglt068-descr_bens,
         txa50              TYPE anla-txa50,
         zugdt              TYPE anla-zugdt,
         kostl              TYPE zglt068-kostl,
         wkurs              TYPE zglt068-wkurs,
         vlr_premio_usd     TYPE zglt068-vlr_premio_usd,
         vlr_premio_brl     TYPE zglt068-vlr_premio_brl,
         dt_baixa           TYPE zglt068-dt_baixa,
         vlr_bx_usd         TYPE zglt068-vlr_premio_usd,
         vlr_bx_brl         TYPE zglt068-vlr_premio_brl,
         aprop_usd          TYPE zglt068-vlr_premio_usd,
         aprop_brl          TYPE zglt068-vlr_premio_brl,
         saldo_usd          TYPE zglt068-vlr_premio_usd,
         saldo_brl          TYPE zglt068-vlr_premio_brl,
         perc_aprop         TYPE p DECIMALS 2,
         nro_aprop          TYPE i,
         vlr_risco_usd      TYPE zglt068-vlr_risco_usd,
         vlr_risco_brl      TYPE zglt068-vlr_risco_brl,
         vlr_aprop_aj_usd   TYPE zglt068-vlr_aprop_aj_usd,
         vlr_aprop_aj_brl   TYPE zglt068-vlr_aprop_aj_brl,
         meses              TYPE i,
*-CS2022000122 - 07.03.2022 - JT - inicio
*        color             TYPE kkblo_specialcol OCCURS 0,
*-CS2022000122 - 07.03.2022 - JT - fim
         vlr_aprop_mes_usd  TYPE zglt068-vlr_premio_usd,
         vlr_aprop_mes_brl  TYPE zglt068-vlr_premio_brl,
         hkont              TYPE zglt032-hkont,
         dt_lcto_ctb        TYPE zglt050-dt_lcto_ctb,
         aufnr              TYPE zglt068-aufnr,
         vornr              TYPE zglt068-vornr,
         ref_seq_lcto       TYPE zglt050-ref_seq_lcto,
         qtde_meses_vig(10) TYPE c,
         qtde_dias_vig(10)  TYPE c,
         tipo_competencia   TYPE char30,
       END OF ty_saida,

       BEGIN OF ty_saida_excel,
         nro_apolice   TYPE zglt050-nro_apolice,
         vig_de        TYPE zglt050-vig_de,
         vig_ate       TYPE zglt050-vig_ate,
         perc_aprop    TYPE zsgl0001-perc_aprop,
         saldo_brl(20) TYPE c,
         saldo_usd(20) TYPE c,
       END OF ty_saida_excel,

       BEGIN OF ty_073,
         belnr TYPE zib_contabil_chv-belnr.
         INCLUDE STRUCTURE zglt073.
TYPES END OF ty_073.

TYPES: BEGIN OF ty_068,
         bukrs TYPE zglt050-bukrs.
         INCLUDE STRUCTURE zglt068.
TYPES  END OF ty_068.

*-CS2022000122 - 07.03.2022 - JT - inicio
DATA: t_saida_st  TYPE ty_saida.

TYPES: BEGIN OF ty_saida1.
         INCLUDE STRUCTURE t_saida_st.
TYPES:   color TYPE kkblo_specialcol OCCURS 0.
TYPES: END OF ty_saida1.

TYPES: BEGIN OF ty_saida2.
         INCLUDE STRUCTURE t_saida_st.
TYPES: END OF ty_saida2.
*-CS2022000122 - 07.03.2022 - JT - fim

*---------------------------------------------------------------------*
*  Internal Tables e Work Areas
*---------------------------------------------------------------------*

DATA: tg_034       TYPE TABLE OF zglt034 WITH HEADER LINE,
      tg_050       TYPE TABLE OF zglt050 WITH HEADER LINE,
      tg_050_aux   TYPE TABLE OF zglt050 WITH HEADER LINE,
      tg_050_ref   TYPE TABLE OF zglt050 WITH HEADER LINE,
      tg_064       TYPE TABLE OF zglt064 WITH HEADER LINE,
      tg_032       TYPE TABLE OF zglt032 WITH HEADER LINE,
      tg_068       TYPE TABLE OF ty_068  WITH HEADER LINE,
      tg_anla      TYPE TABLE OF anla    WITH HEADER LINE,
      tg_050_bx    TYPE TABLE OF zglt050 WITH HEADER LINE,
      tg_068_bx    TYPE TABLE OF zglt068 WITH HEADER LINE,
      tg_073       TYPE TABLE OF ty_073  WITH HEADER LINE,
*-CS2022000122 - 07.03.2022 - JT - inicio
      it_saida     TYPE TABLE OF ty_saida1,
      wa_saida     TYPE ty_saida1,
      it_saida2    TYPE TABLE OF ty_saida2,
      wa_saida2    TYPE ty_saida2,
*-CS2022000122 - 07.03.2022 - JT - fim
      it_saida_exp TYPE TABLE OF zsgl0001,
      wa_saida_exp TYPE zsgl0001,
      it_saida_exp_f TYPE TABLE OF zsgl0001,
      wa_saida_exp_f TYPE zsgl0001,
      it_saida_exp_g TYPE TABLE OF zsgl0001,
      wa_saida_exp_g TYPE zsgl0001,

      it_saida_exc TYPE TABLE OF ty_saida_excel,
      wa_saida_exc TYPE ty_saida_excel.

*---------------------------------------------------------------------*
*  Variáveis
*---------------------------------------------------------------------*

DATA: vg_not_found    TYPE c,
      vg_dt_aprop_ini TYPE sy-datum,
      vg_dt_aprop_fim TYPE sy-datum.


DATA: vg_vlr_premio_brl      TYPE zglt068-vlr_premio_brl,
      vg_vlr_premio_brl_f    TYPE zglt068-vlr_premio_brl,
      vg_vlr_bx_brl          TYPE zglt068-vlr_premio_brl,
      vg_vlr_bx_brl_f        TYPE zglt068-vlr_premio_brl,
      vg_aprop_brl           TYPE zglt068-vlr_premio_brl,
      vg_aprop_brl_f         TYPE zglt068-vlr_premio_brl,
      vg_saldo_brl_f         TYPE zglt068-vlr_premio_brl,
      vg_saldo_brl           TYPE zglt068-vlr_premio_brl,
      vg_vlr_risco_brl       TYPE zglt068-vlr_risco_brl,
      vg_vlr_risco_brl_f     TYPE zglt068-vlr_risco_brl,
      vg_vlr_aprop_aj_brl    TYPE zglt068-vlr_aprop_aj_brl,
      vg_vlr_aprop_aj_brl_f  TYPE zglt068-vlr_aprop_aj_brl,
      vg_vlr_aprop_mes_brl   TYPE zglt068-vlr_premio_brl,
      vg_vlr_aprop_mes_brl_f TYPE zglt068-vlr_premio_brl.

DATA: qtde_meses_vig(10) TYPE c.
DATA: qtd_dias(10)  TYPE c,
      tot           TYPE p DECIMALS 2,
      tot_int       TYPE p,
      tot_dec       TYPE p DECIMALS 2,
      v_meses       TYPE p,
      v_dias        TYPE p,
      resultado     TYPE p,
      qtd_meses(10) TYPE c,
      lr_seqlcto    TYPE RANGE OF zglt050-seq_lcto WITH HEADER LINE,
      v_mes         TYPE p.
*----------------------------------------------------------------------*
* Estruturas ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      gs_variant   TYPE disvariant,
      variante     LIKE disvariant,
      gs_variant_c TYPE disvariant.





*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs   FOR zglt050-bukrs, "OBLIGATORY NO INTERVALS NO-EXTENSION,
                  s_vgde    FOR zglt050-vig_de,
                  s_vgate   FOR zglt050-vig_ate,
                  s_lifnr   FOR zglt050-cod_seguradora,
                  s_stipo   FOR zglt064-seq_tipo, " NO INTERVALS NO-EXTENSION,
                  s_tpopr   FOR zglt050-tp_opr NO INTERVALS NO-EXTENSION,
                  s_slcto   FOR zglt050-seq_lcto,
                  s_nr_apl  FOR zglt050-nro_apolice,
                  s_werks   FOR zglt068-werks,
                  s_anln1   FOR zglt068-anln1,
                  s_matnr   FOR zglt068-matnr,
                  s_dsbem   FOR zglt068-descr_bens.

  PARAMETER: p_comp TYPE c LENGTH 6.

  PARAMETER: p_sald TYPE char1 AS CHECKBOX.

*-CS2022000122 - 07.03.2022 - JT - inicio
  PARAMETER: p_call  TYPE char1 NO-DISPLAY DEFAULT abap_false.
*-CS2022000122 - 07.03.2022 - JT - fim

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b2.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*

DATA: vg_variant        TYPE disvariant.

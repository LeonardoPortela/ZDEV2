*&---------------------------------------------------------------------*
*& Include          ZFIR0125_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Type Declarations
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_output,

         cd_sol_isen       TYPE zed_cod_solict_isencao,
         ov_principal      TYPE vbeln,
         simulador         TYPE vbeln,
         data_venc         TYPE datum,
         moeda             TYPE zed_moeda_doc,
         vl_moeda_doc      TYPE zvalor_moeda_doc,
         tipo_negocio      TYPE zdefi_tp_neg,
         tipo_negocio2     TYPE zdefi_tp_neg,
         usuario_solicit   TYPE ernam,
         data              TYPE erdat,
         hora              TYPE erzet,
         justificativa     TYPE char255,
         justificativa_ger TYPE char255,
         justificativa2    TYPE char255,
         justificativa3    TYPE char255,
         status_solicit    TYPE  zdefi_stat_solic2,
         vkorg             TYPE vbak-vkorg,
         vkbur             TYPE vbak-vkbur,
         vl_isenc_brl      TYPE zvalor_aprov,
         ptax              TYPE zvalor_ptax,

       END OF ty_output.


TYPES: BEGIN OF ty_cad_ordem,
         vbeln       TYPE vbeln,
         org_vendas  TYPE vkorg,
         valor       TYPE netwr,
         filial      TYPE werks_d,
         solicitante TYPE string,
       END OF ty_cad_ordem.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_cad_ordem2,
         empresa TYPE  char30,
         vbeln   TYPE  vbeln,
         usuario TYPE  usnam,
         netwr   TYPE  netwr,
         data    TYPE  erdat,
         waerk   TYPE  waerk,
         filial  TYPE werks_d,
       END OF ty_cad_ordem2.

TYPES:  BEGIN OF ty_estra.
          INCLUDE TYPE zsd_estrategia_ov.
TYPES:  END OF ty_estra.

TYPES:  BEGIN OF ty_itens.
          INCLUDE TYPE zsd_itens_ov_est_isen_jur.
TYPES:    escvenda TYPE string.
TYPES   END OF ty_itens.

TYPES:  BEGIN OF ty_ordens.
          INCLUDE TYPE zsd_ord_vendas_est_isen_juros.
TYPES:    escvenda TYPE char250.
TYPES:    vkbur TYPE vbak-vkbur.
TYPES:  END OF ty_ordens.

TYPES:
  BEGIN OF ty_edit_juros,
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
    por          TYPE char3,
  END OF ty_edit_juros.

DATA   it_estra     TYPE TABLE OF ty_estra.
*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_output      TYPE TABLE OF ty_output,
      gs_output      TYPE ty_output,
      gs_zfit186     TYPE zfit186,
      gt_zfit186     TYPE TABLE OF zfit186,
      go_container   TYPE REF TO cl_gui_custom_container,
      go_container2  TYPE REF TO cl_gui_custom_container,
      go_container3  TYPE REF TO cl_gui_custom_container,
      go_alv         TYPE REF TO cl_gui_alv_grid,
      grid3          TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat    TYPE slis_t_fieldcat_alv,
      wa_stable      TYPE lvc_s_stbl,
      g_cc_itens     TYPE scrfname VALUE 'CC_ITENS',

      t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      tl_function    TYPE ui_functions,
      wl_function    LIKE LINE OF tl_function,
      tg_026         TYPE TABLE OF zfit0026.

DATA  tg_estrat    TYPE TABLE OF zsds019.
DATA: tg_itens TYPE TABLE OF ty_itens,
      wa_itens TYPE ty_itens.
DATA  it_itens     TYPE TABLE OF ty_itens.
DATA  tg_ordens    TYPE TABLE OF ty_ordens WITH HEADER LINE.
DATA  wg_cad_ordem TYPE ty_cad_ordem2.
DATA: tg_estra TYPE TABLE OF ty_estra,
      wa_estra TYPE ty_estra.

*----------------------------------------------------------------------*
* Selection Screen Variables (for screen fields)
*----------------------------------------------------------------------*
DATA p_sol_isen TYPE zfit186-cd_sol_isen.
DATA p_ov_princ TYPE zfit186-ov_principal.
DATA p_simula   TYPE zfit186-ov_principal.

DATA go_textedit             TYPE REF TO cl_gui_textedit.
DATA gt_text                 TYPE TABLE OF as4text.

CONSTANTS c_aguard_lib TYPE c VALUE '1'.

DATA estrutura       TYPE TABLE OF ty_estrutura.

DATA: aux_empresa TYPE vbak-vkbur,
      aux_t001w   TYPE t001w,
      p_doc       TYPE bseg-belnr,
      p_emp       TYPE vbak-vkbur.

DATA it_bdcdata           TYPE TABLE OF bdcdata.

DATA wa_edit              TYPE ty_edit_juros.
DATA w_edit               TYPE ty_edit_juros.


DATA: ind_rec_total            TYPE c.
DATA: ind_rec_parc             TYPE c.
DATA: ind_doc_fatura           TYPE c.
DATA: tot_saldo                TYPE zfit0026-mont_moeda.
DATA: vlr_saldo_parcial        TYPE zfit0026-mont_moeda.
DATA: vlr_saldo_finc           TYPE zfit0026-mont_moeda.
DATA: tot_saldo_fin            TYPE zfit0026-mont_moeda.

DATA gt_ordens_aux  TYPE TABLE OF zsd_ord_vendas_est_isen_juros.
DATA gt_estra_aux   TYPE TABLE OF zsd_estrategia_ov.
DATA gt_itens_aux   TYPE TABLE OF zsd_itens_ov_est_isen_jur.

DATA gv_ucomm   TYPE sy-ucomm.


DATA tg_texto     TYPE catsxt_longtext_itab.
DATA gv_display.

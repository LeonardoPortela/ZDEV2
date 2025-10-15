*&---------------------------------------------------------------------*
*& Include          ZFIR0101_TOP
*&---------------------------------------------------------------------*


**********************************************************************
* field symbols
**********************************************************************
FIELD-SYMBOLS: <fs_fld> TYPE any.

**********************************************************************
* Types
**********************************************************************

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_bsad_id,
         bukrs  TYPE bsad-bukrs,
         gjahr  TYPE bsad-gjahr,
         belnr  TYPE bsad-belnr,
         kunnr  TYPE bsad-kunnr,
         zuonr  TYPE bsad-zuonr,
         gsber  TYPE bsad-gsber,
         bldat  TYPE bsad-bldat,
         infae  TYPE bsad-infae,
         budat  TYPE bsad-budat,
         waers  TYPE bsad-waers,
         blart  TYPE bsad-blart,
         bschl  TYPE bsad-bschl,
         umskz  TYPE bsad-umskz,
         sgtxt  TYPE bsad-sgtxt,
         augdt  TYPE bsad-augdt,
         augbl  TYPE bsad-augbl,
         kostl  TYPE bsad-kostl,
         prctr  TYPE bsad-prctr,
         anln1  TYPE bsad-anln1,
         vbel2  TYPE bsad-vbel2,
         saknr  TYPE bsad-saknr,
         shkzg  TYPE bsad-shkzg,
         hbkid  TYPE bsad-hbkid,
         xstov  TYPE bsad-xstov,
         vbeln  TYPE bsad-vbeln,
         zfbdt  TYPE bsad-zfbdt,
         hkont  TYPE bsad-hkont,
         vbund  TYPE bsad-vbund,
         dmbtr  TYPE bsid-dmbtr,
         wrbtr  TYPE bsid-wrbtr,
         fcsl   TYPE bsid-fcsl,
         rfccur TYPE bsid-rfccur,
*SAKNR type bsad-SAKNR,
       END   OF ty_bsad_id.

TYPES: BEGIN OF ty_bsak_ik,
         bukrs  TYPE bsak-bukrs,
         gjahr  TYPE bsak-gjahr,
         belnr  TYPE bsak-belnr,
         lifnr  TYPE bsak-lifnr,
         zuonr  TYPE bsak-zuonr,
         gsber  TYPE bsak-gsber,
         bldat  TYPE bsak-bldat,
         ebeln  TYPE bsak-ebeln,
         ebelp  TYPE bsak-ebelp,
*         belnr type bsak-belnr,
*         infae TYPE bsak-infae,
         budat  TYPE bsak-budat,
         waers  TYPE bsak-waers,
         blart  TYPE bsak-blart,
         bschl  TYPE bsak-bschl,
         umskz  TYPE bsak-umskz,
         sgtxt  TYPE bsak-sgtxt,
         augdt  TYPE bsak-augdt,
         augbl  TYPE bsak-augbl,
         kostl  TYPE bsak-kostl,
         prctr  TYPE bsak-prctr,
         anln1  TYPE bsak-anln1,
*         vbel2 TYPE bsak-vbel2,
         saknr  TYPE bsak-saknr,
         shkzg  TYPE bsak-shkzg,
         hbkid  TYPE bsak-hbkid,
         xstov  TYPE bsak-xstov,
         zfbdt  TYPE bsak-zfbdt,
         hkont  TYPE bsak-hkont,
         vbund  TYPE bsak-vbund,
         dmbtr  TYPE bsik-dmbtr,
         wrbtr  TYPE bsik-wrbtr,
         fcsl   TYPE bsik-fcsl,
         rfccur TYPE bsik-rfccur,
*         bukrs type bsid-bukrs,
*SAKNR type bsad-SAKNR,
       END   OF ty_bsak_ik.

*TYPES: BEGIN OF ty_bsak_ik,
*         bukrs TYPE bsak-bukrs,
*         gjahr TYPE bsak-gjahr,
*         belnr TYPE bsak-belnr,
*       END   OF ty_bsak_ik.

TYPES: BEGIN OF ty_acdoca,
         rbukrs    TYPE acdoca-rbukrs,
         gjahr     TYPE acdoca-gjahr,
         belnr     TYPE acdoca-belnr,
         awref_rev TYPE acdoca-awref_rev,
         rtcur     TYPE acdoca-rtcur,
         rwcur     TYPE acdoca-rwcur,
         rkcur     TYPE acdoca-rkcur,
         racct     TYPE acdoca-racct,
         rcntr     TYPE acdoca-rcntr,
         prctr     TYPE acdoca-prctr,
         rbusa     TYPE acdoca-rbusa,
         rassc     TYPE acdoca-rassc,
         hsl       TYPE acdoca-hsl,
         ksl       TYPE acdoca-ksl,
         drcrk     TYPE acdoca-drcrk,
         budat     TYPE acdoca-budat,
         bldat     TYPE acdoca-bldat,
         blart     TYPE acdoca-blart,
         buzei     TYPE acdoca-buzei,
         zuonr     TYPE acdoca-zuonr,
         bschl     TYPE acdoca-bschl,
         ebeln     TYPE acdoca-ebeln,
         ebelp     TYPE acdoca-ebelp,
         sgtxt     TYPE acdoca-sgtxt,
         kdauf     TYPE acdoca-kdauf,
         matnr     TYPE acdoca-matnr,
         werks     TYPE acdoca-werks,
         lifnr     TYPE acdoca-lifnr,
         kunnr     TYPE acdoca-kunnr,
         umskz     TYPE acdoca-umskz,
         hbkid     TYPE acdoca-hbkid,
         augdt     TYPE acdoca-augdt,
         augbl     TYPE acdoca-augbl,
         anln1     TYPE acdoca-anln1,
         gkont     TYPE acdoca-gkont,
         netdt     TYPE acdoca-netdt,
       END   OF ty_acdoca,

       BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END   OF ty_t001,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
**<<<------"189660 - NMS - INI------>>>
         stkzn TYPE kna1-stkzn,
         stcd1 TYPE kna1-stcd1,
         stcd2 TYPE kna1-stcd2,
**<<<------"189660 - NMS - FIM------>>>
       END   OF ty_kna1,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
**<<<------"189660 - NMS - INI------>>>
         stkzn TYPE lfa1-stkzn,
         stcd1 TYPE lfa1-stcd1,
         stcd2 TYPE lfa1-stcd2,
**<<<------"189660 - NMS - FIM------>>>
       END   OF ty_lfa1,

       BEGIN OF ty_ekko,
         ebeln TYPE ekko-ebeln,
         bsart TYPE ekko-bsart,
         ekgrp TYPE ekko-ekgrp,
       END   OF ty_ekko,
**<<<------"189660 - NMS - INI------>>>
       BEGIN OF ty_ekpo,
         ebeln TYPE ekpo-ebeln,
         bednr TYPE ekpo-bednr,
       END   OF ty_ekpo,
**<<<------"189660 - NMS - FIM------>>>
       BEGIN OF ty_eket,
         ebeln TYPE eket-ebeln,
         ebelp TYPE eket-ebelp,
         eindt TYPE eket-eindt,
       END   OF ty_eket,

       BEGIN OF ty_t161t,
         bsart TYPE t161t-bsart,
         batxt TYPE t161t-batxt,
       END   OF ty_t161t,

       BEGIN OF ty_t024,
         ekgrp TYPE t024-ekgrp,
         eknam TYPE t024-eknam,
       END   OF ty_t024,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END   OF ty_t001w,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         xblnr TYPE bkpf-xblnr,
         kursf TYPE bkpf-kursf,
         bstat TYPE bkpf-bstat,
         hwae2 TYPE bkpf-hwae2,
         kurs2 TYPE bkpf-kurs2,
         stblg TYPE bkpf-stblg,
         waers TYPE bkpf-waers, "// WBARBOSA BUG-160850 13/12/2024
       END   OF ty_bkpf,

       BEGIN OF ty_bseg,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         kunnr TYPE bseg-kunnr,
         lifnr TYPE bseg-lifnr,
         zterm TYPE bseg-zterm,
         zlsch TYPE bseg-zlsch,
         zlspr TYPE bseg-zlspr,
         bvtyp TYPE bseg-bvtyp,
         xref1 TYPE bseg-xref1,
         xref2 TYPE bseg-xref2,
         xref3 TYPE bseg-xref3, " BUG - 181038 - CBRAND
         kidno TYPE bseg-kidno,
         matnr TYPE bseg-matnr, " RJF
         netdt TYPE bseg-netdt, " RJF
         ebeln TYPE bseg-ebeln, "// WBARBOSA BUG-160850 13/12/2024
         ebelp TYPE bseg-ebelp, "// WBARBOSA BUG-160850 13/12/2024
         anln1 TYPE bseg-anln1, " BUG - 181038 - CBRAND

       END   OF ty_bseg,

       BEGIN OF ty_ska1,
         saknr TYPE ska1-saknr,
         txt20 TYPE skat-txt20,
       END   OF ty_ska1,

       BEGIN OF ty_cskt,
         kostl TYPE cskt-kostl,
         ltext TYPE cskt-ltext,
       END   OF ty_cskt,

       BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         auart TYPE vbak-auart,
         vkbur TYPE vbak-vkbur,
       END   OF ty_vbak,

       BEGIN OF ty_tvkbt,
         vkbur TYPE tvkbt-vkbur,
         bezei TYPE tvkbt-bezei,
       END   OF ty_tvkbt,

       BEGIN OF ty_tvakt,
         auart TYPE tvakt-auart,
         bezei TYPE tvakt-bezei,
       END   OF ty_tvakt,

       BEGIN OF ty_vbkd,
         vbeln TYPE vbkd-vbeln,
         bstkd TYPE vbkd-bstkd,
       END   OF ty_vbkd,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF ty_makt,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
       END   OF ty_mara,

       BEGIN OF ty_t023t,
         matkl TYPE t023t-matkl,
         wgbez TYPE t023t-wgbez,
       END   OF ty_t023t,

       BEGIN OF ty_lfb1,
         lifnr TYPE lfb1-lifnr,
         bukrs TYPE lfb1-bukrs,
         akont TYPE lfb1-akont,
       END   OF ty_lfb1,

       BEGIN OF ty_zsdt0159,
         adiant        TYPE zsdt0159-adiant,
         doc_simulacao TYPE zsdt0159-doc_simulacao,
       END   OF ty_zsdt0159,

       BEGIN OF ty_zsdt0041,
         doc_simulacao TYPE zsdt0041-doc_simulacao,
         spart         TYPE zsdt0041-spart,
         vbeln         TYPE zsdt0041-vbeln,                 "BUG 181038
       END   OF ty_zsdt0041,

       BEGIN OF ty_zsdt0090,
         doc_simulacao TYPE zsdt0090-doc_simulacao,
         spart         TYPE zsdt0090-spart,
         vbeln         TYPE zsdt0090-vbeln,                 "BUG 181038
       END   OF ty_zsdt0090,

       BEGIN OF ty_zsdt0040,
         doc_simulacao  TYPE zsdt0040-doc_simulacao,
         cultura        TYPE zsdt0040-cultura,
         safra          TYPE zsdt0040-safra,
         vkbur          TYPE zsdt0040-vkbur,    "<<<------"189660 - NMS ------->>>
         dt_entrega_sem TYPE zsdt0040-dt_entrega_sem,
         dt_entrega_def TYPE zsdt0040-dt_entrega_def,
         dt_entrega_fet TYPE zsdt0040-dt_entrega_fet,
       END   OF ty_zsdt0040,

       BEGIN OF ty_zsdt0053,
         nro_sol_ov TYPE zsdt0053-nro_sol_ov,
         charg      TYPE zsdt0053-charg,
         vbeln      TYPE zsdt0053-vbeln,
       END  OF ty_zsdt0053,

       BEGIN OF ty_zsdt0051,
         nro_sol_ov  TYPE zsdt0051-nro_sol_ov,
         dtde_logist TYPE zsdt0051-dtde_logist,
         vkbur       TYPE zsdt0051-vkbur,    "<<<------"189660 - NMS ------->>>
       END  OF ty_zsdt0051,

       BEGIN OF ty_knb1,
         kunnr TYPE knb1-kunnr,
         bukrs TYPE knb1-bukrs,
         akont TYPE knb1-akont,
       END   OF ty_knb1,

       BEGIN OF ty_zfit0208,
         cod_operacao      TYPE zfit0208-cod_operacao,
         saknr             TYPE zfit0208-saknr,
         bschl             TYPE zfit0208-bschl,
         blart             TYPE zfit0208-blart,
         matkl             TYPE zfit0208-matkl,
         bsart             TYPE zfit0208-bsart,
         "kostl             TYPE zfit0208-kostl, "BUG - 181038 - CBRAND
         lifnr             TYPE zfit0208-lifnr,
         "drcrk             TYPE zfit0208-drcrk, "BUG - 181038 - CBRAND
         gkont             TYPE zfit0208-gkont,
         "bukrs             TYPE zfit0208-bukrs, "BUG - 181038 - CBRAND
         desc_cod_operacao TYPE zfit0208-desc_cod_operacao,
         desc_saknr        TYPE zfit0208-desc_saknr,
         atribuicao        TYPE zfit0208-atribuicao,
         pdd               TYPE zfit0208-pdd,
         anln1             TYPE zfit0208-anln1,
         sgtxt             TYPE zfit0208-sgtxt,
         kidno             TYPE zfit0208-kidno,
         xref1             TYPE zfit0208-xref1,
       END OF ty_zfit0208,
*** BUG - 181038 - CBRAND - Inicio
       BEGIN OF ty_zfit0235,
         cod_subop  TYPE zfit0235-cod_subop,
         saknr      TYPE zfit0235-saknr,
         blart      TYPE zfit0235-blart,
         des_subop  TYPE zfit0235-des_subop,
         desc_saknr TYPE zfit0235-desc_saknr,
         sgtxt      TYPE zfit0235-sgtxt,
         kidno      TYPE zfit0235-kidno,
         xref1      TYPE zfit0235-xref1,
         atribuicao TYPE zfit0235-atribuicao,
       END OF ty_zfit0235,
*** BUG - 181038 - CBRAND - Fim

       BEGIN OF ty_ekko_ebeln,
         ebeln TYPE ekko-ebeln,
       END OF ty_ekko_ebeln,

       BEGIN OF ty_ebeln_ebelp,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
       END OF ty_ebeln_ebelp,

       BEGIN OF ty_t001w_werks,
         werks TYPE t001w-werks,
       END OF ty_t001w_werks,

       BEGIN OF ty_ska1_saknr,
         saknr TYPE ska1-saknr,
       END OF ty_ska1_saknr,

       BEGIN OF ty_cskt_kostl,
         kostl TYPE cskt-kostl,
       END OF ty_cskt_kostl,

       BEGIN OF ty_vbak_vbeln,
         vbeln TYPE vbak-vbeln,
       END OF ty_vbak_vbeln,

       BEGIN OF ty_mara_matnr,
         matnr TYPE mara-matnr,
       END OF ty_mara_matnr,

       BEGIN OF ty_kunnr,
         kunnr TYPE kna1-kunnr,
       END OF ty_kunnr,

       BEGIN OF ty_knb1_kunnr_bukrs,
         kunnr TYPE knb1-kunnr,
         bukrs TYPE knb1-bukrs,
       END OF ty_knb1_kunnr_bukrs,

       BEGIN OF ty_lifnr,
         lifnr TYPE lfa1-lifnr,
       END OF ty_lifnr,

       BEGIN OF ty_ov_aux,
         vbeln TYPE zsdt0062-vbeln,
         xblnr TYPE bkpf-xblnr,
       END OF ty_ov_aux,

       BEGIN OF ty_lfb1_lifnr_bukrs,
         lifnr TYPE lfb1-lifnr,
         bukrs TYPE lfb1-bukrs,
       END OF ty_lfb1_lifnr_bukrs.


TYPES: BEGIN OF ty_alv_saida,
         rbukrs                  TYPE acdoca-rbukrs, "Empresa
         nome_empresa            TYPE t001-butxt,   "Nome Empresa
         cliente_fornecedor      TYPE char10,       "ID Conta Cliente/ Fornecedor
         nome_cliente_fornecedor TYPE name1_gp,     "Nome Cliente/Fornecedor
         idfis                   TYPE char20,       "CPF/CNPJ
         ebeln                   TYPE acdoca-ebeln, "Doc. Compras
         bednr                   TYPE ekpo-bednr,   "Nº acompanhamento      "<<<------"189660 - NMS ------->>>
         bsart                   TYPE ekko-bsart,   "Tp. Pedido
         batxt                   TYPE batxt,        "Desc. Tp. Pedido
         eknam                   TYPE eknam,        "Nome Comprador
         eindt                   TYPE eket-eindt,   "Data remessa pedido
         zuonr                   TYPE acdoca-zuonr, "Atribuição
         rbusa                   TYPE acdoca-rbusa, "Divisão
         nome_divisao            TYPE name1,        "Nome Divisao
         xblnr                   TYPE bkpf-xblnr,   "Referência
         belnr                   TYPE acdoca-belnr, "Nº documento
         bldat                   TYPE acdoca-bldat, "Data documento
         budat                   TYPE acdoca-budat, "Data lançamento
         netdt                   TYPE acdoca-netdt, "Vencimento líquido
         gjahr                   TYPE acdoca-gjahr, "Exercício
         zterm                   TYPE bseg-zterm,   "Cond.Pgto
         waers                   TYPE bkpf-waers,   "Moeda "// wbarbosa BUG-160850 13/12/2024
         rtcur                   TYPE acdoca-rtcur, "Moeda documento
         hsl                     TYPE acdoca-hsl,   "Vlr. Moeda documento
         rkcur                   TYPE acdoca-rkcur, "Moeda Forte
         ksl                     TYPE acdoca-ksl,   "Vlr. Moeda forte
         kursf                   TYPE bkpf-kursf,   "Taxa Cambio Efetiva
         blart                   TYPE acdoca-blart, "Tipo de documento
         bschl                   TYPE acdoca-bschl, "Chave de lançamento
         umskz                   TYPE acdoca-umskz, "Cód.Razão Especial
         sgtxt                   TYPE acdoca-sgtxt, "Texto
         racct                   TYPE acdoca-racct, "Conta do Razão
         txt20                   TYPE skat-txt20,   "Txt.descr.cta.Razão
         augdt                   TYPE acdoca-augdt, "Data de compensação
         augbl                   TYPE acdoca-augbl, "Doc.compensação
         rcntr                   TYPE acdoca-rcntr, "Centro custo
         ltext                   TYPE cskt-ltext,   "Nome Centro de Custo
         prctr                   TYPE acdoca-prctr, "Centro de lucro
         anln1                   TYPE acdoca-anln1, "Imobilizado
         kidno                   TYPE bseg-kidno,   "Referência de pagamento
         xref1                   TYPE bseg-xref1,   "Chave referência 1
         xref2                   TYPE bseg-xref2,   "Chave referência 2
         xref3                   TYPE bseg-xref3,   "Chave referência 3 " BUG - 181038 - CBRAND
         zlspr                   TYPE bseg-zlspr,   "Bloqueio pgto.
         vkbur                   TYPE vbak-vkbur,   "Esc Venda
         bezei                   TYPE tvkbt-bezei,  "Nome Esc Venda
         kdauf                   TYPE acdoca-kdauf, "Ordem Venda
         auart                   TYPE vbak-auart,   "TP.OV.
         desc_ov                 TYPE tvakt-bezei,  "Descrição Tipo OV
         cultura                 TYPE zsdt0040-cultura,
         safra                   TYPE zsdt0040-safra,
         dt_final_entrega        TYPE zsdt0040-dt_entrega_sem,
         rassc                   TYPE acdoca-rassc, "Soc.parc.negócios
         bstkd                   TYPE vbkd-bstkd,   "Texto Pedido (SD)
         matnr                   TYPE acdoca-matnr, "Material
         matkl                   TYPE mara-matkl,   "Grupo de Mercadoria
         maktx                   TYPE makt-maktx,   "Descrição Material
         wgbez                   TYPE t023t-wgbez,  "Descrição Grupo Mercadoria
         hbkid                   TYPE acdoca-hbkid, "Banco Empresa
         bvtyp                   TYPE bseg-bvtyp,   "Tipo Banco Parceiro
         zlsch                   TYPE bseg-zlsch,   "Forma Pagamento
         awref_rev               TYPE acdoca-awref_rev, "Estorno com
         drcrk                   TYPE acdoca-drcrk,
         cod_deb_cred            TYPE char10,           "Descrição Cód.débito/crédito
         natureza_cto_razao      TYPE char10,           "Natureza Conta Razão
         akont                   TYPE lfb1-akont,       "Cadastro Cta.de reconciliação
         pdd                     TYPE zfit0208-pdd,     "PDD
         tp_operacao             TYPE char50,           "Tipo de Operação
         tp_subop                TYPE char50,           "Tipo de Sub-operação
         classificacao_st        TYPE char50,           "Classificação Status
         historico               TYPE char50,          "Histórico
         anexo                   TYPE icon_d,           "Anexo
         aging_list              TYPE char50,           "Aging List
         gkont                   TYPE acdoca-gkont.
TYPES: END   OF ty_alv_saida.


**********************************************************************
* Variaveis
**********************************************************************
DATA: l_sel_button TYPE smp_dyntxt,
      l_opcao      TYPE char1,
      l_nfps       TYPE znfnum,
      l_data_char  TYPE char10,
      l_tabix      TYPE sy-tabix,
      l_icon_name  TYPE icon-name,
      l_leave      TYPE syst_ucomm.

DATA: v_p_db_tab(8)  TYPE c,
      v_p_stcnam(12) TYPE c,
      v_p_scmant(4)  TYPE c,
      v_p_title(40)  TYPE c,
      v_classname    TYPE bapibds01-classname,
      v_objkey       TYPE swotobjid-objkey,
      v_client       TYPE sy-mandt,
      v_day          TYPE p,
      g_sel_var      TYPE rsvar-variant,
      g_sel_vartxt   TYPE rsvar-vtext,
      v_data_p       TYPE sy-datum. "BUG - 178687 - CBRAND


**********************************************************************
* Tabela interna
**********************************************************************
DATA: t_alv_saida          TYPE TABLE OF ty_alv_saida,
      t_alv_saidac         TYPE TABLE OF ty_alv_saida, "RJF
      wa_alv_saida         TYPE ty_alv_saida,
      t_acdoca             TYPE TABLE OF ty_acdoca,
      wa_acdoca            TYPE ty_acdoca,
      it_bsad_id           TYPE STANDARD TABLE OF ty_bsad_id,
      wa_bsad_id           TYPE ty_bsad_id,
      it_bsak_ik           TYPE TABLE OF ty_bsak_ik,
      wa_bsak_ik           TYPE ty_bsak_ik,
      t_zsdt0159           TYPE TABLE OF ty_zsdt0159,
      wa_zsdt0159          TYPE ty_zsdt0159,
      t_zsdt0040           TYPE TABLE OF ty_zsdt0040,
      wa_zsdt0040          TYPE ty_zsdt0040,
      t_zsdt0041           TYPE TABLE OF ty_zsdt0041,
      wa_zsdt0041          TYPE ty_zsdt0041,
      t_zsdt0090           TYPE TABLE OF ty_zsdt0090,
      wa_zsdt0090          TYPE ty_zsdt0090,
      t_zsdt0053           TYPE TABLE OF ty_zsdt0053,
      wa_zsdt0053          TYPE ty_zsdt0053,
      t_zsdt0051           TYPE TABLE OF ty_zsdt0051,
      wa_zsdt0051          TYPE ty_zsdt0051,
      t_t001               TYPE TABLE OF ty_t001,
      wa_t001              TYPE ty_t001,
      t_eket               TYPE TABLE OF ty_eket,
      wa_eket              TYPE ty_eket,
      t_kna1               TYPE TABLE OF ty_kna1,
      wa_kna1              TYPE ty_kna1,
      t_lfa1               TYPE TABLE OF ty_lfa1,
      wa_lfa1              TYPE ty_lfa1,
      t_ekko               TYPE TABLE OF ty_ekko,
      wa_ekko              TYPE ty_ekko,
**<<<------"189660 - NMS - INI------>>>
      gt_ekpo              TYPE TABLE OF ty_ekpo,
      el_ekpo              TYPE ty_ekpo,
**<<<------"189660 - NMS - FIM------>>>
      t_t161t              TYPE TABLE OF ty_t161t,
      wa_t161t             TYPE ty_t161t,
      t_t024               TYPE TABLE OF ty_t024,
      wa_t024              TYPE ty_t024,
      t_t001w              TYPE TABLE OF ty_t001w,
      wa_t001w             TYPE ty_t001w,
      t_bkpf               TYPE TABLE OF ty_bkpf,
      wa_bkpf              TYPE ty_bkpf,
      t_bseg               TYPE TABLE OF ty_bseg,
      wa_bseg              TYPE ty_bseg,
      t_ska1               TYPE TABLE OF ty_ska1,
      wa_ska1              TYPE ty_ska1,
      t_cskt               TYPE TABLE OF ty_cskt,
      wa_cskt              TYPE ty_cskt,
      t_vbak               TYPE TABLE OF ty_vbak,
      wa_vbak              TYPE ty_vbak,
      t_tvkbt              TYPE TABLE OF ty_tvkbt,
      wa_tvkbt             TYPE ty_tvkbt,
      t_tvakt              TYPE TABLE OF ty_tvakt,
      wa_tvakt             TYPE ty_tvakt,
      t_vbkd               TYPE TABLE OF ty_vbkd,
      wa_vbkd              TYPE ty_vbkd,
      t_makt               TYPE TABLE OF ty_makt,
      wa_makt              TYPE ty_makt,
      t_mara               TYPE TABLE OF ty_mara,
      wa_mara              TYPE ty_mara,
      t_t023t              TYPE TABLE OF ty_t023t,
      wa_t023t             TYPE ty_t023t,
      t_lfb1               TYPE TABLE OF ty_lfb1,
      wa_lfb1              TYPE ty_lfb1,
      t_knb1               TYPE TABLE OF ty_knb1,
      wa_knb1              TYPE ty_knb1,
      t_zfit0208           TYPE TABLE OF ty_zfit0208,
      t_zfit0208_class     TYPE TABLE OF ty_zfit0208,
      t_zfit0235           TYPE TABLE OF ty_zfit0235, "Bug - 181038
      t_zfit0235_class     TYPE TABLE OF ty_zfit0235, "Bug - 181038
      wa_zfit0208          TYPE ty_zfit0208,
      t_gos_connections    TYPE STANDARD TABLE OF bdn_con,
      t_ekko_ebeln         TYPE TABLE OF ty_ekko_ebeln,
      wa_ekko_ebeln        TYPE ty_ekko_ebeln,
      t_ekko_ebelf         TYPE TABLE OF ty_ekko_ebeln, "RJF
      wa_ekko_ebelf        TYPE ty_ekko_ebeln,          "RJF
      t_ebeln_ebelp        TYPE TABLE OF ty_ebeln_ebelp,
      wa_ebeln_ebelp       TYPE ty_ebeln_ebelp,
      t_ebeln_ebelf        TYPE TABLE OF ty_ebeln_ebelp, "RJF
      wa_ebeln_ebelf       TYPE ty_ebeln_ebelp,          "RJF
      t_t001w_werks        TYPE TABLE OF ty_t001w_werks,
      wa_t001w_werks       TYPE ty_t001w_werks,
      t_t001w_werkc        TYPE TABLE OF ty_t001w_werks, "RJF
      wa_t001w_werkc       TYPE ty_t001w_werks,  "RJF
      t_ska1_saknr         TYPE TABLE OF ty_ska1_saknr,
      wa_ska1_saknr        TYPE ty_ska1_saknr,
      t_ska1_saknc         TYPE TABLE OF ty_ska1_saknr, "RJF
      wa_ska1_saknc        TYPE ty_ska1_saknr,          "RJF
      t_cskt_kostl         TYPE TABLE OF ty_cskt_kostl,
      wa_cskt_kostl        TYPE ty_cskt_kostl,
      t_cskt_kostc         TYPE TABLE OF ty_cskt_kostl, "RJF
      wa_cskt_kostc        TYPE ty_cskt_kostl,      "RJF
      t_vbak_vbeln         TYPE TABLE OF ty_vbak_vbeln,
      wa_vbak_vbeln        TYPE ty_vbak_vbeln,
      t_mara_matnr         TYPE TABLE OF ty_mara_matnr,
      wa_mara_matnr        TYPE ty_mara_matnr,
      t_kunnr              TYPE TABLE OF ty_kunnr,
      wa_kunnr             TYPE ty_kunnr,
      t_kunnr_c            TYPE TABLE OF ty_kunnr, "RJF
      wa_kunnr_c           TYPE ty_kunnr,          "RJF
      t_knb1_kunnr_bukrs   TYPE TABLE OF ty_knb1_kunnr_bukrs,
      wa_knb1_kunnr_bukrs  TYPE ty_knb1_kunnr_bukrs,
      t_knb1_kunnr_bukrc   TYPE TABLE OF ty_knb1_kunnr_bukrs, "RJF
      wa_knb1_kunnr_bukrc  TYPE ty_knb1_kunnr_bukrs,          "RJF
      t_lifnr              TYPE TABLE OF ty_lifnr,
      wa_lifnr             TYPE ty_lifnr,
      t_lifnr_f            TYPE TABLE OF ty_lifnr, "RJF
      wa_lifnr_f           TYPE ty_lifnr,          "RJF
      t_lfb1_lifnr_bukrs   TYPE TABLE OF ty_lfb1_lifnr_bukrs,
      wa_lfb1_lifnr_bukrs  TYPE ty_lfb1_lifnr_bukrs,
      t_lfb1_lifnr_bukrf   TYPE TABLE OF ty_lfb1_lifnr_bukrs, "RJF
      wa_lfb1_lifnr_bukrf  TYPE ty_lfb1_lifnr_bukrs,          "RJF

      it_zsdt0051          TYPE TABLE OF ty_zsdt0051,
      ws_zsdt0051          TYPE ty_zsdt0051,
      it_zsdt0062          TYPE TABLE OF zsdt0062,
      it_zsdt0062_aux      TYPE TABLE OF zsdt0062,
      ws_zsdt0062          TYPE zsdt0062,
      it_zsdt0053          TYPE TABLE OF zsdt0053,
      ws_zsdt0053          TYPE zsdt0053,
      it_ov_aux            TYPE TABLE OF ty_ov_aux,



*------------------------------------
*---- ALV
*------------------------------------
      dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,

      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      t_sort               TYPE lvc_t_sort,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm,
      zcl_util             TYPE REF TO zcl_util,
      gr_tree              TYPE REF TO cl_salv_tree.

DATA: lv_cont TYPE lvc_colpos.


DATA: xs_events TYPE slis_alv_event,
      events    TYPE slis_t_event,
      t_print   TYPE slis_print_alv,
      v_report  LIKE sy-repid,
      t_top     TYPE slis_t_listheader.

*** BUG - 181038 - Inicio - CBRAND
DATA: gva_koart TYPE bseg-koart,
      gva_shkzg TYPE bseg-shkzg.
*** BUG - 181038 - Fim - CBRAND

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

**********************************************************************
* ranges
**********************************************************************
DATA: rg_augdt      TYPE RANGE OF acdoca-augdt,
      wa_augdt      LIKE LINE  OF rg_augdt,
      rg_augdt_sp   TYPE RANGE OF acdoca-augdt,
      wa_augdt_sp   LIKE LINE  OF rg_augdt,
      rg_racct      TYPE RANGE OF acdoca-racct,
      wa_racct      LIKE LINE  OF rg_racct,
      rg_bschl      TYPE RANGE OF acdoca-bschl,
      wa_bschl      LIKE LINE  OF rg_bschl,
      rg_blart      TYPE RANGE OF acdoca-blart,
      wa_blart      LIKE LINE  OF rg_blart,
      rg_matkl      TYPE RANGE OF mara-matkl,
      wa_matkl      LIKE LINE  OF rg_matkl,
      rg_bsart      TYPE RANGE OF ekko-bsart,
      wa_bsart      LIKE LINE  OF rg_bsart,
      rg_rcntr      TYPE RANGE OF acdoca-rcntr,
      wa_rcntr      LIKE LINE  OF rg_rcntr,
      rg_zuonr      TYPE RANGE OF acdoca-zuonr,
      wa_zuonr      LIKE LINE  OF rg_zuonr,
      rg_fornecedor TYPE RANGE OF acdoca-lifnr,
      wa_fornecedor LIKE LINE  OF rg_fornecedor,
      rg_drcrk      TYPE RANGE OF acdoca-drcrk,
      wa_drcrk      LIKE LINE  OF rg_drcrk,
      rg_gkont      TYPE RANGE OF acdoca-gkont,
      wa_gkont      LIKE LINE  OF rg_gkont.


*-----------------------------------------------------------------*
*   TYPE-POOLS                                                    *
*-----------------------------------------------------------------*
TYPE-POOLS: slis.

*-----------------------------------------------------------------*
*   VARIABLES                                                     *
*-----------------------------------------------------------------*
DATA: w_variant        TYPE rsvar-variant,
      w_user_vari      TYPE rsvar-variant,
      w_vari_report    TYPE rsvar-report,
      sel_variant      TYPE rsvar-variant,
      sel_variant_text TYPE rsvar-vtext,
      w_report         TYPE rsvar-report,
      variant_exists   TYPE c.
DATA: vg_edit          TYPE int4.    "<<<------"189660 - NMS ------->>>

*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.

*Class definition for ALV toolbar
CLASS:  lcl_alv_toolbar     DEFINITION DEFERRED.
DATA: obg_toolbar          TYPE REF TO lcl_alv_toolbar.

*** BUG - 171955 - Inicio - CBRAND
*-- Create a filter internal table with multiple values
DATA filter_saknr TYPE SORTED TABLE OF  zfit0208-saknr  WITH UNIQUE KEY table_line.
DATA filter_bschl TYPE SORTED TABLE OF  zfit0208-bschl  WITH UNIQUE KEY table_line.
DATA filter_blart TYPE SORTED TABLE OF  zfit0208-blart  WITH UNIQUE KEY table_line.
DATA filter_matkl TYPE SORTED TABLE OF  zfit0208-matkl  WITH UNIQUE KEY table_line.
DATA filter_bsart TYPE SORTED TABLE OF  zfit0208-bsart  WITH UNIQUE KEY table_line.
"DATA filter_kostl TYPE SORTED TABLE OF  zfit0208-kostl  WITH UNIQUE KEY table_line. "BUG - 181038 - CBRAND
DATA filter_lifnr TYPE SORTED TABLE OF  zfit0208-lifnr  WITH UNIQUE KEY table_line.
"DATA filter_drcrk TYPE SORTED TABLE OF  zfit0208-drcrk  WITH UNIQUE KEY table_line. "BUG - 181038 - CBRAND
DATA filter_gkont TYPE SORTED TABLE OF  zfit0208-gkont  WITH UNIQUE KEY table_line.
"DATA filter_bukrs TYPE SORTED TABLE OF  zfit0208-bukrs  WITH UNIQUE KEY table_line. "BUG - 181038 - CBRAND

*** BUG - 181038 - Inicio - CBRAND
DATA  filter_anln1    TYPE SORTED TABLE OF  zfit0208-anln1 WITH UNIQUE KEY table_line.
DATA  filter_sgtxt    TYPE SORTED TABLE OF  zfit0208-sgtxt WITH UNIQUE KEY table_line.
DATA  filter_kidno    TYPE SORTED TABLE OF  zfit0208-kidno WITH UNIQUE KEY table_line.
DATA  filter_xref1    TYPE SORTED TABLE OF  zfit0208-xref1 WITH UNIQUE KEY table_line.
*** BUG - 181038 - Fim - CBRAND

*** BUG - 171955 - Inicio - CBRAND

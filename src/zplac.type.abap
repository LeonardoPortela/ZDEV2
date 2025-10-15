TYPE-POOL zplac .

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF zplac_nom_transporte.
TYPES: status             TYPE c LENGTH 4,
       ds_status          TYPE string,
       "Tabela Programação
       nr_qtde_programada TYPE j_1bnetqty,
       nr_qtde_saldo_prog TYPE j_1bnetqty,
       "Tabela de Remetentes
       nr_qtde_remetente  TYPE j_1bnetqty,
       nr_qtde_saldo_rem  TYPE j_1bnetqty,
       "Tabela de Notas
       nr_qtde_planejada  TYPE j_1bnetqty,
       nr_qtde_plan_fili  TYPE j_1bnetqty,
       nr_qtde_saldo_pla  TYPE j_1bnetqty,
       "Tabela de Notas Aplicadas
       nr_efetivada       TYPE j_1bnetqty,
       nr_saldo_efetivar  TYPE j_1bnetqty.
       INCLUDE STRUCTURE znom_transporte.
TYPES: END OF zplac_nom_transporte.

TYPES: BEGIN OF zplac_nom_programacao.
TYPES: status             TYPE c LENGTH 4,
       ds_status          TYPE char20,
       ds_empresa         TYPE butxt,
       ds_filial          TYPE name1,
       ds_material        TYPE maktx,
       ds_cliente         TYPE name1,
       nbm                TYPE steuc,
       "Tabela de Remetentes
       nr_total_remetente TYPE j_1bnetqty,
       nr_qtde_remetente  TYPE j_1bnetqty,
       nr_qtde_saldo_rem  TYPE j_1bnetqty,
       "Tabela de Notas
       nr_qtde_planejada  TYPE j_1bnetqty,
       nr_qtde_plan_fili  TYPE j_1bnetqty,
       nr_qtde_saldo_pla  TYPE j_1bnetqty,
       "Tabela de Notas Aplicadas
       nr_efetivada       TYPE j_1bnetqty,
       nr_saldo_efetivar  TYPE j_1bnetqty.
       INCLUDE STRUCTURE znom_programacao.
TYPES: END OF zplac_nom_programacao.

TYPES: BEGIN OF zplac_due_antecipada.
         INCLUDE STRUCTURE zsdt0170.
TYPES: END OF zplac_due_antecipada.

TYPES: BEGIN OF zplac_due_retificacao.
         INCLUDE STRUCTURE zsdt0170.
TYPES: END OF zplac_due_retificacao.

TYPES: BEGIN OF zplac_due_retificacao_conf,
         id_nomeacao_tran    TYPE zsdt0170-id_nomeacao_tran,
         "TP_EXPORTACAO       TYPE ZSDT0170-TP_EXPORTACAO,
         codigo_urf_embarque TYPE zsdt0170-codigo_urf_embarque,
         codigo_ra_embarque  TYPE zsdt0170-codigo_ra_embarque,
         bukrs               TYPE zsdt0170-bukrs,
         matnr               TYPE zsdt0172-matnr,
         "UE_EXPORTADA        TYPE ZSDT0172-UE_EXPORTADA,
         peso_liq_total      TYPE zsdt0172-peso_liq_total,
         check               TYPE c LENGTH 4,
         dt_registro         TYPE zsdt0183-dt_registro,
         hr_registro         TYPE zsdt0183-hr_registro,
         us_registro         TYPE zsdt0183-us_registro,
         ncm                 TYPE steuc.
TYPES: END OF zplac_due_retificacao_conf.

TYPES: BEGIN OF zplac_nom_programacao_2.
TYPES: fili_name1    TYPE name1_gp,
       fili_stcd1    TYPE char18,
       cliente_name1 TYPE name1_gp,
       cliente_stcd1 TYPE char18,
       cliente_uf    TYPE regio,
       cliente_munic TYPE text60.
       INCLUDE STRUCTURE znom_programacao.
TYPES: END OF zplac_nom_programacao_2.

TYPES: BEGIN OF zplac_nom_remetente.
TYPES: icone             TYPE c LENGTH 4,
       name1             TYPE name1_gp,
       stcd1             TYPE char18,
       uf                TYPE regio,
       munic             TYPE text60,
       nr_remetente2     TYPE j_1bnetqty,
       nr_efetivada      TYPE j_1bnetqty,
       nr_recusado       TYPE j_1bnetqty,
       nr_saldo_efetivar TYPE j_1bnetqty,
       name_filial       TYPE name1_gp,
       stcd1_filial      TYPE char18,
       uf_filial         TYPE regio,
       munic_filial      TYPE text60.
       INCLUDE STRUCTURE znom_remetente.
TYPES: END OF zplac_nom_remetente.

TYPES: BEGIN OF zplac_filtro_reme.
TYPES: empresa             LIKE j_1bbranch-bukrs,
       centro              LIKE j_1bbranch-branch,
       ds_centro           LIKE j_1bbranch-name,
       remetente           LIKE lfa1-lifnr,
       ds_remetente        LIKE lfa1-name1,
       material            LIKE mara-matnr,
       data_ini            LIKE j_1bnfdoc-docdat,
       data_fim            LIKE j_1bnfdoc-docdat,
       nr_qtd_vinc         TYPE j_1bnetqty,
       qtde_nf_quebra      TYPE i,
       contrato            LIKE znom_programacao-contrato,
       grp_retorno         LIKE znom_reme_notas-grp_retorno,
       id_due              LIKE zsdt0170-id_due,
       numero_due          LIKE zsdt0170-numero_due,
       regio_due           LIKE zsdt0170-regio,
       codigo_urf_embarque LIKE zsdt0170-codigo_urf_embarque,
       codigo_ra_embarque  LIKE zsdt0170-codigo_ra_embarque,
       codigo_ncm          LIKE zsdt0172-codigo_ncm,
       ue_exportada        LIKE zsdt0172-ue_exportada,
       peso_liq_due        LIKE zsdt0172-peso_liq_total,
       due_eudr            LIKE zsdt0170-eudr, "WPP 23102024 - US-153330 --->>>
       tp_vinc1            TYPE c,
       tp_vinc2            TYPE c,
       c_cct               TYPE c,
       s_cct               TYPE c,
       regio               LIKE zsdt0170-regio,
       ck_cct_cp           TYPE c,
       show_rfl            TYPE c,
       ck_nf_restr         TYPE c,
       tipov               TYPE auart,          "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       preco               TYPE dmbtr,          "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       depst               TYPE lgort_d,        "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       safra               TYPE charg_d,        "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       cvirt               TYPE werks_ext,      "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       desc_cvirt          TYPE ad_sort1,       "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       fins_espec          TYPE c,
       comerc              TYPE c,
       retornar_eudr       TYPE c.
TYPES: END OF zplac_filtro_reme.

TYPES: BEGIN OF zplac_nom_reme_notas.
TYPES: docdat            TYPE j_1bdocdat,
       model             TYPE j_1bmodel,
       series	           TYPE j_1bseries,
       branch	           TYPE j_1bbranc_,
       parvw             TYPE j_1bparvw,
       parid             TYPE j_1bparid,
       name1             TYPE name1_gp,
       regio             TYPE regio,
       nfenum	           TYPE j_1bnfnum9,
       matnr             TYPE matnr,
       maktx             TYPE maktx,
       nbm               TYPE steuc,
       charg             TYPE charg_d,
       cfop	             TYPE j_1bcfop,
       dt_chegada        TYPE j_1bdocdat,
       nr_quantidade2    TYPE j_1bnetqty,
       nr_efetivada      TYPE j_1bnetqty,
       nr_recusado       TYPE j_1bnetqty,
       nr_saldo_efetivar TYPE j_1bnetqty,
       docnum_rt         TYPE znom_remetente-docnum_rt,
       nr_ordem          TYPE znom_remetente-nr_ordem,
       rowcolor(4)       TYPE c,
       index             TYPE lvc_s_row-index,
       novo(1),
       tipov             TYPE auart,          "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       preco             TYPE dmbtr,          "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       depst             TYPE lgort_d,        "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       safra             TYPE charg_d,        "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       cvirt             TYPE werks_ext,      "113637 - CS2023000378 AJUSTAR ZMEMO00 PARA CRIAR ORDEM DE VENDA AUTOMATICA - PARTE 1 - PSA
       eudr              TYPE zeudr. "// WBARBOSA 28102024 US-153330
       INCLUDE STRUCTURE znom_reme_notas.
TYPES: END OF zplac_nom_reme_notas.

TYPES: BEGIN OF zplac_nom_reme_notlog.
TYPES: docdat            TYPE j_1bdocdat,
       model             TYPE j_1bmodel,
       series	           TYPE j_1bseries,
       branch	           TYPE j_1bbranc_,
       parvw             TYPE j_1bparvw,
       parid             TYPE j_1bparid,
       name1             TYPE name1_gp,
       nfenum	           TYPE j_1bnfnum9,
       matnr             TYPE matnr,
       maktx             TYPE maktx,
       nbm               TYPE steuc,
       charg             TYPE charg_d,
       cfop	             TYPE j_1bcfop,
       nr_quantidade2    TYPE j_1bnetqty,
       nr_efetivada      TYPE j_1bnetqty,
       nr_recusado       TYPE j_1bnetqty,
       nr_saldo_efetivar TYPE j_1bnetqty,
       docnum_rt         TYPE znom_remetente-docnum_rt,
       nr_ordem          TYPE znom_remetente-nr_ordem,
       rowcolor(4)       TYPE c.
       INCLUDE STRUCTURE znom_reme_notlog.
TYPES: END OF zplac_nom_reme_notlog.

TYPES: BEGIN OF zplac_notasfiscais.
TYPES: icone            TYPE c LENGTH 4, "CS2019000714
       docnum	          TYPE j_1bdocnum,
       itmnum	          TYPE j_1bitmnum,
       docdat           TYPE j_1bdocdat,
       model            TYPE j_1bmodel,
       series	          TYPE j_1bseries,
       nfnum            TYPE j_1bnfnumb,
       bukrs 	          TYPE bukrs,
       branch	          TYPE j_1bbranc_,
       parvw            TYPE j_1bparvw,
       parid            TYPE j_1bparid,
       partyp	          TYPE j_1bpartyp,
       name1            TYPE name1_gp,
       regio            TYPE regio,
       nfenum	          TYPE j_1bnfnum9,
       matnr            TYPE matnr,
       maktx            TYPE maktx,
       charg            TYPE charg_d,
       cfop	            TYPE j_1bcfop,
       menge            TYPE j_1bnetqty,
       meins            TYPE j_1bnetunt,
       nfe              TYPE j_1bnfe,
       entrad           TYPE j_1bnfdoc-entrad,
       qtde_nf          TYPE j_1bnetqty,
       nr_quantidade2   TYPE j_1bnetqty,
       nr_utilizada     TYPE j_1bnetqty,
       nr_utilizada_cct TYPE j_1bnetqty,
       nr_saldo         TYPE j_1bnetqty,
       dt_chegada       TYPE j_1bdocdat,
       peso_cct         TYPE j_1bnetqty,
       saldo_cct        TYPE j_1bnetqty,
       dif_peso_cct_nf  TYPE j_1bnetqty,
       nf_chegada_porto TYPE c LENGTH 6,
       peso_aferido_cct TYPE j_1bnetqty,
       tp_nf_rem        TYPE znom_reme_notas-tp_nf_rem,
       del_reg          TYPE c,
       id_due           TYPE zsdt0170-id_due,
       rom_completo     TYPE c LENGTH 4,
       docnum_rfl       TYPE j_1bnfdoc-docnum,
       cct_rfl          TYPE c LENGTH 4,
       restricao        TYPE c LENGTH 150,
       cct_prod         TYPE c,
       ind_rfl          TYPE c LENGTH 4,
       eudr             TYPE zeudr, "// WBARBOSA 28102024 US-153330
       line_color(4)    TYPE c.
TYPES: END OF zplac_notasfiscais.

TYPES: BEGIN OF zplac_nom_prog_reme.
TYPES: nr_registro_expo	TYPE znr_reg,
       numero_due       TYPE zsdt0170-numero_due,
       id_dde	          TYPE zid_dde,
       nr_dde	          TYPE znr_dde,
       ntgew            TYPE ntgew_15,
       ntgew_rec        TYPE ntgew_15,
       gewei            TYPE gewei,
       kunnr            TYPE kunwe,
       name1            TYPE name1_gp,
       vbeln            TYPE vbeln_vf,
       waerk            TYPE waerk,
       netwr            TYPE netwr,
       kurrf            TYPE kurrf,
       docnum           TYPE j_1bdocnum.
       INCLUDE STRUCTURE znom_prog_reme.
TYPES: END OF zplac_nom_prog_reme.

TYPES: BEGIN OF zplac_zdoc_nf_produtor.
TYPES: docdat         TYPE j_1bdocdat,
       model          TYPE j_1bmodel,
       series	        TYPE j_1bseries,
       branch	        TYPE j_1bbranc_,
       parvw          TYPE j_1bparvw,
       parid          TYPE j_1bparid,
       name1          TYPE name1_gp,
       nfenum	        TYPE j_1bnfnum9,
       matnr          TYPE matnr,
       maktx          TYPE maktx,
       nbm            TYPE steuc,
       charg          TYPE charg_d,
       cfop	          TYPE j_1bcfop,
       nr_quantidade2 TYPE j_1bnetqty,
       rowcolor(4)    TYPE c.
       INCLUDE STRUCTURE zdoc_nf_produtor.
TYPES: END OF zplac_zdoc_nf_produtor.

TYPES: BEGIN OF zplac_zdoc_rem_bl.
TYPES: pais	   TYPE landx,
       ds_tipo TYPE char30.
       INCLUDE STRUCTURE zdoc_rem_bl.
TYPES: END OF zplac_zdoc_rem_bl.

TYPES: BEGIN OF zplac_znom_conhec.
TYPES: pais	         TYPE landx,
       nr_quantidade TYPE j_1bnetqty,
       nr_utilizada  TYPE j_1bnetqty,
       nr_saldo      TYPE j_1bnetqty.
       INCLUDE STRUCTURE znom_conhec.
TYPES: END OF zplac_znom_conhec.

"Para cadastro de remessas *****************************************************

"Remessa
TYPES: BEGIN OF zplac_vinc_remessa.
TYPES:   vbeln            TYPE vbeln_vl,
         nr_registro_expo TYPE c LENGTH 14,
         numero_due       TYPE zsdt0170-numero_due.
         INCLUDE STRUCTURE znom_prog_reme.
TYPES: END OF zplac_vinc_remessa.

"Volumes de Produtor
TYPES BEGIN OF zplac_vinc_produtor.
TYPES: nr_qtd_possivel   TYPE j_1bnetqty,
       nr_qtd_vincular   TYPE j_1bnetqty,
       icone             TYPE c LENGTH 4,
       name1             TYPE name1_gp,
       stcd1             TYPE char18,
       uf                TYPE regio,
       munic             TYPE text60,
       nr_remetente2     TYPE j_1bnetqty,
       nr_efetivada      TYPE j_1bnetqty,
       nr_saldo_efetivar TYPE j_1bnetqty.
       INCLUDE STRUCTURE znom_remetente.
TYPES END OF zplac_vinc_produtor.

"*******************************************************************************
"*******************************************************************************

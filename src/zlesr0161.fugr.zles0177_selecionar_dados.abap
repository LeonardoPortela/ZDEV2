FUNCTION zles0177_selecionar_dados.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_PROP) TYPE  CHAR1
*"     REFERENCE(P_TERC) TYPE  CHAR1
*"     REFERENCE(P_CARRE) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(IT_SAIDA_CARREGA) TYPE  ANY
*"  TABLES
*"      T_TKNUM STRUCTURE  ZLESE0160 OPTIONAL
*"      T_TPLST STRUCTURE  ZLESE0160 OPTIONAL
*"      T_PLACA STRUCTURE  ZLESE0160 OPTIONAL
*"      T_SAIDA STRUCTURE  ZLEST0227
*"      T_ERDAT STRUCTURE  ZLESE0160 OPTIONAL
*"      I_T_SELECT TYPE  SBIWA_T_SELECT OPTIONAL
*"----------------------------------------------------------------------

  DATA: lit_likp TYPE TABLE OF likp.
  DATA: lit_vttp TYPE TABLE OF vttp.

  DATA: vl_knumv         TYPE vfkp-knumv,
        sl_konv          TYPE konv,
        it_prcd_elements TYPE TABLE OF prcd_elements,
        wa_prcd_elements TYPE prcd_elements.

  TABLES: vttk, vtpa, adrc, vttp, lips, makt, vfkp, bkpf, bsis, vbak, vbfa, j_1bnflin, j_1bnfdoc, zlest0002.

  TYPES: BEGIN OF ty_saida,
           tknum              TYPE vttk-tknum, " Doc. Transp com 0 a esquerda
           tknum1             TYPE vttk-tknum, " Doc. Transp
           fknum              TYPE vfkp-fknum, " Doc. Custo com 0 a esquerda
           fknum1             TYPE vfkp-fknum, " Doc. Custo
           tplst              TYPE vttk-tplst, " Filial
           erdat              TYPE vttk-erdat, " Data Documento
*           hora               TYPE zlest0039-hora, "Hora / 111592 CS2023000322 SIM4.0 - Inserir a hora na coluna Data Documento ZLES0177 PSA
           matnr              TYPE lips-matnr, " Cod. Produto
           matnr1             TYPE lips-matnr, " Cod. Produto zeros a esquerda
           maktx              TYPE makt-maktx, " Descrição Produto
           text1              TYPE vttk-text1, " Placa Cavalo
           text2              TYPE vttk-text2, " Carreta1
           text3              TYPE vttk-text3, " Carreta2
           text4              TYPE vttk-text4, " Carreta2
           placa_carreta      TYPE vttk-text1, " Placa Cavalo
           carreta1           TYPE vttk-text2, " Carreta1
           carreta2           TYPE vttk-text3, " Carreta2
           carreta3           TYPE vttk-text3, " Carreta3
           shtyp              TYPE vttk-shtyp, " Tp. Transporte
           tdlnr              TYPE vttk-tdlnr, " Agente Fretecom 0 a esquerda
           tdlnr1             TYPE vttk-tdlnr, " Agente Frete
           lifnr              TYPE vtpa-lifnr, " Origem - Código --- Código do parceiro PC com 0 a esquerda
           lifnr1             TYPE vtpa-lifnr, " Origem - Código --- Código do parceiro PC
           desc_origem        TYPE adrc-name1, " Origem - Descrição --- concatenar (ADRC-NAME1 - ADRC-CITY1) do parceiro PC
           kunnr              TYPE vtpa-kunnr, "Destino - Código --- Código do parceiro LR com 0 a esquerda
           kunnr1             TYPE vtpa-kunnr, "Destino - Código --- Código do parceiro LR
           desc_destino       TYPE adrc-name1, "Destino - Descrição --- concatenar (ADRC-NAME1 - ADRC-CITY1) do parceiro LR
           tipo               TYPE char15, " Tipo --- Próprio , Terceiros e Intercompany
           dt_protocolo       TYPE zib_cte_dist_ter-dt_protocolo,
           hr_protocolo       TYPE zib_cte_dist_ter-hr_protocolo,
           mwsbp              TYPE vfkp-mwsbp, " Imp_Receita
           ntgew              TYPE vfkp-netwr, " Kg Trasportados
           preco_tonelada_rs  TYPE vfkp-netwr, " Preço por Ton_R$
           netwr              TYPE vfkp-netwr, " Valor Total R$
           kzwi1              TYPE vfkp-netwr,
           taxa_dolar         TYPE p DECIMALS 4, " Tx_Dólar
           dmbe2              TYPE vfkp-netwr, " Valor Total USD
           bkpf_belnr         TYPE bkpf-belnr, " Doc Cont  Custo
           belnr2             TYPE bkpf-belnr, " Doc Cont  Receita
           docnum             TYPE j_1bnfdoc-docnum, " Docnum_Receita
           nfenum             TYPE j_1bnfdoc-nfenum, " Nr_Ct-e
           nfenum1            TYPE j_1bnfdoc-nfenum, " Nr_Ct-e

           adrnr              TYPE vtpa-adrnr,
           parvw              TYPE vtpa-parvw,
           xblnr              TYPE bkpf-xblnr,
           bukrs              TYPE bkpf-bukrs,
           gjahr              TYPE bkpf-gjahr,
           tp_veiculo         TYPE char01,
           vbak_vbeln         TYPE vbak-vbeln,
           vbfa_vbeln         TYPE vbfa-vbeln,
           vbak_bukrs_vf      TYPE vbak-bukrs_vf,
           vbak_auart         TYPE vbak-auart,
           vbfa_erdat         TYPE vbfa-erdat,
           vttp_vbeln         TYPE vttp-vbeln,
           grupo_veiculo      TYPE zlest0002-grupo,
           frota              TYPE zlest0002-frota,
           grupo_produto      TYPE lips-matwa,
           cod_motorista      TYPE vtpa-lifnr,  "DO PARCEIRO MT
           nome_motorista     TYPE adrc-name1, " DO PARCEIRO MT
           cpf_motorista      TYPE lfa1-stcd1,

           bezei              TYPE ttdst-bezei,  "Nome Filial
           munic_origem       TYPE adrc-city1,   " Origem do parceiro PC
           uf_origem          TYPE adrc-country, " UF do parceiro PC Origem
           munic_destino      TYPE adrc-city1,   "Destino do parceiro LR
           uf_destino         TYPE adrc-country, " UF do parceiro PC Destino
           ov_pedido          TYPE lips-vgbel,   "OV do pedido
           tp_ov              TYPE vbak-auart,   "Tipo de ordem de venda
           transgenese        TYPE vbak-kvgr3,   "
           dt_descarga        TYPE zlest0039-datachegada, " Data da descarga
           peso_descarga      TYPE zlest0039-pesotransb, "Peso transporte
           cod_tran_efetivo   TYPE zlest0039-transb_efetivo,  "Codigo do transporte efetivo
           descr_tran_efetivo TYPE kna1-name1, "Descrição transporte efetivo
           docnum_mdfe        TYPE zsdt0105-docnum_ref, "Documento referencia
           nr_mdfe            TYPE zsdt0102-nmdfe, "Numero da MDFE
           ordem_car          TYPE zsdt0001od-nr_ordem, "Ordem de carregamento
           dt_oc              TYPE zsdt0001od-dt_emissao, " Data de emissão
           cd_chave_cte       TYPE zib_cte_dist_n55-cd_chave_cte,   "Chave CTe.
           chave_nfe          TYPE zib_cte_dist_n55-n55_chave_acesso, "Chave de acesso NFe.

           kbetr              TYPE konv-kbetr, " Valor Pedágio - RJF
           vlr_frete          TYPE vfkp-netwr, " Valor Frete R$ - RJF
           hora_mdfe          TYPE uzeit, "RJF
           data_mdfe          TYPE datum, "RJF
         END OF ty_saida,

         BEGIN OF ty_saida_ordem_carregamento,
           nr_ordem             TYPE zsdt0001od-nr_ordem,            "Nr. Od  Carreg.
           tp_status            TYPE zsdt0001od-tp_status,           "Status_OD
           dt_emissao           TYPE zsdt0001od-dt_emissao,          "Dt_emissao_OD
           dt_validade          TYPE zsdt0001od-dt_validade,         "Dt_validade_OD
           id_bukrs             TYPE zsdt0001od-id_bukrs,            "Empresa
           id_branch            TYPE zsdt0001od-id_branch,           "Filial
           name                 TYPE j_1bbranch-name,                "Descr_filial
           id_bukrs_ag          TYPE zsdt0001od-id_bukrs_ag,         "Empresa_Transp
           id_branch_ag         TYPE zsdt0001od-id_branch_ag,        "Filial_Transp
           id_local_coleta      TYPE zsdt0001od-id_local_coleta,     "Cód_Coleta
           local_coleta_name1   TYPE  lfa1-name1,                    "Descr. Ponto Coleta
           local_coleta_ort01   TYPE  lfa1-ort01,                    "Munic. Ponto Coleta
           local_coleta_regio   TYPE  lfa1-regio,                    "UF_PC
           id_local_descarga    TYPE  zsdt0001od-id_local_descarga,  "Cód. Descarga
           descarga_kname1      TYPE  kna1-name1,                    "Desc. Descarga
           descarga_ort01       TYPE  kna1-ort01,                    "Munic. Descarga
           descarga_regio       TYPE  kna1-regio,                    "UF_Descarga
           id_local_destino     TYPE  zsdt0001od-id_local_destino,       "Cód. Destino
           local_destino_name1  TYPE  lfa1-name1,                    "Desc. Destino
           local_destino_ort01  TYPE  lfa1-ort01,                    "Munic. Destino
           local_destino_regio  TYPE  lfa1-regio,                    "UF_Destino
           doc_transp           TYPE  zsdt0001-doc_transp,           "Doc_transp
           tknum                TYPE  zsdt0001-tknum,                "Doc_custo
           erdat                TYPE  vttk-erdat,                    "Dt_doc_Transp
*           hora                 TYPE zlest0039-hora,                  "Hora / 111592 CS2023000322 SIM4.0 - Inserir a hora na coluna Data Documento ZLES0177 PSA
           peso_fiscal          TYPE  zsdt0001-peso_fiscal,          "Peso_embarque
           datatransb           TYPE  zlest0039-datatransb,          "Dt_descarga
           pesotransb           TYPE  zlest0039-pesotransb,          "Peso_descarga
           transb_efetivo       TYPE  zlest0039-transb_efetivo,      "Cód_transb_efetivo
           transb_efetivo_name1 TYPE  kna1-name1,                    "Descr. Transb_efetivo
           ds_placa_trator      TYPE  zsdt0001od-ds_placa_trator,    "Placa_Cavalo
           s_placa_reboq_1      TYPE  zsdt0001od-ds_placa_reboq_1,   "Carreta_1
           ds_placa_reboq_2     TYPE  zsdt0001od-ds_placa_reboq_2,   "Carreta_2
           ds_placa_reboq_3     TYPE  zsdt0001od-ds_placa_reboq_3,   "Carreta_3
           grupo                TYPE  zlest0002-grupo,               "Grupo
           frota                TYPE  zlest0002-frota,               "Frota
           id_motorista         TYPE  zsdt0001od-id_motorista,       "Cód+motorista
           motorista_stcd2      TYPE lfa1-stcd2,                     "CPF_motorista
           motorista_name1      TYPE lfa1-name1,                     "Nome_motorista
           nr_frete_comb        TYPE  zsdt0001od-nr_frete_comb,      "Frete_Combinado
           id_ordem             TYPE zsdt0001od-id_ordem,             "Nr_ordem
           cd_chave_cte         TYPE zib_cte_dist_n55-cd_chave_cte,   "Chave CTe.
           chave_nfe            TYPE zib_cte_dist_n55-n55_chave_acesso, "Chave de acesso NFe.
           nro_nf_prod          TYPE j_1bdocnum,
         END OF ty_saida_ordem_carregamento,


         BEGIN OF ty_zsdt0001od,
           nr_ordem          TYPE zsdt0001od-nr_ordem,
           dt_validade       TYPE zsdt0001od-dt_validade,
           dt_emissao        TYPE zsdt0001od-dt_emissao,
           id_bukrs          TYPE zsdt0001od-id_bukrs,
           id_branch         TYPE zsdt0001od-id_branch,
           id_bukrs_ag       TYPE zsdt0001od-id_bukrs_ag,
           id_branch_ag      TYPE zsdt0001od-id_branch_ag,
           id_local_coleta   TYPE zsdt0001od-id_local_coleta,
           id_local_destino  TYPE zsdt0001od-id_local_destino,
           id_local_descarga TYPE zsdt0001od-id_local_descarga,
           ds_placa_trator   TYPE zsdt0001od-ds_placa_trator,
           ds_placa_reboq_1  TYPE zsdt0001od-ds_placa_reboq_1,
           ds_placa_reboq_2  TYPE zsdt0001od-ds_placa_reboq_2,
           ds_placa_reboq_3  TYPE zsdt0001od-ds_placa_reboq_3,
           id_motorista      TYPE zsdt0001od-id_motorista,
           nr_frete_comb     TYPE zsdt0001od-nr_frete_comb,
           tp_status         TYPE zsdt0001od-tp_status,
           id_ordem          TYPE zsdt0001od-id_ordem,
         END OF ty_zsdt0001od,

         BEGIN OF ty_vttk, "PRINCIOAL
           tknum TYPE  vttk-tknum,
           shtyp TYPE  vttk-shtyp,
           tplst TYPE  vttk-tplst,
           tdlnr TYPE  vttk-tdlnr,
           text1 TYPE  vttk-text1,
           text2 TYPE  vttk-text2,
           text3 TYPE  vttk-text3,
           erdat TYPE  vttk-erdat,
           xblnr TYPE  bkpf-xblnr,
         END OF ty_vttk,

         BEGIN OF ty_likpx, "RJF
           vbeln  TYPE likp-vbeln,
           parid  TYPE j_1bnfdoc-parid,
           nfenum TYPE j_1bnfdoc-nfenum,
           series TYPE j_1bnfdoc-series,
         END OF ty_likpx,

         BEGIN OF ty_lfa1,
           lifnr TYPE lfa1-lifnr,
           stcd1 TYPE lfa1-stcd1,
         END OF ty_lfa1,

         BEGIN OF ty_zlest0002,
           pc_veiculo TYPE  zlest0002-pc_veiculo,
           grupo      TYPE  zlest0002-grupo,
           frota      TYPE  zlest0002-frota,
         END OF ty_zlest0002,

         BEGIN OF ty_vtpa, "parceiros  do  transporte
           kunnr TYPE vtpa-kunnr,
           lifnr TYPE vtpa-lifnr,
           adrnr TYPE vtpa-adrnr,
           parvw TYPE vtpa-parvw,
           vbeln TYPE vtpa-vbeln,
         END OF ty_vtpa,

         BEGIN OF ty_adrc, " nome dos  parceiros
           name1 TYPE adrc-name1,
           city1 TYPE adrc-city1,
         END OF ty_adrc,

         BEGIN OF ty_vttp, " Identificar material e quantidade transportada
           vbeln TYPE vttp-vbeln,
           tknum TYPE vttp-tknum,
         END OF ty_vttp,

         BEGIN OF ty_lips,
           matnr TYPE lips-matnr,
           "NTGEW TYPE LIPS-NTGEW,
           vbeln TYPE lips-vbeln,
           brgew TYPE lips-brgew,
           matwa TYPE lips-matwa,
           vgbel TYPE lips-vgbel,
           pstyv TYPE lips-pstyv,
           matkl TYPE lips-matkl,
           vgtyp TYPE lips-vgtyp,
         END OF ty_lips,

         BEGIN OF ty_makt, "Descritivo do  material
           maktx TYPE makt-maktx,
           matnr TYPE makt-matnr,
         END OF ty_makt,

         BEGIN OF ty_vfkp, "buscar  dados  de  custo e  valores  em BRL e impostos
           knumv TYPE vfkp-knumv,
           fknum TYPE vfkp-fknum,
           netwr TYPE vfkp-netwr,
           kzwi1 TYPE vfkp-kzwi1, "RJF
           mwsbp TYPE vfkp-mwsbp,
           rebel TYPE vfkp-rebel,
           fkpty TYPE vfkp-fkpty,
         END OF ty_vfkp,

         BEGIN OF ty_bkpf, "Buscar  valor em USD
           belnr TYPE bkpf-belnr,
           bukrs TYPE bkpf-bukrs,
           gjahr TYPE bkpf-gjahr,
         END OF ty_bkpf,

         BEGIN OF ty_bsis,
           dmbtr TYPE bsis-dmbtr,
           dmbe2 TYPE bsis-dmbe2,
         END OF ty_bsis,

         BEGIN OF ty_vbak, "Documento Contábil Receita
           vbeln    TYPE vbak-vbeln,
           bukrs_vf TYPE vbak-bukrs_vf,
           auart    TYPE vbak-auart,
         END OF ty_vbak,

         BEGIN OF ty_vbfa,
           vbeln TYPE char35,
           erdat TYPE vbfa-erdat,
           awkey TYPE bkpf-awkey,
         END OF ty_vbfa,

         BEGIN OF ty_refkey,
           refkey TYPE j_1brefkey,
         END OF ty_refkey,

         BEGIN OF ty_j_1bnflin, " Docnum da  Receita
           refkey TYPE j_1bnflin-refkey,
           docnum TYPE j_1bnflin-docnum,
         END OF ty_j_1bnflin,

         BEGIN OF ty_j_1bnfdoc,
           nfenum TYPE j_1bnfdoc-nfenum,
           docnum TYPE j_1bnfdoc-docnum,
           belnr  TYPE j_1bnfdoc-belnr,
         END OF ty_j_1bnfdoc,

         BEGIN OF ty_zsdt0241_aux,
           docnum    TYPE zsdt0241-docnum,
           chave_nfe TYPE zib_nfe_dist_ter-chave_nfe,
         END OF ty_zsdt0241_aux,

         BEGIN OF ty_likp,
           vbeln     TYPE likp-vbeln,
           berot     TYPE likp-berot,
           lifnr     TYPE likp-lifnr,
           lifex     TYPE likp-lifex,
           xblnr     TYPE likp-xblnr,
           tcode     TYPE likp-tcode,
           nr_nf     TYPE j_1bnfdoc-nfenum,
           serie_nf  TYPE j_1bnfdoc-series,
           serie_nf1 TYPE j_1bnfdoc-series,
           serie_nf2 TYPE j_1bnfdoc-series,  "*-IR190741-29.10.2024-#156620-JT
         END OF ty_likp.


  "DECLARA AS TABELAS TEMPORARIAS E AS WORK_AREAS PARA AS ESTRUTURAS DECLARADAS A CIMA
  DATA: it_refkey                   TYPE TABLE OF ty_refkey,
        it_saida                    TYPE TABLE OF ty_saida,
        it_zsdt0001                 TYPE TABLE OF zsdt0001,
        it_zsdt0001od               TYPE TABLE OF ty_zsdt0001od,
        it_saida_carregamento       TYPE TABLE OF ty_saida_ordem_carregamento,
        it_saida_aux                TYPE TABLE OF ty_saida,
        t_zsdt0001od                TYPE TABLE OF zsdt0001od,
        it_vtpa                     TYPE TABLE OF  ty_vtpa,
        it_sort_vtpa                TYPE SORTED TABLE OF ty_vtpa WITH NON-UNIQUE DEFAULT KEY,
        it_adrc                     TYPE TABLE OF  ty_adrc,
        it_vttp                     TYPE TABLE OF  ty_vttp,
        it_sort_vttp                TYPE SORTED TABLE OF ty_vttp WITH NON-UNIQUE DEFAULT KEY,
        it_lips                     TYPE TABLE OF  ty_lips,
        it_likpx                    TYPE TABLE OF  ty_likpx, "RJF
        wa_likpx                    TYPE ty_likpx,
        t_vbak                      TYPE TABLE OF  vbak,
        t_vbak_aux                  TYPE TABLE OF  vbak,
        t_vbfa                      TYPE TABLE OF  vbfa,
        t_vbfa_aux                  TYPE TABLE OF  vbfa,

**
        tg_vbfa_r                   TYPE TABLE OF  vbfa,
        tg_zlest0213_r              TYPE TABLE OF zlest0213,
        tg_j_1bnflin_r              TYPE TABLE OF  j_1bnflin,
        tg_j_1bnfe_active_r         TYPE TABLE OF  j_1bnfe_active,

**
        tg_vbfa_rt                  TYPE TABLE OF  vbfa,
        tg_j_1bnflin_rt             TYPE TABLE OF  j_1bnflin,
        tg_j_1bnfe_active_rt        TYPE TABLE OF  j_1bnfe_active,

**
        tg_vbfa_a                   TYPE TABLE OF  vbfa,
        tg_j_1bnflin_a              TYPE TABLE OF  j_1bnflin,
        tg_j_1bnflin_aux_a          TYPE TABLE OF  j_1bnflin,
        tg_likp_a                   TYPE TABLE OF  ty_likp,
        tg_likp_ux                  TYPE TABLE OF  ty_likp,
        tg_zfiwrt0008_a             TYPE TABLE OF  zfiwrt0008,
        tg_j_1bnfdoc_a              TYPE TABLE OF  j_1bnfdoc,
        tg_j_1bnfe_active_a         TYPE TABLE OF  j_1bnfe_active,
        tg_j_1bnfe_active_aux_a     TYPE TABLE OF  j_1bnfe_active,
        tg_zsdt0241_aux             TYPE TABLE OF ty_zsdt0241_aux,
        it_zsdt0241_aux             TYPE TABLE OF zsdt0241,
        it_zmmt_zgr_docs            TYPE TABLE OF zmmt_ee_zgr_docs,
        t_j_1bnflin                 TYPE TABLE OF j_1bnflin,
        t_j_1bnflin_aux             TYPE TABLE OF j_1bnflin,
        t_j_1bnfdoc_aux             TYPE TABLE OF  j_1bnfdoc,
        t_j_1bnfe_active_aux        TYPE TABLE OF j_1bnfe_active,
        it_makt                     TYPE TABLE OF  ty_makt,
        it_vfkp                     TYPE TABLE OF  ty_vfkp,
        it_bkpf                     TYPE TABLE OF  ty_bkpf,
        it_bsis                     TYPE TABLE OF  ty_bsis,
        it_vbak                     TYPE TABLE OF  ty_vbak,
        it_vbfa                     TYPE TABLE OF  ty_vbfa,
        it_j_1bnflin                TYPE TABLE OF  ty_j_1bnflin,
        it_j_1bnfdoc                TYPE TABLE OF  ty_j_1bnfdoc,
        t_j_1bnfdoc                 TYPE TABLE OF  j_1bnfdoc,
*        it_lfa1                     TYPE TABLE OF ty_lfa1,
        it_lfa1                     TYPE TABLE OF lfa1,
        wa_saida                    TYPE ty_saida,
        wa_saida_ordem_carregamento TYPE ty_saida_ordem_carregamento,
        wa_zsdt0001od               TYPE ty_zsdt0001od,
        wa_vttp                     TYPE ty_vttp,
        wa_lips                     TYPE ty_lips,
        wa_makt                     TYPE ty_makt,
        wa_vfkp                     TYPE ty_vfkp,
        wa_bkpf                     TYPE ty_bkpf,
        wa_bsis                     TYPE ty_bsis,
        wa_vbak                     TYPE ty_vbak,
        wa_vbfa                     TYPE ty_vbfa,
        wa_j_1bnflin                TYPE ty_j_1bnflin,
        wa_j_1bnfdoc                TYPE ty_j_1bnfdoc,
        v_name1                     LIKE adrc-name1,
        v_city1                     LIKE adrc-city1,
        v_descricao                 TYPE string,
        cpf_motorista(14)           TYPE c,
        lw_zlest0227                TYPE zlest0227.

  DATA: gs_variant   TYPE disvariant, " é para poder escolher o layout
        variante     LIKE disvariant,
        gs_variant_c TYPE disvariant,
        vs_refkey    TYPE j_1bnflin-refkey.

  DATA: r_refkey     TYPE RANGE OF j_1brefkey,
        vg_refkey    TYPE j_1brefkey,
        r_refkey_aux TYPE RANGE OF j_1brefkey.

  DATA: r_vbeln     TYPE RANGE OF vbeln,
        r_vbeln_aux TYPE RANGE OF vbeln.

  DATA: t_zsdt0102_rt TYPE TABLE OF zsdt0102,  "*-IR212606-18.12.2024-#160894-JT-inicio
        t_zsdt0105_rt TYPE TABLE OF zsdt0105,  "*-IR212606-18.12.2024-#160894-JT-inicio
        t_zsdt0102_a  TYPE TABLE OF zsdt0102,  "*-IR212606-18.12.2024-#160894-JT-inicio
        t_zsdt0105_a  TYPE TABLE OF zsdt0105.  "*-IR212606-18.12.2024-#160894-JT-inicio

* ALV

  DATA:
    g_custom_container TYPE REF TO cl_gui_custom_container,
    dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
    dg_parent_1        TYPE REF TO cl_gui_container,
    dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
    dg_parent_2        TYPE REF TO cl_gui_container,
    dg_parent_2a       TYPE REF TO cl_gui_container,
    dg_parent_alv      TYPE REF TO cl_gui_container,
    picture            TYPE REF TO cl_gui_picture,
    ctl_alv            TYPE REF TO cl_gui_alv_grid,
    dg_dyndoc_id       TYPE REF TO cl_dd_document,
    table_element      TYPE REF TO cl_dd_table_element,
    column             TYPE REF TO cl_dd_area,
    table_element2     TYPE REF TO cl_dd_table_element,
    column_1           TYPE REF TO cl_dd_area,
    dg_html_cntrl      TYPE REF TO cl_gui_html_viewer,
    it_exclude_fcode   TYPE ui_functions,
    wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
    gs_layout          TYPE lvc_s_layo,
    it_fieldcatalog    TYPE lvc_t_fcat,
    wa_fieldcatalog    TYPE lvc_s_fcat,
    it_sort            TYPE lvc_t_sort,
    ls_stable          TYPE lvc_s_stbl,
    str                TYPE REF TO data,
    ob_timer           TYPE REF TO cl_gui_timer,
    frota              TYPE zlest0002-frota,
    grupo_veiculo      TYPE zlest0002-grupo.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_butxt                TYPE t001-butxt,
        vl_dates1               TYPE char10,
        vl_dates2               TYPE char10.

  DATA: vg_repid        LIKE sy-repid,
        vl_message(150) TYPE c,
        vg_veiculo      TYPE char20,
        vg_variant      TYPE disvariant.


  TYPES: ty_rg_text1    TYPE RANGE OF vttk-text1. " Placa Cavalo
  TYPES: ty_rg_tknum    TYPE RANGE OF vttk-tknum. " Doc. Transp com 0 a esquerda
  TYPES: ty_rg_vbeln    TYPE RANGE OF vbak-vbeln. " Ordem de venda
  TYPES: ty_rg_bukrs_vf TYPE RANGE OF vbak-bukrs_vf. " Empresa
  TYPES: ty_rg_docnum   TYPE RANGE OF j_1bnflin-docnum. " Docnum
  DATA: zcl_faturamento TYPE REF TO zcl_faturamento.
  DATA: message    TYPE itex132.

  FREE: it_saida, it_saida_aux.

  FREE zcl_faturamento.

  DATA: p_erdat       TYPE RANGE OF vttk-erdat,
        wa_i_t_select TYPE sbiwa_s_select,
        ws_erdat      TYPE zlese0160,
*        l_erdat LIKE LINE OF p_erdat,
        p_tknum       TYPE RANGE OF vttk-tknum, " DOC. TRANSP.
        l_tknum       LIKE LINE OF p_tknum, " DOC. TRANSP.
        p_tplst       TYPE RANGE OF vttk-tplst, " FILIAL
        l_tplst       LIKE LINE OF p_tplst, " FILIAL
        p_placa       TYPE RANGE OF zlest0002-pc_veiculo, "placa
        l_placa       LIKE LINE OF p_placa, "placa
        w_likp_ux     TYPE ty_likp,           "*-IR190741-29.10.2024-#156620-JT-inicio
        w_j_1bnfdoc   TYPE j_1bnfdoc,         "*-IR190741-29.10.2024-#156620-JT-inicio
        w_zlest0110   TYPE zlest0110,         "*-IR190741-29.10.2024-#156620-JT-inicio
        w_zsdt0105    TYPE zsdt0105,          "*-IR190741-29.10.2024-#156620-JT-inicio
        w_zsdt0102    TYPE zsdt0102,          "*-IR190741-29.10.2024-#156620-JT-inicio
        w_zsdt0241    TYPE zsdt0241,          "*-IR190741-29.10.2024-#156620-JT-inicio
        w_zfiwrt0008  TYPE zfiwrt0008.        "*-IR212606-24.12.2024-#161504-JT-inicio

  DATA: obj_zcl_util TYPE REF TO zcl_util.
  CREATE OBJECT obj_zcl_util.

*  LOOP AT t_tknum INTO DATA(lw_t_tknum).
*    l_tknum-sign   =  lw_t_tknum-sign.
*    l_tknum-option =  lw_t_tknum-opt.
*    l_tknum-low    =  lw_t_tknum-low.
*    l_tknum-high   =  lw_t_tknum-high.
*
*    APPEND l_tknum TO p_tknum.
*    CLEAR l_tknum.
*  ENDLOOP.

*  LOOP AT t_tplst INTO DATA(lw_t_tplst).
*    l_tplst-sign   =  lw_t_tplst-sign.
*    l_tplst-option =  lw_t_tplst-opt.
*    l_tplst-low    =  lw_t_tplst-low.
*    l_tplst-high   =  lw_t_tplst-high.
*
*    APPEND l_tplst TO p_tplst.
*    CLEAR l_tplst.
*  ENDLOOP.

*  LOOP AT t_placa INTO DATA(lw_t_placa).
*    l_placa-sign   =  lw_t_placa-sign.
*    l_placa-option =  lw_t_placa-opt.
*    l_placa-low    =  lw_t_placa-low.
*    l_placa-high   =  lw_t_placa-high.
*
*    APPEND l_placa TO p_placa.
*    CLEAR l_placa.
*  ENDLOOP.

  LOOP AT i_t_select INTO wa_i_t_select.
    CASE wa_i_t_select-fieldnm.
      WHEN 'erdat'.
        APPEND VALUE #( sign = wa_i_t_select-sign option = wa_i_t_select-option low = wa_i_t_select-low high = wa_i_t_select-high ) TO p_erdat.

      WHEN 'placa'.
        APPEND VALUE #( sign = wa_i_t_select-sign option = wa_i_t_select-option low = wa_i_t_select-low high = wa_i_t_select-high ) TO p_placa.

      WHEN 'tplst'.
        APPEND VALUE #( sign = wa_i_t_select-sign option = wa_i_t_select-option low = wa_i_t_select-low high = wa_i_t_select-high ) TO p_tplst.

      WHEN 'tknum'.
        APPEND VALUE #( sign = wa_i_t_select-sign option = wa_i_t_select-option low = wa_i_t_select-low high = wa_i_t_select-high ) TO p_tknum.
    ENDCASE.
  ENDLOOP. "i_t_select

  CREATE OBJECT zcl_faturamento.

  IF p_carre EQ 'X'.

    vl_message = 'Aguarde, selecionando dados ordem carregamento'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 99
        text       = vl_message.

    IF p_tknum IS NOT INITIAL.
      MESSAGE 'Não é possível selecionar ordens de carregamento informando Doc. de transporte' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT DISTINCT
      nr_ordem
      dt_validade
      dt_emissao
      id_bukrs
      id_branch
      id_bukrs_ag
      id_branch_ag
      id_local_coleta
      id_local_destino
      id_local_descarga
      ds_placa_trator
      ds_placa_reboq_1
      ds_placa_reboq_2
      ds_placa_reboq_3
      id_motorista
      nr_frete_comb
      tp_status
      id_ordem
    FROM
      zsdt0001od
    INTO TABLE it_zsdt0001od
    WHERE
      dt_emissao IN p_erdat AND
      id_branch IN p_tplst AND
      ds_placa_trator IN p_placa.

    SORT it_zsdt0001od BY nr_ordem ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0001od COMPARING ALL FIELDS.

    IF it_zsdt0001od[] IS NOT INITIAL.

      "- Busca dados parceiro ponto de coleta
      SELECT DISTINCT
       name1,
        ort01,
        regio,
        lifnr
     FROM
        lfa1
     INTO TABLE
        @DATA(it_lfa1_coleta)
     FOR ALL ENTRIES IN
        @it_zsdt0001od
     WHERE
        lifnr EQ @it_zsdt0001od-id_local_coleta.

      SORT it_lfa1_coleta BY lifnr ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_lfa1_coleta COMPARING ALL FIELDS.

      "Busca dados do local de Descarga
      SELECT DISTINCT
        name1,
         ort01,
         regio,
         kunnr
      FROM
         kna1
      INTO TABLE
         @DATA(it_kna1_descarga)
      FOR ALL ENTRIES IN
         @it_zsdt0001od
      WHERE
         kunnr EQ @it_zsdt0001od-id_local_descarga.

      SORT it_kna1_descarga BY kunnr ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_kna1_descarga COMPARING ALL FIELDS.

      "Busca dados do local de destino
      SELECT DISTINCT
       name1,
        ort01,
        regio,
        lifnr
     FROM
        lfa1
     INTO TABLE
        @DATA(it_lfa1_destino)
     FOR ALL ENTRIES IN
        @it_zsdt0001od
     WHERE
        lifnr EQ @it_zsdt0001od-id_local_destino.

      SORT it_lfa1_destino BY lifnr ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_lfa1_destino COMPARING ALL FIELDS.

      "Ajuste Dump Range PSA

      TYPES: BEGIN OF ty_idordem,
               id TYPE zsdt0001od-id_ordem,
             END OF ty_idordem.

      DATA: it_ordem TYPE STANDARD TABLE OF ty_idordem WITH HEADER LINE.

      LOOP AT it_zsdt0001od ASSIGNING FIELD-SYMBOL(<it_zsdt0001od>).
        it_ordem-id = <it_zsdt0001od>-id_ordem.
        APPEND it_ordem.
      ENDLOOP.

      SORT it_ordem BY id ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_ordem COMPARING id.

*      DATA: lr_idorder_od TYPE RANGE OF zsdt0001od-id_ordem.
*      CLEAR:lr_idorder_od.
*      lr_idorder_od[] = VALUE #( FOR wa_od IN it_zsdt0001od WHERE ( id_ordem <> '' ) ( option = 'EQ' sign = 'I' low = wa_od-id_ordem ) ).
*      SORT lr_idorder_od BY low ASCENDING.
*      DELETE ADJACENT DUPLICATES FROM lr_idorder_od COMPARING low.


      "Busca dados de faturamento da ordem de carregamento
      SELECT DISTINCT
        id_ordem,
        doc_transp,
        fknum,
        peso_fiscal,
        doc_rem,
        fatura_prod,
        nro_nf_prod
      FROM
        zsdt0001
     INTO TABLE
         @DATA(it_zsdt0001_faturamento)
        FOR ALL ENTRIES IN @it_ordem[]
      WHERE tp_movimento = 'S'
        AND id_ordem = @it_ordem-id.
      "id_ordem IN @lr_idorder_od.

      IF sy-subrc IS INITIAL. "RJF
        SORT it_zsdt0001_faturamento BY doc_transp ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_zsdt0001_faturamento COMPARING ALL FIELDS.
      ELSE. "RJF

* Busca Nr do aviso

*      Selecione
*      ZLEST0108-ID_ORDEM =  it_zsdt0001od-id_ordem
*      pegar
*      ZLEST0108-VBELN
*      ZLEST0108-TKNUM guardar em it_zsdt0001od_doc-transp
*      ZLEST0108-FKNUM guardar em it_zsdt0001od_fknum



        SELECT id_ordem, vbeln, doc_transp, fknum
          FROM zlest0108
          INTO TABLE @DATA(it_zlest0108)
          FOR ALL ENTRIES IN @it_ordem[]
          WHERE id_ordem EQ @it_ordem-id.
*
*   Busca dos dados do aviso
*      Selecione
*      LIKP-VBELN = ZLEST0108-VBELN
*      pegar
*      LIKP-LIFNR
*      LIKP-LIFEX
*

        IF it_zlest0108 IS NOT INITIAL.
          SELECT vbeln, lifnr, lifex
            FROM likp
            INTO TABLE @DATA(it_likp)
            FOR ALL ENTRIES IN @it_zlest0108
            WHERE vbeln EQ @it_zlest0108-vbeln.

          IF sy-subrc IS INITIAL AND it_likp IS NOT INITIAL.

            LOOP AT it_likp INTO DATA(wa_likp).

              IF wa_likp-lifex IS NOT INITIAL.
*                FIND '-' IN wa_likp-lifex MATCH COUNT DATA(lv_found).
*               FIND 'FIND' IN v_text MATCH OFFSET v_off MATCH LENGTH data(v_len).
                FIND '-' IN wa_likp-lifex MATCH OFFSET DATA(lv_found) MATCH LENGTH DATA(v_len).

                IF lv_found IS NOT INITIAL.
                  wa_likpx-nfenum  = wa_likp-lifex(lv_found).
                  lv_found = lv_found + 1.
                  wa_likpx-series  = wa_likp-lifex+lv_found(3).
                ELSE.
                  wa_likpx-nfenum  = wa_likp-lifex(9).
                ENDIF.
              ENDIF.

*            wa_likpx-series  = wa_likp-lifex.
              wa_likpx-vbeln   = wa_likp-vbeln.
              wa_likpx-parid   = wa_likp-lifnr.
*            wa_likpx-nfenum  = wa_likp-lifex.
              APPEND wa_likpx TO it_likpx.
              CLEAR wa_likpx.

            ENDLOOP.

          ENDIF.
        ENDIF.

*   Busca do docnum da entrada
*      Selecione
*      J_1BNFDOC-PARID =  LIKP-LIFNR
*      J_1BNFDOC-NFENUM = LIKP-LIFEX (os caracteres antes do hífen - inclusive os zeros a esquerda)
*      J_1BNFDOC-SERIES = LIKP-LIFEX (os caracteres após o hífen)
*      J_1BNFDOC-CANCEL = "vazio"
*      pegar
*      J_1BNFDOC-docnum, guardar em it_zsdt0001od_docnum_nfe ( pode ser que o campo correto a salvar seja it_ZSDT0001od_nr_nf_prod)
        SELECT docnum, parid, nfenum, series, cancel
          FROM j_1bnfdoc
          INTO TABLE @DATA(it_j_1bnfdocc)
          FOR ALL ENTRIES IN @it_likpx
          WHERE parid  = @it_likpx-parid
            AND nfenum = @it_likpx-nfenum
            AND series = @it_likpx-series
            AND cancel = ''.

      ENDIF.  "RJF

      "...Compara as ordens do resultado it_zsdt0001_faturamento diferentes it_zsdt0001od.

      DATA: lr_idorder_fat TYPE RANGE OF zsdt0001od-id_ordem.
      DATA: lr_idorder_filtro TYPE RANGE OF zsdt0001od-id_ordem.

      CLEAR: lr_idorder_fat,lr_idorder_filtro.

      lr_idorder_fat[] = VALUE #( FOR wa_fat IN it_zsdt0001_faturamento ( option = 'EQ' sign = 'I' low = wa_fat-id_ordem ) ).
      SORT lr_idorder_fat BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_idorder_fat.

      lr_idorder_filtro[] = VALUE #( FOR wa_fil IN it_zsdt0001od WHERE ( id_ordem NOT IN lr_idorder_fat ) ( option = 'EQ' sign = 'I' low = wa_fil-id_ordem ) ).
      SORT lr_idorder_filtro BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_idorder_filtro.


      TYPES: BEGIN OF aux_fat,
               id_ordem   TYPE zsdt0001-id_ordem,
               doc_transp TYPE zsdt0001-doc_transp,
               fknum      TYPE zsdt0001-fknum,
             END OF aux_fat.

      DATA: aux_vttk TYPE STANDARD TABLE OF aux_fat.

      IF lr_idorder_filtro[] IS NOT INITIAL.
        SELECT DISTINCT
                  a~id_ordem,
                  a~tknum AS doc_transp,
                  b~fknum
                FROM
                   vttk AS a
          LEFT JOIN vfkp AS b ON b~rebel = a~tknum AND b~fkpty = 'Z001'
                INTO TABLE @aux_vttk
                  WHERE a~id_ordem IN @lr_idorder_filtro.

        SORT aux_vttk BY doc_transp ASCENDING.
        DELETE ADJACENT DUPLICATES FROM aux_vttk COMPARING ALL FIELDS.

        APPEND LINES OF aux_vttk[] TO it_zsdt0001_faturamento.



      ENDIF.
      "Fim PSA


      IF it_zsdt0001_faturamento[] IS NOT INITIAL.
        "Busca data de criação da VT
        SELECT DISTINCT
          erdat,
          tknum,
          tplst
        FROM
           vttk
        INTO TABLE
            @DATA(it_vttk)
         FOR ALL ENTRIES IN
            @it_zsdt0001_faturamento
         WHERE
            tknum  EQ @it_zsdt0001_faturamento-doc_transp.

        SORT it_vttk BY tknum ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_vttk COMPARING ALL FIELDS.

        IF it_vttk IS NOT INITIAL.
          "Seleção Nome Filial.
          SELECT DISTINCT * FROM ttds
            INTO TABLE @DATA(t_ttds)
            FOR ALL ENTRIES IN  @it_vttk
            WHERE tplst EQ @it_vttk-tplst.

          SORT t_ttds BY tplst ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_ttds COMPARING ALL FIELDS.

        ENDIF.

        "Busca  data descarga, peso de descarga e transbordo efetivo
        SELECT DISTINCT
          pontotransb,
          datatransb,
          pesotransb,
          datachegada,
          pesochegada,
          transb_efetivo,
          docnum
        FROM
          zlest0039
        INTO TABLE
            @DATA(it_zlest0039)
         FOR ALL ENTRIES IN
            @it_zsdt0001_faturamento
         WHERE
            docnum  EQ @it_zsdt0001_faturamento-nro_nf_prod.

        SORT it_zlest0039 BY docnum ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_zlest0039 COMPARING ALL FIELDS.

        "Busca descrição Transbordo Efetivo
        IF it_zlest0039 IS NOT INITIAL.
          SELECT DISTINCT
            name1,
            kunnr
          FROM
            kna1
          INTO TABLE
            @DATA(it_kna1_transb_efetivo)
          FOR ALL ENTRIES IN
            @it_zlest0039
          WHERE
            kunnr EQ @it_zlest0039-transb_efetivo.

          SORT it_kna1_transb_efetivo BY name1 kunnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_kna1_transb_efetivo COMPARING ALL FIELDS.

        ENDIF.

      ENDIF.

      "Busca dados frota e grupo
      SELECT DISTINCT
        grupo,
        frota,
        pc_veiculo
      FROM
        zlest0002
      INTO TABLE
          @DATA(it_zlest0002)
       FOR ALL ENTRIES IN
          @it_zsdt0001od
       WHERE
          pc_veiculo EQ @it_zsdt0001od-ds_placa_trator.

      SORT it_zlest0002 BY grupo frota pc_veiculo ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_zlest0002 COMPARING ALL FIELDS.

      "Busca dados Motorista
      SELECT DISTINCT
        name1,
        stcd2,
        lifnr
      FROM
        lfa1
      INTO TABLE
          @DATA(it_lfa1_motorista)
       FOR ALL ENTRIES IN
          @it_zsdt0001od
       WHERE
          lifnr EQ @it_zsdt0001od-id_motorista.

      SORT it_lfa1_motorista BY name1 stcd2 lifnr ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_lfa1_motorista COMPARING ALL FIELDS.

      "Busca descrição filial
      SELECT DISTINCT
        name,
        bukrs,
        branch
      FROM
        j_1bbranch
      INTO TABLE
          @DATA(it_j_1bbranch)
       FOR ALL ENTRIES IN
          @it_zsdt0001od
       WHERE
          bukrs EQ @it_zsdt0001od-id_bukrs AND
          branch EQ @it_zsdt0001od-id_branch.

      SORT it_j_1bbranch BY name bukrs branch ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_j_1bbranch COMPARING ALL FIELDS.

    ENDIF.

    "Seleção dados para pegar chave cte.

    FREE: t_vbak_aux.
    IF it_vttk IS NOT INITIAL.
      SELECT DISTINCT * FROM  vbak INTO TABLE t_vbak_aux
        FOR ALL ENTRIES IN it_vttk
        WHERE tknum EQ  it_vttk-tknum.

      SORT t_vbak_aux ASCENDING.
      DELETE ADJACENT DUPLICATES FROM t_vbak_aux COMPARING ALL FIELDS.

      IF t_vbak_aux IS NOT INITIAL.
        "Seleção dados para pegar chave cte.
        FREE: t_vbfa_aux.
        SELECT DISTINCT * FROM  vbfa INTO TABLE t_vbfa_aux
          FOR ALL ENTRIES IN t_vbak_aux
          WHERE vbelv EQ  t_vbak_aux-vbeln
            AND vbtyp_n = 'M'
            AND vbtyp_v = 'C'.

        SORT t_vbfa_aux ASCENDING.
        DELETE ADJACENT DUPLICATES FROM t_vbfa_aux COMPARING ALL FIELDS.

        IF t_vbfa_aux IS NOT INITIAL.
          FREE: it_refkey.
          it_refkey = VALUE #( FOR l IN t_vbfa_aux ( refkey  = l-vbeln ) ).
          IF it_refkey IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnflin INTO TABLE t_j_1bnflin_aux
              FOR ALL ENTRIES IN it_refkey
              WHERE refkey EQ it_refkey-refkey.

            SORT t_j_1bnflin_aux ASCENDING.
            DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING ALL FIELDS.

            IF t_j_1bnflin_aux IS NOT INITIAL.
              SELECT DISTINCT * FROM j_1bnfdoc INTO TABLE t_j_1bnfdoc_aux
                FOR ALL ENTRIES IN t_j_1bnflin_aux
              WHERE docnum = t_j_1bnflin_aux-docnum
                  AND cancel NE abap_true.

              SORT t_j_1bnfdoc_aux ASCENDING.
              DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc_aux COMPARING ALL FIELDS.

              IF t_j_1bnfdoc_aux IS NOT INITIAL.
                SELECT DISTINCT * FROM j_1bnfe_active INTO TABLE t_j_1bnfe_active_aux
                FOR ALL ENTRIES IN t_j_1bnfdoc_aux
                WHERE docnum EQ t_j_1bnfdoc_aux-docnum.

                SORT t_j_1bnfe_active_aux ASCENDING.
                DELETE ADJACENT DUPLICATES FROM t_j_1bnfe_active_aux COMPARING ALL FIELDS.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    vl_message = 'Aguarde, preparando dados ordem carregamento'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 99
        text       = vl_message.

    IF p_tknum IS NOT INITIAL.
      MESSAGE 'Não é possível selecionar ordens de carregamento informando Doc. de transporte' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    LOOP AT it_zsdt0001od[] INTO wa_zsdt0001od.

      TRY.
          zcl_faturamento->zif_faturamento~get_tipo_veiculo(
            EXPORTING
              i_placa = wa_zsdt0001od-ds_placa_trator
              "I_TKNUM = I_TKNUM
            IMPORTING
              e_tipo  = DATA(e_tipo_veiculo) ).
          "E_PROPRIETARIO = DATA(E_PROPRIETARIO) ).
        CATCH zcx_error .

      ENDTRY.

      wa_saida_ordem_carregamento-id_ordem = wa_zsdt0001od-id_ordem. "RJF

      IF e_tipo_veiculo EQ 'P'.


        READ TABLE it_zsdt0001_faturamento INTO DATA(wa_zsdt0001) WITH KEY id_ordem  = wa_zsdt0001od-id_ordem.
        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-nro_nf_prod = wa_zsdt0001-nro_nf_prod.
          wa_saida_ordem_carregamento-doc_transp = wa_zsdt0001-doc_transp.
          wa_saida_ordem_carregamento-tknum = wa_zsdt0001-fknum.

* Ini - RJF - CS2023000859 - ZLES0177 - Opção : Ordem Carregamento - Status
          READ TABLE it_zlest0108 INTO DATA(wa_zlest0108) WITH KEY id_ordem  = wa_zsdt0001od-id_ordem.
          IF sy-subrc IS INITIAL.
            wa_saida_ordem_carregamento-doc_transp = wa_zlest0108-doc_transp.
            wa_saida_ordem_carregamento-tknum = wa_zlest0108-fknum.

            READ TABLE it_likpx INTO wa_likpx WITH KEY vbeln = wa_zlest0108-vbeln. "RFJ ult
            IF sy-subrc IS INITIAL.

              READ TABLE it_j_1bnfdocc INTO DATA(wa_j_1bnfdocc) WITH KEY parid = wa_likpx-parid
                                                                         nfenum = wa_likpx-nfenum
                                                                         series = wa_likpx-series.
              IF sy-subrc IS INITIAL.
                wa_zsdt0001-nro_nf_prod = wa_j_1bnfdocc-docnum.
                wa_saida_ordem_carregamento-nro_nf_prod = wa_zsdt0001-nro_nf_prod.
              ENDIF.
            ENDIF.
          ENDIF.
* Fim - RJF - CS2023000859 - ZLES0177 - Opção : Ordem Carregamento - Status

          wa_saida_ordem_carregamento-peso_fiscal = wa_zsdt0001-peso_fiscal.

          READ TABLE it_vttk INTO DATA(wa_vttk) WITH KEY tknum  = wa_zsdt0001-doc_transp.
          IF sy-subrc EQ 0.

            wa_saida_ordem_carregamento-erdat = wa_vttk-erdat.

            "Dados chave cte.
            READ TABLE t_vbak_aux INTO DATA(ls_vbak) WITH KEY tknum =  wa_vttk-tknum.
            IF sy-subrc EQ 0.
              READ TABLE t_vbfa_aux INTO DATA(ls_vbfa) WITH KEY vbelv      =  ls_vbak-vbeln.
              IF sy-subrc EQ 0.
                READ TABLE t_j_1bnflin_aux INTO DATA(ls_j_1bnflin) WITH KEY refkey = ls_vbfa-vbeln.
                IF sy-subrc EQ 0.
                  READ TABLE t_j_1bnfdoc_aux INTO DATA(ls_j_1bnfdoc) WITH KEY docnum = ls_j_1bnflin-docnum.
                  IF sy-subrc EQ 0.
                    READ TABLE t_j_1bnfe_active_aux INTO DATA(ls_1bnfe_active) WITH KEY docnum = ls_j_1bnfdoc-docnum.
                    IF sy-subrc EQ 0.
                      wa_saida_ordem_carregamento-cd_chave_cte = ls_1bnfe_active-regio &&
                                                 ls_1bnfe_active-nfyear &&
                                                 ls_1bnfe_active-nfmonth &&
                                                 ls_1bnfe_active-stcd1 &&
                                                 ls_1bnfe_active-model &&
                                                 ls_1bnfe_active-serie &&
                                                 ls_1bnfe_active-nfnum9 &&
                                                 ls_1bnfe_active-docnum9 &&
                                                 ls_1bnfe_active-cdv.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDIF.

          ENDIF.
          READ TABLE it_zlest0039 INTO DATA(wa_zlest0039) WITH KEY docnum  = wa_zsdt0001-nro_nf_prod.
          IF sy-subrc EQ 0.

            IF wa_zlest0039-pontotransb IS NOT INITIAL.

              wa_saida_ordem_carregamento-datatransb = wa_zlest0039-datatransb.
              wa_saida_ordem_carregamento-pesotransb = wa_zlest0039-pesotransb.

            ELSE.

              wa_saida_ordem_carregamento-datatransb = wa_zlest0039-datachegada.
              wa_saida_ordem_carregamento-pesotransb = wa_zlest0039-pesochegada.

            ENDIF.

            wa_saida_ordem_carregamento-transb_efetivo = wa_zlest0039-transb_efetivo.

            READ TABLE it_kna1_transb_efetivo INTO DATA(wa_kna1_transb_efetivo) WITH KEY kunnr  = wa_zlest0039-transb_efetivo.
            IF sy-subrc EQ 0.

              wa_saida_ordem_carregamento-transb_efetivo_name1 = wa_kna1_transb_efetivo-name1.

            ENDIF.

          ENDIF.

        ENDIF.

        wa_saida_ordem_carregamento-nr_ordem =   wa_zsdt0001od-nr_ordem.

* Ini - RJF - CS2023000859 - ZLES0177 - Opção : Ordem Carregamento - Status
        IF wa_zsdt0001od-tp_status NE 'CA'.

          IF wa_zsdt0001-nro_nf_prod  IS NOT INITIAL AND
          wa_saida_ordem_carregamento-doc_transp IS NOT INITIAL AND
          wa_saida_ordem_carregamento-tknum IS NOT INITIAL.
            wa_saida_ordem_carregamento-tp_status = 'FE'. "- fechada
          ENDIF.

*          se it_zsdt0001od_docnum_nfe is not initial e
*          it_zsdt0001od_doc-transp is not initial e
*          it_zsdt0001od_fknum is initial
*           então
*          wa_zsdt0001od-tp_status = "SI" - Sem VI.

          IF wa_zsdt0001-nro_nf_prod  IS NOT INITIAL AND
          wa_saida_ordem_carregamento-doc_transp IS NOT INITIAL AND
          wa_saida_ordem_carregamento-tknum IS INITIAL.
            wa_saida_ordem_carregamento-tp_status = 'SI'. "-Sem VI.
          ENDIF.

*          se it_zsdt0001od_docnum_nfe is not initial e
*          it_zsdt0001od_doc-transp is initial e
*          it_zsdt0001od_fknum is initial
*           então
*          wa_zsdt0001od-tp_status = "ST" - Sem VT e VI.

          IF wa_zsdt0001-nro_nf_prod  IS NOT INITIAL AND
          wa_saida_ordem_carregamento-doc_transp IS INITIAL AND
          wa_saida_ordem_carregamento-tknum IS INITIAL.
            wa_saida_ordem_carregamento-tp_status = 'ST'. "- Sem VT e VI.
          ENDIF.

*          se it_zsdt0001od_docnum_nfe is initial e
*          it_zsdt0001od_doc-transp is initial e
*          it_zsdt0001od_fknum is initial
*           então
*          wa_zsdt0001od-tp_status = "AB" - Aberta.

          IF wa_zsdt0001-nro_nf_prod  IS INITIAL AND
          wa_saida_ordem_carregamento-doc_transp IS INITIAL AND
          wa_saida_ordem_carregamento-tknum IS INITIAL.
            wa_saida_ordem_carregamento-tp_status = 'AB'. "- Aberta.
          ENDIF.

          IF wa_saida_ordem_carregamento-tp_status IS INITIAL.
            wa_saida_ordem_carregamento-tp_status       =   wa_zsdt0001od-tp_status.
          ENDIF.

        ELSE.
          wa_saida_ordem_carregamento-tp_status       =   wa_zsdt0001od-tp_status.
        ENDIF.
*       wa_saida_ordem_carregamento-tp_status       =   wa_zsdt0001od-tp_status.
* Fim - RJF - CS2023000859 - ZLES0177 - Opção : Ordem Carregamento - Status

        wa_saida_ordem_carregamento-dt_emissao      =   wa_zsdt0001od-dt_emissao.
        wa_saida_ordem_carregamento-dt_validade     =   wa_zsdt0001od-dt_validade.
        wa_saida_ordem_carregamento-id_bukrs        =   wa_zsdt0001od-id_bukrs.
        wa_saida_ordem_carregamento-id_branch       =   wa_zsdt0001od-id_branch.
        wa_saida_ordem_carregamento-id_bukrs_ag     =   wa_zsdt0001od-id_bukrs_ag.
        wa_saida_ordem_carregamento-id_branch_ag    =   wa_zsdt0001od-id_branch_ag.
        wa_saida_ordem_carregamento-id_local_coleta =   wa_zsdt0001od-id_local_coleta.

        READ TABLE it_j_1bbranch INTO DATA(wa_j_1bbranch) WITH KEY bukrs = wa_zsdt0001od-id_bukrs
                                                                   branch = wa_zsdt0001od-id_branch.
        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-name = wa_j_1bbranch-name.

        ENDIF.

        READ TABLE it_lfa1_coleta INTO DATA(wa_lfa1_coleta) WITH KEY lifnr = wa_zsdt0001od-id_local_coleta.

        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-local_coleta_name1 = wa_lfa1_coleta-name1.
          wa_saida_ordem_carregamento-local_coleta_ort01 = wa_lfa1_coleta-ort01.
          wa_saida_ordem_carregamento-local_coleta_regio = wa_lfa1_coleta-regio.

        ENDIF.


        wa_saida_ordem_carregamento-id_local_descarga = wa_zsdt0001od-id_local_descarga.

        READ TABLE it_kna1_descarga INTO DATA(wa_kna1_descarga) WITH KEY kunnr = wa_zsdt0001od-id_local_descarga.
        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-descarga_kname1 = wa_kna1_descarga-name1.
          wa_saida_ordem_carregamento-descarga_ort01 = wa_kna1_descarga-ort01.
          wa_saida_ordem_carregamento-descarga_regio = wa_kna1_descarga-regio.

        ENDIF.


        wa_saida_ordem_carregamento-id_local_destino = wa_zsdt0001od-id_local_destino.

        READ TABLE it_lfa1_destino INTO DATA(wa_lfa1_destino) WITH KEY lifnr = wa_zsdt0001od-id_local_destino.

        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-local_destino_name1 = wa_lfa1_destino-name1.
          wa_saida_ordem_carregamento-local_destino_ort01 = wa_lfa1_destino-ort01.
          wa_saida_ordem_carregamento-local_destino_regio = wa_lfa1_destino-regio.

        ENDIF.

        wa_saida_ordem_carregamento-ds_placa_trator =   wa_zsdt0001od-ds_placa_trator.
        wa_saida_ordem_carregamento-s_placa_reboq_1 =   wa_zsdt0001od-ds_placa_reboq_1.
        wa_saida_ordem_carregamento-ds_placa_reboq_2 =   wa_zsdt0001od-ds_placa_reboq_2.
        wa_saida_ordem_carregamento-ds_placa_reboq_3 =   wa_zsdt0001od-ds_placa_reboq_3.

        READ TABLE it_zlest0002 INTO DATA(wa_zlest0002) WITH KEY pc_veiculo  = wa_zsdt0001od-ds_placa_trator.
        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-grupo = wa_zlest0002-grupo.
          wa_saida_ordem_carregamento-frota = wa_zlest0002-frota.

        ENDIF.

        wa_saida_ordem_carregamento-id_motorista = wa_zsdt0001od-id_motorista.

        READ TABLE it_lfa1_motorista INTO DATA(wa_lfa1_motorista) WITH KEY lifnr = wa_zsdt0001od-id_motorista.
        IF sy-subrc EQ 0.

          wa_saida_ordem_carregamento-motorista_stcd2 = wa_lfa1_motorista-stcd2.
          wa_saida_ordem_carregamento-motorista_name1 = wa_lfa1_motorista-name1.

        ENDIF.


        wa_saida_ordem_carregamento-nr_frete_comb = wa_zsdt0001od-nr_frete_comb.

        APPEND wa_saida_ordem_carregamento TO it_saida_carregamento.
        CLEAR: wa_saida_ordem_carregamento, wa_zsdt0001od.
        CLEAR: ls_vbak, ls_vbfa, ls_j_1bnflin, ls_j_1bnfdoc, ls_1bnfe_active.
      ENDIF.
    ENDLOOP.




  ELSE.


    "Proprio/Terceiro.

    SELECT DISTINCT erdat tknum shtyp tplst  tdlnr  text1 text2 text3 text4
      FROM vttk
     INTO CORRESPONDING FIELDS OF TABLE it_saida
     WHERE erdat IN p_erdat
       AND add03 EQ '0000000001'
       AND tknum IN p_tknum
       AND tplst IN p_tplst.

    IF it_saida IS NOT INITIAL.
      "Seleção Nome Filial.

      SELECT DISTINCT * FROM ttdst
        INTO TABLE @DATA(t_ttdst)
        FOR ALL ENTRIES IN  @it_saida
        WHERE tplst EQ @it_saida-tplst.

      SORT t_ttdst BY spras tplst ASCENDING.
      DELETE ADJACENT DUPLICATES FROM t_ttdst COMPARING ALL FIELDS.

      SELECT DISTINCT * FROM zsdt0001 INTO TABLE it_zsdt0001
      FOR ALL ENTRIES IN it_saida
      WHERE doc_transp EQ it_saida-tknum.

      SORT it_zsdt0001 BY tknum ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_zsdt0001 COMPARING ALL FIELDS.


      IF it_zsdt0001 IS NOT INITIAL.
        SELECT DISTINCT * FROM zsdt0001od INTO TABLE t_zsdt0001od
          FOR ALL ENTRIES IN it_zsdt0001
          WHERE id_ordem EQ it_zsdt0001-id_ordem.

        SORT t_zsdt0001od BY id_ordem ASCENDING.
        DELETE ADJACENT DUPLICATES FROM t_zsdt0001od COMPARING ALL FIELDS.

      ENDIF.

      "Seleção dados para pegar chave cte.
      FREE: t_vbak_aux.
      SELECT DISTINCT * FROM  vbak INTO TABLE t_vbak_aux
        FOR ALL ENTRIES IN it_saida
        WHERE tknum EQ  it_saida-tknum.

      SORT t_vbak_aux BY tknum ASCENDING.
      DELETE ADJACENT DUPLICATES FROM t_vbak_aux COMPARING ALL FIELDS.

      IF sy-subrc EQ 0.
        "Seleção dados para pegar chave cte.
        FREE: t_vbfa_aux.
        SELECT DISTINCT * FROM  vbfa INTO TABLE t_vbfa_aux
          FOR ALL ENTRIES IN t_vbak_aux
          WHERE vbelv EQ  t_vbak_aux-vbeln
            AND vbtyp_n = 'M'
            AND vbtyp_v = 'C'.

        SORT  t_vbfa_aux BY vbeln ASCENDING.
        DELETE ADJACENT DUPLICATES FROM  t_vbfa_aux COMPARING ALL FIELDS.

        IF t_vbfa_aux IS NOT INITIAL.
          FREE: it_refkey.
          it_refkey = VALUE #( FOR l IN t_vbfa_aux ( refkey = l-vbeln ) ).
          IF it_refkey IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnflin INTO TABLE t_j_1bnflin_aux
              FOR ALL ENTRIES IN it_refkey
              WHERE refkey EQ it_refkey-refkey.

            SORT  t_j_1bnflin_aux BY refkey ASCENDING.
            DELETE ADJACENT DUPLICATES FROM  t_j_1bnflin_aux COMPARING ALL FIELDS.

            IF t_j_1bnflin_aux IS NOT INITIAL.
              SELECT DISTINCT * FROM j_1bnfdoc INTO TABLE t_j_1bnfdoc_aux
                FOR ALL ENTRIES IN t_j_1bnflin_aux
              WHERE docnum = t_j_1bnflin_aux-docnum.

              SORT  t_j_1bnflin_aux BY docnum ASCENDING.
              DELETE ADJACENT DUPLICATES FROM  t_j_1bnflin_aux COMPARING ALL FIELDS.

              IF t_j_1bnflin_aux IS NOT INITIAL.
                SELECT DISTINCT * FROM j_1bnfe_active INTO TABLE t_j_1bnfe_active_aux
                FOR ALL ENTRIES IN t_j_1bnfdoc_aux
                WHERE docnum EQ t_j_1bnfdoc_aux-docnum.

                SORT  t_j_1bnfe_active_aux BY docnum ASCENDING.
                DELETE ADJACENT DUPLICATES FROM  t_j_1bnfe_active_aux COMPARING ALL FIELDS.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(qtd_lines) = lines( it_saida ).
    DATA qtd_ini TYPE sy-tabix.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_ajuste>).

      qtd_ini = sy-tabix.

      <wa_ajuste>-xblnr = <wa_ajuste>-tknum.
      <wa_ajuste>-placa_carreta = <wa_ajuste>-text1(7).

      CHECK <wa_ajuste>-placa_carreta IS NOT INITIAL.

      CLEAR message.
      message = | ( { qtd_ini } de { qtd_lines } ) - Checando Placa: { <wa_ajuste>-placa_carreta }|.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = message.

      TRY.
          zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
            EXPORTING
              i_tknum                = <wa_ajuste>-tknum
            IMPORTING
              e_tipo_veiculo         = DATA(_tp_veiculo)
              e_tipo_remetente       = DATA(_tp_remetente) ).
        CATCH zcx_faturamento.
        CATCH zcx_error.
      ENDTRY.

      <wa_ajuste>-tp_veiculo = _tp_veiculo.

      CASE <wa_ajuste>-tp_veiculo.
        WHEN 'P'.
          <wa_ajuste>-tipo =
          SWITCH #( _tp_remetente WHEN 'P' THEN 'PRÓPRIO'
                                  WHEN 'T' THEN 'TERCEIRO'
                                  WHEN 'I' THEN 'INTERCOMPANY'
                   ).
        WHEN 'T'.
          <wa_ajuste>-tipo = 'TERCEIRO'.
      ENDCASE.

    ENDLOOP.

    IF p_prop = 'X'.
      DELETE it_saida WHERE tp_veiculo = 'T'.
      vg_veiculo = 'Próprio'.
    ELSE.
      DELETE it_saida WHERE tp_veiculo = 'P'.
      vg_veiculo = 'Terceiro'.
    ENDIF.

    vl_message = 'Aguarde, selecionando dados transporte' && vg_veiculo.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 99
        text       = vl_message.

    DELETE it_saida WHERE placa_carreta EQ ' '.


    IF p_placa IS NOT INITIAL.
      DELETE it_saida WHERE placa_carreta NOT IN p_placa.
    ENDIF.

*    CHECK it_saida IS NOT INITIAL.
    IF it_saida IS NOT INITIAL.

      SELECT DISTINCT *
      FROM vtpa
        INTO CORRESPONDING FIELDS OF TABLE it_vtpa
            FOR ALL ENTRIES IN it_saida
      WHERE vbeln EQ it_saida-tknum
      AND parvw IN ('PC' ,'LR', 'PV', 'MT').
      SORT it_vtpa[] BY vbeln ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_vtpa COMPARING ALL FIELDS.


      IF it_vtpa IS NOT INITIAL.
        it_sort_vtpa[] = it_vtpa[].
      ENDIF.

      SELECT DISTINCT *
        FROM vttp
      INTO CORRESPONDING FIELDS OF TABLE it_vttp
              FOR ALL ENTRIES IN it_saida
      WHERE tknum EQ it_saida-tknum.
      SORT it_vttp[] BY vbeln ASCENDING.

      SORT it_vttp BY tknum ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_vttp COMPARING ALL FIELDS.

      IF it_vttp IS NOT INITIAL.

        it_sort_vttp[] = it_vttp[].

        SELECT DISTINCT *
          FROM lips
         INTO CORRESPONDING FIELDS OF TABLE it_lips
            FOR ALL ENTRIES IN it_vttp
         WHERE vbeln EQ it_vttp-vbeln
         ORDER BY PRIMARY KEY.

        SORT it_lips BY vbeln ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_lips COMPARING ALL FIELDS.

        IF it_lips IS NOT INITIAL.

          "Seleciona dados entrada de grãos.
          FREE: t_vbak.
          SELECT DISTINCT * FROM vbak
            INTO CORRESPONDING FIELDS OF TABLE t_vbak
            FOR ALL ENTRIES IN it_lips
            WHERE vbeln EQ it_lips-vgbel.

          SORT t_vbak BY vbeln ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_vbak COMPARING ALL FIELDS.

*        r_vbeln = VALUE #( FOR l IN it_lips ( sign   = 'I' option = 'EQ' low    = l-vbeln ) ).
*        r_vbeln_aux = VALUE #( FOR t IN it_vttp ( sign   = 'I' option = 'EQ' low    = t-vbeln  ) ).
*        APPEND LINES OF r_vbeln_aux TO r_vbeln.
*        SORT r_vbeln BY low.
*        DELETE ADJACENT DUPLICATES FROM r_vbeln COMPARING low.

          FREE: t_vbfa.

          SELECT * FROM vbfa
          INTO CORRESPONDING FIELDS OF TABLE t_vbfa
            FOR ALL ENTRIES IN it_lips
          WHERE vbelv EQ it_lips-vbeln.

          SORT t_vbfa BY vbelv ASCENDING.
**********************************************************************
          DATA: t_vbfa_temp  TYPE STANDARD TABLE OF vbfa,
                lr_vbeln_del TYPE RANGE OF vbfa-vbeln.
          t_vbfa_temp = t_vbfa.
          DELETE t_vbfa_temp WHERE fktyp <> 'L'.

          SORT t_vbfa_temp BY erdat DESCENDING erzet DESCENDING.
          DELETE ADJACENT DUPLICATES FROM t_vbfa_temp COMPARING vbelv.

          lr_vbeln_del[] = VALUE #( FOR wa_del IN t_vbfa_temp ( option = 'EQ' sign = 'I' low = wa_del-vbeln ) ).

          FREE: t_vbfa_temp.

          IF lr_vbeln_del[] IS NOT INITIAL.
            DELETE t_vbfa WHERE fktyp = 'L' AND vbeln NOT IN lr_vbeln_del[].
          ENDIF.
**********************************************************************

*          SELECT DISTINCT * FROM vbfa
*  INTO CORRESPONDING FIELDS OF TABLE t_vbfa
*    FOR ALL ENTRIES IN it_lips
*  WHERE vbelv EQ it_lips-vbeln
*  AND   NOT EXISTS ( SELECT * FROM vbfa AS estorno WHERE vbelv = vbfa~vbeln AND vbtyp_n = 'N' ).

          SORT t_vbfa BY vbelv ASCENDING vbeln DESCENDING. "ALRS
          DELETE ADJACENT DUPLICATES FROM t_vbfa COMPARING ALL FIELDS.

          IF t_vbfa IS NOT INITIAL.
            FREE: it_refkey.
            it_refkey  = VALUE #( FOR g IN t_vbfa ( refkey = g-vbeln ) ).
            r_refkey_aux = VALUE #( FOR s IN t_vbfa ( sign   = 'I' option = 'EQ' low    = |{ s-vbeln }{ s-mjahr }|  ) ).
            APPEND LINES OF r_refkey_aux TO r_refkey.
            SORT it_refkey BY refkey.
            DELETE ADJACENT DUPLICATES FROM it_refkey COMPARING refkey.

            IF it_refkey IS NOT INITIAL.
              SELECT DISTINCT * FROM j_1bnflin INTO TABLE t_j_1bnflin
                FOR ALL ENTRIES IN it_refkey
              WHERE refkey EQ it_refkey-refkey.

              SORT t_j_1bnflin BY refkey ASCENDING.
              DELETE ADJACENT DUPLICATES FROM t_j_1bnflin COMPARING ALL FIELDS.

            ENDIF.

            IF t_j_1bnflin IS NOT INITIAL.
              SELECT DISTINCT * FROM j_1bnfe_active
                  INTO TABLE @DATA(t_j_1bnfe_active)
                FOR ALL ENTRIES IN @t_j_1bnflin
                  WHERE docnum EQ @t_j_1bnflin-docnum
                   AND docsta EQ '1'
                   AND cancel EQ @space.

              SORT t_j_1bnfe_active BY docnum ASCENDING.
              DELETE ADJACENT DUPLICATES FROM t_j_1bnfe_active COMPARING ALL FIELDS.

              IF t_j_1bnfe_active IS NOT INITIAL.

                SELECT * FROM j_1bnfdoc APPENDING TABLE t_j_1bnfdoc
                  FOR ALL ENTRIES IN t_j_1bnfe_active
                  WHERE docnum EQ t_j_1bnfe_active-docnum.

                SELECT DISTINCT * FROM zsdt0105 INTO TABLE @DATA(t_zsdt0105)
                  FOR ALL ENTRIES IN @t_j_1bnfe_active
                  WHERE docnum EQ @t_j_1bnfe_active-docnum.

                DELETE t_zsdt0105 WHERE nmdfe IS INITIAL.  "*-IR212606-18.12.2024-#160894-JT-inicio

                SORT t_zsdt0105 BY docnum ASCENDING docnum_ref DESCENDING.
                DELETE ADJACENT DUPLICATES FROM t_zsdt0105 COMPARING ALL FIELDS.

                IF t_zsdt0105 IS NOT INITIAL.
                  SELECT DISTINCT * FROM zsdt0102 INTO TABLE @DATA(t_zsdt0102)
                  FOR ALL ENTRIES IN @t_zsdt0105
                  WHERE docnum EQ @t_zsdt0105-docnum_ref
                    AND autorizado EQ 'X'
                    AND estornado  EQ @space
                    AND cancel     EQ @space.

                  DELETE t_zsdt0102 WHERE nmdfe IS INITIAL.  "*-IR212606-18.12.2024-#160894-JT-inicio

                  SORT t_zsdt0102 BY docnum ASCENDING.
                  DELETE ADJACENT DUPLICATES FROM t_zsdt0102 COMPARING ALL FIELDS.
                ELSE.
*>>>>>>>>>>>Inicio ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
                  LOOP AT t_j_1bnfe_active INTO DATA(lS_j_1bn_active).
                    APPEND VALUE #(  chave_nfe =   lS_j_1bn_active-regio &&
                                                    lS_j_1bn_active-nfyear &&
                                                    lS_j_1bn_active-nfmonth &&
                                                    lS_j_1bn_active-stcd1 &&
                                                    lS_j_1bn_active-model &&
                                                    lS_j_1bn_active-serie &&
                                                    lS_j_1bn_active-nfnum9 &&
                                                    lS_j_1bn_active-docnum9 &&
                                                    lS_j_1bn_active-cdv ) TO tg_zsdt0241_aux.
                  ENDLOOP.

                  IF tg_zsdt0241_aux IS NOT INITIAL.
                    SELECT * FROM zsdt0241 APPENDING TABLE it_zsdt0241_aux
                    FOR ALL ENTRIES IN tg_zsdt0241_aux
                    WHERE chave EQ tg_zsdt0241_aux-chave_nfe.
                  ENDIF.
                ENDIF.
*>>>>>>>>>>>Fim ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<

                SELECT DISTINCT * FROM zlest0039
                  INTO TABLE @DATA(t_zlest0039)
                  FOR ALL ENTRIES IN @t_j_1bnfe_active
                  WHERE docnum EQ @t_j_1bnfe_active-docnum.

                SORT t_zlest0039 BY docnum ASCENDING.
                DELETE ADJACENT DUPLICATES FROM t_zlest0039 COMPARING ALL FIELDS.

                IF t_zlest0039 IS NOT INITIAL.
                  SELECT DISTINCT name1, kunnr FROM kna1
                 INTO TABLE @DATA(it_kna1)
                 FOR ALL ENTRIES IN @t_zlest0039
                    WHERE kunnr EQ @t_zlest0039-transb_efetivo.

                  SORT it_kna1 BY kunnr ASCENDING.
                  DELETE ADJACENT DUPLICATES FROM it_kna1 COMPARING ALL FIELDS.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.


          SELECT DISTINCT *
            FROM makt
            INTO CORRESPONDING FIELDS OF TABLE it_makt
            FOR ALL ENTRIES IN it_lips
            WHERE matnr = it_lips-matnr
              AND spras = sy-langu
              ORDER BY PRIMARY KEY.

          SORT it_makt BY matnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_makt COMPARING ALL FIELDS.
        ENDIF.


*------------------------------------------------------------------------------------
* "Processo para encontrar a chave NFe - Remessa.
*------------------------------------------------------------------------------------
        FREE: tg_vbfa_r.
        SELECT DISTINCT * FROM vbfa
        INTO CORRESPONDING FIELDS OF TABLE tg_vbfa_r
          FOR ALL ENTRIES IN it_lips
        WHERE vbelv EQ it_lips-vbeln
          AND vbtyp_n  = 'M'
          AND vbtyp_v  = 'J'
          AND fktyp = 'L'. "169673 IR224170 ZLES0177 NF remessa  por conta e ordem PSA

        SORT tg_vbfa_r BY vbelv ASCENDING.
        DELETE ADJACENT DUPLICATES FROM tg_vbfa_r COMPARING ALL FIELDS.

*        FREE: tg_zlest0213_r.
*        SELECT DISTINCT * FROM zlest0213 INTO TABLE tg_zlest0213_r
*          FOR ALL ENTRIES IN it_lips
*           WHERE vbeln EQ it_lips-vbeln.
*
*        SORT tg_zlest0213_r BY vbeln ASCENDING.
*        DELETE ADJACENT DUPLICATES FROM tg_zlest0213_r COMPARING ALL FIELDS.

        FREE: it_refkey.
        it_refkey = VALUE #( FOR y IN tg_vbfa_r ( refkey   = y-vbeln ) ).

        SORT it_refkey BY refkey ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_refkey COMPARING ALL FIELDS.

        IF it_refkey IS NOT INITIAL.
          SELECT DISTINCT * FROM j_1bnflin INTO TABLE tg_j_1bnflin_r
            FOR ALL ENTRIES IN it_refkey
          WHERE refkey EQ it_refkey-refkey
            AND reftyp = 'BI'.

          SORT tg_j_1bnflin_r BY refkey ASCENDING.
          DELETE ADJACENT DUPLICATES FROM tg_j_1bnflin_r COMPARING ALL FIELDS.

          IF tg_j_1bnflin_r IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnfe_active
            INTO TABLE tg_j_1bnfe_active_r
            FOR ALL ENTRIES IN tg_j_1bnflin_r
            WHERE docnum EQ tg_j_1bnflin_r-docnum
             AND docsta EQ '1'
             AND cancel EQ space.

            SORT tg_j_1bnfe_active_r BY docnum ASCENDING.
            DELETE ADJACENT DUPLICATES FROM tg_j_1bnfe_active_r COMPARING ALL FIELDS.

          ENDIF.
        ENDIF.



*------------------------------------------------------------------------------------
* "Processo para encontrar a chave NFe - *Remessas de transferência*
*------------------------------------------------------------------------------------

        FREE: tg_vbfa_rt.
        SELECT DISTINCT * FROM vbfa
        INTO CORRESPONDING FIELDS OF TABLE tg_vbfa_rt
          FOR ALL ENTRIES IN it_lips
        WHERE vbelv EQ it_lips-vbeln
          AND vbtyp_n  = 'R'
          AND vbtyp_v  = 'J'.

        SORT tg_vbfa_rt BY vbelv ASCENDING.
        DELETE ADJACENT DUPLICATES FROM tg_vbfa_rt COMPARING ALL FIELDS.

        IF tg_vbfa_rt IS NOT INITIAL.
          FREE: it_refkey.
          it_refkey = VALUE #( FOR c IN tg_vbfa_rt ( refkey = |{ c-vbeln }{ c-mjahr } | ) ).

          SORT it_refkey BY refkey ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_refkey COMPARING ALL FIELDS.

          IF it_refkey IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnflin INTO TABLE tg_j_1bnflin_rt
              FOR ALL ENTRIES IN it_refkey
            WHERE refkey EQ it_refkey-refkey
            AND reftyp = 'MD'.

            SORT tg_j_1bnflin_rt BY refkey ASCENDING.
            DELETE ADJACENT DUPLICATES FROM tg_j_1bnflin_rt COMPARING ALL FIELDS.

            IF tg_j_1bnflin_rt IS NOT INITIAL.
              SELECT DISTINCT * FROM j_1bnfe_active
              INTO TABLE tg_j_1bnfe_active_rt
              FOR ALL ENTRIES IN tg_j_1bnflin_rt
              WHERE docnum EQ tg_j_1bnflin_rt-docnum
               AND docsta EQ '1'
               AND cancel EQ space.

              SORT tg_j_1bnfe_active_rt BY docnum ASCENDING.
              DELETE ADJACENT DUPLICATES FROM tg_j_1bnfe_active_rt COMPARING ALL FIELDS.

            ENDIF.
          ENDIF.

*-IR212606-18.12.2024-#160894-JT-inicio
          IF tg_j_1bnfe_active_rt[] IS NOT INITIAL.
            SELECT *
              FROM zsdt0105
              INTO TABLE t_zsdt0105_rt
               FOR ALL ENTRIES IN tg_j_1bnfe_active_rt
             WHERE docnum = tg_j_1bnfe_active_rt-docnum.

            IF t_zsdt0105_rt[] IS NOT INITIAL.
              SELECT *
                FROM zsdt0102
                INTO TABLE t_zsdt0102_rt
                 FOR ALL ENTRIES IN t_zsdt0105_rt
               WHERE docnum = t_zsdt0105_rt-docnum_ref.
            ENDIF.
          ENDIF.
*-IR212606-18.12.2024-#160894-JT-fim

        ENDIF.

*------------------------------------------------------------------------------------
* "Processo para encontrar a chave NFe - *Aviso de Recebimento*
*------------------------------------------------------------------------------------
        FREE: tg_likp_a.
        SELECT DISTINCT * FROM likp INTO CORRESPONDING FIELDS OF TABLE tg_likp_a
          FOR ALL ENTRIES IN it_lips
           WHERE vbeln EQ it_lips-vbeln.
*            AND tcode EQ 'ZLES0136'.  "*-IR212606-24.12.2024-#161504-JT-inicio

        SORT tg_likp_a BY berot ASCENDING.
        DELETE tg_likp_a WHERE berot EQ space.

        IF tg_likp_a IS NOT INITIAL.
          FREE: tg_zfiwrt0008_a.
          SELECT DISTINCT * FROM zfiwrt0008 INTO TABLE tg_zfiwrt0008_a
            FOR ALL ENTRIES IN tg_likp_a
             WHERE ch_referencia EQ tg_likp_a-berot.

          SORT tg_zfiwrt0008_a BY ch_referencia ASCENDING.
          DELETE ADJACENT DUPLICATES FROM tg_zfiwrt0008_a COMPARING ALL FIELDS.

          IF tg_zfiwrt0008_a IS NOT INITIAL.
            FREE: it_refkey.
            it_refkey = VALUE #( FOR o IN tg_zfiwrt0008_a ( refkey  = o-seq_lcto ) ).

            SORT it_refkey BY refkey ASCENDING.
            DELETE ADJACENT DUPLICATES FROM it_refkey COMPARING ALL FIELDS.

            IF it_refkey IS NOT INITIAL.
              SELECT DISTINCT * FROM j_1bnflin INTO TABLE tg_j_1bnflin_a
                FOR ALL ENTRIES IN it_refkey
               WHERE refkey EQ it_refkey-refkey
                 AND reftyp EQ 'ZW'.

              SORT tg_j_1bnflin_a BY refkey ASCENDING.
              DELETE ADJACENT DUPLICATES FROM tg_j_1bnflin_a COMPARING ALL FIELDS.

              IF tg_j_1bnflin_a IS NOT INITIAL.
                SELECT DISTINCT * FROM j_1bnfe_active
              INTO TABLE tg_j_1bnfe_active_a
                  FOR ALL ENTRIES IN tg_j_1bnflin_a
              WHERE docnum EQ tg_j_1bnflin_a-docnum
               AND docsta EQ '1'
               AND cancel EQ space.

                SORT tg_j_1bnfe_active_a BY docnum ASCENDING.
                DELETE ADJACENT DUPLICATES FROM tg_j_1bnfe_active_a COMPARING ALL FIELDS.

              ENDIF.
            ENDIF.
          ENDIF.

*-IR212606-18.12.2024-#160894-JT-inicio
          IF tg_j_1bnfe_active_a[] IS NOT INITIAL.
            SELECT *
              FROM zsdt0105
              INTO TABLE t_zsdt0105_a
               FOR ALL ENTRIES IN tg_j_1bnfe_active_a
             WHERE docnum = tg_j_1bnfe_active_a-docnum.

            IF sy-subrc = 0.
              SELECT *
                FROM zsdt0102
                INTO TABLE t_zsdt0102_a
                 FOR ALL ENTRIES IN t_zsdt0105_a
               WHERE docnum     = t_zsdt0105_a-docnum_ref
                 AND autorizado = abap_true
                 AND estornado  = abap_false
                 AND cancel     = abap_false.
            ENDIF.

          ENDIF.
        ENDIF.


        FREE: tg_likp_ux.
        SELECT DISTINCT * FROM likp INTO CORRESPONDING FIELDS OF TABLE tg_likp_ux
          FOR ALL ENTRIES IN it_lips
           WHERE vbeln EQ it_lips-vbeln.
*             and tcode ne 'ZLES0136'.

        SORT tg_likp_ux BY vbeln ASCENDING.
        DELETE ADJACENT DUPLICATES FROM tg_likp_ux COMPARING ALL FIELDS.

        LOOP AT tg_likp_ux ASSIGNING FIELD-SYMBOL(<ls_likp>).
          IF <ls_likp>-lifex IS NOT INITIAL.
            SPLIT <ls_likp>-lifex AT '-' INTO: <ls_likp>-nr_nf <ls_likp>-serie_nf. "177935 IR224170   ZLES0177  - Dados MDF-e avulsa

            IF <ls_likp>-serie_nf IS NOT INITIAL.
              <ls_likp>-serie_nf = |{ <ls_likp>-serie_nf ALPHA = OUT }|.
              <ls_likp>-serie_nf1 = |0{ <ls_likp>-serie_nf }|.
              <ls_likp>-serie_nf2 = |{ <ls_likp>-serie_nf ALPHA = IN }|.
            ENDIF.

*          if <ls_likp>-lifex is not initial.
*            split <ls_likp>-lifex at '-' into: <ls_likp>-nr_nf <ls_likp>-serie_nf.
*            split <ls_likp>-lifex at '-' into: <ls_likp>-nr_nf <ls_likp>-serie_nf2.  "*-IR212606-18.12.2024-#160894-JT-inicio
*          else.
*            split <ls_likp>-xblnr at '-' into: <ls_likp>-nr_nf <ls_likp>-serie_nf.
*            split <ls_likp>-xblnr at '-' into: <ls_likp>-nr_nf <ls_likp>-serie_nf2.  "*-IR212606-18.12.2024-#160894-JT-inicio
*          endif.
            <ls_likp>-nr_nf = |{ <ls_likp>-nr_nf ALPHA = IN }|.
          ENDIF.
        ENDLOOP.

        IF tg_likp_ux IS NOT INITIAL.
          FREE: tg_j_1bnfdoc_a.
          SELECT DISTINCT * FROM j_1bnfdoc INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc_a
            FOR ALL ENTRIES IN tg_likp_ux
             WHERE   parid  EQ tg_likp_ux-lifnr
               AND   nfenum EQ tg_likp_ux-nr_nf
*               and ( model  eq tg_likp_ux-serie_nf  "*-IR212606-18.12.2024-#160894-JT-inicio
                AND   series EQ tg_likp_ux-serie_nf. "*-IR212606-18.12.2024-#160894-JT-inicio
*          if sy-subrc ne 0.
          SELECT DISTINCT * FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc_a
          FOR ALL ENTRIES IN tg_likp_ux
           WHERE   parid  EQ tg_likp_ux-lifnr
             AND   nfenum EQ tg_likp_ux-nr_nf
*               and ( model  eq tg_likp_ux-serie_nf   "*-IR212606-18.12.2024-#160894-JT-inicio
              AND   series EQ tg_likp_ux-serie_nf1. "*-IR212606-18.12.2024-#160894-JT-inicio
*            if sy-subrc ne 0.
          SELECT DISTINCT * FROM j_1bnfdoc APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc_a
        FOR ALL ENTRIES IN tg_likp_ux
         WHERE   parid  EQ tg_likp_ux-lifnr
           AND   nfenum EQ tg_likp_ux-nr_nf
*               and ( model  eq tg_likp_ux-serie_nf   "*-IR212606-18.12.2024-#160894-JT-inicio
            AND   series EQ tg_likp_ux-serie_nf2. "*-IR212606-18.12.2024-#160894-JT-inicio
*            endif.

*          endif.

          SORT tg_j_1bnfdoc_a BY parid ASCENDING.
          DELETE ADJACENT DUPLICATES FROM tg_j_1bnfdoc_a COMPARING ALL FIELDS.


          IF tg_j_1bnfdoc_a IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnfe_active
            APPENDING TABLE tg_j_1bnfe_active_a
            FOR ALL ENTRIES IN tg_j_1bnfdoc_a
            WHERE docnum EQ tg_j_1bnfdoc_a-docnum
             AND docsta EQ '1'
             AND cancel EQ space.

            SORT tg_j_1bnfe_active_a BY docnum ASCENDING.
            DELETE ADJACENT DUPLICATES FROM tg_j_1bnfe_active_a COMPARING ALL FIELDS.
*>>>>>>>>>>>Inicio ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
            tg_zsdt0241_aux = VALUE #( FOR lw_active IN tg_j_1bnfe_active_a ( chave_nfe = lw_active-regio &&
                                            lw_active-nfyear &&
                                            lw_active-nfmonth &&
                                            lw_active-stcd1 &&
                                            lw_active-model &&
                                            lw_active-serie &&
                                            lw_active-nfnum9 &&
                                            lw_active-docnum9 &&
                                            lw_active-cdv ) ).

            IF tg_zsdt0241_aux IS NOT INITIAL.
              SELECT * FROM zsdt0241 INTO TABLE it_zsdt0241_aux
                FOR ALL ENTRIES IN tg_zsdt0241_aux
                WHERE chave EQ tg_zsdt0241_aux-chave_nfe.
            ENDIF.
*>>>>>>>>>>>Fim ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
          ENDIF.
        ENDIF.
      ENDIF.

*>>>>>>>>>>>Inicio ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
*      Seleção de dados notas entrada de graõs.
      SELECT * FROM zmmt_ee_zgr_docs
      INTO TABLE it_zmmt_zgr_docs
      FOR ALL ENTRIES IN it_lips
      WHERE av_vbeln EQ it_lips-vbeln.

      IF it_zmmt_zgr_docs IS NOT INITIAL.
        "Seleciona nota fiscal.
        SELECT *
        FROM j_1bnfe_active
        APPENDING TABLE tg_j_1bnfe_active_a
        FOR ALL ENTRIES IN it_zmmt_zgr_docs
        WHERE docnum EQ it_zmmt_zgr_docs-docnum
        AND docsta EQ '1'
        AND cancel EQ space.

        IF tg_j_1bnfe_active_a[] IS NOT INITIAL. "PSA


          SELECT *
           FROM zsdt0105
           APPENDING TABLE t_zsdt0105_a
            FOR ALL ENTRIES IN tg_j_1bnfe_active_a
           WHERE docnum = tg_j_1bnfe_active_a-docnum.

          IF t_zsdt0105_a IS NOT INITIAL.
            SORT t_zsdt0105_a.
            DELETE ADJACENT DUPLICATES FROM t_zsdt0105_a.

            SELECT *
              FROM zsdt0102
              APPENDING TABLE t_zsdt0102_a
               FOR ALL ENTRIES IN t_zsdt0105_a
             WHERE docnum     = t_zsdt0105_a-docnum_ref
               AND autorizado = abap_true
               AND estornado  = abap_false
               AND cancel     = abap_false.


          ELSE.

            LOOP AT tg_j_1bnfe_active_a INTO DATA(lw_j_1bn_active).

              APPEND VALUE #(  chave_nfe =    lw_j_1bn_active-regio &&
                                              lw_j_1bn_active-nfyear &&
                                              lw_j_1bn_active-nfmonth &&
                                              lw_j_1bn_active-stcd1 &&
                                              lw_j_1bn_active-model &&
                                              lw_j_1bn_active-serie &&
                                              lw_j_1bn_active-nfnum9 &&
                                              lw_j_1bn_active-docnum9 &&
                                              lw_j_1bn_active-cdv ) TO tg_zsdt0241_aux.

            ENDLOOP.

            IF tg_zsdt0241_aux IS NOT INITIAL.
              SELECT * FROM zsdt0241 APPENDING TABLE it_zsdt0241_aux
              FOR ALL ENTRIES IN tg_zsdt0241_aux
              WHERE chave EQ tg_zsdt0241_aux-chave_nfe.
            ENDIF.


          ENDIF.
        ENDIF.



*        IF tg_j_1bnfe_active_a[] IS NOT INITIAL.
*          SELECT *
*            FROM zsdt0105
*            APPENDING TABLE t_zsdt0105_a
*             FOR ALL ENTRIES IN tg_j_1bnfe_active_a
*           WHERE docnum = tg_j_1bnfe_active_a-docnum.
*
*          IF sy-subrc = 0.
*            SELECT *
*              FROM zsdt0102
*              APPENDING TABLE t_zsdt0102_a
*               FOR ALL ENTRIES IN t_zsdt0105_a
*             WHERE docnum     = t_zsdt0105_a-docnum_ref
*               AND autorizado = abap_true
*               AND estornado  = abap_false
*               AND cancel     = abap_false.
*          ELSE.
*
*            LOOP AT tg_j_1bnfe_active_a INTO DATA(lw_j_1bn_active).
*              APPEND VALUE #(  chave_nfe =   lw_j_1bn_active-regio &&
*                                              lw_j_1bn_active-nfyear &&
*                                              lw_j_1bn_active-nfmonth &&
*                                              lw_j_1bn_active-stcd1 &&
*                                              lw_j_1bn_active-model &&
*                                              lw_j_1bn_active-serie &&
*                                              lw_j_1bn_active-nfnum9 &&
*                                              lw_j_1bn_active-docnum9 &&
*                                              lw_j_1bn_active-cdv ) TO tg_zsdt0241_aux.
*            ENDLOOP.
*
*            IF tg_zsdt0241_aux IS NOT INITIAL.
*              SELECT * FROM zsdt0241 APPENDING TABLE it_zsdt0241_aux
*              FOR ALL ENTRIES IN tg_zsdt0241_aux
*              WHERE chave EQ tg_zsdt0241_aux-chave_nfe.
*            ENDIF.
*          ENDIF.
*        ENDIF.
      ENDIF.
*>>>>>>>>>>>Fim ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<

      DATA(rg_text1) = VALUE ty_rg_text1( FOR lwa_saida IN it_saida[] (
          sign   = 'I'
          option = 'EQ'
          low    = lwa_saida-text1(7)
          high   = lwa_saida-text1(7) )  ).
      SORT rg_text1[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rg_text1[] COMPARING low.

      DATA(rg_tknum) = VALUE ty_rg_tknum( FOR lwa_saida IN it_saida[] (
          sign   = 'I'
          option = 'EQ'
          low    = lwa_saida-tknum
          high   = lwa_saida-tknum )  ).
      SORT rg_tknum[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rg_tknum[] COMPARING low.

      SELECT DISTINCT pc_veiculo, frota, grupo
          FROM zlest0002
          INTO TABLE @DATA(lt_zlest0002)
        WHERE pc_veiculo IN @rg_text1
        ORDER BY pc_veiculo ASCENDING.

      SORT lt_zlest0002 BY pc_veiculo ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_zlest0002 COMPARING ALL FIELDS.

      SELECT DISTINCT tknum, vbeln, bukrs_vf, auart, kvgr3
        FROM vbak
        INTO TABLE @DATA(lt_vbak)
        WHERE tknum IN @rg_tknum
        ORDER BY tknum ASCENDING.

      SORT lt_vbak BY tknum ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_vbak COMPARING ALL FIELDS.

      IF ( lt_vbak[] IS NOT INITIAL ).

        DATA(rg_vbeln) = VALUE ty_rg_vbeln( FOR lwa_vbak IN lt_vbak[] (
          sign   = 'I' option = 'EQ' low = lwa_vbak-vbeln high = lwa_vbak-vbeln )  ).
        SORT rg_vbeln[] BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM rg_vbeln[] COMPARING low.

        DATA(rg_bukrs_vf) = VALUE ty_rg_bukrs_vf( FOR lwa_vbak IN lt_vbak[] (
          sign   = 'I' option = 'EQ' low = lwa_vbak-bukrs_vf high = lwa_vbak-bukrs_vf )  ).
        SORT rg_bukrs_vf[] BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM rg_bukrs_vf[] COMPARING low.

        SELECT DISTINCT vbeln, erdat, vbtyp_n, vbtyp_v, mjahr, vbelv FROM vbfa
            INTO TABLE @DATA(lt_vbfa)
            WHERE vbelv IN @rg_vbeln[]
            AND vbtyp_n = 'M'
          ORDER BY vbeln ASCENDING.

        SORT lt_vbfa BY vbeln ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_vbfa COMPARING ALL FIELDS.

*        IF ( sy-subrc = 0 ).
        IF ( lt_vbfa[] IS NOT INITIAL ).

          DATA(rg_vbeln_vbfa) = VALUE ty_rg_vbeln( FOR lwa_vbfa IN lt_vbfa[] (
            sign   = 'I' option = 'EQ' low = lwa_vbfa-vbeln high = lwa_vbfa-vbeln )  ).
          SORT rg_vbeln_vbfa[] BY low ASCENDING.
          DELETE ADJACENT DUPLICATES FROM rg_vbeln_vbfa[] COMPARING low.

          SELECT DISTINCT refkey, docnum FROM j_1bnflin
              INTO TABLE @DATA(lt_j_1bnflin)
            WHERE refkey IN @rg_vbeln_vbfa[] AND reftyp EQ 'BI'
          ORDER BY refkey ASCENDING.

          SORT lt_j_1bnflin BY refkey ASCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_j_1bnflin COMPARING ALL FIELDS.

          IF lt_j_1bnflin IS NOT INITIAL.
            SELECT DISTINCT * FROM zsdt0105 INTO TABLE @DATA(it_zsdt0105)
              FOR ALL ENTRIES IN @lt_j_1bnflin
              WHERE docnum EQ @lt_j_1bnflin-docnum.

            SORT it_zsdt0105 BY docnum ASCENDING docnum_ref DESCENDING. "ALRS
            DELETE ADJACENT DUPLICATES FROM it_zsdt0105 COMPARING ALL FIELDS.

            IF it_zsdt0105 IS NOT INITIAL.
              SELECT DISTINCT * FROM zsdt0102 INTO TABLE @DATA(it_zsdt0102)
              FOR ALL ENTRIES IN @it_zsdt0105
              WHERE docnum EQ @it_zsdt0105-docnum_ref
                AND autorizado EQ 'X'
                AND estornado  EQ @space
                AND cancel     EQ @space.

              SORT it_zsdt0102 BY docnum ASCENDING.
              DELETE ADJACENT DUPLICATES FROM it_zsdt0102 COMPARING ALL FIELDS.

            ENDIF.

            SELECT DISTINCT * FROM zlest0039
              INTO TABLE @DATA(tl_zlest0039)
               FOR ALL ENTRIES IN @lt_j_1bnflin
              WHERE docnum EQ @lt_j_1bnflin-docnum.

            SORT tl_zlest0039 BY docnum ASCENDING.
            DELETE ADJACENT DUPLICATES FROM tl_zlest0039 COMPARING ALL FIELDS.

            IF it_zlest0039 IS NOT INITIAL.
              SELECT name1, kunnr FROM kna1
             INTO TABLE @DATA(tl_kna1)
             FOR ALL ENTRIES IN @tl_zlest0039
                WHERE kunnr EQ @tl_zlest0039-transb_efetivo.

              SORT tl_kna1 BY kunnr ASCENDING.
              DELETE ADJACENT DUPLICATES FROM tl_kna1 COMPARING ALL FIELDS.

            ENDIF.
          ENDIF.


          IF ( sy-subrc = 0 ).

            DATA(rg_docnum) = VALUE ty_rg_docnum( FOR lwa_j_1bnflin IN lt_j_1bnflin[] (
            sign   = 'I' option = 'EQ' low = lwa_j_1bnflin-docnum high = lwa_j_1bnflin-docnum )  ).
            SORT rg_docnum[] BY low ASCENDING.
            DELETE ADJACENT DUPLICATES FROM rg_docnum[] COMPARING low.

            SELECT DISTINCT docnum, nfenum, belnr
                  FROM j_1bnfdoc
                INTO TABLE @DATA(lt_j_1bnfdoc)
                  WHERE docnum  IN @rg_docnum[]
                    AND cancel  EQ ''
            ORDER BY docnum.

            SORT lt_j_1bnfdoc BY docnum ASCENDING.
            DELETE ADJACENT DUPLICATES FROM lt_j_1bnfdoc COMPARING ALL FIELDS.

          ENDIF.

        ENDIF.

      ENDIF.

      IF ( it_vtpa[] IS NOT INITIAL ).
        SELECT DISTINCT addrnumber, name1, city1, country, region
              FROM adrc
              INTO TABLE @DATA(lt_adrc)
              FOR ALL ENTRIES IN @it_vtpa[]
            WHERE addrnumber  = @it_vtpa-adrnr.
        SORT lt_adrc[] BY addrnumber ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_adrc[] COMPARING ALL FIELDS.

        SELECT DISTINCT lifnr, stcd2 FROM lfa1
              INTO TABLE @DATA(lt_lfa1)
              FOR ALL ENTRIES IN @it_vtpa[]
             WHERE lifnr = @it_vtpa-lifnr.
        SORT lt_lfa1[] BY lifnr ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_lfa1[] COMPARING ALL FIELDS.
      ENDIF.

      vl_message = 'Aguarde, preparando dados transporte' && vg_veiculo.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 99
          text       = vl_message.

      qtd_lines = lines( it_saida ).


      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_saida_simplicada>).

        CLEAR message.
        qtd_ini = sy-tabix.
        message = | ( { qtd_ini } de { qtd_lines } ) - Checando doc transporte: { <wa_saida_simplicada>-tknum }|.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = message.


***    SELECT SINGLE frota grupo
***      FROM zlest0002
***      INTO (frota, grupo_veiculo)
***    WHERE pc_veiculo = <wa_saida_simplicada>-text1.

        READ TABLE lt_zlest0002[] ASSIGNING FIELD-SYMBOL(<lfs_zlest0002>) WITH KEY pc_veiculo = <wa_saida_simplicada>-text1 BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          <wa_saida_simplicada>-frota = <lfs_zlest0002>-frota.
          <wa_saida_simplicada>-grupo_veiculo = <lfs_zlest0002>-grupo.
        ENDIF.

        READ TABLE t_ttdst ASSIGNING FIELD-SYMBOL(<ws_ttdst>) WITH KEY tplst = <wa_saida_simplicada>-tplst.
        IF sy-subrc EQ 0.
          <wa_saida_simplicada>-bezei = <ws_ttdst>-bezei.
        ENDIF.

        READ TABLE it_zsdt0001 ASSIGNING FIELD-SYMBOL(<ws_zsdt0001>) WITH KEY doc_transp = <wa_saida_simplicada>-tknum.
        IF sy-subrc EQ 0.
          READ TABLE t_zsdt0001od ASSIGNING FIELD-SYMBOL(<ws_zsdt0001od>) WITH KEY id_ordem = <ws_zsdt0001>-id_ordem.
          IF sy-subrc EQ 0.
            <wa_saida_simplicada>-ordem_car = <ws_zsdt0001od>-nr_ordem.
            <wa_saida_simplicada>-dt_oc     = <ws_zsdt0001od>-dt_emissao.
          ENDIF.
        ENDIF.

*    SELECT SINGLE BELNR BUKRS GJAHR FROM BKPF INTO
*    CORRESPONDING FIELDS OF WA_BKPF
*    WHERE XBLNR = <WA_SAIDA_SIMPLICADA>-XBLNR
*     AND BUDAT >= <WA_SAIDA_SIMPLICADA>-ERDAT.
*
*    <WA_SAIDA_SIMPLICADA>-BUKRS = WA_BKPF-BUKRS.
*    <WA_SAIDA_SIMPLICADA>-GJAHR = WA_BKPF-GJAHR.
*
*    SELECT SINGLE DMBE2 FROM BSIS INTO CORRESPONDING FIELDS OF WA_BSIS
*    WHERE BELNR = WA_BKPF-BELNR
*     AND BUKRS = <WA_SAIDA_SIMPLICADA>-BUKRS
*     AND GJAHR = <WA_SAIDA_SIMPLICADA>-GJAHR
*     AND SHKZG = 'S'.
*
*    <WA_SAIDA_SIMPLICADA>-DMBE2 = WA_BSIS-DMBE2.
*
*    IF <WA_SAIDA_SIMPLICADA>-NETWR > 0 AND <WA_SAIDA_SIMPLICADA>-NTGEW > 0.
*      <WA_SAIDA_SIMPLICADA>-PRECO_TONELADA_RS = ( <WA_SAIDA_SIMPLICADA>-NETWR / <WA_SAIDA_SIMPLICADA>-NTGEW ) * 1000.
*    ENDIF.
*
*    IF <WA_SAIDA_SIMPLICADA>-NETWR > 0 AND <WA_SAIDA_SIMPLICADA>-DMBE2 > 0.
*      <WA_SAIDA_SIMPLICADA>-TAXA_DOLAR = <WA_SAIDA_SIMPLICADA>-NETWR / <WA_SAIDA_SIMPLICADA>-DMBE2.
*    ENDIF.

        <wa_saida_simplicada>-placa_carreta = <wa_saida_simplicada>-text1(7).
        <wa_saida_simplicada>-carreta1 = <wa_saida_simplicada>-text2(7).
        <wa_saida_simplicada>-carreta2 = <wa_saida_simplicada>-text3(7).
        <wa_saida_simplicada>-carreta3 = <wa_saida_simplicada>-text4(7).

*      " está retindo muito o WA_VBAK-VBELN.
***    SELECT SINGLE vbeln bukrs_vf auart FROM vbak
***    INTO CORRESPONDING FIELDS OF wa_vbak
***    WHERE tknum EQ <wa_saida_simplicada>-tknum.
*      AND AUART NE 'ZTRO'.

        READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<lfs_vbak>) WITH KEY tknum = <wa_saida_simplicada>-tknum BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          wa_vbak = CORRESPONDING #( <lfs_vbak> ).
        ENDIF.

        IF wa_vbak IS NOT INITIAL.

          <wa_saida_simplicada>-vbak_vbeln =    wa_vbak-vbeln.
          <wa_saida_simplicada>-vbak_bukrs_vf = wa_vbak-bukrs_vf.
          <wa_saida_simplicada>-vbak_auart =    wa_vbak-auart.

***      SELECT SINGLE vbeln erdat FROM vbfa
***      INTO CORRESPONDING FIELDS OF wa_vbfa
***      WHERE vbelv EQ <wa_saida_simplicada>-vbak_vbeln
***        AND vbtyp_n = 'M'.

          READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lfs_vbfa>) WITH KEY vbeln = <wa_saida_simplicada>-vbak_vbeln BINARY SEARCH.
          IF ( sy-subrc = 0 ).
            wa_vbfa = CORRESPONDING #( <lfs_vbfa> ).
          ENDIF.

          IF wa_vbfa IS NOT INITIAL.

**        Adicionando zeros as esquerdas.
            "WA_VBFA-VBELN = |{ WA_VBFA-VBELN ALPHA = IN }|.
            <wa_saida_simplicada>-vbfa_vbeln = wa_vbfa-vbeln.
            <wa_saida_simplicada>-vbfa_erdat = wa_vbfa-erdat.

            SELECT SINGLE belnr bukrs gjahr FROM bkpf INTO CORRESPONDING FIELDS OF wa_bkpf
            WHERE  awkey EQ <wa_saida_simplicada>-vbfa_vbeln
            AND bukrs EQ <wa_saida_simplicada>-vbak_bukrs_vf
            AND gjahr EQ <wa_saida_simplicada>-vbfa_erdat(4).

            IF wa_bkpf IS NOT INITIAL.

              <wa_saida_simplicada>-belnr2 = wa_bkpf-belnr.

              READ TABLE lt_j_1bnflin[] ASSIGNING FIELD-SYMBOL(<lfs_j_1bnflin>) WITH KEY refkey = <wa_saida_simplicada>-vbfa_vbeln BINARY SEARCH.
              IF ( sy-subrc = 0 ).
                wa_j_1bnflin = CORRESPONDING #( <lfs_j_1bnflin> ).
              ENDIF.

***          SELECT SINGLE  docnum FROM j_1bnflin INTO CORRESPONDING FIELDS OF wa_j_1bnflin
***          WHERE refkey EQ <wa_saida_simplicada>-vbfa_vbeln AND reftyp EQ 'BI'.

              IF wa_j_1bnflin IS NOT INITIAL.

                <wa_saida_simplicada>-docnum = wa_j_1bnflin-docnum.

***            SELECT SINGLE nfenum belnr
***              FROM j_1bnfdoc
***            INTO CORRESPONDING FIELDS OF wa_j_1bnfdoc
***              WHERE docnum  EQ <wa_saida_simplicada>-docnum
***                AND cancel  EQ ''.

                READ TABLE lt_j_1bnfdoc[] ASSIGNING FIELD-SYMBOL(<lfs_j_1bnfdoc>) WITH KEY docnum = <wa_saida_simplicada>-docnum BINARY SEARCH.
                IF ( sy-subrc = 0 ).
                  wa_j_1bnfdoc = CORRESPONDING #( <lfs_j_1bnfdoc> ).
                ENDIF.

                IF wa_j_1bnfdoc IS NOT INITIAL.
                  <wa_saida_simplicada>-nfenum = wa_j_1bnfdoc-nfenum.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.




        CLEAR: wa_vbak, wa_vbfa, wa_bkpf, wa_j_1bnflin, wa_j_1bnfdoc.

        "Loop para pegar a origem e destino
***    LOOP AT it_vtpa ASSIGNING FIELD-SYMBOL(<w_vtpa>) WHERE vbeln EQ <wa_saida_simplicada>-tknum.
        LOOP AT it_sort_vtpa[] ASSIGNING FIELD-SYMBOL(<w_vtpa>) WHERE vbeln EQ <wa_saida_simplicada>-tknum.

          <wa_saida_simplicada>-parvw = <w_vtpa>-parvw.
          <wa_saida_simplicada>-adrnr = <w_vtpa>-adrnr.

***      SELECT SINGLE name1 city1
***          FROM adrc
***        INTO (v_name1, v_city1)
***        WHERE addrnumber  = <wa_saida_simplicada>-adrnr.

          READ TABLE lt_adrc[] ASSIGNING FIELD-SYMBOL(<lfs_adrc>) WITH KEY addrnumber  = <wa_saida_simplicada>-adrnr BINARY SEARCH.
          IF ( sy-subrc = 0 ).
            CONCATENATE <lfs_adrc>-name1 '-' <lfs_adrc>-city1 INTO v_descricao SEPARATED BY space.
          ENDIF.

***      CONCATENATE v_name1 '-' v_city1 INTO v_descricao SEPARATED BY space.

          IF <w_vtpa>-parvw = 'LR'.
            <wa_saida_simplicada>-kunnr = <w_vtpa>-kunnr.
            <wa_saida_simplicada>-desc_destino  = <lfs_adrc>-name1.  "v_descricao.
            <wa_saida_simplicada>-munic_destino = <lfs_adrc>-city1.
            <wa_saida_simplicada>-uf_destino    = <lfs_adrc>-region.
          ELSEIF <w_vtpa>-parvw = 'PC'.
            <wa_saida_simplicada>-lifnr = <w_vtpa>-lifnr.
            <wa_saida_simplicada>-desc_origem  = <lfs_adrc>-name1.  "v_descricao.
            <wa_saida_simplicada>-munic_origem = <lfs_adrc>-city1.
            <wa_saida_simplicada>-uf_origem    = <lfs_adrc>-region.
          ELSEIF <w_vtpa>-parvw = 'MT'.

            <wa_saida_simplicada>-cod_motorista = <w_vtpa>-lifnr.
            <wa_saida_simplicada>-nome_motorista = <lfs_adrc>-name1."v_name1.

***        SELECT SINGLE stcd2 FROM lfa1
***          INTO cpf_motorista
***         WHERE lifnr = <w_vtpa>-lifnr.

***        <wa_saida_simplicada>-cpf_motorista = cpf_motorista.

            READ TABLE lt_lfa1[] ASSIGNING FIELD-SYMBOL(<lfs_lfa1>) WITH KEY lifnr = <w_vtpa>-lifnr BINARY SEARCH.
            IF ( sy-subrc = 0 ).
              <wa_saida_simplicada>-cpf_motorista = <lfs_lfa1>-stcd2.
            ENDIF.

          ENDIF.

        ENDLOOP.

        SELECT SINGLE *
          FROM vfkp
        INTO CORRESPONDING FIELDS OF wa_vfkp
          WHERE rebel = <wa_saida_simplicada>-tknum
            AND fkpty = 'Z001'.

* Ini - RJF - CS2022001178 AJUSTE DE VALOR DE PEDAGIO FROTA RODOVIARIA - 2023.01
        "*---> 06/07/2023 - Migração S4 - DG / IR158761 / AOENNING
*        IF wa_vfkp-knumv IS NOT INITIAL.
*          select distinct * "KONV-KBETR
*            UP TO 1 ROWS
*            FROM konv
*          INTO @DATA(wa_konv)
*            WHERE knumv = @wa_vfkp-knumv
*              AND kschl = 'ZPED'.
*          ENDSELECT.
*        ENDIF.

* Fim - RJF - CS2022001178 AJUSTE DE VALOR DE PEDAGIO FROTA RODOVIARIA - 2023.01

*        SELECT SINGLE * FROM prcd_elements INTO wa_prcd_elements
*                WHERE  knumv EQ wa_vfkp-knumv
*                AND  kschl EQ 'ZPED'.
*
*        CLEAR sl_konv.
*        MOVE-CORRESPONDING wa_prcd_elements TO sl_konv.
**        sl_konv-kbetr = ( wa_prcd_elements-kbetr / '1000.000' ). "conversao de tipos ALRS
*        sl_konv-kbetr = wa_prcd_elements-kwert. "ALRS


        "*---> 06/07/2023 - Migração S4 - DG / IR158761 / AOENNING

        <wa_saida_simplicada>-kzwi1 =  wa_vfkp-kzwi1. "rjf
        <wa_saida_simplicada>-fknum =  wa_vfkp-fknum.
*        IF <wa_saida_simplicada>-tipo <> 'PRÓPRIO'.
**          <wa_saida_simplicada>-mwsbp = wa_vfkp-mwsbp.
*          <wa_saida_simplicada>-mwsbp = wa_vfsi-KZWI2. "RJF
*        ENDIF.

        READ TABLE it_vttp ASSIGNING FIELD-SYMBOL(<ws_it_vttp>) WITH KEY tknum = <wa_saida_simplicada>-tknum.

        CLEAR: wa_lips.
        READ TABLE it_lips INTO wa_lips WITH KEY vbeln = <ws_it_vttp>-vbeln .
        IF sy-subrc IS INITIAL.
          <wa_saida_simplicada>-ov_pedido = wa_lips-vgbel.

          READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<ws_vbak>) WITH KEY vbeln = wa_lips-vgbel .
          IF ( sy-subrc EQ 0 ).
            IF  ( wa_lips-pstyv NE 'ELN' AND wa_lips-pstyv NE 'NLN').
              IF wa_lips-matkl = '700110'.
                <wa_saida_simplicada>-transgenese = <ws_vbak>-kvgr3.
              ENDIF.
              <wa_saida_simplicada>-tp_ov = <ws_vbak>-auart.

*            READ TABLE t_vbfa INTO DATA(ws_vbfa) WITH KEY vbeln = <ws_it_vttp>-vbeln
*                                                                       vbtyp_n = 'M'
*                                                                       vbtyp_v = 'J'.


              READ TABLE t_vbfa INTO DATA(ws_vbfa) WITH KEY vbelv = wa_lips-vbeln
                                                         vbtyp_n  = 'M'
                                                         vbtyp_v  = 'J'
                                                         fktyp = 'L'. "169673 IR224170 ZLES0177 NF remessa  por conta e ordem PSA
              IF sy-subrc EQ 0.
                READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<ws_j_1bnflin>) WITH KEY refkey = ws_vbfa-vbeln.
                IF sy-subrc EQ 0.
                  READ TABLE t_j_1bnfe_active ASSIGNING FIELD-SYMBOL(<ls_1bnfe_active>) WITH KEY docnum = <ws_j_1bnflin>-docnum.
                  IF sy-subrc EQ 0.
                    READ TABLE t_zlest0039 ASSIGNING FIELD-SYMBOL(<ws_zlest0039>) WITH KEY docnum = <ls_1bnfe_active>-docnum.
                    IF sy-subrc EQ 0.

*                      IF <ws_zlest0039>-pontotransb NE 0.
*                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datatransb.
*                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesotransb.
*                      ELSE.
*                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datachegada.
*                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesochegada.
*                      ENDIF.

                      <wa_saida_simplicada>-cod_tran_efetivo = <ws_zlest0039>-transb_efetivo.
*                      <wa_saida_simplicada>-hora = <ws_zlest0039>-hora. "Hora / 111592 CS2023000322 SIM4.0 - Inserir a hora na coluna Data Documento ZLES0177 PSA

                      READ TABLE it_kna1 ASSIGNING FIELD-SYMBOL(<ws_kna1>) WITH KEY kunnr = <ws_zlest0039>-transb_efetivo.
                      IF sy-subrc EQ 0.
                        <wa_saida_simplicada>-descr_tran_efetivo = <ws_kna1>-name1.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          IF wa_lips-pstyv EQ 'NLN'.
            CLEAR: ws_vbfa.
            READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbeln = wa_lips-vgbel
                                                  vbtyp_n = 'R'
                                                  vbtyp_v = 'J'.


            IF ( sy-subrc = 0 ).
              CLEAR: vs_refkey.
              vs_refkey = |{ ws_vbfa-vbeln }{ ws_vbfa-mjahr }|.
              READ TABLE t_j_1bnflin ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
              IF sy-subrc EQ 0.
                READ TABLE t_j_1bnfe_active ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
                IF sy-subrc EQ 0.
                  READ TABLE t_zlest0039 ASSIGNING <ws_zlest0039> WITH KEY docnum = <ls_1bnfe_active>-docnum.
                  IF sy-subrc EQ 0.
*                    IF <ws_zlest0039>-pontotransb NE 0.
*                      <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datatransb.
*                      <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesotransb.
*                    ELSE.
*                      <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datachegada.
*                      <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesochegada.
*                    ENDIF.

                    <wa_saida_simplicada>-cod_tran_efetivo = <ws_zlest0039>-transb_efetivo.

                    READ TABLE it_kna1 ASSIGNING <ws_kna1> WITH KEY kunnr = <ws_zlest0039>-transb_efetivo.
                    IF sy-subrc EQ 0.
                      <wa_saida_simplicada>-descr_tran_efetivo = <ws_kna1>-name1.
                    ENDIF.
                  ENDIF.


                  "Monta chave NFe.
                  <wa_saida_simplicada>-chave_nfe = <ls_1bnfe_active>-regio &&
                                           <ls_1bnfe_active>-nfyear &&
                                           <ls_1bnfe_active>-nfmonth &&
                                           <ls_1bnfe_active>-stcd1 &&
                                           <ls_1bnfe_active>-model &&
                                           <ls_1bnfe_active>-serie &&
                                           <ls_1bnfe_active>-nfnum9 &&
                                           <ls_1bnfe_active>-docnum9 &&
                                           <ls_1bnfe_active>-cdv.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: ws_vbfa.
        READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbelv = wa_lips-vbeln
                                                         vbtyp_n  = 'M'
                                                         vbtyp_v  = 'J'
                                                         fktyp = 'L'. "169673 IR224170 ZLES0177 NF remessa  por conta e ordem PSA
        CLEAR: vs_refkey.
        vs_refkey = ws_vbfa-vbeln.
        IF ws_vbfa IS INITIAL.
          READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbelv = wa_lips-vbeln
                                                       vbtyp_n  = 'R'
                                                       vbtyp_v  = 'J'.

          CLEAR: vs_refkey.
          vs_refkey = |{ ws_vbfa-vbeln }{ ws_vbfa-mjahr }|.
        ENDIF.

        IF <wa_saida_simplicada>-tipo EQ 'PRÓPRIO'.
          IF sy-subrc EQ 0.
            READ TABLE t_j_1bnflin ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
            IF sy-subrc EQ 0.
              READ TABLE t_j_1bnfe_active ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
              IF sy-subrc EQ 0.
                "Monta chave NFe.
                <wa_saida_simplicada>-chave_nfe = <ls_1bnfe_active>-regio &&
                                         <ls_1bnfe_active>-nfyear &&
                                         <ls_1bnfe_active>-nfmonth &&
                                         <ls_1bnfe_active>-stcd1 &&
                                         <ls_1bnfe_active>-model &&
                                         <ls_1bnfe_active>-serie &&
                                         <ls_1bnfe_active>-nfnum9 &&
                                         <ls_1bnfe_active>-docnum9 &&
                                         <ls_1bnfe_active>-cdv.


                READ TABLE t_zsdt0105 ASSIGNING FIELD-SYMBOL(<ws_zsdt0105>) WITH KEY docnum = <ls_1bnfe_active>-docnum.
                IF sy-subrc EQ 0.
                  READ TABLE t_zsdt0102 ASSIGNING FIELD-SYMBOL(<ws_zsdt0102>) WITH KEY docnum = <ws_zsdt0105>-docnum_ref.
                  IF sy-subrc EQ 0.
                    <wa_saida_simplicada>-nr_mdfe = <ws_zsdt0102>-nmdfe.
                    <wa_saida_simplicada>-docnum_mdfe = <ws_zsdt0102>-docnum.
                    <wa_saida_simplicada>-data_mdfe = <ws_zsdt0102>-data_emi.  " RJF
                    <wa_saida_simplicada>-hora_mdfe = <ws_zsdt0102>-hora_emi.  " RJF
                  ENDIF.
                ELSE.
                  READ TABLE it_zsdt0241_aux INTO DATA(wa_zsdt0241_aux) WITH KEY chave = <wa_saida_simplicada>-chave_nfe.
                  IF sy-subrc EQ 0.
                    <wa_saida_simplicada>-docnum_mdfe = wa_zsdt0241_aux-docnum.
                  ENDIF.

                  READ TABLE t_j_1bnfdoc INTO DATA(wa_doc) WITH KEY docnum = <ls_1bnfe_active>-docnum.
                  IF sy-subrc EQ 0.
                    <wa_saida_simplicada>-nr_mdfe     = wa_doc-nfenum.
                    <wa_saida_simplicada>-data_mdfe   = wa_doc-credat.
                    <wa_saida_simplicada>-hora_mdfe   = wa_doc-cretim.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

*-IR212606-18.12.2024-#160894-JT-inicio
          IF <wa_saida_simplicada>-nr_mdfe IS INITIAL.
            READ TABLE tg_j_1bnflin_rt ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
            IF sy-subrc EQ 0.
              READ TABLE tg_j_1bnfe_active_rt ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
              IF sy-subrc EQ 0.
                READ TABLE t_zsdt0105_rt ASSIGNING FIELD-SYMBOL(<ws_zsdt0105_rt>) WITH KEY docnum = <ls_1bnfe_active>-docnum.
                IF sy-subrc EQ 0.
                  READ TABLE t_zsdt0102_rt ASSIGNING FIELD-SYMBOL(<ws_zsdt0102_rt>) WITH KEY docnum = <ws_zsdt0105_rt>-docnum_ref.
                  IF sy-subrc EQ 0.
                    <wa_saida_simplicada>-nr_mdfe     = <ws_zsdt0102_rt>-nmdfe.
                    <wa_saida_simplicada>-docnum_mdfe = <ws_zsdt0102_rt>-docnum.
                    <wa_saida_simplicada>-data_mdfe   = <ws_zsdt0102_rt>-data_emi.  " RJF
                    <wa_saida_simplicada>-hora_mdfe   = <ws_zsdt0102_rt>-hora_emi.  " RJF
                  ENDIF.
                ENDIF.

                "Monta chave NFe.
                <wa_saida_simplicada>-chave_nfe = <ls_1bnfe_active>-regio  && <ls_1bnfe_active>-nfyear  && <ls_1bnfe_active>-nfmonth &&
                                                  <ls_1bnfe_active>-stcd1  && <ls_1bnfe_active>-model   && <ls_1bnfe_active>-serie   &&
                                                  <ls_1bnfe_active>-nfnum9 && <ls_1bnfe_active>-docnum9 && <ls_1bnfe_active>-cdv.
              ENDIF.
            ENDIF.
          ENDIF.
*-IR212606-18.12.2024-#160894-JT-fim
        ENDIF.

        CLEAR: wa_vbak, wa_vbfa, wa_j_1bnflin.
        IF <wa_saida_simplicada>-tipo NE 'PRÓPRIO'.
          READ TABLE lt_vbak ASSIGNING <lfs_vbak> WITH KEY tknum = <wa_saida_simplicada>-tknum BINARY SEARCH.
          IF ( sy-subrc = 0 ).
            wa_vbak = CORRESPONDING #( <lfs_vbak> ).
          ENDIF.

          READ TABLE lt_vbfa ASSIGNING <lfs_vbfa> WITH KEY vbelv = wa_vbak-vbeln. " BINARY SEARCH.
          IF ( sy-subrc = 0 ).
            wa_vbfa = CORRESPONDING #( <lfs_vbfa> ).
          ENDIF.

          READ TABLE lt_j_1bnflin[] ASSIGNING <lfs_j_1bnflin> WITH KEY refkey = wa_vbfa-vbeln BINARY SEARCH.
          IF ( sy-subrc = 0 ).
            wa_j_1bnflin = CORRESPONDING #( <lfs_j_1bnflin> ).
          ENDIF.

          READ TABLE it_zsdt0105 ASSIGNING <ws_zsdt0105> WITH KEY docnum = wa_j_1bnflin-docnum.
          IF sy-subrc EQ 0.
            READ TABLE it_zsdt0102 ASSIGNING <ws_zsdt0102> WITH KEY docnum = <ws_zsdt0105>-docnum_ref.
            IF sy-subrc EQ 0.
              <wa_saida_simplicada>-nr_mdfe = <ws_zsdt0102>-nmdfe.
              <wa_saida_simplicada>-docnum_mdfe = <ws_zsdt0102>-docnum.
              <wa_saida_simplicada>-data_mdfe = <ws_zsdt0102>-data_emi.  " RJF
              <wa_saida_simplicada>-hora_mdfe = <ws_zsdt0102>-hora_emi.  " RJF
            ENDIF.
          ENDIF.
        ENDIF.

        "Dados chave cte.
        READ TABLE t_vbak_aux INTO ls_vbak WITH KEY tknum =  <wa_saida_simplicada>-tknum.
        IF sy-subrc EQ 0.
          READ TABLE t_vbfa_aux INTO ls_vbfa WITH KEY vbelv      =  ls_vbak-vbeln.
          IF sy-subrc EQ 0.
            READ TABLE t_j_1bnflin_aux INTO ls_j_1bnflin WITH KEY refkey = ls_vbfa-vbeln.
            IF sy-subrc EQ 0.
              READ TABLE t_j_1bnfdoc_aux INTO ls_j_1bnfdoc WITH KEY docnum = ls_j_1bnflin-docnum.
              IF sy-subrc EQ 0.
                READ TABLE t_j_1bnfe_active_aux INTO ls_1bnfe_active WITH KEY docnum = ls_j_1bnfdoc-docnum.
                IF sy-subrc EQ 0.
                  <wa_saida_simplicada>-cd_chave_cte = ls_1bnfe_active-regio &&
                                             ls_1bnfe_active-nfyear &&
                                             ls_1bnfe_active-nfmonth &&
                                             ls_1bnfe_active-stcd1 &&
                                             ls_1bnfe_active-model &&
                                             ls_1bnfe_active-serie &&
                                             ls_1bnfe_active-nfnum9 &&
                                             ls_1bnfe_active-docnum9 &&
                                             ls_1bnfe_active-cdv.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.

***    LOOP AT it_vttp INTO wa_vttp WHERE tknum EQ <wa_saida_simplicada>-tknum.
        LOOP AT it_sort_vttp INTO wa_vttp WHERE tknum EQ <wa_saida_simplicada>-tknum.

          <wa_saida_simplicada>-vttp_vbeln = wa_vttp-vbeln.

          SELECT SINGLE *
            FROM vfsi
           INTO @DATA(wa_vfsi)
          WHERE knumv EQ @wa_vfkp-knumv
            AND vbeln EQ @wa_vttp-vbeln.

          IF <wa_saida_simplicada>-tipo <> 'PRÓPRIO'. "RJF
            <wa_saida_simplicada>-mwsbp = wa_vfsi-kzwi2. "RJF
          ENDIF. "RJF

*          <wa_saida_simplicada>-netwr = wa_vfsi-netwr. "WPP
          <wa_saida_simplicada>-netwr = wa_vfsi-kzwi1. "ALRS
          "ALRS
          SELECT SINGLE * FROM prcd_elements INTO wa_prcd_elements
            WHERE  knumv EQ wa_vfsi-knumv
            AND    kposn EQ wa_vfsi-kposn
            AND    kschl EQ 'ZPED'.

          CLEAR sl_konv.
          MOVE-CORRESPONDING wa_prcd_elements TO sl_konv.
          sl_konv-kbetr = wa_prcd_elements-kwert. "ALRS

          "ALRS

          SELECT SINGLE belnr bukrs gjahr
            FROM bkpf
            INTO CORRESPONDING FIELDS OF wa_bkpf
            WHERE xblnr = <wa_saida_simplicada>-xblnr
             AND budat >= <wa_saida_simplicada>-erdat.

          <wa_saida_simplicada>-bukrs = wa_bkpf-bukrs.
          <wa_saida_simplicada>-gjahr = wa_bkpf-gjahr.

          SELECT SINGLE dmbtr,dmbe2
            FROM bsis_view
           INTO CORRESPONDING FIELDS OF @wa_bsis
          WHERE belnr = @wa_bkpf-belnr
           AND bukrs = @<wa_saida_simplicada>-bukrs
           AND gjahr = @<wa_saida_simplicada>-gjahr
           AND shkzg = 'S'.

          CLEAR: wa_lips.
          READ TABLE it_lips INTO wa_lips WITH KEY vbeln = <wa_saida_simplicada>-vttp_vbeln BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <wa_saida_simplicada>-ov_pedido = wa_lips-vgbel. "RJF
            <wa_saida_simplicada>-matnr1 = wa_lips-matnr.
            <wa_saida_simplicada>-ntgew = wa_lips-brgew.
            <wa_saida_simplicada>-grupo_produto = wa_lips-matkl.
            READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<ws_vbak_aux>) WITH KEY vbeln = wa_lips-vgbel .
            IF ( sy-subrc EQ 0 ).

              IF  ( wa_lips-pstyv NE 'ELN' AND wa_lips-pstyv NE 'NLN').
                READ TABLE t_vbfa INTO DATA(ws_vbfa_aux) WITH KEY vbelv = wa_lips-vbeln
                                                        vbtyp_n  = 'M'
                                                        vbtyp_v  = 'J'
                                                        fktyp = 'L'. "169673 IR224170 ZLES0177 NF remessa  por conta e ordem PSA
                IF sy-subrc EQ 0.
                  READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<ws_j_1bnflin_aux>) WITH KEY refkey = ws_vbfa_aux-vbeln.
                  IF sy-subrc EQ 0.
                    READ TABLE t_j_1bnfe_active ASSIGNING FIELD-SYMBOL(<ls_1bnfe_active_aux>) WITH KEY docnum = <ws_j_1bnflin_aux>-docnum.
                    IF sy-subrc EQ 0.
                      READ TABLE t_zlest0039 ASSIGNING FIELD-SYMBOL(<ws_zlest0039_aux>) WITH KEY docnum = <ls_1bnfe_active_aux>-docnum.
                      IF sy-subrc EQ 0.

                        IF <ws_zlest0039_aux>-pontotransb NE 0.
                          <wa_saida_simplicada>-dt_descarga = <ws_zlest0039_aux>-datatransb.
                          <wa_saida_simplicada>-peso_descarga = <ws_zlest0039_aux>-pesotransb.
                        ELSE.
                          <wa_saida_simplicada>-dt_descarga = <ws_zlest0039_aux>-datachegada.
                          <wa_saida_simplicada>-peso_descarga = <ws_zlest0039_aux>-pesochegada.
                        ENDIF.
                      ENDIF.

                      "Monta chave NFe.
                      <wa_saida_simplicada>-chave_nfe = <ls_1bnfe_active_aux>-regio &&
                                               <ls_1bnfe_active_aux>-nfyear &&
                                               <ls_1bnfe_active_aux>-nfmonth &&
                                               <ls_1bnfe_active_aux>-stcd1 &&
                                               <ls_1bnfe_active_aux>-model &&
                                               <ls_1bnfe_active_aux>-serie &&
                                               <ls_1bnfe_active_aux>-nfnum9 &&
                                               <ls_1bnfe_active_aux>-docnum9 &&
                                               <ls_1bnfe_active_aux>-cdv.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            IF wa_lips-pstyv EQ 'NLN'.
              CLEAR: ws_vbfa.
              READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbeln = wa_lips-vgbel
                                                    vbtyp_n = 'R'
                                                    vbtyp_v = 'J'.


              IF ( sy-subrc = 0 ).
                CLEAR: vs_refkey.
                vs_refkey = |{ ws_vbfa-vbeln }{ ws_vbfa-mjahr }|.
                READ TABLE t_j_1bnflin ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
                IF sy-subrc EQ 0.
                  READ TABLE t_j_1bnfe_active ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
                  IF sy-subrc EQ 0.
                    READ TABLE t_zlest0039 ASSIGNING <ws_zlest0039> WITH KEY docnum = <ls_1bnfe_active>-docnum.
                    IF sy-subrc EQ 0.
                      IF <ws_zlest0039>-pontotransb NE 0.
                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datatransb.
                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesotransb.
                      ELSE.
                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datachegada.
                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesochegada.
                      ENDIF.

                    ENDIF.
                    "Monta chave NFe.
                    <wa_saida_simplicada>-chave_nfe = <ls_1bnfe_active>-regio &&
                                             <ls_1bnfe_active>-nfyear &&
                                             <ls_1bnfe_active>-nfmonth &&
                                             <ls_1bnfe_active>-stcd1 &&
                                             <ls_1bnfe_active>-model &&
                                             <ls_1bnfe_active>-serie &&
                                             <ls_1bnfe_active>-nfnum9 &&
                                             <ls_1bnfe_active>-docnum9 &&
                                             <ls_1bnfe_active>-cdv.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.



*------------------------------------------------------------------------------------
* "Processo para encontrar a chave NFe - Remessa por Conta e Ordem
*------------------------------------------------------------------------------------
            IF wa_lips-vgtyp  EQ 'C'.
              CLEAR: ws_vbfa.

              SELECT SINGLE * FROM vbfa WHERE vbelv = @wa_lips-vbeln AND vbtyp_n = '8' AND vbtyp_v = 'J' INTO @ws_vbfa.  "PSA IR224170   ZLES0177  - Dados MDF-e avulsa

              "READ TABLE tg_vbfa_r INTO ws_vbfa WITH KEY vbeln = wa_lips-vbeln
              "vbtyp_n = '8' "'M' "ws_zlest0213, "PSA IR224170   ZLES0177  - Dados MDF-e avulsa
              "vbtyp_v = 'J'.

              IF sy-subrc eq 0.  "PSA IR224170   ZLES0177  - Dados MDF-e avulsa

                SELECT SINGLE * FROM zlest0213 WHERE vbeln EQ @ws_vbfa-vbelv INTO @DATA(wa_zlest0213_pco). "ws_zlest0213, "PSA IR224170   ZLES0177  - Dados MDF-e avulsa
                IF sy-subrc = 0.
                  SELECT SINGLE a~nmdfe AS nr_mdfe, a~docnum AS docnum_mdfe, a~data_emi AS data_mdfe, a~hora_emi AS hora_mdfe, b~chave AS chave_nfe
                  FROM zsdt0102 AS a
                  INNER JOIN zsdt0241 AS b ON b~docnum EQ a~docnum
                  INTO  CORRESPONDING FIELDS OF @<wa_saida_simplicada>
                 WHERE a~autorizado = @abap_true
                   AND a~estornado = @abap_false
                   AND a~cancel    = @abap_false
                    AND b~chave = @wa_zlest0213_pco-chave.
                ENDIF.



*                READ TABLE tg_zlest0213_r INTO DATA(ws_zlest0213) WITH KEY vbeln = wa_lips-vbeln. "ws_zlest0213, "PSA IR224170   ZLES0177  - Dados MDF-e avulsa
*                IF sy-subrc EQ 0.
*                  "********************************************************************** 177935 IR224170   ZLES0177  - Dados MDF-e avulsa
*                  SELECT SINGLE a~nmdfe AS nr_mdfe, a~docnum AS docnum_mdfe, a~data_emi AS data_mdfe, a~hora_emi AS hora_mdfe, b~chave AS chave_nfe
*                  FROM zsdt0102 AS a
*                  INNER JOIN zsdt0241 AS b ON b~docnum EQ a~docnum
*                  INTO  CORRESPONDING FIELDS OF @<wa_saida_simplicada>
*                 WHERE a~autorizado = @abap_true
*                   AND a~estornado = @abap_false
*                   AND a~cancel    = @abap_false
*                    AND b~chave = @ws_zlest0213-chave.
*
*                ENDIF.
                "********************************************************************** 177935 IR224170   ZLES0177  - Dados MDF-e avulsa
              ENDIF.
            ELSE.
              LOOP AT tg_j_1bnflin_r INTO DATA(ws_1bnflin_r) WHERE refkey EQ ws_vbfa-vbeln.
                READ TABLE tg_j_1bnfe_active_r INTO DATA(lw_1bnfe_active) WITH KEY docnum = ws_1bnflin_r-docnum.
                IF sy-subrc EQ 0.
                  "Monta chave NFe.
                  <wa_saida_simplicada>-chave_nfe = lw_1bnfe_active-regio &&
                                           lw_1bnfe_active-nfyear &&
                                           lw_1bnfe_active-nfmonth &&
                                           lw_1bnfe_active-stcd1 &&
                                           lw_1bnfe_active-model &&
                                           lw_1bnfe_active-serie &&
                                           lw_1bnfe_active-nfnum9 &&
                                           lw_1bnfe_active-docnum9 &&
                                           lw_1bnfe_active-cdv.

                ENDIF.
              ENDLOOP.
            ENDIF.

*------------------------------------------------------------------------------------
* "Processo para encontrar a chave NFe - *Remessas de transferência*
*------------------------------------------------------------------------------------
            IF wa_lips-vgtyp  EQ 'V' AND wa_lips-pstyv  EQ 'NLN'.
              READ TABLE tg_vbfa_rt INTO DATA(ws_vbfa_rt) WITH KEY vbelv = wa_lips-vbeln
                                                                    vbtyp_n  = 'R'
                                                                     vbtyp_v  = 'J'.

              IF sy-subrc EQ 0.
                CLEAR: vg_refkey.
                vg_refkey = |{ ws_vbfa_rt-vbeln }{ ws_vbfa_rt-mjahr } | .
                LOOP AT tg_j_1bnflin_rt INTO DATA(ws_j_1bnflin_rt) WHERE  refkey = vg_refkey
                                                                      AND reftyp = 'MD'.

                  READ TABLE tg_j_1bnfe_active_rt INTO DATA(lw_1bnfe_active_rt) WITH KEY docnum = ws_j_1bnflin_rt-docnum.
                  IF sy-subrc EQ 0 AND <wa_saida_simplicada>-chave_nfe IS INITIAL.
                    "Monta chave NFe.
                    <wa_saida_simplicada>-chave_nfe = lw_1bnfe_active_rt-regio &&
                                             lw_1bnfe_active_rt-nfyear &&
                                             lw_1bnfe_active_rt-nfmonth &&
                                             lw_1bnfe_active_rt-stcd1 &&
                                             lw_1bnfe_active_rt-model &&
                                             lw_1bnfe_active_rt-serie &&
                                             lw_1bnfe_active_rt-nfnum9 &&
                                             lw_1bnfe_active_rt-docnum9 &&
                                             lw_1bnfe_active_rt-cdv.

                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.

*------------------------------------------------------------------------------------
* "Processo para encontrar a chave NFe - *Aviso de Recebimento*
*------------------------------------------------------------------------------------
            IF wa_lips-vgtyp  EQ 'V' AND wa_lips-pstyv  EQ 'ELN'.
              READ TABLE tg_likp_a INTO DATA(ws_likp_a) WITH KEY vbeln = wa_lips-vbeln.
              "tcode = 'ZLES0136'. "*-IR212606-24.12.2024-#161504-JT-inicio
              IF sy-subrc EQ 0.
                LOOP AT tg_zfiwrt0008_a INTO DATA(ws_zfiwrt0008_a) WHERE ch_referencia = ws_likp_a-berot.
                  READ TABLE tg_j_1bnflin_a INTO DATA(ws_1bnflin_a) WITH KEY refkey = ws_zfiwrt0008_a-seq_lcto reftyp = 'ZW'.
                  IF sy-subrc EQ 0.
                    READ TABLE tg_j_1bnfe_active_a INTO DATA(lw_1bnfe_active_a) WITH KEY docnum = ws_1bnflin_a-docnum.
                    IF sy-subrc EQ 0 AND <wa_saida_simplicada>-chave_nfe IS INITIAL.

                      SELECT SINGLE a~nmdfe AS nr_mdfe, a~docnum AS docnum_mdfe, a~data_emi AS data_mdfe, a~hora_emi AS hora_mdfe
            FROM zsdt0102 AS a
            INNER JOIN zsdt0105 AS b ON b~docnum_ref EQ a~docnum
            INTO  CORRESPONDING FIELDS OF @<wa_saida_simplicada>
           WHERE b~docnum = @lw_1bnfe_active_a-docnum
            AND  a~autorizado = @abap_true
             AND a~estornado = @abap_false
             AND a~cancel    = @abap_false.
*-IR212606-18.12.2024-#160894-JT-fim

*>>>>>>>>>>>Inicio ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
                      <wa_saida_simplicada>-chave_nfe = lw_1bnfe_active_a-regio &&
                                             lw_1bnfe_active_a-nfyear &&
                                             lw_1bnfe_active_a-nfmonth &&
                                             lw_1bnfe_active_a-stcd1 &&
                                             lw_1bnfe_active_a-model &&
                                             lw_1bnfe_active_a-serie &&
                                             lw_1bnfe_active_a-nfnum9 &&
                                             lw_1bnfe_active_a-docnum9 &&
                                             lw_1bnfe_active_a-cdv.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ELSE.
                READ TABLE tg_likp_ux INTO ws_likp_a WITH KEY vbeln = wa_lips-vbeln.
                IF sy-subrc EQ 0.
                  LOOP AT tg_j_1bnfdoc_a INTO DATA(ws_j_1bnfdoc_a) WHERE   parid  = ws_likp_a-lifnr
                                                                     AND   nfenum = ws_likp_a-nr_nf
                         AND ( series  = ws_likp_a-serie_nf OR   series = ws_likp_a-serie_nf1 OR   series = ws_likp_a-serie_nf2 ).  "*-IR212606-18.12.2024-#160894-JT


                    READ TABLE tg_j_1bnfe_active_a INTO lw_1bnfe_active_a WITH KEY docnum = ws_j_1bnfdoc_a-docnum.
                    IF sy-subrc EQ 0 AND <wa_saida_simplicada>-chave_nfe IS INITIAL.
                      <wa_saida_simplicada>-chave_nfe = lw_1bnfe_active_a-regio &&
                                             lw_1bnfe_active_a-nfyear &&
                                             lw_1bnfe_active_a-nfmonth &&
                                             lw_1bnfe_active_a-stcd1 &&
                                             lw_1bnfe_active_a-model &&
                                             lw_1bnfe_active_a-serie &&
                                             lw_1bnfe_active_a-nfnum9 &&
                                             lw_1bnfe_active_a-docnum9 &&
                                             lw_1bnfe_active_a-cdv.

                      READ TABLE it_zsdt0241_aux INTO wa_zsdt0241_aux WITH KEY chave = <wa_saida_simplicada>-chave_nfe.
                      IF sy-subrc EQ 0.
                        "********************************************************************** 177935 IR224170   ZLES0177  - Dados MDF-e avulsa

                        SELECT SINGLE nmdfe, docnum, data_emi, hora_emi
          FROM zsdt0102 AS a
          WHERE docnum =  @wa_zsdt0241_aux-docnum
          AND  autorizado = @abap_true
           AND estornado = @abap_false
           AND cancel    = @abap_false
                          INTO @DATA(wa_zsdt0102).
                        IF sy-subrc = 0.
                          <wa_saida_simplicada>-nr_mdfe     = wa_zsdt0102-nmdfe.
                          <wa_saida_simplicada>-docnum_mdfe = wa_zsdt0102-docnum.
                          <wa_saida_simplicada>-data_mdfe   = wa_zsdt0102-data_emi.
                          <wa_saida_simplicada>-hora_mdfe   = wa_zsdt0102-hora_emi.
                        ENDIF.
                        "********************************************************************** 177935 IR224170   ZLES0177  - Dados MDF-e avulsa
*                        <wa_saida_simplicada>-nr_mdfe     = ws_j_1bnfdoc_a-nfenum.
*                        <wa_saida_simplicada>-docnum_mdfe = wa_zsdt0241_aux-docnum.
*                        <wa_saida_simplicada>-data_mdfe   = ws_j_1bnfdoc_a-credat.
*                        <wa_saida_simplicada>-hora_mdfe   = ws_j_1bnfdoc_a-cretim.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.

                  READ TABLE it_zmmt_zgr_docs INTO DATA(ws_zgr_docs) WITH KEY av_vbeln = wa_lips-vbeln.
                  IF sy-subrc EQ 0.
                    READ TABLE tg_j_1bnfe_active_a INTO lw_1bnfe_active_a WITH KEY docnum = ws_zgr_docs-docnum.
                    IF sy-subrc EQ 0 AND <wa_saida_simplicada>-chave_nfe IS INITIAL.
                      <wa_saida_simplicada>-chave_nfe = lw_1bnfe_active_a-regio &&
                                             lw_1bnfe_active_a-nfyear &&
                                             lw_1bnfe_active_a-nfmonth &&
                                             lw_1bnfe_active_a-stcd1 &&
                                             lw_1bnfe_active_a-model &&
                                             lw_1bnfe_active_a-serie &&
                                             lw_1bnfe_active_a-nfnum9 &&
                                             lw_1bnfe_active_a-docnum9 &&
                                             lw_1bnfe_active_a-cdv.


                      SELECT SINGLE a~nmdfe AS nr_mdfe, a~docnum AS docnum_mdfe, a~data_emi AS data_mdfe, a~hora_emi AS hora_mdfe
                        FROM zsdt0102 AS a
                        INNER JOIN zsdt0105 AS b ON b~docnum_ref EQ a~docnum
                        INTO  CORRESPONDING FIELDS OF @<wa_saida_simplicada>
                       WHERE b~docnum = @lw_1bnfe_active_a-docnum
                        AND  a~autorizado = @abap_true
                         AND a~estornado = @abap_false
                         AND a~cancel    = @abap_false.

                      IF sy-subrc <> 0. "PSA

                        "READ TABLE it_zsdt0241_aux INTO wa_zsdt0241_aux WITH KEY chave = <wa_saida_simplicada>-chave_nfe.

                        SELECT SINGLE docnum FROM zsdt0241 WHERE chave = @<wa_saida_simplicada>-chave_nfe INTO @DATA(_docnum).
                        IF sy-subrc = 0.
                          SELECT SINGLE a~nmdfe AS nr_mdfe, a~docnum AS docnum_mdfe, a~data_emi AS data_mdfe, a~hora_emi AS hora_mdfe
                            FROM zsdt0102 AS a
                            INTO  CORRESPONDING FIELDS OF @<wa_saida_simplicada>
                            WHERE a~docnum = @_docnum
                            AND  a~autorizado = @abap_true
                             AND a~estornado = @abap_false
                             AND a~cancel    = @abap_false.
                        ENDIF.


                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
*>>>>>>>>>>>Fim ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
              ENDIF.
            ENDIF.
          ENDIF.

          <wa_saida_simplicada>-dmbe2 = wa_bsis-dmbe2.

          READ TABLE it_makt INTO wa_makt WITH KEY matnr = <wa_saida_simplicada>-matnr1 BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <wa_saida_simplicada>-maktx = wa_makt-maktx.
          ENDIF.

          IF <wa_saida_simplicada>-netwr > 0 AND <wa_saida_simplicada>-ntgew > 0.
            <wa_saida_simplicada>-preco_tonelada_rs = ( <wa_saida_simplicada>-netwr / <wa_saida_simplicada>-ntgew ) * 1000.
*            <wa_saida_simplicada>-preco_tonelada_rs = ( <wa_saida_simplicada>-kzwi1 / <wa_saida_simplicada>-ntgew ) * 1000. "RJF
          ENDIF.

*          <wa_saida_simplicada>-netwr = <wa_saida_simplicada>-kzwi1. "Valor em BRL da ZFRE

          "IF <wa_saida_simplicada>-netwr > 0 AND wa_bsis-dmbtr > 0 AND wa_bsis-dmbe2 > 0. "RJF
          TRY.
              IF <wa_saida_simplicada>-netwr NE wa_bsis-dmbtr. "RJF
                <wa_saida_simplicada>-dmbe2 = <wa_saida_simplicada>-netwr / ( wa_bsis-dmbtr / wa_bsis-dmbe2 ).
*                <wa_saida_simplicada>-dmbe2 = <wa_saida_simplicada>-kzwi1 / <wa_saida_simplicada>-taxa_dolar.
              ELSE.
                <wa_saida_simplicada>-dmbe2 = wa_bsis-dmbe2.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          IF <wa_saida_simplicada>-netwr > 0 AND <wa_saida_simplicada>-dmbe2 > 0.
            <wa_saida_simplicada>-taxa_dolar = <wa_saida_simplicada>-netwr / <wa_saida_simplicada>-dmbe2.
          ELSE.
            <wa_saida_simplicada>-taxa_dolar = wa_bsis-dmbtr.
          ENDIF.

* Ini - RJF - CS2022001178 AJUSTE DE VALOR DE PEDAGIO FROTA RODOVIARIA - 2023.01
          <wa_saida_simplicada>-kbetr = sl_konv-kbetr."wa_konv-kbetr. "*---> 06/07/2023 - Migração S4 - DG / IR158761 / AOENNING
          <wa_saida_simplicada>-vlr_frete = <wa_saida_simplicada>-netwr + sl_konv-kbetr."wa_konv-kbetr. "*---> 06/07/2023 - Migração S4 - DG / IR158761 / AOENNING
* Fim - RJF - CS2022001178 AJUSTE DE VALOR DE PEDAGIO FROTA RODOVIARIA - 2023.01

          APPEND <wa_saida_simplicada> TO it_saida_aux.

        ENDLOOP.

        CLEAR: ls_vbak, ls_vbfa, ls_j_1bnflin, ls_j_1bnfdoc, ls_1bnfe_active.
        CLEAR: lw_1bnfe_active_a, ws_j_1bnfdoc_a, ws_likp_a, lw_1bnfe_active_a, ws_1bnflin_a, ws_zfiwrt0008_a, ws_likp_a, lw_1bnfe_active, ws_1bnflin_r, "ws_zlest0213, "PSA IR224170   ZLES0177  - Dados MDF-e avulsa
                ws_vbfa_rt, lw_1bnfe_active_rt, ws_j_1bnflin_rt, ws_vbfa.

      ENDLOOP.

      IF p_placa IS NOT INITIAL.
        DELETE it_saida_aux WHERE placa_carreta NOT IN p_placa.
      ENDIF.

      IF p_prop = 'X'.
        DELETE it_saida_aux WHERE tp_veiculo = 'T'.
      ELSE.
        DELETE it_saida_aux WHERE tp_veiculo = 'P'.
      ENDIF.

    ENDIF.

    IF p_prop = 'X'.


      vl_message = 'Aguarde, selecionando dados transporte subcontratado'.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 99
          text       = vl_message.

      TYPES: BEGIN OF ty_saida_frete_subcontratado,
               vbeln   TYPE vbak-vbeln,
               vbeln_a TYPE vbfa-vbelv,
               docnum  TYPE j_1bnflin-docnum,
             END OF ty_saida_frete_subcontratado.



      DATA: t_zlest0194        TYPE TABLE OF zlest0194,
            t_saida_aux_sub    TYPE TABLE OF ty_saida_frete_subcontratado,
            t_lfa1             TYPE TABLE OF lfa1,
            t_kna1             TYPE TABLE OF kna1,
            t_bkpf             TYPE TABLE OF bkpf,
*            t_j_1bnfdoc        type table of j_1bnfdoc,
            t_zib_cte_dist_n55 TYPE TABLE OF zib_cte_dist_n55,
            t_zlest0002        TYPE TABLE OF zlest0002,
            t_vbrk             TYPE TABLE OF vbrk,
            t_saida_sub        TYPE TABLE OF ty_saida,
            ws_saida_sub       TYPE ty_saida.

      DATA: r_awkey     TYPE RANGE OF awkey,
            r_gjahr     TYPE RANGE OF gjahr,
            z_awkey     TYPE awkey,
            z_gjahr     TYPE gjahr,
            z_kurs2     TYPE kurs2,
            z_kurs2_aux TYPE char9.

      FREE: r_refkey, t_j_1bnflin, t_j_1bnfe_active, t_zlest0194, t_lfa1, t_kna1, t_kna1, t_saida_aux_sub, it_refkey.
      SELECT DISTINCT * FROM vbak
        INTO CORRESPONDING FIELDS OF TABLE t_vbak
        WHERE auart EQ 'ZSSF'
          AND erdat IN p_erdat.

      SORT t_vbak BY vbeln ASCENDING.
      DELETE ADJACENT DUPLICATES FROM t_vbak COMPARING ALL FIELDS.

      IF t_vbak IS NOT INITIAL.

        SELECT DISTINCT * FROM vbfa AS a
         INTO CORRESPONDING FIELDS OF TABLE t_vbfa
          FOR ALL ENTRIES IN t_vbak
          WHERE a~vbelv EQ t_vbak-vbeln
            AND a~vbtyp_n = 'M'
            AND a~vbtyp_v = 'C'
            AND NOT EXISTS ( SELECT DISTINCT * FROM vbfa  AS b
                                WHERE b~vbelv EQ a~vbeln
                                  AND b~vbtyp_n EQ 'N' ).

        SORT t_vbfa BY vbelv ASCENDING.
        DELETE ADJACENT DUPLICATES FROM t_vbfa COMPARING ALL FIELDS.


        IF t_vbfa IS NOT INITIAL.


          it_refkey = VALUE #( FOR l IN t_vbfa ( refkey = l-vbeln ) ).
          SORT it_refkey BY refkey ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_refkey COMPARING refkey.

          IF it_refkey IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnflin INTO TABLE t_j_1bnflin
              FOR ALL ENTRIES IN it_refkey
              WHERE refkey EQ it_refkey-refkey.

            SORT t_j_1bnflin BY refkey ASCENDING.
            DELETE ADJACENT DUPLICATES FROM t_j_1bnflin COMPARING ALL FIELDS.

          ENDIF.

          IF t_j_1bnflin IS NOT INITIAL.
            SELECT DISTINCT * FROM j_1bnfe_active INTO TABLE t_j_1bnfe_active
              FOR ALL ENTRIES IN t_j_1bnflin
              WHERE docnum EQ t_j_1bnflin-docnum.

            SORT t_j_1bnfe_active BY cancel.
            DELETE ADJACENT DUPLICATES FROM t_j_1bnfe_active COMPARING ALL FIELDS.

            IF t_j_1bnfe_active IS NOT INITIAL.
              DELETE t_j_1bnfe_active WHERE cancel EQ 'X'.
            ENDIF.

            IF t_j_1bnfe_active IS NOT INITIAL.
              SORT t_j_1bnfe_active BY docnum.
            ENDIF.
          ENDIF.

          MOVE-CORRESPONDING t_vbak TO t_saida_aux_sub.

          LOOP AT t_saida_aux_sub ASSIGNING FIELD-SYMBOL(<ws_saida>).
            READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbelv = <ws_saida>-vbeln.
            IF sy-subrc EQ 0.
              <ws_saida>-vbeln_a = ws_vbfa-vbeln.
              READ TABLE t_j_1bnflin INTO DATA(ws_j_1bnflin) WITH KEY refkey = ws_vbfa-vbeln.
              IF sy-subrc EQ 0.
                READ TABLE t_j_1bnfe_active INTO DATA(ws_j_1bnfe_active) WITH KEY docnum = ws_j_1bnflin-docnum.
                IF sy-subrc EQ 0.
                  <ws_saida>-docnum = ws_j_1bnfe_active-docnum.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          SELECT DISTINCT * FROM zlest0194 INTO TABLE t_zlest0194
            FOR ALL ENTRIES IN t_saida_aux_sub
            WHERE ov_sub EQ t_saida_aux_sub-vbeln
              AND fat_sub EQ t_saida_aux_sub-vbeln_a
              AND docnum_sub EQ t_saida_aux_sub-docnum.

          SORT t_zlest0194 BY ov_sub ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_zlest0194 COMPARING ALL FIELDS.

          CHECK t_zlest0194 IS NOT INITIAL.

          SELECT DISTINCT * FROM zib_cte_dist_ter INTO TABLE @DATA(t_cte_dist_ter) "RJF
          FOR ALL ENTRIES IN @t_zlest0194
          WHERE cd_chave_cte EQ @t_zlest0194-chave_xml_cte.

          FREE: t_lfa1.
          SELECT DISTINCT * FROM lfa1 INTO TABLE t_lfa1
            FOR ALL ENTRIES IN t_zlest0194
            WHERE lifnr EQ t_zlest0194-reme_cod_forn.

          SORT t_lfa1 BY lifnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_lfa1 COMPARING ALL FIELDS.

          SELECT DISTINCT * FROM lfa1 INTO TABLE it_lfa1
          FOR ALL ENTRIES IN t_zlest0194
          WHERE lifnr EQ t_zlest0194-motorista.

          SORT it_lfa1 BY lifnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_lfa1 COMPARING ALL FIELDS.

          SELECT DISTINCT * FROM kna1 INTO TABLE t_kna1
            FOR ALL ENTRIES IN t_zlest0194
            WHERE kunnr EQ t_zlest0194-dest_cod_cliente.

          SORT t_kna1 BY kunnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_kna1 COMPARING ALL FIELDS.


          r_awkey = VALUE #( FOR t IN t_zlest0194 ( sign   = 'I' option = 'EQ' low    = t-fat_sub ) ).
          SORT r_awkey BY low.
          DELETE ADJACENT DUPLICATES FROM r_awkey COMPARING low.

          r_gjahr = VALUE #( FOR b IN t_zlest0194 ( sign   = 'I' option = 'EQ' low    = b-dt_mov_ov(4) ) ).
          SORT r_gjahr BY low.
          DELETE ADJACENT DUPLICATES FROM r_refkey COMPARING low.

          SELECT DISTINCT * FROM bkpf INTO TABLE t_bkpf
            FOR ALL ENTRIES IN t_zlest0194
            WHERE awkey IN r_awkey
             AND  bukrs EQ  t_zlest0194-bukrs_ov
             AND  gjahr IN r_gjahr.

          SORT t_bkpf BY bukrs gjahr awkey ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_bkpf COMPARING ALL FIELDS.

          SELECT DISTINCT * FROM j_1bnfdoc INTO TABLE t_j_1bnfdoc
            FOR ALL ENTRIES IN t_zlest0194
            WHERE docnum   EQ t_zlest0194-docnum_sub.

          SORT t_j_1bnfdoc BY docnum ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc COMPARING ALL FIELDS.

          SELECT DISTINCT * FROM zlest0002 INTO TABLE t_zlest0002
            FOR ALL ENTRIES IN t_zlest0194
            WHERE pc_veiculo EQ t_zlest0194-placa_cav.

          SORT t_zlest0002 BY pc_veiculo ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_zlest0002 COMPARING ALL FIELDS.


          SELECT DISTINCT * FROM vbrk INTO TABLE t_vbrk
            FOR ALL ENTRIES IN t_zlest0194
            WHERE vbeln EQ t_zlest0194-fat_sub.

          SORT t_vbrk BY vbeln ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_vbrk COMPARING ALL FIELDS.

          SELECT DISTINCT * FROM zib_cte_dist_n55 INTO TABLE t_zib_cte_dist_n55
            FOR ALL ENTRIES IN t_zlest0194
            WHERE cd_chave_cte EQ t_zlest0194-chave_xml_cte.

          SORT t_zib_cte_dist_n55 BY cd_chave_cte ASCENDING.
          DELETE ADJACENT DUPLICATES FROM t_zib_cte_dist_n55 COMPARING ALL FIELDS.


          LOOP AT t_zlest0194 INTO DATA(ws_zlest0194).
            ws_saida_sub-erdat             = ws_zlest0194-dt_mov_ov.
            ws_saida_sub-maktx             = ws_zlest0194-ds_prod_pred.
            ws_saida_sub-placa_carreta     = ws_zlest0194-placa_cav.
            ws_saida_sub-carreta1          = ws_zlest0194-placa_car1.
            ws_saida_sub-carreta2          = ws_zlest0194-placa_car2.
            ws_saida_sub-carreta3          = ws_zlest0194-placa_car3.
            ws_saida_sub-tdlnr             = ws_zlest0194-branch_ov.
            ws_saida_sub-lifnr             = ws_zlest0194-reme_cod_forn.
            ws_saida_sub-desc_origem       = ws_zlest0194-reme_rsocial.

            ws_saida_sub-kunnr             = ws_zlest0194-dest_cod_cliente.
            ws_saida_sub-desc_destino      = ws_zlest0194-dest_rsocial.
            ws_saida_sub-tipo              = 'Subcontratação'.

            READ TABLE t_cte_dist_ter INTO DATA(ws_dist_ter) WITH KEY cd_chave_cte = ws_zlest0194-chave_xml_cte.
            IF sy-subrc IS INITIAL.
              ws_saida_sub-dt_protocolo      = ws_dist_ter-dt_protocolo. "RJF
              ws_saida_sub-hr_protocolo      = ws_dist_ter-hr_protocolo. "RJF
            ENDIF.

            ws_saida_sub-ntgew             = ws_zlest0194-qt_carga_cte.
            ws_saida_sub-netwr             = ws_zlest0194-valor_prestacao.
            ws_saida_sub-docnum            = ws_zlest0194-docnum_sub.
            ws_saida_sub-dt_descarga       = ws_zlest0194-dt_descarga.
            ws_saida_sub-peso_descarga     = ws_zlest0194-qt_descarga_cte.
            ws_saida_sub-cod_motorista     = ws_zlest0194-motorista.
            ws_saida_sub-frota             = ws_zlest0194-frota.
            ws_saida_sub-cpf_motorista     = ws_zlest0194-mot_cpf.
            ws_saida_sub-preco_tonelada_rs = ( ws_zlest0194-valor_prestacao / ws_zlest0194-qt_carga_cte ) * 1000.
            ws_saida_sub-cd_chave_cte      = ws_zlest0194-chave_cte_sub.

            READ TABLE t_zib_cte_dist_n55 INTO DATA(ws_zib_cte_dist_n55) WITH KEY cd_chave_cte = ws_zlest0194-chave_xml_cte.
            IF sy-subrc EQ 0.
              "Monta chave NFe.
              ws_saida_sub-chave_nfe = ws_zib_cte_dist_n55-n55_chave_acesso.
            ENDIF.

            READ TABLE t_lfa1 INTO DATA(ws_lfa1) WITH KEY lifnr = ws_zlest0194-reme_cod_forn.
            IF sy-subrc EQ 0.
              ws_saida_sub-munic_origem      = ws_lfa1-ort01.
              ws_saida_sub-uf_origem         = ws_lfa1-regio.
            ENDIF.

            READ TABLE t_kna1 INTO DATA(ws_kna1) WITH KEY kunnr = ws_zlest0194-dest_cod_cliente.
            IF sy-subrc EQ 0.
              ws_saida_sub-munic_destino      = ws_kna1-ort01.
              ws_saida_sub-uf_destino         = ws_kna1-regio.
            ENDIF.

            READ TABLE t_vbrk INTO DATA(ws_vbrk) WITH KEY vbeln = ws_zlest0194-fat_sub.
            IF sy-subrc EQ 0.
              ws_saida_sub-mwsbp                = ws_vbrk-mwsbk.
            ENDIF.


            CLEAR: z_awkey, z_gjahr.
            z_awkey = CONV #( ws_zlest0194-fat_sub ).
            z_gjahr = ws_zlest0194-dt_mov_ov(4).


            READ TABLE t_bkpf INTO DATA(ws_bkpf) WITH KEY awkey = z_awkey
                                              bukrs = ws_zlest0194-bukrs_ov
                                              gjahr = z_gjahr.

            IF sy-subrc EQ 0.

              IF ws_bkpf-kurs2 IS NOT INITIAL.
                z_kurs2_aux = ws_bkpf-kurs2.
                REPLACE ALL OCCURRENCES OF '/' IN z_kurs2_aux WITH space.
                REPLACE ALL OCCURRENCES OF '-' IN z_kurs2_aux WITH space.

                z_kurs2 = CONV #( z_kurs2_aux ).
              ENDIF.

              ws_saida_sub-taxa_dolar        = z_kurs2. "(desconsiderar a / do início)
* Início - 23/04/2024 - 2000005091/IR177208 - Stefanini - Ajuste Dólar Subcontratação - PRB
*              ws_saida_sub-dmbe2             = ( ws_zlest0194-valor_prestacao * z_kurs2 ).
              ws_saida_sub-dmbe2             = COND #( WHEN z_kurs2 IS NOT INITIAL THEN ( ws_zlest0194-valor_prestacao / z_kurs2 )
                                                                                   ELSE ws_zlest0194-valor_prestacao ).
* Fim    - 23/04/2024 - 2000005091/IR177208 - Stefanini - Ajuste Dólar Subcontratação - PRB
              ws_saida_sub-belnr2            = ws_bkpf-belnr.
            ENDIF.

            READ TABLE t_j_1bnfdoc INTO DATA(ws_j_1bnfdoc) WITH KEY docnum = ws_zlest0194-docnum_sub.
            IF sy-subrc EQ 0.
              ws_saida_sub-nfenum             = ws_j_1bnfdoc-nfenum.
            ENDIF.

*Início - FA - 30/11/2022 - CS2022000700 - ZLES0177 mostrando  faturamento com erro
            READ TABLE t_j_1bnfe_active INTO DATA(ws_j_1bnfe_active_aux) WITH KEY docnum = ws_zlest0194-docnum_sub.
            IF sy-subrc EQ 0.
              IF  ws_j_1bnfe_active_aux-docsta <> 1.
                CONTINUE.
              ENDIF.

            ENDIF.
*Fim - FA - 30/11/2022 - CS2022000700 - ZLES0177 mostrando  faturamento com erro

            READ TABLE t_zlest0002 INTO DATA(ws_zlest0002) WITH KEY pc_veiculo = ws_zlest0194-placa_cav.
            IF sy-subrc EQ 0.
              ws_saida_sub-grupo_veiculo        =  ws_zlest0002-grupo.
            ENDIF.

            CLEAR: ws_lfa1.
            READ TABLE it_lfa1 INTO ws_lfa1 WITH KEY lifnr = ws_zlest0194-motorista.
            IF sy-subrc EQ 0.
              ws_saida_sub-nome_motorista    = ws_lfa1-name1.
            ENDIF.

            APPEND ws_saida_sub  TO it_saida_aux.
            CLEAR: ws_saida_sub, ws_lfa1, ws_kna1, ws_vbrk, ws_bkpf, ws_j_1bnfdoc, ws_zlest0002, z_kurs2_aux, z_kurs2.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  LOOP AT it_saida INTO DATA(lw_saida_final).
    lw_zlest0227-mandt = sy-mandt.
    lw_zlest0227-data_modificacao = sy-datum.
    MOVE-CORRESPONDING lw_saida_final TO lw_zlest0227.
    APPEND lw_zlest0227 TO t_saida.
    CLEAR lw_zlest0227.
  ENDLOOP.

  LOOP AT it_saida_aux INTO lw_saida_final.
    lw_zlest0227-mandt = sy-mandt.
    lw_zlest0227-data_modificacao = sy-datum.
    MOVE-CORRESPONDING lw_saida_final TO lw_zlest0227.
    APPEND lw_zlest0227 TO t_saida.
    CLEAR lw_zlest0227.
  ENDLOOP.

  SORT t_saida BY tknum
  fknum
  tplst
  bezei
  erdat
  matnr1
  maktx
  placa_carreta
  carreta1
  carreta2
  carreta3
  shtyp
  tdlnr
  lifnr
  desc_origem
  munic_origem
  uf_origem
  kunnr
  desc_destino
  munic_destino
  uf_destino
  tipo
  mwsbp
  ntgew
  preco_tonelada_rs
  netwr
  taxa_dolar
  dmbe2
  belnr2
  docnum
  nfenum
  vttp_vbeln
  ov_pedido
  tp_ov
  transgenese
  dt_descarga
  peso_descarga
  cod_tran_efetivo
  descr_tran_efetivo
  grupo_veiculo
  frota
  grupo_produto
  cod_motorista
  nome_motorista
  cpf_motorista
  docnum_mdfe
  nr_mdfe
  ordem_car
  dt_oc
  data_modificacao.

  DELETE ADJACENT DUPLICATES FROM t_saida COMPARING ALL FIELDS.

  IF it_saida_carregamento[] IS NOT INITIAL.
    it_saida_carrega = it_saida_carregamento.
  ENDIF.


  IF t_saida[] IS NOT INITIAL.

    CLEAR: lit_likp[], lit_likp[].

    SELECT DISTINCT *
      FROM vttp INTO TABLE lit_vttp
      FOR ALL ENTRIES IN t_saida
     WHERE tknum = t_saida-tknum.

    SORT lit_vttp BY tknum ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lit_vttp COMPARING ALL FIELDS.

    IF lit_vttp[] IS NOT INITIAL.
      SELECT DISTINCT *
        FROM likp INTO TABLE lit_likp
        FOR ALL ENTRIES IN lit_vttp
       WHERE vbeln = lit_vttp-vbeln.

      SORT lit_likp BY vbeln ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lit_likp COMPARING ALL FIELDS.

    ENDIF.

    SORT lit_vttp BY tknum.
    SORT lit_likp BY vbeln.

    LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WHERE tknum IS NOT INITIAL.
      READ TABLE lit_vttp INTO DATA(lwa_vttp) WITH KEY tknum = <fs_saida>-tknum BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_likp INTO DATA(lwa_likp) WITH KEY vbeln = lwa_vttp-vbeln BINARY SEARCH.
      CHECK sy-subrc EQ 0 AND lwa_likp-fkdat IS NOT INITIAL.
      SELECT SINGLE *
        FROM zsdt0001
        INTO @DATA(w_romaneio)
        WHERE ch_referencia = @lwa_likp-xblnr.

      IF sy-subrc = 0 AND w_romaneio-fat_contingencia_ecc = 'X'.
        <fs_saida>-erdat = lwa_likp-fkdat.
      ENDIF.

    ENDLOOP.
  ENDIF.

*-IR190741-29.10.2024-#156620-JT-inicio
*--------------------------------------------------
* Procurar MDF-e avulsa
*--------------------------------------------------
  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<f_dados>) WHERE docnum_mdfe IS INITIAL.

    CLEAR: w_j_1bnfdoc, w_zlest0110, w_zsdt0241, w_zfiwrt0008.

    CHECK <f_dados>-tknum IS NOT INITIAL.  "*-IR212606-24.12.2024-#161504-JT

    SELECT SINGLE vbeln
      INTO @DATA(_vttp_vbeln)
      FROM vttp
    WHERE tknum EQ @<f_dados>-tknum.

    CHECK sy-subrc = 0.

    SELECT SINGLE vbeln
      INTO @DATA(_lips_vbeln)
      FROM lips
     WHERE vbeln = @_vttp_vbeln.

    CHECK sy-subrc = 0.

    SELECT SINGLE vbeln, lifex, xblnr, lifnr "*-IR212606-24.12.2024-#161504-JT-inicio
      FROM likp
      INTO CORRESPONDING FIELDS OF @w_likp_ux
     WHERE vbeln  = @_lips_vbeln.
*      AND tcode <> 'ZLES0136'.  "*-IR212606-24.12.2024-#161504-JT

    CHECK sy-subrc = 0.

    IF w_likp_ux-lifex IS NOT INITIAL.
      SPLIT w_likp_ux-lifex AT '-' INTO: w_likp_ux-nr_nf w_likp_ux-serie_nf2. "*-IR190741-29.10.2024-#156620-JT-inicio
    ELSE.
      SPLIT w_likp_ux-xblnr AT '-' INTO: w_likp_ux-nr_nf w_likp_ux-serie_nf2. "*-IR190741-29.10.2024-#156620-JT-inicio
    ENDIF.

    w_likp_ux-nr_nf = |{ w_likp_ux-nr_nf ALPHA = IN }|.

    SELECT SINGLE *
      FROM j_1bnfdoc
      INTO w_j_1bnfdoc
     WHERE parid  = w_likp_ux-lifnr
       AND nfenum = w_likp_ux-nr_nf
       AND series = w_likp_ux-serie_nf2.

    IF sy-subrc <> 0.
      SELECT SINGLE chave, cliente, numero, serie
        FROM zlest0110
        INTO CORRESPONDING FIELDS OF @w_zlest0110
       WHERE vbeln  = @w_likp_ux-vbeln
         AND numero = @w_likp_ux-nr_nf
         AND serie  = @w_likp_ux-serie_nf2.
      IF sy-subrc EQ 0.
        obj_zcl_util->get_docnum(
          EXPORTING
            i_chave_nfe  = w_zlest0110-chave               " Chave NF-e
          RECEIVING
            t_documentos = DATA(it_documentos)
        ).
        READ TABLE it_documentos INTO DATA(wa_documentos) INDEX 1.
*        select single docnum
*          from j_1bnfdoc
*          into corresponding fields of w_j_1bnfdoc
*         where parid  = w_zlest0110-cliente
*           and nfenum = w_zlest0110-numero
*           and series = w_zlest0110-serie.
      ENDIF.
    ENDIF.

    IF wa_documentos IS NOT INITIAL.

      SELECT SINGLE a~nmdfe, a~docnum, a~data_emi, a~hora_emi
        FROM zsdt0102 AS a
        INNER JOIN zsdt0105 AS b ON b~docnum_ref EQ a~docnum
        INTO  CORRESPONDING FIELDS OF @w_zsdt0102
       WHERE b~docnum = @wa_documentos-docnum
        AND  a~autorizado = @abap_true
         AND a~estornado = @abap_false
         AND a~cancel    = @abap_false.

      IF sy-subrc = 0.
*        select single *
*          from zsdt0102
*          into w_zsdt0102
*         where docnum     = w_zsdt0105-docnum_ref
*           and autorizado = abap_true
*           and estornado  = abap_false
*           and cancel     = abap_false.

        IF sy-subrc = 0.
          <f_dados>-nr_mdfe     = w_zsdt0102-nmdfe.
          <f_dados>-docnum_mdfe = w_zsdt0102-docnum.
          <f_dados>-data_mdfe   = w_zsdt0102-data_emi.
          <f_dados>-hora_mdfe   = w_zsdt0102-hora_emi.
        ENDIF.
      ELSE.
        IF w_zlest0110 IS NOT INITIAL.
          SELECT SINGLE a~docnum
          INTO CORRESPONDING FIELDS OF w_zsdt0241
          FROM zsdt0241 AS a
      INNER JOIN j_1bnfdoc AS b ON b~docnum EQ a~docnum
      AND b~cancel NE abap_true
      WHERE a~chave  = w_zlest0110-chave.
        ELSE.
*-IR212606-24.12.2024-#161504-JT-inicio
          SELECT SINGLE docnum
            INTO CORRESPONDING FIELDS OF w_zfiwrt0008
            FROM zfiwrt0008
           WHERE ch_referencia = w_likp_ux-xblnr
             AND loekz <> abap_true
             AND docs_estornados EQ space.

          IF sy-subrc <> 0 AND w_likp_ux-lifex IS NOT INITIAL.
            SELECT SINGLE docnum
          INTO CORRESPONDING FIELDS OF w_zfiwrt0008
          FROM zfiwrt0008
         WHERE ch_referencia = w_likp_ux-lifex
           AND loekz <> abap_true
           AND docs_estornados EQ space.
          ENDIF.
*-IR212606-24.12.2024-#161504-JT-fim
        ENDIF.

*>>>>>>>>>>>Ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
        IF w_zfiwrt0008 IS NOT INITIAL.
          SELECT SINGLE a~nmdfe, a~docnum, a~data_emi, a~hora_emi
          FROM zsdt0102 AS a
          INNER JOIN zsdt0105 AS b ON b~docnum_ref EQ a~docnum
          INTO  CORRESPONDING FIELDS OF @w_zsdt0102
          WHERE b~docnum = @w_zfiwrt0008-docnum
          AND  a~autorizado = @abap_true
           AND a~estornado = @abap_false
           AND a~cancel    = @abap_false.

          IF sy-subrc = 0.
*            select single *
*              from zsdt0102
*              into w_zsdt0102
*             where docnum     = w_zsdt0105-docnum_ref
*               and autorizado = abap_true
*               and estornado  = abap_false
*               and cancel     = abap_false.

*            if sy-subrc = 0.
            <f_dados>-nr_mdfe     = w_zsdt0102-nmdfe.
            <f_dados>-docnum_mdfe = w_zsdt0102-docnum.
            <f_dados>-data_mdfe   = w_zsdt0102-data_emi.
            <f_dados>-hora_mdfe   = w_zsdt0102-hora_emi.
*            endif.
          ENDIF.
*-IR212606-24.12.2024-#161504-JT-fim

        ELSEIF w_zsdt0241 IS NOT INITIAL.
          SELECT SINGLE *
            FROM j_1bnfdoc
            INTO w_j_1bnfdoc
           WHERE docnum = w_zsdt0241-docnum
             AND cancel = abap_false.

          IF sy-subrc = 0.
            <f_dados>-nr_mdfe     = w_j_1bnfdoc-nfenum.
            <f_dados>-docnum_mdfe = w_zsdt0241-docnum.
            <f_dados>-data_mdfe   = w_j_1bnfdoc-credat.
            <f_dados>-hora_mdfe   = w_j_1bnfdoc-cretim.

            IF w_zsdt0241-chave IS INITIAL.
              SELECT SINGLE * FROM j_1bnfe_active INTO ls_1bnfe_active
                WHERE docnum EQ w_zsdt0241-docnum.
              IF sy-subrc EQ 0.
                <f_dados>-chave_nfe = ls_1bnfe_active-regio &&
                                   ls_1bnfe_active-nfyear &&
                                   ls_1bnfe_active-nfmonth &&
                                   ls_1bnfe_active-stcd1 &&
                                   ls_1bnfe_active-model &&
                                   ls_1bnfe_active-serie &&
                                   ls_1bnfe_active-nfnum9 &&
                                   ls_1bnfe_active-docnum9 &&
                                   ls_1bnfe_active-cdv.
              ENDIF.
            ELSE.
              <f_dados>-chave_nfe = w_zsdt0241-chave.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*>>>>>>>>>>>>>Ajuste seleção MDF-e BUG #163799 / AOENNING
    ELSE.

      SELECT SINGLE docnum
    INTO CORRESPONDING FIELDS OF w_zfiwrt0008
    FROM zfiwrt0008
   WHERE ch_referencia = w_likp_ux-xblnr
      AND loekz <> abap_true
      AND docs_estornados EQ space.

      IF sy-subrc <> 0 AND w_likp_ux-lifex IS NOT INITIAL.
        SELECT SINGLE docnum
        INTO CORRESPONDING FIELDS OF w_zfiwrt0008
        FROM zfiwrt0008
        WHERE ch_referencia = w_likp_ux-lifex
        AND loekz <> abap_true.
      ENDIF.
    ENDIF.
*>>>>>>>>>>>Inicio ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
    IF w_zfiwrt0008 IS NOT INITIAL.
      SELECT SINGLE a~nmdfe, a~docnum, a~data_emi, a~hora_emi
        FROM zsdt0102 AS a
        INNER JOIN zsdt0105 AS b ON b~docnum_ref EQ a~docnum
        INTO  CORRESPONDING FIELDS OF @w_zsdt0102
       WHERE b~docnum = @w_zfiwrt0008-docnum
        AND  a~autorizado = @abap_true
         AND a~estornado = @abap_false
         AND a~cancel    = @abap_false.

      IF sy-subrc = 0.
*        select single *
*          from zsdt0102
*          into w_zsdt0102
*         where docnum     = w_zsdt0105-docnum_ref
*           and autorizado = abap_true
*           and estornado  = abap_false
*           and cancel     = abap_false.

*        if sy-subrc = 0.
        <f_dados>-nr_mdfe     = w_zsdt0102-nmdfe.
        <f_dados>-docnum_mdfe = w_zsdt0102-docnum.
        <f_dados>-data_mdfe   = w_zsdt0102-data_emi.
        <f_dados>-hora_mdfe   = w_zsdt0102-hora_emi.
*        endif.
      ENDIF.
    ENDIF.
*>>>>>>>>>>>Inicio ajuste seleção MDF-e #IR212606 / BUG SOLTO 163799/ AOENNING <<<<<<<<<<<<<<<<<<<<<<<<<<
  ENDLOOP.


ENDFUNCTION.

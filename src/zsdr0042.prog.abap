
*&---------------------------------------------------------------------*
*& Report  ZSDR0042
*&
*&---------------------------------------------------------------------*
*&TITULO  : Cockipt do Simulador de Vendas
*&AUTOR   : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA.   : 11.07.2014
*TRANSACAO: ZSDT0087
*&---------------------------------------------------------------------*

report  zsdr0042.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: icon,
            slis.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
constants: on  type raw4 value cl_gui_alv_grid=>mc_style_enabled,
           off type raw4 value cl_gui_alv_grid=>mc_style_disabled.

constants gc_0_faturada type numc2 value '01'.
constants gc_parc_faturada type numc2 value '02'.
constants gc_100_faturada type numc2 value '03'.

constants gc_0_liquidada type numc2 value '01'.
constants gc_parc_liquidada type numc2 value '02'.
constants gc_100_liquidada type numc2 value '03'.

data(categoria) =
value zrsdsselopts(
                    ( sign = 'I' option = 'EQ' low = 'A' ) " Alterar Qtd
                    ( sign = 'I' option = 'EQ' low = 'C' ) " Travar Câmbio
                    ( sign = 'I' option = 'EQ' low = 'O' ) " Des. Absoluto
                    ( sign = 'I' option = 'EQ' low = 'V' ) " Vencimento
                    ( sign = 'I' option = 'EQ' low = 'P' ) " Parceiro
                    ( sign = 'I' option = 'EQ' low = 'G' ) " Pesagem
                    ( sign = 'I' option = 'EQ' low = 'I' ) " Itinerário
                    ( sign = 'I' option = 'EQ' low = 'F' ) " Frete
                    ( sign = 'I' option = 'EQ' low = 'L' ) " depósito/Lote
                    ( sign = 'I' option = 'EQ' low = 'K' ) " Retorno da Devolução
*-BUG 54642 - 24.03.2021 - JT - inicio
                    ( sign = 'I' option = 'EQ' low = 'T' ) " Troca Cond.Pagamento
*-BUG 54642 - 24.03.2021 - JT - fim
                    ( sign = 'I' option = 'EQ' low = 'B' ) " Alterar Dt Entrega
                   ).
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: zsdt0040, zsdt0041.

data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

types: begin of ty_saida_exec,
         inco1    type zsdt0041-werks,
         spart    type zsdt0041-spart,
         auart    type zsdt0041-auart,
         werks    type zsdt0041-werks,
         vbeln    type vbak-vbeln,
         msg(255),
       end of ty_saida_exec,

       "Documento de vendas: dados de cabeçalho
       begin of ty_vbak,
         vbeln    type vbak-vbeln,
         auart    type vbak-auart,
         spart    type vbak-spart,
         kunnr    type vbak-kunnr,
         knumv    type vbak-knumv,
         zpesagem type vbak-zpesagem,
         vkgrp    type vbak-vkgrp,
       end of ty_vbak,

       "Documento de vendas: dados de item
       begin of ty_vbap,
         vbeln  type vbap-vbeln,
         posnr  type vbap-posnr,
         matnr  type vbap-matnr,
         arktx  type vbap-arktx,
         werks  type vbap-werks,
         charg  type vbap-charg,
         kwmeng type vbap-kwmeng,
         vrkme  type vbap-vrkme,
         netpr  type vbap-netpr,
         netwr  type vbap-netwr,
         lgort  type vbap-lgort,
         umziz  type vbap-umziz,
         route  type vbap-route,
         mwsbp  type vbap-mwsbp,
       end of ty_vbap,

       begin of ty_vbpa,
         vbeln type vbeln,
         parvw type parvw,
         kunnr type kunnr,
         lifnr type lifnr,
       end of ty_vbpa,

       begin of ty_konv,
         kschl type konv-kschl,
         kmein type konv-kmein,
         kbetr type konv-kbetr,
         knumv type konv-knumv,
         kposn type konv-kposn,
       end of ty_konv,

       "Documento de vendas: dados comerciais
       begin of ty_vbkd,
         vbeln type vbkd-vbeln,
         posnr type vbkd-posnr,
         inco1 type vbkd-inco1,
       end of ty_vbkd,

       "Fluxo de documentos de vendas e distribuição
       begin of ty_vbfa,
         vbelv type vbfa-vbelv,
         posnv type vbfa-posnv,
         rfmng type vbfa-rfmng,
         vbeln type vbfa-vbeln,
         posnn type vbfa-posnn,
         meins type vbfa-meins,
       end of ty_vbfa,

       begin of ty_kna1,
         kunnr type kna1-kunnr,
         name1 type kna1-name1,
       end of ty_kna1,

       begin of ty_lfa1,
         lifnr type lfa1-lifnr,
         name1 type lfa1-name1,
       end of ty_lfa1,

       begin of ty_mara,
         matnr type mara-matnr,
         matkl type mara-matkl,
         meins type mara-meins,
       end of ty_mara,

       begin of ty_venda,
         kunnr     type kna1-kunnr,
         name1     type kna1-name1,
         txt_ordem type zsdt0089-txt_ordem,
         txt_item  type zsdt0089-txt_item,
         kursf     type zsdt0040-kursf,
         inco1     type vbkd-inco1,
         r_futura  type c length 1,
         d_futura  type c length 20,
         r_ordem   type c length 1,
         d_ordem   type c length 20,
         c_ordem   type c length 1,
         r_triang  type c length 1,
         d_triang  type c length 20,
         ztri      type c length 1,
         zrad      type c length 1,
       end of ty_venda,

       begin of ty_desmem,
         kunnr   type kna1-kunnr,
         name1   type kna1-name1,
         zdes(1),
       end of ty_desmem,

       begin of ty_desmem_massa_header,
         matnr type makt-matnr,
         maktx type makt-maktx,
         saldo type vbap-zmeng,
       end of ty_desmem_massa_header,

       begin of ty_desmem_massa_item,
         vbeln type vbap-vbeln,
         posnr type vbap-posnr,
         kunnr type vbak-kunnr,
         name1 type kna1-name1,
         zmeng type vbap-zmeng,
         zieme type vbap-zieme,
         qtd   type vbap-zmeng,
       end of ty_desmem_massa_item,

       begin of ty_material,
         matnr        type mara-matnr,
         maktx        type makt-maktx,
         meins        type zsdt0036-meins,
         meins_mara   type zsdt0036-meins,
         werks_fornec type zsdt0036-werks_fornec,
         inco1        type zsdt0036-inco1,
         waerk        type zsdt0036-waerk,
         matkl        type mara-matkl,
         id           type sy-tabix,
       end of ty_material,

       begin of ty_mat,

         matnr        type mara-matnr,
         maktx        type makt-maktx,
         meins        type zsdt0036-meins,
         meins_mara   type zsdt0036-meins,
         werks_fornec type zsdt0036-werks_fornec,
         inco1        type zsdt0036-inco1,
         waerk        type zsdt0036-waerk,
         matkl        type mara-matkl,
         vlr_venda    type zsdt0036-vlr_venda,
         spart        type mara-spart,
         mtart        type mara-mtart,
         id           type sy-tabix,
       end of ty_mat,

       begin of ty_tela_material,
         matnr        type mara-matnr,
         maktx        type makt-maktx,
         meins        type zsdt0036-meins,
         meins_mara   type zsdt0036-meins,
         werks_fornec type zsdt0036-werks_fornec,
         inco1        type zsdt0036-inco1,
         waerk        type zsdt0036-waerk,
         id           type sy-tabix,
         lote         type vbap-charg,
         qtd          type vbap-kwmeng,
         matkl        type mara-matkl,
         lgort        type lgort_d,
       end of ty_tela_material,

       begin of ty_troca_material_massa_header,
         matnr     type zsdt0036-matnr,
         maktx     type makt-maktx,
         waerk     type zsdt0036-waerk,
         inco1     type zsdt0036-inco1,
         safra     type zsdt0036-safra,
         cultura   type zsdt0036-cultura,
         werks     type zsdt0036-werks_fornec,
         meins     type zsdt0036-meins,
         vlr_venda type zsdt0036-vlr_venda,
         lote      type charg_d,
         lgort     type lgort_d,
       end of ty_troca_material_massa_header,

       begin of ty_troca_material_massa_item,
         kunnr        type kunnr,
         vbeln        type vbeln_va,
         posnr        type posnr_va,
         matnr        type matnr,
         arktx        type arktx,
         kwmeng       type kwmeng,
         vrkme        type vrkme,
         netwr        type netwr,
         sd_disp      type rfmng,
         qtd_removida type rfmng,

         lote         type charg_d,
         lgort        type lgort_d,
         vlr_venda    type kbetr,
         vlr_total    type netwr,
         qtd_recebida type rfmng,

         itinerario   type c length 4,
         color        type c length 4,
         cellcolor    type lvc_t_scol,
         style        type lvc_t_styl,
       end of ty_troca_material_massa_item.

types begin of ty_price.
include structure zsdt0090.
types posnr type vbap-posnr.
types end of ty_price.

types: begin of ty_calc,
         vbeln    type vbeln,
         posnr    type posnr,
         matnr    type matnr,
         netpr_1  type netpr,
         kwmeng_1 type kwmeng,
         netwr_1  type netwr_ap,
         mwsbp_1  type mwsbp,
         calc_1   type p decimals 5,
         netpr_2  type netpr,
         kwmeng_2 type kwmeng,
         netwr_2  type netwr_ap,
         mwsbp_2  type mwsbp,
         calc_2   type p decimals 5,
         difer_0  type p decimals 5,
       end of ty_calc.

types: begin of ty_saldo,
         doc_simulacao type zsdt0041-doc_simulacao,
         vbeln         type vbfa-vbeln,
         posnr         type zsdt0041-posnr,
         werks         type zsdt0041-werks,
         zmeng         type zsdt0041-zmeng,
         total         type zsdt0041-zmeng,
         saldo         type zsdt0041-zmeng,
       end of ty_saldo,

       begin of ty_saida,
         check(1),
         icon(4),
         doc_simulacao  type zsdt0040-doc_simulacao, "Nº Simulação
         doc_simulacao2 type zsdt0040-doc_simulacao, "Nº Simulação
         vkorg          type zsdt0040-vkorg,         "Organização de Vendas
         vkbur          type zsdt0040-vkbur,         "Escritorio de Vendas
         vtweg          type zsdt0040-vtweg,         "Canal de Distribuição
*         VENDEDOR       TYPE ZSDT0040-VENDEDOR,     "Vendedor
         vendedor       type vbak-vkgrp,             "Vendedor
         waerk          type zsdt0040-waerk,         "Moeda
         cultura        type zsdt0040-cultura,       "Cultura
         safra          type zsdt0040-safra,         "Safra
         tpsim          type zsdt0040-tpsim,         "Condição Pagamento
         kunnr          type zsdt0040-kunnr,         "Emissor Ordem
         kunnr2         type zsdt0040-kunnr,         "Emissor Ordem
         name1          type kna1-name1,             "Descrição Emissor
         vbeln          type zsdt0041-vbeln,         "Nº OV
         vbeln2         type zsdt0041-vbeln,         "Nº OV
         itens          type zsdt0041-posnr,
         auart          type vbak-auart,             "Tipo de OV
         spart          type vbak-spart,             "Setor de Atividades
         posnr          type vbap-posnr,             "Item
         matnr          type vbap-matnr,             "Material
         matnr2         type vbap-matnr,             "Material
         matkl          type mara-matkl,             "Grupo de Mercadoria
         arktx          type vbap-arktx,             "Descrição Material
         werks          type vbap-werks,             "Centro
*         LGORT          TYPE VBAP-LGORT,             "Deposito
         charg          type vbap-charg,             "Lote
         inco1          type vbkd-inco1,             "Incoterms
         kwmeng         type vbap-kwmeng,            "Quatidade Prevista
         sd_disp        type vbfa-rfmng,             " Saldo disponível
         vrkme          type vbap-vrkme,             "UM
         kbetr          type konv-kbetr,             "Preço
         kmein          type konv-kmein,             "UM
         netwr          type vbap-netwr,             "Valor Total
         lgort          type vbap-lgort,             "Armazem
         status         type zsdt0041-status,        "Status do Item
         qt_tran        type vbfa-rfmng,
         seq            type zsdt0090-sequencia,
         " 31.07.2023 - RAMON - 98680 -->
         categoria      type zsdt0090-categoria,
         " 31.07.2023 - RAMON - 98680 --<
         desc_absoluto  type zsdt0041-desc_absoluto,
         zpesagem       type zpesagem,
         parvw          type parvw,
         cod_parc       type char10,
         route          type route,
         "19.06.2023 - RAMON -- 97513 -->
         meio_pago      type zsded049,
         "19.06.2023 - RAMON -- 97513 --<
         color          type c length 4,
         style          type lvc_t_styl,
       end of ty_saida,

       tt_saida  type ty_saida,
       tab_saida type table of ty_saida,

       begin of ty_desc_enc,
         vbeln2        type zsdt0041-vbeln,         "Nº OV
         posnr         type vbap-posnr,             "Item
         desc_absoluto type zsdt0041-desc_absoluto,
         desc_aux      type zsdt0041-desc_absoluto,
         perc_desc     type p decimals 10,
       end of ty_desc_enc,

       begin of ty_redist,
         kunnr   type vbak-kunnr,
         kunnr2  type vbak-kunnr,
         name1   type kna1-name1,
         vbeln   type vbak-vbeln,
         vbeln2  type vbak-vbeln,
         posnr   type vbap-posnr,
         matnr   type vbap-matnr,
         matnr2  type vbap-matnr,
         arktx   type vbap-arktx,
         werks   type vbap-werks,
         charg   type vbap-charg,
         kbetr   type konv-kbetr,
         kwmeng  type vbap-kwmeng,
         vrkme   type vbap-vrkme,
         sd_disp type vbfa-rfmng,
         qt_tran type vbfa-rfmng,
         rfmng   type vbfa-rfmng,
       end of ty_redist,

       begin of ty_alt_gerais,
         vbeln      type vbeln,
         auart      type auart,
         zpesagem   type zpesagem,
         route      type route,
         desc_route type char50,
         inco1      type inco1,
         parvw      type parvw,
         cod_parc   type char10,
         desc_parc  type char50,
         werks      type werks_d,
         desc_werks type name1,
       end of ty_alt_gerais,

       begin of ty_preco,
         vlr_venda      type kbetr,
         uni_venda      type dzieme,
         vlr_cadastrado type kbetr,
         uni_cadastrado type dzieme,
         vlr_total      type netwr,
       end of ty_preco,

       begin of taxa,
         taxa type vbkd-kurrf,
         venc type vbkd-valdt,
       end of taxa,

       begin of ty_dt_ent,
         doc_simulacao type zsdt0040-doc_simulacao, "Nº Simulação
         vbeln         type zsdt0041-vbeln,         "Nº OV
         vbelv         type vbfa-vbelv,
         spart         type zsdt0040-spart,
         matnr         type zsdt0041-matnr,
         cultura       type zsdt0040-cultura,
         sd_disp       type vbfa-rfmng,
         kbetr         type konv-kbetr, "Preço
         dt_entrega_v  type zsdt0090-dt_entrega_v,
         dt_entrega    type zsdt0090-dt_entrega,
       end of ty_dt_ent,

       begin of ty_dt_ent_aux,
         doc_simulacao type zsdt0040-doc_simulacao, "Nº Simulação
         vbeln         type zsdt0041-vbeln,         "Nº OV
         vbelv         type vbfa-vbelv,
         werks         type vbap-werks,
         kmein         type konv-kmein,
         vrkme         type vbap-vrkme,
         posnr         type zsdt0041-posnr,
         spart         type zsdt0040-spart,
         matnr         type zsdt0041-matnr,
         matklv        type vbap-matkl,
         cultura       type zsdt0040-cultura,
         sd_disp       type vbfa-rfmng,
         kbetr         type konv-kbetr, "Preço
         dt_entrega_v  type zsdt0090-dt_entrega_v,
         dt_entrega    type zsdt0090-dt_entrega,
       end of ty_dt_ent_aux.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

data: it_zsdt0040                    type table of zsdt0040,
      it_zsdt0041                    type table of zsdt0041,
      it_zsdt0090                    type table of zsdt0090,
      tl_vbep                        type table of vbep with header line,
      it_vbep                        type table of vbep with header line,
      it_vbap                        type table of ty_vbap,
      gt_konv                        type table of ty_konv,
      it_vbap2                       type table of ty_vbap,
      it_vbpa                        type table of ty_vbpa with header line,
      it_vbkd                        type table of ty_vbkd,
      it_vbak                        type table of ty_vbak,
      it_vbfa                        type table of ty_vbfa,
      it_kna1                        type table of ty_kna1,
      it_lfa1                        type table of ty_lfa1,
      it_mara                        type table of ty_mara,
      it_marm                        type table of marm with header line,
      it_ov                          type table of zsds015,
      t_ov                           type table of zsds015,
      tg_material                    type table of ty_material   with header line,
      tg_mat                         type table of ty_mat,
      tg_mat1                        type table of ty_mat,
      tl_saida_exec                  type table of ty_saida_exec with header line,
      t_usermd                       type standard table of  rgsb4 with header line,
      t_set                          type standard table of setleaf  with header line,
      t_lay                          type standard table of setlinet with header line,
      t_mv45afzz_werks               type standard table of setleaf with header line,
      it_saida                       type standard table of ty_saida,
      it_venc                        type standard table of ty_saida,
      it_dt_ent                      type standard table of ty_dt_ent,
      it_dt_ent_f                    type standard table of ty_dt_ent,
      it_dt_ent_s                    type standard table of ty_dt_ent,
      it_dt_ent_d                    type standard table of ty_dt_ent,
      it_desc_enc                    type standard table of ty_desc_enc with header line,
      it_sai_aux                     type table of ty_saida with header line,
*      it_popup     TYPE TABLE OF ty_redist WITH HEADER LINE,
*      it_popup2    TYPE TABLE OF ty_redist WITH HEADER LINE,
      it_popup                       type table of ty_saida with header line,
      it_popup2                      type table of ty_saida with header line,
      it_troca                       type table of ty_saida,
      t_troca_aux                    type table of ty_saida,
      it_saldo                       type table of ty_saida,
      it_dep_lote1                   type table of ty_saida,
      it_dep_lote2                   type table of ty_saida,
      it_popup2_aux                  type table of ty_saida with header line,
      tg_saldo                       type table of ty_saldo,
      ls_desmem_massa_header         type ty_desmem_massa_header,
      it_desmem_massa_item           type table of ty_desmem_massa_item,
      gs_desmembramento              type ty_saida,
      t_vbap                         type table of vbap with header line,
      tl_0041                        type table of zsdt0041 with header line,
      t_vbfa                         type table of vbfa with header line,
      tl_vbfa_aux                    type table of vbfa with header line,
      wg_saldo                       type ty_saldo,
      var_venc_old                   type zsdt0040-dt_entrega_fet,
      var_venc_new                   type zsdt0040-dt_entrega_fet,
      var_ent_old                    type zsdt0090-dt_entrega,
      var_fert(1)                    type c,
      var_sem(1)                     type c,
      var_defen(1)                   type c,
      it_alt_gerais                  type table of ty_alt_gerais,
      wa_alt_gerais                  type ty_alt_gerais,
      tg_0090                        type table of zsdt0090,
      gs_troca_material_massa_header type ty_troca_material_massa_header,
      gt_troca_material_massa_header type table of ty_troca_material_massa_header,
      gt_troca_material_massa_item   type table of ty_troca_material_massa_item,
      gs_troca_massa                 type ty_saida,
*      TG_0090       TYPE ZSDT0090,

*      WA_0116       TYPE ZSDT0116,
*      WA_ZSDT0116   TYPE ZSDT0116,

      begin of tg_vbfa occurs 0,
        vbeln type vbfa-vbeln,
        rfmng type vbfa-rfmng,
      end of tg_vbfa,

      begin of tg_ov occurs 0,
        vbeln   type vbak-vbeln,
        vbeln2  type vbak-vbeln,
        posnr   type vbap-posnr,
        matnr   type vbap-matnr,
        matnr2  type vbap-matnr,
        arktx   type vbap-arktx,
        kwmeng  type vbap-kwmeng,
        vrkme   type vbap-vrkme,
        sd_disp type vbfa-rfmng,
        qt_tran type vbfa-rfmng,
        charg   type vbap-charg,
        lgort   type vbap-lgort,
      end of tg_ov,

      begin of tg_ov2 occurs 0,
        vbeln   type vbak-vbeln,
        vbeln2  type vbak-vbeln,
        posnr   type vbap-posnr,
        matnr   type vbap-matnr,
        matnr2  type vbap-matnr,
        arktx   type vbap-arktx,
        kwmeng  type vbap-kwmeng,
        vrkme   type vbap-vrkme,
        sd_disp type vbfa-rfmng,
        qt_tran type vbfa-rfmng,
        rfmng   type vbfa-rfmng,
        spart   type vbak-spart,
      end of tg_ov2,

      begin of tg_ov_mat occurs 0,
        vbeln   type vbak-vbeln,
        vbeln2  type vbak-vbeln,
        posnr   type vbap-posnr,
        matnr   type vbap-matnr,
        charg   type vbap-charg,
        matnr2  type vbap-matnr,
        arktx   type vbap-arktx,
        kwmeng  type vbap-kwmeng,
        vrkme   type vbap-vrkme,
        werks   type vbap-werks,
        meins   type vbap-meins,
        sd_disp type vbfa-rfmng,
        qt_tran type vbfa-rfmng,
        rfmng   type vbfa-rfmng,
        lgort   type vbap-lgort,
        netpr   type vbap-netpr,
        netwr   type vbap-netwr,
      end of tg_ov_mat,

      begin of it_troca_old occurs 0,
        id      type i,
        vbeln   type vbak-vbeln,
        posnr   type vbap-posnr,
        matnr   type vbap-matnr,
        matkl   type mara-matkl,
        charg   type vbap-charg,
        arktx   type vbap-arktx,
        kwmeng  type vbap-kwmeng,
        vrkme   type vbap-vrkme,
        werks   type vbap-werks,
        meins   type vbap-meins,
        sd_disp type vbfa-rfmng,
        qt_tran type vbfa-rfmng,
        rfmng   type vbfa-rfmng,
        lgort   type vbap-lgort,
        netpr   type vbap-netpr,
        netwr   type vbap-netwr,
        vkorg   type zsdt0040-vkorg,
        waerk   type zsdt0040-waerk,
        cultura type zsdt0040-cultura,
        inco1   type vbkd-inco1,
        auart   type auart,
        check   type char1, "// Marca o Vbeln a ser Disparado na 90 no momenta da criação da Nova OV
        spart   type mara-spart, " SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980
        kunnr   type kunnr,
        tpsim   type c length 2,
      end of it_troca_old,

      begin of it_troca_new occurs 0,
        id             type i,
        matnr          type matnr,
        matkl          type matkl,
        maktx          type maktx,
        inco1          type inco1,
        werks_fornec   type werks_d,
        lote           type charg_d,
        lgort          type lgort_d,
        qtd            type kwmeng,
        meins          type meins,
        vlr_cadastrado type kbetr,
        uni_cadastrado type dzieme,
        vlr_venda      type kbetr,
        uni_venda      type dzieme,
        vlr_total      type netwr,
        spart          type spart,
        mtart          type mtart,
        itinerario     type c length 4,
        style          type lvc_t_styl,
      end of it_troca_new.
*
*      IT_TROCA_NEW TYPE TY_TROCA_NEW.

types: begin of ty_itens_id,
         index   type i,
         doc     type zsdt0041-doc_simulacao,
         vbeln   type zsdt0041-vbeln,
         posnr   type zsdt0041-posnr,
         vlr_ini type zsded005,
         vlr_atu type zsded005,
         vlr_dif type zsded005,
       end of ty_itens_id,

*       BEGIN OF TY_TROCA_NEW,
*         ID             TYPE I,
*         MATNR          TYPE MATNR,
*         MATKL          TYPE MATKL,
*         MAKTX          TYPE MAKTX,
*         INCO1          TYPE INCO1,
*         WERKS_FORNEC   TYPE WERKS_D,
*         LOTE           TYPE CHARG_D,
*         LGORT          TYPE LGORT_D,
*         QTD            TYPE KWMENG,
*         MEINS          TYPE MEINS,
*         VLR_CADASTRADO TYPE KBETR,
*         UNI_CADASTRADO TYPE DZIEME,
*         VLR_VENDA      TYPE KBETR,
*         UNI_VENDA      TYPE DZIEME,
*         VLR_TOTAL      TYPE NETWR,
*         STYLE          TYPE LVC_T_STYL,
*       END OF TY_TROCA_NEW,

*       TY_T_TROCA_NEW TYPE TABLE OF TY_TROCA_NEW WITH EMPTY KEY,
*       TY_T_EMPLOYEE     TYPE TABLE OF TY_EMPLOYEE       WITH EMPTY KEY,

       begin of ty_est_zrfu,
         etapa type sy-tabix,
         desc  type char30,
         erro  type char1,
         vbeln type vbeln,
       end of ty_est_zrfu.

*DATA IT_TROCA_NEW TYPE TY_T_TROCA_NEW.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
data: wa_zsdt0040   type zsdt0040,
      w_zsdt0040    type zsdt0040,
      wa_zsdt0041   type zsdt0041,
      wa_zsdt0090   type zsdt0090,
      wx_0090       type zsdt0090,
      w_zsdt0090    type zsdt0090,
      wa_vbep       type vbep,
      wa_vbap       type ty_vbap,
      wa_vbap2      type ty_vbap,
      gw_konv       type ty_konv,
      wa_vbkd       type ty_vbkd,
      wa_vbak       type ty_vbak,
      wa_vbfa       type ty_vbfa,
      wa_fatura     type ty_vbfa,
      wa_vbfa2      type ty_vbfa,
      wa_kna1       type ty_kna1,
      wa_mara       type ty_mara,
      wa_ov         type zsds015,
      wg_material   type ty_tela_material,
      wa_saida      type ty_saida,
      dep_lote2     type ty_saida,
      dep_lote1     type ty_saida,
      wa_0090       type ty_price,
      im_0090       type zsdt0090,
      tm_0090       type table of zsdt0090,
      p_0090        type ty_price,
      p_ucomm       type sy-ucomm,
      h_ucomm       type sy-ucomm,
      wa_taxa_old   type taxa,
      wa_preco_     type ty_preco,
      wa_dt_ent     type ty_dt_ent,
      wa_dt_ent_aux type ty_dt_ent_aux,
      wa_dt_ent_f   type ty_dt_ent,
      wa_dt_ent_s   type ty_dt_ent,
      wa_dt_ent_d   type ty_dt_ent.

*DATA: IT_TROCA_NEW TYPE  TT_TROCA_NEW.

data: vl_vlr_venda type kbetr.
" 10.09.2024 - RAMON - 61181 -->
data gv_ov_desme_atual type vbeln.
data gv_ucomm type sy-ucomm.
" 10.09.2024 - RAMON - 61181 --<

data gv_stvarv_ativa." 10.09.2024 - RAMON - 97513

data: it_itens_id type table of ty_itens_id.
data: wa_itens_id type ty_itens_id,
      it_edit     type lvc_t_styl,
      wa_edit     type lvc_s_styl.

data: inco1_    type c,
      zpesagem_ type c,
      route_    type c,
      cod_parc_ type c,
      dep_lote_ type c,
      zfer      type c,
      centro_   type c.

data: v_erro type c.
data: oculta_inco1 type c.
data: v_mtart type mtart.

field-symbols <saida> type ty_saida.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*

data: gt_return_tab    type table of ddshretval with header line,
      gt_dselc         type table of dselc      with header line,

      editcontainer    type ref to cl_gui_custom_container,
      cl_container     type ref to cl_gui_custom_container,
      cl_container_95  type ref to cl_gui_custom_container,
      obj_dyndoc_id    type ref to cl_dd_document,
      cl_grid          type ref to cl_gui_alv_grid,

      grid1            type ref to cl_gui_alv_grid,
      grid2            type ref to cl_gui_alv_grid,
      grid_desmemb_mas type ref to cl_gui_alv_grid,
      grid_troca_mas   type ref to cl_gui_alv_grid,
      grid_r_c1        type ref to cl_gui_alv_grid,
      grid_r_c2        type ref to cl_gui_alv_grid,
      gr_dep_lote1     type ref to cl_gui_alv_grid,
      gr_dep_lote2     type ref to cl_gui_alv_grid,
      obj_grid_mat     type ref to cl_gui_alv_grid,
      obj_gtroca_old   type ref to cl_gui_alv_grid,
      obj_gtroca_new   type ref to cl_gui_alv_grid,
      gr_venc          type ref to cl_gui_alv_grid,
      gr_dt_ent        type ref to cl_gui_alv_grid,
      gr_alt_gerais    type ref to cl_gui_alv_grid,

      obg_conteiner    type ref to cl_gui_custom_container,
      obg_conteiner2   type ref to cl_gui_custom_container,
      obg_desmemb_mas  type ref to cl_gui_custom_container,
      obg_troca_mas    type ref to cl_gui_custom_container,
      obg_r_c01        type ref to cl_gui_custom_container,
      obg_r_c02        type ref to cl_gui_custom_container,
      cc_dep_lote1     type ref to cl_gui_custom_container,
      cc_dep_lote2     type ref to cl_gui_custom_container,
      obj_cont_mat     type ref to cl_gui_custom_container,
      obj_ctroca_old   type ref to cl_gui_custom_container,
      obj_ctroca_new   type ref to cl_gui_custom_container,
      cc_venc          type ref to cl_gui_custom_container,
      cc_dt_ent        type ref to cl_gui_custom_container,
      cc_alt_gerais    type ref to cl_gui_custom_container,

      wa_stable        type lvc_s_stbl,
      ls_stable	       type lvc_s_stbl value 'XX',
*      IS_STABLE       TYPE LVC_S_STBL VALUE 'XX',

      wa_afield        type lvc_s_fcat,
      it_fieldcat      type lvc_t_fcat,
      w_fieldcat       type lvc_s_fcat,

      i_sort           type lvc_t_sort,

      wa_layout        type lvc_s_layo,

      gs_variant_c     type disvariant,
      ok_code          type sy-ucomm,
      wa_estrutura     type ty_estrutura,
      estrutura        type table of ty_estrutura,
      check_dt         type sy-subrc,
      check_dt_ent     type sy-subrc,
      diferenca_qt     type netpr,
      it_est_zrfu      type table of ty_est_zrfu with header line,
      it_est           type table of ty_est_zrfu with header line.

**************  CAIXA DE TEXTO  **********************
data:
  t_lines   like tline occurs 0 with header line,
  wa_header like thead.

data:
  t_text  type catsxt_longtext_itab,
  wa_text like line of t_text.
**************  CAIXA DE TEXTO  **********************

*&--------------------------------------------------------------------&*
*& Variáveis                                                          &*
*&--------------------------------------------------------------------&*
data: begin of i_order_item_in occurs 0.
        include structure bapisditm.
data: end   of i_order_item_in.

data: begin of i_order_item_inx occurs 0.
        include structure bapisditmx.
data: end   of i_order_item_inx.

data: begin of i_sched occurs 10.
        include structure bapischdl.
data: end of i_sched.

data: begin of i_schedx occurs 10.
        include structure bapischdlx.
data: end of i_schedx.

data: v_botoes(1),
      wg_exit(1)        type c,
      wg_save(1)        type c,
      wg_linhas         type i,
      w_answer(1),
      vg_erro_pic       type c length 1,
      v_vbeln           type vbak-vbeln,
      v_auart           type vbak-auart,
      wg_venda          type ty_venda,
      wg_desmem         type ty_desmem,
      v_name1           type kna1-name1,

      wl_orderheaderin  type bapisdh1,
      wl_orderheaderinx type bapisdh1x,
      f_headinx         like bapisdh1x,
      wg_des_matnr      type c length 255,
      tl_return         type table of bapiret2   with header line,
      desconto          type p decimals 2,
      ztotal            type zsdt0040-vlrtot,
      v_doc_simulacao   type zsdt0040-doc_simulacao.

data: at_preco type netpr.
data: at_difen type netpr.

data: h1     type n length 3,
      altura type n length 3.

** Criação de tabela dinamica
data: t_fieldcatalog type lvc_t_fcat,
      w_fieldcatalog type lvc_s_fcat.

data: tg_selectedcell type lvc_t_cell,
      wg_selectedcell type lvc_s_cell.


data: gt_bdc type table of bdcdata,
      gw_bdc type bdcdata.

data: obj_zcl_util_sd type ref to zcl_util_sd.
create object obj_zcl_util_sd.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
constants:
         c_x               type c value 'X'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
class lcl_event_handler definition.
  public section.
    class-methods:
      catch_hotspot for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no.
    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed1 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed2 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed3 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed4 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed5 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed6 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_dt_chd_des_mas for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_dt_chd_tro_mas for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_finished1 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished2 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished3 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished4 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished5 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_data_changed_finished6 for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_dt_chd_f_des_mas for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_dt_chd_f_tro_mas for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.


**  Métodos do ALV de torca de materiais.
    class-methods:
      on_data_changed_finished_mat for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells,

      on_data_changed_mat for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      on_u_c_des_mas for event user_command of cl_gui_alv_grid
        importing e_ucomm,

      on_u_c_tro_mas for event user_command of cl_gui_alv_grid
        importing e_ucomm.

**  Métodos do ALV de torca de materiais Novo.
    class-methods:
      on_dtchfmat for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells,

      on_dtchmat for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_onf4 for event onf4 of cl_gui_alv_grid
        importing e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display,

      on_dtchfnew for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells,

      on_dtchnew for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_btclick for event button_click of cl_gui_alv_grid
        importing es_col_id
                  es_row_no,
      on_btclick_tro_mas for event button_click of cl_gui_alv_grid
        importing es_col_id
                  es_row_no.

endclass.                    "lcl_event_handler DEFINITION

class zcl_util_ definition.

  public section.
    class-methods:
      get_desc_parc  importing input type char10  returning value(return) type name1,
      get_desc_werks importing input type werks_d returning value(return) type name1,
      get_desc_route importing input type route   returning value(return) type name1,
      set_insert_90 importing doc_simulador type zsded003
                              vbeln_old     type vbeln
                              vbeln_new     type vbeln optional
                              categoria     type char1
                              gerais        type any,
      get_seq_90 importing input         type zsded003
                 returning value(return) type numc4,
      get_vbak importing input         type vbeln
               returning value(return) type vbak,
      get_vbkd importing input         type vbeln
               returning value(return) type vbkd,
      get_mara importing input         type matnr
               returning value(return) type matkl,
      get_mard importing matnr         type matnr
                         werks         type werks_d
                         lgort         type lvc_value
               returning value(return) type sy-subrc,
      get_mchb importing matnr         type matnr
                         werks         type werks_d
                         lgort         type lgort_d
                         charg         type lvc_value
               returning value(return) type sy-subrc,
      get_zsdt0037 importing input         type tt_saida
                   returning value(return) type sy-subrc,
      verifica_erros importing input  type vbeln
                               input1 type posnr optional,
      check_romaneio importing input         type vbeln
                               input1        type posnr optional
                     returning value(return) type sy-subrc,
      check_faturamento importing input         type vbeln
                        returning value(return) type sy-subrc,
      check_desc_abs importing _vbeln        type vbeln
                               _posnr        type posnr
                               des           type netwr_ap optional
                               dir           type c optional
                     returning value(return) type kbetr,
      modify_ordem importing i_auart         type auart
                             i_acao          type sy-ucomm
                             i_kunnr         type kunnr optional
                             i_txt_ordem     type char255 optional
                             i_txt_item      type char255 optional
                             t_ov            type zsds015_t
                             t_obs           type tline_t optional
                             i_extcal        type flag optional "FF ##145609
                   changing  value(i_vbeln)  type vbeln
                             value(t_return) type bapiret2_t optional,
      confirm_ordem,
      create_ordem,
      get_0048 importing bukrs         type bukrs
                         vtweg         type vtweg
                         spart         type spart
                         vkbur         type vkbur  optional
                         werks         type werks_d optional
                         lgort         type lgort_d
               returning value(return) type lgort_d,
      get_target_qty importing _auart        type auart
                               _vrkme        type vrkme
                               _dzmeng       type dzmeng
                     returning value(return) type dzmeng,
      get_unidade importing _auart        type auart
                            _vrkme        type vrkme
                            _spart        type spart optional
                            _dir          type char1 optional
                  returning value(return) type dzieme,
      check_vbuv,
      get_preco importing old like it_troca_old optional
                          new like it_troca_new optional,
*                          NEW LIKE IT_TROCA_NEW OPTIONAL,

      insert_90old importing simulador     type zsded003
                             ordem         type vbeln
                             material      type matnr
                             qtd           type rfmng optional
                             vlr_venda     type kbetr optional
                             uni_venda     type meins optional
                             categoria     type char1 optional
                   returning value(return) type zsdt0090,

      insert_90new importing simulador     type zsded003
                             ordem         type vbeln
                             material      type matnr
                             seq           type numc4 optional
                             qtd           type kwmeng optional
                             meins         type meins optional
                             vlr_venda     type kbetr optional
                             vlr_total     type netwr_ap optional
                             categoria     type char1 optional
                   returning value(return) type zsdt0090,
      block_linhas importing id         type int4
                   changing  value(new) like it_troca_new optional,
      aplica_desc_abs importing i_vbeln     type vbeln
                                i_posnr     type posnr
                                i_matnr     type matnr
                                i_diferenca type kbetr
                                i_zero      type char1 optional,
      get_kbetr importing i_vbeln       type vbeln
                          i_posnr       type posnr
                          i_add_imposto type char1 optional
                returning value(_konv)  type konv,
      get_vbap importing i_vbeln       type vbeln
                         i_matnr       type matnr
               returning value(e_vbap) type vbap,
      delete_ov importing i_vbeln         type vbeln
                returning value(t_return) type bapiret2_t,
      deleta_iten importing i_vbeln         type vbeln
                            i_posnr         type posnr
                            i_matnr         type matnr
                  returning value(t_return) type bapiret2_t,
      processa_desc importing i_vbeln    type vbeln
                              i_matnr    type matnr
                              i_vlr_real type netwr optional,
      modify_vlrtot importing simulador type zsded003
                              t_0090    type zsdt0090_t,
      back_qtd importing i_vbeln type vbeln,
      estorna_frete importing w_0090    type zsdt0090,
      get_87 importing matkl         type matkl
                       tpsim         type zsded004
                       inco1         type inco1
             returning value(return) type zsdt0087,
      arredonda changing dife type string,
      check_block importing table type tab_saida returning value(subrc) type sy-subrc,
      get_qtd_embarcado importing i_vbeln type vbeln
                                  i_posnr type posnr
                        exporting qtd_emb type zsded054,
      zsdmf001_atuali_ov_simulador importing i_vbeln     type vbeln
                                             i_posnr     type posnr
                                             i_matnr     type matnr
                                             i_diferenca type kbetr,
      processa_40 importing simulador type zsded003
                            i_vbeln   type vbeln
                            i_matnr   type matnr,
      indicator importing text type sy-ucomm,
      loop_40 importing simulador type zsded003
                        i_vbeln   type vbeln
                        i_matnr   type matnr
                        qtd       type int4,
      get_rb00 importing i_vbeln       type vbeln
                         i_posnr       type posnr
                         i_add_imposto type char1 optional
               returning value(_konv)  type konv,
      get_desconto importing i_vbeln         type vbeln
                             i_posnr         type posnr
                             i_add_imposto   type char1 optional
                   returning value(desc_abs) type kbetr,
      get_aut_qtd importing simulador      type zsded003
                  returning value(vlr_qtd) type dzwert,
      get_vlr_90 importing simulador  type zsded003
                           categoria  type zsded047
                 returning value(vlr) type dzwert,
      get_encerramento importing simulador      type zsded003
                       returning value(vlr_qtd) type dzwert,
      disparo_0116 importing i_vbeln type vbeln
                             i_posnr type posnr.

    class-data: at_contador type i.

endclass.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_event_handler implementation.

  method catch_hotspot.
    read table it_saida into wa_saida index e_row_id-index.
    if sy-subrc = 0.
      if e_column_id = 'VBELN2'.
        set parameter id 'AUN' field wa_saida-vbeln2.
        call transaction 'VA03' and skip first screen.
      endif.

      if e_column_id = 'DOC_SIMULACAO2'.
        perform f_call_zsdt0044.
      endif.

      perform refresh.
    endif.

  endmethod.                    "catch_hotspot_4

  method on_data_changed.

  endmethod.                    "on_data_changed

  method on_data_changed_finished.

    field-symbols <saida> type ty_saida.

    loop at it_saida assigning <saida>.

      if not <saida>-check is initial.
        <saida>-color = 'C510'.
      else.
        <saida>-color = ''.
      endif.

    endloop.

    call method cl_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "on_data_changed_finished

  method on_data_changed1.
    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          wg_ov    like line of tg_ov,
          _qtd_ov  type zsded054,
          _qtd     type zsded054.


    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'QT_TRAN'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      read table tg_ov into wg_ov index ls_good-row_id.
      wg_ov-qt_tran = lv_value.

      call method zcl_util_=>get_qtd_embarcado
        exporting
          i_vbeln = wg_ov-vbeln
          i_posnr = wg_ov-posnr
        importing
          qtd_emb = data(_qtd_emb).

      _qtd = lv_value.
      _qtd_ov = wg_ov-kwmeng - _qtd.

      if wg_ov-sd_disp lt wg_ov-qt_tran.
        move 0 to lv_value.
      elseif _qtd_ov < _qtd_emb.
        move 0 to lv_value.
        message |{ text-i02 } { text-i01 }| type 'I' display like 'E'.
      endif.

      call method zcl_manutencao_insumos=>chk_regra_proporcional
        exporting
          i_matnr      = wa_saida-matnr
          i_quantidade = conv #( lv_value )
        importing
          e_msg        = data(e_msg).
      if e_msg is not initial.
        move 0 to lv_value.
        message |{ e_msg }| type 'I' display like 'E'.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'QT_TRAN'
          i_value     = lv_value.

    endloop.
  endmethod.                    "on_data_changed

  method on_data_changed_finished1.
*** Método de atualização de dados na Tela
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "on_data_changed_finished

  method on_data_changed2.
    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          wg_ov2   like line of tg_ov2,
          v_limite type vbap-kwmeng,
          _qtd_ov  type zsded054,
          _qtd     type zsded054.


    loop at er_data_changed->mt_good_cells into ls_good where fieldname = 'QT_TRAN'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      read table tg_ov2 into wg_ov2 index ls_good-row_id.

*      BREAK WBARBOSA.
      call method zcl_util_=>get_qtd_embarcado
        exporting
          i_vbeln = wg_ov2-vbeln
          i_posnr = wg_ov2-posnr
        importing
          qtd_emb = data(_qtd_emb).

      _qtd = lv_value.

**  Checa se a tela está apontando para desmenbramento ou alteração de quantidade
      if wg_desmem is initial.
*      Checa a permissão de botão para incluir o limite
        if  v_botoes eq 'N'.
          v_limite = switch #( wg_ov2-spart
                              when '02' then switch #( wg_ov2-vrkme
                                                       when 'TO'           then wg_ov2-kwmeng + 1
                                                       when 'KG'           then wg_ov2-kwmeng + 1000
                                                       when 'SAC' or 'BAG' then wg_ov2-kwmeng + 25 )
                              when '03' then wg_ov2-kwmeng + 10
                              when '04' then switch #( wg_ov2-vrkme
                                                       when 'KG'           then wg_ov2-kwmeng + 40
                                                       when 'SAC' or 'BAG' then wg_ov2-kwmeng + 1
                                                        ) ) .
        else.
          v_limite = 999999999999.
        endif.

        if lv_value lt wg_ov2-sd_disp. "wg_ov2-rfmng.
          move 0 to lv_value.

*       "//Qtd digitada tem que ser menor que 1000 que a original e menor que a qtde faturada
        elseif lv_value gt v_limite
        or lv_value lt wg_ov2-sd_disp. "wg_ov2-rfmng.
          move 0 to lv_value.

*       "// Quantidade da OV não po de ficar inferior ao quantidade já solicitada da zsdt0079
          " 19/10/2020 IR041835  Ajuste deixar alterar a quantidade se for igual ou maior quantidade já solicitada da zsdt0079
        elseif _qtd < _qtd_emb.
          move 0 to lv_value.
          message |{ text-i02 } { text-i01 }| type 'I' display like 'E'.

*       "// Permite alteração do valor se não houver erros
        else.
          wg_ov2-qt_tran = lv_value.
        endif.

      else.

        _qtd_ov = wg_ov2-kwmeng - _qtd.

        v_limite = wg_ov2-sd_disp.

        if  v_limite lt lv_value.
          clear lv_value.
        elseif _qtd_ov < _qtd_emb.
          move 0 to lv_value.
          message |{ text-i02 } { text-i01 }| type 'I' display like 'E'.
        endif.

      endif.

      call method zcl_manutencao_insumos=>chk_regra_proporcional
        exporting
          i_matnr      = wa_saida-matnr
          i_quantidade = conv #( lv_value )
        importing
          e_msg        = data(e_msg).
      if e_msg is not initial.
        move 0 to lv_value.
        message |{ e_msg }| type 'I' display like 'E'.
      endif.

      if h_ucomm ne 'ALTERAR'.
        call method zcl_manutencao_insumos=>chk_desconto_abs_faturado
          exporting
            i_vbeln = wa_saida-vbeln
            i_matnr = wa_saida-matnr
          importing
            is_ok   = data(is_ok).

        if is_ok is not initial.
          move wa_saida-sd_disp to lv_value.
          message |{ text-003 }| type 'I' display like 'E'.
        endif.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'QT_TRAN'
          i_value     = lv_value.

    endloop.

  endmethod.                    "on_data_changed

  method on_data_changed3.

    data: ls_good   type lvc_s_modi,
          lv_value  type lvc_value,
          wa_redist like line of it_popup,
          v_limite  type vbap-kwmeng,
          v_name    type kna1-name1,
          _qtd_ov   type zsded054,
          _qtd      type zsded054.

    field-symbols <redist> type ty_saida.

    loop at er_data_changed->mt_good_cells into ls_good where fieldname = 'QT_TRAN'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      read table it_popup into wa_redist index ls_good-row_id.

      call method zcl_util_=>get_qtd_embarcado
        exporting
          i_vbeln = wa_redist-vbeln
          i_posnr = wa_redist-posnr
        importing
          qtd_emb = data(_qtd_emb).

      _qtd = lv_value.
      _qtd_ov = wa_redist-kwmeng - _qtd.

      if _qtd_ov < _qtd_emb.
        move 0 to lv_value.
        message |{ text-i02 } { text-i01 }| type 'I' display like 'E'.
      elseif lv_value <= wa_redist-sd_disp.
        wa_redist-qt_tran = lv_value.
        read table it_popup2 assigning <redist> with key vbeln2 = wa_redist-vbeln2
                                                          posnr  = wa_redist-posnr.
        <redist>-kwmeng = wa_redist-qt_tran.
      else.
        move 0 to lv_value.
      endif.

      call method zcl_manutencao_insumos=>chk_regra_proporcional
        exporting
          i_matnr      = wa_saida-matnr
          i_quantidade = conv #( lv_value )
        importing
          e_msg        = data(e_msg).
      if e_msg is not initial.
        move 0 to lv_value.
        message |{ e_msg }| type 'I' display like 'E'.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'QT_TRAN'
          i_value     = lv_value.

*      CALL METHOD grid1->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.
    endloop.

    loop at er_data_changed->mt_good_cells into ls_good.

      lv_value = ls_good-value.

      read table it_popup2 assigning <redist> index ls_good-row_id.

      case ls_good-fieldname.
        when 'KUNNR2'.
          <redist>-kunnr2 = lv_value.

          select single name1
            from kna1
            into v_name
            where kunnr eq <redist>-kunnr2.

          shift <redist>-kunnr2 left deleting leading space.

          loop at it_popup2 assigning <redist>.
            <redist>-kunnr2 = lv_value.
            <redist>-kunnr  = lv_value.
            <redist>-name1  = v_name.
          endloop.

        when 'WERKS'.
          loop at it_popup2 assigning <redist>.
            <redist>-werks = lv_value.
          endloop.
        when 'CHARG'.
          <redist>-charg = lv_value.
        when 'KBETR'.
          <redist>-kbetr = lv_value.
      endcase.

**  Checa se a tela está apontando para desmenbramento ou alteração de quantidade
*      IF WG_DESMEM IS INITIAL.
*        V_LIMITE = WG_OV2-KWMENG + 1000.
*
*        IF LV_VALUE LT WG_OV2-RFMNG.
*          MOVE 0 TO LV_VALUE.
*
***  Qtd digitada tem que ser menor que 1000 que a original e menor que a qtde faturada
*        ELSEIF LV_VALUE GT V_LIMITE
*        OR LV_VALUE LT WG_OV2-RFMNG.
*          MOVE 0 TO LV_VALUE.
*
***  Permite alteração do valor se não houver erros
*        ELSE.
*          WG_OV2-QT_TRAN = LV_VALUE.
*        ENDIF.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'QT_TRAN'
*            I_VALUE     = LV_VALUE.
*
*      ELSE.
*        MOVE LV_VALUE TO V_LIMITE.
*        IF V_LIMITE GT WG_OV2-SD_DISP.
*          CLEAR LV_VALUE.
*        ENDIF.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'QT_TRAN'
*            I_VALUE     = LV_VALUE.
*
*      ENDIF.

    endloop.

  endmethod.

  method on_data_changed4.

    data: ls_good     type lvc_s_modi,
          lv_value    type lvc_value,
          wl_desc_abs type dzmeng,
          netwr       type netwr_ap,
          wl_total    type dzmeng,
          vrkme       type vrkme,
          kmein       type kmein,
          matnr       type matnr,
          cadencia    type dzmeng.

    data: obj_0094     type ref to zcl_taxa_curva.
    create object: obj_0094.


    loop at er_data_changed->mt_good_cells into ls_good where fieldname = 'DESC_ABSOLUTO'.

      data(venc) = it_venc[ ls_good-row_id ].

      move: venc-doc_simulacao2 to wa_itens_id-doc,
            venc-vbeln2         to wa_itens_id-vbeln,
            venc-posnr          to wa_itens_id-posnr,
            ls_good-row_id      to wa_itens_id-index,
            venc-netwr          to wa_itens_id-vlr_ini.

      if line_exists( it_itens_id[ index = ls_good-row_id ] ).

        modify it_itens_id
          from wa_itens_id
            index ls_good-row_id
          transporting doc
                       vbeln
                       posnr
                       index.
      else.
        append wa_itens_id to it_itens_id.
      endif.

      wa_itens_id = it_itens_id[ index = ls_good-row_id ].

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'DESC_ABSOLUTO'
        importing
          e_value     = wl_desc_abs.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'NETWR'
        importing
          e_value     = netwr.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'VRKME'
        importing
          e_value     = vrkme.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'KMEIN'
        importing
          e_value     = kmein.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'MATNR2'
        importing
          e_value     = matnr.

      select single *
        from mara
        into @data(mat)
        where matnr eq @matnr.

*        PREÇO     QTD
*      IF KMEIN EQ VRKME.
*        NETWR = ( VENC-KWMENG * VENC-KBETR ) - WL_DESC_ABS.
*      ELSE.
*        OBJ_0094->SET_TIPO( 'VDI' ).
*        OBJ_0094->SET_MATKL( I_MATKL = MAT-MATKL
*                             I_BRGEW = MAT-BRGEW
*                           ).
*        OBJ_0094->SET_NETPR( I_NETPR = VENC-KBETR "Preço
*                             I_KMEIN = KMEIN ).   "Unidade de Medida do Preço
*
*        OBJ_0094->SET_ZIEME( VRKME ). "Unidade de Medida da QTD
*
*        CADENCIA = VENC-KWMENG. "Convertendo QTD em Cadencia
*        OBJ_0094->SET_CADENCIA_IN( CADENCIA ).
*
*        OBJ_0094->CALC_TOT( ).

*        NETWR = OBJ_0094->GET_TOTAL_PROPORCIONAL( ) - WL_DESC_ABS.
      netwr =  wa_itens_id-vlr_ini - wl_desc_abs.

*      ENDIF.

      move netwr to venc-netwr.

      modify it_venc
        from venc
          index ls_good-row_id
        transporting desc_absoluto
                     netwr.

      venc = it_venc[ ls_good-row_id ].
      wa_itens_id = it_itens_id[ index = ls_good-row_id ].

      wa_itens_id-vlr_atu =  venc-netwr.
      wa_itens_id-vlr_dif = wa_itens_id-vlr_atu - wa_itens_id-vlr_ini.

      modify it_itens_id
        from wa_itens_id
          transporting vlr_atu
                       vlr_dif
                       where index eq ls_good-row_id.

      clear: venc, wa_itens_id .

    endloop.

  endmethod.

  method on_data_changed5.

  endmethod.

  method on_data_changed6.

    data: ls_good    type lvc_s_modi,
          lv_value   type lvc_value,
          lva_dt_ent type zsdt0090-dt_entrega.

    loop at er_data_changed->mt_good_cells into ls_good.

      lv_value = ls_good-value.

      case ls_good-fieldname.
        when 'DT_ENTREGA'.
          lv_value = ls_good-value.

          move lv_value to lva_dt_ent.

          clear check_dt_ent.

          data(dt_ent) = it_dt_ent[ ls_good-row_id ].


          if lva_dt_ent is initial.
            message |A nova data de entrega deve ser informada para a OV: {  dt_ent-vbeln  }| type 'S' display like 'E'.
            exit.
          else.
            if lva_dt_ent < sy-datum.
              message |"A nova data de entrega informada para a OV: { dt_ent-vbeln }, deve ser maior ou igual ao dia de hoje"| type 'S' display like 'E'.
              exit.
            else.

              zcl_solicitacao_ov=>dia_util( exporting p_vencimento = lva_dt_ent
                                            importing e_subrc      = check_dt_ent ).

              if check_dt_ent is initial.
                message |A nova data de entrega informada para a OV: { dt_ent-vbeln } , deve ser dia útil| type 'S' display like 'E'.
                exit.
              else.
*                CALL METHOD er_data_changed->modify_cell
*                  EXPORTING
*                    i_row_id    = ls_good-row_id
*                    i_fieldname = 'DT_ENTREGA'
*                    i_value     = lv_value.
              endif.
            endif.
          endif.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'DT_ENTREGA'
              i_value     = lv_value.

      endcase.
    endloop.
  endmethod.

  method on_dt_chd_des_mas.

    data: lv_kunnr type kunnr,
          lv_saldo type zmenge.

    loop at er_data_changed->mt_good_cells into data(ls_good).

      case ls_good-fieldname.
        when 'KUNNR'.

          lv_kunnr = ls_good-value.

          select single name1
            from kna1
            into @data(lv_name)
            where kunnr eq @lv_kunnr.

          if sy-subrc is initial.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'NAME1'
                i_value     = lv_name.
          endif.

        when 'QTD'.

          read table it_saida into data(wa_saida_qtd) with key check = abap_true.

          ls_desmem_massa_header-saldo = wa_saida_qtd-sd_disp.
          lv_saldo = ls_good-value.

          data(lv_saldo_table) = reduce dzmeng( init s = 0 for ls_desmem_massa_item in it_desmem_massa_item next s = s + ls_desmem_massa_item-qtd ).
          lv_saldo_table += lv_saldo.

          if lv_saldo_table > wa_saida_qtd-sd_disp.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'QTD'
                i_value     = 0.

            message |Saldo Insuficiente!| type 'I' display like 'E'.
            return.

          endif.

          subtract lv_saldo from ls_desmem_massa_header-saldo.

          loop at it_desmem_massa_item assigning field-symbol(<fs_d_m_i>).

            if ls_good-row_id eq sy-tabix.
              continue.
            endif.

            <fs_d_m_i>-vbeln = gs_desmembramento-vbeln.
            <fs_d_m_i>-posnr = gs_desmembramento-posnr.
            <fs_d_m_i>-zmeng = gs_desmembramento-kwmeng.
            <fs_d_m_i>-zieme = gs_desmembramento-kmein.

            subtract <fs_d_m_i>-qtd from ls_desmem_massa_header-saldo.

          endloop.

          call function 'SAPGUI_SET_FUNCTIONCODE'
            exporting
              functioncode           = '/00'
            exceptions
              function_not_supported = 1.

      endcase.

    endloop.

    loop at it_desmem_massa_item assigning <fs_d_m_i>.
      <fs_d_m_i>-vbeln = gs_desmembramento-vbeln.
      <fs_d_m_i>-posnr = gs_desmembramento-posnr.
      <fs_d_m_i>-zmeng = gs_desmembramento-kwmeng.
      <fs_d_m_i>-zieme = gs_desmembramento-kmein.
    endloop.

    call method grid_desmemb_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endmethod.

  method on_dt_chd_tro_mas.

    data: ls_good      type lvc_s_modi,
          lv_value     type lvc_value,
          vl_value     type lvc_value,
          lv_qtd_check type zsded054,
          _qtd_ov      type zsded054,
          _qtd         type zsded054.

    loop at er_data_changed->mt_good_cells
                             into ls_good.

      read table gt_troca_material_massa_item assigning field-symbol(<ls_troca_material_massa_item>) index ls_good-row_id.

      case ls_good-fieldname.
        when 'QTD_RECEBIDA'.

          lv_value = ls_good-value.
          condense lv_value no-gaps.

          <ls_troca_material_massa_item>-qtd_recebida = conv #( lv_value ).

          try .
              <ls_troca_material_massa_item>-vlr_venda =
              <ls_troca_material_massa_item>-vlr_total /
              <ls_troca_material_massa_item>-qtd_recebida.
            catch cx_sy_zerodivide.
          endtry.

        when 'QTD_REMOVIDA'.

          lv_value = ls_good-value.
          condense lv_value no-gaps.

          read table gt_troca_material_massa_item assigning <ls_troca_material_massa_item> index ls_good-row_id.

          call method zcl_manutencao_insumos=>get_quantidade_embarcado
            exporting
              i_vbeln          = <ls_troca_material_massa_item>-vbeln
              i_posnr          = <ls_troca_material_massa_item>-posnr
            importing
              e_qtde_embarcada = data(e_qtde_embarcada).

          _qtd = lv_value.
          lv_qtd_check = <ls_troca_material_massa_item>-kwmeng - _qtd.

          <ls_troca_material_massa_item>-qtd_removida = lv_value.
          lv_value = <ls_troca_material_massa_item>-qtd_removida.

          if <ls_troca_material_massa_item>-sd_disp lt <ls_troca_material_massa_item>-qtd_removida.
            <ls_troca_material_massa_item>-qtd_removida = 0.
            lv_value = 0.
          endif.

          if lv_qtd_check < e_qtde_embarcada.
            <ls_troca_material_massa_item>-qtd_removida = 0.
            lv_value = 0.

            message |{ text-i02 } { text-i01 }| type 'I' display like 'E'.

            <ls_troca_material_massa_item>-vlr_venda = ''.
            <ls_troca_material_massa_item>-qtd_recebida = ''.
            <ls_troca_material_massa_item>-vlr_total = ''.

          endif.

          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'QTD_REMOVIDA'
              i_value     = lv_value.

          check _qtd is not initial.

          data(wpreco) = zcl_util_=>get_kbetr( i_vbeln = <ls_troca_material_massa_item>-vbeln i_posnr = <ls_troca_material_massa_item>-posnr ).
          <ls_troca_material_massa_item>-vlr_venda = ''.
          <ls_troca_material_massa_item>-qtd_recebida = ''.

          try .
              <ls_troca_material_massa_item>-vlr_total =
                    wpreco-kbetr * cond rfmng( when wpreco-kmein eq 'TO'
                                                        then <ls_troca_material_massa_item>-qtd_removida / 1000
                                                        else <ls_troca_material_massa_item>-qtd_removida ).
            catch cx_sy_zerodivide.
          endtry.

          if <ls_troca_material_massa_item>-sd_disp eq <ls_troca_material_massa_item>-qtd_recebida and
             <ls_troca_material_massa_item>-kwmeng eq <ls_troca_material_massa_item>-qtd_recebida.
            <ls_troca_material_massa_item>-vlr_total = <ls_troca_material_massa_item>-netwr.
          endif.

        when others.
      endcase.

      <ls_troca_material_massa_item>-itinerario = '@AW@'.

      call method zcl_manutencao_insumos=>verificar_itinerario
        exporting
          i_pc    = conv #( gs_troca_material_massa_header-werks )
          i_lr    = <ls_troca_material_massa_item>-kunnr
          i_check = abap_true
        importing
          is_ok   = data(is_ok).

      if is_ok is not initial.
        <ls_troca_material_massa_item>-itinerario = '@2K@'.
      endif.

    endloop.

    call method grid_troca_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endmethod.

  method on_data_changed_finished3.

*** Método de atualização de dados na Tela

    call method grid_r_c1->refresh_table_display
      exporting
        is_stable = wa_stable.

    call method grid_r_c2->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "on_data_changed_finished

  method on_data_changed_finished4.
    call method gr_venc->refresh_table_display
      exporting
        is_stable = wa_stable.
  endmethod.

  method on_data_changed_finished5.

    check e_modified is not initial.

    v_erro = abap_false.

    loop at et_good_cells into data(ls_good).

      read table it_dep_lote2 assigning field-symbol(<dep_lote>) index ls_good-row_id.

      case ls_good-fieldname.
        when 'LGORT'.
          if zcl_util_=>get_mard(
                                        matnr = <dep_lote>-matnr
                                        werks = <dep_lote>-werks
                                        lgort = ls_good-value
                            ) is initial.
            <dep_lote>-lgort = ls_good-value.
          else.
            <dep_lote>-lgort = ''.
            v_erro = abap_true.
          endif.

        when 'CHARG'.
          check ls_good-value is not initial.

          if zcl_util_=>get_mchb(
                                        matnr = <dep_lote>-matnr
                                        werks = <dep_lote>-werks
                                        lgort = <dep_lote>-lgort
                                        charg = ls_good-value
                            ) is initial.
            <dep_lote>-charg = ls_good-value.
          else.
            <dep_lote>-charg = ''.
            v_erro = abap_true.
          endif.

      endcase.

    endloop.

  endmethod.

  method on_data_changed_finished2.
*** Método de atualização de dados na Tela
    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "on_data_changed_finished

  method on_data_changed_finished6.
    call method gr_dt_ent->refresh_table_display
      exporting
        is_stable = wa_stable.
  endmethod.

  method on_dt_chd_f_des_mas.

    loop at it_desmem_massa_item assigning field-symbol(<fs_d_m_i>).
      <fs_d_m_i>-vbeln = gs_desmembramento-vbeln.
      <fs_d_m_i>-posnr = gs_desmembramento-posnr.
      <fs_d_m_i>-zmeng = gs_desmembramento-kwmeng.
      <fs_d_m_i>-zieme = gs_desmembramento-vrkme.
    endloop.

    call method grid_desmemb_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endmethod.

  method on_dt_chd_f_tro_mas.

*    LOOP AT IT_DESMEM_MASSA_ITEM ASSIGNING FIELD-SYMBOL(<FS_D_M_I>).
*      <FS_D_M_I>-VBELN = GS_DESMEMBRAMENTO-VBELN.
*      <FS_D_M_I>-POSNR = GS_DESMEMBRAMENTO-POSNR.
*      <FS_D_M_I>-ZMENG = GS_DESMEMBRAMENTO-KWMENG.
*      <FS_D_M_I>-ZIEME = GS_DESMEMBRAMENTO-KMEIN.
*    ENDLOOP.
*
    call method grid_troca_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endmethod.

  method on_u_c_des_mas.

    call method grid_desmemb_mas->get_selected_cells
      importing
        et_cell = tg_selectedcell.

*    LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
*      DELETE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

*    ENDLOOP.

    case e_ucomm.
      when 'ADD'.
      when 'DEL'.
      when others.
    endcase.

    call method grid_desmemb_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endmethod.

  method on_u_c_tro_mas.

*    CALL METHOD GRID_DESMEMB_MAS->GET_SELECTED_CELLS
*      IMPORTING
*        ET_CELL = TG_SELECTEDCELL.

*    LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
*      DELETE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

*    ENDLOOP.

*    CASE E_UCOMM.
*      WHEN 'ADD'.
*      WHEN 'DEL'.
*      WHEN OTHERS.
*    ENDCASE.


    call method grid_troca_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endmethod.

  method on_data_changed_mat.
    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          wg_ov    like line of tg_ov_mat.


    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'QT_TRAN'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      read table tg_ov_mat into wg_ov index ls_good-row_id.
      wg_ov-qt_tran = lv_value.
      if wg_ov-sd_disp lt wg_ov-qt_tran.
        move 0 to lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'QT_TRAN'
            i_value     = lv_value.
      endif.

    endloop.
  endmethod.                    "ON_DATA_CHANGED_MAT

  method on_data_changed_finished_mat.
*** Método de atualização de dados na Tela
    call method obj_grid_mat->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "ON_DATA_CHANGED_FINISHED_MAT

  method on_dtchfmat.
    call method obj_gtroca_old->refresh_table_display
      exporting
        is_stable = wa_stable.
  endmethod.

  method on_dtchmat.
    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          _qtd_ov  type zsded054,
          _qtd     type zsded054.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'QT_TRAN'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      try.
          data(w_ov) = it_troca_old[ ls_good-row_id ].
        catch cx_sy_itab_line_not_found.
      endtry.

      call method zcl_util_=>get_qtd_embarcado
        exporting
          i_vbeln = w_ov-vbeln
          i_posnr = w_ov-posnr
        importing
          qtd_emb = data(_qtd_emb).

      _qtd = lv_value.
      _qtd_ov = w_ov-kwmeng - _qtd.

      w_ov-qt_tran = lv_value.

      if w_ov-sd_disp lt w_ov-qt_tran.
        move 0 to lv_value.
      elseif w_ov-sd_disp lt w_ov-qt_tran.
        move 0 to lv_value.
      elseif _qtd_ov < _qtd_emb.
        move 0 to lv_value.
        _qtd = lv_value.
        message |{ text-i02 } { text-i01 }| type 'I' display like 'E'.

        loop at it_troca_new assigning field-symbol(<troca>) where id eq w_ov-id.
          <troca>-vlr_venda = ''.
          <troca>-qtd = ''.
          <troca>-vlr_total = ''.
          <troca>-itinerario = '@AW@'.
        endloop.

      endif.

      call method zcl_manutencao_insumos=>chk_regra_proporcional
        exporting
          i_matnr      = w_ov-matnr
          i_quantidade = conv #( _qtd )
        importing
          e_msg        = data(e_msg).

      if e_msg is not initial.

        move 0 to lv_value.
        _qtd = lv_value.
        message |{ e_msg }| type 'I' display like 'E'.

        loop at it_troca_new assigning <troca> where id eq w_ov-id.
          <troca>-vlr_venda = ''.
          <troca>-qtd = ''.
          <troca>-vlr_total = ''.
          <troca>-itinerario = '@AW@'.
        endloop.

      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'QT_TRAN'
          i_value     = lv_value.

      check _qtd is not initial.

      loop at it_troca_new assigning <troca> where id eq w_ov-id.
        data(wpreco) = zcl_util_=>get_kbetr( i_vbeln = w_ov-vbeln i_posnr = w_ov-posnr ).
        <troca>-vlr_venda = ''.
        <troca>-qtd = ''.
        try .
            <troca>-vlr_total = wpreco-kbetr * cond rfmng( when wpreco-kmein eq 'TO' then w_ov-qt_tran / 1000 else w_ov-qt_tran ).
          catch cx_sy_zerodivide.
        endtry.
        if w_ov-sd_disp eq w_ov-qt_tran and w_ov-kwmeng eq w_ov-qt_tran.
          <troca>-vlr_total = w_ov-netwr.
        endif.
        <troca>-itinerario = '@AW@'.
      endloop.

    endloop.

    call method obj_gtroca_new->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.

  method on_onf4.
    data(wa_troca) = it_troca_old[ es_row_no-row_id ].

** Encontra grupo de mercadorias
    select count(*)
      from mara
      where matnr eq wa_troca-matnr.

    if sy-subrc is initial.
** Busca materias que estão cadastrados pelo dep. insumos( TABELA ZSDT0036)
      select distinct c~matnr b~maktx c~meins a~meins c~werks_fornec c~inco1 c~waerk a~matkl c~vlr_venda a~spart a~mtart "SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980
        into table tg_mat
        from mara as a
        inner join makt as b on a~matnr eq b~matnr
        inner join zsdt0036 as c on a~matnr eq c~matnr
        where bukrs      eq wa_troca-vkorg
         and  waerk      eq wa_troca-waerk
         and  cultura    eq wa_troca-cultura
         and  eliminado  ne abap_true
         and  val_ate    >= sy-datum.

      loop at tg_mat assigning field-symbol(<f_mar>). <f_mar>-id = sy-tabix. endloop.

* Eliminar da seleção apenas os registros que: são do mesmo material/Frete/Centro da OV.
      delete tg_mat
           where matnr eq wa_troca-matnr
           and   inco1 eq wa_troca-inco1
           and   werks_fornec eq wa_troca-werks.

    endif.

    tg_material[] = value #( for ls1 in tg_mat ( corresponding #( ls1 ) ) ).

    free: gt_dselc, gt_return_tab.

    if tg_material[] is not initial.

      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'ID'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          value_org       = 'S'
        tables
          value_tab       = tg_material
          return_tab      = gt_return_tab
          dynpfld_mapping = gt_dselc
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.
*
      if sy-subrc is initial.
        try .
            data(id_mat) = gt_return_tab[ 1 ]-fieldval.
          catch cx_sy_itab_line_not_found.
            clear id_mat.
        endtry.

        replace all occurrences of '.'  in id_mat with '' ignoring case.
        condense id_mat no-gaps.

        try .
            data(mat) = tg_mat[ id = conv #( id_mat ) ].
          catch cx_sy_itab_line_not_found.
            clear mat.
        endtry.


        loop at it_troca_new assigning field-symbol(<troca>) where id eq wa_troca-id.

          move: mat-matnr to <troca>-matnr,
                mat-maktx to <troca>-maktx,
                mat-matkl to <troca>-matkl,
                mat-inco1 to <troca>-inco1,
                mat-werks_fornec to <troca>-werks_fornec,
                mat-meins to <troca>-meins,
                mat-meins to <troca>-uni_venda,
                mat-vlr_venda to <troca>-vlr_cadastrado,
                mat-meins to <troca>-uni_cadastrado,
                mat-spart to <troca>-spart,
                mat-mtart to <troca>-mtart.
          <troca>-itinerario = '@AW@'.
*          <TROCA>-VLR_TOTAL = MAT-VLR_VENDA * <TROCA>-QTD.

          call method zcl_manutencao_insumos=>verificar_itinerario
            exporting
              i_pc    = conv lifnr( <troca>-werks_fornec )
              i_lr    = wa_troca-kunnr
              i_check = abap_true
            importing
              is_ok   = data(is_check).

          if is_check is not initial.
            <troca>-itinerario = '@2K@'.
          endif.

          select count(*)
            from mara
            where matnr eq <troca>-matnr
              and mtart eq 'ZFER'.

          <troca>-style = value #(
                                      ( fieldname = 'LOTE'  style = cond #( when sy-subrc is initial then off else on ) )
                                      ( fieldname = 'LGORT' style = on )
                                      ( fieldname = 'QTD'   style = on )
                                 ).

        endloop.

      else.
        message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

*    APPEND LINES OF IT_TROCA_NEW TO _TROCA_NEW.

    call method obj_gtroca_new->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.

  method on_dtchfnew.
    data _matnr type matnr.

    loop at et_good_cells into data(ls_good)
                         where fieldname = 'QTD'.
*
      data(wa_troca) = it_troca_old[ ls_good-row_id ].
*
*      _MATNR = LS_GOOD-VALUE.
*
**     "//  Verifica se o Material Existe
*      SELECT COUNT(*)
*        FROM MARA
*        WHERE MATNR EQ _MATNR.
*
*      IF SY-SUBRC IS INITIAL.
*** Busca materias que estão cadastrados pelo dep. insumos( TABELA ZSDT0036)
*        SELECT DISTINCT C~MATNR B~MAKTX C~MEINS A~MEINS C~WERKS_FORNEC C~INCO1 C~WAERK A~MATKL C~VLR_VENDA
*          INTO TABLE TG_MAT1
*          FROM MARA AS A
*          INNER JOIN MAKT AS B ON A~MATNR EQ B~MATNR
*          INNER JOIN ZSDT0036 AS C ON A~MATNR EQ C~MATNR
*          WHERE BUKRS      EQ WA_TROCA-VKORG
*           AND  WAERK      EQ WA_TROCA-WAERK
*           AND  CULTURA    EQ WA_TROCA-CULTURA
*           AND  A~MATNR    EQ _MATNR
*           AND  ELIMINADO  NE ABAP_TRUE
*           AND  VAL_ATE    >= SY-DATUM.
*
** Eliminar da seleção apenas os registros que: são do mesmo material/Frete/Centro da OV.
*        DELETE TG_MAT
*             WHERE MATNR EQ WA_TROCA-MATNR
*             AND   INCO1 EQ WA_TROCA-INCO1
*             AND   WERKS_FORNEC EQ WA_TROCA-WERKS.
*
*        DATA(WA_MAT1) = TG_MAT1[ 1 ].

      loop at it_troca_new assigning field-symbol(<troca>) where id eq wa_troca-id.

*          MOVE: WA_MAT1-MATNR TO <TROCA>-MATNR,
*                WA_MAT1-MAKTX TO <TROCA>-MAKTX,
*                WA_MAT1-MATKL TO <TROCA>-MATKL,
*                WA_MAT1-INCO1 TO <TROCA>-INCO1,
*                WA_MAT1-WERKS_FORNEC TO <TROCA>-WERKS_FORNEC,
*                WA_MAT1-MEINS TO <TROCA>-MEINS,
*                WA_MAT1-MEINS TO <TROCA>-UNI_VENDA,
*                WA_MAT1-VLR_VENDA TO <TROCA>-VLR_CADASTRADO,
*                WA_MAT1-MEINS TO <TROCA>-UNI_CADASTRADO.
*
*          <TROCA>-VLR_TOTAL = <TROCA>-VLR_VENDA * <TROCA>-QTD.
*
*          ZCL_UTIL_=>BLOCK_LINHAS( <TROCA>-ID ).
*
*          SELECT COUNT(*)
*            FROM MARA
*            WHERE MATNR EQ _MATNR
*              AND MTART EQ 'ZFER'.
*
*          <TROCA>-STYLE = VALUE #(
*                                      ( FIELDNAME = 'LOTE'  STYLE = COND #( WHEN SY-SUBRC IS INITIAL THEN OFF ELSE ON ) )
*                                      ( FIELDNAME = 'LGORT' STYLE = ON )
**                                      ( FIELDNAME = 'MATNR' STYLE = ON )
*                                      ( FIELDNAME = 'QTD'   STYLE = ON )
*                                 ).

      endloop.
*      ENDIF.

      call method obj_gtroca_new->refresh_table_display
        exporting
          is_stable = wa_stable.

    endloop.

  endmethod.

  method on_dtchnew.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value,
          it_edit  type lvc_t_styl,
          wa_edit  type lvc_s_styl,
          l_index  type i.

    data _matnr type matnr.
    data _inco1 type inco1.
    data _werks type werks_d.

*"// INICIO US-169490 WBARBOSA 15/05/2025
    loop at er_data_changed->mt_good_cells
                             into ls_good.
*                             WHERE FIELDNAME = 'QTD'.

      data(w_new) = it_troca_new[ ls_good-row_id ].
      data(w_old) = it_troca_old[ id = w_new-id ].

      case ls_good-fieldname.
        when 'QTD'.

          lv_value = ls_good-value.
          condense lv_value no-gaps.

          w_new-qtd = conv #( lv_value ).

          try .
              w_new-vlr_venda = w_new-vlr_total / w_new-qtd.
            catch cx_sy_zerodivide.
          endtry.

          modify it_troca_new from w_new index ls_good-row_id.

        when 'MATNR'.

*"//  Verifica se o Material Existe
          call method zcl_manutencao_insumos=>check_material
            exporting
              i_matnr = conv #( ls_good-value )
            importing
              e_maktx = data(lv_desc_mat)                " Texto breve de material
              e_mara  = data(e_mara)                " Dados gerais de material
              is_ok   = data(is_ok).

          if is_ok is initial.

            call method er_data_changed->modify_cell(
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'MATNR'
                i_value     = '' ).

            message 'Material não Existe!' type 'I'.
            continue.
          endif.

*"//  Verifica se o Material Existe
          call method zcl_manutencao_insumos=>check_material_zfer
            exporting
              i_matnr = conv #( ls_good-value )
            importing
              is_ok   = is_ok.

*"// Verifica se o Material esta Cadastrado na Tabela de Parametrização
          call method zcl_manutencao_insumos=>get_cadastro_preco
            exporting
              i_bukrs   = w_old-vkorg
              i_waerk   = w_old-waerk
              i_cultura = w_old-cultura
              i_matnr   = conv #( ls_good-value )
            importing
              is_ok     = is_ok.

          if is_ok is initial.

            call method er_data_changed->modify_cell(
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'MATNR'
                i_value     = '' ).

            message 'Material não Parametrizado na ZSDT0043!' type 'I'.
            continue.
          endif.

          if is_ok is not initial.
            loop at it_troca_new assigning field-symbol(<fs_troca_new>) from ls_good-row_id.

              loop at <fs_troca_new>-style assigning field-symbol(<fs_style>) where fieldname eq 'INCO1'.
                <fs_style>-style = on.
              endloop.

              <fs_troca_new>-inco1 = ''.
              <fs_troca_new>-werks_fornec = ''.

              <fs_troca_new>-matnr = e_mara-matnr.
              <fs_troca_new>-maktx = lv_desc_mat.
              <fs_troca_new>-matkl = e_mara-matkl.
              <fs_troca_new>-spart = e_mara-spart.
              <fs_troca_new>-mtart = e_mara-mtart.

            endloop.
          endif.

        when 'INCO1'.

*"// Verifica se o Material esta Cadastrado na Tabela de Parametrização
          call method zcl_manutencao_insumos=>get_cadastro_preco
            exporting
              i_bukrs   = w_old-vkorg
              i_waerk   = w_old-waerk
              i_cultura = w_old-cultura
              i_matnr   = w_new-matnr
              i_inco1   = conv #( ls_good-value )
            importing
              is_ok     = is_ok.

          if is_ok is initial.

            call method er_data_changed->modify_cell(
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'INCO1'
                i_value     = '' ).

            loop at it_troca_new assigning <fs_troca_new> from ls_good-row_id.
              loop at <fs_troca_new>-style assigning <fs_style> where fieldname eq 'INCO1'.
                <fs_style>-style = on.
              endloop.
            endloop.

            message 'Incoterms não Parametrizado na ZSDT0043!' type 'I'.
            continue.
          endif.

          if is_ok is not initial.
            loop at it_troca_new assigning <fs_troca_new> from ls_good-row_id.

              loop at <fs_troca_new>-style assigning <fs_style> where fieldname eq 'WERKS_FORNEC'.
                <fs_style>-style = on.
              endloop.

              <fs_troca_new>-werks_fornec = ''.

            endloop.
          endif.

        when 'WERKS_FORNEC'.

*"// Verifica se o Material esta Cadastrado na Tabela de Parametrização
          call method zcl_manutencao_insumos=>get_cadastro_preco
            exporting
              i_bukrs        = w_old-vkorg
              i_waerk        = w_old-waerk
              i_cultura      = w_old-cultura
              i_matnr        = w_new-matnr
              i_inco1        = w_new-inco1
              i_werks_fornec = conv #( ls_good-value )
            importing
              ls_preco       = data(ls_preco)
              is_ok          = is_ok.

          if is_ok is initial.

            call method er_data_changed->modify_cell(
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = 'WERKS_FORNEC'
                i_value     = '' ).

            loop at it_troca_new assigning <fs_troca_new> from ls_good-row_id.
              loop at <fs_troca_new>-style assigning <fs_style> where fieldname eq 'WERKS_FORNEC'.
                <fs_style>-style = on.
              endloop.
            endloop.

            message 'Centro não Parametrizado na ZSDT0043!' type 'I'.
            continue.
          endif.

          if is_ok is not initial.
            loop at it_troca_new assigning <fs_troca_new> from ls_good-row_id.

              <fs_troca_new>-inco1          = ls_preco-inco1.
              <fs_troca_new>-werks_fornec   = ls_preco-werks_fornec.
              <fs_troca_new>-meins          = ls_preco-meins.
              <fs_troca_new>-uni_venda      = ls_preco-meins.
              <fs_troca_new>-vlr_cadastrado = ls_preco-vlr_venda.
              <fs_troca_new>-uni_cadastrado = ls_preco-meins.

            endloop.
          endif.

        when others.

      endcase.

      loop at it_troca_new assigning <fs_troca_new> from ls_good-row_id.

        if <fs_troca_new>-werks_fornec is initial or
           w_old-kunnr is initial.
          continue.
        endif.

        call method zcl_manutencao_insumos=>verificar_itinerario
          exporting
            i_pc    = conv lifnr( <fs_troca_new>-werks_fornec )
            i_lr    = w_old-kunnr
            i_check = abap_true
          importing
            is_ok   = data(is_check).

        if is_check is not initial.
          <fs_troca_new>-itinerario = '@2K@'.
        endif.

      endloop.

    endloop.
*"// FIM US-169490 WBARBOSA 15/05/2025

    call method obj_gtroca_new->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.

  method on_btclick.

    case es_col_id.
      when 'ITINERARIO'.
        read table it_troca_new assigning field-symbol(<fs_send_itinerario>) index es_row_no-row_id.
        if sy-subrc is initial.

          check <fs_send_itinerario>-itinerario ne '@2K@'.

          call method zcl_manutencao_insumos=>call_create_itinerario
            exporting
              i_pc = conv #( <fs_send_itinerario>-werks_fornec )
              i_lr = wa_saida-kunnr.

          call method zcl_manutencao_insumos=>verificar_itinerario
            exporting
              i_pc  = conv #( <fs_send_itinerario>-werks_fornec )
              i_lr  = wa_saida-kunnr
            importing
              is_ok = data(is_ok).

          if is_ok is not initial.
            <fs_send_itinerario>-itinerario = '@2K@'.
          endif.

        endif.
      when others.
    endcase.

  endmethod.

  method on_btclick_tro_mas.

    case es_col_id.
      when 'ITINERARIO'.
        read table gt_troca_material_massa_item  assigning field-symbol(<fs_troca_material_massa_item>) index es_row_no-row_id.

        check sy-subrc is initial.

        check <fs_troca_material_massa_item>-itinerario eq '@AW@'.

        call method zcl_manutencao_insumos=>call_create_itinerario
          exporting
            i_pc = conv #( gs_troca_material_massa_header-werks )
            i_lr = <fs_troca_material_massa_item>-kunnr.

        call method zcl_manutencao_insumos=>verificar_itinerario
          exporting
            i_pc  = conv #( gs_troca_material_massa_header-werks )
            i_lr  = <fs_troca_material_massa_item>-kunnr
          importing
            is_ok = data(is_ok).

        if is_ok is not initial.
          <fs_troca_material_massa_item>-itinerario = '@2K@'.
        endif.

      when others.
    endcase.

  endmethod.

endclass.                    "lcl_event_handler IMPLEMENTATION

class zcl_util_ implementation.

  method get_desc_parc.
    check input is not initial.
    select single name1 from lfa1 into return where lifnr eq input.
    check sy-subrc is not initial.
    message |Código de Parceiro nº. { input alpha = out } não é Válido!| type 'E'.
    clear wa_alt_gerais-cod_parc.
  endmethod.

  method get_desc_werks.
    check input is not initial.
    select single name1 from t001w into return where werks eq input.
    check sy-subrc is not initial.
    message |Código de Centro Fornecedor nº. { input alpha = out } não é Válido!| type 'E'.
    clear wa_alt_gerais-werks.
  endmethod.

  method get_desc_route.
    check input is not initial.
    select single bezei from tvrot into return where route eq input.
  endmethod.

  method get_seq_90.
    select count(*) from zsdt0090 into return where doc_simulacao eq input.
    add 1 to return.
  endmethod.

  method set_insert_90.

    data: _0090 type zsdt0090.

    _0090 = value #(
                     mandt         = sy-mandt
                     doc_simulacao = doc_simulador
                     sequencia     = zcl_util_=>get_seq_90( doc_simulador )
                     inco1         = cond #( when categoria eq 'F' then conv #( gerais ) else '' )
                     zpesagem      = cond #( when categoria eq 'G' then conv #( gerais ) else '' )
                     route         = cond #( when categoria eq 'I' then conv #( gerais ) else '' )
                     cod_parc      = cond #( when categoria eq 'P' then conv #( gerais ) else '' )
                     parvw         = cond #( when categoria eq 'P' then 'PC' else '' )
                     charg         = switch #( categoria  when 'L' then dep_lote2-charg )
                     lgort         = switch #( categoria  when 'L' then dep_lote2-lgort )
                     auartv        = zcl_util_=>get_vbak( vbeln_old )-auart
                     vbelv         = vbeln_old
                     posnv         = switch #( categoria  when 'F' or 'H' then wa_saida-posnr
                                                          when 'L' then dep_lote1-posnr )
                     spartv        = cond #( when categoria eq 'F' then wa_saida-spart else '' )
                     zmengv        = cond #( when categoria eq 'F' then wa_saida-kwmeng else '' )
                     ziemev        = cond #( when categoria eq 'F' then wa_saida-vrkme else '' )
                     netprv        = cond #( when categoria eq 'F' then wa_saida-kbetr else '' )
                     kmeinv        = cond #( when categoria eq 'F' then wa_saida-kmein else '' )
                     chargv        = switch #( categoria  when 'F' then wa_saida-charg
                                                          when 'L' then dep_lote1-charg )
                     matnrv        = cond #( when categoria eq 'F' then wa_saida-matnr else '' )
                     inco1v        = cond #( when categoria eq 'F' then wa_saida-inco1 else '' )
*                    WERKSV        = COND #( WHEN CATEGORIA EQ 'F' THEN WA_SAIDA-WERKS ELSE '' )
                     werksv        = switch #( categoria  when 'F' then wa_saida-werks
                                                          when 'H' then conv #( gerais )
                                                          else '' )
                     kunnrv        = cond #( when categoria eq 'F' then wa_saida-kunnr else '' )
                     lgortv        = switch #( categoria  when 'F' then wa_saida-lgort
                                                          when 'L' then dep_lote1-lgort )
                     categoria     = categoria
                     usnam         = sy-uname
                     data_atual    = sy-datum
                     hora_atual    = sy-uzeit
                  ).

    insert into zsdt0090 values _0090.
    append value #( message = cond #( when sy-subrc is initial
                                                              then |OV. { vbeln_old } foi Incluida na ZSDT0090 Categoria: { categoria }|
                                                              else |OV. { vbeln_old } não foi Incluida na ZSDT0090  Categoria: { categoria }| )
                    type    = cond #( when sy-subrc is initial then 'S' else 'E'  )
                  ) to tl_return.

    move-corresponding _0090 to p_0090.

  endmethod.

  method get_vbak.
    select single * from vbak into return where vbeln eq input.
  endmethod.

  method get_vbkd.
    select single * from vbkd into return where vbeln eq input.
  endmethod.

  method get_mara.
    select single matkl from mara into return where matnr eq input.
  endmethod.

  method get_mard.
    select count(*)
      from mard
      where matnr eq matnr
        and werks eq werks
        and lgort eq lgort.
    return = sy-subrc. check return is not initial.
    message |Depósito { lgort } não expandido para o Material { matnr alpha = out } e Centro { werks }!| type 'S' display like 'E'.
  endmethod.

  method get_mchb.
    check charg is not initial.
    select count(*)
      from mchb
        where matnr eq matnr
          and werks eq werks
          and lgort eq lgort
          and charg eq charg.
    return = sy-subrc. check return is not initial.
    message |Lote { charg } não expandido para o Material { matnr alpha = out }, Centro { werks } e Deposito { lgort }!| type 'S' display like 'E'.
  endmethod.

  method get_zsdt0037.

    data(v_matkl) = zcl_util_=>get_mara( input-matnr ).

    select count(*)
      from zsdt0037
      where val_de         le @sy-datum
        and val_ate        ge @sy-datum
        and bukrs          eq @input-vkorg
        and matkl          eq @v_matkl
        and filial_origem  eq @input-werks
        and meins          eq @input-kmein
        and filial_destino eq @input-vkbur
        and waers          eq 'BRL'.

    return = sy-subrc. check return is not initial.
    message |Não existe Frete Associoado para o Item, Cadastrar na ZSDT0047!| type 'S' display like 'E'.

  endmethod.

  method verifica_erros.

    v_erro = abap_false.

    if  zcl_util_=>check_faturamento( input ) is initial and sy-ucomm eq 'OK'.
      v_erro = abap_true.
      message |Existe Faturamento para a Ordem { input alpha = out }| type 'S' display like 'E'.
    endif.

    if zpesagem_ is not initial.

      if wa_alt_gerais-zpesagem is initial and sy-ucomm eq 'OK'.
        clear: wa_alt_gerais-zpesagem, zpesagem_.
        v_erro = abap_true.
        message |O campo Zpesagem é Obrigatório!| type 'S' display like 'E'.
      endif.

      if wa_saida-zpesagem ne wa_alt_gerais-zpesagem.
        if zcl_util_=>check_romaneio( input ) is initial.
          clear: wa_alt_gerais-zpesagem, zpesagem_.
          v_erro = abap_true.
          message |Existe Romaneio Gerado para a Ordem { input alpha = out }| type 'S' display like 'E'.
        endif.
      else.
        clear: wa_alt_gerais-zpesagem, zpesagem_.
        v_erro = abap_true.
        message |Tipo Pesagem Igual a Anterior!| type 'S' display like 'E'.
      endif.

    endif.

    if inco1_ is not initial.

*      SELECT *
*        FROM ZSDT0087
*        INTO TABLE @DATA(IT_FRETE)
*        WHERE MATKL EQ @WA_SAIDA-MATKL
*          AND TPSIM EQ @WA_SAIDA-TPSIM
*          AND AUART EQ @WA_SAIDA-AUART.
*      IF SY-SUBRC IS NOT INITIAL.
*        V_ERRO = ABAP_TRUE.
*        CLEAR: WA_ALT_GERAIS-INCO1, INCO1_. MESSAGE |Dados não encontrado "Tabela ZSDT0087"!| TYPE 'S' DISPLAY LIKE 'E'.
*      ELSE.
*
*      ENDIF.

*      IF WA_SAIDA-INCO1 EQ WA_ALT_GERAIS-INCO1.
*        V_ERRO = ABAP_TRUE.
*        CLEAR: WA_ALT_GERAIS-INCO1, INCO1_. MESSAGE |Troca não Permitida| TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.

*      CASE WA_SAIDA-INCO1.
*        WHEN 'CIF'.
*          IF WA_ALT_GERAIS-INCO1 NE 'FOB' AND WA_ALT_GERAIS-INCO1 IS NOT INITIAL.
*            V_ERRO = ABAP_TRUE.
*            CLEAR: WA_ALT_GERAIS-INCO1, INCO1_. MESSAGE |Troca não Permitida| TYPE 'S' DISPLAY LIKE 'E'.
*          ENDIF.
*        WHEN 'FOB'.
*          IF WA_ALT_GERAIS-INCO1 NE 'CIF' AND WA_ALT_GERAIS-INCO1 IS NOT INITIAL.
*            V_ERRO = ABAP_TRUE.
*            CLEAR: WA_ALT_GERAIS-INCO1, INCO1_. MESSAGE |Troca não Permitida| TYPE 'S' DISPLAY LIKE 'E'.
*          ENDIF.
*        WHEN OTHERS.
*          CLEAR INCO1_.
*          V_ERRO = ABAP_TRUE.
*          MESSAGE |Tipo de Frete não Pode ser Alterado!| TYPE 'S' DISPLAY LIKE 'E'.
*      ENDCASE.

      if wa_alt_gerais-inco1 is initial and sy-ucomm eq 'OK'.
        v_erro = abap_true.
        message |O campo Incoterms é Obrigatório!| type 'S' display like 'E'.
      endif.

*     Caso o Spart eq 03 Defensivos e Incoterns eq 'FOB' não valida.
      if wa_saida-spart ne '03' and wa_alt_gerais-inco1 ne 'FOB' and wa_alt_gerais-inco1 is not initial.
*     Verifica se existe Frete Associoado.
        v_erro = cond #( when zcl_util_=>get_zsdt0037( wa_saida ) is not initial then abap_true else v_erro ).
      endif.

    endif.

    if route_ is not initial.
      if  wa_alt_gerais-route eq wa_saida-route.
        v_erro = abap_true.
        message |Itinerário Igual a Anterior!| type 'S' display like 'E'.
      endif.

      if wa_alt_gerais-route is initial and sy-ucomm eq 'OK'.
        v_erro = abap_true.
        message |O campo Route é Obrigatório!| type 'S' display like 'E'.
      endif.

      select count(*)
        from trolz
        where route  eq wa_alt_gerais-route.

      if sy-subrc is not initial.
        v_erro = abap_true.
        message |Itinerário não Exixte!| type 'S' display like 'E'.
      endif.

    endif.

    if cod_parc_ is not initial.
      if  wa_alt_gerais-cod_parc eq wa_saida-cod_parc.
        v_erro = abap_true.
        message |Parceiro Igual a Anterior!| type 'S' display like 'E'.
      endif.

      if wa_alt_gerais-cod_parc is initial and sy-ucomm eq 'OK'.
        v_erro = abap_true.
        message |O campo Parceiro é Obrigatório!| type 'S' display like 'E'.
      endif.

      if wa_alt_gerais-cod_parc is not initial and sy-ucomm eq 'OK'.
        select single *
          from lfa1
          into @data(w_lfa1)
          where lifnr eq @wa_alt_gerais-cod_parc.

        if sy-subrc is not initial.
          v_erro = abap_true.
          message |Parceiro não Exixte!| type 'S' display like 'E'.
        else.

          select *
            from lfm1
            into table @data(t_lfm1)
            where lifnr eq @w_lfa1-lifnr.

          select *
            from lfb1
            into table @data(t_lfb1)
            where lifnr eq @w_lfa1-lifnr.

          if w_lfa1-sperr is not initial or
             w_lfa1-sperm is not initial or
             w_lfa1-sperq ne space.
            v_erro = abap_true.
            message |Fornecedor informado está Bloqueado!| type 'S' display like 'E'.
          else.

            if line_exists( t_lfm1[ lifnr = w_lfa1-lifnr sperm = abap_true  ] ).
              v_erro = abap_true.
              message |Fornecedor informado está Bloqueado!| type 'S' display like 'E'.
            endif.

            if line_exists( t_lfb1[ lifnr = w_lfa1-lifnr sperr = abap_true  ] ).
              v_erro = abap_true.
              message |Fornecedor informado está Bloqueado!| type 'S' display like 'E'.
            endif.

          endif.
        endif.
      endif.
    endif.

    if dep_lote_ is not initial.

      loop at it_dep_lote2 into data(dep_lote).
        if zcl_util_=>check_romaneio( input = dep_lote-vbeln input1 = dep_lote-posnr ) is initial.
          v_erro = abap_true.
          message |Ordem { dep_lote-vbeln } - { dep_lote-posnr } já Possui Romaneio!| type 'S' display like 'E'.
        endif.

        v_erro = cond #( when zcl_util_=>get_mard(
                              matnr = dep_lote-matnr
                              werks = dep_lote-werks
                              lgort = conv #( dep_lote-lgort )
                                                  ) is not initial then abap_true else v_erro ).

        v_erro = cond #( when zcl_util_=>get_mchb(
                             matnr = dep_lote-matnr
                             werks = dep_lote-werks
                             lgort = dep_lote-lgort
                             charg = conv #( dep_lote-charg )
                                                  ) is not initial then abap_true else v_erro ).
      endloop.
    endif.
    if centro_ is not initial.
      if wa_alt_gerais-werks is not initial.
        call method zcl_manutencao_insumos=>verificar_itinerario
          exporting
            i_pc  = conv #( wa_alt_gerais-werks )
            i_lr  = wa_saida-kunnr
          importing
            is_ok = data(is_ok).
        if is_ok is initial.
          v_erro = abap_true.
          message |Parceiros sem Itinerario!| type 'S' display like 'E'.
        endif.
      endif.
    endif.


  endmethod.

  method check_romaneio.
    if dep_lote_ is not initial.
      select count(*) from zsdt0001_item where vbeln eq input and posnr eq input1.
      return = sy-subrc.
    else.
      select count(*) from zsdt0001 where vbeln eq input.
      return = sy-subrc.
    endif.
  endmethod.

  method check_faturamento.
    select count(*) from vbfa where vbelv = input and vbtyp_n = 'J' and vbtyp_v = 'C'.
    return = sy-subrc.
  endmethod.

  method check_desc_abs.

    data: coeficiente    type p decimals 5,
          _descontoabslq type p decimals 3.

    select single *
      from vbak
      into @data(_vbak)
      where vbeln eq @_vbeln.

    select single *
      from vbap
      into @data(_vbap)
      where vbeln eq @_vbeln
        and posnr eq @_posnr.


* ---> S4 Migration - 10/07/2023 - DG
*    SELECT *
*      INTO TABLE @DATA(it_konv)
*      FROM V_KONV " ---> S4 Migration - 10/07/2023 - DG
*      WHERE kschl IN ('ICVA', 'ICBS', 'RB00')
*      AND   knumv  EQ @_vbak-knumv.

    select *
      into table @data(it_konv)
      from v_konv
      where kschl in ('ICVA', 'ICBS', 'RB00')
      and   knumv  eq @_vbak-knumv.
* <--- S4 Migration - 10/07/2023 - DG


    data(_descontoabs) = des.

    if _vbap-mwsbp is not initial.

      try .
          data(v_icva) = it_konv[ kposn = _vbap-posnr kschl = 'ICVA' ]-kbetr.
        catch cx_sy_itab_line_not_found.
      endtry.

      try .
          data(v_icbs) = it_konv[ kposn = _vbap-posnr kschl = 'ICBS' ]-kbetr.
        catch cx_sy_itab_line_not_found.
      endtry.

      try .
          v_icva = v_icva / it_konv[ kposn = _vbap-posnr kschl = 'ICVA' ]-kawrt.
        catch cx_sy_zerodivide.
      endtry.

      try .
          v_icbs = v_icbs / it_konv[ kposn = _vbap-posnr kschl = 'ICBS' ]-kawrt.
        catch cx_sy_zerodivide.
      endtry.

      try .
          coeficiente = 1 - ( ( v_icbs * ( v_icva / 100 ) ) / 100 ).
        catch cx_sy_zerodivide.
      endtry.

      if dir is initial.
        _descontoabslq  = _descontoabs * coeficiente. "Remove o Imposto
      else.
        _descontoabslq  = _descontoabs / coeficiente. "Adiciona o Imposto
      endif.

    endif.

    return = cond #( when _descontoabslq is initial then _descontoabs else _descontoabslq ).

  endmethod.

  method modify_ordem.

    data: b_vbeln     type vbeln,
          calc_1      type p decimals 5,
          calc_2      type p decimals 5,
          difer_0     type p decimals 5,
          valor_real  type p decimals 5,
          valor_vbap  type p decimals 5,
          diferenca   type p decimals 5,
          it_desc_abs type table of zsdt0041,
          tt_ov       type table of zsds015,
          t_calc      type  table of ty_calc.

    free: t_calc, tt_ov.

    loop at t_ov into data(wa_).

*     "// get do Iten da OV
      data(_vbap)  = zcl_util_=>get_vbap( i_vbeln = wa_-vbeln i_matnr = wa_-matnr ).

      append value #(
                      vbeln     = _vbap-vbeln
                      posnr     = _vbap-posnr
                      matnr     = _vbap-matnr
                    ) to t_calc.

      append wa_ to tt_ov.

    endloop.

    call function 'ZSDMF001_GERA_OV_COCKPIT'
      exporting
        i_auart     = i_auart
        i_acao      = i_acao
        i_kunnr     = i_kunnr
        i_txt_ordem = i_txt_ordem
        i_txt_item  = i_txt_item
        i_extcal    = i_extcal "FF ##145609
      importing
        e_vbeln     = i_vbeln
      tables
        it_ov       = tt_ov
        it_obs      = t_lines
        te_return   = t_return.

    "FF #145609 - inicio
    export i_vbeln  from i_vbeln  to memory id 'memory_i_vbeln'. "Import será feito na transação ZSDT0079/ZSDR0037 e no ZSDR0038
    "FF #145609 - fim

*   "//aplicando o desconto se existir no Item da OV
    loop at t_calc assigning field-symbol(<wa_>).

      call method zcl_manutencao_insumos=>reprocessa_desconto_abs
        exporting
          i_simulador = wa_saida-doc_simulacao
          i_vbeln     = i_vbeln
          i_matnr     = <wa_>-matnr.

*      ZCL_UTIL_=>INDICATOR( |Processando O.V de Origem!| ).
**     "// Processa O.V antiga
*      ZCL_UTIL_=>PROCESSA_DESC( I_VBELN = <WA_>-VBELN I_MATNR = <WA_>-MATNR ).
*      ZCL_UTIL_=>INDICATOR( |Processando O.V Nova!| ).
**     "// Processa O.V Nova
*      ZCL_UTIL_=>PROCESSA_DESC( I_VBELN = I_VBELN     I_MATNR = <WA_>-MATNR ).
**     "// Processa 40
*      ZCL_UTIL_=>LOOP_40(
*        SIMULADOR = WA_SAIDA-DOC_SIMULACAO
*        I_VBELN   = I_VBELN
*        I_MATNR   = <WA_>-MATNR
*        QTD       = 10
*      ).
    endloop.

  endmethod.

  method create_ordem.

    data:
      tl_texto         type catsxt_longtext_itab,
      wl_bape_vbak     type bape_vbak,
      wl_bape_vbakx    type bape_vbakx,
      wl_header_in     type bapisdhd1,
      wl_header        like wa_saida,
      wl_posnr         type sy-tabix,
      tl_items_in      type table of bapisditm,
      tl_conditions_in type table of bapicond,
      tl_schedules_in  type table of bapischdl,
      tl_return_aux    type table of bapiret2,
      tl_text_in       type table of bapisdtext,
      tl_partners      type table of bapiparnr,
      tl_bapiparex     type table of bapiparex,
      wl_header_inx2   type bapisdh1x,
      wl_matkl         type mara-matkl,
      wl_vbeln         type vbak-vbeln,
      wl_text          type dd04v-ddtext,
      it_0041          type table of zsdt0041.

    data _total type netwr.
    data new_total type netwr.
    data old_total type netwr.
    data vlr type netwr.

    free: tl_partners, tl_items_in, tl_conditions_in, tl_schedules_in, tl_return, tl_return_aux.

    data(t_troca) = t_troca_aux.
    data(_troca) = t_troca_aux.
    data(tr_old) = t_troca_aux[ 1 ].

    sort _troca by inco1 spart auart.
    delete adjacent duplicates from _troca comparing inco1 spart auart.

    move-corresponding tr_old to wl_header.

    call function 'CATSXT_SIMPLE_TEXT_EDITOR'
      exporting
        im_title = 'Texto para Ordem de Venda'
      changing
        ch_text  = tl_texto.

    tl_text_in =
    value #( for ls in tl_texto
               (
                 text_line  = ls
                 text_id    = '0002'
                 langu      = sy-langu
                 format_col = '/'
                )
            ).

    sort t_troca_aux by posnr.

    free tl_return.
    loop at _troca into data(w_saldo).

      data(cont) = 0.

      loop at it_troca_old assigning field-symbol(<troca_old>).
        <troca_old>-check = abap_false.
      endloop.

      loop at t_troca_aux into data(w_iten) where inco1 eq w_saldo-inco1
                                              and spart eq w_saldo-spart
                                              and auart eq w_saldo-auart.
*--> 29/07/2023 - MG-5795 – FC - Início
        data(lv_tabix) = sy-tabix.
*<-- 29/07/2023 - MG-5795 – FC – Fim

        add 1 to cont.

        loop at it_troca_old assigning <troca_old>
          where vbeln eq w_iten-vbeln
            and posnr eq w_iten-posnr.
          <troca_old>-check = abap_true.
        endloop.

*---MONTA DADOS DE PARCEIRO
        append value #(
                        partn_role = 'AG'
                        partn_numb = wl_header-kunnr
                      ) to tl_partners.

*  Cabeçario do Documento
        wl_header_in = value #(
                                sales_org  = w_iten-vkorg
                                distr_chan = w_iten-vtweg
                                sales_off  = w_iten-vkbur
                                sales_grp  = w_iten-vendedor(3)
                                purch_no_c = |{ w_iten-cultura }-{ w_iten-safra }-{ w_iten-doc_simulacao }|
                                currency   = w_iten-waerk
                                pymt_meth  = 'P'
                                fix_val_dy = get_vbkd( w_iten-vbeln )-valdt
                                exrate_fi  = get_vbkd( w_iten-vbeln )-kurrf
                                pmnttrms   = switch #( w_iten-tpsim when 'TS' then 'I001'
                                                                    when 'TT' then 'I008'
                                                                    when 'AD' then 'I002'
                                                                    when 'VV' or 'VF' or 'BN' then 'I003'
                                                                    when 'TV' then 'I004'
                                                                    when 'VP' then 'I005'
                                                                    when 'PM' then 'I006' )
                                incoterms1 = w_iten-inco1
                                incoterms2 = w_iten-inco1
                                division   = w_iten-spart
                                doc_type   = w_iten-auart
                              ).

        wl_posnr = conv #( cont ).
        multiply wl_posnr by 10.

*       "// Dados Parceiro PONTO DE COLETA
        if w_iten-werks is not initial.
          append value #(
                          partn_role = 'PC'
                          partn_numb = |{ w_iten-werks alpha = in }|
                        ) to tl_partners.
        endif.


*        SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980

        "wl_bape_vbak-zpesagem = COND #( WHEN w_iten-werks EQ '0175' THEN '01' ELSE '02').
        wl_bape_vbak-zpesagem = obj_zcl_util_sd->set_tp_pesagem_ov_simulador( centro = w_iten-werks
                                                                              matnr  = w_iten-matnr ).

        append value #(
                        structure = 'BAPE_VBAK'
                        valuepart1 = wl_bape_vbak
                      ) to tl_bapiparex.

        wl_bape_vbakx-zpesagem = abap_true.
        append value #(
                        structure     = 'BAPE_VBAKX'
                        valuepart1 = wl_bape_vbakx
                      ) to tl_bapiparex.

        append value #(
                        store_loc = get_0048(
                                              bukrs = w_iten-vkorg
                                              vtweg = w_iten-vtweg
                                              spart = w_iten-spart
                                              vkbur = w_iten-vkbur
                                              werks = w_iten-werks
                                              lgort = w_iten-lgort
                                             )

                        itm_number   = wl_posnr

*--> 29/07/2023 - MG-5795 – FC - Início
**"*---> 14/07/2023 - Migração S4 - LO
*                         material     = w_iten-matnr
**                        material_long     = |{ w_iten-matnr ALPHA = IN  }|
**"*---> 14/07/2023 - Migração S4 - LO
*--> 29/07/2023 - MG-5795 – FC - Fim


                        target_qty   = get_target_qty(
                                                        _auart = wl_header_in-doc_type
                                                        _vrkme = w_iten-vrkme
                                                        _dzmeng =  conv #( w_iten-kwmeng )
                                                     )
                        target_qu    = get_unidade(
                                                     _auart = wl_header_in-doc_type
                                                     _vrkme = w_iten-vrkme
                                                  )
                        sales_unit   = get_unidade(
                                                     _auart = wl_header_in-doc_type
                                                     _vrkme = w_iten-vrkme
                                                  )
                        usage_ind    = 'I'
                        plant        = w_iten-werks
                        batch        = w_iten-charg
                        ship_point   = w_iten-werks
                        matfrgtgrp   = '00000001'
                      ) to tl_items_in.

        data(coeficiente_o) =
          zcl_solicitacao_ov=>get_imposto(
          _direcao = 'O'
          _vbeln   = w_iten-vbeln
          _posnr   = w_iten-posnr
        ).

        data(coeficiente_d) =
          zcl_solicitacao_ov=>get_imposto(
          _cliente    = w_iten-kunnr
          _fornecedor = conv #( |{ w_iten-werks alpha = in }| )
          _material   = w_iten-matnr
          _tipo_ordem = w_iten-auart
          _direcao    = 'D'
          _werks      = w_iten-werks  "<<RIM-SKM-IR120585-23.12.22
        ).

        append value #(
                          itm_number  = wl_posnr
                          currency    = w_iten-waerk
                          cond_value  = cond #( when coeficiente_d is not initial then w_iten-kbetr * coeficiente_d else w_iten-kbetr )
                          cond_unit   = get_unidade(
                                                     _auart = wl_header_in-doc_type
                                                     _vrkme = w_iten-kmein
                                                     _dir    = abap_true
                                                  )
                          cond_type   = 'PR00'
                      ) to tl_conditions_in.

        append value #(
                        itm_number = wl_posnr
                        req_qty    = get_target_qty(
                                                      _auart  = wl_header_in-doc_type
                                                      _vrkme  = w_iten-vrkme
                                                      _dzmeng = conv #( w_iten-kwmeng )
                                                    )
                        req_dlv_bl = '10'
                      ) to tl_schedules_in.

*--> 29/07/2023 - MG-5795 – FC - Início
        read table tl_items_in assigning field-symbol(<items>) index lv_tabix.

        if <items> is assigned.

          data(lv_len) = strlen( w_iten-matnr ).

          if lv_len > 18.
            <items>-material_long = w_iten-matnr.
          else.
            <items>-material      = w_iten-matnr.
          endif.

        endif.
*--> 29/07/2023 - MG-5795 – FC - Fim

      endloop.

* Criar Ordem
      "*---> 14/07/2023 - Migração S4 - LO
      call function 'SD_SALESDOCUMENT_CREATE' "#EC CI_USAGE_OK[2438131]
        exporting
          sales_header_in     = wl_header_in
        importing
          salesdocument_ex    = wl_vbeln
        tables
          return              = tl_return
          sales_partners      = tl_partners
          sales_items_in      = tl_items_in
          sales_schedules_in  = tl_schedules_in
          sales_conditions_in = tl_conditions_in
          sales_text          = tl_text_in
          extensionin         = tl_bapiparex.

      if not wl_vbeln is initial.

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = abap_true.

        select *
          from vbuv
          into table @data(tl_vbuv)
           where vbeln eq @wl_vbeln.

        if not tl_vbuv is initial.

          loop at tl_vbuv into data(_vbuv).

            clear: tl_return, wl_text.

            data(wl_fieldname) = _vbuv-fdnam.

            call function 'RM_DDIC_TEXTS_GET'
              exporting
                i_name                = wl_fieldname
                i_type                = 'DTEL'
                i_langu               = sy-langu
              importing
                e_ddtxt               = wl_text
              exceptions
                objtype_not_supported = 1
                illegal_input         = 2
                others                = 3.

            append value #(
                            type = 'E'
                            message = cond #( when sy-subrc is not initial
                                                then |Existem campos incompletos na OV: { _vbuv-fdnam }|
                                                else |Existem campos incompletos na OV: { wl_text }| )
                          ) to tl_return.

          endloop.

          wl_header_inx2-updateflag = 'D'.

          "*---> 14/07/2023 - Migração S4 - LO
          call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
            exporting
              salesdocument    = wl_vbeln
              order_header_inx = wl_header_inx2
            tables
              return           = tl_return_aux.

          if not line_exists( tl_return_aux[ type = 'E' ] ).
            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = abap_true.
          endif.

          loop at tl_return_aux into data(w_ret).
            append w_ret to tl_return.
          endloop.

          clear: wl_vbeln.

        else.

          if not t_ov is initial.

*            BREAK WBARBOSA.
*           "// selecção dos Itens Novos
            select *
              from vbap
              into table @data(wvbap)
              where vbeln eq @wl_vbeln.

**           "//Corrige o Novo Itens para o Valor calculado
*            LOOP AT WVBAP INTO DATA(_VBAP).
**             "// Pega o valor calculado para o Iten
*              TRY .
*                  DATA(VLR_T) = T_TROCA_AUX[ MATNR = _VBAP-MATNR ]-NETWR.
*                CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*              ENDTRY.
**             "// Processa o Itens com o Valor calculado
*              ZCL_UTIL_=>PROCESSA_DESC( I_VBELN = _VBAP-VBELN I_MATNR = _VBAP-MATNR I_VLR_REAL = VLR_T ).
*            ENDLOOP.

            delete t_ov where posnr eq '000000'.

            loop at it_troca_old into data(_troca_old) where check eq abap_true.
              clear: old_total, new_total, _total, vlr.

              data(troca_new) = it_troca_new[ id = _troca_old-id ].
              data(_simulacao) = t_troca[ vbeln = _troca_old-vbeln ]-doc_simulacao.
*             "// Dispara para 90 o Iten Velho
              p_0090 = insert_90old(
                simulador = _simulacao
                ordem     = _troca_old-vbeln
                material  = _troca_old-matnr
                qtd       = _troca_old-qt_tran
                categoria = 'M'
              ).

              data(table_ov) = t_ov.
              delete table_ov where matnr ne _troca_old-matnr.
*             "// Atualiza o Iten Velho Removendo a quantidade
              call function 'ZSDMF001_ATUALI_OV_MAT'
                exporting
                  i_soma    = 'A'
                tables
                  it_ov     = table_ov
                  te_return = tl_return.

              call method zsdmf001_atuali_ov_simulador
                exporting
                  i_vbeln     = _troca_old-vbeln
                  i_posnr     = _troca_old-posnr
                  i_matnr     = _troca_old-matnr
                  i_diferenca = zcl_util_=>get_desconto( i_vbeln = _troca_old-vbeln i_posnr = _troca_old-posnr i_add_imposto = abap_true ).

*             "// Reprocessa o Iten Velho
              zcl_util_=>processa_desc( i_vbeln = _troca_old-vbeln i_matnr = _troca_old-matnr ).

*           "// Get dados da VBAP do Velho e do Novo
              data(_old)  = zcl_util_=>get_vbap( i_vbeln = _troca_old-vbeln i_matnr = _troca_old-matnr ). "// old
              data(_new)  = zcl_util_=>get_vbap( i_vbeln = wl_vbeln         i_matnr = troca_new-matnr  ). "// new

*           "// Soma do impostos e valores liquido do novo e velho.
              add _old-netwr to old_total.
              add _old-mwsbp to old_total.
              add _new-netwr to new_total.
              add _new-mwsbp to new_total.
              _total = old_total + new_total.

*            LOOP AT IT_TROCA_OLD INTO DATA(_TROCA_OLD) WHERE CHECK EQ ABAP_TRUE.
*           "//  verifica se há diferenças
              if _troca_old-netwr ne _total.
                vlr = _troca_old-netwr - _total.
                add vlr to new_total.
*               "//Reprocessa com o Valor calculado
                zcl_util_=>processa_desc( i_vbeln = wl_vbeln i_matnr = troca_new-matnr i_vlr_real = new_total ).
              endif.

              _new = zcl_util_=>get_vbap( i_vbeln = wl_vbeln i_matnr = troca_new-matnr ). "// new
              p_0090 = insert_90new(
                simulador = _simulacao
                ordem     = wl_vbeln
                material  = troca_new-matnr
                seq       = p_0090-sequencia
                qtd       = troca_new-qtd
                meins     = troca_new-meins
                vlr_venda = troca_new-vlr_venda
                vlr_total = ( _new-netwr + _new-mwsbp )
              ).

              zcl_util_=>disparo_0116( i_vbeln = p_0090-vbeln i_posnr = p_0090-posnn ).

              wa_saida-vbeln = wl_vbeln.
              perform hedge using 'M'.

            endloop.

*           "// Processa 40
            zcl_util_=>loop_40(
              simulador = _simulacao
              i_vbeln   = wl_vbeln
              i_matnr   = troca_new-matnr
              qtd       = 1
            ).
          endif.

        endif.
      else.
        append value #(
                         message = 'Ov não foi Criado'
                         type = 'E'
                      ) to tl_return.
      endif.

      free: tl_partners, tl_items_in, tl_conditions_in, tl_schedules_in, tl_return_aux.

    endloop.
  endmethod.

  method get_0048.

    return = lgort.

    check spart eq '03' or spart  eq '04'.

    data(condicao) = |BUKRS EQ { bukrs } AND VTWEG EQ { vtweg } AND SPART EQ { spart } AND { cond #( when spart eq '03' then |VKBUR EQ { vkbur }| else |WERKS_D EQ { werks }| ) }|.

    select single lgort
      from zsdt0048
      into return
        where (condicao).

  endmethod.

  method get_target_qty.

    return = _dzmeng.

    if ( _auart  eq 'ZFTE' or
         _auart  eq 'ZSEM' or
         _auart  eq 'ZOSM' or
         _auart  eq 'ZOFE' )
       and _vrkme eq 'TO'.
      multiply return by 1000.
    endif.

  endmethod.

  method get_unidade.

    if ( _auart  eq 'ZFTE' or
         _auart  eq 'ZSEM' or
         _auart  eq 'ZOSM' or
         _auart  eq 'ZOFE' )
       and _vrkme eq 'TO'.

      return = cond #( when _dir is not initial then _vrkme else 'KG'  ).

    elseif _auart  eq 'ZRFU' and _vrkme eq 'TO'.

      if _spart eq '02' or _spart eq '04'.
        return = _vrkme.
      endif.

    endif.

  endmethod.

  method check_vbuv.

  endmethod.

  method confirm_ordem.

    data _total type netwr.
    data new_total type netwr.
    data old_total type netwr.
    data: zmessage type char255.
    data: ws_vbap type vbap.
    data vlr type netwr.

    data erro type c.
    data wl_vbeln type vbeln.
    free: it_ov, t_troca_aux.

    loop at it_troca_old into data(lwa_troca_old).

      if lwa_troca_old-auart <> 'ZRFU' and lwa_troca_old-auart <> 'ZREM'.
        continue.
      endif.

      if erro is not initial.
        continue.
      endif.

      data(lwa_troca_new) = it_troca_new[ id = lwa_troca_old-id ].

      "Verificar se o centro é igual, se for diferente apresentar msg o erro para o usuario.
      if lwa_troca_old-werks <> lwa_troca_new-werks_fornec.
        clear: zmessage.
        zmessage = |{ text-e07 } { lwa_troca_old-auart }, { text-e08 }|.
        message zmessage  type 'E'.
        erro = abap_true.
        exit.
      endif.

      "Verificar se o grupo de material é igual, se for diferente apresentar msg o erro para o usuario.
      if lwa_troca_old-matkl <> lwa_troca_new-matkl.
        message text-e06 type 'E'.
        erro = abap_true.
        exit.
      endif.

      "Verificar se o inco1 é igual , se for diferente apresentar msg o erro para o usuario.
      if lwa_troca_old-inco1 <> lwa_troca_new-inco1.
        clear: zmessage.
        zmessage = |{ text-e07 } { lwa_troca_old-auart }, { text-e09 }|.
        message zmessage type 'E'.
        erro = abap_true.
        exit.
      endif.

      "Verificar se ja existe o material para o item da OV.
      clear: ws_vbap.
      select single * from vbap into ws_vbap
      where vbeln eq lwa_troca_old-vbeln
        and matnr eq lwa_troca_new-matnr.
      if sy-subrc eq 0.
        clear: zmessage.
        zmessage = |{ text-e11 } { lwa_troca_new-matnr }|.
        message zmessage type 'E'.
        erro = abap_true.
        exit.
      endif.

      call method zcl_manutencao_insumos=>verificar_itinerario
        exporting
          i_pc  = conv #( lwa_troca_new-werks_fornec )
          i_lr  = wa_saida-kunnr
        importing
          is_ok = data(is_ok).
      if is_ok is initial.
        clear: zmessage.
        zmessage = |{ text-e12 } { lwa_troca_new-matnr alpha = out }/{ wa_saida-kunnr alpha = out }|.
        message zmessage type 'E'.
        erro = abap_true.
        exit.
      endif.

    endloop.

    loop at it_troca_old into data(troca_old).

      if erro is not initial.
        continue.
      endif.

      data(troca_new) = it_troca_new[ id = troca_old-id ].

      if troca_old-qt_tran is initial.
        message text-e01 type 'E'.
        erro = abap_true.
        exit.
      endif.

      if troca_new-matnr is initial.
        message text-e02 type 'E'.
        erro = abap_true.
        exit.
      else.
        if not line_exists( tg_mat[ matnr = troca_new-matnr ] ).
*          MESSAGE |Material { TROCA_NEW-MATNR } inválido.| TYPE 'E'.
*          ERRO = ABAP_TRUE.
*          EXIT.
        endif.
      endif.

      if troca_new-qtd is initial.
        message text-e03 type 'E'.
        erro = abap_true.
        exit.
      endif.

      if troca_new-lgort is initial.
        message text-e04 type 'E'.
        erro = abap_true.
        exit.
      endif.

      select count(*)
             from mard
             where matnr eq troca_new-matnr
               and werks eq troca_new-werks_fornec
               and lgort eq troca_new-lgort.

      if sy-subrc is not initial.
        message text-e05 type 'E'.
        erro = abap_true.
        exit.
      endif.

      if troca_new-matkl eq troca_old-matkl and
         troca_new-werks_fornec eq troca_old-werks and
         troca_new-inco1 eq troca_old-inco1.
        data new_ type c.

*SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980 - INICIO item 01
        clear: v_mtart.
        select single mtart from mara into v_mtart where matnr eq troca_old-matnr.
        " CS2023000023 ZSDT0087 - Ajuste para Trocas de Materiais Fertilizantes - OVs ZFRU/ZREM - criar novo item - BG #100663
        if troca_new-spart eq '02'.
          if ( troca_old-auart  eq 'ZRFU' or  troca_old-auart  eq 'ZREM' ). "BUG IMPEDITIVO 102034 / AOENNING
            new_ = abap_false.
          else.
            if troca_new-mtart eq 'ZHAW'.
              new_ = abap_true.
            elseif troca_new-mtart ne v_mtart.
              new_ = abap_true.
            endif.
          endif.
        endif.
*SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980 - fim item 01

        select count(*)
          from vbap
        where vbeln eq troca_old-vbeln
          and matnr eq troca_new-matnr.

        if sy-subrc is initial.
          new_ = abap_true.
        endif.
      else.
        if ( troca_new-matkl eq troca_old-matkl )
          and ( troca_new-inco1 ne troca_old-inco1 )
          and ( troca_new-spart eq '02' )
          and ( troca_old-auart  eq 'ZRFU' or  troca_old-auart  eq 'ZREM' ).

          new_ = abap_false.
        else.
          new_ = abap_true.
        endif.
      endif.



      loop at it_troca assigning field-symbol(<f_saldo>)
                      where vbeln eq troca_old-vbeln
                        and posnr eq troca_old-posnr.

        data(w0087) = get_87(
          matkl = troca_new-matkl
          tpsim = <f_saldo>-tpsim
          inco1 = troca_new-inco1
        ).

        if w0087 is initial.
          message |Grp. Material: { troca_new-matkl }, | &&
                  |Tp Simulador: { <f_saldo>-tpsim }, | &&
                  |Incoterms: { troca_new-inco1 }. | &&
                  |Combinação não parametrizada na Transação ZSDT0085.| type 'I'.
          erro = abap_true.
          exit.
        endif.

      endloop.

      call method zcl_manutencao_insumos=>verificar_itinerario
        exporting
          i_pc  = conv #( troca_new-werks_fornec )
          i_lr  = wa_saida-kunnr
        importing
          is_ok = is_ok.
      if is_ok is initial.
        clear: zmessage.
        zmessage = |{ text-e12 } { troca_new-werks_fornec }/{ wa_saida-kunnr }|.
        message zmessage type 'E'.
        erro = abap_true.
        exit.
      endif.

      call method zcl_manutencao_insumos=>chk_regra_proporcional
        exporting
          i_matnr      = troca_new-matnr
          i_quantidade = troca_new-qtd
        importing
          e_msg        = data(e_msg).

      if e_msg is not initial.
        clear: zmessage.
        zmessage = |Destino: { e_msg }|.
        message zmessage type 'E'.
        erro = abap_true.
        exit.
      endif.

      call method zcl_manutencao_insumos=>chk_regra_proporcional
        exporting
          i_matnr      = troca_old-matnr
          i_quantidade = troca_old-qt_tran
        importing
          e_msg        = e_msg.

      if e_msg is not initial.
        clear: zmessage.
        zmessage = |Origem: { e_msg }|.
        message zmessage type 'E'.
        erro = abap_true.
        exit.
      endif.

    endloop.

    check erro is initial.

    loop at it_troca_old into troca_old.
      free it_ov.
      clear: old_total, new_total, _total, vlr.

      troca_new = it_troca_new[ id = troca_old-id ].

      append value #(
                      vbeln = troca_old-vbeln
                      posnr = troca_old-posnr
                      zmeng = troca_old-kwmeng - troca_old-qt_tran
                      matnr = troca_old-matnr
*                      DIF_DESC = ZCL_UTIL_=>GET_RB00( I_VBELN = TROCA_OLD-VBELN I_POSNR = TROCA_OLD-POSNR I_ADD_IMPOSTO = 'N' )-KBETR
                    ) to it_ov.


      clear: at_preco, at_difen.

      loop at it_troca assigning <f_saldo>
        where vbeln eq troca_old-vbeln
          and posnr eq troca_old-posnr.

        <f_saldo>-qt_tran = troca_old-qt_tran.

*        GET_PRECO(
*                   OLD = TROCA_OLD
*                   NEW = TROCA_NEW
*                   ).

*        <F_SALDO>-NETWR = TROCA_NEW-VLR_VENDA.
*<F_SALDO>-NETWR = AT_PRECO.

      endloop.

      append value #(
                  vbeln    = troca_old-vbeln
                  zmeng    = troca_new-qtd
                  matnr    = troca_new-matnr
                  charg    = troca_new-lote
                  vrkme    = troca_new-meins
                  werks    = troca_new-werks_fornec
                  lgort    = troca_new-lgort
                  netpr    = troca_new-vlr_venda
*                  DIF_DESC = ZCL_UTIL_=>GET_RB00( I_VBELN = TROCA_OLD-VBELN I_POSNR = TROCA_OLD-POSNR I_ADD_IMPOSTO = 'N' )-KBETR
                ) to it_ov.

      append lines of it_ov to t_ov.

      if new_ is initial.

        data(_simulacao) = it_troca[ vbeln = troca_old-vbeln ]-doc_simulacao.
        p_0090 = insert_90old(
          simulador = _simulacao
          ordem     = troca_old-vbeln
          material  = troca_old-matnr
          qtd       = troca_old-qt_tran
          categoria = 'M'
        ).
*       "// Devolve o Valor para a O.V Original
        call function 'ZSDMF001_ATUALI_OV_MAT'
          exporting
            i_soma    = 'A'
          tables
            it_ov     = it_ov
            te_return = tl_return.


        if line_exists( tl_return[ type = 'E' ] ).
          perform f_exibe_bapi.
          free: it_ov, t_ov.
          delete from zsdt0090 where doc_simulacao eq p_0090-doc_simulacao and sequencia eq p_0090-sequencia.
          commit work.
          leave to screen 0.
          exit.
        endif.

        call method zsdmf001_atuali_ov_simulador
          exporting
            i_vbeln     = troca_old-vbeln
            i_posnr     = troca_old-posnr
            i_matnr     = troca_old-matnr
            i_diferenca = zcl_util_=>get_desconto( i_vbeln = troca_old-vbeln i_posnr = troca_old-posnr i_add_imposto = abap_true ).

*       "// Reprocessa a O.V
        zcl_util_=>processa_desc( i_vbeln = troca_old-vbeln i_matnr = troca_old-matnr ).

        troca_new = it_troca_new[ id = troca_old-id ].
*
**       "// Get dados da VBAP do Velho e do Novo
        data(_old)  = zcl_util_=>get_vbap( i_vbeln = troca_old-vbeln i_matnr = troca_old-matnr ). "// old
        data(_new)  = zcl_util_=>get_vbap( i_vbeln = troca_old-vbeln i_matnr = troca_new-matnr ). "// new

        "//Aplica a Diferença entre todos as O.Vs com o VlrTot do Simulador
*       "// Soma do impostos e valores liquido do novo e velho.
        add _old-netwr to old_total.
        add _old-mwsbp to old_total.
        add _new-netwr to new_total.
        add _new-mwsbp to new_total.
        _total = old_total + new_total.

        if troca_old-netwr ne _total.
          vlr = troca_old-netwr - _total.
          add vlr to new_total.
*         "// Processa a Diferença encontrada entre o Simulador e a O.Vs
          zcl_util_=>processa_desc( i_vbeln = troca_old-vbeln i_matnr = troca_new-matnr i_vlr_real = new_total ).
*         "// Calcula se há diferença para ser aplicada
        endif.

        _new  = zcl_util_=>get_vbap( i_vbeln = troca_old-vbeln i_matnr = troca_new-matnr ). "// new
*       "// Disparo para 90
        p_0090 = insert_90new(
          simulador = _simulacao
          ordem     = troca_old-vbeln
          material  = troca_new-matnr
          seq       = p_0090-sequencia
          qtd       = troca_new-qtd
          meins     = troca_new-meins
          vlr_venda = troca_new-vlr_venda
          vlr_total = ( _new-netwr + _new-mwsbp )
        ).

        zcl_util_=>disparo_0116( i_vbeln = p_0090-vbeln i_posnr = p_0090-posnn ).

        wa_saida-vbeln = troca_old-vbeln.
        perform hedge using 'M'.

*         "// Processa 40
        zcl_util_=>loop_40(
          simulador = _simulacao
          i_vbeln   = troca_old-vbeln
          i_matnr   = troca_new-matnr
          qtd       = 10
        ).

        erro = abap_true.
      else.

        loop at it_troca assigning <f_saldo>
                where vbeln eq troca_old-vbeln
                  and posnr eq troca_old-posnr.

          w0087 = get_87(
            matkl = troca_new-matkl
            tpsim = <f_saldo>-tpsim
            inco1 = troca_new-inco1
          ).

          <f_saldo>-inco1   = troca_new-inco1.
          <f_saldo>-auart   = w0087-auart.
          <f_saldo>-spart   = w0087-spart.
          <f_saldo>-kwmeng  = troca_new-qtd.
          <f_saldo>-qt_tran = troca_old-qt_tran.
          <f_saldo>-matnr   = troca_new-matnr.
          <f_saldo>-matkl   = troca_new-matkl.
          <f_saldo>-charg   = troca_new-lote.
          <f_saldo>-kmein   = troca_new-meins.
          <f_saldo>-vrkme   = troca_new-meins.
          <f_saldo>-netwr   = troca_new-vlr_total.
          <f_saldo>-kbetr   = troca_new-vlr_venda.
          <f_saldo>-werks   = troca_new-werks_fornec.
          <f_saldo>-lgort   = troca_new-lgort.

        endloop.
      endif.
    endloop.

    if erro is initial.

      loop at it_troca assigning <f_saldo>.

        if line_exists( t_troca_aux[ inco1 = <f_saldo>-inco1
                                     spart = <f_saldo>-spart
                                     auart = <f_saldo>-auart
                                     matnr = <f_saldo>-matnr
                                     werks = <f_saldo>-werks
                                    ]
                      ).

          data(w_t_a) = t_troca_aux[ inco1 = <f_saldo>-inco1
                                     spart = <f_saldo>-spart
                                     auart = <f_saldo>-auart
                                     matnr = <f_saldo>-matnr
                                     werks = <f_saldo>-werks ].

          add <f_saldo>-netwr to w_t_a-sd_disp.
          add <f_saldo>-kwmeng to w_t_a-kwmeng.

          try .
              w_t_a-kbetr = w_t_a-sd_disp / w_t_a-kwmeng.
            catch cx_sy_itab_line_not_found.
          endtry.

          modify t_troca_aux from w_t_a
                            transporting kwmeng
                                         kbetr
                                         sd_disp
                                  where inco1 eq <f_saldo>-inco1 and
                                        spart eq <f_saldo>-spart and
                                        auart eq <f_saldo>-auart and
                                        matnr eq <f_saldo>-matnr and
                                        werks eq <f_saldo>-werks.

        else.
          <f_saldo>-sd_disp = <f_saldo>-netwr.
          append <f_saldo> to t_troca_aux.
        endif.

      endloop.
      create_ordem( ).
    endif.

    perform f_exibe_bapi.
    free: it_ov, t_ov.
    leave to screen 0.

  endmethod.

  method get_preco.

    at_preco = cond #( when old-vrkme ne old-meins
                              then switch #( old-vrkme when 'TO' or 'KG'
                                                        then switch #( old-meins when 'TO' then ( ( ( old-netpr / 1000 ) * old-qt_tran ) / new-qtd )
                                                                                 when 'KG' then ( ( ( old-netpr * 1000 ) * old-qt_tran ) / new-qtd )
                                                                                 else ( ( old-netpr * old-qt_tran ) / new-qtd )
                                                                     )
                                                        else ( ( old-netpr * old-qt_tran ) / new-qtd )
                                           )
                       else ( ( old-netpr * old-qt_tran ) / new-qtd )
                     ).


    at_difen = cond #( when old-vrkme ne old-meins
                              then switch #( old-vrkme when 'TO' or 'KG'
                                                        then switch #( old-meins when 'TO' then ( at_preco * new-qtd ) - ( ( old-netpr * old-qt_tran ) / 1000 )
                                                                                 when 'KG' then ( at_preco * new-qtd ) - ( ( old-netpr * old-qt_tran ) * 1000 )
                                                                                 else ( at_preco * new-qtd ) - ( old-netpr * old-qt_tran )
                                                                     )
                                                        else ( at_preco * new-qtd ) - ( old-netpr * old-qt_tran )
                                            )
                       else ( at_preco * new-qtd ) - ( old-netpr * old-qt_tran )
                     ).

  endmethod.

  method insert_90old.
*  Metodo espefico para Troca, em caso de utilização de outras ações tera que adaptar.
    data: seq type zsdt0090-sequencia.

    select count(*)
      from zsdt0090
      into seq
      where doc_simulacao eq simulador.

    select single * from vbak
      into @data(w_vbak)
        where vbeln eq @ordem.

    if sy-subrc is initial.

      select single * from vbap
        into @data(w_vbap)
          where vbeln eq @w_vbak-vbeln
        and matnr eq @material.

      select single * from vbkd
        into @data(w_vbkd)
          where vbeln eq @w_vbak-vbeln.

      select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
        into table @data(t_konv)
          where knumv eq @w_vbak-knumv
            and kschl in ('PR00', 'RB00')
            and kposn eq @w_vbap-posnr.

    endif.

    try .
        data(w_pr00) = t_konv[ kschl = 'PR00' ].
        data(w_rb00) = t_konv[ kschl = 'RB00' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    add 1 to seq.

    return-doc_simulacao = simulador.     "*DOC_SIMULACAO
    return-sequencia     = seq.           "*SEQUENCIA
    return-auartv        = w_vbak-auart.  "*AUARTV
    return-vbelv         = w_vbap-vbeln.  "*VBELV
    return-posnv         = w_vbap-posnr.  "*POSNV
    return-spartv        = w_vbap-spart.  "*SPARTV

    return-zmengv = switch #( categoria when 'A' or 'E' then qtd
                                        when 'M' then qtd * -1
                                        else 0
                            ).

*   RETURN-DESC_ABSOLUTO = W_RB00-KBETR.  "*DESC_ABSOLUTO
    return-ziemev        = w_vbap-zieme.  "*ZIEMEV

    return-netprv        = cond #( when categoria eq 'E' then vlr_venda else zcl_util_=>get_kbetr( i_vbeln = w_vbap-vbeln i_posnr = w_vbap-posnr )-kbetr ).
    return-kmeinv        = cond #( when categoria eq 'E' then uni_venda else w_pr00-kmein ).  "*KMEINV

    return-chargv        = w_vbap-charg.  "*CHARGV
    return-matnrv        = w_vbap-matnr.  "*MATNRV
    return-matklv        = w_vbap-matkl.  "*MATKLV
    return-inco1v        = w_vbkd-inco1.  "*INCO1V
    return-werksv        = w_vbap-werks.  "*WERKSV
    return-kunnrv        = w_vbak-kunnr.  "*KUNNRV
    return-categoria     = categoria.     "*CATEGORIA
    return-kurrf         = w_vbkd-kurrf.  "*KURRF
    return-usnam         = sy-uname.      "*USNAM
    return-data_atual    = sy-datum.      "*DATA_ATUAL
    return-hora_atual    = sy-uzeit.      "*HORA_ATUAL

    modify zsdt0090 from return.

    commit work.

  endmethod.

  method insert_90new.
*  METODO ESPEFICO PARA TROCA, EM CASO DE UTILIZAÇÃO DE OUTRAS AÇÕES TERA QUE ADAPTAR.

    data: sequencia type zsdt0090-sequencia.

    sequencia = seq.

    if sequencia is not initial.
      select single *
        from zsdt0090
        into return
        where sequencia eq sequencia
        and doc_simulacao eq simulador.
    else.
      select count(*)
        from zsdt0090
        into sequencia
        where doc_simulacao eq simulador.
    endif.

    select single * from vbak
      into @data(w_vbak)
        where vbeln eq @ordem.

    if sy-subrc is initial.

      select single * from vbap
        into @data(w_vbap)
          where vbeln eq @w_vbak-vbeln
        and matnr eq @material.

      select single * from vbkd
        into @data(w_vbkd)
          where vbeln eq @w_vbak-vbeln.

      select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
        into table @data(t_konv)
          where knumv eq @w_vbak-knumv
            and kschl in ('PR00', 'RB00')
            and kposn eq @w_vbap-posnr.

    endif.

    try .
        data(w_pr00) = t_konv[ kschl = 'PR00' ].
        data(w_rb00) = t_konv[ kschl = 'RB00' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    return-doc_simulacao = simulador.
    return-sequencia     = sequencia.
    return-auart         = w_vbak-auart.
    return-vbeln         = w_vbap-vbeln.
    return-posnn         = w_vbap-posnr.
    return-spart         = w_vbap-spart.

    return-zmeng         = cond #( when meins eq 'TO' then qtd * 1000 else qtd ).
    return-zieme         = cond #( when meins eq 'TO' then 'KG' else meins ).
    return-netpr         = vlr_venda.
    return-kmein         = meins.
    return-netwr         = vlr_total.
    return-charg         = w_vbap-charg.
    return-matnr         = w_vbap-matnr.
    return-matkl         = w_vbap-matkl.
    return-inco1         = w_vbkd-inco1.
    return-werks         = w_vbap-werks.
    return-kunnr         = w_vbak-kunnr.
    return-categoria     = cond #( when categoria is not initial then categoria else 'M' ).  "*CATEGORIA
    return-kurrf         = w_vbkd-kurrf.
    return-usnam         = sy-uname.
    return-data_atual    = sy-datum.
    return-hora_atual    = sy-uzeit.

    modify zsdt0090 from return.

    commit work.

  endmethod.

  method block_linhas.

    loop at it_troca_new assigning field-symbol(<troca>) where id eq id.
      select count(*)
        from mara
        where matnr eq <troca>-matnr
          and mtart eq 'ZFER'.

      <troca>-style = value #( (
                                     style = cond #( when sy-subrc is initial
                                                       then cl_gui_alv_grid=>mc_style_disabled
                                                       else cl_gui_alv_grid=>mc_style_enabled )
                                     fieldname = 'LOTE'
                             ) ).
    endloop.

  endmethod.

  method aplica_desc_abs.

    data diferenca type p decimals 6.
    data dif_fixa type p decimals 2.
    data dif_char type string.

    diferenca = i_diferenca.

    if i_zero is not initial.

*   "// criando uma tabela para REMOVER o ("Desconto/Acrescimo")
      call method zsdmf001_atuali_ov_simulador
        exporting
          i_vbeln     = i_vbeln
          i_posnr     = i_posnr
          i_matnr     = i_matnr
          i_diferenca = conv #( diferenca ).

*      DATA(T_DESC_ABS) = VALUE ZSDT0041_T( (
*                                            VBELN         = I_VBELN
*                                            POSNR         = I_POSNR
*                                            MATNR         = I_MATNR
*                                            DESC_ABSOLUTO = I_DIFERENCA
*                                       ) ).
**   "// função que aplica o desconto/acrescimo na OV
*      CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
*        TABLES
*          TI_ITENS_OV       = T_DESC_ABS
*        EXCEPTIONS
*          OV_NAO_ENCONTRADA = 1
*          OTHERS            = 2.
    else.

*   "// get do coeficiente o Iten da OV
      data(coeficiente_diferenca) = zcl_solicitacao_ov=>get_imposto(
        _direcao = 'O'
        _vbeln   = i_vbeln
        _posnr   = i_posnr
      ).
*   "// add 1 quando o valor estiver ZERADO
      coeficiente_diferenca = cond #( when coeficiente_diferenca is initial then 1 else coeficiente_diferenca ).

*   "// get do  valor Total
      select single netwr
        from zsdt0090
        into @data(total_ok)
        where vbelv eq @i_vbeln
          and posnv eq @i_posnr
          and categoria eq 'M'
          and estorno eq @abap_false.

*      IF P_UCOMM NE 'ESTORNAR'.
      if total_ok is initial.

*   "// get dos Descontos aplicado pelo usuario
        select sum( desc_absoluto )
          from zsdt0090
          into @data(desc_abs)
          where vbelv eq @i_vbeln
            and posnv eq @i_posnr
            and categoria eq 'O'
            and estorno eq @abap_false.
      endif.

*   "// aplicando o coeficiente na diferença
      multiply diferenca by coeficiente_diferenca.

      dif_char = diferenca.
      condense dif_char no-gaps.
      zcl_util_=>arredonda(  changing dife = dif_char ).
      diferenca = cond #( when diferenca < 0 then |{ dif_char }-| else dif_char ).
*   "// aplicando o coeficiente no desconto
      multiply desc_abs by coeficiente_diferenca.
*   "// somando o desconta na diferença
      add desc_abs to diferenca.

*   "// criando uma tabela para aplicar a diferença("Desconto/Acrescimo")
      call method zsdmf001_atuali_ov_simulador
        exporting
          i_vbeln     = i_vbeln
          i_posnr     = i_posnr
          i_matnr     = i_matnr
          i_diferenca = conv #( diferenca ).
*      T_DESC_ABS = VALUE ZSDT0041_T( (
*                                        VBELN         = I_VBELN
*                                        POSNR         = I_POSNR
*                                        MATNR         = I_MATNR
*                                        DESC_ABSOLUTO = DIFERENCA
*                                   ) ).
**   "// função que aplica o desconto/acrescimo na OV
*      CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
*        TABLES
*          TI_ITENS_OV       = T_DESC_ABS
*        EXCEPTIONS
*          OV_NAO_ENCONTRADA = 1
*          OTHERS            = 2.
    endif.

  endmethod.

  method get_kbetr.

    select single knumv
      from vbak
      into @data(_knumv)
      where vbeln eq @i_vbeln.

* ---> S4 Migration - 10/07/2023 - DG
*    SELECT SINGLE *
*      FROM KONV
*      INTO @DATA(w_konv)
*      WHERE knumv EQ @_knumv
*        AND kposn EQ @i_posnr
*        AND kschl EQ 'PR00'.

    data: w_konv type konv.

    select single *
      from v_konv
      into @data(w_konv_aux)
      where knumv eq @_knumv
        and kposn eq @i_posnr
        and kschl eq 'PR00'.

    move-corresponding w_konv_aux to w_konv.
* <--- S4 Migration - 10/07/2023 - DG


    _konv = w_konv.
    if i_add_imposto eq abap_false.
      _konv-kbetr = zcl_util_=>check_desc_abs(
        _vbeln = i_vbeln
        _posnr = i_posnr
        des    = conv #( w_konv-kbetr )
        dir    = abap_true
      ).
    endif.

  endmethod.

  method get_vbap.
    select single *
      from vbap
      into e_vbap
      where vbeln eq i_vbeln
        and matnr eq i_matnr.

    select count(*)
      from vbep
        where vbeln eq e_vbap-vbeln
          and posnr eq e_vbap-posnr
          and lifsp eq '12'.

    if sy-subrc is initial.
      clear e_vbap.
    endif.

  endmethod.

  method delete_ov.

    data(header_inx) = value bapisdh1x( updateflag = 'D' ).
    "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
    call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      exporting
        salesdocument    = i_vbeln
        order_header_inx = header_inx
      tables
        return           = tl_return.

    if not line_exists( tl_return[ type = 'E' ] ).
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.

      call method zcl_manutencao_insumos=>cancel_aprovadores_embarque
        exporting
          i_vbelv = i_vbeln.

    else.
      append lines of tl_return to t_return.
    endif.

  endmethod.

  method deleta_iten.

    data(order_header_inx) = value bapisdh1x( updateflag = 'U' ).
    data(order_item_in) = value wiso_t_sditm( (
                                                itm_number = i_posnr
                                                material   = i_matnr
                                            ) ).

    data(order_item_inx) = value wiso_t_sditmx( (
                                                  itm_number = i_posnr
                                                  material   = i_matnr
                                                  updateflag = 'D'
                                              ) ).
    "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
    call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      exporting
        salesdocument    = i_vbeln
        order_header_inx = order_header_inx
      tables
        order_item_in    = order_item_in
        order_item_inx   = order_item_inx
        return           = tl_return.

    if not line_exists( tl_return[ type = 'E' ] ).
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    else.
      append lines of tl_return to t_return.
    endif.

  endmethod.

  method processa_desc.

    data: valor_real type p decimals 5,
          valor_vbap type p decimals 5,
          diferenca  type p decimals 5.

*     "// get do Iten da OV
    data(_vbap)  = zcl_util_=>get_vbap( i_vbeln = i_vbeln i_matnr = i_matnr ).
*     "// remove do desconto para reprocessa-lo
    if _vbap is not initial.
      zcl_util_=>aplica_desc_abs(
        i_vbeln     = _vbap-vbeln
        i_posnr     = _vbap-posnr
        i_matnr     = _vbap-matnr
        i_diferenca = 0
        i_zero      = abap_true
      ).
    endif.
*     "// get do Iten da OV
    _vbap  = zcl_util_=>get_vbap( i_vbeln = i_vbeln i_matnr = i_matnr ).
    check _vbap is not initial.
*     "// get do preço da OV/Iten
    data(_kbetr) = zcl_util_=>get_kbetr( i_vbeln = _vbap-vbeln i_posnr = _vbap-posnr )-kbetr.
*     "// get da unidade de Medida do Preço
    data(_kmein) = zcl_util_=>get_kbetr( i_vbeln = _vbap-vbeln i_posnr = _vbap-posnr )-kmein.
*     "// multiplicação da quantidade(KWMENG) com Preço(_KBETR) Convertendo a quantidade  na unidade de medida referente ao do Preço (_KMEIN)
    valor_real = cond #( when i_vlr_real is initial then
                ( cond #(
                          when _kmein eq _vbap-vrkme
                                then _vbap-kwmeng
                                else switch #( _vbap-vrkme
                                                when 'TO'
                                                  then ( _vbap-kwmeng * 1000 )
                                                when 'KG'
                                                  then ( _vbap-kwmeng / 1000 )
                                           )
                        ) * _kbetr
                 ) else i_vlr_real ).
*     "// Soma valor Liquido(NETWR) com Imposto do iten(MWSBP)
    valor_vbap = _vbap-netwr + _vbap-mwsbp.
*     "// Subtração entre Valor da VBAP com Valor real
*    BREAK WBARBOSA.
    diferenca = valor_real - valor_vbap.
*    DIFERENCA = FLOOR( DIFERENCA ).
*    floor(DIFERENCA) = VALOR_REAL - VALOR_VBAP.
*     "// aplica a diferença no OV
    zcl_util_=>aplica_desc_abs(
      i_vbeln     = _vbap-vbeln
      i_posnr     = _vbap-posnr
      i_matnr     = _vbap-matnr
      i_diferenca = conv #( diferenca )
    ).

  endmethod.

  method modify_vlrtot.

*    SELECT *
*      FROM ZSDT0090
*      INTO TABLE @DATA(_0090)
*      FOR ALL ENTRIES IN @T_0090
*      WHERE ( VBELV EQ @T_0090-VBELN
*        OR VBELN EQ @T_0090-VBELN )
*        AND DOC_SIMULACAO EQ @T_0090-DOC_SIMULACAO
*        AND ESTORNO EQ @ABAP_FALSE.

* "// Soma os Descontos
    desconto = reduce kbetr( init x type kbetr for l_0090 in t_0090
                                  where ( categoria eq 'O' and flag eq abap_true and flag1 eq abap_false )
                               next x = x + l_0090-desc_absoluto
                           ).
* "// Inverte o sinal
    multiply desconto by -1.
* "// Get total do Simulador
    select single vlrtot
      from zsdt0040
        into ztotal
      where doc_simulacao = simulador.
* "// Soma com o desconto agredado da ZSDT0090
    add desconto to ztotal.
* "// atualiza o Valor total da 40
    update zsdt0040
      set vlrtot = ztotal
        where doc_simulacao eq simulador.

    loop at t_0090 into data(w_0090).
*         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = w_0090-doc_simulacao
                                              i_vbeln  = w_0090-vbeln
                                              i_seq    = w_0090-sequencia
                                              i_tipo   = 'EST'
                                              ).
    endloop.

  endmethod.

  method back_qtd.
*     "// Verifica se Existe Alteração de Quantidade
    select single *
     from zsdt0090
     into @data(w_0090a)
     where vbelv = @i_vbeln
      and categoria eq 'A'
      and estorno eq @abap_false.

    if sy-subrc is initial.
*       "// Soma a Quantidade alterada
      select sum( zmengv )
       from zsdt0090
       into @data(qtd_a)
       where vbelv = @w_0090a-vbelv
        and categoria eq 'A'
        and estorno eq @abap_false.

      data(t_ova) =
      value zsds015_t(
                        (
                          vbeln = w_0090a-vbelv
                          posnr = w_0090a-posnv
                          matnr = w_0090a-matnrv
                          zmeng = qtd_a
                        )
                     ).

      zcl_util_=>indicator( |Desfazendo as Alterações de Quantidade!| ).
      free tl_return.
      call function 'ZSDMF001_ATUALI_OV_SIM'
        exporting
          i_soma    = abap_false
          i_acao    = 'R_ALTERA'
        tables
          it_ov     = t_ova
          te_return = tl_return.

      loop at t_ova into data(w_ov).
        zcl_util_=>processa_desc( i_vbeln = w_ov-vbeln i_matnr = w_ov-matnr ).
      endloop.

      zcl_util_=>indicator( |Revertendo Hedge!| ).
*         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = w_0090a-doc_simulacao
                                              i_vbeln  = w_0090a-vbeln
                                              i_seq    = w_0090a-sequencia
                                              i_tipo   = 'EST'
                                              ).
    endif.
  endmethod.

  method estorna_frete.

    update zsdt0090
      set estorno      = abap_true
          usnam_e      = sy-uname
          data_atual_e = sy-datum
          hora_atual_e = sy-uzeit
        where vbelv eq w_0090-vbelv
          and posnv eq w_0090-posnv
          and sequencia eq w_0090-sequencia
          and estorno eq abap_false.
*   "// Dispara o Hedge para a OV
    zcl_webservice_tx_curva=>hedge_insumos(
      i_numero = w_0090-doc_simulacao
      i_vbeln  = w_0090-vbelv
      i_seq    = w_0090-sequencia
      i_tipo   = 'EST'
    ).
  endmethod.

  method get_87.
    select single * from zsdt0087 into return where matkl eq matkl and tpsim eq tpsim and inco1 eq inco1.
  endmethod.

  method arredonda.
    data: len1  type i,
          ofst1 type i,
          ofst2 type i,
          val   type c,
          str_c type string.

    len1 = strlen( dife ).
    translate dife to upper case.

    do.
      if ofst1 = len1.
        exit.
      endif.

      if val is not initial.
        if ofst2 eq 2.
          exit.
        endif.
        add 1 to ofst2.
      endif.

      if dife+ofst1(1) eq '.'.
        val = abap_true.
      endif.

      str_c = |{ str_c }{ dife+ofst1(1) }|.

      add 1 to ofst1.

    enddo.

    dife = str_c.

  endmethod.

  method check_block.

    loop at table into data(w_table) where check eq abap_true.
      select count(*)
        from vbep
        where vbeln eq w_table-vbeln
        and posnr eq w_table-posnr
        and lifsp eq '12'.

      subrc = sy-subrc.

      if subrc is initial.
        message |O.V. nº: { w_table-vbeln }, item: { w_table-posnr } Bloqueado!| type 'S' display like 'E'.
        exit.
      endif.
    endloop.

  endmethod.

  method get_qtd_embarcado.
    select sum( qte_sol )
      from zsdt0082
      into qtd_emb
        where seq    eq 1
          and vbeln  eq i_vbeln
          and posnr  eq i_posnr
          and status eq 1.
  endmethod.

  method zsdmf001_atuali_ov_simulador.
*   "// criando uma tabela para REMOVER o ("Desconto/Acrescimo")
    data(t_desc_abs) = value zsdt0041_t( (
                                          vbeln         = i_vbeln
                                          posnr         = i_posnr
                                          matnr         = i_matnr
                                          desc_absoluto = i_diferenca
                                     ) ).
*   "// função que aplica o desconto/acrescimo na OV
    call function 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
      tables
        ti_itens_ov       = t_desc_abs
      exceptions
        ov_nao_encontrada = 1
        others            = 2.

  endmethod.

  method processa_40.

    data soma type netwr.
    data diferenca type kbetr.
    data vlr type dzwert.
    data it_vbap type table of vbap.

    select single vlrtot
      from zsdt0040
      into @data(vl_total)
        where doc_simulacao eq @simulador.

    check sy-subrc is initial.

    select *
      from zsdt0090
      into table @data(it_90)
        where doc_simulacao eq @simulador
          and estorno eq @abap_false.

    check sy-subrc is initial.

    select a~*
        from vbap as a
        inner join vbep as e on e~vbeln eq a~vbeln
                            and e~posnr eq a~posnr
        into table @it_vbap
          where e~lifsp ne '12'
          and a~vbeln in ( select vbeln from zsdt0041 where doc_simulacao = @simulador ).

    select a~*
      from vbap as a
      inner join vbep as e on e~vbeln eq a~vbeln
                          and e~posnr eq a~posnr
      appending table @it_vbap
      for all entries in @it_90
        where ( a~vbeln eq @it_90-vbelv
             or a~vbeln eq @it_90-vbeln
             or a~vbeln eq @i_vbeln     )
            and e~lifsp ne '12'.

    check it_vbap is not initial.

    sort it_vbap by vbeln posnr.
    delete adjacent duplicates from it_vbap comparing vbeln posnr.

    select *
      from zsdt0090
      into table @data(t_90)
*      FOR ALL ENTRIES IN @IT_VBAP
        where doc_simulacao eq @simulador
        and categoria eq 'O'
        and flag eq @abap_false
        and estorno eq @abap_false.

    select single knumv
      from vbak
      into @data(vl_knumv)
      where vbeln eq @i_vbeln.

    select single posnr
    from vbap
    into @data(vl_posnr)
    where vbeln eq @i_vbeln
      and matnr eq @i_matnr.

    select single kbetr
      from v_konv " ---> S4 Migration - 10/07/2023 - DG
      into @data(vl_kbetr)
      where knumv eq @vl_knumv
        and kposn eq @vl_posnr
        and kschl eq 'RB00'.

    data(vl_netwr) =  reduce netwr( init x type netwr for w_vbap in it_vbap
                             next x = x + w_vbap-netwr
                         ).
    data(vl_mwsbp) =  reduce mwsbp( init y type mwsbp for w_vbap in it_vbap
                             next y = y + w_vbap-mwsbp
                         ).
    data(vl_desc)  =  reduce kbetr( init z type kbetr for w_90 in t_90
                             next z = z + w_90-desc_absoluto
                         ).
*   "// get nas auterações de quantidades, encerramentos e redistribuições do Simulador
*   "// Auteração de Quantidade
    call method get_vlr_90
      exporting
        simulador = simulador
        categoria = 'A'
      receiving
        vlr       = data(vlr_qtd).
*   "// Encerramento
    call method get_vlr_90
      exporting
        simulador = simulador
        categoria = 'E'
      receiving
        vlr       = data(vlr_enc).

*   "// Redistribuição
    call method get_vlr_90
      exporting
        simulador = simulador
        categoria = 'R'
      receiving
        vlr       = data(vlr_red).

**   "// Devolução
    call method get_vlr_90
      exporting
        simulador = simulador
        categoria = 'Y'
      receiving
        vlr       = data(vlr_dev).

**   "// Retorno da Devolução
    call method get_vlr_90
      exporting
        simulador = simulador
        categoria = 'K'
      receiving
        vlr       = data(vlr_rde).

*   "// Complemento
    call method get_vlr_90
      exporting
        simulador = simulador
        categoria = 'W'
      receiving
        vlr       = data(vlr_com).

    add vlr_qtd to vlr. " Alteração de Quantidade
    add vlr_enc to vlr. " Encerramento
    add vlr_red to vlr. " Redistribuição
    add vlr_dev to vlr. " Devolução
    add vlr_com to vlr. " Complemento
    add vlr_rde to vlr. " Retorno da Devolução

    add vl_netwr to soma.
    add vl_mwsbp to soma.

    subtract vlr from soma.

    add vl_desc to vl_total.

    if vl_total ne soma.

      diferenca = vl_total - soma.
      add vl_kbetr to diferenca.

      call method zsdmf001_atuali_ov_simulador
        exporting
          i_vbeln     = i_vbeln
          i_posnr     = vl_posnr
          i_matnr     = i_matnr
          i_diferenca = conv #( diferenca ).
    endif.

  endmethod.

  method indicator.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = text.
  endmethod.

  method loop_40.
*    BREAK WBARBOSA.
    at_contador = 0.
    do.
      if at_contador eq qtd.
        exit.
      endif.
      zcl_util_=>indicator( |Processando 40 { at_contador }| ).
      call method zcl_util_=>processa_40
        exporting
          simulador = simulador
          i_vbeln   = i_vbeln
          i_matnr   = i_matnr.

      add 1 to at_contador.
    enddo.
  endmethod.

  method get_rb00.

    select single knumv
      from vbak
      into @data(_knumv)
      where vbeln eq @i_vbeln.

* ---> S4 Migration - 10/07/2023 - DG
*    SELECT SINGLE *
*      FROM KONV
*      INTO @DATA(w_konv)
*      WHERE knumv EQ @_knumv
*        AND kposn EQ @i_posnr
*        AND kschl EQ 'RB00'.

    data: w_konv type konv.

    select single *
      from v_konv
      into @data(w_konv_aux)
      where knumv eq @_knumv
        and kposn eq @i_posnr
        and kschl eq 'RB00'.

    move-corresponding w_konv_aux to w_konv.
* <--- S4 Migration - 10/07/2023 - DG



    _konv = w_konv.
    if i_add_imposto eq abap_false.
      _konv-kbetr = zcl_util_=>check_desc_abs(
        _vbeln = i_vbeln
        _posnr = i_posnr
        des    = conv #( w_konv-kbetr )
        dir    = abap_true
      ).
    endif.

  endmethod.

  method get_desconto.
*   "// get dos Descontos aplicado pelo usuario
    select sum( desc_absoluto )
      from zsdt0090
      into desc_abs
      where vbelv eq i_vbeln
        and posnv eq i_posnr
        and categoria eq 'O'
        and estorno eq abap_false.

    if i_add_imposto is not initial.
*   "// get do coeficiente o Iten da OV
      data(coeficiente_diferenca) = zcl_solicitacao_ov=>get_imposto(
        _direcao = 'O'
        _vbeln   = i_vbeln
        _posnr   = i_posnr
      ).
*   "// add 1 quando o valor estiver ZERADO
      coeficiente_diferenca = cond #( when coeficiente_diferenca is initial then 1 else coeficiente_diferenca ).
*   "// aplicando o coeficiente no desconto
      multiply desc_abs by coeficiente_diferenca.
    endif.
  endmethod.

  method get_aut_qtd.

    data(vlr_aux) = vlr_qtd.

    select *
      from zsdt0090
      into table @data(t_90)
      where doc_simulacao eq @simulador
      and categoria eq 'A'
      and estorno eq @abap_false.

    loop at t_90 into data(w_90).
      vlr_aux = cond #( when w_90-ziemev eq 'KG' then ( w_90-zmengv / 1000 ) * w_90-netprv else w_90-zmengv * w_90-netprv ).
      add vlr_aux to vlr_qtd.
    endloop.

  endmethod.

  method get_encerramento.

    data(vlr_aux) = vlr_qtd.

    select *
      from zsdt0090
      into table @data(t_90)
      where doc_simulacao eq @simulador
      and categoria eq 'E'
      and estorno eq @abap_false.

    loop at t_90 into data(w_90).
      vlr_aux = cond #( when w_90-ziemev eq 'KG' then ( w_90-zmengv / 1000 ) * w_90-netprv else w_90-zmengv * w_90-netprv ).
      add vlr_aux to vlr_qtd.
    endloop.

  endmethod.

  method get_vlr_90.

    data: vlr_aux type dzwert,
          vlr_old type dzwert,
          vlr_new type dzwert.

    select *
      from zsdt0090
      into table @data(t_90)
      where doc_simulacao eq @simulador
        and categoria     eq @categoria
        and estorno       eq @abap_false.

    loop at t_90 into data(w_90).

*          "// Unidade de Medida da Quantidade W_90-ZIEMEV
*          "// Unidade de Medida do Preço W_90-KMEINV

*      CASE CATEGORIA.
*        WHEN 'R'.
*          VLR_OLD = COND #( WHEN W_90-ZIEMEV EQ 'KG' THEN ( W_90-ZMENGV / 1000 ) * W_90-NETPRV ELSE W_90-ZMENGV * W_90-NETPRV ).
*          VLR_NEW = COND #( WHEN W_90-ZIEME  EQ 'KG' THEN ( W_90-ZMENG  / 1000 ) * W_90-NETPR  ELSE W_90-ZMENG  * W_90-NETPR  ).
*          VLR_AUX = VLR_OLD + VLR_NEW.
*        WHEN OTHERS.
*          VLR_AUX = COND #( WHEN W_90-ZIEMEV EQ 'KG' THEN ( W_90-ZMENGV / 1000 ) * W_90-NETPRV ELSE W_90-ZMENGV * W_90-NETPRV ).
*      ENDCASE.

      case categoria.
        when 'R'.

          if w_90-ziemev eq w_90-kmeinv.
            vlr_old = w_90-zmengv * w_90-netprv.
          else.
            vlr_old = cond #( when w_90-ziemev eq 'TO' then ( w_90-zmengv * 1000 ) * w_90-netprv
                                                       else ( w_90-zmengv / 1000 ) * w_90-netprv ).
          endif.

          if w_90-ziemev eq w_90-kmeinv.
            vlr_new = w_90-zmeng  * w_90-netpr.
          else.
            vlr_new = cond #( when w_90-zieme  eq 'TO' then ( w_90-zmeng  * 1000 ) * w_90-netpr
                                                       else ( w_90-zmeng  / 1000 ) * w_90-netpr  ).
          endif.

          vlr_aux = vlr_old + vlr_new.

        when 'A'.
          if w_90-ziemev eq w_90-kmeinv.
            vlr_aux =  w_90-zmengv * w_90-netprv.
          else.
            vlr_aux = switch #( w_90-kmeinv when 'TO' then ( w_90-zmengv / 1000 ) * w_90-netprv
                                                      else ( w_90-zmengv * 1000 ) * w_90-netprv ).
          endif.
        when 'Y'.
          w_90-zmengv = abs( w_90-zmeng ).
          if w_90-ziemev eq w_90-kmeinv.
            vlr_aux =  w_90-zmengv * w_90-netprv.
          else.
            vlr_aux = switch #( w_90-kmeinv when 'TO' then ( w_90-zmengv / 1000 ) * w_90-netprv
                                                      else ( w_90-zmengv * 1000 ) * w_90-netprv ).
          endif.

        when 'K'.

          w_90-zmengv = abs( w_90-zmengv ).

          if w_90-ziemev eq w_90-kmeinv.
            vlr_aux =  w_90-zmengv * w_90-netprv.
          else.
            vlr_aux = switch #( w_90-kmeinv when 'TO'
                                    then ( w_90-zmengv / 1000 ) * w_90-netprv
                                    else ( w_90-zmengv * 1000 ) * w_90-netprv ).
          endif.


        when others.

*          vlr_aux = cond #( when w_90-ziemev eq 'KG' then ( w_90-zmengv / 1000 ) * w_90-netprv else w_90-zmengv * w_90-netprv ).

          if w_90-ziemev eq w_90-kmeinv.
            vlr_aux =  w_90-zmengv * w_90-netprv.
          else.
            vlr_aux = switch #( w_90-kmeinv when 'TO'
                                    then ( w_90-zmengv / 1000 ) * w_90-netprv
                                    else ( w_90-zmengv * 1000 ) * w_90-netprv ).
          endif.

      endcase.

      add vlr_aux to vlr.

    endloop.

  endmethod.

  method disparo_0116.

    data: seq type numc10.

    select single *
      from zsdt0090
      into @data(i_0090)
      where vbeln eq @i_vbeln
        and posnn eq @i_posnr.

    check sy-subrc is initial.

    if ( i_0090-categoria eq ' ' ) or
       ( i_0090-categoria eq 'M' ) or
       ( i_0090-categoria eq 'D' and i_0090-kunnr eq i_0090-kunnrv ).

*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Inicio
      perform insere_zsdt0116 using i_0090-vbeln i_0090-vbelv i_0090-posnn.

*      SELECT SINGLE *
*        FROM zsdt0116
*        INTO @DATA(wa_zsdt0116)
*        WHERE vbeln   EQ @i_0090-vbelv
*          AND status  EQ @abap_false
*          AND status_workflow IN ( ' ', 'A' ).
*
*      IF sy-subrc IS INITIAL.
*
*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            nr_range_nr = '01'
*            object      = 'ZSEQ_0116_'
*          IMPORTING
*            number      = seq.
**
*        IF seq IS INITIAL.
*          ROLLBACK WORK.
*          MESSAGE 'Objeto numeração ZSEQ_0116_ não configurado!' TYPE 'S'.
*          RETURN.
*        ENDIF.
*
*        DATA(wa_0116) =
*        VALUE zsdt0116(
*                        seq       = seq
*                        vbeln     =  i_0090-vbeln
*                        posnr     =  i_0090-posnn
*                        user_apv  =  wa_zsdt0116-user_apv
*                        dt_apv    =  wa_zsdt0116-dt_apv
*                        hr_apv    =  wa_zsdt0116-hr_apv
*                      ).
*
*        INSERT INTO zsdt0116 VALUES wa_0116.
*        IF sy-subrc IS INITIAL.
*          COMMIT WORK.
*          APPEND VALUE #(
*                          type = 'W'
*                          message = |OV. { i_0090-vbeln } foi Incluida na tabela ZSDT0116.|
*                        ) TO tl_return[].
*        ENDIF.
*      ENDIF.
*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Fim
    endif.

  endmethod.

endclass.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.
  select-options: s_vkorg for zsdt0040-vkorg obligatory,
                  s_vkbur for zsdt0040-vkbur obligatory,
                  s_vbeln for zsdt0041-vbeln,
                  s_docsi for zsdt0040-doc_simulacao,
                  s_kunnr for zsdt0040-kunnr,
                  s_waerk for zsdt0040-waerk,
                  s_cultu for zsdt0040-cultura,
                  s_safra for zsdt0040-safra,
                  s_spart for zsdt0041-spart,
                  s_auart for zsdt0041-auart,
                  s_data  for zsdt0040-erdat no-extension,
                  s_matnr for zsdt0041-matnr no intervals.
selection-screen: end of block b1.

parameters: p_extcal type c no-display. "Parameter para controle da chamada do programa via submit pelo progrma ZSDR0038 "FF #145609

"FF #145609 - inicio

initialization.

  data: lv_qte_sol  type vbap-kwmeng,
        lv_kunnr    type vbak-kunnr,
        lv_iti_ov   type vbap-route,                        "FF #145609
        lv_iti_rote type trolz-route.                       "FF #145609

  import _saida-qte_sol to lv_qte_sol from memory id 'memory_qte_sol'. "Export feito no programa ZSDR0038
  import _saida-kunnr   to lv_kunnr   from memory id 'memory_kunnr'. "Export feito no programa ZSDR0038

  import _saida-iti_ov   to lv_iti_ov   from memory id 'memory_iti_ov'. "Export feito no programa ZSDR0038
  import _saida-iti_rote to lv_iti_rote from memory id 'memory_iti_rote'. "Export feito no programa ZSDR0038



  if lv_qte_sol is not initial.
    p_extcal = abap_true.
  else.
    clear p_extcal.
  endif.
  "FF #145609 - fim

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
start-of-selection.

  " 17.04.2024 - RAMON - 96174 -->
  clear gv_ucomm.
  " 17.04.2024 - RAMON - 96174 --<

  wa_stable = value #( row = abap_true col = abap_true ).

  perform:
          " 10.09.2024 - RAMON - 97513 -->
          f_seleciona_dados using space, " Form seleciona dados
                " 10.09.2024 - RAMON - 97513 --<
          f_saida, " Form de saida
          f_imprime_dados.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados using " 10.09.2024 - RAMON - 97513 -->
                             p_refresh type c.
  " 10.09.2024 - RAMON - 97513 --<

  refresh : t_usermd,
            it_zsdt0040,
            it_zsdt0041,
            it_zsdt0090,
            it_vbap,
            it_vbak,
            it_vbkd,
            it_vbfa,
            it_mara,
            it_vbep.

  " 10.09.2024 - RAMON - 97513 -->
  select count(*) from tvarvc
  where name = 'ZSDT0087_OBJ_AUTH'.

  if sy-dbcnt > 0.
    gv_stvarv_ativa = abap_true.
  endif.
  " 10.09.2024 - RAMON - 97513 --<

  select *
     from  setleaf
     into table t_set
     where setclass      = '0000'
  and   setname        = 'MAGGI_ZSDT0044_02'.

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


  if gv_stvarv_ativa is not initial." 10.09.2024 - RAMON - 97513

    " 27.08.2024 - RAMON - dev autorização -->
    perform f_check_autori_empresa using p_refresh.
    " 27.08.2024 - RAMON - dev autorização --<

  else." 10.09.2024 - RAMON - 97513

    " 05.09.2024 - 101482 - DESCOMENTADO, PQ A VALIDAÇÃO NOVA AINDA NAO FOI TESTADA EM QAS -->
    " 30.11.2023 - RAMON/SAMUEL - Novo desenvolvimento, autorização por obj.autorização -->>
    " Usuários com Acesso a todas as funções do Cockipt
    call function 'G_SET_GET_ALL_VALUES'
      exporting
        class         = '0000'
        setnr         = 'MAGGI_ZSDT0087'
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

    read table t_usermd with key from = sy-uname.
    v_botoes = 'N'.
    if sy-subrc eq 0.
      v_botoes = 'S'.
    endif.
    " 30.11.2023 - RAMON/SAMUEL - Novo desenvolvimento, autorização por obj.autorização --<<
    " 05.09.2024 - 101482 - DESCOMENTADO, PQ A VALIDAÇÃO NOVA AINDA NAO FOI TESTADA EM QAS --<

  endif." 10.09.2024 - RAMON - 97513

  if not s_data is initial.
    data where type c length 255.

    if not s_data-low is initial and not s_data-high is initial.
      where = | ERDAT BETWEEN '{ s_data-low }' AND '{ s_data-high }'|.
    endif.

    if not s_data-low is initial and s_data-high is initial.
      where = | ERDAT EQ '{ s_data-low }'|.
    endif.

    if s_data-low is initial and not s_data-high is initial.
      where = | ERDAT EQ '{ s_data-high }'|.
    endif.

  endif.

  "Simulador de Vendas - dados de cabeçalho
  select *
    from zsdt0040
    into table it_zsdt0040
    where vkorg           in s_vkorg
     and  vkbur           in s_vkbur
     and  doc_simulacao   in s_docsi
     and  kunnr           in s_kunnr
     and  waerk           in s_waerk
     and  cultura         in s_cultu
     and  safra           in s_safra
  and  (where).


**    " 27.08.2024 - RAMON - dev autorização -->
**    PERFORM f_check_autori_empresa USING p_refresh.
**    " 27.08.2024 - RAMON - dev autorização --<

  check it_zsdt0040[] is not initial.

  "Simulador de Vendas - dados de itens
  select *
    from zsdt0041
    into table it_zsdt0041
    for all entries in it_zsdt0040
    where doc_simulacao = it_zsdt0040-doc_simulacao
     and  spart in s_spart
     and  auart in s_auart
     and  vbeln in s_vbeln
  and  vbeln ne ''.

  "Simulador de Vendas – Controle de Transferencias de OV
  select *
    from zsdt0090
    into table it_zsdt0090
    for all entries in it_zsdt0040
    where doc_simulacao = it_zsdt0040-doc_simulacao
     and  vbeln   in s_vbeln
     and  estorno ne 'X'
  and  auart   in s_auart.

  if it_zsdt0041[] is not initial.
    select vbeln posnr matnr arktx werks charg kwmeng vrkme netpr netwr lgort umziz route mwsbp
      from vbap
      into corresponding fields of table it_vbap
      for all entries in it_zsdt0041
    where vbeln eq it_zsdt0041-vbeln.

  endif.

  if it_zsdt0090[] is not initial.
    select vbeln posnr matnr arktx werks charg kwmeng vrkme netpr netwr lgort umziz route mwsbp
      from vbap
      appending corresponding fields of table it_vbap
      for all entries in it_zsdt0090
    where vbeln eq it_zsdt0090-vbeln.
  endif.

  if it_vbap[] is not initial.

    select *
      from marm
      into table it_marm
      for all entries in it_vbap
    where matnr = it_vbap-matnr.

    select matnr matkl meins
      from mara
      into table it_mara
      for all entries in it_vbap
    where matnr = it_vbap-matnr.

    select vbeln auart spart kunnr knumv zpesagem vkgrp
      from vbak
      into table it_vbak
      for all entries in it_vbap
    where vbeln eq it_vbap-vbeln.

    select * from vbpa
      into corresponding fields of table it_vbpa[]
      for all entries in it_vbak
      where vbeln eq it_vbak-vbeln
    and parvw eq 'PC'.

    select lifnr name1
      from lfa1
      into corresponding fields of table it_lfa1
      for all entries in it_vbpa
    where lifnr eq it_vbpa-lifnr.

    select *
      into corresponding fields of table @gt_konv
      from v_konv
      for all entries in @it_vbak
    where knumv = @it_vbak-knumv.

    select kunnr name1
       from kna1
       into table it_kna1
       for all entries in it_vbak
    where kunnr eq it_vbak-kunnr.

    select vbeln posnr inco1
      from vbkd
      into table it_vbkd
      for all entries in it_vbap
    where vbeln eq it_vbap-vbeln.

    select vbelv posnv rfmng vbeln posnn meins
      from vbfa
      into table it_vbfa
      for all entries in it_vbap
      where vbelv = it_vbap-vbeln
      and   posnv = it_vbap-posnr
      and   vbtyp_n = 'J'
      and   vbtyp_v = 'C'
    and   bwart ne space.  "<< RIM-SKM-IR113139-03.10.22

    select *
      from vbep
      into table it_vbep
      for all entries in it_vbap
      where vbeln = it_vbap-vbeln
    and   posnr = it_vbap-posnr.

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

  data: wl_saida type ty_saida.
  data: qtd_convertida type vbap-kwmeng.

  sort: it_zsdt0041 by doc_simulacao,
        it_zsdt0090 by doc_simulacao,
        it_vbap     by vbeln matnr,
        it_vbkd     by vbeln posnr,
        it_vbak     by vbeln,
        it_vbfa     by vbelv posnv,
        it_kna1     by kunnr,
        it_mara     by matnr,
        it_marm     by matnr,
" RMNI - CS0971620 - Correção de dump read table  - Início
        it_vbpa     by vbeln,
" RMNI - CS0971620 - Correção de dump read table  - Fim
        it_vbep     by vbeln posnr.




  refresh it_saida.
  clear wa_saida.
  loop at it_zsdt0040 into wa_zsdt0040.
    "Nº Simulação
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_zsdt0040-doc_simulacao
      importing
        output = wa_saida-doc_simulacao2.

    wa_saida-doc_simulacao     = wa_zsdt0040-doc_simulacao.
    wa_saida-vkorg             = wa_zsdt0040-vkorg.         "Organização de Vendas
    wa_saida-vkbur             = wa_zsdt0040-vkbur.         "Escritorio de Vendas
    wa_saida-vtweg             = wa_zsdt0040-vtweg.         "Canal de Distribuição
*    WA_SAIDA-VENDEDOR          = WA_ZSDT0040-VENDEDOR.      "Vendedor
    wa_saida-waerk             = wa_zsdt0040-waerk.         "Moeda
    wa_saida-cultura           = wa_zsdt0040-cultura.       "Cultura
    wa_saida-safra             = wa_zsdt0040-safra.         "Safra
    wa_saida-tpsim             = wa_zsdt0040-tpsim.         "Condição Pagamento

    " 19.06.2023 - RAMON -- 97513 -->
    wa_saida-meio_pago = wa_zsdt0040-meio_pago.
    " 19.06.2023 - RAMON -- 97513 --<
    "Simulador de Vendas - dados de itens
    loop at it_zsdt0041 into wa_zsdt0041 where doc_simulacao = wa_zsdt0040-doc_simulacao.
      "Nº OV
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_zsdt0041-vbeln
        importing
          output = wa_saida-vbeln2.

      wa_saida-vbeln  = wa_zsdt0041-vbeln.
      wa_saida-itens  = wa_zsdt0041-posnr.

      read table it_vbak into wa_vbak with key vbeln = wa_zsdt0041-vbeln binary search.
      case wa_vbak-auart.

        when 'ZCOP' or 'ZROB' or 'ZREB' or 'ZRPF'.
          continue.
        when others.
          if v_botoes = 'N'. " Acesso somente para listar as OV do Tipo ZREM / ZTRI e ao Botão “GERAR Remessa/Fatura/ZTRI
            if  wa_vbak-auart ne 'ZREM'
            and wa_vbak-auart ne 'ZFTE'
            and wa_vbak-auart ne 'ZRFU'
            and wa_vbak-auart ne 'ZOFE'.
              continue.
            endif.
          endif.
      endcase.

      "Emissor Ordem
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_vbak-kunnr
        importing
          output = wa_saida-kunnr2.

      wa_saida-kunnr             = wa_vbak-kunnr.

      read table it_kna1 into wa_kna1 with key kunnr = wa_vbak-kunnr  binary search.
      wa_saida-name1             = wa_kna1-name1.             "Descrição Emissor

      wa_saida-auart             = wa_vbak-auart.             "Tipo de OV
      wa_saida-spart             = wa_vbak-spart.             "Setor de Atividades

      wa_saida-vendedor          = wa_vbak-vkgrp.             "Vendedor


      loop at it_vbap into wa_vbap where vbeln = wa_zsdt0041-vbeln.
*                                   AND   MATNR = WA_ZSDT0041-MATNR. ** removido pq impedia a exibição de item criados fora do simulador

        wa_saida-matkl = it_mara[ matnr = wa_vbap-matnr ]-matkl.

        read table it_vbep with key vbeln = wa_vbap-vbeln
                                    posnr = wa_vbap-posnr.
        if sy-subrc is initial.
          if it_vbep-lifsp is initial.
            wa_saida-icon  = '@07@'.
          else.
            wa_saida-icon  = '@06@'.
          endif.
        endif.

        clear wl_saida.
        read table it_saida into wl_saida with key vbeln = wa_vbap-vbeln
                                                   posnr = wa_vbap-posnr.
        if sy-subrc is not initial.
          wa_saida-posnr             = wa_vbap-posnr.             "Item
          "Material
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = wa_vbap-matnr
            importing
              output = wa_saida-matnr2.

          wa_saida-matnr          = wa_vbap-matnr.
          wa_saida-arktx          = wa_vbap-arktx.             "Descrição Material
          wa_saida-werks          = wa_vbap-werks.             "Centro
          wa_saida-lgort          = wa_vbap-lgort.             "Depósito
          wa_saida-charg          = wa_vbap-charg.             "Lote
          wa_saida-route          = wa_vbap-route.             "Rota

          wa_saida-zpesagem       = wa_vbak-zpesagem.
          " RMNI - CS0971620 - Correção de dump read table  - Início
          read table it_vbpa with key vbeln = wa_vbak-vbeln binary search.
          if sy-subrc eq 0.
            wa_saida-parvw          = it_vbpa-parvw.
            wa_saida-cod_parc       = it_vbpa-lifnr.
          else.
            clear: wa_saida-parvw,wa_saida-cod_parc.
          endif.
*          wa_saida-parvw          = it_vbpa[ vbeln = wa_vbak-vbeln ]-parvw.
*          wa_saida-cod_parc       = it_vbpa[ vbeln = wa_vbak-vbeln ]-lifnr.
          " RMNI - CS0971620 - Correção de dump read table  - Fim.

          read table it_vbkd into wa_vbkd with key vbeln = wa_vbap-vbeln
                                                   posnr = wa_vbap-posnr binary search.
          if sy-subrc = 0.
            wa_saida-inco1             = wa_vbkd-inco1.             "Incoterms
          else.
            read table it_vbkd into wa_vbkd with key vbeln = wa_vbap-vbeln binary search.
            if sy-subrc = 0.
              wa_saida-inco1             = wa_vbkd-inco1.             "Incoterms
            endif.
          endif.

          wa_saida-kwmeng            = wa_vbap-kwmeng.             "Quatidade Prevista

**  Gera saldo disponível da OV
          wa_saida-sd_disp = 0.
          clear wa_vbfa.
          loop at it_vbfa into wa_vbfa where vbelv = wa_saida-vbeln
                                       and   posnv = wa_saida-posnr.
            add wa_vbfa-rfmng to wa_saida-sd_disp.
          endloop.

          if not sy-subrc is initial.
            wa_saida-sd_disp = wa_saida-kwmeng.
          else.
            if wa_vbfa-meins  eq wa_vbap-vrkme.
              wa_saida-sd_disp = wa_saida-kwmeng - wa_saida-sd_disp.
            else.
              wa_saida-sd_disp =  wa_saida-kwmeng - ( wa_saida-sd_disp / wa_vbap-umziz ).
            endif.

          endif.

          wa_saida-vrkme             = wa_vbap-vrkme.             "UM
*          WA_SAIDA-NETWR             = WA_VBAP-NETWR.             "Valor Total

          read table gt_konv into gw_konv with key knumv = wa_vbak-knumv
                                                   kposn = wa_vbap-posnr
                                                   kschl = 'PR00'. "correção para buscar a condição correta

          wa_saida-kmein = gw_konv-kmein.
*          WA_SAIDA-KBETR = GW_KONV-KBETR.

          if wa_vbap-vrkme ne gw_konv-kmein.
            case gw_konv-kmein.
              when 'TO'.
                qtd_convertida = wa_vbap-kwmeng / 1000.
              when others.
                qtd_convertida = wa_vbap-kwmeng.
            endcase.
          else.
            qtd_convertida = wa_vbap-kwmeng.
          endif.

          wa_saida-kbetr = zcl_util_=>check_desc_abs(
            _vbeln = wa_saida-vbeln
            _posnr = wa_saida-posnr
            des    = conv #( gw_konv-kbetr )
            dir    = abap_true ).  "Busca Preço

          data: p_des  type netwr_ap.


          try .
              p_des = conv #( gt_konv[ kschl = 'RB00' knumv = wa_vbak-knumv kposn = wa_vbap-posnr ]-kbetr  ).
            catch cx_sy_itab_line_not_found.

              p_des = 0.

          endtry.

          wa_saida-desc_absoluto = zcl_util_=>check_desc_abs(
            _vbeln = wa_saida-vbeln
            _posnr = wa_saida-posnr
            des    = p_des
            dir    = abap_true ).  "Busca Desconto Absoluto

*          WA_SAIDA-NETWR = WA_SAIDA-KBETR * QTD_CONVERTIDA.             "Valor Total
          wa_saida-netwr = wa_vbap-netwr + wa_vbap-mwsbp.             "Valor Total    'Alterado para pegar direto da tabela Standard

          data base_liq type kbetr.

*          TRY .
*              BASE_LIQ =  ( ( WA_SAIDA-DESC_ABSOLUTO / QTD_CONVERTIDA ) + GW_KONV-KBETR ).
*            CATCH CX_SY_ZERODIVIDE.
*              CLEAR BASE_LIQ.
*          ENDTRY.

*          IF  GW_KONV-KBETR NE BASE_LIQ.
*            ADD WA_SAIDA-DESC_ABSOLUTO TO WA_SAIDA-NETWR.
*          ENDIF.

          "Faz a verificação se ao considerar o desconto o valor do Preço sofre ou não alteração
          try .
              base_liq =  ( ( wa_saida-desc_absoluto / qtd_convertida ) + wa_saida-kbetr ).
            catch cx_sy_zerodivide.
              clear base_liq.
          endtry.

          if  wa_saida-kbetr ne base_liq.
            wa_saida-kbetr = base_liq.
          endif.

          if it_vbep-lifsp eq '12'.
            clear: wa_saida-netwr, wa_saida-kbetr, wa_saida-sd_disp, wa_saida-kwmeng.
          endif.

          append wa_saida to it_saida.
        endif.
      endloop.
    endloop.

    "Simulador de Vendas – Controle de Transferencias de OV
    loop at it_zsdt0090 into wa_zsdt0090 where doc_simulacao = wa_zsdt0040-doc_simulacao.
      "Nº OV
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_zsdt0090-vbeln
        importing
          output = wa_saida-vbeln2.

      wa_saida-vbeln = wa_zsdt0090-vbeln.
      wa_saida-seq = wa_zsdt0090-sequencia.

      " 31.07.2023 - RAMON - 98680 ---->
      wa_saida-categoria = wa_zsdt0090-categoria.
      " 31.07.2023 - RAMON - 98680 ----<
      read table it_vbak into wa_vbak with key vbeln = wa_zsdt0090-vbeln binary search.
      case wa_vbak-auart.

* Deverá ser exibido no ALV os tipos de O.V. de Devolução AURT = (ZROB / ZRPF) e Recusa (ZREB)
*         WHEN 'ZCOP' OR 'ZROB' OR 'ZREB' OR 'ZRPF'.
        when 'ZCOP'.

          continue.
        when others.
          if v_botoes = 'N'. " Acesso somente para listar as OV do Tipo ZREM / ZTRI e ao Botão “GERAR Remessa/Fatura/ZTRI
            if wa_vbak-auart ne 'ZREM'
               and wa_vbak-auart ne 'ZFTE'
               and wa_vbak-auart ne 'ZRFU'
               and wa_vbak-auart ne 'ZOFE'.
              continue.
            endif.
          endif.
      endcase.

      "Emissor Ordem
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_vbak-kunnr
        importing
          output = wa_saida-kunnr2.

      wa_saida-kunnr             = wa_vbak-kunnr.
      read table it_kna1 into wa_kna1 with key kunnr = wa_vbak-kunnr  binary search.
      wa_saida-name1             = wa_kna1-name1.             "Descrição Emissor

      wa_saida-vendedor          = wa_vbak-vkgrp.             "Vendedor

      wa_saida-auart             = wa_vbak-auart.             "Tipo de OV
      wa_saida-spart             = wa_vbak-spart.             "Setor de Atividades

      loop at it_vbap into wa_vbap  where vbeln = wa_zsdt0090-vbeln.
        clear wl_saida.

        wa_saida-matkl = it_mara[ matnr = wa_vbap-matnr ]-matkl.

        clear it_vbep.                     "<<RIM-SKM-IR120585-23.12.22
        read table it_vbep with key vbeln = wa_vbap-vbeln
                                    posnr = wa_vbap-posnr.
        if sy-subrc is initial.
          if it_vbep-lifsp is initial.
            wa_saida-icon  = '@07@'.
          else.
            wa_saida-icon  = '@06@'.
          endif.
        endif.

        read table it_saida into wl_saida with key vbeln = wa_vbap-vbeln
                                                   posnr = wa_vbap-posnr.
        if sy-subrc is not initial.
          wa_saida-posnr = wa_vbap-posnr.             "Item
          "Material
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = wa_vbap-matnr
            importing
              output = wa_saida-matnr2.

          wa_saida-matnr          = wa_vbap-matnr.
          wa_saida-arktx          = wa_vbap-arktx.             "Descrição Material
          wa_saida-werks          = wa_vbap-werks.             "Centro
          wa_saida-charg          = wa_vbap-charg.             "Lote
          wa_saida-lgort          = wa_vbap-lgort.             "Depósito
          wa_saida-route          = wa_vbap-route.             "Rota

          wa_saida-zpesagem       = wa_vbak-zpesagem.
          " RMNI - CS0971620 - Correção de dump read table  - Início
          read table it_vbpa with key vbeln = wa_vbak-vbeln binary search.
          if sy-subrc eq 0.
            wa_saida-parvw          = it_vbpa-parvw.
            wa_saida-cod_parc       = it_vbpa-lifnr.
          else.
            clear: wa_saida-parvw,wa_saida-cod_parc.
          endif.
*          wa_saida-parvw          = it_vbpa[ vbeln = wa_vbak-vbeln ]-parvw.
*          wa_saida-cod_parc       = it_vbpa[ vbeln = wa_vbak-vbeln ]-lifnr.
          " RMNI - CS0971620 - Correção de dump read table  - Fim

          read table it_vbkd into wa_vbkd with key vbeln = wa_vbap-vbeln
                                                   posnr = wa_vbap-posnr binary search.
          if sy-subrc = 0.
            wa_saida-inco1             = wa_vbkd-inco1.             "Incoterms
          else.
            read table it_vbkd into wa_vbkd with key vbeln = wa_vbap-vbeln binary search.
            if sy-subrc = 0.
              wa_saida-inco1             = wa_vbkd-inco1.             "Incoterms
            endif.
          endif.

          wa_saida-kwmeng            = wa_vbap-kwmeng.             "Quatidade Prevista


* Deverá ser exibido no ALV os tipos de O.V. de Devolução AURT = (ZROB / ZRPF) e Recusa (ZREB)
* A Quantidade e Valor dessas Ordens devem ser exibidos com valores negativos.
          case wa_saida-auart.
            when 'ZROB' or 'ZREB' or 'ZRPF'.
              wa_saida-kwmeng =  wa_saida-kwmeng * -1.
            when others.
          endcase.


**  Gera saldo disponível da OV
          wa_saida-sd_disp = 0.
          loop at it_vbfa into wa_vbfa where vbelv = wa_saida-vbeln
                                       and   posnv = wa_saida-posnr.
            add wa_vbfa-rfmng to wa_saida-sd_disp.
          endloop.

          wa_saida-sd_disp = wa_saida-kwmeng - wa_saida-sd_disp.

          wa_saida-vrkme             = wa_vbap-vrkme.             "UM

          read table gt_konv into gw_konv with key knumv = wa_vbak-knumv
                                                   kposn = wa_vbap-posnr
                                                   kschl = 'PR00'. "correção para buscar a condição correta
          wa_saida-kmein = gw_konv-kmein.

          if wa_vbap-vrkme ne gw_konv-kmein.
            case gw_konv-kmein.
              when 'TO'.
                qtd_convertida = wa_vbap-kwmeng / 1000.
              when others.
                qtd_convertida = wa_vbap-kwmeng.
            endcase.
          else.
            qtd_convertida = wa_vbap-kwmeng.
          endif.

          wa_saida-kbetr = zcl_util_=>check_desc_abs(
            _vbeln = wa_saida-vbeln
            _posnr = wa_saida-posnr
            des    = conv #( gw_konv-kbetr )
            dir    = abap_true ).  " Busca Preço




          try .
              p_des = conv #( gt_konv[ kschl = 'RB00' knumv = wa_vbak-knumv kposn = wa_vbap-posnr ]-kbetr  ).
            catch cx_sy_itab_line_not_found.

              p_des = 0.

          endtry.

          wa_saida-desc_absoluto = zcl_util_=>check_desc_abs(
            _vbeln = wa_saida-vbeln
            _posnr = wa_saida-posnr
            des    = p_des
            dir    = abap_true ).  "Busca Desconto Absoluto

*          WA_SAIDA-NETWR = WA_SAIDA-KBETR * QTD_CONVERTIDA.             "Valor Total
*          ADD WA_SAIDA-DESC_ABSOLUTO TO WA_SAIDA-NETWR.
          wa_saida-netwr = wa_vbap-netwr + wa_vbap-mwsbp.             "Valor Total    'Alterado para pegar direto da tabela Standard


* Deverá ser exibido no ALV os tipos de O.V. de Devolução AURT = (ZROB / ZRPF) e Recusa (ZREB)
* A Quantidade e Valor dessas Ordens devem ser exibidos com valores negativos.
          case wa_saida-auart.
            when 'ZROB' or 'ZREB' or 'ZRPF'.
*              WA_SAIDA-NETWR =  WA_SAIDA-NETWR * -1. "wbarbosa US-169490 10/06/2025
              wa_saida-sd_disp = 0. "wbarbosa US-169490 29/07/2025
              wa_saida-netwr *= -1. "WBARBOSA US-169490 01/09/2025
            when others.
          endcase.


          "Faz a verificação se ao considerar o desconto o valor do Preço sofre ou não alteração
          try .
              base_liq =  ( ( wa_saida-desc_absoluto / qtd_convertida ) + wa_saida-kbetr ).
            catch cx_sy_zerodivide.
              clear base_liq.
          endtry.

          if  wa_saida-kbetr ne base_liq.
            wa_saida-kbetr = base_liq.
          endif.



          if it_vbep-lifsp eq '12'.
            clear: wa_saida-netwr, wa_saida-kbetr, wa_saida-sd_disp, wa_saida-kwmeng.
          endif.

          append wa_saida to it_saida.
          clear: base_liq.

        endif.
      endloop.

    endloop.

  endloop.

  sort it_saida by doc_simulacao vbeln posnr.

  check not s_matnr is initial.
  delete it_saida where matnr not in s_matnr.


  "FF #172796 - inicio
  if it_saida[] is initial and p_extcal is not initial.
    data(lv_tb_saida_vazia) = abap_true.
    export lv_tb_saida_vazia from lv_tb_saida_vazia  to memory id 'memory_tb_saida_vazia'. "Import será feito na transação ZSDT0081, programa ZSDR0038
  endif.
  "FF #172796 - fim


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

  perform f_alv_fieldcat.

  wa_layout = value #(
                        zebra      = abap_true
                        no_rowins  = abap_true
                        stylefname = 'ESTILO'
                        info_fname = 'COLOR'
                        sel_mode   = 'D'
                        no_rowmark = abap_true
                        no_toolbar = abap_false
                        cwidth_opt = abap_true
                      ).

  call screen 0100.

endform.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: fcode type table of sy-ucomm.

  "FF #145609 - inicio
* Verificar o número de linhas
  if p_extcal is not initial. "Programa chamado pela ZSDT0081
    data(lv_count) = lines( it_saida ).

* Se a tabela tiver apenas uma linha, preencher o campo CHECK com 'X'
    if lv_count = 1.
      read table it_saida index 1 assigning field-symbol(<fs_saida>).
      <fs_saida>-check = 'X'.
    endif.

    sy-ucomm = 'DESMEMBRAR'.
    perform desmembrar using 'X'.
  endif.
*  "FF #145609 - fim


  check p_extcal is initial. "Se o programa estiver sendo chamado pelo ZSDR0038, não executar a parte do ALV.
  "FF #145609 - fim

  free: fcode.

  fcode = cond #(
     when v_botoes eq 'N'
          then
                value #(
                          ( 'TRANSF' )
                          ( 'ESTORNAR' )
                          ( 'CANCELAR' )
                          ( 'DESMEMBRAR' )
                          ( 'DESMEMB_M' )
*                         ( 'DESBLOQ' )
                          ( 'ALT_DT_ENT' )
                          ( 'TROCA' )
                          ( 'TROCA_M' )
                          ( 'ENCERRAR' )
                          ( 'PRICE' )
                          " 10.09.2024 - RAMON - 97513 -->
                          ( 'PRICE_NEW' )
                          ( 'AGRUP_OV' )
                          ( 'ATU_IMP' )
                          " 10.09.2024 - RAMON - 97513 --<
                          ( 'REDIST' )
                          ( 'MDF_VENC' )
                        )
                  ).

  " 30.03.2023 - RAMON - 97513 -->
  "  " não habilita o novo botao em PRD
  if sy-sysid = 'PRD'. " #teste #apagar "#debug "#comentar
    append 'PRICE_NEW' to fcode.
    append 'AGRUP_OV' to fcode.
    "APPEND 'ATU_IMP' TO fcode. " 10.09.2024 - RAMON - 97513
  endif.
  " 30.03.2023 - RAMON - 97513 --<

* Ocultar Botão 'GERAR'
* Com a Criação do Cockpit de montagem de Carga (ZSDT0112) e o Cockpit de Faturamento (ZLES0136)
* essa funcionalidade passou a não ter sentido aqui na ZSDT0087.
  data: vl_ucomm type sy-ucomm.
  vl_ucomm = 'GERAR'.
  append vl_ucomm to fcode.

  " 18.05.2023 - RAMON - controle de acesso via auth - 97513 -->

  if sy-uname = 'RBLIMA'. " #teste #apagar "#debug "#comentar
    clear fcode[].
  else.

    if gv_stvarv_ativa is not initial.

      call function 'ZSDMF_AUTORIZ_BOTOES_ZSDT0087'
        exporting
          iv_bukrs = wa_saida-vkorg
          iv_vkbur = wa_saida-vkbur
        tables
          ct_ucomm = fcode[].

    endif.

  endif.

  " 18.05.2023 - RAMON - controle de acesso via auth - 97513 --<
  set pf-status 'F_SET_PF' excluding fcode.
  set titlebar  'ZFTITLE'.

  if cl_container_95 is initial.
    create object cl_container_95
      exporting
        container_name = 'CONTAINER'.

    create object cl_grid
      exporting
        i_parent = cl_container_95.

    call method cl_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    wg_save = 'X'.
    gs_variant_c-report = sy-repid. "Enable users save own LAYOUTs

    call method cl_grid->set_table_for_first_display
      exporting
        is_variant      = gs_variant_c
        is_layout       = wa_layout
        i_save          = wg_save
        i_default       = abap_true
      changing
        it_fieldcatalog = it_fieldcat[]        "i_default = 'X'
        it_sort         = i_sort[]
        it_outtab       = it_saida[].

    call method cl_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method cl_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler: lcl_event_handler=>catch_hotspot for cl_grid,
                 lcl_event_handler=>on_data_changed_finished for cl_grid,
                 lcl_event_handler=>on_data_changed for cl_grid.
  else.
    call method cl_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_alv_header .
  data:   wl_data(10),
           wl_hora(8),
           wl_linha(60),
  wl_text type sdydo_text_element.

  wl_text = 'Cockipt do Simulador de Vendas'.


  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

endform.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv_fieldcat .

  free: it_fieldcat.
  data i type i.

  perform alv using:
*COL_POS FIELDNAME         SCRTEXT_S                  DO_SUM  EDIT  NO_ZERO FIX_COLUMN  CHECKBOX  HOTSPOT ICON  OUTPUTLEN
      01   'ICON'            'Status'                 ' '     ' '   ' '     'X'         ' '       ' '     'X'   '06',
      02   'CHECK'           'Chk'                    ' '     'X'   ' '     'X'         'X'       ' '     ' '   ' ',
      03   'DOC_SIMULACAO2'  'Nº Simulação'           ' '     ' '   'X'     ' '         ' '       'X'     ' '   ' ',
      04   'VKORG'           'Org. de Vendas'         ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      05   'VKBUR'           'Esc. de Vendas'         ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      06   'VTWEG'           'Canal de Dist.'         ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      07   'VENDEDOR'        'Vendedor'               ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      08   'WAERK'           'Moeda'                  ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      09   'CULTURA'         'Cultura'                ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      10   'SAFRA'           'Safra'                  ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      11   'TPSIM'           'Cond. Pagamento'        ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      12   'KUNNR2'          'Emissor Ordem'          ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      13   'NAME1'           'Descrição Emissor'      ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      14   'VBELN2'          'Nº OV'                  ' '     ' '   'X'     ' '         ' '       'X'     ' '   ' ',
      15   'AUART'           'Tipo de OV'             ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      16   'SPART'           'Setor de Atividades'    ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      17   'POSNR'           'Item'                   ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      18   'MATNR2'          'Material'               ' '     ' '   'X'     ' '         ' '       ' '     ' '   ' ',
      19   'ARKTX'           'Descrição Material'     ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      20   'WERKS'           'Centro'                 ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      21   'LGORT'           'Depósito'               ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      22   'CHARG'           'Lote'                   ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      23   'INCO1'           'Incoterms'              ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      24   'KWMENG'          'Quatidade Prevista'     'X'     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      25   'VRKME'           'UM'                     ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      26   'SD_DISP'         'Saldo disponível'       'X'     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      27   'KBETR'           'Preço'                  'X'     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      28   'KMEIN'           'UM Preço'               ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      29   'DESC_ABSOLUTO'   'Desc. Abs'              ' '     ' '   ' '     ' '         ' '       ' '     ' '   ' ',
      30   'NETWR'           'Valor Total'            'X'     ' '   ' '     ' '         ' '       ' '     ' '   ' '.

endform.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  data: begin of tl_0090 occurs 0.
          include type zsdt0090.
  data: end of tl_0090.

  data: tl_fields type table of sval with header line.

  data: vl_vbeln   type p,
        vl_vbeln_c type string.


  types: begin of ty_bsid,
           bukrs type bsid-bukrs,
           kunnr type bsid-kunnr,
           vbel2 type bsid-vbel2,
         end of ty_bsid,

         begin of ty_bsad,
           bukrs type bsad-bukrs,
           kunnr type bsad-kunnr,
           vbel2 type bsad-vbel2,
         end of ty_bsad.

  data: it_bsid type table of ty_bsid,
        it_bsad type table of ty_bsad.

  data: begin of wl_active,
          docsta type j_1bnfe_active-docsta,
          cancel type j_1bnfe_active-cancel,
        end of wl_active,


        lv_matkl  type mara-matkl,
        lv_matnr  type mara-matnr,
        lv_msg    type c length 255,
        lv_return.

  data lv_erro." 10.09.2024 - RAMON - 97513
*** Alteracao Alexandre Rimini 05.04.2023 - CS1078275
  data: begin of it_fat,
          vbelv type vbfa-vbelv,
        end of it_fat.

  data: begin of it_vbrk,
          vbeln type vbrk-vbeln,
        end of it_vbrk.

***ffim alteracao Alexandre Rimini

  if not cl_grid is initial.
    call method cl_grid->dispatch
      exporting
        cargo         = sy-ucomm
        eventid       = 19
        is_shellevent = ' '.

    if sy-ucomm is initial.
      call method cl_grid->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.
  endif.

  refresh tl_return.

  h_ucomm = sy-ucomm.

  case sy-ucomm.
    when 'BACK' or 'UP'.
      refresh it_saida.
      call method cl_grid->refresh_table_display
        exporting
          is_stable = wa_stable.

      " 29.09.2025 - US #191706 - RAMON -->
      data lv_fluxo type flag.

      get parameter id 'ZRT' field lv_fluxo.

      if lv_fluxo = 'X'.
        leave program.
      else.
        leave to screen 0.
      endif.
      " 29.09.2025 - US #191706 - RAMON --<

    when 'CANCEL'.
      leave program.
    when 'REFRESH'.
      perform refresh.
  endcase.

  if zcl_util_=>check_block( table = it_saida ) is not initial.

    " 10.09.2024 - RAMON - 97513 -->

    if gv_stvarv_ativa is not initial.
      perform f_check_autorizacao changing lv_erro.
    endif.

    if lv_erro is initial.

      read table it_saida into wa_saida with key check = abap_true.
      if sy-subrc is initial.
        case sy-ucomm.
          when 'CANCELAR' or 'ESTORNAR'.
          when others.

            if sy-ucomm ne 'DESMEMBRAR' and
               sy-ucomm ne 'ALTERAR'.

              call method zcl_manutencao_insumos=>chk_desconto_abs_faturado
                exporting
                  i_vbeln = wa_saida-vbeln
                importing
                  is_ok   = data(is_ok).

              if is_ok is not initial.
                message text-e13 type 'I'.
                exit.
              endif.

            endif.

            call method zcl_manutencao_insumos=>chk_divergencia_ov_simulador
              exporting
                i_vbeln               = wa_saida-vbeln
                i_matnr               = wa_saida-matnr
                i_simulador           = wa_saida-doc_simulacao
              importing
                e_ordem_venda         = data(e_ordem_venda)
                e_simulador           = data(e_simulador)
                e_diferenca_simulador = data(e_diferenca).

            if e_diferenca is not initial.
              call method zcl_manutencao_insumos=>get_tolerancia_desconto_abs
                importing
                  e_tolerancia = data(e_tolerancia).

              data(e_tolerancia_negativo) = e_tolerancia * -1.

              if e_diferenca not between e_tolerancia_negativo and e_tolerancia.
                message |Totais: Ordem de Venda: { e_ordem_venda }, Simulador: { e_simulador }, Diferença: { e_diferenca }| type 'I'.
                exit.
              endif.
            endif.

        endcase.
      endif.

      " 10.09.2024 - RAMON - 97513 <--
      case sy-ucomm.
        when 'ZTRI'.
          p_ucomm = sy-ucomm.
          perform ztri.
        when 'TROCA'.

*      BREAK-POINT.

*      PERFORM TROCA.

          perform troca_material.
*        ZCL_UTIL_=>DISPARO_0116( CONV #( P_0090 ) ).
        when 'DESBLOQ'.
          perform desbloq.
        when 'DESMEMBRAR'.
          p_ucomm = sy-ucomm.
          perform desmembrar using 'X'.
*        ZCL_UTIL_=>DISPARO_0116( CONV #( P_0090 ) ).
        when 'CANCELAR'.
          perform cancelar.
        when 'ESTORNAR'.
          p_ucomm = sy-ucomm.
          perform estornar.
        when 'ALTERAR'.
          p_ucomm = sy-ucomm.
          perform alterar.
        when 'TRANSF'.
          p_ucomm = sy-ucomm.
          perform transf.
*        ZCL_UTIL_=>DISPARO_0116( CONV #( P_0090 ) ).
        when 'GERAR'.
          perform gerar.
        when 'ENCERRAR'.
          perform encerrar.
        when 'PRICE'.
          perform price.

          " 09.01.2023 - RAMON - 61181 -->
        when 'PRICE_NEW'.
          perform price_new.
          " 09.01.2023 - RAMON - 61181 --<

          "16.06.2023 - RAMON - 98680 -->
        when 'AGRUP_OV'.
          perform f_agrupar_ov.
          "16.06.2023 - RAMON - 98680 --<

          " 31.08.2023 - RAMON - 101482 -->
        when 'ATU_IMP'.
          perform f_atualiza_imposto.
          " 31.08.2023 - RAMON - 101482 --<

        when 'REDIST'.
          perform redist.
        when 'MDF_VENC'.
          perform mdf_venc using ''.
        when 'ALT_GERAIS'.
          perform alt_gerais.
        when 'DEP_LOTE'.
          perform dep_lote.
        when 'ALT_DT_ENT'.
          perform alt_data_entrega.
        when 'ITINERATIO'.
          read table it_saida into data(ls_saida) with key check = abap_true.
          if sy-subrc is initial.
            call method zcl_manutencao_insumos=>call_create_itinerario
              exporting
                i_pc = conv #( ls_saida-werks )
                i_lr = ls_saida-kunnr.
          endif.
        when 'DESMEMB_M'.
          perform f_desmembramento_massa.
        when 'TROCA_M'.
          perform f_troca_massa.
        when 'DESMEMB_R'.
          perform f_desmembrar_devolucao.
      endcase.
    endif.
  endif.

  perform refresh.
  clear: p_ucomm, h_ucomm.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  set pf-status 'Z001'.
  set titlebar 'Z001'.
endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.

  if ( sy-dynnr eq '0200' ).
    case sy-ucomm.

      when: 'RADIO'.
        case 'X'.
          when wg_venda-r_triang.
            wg_venda-zrad     = 'X'.

          when wg_venda-r_ordem.
            case wa_saida-auart.
              when 'ZFTE' or 'ZDEF' or 'ZSEM'.
                wg_venda-inco1 = 'CPT'.
                wg_venda-c_ordem = 'X'.
              when 'ZOFE' or 'ZODF' or 'ZOSM'.
                wg_venda-inco1 = ' '.
            endcase.
            clear wg_venda-zrad.

          when others.
            wg_venda-inco1 = wa_saida-inco1.
            clear wg_venda-zrad.
            clear wg_venda-c_ordem.
        endcase.

      when: 'OBS'.
*        INCLUIR TEXTO PARA A OV
*        É ARMAZENADO COM A CHAVE DOC_SIMULADOR E SEQ DA ZSDT0090
        perform incluir_texto.

      when: 'SAIR'.
        leave to screen 0.

      when: 'SEARCH'.
        if wg_venda-r_triang = 'X'.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wg_venda-kunnr
            importing
              output = wg_venda-kunnr.

          select single name1
            from kna1
            into v_name1
          where kunnr = wg_venda-kunnr.

          if sy-subrc = 0.
            wg_venda-name1 = v_name1.
          else.
            message 'Cliente não cadastrado' type 'I'.
            clear: wg_venda-kunnr, wg_venda-name1.
          endif.
        endif.

      when: 'CONF'.

        if wg_venda-r_triang = 'X'.
          if wg_venda-kunnr is initial.
            message 'Informe o cliente' type 'I'.
            exit.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wg_venda-kunnr
            importing
              output = wg_venda-kunnr.

          select single name1
         from kna1
         into v_name1
          where kunnr = wg_venda-kunnr.
          if sy-subrc ne 0.
            message 'Cliente não cadastrado' type 'I'.
            clear: wg_venda-kunnr, wg_venda-name1.
            exit.
          endif.
*          IF WG_VENDA-TXT_ORDEM IS INITIAL.
          if t_lines[] is initial.
            message 'Informe o texto da ordem' type 'I'.
            exit.
          endif.
*          IF WG_VENDA-TXT_ITEM IS INITIAL.
*            MESSAGE 'Informe o texto do item' TYPE 'I'.
*            EXIT.
*          ENDIF.
        endif.

        clear v_erro.
        if wg_venda-r_ordem is not initial.
          loop at it_saida into wa_saida where check eq abap_true.
*   Verifica se existe Frete Associoado.
            v_erro = cond #( when zcl_util_=>get_zsdt0037( wa_saida ) is not initial then abap_true else v_erro ).
          endloop.
        endif.

*   Sai se não esta correto a Associoação do Frete
        check v_erro is initial.

        refresh it_ov.
        loop at tg_ov.
          if tg_ov-qt_tran gt 0.
            wa_ov-vbeln = tg_ov-vbeln.
            wa_ov-posnr = tg_ov-posnr.
            wa_ov-matnr = tg_ov-matnr.
            wa_ov-zmeng = tg_ov-qt_tran.
            wa_ov-kursf = wg_venda-kursf.
            wa_ov-inco1 = wg_venda-inco1.

            data(a_vbeln) = wa_ov-vbeln.
            data(a_posnr) = wa_ov-posnr.

            append wa_ov to it_ov.
          endif.
        endloop.
        clear v_auart.
        if wg_venda-r_triang = 'X'.
          v_auart = 'ZREM'.
        endif.
        if wg_venda-r_futura = 'X'.
          v_auart = 'ZFUT'.
** Checar se taxa de caâmbio foi preenchido
          if wg_venda-kursf is initial.
            message 'Informe a taxa de câmbio' type 'I'.
            exit.
          endif.
        endif.
        if wg_venda-r_ordem = 'X'.
          v_auart = wa_saida-auart.
          if wa_saida-auart = 'ZRFU'.
            clear v_auart.
            read table it_mara into wa_mara with key matnr = wa_saida-matnr binary search.
            if sy-subrc = 0 .
              read table t_set with key valfrom = wa_mara-matkl.
              if sy-subrc  = 0.
                read table t_lay with key  setclass   = t_set-setclass
                                           subclass   = t_set-subclass
                                           setname    = t_set-setname
                                           lineid     = t_set-lineid.
                if sy-subrc = 0.
                  v_auart = t_lay-descript+0(4).
                endif.
              endif.
            endif.
          endif.

          if wg_venda-ztri ne 'X'. "Gerar Remessa/Fatura/ZTRI ( não testa)
            case v_auart.
              when 'ZDEF'.
                v_auart = 'ZODF'.
              when 'ZFTE'.
                v_auart = 'ZOFE'.
              when 'ZSEM'.
                v_auart = 'ZOSM'.
              when 'ZODF'.
                v_auart = 'ZDEF'.
              when 'ZOFE'.
                v_auart = 'ZFTE'.
              when 'ZOSM'.
                v_auart = 'ZSEM'.
            endcase.
          endif.
          if v_auart is initial.
            message 'Tipo de OV destino não definida' type 'I'.
            exit.
          endif.
        endif.

        if it_ov[] is initial.
          message 'Não foi informada quantidade a transferir em nenhum item' type 'I'.
          exit.
        endif.

        data b_vbeln type vbeln.
        data calc_1 type p decimals 5.
        data calc_2 type p decimals 5.
        data difer_0 type p decimals 5.
        data it_desc_abs type table of zsdt0041.

        if wg_venda-ztri = 'X'. "Gerar Remessa/Fatura/ZTRI
          perform z_cria_remessa.

        elseif wg_venda-r_triang = 'X'.


          zcl_util_=>modify_ordem(
            exporting
              i_auart     = v_auart
              i_acao      = p_ucomm
              i_kunnr     = wg_venda-kunnr
              i_txt_ordem = wg_venda-txt_ordem
              i_txt_item  = wg_venda-txt_item
              t_ov        = it_ov
              t_obs       = t_lines[]
            changing
              i_vbeln     = b_vbeln
              t_return    = tl_return[]
          ).

          perform f_exibe_bapi.

        else.

          zcl_util_=>modify_ordem(
            exporting
              i_auart  = v_auart
              i_acao   = p_ucomm
              t_ov     = it_ov
            changing
              i_vbeln  = b_vbeln
              t_return = tl_return[]
          ).

          perform f_exibe_bapi.

        endif.

*        ZCL_UTIL_=>DISPARO_0116( B_VBELN ).

        perform refresh.
        leave to screen 0.

    endcase.
  endif.


endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields output.
  loop at screen.
    if wa_saida-auart = 'ZRFU'.
      if screen-name ne 'WG_VENDA-R_ORDEM' and
         screen-name ne 'WG_VENDA-D_ORDEM'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
*    ELSEIF 'ZOFE_ZODF_ZOSM' CS WA_SAIDA-AUART.
*      IF SCREEN-NAME   NE 'WG_VENDA-R_FUTURA' AND
*         SCREEN-NAME   NE 'WG_VENDA-D_FUTURA' AND
*         SCREEN-GROUP1 NE 'B3'.
*        SCREEN-INPUT     = 0.
*        SCREEN-INVISIBLE = 1.
*        MODIFY SCREEN.
*      ENDIF.
    elseif wa_saida-auart = 'ZREM'.
      screen-input     = 0.
      screen-invisible = 1.
      modify screen.
    endif.
    if  wg_venda-zrad     ne 'X'.
      if screen-group1 eq 'B1'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endif.
    if wg_venda-ztri is not initial.
      if screen-name eq 'WG_VENDA-TXT_ITEM'
      or screen-name eq 'TXTITEM'.
        screen-input     = 1.
        screen-invisible = 0.
        modify screen.
      endif.
    elseif wg_venda-ztri is initial.
      if screen-name eq 'WG_VENDA-TXT_ITEM'
      or screen-name eq 'TXTITEM'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endif.
    if wg_venda-r_futura is initial.
      if screen-group1 eq 'B3'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endif.
    if wg_venda-r_ordem is initial or wg_venda-c_ordem is not initial.
      if screen-group1 eq 'B5'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endif.
  endloop.


endmodule.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.
  data: "EVENT TYPE CNTL_SIMPLE_EVENT,
    "EVENTS TYPE CNTL_SIMPLE_EVENTS,
    tl_filter   type lvc_t_filt,
    wl_filter   type lvc_s_filt,
    tl_function type ui_functions,
    wl_function like tl_function with header line.

  clear: wa_layout.
  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
*  WA_STABLE-ROW        = C_X.
  wa_layout-info_fname = 'COLOR'.
  wa_layout-no_toolbar = c_x.
  wa_layout-no_toolbar = space.
*  WA_LAYOUT-COL_OPT    = C_X.
  wa_layout-grid_title = ''.
  wa_layout-no_toolbar = abap_true.
  wa_layout-grid_title = ' '.

  "GRID1
  if obg_conteiner is initial.
    create object obg_conteiner
      exporting
        container_name = 'CC_OV'.

    create object grid1
      exporting
        i_parent = obg_conteiner.

    perform montar_layout.

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

    call method grid1->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_ov[].

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler:
              lcl_event_handler=>on_data_changed_finished1 for grid1,
              lcl_event_handler=>on_data_changed1 for grid1.

  else.

    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
         1 ' '           ' '        'TG_OV' 'VBELN2'          'Ordem'           '10' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV' 'POSNR'           'Item'            '05' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV' 'MATNR2'          'Material'        '12' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV' 'ARKTX'           'Descrição'       '20' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV' 'KWMENG'          'Qtd. Prevista'   '15' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV' 'VRKME'           'UM'              '05' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV' 'SD_DISP'         'Sld. Disponivel' '15' ' ' ' ' ' ' ' ' ' ',
         1 'VBAP'        'KWMENG'   'TG_OV' 'QT_TRAN'         'Sld. Transferir' '15' 'X' ' ' ' ' ' ' ' '.


endform.                    " MONTAR_LAYOUT
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
                            value(p_emphasize)
                            value(p_zero)
                            value(p_f4).

  clear w_fieldcatalog.

  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.
  w_fieldcatalog-col_pos       = p_col_pos.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.
  w_fieldcatalog-no_zero       = p_zero.
  w_fieldcatalog-f4availabl    = p_f4.

  if p_outputlen is not initial.
    w_fieldcatalog-outputlen      = p_outputlen.
  endif.

  if p_field eq 'ZWERT' or
     p_field eq 'KBETR'.
    w_fieldcatalog-datatype         = 'CURR'.
  endif.

  if p_field eq 'ITINERARIO'.
*    W_FIELDCATALOG-ICON = ABAP_TRUE.
    w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
  endif.

  append w_fieldcatalog to t_fieldcatalog.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  SEARCH_CLI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_cli input.

endmodule.                 " SEARCH_CLI  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_cria_remessa .

  data: vl_delivery      type bapishpdelivnumb-deliv_numb,
        tl_item          type table of bapidlvreftosalesorder,
        sl_item          type bapidlvreftosalesorder,
        tl_text          type table of bapisdtext,
        sl_text          type bapisdtext,

        sl_data_rem      type ledat,
        sl_ship_point    type vstel,
        wl_erro(1),

        wg_documento(10),
        wa_zsdt0090      type zsdt0090.

  data: wl_data_fatura type c length 10.

  data: t_success type standard table of bapivbrksuccess with header line.
  data: t_billing type standard table of bapivbrk        with header line.
  data: t_return  type standard table of bapireturn1     with header line.

  refresh: tl_item, tl_return.
  clear: vl_delivery, sl_item, wl_erro.

  free: tl_return, tl_text , i_order_item_in, i_order_item_inx, i_sched, i_schedx.
  clear f_headinx.

  f_headinx-updateflag = 'U'.

** Altera texto para o item da ordem
  loop at tg_ov.
    if tg_ov-qt_tran gt 0.
      sl_item-ref_doc    = tg_ov-vbeln.
      sl_item-ref_item   = tg_ov-posnr.
      sl_item-dlv_qty    = tg_ov-qt_tran.
      sl_item-sales_unit = tg_ov-vrkme.
      sl_ship_point      = wa_saida-werks.
      append sl_item to tl_item.

      clear: sl_text, i_order_item_in, i_order_item_inx.
      sl_text-text_line(72) = wg_venda-txt_item.
      sl_text-text_id       = '0001'.
      sl_text-itm_number    = tg_ov-posnr.
      sl_text-langu         = sy-langu.
      sl_text-format_col    = '/'.
      append sl_text to tl_text.

      move: 'U'         to i_order_item_inx-updateflag,
            tg_ov-posnr to i_order_item_inx-itm_number,
            tg_ov-posnr to i_order_item_in-itm_number.

      append: i_order_item_in, i_order_item_inx.
    endif.
  endloop.
  "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = wa_saida-vbeln
      order_header_inx = f_headinx
    tables
      return           = tl_return
      order_item_in    = i_order_item_in
      order_item_inx   = i_order_item_inx
      order_text       = tl_text.

  call function 'BAPI_TRANSACTION_COMMIT'
    exporting
      wait = 'X'.

  sl_data_rem = sy-datum.
  "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
  call function 'BAPI_OUTB_DELIVERY_CREATE_SLS' "#EC CI_USAGE_OK[2438131]
    exporting
      ship_point        = sl_ship_point
      due_date          = sl_data_rem
    importing
      delivery          = vl_delivery
    tables
      sales_order_items = tl_item
      return            = tl_return.

  if vl_delivery is initial.
*     Retorna Erro
    perform f_exibe_bapi.
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    "
    clear vg_erro_pic.
    perform  z_picking  using vl_delivery.

    if vg_erro_pic is initial. " faturameto
      wl_data_fatura = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.

      t_billing-ref_doc    = vl_delivery.
      t_billing-ref_doc_ca = 'J'.
      t_billing-bill_date  = wl_data_fatura.

      append t_billing.
      "*---> 19/07/2023 - Migração S4 - LO
      call function 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
        tables
          billingdatain = t_billing
          return        = t_return
          success       = t_success.

      if t_success[] is not initial.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.
        "Tempo para o faturamento commitar
*        WAIT UP TO 10 SECONDS.
        loop at t_return.
          move-corresponding t_return to tl_return.
          append tl_return.
        endloop.
      endif.
    endif.

    perform f_exibe_bapi.
  endif.

endform.                    " Z_CRIA_REMESSA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0300 output.
  set pf-status 'Z001'.
  set titlebar 'Z001'.
endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_0300 output.

  clear wa_layout.
  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
*  WA_STABLE-ROW        = C_X.
  wa_layout-info_fname = 'COLOR'.
  wa_layout-no_toolbar = c_x.
  wa_layout-grid_title = ' '.

  if obg_conteiner2 is initial.
    create object obg_conteiner2
      exporting
        container_name = 'CC_OV2'.

    create object grid2
      exporting
        i_parent = obg_conteiner2.

    perform montar_layout2 using ''.

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
    wa_layout-no_toolbar = 'X'.


    call method grid2->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_ov2[].

    call method grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_data_changed_finished2 for grid2,
                 lcl_event_handler=>on_data_changed2 for grid2.
  else.

    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS_0300  OUTPUT

*&---------------------------------------------------------------------*
*& Module EXT_CAL_CONF_300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module ext_cal_conf_300 output.

  check p_extcal is not initial.

  perform f_conf_300. "Se for uma chamada feita pelo  programa ZSDR0038, executar o "CONF" pois o PAI não será executado

endmodule.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout2 using p_direcao.

  data: c_x type c,
        tam type n length 3 value 10.

  free t_fieldcatalog.


  if p_direcao eq 'C2'.
    c_x = abap_true.
    tam = 25.
  endif.

  case sy-ucomm.
    when 'REDIST'.

      perform montar_estrutura using:
           1 ' '     ' '        'it_popup2' 'KUNNR2'    'Emissor Ordem'       '10' c_x ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'NAME1'     'Descrição Emissor'   tam  ' ' ' ' ' ' ' ' ' '.

      if c_x is initial .
        perform montar_estrutura using:
             1 ' '     ' '        'it_popup2' 'VBELN2'    'Ordem'               '10' ' ' ' ' ' ' ' ' ' ',
             1 ' '     ' '        'it_popup2' 'POSNR'     'Item'                '05' ' ' ' ' ' ' ' ' ' '.
      endif.

      perform montar_estrutura using:
           1 ' '     ' '        'it_popup2' 'MATNR2'    'Material'            '09' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'ARKTX'     'Descrição'           tam  ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'KWMENG'    'Qtd. Prevista'       '10' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'VRKME'     'UM'                  '04' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'LGORT'     'Deposito'            '05' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'WERKS'     'Centro'              '05' c_x ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'CHARG'     'Lote'                '10' c_x ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'KBETR'     'Preço'               '08' c_x ' ' ' ' ' ' ' ',
           1 ' '     ' '        'it_popup2' 'KMEIN'     'UM P'                '04' ''  ' ' ' ' ' ' ' '.

      if c_x is initial .
        perform montar_estrutura using:
             1 ' '     ' '        'it_popup2' 'SD_DISP'   'Saldo Disp.'       '15' ' ' ' ' ' ' ' ' ' ',
             1 'VBAP'  'KWMENG'   'it_popup2' 'QT_TRAN'   'Quantidade'          '15' 'X' ' ' ' ' ' ' ' '.
      endif.

    when 'DEP_LOTE'.

      perform montar_estrutura using:
*           1 ' '     ' '        'IT_DEP_LOTE1' 'KUNNR2'    'Emissor Ordem'       '10' ' ' ' ' ' ',
*           1 ' '     ' '        'IT_DEP_LOTE1' 'NAME1'     'Descrição Emissor'   '15' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'VBELN2'    'Ordem'               '10' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'POSNR'     'Item'                '05' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'MATNR2'    'Material'            '09' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'ARKTX'     'Descrição'           '15' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'KWMENG'    'Qtd. Prevista'       '10' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'VRKME'     'UM'                  '04' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'WERKS'     'Centro'              '05' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'LGORT'     'Deposito'            '05' c_x ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'CHARG'     'Lote'                '10' c_x ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'KBETR'     'Preço'               '08' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'KMEIN'     'UM P'                '04' ' ' ' ' ' ' ' ' ' ',
           1 ' '     ' '        'IT_DEP_LOTE1' 'SD_DISP'   'Saldo Disp.'         '15' ' ' ' ' ' ' ' ' ' '.

    when others.
      perform montar_estrutura using:
          1 ' '           ' '        'TG_OV2' 'VBELN2'          'Ordem'           '10' ' ' ' ' ' ' ' ' ' ',
          1 ' '           ' '        'TG_OV2' 'POSNR'           'Item'            '05' ' ' ' ' ' ' ' ' ' ',
          1 ' '           ' '        'TG_OV2' 'MATNR2'          'Material'        '12' ' ' ' ' ' ' ' ' ' ',
          1 ' '           ' '        'TG_OV2' 'ARKTX'           'Descrição'       '20' ' ' ' ' ' ' ' ' ' ',
          1 ' '           ' '        'TG_OV2' 'KWMENG'          'Qtd. Prevista'   '15' ' ' ' ' ' ' ' ' ' ',
          1 ' '           ' '        'TG_OV2' 'VRKME'           'UM'              '05' ' ' ' ' ' ' ' ' ' '.

      if wg_desmem-zdes eq abap_true.
        perform montar_estrutura using:
              1 ' '           ' '        'TG_OV2' 'SD_DISP'         'Saldo Disp.'   '15' ' ' ' ' ' ' ' ' ' '.
      else.
        perform montar_estrutura using:
              1 ' '           ' '        'TG_OV2' 'SD_DISP'         'Qtd. Entregue'   '15' ' ' ' ' ' ' ' ' ' '.
      endif.

      perform montar_estrutura using:
              1 'VBAP'        'KWMENG'   'TG_OV2' 'QT_TRAN'         'Quantidade'      '15' 'X' ' ' ' ' ' ' ' '.

  endcase.

endform.                    " MONTAR_LAYOUT2
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0300 input.
  if ( sy-dynnr eq '0300' ).
    case sy-ucomm.
      when: 'SAIR'.
        clear wg_desmem-zdes.
        leave to screen 0.
      when: 'SEARCH'.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wg_desmem-kunnr
          importing
            output = wg_desmem-kunnr.

        select single name1
          from kna1
          into v_name1
        where kunnr = wg_desmem-kunnr.
        if sy-subrc = 0.
          wg_desmem-name1 = v_name1.
        else.
          message 'Cliente não cadastrado' type 'I'.
          clear: wg_desmem-kunnr, wg_desmem-name1.
        endif.
      when: 'CONF'.

        "FF #145609  - inicio
        perform f_conf_300.

*        IF wg_desmem-zdes EQ abap_true.
*          IF wg_desmem-kunnr IS INITIAL.
*            MESSAGE 'Informe o cliente' TYPE 'I'.
*            EXIT.
*          ENDIF.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = wg_desmem-kunnr
*            IMPORTING
*              output = wg_desmem-kunnr.
*
*          SELECT SINGLE name1
*            FROM kna1
*            INTO v_name1
*          WHERE kunnr = wg_desmem-kunnr.
*
*          IF sy-subrc NE 0.
*            MESSAGE 'Cliente não cadastrado' TYPE 'I'.
*            CLEAR: wg_desmem-kunnr, wg_desmem-name1.
*            EXIT.
*          ENDIF.
*
*        ENDIF.
*
*        FREE: it_ov, it_popup2_aux.
*
**        "FF #145609 - inicio
*        IF p_extcal IS NOT INITIAL.
*          READ TABLE tg_ov2 ASSIGNING FIELD-SYMBOL(<fs_ov2>) INDEX 1.
*          IF <fs_ov2> IS ASSIGNED.
*            <fs_ov2>-qt_tran = lv_qte_sol.
*          ENDIF.
*        ENDIF.
**        "FF #145609 -  fim
*
*        LOOP AT tg_ov2.
*          IF tg_ov2-qt_tran EQ 0.
*            MESSAGE 'Não foi informada a quantidade.' TYPE 'I'.
*            CONTINUE.
*          ENDIF.
*
*          READ TABLE it_sai_aux WITH KEY vbeln = tg_ov2-vbeln posnr = tg_ov2-posnr.
*
*          wa_ov-vbeln = tg_ov2-vbeln.
*          wa_ov-posnr = tg_ov2-posnr.
*          wa_ov-matnr = tg_ov2-matnr.
*          wa_ov-zmeng = tg_ov2-qt_tran.
*
*          IF wg_desmem-zdes NE abap_true.
*            it_sai_aux-qt_tran = tg_ov2-qt_tran - it_sai_aux-kwmeng.
*          ELSE.
*            it_sai_aux-qt_tran = tg_ov2-qt_tran.
*          ENDIF.
*
*          APPEND wa_ov TO it_ov.
*          MODIFY it_sai_aux INDEX sy-tabix TRANSPORTING qt_tran.
*
*        ENDLOOP.
*
*        CHECK it_ov IS NOT INITIAL.
*
*        it_popup2_aux[] = it_sai_aux[].
*
*        IF wg_desmem-zdes EQ abap_true.
*
*          zcl_util_=>modify_ordem(
*          EXPORTING
*                    i_auart     = wa_saida-auart
*                    i_acao      = p_ucomm
*                    i_kunnr     = wg_desmem-kunnr
*                    t_ov        = it_ov
*           CHANGING
*                    i_vbeln = it_popup2_aux-vbeln ).
*
*          DATA(vbeln) = it_popup2_aux-vbeln.
*
*          LOOP AT it_popup2_aux.
*            it_popup2_aux-vbeln = vbeln.
*            MODIFY it_popup2_aux INDEX sy-tabix TRANSPORTING vbeln.
*          ENDLOOP.
*
*          LOOP AT it_popup2_aux.
*
*            CLEAR: p_0090, wa_0090.
*            PERFORM insert_zsdt0090 USING 'D'
*                                          it_popup2_aux-doc_simulacao
*                                          it_popup2_aux-vbeln
*                                          it_popup2_aux-vbeln2
*                                          it_popup2_aux-posnr
*                                    CHANGING p_0090.
**         // Inserir depois do Disparo da 90
*            zcl_util_=>disparo_0116( i_vbeln = p_0090-vbeln i_posnr = p_0090-posnn ).
*          ENDLOOP.
*
*
*        ELSE.
*
*          DATA: t_calc TYPE TABLE OF ty_calc,
*                tt_ov  TYPE TABLE OF zsds015.
*
*          FREE tl_return.
*
**          "FF #145609 - inicio
*          IF p_extcal IS NOT INITIAL. "Se este programa foi chamado pela ZSDT0081... lv_0081  = X.
*            DATA(lv_0081) = abap_true.
*          ENDIF.
**          "FF #145609 - fim.
*
*          CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
*            EXPORTING
*              i_soma    = 'A'
*              i_acao    = p_ucomm
*              i_0081    = lv_0081 "FF #145609
*            TABLES
*              it_ov     = it_ov
*              te_return = tl_return.
*
*          LOOP AT it_ov ASSIGNING FIELD-SYMBOL(<wa_>).
*            zcl_util_=>processa_desc( i_vbeln = <wa_>-vbeln i_matnr = <wa_>-matnr ).
*          ENDLOOP.
*
*          READ TABLE it_popup2_aux INDEX 1.
*          CLEAR: p_0090, wa_0090.
*
*          p_0090 = zcl_util_=>insert_90old(
*                                               simulador = it_popup2_aux-doc_simulacao
*                                               ordem     = it_popup2_aux-vbeln
*                                               material  = it_popup2_aux-matnr
*                                               qtd       = it_popup2_aux-qt_tran
*                                               categoria = 'A'
*                                          ).
*
**          PERFORM INSERT_ZSDT0090 USING 'A'
**                            IT_POPUP2_AUX-DOC_SIMULACAO
**                            IT_POPUP2_AUX-VBELN
**                            IT_POPUP2_AUX-VBELN2
**                            IT_POPUP2_AUX-POSNR
**                            CHANGING P_0090.
*
*          PERFORM hedge USING 'A'.
*          PERFORM f_exibe_bapi.
*
*        ENDIF.
*
**     "// Processa 40
*        zcl_util_=>loop_40(
*                              simulador = it_popup2_aux-doc_simulacao
*                              i_vbeln   = it_popup2_aux-vbeln
*                              i_matnr   = it_popup2_aux-matnr
*                              qtd       = 10
*                           ).
*
*        PERFORM refresh.
*        LEAVE TO SCREEN 0.

        "FF #145609  - fim

    endcase.
  endif.
endmodule.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields_0300 output.
  loop at screen.
    if wg_desmem-zdes ne 'X'.
      if screen-group1 eq 'B1'.
        screen-input     = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endif.
  endloop.

endmodule.                 " TRATA_FIELDS_0300  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_exibe_bapi .

  check tl_return[] is not initial.
  refresh tl_saida_exec.
  clear: tl_saida_exec.

  move: wa_saida-inco1 to tl_saida_exec-inco1,
        wa_saida-spart to tl_saida_exec-spart,
        wa_saida-auart to tl_saida_exec-auart,
        wa_saida-werks to tl_saida_exec-werks,
        wa_saida-vbeln to tl_saida_exec-vbeln.

  loop at tl_return.
    move: tl_return-message to tl_saida_exec-msg.
    append tl_saida_exec.
  endloop.

  perform montar_layout_err.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    tables
      t_outtab              = tl_saida_exec.

endform.                    "F_EXIBE_BAPI
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_err .
  refresh estrutura.
  perform montar_estrutura2 using:
   1  'ZSDT0041'   'INCO1'            'TL_SAIDA_EXEC' 'INCO1'     ' '  ' ',
   2  'ZSDT0041'   'SPART'            'TL_SAIDA_EXEC' 'SPART'     ' '  ' ',
   3  'ZSDT0041'   'AUART'            'TL_SAIDA_EXEC' 'AUART'     ' '  ' ',
   4  'ZSDT0041'   'WERKS'            'TL_SAIDA_EXEC' 'WERKS'     ' '  ' ',
   4  'ZSDT0041'   'VBELN'            'TL_SAIDA_EXEC' 'VBELN'     ' '  ' ',
   4  'ZSDT0041'   'MSG'              'TL_SAIDA_EXEC' 'MSG'       'Msg de bapi'   '80'.
endform.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
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
form montar_estrutura2 using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen).

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  if p_outputlen is initial.
    wa_estrutura-outputlen     = x_contador.
  else.
    wa_estrutura-outputlen     =  p_outputlen.
  endif.

  case p_field.
    when 'BELNR'
      or 'AWKEY'.
      wa_estrutura-hotspot = 'X'.
      wa_estrutura-key = 'X'.

  endcase.


  append wa_estrutura to estrutura.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  Z_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_DELIVERY  text
*----------------------------------------------------------------------*
form z_picking  using    p_delivery.

  data: sl_vbkok_wa type vbkok,
        tl_vbpok    type table of vbpok,
        sl_vbpok    type vbpok,
        tl_prot     type table of prott,
        sl_prot     type prott,
        it_lips     type table of lips with header line.
  " Delete remessa
  data: sl_hdata    type bapiobdlvhdrchg,
        sl_hcont    type bapiobdlvhdrctrlchg,
        tl_bapiret2 type bapiret2_t.

  refresh: tl_vbpok ,
           tl_return.

  free it_lips.

  select *
    from lips
      into table it_lips
  where vbeln eq p_delivery.

  sl_vbkok_wa-vbeln_vl  = p_delivery.
  sl_vbkok_wa-vbeln     = p_delivery.
  sl_vbkok_wa-wabuc     = 'X'.
  sl_vbkok_wa-komue     = 'X'.
  sl_vbkok_wa-wadat_ist = sy-datum.

  loop at it_lips.

    sl_vbpok-vbeln_vl       = p_delivery.
    sl_vbpok-posnr_vl       = it_lips-posnr.
    sl_vbpok-vbeln          = p_delivery.
    sl_vbpok-posnn          = it_lips-posnr.
    sl_vbpok-matnr          = it_lips-matnr.
    sl_vbpok-pikmg          = it_lips-lfimg.
    sl_vbpok-charg          = it_lips-charg.
    sl_vbpok-gewei          = it_lips-gewei.

**************************************************
    sl_vbpok-lgort          = it_lips-lgort.
    sl_vbpok-brgew          = it_lips-lfimg.
    sl_vbpok-ntgew          = it_lips-lfimg.
**************************************************

    append sl_vbpok to tl_vbpok.
  endloop.

*  LOOP AT tg_ov.
*    sl_vbpok-vbeln_vl       = p_delivery.
*    sl_vbpok-posnr_vl       = tg_ov-posnr.
*    sl_vbpok-vbeln          = p_delivery.
*    sl_vbpok-posnn          = tg_ov-posnr.
*    sl_vbpok-matnr          = tg_ov-matnr.
*    sl_vbpok-pikmg          = tg_ov-qt_tran.
*    sl_vbpok-charg          = tg_ov-charg.
*    sl_vbpok-gewei          = 'KG'.
*
***************************************************
*    sl_vbpok-lgort          = tg_ov-lgort.
*    sl_vbpok-brgew          = tg_ov-qt_tran.
*    sl_vbpok-ntgew          = tg_ov-qt_tran.
***************************************************
*
*    APPEND sl_vbpok TO tl_vbpok.
*  ENDLOOP.


  clear: tl_prot[].

  call function 'SD_DELIVERY_UPDATE_PICKING_1'
    exporting
      vbkok_wa                 = sl_vbkok_wa
      synchron                 = 'X'
      if_error_messages_send_1 = 'X'
    tables
      vbpok_tab                = tl_vbpok
      prot                     = tl_prot.

  if ( tl_prot[] is initial ).
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    vg_erro_pic = 'X'.
    refresh tl_return.
    loop at tl_prot into sl_prot.
      message id sl_prot-msgid
        type sl_prot-msgty
      number sl_prot-msgno
        with sl_prot-msgv1
             sl_prot-msgv2
             sl_prot-msgv3
             sl_prot-msgv4
        into tl_return-message.
      tl_return-type = sl_prot-msgty.
      append tl_return.
      clear: sl_prot  .
    endloop.

*   Deleta Delivery Criado
    sl_hdata-deliv_numb = p_delivery.
    sl_hcont-deliv_numb = p_delivery.
    sl_hcont-dlv_del    = 'X'.

    call function 'BAPI_OUTB_DELIVERY_CHANGE'
      exporting
        header_data    = sl_hdata
        header_control = sl_hcont
        delivery       = p_delivery
      tables
        return         = tl_bapiret2.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    perform f_exibe_bapi.
  endif.
endform.                    " Z_PICKING
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0400 output.
  set pf-status 'Z001'.
  set titlebar 'Z001'.
endmodule.                 " STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0400 input.
  data: lv_valor    type bapikbetr1,
        wl_ov       like line of tg_ov_mat,
        tw_zsdt0087 type zsdt0087.

  case sy-ucomm.
    when 'BTSH'.
      perform f4_mat using ''.
    when 'SEARCH'.

      lv_matnr = |{ wg_material-matnr alpha = in }|.

      try .
          wg_des_matnr = tg_material[ matnr = lv_matnr ]-maktx.
        catch cx_sy_itab_line_not_found.
      endtry.

      try .
          data(wa_mat) = tg_ov_mat[ 1 ].
          check wg_material-qtd is not initial.
          wa_preco_-vlr_venda =
          cond #( when wa_mat-vrkme ne wa_mat-meins
                       then switch #( wa_mat-vrkme when 'TO' or 'KG'
                                       then cond #( when wa_mat-meins eq 'TO'
                                                    then ( ( ( wa_mat-netpr / 1000 ) * wa_mat-qt_tran ) / wg_material-qtd )
                                                    else ( ( ( wa_mat-netpr * 1000 ) * wa_mat-qt_tran ) / wg_material-qtd ) )
                                       else ( wa_mat-netpr * wa_mat-qt_tran ) / wg_material-qtd
                                     )
                       else ( wa_mat-netpr * wa_mat-qt_tran ) / wg_material-qtd
                ).
        catch cx_sy_itab_line_not_found.
      endtry.

      wa_preco_-uni_venda = tg_mat[ wg_material-id ]-meins.

      wa_preco_-vlr_cadastrado = tg_mat[ wg_material-id ]-vlr_venda.
      wa_preco_-uni_cadastrado = tg_mat[ wg_material-id ]-meins.
      wa_preco_-vlr_total = wa_preco_-vlr_venda * wg_material-qtd.


    when 'BACK' or 'EXIT' or 'CANCEL' or 'SAIR'.

      leave to screen 0.

    when 'CONF'.

      if tg_ov_mat[ 1 ]-qt_tran eq 0.
        message 'Informe a quantidade a ser trocada' type 'E'.
        exit.
      endif.

      if wg_material-matnr is initial.
        message 'Informe material para substituir' type 'E'.
        exit.
      else.

        lv_matnr = |{ wg_material-matnr alpha = in }|.

        if not line_exists( tg_material[ matnr = lv_matnr ] ).
          message 'Material inválido.' type 'E'.
          exit.
        endif.

      endif.

      if wg_material-qtd is initial.
        message 'Informe a quantidade a ser substituída.' type 'E'.
        exit.
      endif.

      if wg_material-lgort is initial.
        message 'O campo Deposito é Obrigatório!' type 'E'.
        exit.
      endif.

      select count(*)
             from mard
             where matnr eq wg_material-matnr
               and werks eq wg_material-werks_fornec
      and lgort eq wg_material-lgort.
      if sy-subrc is not initial.
        message 'Material não expandido para o Depósito informado!' type 'E'.
        exit.
      endif.

      free: it_ov, it_popup2.
      clear: wa_ov.

      read table tg_ov_mat index 1.

      append value #(
                      vbeln = tg_ov_mat-vbeln
                      posnr = tg_ov_mat-posnr
                      zmeng = tg_ov_mat-kwmeng - tg_ov_mat-qt_tran
                      matnr = tg_ov_mat-matnr
                    ) to it_ov.

      clear: wa_ov, it_popup2.

      move-corresponding wa_saida to it_popup2.

      it_popup2-vbeln = wa_ov-vbeln = tg_ov_mat-vbeln.
      it_popup2-posnr = tg_ov_mat-posnr.
      wa_ov-zmeng = wg_material-qtd.
      it_popup2-qt_tran = tg_ov_mat-qt_tran.
      wa_ov-matnr = wg_material-matnr.
      wa_ov-charg = wg_material-lote.
      wa_ov-meins = wg_material-meins_mara. "UNIDADE DE MEDIDA MATERIAL
      wa_ov-vrkme = wg_material-meins.
      wa_ov-werks = wg_material-werks_fornec.
*      WA_OV-LGORT = TG_OV_MAT-LGORT.
      wa_ov-lgort = wg_material-lgort.

      if  tg_ov_mat-vrkme ne tg_ov_mat-meins. " VRKME, MEINS DA SAIDA "PRINCIPAL OU POPUP"
        case tg_ov_mat-vrkme. " Unidade de Medida do Peso do Material Velho
          when 'TO' or 'KG'.
            case tg_ov_mat-meins. "Unidade de Medida do Preço do Material Velho
              when 'TO'.
                it_popup2-netwr = wa_ov-netpr = ( ( ( tg_ov_mat-netpr / 1000 ) * tg_ov_mat-qt_tran ) / wg_material-qtd ).
                wa_ov-dif_desc = ( it_popup2-netwr * wg_material-qtd ) - ( ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ) / 1000 ).
              when 'KG'.
                it_popup2-netwr = wa_ov-netpr = ( ( ( tg_ov_mat-netpr * 1000 ) * tg_ov_mat-qt_tran ) / wg_material-qtd ).
                wa_ov-dif_desc = ( it_popup2-netwr * wg_material-qtd ) - ( ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ) * 1000 ).
              when others.
                it_popup2-netwr = wa_ov-netpr = ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ) / wg_material-qtd.
                wa_ov-dif_desc = ( it_popup2-netwr * wg_material-qtd ) - ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ).
            endcase.
          when others.
            it_popup2-netwr = wa_ov-netpr = ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ) / wg_material-qtd.
            wa_ov-dif_desc = ( it_popup2-netwr * wg_material-qtd ) - ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ).
        endcase.
      else.
        it_popup2-netwr = wa_ov-netpr = ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ) / wg_material-qtd.
        wa_ov-dif_desc = ( it_popup2-netwr * wg_material-qtd ) - ( tg_ov_mat-netpr * tg_ov_mat-qt_tran ).
      endif.

      diferenca_qt = wa_ov-dif_desc.

      append wa_ov to it_ov.
      append it_popup2.

      free tl_return.
      clear v_auart.

      if wg_material-matkl eq lv_matkl.
        if wg_material-werks_fornec eq wa_saida-werks.
          if wg_material-inco1 eq wa_saida-inco1.

            select count(*)
              from vbap
            where vbeln eq tg_ov_mat-vbeln
            and matnr eq wg_material-matnr.

            if not sy-subrc is initial.

              clear: p_0090, wa_0090.
              it_popup2_aux[] = it_popup2[].
              read table it_popup2_aux index 1.
              perform insert_zsdt0090 using 'UV'
                                            it_popup2_aux-doc_simulacao
                                            it_popup2_aux-vbeln "OLD
                                            it_popup2_aux-vbeln2 "NEW
                                            it_popup2_aux-posnr
                                      changing p_0090.

              call function 'ZSDMF001_ATUALI_OV_MAT'
                exporting
                  i_soma    = 'A'
                tables
                  it_ov     = it_ov
                  te_return = tl_return.

              perform insert_zsdt0090 using 'UN'
                                            it_popup2_aux-doc_simulacao
                                            it_popup2_aux-vbeln
                                            it_popup2_aux-vbeln2
                                            it_popup2_aux-posnr
                                      changing p_0090.

              read table tl_return with key type = 'S'.
              if sy-subrc = 0.
                commit work.
              endif.

              perform hedge using 'U'.

              if not diferenca_qt is initial.
                perform insert_zsdt0090 using 'OO'
                                              wa_saida-doc_simulacao
                                              ' '             "NEW
                                              tg_ov_mat-vbeln "OLD
                                              wg_material-matnr
                                              changing p_0090.

                read table tl_return with key type = 'S'.
                if sy-subrc = 0.
                  commit work.
                endif.
              endif.

            else.
              message |Material: { wg_material-matnr }, INCO: { wg_material-inco1 }, Centro: { wg_material-werks_fornec } já Existe nesta OV. { tg_ov_mat-vbeln }| type 'I'.
            endif.

          else.
            v_auart = 'TROC'.
          endif.
        else.
          v_auart = 'TROC'.
        endif.
      else.
        v_auart = 'TROC'.
      endif.

      if v_auart eq 'TROC'.

        it_popup2_aux[] = it_popup2[].

        free it_popup2[].

        select single * from zsdt0087
          into tw_zsdt0087
          where matkl eq wg_material-matkl
            and tpsim eq it_popup2-tpsim
        and inco1 eq wg_material-inco1.

        if not sy-subrc is initial.
          message |Grp. Material: { wg_material-matkl }, Tp Simulador: { it_popup2-tpsim }, Incoterms: { wg_material-inco1 }.  Combinação não parametrizada na Transação ZSDT0085.| type 'I'.
          exit.
        endif.

        it_popup2-inco1  = tw_zsdt0087-inco1.
        it_popup2-auart  = tw_zsdt0087-auart.
        it_popup2-spart  = tw_zsdt0087-spart.

        it_popup2-kwmeng  = wg_material-qtd.
        it_popup2-qt_tran = tg_ov_mat-qt_tran.
        it_popup2-matnr   = wg_material-matnr.
        it_popup2-charg   = wg_material-lote.

*       IT_POPUP2-KMEIN   = WG_MATERIAL-MEINS_MARA.
        it_popup2-kmein   = wg_material-meins.

        it_popup2-vrkme   = wg_material-meins.
        it_popup2-kbetr   = it_popup2-netwr.
        it_popup2-werks   = wg_material-werks_fornec.
        it_popup2-lgort   = wg_material-lgort.

        append it_popup2.

        perform cria_ov using 'M'."TROCA DE MATERIAS

        read table tl_return with key type = 'E'.
        if not sy-subrc is initial.

          sort it_ov[] by posnr.
          delete it_ov index 1.

          call function 'ZSDMF001_ATUALI_OV_MAT'
            exporting
              i_soma    = 'A'
            tables
              it_ov     = it_ov
              te_return = tl_return.

        endif.
      endif.

      perform f_exibe_bapi.
      perform refresh.
      free it_ov[].
      clear diferenca_qt.
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0400  INPUT

*----------------------------------------------------------------------*
*  MODULE CRIA_OBJETOS_0400 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module cria_objetos_0400 output.

  clear: wa_layout.

  wa_layout =  value #(
                        zebra      = abap_true
                        no_rowmark = abap_true
                        no_toolbar = abap_true
                        grid_title = abap_false
*                        CWIDTH_OPT = ABAP_TRUE
                      ).

*  WA_STABLE-ROW        = ABAP_TRUE.

  if obj_cont_mat is initial.
    create object obj_cont_mat
      exporting
        container_name = 'OBJ_CONT_MAT'.

    create object obj_grid_mat
      exporting
        i_parent = obj_cont_mat.

    perform montar_layout_mat.

    tl_function
    = value #(
               ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
               ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
               ( cl_gui_alv_grid=>mc_fc_loc_move_row )
               ( cl_gui_alv_grid=>mc_fc_loc_paste )
               ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
               ( cl_gui_alv_grid=>mc_fc_loc_undo )
               ( cl_gui_alv_grid=>mc_fc_loc_append_row )
               ( cl_gui_alv_grid=>mc_fc_loc_copy )
               ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
               ( cl_gui_alv_grid=>mc_fc_loc_cut )
             ).

    call method obj_grid_mat->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_ov_mat[].

    call method obj_grid_mat->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method obj_grid_mat->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_data_changed_finished_mat for obj_grid_mat,
                 lcl_event_handler=>on_data_changed_mat for obj_grid_mat.

  else.

    call method obj_grid_mat->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.
endmodule.                    "CRIA_OBJETOS_0400 OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout_mat.
  refresh t_fieldcatalog.
  perform montar_estrutura using:
         1 ' '           ' '        'TG_OV_MAT' 'ID'              'Seq'             '02' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV_MAT' 'VBELN'           'Ordem'           '10' ' ' ' ' ' ' 'X' ' ',
         1 ' '           ' '        'TG_OV_MAT' 'POSNR'           'Item'            '05' ' ' ' ' ' ' 'X' ' ',
         1 ' '           ' '        'TG_OV_MAT' 'MATNR'           'Material'        '12' ' ' ' ' ' ' 'X' 'X',
         1 ' '           ' '        'TG_OV_MAT' 'ARKTX'           'Descrição'       '15' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV_MAT' 'KWMENG'          'Qtd. Prevista'   '10' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV_MAT' 'VRKME'           'UM'              '05' ' ' ' ' ' ' ' ' ' ',
         1 'VBAP'        'NETWR'    'TG_OV_MAT' 'NETWR'           'Valor Total'     '10' ' ' ' ' ' ' ' ' ' ',
         1 ' '           ' '        'TG_OV_MAT' 'SD_DISP'         'Sld. Disponivel' '10' ' ' ' ' ' ' ' ' ' ',
         1 'VBAP'        'KWMENG'   'TG_OV_MAT' 'QT_TRAN'         'Quantidade'      '10' 'X' ' ' ' ' ' ' ' '.


endform.                    "MONTAR_LAYOUT_MAT

form montar_layout_mat_new.
  refresh t_fieldcatalog.
  perform montar_estrutura using:
         01 ''  ''  'IT_TROCA_NEW' 'ID'              'Seq'               '02' '' '' '' '' '',
         02 ''  ''  'IT_TROCA_NEW' 'MATNR'           'Material'          '12' '' '' '' 'X' 'X',
         03 ''  ''  'IT_TROCA_NEW' 'MAKTX'           'Descrição'         '15' '' '' '' '' '',
         04 ''  ''  'IT_TROCA_NEW' 'INCO1'           'Icoterms'          '04' '' '' '' '' '',
         05 ''  ''  'IT_TROCA_NEW' 'WERKS_FORNEC'    'Centro Fornecedor' '04' '' '' '' '' '',
         06 ''  ''  'IT_TROCA_NEW' 'LOTE'            'Lote'              '10' '' '' '' '' '',
         07 ''  ''  'IT_TROCA_NEW' 'LGORT'           'Deposito'          '10' '' '' '' '' '',
         08 'VBAP' 'KWMENG'  'IT_TROCA_NEW' 'QTD'    'Quantidade'        '10' '' '' '' '' '',
         09 ''  ''  'IT_TROCA_NEW' 'MEINS'           'Unidade de Medida' '03' '' '' '' '' '',
         10 ''  ''  'IT_TROCA_NEW' 'VLR_CADASTRADO'  'Pr. Cadastrado'    '10' '' '' '' '' '',
         11 ''  ''  'IT_TROCA_NEW' 'VLR_VENDA'       'Pr. Troca'         '10' '' '' '' '' '',
         12 ''  ''  'IT_TROCA_NEW' 'VLR_TOTAL'       'Vl. Total'         '10' '' '' '' '' '',
         13 ''  ''  'IT_TROCA_NEW' 'ITINERARIO'      'Itinerario'        '10' '' '' '' '' ''.

endform.                    "MONTAR_LAYOUT_MAT
*&---------------------------------------------------------------------*
*&      Module  MO_BUSCA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module mo_busca_material input.
  free: gt_dselc, gt_return_tab.

  if tg_material[] is not initial.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'ID'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
*       DYNPROFIELD     = 'MARA-MATNR'
        value_org       = 'S'
      tables
        value_tab       = tg_material
        return_tab      = gt_return_tab
        dynpfld_mapping = gt_dselc
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    if sy-subrc = 0.
      read table gt_return_tab index 1.
      condense gt_return_tab-fieldval no-gaps.
      read table tg_material with key id = gt_return_tab-fieldval.
      move-corresponding tg_material to wg_material.
    else.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
endmodule.                 " MO_BUSCA_MATERIAL  INPUT


*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABNAME    text
*      -->FIELDNAME  text
*      -->DISPLAY    text
*      -->RETURNCODE text
*      -->VALUE      text
*----------------------------------------------------------------------*
form f_busca_fatura using tabname fieldname display         "#EC CALLED
                    changing returncode value.

  data: begin of tl_vbfa occurs 0,
          vbeln type vbfa-vbeln,
        end of tl_vbfa.

  free: gt_dselc, gt_return_tab.

  loop at tg_vbfa.
    clear tl_vbfa.
    tl_vbfa-vbeln = tg_vbfa-vbeln.
    append tl_vbfa.
  endloop.


  sort tl_vbfa by vbeln.
  delete adjacent duplicates from tl_vbfa comparing vbeln.


  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'NR_ORDEM'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'NR_ORDEM'
      value_org       = 'S'
    tables
      value_tab       = tl_vbfa
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

endform.                    "F_BUSCA_FATURA
*&---------------------------------------------------------------------*
*&      Form  ENCERRAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form encerramento.

  data: p_resp.

  call function 'POPUP_TO_CONFIRM'
    exporting
      text_question         = 'Deseja Encerrar esta Ordem de Venda?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    importing
      answer                = p_resp.

  check p_resp eq 1.

  it_sai_aux[] = it_saida.
  delete it_sai_aux where check eq abap_false.

  clear v_erro.

  loop at it_sai_aux  where check eq abap_true.
    if it_sai_aux-inco1 eq 'CIF'.
*       Verifica se existe Frete Associoado, apenas quando OV for CIF
      v_erro = cond #( when zcl_util_=>get_zsdt0037( it_sai_aux ) is not initial then abap_true else v_erro ).
    endif.
  endloop.
*   Sai se não esta correto a Associoação do Frete
  check v_erro is initial.

  perform busca_saldo using 'E' changing it_sai_aux[].

  data(_aux) = it_sai_aux[ 1 ].

  call method zcl_util_=>get_qtd_embarcado
    exporting
      i_vbeln = _aux-vbeln
      i_posnr = _aux-posnr
    importing
      qtd_emb = data(_qtd_emb).

  if  _qtd_emb is not initial.

    call method zcl_manutencao_insumos=>popup_confirm
      exporting
        i_mensagem = text-002
      importing
        is_ok      = data(is_ok).

    check is_ok is not initial.

  endif.

  call method zcl_manutencao_insumos=>rowback_encerramento
    exporting
      i_vbeln          = _aux-vbeln
      i_posnr          = _aux-posnr
      i_qtde_encerrada = abs( _aux-sd_disp ).

  perform atualiza_ov.

  perform f_exibe_bapi.
  perform refresh.

  clear ok_code.

endform.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
form busca_saldo using p_direcao changing p_table type standard table.

  data:
    wl_matnr   type mara-matnr,
    wl_zieme   type mara-meins,
    wl_pmein   type mara-meins,
    wl_menge   type ekpo-menge,
    wl_tabix   type sy-tabix,
    tw_convert type table of ty_saida with header line.

  tw_convert[] = p_table[].

  select * from vbap
    into table t_vbap
    for all entries in tw_convert
    where vbeln eq tw_convert-vbeln
  and matnr eq tw_convert-matnr.

  check not t_vbap[] is initial.

  select * from vbfa
    into table t_vbfa
    for all entries in t_vbap
    where vbelv eq t_vbap-vbeln
      and posnv eq t_vbap-posnr
      and vbtyp_n eq 'J'
      and vbtyp_v eq 'C'
  and bwart ne space.

  if t_vbfa[] is not initial.
    select *
      from vbfa
      into table tl_vbfa_aux
       for all entries in t_vbfa
       where vbeln eq t_vbfa-vbeln
         and vbtyp_n eq 'J'
         and vbtyp_v eq 'J'
    and bwart ne space.

    loop at tl_vbfa_aux.
      delete t_vbfa where vbeln eq tl_vbfa_aux-vbeln.
    endloop.
  endif.

  sort t_vbfa by vbelv.
  free tg_saldo.

  loop at tw_convert.
    clear wg_saldo.

    loop at t_vbfa where vbelv eq tw_convert-vbeln
                     and posnv eq tw_convert-posnr.

      wg_saldo-vbeln = t_vbfa-vbelv.
      wg_saldo-posnr = t_vbfa-posnv.
      wg_saldo-werks = tw_convert-werks.

      read table t_vbap with key vbeln = t_vbfa-vbelv posnr = t_vbfa-posnv.
      if t_vbfa-meins  eq t_vbap-vrkme.
        wg_saldo-total = t_vbfa-rfmng.
      else.
        wg_saldo-total = t_vbfa-rfmng / t_vbap-umziz.
      endif.

      collect wg_saldo into tg_saldo.
      wl_tabix = sy-tabix.
    endloop.
    if sy-subrc is initial.
      read table tg_saldo into wg_saldo index wl_tabix.
    endif.

    wl_matnr = tw_convert-matnr.
    wl_zieme = tw_convert-vrkme.
    wl_pmein = t_vbfa-meins.
    wl_menge = tw_convert-kwmeng.

    wg_saldo-zmeng = wl_menge.
    wg_saldo-doc_simulacao = tw_convert-doc_simulacao.

    if  'A' cs p_direcao.
      wg_saldo-saldo = wg_saldo-total.
    else.
      if 'M_D' cs p_direcao.
        wg_saldo-saldo = ( wg_saldo-zmeng - wg_saldo-total ).
      else.
        wg_saldo-saldo = ( wg_saldo-zmeng - wg_saldo-total ) * -1.
      endif.
    endif.

    if wl_tabix is not initial.
      modify tg_saldo from wg_saldo index wl_tabix.
    else.
      wg_saldo-vbeln = tw_convert-vbeln.
      wg_saldo-werks = tw_convert-werks.
      wg_saldo-posnr = tw_convert-posnr.
      append wg_saldo to tg_saldo.
    endif.
    clear: wl_tabix.

  endloop.

  delete tg_saldo where vbeln is initial.

  if tg_saldo[] is not initial.
    loop at tg_saldo into wg_saldo.
      read table tw_convert with key vbeln = wg_saldo-vbeln
                             doc_simulacao = wg_saldo-doc_simulacao
                             posnr         = wg_saldo-posnr
                             werks         = wg_saldo-werks.
      if sy-subrc is initial.

        case p_direcao.
          when 'A'.
            tw_convert-kwmeng  = wg_saldo-zmeng.
          when 'E'.
            tw_convert-kwmeng  = wg_saldo-total.
        endcase.

        tw_convert-sd_disp = wg_saldo-saldo.
        modify tw_convert index sy-tabix.
      endif.
      clear: tl_0041.
    endloop.
  endif.

  p_table[] = tw_convert[].

endform.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form atualiza_ov .

  data: tl_vbak            type table of vbak with header line,
        tl_konv            type table of konv with header line,
        tl_vbap            type table of vbap with header line,
        tl_vbkd            type table of vbkd with header line,
        tl_vbep            type table of vbep with header line,
        wl_orderheaderin   type bapisdh1,
        wl_orderheaderinx  type bapisdh1x,
        wl_logic_switch    type bapisdls,
        tl_conditions_in   type table of bapicond with header line,
        tl_conditions_inx  type table of bapicondx with header line,
        tl_bapisditm       type table of bapisditm with header line,
        tl_bapisditmx      type table of bapisditmx with header line,
        wl_return          type bapiret2,
        tl_schedule_lines  type table of bapischdl with header line,
        tl_schedule_linesx type table of bapischdlx with header line,
        wl_tabix           type sy-tabix,
        v_check            type c,
        vl_desc_abs        type konv-kbetr.

  clear: it_desc_enc[].

  check not it_sai_aux[] is initial.

  select *
   from zsdt0090
   into table @data(it_0090)
   where vbelv eq @wa_saida-vbeln
     and posnv eq @wa_saida-posnr
     and categoria eq 'O'
  and estorno eq @abap_false.

  select * from vbak
    into table tl_vbak
      for all entries in it_sai_aux
  where vbeln eq it_sai_aux-vbeln.

  check not tl_vbak[] is initial.

  "26.04.2017 Ajuste Desconto
  select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
    into corresponding fields of table @tl_konv
     for all entries in @tl_vbak
  where knumv eq @tl_vbak-knumv.

  select * from vbkd
    into table tl_vbkd
      for all entries in tl_vbak
  where vbeln eq tl_vbak-vbeln.

  select * from vbap
    into table tl_vbap
      for all entries in tl_vbak
  where vbeln eq tl_vbak-vbeln.

  if sy-subrc is initial.

    select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
      into table @data(it_konv)
         for all entries in @tl_vbak
          where knumv eq @tl_vbak-knumv
    and kschl eq 'PR00'.

    select * from vbep
      into table tl_vbep
        for all entries in tl_vbap
        where vbeln eq tl_vbap-vbeln
    and posnr eq tl_vbap-posnr.

    loop at tl_vbep where wmeng is initial.
      delete tl_vbep where vbeln eq tl_vbep-vbeln
                       and posnr eq tl_vbep-posnr
                       and etenr eq tl_vbep-etenr.
    endloop.

    wl_orderheaderinx-updateflag = 'U'.
    wl_logic_switch-cond_handl   = 'X'.

    loop at tl_vbap.

      v_check = abap_false.

      free: tl_bapisditm, tl_bapisditmx, tl_conditions_in, tl_conditions_inx,
            tl_schedule_lines, tl_schedule_linesx, tl_return.

      clear: tl_bapisditm, tl_bapisditmx, tl_conditions_in, tl_conditions_inx,
             tl_schedule_lines, tl_schedule_linesx.

      clear: it_sai_aux, tl_bapisditm, tl_bapisditmx, tl_vbak, tl_vbkd, vl_desc_abs.


      read table tl_vbak with key vbeln = tl_vbap-vbeln.
      read table tl_vbkd with key vbeln = tl_vbak-vbeln.

      "Check Desconto
      if ( h_ucomm = 'ENCERRAR' ).
        loop at tl_konv where knumv eq tl_vbak-knumv
                          and kposn eq tl_vbap-posnr
                          and kschl eq 'RB00'.
          vl_desc_abs = vl_desc_abs + abs( tl_konv-kbetr ).
        endloop.
      endif.

      read table it_sai_aux with key vbeln = tl_vbap-vbeln
                                     posnr = tl_vbap-posnr.
      if sy-subrc is initial.
        wl_tabix = sy-tabix.

        move: 'U'               to tl_bapisditmx-updateflag,
              tl_vbap-posnr     to tl_bapisditmx-itm_number,
              'X'               to tl_bapisditmx-target_qty,
              tl_vbap-posnr     to tl_bapisditm-itm_number.

        if it_sai_aux-kwmeng eq 0.
          move 1 to tl_bapisditm-target_qty.
          v_check = abap_true.
        else.
          move it_sai_aux-kwmeng to tl_bapisditm-target_qty.
        endif.

        append tl_bapisditmx.
        append tl_bapisditm.

        data(saldo) = tl_bapisditm-target_qty.

        clear: tl_bapisditm, tl_bapisditmx.

        field-symbols <vbep> type vbep.
        clear tl_vbep.

        loop at tl_vbep assigning <vbep>
             where posnr eq tl_vbap-posnr.

          if saldo is initial.
            <vbep>-wmeng = 0.
            continue.
          endif.

          if saldo > <vbep>-wmeng.
            saldo = saldo - <vbep>-wmeng.
          else.
            <vbep>-wmeng = saldo.
            saldo = 0.
          endif.
        endloop.

        if saldo > 0.
          <vbep>-wmeng = <vbep>-wmeng + saldo.
        endif.

        if ( vl_desc_abs is not initial ) and ( h_ucomm = 'ENCERRAR' ).

          read table tg_saldo into wg_saldo with key vbeln = tl_vbap-vbeln
                                                     posnr = tl_vbap-posnr.

          if ( sy-subrc = 0 ).

            clear: it_desc_enc.
            it_desc_enc-vbeln2    =  tl_vbap-vbeln.
            it_desc_enc-posnr     =  tl_vbap-posnr.

            if v_check eq abap_true. "Quantidade Remessa = Zero, Zera Desconto.
              it_desc_enc-desc_absoluto  = 0.
            else.
              if ( wg_saldo-zmeng > 0 ).
                it_desc_enc-perc_desc     = ( wg_saldo-total * 100 ) / wg_saldo-zmeng.
                it_desc_enc-desc_absoluto = ( it_desc_enc-perc_desc * vl_desc_abs ) / 100.
              endif.
            endif.

            append it_desc_enc.
          endif.

        endif.

        if v_check eq abap_true.

          if line_exists( it_konv[ kposn = tl_vbap-posnr ] ).
            tl_conditions_in-cond_unit  = switch #( it_konv[ kposn = tl_vbap-posnr ]-kmein when 'TO' then 'KG' ).
            tl_conditions_inx-cond_unit = switch #( it_konv[ kposn = tl_vbap-posnr ]-kmein when 'TO' then abap_true ).
          endif.


          move: tl_vbap-posnr to tl_conditions_in-itm_number,
                '01'          to tl_conditions_in-cond_count,
                'PR00'        to tl_conditions_in-cond_type,
                '0.1'           to tl_conditions_in-cond_value,
*                  1             TO TL_CONDITIONS_IN-CURRENCY,

                tl_vbap-posnr to tl_conditions_inx-itm_number,
                '01'          to tl_conditions_inx-cond_count,
                'PR00'        to tl_conditions_inx-cond_type,
                'X'           to tl_conditions_inx-cond_value,
*                  'X'           TO TL_CONDITIONS_INX-CURRENCY,
                'U'           to tl_conditions_inx-updateflag.

          append: tl_conditions_in,
                  tl_conditions_inx.

          clear: tl_conditions_in, tl_conditions_inx.

          append value #(
                          itm_number = tl_vbap-posnr
                          cond_count = '01'
                          cond_type  = 'RB00'
                          cond_value = 0
                          cond_unit  = tl_conditions_in-cond_unit
*                          CURRENCY   = TL_VBAK-WAERK
                        ) to tl_conditions_in[].

          append value #(
                          itm_number = tl_vbap-posnr
                          cond_count = '01'
                          cond_type  = 'RB00'
                          cond_value = abap_true
                          cond_unit  = abap_true
                          updateflag = 'U'
*                          CURRENCY   = ABAP_TRUE
                        ) to tl_conditions_inx[].


        endif.


        loop at tl_vbep where vbeln eq tl_vbap-vbeln
                          and posnr eq tl_vbap-posnr.

          move: 'U'                to tl_schedule_linesx-updateflag,
                tl_vbap-posnr      to tl_schedule_linesx-itm_number,
                tl_vbep-etenr      to tl_schedule_linesx-sched_line,
                'X'                to tl_schedule_linesx-req_qty,
                tl_vbap-posnr      to tl_schedule_lines-itm_number,
                tl_vbep-etenr      to tl_schedule_lines-sched_line,
                tl_vbep-wmeng      to tl_schedule_lines-req_qty.

          if v_check eq abap_true.
            move: abap_true     to tl_schedule_linesx-req_dlv_bl,
                  '12'          to tl_schedule_lines-req_dlv_bl.
          endif.

          append tl_schedule_linesx.
          append tl_schedule_lines.
          clear: tl_schedule_lines, tl_schedule_linesx.
        endloop.
        "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
        call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
          exporting
            salesdocument    = tl_vbap-vbeln
            order_header_in  = wl_orderheaderin
            order_header_inx = wl_orderheaderinx
            logic_switch     = wl_logic_switch
          tables
            order_item_in    = tl_bapisditm
            order_item_inx   = tl_bapisditmx
            conditions_in    = tl_conditions_in
            conditions_inx   = tl_conditions_inx
            schedule_lines   = tl_schedule_lines
            schedule_linesx  = tl_schedule_linesx
            return           = tl_return.

        clear:wl_return.
        read table tl_return into wl_return with key type = 'E'.

        if sy-subrc ne 0.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = abap_true.

          if ok_code eq 'ENCERRAR'.

            update zsdt0041 set status = 'E'
                            where vbeln eq tl_vbap-vbeln
                              and matnr eq tl_vbap-matnr
                              and inco1 eq tl_vbkd-inco1
                              and werks eq tl_vbap-werks
                              and spart eq tl_vbak-spart.

            it_popup2_aux[] = it_sai_aux[].
            read table it_popup2_aux with key vbeln = tl_vbap-vbeln
                                              posnr = tl_vbap-posnr.
            clear: p_0090, wa_0090.

            p_0090 = zcl_util_=>insert_90old(
              simulador = it_popup2_aux-doc_simulacao
              ordem     = tl_vbap-vbeln
              material  = tl_vbap-matnr
              qtd       = it_popup2_aux-sd_disp
              vlr_venda = it_popup2_aux-kbetr
              uni_venda = it_popup2_aux-kmein
              categoria = 'E'
            ).

*            PERFORM INSERT_ZSDT0090 USING 'E'
*                                          IT_POPUP2_AUX-DOC_SIMULACAO
*                                          IT_POPUP2_AUX-VBELN
*                                          IT_POPUP2_AUX-VBELN2
*                                          IT_POPUP2_AUX-POSNR
*                                          CHANGING P_0090.

*            PERFORM ADD_TABLE USING 'E'.
            perform hedge using 'E'.

            if v_check is not initial.

              zcl_util_=>modify_vlrtot(
                simulador = wa_saida-doc_simulacao
                t_0090    = conv #( it_0090 )
              ).

              update  zsdt0090
                      set estorno      = abap_true
                          usnam_e      = sy-uname
                          data_atual_e = sy-datum
                          hora_atual_e = sy-uzeit
                      where ( vbelv eq wa_saida-vbeln
                           or vbeln eq wa_saida-vbeln )
                         and categoria eq 'O'.

              commit work.

            endif.

          endif.
          zcl_util_=>processa_desc( i_vbeln = tl_vbap-vbeln i_matnr = tl_vbap-matnr ).
        endif.

        free tl_saida_exec.
        clear: tl_saida_exec.

        move: it_sai_aux-inco1 to tl_saida_exec-inco1,
              it_sai_aux-spart to tl_saida_exec-spart,
              it_sai_aux-auart to tl_saida_exec-auart,
              it_sai_aux-werks to tl_saida_exec-werks,
              it_sai_aux-vbeln to tl_saida_exec-vbeln.

        if wl_return is initial.
          append tl_saida_exec.
        else.
          loop at tl_return where type eq 'E'.
            move: tl_return-message to tl_saida_exec-msg.
            append tl_saida_exec.
          endloop.
        endif.

      endif.
    endloop.

    "Ajustar Desconto após encerramento
*    IF ( IT_DESC_ENC[] IS NOT INITIAL ) AND ( H_UCOMM = 'ENCERRAR' ).
*      H_UCOMM = 'MDF_VENC'.
*      PERFORM MDF_VENC USING 'X'.
*      PERFORM DESC_ABS USING 'X'.
*    ENDIF.
    "Fim

*    PERFORM HEDGE USING 'E'.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh .

  check p_extcal is initial.                                "FF #145609

  perform: f_seleciona_dados using 'X', "<-- 10.09.2024 - RAMON - 97513
           f_saida.

  call method cl_grid->refresh_table_display
    exporting
      is_stable = wa_stable.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0500 output.
  set pf-status 'Z001'.
  set titlebar 'Z001'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0500 input.
  case sy-ucomm.
    when 'SAIR'.
      leave to screen 0.
    when 'SEARCH'.

    when 'CONF'.
      if not check_dt is initial.
        perform atualiza_price.
      else.
        message 'Informe o Vencimento, se não houve alteração repita a Data Venc. Atual!' type 'E'.
        exit.
      endif.
      leave to screen 0.
    when others.
      leave to screen 0.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form atualiza_price .

  data: tl_vbak            type table of vbak with header line,
        tl_vbap            type table of vbap with header line,
        tl_vbkd            type table of vbkd with header line,
        tl_vbep            type table of vbep with header line,
        wl_orderheaderin   type bapisdh1,
        wl_orderheaderinx  type bapisdh1x,
        tl_bapisditm       type table of bapisditm with header line,
        tl_bapisditmx      type table of bapisditmx with header line,
        wl_return          type bapiret2,
        tl_schedule_lines  type table of bapischdl with header line,
        tl_schedule_linesx type table of bapischdlx with header line,
        wl_tabix           type sy-tabix,
        tl_zsdt0090        type table of zsdt0090 with header line,
        seq                type zsdt0090-sequencia.

  wl_orderheaderinx-updateflag = 'U'.
  wl_orderheaderin-exrate_fi = wa_0090-kurrf.
  wl_orderheaderinx-exrate_fi = abap_true.
  wl_orderheaderin-fix_val_dy = wa_0090-valdt.
  wl_orderheaderinx-fix_val_dy = abap_true.
  "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = wa_0090-vbelv
      order_header_in  = wl_orderheaderin
      order_header_inx = wl_orderheaderinx
    tables
      return           = tl_return.

  clear:wl_return.
  read table tl_return into wl_return with key type = 'E'.

  if sy-subrc ne 0.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.

    move-corresponding wa_saida to it_popup2_aux.

    select single *
      from zsdt0090
        into @data(gw_0090)
      where vbelv eq @wa_saida-vbeln
        and categoria eq 'C'
    and estorno eq @abap_false.

    if sy-subrc is initial.
      perform hedge using 'T'.
    endif.

    clear: p_0090.
    perform insert_zsdt0090 using 'C'
                                  it_popup2_aux-doc_simulacao
                                  it_popup2_aux-vbeln
                                  it_popup2_aux-vbeln2
                                  it_popup2_aux-posnr
                            changing p_0090.
    im_0090 = p_0090.


    if not im_0090-kurrf is initial
   and not im_0090-valdt is initial.
      perform hedge using 'P'.
    endif.

  endif.

  perform f_exibe_bapi.

endform.
*&---------------------------------------------------------------------*
*&      Module  REFRESH_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module refresh_material input.

  check not gt_return_tab[] is initial.
  clear wg_material.

  read table gt_return_tab index 1.
  condense gt_return_tab-fieldval no-gaps.
  read table tg_material with key id = gt_return_tab-fieldval.
  move-corresponding tg_material to wg_material.

endmodule.

*&---------------------------------------------------------------------*
*&      Form  CRIA_OV
*&---------------------------------------------------------------------*
form cria_ov using p_direcao.

  data:
    tl_texto          type catsxt_longtext_itab,
    wl_texto          type line of catsxt_longtext_itab,
    wl_bape_vbak      type bape_vbak,
    wl_bape_vbakx     type bape_vbakx,
    wl_header_in      type bapisdhd1,
    wl_header_inx     type bapisdhd1x,
    wl_header         like wa_saida,
    wl_posnr          type sy-tabix,
    wl_vlr_covert     type dzmeng,
    tl_items_in       type table of bapisditm     with header line,
    tl_items_inx      type table of bapisditmx    with header line,
    tl_conditions_in  type table of bapicond      with header line,
    tl_conditions_inx type table of bapicondx     with header line,
    tl_schedules_in   type table of bapischdl     with header line,
    tl_schedules_inx  type table of bapischdlx    with header line,
    tl_vbuv           type table of vbuv          with header line,
    tl_return_aux     type table of bapiret2      with header line,
    tl_text_in        type table of bapisdtext    with header line,
    tl_partners       type table of bapiparnr     with header line,
    tl_bapiparex      type table of bapiparex     with header line,
    tl_0048           type table of zsdt0048      with header line,
    wl_header_inx2    like bapisdh1x,
    wl_matkl          type mara-matkl,
    wl_vbeln          type vbak-vbeln,
    wl_fieldname      type dcobjdef-name,
    wl_text           type dd04v-ddtext,
    it_0041           type table of zsdt0041 with header line.

  case p_direcao.
    when 'R' or 'M'.

      if p_direcao ne 'M'.
        it_popup2_aux[] = it_popup2[].
      endif.

      read table it_popup2 index 1.
      move-corresponding it_popup2 to wl_header.

      if p_direcao eq 'M'.
        call function 'CATSXT_SIMPLE_TEXT_EDITOR'
          exporting
            im_title = 'Texto para Ordem de Venda'
          changing
            ch_text  = tl_texto.

        loop at tl_texto into wl_texto.
          tl_text_in-text_line(72) = wl_texto.
          tl_text_in-text_id    = '0002'.
          tl_text_in-langu      = sy-langu.
          tl_text_in-format_col = '/'.
          append tl_text_in.
        endloop.

      endif.

  endcase.

  select *
    from zsdt0048
    into table tl_0048
     for all entries in it_popup2
      where bukrs   eq it_popup2-vkorg
        and vtweg   eq it_popup2-vtweg
  and spart   eq it_popup2-spart.

*  Extension - Campo ZPESAGEM
  clear tl_bapiparex.
  tl_bapiparex-structure     = 'BAPE_VBAK'.



*SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980
  "wl_bape_vbak-zpesagem = '02'.
  wl_bape_vbak-zpesagem = obj_zcl_util_sd->set_tp_pesagem_ov_simulador(
    centro = wl_header-werks
    matnr  = wl_header-matnr
  ).
  tl_bapiparex-valuepart1 = wl_bape_vbak.
  append tl_bapiparex.
  clear tl_bapiparex.

  tl_bapiparex-structure     = 'BAPE_VBAKX'.
  wl_bape_vbakx-zpesagem = 'X'.
  tl_bapiparex-valuepart1 = wl_bape_vbakx.
  append  tl_bapiparex.

*  Cabeçario do Documento
  wl_header_in-sales_org  = wl_header-vkorg.
  wl_header_in-distr_chan = wl_header-vtweg.
  wl_header_in-sales_off  = wl_header-vkbur.
  wl_header_in-sales_grp  = wl_header-vendedor(3).
  concatenate wl_header-cultura wl_header-safra wl_header-doc_simulacao into wl_header_in-purch_no_c separated by '-'.
  wl_header_in-currency   = wl_header-waerk.
  wl_header_in-pymt_meth   = 'P'.

  select single valdt kurrf
   from vbkd
    into (wl_header_in-fix_val_dy, wl_header_in-exrate_fi)
  where vbeln eq wl_header-vbeln.

  case wl_header-tpsim.
    when 'TS'.
      wl_header_in-pmnttrms = 'I001'.
    when 'TT'.
      wl_header_in-pmnttrms = 'I008'.
    when 'AD'.
      wl_header_in-pmnttrms = 'I002'.
    when 'VV' or 'VF' or 'BN'.
      wl_header_in-pmnttrms = 'I003'.
    when 'TV'.
      wl_header_in-pmnttrms = 'I004'.
    when 'VP'.
      wl_header_in-pmnttrms = 'I005'.
    when 'PM'.
      wl_header_in-pmnttrms = 'I006'.
  endcase.

*  CLEAR: wa_ov.
*  READ TABLE it_ov INTO wa_ov INDEX 2.

*---Monta dados de Parceiro
  tl_partners-partn_role = 'AG'.

*  IF P_DIRECAO EQ 'M'.
  tl_partners-partn_numb = wl_header-kunnr.
*  ELSE.
*    TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR2.
*  ENDIF.

  append tl_partners.
  clear tl_partners.

  sort it_popup2[] by posnr.

  loop at it_popup2.

    if p_direcao eq 'M'.
      if sy-tabix ne 1.
        continue.
      endif.
    endif.

    read table tl_return with key type = 'E'.
    if sy-subrc is initial.
      continue.
    endif.

    refresh: tl_items_in, tl_conditions_in, tl_schedules_in.

    wl_header_in-incoterms1 = wl_header-inco1.
    wl_header_in-incoterms2 = wl_header-inco1.
    wl_header_in-division   = wl_header-spart.
    wl_header_in-doc_type   = wl_header-auart.
    wl_matkl                = wg_material-matkl.


    loop at it_popup2 where inco1 eq wl_header_in-incoterms1
                        and spart eq wl_header_in-division
                        and auart eq wl_header_in-doc_type.

      if p_direcao eq 'M'.
        if sy-tabix ne 1.
          continue.
        endif.
      endif.

      read table tl_return with key type = 'E'.
      if sy-subrc is initial.
        continue.
      endif.

      clear: tl_0048.
      if it_popup2-spart eq '03'.
        read table tl_0048
          with key bukrs = wl_header-vkorg
                   vtweg = wl_header-vtweg
                   spart = it_popup2-spart
                   vkbur = wl_header-vkbur.

        move: tl_0048-lgort to tl_items_in-store_loc.
      elseif it_popup2-spart eq '04'.
        read table tl_0048
        with key bukrs   = wl_header-vkorg
                 vtweg   = wl_header-vtweg
                 spart   = it_popup2-spart
                 werks_d = it_popup2-werks.
*                   vkbur   = space.

        move: tl_0048-lgort to tl_items_in-store_loc.
      endif.

      add 1 to wl_posnr.
      tl_items_in-itm_number   = wl_posnr * 10.
      tl_items_in-material     = it_popup2-matnr.

      if ( wl_header_in-doc_type  eq 'ZFTE' or
           wl_header_in-doc_type  eq 'ZSEM' or
           wl_header_in-doc_type  eq 'ZOSM' or
           wl_header_in-doc_type  eq 'ZOFE' )
      and it_popup2-vrkme eq 'TO'.
        tl_items_in-target_qty   = wl_vlr_covert = ( it_popup2-kwmeng * 1000 ).
        tl_items_in-target_qu    = 'KG'.
        tl_items_in-sales_unit   = 'KG'.
      else.
        tl_items_in-target_qty   = wl_vlr_covert = it_popup2-kwmeng.
        tl_items_in-target_qu    = it_popup2-vrkme.
        tl_items_in-sales_unit   = it_popup2-vrkme.
      endif.

*      TL_ITEMS_IN-TARGET_QTY   = WL_VLR_COVERT = IT_POPUP2-KWMENG.

*      TL_ITEMS_IN-TARGET_QU    = IT_POPUP2-VRKME.
*      TL_ITEMS_IN-SALES_UNIT   = IT_POPUP2-VRKME.
*      TL_ITEMS_IN-SALES_UNIT   = IT_POPUP2-KMEIN.
      tl_items_in-usage_ind    = 'I'.
      tl_items_in-plant        = it_popup2-werks.
      tl_items_in-batch        = it_popup2-charg.
      tl_items_in-ship_point   = it_popup2-werks.

      tl_items_in-store_loc    = wl_header-lgort.

*      IF TL_ITEMS_IN-STORE_LOC IS INITIAL.

*        SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980 - comenta rio do IF
*      IF it_popup2-werks EQ '0175'.
**          TL_ITEMS_IN-STORE_LOC    = 'PR01'.
*
** Extension - Campo ZPESAGEM
*        FREE tl_bapiparex.
*        CLEAR tl_bapiparex.
*        tl_bapiparex-structure     = 'BAPE_VBAK'.
*
**        SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980
*wl_bape_vbak-zpesagem = zcl_util_=>set_tp_pesagem_ov_simulador(
*                                                            i_centro = it_popup2-werks
*                                                            i_mtart = zcl_util_=>busca_mtart(
*                                                                                   it_popup2-matnr
*                                                                                  )
*                                                            ).
*
*        tl_bapiparex-valuepart1 = wl_bape_vbak.
*        APPEND tl_bapiparex.
*        CLEAR tl_bapiparex.
*        tl_bapiparex-structure     = 'BAPE_VBAKX'.
*        wl_bape_vbakx-zpesagem = 'X'.
*        tl_bapiparex-valuepart1 = wl_bape_vbakx.
*        APPEND  tl_bapiparex.
*
**        ELSE.
**          TL_ITEMS_IN-STORE_LOC    = 'IN01'.
*      ENDIF.
*      ENDIF.

      tl_items_in-matfrgtgrp   = '00000001'.

      append tl_items_in.

      clear: tl_conditions_in.
      tl_conditions_in-itm_number  = tl_items_in-itm_number .
      tl_conditions_in-currency    = it_popup2-waerk.


      call method zcl_solicitacao_ov=>get_imposto_v2
        exporting
          i_direcao     = 'O'
          i_vbeln       = it_popup2-vbeln
          i_posnr       = it_popup2-posnr
        receiving
          i_coeficiente = data(coeficiente_o).

      call method zcl_solicitacao_ov=>get_imposto_v2
        exporting
          i_direcao     = 'D'
          i_cliente     = wl_header-kunnr
          i_fornecedor  = conv #( |{ it_popup2-werks alpha = in }| )
          i_material    = it_popup2-matnr
          i_tipo_ordem  = wl_header-auart
          i_werks       = it_popup2-werks
        receiving
          i_coeficiente = data(coeficiente_d).

*      IF  COEFICIENTE_O NE COEFICIENTE_D.
      if coeficiente_d is not initial.
        tl_conditions_in-cond_value = it_popup2-kbetr * coeficiente_d.
      else.
        tl_conditions_in-cond_value  = it_popup2-kbetr.
      endif.
*      ELSE.
*        TL_CONDITIONS_IN-COND_VALUE  = IT_POPUP2-KBETR.
*      ENDIF.

      if ( wl_header_in-doc_type eq 'ZFTE' or
           wl_header_in-doc_type eq 'ZSEM' or
           wl_header_in-doc_type eq 'ZOSM' or
           wl_header_in-doc_type eq 'ZOFE')
       and it_popup2-kmein eq 'TO'.
*        TL_CONDITIONS_IN-COND_VALUE  = IT_POPUP2-KBETR.
        tl_conditions_in-cond_unit  = it_popup2-kmein.
      else.
*        TL_CONDITIONS_IN-COND_VALUE  = IT_POPUP2-KBETR.
      endif.

      if ( '02_04' cs it_popup2-spart      ) and
         ( wl_header_in-doc_type eq 'ZRFU' ) and
         ( it_popup2-kmein eq 'TO' ).
        tl_conditions_in-cond_unit  = it_popup2-kmein.
      endif.

*      TL_CONDITIONS_IN-COND_VALUE  = IT_POPUP2-KBETR.
*      TL_CONDITIONS_IN-COND_UNIT   = IT_POPUP2-KMEIN.

*  IF wl_header-tpsim EQ 'VF'.
*    tl_conditions_in-conexchrat  = wa_ov-kursf.
*  ENDIF.
      tl_conditions_in-cond_type   = 'PR00'.

      append tl_conditions_in.

*       ---Monta dados de Parceiro PONTO DE COLETA
      if wl_header-werks is not initial.

        read table tl_partners
          with key partn_role = 'PC'.
        if sy-subrc is initial.
          tl_partners-partn_numb = |{ wl_header-werks alpha = in }|.
          modify tl_partners index sy-tabix.
        else.
          tl_partners-partn_role = 'PC'.
          tl_partners-partn_numb = |{ wl_header-werks alpha = in }|.
          append tl_partners.
        endif.
        clear tl_partners.
      endif.

      clear: tl_schedules_in.

      append value #(
                      itm_number = tl_items_in-itm_number
                      req_qty    = wl_vlr_covert
                      req_dlv_bl = '10'
                    ) to tl_schedules_in.

      clear: tl_items_in, tl_conditions_in, tl_schedules_in, wa_ov.

      delete it_popup2.

    endloop.

* Criar Ordem
    call function 'SD_SALESDOCUMENT_CREATE'
      exporting
        sales_header_in      = wl_header_in
        sales_header_inx     = wl_header_inx
      importing
        salesdocument_ex     = wl_vbeln
      tables
        return               = tl_return
        sales_items_in       = tl_items_in
        sales_items_inx      = tl_items_inx
        sales_partners       = tl_partners
        sales_schedules_in   = tl_schedules_in
        sales_schedules_inx  = tl_schedules_inx
        sales_conditions_in  = tl_conditions_in
        sales_conditions_inx = tl_conditions_inx
        sales_text           = tl_text_in
        extensionin          = tl_bapiparex.

*   Verirfica se a ordem foi criada.

    refresh:it_msg.
    if not wl_vbeln is initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

* VERIFICA SE EXISTEM OCORRÊNCIAS DE NÃO CONFORMIDADE NA CRIAÇÃO DA ORDEM
      refresh: tl_vbuv.
      select *
        from vbuv
        into table tl_vbuv
      where vbeln eq wl_vbeln.

      if not tl_vbuv[] is initial.

        loop at tl_vbuv.
          clear: tl_return, wl_fieldname, wl_text.

          wl_fieldname = tl_vbuv-fdnam.

          call function 'RM_DDIC_TEXTS_GET'
            exporting
              i_name                = wl_fieldname
              i_type                = 'DTEL'
              i_langu               = sy-langu
            importing
              e_ddtxt               = wl_text
            exceptions
              objtype_not_supported = 1
              illegal_input         = 2
              others                = 3.
          if sy-subrc <> 0.
            concatenate 'Existem campos incompletos na OV:' tl_vbuv-fdnam into tl_return-message separated by space.
          else.
            concatenate 'Existem campos incompletos na OV:' wl_text into tl_return-message separated by space.
          endif.

          tl_return-type = 'E'.
          append tl_return.
        endloop.

        refresh: tl_return_aux.
        wl_header_inx2-updateflag = 'D'.
        "*---> 19/07/2023 - Migração S4 - LO
        call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
          exporting
            salesdocument    = wl_vbeln
            order_header_inx = wl_header_inx2
          tables
            return           = tl_return_aux.

        read table tl_return_aux  with key type = 'E'.
        if sy-subrc ne 0.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

        endif.

        loop at tl_return_aux.
          move-corresponding tl_return_aux to tl_return.
          append tl_return.
        endloop.

        clear: wl_vbeln.
      else.
*       RECEBE A NOVA ORDERM CRIADA PARA INSERIR NA ALV DE MSG

        if not diferenca_qt is initial.
*          WSB DIFERENÇA

          select single *
            from vbap
            into @data(wvbap)
          where vbeln eq @wl_vbeln.

          it_0041-doc_simulacao = wl_header-doc_simulacao.
          it_0041-vbeln = wvbap-vbeln.
          it_0041-posnr = wvbap-posnr.
          it_0041-matnr = wvbap-matnr.
          it_0041-desc_absoluto = diferenca_qt.
          append it_0041.

          if not it_0041 is initial.

            call function 'ZSDMF001_ATUALI_OV_SIMULADOR'
              tables
                ti_itens_ov       = it_0041
              exceptions
                ov_nao_encontrada = 1
                others            = 2.

            loop at it_0041 into data(wg_41).
              clear: p_0090, wa_0090.
              perform insert_zsdt0090 using 'OO'
                                          wg_41-doc_simulacao
                                          ' '         "NEW
                                          wg_41-vbeln "OLD
                                          wg_41-matnr
                                          changing p_0090.
            endloop.
            commit work.
          endif.
        endif.

        loop at it_popup2_aux.
          it_popup2_aux-vbeln = wl_vbeln.
          wa_saida-vbeln = wl_vbeln.
          modify it_popup2_aux index sy-tabix transporting vbeln.
          clear: p_0090, wa_0090.
          perform insert_zsdt0090
                            using p_direcao
                                  it_popup2_aux-doc_simulacao
                                  it_popup2_aux-vbeln
                                  it_popup2_aux-vbeln2
                                  it_popup2_aux-matnr
                            changing p_0090.

          case p_direcao.
            when 'M' or 'R'.
              perform hedge using p_direcao.
          endcase.

        endloop.

      endif.

    else.

      tl_return-message = 'Ov não foi Criado'.
      tl_return-type = 'E'.
      append tl_return.

    endif.

  endloop.

  it_popup2[] = it_popup2_aux[].

endform.
*&---------------------------------------------------------------------*
*&      Form  INSERT_ZSDT0090
*&---------------------------------------------------------------------*
form insert_zsdt0090 using p_direcao p_simulador p_vbeln p_vbelv p_matnr changing p_0090.

  data: obj_0094  type ref to zcl_taxa_curva.
  create object obj_0094.

  data: seq type zsdt0090-sequencia.
  data: it_vbap type table of vbap with header line,
        it_vbak type table of vbak with header line,
        it_vbkd type table of vbkd with header line,
        it_konv type table of konv with header line,
        qtd     type dzmeng.

  data: wa_0116     type zsdt0116,
        wa_zsdt0116 type zsdt0116.

  free: it_vbap, it_vbak, it_vbkd.

  data: total type vbap-kwmeng.
  data: matnr type matnr,
        charg type charg_d.

  data: wa_0090_aux   type zsdt0090.

*** Stefanini - IR195082 - 03/02/2025 - LAZAROSR - Início de Alteração
  data:
    v_qtd_busca       type i value 10,
    v_segundos_espera type i value 5,
    c_qtd_ordens      type i value 2.
*** Stefanini - IR195082 - 03/02/2025 - LAZAROSR - Fim de Alteração

  read table tl_return with key type = 'E'.

  check not sy-subrc is initial.

  if not p_vbeln is initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = p_vbeln
      importing
        output = p_vbeln.
  endif.

  if not p_vbelv is initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = p_vbelv
      importing
        output = p_vbelv.
  endif.

*  BREAK-POINT.


  case p_direcao.
    when 'Y' or 'W'.

      data: cont type i.

      do.
        if p_matnr+cont(1) eq '#'.
          charg = p_matnr(cont).
          add 1 to cont.
          matnr = p_matnr+cont(18).
          shift charg left deleting leading '0'.
          exit.
        endif.
        add 1 to cont.
      enddo.

  endcase.

  case p_direcao.
    when ''  or 'R' or 'M' or
         'UN' or'UV' or 'Y' or 'W' or
         'V' or 'O' or 'OO' or 'F'.

      select count(*)
          from zsdt0090
            into seq
      where doc_simulacao eq p_simulador.


*"// Analise da Lentidão na Troca de Data de Vencimanto na OV
*"// Aplicar ação de espera somente para Transferencia de acordo com o IR195082
*"// inicio WBARBOSA
*** Stefanini - IR195082 - 03/02/2025 - LAZAROSR - Início de Alteração
      if p_direcao is not initial and p_direcao ne 'R'.

        select *
          from vbak
          into table it_vbak
        where vbeln in (p_vbeln,p_vbelv). "(NEW,OLD)

      else.

        do v_qtd_busca times.

          select  *
          from vbak
          into table it_vbak
          where vbeln in (p_vbeln,p_vbelv). "(NEW,OLD)

          if sy-subrc          is initial
          and lines( it_vbak ) eq c_qtd_ordens.

            " Se encontrou a ordem antiga e a nova, pode seguir
            exit.

          else.
            zcl_util_=>indicator( |Aguardando Conclusão da Nova O.V.: { p_vbeln } -  1 de { v_qtd_busca }| ).
            wait up to v_segundos_espera seconds.
          endif.

        enddo.
      endif.
*** Stefanini - IR195082 - 03/02/2025 - LAZAROSR - Fim de Alteração
*"// Fim WBARBOSA

      if not it_vbak[] is initial.

        select * from vbap
          into table it_vbap
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        case p_direcao.
          when 'UN' or 'UV'.

            sort it_vbap by posnr descending.
            read table it_vbap index 1.
            delete it_vbap where posnr ne it_vbap-posnr.

            select * from vbap
              appending table it_vbap
              for all entries in it_vbak
                where vbeln eq it_vbak-vbeln
            and posnr eq p_matnr. " A variavel P_MATNR recebe o POSNR somente neste caso de Update da Troca de Material

          when 'Y' or 'W'.

            select * from vbap
              into table it_vbap
              for all entries in it_vbak
                where vbeln eq it_vbak-vbeln
            and matnr eq matnr.

            if lines( it_vbap[] ) > 2.
*              LOOP IT_VBAP INTO DATA(WC_VBAP) WHERE VBELN EQ P_VBELV.
*                IF WC_VBAP-CHARG IS NOT INITIAL.
*                    DELETE IT_VBAP WHERE VBELN EQ P_VBELV AND CHARG NE CHARG.
*                ENDIF.
*              ENDLOOP.

              delete it_vbap where vbeln eq p_vbelv and charg ne charg.
              delete it_vbap where vbeln eq p_vbeln and charg ne charg.
            endif.

          when others.

            delete it_vbap where vbeln eq p_vbelv and matnr ne p_matnr.

            if p_direcao ne 'M'.
              read table it_vbap with key vbeln = p_vbelv matnr = p_matnr.
              delete it_vbap where matnr ne it_vbap-matnr.
            endif.

        endcase.

        clear it_vbap.

        select * from vbkd
          into table it_vbkd
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
        into corresponding fields of table @it_konv
        for all entries in @it_vbak
          where knumv eq @it_vbak-knumv
        and kschl in ('PR00').

      endif.

      add 1 to seq.

      wa_0090-doc_simulacao = p_simulador.
      wa_0090-sequencia     = seq.

      if p_direcao ne 'UN' and p_direcao ne 'UV'.
        wa_0090-categoria     = p_direcao.
      else.
        wa_0090-categoria     = 'M'.
      endif.

      wa_0090-usnam         = sy-uname.
      wa_0090-data_atual    = sy-datum.
      wa_0090-hora_atual    = sy-uzeit.

***OLD****>>>>>
      if p_direcao ne 'UN'.

        clear: it_vbap, it_vbkd, it_vbap.
        read table it_vbak with key vbeln = p_vbelv.
        read table it_vbkd with key vbeln = it_vbak-vbeln.

        if p_direcao ne 'UN' and p_direcao ne 'UV'.
          read table it_vbap with key vbeln = it_vbak-vbeln.
        else.
          read table it_vbap with key vbeln = it_vbak-vbeln
                                      posnr = p_matnr.
          delete it_vbap where vbeln eq it_vbak-vbeln
                                    and posnr eq p_matnr.
        endif.

        read table it_konv with key knumv = it_vbak-knumv
                                    kposn = it_vbap-posnr.

* ORDEM VELHA

        wa_0090-auartv        = it_vbak-auart.
        wa_0090-vbelv         = it_vbap-vbeln.
        wa_0090-spartv        = it_vbap-spart.
        wa_0090-kunnrv        = it_vbak-kunnr.
        wa_0090-matklv        = it_vbap-matkl.
        wa_0090-inco1v        = it_vbkd-inco1.
        wa_0090-werksv        = it_vbap-werks.
        wa_0090-chargv        = it_vbap-charg.
        wa_0090-posnv         = it_vbap-posnr.
        wa_0090-matnrv        = it_vbap-matnr.
        wa_0090-kurrf         = it_vbkd-kurrf.

        if it_vbak-auart eq 'ZTRI'.
          wa_0090-zmengv      = 0.
        elseif it_vbak-auart eq 'ZFUT'.
          wa_0090-zmengv      = it_vbap-zmeng * -1.
        else.
          read table it_ov into wa_ov with key vbeln = p_vbelv
                                               matnr = p_matnr.
          if sy-subrc is initial.
            wa_0090-zmengv      = wa_ov-zmeng * -1.
          endif.
        endif.

*      "// Marca o Item com Imposto
        wa_0090-flag_impostov = cond #( when it_vbap-mwsbp is not initial then abap_true else abap_false ).

*     "// get do preço da OV/Iten
        wa_0090-netprv = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kbetr.
*     "// get da unidade de Medida do Preço
        wa_0090-kmeinv = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kmein.

        wa_0090-ziemev        = it_vbap-zieme.
*        WA_0090-NETPRV        = IT_KONV-KBETR.
*        WA_0090-KMEINV        = IT_KONV-KMEIN.

        case p_direcao.
          when 'O'.
            try.
                wa_0090-desc_absoluto = it_itens_id[ doc = p_simulador
                                                   vbeln = it_vbap-vbeln
                                                   posnr = it_vbap-posnr ]-vlr_dif.

*                WA_0090-DESC_ABSOLUTO_LQ = ZCL_UTIL_=>CHECK_DESC_ABS(
*                  _VBELN = IT_VBAP-VBELN
*                  _POSNR = IT_VBAP-POSNR
*                  DES = CONV #( WA_0090-DESC_ABSOLUTO ) ).

              catch cx_sy_itab_line_not_found.
            endtry.
          when 'OO'.

            select single kbetr from v_konv " ---> S4 Migration - 10/07/2023 - DG
            into @wa_0090-desc_absoluto
              where knumv eq @it_vbak-knumv
                and kposn eq @it_vbap-posnr
            and kschl in ('RB00').

          when 'V'.
            clear wa_0090.

            wa_0090-doc_simulacao = p_simulador.
            wa_0090-sequencia     = seq.
            wa_0090-categoria     = p_direcao.

            wa_0090-usnam         = sy-uname.
            wa_0090-data_atual    = sy-datum.
            wa_0090-hora_atual    = sy-uzeit.

            wa_0090-vbelv         = it_vbap-vbeln.
            wa_0090-auartv        = it_vbak-auart.
            wa_0090-valdt         = var_venc_new.

        endcase.

*<<<<<***OLD****
*        ALIMENTA SOMENTE OS CAMPOS VELHOS E NÃO FAZ O NOVO EM CADO DE DIREÇÃO 'U'
      endif.

      check p_direcao ne 'UV'.
**************************************************************

*****NEW>>>>>>
      clear: it_vbak, it_vbap, it_vbkd, it_konv.
      read table it_vbak with key vbeln = p_vbeln.
      read table it_vbkd with key vbeln = it_vbak-vbeln.

      if p_direcao ne 'UN'.

        if it_vbak-auart eq 'ZTRI'.
          wa_0090_aux = p_0090.
          "Ler com item da ordem ZTRI Criada
          read table it_vbap with key vbeln = it_vbak-vbeln
                                      posnr = wa_0090_aux-posnn.
        else.

          read table it_vbap with key vbeln = it_vbak-vbeln.
        endif.
      else.
        read table it_vbap with key vbeln = it_vbak-vbeln.
      endif.

      read table it_konv with key knumv = it_vbak-knumv
                                  kposn = it_vbap-posnr.

* ORDEM NOVA
      wa_0090-auart         = it_vbak-auart.
      wa_0090-vbeln         = it_vbap-vbeln.
      wa_0090-posnn         = it_vbap-posnr.
      wa_0090-spart         = it_vbap-spart.

      case it_vbak-auart.
        when 'ZTRI'.
          wa_0090-zmeng         = it_vbap-zmeng.

        when 'ZFUT'.

          wa_0090-zmeng         = it_vbap-zmeng.

*         Quando a transferencia Total alteramos o preço da Ordem Principal
          wa_0090-netprv = it_konv-kbetr.
          wa_0090-kmeinv = it_konv-kmein.

        when others.
          wa_0090-zmeng         = it_vbap-kwmeng.

          case p_direcao.
            when 'Y'.
              wa_0090-zmeng = wa_0090-zmeng * -1.
            when 'W'.
              wa_0090-zmeng = it_vbap-zmeng.
          endcase.

      endcase.

      case p_direcao.
        when 'R'.
          wa_0090-zmengv      = it_vbap-kwmeng * -1.
        when 'M' or 'UN'.
          wa_0090-zmengv      = it_popup2_aux-qt_tran * -1.
      endcase.

      if it_vbak-auart eq 'ZTRI'.
        wa_0090-zmengv      = 0.
      endif.

*     "// get do preço da OV/Iten
      wa_0090-netpr = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kbetr.
*     "// get da unidade de Medida do Preço
      wa_0090-kmein = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kmein.

      wa_0090-zieme         = it_vbap-zieme.
*      WA_0090-NETPR         = IT_KONV-KBETR.
*      WA_0090-KMEIN         = IT_KONV-KMEIN.
      wa_0090-charg         = it_vbap-charg.
      wa_0090-matnr         = it_vbap-matnr.
      wa_0090-matkl         = it_vbap-matkl.
      wa_0090-inco1         = it_vbkd-inco1.
      wa_0090-werks         = it_vbap-werks.
      wa_0090-kunnr         = it_vbak-kunnr.
      wa_0090-kurrf         = it_vbkd-kurrf.

*    "// Marca o Item com Imposto
      wa_0090-flag_imposto = cond #( when it_vbap-mwsbp is not initial then abap_true else abap_false ).

      if wa_0090-auart eq 'ZREM'.
        perform save_text using p_simulador seq.
      endif.

      insert into zsdt0090  values wa_0090.
      if sy-subrc is initial.

*** Stefanini - IR195082 - 03/02/2025 - LAZAROSR - Início de Alteração
        commit work and wait.
*** Stefanini - IR195082 - 03/02/2025 - LAZAROSR - Fim de Alteração

        concatenate 'OV. ' it_vbap-vbeln  'foi Incluida na tabela ZSDT0090'  into  tl_return-message separated by space.
        tl_return-type = 'W'.
        append tl_return.
      endif.
**************************************************
      " inserindo registro na ZSDT0116, Tabela de Aprovação de embarque (Transção ZSDT0100)

      clear: wa_0116, wa_zsdt0116.

      if ( wa_0090-categoria eq ' ' ).
*      OR ( WA_0090-CATEGORIA EQ 'M' )
*      OR ( WA_0090-CATEGORIA EQ 'D' AND WA_0090-KUNNR EQ WA_0090-KUNNRV ).

*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Inicio
        perform insere_zsdt0116 using wa_0090-vbeln wa_0090-vbelv wa_0090-posnn.
*        SELECT SINGLE *
*          FROM zsdt0116
*          INTO wa_zsdt0116
*          WHERE vbeln   EQ wa_0090-vbelv
*            AND status  EQ ' '
*            AND status_workflow IN ( ' ', 'A' ).
*
*        IF sy-subrc IS INITIAL.
*
*          CALL FUNCTION 'NUMBER_GET_NEXT'
*            EXPORTING
*              nr_range_nr = '01'
*              object      = 'ZSEQ_0116_'
*            IMPORTING
*              number      = wa_0116-seq.
*
*          IF wa_0116-seq IS INITIAL.
*            ROLLBACK WORK.
*            MESSAGE 'Objeto numeração ZSEQ_0116_ não configurado!' TYPE 'S'.
*            RETURN.
*          ENDIF.
*
*          wa_0116-vbeln      =  wa_0090-vbeln.
*          wa_0116-posnr      =  wa_0090-posnn.
*          wa_0116-user_apv   =  wa_zsdt0116-user_apv.
*          wa_0116-dt_apv     =  wa_zsdt0116-dt_apv.
*          wa_0116-hr_apv     =  wa_zsdt0116-hr_apv.
*
*          INSERT INTO zsdt0116  VALUES wa_0116.
*          IF sy-subrc IS INITIAL.
*            CONCATENATE 'OV. ' it_vbap-vbeln  'foi Incluida na tabela ZSDT0116'  INTO  tl_return-message SEPARATED BY space.
*            tl_return-type = 'W'.
*            APPEND tl_return.
*          ENDIF.
*        ENDIF.
*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Fim

      endif.
**************************************************

    when others.

      select count(*)
    from zsdt0090
      into seq
      where doc_simulacao eq it_popup2_aux-doc_simulacao.

      select  * from vbak
        into table it_vbak
      where vbeln eq it_popup2_aux-vbeln.

      if not it_vbak[] is initial.
        select * from vbap
          into table it_vbap
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        select * from vbkd
          into table it_vbkd
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
         into corresponding fields of table @it_konv
         for all entries in @it_vbak
           where knumv eq @it_vbak-knumv
        and kschl in ('PR00').

      endif.

      case p_direcao.
        when 'C' or 'E' or 'A'.
          delete it_vbap where posnr ne it_popup2_aux-posnr.
        when 'M' or 'R'.
          sort it_vbap by posnr descending.
          read table it_vbap index 1.
          delete it_vbap where posnr ne it_vbap-posnr.
      endcase.

      loop at it_vbak.
        loop at it_vbap where vbeln eq it_vbak-vbeln and
                              matnr eq it_popup2_aux-matnr.
          clear it_vbkd.
          read table it_vbkd with key vbeln = it_vbap-vbeln.
          read table it_konv with key knumv = it_vbak-knumv
                                      kposn = it_vbap-posnr.

          add 1 to seq.

          wa_0090-doc_simulacao = it_popup2_aux-doc_simulacao.
          wa_0090-sequencia     = seq.

          case p_direcao.
            when 'C' or 'E' or 'A'.
            when others.
              wa_0090-auart         = it_vbak-auart.
              wa_0090-vbeln         = it_vbap-vbeln.
              wa_0090-posnn         = it_vbap-posnr.
              wa_0090-spart         = it_vbap-spart.
              wa_0090-zmeng         = it_vbap-kwmeng.
              wa_0090-zieme         = it_vbap-zieme.

              data(coeficiente) = zcl_solicitacao_ov=>get_imposto(
                _cliente    = it_vbak-kunnr
                _fornecedor = conv #( |{ it_vbap-werks alpha = in }| )
                _material   = it_vbap-matnr
                _tipo_ordem = it_vbak-auart
                _direcao    = 'D'
                _werks      = it_vbap-werks  "<<RIM-SKM-IR120585-23.12.22
              ).

              wa_0090-netpr         = cond #( when coeficiente is initial then it_konv-kbetr else it_konv-kbetr / coeficiente ).

              wa_0090-kmein         = it_konv-kmein.
              wa_0090-charg         = it_vbap-charg.
              wa_0090-matnr         = it_vbap-matnr.
              wa_0090-matkl         = it_vbap-matkl.
              wa_0090-inco1         = it_vbkd-inco1.
              wa_0090-werks         = it_vbap-werks.
              wa_0090-kunnr         = it_vbak-kunnr.
          endcase.

          wa_0090-auartv        = it_popup2_aux-auart.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = it_popup2_aux-vbeln2
            importing
              output = wa_0090-vbelv.

          wa_0090-spartv         = it_popup2_aux-spart.
          wa_0090-kunnrv         = it_popup2_aux-kunnr.

          if 'M' eq p_direcao.
            wa_0090-matklv         = lv_matkl.
          else.
            wa_0090-matklv         = it_vbap-matkl.
          endif.

          wa_0090-inco1v         = it_popup2_aux-inco1.
          wa_0090-werksv         = it_popup2_aux-werks.
          wa_0090-chargv         = it_popup2_aux-charg.

          if 'C' ne p_direcao.
            wa_0090-posnv         = it_popup2_aux-posnr. " foi incluido o POSNR por conta do Redistirbuição estava ITENS
            wa_0090-matnrv         = it_popup2_aux-matnr.

            if 'E' eq p_direcao.
              wa_0090-zmengv         = it_popup2_aux-sd_disp.
            else.
              wa_0090-zmengv         = it_popup2_aux-qt_tran.
            endif.
            wa_0090-ziemev         = it_popup2_aux-vrkme.


            case p_direcao.
              when 'D'.
                wa_0090-zmengv = wa_0090-zmengv * -1.
              when 'R'.
                wa_0090-zmengv = wa_0090-zmeng * -1.
            endcase.

            clear it_popup.
            read table it_popup with key vbeln2 = it_popup2_aux-vbeln2 posnr = it_popup2_aux-posnr.
            if sy-subrc is initial.
              wa_0090-netprv        = it_popup-kbetr.
            else.
              wa_0090-netprv        = it_popup2_aux-kbetr.
            endif.
            wa_0090-kmeinv         = it_popup2_aux-kmein.

          endif.

          wa_0090-categoria     = p_direcao.
          wa_0090-usnam         = sy-uname.
          wa_0090-data_atual    = sy-datum.
          wa_0090-hora_atual    = sy-uzeit.

          insert into zsdt0090  values wa_0090.
          if sy-subrc is initial.
            concatenate 'OV. ' it_vbap-vbeln  'foi Incluida na Tabela ZSDT0090'  into  tl_return-message separated by space.
            tl_return-type = 'W'.
            append tl_return.
          endif.
**************************************************
          " inserindo registro na ZSDT0116, Tabela de Aprovação de embarque (Transção ZSDT0100)
          clear: wa_0116, wa_zsdt0116.

          if ( wa_0090-categoria eq ' ' ).
*      OR ( WA_0090-CATEGORIA EQ 'M' )
*      OR ( WA_0090-CATEGORIA EQ 'D' AND WA_0090-KUNNR EQ WA_0090-KUNNRV ).

*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Inicio
            perform insere_zsdt0116 using wa_0090-vbeln wa_0090-vbelv wa_0090-posnn.
*            SELECT SINGLE * FROM zsdt0116 INTO wa_zsdt0116
*              WHERE vbeln   EQ wa_0090-vbelv
*                AND status  EQ ' '
*                AND status_workflow IN ( ' ', 'A' ).
*
*            IF sy-subrc IS INITIAL.
*
*              CALL FUNCTION 'NUMBER_GET_NEXT'
*                EXPORTING
*                  nr_range_nr = '01'
*                  object      = 'ZSEQ_0116_'
*                IMPORTING
*                  number      = wa_0116-seq.
*
*              IF wa_0116-seq IS INITIAL.
*                ROLLBACK WORK.
*                MESSAGE 'Objeto numeração ZSEQ_0116_ não configurado!' TYPE 'S'.
*                RETURN.
*              ENDIF.
*
*              wa_0116-vbeln      =  wa_0090-vbeln.
*              wa_0116-posnr      =  wa_0090-posnn.
*              wa_0116-user_apv   =  wa_zsdt0116-user_apv.
*              wa_0116-dt_apv     =  wa_zsdt0116-dt_apv.
*              wa_0116-hr_apv     =  wa_zsdt0116-hr_apv.
*
*              INSERT INTO zsdt0116  VALUES wa_0116.
*              IF sy-subrc IS INITIAL.
*                CONCATENATE 'OV. ' it_vbap-vbeln  'foi Incluida na tabela ZSDT0116'  INTO  tl_return-message SEPARATED BY space.
*                tl_return-type = 'W'.
*                APPEND tl_return.
*              ENDIF.
*            ENDIF.
*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Fim
          endif.
*****************************************************
        endloop.
      endloop.

  endcase.

  move wa_0090 to p_0090.

  clear: wa_0090, wa_ov.
  free: it_vbap, it_vbak, it_vbkd.

endform.
*&---------------------------------------------------------------------*
*&      Form  INSERT_ZSDT0090
*&---------------------------------------------------------------------*
form f_insert_zsdt0090_2 using p_direcao
                      changing cs_values_popup type zsds078
                               cs_estorno type zsdt0090
                               cs_0090 type zsdt0090.

  data: obj_0094  type ref to zcl_taxa_curva.

  create object obj_0094.

  data: seq type zsdt0090-sequencia.

  data: it_vbap type table of vbap with header line,
        it_vbak type table of vbak with header line,
        it_vbkd type table of vbkd with header line,
        it_konv type table of konv with header line,
        qtd     type dzmeng.

  data: wa_0116     type zsdt0116,
        wa_zsdt0116 type zsdt0116.

  free: it_vbap, it_vbak, it_vbkd.

  data: total type vbap-kwmeng.
  data: matnr type matnr,
        charg type charg_d.

  data: wa_0090_aux   type zsdt0090.

  read table tl_return with key type = 'E'.

  check not sy-subrc is initial.

  if not cs_values_popup-vbeln is initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cs_values_popup-vbeln
      importing
        output = cs_values_popup-vbeln.
  endif.

  if not cs_values_popup-vbelv is initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cs_values_popup-vbelv
      importing
        output = cs_values_popup-vbelv.
  endif.

*  BREAK-POINT.


  case p_direcao.
    when 'Y' or 'W'.

      data: cont type i.

      do.
        if cs_values_popup-matnr+cont(1) eq '#'.
          charg = cs_values_popup-matnr(cont).
          add 1 to cont.
          matnr = cs_values_popup-matnr+cont(18).
          shift charg left deleting leading '0'.
          exit.
        endif.
        add 1 to cont.
      enddo.

  endcase.

  case p_direcao.
    when ''  or 'R' or 'M' or
         'UN' or'UV' or 'Y' or 'W' or
         'V' or 'O' or 'OO' or 'F'.

      select count(*)
          from zsdt0090
            into seq
      where doc_simulacao eq cs_values_popup-doc_simulacao.

      select  * from vbak
        into table it_vbak
      where vbeln in (cs_values_popup-vbeln,cs_values_popup-vbelv). "(NEW,OLD)

      if not it_vbak[] is initial.

        select * from vbap
          into table it_vbap
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        case p_direcao.
          when 'UN' or 'UV'.

            sort it_vbap by posnr descending.
            read table it_vbap index 1.
            delete it_vbap where posnr ne it_vbap-posnr.

            select * from vbap
              appending table it_vbap
              for all entries in it_vbak
                where vbeln eq it_vbak-vbeln
            and posnr eq cs_values_popup-matnr. " A variavel cs_values_popup-matnr recebe o POSNR somente neste caso de Update da Troca de Material

          when 'Y' or 'W'.

            select * from vbap
              into table it_vbap
              for all entries in it_vbak
                where vbeln eq it_vbak-vbeln
            and matnr eq matnr.

            if lines( it_vbap[] ) > 2.
*              LOOP IT_VBAP INTO DATA(WC_VBAP) WHERE VBELN EQ cs_values_popup-vbelv.
*                IF WC_VBAP-CHARG IS NOT INITIAL.
*                    DELETE IT_VBAP WHERE VBELN EQ cs_values_popup-vbelv AND CHARG NE CHARG.
*                ENDIF.
*              ENDLOOP.

              delete it_vbap where vbeln eq cs_values_popup-vbelv and charg ne charg.
              delete it_vbap where vbeln eq cs_values_popup-vbeln and charg ne charg.
            endif.

          when others.

            delete it_vbap where vbeln eq cs_values_popup-vbelv and matnr ne cs_values_popup-matnr.

            if p_direcao ne 'M'.
              read table it_vbap with key vbeln = cs_values_popup-vbelv matnr = cs_values_popup-matnr.
              delete it_vbap where matnr ne it_vbap-matnr.
            endif.

        endcase.

        clear it_vbap.

        select * from vbkd
          into table it_vbkd
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
        into corresponding fields of table @it_konv
        for all entries in @it_vbak
          where knumv eq @it_vbak-knumv
        and kschl in ('PR00').

      endif.

      add 1 to seq.

      wa_0090-doc_simulacao = cs_values_popup-doc_simulacao.
      wa_0090-sequencia     = seq.

      if p_direcao ne 'UN' and p_direcao ne 'UV'.
        wa_0090-categoria     = p_direcao.
      else.
        wa_0090-categoria     = 'M'.
      endif.

      wa_0090-usnam         = sy-uname.
      wa_0090-data_atual    = sy-datum.
      wa_0090-hora_atual    = sy-uzeit.

***OLD****>>>>>
      if p_direcao ne 'UN'.

        clear: it_vbap, it_vbkd, it_vbap.
        read table it_vbak with key vbeln = cs_values_popup-vbelv.
        read table it_vbkd with key vbeln = it_vbak-vbeln.

        if p_direcao ne 'UN' and p_direcao ne 'UV'.
          read table it_vbap with key vbeln = it_vbak-vbeln.
        else.
          read table it_vbap with key vbeln = it_vbak-vbeln
                                      posnr = cs_values_popup-matnr.
          delete it_vbap where vbeln eq it_vbak-vbeln
                                    and posnr eq cs_values_popup-matnr.
        endif.

        read table it_konv with key knumv = it_vbak-knumv
                                    kposn = it_vbap-posnr.

* ORDEM VELHA

        wa_0090-auartv        = it_vbak-auart.
        wa_0090-vbelv         = it_vbap-vbeln.
        wa_0090-spartv        = it_vbap-spart.
        wa_0090-kunnrv        = it_vbak-kunnr.
        wa_0090-matklv        = it_vbap-matkl.
        wa_0090-inco1v        = it_vbkd-inco1.
        wa_0090-werksv        = it_vbap-werks.
        wa_0090-chargv        = it_vbap-charg.
        wa_0090-posnv         = it_vbap-posnr.
        wa_0090-matnrv        = it_vbap-matnr.
        wa_0090-kurrf         = it_vbkd-kurrf.

        if it_vbak-auart eq 'ZTRI'.
          wa_0090-zmengv      = 0.
        elseif it_vbak-auart eq 'ZFUT'.
          wa_0090-zmengv      = it_vbap-zmeng * -1.
        else.
          read table it_ov into wa_ov with key vbeln = cs_values_popup-vbelv
                                               matnr = cs_values_popup-matnr.
          if sy-subrc is initial.
            wa_0090-zmengv      = wa_ov-zmeng * -1.
          endif.
        endif.

*      "// Marca o Item com Imposto
        wa_0090-flag_impostov = cond #( when it_vbap-mwsbp is not initial then abap_true else abap_false ).

*     "// get do preço da OV/Iten
        wa_0090-netprv = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kbetr.
*     "// get da unidade de Medida do Preço
        wa_0090-kmeinv = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kmein.

        wa_0090-ziemev        = it_vbap-zieme.
*        WA_0090-NETPRV        = IT_KONV-KBETR.
*        WA_0090-KMEINV        = IT_KONV-KMEIN.

        case p_direcao.
          when 'O'.
            try.
                wa_0090-desc_absoluto = it_itens_id[ doc = cs_values_popup-doc_simulacao
                                                   vbeln = it_vbap-vbeln
                                                   posnr = it_vbap-posnr ]-vlr_dif.

*                WA_0090-DESC_ABSOLUTO_LQ = ZCL_UTIL_=>CHECK_DESC_ABS(
*                  _VBELN = IT_VBAP-VBELN
*                  _POSNR = IT_VBAP-POSNR
*                  DES = CONV #( WA_0090-DESC_ABSOLUTO ) ).

              catch cx_sy_itab_line_not_found.
            endtry.
          when 'OO'.

            select single kbetr from v_konv " ---> S4 Migration - 10/07/2023 - DG
            into @wa_0090-desc_absoluto
              where knumv eq @it_vbak-knumv
                and kposn eq @it_vbap-posnr
            and kschl in ('RB00').

          when 'V'.
            clear wa_0090.

            wa_0090-doc_simulacao = cs_values_popup-doc_simulacao.
            wa_0090-sequencia     = seq.
            wa_0090-categoria     = p_direcao.

            wa_0090-usnam         = sy-uname.
            wa_0090-data_atual    = sy-datum.
            wa_0090-hora_atual    = sy-uzeit.

            wa_0090-vbelv         = it_vbap-vbeln.
            wa_0090-auartv        = it_vbak-auart.
            wa_0090-valdt         = var_venc_new.

        endcase.

*<<<<<***OLD****
*        ALIMENTA SOMENTE OS CAMPOS VELHOS E NÃO FAZ O NOVO EM CADO DE DIREÇÃO 'U'
      endif.

      check p_direcao ne 'UV'.
**************************************************************

*****NEW>>>>>>
      clear: it_vbak, it_vbap, it_vbkd, it_konv.
      read table it_vbak with key vbeln = cs_values_popup-vbeln.
      read table it_vbkd with key vbeln = it_vbak-vbeln.

      if p_direcao ne 'UN'.

        if it_vbak-auart eq 'ZTRI'.
          wa_0090_aux = p_0090.
          "Ler com item da ordem ZTRI Criada
          read table it_vbap with key vbeln = it_vbak-vbeln
                                      posnr = wa_0090_aux-posnn.
        else.

          read table it_vbap with key vbeln = it_vbak-vbeln.
        endif.
      else.
        read table it_vbap with key vbeln = it_vbak-vbeln.
      endif.

      read table it_konv with key knumv = it_vbak-knumv
                                  kposn = it_vbap-posnr.

* ORDEM NOVA
      wa_0090-auart         = it_vbak-auart.
      wa_0090-vbeln         = it_vbap-vbeln.
      wa_0090-posnn         = it_vbap-posnr.
      wa_0090-spart         = it_vbap-spart.

      case it_vbak-auart.
        when 'ZTRI'.
          wa_0090-zmeng         = it_vbap-zmeng.

        when 'ZFUT'.

          wa_0090-zmeng         = it_vbap-zmeng.

*         Quando a transferencia Total alteramos o preço da Ordem Principal
          wa_0090-netprv = it_konv-kbetr.
          wa_0090-kmeinv = it_konv-kmein.

        when others.
          wa_0090-zmeng         = it_vbap-kwmeng.

          case p_direcao.
            when 'Y'.
              wa_0090-zmeng = wa_0090-zmeng * -1.
            when 'W'.
              wa_0090-zmeng = it_vbap-zmeng.
          endcase.

      endcase.

      case p_direcao.
        when 'R'.
          wa_0090-zmengv      = it_vbap-kwmeng * -1.
        when 'M' or 'UN'.
          wa_0090-zmengv      = it_popup2_aux-qt_tran * -1.
      endcase.

      if it_vbak-auart eq 'ZTRI'.
        wa_0090-zmengv      = 0.
      endif.

*     "// get do preço da OV/Iten
      wa_0090-netpr = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kbetr.
*     "// get da unidade de Medida do Preço
      wa_0090-kmein = zcl_util_=>get_kbetr( i_vbeln = it_vbap-vbeln i_posnr = it_vbap-posnr )-kmein.

      wa_0090-zieme         = it_vbap-zieme.
*      WA_0090-NETPR         = IT_KONV-KBETR.
*      WA_0090-KMEIN         = IT_KONV-KMEIN.
      wa_0090-charg         = it_vbap-charg.
      wa_0090-matnr         = it_vbap-matnr.
      wa_0090-matkl         = it_vbap-matkl.
      wa_0090-inco1         = it_vbkd-inco1.
      wa_0090-werks         = it_vbap-werks.
      wa_0090-kunnr         = it_vbak-kunnr.
      wa_0090-kurrf         = it_vbkd-kurrf.

      wa_0090-data_prevpgto = cs_values_popup-dtprevpag_n.
      wa_0090-kurrf = cs_values_popup-taxa_neg.
      wa_0090-prev_pgto_usd = cs_values_popup-vlr_prev.
      wa_0090-prev_pgto_brl = cs_values_popup-vlr_prev_brl.
      wa_0090-prev_multa_usd = cs_values_popup-multa_prev.
      wa_0090-prev_juros_usd = cs_values_popup-juros_prev.
      wa_0090-prev_vl_liq_usd = cs_values_popup-vlr_liq_prev.

*    "// Marca o Item com Imposto
      wa_0090-flag_imposto = cond #( when it_vbap-mwsbp is not initial then abap_true else abap_false ).

      if wa_0090-auart eq 'ZREM'.
        perform save_text using cs_values_popup-doc_simulacao seq.
      endif.

      " se teve estorno
      if cs_estorno-sequencia is not initial.

        wa_0090-sequenciav = cs_estorno-sequencia.
        wa_0090-data_prevpgtov = cs_estorno-data_prevpgto.

      endif.

      insert into zsdt0090 values wa_0090.

      if sy-subrc is initial.
        concatenate 'OV. ' it_vbap-vbeln  'foi Incluida na tabela ZSDT0090'  into  tl_return-message separated by space.
        tl_return-type = 'W'.
        append tl_return.
      endif.
**************************************************
      " inserindo registro na ZSDT0116, Tabela de Aprovação de embarque (Transção ZSDT0100)

      clear: wa_0116, wa_zsdt0116.

      if ( wa_0090-categoria eq ' ' ).
*      OR ( WA_0090-CATEGORIA EQ 'M' )
*      OR ( WA_0090-CATEGORIA EQ 'D' AND WA_0090-KUNNR EQ WA_0090-KUNNRV ).

*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Inicio
        perform insere_zsdt0116 using wa_0090-vbeln wa_0090-vbelv wa_0090-posnn.
*        SELECT SINGLE *
*          FROM zsdt0116
*          INTO wa_zsdt0116
*          WHERE vbeln   EQ wa_0090-vbelv
*            AND status  EQ ' '
*            AND status_workflow IN ( ' ', 'A' ).
*
*        IF sy-subrc IS INITIAL.
*
*          CALL FUNCTION 'NUMBER_GET_NEXT'
*            EXPORTING
*              nr_range_nr = '01'
*              object      = 'ZSEQ_0116_'
*            IMPORTING
*              number      = wa_0116-seq.
*
*          IF wa_0116-seq IS INITIAL.
*            ROLLBACK WORK.
*            MESSAGE 'Objeto numeração ZSEQ_0116_ não configurado!' TYPE 'S'.
*            RETURN.
*          ENDIF.
*
*          wa_0116-vbeln      =  wa_0090-vbeln.
*          wa_0116-posnr      =  wa_0090-posnn.
*          wa_0116-user_apv   =  wa_zsdt0116-user_apv.
*          wa_0116-dt_apv     =  wa_zsdt0116-dt_apv.
*          wa_0116-hr_apv     =  wa_zsdt0116-hr_apv.
*
*          INSERT INTO zsdt0116  VALUES wa_0116.
*          IF sy-subrc IS INITIAL.
*            CONCATENATE 'OV. ' it_vbap-vbeln  'foi Incluida na tabela ZSDT0116'  INTO  tl_return-message SEPARATED BY space.
*            tl_return-type = 'W'.
*            APPEND tl_return.
*          ENDIF.
*        ENDIF.
*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Fim
      endif.
**************************************************

    when others.

      select max( sequencia ) from zsdt0090
            into seq
      where doc_simulacao eq cs_values_popup-doc_simulacao.

*      SELECT COUNT(*)
*    FROM zsdt0090
*      INTO seq
*        WHERE doc_simulacao EQ it_popup2_aux-doc_simulacao.

      select  * from vbak
        into table it_vbak
      where vbeln eq cs_values_popup-vbeln.

      if not it_vbak[] is initial.
        select * from vbap
          into table it_vbap
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        select * from vbkd
          into table it_vbkd
          for all entries in it_vbak
        where vbeln eq it_vbak-vbeln.

        select * from v_konv " ---> S4 Migration - 10/07/2023 - DG
         into corresponding fields of table @it_konv
         for all entries in @it_vbak
           where knumv eq @it_vbak-knumv
        and kschl in ('PR00').

      endif.

      case p_direcao.
        when 'C' or 'E' or 'A'.
          delete it_vbap where posnr ne cs_values_popup-posnr.
        when 'M' or 'R'.
          sort it_vbap by posnr descending.
          read table it_vbap index 1.
          delete it_vbap where posnr ne it_vbap-posnr.
      endcase.

      loop at it_vbak.
        loop at it_vbap where vbeln eq it_vbak-vbeln and
                              posnr eq cs_values_popup-posnr.
          clear it_vbkd.
          read table it_vbkd with key vbeln = it_vbap-vbeln.
          read table it_konv with key knumv = it_vbak-knumv
                                      kposn = it_vbap-posnr.

          add 1 to seq.

          wa_0090-doc_simulacao = cs_0090-doc_simulacao.

          if wa_0090-doc_simulacao is initial.
            wa_0090-doc_simulacao = cs_0090-doc_simulacao = cs_values_popup-doc_simulacao.
          endif.

          wa_0090-sequencia     = seq.

          case p_direcao.

            when 'C' or 'E' or 'A'.

            when others.

              wa_0090-auart         = it_vbak-auart.
              wa_0090-vbeln         = it_vbap-vbeln.
              wa_0090-posnn         = it_vbap-posnr.
              wa_0090-spart         = it_vbap-spart.
              wa_0090-zmeng         = it_vbap-kwmeng.
              wa_0090-zieme         = it_vbap-zieme.

              data(coeficiente) = zcl_solicitacao_ov=>get_imposto(
                _cliente    = it_vbak-kunnr
                _fornecedor = conv #( |{ it_vbap-werks alpha = in }| )
                _material   = it_vbap-matnr
                _tipo_ordem = it_vbak-auart
                _direcao    = 'D'
                _werks      = it_vbap-werks  "<<RIM-SKM-IR120585-23.12.22
              ).

              wa_0090-netpr         = cond #( when coeficiente is initial then it_konv-kbetr else it_konv-kbetr / coeficiente ).

              wa_0090-kmein         = it_konv-kmein.
              wa_0090-charg         = it_vbap-charg.
              wa_0090-matnr         = it_vbap-matnr.
              wa_0090-matkl         = it_vbap-matkl.
              wa_0090-inco1         = it_vbkd-inco1.
              wa_0090-werks         = it_vbap-werks.
              wa_0090-kunnr         = it_vbak-kunnr.
          endcase.

          wa_0090-auartv        = cs_0090-auart.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = cs_0090-vbelv
            importing
              output = wa_0090-vbelv.

          wa_0090-spartv         = it_vbap-spart.
          wa_0090-kunnrv         = wa_0090-kunnr.

          if 'M' eq p_direcao.
            wa_0090-matklv         = lv_matkl.
          else.
            wa_0090-matklv         = it_vbap-matkl.
          endif.

          wa_0090-inco1v         = it_vbkd-inco1.
          wa_0090-werksv         = it_vbap-werks.
          wa_0090-chargv         = it_vbap-charg.

          if 'C' ne p_direcao.
            wa_0090-posnv         = it_vbap-posnr. " foi incluido o POSNR por conta do Redistirbuição estava ITENS
            wa_0090-matnrv         = it_vbap-matnr.

            if 'E' eq p_direcao.
              wa_0090-zmengv         = 0. "it_popup2_aux-sd_disp.
            else.
              wa_0090-zmengv         = cs_0090-zmengv.
            endif.

            wa_0090-ziemev         = cs_0090-ziemev.


            case p_direcao.
              when 'D'.
                wa_0090-zmengv = wa_0090-zmengv * -1.
              when 'R'.
                wa_0090-zmengv = wa_0090-zmeng * -1.
            endcase.

            "CLEAR it_popup.
            "READ TABLE it_popup WITH KEY vbeln2 = it_popup2_aux-vbeln2 posnr = it_popup2_aux-posnr.
            "IF sy-subrc IS INITIAL.
            wa_0090-netprv        = cs_0090-netprv.
            "ELSE.
            "wa_0090-netprv        = it_popup2_aux-kbetr.
            "ENDIF.
            wa_0090-kmeinv         = cs_0090-kmeinv.

          endif.

          wa_0090-categoria     = p_direcao.
          wa_0090-usnam         = sy-uname.
          wa_0090-data_atual    = sy-datum.
          wa_0090-hora_atual    = sy-uzeit.

          wa_0090-data_prevpgto = cs_values_popup-dtprevpag_n.
          "wa_0090-prev_taxa = cs_values_popup-taxa_neg.

          if p_direcao ne 'D'.
            wa_0090-kurrf = cs_values_popup-taxa_neg.
          endif.

          wa_0090-prev_pgto_usd = cs_values_popup-vlr_prev.
          wa_0090-prev_pgto_brl = cs_values_popup-vlr_prev_brl.
          wa_0090-prev_multa_usd = cs_values_popup-multa_prev.
          wa_0090-prev_juros_usd = cs_values_popup-juros_prev.
          wa_0090-prev_vl_liq_usd = cs_values_popup-vlr_liq_prev.

          " se teve estorno
          if cs_estorno-sequencia is not initial.

            wa_0090-sequenciav = cs_estorno-sequencia.
            wa_0090-data_prevpgtov = cs_estorno-data_prevpgto.

          endif.

          " 02.03.2023 - RAMON - Seq para trava -->
          if p_direcao ne 'D'.
            perform f_get_seq_trava changing wa_0090.
          endif.
          " 02.03.2023 - RAMON - Seq para trava --<

          insert into zsdt0090 values wa_0090.

          if sy-subrc is initial.
            concatenate 'OV. ' it_vbap-vbeln  'foi Incluida na Tabela ZSDT0090'  into  tl_return-message separated by space.
            tl_return-type = 'W'.
            append tl_return.
          endif.
**************************************************
          " inserindo registro na ZSDT0116, Tabela de Aprovação de embarque (Transção ZSDT0100)
          clear: wa_0116, wa_zsdt0116.

          if ( wa_0090-categoria eq ' ' ).
*      OR ( WA_0090-CATEGORIA EQ 'M' )
*      OR ( WA_0090-CATEGORIA EQ 'D' AND WA_0090-KUNNR EQ WA_0090-KUNNRV ).















*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Inicio
            perform insere_zsdt0116 using wa_0090-vbeln wa_0090-vbelv wa_0090-posnn.










*            SELECT SINGLE * FROM zsdt0116 INTO wa_zsdt0116
*              WHERE vbeln   EQ wa_0090-vbelv
*                AND status  EQ ' '
*                AND status_workflow IN ( ' ', 'A' ).
*
*            IF sy-subrc IS INITIAL.
*
*              CALL FUNCTION 'NUMBER_GET_NEXT'
*                EXPORTING
*                  nr_range_nr = '01'
*                  object      = 'ZSEQ_0116_'
*                IMPORTING
*                  number      = wa_0116-seq.
*
*              IF wa_0116-seq IS INITIAL.
*                ROLLBACK WORK.
*                MESSAGE 'Objeto numeração ZSEQ_0116_ não configurado!' TYPE 'S'.
*                RETURN.
*              ENDIF.
*
*              wa_0116-vbeln      =  wa_0090-vbeln.
*              wa_0116-posnr      =  wa_0090-posnn.
*              wa_0116-user_apv   =  wa_zsdt0116-user_apv.
*              wa_0116-dt_apv     =  wa_zsdt0116-dt_apv.
*              wa_0116-hr_apv     =  wa_zsdt0116-hr_apv.
*
*              INSERT INTO zsdt0116  VALUES wa_0116.
*              IF sy-subrc IS INITIAL.
*                CONCATENATE 'OV. ' it_vbap-vbeln  'foi Incluida na tabela ZSDT0116'  INTO  tl_return-message SEPARATED BY space.
*                tl_return-type = 'W'.
*                APPEND tl_return.
*              ENDIF.
*            ENDIF.
*#133809 -  ITSOUZA - 27.05.2024 15:37:55 - Fim
          endif.
*****************************************************
        endloop.
      endloop.

  endcase.

  cs_0090 =  wa_0090.

  if wa_0090 is not initial.

    commit work and wait.

    " 10.09.2024 - RAMON - 97513 -->
    data lr_docsi type range of zsds0090_email-doc_simulacao.
    data lr_seq type range of zsds0090_email-sequencia.

    append 'IEQ' && wa_0090-doc_simulacao to lr_docsi.

    append initial line to lr_seq assigning field-symbol(<fs_seq>).

    <fs_seq>-sign = 'I'.
    <fs_seq>-option = 'EQ'.
    <fs_seq>-low = wa_0090-sequencia.

    zcl_util_=>indicator( |Enviando e-mail trava de cambio...| ).

    submit zsdr0157
            with p_exec eq 'X'
            with so_docsi in lr_docsi[]
            with so_seq in lr_seq[]
      and return.

  endif.

  " 10.09.2024 - RAMON - 97513 <--

  clear: wa_0090, wa_ov.
  free: it_vbap, it_vbak, it_vbkd.

endform.
*&---------------------------------------------------------------------*
*&      Form  F4_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_mat using id.

*  DATA(WA_TROCA) = IT_TROCA_OLD[ ID ].
  data(wa_troca) = tg_ov_mat[ 1 ].


** Encontra grupo de mercadorias
  select single matkl matnr
    into (lv_matkl, lv_matnr)
    from mara
  where matnr eq wa_troca-matnr.

  if sy-subrc is initial.
** Busca materias que estão cadastrados pelo dep. insumos( TABELA ZSDT0036)
    select distinct c~matnr b~maktx c~meins a~meins c~werks_fornec c~inco1 c~waerk a~matkl c~vlr_venda
      into table tg_mat
      from mara as a
      inner join makt as b on a~matnr eq b~matnr
      inner join zsdt0036 as c on a~matnr eq c~matnr
      where bukrs      eq wa_saida-vkorg
       and  waerk      eq wa_saida-waerk
       and  cultura    eq wa_saida-cultura
       and  eliminado  ne abap_true
    and  val_ate    >= sy-datum.

    loop at tg_mat assigning field-symbol(<f_mar>). <f_mar>-id = sy-tabix. endloop.

* Eliminar da seleção apenas os registros que: são do mesmo material/Frete/Centro da OV.
    delete tg_mat
         where matnr eq wa_saida-matnr
         and   inco1 eq wa_saida-inco1
         and   werks_fornec eq wa_saida-werks.

  endif.

  tg_material[] = value #( for ls1 in tg_mat ( corresponding #( ls1 ) ) ).

  free: gt_dselc, gt_return_tab.

  if tg_material[] is not initial.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'ID'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      tables
        value_tab       = tg_material
        return_tab      = gt_return_tab
        dynpfld_mapping = gt_dselc
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.
*
    if sy-subrc is initial.

      read table gt_return_tab index 1.
      replace all occurrences of '.'  in gt_return_tab-fieldval with '' ignoring case.
      condense gt_return_tab-fieldval no-gaps.
      read table tg_material with key id = gt_return_tab-fieldval.
      move-corresponding tg_material to wg_material.
      move tg_material-maktx to wg_des_matnr.

*      IT_TROCA_OLD

    else.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0600 output.

  set pf-status 'Z001'.
  set titlebar 'Z001'.

  clear wa_layout.
  wa_layout = value #(
                       zebra      = abap_true
                       no_rowmark = abap_true
                       info_fname = 'COLOR'
                       no_toolbar = abap_true
                       grid_title = abap_false
                      ).

*  WA_STABLE-ROW        = ABAP_TRUE.

  tl_function
  = value #(
             ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
             ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
             ( cl_gui_alv_grid=>mc_fc_loc_move_row )
             ( cl_gui_alv_grid=>mc_fc_loc_paste )
             ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
             ( cl_gui_alv_grid=>mc_fc_loc_undo )
             ( cl_gui_alv_grid=>mc_fc_loc_append_row )
             ( cl_gui_alv_grid=>mc_fc_loc_copy )
             ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
             ( cl_gui_alv_grid=>mc_fc_loc_cut )
           ).

  if obg_r_c01 is initial.

    create object obg_r_c01
      exporting
        container_name = 'R_C1'.

    create object grid_r_c1
      exporting
        i_parent = obg_r_c01.

    perform montar_layout2 using 'C1'.

    wa_layout = value #(
                          grid_title = abap_false
                          no_toolbar = abap_true
                        ).

    grid_r_c1->set_table_for_first_display(
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_popup[] ).

    grid_r_c1->register_edit_event( exporting i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    grid_r_c1->register_edit_event( exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    set handler: lcl_event_handler=>on_data_changed_finished3 for grid_r_c1,
                 lcl_event_handler=>on_data_changed3 for grid_r_c1.
  else.
    grid_r_c1->refresh_table_display( exporting is_stable = wa_stable ).
  endif.

  if obg_r_c02 is initial.

    create object obg_r_c02
      exporting
        container_name = 'R_C2'.

    create object grid_r_c2
      exporting
        i_parent = obg_r_c02.

    perform montar_layout2 using 'C2'.

    grid_r_c2->set_table_for_first_display(
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_popup2[] ).

    grid_r_c2->register_edit_event( exporting i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    grid_r_c2->register_edit_event( exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    set handler: lcl_event_handler=>on_data_changed_finished3 for grid_r_c2,
               lcl_event_handler=>on_data_changed3 for grid_r_c2.

  else.
    grid_r_c2->refresh_table_display( exporting is_stable = wa_stable ).
  endif.


endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0600 input.

  data: valida type char1.

  clear: valida.

  check sy-dynnr eq '0600'.

  if it_popup2 is not initial.
    loop at it_popup2 where check eq abap_true.
      if it_popup2-inco1 eq 'CIF'.
        data(v_matkl) = zcl_util_=>get_mara( it_popup2-matnr ).

        select count(*)
        from zsdt0037
        where val_de         le sy-datum
          and val_ate        ge sy-datum
          and bukrs          eq it_popup2-vkorg
          and matkl          eq v_matkl
          and filial_origem  eq it_popup2-werks
          and meins          eq it_popup2-kmein
          and filial_destino eq it_popup2-vkbur
        and waers          eq 'BRL'.

        check sy-subrc is not initial.
        message |Não existe Frete Associoado para gerar a nova O.V., Cadastrar na ZSDT0047!| type 'S' display like 'E'.
        valida = 'X'.
      endif.
    endloop.
  endif.

  case sy-ucomm.
    when 'BACK' or 'EXIT' or 'CANCEL' or 'SAIR'.
      leave to screen 0.
    when: 'CONF'.
      if valida ne 'X'.
        perform redistribuicao.
        if sy-ucomm ne 'CANCEL'.
          leave to screen 0.
        endif.
      endif.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  REDISTRIBUICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form redistribuicao .

  data: w_zsdt0036 type zsdt0036,
        tl_fields  type table of sval with header line,
        lv_return  type konv-kbetr.

  free tl_return[].
  loop at it_popup2.
    clear vl_vlr_venda.

    select single *
      from  mara
    into @data(_mara)
    where matnr = @it_popup2-matnr.

    select a~*
    from zsdt0036 as a
      inner join mara as b on b~matnr eq a~matnr
                          and b~matkl eq @_mara-matkl
         into table @data(t_zsdt0036)
         where a~val_de  le @sy-datum
         and   a~val_ate ge @sy-datum
         and   a~waerk   eq @it_popup2-waerk
         and   a~safra   eq @it_popup2-safra
         and   a~cultura eq @it_popup2-cultura
         and   a~meins   eq @it_popup2-kmein
         and   a~eliminado ne 'X'
    order by vlr_venda descending.

    read table t_zsdt0036 into w_zsdt0036 index 1.
    if it_popup2-kbetr gt w_zsdt0036-vlr_venda.
      call screen 0601 starting at 30 05 ending at 68 1.
    endif.
  endloop.

  if sy-ucomm eq 'CANCEL'.
    exit.
  endif.

  perform cria_ov using 'R'.
  if not line_exists( tl_return[ type = 'E' ] ).

    perform insere_zsdt0116 using p_0090-vbeln p_0090-vbelv p_0090-posnn.

    free it_ov.

    loop at it_popup.
      try.
          data(w_new) = it_popup2[ vbeln2 = it_popup-vbeln2 posnr = it_popup-posnr ].
        catch cx_sy_itab_line_not_found.
      endtry.
*
*      IF W_NEW-KBETR EQ IT_POPUP-KBETR.
*
*        FREE IT_DESC_ABS.
*        CALC_1 = 0.
*        CALC_2 = 0.
*        DIFER_0 = 0.
*
*        DATA(VBAP_1) = ZCL_UTIL_=>GET_VBAP( I_VBELN = IT_POPUP-VBELN I_MATNR = IT_POPUP-MATNR ). "// old
*        DATA(VBAP_2) = ZCL_UTIL_=>GET_VBAP( I_VBELN = W_NEW-VBELN I_MATNR = IT_POPUP-MATNR ). "// new
*
**        SELECT SINGLE *
**          FROM VBAP
**            INTO @DATA(VBAP_1)
**             WHERE VBELN EQ @IT_POPUP-VBELN
**              AND POSNR EQ @IT_POPUP-POSNR.
**
**        SELECT SINGLE *
**          FROM VBAP
**          INTO @DATA(VBAP_2)
**          WHERE VBELN EQ @W_NEW-VBELN
**            AND POSNR EQ @IT_POPUP-POSNR.
*
*        CALC_1 = VBAP_1-NETWR + VBAP_1-MWSBP.
*        CALC_2 = VBAP_2-NETWR + VBAP_2-MWSBP.
*
*        IF CALC_1 NE CALC_2.
*          DIFER_0 = CALC_1 - CALC_2.
*
*          DATA(COEFICIENTE_O) = ZCL_SOLICITACAO_OV=>GET_IMPOSTO(
*                                                                 _DIRECAO = 'O'
*                                                                 _VBELN   = IT_POPUP-VBELN
*                                                                 _POSNR   = IT_POPUP-POSNR
*                                                               ).
*          MULTIPLY DIFER_0 BY COEFICIENTE_O.
*
*          APPEND VALUE #(
*                           VBELN         = W_NEW-VBELN
*                           POSNR         = IT_POPUP-POSNR
*                           MATNR         = IT_POPUP-MATNR
*                           DESC_ABSOLUTO = DIFER_0
*                        ) TO IT_DESC_ABS.
*        ENDIF.
*      ENDIF.

      wa_ov-vbeln = it_popup-vbeln.
      wa_ov-posnr = it_popup-posnr.
      wa_ov-matnr = it_popup-matnr.
      wa_ov-zmeng = it_popup-qt_tran.
      append wa_ov to it_ov.

      call function 'ZSDMF001_ATUALI_OV_SIM'
        exporting
          i_soma    = 'R'
          i_acao    = 'REDIST'
        tables
          it_ov     = it_ov
          te_return = tl_return.

      call method zcl_util_=>zsdmf001_atuali_ov_simulador
        exporting
          i_vbeln     = it_popup-vbeln
          i_posnr     = it_popup-posnr
          i_matnr     = it_popup-matnr
          i_diferenca = zcl_util_=>get_desconto( i_vbeln = it_popup-vbeln i_posnr = it_popup-posnr i_add_imposto = abap_true ).

*       "// Reprocessa a O.V OLD
      zcl_util_=>processa_desc( i_vbeln = it_popup-vbeln i_matnr = it_popup-matnr ).
*       "// Reprocessa a O.V NEW
      zcl_util_=>processa_desc( i_vbeln = w_new-vbeln i_matnr = w_new-matnr ).

*      CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
*        TABLES
*          TI_ITENS_OV       = IT_DESC_ABS
*        EXCEPTIONS
*          OV_NAO_ENCONTRADA = 1
*          OTHERS            = 2.

    endloop.
  endif.

*  LOOP AT IT_OV INTO WA_OV.
*    ZCL_UTIL_=>INDICATOR( |Reprocessa O.V { WA_OV-VBELN }, { WA_OV-MATNR }!| ).
*    ZCL_UTIL_=>PROCESSA_DESC( I_VBELN = WA_OV-VBELN I_MATNR = WA_OV-MATNR ).
*  ENDLOOP.

* "// Processa 40
  zcl_util_=>loop_40(
    simulador = w_new-doc_simulacao
    i_vbeln   = w_new-vbeln
    i_matnr   = w_new-matnr
    qtd       = 10
  ).

  perform f_exibe_bapi.


endform.
*&---------------------------------------------------------------------*
*&      Form  ZTRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ztri .

  clear: wg_linhas, tg_vbfa.

  data v_mensagem type string. "Alteracao feita por alexandre Rimini 05.04.2023

  loop at it_saida into wa_saida where check = abap_true.
    if wa_saida-auart ne 'ZREM'.
      message 'Tipo de OV não permitida.' type 'I'.
      exit.
    endif.
    add 1 to wg_linhas.
  endloop.

  if wg_linhas gt 1.
    message 'Selecione uma linha por vez' type 'I'.
    exit.
  elseif wg_linhas eq 0.
    exit.
  endif.

** Selecionando número das faturas
  select distinct vbeln rfmng
    from vbfa
    into table tg_vbfa
    where vbelv   eq wa_saida-vbeln
     and  posnv   eq wa_saida-posnr
     and  vbtyp_n eq 'M'
     and  vbtyp_v eq 'C'
  and  fktyp   eq 'L'.

  free tl_fields.
  clear tl_fields.
  tl_fields-tabname    = 'VBFA'.
  tl_fields-fieldname  = 'VBELN'.
  tl_fields-field_obl  = 'X'.
  tl_fields-fieldtext  = 'Informar Doc.Fatura'.
  append tl_fields.

  call function 'POPUP_GET_VALUES_USER_HELP'
    exporting
      f4_formname     = 'F_BUSCA_FATURA'
      f4_programname  = sy-cprog
      popup_title     = 'Selecionar Documento de fatura'
      programname     = sy-cprog
    importing
      returncode      = lv_return
    tables
      fields          = tl_fields
    exceptions
      error_in_fields = 1
      others          = 2.


*** inicio alteracao feita por Alexandre Rimini - 05.04.2023 - CS1078275

  select single vbeln into it_fat
    from vbfa
    where vbelv = tl_fields-value
  and vbtyp_n = 'M'.

  if sy-subrc = 0 .

    select single vbeln from vbrk
      into it_vbrk
      where vbeln = it_fat-vbelv
      and fksto <> 'X'
    and fkart = 'ZTRI'.

    concatenate 'Esse documento ja foi faturado, fatura No '  it_vbrk-vbeln  ' Por favor, verifique.' into v_mensagem.
    if sy-subrc = 0.
      message v_mensagem  type 'E'.
      exit.
    endif.
  endif.

*** fim alteracao feita por Alexandre Rimini - 05.04.2023 - CS1078275

  read table tg_vbfa with key vbeln = tl_fields-value.
  if sy-subrc is initial.
** Checar se possui nota e se foi validada.
    select single docsta cancel
      into wl_active
      from j_1bnflin as a
      inner join j_1bnfe_active as b on b~docnum = a~docnum
      where refkey eq tl_fields-value
       and  docsta eq 1
       and  scssta ne 2
    and  cancel ne 'X' .

    if sy-subrc is initial.

      clear wa_zsdt0090.

      select single *
        into wa_zsdt0090
        from zsdt0090
      where vbeln eq wa_saida-vbeln.

      free it_ov.
      clear wa_ov.

      wa_ov-vbeln = wa_zsdt0090-vbelv.
      wa_ov-posnr = wa_zsdt0090-posnv.
      wa_ov-matnr = wa_zsdt0090-matnr.
      append wa_ov to it_ov.

      clear wa_ov.

      wa_ov-vbeln = wa_saida-vbeln.
      wa_ov-posnr = wa_saida-posnr.
      wa_ov-zmeng = tg_vbfa-rfmng.
      wa_ov-auart = wa_saida-auart.
      wa_ov-matnr = wa_zsdt0090-matnr.
      append wa_ov to it_ov.

      v_auart = 'ZTRI'.

*      CALL FUNCTION 'ZSDMF001_GERA_OV_COCKPIT'
      call function 'ZSDMF001_GERA_OV_COCKPIT_ZTRI'
        exporting
          i_auart    = v_auart
          i_acao     = p_ucomm
          i_bill_doc = tg_vbfa-vbeln
        tables
          it_ov      = it_ov
          te_return  = tl_return.

      perform f_exibe_bapi.

    else.
      message 'Nota não autorizada.' type 'I'.
    endif.
  else.
    message 'Número de fatura inválido.' type 'I'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  TROCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form troca .

  clear: wg_linhas, wa_preco_.
  loop at it_saida into wa_saida where check = abap_true.
    add 1 to wg_linhas.
  endloop.

  if wg_linhas ne 1.
    message 'Selecione uma linha por vez' type 'I'.
    exit.
  endif.

  free: tg_ov_mat[], it_sai_aux[].
  loop at it_saida into wa_saida where check eq abap_true.

    move-corresponding wa_saida to it_sai_aux.
    append it_sai_aux.

    tg_ov_mat-vbeln       = wa_saida-vbeln.
    tg_ov_mat-charg       = wa_saida-charg.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_saida-vbeln
      importing
        output = tg_ov_mat-vbeln2.

    tg_ov_mat-posnr       = wa_saida-posnr.
    tg_ov_mat-matnr       = wa_saida-matnr.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_saida-matnr
      importing
        output = tg_ov_mat-matnr2.

    tg_ov_mat-arktx       = wa_saida-arktx.
    tg_ov_mat-kwmeng      = wa_saida-kwmeng.
    tg_ov_mat-vrkme       = wa_saida-vrkme.
    tg_ov_mat-werks       = wa_saida-werks.
    tg_ov_mat-charg       = wa_saida-charg.
    tg_ov_mat-lgort       = wa_saida-lgort.
    tg_ov_mat-meins       = wa_saida-kmein.
    tg_ov_mat-netpr       = wa_saida-kbetr.
    tg_ov_mat-netwr       = wa_saida-netwr.

    tg_ov_mat-sd_disp     = 0.

    perform busca_saldo using 'M' changing it_sai_aux[].
    read table it_sai_aux index 1.

    tg_ov_mat-sd_disp = it_sai_aux-sd_disp.
    tg_ov_mat-qt_tran = 0.
    append tg_ov_mat.

  endloop.

  read table tg_ov_mat index 1.

** Encontra grupo de mercadorias
  select single matkl matnr
    into (lv_matkl, lv_matnr)
    from mara
  where matnr eq tg_ov_mat-matnr.

  if sy-subrc is initial.
** Busca materias que estão cadastrados pelo dep. insumos( TABELA ZSDT0036)
    select distinct c~matnr b~maktx c~meins a~meins c~werks_fornec c~inco1 c~waerk a~matkl c~vlr_venda
      into table tg_mat
      from mara as a
      inner join makt as b on a~matnr eq b~matnr
      inner join zsdt0036 as c on a~matnr eq c~matnr
      where bukrs      eq wa_saida-vkorg
       and  waerk      eq wa_saida-waerk
       and  cultura    eq wa_saida-cultura
*       AND  A~MATNR    NE LV_MATNR
       and  eliminado  ne abap_true
    and  val_ate    >= sy-datum.

    loop at tg_mat assigning field-symbol(<f_mar>). <f_mar>-id = sy-tabix. endloop.

* Eliminar da seleção apenas os registros que: são do mesmo material/Frete/Centro da OV.
    delete tg_mat
         where matnr eq wa_saida-matnr
         and   inco1 eq wa_saida-inco1
         and   werks_fornec eq wa_saida-werks.

  endif.

  tg_material[] = value #( for ls1 in tg_mat ( corresponding #( ls1 ) ) ).

  clear: wg_material, wg_des_matnr.

  clear: altura, h1.
  h1 = lines( tg_ov_mat ).
  perform tela using h1 changing altura.

  call screen 0400 starting at 050 3
                   ending   at 160 altura.

endform.
*&---------------------------------------------------------------------*
*&      Form  DESBLOQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form desbloq .

  clear wg_linhas.

  loop at it_saida into wa_saida where check = abap_true.

    select *
      from vbep
      into table tl_vbep
      where vbeln eq wa_saida-vbeln
    and  posnr eq wa_saida-posnr.

    refresh: tl_return,i_order_item_in,i_order_item_inx,i_sched,i_schedx.
    clear f_headinx.
    f_headinx-updateflag = 'U'.

    loop at tl_vbep.
      i_sched-itm_number = wa_saida-posnr.
      i_sched-sched_line = tl_vbep-etenr.
      if tl_vbep-lifsp = ''.
        i_sched-req_dlv_bl  = '10'.
      else.
        i_sched-req_dlv_bl  = ''.
      endif.
      i_schedx-req_dlv_bl  = 'X'.
      i_schedx-itm_number = wa_saida-posnr.
      i_schedx-sched_line = tl_vbep-etenr.
      i_schedx-updateflag  = 'U'.
      append: i_sched, i_schedx.
    endloop.

    "*---> 19/07/2023 - Migração S4 - LO
    call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      exporting
        salesdocument    = wa_saida-vbeln
        order_header_inx = f_headinx
      tables
        return           = tl_return
        order_item_in    = i_order_item_in
        order_item_inx   = i_order_item_inx
        schedule_lines   = i_sched
        schedule_linesx  = i_schedx.

    read table tl_return with key type = 'E'.
    if sy-subrc ne 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      wait up to 1 seconds.
    endif.

  endloop.

  perform f_exibe_bapi.

endform.
*&---------------------------------------------------------------------*
*&      Form  DESMEMBRAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form desmembrar using p_tela type c.

  clear: v_vbeln, wg_exit.
  loop at it_saida into wa_saida where check = abap_true .
    v_vbeln = wa_saida-vbeln.
    exit.
  endloop.

  loop at it_saida into wa_saida where check = abap_true .
    if wa_saida-vbeln ne v_vbeln.
      wg_exit = abap_true.
      message 'A Seleção Multipla de linhas só pode ocorrer para o mesmo numero de OV!' type 'I'.
      exit.
    endif.
  endloop.

  check wg_exit is initial.
  check v_vbeln is not initial.

  if not 'ZDEF_ZFTE_ZSEM_ZODF_ZOFE_ZOSM_ZBON' cs wa_saida-auart.
    message 'Tipo de OV não permitida!' type 'I'.
    exit.
  endif.

  free: tg_ov2, it_sai_aux.
  it_sai_aux[] = it_saida.
  delete it_sai_aux where check = abap_false.
  perform busca_saldo using 'D' changing it_sai_aux[].

  loop at it_sai_aux.

    tg_ov2-vbeln       = it_sai_aux-vbeln.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = it_sai_aux-vbeln
      importing
        output = tg_ov2-vbeln2.

    tg_ov2-posnr       = it_sai_aux-posnr.

    tg_ov2-matnr       = it_sai_aux-matnr.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = it_sai_aux-matnr
      importing
        output = tg_ov2-matnr2.

    tg_ov2-arktx       = it_sai_aux-arktx.
    tg_ov2-kwmeng      = it_sai_aux-kwmeng.
    tg_ov2-vrkme       = it_sai_aux-vrkme.
    tg_ov2-sd_disp     = it_sai_aux-sd_disp.


    call method zcl_manutencao_insumos=>chk_desconto_abs_faturado
      exporting
        i_vbeln = it_sai_aux-vbeln
      importing
        is_ok   = data(is_ok).

    if is_ok is initial.
      tg_ov2-qt_tran     = 0.
    else.
      tg_ov2-qt_tran     = it_sai_aux-sd_disp.
    endif.

    append tg_ov2.

  endloop.

  clear wg_desmem.
  wg_desmem-zdes = 'X'.

  clear: altura, h1.
  h1 = lines( tg_ov2 ).
  perform tela using h1 changing altura.

  call screen 0300 starting at 050 3
                   ending   at 160 18.

endform.
*&---------------------------------------------------------------------*
*&      Form  CANCELAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cancelar .

  if reduce i( init x = 0 for ls in it_saida where ( check is not initial ) next x = x + 1 ) ne 1.
    message 'Selecione uma linha por vez' type 'I'.
    exit.
  endif.

  wa_saida = it_saida[ check = abap_true ].

  clear wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    if wa_saida-auart eq 'ZTRI'.
      wg_exit = abap_true.
      message 'Ordem não pode ser Cancelada, deve ser estornada!' type 'I'.
      exit.
    endif.
  endloop.

  check wg_exit is initial.

  select single count(*)
  from zsdt0090
  where vbeln eq @wa_saida-vbeln
  and categoria eq 'K'
  and estorno eq @abap_false.

  if sy-subrc is initial.
    message 'O.V proveniente de uma Devolução não pode ser Estornada!' type 'I'.
    exit.
  endif.

  call function 'MOVE_CHAR_TO_NUM'
    exporting
      chr = wa_saida-vbeln
    importing
      num = vl_vbeln.

  vl_vbeln_c = vl_vbeln.

  select count(*)
    from zsdt0159
     where vbeln  eq wa_saida-vbeln
  and estorno eq abap_false.

  if sy-subrc is initial.
    message |O.V. possui Boleto Bancário/ou Pré lançamento do Boleto. Eliminar Antes!| type 'S' display like 'E'.
    exit.
  endif.

  refresh: it_bsid, it_bsad.
  select a~bukrs a~kunnr a~vbel2
    into table it_bsid
    from bsid as a
   inner join bkpf as b on a~bukrs = b~bukrs
                       and a~belnr = b~belnr
                       and a~gjahr = b~gjahr
  where a~bukrs eq wa_saida-vkorg
    and a~kunnr eq wa_saida-kunnr
    and b~xreversal eq ''
    and ( ( a~vbel2 eq wa_saida-vbeln ) or
          ( a~xblnr eq wa_saida-vbeln ) or
          ( a~vbel2 eq vl_vbeln_c     ) or
  ( a~xblnr eq vl_vbeln_c     )  ).

  select a~bukrs a~kunnr a~vbel2
    into table it_bsad
    from bsad as a
    inner join bkpf as b on a~bukrs = b~bukrs
                        and a~belnr = b~belnr
                        and a~gjahr = b~gjahr
   where a~bukrs eq wa_saida-vkorg
     and a~kunnr eq wa_saida-kunnr
     and b~xreversal eq ''
     and ( ( a~vbel2 eq wa_saida-vbeln ) or
           ( a~xblnr eq wa_saida-vbeln ) or
           ( a~vbel2 eq vl_vbeln_c     ) or
  ( a~xblnr eq vl_vbeln_c     )  ).

  if ( it_bsid[] is not initial ) or ( it_bsad[] is not initial ).
    message 'Ordem de venda não pode ser cancelada, existem Doc. Contábeis Vinculados!' type 'S' display like 'E'.
    exit.
  endif.

  select single *
    from zsdt0090
    into wa_zsdt0090
    where vbeln eq wa_saida-vbeln
      and posnn eq wa_saida-posnr
  and estorno ne abap_true.

  if sy-subrc is initial.
    message 'Ordem não pode ser Cancelada, deve ser estornada! !' type 'I'.
    exit.
  endif.

  select single *
  from zsdt0090
  into wa_zsdt0090
  where vbelv eq wa_saida-vbeln
    and categoria not in categoria
  and estorno ne abap_true.

  if sy-subrc is initial.
    message 'Ordem não pode ser Cancelada, possui Doc. subsequente!' type 'I'.
    exit.
  endif.

  select single vbelv posnv rfmng vbeln
   from vbfa
   into wa_vbfa
  where vbelv = wa_saida-vbeln.

  if sy-subrc is initial.
    message 'Ordem Possui documentos Subsequentes, Verificar!' type 'I'.
    exit.
  endif.

  select *
   from vbap
   into corresponding fields of table it_vbap
  where vbeln eq wa_saida-vbeln.

  if lines( it_vbap ) gt 1.

    call function 'K_KKB_POPUP_RADIO3'
      exporting
        i_title   = 'Ordem com mais de um Item, o que deseja realizar?'
        i_text1   = 'Sair'
        i_text2   = 'Cancelar toda a Ordem'
        i_text3   = 'Cancelar o item selecionado'
        i_default = '1'
      importing
        i_result  = w_answer
      exceptions
        cancel    = 1
        others    = 2.

    check w_answer is not initial.

    case w_answer.
      when '1'.
        exit.
      when '2'.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        select single *
          from zsdt0082 into @data(lwa_zsdt0082_exists)
         where vbeln  eq @wa_saida-vbeln
           and status not in ('3','4').

        if sy-subrc eq 0.
          message 'Ordem possui solicitação realizada, não pode ser estornada!' type 'I'.
          exit.
        endif.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

        call method zcl_manutencao_insumos=>set_estornar_solicitacao
          exporting
            i_vbeln = wa_saida-vbeln
            i_posnr = wa_saida-posnr
          importing
            e_msg   = data(e_msg).

        if e_msg is not initial.
          message e_msg type 'I'.
          exit.
        endif.

        zcl_util_=>delete_ov( wa_saida-vbeln ).
        perform verifica_desc_abs using 'T'.

      when '3'.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        select single *
          from zsdt0082 into lwa_zsdt0082_exists
         where vbeln  eq wa_saida-vbeln
           and posnr  eq wa_saida-posnr
           and status not in ('3','4').

        if sy-subrc eq 0.
          message 'Ordem/Item possui solicitação realizada, não pode ser estornada!' type 'I'.
          exit.
        endif.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

        call method zcl_manutencao_insumos=>set_estornar_solicitacao
          exporting
            i_vbeln = wa_saida-vbeln
            i_posnr = wa_saida-posnr
          importing
            e_msg   = e_msg.

        if e_msg is not initial.
          message e_msg type 'I'.
          exit.
        endif.

        perform cancela_iten.

        update  zsdt0041 set vbeln = ''
         where doc_simulacao = wa_saida-doc_simulacao
         and   matnr         = wa_saida-matnr
         and   werks         = wa_saida-werks
         and   vbeln         = wa_saida-vbeln.

        perform verifica_desc_abs using 'I'.

    endcase.

  else.

    call method zcl_manutencao_insumos=>set_estornar_solicitacao
      exporting
        i_vbeln = wa_saida-vbeln
        i_posnr = wa_saida-posnr
      importing
        e_msg   = e_msg.

    if e_msg is not initial.
      message e_msg type 'I'.
      exit.
    endif.

    zcl_util_=>delete_ov( wa_saida-vbeln ).
    perform verifica_desc_abs using 'T'.

  endif.

  perform f_exibe_bapi.

endform.
*&---------------------------------------------------------------------*
*&      Form  ESTORNAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form estornar.


  data: valor_real type p decimals 5,
        valor_vbap type p decimals 5,
        diferenca  type p decimals 5.

  if reduce i( init x = 0 for ls in it_saida where ( check is not initial ) next x = x + 1 ) ne 1.
    message 'Selecione uma linha por vez' type 'I'.
    exit.
  endif.

  wa_saida = it_saida[ check = abap_true ].

  select count(*)
   from zsdt0090
    into wa_zsdt0090
    where vbeln eq wa_saida-vbeln
      and posnn eq wa_saida-posnr
  and estorno eq abap_false.

  if sy-subrc is not initial.
    message 'Ordem não pode ser estornada' type 'I'.
    exit.
  endif.

  if wa_saida-auart = 'ZFUT'.
    message 'O estorno deve Ocorrer pela OV ZRFU!' type 'I'.
    exit.
  endif.

  select single count(*)
    from zsdt0090
    where vbeln eq @wa_saida-vbeln
    and categoria eq 'K'
    and estorno eq @abap_false.

  if sy-subrc is initial.
    message 'O.V proveniente de uma Devolução não pode ser Estornada!' type 'I'.
    exit.
  endif.

  select count(*)
  from zsdt0090
  where doc_simulacao eq wa_saida-doc_simulacao
    and vbelv eq wa_saida-vbeln
    and posnv eq wa_saida-posnr
    and estorno eq abap_false
    and categoria not in categoria.

  if sy-subrc is initial.
    message |Esta Ordem possui vinculações de transferências, realizadas por este Cockpit, que devem ser estornadas antes! | type 'I'.
    exit.
  endif.

  check not wa_saida-auart is initial.

  call method zcl_manutencao_insumos=>set_estornar_solicitacao
    exporting
      i_vbeln = wa_saida-vbeln
      i_posnr = wa_saida-posnr
    importing
      e_msg   = data(e_msg).

  if e_msg is not initial.
    message e_msg type 'I'.
    exit.
  endif.

  select single *
   from zsdt0090
   into wa_zsdt0090
   where vbeln eq wa_saida-vbeln
     and posnn eq wa_saida-posnr
  and estorno ne abap_true.

* TROCA DE MATERIAL
  if wa_zsdt0090-categoria eq 'M'.

    if wa_zsdt0090-vbeln eq wa_zsdt0090-vbelv.
*    ESTORNA O ITEN INCLUIDO
      perform estorna_i.
    else.
*    ESTORNA O MATERIAL NOVO
      perform estorna_m.
    endif.

    exit.
  endif.

  " 02.08.2023 - RAMON - 98680 -->
  if wa_saida-categoria = 'H'.

    break rblima. "#DEBUG

    clear tl_return[].

    call function 'ZSDMF_ESTORNA_OV_AGRUPAMENTO'
      exporting
        iv_doc_simulacao = wa_saida-doc_simulacao
        iv_vbeln         = wa_saida-vbeln
        iv_commit        = abap_true
      tables
        et_ret           = tl_return.

    perform f_exibe_bapi.

    exit.

  endif.
  " 02.08.2023 - RAMON - 98680 --<

  clear wa_zsdt0090.

  if 'ZRFU' eq wa_saida-auart.
    select single *
      from zsdt0090 into @data(wa_zsdt0090_zrfu)
     where vbeln   eq @wa_saida-vbeln
       and auart   eq 'ZRFU'
       and auartv  eq 'ZRFU'
    and estorno eq ''.
  endif.

  case wa_saida-auart.
*    WHEN 'ZOFE' OR 'ZOSM' OR 'ZODF'.
*
*      SELECT COUNT(*)
*        FROM VBFA
*        WHERE VBELV = WA_SAIDA-VBELN.
*
*      IF SY-SUBRC IS INITIAL.
*        MESSAGE 'Ordem possui documentos subsequentes, verificar!' TYPE 'I'.
*        EXIT.
*      ENDIF.
*
**     "// Retorna a Quantidade das Alterações da Quantidade realizada nela.
*      ZCL_UTIL_=>BACK_QTD( WA_SAIDA-VBELN ).
*
*      "Pega Materiais da OV estornada
*      SELECT *
*       FROM VBAP
*       INTO CORRESPONDING FIELDS OF TABLE IT_VBAP
*       WHERE VBELN EQ WA_SAIDA-VBELN.
*
*      SELECT *
*       FROM ZSDT0090
*       INTO TABLE @DATA(IT_0090)
*       WHERE VBELN = @WA_SAIDA-VBELN
*       AND ESTORNO EQ @ABAP_FALSE.
*
*      CHECK NOT IT_0090 IS INITIAL.
*      ZCL_UTIL_=>INDICATOR( |Deletando O.V { WA_SAIDA-VBELN }| ).
*      ZCL_UTIL_=>DELETE_OV( WA_SAIDA-VBELN ).
*
*      " Pega Materiais da OV original
*      SELECT *
*        FROM VBAP
*        INTO CORRESPONDING FIELDS OF TABLE IT_VBAP2
*        FOR ALL ENTRIES IN IT_0090
*        WHERE VBELN EQ IT_0090-VBELV.
*
*      FREE IT_OV.
*
*      LOOP AT IT_VBAP INTO WA_VBAP.
*
*        TRY .
*            WA_VBAP2 = IT_VBAP2[ MATNR = WA_VBAP-MATNR ].
*            DATA(PRECO) = IT_0090[ MATNR = WA_VBAP-MATNR ]-NETPRV.
*          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        ENDTRY.
*
*        WA_OV-VBELN = WA_VBAP2-VBELN. " OV original
*        WA_OV-MATNR = WA_VBAP2-MATNR.
*        WA_OV-POSNR = WA_VBAP2-POSNR.
*        WA_OV-ZMENG = WA_VBAP-KWMENG. " Quantidade estornada
*
*        WA_OV-NETPR = ZCL_UTIL_=>CHECK_DESC_ABS(
*                                                    _VBELN = WA_VBAP2-VBELN
*                                                    _POSNR = WA_VBAP2-POSNR
*                                                    DES    = CONV #( PRECO )
*                                                    DIR    = ABAP_FALSE
*                                                ).
*
*        TRY .
*            WA_OV-VRKME = IT_0090[ MATNR = WA_VBAP-MATNR ]-KMEINV.
*          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        ENDTRY.
*
*        APPEND WA_OV TO IT_OV.
*
*      ENDLOOP.
*
*      ZCL_UTIL_=>INDICATOR( |Retornando Quantidade para O.V de Origem!| ).
*      FREE TL_RETURN.
*      CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
*        EXPORTING
*          I_SOMA    = ABAP_TRUE
*          I_ACAO    = P_UCOMM
*        TABLES
*          IT_OV     = IT_OV
*          TE_RETURN = TL_RETURN.
*
*      IF NOT LINE_EXISTS( TL_RETURN[ TYPE = 'E' ] ).
*
*        LOOP AT IT_OV INTO WA_OV.
*          ZCL_UTIL_=>INDICATOR( |Reprocessa O.V { WA_OV-VBELN }, { WA_OV-MATNR }!| ).
*          ZCL_UTIL_=>PROCESSA_DESC( I_VBELN = WA_OV-VBELN I_MATNR = WA_OV-MATNR ).
*        ENDLOOP.
*
*        SELECT *
*         FROM ZSDT0090
*         INTO TABLE @DATA(T_90)
*          WHERE VBELV EQ @WA_SAIDA-VBELN
*             OR VBELN EQ @WA_SAIDA-VBELN
*          AND ESTORNO EQ @ABAP_FALSE.
*
*        LOOP AT T_90 INTO WA_0090.
*          ZCL_UTIL_=>INDICATOR( |Revertendo Hedge Seq { WA_0090-SEQUENCIA }!| ).
**         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
*          ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WA_0090-DOC_SIMULACAO
*                                                  I_VBELN  = WA_0090-VBELN
*                                                  I_SEQ    = WA_0090-SEQUENCIA
*                                                  I_TIPO   = 'EST'
*                                                 ).
*        ENDLOOP.
*
*        ZCL_UTIL_=>MODIFY_VLRTOT(
*                                  SIMULADOR = WA_SAIDA-DOC_SIMULACAO
*                                  T_0090   = CONV #( IT_0090 )
*                                ).
*
*        UPDATE  ZSDT0090
*              SET ESTORNO      = ABAP_TRUE
*                  USNAM_E      = SY-UNAME
*                  DATA_ATUAL_E = SY-DATUM
*                  HORA_ATUAL_E = SY-UZEIT
*              WHERE VBELV = WA_SAIDA-VBELN
*                 OR VBELN = WA_SAIDA-VBELN.
*        COMMIT WORK.
*
*      ENDIF.
*
**     "// Processa 40
*      ZCL_UTIL_=>LOOP_40(
*                            SIMULADOR = WA_SAIDA-DOC_SIMULACAO
*                            I_VBELN   = WA_OV-VBELN
*                            I_MATNR   = WA_OV-MATNR
*                            QTD       = 10
*                         ).
*
*      PERFORM F_EXIBE_BAPI.
*
    when 'ZDEF' or 'ZSEM' or 'ZFTE'
      or 'ZOFE' or 'ZOSM' or 'ZODF'.

      free tl_0090.

      select *
        from zsdt0090
        into table tl_0090
        where vbelv   eq wa_saida-vbeln
         and  categoria not in categoria
      and  estorno eq abap_false.

      if sy-subrc is initial.
        clear lv_msg.
        loop at tl_0090.
          lv_msg = |{ lv_msg }, { tl_0090-vbeln }|.
        endloop.

        concatenate 'A OV ' wa_saida-vbeln ' possui a(s) seguinte(s) ORDEN(s) subsequente(s):' lv_msg '.' into lv_msg separated by space.
        message lv_msg type 'I'.
        exit.
      endif.

      select count(*)
        from vbfa
        where vbelv = wa_saida-vbeln
        and   vbtyp_n = 'J'
      and   vbtyp_v = 'C'.

      if sy-subrc is initial.
        message 'Ordem possui documentos subsequentes, Verificar!' type 'I'.
        exit.
      endif.

*     "// Retorna a Quantidade das Alterações da Quantidade realizada nela.
      zcl_util_=>back_qtd( wa_saida-vbeln ).

*     Pega Materiais da OV estornada
      select *
       from vbap
         into corresponding fields of table it_vbap
      where vbeln eq wa_saida-vbeln.

*     "// Marca Estorno Controle de Transferencias de OV
      select *
        from zsdt0090
        into table @data(it_0090)
      where vbeln = @wa_saida-vbeln.

      check not it_0090 is initial.
      zcl_util_=>indicator( |Deletando O.V { wa_saida-vbeln }| ).

      zcl_util_=>delete_ov(
        exporting
          i_vbeln  = wa_saida-vbeln
        receiving
          t_return = tl_return[] ).

      if line_exists( tl_return[ type = 'E' ] ).
        perform f_exibe_bapi.
        exit.
      endif.

*     " Pega Materiais da OV original
      select *
        from vbap
        into corresponding fields of table it_vbap2
        for all entries in it_0090
      where vbeln eq it_0090-vbelv.

      free it_ov.

      loop at it_vbap into wa_vbap.

        try .
            wa_vbap2 = it_vbap2[ matnr = wa_vbap-matnr ].

            data(preco) = it_0090[ matnr = wa_vbap-matnr ]-netprv.

          catch cx_sy_itab_line_not_found.
        endtry.

        wa_ov-vbeln = wa_vbap2-vbeln. "// OV original
        wa_ov-matnr = wa_vbap2-matnr.
        wa_ov-posnr = wa_vbap2-posnr.
        wa_ov-zmeng = wa_vbap-kwmeng. "// Quantidade estornada.

        wa_ov-netpr = zcl_util_=>check_desc_abs(
          _vbeln = wa_vbap2-vbeln
          _posnr = wa_vbap2-posnr
          des    = conv #( preco )
          dir    = abap_false
        ).
        try .
            wa_ov-vrkme = it_0090[ matnr = wa_vbap-matnr ]-kmeinv.

          catch cx_sy_itab_line_not_found.
        endtry.

        append wa_ov to it_ov.
      endloop.

      zcl_util_=>indicator( |Retornando Quantidade para O.V de Origem!| ).
      free tl_return.
      call function 'ZSDMF001_ATUALI_OV_SIM'
        exporting
          i_soma    = abap_true
          i_acao    = p_ucomm
        tables
          it_ov     = it_ov
          te_return = tl_return.

      if not line_exists( tl_return[ type = 'E' ] ).

        loop at it_ov into wa_ov.
          zcl_util_=>indicator( |Reprocessa O.V { wa_ov-vbeln }, { wa_ov-matnr }!| ).
          zcl_util_=>processa_desc( i_vbeln = wa_ov-vbeln i_matnr = wa_ov-matnr ).
        endloop.

        select *
         from zsdt0090
         into table @data(t_90)
          where vbelv eq @wa_saida-vbeln
             or vbeln eq @wa_saida-vbeln
        and estorno eq @abap_false.

        data ls_0090 type zsdt0090. " 10.09.2024 - RAMON - 97513


        loop at t_90 into wa_0090.
          zcl_util_=>indicator( |Revertendo Hedge Seq { wa_0090-sequencia }!| ).
*
          " 10.09.2024 - RAMON - 97513 -->

          move-corresponding wa_0090 to ls_0090.

          " 22.06.2023 - RAMON - NO CASO DE ESTORNO DE 'FATURADO',
          " NÃO FOI DISPARADO HEDGE NA OV DESMEMBRADA, ENTÃO ANTES DE MANDAR ESTORNAR O HEDGE DE ALGO QUE NAO EXISTE
          " VERIFICA SE EXISTE NA 94,

          select count(*) from zsdt0094
              where fixacao    eq wa_0090-sequencia
                and nro_sol_ov eq wa_0090-doc_simulacao
                and programa   eq sy-cprog
          and estorno    eq 0.

          " SE NAO TIVER, TENHO QUE VER SE TEM VINCULO E PASSAR O VINCULO
          if sy-dbcnt < 1.

            "TEM VINCULO, PORQUE DAI ESTORNA O HEDGE DA OV PRINCIPAL.

            select single sequencia,sequenciav,data_prevpgto,data_prevpgtov from zsdt0090
              into @data(ls_seqs)
                where doc_simulacao = @wa_0090-doc_simulacao
            and sequenciao = @wa_0090-sequencia.

            if sy-subrc eq 0.

              break rblima.

              wa_0090-sequencia = ls_seqs-sequencia.

              " 01.02.2024 - RAMON - fizemos uma alteração aqui pq estava estornando o hedge da ov nova com a data errada
              " no caso era para ser a mesma da entrada do hedge 06.02.2024, mas estava colocando 05.02.2023
              " - doc_simu: 351775, ov: 12795035 -->


              " "ls_0090-data_prevpgto = ls_seqs-data_prevpgto.
              "ls_0090-data_prevpgtov = ls_seqs-data_prevpgtov.


              " se tiver vencido, passa o vencido, o hedge corrije o valor sozinho
              if ls_seqs-data_prevpgtov < sy-datum.
                ls_0090-data_prevpgtov = ls_seqs-data_prevpgtov.
                " se nao tiver vencido, passa a prev pgto
              else.
                ls_0090-data_prevpgtov = ls_seqs-data_prevpgto.
              endif.

*              IF ls_seqs-sequenciav IS NOT INITIAL.
*                wa_0090-sequencia = ls_seqs-sequenciav.
*              ENDIF.


            endif.

          endif.

          " CASO CONTRARIO, PODE EXECUTAR COMO FAZ HJ

          " 10.09.2024 - RAMON - 97513 <--


          "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
          zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_0090-doc_simulacao
                                                  i_vbeln  = wa_0090-vbeln
                                                  i_seq    = wa_0090-sequencia
                                                  " 10.09.2024 - RAMON - 97513 -->
                                                  " 22.06.2023 - foi colocado a estrutura aqui para
                                                  " poder estornar o hedge e voltar para a data da ultima previsao
                                                  i_0090   = ls_0090
                                                  " 10.09.2024 - RAMON - 97513 --<
                                                  i_tipo   = 'EST'
                                                  ).
        endloop.

        zcl_util_=>modify_vlrtot(
          simulador = wa_saida-doc_simulacao
          t_0090    = conv #( it_0090 )
        ).

        " 29.06.2023 - 98623 - RAMON - #descomentar ->
        if sy-sysid <> 'PRD'.
          update  zsdt0090
              set estorno      = abap_true
                  usnam_e      = sy-uname
                  data_atual_e = sy-datum
                  hora_atual_e = sy-uzeit
              where doc_simulacao = wa_saida-doc_simulacao
                and sequencia = wa_0090-sequencia.

          commit work and wait.
        endif.

        " 29.06.2023 - 98623 - RAMON -<

        update  zsdt0090
              set estorno      = abap_true
                  usnam_e      = sy-uname
                  data_atual_e = sy-datum
                  hora_atual_e = sy-uzeit
              where vbelv = wa_saida-vbeln
                 or vbeln = wa_saida-vbeln.


        " 29.06.2023 - 114378 - RAMON ->
        "COMMIT WORK.
        commit work and wait.

        if sy-sysid <> 'PRD'.

          perform f_recompoe_zsdt0090 tables t_90[].

        endif.
        " 29.06.2023 - 114378 - RAMON -<

      endif.

      " 16.05.2023 - 98623 - RAMON ->
      if sy-sysid <> 'PRD'.
        " Estorna da zsdt0315
        perform f_canc_lcto_zsdt0315 using wa_saida-vbeln.

        " Rcompoe o valor na OV original
        perform f_reco_lcto_zsdt0315 using wa_ov-vbeln.

        perform f_hedge_apagar_vinculo using wa_saida-vbeln.
      endif.
      " 16.05.2023 - 98623 - RAMON -<
*     "// Processa 40
      zcl_util_=>loop_40(
        simulador = wa_saida-doc_simulacao
        i_vbeln   = wa_ov-vbeln
        i_matnr   = wa_ov-matnr
        qtd       = 10
      ).

      perform f_exibe_bapi.

    when 'ZREM'.

      free tl_0090.

      select *
        from zsdt0090
        into table tl_0090
        where vbelv   eq wa_saida-vbeln
         and  categoria not in categoria
      and  estorno eq abap_false.

      if sy-subrc is initial.
        loop at tl_0090.
          concatenate lv_msg tl_0090-vbeln into lv_msg separated by ','.
        endloop.

        concatenate 'A OV ' wa_saida-vbeln ' possui a(s) seguinte(s) ORDEN(s) subsequente(s):' lv_msg '.' into lv_msg separated by space.
        message lv_msg type 'I'.
      else.

        select  vbelv posnv rfmng vbeln
          from vbfa
          into table it_vbfa
          where vbelv = wa_saida-vbeln
           and  vbtyp_n = 'M'
        and  vbtyp_v = 'C'.

        clear wg_exit.

        loop at it_vbfa into wa_vbfa.
          select single vbelv posnv rfmng vbeln
             from vbfa
             into wa_vbfa2
             where vbelv = wa_vbfa-vbeln
             and   vbtyp_n = 'N'
          and   vbtyp_v = 'M'.

          if sy-subrc ne 0.
            wg_exit = abap_true.
            message 'Ordem possui documentos subsequentes, verificar!' type 'I'.
            exit.
          endif.
        endloop.

        check wg_exit is initial.

        if it_vbfa[] is not initial.
          loop at it_vbfa into wa_vbfa.

            "Pega Materiais da OV estornada
            select *
             from vbap
             into corresponding fields of table it_vbap
            where vbeln eq wa_saida-vbeln.

*            ZCL_UTIL_=>DELETE_OV( WA_SAIDA-VBELN ).

            zcl_util_=>delete_ov(
              exporting
                i_vbeln  = wa_saida-vbeln
              receiving
                t_return = tl_return[] ).

            if line_exists( tl_return[ type = 'E' ] ).
              perform f_exibe_bapi.
              exit.
            endif.

            free it_0090.

            select *
             from zsdt0090
             into table it_0090
            where vbeln = wa_saida-vbeln.

            check not it_0090 is initial.

            " Pega Materiais da OV original
            select *
              from vbap
              into corresponding fields of table it_vbap2
              for all entries in it_0090
            where vbeln eq it_0090-vbelv.

            free it_ov.

            loop at it_vbap into wa_vbap.

              try .
                  wa_vbap2 = it_vbap2[ matnr = wa_vbap-matnr ].
                  preco = it_0090[ matnr = wa_vbap-matnr ]-netprv.
                catch cx_sy_itab_line_not_found.
              endtry.

              wa_ov-vbeln = wa_vbap2-vbeln. " OV original
              wa_ov-matnr = wa_vbap2-matnr.
              wa_ov-posnr = wa_vbap2-posnr.
              wa_ov-zmeng = wa_vbap-kwmeng. " Quantidade estornada

              wa_ov-netpr = zcl_util_=>check_desc_abs(
                _vbeln = wa_vbap2-vbeln
                _posnr = wa_vbap2-posnr
                des    = conv #( preco )
                dir    = abap_false
              ).

              try .
                  wa_ov-vrkme = it_0090[ matnr = wa_vbap-matnr ]-kmeinv.
                catch cx_sy_itab_line_not_found.
              endtry.

              append wa_ov to it_ov.
            endloop.

            free tl_return.
            call function 'ZSDMF001_ATUALI_OV_SIM'
              exporting
                i_soma    = abap_true
                i_acao    = p_ucomm
              tables
                it_ov     = it_ov
                te_return = tl_return.

            if not line_exists( tl_return[ type = 'E' ] ).

              loop at it_ov into wa_ov.
                zcl_util_=>processa_desc( i_vbeln = wa_ov-vbeln i_matnr = wa_ov-matnr ).
*               "// Processa 40
                zcl_util_=>loop_40(
                  simulador = wa_saida-doc_simulacao
                  i_vbeln   = wa_ov-vbeln
                  i_matnr   = wa_ov-matnr
                  qtd       = 10
                ).
              endloop.

              loop at it_0090 into wa_0090.
                if sy-tabix eq 1.
*                 "// Marca como Estornado dos os Itens da OV na ZSDT0090
                  update  zsdt0090 set estorno      = abap_true
                                       usnam_e      = sy-uname
                                       data_atual_e = sy-datum
                                       hora_atual_e = sy-uzeit
                    where vbeln = wa_0090-vbeln.

                  commit work.
                endif.
*               "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
                zcl_webservice_tx_curva=>hedge_insumos( i_vbeln = wa_0090-vbeln
                                                        i_tipo  = 'EST'
                                                        ).
              endloop.

            endif.

          endloop.

          perform f_exibe_bapi.

        else.

          "Pega Materiais da OV estornada
          select vbeln posnr matnr arktx werks charg kwmeng vrkme netpr netwr
           from vbap
            into table it_vbap
          where vbeln eq wa_saida-vbeln.

*          ZCL_UTIL_=>DELETE_OV( WA_SAIDA-VBELN ).
          zcl_util_=>delete_ov(
            exporting
              i_vbeln  = wa_saida-vbeln
            receiving
              t_return = tl_return[] ).

          if line_exists( tl_return[ type = 'E' ] ).
            perform f_exibe_bapi.
            exit.
          endif.

          select *
           from zsdt0090
           into table it_0090
          where vbeln = wa_saida-vbeln.

          check not it_0090 is initial.

          " Pega Materiais da OV original
          select *
            from vbap
            into corresponding fields of table it_vbap2
            for all entries in it_0090
          where vbeln eq it_0090-vbelv.

          free it_ov.

          loop at it_vbap into wa_vbap.

            try .
                wa_vbap2 = it_vbap2[ matnr = wa_vbap-matnr ].
                preco = it_0090[ matnr = wa_vbap-matnr ]-netprv.
              catch cx_sy_itab_line_not_found.
            endtry.

            wa_ov-vbeln = wa_vbap2-vbeln. " OV original
            wa_ov-matnr = wa_vbap2-matnr.
            wa_ov-posnr = wa_vbap2-posnr.
            wa_ov-zmeng = wa_vbap-kwmeng. " Quantidade estornada

            wa_ov-netpr = zcl_util_=>check_desc_abs(
              _vbeln = wa_vbap2-vbeln
              _posnr = wa_vbap2-posnr
              des    = conv #( preco )
              dir    = abap_false
            ).

            try .
                wa_ov-vrkme = it_0090[ matnr = wa_vbap-matnr ]-kmeinv.
              catch cx_sy_itab_line_not_found.
            endtry.

            append wa_ov to it_ov.
          endloop.

          free tl_return.
          call function 'ZSDMF001_ATUALI_OV_SIM'
            exporting
              i_soma    = abap_true
              i_acao    = p_ucomm
            tables
              it_ov     = it_ov
              te_return = tl_return.

          if not line_exists( tl_return[ type = 'E' ] ).

            loop at it_ov into wa_ov.
              zcl_util_=>processa_desc( i_vbeln = wa_ov-vbeln i_matnr = wa_ov-matnr ).
*               "// Processa 40
              zcl_util_=>loop_40(
                simulador = wa_saida-doc_simulacao
                i_vbeln   = wa_ov-vbeln
                i_matnr   = wa_ov-matnr
                qtd       = 10
              ).
            endloop.

            loop at it_0090 into wa_0090.
              if sy-tabix eq 1.
*         "// Marca como Estornado dos os Itens da OV na ZSDT0090
                update  zsdt0090 set estorno      = abap_true
                                     usnam_e      = sy-uname
                                     data_atual_e = sy-datum
                                     hora_atual_e = sy-uzeit
                  where vbeln = wa_0090-vbeln.

                commit work.
              endif.
*         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
              zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_0090-doc_simulacao
                                                      i_vbeln  = wa_0090-vbeln
                                                      i_seq    = wa_0090-sequencia
                                                      i_tipo   = 'EST'
                                                      ).
            endloop.

          endif.

          perform f_exibe_bapi.

        endif.
      endif.

    when 'ZRFU'.

      free: it_est_zrfu.

      select count(*)
        from vbfa
        where vbelv = wa_saida-vbeln
        and   vbtyp_n in ('J')
      and   vbtyp_v in ('C').

      if sy-subrc is initial.
        message 'Ordem possui documentos subsequentes, Verificar!' type 'I'.
        exit.
      endif.

      select *
        from vbfa
        into table @data(t_vbfa)
        where vbeln = @wa_saida-vbeln
        and   vbtyp_n in ('C')
      and   vbtyp_v in ('M', 'L').

      try .
          it_est_zrfu[] =
          value #(
                   ( etapa = 3 desc = 'ZRFU'   vbeln = wa_saida-vbeln )
                   ( etapa = 2 desc = 'FATURA' vbeln = t_vbfa[ vbtyp_n  = 'C' vbtyp_v = 'M' ]-vbelv )
                   ( etapa = 1 desc = 'ZFUT'   vbeln = t_vbfa[ vbtyp_n  = 'C' vbtyp_v = 'L' ]-vbelv )
                 ).
        catch cx_sy_itab_line_not_found.
      endtry.

      sort it_est_zrfu by etapa descending.

      loop at it_est_zrfu.
        perform estorno_erros using it_est_zrfu-vbeln it_est_zrfu-etapa.
        if it_est_zrfu-etapa ne 2.

          update  zsdt0090 set estorno      = abap_true
                               usnam_e      = sy-uname
                               data_atual_e = sy-datum
                               hora_atual_e = sy-uzeit
            where vbeln = it_est_zrfu-vbeln.
          commit work.

          zcl_webservice_tx_curva=>hedge_insumos(
            i_vbeln = it_est_zrfu-vbeln
            i_tipo  = 'EST'
          ).
        endif.
      endloop.

      perform f_exibe_bapi.

    when 'ZTRI'.

      free: tg_vbfa, it_est.

      select distinct vbeln rfmng
        from vbfa
        into table tg_vbfa
        where vbelv   eq wa_saida-vbeln
         and  posnv   eq wa_saida-posnr
         and  vbtyp_n eq 'M'
         and  vbtyp_v eq 'L'
      and  fktyp   eq 'A'.

      if sy-subrc is initial.

        read table tg_vbfa index 1.

        select single *
        into @data(wa_lin)
        from j_1bnflin
        where refkey eq @tg_vbfa-vbeln.

        if sy-subrc is initial.
          select single *
                into @data(wa_active)
                from j_1bnfe_active
          where docnum eq @wa_lin-docnum.

          if wa_active-docsta is initial and
             wa_active-scssta is initial and
             wa_active-cancel is initial.
          else.
            if wa_active-docsta eq 1 and
               wa_active-scssta eq 2 and
               wa_active-cancel eq abap_true.
            else.
              message 'Ordem Possui Nfe Ativa, Verificar!' type 'I'.
              exit.
            endif.
          endif.

        endif.
      endif.

      select  vbelv posnv rfmng vbeln
        from vbfa
        into table it_vbfa
        where vbelv = wa_saida-vbeln
        and   vbtyp_n = 'M'
      and   vbtyp_v = 'L'.

      if sy-subrc is initial.
        clear wg_exit.
        loop at it_vbfa into wa_vbfa.

*      verifica se ja esta estornado
          select single vbelv posnv rfmng vbeln
             from vbfa
             into wa_vbfa2
             where vbelv = wa_vbfa-vbeln
             and   vbtyp_n = 'N'
          and   vbtyp_v = 'M'.

          if sy-subrc is initial.
            append value #( etapa = 1 desc = 'ZTRI'   vbeln = wa_saida-vbeln ) to it_est.
          else.
*            APPEND VALUE #( ETAPA = 2 DESC = 'FATURA' VBELN = WA_SAIDA-VBELN ) TO IT_EST.
            append value #( etapa = 2 desc = 'FATURA' vbeln = wa_vbfa-vbeln ) to it_est.
            append value #( etapa = 1 desc = 'ZTRI'   vbeln = wa_saida-vbeln ) to it_est.
          endif.

        endloop.
      else.
*        APPEND VALUE #( ETAPA = 1 DESC = 'ZTRI'   VBELN = WA_SAIDA-VBELN ) TO IT_EST.
        " Se não existir Fatura, apenas deletar a OV
        append value #( etapa = 3 desc = 'ZTRI'   vbeln = wa_saida-vbeln ) to it_est.
      endif.

      sort it_est by etapa descending.

      loop at it_est.
        perform estorno_erros using it_est-vbeln it_est-etapa.
        if it_est-etapa ne 2.

          update  zsdt0090 set estorno      = abap_true
                               usnam_e      = sy-uname
                               data_atual_e = sy-datum
                               hora_atual_e = sy-uzeit
            where vbeln = it_est-vbeln.
          commit work.

          zcl_webservice_tx_curva=>hedge_insumos(
            i_vbeln = it_est-vbeln
            i_tipo  = 'EST'
          ).
        endif.
      endloop.

      perform f_exibe_bapi.

    when others.

      message 'Tipo de OV não previsto para ser estornado' type 'I'.

  endcase.

  if ( 'ZDEF_ZSEM_ZFTE' cs wa_saida-auart ) or ( wa_zsdt0090_zrfu is not initial ).
***
***    FREE TL_0090.
***
***    SELECT *
***      FROM ZSDT0090
***      INTO TABLE TL_0090
***      WHERE VBELV   EQ WA_SAIDA-VBELN
***       AND  CATEGORIA NOT IN ( 'O', 'V' )
***       AND  ESTORNO EQ ABAP_FALSE.
***
***    IF SY-SUBRC IS INITIAL.
***      CLEAR LV_MSG.
***      LOOP AT TL_0090.
***        LV_MSG = |{ LV_MSG }, { TL_0090-VBELN }|.
***      ENDLOOP.
***
***      CONCATENATE 'A OV ' WA_SAIDA-VBELN ' possui a(s) seguinte(s) ORDEN(s) subsequente(s):' LV_MSG '.' INTO LV_MSG SEPARATED BY SPACE.
***      MESSAGE LV_MSG TYPE 'I'.
***    ELSE.
***      SELECT SINGLE VBELV POSNV RFMNG VBELN
***        FROM VBFA
***        INTO WA_VBFA
***        WHERE VBELV = WA_SAIDA-VBELN
***        AND   VBTYP_N = 'J'
***        AND   VBTYP_V = 'C'.
***
***      IF SY-SUBRC IS INITIAL.
***        MESSAGE 'Ordem possui documentos subsequentes, Verificar!' TYPE 'I'.
***      ELSE.
****        Pega Materiais da OV estornada
***        SELECT VBELN POSNR MATNR ARKTX WERKS CHARG KWMENG VRKME NETPR NETWR
***          FROM VBAP
***          INTO TABLE IT_VBAP
***          WHERE VBELN EQ WA_SAIDA-VBELN.
***
****        DELETE IT_VBAP WHERE MATNR NE WA_SAIDA-MATNR.
****        READ TABLE IT_VBAP INTO WA_VBAP INDEX 1.
***        "
***        REFRESH TL_RETURN.
***        CLEAR F_HEADINX.
***        F_HEADINX-UPDATEFLAG = 'D'.
***        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
***          EXPORTING
***            SALESDOCUMENT    = WA_SAIDA-VBELN
***            ORDER_HEADER_INX = F_HEADINX
***          TABLES
***            RETURN           = TL_RETURN.
***
***        COMMIT WORK.
***
***        PERFORM F_EXIBE_BAPI.
***
***        READ TABLE TL_RETURN WITH KEY TYPE = 'E'.
***        IF SY-SUBRC IS NOT INITIAL.
****          " Marca Estorno Controle de Transferencias de OV
***
***          SELECT *
***            FROM ZSDT0090
***            INTO TABLE IT_0090
***            WHERE VBELN = WA_SAIDA-VBELN.
***
***          CHECK NOT IT_0090 IS INITIAL.
***
***          LOOP AT IT_0090 INTO WA_0090.
***
***            IF SY-TABIX EQ 1.
***              UPDATE  ZSDT0090 SET ESTORNO      = ABAP_TRUE
***                                   USNAM_E      = SY-UNAME
***                                   DATA_ATUAL_E = SY-DATUM
***                                   HORA_ATUAL_E = SY-UZEIT
***                WHERE VBELN = WA_0090-VBELN.
***              COMMIT WORK.
***            ENDIF.
***
***            ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WA_0090-DOC_SIMULACAO
***                                                    I_VBELN  = WA_0090-VBELN
***                                                    I_SEQ    = WA_0090-SEQUENCIA
***                                                    I_TIPO   = 'EST'
***                                                   ).
***          ENDLOOP.
***
****          " Pega Materiais da OV original
***          SELECT VBELN POSNR MATNR ARKTX WERKS CHARG KWMENG VRKME NETPR NETWR
***            FROM VBAP
***            INTO TABLE IT_VBAP2
***            FOR ALL ENTRIES IN IT_0090
***            WHERE VBELN EQ IT_0090-VBELV.
***
***          REFRESH IT_OV.
***          SORT IT_VBAP2 BY MATNR.
***          LOOP AT IT_VBAP INTO WA_VBAP.
***            READ TABLE IT_VBAP2 INTO WA_VBAP2 WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
***
***            WA_OV-VBELN = WA_VBAP2-VBELN. "// OV original
***            WA_OV-POSNR = WA_VBAP2-POSNR.
***            WA_OV-ZMENG = WA_VBAP-KWMENG. "// Quantidade estornada.
***
***            TRY .
***                WA_OV-NETPR = IT_0090[ MATNR = WA_VBAP-MATNR ]-NETPRV.
***                WA_OV-VRKME = IT_0090[ MATNR = WA_VBAP-MATNR ]-KMEINV.
***              CATCH CX_SY_ITAB_LINE_NOT_FOUND.
***            ENDTRY.
***
***            APPEND WA_OV TO IT_OV.
***          ENDLOOP.
***
***          REFRESH TL_RETURN.
***          CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
***            EXPORTING
***              I_SOMA    = ABAP_TRUE
***              I_ACAO    = P_UCOMM
***            TABLES
***              IT_OV     = IT_OV
***              TE_RETURN = TL_RETURN.
***
***          "
***          PERFORM F_EXIBE_BAPI.
***        ENDIF.
***
***      ENDIF.
***    ENDIF.
***
  elseif wa_saida-auart = 'ZRFU'.
***
***    FREE: IT_EST_ZRFU.
***
***    SELECT COUNT(*)
***      FROM VBFA
***      WHERE VBELV = WA_SAIDA-VBELN
***      AND   VBTYP_N IN ('J')
***      AND   VBTYP_V IN ('C').
***
***    IF SY-SUBRC IS INITIAL.
***      MESSAGE 'Ordem possui documentos subsequentes, Verificar!' TYPE 'I'.
***      EXIT.
***    ELSE.
***
***      SELECT *
***        FROM VBFA
***        INTO TABLE @DATA(T_VBFA)
***        WHERE VBELN = @WA_SAIDA-VBELN
***        AND   VBTYP_N IN ('C')
***        AND   VBTYP_V IN ('M', 'L').
***
***      CLEAR IT_EST_ZRFU.
***      IT_EST_ZRFU-ETAPA = 3.
***      IT_EST_ZRFU-DESC = 'ZRFU'.
***      IT_EST_ZRFU-VBELN = WA_SAIDA-VBELN.
***
***      APPEND IT_EST_ZRFU.
***
***      CLEAR IT_EST_ZRFU.
***      IT_EST_ZRFU-ETAPA = 2.
***      IT_EST_ZRFU-DESC = 'FATURA'.
***
***      TRY .
***          IT_EST_ZRFU-VBELN = T_VBFA[ VBTYP_N  = 'C' VBTYP_V = 'M' ]-VBELV.
***        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
***      ENDTRY.
***
***      APPEND IT_EST_ZRFU.
***
***      CLEAR IT_EST_ZRFU.
***      IT_EST_ZRFU-ETAPA = 1.
***      IT_EST_ZRFU-DESC = 'ZFUT'.
***
***      TRY .
***          IT_EST_ZRFU-VBELN = T_VBFA[ VBTYP_N  ='C' VBTYP_V = 'L' ]-VBELV.
***        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
***      ENDTRY.
***
***      APPEND IT_EST_ZRFU.
***
***      SORT IT_EST_ZRFU BY ETAPA DESCENDING.
***
***      LOOP AT IT_EST_ZRFU.
***        PERFORM ESTORNO_ERROS USING IT_EST_ZRFU-VBELN IT_EST_ZRFU-ETAPA.
***        IF IT_EST_ZRFU-ETAPA NE 2.
***
***          UPDATE  ZSDT0090 SET ESTORNO      = ABAP_TRUE
***                               USNAM_E      = SY-UNAME
***                               DATA_ATUAL_E = SY-DATUM
***                               HORA_ATUAL_E = SY-UZEIT
***            WHERE VBELN = IT_EST_ZRFU-VBELN.
***          COMMIT WORK.
***
***          ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_VBELN = IT_EST_ZRFU-VBELN
***                                                  I_TIPO   = 'EST'
***                                                ).
***        ENDIF.
***      ENDLOOP.
***    ENDIF.
***    PERFORM F_EXIBE_BAPI.
***
  elseif 'ZOFE_ZOSM_ZODF' cs wa_saida-auart.
****
****    SELECT SINGLE VBELV POSNV RFMNG VBELN
****      FROM VBFA
****      INTO WA_VBFA
****      WHERE VBELV = WA_SAIDA-VBELN.
****
****    IF SY-SUBRC EQ 0.
****      MESSAGE 'Ordem possui documentos subsequentes, verificar!' TYPE 'I'.
****      EXIT.
****    ENDIF.
****
****    "Pega Materiais da OV estornada
****    SELECT *
****     FROM VBAP
****     INTO CORRESPONDING FIELDS OF TABLE IT_VBAP
****     WHERE VBELN EQ WA_SAIDA-VBELN.
****
****    SELECT SINGLE *
****     FROM VBAK
****     INTO W_VBAK
****     WHERE VBELN EQ WA_SAIDA-VBELN.
****
****    SELECT SINGLE *
****      FROM V_KONV " ---> S4 Migration - 10/07/2023 - DG
****      INTO W_KONV
****      WHERE KNUMV EQ W_VBAK-KNUMV
****        AND KSCHL EQ 'RB00'.
****
****    CLEAR F_HEADINX.
****    F_HEADINX-UPDATEFLAG = 'D'.
****    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
****      EXPORTING
****        SALESDOCUMENT    = WA_SAIDA-VBELN
****        ORDER_HEADER_INX = F_HEADINX
****      TABLES
****        RETURN           = TL_RETURN.
****
****    IF NOT LINE_EXISTS( TL_RETURN[ TYPE = 'E' ] ).
****      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
****        EXPORTING
****          WAIT = ABAP_TRUE.
****    ENDIF.
****
****    SELECT *
****     FROM ZSDT0090
****     INTO TABLE IT_0090
****     WHERE VBELN = WA_SAIDA-VBELN.
****
****    CHECK NOT IT_0090 IS INITIAL.
****
****    " Pega Materiais da OV original
****    SELECT *
****      FROM VBAP
****      INTO CORRESPONDING FIELDS OF TABLE IT_VBAP2
****      FOR ALL ENTRIES IN IT_0090
****      WHERE VBELN EQ IT_0090-VBELV.
****
****    REFRESH IT_OV.
****
****    SORT IT_VBAP2 BY MATNR.
****
****    LOOP AT IT_VBAP INTO WA_VBAP.
****      READ TABLE IT_VBAP2 INTO WA_VBAP2 WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
****
****      FREE IT_DESC_ABS.
****      APPEND VALUE #(
****                       VBELN         = WA_VBAP2-VBELN
****                       POSNR         = WA_VBAP2-POSNR
****                       MATNR         = WA_VBAP2-MATNR
****                       DESC_ABSOLUTO = W_KONV-KBETR
****                    ) TO IT_DESC_ABS.
****
****      WA_OV-VBELN = WA_VBAP2-VBELN. " OV original
****      WA_OV-POSNR = WA_VBAP2-POSNR.
****      WA_OV-ZMENG = WA_VBAP-KWMENG. " Quantidade estornada.
****
****      TRY .
****          WA_OV-NETPR = IT_0090[ MATNR = WA_VBAP-MATNR ]-NETPRV.
****          WA_OV-VRKME = IT_0090[ MATNR = WA_VBAP-MATNR ]-KMEINV.
****        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
****      ENDTRY.
****
****      APPEND WA_OV TO IT_OV.
****    ENDLOOP.
****
****    REFRESH TL_RETURN.
****    CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
****      EXPORTING
****        I_SOMA    = ABAP_TRUE
****        I_ACAO    = P_UCOMM
****      TABLES
****        IT_OV     = IT_OV
****        TE_RETURN = TL_RETURN.
****
****    READ TABLE TL_RETURN WITH KEY TYPE = 'E'.
****    IF SY-SUBRC NE 0.
****
****      CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
****        TABLES
****          TI_ITENS_OV       = IT_DESC_ABS
****        EXCEPTIONS
****          OV_NAO_ENCONTRADA = 1
****          OTHERS            = 2.
****
****      LOOP AT IT_0090 INTO WA_0090.
****        IF SY-TABIX EQ 1.
****          UPDATE  ZSDT0090 SET ESTORNO      = ABAP_TRUE
****                               USNAM_E      = SY-UNAME
****                               DATA_ATUAL_E = SY-DATUM
****                               HORA_ATUAL_E = SY-UZEIT
****            WHERE VBELN = WA_0090-VBELN.
****
****          COMMIT WORK.
****        ENDIF.
****        ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WA_0090-DOC_SIMULACAO
****                                                I_VBELN  = WA_0090-VBELN
****                                                I_SEQ    = WA_0090-SEQUENCIA
****                                                I_TIPO   = 'EST'
****                                               ).
****      ENDLOOP.
****
****    ENDIF.
****
****    PERFORM F_EXIBE_BAPI.
****
  elseif wa_saida-auart = 'ZTRI'.
*
*    FREE: TG_VBFA, IT_EST.
*
*** SELECIONANDO NÚMERO DAS FATURAS
*    SELECT DISTINCT VBELN RFMNG
*      FROM VBFA
*      INTO TABLE TG_VBFA
*      WHERE VBELV   EQ WA_SAIDA-VBELN
*       AND  POSNV   EQ WA_SAIDA-POSNR
*       AND  VBTYP_N EQ 'M'
*       AND  VBTYP_V EQ 'L'
*       AND  FKTYP   EQ 'A'.
*
*    IF SY-SUBRC IS INITIAL.
*
*      READ TABLE TG_VBFA INDEX 1.
*
*      SELECT SINGLE *
*      INTO @DATA(WA_LIN)
*      FROM J_1BNFLIN
*      WHERE REFKEY EQ @TG_VBFA-VBELN.
*
*      IF SY-SUBRC IS INITIAL.
*        SELECT SINGLE *
*              INTO @DATA(WA_ACTIVE)
*              FROM J_1BNFE_ACTIVE
*              WHERE DOCNUM EQ @WA_LIN-DOCNUM.
*
*        IF WA_ACTIVE-DOCSTA IS INITIAL AND
*           WA_ACTIVE-SCSSTA IS INITIAL AND
*           WA_ACTIVE-CANCEL IS INITIAL.
*        ELSE.
*          IF WA_ACTIVE-DOCSTA EQ 1 AND
*             WA_ACTIVE-SCSSTA EQ 2 AND
*             WA_ACTIVE-CANCEL EQ ABAP_TRUE.
*          ELSE.
*            MESSAGE 'Ordem Possui Nfe Ativa, Verificar!' TYPE 'I'.
*            EXIT.
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
*
*    SELECT  VBELV POSNV RFMNG VBELN
*      FROM VBFA
*      INTO TABLE IT_VBFA
*      WHERE VBELV = WA_SAIDA-VBELN
*      AND   VBTYP_N = 'M'
*      AND   VBTYP_V = 'L'.
*
*    IF SY-SUBRC IS INITIAL.
*      CLEAR WG_EXIT.
*      LOOP AT IT_VBFA INTO WA_VBFA.
*
**      verifica se ja esta estornado
*        SELECT SINGLE VBELV POSNV RFMNG VBELN
*           FROM VBFA
*           INTO WA_VBFA2
*           WHERE VBELV = WA_VBFA-VBELN
*           AND   VBTYP_N = 'N'
*           AND   VBTYP_V = 'M'.
*
*        IF SY-SUBRC IS INITIAL.
*          CLEAR IT_EST.
*          IT_EST-ETAPA = 1.
*          IT_EST-DESC = 'ZTRI'.
*          IT_EST-VBELN = WA_SAIDA-VBELN.
*          APPEND IT_EST.
*
*        ELSE.
*
*          CLEAR IT_EST.
*          IT_EST-ETAPA = 2.
*          IT_EST-DESC = 'FATURA'.
*          IT_EST-VBELN = WA_VBFA-VBELN.
*          APPEND IT_EST.
*
*          CLEAR IT_EST.
*          IT_EST-ETAPA = 1.
*          IT_EST-DESC = 'ZTRI'.
*          IT_EST-VBELN = WA_SAIDA-VBELN.
*          APPEND IT_EST.
*
*        ENDIF.
*      ENDLOOP.
*    ELSE.
*      CLEAR IT_EST.
*      IT_EST-ETAPA = 1.
*      IT_EST-DESC = 'ZTRI'.
*      IT_EST-VBELN = WA_SAIDA-VBELN.
*      APPEND IT_EST.
*    ENDIF.
*
*    LOOP AT IT_EST.
*      PERFORM ESTORNO_ERROS USING IT_EST-VBELN IT_EST-ETAPA.
*      IF IT_EST-ETAPA NE 2.
*
*        UPDATE  ZSDT0090 SET ESTORNO      = ABAP_TRUE
*                             USNAM_E      = SY-UNAME
*                             DATA_ATUAL_E = SY-DATUM
*                             HORA_ATUAL_E = SY-UZEIT
*          WHERE VBELN = IT_EST-VBELN.
*        COMMIT WORK.
*
*        ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_VBELN = IT_EST-VBELN
*                                                I_TIPO   = 'EST'
*                                              ).
*      ENDIF.
*    ENDLOOP.
*    PERFORM F_EXIBE_BAPI.
*
  elseif wa_saida-auart = 'ZREM'.
*
*    FREE TL_0090.
*
*    SELECT *
*      FROM ZSDT0090
*      INTO TABLE TL_0090
*      WHERE VBELV   EQ WA_SAIDA-VBELN
*       AND  ESTORNO EQ ABAP_FALSE.
*
*    IF SY-SUBRC IS INITIAL.
*      LOOP AT TL_0090.
*        CONCATENATE LV_MSG TL_0090-VBELN INTO LV_MSG SEPARATED BY ','.
*      ENDLOOP.
*
*      CONCATENATE 'A OV ' WA_SAIDA-VBELN ' possui a(s) seguinte(s) ORDEN(s) subsequente(s):' LV_MSG '.' INTO LV_MSG SEPARATED BY SPACE.
*      MESSAGE LV_MSG TYPE 'I'.
*    ELSE.
*
*      SELECT  VBELV POSNV RFMNG VBELN
*        FROM VBFA
*        INTO TABLE IT_VBFA
*        WHERE VBELV = WA_SAIDA-VBELN
*         AND  VBTYP_N = 'M'
*         AND  VBTYP_V = 'C'.
*
*      CLEAR WG_EXIT.
*
*      LOOP AT IT_VBFA INTO WA_VBFA.
*        SELECT SINGLE VBELV POSNV RFMNG VBELN
*           FROM VBFA
*           INTO WA_VBFA2
*           WHERE VBELV = WA_VBFA-VBELN
*           AND   VBTYP_N = 'N'
*           AND   VBTYP_V = 'M'.
*
*        IF SY-SUBRC NE 0.
*          WG_EXIT = ABAP_TRUE.
*          MESSAGE 'Ordem possui documentos subsequentes, verificar!' TYPE 'I'.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
*      CHECK WG_EXIT IS INITIAL.
*
*      IF IT_VBFA[] IS NOT INITIAL.
*        LOOP AT IT_VBFA INTO WA_VBFA.
*          "Pega Materiais da OV estornada
*          SELECT VBELN POSNR MATNR ARKTX WERKS CHARG KWMENG VRKME NETPR NETWR
*           FROM VBAP
*           INTO TABLE IT_VBAP
*           WHERE VBELN EQ WA_SAIDA-VBELN.
*
*          REFRESH TL_RETURN.
*          CLEAR F_HEADINX.
*          F_HEADINX-UPDATEFLAG = 'D'.
*          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*            EXPORTING
*              SALESDOCUMENT    = WA_SAIDA-VBELN
*              ORDER_HEADER_INX = F_HEADINX
*            TABLES
*              RETURN           = TL_RETURN.
*
*          COMMIT WORK.
*
*          PERFORM F_EXIBE_BAPI.
*
*          READ TABLE TL_RETURN WITH KEY TYPE = 'E'.
*          IF SY-SUBRC IS NOT INITIAL.
*            " Marca Estorno Controle de Transferencias de OV
*            UPDATE  ZSDT0090 SET ESTORNO      = ABAP_TRUE
*                                 USNAM_E      = SY-UNAME
*                                 DATA_ATUAL_E = SY-DATUM
*                                 HORA_ATUAL_E = SY-UZEIT
*              WHERE VBELN = WA_SAIDA-VBELN.
*            COMMIT WORK.
*
*            ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_VBELN = WA_SAIDA-VBELN
*                                                    I_TIPO  = 'EST'
*                                                   ).
*            FREE IT_0090.
*            SELECT *
*             FROM ZSDT0090
*             INTO TABLE IT_0090
*             WHERE VBELN = WA_SAIDA-VBELN.
*
*            " Pega Materiais da OV original
*            SELECT VBELN POSNR MATNR ARKTX WERKS CHARG KWMENG VRKME NETPR NETWR
*              FROM VBAP
*              INTO TABLE IT_VBAP2
*              FOR ALL ENTRIES IN IT_0090
*              WHERE VBELN EQ IT_0090-VBELV.
*
*            REFRESH IT_OV.
*            SORT IT_VBAP2 BY MATNR.
*            LOOP AT IT_VBAP INTO WA_VBAP.
*              READ TABLE IT_VBAP2 INTO WA_VBAP2 WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
*              WA_OV-VBELN = WA_VBAP2-VBELN. " OV original
*              WA_OV-POSNR = WA_VBAP2-POSNR.
*              WA_OV-ZMENG = WA_VBAP-KWMENG. " Quantidade estornada.
*
*              TRY .
*                  WA_OV-NETPR = IT_0090[ MATNR = WA_VBAP-MATNR ]-NETPRV.
*                CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*              ENDTRY.
*
*              APPEND WA_OV TO IT_OV.
*
*            ENDLOOP.
*
*            REFRESH TL_RETURN.
*            CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
*              EXPORTING
*                I_SOMA    = ABAP_TRUE
*                I_ACAO    = P_UCOMM
*              TABLES
*                IT_OV     = IT_OV
*                TE_RETURN = TL_RETURN.
*
*            "
*            PERFORM F_EXIBE_BAPI.
*
*          ENDIF.
*        ENDLOOP.
*
*      ELSE.
*
*        "Pega Materiais da OV estornada
*        SELECT VBELN POSNR MATNR ARKTX WERKS CHARG KWMENG VRKME NETPR NETWR
*         FROM VBAP
*         INTO TABLE IT_VBAP
*         WHERE VBELN EQ WA_SAIDA-VBELN.
*
*        REFRESH: TL_RETURN,I_ORDER_ITEM_IN,I_ORDER_ITEM_INX,I_SCHED,I_SCHEDX.
*
*        CLEAR F_HEADINX.
*        F_HEADINX-UPDATEFLAG = 'D'.
*
*        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*          EXPORTING
*            SALESDOCUMENT    = WA_VBAP-VBELN
*            ORDER_HEADER_INX = F_HEADINX
*          TABLES
*            RETURN           = TL_RETURN
*            ORDER_ITEM_IN    = I_ORDER_ITEM_IN
*            ORDER_ITEM_INX   = I_ORDER_ITEM_INX
*            SCHEDULE_LINES   = I_SCHED
*            SCHEDULE_LINESX  = I_SCHEDX.
*
*        IF SY-SUBRC NE 0.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              WAIT = ABAP_TRUE.
*        ENDIF.
*        "
*        PERFORM F_EXIBE_BAPI.
*
*        FREE IT_0090.
*        SELECT *
*         FROM ZSDT0090
*         INTO TABLE IT_0090
*         WHERE VBELN = WA_SAIDA-VBELN.
*
*        " Pega Materiais da OV original
*        SELECT VBELN POSNR MATNR ARKTX WERKS CHARG KWMENG VRKME NETPR NETWR
*          FROM VBAP
*          INTO TABLE IT_VBAP2
*          FOR ALL ENTRIES IN IT_0090
*          WHERE VBELN EQ IT_0090-VBELV.
*
*        REFRESH IT_OV.
*        SORT IT_VBAP2 BY MATNR.
*        LOOP AT IT_VBAP INTO WA_VBAP.
*          READ TABLE IT_VBAP2 INTO WA_VBAP2 WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
*          WA_OV-VBELN = WA_VBAP2-VBELN. " OV original
*          WA_OV-POSNR = WA_VBAP2-POSNR.
*          WA_OV-ZMENG = WA_VBAP-KWMENG. " Quantidade estornada.
*
*          TRY .
*              WA_OV-NETPR = IT_0090[ MATNR = WA_VBAP-MATNR ]-NETPRV.
*              WA_OV-VRKME = IT_0090[ MATNR = WA_VBAP-MATNR ]-KMEINV.
*            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*          ENDTRY.
*
*          APPEND WA_OV TO IT_OV.
*
*        ENDLOOP.
*
*        REFRESH TL_RETURN.
*        CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
*          EXPORTING
*            I_SOMA    = ABAP_TRUE
*            I_ACAO    = P_UCOMM
*          TABLES
*            IT_OV     = IT_OV
*            TE_RETURN = TL_RETURN.
*
*        READ TABLE TL_RETURN WITH KEY TYPE = 'E'.
*        IF SY-SUBRC NE 0.
*          " Marca Estorno Controle de Transferencias de OV
*          UPDATE  ZSDT0090 SET ESTORNO      = ABAP_TRUE
*                               USNAM_E      = SY-UNAME
*                               DATA_ATUAL_E = SY-DATUM
*                               HORA_ATUAL_E = SY-UZEIT
*            WHERE VBELN = WA_SAIDA-VBELN.
*          COMMIT WORK.
*
*          ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_VBELN = WA_SAIDA-VBELN
*                                                  I_TIPO  = 'EST'
*                                                 ).
*        ENDIF.
*
*        PERFORM F_EXIBE_BAPI.
*
*      ENDIF.
*    ENDIF.
*
  else.
    message 'Tipo de OV não previsto para ser estornado' type 'I'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  REDIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form redist .

  free: it_popup[], it_popup2[], it_popup, it_popup2.


  clear wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.


  loop at it_saida into wa_saida where check eq abap_true.
    move-corresponding wa_saida to it_popup.
    append it_popup.
  endloop.

  sort it_popup by vbeln.
  delete adjacent duplicates from it_popup comparing vbeln.
  if lines( it_popup[] ) ne 1.
    message 'Selecione apenas Linhas da Mesma Ordem!' type 'I'.
    exit.
  endif.

  wa_saida = it_saida[ check = abap_true ].
  if wa_saida-sd_disp eq 0.
    message 'Selecione apenas Linhas com Saldo!' type 'I'.
    exit.
  endif.

  free: it_popup[], it_popup2[], it_popup, it_popup2.

  loop at it_saida into wa_saida where check eq abap_true.
    move-corresponding wa_saida to it_popup.

    clear it_popup-sd_disp.
    loop at it_vbfa into wa_vbfa where vbelv = wa_saida-vbeln
                                 and   posnv = wa_saida-posnr.
      add wa_vbfa-rfmng to it_popup-sd_disp.
    endloop.

    read table it_mara into wa_mara with key matnr = it_popup-matnr binary search.

    if wa_mara-meins eq it_popup-vrkme.
      it_popup-sd_disp = it_popup-kwmeng - it_popup-sd_disp.
    else.
      read table it_marm with key matnr = it_popup-matnr
                                  meinh = it_popup-vrkme.
      if sy-subrc is initial.
        it_popup-sd_disp =  it_popup-kwmeng - ( wa_vbfa-rfmng / it_marm-umrez ).
      endif.
    endif.
    it_popup-color = abap_false.
    append it_popup.
  endloop.

  loop at it_popup.
    move-corresponding it_popup to it_popup2.
    it_popup2-kwmeng = 0.
    append it_popup2.
  endloop.

  call screen 0600 starting at 030 6
                   ending   at 175 21.

endform.
*&---------------------------------------------------------------------*
*&      Form  PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form price .

  clear wa_0090.

  it_sai_aux[] = it_saida.
  delete it_sai_aux where check = abap_false.
  if lines( it_sai_aux ) eq 1.
    read table it_saida into wa_saida with key check = abap_true.

    clear wg_exit.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
    check wg_exit is initial.


    if wa_saida-waerk eq 'BRL'.
      message 'A Trava Cambio é utilizada somente para Moedas em USD!' type 'I'.
      exit.
    endif.

    move wa_saida-vbeln to wa_0090-vbelv.

    select single kurrf valdt
      from vbkd
      into (wa_taxa_old-taxa, wa_taxa_old-venc)
    where vbeln eq wa_0090-vbelv.

    call screen 0500 starting at 050 3
                     ending   at 170 10.

  else.
    message 'Selecione apenas uma Linha!' type 'I'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  ENCERRAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form encerrar .

  data: wl_vbep type vbep.

  move sy-ucomm to ok_code.


  clear wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.


  read table it_saida into wa_saida with key check = abap_true.
  it_sai_aux[] = it_saida.

  "Check OV. de Transferencia
  loop at it_sai_aux where check = abap_true.
    select single *
      from vbep into wl_vbep
     where vbeln  = it_sai_aux-vbeln
    and posnr  = it_sai_aux-posnr.

    if ( sy-subrc = 0 ) and ( wl_vbep-lifsp eq '12' ).
      message s897(sd) with it_sai_aux-vbeln it_sai_aux-posnr ', saldo existente é proveniente de transferencia!' display like 'W'.
      return.
    endif.
  endloop.

  delete it_sai_aux where check = abap_false.

  sort it_sai_aux by doc_simulacao.
  delete adjacent duplicates from it_sai_aux comparing doc_simulacao.
  if lines( it_sai_aux ) eq 1.
    perform encerramento.
  else.
    message 'A Seleção Multipla de linhas só pode ocorrer para o mesmo numero de OV!' type 'I'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GERAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form gerar .

  clear: v_vbeln,wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    v_vbeln = wa_saida-vbeln.
    exit.
  endloop.

  loop at it_saida into wa_saida where check = abap_true.
    if wa_saida-vbeln ne v_vbeln.
      wg_exit = 'X'.
      message 'A Seleção Multipla de linhas só pode ocorrer para o mesmo numero de OV!' type 'I'.
      exit.
    endif.
    if wa_saida-auart ne 'ZREM'.
      wg_exit = 'X'.
      message 'Ordem deve ser do tipo ZREM!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.
  check v_vbeln is not initial.

  refresh tg_ov.
  loop at it_saida into wa_saida where check = abap_true.
    tg_ov-vbeln       = wa_saida-vbeln.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_saida-vbeln
      importing
        output = tg_ov-vbeln2.

    tg_ov-posnr       = wa_saida-posnr.
    tg_ov-matnr       = wa_saida-matnr.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_saida-matnr
      importing
        output = tg_ov-matnr2.

    tg_ov-arktx       = wa_saida-arktx.
    tg_ov-kwmeng      = wa_saida-kwmeng.
    tg_ov-vrkme       = wa_saida-vrkme.
    tg_ov-charg       = wa_saida-charg.
    tg_ov-lgort       = wa_saida-lgort.

    tg_ov-sd_disp     = 0.
    loop at it_vbfa into wa_vbfa where vbelv = wa_saida-vbeln
                                 and   posnv = wa_saida-posnr.
      add wa_vbfa-rfmng to tg_ov-sd_disp.
    endloop.
    tg_ov-sd_disp = tg_ov-kwmeng - tg_ov-sd_disp.

    tg_ov-qt_tran     = 0.
    append tg_ov.
  endloop.

  clear wg_venda.
  wg_venda-ztri = abap_true.

  clear: altura, h1.
  h1 = lines( tg_ov ).
  perform tela using h1 changing altura.

  free: t_text, t_lines.
  clear t_lines.
  call screen 0200 starting at 050 3
                   ending   at 160 altura.

endform.
*&---------------------------------------------------------------------*
*&      Form  TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form transf.

  data: auart_txt(30).

  clear: v_vbeln, wg_exit.

  loop at it_saida into wa_saida where check = abap_true.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.

  loop at it_saida into wa_saida where check = abap_true.
    v_vbeln = wa_saida-vbeln.
    exit.
  endloop.

  loop at it_saida into wa_saida where check = abap_true.
    if wa_saida-vbeln ne v_vbeln.
      wg_exit = abap_true.
      message 'A Seleção Multipla de linhas só pode ocorrer para o mesmo numero de OV!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.
  check v_vbeln is not initial.

  refresh tg_ov.
  loop at it_saida into wa_saida where check eq abap_true.
    tg_ov-vbeln       = wa_saida-vbeln.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_saida-vbeln
      importing
        output = tg_ov-vbeln2.

    tg_ov-posnr       = wa_saida-posnr.
    tg_ov-matnr       = wa_saida-matnr.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_saida-matnr
      importing
        output = tg_ov-matnr2.

    tg_ov-arktx       = wa_saida-arktx.
    tg_ov-kwmeng      = wa_saida-kwmeng.
    tg_ov-vrkme       = wa_saida-vrkme.

    tg_ov-sd_disp     = 0.
    loop at it_vbfa into wa_vbfa where vbelv = wa_saida-vbeln
                                 and   posnv = wa_saida-posnr.
      add wa_vbfa-rfmng to tg_ov-sd_disp.
    endloop.
    tg_ov-sd_disp = tg_ov-kwmeng - tg_ov-sd_disp.

    tg_ov-qt_tran     = 0.
    append tg_ov.

*   Verifica se existe Frete Associoado.
*    V_ERRO = COND #( WHEN ZCL_UTIL_=>GET_ZSDT0037( WA_SAIDA ) IS NOT INITIAL THEN ABAP_TRUE ELSE V_ERRO ).
  endloop.
*   Sai se não esta correto a Associoação do Frete
*  CHECK V_ERRO IS INITIAL.

  if 'ZFTE_ZDEF_ZSEM' cs wa_saida-auart.
    auart_txt = 'Venda Conta e Ordem'.
*    WG_VENDA-INCO1 = 'CPT'.
    "Prosseguir para o passo 3;
  elseif wa_saida-auart = 'ZRFU'.
    auart_txt = 'Venda Conta e Ordem'.
    "Prosseguir para o passo 4;
  elseif 'ZOFE_ZODF_ZOSM' cs wa_saida-auart.
    auart_txt = 'Venda Padrão'.
    "Prosseguir para o passo 5;
  else.
    concatenate 'Tipo de Venda "' wa_saida-auart '" não aceita Transferência!' into lv_msg.
    message lv_msg type 'I'.
    exit.
  endif.
  clear wg_venda.

*  CASE WA_SAIDA-AUART.
*    WHEN 'ZFTE' OR 'ZDEF' OR 'ZSEM'.
*      WG_VENDA-INCO1 = 'CPT'.
*  ENDCASE.

  if wg_venda-r_ordem = 'X'.
    case wa_saida-auart.
      when 'ZFTE' or 'ZDEF' or 'ZSEM'.
        wg_venda-inco1 = 'CPT'.
        wg_venda-c_ordem = 'X'.
    endcase.
  else.
    wg_venda-inco1 = wa_saida-inco1.
    wg_venda-c_ordem = ''.
  endif.

  wg_venda-d_futura = 'Venda Futura'.
  wg_venda-d_ordem  = auart_txt.
  wg_venda-d_triang = 'Venda Triangular'.

  if wa_saida-auart ne 'ZRFU'.
    wg_venda-r_futura = abap_true.
  endif.

  clear: altura, h1.
  h1 = lines( tg_ov ).
  perform tela using h1 changing altura.

  free: t_text, t_lines.
  clear t_lines.
  call screen 0200 starting at 050 3
                   ending   at 160 altura.

endform.
*&---------------------------------------------------------------------*
*&      Form  ALTERAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alterar.

  clear wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
    if  wa_saida-auart eq 'ZTRI'.
      wg_exit = abap_true.
      message 'Alteração não permitida para esse tipo de OV!' type 'I'.
      exit.
    endif.
  endloop.

  check  wg_exit is initial.


  if reduce i( init x = 0 for ls in it_saida where ( check is not initial ) next x = x + 1 ) ne 1.
    message 'Selecione uma linha por vez' type 'I'.
    exit.
  endif.

  wa_saida = it_saida[ check = abap_true ].

  if not line_exists( t_usermd[ from = sy-uname ] ).
    if  wa_saida-spart ne '02'.
      message 'Tipo de material não liberado para alterações de quantidade.' type 'I'.
      exit.
    endif.
  endif.

  free: tg_ov2, it_sai_aux.
  it_sai_aux[] = it_saida.
  delete it_sai_aux where check = abap_false.
  perform busca_saldo using 'A' changing it_sai_aux[].

  loop at it_sai_aux.

    tg_ov2-vbeln       = it_sai_aux-vbeln.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = it_sai_aux-vbeln
      importing
        output = tg_ov2-vbeln2.

    tg_ov2-posnr        = it_sai_aux-posnr.
    tg_ov2-matnr        = it_sai_aux-matnr.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = it_sai_aux-matnr
      importing
        output = tg_ov2-matnr2.


    tg_ov2-arktx   = it_sai_aux-arktx.
    tg_ov2-spart   = it_sai_aux-spart.
    tg_ov2-kwmeng  = it_sai_aux-kwmeng.
    tg_ov2-vrkme   = it_sai_aux-vrkme.
    tg_ov2-sd_disp = it_sai_aux-sd_disp.

    tg_ov2-qt_tran = 0.
    append tg_ov2.

  endloop.

  clear wg_desmem.
  call screen 0300 starting at 050 3
                   ending   at 160 18.

endform.
*&---------------------------------------------------------------------*
*&      Form  INCLUIR_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form incluir_texto.

*  FREE: T_TEXT, T_LINES.
*  CLEAR T_LINES.

  call function 'CATSXT_SIMPLE_TEXT_EDITOR'
    exporting
      im_title = 'Observação'
    changing
      ch_text  = t_text.

  loop at t_text into wa_text.
    move '*'     to t_lines-tdformat.
    move wa_text to t_lines-tdline.
    append t_lines. clear t_lines.
  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  SAVE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_text using simulador seq.

* Clave de texto
  wa_header-tdobject = 'ZOBS0087'.
  wa_header-tdid     = 'Z87'.
  wa_header-tdspras  = 'P'.

* Identificador de texto
  concatenate simulador seq into wa_header-tdname.

* Guardar texto
  call function 'SAVE_TEXT'
    exporting
      header          = wa_header
      savemode_direct = abap_true
    tables
      lines           = t_lines
    exceptions
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      others          = 5.
  if sy-subrc <> 0.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QTD  text
*      <--P_ALTURA  text
*----------------------------------------------------------------------*
form tela  using    p_qtd
           changing p_altura.

  data(h) = 28.

  case p_qtd.
    when 1 or 2.
    when 3.
      h = h + 1.
    when 4.
      h = h + 2.
    when 5.
      h = h + 3.
    when 6.
      h = h + 4.
    when 7.
      h = h + 5.
    when 8.
      h = h + 6.
    when others.
      h = h + 7.
  endcase.

  p_altura = h.

endform.
*&---------------------------------------------------------------------*
*&      Form  ESTORNA_M
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form estorna_m .

  select single vbelv posnv rfmng vbeln
    from vbfa
    into wa_vbfa
  where vbelv = wa_saida-vbeln.

  if sy-subrc eq 0.
    message 'Ordem possui documentos subsequentes, verificar!' type 'I'.
    exit.
  endif.

  "Pega Materiais da OV estornada
  select *
   from vbap
   into corresponding fields of table it_vbap
  where vbeln eq wa_saida-vbeln.

*     "// Marca Estorno Controle de Transferencias de OV
  select *
    from zsdt0090
    into table @data(it_0090)
    where vbeln = @wa_saida-vbeln
  and estorno eq @abap_false.

  check not it_0090 is initial.
  zcl_util_=>indicator( |Deletando O.V { wa_saida-vbeln }| ).

  zcl_util_=>delete_ov(
    exporting
      i_vbeln  = wa_saida-vbeln
    receiving
      t_return = tl_return[] ).

  if line_exists( tl_return[ type = 'E' ] ).
    perform f_exibe_bapi.
    exit.
  endif.

*     " Pega Materiais da OV original
  select vbeln posnr matnr arktx werks charg kwmeng vrkme netpr netwr
    from vbap
    into table it_vbap2
    for all entries in it_0090
  where vbeln eq it_0090-vbelv.

  free it_ov.
  loop at it_0090 into wa_0090.

    try .
        data(preco) = wa_0090-netprv.
      catch cx_sy_itab_line_not_found.
    endtry.

    wa_ov-vbeln = wa_0090-vbelv. "// OV original
    wa_ov-matnr = wa_0090-matnrv.
    wa_ov-posnr = wa_0090-posnv.
    wa_ov-zmeng = wa_0090-zmengv * -1. "// Quantidade estornada.

    wa_ov-netpr = zcl_util_=>check_desc_abs(
      _vbeln = wa_0090-vbelv
      _posnr = wa_0090-posnv
      des    = conv #( preco )
      dir    = abap_false
    ).
    try .
        wa_ov-vrkme = wa_0090-kmeinv.
      catch cx_sy_itab_line_not_found.
    endtry.

    append wa_ov to it_ov.
  endloop.

  zcl_util_=>indicator( |Retornando Quantidade para O.V de Origem!| ).
  free tl_return.
  call function 'ZSDMF001_ATUALI_OV_SIM'
    exporting
      i_soma    = abap_true
      i_acao    = p_ucomm
    tables
      it_ov     = it_ov
      te_return = tl_return.

  if not line_exists( tl_return[ type = 'E' ] ).

    loop at it_ov into wa_ov.

      try .
          wa_0090 = it_0090[ vbelv = wa_ov-vbeln matnrv = wa_ov-matnr ].
        catch cx_sy_itab_line_not_found.
      endtry.

*      ZCL_UTIL_=>PROCESSA_DESC( I_VBELN = WA_OV-VBELN I_MATNR = WA_OV-MATNR I_VLR_REAL = WA_0090-NETWR ).
      zcl_util_=>indicator( |Reprocessa O.V { wa_ov-vbeln }, { wa_ov-matnr }!| ).
      zcl_util_=>processa_desc( i_vbeln = wa_ov-vbeln i_matnr = wa_ov-matnr ).

*   "// criando uma tabela para aplicar a diferença("Desconto/Acrescimo")
      call method zcl_util_=>zsdmf001_atuali_ov_simulador
        exporting
          i_vbeln     = wa_ov-vbeln
          i_posnr     = wa_ov-posnr
          i_matnr     = wa_ov-matnr
          i_diferenca = conv #( zcl_util_=>get_desconto( i_vbeln = wa_ov-vbeln i_posnr = wa_ov-posnr i_add_imposto = abap_true ) ).

    endloop.

    update  zsdt0090
    set estorno      = abap_true
        usnam_e      = sy-uname
        data_atual_e = sy-datum
        hora_atual_e = sy-uzeit
    where vbeln = wa_saida-vbeln.
    commit work.

    loop at it_0090 into wa_0090.
      zcl_util_=>indicator( |Revertendo Hedge seq { wa_0090-sequencia }!| ).
*         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_0090-doc_simulacao
                                              i_vbeln  = wa_0090-vbeln
                                              i_seq    = wa_0090-sequencia
                                              i_tipo   = 'EST'
                                              ).
    endloop.

    zcl_util_=>modify_vlrtot(
      simulador = wa_saida-doc_simulacao
      t_0090    = conv #( it_0090 )
    ).

    select *
      from zsdt0090
      into table @data(t_90)
       where vbelv eq @wa_saida-vbeln
    and estorno eq @abap_false.

    loop at t_90 into wa_0090.
      zcl_util_=>indicator( |Revertendo Hedge seq { wa_0090-sequencia }!| ).
*     "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_0090-doc_simulacao
                                              i_vbeln  = wa_0090-vbeln
                                              i_seq    = wa_0090-sequencia
                                              i_tipo   = 'EST'
                                              ).
    endloop.

    update  zsdt0090
          set estorno      = abap_true
              usnam_e      = sy-uname
              data_atual_e = sy-datum
              hora_atual_e = sy-uzeit
          where vbelv = wa_saida-vbeln.
    commit work.

**     "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
*    ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS(
*                                            I_VBELN  = WA_SAIDA-VBELN
*                                            I_TIPO   = 'EST'
*                                          ).
  endif.

* "// Processa 40
  zcl_util_=>loop_40(
    simulador = wa_0090-doc_simulacao
    i_vbeln   = wa_0090-vbelv
    i_matnr   = wa_0090-matnrv
    qtd       = 10
  ).

  perform f_exibe_bapi.

endform.
*&---------------------------------------------------------------------*
*&      Form  ESTORNA_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form estorna_i .

  select single vbelv posnv rfmng vbeln
       from vbfa
       into wa_vbfa
       where vbelv = wa_saida-vbeln
  and   posnv = wa_saida-posnr.

  if sy-subrc eq 0.
    message 'Ordem Possui documentos Subsequentes, Verificar!' type 'I'.
    exit.
  endif.

  select *
   from zsdt0090
   into table @data(it_0090)
   where vbeln eq @wa_saida-vbeln
     and posnn eq @wa_saida-posnr
  and estorno eq @abap_false.

  check not it_0090 is initial.
  zcl_util_=>indicator( |Deletando O.V { wa_saida-vbeln }, { wa_saida-posnr }| ).
  call method zcl_util_=>deleta_iten
    exporting
      i_vbeln  = wa_saida-vbeln
      i_posnr  = wa_saida-posnr
      i_matnr  = wa_saida-matnr
    receiving
      t_return = tl_return[].

  if line_exists( tl_return[ type = 'E' ] ).
    perform f_exibe_bapi.
    exit.
  endif.

  free it_ov.

  loop at it_0090 into wa_0090.

    wa_ov-vbeln = wa_0090-vbelv. "// OV original
    wa_ov-matnr = wa_0090-matnrv.
    wa_ov-posnr = wa_0090-posnv.
    wa_ov-zmeng = wa_0090-zmengv * -1. "// Quantidade estornada.
    wa_ov-netpr = zcl_util_=>check_desc_abs(
      _vbeln = wa_0090-vbelv
      _posnr = wa_0090-posnv
      des    = conv #( wa_0090-netprv )
      dir    = abap_false
    ).
    wa_ov-vrkme = wa_0090-kmeinv.
    append wa_ov to it_ov.
  endloop.


  zcl_util_=>indicator( |Retornando Quantidade para O.V de Origem!| ).
  refresh tl_return.
  call function 'ZSDMF001_ATUALI_OV_SIM'
    exporting
      i_soma    = abap_true
      i_acao    = p_ucomm
    tables
      it_ov     = it_ov
      te_return = tl_return.

  if not line_exists( tl_return[ type = 'E' ] ).

    call method zcl_manutencao_insumos=>set_desconto_abs
      exporting
        i_vbeln        = wa_0090-vbelv
        i_posnr        = wa_0090-posnv
        i_desconto_abs = 0
        i_zerar        = abap_true
      importing
        r_return       = data(e_return).

    append lines of e_return to tl_return.

    call method zcl_manutencao_insumos=>set_desconto_abs_ov
      exporting
        i_vbeln  = wa_0090-vbelv
        i_posnr  = wa_0090-posnv
      importing
        r_return = e_return.

    append lines of e_return to tl_return.

    zcl_util_=>indicator( |Reprocessa O.V { wa_0090-vbelv }, { wa_0090-matnrv }!| ).
    zcl_util_=>processa_desc( i_vbeln = wa_0090-vbelv i_matnr = wa_0090-matnrv ).

    update  zsdt0090
          set estorno      = abap_true
              usnam_e      = sy-uname
              data_atual_e = sy-datum
              hora_atual_e = sy-uzeit
          where vbeln = wa_saida-vbeln
            and posnn = wa_saida-posnr.
    commit work.

    zcl_util_=>indicator( |Revertendo Hedge!| ).
    loop at it_0090 into wa_0090.
*         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_0090-doc_simulacao
                                              i_vbeln  = wa_0090-vbeln
                                              i_seq    = wa_0090-sequencia
                                              i_tipo   = 'EST'
                                              ).
    endloop.

    zcl_util_=>modify_vlrtot(
      simulador = wa_saida-doc_simulacao
      t_0090    = conv #( it_0090 )
    ).

    select *
      from zsdt0090
      into table @data(t_90)
       where vbelv eq @wa_saida-vbeln
         and posnv eq @wa_saida-posnr
    and estorno eq @abap_false.

    loop at t_90 into wa_0090.
*         "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_0090-doc_simulacao
                                              i_vbeln  = wa_0090-vbeln
                                              i_seq    = wa_0090-sequencia
                                              i_tipo   = 'EST'
                                              ).
    endloop.

    update  zsdt0090
          set estorno      = abap_true
              usnam_e      = sy-uname
              data_atual_e = sy-datum
              hora_atual_e = sy-uzeit
          where vbelv = wa_saida-vbeln
            and posnv = wa_saida-posnr.
    commit work.

**   "// Dispara o Hedge do Estorno Revertendo todos os Lançamentos.
*    ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS(
*                                            I_VBELN  = WA_SAIDA-VBELN
*                                            I_TIPO   = 'EST'
*                                          ).

*   "// Processa 40
    zcl_util_=>loop_40(
      simulador = wa_0090-doc_simulacao
      i_vbeln   = wa_0090-vbelv
      i_matnr   = wa_0090-matnrv
      qtd       = 10
    ).

  endif.

  perform f_exibe_bapi.

endform.
*&---------------------------------------------------------------------*
*&      Form  HEDGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form hedge using p_direcao.
*  CHECK 1 EQ 2.
  data: vl_trava_cambio  type c,
        vl_orig_trv_cam  type c,
        vl_ov_precedente type vbeln,
        vl_qtde_equal    type c.

  case p_direcao.
    when 'T'.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_saida-doc_simulacao
                                              i_dir    = p_direcao
                                              i_vbeln  = wa_saida-vbeln
                                              i_tipo   = 'EST'
                                              ).
    when others.

      select single * from zsdt0040
       into @data(check)
      where doc_simulacao eq @p_0090-doc_simulacao.

      move-corresponding p_0090 to im_0090.

      if check-tpsim ne 'PM'.
        if check-tpsim ne 'BN'.

          clear: vl_trava_cambio, vl_orig_trv_cam, vl_ov_precedente.
          if check-waerk eq 'USD'.
            call function 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
              exporting
                i_doc_simulacao     = im_0090-doc_simulacao
                i_vbeln             = im_0090-vbelv
              changing
                c_trava_cambio      = vl_trava_cambio
                c_trava_cambio_prec = vl_orig_trv_cam
                c_ov_precedente     = vl_ov_precedente.
          endif.

          call function 'ZSDMF001_COMPARE_UNIT_MAT'
            exporting
              i_matnr_01 = im_0090-matnr
              i_menge_01 = im_0090-zmeng
              i_matnr_02 = im_0090-matnrv
              i_menge_02 = im_0090-zmengv
            importing
              e_equal    = vl_qtde_equal.

          if ( check-waerk eq 'BRL' ) or

             ( ( check-waerk eq 'USD' ) and
               ( vl_trava_cambio is not initial ) and
               ( p_direcao ne 'P' ) ).

            case p_direcao.
              when 'A' or 'E' or 'O'.
                zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                        i_0090 = im_0090
                                                        i_tipo = 'VDI'
                                                        ).

              when others.

                if ( 'U_M' cs p_direcao                          ) and
                   ( im_0090-matklv        eq im_0090-matkl      ) and
                   ( im_0090-matklv        ne '658445'           ) and
                   ( vl_qtde_equal         eq abap_false         ).
                  "( ABS( IM_0090-ZMENGV ) NE ABS( IM_0090-ZMENG ) ).

                  zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                          i_0090 = im_0090
                                                          i_tipo = 'VDI'
                                                          i_dir  = p_direcao
                                                          ).
                elseif im_0090-matklv ne im_0090-matkl.
                  zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                          i_0090 = im_0090
                                                          i_tipo = 'VDI'
                                                          ).
                elseif im_0090-netpr ne im_0090-netprv. " REDISTRIBUIÇÃO
                  if p_direcao eq 'R'.
                    zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                            i_0090 = im_0090
                                                            i_tipo = 'VDI'
                                                            ).
                  endif.
                endif.
            endcase.
          elseif check-waerk eq 'USD'.

            if p_direcao eq 'P'.

*              IF vl_orig_trv_cam IS NOT INITIAL.
*                zcl_webservice_tx_curva=>hedge_insumos( i_numero = wa_saida-doc_simulacao
*                                                        i_dir    = 'J'
*                                                        i_vbeln  = vl_ov_precedente
*                                                        i_tipo   = 'EST'
*                                                        i_0090   = im_0090
*                                                       ).
*              ENDIF.

              zcl_webservice_tx_curva=>hedge_insumos( i_0090 = im_0090
                                                      i_acao = h_ucomm
                                                      i_tipo = 'VDI'
                                                      ).
              exit.
            endif.

          endif.
        endif.

*    DISPARO DO HEDGE PARA FRETE
        case p_direcao.
          when 'A' or 'E'.
            zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                    i_0090 = im_0090
                                                    i_tipo = 'FRI'
                                                    ).
          when 'R'.
*          Dispara o Frete quando a Alteração no Preço e se For Defensivos
            if im_0090-netpr ne im_0090-netprv. " REDISTRIBUIÇÃO
              if im_0090-matklv eq '658445'.
                zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                        i_0090 = im_0090
                                                        i_tipo = 'FRI'
                                                        ).
              endif.
            endif.

            if im_0090-werks ne im_0090-werksv. " REDISTRIBUIÇÃO
              if im_0090-matklv ne '658445'.
                zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                        i_0090 = im_0090
                                                        i_tipo = 'FRI'
                                                        ).
              endif.
            endif.
          when 'O'.
*  Desconto ABS
*  Dispara o Frete Somnete para Defensivos
            if im_0090-matklv eq '658445'.
              zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                      i_0090 = im_0090
                                                      i_tipo = 'FRI'
                                                      ).

            endif.
*         Dispara em caso da Alteração do Incoterms
          when 'F'.
            zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                    i_0090 = im_0090
                                                    i_tipo = 'INV'
                                                    i_dir  = p_direcao
                                                    ).

          when others. " POR ENQUANTO TROCA DE MATERIAIS

            if ( 'U_M' cs p_direcao              ) and
               ( im_0090-matklv eq im_0090-matkl ) and
               ( im_0090-matklv ne '658445'      ) and
               ( vl_qtde_equal  eq abap_false    ).
              "( ABS( IM_0090-ZMENGV ) NE ABS( IM_0090-ZMENG ) ).

              zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                      i_0090 = im_0090
                                                      i_tipo = 'FRI'
                                                      i_dir  = p_direcao
                                                      ).

            elseif ( im_0090-matklv ne im_0090-matkl ) or ( im_0090-inco1v ne im_0090-inco1 ).
              zcl_webservice_tx_curva=>hedge_insumos( i_acao = h_ucomm
                                                      i_0090 = im_0090
                                                      i_tipo = 'FRI'
                                                      ).
            endif.

        endcase.
      endif.

  endcase.
endform.
*&---------------------------------------------------------------------*
*&      Form  ADD_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_9179   text
*----------------------------------------------------------------------*
form add_table using p_direcao.

  move-corresponding p_0090 to im_0090.
  append im_0090 to tm_0090.

endform.
*&---------------------------------------------------------------------*
*&      Form  MDF_VENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mdf_venc using p_not_call_screen.

  free: it_venc, var_venc_old.

  " 10.09.2024 - RAMON - 61181 -->
  gv_ucomm = 'MDF_VENC'.
  " 10.09.2024 - RAMON - 61181 --<

  check not it_saida is initial.

  types: lt_saida type standard table of ty_saida with key vbeln posnr.

  data(it_check) =
      value lt_saida(
           for ls_saida in it_saida where ( check = abap_true )
           ( ls_saida ) ).

  if it_check is initial.
    message 'Nenhuma Linha Selecionada!' type 'I'.
    exit.
  endif.

  delete adjacent duplicates from it_check comparing vbeln.
  if lines( it_check ) ne 1.
    message 'A Seleção Multipla de linhas só pode ocorrer para o mesmo número de OV!' type 'I'.
    exit.
  endif.

  data(vbeln) =  it_saida[ check = abap_true ]-vbeln.

* VERIFICA SE A OV POSSUI ADIANTAMENTO
  select count(*) from zsdt0159
    where vbeln   eq @vbeln
  and estorno ne 'X'.
*CASO EXISTA O ADIANTAMENTO, NÃO DEIXA FAZER A ALTERAÇÃO DE DATA.
  if sy-subrc is initial.
    message |O.V. possui Boleto Bancário/ou Pré lançamento do Boleto. Eliminar Antes!| type 'S' display like 'E'.
    exit.
  endif.

  select single *
    from vbak
   into @data(w_vbak)
  where vbeln eq @vbeln.

* ---> S4 Migration - 07/07/2023 - JP
  select * from v_konv
    into table @data(t_konv)
    where knumv eq @w_vbak-knumv
  and kschl eq 'RB00'.



* <--- S4 Migration - 07/07/2023 - JP
  it_venc =
    value lt_saida(
         for ls_saida in it_saida where ( check = abap_true )
         (
         doc_simulacao2 = ls_saida-doc_simulacao
         matnr2 = ls_saida-matnr
         arktx = ls_saida-arktx
         vbeln2 = ls_saida-vbeln
         posnr = ls_saida-posnr
         kwmeng = ls_saida-kwmeng
         vrkme = ls_saida-vrkme
         kbetr = ls_saida-kbetr
         kmein = ls_saida-kmein
*         DESC_ABSOLUTO = LS_SAIDA-DESC_ABSOLUTO * -1
*         DESC_ABSOLUTO = ZCL_UTIL_=>CHECK_DESC_ABS( _VBELN = LS_SAIDA-VBELN  _POSNR = LS_SAIDA-POSNR DES = CONV #( LS_SAIDA-DESC_ABSOLUTO ) ) * - 1
*         DESC_ABSOLUTO = T_KONV[ KPOSN = LS_SAIDA-POSNR ]-KBETR * -1
         netwr = ls_saida-netwr
         ) ).

*  LOOP AT LT_SAIDA ASSIGNING FIELD-SYMBOL(<SAIDA>).

*  ENDLOOP.\

  select single valdt
    from vbkd
    into var_venc_old
  where vbeln eq w_vbak-vbeln.


  perform change_rows.

  if p_not_call_screen is initial.

    call screen 0700 starting at 039 10
                     ending   at 170 20.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Module  PBO_0700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo_0700 output.
  set pf-status 'Z007'.
  set titlebar 'P007'.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  PBO_0710  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo_0710 output.
  set pf-status 'Z007'.
  set titlebar 'P0710'.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  PAI_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_0700 input.
  case sy-ucomm.
    when 'OK'.
      perform desc_abs using ''.
      clear: var_venc_new, var_venc_old.
      leave to screen 0.
    when 'SAIR'.
      clear: var_venc_new, var_venc_old.
      free it_itens_id.
      leave to screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  PAI_0710  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_0710 input.
  case sy-ucomm.
    when 'OK'.

      gr_dt_ent->check_changed_data( ).

      perform check_dt_entrega.
      if v_erro is initial.
        perform save_new_data_entrega.
        free: it_dt_ent_f,
              it_dt_ent_s,
              it_dt_ent_d,
              it_dt_ent.
        clear: var_fert, var_sem, var_defen.
        leave to screen 0.
      endif.

    when 'SAIR'.
      free: it_dt_ent_f,
            it_dt_ent_s,
            it_dt_ent_d,
            it_dt_ent.

      clear: var_fert, var_sem, var_defen.

      leave to screen 0.
    when 'C_FERT'.
      perform fill_grid_0710.
    when 'C_SEM'.
      perform fill_grid_0710.
    when 'C_DEF'.
      perform fill_grid_0710.
  endcase.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_obj output.

  types: ll_fcat type standard table of lvc_s_fcat with default key.

  clear wa_layout.
  wa_layout-no_toolbar = space.
  wa_layout-grid_title = ''.
  wa_layout-no_toolbar = 'X'.

  if cc_venc is initial.
    create object cc_venc
      exporting
        container_name = 'CC_VENC'.


    create object gr_venc
      exporting
        i_parent = cc_venc.

    perform f_alv_fieldcat.

    wa_layout-stylefname = 'STYLE'.

    data(tl_fcat) =
      value ll_fcat(  for ls_fcat in it_fieldcat where ( fieldname eq 'DOC_SIMULACAO2' or
                                                         fieldname eq 'MATNR2'  or
                                                         fieldname eq 'ARKTX'   or
                                                         fieldname eq 'VBELN2'  or
                                                         fieldname eq 'POSNR'   or
                                                         fieldname eq 'KWMENG'  or
                                                         fieldname eq 'VRKME'   or
                                                         fieldname eq 'KBETR'   or
                                                         fieldname eq 'KMEIN'   or
                                                         fieldname eq 'DESC_ABSOLUTO' or
                                                         fieldname eq 'NETWR'
                                                        )
                     ( ls_fcat ) ).

    loop at tl_fcat assigning field-symbol(<fcat_venc>) where fieldname eq 'DESC_ABSOLUTO'.
      <fcat_venc>-edit = abap_true.
    endloop.

    call method gr_venc->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = tl_fcat
        it_outtab            = it_venc[].

    call method gr_venc->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method gr_venc->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_data_changed_finished4 for gr_venc,
                 lcl_event_handler=>on_data_changed4 for gr_venc.
  else.

    call method gr_venc->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_obj_0710 output.

  data: tl_function_0710 type ui_functions,
        wl_function_0710 like tl_function with header line.


  data: str_0710 type ref to data.

  clear wa_layout.
  wa_layout-no_toolbar = space.
  wa_layout-grid_title = ''.

  refresh: tl_function_0710.

  refresh: tl_function_0710.
*  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*  APPEND wl_function_0710 TO tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_paste.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_undo.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_copy.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append wl_function_0710 to tl_function_0710.
  wl_function_0710 = cl_gui_alv_grid=>mc_fc_loc_cut.
  append wl_function_0710 to tl_function_0710.


  assign 'TY_DT_ENT' to field-symbol(<fs_str_0710>).
  create data str_0710 type (<fs_str_0710>).

  data(it_fcat_0710) = corresponding lvc_t_fcat( cl_salv_data_descr=>read_structdescr( cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str_0710 ) ) ) ).

  loop at it_fcat_0710 assigning field-symbol(<fcat_0710>).

    case <fcat_0710>-fieldname.
      when 'DOC_SIMULACAO'.
        <fcat_0710>-scrtext_s = 'Nº Simulação'. <fcat_0710>-outputlen = 14. <fcat_0710>-scrtext_m = 'Nº Simulação'.       <fcat_0710>-scrtext_l = <fcat_0710>-scrtext_s.
      when 'VBELN'.
        <fcat_0710>-scrtext_s = 'OV'.           <fcat_0710>-outputlen = 14. <fcat_0710>-scrtext_m = <fcat_0710>-scrtext_s. <fcat_0710>-scrtext_l = <fcat_0710>-scrtext_s.
      when 'VBELV'.
        <fcat_0710>-no_out    = 'X'.
      when 'SPART'.
        <fcat_0710>-no_out    = 'X'.
      when 'SD_DISP'.
        <fcat_0710>-no_out    = 'X'.
      when 'KBETR'.
        <fcat_0710>-no_out    = 'X'.
      when 'MATNR'.
        <fcat_0710>-no_out    = 'X'.
      when 'CULTURA'.
        <fcat_0710>-no_out    = 'X'.
      when 'DT_ENTREGA_V'.
        <fcat_0710>-scrtext_s = 'Data Atual'.   <fcat_0710>-outputlen = 12. <fcat_0710>-scrtext_m = <fcat_0710>-scrtext_s. <fcat_0710>-scrtext_l = <fcat_0710>-scrtext_s.
      when 'DT_ENTREGA'.
        <fcat_0710>-scrtext_s = 'Nova Data'.    <fcat_0710>-outputlen = 12. <fcat_0710>-scrtext_m = <fcat_0710>-scrtext_s. <fcat_0710>-scrtext_l = <fcat_0710>-scrtext_s.
        <fcat_0710>-edit = abap_true.
    endcase.

  endloop.

  if cc_dt_ent is initial.

    create object cc_dt_ent
      exporting
        container_name = 'CC_DT_ENT'.

    create object gr_dt_ent
      exporting
        i_parent = cc_dt_ent.

    call method gr_dt_ent->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function_0710
      changing
        it_fieldcatalog      = it_fcat_0710
        it_outtab            = it_dt_ent[].

    call method gr_dt_ent->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method gr_dt_ent->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_data_changed_finished6 for gr_dt_ent,
                 lcl_event_handler=>on_data_changed6 for gr_dt_ent.
  else.

    call method gr_dt_ent->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  DESC_ABS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form desc_abs using p_ajuste_desc_enc type c.

  field-symbols: <saida_venc> type ty_saida.

  data: vl_trava_cambio  type c.
  data: v_cambio         type kurrf.

  data: _ok(1),
        sequencia    type posnr,
        obj_0094     type ref to zcl_taxa_curva,
        obj_insere   type ref to zcl_taxa_curva_db,
        obj_tx_curva type ref to zcl_webservice_tx_curva,
        qtd          type dzmeng,
        var_total    type dmbtr,
        rt_vbap      type table of vbap with header line,
        lw_vbap      type vbap,
        cont         type sy-tabix,
        vl_idx_venc  type sy-tabix.


  create object: obj_0094, obj_tx_curva, obj_insere.

  if ( not var_venc_new is initial ).

    wa_saida = it_saida[ check = abap_true ].


    select single *
   from zsdt0159
     into @data(_0159)
   where doc_simulacao eq @wa_saida-doc_simulacao
    and estorno eq @abap_false.

    if _0159 is initial.
*   Atualiza a Data de Vencimento e se tiver vencimento lançado é estornado
      perform atualiza_venc using wa_saida-vbeln var_venc_new changing _ok.
    else.

      message 'Existe progamação de pagamemto ativa para o lançamento selecionado' type  'I'.

    endif.

    if not _ok is initial.
      clear: p_0090, wa_0090.
      perform insert_zsdt0090 using 'V'
                                  wa_saida-doc_simulacao
                                  ' '            "NEW
                                  wa_saida-vbeln "OLD
                                  wa_saida-matnr
                                  changing p_0090.

      select single * from vbak
        into @data(wa_vbak)
      where vbeln eq @wa_saida-vbeln.

      select * from vbap
        into table rt_vbap
      where vbeln eq wa_vbak-vbeln.

      data(mv_vbap) = rt_vbap[].
      free: mv_vbap.

      select * from mara
        into table @data(it_mara)
        for all entries in @it_vbap
      where matnr eq @it_vbap-matnr.

      select single * from vbkd
        into @data(wa_vbkd)
      where vbeln eq @wa_saida-vbeln.

      cont = 0.

      loop at rt_vbap assigning field-symbol(<fvbap>).

        data(mara) = it_mara[ matnr = <fvbap>-matnr ].

        obj_0094->set_matkl( i_matkl = mara-matkl
                             i_brgew = mara-brgew
                             ).
        obj_0094->set_zieme( <fvbap>-zieme ).

        if mara-matkl eq '658445'.
          qtd = 0.
        else.
          qtd = <fvbap>-kwmeng.
        endif.

        obj_0094->set_cadencia_in( qtd ).
        <fvbap>-kwmeng = obj_0094->get_cadencia( ).

      endloop.

*   Agrupa todos os Itens para a Reversão do Vencimento
      free mv_vbap.
      clear lw_vbap.
      loop at rt_vbap.
        lw_vbap-vbeln  = rt_vbap-vbeln.
        lw_vbap-kwmeng = rt_vbap-kwmeng.
        lw_vbap-netwr  = rt_vbap-netwr.
        lw_vbap-mwsbp  = rt_vbap-mwsbp.
        collect lw_vbap into mv_vbap.
      endloop.

      data(rw_vbap) = mv_vbap[ 1 ] .

      if wa_saida-waerk eq 'USD'.

        call function 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
          exporting
            i_doc_simulacao = wa_saida-doc_simulacao
            i_vbeln         = wa_saida-vbeln
          changing
            c_trava_cambio  = vl_trava_cambio
            c_taxa          = v_cambio.

      endif.

*      IF NOT VL_TRAVA_CAMBIO IS INITIAL.
      if ( wa_saida-waerk eq 'BRL' ) or ( vl_trava_cambio is not initial ).
*  0(Zero) Reverte o Vencimento Velho
*  1(Um)   Lança o Novo Vencimento
        do.

          qtd = 0.
          var_total = 0.

          if cont eq 2.
            exit.
          endif.

          move p_0090-sequencia to sequencia.

          obj_0094->set_numero( p_0090-doc_simulacao ).
          obj_0094->set_posnr( sequencia ).
          obj_0094->set_data_lib( sy-datum ).

          qtd       = rw_vbap-kwmeng.
*          VAR_TOTAL = RW_VBAP-NETWR.

          if cont eq 1.
            obj_0094->set_data_venc( wa_vbkd-valdt ).
          else.
            if var_venc_old < sy-datum.
              obj_0094->set_data_venc( wa_vbkd-valdt ).
            else.
              obj_0094->set_data_venc( var_venc_old ).
            endif.
            qtd = qtd * -1.
*            VAR_TOTAL = VAR_TOTAL * -1.
          endif.

          if obj_0094->get_matkl( ) ne '658445'.
            obj_0094->set_zieme( 'KG' ).
          else.
            obj_0094->set_zieme( '' ).
          endif.

          obj_0094->set_cadencia( qtd ).

          obj_0094->set_incoterms( wa_vbkd-inco1 ).
          obj_0094->set_vbeln( rw_vbap-vbeln ).
          obj_0094->set_tipo('VDI').

          " 08.02.2023 - RAMON - 99548 -->
          if cont eq 1.
            if wa_saida-waerk eq 'USD'.
              var_total = ( ( rw_vbap-netwr + rw_vbap-mwsbp ) * obj_0094->get_taxa_cambio( ) ).
            else.
              var_total = rw_vbap-netwr + rw_vbap-mwsbp.
            endif.

            if cont ne 1.
              var_total = var_total * -1.
            endif.

            obj_0094->set_total_proporcional( var_total ).
          endif.
          " 08.02.2023 - RAMON - 99548 --<

          obj_0094->tipo_taxa_in( 'OTH' ).
          obj_0094->set_taxa_curva(
          obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                     i_data_lib = obj_0094->get_data_lib(  )
                                     i_tipo     = obj_0094->get_tipo_taxa( )
                                   ) ).
          obj_0094->set_tx_cambio_in( i_taxa   = ''
                                      i_numero = obj_0094->get_numero( )
                                      i_tipo   = 'VDI' ).

          " 08.02.2023 - RAMON - 99548 -->
          if cont eq 0.

            if wa_saida-waerk eq 'USD'.
              var_total = ( ( rw_vbap-netwr + rw_vbap-mwsbp ) * obj_0094->get_taxa_cambio( ) ).
            else.
              var_total = rw_vbap-netwr + rw_vbap-mwsbp.
            endif.

            if cont ne 1.
              var_total = var_total * -1.
            endif.

            obj_0094->set_total_proporcional( var_total ).


          endif.

          " 19.03.2025 - RAMON - 171294 -->
****          " 08.02.2023 - RAMON - 99548 --<
****          " 17.04.2024 - RAMON - 96174 -->
****          IF gv_ucomm NE 'MDF_VENC' AND sy-sysid <> 'PRD'.
          " comentado dia 19.03.2025, foi inserido para não deixar gerar hedge vazio, porém já existe codigo para isso,
          " e o controle nao deve ser feito pelo ucomm
          obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).
****          ENDIF.
****          " 17.04.2024 - RAMON - 96174 --<
          " 19.03.2025 - RAMON - 171294 --<
          add 1 to cont.

        enddo.
      endif.

    endif.

  endif.

  if ( p_ajuste_desc_enc is not initial ).
    clear: it_itens_id[].

    loop at it_venc assigning <saida_venc>.

      read table it_desc_enc with key vbeln2 = <saida_venc>-vbeln2
                                      posnr  = <saida_venc>-posnr.

      if ( sy-subrc = 0 ) and ( <saida_venc>-desc_absoluto ne 0 ).

        if <saida_venc>-desc_absoluto > 0.
          wa_itens_id-vlr_dif = <saida_venc>-desc_absoluto - it_desc_enc-desc_absoluto.
          <saida_venc>-desc_absoluto = it_desc_enc-desc_absoluto.
        else.
          wa_itens_id-vlr_dif = <saida_venc>-desc_absoluto + it_desc_enc-desc_absoluto.
          <saida_venc>-desc_absoluto = it_desc_enc-desc_absoluto * -1.
        endif.

        move: <saida_venc>-doc_simulacao2 to wa_itens_id-doc,
              <saida_venc>-vbeln2         to wa_itens_id-vbeln,
              <saida_venc>-posnr          to wa_itens_id-posnr,
              <saida_venc>-netwr          to wa_itens_id-vlr_ini.

        append wa_itens_id to it_itens_id.
      endif.

    endloop.
  endif.

  sort it_itens_id by index.
  delete adjacent duplicates from it_itens_id comparing index.

  if it_itens_id is initial.
    perform f_exibe_bapi.
  endif.

  check not it_itens_id is initial.

  select *
    from vbap
    into table @data(tg_vbap)
 for all entries in @it_itens_id
   where posnr eq @it_itens_id-posnr
  and vbeln eq @it_itens_id-vbeln.

  check tg_vbap is not initial.

  types: tt_itens type standard table of zsdt0041 with key vbeln posnr.

  data(tg_itens_41) =
        value tt_itens(
                       for wa_vbap in tg_vbap (
                       doc_simulacao = it_itens_id[ 1 ]-doc
                       vbeln = wa_vbap-vbeln
                       posnr = wa_vbap-posnr
                       matnr = wa_vbap-matnr
                       desc_absoluto =
                       zcl_util_=>check_desc_abs( _vbeln = wa_vbap-vbeln
                                                  _posnr = wa_vbap-posnr
                                                  des = conv #( it_venc[ vbeln2 = wa_vbap-vbeln
                                                                         posnr  = wa_vbap-posnr ]-desc_absoluto -
                                                                it_saida[ vbeln = wa_vbap-vbeln
                                                                          posnr = wa_vbap-posnr ]-desc_absoluto ) )
*                       DESC_ABSOLUTO = IT_VENC[ VBELN2 = WA_VBAP-VBELN
*                                                POSNR  = WA_VBAP-POSNR ]-DESC_ABSOLUTO
                       ) ).

  check not tg_itens_41 is initial.

  call function 'ZSDMF001_ATUALI_OV_SIMULADOR'
    tables
      ti_itens_ov       = tg_itens_41
    exceptions
      ov_nao_encontrada = 1
      others            = 2.

  loop at tg_itens_41 into data(wg_41).
    clear: p_0090, wa_0090.
    perform insert_zsdt0090 using 'O'
                                wg_41-doc_simulacao
                                ' '         "NEW
                                wg_41-vbeln "OLD
                                wg_41-matnr
                                changing p_0090.
    "PERFORM HEDGE USING 'O'.
  endloop.

  perform f_exibe_bapi.
  free it_itens_id[].


endform.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form change_rows.

  loop at it_venc into data(check).

    data(tabix) = sy-tabix.

    select single * from vbfa
      into @data(vbfa)
      where vbelv eq @check-vbeln2
    and posnv  eq @check-posnr.

    clear: check-style, wa_edit, it_edit[].

    if sy-subrc is initial.
      wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
    else.
      wa_edit-style = cl_gui_alv_grid=>mc_style_enabled.
    endif.

    wa_edit-fieldname = 'DESC_ABSOLUTO'.
    wa_edit-style = wa_edit-style.
    insert wa_edit into table it_edit.

    insert lines of it_edit into table check-style.

    modify it_venc from check
        index tabix
          transporting style.

  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_VENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form atualiza_venc using vbeln dt_venc changing _ok.

  clear _ok.

  data:
    wl_orderheaderin  type bapisdh1,
    wl_orderheaderinx type bapisdh1x,
    tl_return         type table of bapiret2 with header line.

  wl_orderheaderinx-updateflag = 'U'.
  wl_orderheaderin-fix_val_dy  = dt_venc.
  wl_orderheaderinx-fix_val_dy = abap_true.
  "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = vbeln
      order_header_in  = wl_orderheaderin
      order_header_inx = wl_orderheaderinx
    tables
      return           = tl_return.

  read table tl_return with key type = 'E'.
  if not sy-subrc is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.

    _ok = abap_true.

    select single *
   from zsdt0090
     into @data(_0090)
   where vbelv eq @vbeln
     and categoria eq 'V'
    and estorno eq @abap_false.

    if sy-subrc is initial.
      update zsdt0090 set estorno      = abap_true
                          usnam_e      = sy-uname
                          data_atual_e = sy-datum
                          hora_atual_e = sy-uzeit
                      where vbelv eq vbeln
                      and categoria eq 'V'
                      and estorno eq abap_false.
      commit work.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_vencimento input.

  data: s_subrc type sy-subrc.

  check not var_venc_new is initial.

  zcl_solicitacao_ov=>dia_util( exporting p_vencimento = var_venc_new
                                importing e_subrc      = s_subrc ).
  if s_subrc is initial.
    clear var_venc_new.
  endif.

  if var_venc_new eq var_venc_old.
    clear var_venc_new.
  endif.

  if var_venc_new < sy-datum.
    clear var_venc_new.
  endif.

endmodule.

form estorno_erros using wg_documento etapa.

  data: is_cancelled type bapivbrkout-cancelled,
        t_success    type standard table of bapivbrksuccess with header line,
        t_return     type standard table of bapireturn1 with header line,
        vl_docnum    type j_1bnflin-docnum,
        v_docnum     type j_1bnfe_active-docnum,
        vcandat      type j_1bnfdoc-candat,
        vdocnum_est  type j_1bdocnum,
        w_erro(1),
        fatura(1).

  clear fatura.

  case etapa.
    when 1.
      perform update using wg_documento.
    when 2.
      fatura = abap_true.
    when 3.
      perform deleta using wg_documento.
    when 4.
      perform deleta using wg_documento.
  endcase.

  check not fatura is initial.


  "*---> 19/07/2023 - Migração S4 - LO
*  "//Cancela fatura
*  CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*    EXPORTING
*      billingdoc_number       = wg_documento
*    IMPORTING
*      billingdoc_is_cancelled = is_cancelled.

  data lwa_bill_detail      type bapivbrkout.
  data lwa_return           type bapireturn1.


  call function 'BAPI_BILLINGDOC_GETDETAIL'
    exporting
      billingdocument       = wg_documento
    importing
      billingdocumentdetail = lwa_bill_detail
      return                = lwa_return.
  is_cancelled = lwa_bill_detail-cancelled.
  "*<--- 19/07/2023 - Migração S4 - LO


  if ( is_cancelled is initial ).
    "Cancela fatura
    call function 'ZBAPI_BILLINGDOC_CANCEL1'
      exporting
        billingdocument = wg_documento
      tables
        return          = t_return         " bapireturn1 Table of Error Messages Entered
        success         = t_success.       " bapivbrksuccess Table of Successfully Processed Documents
  endif.

  if ( t_success[] is not initial ) or ( is_cancelled is not initial ).

    select single docnum
      from j_1bnflin
      into vl_docnum
    where refkey = wg_documento.

    clear vcandat.
    select single  candat
      from j_1bnfdoc
      into  vcandat
    where docnum     = vl_docnum.

    if vcandat is initial. "Documento Fiscal não está estornado ainda....
*
      "Verificar se documento esta autorizado na SEFAZ
      select single docnum
       from j_1bnfe_active
       into v_docnum
       where docnum     = vl_docnum
         and docsta     = '1'
      and cancel     = ''.

      if sy-subrc ne 0. "Caso não esteja, forçar o cancelamento do documento fiscal, serviço que a bapi deveria ter feito e não fez.
        call function 'J_1B_NF_DOCUMENT_CANCEL'
          exporting
            doc_number               = vl_docnum
            ref_type                 = space
            ref_key                  = space
            can_dat                  = sy-datum
          importing
            doc_number               = vdocnum_est
          exceptions
            document_not_found       = 1
            cancel_not_possible      = 2
            nf_cancel_type_not_found = 3
            database_problem         = 4
            docum_lock               = 5
            nfe_cancel_simulation    = 6
            others                   = 7.

        if sy-subrc eq 0.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = abap_true.
        else.
          w_erro = abap_true.
        endif.
      else.
        w_erro = abap_true.
      endif.

      check w_erro is not initial. "Não houve êxito na tentativa do cancelamento do Doc. Fiscal, e prosseguir para gravar o log. de erro.

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      message 'Impossível estorno de fatura. Danfe, não estornada!' type  'I'.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  DELETA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form deleta using wg_documento.

  data: f_headinx like bapisdh1x,
        t_return  type table of bapiret2 with header line.

  clear f_headinx.

  f_headinx-updateflag = 'D'.
  "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = wg_documento
      order_header_inx = f_headinx
    tables
      return           = t_return.

  read table t_return  with key type = 'E'.

  if sy-subrc is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.

    wait up to 1 seconds.
  endif.

  append lines of t_return to tl_return.

endform.
*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_DOCUMENTO  text
*----------------------------------------------------------------------*
form update  using  wg_documento.


  data: tl_schedule_lines  type table of bapischdl  with header line,
        tl_schedule_linesx type table of bapischdlx with header line,
        tl_bapisditm       type table of bapisditm with header line,
        tl_bapisditmx      type table of bapisditmx with header line,
        tl_return          type table of bapiret2 with header line,
        wl_orderheaderinx  type bapisdh1x,
        wl_logic_switch    type bapisdls.

  select *
    from vbap
    into table @data(vbap)
  where vbeln eq @wg_documento.

  select *
    from vbep
    into table @data(vbep)
    for  all entries in @vbap
  where vbeln eq @vbap-vbeln.

  wl_orderheaderinx-updateflag = 'U'.

  loop at vbap into data(wvbap).

    move: 'U'            to tl_bapisditmx-updateflag,
          wvbap-posnr    to tl_bapisditmx-itm_number,
          'X'            to tl_bapisditmx-target_qty,
          wvbap-posnr    to tl_bapisditm-itm_number,
          0              to tl_bapisditm-target_qty.

    append tl_bapisditmx.
    append tl_bapisditm.
    clear: tl_bapisditm, tl_bapisditmx.

    loop at vbep into data(w_vbep).

      move: 'U'           to tl_schedule_linesx-updateflag,

            wvbap-posnr   to tl_schedule_linesx-itm_number,
            w_vbep-etenr  to tl_schedule_linesx-sched_line,
            'X'           to tl_schedule_linesx-req_qty,

            wvbap-posnr   to tl_schedule_lines-itm_number,
            w_vbep-etenr  to tl_schedule_lines-sched_line,
            0             to tl_schedule_lines-req_qty.

      append tl_schedule_linesx.
      append tl_schedule_lines.
      clear: tl_schedule_lines, tl_schedule_linesx.

    endloop.
    "*---> 19/07/2023 - Migração S4 - LO --> Material não foi utilizado
    call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      exporting
        salesdocument    = wvbap-vbeln
        order_header_inx = wl_orderheaderinx
      tables
        order_item_in    = tl_bapisditm
        order_item_inx   = tl_bapisditmx
        schedule_lines   = tl_schedule_lines
        schedule_linesx  = tl_schedule_linesx
        return           = tl_return.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.

    wait up to 2 seconds.

    check p_ucomm eq 'ESTORNAR'.
    check wa_saida-auart ne 'ZTRI'.

    clear wa_zsdt0090.

    select single *
      from zsdt0090
      into wa_zsdt0090
      where vbeln = wvbap-vbeln
    and posnn = wvbap-posnr.

    if sy-subrc is initial.

      free it_ov.
      wa_ov-vbeln = wa_zsdt0090-vbelv.
      wa_ov-posnr = wa_zsdt0090-posnv.
      wa_ov-zmeng = wa_zsdt0090-zmengv * -1.
*      WA_OV-NETPR = WA_ZSDT0090-NETPRV.

      wa_ov-netpr = zcl_util_=>check_desc_abs(
        _vbeln = wa_zsdt0090-vbelv
        _posnr = wa_zsdt0090-posnv
        des    = conv #( wa_zsdt0090-netprv )
        dir    = abap_false
      ).
      try .
        catch cx_sy_itab_line_not_found.
      endtry.

      append wa_ov to it_ov.
      clear wa_ov.

    endif.

    refresh tl_return.

    call function 'ZSDMF001_ATUALI_OV_SIM'
      exporting
        i_soma    = 'A'
        i_acao    = p_ucomm
      tables
        it_ov     = it_ov
        te_return = tl_return.
    "
    perform f_exibe_bapi.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VALDT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_valdt input.

  clear check_dt.

  check not wa_0090-valdt is initial.

  zcl_solicitacao_ov=>dia_util( exporting p_vencimento = wa_0090-valdt
                                importing e_subrc      = check_dt ).
  if check_dt is initial.
    clear wa_0090-valdt.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  CANCELA_ITEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cancela_iten .

* US-169490-ZSDT0087 WBARBOSA 27/08/2025 INICIO
  data: order_item_in type wiso_t_sditm.
  data: order_item_inx type wiso_t_sditmx.

  data(order_header_inx) = value bapisdh1x( updateflag = 'U' ).

  append initial line to order_item_in assigning field-symbol(<fs_order_item_in>).
  <fs_order_item_in>-itm_number = wa_saida-posnr.

  append initial line to order_item_inx assigning field-symbol(<fs_order_item_inx>).
  <fs_order_item_inx>-itm_number = wa_saida-posnr.
  <fs_order_item_inx>-updateflag = 'D'.
* US-169490-ZSDT0087 WBARBOSA 27/08/2025 FIM

  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = wa_saida-vbeln
      order_header_inx = order_header_inx
    tables
      order_item_in    = order_item_in
      order_item_inx   = order_item_inx
      return           = tl_return.

  if not line_exists( tl_return[ type = 'E' ] ).
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  ALT_GERAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alt_gerais.

  clear wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.

  if reduce i( init x = 0 for ls in it_saida where ( check is not initial ) next x = x + 1 ) ne 1.
    message 'Selecione somente uma Linha' type 'I'.
    exit.
  endif.

  clear: wa_alt_gerais, inco1_, zpesagem_, route_, cod_parc_.

  select single name1
    from t001w
    into wa_alt_gerais-desc_werks
    where werks eq wa_alt_gerais-werks.

  it_alt_gerais = value #( for ls in it_saida
                            where ( check = abap_true )
                            (
                               vbeln      = ls-vbeln
                               auart      = ls-auart
                               zpesagem   = ls-zpesagem
                               route      = ls-route
                               desc_route = zcl_util_=>get_desc_route( ls-route )
                               inco1      = ls-inco1
                               parvw      = ls-parvw
                               cod_parc   = ls-cod_parc
                               desc_parc  = zcl_util_=>get_desc_parc( ls-cod_parc )
                               werks      = ls-werks
                               desc_werks = zcl_util_=>get_desc_werks( ls-werks )
                             )
                         ).

  wa_alt_gerais-vbeln = it_alt_gerais[ 1 ]-vbeln.

  wa_saida = it_saida[ check = abap_true ].

  data(quantidade) = reduce kwmeng( init k type kwmeng for ls in it_saida where ( vbeln eq wa_alt_gerais-vbeln ) next k = k + ls-kwmeng ).
  data(saldo)      = reduce rfmng(  init r type rfmng for ls in it_saida where ( vbeln eq wa_alt_gerais-vbeln ) next r = r + ls-sd_disp ).

  oculta_inco1 = cond #( when quantidade ne saldo then abap_true else abap_false ).

  call screen 0800 starting at 050 3
                   ending   at 170 15.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0800 output.

  data values   type vrm_values.
  data: str type ref to data.

  set pf-status 'PF0800'.
  set titlebar 'TI0800'.

  clear wa_layout.
  wa_layout-no_toolbar = space.
  wa_layout-grid_title = ''.
  wa_layout-no_toolbar = 'X'.

  assign 'TY_ALT_GERAIS' to field-symbol(<fs_str>).
  create data str type (<fs_str>).

  data(it_fcat) = corresponding lvc_t_fcat( cl_salv_data_descr=>read_structdescr( cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  loop at it_fcat assigning field-symbol(<fcat>).

    case <fcat>-fieldname.
      when 'VBELN'.
        <fcat>-scrtext_s = 'OV'.              <fcat>-outputlen = 11.
      when 'AUART'.
        <fcat>-scrtext_s = 'Tipo OV'.         <fcat>-outputlen = 5.
      when 'ZPESAGEM'.
        <fcat>-scrtext_s = 'Tipo Pesagem'.    <fcat>-outputlen = 3.
      when 'ROUTE'.
        <fcat>-scrtext_s = 'Itinerário'.      <fcat>-outputlen = 7.
      when 'DESC_ROUTE'.
        <fcat>-scrtext_s = 'Desc Itinerátio'. <fcat>-outputlen = 15.
      when 'INCO1'.
        <fcat>-scrtext_s = 'Incoterms'.       <fcat>-outputlen = 4.
      when 'PARVW'.
        <fcat>-scrtext_s = 'Parceiro'.        <fcat>-outputlen = 3.
      when 'COD_PARC'.
        <fcat>-scrtext_s = 'Cod. Parceiro'.   <fcat>-outputlen = 10.
      when 'DESC_PARC'.
        <fcat>-scrtext_s = 'Desc Parceiro'.   <fcat>-outputlen = 15.
      when 'WERKS'.
        <fcat>-scrtext_s = 'Centro'.          <fcat>-outputlen = 04.
      when 'DESC_WERKS'.
        <fcat>-scrtext_s = 'Desc Centro'.     <fcat>-outputlen = 15.
    endcase.

    <fcat>-scrtext_l = <fcat>-scrtext_s.
    <fcat>-scrtext_m = <fcat>-scrtext_s.

  endloop.

  if cc_alt_gerais is initial.

    create object cc_alt_gerais
      exporting
        container_name = 'CC_ALT_GERAIS'.

    create object gr_alt_gerais
      exporting
        i_parent = cc_alt_gerais.

    call method gr_alt_gerais->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_fieldcatalog      = it_fcat
        it_outtab            = it_alt_gerais[].

  else.
    gr_alt_gerais->refresh_table_display( exporting is_stable = wa_stable ).
  endif.

  select  *  from zsdt0258
    into table @data(it_frete)
  where auart eq @wa_saida-auart.


  values = value #( for fre in it_frete ( text = fre-inco1 key = fre-inco1 ) ( text = fre-inco2 key = fre-inco2 )
                                        ( text = fre-inco3 key = fre-inco3 ) ( text = fre-inco4 key = fre-inco4 )  ).

  delete values where key eq wa_saida-inco1.

  call function 'VRM_SET_VALUES'
    exporting
      id              = 'WA_ALT_GERAIS-INCO1'
      values          = values
    exceptions
      id_illegal_name = 1
      others          = 2.

  if sy-subrc is not initial.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  free: values.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0800 input.
  case sy-ucomm.
    when 'OK'.
      zcl_util_=>verifica_erros( input = wa_alt_gerais-vbeln ).

      case abap_true.
        when inco1_ or zpesagem_ or route_ or cod_parc_ or dep_lote_.
          perform executa_alteracao using wa_alt_gerais-vbeln.
          if v_erro is initial.
            leave to screen 0.
          endif.
        when centro_.

          call method zcl_manutencao_insumos=>troca_centro_fornecedor
            exporting
              i_vbeln  = wa_saida-vbeln
              i_posnr  = wa_saida-posnr
              i_werks  = wa_alt_gerais-werks
            importing
              r_return = data(r_return).

          call method zcl_manutencao_insumos=>exibe_mensagens
            exporting
              i_return = r_return.

          leave to screen 0.

      endcase.

    when 'BTN_ITI'.

      call method zcl_manutencao_insumos=>call_create_itinerario
        exporting
          i_pc = conv #( wa_alt_gerais-werks )
          i_lr = wa_saida-kunnr.

    when 'EXIT'.
      leave to screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form executa_alteracao using x_vbeln.

  data: _orderheaderin  type bapisdh1,
        _orderheaderinx type bapisdh1x,
        _logic_switch   type bapisdls,
        _bape_vbak      type bape_vbak,
        _bape_vbakx     type bape_vbakx,
        _bapisditm      type table of bapisditm,
        _bapisditmx     type table of bapisditmx,
        _partners       type table of bapiparnr,
        _partnersc      type table of bapiparnrc,
        _bapiparex      type table of bapiparex.

  data: vbeln type vbeln.

  if v_erro is not initial.
    exit.
  endif.

  vbeln = |{ x_vbeln alpha = in }|.

  select *
    from vbap
    into table @data(lt_vbap)
  where vbeln eq @vbeln.

  select single *
    from vbpa
    into @data(wa_vbpa)
    where vbeln eq @vbeln
  and parvw eq 'PC'.

  _logic_switch-cond_handl   = abap_true.

* Atualiza o Incoterms1
  _orderheaderin  = value #(
                             incoterms1 = cond #( when inco1_ eq abap_true then wa_alt_gerais-inco1 else abap_false )
                             incoterms2 = cond #( when inco1_ eq abap_true then wa_alt_gerais-inco1 else abap_false )
                           ).
  _orderheaderinx = value #(
                             updateflag = 'U'
                             incoterms1 = cond #( when inco1_ eq abap_true then abap_true else abap_false )
                             incoterms2 = cond #( when inco1_ eq abap_true then abap_true else abap_false )
                           ).

*  Atualiza a ZPESAGEM
  if zpesagem_ eq abap_true.
    _bape_vbak = value #(
                           vbeln = vbeln
                           zpesagem = wa_alt_gerais-zpesagem
                        ).
    _bape_vbakx = value #(
                         vbeln = vbeln
                         zpesagem = abap_true
                       ).
    _bapiparex = value #(
                          ( structure = 'BAPE_VBAK'  valuepart1 = _bape_vbak  )
                          ( structure = 'BAPE_VBAKX' valuepart1 = _bape_vbakx )
                        ).
  endif.

* Atualiza a Rota
  if route_ eq abap_true or inco1_ eq abap_true.
    _bapisditm = value #( for ls in lt_vbap
                            (
                              itm_number = ls-posnr
                              route      = cond #( when route_ eq abap_true then wa_alt_gerais-route )
                              incoterms1 = cond #( when inco1_ eq abap_true then wa_alt_gerais-inco1 )
                              incoterms2 = cond #( when inco1_ eq abap_true then wa_alt_gerais-inco1 )
                            )
                        ).
    _bapisditmx = value #( for ls1 in lt_vbap
                            (
                              updateflag = 'U'
                              itm_number = ls1-posnr
                              route      = cond #( when route_ eq abap_true then abap_true else abap_false )
                              incoterms1 = cond #( when inco1_ eq abap_true then abap_true else abap_false )
                              incoterms2 = cond #( when inco1_ eq abap_true then abap_true else abap_false )
                            )
                         ).
  elseif  dep_lote_ is not initial.
* Alteração do Deposito e Lote
    _bapisditm = value #( for ls3 in it_dep_lote2
                            (
                              itm_number = ls3-posnr
                              "*---> 19/07/2023 - Migração S4 - LO
                              material        = cond #( when dep_lote_ eq abap_true then ls3-matnr )
*                              material_long   = COND #( WHEN dep_lote_ EQ abap_true THEN |{ ls3-matnr ALPHA = IN  }| )
                              "*---> 19/07/2023 - Migração S4 - LO
                              plant           = cond #( when dep_lote_ eq abap_true then ls3-werks )
                              store_loc       = cond #( when dep_lote_ eq abap_true then ls3-lgort )
                              batch           = cond #( when dep_lote_ eq abap_true then ls3-charg )

                            )
                        ).
    _bapisditmx = value #( for ls4 in it_dep_lote2
                            (
                              updateflag = 'U'
                              itm_number = ls4-posnr
                              "*---> 19/07/2023 - Migração S4 - LO
                              material   = cond #( when dep_lote_ eq abap_true then abap_true else abap_false )
*                              material_long   = COND #( WHEN dep_lote_ EQ abap_true THEN abap_true ELSE abap_false )
                              "*---> 19/07/2023 - Migração S4 - LO
                              plant          = cond #( when dep_lote_ eq abap_true then abap_true else abap_false )
                              store_loc      = cond #( when dep_lote_ eq abap_true then abap_true else abap_false )
                              batch          = cond #( when dep_lote_ eq abap_true then abap_true else abap_false )
                            )
                         ).

  endif.

* Atualiza o Parceiro PC
  if cod_parc_ eq abap_true.

    _partnersc = value #( (
                            document = wa_vbpa-vbeln
                            itm_number = wa_vbpa-posnr
                            updateflag = 'U'
                            partn_role = wa_vbpa-parvw
                            p_numb_old = wa_vbpa-lifnr
                            p_numb_new = wa_alt_gerais-cod_parc
                        ) ).

    " 20.12.2022 - RAMON --- 74130 -->> Comentando para subir os ajustes do <RIM-SKM-IR120585-23.12.22

    select single kunnr from vbak
      into @data(lv_kunnr)
    where vbeln = @wa_alt_gerais-vbeln.

    perform f_get_route
      using wa_alt_gerais-cod_parc
            lv_kunnr
   changing wa_alt_gerais-route.

    if v_erro = 'X'.
      message 'Não existe itinerário para zona de transporte do cliente' type 'I'.
      exit.
    endif.

    _bapisditm = value #( for ls in lt_vbap
                        (
                          itm_number = ls-posnr
                          route      =  wa_alt_gerais-route
                          "incoterms1 = COND #( WHEN inco1_ EQ abap_true THEN wa_alt_gerais-inco1 )
                          "incoterms2 = COND #( WHEN inco1_ EQ abap_true THEN wa_alt_gerais-inco1 )
                        )
                    ).
    _bapisditmx = value #( for ls1 in lt_vbap
                            (
                              updateflag = 'U'
                              itm_number = ls1-posnr
                              route      = 'X'
                              "incoterms1 = COND #( WHEN inco1_ EQ abap_true THEN abap_true ELSE abap_false )
                              "incoterms2 = COND #( WHEN inco1_ EQ abap_true THEN abap_true ELSE abap_false )
                            )
                         ).

    " 20.12.2022 - RAMON --- 74130 --<<

  endif.

* "// Atualiza Centro Fornecedor
  if centro_ is not initial.

    _bapisditm = value #( for ls in lt_vbap
                        (
                          itm_number = ls-posnr
                          plant      = wa_alt_gerais-werks
                        )
                    ).

    _bapisditmx = value #( for ls in lt_vbap
                            (
                              updateflag = 'U'
                              itm_number = ls-posnr
                              plant      = abap_true
                            )
                         ).

  endif.

* Atualiza OV
  "*---> 19/07/2023 - Migração S4 - LO
  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = vbeln
      order_header_in  = _orderheaderin
      order_header_inx = _orderheaderinx
      logic_switch     = _logic_switch
    tables
      order_item_in    = _bapisditm
      order_item_inx   = _bapisditmx
      extensionin      = _bapiparex
      partnerchanges   = _partnersc
      return           = tl_return.

  if line_exists( tl_return[ type = 'E' ] ).
  else.

    if inco1_ eq abap_true.

      loop at it_saida into wa_saida where vbeln eq vbeln.
        zcl_util_=>set_insert_90(
          doc_simulador = wa_saida-doc_simulacao
          vbeln_old     = vbeln
          categoria     = 'F'
          gerais        = wa_alt_gerais-inco1
        ).
        perform hedge using 'F'.
      endloop.
    endif.

    if zpesagem_ eq abap_true.
      zcl_util_=>set_insert_90(
        doc_simulador = wa_saida-doc_simulacao
        vbeln_old     = vbeln
        categoria     = 'G'
        gerais        = wa_alt_gerais-zpesagem
      ).
    endif.

    if route_ eq abap_true.
      zcl_util_=>set_insert_90(
        doc_simulador = wa_saida-doc_simulacao
        vbeln_old     = vbeln
        categoria     = 'I'
        gerais        = wa_alt_gerais-route
      ).
    endif.

    if cod_parc_ eq abap_true.
      zcl_util_=>set_insert_90(
        doc_simulador = wa_saida-doc_simulacao
        vbeln_old     = vbeln
        categoria     = 'P'
        gerais        = wa_alt_gerais-cod_parc
      ).
    endif.

    if centro_ eq abap_true.
      zcl_util_=>set_insert_90(
        doc_simulador = wa_saida-doc_simulacao
        vbeln_old     = vbeln
        categoria     = 'H'
        gerais        = wa_alt_gerais-werks
      ).
    endif.

    if dep_lote_ eq abap_true.
      loop at it_dep_lote2 into dep_lote2.
        dep_lote1 = it_dep_lote1[ vbeln = dep_lote2-vbeln posnr = dep_lote2-posnr ].
        zcl_util_=>set_insert_90(
          doc_simulador = dep_lote2-doc_simulacao
          vbeln_old     = dep_lote2-vbeln
          categoria     = 'L'
          gerais        = ''
        ).
      endloop.
    endif.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.

  endif.

  perform f_exibe_bapi.

endform.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_0800 output.

  zcl_util_=>verifica_erros( input = wa_saida-vbeln input1 = wa_saida-posnr ).

  if wa_alt_gerais-werks is not initial and centro_ eq abap_true.
    call method zcl_manutencao_insumos=>verificar_itinerario
      exporting
        i_pc  = conv #( wa_alt_gerais-werks )
        i_lr  = wa_saida-kunnr
      importing
        is_ok = data(itinerario_ok).
  endif.

  loop at screen.

    if inco1_ eq abap_true and
       screen-name eq 'WA_ALT_GERAIS-INCO1'.
      screen-input = 1.
    endif.

    if zpesagem_ eq abap_true and
       screen-name eq 'WA_ALT_GERAIS-ZPESAGEM'.
      screen-input = 1.
    endif.

    if route_ eq abap_true and
       screen-name eq 'WA_ALT_GERAIS-ROUTE'.
      screen-input = 1.
    endif.

    if cod_parc_ eq abap_true and
       screen-name eq 'WA_ALT_GERAIS-COD_PARC'.
      screen-input = 1.
    endif.

    if centro_ eq abap_true and
       screen-name eq 'WA_ALT_GERAIS-WERKS'.
      screen-input = 1.
    endif.

    if screen-name eq 'BT_ITINER'.
      if itinerario_ok is not initial.
        screen-invisible = 1.
      else.
        screen-invisible = 0.
      endif.
    endif.


    modify screen.

  endloop.

  " 30.05.2023 - RAMON - 97513 -->
  if gv_stvarv_ativa is not initial.
    perform f_check_autorizacao_gerais.
  endif.
  " 30.05.2023 - RAMON - 97513 --<

endmodule.

*&---------------------------------------------------------------------*
*&      Module  GET_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_dados input.

  wa_alt_gerais-cod_parc = |{ wa_alt_gerais-cod_parc alpha = in }|.

  zcl_util_=>verifica_erros( input = wa_saida-vbeln input1 = wa_saida-posnr ).

  wa_alt_gerais-desc_route = zcl_util_=>get_desc_route( wa_alt_gerais-route ).
  wa_alt_gerais-desc_parc  = zcl_util_=>get_desc_parc( wa_alt_gerais-cod_parc ).

  wa_alt_gerais-desc_werks = zcl_util_=>get_desc_werks( wa_alt_gerais-werks ).

endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0900 output.

  set pf-status 'PF0900'.
  set titlebar 'TI0900'.

  if cc_dep_lote1 is initial.

    create object cc_dep_lote1
      exporting
        container_name = 'CC_DEP_LOTE1'.

    create object gr_dep_lote1
      exporting
        i_parent = cc_dep_lote1.

    perform montar_layout2 using 'C1'.

    gr_dep_lote1->set_table_for_first_display(
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_dep_lote1 ).

    set handler: lcl_event_handler=>on_data_changed_finished3 for gr_dep_lote1,
                 lcl_event_handler=>on_data_changed3 for gr_dep_lote1.

  else.
    gr_dep_lote1->refresh_table_display( exporting is_stable = wa_stable ).
  endif.

  if cc_dep_lote2 is initial.

    create object cc_dep_lote2
      exporting
        container_name = 'CC_DEP_LOTE2'.

    create object gr_dep_lote2
      exporting
        i_parent = cc_dep_lote2.

    perform montar_layout2 using 'C2'.

    gr_dep_lote2->set_table_for_first_display(
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_dep_lote2 ).

    gr_dep_lote2->register_edit_event( exporting i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    gr_dep_lote2->register_edit_event( exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    set handler: lcl_event_handler=>on_data_changed_finished5 for gr_dep_lote2,
                 lcl_event_handler=>on_data_changed5 for gr_dep_lote2.

  else.
    gr_dep_lote2->refresh_table_display( exporting is_stable = wa_stable ).
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0900 input.

  case sy-ucomm.
    when 'SAIR'.
      leave to screen 0.
    when 'CONFI'.
      dep_lote_ = abap_true.

      zcl_util_=>verifica_erros( v_vbeln ).
      check v_erro is initial.

      perform executa_alteracao using v_vbeln.

      clear: v_vbeln, dep_lote_.
      leave to screen 0.

  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  DEP_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dep_lote .

  free: it_dep_lote2, it_dep_lote1.

  clear wg_exit.
  loop at it_saida into wa_saida where check = abap_true.
    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.
      wg_exit = abap_true.
      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.
      exit.
    endif.
  endloop.
  check wg_exit is initial.

  it_dep_lote1 = value #( for ls in it_saida where ( check is not initial ) ( corresponding #( ls ) ) ).

  sort it_dep_lote1 by vbeln.
  delete adjacent duplicates from it_dep_lote1   comparing vbeln.

  if reduce i( init x = 0 for ls in it_dep_lote1 next x = x + 1 ) ne 1.
    message 'Selecione somente Linha da mesma O.V.' type 'I'.
    exit.
  endif.

  it_dep_lote1 = value #( for ls in it_saida where ( check is not initial ) ( corresponding #( ls ) ) ).

  loop at it_dep_lote1 assigning field-symbol(<dep_lote>).

    v_vbeln = <dep_lote>-vbeln.
    clear: <dep_lote>-color.
  endloop.

  it_dep_lote2 = it_dep_lote1.

  call screen 0900 starting at 030 6
                   ending   at 175 21.

endform.
*&---------------------------------------------------------------------*
*&      Module  CHECK_INCO1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_inco1 input.

  case wg_venda-inco1.
    when 'FOB' or 'CIF'.
    when others.
      clear wg_venda-inco1.
      message 'Incoterms Inválido para o Tipo de Ordem!' type 'E' display like 'S'.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0601  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0601 output.
  set pf-status 'PF0601'.
  set titlebar 'TI0601'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0601  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0601 input.
  case sy-ucomm.
    when 'OK'.

      if vl_vlr_venda ne it_popup2-kbetr.
        message |Analisar preço informado para a Redistribuição!| type 'S' display like 'E'.
      else.
        leave to screen 0.
      endif.

    when 'CANCEL'.
      leave to screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_DESC_ABS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form verifica_desc_abs using dir.

  clear: desconto, ztotal.

  case dir.
    when 'T'.
*     "// get todos as movimentações do vbeln para estornar
      select *
         from zsdt0090
         into table @data(t_0090)
         where vbelv eq @wa_saida-vbeln
      and estorno eq @abap_false.
    when 'I'.
*     "// get todos as movimentações do vbeln para estornar
      select *
         from zsdt0090
         into table t_0090
         where vbelv eq wa_saida-vbeln
           and posnv eq wa_saida-posnr
           and categoria in ('A', 'L', 'O', 'F')
      and estorno eq abap_false.
  endcase.

  check t_0090 is not initial.
  zcl_util_=>modify_vlrtot(
    simulador = wa_saida-doc_simulacao
    t_0090    = conv #( t_0090 )
  ).

  loop at t_0090 into data(w_0090).
* "// Modifica a 90 como Valor desagregado
    update zsdt0090
      set flag1 = abap_true
        where vbelv eq w_0090-vbelv
          and posnv eq w_0090-posnv
          and sequencia eq w_0090-sequencia
          and categoria eq 'O'
          and estorno eq abap_false
          and flag eq abap_true.
*  "// Estorna todos as movimentações da OV
    update zsdt0090
      set estorno      = abap_true
          usnam_e      = sy-uname
          data_atual_e = sy-datum
          hora_atual_e = sy-uzeit
        where vbelv eq w_0090-vbelv
          and posnv eq w_0090-posnv
          and sequencia eq w_0090-sequencia
          and estorno eq abap_false.
*   "// Dispara o Hedge para a OV
    zcl_webservice_tx_curva=>hedge_insumos(
      i_numero = w_0090-doc_simulacao
      i_vbeln  = w_0090-vbelv
      i_seq    = w_0090-sequencia
      i_tipo   = 'EST'
    ).
  endloop.

*  CASE DIR.
*    WHEN 'T'.
*      SELECT SINGLE *
*         FROM ZSDT0090
*         INTO WX_0090
*         WHERE VBELV EQ WA_SAIDA-VBELN
*           AND CATEGORIA EQ 'O'
*           AND ESTORNO NE ABAP_TRUE.
*
*      IF SY-SUBRC EQ 0.
*
*        UPDATE  ZSDT0090 SET
*              ESTORNO      = ABAP_TRUE
*              USNAM_E      = SY-UNAME
*              DATA_ATUAL_E = SY-DATUM
*              HORA_ATUAL_E = SY-UZEIT
*            WHERE VBELV EQ WA_SAIDA-VBELN
*              AND CATEGORIA EQ 'O'.
*      ENDIF.
*
*      CLEAR: WX_0090.
*
  " Verifica se há algum registro de ALTERAÇÃO DE VENCIMENTO para OV e marca como estornado
*      SELECT SINGLE *
*         FROM ZSDT0090
*         INTO WX_0090
*         WHERE VBELV EQ WA_SAIDA-VBELN
*           AND CATEGORIA EQ 'V'
*           AND ESTORNO NE ABAP_TRUE.
*
*      IF SY-SUBRC EQ 0.
*
*        UPDATE  ZSDT0090 SET
*              ESTORNO      = ABAP_TRUE
*              USNAM_E      = SY-UNAME
*              DATA_ATUAL_E = SY-DATUM
*              HORA_ATUAL_E = SY-UZEIT
*            WHERE VBELV EQ WA_SAIDA-VBELN
*              AND CATEGORIA EQ 'V'.
*      ENDIF.
*
  "Verifica se há algum desconto feito nessa OV e se foi agregado ao valor total do Simulador, e desfaz a ação.
*      SELECT * FROM
*        ZSDT0090 INTO TABLE TG_0090
*         WHERE VBELV EQ WA_SAIDA-VBELN
*           AND CATEGORIA EQ 'O'
*           AND ESTORNO EQ ABAP_TRUE
*           AND FLAG EQ ABAP_TRUE.
*
*      IF SY-SUBRC EQ 0.
*
*        SELECT SINGLE VLRTOT
*          FROM ZSDT0040
*          INTO ZTOTAL
*          WHERE DOC_SIMULACAO = WA_SAIDA-DOC_SIMULACAO.
*
*        LOOP AT TG_0090 INTO WA_0090.
*          ADD WA_0090-DESC_ABSOLUTO TO DESCONTO.
*        ENDLOOP.
*
*        "Inverte o sinal, pois irá devolver um valor que já foi agregado.
*        DESCONTO = DESCONTO * -1.
*        ADD DESCONTO TO ZTOTAL.
*
*        UPDATE  ZSDT0090 SET
*             FLAG1 = ABAP_TRUE
*        WHERE VBELV EQ WA_SAIDA-VBELN
*          AND CATEGORIA EQ 'O'
*          AND ESTORNO EQ ABAP_TRUE
*          AND FLAG EQ ABAP_TRUE.
*
*        UPDATE ZSDT0040 SET VLRTOT = ZTOTAL
*        WHERE DOC_SIMULACAO EQ WA_SAIDA-DOC_SIMULACAO.
*
*      ENDIF.
*
*    WHEN 'I'.
*      " Verifica se há algum registro de desconto para O ITEM DA OV e marca como estornado
*      SELECT SINGLE *
*       FROM ZSDT0090
*       INTO WX_0090
*       WHERE VBELV EQ WA_SAIDA-VBELN
*         AND POSNV EQ WA_SAIDA-POSNR
*         AND CATEGORIA EQ 'O'
*         AND ESTORNO NE ABAP_TRUE.
*
*      IF SY-SUBRC EQ 0.
*        UPDATE  ZSDT0090 SET
*              ESTORNO      = ABAP_TRUE
*              USNAM_E      = SY-UNAME
*              DATA_ATUAL_E = SY-DATUM
*              HORA_ATUAL_E = SY-UZEIT
*            WHERE VBELV EQ WA_SAIDA-VBELN
*              AND POSNV EQ WA_SAIDA-POSNR
*              AND CATEGORIA EQ 'O'.
*      ENDIF.
*
*      "Verifica se há algum desconto feito no ITEM selecionado da OV e se foi agregado ao valor total do Simulador, e desfaz a ação.
*      SELECT * FROM
*        ZSDT0090 INTO TABLE TG_0090
*         WHERE VBELV EQ WA_SAIDA-VBELN
*           AND POSNV EQ WA_SAIDA-POSNR
*           AND CATEGORIA EQ 'O'
*           AND ESTORNO EQ ABAP_TRUE
*           AND FLAG EQ ABAP_TRUE.
*
*      IF SY-SUBRC EQ 0.
*
*        SELECT SINGLE VLRTOT
*          FROM ZSDT0040
*          INTO ZTOTAL
*          WHERE DOC_SIMULACAO = WA_SAIDA-DOC_SIMULACAO.
*
*        LOOP AT TG_0090 INTO WA_0090.
*          ADD WA_0090-DESC_ABSOLUTO TO DESCONTO.
*        ENDLOOP.
*
*        "Inverte o sinal, pois irá devolver um valor que já foi agregado.
*        DESCONTO = DESCONTO * -1.
*        ADD DESCONTO TO ZTOTAL.
*
*        UPDATE  ZSDT0090 SET
*             FLAG1 = ABAP_TRUE
*        WHERE VBELV EQ WA_SAIDA-VBELN
*          AND POSNV EQ WA_SAIDA-POSNR
*          AND CATEGORIA EQ 'O'
*          AND ESTORNO EQ ABAP_TRUE
*          AND FLAG EQ ABAP_TRUE.
*
*        UPDATE ZSDT0040 SET VLRTOT = ZTOTAL
*        WHERE DOC_SIMULACAO EQ WA_SAIDA-DOC_SIMULACAO.
*      ENDIF.
*  ENDCASE.

endform.
*&---------------------------------------------------------------------*
*&      Form  TROCA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form troca_material.

  data: _id type i.

  free: it_troca_old, it_troca_new, it_troca.

  data(check) = it_saida[].
  delete check where check ne abap_true.
  delete adjacent duplicates from check comparing vbeln.

  if lines( check ) ne 1.
    message 'Selecione linha da mesma Ordem!' type 'I'.
    exit.
  endif.

  loop at it_saida into wa_saida where check eq abap_true.

    free it_saldo.
    append corresponding #( wa_saida ) to it_saldo.
    perform busca_saldo using 'M' changing it_saldo.
    append lines of it_saldo to it_troca.

  endloop.

  loop at it_troca into data(wa_troca).
    add 1 to _id.

    append value #(
                    id      = _id
                    vbeln   = wa_troca-vbeln
                    posnr   = wa_troca-posnr
                    charg   = wa_troca-charg
                    matnr   = wa_troca-matnr
                    arktx   = wa_troca-arktx
                    matkl   = wa_troca-matkl
                    kwmeng  = wa_troca-kwmeng
                    vrkme   = wa_troca-vrkme
                    werks   = wa_troca-werks
                    lgort   = wa_troca-lgort
                    meins   = wa_troca-kmein
                    netpr   = wa_troca-kbetr
                    netwr   = wa_troca-netwr
                    sd_disp = wa_troca-sd_disp
                    qt_tran = 0
                    vkorg   = wa_troca-vkorg
                    waerk   = wa_troca-waerk
                    cultura = wa_troca-cultura
                    inco1   = wa_troca-inco1
                    auart   = wa_troca-auart
                    spart   = wa_troca-spart
                    kunnr   = wa_troca-kunnr
                    tpsim   = wa_troca-tpsim

                  ) to it_troca_old.

    append value #(
                    id = _id
                    style = value #(
                                       ( fieldname = 'LGORT' style = on )
                                       ( fieldname = 'LOTE'  style = on )
                                       ( fieldname = 'QTD'   style = on )
                                       ( fieldname = 'MATNR' style = on )
                                       ( fieldname = 'INCO1' style = off )
                                       ( fieldname = 'WERKS_FORNEC' style = off )
                                     )
                   itinerario = '@AW@'
                  ) to it_troca_new.

  endloop.

  call screen 0410 starting at 040 5
                   ending   at 180 22.


endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0410  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0410 output.

  data: lt_f4 type lvc_t_f4.

  set pf-status 'PF_0410'.
  set titlebar 'TI_0410'.

  clear: wa_layout.

  wa_layout =  value #(
                        zebra      = abap_true
                        no_rowmark = abap_true
                        no_toolbar = abap_true
                        grid_title = abap_false
  ).

*  WA_STABLE-ROW        = ABAP_TRUE.

  if obj_ctroca_old is initial.

    create object obj_ctroca_old
      exporting
        container_name = 'CC_OLD'.

    create object obj_gtroca_old
      exporting
        i_parent = obj_ctroca_old.

    perform montar_layout_mat.

    tl_function
    = value #(
               ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
               ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
               ( cl_gui_alv_grid=>mc_fc_loc_move_row )
               ( cl_gui_alv_grid=>mc_fc_loc_paste )
               ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
               ( cl_gui_alv_grid=>mc_fc_loc_undo )
               ( cl_gui_alv_grid=>mc_fc_loc_append_row )
               ( cl_gui_alv_grid=>mc_fc_loc_copy )
               ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
               ( cl_gui_alv_grid=>mc_fc_loc_cut )
             ).

    call method obj_gtroca_old->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_troca_old[].

    call method obj_gtroca_old->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method obj_gtroca_old->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_dtchmat for obj_gtroca_old,
                 lcl_event_handler=>on_dtchfmat for obj_gtroca_old.

  else.

    call method obj_gtroca_old->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.

  if obj_ctroca_new is initial.

    wa_layout-stylefname = 'STYLE'.

    create object obj_ctroca_new
      exporting
        container_name = 'CC_NEW'.

    create object obj_gtroca_new
      exporting
        i_parent = obj_ctroca_new.

    perform montar_layout_mat_new.

    tl_function
    = value #(
               ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
               ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
               ( cl_gui_alv_grid=>mc_fc_loc_move_row )
               ( cl_gui_alv_grid=>mc_fc_loc_paste )
               ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
               ( cl_gui_alv_grid=>mc_fc_loc_undo )
               ( cl_gui_alv_grid=>mc_fc_loc_append_row )
               ( cl_gui_alv_grid=>mc_fc_loc_copy )
               ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
               ( cl_gui_alv_grid=>mc_fc_loc_cut )
             ).

    append value #(
                    fieldname = 'MATNR'
                    register = abap_true
                    getbefore = abap_true
                   ) to lt_f4.

    call method obj_gtroca_new->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_troca_new[].

    call method obj_gtroca_new->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method obj_gtroca_new->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method obj_gtroca_new->register_f4_for_fields
      exporting
        it_f4 = lt_f4.

    call method obj_gtroca_new->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    set handler: lcl_event_handler=>on_onf4     for obj_gtroca_new,
                 lcl_event_handler=>on_dtchnew  for obj_gtroca_new,
                 lcl_event_handler=>on_dtchfnew for obj_gtroca_new,
                 lcl_event_handler=>on_btclick  for obj_gtroca_new.

  else.

    call method obj_gtroca_new->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0410  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0410 input.

  data ls_manutencao type zde_manutencao_ov.
  data: lt_return type  bapiret2_t.

  free lt_return.

  case sy-ucomm.
    when 'OK'.
*        ZCL_UTIL_=>CONFIRM_ORDEM( ).
*        LEAVE TO SCREEN 0.

      loop at it_troca into data(ls_troca) where check eq abap_true.

        read table it_troca_old into data(ls_troca_old) with key vbeln = ls_troca-vbeln posnr = ls_troca-posnr.
        read table it_troca_new into data(ls_troca_new) with key id = ls_troca_old-id.

        ls_manutencao = value #(
        saldo_ov = ls_troca-sd_disp
        troca = value #(
                          doc_simulacao = ls_troca-doc_simulacao
                          cultura       = ls_troca-cultura
                          safra         = ls_troca-safra
                          netwr         = ls_troca-netwr
            ordem_old = value #(
                                  vbeln        = ls_troca_old-vbeln
                                  posnr        = ls_troca_old-posnr
                                  matnr        = ls_troca_old-matnr
                                  qtd_removida = ls_troca_old-qt_tran
                                  auart        = ls_troca_old-auart
                                  werks        = ls_troca_old-werks
                                  matkl        = ls_troca_old-matkl
                                  inco1        = ls_troca_old-inco1
                                  kunnr        = ls_troca_old-kunnr
                                  tpsim        = ls_troca_old-tpsim
                                  kwmeng       = ls_troca_old-kwmeng
                               )
            ordem_new = value #(
                                  vbeln        = ''
                                  posnr        = ''
                                  matnr        = ls_troca_new-matnr
                                  matkl        = ls_troca_new-matkl
                                  inco1        = ls_troca_new-inco1
                                  werks        = ls_troca_new-werks_fornec
                                  lote         = ls_troca_new-lote
                                  lgort        = ls_troca_new-lgort
                                  qtd_recebida = ls_troca_new-qtd
                                  meins        = ls_troca_new-meins
                                  vlr_venda    = ls_troca_new-vlr_venda
                                  spart        = ls_troca_new-spart
                                  mtart        = ls_troca_new-mtart
                               )
            ) ).

        call method zcl_manutencao_insumos=>set_dados_troca
          exporting
            i_manutencao = ls_manutencao.

        call method zcl_manutencao_insumos=>check_dados_troca
          importing
            r_return = data(e_return).

        append lines of e_return to lt_return.

        clear: ls_troca_old, ls_troca_new, ls_manutencao.

      endloop.

      call method zcl_manutencao_insumos=>exibe_mensagens
        exporting
          i_return = lt_return.

      check lt_return is initial.

      loop at it_troca into ls_troca where check eq abap_true.

        read table it_troca_old into ls_troca_old with key vbeln = ls_troca-vbeln posnr = ls_troca-posnr.
        read table it_troca_new into ls_troca_new with key id = ls_troca_old-id.

        ls_manutencao-troca =  value #(
        doc_simulacao = ls_troca-doc_simulacao
        cultura       = ls_troca-cultura
        safra         = ls_troca-safra
        netwr         = ls_troca-netwr
        ordem_old = value #(
                              vbeln        = ls_troca_old-vbeln
                              posnr        = ls_troca_old-posnr
                              matnr        = ls_troca_old-matnr
                              qtd_removida = ls_troca_old-qt_tran
                              auart        = ls_troca_old-auart
                              werks        = ls_troca_old-werks
                              matkl        = ls_troca_old-matkl
                              inco1        = ls_troca_old-inco1
                              kunnr        = ls_troca_old-kunnr
                              tpsim        = ls_troca_old-tpsim
                              kwmeng       = ls_troca_old-kwmeng
                           )
        ordem_new = value #(
                              vbeln        = ''
                              posnr        = ''
                              matnr        = ls_troca_new-matnr
                              matkl        = ls_troca_new-matkl
                              inco1        = ls_troca_new-inco1
                              werks        = ls_troca_new-werks_fornec
                              lote         = ls_troca_new-lote
                              lgort        = ls_troca_new-lgort
                              qtd_recebida = ls_troca_new-qtd
                              meins        = ls_troca_new-meins
                              vlr_venda    = ls_troca_new-vlr_venda
                              spart        = ls_troca_new-spart
                              mtart        = ls_troca_new-mtart
                           )
        ).

        call method zcl_manutencao_insumos=>set_dados_troca
          exporting
            i_manutencao = ls_manutencao.

        call method zcl_manutencao_insumos=>troca_materiais_massa
          exporting
            i_check = abap_false.

        clear: ls_manutencao, ls_troca_old, ls_troca_new.

      endloop.

      leave to screen 0.

*      ELSE.

*        ZCL_UTIL_=>CONFIRM_ORDEM( ).

*      ENDIF.

    when 'CANCEL'.
      leave to screen 0.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv_fieldcat1 .

endform.
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_2542   text
*      -->P_2543   text
*      -->P_2544   text
*      -->P_2545   text
*      -->P_2546   text
*      -->P_2547   text
*      -->P_2548   text
*      -->P_2549   text
*      -->P_2550   text
*      -->P_2551   text
*      -->P_2552   text
*      -->P_2553   text
*----------------------------------------------------------------------*
form alv  using     value(p_col_pos)
                    value(p_fieldname)
                    value(p_scrtext_s)
                    value(p_do_sum)
                    value(p_edit)
                    value(p_no_zero)
                    value(p_fix_column)
                    value(p_checkbox)
                    value(p_hotspot)
                    value(p_icon)
                    value(p_outputlen).

  append value #(
                     col_pos    = p_col_pos
                     fieldname  = p_fieldname
                     scrtext_s  = p_scrtext_s
                     do_sum     = p_do_sum
                     edit       = p_edit
                     no_zero    = p_no_zero
                     fix_column = p_fix_column
                     checkbox   = p_checkbox
                     hotspot    = p_hotspot
                     icon       = p_icon
                     outputlen  = p_outputlen
                     tabname    = 'IT_SAIDA'
                     colddictxt = 'M'
                     selddictxt = 'M'
                     tipddictxt = 'M'
                     col_opt    = abap_true
                     datatype   = cond #( when p_fieldname eq 'DESC_ABSOLUTO' then 'CURR' else '' )
                ) to it_fieldcat.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_CALL_ZSDT0044
*&---------------------------------------------------------------------*
form f_call_zsdt0044.

  data opt type ctu_params.
  free gt_bdc.

  perform f_preencher_dynpro using:
  'X' 'ZSDR016'                      '0100',
  ' ' 'WG_HEADER-DOC_SIMULACAO'      wa_saida-doc_simulacao2,
  ' ' 'BDC_OKCODE'                   'ATUAL'.
  opt-dismode = 'E'.
  opt-defsize = ' '.

  call transaction 'ZSDT0044' using gt_bdc options from opt.

endform.                    " F_CALL_ZSDT0044

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1141   text
*      -->P_1142   text
*      -->P_1143   text
*----------------------------------------------------------------------*
form f_preencher_dynpro   using l_start type c l_name type c l_value.

  move l_start to gw_bdc-dynbegin.
  if l_start = 'X'.
    move:
  l_name  to gw_bdc-program,
  l_value to gw_bdc-dynpro.
  else.
    move:
      l_name  to gw_bdc-fnam,
      l_value to gw_bdc-fval.
  endif.
  append gw_bdc to gt_bdc.
  clear: gw_bdc.

endform.                    " F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  ALT_DATA_ENTREGA
*&---------------------------------------------------------------------*
form alt_data_entrega .

  check not it_saida is initial.

  if reduce i( init x = 0 for ls in it_saida where ( check is not initial ) next x = x + 1 ) ne 1.
    message 'Selecione somente uma Linha' type 'I'.
    exit.
  endif.

  v_doc_simulacao =  it_saida[ check = abap_true ]-doc_simulacao.

  call screen 0710 starting at 039 10
                   ending   at 170 20.

endform.
*&---------------------------------------------------------------------*
*&      Form  FILL_GRID_0710
*&---------------------------------------------------------------------*
form fill_grid_0710.

  data: v_tabix type sy-tabix.

  free: wa_dt_ent .

*Fertilizantes = FT ( it_saida-SPART = 02 )
  if var_fert = 'X' and it_dt_ent_f is initial.
    it_dt_ent_f =  value #( for ls in it_saida
                            where ( spart = '02' and doc_simulacao eq v_doc_simulacao )
                            (
                               doc_simulacao = ls-doc_simulacao
                               vbeln         = ls-vbeln
                               vbelv         = ls-vbeln
                               spart         = ls-spart
                               matnr         = ls-matnr
                               cultura       = ls-cultura
                               dt_entrega_v  = ''
                               dt_entrega    = ''
                             )
                         ).
    loop at it_dt_ent_f into wa_dt_ent_f where spart = '02'.
      v_tabix = sy-tabix.

      select single *
        from zsdt0090
         into w_zsdt0090
        where doc_simulacao eq wa_dt_ent_f-doc_simulacao
            and estorno <> 'X'
            and spartv = '02'
            and categoria = 'B'
      and vbelv = wa_dt_ent_f-vbeln.

      if w_zsdt0090-dt_entrega is initial.

        select single *
           from zsdt0040
          into  w_zsdt0040
        where doc_simulacao eq wa_dt_ent_f-doc_simulacao.

        wa_dt_ent_f-dt_entrega_v  = w_zsdt0040-dt_entrega_fet.

      else.
        wa_dt_ent_f-dt_entrega_v =  w_zsdt0090-dt_entrega.
      endif.

      modify it_dt_ent_f from wa_dt_ent_f index v_tabix transporting dt_entrega_v.

      clear: w_zsdt0090, w_zsdt0040.
    endloop.
    append lines of  it_dt_ent_f[] to it_dt_ent[].
  else.
    if var_fert is initial and it_dt_ent_f is not initial.
      loop at it_dt_ent_f into wa_dt_ent_f.
        read table it_dt_ent into wa_dt_ent with key doc_simulacao = wa_dt_ent_f-doc_simulacao
                                                     vbeln         = wa_dt_ent_f-vbeln
                                                     spart         = '02'.
        check sy-subrc = 0.
        v_tabix = sy-tabix.

        wa_dt_ent-spart = '00'.

        modify it_dt_ent from wa_dt_ent index v_tabix transporting spart.
        clear: wa_dt_ent.
      endloop.
      clear: it_dt_ent_f.
    endif.

  endif.

*Semente Soja = SS     ( it_saida-SPART = 04   /  it_saida-CULTURA = SJ )
*Semente Milho = SM    ( it_saida-SPART = 04   /  it_saida-CULTURA = ML )
*Semente Algodão = SA  ( it_saida-SPART = 04   /  it_saida-CULTURA = AL )

  if var_sem = 'X' and it_dt_ent_s is initial.
    it_dt_ent_s =  value #( for ls in it_saida
                    where ( spart = '04' and doc_simulacao eq v_doc_simulacao )
                    (
                       doc_simulacao = ls-doc_simulacao
                       vbeln         = ls-vbeln
                       vbelv         = ls-vbeln
                       spart         = ls-spart
                       matnr         = ls-matnr
                       cultura       = ls-cultura
                       dt_entrega_v  = ''
                       dt_entrega    = ''
                     )
                 ).

    loop at it_dt_ent_s into wa_dt_ent_s where spart = '04'.
      v_tabix = sy-tabix.

      select single *
        from zsdt0090
         into w_zsdt0090
        where doc_simulacao eq wa_dt_ent_s-doc_simulacao
            and estorno <> 'X'
            and categoria = 'B'
            and spartv = '04'
      and vbelv = wa_dt_ent_s-vbeln.

      if w_zsdt0090-dt_entrega is initial.

        select single *
           from zsdt0040
          into  w_zsdt0040
        where doc_simulacao eq wa_dt_ent_s-doc_simulacao.

        wa_dt_ent_s-dt_entrega_v  = w_zsdt0040-dt_entrega_sem.

      else.
        wa_dt_ent_s-dt_entrega_v =  w_zsdt0090-dt_entrega.
      endif.

      modify it_dt_ent_s from wa_dt_ent_s index v_tabix transporting dt_entrega_v.

      clear: w_zsdt0090, w_zsdt0040.
    endloop.
    append lines of  it_dt_ent_s[] to it_dt_ent[].
  else.
    if var_sem is initial and it_dt_ent_s is not initial.
      loop at it_dt_ent_s into wa_dt_ent_s.
        read table it_dt_ent into wa_dt_ent with key doc_simulacao = wa_dt_ent_s-doc_simulacao
                                                     vbeln         = wa_dt_ent_s-vbeln
                                                     spart         = '04'.
        check sy-subrc = 0.

        v_tabix = sy-tabix.

        wa_dt_ent-spart = '00'.

        modify it_dt_ent from wa_dt_ent index v_tabix transporting spart.
        clear: wa_dt_ent.
      endloop.
      clear: it_dt_ent_s.
    endif.
  endif.

* Defensivos  =  DF  ( it_saida-SPART = 03 )
  if var_defen = 'X'  and it_dt_ent_d is initial.
    it_dt_ent_d =  value #( for ls in it_saida
                        where ( spart = '03' and doc_simulacao eq v_doc_simulacao )
                        (
                           doc_simulacao = ls-doc_simulacao
                           vbeln         = ls-vbeln
                           vbelv         = ls-vbeln
                           spart         = ls-spart
                           matnr         = ls-matnr
                           cultura       = ls-cultura
                           dt_entrega_v  = ''
                           dt_entrega    = ''
                         )
                     ).


    loop at it_dt_ent_d into wa_dt_ent_d where spart = '03'.
      v_tabix = sy-tabix.

      select single *
        from zsdt0090
         into w_zsdt0090
        where doc_simulacao eq wa_dt_ent_d-doc_simulacao
            and estorno <> 'X'
            and spartv = '03'
            and categoria = 'B'
      and vbelv = wa_dt_ent_d-vbeln.

      if w_zsdt0090-dt_entrega is initial.
        select single *
           from zsdt0040
          into  w_zsdt0040
        where doc_simulacao eq wa_dt_ent_d-doc_simulacao.

        wa_dt_ent_d-dt_entrega_v  = w_zsdt0040-dt_entrega_def.

      else.
        wa_dt_ent_d-dt_entrega_v =  w_zsdt0090-dt_entrega.
      endif.

      modify it_dt_ent_d from wa_dt_ent_d index v_tabix transporting dt_entrega_v.

      clear: w_zsdt0090, w_zsdt0040.
    endloop.
    append lines of  it_dt_ent_d[] to it_dt_ent[].
  else.
    if var_defen is initial and it_dt_ent_d is not initial.
      loop at it_dt_ent_d into wa_dt_ent_d.
        read table it_dt_ent into wa_dt_ent with key doc_simulacao = wa_dt_ent_d-doc_simulacao
                                                     vbeln         = wa_dt_ent_d-vbeln
                                                     spart         = '03'.
        check sy-subrc = 0.

        v_tabix = sy-tabix.

        wa_dt_ent-spart = '00'.

        modify it_dt_ent from wa_dt_ent index v_tabix transporting spart.
        clear: wa_dt_ent.

      endloop.
      clear: it_dt_ent_d.
    endif.
  endif.

  delete it_dt_ent where spart = '00'.
  delete adjacent duplicates from it_dt_ent comparing vbeln.

  call method gr_dt_ent->refresh_table_display
    exporting
      is_stable = wa_stable.

endform.
*&---------------------------------------------------------------------*
*&      Form  SAVE_NEW_DATA_ENTREGA
*&---------------------------------------------------------------------*
form save_new_data_entrega .

  data: seq type zsdt0090-sequencia.

  data: w_vbap       type vbap,
        w_vbak       type vbak,
        w_zsdt0090   type  zsdt0090,
        w_zsdt0090_e type zsdt0090.

  loop at it_dt_ent into wa_dt_ent.

    select single *
       from vbak
      into w_vbak
    where vbeln eq wa_dt_ent-vbeln.

    select single *
       from vbap
      into w_vbap
        where vbeln eq w_vbak-vbeln
    and matnr eq wa_dt_ent-matnr.

    select count(*)
      from zsdt0090
      into seq
    where doc_simulacao eq wa_dt_ent-doc_simulacao.

    add 1 to seq.

    w_zsdt0090-mandt           = sy-mandt.
    w_zsdt0090-doc_simulacao   = wa_dt_ent-doc_simulacao.
    w_zsdt0090-sequencia       = seq.
    w_zsdt0090-auartv           = w_vbak-auart.
    w_zsdt0090-vbelv           = w_vbap-vbeln.
    w_zsdt0090-spartv           = w_vbap-spart.
    w_zsdt0090-matklv           = w_vbap-matkl.
    w_zsdt0090-categoria       = 'B'. "(Data de entrega)
    w_zsdt0090-usnam           = sy-uname.
    w_zsdt0090-data_atual       = sy-datum.
    w_zsdt0090-hora_atual       = sy-uzeit.
    w_zsdt0090-dt_entrega       = wa_dt_ent-dt_entrega.
    w_zsdt0090-dt_entrega_v     = wa_dt_ent-dt_entrega_v.

    select single *
     from zsdt0090
      into w_zsdt0090_e
        where doc_simulacao eq wa_dt_ent-doc_simulacao
          and vbelv         eq wa_dt_ent-vbelv
          and categoria     eq 'B'
    and  estorno      <> 'X' .

    if w_zsdt0090_e is not initial.

      update zsdt0090
        set estorno      = abap_true
            usnam_e      = sy-uname
            data_atual_e = sy-datum
            hora_atual_e = sy-uzeit
            origem_est   = sy-repid
          where doc_simulacao eq  w_zsdt0090_e-doc_simulacao
            and vbelv         eq  w_zsdt0090_e-vbelv
            and estorno       <> 'X'
            and categoria     eq 'B'.
    endif.

    modify zsdt0090 from w_zsdt0090.
    commit work.
    wait up to 2 seconds.

    clear: seq,  w_vbak, w_vbap, w_zsdt0090, w_zsdt0090_e.
  endloop.

  perform revet_hedge_fri.

endform.
*&---------------------------------------------------------------------*
*&      Form  REVET_HEDGE_FRI
*&---------------------------------------------------------------------*
form revet_hedge_fri.

  data: lva_bezei        type zsdt0094-bezei,
        lva_cadencia_qte type vbfa-rfmng,
        wa_saida_r       type ty_saida,
        in_0090          type zsdt0090,
        it_in_dt_ent     type table of ty_dt_ent_aux,
        wa_in_dt_ent     type ty_dt_ent_aux,
        v_tabix          type sy-tabix.


  data: obj_0094             type ref to zcl_taxa_curva.
  data: obj_zcl_web_tx_curva type ref to zcl_webservice_tx_curva.
  data: obj_taxa             type ref to zcl_taxa_curva_db.
  create object: obj_0094.
  create object: obj_zcl_web_tx_curva.
  create object: obj_taxa.


  loop at it_dt_ent into wa_dt_ent.


    check wa_dt_ent-dt_entrega is not initial.

    loop at it_saida into wa_saida_r where doc_simulacao eq wa_dt_ent-doc_simulacao
                                      and  vbeln         eq wa_dt_ent-vbeln.


      wa_in_dt_ent-doc_simulacao   = wa_saida_r-doc_simulacao.
      wa_in_dt_ent-vbeln           = wa_saida_r-vbeln.
      wa_in_dt_ent-kmein           = wa_saida_r-kmein.
      wa_in_dt_ent-werks           = wa_saida_r-werks.
      wa_in_dt_ent-vrkme           = wa_saida_r-vrkme.
      wa_in_dt_ent-posnr           = wa_saida_r-posnr.
      wa_in_dt_ent-spart           = wa_saida_r-spart.
      wa_in_dt_ent-matnr           = wa_saida_r-matnr.
      wa_in_dt_ent-matklv          = wa_saida_r-matkl.
      wa_in_dt_ent-cultura         = wa_saida_r-cultura.
      wa_in_dt_ent-sd_disp         = wa_saida_r-sd_disp.
      wa_in_dt_ent-kbetr           = wa_saida_r-kbetr.
      wa_in_dt_ent-dt_entrega_v    = wa_dt_ent-dt_entrega_v.
      wa_in_dt_ent-dt_entrega      = wa_dt_ent-dt_entrega.


      append wa_in_dt_ent to it_in_dt_ent .
      clear: wa_in_dt_ent.

    endloop.


    case wa_dt_ent-spart.
      when '03'.
        lva_bezei = 'DF'.
      when '02'.
        lva_bezei = 'FT'.
      when '04'.
        if wa_dt_ent-cultura = 'SJ'.
          lva_bezei = 'SS'.
        else.
          if wa_dt_ent-cultura = 'ML'.
            lva_bezei = 'SM'.
          else.
            if wa_dt_ent-cultura = 'AL'.
              lva_bezei = 'SA'.
            endif.
          endif.
        endif.
    endcase.


    obj_taxa->altera_data_entrega( i_numero     = wa_dt_ent-doc_simulacao
                                   i_tipo       = 'FRI'
                                   i_bezei      = lva_bezei
                                   i_acao       = ''
                                   i_dt_entrega = it_in_dt_ent ).

    clear:  it_in_dt_ent, lva_bezei .


  endloop.

endform.

*&---------------------------------------------------------------------*
*&      FORM CHECK_DT_ENTREGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form check_dt_entrega.

  loop at it_dt_ent into wa_dt_ent.

    clear: check_dt_ent, v_erro.

    if  wa_dt_ent-dt_entrega is initial.
      message |A nova data de entrega deve ser informada para a OV: {  wa_dt_ent-vbeln  }| type 'S' display like 'E'.
      v_erro = 'X'.
      "EXIT.

    else.
      if  wa_dt_ent-dt_entrega < sy-datum.
        message |"A nova data de entrega informada para a OV: { wa_dt_ent-vbeln }, deve ser maior ou igual ao dia de hoje"| type 'S' display like 'E'.
        v_erro = 'X'.
        "EXIT.
      else.

        zcl_solicitacao_ov=>dia_util( exporting p_vencimento = wa_dt_ent-dt_entrega
                                      importing e_subrc      = check_dt_ent ).

        if check_dt_ent is initial.
          message |A nova data de entrega informada para a OV: { wa_dt_ent-vbeln } , deve ser dia útil| type 'S' display like 'E'.
          v_erro = 'X'.
          "EXIT.
        endif.
      endif.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ROUTE
*&---------------------------------------------------------------------*

"Comentando o f_get_route para subir os ajustes do <RIM-SKM-IR120585-23.12.22

form f_get_route using p_lifnr type lifnr
                       p_kunnr type kunnr
              changing p_route type route_vl.

  v_erro = 'X'. " se nao tiver erro, será limpada

  check p_kunnr is not initial.

  select single lzone from kna1
    into @data(lv_lzone_k)
  where kunnr = @p_kunnr.

  check lv_lzone_k is not initial.

  select single lzone from lfa1
    into @data(lv_lzone_l)
  where lifnr = @p_lifnr.

  check lv_lzone_l is not initial.

  select single route from trolz
    into p_route
      where aland = 'BR'
        and azone = lv_lzone_l
        and lland = 'BR'
  and lzone = lv_lzone_k.

  check sy-subrc eq 0.

  check p_route is not initial.

  v_erro = space.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
form f_popup_to_confirm using p_question type c
                     changing p_answer type c.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar       = sy-title
      text_question  = p_question
    importing
      answer         = p_answer
    exceptions
      text_not_found = 1
      others         = 2.

  if sy-subrc <> 0.
    perform f_mensagem_sistema.
  endif.

endform.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
form f_mensagem_sistema.

  message id sy-msgid type 'S' number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like sy-msgty.

endform.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
form f_get_mensagem_sistema changing p_msgtx type msgtx.

  message id sy-msgid type 'S' number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into p_msgtx.

endform.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_PRICE
*&---------------------------------------------------------------------*
form f_atualiza_price using p_cenar_fat type numc2
                            p_vbeln type vbeln
                            ps_values_popup type zsds078
                            uv_deduzir_hedge type c
                   changing cs_zsdt0090 type zsdt0090.

  data: tl_vbak            type table of vbak with header line,
        tl_vbap            type table of vbap with header line,
        tl_vbkd            type table of vbkd with header line,
        tl_vbep            type table of vbep with header line,
        wl_orderheaderin   type bapisdh1,
        wl_orderheaderinx  type bapisdh1x,
        tl_bapisditm       type table of bapisditm with header line,
        tl_bapisditmx      type table of bapisditmx with header line,
        wl_return          type bapiret2,
        tl_schedule_lines  type table of bapischdl with header line,
        tl_schedule_linesx type table of bapischdlx with header line,
        wl_tabix           type sy-tabix,
        tl_zsdt0090        type table of zsdt0090 with header line,
        seq                type zsdt0090-sequencia.


  data ls_estorno type zsdt0090.











  " 10.04.2023 - RAMON - 109279 - NAO ATUALIZAR A DATA DE VENCIMENTO NA OV -->














**  IF p_cenar_fat NE gc_100_faturada.
**
**    CLEAR tl_return[].
**
**    wl_orderheaderinx-updateflag = 'U'.
**    wl_orderheaderin-exrate_fi = ps_values_popup-taxa_curv_proj.
**    wl_orderheaderinx-exrate_fi = abap_true.
**    wl_orderheaderin-fix_val_dy = ps_values_popup-dtprevpag_n.
**    wl_orderheaderinx-fix_val_dy = abap_true.
**
**    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
**      EXPORTING
**        salesdocument    = p_vbeln
**        order_header_in  = wl_orderheaderin
**        order_header_inx = wl_orderheaderinx
**      TABLES
**        return           = tl_return.
**
**    IF NOT line_exists( tl_return[ type = 'E'] ).
**
**      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**        EXPORTING
**          wait = abap_true.
**
**    ENDIF.
**
**  ENDIF.

  " 10.04.2023 - RAMON - 109279 - NAO ATUALIZAR A DATA DE VENCIMENTO NA OV -->

  move-corresponding wa_saida to it_popup2_aux.

  select single *
    from zsdt0090
      into cs_zsdt0090
    where sequencia = ps_values_popup-sequencia
      and vbelv eq p_vbeln
      and categoria eq 'C'
  and estorno eq abap_false.

  if sy-subrc is initial.

    if ps_values_popup-sequencia is not initial.

      if uv_deduzir_hedge = space.
        perform f_hedge_new using 'T' ps_values_popup cs_zsdt0090.
      else.
        perform f_hedge_new using 'D' ps_values_popup cs_zsdt0090.
      endif.

      perform f_estorna_zsdt0090
        using ps_values_popup
     changing ls_estorno.

    endif.

  else.

    cs_zsdt0090-vbelv = p_vbeln.

  endif.

  "CLEAR cs_zsdt0090.

  perform f_insert_zsdt0090_2
    using 'C'
          ps_values_popup
          ls_estorno
 changing cs_zsdt0090.

  p_0090 = cs_zsdt0090.

  if cs_zsdt0090-kurrf is not initial
    and cs_zsdt0090-data_prevpgto is not initial.
    "PERFORM hedge USING 'P'.

    perform f_hedge_new using 'P' ps_values_popup cs_zsdt0090.

  endif.

  "PERFORM f_exibe_bapi.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNA_ZSDT0090
*&---------------------------------------------------------------------*
form f_estorna_zsdt0090 using ps_popup type zsds078 changing p_estorno type zsdt0090.

  select single * from zsdt0090
    into p_estorno
    where doc_simulacao = ps_popup-doc_simulacao
  and sequencia = ps_popup-sequencia.

  check sy-subrc eq 0.

  p_estorno-estorno = 'X'.

  " 17.04.2024 - RAMON - 96174 -->
  p_estorno-usnam_e = sy-uname.
  p_estorno-data_atual_e = sy-datum.
  p_estorno-hora_atual_e = sy-uzeit.
  " 17.04.2024 - RAMON - 96174 --<

  modify zsdt0090 from p_estorno.

  commit work and wait.

endform.
" 17.04.2024 - RAMON - 96174 -->
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_HEDGE
*&---------------------------------------------------------------------*
form f_popup_trava_cambio using uv_opt_fat type c
                                uv_tela_trav type c
                                ut_desab type  zsdc082
                       changing ct_travas type zsdt0090_t
                                ct_desme type zsdc081
                                cs_popup_values type zsds078
                                cv_erro type c.

  if uv_tela_trav = abap_true.

    call function 'ZSDMF_POPUP_TRAVA_CAMBIO'
      exporting
        iv_edit         = 'X'
        iv_opt_fat      = uv_opt_fat
        it_fields_desab = ut_desab
      importing
        ev_canc         = cv_erro
      tables
        ct_travas       = ct_travas[]
      changing
        cs_popup_values = cs_popup_values.

  else.

    call function 'ZSDMF_POPUP_TRAVA_CAMBIO_0'
      exporting
        iv_edit         = 'X'
        iv_opt_fat      = uv_opt_fat
        it_fields_desab = ut_desab
      importing
        ev_canc         = cv_erro
      tables
        ct_desmembra    = ct_desme
        ct_travas       = ct_travas[]
      changing
        cs_popup_values = cs_popup_values.

  endif.

  check cv_erro is initial.

  perform f_valida_horario_trava
    using cs_popup_values-vkbur
 changing cv_erro.

endform.
" 17.04.2024 - RAMON - 96174 --<
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_HORARIO_TRAVA
*&---------------------------------------------------------------------*
form f_valida_horario_trava using uv_vkorg type vkorg
                         changing cv_erro type c.

  data lr_0333 type range of sy-uzeit.

  "CLEAR cv_erro.

  select 'I' as sign, 'BT' as option, hora_ini as low, hora_fim as high
    from zsdt0333 into table @lr_0333
  where bukrs = @wa_saida-vkorg.

  if sy-subrc eq 0.

    if sy-uzeit not in lr_0333.

      message 'Trava de câmbio não permitida, está fora do horário do mercado!' type 'E'.

      cv_erro = 'X'.

    endif.

  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_DESMEMBRAR_OV
*&---------------------------------------------------------------------*
form f_desmembrar_ov_new using uv_kunnr type kunnr
                               uv_auart type auart
                               ut_desme type zsdc081
                 ut_travas type zsdt0090_t
                      changing cs_popup_values type zsds078
                               cv_vbeln_new type vbeln
                               cv_erro type c.

  data ls_popup_edit type zsds078.
  data ls_zsdt0090 type zsdt0090.
  data ls_estorno type zsdt0090.
  data lt_ov type table of zsds015.
  data lt_ret type table of bapiret2.

  ls_popup_edit = cs_popup_values.

  call function 'ZSDMF_CALCULA_DESMEMBRAMENTO'
    exporting
      iv_vbeln         = cs_popup_values-vbelv
      iv_doc_simulacao = cs_popup_values-doc_simulacao
      "iv_vlr_desme     = us_popup_values-vlr_prev
      "IV_QTD_DESME     =
    importing
      ev_erro          = cv_erro
    tables
      it_ov_desmemb    = ut_desme
      et_ov_desmemb    = lt_ov
    exceptions
      sem_saldo        = 1
      others           = 2.

  if sy-subrc <> 0.

    perform f_mensagem_sistema.
    exit.

  endif.

  check lt_ov is not initial.

  call function 'ZSDMF001_GERA_OV_COCKPIT'
    exporting
      i_auart     = uv_auart
      i_kunnr     = uv_kunnr
*     I_TXT_ORDEM =
*     I_TXT_ITEM  =
*     I_BILL_DOC  =
      i_acao      = 'DESMEMBRAR'
      i_exibe_ret = space
      i_extcal    = p_extcal "FF #145609
    importing
      e_vbeln     = cv_vbeln_new
    tables
      it_ov       = lt_ov
      te_return   = lt_ret.


  " 24.03.2023 - RAMON ---->

  do.

    if sy-index > 20.
      cv_erro = 'X'.
      exit.
    endif.

    select count(*) from vbak
    where vbeln = cv_vbeln_new.

    if sy-dbcnt = 0.
      wait up to 1 seconds.
    else.
      exit.
    endif.

  enddo.

  check cv_erro is initial.

  gv_ov_desme_atual = cv_vbeln_new.

  data ls_0090 type ty_price.

  loop at lt_ov assigning field-symbol(<fs_ov>).

    clear ls_zsdt0090.

    ls_zsdt0090-doc_simulacao = cs_popup_values-doc_simulacao.

    " ------------------------------- OV NOVA
    ls_zsdt0090-vbeln = cv_vbeln_new.
    ls_zsdt0090-posnn = <fs_ov>-posnr.

    " ------------------------------- OV ANTIGA
    ls_zsdt0090-vbelv = cs_popup_values-vbelv.
    ls_zsdt0090-posnv = <fs_ov>-posnr.

    ls_zsdt0090-auart = uv_auart.
    ls_zsdt0090-kunnr = uv_kunnr.

    ls_zsdt0090-zmengv = <fs_ov>-zmeng.
    ls_zsdt0090-ziemev = <fs_ov>-meins.

    " 25.05.2023 - RAMON - ERRO DO ESTORNO -->
    ls_zsdt0090-kmeinv = <fs_ov>-kmeinv.
    ls_zsdt0090-netprv = <fs_ov>-netprv.
    " 25.05.2023 - RAMON - ERRO DO ESTORNO --<

    "PERFORM f_insert_0090_new USING 'D' ls_zsdt0090.

    ls_popup_edit-vbeln = cv_vbeln_new.
    ls_popup_edit-posnr = <fs_ov>-posnr.
    ls_popup_edit-matnr = <fs_ov>-matnr.

    ls_popup_edit-vbelv = cs_popup_values-vbelv.

*    IF ls_popup_edit-edit_index IS NOT INITIAL.
*
*
*      READ TABLE ut_travas ASSIGNING FIELD-SYMBOL(<fs_trava>)
*        INDEX ls_popup_edit-edit_index.
*
*      IF sy-subrc EQ 0.
*
*        " se for faturado, entao o tipo 'D', vai ficar com o que vai para a nova OV
*        IF ls_popup_edit-fat_flg = 'X'.

    ls_popup_edit-vlr_prev = <fs_ov>-netpr.
    ls_popup_edit-juros_prev = 0.
    ls_popup_edit-multa_prev  = 0.
    ls_popup_edit-vlr_liq_prev = ls_popup_edit-vlr_prev.
    ls_popup_edit-vlr_prev_brl = ls_popup_edit-vlr_liq_prev * cs_popup_values-taxa_neg.

*          " se for a faturar, então o D, fica com o que está na tela
*        ELSE.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

    perform f_insert_zsdt0090_2
      using 'D'
   changing ls_popup_edit
            ls_estorno
            ls_zsdt0090.

    zcl_util_=>disparo_0116( i_vbeln = cv_vbeln_new i_posnr = <fs_ov>-posnr ).

  endloop.

  "PERFORM f_exibe_bapi.

  " 24.03.2023 - RAMON ----<

endform.
*&---------------------------------------------------------------------*
*&      Form  HEDGE
*&---------------------------------------------------------------------*
form f_hedge_new using p_direcao type c
                       cs_values_popup type zsds078
                       cs_0090 type zsdt0090.

  cs_values_popup-doc_simulacao = cs_0090-doc_simulacao.

  call function 'ZSDMF_EXECUTA_HEDGE_NEW'
    exporting
      iv_direcao      = p_direcao
      is_popup_values = cs_values_popup
      is_0090         = cs_0090
      is_0040         = wa_zsdt0040.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQ_TRAVA
*&---------------------------------------------------------------------*
form f_get_seq_trava changing cs_0090 type ty_price.

*  SELECT MAX( seq_trava ) AS max FROM zsdt0090
*    INTO @DATA(lv_max)
*    WHERE doc_simulacao = @cs_0090-doc_simulacao
*      AND vbelv = @cs_0090-vbelv
*      AND categoria = 'C'.
*
*  cs_0090-seq_trava = lv_max + 1.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQ_TRAVA
*&---------------------------------------------------------------------*
form f_get_next_seq using uv_doc_simu type zsded003
                changing cv_seq type numc4.

  select max( sequencia ) as max from zsdt0090
    into @data(lv_max)
  where doc_simulacao = @uv_doc_simu.

  cv_seq = lv_max + 1.

  unpack cv_seq to cv_seq.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_0090_NEW
*&---------------------------------------------------------------------*
form f_insert_0090_new using uv_categ type zsded047
                             us_0090 type zsdt0090.

*  DATA ls_0090 TYPE zsdt0090.
*
*  MOVE-CORRESPONDING us_0090 TO ls_0090.
*
*  PERFORM f_get_next_seq
*    USING ls_0090-doc_simulacao
* CHANGING ls_0090-sequencia.
*
*  PERFORM f_get_seq_trava CHANGING ls_0090.
*
*  SELECT SINGLE * FROM vbap
*    INTO @DATA(lt_vbap)
*      WHERE vbeln = ls_0090-vbeln
*        AND posnr = ls_0090-posnr.
*
*    CHECK sy-subrc eq 0.
*
*        ls_zsdt0090-auart = uv_auart.
*    ls_zsdt0090-kunnr = uv_kunnr.
*
*    ls_zsdt0090-zmengv = <fs_ov>-zmeng.
*    ls_zsdt0090-ziemev = <fs_ov>-meins.
*    ls_zsdt0090-kmeinv = <fs_ov>-meins.
*    ls_zsdt0090-netprv = <fs_ov>-netpr.
*
*    ls_0090-spart =
*    ls_0090-zmeng
*    ls_0090-zieme
*    ls_0090-netpr
*    ls_0090-kmein
*    ls_0090-charg
*    ls_0090-matnr
*    ls_0090-matkl
*    ls_0090-inco1
*    ls_0090-werks
*    ls_0090-kunnr
*    ls_0090-lgort
*    ls_0090-flag_imposto
*    ls_0090-auartv
*    ls_0090-vbelv
*    ls_0090-posnv
*    ls_0090-spartv
*    ls_0090-zmengv
*    ls_0090-ziemev
*    ls_0090-netprv
*    ls_0090-kmeinv
*    ls_0090-chargv
*    ls_0090-matnrv
*    ls_0090-matklv
*    ls_0090-inco1v
*    ls_0090-werksv
*    ls_0090-kunnrv
*    ls_0090-lgortv
*    ls_0090-netwr
*    ls_0090-flag_impostov
*    ls_0090-categoria = uv_categ.
*    ls_0090-kurrf
*    ls_0090-valdt
*    ls_0090-desc_absoluto
*    ls_0090-desc_absoluto_lq
*    ls_0090-estorno
*    ls_0090-usnam_e
*    ls_0090-data_atual_e
*    ls_0090-hora_atual_e
*    ls_0090-origem_est
*    ls_0090-route
*    ls_0090-zpesagem
*    ls_0090-parvw
*    ls_0090-cod_parc
*    ls_0090-usnam
*    ls_0090-data_atual
*    ls_0090-hora_atual
*    ls_0090-flag
*    ls_0090-flag1
*    ls_0090-email
*    ls_0090-dt_email
*    ls_0090-hr_email
*    ls_0090-job
*    ls_0090-zterm
*    ls_0090-ztermv
*    ls_0090-dt_entrega
*    ls_0090-dt_entrega_v
*    ls_0090-data_prevpgto
*    ls_0090-data_prevpgtov
*    ls_0090-prev_pgto_usd
*    ls_0090-prev_pgto_brl
*    ls_0090-prev_multa_usd
*    ls_0090-prev_juros_usd
*    ls_0090-prev_vl_liq_usd
*    ls_0090-sequenciav
*    ls_0090-trav_camb_utilizada
*
*

endform.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_TAB_DESMEM
*&---------------------------------------------------------------------*
form f_monta_tab_desmem tables t_ov structure zsds081
                         using us_popup_values type zsds078
                      changing cv_erro type c.

  data lt_desmemb type table of zsds015.

  clear cv_erro.

  call function 'ZSDMF_CALCULA_DESMEMBRAMENTO'
    exporting
      iv_vbeln         = us_popup_values-vbelv
      iv_doc_simulacao = us_popup_values-doc_simulacao
    importing
      ev_erro          = cv_erro
    tables
      et_ov_desmemb    = lt_desmemb
    exceptions
      sem_saldo        = 1
      others           = 2.

  it_sai_aux[] = it_saida.

  delete it_sai_aux where vbeln <> us_popup_values-vbeln.

  loop at lt_desmemb assigning field-symbol(<fs_desm>).

    read table it_sai_aux assigning field-symbol(<fs_aux>)
      with key vbeln = <fs_desm>-vbeln
               posnr = <fs_desm>-posnr.

    check sy-subrc eq 0.

    append initial line to t_ov assigning field-symbol(<fs_ov>).

    <fs_ov>-vbeln = <fs_desm>-vbeln.
    "<fs_ov>-vbeln2 = <fs_desm>-vbeln2.
    <fs_ov>-posnr = <fs_desm>-posnr.
    <fs_ov>-matnr = <fs_desm>-matnr.
    "<fs_ov>-matnr2 = <fs_desm>-matnr2.
    <fs_ov>-arktx = <fs_aux>-arktx.
    <fs_ov>-kwmeng = <fs_aux>-kwmeng.
    <fs_ov>-spart = <fs_aux>-spart.
    <fs_ov>-groes = <fs_desm>-groes.
    <fs_ov>-spart = <fs_desm>-spart.
    <fs_ov>-vrkme = <fs_aux>-vrkme.
    <fs_ov>-sd_disp = <fs_aux>-sd_disp.
    <fs_ov>-qt_tran = 0.
    "<fs_ov>-vl_tran = <fs_desm>-vl_tran.
    "<fs_ov>-rfmng = <fs_desm>-rfmng.
    "<fs_ov>-spart = <fs_desm>-spart.
    <fs_ov>-waers = 'USD'.
    <fs_ov>-vlr_ttl_item = <fs_aux>-netwr.
    "<fs_ov>-vlr_unit = <fs_aux>-netwr / <fs_aux>-kwmeng.
    <fs_ov>-vlr_calc = ( <fs_aux>-netwr / <fs_aux>-kwmeng ) * 1000. " vezes 1000 para preservar as casas decimais
    "<fs_ov>-unit_calc = <fs_desm>-unit_calc.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_LCTO_OV_NFATURADA
*&---------------------------------------------------------------------*
form f_lcto_ov_nfaturada using uv_vbeln_old type vbeln
                               uv_vbeln_new type vbeln.

  data lt_315 type table of zsdt0315.

  call function 'ZSDMF_GRAVA_REG_ZSDT0315'
    exporting
      iv_vbeln             = uv_vbeln_old
      iv_vbeln_new         = uv_vbeln_new
      iv_commit            = space
    tables
      et_zsdt0315          = lt_315
    exceptions
      ov_100_liquidada     = 1
      vlr_desm_maior_ov    = 2
      desm_e_liqui         = 3
      vlr_liqui_maior_ov   = 4
      informar_ov_desmem   = 5
      vlr_ov_desatualizado = 6
      ov_nova_existente    = 7
      others               = 8.

  if sy-subrc = 0.

    modify zsdt0315 from table lt_315.
    commit work and wait.

  else.

    perform f_mensagem_sistema.
    exit.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  f_atualiza_zsdt0090
*&---------------------------------------------------------------------*
form f_atualiza_zsdt0090 using p_vbeln type vbeln
                 p_upd_taxa type c
                               ps_values_popup type zsds078
                      changing cs_zsdt0090 type zsdt0090.

  data ls_estorno type zsdt0090.

  move-corresponding wa_saida to it_popup2_aux.

  select single *
    from zsdt0090
      into cs_zsdt0090
    where sequencia = ps_values_popup-sequencia
      and vbelv eq p_vbeln
      and categoria eq 'C'
  and estorno eq abap_false.

  if sy-subrc is initial.

    if ps_values_popup-sequencia is not initial.

      perform f_estorna_zsdt0090
        using ps_values_popup
     changing ls_estorno.

    endif.

  else.

    cs_zsdt0090-vbelv = p_vbeln.

  endif.

  " 02.06.2023 - atualiza taxa?
  if p_upd_taxa is not initial.
    cs_zsdt0090-kurrf = ps_values_popup-taxa_neg.
  endif.

  if ps_values_popup-vlr_liq_prev is not initial.

    perform f_insert_zsdt0090_2
      using 'C'
            ps_values_popup
            ls_estorno
   changing cs_zsdt0090.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_HEDGE_INSERT
*&---------------------------------------------------------------------*
form f_hedge_insert using us_zsdt0090 type zsdt0090
                          uv_vbeln_old type vbeln
                          uv_tipo type c.

  data lv_count type sytabix value 0.
  data lv_tot type zdmbtr.

  data ls_vinc0090 type zsdt0090.
  data lv_tot_vinc type zdmbtr.

  data lv_venda_ativa type c.

  data ls_0090 type zsdt0090.

  ls_0090 = us_zsdt0090.

  case uv_tipo.
    when 'V'. " Venda

      " SÓ VENDE SE TIVER COMPRADO

      if uv_vbeln_old is not initial.

        select * from zsdt0094
          into table @data(lt_0094)
          where nro_sol_ov = @ls_0090-doc_simulacao
            and vbeln = @uv_vbeln_old
        and tipo_taxa in ('C','V').

        if sy-subrc eq 0.

          ls_0090-vbelv = uv_vbeln_old.

          loop at lt_0094 assigning field-symbol(<fs_ult_compra>) where tipo_taxa = 'C'.
          endloop.

          if <fs_ult_compra> is assigned.

            " A taxa curva da venda tem que ser igual ao da ultima compra
            ls_0090-kurrf = <fs_ult_compra>-taxa_cambio.

            " A data prev pgto deve ser igual a data de vencimento da ultima compra
            ls_0090-data_prevpgto = <fs_ult_compra>-data_venc.

          endif.

        endif.

      else.

        select * from zsdt0094
          into table lt_0094
          where nro_sol_ov = ls_0090-doc_simulacao
            and vbeln = ls_0090-vbelv
        and tipo_taxa in ('C','V').

      endif.

      sort lt_0094 by data_registro hora_registro ascending.

      loop at lt_0094 assigning field-symbol(<fs_0094>).

        case <fs_0094>-tipo_taxa.
          when 'C'.
            add 1 to lv_count.
          when 'V'.
            subtract 1 from lv_count.
          when others.
        endcase.

      endloop.

      if lv_count > 0.

        perform f_sap_indicator using 'Executando hedge Venda...'.

        zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_0090
                                                i_acao = 'VENDA'
                                                i_tipo = 'VDI'
                                                i_dir  = 'T'
                                                ).
        " verifica se tem no pai, faz a venda do pai se tiver.
      else.

        ls_vinc0090 = ls_0090.

        perform f_hedge_consulta_vinculo
          using ls_0090-vbeln
       changing ls_vinc0090-vbelv
                lv_tot_vinc.

        if lv_tot_vinc > 0.

          ls_vinc0090-vbeln = ls_vinc0090-vbelv.
          "ls_vinc0090-vbelv =

          perform f_sap_indicator using 'Executando hedge Venda...'.

          " venda do pai
          zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_vinc0090
                                                  i_acao = 'VENDA'
                                                  i_tipo = 'VDI'
                                                  i_dir  = 'T'
                                                  ).
        endif.

      endif.

    when 'C'. " Compra

      if uv_vbeln_old is not initial.

        select * from zsdt0094
        into table lt_0094
        where nro_sol_ov = ls_0090-doc_simulacao
          and vbeln = uv_vbeln_old
        and tipo_taxa in ('C','V').

        sort lt_0094 by data_registro hora_registro ascending.

        clear lv_tot.

        loop at lt_0094 assigning <fs_0094>.

          case <fs_0094>-tipo_taxa.

            when 'C'.

              add <fs_0094>-total_proporc to lv_tot.

            when 'V'.

              add <fs_0094>-total_proporc to lv_tot.

            when others.

          endcase.

        endloop.

        " se zerou compra e venda na anterior, entao pode lançar novo
        if lv_tot is initial.

          perform f_sap_indicator using 'Executando hedge Compra...'.

          zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_0090
                                                  i_acao = 'PRICE_NEW'
                                                  i_tipo = 'VDI'
                                                  i_dir  = 'P'
                                                  ).

          " 31.05.2023 - se é uma compra e nao achou na antiga, então verifica se tem uma venda ativa
        else.

          select * from zsdt0094
            into table lt_0094
            where nro_sol_ov = ls_0090-doc_simulacao
              and vbeln = ls_0090-vbelv
          and tipo_taxa in ('C','V').

          loop at lt_0094 assigning <fs_0094>.

            case <fs_0094>-tipo_taxa.

              when 'C'.

                add <fs_0094>-total_proporc to lv_tot.
                lv_venda_ativa = space.

              when 'V'.

                add <fs_0094>-total_proporc to lv_tot.
                lv_venda_ativa = 'X'.

              when others.

            endcase.

          endloop.

          " se sim, pode comprar com taxas atualizadas, ou se initial pode ter uma compra
          if lv_venda_ativa = 'X' or lt_0094[] is initial.

            perform f_sap_indicator using 'Executando hedge Compra...'.

            zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_0090
                                                    i_acao = 'PRICE_NEW'
                                                    i_tipo = 'VDI'
                                                    i_dir  = 'P'
                                                    ).
          endif.

        endif.

      else.

        perform f_sap_indicator using 'Executando hedge Compra...'.

        zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_0090
                                                i_acao = 'PRICE_NEW'
                                                i_tipo = 'VDI'
                                                i_dir  = 'P'
                                                ).

      endif.


    when others.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
form f_sap_indicator using p_text type c.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 80
      text       = p_text.

endform.
"16.06.2023 - RAMON - 98680 -->
*&---------------------------------------------------------------------*
*&      Form  F_AGRUPAR_OV
*&---------------------------------------------------------------------*
form f_agrupar_ov .

  data lr_vbeln_range type shp_vbeln_range_t.
  data lv_vbeln type vbeln.
  data lv_canc.

  it_sai_aux[] = it_saida.

  delete it_sai_aux where check = abap_false.

  if it_sai_aux[] is initial.
    message 'Selecionar pelo menos 1 linhas' type 'S' display like 'E'.
    exit.
  endif.

  loop at it_sai_aux assigning field-symbol(<fs_sai>).

    append initial line to lr_vbeln_range assigning field-symbol(<fs_vbeln>).

    <fs_vbeln>-sign = 'I'.
    <fs_vbeln>-option  = 'EQ'.
    <fs_vbeln>-low = <fs_sai>-vbeln.

  endloop.

  call function 'ZSDMF_INSUMOS_AGRUPAMENTO_OV'
    exporting
      ir_vbeln_range = lr_vbeln_range
    importing
      ev_vbeln_agp   = lv_vbeln
      ev_erro        = lv_canc.

  if lv_canc is initial and lv_vbeln is not initial.

    message s260(69) with lv_vbeln.
    exit.

  endif.

endform.
"16.06.2023 - RAMON - 98680 --<
" 10.09.2024 - RAMON - 97513 -->
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_IMPOSTO
*&---------------------------------------------------------------------*
form f_atualiza_imposto.

  call function 'ZSDMF_INSUMOS_ATU_IMPO_FERTILI'.

endform.
*&---------------------------------------------------------------------*
*&      FORM  PRICE_NEW
*&---------------------------------------------------------------------*
form price_new.

  data lt_desab type zsdc082.
  data ls_popup_hedge type zsds078.
  data lt_travas type table of zsdt0090.

  data lv_msgtx type msgtx.
  data lv_ret type c.
  data lv_cenar_fat type numc2.
  data lv_cenar_liq type numc2.
  data lt_0090 type table of zsdt0090.
  data lt_desme type table of zsds081.
  data lv_erro type c.
  data lv_dt_prevpgto type sydatum.

  data lv_nova_ov type vbeln.

  data ls_0090_esto type zsdt0090.
  data ls_popup_edit type zsds078.

  clear: wa_0090, gv_ov_desme_atual.

  it_sai_aux[] = it_saida.

  delete it_sai_aux where check = abap_false.

  "break rblima.

  if lines( it_sai_aux ) eq 1.

    read table it_saida into wa_saida with key check = abap_true.

    clear wg_exit.

    if 'ZROB_ZRPF_ZREB' cs wa_saida-auart.

      wg_exit = abap_true.

      message 'Ação não permitida para ordens de Devolução/Recusa!' type 'I'.

      exit.

    endif.

    check wg_exit is initial.

    if wa_saida-waerk eq 'BRL'.

      message 'A Trava Cambio é utilizada somente para Moedas em USD!' type 'I'.

      exit.

    endif.

    move wa_saida-vbeln to wa_0090-vbelv.

*    SELECT SINGLE kurrf valdt
*      FROM vbkd
*      INTO (wa_taxa_old-taxa, lv_dt_prevpgto)
*      WHERE vbeln EQ wa_0090-vbelv.

    "READ TABLE it_zsdt0040 ASSIGNING FIELD-SYMBOL(<fs_0040>)
    "WITH KEY doc_simulacao = wa_saida-doc_simulacao.

    "IF sy-subrc EQ 0.

    read table it_zsdt0040 into wa_zsdt0040
      with key doc_simulacao = wa_saida-doc_simulacao.

    if wa_saida-tpsim = 'VV'.
      lv_dt_prevpgto = wa_zsdt0040-dtvencov.
    else.

      if wa_zsdt0040-dtprevpgto is not initial.
        lv_dt_prevpgto = wa_zsdt0040-dtprevpgto.
      else.
        lv_dt_prevpgto = wa_zsdt0040-dtpgtcult.
      endif.

    endif.

    " 12.06.2023 - 114475 - RAMON -------->

    perform f_valida_horario_trava
      using wa_saida-vkorg
      changing lv_erro.

    check lv_erro is initial.

    " 12.06.2023 - 114475 - RAMON --------<


    " verifica se tem alteração na data de pagamento
    select single valdt from zsdt0090
      into @data(lv_valdt)
        where doc_simulacao = @wa_saida-doc_simulacao
          and vbelv = @wa_saida-vbeln
          and categoria = 'V'
    and estorno = @space.

    " se tiver, altera a prevpagto
    if sy-subrc eq 0.
      lv_dt_prevpgto = lv_valdt.
      wa_zsdt0040-dtinijuros = lv_valdt.
    else.

      " 13.06.2023 - 115347 - cen 3 -->
      select single vbelv from zsdt0090
       into @data(lv_vbelv_d)
         where doc_simulacao = @wa_saida-doc_simulacao
           and vbeln = @wa_saida-vbeln
           and categoria = 'D'
      and estorno = @space.

      if sy-subrc eq 0.

        select single valdt from zsdt0090
          into lv_valdt
            where doc_simulacao = wa_saida-doc_simulacao
              and vbelv = lv_vbelv_d
              and categoria = 'V'
        and estorno = space.

        if sy-subrc eq 0.

          lv_dt_prevpgto = lv_valdt.
          wa_zsdt0040-dtinijuros = lv_valdt.

        endif.

      else.

        wa_zsdt0040-dtinijuros = lv_dt_prevpgto.

      endif.

      " 13.06.2023 - 115347 - cen 3 --<

      "wa_zsdt0040-dtinijuros = lv_dt_prevpgto.
    endif.

    select single tx_multa from zsdt0051
      into @data(lv_tx_multa)
    where nro_sol_ov = @wa_saida-doc_simulacao.

    call function 'ZSDMF_DEFINE_CENARIO_INSUMOS'
      exporting
        iv_doc_simulacao = wa_saida-doc_simulacao
        iv_vbeln         = wa_saida-vbeln
        iv_posnr         = wa_saida-posnr
        iv_matnr         = wa_saida-matnr
        iv_werks         = wa_saida-werks
        iv_bukrs         = wa_saida-vkorg
        iv_vkbuk         = wa_saida-vkbur
        iv_juros_ano     = wa_zsdt0040-juros_ano
        iv_dtinijuros    = wa_zsdt0040-dtinijuros
        iv_taxa_multa    = lv_tx_multa
        iv_dt_prevpgto   = lv_dt_prevpgto
      importing
        ev_erro          = lv_erro
        ev_cenar_fat     = lv_cenar_fat
        ev_cenar_liq     = lv_cenar_liq
        es_popup_values  = ls_popup_hedge
      tables
        et_travas        = lt_travas
      exceptions
        fat_incompleto   = 1
        others           = 2.

    if sy-subrc <> 0.

      perform f_mensagem_sistema.
      lv_erro = 'X'.
      exit.

    endif.

    " 10.09.2024 - RAMON - 97513 -->
    if gv_stvarv_ativa is not initial.

      perform f_check_autorizacao_trava
        using wa_saida-vkorg
              wa_saida-vkbur
     changing lt_desab.

    endif.
    " 10.09.2024 - RAMON - 97513 --<

    " 09.01.2024 - RAMON -- Regra deve ser antes de qualquer cenario -->
    if lv_cenar_liq = gc_100_liquidada.

      message 'Ordem de Venda Totalmente Liquidada!' type 'S' display like 'E'.

      exit.

    endif.
    " 09.01.2024 - RAMON -- Regra deve ser antes de qualquer cenario --<

    case lv_cenar_fat.
      when gc_0_faturada.
        " CS2021000667 ZSDT0087 - melhorias trava cambio Ov não faturada - Insumos - Parte 3

        append value #( fieldname = 'ZSDS078-VLR_PREV' ) to lt_desab.



        " 01 - OV que possui Liquidação - não possui faturamento - não possui trava de cambio:
        if ( ls_popup_hedge-tt_liq > 0 and ls_popup_hedge-tt_fat = 0 and lt_travas is initial )

          " 18.09.2023 - 123292 - Quando tiver mais de uma trava e nao tiver selecionado nenhuma
          or ( ls_popup_hedge-editing = abap_false and lines( lt_travas ) > 1 ).
          " 18.09.2023 - <
          perform f_monta_tab_desmem
                    tables lt_desme
                     using ls_popup_hedge
                  changing lv_erro.

          check lv_erro is initial.

          perform f_popup_trava_cambio
            using space ''
                  lt_desab
         changing lt_travas
                  lt_desme
                  ls_popup_hedge
                  lv_erro.

*          CALL FUNCTION 'ZSDMF_POPUP_TRAVA_CAMBIO_0'
*            EXPORTING
*              iv_edit         = 'X'
*              iv_opt_fat      = space
*              it_fields_desab = lt_desab
*            IMPORTING
*              ev_canc         = lv_erro
*            TABLES
*              ct_desmembra    = lt_desme
*            CHANGING
*              cs_popup_values = ls_popup_hedge.

          check lv_erro is initial.

          break rblima. "#debug #apagar

          " Gerar Nova OV com a trava de cambio
          perform f_desmembrar_ov_new
            using ls_popup_hedge-kunnr
                  wa_saida-auart
                  lt_desme
                  lt_travas
         changing ls_popup_hedge
                  lv_nova_ov
                  lv_erro.

          if lv_erro is initial.

            " E disparar hedge de compra.

            data(lv_vbeln_old) = wa_saida-vbeln.

            ls_popup_hedge-vbeln = lv_nova_ov.
            ls_popup_hedge-vbelv = lv_nova_ov.
            ls_popup_hedge-posnr = wa_saida-posnr.
            ls_popup_hedge-matnr = wa_saida-matnr.

            " Lança Trava de Cambio - 'C'
            perform f_atualiza_zsdt0090
              using lv_nova_ov
                    ''
                    ls_popup_hedge
           changing im_0090.

            " Lança no Hedge uma compra do valor da nova OV
            perform f_hedge_insert using im_0090 '' 'C'.

            " Estornar anterior
            perform f_canc_lcto_zsdt0315 using lv_vbeln_old.

            " desconta o valor da OV
            data lv_ov_old type netwr_ap.

            " vlr antigo é igual ao valor da ov menos o valor do desmembramento
            lv_ov_old = ls_popup_hedge-vlr_ov - ls_popup_hedge-vlr_liq_prev.

            if lv_ov_old < 0.
              lv_ov_old = lv_ov_old * -1.
            endif.

            " Atualizar ov antiga
            perform f_updt_lcto_zsdt0315
              using lv_vbeln_old
                    ''
                    lv_ov_old
                    lv_ov_old.

            " Atualizar ov nova
            perform f_updt_lcto_zsdt0315
              using lv_nova_ov
                    lv_vbeln_old
                    ls_popup_hedge-vlr_liq_prev
                    0.

            perform f_exibe_bapi.

          endif.

          " 02 - OV não possui Liquidação - não possui faturamento - não possui trava de cambio:
        elseif ls_popup_hedge-tt_liq = 0 and ls_popup_hedge-tt_fat = 0 and lt_travas is initial.

          perform f_monta_tab_desmem
                    tables lt_desme
                     using ls_popup_hedge
                  changing lv_erro.

          check lv_erro is initial.

          perform f_popup_trava_cambio
            using space ''
                  lt_desab
         changing lt_travas
                  lt_desme
                  ls_popup_hedge
                  lv_erro.


*          CALL FUNCTION 'ZSDMF_POPUP_TRAVA_CAMBIO_0'
*            EXPORTING
*              iv_edit         = 'X'
*              iv_opt_fat      = space
*              it_fields_desab = lt_desab
*            IMPORTING
*              ev_canc         = lv_erro
*            TABLES
*              ct_desmembra    = lt_desme
*            CHANGING
*              cs_popup_values = ls_popup_hedge.

          check lv_erro is initial.

          break rblima. "#debug #apagar

          " Se a quantidade preenchida no campo for igual ao total dos Itens da OV e o
          " campo Cliente for preenchido com o mesmo Emissor da OV, não realizar desmembramento da OV
          data(lv_qtde_desm_ov) = reduce rfmng( init sum = 0 for tab in it_saida where ( vbeln = wa_saida-vbeln ) next sum = sum + tab-sd_disp ).

          if ( ls_popup_hedge-qtde_total_desm = lv_qtde_desm_ov ) and ( ls_popup_hedge-kunnr = wa_saida-kunnr ).

            perform f_atualiza_price
              using lv_cenar_fat
                    wa_saida-vbeln
                    ls_popup_hedge
                    space
           changing im_0090.

          else.

            " Gerar Nova OV com a trava de cambio - e disparar hedge de compra.
            perform f_desmembrar_ov_new
              using ls_popup_hedge-kunnr
                    wa_saida-auart
                    lt_desme
                    lt_travas
           changing ls_popup_hedge
                    lv_nova_ov
                    lv_erro.

            if lv_erro is initial.

              ls_popup_hedge-vbeln = lv_nova_ov.
              ls_popup_hedge-vbelv = lv_nova_ov.
              ls_popup_hedge-posnr = wa_saida-posnr.
              ls_popup_hedge-matnr = wa_saida-matnr.

              perform f_atualiza_price
                using lv_cenar_fat
                      lv_nova_ov
                      ls_popup_hedge
                      space
             changing im_0090.

            endif.

          endif.

          " 03 - OV não possui Liquidação - não possui faturamento - já possui trava de cambio:
        elseif ls_popup_hedge-tt_liq = 0 and ls_popup_hedge-tt_fat = 0 and lt_travas is not initial.

          " Usuário for da Filial, nao permitir abrir popup.
          if ls_popup_hedge-acesso_filial is initial.

            message 'Já existe trava de cambio para esta OV! Por favor verifique com o Corporativo.' type 'S' display like 'E'.
            lv_erro = 'X'.
            exit.

          else.

            "QUANTIDADE E VALOR PREVISTO DEVERÃO VIR FECHADO E VAZIO
            append value #( fieldname = 'ZSDS081-QT_TRAN' ) to lt_desab.
            append value #( fieldname = 'ZSDS078-VLR_PREV' ) to lt_desab.

            perform f_monta_tab_desmem
                      tables lt_desme
                       using ls_popup_hedge
                    changing lv_erro.

            perform f_popup_trava_cambio
              using space ''
                    lt_desab
           changing lt_travas
                    lt_desme
                    ls_popup_hedge
                    lv_erro.


*            CALL FUNCTION 'ZSDMF_POPUP_TRAVA_CAMBIO_0'
*              EXPORTING
*                iv_edit         = 'X'
*                iv_opt_fat      = space
*                it_fields_desab = lt_desab
*              IMPORTING
*                ev_canc         = lv_erro
*              TABLES
*                ct_desmembra    = lt_desme
*                ct_travas       = lt_travas
*              CHANGING
*                cs_popup_values = ls_popup_hedge.

            check lv_erro is initial.

            break rblima. "#debug #apagar

            clear im_0090.

            perform f_atualiza_price
              using gc_0_faturada
                    wa_0090-vbelv
                    ls_popup_hedge
                    space
           changing im_0090.

            perform f_exibe_bapi.

          endif.

          " 04 - OV possui Liquidação - não possui faturamento - já possui trava de cambio:
        elseif ls_popup_hedge-tt_liq > 0 and ls_popup_hedge-tt_fat = 0 and lt_travas is not initial.

          " Usuário for da Filial, nao permitir abrir popup.
          if ls_popup_hedge-acesso_filial is initial.

            message 'Já existe trava de cambio e liquidação parcial para esta OV! Por favor verifique com o Corporativo.' type 'S' display like 'E'.
            lv_erro = 'X'.
            exit.

          else.

            message 'Existe trava de cambio e liquidação parcial para esta OV! Favor usar ação Desmembrar OV.' type 'S' display like 'E'.
            lv_erro = 'X'.
            exit.

          endif.

        endif.

      when gc_parc_faturada.

        " Ov com faturamento parcial, é necessário desmembrar! Deseja continuar?
        message s102(zsd) into lv_msgtx.

        perform f_popup_to_confirm
          using lv_msgtx
       changing lv_ret.

        if lv_ret eq '1'.

          perform f_popup_trava_cambio
            using 'X' 'X'
                  lt_desab
         changing lt_travas
                  lt_desme
                  ls_popup_hedge
                  lv_erro.

          if lv_erro is initial.

            break rblima. "#debug #apagar

            clear lt_desme[].

            perform f_desmembrar_ov_new
              using wa_saida-kunnr
                    wa_saida-auart
                    lt_desme
                    lt_travas
           changing ls_popup_hedge
                    lv_nova_ov
                    lv_erro.

            if lv_nova_ov is not initial and lv_erro is initial.

*              IF sy-uname = 'RBLIMA'.

              " saber qual flag esta marcado
              if ls_popup_hedge-fat_flg = 'X'.

                " estorna na antiga(e houver) e lança de novo
                perform f_upd_trava_cambio
                  using ls_popup_hedge-doc_simulacao
                        ls_popup_hedge-vbelv
                        ls_popup_hedge-sequencia
                        ls_popup_hedge-vbelv
                        ls_popup_hedge-dtprevpag_n
                        ls_popup_hedge-taxa_neg
                        ls_popup_hedge-vlr_prev
                        ls_popup_hedge-multa_prev
                        ls_popup_hedge-juros_prev.

                " lança as demais travas na nova
                "PERFORM f_lcto_nova_ov_trava USING lv_nova_ov ls_popup_hedge-edit_index lt_travas.

                " consolidar o valor do Hedge
                "PERFORM f_consolidar_hedge USING lv_nova_ov ls_popup_hedge.

*                  " Hedge lança vinculo
*                  PERFORM f_hedge_criar_vinculo
*                    USING ls_popup_hedge-doc_simulacao
*                          ls_popup_hedge-sequencia
*                          lv_nova_ov
*                          wa_saida-vbeln.


              elseif ls_popup_hedge-afat_flg = 'X'.

                "IF ls_popup_hedge-editing IS NOT INITIAL.

                " estorna na antiga e lança na nova
                perform f_upd_trava_cambio
                  using ls_popup_hedge-doc_simulacao
                        ls_popup_hedge-vbelv
                        ls_popup_hedge-sequencia
                        lv_nova_ov
                        ls_popup_hedge-dtprevpag_n
                        ls_popup_hedge-taxa_neg
                        ls_popup_hedge-vlr_prev
                        ls_popup_hedge-multa_prev
                        ls_popup_hedge-juros_prev.

                perform f_upd_trava_rest_fat
                  using ls_popup_hedge
                        lv_nova_ov
                        lt_travas.

*                  " Hedge lança vinculo
*                  PERFORM f_hedge_criar_vinculo
*                    USING ls_popup_hedge-doc_simulacao
*                          ls_popup_hedge-sequencia
*                          lv_nova_ov
*                          wa_saida-vbeln.

              endif.

*              ELSE.
*
*                IF ls_popup_hedge-fat_flg = 'X'.
*
*                  " 26.04.2023 - RAMON - Correção OV com trava, alteração -->
*                  IF ls_popup_hedge-editing = 'X'.
*
*                    PERFORM f_edita_trava_hedge USING ls_popup_hedge lt_travas 'X' 'X' 'X' 'X'.
*
*                  ELSE.
*
*                    " executa somente hedge de compra
*                    PERFORM f_edita_trava_hedge USING ls_popup_hedge lt_travas '' '' '' 'X'.
*
*                  ENDIF.
*
*                  ls_popup_hedge-vbeln = lv_nova_ov.
*                  ls_popup_hedge-vbelv = lv_nova_ov.
*                  ls_popup_hedge-posnr = wa_saida-posnr.
*                  ls_popup_hedge-matnr = wa_saida-matnr.
*
*                  PERFORM f_atualiza_afat_zsdt0090 USING ls_popup_hedge lt_travas.
*
*                  " Faurado sem trava, compra na nova OV
*                  "IF ls_popup_hedge-editing = 'X'.
*                  "PERFORM f_hedge_insert USING im_0090 '' 'C'.
*                  "ENDIF.
*
*                  " Hedge lança vinculo
*                  PERFORM f_hedge_criar_vinculo
*                    USING ls_popup_hedge-doc_simulacao
*                          ls_popup_hedge-sequencia
*                          lv_nova_ov
*                          wa_saida-vbeln.
*
*                ELSEIF ls_popup_hedge-afat_flg = 'X'.
*
*                  " 26.04.2023 - RAMON - Correção OV com trava, alteração -->
*                  IF ls_popup_hedge-editing = 'X'.
*
*                    " só atualiza a 0090, a venda do valor a faturar vai ser feito logo abaixo
*                    PERFORM f_edita_trava_hedge USING ls_popup_hedge lt_travas '' 'X' '' ''.
*
*                  ENDIF.
*
*                  " 26.04.2023 - RAMON - Correção OV com trava, alteração --<
*
*                  ls_popup_hedge-vbeln = lv_nova_ov.
*                  ls_popup_hedge-vbelv = lv_nova_ov.
*                  ls_popup_hedge-posnr = wa_saida-posnr.
*                  ls_popup_hedge-matnr = wa_saida-matnr.
*
*                  " Lança somente o faturado na 0090.
*                  PERFORM f_atualiza_zsdt0090
*                    USING lv_nova_ov
*                          ''
*                          ls_popup_hedge
*                 CHANGING im_0090.
*
*                  " Lança no Hedge uma venda do valor faturado
*                  PERFORM f_hedge_insert USING im_0090 wa_saida-vbeln 'V'.
*
*                  " Lança no Hedge uma compra do valor faturado
*                  PERFORM f_hedge_insert USING im_0090 wa_saida-vbeln 'C'.
*
*                  " Hedge lança vinculo
*                  PERFORM f_hedge_criar_vinculo
*                    USING ls_popup_hedge-doc_simulacao
*                          ls_popup_hedge-sequencia
*                          lv_nova_ov
*                          wa_saida-vbeln.
*
*                ENDIF.
*
*
*              ENDIF.

            endif.

          else.

            exit.

          endif.

        endif.

      when gc_100_faturada.

        " 09.01.2024 - RAMON -- Regra deve ser antes de qualquer cenario -->
*        " verificar se esta liquidada
*        IF lv_cenar_liq = gc_100_liquidada.
*
*          MESSAGE 'Ordem de Venda Totalmente Liquidada!' TYPE 'S' DISPLAY LIKE 'E'.
*
*          EXIT.
*
*        ENDIF.
        " 09.01.2024 - RAMON -- Regra deve ser antes de qualquer cenario --<

        perform f_popup_trava_cambio
          using space 'X'
                lt_desab
       changing lt_travas
                lt_desme
                ls_popup_hedge
                lv_erro.

        if ls_popup_hedge is not initial.

          clear im_0090.

          perform f_atualiza_price
            using gc_100_faturada
                  wa_0090-vbelv
                  ls_popup_hedge
                  space
         changing im_0090.

          perform f_exibe_bapi.

        endif.

    endcase.

  else.

    message 'Selecione apenas uma Linha!' type 'I'.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_UPD_TRAVA_REST_FAT
*&---------------------------------------------------------------------*
form f_upd_trava_rest_fat using us_values_popup type zsds078
                                uv_vbeln type vbeln
                                ut_travas type zsdt0090_t.

  data(lt_travas) = ut_travas.
  data lv_tt_saldo_trava type netwr_ap.
  data lv_vlr_trava_new type netwr_ap.
  data lv_vlr_trava_old type netwr_ap.
  data lv_vlr_trava_aux type netwr_ap.

  lv_tt_saldo_trava = us_values_popup-c_trav_cam.

  if us_values_popup-editing = 'X'.

    " tem que tirar o valor que esta editando pq foi para outra ov
    read table lt_travas assigning field-symbol(<fs_trava>)
      with key sequencia = us_values_popup-sequencia.

    if sy-subrc eq 0.
      subtract <fs_trava>-prev_vl_liq_usd from lv_tt_saldo_trava.
    endif.

    " retira das travas para o calculo
    delete lt_travas where sequencia = us_values_popup-sequencia.
  endif.

  check lv_tt_saldo_trava > us_values_popup-tt_fat.

  " saldo que precisa ser levado para outra ov
  lv_tt_saldo_trava = lv_tt_saldo_trava - us_values_popup-tt_fat.

  sort lt_travas by data_prevpgto ascending.

  loop at lt_travas assigning <fs_trava>.

    " retiro o valor
    lv_vlr_trava_old = <fs_trava>-prev_vl_liq_usd - lv_tt_saldo_trava. " 14.07.2023
    "lv_vlr_trava_old = <fs_trava>-prev_pgto_usd - lv_tt_saldo_trava. " 14.07.2023

    " se ficar positivo, significa que tenho que alterar essa trava e o restante vai para a nova
    if lv_vlr_trava_old > 0.

      lv_vlr_trava_new = lv_vlr_trava_old - <fs_trava>-prev_vl_liq_usd. " 17.07.2023
      "lv_vlr_trava_new = lv_vlr_trava_old - <fs_trava>-prev_pgto_usd. " 17.07.2023

      if lv_vlr_trava_new < 0.
        lv_vlr_trava_new = lv_vlr_trava_new * -1.
      endif.

      " o calculo da ov antiga(atual), deve ser refeito, pq  é passado juros e multa
      " e com isso não pode ir o valor que está, tem que pegar o valor bruto - 18/07/2023
      lv_vlr_trava_old = <fs_trava>-prev_pgto_usd - lv_tt_saldo_trava.

      " alterar na atual ----
      perform f_upd_trava_cambio
        using <fs_trava>-doc_simulacao
              <fs_trava>-vbelv
              <fs_trava>-sequencia
              <fs_trava>-vbelv "<-- ov atual
              <fs_trava>-data_prevpgto
              <fs_trava>-kurrf
              lv_vlr_trava_old " <--lança aqui o max permitido
              <fs_trava>-prev_multa_usd
              <fs_trava>-prev_juros_usd.

      " lança o restante na nova OV
      perform f_upd_trava_cambio
        using <fs_trava>-doc_simulacao
              <fs_trava>-vbelv
              <fs_trava>-sequencia
              uv_vbeln "<-- ov nova
              <fs_trava>-data_prevpgto
              <fs_trava>-kurrf
              lv_vlr_trava_new
              '0.00' "<fs_trava>-prev_multa_usd " nova nasce sem juros e multa 14.07.2023
              '0.00'. "<fs_trava>-prev_juros_usd.

      " pode sair do loop o saldo da linha da trava ficou positivo
      exit.

      " se ficar negativo, jogo o valor total dessa trava para a outra
    else.

      lv_vlr_trava_new = <fs_trava>-prev_pgto_usd.

      "lv_vlr_trava_aux = lv_tt_saldo_trava.

      "SUBTRACT lv_vlr_trava_new FROM lv_tt_saldo_trava.
      subtract <fs_trava>-prev_vl_liq_usd from lv_tt_saldo_trava.
      "SUBTRACT <fs_trava>-prev_vl_liq_usd FROM lv_vlr_trava_aux.

      if lv_vlr_trava_old < 0.
        lv_vlr_trava_old = lv_vlr_trava_old * -1.
      endif.

      " lança o restante na nova OV
      perform f_upd_trava_cambio
        using <fs_trava>-doc_simulacao
              <fs_trava>-vbelv
              <fs_trava>-sequencia
              uv_vbeln "<-- ov nova
              <fs_trava>-data_prevpgto
              <fs_trava>-kurrf
              lv_vlr_trava_new
              <fs_trava>-prev_multa_usd
              <fs_trava>-prev_juros_usd.

      " se o valor ficar zero então pode sair
      if lv_vlr_trava_old is initial. "IF lv_vlr_trava_aux IS INITIAL.
        exit.
      endif.

    endif.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_UPD_TRAVA_CAMBIO
*&---------------------------------------------------------------------*
form f_upd_trava_cambio using uv_doc_simu type zsded003
                              uv_ov_atual type vbeln
                              uv_seq_esto type numc4
                              uv_ov_lcto  type vbeln
                              uv_data_prevpgto type zsde_prevpag
                              uv_taxa     type kurrf
                              uv_prev_pgto_usd "TYPE zsde_prev_pgto_usd
                              "uv_prev_pgto_brl ""TYPE zsde_prev_pgto_brl
                              uv_prev_multa_usd "TYPE zsde_prev_multa_usd
                              uv_prev_juros_usd. "TYPE zsde_prev_juros_usd
  "uv_prev_vl_liq_usd." TYPE zsde_prev_vl_liq_usd.

  data ls_trava_0090 type zsdt0090.

  if uv_seq_esto is not initial.

    select single * from zsdt0090
      into @data(ls_estorno)
      where doc_simulacao = @uv_doc_simu
    and sequencia = @uv_seq_esto.

    if sy-subrc eq 0 and ls_estorno-estorno is initial.

      ls_estorno-estorno = 'X'.
      ls_estorno-usnam_e = sy-uname.
      ls_estorno-data_atual_e = sy-datum.
      ls_estorno-hora_atual_e = sy-uzeit.

      modify zsdt0090 from ls_estorno.

      commit work and wait.

    endif.

  endif.

  ls_trava_0090-sequenciav = uv_seq_esto.
  ls_trava_0090-data_prevpgtov = ls_estorno-data_prevpgto.
  ls_trava_0090-kurrfv = ls_estorno-kurrf.

  select max( sequencia ) from zsdt0090
    into @data(lv_sequencia)
  where doc_simulacao = @uv_doc_simu.

  add 1 to lv_sequencia.

  ls_trava_0090-doc_simulacao = uv_doc_simu.
  ls_trava_0090-sequencia     = lv_sequencia.

  select sequencia from zsdt0090
    into ls_trava_0090-sequenciao
    where doc_simulacao = ls_trava_0090-doc_simulacao
      and vbelv = uv_ov_atual
      and vbeln = gv_ov_desme_atual
      and categoria = 'D'
      and estorno = space. endselect.

  ls_trava_0090-vbelv = uv_ov_lcto.
  ls_trava_0090-spartv = wa_saida-spart.
  ls_trava_0090-matklv = wa_saida-matkl.
  ls_trava_0090-inco1v = wa_saida-inco1.
  ls_trava_0090-werksv = wa_saida-werks.
  ls_trava_0090-chargv = wa_saida-charg.
  ls_trava_0090-kmeinv = wa_saida-kmein.

  " 24.04.2024 - Preenchimento de qtde para ajuste do hedge - RAMON -->
  ls_trava_0090-zmeng = wa_saida-kwmeng.
  ls_trava_0090-zieme = wa_saida-vrkme.
  " 24.04.2024 -<<<<

  ls_trava_0090-netprv = 0.
  ls_trava_0090-categoria = 'C'.

  ls_trava_0090-usnam         = sy-uname.
  ls_trava_0090-data_atual    = sy-datum.
  ls_trava_0090-hora_atual    = sy-uzeit.

  ls_trava_0090-data_prevpgto = uv_data_prevpgto.
  ls_trava_0090-kurrf = uv_taxa.

  ls_trava_0090-prev_pgto_usd = uv_prev_pgto_usd.
  ls_trava_0090-prev_multa_usd = uv_prev_multa_usd.
  ls_trava_0090-prev_juros_usd = uv_prev_juros_usd.

  ls_trava_0090-prev_vl_liq_usd = ls_trava_0090-prev_pgto_usd - ( ls_trava_0090-prev_multa_usd + ls_trava_0090-prev_juros_usd ).
  ls_trava_0090-prev_pgto_brl = ls_trava_0090-prev_pgto_usd * ls_trava_0090-kurrf.

  insert into zsdt0090 values ls_trava_0090.

  perform f_executa_hedge using ls_trava_0090.

  commit work and wait.

  " Hedge lança vinculo
  perform f_hedge_criar_vinculo
    using ls_trava_0090-doc_simulacao
          ls_trava_0090-sequencia
          uv_ov_lcto
          uv_ov_atual.

  "PERFORM f_email_trava_cambio USING ls_trava_0090. " 13.07.2023

endform.
*&---------------------------------------------------------------------*
*&      Form  F_HEDGE_CRIAR_VINCULO
*&---------------------------------------------------------------------*
form f_hedge_criar_vinculo using uv_doc_simu type zsded003
                                 uv_seq type numc4
                                 uv_vbeln_new type vbeln
                                 uv_vbeln_old type vbeln.

  data ls_329 type zsdt0329.
  data lv_tot type zdmbtr.
  data lv_ov_new type vbeln.

  lv_ov_new = uv_vbeln_new.

  if uv_vbeln_new = uv_vbeln_old.

    select single vbeln from zsdt0090
     into lv_ov_new
       where doc_simulacao = wa_saida-doc_simulacao
         and vbelv = uv_vbeln_old
         and categoria = 'D'
    and estorno = space.

  endif.

  select * from zsdt0094
    into table @data(lt_0094)
    where nro_sol_ov = @uv_doc_simu
      and vbeln = @uv_vbeln_old
      and tipo_taxa in ('C','V')
  and estorno = @space.

  sort lt_0094 by data_registro hora_registro ascending.

  "lv_seq = uv_seq.

  loop at lt_0094 assigning field-symbol(<fs_0094>).

    case <fs_0094>-tipo_taxa.
      when 'C'.
        add <fs_0094>-total_proporc to lv_tot.
      when 'V'.
        add <fs_0094>-total_proporc to lv_tot.
      when others.
    endcase.

  endloop.

  if lv_tot > 0.

*    " ANTES DE CRIAR O VINCULO, pega a ultima sequencia
*    SELECT * FROM zsdt0090
*      INTO TABLE @DATA(lt_seq)
*        WHERE doc_simulacao = @uv_doc_simu
*          AND categoria = 'C'
*          AND estorno = @space
*          AND vbelv = @uv_vbeln_old.
*
*    SORT lt_seq BY sequencia DESCENDING.
*
*    " SE NAO FOR, RECUPERA A ATUAL E GRAVA NO CAMPO DA 329 (VAI SER USADO NO ESTORNO SE HOUVER)
*    IF sy-subrc EQ 0.
*      lv_seq = lt_seq[ 1 ]-sequencia.
*    ENDIF.
*    "

    ls_329-vbeln = lv_ov_new.
    ls_329-seqnr = uv_seq.
    ls_329-nro_sol_ov = uv_doc_simu.
    ls_329-vbelv = uv_vbeln_old.

    modify zsdt0329 from ls_329.

    commit work.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_UPDT_LCTO_ZSDT0315
*&---------------------------------------------------------------------*
form f_updt_lcto_zsdt0315 using uv_vbeln type vbeln
                                uv_vbelv type vbelv
                                uv_vlr_ov type netwr_ap
                                uv_vlr_liq type netwr_ap.

  data ls_0315 type zsdt0315.

  perform f_get_sequencial
    using uv_vbeln
 changing ls_0315-seqnr.

  ls_0315-vbeln = uv_vbeln.
  ls_0315-vbelv = uv_vbelv.
  ls_0315-valor_ov = uv_vlr_ov.
  ls_0315-vl_liquidado = uv_vlr_liq.
  ls_0315-waers = 'USD'.
  ls_0315-cancelado = space.
  ls_0315-uname = sy-uname.
  ls_0315-udate = sy-datum.
  ls_0315-uzeit = sy-uzeit.

  modify zsdt0315 from ls_0315.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQUENCIAL
*&---------------------------------------------------------------------*
form f_get_sequencial using p_vbeln type vbeln
                   changing p_seqnr type bdl_seq.

  select max( seqnr ) from zsdt0315
    into p_seqnr
  where vbeln = p_vbeln.

  add 1 to p_seqnr.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CANC_LCTO_ZSDT0315
*&---------------------------------------------------------------------*
form f_canc_lcto_zsdt0315 using uv_vbeln type vbeln.

  select * from zsdt0315
     into table @data(lt_0315)
       where vbeln = @uv_vbeln
  and cancelado = @space.

  check sy-subrc eq 0.

  loop at lt_0315 assigning field-symbol(<fs_0315>).

    <fs_0315>-cancelado = abap_true.

  endloop.

  modify zsdt0315 from table lt_0315.

  commit work and wait.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AUTORIZACAO_TRAVA
*&---------------------------------------------------------------------*
form f_check_autorizacao_trava using uv_bukrs type bukrs
                                     uv_vkbuk type vkbuk
                            changing ct_desab type zsdc082.

  data lv_field type fieldname.
  data lv_value type fieldvalue.

  lv_field = 'ZFLG_TXNEG'.
  lv_value = 'X'.

  call function 'ZSDMF_AUTORIZACAO_ZSDT0087'
    exporting
      iv_bukrs        = uv_bukrs
      iv_vkbur        = uv_vkbuk
      iv_field        = lv_field
      iv_value        = lv_value
    exceptions
      sem_autorizacao = 1
      others          = 2.

  if sy-subrc <> 0.
    append value #( fieldname = 'ZSDS078-TAXA_NEG' ) to ct_desab.
  endif.

  lv_field = 'ZFLG_JRPRE'.
  lv_value = 'X'.

  call function 'ZSDMF_AUTORIZACAO_ZSDT0087'
    exporting
      iv_bukrs        = uv_bukrs
      iv_vkbur        = uv_vkbuk
      iv_field        = lv_field
      iv_value        = lv_value
    exceptions
      sem_autorizacao = 1
      others          = 2.

  if sy-subrc <> 0.

    append value #( fieldname = 'ZSDS078-JUROS_PREV' ) to ct_desab.
    append value #( fieldname = 'ZZSDS078-MULTA_PREV' ) to ct_desab.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_HEDGE_CRIAR_VINCULO
*&---------------------------------------------------------------------*
form f_hedge_consulta_vinculo using uv_vbeln type vbeln
                           changing cv_vbelv type vbelv
                                    cv_tot type zdmbtr.
  data lv_doc type zsded013.
  clear: cv_vbelv, cv_tot.

  select single vbelv nro_sol_ov from zsdt0329
    into (cv_vbelv,lv_doc)
  where vbeln = uv_vbeln.

  check sy-subrc eq 0.

  select * from zsdt0094
    into table @data(lt_0094)
    where nro_sol_ov = @lv_doc
      and vbeln = @cv_vbelv
  and tipo_taxa in ('C','V').

  sort lt_0094 by data_registro hora_registro ascending.

  loop at lt_0094 assigning field-symbol(<fs_0094>).

    case <fs_0094>-tipo_taxa.
      when 'C'.
        add <fs_0094>-total_proporc to cv_tot.
      when 'V'.
        add <fs_0094>-total_proporc to cv_tot.
      when others.
    endcase.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_HEDGE
*&---------------------------------------------------------------------*
form f_executa_hedge using us_0090 type zsdt0090.

  data ls_0090_cambio type zsdt0090.

  " se tiver estornando uma trava então tenho que vender o saldo
  if us_0090-sequenciav is not initial.

    " se a taxa mudou vendo na taxa antiga
    if us_0090-kurrfv <> us_0090-kurrf.

      select single vbelv,prev_pgto_usd,prev_juros_usd,prev_multa_usd
        from zsdt0090
        into @data(ls_estorno)
          where doc_simulacao = @us_0090-doc_simulacao
      and sequencia = @us_0090-sequenciav.

      ls_0090_cambio-doc_simulacao = us_0090-doc_simulacao.
      "ls_0090_cambio-sequencia = us_0090-sequenciav. " <-- vendo na sequencia anterior
      ls_0090_cambio-sequencia = us_0090-sequencia.

      "ls_0090_cambio-seq_trava = us_0090-seq_trava.
      ls_0090_cambio-spartv = us_0090-spartv.
      ls_0090_cambio-matklv = us_0090-matklv.
      ls_0090_cambio-inco1v  = us_0090-inco1v.
      ls_0090_cambio-werksv = us_0090-werksv.
      ls_0090_cambio-categoria = us_0090-categoria.
      ls_0090_cambio-kurrf = us_0090-kurrfv.
      ls_0090_cambio-data_prevpgto = us_0090-data_prevpgtov.

*****      " ------ DADOS DA TRAVA ESTORNADA - foi comentado pq precisa sempre estornar todo o hedge
*****      ls_0090_cambio-vbelv = ls_estorno-vbelv.  """"
*****      ls_0090_cambio-prev_pgto_usd = us_0090-prev_pgto_usd. """"""
*****      ls_0090_cambio-prev_juros_usd  = ls_0090_cambio-prev_juros_usd. """"
*****      ls_0090_cambio-prev_multa_usd = ls_0090_cambio-prev_multa_usd. """""
*****
*****      ls_0090_cambio-prev_vl_liq_usd = ls_0090_cambio-prev_pgto_usd - ( ls_0090_cambio-prev_juros_usd + ls_0090_cambio-prev_multa_usd ).
*****      ls_0090_cambio-prev_pgto_brl = ls_0090_cambio-prev_vl_liq_usd * ls_0090_cambio-kurrf.

      " ------ DADOS DA TRAVA ESTORNADA
      ls_0090_cambio-vbelv = ls_estorno-vbelv.  """"
      ls_0090_cambio-prev_pgto_usd = ls_estorno-prev_pgto_usd. """"""
      ls_0090_cambio-prev_juros_usd  = ls_estorno-prev_juros_usd. """"
      ls_0090_cambio-prev_multa_usd = ls_estorno-prev_multa_usd. """""

      ls_0090_cambio-prev_vl_liq_usd = ls_0090_cambio-prev_pgto_usd - ( ls_0090_cambio-prev_juros_usd + ls_0090_cambio-prev_multa_usd ).
      ls_0090_cambio-prev_pgto_brl = ls_0090_cambio-prev_vl_liq_usd * ls_0090_cambio-kurrf.

      perform f_sap_indicator using 'Executando Hedge Venda...'.

      zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_0090_cambio
                                              i_acao = 'VENDA'
                                              i_tipo = 'VDI'
                                              i_dir  = 'T'
                                              ).
    endif.

  endif.

  " se a taxa mudou compra na taxa nova
  if us_0090-kurrfv <> us_0090-kurrf.

    clear ls_0090_cambio.

    ls_0090_cambio-doc_simulacao = us_0090-doc_simulacao.
    ls_0090_cambio-sequencia = us_0090-sequencia. " <-- compra na sequencia nova
    "ls_0090_cambio-seq_trava = us_0090-seq_trava.
    ls_0090_cambio-spartv = us_0090-spartv.
    ls_0090_cambio-matklv = us_0090-matklv.
    ls_0090_cambio-inco1v  = us_0090-inco1v.
    ls_0090_cambio-werksv = us_0090-werksv.
    ls_0090_cambio-categoria = us_0090-categoria.
    ls_0090_cambio-kurrf = us_0090-kurrf.
    ls_0090_cambio-data_prevpgto = us_0090-data_prevpgto.

    " ------ DADOS DA TRAVA NOVA
    ls_0090_cambio-vbelv = us_0090-vbelv.
    ls_0090_cambio-prev_pgto_usd = us_0090-prev_pgto_usd.
    ls_0090_cambio-prev_juros_usd  = ls_0090_cambio-prev_juros_usd.
    ls_0090_cambio-prev_multa_usd = ls_0090_cambio-prev_multa_usd.

    ls_0090_cambio-prev_vl_liq_usd = ls_0090_cambio-prev_pgto_usd - ( us_0090-prev_juros_usd + us_0090-prev_multa_usd ).
    ls_0090_cambio-prev_pgto_brl = ls_0090_cambio-prev_vl_liq_usd * ls_0090_cambio-kurrf.

    perform f_sap_indicator using 'Executando Hedge Compra...'.

    zcl_webservice_tx_curva=>hedge_insumos( i_0090 = ls_0090_cambio
                                            i_acao = 'PRICE_NEW'
                                            i_tipo = 'VDI'
                                            i_dir  = 'P'
                                            ).
  endif.

  " se executou hedge manda email da trava
  if us_0090-kurrfv <> us_0090-kurrf.
    perform f_email_trava_cambio using us_0090.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_EMAIL_TRAVA_CAMBIO
*&---------------------------------------------------------------------*
form f_email_trava_cambio using us_trava type zsdt0090.

  data lr_docsi type range of zsds0090_email-doc_simulacao.
  data lr_seq type range of zsds0090_email-sequencia.

  append 'IEQ' && us_trava-doc_simulacao to lr_docsi.

  append initial line to lr_seq assigning field-symbol(<fs_seq>).

  <fs_seq>-sign = 'I'.
  <fs_seq>-option = 'EQ'.
  <fs_seq>-low = us_trava-sequencia.

  zcl_util_=>indicator( |Enviando e-mail trava de cambio...| ).

  submit zsdr0157
          with p_exec eq 'X'
          with so_docsi in lr_docsi[]
          with so_seq in lr_seq[]
    and return.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AUTORI_EMPRESA
*&---------------------------------------------------------------------*
form f_check_autori_empresa using p_refresh type c.

  data lv_restrict.

  loop at it_zsdt0040 assigning field-symbol(<fs_0040>).

    data(lv_index) = sy-tabix.

    call function 'ZSDMF_AUTORIZACAO_ZSDT0087'
      exporting
        iv_bukrs        = <fs_0040>-vkorg
        iv_vkbur        = <fs_0040>-vkbur
      exceptions
        sem_autorizacao = 1
        others          = 2.

    if sy-subrc <> 0.
      delete it_zsdt0040 index lv_index.
      lv_restrict = 'X'.
    endif.

  endloop.

  if p_refresh is initial.

    if  lv_restrict = 'X'.
      message 'Existe restrição de acesso p alguma Empresa/Esc.Venda da seleção'
        type 'S' display like 'W'.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_HEDGE_APAGAR_VINCULO
*&---------------------------------------------------------------------*
form f_hedge_apagar_vinculo using uv_vbeln type vbeln.

  delete from zsdt0329 where vbeln = uv_vbeln.

  commit work.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_RECOMPOE_ZSDT0090
*&---------------------------------------------------------------------*
form f_recompoe_zsdt0090 tables tt_0090 structure zsdt0090.

  data(lt_0090) = tt_0090[].
  data lv_seq type numc4.
  data lv_seqv type numc4.

  " elimina as demais deixa só um tipo,
  " fizemos isso para nao ficar dois tipos aqui 'D' e 'C' por exemplo
  delete lt_0090 where categoria ne 'D'.

  check lt_0090[] is not initial.

  " recupero quais estao ativos
  select * from zsdt0090
    into table @data(lt_ativos)
      for all entries in @lt_0090
      where doc_simulacao = @lt_0090-doc_simulacao
  and sequenciao = @lt_0090-sequencia.
  "AND estorno = @space. " faturado vem X a faturar vem space, porem no final vai colocar x

  check sy-subrc eq 0.

  " recupero quais quero recompor
  select * from zsdt0090
    into table @data(lt_reco)
      for all entries in @lt_ativos
      where doc_simulacao = @lt_ativos-doc_simulacao
        and sequencia = @lt_ativos-sequenciav
  and estorno = 'X'.

  loop at lt_reco assigning field-symbol(<fs_reco>).

    " retiro flags de estorno
    clear <fs_reco>-estorno.
    clear <fs_reco>-usnam_e.
    clear <fs_reco>-data_atual_e.
    clear <fs_reco>-hora_atual_e.

    <fs_reco>-usnam = sy-uname.
    <fs_reco>-data_atual = sy-datum.
    <fs_reco>-hora_atual = sy-uzeit.

    if lv_seq is initial.

      " gero nova sequencia
      perform f_get_next_seq
        using <fs_reco>-doc_simulacao
     changing lv_seq.

    else.

      add 1 to lv_seq.

    endif.

    lv_seqv = <fs_reco>-sequencia.
    <fs_reco>-sequencia = lv_seq.

    if <fs_reco>-data_prevpgto < sy-datum.

      " pega o ultimo ativo dessa sequencia
      read table lt_ativos assigning field-symbol(<fs_ativo>)
        with key doc_simulacao = <fs_reco>-doc_simulacao
                 sequenciav = lv_seqv.
      " 21.07.2023 - foi tirado o vbelv, pq a lv_seqv já define a chave da tabela independente da ov,
      " se der erro na data de estorno da 0090, verificar aqui.
      " o vbelv esta chegando diferente do que estorna......
      "vbelv = wa_saida-vbeln. " teve hedge na que esta estornando?

      if sy-subrc eq 0.

        " se existir ve se a taxa foi alterada, que significa que foi movido hedge
        if <fs_ativo>-kurrfv <> <fs_ativo>-kurrf.

          " e se foi movido então, atualiza a data e manda email no final do codigo
          <fs_reco>-data_prevpgtov = <fs_reco>-data_prevpgto.
          <fs_reco>-data_prevpgto = value #( lt_0090[ 1 ]-data_prevpgto default <fs_reco>-data_prevpgto ).

          " 21.07.2023 - guarda taxa antiga
          <fs_reco>-kurrfv = <fs_ativo>-kurrf.

        endif.

      endif.

      " 21.07.2023 -criado para verificar se a taxa mudou, se mudou dispara o email
    else.

      " como vai estornar, então pega a ultima taxa e coloca no campo de taxa antiga
      read table lt_ativos assigning <fs_ativo>
        with key doc_simulacao = <fs_reco>-doc_simulacao
                 sequenciav = lv_seqv.

      if sy-subrc eq 0.

        " 21.07.2023 - guarda taxa antiga
        <fs_reco>-kurrfv = <fs_ativo>-kurrf.

      endif.

    endif.

  endloop.

  check lt_reco is not initial.

  modify zsdt0090 from table lt_reco.

  commit work and wait.

  loop at lt_reco assigning <fs_reco>. "WHERE data_prevpgtov IS NOT INITIAL. 21.07.2023

    " se as taxas mudaram, manda email, significa que movimentou o hedge
    check <fs_reco>-kurrfv <> <fs_reco>-kurrf.

    perform f_email_trava_cambio using <fs_reco>.

  endloop.

  loop at lt_ativos assigning field-symbol(<fs_ativos>).

    <fs_ativos>-estorno = abap_true.
    <fs_ativos>-usnam_e = sy-uname.
    <fs_ativos>-data_atual_e = sy-datum.
    <fs_ativos>-hora_atual_e = sy-uzeit.


  endloop.

  modify zsdt0090 from table lt_ativos.

  commit work and wait.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CANC_LCTO_ZSDT0315
*&---------------------------------------------------------------------*
form f_reco_lcto_zsdt0315 using uv_vbeln type vbeln.

  select * from zsdt0315
     into table @data(lt_0315)
  where vbelv = @uv_vbeln. " 297.898,79

  check sy-subrc eq 0.

  sort lt_0315 by seqnr descending.

  do 2 times.

    read table lt_0315 assigning field-symbol(<fs_0315>) index sy-index.

    check sy-subrc eq 0.

    if <fs_0315>-cancelado is initial.
      delete from zsdt0315 where vbeln = <fs_0315>-vbeln and seqnr = <fs_0315>-seqnr.
    else.
      update zsdt0315 set cancelado = space
        where vbeln = <fs_0315>-vbeln and seqnr = <fs_0315>-seqnr.
    endif.

  enddo.

  commit work and wait.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AUTORIZACAO
*&---------------------------------------------------------------------*
form f_check_autorizacao changing cv_erro type c.

  data(lv_bukrs) = value #( it_saida[ check = 'X' ]-vkorg default '' ).
  data(lv_vbuk) = value #( it_saida[ check = 'X' ]-vkbur default '' ).
  data(lv_ztpsim) = value #( it_saida[ check = 'X' ]-tpsim default '' ).
  data(lv_zmeiopgto) = value #( it_saida[ check = 'X' ]-meio_pago default '' ).

  data lv_field type fieldname.
  data lv_value type fieldvalue.

  clear cv_erro.

  lv_field = 'ZTPSIM'.
  lv_value = lv_ztpsim.

  call function 'ZSDMF_AUTORIZACAO_ZSDT0087'
    exporting
      iv_bukrs        = lv_bukrs
      iv_vkbur        = lv_vbuk
      iv_field        = lv_field
      iv_value        = lv_value
    exceptions
      sem_autorizacao = 1
      others          = 2.

  if sy-subrc <> 0.

    cv_erro = 'X'.
    perform f_mensagem_sistema.
    exit.

  endif.

  lv_field = 'ZMEIOPGTO'.
  lv_value = lv_zmeiopgto.

  call function 'ZSDMF_AUTORIZACAO_ZSDT0087'
    exporting
      iv_bukrs        = lv_bukrs
      iv_vkbur        = lv_vbuk
      iv_field        = lv_field
      iv_value        = lv_value
    exceptions
      sem_autorizacao = 1
      others          = 2.

  if sy-subrc <> 0.

    cv_erro = 'X'.
    perform f_mensagem_sistema.
    exit.

  endif.

  lv_field = 'ZSD_87_BUT'.
  lv_value = ''.

  case sy-ucomm.
    when 'TRANSF'.
      lv_value = '01'.
    when 'GERAR'.
      lv_value = '02'.
    when 'ZTRI'.
      lv_value = '03'.
    when 'ESTORNAR'.
      lv_value = '04'.
    when 'ALTERAR'.
      lv_value = '05'.
    when 'CANCELAR'.
      lv_value = '06'.
    when 'DESMEMBRAR'.
      lv_value = '07'.
    when 'DESBLOQ'.
      lv_value = '08'.
    when 'TROCA'.
      lv_value = '09'.
    when 'ENCERRAR'.
      lv_value = '10'.
    when 'PRICE'.
      lv_value = '11'.
    when 'PRICE_NEW'.
      lv_value = '12'.
    when 'REDIST'.
      lv_value = '13'.
    when 'MDF_VENC'.
      lv_value = '14'.
    when 'ALT_GERAIS'.
      lv_value = '15'.
    when 'DEP_LOTE'.
      lv_value = '16'.
    when 'ALT_DT_ENT'.
      lv_value = '17'.
    when 'AGRUP_OV'.
      lv_value = '18'.
  endcase.

  check lv_value is not initial.

  call function 'ZSDMF_AUTORIZACAO_ZSDT0087'
    exporting
      iv_bukrs        = lv_bukrs
      iv_vkbur        = lv_vbuk
      iv_field        = lv_field
      iv_value        = lv_value
    exceptions
      sem_autorizacao = 1
      others          = 2.

  if sy-subrc <> 0.

    cv_erro = 'X'.

    select single ddtext from dd07t
       into @data(lv_texto)
        where domname = 'ZDD_AUT_0087_BUTTON'
          and ddlanguage = @sy-langu
          and as4local = 'A'
    and domvalue_l = @lv_value.

    message id 'DS' type 'S' number '016'
      with 'Sem autorização' lv_texto
        display like 'E'.

    exit.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form f_check_autorizacao_gerais
*&---------------------------------------------------------------------*
form f_check_autorizacao_gerais .


  data lv_inco_x type c.
  data lv_zpesa_x type c.
  data lv_route_x type c.
  data lv_parc_x type c.

  call function 'ZSDMF_AUTORIZ_DADOS_G_ZSDT0087'
    exporting
      iv_bukrs = wa_saida-vkorg
      iv_vkbur = wa_saida-vkbur
    importing
      ev_01_x  = lv_inco_x
      ev_02_x  = lv_zpesa_x
      ev_03_x  = lv_route_x
      ev_04_x  = lv_parc_x.

  loop at screen.

    if screen-name eq 'WA_ALT_GERAIS-INCO1' or screen-name eq 'INCO1_'.

      if lv_inco_x = 'X'.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.

    elseif screen-name eq 'WA_ALT_GERAIS-ZPESAGEM'or screen-name eq 'ZPESAGEM_'.

      if lv_zpesa_x = 'X'.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.

    elseif screen-name eq 'WA_ALT_GERAIS-ROUTE'or screen-name eq 'ROUTE_'.

      if lv_route_x = 'X'.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.

    elseif screen-name eq 'WA_ALT_GERAIS-COD_PARC'or screen-name eq 'COD_PARC_'.

      if lv_parc_x = 'X'.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.

    endif.

    modify screen.

  endloop.

endform.
" 10.09.2024 - RAMON - 97513 --<
*&---------------------------------------------------------------------*
*& Form insere_zsdt0116
*&---------------------------------------------------------------------*
form insere_zsdt0116 using p_vbeln p_vbelv p_posnn.

  call method zcl_manutencao_insumos=>send_aprovadores_embarque
    exporting
      i_vbeln  = p_vbeln
      i_posnn  = p_posnn
      i_vbelv  = p_vbelv
    importing
      r_return = data(e_return).

  append lines of e_return to tl_return[].

  exit.

  data: seq type numc10.
  select single *
    from zsdt0116
    into @data(wa_zsdt0116)
    where vbeln   eq @p_vbelv
      and status  eq @abap_false
  and status_workflow in ( ' ', 'A' ).

  if sy-subrc is initial.

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr = '01'
        object      = 'ZSEQ_0116_'
      importing
        number      = seq.
*
    if seq is initial.
      rollback work.
      message 'Objeto numeração ZSEQ_0116_ não configurado!' type 'S'.
      return.
    endif.

    data(wa_0116) =
    value zsdt0116(
                    seq               =  seq
                    vbeln             =  p_vbeln
                    posnr             =  p_posnn
                    user_apv          =  wa_zsdt0116-user_apv
                    dt_apv            =  wa_zsdt0116-dt_apv
                    hr_apv            =  wa_zsdt0116-hr_apv
                    user_solicitante  = sy-uname
                    data_solicitante  = sy-datum
                    hora_solicitante  = sy-uzeit
                    aprov_por_ref     = abap_true
                    seq_ref           = wa_zsdt0116-seq
                    vbeln_ref         = wa_zsdt0116-vbeln
                    posnr_ref         = wa_zsdt0116-posnr
                  ).

    insert into zsdt0116 values wa_0116.
    if sy-subrc is initial.
      commit work.
      append value #(
                      type = 'W'
                      message = |OV. { p_vbelv } foi Incluida na tabela ZSDT0116.|
                    ) to tl_return[].
    endif.
  endif.
endform.


form f_conf_300.

*        "FF #145609 - inicio
  if p_extcal is not initial.
    wg_desmem-kunnr  = lv_kunnr.
  endif.
*        "FF #145609 -  fim

  if wg_desmem-zdes eq abap_true.
    if wg_desmem-kunnr is initial.
      message 'Informe o cliente' type 'I'.
      exit.
    endif.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_desmem-kunnr
      importing
        output = wg_desmem-kunnr.

    select single name1
      from kna1
      into v_name1
    where kunnr = wg_desmem-kunnr.

    if sy-subrc ne 0.
      message 'Cliente não cadastrado' type 'I'.
      clear: wg_desmem-kunnr, wg_desmem-name1.
      exit.
    endif.

  endif.

  free: it_ov, it_popup2_aux.

*        "FF #145609 - inicio
  if p_extcal is not initial.
    read table tg_ov2 assigning field-symbol(<fs_ov2>) index 1.
    if <fs_ov2> is assigned.
      <fs_ov2>-qt_tran = lv_qte_sol.
    endif.

    p_ucomm = 'DESMEMBRAR'.

  endif.
*        "FF #145609 -  fim

  loop at tg_ov2.
    if tg_ov2-qt_tran eq 0.
      message 'Não foi informada a quantidade.' type 'I'.
      continue.
    endif.

    read table it_sai_aux with key vbeln = tg_ov2-vbeln posnr = tg_ov2-posnr.

    wa_ov-vbeln = tg_ov2-vbeln.
    wa_ov-posnr = tg_ov2-posnr.
    wa_ov-matnr = tg_ov2-matnr.
    wa_ov-zmeng = tg_ov2-qt_tran.

    if wg_desmem-zdes ne abap_true.
      it_sai_aux-qt_tran = tg_ov2-qt_tran - it_sai_aux-kwmeng.
    else.
      it_sai_aux-qt_tran = tg_ov2-qt_tran.
    endif.

    append wa_ov to it_ov.
    modify it_sai_aux index sy-tabix transporting qt_tran.

  endloop.

  check it_ov is not initial.

  free tl_return[].

  it_popup2_aux[] = it_sai_aux[].

  if wg_desmem-zdes eq abap_true.

    loop at it_ov into data(ls_ov).

      free: r_return, ls_manutencao.

      ls_manutencao-desmembramento =
        value #(
                  vbeln = ls_ov-vbeln
                  posnr = ls_ov-posnr
                  kunnr = wg_desmem-kunnr
                  qtd_recebida = ls_ov-zmeng
               ).

      call method zcl_manutencao_insumos=>set_dados_desmembramento
        exporting
          i_manutencao = ls_manutencao.

      call method zcl_manutencao_insumos=>check_dados_desmembramento
        importing
          r_return = r_return.

      append lines of r_return to lt_return.

      clear: ls_manutencao, r_return.

    endloop.

    call method zcl_manutencao_insumos=>exibe_mensagens
      exporting
        i_return = lt_return.

    check not line_exists( lt_return[ type = '#' ] ).

    loop at it_ov into ls_ov.

      free: r_return, ls_manutencao.

      ls_manutencao-desmembramento =
        value #(
                  vbeln = ls_ov-vbeln
                  posnr = ls_ov-posnr
                  kunnr = wg_desmem-kunnr
                  qtd_recebida = ls_ov-zmeng
               ).

      call method zcl_manutencao_insumos=>set_dados_desmembramento
        exporting
          i_manutencao = ls_manutencao.

      call method zcl_manutencao_insumos=>desmembramento_massa
        exporting
          i_check  = abap_false
        importing
          r_return = r_return.

      append lines of r_return to tl_return[].

    endloop.

    perform f_exibe_bapi.

*    ZCL_UTIL_=>MODIFY_ORDEM(
*      EXPORTING
*        I_AUART  = WA_SAIDA-AUART
*        I_ACAO   = P_UCOMM
*        I_KUNNR  = WG_DESMEM-KUNNR
*        T_OV     = IT_OV
*        I_EXTCAL = P_EXTCAL
*      CHANGING
*        I_VBELN  = IT_POPUP2_AUX-VBELN ).
*
*    DATA(VBELN) = IT_POPUP2_AUX-VBELN.
*
*    LOOP AT IT_POPUP2_AUX.
*      IT_POPUP2_AUX-VBELN = VBELN.
*      MODIFY IT_POPUP2_AUX INDEX SY-TABIX TRANSPORTING VBELN.
*    ENDLOOP.
*
*    LOOP AT IT_POPUP2_AUX.
*
*      CLEAR: P_0090, WA_0090.
*      PERFORM INSERT_ZSDT0090 USING 'D'
*                                    IT_POPUP2_AUX-DOC_SIMULACAO
*                                    IT_POPUP2_AUX-VBELN
*                                    IT_POPUP2_AUX-VBELN2
*                                    IT_POPUP2_AUX-POSNR
*                              CHANGING P_0090.
**         // Inserir depois do Disparo da 90
*      ZCL_UTIL_=>DISPARO_0116( I_VBELN = P_0090-VBELN I_POSNR = P_0090-POSNN ).
*    ENDLOOP.

  else.

    data: t_calc type table of ty_calc,
          tt_ov  type table of zsds015.

    free tl_return.

*          "FF #145609 - inicio
    if p_extcal is not initial. "Se este programa foi chamado pela ZSDT0081... lv_0081  = X.
      data(lv_0081) = abap_true.
    endif.
*          "FF #145609 - fim.

    call function 'ZSDMF001_ATUALI_OV_SIM'
      exporting
        i_soma    = 'A'
        i_acao    = p_ucomm
        i_0081    = lv_0081 "FF #145609
      tables
        it_ov     = it_ov
        te_return = tl_return.

    loop at it_ov assigning field-symbol(<wa_>).
      zcl_util_=>processa_desc( i_vbeln = <wa_>-vbeln i_matnr = <wa_>-matnr ).
    endloop.

    read table it_popup2_aux index 1.
    clear: p_0090, wa_0090.

    p_0090 = zcl_util_=>insert_90old(
      simulador = it_popup2_aux-doc_simulacao
      ordem     = it_popup2_aux-vbeln
      material  = it_popup2_aux-matnr
      qtd       = it_popup2_aux-qt_tran
      categoria = 'A'
    ).

*          PERFORM INSERT_ZSDT0090 USING 'A'
*                            IT_POPUP2_AUX-DOC_SIMULACAO
*                            IT_POPUP2_AUX-VBELN
*                            IT_POPUP2_AUX-VBELN2
*                            IT_POPUP2_AUX-POSNR
*                            CHANGING P_0090.

    perform hedge using 'A'.
    perform f_exibe_bapi.

*     "// Processa 40
    zcl_util_=>loop_40(
      simulador = it_popup2_aux-doc_simulacao
      i_vbeln   = it_popup2_aux-vbeln
      i_matnr   = it_popup2_aux-matnr
      qtd       = 10
    ).

  endif.

  perform refresh.
  leave to screen 0.


endform.
*&---------------------------------------------------------------------*
*& Module STATUS_0310 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0310 output.
  set pf-status 'Z310'.
  set titlebar 'Z310'.
endmodule.
*&---------------------------------------------------------------------*
*& Module CRIA_OBJETOS_0310 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module cria_objetos_0310 output.

  data: lt_fieldcatalog type lvc_t_fcat.

*  DATA(LS_LAYOUT) =
*  VALUE LVC_S_LAYO(
*            ZEBRA      = ABAP_TRUE
*            NO_ROWMARK = ABAP_TRUE
*            INFO_FNAME = 'COLOR'
*            GRID_TITLE = ABAP_FALSE
**            NO_TOOLBAR = ABAP_TRUE
*          ).

  if obg_desmemb_mas is initial.

    create object obg_desmemb_mas
      exporting
        container_name = 'DESMEMBRAMENTO'.

    create object grid_desmemb_mas
      exporting
        i_parent = obg_desmemb_mas.

    perform f_fcat_desmembramento changing lt_fieldcatalog.

    data(lt_function) =
    value ui_functions(
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW )
        ( cl_gui_alv_grid=>mc_fc_loc_move_row )
        ( cl_gui_alv_grid=>mc_fc_loc_paste )
        ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
        ( cl_gui_alv_grid=>mc_fc_loc_undo )
        ( cl_gui_alv_grid=>mc_fc_loc_append_row )
        ( cl_gui_alv_grid=>mc_fc_loc_copy )
        ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
        ( cl_gui_alv_grid=>mc_fc_loc_cut )
    ).

    call method grid_desmemb_mas->set_table_for_first_display
      exporting
*       IS_LAYOUT            = LS_LAYOUT
        it_toolbar_excluding = lt_function
      changing
        it_fieldcatalog      = lt_fieldcatalog
        it_outtab            = it_desmem_massa_item.

    call method grid_desmemb_mas->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method grid_desmemb_mas->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_dt_chd_f_des_mas for grid_desmemb_mas,
                 lcl_event_handler=>on_dt_chd_des_mas   for grid_desmemb_mas,
                 lcl_event_handler=>on_u_c_des_mas      for grid_desmemb_mas.
  else.

    call method grid_desmemb_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0310  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0310 input.

  case sy-ucomm.
    when 'SOK310'. "Busca
      perform f_busca_0310.
    when 'YOK310'. "Confirma
      perform f_executa_0310.
    when 'NOK310'. "Cancela
      free it_desmem_massa_item.
      leave to screen 0.
    when others.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*& Form F_FCAT_DESMEMBRAMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FIELDCATALOG
*&---------------------------------------------------------------------*
form f_fcat_desmembramento changing pt_fieldcatalog.

  free t_fieldcatalog.

  perform montar_estrutura using:
  1 ''     ''       'IT_DESMEM_MASSA_ITEM' 'VBELN' 'Ordem'         '10' ''  '' '' '' '',
  2 ''     ''       'IT_DESMEM_MASSA_ITEM' 'POSNR' 'Item'          '06' ''  '' '' '' '',
  3 ''     ''       'IT_DESMEM_MASSA_ITEM' 'KUNNR' 'Cliente'       '10' 'X' '' '' '' '',
  4 ''     ''       'IT_DESMEM_MASSA_ITEM' 'NAME1' 'Nome'          '20' ''  '' '' '' '',
  5 ''     ''       'IT_DESMEM_MASSA_ITEM' 'ZMENG' 'Quantidade OV' '15' ''  '' '' '' '',
  6 ''     ''       'IT_DESMEM_MASSA_ITEM' 'ZIEME' 'Unidade'       '04' ''  '' '' '' '',
  7 'VBAP' 'KWMENG' 'IT_DESMEM_MASSA_ITEM' 'QTD'   'Quantidade'    '15' 'X' '' '' '' ''.

  pt_fieldcatalog = t_fieldcatalog.

endform.
*&---------------------------------------------------------------------*
*& Form F_DESMEMBRAMENTO_MASSA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_desmembramento_massa.

  data(check) = it_saida[].
  delete check where check ne abap_true.

  if lines( check ) ne 1.
    message 'Selecione somente uma Linha!' type 'I'.
    exit.
  endif.

  read table it_saida into gs_desmembramento with key check = abap_true.
  check sy-subrc is initial.

  ls_desmem_massa_header =
  value #(
            matnr = |{ gs_desmembramento-matnr alpha = out }|
            maktx = gs_desmembramento-arktx
            saldo = gs_desmembramento-sd_disp
         ).

  call screen 0310 starting at 050 3
                   ending   at 160 18.

endform.
*&---------------------------------------------------------------------*
*& Module STATUS_0420 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0420 output.
  set pf-status 'Z420'.
  set titlebar 'Z420'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0420  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0420 input.

  case sy-ucomm.
    when 'SOK420'. "Busca
      perform f_busca_0420.
    when 'YOK420'. "Confirma
      perform f_executa_0420.
    when 'NOK420'. "Cancela
      leave to screen 0.
    when others.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*& Form F_TROCA_MASSA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_troca_massa .

  data: n1 type n length 3 value '040',
        n2 type n length 3 value '005',
        n3 type n length 3 value '155',
        n4 type n length 3 value '015'.

  data: lv_qtd_check  type zsded054.

  free: gt_troca_material_massa_item, gs_troca_material_massa_header.

  loop at it_saida into gs_troca_massa where check is not initial.

    append initial line to gt_troca_material_massa_item assigning field-symbol(<fs_troca_material_massa_item>).

    <fs_troca_material_massa_item>-kunnr = gs_troca_massa-kunnr.

    <fs_troca_material_massa_item>-vbeln = gs_troca_massa-vbeln.
    <fs_troca_material_massa_item>-posnr = gs_troca_massa-posnr.
    <fs_troca_material_massa_item>-matnr = gs_troca_massa-matnr.
    <fs_troca_material_massa_item>-arktx = gs_troca_massa-arktx.
    <fs_troca_material_massa_item>-kwmeng = gs_troca_massa-kwmeng.
    <fs_troca_material_massa_item>-vrkme = gs_troca_massa-vrkme.
    <fs_troca_material_massa_item>-netwr = gs_troca_massa-netwr.
    <fs_troca_material_massa_item>-sd_disp = gs_troca_massa-sd_disp.
*    <FS_TROCA_MATERIAL_MASSA_ITEM>-QTD_REMOVIDA = ''.

    <fs_troca_material_massa_item>-lote = ''.
    <fs_troca_material_massa_item>-lgort = ''.
    <fs_troca_material_massa_item>-vlr_venda = ''.
    <fs_troca_material_massa_item>-vlr_total = <fs_troca_material_massa_item>-qtd_removida.
    <fs_troca_material_massa_item>-qtd_recebida = ''.

    <fs_troca_material_massa_item>-itinerario = '@AW@'.
*    <FS_TROCA_MATERIAL_MASSA_ITEM>-COLOR = ''.
*    <FS_TROCA_MATERIAL_MASSA_ITEM>-STYLE = ''.

    <fs_troca_material_massa_item>-cellcolor =
    value #(
            ( fname = 'QTD_REMOVIDA' color-col = '6' )
            ( fname = 'QTD_RECEBIDA' color-col = '5' )
           ).

    call method zcl_manutencao_insumos=>verificar_itinerario
      exporting
        i_pc    = conv lifnr( gs_troca_material_massa_header-werks )
        i_lr    = <fs_troca_material_massa_item>-kunnr
        i_check = abap_true
      importing
        is_ok   = data(is_ok).

    if is_ok is not initial.
      <fs_troca_material_massa_item>-itinerario = '@2K@'.
    endif.

  endloop.

*  LOOP AT IT_TROCA_NEW ASSIGNING <TROCA> WHERE ID EQ W_OV-ID.
*    DATA(WPRECO) = ZCL_UTIL_=>GET_KBETR( I_VBELN = W_OV-VBELN I_POSNR = W_OV-POSNR ).
*    <TROCA>-VLR_VENDA = ''.
*    <TROCA>-QTD = ''.
*    TRY .
*        <TROCA>-VLR_TOTAL = WPRECO-KBETR * COND RFMNG( WHEN WPRECO-KMEIN EQ 'TO' THEN W_OV-QT_TRAN / 1000 ELSE W_OV-QT_TRAN ).
*      CATCH CX_SY_ZERODIVIDE.
*    ENDTRY.
*    IF W_OV-SD_DISP EQ W_OV-QT_TRAN AND W_OV-KWMENG EQ W_OV-QT_TRAN.
*      <TROCA>-VLR_TOTAL = W_OV-NETWR.
*    ENDIF.
*    <TROCA>-ITINERARIO = '@AW@'.
*  ENDLOOP.
*
*ENDLOOP.

  data(line) = lines( gt_troca_material_massa_item ).
  if line >= 5.
    line -= 5.
  else.
    line = 0.
  endif.

  add line to n4.

  if n4 > 035.
    n4 = '035'.
  endif.

  call screen 0420 starting at n1 n2
                   ending   at n3 n4.

endform.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_MAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_mat input.

  select *
    from zi_material_preco_vh
    into table @data(lt_material_preco)
    where waerk   eq @gs_troca_massa-waerk
      and safra   eq @gs_troca_massa-safra
      and cultura eq @gs_troca_massa-cultura.

*  DELETE LT_MATERIAL_PRECO WHERE WERKS EQ GS_TROCA_MASSA-WERKS
*                             AND MATNR EQ GS_TROCA_MASSA-MATNR.

  data: lt_set        type table of rgsb4,
        lt_return_tab type table of ddshretval,
        lt_dselc      type table of dselc.

  lt_dselc = value #(
   ( fldname = 'F0001'  dyfldname = 'MATNR' )
   ( fldname = 'F0002'  dyfldname = 'WAERK' )
   ( fldname = 'F0003'  dyfldname = 'INCO1' )
   ( fldname = 'F0004'  dyfldname = 'SAFRA' )
   ( fldname = 'F0005'  dyfldname = 'CULTURA' )
   ( fldname = 'F0006'  dyfldname = 'WERKS' )
   ( fldname = 'F0007'  dyfldname = 'MAKTG' )
   ( fldname = 'F0008'  dyfldname = 'MEINS' )
   ( fldname = 'F0009'  dyfldname = 'VLR_VENDA' )
  ).

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'MATNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GS_TROCA_MATERIAL_MASSA_HEADER-MATNR'
      value_org       = 'S'
      multiple_choice = abap_true
    tables
      value_tab       = lt_material_preco
      return_tab      = lt_return_tab
      dynpfld_mapping = lt_dselc.

  loop at lt_return_tab into data(ls_return_tab).

    case ls_return_tab-retfield.
      when 'MATNR'.
        gs_troca_material_massa_header-matnr     = ls_return_tab-fieldval.
      when 'MAKTG'.
        gs_troca_material_massa_header-maktx     = ls_return_tab-fieldval.
      when 'WAERK'.
        gs_troca_material_massa_header-waerk     = ls_return_tab-fieldval.
      when 'INCO1'.
        gs_troca_material_massa_header-inco1     = ls_return_tab-fieldval.
      when 'SAFRA'.
        gs_troca_material_massa_header-safra     = ls_return_tab-fieldval.
      when 'CULTURA'.
        gs_troca_material_massa_header-cultura   = ls_return_tab-fieldval.
      when 'WERKS'.
        gs_troca_material_massa_header-werks     = ls_return_tab-fieldval.
      when 'MEINS'.
        gs_troca_material_massa_header-meins     = ls_return_tab-fieldval.
      when 'VLR_VENDA'.
        replace '.' in ls_return_tab-fieldval with ''.
        replace ',' in ls_return_tab-fieldval with '.'.
        gs_troca_material_massa_header-vlr_venda = ls_return_tab-fieldval.
      when others.
    endcase.

  endloop.

endmodule.
*&---------------------------------------------------------------------*
*& Module CRIA_OBJETOS_0420 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module cria_objetos_0420 output.

  free lt_fieldcatalog.

  data(ls_layout) =
  value lvc_s_layo(
            zebra      = abap_true
            no_rowmark = abap_true
            info_fname = 'COLOR'
            ctab_fname = 'CELLCOLOR'
            grid_title = abap_false
*            NO_TOOLBAR = ABAP_TRUE
          ).

  if obg_troca_mas is initial.

    create object obg_troca_mas
      exporting
        container_name = 'TROCA_MASSA'.

    create object grid_troca_mas
      exporting
        i_parent = obg_troca_mas.

    perform f_fcat_troca_massa changing lt_fieldcatalog.
*
    lt_function =
    value ui_functions(
**        ( CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW )
**        ( CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_COPY )
*        ( CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW )
        ( cl_gui_alv_grid=>mc_fc_excl_all )
    ).

    call method grid_troca_mas->set_table_for_first_display
      exporting
        is_layout            = ls_layout
        it_toolbar_excluding = lt_function
      changing
        it_fieldcatalog      = lt_fieldcatalog
        it_outtab            = gt_troca_material_massa_item.

    call method grid_troca_mas->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method grid_troca_mas->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    set handler: lcl_event_handler=>on_dt_chd_f_tro_mas for grid_troca_mas,
                 lcl_event_handler=>on_dt_chd_tro_mas   for grid_troca_mas,
                 lcl_event_handler=>on_u_c_tro_mas      for grid_troca_mas,
                 lcl_event_handler=>on_btclick_tro_mas  for grid_troca_mas.
  else.

    call method grid_troca_mas->refresh_table_display
      exporting
        is_stable = ls_stable.

  endif.

endmodule.
*&---------------------------------------------------------------------*
*& Form F_FCAT_TROCA_MASSA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FIELDCATALOG
*&---------------------------------------------------------------------*
form f_fcat_troca_massa  changing pt_fieldcatalog.

  free t_fieldcatalog.

  perform montar_estrutura using:
  01 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'VBELN'        'Ordem'               '08' ''  '' '' 'X' '',
  02 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'POSNR'        'Item'                '03' ''  '' '' 'X' '',
  03 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'MATNR'        'Material'            '08' ''  '' '' 'X' '',
  04 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'ARKTX'        'Descrição'           '10' ''  '' '' '' '',
*  05 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'KWMENG'       'Qtd. Prevista'       '10' ''  '' '' '' '',
  06 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'VRKME'        'UM'                  '04' ''  '' '' '' '',
*  07 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'NETWR'        'Valor Total'         '10' ''  '' '' '' '',
  08 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'SD_DISP'      'Saldo Disponivel'    '10' ''  '' '' '' '',
  09 'VBAP' 'KWMENG' 'GT_TROCA_MATERIAL_MASSA_ITEM' 'QTD_REMOVIDA' 'Qtd Removida'        '10' 'X' '' '' '' '',
*  10 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'LOTE'         'Lote'                '04' 'X' '' '' '' '',
*  11 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'LGORT'        'Deposito'            '04' 'X' '' '' '' '',
  12 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'VLR_VENDA'    'Pr. Troca'           '05' '' '' '' '' '',
  13 'VBAP' 'NETWR'  'GT_TROCA_MATERIAL_MASSA_ITEM' 'VLR_TOTAL'    'Valor Total'         '10' '' '' '' '' '',
  14 'VBAP' 'KWMENG' 'GT_TROCA_MATERIAL_MASSA_ITEM' 'QTD_RECEBIDA' 'Qtd Recebida'        '10' 'X' '' '' '' '',
  15 ''     ''       'GT_TROCA_MATERIAL_MASSA_ITEM' 'ITINERARIO'   'Itinerario'          '04' '' '' '' '' ''.

  pt_fieldcatalog = t_fieldcatalog.


endform.
*&---------------------------------------------------------------------*
*& Form F_BUSCA_0420
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_busca_0420 .

  loop at gt_troca_material_massa_item assigning field-symbol(<fs_troca_material_massa_item>).

    call method zcl_manutencao_insumos=>verificar_itinerario
      exporting
        i_pc    = conv lifnr( gs_troca_material_massa_header-werks )
        i_lr    = <fs_troca_material_massa_item>-kunnr
        i_check = abap_true
      importing
        is_ok   = data(is_ok).

    if is_ok is not initial.
      <fs_troca_material_massa_item>-itinerario = '@2K@'.
    else.
      <fs_troca_material_massa_item>-itinerario = '@AW@'.
    endif.

  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form F_EXECUTA_0420
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_executa_0420 .

  data: lt_return type  bapiret2_t.

  loop at it_saida into gs_troca_massa where check is not initial.

    read table gt_troca_material_massa_item
    into data(ls_troca_material_massa_item)
    with key vbeln = gs_troca_massa-vbeln
             posnr = gs_troca_massa-posnr.

    if sy-subrc is not initial.
      continue.
    endif.

    check ls_troca_material_massa_item-qtd_removida is not initial. "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP

    ls_manutencao = value #(
    saldo_ov = gs_troca_massa-sd_disp
    troca = value #(
                      doc_simulacao = gs_troca_massa-doc_simulacao
                      cultura       = gs_troca_massa-cultura
                      safra         = gs_troca_massa-safra
                      netwr         = gs_troca_massa-netwr
                      troca_massa   = abap_true "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP
    ordem_old = value #(
                          vbeln        = ls_troca_material_massa_item-vbeln
                          posnr        = ls_troca_material_massa_item-posnr
                          matnr        = ls_troca_material_massa_item-matnr
                          qtd_removida = ls_troca_material_massa_item-qtd_removida
                          auart        = gs_troca_massa-auart
                          werks        = gs_troca_massa-werks
                          matkl        = gs_troca_massa-matkl
                          inco1        = gs_troca_massa-inco1
                          kunnr        = ls_troca_material_massa_item-kunnr
                          tpsim        = gs_troca_massa-tpsim
                          kwmeng       = ls_troca_material_massa_item-kwmeng
                       )
    ordem_new = value #(
                          vbeln        = ''
                          posnr        = ''
                          matnr        = gs_troca_material_massa_header-matnr
*                        MATKL        = GS_TROCA_MATERIAL_MASSA_HEADER-MATKL
                          inco1        = gs_troca_material_massa_header-inco1
                          werks        = gs_troca_material_massa_header-werks
                          lote         = gs_troca_material_massa_header-lote
                          lgort        = gs_troca_material_massa_header-lgort
                          qtd_recebida = ls_troca_material_massa_item-qtd_recebida
                          meins        = gs_troca_material_massa_header-meins
                          vlr_venda    = ls_troca_material_massa_item-vlr_venda
*                        SPART        = GS_TROCA_MATERIAL_MASSA_HEADER-SPART
*                        MTART        = GS_TROCA_MATERIAL_MASSA_HEADER-MTART
                       )
    ) ).

    call method zcl_manutencao_insumos=>set_dados_troca
      exporting
        i_manutencao = ls_manutencao.

    call method zcl_manutencao_insumos=>check_dados_troca
      importing
        r_return = data(e_return).

    "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP --->>
    loop at e_return assigning field-symbol(<fs_return>).
      <fs_return>-message = |OV: { ls_troca_material_massa_item-vbeln } -> { <fs_return>-message } |.
    endloop.
    "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP <<---

    append lines of e_return to lt_return.

    clear ls_manutencao.

  endloop.

  call method zcl_manutencao_insumos=>exibe_mensagens
    exporting
      i_return = lt_return.

  check lt_return is initial.

  loop at it_saida into gs_troca_massa where check is not initial.

    read table gt_troca_material_massa_item
    into ls_troca_material_massa_item
    with key vbeln = gs_troca_massa-vbeln
             posnr = gs_troca_massa-posnr.

    if sy-subrc is not initial.
      continue.
    endif.

    check ls_troca_material_massa_item-qtd_removida is not initial. "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP

    clear ls_manutencao.

    ls_manutencao-troca =  value #(
    doc_simulacao = gs_troca_massa-doc_simulacao
    cultura       = gs_troca_massa-cultura
    safra         = gs_troca_massa-safra
    netwr         = gs_troca_massa-netwr
    ordem_old = value #(
                          vbeln        = ls_troca_material_massa_item-vbeln
                          posnr        = ls_troca_material_massa_item-posnr
                          matnr        = ls_troca_material_massa_item-matnr
                          qtd_removida = ls_troca_material_massa_item-qtd_removida
                          auart        = gs_troca_massa-auart
                          werks        = gs_troca_massa-werks
                          matkl        = gs_troca_massa-matkl
                          inco1        = gs_troca_massa-inco1
                          kunnr        = ls_troca_material_massa_item-kunnr
                          tpsim        = gs_troca_massa-tpsim
                          kwmeng       = ls_troca_material_massa_item-kwmeng
                       )
    ordem_new = value #(
                          vbeln        = ''
                          posnr        = ''
                          matnr        = gs_troca_material_massa_header-matnr
*                        MATKL        = GS_TROCA_MATERIAL_MASSA_HEADER-MATKL
                          inco1        = gs_troca_material_massa_header-inco1
                          werks        = gs_troca_material_massa_header-werks
                          lote         = gs_troca_material_massa_header-lote
                          lgort        = gs_troca_material_massa_header-lgort
                          qtd_recebida = ls_troca_material_massa_item-qtd_recebida
                          meins        = gs_troca_material_massa_header-meins
                          vlr_venda    = ls_troca_material_massa_item-vlr_venda
*                        SPART        = GS_TROCA_MATERIAL_MASSA_HEADER-SPART
*                        MTART        = GS_TROCA_MATERIAL_MASSA_HEADER-MTART
                       )
    ).

    call method zcl_manutencao_insumos=>set_dados_troca
      exporting
        i_manutencao = ls_manutencao.

    call method zcl_manutencao_insumos=>troca_materiais_massa
      exporting
        i_check = abap_false.

    clear ls_manutencao.

  endloop.

  perform refresh.
  free: gt_troca_material_massa_item, gs_troca_material_massa_header.
  leave to screen 0.

endform.
*&---------------------------------------------------------------------*
*& Form F_BUSCA_0310
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_busca_0310 .

endform.
*&---------------------------------------------------------------------*
*& Form F_EXECUTA_0310
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_executa_0310 .

  data: ls_manutencao type zde_manutencao_ov,
        lt_return     type bapiret2_t.


  loop at it_desmem_massa_item into data(ls_desmem_massa_item).
    if ls_desmem_massa_item-kunnr is initial.
      message 'Existe linha com cliente em Branco!' type 'I' display like 'E'.
      exit.
    endif.
    if ls_desmem_massa_item-qtd is initial.
      message 'Existe linha com Quatidade em Branco!' type 'I' display like 'E'.
      exit.
    endif.
  endloop.

  loop at it_desmem_massa_item into ls_desmem_massa_item.

    ls_manutencao-desmembramento =
    value #(
              vbeln = ls_desmem_massa_item-vbeln
              posnr = ls_desmem_massa_item-posnr
              kunnr = ls_desmem_massa_item-kunnr
              qtd_recebida = ls_desmem_massa_item-qtd
           ).

    call method zcl_manutencao_insumos=>set_dados_desmembramento
      exporting
        i_manutencao = ls_manutencao.

    call method zcl_manutencao_insumos=>check_dados_desmembramento
      importing
        r_return = r_return.

    append lines of r_return to lt_return.

    clear: ls_manutencao, r_return.

  endloop.

  call method zcl_manutencao_insumos=>exibe_mensagens
    exporting
      i_return = lt_return.

  check not line_exists( lt_return[ type = '#' ] ).

  loop at it_desmem_massa_item into ls_desmem_massa_item.

    ls_manutencao-desmembramento =
    value #(
              vbeln = ls_desmem_massa_item-vbeln
              posnr = ls_desmem_massa_item-posnr
              kunnr = ls_desmem_massa_item-kunnr
              qtd_recebida = ls_desmem_massa_item-qtd
           ).

    call method zcl_manutencao_insumos=>set_dados_desmembramento
      exporting
        i_manutencao = ls_manutencao.

    call method zcl_manutencao_insumos=>desmembramento_massa
      exporting
        i_check  = abap_false
      importing
        r_return = data(r_return).

    append lines of r_return to lt_return.

    clear: ls_manutencao, r_return.

  endloop.

  call method zcl_manutencao_insumos=>exibe_mensagens
    exporting
      i_return = lt_return.

  perform refresh.
  free: it_desmem_massa_item, ls_manutencao, lt_return.
  leave to screen 0.

endform.
*&---------------------------------------------------------------------*
*& Form DESMEMBRAR_DEVOLUCAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_desmembrar_devolucao .

  data(check) = it_saida[].
  delete check where check ne abap_true.

  if lines( check ) ne 1.
    message 'Selecione somente uma Linha!' type 'I'.
    exit.
  endif.

  read table it_saida into wa_saida with key check = abap_true.

  if not 'ZROB_ZREB_ZRPF' cs wa_saida-auart.
    message 'Tipo de OV não permitida!' type 'I'.
    exit.
  endif.

  select single *
    from zsdt0090
    into @data(ls_0090)
    where vbeln eq @wa_saida-vbeln
      and posnn eq @wa_saida-posnr
      and estorno eq @abap_false.

  if sy-subrc is not initial.
    message 'Referencia de Devolução não encontrada!' type 'I'.
    exit.
  endif.

  select single count(*)
    from zsdt0090
    where vbelv eq @ls_0090-vbelv
    and posnv eq @ls_0090-posnv
    and categoria eq 'K'
    and estorno eq @abap_false.

  if sy-subrc is initial.
    message 'Já Existe uma Ação na Devolução Executada!' type 'I'.
    exit.
  endif.

  call method zcl_manutencao_insumos=>chk_desconto_abs_faturado
    exporting
      i_vbeln = ls_0090-vbelv
      i_matnr = ls_0090-matnrv
    importing
      is_ok   = data(is_ok).

  if is_ok is initial.
    message text-e14 type 'I'.
    exit.
  endif.

  clear ls_manutencao.

  ls_manutencao-desmembramento =
      value #(
                vbeln = ls_0090-vbelv
                posnr = ls_0090-posnv
                kunnr = wa_saida-kunnr
                qtd_recebida = abs( wa_saida-kwmeng )
             ).

  call method zcl_manutencao_insumos=>set_dados_desmembramento
    exporting
      i_manutencao = ls_manutencao.

  call method zcl_manutencao_insumos=>desmembramento_devolucao
    exporting
      i_desc_abs = wa_saida-desc_absoluto
    importing
      r_return   = data(r_return).

  append lines of r_return to lt_return.

  clear: ls_manutencao, r_return.

  call method zcl_manutencao_insumos=>exibe_mensagens
    exporting
      i_return = lt_return.

  perform refresh.
  free: ls_manutencao, lt_return.

endform.

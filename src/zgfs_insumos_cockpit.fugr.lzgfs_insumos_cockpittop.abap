*&-------------------------------------------------------------------&*
*&                    Histórico de Modificações                      &*
*& Autor ABAP |Request    |Data       |Descrição                     &*
*&-------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato&*
*&                                    |Compra.                       &*
*&                                    |Chamado: 168919 2ª Parte.     &*
*&-------------------------------------------------------------------&*
FUNCTION-POOL zgfs_insumos_cockpit.         "MESSAGE-ID ..

* INCLUDE LZGFS_INSUMOS_COCKPITD...          " Local class definition

DATA: g_popup         TYPE char1,
      g_doc_simulacao TYPE zsded003,
      p_insumo        TYPE char1,
      p_tpinsm        TYPE char1,      "<<<------"168919 - NMS ------>>>
      p_merint        TYPE char1,
      p_sintet        TYPE char1,
      p_analit        TYPE char1,
      p_contra        TYPE char1,
      p_venda         TYPE char1,
      p_distra        TYPE char1,
      p_aditiv        TYPE char1,
      p_decrec        TYPE char1,
      p_doctod        TYPE char1,
      p_pend          TYPE char1,
      p_conclu        TYPE char1,
      p_todos         TYPE char1,
*
      p_insumo_sel    TYPE char1,
      p_tpinsm_sel    TYPE char1,      "<<<------"168919 - NMS ------>>>
      p_merint_sel    TYPE char1,
      p_sintet_sel    TYPE char1,
      p_analit_sel    TYPE char1,
      p_contra_sel    TYPE char1,
      p_venda_sel     TYPE char1,
      p_distra_sel    TYPE char1,
      p_aditiv_sel    TYPE char1,
      p_decrec_sel    TYPE char1,
      p_doctod_sel    TYPE char1,
      p_pend_sel      TYPE char1,
      p_conclu_sel    TYPE char1,
      p_todos_sel     TYPE char1.
*
RANGES: s_vkorg        FOR zsdt0040-vkorg,
        s_vkbur        FOR zsdt0040-vkbur,
        s_docsi        FOR zsdt0040-doc_simulacao,
        s_kunnr        FOR zsdt0040-kunnr,
        s_erdat        FOR zsdt0040-erdat,
        s_cultu        FOR zsdt0040-cultura,
        s_safra        FOR zsdt0040-safra,
        s_spart        FOR zsdt0040-spart,
        s_moeda        FOR zsdt0040-waerk,
**<<<------"168919 - NMS - INI------>>>
        s_bukrs        FOR t001-bukrs,
        s_werks        FOR zmmt0035-werks,
        s_nosol        FOR zmmt0035-nro_sol_cp,
        s_ebeln        FOR zmmt0035-ebeln,
        s_lifnr        FOR zmmt0035-lifnr,
        s_safr2        FOR zmmt0035-safra,
        s_dtatl        FOR zmmt0035-data_atual,
        s_cult2 FOR zmmt0035-cultura,
**<<<------"168919 - NMS - FIM------>>>
*
        s_vkorg_sel    FOR zsdt0040-vkorg,
        s_vkbur_sel    FOR zsdt0040-vkbur,
        s_docsi_sel    FOR zsdt0040-doc_simulacao,
        s_kunnr_sel    FOR zsdt0040-kunnr,
        s_erdat_sel    FOR zsdt0040-erdat,
        s_cultu_sel    FOR zsdt0040-cultura,
        s_safra_sel    FOR zsdt0040-safra,
        s_spart_sel    FOR zsdt0040-spart,
**<<<------"168919 - NMS - INI------>>>
*        s_moeda_sel    FOR zsdt0040-waerk.
        s_moeda_sel    FOR zsdt0040-waerk,
        s_bukrs_sel    FOR t001-bukrs,
        s_werks_sel    FOR zmmt0035-werks,
        s_nosol_sel    FOR zmmt0035-nro_sol_cp,
        s_ebeln_sel    FOR zmmt0035-ebeln,
        s_lifnr_sel    FOR zmmt0035-lifnr,
        s_safr2_sel    FOR zmmt0035-safra,
        s_dtatl_sel    FOR zmmt0035-data_atual,
        s_cult2_sel    FOR zmmt0035-cultura.
**<<<------"168919 - NMS - FIM------>>>
INCLUDE zsdr0150_top_comum.

*****************************************************************************
*****************************************************************************

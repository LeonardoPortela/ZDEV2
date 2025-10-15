*&---------------------------------------------------------------------&*
*&                    Histórico de Modificações                        &*
*& Autor ABAP |Request    |Data       |Descrição                       &*
*&---------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações pra geração Contrato &*
*&                                    |Compra.                         &*
*&                                    |Chamado: 168919 2ª Parte.       &*
*&---------------------------------------------------------------------&*
FUNCTION zsd_insumos_cockpit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_POPUP) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DOC_SIMULACAO) TYPE  ZSDED003 OPTIONAL
*"     REFERENCE(I_INSUMO) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_TPINSM) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_MERINT) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_SINTET) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_ANALIT) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_CONTRA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_VENDA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DISTRA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_ADITIV) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DECREC) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DOCTOD) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_PEND) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_CONCLU) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_TODOS) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      I_VKORG STRUCTURE  ZSD_RANGE_VKORG OPTIONAL
*"      I_VKBUR STRUCTURE  ZSD_RANGE_VKBUR OPTIONAL
*"      I_DOCSI STRUCTURE  ZSD_RANGE_DOCSI OPTIONAL
*"      I_KUNNR STRUCTURE  ZSD_RANGE_KUNNR OPTIONAL
*"      I_ERDAT STRUCTURE  ZSD_RANGE_ERDAT OPTIONAL
*"      I_CULTU STRUCTURE  ZSD_RANGE_CULTURA OPTIONAL
*"      I_SAFRA STRUCTURE  ZSD_RANGE_SAFRA OPTIONAL
*"      I_SPART STRUCTURE  ZSD_RANGE_SPART OPTIONAL
*"      I_MOEDA STRUCTURE  ZSD_RANGE_WAERK OPTIONAL
*"      I_BUKRS STRUCTURE  ZDE_BUKRS_R OPTIONAL
*"      I_WERKS STRUCTURE  WERKS_RANG OPTIONAL
*"      I_NOSOL STRUCTURE  RANGE_N10 OPTIONAL
*"      I_EBELN STRUCTURE  ZDE_EBELN_R OPTIONAL
*"      I_LIFNR STRUCTURE  ZDE_LIFNR_RANGES OPTIONAL
*"      I_SAFR2 STRUCTURE  SRG_CHAR10 OPTIONAL
*"      I_DTATL STRUCTURE  RANGE_DATE OPTIONAL
*"----------------------------------------------------------------------

  FREE: p_insumo, p_merint,
        p_sintet, p_analit,
        p_contra, p_venda,  p_distra, p_aditiv, p_decrec, p_doctod,
        p_pend,   p_conclu, p_todos,
        p_vlr,    p_manual, p_eletronica,
        s_vkorg,  s_vkbur,  s_docsi,  s_kunnr,  s_erdat,
        s_cultu , s_safra,  s_spart,  s_moeda.
  FREE: p_tpinsm, s_bukrs, s_werks, s_nosol, s_ebeln, s_lifnr, s_safr2, s_dtatl.   "<<<------"168919 - NMS ------>>>

  g_popup         = i_popup.
  g_doc_simulacao = i_doc_simulacao.

*------------------------------------
* guarda a selecao original feita
*------------------------------------
  IF g_popup = abap_false.
    p_insumo_sel  = i_insumo.
    p_tpinsm_sel  = i_tpinsm.
    p_merint_sel  = i_merint.
    p_sintet_sel  = i_sintet.
    p_analit_sel  = i_analit.
    p_contra_sel  = i_contra.
    p_venda_sel   = i_venda.
    p_distra_sel  = i_distra.
    p_aditiv_sel  = i_aditiv.
    p_decrec_sel  = i_decrec.
    p_doctod_sel  = i_doctod.
    p_pend_sel    = i_pend.
    p_conclu_sel  = i_conclu.
    p_todos_sel   = i_todos.
*
    s_vkorg_sel[] = i_vkorg[].
    s_vkbur_sel[] = i_vkbur[].
    s_docsi_sel[] = i_docsi[].
    s_kunnr_sel[] = i_kunnr[].
    s_erdat_sel[] = i_erdat[].
    s_cultu_sel[] = i_cultu[].
    s_safra_sel[] = i_safra[].
    s_spart_sel[] = i_spart[].
    s_moeda_sel[] = i_moeda[].
**<<<------"168919 - NMS - INI------>>>
    s_bukrs_sel[] = i_bukrs[].
    s_werks_sel[] = i_werks[].
    s_nosol_sel[] = i_nosol[].
    s_ebeln_sel[] = i_ebeln[].
    s_lifnr_sel[] = i_lifnr[].
    s_safr2_sel[] = i_safr2[].
    s_dtatl_sel[] = i_dtatl[].
**<<<------"168919 - NMS - FIM------>>>
  ENDIF.

*------------------------------------
* set variaveis de selecao
*------------------------------------
  p_insumo        = i_insumo.
  p_tpinsm        = i_tpinsm.
  p_merint        = i_merint.
  p_sintet        = i_sintet.
  p_analit        = i_analit.
  p_contra        = i_contra.
  p_venda         = i_venda.
  p_distra        = i_distra.
  p_aditiv        = i_aditiv.
  p_decrec        = i_decrec.
  p_doctod        = i_doctod.
  p_pend          = i_pend.
  p_conclu        = i_conclu.
  p_todos         = i_todos.
*
  s_vkorg[]       = i_vkorg[].
  s_vkbur[]       = i_vkbur[].
  s_docsi[]       = i_docsi[].
  s_kunnr[]       = i_kunnr[].
  s_erdat[]       = i_erdat[].
  s_cultu[]       = i_cultu[].
  s_safra[]       = i_safra[].
  s_spart[]       = i_spart[].
  s_moeda[]       = i_moeda[].
**<<<------"168919 - NMS - INI------>>>
  s_bukrs[]       = i_bukrs[].
  s_werks[]       = i_werks[].
  s_nosol[]       = i_nosol[].
  s_ebeln[]       = i_ebeln[].
  s_lifnr[]       = i_lifnr[].
  s_safr2[]       = i_safr2[].
  s_dtatl[]       = i_dtatl[].
**<<<------"168919 - NMS - FIM------>>>
*---------------------------
*-tela inicial
*---------------------------
  PERFORM f_alv_saida.

ENDFUNCTION.

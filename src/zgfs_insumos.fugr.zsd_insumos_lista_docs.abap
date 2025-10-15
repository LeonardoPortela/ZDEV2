FUNCTION zsd_insumos_lista_docs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOC_SIMULACAO) TYPE  ZSDED003
*"     REFERENCE(I_TIPO_DOC) TYPE  ZTIPO_DOC OPTIONAL
*"     REFERENCE(I_STATUS) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_IMPRIME) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------

*  FREE: t_anali,
*        p_insumo, p_merint, p_sintet, p_analit, p_contra,
*        p_venda,  p_distra, p_aditiv, p_decrec, p_doctod,
*        p_pend,   p_conclu, p_todos , p_vlr,    p_manual, p_eletronica.
*
*  l_imprime = i_imprime.
*
**----------------------------
** ajusta funcionalidades
**----------------------------
*  CASE i_tipo_doc.
*    WHEN 'CTR'.
*      p_contra = abap_true.
*    WHEN 'OVD'.
*      p_venda  = abap_true.
*    WHEN 'DTR'.
*      p_distra = abap_true.
*    WHEN 'ADT'.
*      p_aditiv = abap_true.
*    WHEN 'DCR'.
*      p_decrec = abap_true.
*    WHEN OTHERS.
*      p_doctod = abap_true.
*  ENDCASE.
*
*  CASE i_status.
*    WHEN 'P'.
*      p_pend   = abap_true.
*    WHEN 'C'.
*      p_conclu = abap_true.
*    WHEN 'T'.
*      p_todos  = abap_true.
*    WHEN OTHERS.
*      p_todos  = abap_true.
*  ENDCASE.
*
**----------------------------
** SELECAO
**----------------------------
*  PERFORM f_selecao_dados            USING i_doc_simulacao.
*  PERFORM f_processa_dados_analitico.
*
*  CHECK t_anali[] IS NOT INITIAL.
*
*  CALL SCREEN 0100 STARTING AT 20   2
*                     ENDING AT 170  19.

ENDFUNCTION.

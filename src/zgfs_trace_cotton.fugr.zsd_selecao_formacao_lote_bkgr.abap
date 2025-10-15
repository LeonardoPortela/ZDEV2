FUNCTION zsd_selecao_formacao_lote_bkgr.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NRO_SOL_OV) TYPE  OBJNUM OPTIONAL
*"     REFERENCE(I_POSNR) TYPE  POSNR_VA OPTIONAL
*"     REFERENCE(I_CONTRATO) TYPE  TEXT50 OPTIONAL
*"     REFERENCE(I_SOMENTE_EXIBE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_EDITAR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_REFERENCIA) TYPE  NUMC10 OPTIONAL
*"  TABLES
*"      T_RETORNO
*"----------------------------------------------------------------------

  FREE: t_saida, t_retorno, g_back.

*-----------------------
* seleao dos lotes
*-----------------------
  PERFORM f_selecao2 USING i_nro_sol_ov
                          i_posnr
                          i_contrato.

  PERFORM f_agrupamento CHANGING t_retorno[].

ENDFUNCTION.

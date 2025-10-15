FUNCTION zsd_insumos_lista_aditivos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOC_SIMULACAO) TYPE  ZNR_VENDA
*"----------------------------------------------------------------------

  FREE: t_zsdt0090.

  PERFORM f_selecao_aditivos     USING i_doc_simulacao.

  CHECK t_zsdt0090[] IS NOT INITIAL.

  CALL SCREEN 0200 STARTING AT 20   2
                     ENDING AT 170  19.

ENDFUNCTION.

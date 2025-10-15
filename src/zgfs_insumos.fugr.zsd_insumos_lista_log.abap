FUNCTION zsd_insumos_lista_log.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ID_DOC_AGRUPADOR) TYPE  ZID_AGRUPADOR
*"----------------------------------------------------------------------

  FREE: t_log.

  PERFORM f_selecao_log     USING i_id_doc_agrupador.

  CHECK t_log[] IS NOT INITIAL.

  CALL SCREEN 0300 STARTING AT 20   2
                     ENDING AT 170  19.

ENDFUNCTION.

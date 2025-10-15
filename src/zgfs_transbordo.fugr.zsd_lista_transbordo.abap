FUNCTION zsd_lista_transbordo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ID_SEQ) TYPE  NUMC10
*"  TABLES
*"      T_RESULTADO STRUCTURE  ZSDS077
*"----------------------------------------------------------------------

  FREE: g_custom_container,
        t_saida,
        t_resultado,
        t_selecao.

  g_id_seq = i_id_seq.

  CALL SCREEN 0100 STARTING AT 30   2
                     ENDING AT 150  19.

  t_resultado[] = t_selecao[].

ENDFUNCTION.

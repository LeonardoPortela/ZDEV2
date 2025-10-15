FUNCTION zco_movimentacao_mensal.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_MOVIMENTO_MENSAL STRUCTURE  ZMOVIMENTO_MENSAL_BPC
*"      T_RET_MOVIMENTO_MENSAL STRUCTURE  ZRET_MOVIMENTO_MENSAL_BPC
*"       OPTIONAL
*"----------------------------------------------------------------------

  it_movimento_mensal[] = t_movimento_mensal[].

  PERFORM : zseleciona_dados_mov_mensal,
            zprocessa_retorno_mov_mensal  .

  t_ret_movimento_mensal[] = it_ret_movimento_mensal[].


ENDFUNCTION.

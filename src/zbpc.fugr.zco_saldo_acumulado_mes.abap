FUNCTION ZCO_SALDO_ACUMULADO_MES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_SALDO_ACUMULADO STRUCTURE  ZSALDO_ACUMULADO_BPC
*"      T_RET_SALDO_ACUMULADO STRUCTURE  ZRET_SALDO_ACUMULADO_BPC
*"       OPTIONAL
*"----------------------------------------------------------------------

  IT_SALDO_ACUMULADO_MES[] = T_SALDO_ACUMULADO[].

  PERFORM : ZSELECIONA_DADOS_SLD_ACM_MES,
            ZPROCESSA_RETORNO_SLD_ACM_MES .

  T_RET_SALDO_ACUMULADO[] = IT_RET_SALDO_ACUMULADO_MES[].


ENDFUNCTION.

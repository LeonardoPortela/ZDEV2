FUNCTION ZSD_ITENS_SIMULADOR_VENDAS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0041 STRUCTURE  ZSDT0041
*"----------------------------------------------------------------------


  SELECT *
    FROM zsdt0041
    INTO TABLE et_zsdt0041.


ENDFUNCTION.

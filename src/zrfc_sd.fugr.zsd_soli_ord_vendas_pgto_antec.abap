FUNCTION zsd_soli_ord_vendas_pgto_antec.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0054 STRUCTURE  ZSDT0054
*"----------------------------------------------------------------------


  SELECT *
    FROM zsdt0054
    INTO TABLE et_zsdt0054.


ENDFUNCTION.

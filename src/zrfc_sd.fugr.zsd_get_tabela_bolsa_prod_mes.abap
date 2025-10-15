FUNCTION ZSD_GET_TABELA_BOLSA_PROD_MES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0076 STRUCTURE  ZSDT0076
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0076
    INTO TABLE et_ZSDT0076.


ENDFUNCTION.

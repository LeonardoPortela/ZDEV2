FUNCTION ZSD_GET_TABELA_PRECO_CONTRATO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0227 STRUCTURE  ZSDT0227
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0227
    INTO TABLE et_ZSDT0227.


ENDFUNCTION.

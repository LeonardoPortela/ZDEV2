FUNCTION ZSD_GET_SOLIC_ORDEM_VENDA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0063 STRUCTURE  ZSDT0063
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0063
    INTO TABLE et_ZSDT0063.


ENDFUNCTION.

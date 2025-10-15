FUNCTION ZSD_GET_SOLIC_OV_FORMACAO_LOTE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0066 STRUCTURE  ZSDT0066
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0066
    INTO TABLE et_ZSDT0066.


ENDFUNCTION.

FUNCTION ZSD_GET_SOLIC_OV_FIX_COND_ESPE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0073 STRUCTURE  ZSDT0073
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0073
    INTO TABLE et_ZSDT0073.


ENDFUNCTION.

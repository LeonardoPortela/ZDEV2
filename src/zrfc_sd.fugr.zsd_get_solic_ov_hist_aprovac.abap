FUNCTION ZSD_GET_SOLIC_OV_HIST_APROVAC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0069 STRUCTURE  ZSDT0069
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0069
    INTO TABLE et_ZSDT0069.


ENDFUNCTION.

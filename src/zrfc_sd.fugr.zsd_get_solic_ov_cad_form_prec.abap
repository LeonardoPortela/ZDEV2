FUNCTION ZSD_GET_SOLIC_OV_CAD_FORM_PREC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0070 STRUCTURE  ZSDT0070
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0070
    INTO TABLE et_ZSDT0070.


ENDFUNCTION.

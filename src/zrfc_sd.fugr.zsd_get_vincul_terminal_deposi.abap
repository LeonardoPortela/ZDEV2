FUNCTION ZSD_GET_VINCUL_TERMINAL_DEPOSI.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0108 STRUCTURE  ZSDT0108
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0108
    INTO TABLE et_ZSDT0108.


ENDFUNCTION.

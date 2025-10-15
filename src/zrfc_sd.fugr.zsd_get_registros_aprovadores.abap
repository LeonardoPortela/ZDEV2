FUNCTION ZSD_GET_REGISTROS_APROVADORES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0162 STRUCTURE  ZSDT0162
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0162
    INTO TABLE et_ZSDT0162.


ENDFUNCTION.

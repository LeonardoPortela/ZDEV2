FUNCTION zsd_get_cadastro_culturas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0038 STRUCTURE  ZSDT0038
*"----------------------------------------------------------------------


  SELECT *
    FROM zsdt0038
    INTO TABLE et_zsdt0038.


ENDFUNCTION.

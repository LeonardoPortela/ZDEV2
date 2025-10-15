FUNCTION ZFI_GET_DATA_EMPRESAS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_T001 STRUCTURE  T001
*"----------------------------------------------------------------------

  SELECT *
    FROM t001
    INTO TABLE et_t001.

ENDFUNCTION.

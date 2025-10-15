FUNCTION ZFI_GET_DATA_DEPOSITOS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_T001L STRUCTURE  T001L
*"----------------------------------------------------------------------

  SELECT *
    FROM t001L
    INTO TABLE et_t001L.

ENDFUNCTION.

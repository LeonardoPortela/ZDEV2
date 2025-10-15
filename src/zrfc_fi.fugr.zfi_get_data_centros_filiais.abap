FUNCTION ZFI_GET_DATA_CENTROS_FILIAIS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_T001W STRUCTURE  T001W
*"----------------------------------------------------------------------

  SELECT *
    FROM t001W
    INTO TABLE et_t001W.

ENDFUNCTION.

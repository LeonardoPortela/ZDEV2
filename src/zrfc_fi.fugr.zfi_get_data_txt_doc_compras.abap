FUNCTION ZFI_GET_DATA_TXT_DOC_COMPRAS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_T161T STRUCTURE  T161T
*"----------------------------------------------------------------------

  SELECT *
    FROM T161T
    INTO TABLE et_T161T.

ENDFUNCTION.

FUNCTION ZFI_GET_DATA_COND_PAGAMENTO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_T052U STRUCTURE  T052U
*"----------------------------------------------------------------------

  SELECT *
    FROM T052U
    INTO TABLE et_T052U.

ENDFUNCTION.

FUNCTION ZFI_GET_DATA_DESCRICAO_CFOP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_J_1BAGNT STRUCTURE  J_1BAGNT
*"----------------------------------------------------------------------

  SELECT *
    FROM j_1bagnt
    INTO TABLE et_j_1bagnt.

ENDFUNCTION.

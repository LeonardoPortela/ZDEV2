FUNCTION zfi_get_data_cfop.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_J_1BAGN STRUCTURE  J_1BAGN
*"----------------------------------------------------------------------

  SELECT *
    FROM j_1bagn
    INTO TABLE et_j_1bagn.

ENDFUNCTION.

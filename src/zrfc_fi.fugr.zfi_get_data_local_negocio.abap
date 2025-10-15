FUNCTION zfi_get_data_local_negocio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_J_1BBRANCH STRUCTURE  J_1BBRANCH
*"----------------------------------------------------------------------

  SELECT *
    FROM j_1bbranch
    INTO TABLE et_j_1bbranch.

ENDFUNCTION.

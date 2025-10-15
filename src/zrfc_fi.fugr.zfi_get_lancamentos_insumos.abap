FUNCTION zfi_get_lancamentos_insumos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZFIT0026 STRUCTURE  ZFIT0026
*"----------------------------------------------------------------------


  SELECT *
    FROM zfit0026
    INTO TABLE et_zfit0026.


ENDFUNCTION.

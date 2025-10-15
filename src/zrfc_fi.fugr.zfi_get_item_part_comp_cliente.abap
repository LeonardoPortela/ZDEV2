FUNCTION zfi_get_item_part_comp_cliente.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CPUDT_INI) TYPE  CPUDT OPTIONAL
*"     VALUE(I_CPUDT_FIM) TYPE  CPUDT OPTIONAL
*"  TABLES
*"      ET_BSAD STRUCTURE  BSAD
*"----------------------------------------------------------------------


  DATA: lt_range_cpudt TYPE RANGE OF bsad-cpudt.

  CHECK i_cpudt_ini IS NOT INITIAL.

  IF i_cpudt_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_cpudt_ini
                             high = i_cpudt_fim ) TO lt_range_cpudt.
  ENDIF.

  SELECT *
    FROM bsad
    INTO TABLE et_bsad
    WHERE cpudt IN lt_range_cpudt.

ENDFUNCTION.

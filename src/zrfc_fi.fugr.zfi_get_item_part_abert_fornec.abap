FUNCTION ZFI_GET_ITEM_PART_ABERT_FORNEC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CPUDT_INI) TYPE  CPUDT OPTIONAL
*"     VALUE(I_CPUDT_FIM) TYPE  CPUDT OPTIONAL
*"  TABLES
*"      ET_BSIK STRUCTURE  BSIK
*"----------------------------------------------------------------------


  DATA: lt_range_cpudt TYPE RANGE OF bsik-cpudt.

  CHECK i_cpudt_ini IS NOT INITIAL.

  IF i_cpudt_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_cpudt_ini
                             high = i_cpudt_fim ) TO lt_range_cpudt.
  ENDIF.

  SELECT *
    FROM bsik
    INTO TABLE et_bsik
    WHERE cpudt IN lt_range_cpudt.

ENDFUNCTION.

FUNCTION ZFI_GET_ITEM_PART_ABERT_CLIENT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CPUDT_INI) TYPE  CPUDT OPTIONAL
*"     VALUE(I_CPUDT_FIM) TYPE  CPUDT OPTIONAL
*"  TABLES
*"      ET_BSID STRUCTURE  BSID
*"----------------------------------------------------------------------


  DATA: lt_range_cpudt TYPE RANGE OF bsid-cpudt.

  CHECK i_cpudt_ini IS NOT INITIAL.

  IF i_cpudt_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_cpudt_ini
                             high = i_cpudt_fim ) TO lt_range_cpudt.
  ENDIF.

  SELECT *
    FROM bsid
    INTO TABLE et_bsid
    WHERE cpudt IN lt_range_cpudt.

ENDFUNCTION.

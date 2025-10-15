FUNCTION zmm_get_cab_documento_material .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  CPUDT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  CPUDT OPTIONAL
*"  TABLES
*"      ET_RET_MKPF STRUCTURE  MKPF
*"----------------------------------------------------------------------

  DATA: lt_range_data TYPE RANGE OF mkpf-cpudt.


  CHECK i_data_ini IS NOT INITIAL.

  IF i_data_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_data_ini
                             high = i_data_fim ) TO lt_range_data.
  ENDIF.


  SELECT *
    FROM mkpf
    INTO CORRESPONDING FIELDS OF TABLE et_ret_mkpf
    WHERE cpudt IN lt_range_data.

  SELECT *
    FROM mkpf
    APPENDING TABLE et_ret_mkpf
    WHERE aedat IN lt_range_data.

ENDFUNCTION.

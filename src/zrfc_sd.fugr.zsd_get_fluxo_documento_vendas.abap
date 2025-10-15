FUNCTION zsd_get_fluxo_documento_vendas .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      ET_VBFA STRUCTURE  VBFA
*"----------------------------------------------------------------------
  DATA:
    lt_range_data  TYPE RANGE OF vbak-erdat,
    lt_range_aedat TYPE RANGE OF vbak-aedat.

  IF i_data_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_data_ini
                             high = i_data_fim ) TO lt_range_data.
  ENDIF.

  SELECT *
    FROM vbfa
    INTO TABLE et_vbfa
    WHERE erdat IN lt_range_data.

  SELECT *
    FROM vbfa
    APPENDING TABLE et_vbfa
    WHERE aedat IN lt_range_data.

  SORT et_vbfa BY ruuid.
  DELETE ADJACENT DUPLICATES FROM et_vbfa COMPARING ruuid.

ENDFUNCTION.

FUNCTION zfi_get_data_doc_contabil.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  BLDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  BLDAT OPTIONAL
*"  TABLES
*"      ET_BKPF STRUCTURE  BKPF
*"----------------------------------------------------------------------


  DATA: lt_range_data TYPE RANGE OF bkpf-bldat.

  CHECK i_data_ini IS NOT INITIAL.

  IF i_data_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_data_ini
                             high = i_data_fim ) TO lt_range_data.
  ENDIF.

  SELECT *
    FROM bkpf
    INTO TABLE et_bkpf
    WHERE cpudt IN lt_range_data.

  SELECT *
   FROM bkpf
   APPENDING TABLE et_bkpf
   WHERE upddt IN lt_range_data.

  SORT et_bkpf BY bukrs belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM et_bkpf COMPARING bukrs belnr gjahr.

ENDFUNCTION.

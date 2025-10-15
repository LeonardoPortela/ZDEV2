FUNCTION zmm_data_item_documento_compra.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  AEDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  AEDAT OPTIONAL
*"  TABLES
*"      ET_EKPO STRUCTURE  EKPO
*"----------------------------------------------------------------------

  DATA: lt_range_data         TYPE RANGE OF ekpo-aedat,
        lt_range_creationdate TYPE RANGE OF ekpo-creationdate.

  CHECK i_data_ini IS NOT INITIAL.


  APPEND VALUE #(  sign = 'I'
                           option = 'BT'
                           low  = i_data_ini
                           high = i_data_fim ) TO lt_range_data.

  SELECT *
    FROM ekpo
    INTO TABLE et_ekpo
    WHERE aedat IN lt_range_data.

  SELECT *
    FROM ekpo
    APPENDING TABLE et_ekpo
    WHERE creationdate IN lt_range_data.

ENDFUNCTION.

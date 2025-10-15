FUNCTION zsd_get_divisao_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_VBEP STRUCTURE  VBEP
*"----------------------------------------------------------------------


  TYPES: BEGIN OF ty_cdhdr,
           objectid TYPE cdhdr-objectid,
         END OF ty_cdhdr.


  DATA: lt_cdhdr      TYPE STANDARD TABLE OF ty_cdhdr,
        lt_range_data TYPE RANGE OF datum.

  CONSTANTS: c_class TYPE cdhdr-objectclas VALUE 'VERKBELEG'.

  CHECK i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                   option = 'BT'
                   low  = i_data_ini
                   high = i_data_fim ) TO lt_range_data.

  SELECT objectid
    FROM cdhdr
    INTO TABLE lt_cdhdr
    WHERE udate IN lt_range_data
      AND objectclas EQ c_class.
  IF sy-subrc IS INITIAL.
    SELECT *
     FROM vbep
     APPENDING CORRESPONDING FIELDS OF TABLE et_vbep
      FOR ALL ENTRIES IN lt_cdhdr
     WHERE vbeln EQ lt_cdhdr-objectid(10).
  ENDIF.

ENDFUNCTION.

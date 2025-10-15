FUNCTION ZSD_GET_ITEM_DOC_FATURAMENTO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      ET_RET_VBRP STRUCTURE  VBRP
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_cdhdr,
           objectid TYPE cdhdr-objectid,
         END OF ty_cdhdr.


  DATA: lt_cdhdr      TYPE STANDARD TABLE OF ty_cdhdr,
        lt_range_data TYPE RANGE OF vbrp-erdat.

  CONSTANTS: c_class TYPE cdhdr-objectclas VALUE 'FAKTBELEG'.

  CHECK i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                   option = 'BT'
                   low  = i_data_ini
                   high = i_data_fim ) TO lt_range_data.

  SELECT *
    FROM vbrp
    INTO CORRESPONDING FIELDS OF TABLE et_ret_vbrp
    WHERE erdat IN lt_range_data.
  IF sy-subrc IS INITIAL.
    SORT et_ret_vbrp BY vbeln erdat.
  ENDIF.

  SELECT objectid
    FROM cdhdr
    INTO TABLE lt_cdhdr
    WHERE udate IN lt_range_data
      AND objectclas EQ c_class.
  IF sy-subrc IS INITIAL.
    SELECT *
     FROM vbrp
     APPENDING CORRESPONDING FIELDS OF TABLE et_ret_vbrp
      FOR ALL ENTRIES IN lt_cdhdr
     WHERE vbeln EQ lt_cdhdr-objectid(10).
    IF sy-subrc IS INITIAL.
      SORT et_ret_vbrp BY vbeln erdat.
    ENDIF.
  ENDIF.

  SORT et_ret_vbrp by vbeln.
  delete ADJACENT DUPLICATES FROM et_ret_vbrp COMPARING vbeln.





ENDFUNCTION.

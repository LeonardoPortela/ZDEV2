FUNCTION zmaster_data_get_supplier.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      ET_RET_LFA1 TYPE  ZSDST0041_LFA1
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_cdhdr,
           objectid TYPE cdhdr-objectid,
         END OF ty_cdhdr.


  DATA: lt_cdhdr      TYPE STANDARD TABLE OF ty_cdhdr,
        lt_range_data TYPE RANGE OF lfa1-erdat.

  CONSTANTS: c_class TYPE cdhdr-objectclas VALUE 'KRED'.

  CHECK i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                   option = 'BT'
                   low  = i_data_ini
                   high = i_data_fim ) TO lt_range_data.

  SELECT *
    FROM lfa1
    INTO CORRESPONDING FIELDS OF TABLE et_ret_lfa1
    WHERE erdat IN lt_range_data.
  IF sy-subrc IS INITIAL.
    SORT et_ret_lfa1 BY lifnr erdat.
  ENDIF.

  SELECT objectid
    FROM cdhdr
    INTO TABLE lt_cdhdr
    WHERE udate IN lt_range_data
      AND objectclas EQ c_class.
  IF sy-subrc IS INITIAL.
    SELECT *
     FROM lfa1
     APPENDING CORRESPONDING FIELDS OF TABLE et_ret_lfa1
      FOR ALL ENTRIES IN lt_cdhdr
     WHERE lifnr EQ lt_cdhdr-objectid(10).
    IF sy-subrc IS INITIAL.
      SORT et_ret_lfa1 BY lifnr erdat.
    ENDIF.
  ENDIF.

  SORT et_ret_lfa1 by lifnr.
  delete ADJACENT DUPLICATES FROM et_ret_lfa1 COMPARING lifnr.





ENDFUNCTION.

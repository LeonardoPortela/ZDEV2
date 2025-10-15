FUNCTION zfi_get_itens_nota_fiscal.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      ET_J_1BNFLIN STRUCTURE  J_1BNFLIN
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_cdhdr,
           objectid TYPE cdhdr-objectid,
         END OF ty_cdhdr.


  DATA: lt_cdhdr      TYPE STANDARD TABLE OF ty_cdhdr,
        lt_range_data TYPE RANGE OF lfa1-erdat,
        lt_docnum     TYPE TABLE OF j_1bnflin.

  CONSTANTS: c_class TYPE cdhdr-objectclas VALUE 'NOTAFISCAL'.

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

    LOOP AT lt_cdhdr ASSIGNING FIELD-SYMBOL(<fs_cdhdr>).
      APPEND INITIAL LINE TO lt_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).

      <fs_docnum>-docnum = <fs_cdhdr>-objectid+3(10).

    ENDLOOP.

    SELECT *
     FROM j_1bnflin
     APPENDING CORRESPONDING FIELDS OF TABLE et_j_1bnflin
      FOR ALL ENTRIES IN lt_docnum
     WHERE docnum EQ lt_docnum-docnum.
    IF sy-subrc IS INITIAL.
      SORT et_j_1bnflin BY docnum itmnum.
    ENDIF.
  ENDIF.

  SORT et_j_1bnflin BY docnum itmnum.
  DELETE ADJACENT DUPLICATES FROM et_j_1bnflin COMPARING docnum itmnum.

ENDFUNCTION.

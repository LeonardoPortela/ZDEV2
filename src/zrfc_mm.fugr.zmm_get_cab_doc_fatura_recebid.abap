FUNCTION zmm_get_cab_doc_fatura_recebid.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  CPUDT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  CPUDT OPTIONAL
*"  TABLES
*"      ET_RBKP STRUCTURE  RBKP
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_cdhdr,
           objectid TYPE cdhdr-objectid,
         END OF ty_cdhdr.


  DATA: lt_cdhdr      TYPE STANDARD TABLE OF ty_cdhdr,
        lt_range_data TYPE RANGE OF rbkp-cpudt.

  CONSTANTS: c_class TYPE cdhdr-objectclas VALUE 'INCOMINGINVOICE'.

  CHECK i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                   option = 'BT'
                   low  = i_data_ini
                   high = i_data_fim ) TO lt_range_data.

  SELECT *
    FROM rbkp
    INTO CORRESPONDING FIELDS OF TABLE et_rbkp
    WHERE cpudt IN lt_range_data.
  IF sy-subrc IS INITIAL.
    SORT et_rbkp BY belnr cpudt.
  ENDIF.

  SELECT objectid
    FROM cdhdr
    INTO TABLE lt_cdhdr
    WHERE udate IN lt_range_data
      AND objectclas EQ c_class.
  IF sy-subrc IS INITIAL.
    SELECT *
     FROM rbkp
     APPENDING CORRESPONDING FIELDS OF TABLE et_rbkp
      FOR ALL ENTRIES IN lt_cdhdr
     WHERE belnr EQ lt_cdhdr-objectid(10).
    IF sy-subrc IS INITIAL.
      SORT et_rbkp BY belnr cpudt.
    ENDIF.
  ENDIF.

  SORT et_rbkp BY belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM et_rbkp COMPARING belnr gjahr.


ENDFUNCTION.

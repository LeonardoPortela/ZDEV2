*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_INDICADORESF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_date_range
*&---------------------------------------------------------------------*
FORM f_date_range  USING    i_start i_end
                            i_hour
                   CHANGING ir_data_range ir_tims_range.

  DATA: r_dats TYPE RANGE OF dats,
        r_tims TYPE RANGE OF tims.

  IF i_end IS NOT INITIAL.
    r_dats = VALUE #( sign = 'I' option = 'BT' ( low  = i_start high = i_end ) ).
  ELSE.
    r_dats = VALUE #( sign = 'I' option = 'EQ' ( low  = i_start ) ).
  ENDIF.

  IF i_hour IS NOT INITIAL.
    r_tims = VALUE #( sign = 'I' option = 'BT' ( low  = i_hour
                                                 high = '235959') ).
    ir_tims_range = r_tims.
  ENDIF.

  ir_data_range = r_dats.

ENDFORM.

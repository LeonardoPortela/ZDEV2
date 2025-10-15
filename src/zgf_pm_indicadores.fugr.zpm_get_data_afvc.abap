FUNCTION zpm_get_data_afvc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      AFVC STRUCTURE  AFVC
*"----------------------------------------------------------------------

  r_date = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = COND #( WHEN i_end IS INITIAL THEN i_start
                                                              ELSE i_end ) ) ).

  SELECT aufpl FROM afru INTO TABLE @DATA(t_afru)
    WHERE budat IN @r_date
       OR laeda IN @r_date.

  IF t_afru IS NOT INITIAL.
    SELECT * FROM afvc INTO TABLE afvc
      FOR ALL ENTRIES IN t_afru
      WHERE aufpl EQ t_afru-aufpl.
  ENDIF.


ENDFUNCTION.

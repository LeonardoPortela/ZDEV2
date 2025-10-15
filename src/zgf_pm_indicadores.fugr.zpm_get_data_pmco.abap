FUNCTION zpm_get_data_pmco.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_YEAR) TYPE  GJAHR
*"  TABLES
*"      PMCO STRUCTURE  PMCO
*"----------------------------------------------------------------------

  r_year = VALUE #( sign = 'I' option = 'EQ' ( low = i_year ) ).

  SELECT * FROM pmco INTO TABLE pmco
    WHERE gjahr IN r_year.

ENDFUNCTION.

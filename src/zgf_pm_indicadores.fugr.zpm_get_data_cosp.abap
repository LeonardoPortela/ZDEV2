FUNCTION ZPM_GET_DATA_COSP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_YEAR) TYPE  GJAHR
*"  TABLES
*"      COSP STRUCTURE  COSP
*"----------------------------------------------------------------------


SELECT * FROM v_COSP_view INTO CORRESPONDING FIELDS OF TABLE @COSP
  WHERE GJAHR EQ @i_year.


ENDFUNCTION.

FUNCTION ZPM_GET_DATA_MAKT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      MAKT STRUCTURE  MAKT OPTIONAL
*"----------------------------------------------------------------------


SELECT * FROM makt INTO TABLE makt
  WHERE spras EQ sy-langu.


ENDFUNCTION.

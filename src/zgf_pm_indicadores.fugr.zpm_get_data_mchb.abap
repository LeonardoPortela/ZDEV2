FUNCTION ZPM_GET_DATA_MCHB.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      MCHB STRUCTURE  MCHB OPTIONAL
*"----------------------------------------------------------------------


SELECT * FROM mchb INTO TABLE mchb
  WHERE clabs NE space.


ENDFUNCTION.

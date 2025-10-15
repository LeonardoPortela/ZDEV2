FUNCTION ZPM_GET_DATA_MMPX.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      MMPX STRUCTURE  MMPX
*"----------------------------------------------------------------------


SELECT * FROM mmpx INTO TABLE mmpx
  WHERE spras EQ 'P'.


ENDFUNCTION.

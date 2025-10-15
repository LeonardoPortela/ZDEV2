FUNCTION ZPM_GET_DATA_ZHCMT0007.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(ET_ZHCMT0007) TYPE  ZDE_ZHCMT0007
*"----------------------------------------------------------------------

SELECT * FROM ZHCMT0007 INTO TABLE et_ZHCMT0007.

ENDFUNCTION.

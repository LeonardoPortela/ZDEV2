FUNCTION ZFI_GET_DATA_TIPO_DOC_VENDAS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_TVAKT STRUCTURE  TVAKT
*"----------------------------------------------------------------------

  SELECT *
    FROM TVAKT
    INTO TABLE et_TVAKT.

ENDFUNCTION.

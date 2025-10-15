FUNCTION zpm_get_media_consumo_frotas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZPMT0067 STRUCTURE  ZPMT0067
*"----------------------------------------------------------------------

  SELECT *
    FROM zpmt0067
    INTO TABLE et_zpmt0067.

ENDFUNCTION.

FUNCTION ZLES_MONITORA_001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NR_ANO) TYPE  ZDS_NR_ANO
*"----------------------------------------------------------------------

  PERFORM SHOW_ALV_NOMEACOES USING I_NR_ANO.

ENDFUNCTION.

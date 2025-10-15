FUNCTION ZMF_PESQ_INSTRU_OV_ALGO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VSTEL) TYPE  VSTEL
*"     REFERENCE(I_CHARG) TYPE  CHARG_D
*"     REFERENCE(I_FUNDO) TYPE  CHAR01 DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_ALGODAO_ALV) TYPE  ZSDT0001OV_ALGODAO_ALV_T
*"----------------------------------------------------------------------

  CLEAR: E_ALGODAO_ALV.

  PERFORM PESQ_ORDEM_VENDA USING I_FUNDO I_VSTEL I_CHARG CHANGING E_ALGODAO_ALV.

ENDFUNCTION.

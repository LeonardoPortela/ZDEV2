FUNCTION ZMF_RELA_INSTRU_OV_ALGO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NR_SAFRA) TYPE  ZDE_NR_SAFRA
*"     REFERENCE(I_ID_BUKRS) TYPE  ZDE_BUKRS_RECEB OPTIONAL
*"     REFERENCE(I_ID_BRANCH) TYPE  ZDE_BRANCH_RECEB OPTIONAL
*"----------------------------------------------------------------------

  PERFORM CALL_REPORT_INSTRUCAO_ALV USING I_NR_SAFRA I_ID_BUKRS I_ID_BRANCH.

ENDFUNCTION.

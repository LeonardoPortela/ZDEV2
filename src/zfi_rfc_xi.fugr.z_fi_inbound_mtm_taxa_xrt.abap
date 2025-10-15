FUNCTION Z_FI_INBOUND_MTM_TAXA_XRT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_MTM_FINAN_TAXAS STRUCTURE  ZFIT0071
*"----------------------------------------------------------------------
  DATA : WA_MTM_FINAN_TAXAS  TYPE ZFIT0071.

  LOOP AT T_MTM_FINAN_TAXAS INTO WA_MTM_FINAN_TAXAS.

    MODIFY ZFIT0071 FROM  WA_MTM_FINAN_TAXAS.

  ENDLOOP.




ENDFUNCTION.

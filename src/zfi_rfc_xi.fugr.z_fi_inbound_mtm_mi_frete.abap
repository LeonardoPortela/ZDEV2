FUNCTION Z_FI_INBOUND_MTM_MI_FRETE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_MTM_MI_FRETE STRUCTURE  ZFIT0050
*"----------------------------------------------------------------------

  DATA : WA_MTM_MI_FRETE  TYPE ZFIT0050.

  LOOP AT T_MTM_MI_FRETE INTO WA_MTM_MI_FRETE.

    MODIFY ZFIT0050 FROM WA_MTM_MI_FRETE.

  ENDLOOP.




ENDFUNCTION.

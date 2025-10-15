FUNCTION Z_FI_INBOUND_MTM_MI.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_MTM_MI STRUCTURE  ZFIT0049
*"----------------------------------------------------------------------


  DATA : WA_MTM_MI  TYPE ZFIT0049.

  LOOP AT T_MTM_MI INTO WA_MTM_MI.

    MODIFY ZFIT0049 FROM WA_MTM_MI.

  ENDLOOP.



ENDFUNCTION.

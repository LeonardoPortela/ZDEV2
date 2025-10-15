FUNCTION Z_FI_INBOUND_BLOOMBERG.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ZFIT0083 STRUCTURE  ZFIT0083
*"----------------------------------------------------------------------

  DATA : WA_T_ZFIT0083  TYPE ZFIT0083.

  " carrega todos os dados. Limpa a tabela sempre
  DELETE FROM ZFIT0083.


  LOOP AT T_ZFIT0083 INTO WA_T_ZFIT0083.

    MODIFY ZFIT0083 FROM WA_T_ZFIT0083.

  ENDLOOP.

ENDFUNCTION.

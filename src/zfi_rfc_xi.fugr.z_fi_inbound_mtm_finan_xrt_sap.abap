FUNCTION Z_FI_INBOUND_MTM_FINAN_XRT_SAP.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_MTM_FINAN STRUCTURE  ZFIT0059
*"--------------------------------------------------------------------

  INCLUDE zafl_macros.
**  ***initialize logger. It should be always on the top of the FUNCTION.
  /afl/log_init.


  DATA : wa_mtm_finan  TYPE zfit0059.

  " carrega todos os dados. Limpa a tabela sempre
  DELETE FROM zfit0059.


  LOOP AT t_mtm_finan INTO wa_mtm_finan.

    MODIFY zfit0059 FROM wa_mtm_finan.

  ENDLOOP.


**save logs. It should be always on the bottom of the FUNCTION.
  /afl/save.

ENDFUNCTION.


PROCESS BEFORE OUTPUT.
  MODULE status_2104.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zcte_motorista-lifnr.
    MODULE set_update_flag ON CHAIN-REQUEST.
    MODULE set_dados_lifnr ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_2104.


PROCESS BEFORE OUTPUT.
  MODULE cte_status_0104.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zcte_parceiros-reme_codigo.
    MODULE cte_muda_remetente ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zcte_parceiros-dest_codigo.
    MODULE cte_muda_destinatario ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_0104.

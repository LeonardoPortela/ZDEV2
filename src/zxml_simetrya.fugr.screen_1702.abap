
PROCESS BEFORE OUTPUT.

  MODULE status_1702.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zcte_seguro-respseg.
    FIELD zcte_seguro-resp_codigo.
    FIELD zcte_seguro-xseg.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_1702.

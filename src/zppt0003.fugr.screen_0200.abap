
PROCESS BEFORE OUTPUT.
  MODULE detail_init.
*
PROCESS AFTER INPUT.
  MODULE detail_exit_command AT EXIT-COMMAND.
  MODULE detail_set_pfstatus.
  CHAIN.
    FIELD zppt0003-werks .
    FIELD zppt0003-verid .
    FIELD zppt0003-matnr .
    FIELD zppt0003-peso_desc .
    FIELD zppt0003-usnam .
    FIELD zppt0003-data_atual .
    FIELD zppt0003-hora_atual .
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
  CHAIN.
    FIELD zppt0003-werks .
    FIELD zppt0003-verid .
    FIELD zppt0003-matnr .
    MODULE detail_pai.
  ENDCHAIN.

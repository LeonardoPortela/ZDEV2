
PROCESS BEFORE OUTPUT.
  MODULE detail_init.
*
PROCESS AFTER INPUT.
  MODULE detail_exit_command AT EXIT-COMMAND.
  MODULE detail_set_pfstatus.
  CHAIN.
    FIELD zsdt0034-numseqn .
    FIELD zsdt0034-bukrs .
    FIELD zsdt0034-branch .
    FIELD zsdt0034-smtp_addr .
    FIELD zsdt0034-status .
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
  CHAIN.
    FIELD zsdt0034-numseqn .
    MODULE detail_pai.
  ENDCHAIN.

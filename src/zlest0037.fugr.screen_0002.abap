PROCESS BEFORE OUTPUT.
  MODULE detail_init.
*
PROCESS AFTER INPUT.
  MODULE detail_exit_command AT EXIT-COMMAND.
  MODULE detail_set_pfstatus.
  CHAIN.
    FIELD zlest0037-matnr .
    FIELD zlest0037-bukrs .
    FIELD zlest0037-cd_modal .
    FIELD zlest0037-lifnr .
    FIELD zlest0037-matkl .
    FIELD zlest0037-operacao .
    FIELD zlest0037-ck_servico .
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
  CHAIN.
    FIELD zlest0037-matnr .
    FIELD zlest0037-bukrs .
    FIELD zlest0037-cd_modal .
    FIELD zlest0037-lifnr .
    FIELD zlest0037-matkl .
    FIELD zlest0037-operacao .
    MODULE detail_pai.
  ENDCHAIN.

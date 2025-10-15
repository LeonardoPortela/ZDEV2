PROCESS BEFORE OUTPUT.
  MODULE status_0102.
*
PROCESS AFTER INPUT.
  MODULE user_command_0102.
  MODULE exit_screen AT EXIT-COMMAND.

PROCESS ON VALUE-REQUEST. "F4

  FIELD wg_saida-zieme MODULE help_wg_saida-zieme.
  FIELD wg_saida-pmein MODULE help_wg_saida-zieme.
  FIELD wg_saida-zlsch MODULE help_wg_saida-zlsch.
  FIELD wg_saida-vkaus MODULE help_wg_saida-vkaus.

  FIELD wg_saida-tipo_calc    MODULE help_wg_saida-tipo_calc.
  FIELD wg_saida-preco        MODULE help_wg_saida-preco.
  FIELD wg_saida-c_decimais   MODULE help_wg_saida-c_decimais.
  FIELD wg_saida-param_espec  MODULE help_wg_saida-param_espec.
  FIELD wg_saida-status       MODULE help_wg_saida-status.

  " 05.07.2022 - RAMON - 76636 -->
  "FIELD wg_saida-kunnr      MODULE help_wg_saida-kunnr.
  " 05.07.2022 - RAMON - 76636 --<

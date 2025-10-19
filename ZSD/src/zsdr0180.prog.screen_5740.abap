PROCESS BEFORE OUTPUT.
  MODULE status_5740.
  MODULE status_5740_trata_cpf.
*
PROCESS AFTER INPUT.
  MODULE user_command_5740.

*-CS2019001891 - JT - 04.02.2021 - inicio
PROCESS ON VALUE-REQUEST.
  FIELD wa_header_cargad-cpf_rtc  MODULE z_help_cpf_rtc_5740.
*-CS2019001891 - JT - 04.02.2021 - fim

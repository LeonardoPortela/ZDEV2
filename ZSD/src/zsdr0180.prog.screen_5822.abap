PROCESS BEFORE OUTPUT.
  MODULE status_5822.
  MODULE status_5822_trata_cpf.
*
PROCESS AFTER INPUT.
  MODULE user_command_5822.

PROCESS ON VALUE-REQUEST.
  FIELD wa_header_cargad-cpf_rtc  MODULE z_help_cpf_rtc_5740.

PROCESS BEFORE OUTPUT.
  MODULE zm_status_0110.
*
PROCESS AFTER INPUT.
  MODULE zm_user_command_0110_exit AT EXIT-COMMAND.
  MODULE zm_user_command_0110.
*
PROCESS ON VALUE-REQUEST.
  FIELD eg_guia_agro-tpguia MODULE zm_srh_tp_guia_agro.
  FIELD eg_guia_agro-ufguia MODULE zm_srh_uf_guia_agro.

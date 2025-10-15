PROCESS BEFORE OUTPUT.
  MODULE zm_status_9100.
*
PROCESS AFTER INPUT.

  FIELD w_saida-id_lote.

  MODULE zm_id_lote. "ON REQUEST.

  MODULE zm_exit AT EXIT-COMMAND.

  MODULE zm_user_command_9100.

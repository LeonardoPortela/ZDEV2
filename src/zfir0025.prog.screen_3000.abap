
PROCESS BEFORE OUTPUT.
  MODULE status_3000.
*
PROCESS AFTER INPUT.
  MODULE user_command_3000.
  MODULE user_command_exit AT EXIT-COMMAND.

PROCESS ON VALUE-REQUEST.
  FIELD wg_cadliq-hbkid     MODULE search_banco.

" FIELD wg_cadliq-path      MODULE search_path.

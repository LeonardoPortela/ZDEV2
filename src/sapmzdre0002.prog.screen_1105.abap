
PROCESS BEFORE OUTPUT.
  MODULE status_1105.
*
PROCESS AFTER INPUT.
  MODULE user_command_1103_exit AT EXIT-COMMAND.
  MODULE user_command_1105.

process on value-request.
 field wg_file_local module busca_file.

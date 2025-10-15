PROCESS BEFORE OUTPUT.
  MODULE zm_status_0200.

PROCESS AFTER INPUT.
  MODULE zm_user_command_0100_exit AT EXIT-COMMAND.
  MODULE zm_user_command_0100.

PROCESS ON VALUE-REQUEST.
  FIELD *zlest0255-branch MODULE zm_srh_tp_filial_tarsp.

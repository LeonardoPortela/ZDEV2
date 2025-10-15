
PROCESS BEFORE OUTPUT.

  MODULE zm_status.

PROCESS AFTER INPUT.

  FIELD s_cabec-remet MODULE zm_remet ON REQUEST.

  MODULE zm_user_command.

  MODULE zm_exit_command AT EXIT-COMMAND.


PROCESS BEFORE OUTPUT.

  MODULE status_100.

  MODULE visibilidade_tabstrip_100.

  CALL SUBSCREEN: sub100 INCLUDING sy-repid vg_dynnr_tabstrip.

PROCESS AFTER INPUT.

  MODULE exit_aplicativo AT EXIT-COMMAND.

  MODULE user_command_100.

  CALL SUBSCREEN: sub100.

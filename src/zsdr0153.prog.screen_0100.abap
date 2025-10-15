PROCESS BEFORE OUTPUT.
  MODULE status_0100.

  MODULE tab_strip_set.

  CALL SUBSCREEN tab_strip_cli_ref INCLUDING g_tab_strip_cli-prog
                                             g_tab_strip_cli-subscreen.

*
PROCESS AFTER INPUT.
  MODULE tab_strip_get.
  MODULE user_command_0100.

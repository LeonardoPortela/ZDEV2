PROCESS BEFORE OUTPUT.
  MODULE status_0100.

  CALL SUBSCREEN main_tab_tab1_ref1 INCLUDING sy-cprog tl_tab.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: main_tab_tab1_ref1.

  MODULE user_command_0100.

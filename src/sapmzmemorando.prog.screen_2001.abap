
PROCESS BEFORE OUTPUT.

  MODULE status_2001.

  CALL SUBSCREEN: sub2001 INCLUDING sy-cprog vg_dynnr_cad,
                  sub2002 INCLUDING sy-cprog vg_dynnr_tab.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub2001,
                  sub2002.

  MODULE user_command_2001.

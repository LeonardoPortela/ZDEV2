
PROCESS BEFORE OUTPUT.
* MODULE STATUS_1001.
*
  CALL SUBSCREEN: sub0001 INCLUDING sy-cprog vg_dynnr_0001,
                  sub0002 INCLUDING sy-cprog vg_dynnr_0002.

PROCESS AFTER INPUT.

  MODULE user_command_1001.

  CALL SUBSCREEN: sub0001,
                  sub0002.

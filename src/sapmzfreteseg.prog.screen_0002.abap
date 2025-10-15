
PROCESS BEFORE OUTPUT.

  MODULE status_0002.
*
  CALL SUBSCREEN: sub1100 INCLUDING sy-cprog vg_dynnr_1100,
                  sub0003 INCLUDING sy-cprog vg_dynnr_0003.

PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0002.

  CALL SUBSCREEN: sub1100,
                  sub0003.

PROCESS BEFORE OUTPUT.
  MODULE status_5900.
  CALL SUBSCREEN: sub1 INCLUDING sy-repid '5901',
                  sub2 INCLUDING sy-repid '5902',
                  sub3 INCLUDING sy-repid '5903'.
*
PROCESS AFTER INPUT.
  CALL SUBSCREEN: sub1,
                  sub2,
                  sub3 .
  MODULE user_command_5900.

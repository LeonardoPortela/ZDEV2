
PROCESS BEFORE OUTPUT.
* MODULE STATUS_1000.
*
  CALL SUBSCREEN: sub1001 INCLUDING sy-cprog c_1001,
                  sub1002 INCLUDING sy-cprog c_1002.


PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub1001,
                  sub1002.

  MODULE user_command_1000.

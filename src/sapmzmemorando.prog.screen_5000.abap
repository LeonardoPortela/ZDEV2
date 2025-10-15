
PROCESS BEFORE OUTPUT.
* MODULE STATUS_5000.
*
  CALL SUBSCREEN: sub5001 INCLUDING sy-cprog c_5001,
                  sub5002 INCLUDING sy-cprog c_5002.


PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub5001,
                  sub5002.

  MODULE user_command_5000.

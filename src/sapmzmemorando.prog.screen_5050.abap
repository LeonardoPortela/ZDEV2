
PROCESS BEFORE OUTPUT.
* MODULE STATUS_5050.
*
  CALL SUBSCREEN: sub5051 INCLUDING sy-cprog c_5051,
                  sub5002 INCLUDING sy-cprog c_5002.


PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub5051,
                  sub5002.

  MODULE user_command_5050.

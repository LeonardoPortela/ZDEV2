
PROCESS BEFORE OUTPUT.

  MODULE status_8000.
*
  CALL SUBSCREEN: sub8001 INCLUDING sy-cprog c_8001,
                  sub8002 INCLUDING sy-cprog c_8002.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub8001,
                  sub8002.

  MODULE user_command_8000.

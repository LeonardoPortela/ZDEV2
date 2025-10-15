
PROCESS BEFORE OUTPUT.
* MODULE STATUS_8050.
*
  CALL SUBSCREEN: sub8051 INCLUDING sy-cprog c_8051,
                  sub8052 INCLUDING sy-cprog c_8052.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub8051,
                  sub8052.

  MODULE user_command_8050.

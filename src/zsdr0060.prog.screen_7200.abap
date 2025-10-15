PROCESS BEFORE OUTPUT.
  MODULE status_7200.

  CALL SUBSCREEN: sub1 INCLUDING sy-repid '7201',
                  sub2 INCLUDING sy-repid '7202',
                  sub3 INCLUDING sy-repid '7203',
                  sub4 INCLUDING sy-repid '7204',
                  sub5 INCLUDING sy-repid '7205'.

  MODULE pbo_7200_config_campos.

PROCESS AFTER INPUT.
  CALL SUBSCREEN: sub1,
                  sub2,
                  sub3,
                  sub4,
                  sub5.
  MODULE user_command_7200.


PROCESS BEFORE OUTPUT.
* MODULE STATUS_0700.
  CALL SUBSCREEN: subv01 INCLUDING sy-repid tela_0800,
                  subv02 INCLUDING sy-repid tela_0900,
                  "subv03 INCLUDING sy-repid tela_1000,
                  subv04 INCLUDING sy-repid tela_1100.



PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0700.

  CALL SUBSCREEN: subv01,
                  subv02,
                  "subv03,
                  subv04.


PROCESS BEFORE OUTPUT.
  CALL SUBSCREEN: subv01 INCLUDING sy-repid tela_0300,
                  subv02 INCLUDING sy-repid tela_0400,
                  subv03 INCLUDING sy-repid tela_0500.

  MODULE pbo.

PROCESS AFTER INPUT.
  CALL SUBSCREEN: subv01,
                  subv02,
                  subv03.
  MODULE pai.

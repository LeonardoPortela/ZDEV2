
PROCESS BEFORE OUTPUT.

  CALL SUBSCREEN: sub201 INCLUDING sy-repid '1200',
                  sub202 INCLUDING sy-repid '0202',
                  sub203 INCLUDING sy-repid '0203'.
*
PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub201,
                  sub202,
                  sub203.

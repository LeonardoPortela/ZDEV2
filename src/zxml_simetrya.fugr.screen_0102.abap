
PROCESS BEFORE OUTPUT.

  MODULE cte_status_0102.
*
  CALL SUBSCREEN: sub0100 INCLUDING sy-repid cte_dynnr_mod,
                  sub0101 INCLUDING sy-repid cte_dynnr_ifn.

PROCESS AFTER INPUT.

  MODULE user_command_0102.

  CALL SUBSCREEN: sub0100,
                  sub0101.

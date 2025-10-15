
PROCESS BEFORE OUTPUT.

  MODULE cte_status_2106.
*
  CALL SUBSCREEN: subciot INCLUDING sy-repid cte_dynnr_ciot.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: subciot.

  MODULE user_command_2106.

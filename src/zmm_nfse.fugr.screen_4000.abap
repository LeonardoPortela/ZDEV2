PROCESS BEFORE OUTPUT.

  CALL SUBSCREEN subscr_4050x INCLUDING sy-repid gv_subscr_4050x.
  CALL SUBSCREEN subscr_4051x INCLUDING sy-repid gv_subscr_4051x.

  MODULE status_4000.
  MODULE alv_4000_init.
  MODULE screen_control.
*** Inicio - ALX
*  MODULE set_iss_4000.
*** Fim - ALX

PROCESS AFTER INPUT.

  CALL SUBSCREEN subscr_4050x.
  CALL SUBSCREEN subscr_4051x.

  MODULE user_command_4000.

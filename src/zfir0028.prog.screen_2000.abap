
PROCESS BEFORE OUTPUT.
  CALL SUBSCREEN :
        su_bsidsel INCLUDING g_tab_strip_imp-prog '2100'.

  MODULE status_2000.
  MODULE cria_objetos.
*
PROCESS AFTER INPUT.
  MODULE user_command_2000.
  MODULE user_command_exit AT EXIT-COMMAND.

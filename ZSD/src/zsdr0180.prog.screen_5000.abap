PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TABSTRIP'
  MODULE tabstrip_active_tab_set.
  CALL SUBSCREEN tabstrip_sca
    INCLUDING g_tabstrip-prog g_tabstrip-subscreen.

  MODULE excluir_botoes.
  MODULE status_5000.

*
PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TABSTRIP'
  CALL SUBSCREEN tabstrip_sca.
  MODULE tabstrip_active_tab_get.

  MODULE user_command_5000_exit AT EXIT-COMMAND.
*MODULE USER_COMMAND_5000.

PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TAB_STRIP_DG'
  MODULE tab_strip_dg_active_tab_set.
  CALL SUBSCREEN tab_strip_dg_sca
    INCLUDING g_tab_strip_dg-prog g_tab_strip_dg-subscreen.
* MODULE STATUS_0110.
*
PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TAB_STRIP_DG'
  CALL SUBSCREEN tab_strip_dg_sca.
  MODULE tab_strip_dg_active_tab_get.
* MODULE USER_COMMAND_0110.


PROCESS BEFORE OUTPUT."
  MODULE inicializa_tela.
  MODULE tab_strip_imp_active_tab_set.
  MODULE trata_fields.

  CALL SUBSCREEN   tab_strip_imp_sca
  INCLUDING g_tab_strip_imp-prog g_tab_strip_imp-subscreen.

  MODULE status_0100.
  MODULE cria_objetos.
*
PROCESS AFTER INPUT.

  CALL SUBSCREEN tab_strip_imp_sca.
  MODULE tab_strip_imp_active_tab_get.

  FIELD wg_cadlan-nro_sol_cp MODULE limpa_tela   ON CHAIN-REQUEST.
  FIELD wg_cadlan-ped_forn   MODULE valida_ebeln ON CHAIN-REQUEST.
  MODULE user_command_0100.
  MODULE user_command_exit AT EXIT-COMMAND.

PROCESS ON VALUE-REQUEST.
  FIELD wg_cadlan-safra MODULE search_safra.
  FIELD wg_cadlan-zterm MODULE search_zterm.
  FIELD wg_cadlan-bsart MODULE search_bsart.
  FIELD wg_cadlan-bvtyp MODULE search_bvtyp.

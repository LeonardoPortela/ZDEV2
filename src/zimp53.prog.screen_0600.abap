PROCESS BEFORE OUTPUT.

  MODULE tb_strip_imp_act_set_cl.

  CALL SUBSCREEN   tab_strip_imp_cl1
  INCLUDING g_tab_strip_imp_cl-prog g_tab_strip_imp_cl-subscreen.

  MODULE status_0600.

  MODULE cria_objetos_cl.

PROCESS AFTER INPUT.

  CALL SUBSCREEN tab_strip_imp_cl1.
  MODULE tb_strip_imp_act_get_cl.

  MODULE user_command_0600.
  MODULE user_command_exit AT EXIT-COMMAND.

PROCESS ON VALUE-REQUEST.
  FIELD zimp_cad_lote-lote MODULE search_lote.

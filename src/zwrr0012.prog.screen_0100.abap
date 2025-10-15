
PROCESS BEFORE OUTPUT.
  MODULE carrega_operacoes.
  MODULE trata_fields.
  CALL SUBSCREEN :
    su_operacoes INCLUDING g_tab_strip_imp-prog '0110',
    su_estra INCLUDING g_tab_strip_imp-prog '0120',
    su_docs  INCLUDING g_tab_strip_imp-prog '0130'.

  MODULE status_0100.
  MODULE cria_objetos.

PROCESS AFTER INPUT.

  CALL SUBSCREEN:
                 su_operacoes,
                 su_estra,
                 su_docs.

  MODULE user_command_0100.
  MODULE user_command_exit AT EXIT-COMMAND.


PROCESS BEFORE OUTPUT.

  MODULE zm_status.

  LOOP AT t_t0014 INTO s_t0014 WITH CONTROL tc_t0014
    CURSOR tc_t0014-current_line.
    MODULE zm_trata_campos.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT t_t0014.

    CHAIN.
      FIELD: s_t0014-conhec,
             s_t0014-ctafrete,
             s_t0014-reimp,
             s_t0014-alterar,
             s_t0014-motivo
             MODULE z_atualiza_campos ON CHAIN-REQUEST.
    ENDCHAIN.

    FIELD: s_t0014-marc  MODULE z_marc  ON REQUEST,
           s_t0014-tknum MODULE z_tknum ON REQUEST.

  ENDLOOP.

  MODULE zm_user_command.

  MODULE zm_exit_command AT EXIT-COMMAND.

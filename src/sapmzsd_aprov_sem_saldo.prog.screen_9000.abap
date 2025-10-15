PROCESS BEFORE OUTPUT.
  MODULE zm_status_9000.
  MODULE zm_sbsc_set_active_tab.
  MODULE zm_seleciona_monta_dados.
  CALL SUBSCREEN sbscr_9000 INCLUDING eg_sbsc_9000-program
                                      eg_sbsc_9000-subscreen.
  CALL SUBSCREEN SBSCR_9000_AP INCLUDING eg_sbsc_9000-program
                                         eg_sbsc_9000-subscreen2.
*
PROCESS AFTER INPUT.
  MODULE zm_user_command_9000.

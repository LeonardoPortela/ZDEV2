
PROCESS BEFORE OUTPUT.

  MODULE zm_status.

  LOOP AT t_t0021 INTO s_t0021 WITH CONTROL tc_t0021
    CURSOR tc_t0021-current_line.
  ENDLOOP.


PROCESS AFTER INPUT.

  LOOP AT t_t0021.
    FIELD s_t0021-marc MODULE z_marc ON REQUEST.
  ENDLOOP.

  MODULE zm_user_command.

  MODULE zm_exit_command AT EXIT-COMMAND.

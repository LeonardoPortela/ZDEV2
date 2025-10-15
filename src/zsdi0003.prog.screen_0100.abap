
PROCESS BEFORE OUTPUT.

  MODULE zm_status.

  MODULE zm_obj_alv.

  LOOP AT t_calc INTO s_calc WITH CONTROL tc_frame
    CURSOR tc_frame-current_line.
  ENDLOOP.


PROCESS AFTER INPUT.

  LOOP AT t_calc.
    FIELD s_calc-marc MODULE zm_marc ON REQUEST.
  ENDLOOP.

  MODULE zm_user_command.

  MODULE zm_exit_command AT EXIT-COMMAND.

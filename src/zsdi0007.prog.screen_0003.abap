PROCESS BEFORE OUTPUT.
* MODULE STATUS_0003.
*
  LOOP AT   t_vincu
       WITH CONTROL tab_vinc
       CURSOR tab_vinc-current_line.
  ENDLOOP.

PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0003.

  LOOP AT t_vincu.
    FIELD t_vincu-mark
      MODULE tab_vinc_mark ON REQUEST.
  ENDLOOP.

  MODULE tab_vinc_user_command.

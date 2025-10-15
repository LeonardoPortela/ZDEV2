
PROCESS BEFORE OUTPUT.

  LOOP AT   t_notas
       WITH CONTROL tab_notas
       CURSOR tab_notas-current_line.
  ENDLOOP.

  CALL SUBSCREEN: sub0003 INCLUDING sy-cprog '0001'.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub0003.

  LOOP AT t_notas.
    FIELD t_notas-mark
      MODULE tab_notas_mark ON REQUEST.
  ENDLOOP.

  MODULE tab_notas_user_command.

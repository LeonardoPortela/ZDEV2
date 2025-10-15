PROCESS BEFORE OUTPUT.

  MODULE status_9000.

  MODULE screen_control_9000.

  MODULE set_cursor.

  MODULE display_alv_9000.

*  LOOP AT gt_0005 INTO zsde0005 WITH CONTROL grd_zsde0005.
*
*  ENDLOOP.

*  LOOP AT gt_0006 INTO zsde0006 WITH CONTROL grd_zsde0006.
*
*  ENDLOOP.

PROCESS AFTER INPUT.

*  LOOP AT gt_0005.
*    MODULE selec_change.
*    "FIELD zsde0005-selec MODULE selec_change ON INPUT.
*  ENDLOOP.

*  LOOP AT gt_0006.
*
*  ENDLOOP.

  MODULE user_command_9000.

*----------------------------------------------------------------------*
***INCLUDE LZSD_ZSDT0132O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.

  SET TITLEBAR '9000' WITH gv_title.

*  DESCRIBE TABLE gt_0005 LINES grd_zsde0005-lines.
*
*  IF grd_zsde0005-lines = 0.
*    grd_zsde0005-top_line = 1.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_cursor OUTPUT.

  IF gv_cursor IS NOT INITIAL.
    SET CURSOR FIELD gv_cursor.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.

  SET PF-STATUS '9100'.
  SET TITLEBAR '9100' WITH gv_title.

  LOOP AT SCREEN.

    IF screen-group1 = 'RB1'.

      IF gv_9100_hide = 'X'.

        screen-input = 0.

        MODIFY SCREEN.

      ENDIF.

    ENDIF.


  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE screen_control_9000 OUTPUT.

  LOOP AT SCREEN.

    IF screen-name = 'ZSDE0004-MATNR'.

      screen-input = 0.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9200 OUTPUT.

  SET PF-STATUS '9200'.

  IF gv_bukrs IS INITIAL OR gv_renew = 'X'.

    CALL FUNCTION 'ZSD_BEFORE_ZSDT0132'
      EXPORTING
        i_reiniciar = gv_renew
      IMPORTING
        e_bukrs     = gv_bukrs
        e_matnr     = gv_matnr.

  ENDIF.

  SET TITLEBAR '9000' WITH gv_bukrs gv_matnr.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9300 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '9000' WITH gv_bukrs gv_matnr.

  DESCRIBE TABLE gt_0005 LINES grd_zsde0005-lines.

  IF grd_zsde0005-lines = 0.
    grd_zsde0005-top_line = 1.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_9300  OUTPUT
*&---------------------------------------------------------------------*
MODULE display_alv_9000 OUTPUT.

  PERFORM f_display_alv_gt_005.
  PERFORM f_display_alv_gt_006.

ENDMODULE.

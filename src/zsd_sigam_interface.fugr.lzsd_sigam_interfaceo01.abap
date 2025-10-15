*----------------------------------------------------------------------*
***INCLUDE LZSD_SIGAM_INTERFACEO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '9000' WITH gv_param-titulo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE screen_9000 OUTPUT.

  st_btn_01-icon_id = icon_search .
  st_btn_01-text = gv_param-titulo_btn_1.
  st_btn_01-icon_text = gv_param-titulo_btn_1.

  st_btn_02-icon_id = icon_create .
  st_btn_02-text = gv_param-titulo_btn_2.
  st_btn_02-icon_text = gv_param-titulo_btn_2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.

  SET PF-STATUS '9100'.

  SET TITLEBAR '9100' WITH gv_param-titulo.

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
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9200 OUTPUT.

  SET PF-STATUS '9200'.

  SET TITLEBAR '9000' WITH gv_param-titulo.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE screen_control_9200 OUTPUT.

  LOOP AT SCREEN.

    IF screen-name = 'ZSDE0004-MATNR'.

      screen-input = 0.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

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
*&      Module  DISPLAY_ALV_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE display_alv_9200 OUTPUT.

  PERFORM f_display_alv_gt_005.
  PERFORM f_display_alv_gt_006.

ENDMODULE.

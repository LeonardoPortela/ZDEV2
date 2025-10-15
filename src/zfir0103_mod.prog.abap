*&---------------------------------------------------------------------*
*& Include ZFIR0103_MOD
*&---------------------------------------------------------------------*



MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLEBAR_0100'.
  PERFORM action_process.
ENDMODULE.


MODULE user_command_0100 OUTPUT.

  PERFORM action_process.

ENDMODULE.

FORM action_process.

  CASE sy-ucomm.
    WHEN 'ONLI'.

    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
*    WHEN 'REFRESH'.
*      gr_table->refresh( ).
*      gr_table2->refresh( ).
    WHEN 'VOLTAA'.
      SUBMIT ZFIR0100_volta_a
      AND RETURN.
  ENDCASE.

ENDFORM.

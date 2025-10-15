*&---------------------------------------------------------------------*
*& Include          ZGLR081_MOD
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
  ENDCASE.

ENDFORM.

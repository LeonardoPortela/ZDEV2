*&---------------------------------------------------------------------*
*& Include ZFIR0100_MOD
*&---------------------------------------------------------------------*


MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLEBAR_0100'.
  PERFORM action_process.
ENDMODULE.


FORM action_process.

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
      EXIT.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

*----------------------------------------------------------------------*
***INCLUDE MZREMZARM9999I .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9999  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9999 INPUT.

  CASE ok_code.
    WHEN 'QUIT'.
      CLEAR ok_code.
  ENDCASE.

  CLEAR ok_code.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " USER_COMMAND_9999  INPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_LOG_MSG'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_LOG_MSG_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_MESSAGEM LINES TAB_LOG_MSG-lines.
ENDMODULE.

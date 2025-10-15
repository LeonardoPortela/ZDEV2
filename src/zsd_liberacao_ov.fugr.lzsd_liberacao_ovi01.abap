*----------------------------------------------------------------------*
***INCLUDE LZSD_LIBERACAO_OVI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9090  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9090 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRMAR'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCE'.
      gv_canc = abap_true.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

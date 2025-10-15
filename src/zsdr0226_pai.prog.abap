*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM SALVAR_REG.
    WHEN 'LEGEN'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 56 12.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

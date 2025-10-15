*&---------------------------------------------------------------------*
*&  Include           ZLESR0120_PAI
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'CLOCK'.
      LEAVE TO SCREEN 0100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS.

      LEAVE TO SCREEN 0100.

  ENDCASE.

ENDMODULE.

MODULE CANCEL_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.

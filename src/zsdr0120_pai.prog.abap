*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_PAI
*&---------------------------------------------------------------------*

MODULE PAI_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.

      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.

  ENDCASE.

ENDMODULE.

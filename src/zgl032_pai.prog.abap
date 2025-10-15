*&---------------------------------------------------------------------*
*&  Include           ZGL032_PAI
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
    WHEN 'REFRESH'.
      PERFORM SELECIONA_DADOS.
      IF VG_NOT_FOUND IS NOT INITIAL.
        EXIT.
      ENDIF.
      PERFORM PROCESSA_DADOS.
      PERFORM IMPRIME_DADOS.
  ENDCASE.

ENDMODULE.

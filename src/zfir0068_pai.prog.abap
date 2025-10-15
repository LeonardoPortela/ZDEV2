*&---------------------------------------------------------------------*
*&  Include           ZFIR0068_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'SEA'.
      PERFORM: SELECIONA_DADOS.

      IF VG_RET_CONSULTA IS NOT INITIAL.
        PERFORM: PROCESSA_DADOS.
      ENDIF.

      PERFORM REFRESH_ALV.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      PERFORM GRAVAR_LCTOS.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT

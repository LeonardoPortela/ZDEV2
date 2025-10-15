*----------------------------------------------------------------------*
***INCLUDE ZSDR0051_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'SEA'.
       PERFORM: SELECIONA_DADOS,
                PROCESSA_DADOS.
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

  DATA: V_IMPORTADO TYPE C,
        V_MSG       TYPE TY_CHAVES_ERRO-MSG.

  CASE SY-UCOMM.
    WHEN 'IMP_XML'.
      PERFORM LER_DIRETORIO.
    WHEN 'PSQ_FILIAL'.
      PERFORM PSQ_FILIAL.
    WHEN 'CONFIRM'.
      PERFORM GRAVAR_DOC CHANGING V_IMPORTADO
                                  V_MSG.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT

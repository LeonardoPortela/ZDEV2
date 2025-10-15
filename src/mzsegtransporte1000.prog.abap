*----------------------------------------------------------------------*
***INCLUDE MZSEGTRANSPORTE1000.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.

  IF VG_TELA_1000 IS INITIAL.
    DT_INICIAL_VALIDADE = SY-DATUM.
    PERFORM PESQUISAR_FILTRAR_APOLICES.
    VG_TELA_1000 = TL_1100.
  ENDIF.

  CASE VG_TELA_1000.
    WHEN TL_1100.
      SET PF-STATUS 'PF1100'.
      SET TITLEBAR  'TL1100'.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.

  "CK_OPERACAO  TYPE CHAR01.  "I - Inclusão / A - Modificação / C - Consulta

  CASE OK_CODE.
    WHEN OK_PESQUISAR.
      PERFORM PESQUISAR_APOLICES.
      CLEAR: OK_CODE.
    WHEN OK_INCLUIR.
      PERFORM CADASTRAR_APOLICE.
      CLEAR: OK_CODE.
    WHEN OK_MODIFICAR.
      PERFORM EDITAR_APOLICE.
      CLEAR: OK_CODE.
    WHEN OK_CONSULTAR.
      PERFORM ABRIR_APOLICE.
    WHEN OK_COPIA.
      PERFORM COPIAR_APOLICE.
      CLEAR: OK_CODE.
    WHEN OK_ATIVAR.
      PERFORM ATIVAR_APOLICE.
      CLEAR: OK_CODE.
    WHEN OK_EXCLUIR.
      PERFORM EXCLUIR_APOLICE.
      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000_EXIT INPUT.
  LEAVE PROGRAM.
ENDMODULE.

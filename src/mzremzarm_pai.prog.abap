*----------------------------------------------------------------------*
***INCLUDE MZREMZARM_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_3000 INPUT.
  CASE OK_CODE.
    WHEN C_BACKR OR C_EXITR OR C_CANCELR.
      CLEAR: OK_CODE.
      CLEAR: IT_MESSAGEM[].
      VG_DYNNR_000 = C_1000.
      SET SCREEN 0.
    WHEN C_ROSAIDA.
      CLEAR: OK_CODE.
      PERFORM REGISTRAR_MOVIMENTO_MERCADORIA.
    WHEN C_ROLOGSA.
      CLEAR: OK_CODE.
      PERFORM CHAMA_LOG.
    WHEN C_RODOCM.
      PERFORM CHAMA_DOC_MATERIAL.
    WHEN C_ROESTORNO.
      CLEAR: OK_CODE.
      PERFORM CHAMA_ESTORNO.
  ENDCASE.
ENDMODULE.

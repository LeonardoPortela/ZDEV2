*----------------------------------------------------------------------*
***INCLUDE ZMMR126_USER_COMMAND_0001.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

  CASE OK_CODE.
    WHEN 'PESQUISAR'.

        CALL SCREEN 0100 STARTING AT 20 02.
        CLEAR: OK_CODE.

    WHEN 'SELECAOPR'.
      PERFORM SELECAO_PADRAO.
      CLEAR: OK_CODE.
    WHEN 'NOVA_CARGA'.
      PERFORM NOVA_CARGA.
      CLEAR: OK_CODE.
    WHEN 'REP_00001'.
      NM_REPORT = 'MM_0001'.
      CALL SCREEN 9005.
      CLEAR: OK_CODE.
    WHEN 'REP_00002'.
      NM_REPORT = 'MM_0001'.
      NM_ATUAL  = ABAP_TRUE.
      CALL SCREEN 9005.
      CLEAR: OK_CODE.
    WHEN 'NOVA_SOLIC'.
      SUBMIT ZMMR128 WITH PSAFRA   EQ PSAFRA
                     WITH PEMPRE   EQ PEMPRE
                     WITH PFILIA   EQ PFILIA AND RETURN.
      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXIT INPUT.
  LEAVE PROGRAM.
ENDMODULE.

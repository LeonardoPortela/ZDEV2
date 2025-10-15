*----------------------------------------------------------------------*
***INCLUDE MZSEGTRANSPORTE1132.
*----------------------------------------------------------------------*

DATA: CK_CONFIRMADO_1132 TYPE CHAR01,
      CK_ALTEROU_1132    TYPE CHAR01.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1132_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1132_EXIT INPUT.
  CLEAR OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1132  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1132 INPUT.

  CASE OK_CODE.
    WHEN OK_ADICIONAR.
      CLEAR OK_CODE.

      CHECK CK_ALTEROU_1132 EQ ABAP_FALSE.

      READ TABLE IT_ZLEST0116_ALV WITH KEY CD_EMPRESA = ZDE_ZLEST0116_ALV-CD_EMPRESA
                                           CD_GRUPO   = ZDE_ZLEST0116_ALV-CD_GRUPO.
      IF SY-SUBRC IS INITIAL.
        MESSAGE S005.
        EXIT.
      ENDIF.

      CK_CONFIRMADO_1132 = ABAP_TRUE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_GRUPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INCLUIR_GRUPO .

  CLEAR: ZDE_ZLEST0116_ALV.
  ZDE_ZLEST0116_ALV-CD_APOLICE = ZDE_ZLEST0115_ALV-CD_APOLICE.
  CK_CONFIRMADO_1132 = ABAP_FALSE.

  CALL SCREEN 1132 STARTING AT 10 5.

  IF CK_CONFIRMADO_1132 EQ ABAP_TRUE.
    APPEND ZDE_ZLEST0116_ALV TO IT_ZLEST0116_ALV.
    PERFORM ATUALIZA_TELA_1131.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1132  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1132 OUTPUT.
  SET PF-STATUS 'PF1132'.
  SET TITLEBAR 'TL1132'.

  CK_ALTEROU_1132 = ABAP_FALSE.

  IF ZDE_ZLEST0116_ALV-CD_EMPRESA IS NOT INITIAL.
    SELECT SINGLE BUTXT INTO ZDE_ZLEST0116_ALV-DS_EMPRESA
      FROM T001
     WHERE BUKRS EQ ZDE_ZLEST0116_ALV-CD_EMPRESA.
  ENDIF.

  IF ZDE_ZLEST0116_ALV-CD_GRUPO IS NOT INITIAL.
    SELECT SINGLE WGBEZ INTO ZDE_ZLEST0116_ALV-DS_GRUPO
        FROM T023T
       WHERE SPRAS EQ SY-LANGU
         AND MATKL EQ ZDE_ZLEST0116_ALV-CD_GRUPO.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_1132  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_1132 INPUT.
  CK_ALTEROU_1132 = ABAP_TRUE.
ENDMODULE.

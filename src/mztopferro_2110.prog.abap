*----------------------------------------------------------------------*
***INCLUDE MZTOPFERRO_2110.
*----------------------------------------------------------------------*

DATA: CK_ALTEROU_2110 TYPE C LENGTH 1.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_2110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_2110 INPUT.
  CK_ALTEROU_2110 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2110 INPUT.

  CASE OK_CODE.
    WHEN OK_SALVAR.

      CLEAR OK_CODE.

      CHECK CK_ALTEROU_2110 EQ ABAP_FALSE.

      "Não permitir no mesmo período um preço do mesmo fornecedor
      SELECT SINGLE * INTO WA_ZLEST0118
        FROM ZLEST0118
       WHERE LIFNR            EQ ZDE_ZLEST0118_ALV-LIFNR
         AND PAIS             EQ ZDE_ZLEST0118_ALV-PAIS
         AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0118_ALV-DOMICILIO_ORIGEM
         AND DOMICILIO_DESTIN EQ ZDE_ZLEST0118_ALV-DOMICILIO_DESTIN.

      IF SY-SUBRC IS INITIAL.
        MESSAGE S010.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING ZDE_ZLEST0118_ALV TO WA_ZLEST0118.
      MODIFY ZLEST0118 FROM WA_ZLEST0118.
      COMMIT WORK.

      MESSAGE S006.

      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2110 OUTPUT.

  CLEAR: IT_COMANDOS, IT_COMANDOS[].

  CASE CK_OPERACAO.
    WHEN 'I'.
      SET TITLEBAR 'TL2110' WITH TEXT-007.
    WHEN 'A'.
      SET TITLEBAR 'TL2110' WITH TEXT-008.
    WHEN 'C'.
      SET TITLEBAR 'TL2110' WITH TEXT-009.
      APPEND OK_SALVAR TO IT_COMANDOS.
  ENDCASE.

  SET PF-STATUS 'PF1130' EXCLUDING IT_COMANDOS.

  CK_ALTEROU_2110 = ABAP_FALSE.

  IF ZDE_ZLEST0118_ALV-LIFNR IS NOT INITIAL.
    SELECT SINGLE NAME1 INTO ZDE_ZLEST0118_ALV-NAME1 FROM LFA1 WHERE LIFNR = ZDE_ZLEST0118_ALV-LIFNR.
  ENDIF.

  CLEAR: ZDE_ZLEST0118_ALV-TEXT_ORIGEM,
         ZDE_ZLEST0118_ALV-TEXT_DESTINO.

  IF ZDE_ZLEST0118_ALV-DOMICILIO_ORIGEM IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0118_ALV-TEXT_ORIGEM
      FROM J_1BTXJURT
     WHERE SPRAS   EQ SY-LANGU
       AND COUNTRY EQ ZDE_ZLEST0118_ALV-PAIS
       AND TAXJURCODE EQ ZDE_ZLEST0118_ALV-DOMICILIO_ORIGEM.
  ENDIF.

  IF ZDE_ZLEST0118_ALV-DOMICILIO_DESTIN IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0118_ALV-TEXT_DESTINO
      FROM J_1BTXJURT
     WHERE SPRAS   EQ SY-LANGU
       AND COUNTRY EQ ZDE_ZLEST0118_ALV-PAIS
       AND TAXJURCODE EQ ZDE_ZLEST0118_ALV-DOMICILIO_DESTIN.
  ENDIF.

  IF CK_OPERACAO EQ 'C'.
    LOOP AT SCREEN.
      IF SCREEN-NAME(17) EQ 'ZDE_ZLEST0118_ALV'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2110_EXIT INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2110_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

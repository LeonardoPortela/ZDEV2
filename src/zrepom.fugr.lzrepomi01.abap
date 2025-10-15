*----------------------------------------------------------------------*
***INCLUDE LZREPOMI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  DATA: CK_VALIDO TYPE CHAR01.

  CASE OK_CODE.
    WHEN OK_CONFIRMAR.
      CLEAR: OK_CODE.
      CALL FUNCTION 'Z_REPOM_VALIDA_CARTAO'
        EXPORTING
          I_CARTAO = ZDE_REPOM_CARTAO "WA_REPOM_CARTAO
        IMPORTING
          E_VALIDO = CK_VALIDO
          E_ERROS  = E_ERROS
        EXCEPTIONS
          ERRO     = 1
          OTHERS   = 2.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE SY-MSGTY.
      ELSEIF CK_VALIDO EQ ABAP_FALSE.
        LOOP AT E_ERROS INTO WA_ERRO.
          MESSAGE W017(ZREPOM) WITH WA_ERRO-ERRO_CODIGO WA_ERRO-ERRO_DESCRICAO.
        ENDLOOP.
      ELSEIF CK_VALIDO EQ ABAP_TRUE.
        CK_INFORMADO = ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0103 OUTPUT.
  SET PF-STATUS 'PF0103'.
  SET TITLEBAR 'TL0103'.

  ZDE_ZLEST0122_ALV-CD_PAIS = 'BR'.

  IF ZDE_ZLEST0122_ALV-CD_CID_ORIGEM IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0121_ALV-DS_CID_ORIGEM
      FROM J_1BTXJURT
     WHERE SPRAS EQ SY-LANGU
       AND COUNTRY EQ ZDE_ZLEST0122_ALV-CD_PAIS
       AND TAXJURCODE EQ ZDE_ZLEST0122_ALV-CD_CID_ORIGEM.
  ENDIF.

  IF ZDE_ZLEST0122_ALV-CD_CID_DESTINO IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0122_ALV-DS_CID_DESTINO
      FROM J_1BTXJURT
     WHERE SPRAS EQ SY-LANGU
       AND COUNTRY EQ ZDE_ZLEST0122_ALV-CD_PAIS
       AND TAXJURCODE EQ ZDE_ZLEST0122_ALV-CD_CID_DESTINO.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103_EXIT INPUT.
  CLEAR: CK_ALTEROU_PERCURSO.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103 INPUT.
  CHECK CK_ALTEROU_0100 EQ ABAP_FALSE.

  CASE OK_CODE.
    WHEN OK_GRAVAR.
      CK_ALTEROU_PERCURSO = ABAP_TRUE.
      CLEAR OK_CODE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

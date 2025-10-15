*----------------------------------------------------------------------*
***INCLUDE MZREPOM_9999.
*----------------------------------------------------------------------*

TABLES: ZDE_REPOM_CARTAO.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9999_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9999_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9999  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9999 OUTPUT.
  SET PF-STATUS 'PF9999'.
  SET TITLEBAR 'TL9999'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9999  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9999 INPUT.

  DATA: CK_VALIDO TYPE CHAR01,
        E_ERROS   TYPE ZDE_REPOM_ERROS_T,
        WA_ERRO   TYPE ZDE_REPOM_ERROS.

  CASE OK_CODE.
    WHEN OK_VALIDAR.

      CALL FUNCTION 'Z_REPOM_VALIDA_CARTAO'
        EXPORTING
          I_CARTAO = ZDE_REPOM_CARTAO
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
          MESSAGE W017 WITH WA_ERRO-ERRO_CODIGO WA_ERRO-ERRO_DESCRICAO.
        ENDLOOP.
      ENDIF.

      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_9002.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.

  SET PF-STATUS 'PF9002'.
  SET TITLEBAR 'TL9002'.

  IF WA_ADD_NFE_9002-N55_CHAVE_ACESSO IS NOT INITIAL.
    PERFORM BUSCAR_INDO_NOTA_DIGITADA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002_EXIT INPUT.
  CLEAR: OK_CODE.
  WA_ADD_NFE_9002-CK_INCLUIR = ABAP_FALSE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CHAVE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUI_INFO_CHAVE INPUT.
  CK_ALTERADO_CHAVE = ABAP_TRUE.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.
      CLEAR: OK_CODE.
      PERFORM BUSCAR_INDO_NOTA_DIGITADA.

      IF CK_ALTERADO_CHAVE EQ ABAP_TRUE.
        CK_ALTERADO_CHAVE = ABAP_FALSE.
        EXIT.
      ENDIF.

      IF WA_ADD_NFE_9002-CK_INCLUIR EQ ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

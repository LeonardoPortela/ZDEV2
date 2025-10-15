*&---------------------------------------------------------------------*
*&  Include           ZLESR0100_0002
*&---------------------------------------------------------------------*

DATA: CK_ALTEROU TYPE C LENGTH 1.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0002 OUTPUT.
  SET PF-STATUS 'PF0002'.
  SET TITLEBAR 'TL0002'.

  IF CK_PRIMEIRA_ENTRADA EQ ABAP_TRUE.
    CLEAR: CK_ALTEROU, CK_PRIMEIRA_ENTRADA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002_EXIT INPUT.
  CLEAR: OK_CODE.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.
  CASE OK_CODE.
    WHEN OK_SALVAR.

      IF CK_ALTEROU EQ ABAP_TRUE.
        CLEAR: OK_CODE.
        MODIFY ZLEST0032.
        CK_GRAVOU = ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_UPDATE_FLAG INPUT.
  CK_ALTEROU = ABAP_TRUE.

ENDMODULE.

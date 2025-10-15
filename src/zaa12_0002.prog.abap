*&---------------------------------------------------------------------*
*&  Include           ZAA10_0002
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
  "CLEAR: OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.
  CASE OK_CODE.
    WHEN 'SALVAR'.
      IF CK_ALTEROU EQ ABAP_TRUE.
        "CLEAR: OK_CODE.
        PERFORM SALVAR_DATA.
        CK_GRAVOU = ABAP_TRUE.
        CLEAR ZAA006-GJAHR.
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
*&---------------------------------------------------------------------*
*&      Form  SALVAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALVAR_DATA .

*  LOOP AT IT_SELECT INTO WA_SELECT.
*    DELETE ZAA004 FROM WA_SELECT.
*  ENDLOOP.

*  LOOP AT IT_SELECT INTO WA_SELECT.
*    WA_SELECT-PRAZO_AA = ZAA004-PRAZO_AA.
*    MODIFY ZAA004 FROM WA_SELECT.
*  ENDLOOP.

  LOOP AT IT_SELECT INTO WA_SELECT.

    WA_SELECT-GJAHR = ZAA006-GJAHR.
    APPEND WA_SELECT TO IT_ZAA006.

  ENDLOOP.

ENDFORM.

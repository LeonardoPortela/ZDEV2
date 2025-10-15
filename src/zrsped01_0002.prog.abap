*&---------------------------------------------------------------------*
*&  Include           ZRSPED01_0002
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
  CLEAR: CK_ALTEROU. "CK_PRIMEIRA_ENTRADA.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.
  CASE SY-UCOMM.
    WHEN 'SALVAR'.
      IF CK_ALTEROU EQ ABAP_TRUE.
        PERFORM SALVAR_DATA.
        CK_GRAVOU = ABAP_TRUE.
        CLEAR ZSPED002-BUKRS.
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

  LOOP AT IT_SELECT INTO WA_SELECT.

    WA_SELECT-BUKRS = ZSPED002-BUKRS.
    IF ZSPED002-VERSN IS NOT INITIAL.
      WA_SELECT-VERSN = ZSPED002-VERSN.
    ENDIF.
    CLEAR: WA_SELECT-NR_PARAMETRO.
    APPEND WA_SELECT TO IT_ZSPED002.

  ENDLOOP.

ENDFORM.

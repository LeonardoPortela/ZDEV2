*----------------------------------------------------------------------*
***INCLUDE ZGLT068_0002.
*----------------------------------------------------------------------*

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
        ZGLT_DRE_04-DS_USUARIO   = SY-UNAME.
        ZGLT_DRE_04-DT_ALTERACAO = SY-DATUM.
        ZGLT_DRE_04-HR_ALTERACAO = SY-UZEIT.
        MODIFY ZGLT_DRE_04.
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

*&---------------------------------------------------------------------*
*&      Module  ALTERA_KOSAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTERA_KOSAR INPUT.
  IF ZGLT_DRE_04-KOSTL IS NOT INITIAL.
    SELECT SINGLE KOSAR INTO ZGLT_DRE_04-KOSAR
      FROM CSKS
     WHERE KOKRS EQ ZGLT_DRE_04-KOKRS
       AND KOSTL EQ ZGLT_DRE_04-KOSTL
       AND DATBI GE SY-DATUM.
  ELSE.
    CLEAR: ZGLT_DRE_04-KOSAR.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_KOSAR_V  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTERA_KOSAR_V INPUT.

  DATA: WA_CSKS TYPE CSKS.

  IF ZGLT_DRE_04-KOSTL IS NOT INITIAL.

    SELECT SINGLE * INTO WA_CSKS
      FROM CSKS
     WHERE KOKRS EQ ZGLT_DRE_04-KOKRS
       AND KOSTL EQ ZGLT_DRE_04-KOSTL
       AND DATBI GE SY-DATUM.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E059 WITH ZGLT_DRE_04-KOSTL ZGLT_DRE_04-KOKRS.
    ENDIF.

  ENDIF.

ENDMODULE.

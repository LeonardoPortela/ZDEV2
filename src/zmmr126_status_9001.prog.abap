*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9001.
*----------------------------------------------------------------------*
TABLES: ZSDT0001US.

DATA: LC_NAME TYPE NAME1.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.

  IF ZSDT0001US-TP_CARGA IS INITIAL.
    ZSDT0001US-TP_CARGA = ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB.
  ENDIF.

  IF LC_NAME IS INITIAL AND PFILIA IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(WA_J_1BBRANCH)
      FROM J_1BBRANCH
     WHERE BRANCH EQ @PFILIA.
    PEMPRE  = WA_J_1BBRANCH-BUKRS.
    LC_NAME = WA_J_1BBRANCH-NAME.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  CASE OK_CODE.
    WHEN 'SELECIONAR'.
      CHECK LC_NAME IS NOT INITIAL.
      CK_SELECIONOU = ABAP_TRUE.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001US)
        FROM ZSDT0001US
       WHERE US_NAME EQ @SY-UNAME.

      WA_ZSDT0001US-US_NAME   = SY-UNAME.
      WA_ZSDT0001US-NR_SAFRA  = PSAFRA.
      WA_ZSDT0001US-ID_BUKRS  = PEMPRE.
      WA_ZSDT0001US-ID_BRANCH = PFILIA.
      WA_ZSDT0001US-TP_CARGA  = ZSDT0001US-TP_CARGA.
      PTIPCA = ZSDT0001US-TP_CARGA.
      MODIFY ZSDT0001US FROM WA_ZSDT0001US.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_PSAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDAR_PSAFRA INPUT.

  DATA: PNO_INI TYPE I,
        PNO_FIM TYPE I.

  PNO_INI = SY-DATUM(4).
  PNO_FIM = SY-DATUM(4).
  ADD -1 TO PNO_INI.
  ADD  1 TO PNO_FIM.

  IF PSAFRA LT PNO_INI OR
     PSAFRA GT PNO_FIM.
    MESSAGE E002 WITH PSAFRA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_PFILIA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDAR_PFILIA INPUT.

  CLEAR: LC_NAME.

  SELECT SINGLE * INTO WA_J_1BBRANCH
    FROM J_1BBRANCH
   WHERE BRANCH EQ PFILIA.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE E003 WITH PFILIA.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
    ID 'WERKS' FIELD  PFILIA
    ID 'ACTVT' FIELD '03'.    "Alteração

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E004.
    WHEN 12.
      MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WHEN OTHERS.
  ENDCASE.

  PEMPRE  = WA_J_1BBRANCH-BUKRS.

  CLEAR: WA_J_1BBRANCH.

ENDMODULE.

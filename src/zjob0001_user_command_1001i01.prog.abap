*----------------------------------------------------------------------*
***INCLUDE ZJOB0001_USER_COMMAND_1001I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.
  IF BT = '&NOVO'.
    IF SY-UCOMM = 'SAVE'.
      READ TABLE IT_SAIDA WITH KEY JOBNAME = WA_ZJOB0001-JOBNAME TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        MESSAGE 'Este JOB já existe.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        WA_ZJOB0001-USNAM = SY-UNAME.
        WA_ZJOB0001-DT_ATUAL = SY-DATUM.
        WA_ZJOB0001-HR_ATUAL = SY-UZEIT.
        MODIFY ZJOB0001 FROM WA_ZJOB0001.
        CLEAR: IT_SAIDA,
               WA_ZJOB0001.
        PERFORM SELECT_TAB.
        MESSAGE 'JOB salvo com sucesso.' TYPE 'S'.
      ENDIF.
    ENDIF.
  ELSEIF BT = '&MODIFICAR'.
    IF SY-UCOMM = 'SAVE'.
      WA_ZJOB0001-USNAM = SY-UNAME.
      WA_ZJOB0001-DT_ATUAL = SY-DATUM.
      WA_ZJOB0001-HR_ATUAL = SY-UZEIT.
      MODIFY ZJOB0001 FROM WA_ZJOB0001.
      CLEAR: IT_SAIDA,
             WA_ZJOB0001.
      PERFORM SELECT_TAB.
      LEAVE TO SCREEN 0.
      MESSAGE 'JOB alterado com sucesso.' TYPE 'S'.

    ELSEIF SY-UCOMM = 'CLOSE' OR SY-UCOMM = 'EXIT' OR SY-UCOMM = 'CANCEL'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1001 OUTPUT.

  DATA: WA_MOD TYPE L_SAIDA.

  SET PF-STATUS '1001'.
  IF BT = '&NOVO'.
    SET TITLEBAR '1001-1'.
  ELSEIF BT = '&MODIFICAR'.
    SET TITLEBAR '1001-2'.
    READ TABLE IT_SAIDA INTO WA_MOD INDEX POINT.
    MOVE-CORRESPONDING WA_MOD TO WA_ZJOB0001.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'A1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001_EXIT INPUT.
  CLEAR: WA_ZJOB0001.
  LEAVE TO SCREEN 0.
ENDMODULE.

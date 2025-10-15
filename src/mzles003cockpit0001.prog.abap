*----------------------------------------------------------------------*
***INCLUDE MZLES0030001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       Status tela 0001
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  IF VG_TELA_0001 IS INITIAL.
    VG_TELA_0001 = C_0002.
  ENDIF.

  CLEAR: IT_FCODE[].

  IF ( TAB_CONTROL_0002-ACTIVETAB EQ OK_TABOPER ) OR ( TAB_CONTROL_0002-ACTIVETAB EQ OK_TABCONFER ).

    WA_FCODE = OK_PESQ.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_LERARQ.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_GERAR.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_EDITAR.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_ARQUIVO.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_ARQUIVO_C.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_DELOTE.
    APPEND WA_FCODE TO IT_FCODE.

    IF TAB_CONTROL_0002-ACTIVETAB EQ OK_TABOPER.
      WA_FCODE = OK_CONTB.
      APPEND WA_FCODE TO IT_FCODE.
      WA_FCODE = OK_LOG_PROC.
      APPEND WA_FCODE TO IT_FCODE.
      WA_FCODE = OK_SALV_CONF.
      APPEND WA_FCODE TO IT_FCODE.
      WA_FCODE = OK_DT_BAIXA.
      APPEND WA_FCODE TO IT_FCODE.
    ELSE.
      IF VG_ALTEROU_CONF IS INITIAL.
        WA_FCODE = OK_SALV_CONF.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
    ENDIF.

  ELSE.
    WA_FCODE = OK_CONTB.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_SALV_CONF.
    APPEND WA_FCODE TO IT_FCODE.
    WA_FCODE = OK_DT_BAIXA.
    APPEND WA_FCODE TO IT_FCODE.
  ENDIF.

  SET PF-STATUS 'PF0001' EXCLUDING IT_FCODE.
  SET TITLEBAR  'TL0001'.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       Comandos tela 0001
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

  CASE OK_CODE.
    WHEN OK_ATUALIZA.
      PERFORM DISMARCA_GRID_CONF.
      PERFORM PESQUISA_LOTE.
    WHEN OK_BACK OR OK_CANCEL.
      PERFORM DISMARCA_GRID_CONF.
      VG_TELA_0002 = C_0003.
      TAB_CONTROL_0002-ACTIVETAB = OK_TABLOTE.
      CLEAR OK_CODE.
    WHEN OK_EXIT.
      PERFORM DISMARCA_GRID_CONF.
    WHEN OK_PESQ.
      PERFORM DISMARCA_GRID_CONF.
      PERFORM PESQUISA_LOTE.
    WHEN OK_ARQUIVO.
      PERFORM DISMARCA_GRID_CONF.
      CALL SCREEN 9001 STARTING AT 07 05 ENDING AT 120 30.
    WHEN OK_ARQUIVO_C.
      PERFORM DISMARCA_GRID_CONF.
      PERFORM ARQUIVO_CONFERENCIA.
    WHEN OTHERS.
      IF ( SY-UCOMM NE OK_BTNE ) AND ( SY-UCOMM NE OK_BTNC ).
        PERFORM DISMARCA_GRID_CONF.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       Comando de saida de programa
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK_CODE.
    WHEN OK_BACK OR OK_CANCEL.
      IF TAB_CONTROL_0002-ACTIVETAB EQ OK_TABLOTE.
        LEAVE PROGRAM.
      ENDIF.
    WHEN OK_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

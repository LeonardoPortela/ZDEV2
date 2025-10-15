*----------------------------------------------------------------------*
***INCLUDE ZJOB0001_1000.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PRINT_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM PRINT_ALV.

  DATA: WA_LAY  TYPE SLIS_LAYOUT_ALV.
  WA_LAY-COLWIDTH_OPTIMIZE = 'X'. "auto-ajuste das colunas do fieldcat

  PERFORM BUILDING_FIELDCAT USING:
    'ICON' 'Status',
    'PGMNA' 'Programa',
    'JOBNAME' 'Nome JOB',
    'MOD_SAP' 'Módulo SAP',
    'AREA_RESP' 'Área Responsável',
    'PERIOCID_JOB' 'Periocidade JOB',
    'DESCR_FUNC' 'Desc. Funcionalidade',
    'DT_ATIVACAO' 'Dt. Ativação',
    'CLASSIFCACAO' 'Classificação',
    'ANAL_RESP' 'Analista Responsável',
    'USUARIO_PROC' 'Usuário Processador',
    'USNAM_BASIS'  'Analista BASIS'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IT_FIELDCAT              = IT_FCAT
      IS_LAYOUT                = WA_LAY
    TABLES
      T_OUTTAB                 = IT_SAIDA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILDING_FIELDCAT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM BUILDING_FIELDCAT USING P_CAMPO P_DESC.

  DATA: WA_FCAT TYPE SLIS_FIELDCAT_ALV.

  WA_FCAT-FIELDNAME = P_CAMPO.
  WA_FCAT-SELTEXT_M = P_DESC.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE. "Formulário para Criação do Cabeçalho da ALV

  DATA: IT_HEADER TYPE SLIS_T_LISTHEADER,
        ST_HEADER TYPE SLIS_LISTHEADER.

  ST_HEADER-TYP  = 'H'.
  ST_HEADER-INFO = 'CONTROLE DE JOBS'.
  APPEND ST_HEADER TO IT_HEADER.
  CLEAR ST_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB. "Formulário para a criação dos botões da ALV
  SET PF-STATUS 'STANDARD'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM
                        SELFIELD TYPE KKBLO_SELFIELD. "Formulário para captura de evento de usuário

  DATA: ANS   TYPE CHAR1,
        CORPO TYPE STRING.

  SELFIELD-REFRESH = 'X'.
  POINT = SELFIELD-TABINDEX.

  IF UCOMM EQ '&NOVO'.
    BT = '&NOVO'.
    CALL SCREEN 1001 STARTING AT 40 10.
  ELSEIF UCOMM EQ '&EXCLUIR'.
    IF POINT IS INITIAL.
      MESSAGE 'Favor selecione um registro para excluir.' TYPE 'I'.
    ELSE.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX POINT.
      CONCATENATE 'Você tem certeza que deseja excluir o JOB' WA_SAIDA-JOBNAME '?' INTO CORPO SEPARATED BY SPACE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Excluir Registro'
          TEXT_QUESTION         = CORPO
          TEXT_BUTTON_1         = 'Sim'(002)
          TEXT_BUTTON_2         = 'Não'(005)
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = ANS.

      IF ANS = 1.
        DELETE FROM ZJOB0001 WHERE JOBNAME = WA_SAIDA-JOBNAME.
        CLEAR WA_SAIDA.
        PERFORM SELECT_TAB.
      ENDIF.
    ENDIF.
  ELSEIF UCOMM EQ '&MODIFICAR'.
    BT = '&MODIFICAR'.
    IF POINT IS INITIAL.
      MESSAGE 'Favor selecione um registro para modificar.' TYPE 'I'.
    ELSE.
      CALL SCREEN 1001 STARTING AT 40 10.
    ENDIF.
  ELSEIF UCOMM = 'CANCEL'.
    LEAVE TO SCREEN 0.
  ELSEIF UCOMM = 'EXIT'.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

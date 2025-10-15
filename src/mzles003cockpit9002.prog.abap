*&---------------------------------------------------------------------*
*&  Include           MZLES003COCKPIT9002
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF SCREEN 9002 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK ZB02.
PARAMETERS: P_FILE   LIKE RLGRAP-FILENAME .
SELECTION-SCREEN END OF BLOCK ZB02.
SELECTION-SCREEN:BEGIN OF BLOCK  B2 WITH FRAME TITLE TEXT-025.
PARAMETERS: P_ON   RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND ASQ,
            P_BACK RADIOBUTTON GROUP G1.
SELECTION-SCREEN: END OF BLOCK B2.
SELECTION-SCREEN END OF SCREEN 9002.

*----------------------------------------------------------------------
* Match code para o nome do arquivo
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = ' '
      MASK             = ',*.txt.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar da Administradora!'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_ARQUIVO_ADIMINISTRADORA
*&---------------------------------------------------------------------*
*       Busca arquivo administradora
*----------------------------------------------------------------------*
FORM BUSCA_ARQUIVO_ADIMINISTRADORA .


  DATA: ARQUIVO    TYPE ADMI_PATH, "STRING,
        DIRETORIO  TYPE ADMI_PATH, "STRING,
        ARQSTRING  TYPE STRING,
        PRC_RESULT TYPE C,
        QTDCHAR    TYPE I,
        T_ARQUIVO	         TYPE TABLE OF ZPFE_ARQUIVO WITH HEADER LINE,
        T_REG_CABECALHO    TYPE TABLE OF ZPFE_LOTE WITH HEADER LINE,
        T_REG_ITENS        TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        WL_LOTE TYPE I.

  CLASS CL_GUI_FRONTEND_SERVICES DEFINITION LOAD.
  REFRESH TG_0062.

  IF P_FILE IS INITIAL.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Preencher o campo  Arquivo Administradora.'.
  ELSE.
    ARQSTRING = P_FILE.
    PRC_RESULT = CL_GUI_FRONTEND_SERVICES=>FILE_EXIST( ARQSTRING ).

    IF PRC_RESULT IS INITIAL.
      MESSAGE ID 'FES' TYPE 'E' NUMBER '000' RAISING FILE_OPEN_ERROR.
    ENDIF.

    QTDCHAR = STRLEN( ARQSTRING ).
    QTDCHAR = QTDCHAR - 1.

    WHILE ARQSTRING+QTDCHAR(1) NE '\'.
      CONCATENATE ARQSTRING+QTDCHAR(1) ARQUIVO INTO ARQUIVO.
      QTDCHAR = QTDCHAR - 1.
    ENDWHILE.

    IF QTDCHAR GT 0.
      QTDCHAR = QTDCHAR + 1.
      DIRETORIO = ARQSTRING(QTDCHAR).
    ENDIF.

    IF P_ON IS NOT INITIAL.
      CALL FUNCTION 'Z_PFE_LEITURA_ARQ_AUX'
        EXPORTING
          DIRETORIO               = DIRETORIO
          ARQUIVO                 = ARQUIVO
          UNIX                    = SPACE
          LOCAL                   = 'X'
          GERAR_LOTES             = 'X'
        EXCEPTIONS
          INVALID_EPS_SUBDIR      = 1
          SAPGPARAM_FAILED        = 2
          BUILD_DIRECTORY_FAILED  = 3
          NO_AUTHORIZATION        = 4
          READ_DIRECTORY_FAILED   = 5
          TOO_MANY_READ_ERRORS    = 6
          EMPTY_DIRECTORY_LIST    = 7
          NAO_ADMINISTRADORA      = 8
          NAO_LOCAL_NEGOCIO       = 9
          INTERVAL_NOT_FOUND      = 10
          NUMBER_RANGE_NOT_INTERN = 11
          OUTROS_ERROS            = 11
          OTHERS                  = 12.
    ELSE.
      CALL FUNCTION 'Z_PFE_LEITURA_ARQ_AUX2'
        EXPORTING
          DIRETORIO               = DIRETORIO
          ARQUIVO                 = ARQUIVO
          UNIX                    = SPACE
          LOCAL                   = 'X'
          GERAR_LOTES             = 'X'
        TABLES
          T_ARQUIVO               = T_ARQUIVO
          T_REG_CABECALHO         = T_REG_CABECALHO
          T_REG_ITENS             = T_REG_ITENS
        EXCEPTIONS
          INVALID_EPS_SUBDIR      = 1
          SAPGPARAM_FAILED        = 2
          BUILD_DIRECTORY_FAILED  = 3
          NO_AUTHORIZATION        = 4
          READ_DIRECTORY_FAILED   = 5
          TOO_MANY_READ_ERRORS    = 6
          EMPTY_DIRECTORY_LIST    = 7
          NAO_ADMINISTRADORA      = 8
          NAO_LOCAL_NEGOCIO       = 9
          INTERVAL_NOT_FOUND      = 10
          NUMBER_RANGE_NOT_INTERN = 11
          OUTROS_ERROS            = 11
          OTHERS                  = 12.


      MESSAGE S836(SD) WITH 'O arquivo foi marcado para execução em background.'.
      LEAVE TO SCREEN 0.

    ENDIF.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CALL FUNCTION 'Z_PFE_LOG_PROC_ARQ'
      EXPORTING
        I_POPUP  = '1'
        I_SEARCH = ' '
      TABLES
        IT_LOG   = TG_0062.
  ENDIF.
ENDFORM.                    " BUSCA_ARQUIVO_ADIMINISTRADORA

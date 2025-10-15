************************************************************************
* Program        : ZOPENSIS_CARGA_002                                  *
* Transaction    : ZOPENSIS_006                                        *
* Title          : Carga dos Riscos Gestores                           *
* Developer      : Fernando Oliveira                                   *
* Date           : 10/08/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     10/08/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************

REPORT  ZOPENSIS_CARGA_002.

*----------------------------------------------------------------------*
* Declaração de Tipos                                                  *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_LINE,
         LINE(255) TYPE C,
       END OF TY_LINE.

"ZTOPENSIS_006 - Dados
TYPES: BEGIN OF TY_ZTOPENSIS_006,
         ZCOD_USER          TYPE ZTOPENSIS_006-ZCOD_USER,
         ZDSC_USER          TYPE ZTOPENSIS_006-ZDSC_USER,
         ZDSC_DEPTO         TYPE ZTOPENSIS_006-ZDSC_DEPTO,
         ZDSC_CARGO         TYPE ZTOPENSIS_006-ZDSC_CARGO,
         ZCOD_OPUS          TYPE ZTOPENSIS_006-ZCOD_OPUS,
         ZCOD_SIGAM         TYPE ZTOPENSIS_006-ZCOD_SIGAM,
         ZCOD_ROLE_COMPOSTO TYPE ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO,
         ZCOD_ROLE_RESTRITO TYPE ZTOPENSIS_006-ZCOD_ROLE_RESTRITO,
         ZCOD_GESTOR        TYPE ZTOPENSIS_006-ZCOD_GESTOR,
         ZCOD_EMAIL         TYPE ZTOPENSIS_006-ZCOD_EMAIL,
       END OF TY_ZTOPENSIS_006.

"ZTOPENSIS_007 - Dados - Log
TYPES: BEGIN OF TY_ZTOPENSIS_007,
         ZCOD_USER         TYPE ZTOPENSIS_007-ZCOD_USER,
         ZDSC_USER         TYPE ZTOPENSIS_007-ZDSC_USER,
         ZDSC_CARGO        TYPE ZTOPENSIS_007-ZDSC_CARGO,
         ZDSC_DEPTO        TYPE ZTOPENSIS_007-ZDSC_DEPTO,
         ZCOD_TCODE        TYPE ZTOPENSIS_007-ZCOD_TCODE,
         ZDSC_TTEXT        TYPE ZTOPENSIS_007-ZDSC_TTEXT,
         ZCOD_ROLE_SIMPLES TYPE ZTOPENSIS_007-ZCOD_ROLE_SIMPLES,
         ZIND_RISCO        TYPE ZTOPENSIS_007-ZIND_RISCO,
       END OF TY_ZTOPENSIS_007.

"ZTOPENSIS_005 - Dados Transação
TYPES: BEGIN OF TY_ZTOPENSIS_005,
         ZCOD_TCODE       TYPE ZTOPENSIS_005-ZCOD_TCODE,
         ZDSC_TTEXT       TYPE ZTOPENSIS_005-ZDSC_TTEXT,
         ZCOD_ROLE_USER   TYPE ZTOPENSIS_005-ZCOD_ROLE_USER,
         ZIND_ROLE_STATUS TYPE ZTOPENSIS_005-ZIND_ROLE_STATUS,
         ZDSC_RISCO       TYPE ZTOPENSIS_005-ZDSC_RISCO,
         ZCOD_RISCO       TYPE ZTOPENSIS_005-ZCOD_RISCO,
       END OF TY_ZTOPENSIS_005.

"ZTOPENSIS_007 - Dados - Log Adicional
TYPES: BEGIN OF TY_ZTOPENSIS_007_X,
         ZCOD_USER  TYPE ZTOPENSIS_007-ZCOD_USER,
         ZCOD_TCODE TYPE ZTOPENSIS_007-ZCOD_TCODE,
       END OF TY_ZTOPENSIS_007_X.

*----------------------------------------------------------------------*
* Declaração de tabelas interna                                        *
*----------------------------------------------------------------------*
DATA: GT_FILE            TYPE TABLE OF TY_LINE,
      GT_ZTOPENSIS_005   TYPE TABLE OF ZTOPENSIS_005,       " Dados Transação
      GT_ZTOPENSIS_006   TYPE TABLE OF ZTOPENSIS_006,       " Dados
      GT_ZTOPENSIS_007   TYPE TABLE OF ZTOPENSIS_007,       " Dados de Log
      GT_ZTOPENSIS_007_X TYPE TABLE OF TY_ZTOPENSIS_007_X.  " Dados de Log Adicional

*----------------------------------------------------------------------*
* Declaração de Work-áreas                                             *
*----------------------------------------------------------------------*
DATA: GS_LINE             LIKE LINE OF GT_FILE,
      GS_ZTOPENSIS_006    TYPE ZTOPENSIS_006,        " Dados
      GS_ZTOPENSIS_007    TYPE ZTOPENSIS_007,        " Dados de Log
      GS_ZTOPENSIS_005    TYPE ZTOPENSIS_005,        " Dados Transação

      GS_ZTOPENSIS_006_X  TYPE TY_ZTOPENSIS_006,     " Dados
      GS_ZTOPENSIS_007_X  TYPE TY_ZTOPENSIS_007,     " Dados de Log
      GS_ZTOPENSIS_007_XX TYPE TY_ZTOPENSIS_007_X,   " Dados de Log
      GS_ZTOPENSIS_005_X  TYPE TY_ZTOPENSIS_005.     " Dados Transação

*----------------------------------------------------------------------*
* Declaração de Variáveis                                              *
*----------------------------------------------------------------------*
DATA: V_FILENAME TYPE STRING,
      GT_FILEBIN TYPE FILETABLE,
      GS_FILESTR TYPE FILE_TABLE,
      V_RC       TYPE I,
      GC_ERRO.

*----------------------------------------------------------------------*
* Parãmetros de Seleção                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001. "Arquivo DADOS:
PARAMETERS: P_FILE_1  TYPE STRING.
SELECTION-SCREEN END   OF BLOCK A1.

SELECTION-SCREEN BEGIN OF BLOCK A2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN COMMENT /1(50) A2_COL00.
SELECTION-SCREEN COMMENT /1(50) A2_COL01.
SELECTION-SCREEN COMMENT /1(50) A2_COL02.
SELECTION-SCREEN COMMENT /1(50) A2_COL03.
SELECTION-SCREEN COMMENT /1(50) A2_COL04.
SELECTION-SCREEN COMMENT /1(50) A2_COL05.
SELECTION-SCREEN COMMENT /1(50) A2_COL06.
SELECTION-SCREEN COMMENT /1(50) A2_COL07.
SELECTION-SCREEN COMMENT /1(50) A2_COL08.
SELECTION-SCREEN COMMENT /1(50) A2_COL09.
SELECTION-SCREEN COMMENT /1(50) A2_COL10.
SELECTION-SCREEN END   OF BLOCK A2.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-004. "Arquivo Dados-Log:
PARAMETERS: P_FILE_2  TYPE STRING.
SELECTION-SCREEN END   OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-006.
SELECTION-SCREEN COMMENT /1(50) B2_COL00.
SELECTION-SCREEN COMMENT /1(50) B2_COL01.
SELECTION-SCREEN COMMENT /1(50) B2_COL02.
SELECTION-SCREEN COMMENT /1(50) B2_COL03.
SELECTION-SCREEN COMMENT /1(50) B2_COL04.
SELECTION-SCREEN COMMENT /1(50) B2_COL05.
SELECTION-SCREEN COMMENT /1(50) B2_COL06.
SELECTION-SCREEN COMMENT /1(50) B2_COL07.
SELECTION-SCREEN COMMENT /1(50) B2_COL08.
SELECTION-SCREEN END   OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-007. "Arquivo Dados Transação:
PARAMETERS: P_FILE_3  TYPE STRING.
SELECTION-SCREEN END   OF BLOCK C1.

SELECTION-SCREEN BEGIN OF BLOCK C2 WITH FRAME TITLE TEXT-008.
SELECTION-SCREEN COMMENT /1(50) C2_COL00.
SELECTION-SCREEN COMMENT /1(50) C2_COL01.
SELECTION-SCREEN COMMENT /1(50) C2_COL02.
SELECTION-SCREEN COMMENT /1(50) C2_COL03.
SELECTION-SCREEN COMMENT /1(50) C2_COL04.
SELECTION-SCREEN COMMENT /1(50) C2_COL05.
SELECTION-SCREEN COMMENT /1(50) C2_COL06.
SELECTION-SCREEN END   OF BLOCK C2.

SELECTION-SCREEN BEGIN OF BLOCK E1 WITH FRAME TITLE TEXT-BE1. "Arquivo Dados-Log Adicionais:
PARAMETERS: P_FILE_4  TYPE STRING.
SELECTION-SCREEN END   OF BLOCK E1.

SELECTION-SCREEN BEGIN OF BLOCK E2 WITH FRAME TITLE TEXT-BE2.
SELECTION-SCREEN COMMENT /1(50) E2_COL00.
SELECTION-SCREEN COMMENT /1(50) E2_COL01.
SELECTION-SCREEN COMMENT /1(50) E2_COL02.
SELECTION-SCREEN END   OF BLOCK E2.

SELECTION-SCREEN BEGIN OF BLOCK D1 WITH FRAME TITLE TEXT-005.
PARAMETERS: P_CABEC TYPE FLAG DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK D1.

SELECTION-SCREEN BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_EXIB_1 RADIOBUTTON GROUP G1 DEFAULT 'X'.
PARAMETERS: P_EXIB_2 RADIOBUTTON GROUP G1.
PARAMETERS: P_EXIB_3 RADIOBUTTON GROUP G1.
PARAMETERS: P_SAVE   RADIOBUTTON GROUP G1.
PARAMETERS: P_DELE   RADIOBUTTON GROUP G1.
SELECTION-SCREEN END   OF BLOCK A3.

*----------------------------------------------------------------------*
* AT Selection Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE_1.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE = GT_FILEBIN
      RC         = V_RC.

  IF SY-SUBRC IS INITIAL.
    READ TABLE GT_FILEBIN INTO GS_FILESTR INDEX 1.
    P_FILE_1 = GS_FILESTR-FILENAME.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE_2.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE = GT_FILEBIN
      RC         = V_RC.

  IF SY-SUBRC IS INITIAL.
    READ TABLE GT_FILEBIN INTO GS_FILESTR INDEX 1.
    P_FILE_2 = GS_FILESTR-FILENAME.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE_3.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE = GT_FILEBIN
      RC         = V_RC.

  IF SY-SUBRC IS INITIAL.
    READ TABLE GT_FILEBIN INTO GS_FILESTR INDEX 1.
    P_FILE_3 = GS_FILESTR-FILENAME.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE_4.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE = GT_FILEBIN
      RC         = V_RC.

  IF SY-SUBRC IS INITIAL.
    READ TABLE GT_FILEBIN INTO GS_FILESTR INDEX 1.
    P_FILE_4 = GS_FILESTR-FILENAME.
  ENDIF.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.

  A2_COL00 = 'A Extensão do Arquivo deve ser .CSV'.
  A2_COL01 = 'Coluna 1: Usúario'.
  A2_COL02 = 'Coluna 2: Nome Completo'.
  A2_COL03 = 'Coluna 3: Departamento'.
  A2_COL04 = 'Coluna 4: Cargo'.
  A2_COL05 = 'Coluna 5: OPUS'.
  A2_COL06 = 'Coluna 6: SIGAM'.
  A2_COL07 = 'Coluna 7: Perfil Composto'.
  A2_COL08 = 'Coluna 8: Perfil Restrito'.
  A2_COL09 = 'Coluna 9: Gestor'.
  A2_COL10 = 'Coluna 9: E-mail'.

  B2_COL00 = 'A Extensão do Arquivo deve ser .CSV'.
  B2_COL01 = 'Coluna 1: User-ID'.
  B2_COL02 = 'Coluna 2: Nome'.
  B2_COL03 = 'Coluna 3: Cargo'.
  B2_COL04 = 'Coluna 4: Departamento'.
  B2_COL05 = 'Coluna 5: Transação'.
  B2_COL06 = 'Coluna 6: Descrição'.
  B2_COL07 = 'Coluna 7: Perfil Simples'.
  B2_COL08 = 'Coluna 8: Risco'.

  C2_COL00 = 'A Extensão do Arquivo deve ser .CSV'.
  C2_COL01 = 'Coluna 1: Transação'.
  C2_COL02 = 'Coluna 2: Descrição Transação'.
  C2_COL03 = 'Coluna 3: Função'.
  C2_COL04 = 'Coluna 4: Existe Risco'.
  C2_COL05 = 'Coluna 5: Descrição do Risco'.
  C2_COL06 = 'Coluna 6: Risco'.

  E2_COL00 = 'A Extensão do Arquivo deve ser .CSV'.
  E2_COL01 = 'Coluna 1: User-ID'.
  E2_COL02 = 'Coluna 2: Transação'.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF P_DELE IS NOT INITIAL.
    DELETE FROM ZTOPENSIS_005.
    DELETE FROM ZTOPENSIS_006.
    DELETE FROM ZTOPENSIS_007.
    COMMIT WORK.
    EXIT.
  ENDIF.

  IF P_FILE_1 IS INITIAL AND
     P_FILE_2 IS INITIAL AND
     P_FILE_3 IS INITIAL AND
     P_FILE_4 IS INITIAL.
    MESSAGE 'Parametro Caminho do Arquivo obrigatório para essa seleção' TYPE 'E'.
  ELSE.
    IF P_FILE_1 IS NOT INITIAL.
      PERFORM ZF_UPLOAD_ARQUIVO USING P_FILE_1.
      PERFORM ZF_TRATAR_DADOS_ARQ_DADOS.
    ENDIF.
    IF P_FILE_2 IS NOT INITIAL.
      PERFORM ZF_UPLOAD_ARQUIVO USING P_FILE_2.
      PERFORM ZF_TRATAR_DADOS_ARQ_DADOS_LOG.
    ENDIF.
    IF P_FILE_3 IS NOT INITIAL.
      PERFORM ZF_UPLOAD_ARQUIVO USING P_FILE_3.
      PERFORM ZF_TRATAR_DADOS_ARQ_TRANSACAO.
    ENDIF.
    IF P_FILE_4 IS NOT INITIAL.
      PERFORM ZF_UPLOAD_ARQUIVO USING P_FILE_4.
      PERFORM ZF_TRATAR_DADOS_ARQ_DADOS_LOGX.
    ENDIF.

    CHECK GC_ERRO IS INITIAL.

    IF P_EXIB_1 IS NOT INITIAL.
      PERFORM ZF_EXIBIR_RELATORIO_DADOS.
    ENDIF.
    IF P_EXIB_2 IS NOT INITIAL.
      PERFORM ZF_EXIBIR_RELATORIO_DADOS_LOG.
    ENDIF.
    IF P_EXIB_3 IS NOT INITIAL.
      PERFORM ZF_EXIBIR_RELATORIO_TRANSACAO.
    ENDIF.

    IF P_SAVE IS NOT INITIAL.
      PERFORM ZF_GRAVAR_DADOS.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*
*&---------------------------------------------------------------------*
*&      Form  ZF_UPLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_UPLOAD_ARQUIVO USING P_FILE.

  V_FILENAME = P_FILE.

  FREE GT_FILE[].

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = V_FILENAME
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = GT_FILE
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF P_CABEC IS NOT INITIAL.
      DELETE GT_FILE INDEX 1.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS_ARQ_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_TRATAR_DADOS_ARQ_DADOS .

  LOOP AT GT_FILE INTO GS_LINE.
    CLEAR: GS_ZTOPENSIS_006, GS_ZTOPENSIS_006_X.
    SPLIT GS_LINE AT ';' INTO GS_ZTOPENSIS_006_X-ZCOD_USER
                              GS_ZTOPENSIS_006_X-ZDSC_USER
                              GS_ZTOPENSIS_006_X-ZDSC_DEPTO
                              GS_ZTOPENSIS_006_X-ZDSC_CARGO
                              GS_ZTOPENSIS_006_X-ZCOD_OPUS
                              GS_ZTOPENSIS_006_X-ZCOD_SIGAM
                              GS_ZTOPENSIS_006_X-ZCOD_ROLE_COMPOSTO
                              GS_ZTOPENSIS_006_X-ZCOD_ROLE_RESTRITO
                              GS_ZTOPENSIS_006_X-ZCOD_GESTOR
                              GS_ZTOPENSIS_006_X-ZCOD_EMAIL.
    MOVE-CORRESPONDING GS_ZTOPENSIS_006_X TO GS_ZTOPENSIS_006.
    CHECK GS_ZTOPENSIS_006_X-ZCOD_USER IS NOT INITIAL.
    APPEND GS_ZTOPENSIS_006 TO GT_ZTOPENSIS_006.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS_ARQ_DADOS_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_TRATAR_DADOS_ARQ_DADOS_LOG .

  LOOP AT GT_FILE INTO GS_LINE.
    CLEAR: GS_ZTOPENSIS_007, GS_ZTOPENSIS_007_X.
    SPLIT GS_LINE AT ';' INTO GS_ZTOPENSIS_007_X-ZCOD_USER
                              GS_ZTOPENSIS_007_X-ZDSC_USER
                              GS_ZTOPENSIS_007_X-ZDSC_CARGO
                              GS_ZTOPENSIS_007_X-ZDSC_DEPTO
                              GS_ZTOPENSIS_007_X-ZCOD_TCODE
                              GS_ZTOPENSIS_007_X-ZDSC_TTEXT
                              GS_ZTOPENSIS_007_X-ZCOD_ROLE_SIMPLES
                              GS_ZTOPENSIS_007_X-ZIND_RISCO .

    CHECK GS_ZTOPENSIS_007_X-ZCOD_USER IS NOT INITIAL.

    MOVE-CORRESPONDING GS_ZTOPENSIS_007_X TO GS_ZTOPENSIS_007.
    APPEND GS_ZTOPENSIS_007 TO GT_ZTOPENSIS_007.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS_ARQ_DADOS_TRANSACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_TRATAR_DADOS_ARQ_TRANSACAO.

  LOOP AT GT_FILE INTO GS_LINE.
    CLEAR: GS_ZTOPENSIS_005, GS_ZTOPENSIS_005_X.
    SPLIT GS_LINE AT ';' INTO GS_ZTOPENSIS_005_X-ZCOD_TCODE
                              GS_ZTOPENSIS_005_X-ZDSC_TTEXT
                              GS_ZTOPENSIS_005_X-ZCOD_ROLE_USER
                              GS_ZTOPENSIS_005_X-ZIND_ROLE_STATUS
                              GS_ZTOPENSIS_005_X-ZDSC_RISCO
                              GS_ZTOPENSIS_005_X-ZCOD_RISCO.
    MOVE-CORRESPONDING GS_ZTOPENSIS_005_X TO GS_ZTOPENSIS_005.
    CHECK GS_ZTOPENSIS_005_X-ZCOD_TCODE IS NOT INITIAL.
    APPEND GS_ZTOPENSIS_005 TO GT_ZTOPENSIS_005.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_RELATORIO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_EXIBIR_RELATORIO_DADOS.

  DATA: R_TABLE     TYPE REF TO CL_SALV_TABLE,
        R_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS.

  TRY .
      CALL METHOD CL_SALV_TABLE=>FACTORY
        IMPORTING
          R_SALV_TABLE = R_TABLE
        CHANGING
          T_TABLE      = GT_ZTOPENSIS_006.
    CATCH CX_SALV_MSG.
  ENDTRY.

  CALL METHOD R_TABLE->DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_RELATORIO_DADOS_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_EXIBIR_RELATORIO_DADOS_LOG.

  DATA: R_TABLE     TYPE REF TO CL_SALV_TABLE,
        R_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS.

  TRY .
      CALL METHOD CL_SALV_TABLE=>FACTORY
        IMPORTING
          R_SALV_TABLE = R_TABLE
        CHANGING
          T_TABLE      = GT_ZTOPENSIS_007.
    CATCH CX_SALV_MSG.
  ENDTRY.

  CALL METHOD R_TABLE->DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_RELATORIO_TRANSACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_EXIBIR_RELATORIO_TRANSACAO.

  DATA: R_TABLE     TYPE REF TO CL_SALV_TABLE,
        R_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS.

  TRY .
      CALL METHOD CL_SALV_TABLE=>FACTORY
        IMPORTING
          R_SALV_TABLE = R_TABLE
        CHANGING
          T_TABLE      = GT_ZTOPENSIS_005.
    CATCH CX_SALV_MSG.
  ENDTRY.

  CALL METHOD R_TABLE->DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_GRAVAR_DADOS .

  MODIFY ZTOPENSIS_005 FROM TABLE GT_ZTOPENSIS_005.
  COMMIT WORK.

  MODIFY ZTOPENSIS_006 FROM TABLE GT_ZTOPENSIS_006.
  COMMIT WORK.

  MODIFY ZTOPENSIS_007 FROM TABLE GT_ZTOPENSIS_007.
  COMMIT WORK.

  MESSAGE 'Carga Efetuada com Sucesso' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS_ARQ_DADOS_LOGX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_TRATAR_DADOS_ARQ_DADOS_LOGX .

  DATA: LT_ZTOPENSIS_005 TYPE TABLE OF ZTOPENSIS_005,
        LT_ZTOPENSIS_006 TYPE TABLE OF ZTOPENSIS_006.
  DATA: LS_ZTOPENSIS_005 TYPE ZTOPENSIS_005,
        LS_ZTOPENSIS_006 TYPE ZTOPENSIS_006.

  DATA: LI_INDEX TYPE I.

  SELECT *
    FROM ZTOPENSIS_005
    INTO TABLE LT_ZTOPENSIS_005.

  SELECT *
    FROM ZTOPENSIS_006
    INTO TABLE LT_ZTOPENSIS_006.

  LOOP AT GT_FILE INTO GS_LINE.
    CLEAR: GS_ZTOPENSIS_007, GS_ZTOPENSIS_007_X.
    SPLIT GS_LINE AT ';' INTO GS_ZTOPENSIS_007_XX-ZCOD_USER
                              GS_ZTOPENSIS_007_XX-ZCOD_TCODE.

    CHECK GS_ZTOPENSIS_007_XX-ZCOD_USER IS NOT INITIAL.
    MOVE-CORRESPONDING GS_ZTOPENSIS_007_XX TO GS_ZTOPENSIS_007.
    APPEND GS_ZTOPENSIS_007 TO GT_ZTOPENSIS_007.
  ENDLOOP.

  LOOP AT GT_ZTOPENSIS_007 INTO GS_ZTOPENSIS_007.
    LI_INDEX = SY-TABIX.
    READ TABLE LT_ZTOPENSIS_006 INTO LS_ZTOPENSIS_006 WITH KEY ZCOD_USER = GS_ZTOPENSIS_007-ZCOD_USER.
    IF SY-SUBRC = 0.
      GS_ZTOPENSIS_007-ZDSC_USER            = LS_ZTOPENSIS_006-ZDSC_USER.
      GS_ZTOPENSIS_007-ZDSC_DEPTO           = LS_ZTOPENSIS_006-ZDSC_DEPTO.
      GS_ZTOPENSIS_007-ZDSC_CARGO           = LS_ZTOPENSIS_006-ZDSC_CARGO.
    ENDIF.
    READ TABLE LT_ZTOPENSIS_005 INTO LS_ZTOPENSIS_005 WITH KEY ZCOD_TCODE = GS_ZTOPENSIS_007-ZCOD_TCODE.
    IF SY-SUBRC = 0.
      GS_ZTOPENSIS_007-ZDSC_TTEXT           = LS_ZTOPENSIS_005-ZDSC_TTEXT.
      GS_ZTOPENSIS_007-ZCOD_ROLE_SIMPLES    = LS_ZTOPENSIS_005-ZCOD_ROLE_USER.
      GS_ZTOPENSIS_007-ZIND_RISCO           = LS_ZTOPENSIS_005-ZCOD_RISCO.
    ENDIF.
    GS_ZTOPENSIS_007-ZIND_CARGA_ADICIONAL = 'X'.
    MODIFY GT_ZTOPENSIS_007 FROM GS_ZTOPENSIS_007 INDEX LI_INDEX.
  ENDLOOP.

ENDFORM.

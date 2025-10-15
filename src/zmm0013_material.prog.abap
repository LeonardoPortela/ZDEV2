************************************************************************
* Responsável ...: Ricardo Furst - Consultor ABAP                      *
* Data desenv ...: 23.10.2009                                          *
* Tipo de prg ...: executável
* Objetivo    ...: Programa para Saneamento de Materiais               *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 23.10.2009   Ricardo               Criação              DEVK906818   *
*                                                                      *
************************************************************************

REPORT ZMM0013_MATERIAL
              NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
              LINE-SIZE 076               "Comprimento da Linha
              LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: W_BDC              TYPE BDCDATA,
      LW_BDC_OPT         TYPE CTU_PARAMS.
*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_ARQUIVO,
         MATNR            LIKE RMMG1-MATNR,
         WERKS            LIKE RMMG1-WERKS,
         MAKTX            LIKE MAKT-MAKTX,
         MATKL            LIKE MARA-MATKL,
         TXT01(200)       TYPE C,
         TXT02(200)       TYPE C,
         NORMT            LIKE MARA-NORMT,
         STEUC            LIKE MARC-STEUC,
         INDUS            LIKE MARC-INDUS,
         TXT03(200)       TYPE C,
         LGORT            TYPE MARD-LGORT,
         LGPBE            TYPE MARD-LGPBE,
       END   OF TY_ARQUIVO.
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: WA_PLANILHA        LIKE ALSMEX_TABLINE,  "Work Area p/ Planilha
      WA_ARQUIVO         TYPE TY_ARQUIVO,
      WA_ARQERROR        LIKE WA_ARQUIVO.

DATA: IT_ARQUIVO         LIKE STANDARD TABLE OF WA_ARQUIVO,
      IT_ARQERROR        LIKE STANDARD TABLE OF WA_ARQUIVO,
      IT_PLANILHA        LIKE STANDARD TABLE OF WA_PLANILHA.

DATA: T_BDC LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
      T_MSG LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*
DATA: VG_BEGIN_COL       TYPE I VALUE 1,
      VG_BEGIN_ROW       TYPE I VALUE 1,
      VG_TABIX           LIKE SY-TABIX.

DATA: MENSG LIKE MESSAGE VALUE IS INITIAL,
      MSGNO LIKE SY-MSGNO.

DATA: V_MSG            LIKE SY-LISEL,
      V_MESSAGE_ID     LIKE  SY-MSGID,
      V_MESSAGE_NUMBER LIKE  SY-MSGNO,
      V_MESSAGE_VAR1   LIKE  SY-MSGV1,
      V_MESSAGE_VAR2   LIKE  SY-MSGV2,
      V_MESSAGE_VAR3   LIKE  SY-MSGV3,
      V_MESSAGE_VAR4   LIKE  SY-MSGV4,
      P_PODEP(1) VALUE 'X'.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: C_END_COL      TYPE I VALUE 60,
           C_LINES        TYPE I VALUE 64000,
           C_MARK         TYPE C VALUE 'X',
           C_N            TYPE C VALUE 'N',
           C_X            TYPE C              VALUE 'X',
           C_0000         TYPE BDCDATA-DYNPRO VALUE '0000'.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S00.
PARAMETERS: P_ARQ   LIKE RLGRAP-FILENAME OBLIGATORY.  "Arq APLIC.SERVER
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
*PARAMETERS: "P_SANEA RADIOBUTTON GROUP R1,
*            "P_DADME RADIOBUTTON GROUP R1,
*            P_PODEP RADIOBUTTON GROUP R1.
*SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF BLOCK B0.
*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ARQ.


  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_PATH         = P_ARQ
      MASK             = '*.*'
      MODE             = 'O'
      TITLE            = 'Diretório do arquivo de Entrada'
    IMPORTING
      FILENAME         = P_ARQ
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Field Symbols                                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Definição de Range                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Ponteiro de Objeto                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Classes Locais                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Containers                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* Definição Macros                                                     *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  IF NOT P_SANEA IS INITIAL OR
*     NOT P_PODEP IS INITIAL.
  IF  NOT P_PODEP IS INITIAL.
    PERFORM F_CARREGA_ARQUIVO_SANEA.
    IF NOT P_PODEP IS INITIAL.
      PERFORM F_GRAVA_MM_DEP.
    ELSE.
      PERFORM F_GRAVA_MM02.
    ENDIF.
  ELSE.
    MESSAGE I000(Z01) WITH 'Tratamento somente para Saneamento.'.
    STOP.
  ENDIF.

END-OF-SELECTION.

*----------------------------------------------------------------------*
* Top-of-page                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* End-of-page                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* At User-command                                                      *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* At Line-selection                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Início das Sub-Rotinas                                               *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  f_carrega_arquivo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGA_ARQUIVO_SANEA.
  DATA: VL_FLG_DEL       TYPE C.

  REFRESH IT_PLANILHA.
  "Caregar os dados de uma planilha em uma tabela interna
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_ARQ
      I_BEGIN_COL             = VG_BEGIN_COL
      I_BEGIN_ROW             = VG_BEGIN_ROW
      I_END_COL               = C_END_COL
      I_END_ROW               = C_LINES
    TABLES
      INTERN                  = IT_PLANILHA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE E000(Z01) WITH TEXT-M01.
  ENDIF.

  SORT IT_PLANILHA BY ROW COL.

  LOOP AT IT_PLANILHA INTO WA_PLANILHA.
*
*    AT NEW row.
*      CLEAR: wa_arquivo, vl_flg_del.
*    ENDAT.

    IF WA_PLANILHA-ROW NE 1.
      IF NOT P_PODEP IS INITIAL.
        CASE WA_PLANILHA-COL.
          WHEN  1.  WA_ARQUIVO-MATNR = WA_PLANILHA-VALUE.
          WHEN  2.  WA_ARQUIVO-WERKS = WA_PLANILHA-VALUE.
          WHEN  3.  WA_ARQUIVO-LGORT = WA_PLANILHA-VALUE.
          WHEN  4.  WA_ARQUIVO-LGPBE = WA_PLANILHA-VALUE.
        ENDCASE.
      ELSE.
        CASE WA_PLANILHA-COL.
          WHEN  1.  WA_ARQUIVO-MATNR = WA_PLANILHA-VALUE.
          WHEN  2.  WA_ARQUIVO-WERKS = WA_PLANILHA-VALUE.
          WHEN  3.  WA_ARQUIVO-MAKTX = WA_PLANILHA-VALUE.
          WHEN  4.  WA_ARQUIVO-MATKL = WA_PLANILHA-VALUE.
          WHEN  5.  WA_ARQUIVO-TXT01 = WA_PLANILHA-VALUE.
          WHEN  6.  WA_ARQUIVO-TXT02 = WA_PLANILHA-VALUE.
          WHEN  7.  WA_ARQUIVO-NORMT = WA_PLANILHA-VALUE.
          WHEN  8.  WA_ARQUIVO-STEUC = WA_PLANILHA-VALUE.
          WHEN  9.  WA_ARQUIVO-INDUS = WA_PLANILHA-VALUE.
          WHEN  10. WA_ARQUIVO-TXT03 = WA_PLANILHA-VALUE.
        ENDCASE.
      ENDIF.

      AT END OF ROW.
        APPEND WA_ARQUIVO TO IT_ARQUIVO.
      ENDAT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " f_carrega_arquivo_sanea
*&---------------------------------------------------------------------*
*&      Form  f_atualiza_material
*&---------------------------------------------------------------------*
*       Atualizo a tabela MAKT conforme dados da planilha.
*----------------------------------------------------------------------*
FORM F_GRAVA_MM02 .
  DATA: VL_RGATU         TYPE N,
        VL_MSG           TYPE C LENGTH 50.
  CLEAR VL_RGATU.

  LW_BDC_OPT-DEFSIZE  = C_X.
  LW_BDC_OPT-RACOMMIT = C_X.
  LW_BDC_OPT-NOBINPT  = C_X.
  LW_BDC_OPT-DISMODE  = 'N'.

  LOOP AT IT_ARQUIVO INTO WA_ARQUIVO.

    REFRESH T_BDC.

    PERFORM ZF_INSERE_BDC USING:
          'SAPLMGMM' '0060' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'  '=ENTR',
          SPACE      C_0000 SPACE 'RMMG1-MATNR' WA_ARQUIVO-MATNR,

          'SAPLMGMM' '0070' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'  '=ENTR',
          SPACE      C_0000 SPACE 'MSICHTAUSW-KZSEL(01)' 'X',
          SPACE      C_0000 SPACE 'MSICHTAUSW-KZSEL(02)' 'X',
          SPACE      C_0000 SPACE 'MSICHTAUSW-KZSEL(03)' 'X',
          SPACE      C_0000 SPACE 'MSICHTAUSW-KZSEL(04)' 'X',
          SPACE      C_0000 SPACE 'MSICHTAUSW-KZSEL(05)' 'X',

          'SAPLMGMM' '0080' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'  '=ENTR',
          SPACE      C_0000 SPACE 'RMMG1-WERKS' WA_ARQUIVO-WERKS,

          'SAPLMGMM' '4004' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=PB26',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,
          SPACE      C_0000 SPACE 'MARA-MEINS'      'UN',
          SPACE      C_0000 SPACE 'MARA-MATKL'      WA_ARQUIVO-MATKL,
          SPACE      C_0000 SPACE 'MARA-MTPOS_MARA' 'NORM',

          'SAPLMGMM' '4300' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=ZU07',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,

          'SAPLMGMM' '4300' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=BABA',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,

          'SAPLMGMM' '4004' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=SP02',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,
          SPACE      C_0000 SPACE 'MARA-MEINS'      'UN',
          SPACE      C_0000 SPACE 'MARA-MATKL'      WA_ARQUIVO-MATKL,
          SPACE      C_0000 SPACE 'MARA-MTPOS_MARA' 'NORM',
          SPACE      C_0000 SPACE 'DESC_LANGU_GDTXT' 'P',

          'SAPLMGMM' '4004' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=SP09',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,
          SPACE      C_0000 SPACE 'MARA-NORMT'      'SANEAMENTO',

          'SAPLMGMM' '4000' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=SP10',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,
          SPACE      C_0000 SPACE 'MARA-MEINS'      'UN',
          SPACE      C_0000 SPACE 'MARA-MATKL'      WA_ARQUIVO-MATKL,

          'SAPLMGMM' '4000' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '/00',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,
          SPACE      C_0000 SPACE 'MARC-STEUC'      WA_ARQUIVO-STEUC,
          SPACE      C_0000 SPACE 'MARC-INDUS'      WA_ARQUIVO-INDUS,

          'SAPLMGMM' '4000' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=BU',
          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX.

    CALL TRANSACTION 'MM02' USING T_BDC
       MESSAGES INTO T_MSG OPTIONS FROM LW_BDC_OPT.

    IF SY-SUBRC NE 0.

      LOOP AT T_MSG.
        IF T_MSG-MSGTYP EQ 'E'.
*       it_msg-msgtyp eq 'S'.

          MOVE: T_MSG-MSGID TO V_MESSAGE_ID,
                T_MSG-MSGNR TO V_MESSAGE_NUMBER,
                T_MSG-MSGV1 TO V_MESSAGE_VAR1,
                T_MSG-MSGV2 TO V_MESSAGE_VAR2,
                T_MSG-MSGV3 TO V_MESSAGE_VAR3,
                T_MSG-MSGV4 TO V_MESSAGE_VAR4.

          CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
            EXPORTING
              MESSAGE_ID        = V_MESSAGE_ID
              MESSAGE_NUMBER    = V_MESSAGE_NUMBER
              MESSAGE_VAR1      = V_MESSAGE_VAR1
              MESSAGE_VAR2      = V_MESSAGE_VAR2
              MESSAGE_VAR3      = V_MESSAGE_VAR3
              MESSAGE_VAR4      = V_MESSAGE_VAR4
            IMPORTING
              MESSAGE_TEXT      = V_MSG
            EXCEPTIONS
              MESSAGE_NOT_FOUND = 1
              OTHERS            = 2.

          FORMAT COLOR COL_NEGATIVE.
          WRITE:/ WA_ARQUIVO-WERKS,
                  WA_ARQUIVO-MATNR,
                  WA_ARQUIVO-MATKL, '-->',
                  V_MSG.
          SKIP 1.
*          WRITE:/ .
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_grava_mm02
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE_BDC
*&---------------------------------------------------------------------*
FORM ZF_INSERE_BDC USING P_PROGRAM  TYPE BDCDATA-PROGRAM
                         P_DYNPRO   TYPE BDCDATA-DYNPRO
                         P_DYNBEGIN TYPE BDCDATA-DYNBEGIN
                         P_FNAM     TYPE BDCDATA-FNAM
                         P_FVAL     TYPE ANY.

  W_BDC-PROGRAM  = P_PROGRAM.
  W_BDC-DYNPRO   = P_DYNPRO.
  W_BDC-DYNBEGIN = P_DYNBEGIN.
  W_BDC-FNAM     = P_FNAM.
  W_BDC-FVAL     = P_FVAL.

  APPEND W_BDC TO T_BDC.

ENDFORM.                    " ZF_INSERE_BDC
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_MM_DEP
*&---------------------------------------------------------------------*
FORM F_GRAVA_MM_DEP .

  DATA: VL_RGATU         TYPE N,
        VL_MSG           TYPE C LENGTH 50.
  CLEAR VL_RGATU.

  LW_BDC_OPT-DEFSIZE  = C_X.
  LW_BDC_OPT-RACOMMIT = C_X.
  LW_BDC_OPT-NOBINPT  = C_X.
  LW_BDC_OPT-DISMODE  = 'N'.

  LOOP AT IT_ARQUIVO INTO WA_ARQUIVO.

    REFRESH T_BDC.

    PERFORM ZF_INSERE_BDC USING:
          'SAPLMGMM' '0060' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'  '=ENTR',
          SPACE      C_0000 SPACE 'RMMG1-MATNR' WA_ARQUIVO-MATNR,

          'SAPLMGMM' '0070' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'  '=ENTR',
          "SPACE      C_0000 SPACE 'MSICHTAUSW-KZSEL(06)' 'X',


          'SAPLMGMM' '0080' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'  '=ENTR',
          SPACE      C_0000 SPACE 'RMMG1-WERKS' WA_ARQUIVO-WERKS,
          SPACE      C_0000 SPACE 'RMMG1-LGORT' WA_ARQUIVO-LGORT,



          'SAPLMGMM' '4000' C_X   SPACE SPACE,
          SPACE      C_0000 SPACE 'BDC_OKCODE'      '=BU',
*          SPACE      C_0000 SPACE 'MAKT-MAKTX'      WA_ARQUIVO-MAKTX,
*          SPACE      C_0000 SPACE 'MARA-MEINS'      'UN',
          SPACE      C_0000 SPACE 'MARD-LGPBE'      WA_ARQUIVO-LGPBE.
    CALL TRANSACTION 'MM02' USING T_BDC
       MESSAGES INTO T_MSG OPTIONS FROM LW_BDC_OPT.

    IF SY-SUBRC NE 0.

      LOOP AT T_MSG.
        IF T_MSG-MSGTYP EQ 'E'.
*       it_msg-msgtyp eq 'S'.

          MOVE: T_MSG-MSGID TO V_MESSAGE_ID,
                T_MSG-MSGNR TO V_MESSAGE_NUMBER,
                T_MSG-MSGV1 TO V_MESSAGE_VAR1,
                T_MSG-MSGV2 TO V_MESSAGE_VAR2,
                T_MSG-MSGV3 TO V_MESSAGE_VAR3,
                T_MSG-MSGV4 TO V_MESSAGE_VAR4.

          CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
            EXPORTING
              MESSAGE_ID        = V_MESSAGE_ID
              MESSAGE_NUMBER    = V_MESSAGE_NUMBER
              MESSAGE_VAR1      = V_MESSAGE_VAR1
              MESSAGE_VAR2      = V_MESSAGE_VAR2
              MESSAGE_VAR3      = V_MESSAGE_VAR3
              MESSAGE_VAR4      = V_MESSAGE_VAR4
            IMPORTING
              MESSAGE_TEXT      = V_MSG
            EXCEPTIONS
              MESSAGE_NOT_FOUND = 1
              OTHERS            = 2.

          FORMAT COLOR COL_NEGATIVE.
          WRITE:/ WA_ARQUIVO-WERKS,
                  WA_ARQUIVO-MATNR,
                  WA_ARQUIVO-MATKL, '-->',
                  V_MSG.
          SKIP 1.
*          WRITE:/ .
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_GRAVA_MM_DEP

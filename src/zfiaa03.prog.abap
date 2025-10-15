*&---------------------------------------------------------------------*
*& Report  ZFIAA03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFIAA03.

TYPES: BEGIN OF TY_CARGA_AS02,
  BUKRS         TYPE ANLA-BUKRS,
  ANLN1         TYPE ANLA-ANLN1,
  ANLN2         TYPE ANLA-ANLN2,
  VIDA_UTIL     TYPE ANLB-NDJAR,
  ID1           TYPE ANLB-NDPER,
  VDE           TYPE ANLC-NDABJ,
  VIDA_REMANESC TYPE ANLC-NDABP,
END OF TY_CARGA_AS02.

DATA: GT_PLANILHA   LIKE STANDARD TABLE OF ALSMEX_TABLINE,
      GT_CARGA_AS02 TYPE TABLE OF TY_CARGA_AS02,
      GT_BDC        TYPE TABLE OF BDCDATA,
      WL_CARGA_AS02 TYPE TY_CARGA_AS02,
      WL_PLANILHA   LIKE ALSMEX_TABLINE,
      OPT           TYPE CTU_PARAMS.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_FILE TYPE RLGRAP-FILENAME DEFAULT ''.

SELECTION-SCREEN SKIP.

PARAMETERS: P_MOD1 RADIOBUTTON GROUP G1 DEFAULT 'X',
            P_MOD2 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = P_FILE
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.

START-OF-SELECTION.
  PERFORM: F_GET_EXCEL, F_TRATAR_EXCEL.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ARCHIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_EXCEL.
  REFRESH: GT_PLANILHA.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Importando arquivos...'.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 55
      I_END_ROW               = 50000
    TABLES
      INTERN                  = GT_PLANILHA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
ENDFORM.                    "F_GET_ARCHIVE

*&---------------------------------------------------------------------*
*&      Form  F_TRATAR_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_TRATAR_EXCEL.
  REFRESH GT_CARGA_AS02.
  LOOP AT GT_PLANILHA INTO WL_PLANILHA.

    AT NEW ROW.
      CLEAR WL_CARGA_AS02.
    ENDAT.

    CASE WL_PLANILHA-COL.
      WHEN 1.
        WL_CARGA_AS02-BUKRS = WL_PLANILHA-VALUE.
      WHEN 2.
        WL_CARGA_AS02-ANLN1 = WL_PLANILHA-VALUE.
      WHEN 3.
        WL_CARGA_AS02-ANLN2 = WL_PLANILHA-VALUE.
      WHEN 4.
        WL_CARGA_AS02-VIDA_UTIL = WL_PLANILHA-VALUE.
      WHEN 5.
        WL_CARGA_AS02-ID1 = WL_PLANILHA-VALUE.
      WHEN 6.
        WL_CARGA_AS02-VDE = WL_PLANILHA-VALUE.
      WHEN 7.
        WL_CARGA_AS02-VIDA_REMANESC = WL_PLANILHA-VALUE.
    ENDCASE.

    AT END OF ROW.
      APPEND WL_CARGA_AS02 TO GT_CARGA_AS02.
    ENDAT.
  ENDLOOP.

  IF ( P_MOD1 = 'X' ).

    LOOP AT GT_CARGA_AS02 INTO WL_CARGA_AS02.
      REFRESH GT_BDC.

      PERFORM F_PREENCHE_DYNPRO USING:
      'X' 'SAPLAIST'       '0100',
      ''  'BDC_OKCODE'     '/00',
      ''  'ANLA-ANLN1'     WL_CARGA_AS02-ANLN1,
      ''  'ANLA-ANLN2'     WL_CARGA_AS02-ANLN2,
      ''  'ANLA-BUKRS'     WL_CARGA_AS02-BUKRS,
      'X' 'SAPLAIST'       '1000',
      ''  'BDC_OKCODE'     '=TAB08',
      ''  'BDC_SUBSCR'     'SAPLAIST                                0099KOPF',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0100TABSTRIP',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0200SUBSC',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1140AREA1',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1141AREA2',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1142AREA3',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA4',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA5',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA6',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA7',
      'X' 'SAPLAIST'       '1000',
      ''  'BDC_OKCODE'     '=BUCH',
      ''  'BDC_SUBSCR'     'SAPLAIST                                0099KOPF',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0100TABSTRIP',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0201SUBSC',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1190AREA1',
      ''  'ANLB-NDJAR(01)' WL_CARGA_AS02-VIDA_UTIL,
      ''  'ANLB-NDJAR(04)' WL_CARGA_AS02-VIDA_UTIL,
      ''  'ANLB-NDJAR(05)' WL_CARGA_AS02-VIDA_UTIL,
      ''  'ANLB-NDPER(01)' WL_CARGA_AS02-ID1,
      ''  'ANLB-NDPER(04)' WL_CARGA_AS02-ID1,
      ''  'ANLB-NDPER(05)' WL_CARGA_AS02-ID1,
      ''  'ANLC-NDABJ(01)' WL_CARGA_AS02-VDE,
      ''  'ANLC-NDABJ(04)' WL_CARGA_AS02-VDE,
      ''  'ANLC-NDABJ(05)' WL_CARGA_AS02-VDE,
      ''  'ANLC-NDABP(01)' WL_CARGA_AS02-VIDA_REMANESC,
      ''  'ANLC-NDABP(04)' WL_CARGA_AS02-VIDA_REMANESC,
      ''  'ANLC-NDABP(05)' WL_CARGA_AS02-VIDA_REMANESC.

      CALL TRANSACTION 'AS02' USING GT_BDC MODE 'N'.
    ENDLOOP.

  ELSE.
    LOOP AT GT_CARGA_AS02 INTO WL_CARGA_AS02.
      REFRESH GT_BDC.

      PERFORM F_PREENCHE_DYNPRO USING:
      'X' 'SAPLAIST'       '0100',
      ''  'BDC_OKCODE'     '/00',
      ''  'ANLA-ANLN1'     WL_CARGA_AS02-ANLN1,
      ''  'ANLA-ANLN2'     WL_CARGA_AS02-ANLN2,
      ''  'ANLA-BUKRS'     WL_CARGA_AS02-BUKRS,
      'X' 'SAPLAIST'       '1000',
      ''  'BDC_OKCODE'     '=TAB08',
      ''  'BDC_SUBSCR'     'SAPLAIST                                0099KOPF',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0100TABSTRIP',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0200SUBSC',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1140AREA1',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1141AREA2',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1142AREA3',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA4',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA5',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA6',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0300AREA7',
      'X' 'SAPLAIST'       '1000',
      ''  'BDC_OKCODE'     '=BUCH',
      ''  'BDC_SUBSCR'     'SAPLAIST                                0099KOPF',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0100TABSTRIP',
      ''  'BDC_SUBSCR'     'SAPLATAB                                0201SUBSC',
      ''  'BDC_SUBSCR'     'SAPLAIST                                1190AREA1',
      ''  'ANLB-NDJAR(01)' WL_CARGA_AS02-VIDA_UTIL,
      ''  'ANLB-NDJAR(03)' WL_CARGA_AS02-VIDA_UTIL,
      ''  'ANLB-NDJAR(04)' WL_CARGA_AS02-VIDA_UTIL,
      ''  'ANLB-NDPER(01)' WL_CARGA_AS02-ID1,
      ''  'ANLB-NDPER(03)' WL_CARGA_AS02-ID1,
      ''  'ANLB-NDPER(04)' WL_CARGA_AS02-ID1.

      CALL TRANSACTION 'AS02' USING GT_BDC MODE 'N'.
    ENDLOOP.
  ENDIF.

  MESSAGE 'Carga concluída!' TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM.                    "F_TRATAR_EXCEL

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_PREENCHE_DYNPRO USING I_START
                             I_NAME
                             I_VALUE.

  DATA: WL_BDC TYPE BDCDATA.

  MOVE I_START TO WL_BDC-DYNBEGIN.

  IF ( I_START = 'X' ).
    WL_BDC-PROGRAM = I_NAME.
    WL_BDC-DYNPRO  = I_VALUE.
  ELSE.
    WL_BDC-FNAM  = I_NAME.
    WL_BDC-FVAL  = I_VALUE.
  ENDIF.

  APPEND WL_BDC TO GT_BDC.
  CLEAR WL_BDC.
ENDFORM.                    "F_PREENCHE_DYNPRO

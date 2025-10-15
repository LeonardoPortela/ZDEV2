*&---------------------------------------------------------------------*
*& Report  ZCOR019
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCOR019.


TYPES: BEGIN OF TY_CARGA_MR22,
         BUDAT  TYPE MR21HEAD-BUDAT,
         BUKRS  TYPE MR21HEAD-BUKRS,
         WERKS  TYPE MR21HEAD-WERKS,
         XBLNR  TYPE MR21HEAD-XBLNR,
         BKTXT  TYPE MR21HEAD-BKTXT,
         MATNR  TYPE CKI_MR22_0250-MATNR,
         ZUUMB  TYPE STRING, "REAIS
         ZUUMB2 TYPE STRING, "DOLAR
       END OF TY_CARGA_MR22.

DATA: GT_PLANILHA   TYPE STANDARD TABLE OF ALSMEX_TABLINE,
      GT_CARGA_MR22 TYPE TABLE OF TY_CARGA_MR22,
      GT_BDC        TYPE TABLE OF BDCDATA,
      GW_PLANILHA   TYPE ALSMEX_TABLINE,
      GW_BDC        TYPE BDCDATA,
      GW_CARGA_MR22 TYPE TY_CARGA_MR22.

DEFINE D_PREENCHE_SHDB.
  CLEAR GW_BDCDATA.
  GW_BDCDATA-PROGRAM   = &1.
  GW_BDCDATA-DYNPRO    = &2.
  GW_BDCDATA-DYNBEGIN  = &3.
  GW_BDCDATA-FNAM      = &4.
  GW_BDCDATA-FVAL      = &5.

  REPLACE '.' WITH ',' INTO GW_BDCDATA-FVAL.
  CONDENSE GW_BDCDATA-FVAL NO-GAPS.

  APPEND GW_BDCDATA TO GT_BDCDATA.
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-I01.
PARAMETERS P_FILE TYPE RLGRAP-FILENAME.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM F_CARREGA_ARQUIVO USING P_FILE.


START-OF-SELECTION.
  PERFORM F_TRATA_ARQUIVO.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGA_ARQUIVO USING I_FILENAME.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = I_FILENAME
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar'
    IMPORTING
      FILENAME         = I_FILENAME
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.
ENDFORM.                    "F_CARREGA_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_TRATA_ARQUIVO.

  CHECK ( P_FILE IS NOT INITIAL ).

  REFRESH: GT_PLANILHA,
           GT_CARGA_MR22,
           GT_BDC.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = TEXT-I02.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 55
      I_END_ROW               = 10000
    TABLES
      INTERN                  = GT_PLANILHA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  REFRESH GT_CARGA_MR22.

  LOOP AT GT_PLANILHA INTO GW_PLANILHA.
    CASE GW_PLANILHA-COL.
      WHEN 1.
        REPLACE ALL OCCURRENCES OF '.' IN GW_PLANILHA-VALUE WITH ' '.
        GW_CARGA_MR22-BUDAT = GW_PLANILHA-VALUE.
      WHEN 2.
        GW_CARGA_MR22-BUKRS = GW_PLANILHA-VALUE.
      WHEN 3.
        GW_CARGA_MR22-WERKS = GW_PLANILHA-VALUE.
      WHEN 4.
        GW_CARGA_MR22-XBLNR = GW_PLANILHA-VALUE.
      WHEN 5.
        GW_CARGA_MR22-BKTXT =	GW_PLANILHA-VALUE.

      WHEN 6.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GW_PLANILHA-VALUE
          IMPORTING
            OUTPUT = GW_CARGA_MR22-MATNR.

      WHEN 7.
        PERFORM F_TRATAR_CAMPO CHANGING GW_PLANILHA-VALUE.
        GW_CARGA_MR22-ZUUMB = GW_PLANILHA-VALUE.
      WHEN 8.
        PERFORM F_TRATAR_CAMPO CHANGING GW_PLANILHA-VALUE.
        GW_CARGA_MR22-ZUUMB2 = GW_PLANILHA-VALUE.
    ENDCASE.

    AT END OF ROW.
      APPEND GW_CARGA_MR22 TO GT_CARGA_MR22.
      CLEAR GW_CARGA_MR22.
    ENDAT.
  ENDLOOP.

  IF ( NOT GT_CARGA_MR22 IS INITIAL ).
    PERFORM F_CARGA_MR22.
  ELSE.
    MESSAGE TEXT-I04 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    "F_CARREGA_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  F_SHDB_EXECUTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARGA_MR22.
  DATA: LT_MESSAGE TYPE TABLE OF BDCMSGCOLL,
        LINHA(5),
* ---> S4 Migração - 19/06/2023 - FC - Inicio
        "VMSG(50),
        VMSG(62),
* <--- S4 Migração - 19/06/2023 - FC - Fim
        P_MODE(1).

  LOOP AT GT_CARGA_MR22 INTO GW_CARGA_MR22.
    LINHA = SY-TABIX.
    CONCATENATE 'Linha ' LINHA 'Material'  GW_CARGA_MR22-MATNR INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    PERFORM F_BDC_FIELD USING:  'X' 'SAPRCKM_MR22'             '0201',
                                ' ' 'BDC_OKCODE'               '=ENTR',
                                ' ' 'MR21HEAD-BUDAT'           GW_CARGA_MR22-BUDAT,
                                ' ' 'MR21HEAD-BUKRS'           GW_CARGA_MR22-BUKRS,
                                ' ' 'MR21HEAD-WERKS'           GW_CARGA_MR22-WERKS,
                                ' ' 'MR21HEAD-XBLNR'           GW_CARGA_MR22-XBLNR,
                                ' ' 'MR21HEAD-BKTXT'           GW_CARGA_MR22-BKTXT.

    PERFORM F_BDC_FIELD USING:  'X' 'SAPRCKM_MR22'             '0201',
                                ' ' 'BDC_OKCODE'               '=TAB2',
                                ' ' 'CKI_MR22_0250-MATNR(01)'  GW_CARGA_MR22-MATNR,
                                ' ' 'CKI_MR22_0250-ZUUMB(01)'  GW_CARGA_MR22-ZUUMB. "reais


    PERFORM F_BDC_FIELD USING: 'X' 'SAPRCKM_MR22'             '0201',
                               ' ' 'BDC_OKCODE'               '=TAB3',
                               ' ' 'CKI_MR22_0250-ZUUMB(01)'  GW_CARGA_MR22-ZUUMB2. "dolar

    PERFORM F_BDC_FIELD USING: 'X' 'SAPRCKM_MR22'             '0201',
                               ' ' 'BDC_OKCODE'               '=CALC',
                               ' ' 'CKI_MR22_0250-SELKZ(01)' 'X'.


    PERFORM F_BDC_FIELD USING: 'X' 'SAPRCKM_MR22'              '0400',
                               ' ' 'BDC_OKCODE'               '=ENTR'.
    IF GW_CARGA_MR22-ZUUMB IS NOT INITIAL AND GW_CARGA_MR22-ZUUMB NE '0,00'.
      PERFORM F_BDC_FIELD USING: ' ' 'DISPLAY-F_CURR1-SELKZ' 'X'.
    ELSE.
      PERFORM F_BDC_FIELD USING: ' ' 'DISPLAY-F_CURR2-SELKZ' 'X'.
    ENDIF.

    PERFORM F_BDC_FIELD USING:  'X' 'SAPRCKM_MR22'              '0201',
                                ' ' 'BDC_OKCODE'               '=SAVE'.

    P_MODE = 'N'.
    CALL TRANSACTION 'MR22' USING GT_BDC MODE P_MODE MESSAGES INTO LT_MESSAGE.
  ENDLOOP.

  MESSAGE TEXT-I03 TYPE 'S'.
ENDFORM.                    "F_SHDB_EXECUTION

*&---------------------------------------------------------------------*
*&      Form  f_TRATAR_CAMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->V_VALUE    text
*----------------------------------------------------------------------*
FORM F_TRATAR_CAMPO CHANGING P_VALUE.
  REPLACE ALL OCCURRENCES OF '.' IN P_VALUE WITH ' '.
  CONDENSE P_VALUE NO-GAPS.
ENDFORM.                    "tratar_campo

*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_FLAG)  text
*      -->VALUE(P_FNAM)  text
*      -->VALUE(P_FVAL)  text
*----------------------------------------------------------------------*
FORM F_BDC_FIELD  USING    VALUE(P_FLAG)
                           VALUE(P_FNAM)
                           VALUE(P_FVAL).

  IF NOT P_FLAG IS INITIAL.
    GW_BDC-PROGRAM  = P_FNAM.
    GW_BDC-DYNPRO   = P_FVAL.
    GW_BDC-DYNBEGIN = 'X'.
  ELSE.
    GW_BDC-FNAM = P_FNAM.
    GW_BDC-FVAL = P_FVAL.
  ENDIF.

  APPEND GW_BDC TO GT_BDC.
  CLEAR GW_BDC.
ENDFORM.                    " F_BDC_FIELD

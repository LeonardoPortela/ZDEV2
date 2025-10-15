*&---------------------------------------------------------------------*
*& Report  Z_AJUSTE_REQUISI_PM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_AJUSTE_REQUISI_PM.

* Tipos
TYPES: BEGIN OF TY_REQUISI,
         BANFN TYPE EBAN-BANFN,
         BNFPO TYPE EBAN-BNFPO,
       END OF TY_REQUISI.

* Tabelas internas
DATA: GT_REQUISI TYPE TABLE OF TY_REQUISI.

* Work Área
DATA  GW_REQUISI TYPE TY_REQUISI.

PERFORM: F_IMPORTAR CHANGING SY-SUBRC.
IF SY-SUBRC IS INITIAL.
  PERFORM F_ATUALIZA_REQUI.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_atualiza_requi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ATUALIZA_REQUI.
  DATA: LV_MSG   TYPE C LENGTH 255,
        LV_BANFN TYPE EBAN-BANFN,
        LV_BNFPO TYPE EBAN-BNFPO.

  FIELD-SYMBOLS: <FS_REQUISI> TYPE TY_REQUISI.

  LOOP AT GT_REQUISI ASSIGNING <FS_REQUISI>.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <FS_REQUISI>-BANFN
      IMPORTING
        OUTPUT = LV_BANFN.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <FS_REQUISI>-BNFPO
      IMPORTING
        OUTPUT = LV_BNFPO.

    UPDATE EBAN SET LOEKZ = 'X'
    WHERE BANFN = LV_BANFN
     AND  BNFPO = LV_BNFPO.

    IF SY-DBCNT IS NOT INITIAL.
      CONCATENATE 'Requisição atualizada: ' <FS_REQUISI>-BANFN 'item -> ' <FS_REQUISI>-BNFPO INTO LV_MSG SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Erro ao atualizadar requisição: ' <FS_REQUISI>-BANFN 'item -> ' <FS_REQUISI>-BNFPO INTO LV_MSG SEPARATED BY SPACE.
    ENDIF.

    WRITE LV_MSG.
  ENDLOOP.
ENDFORM.                    "f_atualiza_requi

*&---------------------------------------------------------------------*
*&      Form  F_IMPORTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_IMPORTAR CHANGING C_ERRO TYPE SY-SUBRC.

  DATA: WL_SEQ        TYPE I,
        WL_SEQ1       TYPE C LENGTH 11,
        WL_SEQ2       TYPE C LENGTH 5,
        LV_MSG        TYPE C LENGTH 255,
        LV_FILE       TYPE RLGRAP-FILENAME,
        TL_FILE_TABLE TYPE FILETABLE,
        WL_FILE_TABLE LIKE LINE OF TL_FILE_TABLE,
        RC            TYPE I.

  DATA: TL_EXCEL     LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
        TL_EXCEL_AUX LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .

  C_ERRO = 4.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE              = TL_FILE_TABLE
      RC                      = RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  READ TABLE TL_FILE_TABLE INTO WL_FILE_TABLE INDEX 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WL_FILE_TABLE-FILENAME
    IMPORTING
      OUTPUT = LV_FILE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = LV_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 1
      I_END_COL               = 2
      I_END_ROW               = 10000
    TABLES
      INTERN                  = TL_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE 'Parâmetros inválidos.' TYPE 'E'.
    WHEN 2.
      MESSAGE 'Não foi possível ler o arquivo, verifique o caminho e tente novamente.' TYPE 'E'.
    WHEN 3.
      MESSAGE 'Erro ao importar o arquivo.' TYPE 'E'.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Atualizando Dados'.

  TL_EXCEL_AUX[] = TL_EXCEL[].

  SORT TL_EXCEL_AUX BY ROW COL.

  CLEAR: TL_EXCEL_AUX, WL_SEQ.

  LOOP AT TL_EXCEL.
    IF TL_EXCEL-ROW = TL_EXCEL_AUX-ROW.
      CONTINUE.
    ENDIF.
    LOOP AT TL_EXCEL_AUX WHERE ROW = TL_EXCEL-ROW.
      CASE TL_EXCEL_AUX-COL.
        WHEN 1.
          GW_REQUISI-BANFN = TL_EXCEL_AUX-VALUE.
        WHEN 2.
          GW_REQUISI-BNFPO = TL_EXCEL_AUX-VALUE.
      ENDCASE.
    ENDLOOP.

    ADD 1 TO WL_SEQ.

    WL_SEQ1 = WL_SEQ.

    CONCATENATE 'Gravando linha:' WL_SEQ1 INTO LV_MSG.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = LV_MSG.

    APPEND GW_REQUISI TO GT_REQUISI.

  ENDLOOP.

  MESSAGE 'Importação concluída' TYPE 'S'.

  CLEAR C_ERRO.

ENDFORM.                    "f_importar

*&---------------------------------------------------------------------*
*& Report  ZATUAL_CLIENTE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_ZSDT0023.

TYPES: BEGIN OF TY_REMESSA,
         VBELN   TYPE ZSDT0023-VBELN,
       END OF TY_REMESSA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:  VMSG(50),
      WA_REMESSA    TYPE TY_REMESSA.


PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT ''.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = P_FILE
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar !'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

START-OF-SELECTION.

  DATA: T_EXCEL   LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
        T_EXCEL2  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .


  CLEAR T_EXCEL.
  REFRESH T_EXCEL.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 1
      I_END_ROW               = 5600
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Atualizando Dados'.

  T_EXCEL2[] = T_EXCEL[].
  SORT T_EXCEL2 BY ROW COL.
  CLEAR T_EXCEL2.
  PERFORM F_REMESSA.


  MESSAGE 'Fim atualização' TYPE 'I'.
*&---------------------------------------------------------------------*
*&      Form  F_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_REMESSA .
  LOOP AT T_EXCEL.
    IF T_EXCEL-ROW = T_EXCEL2-ROW.
      CONTINUE.
    ENDIF.
    LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
      CASE T_EXCEL2-COL.
        WHEN 1.
          WA_REMESSA-VBELN        = T_EXCEL2-VALUE.
      ENDCASE.
    ENDLOOP.
    CONCATENATE 'Linha ' T_EXCEL-ROW 'Nome '  WA_REMESSA-VBELN INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    UPDATE ZSDT0023 SET LGORT_V = 'PO58'
    WHERE VBELN = WA_REMESSA-VBELN.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.                    " F_CLIENTE

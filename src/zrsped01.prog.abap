*&---------------------------------------------------------------------*
*& Report  ZRSPED01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZRSPED01.

TABLES ZSPED002.

TYPES: BEGIN OF TY_AGLUT,
         KTOPL TYPE ZSPED002-KTOPL,
         VERSN TYPE ZSPED002-VERSN,
         BILKT TYPE RF011Z-BILKT,
         VONKT TYPE RF011Z-VONKT,
         ERGSO TYPE RF011Z-ERGSO.
TYPES: END OF TY_AGLUT.

DATA: IT_ZSPED002 TYPE STANDARD TABLE OF ZSPED002,
      WA_ZSPED002 TYPE ZSPED002.

*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS LCL_EVENTHANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      HANDLE_DATA_CHANGED
            FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING
            ER_DATA_CHANGED
            E_ONF4
            E_ONF4_BEFORE
            E_ONF4_AFTER
            E_UCOMM
            SENDER,

      HANDLE_DATA_CHANGED_FINISHED
            FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING
            E_MODIFIED
            ET_GOOD_CELLS.

ENDCLASS.                    "lcl_eventhandler DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS LCL_EVENTHANDLER IMPLEMENTATION.

  METHOD HANDLE_DATA_CHANGED.
    CLEAR ER_DATA_CHANGED->MT_PROTOCOL.
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_DATA_CHANGED_FINISHED.

  ENDMETHOD.                    "handle_data_changed_finished

ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: P_BUKRS FOR ZSPED002-BUKRS,
                P_KTOPL FOR ZSPED002-KTOPL,
                P_VERSN FOR ZSPED002-VERSN,
                P_DTINI FOR ZSPED002-DT_INIC_VAL,
                P_DTFIM FOR ZSPED002-DT_FIM_VAL.
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.

  PERFORM LIMPAR_TABELAS.
  PERFORM SELECIONAR_REGISTROS.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TABELAS .
  CLEAR: IT_ZSPED002.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_REGISTROS .

  SELECT *
    FROM ZSPED002
    INTO TABLE IT_ZSPED002
    WHERE BUKRS       IN P_BUKRS
      AND KTOPL       IN P_KTOPL
      AND VERSN       IN P_VERSN
      AND DT_INIC_VAL IN P_DTINI
      AND DT_FIM_VAL  IN P_DTFIM.

  SORT IT_ZSPED002 BY BUKRS KTOPL VERSN SAKNR ASCENDING.

ENDFORM.

INCLUDE ZRSPED01_0100.

INCLUDE ZRSPED01_0002.

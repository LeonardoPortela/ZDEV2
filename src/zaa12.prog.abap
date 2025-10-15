*&---------------------------------------------------------------------*
*& Report  ZAA10
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZAA12.

TABLES ZAA006.

DATA: IT_ZAA006      TYPE STANDARD TABLE OF ZAA006,
      IT_ZAA006_BASE TYPE STANDARD TABLE OF ZAA006,
      WA_ZAA006      TYPE ZAA006,
      OK_CODE        TYPE SY-UCOMM.

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

  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_DATA_CHANGED_FINISHED.

  ENDMETHOD.                    "handle_data_changed_finished

ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION

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
  CLEAR: IT_ZAA006.
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
    FROM ZAA006
    INTO TABLE IT_ZAA006.

  SORT IT_ZAA006 BY BUKRS GJAHR ASCENDING.

ENDFORM.

INCLUDE ZAA12_0100.

INCLUDE ZAA12_0002.

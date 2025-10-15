*&---------------------------------------------------------------------*
*& Report  ZAA10
*& Programa: Inventário de Imobilizado - Parâmetro de usuários
*&           Cadastro de Aprovadores   - Fluxo de baixa
*&Transação: ZAA15
*&---------------------------------------------------------------------*
REPORT ZAA10.


*---------------------------------------------------------------------*
*       TABLES
*---------------------------------------------------------------------*
TABLES ZAA004.


*=======================================================================
* Constantes
*=======================================================================

CONSTANTS: BEGIN OF C_MAIN_TAB,
             TAB1 LIKE SY-UCOMM VALUE 'MAIN_TAB_TAB1',
             TAB2 LIKE SY-UCOMM VALUE 'MAIN_TAB_TAB2',
           END OF C_MAIN_TAB.

*---------------------------------------------------------------------*
*       DATA
*---------------------------------------------------------------------*
DATA: IT_ZAA004       TYPE STANDARD TABLE OF ZAA004,
      IT_ZAA004_BASE  TYPE STANDARD TABLE OF ZAA004,
      IT_ZAA004_BAIXA TYPE STANDARD TABLE OF ZAA004,
      WA_ZAA004       TYPE ZAA004,
      OK_CODE         TYPE SY-UCOMM.

DATA: BEGIN OF I_MAIN_TAB,                                              "Tabela para controle das tabs selecionadas na screen 9000.
        SUBSCREEN   LIKE SY-DYNNR,                                      "Subscreen
        PROG        LIKE SY-REPID VALUE 'ZAA10',                     "Program
        PRESSED_TAB LIKE SY-UCOMM VALUE C_MAIN_TAB-TAB1,                "Tab
      END OF I_MAIN_TAB.

*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
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
FORM LIMPAR_TABELAS .
  CLEAR: IT_ZAA004, IT_ZAA004_BAIXA.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_REGISTROS
*&---------------------------------------------------------------------*
FORM SELECIONAR_REGISTROS .

  SELECT *
    FROM ZAA004
    INTO TABLE IT_ZAA004
    WHERE FLUXO_BAIXA EQ ''.

  SORT IT_ZAA004 BY GSBER KOSTL GJAHR NIVEL_AA ASCENDING.

  SELECT *
    FROM ZAA004
    INTO TABLE IT_ZAA004_BAIXA
    WHERE FLUXO_BAIXA EQ 'X'.

  SORT IT_ZAA004_BAIXA BY BUKRS GSBER KOSTL NIVEL_AA ASCENDING.

ENDFORM.

INCLUDE ZAA10_0100.

INCLUDE ZAA10_0002.

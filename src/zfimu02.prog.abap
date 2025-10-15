*&--------------------------------------------------------------------&*
*&                        FI                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 07/04/2016                                              &*
*& Descrição: Aliquota IOF                                            &*
*& Transação: ZFIMU02                                                 &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Request DEVK956038     Data 01/04/2016  IR IR98191                 &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
PROGRAM ZFIMU02.
TABLES ZFIALIOFMU.
DATA: IT_ZFIALIOFMU TYPE TABLE OF ZFIALIOFMU,
      WA_ZFIALIOFMU TYPE ZFIALIOFMU,
      IT_SELECT_ROWS  TYPE LVC_T_ROW,
      WA_SELECT_ROWS    TYPE LVC_S_ROW.

DATA: OK_CODE LIKE SY-UCOMM,
      SAVE_OK LIKE SY-UCOMM,
      G_CONTAINER TYPE SCRFNAME VALUE 'BCALV_GRID_DEMO_0100_CONT1',
      G_GRID  TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRID1 TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT TYPE LVC_S_LAYO.
*      G_MAX TYPE I VALUE 100.


DATA: GT_OUTTAB TYPE TABLE OF ZFIALIOFMU.

*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*
CALL SCREEN 100.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAIN100'.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.
    CREATE OBJECT G_GRID
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.
*§1.Set status of all cells to editable using the layout structure.
    GS_LAYOUT-EDIT = 'X'.

    SELECT * FROM ZFIALIOFMU INTO TABLE GT_OUTTAB.
*      UP TO G_MAX ROWS.

      CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          I_STRUCTURE_NAME = 'ZFIALIOFMU'
          IS_LAYOUT        = GS_LAYOUT
        CHANGING
          IT_OUTTAB        = GT_OUTTAB.
*§2.Use SET_READY_FOR_INPUT to allow editing initially.
*   (state "editable and ready for input").

      CALL METHOD G_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 0.
*    ELSE.
*            CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.

    ENDIF.
  ENDMODULE.                    "pbo OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE PAI INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  DATA: L_VALID(1) TYPE C.

  CASE SAVE_OK.
    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.
    WHEN 'SWITCH'.
      PERFORM SWITCH_EDIT_MODE.
    WHEN 'SAVE'.
      CALL METHOD G_GRID->CHECK_CHANGED_DATA
        IMPORTING
          E_VALID = GS_LAYOUT-EDIT.
      IF GS_LAYOUT-EDIT = 'X'.
        MOVE GT_OUTTAB TO IT_ZFIALIOFMU.
*        LOOP AT  GT_OUTTAB INTO WA_ZFIALIOFMU .
          modify  ZFIALIOFMU from TABLE GT_OUTTAB.
          commit work.
*        ENDLOOP.
      ENDIF.
    WHEN 'EXCLUIR'.
      PERFORM  EXCLUIR.

    WHEN OTHERS.
*     do nothing
  ENDCASE.
ENDMODULE.                    "pai INPUT
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM EXIT_PROGRAM.
  LEAVE PROGRAM.
ENDFORM.                    "exit_program
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*

FORM SWITCH_EDIT_MODE.
*§3.Use IS_READY_FOR_INPUT to fetch current substate of editable cells.
  IF G_GRID->IS_READY_FOR_INPUT( ) EQ 0.
*§4.Use SET_READY_FOR_INPUT to switch between the substates.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.
  ELSE.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 0.
  ENDIF.

ENDFORM.                               " SWITCH_EDIT_MODE


*&---------------------------------------------------------------------*
*&      Form  EXCLUIR
*&---------------------------------------------------------------------*
FORM EXCLUIR .

  IF G_GRID->IS_READY_FOR_INPUT( ) EQ 0.
*§4.Use SET_READY_FOR_INPUT to switch between the substates.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 0.
  ELSE.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECT_ROWS.


    LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
      READ TABLE GT_OUTTAB INTO WA_ZFIALIOFMU INDEX WA_SELECT_ROWS-INDEX.
      DELETE FROM ZFIALIOFMU
      WHERE ID_ALIQUOTA_IOF = WA_ZFIALIOFMU-ID_ALIQUOTA_IOF.
            DELETE GT_OUTTAB  INDEX WA_SELECT_ROWS-INDEX.

    ENDLOOP.
        CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          I_STRUCTURE_NAME = 'ZFIALIOFMU'
          IS_LAYOUT        = GS_LAYOUT
        CHANGING
          IT_OUTTAB        = GT_OUTTAB.
  ENDIF.

ENDFORM.                    " EXCLUIR
**&---------------------------------------------------------------------*
**&      Form  INCLUIR
**&---------------------------------------------------------------------*
FORM ATUALIZAR .

*CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " INCLUIR

*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIAR_OBJETOS OUTPUT.

  DATA: LT_SORT TYPE LVC_T_SORT.

  CLEAR GV_ERRO.

  IF OBJ_CONTAINER IS INITIAL.

    PERFORM SELECIONAR_DADOS.
    PERFORM REFRESH_OBJETOS_ALV.
    PERFORM CRIAR_FIELD_CATALOG.

    CREATE OBJECT OBJ_CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CC_PARAMETROS'.

    CREATE OBJECT OBJ_ALV
      EXPORTING
        I_PARENT = OBJ_CONTAINER.

    CREATE OBJECT OBJ_TOOLBAR
      EXPORTING
        IO_ALV_GRID = OBJ_ALV.

    GS_LAYOUT-ZEBRA      = 'X'.
    GS_LAYOUT-STYLEFNAME = 'STYLE'.
    GS_LAYOUT-NO_TOOLBAR = ABAP_TRUE.



    GS_VARIANT-REPORT  = SY-REPID.

    SET HANDLER: OBJ_TOOLBAR->ON_TOOLBAR          FOR OBJ_ALV,
                 OBJ_TOOLBAR->HANDLE_USER_COMMAND FOR OBJ_ALV,
                 OBJ_TOOLBAR->HANDLE_DATA_CHANGED FOR OBJ_ALV,
                 LCL_EVENT_HANDLER=>HANDLE_BUTTON_CLICK   FOR OBJ_ALV,
                 OBJ_TOOLBAR->ON_HOTSPOT_CLICK FOR OBJ_ALV.

    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

    CALL METHOD OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        IS_VARIANT           = GS_VARIANT
      CHANGING
        IT_FIELDCATALOG      = IT_FCAT
        IT_OUTTAB            = IT_SAIDA[].

    CALL METHOD OBJ_ALV->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.

    CALL METHOD OBJ_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.                 " CRIAR_OBJETOS  OUTPUT

FORM REFRESH_OBJETOS_ALV .
  CLEAR: GS_LAYOUT,
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.

ENDFORM.                    " REFRESH_OBJETOS


*&---------------------------------------------------------------------*
*& Module STATUS_6001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_6001 OUTPUT.
  SET PF-STATUS 'PF6001'.
  SET TITLEBAR 'T6001'.

  PERFORM MONTA_POP_6001.
  PERFORM MOSTRA_POP_6001.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_6001 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

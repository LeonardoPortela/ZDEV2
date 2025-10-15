*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O02
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECTS OUTPUT.

  DATA: LS_LAYOUT_REF TYPE LVC_S_LAYO.

* Create container and ALV objects only once
  IF GF_FIRST_DISPLAY = 'X'.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CTL_CCCONTAINER  = SPLITTER->GET_CONTAINER( EXPORTING ROW = 1 COLUMN = 1 ).
    CTL_CCCONTAINER2 = SPLITTER->GET_CONTAINER( EXPORTING ROW = 2 COLUMN = 1 ).

    CREATE OBJECT SPLITTER_BUTTON
      EXPORTING
        PARENT  = CTL_CCCONTAINER2
        ROWS    = 1
        COLUMNS = COND I( WHEN SY-TCODE EQ 'ZMDFE' THEN 2 ELSE 1 ).

    CTL_CCCONTAINER_HIST = SPLITTER_BUTTON->GET_CONTAINER( EXPORTING ROW = 1 COLUMN = 1 ).

    IF SY-TCODE EQ 'ZMDFE'.
      CTL_CCCONTAINER_REF  = SPLITTER_BUTTON->GET_CONTAINER( EXPORTING ROW = 1 COLUMN = 2 ).
      SPLITTER_BUTTON->SET_COLUMN_WIDTH( EXPORTING ID = 2 WIDTH = 30 ).

      CREATE OBJECT GC_ALV_MDFE
        EXPORTING
          I_PARENT = CTL_CCCONTAINER_REF.

      PERFORM: CREATE_CATALOG_MDFE.

      LS_LAYOUT_REF-GRID_TITLE = TEXT-205.

      CALL METHOD GC_ALV_MDFE->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT            = LS_LAYOUT_REF
          I_SAVE               = 'A'
          IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        CHANGING
          IT_FIELDCATALOG      = GT_CATALOG_MDFE
          IT_OUTTAB            = GT_MDFE.
    ENDIF.

    SPLITTER->SET_ROW_HEIGHT( EXPORTING ID = 2 HEIGHT = 30 ).

*   Create object for ALV grid inside container
    CREATE OBJECT CTL_ALV_NFE
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.
*   Fill field catalog

    PERFORM FILL_IT_FIELDCATALOG.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT.
*   Set layout parameters for ALV grid

    CASE SY-TCODE.
      WHEN 'ZNFE' OR 'ZNFE_TERC'.
        GS_LAYOUT-GRID_TITLE = TEXT-100.
      WHEN 'ZCTE' OR 'ZCTE_TERC'.
        GS_LAYOUT-GRID_TITLE = TEXT-198.
      WHEN 'ZMDFE'.
        GS_LAYOUT-GRID_TITLE = TEXT-201.
    ENDCASE.

    GS_LAYOUT-SEL_MODE = 'A'.
*   Send data to ALV grid
    CTL_ALV_NFE->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_NFE_ALV ).

*   Create object for ALV grid inside container
    CREATE OBJECT CTL_ALV_NFE_HIST
      EXPORTING
        I_PARENT = CTL_CCCONTAINER_HIST.

*   Fill field catalog 2
    PERFORM FILL_IT_FIELDCATALOG2.
*   Set layout parameters for ALV grid2

    CASE SY-TCODE.
      WHEN 'ZNFE' OR 'ZNFE_TERC'.
        GS_LAYOUT2-GRID_TITLE = TEXT-110.
      WHEN 'ZCTE' OR 'ZCTE_TERC'.
        GS_LAYOUT2-GRID_TITLE = TEXT-199.
      WHEN 'ZMDFE'.
        GS_LAYOUT2-GRID_TITLE = TEXT-202.
    ENDCASE.

*   Send data to ALV grid
    CALL METHOD CTL_ALV_NFE_HIST->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT2
        IS_VARIANT           = GS_VARIANT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG2
        IT_OUTTAB            = IT_NFE_ALV2.

*   Create Object for Event Handler
    CREATE OBJECT EVENT_HANDLER.
    SET HANDLER EVENT_HANDLER->HANDLE_HOTSPOT_CLICK
            FOR CTL_ALV_NFE.

    CLEAR GF_FIRST_DISPLAY.
  ENDIF.

ENDMODULE.                 " create_objects  OUTPUT

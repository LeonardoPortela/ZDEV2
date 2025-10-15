*----------------------------------------------------------------------*
***INCLUDE ZPPR017_STATUS_0100O01.
*----------------------------------------------------------------------*

DATA: DG_SPLITTER     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV         TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG TYPE LVC_T_FCAT,
      GS_VARIANT      TYPE DISVARIANT,
      GS_LAYOUT       TYPE LVC_S_LAYO.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF DG_SPLITTER IS INITIAL.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 1
        COLUMNS = 1.

    CTL_CCCONTAINER = DG_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    PERFORM FILL_IT_FIELDCATALOG.
    PERFORM FILL_GS_VARIANT.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        IS_VARIANT      = GS_VARIANT
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_HVIS[].

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  IF CTL_ALV IS NOT INITIAL.
    CTL_ALV->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV.

  IF CTL_CCCONTAINER IS NOT INITIAL.
    CTL_CCCONTAINER->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER.

  IF DG_SPLITTER IS NOT INITIAL.
    DG_SPLITTER->FREE( ).
  ENDIF.
  CLEAR: DG_SPLITTER.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZPP_KUHLMANN_HVI'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.
    <FS_CAT>-TABNAME = 'ZPP_KUHLMANN_HVI'.
    <FS_CAT>-COL_OPT = ABAP_TRUE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.
  GS_LAYOUT-ZEBRA        = ABAP_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
***INCLUDE LZABAP_UTILF01.
*----------------------------------------------------------------------*
DATA: CON_0001           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_0001           TYPE REF TO CL_GUI_ALV_GRID,
      IT_CATALOG_0001    TYPE LVC_T_FCAT,
      LAY_0001           TYPE LVC_S_LAYO,
      VAR_0001           TYPE DISVARIANT,
      GS_SCROLL_COL_0001 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0001 TYPE LVC_S_ROID.

DATA: GB_I_STRUCTURE_NAME TYPE DD02L-TABNAME,
      GB_E_INDEX          TYPE LVC_INDEX,
      GB_TITLE            TYPE STRING.

CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK.

    DATA: LC_ROW TYPE LVC_T_ROW.

    IF E_ROW-ROWTYPE IS INITIAL.
      APPEND E_ROW TO LC_ROW.
      GB_E_INDEX = E_ROW-INDEX.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUTTAB  text
*      -->P_I_STRUCTURE_NAME  text
*      <--P_E_INDEX  text
*----------------------------------------------------------------------*
FORM SELECIONA_LINHA  USING    P_TITLE TYPE STRING
                               P_I_STRUCTURE_NAME TYPE DD02L-TABNAME
                      CHANGING P_E_INDEX TYPE LVC_INDEX.

  GB_I_STRUCTURE_NAME = P_I_STRUCTURE_NAME.
  GB_TITLE            = P_TITLE.
  CALL SCREEN 0001 STARTING AT 05 10.
  P_E_INDEX = GB_E_INDEX.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'T0001' WITH GB_TITLE.

  IF CON_0001 IS INITIAL.

    CREATE OBJECT CON_0001
      EXPORTING
        CONTAINER_NAME = 'ALV_0001'.

    CREATE OBJECT ALV_0001
      EXPORTING
        I_PARENT = CON_0001.

    VAR_0001-REPORT      = SY-REPID.
    VAR_0001-HANDLE      = '0001'.
    VAR_0001-LOG_GROUP   = ABAP_FALSE.
    VAR_0001-USERNAME    = ABAP_FALSE.
    VAR_0001-VARIANT     = ABAP_FALSE.
    VAR_0001-TEXT        = ABAP_FALSE.
    VAR_0001-DEPENDVARS  = ABAP_FALSE.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME = GB_I_STRUCTURE_NAME
      CHANGING
        CT_FIELDCAT      = IT_CATALOG_0001.

    LAY_0001-SEL_MODE   = 'A'.
    LAY_0001-ZEBRA      = ABAP_TRUE.

*    DATA: EP_TABLE  TYPE REF TO DATA.
*
*    CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
*      EXPORTING
*        IT_FIELDCATALOG  = IT_CATALOG_0001
*        I_LENGTH_IN_BYTE = 'X'
*      IMPORTING
*        EP_TABLE         = EP_TABLE.
*
*    ASSIGN EP_TABLE->* TO <FS_T>.
*
*    CREATE DATA DY_LINE LIKE LINE OF <FS_T>.
*
*    LOOP AT <FS_ANY> INTO DATA(WA_ANY).
*      APPEND WA_ANY TO .
*    ENDLOOP.

    ALV_0001->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT       = LAY_0001
        IS_VARIANT      = VAR_0001
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0001
        IT_OUTTAB       = <FS_ANY> ).

    CREATE OBJECT EVENT_HANDLER.
    SET HANDLER EVENT_HANDLER->HANDLE_DOUBLE_CLICK FOR ALV_0001.

  ENDIF.

  ALV_0001->REFRESH_TABLE_DISPLAY( ).

  ALV_0001->GET_SCROLL_INFO_VIA_ID(
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0001
      ES_ROW_NO   = GS_SCROLL_ROW_0001 ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

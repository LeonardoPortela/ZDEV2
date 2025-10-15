*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_0313.
*----------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0313A DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.

DATA: CTL_CCCONTAINER_0313A    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_0313A            TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG_0313A    TYPE LVC_T_FCAT,
      GS_VARIANT_0313A         TYPE DISVARIANT,
      GS_LAYOUT_0313A          TYPE LVC_S_LAYO,
      OBG_TOOLBAR_0313A        TYPE REF TO LCL_ALV_TOOLBAR_0313A,
      OBJ_TOOLBARMANAGER_0313A TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      GS_SCROLL_COL_0313A      TYPE LVC_S_COL,
      GS_SCROLL_ROW_0313A      TYPE LVC_S_ROID,
      WA_STABLE_0313A          TYPE LVC_S_STBL.

CLASS LCL_ALV_TOOLBAR_0313A IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT OBJ_TOOLBARMANAGER_0313A
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.

  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*    "Separador
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD OBJ_TOOLBARMANAGER_0313A->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: ET_INDEX_ROWS	TYPE LVC_T_ROW.

    CALL METHOD CTL_ALV_0313A->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = ET_INDEX_ROWS.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0313  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0313 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF CTL_ALV_0313A IS INITIAL.

    CREATE OBJECT CTL_CCCONTAINER_0313A
      EXPORTING
        CONTAINER_NAME = 'ALV_TAKE_VINC'.

    CREATE OBJECT CTL_ALV_0313A
      EXPORTING
        I_PARENT = CTL_CCCONTAINER_0313A.

    PERFORM FILL_IT_FIELDCATALOG_0313A.

    PERFORM FILL_GS_VARIANT_0313A.

    GS_LAYOUT_0313A-SEL_MODE   = 'A'.
    GS_LAYOUT_0313A-ZEBRA      = ABAP_FALSE.
    GS_LAYOUT_0313A-CWIDTH_OPT = ABAP_FALSE.
    GS_LAYOUT_0313A-NO_TOOLBAR = ABAP_TRUE.

    CREATE OBJECT OBG_TOOLBAR_0313A
      EXPORTING
        IO_ALV_GRID = CTL_ALV_0313A.

    SET HANDLER OBG_TOOLBAR_0313A->ON_TOOLBAR FOR CTL_ALV_0313A.
    SET HANDLER OBG_TOOLBAR_0313A->HANDLE_USER_COMMAND FOR CTL_ALV_0313A.

    CALL METHOD CTL_ALV_0313A->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_0313A
        IS_VARIANT      = GS_VARIANT_0313A
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_0313A
        IT_OUTTAB       = IT_TAKES_VINCU[].

  ENDIF.

  WA_STABLE_0313A-ROW = ABAP_TRUE.
  WA_STABLE_0313A-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_0313A->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE_0313A.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0312A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0313A.

  GS_VARIANT_0313A-REPORT      = SY-REPID.
  GS_VARIANT_0313A-HANDLE      = '0313'.
  GS_VARIANT_0313A-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_0313A-USERNAME    = ABAP_FALSE.
  GS_VARIANT_0313A-VARIANT     = ABAP_FALSE.
  GS_VARIANT_0313A-TEXT        = ABAP_FALSE.
  GS_VARIANT_0313A-DEPENDVARS  = ABAP_FALSE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0312A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0313A .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG_0313A[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZSDT0001TK_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_0313A.

  LOOP AT IT_FIELDCATALOG_0313A ASSIGNING <FS_CAT>.

    <FS_CAT>-TABNAME = 'ZDE_ZSDT0001TK_ALV'.

    CASE <FS_CAT>-FIELDNAME.
      WHEN 'DS_FORNECEDOR'.
        <FS_CAT>-OUTPUTLEN = 33.
      WHEN 'ID_MOD_FISCAL'.
        <FS_CAT>-JUST = 'C'.
        <FS_CAT>-OUTPUTLEN = 8.
      WHEN 'NR_NOTA'.
        <FS_CAT>-JUST = 'R'.
        <FS_CAT>-OUTPUTLEN = 12.
      WHEN 'NM_SERIE'.
        <FS_CAT>-JUST = 'R'.
        <FS_CAT>-OUTPUTLEN = 6.
      WHEN 'DT_EMISSAO'.
        <FS_CAT>-JUST = 'C'.
        <FS_CAT>-OUTPUTLEN = 13.
      WHEN 'ID_TAKEUP' OR 'NU_BLOCO'.
        <FS_CAT>-JUST = 'C'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'QT_VINCULADA'.
        <FS_CAT>-DO_SUM = ABAP_TRUE.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN OTHERS.
        <FS_CAT>-NO_OUT = ABAP_TRUE.
    ENDCASE.

  ENDLOOP.

ENDFORM.

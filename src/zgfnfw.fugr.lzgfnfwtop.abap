FUNCTION-POOL ZGFNFW                    . "MESSAGE-ID ...
INCLUDE <ICON>.
CONSTANTS:    C_X               TYPE C VALUE 'X',
              C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG'.

*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.
* Docking Container
DATA: OBG_DOCKING         TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBG_GRID1           TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR         TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      SCREEN               TYPE SY-DYNNR.
*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.

DATA: WG_MENSAGENS(30)  VALUE '@5C@ Messagens',
      TG_FIELDCATALOG TYPE LVC_T_FCAT,
      WG_FIELDCATALOG TYPE LVC_S_FCAT,
      WG_LAYOUT       TYPE LVC_S_LAYO,
      WG_STABLE       TYPE LVC_S_STBL,
      TG_MSGS         TYPE TABLE OF ZFIWRS0002,
      WG_MSGS         TYPE ZFIWRS0002,
      E_ABA(50),
      E_FIELD(30).

DATA: tl_bdc    TYPE TABLE OF bdcdata,
      wl_bdc    TYPE bdcdata.

FIELD-SYMBOLS: <FS_ABA> TYPE ANY,
               <FS_FIELD> TYPE ANY.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
                IMPORTING
                 IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
    ON_TOOLBAR
       FOR EVENT TOOLBAR
       OF  CL_GUI_ALV_GRID
       IMPORTING
         E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING
                         E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
                      IMPORTING E_ROW E_COLUMN.

*    CLASS-METHODS:
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                                   IMPORTING  E_ROW_ID E_COLUMN_ID.
*
*    CLASS-METHODS:
*     ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
* IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
*
*    CLASS-METHODS:
* ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV
*_GRID
*                      IMPORTING E_MODIFIED ET_GOOD_CELLS.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
*   Add customized toolbar buttons.
*   variable for Toolbar Button
    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
    TY_TOOLBAR-BUTN_TYPE = 0.
*    ty_toolbar-text = 'Button1'.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*    ty_toolbar-icon      =  icon_voice_output.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button2'.
*
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*
*    ty_toolbar-icon      =  icon_phone.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button3'.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*
*    ty_toolbar-icon      =  icon_mail.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button4'.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*
*    ty_toolbar-icon      =  icon_voice_input.
*    ty_toolbar-butn_type = 0.
*    ty_toolbar-text = 'Button5'.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
*    break abap.
    CASE E_UCOMM.
      WHEN C_CLOS_MSG.
        CALL METHOD OBG_DOCKING->SET_VISIBLE
          EXPORTING
            VISIBLE = SPACE.

        LEAVE TO SCREEN SCREEN.
    ENDCASE.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.
    IF E_ROW GT 0.
      READ TABLE TG_MSGS INTO WG_MSGS INDEX E_ROW.
      IF SY-SUBRC IS INITIAL.
        IF <FS_ABA> IS ASSIGNED.
          MOVE: WG_MSGS-ABA   TO <FS_ABA>.
        ENDIF.

        IF <FS_FIELD> IS ASSIGNED.
          MOVE: WG_MSGS-FIELD TO <FS_FIELD>.
        ENDIF.

        LEAVE TO SCREEN SCREEN.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

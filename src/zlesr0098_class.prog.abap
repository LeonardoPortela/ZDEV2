*&---------------------------------------------------------------------*
*&  Include           ZLESR0098_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0100 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

*    TY_TOOLBAR-ICON      = ICON_REFRESH.
*    TY_TOOLBAR-FUNCTION  = 'REFRESH'.
*    TY_TOOLBAR-TEXT      = 'Atualizar'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN ''.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

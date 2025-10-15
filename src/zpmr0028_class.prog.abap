*&---------------------------------------------------------------------*
*&  Include           ZPMR0028_CLASS
*&---------------------------------------------------------------------*

** Classes
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

** Classes
CLASS LCL_EVENT_HANDLER_NOTA DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

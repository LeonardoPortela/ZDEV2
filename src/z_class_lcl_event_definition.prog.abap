*&-----------------------------------------------------------------------------*
*& CLASS LCL_EVENT_DEFINITION                                                  *
*& AUTOR: ENIO JESUS                                                           *
*& 13.07.2015                                                                  *
*&-----------------------------------------------------------------------------*

  PUBLIC SECTION.

*&-----------------------------------------------------------------------------*
*& METHOD CBX_DATA_CHANGED                                                     *
*&-----------------------------------------------------------------------------*
    CLASS-METHODS:
      CBX_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                       IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

*&-----------------------------------------------------------------------------*
*& METHOD SET_TOOLBAR                                                          *
*&-----------------------------------------------------------------------------*
    CLASS-METHODS:
      SET_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                        IMPORTING E_OBJECT.

*&-----------------------------------------------------------------------------*
*& METHOD GET_UCOMM                                                            *
*&-----------------------------------------------------------------------------*
    CLASS-METHODS:
      GET_UCOMM FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                        IMPORTING E_UCOMM.

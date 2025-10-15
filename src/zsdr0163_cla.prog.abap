*&---------------------------------------------------------------------*
*& Include          ZSDR0163_CLA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------*
* CLASS DEFINITION                                                     *
*----------------------------------------------------------------------*
* Eventhandler
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.

*    CLASS-METHODS : on_toolbar_click FOR EVENT added_function OF cl_salv_events_table
*      IMPORTING
*        e_salv_function
*        sender.

ENDCLASS.


*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION                                                 *
*----------------------------------------------------------------------*
*CLASS lcl_events IMPLEMENTATION.
*  METHOD on_toolbar_click.
*
*    DATA:
*      lr_selections   TYPE REF TO cl_salv_selections.
*
*    DATA:
*      lt_rows         TYPE        salv_t_row.
*
*
*    lr_selections = gr_alv->get_selections( ).
*    lt_rows = lr_selections->get_selected_rows( ).
*
*
*    CASE e_salv_function.
*
**      WHEN GC_BTN_CLOSE.
*
**        PERFORM UPDATE_DATA        USING lt_rows
**                                CHANGING GT_DATA.
*
*      WHEN OTHERS.
*
*    ENDCASE.

*
*    gr_alv->refresh( ).
*
*  ENDMETHOD.
*ENDCLASS.

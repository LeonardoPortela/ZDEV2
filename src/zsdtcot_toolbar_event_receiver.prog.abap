*----------------------------------------------------------------------*
*   INCLUDE BCALV_TOOLBAR_EVENT_RECEIVER                               *
*----------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
ENDCLASS.

CLASS lcl_toolbar_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS: on_function_selected
                FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode,

      on_toolbar_dropdown
                  FOR EVENT dropdown_clicked OF cl_gui_toolbar
        IMPORTING fcode
                  posx
                  posy.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_toolbar_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_toolbar_event_receiver IMPLEMENTATION.

  METHOD on_function_selected.
    DATA: ls_sflight TYPE sflight.
    CASE fcode.
      WHEN 'DELETE'.
      WHEN 'INSERT_LC'.
      WHEN 'INSERT_FC'.
      WHEN 'INSERT_FS'.
      WHEN 'INSERT_LS'.
      WHEN 'INSERT_NS'.
    ENDCASE.
*   update frontend

    CALL METHOD tree1->frontend_update.
  ENDMETHOD.

  METHOD on_toolbar_dropdown.
* create contextmenu
    DATA: l_menu       TYPE REF TO cl_ctmenu,
          l_fc_handled TYPE as4flag.

    CREATE OBJECT l_menu.
    CLEAR l_fc_handled.

    CASE fcode.
      WHEN 'INSERT_LC'.
        l_fc_handled = 'X'.
*       insert as last child
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_LC'
            text  = 'Insert New Line as Last Child'.        "#EC NOTEXT
*       insert as first child
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_FC'
            text  = 'Insert New Line as First Child'.       "#EC NOTEXT
*       insert as next sibling
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_NS'
            text  = 'Insert New Line as Next Sibling'.      "#EC NOTEXT
*       insert as last sibling
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_LS'
            text  = 'Insert New Line as Last Sibling'.      "#EC NOTEXT
*       insert as first sibling
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_FS'
            text  = 'Insert New Line as First Sibling'.     "#EC NOTEXT
    ENDCASE.

* show dropdownbox
    IF l_fc_handled = 'X'.
      CALL METHOD mr_toolbar->track_context_menu
        EXPORTING
          context_menu = l_menu
          posx         = posx
          posy         = posy.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed4.
  ENDMETHOD.

ENDCLASS.

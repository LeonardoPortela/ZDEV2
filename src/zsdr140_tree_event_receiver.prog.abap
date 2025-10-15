*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_RECEIVER                                  *
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS: handle_hotspot_click
                FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.

    METHODS handle_node_ctmenu_request
                FOR EVENT node_context_menu_request OF cl_gui_alv_tree
      IMPORTING node_key
                menu.
    METHODS handle_node_ctmenu_selected
                FOR EVENT node_context_menu_selected OF cl_gui_alv_tree
      IMPORTING node_key
                fcode.
    METHODS handle_item_ctmenu_request
                FOR EVENT item_context_menu_request OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname
                menu.
    METHODS handle_item_ctmenu_selected
                FOR EVENT item_context_menu_selected OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname
                fcode.

    METHODS handle_item_double_click
                FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_button_click
                FOR EVENT button_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_link_click
                FOR EVENT link_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_header_click
                FOR EVENT header_click OF cl_gui_alv_tree
      IMPORTING fieldname.

    METHODS: on_add_hierarchy_node
                FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree_simple
      IMPORTING grouplevel
                index_outtab.

ENDCLASS.

CLASS lcl_tree_event_receiver IMPLEMENTATION.

  METHOD handle_node_ctmenu_request.
*   append own functions
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'USER1'
        text  = 'Usercmd 1'.                                "#EC NOTEXT
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'USER2'
        text  = 'Usercmd 2'.                                "#EC NOTEXT
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'USER3'
        text  = 'Usercmd 3'.                                "#EC NOTEXT
  ENDMETHOD.

  METHOD handle_node_ctmenu_selected.
    CASE fcode.
      WHEN 'USER1' OR 'USER2' OR 'USER3'.
        MESSAGE i000(0h) WITH 'Node-Context-Menu on Node ' node_key
                              'fcode : ' fcode.             "#EC NOTEXT
    ENDCASE.
  ENDMETHOD.

  METHOD handle_item_ctmenu_request .
*   append own functions
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'USER1'                                     "#EC NOTEXT
        text  = 'Usercmd 1'.                                "#EC NOTEXT
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'USER2'                                     "#EC NOTEXT
        text  = 'Usercmd 2'.                                "#EC NOTEXT
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'USER3'                                     "#EC NOTEXT
        text  = 'Usercmd 3'.                                "#EC NOTEXT
  ENDMETHOD.

  METHOD handle_item_ctmenu_selected.
    CASE fcode.
      WHEN 'USER1' OR 'USER2' OR 'USER3'.
        MESSAGE i000(0h) WITH 'Item-Context-Menu on Node ' node_key
                              'Fieldname : ' fieldname.     "#EC NOTEXT
    ENDCASE.
  ENDMETHOD.

  METHOD handle_item_double_click.
  ENDMETHOD.

  METHOD handle_button_click.
  ENDMETHOD.

  METHOD on_add_hierarchy_node.
  ENDMETHOD.

  METHOD handle_link_click.
  ENDMETHOD.

  METHOD handle_header_click.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*   data lv_row TYPE LVC_S_ROW.
*   data lv_column type LVC_S_COL.

*      lv_row = e_row_id .
*      lv_column = e_column_id.
  ENDMETHOD. "handle_hotspot_click

ENDCLASS.

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
*       get selected node
        DATA: lt_selected_node TYPE lvc_t_nkey.
        CALL METHOD tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_node.
        CALL METHOD cl_gui_cfw=>flush.
        DATA l_selected_node TYPE lvc_nkey.
        READ TABLE lt_selected_node INTO l_selected_node INDEX 1.

*       delete subtree
        IF NOT l_selected_node IS INITIAL.
          CALL METHOD tree1->delete_subtree
            EXPORTING
              i_node_key                = l_selected_node
              i_update_parents_expander = ''
              i_update_parents_folder   = 'X'.
        ELSE.
          MESSAGE i227(0h).
        ENDIF.
      WHEN 'INSERT_LC'.
*       get selected node
        CALL METHOD tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_node.
        CALL METHOD cl_gui_cfw=>flush.
        READ TABLE lt_selected_node INTO l_selected_node INDEX 1.
*       get current Line
        IF NOT l_selected_node IS INITIAL.
          CALL METHOD tree1->get_outtab_line
            EXPORTING
              i_node_key    = l_selected_node
            IMPORTING
              e_outtab_line = ls_sflight.
          ls_sflight-seatsmax = ls_sflight-price + 99.
          ls_sflight-price = ls_sflight-seatsmax + '99.99'.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_selected_node
              i_relationship   = cl_tree_control_base=>relat_last_child
              is_outtab_line   = ls_sflight
*             is_node_layout
*             it_item_layout
              i_node_text      = 'Last Child'.              "#EC NOTEXT
*           importing
*             e_new_node_key
        ELSE.
          MESSAGE i227(0h).
        ENDIF.
      WHEN 'INSERT_FC'.
*       get selected node
        CALL METHOD tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_node.
        CALL METHOD cl_gui_cfw=>flush.
        READ TABLE lt_selected_node INTO l_selected_node INDEX 1.
*       get current Line
        IF NOT l_selected_node IS INITIAL.
          CALL METHOD tree1->get_outtab_line
            EXPORTING
              i_node_key    = l_selected_node
            IMPORTING
              e_outtab_line = ls_sflight.
          ls_sflight-seatsmax = ls_sflight-price + 99.
          ls_sflight-price = ls_sflight-seatsmax + '99.99'.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_selected_node
              i_relationship   = cl_tree_control_base=>relat_first_child
              is_outtab_line   = ls_sflight
*             is_node_layout
*             it_item_layout
              i_node_text      = 'First Child'.             "#EC NOTEXT
*           importing
*             e_new_node_key
        ELSE.
          MESSAGE i227(0h).
        ENDIF.
      WHEN 'INSERT_FS'.
*       get selected node
        CALL METHOD tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_node.
        CALL METHOD cl_gui_cfw=>flush.
        READ TABLE lt_selected_node INTO l_selected_node INDEX 1.
*       get current Line
        IF NOT l_selected_node IS INITIAL.
          CALL METHOD tree1->get_outtab_line
            EXPORTING
              i_node_key    = l_selected_node
            IMPORTING
              e_outtab_line = ls_sflight.
          ls_sflight-seatsmax = ls_sflight-price + 99.
          ls_sflight-price = ls_sflight-seatsmax + '99.99'.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_selected_node
              i_relationship   =
                                 cl_tree_control_base=>relat_first_sibling
              is_outtab_line   = ls_sflight
*             is_node_layout
*             it_item_layout
              i_node_text      = 'First Sibling'.           "#EC NOTEXT
*           importing
*             e_new_node_key
        ELSE.
          MESSAGE i227(0h).
        ENDIF.
      WHEN 'INSERT_LS'.
*       get selected node
        CALL METHOD tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_node.
        CALL METHOD cl_gui_cfw=>flush.
        READ TABLE lt_selected_node INTO l_selected_node INDEX 1.
*       get current Line
        IF NOT l_selected_node IS INITIAL.
          CALL METHOD tree1->get_outtab_line
            EXPORTING
              i_node_key    = l_selected_node
            IMPORTING
              e_outtab_line = ls_sflight.
          ls_sflight-seatsmax = ls_sflight-price + 99.
          ls_sflight-price = ls_sflight-seatsmax + '99.99'.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_selected_node
              i_relationship   =
                                 cl_tree_control_base=>relat_last_sibling
              is_outtab_line   = ls_sflight
*             is_node_layout
*             it_item_layout
              i_node_text      = 'Last Sibling'.            "#EC NOTEXT
*           importing
*             e_new_node_key
        ELSE.
          MESSAGE i227(0h).
        ENDIF.
      WHEN 'INSERT_NS'.
*       get selected node
        CALL METHOD tree1->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_node.
        CALL METHOD cl_gui_cfw=>flush.
        READ TABLE lt_selected_node INTO l_selected_node INDEX 1.
*       get current Line
        IF NOT l_selected_node IS INITIAL.
          CALL METHOD tree1->get_outtab_line
            EXPORTING
              i_node_key    = l_selected_node
            IMPORTING
              e_outtab_line = ls_sflight.
          ls_sflight-seatsmax = ls_sflight-price + 99.
          ls_sflight-price = ls_sflight-seatsmax + '99.99'.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_selected_node
              i_relationship   =
                                 cl_tree_control_base=>relat_next_sibling
              is_outtab_line   = ls_sflight
*             is_node_layout
*             it_item_layout
              i_node_text      = 'Next Sibling'.            "#EC NOTEXT
*           importing
*             e_new_node_key
        ELSE.
          MESSAGE i227(0h).
        ENDIF.

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

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          l_erro   TYPE c.

    PERFORM f_ajusta_celltab.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      CASE ls_good-fieldname.

        WHEN 'ANLN1' OR 'ANLN2'.
          CLEAR w_sobra.
          READ TABLE t_sobra INTO w_sobra INDEX ls_good-row_id.
*         IF sy-subrc = 0.
*           DELETE t_sobra INDEX ls_good-row_id.
*         ENDIF.

*         READ TABLE t_sobra INTO w_sobra_aux WITH KEY anln1 = lv_value
*                                                      anln2 = w_sobra-anln2.
*         IF sy-subrc = 0.
*           CLEAR w_sobra.
*           MESSAGE i024(sd) WITH text-011.
*         ENDIF.

          FREE t_dados.

          IF ls_good-fieldname = 'ANLN1'.
            w_dados-bukrs   = l_bukrs.
            w_dados-anln1   = lv_value.
            w_dados-anln2   = w_sobra-anln2.
            APPEND w_dados TO t_dados.
          ELSE.
            w_dados-bukrs   = l_bukrs.
            w_dados-anln1   = w_sobra-anln1.
            w_dados-anln2   = lv_value.
            APPEND w_dados TO t_dados.
          ENDIF.
          PERFORM f_selecao_dados.
          PERFORM f_monta_dados USING ls_good-row_id.

        WHEN 'KOSTL' OR 'CC_SOBRA'.
          SELECT kostl
            FROM csks
              UP TO 1 ROWS
            INTO @DATA(l_kostl)
           WHERE kostl  = @lv_value
             AND datbi >= @sy-datum.
          ENDSELECT.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'E'.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    l_stable-row = 'X'.
    l_stable-col = 'X'.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.

ENDCLASS.

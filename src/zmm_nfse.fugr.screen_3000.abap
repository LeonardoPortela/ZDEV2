PROCESS BEFORE OUTPUT.

  CALL SUBSCREEN subscr_905x INCLUDING sy-repid gv_subscr_905x.

  MODULE tc_popup_list_assign_attr.

  LOOP AT gt_scr_popup_search INTO gw_scr_popup_search
                            WITH CONTROL tc_popup_search
                            CURSOR tc_popup_search-current_line.

  ENDLOOP.

  LOOP AT gt_scr_popup_list INTO gw_scr_popup_list
                            WITH CONTROL tc_popup_list
                            CURSOR tc_popup_list-current_line.
    MODULE tc_popup_list_get_lines.
    MODULE tc_popup_list_set_input_field.
    MODULE fill_totals.
  ENDLOOP.

  MODULE status_3000.

PROCESS AFTER INPUT.

  CALL SUBSCREEN subscr_905x.

*  MODULE get_cursor_3000.

  LOOP AT gt_scr_popup_search.

    CHAIN.
      FIELD gw_scr_popup_search-checkbox
                       MODULE checkbox_search ON CHAIN-REQUEST.

    ENDCHAIN.

  ENDLOOP.

  LOOP AT gt_scr_popup_list.

    CHAIN.
      FIELD: gw_scr_popup_list-checkbox,
             gw_scr_popup_list-dmbtr_iv,
             gw_scr_popup_list-menge_iv
                       MODULE checkbox ON CHAIN-REQUEST.
    ENDCHAIN.


  ENDLOOP.

  MODULE user_command_3000.

*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F16
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  grid_scroll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid_scroll using p_mode type c.

  data lv_row_number type i.

  gf_scroll = c_x.

  case p_mode.
    when c_100.
      case ok_code.
    when 'FIRST_PAGE'.
      gs_scroll_row-row_id = c_1.
    when 'NEXT_PAGE'.
      lv_row_number = gs_scroll_row-row_id + c_18.
      if gf_col_num_total >= lv_row_number.
        gs_scroll_row-row_id = gs_scroll_row-row_id + c_18.
      else.
        gs_scroll_row-row_id = gf_col_num_total - c_18 + c_1.
      endif.
    when 'PREV_PAGE'.
      if gs_scroll_row-row_id >= c_18.
        gs_scroll_row-row_id = gs_scroll_row-row_id - c_18.
      else.
        gs_scroll_row-row_id = 1.
      endif.
    when 'LAST_PAGE'.
      if gf_col_num_total > c_18.
        gs_scroll_row-row_id = gf_col_num_total - c_18 + c_1 + c_1.
      else.
        gs_scroll_row-row_id = c_1.
      endif.
      endcase.
    when c_102.
      case ok_code.
        when 'FIRST_PAGE'.
          gs_scroll_row_102-row_id = c_1.
        when 'NEXT_PAGE'.
          lv_row_number = gs_scroll_row_102-row_id + c_15.
          if dynp_0102_no_nfe >= lv_row_number.
            gs_scroll_row_102-row_id = gs_scroll_row_102-row_id + c_15.
          else.
            gs_scroll_row_102-row_id = dynp_0102_no_nfe - c_15 + c_1.
          endif.
        when 'PREV_PAGE'.
          if gs_scroll_row_102-row_id >= c_15.
            gs_scroll_row_102-row_id = gs_scroll_row_102-row_id - c_15.
          else.
            gs_scroll_row_102-row_id = 1.
          endif.
        when 'LAST_PAGE'.
          if dynp_0102_no_nfe > c_15.
            gs_scroll_row_102-row_id =
               dynp_0102_no_nfe - c_15 + c_1 + c_1.
          else.
            gs_scroll_row_102-row_id = c_1.
          endif.
      endcase.
  endcase.

endform.                    " grid_scroll

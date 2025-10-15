*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I06
*&---------------------------------------------------------------------*
module user_command_0102 input.

  case ok_code.
    when 'GO'.
*   Check assignment and if ok continue with canellation process
      perform check_reasons.
    when 'SELECT'.
*   Select/de-select present row
      perform row_selection_0102.
    when 'DEL_NFE'.
*   Delete selected documents from list
      perform delete_from_list.
    when 'NF_WRITER'.
*   Display selected NF-e in NF writer
      perform display_nfe using c_102.
    when 'SET_SEL'.
*   Assign cancellation reason to selected documents
      perform set_reason.
    when 'FIRST_PAGE'.
      perform grid_scroll using c_102.
    when 'NEXT_PAGE'.
      perform grid_scroll using c_102.
    when 'PREV_PAGE'.
      perform grid_scroll using c_102.
    when 'LAST_PAGE'.
      perform grid_scroll using c_102.
  endcase.

  clear ok_code.

endmodule.                 " USER_COMMAND_0102  INPUT

*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I09
*&---------------------------------------------------------------------*
module get_selected_rows_0102 input.

  clear it_selected_rows_0102.
  call method ctl_cancel_alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows_0102.

endmodule.                 " GET_SELECTED_ROWS_0102  INPUT

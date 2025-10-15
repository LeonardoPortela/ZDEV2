*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I10
*&---------------------------------------------------------------------*
module get_scroll_info_0102 input.

  call method ctl_cancel_alv->get_scroll_info_via_id
    importing
      es_col_info = gs_scroll_col_102
      es_row_no   = gs_scroll_row_102.

endmodule.                 " GET_SCROLL_INFO_0102  INPUT

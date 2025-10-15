*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I03
*&---------------------------------------------------------------------*
module get_scroll_info input.

  call method ctl_alv_nfe->get_scroll_info_via_id
    importing
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

endmodule.                 " get_scroll_info  INPUT

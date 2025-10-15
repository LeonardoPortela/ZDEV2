*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O09
*&---------------------------------------------------------------------*
module set_scroll_info_0102 output.

* Set ALV output area
  perform set_scroll_info_via_id using c_102.

endmodule.                 " SET_SCROLL_INFO_0102  OUTPUT

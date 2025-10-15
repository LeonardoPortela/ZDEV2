*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O04
*&---------------------------------------------------------------------*
module set_scroll_info output.

* Set ALV output area
  perform set_scroll_info_via_id using c_100.

endmodule.                 " set_scroll_info  OUTPUT

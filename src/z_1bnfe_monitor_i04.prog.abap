*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I04
*&---------------------------------------------------------------------*
module get_selected_rows input.

  clear it_selected_rows.
  call method ctl_alv_nfe->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  clear it_alv_selection.
  loop at it_selected_rows into wa_selected_rows.
    read table it_nfe_alv
          into wa_nfe_alv
         index wa_selected_rows-index.
    move-corresponding wa_nfe_alv to wa_alv_selection.
    append wa_alv_selection to it_alv_selection.
  endloop.

endmodule.                 " get_selected_rows  INPUT

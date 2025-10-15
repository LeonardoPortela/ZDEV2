*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O08
*&---------------------------------------------------------------------*
module fill_alv_0102 output.

* Fill ALV table it_cancel_alv for subsequent calls of screen 0102
  if gf_first_display_0102 = 'R'.

    clear it_cancel_alv.
    clear wa_cancel_alv.
    clear dynp_0102_no_nfe.
    loop at it_alv_selection into wa_alv_selection.
      move-corresponding wa_alv_selection to wa_cancel_alv.
      append wa_cancel_alv to it_cancel_alv.
      dynp_0102_no_nfe = dynp_0102_no_nfe + 1.
    endloop.
*   Update ALV display
    perform grid_update_0102.

    clear gf_first_display_0102.
  endif.

endmodule.                 " FILL_ALV_0102  OUTPUT

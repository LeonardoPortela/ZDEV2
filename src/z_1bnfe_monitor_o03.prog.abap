*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O03
*&---------------------------------------------------------------------*
module log_icon_init output.

  if gf_first_display = 'X'.

    if gf_scroll = c_x.
      return.
    endif.

    perform read_error_log using 'ALL'.

*   Set error-log icon for initial ALV diplay
    loop at it_nfe_alv into wa_nfe_alv.
      index = sy-tabix.
      read table it_log_header into wa_log_header
         with key extnumber = wa_nfe_alv-docnum.
      if sy-subrc = 0.
        wa_nfe_alv-errlog = icon_defect.
      else.
        wa_nfe_alv-errlog = icon_led_green.
      endif.

      modify it_nfe_alv from wa_nfe_alv index index.
    endloop.

  endif.

endmodule.                 " log_icon_init  OUTPUT

*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F11
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  grid_refresh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid_refresh.

* Get records with actual status from the data base
  perform nfe_active_read.
* Get actual error log statuses
  perform read_error_log using 'ALL'.
* Set error-log icon for initial ALV display
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

* Update ALV display
  call method ctl_alv_nfe->refresh_table_display.
* Update ALV2 display                                   "1090279
  if not gf_docnum is initial.                          "1090279
    perform fill_nfe_history using gf_docnum.           "1090279
  endif.                                                "1090279

endform.                    " grid_refresh

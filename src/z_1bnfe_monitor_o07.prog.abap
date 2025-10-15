*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O07
*&---------------------------------------------------------------------*
module create_objects_0102 output.


* Create container and ALV objects only once
  if gf_first_display_0102 = 'X'.

*   Fill ALV table it_cancel_alv for first call of screen 0102
    clear it_cancel_alv.
    clear wa_cancel_alv.
    clear dynp_0102_no_nfe.
    loop at it_alv_selection into wa_alv_selection.
      move-corresponding wa_alv_selection to wa_cancel_alv.
      append wa_cancel_alv to it_cancel_alv.
      dynp_0102_no_nfe = dynp_0102_no_nfe + 1.
    endloop.
*   Set status icon initially
    perform set_status_icon_0102.


*   Create object for container
    create object ctl_cccontainer_c
       exporting container_name = 'CANCEL_CONTAINER'.
*   Create object for ALV grid inside container
    create object ctl_cancel_alv
       exporting i_parent = ctl_cccontainer_c.
*   Fill field catalog
    perform fill_it_fieldcatalog_c.
*   Fill info for layout variant
    perform fill_gs_variant_0102.
*   Set layout parameters for ALV grid
    gs_layout_c-grid_title = text-191.
    gs_layout_c-sel_mode = 'A'.
*   Send data to ALV grid
    call method ctl_cancel_alv->set_table_for_first_display
      exporting
        is_layout            = gs_layout_c
        is_variant           = gs_variant_c
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      changing
        it_fieldcatalog      = it_fieldcatalog_c
        it_outtab            = it_cancel_alv.

*   Create Object for Event Handler
    create object event_handler_0102.
    set handler event_handler_0102->handle_hotspot_click
            for ctl_cancel_alv.

    clear gf_first_display_0102.
  endif.

endmodule.                 " CREATE_OBJECTS_0102  OUTPUT

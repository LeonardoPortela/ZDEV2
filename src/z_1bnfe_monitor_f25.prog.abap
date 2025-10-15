*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F25
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_scs_to_8
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_scs_to_8.

  if it_selected_rows is initial.
    message id 'J1B_NFE' type 'E' number '030'.
    return.
  endif.

* Check authorization
  if gf_authorization_nfe_35 is initial.
    message id 'J1B_NFE' type 'E' number '056'.
  endif.

* Switch SCS to '8' for selected NF-e
  clear subrc.
  refresh it_active_mod.                                      "1090279
  loop at it_selected_rows into wa_selected_rows.

    read table it_nfe_alv into wa_nfe_alv index wa_selected_rows-index.
    call function 'J_1B_NFE_ACCEPT_REJECTION'
      exporting
        i_docnum       = wa_nfe_alv-docnum
      importing                                               "1090279
        es_active_mod  = wa_active_mod                        "1090279
      exceptions
        process_errors = 1
        others         = 2.

    if sy-subrc <> 0.
      subrc = c_x.
    else.                                                     "1090279
      append wa_active_mod to it_active_mod.                  "1090279
    endif.
  endloop.

* Update ALV display                                          "1090279
  perform grid_update using space.                            "1090279

  if not subrc is initial.
    message id 'J1B_NFE' type 'E' number '024'.
  else.
    message id 'J1B_NFE' type 'S' number '027'.
  endif.

endform.                    " set_scs_to_8

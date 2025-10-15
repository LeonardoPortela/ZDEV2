*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F24
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  contingency_reset
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form contingency_reset .

* Check authorization
  if gf_authorization_nfe_35 is initial.
    message id 'J1B_NFE' type 'E' number '056'.
  endif.

* Check if an NF-e selection was made
  if it_selected_rows is initial.
    message id 'J1B_NFE' type 'E' number '030'.
    return.
  endif.

* Undo contingency mode for selected NF-e documents
  clear subrc.
  refresh it_active_mod.                                      "1090279
  loop at it_selected_rows into wa_selected_rows.

    read table it_nfe_alv into wa_nfe_alv index wa_selected_rows-index.
    call function 'J_1B_NFE_CONTINGENCY_RESET'
      exporting
        iv_docnum         = wa_nfe_alv-docnum
      importing                                               "1090279
        es_active_mod     = wa_active_mod                     "1090279
      exceptions
        reset_not_allowed = 1
        reset_failure     = 2
        others            = 3.

    if sy-subrc <> 0.
      subrc = c_x.
      call function 'DEQUEUE_E_J1BNFE'
        exporting
          mode_j_1bnfe_active = 'E'
          mandt               = sy-mandt
          docnum              = wa_nfe_alv-docnum.
      call function 'DEQUEUE_E_J1B_INVALID'
        exporting
          mode_j_1bnfe_invalid = 'E'
          mandt                = sy-mandt
          docnum               = wa_nfe_alv-docnum.
    else.                                                     "1090279
      append wa_active_mod to it_active_mod.                  "1090279
    endif.
  endloop.

* Update ALV display                                          "1090279
  perform grid_update using 'RESET'.                          "1090279

  if not subrc is initial.
    message id 'J1B_NFE' type 'E' number '024'.
  else.
    message id 'J1B_NFE' type 'S' number '027'.
  endif.

endform.                    " contingency_reset

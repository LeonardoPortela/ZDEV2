*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F14
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  contingency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form contingency .

  if it_selected_rows is initial.
    message id 'J1B_NFE' type 'E' number '030'.
    return.
  endif.

* Check authorization
  if gf_authorization_nfe_35 is initial.
    message id 'J1B_NFE' type 'E' number '056'.
  endif.

* Contingency processing for selected NF-e
  clear subrc.
  refresh it_active_mod.                                      "1090279
  loop at it_selected_rows into wa_selected_rows.

    read table it_nfe_alv into wa_nfe_alv index wa_selected_rows-index.
*
* check if NF-e is already numbered - ohterwise process    "1265172
* is not allowed                                           "1265172
    perform check_numbered_nfe using wa_nfe_alv-docnum     "1265172
                                     wa_nfe_alv-nfnum9     "1265172
                                     c_1                   "1265172
                               changing subrc.             "1265172

    if subrc is initial.                                   "1265172

      call function 'J_1B_NFE_CONTINGENCY'
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

    endif.                                                 "1265172

  endloop.

* Update ALV display                                          "1090279
  perform grid_update using space.                            "1090279

  if not subrc is initial.
    message id 'J1B_NFE' type 'E' number '024'.
  else.
    message id 'J1B_NFE' type 'S' number '027'.
  endif.

endform.                    " contingency

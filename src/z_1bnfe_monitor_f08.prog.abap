*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F08
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  read_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0017   text
*----------------------------------------------------------------------*
form read_error_log using p_mode type any.

  data: lt_nfe_aux like table of wa_nfe_alv                 "1150172
         with key docnum.                                   "1150172
  data: idx1   type i,                                      "1150172
        idx2   type i,                                      "1150172
        lv_max type i value '500'.                          "1150172


  clear gs_log_filter.
  clear gf_log_entries_exist.
  clear it_log_header.                                      "1150172

* Prepare data for Application-Log query                    "1150172
  case p_mode.                                              "1150172
    when 'ALL'.                                             "1150172
                                                            "1150172
      while idx2 < gf_col_num_total.                        "1150172
        clear: gs_log_filter, it_log_header2, lt_nfe_aux.   "1150172
        if idx1 is initial.                                 "1150172
          idx1 = c_1.                                       "1150172
          idx2 = lv_max.                                    "1150172
          if idx2 > gf_col_num_total.                       "1150172
            idx2 = gf_col_num_total.                        "1150172
          endif.                                            "1150172
        else.                                               "1150172
          idx1 = idx1 + lv_max.                             "1150172
          idx2 = idx2 + lv_max.                             "1150172
          if idx2 > gf_col_num_total.                       "1150172
            idx2 = gf_col_num_total.                        "1150172
          endif.                                            "1150172
        endif.                                              "1150172
        append lines of it_nfe_alv                          "1150172
               from idx1 to idx2                            "1150172
               to lt_nfe_aux.                               "1150172

        loop at lt_nfe_aux into wa_nfe_alv.
        gs_object-sign   = 'I'.
        gs_object-option = 'EQ'.
        gs_object-low = gs_object-high = c_object.
*        APPEND gs_object TO gs_log_filter-object.           "1258974
        collect gs_object into gs_log_filter-object.         "1258974

        gs_subobject-sign   = 'I'.
        gs_subobject-option = 'EQ'.
        gs_subobject-low = gs_subobject-high = c_subobject.
*        APPEND gs_subobject TO gs_log_filter-subobject.     "1258974
        collect gs_subobject into gs_log_filter-subobject.   "1258974

        gs_extnum-sign   = 'I'.
        gs_extnum-option = 'EQ'.
        gs_extnum-low = gs_extnum-high = wa_nfe_alv-docnum.
        append gs_extnum to gs_log_filter-extnumber.
      endloop.

        clear it_log_header2.                               "1150172
      call function 'BAL_DB_SEARCH'
        exporting
          i_client           = sy-mandt
          i_s_log_filter     = gs_log_filter
        importing
            e_t_log_header     = it_log_header2
        exceptions
          log_not_found      = 1
          no_filter_criteria = 2
          others             = 3.
      if sy-subrc <> 0.
      endif.

        append lines of it_log_header2 to it_log_header.    "1150172

      endwhile.

      if not it_log_header is initial.
        gf_log_entries_exist = 'X'.
      endif.

  endcase.

endform.                    " read_error_log

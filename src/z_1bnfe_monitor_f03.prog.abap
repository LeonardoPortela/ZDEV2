*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  display_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_error_log .

  call function 'BAL_GLB_MEMORY_REFRESH'
    exporting
      i_refresh_all = 'X'.
  if sy-subrc <> 0.
  endif.
* Clear log_handle in FUGR J1BB                               1158356
  call function 'J_1B_NFE_PROCESSING' .                      "1158356

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
  if sy-subrc <>  0.
    message id 'J1B_NFE' type 'I' number '037'
            with wa_nfe_alv-docnum.
    exit.
  endif.

  clear it_log_handle.
  clear it_msg_handle.
  call function 'BAL_DB_LOAD'
    exporting
      i_t_log_header     = it_log_header2
      i_client           = sy-mandt
    importing
      e_t_log_handle     = it_log_handle
      e_t_msg_handle     = it_msg_handle
    exceptions
      no_logs_specified  = 1
      log_not_found      = 2
      log_already_loaded = 3
      others             = 4.
  if sy-subrc <> 0.                              "#EC *
  endif.

  call function 'BAL_DSP_LOG_DISPLAY'
    exporting
      i_t_log_handle       = it_log_handle
      i_t_msg_handle       = it_msg_handle
    exceptions
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      others               = 5.
  if sy-subrc <> 0.                              "#EC *
  endif.

endform.                    " display_error_log

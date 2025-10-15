*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F15
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  delete_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_error_log USING p_mesg.

  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
  ENDIF.

* Check authorization
  IF gf_authorization_nfe_35 IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '057'.
  ENDIF.

  CLEAR gs_log_filter.
  CLEAR it_log_header2.
  gs_subobject-sign   = gs_object-sign    = gs_extnum-sign   = 'I'.
  gs_subobject-option = gs_object-option  = gs_extnum-option = 'EQ'.
  gs_object-low       = gs_object-high    = c_object.
  gs_subobject-low    = gs_subobject-high = c_subobject.
* Create list for error-log query
  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
    APPEND gs_object    TO gs_log_filter-object.
    APPEND gs_subobject TO gs_log_filter-subobject.
    gs_extnum-low = gs_extnum-high = wa_nfe_alv-docnum.
    APPEND gs_extnum TO gs_log_filter-extnumber.
  ENDLOOP.

* Get header of application log for selected NF-e documents
  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_client           = sy-mandt
      i_s_log_filter     = gs_log_filter
    IMPORTING
      e_t_log_header     = it_log_header2
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2
      OTHERS             = 3.

  IF p_mesg = abap_true.
    IF sy-subrc <> 0.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '034'.
    ENDIF.
  ENDIF.

* Delete application-log entries for selected NF-e documents
  CALL FUNCTION 'BAL_DB_DELETE'
    EXPORTING
      i_t_logs_to_delete = it_log_header2
      i_client           = sy-mandt
      i_with_commit_work = 'X'
    EXCEPTIONS
      no_logs_specified  = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    " '1' should never happen;
  ELSE.
    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE it_nfe_alv INTO wa_nfe_alv
           INDEX wa_selected_rows-index.
      wa_nfe_alv-errlog = icon_led_green.
      MODIFY it_nfe_alv FROM wa_nfe_alv INDEX wa_selected_rows-index.
    ENDLOOP.
*   Check application-log entries and set error-log icons
    PERFORM read_error_log USING 'ALL'.
    PERFORM set_log_icons.
*   Update ALV display
    CALL METHOD ctl_alv_nfe->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = c_x.
    MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '033'.
  ENDIF.

ENDFORM.                    " delete_error_log

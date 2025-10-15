"Name: \PR:SAPLJ_1B_NFE_EVENT\IC:LJ_1B_NFE_EVENTF04\SE:END\EI
ENHANCEMENT 0 Z_1B_NFE_EVENTF04.

FORM z_process_status_events                                "2152098
  USING it_events     TYPE j_1bnfe_event_in_tab           "2152098
        iv_event_type TYPE j_1bnfe_int_event              "2152098
  CHANGING ct_bapiret2 TYPE bapirettab.

  DATA: ls_event_in       TYPE j_1bnfe_event_in,          "2152098
        ls_previous_event LIKE ls_event_in,
        ls_send_event TYPE j_1bnfe_event,                 "2152098
        ls_active TYPE j_1bnfe_active,
        lv_nfe_msgtyp TYPE j_1bnfe_message_type,
        lv_nfe_status_code TYPE j_1bstatuscode,
        lv_nfe_auth_date TYPE j_1bauthdate,
        lv_nfe_auth_code TYPE j_1bnfeauthcode,
        lv_nfe_auth_time TYPE j_1bauthtime,
        lx_err TYPE flag. "='X' error occured

* Set WAIT flag for first automatic cancellation request        "1779143
  CALL FUNCTION 'J_1BDFE_SET_WAIT'.                             "1779143

  LOOP AT it_events INTO ls_event_in.                     "2152098

    "Handle previous event in case of error
    IF lx_err = 'X'.
      PERFORM put_general_msg_in_monitor_log
        USING ls_previous_event.
      PERFORM dequeue_event
        USING ls_previous_event-docnum.
    ENDIF.
    ls_previous_event = ls_event_in.
    CLEAR lx_err.

    "set 'key' for BAPIRET2 entries for GRC
    CONCATENATE ls_event_in-docnum ls_event_in-ext_event ls_event_in-seqnum
         INTO gv_message_key SEPARATED BY space.

    PERFORM check_event_msgtyp
      USING ls_event_in
      CHANGING ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.

    PERFORM read_active_table
      USING ls_event_in
      CHANGING ls_active ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.
    ls_event_in-docnum = ls_active-docnum.

    PERFORM check_event_allowed_for_bupla
      USING iv_event_type ls_active                       "2152098
      CHANGING ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.

    PERFORM check_event_status_code
      USING ls_event_in
      CHANGING ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.

    PERFORM lock_event
      USING ls_event_in
      CHANGING ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.

    PERFORM check_open_send_event
      USING ls_event_in iv_event_type                     "2152098
      CHANGING ls_send_event ct_bapiret2 lx_err.          "2152098
    IF lx_err = 'X'. CONTINUE. ENDIF.

    PERFORM map_message_type
      USING ls_event_in ls_active
      CHANGING lv_nfe_msgtyp
               ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.

    PERFORM update_event_table
      USING ls_event_in ls_send_event                     "2152098
      CHANGING ct_bapiret2 lx_err.
    IF lx_err = 'X'. CONTINUE. ENDIF.

*---> 01/07/2023 - Migração S4 - DG
*    PERFORM map_status_code
*      USING ls_event_in
*      CHANGING lv_nfe_status_code.
*
*    PERFORM map_auth_date_time
*      USING ls_event_in
*      CHANGING lv_nfe_auth_date lv_nfe_auth_time.

    PERFORM map_status_and_auth_code
      USING ls_event_in ls_active
      CHANGING lv_nfe_status_code lv_nfe_auth_code.

    PERFORM map_auth_date_time
      USING ls_event_in ls_active
      CHANGING lv_nfe_auth_date lv_nfe_auth_time.
*<--- 01/07/2023 - Migração S4 - DG



    IF iv_event_type = gc_cancel_int.                     "2152098
      PERFORM process_nfe_for_cancel_event "incl. commit
        USING ls_event_in
              lv_nfe_msgtyp
              lv_nfe_status_code lv_nfe_auth_code
              lv_nfe_auth_date lv_nfe_auth_time
              ls_active
        CHANGING ct_bapiret2 lx_err.
      IF lx_err = 'X'.                                    "2152098
         CONTINUE.                                        "2152098
      ENDIF.                                              "2152098
    ELSEIF iv_event_type = gc_enc_int.
    ELSE.                                                 "2152098
      PERFORM process_nfe_for_epec_event                  "2152098
        USING ls_event_in                                 "2152098
        CHANGING ls_active                                "2152098
                 ct_bapiret2                              "2152098
                 lx_err.                                  "2152098
      IF lx_err = 'X'.                                    "2152098
        CONTINUE.                                         "2152098
      ENDIF.                                              "2152098
    ENDIF.                                                "2152098

  ENDLOOP.

ENDFORM.

ENDENHANCEMENT.

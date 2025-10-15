"Name: \FU:J_1BNFE_EVENT_IN\SE:BEGIN\EI
ENHANCEMENT 0 Z_GRC_RETORNO_EVENT_DOC.

  DATA: wl_zevent_in TYPE zevent_in,
        w_campos_nfe TYPE zde_campos_nfe,
        zcl_util     TYPE REF TO zcl_util.

*-#127611-17.11.2023-JT-inicio
  CREATE OBJECT zcl_util.

*-atribuir DOCNUM
  LOOP AT it_events INTO DATA(wa_events) WHERE docnum IS INITIAL.
    w_campos_nfe = zcl_util->get_atributos_nfe( wa_events-acckey ).

    SELECT docnum
      INTO @DATA(l_docnum)
      FROM j_1bnfe_active
        UP TO 1 ROWS
     WHERE regio   = @w_campos_nfe-regio
       AND nfyear  = @w_campos_nfe-nfyear
       AND nfmonth = @w_campos_nfe-nfmonth
       AND stcd1   = @w_campos_nfe-stcd1
       AND model   = @w_campos_nfe-model
       AND serie   = @w_campos_nfe-serie
       AND nfnum9  = @w_campos_nfe-nfnum9
       AND docnum9 = @w_campos_nfe-docnum9
       AND cdv     = @w_campos_nfe-cdv
       AND form   <> @abap_off.
    ENDSELECT.

    IF sy-subrc = 0.
      wa_events-docnum     = l_docnum.
      MODIFY  it_events FROM wa_events.
    ENDIF.
  ENDLOOP.
*-#127611-17.11.2023-JT-fim

  "Gravar retorno eventos em tabela intermediaria
  LOOP AT it_events INTO DATA(ls_events_ret).
    CLEAR: wl_zevent_in.

    MOVE-CORRESPONDING ls_events_ret TO wl_zevent_in.

    wl_zevent_in-dt_registro   = sy-datum.
    wl_zevent_in-hr_registro   = sy-uzeit.
    wl_zevent_in-us_registro   = sy-uname.
    wl_zevent_in-rg_atualizado = abap_false.

    MODIFY zevent_in FROM wl_zevent_in.
  ENDLOOP.
  COMMIT WORK.
  "Fim retorno eventos em tabela intermediaria

  DATA: lt_j_1bnfe_event TYPE TABLE OF j_1bnfe_event.

  DATA: ls_events_z      TYPE j_1bnfe_event_in,             "1711095
        lt_cce_in_z      TYPE j_1bnfe_event_in_tab,         "1711095
        ls_evemap_z      TYPE j_1bnfe_evemap,               "1711095
        lv_message_z     TYPE c, "#EC NEEDED                   "1711095
        lv_message_key_z TYPE string,                       "1711095
        lt_events_z      TYPE j_1bnfe_event_in_tab,         "2152098
        lv_event_type_z  TYPE j_1bnfe_int_event.            "2152098

  DATA  lv_subrc_z       TYPE sy-subrc.                     "2492149
  DATA  ls_bapiret2_z    TYPE bapiret2.                     "2492149

  cl_j_1bnfe_authority_check=>authority_check(              "2492149
    IMPORTING                                               "2492149
      ev_subrc = lv_subrc_z ).                              "2492149
  IF lv_subrc_z = 8.                                        "2492149
    cl_j_1bnfe_authority_check=>message_not_authorized(     "2492149
      IMPORTING                                             "2492149
        es_bapiret = ls_bapiret2_z ).                       "2492149
    APPEND ls_bapiret2_z TO ct_bapiret2.                    "2492149
    RETURN.                                                 "2492149
  ENDIF.                                                    "2492149

  SORT it_events BY docnum ext_event seqnum.

*-------------------------------------------------------------  1711095*
* Identification which type of event                            1711095
*-------------------------------------------------------------- 1711095*
  LOOP AT it_events INTO ls_events_z.

    CLEAR lv_message_key_z.
    CONCATENATE ls_events_z-docnum ls_events_z-ext_event ls_events_z-seqnum
               INTO lv_message_key_z SEPARATED BY space.
*  Read event mapping
    READ TABLE gt_eve_map INTO ls_evemap_z
         WITH KEY ext_event = ls_events_z-ext_event.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM j_1bnfe_evemap INTO ls_evemap_z
        WHERE ext_event = ls_events_z-ext_event.
      IF sy-subrc <> 0.
        MESSAGE e501(j1b_nfe_erp_grc) WITH ls_events_z-ext_event
               INTO lv_message_z.
        PERFORM fill_bapiret2
        USING sy-msgty sy-msgid sy-msgno
              sy-msgv1 sy-msgv2 sy-msgv3
              lv_message_key_z space space space
        CHANGING ct_bapiret2[].
        CONTINUE.
      ELSE.
        APPEND ls_evemap_z TO gt_eve_map.
      ENDIF.
    ENDIF.

    "Collect events                                             "1711095
    CASE  ls_evemap_z-int_event.

      WHEN gc_cce_int. "CCe events                               1711095
        APPEND ls_events_z TO lt_cce_in_z.

      WHEN gc_cancel_int. "Cancel events                         1711095
        APPEND ls_events_z TO lt_events_z.                  "2152098
        lv_event_type_z = gc_cancel_int.                    "2152098
      WHEN gc_epec_int.                                     "2152098
        APPEND ls_events_z TO lt_events_z.                  "2152098
        lv_event_type_z = gc_epec_int.                      "2152098
      WHEN gc_enc_int. "Evento Encerramento
        APPEND ls_events_z TO lt_events_z.
        lv_event_type_z = gc_enc_int.
      WHEN OTHERS.
        "Currently not supported, error.                         1711095
        MESSAGE e502(j1b_nfe_erp_grc) WITH ls_evemap_z-ext_event
                INTO lv_message_z.
        PERFORM fill_bapiret2
        USING sy-msgty sy-msgid sy-msgno
              sy-msgv1 sy-msgv2 sy-msgv3
              lv_message_key_z space space space
        CHANGING ct_bapiret2[].
        CONTINUE.
    ENDCASE.
  ENDLOOP.

*-------------------------------------------------------------  1711095*
* Processing the events depending on their type                 1711095
*-------------------------------------------------------------- 1711095*
                                                            "1711095
* Process CCe events moved to Include LJ_1B_NFE_EVENTF03        "1711095
  PERFORM process_cce_events                                "1711095
    USING lt_cce_in_z                                       "1711095
    CHANGING ct_bapiret2[].                                 "1711095
                                                            "1711095
  PERFORM z_process_status_events                           "2152098
    USING lt_events_z lv_event_type_z                       "2152098
    CHANGING ct_bapiret2[].                                 "1711095

* Update event table
*    IF NOT lt_eve_update IS INITIAL.
*      UPDATE j_1bnfe_event FROM TABLE lt_eve_update.
*      IF sy-subrc <> 0.
*        MESSAGE e503(j1b_nfe_erp_grc) INTO lv_message.
*        PERFORM fill_bapiret2
*        USING sy-msgty sy-msgid sy-msgno
*              sy-msgv1 sy-msgv2 sy-msgv3
*              sy-msgv4 space space space
*        CHANGING ct_bapiret2[].
*      ENDIF.
*    ENDIF.

  READ TABLE ct_bapiret2 TRANSPORTING NO FIELDS             "2029305
    WITH KEY type = 'E'. " Error                          "2029305
                                                            "2029305
  IF sy-subrc <> 0.                                         "2029305
    READ TABLE ct_bapiret2 TRANSPORTING NO FIELDS           "2029305
      WITH KEY type = 'W'. " Warning                      "2029305
  ENDIF.                                                    "2029305
                                                            "2029305
  DATA(v_error_proc) = abap_false.
  IF sy-subrc = 0.                                          "2029305
    v_error_proc = abap_true.
    ROLLBACK WORK.                                          "2029305
  ENDIF.                                                    "2029305

* We can only update events which are issued by ERP. Not own events
* can be inserted or updated - rules to be determined...

* Release locks
  CALL FUNCTION 'DEQUEUE_ALL'.

  COMMIT WORK.

  IF ( v_error_proc = abap_false ).

    CLEAR: lt_j_1bnfe_event[].

    SELECT *
      FROM j_1bnfe_event INTO TABLE lt_j_1bnfe_event
       FOR ALL ENTRIES IN it_events
     WHERE docnum = it_events-docnum
       AND seqnum = it_events-seqnum.

    LOOP AT lt_j_1bnfe_event INTO DATA(wl_event_ret).

      DATA(task) = 'RET_EVENT_DOC_GRC' && wl_event_ret-docnum && wl_event_ret-seqnum.

      CALL FUNCTION 'Z_GRC_RETORNO_EVENT_DOC' STARTING NEW TASK task
        EXPORTING
          is_event_common = wl_event_ret.

    ENDLOOP.

  ENDIF.

  CHECK 1 = 2.

ENDENHANCEMENT.

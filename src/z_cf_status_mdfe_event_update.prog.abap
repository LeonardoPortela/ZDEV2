*&---------------------------------------------------------------------*
*& Report J_1BNFE_CF_STATUS_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_cf_status_mdfe_event_update MESSAGE-ID j_1bnfe_cf.

CONSTANTS: lc_obj_name    TYPE tadir-obj_name  VALUE 'CL_J1BNFE_DANFE_DH',
           lc_pgmid       TYPE tadir-pgmid     VALUE 'R3TR',
           lc_object      TYPE tadir-object    VALUE 'CLAS',
           lc_print_class TYPE string          VALUE 'CL_J1BNFE_DANFE_PRINT',
           lc_job_name    TYPE j_1bnfe_jobname VALUE '0'.

DATA: lo_service_exception      TYPE REF TO cx_j_1bnfe_cf,
      lx_nfe_lock               TYPE REF TO cx_nfe_lock,
      lo_job_parameters_handler TYPE REF TO cl_nfe_job_parameters_handler, "2920155
      lt_creation_date_filter   TYPE nfe_date_select_option_tab, "2920155
      lt_document_number_filter TYPE nfe_docnum_select_option_tab, "2920155
      lo_nfe_cancel_helper      TYPE REF TO cl_nfe_cancel_helper,
      lo_nfe_cloud_log_text     TYPE REF TO if_nfe_cloud_log_text,
      ls_active                 TYPE j_1bnfe_active,
      lo_active_da              TYPE REF TO cl_j_1bnfe_active_da.

* New Declarations ----------------------------------------------------------*
DATA: lva_data_credat       TYPE j_1bnfe_active-credat.
DATA: lva_data_credat_event TYPE j_1bnfe_event-issue_tmpl.

DATA: lit_active       TYPE TABLE OF j_1bnfe_active,
      lit_event        TYPE TABLE OF j_1bnfe_event,
      lit_doc          TYPE TABLE OF j_1bnfdoc,
      lit_event_update TYPE j_1bnfe_event_in_tab.

DATA: lob_mdfe_processor TYPE REF TO cl_nfe_cloud_mdfe_processor,
      lob_external_cf    TYPE REF TO cl_j_1bnfe_external_cf.


DATA: lwa_status_request     TYPE nfe_cloud_request_uuid_list,
      lwa_company_definition TYPE j_1bnfe_s_company_code_def.

CONSTANTS: c_action_authorize	 TYPE c LENGTH 50 VALUE 'AUTHORIZE'.
CONSTANTS: c_action_ev_status	 TYPE c LENGTH 50 VALUE 'EV_STATUS'.
CONSTANTS: c_completed         TYPE c LENGTH 50 VALUE 'COMPLETED'.
CONSTANTS: c_not_found         TYPE c LENGTH 50 VALUE 'NOT_FOUND'.

DATA lwa_response_status TYPE j_1bnfe_s_authorize_status_ret.

DATA: lit_event_single TYPE j_1bnfe_event_tab.

*----------------------------------------------------------*

PARAMETERS: p_times TYPE i DEFAULT 1,
            p_wait  TYPE i DEFAULT 10.

START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  GET TIME STAMP FIELD lva_data_credat_event.
  TRY.
      cl_abap_tstmp=>subtractsecs(
        EXPORTING
          tstmp   = lva_data_credat_event
          secs    = 172800 "- 2 Dias
        RECEIVING
          r_tstmp = lva_data_credat_event
      ).
    CATCH cx_parameter_invalid_range.
    CATCH cx_parameter_invalid_type.
  ENDTRY.

  DATA(_del_event_no_btp) = abap_false.
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarc)
   WHERE name = 'DEL_EVE_MDFE_NO_INTREGADO_BTP'.
  IF sy-subrc EQ 0.
    _del_event_no_btp = abap_true.
  ENDIF.

  DO p_times TIMES.

    FREE: lob_mdfe_processor, lob_external_cf.

    CLEAR: lit_active[], lit_doc[],  lit_event[].

    CREATE OBJECT lob_mdfe_processor.
    CREATE OBJECT lob_external_cf.

    SELECT *
      FROM j_1bnfe_event INTO TABLE lit_event
     WHERE issue_tmpl GE lva_data_credat_event
       AND docsta     EQ space.

    IF lit_event[] IS NOT INITIAL.
      SELECT *
        FROM j_1bnfe_active INTO TABLE lit_active
        FOR ALL ENTRIES IN lit_event[]
        WHERE docnum = lit_event-docnum
          AND model  = '58'.
    ENDIF.

    IF lit_active[] IS NOT INITIAL.
      SELECT *
       FROM j_1bnfdoc INTO TABLE lit_doc
        FOR ALL ENTRIES IN lit_event
      WHERE docnum = lit_event-docnum.
    ENDIF.

    "DATA(lit_doc_group) = lit_doc[].

    "SORT lit_doc_group BY bukrs branch model nftype.
    "DELETE ADJACENT DUPLICATES FROM lit_doc_group COMPARING bukrs branch model nftype.

    SORT lit_doc BY bukrs branch model nftype.
    SORT lit_active BY docnum.
    SORT lit_event BY docnum.

    LOOP AT lit_doc INTO DATA(lwa_doc).

      CLEAR: lwa_status_request, lwa_company_definition.

      READ TABLE lit_event INTO DATA(lwa_event) WITH KEY docnum = lwa_doc-docnum BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      APPEND lwa_event-cloud_uuid TO lwa_status_request-uuid_list.

      CHECK lwa_status_request-uuid_list[] IS NOT INITIAL.

      lwa_company_definition-bukrs    = lwa_doc-bukrs.
      lwa_company_definition-branch   = lwa_doc-branch.
      lwa_company_definition-model    = lwa_doc-model.
      lwa_company_definition-nftype   = lwa_doc-nftype.

      DATA(lwa_result) = lob_mdfe_processor->get_status(
        EXPORTING
          is_status_request     = lwa_status_request
          is_company_definition = lwa_company_definition
          iv_action             = CONV #( c_action_ev_status ) ).

      LOOP AT lwa_result-statuses INTO DATA(lwa_statuses).

        CLEAR: lit_event_update[], lit_event_single[].

        READ TABLE lit_event INTO lwa_event WITH KEY cloud_uuid = lwa_statuses-uuid.
        CHECK sy-subrc EQ 0.

        READ TABLE lit_active INTO DATA(lwa_active) WITH KEY docnum = lwa_event-docnum.
        CHECK sy-subrc EQ 0.

        READ TABLE lit_doc INTO lwa_doc WITH KEY docnum = lwa_active-docnum.
        CHECK sy-subrc EQ 0.

        CASE lwa_statuses-processing_status.
          WHEN c_completed.

            APPEND lwa_event TO lit_event_single.

            APPEND INITIAL LINE TO lit_event_update ASSIGNING FIELD-SYMBOL(<fs_event_update>).

            GET TIME STAMP FIELD DATA(lva_timestamp).

            <fs_event_update>-acckey            = lwa_active-regio && lwa_active-nfyear && lwa_active-nfmonth && lwa_active-stcd1 && lwa_active-model && lwa_active-serie && lwa_active-nfnum9 && lwa_active-docnum9 && lwa_active-cdv..
            <fs_event_update>-code              = lwa_statuses-government_status-official_status_code.
            <fs_event_update>-code_description  = lwa_statuses-government_status-official_status_description.
            <fs_event_update>-authcode          = lwa_statuses-government_status-protocol_number.

            IF lwa_statuses-government_status-response_at IS NOT INITIAL.
              PERFORM convert_utc_to_datetime USING lwa_statuses-government_status-response_at
                                            CHANGING <fs_event_update>-ext_reply_tmpl.

              <fs_event_update>-reply_tmpl  = <fs_event_update>-ext_reply_tmpl.
            ENDIF.

            <fs_event_update>-ext_event         = lwa_event-ext_event.
            <fs_event_update>-seqnum            = lwa_event-seqnum.
            <fs_event_update>-ext_seqnum        = lwa_event-seqnum.

            IF <fs_event_update>-code EQ cl_j_1bnfe_cf_constant=>c_event_processed OR
               <fs_event_update>-code EQ cl_j_1bnfe_cf_constant=>c_event_registered_bound OR
               <fs_event_update>-code EQ cl_j_1bnfe_cf_constant=>c_event_registered_not_bound OR
               <fs_event_update>-code EQ cl_j_1bnfe_cf_constant=>c_event_registered_bound_155 OR
               <fs_event_update>-code EQ cl_j_1bnfe_cf_constant=>c_event_registered_bound_151.
              <fs_event_update>-msgtyp = cl_j_1bnfe_cf_constant=>c_msgtyp_authorization.
            ELSE.
              <fs_event_update>-msgtyp = cl_j_1bnfe_cf_constant=>c_msgtyp_rejection.
            ENDIF.

            lob_external_cf->event_update( lit_event_update ).
            COMMIT WORK.

            PERFORM unlock_events USING lit_event_single.

            MESSAGE s039 WITH  <fs_event_update>-acckey <fs_event_update>-code.

          WHEN c_not_found.

            IF _del_event_no_btp = abap_true.

              DELETE FROM j_1bnfe_event WHERE docnum     = lwa_event-docnum
                                          AND int_event  = lwa_event-int_event
                                          AND seqnum     = lwa_event-seqnum.

              MESSAGE s039 WITH  lwa_event-docnum 'REMOVE_EVENT'.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.

    ENDLOOP.

    WAIT UP TO p_wait SECONDS.

  ENDDO.



FORM lock_documents CHANGING ct_electronic_documents TYPE j_1b_tt_nfe_active.

  DATA: ls_electronic_document TYPE j_1bnfe_active,
        ls_job_lock            TYPE nfe_job_lock,
        lv_index               TYPE sy-tabix.

  LOOP AT ct_electronic_documents INTO ls_electronic_document.
    CLEAR ls_job_lock.
    lv_index = sy-tabix.
    TRY.
        ls_job_lock-job_key  = ls_electronic_document-docnum.
        ls_job_lock-job_name = lc_job_name.
        cl_nfe_job_key_lock=>lock( ls_job_lock ).
      CATCH cx_nfe_lock_job_key.
        DELETE ct_electronic_documents INDEX lv_index.
    ENDTRY.
  ENDLOOP.

ENDFORM.

FORM unlock_documents USING it_electronic_documents TYPE j_1b_tt_nfe_active.

  DATA: ls_electronic_document TYPE j_1bnfe_active,
        ls_job_lock            TYPE nfe_job_lock.

  LOOP AT it_electronic_documents INTO ls_electronic_document.
    CLEAR ls_job_lock.
    ls_job_lock-job_key  = ls_electronic_document-docnum.
    ls_job_lock-job_name = lc_job_name.
    cl_nfe_job_key_lock=>unlock( ls_job_lock ).
  ENDLOOP.

ENDFORM.


FORM unlock_events USING it_electronic_events TYPE j_1bnfe_event_tab.

  DATA: ls_electronic_event TYPE j_1bnfe_event,
        ls_job_lock         TYPE nfe_job_lock.

  LOOP AT it_electronic_events INTO ls_electronic_event.
    CLEAR ls_job_lock.
    ls_job_lock-job_key  = ls_electronic_event-docnum.
    ls_job_lock-job_name = lc_job_name.
    cl_nfe_job_key_lock=>unlock( ls_job_lock ).
  ENDLOOP.

ENDFORM.

FORM lock_events CHANGING ct_electronic_events TYPE j_1bnfe_event_tab.

  DATA: ls_electronic_event TYPE j_1bnfe_event,
        ls_job_lock         TYPE nfe_job_lock,
        lv_index            TYPE sy-tabix.

  LOOP AT ct_electronic_events INTO ls_electronic_event.
    CLEAR ls_job_lock.
    lv_index = sy-tabix.
    TRY.
        ls_job_lock-job_key  = ls_electronic_event-docnum.
        ls_job_lock-job_name = lc_job_name.
        cl_nfe_job_key_lock=>lock( ls_job_lock ).
      CATCH cx_nfe_lock_job_key.
        DELETE ct_electronic_events INDEX lv_index.
    ENDTRY.
  ENDLOOP.

ENDFORM.


FORM convert_utc_to_datetime USING p_dh_utc
                          CHANGING c_date_time.

  DATA: lv_timestamp_str TYPE string.

  CHECK p_dh_utc IS NOT INITIAL.

  MOVE p_dh_utc TO lv_timestamp_str.

  REPLACE ALL OCCURRENCES OF ':' IN lv_timestamp_str WITH space
    IN CHARACTER MODE.
  REPLACE ALL OCCURRENCES OF 'T' IN lv_timestamp_str WITH space
    IN CHARACTER MODE.
  REPLACE ALL OCCURRENCES OF '-' IN lv_timestamp_str WITH space
    IN CHARACTER MODE.

  CONDENSE lv_timestamp_str NO-GAPS.

  c_date_time = lv_timestamp_str(8) && lv_timestamp_str+8(6).

ENDFORM.

*&---------------------------------------------------------------------*
*& Report J_1BNFE_CF_STATUS_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_cf_status_mdfe_update MESSAGE-ID j_1bnfe_cf.

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

DATA: lit_active TYPE TABLE OF j_1bnfe_active,
      lit_doc    TYPE TABLE OF j_1bnfdoc.

DATA: lob_mdfe_processor TYPE REF TO cl_nfe_cloud_mdfe_processor,
      lob_update         TYPE REF TO cl_j_1bnfe_cf_update.

DATA: lwa_status_request     TYPE nfe_cloud_request_uuid_list,
      lwa_company_definition TYPE j_1bnfe_s_company_code_def.

CONSTANTS: c_action_authorize	 TYPE c LENGTH 50 VALUE 'AUTHORIZE'.
CONSTANTS: c_completed         TYPE c LENGTH 50 VALUE 'COMPLETED'.

DATA lwa_response_status TYPE j_1bnfe_s_authorize_status_ret.

DATA: lit_active_single TYPE j_1b_tt_nfe_active.

DATA lv_text TYPE string.
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

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
* lva_data_credat = sy-datum - 5.
  lva_data_credat = sy-datum - 15.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  DO p_times TIMES.

    FREE: lob_mdfe_processor, lob_update.

    CLEAR: lit_active[], lit_doc[].

    CREATE OBJECT lob_mdfe_processor.
    CREATE OBJECT lob_update.

    "Consulta processo Autorização MDF-e
    SELECT *
      FROM j_1bnfe_active INTO TABLE lit_active
      WHERE form    <> space
        AND docsta  =  space
        AND msstat  = 'A'    "Solicitação de autorização recebida pelo SM
        AND credat  GE lva_data_credat
        AND model   EQ '58'.

  delete lit_active where cloud_guid is INITIAL.
  SORT lit_active BY CREDAT ASCENDING .


    IF lit_active[] IS NOT INITIAL.
      SELECT *
        FROM j_1bnfdoc INTO TABLE lit_doc
         FOR ALL ENTRIES IN lit_active
        WHERE docnum = lit_active-docnum.
    ENDIF.

    DATA(lit_doc_group) = lit_doc[].

    SORT lit_doc_group BY bukrs branch model nftype.
    DELETE ADJACENT DUPLICATES FROM lit_doc_group COMPARING bukrs branch model nftype.

    SORT lit_doc BY bukrs branch model nftype.
    SORT lit_active BY docnum.

    LOOP AT lit_doc_group INTO DATA(lwa_doc_group).

      CLEAR: lwa_status_request, lwa_company_definition.

      LOOP AT lit_doc INTO DATA(lwa_doc) WHERE bukrs  = lwa_doc_group-bukrs
                                           AND branch = lwa_doc_group-branch
                                           AND model  = lwa_doc_group-model
                                           AND nftype = lwa_doc_group-nftype.

        READ TABLE lit_active INTO DATA(lwa_active) WITH KEY docnum = lwa_doc-docnum BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        APPEND lwa_active-cloud_guid TO lwa_status_request-uuid_list.

      ENDLOOP.

      CHECK lwa_status_request-uuid_list[] IS NOT INITIAL.

      lwa_company_definition-bukrs    = lwa_doc_group-bukrs.
      lwa_company_definition-branch   = lwa_doc_group-branch.
      lwa_company_definition-model    = lwa_doc_group-model.
      lwa_company_definition-nftype   = lwa_doc_group-nftype.

      DATA(lwa_result) = lob_mdfe_processor->get_status(
        EXPORTING
          is_status_request     = lwa_status_request
          is_company_definition = lwa_company_definition
          iv_action             = CONV #( c_action_authorize ) ).

      LOOP AT lwa_result-statuses INTO DATA(lwa_statuses) WHERE processing_status = c_completed.

        CLEAR: lit_active_single[].

        READ TABLE lit_active INTO lwa_active WITH KEY cloud_guid = lwa_statuses-uuid.
        CHECK sy-subrc EQ 0.

        APPEND lwa_active TO lit_active_single.

        READ TABLE lit_doc INTO lwa_doc WITH KEY docnum = lwa_active-docnum.
        CHECK sy-subrc EQ 0.


        "lwa_response_status-ID
        lwa_response_status-cnpj       = lwa_active-stcd1.
        lwa_response_status-chnfe      = lwa_active-regio && lwa_active-nfyear && lwa_active-nfmonth && lwa_active-stcd1 && lwa_active-model && lwa_active-serie && lwa_active-nfnum9 && lwa_active-docnum9 && lwa_active-cdv.

        IF lwa_statuses-government_status-response_at IS NOT INITIAL.
          PERFORM convert_utc_to_datetime USING lwa_statuses-government_status-response_at
                                       CHANGING lwa_response_status-timestamp.
        ENDIF.

        lwa_response_status-versao     = lwa_doc-xmlvers.
        lwa_response_status-tpamb      = lwa_active-tpamb.
        lwa_response_status-cstat      = lwa_statuses-government_status-official_status_code.
        lwa_response_status-xmotivo    = lwa_statuses-government_status-official_status_description.
        lwa_response_status-nprot      = lwa_statuses-government_status-protocol_number.
        lwa_response_status-cmsg       = lwa_statuses-government_status-fisco_information-message_status_code.
        lwa_response_status-xmsg       = lwa_statuses-government_status-fisco_information-message.

        MESSAGE s039 WITH lwa_response_status-chnfe lwa_response_status-cstat.

        TRY.
            lob_update->update_nfe( is_nfe_status  = lwa_response_status ).
          CATCH cx_j_1bnfe_cf INTO lo_service_exception.
            PERFORM unlock_documents USING lit_active_single.
            CLEAR lit_active_single.
            lv_text = lo_service_exception->get_text( ).
            MESSAGE s040 WITH lv_text.
          CATCH cx_nfe_lock INTO lx_nfe_lock.
            MESSAGE s036.
            CONTINUE.
          CATCH cx_root ##CATCH_ALL.                        "2963428
            PERFORM unlock_documents USING lit_active_single.
            CLEAR lit_active_single.                        "2963428
            MESSAGE s041.                                   "2963428
        ENDTRY.

        PERFORM unlock_documents USING lit_active_single.

      ENDLOOP.

    ENDLOOP.


    WAIT UP TO p_wait SECONDS.

  ENDDO.


FORM lock_documents CHANGING ct_electronic_documents TYPE j_1b_tt_nfe_active.

  DATA: ls_electronic_document TYPE j_1bnfe_active,
        ls_job_lock            TYPE nfe_job_lock,
        lv_index               TYPE sy-tabix.

  LOOP AT ct_electronic_documents INTO ls_electronic_document.
*    CLEAR ls_job_lock.
    lv_index = sy-tabix.
*    TRY.
*        ls_job_lock-job_key  = ls_electronic_document-docnum.
*        ls_job_lock-job_name = lc_job_name.
*        cl_nfe_job_key_lock=>lock( ls_job_lock ).
*      CATCH cx_nfe_lock_job_key.
*        DELETE ct_electronic_documents INDEX lv_index.
*    ENDTRY.

*   Lock J_1BNFDOC
    CALL FUNCTION 'ENQUEUE_EJ_1BNFS'
      EXPORTING
        mode_j_1bnfdoc = 'S'
        mandt          = sy-mandt
        docnum         = ls_electronic_document-docnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      DELETE ct_electronic_documents INDEX lv_index.
    ENDIF.

*   Lock J_1BNFE_ACTIVE
    CALL FUNCTION 'ENQUEUE_E_J1BNFE'
      EXPORTING
        mode_j_1bnfe_active = 'E'
        mandt               = sy-mandt
        docnum              = ls_electronic_document-docnum
      EXCEPTIONS
        foreign_lock        = 1
        system_failure      = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      DELETE ct_electronic_documents INDEX lv_index.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM unlock_documents USING it_electronic_documents TYPE j_1b_tt_nfe_active.

  DATA: ls_electronic_document TYPE j_1bnfe_active,
        ls_job_lock            TYPE nfe_job_lock.

  LOOP AT it_electronic_documents INTO ls_electronic_document.
    "CLEAR ls_job_lock.
    "ls_job_lock-job_key  = ls_electronic_document-docnum.
    "ls_job_lock-job_name = lc_job_name.
    "cl_nfe_job_key_lock=>unlock( ls_job_lock ).

    CALL FUNCTION 'DEQUEUE_EJ_1BNFS'
      EXPORTING
        mode_j_1bnfdoc = 'S'
        mandt          = sy-mandt
        docnum         = ls_electronic_document-docnum.

    CALL FUNCTION 'DEQUEUE_E_J1BNFE'
      EXPORTING
        mode_j_1bnfe_active = 'E'
        mandt               = sy-mandt
        docnum              = ls_electronic_document-docnum.
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

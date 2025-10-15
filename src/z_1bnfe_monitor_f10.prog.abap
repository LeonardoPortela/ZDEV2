*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F10
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  row_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM row_selection .

  CALL METHOD ctl_alv_nfe->get_current_cell
    IMPORTING
      es_row_id = wa_selected_rows.

  APPEND wa_selected_rows TO it_selected_rows.

  CALL METHOD ctl_alv_nfe->set_selected_rows
    EXPORTING
      it_index_rows = it_selected_rows.

ENDFORM.                    " row_selection

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_XML
*&---------------------------------------------------------------------*
*-#131273-15.01.2024-JT-inicio
FORM download_xml USING p_op_type  TYPE c
                        p_is_event TYPE abap_bool
                        p_grupo    type c.

  DATA: lo_download                 TYPE REF TO cl_j_1bnfe_xml_download,
        lv_access_key               TYPE j_1b_nfe_access_key_dtel44,
        lv_direction                TYPE j_1b_nfe_direction,
        lv_doctype                  TYPE j_1b_nfe_doctype,
        ls_acckey                   TYPE j_1b_nfe_access_key,
        ls_active                   TYPE j_1bnfe_active,
        ls_event                    TYPE j_1bnfe_event,
        lv_rfcdest                  TYPE rfcdest,
        lv_docentrad                TYPE j_1bnfdoc-entrad,  "2176338
        lo_nfse                     TYPE REF TO if_j_1bnfse,
        lo_download_cloud           TYPE REF TO cl_nfe_cloud_download, "2932848
        lv_is_valid_for_cloud       TYPE abap_bool,                         "2932848 "3039634
        lv_cloud_uuid               TYPE nfe_document_uuid, "2932848
        lx_nfe                      TYPE REF TO cx_nfe,
        lx_file_handler             TYPE REF TO cx_nfe_cloud_badi_file_handler,
        lo_log_error                TYPE REF TO cl_j_1bnfe_error_log,
        lv_is_mass_download_allowed TYPE abap_bool,
*---MDF-e
        lob_mdfe_processor          TYPE REF TO cl_nfe_cloud_mdfe_processor,
        lv_xstring_content          TYPE xstring,                               "*#127333 - 04.12.2023 - JT
        mo_local_file               TYPE REF TO cl_nfe_local_file,
        lv_path                     TYPE string,
        w_result                    TYPE bapiret2,
        t_result                    TYPE bapirettab.

  CONSTANTS lc_model_nfe  TYPE j_1b_nfe_doctype VALUE 'NFE'.
  CONSTANTS lc_model_cte  TYPE j_1b_nfe_doctype VALUE 'CTE'.
  CONSTANTS lc_direct_in  TYPE j_1b_nfe_direction VALUE 'INBD'.
  CONSTANTS lc_direct_out TYPE j_1b_nfe_direction VALUE 'OUTB'.

  " At least one row must be selected
  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'I' NUMBER '030'.
    RETURN.
  ENDIF.

  IF p_op_type = '2'. "Display XML
    "Only one row can be selected for this operation
    READ TABLE it_selected_rows INDEX 2 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MESSAGE ID 'J1B_NFE' TYPE 'I' NUMBER '009'.
      RETURN.
    ENDIF.
  ENDIF.

  IF lines( it_selected_rows ) > 1.                         "3007000
    PERFORM check_mass_download CHANGING lv_is_mass_download_allowed.
    IF lv_is_mass_download_allowed = abap_true.
      PERFORM mass_download USING p_grupo.                                "3007000
    ENDIF.
  ELSE.                                                     "3007000

    " Retrieve docnum from selected line in the ALV grid
    READ TABLE it_selected_rows INDEX 1
          INTO wa_selected_rows.
    READ TABLE it_nfe_alv INDEX wa_selected_rows-index
          INTO wa_nfe_alv.

    lv_cloud_uuid = wa_nfe_alv-cloud_guid.                  "2932848

    " Retrieve key information
    SELECT SINGLE *
      FROM j_1bnfe_active
      INTO ls_active
     WHERE docnum = wa_nfe_alv-docnum.

    MOVE-CORRESPONDING ls_active TO ls_acckey.

*   SELECT SINGLE *
*     FROM j_1bnfe_event
*     INTO ls_event
*    WHERE docnum    EQ wa_nfe_alv-docnum
*      AND ext_event EQ i_evento
*      AND docsta    EQ '1' "Autorizado
*      AND seqnum    EQ ( SELECT MAX( seqnum )
*                           FROM j_1bnfe_event
*                          WHERE docnum    EQ ls_active-docnum
*                            AND ext_event EQ i_evento
*                            AND docsta    EQ '1' ).

    " Map the key to the proper structure
    lv_access_key = ls_acckey.

    " Retrieve the GRC rfc connection
    PERFORM get_rfc_destination
      USING ls_active
      CHANGING lv_rfcdest.

* Service Nota Fiscal (NFS-e)                                                                                         "2520709
    lo_nfse = cl_j_1bnfse=>get_instance( ).
    IF lo_nfse->is_service_notafiscal( iv_document_number = ls_active-docnum ) = abap_true. "2520709.
      lv_rfcdest = if_j_1bnfse=>mc_nfse_downloadxml_key.    "2520709
      lv_access_key = ls_active-rps.                        "3001273
      IF wa_nfe_alv-conting = abap_true.                    "2932848
        MESSAGE ID 'NFE' TYPE 'I' NUMBER '004'.             "2932848
        RETURN.                                             "2932848
      ENDIF.                                                "2932848
    ENDIF.                                                  "2520709

*---------------------------------------
*-  MDF-e
*---------------------------------------
    IF ls_active-model = '58'.
      CREATE OBJECT lob_mdfe_processor.

      TRY.
          DATA(lwa_document_info) = lob_mdfe_processor->get_file(
            EXPORTING
              iv_environment_type = CONV #( ls_active-tpamb )
              iv_action           = 'AUTHORIZE'
              iv_uuid             = CONV #( ls_active-cloud_guid ) ).

          IF lwa_document_info-file_content IS NOT INITIAL.
            CREATE OBJECT mo_local_file.

            IF p_op_type = cl_nfe_constant_download_actio=>download.
              mo_local_file->download_file( iv_base64_content = lwa_document_info-file_content
                                            iv_filepath       = lv_path
                                            iv_filename       = lwa_document_info-file_name ).
            ELSE.
              mo_local_file->display_file( iv_base64_content = lwa_document_info-file_content
                                           iv_filename       = lwa_document_info-file_name ).
            ENDIF.
          ELSE.
            FREE: t_result.
            w_result-type       = 'E'.
            w_result-id         = 'SD'.
            w_result-number     = '024'.
            w_result-message_v1 = 'Não foi possível abrir XML.'.
            APPEND w_result  TO t_result.

            CREATE OBJECT lo_log_error.
            lo_log_error->add_error_message_table( t_result ).
            lo_log_error->display_error_log( ).
          ENDIF.
        CATCH cx_nfe_cloud_download_error.
          FREE: t_result.
          w_result-type       = 'E'.
          w_result-id         = 'SD'.
          w_result-number     = '024'.
          w_result-message_v1 = 'Não foi possível abrir XML.'.
          APPEND w_result  TO t_result.

          CREATE OBJECT lo_log_error.
          lo_log_error->add_error_message_table( t_result ).
          lo_log_error->display_error_log( ).
      ENDTRY.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_download_cloud.
    lv_is_valid_for_cloud = lo_download_cloud->is_valid_for_cloud( iv_document_number     = ls_active-docnum    "2932848 "3039634
                                                                   iv_company_code        = ls_active-bukrs     "2932848 "3039634
                                                                   iv_business_place      = ls_active-branch    "2932848 "3039634
                                                                   is_electronic_document = ls_active
                                                                   is_event               = p_is_event ).
    IF lv_is_valid_for_cloud = abap_true.                    "2932848 "3039634
*     MOVE-CORRESPONDING wa_nfe_alv TO ls_event ##ENH_OK.
      TRY.
          lo_download_cloud->download( is_electronic_nota_fiscal = ls_active
                                       is_nfe_event              = ls_event
                                       iv_uuid                   = COND #( WHEN ls_event-cloud_uuid IS NOT INITIAL THEN ls_event-cloud_uuid  ""#131037 - 09.01.2024 - JT
                                                                                                                   ELSE lv_cloud_uuid )
*                                      iv_uuid                   = lv_cloud_uuid                           "2932848
                                       iv_option                 = p_op_type ). "2932848
        CATCH cx_nfe_cloud_badi_file_handler INTO lx_file_handler.
          CREATE OBJECT lo_log_error.
          lo_log_error->add_error_message_table( lx_file_handler->get_messages( ) ).
          lo_log_error->display_error_log( ).
        CATCH cx_nfe INTO lx_nfe.
          lx_nfe->raise_message( ).
      ENDTRY.
      RETURN.                                               "2932848
    ENDIF.                                                  "2932848

    " If connection was found
    IF lv_rfcdest IS NOT INITIAL.

      " Instantiate download object
      CREATE OBJECT lo_download
        EXPORTING
          iv_xml_key = lv_access_key
          iv_rfc     = lv_rfcdest.

      "   Check the nf type
      CASE ls_active-model.
        WHEN 55.
          lv_doctype = lc_model_nfe.
        WHEN 57.
          lv_doctype = lc_model_cte.
      ENDCASE.

      "   Check the direction
      CASE ls_active-direct.
        WHEN '1'.
          " Check if flaged as 'entrada' and
          " chage direction if needed
          SELECT SINGLE entrad                              "2176338
            FROM j_1bnfdoc                                  "2176338
            INTO lv_docentrad                               "2176338
            WHERE docnum = ls_active-docnum.                "2176338
          IF lv_docentrad = abap_true.                      "2176338
            lv_direction = lc_direct_out.                   "2176338
          ELSE.                                             "2176338
            lv_direction = lc_direct_in.                    "2176338
          ENDIF.                                            "2176338
        WHEN '2'.
          lv_direction = lc_direct_out.
      ENDCASE.

      CASE p_op_type.
        WHEN '1'.
          " Start downloading XML
          CALL METHOD lo_download->save_xml_to_file
            EXPORTING
              iv_docnum       = wa_nfe_alv-docnum
              iv_event_type   = ls_event-ext_event
              iv_event_seqnum = ls_event-seqnum
              iv_direction    = lv_direction
              iv_doctype      = lv_doctype.

        WHEN '2'.
          " Start downloading XML
          CALL METHOD lo_download->save_xml_to_screen
            EXPORTING
              iv_docnum       = wa_nfe_alv-docnum
              iv_event_type   = ls_event-ext_event
              iv_event_seqnum = ls_event-seqnum
              iv_direction    = lv_direction
              iv_doctype      = lv_doctype.

      ENDCASE.

      FREE lo_download.
      CLEAR wa_selected_rows.
      CLEAR it_selected_rows.

    ENDIF.

  ENDIF.

ENDFORM.
*-#131273-15.01.2024-JT-fim

FORM download_xml_old.

  DATA: selected_folder	TYPE string,
        lt_xml          TYPE TABLE OF char80,
        tamanho         TYPE i.

  PERFORM row_selection.

* Check if a selection was made
  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

  sy-subrc = 0.

  IF ok_code EQ 'ARQXML'.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Pasta para salvar arquivos XML'
      CHANGING
        selected_folder      = selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
  ENDIF.

  CHECK sy-subrc IS INITIAL.

  LOOP AT it_selected_rows INTO wa_selected_rows.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
    CASE ok_code.
      WHEN 'ARQXML'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_nfe_alv-docnum
              )->set_registro( EXPORTING i_docnum = wa_nfe_alv-docnum i_sem_bloqueio = abap_true
              )->get_xml( IMPORTING e_xml = DATA(e_xml)
              )->get_registro( IMPORTING e_info_doc_eletronico = DATA(e_info_doc_eletronico)
              ).

            DATA(filename) = selected_folder && '\' &&
                             e_info_doc_eletronico-regio &&
                             e_info_doc_eletronico-nfyear &&
                             e_info_doc_eletronico-nfmonth &&
                             e_info_doc_eletronico-stcd1 &&
                             e_info_doc_eletronico-model &&
                             e_info_doc_eletronico-serie &&
                             e_info_doc_eletronico-nfnum9 &&
                             e_info_doc_eletronico-docnum9 &&
                             e_info_doc_eletronico-cdv &&
                             '.xml'.

            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = e_xml
              IMPORTING
                output_length = tamanho
              TABLES
                binary_tab    = lt_xml.

            cl_gui_frontend_services=>gui_download(
              EXPORTING
                bin_filesize            = tamanho
                filename                = filename
                filetype                = 'BIN'
              CHANGING
                data_tab                = lt_xml
              EXCEPTIONS
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                not_supported_by_gui    = 22
                error_no_gui            = 23
                OTHERS                  = 24
            ).

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            ENDIF.

          CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
        ENDTRY.

      WHEN 'OPENXML'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_nfe_alv-docnum
              )->set_registro( EXPORTING i_docnum = wa_nfe_alv-docnum i_sem_bloqueio = abap_true
              )->get_urls_docs( IMPORTING e_link_xml = DATA(e_link_xml)
              ).

            cl_gui_frontend_services=>execute(
              EXPORTING
                document               = e_link_xml
                operation              = 'OPEN'
              EXCEPTIONS
                cntl_error             = 1
                error_no_gui           = 2
                bad_parameter          = 3
                file_not_found         = 4
                path_not_found         = 5
                file_extension_unknown = 6
                error_execute_failed   = 7
                synchronous_failed     = 8
                not_supported_by_gui   = 9
                OTHERS                 = 10
            ).

          CATCH zcx_doc_eletronico INTO ex_doc_eletronico.    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
        ENDTRY.

    ENDCASE.


  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_pdf .

  DATA: selected_folder	TYPE string,
        lt_pdf          TYPE TABLE OF char80,
        tamanho         TYPE i.

  PERFORM row_selection.

* Check if a selection was made
  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

  sy-subrc = 0.

  IF ok_code EQ 'ARQPDF'.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Pasta para salvar arquivos XML'
      CHANGING
        selected_folder      = selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
  ENDIF.

  CHECK sy-subrc IS INITIAL.

  LOOP AT it_selected_rows INTO wa_selected_rows.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

    CASE ok_code.
      WHEN 'ARQPDF'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_nfe_alv-docnum
              )->set_registro( EXPORTING i_docnum = wa_nfe_alv-docnum i_sem_bloqueio = abap_true
              )->get_pdf( IMPORTING e_pdf = DATA(e_pdf)
              )->get_registro( IMPORTING e_info_doc_eletronico = DATA(e_info_doc_eletronico)
              ).

            DATA(filename) = selected_folder && '\' &&
                             e_info_doc_eletronico-regio &&
                             e_info_doc_eletronico-nfyear &&
                             e_info_doc_eletronico-nfmonth &&
                             e_info_doc_eletronico-stcd1 &&
                             e_info_doc_eletronico-model &&
                             e_info_doc_eletronico-serie &&
                             e_info_doc_eletronico-nfnum9 &&
                             e_info_doc_eletronico-docnum9 &&
                             e_info_doc_eletronico-cdv &&
                             '.pdf'.

            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = e_pdf
              IMPORTING
                output_length = tamanho
              TABLES
                binary_tab    = lt_pdf.

            cl_gui_frontend_services=>gui_download(
              EXPORTING
                bin_filesize            = tamanho
                filename                = filename
                filetype                = 'BIN'
              CHANGING
                data_tab                = lt_pdf
              EXCEPTIONS
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                not_supported_by_gui    = 22
                error_no_gui            = 23
                OTHERS                  = 24
            ).

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            ENDIF.

          CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
        ENDTRY.

      WHEN 'OPENPDF'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_nfe_alv-docnum
              )->set_registro( EXPORTING i_docnum = wa_nfe_alv-docnum i_sem_bloqueio = abap_true
              )->get_urls_docs( IMPORTING e_link_pdf = DATA(e_link_pdf)
              ).

            cl_gui_frontend_services=>execute(
              EXPORTING
                document               = e_link_pdf
                operation              = 'OPEN'
              EXCEPTIONS
                cntl_error             = 1
                error_no_gui           = 2
                bad_parameter          = 3
                file_not_found         = 4
                path_not_found         = 5
                file_extension_unknown = 6
                error_execute_failed   = 7
                synchronous_failed     = 8
                not_supported_by_gui   = 9
                OTHERS                 = 10
            ).

          CATCH zcx_doc_eletronico INTO ex_doc_eletronico.    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
        ENDTRY.

    ENDCASE.



  ENDLOOP.

ENDFORM.

*-#131273-15.01.2024-JT-inicio
FORM get_rfc_destination     USING p_active  TYPE j_1bnfe_active
                          CHANGING p_rfcdest TYPE rfcdest.

  DATA: lv_rfcdest       TYPE rfcdest.
  DATA: lv_xnfeactive    TYPE j_1bxnfeactive.

  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      i_bukrs      = p_active-bukrs
      i_branch     = p_active-branch
      i_model      = p_active-model
    IMPORTING
      e_rfcdest    = lv_rfcdest
      e_xnfeactive = lv_xnfeactive
    EXCEPTIONS
      rfc_error    = 1
      OTHERS       = 2.

  IF sy-subrc IS INITIAL AND lv_xnfeactive = 'X'.
    p_rfcdest = lv_rfcdest.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form check_mass_download
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_mass_download CHANGING cv_is_mass_download_allowed TYPE abap_bool.

  DATA: ls_nfe_active      TYPE j_1bnfe_active,
        lt_nfe_active      TYPE TABLE OF j_1bnfe_active,
        lo_download_helper TYPE REF TO cl_nfe_monitor_download_helper.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_nfe_alv INDEX wa_selected_rows-index INTO wa_nfe_alv.
    IF sy-subrc = 0.
      CLEAR ls_nfe_active.
      MOVE-CORRESPONDING wa_nfe_alv TO ls_nfe_active.
      APPEND ls_nfe_active TO lt_nfe_active.
    ENDIF.
  ENDLOOP.

  CREATE OBJECT lo_download_helper.

  cv_is_mass_download_allowed = lo_download_helper->is_mass_download_allowed( lt_nfe_active ).

ENDFORM.

FORM mass_download  USING p_grupo    type c.

  CONSTANTS lc_model_nfe  TYPE j_1b_nfe_doctype   VALUE 'NFE'.
  CONSTANTS lc_direct_out TYPE j_1b_nfe_direction VALUE 'OUTB'.
  CONSTANTS lc_cloud      TYPE rfcdest            VALUE 'CLOUDNFE'.
  CONSTANTS lc_event_path TYPE string             VALUE '\Event\'.
  CONSTANTS lc_slash      TYPE string             VALUE '\'.

  DATA lo_download   TYPE REF TO cl_j_1bnfe_xml_download.
  DATA ls_acckey     TYPE j_1b_nfe_access_key.
  DATA lv_access_key TYPE j_1b_nfe_access_key_dtel44.
  DATA ls_nfe_active TYPE j_1bnfe_active.
  DATA lv_folder     TYPE string.
  DATA lv_path       TYPE string.

  DATA: lo_download_cloud     TYPE REF TO cl_nfe_cloud_download, "2932848
        lv_is_valid_for_cloud TYPE abap_bool.                "2932848 "3039634
  DATA lx_nfe          TYPE REF TO cx_nfe.
  DATA lx_file_handler TYPE REF TO cx_nfe_cloud_badi_file_handler.
  DATA lo_log_error    TYPE REF TO cl_j_1bnfe_error_log.

  lv_folder = cl_j_1bnfe_xml_download=>mass_download_folder_selection( ).

  CHECK lv_folder IS NOT INITIAL.
  CREATE OBJECT lo_download_cloud.

  LOOP AT it_selected_rows INTO wa_selected_rows.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING wa_nfe_alv TO ls_acckey.

    CLEAR ls_nfe_active.
    MOVE-CORRESPONDING wa_nfe_alv TO ls_nfe_active ##ENH_OK.

*>>>Begin-Stefanini-140437-Ajuste download XML em massa Vitor  Rienzo - 20.06.2024
    if  p_grupo = 'S'.
    lv_path = lv_folder && lc_slash.
    else.
    lv_path = lv_folder        && lc_slash && ls_acckey-stcd1   && lc_slash "2932848
           && ls_acckey-nfyear && lc_slash && ls_acckey-nfmonth && lc_slash. "2932848
    endif.
*<<<End-Stefanini-140437-Ajuste download XML em massa Vitor  Rienzo - 20.06.2024

    lv_is_valid_for_cloud = lo_download_cloud->is_valid_for_cloud( iv_document_number     = wa_nfe_alv-docnum   "2932848 "3039634
                                                                   iv_company_code        = wa_nfe_alv-bukrs    "2932848 "3039634
                                                                   iv_business_place      = wa_nfe_alv-branch   "2932848 "3039634
                                                                   is_electronic_document = ls_nfe_active ).
    IF lv_is_valid_for_cloud = abap_true.                                                                 "2932848 "3039634
      TRY.                                                  "2932848
          lo_download_cloud->download( is_electronic_nota_fiscal = ls_nfe_active
                                       iv_uuid                   = wa_nfe_alv-cloud_guid                      "2932848
                                       iv_path                   = lv_path ).                                 "2932848
        CATCH cx_nfe_cloud_badi_file_handler INTO lx_file_handler.
          CREATE OBJECT lo_log_error.
          lo_log_error->add_error_message_table( lx_file_handler->get_messages( ) ).
          lo_log_error->display_error_log( ).
          FREE lo_log_error.
        CATCH cx_nfe INTO lx_nfe.
          lx_nfe->raise_message( ).
      ENDTRY.
      CONTINUE.                                             "2932848
    ENDIF.                                                  "2932848

*   Mass Download NF-es
    CHECK ls_acckey IS NOT INITIAL.
    lv_access_key = ls_acckey.

    " Instantiate download object
    CREATE OBJECT lo_download
      EXPORTING
        iv_xml_key = lv_access_key
        iv_rfc     = lc_cloud.

    " Start downloading XML
    CALL METHOD lo_download->mass_download_xml
      EXPORTING
        iv_docnum    = wa_nfe_alv-docnum
        iv_direction = lc_direct_out
        iv_doctype   = lc_model_nfe
        iv_path      = lv_path.


*   Mass Download NF-es Events
    IF wa_nfe_alv-event_flag IS NOT INITIAL.
      lv_path = lv_path && lc_event_path.

      " Start downloading XML
      CALL METHOD lo_download->mass_download_xml
        EXPORTING
          iv_docnum       = wa_nfe_alv-docnum
          iv_direction    = lc_direct_out
          iv_doctype      = lc_model_nfe
          iv_path         = lv_path
          iv_mass_dl_type = 'E'.
    ENDIF.

    FREE lo_download.
    CLEAR wa_selected_rows.
    CLEAR lv_path.
    CLEAR ls_acckey.
  ENDLOOP.
  CLEAR it_selected_rows.

ENDFORM.
*-#131273-15.01.2024-JT-fim

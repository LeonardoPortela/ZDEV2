"Name: \TY:CL_J_1BNFE_XML_DOWNLOAD\ME:LOAD_XML_CONTENT\SE:BEGIN\EI
ENHANCEMENT 0 Z_XML_MDFE_DOWNLOAD_OBJ.
    DATA lo_xml_download_v TYPE REF TO cl_j_1bnfe_xml_download_da.
    DATA lt_messages_v     TYPE bapirettab.
    DATA lo_log_error_v    TYPE REF TO cl_j_1bnfe_error_log.
    DATA lv_rfc_msg_v      TYPE string.
    DATA lo_xml_v          TYPE REF TO cl_j_1bnfe_cf_xml_download.
    data: check TYPE char01.

    DATA lv_is_cloud_service_v TYPE boolean.

*   Service Nota Fiscal (NFS-e)                                               "2520709
*    IF rfcdest = if_j_1bnfse=>mc_nfse_downloadxml_key                         "2520709
*    AND xml_key = if_j_1bnfse=>mc_nfse_downloadxml_key.                       "2520709
*      xml_content = cl_j_1bnfse=>get_instance( )->download_xml( iv_docnum ).  "2520709
*      RETURN.                                                                 "2520709
*    ENDIF.                                                                    "2520709

    lo_xml_download_v = cl_j_1bnfe_xml_download_da=>get_instance( ).

*   NF-e Cloud Services
    lv_is_cloud_service_v = load_xml_content_from_service(  iv_docnum       = iv_docnum
                                                          iv_event_type   = iv_event_type
                                                          iv_event_seqnum = iv_event_seqnum
                                                          iv_mass_dl_type = iv_mass_dl_type ).

    IF lv_is_cloud_service_v IS INITIAL.

      TRY.

      lo_xml_download_v->xml_read(
        EXPORTING
            IV_XML_KEY   = xml_key
            IV_DIRECTION = iv_direction
            IV_DOCTYPE   = iv_doctype
            IV_RFC_DEST  = rfcdest
        IMPORTING
            et_messages  = lt_messages_v
            EV_XML       = xml_content
        ).

    "RFC connection failure
        CATCH cx_j1bnfe_rfc_conn INTO DATA(lx_excep_con).
          lv_rfc_msg_v  = lx_excep_con->if_message~get_text( ). "RFC message
      MESSAGE lv_rfc_msg_v TYPE 'E'.

    "Messages from GRC
        CATCH cx_j1bnfe_messages INTO DATA(lo_excep_msg).
      CREATE OBJECT lo_log_error_V.

      CALL METHOD lo_log_error_v->add_error_message_table
        EXPORTING
              it_bapi_ret = lt_messages_v.

*      CALL METHOD lo_log_error->display_error_log.

      FREE lo_log_error_v.

    ENDTRY.
    ENDIF.

    clear: check.
    IF sy-tcode eq 'ZFIS59' OR sy-tcode eq 'zsdt0110' OR  sy-tcode eq 'ZFIS25'.
    check = abap_true.
    ENDIF.

   CHECK check is INITIAL.
ENDENHANCEMENT.

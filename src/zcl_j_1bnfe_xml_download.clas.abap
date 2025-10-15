class ZCL_J_1BNFE_XML_DOWNLOAD definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(IV_XML_KEY) type J_1B_NFE_ACCESS_KEY_DTEL44
      value(IV_RFC) type RFCDEST .
  methods LOAD_XML_CONTENT
    importing
      value(IV_DIRECTION) type J_1B_NFE_DIRECTION
      !IV_DOCNUM type J_1BDOCNUM optional
      value(IV_DOCTYPE) type J_1B_NFE_DOCTYPE
      !IV_EVENT_TYPE type J_1BNFE_EXT_EVENT optional
      !IV_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO optional
      !IV_MASS_DL_TYPE type CHAR1 optional
    raising
      ZCX_ERROR .
  methods SAVE_XML_TO_SCREEN
    importing
      value(IV_DIRECTION) type J_1B_NFE_DIRECTION
      !IV_DOCNUM type J_1BDOCNUM optional
      value(IV_DOCTYPE) type J_1B_NFE_DOCTYPE
      !IV_EVENT_TYPE type J_1BNFE_EXT_EVENT optional
      !IV_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO optional .
  methods MASS_DOWNLOAD_XML
    importing
      !IV_DOCNUM type J_1BDOCNUM optional
      !IV_EVENT_TYPE type J_1BNFE_EXT_EVENT optional
      !IV_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO optional
      !IV_PATH type STRING optional
      !IV_MASS_DL_TYPE type CHAR1 optional
      value(IV_DIRECTION) type J_1B_NFE_DIRECTION
      value(IV_DOCTYPE) type J_1B_NFE_DOCTYPE .
  methods SAVE_XML_TO_FILE
    importing
      value(IV_DIRECTION) type J_1B_NFE_DIRECTION
      !IV_DOCNUM type J_1BDOCNUM optional
      value(IV_DOCTYPE) type J_1B_NFE_DOCTYPE
      !IV_EVENT_TYPE type J_1BNFE_EXT_EVENT optional
      !IV_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO optional .
  methods GET_XML_CONTENT
    returning
      value(EV_XML_CONTENT) type J_1B_NFE_XML_CONTENT .
  class-methods MASS_DOWNLOAD_FOLDER_SELECTION
    returning
      value(RV_MASS_DL_FOLDER) type STRING .
protected section.
private section.

  data XML_KEY type J_1B_NFE_ACCESS_KEY_DTEL44 .
  data XML_CONTENT type J_1B_NFE_XML_CONTENT .
  data RFCDEST type RFCDEST .
  data MV_EVENT_TYPE type J_1BNFE_INT_EVENT .
  data MV_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO .

  methods LOAD_XML_CONTENT_FROM_SERVICE
    importing
      !IV_DOCNUM type J_1BDOCNUM optional
      !IV_EVENT_TYPE type J_1BNFE_EXT_EVENT optional
      !IV_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO optional
      !IV_MASS_DL_TYPE type CHAR1 optional
    returning
      value(RV_IS_CLOUD_SERVICE) type BOOLEAN .
ENDCLASS.



CLASS ZCL_J_1BNFE_XML_DOWNLOAD IMPLEMENTATION.


  method CONSTRUCTOR.

 " Set the key for the NF-e
  xml_key = iv_xml_key.
  rfcdest = iv_rfc.

  endmethod.


  method GET_XML_CONTENT.
    ev_xml_content = xml_content.
  endmethod.


  METHOD LOAD_XML_CONTENT.

    DATA LO_XML_DOWNLOAD TYPE REF TO CL_J_1BNFE_XML_DOWNLOAD_DA.
    DATA LT_MESSAGES     TYPE BAPIRETTAB.
    DATA LO_LOG_ERROR    TYPE REF TO CL_J_1BNFE_ERROR_LOG.
    DATA LV_RFC_MSG      TYPE STRING.
    DATA LO_XML          TYPE REF TO CL_J_1BNFE_CF_XML_DOWNLOAD.

    DATA LV_IS_CLOUD_SERVICE TYPE BOOLEAN.

*   Service Nota Fiscal (NFS-e)                                               "2520709
*    IF rfcdest = if_j_1bnfse=>mc_nfse_downloadxml_key                         "2520709
*    AND xml_key = if_j_1bnfse=>mc_nfse_downloadxml_key.                       "2520709
*      xml_content = cl_j_1bnfse=>get_instance( )->download_xml( iv_docnum ).  "2520709
*      RETURN.                                                                 "2520709
*    ENDIF.                                                                    "2520709

    LO_XML_DOWNLOAD = CL_J_1BNFE_XML_DOWNLOAD_DA=>GET_INSTANCE( ).

*   NF-e Cloud Services
    LV_IS_CLOUD_SERVICE = LOAD_XML_CONTENT_FROM_SERVICE(  IV_DOCNUM       = IV_DOCNUM
                                                          IV_EVENT_TYPE   = IV_EVENT_TYPE
                                                          IV_EVENT_SEQNUM = IV_EVENT_SEQNUM
                                                          IV_MASS_DL_TYPE = IV_MASS_DL_TYPE ).

    IF LV_IS_CLOUD_SERVICE IS INITIAL.

      TRY.

          LO_XML_DOWNLOAD->XML_READ(
            EXPORTING
                IV_XML_KEY   = XML_KEY
                IV_DIRECTION = IV_DIRECTION
                IV_DOCTYPE   = IV_DOCTYPE
                IV_RFC_DEST  = RFCDEST
            IMPORTING
                ET_MESSAGES  = LT_MESSAGES
                EV_XML       = XML_CONTENT
            ).

          "RFC connection failure
        CATCH CX_J1BNFE_RFC_CONN INTO DATA(LX_EXCEP_CONN).
          ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LX_EXCEP_CONN->GET_TEXT( ) ).
        CATCH CX_J1BNFE_MESSAGES INTO DATA(LO_EXCEP_MSGS).
          ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LO_EXCEP_MSGS->GET_TEXT( ) ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD load_xml_content_from_service.

    DATA lo_xml                  TYPE REF TO cl_j_1bnfe_cf_xml_download.
    DATA lo_service_locator      TYPE REF TO cl_j_1bnfe_cf_service_loc.
    DATA lo_service_communicator TYPE REF TO cl_j_1bnfe_cf_service_comm.
    DATA lo_j_1bnfe_cf_exception TYPE REF TO cx_j_1bnfe_cf.
    DATA lo_doc_da               TYPE REF TO cl_j_1bnf_doc_da.
    DATA lo_active_da            TYPE REF TO cl_j_1bnfe_active_da.
    DATA lo_log_error            TYPE REF TO cl_j_1bnfe_error_log.
    DATA ls_doc                  TYPE j_1bnfdoc.
    DATA ls_active               TYPE j_1bnfe_active.
    DATA lt_bapiret              TYPE bapirettab.
    DATA ls_bapiret              TYPE bapiret2.
    DATA lv_request_json         TYPE string.
    DATA lv_service_type         TYPE string.
    DATA lv_mass_event_str       TYPE string.
    DATA lv_http_status_response TYPE i.
    DATA: t_callstack TYPE abap_callstack. "#131831-17.01.2024-JT

    TRY.
        CREATE OBJECT lo_active_da.
        lo_active_da->read_entity( EXPORTING iv_docnum  = iv_docnum IMPORTING es_active = ls_active ).

        IF iv_event_type IS NOT INITIAL.
          mv_event_type = iv_event_type.
          mv_event_seqnum = iv_event_seqnum.
          lv_service_type = cl_j_1bnfe_cf_constant=>c_service_event_download.
        ELSEIF iv_mass_dl_type = cl_j_1bnfe_cf_constant=>c_mass_download_auth.
          lv_service_type = cl_j_1bnfe_cf_constant=>c_service_auth_mass_download.
        ELSEIF iv_mass_dl_type = cl_j_1bnfe_cf_constant=>c_mass_download_event.
          lv_service_type = cl_j_1bnfe_cf_constant=>c_service_event_mass_download.
        ELSE.
          lv_service_type = cl_j_1bnfe_cf_constant=>c_service_authorize_download.
        ENDIF.

        CREATE OBJECT lo_doc_da.
        ls_doc = lo_doc_da->find( iv_docnum ).

*       Create service locator using Branch Info.
        lo_service_locator = cl_j_1bnfe_cf_monitor=>create_service_locator_object(
          iv_bukrs        = ls_active-bukrs
          iv_branch       = ls_active-branch
          iv_model        = cl_j_1bnfe_cf_constant=>c_model_nfe
          iv_nftype       = ls_doc-nftype
          iv_service_type = lv_service_type
        ).

        CHECK lo_service_locator->is_cloud_service( ).
        rv_is_cloud_service = abap_true.

*       Map Based on XML Version
        CREATE OBJECT lo_xml
          EXPORTING
            iv_xml_access_key = xml_key
            iv_event_type     = iv_event_type
            iv_event_seqnum   = iv_event_seqnum.

*       Create Client and Oa2c for nfe cloud services
        CREATE OBJECT lo_service_communicator
          EXPORTING
            iv_service_url        = lo_xml->add_url_parameters( lo_service_locator->get_service_url_with_path( ) )
            iv_oa2c_profile       = lo_service_locator->get_service_oa2c_profile( )
            iv_oa2c_configuration = lo_service_locator->get_service_oa2c_configurator( ).

*       Send Request to Cloud Services.
        lo_service_communicator->send_request_to_cloud( iv_posting_method = cl_j_1bnfe_cf_constant=>c_get ).

*       Get HTTP response.
        lv_http_status_response = lo_service_communicator->get_http_status_from_cloud( ).

        IF lv_http_status_response <> cl_j_1bnfe_cf_constant=>c_service_status_accepted "202
          AND lv_http_status_response <> cl_j_1bnfe_cf_constant=>c_service_status_server_error "500
          AND lv_http_status_response <> cl_j_1bnfe_cf_constant=>c_service_status_ok. "200
*         Raise HTTP response exception.
          lo_service_communicator->http_error_handler( lv_http_status_response ).
        ENDIF.

        TRY.
            IF iv_mass_dl_type = cl_j_1bnfe_cf_constant=>c_service_event_download.
              lv_mass_event_str = lo_service_communicator->get_response_from_cloud( ).
              lv_mass_event_str = '<eventosNfe>' && lv_mass_event_str && '</eventosNfe>'.
              xml_content = cl_bcs_convert=>string_to_xstring( lv_mass_event_str ).
            ELSE.
              xml_content = cl_bcs_convert=>string_to_xstring( lo_service_communicator->get_response_from_cloud( ) ).
            ENDIF.
            CLEAR mv_event_type.
            CLEAR mv_event_seqnum.
          CATCH cx_bcs.
            RAISE EXCEPTION TYPE cx_j_1bnfe_cf EXPORTING textid = cx_j_1bnfe_cf=>download_xml_error.
        ENDTRY.

      CATCH cx_j_1bnfe_exception.
      CATCH cx_j_1bnfe_cf INTO lo_j_1bnfe_cf_exception.

*-#131831-17.01.2024-JT-inicio
        CALL FUNCTION 'SYSTEM_CALLSTACK'
          EXPORTING
            max_level = 0
          IMPORTING
            callstack = t_callstack.

        READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZFIS39'.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
*-#131831-17.01.2024-JT-fim

        ls_bapiret = cl_j_1bnfe_external_cf=>fill_bapiret_w_j_1bnfe_cf_excp( lo_j_1bnfe_cf_exception ).
        APPEND ls_bapiret TO lt_bapiret.

        CREATE OBJECT lo_log_error.
        lo_log_error->add_error_message_table( lt_bapiret ).
        lo_log_error->display_error_log( ).
        FREE lo_log_error.
    ENDTRY.
  ENDMETHOD.


  METHOD MASS_DOWNLOAD_FOLDER_SELECTION.
    DATA lv_download_path TYPE string.

    cl_gui_frontend_services=>get_upload_download_path(
      CHANGING
        upload_path                 = lv_download_path
        download_path               = lv_download_path
      EXCEPTIONS
        cntl_error                  = 1
        error_no_gui                = 2
        not_supported_by_gui        = 3
        gui_upload_download_path    = 4
        upload_download_path_failed = 5
        OTHERS                      = 6 ).

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         =  cl_j_1bnfe_cf_constant=>c_mass_download_folder_txt
        initial_folder       =  lv_download_path
      CHANGING
        selected_folder      =  rv_mass_dl_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
    ).

  ENDMETHOD.


  method MASS_DOWNLOAD_XML.

    DATA lo_dom        TYPE REF TO if_ixml_document.  ##NEEDED
    DATA w_string      TYPE xstring.    ##NEEDED
    DATA w_size        TYPE i.
    DATA it_xml        TYPE dcxmllines.

"   Dialog and file properties
    DATA lv_file_filter      TYPE string.
    DATA lv_file_filter_edmx TYPE string.
    DATA lv_file_name        TYPE string.
    DATA lv_def_file_name    TYPE string.
    DATA lv_path             TYPE string.
    DATA lv_fullpath         TYPE string.
    DATA lv_size             TYPE i.

    CONSTANTS lc_file_type TYPE char10 VALUE 'BIN'.
    CONSTANTS lc_window_title TYPE string VALUE 'Save XML As...'.
    CONSTANTS lc_xml_ext      TYPE string VALUE '.XML'.
    CONSTANTS lc_xml_upper    TYPE string VALUE 'XML'.
    CONSTANTS lc_xml_lower    TYPE string VALUE 'xml'.
    CONSTANTS lc_edmx_upper   TYPE string VALUE 'EDMX'.
    CONSTANTS lc_edmx_lower   TYPE string VALUE 'edmx'.

    " Load XML from GRC and store in the class attributes
    CALL METHOD me->load_xml_content
      EXPORTING
        iv_docnum    = iv_docnum
        iv_event_type   = iv_event_type
        iv_event_seqnum = iv_event_seqnum
        iv_direction = iv_direction
        iv_doctype   = iv_doctype
        iv_mass_dl_type = iv_mass_dl_type.

    IF xml_content IS NOT INITIAL.

      " Converts raw string content into DOM object
      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          xml           = xml_content
        IMPORTING
          document      = lo_dom
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.

      " Convert DOM to XML doc (table)
      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
           document      = lo_dom
           pretty_print  = ' '
        IMPORTING
           xml_as_string = w_string
           size          = w_size
        TABLES
           xml_as_table  = it_xml
        EXCEPTIONS
           no_document   = 1
           OTHERS        = 2.

      " Set up the file extension filter for the dialog
      " Exclude cl_gui_frontend_services=>filetype_all
      lv_file_filter_edmx = cl_gui_frontend_services=>filetype_xml.
      REPLACE ALL OCCURRENCES OF lc_xml_upper IN lv_file_filter_edmx WITH lc_edmx_upper RESPECTING CASE.  ##NO_TEXT
      REPLACE ALL OCCURRENCES OF lc_xml_lower IN lv_file_filter_edmx WITH lc_edmx_lower RESPECTING CASE.
      IF cl_gui_frontend_services=>filetype_xml NE lv_file_filter_edmx.
        CONCATENATE cl_gui_frontend_services=>filetype_xml lv_file_filter_edmx '|' INTO lv_file_filter.
      ELSE.
        CONCATENATE cl_gui_frontend_services=>filetype_xml '|' INTO lv_file_filter.
      ENDIF.

      lv_def_file_name = xml_key.

      lv_file_name = iv_path && lv_def_file_name && lc_xml_ext.

      " Only download xml if user chose save button
      IF sy-subrc IS INITIAL AND
         lv_file_name IS NOT INITIAL AND
         iv_path IS NOT INITIAL .

        " Download xml to the selected destination
        cl_gui_frontend_services=>gui_download(
          EXPORTING
            filename     = lv_file_name
            bin_filesize = lv_size
            filetype     = lc_file_type
          CHANGING
            data_tab     = it_xml
          EXCEPTIONS
            file_write_error = 1
            no_authority = 5
            unknown_error = 6
            access_denied = 15
            OTHERS = 24 ).

        IF sy-subrc IS NOT INITIAL.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

    ENDIF.
  endmethod.


  METHOD SAVE_XML_TO_FILE.

    IF IV_DOCTYPE IS INITIAL.

      SELECT SINGLE * INTO @DATA(WA_J_1BNFE_ACTIVE)
        FROM J_1BNFE_ACTIVE
       WHERE DOCNUM EQ @IV_DOCNUM.

      IF SY-SUBRC IS INITIAL.
        CASE WA_J_1BNFE_ACTIVE-MODEL.
            WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.
            IV_DOCTYPE = 'NFE'.
            WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.
            IV_DOCTYPE = 'CTE'.
            WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_MDFE.
            IV_DOCTYPE = 'MFE'.
        ENDCASE.
      ENDIF.

    ENDIF.

    DATA LO_DOM TYPE REF TO IF_IXML_DOCUMENT.  ##NEEDED
    DATA W_STRING TYPE XSTRING.    ##NEEDED
    DATA W_SIZE        TYPE I.
    DATA IT_XML        TYPE DCXMLLINES.

    "   Dialog and file properties
    DATA LV_FILE_FILTER      TYPE STRING.
    DATA LV_FILE_FILTER_EDMX TYPE STRING.
    DATA LV_FILE_NAME        TYPE STRING.
    DATA LV_DEF_FILE_NAME    TYPE STRING.
    DATA LV_PATH             TYPE STRING.
    DATA LV_FULLPATH         TYPE STRING.
    DATA LV_SIZE             TYPE I.

    CONSTANTS LC_FILE_TYPE TYPE CHAR10 VALUE 'BIN'.
    CONSTANTS LC_WINDOW_TITLE TYPE STRING VALUE 'Save XML As...'.

    " Load XML from GRC and store in the class attributes
    CALL METHOD ME->LOAD_XML_CONTENT
      EXPORTING
        IV_DOCNUM       = IV_DOCNUM
        IV_EVENT_TYPE   = IV_EVENT_TYPE
        IV_EVENT_SEQNUM = IV_EVENT_SEQNUM
        IV_DIRECTION    = IV_DIRECTION
        IV_DOCTYPE      = IV_DOCTYPE.

    IF XML_CONTENT IS NOT INITIAL.

      " Converts raw string content into DOM object
      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          XML           = XML_CONTENT
        IMPORTING
          DOCUMENT      = LO_DOM
        EXCEPTIONS
          INVALID_INPUT = 1
          OTHERS        = 2.

      " Convert DOM to XML doc (table)
      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
          DOCUMENT      = LO_DOM
          PRETTY_PRINT  = ' '
        IMPORTING
          XML_AS_STRING = W_STRING
          SIZE          = W_SIZE
        TABLES
          XML_AS_TABLE  = IT_XML
        EXCEPTIONS
          NO_DOCUMENT   = 1
          OTHERS        = 2.

      " Set up the file extension filter for the dialog
      " Exclude cl_gui_frontend_services=>filetype_all
      LV_FILE_FILTER_EDMX = CL_GUI_FRONTEND_SERVICES=>FILETYPE_XML.
      REPLACE ALL OCCURRENCES OF 'XML' IN LV_FILE_FILTER_EDMX WITH 'EDMX' RESPECTING CASE.  ##NO_TEXT
      REPLACE ALL OCCURRENCES OF 'xml' IN LV_FILE_FILTER_EDMX WITH 'edmx' RESPECTING CASE.
      IF CL_GUI_FRONTEND_SERVICES=>FILETYPE_XML NE LV_FILE_FILTER_EDMX.
        CONCATENATE CL_GUI_FRONTEND_SERVICES=>FILETYPE_XML LV_FILE_FILTER_EDMX '|' INTO LV_FILE_FILTER.
      ELSE.
        CONCATENATE CL_GUI_FRONTEND_SERVICES=>FILETYPE_XML '|' INTO LV_FILE_FILTER.
      ENDIF.

      LV_DEF_FILE_NAME = XML_KEY.

      " Retrieve the file destination from the user
      CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG(
        EXPORTING
          WINDOW_TITLE         = LC_WINDOW_TITLE
          DEFAULT_EXTENSION    = LV_FILE_FILTER
          DEFAULT_FILE_NAME    = LV_DEF_FILE_NAME
          FILE_FILTER          = LV_FILE_FILTER
        CHANGING
          FILENAME = LV_FILE_NAME
          PATH = LV_PATH
          FULLPATH = LV_FULLPATH
        EXCEPTIONS
          CNTL_ERROR                = 1
          ERROR_NO_GUI              = 2
          NOT_SUPPORTED_BY_GUI      = 3
          INVALID_DEFAULT_FILE_NAME = 4
          OTHERS                    = 5 ).

      " Only download xml if user chose save button
      IF SY-SUBRC IS INITIAL AND
         LV_FILE_NAME IS NOT INITIAL AND
         LV_PATH IS NOT INITIAL .

        " Download xml to the selected destination
        CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD(
          EXPORTING
            FILENAME     = LV_FILE_NAME
            BIN_FILESIZE = LV_SIZE
            FILETYPE     = LC_FILE_TYPE
          CHANGING
            DATA_TAB     = IT_XML
          EXCEPTIONS
            FILE_WRITE_ERROR = 1
            NO_AUTHORITY = 5
            UNKNOWN_ERROR = 6
            ACCESS_DENIED = 15
            OTHERS = 24 ).

        IF SY-SUBRC IS NOT INITIAL.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD SAVE_XML_TO_SCREEN.

    IF IV_DOCTYPE IS INITIAL.

      SELECT SINGLE * INTO @DATA(WA_J_1BNFE_ACTIVE)
        FROM J_1BNFE_ACTIVE
       WHERE DOCNUM EQ @IV_DOCNUM.

      IF SY-SUBRC IS INITIAL.
        CASE WA_J_1BNFE_ACTIVE-MODEL.
            WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.
            IV_DOCTYPE = 'NFE'.
            WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.
            IV_DOCTYPE = 'CTE'.
            WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_MDFE.
            IV_DOCTYPE = 'MFE'.
        ENDCASE.
      ENDIF.

    ENDIF.

    DATA LO_DOM        TYPE REF TO IF_IXML_DOCUMENT.

    "   Load XML from GRC and store on class attributes
    CALL METHOD ME->LOAD_XML_CONTENT
      EXPORTING
        IV_DOCNUM       = IV_DOCNUM
        IV_EVENT_TYPE   = IV_EVENT_TYPE
        IV_EVENT_SEQNUM = IV_EVENT_SEQNUM
        IV_DIRECTION    = IV_DIRECTION
        IV_DOCTYPE      = IV_DOCTYPE.

    IF XML_CONTENT IS NOT INITIAL.

      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          XML           = XML_CONTENT
        IMPORTING
          DOCUMENT      = LO_DOM
        EXCEPTIONS
          INVALID_INPUT = 1
          OTHERS        = 2.

      IF LO_DOM IS NOT INITIAL.
        CALL FUNCTION 'SDIXML_DOM_TO_SCREEN'
          EXPORTING
            DOCUMENT = LO_DOM
          EXCEPTIONS
            OTHERS   = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

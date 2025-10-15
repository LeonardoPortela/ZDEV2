*&---------------------------------------------------------------------*
*& Report ZTESTE_ASSINA_DIGITAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_assina_digital.

DATA: l_title        TYPE string,
      l_filter       TYPE string,
      l_initial      TYPE string,
      l_filename     TYPE string,
      l_msg          TYPE string,
      l_rc           TYPE i,
      l_resp         TYPE char1,
      l_flen         TYPE i,
      t_lines        TYPE STANDARD TABLE OF tline,
      t_otfdata      TYPE tsfotf,
      l_pdf_xtring   TYPE xstring,
      t_file         TYPE filetable,
      w_file         TYPE file_table,
*
      length         TYPE i,
      fullpath       TYPE string,
      lt_doc_content TYPE STANDARD TABLE OF soli-line,
      ls_doc_content LIKE LINE OF lt_doc_content,
      lo_gui         TYPE REF TO cl_gui_frontend_services.

SELECTION-SCREEN BEGIN OF BLOCK 02 WITH FRAME.
  PARAMETERS    : p_url TYPE char255,
                  p_upl AS CHECKBOX,
                  p_dow AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK 02.

START-OF-SELECTION.

  IF p_url IS NOT INITIAL.
    PERFORM f_url.
    RETURN.
  ENDIF.

  l_title   = 'Upload Documento Assinado'.
  l_filter  = 'Files PDF (*.pdf)|*.pdf|'.
* l_initial = 'C:\'.

*-------------------------------
* local do arquivo
*-------------------------------
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    EXPORTING
*      window_title            = l_title
*      file_filter             = l_filter
**     initial_directory       = l_initial
*    CHANGING
*      file_table              = t_file
*      rc                      = l_rc
*    EXCEPTIONS
*      file_open_dialog_failed = 1
*      cntl_error              = 2
*      error_no_gui            = 3
*      not_supported_by_gui    = 4
*      OTHERS                  = 5.
*
*  CHECK t_file[] IS NOT INITIAL.
*
*  READ TABLE t_file INTO w_file INDEX 1.

  IF p_upl = abap_true.
    w_file-filename = 'C:\Jaime\WAYON - AMS\Amaggi\Documento_assinado digital portal BRY.pdf'.

    l_filename = w_file-filename.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = l_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = l_flen
*       header                  = l_pdf_xtring
      CHANGING
        data_tab                = t_otfdata
      EXCEPTIONS
        file_read_error         = 3
        invalid_type            = 4
        no_batch                = 5
        gui_refuse_filetransfer = 7
        OTHERS                  = 99.

    IF 1 = 2.
      CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
        EXPORTING
          i_otf                    = t_otfdata
        EXCEPTIONS
          convert_otf_to_pdf_error = 1
          cntl_error               = 2
          OTHERS                   = 3.
    ENDIF.

*-------------------------------
* convesao PDF
*-------------------------------
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = l_flen
        bin_file              = l_pdf_xtring
      TABLES
        otf                   = t_otfdata[]
        lines                 = t_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    BREAK-POINT.

    UPDATE zsdt0314 SET doc_pdf_assinado = l_pdf_xtring
                  WHERE nr_doc_gerado    = '2672'
                    AND id_doc_agrupador = '000000000000328'.

    COMMIT WORK AND WAIT.
  ENDIF.

  IF p_dow = abap_true.
    SELECT SINGLE *
      FROM zsdt0314
      INTO @DATA(w_314)
     WHERE nr_doc_gerado    = '27'
       AND id_doc_agrupador = '000000000000439'.

    l_pdf_xtring = w_314-doc_pdf_assinado.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer          = l_pdf_xtring
        append_to_table = ' '
      IMPORTING
        output_length   = length
      TABLES
        binary_tab      = lt_doc_content.

    CREATE OBJECT lo_gui.

    fullpath = 'C:\Jaime\WAYON - AMS\Amaggi\DOC_PDF_BRY_SALVO_NO_SAP.pdf'.

    lo_gui->gui_download( EXPORTING
                           filename = fullpath
                           filetype = 'BIN'
                           bin_filesize = length
                         CHANGING
                           data_tab = lt_doc_content ).
  ENDIF.

*-----------------------------------------------------------
* f_url
*-----------------------------------------------------------
FORM f_url.

  SELECT SINGLE *
    FROM zsdt0314
    INTO @DATA(w_314)
   WHERE nr_doc_gerado    = '27'
     AND id_doc_agrupador = '000000000000439'.

  p_url = w_314-url_pdf_assinado.

  zcl_faturamento=>zif_faturamento~get_instance(
                 )->get_data_url( EXPORTING i_filename   = CONV #( p_url )
                                            i_pula_check = abap_false
                                  IMPORTING e_data       = DATA(e_data)
                                            e_len        = DATA(e_len)
                 ).

  BREAK-POINT.

ENDFORM.

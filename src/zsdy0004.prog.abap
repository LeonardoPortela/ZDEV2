*----------------------------------------------------------------------*
*      Print of a invoice by SAPscript SMART FORMS               *
*----------------------------------------------------------------------*

REPORT zsdy0004.

TABLES: j_1acae.

DATA: ck_gerar_pdf    TYPE char01,
      ck_gerar_mail   TYPE char01,
      ck_gerar_dia    TYPE char01,
      lc_diretorio    TYPE zpathwin,
      lc_diretoriou   TYPE zpathunix,
      lc_diretorior   TYPE zde_dir_rede,
      wa_zlest0007    TYPE zlest0007,
      lc_dir          TYPE string,
      lc_arq          TYPE string,
      wa_j_1acae      TYPE j_1acae,
      wa_zfiyt0035    TYPE zfiyt0035,
      it_j_1acae      TYPE STANDARD TABLE OF j_1acae,
      it_zfiyt0035    TYPE TABLE OF zfiyt0035,
      ls_pdf_string_x TYPE xstring,
      v_path          TYPE string.

" Data GERAR PDF QRCODE
DATA: g_html_container TYPE REF TO cl_gui_custom_container,
      g_html_control   TYPE REF TO cl_gui_html_viewer,
      lv_url           TYPE char255,
      lt_data          TYPE STANDARD TABLE OF x255.

DATA: lv_xstring_pdf TYPE xstring,
      lt_doc_content TYPE STANDARD TABLE OF soli-line,
      ls_doc_content LIKE LINE OF lt_doc_content.

DATA: length   TYPE i,
      fullpath TYPE string.


* declaration of data
INCLUDE rlb_invoice_data_declare.
* definition of forms
INCLUDE rlb_invoice_form01.
INCLUDE rlb_print_forms.

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM entry_pdf USING p_factura TYPE vbeln_vf p_dir TYPE string p_file TYPE string CHANGING return_code .

  DATA: lf_retcode   TYPE sy-subrc,
        wa_zsdyt0054 TYPE zsdyt0054,
        lv_form_name TYPE tdsfname.

  SELECT SINGLE * INTO wa_j_1acae
    FROM j_1acae
   WHERE cae_ref EQ p_factura.

  CHECK wa_j_1acae-cae_status EQ 'A'.

  ck_gerar_pdf  = abap_true.
  nast-objky    = p_factura.

  CLEAR retcode.

  SELECT SINGLE * INTO wa_zlest0007
    FROM zlest0007
   WHERE id_interface = '32'
     AND id_ctg       = 'PDF'
     AND prefix       = 'NF'.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e024(sd) WITH 'Falta Config. Tabela ZLEST0007' 'INTERFACE 32' 'CATEGORIA PDF' 'PREFIXO NF'.
    return_code = 1.
  ELSE.
    MOVE: wa_zlest0007-pathwin      TO lc_diretorio ,
          wa_zlest0007-pathunix     TO lc_diretoriou,
          wa_zlest0007-pathwin_rede TO lc_diretorior.

    IF sy-batch IS NOT INITIAL.
      lc_dir = lc_diretoriou.
    ELSE.
      lc_dir = lc_diretorio.
    ENDIF.

    CLEAR: wa_zsdyt0054.


    SELECT SINGLE * INTO wa_zsdyt0054
      FROM zsdyt0054
     WHERE bukrs      EQ wa_j_1acae-bukrs
       AND brnch      EQ wa_j_1acae-brnch
       AND cae_ref    EQ wa_j_1acae-cae_ref
       AND cae_refyr  EQ wa_j_1acae-cae_refyr
       AND cae_reftyp EQ wa_j_1acae-cae_reftyp
       AND budat      EQ wa_j_1acae-budat.

    IF sy-subrc IS NOT INITIAL.
      wa_zsdyt0054-bukrs      = wa_j_1acae-bukrs.
      wa_zsdyt0054-brnch      = wa_j_1acae-brnch.
      wa_zsdyt0054-cae_ref    = wa_j_1acae-cae_ref.
      wa_zsdyt0054-cae_refyr  = wa_j_1acae-cae_refyr.
      wa_zsdyt0054-cae_reftyp = wa_j_1acae-cae_reftyp.
      wa_zsdyt0054-budat      = wa_j_1acae-budat.
    ENDIF.


    CHECK NOT ( ck_gerar_dia EQ abap_true AND wa_zsdyt0054-ck_pdf = abap_true AND wa_zsdyt0054-ck_mail = abap_true ).

    CONCATENATE 'FT_NFE_' p_factura '.pdf' INTO lc_arq.

*** cs2021000135 - código qr afip - inicio
    CLEAR lv_form_name.
    IF wa_j_1acae-budat >= '20210301'.

* Inicio MOD Formulario Factura Fertilizantes venta directa
      IF wa_j_1acae-brnch = '0008' or wa_j_1acae-brnch = '0009'.
        lv_form_name = 'ZSDY0003'.
        PERFORM processing USING space
                                 lv_form_name
                           CHANGING lf_retcode.
      ELSE.
        PERFORM processing USING 'QR_CODE'
                                 lv_form_name
                           CHANGING lf_retcode.
* Fin MOD Formulario Factura Fertilizantes venta directa
      ENDIF .
      IF lf_retcode IS NOT INITIAL.
        MESSAGE s002(zj_1a_ws_cae) DISPLAY LIKE 'E'.
        return_code = 1.
        LEAVE TO LIST-PROCESSING.
      ENDIF .
    ELSE.
*** cs2021000135 - código qr afip - fim
* Inicio MOD Formulario Factura Fertilizantes venta directa
*      PERFORM processing USING '' CHANGING lf_retcode.
      lv_form_name = 'ZSDY0001'.
      PERFORM processing USING space
                               lv_form_name
                         CHANGING lf_retcode.
* Fin MOD Formulario Factura Fertilizantes venta directa
    ENDIF.

    IF lf_retcode NE 0.
      return_code = 1.
    ELSE.
      return_code = 0.
      p_dir  = lc_diretorior.
      p_file = lc_arq.

      wa_zsdyt0054-ck_pdf      = abap_true.
      wa_zsdyt0054-ds_user_pdf = sy-uname.
      wa_zsdyt0054-dt_pdf      = sy-datum.
      wa_zsdyt0054-hr_pdf      = sy-uzeit.
      MODIFY zsdyt0054 FROM wa_zsdyt0054.
      "COMMIT WORK.
    ENDIF.

  ENDIF.

ENDFORM.                    "ENTRY

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM entry USING return_code us_screen.

  DATA: lf_retcode   TYPE sy-subrc,
        wa_zsdyt0054 TYPE zsdyt0054,
        lv_form_name TYPE tdsfname.

  SELECT SINGLE * INTO wa_j_1acae
    FROM j_1acae
   WHERE cae_ref EQ nast-objky(10).

  IF wa_j_1acae-cae_status EQ 'A'.

    ck_gerar_pdf = abap_false.

    CLEAR retcode.
    xscreen = us_screen.

    CLEAR: wa_zsdyt0054.

    SELECT SINGLE * INTO wa_zsdyt0054
      FROM zsdyt0054
     WHERE bukrs      EQ wa_j_1acae-bukrs
       AND brnch      EQ wa_j_1acae-brnch
       AND cae_ref    EQ wa_j_1acae-cae_ref
       AND cae_refyr  EQ wa_j_1acae-cae_refyr
       AND cae_reftyp EQ wa_j_1acae-cae_reftyp
       AND budat      EQ wa_j_1acae-budat.

    IF sy-subrc IS NOT INITIAL.
      wa_zsdyt0054-bukrs      = wa_j_1acae-bukrs.
      wa_zsdyt0054-brnch      = wa_j_1acae-brnch.
      wa_zsdyt0054-cae_ref    = wa_j_1acae-cae_ref.
      wa_zsdyt0054-cae_refyr  = wa_j_1acae-cae_refyr.
      wa_zsdyt0054-cae_reftyp = wa_j_1acae-cae_reftyp.
      wa_zsdyt0054-budat      = wa_j_1acae-budat.
    ENDIF.

*** cs2021000135 - código qr afip - inicio
    IF wa_j_1acae-budat >= '20210301'.
* Inicio MOD Formulario Factura Fertilizantes venta directa
*      PERFORM processing USING 'QR_CODE' CHANGING lf_retcode.
      IF wa_j_1acae-brnch = '0008' or wa_j_1acae-brnch = '0009'.
        lv_form_name = 'ZSDY0003'.
        PERFORM processing USING space
                                 lv_form_name
                           CHANGING lf_retcode.
      ELSE.
        PERFORM processing USING 'QR_CODE'
                                 lv_form_name
                           CHANGING lf_retcode.
* Fin MOD Formulario Factura Fertilizantes venta directa
      ENDIF .
      IF lf_retcode IS NOT INITIAL.
        MESSAGE s002(zj_1a_ws_cae) DISPLAY LIKE 'E'.
        return_code = 1.
        LEAVE TO LIST-PROCESSING.
      ENDIF .
    ELSE.
*** cs2021000135 - código qr afip - fim
* Inicio MOD Formulario Factura Fertilizantes venta directa
*      PERFORM processing USING '' CHANGING lf_retcode.
      lv_form_name = 'ZSDY0001'.
      PERFORM processing USING space
                               lv_form_name
                         CHANGING lf_retcode.
    ENDIF.
* Fin MOD Formulario Factura Fertilizantes venta directa

    IF lf_retcode NE 0.
      return_code = 1.
    ELSE.
      return_code = 0.
      wa_zsdyt0054-ck_print      = abap_true.
      wa_zsdyt0054-ds_user_print = sy-uname.
      wa_zsdyt0054-dt_print      = sy-datum.
      wa_zsdyt0054-hr_print      = sy-uzeit.
      MODIFY zsdyt0054 FROM wa_zsdyt0054.
      "COMMIT WORK.
    ENDIF.

  ELSE.
    MESSAGE s000(zj_1a_ws_cae) WITH nast-objky(10).
    return_code = 1.
  ENDIF.

ENDFORM.                    "ENTRY
*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
FORM processing USING proc_screen
                      pv_form_name TYPE tdsfname
                CHANGING cf_retcode.

  IF proc_screen =  'QR_CODE'.

    SELECT * INTO TABLE it_zfiyt0035
      FROM zfiyt0035
     WHERE bukrs      EQ wa_j_1acae-bukrs
       AND brnch      EQ wa_j_1acae-brnch
       AND cae_ref    EQ wa_j_1acae-cae_ref
       AND cae_refyr  EQ wa_j_1acae-cae_refyr
       AND budat      EQ wa_j_1acae-budat.

    IF it_zfiyt0035 IS NOT INITIAL.

      CLEAR:wa_zfiyt0035.
      READ TABLE it_zfiyt0035 INTO wa_zfiyt0035 INDEX 1.

      IF wa_zfiyt0035-pdf_factura IS NOT INITIAL.
        PERFORM entry_pdf_qr USING wa_zfiyt0035-pdf_factura wa_zfiyt0035-file_name CHANGING sy-subrc.
        IF sy-subrc <> 0.
          cf_retcode = sy-subrc.
        ENDIF.
      ELSE.
        cf_retcode = '4'.
      ENDIF.
    ELSE.
      cf_retcode = '4'.
    ENDIF.

  ELSE.

    DATA: ls_print_data_to_read TYPE lbbil_print_data_to_read.
    DATA: ls_bil_invoice TYPE lbbil_invoice.
    DATA: lf_fm_name            TYPE rs38l_fnam.
    DATA: ls_control_param      TYPE ssfctrlop.
    DATA: ls_composer_param     TYPE ssfcompop.
    DATA: ls_recipient          TYPE swotobjid.
    DATA: ls_sender             TYPE swotobjid.
    DATA: lf_formname           TYPE tdsfname.
    DATA: ls_addr_key           LIKE addr_key.
    DATA: ls_dlv-land           LIKE vbrk-land1.
    DATA: ls_job_info           TYPE ssfcrescl.

* SmartForm from customizing table TNAPR
*    lf_formname = 'ZSDY0001'.
    lf_formname = pv_form_name.
    IF pv_form_name = 'ZSDY0003'.
      ck_gerar_pdf = abap_TRUE.
    ENDIF.

* BEGIN: Country specific extension for Hungary
    DATA: lv_ccnum TYPE idhuccnum,
          lv_error TYPE c.

* If a valid entry exists for the form in customizing view
* IDHUBILLINGOUT then the localized output shall be used.
    SELECT SINGLE ccnum INTO lv_ccnum FROM idhubillingout WHERE
      kschl = nast-kschl.

    IF sy-subrc EQ 0.
      IF lv_ccnum IS INITIAL.
        lv_ccnum = 1.
      ENDIF.

      IF ( nast-delet IS INITIAL OR nast-dimme IS INITIAL ).

        nast-delet = 'X'.
        nast-dimme = 'X'.

        sy-msgid = 'IDFIHU'.
        sy-msgty = 'W'.
        sy-msgno = 201.
        sy-msgv1 = nast-objky.

        CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
          EXPORTING
            msg_arbgb = sy-msgid
            msg_nr    = sy-msgno
            msg_ty    = sy-msgty
            msg_v1    = sy-msgv1
            msg_v2    = ''
            msg_v3    = ''
            msg_v4    = ''
          EXCEPTIONS
            OTHERS    = 1.
      ENDIF.
    ELSE.
      CLEAR lv_ccnum.
    ENDIF.
* END: Country specific extension for Hungary

* determine print data
    PERFORM set_print_data_to_read USING    lf_formname
                                   CHANGING ls_print_data_to_read
                                   cf_retcode.

    IF cf_retcode = 0.
* select print data
      PERFORM get_data USING    ls_print_data_to_read
                       CHANGING ls_addr_key
                                ls_dlv-land
                                ls_bil_invoice
                                cf_retcode.
    ENDIF.

    IF ck_gerar_pdf EQ abap_false.
      IF cf_retcode = 0.
        PERFORM set_print_param USING    ls_addr_key
                                         ls_dlv-land
                                CHANGING ls_control_param
                                         ls_composer_param
                                         ls_recipient
                                         ls_sender
                                         cf_retcode.
      ENDIF.

      IF cf_retcode NE 0.
        ls_composer_param-tdarmod   = '1'.
        "LS_COMPOSER_PARAM-TDNOPRINT = 'X'.
        ls_composer_param-tddest     = 'LP01'.
        ls_composer_param-tdcopies   = '001'.

        ls_control_param-device	   = 'PRINTER'.
        ls_control_param-no_dialog = 'X'.
        ls_control_param-preview   = 'X'.
        ls_control_param-langu     = 'S'.
        cf_retcode = 0.
      ENDIF.
    ENDIF.

    IF cf_retcode = 0.
* determine smartform function module for invoice
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = lf_formname
*         variant            = ' '
*         direct_call        = ' '
        IMPORTING
          fm_name            = lf_fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
*   error handling
        cf_retcode = sy-subrc.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.

    IF cf_retcode = 0.
      PERFORM check_repeat.
      IF ls_composer_param-tdcopies EQ 0.
        nast_anzal = 1.
      ELSE.
        nast_anzal = ls_composer_param-tdcopies.
      ENDIF.
      ls_composer_param-tdcopies = 1.

      DO nast_anzal TIMES.
* In case of repetition only one time archiving
        IF sy-index > 1 AND nast-tdarmod = 3.
          nast_tdarmod = nast-tdarmod.
          nast-tdarmod = 1.
          ls_composer_param-tdarmod = 1.
        ENDIF.
        IF sy-index NE 1 AND repeat IS INITIAL.
          repeat = 'X'.
        ENDIF.
* BEGIN: Country specific extension for Hungary
        IF lv_ccnum IS NOT INITIAL.
          IF nast-repid IS INITIAL.
            nast-repid = 1.
          ELSE.
            nast-repid = nast-repid + 1.
          ENDIF.
          nast-pfld1 = lv_ccnum.
        ENDIF.
* END: Country specific extension for Hungary
* call smartform invoice
        IF ck_gerar_pdf EQ abap_false.
          CALL FUNCTION lf_fm_name
            EXPORTING
              archive_index      = toa_dara
              archive_parameters = arc_params
              control_parameters = ls_control_param
*             mail_appl_obj      =
              mail_recipient     = ls_recipient
              mail_sender        = ls_sender
              output_options     = ls_composer_param
              user_settings      = space
              is_bil_invoice     = ls_bil_invoice
              is_nast            = nast
              is_repeat          = repeat
            IMPORTING
              job_output_info    = ls_job_info
*             document_output_info =
*             job_output_options =
            EXCEPTIONS
              formatting_error   = 1
              internal_error     = 2
              send_error         = 3
              user_canceled      = 4
              OTHERS             = 5.
        ELSE.

          ls_control_param-no_dialog = 'X'.
          ls_control_param-preview   = space.
          ls_control_param-getotf    = 'X'.

          ls_composer_param-tddest   = 'LP01'.

          CALL FUNCTION lf_fm_name
            EXPORTING
              control_parameters = ls_control_param
              output_options     = ls_composer_param
              user_settings      = space
              is_bil_invoice     = ls_bil_invoice
              is_nast            = nast
              is_repeat          = repeat
            IMPORTING
              job_output_info    = ls_job_info
*             document_output_info =
*             job_output_options =
            EXCEPTIONS
              formatting_error   = 1
              internal_error     = 2
              send_error         = 3
              user_canceled      = 4
              OTHERS             = 5.
        ENDIF.

        IF sy-subrc IS NOT INITIAL.
*   error handling
          cf_retcode = sy-subrc.
          PERFORM protocol_update.
* get SmartForm protocoll and store it in the NAST protocoll
          PERFORM add_smfrm_prot.

        ELSE.

          DATA: pdf_tab      LIKE tline OCCURS 0 WITH HEADER LINE,
                wa_lines     TYPE tline,
                bin_filesize TYPE i,
                file_size    TYPE i,
                filename     TYPE string,
                lt_pdf       TYPE TABLE OF char80,
                ls_pdf       TYPE char80.

          IF ck_gerar_pdf EQ abap_true.

            CONCATENATE lc_dir lc_arq INTO filename.

            IF sy-batch IS INITIAL.

              CALL FUNCTION 'CONVERT_OTF'
                EXPORTING
                  format        = 'PDF'
                  max_linewidth = 132
                IMPORTING
                  bin_filesize  = bin_filesize
                TABLES
                  otf           = ls_job_info-otfdata[]
                  lines         = pdf_tab.

              cf_retcode = sy-subrc.

              IF sy-subrc IS INITIAL.

                IF pv_form_name = 'ZSDY0003'.
                  CLEAR filename.

                  CALL METHOD cl_gui_frontend_services=>directory_browse
                    CHANGING
                      selected_folder      = v_path
                    EXCEPTIONS
                      cntl_error           = 1
                      error_no_gui         = 2
                      not_supported_by_gui = 3
                      OTHERS               = 4.

                  IF sy-subrc EQ 0.
                    CONCATENATE 'facturacion' wa_j_1acae-budat+6(2)
                    '-' wa_j_1acae-budat+4(2)'-' wa_j_1acae-budat+0(4) wa_j_1acae-brnch
                    '.pdf' INTO filename.
                    CONCATENATE  v_path  '\' filename INTO filename.
                  ELSE.
                    RETURN.
                  ENDIF.
                ENDIF.


                CALL FUNCTION 'GUI_DOWNLOAD'
                  EXPORTING
                    bin_filesize = bin_filesize
                    filename     = filename
                    filetype     = 'BIN'
                  IMPORTING
                    filelength   = file_size
                  TABLES
                    data_tab     = pdf_tab.

                CALL FUNCTION 'CONVERT_OTF'
                  EXPORTING
                    format        = 'PDF'
                    max_linewidth = 132
                  IMPORTING
                    bin_filesize  = bin_filesize
                    bin_file      = ls_pdf_string_x
                  TABLES
                    otf           = ls_job_info-otfdata[]
                    lines         = pdf_tab.

              ENDIF.

            ELSE.

              CALL FUNCTION 'CONVERT_OTF'
                EXPORTING
                  format        = 'PDF'
                  max_linewidth = 132
                IMPORTING
                  bin_filesize  = bin_filesize
                  bin_file      = ls_pdf_string_x
                TABLES
                  otf           = ls_job_info-otfdata[]
                  lines         = pdf_tab.

              cf_retcode = sy-subrc.

              IF sy-subrc IS INITIAL.
                CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
                  EXPORTING
                    buffer     = ls_pdf_string_x
                  TABLES
                    binary_tab = lt_pdf.

                OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
                IF sy-subrc IS INITIAL.
                  LOOP AT lt_pdf INTO ls_pdf.
                    TRANSFER ls_pdf TO filename NO END OF LINE.
                  ENDLOOP.
                  CLOSE DATASET filename.
                ENDIF.
                cf_retcode = sy-subrc.
              ENDIF.

*              DATA : GD_BUFFER TYPE STRING.
*
*              OPEN DATASET FILENAME FOR OUTPUT IN BINARY MODE.
*
*              "Write data to output file share.
*              LOOP AT PDF_TAB INTO WA_LINES.
*                TRANSLATE WA_LINES USING ' ~'.
*                CONCATENATE GD_BUFFER WA_LINES INTO GD_BUFFER.
*              ENDLOOP.
*
*              TRANSLATE GD_BUFFER USING '~ '.
*              TRANSFER GD_BUFFER TO FILENAME.
*
*              "Close the file
*              CLOSE DATASET FILENAME.

              cf_retcode = sy-subrc.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.

* get SmartForm spoolid and store it in the NAST protocoll
      DATA ls_spoolid LIKE LINE OF ls_job_info-spoolids.
      LOOP AT ls_job_info-spoolids INTO ls_spoolid.
        IF ls_spoolid NE space.
          PERFORM protocol_update_spool USING '342' ls_spoolid
                                              space space space.
        ENDIF.
      ENDLOOP.
      ls_composer_param-tdcopies = nast_anzal.
      IF NOT nast_tdarmod IS INITIAL.
        nast-tdarmod = nast_tdarmod.
        CLEAR nast_tdarmod.
      ENDIF.

    ENDIF.
  ENDIF.
* get SmartForm protocoll and store it in the NAST protocoll
* PERFORM ADD_SMFRM_PROT.

ENDFORM.                    "PROCESSING

"Informações de Documentos Acessórios
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS: pbukrs FOR j_1acae-bukrs,
                  pbrnch FOR j_1acae-brnch,
                  prefyr FOR j_1acae-cae_refyr,
                  pfats  FOR j_1acae-cae_ref,
                  pbudat FOR j_1acae-budat.

  PARAMETERS: ckpdf   AS CHECKBOX DEFAULT sy-batch,
              ckpmail AS CHECKBOX,
              ckftdia AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK mail WITH FRAME TITLE TEXT-001.
  PARAMETERS: pemail TYPE char120.
SELECTION-SCREEN END OF BLOCK mail.

INITIALIZATION.

  GET PARAMETER ID 'VF' FIELD pfats-low.
  IF pfats-low IS NOT INITIAL.
    pfats-sign   = 'I'.
    pfats-option = 'EQ'.
    APPEND pfats.
  ENDIF.


START-OF-SELECTION.

**** CS2021000135 CODIGO QR AFIP - Inicio

*  SELECT * INTO TABLE it_j_1acae
*    FROM j_1acae
*   WHERE bukrs      IN pbukrs
*     AND brnch      IN pbrnch
*     AND cae_refyr  IN prefyr
*     AND cae_ref    IN pfats
*     AND budat      IN pbudat
*     AND cae_status EQ 'A'.
*
*  READ TABLE it_j_1acae INTO wa_j_1acae INDEX 1.
*
*  IF wa_j_1acae-budat >= '20210301'.
*    " Nova Lei QRCODE.
*
*    SELECT * INTO TABLE it_zfiyt0035
*      FROM zfiyt0035
*     WHERE bukrs      IN pbukrs
*       AND brnch      IN pbrnch
*       AND cae_refyr  IN prefyr
*       AND cae_ref    IN pfats
*       AND budat      IN pbudat.
*
*    IF it_zfiyt0035 IS NOT INITIAL.
*
*      CLEAR:wa_zfiyt0035.
*      READ TABLE it_zfiyt0035 INTO wa_zfiyt0035 INDEX 1.
*
*      IF wa_zfiyt0035-pdf_factura IS NOT INITIAL.
*        PERFORM entry_pdf_qr USING wa_zfiyt0035-pdf_factura wa_zfiyt0035-file_name CHANGING sy-subrc.
*      ELSE.
*        MESSAGE e024(sd) WITH 'Arquivo PDF não Encontrado' '' '' ''.
*      ENDIF.
*    ELSE.
*      MESSAGE e024(sd) WITH 'Arquivo PDF não Encontrado' '' '' ''.
*    ENDIF.
*
*  ELSE.

  CHECK nast-objky IS INITIAL.

  DATA: p_dir_in  TYPE string,
        p_file_in TYPE string.

  DATA: lf_retcode TYPE sy-subrc.

  IF ckftdia IS NOT INITIAL.
    CLEAR: pbudat.
    pbudat-sign   = 'I'.
    pbudat-option = 'EQ'.
    pbudat-low    = sy-datum.
    pbudat-high   = sy-datum.
    APPEND pbudat.
  ENDIF.

  SELECT * INTO TABLE it_j_1acae
    FROM j_1acae
   WHERE bukrs      IN pbukrs
     AND brnch      IN pbrnch
     AND cae_refyr  IN prefyr
     AND cae_ref    IN pfats
     AND budat      IN pbudat
     AND cae_status EQ 'A'.

  ck_gerar_mail = ckpmail.
  ck_gerar_dia  = ckftdia.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s001(zj_1a_ws_cae).
  ELSE.
    LOOP AT it_j_1acae INTO wa_j_1acae.

      CLEAR: ls_pdf_string_x.

      CASE abap_true.

        WHEN ckpdf OR ckpmail.

          PERFORM entry_pdf USING wa_j_1acae-cae_ref p_dir_in p_file_in CHANGING sy-subrc.

          IF ckpmail EQ abap_true AND sy-subrc IS INITIAL.
            PERFORM entry_email USING wa_j_1acae-cae_ref p_dir_in p_file_in CHANGING sy-subrc.
          ENDIF.

          IF ckpdf EQ abap_false.
            nast-objky = wa_j_1acae-cae_ref.
            PERFORM entry USING sy-subrc ''.
          ENDIF.

        WHEN OTHERS.
          nast-objky = wa_j_1acae-cae_ref.
          PERFORM entry USING sy-subrc ''.
      ENDCASE.

    ENDLOOP.
  ENDIF.
* ENDIF.
*&---------------------------------------------------------------------*
*&      Form  ENTRY_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN_CODE  text
*      -->P_P_FACTURA  text
*      -->P_P_DIR  text
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM entry_email USING p_factura TYPE vbeln_vf p_dir TYPE string p_file TYPE string CHANGING return_code .

  DATA: wa_zsdyt0054      TYPE zsdyt0054,
        wa_kna1           TYPE kna1,
        i_html_entra      TYPE string,
        mail_destinatario TYPE string,
        wa_adr6           TYPE adr6.

  DATA: lo_create_mail  TYPE REF TO cl_crm_email_data,
        ls_mail_body    TYPE crms_email_mime_struc,
        ls_recep        TYPE crms_email_recipient,
        e_binary_length TYPE i,
        arquivo         TYPE string,
        lv_activity     TYPE sysuuid_x.

  SELECT SINGLE * INTO wa_j_1acae
    FROM j_1acae
   WHERE cae_ref EQ p_factura.

  CHECK wa_j_1acae-cae_status EQ 'A'.

  CLEAR: wa_zsdyt0054.

  SELECT SINGLE * INTO wa_zsdyt0054
    FROM zsdyt0054
   WHERE bukrs      EQ wa_j_1acae-bukrs
     AND brnch      EQ wa_j_1acae-brnch
     AND cae_ref    EQ wa_j_1acae-cae_ref
     AND cae_refyr  EQ wa_j_1acae-cae_refyr
     AND cae_reftyp EQ wa_j_1acae-cae_reftyp
     AND budat      EQ wa_j_1acae-budat.

  IF sy-subrc IS NOT INITIAL.
    wa_zsdyt0054-bukrs      = wa_j_1acae-bukrs.
    wa_zsdyt0054-brnch      = wa_j_1acae-brnch.
    wa_zsdyt0054-cae_ref    = wa_j_1acae-cae_ref.
    wa_zsdyt0054-cae_refyr  = wa_j_1acae-cae_refyr.
    wa_zsdyt0054-cae_reftyp = wa_j_1acae-cae_reftyp.
    wa_zsdyt0054-budat      = wa_j_1acae-budat.
  ENDIF.

  CHECK NOT ( ck_gerar_dia EQ abap_true AND wa_zsdyt0054-ck_pdf = abap_true AND wa_zsdyt0054-ck_mail = abap_true ).

  SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr EQ wa_j_1acae-kunnr.

  CONCATENATE i_html_entra '<html>'  INTO i_html_entra.
  CONCATENATE i_html_entra '<head>'  INTO i_html_entra.
  CONCATENATE i_html_entra '</head>' INTO i_html_entra.
  CONCATENATE i_html_entra '<body>'  INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Estimado cliente:' wa_kna1-name1 '</FONT></DIV>' INTO i_html_entra SEPARATED BY space.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Enviamos informaciones sobre su Factura.</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Este es un e-mail automático, por otras informaciones favor contactar:' INTO i_html_entra.
  CONCATENATE i_html_entra '<a href="mailto:mercadorias@amaggi.com.ar">mercadorias@amaggi.com.ar</a></FONT></DIV>' INTO i_html_entra SEPARATED BY space.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Atentamente</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Amaggi Argentina SA</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.br">www.amaggi.com.br</a></FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.ar">www.amaggi.com.ar</a></FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, piense en su responsabilidad con el MEDIO AMBIENTE!</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, pense em sua responsabilidade e compromisso com o MEIO AMBIENTE! </FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Before printing, think about your responsibility for the ENVIRONMENT!!!</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '</body>'     INTO i_html_entra.
  CONCATENATE i_html_entra '</html>'     INTO i_html_entra.

  mail_destinatario = pemail.

  IF pemail IS INITIAL.
    IF sy-sysid EQ 'PRD'.
      IF wa_kna1-adrnr IS NOT INITIAL.
        SELECT SINGLE * INTO wa_adr6
          FROM adr6
         WHERE addrnumber EQ wa_kna1-adrnr.
        IF sy-subrc IS INITIAL AND wa_adr6-smtp_addr IS NOT INITIAL.
          mail_destinatario = wa_adr6-smtp_addr.
        ENDIF.
      ENDIF.
    ELSE.

      DATA: it_bapiret2   TYPE TABLE OF bapiret2 WITH HEADER LINE,
            it_bapiadsmtp TYPE TABLE OF bapiadsmtp WITH HEADER LINE.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = sy-uname
        TABLES
          return   = it_bapiret2
          addsmtp  = it_bapiadsmtp.

      READ TABLE it_bapiadsmtp INDEX 1.
      IF sy-subrc IS INITIAL AND it_bapiadsmtp-e_mail IS NOT INITIAL.
        mail_destinatario = it_bapiadsmtp-e_mail.
      ENDIF.
    ENDIF.
  ENDIF.

  return_code = 1.
  CHECK mail_destinatario IS NOT INITIAL.

  CREATE OBJECT lo_create_mail.

  CLEAR: lo_create_mail->subject.
  lo_create_mail->subject = 'Factura'.

  CLEAR ls_mail_body.
  ls_mail_body-content_ascii = i_html_entra.
  ls_mail_body-mime_type     = 'text/html'.
  APPEND  ls_mail_body TO lo_create_mail->body.

  CONCATENATE p_dir p_file INTO arquivo.

  CLEAR ls_mail_body.
  ls_mail_body-is_attachment = 'X'.
  ls_mail_body-file_name     = p_file.
  ls_mail_body-mime_type     = 'application/pdf'.
  ls_mail_body-content_bin   = ls_pdf_string_x.
  APPEND  ls_mail_body TO lo_create_mail->body.

  CLEAR ls_recep.
  ls_recep-address = mail_destinatario.
  APPEND ls_recep TO lo_create_mail->to.

  CLEAR ls_recep.
  ls_recep-name    = 'AMAGGI'.
  "LS_RECEP-ADDRESS = 'suporte.desenv@amaggi.com.br'.
  ls_recep-address = 'mercadorias@amaggi.com.ar'.
  MOVE ls_recep TO lo_create_mail->from.

  CALL METHOD cl_crm_email_utility_base=>send_email
    EXPORTING
      iv_mail_data       = lo_create_mail
    RECEIVING
      ev_send_request_id = lv_activity.

  IF sy-subrc IS INITIAL.
    wa_zsdyt0054-ck_mail      = abap_true.
    wa_zsdyt0054-ds_user_mail = sy-uname.
    wa_zsdyt0054-dt_mail      = sy-datum.
    wa_zsdyt0054-hr_mail      = sy-uzeit.
    MODIFY zsdyt0054 FROM wa_zsdyt0054.
    "COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENTRY_PDF_QR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZFIYT0035_PDF_FACTURA  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM entry_pdf_qr  USING    p_pdf_factura
                            p_filename
                   CHANGING p_sy_subrc.


  CLEAR: lv_xstring_pdf, lt_doc_content ,fullpath,length.

  CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
    EXPORTING
      input  = p_pdf_factura
    IMPORTING
      output = lv_xstring_pdf
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer          = lv_xstring_pdf
        append_to_table = ' '
      IMPORTING
        output_length   = length
      TABLES
        binary_tab      = lt_doc_content.

  ENDIF.

  IF sy-batch IS INITIAL.

    DATA: lo_gui TYPE REF TO cl_gui_frontend_services.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      CHANGING
        selected_folder      = v_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc EQ 0.

      CREATE OBJECT lo_gui.

      CONCATENATE v_path '\' p_filename INTO fullpath.

      lo_gui->gui_download( EXPORTING
                             filename = fullpath
                             filetype = 'BIN'
                             bin_filesize = length
                           CHANGING
                             data_tab = lt_doc_content ).
    ENDIF.

  ELSE.

    DATA: filename  TYPE string.

    CONCATENATE lc_dir lc_arq INTO filename.

    OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_doc_content INTO ls_doc_content.
        TRANSFER ls_doc_content TO filename NO END OF LINE.
      ENDLOOP.
      CLOSE DATASET filename.
    ENDIF.

  ENDIF.

*  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD g_html_control->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  CREATE OBJECT g_html_container
    EXPORTING
      container_name = 'PDF'.

  CREATE OBJECT g_html_control
    EXPORTING
      parent = g_html_container.


  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer          = lv_xstring_pdf
      append_to_table = ' '
    IMPORTING
      output_length   = length
    TABLES
      binary_tab      = lt_data. "lt_doc_content.


  CALL METHOD g_html_control->load_data
    EXPORTING
      type                   = 'application'
      subtype                = 'pdf'
    IMPORTING
      assigned_url           = lv_url
    CHANGING
      data_table             = lt_data "lt_doc_content
    EXCEPTIONS
      dp_invalid_parameter   = 1
      dp_error_general       = 2
      cntl_error             = 3
      html_syntax_notcorrect = 4
      OTHERS                 = 5.

  CALL METHOD g_html_control->show_url
    EXPORTING
      url                    = lv_url
      in_place               = 'x'
    EXCEPTIONS
      cntl_error             = 1
      cnht_error_not_allowed = 2
      cnht_error_parameter   = 3
      dp_error_general       = 4
      OTHERS                 = 5.


ENDMODULE.

************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA       : ZFIY0004
* FECHA CREACION : 13/09/2012
* MODULO SAP     : FI
* TITULO         : Certificados de retención de impuestos
* TRANSACCION    : ZFIY0004
* Copia          : rfwtct10
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
REPORT zfiy0004  NO STANDARD PAGE HEADING
                 LINE-SIZE 124
                 LINE-COUNT 65
                 MESSAGE-ID 8a.

PARAMETERS: pcalld TYPE sy-calld NO-DISPLAY.

INCLUDE zfiy0004_top.
INCLUDE zfiy0004_scr.
INCLUDE zfiy0004_form.

INITIALIZATION.

** Se obtiene la ruta inicial del servidor
*  SELECT SINGLE PATHSERVER
*    INTO V_PATH
*    FROM ZBCY_AFIP
*    WHERE REPID EQ SY-REPID.
*
** Se muestra por default la ruta del servidor
*  P_PATH = V_PATH.


*----------------------------------------------------------------------*
*  START-OF-SELECTION                                                  *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  TRANSLATE p_path TO LOWER CASE.

  b0sg-xdocc = 'X'.          "Note 1048950

*----------------------------------------------------------------------*
*  SELECTION                                                           *
*----------------------------------------------------------------------*
GET bkpf.
  CHECK bkpf-stblg IS INITIAL.

* Check for old documents (from 2.1) - insert begin
* perform verify_j2ac(j_1af012) changing sy-subrc.
  PERFORM verify_j2ac CHANGING sy-subrc.
  CHECK sy-subrc EQ 0.
* insert end

  ON CHANGE OF bkpf-bukrs.
    PERFORM read_company_data.
    PERFORM read_currency_texts.
  ENDON.

  CLEAR: xkunnr, xlifnr, xxcpdd.

* ----------------------------- Get bseg ------------------------------
GET bseg.
  CHECK bseg-qsskz NE space.
  CHECK bseg-koart EQ 'K' OR
        bseg-koart EQ 'D'.

  IF bseg-kunnr NE space.
    xkunnr = bseg-kunnr.
  ENDIF.

  CHECK bseg-koart = 'K'.

* --- CPD vendor ?
  IF bseg-xcpdd NE space AND
     xxcpdd     EQ space.

    CALL FUNCTION 'VENDOR_READ'              " P45K039668
      EXPORTING                           " P45K039668
        i_bukrs = space              " P45K039668
        i_lifnr = bseg-lifnr         " P45K039668
      IMPORTING                           " P45K039668
        e_lfa1  = lfa1.              " P45K039668
*         e_lfb1    =
*    exceptions
*         not_found = 1
*         others    = 2.

    IF lfa1-xcpdk NE space.                  " P45K039668
      xxcpdd = 'X'.                          " CPD data have to be read
    ENDIF.                                   " P45K039668
  ENDIF.

*  XLIFNR = BSEG-LIFNR.                       NOTE 337731

  ON CHANGE OF bseg-bschl.
    PERFORM read_posting_key USING bseg-bschl.
  ENDON.

  PERFORM create_paytab.

*-----------------------------------------------------------------------
GET bkpf LATE.
  CHECK bkpf-stblg IS INITIAL.

  PERFORM read_clrd_documents.

  IF xxcpdd IS INITIAL.
    PERFORM read_vendor_data.
  ENDIF.

  PERFORM print_certificate.

  REFRESH: paytab, codtab, clrdtab, xbsec,
           already_selected.

*----------------------------------------------------------------------*
*  END-OF-SELECTION                                                    *
*----------------------------------------------------------------------*

END-OF-SELECTION.

  DATA: lc_oref     TYPE REF TO zcl_memory_variaveis,
        root        TYPE REF TO zcl_memory_variaveis,
        ck_instance TYPE char01.

  DELETE ADJACENT DUPLICATES FROM t_retencion.

  LOOP AT t_retencion INTO st_retencion WHERE retencion > 0.

    REFRESH: t_ret_pos.
    CLEAR  : st_ret_pos,
             st_retencion-retencion.
    PERFORM f_tabla_detalle    USING st_retencion.
    IF t_ret_pos IS INITIAL.
      CONTINUE.
    ELSE.
      st_retencion-bukrs  = xbkpf-bukrs.
      st_retencion-gjahra = xbkpf-belnr.
      st_retencion-augbl  = xbkpf-gjahr.
      PERFORM f_llamo_smartforms_new USING st_retencion.
    ENDIF.
  ENDLOOP.

  IF sy-batch IS NOT INITIAL OR sy-calld IS NOT INITIAL.
    LEAVE PROGRAM.
  ENDIF.

  LEAVE.


*&---------------------------------------------------------------------*
*&      Form  F_LLAMO_SMARTFORMS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_RETENCION  text
*      -->P_CK_INSTANCE  text
*      -->P_LC_OREF  text
*----------------------------------------------------------------------*
FORM f_llamo_smartforms_new  USING  pi_retencion TYPE zfiys_retencion_cab.

*--- Internal tables, Structures and Variables used for PDF conversion
  DATA: it_otf                  TYPE STANDARD TABLE OF itcoo,
        it_docs                 TYPE STANDARD TABLE OF docs,
        it_lines                TYPE STANDARD TABLE OF tline,
        v_lines                 TYPE tline,
        st_job_output_info      TYPE ssfcrescl,
        st_document_output_info TYPE ssfcrespd,
        st_job_output_options   TYPE ssfcresop,
        st_output_options       TYPE ssfcompop,
        st_control_parameters   TYPE ssfctrlop,
        v_len_in                TYPE so_obj_len,
        v_language              TYPE sflangu VALUE 'S',
        v_e_devtype             TYPE rspoptype,
        v_bin_filesize          TYPE i,
        v_name                  TYPE string,
        v_path                  TYPE string,
        v_fullpath              TYPE string,
        v_filter                TYPE string,
        v_uact                  TYPE i,
        v_guiobj                TYPE REF TO cl_gui_frontend_services,
        v_filename              TYPE string,
        v_filename_s            TYPE string,
        v_certificado           TYPE zfiys_retencion_cab-certificado,
        v_endereco              TYPE zparametros-valor,
        v_na_fname              TYPE na_fname.

  DATA: handle TYPE REF TO zcl_memory_variaveis_area,
        root   TYPE REF TO zcl_memory_variaveis.

  DATA: lv_active     TYPE tdbool,
        wgc_smartform TYPE rs38l_fnam,
        ls_control    TYPE ssfctrlop,
        ls_options    TYPE ssfcompop.


  CONSTANTS: c_printer(7) TYPE c VALUE 'PRINTER'.

  DATA : form_name    TYPE fpname,
         gv_fname(30),
         w_copia      TYPE c. "Flag para saber si es duplicado o original.

  CONSTANTS: cs_form  TYPE na_fname VALUE 'ZFIY0002',
             cs_form1 TYPE na_fname VALUE 'ZFIY0002__IB',
             c_x      TYPE c VALUE 'X'.

  CALL FUNCTION 'SSF_STATUS_INFO'
    EXPORTING
      i_formname = cs_form
    IMPORTING
      o_active   = lv_active.

  IF lv_active IS INITIAL.
    STOP.
  ENDIF.

*Inicio Alteração - Leandro Valentim - 11.01.23 - 99745
  IF pi_retencion-witht eq 'CB'.
    v_na_fname = cs_form.
  ELSE.
    v_na_fname = cs_form1.
  ENDIF.
*Fim Alteração - Leandro Valentim - 11.01.23 - 99745

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = v_na_fname
    IMPORTING
      fm_name            = wgc_smartform
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*  Impresora
  ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = p_impr.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.
  ls_options-tdcopies = s_copy.
  ls_options-tddelete = 'X'.
  ls_control-preview  = space.
  ls_control-device   = c_printer.
  ls_control-getotf   = 'X'.

  CLEAR w_copia.
  CLEAR v_fullpath.

  v_certificado = pi_retencion-certificado.
  REPLACE ALL OCCURRENCES OF '-' IN v_certificado WITH ''.

  "CONCATENATE P_PATH 'Ret_' PI_RETENCION-WT_WITHCD '_' PI_RETENCION-WITHT '_' V_CERTIFICADO '_' PI_RETENCION-OPAGO '.pdf' INTO V_FULLPATH.
  CONCATENATE p_path '\Ret_' pi_retencion-wt_withcd '_' pi_retencion-witht '_' v_certificado '_' pi_retencion-opago '.pdf'  INTO v_fullpath.

*Inicio Alteração - Leandro Valentim - 11.01.23 - 99745
  pi_retencion-paval = xt001-stcd1.
*Fim Alteração - Leandro Valentim - 11.01.23 - 99745

*   Se llama al Smartform
  CALL FUNCTION wgc_smartform
    EXPORTING
      user_settings        = ' '
      control_parameters   = ls_control
      output_options       = ls_options
      gv_chk_fecha_imp     = ''
      gs_retencion         = pi_retencion
      v_flag               = w_copia
    IMPORTING
      document_output_info = st_document_output_info
      job_output_info      = st_job_output_info
      job_output_options   = st_job_output_options
    TABLES
      gt_retencion         = t_ret_pos
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty
    NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA: wa_zsdyt0053    TYPE zsdyt0053,
        bin_filesize    TYPE i,
        pdf_tab         LIKE tline OCCURS 0 WITH HEADER LINE,
        ls_pdf_string_x TYPE xstring.

  SELECT SINGLE * INTO wa_zsdyt0053
    FROM zsdyt0053
   WHERE bukrs     EQ xbkpf-bukrs
     AND gjahr     EQ xbkpf-gjahr
     AND augbl     EQ xbkpf-belnr
     AND wt_withcd EQ pi_retencion-wt_withcd
     AND witht     EQ pi_retencion-witht.

  "IF SY-SUBRC IS NOT INITIAL.
  wa_zsdyt0053-bukrs       = xbkpf-bukrs.
  wa_zsdyt0053-gjahr       = xbkpf-gjahr.
  wa_zsdyt0053-augbl       = xbkpf-belnr.
  wa_zsdyt0053-certificado = v_certificado.
  wa_zsdyt0053-wt_withcd   = pi_retencion-wt_withcd.
  wa_zsdyt0053-witht       = pi_retencion-witht.
  wa_zsdyt0053-belnr_o     = pi_retencion-opago.
  wa_zsdyt0053-gjahr_o     = pi_retencion-gjahr.
  "ENDIF.

  CONCATENATE 'Ret_' pi_retencion-wt_withcd '_' pi_retencion-witht '_' v_certificado '_' pi_retencion-opago '.pdf' INTO wa_zsdyt0053-nm_arquivo.

*  CALL FUNCTION 'CONVERT_OTF'
*    EXPORTING
*      FORMAT        = 'PDF'
*      MAX_LINEWIDTH = 132
*    IMPORTING
*      BIN_FILESIZE  = BIN_FILESIZE
*      BIN_FILE      = LS_PDF_STRING_X
*    TABLES
*      OTF           = ST_JOB_OUTPUT_INFO-OTFDATA[]
*      LINES         = PDF_TAB.

  DATA: it_table TYPE zde_tsfotf_base64_t,
        wa_linha TYPE zde_tsfotf_base64.

  LOOP AT st_job_output_info-otfdata[] INTO DATA(wa_otfdata).
    CLEAR: wa_linha.
    wa_linha-tdprintcom = wa_otfdata-tdprintcom.
    wa_linha-tdprintpar = wa_otfdata-tdprintpar.
    wa_linha-tdprintpar = zcl_string=>string_to_base64( wa_linha-tdprintpar ).
    APPEND wa_linha TO it_table.
  ENDLOOP.

  wa_zsdyt0053-ck_print = 'X'.
  wa_zsdyt0053-pdf = zcl_fmcall_handler=>abap2json( EXPORTING abap_data = it_table ).
  MODIFY zsdyt0053 FROM wa_zsdyt0053 .

  CHECK sy-tcode NE 'ZFIY0036'.

  IF rb_prnt EQ 'X' AND sy-batch IS INITIAL AND pcalld IS INITIAL .
    CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
      EXPORTING
        i_otf                    = st_job_output_info-otfdata[]
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
  ENDIF.

  IF p_path IS NOT INITIAL AND sy-batch IS INITIAL.

*--- Convert OTF to PDF
    CALL FUNCTION 'CONVERT_OTF_2_PDF'
      IMPORTING
        bin_filesize           = v_bin_filesize
      TABLES
        otf                    = st_job_output_info-otfdata
        doctab_archive         = it_docs
        lines                  = it_lines
      EXCEPTIONS
        err_conv_not_possible  = 1
        err_otf_mc_noendmarker = 2
        OTHERS                 = 3.

    IF sy-subrc IS INITIAL.

      MOVE v_fullpath TO v_filename.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize            = v_bin_filesize
          filename                = v_filename
          filetype                = 'BIN'
        TABLES
          data_tab                = it_lines
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
          OTHERS                  = 22.
      IF sy-subrc = 0.
*          MESSAGE 'Se han grabado todas las OP' TYPE 'S'.
*        ELSE.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.


  ENDIF.


ENDFORM.

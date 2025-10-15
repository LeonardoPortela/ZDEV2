***********************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0003
* FECHA CREACION: 13/09/2012
* MODULO SAP    : FI
* TITULO        : IMPRESION DE ORDENES DE PAGOS
* TIPO          : Ejecutable - Customizing (F110)
************************************************************************
* MODIFICACIONES
************************************************************************
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************

REPORT  zfiy0003 NO STANDARD PAGE HEADING MESSAGE-ID 8a.

INCLUDE zfiy0003_top.
INCLUDE zfiy0003_scr.
INCLUDE zfiy0003_form.
"INCLUDE ZFIY0003_0001.

*----------------------------------------------------------------------*
*INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.

*  FUNCTXT-ICON_ID   = ICON_IMPORT.
*  FUNCTXT-QUICKINFO = TEXT-001.
*  FUNCTXT-ICON_TEXT = TEXT-002.
*  SSCRFIELDS-FUNCTXT_01 = FUNCTXT.

*  SET TITLEBAR '100'.

* Se obtiene la ruta inicial del servidor
*  SELECT SINGLE PATHSERVER
*    INTO V_PATH
*    FROM ZBCY_AFIP
*    WHERE REPID EQ SY-REPID.

* Se muestra por default la ruta del servidor
*  P_PATH = V_PATH.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN                                                 *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*  CASE SSCRFIELDS-UCOMM.
*    WHEN 'FC01'.
*      CALL SCREEN 0001.
*  ENDCASE.

*----------------------------------------------------------------------*
*  START-OF-SELECTION                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  IF PDODIA IS NOT INITIAL.
*
*  clear it_selscreen.
*  it_selscreen-selname = 'SD_NOOAP'.
*  it_selscreen-kind    = 'P'.
*  it_selscreen-sign    = 'I'.
*  it_selscreen-option  = 'EQ'.
*  it_selscreen-low     = sd_nooap.
*  append it_selscreen.
*
*  ENDIF.

  TRANSLATE p_path TO LOWER CASE.

  s_copy   = s_copia.
  p_script = 'X'.
*----------------------------------------------------------------------*
*  SELECTION                                                           *
*----------------------------------------------------------------------*

GET bkpf.
  PERFORM f_get_bkpf.

GET bseg.
  PERFORM f_get_bseg.
  APPEND bseg TO t_bseg.

GET bkpf LATE.
  PERFORM f_bkpf_late.

*  PERFORM F_SELECIONA_DADOS.
*----------------------------------------------------------------------*
*  END-OF-SELECTION                                                    *
*----------------------------------------------------------------------*

END-OF-SELECTION.

  CLEAR gs_docdata.

*Armo la salida del formulario
  LOOP AT gt_info_data INTO gs_info_data.

    "Seleccionar solo las compensaciones de la fecha actual.
*    IF PDODIA IS NOT INITIAL.
*      IF GS_INFO_DATA-BUDAT NE SY-DATUM.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    IF gs_info_data-augbl(2) NE '20' AND gs_info_data-augbl(2) NE '15'.
      CONTINUE.
    ENDIF.

    REFRESH gt_docdata.
    CLEAR st_empresa.

*    Armo la tabla de posiciones
    LOOP AT gt_docdata_all INTO gs_docdata
         WHERE augbl = gs_info_data-augbl.

      CLEAR: vg_tabix.
      vg_tabix = sy-tabix.

      CLEAR: bkpf, st_pago_pos.
*   Moneda de la factura que cancela la orden de pago
      SELECT SINGLE *
      FROM bkpf
      INTO bkpf
      WHERE belnr EQ gs_docdata-belnr
      AND bukrs   EQ gs_docdata-bukrs
      AND gjahr   EQ gs_docdata-gjahr.


* ----------------------------------------------------
      MOVE: bkpf-waers       TO st_pago_pos-waers,
            bkpf-kursf       TO st_pago_pos-kursf,
            gs_docdata-belnr TO st_pago_pos-belnr,
            gs_docdata-dmbtr TO st_pago_pos-dmbtr,
            gs_docdata-qbshb TO st_pago_pos-wt_qbshh,
            gs_docdata-augbl TO gs_docdata-augbl.

      st_pago_pos-imp_neto = st_pago_pos-wt_qbshh
                           + st_pago_pos-dmbtr.

      IF st_pago_pos-wt_qbshh < 0.
        st_pago_pos-wt_qbshh =  st_pago_pos-wt_qbshh * - 1 .
      ENDIF.

      PERFORM f_busco_xblnro_blart_bldat USING gs_docdata
                                      CHANGING st_pago_pos-xblnr
                                               st_pago_pos-blart
                                               st_pago_pos-bldat.
      PERFORM f_saber_si_es_anticipo USING gs_docdata
                                   CHANGING st_pago_pos-xblnr.

* Modificado - Diego
*      IF st_pago_pos-blart <> 'KZ'. "Modificación DEVELOPER 03/10/2011
*        APPEND st_pago_pos TO t_pago_pos.
*      ENDIF.
      IF st_pago_pos-blart <> 'KZ' AND
         st_pago_pos-blart <> 'ZP'.
        COLLECT st_pago_pos INTO t_pago_pos.
      ENDIF.
* -------------------------------------------------------------------------

* -----------------------------------------------------
* Incluido - Diego
      READ TABLE t_pago_pos INTO st_pago_pos
                            WITH KEY blart = st_pago_pos-blart
                                     xblnr = st_pago_pos-xblnr
                                     belnr = st_pago_pos-belnr
                                     bldat = st_pago_pos-bldat.
      IF sy-subrc IS INITIAL.

* Incluido 16.11.2012
        SELECT SINGLE kurs2 INTO st_pago_pos-kursf
          FROM bkpf
          WHERE bukrs = gs_docdata-bukrs
            AND belnr = gs_docdata-augbl
            AND gjahr = gs_docdata-gjahr.

        MODIFY t_pago_pos FROM st_pago_pos INDEX vg_tabix
                            TRANSPORTING kursf.
* Fin 16.11.2012
* Incluido 16.11.2012

        IF st_pago_pos-kursf IS NOT INITIAL.
* ---> S4 Migration - 16/06/2023 - MA
*          select single WRBTR
*            from BSEG
*            into ST_PAGO_POS-DMBTR
*            where BELNR   eq GS_DOCDATA-AUGBL
*              and BUKRS   eq BKPF-BUKRS
*              and GJAHR   eq BKPF-GJAHR
*              and KOART   eq 'K'.

          DATA: lt_bseg  TYPE fagl_t_bseg,
                lv_belnr TYPE belnr_d.

          lv_belnr = CONV #( gs_docdata-augbl ).

          CALL FUNCTION 'FAGL_GET_BSEG'
            EXPORTING
* RJF - Ini - 2023.10.17 - Pós Golive
*             i_bukrs   = lv_belnr
*             i_belnr   = bkpf-bukrs
              i_bukrs   = bkpf-bukrs
              i_belnr   = lv_belnr
* RJF - Fim - 2023.10.17 - Pós Golive
              i_gjahr   = bkpf-gjahr
            IMPORTING
              et_bseg   = lt_bseg
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

          DELETE lt_bseg WHERE koart NE 'K'.

          READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
          IF sy-subrc = 0.
            st_pago_pos-dmbtr = CONV #( ls_bseg-wrbtr ).
          ENDIF.
*<--- S4 Migration - 16/06/2023 - MA

        ENDIF.
* Fin 16.11.2012
* ---> S4 Migration - 16/06/2023 - MA
*        SELECT SINGLE wrbtr
*          FROM bseg
*          INTO st_pago_pos-wrbtr
*          WHERE belnr   EQ bkpf-belnr
*            AND bukrs   EQ bkpf-bukrs
*            AND gjahr   EQ bkpf-gjahr
*            AND koart   EQ 'K'
*            AND bschl   EQ '31'.

        CALL FUNCTION 'FAGL_GET_BSEG'
          EXPORTING
* RJF - Ini - 2023.10.17 - Pós Golive
*           i_bukrs   = bkpf-belnr
*           i_belnr   = bkpf-bukrs
            i_bukrs   = bkpf-bukrs
            i_belnr   = bkpf-belnr
* RJF - Fim - 2023.10.17 - Pós Golive
            i_gjahr   = bkpf-gjahr
          IMPORTING
            et_bseg   = lt_bseg
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        DELETE lt_bseg WHERE koart NE 'K' AND bschl NE '31'.

        READ TABLE lt_bseg INTO ls_bseg INDEX 1.
        IF sy-subrc = 0.
          MOVE ls_bseg-wrbtr TO st_pago_pos-wrbtr.
        ENDIF.
*<--- S4 Migration - 16/06/2023 - MA

        MODIFY t_pago_pos FROM st_pago_pos INDEX vg_tabix
                          TRANSPORTING dmbtr wrbtr.      " Modificado 16.11.2012

      ENDIF.
* ---------------------------------------------------------------
    ENDLOOP.

    IF gs_docdata IS NOT INITIAL.

      READ TABLE gt_vendor_address INTO gs_vendor_address1
                         WITH KEY augbl =  gs_info_data-augbl.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING gs_vendor_address1 TO gs_vendor_address.
*    EMPRESA  Que emite el formulario
        PERFORM f_empresa_emisora USING    gs_docdata-bukrs
                                           gs_vendor_address.
*    Obtengo el proveedor
        PERFORM obtener_proveedor USING    gs_docdata
                                  CHANGING st_emisor.
      ENDIF.

*    Fecha de la orden de pago
      PERFORM obtener_fecha_y_clase USING gs_docdata
                                 CHANGING st_emisor-budat.
*    Cuenta bancaria
      PERFORM obtener_cta_bancaria USING gs_docdata v_xvorl st_emisor.
*                                  changing ls_augbl.
*    Via de Pago
      IF v_via_de_pago IS INITIAL.
* ---> S4 Migration - 19/06/2023 - MA
*        select single ZLSCH
*          into V_VIA_DE_PAGO
*          from BSEG
*          where BUKRS = GS_DOCDATA-BUKRS
*            and BELNR = GS_DOCDATA-AUGBL
*            and GJAHR = GS_DOCDATA-GJAHR
*            and KOART = 'K'.

        lv_belnr =  CONV #( gs_docdata-augbl ).

        CALL FUNCTION 'FAGL_GET_BSEG'
          EXPORTING
            i_bukrs   = gs_docdata-bukrs
            i_belnr   = gs_docdata-augbl
            i_gjahr   = gs_docdata-gjahr
          IMPORTING
            et_bseg   = lt_bseg
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        DELETE lt_bseg WHERE koart NE 'K'.

        READ TABLE lt_bseg INTO ls_bseg INDEX 1.
        IF sy-subrc = 0.
          MOVE ls_bseg-zlsch TO v_via_de_pago.
        ENDIF.
*<--- S4 Migration - 19/06/2023 - MA
      ENDIF.

*    Cheque
*      PERFORM obtener_cheque USING gs_docdata v_xvorl
*                             CHANGING st_mpagos.

*busco medio de pagos importe y fecha de pago.
      PERFORM f_medios_de_pagos USING gs_docdata-bukrs
                                      gs_docdata-gjahr
                                      gs_docdata-augbl.
* Buscamos ventas de combustible control de cobranza
      SELECT *
      FROM bsas
      INTO TABLE t_bsas
      WHERE bukrs EQ gs_docdata-bukrs
      AND   gjahr EQ gs_docdata-gjahr
      AND   mwskz EQ ' '
      AND   bschl EQ '50'
      AND   augbl EQ gs_docdata-augbl
      AND   hkont <> '0011130003'.

      LOOP AT t_bsas INTO st_bsas.
        CLEAR st_pago_pos.
        MOVE: st_bsas-hkont TO ls_mpagos-ubhkt,
              st_bsas-dmbtr TO ls_mpagos-dmbtr,
              st_bsas-bldat TO ls_mpagos-fecha,
              st_bsas-zuonr TO ls_mpagos-chect.
        CLEAR vl_subrc.
        PERFORM f_validacion_medio_pago USING ls_mpagos CHANGING vl_subrc .

        APPEND ls_mpagos TO t_mpagos.

      ENDLOOP.

      CLEAR v_tabix.

      DELETE t_mpagos WHERE hkont EQ '0008000015' .
      DELETE t_mpagos WHERE ubhkt EQ '0008000015' .

      LOOP AT t_bseg INTO st_header
        WHERE hkont EQ '0008000015'
        AND   belnr EQ gs_docdata-augbl
        AND   bukrs EQ gs_docdata-bukrs.

        MOVE: st_header-hkont TO ls_mpagos-hkont,
              st_header-hkont TO ls_mpagos-ubhkt,
              st_header-dmbtr TO ls_mpagos-dmbtr,
              st_emisor-budat TO ls_mpagos-fecha.
        APPEND  ls_mpagos TO  t_mpagos.
      ENDLOOP.


      LOOP AT t_mpagos INTO ls_mpagos.
        IF ls_mpagos-dmbtr  > 0.
          v_tabix = sy-tabix.

*  Plan de cuenta
          PERFORM f_plan_de_cuenta USING  gs_docdata-bukrs
                                CHANGING  v_ktopl v_spras.

**Descripcion
          IF ls_mpagos-descrip IS INITIAL.
            PERFORM f_descripcion_pago USING v_ktopl
                                             v_spras
                                             ls_mpagos-ubhkt
                                             gs_docdata-augbl
                                             gs_docdata
                                    CHANGING ls_mpagos-descrip.
          ENDIF.

          MODIFY t_mpagos INDEX v_tabix FROM ls_mpagos.

        ELSE.

          DELETE t_mpagos  INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
*      ENDIF.

      LOOP AT t_mpagos INTO ls_mpagos.

        IF ls_mpagos-ubhkt EQ '0080000003' OR ls_mpagos-ubhkt EQ '0080000004'.

          DELETE t_mpagos INDEX sy-tabix.

        ENDIF.

      ENDLOOP.

      PERFORM f_busco_retenciones USING gs_docdata.

      "PERFORM LLAMO_SMARTFORMS.

      PERFORM llamo_smartforms_new.

*   Certificado de retenciones
      IF p_certif EQ 'X'.
        PERFORM f_certificado_retencion USING  gs_docdata .
      ENDIF.

      "PERFORM ENVIAR_EMAIL_DOC USING ST_EMISOR.

      PERFORM enviar_email_doc_new USING st_emisor.

    ENDIF.
    CLEAR:   ls_mpagos    ,
             v_via_de_pago,
             v_xvorl   ,
             v_ktopl ,
             v_spras ,
             gs_docdata,
             t_ret        ,
             t_mpagos     ,
             t_pago_ret   ,
             t_pago_pos   ,
             st_pago      ,
             st_empresa   ,
             st_emisor    ,
             st_pago_pos  ,
             st_pago_ret  ,
             st_mpagos    ,
             st_emisor    .

    REFRESH: t_ret        ,
             t_mpagos     ,
             t_pago_ret   ,
             t_pago_pos   ,
             t_mpagos,
             t_pago_ret,
             t_pago_pos.

  ENDLOOP.

  IF sy-tcode EQ 'ZFIY0036'.
    LEAVE.
  ENDIF.

  IF sy-batch IS NOT INITIAL OR sy-calld IS NOT INITIAL.
    LEAVE PROGRAM.
  ENDIF.


*------------------------------F-I-N------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  LLAMO_SMARTFORMS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM llamo_smartforms_new .


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
        root                    TYPE REF TO zcl_memory_variaveis,
        oref                    TYPE REF TO zcl_memory_variaveis.

  DATA: lv_active     TYPE tdbool,
        wgc_smartform TYPE rs38l_fnam,
        ls_control    TYPE ssfctrlop,
        l_hkont       TYPE hkont,
        ls_options    TYPE ssfcompop.
  DATA:
    v_wrbtr_t    TYPE dmbtr,
    v_dmbtr_t    TYPE dmbtr,
    v_wt_qbshh_t TYPE dmbtr,
    v_imp_neto_t TYPE dmbtr,
    v_endereco   TYPE zparametros-valor.

  CONSTANTS: c_printer(7) TYPE c VALUE 'PRINTER'.

  DATA : form_name    TYPE fpname,
         gv_fname(30).

  CONSTANTS: cs_form TYPE na_fname VALUE 'ZFIY0001',
             c_x     TYPE c VALUE 'X'.

  CALL FUNCTION 'SSF_STATUS_INFO'
    EXPORTING
      i_formname = cs_form
    IMPORTING
      o_active   = lv_active.

  IF lv_active IS INITIAL.
    STOP.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = cs_form
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
  ls_control-no_dialog = 'X'.     "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest    = p_impr.  "'LOCL'.
  ls_options-tdimmed   = c_x.
  ls_options-tdnewid   = c_x.
  ls_options-tdnoarch  = c_x.
  ls_options-tdcopies  = s_copia.
  ls_options-tddelete  = 'X'.
  ls_control-preview   = space.
  ls_control-device    = c_printer.
  ls_control-getotf    = 'X'.

  PERFORM f_logo.
*  Borro la cuenta Puente de los activos fijos son todas
*  las cuentas contables que son  8111014
  LOOP AT t_mpagos INTO st_mpagos.
    IF  st_mpagos-ubhkt EQ '0008111014'.
      DELETE t_mpagos   INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  LOOP AT t_pago_pos INTO st_pago_pos.
    v_wrbtr_t     = v_wrbtr_t    + st_pago_pos-wrbtr.
    v_dmbtr_t     = v_dmbtr_t    + st_pago_pos-dmbtr.
    v_wt_qbshh_t  = v_wt_qbshh_t + st_pago_pos-wt_qbshh.
    v_imp_neto_t  = v_imp_neto_t + st_pago_pos-imp_neto.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM t_pago_pos.

  PERFORM f_borrar_duplicados_t_pago_pos.

  CONCATENATE p_path 'OP_' st_emisor-augbl '_' st_emisor-lifnr '.pdf' INTO v_fullpath.

* Se llama al Smartform
  CALL FUNCTION wgc_smartform
    EXPORTING
      user_settings        = ' '
      control_parameters   = ls_control
      output_options       = ls_options
      gv_chk_fecha_imp     = ''
      gs_emisor            = st_emisor
      gs_sadr              = sadr
      wrbtr_t              = v_wrbtr_t
      dmbtr_t              = v_dmbtr_t
      wt_qbshh_t           = v_wt_qbshh_t
      imp_neto_t           = v_imp_neto_t
    IMPORTING
      document_output_info = st_document_output_info
      job_output_info      = st_job_output_info
      job_output_options   = st_job_output_options
    TABLES
      gt_mpagos            = t_mpagos
      gt_pago_ret          = t_pago_ret
      gt_pago_pos          = t_pago_pos
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.

  CHECK sy-subrc IS INITIAL.

  DATA: wa_zsdyt0052    TYPE zsdyt0052,
        bin_filesize    TYPE i,
        pdf_tab         LIKE tline OCCURS 0 WITH HEADER LINE,
        ls_pdf_string_x TYPE  xstring,
        lt_pdf          TYPE TABLE OF char80,
        ls_pdf          TYPE char80,
        filename        TYPE string.

  SELECT SINGLE * INTO wa_zsdyt0052
    FROM zsdyt0052
   WHERE bukrs EQ st_emisor-bukrs
     AND gjahr EQ st_emisor-gjahr
     AND augbl EQ st_emisor-augbl.

  IF sy-subrc IS NOT INITIAL.
    wa_zsdyt0052-bukrs = st_emisor-bukrs.
    wa_zsdyt0052-gjahr = st_emisor-gjahr.
    wa_zsdyt0052-augbl = st_emisor-augbl.
  ENDIF.

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
*
*  IF SY-SUBRC IS INITIAL.
  wa_zsdyt0052-ck_print = 'X'.
  wa_zsdyt0052-nm_arquivo = v_fullpath.

  DATA: it_table TYPE zde_tsfotf_base64_t,
        wa_linha TYPE zde_tsfotf_base64.

  LOOP AT st_job_output_info-otfdata[] INTO DATA(wa_otfdata).
    CLEAR: wa_linha.
    wa_linha-tdprintcom = wa_otfdata-tdprintcom.
    wa_linha-tdprintpar = wa_otfdata-tdprintpar.
    wa_linha-tdprintpar = zcl_string=>string_to_base64( wa_linha-tdprintpar ).
    APPEND wa_linha TO it_table.
  ENDLOOP.

  wa_zsdyt0052-pdf = zcl_fmcall_handler=>abap2json( EXPORTING abap_data = it_table ).
  MODIFY zsdyt0052 FROM wa_zsdyt0052.
*  ENDIF.

  IF rb_prnt EQ 'X' AND sy-batch IS INITIAL AND sy-calld IS INITIAL.
    CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
      EXPORTING
        i_otf                    = st_job_output_info-otfdata[]
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
  ENDIF.

  IF p_path IS NOT INITIAL AND sy-batch EQ abap_false.

    CONCATENATE p_path '\OP_' st_emisor-augbl '_' st_emisor-lifnr '.pdf' INTO v_fullpath.

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

    ENDIF.


  ENDIF.

  REFRESH: t_mpagos,
           t_pago_ret,
           t_pago_pos.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL_DOC_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_EMISOR  text
*----------------------------------------------------------------------*
FORM enviar_email_doc_new  USING  p_doc TYPE zfiys_orden_pagos_cab.

  DATA: i_html_entra      TYPE string,
        lo_create_mail    TYPE REF TO cl_crm_email_data,
        ls_mail_body      TYPE crms_email_mime_struc,
        ls_recep          TYPE crms_email_recipient,
        lv_activity       TYPE sysuuid_x,
        mail_destinatario TYPE string,
        wa_lifnr          TYPE lfa1,
        wa_adr6           TYPE adr6,
        arquivo           TYPE string,
        ls_pdf_string_x   TYPE xstring,
        e_binary_length   TYPE i.

  DATA: wa_zlest0007 TYPE zlest0007,
        wa_zsdyt0052 TYPE zsdyt0052,
        wa_zsdyt0055 TYPE zsdyt0055,
        it_zsdyt0053 TYPE TABLE OF zsdyt0053 WITH HEADER LINE.

  CLEAR: wa_zlest0007,
         wa_zsdyt0052,
         it_zsdyt0053,
         it_zsdyt0053[].

  CHECK psend IS NOT INITIAL.

  "CHECK ( SY-BATCH IS NOT INITIAL OR SY-TCODE = 'ZFIY0036' OR SY-CALLD IS NOT INITIAL ).

  SELECT SINGLE * INTO wa_zsdyt0052
    FROM zsdyt0052
   WHERE bukrs    EQ p_doc-bukrs
     AND gjahr    EQ p_doc-gjahr
     AND augbl    EQ p_doc-augbl
     AND ck_print EQ 'X'.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_zsdyt0053
    FROM zsdyt0053
   WHERE bukrs    EQ p_doc-bukrs
     AND gjahr_o  EQ p_doc-gjahr
     AND belnr_o  EQ p_doc-augbl
     AND ck_print EQ 'X'.

  DATA: it_table TYPE zde_tsfotf_base64_t.
  /ui2/cl_json=>deserialize( EXPORTING json = wa_zsdyt0052-pdf CHANGING data = it_table ).

  DATA: wa_itcoo TYPE itcoo,
        e_otf	   TYPE tt_itcoo.
  LOOP AT it_table INTO DATA(wa_table).
    CLEAR: wa_itcoo.
    wa_itcoo-tdprintcom = wa_table-tdprintcom.
    wa_itcoo-tdprintpar = zcl_string=>base64_to_string( wa_table-tdprintpar ).
    APPEND wa_itcoo TO e_otf.
  ENDLOOP.

  DATA: i_textos_impostos	TYPE zde_texto_tipo_t,
        w_textos_impostos	TYPE zde_texto_tipo.

  LOOP AT it_zsdyt0053 INTO DATA(wa_zsdyt0053).
    CLEAR: it_table[], w_textos_impostos.
    /ui2/cl_json=>deserialize( EXPORTING json = wa_zsdyt0053-pdf CHANGING data = it_table ).

    LOOP AT it_table INTO wa_table.
      CLEAR: wa_itcoo.
      wa_itcoo-tdprintcom = wa_table-tdprintcom.
      wa_itcoo-tdprintpar = zcl_string=>base64_to_string( wa_table-tdprintpar ).
      APPEND wa_itcoo TO w_textos_impostos-otf.
    ENDLOOP.
    APPEND w_textos_impostos TO i_textos_impostos.
  ENDLOOP.

  LOOP AT i_textos_impostos INTO DATA(wa_textos_impostos).
    CALL FUNCTION 'ZSMARTFORMS_MERGE_OTF'
      EXPORTING
        otf_01     = e_otf
        otf_02     = wa_textos_impostos-otf
      IMPORTING
        otf_result = e_otf.
  ENDLOOP.

  SELECT SINGLE * INTO wa_lifnr FROM lfa1 WHERE lifnr EQ p_doc-lifnr.

  CONCATENATE i_html_entra '<html>'  INTO i_html_entra.
  CONCATENATE i_html_entra '<head>'  INTO i_html_entra.
  CONCATENATE i_html_entra '</head>' INTO i_html_entra.
  CONCATENATE i_html_entra '<body>'  INTO i_html_entra.

  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Estimado cliente:' wa_lifnr-name1 '</FONT></DIV>' INTO i_html_entra SEPARATED BY space.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Enviamos informaciones sobre su Orden de Pago.</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Este es un e-mail automático, por otras informaciones favor contactar:' INTO i_html_entra.
  CONCATENATE i_html_entra '<a href="mailto:cuentas@amaggi.com.ar">administracion@amaggi.com.ar</a></FONT></DIV>' INTO i_html_entra SEPARATED BY space.
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
      IF wa_lifnr-adrnr IS NOT INITIAL.
        SELECT SINGLE * INTO wa_adr6
          FROM adr6
         WHERE addrnumber EQ wa_lifnr-adrnr.
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

  sy-subrc = 1.
  CHECK mail_destinatario IS NOT INITIAL.

  CREATE OBJECT lo_create_mail.

  CLEAR: lo_create_mail->subject.
  lo_create_mail->subject = 'Orden de Pago'.

  CLEAR ls_mail_body.
  ls_mail_body-content_ascii = i_html_entra.
  ls_mail_body-mime_type     = 'text/html'.
  APPEND  ls_mail_body TO lo_create_mail->body.
  " CONCATENATE WA_ZLEST0007-PATHUNIX WA_ZSDYT0052-NM_ARQUIVO INTO ARQUIVO.

  CONCATENATE  wa_zsdyt0052-nm_arquivo '.PDF' INTO arquivo.

  CLEAR ls_mail_body.
  ls_mail_body-is_attachment = 'X'.
  "LS_MAIL_BODY-FILE_NAME     = WA_ZSDYT0052-NM_ARQUIVO.
  ls_mail_body-file_name     = arquivo.
  ls_mail_body-mime_type     = 'application/pdf'.

  DATA: bin_filesize TYPE i,
        pdf_tab      LIKE tline OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format        = 'PDF'
      max_linewidth = 132
    IMPORTING
      bin_filesize  = bin_filesize
      bin_file      = ls_mail_body-content_bin
    TABLES
      otf           = e_otf[]
      lines         = pdf_tab.

  APPEND ls_mail_body TO lo_create_mail->body.

  CLEAR ls_recep.
  ls_recep-address = mail_destinatario.
  APPEND ls_recep TO lo_create_mail->to.
                                                            " 76424
  CLEAR ls_recep.
  ls_recep-address = 'cuentas@amaggi.com.ar'.
  APPEND ls_recep TO lo_create_mail->copy.
                                                            " 76424
**  CLEAR LS_RECEP.
**  LS_RECEP-NAME    = 'AMAGGI'.
**  "LS_RECEP-ADDRESS = 'suporte.desenv@amaggi.com.br'.
**  LS_RECEP-ADDRESS = 'cuentas@amaggi.com.ar'.
**  MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

  "CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>CREATE_REPLY.
  IF ptestee EQ abap_false.
    CALL METHOD cl_crm_email_utility_base=>send_email
      EXPORTING
        iv_mail_data       = lo_create_mail
      RECEIVING
        ev_send_request_id = lv_activity.
  ELSE.
    sy-subrc = 0.
  ENDIF.

  IF sy-subrc IS INITIAL.
    wa_zsdyt0052-ck_mail = abap_true.
    MODIFY zsdyt0052 FROM wa_zsdyt0052.

    MOVE-CORRESPONDING wa_zsdyt0052 TO wa_zsdyt0055.
    wa_zsdyt0055-e_mail    = mail_destinatario.
    wa_zsdyt0055-dt_envio  = sy-datum.
    wa_zsdyt0055-hr_envio  = sy-uzeit.
    wa_zsdyt0055-us_envio  = sy-uname.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZIDOPAGL'
      IMPORTING
        number      = wa_zsdyt0055-id_sequencia.
    MODIFY zsdyt0055 FROM wa_zsdyt0055.

    LOOP AT it_zsdyt0053.
      it_zsdyt0053-ck_mail = abap_true.
      MODIFY zsdyt0053 FROM it_zsdyt0053.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.


ENDFORM.

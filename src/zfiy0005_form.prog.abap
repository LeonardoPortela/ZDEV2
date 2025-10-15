*&---------------------------------------------------------------------*
*&  Include          ZFIY0005_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  form F_POSICION_FORMULARIO
*&---------------------------------------------------------------------*
FORM f_posicion_formulario.

  DATA: st_bseg         TYPE bseg,
        st_retenciones  TYPE with_item,
        t_retenciones   TYPE STANDARD TABLE OF with_item.

  j_1ai02-gjahr = bkpf-gjahr.

  CLEAR:  st_dkadr,
          clrdtab.

  MOVE-CORRESPONDING dkadr TO st_dkadr.
  MOVE bkpf-belnr TO  st_dkadr-augbl.
  APPEND j_1ai02 TO tj_1ai02.
  APPEND st_dkadr TO t_dkadr.

  SELECT *
    FROM with_item
    INTO TABLE t_retenciones
    FOR ALL ENTRIES IN clrdtab
     WHERE bukrs      EQ bkpf-bukrs
     AND   belnr      EQ clrdtab-belnr
     AND   gjahr      EQ bkpf-gjahr
     AND   witht      NE space
     AND   wt_qbshh   NE 0.

  LOOP AT clrdtab.
    READ TABLE t_bseg INTO st_bseg
    WITH KEY belnr = bkpf-belnr
             bukrs = bkpf-bukrs
             gjahr = bkpf-gjahr.

    SELECT SINGLE waers
    FROM bkpf
    INTO bkpf-hwae2
    WHERE  belnr = clrdtab-belnr
    AND    bukrs = st_bseg-bukrs
    AND    gjahr = clrdtab-gjahr.

    MOVE:
         bkpf-belnr      TO st_recibo_pos-augbl,
*         bkpf-kurs2      TO st_recibo_pos-kursf,
         bkpf-kursf      TO st_recibo_pos-kursf,
         bkpf-hwae2      TO st_recibo_pos-hwae2.
    MOVE clrdtab-dmbtr   TO st_recibo_pos-dmbtr.

    IF bkpf-hwae2 NE bkpf-waers.
      IF st_recibo_pos-kursf < 0.
        st_recibo_pos-kursf = st_recibo_pos-kursf * - 1 .
      ENDIF.
      IF st_recibo_pos-kursf IS NOT INITIAL.
        st_recibo_pos-dmbtr = st_recibo_pos-dmbtr / st_recibo_pos-kursf.
      ELSE.
        CLEAR st_recibo_pos-dmbtr.
      ENDIF.
    ENDIF.

    MOVE:
         clrdtab-belnr   TO st_recibo_pos-belnr,
         clrdtab-xblnr   TO st_recibo_pos-xblnr,
         clrdtab-kunnr   TO st_recibo_pos-kunnr,
         clrdtab-buzei   TO st_recibo_pos-buzei,
         j_1ai02-text3   TO st_recibo_pos-detalle,
         bkpf-waers      TO st_recibo_pos-waers,
         clrdtab-wrbtr   TO st_recibo_pos-importe.

*Retenciones
    LOOP AT t_retenciones INTO st_retenciones
         WHERE  belnr      EQ clrdtab-belnr
           AND  wt_qbshh   EQ clrdtab-qbshh.

      IF st_retenciones-wt_qbshh < 0.
        st_retenciones-wt_qbshh = st_retenciones-wt_qbshh * - 1.
      ENDIF.

      IF st_retenciones-witht EQ 'GC'
      OR st_retenciones-witht EQ 'GD'.
*        ADD: st_retenciones-wt_qbshh TO st_recibo_pos-ret_grav.
        st_recibo_pos-ret_grav = st_retenciones-wt_qbshh.
      ENDIF.

      IF st_retenciones-witht EQ 'T0'
      OR st_retenciones-witht EQ 'T1'
      OR st_retenciones-witht EQ 'T2'
      OR st_retenciones-witht EQ 'T3'
      OR st_retenciones-witht EQ 'T4'
      OR st_retenciones-witht EQ 'T5'
      OR st_retenciones-witht EQ 'T6'
      OR st_retenciones-witht EQ 'T7'
      OR st_retenciones-witht EQ 'T8'
      OR st_retenciones-witht EQ 'T9'
      OR st_retenciones-witht EQ 'TA'
      OR st_retenciones-witht EQ 'TB'
      OR st_retenciones-witht EQ 'TC'
      OR st_retenciones-witht EQ 'TD'
      OR st_retenciones-witht EQ 'TE'
      OR st_retenciones-witht EQ 'TF'
      OR st_retenciones-witht EQ 'ID'
      OR st_retenciones-witht EQ 'IE'
      OR st_retenciones-witht EQ 'IF'.
        ADD: st_retenciones-wt_qbshh  TO st_recibo_pos-ret_iibb.
      ENDIF.

      IF st_retenciones-witht EQ 'SC'
      OR st_retenciones-witht EQ 'SE' .
*        ADD: st_retenciones-wt_qbshh  TO st_recibo_pos-ret_suss.
        st_recibo_pos-ret_suss = st_retenciones-wt_qbshh.
      ENDIF.
      IF st_retenciones-witht EQ 'IX' .
*        ADD: st_retenciones-wt_qbshh  TO st_recibo_pos-ret_iva .
        st_recibo_pos-ret_iva = st_retenciones-wt_qbshh.
      ENDIF.

    ENDLOOP.

    APPEND st_recibo_pos TO t_recibo_pos.
    CLEAR: st_recibo_pos.
  ENDLOOP.

  SORT t_dkadr      BY augbl.
  SORT t_recibo_pos BY augbl.

ENDFORM. "F_POSICION_FORMULARIO
*&---------------------------------------------------------------------*
*&  FORM f_llamo_smartforms .
*&---------------------------------------------------------------------*
FORM f_llamo_smartforms .

  DATA:      lv_active      TYPE tdbool,
             wgc_smartform  TYPE rs38l_fnam,
             ls_control     TYPE ssfctrlop,
             vl_copia       TYPE c,
             ls_options     TYPE ssfcompop.

  v_hoja = v_hoja + 1.

  WRITE v_hoja TO vl_copia.

  CONSTANTS: c_printer(7) TYPE c VALUE 'PRINTER'.


  DATA : form_name TYPE fpname,
         gv_fname(30).

  CONSTANTS: cs_form TYPE na_fname VALUE 'ZFIY0003',
             c_x TYPE c VALUE 'X'.

  CALL FUNCTION 'SSF_STATUS_INFO'
    EXPORTING
      i_formname = cs_form
    IMPORTING
      o_active   = lv_active.

  IF lv_active IS INITIAL.
*    MESSAGE i006(z_IEC) .     "Formulario inexistente.
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
* ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.
  ls_options-tdcopies = 1 ." s_copy.

* Imprime o visualiza
  IF rb_view NE 'X'.
    CLEAR ls_control-preview.
    ls_control-device = c_printer.
  ELSE.
    ls_control-preview = 'X'.
  ENDIF.

  CLEAR dkadr.
  MOVE-CORRESPONDING st_dkadr TO dkadr.
  j_1ai02-augbl = st_dkadr-augbl.

*   Se llama al Smartform
  CALL FUNCTION wgc_smartform
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      gv_chk_fecha_imp   = ''
      j_1ai02            = j_1ai02
      gs_sadr            = sadr
      gs_dkadr           = dkadr
      copia              = vl_copia
    TABLES
      t_recibo_pos       = t_recibo_pos
      t_bseg             = t_bsegaux
      t_paytab           = paytab
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty
    NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. " LLAMO_SMARTFORMS

**************End of PDF conversion by C5062443 Dt: 10/2/05*************

*&---------------------------------------------------------------------*
*&      Form  READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_company_data.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ bkpf-bukrs.

  CLEAR: addr1_sel, sadr.
  addr1_sel-addrnumber = t001-adrnr.                        "SADR40A

  CALL FUNCTION 'ADDR_GET'
       EXPORTING
            address_selection = addr1_sel
            address_group     = 'CA01'
*           read_sadr_only    = 'X'
       IMPORTING
            sadr              = sadr.

*****************Start of Conversion by C5062443 Dt: 10/1/05**********

  MOVE: sadr-anred TO gs_company_address-anred,
        sadr-name1 TO gs_company_address-name1,
        sadr-name2 TO gs_company_address-name2,
        sadr-name3 TO gs_company_address-name3,
        sadr-name4 TO gs_company_address-name4,
        sadr-stras TO gs_company_address-stras,
        sadr-pfach TO gs_company_address-pfach,
        sadr-pstl2 TO gs_company_address-pstl2,
        sadr-ort01 TO gs_company_address-ort02,
        sadr-ort02 TO gs_company_address-ort02,
        sadr-pstlz TO gs_company_address-pstlz,
        sadr-land1 TO gs_company_address-land1,
        sadr-regio TO gs_company_address-regio,
        sadr-adrnr TO gs_company_address-adrnr.

*****************End of Converdion by C5062443 Dt: 10/1/05*************

* select single * from sadr where adrnr eq t001-adrnr
*                           and   natio eq space.

  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1ATID'.

  xt001-stcdt = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AIDN'.

  xt001-stcd1 = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AFTV'.

  xt001-fityp = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AGIN'.

  xstcd2 = t001z-paval.
  CALL FUNCTION 'J_1A_PUT_DASHES_TO_STCD2'
    EXPORTING
      i_stcd2 = xstcd2
    IMPORTING
      e_stcd2 = xt001-stcd2.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AFDT'.

  xt001-fnd_date = t001z-paval.
ENDFORM. " READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*&      Form  OPEN_SAPSCRIPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM open_sapscript.
*  itcpo-tdimmed    = '*'.              " kz sofort drucken
*  itcpo-tddelete   = '*'.              " kz freigbe nach Druck
*  itcpo-tdlifetime = '7'.              " verfalltage
*  itcpo-tdpreview  = 'X'.              " druckansicht

*  CALL FUNCTION 'OPEN_FORM'            " open form for output
*       EXPORTING form    = s_form
*                 dialog  = 'X'
*                 OPTIONS = itcpo
*                 device  = 'PRINTER'.
ENDFORM. " OPEN_SAPSCRIPT
*&---------------------------------------------------------------------*
*&      Form  CREATE_PAYTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM create_paytab.
  CLEAR: paytab, paylinetxt, dp_paytab.

*  IF bseg-umskz = 'A'.
  IF bseg-xcpdd NE space AND
     kna1-xcpdk NE space.
    PERFORM read_cpd_data USING bkpf-belnr bseg-buzei bkpf-gjahr.
    MOVE-CORRESPONDING: xbsec TO paytab,
                        xbsec TO paylinetxt.
  ENDIF.

  MOVE-CORRESPONDING: bseg TO paytab,
                      bseg TO paylinetxt.

  CLEAR: paytab-qbshb, paytab-qbshh.

  PERFORM read_withhld_data USING bseg-belnr bseg-gjahr bseg-buzei
                                  paytab-qbshb paytab-qbshh.

*  IF bseg-shkzg = 'S'.
*    paytab-dmbtr = paytab-dmbtr * -1.
*    paytab-wrbtr = paytab-wrbtr * -1.
*    paytab-mwsts = paytab-mwsts * -1.
*    paytab-wmwst = paytab-wmwst * -1.
*  ENDIF.

  IF ( paytab-wrbtr < 0 AND paytab-qbshb < 0 ) OR
     ( paytab-wrbtr > 0 AND paytab-qbshb > 0 ).
    paytab-qbshb = paytab-qbshb * -1.
    paytab-qbshh = paytab-qbshh * -1.
  ENDIF.

*  IF bseg-umsks = 'A' AND bseg-xzahl NE space. MODIFICADO
IF bseg-xzahl NE space.
*down payment
    MOVE-CORRESPONDING paytab TO dp_paytab.
    dp_paytab-buzei = bseg-buzei.
*    COLLECT dp_paytab.
    APPEND dp_paytab.
  ENDIF.

  APPEND paytab.
  APPEND paylinetxt.
*  COLLECT paylinetxt.
*  COLLECT: paytab, paylinetxt.
*  ENDIF.
ENDFORM. " CREATE_PAYTAB
*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_data USING f_belnr f_gjahr f_buzei f_qbshb f_qbshh.
* read the withholding data segment
  SELECT * FROM with_item WHERE bukrs      EQ bkpf-bukrs
                          AND   belnr      EQ f_belnr
                          AND   gjahr      EQ f_gjahr
                          AND   buzei      EQ f_buzei
                          AND   wt_withcd  NE space
                          AND   wt_stat    EQ space
                          AND   wt_gruwtpd EQ space
                          AND   wt_slfwtpd EQ space
                          AND   wt_qbshb   NE 0.

    ADD: with_item-wt_qbshb TO f_qbshb,
         with_item-wt_qbshh TO f_qbshh.

  ENDSELECT.
* New selection introduced by note number 568390.
  SELECT * FROM with_item   WHERE bukrs      EQ bkpf-bukrs
                          AND   belnr      EQ f_belnr
                          AND   gjahr      EQ f_gjahr
                          AND   buzei      EQ f_buzei
                          AND   wt_withcd  NE space
                          AND   wt_stat    EQ space
                          AND   wt_gruwtpd EQ space
                          AND   wt_slfwtpd  NE space
                          AND   wt_qbshb   NE 0.
    MOVE-CORRESPONDING with_item TO wa_with_item. "Note 568390
    INSERT wa_with_item INTO TABLE itab_with_item."note 568390
  ENDSELECT.
ENDFORM. " READ_WITHHLD_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_CPD_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_cpd_data USING f_belnr f_buzei f_gjahr.
  CLEAR xbsec.
  SELECT SINGLE * FROM bsec WHERE bukrs = bkpf-bukrs
                            AND   belnr = f_belnr
                            AND   gjahr = f_gjahr
                            AND   buzei = f_buzei.

  MOVE-CORRESPONDING bsec TO xbsec.

* foreign id?
  IF bsec-land1 NE t001-land1.
    PERFORM read_foreign_id
            USING bsec-land1 bsec-stkzn
            CHANGING xbsec-stcd1.
  ENDIF.

  APPEND xbsec.
ENDFORM. " READ_CPD_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_MASTER_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_master_data.
  CLEAR: kna1, xkna1, xbsec.

  IF xxcpdd NE space.
    LOOP AT xbsec WHERE empfg = clrdtab-empfg.
      EXIT.
    ENDLOOP.
    CHECK 1 = 2.
  ENDIF.

  READ TABLE xkna1 WITH KEY mandt = sy-mandt
                            kunnr = clrdtab-kunnr.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM kna1 WHERE kunnr = clrdtab-kunnr.

  IF NOT kna1-fiskn IS INITIAL.
    SELECT SINGLE * FROM kna1 WHERE kunnr = kna1-fiskn.
  ENDIF.

  MOVE-CORRESPONDING kna1 TO xkna1.

  IF kna1-land1 NE t001-land1.
    PERFORM read_foreign_id
            USING kna1-land1 kna1-stkzn
            CHANGING xkna1-stcd1.
  ENDIF.

  APPEND xkna1.
ENDFORM. " READ_MASTER_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_document_type USING f_blart.
  CHECK xt003-blart NE f_blart.

  CLEAR xt003.

  READ TABLE xt003 WITH KEY f_blart.

  CHECK sy-subrc NE 0.

  CLEAR: t003t.

* read the document type text
  SELECT SINGLE * FROM t003t WHERE spras = sy-langu
                             AND   blart = f_blart.

  MOVE-CORRESPONDING t003t TO xt003.
  APPEND xt003.
ENDFORM. " READ_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*&      Form  START_SAPSCRIPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM start_sapscript.
*  CALL FUNCTION 'START_FORM'
*    EXPORTING
*      form      = s_form
*      startpage = 'PAGE1'.
ENDFORM. " START_SAPSCRIPT
*&---------------------------------------------------------------------*
*&      Form  FILL_GENERAL_FIELDS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_general_fields.
  CLEAR: j_1ai02, bsec, kna1.

* window prt_char
  j_1ai02-prtchr  = xprtchr-j_1aprtchr.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************
  MOVE j_1ai02-prtchr TO gs_info_data-prtchr.
*************End of PDF conversion by C5062443 Dt: 9-2-05*************

* window cc_data1
  PERFORM read_fiscal_vat USING xt001-fityp.

  j_1ai02-text1 = xfitpvt-text60.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************
  MOVE j_1ai02-text1 TO gs_info_data-text1.
*************End of PDF conversion by C5062443 Dt: 9-2-05***************

* window info-1, info-2
  IF s_pdate IS INITIAL.
    j_1ai02-prdate  = bkpf-budat.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************
    MOVE j_1ai02-prdate TO gs_info_data-prdate.
*************End of PDF conversion by C5062443 Dt: 9-2-05***************

  ELSE.
    j_1ai02-prdate  = s_pdate.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************
    MOVE j_1ai02-prdate TO gs_info_data-prdate.
*************End of PDF conversion by C5062443 Dt: 9-2-05***************
  ENDIF.

  j_1ai02-augbl   = bkpf-belnr.
  j_1ai02-isnr    = bkpf-xblnr(4).
  j_1ai02-offnum  = bkpf-xblnr+5(8).
  j_1ai02-todc_1  = xt001-stcdt.
  j_1ai02-stcd1_1 = xt001-stcd1.
  j_1ai02-stcd2_1 = xt001-stcd2.
  j_1ai02-fnddate = xt001-fnd_date.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************

  MOVE: j_1ai02-augbl TO gs_info_data-augbl,
        j_1ai02-isnr TO gs_info_data-isnr,
        j_1ai02-offnum TO gs_info_data-offnum,
        j_1ai02-todc_1 TO gs_info_data-todc_1,
        j_1ai02-stcd1_1 TO gs_info_data-stcd1_1,
        j_1ai02-stcd2_1 TO gs_info_data-stcd2_1,
        j_1ai02-fnddate TO gs_info_data-fnddate.

*************End of PDF conversion by C5062443 Dt: 9-2-05***************

* window c_data1
  CLEAR dkadr.
  IF xxcpdd NE space.
    MOVE-CORRESPONDING xbsec TO dkadr.

    dkadr-inlnd     = t001-land1.
    dkadr-konto     = clrdtab-kunnr.
    j_1ai02-todc_2  = xbsec-stcdt.
    j_1ai02-stcd1_2 = xbsec-stcd1.

******************Start of conversion by C5062443 Dt:10-2-05************

    MOVE: dkadr-anred TO gs_vendor_address1-anred,
          dkadr-name1 TO gs_vendor_address1-name1,
          dkadr-name2 TO gs_vendor_address1-name2,
          dkadr-name3 TO gs_vendor_address1-name3,
          dkadr-name4 TO gs_vendor_address1-name4,
          dkadr-stras TO gs_vendor_address1-stras,
          dkadr-pfach TO gs_vendor_address1-pfach,
          dkadr-pstl2 TO gs_vendor_address1-pstl2,
          dkadr-ort01 TO gs_vendor_address1-ort01,
          dkadr-ort02 TO gs_vendor_address1-ort02,
          dkadr-pstlz TO gs_vendor_address1-pstlz,
          dkadr-land1 TO gs_vendor_address1-land1,
          dkadr-regio TO gs_vendor_address1-regio,
          dkadr-inlnd TO gs_vendor_address1-inlnd,
          dkadr-konto TO gs_vendor_address1-konto.

******************End of conversion by C5062443 Dt:10-2-05**************

    IF NOT xbsec-fityp IS INITIAL.
      PERFORM read_fiscal_vat USING xbsec-fityp.
      j_1ai02-text2 = j_1afitpvt-text60.
    ENDIF.

    CALL FUNCTION 'J_1A_PUT_DASHES_TO_STCD2'
      EXPORTING
        i_stcd2 = xbsec-stcd2
      IMPORTING
        e_stcd2 = j_1ai02-stcd2_2.

    MOVE-CORRESPONDING xbsec TO bsec.
  ELSE.
    MOVE-CORRESPONDING xkna1 TO dkadr.

    dkadr-inlnd     = t001-land1.
    dkadr-konto     = xkna1-kunnr.
    j_1ai02-todc_2  = xkna1-stcdt.
    j_1ai02-stcd1_2 = xkna1-stcd1.

******************Start of conversion by C5062443 Dt:10-2-05************

    MOVE: dkadr-anred TO gs_vendor_address1-anred,
          dkadr-name1 TO gs_vendor_address1-name1,
          dkadr-name2 TO gs_vendor_address1-name2,
          dkadr-name3 TO gs_vendor_address1-name3,
          dkadr-name4 TO gs_vendor_address1-name4,
          dkadr-stras TO gs_vendor_address1-stras,
          dkadr-pfach TO gs_vendor_address1-pfach,
          dkadr-pstl2 TO gs_vendor_address1-pstl2,
          dkadr-ort01 TO gs_vendor_address1-ort01,
          dkadr-ort02 TO gs_vendor_address1-ort02,
          dkadr-pstlz TO gs_vendor_address1-pstlz,
          dkadr-land1 TO gs_vendor_address1-land1,
          dkadr-regio TO gs_vendor_address1-regio,
          dkadr-inlnd TO gs_vendor_address1-inlnd,
          dkadr-konto TO gs_vendor_address1-konto,
          j_1ai02-augbl TO gs_vendor_address1-augbl.

    APPEND gs_vendor_address1 TO gt_vendor_address.

******************End of conversion by C5062443 Dt:10-2-05**************

    IF NOT xkna1-fityp IS INITIAL.
      PERFORM read_fiscal_vat USING xkna1-fityp.
      j_1ai02-text2 = j_1afitpvt-text60.
    ENDIF.

    CALL FUNCTION 'J_1A_PUT_DASHES_TO_STCD2'
      EXPORTING
        i_stcd2 = xkna1-stcd2
      IMPORTING
        e_stcd2 = j_1ai02-stcd2_2.

    MOVE-CORRESPONDING xkna1 TO kna1.
  ENDIF.

* window copy_no
  j_1ai02-copyno  = copy_no.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************
  MOVE j_1ai02-copyno TO gs_info_data-copyno.
*************End of PDF conversion by C5062443 Dt: 9-2-05***************

* window text1
  j_1ai02-budat2  = bkpf-budat.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************

  MOVE j_1ai02-budat2 TO gs_info_data-budat2.
  MOVE bkpf-belnr TO gs_info_data-belnr.

*************End of PDF conversion by C5062443 Dt: 9-2-05***************

ENDFORM. " FILL_GENERAL_FIELDS
*&---------------------------------------------------------------------*
*&      Form  READ_FISCAL_VAT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_fiscal_vat USING f_fiscal_vat.
  CLEAR: j_1afitpvt, xfitpvt.

  READ TABLE xfitpvt WITH KEY mandt    = sy-mandt
                              spras    = sy-langu
                             j_1afitp = f_fiscal_vat.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM j_1afitpvt WHERE spras    = sy-langu
                                 AND   j_1afitp = f_fiscal_vat.

  CHECK sy-subrc EQ 0.

  MOVE-CORRESPONDING j_1afitpvt TO xfitpvt.
  APPEND xfitpvt.
ENDFORM. " READ_FISCAL_VAT
*&---------------------------------------------------------------------*
*&      Form  PRINT_CLRDTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_clrdtab.
* read clreared documents

  LOOP AT clrdtab.
    AT NEW kunnr.
      PERFORM read_master_data.

*************Start of PDF conversion by C5062443 Dt: 9-2-05*************
      IF NOT p_ss_pdf IS INITIAL.
        PERFORM start_sapscript.
      ENDIF.
*************End of PDF conversion by C5062443 Dt: 9-2-05***************

      PERFORM fill_general_fields.

* call customer-function '003'
      CALL FUNCTION 'J_1A_EXIT_J_1AF011'
        EXPORTING
          i_bkpf    = bkpf
          i_sadr    = sadr
          i_dkadr   = dkadr
          i_kna1    = kna1
          i_bsec    = bsec
          i_j_1ai02 = j_1ai02
        IMPORTING
          e_j_1ai02 = j_1ai02
          e_sadr    = sadr
          e_dkadr   = dkadr.
      MOVE bkpf-bukrs TO j_1ai02-bukrs.
      IF NOT copy_no IS INITIAL.
        IF NOT p_ss_pdf IS INITIAL.
*          CALL FUNCTION 'WRITE_FORM'
*            EXPORTING
*              window  = 'COPY_NO'
*              element = 'LINE'.
        ENDIF.
      ENDIF.

***************Start of conversion by C5062443 Dt: 10/2/05**************
      MOVE xprtchr-j_1adisvat TO gv_vat_number.
***************End of conversion by C5062443 Dt: 10/2/05****************

      IF NOT xprtchr-j_1adisvat IS INITIAL AND
         xumsks                 NE space.
        IF NOT p_ss_pdf IS INITIAL.
*          CALL FUNCTION 'WRITE_FORM'
*            EXPORTING
*              window  = 'HEADER'
*              element = 'NET'.
        ENDIF.
      ELSE.
        IF NOT p_ss_pdf IS INITIAL.

*          CALL FUNCTION 'WRITE_FORM'
*            EXPORTING
*              window  = 'HEADER'
*              element = 'GROSS'.
        ENDIF.

      ENDIF.

      SUM.
* window text1
      j_1ai02-text4   = bkpf-waers.

*Here is the check if there is self withholding involved and display
*amount calculated accordingly.                         Note 568390
      READ TABLE itab_with_item INTO wa_with_item  WITH KEY
                                     belnr = j_1ai02-augbl.
      IF sy-subrc = 0.
        payed_amnt = xwrbtr = clrdtab-wrbtr + clrdtab-wmwst.
      ELSE.
        payed_amnt = xwrbtr = clrdtab-wrbtr + clrdtab-wmwst +
                             clrdtab-qbshb.
      ENDIF.                                            " Note 568390

      WRITE xwrbtr CURRENCY bkpf-waers TO j_1ai02-text4+10.
      CONDENSE j_1ai02-text4.
    ENDAT.

************Start of Conversion by C5062443 Dt:10/2/05****************

    MOVE: j_1ai02-text4 TO gs_info_data-text4,
          j_1ai02-stcd1_2 TO gs_info_data-stcd1_2,
          j_1ai02-stcd2_2 TO gs_info_data-stcd2_2,
          j_1ai02-text2 TO gs_info_data-text2.

    APPEND gs_info_data TO gt_info_data.

************End of Conversion by C5062443 Dt:10/2/05*******************
    AT NEW blart.
      PERFORM read_document_type USING clrdtab-blart.
    ENDAT.

    j_1ai02-isnr2   = clrdtab-xblnr(4).
    j_1ai02-offnum2 = clrdtab-xblnr+5(8).
    j_1ai02-belnr   = clrdtab-belnr.
    j_1ai02-budat   = clrdtab-budat.
    j_1ai02-text3   = xt003-ltext.
    j_1ai02-fwste   = clrdtab-wmwst.
    j_1ai02-qbshb   = clrdtab-qbshb.
    j_1ai02-wrbtr   = clrdtab-wrbtr.
    j_1ai02-waers   = bkpf-waers.

**************Start of Conversion by C5062443 Dt:19/1/05****************

    MOVE: j_1ai02-isnr2 TO gs_docdata-isnr2,
          j_1ai02-offnum2 TO gs_docdata-offnum2,
          j_1ai02-belnr TO gs_docdata-belnr,
          j_1ai02-budat TO gs_docdata-budat,
          j_1ai02-text3 TO gs_docdata-text3,
          j_1ai02-fwste TO gs_docdata-fwste,
          j_1ai02-qbshb TO gs_docdata-qbshb,
          j_1ai02-wrbtr TO gs_docdata-wrbtr,
          j_1ai02-waers TO gs_docdata-waers,
          j_1ai02-augbl TO gs_docdata-augbl,
          t001-waers TO gv_compcurrency,

          j_1ai02-qbshb TO gs_total-qbshb,
          j_1ai02-wrbtr TO gs_total-wrbtr,
          j_1ai02-waers TO gs_total-waers.

********************End of conversion by C5062443 Dt: 19-1-05***********

    IF clrdtab-qbshb IS INITIAL.
      CLEAR j_1ai02-pdata.
    ELSE.
      j_1ai02-pdata = 'X'.
    ENDIF.

    IF NOT xprtchr-j_1adisvat IS INITIAL AND
       xumsks                 NE space.
      PERFORM print_net.
    ELSE.
      PERFORM print_gross.
    ENDIF.

    AT END OF kunnr.
      SUM.
      CLEAR j_1ai02-waers2.

      j_1ai02-qbshb = clrdtab-qbshb.
      j_1ai02-wrbtr = clrdtab-wrbtr + clrdtab-wmwst.
      j_1ai02-waers = bkpf-waers.

**************Start of Conversion by C5062443 Dt:10/2/05****************

      MOVE: j_1ai02-qbshb TO gs_docdata-qbshb,
            j_1ai02-wrbtr TO gs_docdata-wrbtr,
            j_1ai02-waers TO gs_docdata-waers,

            j_1ai02-qbshb TO gs_total-qbshb,
            j_1ai02-wrbtr TO gs_total-wrbtr,
            j_1ai02-waers TO gs_total-waers.

************End of Conversion by C5062443 Dt:10/2/05****************

      IF t001-waers NE bkpf-waers.
        j_1ai02-qbshh = clrdtab-qbshh.
        j_1ai02-dmbtr = clrdtab-dmbtr + clrdtab-mwsts.
        j_1ai02-waers2 = t001-waers.

************Start of Conversion by C5062443 Dt:10/2/05****************

        MOVE: j_1ai02-qbshh TO gs_docdata-qbshh,
              j_1ai02-dmbtr  TO gs_docdata-dmbtr,
              j_1ai02-waers2 TO gs_docdata-waers2,

              j_1ai02-qbshh TO gs_total-qbshh,
              j_1ai02-dmbtr  TO gs_total-dmbtr,
              j_1ai02-waers2 TO gs_total-waers2.

************End of Conversion by C5062443 Dt:10/2/05********************
      ENDIF.

      MOVE j_1ai02-augbl TO gs_total-augbl.
      APPEND gs_total TO gt_total.

************Start of Conversion by C5062443 Dt:10/2/05****************
      IF NOT p_ss_pdf IS INITIAL.
*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'TOTAL'.
      ENDIF.

************End of Conversion by C5062443 Dt:10/2/05********************
      PERFORM print_paylinetab.
************Start of Conversion by C5062443 Dt:10/2/05****************

      IF NOT p_ss_pdf IS INITIAL.
*        CALL FUNCTION 'END_FORM'.
      ENDIF.

************End of Conversion by C5062443 Dt:10/2/05********************
    ENDAT.
  ENDLOOP.
ENDFORM. " PRINT_CLRDTAB
*&---------------------------------------------------------------------*
*&      Form  PRINT_GROSS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_gross.

  j_1ai02-wrbtr = clrdtab-wrbtr + clrdtab-wmwst.

***************Start of conversion by C5062443 Dt: 10/2/05*************
  MOVE j_1ai02-wrbtr TO gs_docdata-wrbtr.
***************End of conversion by C5062443 Dt: 10/2/05*************
  IF t001-waers = bkpf-waers.
***************Start of conversion by C5062443 Dt: 10/2/05*************
    IF NOT p_ss_pdf IS INITIAL.
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          element = 'GROSS_DC'.
    ENDIF.
***************End of conversion by C5062443 Dt: 10/2/05***************
  ELSE.
    j_1ai02-qbshh  = clrdtab-qbshh.
    j_1ai02-dmbtr  = clrdtab-dmbtr + clrdtab-mwsts.
    j_1ai02-waers2 = t001-waers.
**************Start of conversion by C5062443 Dt: 24-1-05**************
    MOVE: j_1ai02-qbshh TO gs_docdata-qbshh,
          j_1ai02-dmbtr TO gs_docdata-dmbtr,
          j_1ai02-waers2 TO gs_docdata-waers2,

          j_1ai02-qbshh TO gs_total-qbshh,
          j_1ai02-dmbtr TO gs_total-dmbtr,
          j_1ai02-waers2 TO gs_total-waers2.
***************End of conversion by C5062443 Dt: 24-1-05**************

***************Start of conversion by C5062443 Dt: 10/2/05*************
    IF NOT p_ss_pdf IS INITIAL.
***************End of conversion by C5062443 Dt: 10/2/05***************
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          element = 'GROSS_LC'.
    ENDIF.
  ENDIF.

***************Start of conversion by C5062443 Dt: 10/2/05*************
  IF gv_copy_flag = space.
    APPEND gs_docdata TO gt_docdata_all.
  ENDIF.

***************End of conversion by C5062443 Dt: 10/2/05***************
ENDFORM. " PRINT_GROSS
*&---------------------------------------------------------------------*
*&      Form  PRINT_NET
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_net.

***************Start of Conversion by C5062443 Dt:10/2/05***************
  IF NOT p_ss_pdf IS INITIAL.
***************End of Conversion by C5062443 Dt:10/2/05*****************

*    CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*        element = 'NET_DC'.
  ENDIF.

***************Start of Conversion by C5062443 Dt:10/2/05***************
  MOVE: j_1ai02-hwste TO gs_docdata-hwste,
        j_1ai02-qbshh TO gs_docdata-qbshh,
        j_1ai02-dmbtr TO gs_docdata-dmbtr,
        j_1ai02-waers TO gs_docdata-waers,
        j_1ai02-waers TO gs_docdata-waers2,

        j_1ai02-qbshh TO gs_total-qbshh,
        j_1ai02-dmbtr TO gs_total-dmbtr,
        j_1ai02-waers TO gs_total-waers.

  IF t001-waers = bkpf-waers.
    IF gv_copy_flag = space.
      APPEND gs_docdata TO gt_docdata_all.
    ENDIF.
  ENDIF.

***************End of Conversion by C5062443 Dt:10/2/05*****************

  CHECK t001-waers NE bkpf-waers.

  j_1ai02-hwste = clrdtab-mwsts.
  j_1ai02-qbshh = clrdtab-qbshh.
  j_1ai02-dmbtr = clrdtab-dmbtr.
  j_1ai02-waers = t001-waers.
***************Start of Conversion by C5062443 Dt:10/2/05***************
  IF t001-waers NE bkpf-waers.
    MOVE: j_1ai02-hwste TO gs_docdata-hwste,
          j_1ai02-qbshh TO gs_docdata-qbshh,
          j_1ai02-dmbtr TO gs_docdata-dmbtr,
          j_1ai02-waers TO gs_docdata-waers2.
    IF gv_copy_flag = space.
      APPEND gs_docdata TO gt_docdata_all.
    ENDIF.
  ENDIF.
***************End of Conversion by C5062443 Dt:10/2/05***************
***************Start of Conversion by C5062443 Dt:10/2/05***************
  IF NOT p_ss_pdf IS INITIAL.
***************End of Conversion by C5062443 Dt:10/2/05***************
*    CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*        element = 'NET_LC'.
  ENDIF.
***************End of Conversion by C5062443 Dt:10/2/05*****************
ENDFORM. " PRINT_NET

*&---------------------------------------------------------------------*
*&      Form  PRINT_PAYLINETAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_paylinetab.
  LOOP AT paylinetxt INTO p_paylinetxt WHERE kunnr = clrdtab-kunnr.
    APPEND p_paylinetxt.
  ENDLOOP.

  LOOP AT p_paylinetxt.
    AT NEW kunnr.

***************Start of Conversion by C5062443 Dt:10/2/05***************
      IF NOT p_ss_pdf IS INITIAL.
*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'PAYMENT_LINES_HD'.
      ENDIF.
    ENDAT.

***************End of Conversion by C5062443 Dt:10/2/05*****************
    j_1ai02-sgtxt = p_paylinetxt-sgtxt.

***************Start of Conversion by C5062443 Dt:10/2/05***************
    MOVE j_1ai02-sgtxt TO gs_payline-sgtxt.
***************End of Conversion by C5062443 Dt:10/2/05*****************

***************Start of Conversion by C5062443 Dt:10/2/05***************
    IF NOT p_ss_pdf IS INITIAL.
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          element = 'PAYMENT_LINES'.
    ENDIF.
***************End of Conversion by C5062443 Dt:10/2/05*****************
    AT END OF kunnr.
      j_1ai02-wrbtr = payed_amnt.
      j_1ai02-waers = bkpf-waers.

***************Start of Conversion by C5062443 Dt:10/2/05***************
      MOVE: j_1ai02-wrbtr TO gs_payline-wrbtr,
            j_1ai02-waers TO gs_payline-waers,
            j_1ai02-augbl TO gs_payline-augbl.

***************End of Conversion by C5062443 Dt:10/2/05*****************

***************Start of Conversion by C5062443 Dt:10/2/05***************
      IF NOT p_ss_pdf IS INITIAL.
*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'PAYMENT_LINES_TOTAL'.
      ENDIF.
***************End of Conversion by C5062443 Dt:10/2/05*****************
    ENDAT.
***************Start of Conversion by C5062443 Dt:10/2/05***************
    APPEND gs_payline TO gt_payline.
***************End of Conversion by C5062443 Dt:10/2/05*****************

  ENDLOOP.

  REFRESH p_paylinetxt.
ENDFORM. " PRINT_PAYLINETAB
*&---------------------------------------------------------------------*
*&      Form  READ_CLRD_DOCUMENTS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_clrd_documents.
  CLEAR dp_paymnt.

  SORT paytab BY augbl augdt.             " Note 138001

  LOOP AT paytab.
*    IF paytab-augbl NE bkpf-belnr.  MODIFICADO
    IF paytab-augbl EQ bkpf-belnr.
      IF xlifnr NE space.
        CLEAR: xbsak, xbsad.
        REFRESH: xbsak, xbsad.
        SELECT * FROM bsak INTO TABLE xbsak WHERE bukrs EQ bkpf-bukrs
                           AND   lifnr EQ xlifnr
                           AND   augdt EQ paytab-augdt
                           AND   augbl EQ bkpf-belnr
                           AND   belnr NE bkpf-belnr.

        LOOP AT xbsak.
          MOVE-CORRESPONDING xbsak TO xbsad.
          xbsad-kunnr = xbsak-lifnr.
          PERFORM create_clrdtab.
        ENDLOOP.

        IF sy-subrc = 0.
          CLEAR xlifnr.
        ENDIF.
      ENDIF.

      IF paytab-umsks = 'A' AND
         paytab-xzahl NE space.
        dp_paymnt = 'X'.
      ENDIF.

      AT NEW augbl.                         " Note 138001
        CLEAR: no_clrd_docm, xbsad.
        REFRESH: xbsad.

        IF paytab-augbl EQ bkpf-belnr.      " Note 138001
          IF paytab-umsks = 'A' AND
             dp_paymnt    NE space.
            IF dp_paymnt    NE space.
              CLEAR dp_paymnt.
              paytab-augdt = bkpf-budat.
            ENDIF.

            SELECT * FROM bsad INTO TABLE xbsad WHERE bukrs EQ bkpf-bukrs
                                                AND   kunnr EQ paytab-kunnr
                                                AND   umsks EQ paytab-umsks
                                                AND   augdt EQ paytab-augdt
                                                AND   augbl EQ bkpf-belnr
                                                AND   belnr NE bkpf-belnr.

            LOOP AT xbsad.
              PERFORM create_clrdtab.
            ENDLOOP.

            IF sy-subrc NE 0.
              no_clrd_docm = 'X'.
            ENDIF.
          ELSE.                               " Note 138001
            no_clrd_docm = 'X'.               " Note 138001
          ENDIF.                              " Note 138001
        ENDIF.
      ENDAT.


      CHECK: ( no_clrd_docm   NE space AND
             paytab-xzahl NE space ) OR ( paytab-umsks = 'A' AND
             NOT paytab-rebzg IS INITIAL ).

      IF NOT paytab-rebzg IS INITIAL.
        PERFORM read_document_header
                USING paytab-rebzg paytab-rebzj.

        MOVE-CORRESPONDING xbkpf TO clrdtab.
      ELSE.
        MOVE-CORRESPONDING bkpf TO clrdtab.
      ENDIF.

      MOVE-CORRESPONDING paytab TO clrdtab.

*      COLLECT clrdtab.
      APPEND clrdtab.
    ENDIF.
  ENDLOOP.
ENDFORM. " READ_CLRD_DOCUMENTS
*&---------------------------------------------------------------------*
*&      Form  CHECK_CURRENCIES
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM check_currencies.
  IF t001-waers EQ bkpf-waers.
* payment is in lc
    IF xbsad-waers NE bkpf-waers.
* cleared document is created in fc
* the withhld.amnt. of the clrd. document is already converted
      clrdtab-qbshb = clrdtab-qbshh.

      PERFORM convert_into_local_curr USING
         clrdtab-wrbtr clrdtab-wrbtr xbsad-waers bkpf-waers '0'.
      PERFORM convert_into_local_curr USING
         clrdtab-wmwst clrdtab-wmwst xbsad-waers bkpf-waers '0'.
    ENDIF.
  ELSEIF xbsad-waers EQ bkpf-waers.
* payment is in fc and cleared document is in the same fc
    PERFORM convert_into_local_curr USING
       clrdtab-wrbtr clrdtab-dmbtr xbsad-waers t001-waers bkpf-kursf.
    PERFORM convert_into_local_curr USING
       clrdtab-wmwst clrdtab-mwsts xbsad-waers t001-waers bkpf-kursf.
*    perform convert_into_local_curr using
*       clrdtab-qbshb clrdtab-qbshh bsad-waers t001-waers bkpf-kursf.
  ELSE.
    IF xbsad-waers NE t001-waers.
* payment is in fc and cleared document is in other fc
* so convert at first into lc
* the withhld.amnt. of the clrd. document is already converted
      PERFORM convert_into_local_curr USING
         clrdtab-wrbtr clrdtab-dmbtr xbsad-waers t001-waers '0'.
      PERFORM convert_into_local_curr USING
         clrdtab-wmwst clrdtab-mwsts xbsad-waers t001-waers '0'.
* convert now into foreign curr.
      PERFORM convert_into_foreign_curr USING
         clrdtab-dmbtr clrdtab-wrbtr t001-waers bkpf-waers bkpf-kursf.
      PERFORM convert_into_foreign_curr USING
         clrdtab-mwsts clrdtab-wmwst t001-waers bkpf-waers bkpf-kursf.
      PERFORM convert_into_foreign_curr USING
         clrdtab-qbshh clrdtab-qbshb t001-waers bkpf-waers bkpf-kursf.
    ELSE.
* payment is in fc and cleared document is in lc
      PERFORM convert_into_foreign_curr USING
         clrdtab-wrbtr clrdtab-dmbtr t001-waers bkpf-waers bkpf-kursf.

      xwrbtr        = clrdtab-wrbtr.
      clrdtab-wrbtr = clrdtab-dmbtr.
      clrdtab-dmbtr = xwrbtr.

      PERFORM convert_into_foreign_curr USING
         clrdtab-wmwst clrdtab-mwsts t001-waers bkpf-waers bkpf-kursf.

      xwrbtr        = clrdtab-wmwst.
      clrdtab-wmwst = clrdtab-mwsts.
      clrdtab-mwsts = xwrbtr.

      PERFORM convert_into_foreign_curr USING
         clrdtab-qbshb clrdtab-qbshh t001-waers bkpf-waers bkpf-kursf.

      xwrbtr        = clrdtab-qbshb.
      clrdtab-qbshb = clrdtab-qbshh.
      clrdtab-qbshh = xwrbtr.
    ENDIF.
  ENDIF.
ENDFORM. " CHECK_CURRENCIES
*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_LOCAL_CURR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM convert_into_local_curr USING f_wrbtr f_dmbtr f_f_waers f_l_waers
                                   f_kursf.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      date             = bkpf-wwert
      foreign_amount   = f_wrbtr
      foreign_currency = f_f_waers
      rate             = f_kursf
      local_currency   = f_l_waers
    IMPORTING
      local_amount     = f_dmbtr.
ENDFORM. " CONVERT_INTO_LOCAL_CURR
*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_FOREIGN_CURR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM convert_into_foreign_curr USING f_dmbtr f_wrbtr f_l_waers f_f_waers
                                     f_kursf.

  CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
    EXPORTING
      date             = bkpf-wwert
      local_amount     = f_dmbtr
      local_currency   = f_l_waers
      rate             = f_kursf
      foreign_currency = f_f_waers
    IMPORTING
      foreign_amount   = f_wrbtr.
ENDFORM. " CONVERT_INTO_FOREIGN_CURR
*&---------------------------------------------------------------------*
*&      Form  PRINT_CERTIFICATE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_certificate.

  SORT: clrdtab.
  CLEAR copy_no.
**********Start of PDF Conversion by C5062443 Dt: 24-3-05**************
  CLEAR gv_copy_flag.
**********End of PDF Conversion by C5062443 Dt: 24-3-05****************

*  DO s_copy TIMES.
  PERFORM print_clrdtab.
*    ADD 1 TO copy_no.
**********Start of PDF Conversion by C5062443 Dt: 24-3-05***************
*    gv_copy_flag = 'X'.
**********End of PDF Conversion by C5062443 Dt: 24-3-05*****************

*  ENDDO.
ENDFORM. " PRINT_CERTIFICATE
*&---------------------------------------------------------------------*
*&      Form  READ_PRINTING_CHAR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_printing_char.
  CHECK xprtchr-j_1aprtchr NE bkpf-xblnr+4(1).

  READ TABLE xprtchr WITH KEY mandt = sy-mandt
                              j_1aprtchr = bkpf-xblnr+4(1).

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM j_1aprtchr WHERE j_1aprtchr = bkpf-xblnr+4(1).

  MOVE-CORRESPONDING j_1aprtchr TO xprtchr.
  APPEND xprtchr.
ENDFORM. " READ_PRINTING_CHAR
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_document_header USING f_belnr f_gjahr.
  CHECK xbkpf-bukrs NE bkpf-bukrs OR
        xbkpf-belnr NE f_belnr    OR
        xbkpf-gjahr NE f_gjahr.

  CLEAR: xbkpf, *bkpf.

  READ TABLE xbkpf WITH KEY mandt = sy-mandt
                            bukrs = bkpf-bukrs
                            belnr = f_belnr
                            gjahr = f_gjahr.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM bkpf INTO *bkpf
                 WHERE bukrs = bkpf-bukrs
                 AND   belnr = f_belnr
                 AND   gjahr = f_gjahr.

  MOVE-CORRESPONDING *bkpf TO xbkpf.
  APPEND xbkpf.
ENDFORM. " READ_DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*&      Form  CREATE_CLRDTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM create_clrdtab.
  IF xbsad-umsks IS INITIAL.
    CLEAR: xbsad-mwsts, xbsad-wmwst.
  ELSEIF bkpf-xblnr IS INITIAL.
* no official document no. and printing char. in down payment document
    bkpf-xblnr = xbsad-xblnr.
  ELSEIF bkpf-xblnr(4)   IS INITIAL AND
         bkpf-xblnr+5(8) IS INITIAL.
* no official document no. in down payment document
    bkpf-xblnr(4)   = xbsad-xblnr(4).
    bkpf-xblnr+5(8) = xbsad-xblnr+5(8).
  ENDIF.

  CLEAR clrdtab.

  IF xbsad-xcpdd NE space AND
     kna1-xcpdk  NE space.
    PERFORM read_cpd_data USING xbsad-belnr xbsad-buzei xbsad-gjahr.
    CHECK xbsec-empfg = paytab-empfg.

    MOVE-CORRESPONDING xbsec TO clrdtab.
  ENDIF.

  MOVE-CORRESPONDING: xbsad TO clrdtab.

  PERFORM fill_clrdtab.

  IF xbsad-xzahl NE space.
    IF xbsad-umsks = 'A'.
* cleared down payment
      CLEAR xbkpf.
      MOVE-CORRESPONDING xbsad TO xbkpf.
      PERFORM read_downpayment USING ' ' xbsad-belnr xbsad-gjahr
                                 xbsad-kunnr xbsad-wrbtr xbsad-wmwst.
      CHECK 1 = 2.
    ENDIF.
    PERFORM read_document_pos USING xbsad-belnr xbsad-gjahr xbsad-buzei.
    IF *bseg-rebzt = 'U' AND
       xbsad-buzei  > 1.
* umbuchungszeile
      xbuzei = xbsad-buzei - 1.
      PERFORM read_document_pos USING xbsad-belnr xbsad-gjahr xbuzei.
      IF *bseg-umsks = 'A' AND
         NOT *bseg-rebzg IS INITIAL.
*anzahlungsver. mit verweis auf anzahlung
* read down payment posting, to get the amounts
        PERFORM read_document_pos USING *bseg-rebzg *bseg-rebzj
                                        *bseg-rebzz.
        PERFORM read_downpayment USING 'X' *bseg-belnr *bseg-gjahr
                                 *bseg-kunnr *bseg-wrbtr *bseg-wmwst.
        CHECK 1 = 2.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM read_document_header USING xbsad-belnr xbsad-gjahr.
  MOVE-CORRESPONDING xbkpf TO clrdtab.

*  COLLECT clrdtab.
  APPEND clrdtab.
ENDFORM. " CREATE_CLRDTAB
*&---------------------------------------------------------------------*
*&      Form  READ_FOREIGN_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_foreign_id USING xland1
                              xstkzn
                     CHANGING xxstcd1      .
  CLEAR: xfrid, j_1afrid.
  READ TABLE xfrid WITH KEY mandt = sy-mandt
                            land1 = xland1
                            stkzn = xstkzn.
*entry exists?
  IF sy-subrc NE 0.

    SELECT SINGLE * FROM j_1afrid
                    WHERE land1 = xland1
                    AND   stkzn = xstkzn.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING j_1afrid TO xfrid.
      APPEND xfrid.
    ENDIF.
  ENDIF.
  IF sy-subrc EQ 0.
    xxstcd1 = xfrid-j_1afpid.
  ENDIF.

ENDFORM. " READ_FOREIGN_ID
*&---------------------------------------------------------------------*
*&      Form  FILL_CLRDTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_clrdtab.
  IF xbsad-umsks = 'A' AND xbsad-bstat = 'S'.
* down payment request
    LOOP AT dp_paytab WHERE empfg = paytab-empfg
                      AND   kunnr = paytab-kunnr
                      AND   dmbtr = xbsad-dmbtr
                      AND   mwsts = xbsad-mwsts.
      MOVE-CORRESPONDING dp_paytab TO clrdtab.
      EXIT.
    ENDLOOP.
  ELSE.
    CLEAR: clrdtab-qbshb, clrdtab-qbshh.
  ENDIF.

  IF xbsad-xzahl IS INITIAL AND
     xbsad-umsks NE 'A'.               " not for down payments
    PERFORM read_withhld_data USING xbsad-belnr xbsad-gjahr xbsad-buzei
                                    clrdtab-qbshb clrdtab-qbshh.
  ENDIF.

  IF xbsad-shkzg = 'H'.
    clrdtab-dmbtr = clrdtab-dmbtr * -1.
    clrdtab-wrbtr = clrdtab-wrbtr * -1.
    clrdtab-mwsts = clrdtab-mwsts * -1.
    clrdtab-wmwst = clrdtab-wmwst * -1.
  ENDIF.

  IF ( clrdtab-wrbtr < 0 AND clrdtab-qbshb < 0 ) OR
     ( clrdtab-wrbtr > 0 AND clrdtab-qbshb > 0 ).
    clrdtab-qbshb = clrdtab-qbshb * -1.
    clrdtab-qbshh = clrdtab-qbshh * -1.
  ENDIF.

  PERFORM check_currencies.
ENDFORM. " FILL_CLRDTAB
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_document_pos USING p_belnr
                                p_gjahr
                                p_buzei.

  DATA ETL1470C2R4586 TYPE TABLE OF BSEG.
DATA RLDNR_L1470C2R5674 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L1470C2R5674
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L1470C2R5674
    I_BUKRS = BKPF-BUKRS
    I_BELNR = P_BELNR
    I_GJAHR = P_GJAHR
    I_BUZEI = P_BUZEI
  IMPORTING
    ET_BSEG = ETL1470C2R4586
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL1470C2R4586 ) = 1.
  *BSEG = ETL1470C2R4586[ 1 ].
  SY-DBCNT = 1.
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  IF sy-subrc NE 0.
    CLEAR *bseg.
  ENDIF.
ENDFORM. " READ_DOCUMENT_POS
*&---------------------------------------------------------------------*
*&      Form  READ_DOWNPAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_downpayment USING f_readhd f_belnr f_gjahr
                            f_kunnr  f_wrbtr f_wmwst.
  CLEAR: ybsad.
  REFRESH ybsad.

  IF f_readhd NE space.
    PERFORM read_document_header USING f_belnr f_gjahr.
  ENDIF.
  MOVE-CORRESPONDING xbkpf TO clrdtab.

  SELECT * FROM bsad INTO TABLE ybsad WHERE bukrs EQ bkpf-bukrs
                                      AND   kunnr EQ f_kunnr
                                      AND   umsks EQ 'A'
                                      AND   augdt EQ xbkpf-budat
                                      AND   augbl EQ xbkpf-belnr
                                      AND   belnr NE xbkpf-belnr
                                      AND   wrbtr EQ f_wrbtr
                                      AND   wmwst EQ f_wmwst.
  LOOP AT ybsad.
    PERFORM read_document_header USING ybsad-belnr ybsad-gjahr.

    IF clrdtab-xblnr IS INITIAL.
* no official document no. and printing char. in down payment document
      clrdtab-xblnr = xbkpf-xblnr.
    ELSEIF clrdtab-xblnr(4)   IS INITIAL AND
           clrdtab-xblnr+5(8) IS INITIAL.
* no official document no. in down payment document
      clrdtab-xblnr(4)   = xbkpf-xblnr(4).
      clrdtab-xblnr+5(8) = xbkpf-xblnr+5(8).
    ENDIF.

    clrdtab-blart = xbkpf-blart.
    clrdtab-belnr = xbkpf-belnr.
    clrdtab-budat = xbkpf-budat.

*    COLLECT clrdtab.
    APPEND clrdtab.
    EXIT.
  ENDLOOP.

  IF sy-subrc NE 0.
*    COLLECT clrdtab.
    APPEND clrdtab.
  ENDIF.
ENDFORM. " READ_DOWNPAYMENT

*****************Start Of Conversion by C5063443 Dt: 10/2/05************

*&---------------------------------------------------------------------*
*&      Form  PDF_FORM_OPEN
*&---------------------------------------------------------------------*

FORM pdf_form_open.

  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name           = 'ZFIY0003'  "gv_fname
        IMPORTING
          e_funcname       = fm_name
          e_interface_type = e_interface_type.

    CATCH cx_root INTO w_cx_root.
      mesg = w_cx_root->get_text( ).
      MESSAGE mesg TYPE 'E'.

  ENDTRY.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = fp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. "PDF_FORM_OPEN


*&---------------------------------------------------------------------*
*&      Form  PDF_FORM_OUTPUT
*&---------------------------------------------------------------------*

FORM pdf_form_output.

  DATA:   docparams TYPE sfpdocparams.
  CALL FUNCTION fm_name
    EXPORTING
      /1bcdwb/docparams = docparams
      companyaddress    = gs_company_address
      vendoraddress     = gs_vendor_address
      info              = gs_info_data
      docdata           = gt_docdata
      total             = gs_total
      compcurrency      = gv_compcurrency
      transaction       = gv_transaction_type
      vatnumber         = gv_vat_number
      payline           = gs_payline
    EXCEPTIONS
      usage_error       = 1
      system_error      = 2
      internal_error    = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. "PDF_FORM_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  PDF_FORM_CLOSE
*&---------------------------------------------------------------------*

FORM pdf_form_close.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. "PDF_FORM_CLOSE

*****************End of conversion by C5062443 Dt: 10/2/05**************

*&---------------------------------------------------------------------*
*&  Include           ZFIY0008_FORM
*&---------------------------------------------------------------------**&---------------------------------------------------------------------*
*&       FORM  TOP-OF-PAGE                                             *
*&---------------------------------------------------------------------*
*&       Seitenkopf schreiben                                          *
*&---------------------------------------------------------------------*

FORM top_of_page.

  DATA: lr_grid TYPE REF TO cl_salv_form_layout_grid.

  CREATE OBJECT lr_grid.

  IF s_plist NE space.
    LOOP AT gt_doctab INTO gs_doctab.
      bhdgd-bukrs = gs_doctab-bukrs.
    ENDLOOP.
  ELSEIF s_precs NE space.
    LOOP AT gt_record_1 INTO gs_record_1.
      bhdgd-bukrs = gs_record_1-bukrs.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  lr_grid->create_label(
    row = 1
    column = 1
    text = text-hc0 ).

  IF xt001-todc = '80'.

    lr_grid->create_text(
      row = 1
      column = 2
     text = xt001-stcd1 ).
  ELSE.
    lr_grid->create_text(
     row = 1
     column = 2
    text = xt001-stcd1 ).

  ENDIF.

  CALL METHOD cl_salv_form_content=>set
    EXPORTING
      value = lr_grid.

ENDFORM.                    "TOP-OF-PAGE

*&--------------------------------------------------------------------*
*&      Form  end_of_list
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*

FORM end_of_list.

  DATA: lr_grid TYPE REF TO cl_salv_form_layout_grid.

  CREATE OBJECT lr_grid.

  lr_grid->create_label(
          row = 1
          column = 1
          text = text-t01 ).

  lr_grid->create_text(
         row = 1
         column = 2
*        text = hlp_sequential ).                           "1061683
        text = count_1 ).                                   "1061683

  CALL METHOD cl_salv_form_content=>set
    EXPORTING
      value = lr_grid.
  IF NOT sy-batch IS INITIAL.
    PERFORM print_log.
  ENDIF.
ENDFORM.                    "END_OF_list

* ALV Changes ends

AT LINE-SELECTION.
  IF doctab-belnr NE space.
    SET PARAMETER ID 'BLN' FIELD doctab-belnr.
    SET PARAMETER ID 'BUK' FIELD doctab-bukrs.
    SET PARAMETER ID 'GJR' FIELD doctab-gjahr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    CLEAR doctab.
  ELSEIF logtab-belnr NE space.
    SET PARAMETER ID 'BLN' FIELD logtab-belnr.
    SET PARAMETER ID 'BUK' FIELD logtab-bukrs.
    SET PARAMETER ID 'GJR' FIELD logtab-gjahr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    CLEAR logtab.
  ENDIF.
************************************************************************
* Selection Screen Checks regarding Company code and dates
************************************************************************
FORM check_selection_screen_1.
* check that only one company code entered
  DESCRIBE TABLE br_bukrs LINES xlines.

  IF xlines       NE 1       OR
     br_bukrs-low IS INITIAL OR
     ( br_bukrs-low NE br_bukrs-high AND
       NOT br_bukrs-high IS INITIAL ).
    MESSAGE e821.
  ENDIF.

  IF br_budat-low  IS INITIAL AND
     br_budat-high IS INITIAL.
    MESSAGE e000(38) WITH text-m01.
  ENDIF.

  IF NOT br_budat-high IS INITIAL AND
     br_budat-low(6)   NE br_budat-high(6).
    MESSAGE w000(38) WITH text-m03.
  ENDIF.
ENDFORM.                    "CHECK_SELECTION_SCREEN_1
************************************************************************
* Selection Screen Check regarding dataset
************************************************************************
FORM check_selection_screen_2.
  IF s_file1 NE space.
    CLOSE DATASET s_file1.
    OPEN DATASET s_file1 FOR OUTPUT IN TEXT MODE
    ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.           " UNICODE
    IF sy-subrc NE 0.
      MESSAGE e000(38) WITH text-m17 s_file1.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_SELECTION_SCREEN_2
************************************************************************
* Gets company code data
************************************************************************
FORM read_company_data.

  SELECT SINGLE * FROM t001 WHERE bukrs = br_bukrs-low.

  xt001-name1 =  t001-butxt.

  SELECT SINGLE * FROM t005 WHERE land1 = t001-land1.

  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AIDN'.

  xt001-stcd1 = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1ATID'.

  xt001-todc = t001z-paval.
ENDFORM.                    "READ_COMPANY_DATA
************************************************************************
* Gets customs data
************************************************************************
FORM read_customs_data.
  CLEAR: xtline, custom.
  REFRESH xtline.

  SELECT * FROM bkorm
           WHERE bukrs = bkpf-bukrs
             AND event = s_event
             AND koart = space
             AND belnr = bkpf-belnr
             AND gjahr = bkpf-gjahr.

*    text_name = bkorm-param+38(26).
    text_name = bkorm-param+38(30).    "<<<< insert - note 121276

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = 'FIKO'
        language = bkorm-param+78(1)
        name     = text_name
        object   = 'BKORM'
      TABLES
        lines    = xtline.
    EXIT.
  ENDSELECT.

  LOOP AT xtline.
    CASE sy-tabix.
      WHEN 1.
        custom-number = xtline-tdline(8).
* Zolldatum steht in der 4. Zeile - Hinweis 133207
*      when 2.
      WHEN 4.
        custom-date(4)   = xtline-tdline+6(4).
        custom-date+4(2) = xtline-tdline+3(2).
        custom-date+6(2) = xtline-tdline(2).
        EXIT.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "READ_CUSTOMS_DATA
************************************************************************
* Get Downpayment information
************************************************************************
FORM read_request.
  IF bseg-rebzg IS INITIAL.            " Downpayment
    IF bseg-koart EQ 'K'.
      PERFORM dpr_from_vendor_dp USING bkpf-budat bseg-belnr
                                       bseg-wrbtr bseg-wmwst.
    ELSE.
      PERFORM dpr_from_customer_dp USING bkpf-budat bseg-belnr
                                         bseg-wrbtr bseg-wmwst.
    ENDIF.
  ELSE.                                " Downpayment clearing
    PERFORM dpr_from_dpc.
  ENDIF.
ENDFORM.                    "READ_REQUEST
************************************************************************
* Get downpayment requests for vendors
************************************************************************
FORM dpr_from_vendor_dp USING f_budat f_belnr f_wrbtr f_wmwst.
  SELECT * FROM bsak WHERE bukrs = bseg-bukrs
                     AND   lifnr = bseg-lifnr
                     AND   umsks = 'A'
                     AND   augdt = f_budat
                     AND   augbl = f_belnr
                     AND   wrbtr = f_wrbtr
                     AND   wmwst = f_wmwst.

    SELECT SINGLE * FROM bkpf INTO *bkpf
                     WHERE bukrs = bsak-bukrs
                     AND   belnr = bsak-belnr
                     AND   gjahr = bsak-gjahr.

    CHECK sy-subrc EQ 0.

    doctab-brnch  = *bkpf-xblnr(4).
    doctab-offnum = *bkpf-xblnr+5(8).
    EXIT.
  ENDSELECT.
ENDFORM.                    "DPR_FROM_VENDOR_DP
************************************************************************
* Get downpayment requests for customers
************************************************************************
FORM dpr_from_customer_dp USING f_budat f_belnr f_wrbtr f_wmwst.
  SELECT * FROM bsad WHERE bukrs = bseg-bukrs
                     AND   kunnr = bseg-kunnr
                     AND   umsks = 'A'
                     AND   augdt = f_budat
                     AND   augbl = f_belnr
                     AND   wrbtr = f_wrbtr
                     AND   wmwst = f_wmwst.

    SELECT SINGLE * FROM bkpf INTO *bkpf
                     WHERE bukrs = bsad-bukrs
                     AND   belnr = bsad-belnr
                     AND   gjahr = bsad-gjahr.

    CHECK sy-subrc EQ 0.

    doctab-brnch  = *bkpf-xblnr(4).
    doctab-offnum = *bkpf-xblnr+5(8).
    EXIT.
  ENDSELECT.
ENDFORM.                    "DPR_FROM_CUSTOMER_DP
************************************************************************
* Search information for to read downpayment requests
************************************************************************
FORM dpr_from_dpc.
  SELECT SINGLE * FROM bkpf INTO *bkpf
                  WHERE bukrs = bseg-bukrs
                  AND   belnr = bseg-rebzg
                  AND   gjahr = bseg-rebzj.

  IF *bkpf-xblnr EQ space.
    DATA ETL276C4R8162 TYPE TABLE OF BSEG.
DATA RLDNR_L276C4R4551 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L276C4R4551
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L276C4R4551
    I_BUKRS = *BKPF-BUKRS
    I_BELNR = *BKPF-BELNR
    I_GJAHR = *BKPF-GJAHR
    I_BUZEI = BSEG-REBZZ
  IMPORTING
    ET_BSEG = ETL276C4R8162
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL276C4R8162 ) = 1.
  *BSEG = ETL276C4R8162[ 1 ].
  SY-DBCNT = 1.
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


    IF bseg-koart EQ 'K'.
      PERFORM dpr_from_vendor_dp USING *bkpf-budat *bseg-belnr
                                       *bseg-wrbtr *bseg-wmwst.
    ELSE.
      PERFORM dpr_from_customer_dp USING *bkpf-budat *bseg-belnr
                                         *bseg-wrbtr *bseg-wmwst.
    ENDIF.
  ELSE.
    doctab-brnch  = *bkpf-xblnr(4).
    doctab-offnum = *bkpf-xblnr+5(8).
  ENDIF.
ENDFORM.                    "DPR_FROM_DPC
************************************************************************
* Fill LOG information
************************************************************************
FORM fill_logtab USING f_bukrs f_belnr f_gjahr f_stext f_ltext.
  logtab-bukrs = f_bukrs.
  logtab-belnr = f_belnr.
  logtab-gjahr = f_gjahr.
  logtab-stext = f_stext.
  logtab-ltext = f_ltext.
  APPEND logtab.
ENDFORM.                    "FILL_LOGTAB
************************************************************************
* Gets TAX information
************************************************************************
FORM fill_taxes.

  IF NOT p_comp IS INITIAL.
*---Compras
    IF bset-ktosl NE xtaxid-ktosl.
      CLEAR xtaxid.
      READ TABLE xtaxid WITH KEY mandt = sy-mandt
                                 kalsm = t005-kalsm
                                 ktosl = bset-ktosl.
    ENDIF.
* Set Debit/credit indicator
    IF bset-shkzg EQ 'H'.
      bset-hwbas = 0 - bset-hwbas.
      bset-hwste = 0 - bset-hwste.
    ENDIF.
    IF   xtaxid-j_1ataxid(2) = 'TX'.
      IF bset-hwste NE 0.
        hlp_vatam = hlp_vatam + bset-hwste.
      ENDIF.
    ENDIF.

  ELSE.

    MOVE bset-buzei TO doctab-buzei.
*---Ventas
    IF bset-ktosl NE xtaxid-ktosl.
      CLEAR xtaxid.
      READ TABLE xtaxid WITH KEY mandt = sy-mandt
                                 kalsm = t005-kalsm
                                 ktosl = bset-ktosl.
    ENDIF.
* Set Debit/credit indicator
    IF bset-shkzg EQ 'H'.
      bset-hwbas = 0 - bset-hwbas.
      bset-hwste = 0 - bset-hwste.
    ENDIF.
    IF   xtaxid-j_1ataxid(2) = 'TX'.
*      IF bset-hwste NE 0.
      hlp_vatam =  bset-hwste ."+ HLP_VATAM .
*      ENDIF.
    ENDIF.

*    CHECK: doctab-accno    NE space,
*           reject_document EQ space,
*           hlp_vatam       NE 0.

    PERFORM read_document_type.

    PERFORM read_document_class.

    PERFORM read_official_doc_type.

    doctab-j_1aoftp = xotdet-j_1aoftp.

    doctab-vatam =  hlp_vatam.
*** Begin of Note 987617 ***
    doctab-waers = t001-waers.
    MOVE-CORRESPONDING bset TO st_bset.
    APPEND st_bset TO t_bset.
    APPEND doctab.
    CLEAR:
              doctab-stcdt,
              doctab-stcd1,
              doctab-j_1aoftp,
              doctab-xcpdd,
              doctab-vatam,
              doctab-waers,
              doctab-xfile,
              doctab-stblg,
              hlp_vatam   .
  ENDIF.


ENDFORM.                    "FILL_TAXES
************************************************************************
* Read SAP document type
************************************************************************
FORM read_document_type.
  CHECK bkpf-blart NE xt003-blart.
  CLEAR: xt003, t003.
  READ TABLE xt003 WITH KEY mandt = sy-mandt
                            blart = bkpf-blart.
  CHECK sy-subrc NE 0.
  SELECT SINGLE * FROM t003 WHERE blart = bkpf-blart.
  MOVE-CORRESPONDING t003 TO xt003.
  APPEND xt003.
ENDFORM.                    "READ_DOCUMENT_TYPE
************************************************************************
* Read Argentinean official document class
************************************************************************
FORM read_document_class.
  CHECK bkpf-blart NE xt003_i-blart.
  CLEAR: xt003_i, t003_i.
  READ TABLE xt003_i WITH KEY  mandt = sy-mandt
                               land1 = 'AR'
                               blart = bkpf-blart.
  CHECK sy-subrc NE 0.
  SELECT SINGLE * FROM t003_i WHERE land1 = 'AR'
                              AND   blart = bkpf-blart.
  MOVE-CORRESPONDING t003_i TO xt003_i.
  APPEND xt003_i.
ENDFORM.                    "READ_DOCUMENT_CLASS
************************************************************************
* Read Argentinean official document type
************************************************************************
FORM read_official_doc_type.
  CHECK xotdet-doccls     NE xt003_i-doccls   OR
        xotdet-j_1aprtchr NE xprtchr.

  CLEAR: xotdet, j_1aotdetr.

  READ TABLE xotdet WITH KEY     doccls = xt003_i-doccls
                             j_1aprtchr = xprtchr.

  IF sy-subrc NE 0.
    SELECT SINGLE * FROM j_1aotdetr WHERE land1 = 'AR'
                                    AND   id_report =  sy-repid
                                    AND   doccls = xt003_i-doccls
                                    AND   j_1aprtchr = xprtchr.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING j_1aotdetr TO xotdet.
      APPEND xotdet.
    ENDIF.
  ENDIF.
ENDFORM.                    "READ_OFFICIAL_DOC_TYPE
************************************************************************
* List Display (Screen)
************************************************************************
FORM print_doctab.
  DATA:   hlp_accno LIKE doctab-accno.
  CLEAR: report_total_vatam.
  CLEAR: hlp_accno,
         hlp_page,
         hlp_sequential.
  FORMAT INTENSIFIED OFF.
  PERFORM batch_heading_prepare.
  NEW-PAGE.

  SORT doctab.

  LOOP AT doctab.
    CLEAR xt003_i.
    READ TABLE xt003_i WITH KEY blart = doctab-blart.

    IF xt003_i-offnrel <> 'Z'. "Note 731898
      IF doctab-j_1aoftp  IS INITIAL.
        PERFORM fill_logtab USING doctab-bukrs doctab-belnr doctab-gjahr
                                  space      text-l20.
      ENDIF.
      IF doctab-offnum IS INITIAL.
        PERFORM fill_logtab USING doctab-bukrs doctab-belnr doctab-gjahr
                                  space      text-l10.
      ENDIF.
    ENDIF.                     "Note 731898
    IF hlp_accno NE doctab-accno.
      hlp_accno = doctab-accno.
      PERFORM read_master_data.
    ENDIF.

    report_total_vatam  = report_total_vatam + doctab-vatam.

*del    if hlp_sequential eq '999999'.
    IF hlp_sequential EQ '763769'.
      CLEAR hlp_sequential.
    ENDIF.
    ADD 1 TO hlp_sequential.

    PERFORM fill_record_1.
  ENDLOOP.

*  PERFORM PRINT_TOTALS.                         "ALV Comment
*  PERFORM PRINT_LOG.                            "ALV Comment

* ALV Changes starts

  DESCRIBE TABLE logtab.
  IF sy-tfill GT 0.
    MESSAGE i820.
    CLEAR logtab.
  ENDIF.

  PERFORM fieldcat_build  USING gc_struct1 gc_struct2
                          CHANGING gt_fieldcat.
  PERFORM eventtab_alv_build CHANGING gt_event.
  PERFORM output.

* ALV Changes ends

  IF s_file1 NE space.
    CLOSE DATASET s_file1.
  ENDIF.
ENDFORM.                    "PRINT_DOCTAB
************************************************************************
* Read vendor/customer master data
************************************************************************
FORM read_master_data.
  IF doctab-koart = 'K'.
    SELECT SINGLE * FROM lfa1 WHERE lifnr = doctab-accno.
  ELSEIF doctab-koart = 'D'.
    SELECT SINGLE * FROM kna1 WHERE kunnr = doctab-accno.
  ENDIF.
ENDFORM.                    "READ_MASTER_DATA
************************************************************************
* Read Identification code for foreigners
************************************************************************
FORM read_foreign_id USING p_land1
                           p_stkzn
                  CHANGING p_stcd1.

  CLEAR: xfrid, j_1afrid.
  READ TABLE xfrid WITH KEY mandt = sy-mandt
                            land1 = p_land1
                            stkzn = p_stkzn.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM j_1afrid WHERE land1 = p_land1
                                  AND   stkzn = p_stkzn.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING j_1afrid TO xfrid.
      APPEND xfrid.
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    p_stcd1 = xfrid-j_1afpid.
  ENDIF.
ENDFORM.                    "READ_FOREIGN_ID
************************************************************************
* Create output dataset
************************************************************************
FORM fill_record_1.

  DATA st_bseg TYPE bseg.
  CLEAR: record_1.

* ALV Comment starts
  record_1-bldat(2)   = doctab-bldat+6(2).
  record_1-bldat+2(2) = doctab-bldat+4(2).
  record_1-bldat+4(4) = doctab-bldat(4).
* ALV Comment ends

*  record_1-bldat     = doctab-bldat.            "ALV Addition

  record_1-j_1aoftp   = doctab-j_1aoftp.
  record_1-brnch      = doctab-brnch.
* RECORD_1-OFFNUM     = DOCTAB-OFFNUM.            "991255
  record_1-offnum     = doctab-offnum+5(11).      "991255
* record_1-eor        = '0D0A'.
* record_1-eor        = '0D'.


  IF doctab-xcpdd NE space.
    SELECT * FROM bsec WHERE bukrs = doctab-bukrs
                       AND   belnr = doctab-belnr
                       AND   gjahr = doctab-gjahr.
      DO 3 TIMES.
        REPLACE '-' WITH ' ' INTO bsec-stcd1.
        CONDENSE bsec-stcd1 NO-GAPS.
      ENDDO.

      hlp_stcdt = bsec-stcdt.
      record_1-stcd1 = bsec-stcd1.

      IF bsec-land1 NE t001-land1.
        PERFORM read_foreign_id USING bsec-land1 bsec-stkzn
                CHANGING record_1-stcd1.
      ENDIF.

      IF bsec-name1 NE space.
        record_1-name1 = bsec-name1.
      ELSE.
        hlp_name    = bsec-name2.
        hlp_name+36 = bsec-name3.
        CONDENSE hlp_name.
        record_1-name1 = hlp_name.
      ENDIF.
      EXIT.
    ENDSELECT.
  ELSEIF doctab-koart = 'D'.
    IF kna1-name1 NE space.
      record_1-name1 = kna1-name1.
    ELSE.
      hlp_name    = kna1-name2.
      hlp_name+36 = kna1-name3.
      CONDENSE hlp_name.
      record_1-name1 = hlp_name.
    ENDIF.

    hlp_stcdt = kna1-stcdt.
    record_1-stcd1 = kna1-stcd1.

    IF kna1-land1 NE t001-land1.
      PERFORM read_foreign_id USING kna1-land1 kna1-stkzn
              CHANGING record_1-stcd1.
    ENDIF.
  ELSEIF doctab-koart = 'K'.
    IF lfa1-name1 NE space.
      record_1-name1 = lfa1-name1.
    ELSE.
      hlp_name    = lfa1-name2.
      hlp_name+36 = lfa1-name3.
      CONDENSE hlp_name.
      record_1-name1 = hlp_name.
    ENDIF.

    hlp_stcdt = lfa1-stcdt.
    record_1-stcd1 = lfa1-stcd1.

    IF lfa1-land1 NE t001-land1.
      PERFORM read_foreign_id USING lfa1-land1 lfa1-stkzn
              CHANGING record_1-stcd1.
    ENDIF.
  ENDIF.

* Change Fields with company data when Credit Memo
*Note 550097: For Credit Memos A and b display company code's CUIT
  IF ( doctab-j_1aoftp = '03' ) OR (  doctab-j_1aoftp = '08' ).
    record_1-stcd1 = xt001-stcd1.
    record_1-name1 = xt001-name1.
  ENDIF.

  doctab-stcdt = hlp_stcdt.
  doctab-stcd1 = record_1-stcd1.

  hlp_accno   = doctab-koart.
  hlp_accno+1 = doctab-accno.

  IF record_1-stcd1 IS INITIAL.
    PERFORM fill_logtab USING doctab-bukrs doctab-belnr doctab-gjahr
                              hlp_accno    text-l12.
  ENDIF.

  IF hlp_stcdt <> '80'.
    PERFORM fill_logtab USING doctab-bukrs doctab-belnr doctab-gjahr
                              hlp_accno    text-l18.
  ENDIF.

  CLEAR:     record_1-stcd1v,
             record_1-name1v,
             record_1-vatamv.

  record_1-vatam = doctab-vatam * 100.

  IF s_file1 NE space.
* Note 1061683 Start
    IF NOT doctab-xfile IS INITIAL OR
       NOT doctab-stblg IS INITIAL.
      READ TABLE doctab WITH KEY belnr = doctab-stblg.
      IF sy-subrc NE 0.
        TRANSFER record_1 TO s_file1.
        ADD 1 TO count_1.
      ENDIF.
    ELSE.
      TRANSFER record_1 TO s_file1.
      ADD 1 TO count_1.
    ENDIF.
* Note 1061683 End
  ENDIF.

  IF s_plist NE space.
* ALV Changes starts

    MOVE hlp_sequential TO gs_doctab-serialno.
    MOVE hlp_sequential TO gs_record_1-serialno.

* ALV Changes ends
    IF doctab-koart = 'D'.
      text50 = text-p10.
    ELSE.
      text50 = text-p12.
    ENDIF.
    text50+11 = doctab-accno.
    CONDENSE text50.
    MOVE text50          TO gs_doctab-bukrs_accno.
    MOVE doctab-bukrs    TO gs_doctab-bukrs.
    MOVE doctab-budat    TO gs_doctab-budat.
    MOVE doctab-stcd1    TO gs_doctab-stcd1.
    MOVE doctab-belnr    TO gs_doctab-belnr.
    MOVE doctab-blart    TO gs_doctab-blart.
    MOVE doctab-gjahr    TO gs_doctab-gjahr.
    MOVE doctab-bldat    TO gs_doctab-bldat.
    MOVE doctab-j_1aoftp TO gs_doctab-j_1aoftp.
    MOVE doctab-brnch    TO gs_doctab-brnch.
    MOVE doctab-offnum   TO gs_doctab-offnum.
    MOVE doctab-vatam    TO gs_doctab-vatam.
    MOVE doctab-waers    TO gs_doctab-waers.
    MOVE doctab-buzei    TO gs_doctab-buzei.

    LOOP AT t_bseg INTO st_bseg
    WHERE bukrs   EQ  gs_doctab-bukrs
    AND   belnr   EQ  gs_doctab-belnr
    AND   gjahr   EQ  gs_doctab-gjahr.
      IF st_bseg-kunnr IS NOT INITIAL.
        MOVE st_bseg-kunnr    TO gs_doctab-kunnr.
      ENDIF.
      IF st_bseg-lifnr IS NOT INITIAL.
        MOVE st_bseg-lifnr    TO gs_doctab-lifnr.
      ENDIF.
    ENDLOOP.
    APPEND gs_doctab     TO gt_doctab.
    CLEAR gs_doctab.

* ALV Changes ends

  ENDIF.

  IF s_precs NE space.
* ALV Changes starts

    MOVE doctab-bukrs         TO gs_record_1-bukrs.
    MOVE record_1-j_1aoftp    TO gs_record_1-j_1aoftp.
    MOVE record_1-brnch       TO gs_record_1-brnch.
    MOVE doctab-offnum        TO gs_record_1-offnum.
    MOVE record_1-stcd1       TO gs_record_1-stcd1 .
    MOVE record_1-name1       TO gs_record_1-name1 .
    MOVE doctab-vatam         TO gs_record_1-vatam .
    MOVE doctab-waers         TO gs_record_1-waers.
    MOVE record_1-stcd1v      TO gs_record_1-stcd1v.
    MOVE record_1-name1v      TO gs_record_1-name1v.
    MOVE record_1-vatamv      TO gs_record_1-vatamv.
    MOVE doctab-bldat         TO gs_record_1-bldat.
    APPEND gs_record_1        TO gt_record_1.
    CLEAR gs_record_1.

* ALV Changes ends
  ENDIF.

* Insert 03.08.2011 - Graba registro para salida de archivo - Diego
    CLEAR st_compras.
* Tratamiento de casos específicos AA/Dancan ------------------------
    IF record_1-j_1aoftp IS INITIAL.
       record_1-j_1aoftp = '39'.
    ENDIF.
    IF record_1-j_1aoftp = '39'.
       record_1-offnum     = doctab-offnum.
    ENDIF.
    IF record_1-stcd1 = '55000000050' OR
       record_1-stcd1 = '00000000000'.
       CLEAR record_1-stcd1.
    ENDIF.
* Fin casos específicos  ---------------------------------------------

    MOVE record_1(70) TO st_compras.

* Change 25.08.2011 - Elimina signo para negativos - Sonda
*    IF doctab-vatam GE 0.
* ..........................................................
       MOVE record_1-vatam TO st_compras-imp_liq.
* ..........................................................
*    ELSE.
*      MOVE doctab-vatam TO st_compras-imp_liq.
*      REPLACE '-' WITH ' ' INTO st_compras-imp_liq.
*      REPLACE '.' WITH ' ' INTO st_compras-imp_liq.
*      CONDENSE st_compras-imp_liq NO-GAPS.
*      SHIFT st_compras-imp_liq RIGHT DELETING TRAILING ''.
*      OVERLAY st_compras-imp_liq WITH '-00000000000'.
*    ENDIF.
* End change 25.08.2011 ....................................

    APPEND st_compras TO t_compras.
* End of 03.08.2011 -------------------------------------------------

  CLEAR: record_1,
         hlp_name.

ENDFORM.                    "FILL_RECORD_1
************************************************************************
* Printing LOG list
************************************************************************
FORM print_log.

* ALV Changes starts

  DATA : lv_concatedata(132) TYPE c,
         lv_pos TYPE i VALUE 40.

  gs_log-aluser    = g_aluser.
  gs_log-alprog    = g_alprog.

** To create the Application Log
  PERFORM create_log .

* ALV Comment ends
  LOOP AT logtab.
    text50    = text-l16.
    text50+16 = logtab-bukrs.
    text50+21 = logtab-gjahr.
    text50+26 = logtab-belnr.
    CONDENSE text50.

    IF logtab-stext NE space.
      IF logtab-stext(1) = 'D'.
        REPLACE '&1' WITH text-p10 INTO logtab-ltext.
      ELSE.
        REPLACE '&1' WITH text-p12 INTO logtab-ltext.
      ENDIF.
      REPLACE '&2' WITH logtab-stext+1(10) INTO logtab-ltext.
      CONDENSE logtab-ltext.
    ENDIF.

*    WRITE: LOGTAB-LTEXT.                      "ALV Comment

* ALV Changes starts

    MOVE : text50 TO lv_concatedata,
           logtab-ltext TO lv_concatedata+lv_pos.

    PERFORM msg_add_alv USING 'I' lv_concatedata .

    CLEAR lv_concatedata.

* ALV Changes ends

    HIDE: logtab-bukrs,
          logtab-gjahr,
          logtab-belnr.

  ENDLOOP.

* ALV Changes starts

** To create the Profile for the Application Log
  PERFORM create_profile CHANGING gs_display_profile.

** To display the application log
  PERFORM display_log.


ENDFORM.                    "PRINT_LOG
************************************************************************
* Prepare information for screen list
************************************************************************
FORM batch_heading_prepare.
  CLEAR bhdgd.
  bhdgd-inifl = 0.
  bhdgd-lines = 130.
  sy-title = text-h00.
  REPLACE '&1' WITH br_budat-low+4(2) INTO sy-title.
  REPLACE '&2' WITH br_budat-low+2(2) INTO sy-title.

  bhdgd-uname = sy-uname.
  bhdgd-repid = sy-repid.
  bhdgd-line1 = sy-title.
  bhdgd-bukrs = t001-bukrs.

  IF  hlp_page = 'L'.
    bhdgd-line2 = text-h09.
  ENDIF.
ENDFORM.                    "BATCH_HEADING_PREPARE
************************************************************************
*       Check if original sd document has been canceled
************************************************************************
FORM check_if_canceled_sd.

  CLEAR: xvbeln, comwa, vbfa_tab.                           "1087918
  REFRESH vbfa_tab.                                         "1087918
  CLEAR: xvbeln.
  xvbeln = bkpf-awkey(10).

  SELECT SINGLE * FROM vbrk WHERE vbeln = xvbeln.
  CHECK sy-subrc = 0.

  comwa-mandt = sy-mandt.

  IF vbrk-sfakn IS INITIAL.
    comwa-vbeln = bkpf-awkey(10).
  ELSE.
    comwa-vbeln = vbrk-sfakn.
  ENDIF.

  CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION' "#EC CI_USAGE_OK[2198647]
    EXPORTING
      comwa    = comwa
    TABLES
      vbfa_tab = vbfa_tab.

  IF vbrk-sfakn IS INITIAL.
* get the sd reverse document
    LOOP AT vbfa_tab WHERE vbelv   = comwa-vbeln
                     AND   vbtyp_n CA  'SN'.   " changed by: Note 532534
      sd_vbeln  = vbfa_tab-vbeln.
      EXIT.
    ENDLOOP.
* get the fi document of the reversal
    LOOP AT vbfa_tab WHERE vbelv   =  sd_vbeln
                     AND   vbtyp_n =  '+'.
      bkpf-stblg = vbfa_tab-vbeln.
      EXIT.
    ENDLOOP.
  ELSE.
* get the fi document of the reversed document
    LOOP AT vbfa_tab WHERE vbelv   = comwa-vbeln
                     AND   vbtyp_n =  '+'.
      bkpf-stblg = vbfa_tab-vbeln.
      EXIT.
    ENDLOOP.
  ENDIF  .
* Note 1087918 End
ENDFORM.                    "CHECK_IF_CANCELED_SD
************************************************************************
*       Check if original MM document has been canceled
************************************************************************
FORM check_if_canceled_mm.

  CLEAR: xmbelnr, xmgjahr.
  CLEAR: mm_doc.                                            "1087918
  REFRESH: mm_doc.                                          "1087918
  xmbelnr = bkpf-awkey(10).
  xmgjahr = bkpf-awkey+10(4).

  SELECT SINGLE * FROM rbkp WHERE belnr = xmbelnr
                            AND   gjahr = xmgjahr.
  CHECK sy-subrc = 0.

  IF NOT rbkp-stblg IS INITIAL.
*    bkpf-stblg = rbkp-stblg.
    mm_rjahr = rbkp-stjah.
    CALL FUNCTION 'AC_DOCUMENT_RECORD'
      EXPORTING
        i_awtyp      = 'RMRP'
        i_awref      = rbkp-stblg
        i_aworg      = mm_rjahr
        x_dialog     = not_dialog
      TABLES
        t_documents  = mm_doc
      EXCEPTIONS
        no_reference = 1
        no_document  = 2
        OTHERS       = 3.

    LOOP AT mm_doc WHERE awtyp = 'BKPF'.
* Note 1114951 Start
      SELECT SINGLE xblnr INTO xblnr
      FROM bkpf
      WHERE bukrs = bkpf-bukrs
      AND   belnr = mm_doc-docnr
      AND   gjahr = mm_rjahr.
      IF sy-subrc EQ 0
      AND NOT xblnr IS INITIAL.
        bkpf-stblg = mm_doc-docnr.
        EXIT.
      ENDIF.
* Note 1114951 End
    ENDLOOP.
  ENDIF.
* Note 1087918 End
ENDFORM.                    "CHECK_IF_CANCELED_MM

* ALV Changes starts

*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
*      This is used to set the PF-STATUS
*---------------------------------------------------------------------*
*      -->IV_EXTAB   text
*---------------------------------------------------------------------*
FORM set_pf_status USING iv_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTANDARD_ALV_CITI'.

ENDFORM.                               " SET_PF_STATUS


*&---------------------------------------------------------------------*
*&      Form  eventtab_alv_build
*&---------------------------------------------------------------------*
*       This function module is used to get the events
*----------------------------------------------------------------------*
*      <--XT_EVENTTAB  text
*----------------------------------------------------------------------*
FORM eventtab_alv_build  CHANGING xt_eventtab TYPE slis_t_event.

  DATA: ls_events TYPE slis_alv_event.

  REFRESH xt_eventtab.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = xt_eventtab
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.  " IF sy-subrc <> 0.

  READ TABLE xt_eventtab INTO ls_events WITH
       KEY name = slis_ev_top_of_page.

  ls_events-form = slis_ev_top_of_page.
  MODIFY xt_eventtab FROM ls_events
                          TRANSPORTING form
                          WHERE name EQ slis_ev_top_of_page.

  READ TABLE xt_eventtab WITH KEY name = slis_ev_end_of_list
                           INTO ls_events.
  IF  sy-subrc EQ 0.
    MOVE slis_ev_end_of_list TO ls_events-form.
    APPEND ls_events TO xt_eventtab.
  ENDIF.  "IF sy-subrc <> 0.

ENDFORM.                               " eventtab_alv_build


*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD
*&---------------------------------------------------------------------*
* This is used for the fieldcatalog merging
*----------------------------------------------------------------------*
*      <-> XT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat_build   USING value(is_struct1) TYPE dd02l-tabname
                            value(is_struct2) TYPE dd02l-tabname
                      CHANGING xt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

  CONSTANTS: lc_mediumtext(1)  TYPE c VALUE 'M'.   "for DDICTXT Variable

*Field symbol for events
  FIELD-SYMBOLS <fs> TYPE  slis_fieldcat_alv.

  IF s_plist NE space AND s_precs EQ space.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = gv_repid
        i_structure_name       = gc_struct1
      CHANGING
        ct_fieldcat            = xt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.   "IF sy-subrc <> 0.


*Fieldcatalog modification
    LOOP AT xt_fieldcat ASSIGNING <fs>.
      CASE <fs>-fieldname.

        WHEN 'SERIALNO'.
          <fs>-col_pos             = 1.
          <fs>-lzero               = gc_value_x.
          <fs>-seltext_m           = text-h10.

        WHEN 'BUKRS_ACCNO'.
          <fs>-col_pos             = 2.
          <fs>-seltext_m           = text-h11.

        WHEN 'STCDT'.
          <fs>-tech                =  gc_value_x.

        WHEN 'BUKRS'.
          <fs>-tech                =  gc_value_x.

        WHEN 'ACCNO'.
          <fs>-tech                =  gc_value_x.

        WHEN 'CALYE'.
          <fs>-tech                =  gc_value_x.

        WHEN 'CALMO'.
          <fs>-tech                =  gc_value_x.

        WHEN 'KOART'.
          <fs>-tech                =  gc_value_x.

        WHEN 'XCPDD'.
          <fs>-tech                =  gc_value_x.

        WHEN 'BUDAT'.
          <fs>-col_pos             = 3.

        WHEN 'BLART'.
          <fs>-col_pos             = 4.

        WHEN 'BELNR'.
          <fs>-col_pos             = 5.

        WHEN 'GJAHR'.
          <fs>-col_pos             = 6.

        WHEN 'BLDAT'.
          <fs>-col_pos             = 7.

        WHEN 'OFFNUM'.
          <fs>-seltext_m           =  text-off.
          <fs>-col_pos             = 8.

        WHEN 'BRNCH'.
          <fs>-col_pos             = 9.

        WHEN 'J_1AOFTP'.
          <fs>-seltext_m           =  text-odt.
          <fs>-col_pos             = 10.

        WHEN 'STCD1'.
          <fs>-col_pos             = 11.
          <fs>-seltext_m           =  text-h27.
          <fs>-edit_mask           = '__-________-_'.

        WHEN 'VATAM'.
          <fs>-col_pos             = 12.
          <fs>-seltext_m           =  text-h24.

*** Begin of Note 987617 ***
          <fs>-cfieldname   = 'WAERS'.
          <fs>-ctabname = <fs>-tabname.

        WHEN 'WAERS'.
          <fs>-tech = 'X'.

*** End of Note 987617 ***

      ENDCASE.   " CASE <fs>-fieldname.
      <fs>-ddictxt = gc_value_m.
    ENDLOOP.   " LOOP AT xt_fieldcat

  ELSEIF s_precs NE space AND s_plist EQ space.

    REFRESH xt_fieldcat.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = gv_repid
        i_structure_name       = gc_struct2
      CHANGING
        ct_fieldcat            = xt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF. "IF sy-subrc <> 0.

    LOOP AT xt_fieldcat ASSIGNING <fs>.
      CASE <fs>-fieldname.

        WHEN 'SERIALNO'.
          <fs>-tech                =  gc_value_x.

        WHEN 'BUKRS'.
          <fs>-tech                =  gc_value_x.

        WHEN 'J_1AOFTP'.
          <fs>-seltext_m           =  text-odt.
          <fs>-col_pos             = 1.

        WHEN 'BRNCH'.
          <fs>-col_pos             = 2.

        WHEN 'OFFNUM'.
          <fs>-seltext_m           =  text-off.
          <fs>-col_pos             = 3.

        WHEN 'BLDAT'.
          <fs>-col_pos             = 4.

        WHEN 'STCD1'.
          <fs>-seltext_m           = text-h27.
          <fs>-col_pos             = 5.

        WHEN 'NAME1'.
          <fs>-col_pos             = 6.

        WHEN 'STCD1V'.
          <fs>-col_pos             = 7.

        WHEN 'VATAM'.
          <fs>-col_pos             = 8.
          <fs>-seltext_m           = text-h24.
*** Begin of Note 987617 ***
          <fs>-cfieldname   = 'WAERS'.
          <fs>-ctabname = <fs>-tabname.

        WHEN 'WAERS'.
          <fs>-tech = 'X'.

*** End of Note 987617 ***


        WHEN 'NAME1V'.
          <fs>-tech                =  gc_value_x.

        WHEN 'VATAMV'.
          <fs>-col_pos             = 9.
          <fs>-seltext_m           = text-vat.

      ENDCASE.  "      CASE <fs>-fieldname.
      <fs>-ddictxt                 = gc_value_m.
    ENDLOOP. "LOOP AT xt_fieldcat


  ELSEIF s_plist NE space AND s_precs NE space.

    REFRESH xt_fieldcat.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = gv_repid
        i_internal_tabname     = gt_tab_header
        i_structure_name       = is_struct1
      CHANGING
        ct_fieldcat            = xt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.  "IF sy-subrc <> 0.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = gv_repid
        i_internal_tabname     = gt_tab_item
        i_structure_name       = is_struct2
      CHANGING
        ct_fieldcat            = xt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.  "IF sy-subrc <> 0.

    LOOP AT xt_fieldcat ASSIGNING <fs>.
      CASE <fs>-fieldname.

        WHEN 'SERIALNO'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-seltext_m        = text-h10.
            <fs>-lzero            = gc_value_x.
          ELSE.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'BUKRS_ACCNO'.
          <fs>-seltext_m          = text-h11.

        WHEN 'STCDT'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-tech             = gc_value_x.
          ELSE.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'BELNR'.
          IF <fs>-tabname         = gc_inttab1.
          ELSE.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'BUKRS'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-tech             = gc_value_x.
          ELSE.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'CALYE'.
          <fs>-tech               = gc_value_x.

        WHEN 'CALMO'.
          <fs>-tech               = gc_value_x.

        WHEN 'KOART'.
          <fs>-tech               = gc_value_x.

        WHEN 'ACCNO'.
          <fs>-tech               = gc_value_x.

        WHEN 'GJAHR'.
          IF <fs>-tabname         = gc_inttab2.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'BRNCH'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'OFFNUM'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-tech             = gc_value_x.
          ELSE.
            <fs>-seltext_m        = text-off.
          ENDIF.

        WHEN 'STCD1'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-edit_mask        = '__-________-_'.
            <fs>-seltext_m        = text-h27.
          ELSE.
            <fs>-seltext_m        = text-h27.
          ENDIF.

        WHEN 'J_1AOFTP'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-tech             = gc_value_x.
          ELSEIF <fs>-tabname     = gc_inttab2.
            <fs>-seltext_m        = text-odt.
          ENDIF.

        WHEN 'XCPDD'.
          IF <fs>-tabname         = gc_inttab1.
            <fs>-tech             = gc_value_x.
          ELSE.
            <fs>-tech             = gc_value_x.
          ENDIF.

        WHEN 'NAME1V'.
          <fs>-tech               = gc_value_x.

        WHEN 'VATAM'.
          <fs>-seltext_m          = text-h24.

*** Begin of Note 987617 ***
          <fs>-cfieldname   = 'WAERS'.
          <fs>-ctabname = <fs>-tabname.

        WHEN 'WAERS'.
          <fs>-tech = 'X'.

*** End of Note 987617 ***

        WHEN 'VATAMV'.
          <fs>-seltext_m          = text-vat.

      ENDCASE.  "CASE <fs>-fieldname.
      <fs>-ddictxt                = gc_value_m.
    ENDLOOP. "LOOP AT xt_fieldcat
  ENDIF.    " if s_plist NE space AND s_precs EQ space.

  CLEAR ls_fieldcat.
  ls_fieldcat-do_sum = gc_value_x.
  MODIFY xt_fieldcat FROM ls_fieldcat TRANSPORTING do_sum
                 WHERE fieldname = gc_vatam.

ENDFORM.                    " FIELDCAT_BUILD


*&---------------------------------------------------------------------*
*&      Form  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this funtion module.
*----------------------------------------------------------------------*

FORM output .
  DATA:
      vl_tabix   TYPE sy-tabix              ,
      stl_bseg   TYPE bseg                  ,
      tl_bkpf    TYPE STANDARD TABLE OF bkpf,
      stl_rbkp   TYPE rbkp                  ,
      stl_lfa1   TYPE lfa1                  ,
      stl_doctab TYPE ty_gss_j_1af217_list1 ,
      stl_aux    TYPE ty_gss_j_1af217_list1 ,
      stl_bkpf   TYPE bkpf                  .

  IF s_plist NE space AND s_precs EQ space.
* Borro todo los documentos anulados.
    SELECT *
    FROM bkpf
    INTO TABLE tl_bkpf
    FOR ALL ENTRIES IN gt_doctab
    WHERE bukrs EQ gt_doctab-bukrs
    AND   belnr EQ gt_doctab-belnr
    AND   gjahr EQ gt_doctab-gjahr.

    LOOP AT gt_doctab INTO stl_doctab.
      vl_tabix = sy-tabix.
      READ TABLE tl_bkpf INTO stl_bkpf
      WITH KEY bukrs = stl_doctab-bukrs
               belnr = stl_doctab-belnr
               gjahr = stl_doctab-gjahr.
      IF sy-subrc EQ 0.
        IF stl_bkpf-stblg IS NOT INITIAL.
          DELETE gt_doctab INDEX vl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.


    IF p_vent EQ 'X'.

*     Ordeno las tablas
      SORT gt_doctab BY belnr.
      SORT t_bkpf    BY belnr.
      SORT t_bseg    BY belnr.
      CLEAR:
            vl_tabix   ,
            stl_bseg   ,
            tl_bkpf    ,
            stl_lfa1   ,
            stl_doctab ,
            stl_aux    ,
            stl_bkpf   .

      LOOP AT t_bseg INTO stl_bseg
        WHERE mwskz IS INITIAL
          AND bschl EQ '50'.
        CLEAR stl_doctab.
        READ TABLE t_bkpf INTO stl_bkpf
        WITH KEY bukrs = stl_bseg-bukrs
                 belnr = stl_bseg-belnr
                 gjahr = stl_bseg-gjahr.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING stl_bkpf TO stl_doctab.
          MOVE: stl_bkpf-xblnr   TO stl_doctab-offnum  .
*           to stl_doctab-stcdt

        ENDIF.

        MOVE-CORRESPONDING: stl_bseg       TO stl_doctab.
        CLEAR: stl_doctab-vatam.
        APPEND stl_doctab TO gt_doctab.
      ENDLOOP.
    ENDIF.

*   Ordeno las tablas
    SORT gt_doctab BY belnr.
    SORT t_bkpf    BY belnr.
    SORT t_bseg    BY belnr.
    CLEAR:
          vl_tabix   ,
          stl_bseg   ,
          tl_bkpf    ,
          stl_lfa1   ,
          stl_doctab ,
          stl_aux    ,
          stl_bkpf   .


* Armo el indice de la tabla.
    LOOP AT gt_doctab INTO stl_doctab.
      vl_tabix = sy-tabix.
      CLEAR: stl_doctab-serialno.
      MOVE vl_tabix TO stl_doctab-serialno.
      IF stl_doctab-bukrs_accno IS INITIAL.
        LOOP AT gt_doctab INTO stl_aux
        WHERE  bukrs = stl_doctab-bukrs
        AND    belnr EQ stl_doctab-belnr
        AND    stcd1 NE space.
          IF stl_doctab-bukrs_accno IS INITIAL.
            MOVE stl_aux-bukrs_accno TO stl_doctab-bukrs_accno.
          ENDIF.
          IF stl_doctab-stcd1 IS INITIAL.
            MOVE stl_aux-stcd1 TO stl_doctab-stcd1.
          ENDIF.
          IF stl_doctab-stcdt IS INITIAL.
            MOVE stl_aux-stcdt TO stl_doctab-stcdt.
          ENDIF.
          IF stl_doctab-j_1aoftp IS INITIAL.
            MOVE stl_aux-j_1aoftp TO stl_doctab-j_1aoftp.
          ENDIF.
          EXIT.
        ENDLOOP.
      ENDIF.
      MODIFY gt_doctab FROM stl_doctab INDEX vl_tabix.
    ENDLOOP.

    REFRESH tl_bkpf.
    SELECT *
    FROM bkpf
    INTO TABLE tl_bkpf
    FOR ALL ENTRIES IN gt_doctab
    WHERE bukrs EQ gt_doctab-bukrs
    AND   belnr EQ gt_doctab-belnr
    AND   gjahr EQ gt_doctab-gjahr.

    LOOP AT gt_doctab INTO stl_doctab.
      vl_tabix = sy-tabix.
      READ TABLE tl_bkpf INTO stl_bkpf
    WITH KEY bukrs = stl_doctab-bukrs
             belnr = stl_doctab-belnr
             gjahr = stl_doctab-gjahr.

      IF stl_bkpf-blart EQ 'RE'.
*Si la clase de documento es RE
*tenes que tomar el número de ese RE (BELNR) y entrar BKPF
        SELECT SINGLE *
        FROM rbkp
        INTO stl_rbkp
        WHERE belnr EQ stl_bkpf-awkey(10)
        AND   gjahr EQ stl_bkpf-gjahr .
        IF stl_rbkp-stblg IS NOT INITIAL.
          DELETE gt_doctab INDEX vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.


    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = gv_repid
        i_callback_pf_status_set = gc_setpfstatus
        i_callback_user_command  = gc_user_command
        it_fieldcat              = gt_fieldcat
        i_save                   = gc_value_x
        is_variant               = gs_variant
        it_events                = gt_event
      TABLES
        t_outtab                 = gt_doctab
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.  "IF sy-subrc <> 0.

  ELSEIF s_precs NE space AND s_plist EQ space .

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = gv_repid
        i_callback_pf_status_set = gc_setpfstatus
        i_callback_user_command  = gc_user_command
        it_fieldcat              = gt_fieldcat
        i_save                   = gc_value_x
        is_variant               = gs_variant
        it_events                = gt_event
      TABLES
        t_outtab                 = gt_record_1
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.  "IF sy-subrc <> 0.

  ELSEIF s_plist NE space AND s_precs NE space.

    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = gv_repid
        i_callback_pf_status_set = gc_setpfstatus
        i_callback_user_command  = gc_user_command
        it_fieldcat              = gt_fieldcat
        i_save                   = gc_value_x
        is_variant               = gs_variant
        it_events                = gt_event
        i_tabname_header         = gt_tab_header
        i_tabname_item           = gt_tab_item
        is_keyinfo               = gs_keyinfo
      TABLES
        t_outtab_header          = gt_doctab
        t_outtab_item            = gt_record_1
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF. "IF sy-subrc <> 0.

  ENDIF.  "IF s_precs NE space AND s_plist EQ space .

ENDFORM.                    " OUTPUT


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       To perform double click on line in list
*----------------------------------------------------------------------*
*       --> iv_ucomm  Store Ok code value
*       --> iv_selfield Selection line data
*----------------------------------------------------------------------*
FORM user_command USING  iv_ucomm    TYPE sy-ucomm
                         iv_selfield TYPE slis_selfield.
  DATA vl_ans TYPE c.
  CASE iv_ucomm.

    WHEN  '&IC1' OR 'SELECT1'.
      READ TABLE gt_doctab INTO gs_doctab INDEX iv_selfield-tabindex.
      IF sy-subrc EQ 0.
        MOVE: gs_doctab-belnr TO logtab-belnr,
              gs_doctab-bukrs TO logtab-bukrs,
              gs_doctab-gjahr TO logtab-gjahr.
        MOVE: gs_doctab-belnr TO logtab-belnr,
              gs_doctab-bukrs TO logtab-bukrs,
              gs_doctab-gjahr TO logtab-gjahr.
      ENDIF.
*** Begin of Note 1000003 ***
*      IF doctab-belnr NE space.
*       SET PARAMETER ID 'BLN' FIELD doctab-belnr.
*        SET PARAMETER ID 'BUK' FIELD doctab-bukrs.
*        SET PARAMETER ID 'GJR' FIELD doctab-gjahr.
*** End of Note 1000003 ***
*** Begin of Note 1000003 ***
      IF gs_doctab-belnr NE space.
        SET PARAMETER ID 'BLN' FIELD gs_doctab-belnr.
        SET PARAMETER ID 'BUK' FIELD gs_doctab-bukrs.
        SET PARAMETER ID 'GJR' FIELD gs_doctab-gjahr.
*** End of Note 1000003 ***

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        CLEAR doctab.
      ELSEIF logtab-belnr NE space.
        SET PARAMETER ID 'BLN' FIELD logtab-belnr.
        SET PARAMETER ID 'BUK' FIELD logtab-bukrs.
        SET PARAMETER ID 'GJR' FIELD logtab-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        CLEAR logtab.
      ENDIF.
*-----Modificacion Javier Mona.
    WHEN '&DESC'.

      PERFORM f_popup_to_decide CHANGING vl_ans.
      IF vl_ans EQ 1.
        PERFORM f_gui_download. " USING 'VENTAS'.
      ENDIF.
*    WHEN '&CPRAS'.
*
*      PERFORM f_popup_to_decide CHANGING vl_ans.
*      IF vl_ans EQ 1.
*        PERFORM f_gui_download USING 'COMPRAS'.
*      ENDIF.
*-----Modificacion Javier Mona.
    WHEN 'APPLOG'.
      READ TABLE logtab INDEX iv_selfield-tabindex.
      PERFORM print_log.
  ENDCASE.
  CLEAR iv_selfield.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_DECIDE
*&---------------------------------------------------------------------*
FORM f_popup_to_decide CHANGING po_ans.

  CALL FUNCTION 'POPUP_TO_DECIDE'
    EXPORTING
      defaultoption  = '1'
      textline1      = 'Se descargará el archivo seleccionado'
      textline2      = '¿Desea continuar?'
      text_option1   = 'Si'
      text_option2   = 'No'
      titel          = ''
      start_column   = 25
      start_row      = 6
      cancel_display = ' '
    IMPORTING
      answer         = po_ans.

ENDFORM.                    " F_POPUP_TO_DECIDE
*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*     This subroutine is used to create the log
*----------------------------------------------------------------------*
*  -->  gs_log
*  <--  g_log_handle
*----------------------------------------------------------------------*
FORM create_log .

* For creating the Application log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = gs_log
    IMPORTING
      e_log_handle            = g_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.                               " If sy-subrc <> 0

ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  msg_add_alv
*&---------------------------------------------------------------------*
*       To add the messages to the log
*----------------------------------------------------------------------*
*       --> iv_msgty       Message type(I,W,S etc..)
*       --> iv_text        Text to be printed
*----------------------------------------------------------------------*
FORM msg_add_alv  USING    value(iv_msgty) TYPE symsgty
                           value(iv_text)  TYPE c.

  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
*   I_LOG_HANDLE              =
    i_msgty                   = iv_msgty
    i_text                    = iv_text
   EXCEPTIONS
    log_not_found             = 1
    msg_inconsistent          = 2
    log_is_full               = 3
    OTHERS                    = 4
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.                    " msg_add_alv

*&---------------------------------------------------------------------*
*&      Form  create_profile
*&---------------------------------------------------------------------*
*      <--LS_DISPLAY_PROFILE
*----------------------------------------------------------------------*
FORM create_profile  CHANGING ls_display_profile TYPE bal_s_prof .
*                changing p_gs_display_profile.
  DATA : ls_mess_fcat       TYPE bal_s_fcat.

* get display profile for showing logs in full screen

  CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile
    EXCEPTIONS
      OTHERS              = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR ls_display_profile-use_grid.
  READ TABLE ls_display_profile-mess_fcat INTO  ls_mess_fcat
                            WITH KEY ref_table = 'BAL_S_SHOW'
                                     ref_field = 'T_MSG'.
  IF sy-subrc = 0.
    ls_mess_fcat-outputlen = 132.
    MODIFY ls_display_profile-mess_fcat FROM ls_mess_fcat INDEX sy-tabix
                             TRANSPORTING outputlen.
  ENDIF.

* set optimization of column width
  MOVE 'X' TO ls_display_profile-cwidth_opt .
* Display all messages
  MOVE 'X' TO ls_display_profile-show_all .
* To get the title on the screen
  ls_display_profile-title = sy-title.

ENDFORM.                    " create_profile

*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*  This subroutine is used to display the log
*----------------------------------------------------------------------*
FORM display_log .
  DATA ls_print_options TYPE slis_print_alv.
  IF sy-batch IS INITIAL.
* display log file
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = gs_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    ls_print_options-print = gc_value_x.
    CALL FUNCTION 'BAL_DSP_LOG_PRINT'
     EXPORTING
       i_s_print_options            = ls_print_options
       i_s_list_append              = gc_value_x
       i_s_display_profile          = gs_display_profile
*   I_T_LOG_HANDLE               =
*   I_T_MSG_HANDLE               =
*   I_S_LOG_FILTER               =
*   I_S_MSG_FILTER               =
*   I_T_LOG_CONTEXT_FILTER       =
*   I_T_MSG_CONTEXT_FILTER       =
     EXCEPTIONS
       profile_inconsistent         = 1
       internal_error               = 2
       no_data_available            = 3
       no_authority                 = 4
       OTHERS                       = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " display_log


*&---------------------------------------------------------------------*
*&      Form  variant_init_alv
*&---------------------------------------------------------------------*
* This subroutine is used to initialise the variant
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine
*----------------------------------------------------------------------*
FORM variant_init_alv .

  CLEAR gs_variant.
  gs_variant-report = gv_repid.

ENDFORM.                    " variant_init_alv

*&---------------------------------------------------------------------*
*&      Form  reuse_alv_get_variant
*&---------------------------------------------------------------------*
*  This form has been used to get the possible values for the layout
*----------------------------------------------------------------------*
*      -->IV_REPID  report name
*     <-->XV_VARIA  variant name
*----------------------------------------------------------------------*
FORM reuse_alv_get_variant  USING value(iv_repid) TYPE sy-repid
                            CHANGING xv_varia TYPE slis_vari.

  DATA: ls_variant TYPE disvariant.        " Structure for variant
  MOVE: xv_varia  TO gs_variant-variant,
        iv_repid  TO gs_variant-report.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant
      i_save        = gc_value_x
    IMPORTING
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.                               " IF sy-subrc <> 0

  MOVE ls_variant-variant TO xv_varia.

ENDFORM.                               " reuse_alv_get_variant

*&---------------------------------------------------------------------*
*&      Form  variant_check_alv
*&---------------------------------------------------------------------*
* This subroutine is used to check for the existance of variant
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine
*----------------------------------------------------------------------*
FORM variant_check_alv .

  DATA: ls_variant TYPE disvariant.     " Structure for variant

  IF NOT p_varia IS INITIAL.
    MOVE: gs_variant TO ls_variant,
          p_varia    TO ls_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = gc_value_x
      CHANGING
        cs_variant = ls_variant.
    gs_variant = ls_variant.
  ELSE.
    PERFORM variant_init_alv.
  ENDIF.                               " IF NOT p_varia IS INITIAL

ENDFORM.                               " variant_check_alv

* ALV Changes ends
*&---------------------------------------------------------------------*
*&      Form  F_GUI_DOWNLOAD
*&---------------------------------------------------------------------*
FORM f_gui_download. " USING pi_bajo.
  DATA: p_path   TYPE string,
        flen     TYPE i,
        vl_tabla TYPE char20.
  DO .
* Ruta de bajada.
    PERFORM file_f4 CHANGING p_path.
    IF p_path IS INITIAL.
      MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E' .
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  IF NOT p_comp IS INITIAL.
*---Compras

    PERFORM f_armo_tabla_compas.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize              = flen
        filename                  = p_path
        filetype                  = 'DAT'
        trunc_trailing_blanks_eol = ' '
      CHANGING
        data_tab                  = t_bajada[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        OTHERS                    = 24.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
*---Ventas
    PERFORM f_armo_tabla_ventas.

    IF p_path IS NOT INITIAL AND t_baj_vta IS NOT INITIAL.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize              = flen
          filename                  = p_path
          filetype                  = 'DAT'
          trunc_trailing_blanks_eol = ' '
        CHANGING
          data_tab                  = t_baj_vta[]
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          OTHERS                    = 24.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GUI_DOWNLOAD
*&---------------------------------------------------------------------*
*&  Include           file_f4
*&---------------------------------------------------------------------*
FORM file_f4  CHANGING po_path.
*  DATA:
*    filetable TYPE filetable,
*    rc        TYPE i,
*    v_usr_action TYPE i,
*    v_path TYPE string,
*    v_fullpath TYPE string,
*    v_filename TYPE string.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      initial_directory    = 'C:\'
*    CHANGING
*      filename             = v_filename
*      path                 = v_path
*      fullpath             = v_fullpath
*      user_action          = v_usr_action
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      not_supported_by_gui = 3
*      OTHERS               = 4.
*  IF sy-subrc IS INITIAL.
*    IF v_usr_action EQ cl_gui_frontend_services=>action_ok.
*      MOVE v_fullpath TO po_path.
*    ENDIF.
*  ENDIF.
DATA:
      filename  TYPE string,
      path      TYPE string,
      fullpath  TYPE string,
      p_file    TYPE rlgrap-filename.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select ruta'
      default_extension = 'TXT'
      file_filter       = '*.TXT'
    CHANGING
      filename          = filename
      path              = path
      fullpath          = fullpath.

  IF sy-subrc EQ 0.
    po_path = fullpath.
  ELSE.
    MESSAGE e000(zerror) WITH 'Completar Ruta'  ."DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                                                    "file_f4
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_COMPAS
*&---------------------------------------------------------------------*
FORM f_armo_tabla_compas .

* Modify 03.08.2011 - Estruturas obsoletas - Diego  -----------------
*  DATA:
*        tl_bkpf    TYPE STANDARD TABLE OF bkpf,
*        st_bajada  TYPE ty_out_aux            ,
*        stl_doctab TYPE ty_gss_j_1af217_list1 ,
*        stl_rbkp   TYPE rbkp                  ,
*        vl_tabix   TYPE sy-tabix              ,
*        stl_bkpf   TYPE bkpf                  .
* -------------------------------------------------------------------
DATA:    st_bajada  TYPE ty_out_aux            .
* End of 03.08.2011 ---------------------------------------------------

  CLEAR  : st_bajada,
           st_compras.

  REFRESH: t_bajada.

* Delete 03.08.2011 - Código obsoleto - Diego  -------------------------
*  REFRESH tl_bkpf.
*
*  SELECT *
*  FROM bkpf
*  INTO TABLE tl_bkpf
*  FOR ALL ENTRIES IN gt_doctab
*  WHERE bukrs EQ gt_doctab-bukrs
*  AND   belnr EQ gt_doctab-belnr
*  AND   gjahr EQ gt_doctab-gjahr
*  and   STBLG eq ' '.
*
*  LOOP AT gt_doctab INTO stl_doctab.
*    CLEAR st_bajada .
*    vl_tabix = sy-tabix  .
*
*    READ TABLE tl_bkpf INTO stl_bkpf
*    WITH KEY bukrs = stl_doctab-bukrs
*             belnr = stl_doctab-belnr
*             gjahr = stl_doctab-gjahr.
*
*    IF stl_bkpf-blart EQ 'RE'.
*
*      SELECT SINGLE *
*      FROM rbkp
*      INTO stl_rbkp
*      WHERE belnr EQ stl_bkpf-awkey(10)
*      AND   gjahr EQ stl_bkpf-gjahr .
*
*      IF stl_rbkp-stblg IS NOT INITIAL.
*        DELETE gt_doctab INDEX vl_tabix.
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.
*
**--------Cuit de la empresa
*    SELECT SINGLE paval
*    FROM t001z
*    INTO  st_compras-cuit_emp
*    WHERE bukrs EQ stl_doctab-bukrs.
*
***--------Razón social de la empresa
*    SELECT SINGLE butxt
*    FROM t001
*    INTO  st_compras-razon_emp
*    WHERE bukrs EQ stl_doctab-bukrs.
*
*    SELECT SINGLE j_1aoftp FROM j_1aotdetr INTO st_compras-tipo_comp
*      WHERE land1 = 'AR' AND
*            doccls = stl_doctab-blart AND
*            j_1aprtchr = stl_doctab-offnum+5(1).
*
*
*    IF stl_bkpf-blart EQ 'GB'.
*      v_gb = v_gb + 1 .
*
*      WRITE:      v_gb(4)                TO st_compras-nro_comp(4)      ,
*                  '00000000000000'       TO st_compras-nro_comp+4       ,
*                  stl_doctab-offnum(6)   TO st_compras-nro_comp+18(6)   .
*
*    ELSE.
*      WRITE:      stl_doctab-offnum(4)   TO st_compras-nro_comp(4)      ,
*                  '000000000000'         TO st_compras-nro_comp+4       ,
*                  stl_doctab-offnum+5(8) TO st_compras-nro_comp+16(8)   .
*    ENDIF.
*
*
*    WRITE:
*            stl_doctab-j_1aoftp    TO st_compras-tipo_comp        ,
*            stl_doctab-bldat       TO st_compras-fecha            ,
*            stl_doctab-vatam       TO st_compras-imp_liq          ,
*            stl_doctab-stcd1       TO st_compras-cuit_emp         .
*
**--------Razón social del cliente
*    SELECT SINGLE name1
*    FROM lfa1
*    INTO st_compras-razon_emp
*     WHERE lifnr EQ stl_doctab-lifnr.
**    WHERE stcd1 EQ stl_doctab-stcd1.
*
*    SHIFT st_compras-imp_liq RIGHT DELETING TRAILING space.
*    DO 10 TIMES.
*      REPLACE '.'   WITH ' ' INTO st_compras-imp_liq.
*      REPLACE ','   WITH ' ' INTO st_compras-imp_liq.
*    ENDDO.
*    CONDENSE st_compras-imp_liq NO-GAPS.
*
*    SHIFT st_compras-imp_liq RIGHT DELETING TRAILING ''.
*    IF stl_doctab-vatam > 0.
*      OVERLAY st_compras-imp_liq WITH '000000000000'.
*    ELSE.
*      DO 5 TIMES.
*        REPLACE '-'   WITH ' ' INTO st_compras-imp_liq.
*      ENDDO.
*
*      CONDENSE st_compras-imp_liq NO-GAPS.
*
*      SHIFT st_compras-imp_liq RIGHT DELETING TRAILING ''.
*
*      OVERLAY st_compras-imp_liq WITH '-00000000000'.
*    ENDIF.
*    CONCATENATE st_compras-tipo_comp
*                st_compras-nro_comp
*                st_compras-fecha
*                st_compras-cuit_emp
*                st_compras-razon_emp
*                st_compras-imp_liq
**Begin Insert Javier Mona 3.4.2011
*                '00000000000'
*                '                         '
*                '000000000000'
**End  Insert Javier Mona 3.4.2011
*           INTO st_bajada-linea RESPECTING BLANKS.
*
*    APPEND st_bajada TO t_bajada.
*    CLEAR st_bajada.
*  ENDLOOP.
* End of 03.08.2011 ---------------------------------------------------

* Insert 03.08.2011 - Graba registro de archivo para salida - Diego ---
  LOOP AT t_compras INTO st_compras.

    CONCATENATE st_compras-tipo_comp
                st_compras-nro_comp
                st_compras-fecha
                st_compras-cuit_emp
                st_compras-razon_emp
                st_compras-imp_liq
                '00000000000'
                '                         '
                '000000000000'
           INTO st_bajada-linea RESPECTING BLANKS.

    APPEND st_bajada TO t_bajada.
    CLEAR st_bajada.

  ENDLOOP.

* End of 03.08.2011 -------------------------------------------------

ENDFORM.                    " F_ARMO_TABLA_COMPAS
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_VENTAS
*&---------------------------------------------------------------------*
FORM f_armo_tabla_ventas .
  DATA:
*------> Variables
       vl_tabix    TYPE sy-tabix  ,
       vl_cant(1)  TYPE n         ,
       vl_importe  TYPE char15    ,
       vl_total    TYPE char15    ,
       vl_exento   TYPE char1     ,
       vl_nograb   TYPE char1     ,
       vl_impneto  TYPE char15    ,
       vl_kbetr    TYPE kbetr_tax ,
       vl_iva      TYPE char5     ,
       vl_impuesto TYPE char15    ,
       vl_exentas  TYPE char15    ,
       vl_hwbas    TYPE hwbas_bses,
       vl_hwste    TYPE bset-hwste,
*       vl_HWBAS    type BSET-HWBAS,
      st_bajada    TYPE  ty_vta   ,

*------> Tablas
      tl_auxiliar  TYPE STANDARD TABLE OF ty_auxiliar,
      tl_bseg      TYPE STANDARD TABLE OF bseg      ,
      t_bset_aux   TYPE STANDARD TABLE OF bset,
      tl_venta     TYPE STANDARD TABLE OF ty_ventas  ,
      tl_exentos   TYPE STANDARD TABLE OF ty_ventas  ,
      tl_kna1      TYPE STANDARD TABLE OF kna1       ,

*------> Estructura
      stl_kna1     TYPE kna1                   ,
      stl_bset     TYPE bset                   ,
      sl_bseg      TYPE bseg                   ,
      stl_bseg     TYPE bseg                   ,
      stl_venta    TYPE ty_ventas              ,
      stl_venta1   TYPE ty_ventas              ,
      stl_auxiliar TYPE ty_auxiliar            ,
      stl_doctab   TYPE ty_gss_j_1af217_list1  ,
      stl_aux      TYPE ty_auxiliar            ,
      stl_bkpf     TYPE bkpf                   .
  REFRESH:  t_bset_aux, tl_bseg.
  LOOP AT t_bset INTO stl_bset.
    APPEND  stl_bset TO t_bset_aux.
  ENDLOOP.

  LOOP AT t_bseg INTO sl_bseg.
    APPEND  sl_bseg TO tl_bseg.
  ENDLOOP.

  CLEAR  : st_bajada,
           st_compras.

  REFRESH: t_bajada.

  SORT gt_doctab BY belnr.
*  cambio los datos a la tabla auxiliar, para poder usar el corte de control
  LOOP AT gt_doctab INTO stl_doctab.
    MOVE-CORRESPONDING stl_doctab TO stl_auxiliar.
    APPEND stl_auxiliar TO tl_auxiliar.
  ENDLOOP.

  CLEAR vl_cant.

  SELECT *
  FROM kna1
  INTO TABLE tl_kna1
  FOR ALL ENTRIES IN tl_auxiliar
  WHERE stcd1 EQ tl_auxiliar-stcd1.
* Ordenar la tabla por cuit
  SORT tl_kna1 BY stcd1
                  stcdt.

  LOOP AT tl_auxiliar INTO stl_aux.
    CLEAR vl_exento.
    vl_cant = 1."vl_cant + 1.
    MOVE-CORRESPONDING stl_aux TO stl_doctab.

    WRITE: '1'                    TO stl_venta-registro  ,
* Inicio GB - 10/09/2010
*           stl_doctab-bldat       TO stl_venta-f_compr   ,
* Inicio GB - 10/09/2010
           stl_doctab-belnr       TO stl_venta-belnr     ,
           stl_doctab-j_1aoftp    TO stl_venta-t_compr   ,
           stl_doctab-offnum(4)   TO stl_venta-pto_vta   ,
           stl_doctab-stcdt       TO stl_venta-stcdt     ,
           stl_doctab-stcd1       TO stl_venta-cuit      ,
           stl_doctab-vatam       TO  stl_venta-impuesto .

* Inicio GB - 10/09/2010
    stl_venta-f_compr = stl_doctab-bldat.
* Inicio GB - 10/09/2010

    IF stl_doctab-stcdt IS INITIAL.
      READ TABLE tl_kna1 INTO stl_kna1
      WITH KEY   stcd1 = stl_doctab-stcd1.
      MOVE: stl_kna1-stcdt TO stl_venta-stcdt.
    ENDIF.
*-------- Razón socila del cliente
    PERFORM f_razon_social USING stl_doctab-stcd1
                        CHANGING stl_venta-name1
                                 stl_venta-stcdt.

*-------- Completo con 0 la posicion con espacio
    PERFORM f_completo_ceros_izq USING stl_doctab-offnum+5(8)
                              CHANGING stl_venta-nro_compr.
    PERFORM f_completo_ceros_izq USING stl_doctab-offnum+5(8)
                              CHANGING stl_venta-nro_comprh.
*-------- % IVA alicuota
    PERFORM f_porc_iva USING stl_doctab
                    CHANGING stl_venta-iva .

*-------- Limpio las variables.
    CLEAR: stl_venta-fecha_ret ,
           stl_venta-retencion .

*-------- Impuesto liquido
    PERFORM f_armo_formato_importe USING  stl_doctab-vatam
                                 CHANGING stl_venta-impuesto.
*-------- Importe total de la operación
    PERFORM f_completo_ceros_izq USING '0'
                              CHANGING stl_venta-importe   .
*-------- Total del concepto que no integran el precio neto

    PERFORM f_completo_ceros_izq USING '0'
                              CHANGING stl_venta-total     .
*-------- Importe neto Gravado
    PERFORM f_completo_ceros_izq USING '0'
                              CHANGING stl_venta-impneto   .
*-------- Importe de operacion exenta.
    PERFORM f_completo_ceros_izq USING '0'
                              CHANGING stl_venta-exentas.
*queda darme cuenta cuando es cero como buscar los datos de la bseg
*y tambien queda ver el totalizado en la ultima linea.
*Ademas queda
    AT END OF belnr .


      CLEAR: stl_venta-iva,
             vl_exento         ,
             stl_venta-total   ,
             stl_venta-exentas ,
             stl_venta-importe ,
             stl_venta-impneto ,
             stl_venta-impuesto.
*-------- Iva
      CONDENSE vl_iva NO-GAPS.
      WRITE vl_iva TO stl_venta-iva LEFT-JUSTIFIED.
      OVERLAY  stl_venta-iva WITH '0000'.
*-------- Impuesto liquido
      PERFORM f_armo_formato_importe USING  stl_doctab-vatam
                                   CHANGING stl_venta-impuesto.
*-------- Importe total de la operación
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-importe   .
*-------- Total del concepto que no integran el precio neto

      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-total     .
*-------- Importe neto Gravado
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-impneto   .
*-------- Importe de operacion exenta.
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-exentas.

      MOVE vl_cant TO stl_venta-alicuota  .

*      Pasar los totales...
      READ TABLE t_bseg INTO stl_bseg
      WITH KEY koart = 'D'
               bukrs = stl_doctab-bukrs
               belnr = stl_doctab-belnr
               gjahr = stl_doctab-gjahr.
      IF sy-subrc EQ 0.

*-------- Importe total de la operación
        PERFORM f_formato_importe USING  stl_bseg-pswbt
                               CHANGING stl_venta-importe   .
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM t_bset COMPARING  bukrs
                                                        belnr
                                                        gjahr
                                                        buzei
                                                        mwskz.
      SORT t_bset BY  bukrs
                      belnr
                      gjahr
                      mwskz.




*-------- Importe neto Gravado
      CLEAR vl_hwbas.
      LOOP AT t_bset INTO stl_bset
        WHERE bukrs = stl_doctab-bukrs
        AND   belnr = stl_doctab-belnr
        AND   gjahr = stl_doctab-gjahr
        AND   mwskz NE 'DB'
        AND   mwskz NE 'D0'
        AND   fwste NE 0
        AND   ktosl EQ 'MWS'
        AND   hwbas >  0.
        DELETE t_bset INDEX sy-tabix.

        vl_hwbas = stl_bset-hwbas." + vl_hwbas.
        vl_hwste = stl_bset-hwste." + vl_hwste.

*-------- Importe total de la operación
        PERFORM f_completo_ceros_izq USING '0'
                                  CHANGING stl_venta-importe   .
*-------- Total del concepto que no integran el precio neto

        PERFORM f_completo_ceros_izq USING '0'
                                  CHANGING stl_venta-total     .
*-------- Importe neto Gravado
        PERFORM f_completo_ceros_izq USING '0'
                                  CHANGING stl_venta-impneto   .
*-------- Importe de operacion exenta.
        PERFORM f_completo_ceros_izq USING '0'
                                  CHANGING stl_venta-exentas.

        PERFORM f_formato_importe USING stl_bseg-dmbtr"vl_hwbas
                               CHANGING stl_venta-importe.

        PERFORM f_formato_importe USING vl_hwbas
                               CHANGING stl_venta-impneto.

        PERFORM f_formato_importe USING vl_hwste
                               CHANGING stl_venta-impuesto."total     .

*-------- % IVA alicuota
        vl_kbetr = stl_bset-kbetr / 10.
        WRITE vl_kbetr TO vl_iva RIGHT-JUSTIFIED.
        DO 3 TIMES.
          REPLACE '.'   WITH ' ' INTO  vl_iva.
          REPLACE ','   WITH ' ' INTO  vl_iva.
        ENDDO.

        CONDENSE vl_iva NO-GAPS.
        WRITE vl_iva TO stl_venta-iva LEFT-JUSTIFIED.
        OVERLAY  stl_venta-iva WITH '0000'.

*-------- Importe de operacion exenta.
      CLEAR: vl_hwbas,
             vl_hwste.
        LOOP AT t_bset INTO stl_bset
        WHERE bukrs = stl_doctab-bukrs
        AND   belnr = stl_doctab-belnr
        AND   gjahr = stl_doctab-gjahr
        AND   hwbas > 0
        AND ( mwskz EQ 'D0'
         OR   mwskz EQ 'SD' AND fwste EQ 0 and ktosl EQ 'MWS' ) .
*          vl_exento = 'L'.
          DELETE t_bset INDEX sy-tabix.
          vl_hwbas = stl_bset-hwbas + vl_hwbas.

        ENDLOOP.

        PERFORM f_formato_importe USING  vl_hwbas
                                        CHANGING stl_venta-exentas.

      CLEAR: vl_hwbas,
             vl_hwste.
*-------- Importe de operacion exenta pero de SD.
        LOOP AT t_bseg INTO sl_bseg
          WHERE bukrs = stl_doctab-bukrs
          AND   belnr = stl_doctab-belnr
          AND   gjahr = stl_doctab-gjahr
          AND   buzid EQ  ' '
          AND   koart EQ 'S'
          AND ( mwskz NE '**' AND
                mwskz NE 'SD'  ).
          DELETE t_bseg INDEX sy-tabix.
*        IF sl_bseg-mwskz = 'DB' OR  sl_bseg-mwskz IS INITIAL.
          IF sl_bseg-mwskz IS INITIAL.
            vl_hwbas  = sl_bseg-dmbtr  + vl_hwbas.
          ENDIF.

        ENDLOOP.

      PERFORM f_formato_importe USING  vl_hwbas
                                      CHANGING stl_venta-total.

      CLEAR: vl_hwbas,
             vl_hwste.
*-------- Impuesto liquido
        LOOP AT t_bset INTO stl_bset
          WHERE bukrs = stl_doctab-bukrs
          AND   belnr = stl_doctab-belnr
          AND   gjahr = stl_doctab-gjahr
          AND   mwskz = 'DB'
          AND   hwbas > 0.
          DELETE t_bset INDEX sy-tabix.
          vl_hwbas = stl_bset-hwbas." + vl_hwbas.
          vl_hwste = stl_bset-hwste." + vl_hwste.

          PERFORM f_formato_importe USING stl_bseg-dmbtr"vl_hwbas
                                 CHANGING stl_venta-importe.

          PERFORM f_formato_importe USING vl_hwbas
                                 CHANGING stl_venta-total.

        ENDLOOP.


        APPEND stl_venta TO tl_venta.
        CLEAR: stl_venta-iva     ,
               vl_hwbas          ,
               vl_hwste          ,
               stl_venta-total   ,
               vl_exento         ,
               stl_venta-exentas ,
               stl_venta-importe ,
               stl_venta-impneto ,
               stl_venta-impuesto. .

      ENDLOOP.

*-------- Impuesto liquido
      CLEAR: vl_hwbas,
             vl_hwste,
             vl_nograb,
             stl_venta-iva.
*-------- IVA
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-iva.

      LOOP AT t_bset INTO stl_bset
        WHERE bukrs = stl_doctab-bukrs
        AND   belnr = stl_doctab-belnr
        AND   gjahr = stl_doctab-gjahr
        AND   mwskz = 'DB'
        AND   hwbas > 0.
        DELETE t_bset INDEX sy-tabix.
        vl_nograb = 'X'.
        vl_hwbas = stl_bset-hwbas + vl_hwbas.
        vl_hwste = stl_bset-hwste + vl_hwste.

        PERFORM f_formato_importe USING stl_bseg-dmbtr"vl_hwbas
                               CHANGING stl_venta-importe.

        PERFORM f_formato_importe USING vl_hwbas
                               CHANGING stl_venta-total.
      ENDLOOP.

      IF  vl_nograb   EQ 'X'.
        APPEND stl_venta TO tl_venta.

        CLEAR: stl_venta-iva,
               vl_exento         ,
               vl_nograb         ,
               stl_venta-total   ,
               stl_venta-exentas ,
               stl_venta-importe ,
               stl_venta-impneto ,
               stl_venta-impuesto.
      CLEAR: vl_hwbas,
             vl_hwste.
      ENDIF.

*-------- Importe de operacion exenta.
      CLEAR: vl_hwbas, vl_exento.
      LOOP AT t_bset INTO stl_bset
      WHERE bukrs = stl_doctab-bukrs
      AND   belnr = stl_doctab-belnr
      AND   gjahr = stl_doctab-gjahr
      AND   hwbas > 0
      AND ( mwskz EQ 'D0'
       OR   mwskz EQ 'SD' AND fwste EQ 0 and ktosl EQ 'MWS' ) .
        vl_exento = 'X'.
        vl_hwbas = stl_bset-hwbas + vl_hwbas.
        DELETE t_bset INDEX sy-tabix.
      ENDLOOP.

      PERFORM f_formato_importe USING  vl_hwbas
                                      CHANGING stl_venta-exentas.

      IF vl_exento = 'X'.
        APPEND stl_venta TO tl_venta.
        CLEAR: vl_exento,
               vl_hwbas ,
               vl_hwste .
      ENDIF.

*-------- Importe de operacion exenta pero de SD.
      LOOP AT t_bseg INTO sl_bseg
        WHERE bukrs = stl_doctab-bukrs
        AND   belnr = stl_doctab-belnr
        AND   gjahr = stl_doctab-gjahr
        AND   buzid EQ  ' '
        AND   koart EQ 'S'
        AND ( mwskz NE '**' AND
              mwskz NE 'SD'  ).
        DELETE t_bseg INDEX sy-tabix.
*        IF sl_bseg-mwskz = 'DB' OR  sl_bseg-mwskz IS INITIAL.
        IF   sl_bseg-mwskz IS INITIAL.
          vl_exento = 'X'.
          vl_hwbas  = sl_bseg-dmbtr  + vl_hwbas.
        ENDIF.

      ENDLOOP.
*-------- Importe total de la operación
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-importe   .
*-------- Total del concepto que no integran el precio neto

      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-total     .
*-------- Impuestos
      PERFORM f_formato_importe USING '0'
                             CHANGING stl_venta-impuesto."total     .

*-------- Importe neto Gravado
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-impneto   .
*-------- Importe de operacion exenta.
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-exentas.

*-------- IVA
      PERFORM f_completo_ceros_izq USING '0'
                                CHANGING stl_venta-iva.

*-------- Importe de operacion exenta.
      PERFORM f_formato_importe USING  vl_hwbas
                                      CHANGING stl_venta-total.

      PERFORM f_formato_importe USING stl_bseg-dmbtr
                             CHANGING stl_venta-importe.

      IF vl_exento = 'X'.
        APPEND stl_venta TO tl_venta.
      ENDIF.
      CLEAR: stl_venta,
             vl_hwbas ,
             vl_hwste .
    ENDAT.
     CLEAR: stl_venta  ,
            vl_tabix   ,
            vl_cant(1) ,
            vl_importe ,
            vl_total   ,
            vl_exento  ,
            vl_nograb  ,
            vl_impneto ,
            vl_kbetr   ,
            vl_iva     ,
            vl_impuesto,
            vl_exentas ,
            vl_hwbas   ,
            vl_hwste   ,
            vl_HWBAS  .

  ENDLOOP.


  SORT tl_venta BY  belnr .
*-------------------------------------------------------------------*

*-----Armo la tabla de bajada del archivo.
  LOOP AT tl_venta INTO stl_venta.
    CONCATENATE stl_venta-registro
                stl_venta-f_compr
                stl_venta-t_compr
* Inicio GB - 10/09/2010
                ' ' " Controlador Fiscal
* Fin GB - 10/09/2010
                stl_venta-pto_vta
                stl_venta-nro_compr
                stl_venta-nro_comprh
                stl_venta-stcdt
                stl_venta-cuit
                stl_venta-name1
                stl_venta-importe
                stl_venta-total
                stl_venta-impneto
                stl_venta-iva
                stl_venta-impuesto
* Inicio GB - 10/09/2010
                '000000000000000' " Impuesto liquidado a RNI o perc. a no categoriz.
* Fin GB - 10/09/2010
                stl_venta-exentas
* Inicio GB - 10/09/2010
                '000000000000000' " Importe de percep. o pagos a cta de impuesto nac.
                '000000000000000' " Importe de percepciones de Ingresos Brutos
                '000000000000000' " Importe de percepciones de Ingresos Municipales
                '000000000000000' " Importe de impuestos internos
                '00'              " Tipo de responsable
                '   '             " Código de moneda
                '0000000000'      " Tipo de cambio
* Fin GB - 10/09/2010
                stl_venta-alicuota
* Inicio GB - 10/09/2010
                ' '               " Código de operación
                '00000000000000'  " CAI
                '00000000'        " Fecha de Vencimiento
                '00000000'        " Fecha de anulación del comprobante
                '                                                                           ' " Info.
* Fin GB - 10/09/2010
                stl_venta-fecha_ret
                stl_venta-retencion
           INTO st_bajada-linea RESPECTING BLANKS.


    APPEND st_bajada TO t_baj_vta.
  ENDLOOP.

  REFRESH: t_bset, t_bseg.
  LOOP AT t_bset_aux INTO stl_bset.
    APPEND  stl_bset TO t_bset.
  ENDLOOP.

  LOOP AT tl_bseg INTO sl_bseg.
    APPEND  sl_bseg TO t_bseg.
  ENDLOOP.

ENDFORM.                    " F_ARMO_TABLA_VENTAS
*&---------------------------------------------------------------------*
*&      Form  F_RAZON_SOCIAL
*&---------------------------------------------------------------------*
FORM f_razon_social  USING    pi_stcd1
                     CHANGING po_name1
                              po_stcdt.
  SELECT SINGLE name1 stcdt
  FROM kna1
  INTO (po_name1, po_stcdt)
  WHERE stcd1 EQ pi_stcd1.
ENDFORM.                    " F_RAZON_SOCIAL
*&---------------------------------------------------------------------*
*&      Form  F_COMPLETO_CEROS_IZQ
*&---------------------------------------------------------------------*
FORM f_completo_ceros_izq  USING    pi_nro_compr
                           CHANGING po_nro_compr.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = pi_nro_compr
*    IMPORTING
*      output = po_nro_compr.

  WRITE    pi_nro_compr TO po_nro_compr.
  SHIFT    po_nro_compr RIGHT DELETING TRAILING ''.
  OVERLAY  po_nro_compr  WITH '0000000000000000'.

ENDFORM.                    " F_COMPLETO_CEROS_IZQ
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_FORMATO_IMPORTE
*&---------------------------------------------------------------------*
FORM f_armo_formato_importe  USING    pi_vatam
                             CHANGING po_impuesto.

  IF pi_vatam < 0.
    pi_vatam = pi_vatam * - 1 .
  ENDIF.

  MOVE pi_vatam TO po_impuesto.
  SHIFT  po_impuesto RIGHT DELETING TRAILING space.

  DO 10 TIMES.
    REPLACE '.'   WITH ' ' INTO  po_impuesto.
    REPLACE ','   WITH ' ' INTO  po_impuesto.
  ENDDO.

  CONDENSE  po_impuesto NO-GAPS.

  SHIFT  po_impuesto RIGHT DELETING TRAILING ''.

*  IF ( po_impuesto > 0 ) OR ( pi_vatam <> 0 ).
*    OVERLAY  po_impuesto WITH '-00000000000'.
*  ELSE.

  DO 5 TIMES.
    REPLACE '-'   WITH ' ' INTO  po_impuesto.
  ENDDO.

  CONDENSE  po_impuesto NO-GAPS.

  SHIFT  po_impuesto RIGHT DELETING TRAILING ''.

  OVERLAY  po_impuesto  WITH '0000000000000000'.
*  ENDIF.



ENDFORM.                    " F_ARMO_FORMATO_IMPORTE
*&---------------------------------------------------------------------*
*&      Form  F_PORC_IVA
*&---------------------------------------------------------------------*
FORM f_porc_iva  USING pl_doctab TYPE ty_gss_j_1af217_list1
              CHANGING po_iva.
  DATA: sl_bset  TYPE bset,
        vl_kbetr TYPE kbetr_tax,
        vl_iva   TYPE char5.

  READ TABLE t_bset INTO sl_bset
  WITH KEY bukrs = pl_doctab-bukrs
           belnr = pl_doctab-belnr
           gjahr = pl_doctab-gjahr
           buzei = pl_doctab-buzei.

  vl_kbetr = sl_bset-kbetr / 10.
  WRITE vl_kbetr TO vl_iva RIGHT-JUSTIFIED.
  DO 3 TIMES.
    REPLACE '.'   WITH ' ' INTO  vl_iva.
    REPLACE ','   WITH ' ' INTO  vl_iva.
  ENDDO.

  CONDENSE vl_iva NO-GAPS.
  WRITE vl_iva TO po_iva LEFT-JUSTIFIED.
  OVERLAY  po_iva WITH '0000'.
ENDFORM.                    " F_PORC_IVA
*&---------------------------------------------------------------------*
*&      Form  F_FORMATO_IMPORTE
*&---------------------------------------------------------------------*
FORM f_formato_importe  USING pi_vatam
                     CHANGING po_impuesto.

  MOVE pi_vatam TO po_impuesto.
  SHIFT  po_impuesto RIGHT DELETING TRAILING space.

  DO 10 TIMES.
    REPLACE '.'   WITH ' ' INTO  po_impuesto.
    REPLACE ','   WITH ' ' INTO  po_impuesto.
  ENDDO.

  CONDENSE  po_impuesto NO-GAPS.

  SHIFT  po_impuesto RIGHT DELETING TRAILING ''.

  IF po_impuesto > 0.
    OVERLAY  po_impuesto WITH '0000000000000000'.
  ELSE.

    DO 5 TIMES.
      REPLACE '-'   WITH ' ' INTO  po_impuesto.
    ENDDO.

    CONDENSE  po_impuesto NO-GAPS.

    SHIFT  po_impuesto RIGHT DELETING TRAILING ''.

*    OVERLAY  po_impuesto  WITH '-00000000000'.
    OVERLAY  po_impuesto  WITH '0000000000000000'.
  ENDIF.


ENDFORM.                    " F_FORMATO_IMPORTE

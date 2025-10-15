
*&---------------------------------------------------------------------*
*&  Include           ZFIY0015_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FILL_BSEG_TO_TAB_BSEG
*&---------------------------------------------------------------------*
*  -->            bkpf,bseg
*  <--            tab_bseg     ( D/K    lines)
*                 tab_bseg_nk  ( no D/K lines )
*----------------------------------------------------------------------*
FORM fill_bseg_to_tab_bseg.
  CLEAR: tab_bseg.
* Signs according to report type
  IF NOT par_vers IS INITIAL.
    IF tab_j_1adrver-j_1aproc = 1 OR
       tab_j_1adrver-j_1aproc = 3.     " input / purchase
      IF bseg-shkzg = 'H'.
        bseg-dmbtr = bseg-dmbtr * -1.
        bseg-mwsts = bseg-mwsts * -1.
      ENDIF.
    ELSEIF bseg-shkzg = 'S'.           " output / sales
      bseg-dmbtr = bseg-dmbtr * -1.
      bseg-mwsts = bseg-mwsts * -1.
    ENDIF.
  ELSE.
    IF bseg-shkzg = 'H'.
      bseg-dmbtr = bseg-dmbtr * -1.
      bseg-mwsts = bseg-mwsts * -1.
    ENDIF.
  ENDIF.

* all lines ( no D/K )
  IF bseg-koart NE 'D' AND
     bseg-koart NE 'K' AND
     bseg-vbeln IS INITIAL.
    MOVE-CORRESPONDING bseg TO tab_bseg_nodk.
    APPEND tab_bseg_nodk.
  ELSE.
* = D or = K
    MOVE-CORRESPONDING bseg TO tab_bseg.

* get KUNNR for client (cash sale)            "Note 921032
    IF bseg-koart EQ 'S' AND NOT bseg-vbeln IS INITIAL.
      SELECT SINGLE * FROM vbrk
         WHERE vbeln = bseg-vbeln.
      IF sy-subrc = 0.
        bseg-kunnr = vbrk-kunrg.           "payer number
      ENDIF.
      SELECT SINGLE * FROM kna1
         WHERE kunnr = bseg-kunnr.
      tab_bseg-xcpdk = kna1-xcpdk.
      tab_bseg-fityp = kna1-fityp.
      IF kna1-xcpdk <> space.
        tab_bseg-ktnra = bseg-kunnr.
      ELSEIF kna1-fiskn <> space.
        tab_bseg-ktnra = kna1-fiskn.
      ELSE.
        tab_bseg-ktnra = bseg-kunnr.
      ENDIF.

* get the KUNNR/LIFNR for the name information
    ELSEIF bseg-koart EQ 'D'.
      SELECT SINGLE * FROM kna1
        WHERE kunnr = bseg-kunnr.
      tab_bseg-xcpdk = kna1-xcpdk.
      tab_bseg-fityp = kna1-fityp.
      IF kna1-xcpdk <> space.
        tab_bseg-ktnra = bseg-kunnr.
      ELSEIF kna1-fiskn <> space.
        tab_bseg-ktnra = kna1-fiskn.
      ELSE.
        tab_bseg-ktnra = bseg-kunnr.
      ENDIF.
* Vendor
    ELSEIF bseg-koart EQ 'K'.
      SELECT SINGLE * FROM lfa1
        WHERE lifnr = bseg-lifnr.
      tab_bseg-xcpdk = lfa1-xcpdk.
      tab_bseg-fityp = lfa1-fityp.
      IF lfa1-xcpdk <> space.
        tab_bseg-ktnra = bseg-lifnr.
      ELSEIF lfa1-fiskn <> space.
        tab_bseg-ktnra = lfa1-fiskn.
      ELSE.
        tab_bseg-ktnra = bseg-lifnr.
      ENDIF.
    ENDIF.                             " bseg-koart eq 'K'.
    APPEND tab_bseg.
  ENDIF.
ENDFORM. " FILL_BSEG_TO_TAB_BSEG

*&---------------------------------------------------------------------*
*&      Form  FILL_BSET_TO_TAB_BSET
*&---------------------------------------------------------------------*
*  -->            bkpf,bset
*  <--            tab_bset     ( tax lines )
*----------------------------------------------------------------------*
FORM fill_bset_to_tab_bset.

*  read TAX customizing data
  PERFORM:
     read_t007b USING bset-ktosl,      " Processing key
     read_j_1ataxid USING tab_001-kalsm bset-ktosl. " Tax identification
  CHECK: flg_reject_doc IS INITIAL.

* Signs according to report type
  IF NOT par_vers IS INITIAL.
    IF tab_j_1adrver-j_1aproc = 1 OR
       tab_j_1adrver-j_1aproc = 3.     " input / purchase
      IF bset-shkzg = 'H'.
        bset-hwbas = bset-hwbas * -1.
        bset-hwste = bset-hwste * -1.
      ENDIF.
    ELSEIF bset-shkzg = 'S'.           " output / sales
      bset-hwbas = bset-hwbas * -1.
      bset-hwste = bset-hwste * -1.
    ENDIF.
  ELSE.
    IF bset-shkzg = 'H'.
      bset-hwbas = bset-hwbas * -1.
      bset-hwste = bset-hwste * -1.
    ENDIF.
  ENDIF.

* Set Rate (condition amount or percentage)
  IF  bset-kbetr = 0 AND
  NOT bset-knumh IS INITIAL.
    PERFORM read_konp USING bset-knumh." Condition (position)
    bset-kbetr = tab_konp-kbetr.
  ENDIF.

* fill ranges with processing key to check persistent processing key
  CLEAR: ran_ktosl_kts1.
  ran_ktosl_kts1-low  = bset-ktosl.
  ran_ktosl_kts1-option = 'EQ'.
  ran_ktosl_kts1-sign   = 'I'.
  APPEND ran_ktosl_kts1.

* fill tab_bset fields.
  CLEAR: tab_bset.
  MOVE-CORRESPONDING bset TO tab_bset.
* tax code attributes
  tab_bset-mwart = tab_007a-mwart.
  tab_bset-zmwsk = tab_007a-zmwsk.
  tab_bset-stgrp = tab_007b-stgrp.
  tab_bset-stazf = tab_007b-stazf.
  tab_bset-regio = tab_j_1ataxid-regio.
  tab_bset-j_1ataxid  = tab_j_1ataxid-j_1ataxid.

* set posnr
  IF flg_is_beleg = 'S'.               " SD document
    tab_bset-xkposn = tab_vbrp-posnr.  "xkposn.
    tab_bset-j_1arfz = tab_vbrp-j_1arfz.
  ELSE.
    tab_bset-xkposn = tab_bset-txgrp.
    tab_bset-j_1arfz = tab_007a-j_1arfz.
  ENDIF.

  APPEND tab_bset.
ENDFORM. " FILL_BSET_TO_TAB_BSET

*&---------------------------------------------------------------------*
*&      Form  READ_SD_INVOICE_DATA
*&---------------------------------------------------------------------*
FORM read_sd_invoice_data.
* Note 586283: Special check for rebate credit memos
  DATA: lf_shkzg TYPE vbrp-shkzg,
        l_kvsl1  LIKE t683s-kvsl1.        " Note 789418

  xvbeln = bkpf-awkey(10).

  IF bkpf-adisc = 'S'.                 " SD discount
    CLEAR *bkpf.
    SELECT SINGLE * FROM bkpf INTO *bkpf
                        WHERE bukrs = bkpf-bukrs
                        AND   belnr = sd_docm-belnr
                        AND   gjahr = sd_docm-gjahr.

    xvbeln = *bkpf-awkey(10).
  ENDIF.

  SELECT SINGLE * FROM vbrk WHERE vbeln = xvbeln.
*  check sy-subrc = 0.
  IF sy-subrc NE 0.
* error: SD document does not exist
    CLEAR:  tab_log_entry2.
    tab_log_entry2-belnr    =  xvbeln.
    tab_log_entry2-description   =  TEXT-v82.
    COLLECT tab_log_entry2.
    EXIT.
  ENDIF.

* Begin Note 586283 - Rebate credit memos: Fill Indicator for returns!
  CLEAR lf_shkzg.
  IF NOT vbrk-knuma IS INITIAL.
    lf_shkzg = 'R'.   " vbrp-shkzg normally never is filled with 'R'
  ENDIF.
* End   Note 586283

* For magnetic output: Get correct number of pages.
  IF NOT par_magn IS INITIAL AND
     bkpf-numpg IS INITIAL.
    bkpf-numpg = vbrk-numpg.
  ENDIF.

* Reversed SD documents
  IF bkpf-awtyp = 'VBRK'.
    IF NOT vbrk-sfakn IS INITIAL OR vbrk-fksto NE space.
      PERFORM get_fi_cancel_docm.
    ENDIF.
  ENDIF.

* tab_bset_taxed, tab_taxes will be filled with SD taxes ( KONV) .
  REFRESH: tab_konv, tab_vbrp, tab_bset.
  REFRESH: sd_reasons, line_total.
  CLEAR: sd_reasons_set.
  SELECT FROM v_konv FIELDS * WHERE knumv = @vbrk-knumv AND koaid = 'D' AND kstat = @space INTO CORRESPONDING FIELDS OF @konv .

* Note 1037583 Start
* If the account key is initial and accruals
* account key is maintianed then it has been
* considered for processing
    IF konv-kvsl1 IS INITIAL
    AND NOT konv-kvsl2 IS INITIAL.
      konv-kvsl1 = konv-kvsl2.
    ENDIF.
* Note 1037583 End

    PERFORM check_tax_code USING konv-mwsk1.
    CHECK rcode = 0.

    IF bkpf-adisc = 'S'.
* field is not filled, so fill it from the SD document invoice
      konv-waers = *bkpf-waers.
    ELSE.
      konv-waers = bkpf-waers.
    ENDIF.

    xkursf = bkpf-kursf.

    ON CHANGE OF konv-knumv OR konv-kposn.
      CLEAR vbrp.
      SELECT SINGLE * FROM vbrp WHERE vbeln = vbrk-vbeln
                                AND   posnr = konv-kposn.
      PERFORM check_foreign_sd_docs.                   " Note 530252
      CHECK rcode = 0.                                 " Note 530252

      IF vbrp-j_1arfz NE space.
        sd_reasons_set = 'X'.
        MOVE-CORRESPONDING vbrp TO sd_reasons.
        APPEND sd_reasons.
      ENDIF.

      IF tab_001-waers NE bkpf-waers.
        PERFORM get_local_amnt USING vbrp-netwr.
        PERFORM get_local_amnt USING vbrp-mwsbp.
      ENDIF.

* Begin Note 586283 - Rebate credit memos: Fill Indicator for returns!
      IF NOT lf_shkzg IS INITIAL.
        vbrp-shkzg = lf_shkzg.
      ENDIF.
* End   Note 586283

*Note 789418 Starts

      SELECT SINGLE kvsl1 INTO l_kvsl1 FROM prcd_elements
      WHERE knumv = vbrk-knumv
      AND kposn = konv-kposn
      AND koaid = 'A' " SURCHARGE/DISCOUNT
      AND kstat = space.

      IF sy-subrc = 0.

* Perform READ_T007B only for taxes
        DATA lv_kappl TYPE kappl.
        SELECT SINGLE kappl INTO lv_kappl
        FROM t687 WHERE kvsl1 = l_kvsl1.
        IF sy-subrc = 0 AND lv_kappl = 'TX'.
          PERFORM:
          read_t007b USING l_kvsl1, " Processing key
          read_j_1ataxid USING tab_001-kalsm l_kvsl1.
          IF tab_j_1ataxid-j_1ataxid(2) = 'VN'
          AND vbrp-netwr <> konv-kawrt.
            g_surc_disc = 'X'.
            g_sd_amt = vbrp-netwr - konv-kawrt.
            vbrp-netwr = konv-kawrt.
            IF g_sd_amt < 0.
              g_sd_amt = g_sd_amt * -1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*Note 789418 Ends

* Signs according to report type  commend because in tab_bset.
      IF NOT par_vers IS INITIAL.
        IF tab_j_1adrver-j_1aproc = 1 OR
           tab_j_1adrver-j_1aproc = 3. " input / purchase
          IF vbrp-shkzg IS INITIAL.
            vbrp-netwr = vbrp-netwr * -1.
            vbrp-mwsbp = vbrp-mwsbp * -1.
          ENDIF.
        ELSEIF vbrp-shkzg NE space.    " output / sales
          vbrp-netwr = vbrp-netwr * -1.
          vbrp-mwsbp = vbrp-mwsbp * -1.
        ENDIF.
      ELSE.
        IF vbrp-shkzg IS INITIAL.
          vbrp-netwr = vbrp-netwr * -1.
          vbrp-mwsbp = vbrp-mwsbp * -1.
        ENDIF.
      ENDIF.

      IF bkpf-adisc NE 'S'.            " not for SD discount
        CLEAR line_total.
        line_total-mwskz = konv-mwsk1.
        line_total-kposn = vbrp-posnr.
        line_total-dmbtr = vbrp-netwr + vbrp-mwsbp.
        COLLECT line_total.
      ENDIF.

      MOVE-CORRESPONDING vbrp TO tab_vbrp.
      tab_vbrp-mwskz = konv-mwsk1.
      APPEND tab_vbrp.

    ENDON.

    IF tab_001-waers NE konv-waers.
* sd invoice in foreign curr.
      IF bkpf-adisc EQ 'S'.
* sd discount document
        IF tab_001-waers   EQ bkpf-waers OR
       ( tab_001-waers NE bkpf-waers AND konv-waers NE bkpf-waers ).
* sd discount document not posted in compaies and sd invoice curr.
* so clear the calculated exchange rate to read the tcurr table
          CLEAR xkursf.
        ENDIF.
      ENDIF.
      PERFORM get_local_amnt USING konv-kwert.
      PERFORM get_local_amnt USING konv-kawrt.
    ENDIF.

    MOVE-CORRESPONDING konv TO tab_konv.
    IF konv-kinak IS INITIAL
    AND NOT konv-mwsk1 IS INITIAL.
      APPEND tab_konv.
      PERFORM fill_bset.
      PERFORM fill_bset_to_tab_bset.
    ENDIF.
  ENDSELECT.
ENDFORM. " READ_SD_INVOICE_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_MM_INVOICE_DATA
*&---------------------------------------------------------------------*
FORM read_mm_invoice_data.
  CLEAR: xmbelnr, xmgjahr, mm_doc.
  REFRESH: mm_doc.
  xmbelnr = bkpf-awkey(10).
  xmgjahr = bkpf-awkey+10(4).

* get the mm-document
  SELECT SINGLE * FROM rbkp WHERE belnr = xmbelnr
                            AND   gjahr = xmgjahr.
  CHECK sy-subrc EQ 0.

  CHECK NOT rbkp-stblg IS INITIAL.     " reversed documents only

* Due to format change
  xmgjahr2 = rbkp-stjah.

  CALL FUNCTION 'AC_DOCUMENT_RECORD'
    EXPORTING
      i_awtyp      = 'RMRP'
      i_awref      = rbkp-stblg
      i_aworg      = xmgjahr2
      x_dialog     = not_dialog
    TABLES
      t_documents  = mm_doc
    EXCEPTIONS
      no_reference = 1
      no_document  = 2
      OTHERS       = 3.

  LOOP AT mm_doc WHERE awtyp = 'BKPF'.
    bkpf-stblg = mm_doc-docnr.
  ENDLOOP.
ENDFORM. " READ_MM_INVOICE_DATA

*&---------------------------------------------------------------------*
*&      Form  FILL_TAB_TAXES
*       tab_taxes contains all the  tax amounts
*----------------------------------------------------------------------*
*  -->  p1        bkpf,tab_bset, flg_is_beleg
*  <--  p2        tab_taxes,
*----------------------------------------------------------------------*
*  TAXID                                | AccKey    |
* tab_j_1ataxid-j_1ataxid.              | BSET-KTOSL|
*-----------------------------------------------------------------------
* TX01     : Input Tax                  | VST       |
* TX02     : Input Tax                  | VST       |
* VL01     : VAT liberation             | J1E       |
* VL02     : VAT liberation             | J1E       |
* VN01:     (Not taxed concept)         | J1I       |
* VN01:     (Not taxed concept)         | J1N       |
* VS01:      Surcharge                  | J1C       |
* VP00/....: Perceptions                | J1G       |
* GP00/....: Regional taxes             | J1J       |
* IT01     : Internal tax               | J1A       |
* IT02     : Internal tax               | J1A       |
* EP01:      ET Perc. Customs           | J1H       |
* EX01       Exports                    | J1X       |
* NC01       Perc. not categorized      | J2E       |
* MP01       Municipal Perception
*----------------------------------------------------------------------*
FORM fill_tab_taxes.
* local declaration for correct display of some MM documents
  DATA: BEGIN OF lcl_tab_mwskz OCCURS 1,
          mwskz LIKE bseg-mwskz,
        END OF lcl_tab_mwskz.

  REFRESH: lcl_tab_mwskz.
  REFRESH: tab_taxes, tax_base, dis_taxes, sum_tab_taxes.
  CLEAR: dis_taxes, tax_base.
* fill tab_taxes
  LOOP AT tab_bset.

    CHECK: tab_bset-mwskz NE '**',
          tab_bset-mwskz NE space.

    IF bkpf-adisc = 'S'.               " SD discount
      PERFORM check_sd_amounts USING sy-subrc.
    ENDIF.

* Event_006
*    <BEGIN OF ENHANCEMENT>
*    CALL FUNCTION 'OPEN_FI_PERFORM_XXXXXX006_X'
*         EXPORTING
*              I_TAB_BSET     = TAB_BSET
*              I_BKPF         = BKPF
*         TABLES
*              DIS_TAXES      = DIS_TAXES.
*    <END OF ENHANCEMENT>

    CLEAR: tab_taxes.
    MOVE-CORRESPONDING tab_bset TO tab_taxes.
* set posnr
    tab_taxes-posnr = tab_bset-xkposn.
* tax rate.
    IF par_magn IS INITIAL.
      tab_taxes-rate = tab_bset-kbetr .
    ELSE.
*   Magnetic output: Take rate only for VAT concepts
      IF tab_bset-j_1ataxid(2) EQ 'TX' OR
         tab_bset-j_1ataxid(2) EQ 'VN' OR
         tab_bset-j_1ataxid(2) EQ p_txid1(2) OR
         tab_bset-j_1ataxid(2) EQ 'VL'.
        tab_taxes-rate = tab_bset-kbetr .
      ELSE.     " all other tax concepts (non-VAT)
        CLEAR: tab_taxes-rate.
      ENDIF.
    ENDIF.

* set amounts
    tab_taxes-amount = tab_taxes-amount2 = tab_bset-hwste.

    PERFORM fill_tax_base_amnt.

* set the tax amounts for identification
    IF par_magn IS INITIAL.
      CASE tab_bset-j_1ataxid(2).      " TaxId for Vorgangsschlüssel
        WHEN 'EP'.                     " et ertragssteuer perception
          tab_taxes-ertrag_per = tab_bset-hwste.
        WHEN 'GP'.                     " gi region perception
          tab_taxes-region_per = tab_bset-hwste.
* Municipal perception are shown in 'Other perc' in usual Daily VAT
* Changed and reassigned to Municiapl perception field in 1055550
        WHEN 'MP'.                     " municipal perception
*          tab_taxes-region_per = tab_bset-hwste.            "1055550
          tab_taxes-munic_per = tab_bset-hwste.             "1055550
* Earning perception are shown in 'VAT perc' in usual Daily VAT
        WHEN 'PN'.                     " earning perception  Note 645449
* --- Begin ---- GGU -----
*  Se cambia por requerimiento del usuario que quiere que salgan en una columna aparte
*          tab_taxes-perception = tab_bset-hwste.           " Note 645449
          tab_taxes-earn_per  = tab_bset-hwste.
* --- End   ---- GGU -----
        WHEN 'IT'.                     " internal tax
          tab_taxes-vat_intern = tab_bset-hwste.
* Special requirement: Rate of internal tax must be displayed as 0%
          CLEAR: tab_taxes-rate.
        WHEN 'TX'.                     " input/output tax
          tab_taxes-vat_amount = tab_bset-hwste.
        WHEN 'VL'.                     " liberation
          tab_taxes-exemption      = tab_bset-hwste.
        WHEN 'VN'.                     " vat not taxable
* Note 1091024 Start
          PERFORM read_t007b USING tab_taxes-ktosl.
          IF tab_007b-stbkz NE '3'.
            tab_taxes-not_taxed         = tab_bset-hwbas.
          ELSE.
            tab_taxes-not_taxed         = tab_bset-hwste.
          ENDIF.
* Note 1091024 End
          CLEAR: tab_taxes-netamount.
        WHEN 'VP'.                     " perception
          tab_taxes-perception     = tab_bset-hwste.
        WHEN 'VS'.                     " surcharge
          tab_taxes-surcharge       = tab_bset-hwste.
          PERFORM fill_surcharge_tax_codes USING tab_bset-mwskz.
        WHEN p_txid1(2).               " exports
          tab_taxes-exports         = tab_bset-hwbas.
          CLEAR: tab_taxes-netamount.
        WHEN p_txid2(2).               " percep. no c.
          tab_taxes-percepnoc       = tab_bset-hwste.
      ENDCASE.
* Fields tab_taxes-ertrag_per, -region_per, -exports, -percepnoc
* are never filled for magnetic output
    ELSE.
      CASE tab_bset-j_1ataxid(2).      " TaxId for Vorgangsschlüssel
        WHEN 'EP'.                     " et ertragssteuer perception
*          tab_taxes-perception = tab_bset-hwste.           "1006186
          tab_taxes-ertrag_per = tab_bset-hwste.            "1006186
        WHEN 'GP'.                     " gi region perception
          tab_taxes-region_per = tab_bset-hwste.
        WHEN 'IT'.                     " internal tax
          tab_taxes-vat_intern = tab_bset-hwste.
* Special requirement: Rate of internal tax must be displayed as 0%
          CLEAR: tab_taxes-rate.
        WHEN 'TX'.                     " input/output tax
          tab_taxes-vat_amount = tab_bset-hwste.
        WHEN 'VL'.                     " liberation
          tab_taxes-exemption      = tab_bset-hwste.
        WHEN 'VN'.                     " vat not taxable
* Note 1091024 Start
          PERFORM read_t007b USING tab_taxes-ktosl.
          IF tab_007b-stbkz NE '3'.
            tab_taxes-not_taxed         = tab_bset-hwbas.
          ELSE.
            tab_taxes-not_taxed         = tab_bset-hwste.
          ENDIF.
* Note 1091024 End
          CLEAR: tab_taxes-netamount.
* Filled for not taxed transaction (only if no exempt reason exists)


*Begin of Note 1007703

*          tab_taxes-j_1arfz = 'N'.

          IF tab_taxes-mwskz = tab_bset-mwskz.
            tab_taxes-j_1arfz = tab_bset-j_1arfz.
          ELSE.
            tab_taxes-j_1arfz = 'N'.
          ENDIF.
*End of Note 1007703


        WHEN 'VP'.                     " perception
          tab_taxes-perception     = tab_bset-hwste.
        WHEN 'VS'.                     " surcharge
* Surcharge is not shown for Purchase File.
          IF NOT par_vers IS INITIAL.
            IF tab_j_1adrver-j_1aproc = 1 OR
               tab_j_1adrver-j_1aproc = 3.     " input / purchase
              CLEAR tab_taxes-surcharge.
              amnt-total = amnt-total + tab_bset-hwste.
            ELSE.
              tab_taxes-surcharge       = tab_bset-hwste.
            ENDIF.
          ELSE.
            tab_taxes-surcharge       = tab_bset-hwste.
          ENDIF.
          PERFORM fill_surcharge_tax_codes USING tab_bset-mwskz.
        WHEN 'MP'.                     " municipal perception
          tab_taxes-munic_per       = tab_bset-hwste.
        WHEN 'PN'.                     " earning perception  Note 645449
          IF NOT par_vers IS INITIAL.
            IF tab_j_1adrver-j_1aproc = 1 OR
               tab_j_1adrver-j_1aproc = 3.     " input / purchase
              tab_taxes-earn_per        = tab_bset-hwste.
            ELSE.
              tab_taxes-region_per = tab_bset-hwste.
            ENDIF.
          ENDIF.
        WHEN p_txid1(2).               " exports
          tab_taxes-taxed          = tab_bset-hwbas.
          CLEAR: tab_taxes-netamount.
        WHEN p_txid2(2).               " percep. no c.
          tab_taxes-percepnoc       = tab_bset-hwste.
      ENDCASE.
    ENDIF.

    IF tab_bset-j_1ataxid   = 'VL01' AND   " vat liber. no credit
       tab_j_1adrver-j_1aproc < 3.     " credit / debit
      CLEAR: tab_taxes-amount2,tab_taxes-exemption.
    ENDIF.

* Regio
    IF NOT tab_bset-regio IS INITIAL.
      CLEAR tab_regio.
      MOVE-CORRESPONDING: bkpf   TO tab_regio.
      tab_regio-mwskz = tab_bset-mwskz.
      tab_regio-regio = tab_bset-regio.
      tab_regio-ktosl = tab_bset-ktosl.                     "1083548
      COLLECT tab_regio.
    ENDIF.

* Note 789418 Starts
    IF g_surc_disc = 'X'.

      tab_taxes-not_taxed = g_sd_amt.
      CLEAR : g_surc_disc,g_sd_amt.

    ENDIF.
*Note 789418 Ends

    APPEND  tab_taxes.
* MM Documents are sometimes wrongly displayed:
* Take amounts from BSEG-HWBAS instead of BSEG-DMBTR then. As this
* does not work generally, special checks have to be performed.
* Here: Take base amount only once per tax code. Else the base amount
* were taken twice.
    IF flg_is_beleg = 'R'.
      READ TABLE lcl_tab_mwskz WITH KEY mwskz = tab_taxes-mwskz.
      IF sy-subrc = 0.
        CLEAR: tab_taxes-hwbas.
      ELSE.
        lcl_tab_mwskz-mwskz = tab_taxes-mwskz.
        APPEND lcl_tab_mwskz.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING tab_taxes TO sum_tab_taxes.
    COLLECT sum_tab_taxes.

  ENDLOOP.
  IF flg_is_beleg = 'R'.
    PERFORM check_mm_base_amounts.
  ENDIF.
ENDFORM. " FILL_TAB_TAXES

*&---------------------------------------------------------------------*
*&      Form  FILL_TAB_BSEG_TAXED
*       tab_bseg_taxed contains all the base amounts                   *
*----------------------------------------------------------------------
*  -->  p1        bkpf,tab_taxes, flg_is_beleg                         *
*  <--  p2        tab_bseg_taxed
*----------------------------------------------------------------------*
FORM fill_tab_bseg_taxed.

  REFRESH: tab_bseg_taxed.
  LOOP AT sum_tab_taxes.
    CLEAR: tab_bseg_taxed.
    tab_bseg_taxed-mwskz  = sum_tab_taxes-mwskz.
    tab_bseg_taxed-posnr  = sum_tab_taxes-posnr.
* taxed
    tab_bseg_taxed-taxed = sum_tab_taxes-netamount.
* exemption
    CLEAR tab_taxes.
    READ TABLE tab_taxes WITH KEY mwskz = sum_tab_taxes-mwskz
                                  posnr = sum_tab_taxes-posnr.
    PERFORM check_if_exempted  USING tab_taxes-j_1arfz.

* not_taxed
* Note 744713 Changes start.

*    LOOP AT TAB_TAXES WHERE   MWSKZ = SUM_TAB_TAXES-MWSKZ
*                          AND POSNR = SUM_TAB_TAXES-POSNR.
*      IF TAB_TAXES-STGRP = '4'.
*        TAB_BSEG_TAXED-NOT_TAXED = TAB_BSEG_TAXED-TAXED.
*        CLEAR TAB_BSEG_TAXED-TAXED.
*        EXIT.
*      ENDIF.
*    ENDLOOP.

    LOOP AT tab_taxes WHERE   mwskz = sum_tab_taxes-mwskz
                          AND posnr = sum_tab_taxes-posnr
                          AND stgrp <> '4'.
    ENDLOOP.

    IF sy-subrc <> 0.
      tab_bseg_taxed-not_taxed = tab_bseg_taxed-taxed.
      CLEAR tab_bseg_taxed-taxed.
    ENDIF.
* Note 744713 Changes ends.

* exports
    IF bkpf-blart NE 'GB'.
      IF  bkpf-xblnr+4(1) EQ 'E' AND par_magn IS INITIAL.
        tab_bseg_taxed-exports   =  sum_tab_taxes-netamount.
        CLEAR: tab_bseg_taxed-taxed,
               tab_bseg_taxed-not_taxed,
               tab_bseg_taxed-exemption.
      ENDIF.
    ENDIF.
*  Only VAT amount is checked for initial as zero VAT implies Zero VAT
*  surcharge and zero VAT Perception
*  Note 727028 Starts (Message 97613 2004)

    IF sum_tab_taxes-vat_amount IS INITIAL
          AND tab_bseg_taxed-exemption IS INITIAL.   "Note 768422
      IF NOT sum_tab_taxes-region_per IS INITIAL.
*       tab_bseg_taxed-exemption = tab_bseg_taxed-taxed. "Note 1034548
        CLEAR tab_bseg_taxed-taxed.
      ELSEIF NOT sum_tab_taxes-munic_per IS INITIAL.
*        tab_bseg_taxed-exemption = tab_bseg_taxed-taxed. "Note 1034548
        CLEAR tab_bseg_taxed-taxed.
      ENDIF.
    ENDIF.
*  Note 727028 Ends

    APPEND tab_bseg_taxed.
  ENDLOOP.                             " sum_tab_taxes
ENDFORM. " FILL_TAB_BSEG_TAXED

*&---------------------------------------------------------------------*
*&      Form  FILL_TAB_BELEG
* tab_beleg contains all lines for output in no compress modus
*----------------------------------------------------------------------*
*  -->  p1        bkpf,tab_taxes, tab_bseg_taxed, flg_is_beleg
*  <--  p2        tab_beleg
*----------------------------------------------------------------------*
FORM fill_tab_beleg.
  DATA: flg_gl    TYPE c,         " Check docs with special G/L item
*---> 20/06/2023 - Migração S4 - MA
*        gl_amount(8) TYPE p  decimals 2.         " Total: Docs with special G/L item
        gl_amount TYPE dmbtr.
*<--- 20/06/2023 - Migração S4 - MA
*   Document without tax (no entry in BSET)
  DESCRIBE TABLE tab_taxes LINES hlp_cnt.
  IF hlp_cnt = 0 OR
    kd-hkont IS INITIAL.
    flg_reject_doc = 'X'.
    EXIT.
  ENDIF.

* set beleg total  and
  READ TABLE tab_bseg INDEX 1.
* if check only selected Vendors
  DESCRIBE TABLE s_lifnr LINES tmp_cnt1.
  IF NOT tmp_cnt1 IS INITIAL.
    IF NOT tab_bseg-ktnra IN s_lifnr.
      flg_reject_doc = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  tab_bseg-total = amnt-total.
  IF NOT par_vers IS INITIAL.
    IF NOT xumsks IS INITIAL.          " Down Payment
      IF tab_j_1adrver-j_1aproc EQ 3
         OR tab_j_1adrver-j_1aproc EQ 4.
        tab_bseg-total = tab_bseg-total * -1 .
      ENDIF.
    ENDIF.
  ENDIF.
* compensation date.
  tab_bseg-augbl = xaugbl.
  tab_bseg-augdt = xaugdt.
  MODIFY tab_bseg INDEX 1.             " show only 1. D/K
*fill header of tab_adrs
  PERFORM read_adrs.

* 1: fill tab_beleg  from tab_bseg_taxed
  REFRESH: tab_beleg.
  LOOP AT tab_bseg_taxed.
    CLEAR: tab_beleg.
    MOVE-CORRESPONDING tab_bseg TO tab_beleg.
    tab_beleg-linetype = '1'. "this line is only for Versteuern Sp.
    tab_beleg-ktnra = tab_adrs-ktnra.
    tab_beleg-name1 = tab_adrs-name1.
    tab_beleg-stcdt = tab_adrs-stcdt.
    tab_beleg-stcd1 = tab_adrs-stcd1.
*---> 09/06/2023 - Migração S4 - JS
*    tab_beleg-total = tab_bseg-total.  " Summe Beleg
    tab_beleg-total = CONV #( tab_bseg-total ).
*<--- 09/06/2023 - Migração S4 - JS

* Fill empty condition type for exports with entry that is not
* allowed in text table T685T.
* (for technical reasons, no kschl can be stored for exports documents)
    IF flg_is_beleg    = 'S' AND
       bkpf-xblnr+4(1) = 'E' AND
       tab_beleg-kschl IS INITIAL.
      tab_beleg-kschl = '!EXP'.
    ENDIF.

    tab_beleg-mwskz =  tab_bseg_taxed-mwskz.
    tab_beleg-posnr =  tab_bseg_taxed-posnr.
    CLEAR: tab_beleg-ktosl.

    IF par_comp IS INITIAL.            " no compress ( detail output )
      tab_beleg-rate =  100000.        " only for sorting
    ELSE.
      CLEAR: tab_beleg-rate.
    ENDIF.
*---> 09/06/2023 - Migração S4 - JS
*    tab_beleg-taxed      = tab_bseg_taxed-taxed .
*    tab_beleg-not_taxed  = tab_bseg_taxed-not_taxed .
*    tab_beleg-exemption  = tab_bseg_taxed-exemption .
*    tab_beleg-exports    = tab_bseg_taxed-exports .
    tab_beleg-taxed     = CONV #( tab_bseg_taxed-taxed ).
    tab_beleg-not_taxed = CONV #( tab_bseg_taxed-not_taxed ).
    tab_beleg-exemption = CONV #( tab_bseg_taxed-exemption ).
    tab_beleg-exports   = CONV #( tab_bseg_taxed-exports ).
*<--- 09/06/2023 - Migração S4 - JS

    tab_beleg-line_total = tab_bseg_taxed-taxed +
                           tab_bseg_taxed-not_taxed +
                           tab_bseg_taxed-exemption +
                           tab_bseg_taxed-exports .
    tab_beleg-fityp     = tab_bseg-fityp. " For magnetic output

* RG 1361 CHANGES STARTS
*   tab_beleg-cai       = gf_cai.      " For magnetic output
*   tab_beleg-due_date  = gf_due_date.
*   tab_beleg-fisc_cont = gf_fisc_cont." For magnetic output
    IF NOT gf_fisc_cont IS INITIAL.
      tab_beleg-fisc_cont = gf_fisc_cont.
      CONCATENATE gf_cai gf_fisc_cont INTO tab_beleg-cai.
      tab_beleg-due_date   = gf_due_date.
    ELSE.
      tab_beleg-cai       = gf_cai.
      tab_beleg-due_date   = gf_due_date.
    ENDIF.
* RG 1361 CHANGES ENDS
    APPEND tab_beleg.
  ENDLOOP.

* 2: fill tab_beleg  from tab_taxes
  SORT tab_taxes BY mwskz posnr.
  LOOP AT tab_taxes.
    CLEAR: tab_beleg.
    MOVE-CORRESPONDING tab_bseg TO tab_beleg.
*    clear: tab_beleg-linetype.
    tab_beleg-ktnra = tab_adrs-ktnra.
    tab_beleg-name1 = tab_adrs-name1.
    tab_beleg-stcdt = tab_adrs-stcdt.
    tab_beleg-stcd1 = tab_adrs-stcd1.
*---> 09/06/2023 - Migração S4 - JS
*    tab_beleg-total = tab_bseg-total.
    tab_beleg-total = CONV #( tab_bseg-total ).
*<--- 09/06/2023 - Migração S4 - JS

    tab_beleg-mwskz =  tab_taxes-mwskz.
    tab_beleg-posnr =  tab_taxes-posnr.
    tab_beleg-ktosl =  tab_taxes-ktosl.
    tab_beleg-rate  =  tab_taxes-rate.
    tab_beleg-hkont =  tab_taxes-hkont.
    tab_beleg-hwbas =  tab_taxes-hwbas.
    tab_beleg-hwste =  tab_taxes-hwste.
    tab_beleg-ktosl =  tab_taxes-ktosl.
    tab_beleg-kschl =  tab_taxes-kschl.
    tab_beleg-j_1arfz = tab_taxes-j_1arfz.  " For magnetic output
*---> 09/06/2023 - Migração S4 - JS
*   tab_beleg-taxed = tab_taxes-taxed .
    tab_beleg-taxed = CONV #( tab_taxes-taxed ).
*<--- 09/06/2023 - Migração S4 - JS

* Note 1041712 Start
*    IF par_magn IS INITIAL.
*      tab_beleg-not_taxed    = tab_taxes-not_taxed +
*                               tab_taxes-vat_intern.
*    ELSE.
*      tab_beleg-not_taxed    = tab_taxes-not_taxed.
*      tab_beleg-vat_intern   = tab_taxes-vat_intern.
*    ENDIF.

*---> 09/06/2023 - Migração S4 - JS
*    tab_beleg-not_taxed    = tab_taxes-not_taxed.
*    tab_beleg-vat_intern   = tab_taxes-vat_intern.
** Note 1041712 End
*    tab_beleg-vat          = tab_taxes-vat_amount.
*    tab_beleg-rnr_vat      = tab_taxes-surcharge.
*    tab_beleg-vat_percep   = tab_taxes-perception.
*    tab_beleg-other_percep = tab_taxes-region_per .
**                             + tab_taxes-ertrag_per.
*    tab_beleg-munic_per    = tab_taxes-munic_per.
    tab_beleg-not_taxed    = CONV #( tab_taxes-not_taxed ).
    tab_beleg-vat_intern   = CONV #( tab_taxes-vat_intern ).
* Note 1041712 End
    tab_beleg-vat          = CONV #( tab_taxes-vat_amount ).
    tab_beleg-rnr_vat      = CONV #( tab_taxes-surcharge ).
    tab_beleg-vat_percep   = CONV #( tab_taxes-perception ).
    tab_beleg-other_percep = CONV #( tab_taxes-region_per ).
*                             + tab_taxes-ertrag_per.
    tab_beleg-munic_per    = CONV #( tab_taxes-munic_per ).
*<--- 09/06/2023 - Migração S4 - JS

    tab_taxes-earn_per   = tab_taxes-earn_per + tab_taxes-ertrag_per.
    tab_taxes-earn_per = tab_taxes-earn_per .

*---> 09/06/2023 - Migração S4 - JS
*    tab_beleg-earn_per     = tab_taxes-earn_per.
*    tab_beleg-exemption    = tab_taxes-exemption.
*    tab_beleg-exports      = tab_taxes-exports.
*    tab_beleg-percepnoc    = tab_taxes-percepnoc.
    tab_beleg-earn_per = CONV #( tab_taxes-earn_per ).
    tab_beleg-exemption = CONV #( tab_taxes-exemption ).
    tab_beleg-exports = CONV #( tab_taxes-exports ).
    tab_beleg-percepnoc = CONV #( tab_taxes-percepnoc ).
*---> 09/06/2023 - Migração S4 - JS
    tab_beleg-line_total =
                              tab_beleg-taxed +
                              tab_beleg-not_taxed +
                              tab_beleg-vat +
                              tab_beleg-rnr_vat +
                              tab_beleg-vat_intern +
                              tab_beleg-vat_percep +
                              tab_beleg-other_percep +
                              tab_beleg-munic_per +
                              tab_beleg-earn_per +
                              tab_beleg-exemption +
                              tab_beleg-exports +
                              tab_beleg-percepnoc .
* RG 1361 CHANGES STARTS
*    tab_beleg-cai        = gf_cai.     " For magnetic output
*    tab_beleg-due_date   = gf_due_date.
*    tab_beleg-fisc_cont = gf_fisc_cont.
    IF NOT gf_fisc_cont IS INITIAL.
      tab_beleg-fisc_cont = gf_fisc_cont.
      CONCATENATE gf_cai gf_fisc_cont INTO tab_beleg-cai.
      tab_beleg-due_date   = gf_due_date.
    ELSE.
      tab_beleg-cai       = gf_cai.
      tab_beleg-due_date   = gf_due_date.
    ENDIF.
* RG 1361 CHANGES ENDS
    APPEND tab_beleg.
  ENDLOOP.

* tab_bseg index 1 was read before -> know field umsks at this point
  IF NOT xumsks IS INITIAL.             " Special G/L item - Note 715618
    PERFORM check_special_gl_amount CHANGING flg_gl gl_amount.
    IF NOT flg_gl IS INITIAL.
* Document total is contained in every tab_bseg line -> adjust all.
      LOOP AT tab_beleg.
* Error only occurs for document total -> Sum of line totals is correct
        tab_beleg-total = gl_amount.
        MODIFY tab_beleg.
      ENDLOOP.
    ENDIF.
  ENDIF.

* konv-waers is either filled from bkpf-waers or from *bkpf-waers
  IF flg_is_beleg = 'S' AND tab_001-waers NE konv-waers.
    PERFORM check_sd_foreign_currency.
  ENDIF.
ENDFORM. " FILL_TAB_BELEG

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_TAB_BELEG_TO_EP
*  extract ep.
* tab_beleg---------------|>ep->/OUTPUT/
* BKPF--------------------|
*----------------------------------------------------------------------*
*  -->  p1        tab_beleg,ep
*  <--  p2        text
*----------------------------------------------------------------------*
FORM extract_tab_beleg_to_ep.
  CLEAR ep.
  MOVE-CORRESPONDING bkpf TO ep.
  ep-buper(4) = bkpf-gjahr.
  ep-buper+4  = bkpf-monat.
  ep-j_1aoftp  = tab_j_1aotdet-j_1aoftp.   " For magnetic output
  ep-oftp_text  = tab_j_1aotdet-text5.
  ep-prtchr   = bkpf-xblnr+4(1).
  CLEAR: ep-xblnr+4(1).
  ep-flg_sd_beleg = flg_sd_beleg.
  ep-flg_is_beleg = flg_is_beleg.
  ep-sd_vbeln     = xvbeln.
  LOOP AT tab_beleg.
    MOVE-CORRESPONDING tab_beleg TO ep.
    ep-rate = ep-rate / 10.
    EXTRACT daten.
  ENDLOOP.
  LOOP AT tab_log_entry1.
    tab_log_entry10 = tab_log_entry1.
    COLLECT tab_log_entry10.
  ENDLOOP.
  LOOP AT tab_log_entry2.
    tab_log_entry20 = tab_log_entry2.
    COLLECT tab_log_entry20.
  ENDLOOP.
ENDFORM. " EXTRACT_TAB_BELEG_TO_EP

*&---------------------------------------------------------------------*
*&      Form  GET_FI_CANCEL_DOCM
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM get_fi_cancel_docm.
  CLEAR: vbfa_tab, comwa.
  REFRESH vbfa_tab.

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
  ENDIF.
ENDFORM. " GET_FI_CANCEL_DOCM

*&---------------------------------------------------------------------*
*&      Form  GET_LOCAL_AMNT
*&---------------------------------------------------------------------*
FORM get_local_amnt USING f_amnt.
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      date             = bkpf-budat
      foreign_amount   = f_amnt
      foreign_currency = konv-waers
      local_currency   = tab_001-waers
      rate             = xkursf
    IMPORTING
      local_amount     = f_amnt.
ENDFORM. " GET_LOCAL_AMNT

*&---------------------------------------------------------------------*
*&      Form  FILL_BSET
*&---------------------------------------------------------------------*
FORM fill_bset.
* set the BSET fields for calling "perform: fill_bset_to_tab_bset."
  CLEAR bset.
  bset-mwskz = konv-mwsk1.
  bset-ktosl = konv-kvsl1.
  bset-kschl = konv-kschl.

  CLEAR sd_hkont.
*  READ TABLE SD_HKONT WITH KEY BSET-KTOSL.                 "989807
  READ TABLE sd_hkont WITH KEY ktosl = bset-ktosl           "989807
                               mwskz = bset-mwskz.          "989807

  IF sy-subrc = 0.
    bset-hkont = sd_hkont-hkont.
  ENDIF.

  bset-hwste = konv-kwert.
  bset-hwbas = konv-kawrt.
  bset-kbetr = konv-kbetr.
*  xkposn     = konv-kposn.

  IF bkpf-adisc = 'S'.                 " SD discount
    bset-hwste = bset-hwste * sd_disc_fact / 100000." by discount factor
    bset-hwbas = bset-hwbas * sd_disc_fact / 100000.
* Divided by 100000 for ABAP type conversion reasons
  ENDIF.

  IF vbrp-shkzg IS INITIAL.
    bset-shkzg = 'H'.
  ELSE.
    bset-shkzg = 'S'.
  ENDIF.

  IF bkpf-adisc = 'S'.                 " SD discount
* so convert the debit/credit sign
    IF bset-shkzg = 'H'.
      bset-shkzg = 'S'.
    ELSE.
      bset-shkzg = 'H'.
    ENDIF.
  ENDIF.

  IF bset-hwste < 0.
* liberation, so convert
    bset-hwste = bset-hwste * -1.
    IF vbrp-shkzg IS INITIAL.
      bset-shkzg = 'S'.
    ELSE.
      bset-shkzg = 'H'.
    ENDIF.
  ENDIF.
ENDFORM. " FILL_BSET

*&---------------------------------------------------------------------*
*&      Form  FILL_TAX_BASE_AMNT
*&---------------------------------------------------------------------*
FORM fill_tax_base_amnt.
  CHECK tax_base-mwskz NE tab_taxes-mwskz OR
        tax_base-posnr NE tab_taxes-posnr.

  CLEAR: tax_base.
  READ TABLE tax_base WITH KEY mwskz = tab_taxes-mwskz
                               posnr = tab_taxes-posnr.
  CHECK sy-subrc NE 0.

* tax base has not been set
  MOVE-CORRESPONDING tab_taxes TO tax_base.
  APPEND tax_base.
  IF flg_is_beleg = 'F'. "AND                              1091024
*       xumsks          NE space.       " down payment 1074703
    tab_taxes-netamount = tab_taxes-hwbas.
*   " SD document or SD discount
  ELSEIF flg_is_beleg = 'S'.
    CLEAR tab_vbrp.
    READ TABLE tab_vbrp WITH KEY mwskz = tab_taxes-mwskz
                                 posnr = tab_taxes-posnr.
    IF bkpf-adisc = 'S'.
* Checking if the disocunt factor is not zero               "1016337
      IF NOT sd_disc_fact IS INITIAL.                       "1016337
        tab_vbrp-netwr = tab_vbrp-netwr * sd_disc_fact / 100000.
        " by discount factor
* Divided by 100000 for ABAP type conversion reasons
      ENDIF.                                                "1016337

* Checking if the tax base amount is less than zero         "1016337
* instead of checking the debit/credit indicator for debit  "1016337
*      IF tab_bset-shkzg EQ 'S'.                            "1016337
      IF tab_bset-hwbas < 0.                                "1016337
        tab_vbrp-netwr = tab_vbrp-netwr * -1.
      ENDIF.
    ENDIF.
    tab_taxes-netamount = tab_vbrp-netwr.
  ELSEIF flg_is_beleg = 'R'.
    CLEAR mm_taxed.

    READ TABLE mm_taxed WITH KEY mwskz = tab_taxes-mwskz
                                 txgrp = tab_taxes-posnr.
    IF sy-subrc NE 0.

*      IF tab_taxes-txmod <> 0.                              "1074703
*        CLEAR tab_taxes-netamount.       " only tax posted
*      ELSE.


      tab_taxes-netamount = tab_taxes-hwbas.




*      ENDIF.
    ELSE.
*** Begin Note 1007681 ***
      IF tab_taxes-netamount = tab_taxes-amount
      AND tab_taxes-j_1arfz IS INITIAL.
        CLEAR tab_taxes-netamount.
      ELSE.
        tab_taxes-netamount = tab_taxes-hwbas.
      ENDIF.
*      tab_taxes-netamount = tab_taxes-hwbas.
    ENDIF.
*** End of Note 1007681 ***


  ELSE.
    CLEAR plines.
    READ TABLE plines WITH KEY mwskz = tab_taxes-mwskz
                               txgrp = tab_taxes-posnr.
    IF sy-subrc NE 0.
      CLEAR tab_taxes-netamount.       " only tax posted
    ELSE.
      tab_taxes-netamount = tab_taxes-hwbas.
    ENDIF.
  ENDIF.

* Event_007
*    <BEGIN OF ENHANCEMENT>
*  CALL FUNCTION '/XXARISM/VAT_BOOK_EVENT_07'
*       EXPORTING
*            I_BKPF         = BKPF
*            I_TAB_BSET     = TAB_BSET
*            I_TAB_VBRP     = TAB_VBRP
*            I_SD_DISC_FACT = SD_DISC_FACT
*       CHANGING
*            E_TAB_TAXES    = TAB_TAXES.
*    <END OF ENHANCEMENT>
ENDFORM. " FILL_TAX_BASE_AMNT

*&---------------------------------------------------------------------*
*&      Form  FILL_SURCHARGE_TAX_CODES
*&---------------------------------------------------------------------*
*       Fill internal table with all tax codes containing TaxID 'VS',
*       i.e. all tax codes for which PAR_SUM should take effect.
*----------------------------------------------------------------------*
*      -->P_MWSKZ Tax code with TaxID 'VS'
*----------------------------------------------------------------------*
FORM fill_surcharge_tax_codes USING p_mwskz LIKE bseg-mwskz.
  READ TABLE tab_surcharge WITH KEY mwskz = p_mwskz.
  CHECK sy-subrc <> 0.
  tab_surcharge-mwskz = p_mwskz.
  APPEND tab_surcharge.
ENDFORM. " FILL_SURCHARGE_TAX_CODES

*&---------------------------------------------------------------------*
*&      Form  GET_DOWNPAYMENT_REQUESTS
*&---------------------------------------------------------------------*
FORM get_downpayment_requests.
* Get downpayments requests for reference numbers
*  check bkpf-xblnr+5(8) is initial.

  IF tab_bseg-rebzg IS INITIAL.        " Anzahlung
    IF tab_bseg-koart EQ 'K'.
      SELECT * FROM bsak WHERE bukrs = tab_bseg-bukrs
                         AND   lifnr = tab_bseg-lifnr
                         AND   umsks = 'A'
                         AND   augdt = bkpf-budat
                         AND   augbl = tab_bseg-belnr
                         AND   wrbtr = tab_bseg-wrbtr
                         AND   wmwst = tab_bseg-wmwst.

        SELECT SINGLE * FROM bkpf INTO *bkpf
                         WHERE bukrs = bsak-bukrs
                         AND   belnr = bsak-belnr
                         AND   gjahr = bsak-gjahr.

        CHECK sy-subrc EQ 0.

        bkpf-xblnr   = *bkpf-xblnr.
        bkpf-brnch   = *bkpf-brnch.
        bkpf-numpg   = *bkpf-numpg.
        EXIT.
      ENDSELECT.
    ELSE.
      SELECT * FROM bsad WHERE bukrs = tab_bseg-bukrs
                         AND   kunnr = tab_bseg-kunnr
                         AND   umsks = 'A'
                         AND   augdt = bkpf-budat
                         AND   augbl = tab_bseg-belnr
                         AND   wrbtr = tab_bseg-wrbtr
                         AND   wmwst = tab_bseg-wmwst.

        SELECT SINGLE * FROM bkpf INTO *bkpf
                         WHERE bukrs = bsad-bukrs
                         AND   belnr = bsad-belnr
                         AND   gjahr = bsad-gjahr.

        CHECK sy-subrc EQ 0.

        bkpf-xblnr   = *bkpf-xblnr.
        bkpf-brnch   = *bkpf-brnch.
        bkpf-numpg   = *bkpf-numpg.
        EXIT.
      ENDSELECT.
    ENDIF.
  ELSE.                                " Anzahlungsverrechnung
    SELECT SINGLE * FROM bkpf INTO *bkpf
                    WHERE bukrs = tab_bseg-bukrs
                    AND   belnr = tab_bseg-rebzg
                    AND   gjahr = tab_bseg-rebzj.

    IF *bkpf-xblnr EQ space.
      DATA etl1237c6r8130 TYPE TABLE OF bseg.
      DATA rldnr_l1237c6r9050 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l1237c6r9050
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr   = rldnr_l1237c6r9050
            i_bukrs   = *bkpf-bukrs
            i_belnr   = *bkpf-belnr
            i_gjahr   = *bkpf-gjahr
            i_buzei   = tab_bseg-rebzz
          IMPORTING
            et_bseg   = etl1237c6r8130
          EXCEPTIONS
            not_found = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl1237c6r8130 ) = 1.
        *bseg = etl1237c6r8130[ 1 ].
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.


      IF tab_bseg-koart EQ 'K'.
        SELECT * FROM bsak WHERE bukrs = tab_bseg-bukrs
                           AND   lifnr = tab_bseg-lifnr
                           AND   umsks = 'A'
                           AND   augdt = *bkpf-budat
                           AND   augbl = *bseg-belnr
                           AND   wrbtr = *bseg-wrbtr
                           AND   wmwst = *bseg-wmwst.

          SELECT SINGLE * FROM bkpf INTO *bkpf
                           WHERE bukrs = bsak-bukrs
                           AND   belnr = bsak-belnr
                           AND   gjahr = bsak-gjahr.

          CHECK sy-subrc EQ 0.

          bkpf-xblnr   = *bkpf-xblnr.
          bkpf-brnch   = *bkpf-brnch.
          bkpf-numpg   = *bkpf-numpg.
          EXIT.
        ENDSELECT.
      ELSE.
        SELECT * FROM bsad WHERE bukrs = tab_bseg-bukrs
                           AND   kunnr = tab_bseg-kunnr
                           AND   umsks = 'A'
                           AND   augdt = *bkpf-budat
                           AND   augbl = *bseg-belnr
                           AND   wrbtr = *bseg-wrbtr
                           AND   wmwst = *bseg-wmwst.

          SELECT SINGLE * FROM bkpf INTO *bkpf
                           WHERE bukrs = bsad-bukrs
                           AND   belnr = bsad-belnr
                           AND   gjahr = bsad-gjahr.

          CHECK sy-subrc EQ 0.

          bkpf-xblnr   = *bkpf-xblnr.
          bkpf-brnch   = *bkpf-brnch.
          bkpf-numpg   = *bkpf-numpg.
          EXIT.
        ENDSELECT.
      ENDIF.
    ELSE.
      bkpf-xblnr   = *bkpf-xblnr.
      bkpf-brnch   = *bkpf-brnch.
      bkpf-numpg   = *bkpf-numpg.
    ENDIF.
  ENDIF.

ENDFORM. " GET_DOWNPAYMENT_REQUESTS

*----------------------------------------------------------------------*
* Subroutines for Master Data and Customizing (alphabetical order)
*----------------------------------------------------------------------*
* READ_ADRS
* READ_CAI_AND_FISC_CONT       Print authorization code (CAI)
* READ_CUSTOMS_DATA
* READ_J_1ADRVER      versions of daily VAT reporting for selected bukrs
* READ_J_1ADRVERT              texts of versions for daily VAT reporting
* READ_J_1ARZTX
* READ_J_1ATAXID TAX Identification from table j_1ataxid / tab_j_1ataxid
* READ_KONP
* READ_OFFICIAL_DOC_TYPE
* READ_T001
* READ_T007A
* READ_T007B
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_ADRS
*&---------------------------------------------------------------------*
*       Read address data of cust./vend. from tables TAB_ADRS/KNA1/LFA1
*----------------------------------------------------------------------*
*  -->  p1        tab_bseg,bkpf header
*  <--  p2        tab_adrs      header
*----------------------------------------------------------------------*
FORM read_adrs.

  IF tab_bseg-ktnra  IS INITIAL AND    " no vendor/custmer
   NOT bkpf-bvorg IS INITIAL.          " but  intercompany_document.
    SELECT * FROM bvor WHERE bvorg EQ bkpf-bvorg
                      AND    bukrs NE bkpf-bukrs. " up to 1 rows.
    ENDSELECT.
    PERFORM read_t001 USING bvor-bukrs.
    CLEAR tab_adrs.
    tab_adrs-koart = tab_bseg-koart.
    tab_adrs-ktnra = tab_bseg-ktnra = bvor-bukrs.
    tab_adrs-name1 = tab_001-name1.
    tab_adrs-stcdt = tab_001-stcdt.
    tab_adrs-stcd1 = tab_001-stcd1.
    PERFORM read_t001 USING bkpf-bukrs." restore
  ELSE.                                " vendor/custmer
    CLEAR tab_adrs.
    tab_adrs-koart = tab_bseg-koart.
    tab_adrs-ktnra = tab_bseg-ktnra.

    IF tab_bseg-xcpdk = space.         " no CPD account
      READ TABLE tab_adrs.
      IF sy-subrc NE 0.
        IF tab_bseg-koart EQ 'D'.
          SELECT SINGLE * FROM kna1
            WHERE kunnr EQ tab_bseg-ktnra.
          IF sy-subrc EQ 0.
            tab_adrs-name1 = kna1-name1.
            tab_adrs-stcdt = kna1-stcdt.
            tab_adrs-stcd1 = kna1-stcd1.
            IF kna1-land1 NE tab_001-land1.   " foreign customer
              SELECT SINGLE * FROM j_1afrid
                              WHERE land1 = kna1-land1
                              AND   stkzn = kna1-stkzn.
              IF sy-subrc EQ 0.
                tab_adrs-stcd1  = j_1afrid-j_1afpid.
              ENDIF.
            ENDIF.
            APPEND tab_adrs.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM lfa1
            WHERE lifnr EQ tab_bseg-ktnra.
          IF sy-subrc EQ 0.
            tab_adrs-name1 = lfa1-name1.
            tab_adrs-stcdt = lfa1-stcdt.
            tab_adrs-stcd1 = lfa1-stcd1.
            IF lfa1-land1 NE tab_001-land1.   " foreign vendor
* For getting correct CUIT /NIF as 80 for foreign vendors
* Begin Note 1034293
              tab_adrs-stcdt = '80'.
* End Note 1034293

              SELECT SINGLE * FROM j_1afrid
                              WHERE land1 = lfa1-land1
                              AND   stkzn = lfa1-stkzn.
              IF sy-subrc EQ 0.
                tab_adrs-stcd1  = j_1afrid-j_1afpid.
              ENDIF.
            ENDIF.
            APPEND tab_adrs.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.                              " CPD account
      SELECT * FROM bsec UP TO 1 ROWS
        WHERE bukrs = bkpf-bukrs
          AND belnr = bkpf-belnr
          AND gjahr = bkpf-gjahr.
      ENDSELECT.
      IF sy-subrc = 0.                 " read from bsec
        tab_adrs-name1 = bsec-name1.
        tab_adrs-stcdt = bsec-stcdt.
        tab_adrs-stcd1 = bsec-stcd1.
* Fiscal type is normally filled in GET BSEG from customer/vendor master
        IF NOT bsec-fityp IS INITIAL.                     " Note 645449
          tab_bseg-fityp = bsec-fityp.                    " Note 645449
        ENDIF.                                            " Note 645449
        IF bsec-land1 NE tab_001-land1.
          SELECT SINGLE * FROM j_1afrid
                          WHERE land1 = bsec-land1
                          AND   stkzn = bsec-stkzn.
          IF sy-subrc EQ 0.
            tab_adrs-stcd1  = j_1afrid-j_1afpid.
          ENDIF.
        ENDIF.
      ELSE.                                        "Note begin 948565
        payer = 'RG'.
*To avoid vbrk is initial.
        IF vbrk-vbeln  IS INITIAL OR
                 vbrk-kunrg IS INITIAL.
          SELECT SINGLE * FROM vbrk WHERE vbeln = bkpf-awkey.
        ENDIF.
*Getting the address number of the customer
        SELECT SINGLE * FROM vbpa
          WHERE vbeln = vbrk-vbeln
            AND kunnr = vbrk-kunrg
            AND parvw = payer.
        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM adrc
            WHERE addrnumber = vbpa-adrnr.
          IF sy-subrc EQ 0.
            tab_adrs-name1 = adrc-name1.
          ENDIF.
          SELECT SINGLE * FROM vbpa3
            WHERE vbeln = bseg-vbeln
              AND parvw = payer.
          IF sy-subrc EQ 0.
            tab_adrs-stcdt = vbpa3-stcdt.
            tab_adrs-stcd1 = vbpa3-stcd1.
          ELSE.
            CLEAR tab_log_entry1.
            tab_log_entry1-name    = 'VBPA3'.
            tab_log_entry1-key     = 'Tax number category & Tax number'.
            PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                                 tab_log_entry1-key
                                                 tab_log_entry1-belnr
                                                 bkpf-belnr.
            COLLECT tab_log_entry1.
          ENDIF.
        ENDIF.                                           "Note end   948565

      ENDIF.                           " read from bsec
    ENDIF.                             " CPD account
  ENDIF.                               " vendor/custmer
ENDFORM. " READ_ADRS
*&---------------------------------------------------------------------*
*&      Form  READ_CAI_AND_FISC_CONT
*&---------------------------------------------------------------------*
*       Read print authorization code (CAI) and fiscal controller
*----------------------------------------------------------------------*
FORM read_cai_and_fisc_cont.
* Only relevant for magnetic output.
  CHECK: NOT par_magn IS INITIAL.
* Form can be called after form READ_SD_INVOICE, or for FI documents
  CASE flg_is_beleg.
    WHEN 'S'. " SD document
*     Read respective table for CAI and fiscal controller.
      PERFORM read_j_1apacd.
    WHEN 'F'.
      PERFORM read_j_1apacd.
      CHECK: ( tab_pac-j_1apacpn  IS INITIAL OR
               kd-koart = 'K' )              AND
             tab_t003_i-xausg     IS INITIAL.             " Note 694277
      IF NOT p_bktxt IS INITIAL.       " read BKTXT if chosen by user
* RG-1361 Changes starts
*        IF bkpf-bktxt(1) = 'C'.
        IF bkpf-bktxt CS 'C'.
          gf_fisc_cont = 'C'.
*          gf_fisc_cont = bkpf-bktxt(1).
          gf_cai = bkpf-bktxt(14).
*          CLEAR: gf_cai, gf_due_date.
          gf_due_date(4)   = bkpf-bktxt+19(4).
          gf_due_date+4(2) = bkpf-bktxt+17(2).
          gf_due_date+6(2) = bkpf-bktxt+15(2).
*          CLEAR: gf_due_date.
* RG-1361 Changes ends
*Only 'C' and PAC for fiscal controller ever was entered into BKPF-BKTXT
        ELSE.
          PERFORM read_cai_corr.
          IF gf_cai IS INITIAL AND
             gf_fisc_cont IS INITIAL AND
             gf_due_date IS INITIAL.
            PERFORM read_j_1apack.
          ENDIF.
        ENDIF.
      ELSE.
        PERFORM read_cai_corr.
        IF gf_cai IS INITIAL AND
           gf_fisc_cont IS INITIAL AND
           gf_due_date IS INITIAL.
          PERFORM read_j_1apack.
        ENDIF.
      ENDIF.
    WHEN 'R'.  " MM doc. - RBKP was read just before calling this form!
      IF NOT p_bktxt IS INITIAL.       " read BKTXT if chosen by user
*       Text field is transferred automatically from MM
*       -> If it is different in FI: It was changed by user on purpose
        IF rbkp-bktxt NE bkpf-bktxt.
          CLEAR rbkp-bktxt.
          MOVE bkpf-bktxt TO rbkp-bktxt.
        ENDIF.
*  RG-1361 Changes starts
*        IF rbkp-bktxt(1) = 'C'.
*          gf_fisc_cont = rbkp-bktxt(1).
*          CLEAR: gf_cai, gf_due_date.
        IF rbkp-bktxt CS 'C'.
          gf_fisc_cont = 'C'.
          gf_cai = rbkp-bktxt(14).
          gf_due_date(4)   = rbkp-bktxt+19(4).
          gf_due_date+4(2) = rbkp-bktxt+17(2).
          gf_due_date+6(2) = rbkp-bktxt+15(2).
*          CLEAR: gf_due_date.
*  RG-1361 Changes ends
        ELSE.
          gf_cai = rbkp-bktxt(14).
          gf_due_date(4)   = rbkp-bktxt+20(4).
          gf_due_date+4(2) = rbkp-bktxt+17(2).
          gf_due_date+6(2) = rbkp-bktxt+14(2).
          CLEAR: gf_fisc_cont.
        ENDIF.
        IF gf_cai IS INITIAL AND
           gf_fisc_cont IS INITIAL AND
           gf_due_date IS INITIAL.
          PERFORM read_cai_corr.
          IF gf_cai IS INITIAL AND
             gf_fisc_cont IS INITIAL AND
             gf_due_date IS INITIAL.
            PERFORM read_j_1apack.
          ENDIF.
        ENDIF.
      ELSE.
        PERFORM read_cai_corr.
        IF gf_cai IS INITIAL AND
           gf_fisc_cont IS INITIAL AND
           gf_due_date IS INITIAL.
          PERFORM read_j_1apack.
        ENDIF.
      ENDIF.
* Note 1091024 Start
    WHEN OTHERS.
      PERFORM read_cai_corr.
      IF gf_cai IS INITIAL AND
         gf_fisc_cont IS INITIAL AND
         gf_due_date IS INITIAL.
        PERFORM read_j_1apack.
      ENDIF.
* Note 1091024 End
  ENDCASE.
ENDFORM. " READ_CAI_AND_FISC_CONT
*&---------------------------------------------------------------------*
*&      Form  read_j_1apacd
*&---------------------------------------------------------------------*
*       Read CAI table
*----------------------------------------------------------------------*
FORM read_j_1apacd .
  DATA: BEGIN OF lcl_tab_pac   OCCURS 10.
          INCLUDE STRUCTURE j_1apacd.
  DATA: END OF lcl_tab_pac.

  CLEAR: gf_cai, gf_fisc_cont.
  CLEAR: tab_pac.                     " Note 694277

  PERFORM read_official_doc_type USING land1 bkpf-blart
                                       bkpf-xblnr+4(1).

  READ TABLE tab_pac WITH KEY bukrs      = bkpf-bukrs
                              brnch      = bkpf-xblnr(4)
                              doccls     = tab_t003_i-doccls
                              j_1aprtchr = bkpf-xblnr+4(1)  " N. 636750
                              j_1apacvd  = bkpf-bldat.
  IF sy-subrc EQ 0.
* RG 1361 Changes starts
*    IF tab_pac-j_1apac(1) EQ 'C'.
    IF NOT tab_pac-j_1apacf IS INITIAL.
      gf_fisc_cont = 'C'.
      gf_cai = tab_pac-j_1apac.
* RG 1361 Changes ends
    ELSE.
      gf_cai = tab_pac-j_1apac.
    ENDIF.
    gf_due_date = tab_pac-j_1apacvd.
  ELSE.
    REFRESH: lcl_tab_pac.
    SELECT * FROM j_1apacd INTO TABLE lcl_tab_pac
                 WHERE bukrs      = bkpf-bukrs
                 AND   brnch      = bkpf-xblnr(4)
                 AND   doccls     = tab_t003_i-doccls
                 AND   j_1aprtchr = bkpf-xblnr+4(1) " Note 636750
                 AND   j_1apacvd  GE bkpf-bldat.
    IF sy-subrc EQ 0.
      SORT lcl_tab_pac BY j_1apacvd ASCENDING.
      READ TABLE lcl_tab_pac INDEX 1.
      MOVE-CORRESPONDING lcl_tab_pac TO tab_pac.
*       Table is filled with EITHER fiscal contr. OR CAI:
* RG 1361 Changes starts
*    IF tab_pac-j_1apac(1) EQ 'C'.
      IF NOT tab_pac-j_1apacf IS INITIAL.
        gf_fisc_cont = 'C'.
        gf_cai = tab_pac-j_1apac.
* RG 1361 Changes ends
      ELSE.
        gf_cai = tab_pac-j_1apac.
      ENDIF.
      gf_due_date = tab_pac-j_1apacvd.
      APPEND tab_pac.
    ELSE.
*     Document types that are neither self-issuing nor having
*     external numbering don't need a log entry.
      CHECK: NOT tab_pac-j_1apacpn  IS INITIAL OR
             NOT tab_t003_i-xausg     IS INITIAL.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'J_1APACD'.
      tab_log_entry1-key     = bkpf-bukrs.
      tab_log_entry1-key+4   = bkpf-xblnr(4).
      tab_log_entry1-key+8   = j_1aotdetr-doccls.
      tab_log_entry1-key+10  = j_1aotdetr-j_1aprtchr.
      CONDENSE tab_log_entry1-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
    ENDIF.
  ENDIF.
ENDFORM. " read_j_1apacd
*&---------------------------------------------------------------------*
*&      Form  READ_J_1APACK
*&---------------------------------------------------------------------*
FORM read_j_1apack.
  DATA: BEGIN OF lcl_tab_pack   OCCURS 10.
          INCLUDE STRUCTURE j_1apack1.
  DATA: END OF lcl_tab_pack.
  DATA: lf_lifnr TYPE lfa1-lifnr.

  CLEAR: gf_cai, gf_fisc_cont, gf_due_date.
  CLEAR lf_lifnr.
  CLEAR: tab_pack.                                         " Note 694521

  IF kd-koart = 'K'.
    lf_lifnr = kd-hkont.
  ENDIF.
  CHECK: NOT lf_lifnr IS INITIAL.
  PERFORM read_official_doc_type USING land1 bkpf-blart
                                       bkpf-xblnr+4(1).
  READ TABLE tab_pack WITH KEY lifnr = lf_lifnr
                               land1 = gf_land1
                               brnch      = bkpf-xblnr(4)
                               doccls     = tab_t003_i-doccls
                               j_1aprtchr = bkpf-xblnr+4(1)
                               j_1apacvd  = bkpf-bldat.
  IF sy-subrc EQ 0.
* RG 1361 Changes starts
*    IF tab_pack-j_1apac(1) EQ 'C'.
*      gf_fisc_cont = tab_pack-j_1apac(1).
    IF NOT tab_pack-j_1apacf IS INITIAL.
      gf_fisc_cont = 'C'.
      gf_cai = tab_pack-j_1apac.
* RG 1361 Changes ends
    ELSE.
      gf_cai = tab_pack-j_1apac.
    ENDIF.
    gf_due_date = tab_pack-j_1apacvd.
  ELSE.
    REFRESH: lcl_tab_pack.
    SELECT * FROM j_1apack1 INTO TABLE lcl_tab_pack
                            WHERE  lifnr = lf_lifnr
                            AND    land1 = gf_land1
                            AND    brnch      = bkpf-xblnr(4)
                            AND    doccls     = tab_t003_i-doccls
                            AND    j_1aprtchr = bkpf-xblnr+4(1)
                            AND    j_1apacvd  GE bkpf-bldat.
    IF sy-subrc EQ 0.
      SORT lcl_tab_pack BY j_1apacvd ASCENDING.
      READ TABLE lcl_tab_pack INDEX 1.
      MOVE-CORRESPONDING lcl_tab_pack TO tab_pack.
*     Table is filled with EITHER fiscal contr. OR CAI:
* RG 1361 Changes starts
*    IF tab_pack-j_1apac(1) EQ 'C'.
*      gf_fisc_cont = tab_pack-j_1apac(1).
      IF NOT tab_pack-j_1apacf IS INITIAL.
        gf_fisc_cont = 'C'.
        gf_cai = tab_pack-j_1apac.
* RG 1361 Changes ends
      ELSE.
        gf_cai = tab_pack-j_1apac.
      ENDIF.
      gf_due_date = tab_pack-j_1apacvd.
      APPEND tab_pack.
    ELSE.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'J_1APACK1'.
      tab_log_entry1-key     = lf_lifnr.
      tab_log_entry1-key+14   = bkpf-xblnr(4).
      tab_log_entry1-key+18   = tab_t003_i-doccls.
      tab_log_entry1-key+20  = bkpf-xblnr+4(1).
      CONDENSE tab_log_entry1-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
    ENDIF.
  ENDIF.
ENDFORM. " READ_J_1APACK
*&---------------------------------------------------------------------*
*&      Form  read_cai_corr
*&---------------------------------------------------------------------*
FORM read_cai_corr.
  CLEAR: gf_cai, gf_fisc_cont, gf_due_date.

  CLEAR: xtline.
  REFRESH: xtline, tab_bkorm.

  SELECT * FROM bkorm
           WHERE bukrs = bkpf-bukrs
             AND event = p_cai
             AND koart = space
             AND belnr = bkpf-belnr
             AND gjahr = bkpf-gjahr.
    MOVE-CORRESPONDING  bkorm TO tab_bkorm.
    APPEND tab_bkorm.
  ENDSELECT.
  IF sy-subrc EQ 0.
    SORT tab_bkorm BY datum DESCENDING uzeit DESCENDING.
    READ TABLE tab_bkorm INDEX 1.
    text_name = tab_bkorm-param+38(30).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = 'FIKO'
        language = tab_bkorm-param+78(1)
        name     = text_name
        object   = 'BKORM'
      TABLES
        lines    = xtline.

    LOOP AT xtline.
      CASE sy-tabix.
        WHEN 1.
* RG 1361 Changes starts
*          IF xtline-tdline(1) = 'C'.
*            gf_fisc_cont = xtline-tdline(1).
*          IF xtline-tdline CS 'C'.            "986034
          IF xtline-tdline+0(16) CS 'C'.                    "986034
            gf_fisc_cont = 'C'.
            gf_cai = xtline-tdline(14).
* RG 1361 Changes ends
          ELSE.
            gf_cai = xtline-tdline(14).
          ENDIF.
        WHEN 2.
          gf_due_date(4)   = xtline-tdline+6(4).
          gf_due_date+4(2) = xtline-tdline+3(2).
          gf_due_date+6(2) = xtline-tdline(2).
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM. " read_cai_corr
*&---------------------------------------------------------------------*
*&      Form  READ_CUSTOMS_DATA
*&---------------------------------------------------------------------*
FORM read_customs_data.
  CLEAR: xtline, wa_custom.
  REFRESH: xtline, tab_bkorm.

  SELECT * FROM bkorm
           WHERE bukrs = tab_ep-bukrs
             AND event = s_event
             AND koart = space
             AND belnr = tab_ep-belnr
             AND gjahr = tab_ep-gjahr.
    MOVE-CORRESPONDING  bkorm TO tab_bkorm.
    APPEND tab_bkorm.
  ENDSELECT.
  IF sy-subrc EQ 0.
    SORT tab_bkorm BY datum DESCENDING uzeit DESCENDING.
    READ TABLE tab_bkorm INDEX 1.
    text_name = tab_bkorm-param+38(30).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = 'FIKO'
        language = tab_bkorm-param+78(1)
        name     = text_name
        object   = 'BKORM'
      TABLES
        lines    = xtline.

    LOOP AT xtline.
      CASE sy-tabix.
        WHEN 1.
          wa_custom-number = xtline-tdline(8).
        WHEN 2.
          wa_custom-code = xtline-tdline(3).
        WHEN 3.
          wa_custom-year = xtline-tdline(4).
        WHEN 4.
          wa_custom-date(4)   = xtline-tdline+6(4).
          wa_custom-date+4(2) = xtline-tdline+3(2).
          wa_custom-date+6(2) = xtline-tdline(2).
        WHEN 5.
          wa_custom-destcd = xtline-tdline(4).
        WHEN 6.
          wa_custom-chksum = xtline-tdline(1).
          EXIT.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM. " READ_CUSTOMS_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_J_1ADRVER
*&---------------------------------------------------------------------*
*       Read version of daily VAT-Reportings for selected bukrs.
*----------------------------------------------------------------------*
*      -->BUKRS      company code                                      *
*      -->ADRVER     version                                           *
*----------------------------------------------------------------------*
FORM read_j_1adrver USING bukrs LIKE t001-bukrs
                          adrver LIKE j_1adrver-j_1adrver.

  CLEAR tab_j_1adrver.
  tab_j_1adrver-bukrs = bukrs.
  tab_j_1adrver-j_1adrver = adrver.
*  READ TABLE tab_j_1adrver.                                "1032335
  READ TABLE tab_j_1adrver WITH KEY
             bukrs = bukrs
             j_1adrver = adrver.                            "1032335
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM j_1adrver
    WHERE bukrs     = bukrs
    AND   j_1adrver = adrver.
    IF sy-subrc NE 0.
      IF sy-batch <> space.
        MESSAGE s847 WITH bukrs.
        MESSAGE s207(f7) WITH sy-repid.
        STOP.
      ELSE.
        MESSAGE a847 WITH bukrs.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING j_1adrver TO tab_j_1adrver.
    IF tab_j_1adrver-j_1aproc IS INITIAL.
      MESSAGE e848 WITH bukrs adrver.
    ENDIF.
    APPEND tab_j_1adrver.
  ENDIF.
ENDFORM. " READ_J_1ADRVER

*&---------------------------------------------------------------------*
*&      Form  READ_J_1ADRVERT
*&---------------------------------------------------------------------*
*       Read text for version
*----------------------------------------------------------------------*
*      -->BUKRS  company code
*----------------------------------------------------------------------*
FORM read_j_1adrvert USING bukrs LIKE t001-bukrs.
  CLEAR tab_j_1adrvert.
  tab_j_1adrvert-spras = sy-langu.
  tab_j_1adrvert-bukrs = bukrs.
  tab_j_1adrvert-j_1adrver = s_drver.
*  READ TABLE tab_j_1adrvert.                               "1032335
  READ TABLE tab_j_1adrvert WITH KEY
             spras = tab_j_1adrvert-spras
             bukrs = tab_j_1adrvert-bukrs
             j_1adrver = s_drver.                           "1032335
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM j_1adrvert
                 WHERE spras   = sy-langu
                 AND   bukrs   = bukrs
                 AND j_1adrver = s_drver.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING j_1adrvert TO tab_j_1adrvert.
      APPEND tab_j_1adrvert.
    ELSE.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'J_1ADRVERT'.
      tab_log_entry1-key     = sy-langu.
      tab_log_entry1-key+1   = bukrs.
      tab_log_entry1-key+5   = s_drver.
      CONDENSE tab_log_entry1-key NO-GAPS.
      COLLECT tab_log_entry1.
    ENDIF.
  ENDIF.
ENDFORM. " READ_J_1ADRVERT

*&---------------------------------------------------------------------*
*&      FORM read_j_1arztx
*&---------------------------------------------------------------------*
*       Reason for zero VAT
*----------------------------------------------------------------------*
*      -->KALSM  calculation scheme
*      -->MWSKZ  taxcode
*----------------------------------------------------------------------*
FORM read_j_1arztx USING kalsm LIKE t007a-kalsm
                         mwskz LIKE t007a-mwskz.
  DATA: lcl_zerotest TYPE p DECIMALS 7,                      " Note 486095
        lcl_zerocomp TYPE p DECIMALS 7 VALUE '0.0000000'.    " Note 486095

  CLEAR: lcl_zerotest.                                     " Note 486095
  rcode = 0.
  CLEAR xrztx.
  READ TABLE xrztx WITH KEY kalsm = kalsm
                             mwskz = mwskz.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM j_1arztx
      WHERE kalsm EQ kalsm
      AND mwskz EQ mwskz.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING j_1arztx TO xrztx.
      APPEND xrztx.
    ELSE.
      xrztx-kalsm = kalsm.
      xrztx-mwskz = mwskz.
      IF sum_tab_taxes-amount2 = 0.
        CHECK: flg_is_beleg = 'F'.
        APPEND xrztx. "For same tax code do not check again. Note 486095
* Note 486095: Check whether tax amount is zero because of rounding
*              by calculating the tax amount with all relevant decimals
* Remark: Attribute 'Fixed point arithmetic' is not set in this report
        lcl_zerotest = tab_bset-hwbas * tab_bset-kbetr.    " Note 486095
* Write log entry only if tax really is zero even after check
        IF lcl_zerotest EQ lcl_zerocomp.                   " Note 486095
          CLEAR tab_log_entry1.
          tab_log_entry1-name  = 'J_1ARZTX'.
          tab_log_entry1-key   = kalsm.
          tab_log_entry1-key+6 = mwskz.
          PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                               tab_log_entry1-key
                                               tab_log_entry1-belnr
                                               bkpf-belnr.
          COLLECT tab_log_entry1.
        ENDIF.                                             " Note 486095
      ENDIF.
    ENDIF.
  ENDIF.
  IF xrztx-j_1arfz IS INITIAL.
    rcode = 4.
  ENDIF.
ENDFORM. " READ_J_1ARZTX

*&---------------------------------------------------------------------*
*&      Form  READ_J_1ATAXID
*&---------------------------------------------------------------------*
*       read tax identification from table    j_1ataxid / tab_j_1ataxid
*----------------------------------------------------------------------*
*      -->KALSM  calculation scheme                                    *
*      -->KTOSL  processing key                                        *
*----------------------------------------------------------------------*
FORM read_j_1ataxid USING kalsm LIKE t007a-kalsm
                          ktosl LIKE bset-ktosl.
  CLEAR tab_j_1ataxid.
  tab_j_1ataxid-kalsm = kalsm.
  tab_j_1ataxid-ktosl = ktosl.
*  READ TABLE tab_j_1ataxid.                                "1032335
  READ TABLE tab_j_1ataxid WITH KEY
             kalsm = kalsm
             ktosl = ktosl.                                 "1032335
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM j_1ataxid
        WHERE kalsm = kalsm
        AND   ktosl = ktosl.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING j_1ataxid TO tab_j_1ataxid.
      APPEND tab_j_1ataxid.
    ENDIF.
    IF  j_1ataxid-j_1ataxid IS INITIAL.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'J_1ATAXID'.
      tab_log_entry1-key     = kalsm.
      tab_log_entry1-key+10  = ktosl.
      CONDENSE tab_log_entry1-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
      flg_reject_doc = '1'.
    ENDIF.
  ENDIF.
ENDFORM. " READ_J_1ATAXID

*&---------------------------------------------------------------------*
*&      Form  READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->KNUMH  condition number
*----------------------------------------------------------------------*
FORM read_konp USING knumh LIKE konv-knumh.

  CLEAR tab_konp.
  tab_konp-knumh = knumh.
  tab_konp-kopos = '01'.
*  READ TABLE tab_konp.                                     "1032335
  READ TABLE tab_konp WITH KEY
             knumh = tab_konp-knumh
             kopos = tab_konp-kopos.                        "1032335
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM konp
                   WHERE knumh EQ knumh
                   AND   kopos EQ '01'.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING konp TO tab_konp.
      APPEND tab_konp.
    ELSE.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'KONP'.
      tab_log_entry1-key     = knumh.
      tab_log_entry1-key+10  = '01'.
      CONDENSE tab_log_entry1-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
      flg_reject_doc = '1'.
    ENDIF.
  ENDIF.
ENDFORM. " READ_KONP

*&---------------------------------------------------------------------*
*&      Form  READ_OFFICIAL_DOC_TYPE
*&---------------------------------------------------------------------*
*      -->DOC_TYPE  document type
*      -->XBLNR     official document number
*----------------------------------------------------------------------*
FORM read_official_doc_type USING land1 LIKE t003_i-land1
                                  blart LIKE bkpf-blart
                                  prtch TYPE c.
* read Belegarten
  DATA: count TYPE i.

  CLEAR tab_t003_i.
  READ TABLE tab_t003_i  WITH KEY blart = blart.
  IF sy-subrc NE 0.
* for release 46c and above, table T003_I should be used for official
* document numbering
    SELECT COUNT(*) INTO count FROM t003_i WHERE land1 = land1.
    IF NOT count IS INITIAL.
      SELECT SINGLE * FROM t003_i
                       WHERE land1 = land1 AND
         blart = blart.
      IF sy-subrc = 0.
        CASE t003_i-offnrel.
          WHEN 'B' OR 'C'.
            t003_i-xausg = 'X'.
          WHEN 'A' OR 'Z'.
            t003_i-xausg = ' '.
          WHEN OTHERS.
        ENDCASE.
        MOVE-CORRESPONDING t003_i TO tab_t003_i.

*    else.
*     SELECT SINGLE * FROM T003
*                             WHERE BLART = BLART.
*      MOVE-CORRESPONDING T003 TO TAB_T003_I.
*      move t003-blkls to tab_t003_i-doccls.
*
      ENDIF.
    ENDIF.

    IF sy-subrc NE 0.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'T003_I'.
      tab_log_entry1-key     = blart.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
    ELSE.
* read document type text
      SELECT SINGLE * FROM t003t
              WHERE spras EQ sy-langu
              AND blart EQ blart.
      IF sy-subrc NE 0.
        CLEAR tab_log_entry1.
        tab_log_entry1-name    = 'T003T'.
        tab_log_entry1-key     = sy-langu.
        tab_log_entry1-key+1   = blart.
        CONDENSE tab_log_entry1-key NO-GAPS.
        PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                             tab_log_entry1-key
                                             tab_log_entry1-belnr
                                             bkpf-belnr.
        COLLECT tab_log_entry1.
      ELSE.
        tab_t003_i-ltext = t003t-ltext.
      ENDIF.
      APPEND tab_t003_i.
    ENDIF.
  ENDIF.

* For magnetic output: Read official document type from new
* customizing table J_1AOTDETR

  CLEAR tab_j_1aotdet.
*Country and Report ID are new key fields in J_1AOTDETR - cp. J_1APTDET
  READ TABLE tab_j_1aotdet WITH KEY land1 = t001-land1
                                    id_report  = sy-repid
                                    doccls = tab_t003_i-doccls
                                    j_1aprtchr = prtch.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM  j_1aotdetr
                 WHERE land1      = t001-land1
                 AND   id_report  = sy-repid
                 AND   doccls     = tab_t003_i-doccls
                 AND   j_1aprtchr = prtch.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM j_1aoftpt
            WHERE j_1aoftp = j_1aotdetr-j_1aoftp
            AND   spras    = sy-langu.
      MOVE-CORRESPONDING j_1aotdetr TO tab_j_1aotdet.
      tab_j_1aotdet-text5 = j_1aoftpt-text5.
      tab_j_1aotdet-text30 = j_1aoftpt-text30.
      APPEND tab_j_1aotdet.
      IF j_1aoftpt-text5 IS INITIAL.
        CLEAR tab_log_entry1.
        tab_log_entry1-name    = 'J_1AOFTPT'.
        tab_log_entry1-key     = j_1aotdetr-j_1aoftp.
        tab_log_entry1-key+2   = sy-langu.
        CONDENSE tab_log_entry1-key NO-GAPS.
        PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                             tab_log_entry1-key
                                             tab_log_entry1-belnr
                                             bkpf-belnr.
        COLLECT tab_log_entry1.
      ENDIF.
    ELSE.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'J_1AOTDETR'.
      tab_log_entry1-key     = tab_t003_i-doccls.
      tab_log_entry1-key+1   = bkpf-xblnr+4(1).
      CONDENSE tab_log_entry1-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
    ENDIF.
  ENDIF.
ENDFORM. " READ_OFFICIAL_DOC_TYPE

*&---------------------------------------------------------------------*
*&      Form  READ_T001
*&---------------------------------------------------------------------*
*      -->BUKRS company code
*----------------------------------------------------------------------*
FORM read_t001 USING bukrs LIKE t001-bukrs.
  CLEAR tab_001.
  tab_001-bukrs = bukrs.
*  READ TABLE tab_001.                                      "1032335
  READ TABLE tab_001 WITH KEY bukrs = bukrs.                "1032335

  IF sy-subrc NE 0.
    SELECT SINGLE * FROM t001
      WHERE bukrs EQ bukrs.
    IF sy-subrc NE 0.
      IF sy-batch <> space.
*  Company code & does not exist
        MESSAGE s116(f7) WITH bukrs.
        MESSAGE s207(f7) WITH sy-repid.
        STOP.
      ELSE.
        MESSAGE a116(f7) WITH bukrs.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING t001 TO tab_001.
* get company code's local currency
      SELECT SINGLE * FROM t005
        WHERE land1 EQ t001-land1.
      IF sy-subrc NE 0.
        IF sy-batch <> space.
          MESSAGE s223(f7) WITH t001-land1.
          MESSAGE s207(f7) WITH sy-repid.
          STOP.
        ELSE.
          MESSAGE a223(f7) WITH t001-land1.
        ENDIF.
      ELSE.
        tab_001-kalsm = t005-kalsm.
      ENDIF.
* Further information about company code
      SELECT SINGLE * FROM t001z WHERE bukrs = bukrs
                                 AND   party = 'J1ATID'.
      IF sy-subrc EQ 0.
        tab_001-stcdt = t001z-paval.
      ELSE.
        CLEAR tab_log_entry1.
        tab_log_entry1-name    = 'T001Z'.
        tab_log_entry1-key     = bukrs.
        tab_log_entry1-key+4   = 'J1ATID'.
        CONDENSE tab_log_entry1-key NO-GAPS.
        COLLECT tab_log_entry1.
      ENDIF.

      SELECT SINGLE * FROM t001z WHERE bukrs = bukrs
                                 AND   party = 'J1AIDN'.
      IF sy-subrc EQ 0.
        tab_001-stcd1 = t001z-paval.
      ELSE.
        CLEAR tab_log_entry1.
        tab_log_entry1-name    = 'T001Z'.
        tab_log_entry1-key     = bukrs.
        tab_log_entry1-key+4   = 'J1AIDN'.
        CONDENSE tab_log_entry1-key NO-GAPS.
        COLLECT tab_log_entry1.
      ENDIF.

* Begin: Note 636750
      SELECT SINGLE * FROM t001z WHERE bukrs = bukrs
                                 AND   party = 'J1AFTV'.
      IF sy-subrc EQ 0.
        tab_001-fityp = t001z-paval.
      ELSE.
        CLEAR tab_log_entry1.
        tab_log_entry1-name    = 'T001Z'.
        tab_log_entry1-key     = bukrs.
        tab_log_entry1-key+4   = 'J1AFTV'.
        CONDENSE tab_log_entry1-key NO-GAPS.
        COLLECT tab_log_entry1.
      ENDIF.
* End:   Note 636750

* address maintenance, company data
      CLEAR: addr1_sel, sadr.
      addr1_sel-addrnumber = t001-adrnr.                    "SADR40A
      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          address_selection = addr1_sel
          address_group     = 'CA01'
        IMPORTING
          sadr              = sadr.
      tab_001-name1 = sadr-name1.

* end of all infos for the bukrs..............
      APPEND tab_001.
    ENDIF.
  ENDIF.
ENDFORM.                                                    " READ_T001

*&---------------------------------------------------------------------*
*&      FORM READ_T007A
*&---------------------------------------------------------------------*
*      -->KALSM  calculation scheme
*      -->MWSKZ  taxcode
*----------------------------------------------------------------------*
FORM read_t007a USING kalsm LIKE t007a-kalsm
                      mwskz LIKE t007a-mwskz.

  CLEAR tab_007a.
  tab_007a-kalsm = kalsm.
  tab_007a-mwskz = mwskz.
*  READ TABLE tab_007a.                                     "1032335
  READ TABLE tab_007a WITH KEY
          kalsm = kalsm
          mwskz = mwskz.                                    "1032335

  IF sy-subrc NE 0.
    SELECT SINGLE * FROM t007a
        WHERE kalsm EQ kalsm
        AND mwskz EQ mwskz.
    IF sy-subrc NE 0.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'T007A'.
      tab_log_entry1-key     = kalsm.
      tab_log_entry1-key+6   = mwskz.
      CONDENSE tab_log_entry1-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
      flg_reject_doc = '1'.
    ELSE.
      MOVE-CORRESPONDING t007a TO tab_007a.
      SELECT SINGLE * FROM t007s
              WHERE spras EQ sy-langu
              AND kalsm EQ kalsm
              AND mwskz EQ mwskz.
      IF sy-subrc NE 0.
        CLEAR tab_log_entry1.
        tab_log_entry1-name    = 'T007S'.
        tab_log_entry1-key     = sy-langu.
        tab_log_entry1-key+1   = kalsm.
        tab_log_entry1-key+7   = mwskz.
        CONDENSE tab_log_entry1-key NO-GAPS.
        PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                             tab_log_entry1-key
                                             tab_log_entry1-belnr
                                             bkpf-belnr.
        COLLECT tab_log_entry1.
      ELSE.
        tab_007a-text1 = t007s-text1.
      ENDIF.
      SELECT SINGLE * FROM j_1arztx
              WHERE kalsm EQ kalsm
              AND mwskz EQ mwskz.
      IF sy-subrc EQ 0.
        tab_007a-j_1arfz = j_1arztx-j_1arfz.
      ENDIF.
      APPEND tab_007a.
    ENDIF.
  ENDIF.
ENDFORM. " READ_T007A

*----------------------------------------------------------------------*
* FORM READ_T007B                                                      *
*----------------------------------------------------------------------*
*      -->KTOSL  processing key                                        *
*----------------------------------------------------------------------*
FORM read_t007b USING ktosl LIKE t007b-ktosl.
  CLEAR tab_007b.
  tab_007b-ktosl = ktosl.
*  READ TABLE tab_007b.                                     "1032335
  READ TABLE tab_007b WITH KEY ktosl = ktosl.               "1032335
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM t007b
        WHERE ktosl = ktosl.
    IF sy-subrc NE 0.
      CLEAR tab_log_entry1.
      tab_log_entry1-name    = 'T007B'.
      tab_log_entry1-key     = ktosl.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry1-name
                                           tab_log_entry1-key
                                           tab_log_entry1-belnr
                                           bkpf-belnr.
      COLLECT tab_log_entry1.
      flg_reject_doc = '1'.
    ELSE.
      MOVE-CORRESPONDING t007b TO tab_007b.
      APPEND tab_007b.
    ENDIF.
  ENDIF.
ENDFORM. " READ_T007B

*----------------------------------------------------------------------*
* Subroutines for Diverse Checks
*----------------------------------------------------------------------*
* CHECK_TAB_BSEG
* CHECK_TAB_BSEG_NODK
* CHECK_IF_EXEMPTED
* CHECK_KTOSL_SELECTION
* CHECK_MM_BASE_AMOUNTS
* CHECK_NEWTAXID
* CHECK_SD_AMOUNTS
* CHECK_SPECIAL_GL_AMOUNT
* CHECK_SD_FOREIGN_CURRENCY
* CHECK_TAX_CODE
* CHECK_FOREIGN_SD_DOCS
* CHECK_FOR_ZERO_LINE
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_TAB_BSEG
*&---------------------------------------------------------------------*
*  -->  p1        bkpf,tab_bseg,flg_is_beleg
*  <--  p2       sd_reasons_set, kd, sd_docm, sd_disc_fact,xumsks,     *
*                xaugdt,xaugbl,amnt, line_total.
*----------------------------------------------------------------------*
FORM check_tab_bseg.
  CLEAR:  sd_reasons_set, kd, sd_docm, sd_disc_fact,xumsks,
          xaugdt,xaugbl,amnt.

  LOOP AT tab_bseg.                    " only customer vendor lines

* customer can use vendor or customer accounts as crossposting accounts
* these have to be omitted in order for the report to show the correct
* vendor or customer information
    IF tab_bseg-ktosl NE 'BUV'.  "deselect intercompany 'K' or 'D' lines

      IF tab_bseg-koart = 'K'.
        MOVE-CORRESPONDING tab_bseg TO kd.
        kd-hkont = tab_bseg-lifnr.
      ELSEIF tab_bseg-koart = 'D'.
        MOVE-CORRESPONDING tab_bseg TO kd.
        kd-hkont = tab_bseg-kunnr.
* Note 1085024 Start
        IF bkpf-adisc = 'S'.               " SD discount ?
          sd_docm      = tab_bseg-esrre.   " get the ref. invoice
          sd_disc_fact = tab_bseg-esrre+18(9). " Save factor
        ENDIF.
***** cash sales
      ELSEIF tab_bseg-koart = 'S' AND NOT tab_bseg-ktnra IS INITIAL.
        MOVE-CORRESPONDING tab_bseg TO kd.
* Note 1085024 End
      ENDIF.
    ENDIF.
* check down payment
    IF tab_bseg-umsks = 'A'.           " down payment ?
      PERFORM get_downpayment_requests." tab_bseg is used ==>
      xumsks = 'X'.
    ELSE.
      CHECK tab_bseg-xzahl IS INITIAL.
    ENDIF.
* set the compensation date
    IF tab_bseg-augbl   NE space AND   " get the first
       ( tab_bseg-augdt LE xaugdt OR   " compensation date
       xaugdt     IS INITIAL ).
      xaugdt = tab_bseg-augdt.

      IF tab_bseg-augbl LE xaugbl OR   " and document no.
         xaugbl     IS INITIAL.
        xaugbl = tab_bseg-augbl.
      ENDIF.
    ENDIF.
* calculate amnt
    IF tab_bseg-ktosl NE 'BUV'.        " Note 309161
      " deselect intercompany 'K' or 'D' lines
      amnt-total = amnt-total + tab_bseg-dmbtr.
    ENDIF.                             " Note 309161

    IF bkpf-xmwst NE space.
      amnt-total = amnt-total + tab_bseg-mwsts.
    ENDIF.
* Check for documents (BSEG-UMSKS not initial) with special G/L item
* performed in fill_tab_beleg: FORM CHECK_SPECIAL_GL_AMOUNT

* set the line total
    IF tab_bseg-umsks = 'A'.
* add the base amount to the line totals for down payments
      PERFORM check_tax_code USING tab_bseg-mwskz.

      CLEAR line_total.
      line_total-mwskz = tab_bseg-mwskz.
      line_total-kposn = tab_bseg-txgrp."use txgrp as substitute for kp
      line_total-dmbtr = tab_bseg-dmbtr.
      COLLECT line_total.
    ENDIF.
  ENDLOOP.                             " tab_bseg

* Event_004
*    <BEGIN OF ENHANCEMENT>
*  CALL FUNCTION 'OPEN_FI_PERFORM_XXXXXX004_X'
*       EXPORTING
*            I_BKPF         = BKPF
*       CHANGING
*            E_SD_DOCM      = SD_DOCM
*            E_SD_DISC_FACT = SD_DISC_FACT
*       TABLES
*            TAB_BSEG       = TAB_BSEG.
*    <END OF ENHANCEMENT>

ENDFORM. " CHECK_TAB_BSEG

*&---------------------------------------------------------------------*
*&      Form  CHECK_TAB_BSEG_NODK
*&---------------------------------------------------------------------*
*  -->  p1        bkpf,tab_bseg_nodk,flg_is_beleg, line_total
*  <--  p2       line_total,dis_net_amount,pline,man_tax        -------*
*----------------------------------------------------------------------*
FORM check_tab_bseg_nodk.
  CLEAR:   dis_net_amount.
  REFRESH: plines,man_tax,mm_taxed.
  LOOP AT tab_bseg_nodk.               " no vendor line no customer line
    IF flg_is_beleg = 'S'.             " SD doc or SD discount,
      IF tab_bseg_nodk-mwart IS INITIAL.    " not a tax account?
* get the net amount from th FI amount; it is needed for the calculation
* of the net amount per sd item (rounding problems)
        ADD tab_bseg_nodk-dmbtr TO dis_net_amount.
      ENDIF.
    ENDIF.
*  elseif bkpf-awtyp      ne 'VBRK' and " deselect SD documents
*         bkpf-adisc ne 'S'.            " deselect SD discounts
    IF flg_is_beleg NE 'S'.
      PERFORM check_tax_code USING tab_bseg_nodk-mwskz.

      CLEAR line_total.
      line_total-mwskz = tab_bseg_nodk-mwskz.
      line_total-kposn = tab_bseg_nodk-txgrp.  " use txgrp as substitute
      line_total-dmbtr = tab_bseg_nodk-dmbtr.
      COLLECT line_total.
    ENDIF.
* Änderung für Belege ohne Sachkontenzeile, da direkt gegen Steuerkonto
* gebucht (KOART = 'S', aber MWART <> INITIAL) - rückgängig wg. 120834
*         tab_bseg_nodk-koart      ne 'K',
*         tab_bseg_nodk-koart      ne 'D',
    CHECK: tab_bseg_nodk-mwart      IS INITIAL,   " no tax accounts
                                                          "Note 782267
           bkpf-adisc      NE 'S'.     " deselect SD discount
* Event_005
*    <BEGIN OF ENHANCEMENT>
*    CALL FUNCTION 'OPEN_FI_PERFORM_XXXXXX005_X'
*         EXPORTING
*              I_BKPF            = BKPF
*         EXCEPTIONS
*              DISCOUNT_DOCUMENT = 1
*              OTHERS            = 2.
*    CHECK SY-SUBRC NE 1.
*    <END OF ENHANCEMENT>

    IF NOT tab_bseg_nodk-mwskz IS INITIAL.       " Note 782267
*** totals for manual taxes
* fill table to know, which lines have been posted with tax.
* the lines, which are not in that table, are only tax postings
      MOVE-CORRESPONDING tab_bseg_nodk TO plines.
      COLLECT plines.
    ENDIF.

    IF flg_is_beleg = 'R'.

      IF NOT tab_bseg_nodk-ktosl IS INITIAL
       AND NOT tab_bseg_nodk-buzid IS INITIAL
       AND tab_bseg_nodk-vorgn = 'RMRP'.
        CLEAR mm_taxed.
        CASE tab_bseg_nodk-xauto.      " totals for manual taxes
          WHEN ' '.
            IF tab_bseg_nodk-buzid <> ' '.
              mm_taxed-mwskz = tab_bseg_nodk-mwskz.
              mm_taxed-txgrp = tab_bseg_nodk-txgrp.
*        MM_TAXED-DMBTR = TAB_BSEG_NODK-DMBTR.
              COLLECT mm_taxed.
            ENDIF.
*      when 'X'.
*      if tab_bseg_nodk-buzid = ' '.
*        MM_TAXED-MWSKZ = TAB_BSEG_NODK-MWSKZ.
*        MM_TAXED-TXGRP = TAB_BSEG_NODK-TXGRP.
**        MM_TAXED-DMBTR = TAB_BSEG_NODK-DMBTR.
*        COLLECT MM_TAXED.
*      endif.
        ENDCASE.                       " totals for manual taxes
      ENDIF.

    ENDIF.
* manual taxes                            "note 159314
    IF  tab_bseg_nodk-xauto IS INITIAL                     "note 159314
    AND tab_bseg_nodk-ktosl IS INITIAL                     "note 159314
    AND tab_bseg_nodk-mwart IS INITIAL                     "note 159314
    AND tab_bseg_nodk-buzid IS INITIAL                     "note 159314
    AND tab_bseg_nodk-vorgn = 'RMRP'.                      "note 159314
* Get the mm-document.                     note 304768
      CLEAR: xmbelnr,xmgjahr.
      xmgjahr = bkpf-awkey+10(4).
      xmbelnr = bkpf-awkey(10).
      SELECT SINGLE * FROM rbkp INTO *rbkp WHERE belnr = xmbelnr
                                             AND gjahr = xmgjahr.
      IF sy-subrc = 0.
        IF rbkp-ivtyp IS INITIAL.
          MOVE tab_bseg_nodk-mwskz TO man_tax-mwskz.       "note 159314
          COLLECT man_tax.                                 "note 159314
        ENDIF.
      ENDIF.
*                                                           note 304768
    ENDIF.                                                 "note 159314
  ENDLOOP.                             " tab_bseg_nodk
ENDFORM. " CHECK_TAB_BSEG_NODK

*&---------------------------------------------------------------------*
*&      Form  CHECK_IF_EXEMPTED
*&---------------------------------------------------------------------*
*       Check if reason for zero VAT exists and if yes,
*       move amounts to respective column
*----------------------------------------------------------------------*
*      -->J_1ARFZ  Reason for zero tax
*----------------------------------------------------------------------*
FORM check_if_exempted USING j_1arfz LIKE j_1arztx-j_1arfz .

  PERFORM read_j_1arztx USING tab_001-kalsm sum_tab_taxes-mwskz.
  IF sum_tab_taxes-amount2 = 0 OR      " no taxes
     xrztx-j_1arfz NE space.           " Note 309998
    IF j_1arfz NE space.
* Note 1091024 Start
      IF j_1arfz NE 'N'.
        tab_bseg_taxed-exemption = tab_bseg_taxed-taxed.
      ENDIF.
* Note 1091024 End
      CLEAR tab_bseg_taxed-taxed.
    ELSE.
      IF rcode = 0.
        IF sum_tab_taxes-not_taxed = 0
        AND sum_tab_taxes-vat_amount = 0.                   "1114190
          tab_bseg_taxed-exemption = tab_bseg_taxed-taxed.
          CLEAR tab_bseg_taxed-taxed.
        ENDIF.
      ENDIF.
    ENDIF.
* Begin Note 442721
* Problem: Exempt reasons for SD are not in table J_1ARZTX
* And: If e.g. other perception is involved, field
* sum_tab_taxes-amount2 ne 0, but base amount is exempted nevertheless
* ->Need to evaluate exempt reasons for SD separately
* No log entry for exempt reason from SD, as not defined in J_1ARZTX!
  ELSE.
    IF flg_is_beleg = 'S'. " SD documents only
      READ TABLE tab_bset WITH KEY mwskz  = sum_tab_taxes-mwskz
                                   xkposn = sum_tab_taxes-posnr.
      IF sy-subrc = 0.     " One entry found
        IF tab_bset-j_1arfz NE space.  " Exempt reason from SD (VBRP)
* Note 1091024 Start
          IF j_1arfz NE 'N'.
            tab_bseg_taxed-exemption = tab_bseg_taxed-taxed.
          ENDIF.
* Note 1091024 End
          CLEAR tab_bseg_taxed-taxed.
        ENDIF.
      ENDIF.
    ENDIF.
* End Note 442721
  ENDIF.
ENDFORM. " CHECK_IF_EXEMPTED

*&---------------------------------------------------------------------*
*&      Form  CHECK_KTOSL_SELECTION
*&---------------------------------------------------------------------*
FORM check_ktosl_selection.
  DATA: BEGIN OF tmp_tab_mwskz OCCURS 10,
          mwskz LIKE bset-mwskz,
        END OF tmp_tab_mwskz.

  REFRESH: tmp_tab_mwskz.
* check the KTOSL in SEL_KTS1 or/and SEL_KTS2.
  CASE flg_checking_ktsol.
    WHEN '0'.                          " no check for SEL_KTS1,SEL_KTS2.
      CLEAR: flg_reject_doc.

    WHEN '1'.       " no check for SEL_KTS1 but check SEL_KTS2.
      LOOP AT tab_beleg.
        IF tab_beleg-ktosl IN sel_kts2.
          tmp_tab_mwskz-mwskz = tab_beleg-mwskz.
          COLLECT tmp_tab_mwskz.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE tmp_tab_mwskz LINES tmp_cnt1.
      IF NOT tmp_cnt1 IS INITIAL.
        REFRESH tmp_tab_beleg.
        LOOP AT tmp_tab_mwskz.
          LOOP AT tab_beleg.
            IF tmp_tab_mwskz-mwskz EQ tab_beleg-mwskz.
              tmp_tab_beleg = tab_beleg.
              APPEND tmp_tab_beleg.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        REFRESH tab_beleg.
        LOOP AT tmp_tab_beleg.
          tab_beleg = tmp_tab_beleg.
          APPEND tab_beleg.
        ENDLOOP.
        CLEAR: flg_reject_doc.
      ENDIF.

    WHEN '2'.   " no check for SEL_KTS2 but SEL_KTS1.
      LOOP AT tab_beleg.
        IF tab_beleg-ktosl IN sel_kts1.
          tmp_tab_mwskz-mwskz = tab_beleg-mwskz.
          COLLECT tmp_tab_mwskz.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE tmp_tab_mwskz LINES tmp_cnt1.
      IF NOT tmp_cnt1 IS INITIAL.
        CLEAR: flg_reject_doc.
      ENDIF.

    WHEN '3'.                          " check for SEL_KTS1 or SEL_KTS2
* when both selected, sel_kts1 has higher priority
      LOOP AT tab_beleg.
        IF tab_beleg-ktosl IN sel_kts1.
          tmp_tab_mwskz-mwskz = tab_beleg-mwskz.
          COLLECT tmp_tab_mwskz.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE tmp_tab_mwskz LINES tmp_cnt1.
      IF NOT tmp_cnt1 IS INITIAL.
        CLEAR: flg_reject_doc.
      ELSE.                            " check SEL_KTS2.
        LOOP AT tab_beleg.
          IF tab_beleg-ktosl IN sel_kts2.
            tmp_tab_mwskz-mwskz = tab_beleg-mwskz.
            COLLECT tmp_tab_mwskz.
          ENDIF.
        ENDLOOP.
        DESCRIBE TABLE tmp_tab_mwskz LINES tmp_cnt1.
        IF NOT tmp_cnt1 IS INITIAL.
          REFRESH tmp_tab_beleg.
          LOOP AT tmp_tab_mwskz.
            LOOP AT tab_beleg.
              IF tmp_tab_mwskz-mwskz EQ tab_beleg-mwskz.
                tmp_tab_beleg = tab_beleg.
                APPEND tmp_tab_beleg.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
          REFRESH tab_beleg.
          LOOP AT tmp_tab_beleg.
            tab_beleg = tmp_tab_beleg.
            APPEND tab_beleg.
          ENDLOOP.
          CLEAR: flg_reject_doc.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM. " CHECK_KTOSL_SELECTION

*&---------------------------------------------------------------------*
*&      Form  CHECK_MM_BASE_AMOUNTS
*&---------------------------------------------------------------------*
*       Only called for MM documents.
*       Base amount for MM documents sometimes wrong.
*       Check table tab_taxes and compare sums of dmbtr and hwbas:
*       Only if they are equal, take hwbas (due to problems
*       with data from BSET if two lines belong to same base amount)
*----------------------------------------------------------------------*
FORM check_mm_base_amounts.
  DATA: BEGIN OF wa_taxes,
          netamount LIKE bseg-dmbtr,
          hwbas     LIKE bseg-hwbas,
        END OF wa_taxes.
  DATA: lcl_tabix LIKE sy-tabix.
  CLEAR: wa_taxes.
*  sort tab_taxes by mwskz posnr.
  LOOP AT sum_tab_taxes.
    lcl_tabix = sy-tabix.
    AT NEW mwskz.
      READ TABLE sum_tab_taxes INDEX lcl_tabix.
      wa_taxes-netamount = wa_taxes-netamount + sum_tab_taxes-netamount.
      wa_taxes-hwbas     = wa_taxes-hwbas     + sum_tab_taxes-hwbas.
    ENDAT.
  ENDLOOP.
  IF wa_taxes-netamount EQ wa_taxes-hwbas.
    LOOP AT sum_tab_taxes.
      lcl_tabix = sy-tabix.
      AT NEW mwskz.
        READ TABLE sum_tab_taxes INDEX lcl_tabix.
        sum_tab_taxes-netamount = sum_tab_taxes-hwbas.
        MODIFY sum_tab_taxes.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM. " CHECK_MM_BASE_AMOUNTS

*----------------------------------------------------------------------*
*       FORM check_newtaxid
*----------------------------------------------------------------------*
FORM check_newtaxid USING taxid LIKE j_1ataxid-j_1ataxid.
  DATA tid(2) TYPE c.
  tid = taxid(2).
  CASE tid.                            " TaxId for processing key
    WHEN 'EP'.                         " et ertragssteuer perception
      CLEAR: flg_not_used_taxid.
    WHEN 'GP'.                         " GI region perception
      CLEAR: flg_not_used_taxid.
    WHEN 'IT'.                         " internal tax
      CLEAR: flg_not_used_taxid.
    WHEN 'TX'.                         " input/output tax
      CLEAR: flg_not_used_taxid.
    WHEN 'VL'.                         " liberation
      CLEAR: flg_not_used_taxid.
    WHEN 'VN'.                         " VAT not taxable
      CLEAR: flg_not_used_taxid.
    WHEN 'VP'.                         " perception
      CLEAR: flg_not_used_taxid.
    WHEN 'VS'.                         " surcharge
      CLEAR: flg_not_used_taxid.
    WHEN OTHERS.
      flg_not_used_taxid = 'X'.        " this tax id is valid
  ENDCASE.
ENDFORM. " CHECK_NEWTAXID

*&---------------------------------------------------------------------*
*&      Form  CHECK_SD_AMOUNTS
*&---------------------------------------------------------------------*
FORM check_sd_amounts USING f_subrc LIKE sy-subrc.
  IF flg_is_beleg NE 'S'.              " not SD document
    MOVE-CORRESPONDING tab_bset TO dis_taxes.    " store the taxes
    COLLECT dis_taxes.
    f_subrc = 4.
    CHECK 1 = 2.
  ELSE.
* check, that the calculated taxes are not bigger then the posted ones
    CLEAR dis_taxes.
    READ TABLE dis_taxes WITH KEY tab_bset-ktosl.
    CHECK sy-subrc = 0.

    IF dis_taxes-hwste < 0 AND
       dis_taxes-hwste < tab_bset-hwste.
      SUBTRACT tab_bset-hwste FROM dis_taxes-hwste.
      MODIFY dis_taxes INDEX sy-tabix.
    ELSEIF tab_bset-hwste < dis_taxes-hwste.
      SUBTRACT tab_bset-hwste FROM dis_taxes-hwste.
      MODIFY dis_taxes INDEX sy-tabix.
    ELSEIF dis_taxes-hwste NE 0.
      tab_bset-hwste = dis_taxes-hwste.
      CLEAR dis_taxes-hwste.
      MODIFY dis_taxes INDEX sy-tabix.
    ELSEIF tab_bset-hwste NE 0.
* overflow of tax amount
      f_subrc = 4.
      CHECK 1 = 2.
    ENDIF.
  ENDIF.

  f_subrc = 0.

ENDFORM. " CHECK_SD_AMOUNTS

*&---------------------------------------------------------------------*
*&      Form  CHECK_SPECIAL_GL_AMOUNT
*&---------------------------------------------------------------------*
*&      Check sum of line totals against document total for
*&      documents with special G/L items only.
*&      Line totals are correct so far, document total not always
*&---------------------------------------------------------------------*
FORM check_special_gl_amount CHANGING p_flg_gl TYPE c
                                       p_gltotal LIKE tab_beleg-total.
  DATA: comp_gl(8)    TYPE p,
        comp_beleg(8) TYPE p.
  CLEAR: p_gltotal, comp_gl, comp_beleg.
  LOOP AT tab_beleg. " Sum of line totals
    p_gltotal = p_gltotal + tab_beleg-line_total.
  ENDLOOP.
* Adjust signs
  IF p_gltotal GE 0.
    comp_gl = p_gltotal.
  ELSE.
    comp_gl = -1 * p_gltotal.
  ENDIF.
* tab_beleg-total was filled from amnt-total for all tab_beleg lines
  IF tab_beleg-total GE 0.
    comp_beleg = tab_beleg-total.
  ELSE.
    comp_beleg = -1 * tab_beleg-total.
  ENDIF.

* Error only occurs for some documents under circumstances that can not
* be identified clearly -> adjust document total manually, but only
* for erroneous containing special G/L items
  IF comp_gl GT comp_beleg.
    p_flg_gl = 'X'.
* p_gltotal needs same sign as tab_beleg-total:
* In version 3 and 4, documents might have different sign for
* document total and each line total, respectively.
* p_gltotal was built from line totals -> check for sign needed!

* Note 715618
    IF ( p_gltotal LT 0 AND tab_beleg-total GE 0 ) OR    " Note 428864
      ( p_gltotal GT 0 AND tab_beleg-total LE 0 ).       " Note 428864
      p_gltotal = -1 * p_gltotal.
    ENDIF.
  ENDIF.
ENDFORM. " CHECK_SPECIAL_GL_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  CHECK_SD_FOREIGN_CURRENCY
*&---------------------------------------------------------------------*
*       For SD documents only: Foreign currencies may lead to
*       rounding errors. Apply solution from FI to correct this.
*----------------------------------------------------------------------*
FORM check_sd_foreign_currency.
  DATA: max_amount(8)  TYPE p DECIMALS 2, " Biggest amount of all line entries
        comp_total(8)  TYPE p DECIMALS 2, " Sum of all line totals for document
* ---> S4 Migration - 08/07/2023 - FC
        "beleg_total(8) TYPE p DECIMALS 2, " Document total: Compare without sign
        beleg_total TYPE dmbtr, " Document total: Compare without sign
* <--- S4 Migration - 08/07/2023 - FC
        diff_curr(8)   TYPE p DECIMALS 2, " Difference: beleg_total - comp_total
        lcl_index      LIKE sy-index,
        lcl_tabix      LIKE sy-tabix.
  CLEAR: comp_total, lcl_index, lcl_tabix. " Note 632617
  LOOP AT tab_beleg.                   " Determine sum of line totals
    comp_total = comp_total + tab_beleg-line_total.
  ENDLOOP.

* tab_beleg-total was filled from amnt-total for all tab_beleg lines
  beleg_total = tab_beleg-total. " Doc. total: Allow adjustment of sign

  diff_curr = beleg_total + comp_total." Difference after rounding

  IF diff_curr <> 0.            " Rounding errors on currency conversion
* NOW: Determine highest amount of line.
    CLEAR: max_amount.
    LOOP AT tab_beleg.                 " Examine all amounts of lines

* Note Changes 766634 start
* tab_beleg changed and it now contains 12 amounts for the 12 columns to
* be filled.
      DO 12 TIMES VARYING        " Use comp_total for other purpose now
* Note Changes 766634 ends
          comp_total FROM tab_beleg-taxed NEXT tab_beleg-not_taxed.
        IF comp_total < 0.             " Evaluate absolute amounts
          comp_total = comp_total * -1.
        ENDIF.
        IF comp_total > max_amount.
          max_amount = comp_total.
          lcl_index  = sy-index.       " Line of tab_beleg
          lcl_tabix  = sy-tabix.       " Amount field of this line
        ENDIF.
      ENDDO.
    ENDLOOP.

    CHECK: NOT lcl_index IS INITIAL,   " Note 632617
           NOT lcl_tabix IS INITIAL.   " Note 632617
* Modify correct line of tab_beleg
    READ TABLE tab_beleg INDEX lcl_tabix.
* Modify correct field of of this line from tab_beleg
    CASE lcl_index.
      WHEN 1.
        tab_beleg-taxed        = tab_beleg-taxed        - diff_curr.
      WHEN 2.
        tab_beleg-not_taxed    = tab_beleg-not_taxed    - diff_curr.

* Note Changes 766634 start
      WHEN 3.
        tab_beleg-vat_intern   = tab_beleg-vat_intern   - diff_curr.
      WHEN 4.
        tab_beleg-vat          = tab_beleg-vat          - diff_curr.
      WHEN 5.
        tab_beleg-rnr_vat      = tab_beleg-rnr_vat      - diff_curr.
      WHEN 6.
        tab_beleg-vat_percep   = tab_beleg-vat_percep   - diff_curr.
      WHEN 7.
        tab_beleg-other_percep = tab_beleg-other_percep - diff_curr.
      WHEN 8.
        tab_beleg-munic_per    = tab_beleg-munic_per    - diff_curr.
      WHEN 9.
        tab_beleg-earn_per     = tab_beleg-earn_per     - diff_curr.
      WHEN 10.
        tab_beleg-exemption    = tab_beleg-exemption    - diff_curr.
      WHEN 11.
        tab_beleg-exports      = tab_beleg-exports      - diff_curr.
      WHEN 12.
        tab_beleg-percepnoc    = tab_beleg-percepnoc    - diff_curr.
    ENDCASE.
* Note Changes 766634 ends

    tab_beleg-line_total       = tab_beleg-line_total   - diff_curr.
    MODIFY tab_beleg INDEX lcl_tabix.
  ENDIF.
ENDFORM. " CHECK_SD_FOREIGN_CURRENCY

*&---------------------------------------------------------------------*
*&      Form  CHECK_TAX_CODE
*&---------------------------------------------------------------------*
FORM check_tax_code USING mwskz LIKE bseg-mwskz.
  rcode = 4.
  CHECK mwskz NE space.
  IF mwskz = '**'.
    CHECK 1 = 2.
  ENDIF.
  PERFORM read_t007a USING tab_001-kalsm mwskz.  " Tax-Code
  IF tab_007a-zmwsk IS INITIAL.
    CHECK mwskz IN s_taxcd.
  ELSE.
    CHECK tab_007a-zmwsk IN s_taxcd.
    mwskz = tab_007a-zmwsk.
  ENDIF.
  rcode = 0.
ENDFORM. " CHECK_TAX_CODE

*----------------------------------------------------------------------*
* Subroutines for history table
*----------------------------------------------------------------------*
* READ_TAB_HISTORY          Read history for daily VAT Reporting
* SET_HISTORY_LINENO
* ADD_ENTRY_TO_TAB_HISTORY  Append an entry to history table
* UPDATE_HISTORY_TABLE      Update history table for daily VAT Reporting
* DELETE_HISTORY_TABLE      Delete entry in history table J_1A101
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_TAB_HISTORY
*&---------------------------------------------------------------------*
*       Read history for daily VAT Reporting
*----------------------------------------------------------------------*
FORM read_tab_history USING bukrs j_1aproc.

  CHECK: par_magn IS INITIAL.

  LOOP AT br_budat.                                         "1069346
    IF from_date IS INITIAL OR
       br_budat-low < from_date.                            "1069346
      from_date = br_budat-low.                             "1069346
    ENDIF.
    IF to_date IS INITIAL OR
       br_budat-low > to_date.                              "1069346
      to_date = br_budat-low.                               "1069346
    ENDIF.
    IF to_date IS INITIAL OR
       br_budat-high > to_date.                             "1069346
      to_date = br_budat-high.                              "1069346
    ENDIF.

  ENDLOOP.

  CLEAR wa_history.
  REFRESH tab_history.

*  read history table (j_1a101) into tab_history
  SELECT * FROM j_1a101 WHERE bukrs     = bukrs
                        AND   repid     = history_repid
                        AND   j_1adrver = s_drver
                        AND   j_1aproc  = j_1aproc
                        AND   j_1adrdt  IN br_budat         "1069346
                        ORDER BY j_1adrdt DESCENDING.
    MOVE-CORRESPONDING j_1a101 TO tab_history.
    APPEND tab_history.
  ENDSELECT.

* get the last entry for setting wa_history.
  SELECT * FROM j_1a101 WHERE bukrs     = tab_001-bukrs
                        AND   repid     = history_repid
                        AND   j_1adrver = s_drver
                        AND   j_1aproc  = tab_j_1adrver-j_1aproc
                        AND   j_1adrdt  < from_date
                        ORDER BY j_1adrdt DESCENDING.
    MOVE-CORRESPONDING j_1a101 TO wa_history.
    EXIT.
  ENDSELECT.

  IF NOT wa_history-j_1adrdt IS INITIAL.
    ADD 1 TO wa_history-j_1alineno.
    ADD 1 TO wa_history-j_1apageno.
  ENDIF.
  wa_history_over = wa_history.

ENDFORM. " READ_TAB_HISTORY

*&---------------------------------------------------------------------*
*&      Form  SET_HISTORY_LINENO
*&---------------------------------------------------------------------*
*       S_INIT defines how to setup the lineno for history
*----------------------------------------------------------------------*
FORM set_history_lineno.
  DATA: fiscal_period_new TYPE type_fiscal_period,
        fiscal_period_old TYPE type_fiscal_period,
        current_date      LIKE sy-datum.                 " Note 451491

  CASE s_init.
    WHEN 1 OR  2.
*          1    " for busines year  : start new lineno and pageno from 1
*          2    " for busines period: start new lineno and pageno from 1
      CALL FUNCTION 'FI_PERIOD_DETERMINE'
        EXPORTING
          i_budat = ep-budat     " get from budat
          i_bukrs = tab_001-bukrs
          i_periv = tab_001-periv
        IMPORTING
          e_gjahr = fiscal_period_new-gjahr
          e_monat = fiscal_period_new-monat.
      IF wa_history-j_1adrdt IS INITIAL .
        current_date = sy-datum.                         " Note 451491
      ELSE.                                              " Note 451491
        current_date = wa_history-j_1adrdt.              " Note 451491
      ENDIF.
      CALL FUNCTION 'FI_PERIOD_DETERMINE'
        EXPORTING
          i_budat = current_date                   " Note 451491
          i_bukrs = tab_001-bukrs
          i_periv = tab_001-periv
        IMPORTING
          e_gjahr = fiscal_period_old-gjahr
          e_monat = fiscal_period_old-monat.
      IF  s_init = 1  AND
         ( fiscal_period_new-gjahr NE fiscal_period_old-gjahr ).
        CLEAR wa_history.
      ENDIF.
      IF  s_init = 2  AND
         ( fiscal_period_new-monat NE fiscal_period_old-monat ).
        CLEAR wa_history.
      ENDIF.
    WHEN 3.   " for calendar year : start new lineno and pageno from 1
      IF ep-budat(4) NE wa_history-j_1adrdt(4).
        CLEAR wa_history.
        current_date = sy-datum.                         " Note 451491
      ENDIF.
    WHEN 4.   " for calendar month :start new lineno and pageno from 1
      IF ep-budat(6) NE wa_history-j_1adrdt(6).
        CLEAR wa_history.
        current_date = sy-datum.                         " Note 451491
      ENDIF.
  ENDCASE.
  IF wa_history-j_1adrdt IS INITIAL.
    wa_history-mandt      = sy-mandt.
    wa_history-bukrs      = tab_001-bukrs.
    wa_history-repid      = history_repid.
    wa_history-j_1adrver  = s_drver.
    wa_history-j_1aproc   = tab_j_1adrver-j_1aproc.
    wa_history-j_1adrdt   = current_date.                " Note 451491
    wa_history-j_1alineno = 1.
    wa_history-j_1apageno = 1.
  ENDIF.

ENDFORM. " SET_HISTORY_LINENO

*&---------------------------------------------------------------------*
*&      Form  ADD_ENTRY_TO_TAB_HISTORY
*&---------------------------------------------------------------------*
*&      Append an entry to history table
*&---------------------------------------------------------------------*
FORM add_entry_to_tab_history.
  CHECK: par_magn IS INITIAL.
  CLEAR tab_history.
  MOVE-CORRESPONDING wa_history TO tab_history.
  SUBTRACT 1 FROM tab_history-j_1alineno.
  SUBTRACT 1 FROM tab_history-j_1apageno.
  APPEND tab_history.
ENDFORM. " ADD_ENTRY_TO_TAB_HISTORY

*&---------------------------------------------------------------------*
*&      Form  UPDATE_HISTORY_TABLE
*&---------------------------------------------------------------------*
*& Update Historiy Table for daly VAT-Reporting
*&---------------------------------------------------------------------*
FORM update_history_table.

  CHECK: par_magn IS INITIAL.
* get the entry which is bigger then the to_date
  SELECT * FROM j_1a101 WHERE bukrs     = tab_001-bukrs
                        AND   repid     = history_repid
                        AND   j_1adrver = s_drver
                        AND   j_1aproc  = tab_j_1adrver-j_1aproc
                        AND   j_1adrdt  > to_date
                        ORDER BY j_1adrdt DESCENDING.
*    move-corresponding j_1a101 to wa_history.
    EXIT.
  ENDSELECT.
  IF sy-subrc EQ 0.
* Chronological table no longer sequential
    CLEAR : tab_log_entry2.
    tab_log_entry2-budat         =  j_1a101-j_1adrdt.
    tab_log_entry2-description   =  TEXT-v81.
    COLLECT tab_log_entry2.
  ENDIF.

  flg_print_tab_history = 2.
  IF NOT par_updh IS INITIAL AND       " update only if this flag set
     NOT par_comp IS INITIAL AND       " update only if compress
     NOT par_vers IS INITIAL.          " update only with version
*    describe table tab_log_entry1 lines tmp_cnt1.
*    if tmp_cnt1 eq 0.                " update wenn tab_log_entry1 leer
    MODIFY j_1a101 FROM TABLE tab_history.
    IF sy-subrc NE 0.
      REFRESH: tab_history.
      MESSAGE s844.
    ELSE.
      flg_print_tab_history = 1.
    ENDIF.
  ENDIF.
ENDFORM. " UPDATE_HISTORY_TABLE

*&---------------------------------------------------------------------*
*&      Form  DELETE_HISTORY_TABLE
*&---------------------------------------------------------------------*
*&      Delete History Table for daily VAT Reporting ( J_1A101 )
*&---------------------------------------------------------------------*
FORM delete_history_table.

  CHECK: par_magn IS INITIAL.
  DESCRIBE TABLE tab_history LINES tmp_cnt1.
  IF tmp_cnt1 GE  1 .                  " there are entries.
* delete table entry
    SUBMIT j_1af103 WITH s_bukrs =  br_bukrs-low
                    WITH s_repid =  history_repid
                    WITH s_drver =  s_drver
                    WITH s_date  IN br_budat                "1069346
                    AND  RETURN.
* print table entry
    list_number = 4.                   " print tab_history
    flg_print_tab_history = 0.
    PERFORM print_tab_history .        " deleted history
  ELSE.
* No entry found in history table
    MESSAGE s802.
  ENDIF.
  STOP.
ENDFORM. " DELETE_HISTORY_TABLE

*----------------------------------------------------------------------*
* Subroutines for List Output
*----------------------------------------------------------------------*
* 1st LIST
* PRINT_TAB_EP              Output of the table TAB_EP
* COMPRES_TAB_EP            Compress tax code in tab_ep.
* COLLECT_SUM_TABLES        Build the totals for current line.
* PRINT_TAB_REGIO
* PRINT_CUSTOMS_DATA
* PRINT_P_RECORD
* 2nd LIST
* PRINT_TAB_SUM_MWSKZ       VAT-Totals which are grouped Taxcode
* PRINT_TAB_SUM_MWSKZ_NEW
* PRINT_TAB_SUM_RATE        VAT-Totals per VAT rate
* PRINT_TAB_SUM_OFTPT       VAT Totals per official doc. type
* PRINT_TAB_SUM_ACCOUNT     VAT-Totals per VAT account
* PRINT_TAB_SUM_OTHERS      VAT-Totals which are grouped as others
* PRINT_TAB_SUM_REGIO
* PRINT_TAB_BRANCH          For magnetic output: Will be printed first
* 3rd LIST
* PRINT_TAB_LOG_ENTRY10     Table entries not found.
* PRINT_TAB_LOG_ENTRY20     Other log entries
* 4th LIST
* PRINT_TAB_TAB_HISTORY     History table
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_EP
*&---------------------------------------------------------------------*
* print 1.list: documen items which are grouped by tax code
* TABLE: TAB_EP                                                --------*
*----------------------------------------------------------------------*
FORM print_tab_ep.
  CLEAR t_alv.
  DATA: flg_firstline        TYPE c,
        lineno(6)            TYPE n,
        flg_print_customdata TYPE c,
        reserve_cnt1         TYPE p,
        value                LIKE tab_ep-total,
        lcl_tabix            LIKE sy-tabix,
        v_lines              TYPE i.
  CLEAR: wa_history, lcl_tabix.

  LOOP AT tab_ep.
    lcl_tabix = sy-tabix.
    READ TABLE t_periibb
    WITH KEY hkont = tab_ep-hkont
             libro = 'C'.
    IF sy-subrc EQ 0.
      IF tab_ep-other_percep IS INITIAL.
*---> 09/06/2023 - Migração S4 - JS
*     MOVE tab_ep-hwste TO tab_ep-other_percep.
        tab_ep-other_percep = CONV #( tab_ep-hwste ).
*<--- 09/06/2023 - Migração S4 - JS

        MODIFY tab_ep INDEX lcl_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.



* print reverse document
  READ TABLE tab_ep INDEX 1.
* Note 1069879 Start
  IF NOT tab_ep-stblg IS INITIAL.
    SELECT SINGLE monat INTO rev_monat
    FROM bkpf
    WHERE bukrs = tab_ep-bukrs
    AND gjahr = tab_ep-gjahr
    AND belnr = tab_ep-stblg.
  ENDIF.
* Note 1069879 End
  IF NOT tab_ep-stblg IS INITIAL AND
     NOT par_canc IS INITIAL AND               " Note 427832
     tab_ep-monat EQ rev_monat.                             "1069879
    PERFORM prepare_rahmen.
    RESERVE 2 LINES.
    CLEAR: txt_zeile7.
    lineno = wa_history-j_1alineno.
    hlp_epos1s-lineno = lineno.
    hlp_epos1s-text1  = TEXT-p03.
    hlp_epos1s-belnr  = tab_ep-belnr.
    MOVE: TEXT-p02 TO hlp_epos1s-text2 .
    MOVE: tab_ep-stblg TO hlp_epos1s-stblg.
    MOVE: TEXT-p15 TO hlp_epos1s-text4 .
    MOVE: ':' TO hlp_epos1s-v9.
    PERFORM read_official_doc_type USING land1 tab_ep-blart
                                          tab_ep-prtchr.
    MOVE: tab_j_1aotdet-text5 TO hlp_epos1s-oftp_text.
    MOVE: tab_j_1aotdet-text30 TO hlp_epos1s-ltext.

    IF tab_ep-xblnr(4)   NE space AND
    tab_ep-xblnr+5(8) NE space.
      WRITE: TEXT-p04 TO hlp_epos1s-text3 RIGHT-JUSTIFIED.
      WRITE: tab_ep-xblnr(4) TO hlp_epos1s-xblnr NO-GAP,
            '-' TO hlp_epos1s-xblnr+4 NO-GAP,
            tab_ep-xblnr+5(8) TO hlp_epos1s-xblnr+5 NO-GAP.
    ELSE.
      IF tab_ep-xblnr+5(8) NE space.
        MOVE tab_ep-xblnr+5(8) TO hlp_epos1s-xblnr.
      ENDIF.
    ENDIF.
    txt_zeile7 = hlp_epos1s.
    listsize = listsize - 1 .
    txt_zeile7+listsize = sy-vline.
    listsize = listsize + 1 .
    FORMAT: COLOR OFF,  INVERSE OFF.
*    WRITE / txt_zeile7.
    DO .
      REPLACE ALL OCCURRENCES OF '|' IN txt_zeile8       WITH space.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE ALL OCCURRENCES OF '|' IN txt_zeile7       WITH space.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDDO.
*BEGIN - PG - 19.03.2010
    IF txt_zeile7+8(8) EQ 'Nº doc.S'.

      DATA: BEGIN OF t_mwskz OCCURS 0,
              mwskz TYPE bseg-mwskz,
            END OF t_mwskz.

      DATA: BEGIN OF t_gravado OCCURS 0,
              dmbtr TYPE bseg-dmbtr,
              shkzg TYPE bseg-shkzg,
            END OF t_gravado.


      DATA: t_bseg_aux LIKE TABLE OF bseg WITH HEADER LINE,
            t_bkpf_aux LIKE TABLE OF bkpf WITH HEADER LINE.

      DATA: v_xblnr_aux LIKE bkpf-xblnr,
            v_saldo     TYPE bseg-dmbtr,
            v_lifnr     TYPE bseg-lifnr,
            r_mwskz     TYPE RANGE OF bseg-mwskz,
            x_mwskz     LIKE LINE OF r_mwskz,
            v_dmbtr     TYPE bseg-dmbtr,
            v_shkzg     TYPE bseg-shkzg.

      RANGES: r_hkont FOR bseg-hkont.

* Obtengo el numero de referencia del documento contable
      SELECT SINGLE xblnr
        FROM bkpf
        INTO v_xblnr_aux
       WHERE bukrs IN br_bukrs
         AND belnr EQ txt_zeile7+22(10)
         AND gjahr IN br_gjahr.

* Obtengo el acreedor correspondiente al documento
* ---> S4 Migration - 16/06/2023 - MA
*Não tem todos os campos chave
      SELECT SINGLE lifnr
        FROM bseg
        INTO v_lifnr
       WHERE bukrs IN br_bukrs
         AND belnr EQ txt_zeile7+22(10)
         AND gjahr IN br_gjahr
         AND koart = 'K'.              "#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 16/06/2023 - MA
* Obtengo todos los documentos que tienen como referencia el mismo numero legal
      SELECT *
        FROM bkpf
        INTO TABLE t_bkpf_aux
       WHERE budat IN br_budat
         AND gjahr IN br_gjahr
         AND bukrs IN br_bukrs
         AND xblnr = v_xblnr_aux.

* Obtengo las posiciones de acreedor
* ---> S4 Migration - 16/06/2023 - MA
      SELECT * FROM bseg
        INTO TABLE t_bseg_aux
         FOR ALL ENTRIES IN t_bkpf_aux
       WHERE bukrs IN br_bukrs
         AND gjahr IN br_gjahr
         AND belnr EQ t_bkpf_aux-belnr
         AND koart = 'K'.              "#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 16/06/2023 - MA
      CLEAR v_saldo.
      LOOP AT t_bseg_aux.
        IF t_bseg_aux-shkzg = 'S'.
          v_saldo = v_saldo + t_bseg_aux-dmbtr.
        ELSE.
          v_saldo = v_saldo - t_bseg_aux-dmbtr.
        ENDIF.
      ENDLOOP.

      IF v_saldo NE 0.
        DO .
          REPLACE ALL OCCURRENCES OF '|' IN txt_zeile8       WITH space.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
        ENDDO.
        DO .
          REPLACE ALL OCCURRENCES OF '|' IN txt_zeile7       WITH space.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
        ENDDO.
* Obtengo las posiciones de los impuestos
* ---> S4 Migration - 16/06/2023 - MA
        SELECT * FROM bseg
          INTO TABLE t_bseg_aux
*           FOR ALL ENTRIES IN t_bkpf_aux
         WHERE bukrs IN br_bukrs
           AND gjahr IN br_gjahr
*           AND belnr EQ t_bkpf_aux-belnr
           AND belnr EQ txt_zeile7+22(10)
           AND mwskz NE space
           AND buzid = 'T'.            "#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 16/06/2023 - MA

        t_alv-lineno = txt_zeile7+1(6).
*        t_alv-budat = txt_zeile7+8(8).
        t_alv-brnch = txt_zeile7+17(4).
        t_alv-belnr = txt_zeile7+22(10).
        SHIFT t_alv-belnr RIGHT DELETING TRAILING space.
        OVERLAY t_alv-belnr WITH '0000000000'.
        t_alv-koart = txt_zeile7+33(1).
*        t_alv-hkont = txt_zeile7+35(10).
        t_alv-name1 = txt_zeile7+46(35).
        t_alv-stcdt = txt_zeile7+82(2).
        t_alv-stcd1 = txt_zeile7+85(16).
        t_alv-bldat = txt_zeile7+102(8).
        t_alv-xblnr = txt_zeile7+111(14).
        IF t_alv-xblnr IS NOT INITIAL.
          t_alv-xblnr+4(1) = '-'.
        ENDIF.
        t_alv-oftp_text = txt_zeile7+126(5).
        IF par_magn = space.
          t_alv-augdt = txt_zeile7+132(8).
          t_alv-augbl = txt_zeile7+141(10).
          t_alv-total = txt_zeile7+152(57).
        ELSE.
          t_alv-codmon = txt_zeile7+141(10).
          t_alv-tpcbio = txt_zeile7+152(15).
          t_alv-vaiva  = txt_zeile7+168(18).
          t_alv-total  = txt_zeile7+187(22).
        ENDIF.
        t_alv-flag = space.


        SELECT *
          FROM bkpf
          INTO TABLE t_bkpf_aux
         WHERE budat IN br_budat
           AND gjahr IN br_gjahr
           AND bukrs IN br_bukrs
           AND belnr = txt_zeile7+22(10).



        READ TABLE t_bkpf_aux INDEX 1.
* ---> S4 Migration - 15/06/2023 - MA
        SELECT SINGLE * FROM bseg WHERE bukrs = t_bkpf_aux-bukrs AND
                                        gjahr = t_bkpf_aux-gjahr AND
                                        belnr = t_bkpf_aux-belnr AND
                                        koart = 'K'. "#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 15/06/2023 - MA
        WRITE: t_bkpf_aux-bldat    TO t_alv-bldat,
               t_bkpf_aux-bldat    TO t_alv-budat,
               t_bkpf_aux-bldat(4) TO t_alv-gjahr.

        t_alv-xblnr = t_bkpf_aux-xblnr.
        t_alv-xblnr+4(1) = '-'.
* Guardo el tipo de cambio
*        t_alv-tpcbio = t_bkpf_aux-kursf.
        WRITE t_bkpf_aux-kursf TO t_alv-tpcbio.
* Guardo el total del documento
        IF v_saldo < 0.
          v_saldo = v_saldo * -1.
        ENDIF.
        MOVE v_saldo TO t_alv-total.

* Guardo en la cuenta el numero de cliente
        t_alv-hkont = v_lifnr.

* Voy a la LFA1 y saco el nombre del cliente
        SELECT SINGLE name1 stcd1 FROM lfa1 INTO (t_alv-name1, t_alv-stcd1 )
          WHERE lifnr = v_lifnr.

        t_alv-stcdt = '80'.

        DATA etl3444c8r2995 TYPE TABLE OF bseg.
        DATA lt_fields_l3444c8r3197 TYPE fagl_t_field.
        lt_fields_l3444c8r3197 = VALUE #( ( line = 'MWSKZ' )
         ).
        DATA rldnr_l3444c8r5854 TYPE rldnr.
        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = rldnr_l3444c8r5854
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.
        IF sy-subrc = 0.
          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr         = rldnr_l3444c8r5854
              i_bukrs         = t_bkpf_aux-bukrs
              i_belnr         = t_bkpf_aux-belnr
              i_gjahr         = t_bkpf_aux-gjahr
              it_fieldlist    = lt_fields_l3444c8r3197
              it_where_clause = VALUE tt_rsdswhere( ( |MWSKZ NE ' '| ) ( | AND | ) ( |BUZID = 'T'| ) )
            IMPORTING
              et_bseg         = etl3444c8r2995
            EXCEPTIONS
              not_found       = 1.
        ENDIF.
        IF sy-subrc = 0 AND lines( etl3444c8r2995 ) > 0.
          MOVE-CORRESPONDING etl3444c8r2995 TO t_mwskz[].
          sy-dbcnt = lines( etl3444c8r2995 ).
        ELSE.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.


        SORT t_mwskz.

        DELETE ADJACENT DUPLICATES FROM t_mwskz.

        LOOP AT t_mwskz WHERE mwskz NE 'VQ' AND mwskz NE 'VZ' AND mwskz NE 'Z1'.

          CLEAR t_alv-iexemption.
          CLEAR t_alv-ivat.
          CLEAR t_alv-iibb.
          CLEAR t_alv-ivat_percep.
          CLEAR t_alv-iother_percep.

          REFRESH r_mwskz.
          IF sy-tabix = 1.
            x_mwskz-sign = 'I'.
            x_mwskz-option = 'EQ'.
            x_mwskz-low = 'Z1'.
            APPEND x_mwskz TO r_mwskz.
            x_mwskz-low = 'VQ'.
            APPEND x_mwskz TO r_mwskz.
            x_mwskz-low = 'VZ'.
            APPEND x_mwskz TO r_mwskz.
          ELSE.    "Como no es la primer linea del numero de documento, solo busco el indicador de la linea
            x_mwskz-sign = 'I'.
            x_mwskz-option = 'EQ'.
            x_mwskz-low = 'ZZ'.
            APPEND x_mwskz TO r_mwskz.
          ENDIF.
          LOOP AT t_bseg_aux WHERE mwskz = t_mwskz-mwskz OR mwskz IN r_mwskz.
            CASE t_bseg_aux-ktosl.
              WHEN 'ZIV'.
                IF t_bseg_aux-mwskz = 'V0'.
                  IF t_bseg_aux-shkzg = 'S'.
                    t_alv-iexemption = t_alv-iexemption + t_bseg_aux-dmbtr.
                  ELSE.
                    t_alv-iexemption = t_alv-iexemption + t_bseg_aux-dmbtr * -1.
                  ENDIF.
                ELSE.
                  IF t_bseg_aux-shkzg = 'S'.
                    t_alv-ivat = t_alv-ivat + t_bseg_aux-dmbtr.
                  ELSE.
                    t_alv-ivat = t_alv-ivat + t_bseg_aux-dmbtr * -1.
                  ENDIF.
                ENDIF.
              WHEN 'Z01'.
                IF t_bseg_aux-shkzg = 'S'.
                  t_alv-ivat = t_alv-ivat + t_bseg_aux-dmbtr.
                ELSE.
                  t_alv-ivat = t_alv-ivat + t_bseg_aux-dmbtr * -1.
                ENDIF.
              WHEN 'J1G'.
                IF t_bseg_aux-mwskz NE 'VZ'.
                  IF t_bseg_aux-shkzg = 'S'.
                    t_alv-iibb = t_alv-iibb + t_bseg_aux-dmbtr.
                  ELSE.
                    t_alv-iibb = t_alv-iibb + t_bseg_aux-dmbtr * -1.
                  ENDIF.
                ELSE.
                  IF t_bseg_aux-shkzg = 'S'.
                    t_alv-ivat_percep = t_alv-ivat_percep + t_bseg_aux-dmbtr.
                  ELSE.
                    t_alv-ivat_percep = t_alv-ivat_percep + t_bseg_aux-dmbtr * -1.
                  ENDIF.
                ENDIF.
              WHEN 'ZPI'.
                IF t_bseg_aux-shkzg = 'S'.
                  t_alv-iibb = t_alv-iibb + t_bseg_aux-dmbtr.
                ELSE.
                  t_alv-iibb = t_alv-iibb + t_bseg_aux-dmbtr * -1.
                ENDIF.
              WHEN 'J1P'.
                IF t_bseg_aux-shkzg = 'S'.
                  t_alv-iother_percep = t_alv-iother_percep + t_bseg_aux-dmbtr.
                ELSE.
                  t_alv-iother_percep = t_alv-iother_percep + t_bseg_aux-dmbtr * -1.
                ENDIF.
            ENDCASE.
          ENDLOOP.

*          t_alv-mwskz+4(2) = t_mwskz-mwskz.

* Obtengo la base (Gravado)
          DATA etl3533c10r3135 TYPE TABLE OF bseg.
          DATA lt_fields_l3533c10r7531 TYPE fagl_t_field.
          lt_fields_l3533c10r7531 = VALUE #( ( line = 'DMBTR' )
           ( line = 'SHKZG' )
           ).
          DATA rldnr_l3533c10r7836 TYPE rldnr.
          CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
            IMPORTING
              e_rldnr       = rldnr_l3533c10r7836
            EXCEPTIONS
              not_found     = 1
              more_than_one = 2.
          IF sy-subrc = 0.
            CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
              EXPORTING
                i_rldnr         = rldnr_l3533c10r7836
                i_bukrs         = t_bkpf_aux-bukrs
                i_belnr         = t_bkpf_aux-belnr
                i_gjahr         = t_bkpf_aux-gjahr
                it_fieldlist    = lt_fields_l3533c10r7531
                it_where_clause = VALUE tt_rsdswhere( ( |MWSKZ NE ' '| ) ( | AND | ) ( |BUZID = 'W'| ) )
              IMPORTING
                et_bseg         = etl3533c10r3135
              EXCEPTIONS
                not_found       = 1.
          ENDIF.
          IF sy-subrc = 0 AND lines( etl3533c10r3135 ) > 0.
            MOVE-CORRESPONDING etl3533c10r3135 TO t_gravado[].
            sy-dbcnt = lines( etl3533c10r3135 ).
          ELSE.
            sy-subrc = 4.
            sy-dbcnt = 0.
          ENDIF.


          IF sy-subrc = 0.
            CLEAR t_alv-itaxed.
            LOOP AT t_gravado.
              IF t_gravado-shkzg = 'S'.
                t_alv-itaxed = t_alv-itaxed + t_gravado-dmbtr.
              ELSE.
                t_alv-itaxed = t_alv-itaxed - t_gravado-dmbtr.
              ENDIF.
            ENDLOOP.
          ELSE.
* ---> S4 Migration - 15/06/2023 - MA
*            SELECT SINGLE dmbtr shkzg FROM bseg INTO (v_dmbtr, v_shkzg)
*              WHERE bukrs = t_bkpf_aux-bukrs AND
*                    gjahr = t_bkpf_aux-gjahr AND
*                    belnr = t_bkpf_aux-belnr AND
*                    ( mwskz = 'VQ' OR mwskz = 'VZ' OR mwskz = 'Z1' ) AND
*                    buzid = 'S'.

            DATA:   lt_bseg TYPE fagl_t_bseg.

            CALL FUNCTION 'FAGL_GET_BSEG'
              EXPORTING
                i_bukrs   = t_bkpf_aux-bukrs
                i_belnr   = t_bkpf_aux-belnr
                i_gjahr   = t_bkpf_aux-gjahr
              IMPORTING
                et_bseg   = lt_bseg
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            DELETE lt_bseg WHERE ( mwskz = 'VQ' OR mwskz = 'VZ' OR mwskz = 'Z1' ) AND
                  buzid = 'S'.
            .
            READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
            IF sy-subrc = 0.
              MOVE ls_bseg-shkzg TO v_shkzg.
              MOVE ls_bseg-dmbtr TO v_dmbtr.
            ENDIF.
*<--- S4 Migration - 15/06/2023 - MA

            IF t_gravado-shkzg = 'S'.
*---> 09/06/2023 - Migração S4 - JS
*     t_alv-itaxed = v_dmbtr.
              t_alv-itaxed = CONV #( v_dmbtr ).
*<--- 09/06/2023 - Migração S4 - JS
            ELSE.
              t_alv-itaxed = v_dmbtr * -1.
            ENDIF.
          ENDIF.
          CLEAR: t_alv-total,
                 line_total.
          APPEND t_alv.
        ENDLOOP.

      ENDIF. "v_saldo NE 0.
    ELSE.

      DO .
        REPLACE ALL OCCURRENCES OF '|' IN txt_zeile8       WITH space.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.
      DO .
        REPLACE ALL OCCURRENCES OF '|' IN txt_zeile7       WITH space.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.
*[BEGIN] -- PM -- 20/08/09
* move-CORRESPONDING to t_alv.
      t_alv-lineno = txt_zeile7+1(6).
      t_alv-budat = txt_zeile7+8(8).
      "t_alv-gjahr = t_alv-budat(4).
      t_alv-brnch = txt_zeile7+17(4).
      t_alv-belnr = txt_zeile7+22(10).
      SHIFT t_alv-belnr RIGHT DELETING TRAILING space.
      OVERLAY t_alv-belnr WITH '0000000000'.
      t_alv-koart = txt_zeile7+33(1).
      t_alv-hkont = txt_zeile7+35(10).
      t_alv-name1 = txt_zeile7+46(35).
      t_alv-stcdt = txt_zeile7+82(2).
      t_alv-stcd1 = txt_zeile7+85(16).
      t_alv-bldat = txt_zeile7+102(8).
      t_alv-xblnr = txt_zeile7+111(14).
      IF t_alv-xblnr IS NOT INITIAL.
        t_alv-xblnr+4(1) = '-'.
      ENDIF.
      t_alv-oftp_text = txt_zeile7+126(5).
      IF par_magn = space.
        t_alv-augdt = txt_zeile7+132(8).
        t_alv-augbl = txt_zeile7+141(10).
        t_alv-total = txt_zeile7+152(57).
      ELSE.
        t_alv-codmon = txt_zeile7+141(10).
        t_alv-tpcbio = txt_zeile7+152(15).
        t_alv-vaiva  = txt_zeile7+168(18).
        t_alv-total  = txt_zeile7+187(22).
      ENDIF.
      t_alv-flag = 'X'.
      CLEAR: t_alv-total,
            line_total.
      APPEND t_alv.

    ENDIF.

    HIDE: tab_ep-bukrs, tab_ep-gjahr, tab_ep-belnr,
             tab_ep-flg_sd_beleg, tab_ep-sd_vbeln,
***** BEGIN OF MODIFICATION *******
             tab_ep-flg_is_beleg.
***** END OF MODIFICATION *******
    IF NOT par_rahm IS INITIAL.
*      ULINE.
    ENDIF.
    EXIT.
  ENDIF.

* compres the tax code.
  SORT tab_ep BY mwskz posnr rate DESCENDING linetype DESCENDING.
* Sorting enhanced in Note 658485

* Table for Perceptions File - filled in FORM COMPRES_TAB_EP
  CLEAR:   p_record2,
           tab_p_record2.
  REFRESH: tab_p_record2.

* Collect Sum per Account                         "Note 932719
  LOOP AT tab_ep.                                 "Note 932719
    PERFORM collect_sum_account.                  "Note 932719
  ENDLOOP.                                        "Note 932719

* Note 1016337 Start
* Summing up the other totals before compressing the data
  LOOP AT tab_ep.
    PERFORM collect_sum_tables.
  ENDLOOP.
* End of note 1016337

  PERFORM compres_tab_ep.
  CLEAR: flg_print_customdata.

* Note 1033709 moved the below code before compressing
* the data to correct the page totals. But it resulted
* in wrong number of documents displayed in a page. Hence
* reverted this change and calculated the page totals after this
* code. Corrected by note 1048207
  DESCRIBE TABLE tab_ep LINES reserve_cnt1.
  reserve_cnt1 = reserve_cnt1 + 3.
  RESERVE reserve_cnt1  LINES.

  LOOP AT tab_ep.
* Build the page totals:
    ADD: tab_ep-taxed         TO wa_history-j_1aamnt01.
    ADD: tab_ep-not_taxed     TO wa_history-j_1aamnt02.
    ADD: tab_ep-vat           TO wa_history-j_1aamnt03.
    ADD: tab_ep-rnr_vat       TO wa_history-j_1aamnt04.
    ADD: tab_ep-vat_percep    TO wa_history-j_1aamnt05.
    ADD: tab_ep-other_percep  TO wa_history-j_1aamnt06.
    ADD: tab_ep-exemption     TO wa_history-j_1aamnt07.
    ADD: tab_ep-line_total    TO wa_history-j_1aamnt08.
    IF NOT par_magn IS INITIAL.
      ADD: tab_ep-earn_per       TO wa_history-j_1aamnt11.
    ENDIF.
    IF NOT tab_ep-vat_intern IS INITIAL.
      ADD: tab_ep-vat_intern    TO wa_history-j_1aamnt09.
    ELSEIF NOT tab_ep-exports IS INITIAL.
      ADD: tab_ep-exports       TO wa_history-j_1aamnt09.
    ENDIF.
    IF NOT tab_ep-munic_per IS INITIAL.
      ADD: tab_ep-munic_per     TO wa_history-j_1aamnt10.
    ELSEIF NOT tab_ep-percepnoc IS INITIAL.
      ADD: tab_ep-percepnoc     TO wa_history-j_1aamnt10.
    ENDIF.
* Note 1074703 End
  ENDLOOP.
* Note 1048207 End

*Collect totals for other tax code
  LOOP AT tab_ep_oth.
    CLEAR: tab_sum_others.
    IF tab_ep_oth-vat IS INITIAL AND
       ( NOT tab_ep_oth-rnr_vat IS INITIAL OR
       NOT tab_ep_oth-vat_percep IS INITIAL OR
       NOT tab_ep_oth-exports IS INITIAL OR
       NOT tab_ep_oth-percepnoc IS INITIAL OR
       NOT tab_ep_oth-other_percep IS INITIAL OR
       NOT tab_ep_oth-vat_intern IS INITIAL OR   " magnetic output
       NOT tab_ep_oth-munic_per IS INITIAL OR" magnetic output
       NOT tab_ep_oth-earn_per IS INITIAL ).   " Note 645449
      tab_sum_others-mwskz = tab_ep_oth-mwskz.
      tab_sum_others-kschl = tab_ep_oth-kschl.
* Needed for access of text table t685t in totals for other tax totals:
      tab_sum_others-flg_is_beleg = tab_ep_oth-flg_is_beleg.
      ADD:  tab_ep_oth-rnr_vat      TO tab_sum_others-hwste,
            tab_ep_oth-vat_percep   TO tab_sum_others-hwste,
            tab_ep_oth-exports      TO tab_sum_others-hwste,
            tab_ep_oth-percepnoc    TO tab_sum_others-hwste,
            tab_ep_oth-other_percep TO tab_sum_others-hwste,
            tab_ep_oth-vat_intern   TO tab_sum_others-hwste,
            tab_ep_oth-munic_per    TO tab_sum_others-hwste,
            tab_ep_oth-earn_per     TO tab_sum_others-hwste.
      COLLECT tab_sum_others.
    ENDIF.
  ENDLOOP.
* Note 992893 end

  flg_firstline = 'X'.
  PERFORM prepare_rahmen.
  LOOP AT tab_ep.
    CLEAR t_alv.
    lcl_tabix = sy-tabix.
    RESERVE 3 LINES.
    CLEAR: txt_zeile8.
    MOVE-CORRESPONDING tab_ep TO  hlp_epos1.
    IF NOT par_rahm IS INITIAL.
      IF NOT par_magn IS INITIAL.      " list format for magnetic output
        hlp_epos1-total+15(1) = hlp_epos1-total+34(1) = sy-vline.
      ENDIF.
    ENDIF.

    IF par_comp IS INITIAL.      " if not initial.it is build already
* display VAT in RNR_VAT colmn if par_sum set
      IF NOT par_sum IS INITIAL.
        READ TABLE tab_surcharge WITH KEY mwskz = tab_ep-mwskz.
        IF sy-subrc = 0.               " tax code contains surcharge.
          tab_ep-rnr_vat = tab_ep-vat + tab_ep-rnr_vat.
          CLEAR: tab_ep-vat.
        ENDIF.
      ENDIF.
*      perform collect_sum_tables.                       " Note 497300
    ENDIF.

* Write one record for the magnetic output
    IF NOT par_magn IS INITIAL.
      DATA: lcl_total_amnt TYPE type_ep-total.
      IF lcl_tabix NE no_of_rates.
        lcl_total_amnt = tab_ep-total.
        CLEAR: tab_ep-total.
      ENDIF.
      PERFORM prepare_magnetic_output.
      IF lcl_tabix NE no_of_rates.
        tab_ep-total = lcl_total_amnt.
      ENDIF.
    ENDIF.

    IF NOT flg_firstline IS INITIAL.
      CLEAR: txt_zeile7.

      lineno = wa_history-j_1alineno.
      hlp_epos1-lineno = lineno.
      WRITE: tab_ep-belnr TO hlp_epos1-belnr NO-ZERO.
      WRITE: tab_ep-ktnra TO hlp_epos1-hkont NO-ZERO.
      hlp_epos1-stcdt = tab_ep-stcdt.
      hlp_epos1-stcd1 = tab_ep-stcd1.

      IF par_magn IS INITIAL.
        WRITE: tab_ep-budat TO hlp_epos1-budat DD/MM/YY NO-ZERO.
        WRITE: tab_ep-bldat TO hlp_epos1-bldat DD/MM/YY NO-ZERO.
      ELSE.
        IF tab_ep-j_1aoftp = '14'.
          PERFORM read_customs_data.
          IF NOT wa_custom-date IS INITIAL.
            WRITE: wa_custom-date TO hlp_epos1-budat DD/MM/YY NO-ZERO.
          ENDIF.
        ELSE.
          WRITE: tab_ep-bldat TO hlp_epos1-budat DD/MM/YY NO-ZERO.
        ENDIF.
        WRITE: tab_ep-budat TO hlp_epos1-bldat DD/MM/YY NO-ZERO.
* Moving the document date to the output list posting date field"1016337
        WRITE: tab_ep-bldat TO hlp_epos1-budat DD/MM/YY NO-ZERO. "1016337
      ENDIF.
* Need to fill currency code from database table TCURC
* and determine exchange rate
      IF tab_curr-waers NE tab_ep-waers.
        PERFORM fill_curr_info.
      ENDIF.
      IF NOT par_magn IS INITIAL.
*        WRITE: TAB_EP-NUMPG TO HLP_EPOS1-AUGDT.  " Note 636750
        CLEAR: hlp_epos1-augdt.                   " Note 636750
        hlp_epos1-augbl = tab_curr-altkey.
        hlp_epos1-oftp_text   = tab_ep-j_1aoftp.
        WRITE: gf_exch_rate TO hlp_epos1-total(15).
        WRITE: no_of_rates TO hlp_epos1-total+16(18).
      ELSE.
        WRITE: tab_ep-augdt TO hlp_epos1-augdt DD/MM/YY NO-ZERO.
        hlp_epos1-oftp_text = tab_ep-oftp_text.
      ENDIF.

      IF tab_ep-xblnr(4)   NE space AND
      tab_ep-xblnr+5(8) NE space.
        WRITE: tab_ep-xblnr(4) TO hlp_epos1-xblnr NO-GAP,
              '-' TO hlp_epos1-xblnr+4 NO-GAP,
              tab_ep-xblnr+5(8) TO hlp_epos1-xblnr+5 NO-GAP.
      ELSE.
        IF tab_ep-xblnr+5(8) NE space.
          MOVE tab_ep-xblnr+5(8) TO hlp_epos1-xblnr.
        ENDIF.
      ENDIF.

* print sum beleg
      IF NOT par_vers IS INITIAL.
        IF tab_j_1adrver-j_1aproc EQ 1
           OR tab_j_1adrver-j_1aproc EQ 2.
          WRITE tab_ep-total TO hlp_epos1-total+35(22)
                        CURRENCY tab_001-waers NO-SIGN.
        ENDIF.
        IF tab_j_1adrver-j_1aproc EQ 3
           OR tab_j_1adrver-j_1aproc EQ 4.
          value = tab_ep-total * -1.
          WRITE value TO hlp_epos1-total+35(22) CURRENCY tab_001-waers.
        ENDIF.
      ELSE.
        WRITE tab_ep-total TO hlp_epos1-total+35(22)
                        CURRENCY tab_001-waers.
      ENDIF.
      IF NOT par_magn IS INITIAL.
* RG 1361 Changes starts
*       IF NOT tab_ep-cai IS INITIAL.
        IF NOT tab_ep-fisc_cont IS INITIAL.
          WRITE tab_ep-cai TO hlp_epos1-cai.
          WRITE tab_ep-fisc_cont TO hlp_epos1-fisc_cont.
        ELSEIF NOT tab_ep-cai IS INITIAL.
          WRITE tab_ep-cai TO hlp_epos1-cai.
*       ELSEIF NOT tab_ep-fisc_cont IS INITIAL.
*         WRITE tab_ep-fisc_cont TO hlp_epos1-cai.
* RG 1361 Changes ends
        ELSE.
          CLEAR hlp_epos1-cai.
        ENDIF.
      ELSE.
        CLEAR hlp_epos1-cai.
      ENDIF.
      txt_zeile7 = hlp_epos1.
      listsize = listsize - 1 .
      txt_zeile7+listsize = sy-vline.
      listsize = listsize + 1 .
      FORMAT: COLOR OFF,  INVERSE OFF.
      CLEAR: hlp_epos1-v1,
             hlp_epos1-v2,
             hlp_epos1-v3,
             hlp_epos1-v4,
             hlp_epos1-v5,
             hlp_epos1-v6,
             hlp_epos1-v7,
             hlp_epos1-v8,
             hlp_epos1-v9,
             hlp_epos1-v10,
             hlp_epos1-v11,
             hlp_epos1-v12,
             hlp_epos1-v13,
             hlp_epos1-v14,
             hlp_epos1-v15,
             hlp_epos1-v16,
             hlp_epos1-total.

      CLEAR hlp_epos1-total.
      MOVE-CORRESPONDING tab_ep TO t_alv.
      MOVE-CORRESPONDING hlp_epos1 TO t_alv.
      IF t_alv-xblnr IS NOT INITIAL.
        t_alv-xblnr+4(1) = '-'.
      ENDIF.
      WRITE: tab_ep-budat    TO t_alv-budat,
             tab_ep-budat(4) TO t_alv-gjahr,
             tab_ep-bldat    TO t_alv-bldat.

      t_alv-tpcbio  = tab_ep-kursf.

      LOOP AT t_bseg INTO bseg
      WHERE belnr = tab_ep-belnr
      AND   bukrs = tab_ep-bukrs
      AND   gjahr = tab_ep-gjahr.
        IF r_cust EQ 'X'.
          IF bseg-kunnr IS NOT INITIAL.
            PERFORM conversion_exit_alpha_input USING bseg-kunnr
                                             CHANGING t_alv-lifnr.
            SELECT SINGLE stcdt
            FROM lfa1
            INTO t_alv-stcdt
            WHERE lifnr EQ t_alv-lifnr.
            EXIT.
          ENDIF.
        ENDIF.
        IF r_vend EQ 'X'.
          IF bseg-lifnr IS NOT INITIAL.
            PERFORM conversion_exit_alpha_input USING bseg-lifnr
                                             CHANGING t_alv-lifnr.

            SELECT SINGLE stcdt
            FROM kna1
            INTO t_alv-stcdt
            WHERE kunnr EQ t_alv-lifnr.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      t_alv-total         = tab_ep-total.
      t_alv-taxed         = wa_history-j_1aamnt01.
      t_alv-not_taxed     = wa_history-j_1aamnt02.
      t_alv-vat           = wa_history-j_1aamnt03.
      t_alv-rnr_vat       = wa_history-j_1aamnt04.
      t_alv-vat_percep    = wa_history-j_1aamnt05.
      t_alv-other_percep  = wa_history-j_1aamnt06.
      t_alv-exemption     = wa_history-j_1aamnt07.
      t_alv-line_total    = wa_history-j_1aamnt08.
*---> 09/06/2023 - Migração S4 - JS
*      t_alv-itaxed        = wa_history-j_1aamnt01.
*      t_alv-inot_taxed    = wa_history-j_1aamnt02.
*      t_alv-ivat          = wa_history-j_1aamnt03.
*      t_alv-irnr_vat      = wa_history-j_1aamnt04.
*      t_alv-ivat_percep   = wa_history-j_1aamnt05.
*      t_alv-iother_percep = wa_history-j_1aamnt06.
*      t_alv-iexemption    = wa_history-j_1aamnt07.
      t_alv-itaxed = CONV #( wa_history-j_1aamnt01 ).
      t_alv-inot_taxed = CONV #( wa_history-j_1aamnt02 ).
      t_alv-ivat = CONV #( wa_history-j_1aamnt03 ).
      t_alv-irnr_vat = CONV #( wa_history-j_1aamnt04 ).
      t_alv-ivat_percep = CONV #( wa_history-j_1aamnt05 ).
      t_alv-iother_percep = CONV #( wa_history-j_1aamnt06 ).
      t_alv-iexemption = CONV #( wa_history-j_1aamnt07 ).
*<--- 09/06/2023 - Migração S4 - JS

      t_alv-line_total    = wa_history-j_1aamnt08.
* --- Begin ---- GGU -----
      t_alv-iearn_per     = tab_ep-earn_per.
* --- End   ---- GGU -----
      t_alv-lineno        = txt_zeile7+1(6).
      t_alv-brnch         = txt_zeile7+17(4).
      t_alv-flag = 'X'.
      t_alv-total = t_alv-total * - 1.
      CLEAR: t_alv-total,
            line_total.
      APPEND t_alv.
      HIDE: tab_ep-bukrs, tab_ep-gjahr, tab_ep-belnr,
               tab_ep-flg_sd_beleg, tab_ep-sd_vbeln,
***** BEGIN OF MODIFICATION *******
               tab_ep-flg_is_beleg.
***** END OF MODIFICATION *******
      CLEAR: flg_firstline.            " firstline is printed
    ENDIF.

* Write custom data
    IF tab_j_1adrver-j_1aproc = 1 OR tab_j_1adrver-j_1aproc = 3.
      IF flg_print_customdata IS INITIAL.
        PERFORM print_customs_data.
        flg_print_customdata = 'X'.
      ENDIF.
    ENDIF.

* Lines with the tax-code and VAT-amounts ( 1 or more for 1 document)
    IF NOT par_comp IS INITIAL.
      WRITE tab_ep-mwskz TO hlp_epos2-mwskz RIGHT-JUSTIFIED.
    ELSE.
      WRITE tab_ep-mwskz TO hlp_epos2-mwskz.
      WRITE tab_ep-ktosl TO hlp_epos2-mwskz+3.
    ENDIF.

* print rate
    CLEAR: hlp_epos2-rate.
    IF par_comp IS INITIAL.          " no compress ( detailed display )
      IF tab_ep-linetype NE '1'.
        WRITE: tab_ep-rate TO hlp_rate-rate NO-SIGN NO-GAP.
        MOVE: '%' TO hlp_rate-percent.
        WRITE: hlp_rate TO hlp_epos2-rate  RIGHT-JUSTIFIED.
      ENDIF.
    ELSE.                              " compress
      IF  NOT tab_ep-mwskz IS INITIAL.
        IF tab_ep-flg_is_beleg = 'S'.
          WRITE: tab_ep-dspl_rate TO hlp_rate-rate NO-SIGN NO-GAP.
        ELSE.
          WRITE: tab_ep-rate TO hlp_rate-rate NO-SIGN NO-GAP.
        ENDIF.
        MOVE: '%' TO hlp_rate-percent.
        WRITE: hlp_rate TO hlp_epos2-rate  RIGHT-JUSTIFIED.
      ENDIF.
    ENDIF.

* print tax base amount
    IF tab_ep-linetype NE '1'  AND
       par_comp IS INITIAL.
      CLEAR: hlp_epos2-taxed.

    ELSE.
      WRITE tab_ep-taxed TO hlp_epos2-taxed        NO-ZERO
          CURRENCY tab_001-waers.
    ENDIF.

* Display tax base for direct VAT posting
    IF tab_ep-linetype NE '1'  AND
       NOT par_dspt IS INITIAL.
      WRITE tab_ep-hwbas TO hlp_epos2-taxed        NO-ZERO
          CURRENCY tab_001-waers.
    ENDIF.

* print taxes
    WRITE tab_ep-not_taxed    TO hlp_epos2-not_taxed NO-ZERO
                                                 CURRENCY tab_001-waers.
    WRITE tab_ep-vat          TO hlp_epos2-vat NO-ZERO
                                                 CURRENCY tab_001-waers.
    WRITE tab_ep-rnr_vat      TO hlp_epos2-rnr_vat NO-ZERO
                                                 CURRENCY tab_001-waers.
    WRITE tab_ep-vat_percep   TO hlp_epos2-vat_percep NO-ZERO
                                                 CURRENCY tab_001-waers.
    WRITE tab_ep-other_percep TO hlp_epos2-other_percep NO-ZERO
                                                 CURRENCY tab_001-waers.
    WRITE tab_ep-exemption    TO hlp_epos2-exemption NO-ZERO
                                                 CURRENCY tab_001-waers.

* For internal/export amount
    IF NOT tab_ep-vat_intern IS INITIAL.
      WRITE: tab_ep-vat_intern TO hlp_epos2-exports NO-ZERO
                                                 CURRENCY tab_001-waers.
    ELSE.
      WRITE tab_ep-exports     TO hlp_epos2-exports NO-ZERO
                                                 CURRENCY tab_001-waers.
    ENDIF.
* For municipal/other perception amount
    IF NOT tab_ep-munic_per IS INITIAL.
      WRITE: tab_ep-munic_per  TO hlp_epos2-percepnoc NO-ZERO
                                                 CURRENCY tab_001-waers.
    ELSE.
      WRITE tab_ep-percepnoc TO hlp_epos2-percepnoc NO-ZERO
                                                 CURRENCY tab_001-waers.
    ENDIF.
* Note 1055550 End
    WRITE tab_ep-line_total TO hlp_epos2-line_total
                                                 CURRENCY tab_001-waers.
    IF NOT par_magn IS INITIAL.

* Begin of note 988302
      IF find_exempted = 'X'.
        WRITE tab_ep-j_1arfz TO hlp_epos2-exempt.
      ELSE.
        CLEAR hlp_epos2-exempt.
      ENDIF.
      CLEAR find_exempted .
* End of note 988302
      WRITE tab_ep-earn_per TO hlp_epos2-earn_per NO-ZERO
                                               CURRENCY tab_001-waers.
    ENDIF.
    txt_zeile8 = hlp_epos2.
    listsize = listsize - 1 .
    txt_zeile8+listsize = sy-vline.
    listsize = listsize + 1 .
*    FORMAT: COLOR 2,  INVERSE ON.
*    WRITE / txt_zeile8.



*[BEGIN] -- PM -- 20/08/09
    DESCRIBE TABLE t_alv LINES v_lines.
    DO.
      READ TABLE t_alv INDEX v_lines.
      IF t_alv-flag = 'X'.
        EXIT.
      ENDIF.
      v_lines = v_lines - 1.
    ENDDO.

    DO 5 TIMES.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-lineno    WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-budat     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-brnch     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-belnr     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-koart     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-hkont     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-name1     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-stcdt     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-stcd1     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-bldat     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-xblnr     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-oftp_text WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-augdt     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-augbl     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-total     WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-cai       WITH space.
      REPLACE ALL OCCURRENCES OF '|' IN hlp_epos1-fisc_cont WITH space.
    ENDDO.

    CLEAR: hlp_epos1-v1,
           hlp_epos1-v2,
           hlp_epos1-v3,
           hlp_epos1-v4,
           hlp_epos1-v5,
           hlp_epos1-v6,
           hlp_epos1-v7,
           hlp_epos1-v8,
           hlp_epos1-v9,
           hlp_epos1-v10,
           hlp_epos1-v11,
           hlp_epos1-v12,
           hlp_epos1-v13,
           hlp_epos1-v14,
           hlp_epos1-v15,
           hlp_epos1-v16,
           hlp_epos1-total.

* --- Begin ---- GGU -----
* Se elimina la lógica de la tabla tab_ep_noh por traer inconsistencias
    CLEAR t_alv.
    MOVE-CORRESPONDING tab_ep TO t_alv.
*    MOVE-CORRESPONDING hlp_epos1 TO t_alv.
    IF t_alv-xblnr IS NOT INITIAL.
      t_alv-xblnr+4(1) = '-'.
    ENDIF.
    t_alv-tpcbio  = tab_ep-kursf.
    WRITE: tab_ep-budat    TO t_alv-budat,
           tab_ep-budat(4) TO t_alv-gjahr,
           tab_ep-bldat    TO t_alv-bldat.
    LOOP AT t_bseg INTO bseg
    WHERE belnr = tab_ep-belnr
    AND   bukrs = tab_ep-bukrs
    AND   gjahr = tab_ep-gjahr.
      IF r_cust EQ 'X'.
        IF bseg-kunnr IS NOT INITIAL.
          PERFORM conversion_exit_alpha_input USING bseg-kunnr
                                           CHANGING t_alv-lifnr.
          SELECT SINGLE stcdt
          FROM lfa1
          INTO t_alv-stcdt
          WHERE lifnr EQ t_alv-lifnr.
          EXIT.
        ENDIF.
      ENDIF.
      IF r_vend EQ 'X'.
        IF bseg-lifnr IS NOT INITIAL.
          PERFORM conversion_exit_alpha_input USING bseg-lifnr
                                           CHANGING t_alv-lifnr.

          SELECT SINGLE stcdt
          FROM kna1
          INTO t_alv-stcdt
          WHERE kunnr EQ t_alv-lifnr.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    READ TABLE t_bset INTO bset
    WITH KEY kschl = 'MWVS'
             mwskz = tab_ep-mwskz
             belnr = tab_ep-belnr.
    IF sy-subrc EQ 0.
      bset-kbetr = bset-kbetr / 10.
      WRITE bset-kbetr TO t_alv-rate.
    ELSE.
      CLEAR bset-kbetr .
      WRITE bset-kbetr TO t_alv-rate.
    ENDIF.
    IF tab_ep-mwskz EQ 'C0'.
      READ TABLE t_bset INTO bset
      WITH KEY kschl = 'MWVS'
          mwskz = tab_ep-mwskz
          belnr = tab_ep-belnr.
      IF sy-subrc EQ 0.

        IF bset-shkzg EQ 'S'.
*---> 09/06/2023 - Migração S4 - JS
*          MOVE bset-hwbas TO tab_ep-exemption.
*        ELSEIF bset-shkzg EQ 'H'.
*          MOVE bset-hwbas TO tab_ep-exemption.
          tab_ep-exemption = CONV #( bset-hwbas ).
        ELSEIF bset-shkzg EQ 'H'.
          tab_ep-exemption = CONV #( bset-hwbas ).
*<--- 09/06/2023 - Migração S4 - JS
          tab_ep-exemption = tab_ep-exemption * ( - 1 ) .
        ELSE.
        ENDIF.
      ENDIF.
    ELSE.
* Importe del IVA
      READ TABLE tab_sum_mwskz
      WITH KEY mwskz = tab_ep-mwskz.
      IF sy-subrc EQ 0.
        MOVE: tab_sum_mwskz-hwste TO t_alv-vat ,
              tab_sum_mwskz-hwste TO t_alv-ivat.
      ENDIF.
    ENDIF.

    IF  tab_ep-kschl IS INITIAL.
      READ TABLE t_bset INTO bset
     WITH KEY gjahr = tab_ep-gjahr
              mwskz = tab_ep-mwskz
              belnr = tab_ep-belnr.

      MOVE: bset-kschl               TO t_alv-kschl        .
    ENDIF.
    MOVE: tab_ep-taxed             TO t_alv-itaxed       ,
          tab_ep-not_taxed         TO t_alv-inot_taxed   ,
          tab_ep-rnr_vat           TO t_alv-irnr_vat     ,
          tab_ep-vat_percep        TO t_alv-ivat_percep  ,
          tab_ep-other_percep      TO t_alv-iother_percep,
* --- Begin ---- GGU -----
          tab_ep-earn_per          TO t_alv-iearn_per,
* --- End   ---- GGU -----
          tab_ep-exemption         TO t_alv-iexemption   .

    APPEND t_alv.
    CONTINUE.
*    IF t_alv-xblnr IS NOT INITIAL.
*      t_alv-xblnr+4(1) = '-'.
*    ENDIF.
*    WRITE tab_ep-budat TO t_alv-budat.
*    WRITE tab_ep-bldat TO t_alv-bldat.
*    t_alv-total = tab_ep-total .
*    t_alv-mwskz = txt_zeile8+1(6).
*    t_alv-rate = txt_zeile8+8(8).
*
*    SORT tab_ep_oth BY mwskz.
*    DATA: stl_alv LIKE t_alv.
*    LOOP AT tab_ep_oth WHERE belnr EQ tab_ep-belnr.
*      CLEAR:
*            t_alv-taxed           ,
*            t_alv-not_taxed       ,
*            t_alv-vat             ,
*            t_alv-rnr_vat         ,
*            t_alv-vat_percep      ,
*            t_alv-other_percep    ,
*            t_alv-exemption       ,
*            t_alv-line_total      ,
*            t_alv-itaxed          ,
*            t_alv-inot_taxed      ,
*            t_alv-ivat            ,
*            t_alv-irnr_vat        ,
*            t_alv-ivat_percep     ,
*            t_alv-iother_percep   ,
*            t_alv-iexemption      ,
*            t_alv-line_total      .
*
*      IF tab_ep_oth-mwskz EQ 'C0'.
*        CLEAR:     stl_alv-total         ,
*                   stl_alv-taxed         ,
*                   stl_alv-not_taxed     ,
*                   stl_alv-vat           ,
*                   stl_alv-rnr_vat       ,
*                   stl_alv-vat_percep    ,
*                   stl_alv-other_percep  ,
*                   stl_alv-exemption     ,
*                   stl_alv-line_total    ,
*                   stl_alv-itaxed        ,
*                   stl_alv-inot_taxed    ,
*                   stl_alv-ivat          ,
*                   stl_alv-irnr_vat      ,
*                   stl_alv-ivat_percep   ,
*                   stl_alv-iother_percep ,
*                   stl_alv-iexemption    ,
*                   stl_alv-line_total    ,
*                   stl_alv-total         .
*
*      ENDIF.
*
**   el Importe total debe salir en 1 sola posicion
*      IF sy-tabix = 1.
*        t_alv-total         = tab_ep_oth-total."tab_ep-total.
*      ELSE.
*        CLEAR: t_alv-total.
*      ENDIF.
*
*      SHIFT t_alv-belnr RIGHT DELETING TRAILING space.
*      OVERLAY t_alv-belnr WITH '0000000000'.
*
*      stl_alv-lineno    = t_alv-lineno.
*      stl_alv-budat     = t_alv-budat.
*      stl_alv-brnch     = t_alv-brnch.
*      stl_alv-belnr     = t_alv-belnr.
*      stl_alv-koart     = t_alv-koart.
*      stl_alv-hkont     = t_alv-hkont.
*      stl_alv-name1     = t_alv-name1.
*      stl_alv-stcdt     = t_alv-stcdt.
*      stl_alv-stcd1     = t_alv-stcd1.
*      stl_alv-bldat     = t_alv-bldat.
*      stl_alv-xblnr     = t_alv-xblnr.
*      stl_alv-oftp_text = t_alv-oftp_text.
*      stl_alv-augdt     = t_alv-augdt.
*      stl_alv-augbl     = t_alv-augbl.
*
*      MOVE:
*           tab_ep_oth-hkont TO t_alv-hkont,
*           tab_ep_oth-mwskz TO t_alv-mwskz.
*
*      READ TABLE t_bset INTO bset
*      WITH KEY kschl = 'MWVS'
*               mwskz = tab_ep_oth-mwskz
*               belnr = tab_ep_oth-belnr.
*      IF sy-subrc EQ 0.
*        bset-kbetr = bset-kbetr / 10.
*        WRITE bset-kbetr TO t_alv-rate.
*      ELSE.
*        CLEAR bset-kbetr .
*        WRITE bset-kbetr TO t_alv-rate.
*      ENDIF.
*      IF tab_ep_oth-mwskz EQ 'C0'.
**        IF  tab_ep_oth-kschl IS INITIAL.
**          CONTINUE.
**        ENDIF.
*        READ TABLE t_bset INTO bset
*   WITH KEY kschl = 'MWVS'
*            mwskz = tab_ep_oth-mwskz
*            belnr = tab_ep_oth-belnr.
*        IF sy-subrc EQ 0.
*
*          IF bset-shkzg EQ 'S'.
*            MOVE bset-hwbas TO tab_ep_oth-exemption.
*          ELSEIF bset-shkzg EQ 'H'.
*            MOVE bset-hwbas TO tab_ep_oth-exemption.
*            tab_ep_oth-exemption = tab_ep_oth-exemption * ( - 1 ) .
*          ELSE.
*          ENDIF.
*        ENDIF.
*      ELSE.
** Importe del IVA
*        READ TABLE tab_sum_mwskz
*        WITH KEY mwskz = tab_ep_oth-mwskz.
*        IF sy-subrc EQ 0.
*          MOVE: tab_sum_mwskz-hwste TO t_alv-vat ,
*                tab_sum_mwskz-hwste TO t_alv-ivat.
*        ENDIF.
*      ENDIF.
*
*      IF  tab_ep_oth-kschl IS INITIAL.
*        READ TABLE t_bset INTO bset
*       WITH KEY gjahr = tab_ep_oth-gjahr
*                mwskz = tab_ep_oth-mwskz
*                belnr = tab_ep_oth-belnr.
*
*        MOVE:
*                   bset-kschl               TO t_alv-kschl        .
*      ENDIF.
*      MOVE:
*            tab_ep_oth-taxed         TO t_alv-taxed        ,
*            tab_ep_oth-not_taxed     TO t_alv-not_taxed    ,
*            tab_ep_oth-rnr_vat       TO t_alv-rnr_vat      ,
*            tab_ep_oth-vat_percep    TO t_alv-vat_percep   ,
*            tab_ep_oth-other_percep  TO t_alv-other_percep ,
*            tab_ep_oth-exemption     TO t_alv-exemption    ,
*            tab_ep_oth-taxed         TO t_alv-itaxed       ,
*            tab_ep_oth-not_taxed     TO t_alv-inot_taxed   ,
*            tab_ep_oth-rnr_vat       TO t_alv-irnr_vat     ,
*            tab_ep_oth-vat_percep    TO t_alv-ivat_percep  ,
*            tab_ep_oth-other_percep  TO t_alv-iother_percep,
*            tab_ep_oth-exemption     TO t_alv-iexemption   .
*
*      IF t_alv-total IS NOT INITIAL.
*        stl_alv-total         = t_alv-total.
*      ENDIF.
*
*      stl_alv-taxed          = t_alv-taxed         + stl_alv-taxed       .
*      stl_alv-not_taxed      = t_alv-not_taxed     + stl_alv-not_taxed   .
*      stl_alv-rnr_vat        = t_alv-rnr_vat       + stl_alv-rnr_vat     .
*      stl_alv-vat_percep     = t_alv-vat_percep    + stl_alv-vat_percep  .
*      stl_alv-other_percep   = t_alv-other_percep  + stl_alv-other_percep.
*      stl_alv-exemption      = t_alv-exemption     + stl_alv-exemption   .
*      stl_alv-itaxed         = t_alv-itaxed        + stl_alv-itaxed       .
*      stl_alv-inot_taxed     = t_alv-inot_taxed    + stl_alv-inot_taxed   .
*      stl_alv-irnr_vat       = t_alv-irnr_vat      + stl_alv-irnr_vat     .
*      stl_alv-ivat_percep    = t_alv-ivat_percep   + stl_alv-ivat_percep  .
*      stl_alv-iother_percep  = t_alv-iother_percep + stl_alv-iother_percep.
*      stl_alv-iexemption     = t_alv-iexemption    + stl_alv-iexemption   .
*
*      AT END OF mwskz.
*        MOVE:
*             stl_alv-taxed         TO t_alv-taxed         ,
*             stl_alv-not_taxed     TO t_alv-not_taxed     ,
*             stl_alv-rnr_vat       TO t_alv-rnr_vat       ,
*             stl_alv-vat_percep    TO t_alv-vat_percep    ,
*             stl_alv-other_percep  TO t_alv-other_percep  ,
*             stl_alv-exemption     TO t_alv-exemption     ,
*             stl_alv-taxed         TO t_alv-itaxed        ,
*             stl_alv-not_taxed     TO t_alv-inot_taxed    ,
*             stl_alv-rnr_vat       TO t_alv-irnr_vat      ,
*             stl_alv-vat_percep    TO t_alv-ivat_percep   ,
*             stl_alv-other_percep  TO t_alv-iother_percep ,
*             stl_alv-exemption     TO t_alv-iexemption    .
*
*        LOOP AT t_bseg INTO bseg
*          WHERE belnr = tab_ep-belnr
*          AND   bukrs = tab_ep-bukrs
*          AND   gjahr = tab_ep-gjahr.
*          IF r_cust EQ 'X'.
*            IF bseg-kunnr IS NOT INITIAL.
*              PERFORM conversion_exit_alpha_input USING bseg-kunnr
*                                               CHANGING t_alv-lifnr.
*              SELECT SINGLE stcdt
*              FROM lfa1
*              INTO t_alv-stcdt
*              WHERE lifnr EQ t_alv-lifnr.
*
*              EXIT.
*            ENDIF.
*          ENDIF.
*          IF r_vend EQ 'X'.
*            IF bseg-lifnr IS NOT INITIAL.
*              PERFORM conversion_exit_alpha_input USING bseg-lifnr
*                                               CHANGING t_alv-lifnr.
*              SELECT SINGLE stcdt
*              FROM kna1
*              INTO t_alv-stcdt
*              WHERE kunnr EQ t_alv-lifnr.
*              EXIT.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*
*        t_alv-flag = space.
*
*        t_alv-total = t_alv-total * - 1.
*        CLEAR stl_alv.
*        CLEAR: t_alv-total,
*       line_total.
*
*        READ TABLE t_bset INTO bset
*        WITH KEY belnr = t_alv-belnr
*                 mwskz = t_alv-mwskz.
*
*        IF sy-subrc EQ 0.
*          t_alv-kschl = bset-kschl.
*          t_alv-ktosl = bset-ktosl.
*        ENDIF.
*
*        APPEND t_alv.
*        CLEAR: stl_alv.
*      ENDAT.
*    ENDLOOP.
**[ END ] -- PM -- 20/08/09
*    SORT t_alv BY belnr mwskz hkont .
*    DELETE ADJACENT DUPLICATES FROM t_alv COMPARING belnr hkont mwskz.
*    FORMAT: COLOR OFF,  INVERSE OFF.
*
** print regio tab
**    AT END OF mwskz.
**      PERFORM print_tab_regio .
**    ENDAT.
* --- End   ---- GGU -----

  ENDLOOP.
  SORT t_alv BY budat.
* Fill Perception File here
  IF NOT par_magn IS INITIAL AND
     NOT par_perf IS INITIAL.
    LOOP AT tab_p_record2 INTO p_record2.
      CLEAR t_alv.
      PERFORM print_p_record  USING p_record2.
      TRANSFER p_record2+6(94) TO par_perf LENGTH 94.
      PERFORM transfer_p_record.  "Moving Perception Record to file Note - 1120234.
    ENDLOOP.
  ENDIF.

  IF NOT par_rahm IS INITIAL.
*    ULINE.
  ENDIF.
ENDFORM. " PRINT_TAB_EP

*&---------------------------------------------------------------------*
*&      Form  COMPRES_TAB_EP
*&---------------------------------------------------------------------*
* compress tax code in tab_ep.
* TABLE: TAB_EP
*----------------------------------------------------------------------*
FORM compres_tab_ep.
* local data declaration for correct display of compressed SD docs
  DATA: BEGIN OF local_rate OCCURS 0,
          hlp_rate LIKE tab_ep-rate,
          posnr    LIKE tab_ep_comp-posnr,
          hwbas    TYPE type_ep-hwbas, " Note 627651: PAR_DSPT display
        END OF local_rate,
        same_rate  TYPE c,
        tabix      LIKE sy-tabix,
        rate_tabix LIKE sy-tabix,      " Note 627651
        hlp_1arfz  LIKE j_1arztx-j_1arfz.
  DATA: BEGIN OF lcl_tab_exempt OCCURS 0,
          mwskz   LIKE bseg-mwskz,
          j_1arfz LIKE j_1arztx-j_1arfz,
        END OF lcl_tab_exempt.

* Magnetic output: Amounts that have to be aggregated to last record
  DATA:
    lcl_total        TYPE type_ep-total,
    lcl_line_total   TYPE type_ep-line_total, " Needed as well
    lcl_not_taxed    TYPE type_ep-not_taxed,
    lcl_exemption    TYPE type_ep-exemption,
    lcl_vat_percep   TYPE type_ep-vat_percep,
    lcl_other_percep TYPE type_ep-other_percep,
    lcl_munic_per    TYPE type_ep-munic_per,
    lcl_earn_per     TYPE type_ep-earn_per,
    lcl_vat_intern   TYPE type_ep-vat_intern.
  DATA: lcl_prev_rate  TYPE type_ep-rate,
        lcl_prev_mwskz TYPE type_ep-mwskz.
  DATA: lcl_current_tabix LIKE sy-tabix.
  DATA: lcl_ktosl         TYPE type_ep-ktosl.

* Local data for Perception File
  DATA: lcl_percfile_regio(8) TYPE n, "TYPE_EP-OTHER_PERCEP,
        lcl_percfile_counc(8) TYPE n, "TYPE_EP-MUNIC_PER,
        lcl_perctabix         LIKE sy-tabix.

  CLEAR: lcl_not_taxed,    lcl_exemption,  lcl_vat_percep,
         lcl_other_percep, lcl_munic_per,
         lcl_earn_per,     lcl_vat_intern,
         lcl_prev_rate,    lcl_prev_mwskz, no_of_rates,
         lcl_line_total.

  CLEAR: lcl_percfile_regio, lcl_percfile_counc.

* Note 992893 start
* Move the other tax details for displaying the summary
  REFRESH tab_ep_oth.                                       "1001697
  CLEAR tab_ep_oth.                                         "1001697
  LOOP AT tab_ep.
    IF tab_ep-vat IS INITIAL.
      MOVE-CORRESPONDING tab_ep TO tab_ep_oth.
      APPEND tab_ep_oth.
    ENDIF.
  ENDLOOP.
* Note 992893 end

  IF NOT par_comp IS INITIAL.
    REFRESH: tab_ep_comp, local_rate, lcl_tab_exempt.
    LOOP AT tab_ep.
      tabix = sy-tabix.                        " Note 627651
* display VAT in RNR_VAT colmn if par_sum set
      IF NOT par_sum IS INITIAL.
        READ TABLE tab_surcharge WITH KEY mwskz = tab_ep-mwskz.
        IF sy-subrc = 0.
          tab_ep-rnr_vat = tab_ep-vat + tab_ep-rnr_vat.
*        tab_ep-vat_percep = tab_ep-vat + tab_ep-vat_percep.
          CLEAR: tab_ep-vat.
        ENDIF.
      ENDIF.

      IF NOT par_magn IS INITIAL.
* Prepare Perception File: One entry for each processing key
*                          for regional or municipal perceptions
* Perception record only required if perceptions are involved

* Get correct Region for Processing Key
        PERFORM read_j_1ataxid   USING tab_001-kalsm
                                       tab_ep-ktosl.
        IF ( tab_j_1ataxid-j_1ataxid(2) EQ 'GP' ) OR
           ( tab_j_1ataxid-j_1ataxid(2) EQ 'MP' ).
          IF NOT tab_ep-other_percep IS INITIAL OR
             NOT tab_ep-munic_per   IS INITIAL.
            PERFORM fill_p_record CHANGING p_record2.
            p_record2-regio = tab_j_1ataxid-regio.
            p_record2-counc = tab_j_1ataxid-counc.
            READ TABLE tab_p_record2 WITH KEY regio = p_record2-regio
                                              counc = p_record2-counc.
            IF sy-subrc NE 0. " Entry not yet existing
              APPEND p_record2 TO tab_p_record2.
            ELSE.
              lcl_perctabix = sy-tabix.
*   Build sum for regional and municipal perception
              lcl_percfile_regio = tab_ep-other_percep.
              p_record2-perc_gi_amnt =
                        tab_p_record2-perc_gi_amnt + lcl_percfile_regio.
              lcl_percfile_counc = tab_ep-munic_per.
              p_record2-perc_mp_amnt =
                        tab_p_record2-perc_mp_amnt + lcl_percfile_counc.
              MODIFY tab_p_record2 FROM p_record2 INDEX lcl_perctabix.
            ENDIF.
*      COLLECT p_record2 into tab_p_record2.
          ENDIF.
        ENDIF.
      ENDIF.

* Clear exempt reason in order to avoid problems with statement
* COLLECT tab_ep_comp -> Else, this table would be filled with 2 lines
      CLEAR: hlp_1arfz.
      hlp_1arfz = tab_ep-j_1arfz.
      CLEAR: tab_ep-j_1arfz.
      MOVE-CORRESPONDING tab_ep TO tab_ep_comp.
      tab_ep-j_1arfz = hlp_1arfz.
* Fill table lcl_tab_exempt with all tax codes of this document
* that contain an exempt reason, for correct magnetic output
* Because exempt reason is defined per tax code.
      IF NOT tab_ep-j_1arfz IS INITIAL.
        lcl_tab_exempt-mwskz = tab_ep-mwskz.
        lcl_tab_exempt-j_1arfz = tab_ep-j_1arfz.
        COLLECT lcl_tab_exempt.
      ENDIF.

* Get correct KTOSL for perception -> Read J_1ATAXID with this key.
      IF lcl_ktosl IS INITIAL.
* Take KTOSL for first doc line with region. and/or municipal perception
        IF tab_ep-other_percep NE 0
          OR tab_ep-munic_per NE 0.
          lcl_ktosl = tab_ep-ktosl.
        ENDIF.
      ENDIF.

* Now: Fulfill requirement that for SD documents in compress, lines
* with same rate but different posnr have to be printed in one line.
      AT NEW posnr. " tab_ep is sorted by mwskz posnr rate descending.
        READ TABLE tab_ep INDEX tabix. " need to know tab_ep-rate
* build table with all occurring rates and their posnr
* READ TABLE LOCAL_RATE WITH KEY HLP_RATE = TAB_EP-RATE. "Note 712000
        READ TABLE local_rate WITH KEY hlp_rate = tab_ep-rate
                                   posnr = tab_ep-posnr. "Note 712000

        IF sy-subrc NE 0.
          local_rate-posnr    = tab_ep-posnr.
          local_rate-hlp_rate = tab_ep-rate.
          local_rate-hwbas    = tab_ep-hwbas.   " Note 627651
          APPEND local_rate.
* Begin Note 627651
        ELSE.
          rate_tabix = sy-tabix.
          local_rate-hwbas = local_rate-hwbas + tab_ep-hwbas.
          MODIFY local_rate INDEX rate_tabix.
          sy-tabix = rate_tabix.
* End   Note 627651
        ENDIF.
        CLEAR same_rate.
* If highest rate is same, all following lines with same posnr have
* to be aggregated into accordingly.
        IF tab_ep-rate = local_rate-hlp_rate.
          same_rate = 'X'.
        ENDIF.
      ENDAT.
      IF same_rate = 'X'.
        tab_ep_comp-posnr = local_rate-posnr.
      ENDIF.
      COLLECT tab_ep_comp.
*      perform collect_sum_tables.                        " Note 497300
    ENDLOOP.

* find the tax rate
    LOOP AT tab_ep_comp.
      LOOP AT tab_ep.
        IF tab_ep-mwskz = tab_ep_comp-mwskz
        AND tab_ep-posnr = tab_ep_comp-posnr.
          IF NOT par_kts1 IS INITIAL.
            IF tab_ep-ktosl EQ par_kts1.
              tab_ep_comp-dspl_rate = tab_ep-rate.
              EXIT.
            ENDIF.
          ELSE.
* Begin modification Note 556504 - only show tax rate for VAT tax
            PERFORM read_j_1ataxid USING tab_001-kalsm tab_ep-ktosl.
            IF tab_j_1ataxid-j_1ataxid(2) = 'TX'.
              tab_ep_comp-dspl_rate = tab_ep-rate.
*             EXIT.                                       "1085024
              MODIFY tab_ep_comp.                           "1085024
              CONTINUE.                                     "1085024
            ENDIF.
* End modification Note 556504
          ENDIF.
        ENDIF.
      ENDLOOP.
*      MODIFY tab_ep_comp.                                 "1085024
    ENDLOOP.

* move tab_ep_comp to tab_ep
    REFRESH: tab_ep_tmp.
    LOOP AT tab_ep_comp.
      LOOP AT tab_ep.
*        IF TAB_EP-MWSKZ = TAB_EP_COMP-MWSKZ.   "Note 712973
        IF tab_ep-mwskz = tab_ep_comp-mwskz    "Note 712973
       AND tab_ep-posnr = tab_ep_comp-posnr.
          MOVE-CORRESPONDING tab_ep TO tab_ep_tmp.
          MOVE-CORRESPONDING tab_ep_comp TO tab_ep_tmp.
* Now fill line for exempted tax code with exempt reason
          READ TABLE lcl_tab_exempt WITH KEY mwskz = tab_ep_comp-mwskz.
          IF sy-subrc = 0.
            tab_ep_tmp-j_1arfz = lcl_tab_exempt-j_1arfz.
          ENDIF.
          APPEND tab_ep_tmp.
          DELETE tab_ep.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    REFRESH: tab_ep.
    IF NOT par_magn IS INITIAL.
      SORT tab_ep_tmp BY rate ASCENDING.
    ENDIF.

    LOOP AT tab_ep_tmp.
      MOVE-CORRESPONDING tab_ep_tmp TO tab_ep.
      IF par_magn IS INITIAL.
        APPEND tab_ep.
      ELSE.
        IF NOT lcl_ktosl IS INITIAL.
          tab_ep-ktosl = lcl_ktosl.
        ENDIF.
        IF sy-tabix = 1.
          APPEND tab_ep.
          lcl_prev_rate  = tab_ep-rate.
          lcl_prev_mwskz = tab_ep-mwskz.
          CONTINUE.
        ENDIF.
* Note 1031583 Start
* If the local tax code is different from TAB_EP tax code
* assign the TAB_EP tax code to local
        IF lcl_prev_mwskz NE tab_ep-mwskz.
          lcl_prev_mwskz = tab_ep-mwskz.
        ENDIF.
* Note 1031583 End

        IF tab_ep-rate EQ lcl_prev_rate.
          READ TABLE tab_ep WITH KEY mwskz = lcl_prev_mwskz
                                     rate = lcl_prev_rate.  "1006186
*Commented by 1074703                 posnr = tab_ep-posnr.  "1032337
* Note 1031583 Start
* If the tax code not found in TAB_EP then append
* the same.
          IF sy-subrc NE 0.
            APPEND tab_ep.
            CONTINUE.
          ENDIF.
* Note 1031583 End
          lcl_current_tabix   = sy-tabix.
* Note 1032337 Start
* Summing up the tax base amount and the tax amount also
          tab_ep-hwbas        = tab_ep-hwbas + tab_ep_tmp-hwbas.
          tab_ep-hwste        = tab_ep-hwste + tab_ep_tmp-hwste.
* Note 1032337 End
          tab_ep-taxed        = tab_ep-taxed + tab_ep_tmp-taxed.
          tab_ep-not_taxed    = tab_ep-not_taxed + tab_ep_tmp-not_taxed.
          tab_ep-vat          = tab_ep-vat + tab_ep_tmp-vat.
          tab_ep-rnr_vat      = tab_ep-rnr_vat + tab_ep_tmp-rnr_vat.
          tab_ep-vat_percep   = tab_ep-vat_percep
                                              + tab_ep_tmp-vat_percep.
          tab_ep-other_percep = tab_ep-other_percep
                                              + tab_ep_tmp-other_percep.
          tab_ep-munic_per    = tab_ep-munic_per + tab_ep_tmp-munic_per.
          tab_ep-earn_per     = tab_ep-earn_per + tab_ep_tmp-earn_per.
          tab_ep-vat_intern   = tab_ep-vat_intern
                                              + tab_ep_tmp-vat_intern.
          tab_ep-exemption  = tab_ep-exemption + tab_ep_tmp-exemption.
          tab_ep-surcharge  = tab_ep-surcharge + tab_ep_tmp-surcharge.
          tab_ep-line_total = tab_ep-line_total + tab_ep_tmp-line_total.
          MODIFY tab_ep INDEX lcl_current_tabix.
        ELSE.
          APPEND tab_ep.
        ENDIF.
        lcl_prev_rate  = tab_ep-rate.
        lcl_prev_mwskz = tab_ep-mwskz.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE tab_ep LINES no_of_rates.
  ENDIF.

  CHECK: NOT par_magn IS INITIAL.
* Falls gewünscht, dass höchster Steuersatz zuerst kommt:
* sort tab_ep by rate descending.
* Jetzt Korrektur von tab_ep: Aggregation pro Steuerrate, bestimmte
* Beträge nur in letzte Zeile.
  CLEAR: tabix.
  LOOP AT tab_ep.
* Begin note 627651
*   Correct tab_ep-hwbas from local_rate in order to prevent problems
*   with parameter par_dspt later.
    rate_tabix = sy-tabix.
    READ TABLE local_rate WITH KEY hlp_rate = tab_ep-rate
                                   posnr    = tab_ep-posnr.
    IF sy-subrc EQ 0.
      IF local_rate-hwbas NE tab_ep-hwbas.
        tab_ep-hwbas = local_rate-hwbas.
        MODIFY tab_ep.
      ENDIF.
    ENDIF.
    sy-tabix = rate_tabix.
    IF no_of_rates EQ 1. " Not more than one rate
      CONTINUE.
    ENDIF.
* End   note 627651
    IF sy-tabix = 1.
      lcl_total = tab_ep-total.
    ENDIF.
    lcl_not_taxed = lcl_not_taxed + tab_ep-not_taxed.
    CLEAR tab_ep-not_taxed.
    lcl_exemption = lcl_exemption + tab_ep-exemption.
    CLEAR tab_ep-exemption.
    lcl_vat_percep = lcl_vat_percep + tab_ep-vat_percep.
    CLEAR tab_ep-vat_percep.
    lcl_other_percep = lcl_other_percep + tab_ep-other_percep.
    CLEAR tab_ep-other_percep.
    lcl_munic_per = lcl_munic_per + tab_ep-munic_per.
    CLEAR tab_ep-munic_per.
    lcl_earn_per  = lcl_earn_per + tab_ep-earn_per.
    CLEAR tab_ep-earn_per.
    lcl_vat_intern = lcl_vat_intern + tab_ep-vat_intern.
    CLEAR tab_ep-vat_intern.
    lcl_line_total = lcl_line_total + tab_ep-line_total.
    CLEAR tab_ep-line_total.
    DESCRIBE TABLE tab_ep LINES no_of_rates.                "1058403
* Check whether ALL amounts and tax rate are zero now
* -> If so, delete line. Case may only occur of Not taxed and/or
*    Exempted amount are aggregated to last line (with rate ne 0)
    PERFORM check_for_zero_line USING tab_ep
                                CHANGING rcode.
* Note 1058403 Start
*    IF rcode NE 0.
*      tabix = sy-tabix.
    IF rcode NE 0 OR tab_ep-rate IS INITIAL.
      DELETE tab_ep.
* Note 1058403 End
    ELSE.
      MODIFY tab_ep.
    ENDIF.
    IF sy-tabix = no_of_rates.
      tab_ep-total = lcl_total.
*      tab_ep-line_total   = lcl_line_total.                 "1074703
      tab_ep-not_taxed    = lcl_not_taxed.

      tab_ep-exemption    = lcl_exemption.
      tab_ep-vat_percep   = lcl_vat_percep.
      tab_ep-other_percep = lcl_other_percep.
      tab_ep-munic_per    = lcl_munic_per.
      tab_ep-earn_per     = lcl_earn_per.
      tab_ep-vat_intern   = lcl_vat_intern.
* Note 1074703 Start
      tab_ep-line_total   = tab_ep-taxed + tab_ep-vat +
                            lcl_not_taxed + lcl_exemption +
                            lcl_vat_percep + lcl_other_percep +
                            lcl_munic_per + lcl_earn_per +
                            lcl_vat_intern.
      MODIFY TABLE tab_ep. " index no_of_rates.
* If not last line and VAT tax then assign the VAT
* to the line total
    ELSEIF NOT tab_ep-rate IS INITIAL.
      tab_ep-line_total   = tab_ep-taxed + tab_ep-vat.      "1085024
      MODIFY tab_ep.                                        "1085024
* Note 1074703 End
    ENDIF.
  ENDLOOP.
* Note 1058403 Start
*  IF NOT par_magn IS INITIAL AND
*     NOT tabix    IS INITIAL.
*    DELETE tab_ep INDEX tabix.
*    DESCRIBE TABLE tab_ep LINES no_of_rates.
  IF NOT par_magn IS INITIAL.
* Note 1058403 End
* Get correct value for exempt reason
    IF NOT lcl_tab_exempt[] IS INITIAL. " exemption exists
      READ TABLE lcl_tab_exempt INDEX 1.
      READ TABLE tab_ep INDEX no_of_rates.
      tab_ep-j_1arfz = lcl_tab_exempt-j_1arfz.
      MODIFY tab_ep INDEX no_of_rates.
    ENDIF.
  ENDIF.
ENDFORM. " COMPRES_TAB_EP

*&---------------------------------------------------------------------*
*&      Form  COLLECT_SUM_TABLES
*&---------------------------------------------------------------------*
* build the totals for current line.
* TABLE: TAB_EP
*----------------------------------------------------------------------*
FORM collect_sum_tables.
* Note 1048207 Start
** Build the page totals:
*  ADD: tab_ep-taxed         TO wa_history-j_1aamnt01.
*  ADD: tab_ep-not_taxed     TO wa_history-j_1aamnt02.
*  ADD: tab_ep-vat           TO wa_history-j_1aamnt03.
*  ADD: tab_ep-rnr_vat       TO wa_history-j_1aamnt04.
*  ADD: tab_ep-vat_percep    TO wa_history-j_1aamnt05.
*  ADD: tab_ep-other_percep  TO wa_history-j_1aamnt06.
*  ADD: tab_ep-exemption     TO wa_history-j_1aamnt07.
*  ADD: tab_ep-line_total    TO wa_history-j_1aamnt08.
*  IF par_magn IS INITIAL.
*    ADD: tab_ep-exports       TO wa_history-j_1aamnt09.
*    ADD: tab_ep-percepnoc     TO wa_history-j_1aamnt10.
*  ELSE.
**   ADD: TAB_EP-EARN_PER      TO WA_HISTORY-J_1AAMNT06.    " Note 685915
*    ADD: tab_ep-earn_per      TO wa_history-j_1aamnt11.    " Note 685915
*    ADD: tab_ep-vat_intern    TO wa_history-j_1aamnt09.
*    ADD: tab_ep-munic_per     TO wa_history-j_1aamnt10.
*  ENDIF.
* Note 1048207 End

* collect totals per tax code
  tab_sum_mwskz-mwskz = tab_ep-mwskz.
  tab_sum_mwskz-hwste = tab_ep-vat.
  COLLECT tab_sum_mwskz.

* collect totals per tax code new
  CLEAR: tab_sum_mwskz_new.
  tab_sum_mwskz_new-mwskz = tab_ep-mwskz.
  MOVE: tab_ep-taxed         TO tab_sum_mwskz_new-taxed.
  MOVE: tab_ep-not_taxed     TO tab_sum_mwskz_new-not_taxed."Note 454626
  MOVE: tab_ep-vat           TO tab_sum_mwskz_new-vat.
  MOVE: tab_ep-rnr_vat       TO tab_sum_mwskz_new-rnr_vat .
  MOVE: tab_ep-vat_percep    TO tab_sum_mwskz_new-vat_percep.
  MOVE: tab_ep-other_percep  TO tab_sum_mwskz_new-other_percep.
* MOVE: TAB_EP-EARN_PER      TO TAB_SUM_MWSKZ_NEW-OTHER_PERCEP."N.645449
  ADD:  tab_ep-earn_per      TO tab_sum_mwskz_new-other_percep. "N.685915
  MOVE: tab_ep-exemption     TO tab_sum_mwskz_new-exemption.

  IF NOT tab_ep-exports IS INITIAL.
    MOVE: tab_ep-exports       TO tab_sum_mwskz_new-exports.
  ELSE.
    MOVE: tab_ep-vat_intern    TO tab_sum_mwskz_new-exports.
  ENDIF.
  IF NOT tab_ep-percepnoc IS INITIAL.
    MOVE: tab_ep-percepnoc     TO tab_sum_mwskz_new-percepnoc.
  ELSE.
    MOVE: tab_ep-munic_per     TO tab_sum_mwskz_new-percepnoc.
  ENDIF.
* Note 1074703 End
  COLLECT tab_sum_mwskz_new.
* collect totals per regio
*  read table tab_regio with key
  READ TABLE tab_regio WITH KEY bukrs = tab_ep-bukrs
                                belnr = tab_ep-belnr
                                gjahr = tab_ep-gjahr
                                mwskz = tab_ep-mwskz
                                BINARY SEARCH.
  gv_tabix = sy-tabix.
  LOOP AT tab_regio FROM gv_tabix.
    IF    tab_regio-bukrs NE tab_ep-bukrs
       OR tab_regio-belnr NE tab_ep-belnr
       OR tab_regio-gjahr NE tab_ep-gjahr
       OR tab_regio-mwskz NE tab_ep-mwskz.
      EXIT.
    ENDIF.

    CLEAR: tab_sum_regio.

    MOVE: tab_regio-regio      TO tab_sum_regio-regio.
    MOVE: tab_ep-mwskz         TO tab_sum_regio-mwskz.
    MOVE: tab_ep-taxed         TO tab_sum_regio-taxed.
    MOVE: tab_ep-not_taxed     TO tab_sum_regio-not_taxed.  "Note 454626

    MOVE: tab_ep-vat           TO tab_sum_regio-vat.
    MOVE: tab_ep-rnr_vat       TO tab_sum_regio-rnr_vat .
    MOVE: tab_ep-vat_percep    TO tab_sum_regio-vat_percep.

*     Note 1083548 start.
    PERFORM read_j_1ataxid USING tab_001-kalsm
                                 tab_ep-ktosl.
    IF tab_j_1ataxid-regio EQ tab_regio-regio
       AND tab_j_1ataxid-ktosl EQ tab_regio-ktosl.
      MOVE: tab_ep-other_percep  TO tab_sum_regio-other_percep.
    ENDIF.
*     Note 1083548 end.

*   MOVE: TAB_EP-EARN_PER      TO TAB_SUM_REGIO-OTHER_PERCEP. "N. 645449
    ADD:  tab_ep-earn_per      TO tab_sum_regio-other_percep. "N. 685915
    MOVE: tab_ep-exemption     TO tab_sum_regio-exemption.

    IF NOT tab_ep-exports IS INITIAL.
      MOVE: tab_ep-exports       TO tab_sum_regio-exports.
    ELSE.
      MOVE: tab_ep-vat_intern    TO tab_sum_regio-exports.
    ENDIF.
    IF NOT tab_ep-percepnoc IS INITIAL.
      MOVE: tab_ep-percepnoc     TO tab_sum_regio-percepnoc.
    ELSE.
      MOVE: tab_ep-munic_per     TO tab_sum_regio-percepnoc.
    ENDIF.
* Note 1074703 End
    COLLECT tab_sum_regio.



*  endif.
  ENDLOOP.
* collect totals per tax rate
  tab_sum_rate-rate = tab_ep-rate.
  tab_sum_rate-hwste = tab_ep-vat.
  COLLECT tab_sum_rate.

* collect totals per official document type
  CLEAR: tab_sum_oftpt.
  MOVE-CORRESPONDING tab_ep TO tab_sum_oftpt.
  IF NOT par_magn IS INITIAL.  " magnetic output
* Note 1074703 Start
*    MOVE: tab_ep-vat_intern TO tab_sum_oftpt-exports,
**      TAB_EP-MUNIC_PER  TO TAB_SUM_OFTPT-PERCEPNOC,    " Note 685915
**      TAB_EP-EARN_PER   TO TAB_SUM_OFTPT-OTHER_PERCEP. " Note 685915
*       tab_ep-munic_per  TO tab_sum_oftpt-percepnoc.    " Note 685915
    ADD:  tab_ep-earn_per   TO tab_sum_oftpt-other_percep. " Note 685915
  ENDIF.
* Internal/Export tax
  IF NOT tab_ep-vat_intern IS INITIAL.
    MOVE tab_ep-vat_intern TO tab_sum_oftpt-exports.
  ELSEIF NOT tab_ep-exports IS INITIAL.
    MOVE tab_ep-exports  TO tab_sum_oftpt-exports.
  ENDIF.
* Municipal/Other perception
  IF NOT tab_ep-munic_per IS INITIAL.
    MOVE tab_ep-munic_per  TO tab_sum_oftpt-percepnoc.
  ELSEIF NOT tab_ep-percepnoc IS INITIAL.
    MOVE tab_ep-percepnoc  TO tab_sum_oftpt-percepnoc.
  ENDIF.
* Note 1074703 End
  COLLECT tab_sum_oftpt.

ENDFORM. " COLLECT_SUM_TABLES

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_REGIO
*&---------------------------------------------------------------------*
FORM print_tab_regio .
  DATA: lcl_comma  TYPE c VALUE ',',
        lcl_first  TYPE c,
        lcl_pos    TYPE i,
        lcl_length TYPE i,
        v_lines    TYPE i.

*  read table tab_regio with key
  DESCRIBE FIELD tab_regio-regio LENGTH lcl_length IN CHARACTER MODE.
  lcl_first = 'X'.
  lcl_pos = 1.
  READ TABLE tab_regio WITH KEY bukrs = tab_ep-bukrs
                                belnr = tab_ep-belnr
                                gjahr = tab_ep-gjahr
                                mwskz = tab_ep-mwskz
                            BINARY SEARCH.

  gv_tabix = sy-tabix.
  LOOP AT tab_regio FROM gv_tabix.
    IF    tab_regio-bukrs NE tab_ep-bukrs
       OR tab_regio-belnr NE tab_ep-belnr
       OR tab_regio-gjahr NE tab_ep-gjahr
       OR tab_regio-mwskz NE tab_ep-mwskz.
      EXIT.
    ENDIF.
    IF NOT lcl_first IS INITIAL.
      RESERVE 2 LINES.
      CLEAR: txt_zeile8.
      txt_zeile8 = sy-vline.
      WRITE:   TEXT-p01 TO txt_zeile8+1.
      listsize = listsize - 1 .
      txt_zeile8+listsize = sy-vline.
      listsize = listsize + 1 .
*      WRITE / txt_zeile8.

*[BEGIN] -- PM -- 20/08/09
      DESCRIBE TABLE t_alv LINES v_lines.
      DO.
        READ TABLE t_alv INDEX v_lines.
        IF t_alv-flag = 'X'.
          EXIT.
        ENDIF.
        v_lines = v_lines - 1.
      ENDDO.
      CLEAR: t_alv-total,
             line_total.
      APPEND t_alv.
*[ END ] -- PM -- 20/08/09

      PERFORM prepare_custom_rahmen.
      CLEAR: lcl_first.
    ENDIF.

    listsize = listsize - 3.
    IF lcl_pos GE listsize.
*      WRITE / txt_zeile8.

*[BEGIN] -- PM -- 20/08/09
      DESCRIBE TABLE t_alv LINES v_lines.
      DO.
        READ TABLE t_alv INDEX v_lines.
        IF t_alv-flag = 'X'.
          EXIT.
        ENDIF.
        v_lines = v_lines - 1.
      ENDDO.
      CLEAR: t_alv-total,
             line_total.

      APPEND t_alv.
*[ END ] -- PM -- 20/08/09

      CLEAR: txt_zeile8.
    ENDIF.
    listsize = listsize + 3.

    WRITE: tab_regio-regio TO txt_zeile8+lcl_pos.
    lcl_pos = lcl_pos + lcl_length.
    WRITE: lcl_comma       TO txt_zeile8+lcl_pos.
    lcl_pos = lcl_pos + 2.
    listsize = listsize - 1 .
    txt_zeile8+listsize = sy-vline.
    listsize = listsize + 1 .
*  endif.
  ENDLOOP.
  IF lcl_first IS INITIAL.             " At least one region was printed
    lcl_pos = lcl_pos - 2.
    WRITE lcl_first TO txt_zeile8+lcl_pos(1).
* No comma after last printed region. lcl_first is space now.
*    WRITE / txt_zeile8.

*[BEGIN] -- PM -- 20/08/09
    DESCRIBE TABLE t_alv LINES v_lines.
    DO.
      READ TABLE t_alv INDEX v_lines.
      IF t_alv-flag = 'X'.
        EXIT.
      ENDIF.
      v_lines = v_lines - 1.
    ENDDO.
    CLEAR: t_alv-total,
       line_total.
    APPEND t_alv.
*[ END ] -- PM -- 20/08/09

  ENDIF.

ENDFORM. " PRINT_TAB_REGIO

*&---------------------------------------------------------------------*
*&      Form  PRINT_CUSTOMS_DATA
*&---------------------------------------------------------------------*
FORM print_customs_data.

  DATA: v_lines TYPE i.

  PERFORM read_customs_data.
  IF NOT wa_custom-number IS INITIAL OR
     NOT wa_custom-code   IS INITIAL OR
     NOT wa_custom-destcd IS INITIAL OR
     NOT wa_custom-chksum IS INITIAL.
    RESERVE 2 LINES.
    CLEAR: txt_zeile8.
    txt_zeile8 = sy-vline.
    WRITE:   TEXT-p10 TO txt_zeile8+1.
    listsize = listsize - 1 .
    txt_zeile8+listsize = sy-vline.
    listsize = listsize + 1 .
*    WRITE / txt_zeile8.

*[BEGIN] -- PM -- 20/08/09
    DESCRIBE TABLE t_alv LINES v_lines.
    DO.
      READ TABLE t_alv INDEX v_lines.
      IF t_alv-flag = 'X'.
        EXIT.
      ENDIF.
      v_lines = v_lines - 1.
    ENDDO.
    APPEND t_alv.
*[ END ] -- PM -- 20/08/09

    PERFORM prepare_custom_rahmen.
    MOVE: TEXT-p11 TO hlp_epos9-text1.
    WRITE: wa_custom-code TO hlp_epos9-code.
    MOVE: TEXT-p14 TO hlp_epos9-text2.
    WRITE: wa_custom-number TO hlp_epos9-number.
    MOVE: TEXT-p16 TO hlp_epos9-text3.
    WRITE: wa_custom-destcd TO hlp_epos9-destcd.
    MOVE: TEXT-p17 TO hlp_epos9-text4.
    WRITE: wa_custom-chksum TO hlp_epos9-chksum.
    txt_zeile8 = hlp_epos9.
    listsize = listsize - 1 .
    txt_zeile8+listsize = sy-vline.
    listsize = listsize + 1 .
*    WRITE / txt_zeile8.

*[BEGIN] -- PM -- 20/08/09
    DESCRIBE TABLE t_alv LINES v_lines.
    DO.
      READ TABLE t_alv INDEX v_lines.
      IF t_alv-flag = 'X'.
        EXIT.
      ENDIF.
      v_lines = v_lines - 1.
    ENDDO.
    t_alv-mwskz = txt_zeile8+1(6).
    t_alv-rate = txt_zeile8+8(8).
    t_alv-taxed = txt_zeile8+17(18).
    t_alv-itaxed = t_alv-taxed.
    CLEAR: t_alv-total,
           line_total.
    APPEND t_alv.


  ENDIF.

ENDFORM. " PRINT_CUSTOMS_DATA
*&---------------------------------------------------------------------*
*&      Form  print_p_record
*&---------------------------------------------------------------------*
*       Print information from Perception record to screen
*----------------------------------------------------------------------*
*      -->p_perc_RECORD2  Perception record
*----------------------------------------------------------------------*
FORM print_p_record USING p_perc_record TYPE type_p_record2.
  DATA: lcl_regio(8) TYPE p,
        lcl_munic(8) TYPE p.
  CLEAR: hlp_epos11.
  hlp_epos11-v1 = sy-vline.
  IF NOT par_rahm IS INITIAL.
    hlp_epos11-v2 = hlp_epos11-v3 = hlp_epos11-v4 = sy-vline.
    hlp_epos11-v5 = hlp_epos11-v6 = hlp_epos11-v7 = sy-vline.
  ENDIF.
  MOVE TEXT-u27 TO hlp_epos11-regio_txt.
  MOVE p_perc_record-regio_code TO hlp_epos11-regio_code.
  lcl_regio = p_perc_record-perc_gi_amnt.
  WRITE lcl_regio TO hlp_epos11-regio_perc CURRENCY tab_001-waers.
  MOVE TEXT-u28 TO hlp_epos11-munic_txt.
  lcl_munic = p_perc_record-perc_mp_amnt.
  WRITE lcl_munic TO hlp_epos11-munic_perc CURRENCY tab_001-waers.
  WRITE p_perc_record-munic_code TO hlp_epos11-munic_code NO-ZERO.

  txt_zeile7 = hlp_epos11.
  listsize = listsize - 1 .
  txt_zeile7+listsize = sy-vline.
  listsize = listsize + 1 .
*  FORMAT: COLOR OFF,  INVERSE OFF.
*  WRITE / txt_zeile7.

*[BEGIN] -- PM -- 20/08/09
  t_alv-lineno = txt_zeile7+1(6).
  t_alv-budat = txt_zeile7+8(8).
  "t_alv-gjahr = t_alv-budat(4).
  t_alv-brnch = txt_zeile7+17(4).
  t_alv-belnr = txt_zeile7+22(10).
  SHIFT t_alv-belnr RIGHT DELETING TRAILING space.
  OVERLAY t_alv-belnr WITH '0000000000'.
  t_alv-koart = txt_zeile7+33(1).
  t_alv-hkont = txt_zeile7+35(10).
  t_alv-name1 = txt_zeile7+46(35).
  t_alv-stcdt = txt_zeile7+82(2).
  t_alv-stcd1 = txt_zeile7+85(16).
  t_alv-bldat = txt_zeile7+102(8).
  t_alv-xblnr = txt_zeile7+111(14).
  IF t_alv-xblnr IS NOT INITIAL.
    t_alv-xblnr+4(1) = '-'.
  ENDIF.
  t_alv-oftp_text = txt_zeile7+126(5).
  IF par_magn = space.
    t_alv-augdt = txt_zeile7+132(8).
    t_alv-augbl = txt_zeile7+141(10).
    t_alv-total = txt_zeile7+152(57).
  ELSE.
    t_alv-codmon = txt_zeile7+141(10).
    t_alv-tpcbio = txt_zeile7+152(15).
    t_alv-vaiva  = txt_zeile7+168(18).
    t_alv-total  = txt_zeile7+187(22).
  ENDIF.
  t_alv-flag = 'X'.
  CLEAR: t_alv-total,
       line_total.
  APPEND t_alv.
*[ END ] -- PM -- 20/08/09

ENDFORM. " print_p_record
*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_MWSKZ
*&---------------------------------------------------------------------*
*& print 2nd list : VAT-Totals which are grouped Taxcode
*& TABLE: TAB_SUM_MWSKZ
*----------------------------------------------------------------------*
FORM print_tab_sum_mwskz.
  DATA: up_length TYPE i.

* vat totals per tax code
  SORT: tab_sum_mwskz.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos4 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_mwskz.
    CHECK tab_sum_mwskz-hwste NE 0.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos4.
      hlp_epos4-v1 = sy-vline.
      WRITE: TEXT-t10 TO hlp_epos4+1 CENTERED.
      hlp_epos4-v4 = sy-vline.
*      WRITE: / hlp_epos4.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_totals USING '4'.
      hlp_epos4-mwskz          = TEXT-v03.
      hlp_epos4-description    = TEXT-v04.
      WRITE: TEXT-v05 TO hlp_epos4-hwste RIGHT-JUSTIFIED.
*      WRITE: / hlp_epos4.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos4-mwskz = tab_sum_mwskz-mwskz.
    PERFORM read_t007a USING  tab_001-kalsm tab_sum_mwskz-mwskz.
    hlp_epos4-description = tab_007a-text1.
    WRITE tab_sum_mwskz-hwste TO hlp_epos4-hwste CURRENCY tab_001-waers.
*    WRITE: / hlp_epos4.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_MWSKZ

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_MWSKZ_NEW
*&---------------------------------------------------------------------*
*& print 2ndlist : vat-totals which are grouped by tax code
*& table: tab_sum_mwskz_new
*----------------------------------------------------------------------*
FORM print_tab_sum_mwskz_new.
  DATA: up_length TYPE i.

  SORT: tab_sum_mwskz_new.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos3a LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_mwskz_new.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos3a.
      hlp_epos3a-v1 = sy-vline.
      WRITE: TEXT-t10 TO hlp_epos3a+1 CENTERED.
      hlp_epos3a-v13 = sy-vline.
*      WRITE: / hlp_epos3a.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_line.
      MOVE: TEXT-v03 TO hlp_epos3a-txt_sum .
      hlp_epos3a-description    = TEXT-v04.
      WRITE: p_col2 TO hlp_epos3a-taxed RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3a-not_taxed RIGHT-JUSTIFIED."Note 454626
      WRITE: p_col4 TO hlp_epos3a-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3a-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3a-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3a-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3a-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3a-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3a-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3a-line_total RIGHT-JUSTIFIED.

*      WRITE: / hlp_epos3a.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos3a-txt_sum   = tab_sum_mwskz_new-mwskz.
    PERFORM read_t007a USING  tab_001-kalsm tab_sum_mwskz_new-mwskz.
    hlp_epos3a-description = tab_007a-text1.
    WRITE tab_sum_mwskz_new-taxed TO
          hlp_epos3a-taxed CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-not_taxed TO                    "Note 454626
          hlp_epos3a-not_taxed CURRENCY tab_001-waers.      "Note 454626
    WRITE tab_sum_mwskz_new-vat TO
          hlp_epos3a-vat CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-rnr_vat TO
          hlp_epos3a-rnr_vat CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-vat_percep TO
           hlp_epos3a-vat_percep CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-other_percep TO
            hlp_epos3a-other_percep CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-exemption TO
            hlp_epos3a-exemption CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-exports TO
              hlp_epos3a-exports CURRENCY tab_001-waers.
    WRITE tab_sum_mwskz_new-percepnoc TO
            hlp_epos3a-percepnoc CURRENCY tab_001-waers.
    ADD: tab_sum_mwskz_new-taxed TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-not_taxed TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-vat TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-rnr_vat TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-vat_percep TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-exemption TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-exports TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-percepnoc TO tab_sum_mwskz_new-line_total,
         tab_sum_mwskz_new-other_percep TO tab_sum_mwskz_new-line_total.
    WRITE tab_sum_mwskz_new-line_total TO
           hlp_epos3a-line_total CURRENCY tab_001-waers.
*    WRITE: / hlp_epos3a.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_MWSKZ_NEW
*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_RATE
*&---------------------------------------------------------------------*
*& print 2nd list : VAT-Totals per vat rate
*& TABLE: TAB_SUM_RATE
*----------------------------------------------------------------------*
FORM print_tab_sum_rate.
  DATA: up_length TYPE i,
        rate(4)   TYPE p DECIMALS 2.

* vat totals per tax rate
  SORT: tab_sum_rate.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos5 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_rate.
    CHECK tab_sum_rate-hwste NE 0.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos5.
      hlp_epos5-v1 = sy-vline.
      WRITE: TEXT-t15 TO hlp_epos5+1 CENTERED.
      hlp_epos5-v3 = sy-vline.
*      WRITE: / hlp_epos5.
*      ULINE AT /(up_length).
      CLEAR: hlp_epos5.
      PERFORM prepare_rahmen_totals USING '5'.
      MOVE: TEXT-u11 TO hlp_epos5-satz .
      WRITE: TEXT-v05 TO hlp_epos5-hwste RIGHT-JUSTIFIED.
*      WRITE: / hlp_epos5.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    rate = tab_sum_rate-rate.
    WRITE: rate TO hlp_rate-rate .
    MOVE: '%' TO hlp_rate-percent.
    WRITE: hlp_rate TO hlp_epos5-satz.
    WRITE tab_sum_rate-hwste TO hlp_epos5-hwste CURRENCY tab_001-waers.
*    WRITE: / hlp_epos5.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_RATE
*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_OFTPT
*&---------------------------------------------------------------------*
*& print 2nd list : VAT-Totals per official document type
*& TABLE: TAB_SUM_OFTPT
*----------------------------------------------------------------------*
FORM print_tab_sum_oftpt.
  DATA: up_length TYPE i.

  SORT: tab_sum_oftpt.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos3 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_oftpt.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos3.
      hlp_epos3-v1 = sy-vline.
      WRITE: TEXT-t20 TO hlp_epos3+1 CENTERED.
*     HLP_EPOS3-V13 = SY-VLINE.                    " Note 685915
      hlp_epos3-v14 = sy-vline.                    " Note 685915
*      WRITE: / hlp_epos3.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_line.
      MOVE: TEXT-v06 TO hlp_epos3-txt_sum .
      WRITE: p_col2 TO hlp_epos3-taxed RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3-not_taxed RIGHT-JUSTIFIED.
      WRITE: p_col4 TO hlp_epos3-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3-line_total RIGHT-JUSTIFIED.

*      WRITE: / hlp_epos3.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos3-txt_sum    = tab_sum_oftpt-oftp_text.
    WRITE tab_sum_oftpt-taxed TO
          hlp_epos3-taxed CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-not_taxed TO
          hlp_epos3-not_taxed CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-vat TO
          hlp_epos3-vat CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-rnr_vat TO
          hlp_epos3-rnr_vat CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-vat_percep TO
           hlp_epos3-vat_percep CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-other_percep TO
            hlp_epos3-other_percep CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-exemption TO
          hlp_epos3-exemption CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-exports TO
            hlp_epos3-exports CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-percepnoc TO
          hlp_epos3-percepnoc CURRENCY tab_001-waers.
    WRITE tab_sum_oftpt-line_total TO
           hlp_epos3-line_total CURRENCY tab_001-waers.
*    WRITE: / hlp_epos3.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_OFTPT

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_ACCOUNT
*&---------------------------------------------------------------------*
*& print 2nd list : VAT-Totals per vat account
*& TABLE: TAB_SUM_ACCOUNT
*----------------------------------------------------------------------*
FORM print_tab_sum_account.
  DATA: up_length TYPE i.

  SORT: tab_sum_account.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos6 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_account.
    CHECK tab_sum_account-saldo NE 0.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos6.
      hlp_epos6-v1 = sy-vline.
      WRITE: TEXT-t25 TO hlp_epos6+1 CENTERED.
      hlp_epos6-v6 = sy-vline.
*      WRITE: / hlp_epos6.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_totals  USING '6'.
      hlp_epos6-hkont          = TEXT-v09.
      hlp_epos6-description    = TEXT-v04.
      WRITE: TEXT-v10 TO hlp_epos6-soll RIGHT-JUSTIFIED.
      WRITE: TEXT-v11 TO hlp_epos6-haben RIGHT-JUSTIFIED.
      WRITE: TEXT-v12 TO hlp_epos6-saldo RIGHT-JUSTIFIED.
*      WRITE: / hlp_epos6.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    WRITE: tab_sum_account-hkont TO hlp_epos6-hkont NO-ZERO.
* read hkont description
    CLEAR skat.
    CALL FUNCTION 'READ_HAUPTBUCH_TEXT'
      EXPORTING
        sprache        = sy-langu
        kontenplan     = tab_001-ktopl
        sachkonto      = tab_sum_account-hkont
      IMPORTING
        text_wa        = skat
      EXCEPTIONS
        text_not_found = 1.
    IF sy-subrc EQ 0.
      hlp_epos6-description = skat-txt20.
    ENDIF.
*
    WRITE tab_sum_account-soll TO hlp_epos6-soll
                           CURRENCY tab_001-waers.
    WRITE tab_sum_account-haben TO hlp_epos6-haben
                           CURRENCY tab_001-waers.
    WRITE tab_sum_account-saldo TO hlp_epos6-saldo
                           CURRENCY tab_001-waers.
*    WRITE: / hlp_epos6.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_ACCOUNT

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_OTHERS
*&---------------------------------------------------------------------*
*& print 2nd list : VAT-Totals which are grouped as others
*& TABLE: TAB_SUM_OTHERS
*----------------------------------------------------------------------*
FORM print_tab_sum_others.
  DATA: up_length TYPE i.
  DATA: hlp_kappl LIKE t685t-kappl.

* vat totals others
  SORT: tab_sum_others.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos4 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_others.
    CHECK tab_sum_others-hwste NE 0.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos4.
      hlp_epos4-v1 = sy-vline.
      WRITE: TEXT-t30 TO hlp_epos4+1 CENTERED.
      hlp_epos4-v4 = sy-vline.
*      WRITE: / hlp_epos4.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_totals USING '4'.
      hlp_epos4-mwskz          = TEXT-v03.
      hlp_epos4-description    = TEXT-v04.
      MOVE: TEXT-v05 TO hlp_epos4-hwste.
*      WRITE: / hlp_epos4.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos4-mwskz = tab_sum_others-mwskz.
* read description
    CLEAR t685t.
    IF tab_sum_others-flg_is_beleg = 'S'.             "SD document
      hlp_kappl = 'V'.
    ELSE.
      hlp_kappl = 'TX'.
    ENDIF.

* Event_008
*    <BEGIN OF ENHANCEMENT>
*    CALL FUNCTION '/XXARISM/VAT_BOOK_EVENT_008'
*         EXPORTING
*              I_BUKRS        = EP-BUKRS
*         CHANGING
*              E_HLP_KAPPL    = HLP_KAPPL.
*    <END OF ENHANCEMENT>

    IF tab_sum_others-kschl NE '!EXP'.
      SELECT SINGLE * FROM t685t WHERE spras = sy-langu
                                 AND   kvewe = 'A'
                                 AND   kappl = hlp_kappl
                                 AND   kschl = tab_sum_others-kschl.
      hlp_epos4-description = t685t-vtext.
    ELSE.
      hlp_epos4-description = p_sdexp.
    ENDIF.
    WRITE tab_sum_others-hwste TO hlp_epos4-hwste
              CURRENCY tab_001-waers.
*    WRITE: / hlp_epos4.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_OTHERS

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_SUM_REGIO
*&---------------------------------------------------------------------*
*& print 2nd list : VAT-Totals which are grouped by region
*& TABLE: TAB_SUM_REGIO
*----------------------------------------------------------------------*
FORM print_tab_sum_regio.
  DATA: up_length TYPE i.

  SORT: tab_sum_regio.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos3b LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_sum_regio.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos3b.
      hlp_epos3b-v0 = sy-vline.
      WRITE: TEXT-t11 TO hlp_epos3b+1 CENTERED.
      hlp_epos3b-v13 = sy-vline.
*      WRITE: / hlp_epos3b.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_regio.
      MOVE: TEXT-p01 TO hlp_epos3b-regio .
      MOVE: TEXT-u10 TO hlp_epos3b-txt_sum .
      hlp_epos3b-description    = TEXT-v04.
      WRITE: p_col2 TO hlp_epos3b-taxed RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3b-not_taxed RIGHT-JUSTIFIED."Note 454626
      WRITE: p_col4 TO hlp_epos3b-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3b-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3b-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3b-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3b-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3b-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3b-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3b-line_total RIGHT-JUSTIFIED.

*      WRITE: / hlp_epos3b.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos3b-regio   = tab_sum_regio-regio.
    hlp_epos3b-txt_sum   = tab_sum_regio-mwskz.
    PERFORM read_t007a USING  tab_001-kalsm tab_sum_regio-mwskz.
    hlp_epos3b-description = tab_007a-text1.
    WRITE tab_sum_regio-taxed TO
          hlp_epos3b-taxed CURRENCY tab_001-waers.
    WRITE tab_sum_regio-not_taxed TO                        "Note 454626
          hlp_epos3b-not_taxed CURRENCY tab_001-waers.      "Note 454626
    WRITE tab_sum_regio-vat TO
          hlp_epos3b-vat CURRENCY tab_001-waers.
    WRITE tab_sum_regio-rnr_vat TO
          hlp_epos3b-rnr_vat CURRENCY tab_001-waers.
    WRITE tab_sum_regio-vat_percep TO
           hlp_epos3b-vat_percep CURRENCY tab_001-waers.
    WRITE tab_sum_regio-other_percep TO
            hlp_epos3b-other_percep CURRENCY tab_001-waers.
    WRITE tab_sum_regio-exemption TO
            hlp_epos3b-exemption CURRENCY tab_001-waers.
    WRITE tab_sum_regio-exports TO
              hlp_epos3b-exports CURRENCY tab_001-waers.
    WRITE tab_sum_regio-percepnoc TO
            hlp_epos3b-percepnoc CURRENCY tab_001-waers.
    ADD: tab_sum_regio-taxed TO tab_sum_regio-line_total,
         tab_sum_regio-not_taxed TO tab_sum_regio-line_total,
         tab_sum_regio-vat TO tab_sum_regio-line_total,
         tab_sum_regio-rnr_vat TO tab_sum_regio-line_total,
         tab_sum_regio-vat_percep TO tab_sum_regio-line_total,
         tab_sum_regio-exemption TO tab_sum_regio-line_total,
         tab_sum_regio-exports TO tab_sum_regio-line_total,
         tab_sum_regio-percepnoc TO tab_sum_regio-line_total,
         tab_sum_regio-other_percep TO tab_sum_regio-line_total.
    WRITE tab_sum_regio-line_total TO
           hlp_epos3b-line_total CURRENCY tab_001-waers.
*    WRITE: / hlp_epos3b.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_SUM_REGIO

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_BRANCH
*&---------------------------------------------------------------------*
*&      For magnetic output only: Print number of records per branch
*&      OBOSOLETE: Not needed after Res. 1361/2002 (see note 597755)
*----------------------------------------------------------------------*
FORM print_tab_branch.
  DATA: up_length TYPE i.
  SORT: tab_sum_regio.
  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos10 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_branch.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos10.
      hlp_epos10-v1 = sy-vline.
      WRITE: TEXT-t40 TO hlp_epos10+1 CENTERED.
      hlp_epos10-v3 = sy-vline.
*      WRITE: / hlp_epos10.
*      ULINE AT /(up_length).
      CLEAR: hlp_epos10.
      hlp_epos10-v1 = hlp_epos10-v2 = hlp_epos10-v3 = sy-vline.
      WRITE: TEXT-t41 TO hlp_epos10-branch  LEFT-JUSTIFIED.
      WRITE: TEXT-t42 TO hlp_epos10-no_recs LEFT-JUSTIFIED.
*      WRITE: / hlp_epos10.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    WRITE: tab_branch-branch  TO hlp_epos10-branch RIGHT-JUSTIFIED.
    WRITE: tab_branch-no_recs TO hlp_epos10-no_recs NO-ZERO.
*    WRITE: / hlp_epos10.
*    ULINE AT /(up_length).
  ENDLOOP.
* no_of_rec contains total number of records
  FORMAT COLOR 1 INTENSIFIED.
  RESERVE 2 LINES.
  WRITE: TEXT-t43 TO hlp_epos10+1(30).
  WRITE: no_of_rec TO hlp_epos10+31(12) RIGHT-JUSTIFIED.
*  WRITE: / hlp_epos10.
*  ULINE AT /(up_length).
ENDFORM. " PRINT_TAB_BRANCH

*&---------------------------------------------------------------------*
*&      Form  INSERT_BELNR_TO_LOG
*&---------------------------------------------------------------------*
*       Insert document number to log
*       Only one document number as example per log entry
*----------------------------------------------------------------------*
*      -->P_TAB_LOG_ENTRY1_NAME  Table for which log entry is caused
*      -->P_TAB_LOG_ENTRY1_KEY   Table key for which log entry is caused
*      -->P_TAB_LOG_ENTRY1_BELNR Example for doc. number
*      -->P_BELNR                No example doc yet? -> Take this one
*----------------------------------------------------------------------*
FORM insert_belnr_to_log CHANGING
                      p_tab_log_entry1_name  LIKE tab_log_entry1-name
                      p_tab_log_entry1_key   LIKE tab_log_entry1-key
                      p_tab_log_entry1_belnr LIKE bkpf-belnr
                      p_belnr                LIKE bkpf-belnr.
  READ TABLE tab_log_entry10 WITH KEY name = p_tab_log_entry1_name
                                      key  = p_tab_log_entry1_key.
  IF sy-subrc = 0.
    IF tab_log_entry10-belnr IS INITIAL.
      p_tab_log_entry1_belnr = p_belnr.
    ELSE.
      p_tab_log_entry1_belnr = tab_log_entry10-belnr.
    ENDIF.
  ELSE.
    READ TABLE tab_log_entry1 WITH KEY name = p_tab_log_entry1_name
                                       key  = p_tab_log_entry1_key.
    IF sy-subrc = 0.
      IF tab_log_entry1-belnr IS INITIAL.
        p_tab_log_entry1_belnr = p_belnr.
      ENDIF.
    ELSE.
      p_tab_log_entry1_belnr = p_belnr.
    ENDIF.
  ENDIF.
ENDFORM. " INSERT_BELNR_TO_LOG

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_LOG_ENTRY10
*&---------------------------------------------------------------------*
*& print : All table entries which have not been found.
*& TABLE: TAB_LOG_ENTRY1
*----------------------------------------------------------------------*
FORM print_tab_log_entry10.
  DATA: up_length TYPE i.

  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos7 LENGTH up_length IN CHARACTER MODE.

  SORT: tab_log_entry10.
  LOOP AT tab_log_entry10.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos7.
      hlp_epos7-v1 = sy-vline.
      WRITE: TEXT-v94 TO hlp_epos7+1 CENTERED.
      hlp_epos7-v4 = sy-vline.
      hlp_epos7-v5 = sy-vline.
*      WRITE: / hlp_epos7.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_logs.
      hlp_epos7-name           = TEXT-v76.
      hlp_epos7-key            = TEXT-v14.
      hlp_epos7-description    = TEXT-v04.
      hlp_epos7-belnr          = TEXT-u04.
*      WRITE: / hlp_epos7.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    IF tab_log_entry10-belnr IS INITIAL. " Check if entry w/ doc num.
      " exists -> print only that
      LOOP AT tab_log_entry10 TRANSPORTING NO FIELDS
                              WHERE name = tab_log_entry10-name
                              AND   key  = tab_log_entry10-key
                              AND   belnr NE space.
      ENDLOOP.
      IF sy-subrc = 0.                 " Log entry exists w/ doc number
        CONTINUE.
      ENDIF.
    ENDIF.
    FORMAT COLOR 2.
    hlp_epos7-name = tab_log_entry10-name.
    hlp_epos7-key  = tab_log_entry10-key.
    hlp_epos7-belnr   = tab_log_entry10-belnr.
    CLEAR: hlp_epos7-description.
* description
    AT NEW name.
      CASE tab_log_entry10-name.
        WHEN 'J_1ATAXID'. hlp_epos7-description = TEXT-l12.
        WHEN 'J_1AOTDETR'. hlp_epos7-description = TEXT-l13.
        WHEN 'J_1AOFTPT'. hlp_epos7-description = TEXT-l16.
        WHEN 'J_1ARZTX'.  hlp_epos7-description = TEXT-l15.
        WHEN 'TCURC'.     hlp_epos7-description = TEXT-l14.
        WHEN 'T001Z'.     hlp_epos7-description = TEXT-l10.
        WHEN 'T007A'.     hlp_epos7-description = TEXT-l11.
        WHEN 'T007S'.     hlp_epos7-description = TEXT-l11.
        WHEN 'VBPA3'.
          hlp_epos7-description = TEXT-l17.
      ENDCASE.
    ENDAT.

*    WRITE: / hlp_epos7.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_LOG_ENTRY10

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_LOG_ENTRY20
*&---------------------------------------------------------------------*
*& print : other log entries
*& TABLE: TAB_LOG_ENTRY20
*----------------------------------------------------------------------*
FORM print_tab_log_entry20.
  DATA: up_length TYPE i.

  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos8 LENGTH up_length IN CHARACTER MODE.

  SORT: tab_log_entry20.
  LOOP AT tab_log_entry20.
    RESERVE 2 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 8 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos8.
      hlp_epos8-v1 = sy-vline.
      WRITE: TEXT-v96 TO hlp_epos8+1 CENTERED.
      hlp_epos8-v5 = sy-vline.
*      WRITE: / hlp_epos8.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_logs.
      hlp_epos8-belnr         = TEXT-u03.
      hlp_epos8-blart         = TEXT-v15.
      hlp_epos8-budat         = TEXT-u02.
      hlp_epos8-description   = TEXT-v04.
      WRITE: / hlp_epos8.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 2.
    hlp_epos8-belnr         = tab_log_entry20-belnr.
    hlp_epos8-blart         = tab_log_entry20-blart.
    WRITE: tab_log_entry20-budat TO hlp_epos8-budat DD/MM/YY NO-ZERO.
    hlp_epos8-description   = tab_log_entry20-description.
*    WRITE: / hlp_epos8.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_LOG_ENTRY20
*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_HISTORY
*&---------------------------------------------------------------------*
*& print history table
*& TABLE: TAB_HISTORY
*& flg_print_tab_history only controls text for title
*& flg_print_tab_history 0 = list deleted entries
*& flg_print_tab_history 1 = list updated entries
*& flg_print_tab_history 2 = list entries without update
*&---------------------------------------------------------------------*
FORM print_tab_history.
  DATA: up_length TYPE i.

  IF par_sort EQ 2.
    SORT tab_history BY
      j_1adrdt   DESCENDING.
  ELSE.
    SORT tab_history BY
      j_1adrdt .                       " Posting date
  ENDIF.

  flg_new_page = 'X'.
  DESCRIBE FIELD hlp_epos3 LENGTH up_length IN CHARACTER MODE.

  LOOP AT tab_history.
    RESERVE 3 LINES.
    IF NOT flg_new_page IS INITIAL.
      FORMAT COLOR 1 INTENSIFIED.
      RESERVE 9 LINES.
      SKIP 1.
*      ULINE AT (up_length).
      CLEAR: hlp_epos3.
      hlp_epos3-v1 = sy-vline.
      IF flg_print_tab_history  EQ '0'.
        WRITE: TEXT-v93 TO hlp_epos3+1 CENTERED.
      ENDIF.
      IF flg_print_tab_history  EQ '1'.
        WRITE: TEXT-v91 TO hlp_epos3+1 CENTERED.
      ENDIF.
      IF flg_print_tab_history  EQ '2'.
        WRITE: TEXT-v92 TO hlp_epos3+1 CENTERED.
      ENDIF.
      hlp_epos3-v13 = sy-vline.
*      WRITE: / hlp_epos3.
*      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_line.
      MOVE: TEXT-h03 TO hlp_epos3-txt_sum.
      MOVE: TEXT-v50 TO hlp_epos3-taxed.
      MOVE: TEXT-v53 TO hlp_epos3-not_taxed .
      MOVE: TEXT-v54 TO hlp_epos3-vat.
      MOVE: TEXT-u02 TO hlp_epos3-rnr_vat.
      MOVE: TEXT-v51 TO hlp_epos3-vat_percep .
      MOVE: TEXT-v52 TO hlp_epos3-other_percep.
      CLEAR: hlp_epos3-exemption .
      CLEAR: hlp_epos3-exports .
      CLEAR: hlp_epos3-percepnoc .
      CLEAR: hlp_epos3-line_total .
*      WRITE: hlp_epos3.
* 2nd line header
      CLEAR: hlp_epos3-txt_sum.
      WRITE: p_col2 TO hlp_epos3-taxed RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3-not_taxed RIGHT-JUSTIFIED.
      WRITE: p_col4 TO hlp_epos3-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3-line_total RIGHT-JUSTIFIED.
*      WRITE: / hlp_epos3.
*      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos3-txt_sum = tab_history-bukrs.
    hlp_epos3-taxed  =  tab_history-repid.
    hlp_epos3-not_taxed  = tab_history-j_1adrver.
    hlp_epos3-vat = tab_history-j_1aproc.
    WRITE: tab_history-j_1adrdt TO hlp_epos3-rnr_vat DD/MM/YY NO-ZERO.
    WRITE: tab_history-j_1alineno(10)  TO hlp_epos3-vat_percep.
    WRITE: tab_history-j_1apageno(5) TO hlp_epos3-other_percep.
    CLEAR: hlp_epos3-exemption.
    CLEAR: hlp_epos3-exports.
    CLEAR: hlp_epos3-percepnoc.
    CLEAR: hlp_epos3-line_total.
*    WRITE: / hlp_epos3.
* 2nd line with amounts
    CLEAR: hlp_epos3-txt_sum .
    WRITE tab_history-j_1aamnt01 TO
          hlp_epos3-taxed CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt02 TO
          hlp_epos3-not_taxed CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt03 TO
          hlp_epos3-vat CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt04 TO
          hlp_epos3-rnr_vat CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt05 TO
           hlp_epos3-vat_percep CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt06 TO
            hlp_epos3-other_percep CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt07 TO
          hlp_epos3-exemption CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt08 TO
           hlp_epos3-line_total CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt09 TO
           hlp_epos3-exports CURRENCY tab_001-waers.
    WRITE tab_history-j_1aamnt10 TO
           hlp_epos3-percepnoc CURRENCY tab_001-waers.
*    WRITE: / hlp_epos3.
*    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM. " PRINT_TAB_HISTORY

*----------------------------------------------------------------------*
* Subroutines for Preparations of List Output
*----------------------------------------------------------------------*
* PREPARE_CUSTOM_RAHMEN
* PREPARE_LIST_HEADER
* PREPARE_PAGE_TOTAL
* PREPARE_RAHMEN
* PREPARE_RAHMEN_SUM_LINE
* PREPARE_RAHMEN_TOTALS
* PREPARE_RAHMEN_LOGS
* PREPARE_RAHMEN_SUM_REGIO
* PREPARE_MAGNETIC_OUTPUT
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PREPARE_CUSTOM_RAHMEN
*&---------------------------------------------------------------------*
*       Put sy-vline (vertical line) to positions between columns
*----------------------------------------------------------------------*
FORM prepare_custom_rahmen.
  CLEAR: hlp_epos9.
  hlp_epos9-v1 =  hlp_epos9-v5 = sy-vline.
  IF NOT par_rahm IS INITIAL.
    hlp_epos9-v2 = hlp_epos9-v3 = hlp_epos9-v4 = sy-vline.
  ENDIF.
ENDFORM. " PREPARE_CUSTOM_RAHMEN

*&---------------------------------------------------------------------*
*&      Form  PREPARE_LIST_HEADER
*&---------------------------------------------------------------------*
*& Aufbau der List-Ueberschrift
*&---------------------------------------------------------------------*
* Parameter LIST_NUMBER :  Determines sort of list:
* 1 - Document list
* 2 - VAT totals
* 3 - tab_log_entry1
* 4 - tab_history
*&---------------------------------------------------------------------*
FORM prepare_list_header.
  DATA:
    up_length(2) TYPE p,
    up_offset(2) TYPE p.

  CLEAR:
    txt_zeile1, txt_zeile2, txt_zeile3,
    txt_zeile4, txt_zeile5, txt_zeile6.

* 1st line
  txt_zeile1 = sy-vline.
  IF NOT par_vers IS INITIAL.
    WRITE: tab_j_1adrvert-text30 TO txt_zeile1+1 CENTERED.
  ELSE.
*    write: tab_j_1adrvert-text30 to txt_zeile1+1 centered.
  ENDIF.
  DESCRIBE FIELD wa_history-j_1apageno LENGTH up_length
                                       IN CHARACTER MODE.
  up_length = listsize - up_length .
  up_length = up_length - 1 .
  WRITE: wa_history-j_1apageno TO txt_zeile1+up_length NO-ZERO.
* 2nd line
  CLEAR: up_offset.
  txt_zeile2 = sy-vline.
  WRITE: TEXT-h01 TO txt_zeile2+1.
  DESCRIBE FIELD TEXT-h01 LENGTH up_length IN CHARACTER MODE.
  up_offset = up_length + 1.
  WRITE: tab_001-name1 TO txt_zeile2+up_offset.
  IF list_number EQ 2.                 " vat totals
    WRITE: TEXT-v01 TO txt_zeile2+75.
  ELSEIF list_number EQ 3.             " log_entry1
    WRITE: TEXT-v02 TO txt_zeile2+75.
  ELSEIF list_number EQ 4.             " history tabelle
    WRITE: TEXT-v55 TO txt_zeile2+75.
  ENDIF.
  DESCRIBE FIELD hlp_repid LENGTH up_length IN CHARACTER MODE.
  up_offset = listsize - up_length.
  up_offset = up_offset - 1.
  WRITE: history_repid TO hlp_repid-repid RIGHT-JUSTIFIED.
  WRITE: hlp_repid TO txt_zeile2+up_offset.

* 3rd line
  txt_zeile3 = sy-vline.
  WRITE: TEXT-h03 TO txt_zeile3+1.
  DESCRIBE FIELD TEXT-h03 LENGTH up_length IN CHARACTER MODE.
  up_length = up_length + 1.
  WRITE: ep-bukrs TO txt_zeile3+up_length.
* 4. line
  txt_zeile4 = sy-vline.
  WRITE: TEXT-h02 TO txt_zeile4+1.
  DESCRIBE FIELD TEXT-h02 LENGTH up_length IN CHARACTER MODE.
  up_length = up_length + 1.
  IF tab_001-stcdt = '80'.
    WRITE: tab_001-stcd1 TO txt_zeile4+up_length
                       USING EDIT MASK '__-________-_'.
  ELSE.
    WRITE: tab_001-stcd1 TO txt_zeile4+up_length.
  ENDIF.
  IF list_number EQ 1.                 " document items
    WRITE: TEXT-h04 TO txt_zeile4+75.
    DESCRIBE FIELD TEXT-h04 LENGTH up_length IN CHARACTER MODE.
    WRITE: ep-monat TO txt_gjahr NO-GAP.
    WRITE: '.' TO txt_gjahr+2 NO-GAP.
    WRITE: ep-gjahr TO txt_gjahr+3 NO-GAP.
    up_length = up_length + 76.
    WRITE: txt_gjahr TO txt_zeile4+up_length.
  ENDIF.

  PERFORM prepare_rahmen.
* First list header line
  hlp_epos1-lineno    = TEXT-u01.
* Magnetic output: Sort by document date instead of posting date
  IF par_magn IS INITIAL.
    hlp_epos1-budat     = TEXT-u02. " Post. date
  ELSE.
    hlp_epos1-budat     = TEXT-u06. " Doc. date
  ENDIF.
  hlp_epos1-brnch     = TEXT-u20.
  hlp_epos1-belnr     = TEXT-u03.
  hlp_epos1-koart     = TEXT-v65.
  hlp_epos1-hkont     = TEXT-v66.
  hlp_epos1-name1(25) = TEXT-v67.
  hlp_epos1-stcdt     = TEXT-v68.
  hlp_epos1-stcd1     = TEXT-v69.
  IF par_magn IS INITIAL.
    hlp_epos1-bldat     = TEXT-u06. " Doc. date
  ELSE.
    hlp_epos1-bldat     = TEXT-u02. " Post. date
  ENDIF.
  hlp_epos1-xblnr     = TEXT-v71.
  hlp_epos1-oftp_text = TEXT-v72.
  IF NOT par_magn IS INITIAL.
*    HLP_EPOS1-AUGDT     = TEXT-V84.  " Note 636750
    hlp_epos1-augbl     = TEXT-v85.
    WRITE: TEXT-v86 TO hlp_epos1-total(15)   LEFT-JUSTIFIED.
    WRITE: TEXT-u24 TO hlp_epos1-total+16(18) LEFT-JUSTIFIED.
    WRITE: TEXT-v75 TO hlp_epos1-total+35(22) RIGHT-JUSTIFIED.
    WRITE: TEXT-u29 TO hlp_epos1-cai LEFT-JUSTIFIED.
  ELSE.
    hlp_epos1-augdt     = TEXT-v73.
    hlp_epos1-augbl     = TEXT-v74.
    WRITE: TEXT-v75 TO hlp_epos1-total RIGHT-JUSTIFIED.
  ENDIF.

* Second header line
  WRITE: TEXT-u10 TO hlp_epos2-mwskz RIGHT-JUSTIFIED.
  WRITE: TEXT-u11 TO hlp_epos2-rate RIGHT-JUSTIFIED.
  IF NOT par_comp IS INITIAL.
    WRITE: p_col2 TO hlp_epos2-taxed RIGHT-JUSTIFIED.
  ELSE.
    WRITE: p_col2 TO hlp_epos2-taxed RIGHT-JUSTIFIED.
  ENDIF.
  WRITE: p_col3 TO hlp_epos2-not_taxed RIGHT-JUSTIFIED.
  WRITE: p_col4 TO hlp_epos2-vat RIGHT-JUSTIFIED.
  WRITE: p_col5 TO hlp_epos2-rnr_vat RIGHT-JUSTIFIED.
  WRITE: p_col6 TO hlp_epos2-vat_percep RIGHT-JUSTIFIED.
  WRITE: p_col7 TO hlp_epos2-other_percep RIGHT-JUSTIFIED.
  WRITE: p_col8 TO hlp_epos2-exemption RIGHT-JUSTIFIED.
  WRITE: p_col09 TO hlp_epos2-exports RIGHT-JUSTIFIED.
  WRITE: p_col10 TO hlp_epos2-percepnoc RIGHT-JUSTIFIED.
  WRITE: p_col11 TO hlp_epos2-line_total RIGHT-JUSTIFIED.
  IF NOT par_magn IS INITIAL.
    WRITE: TEXT-u23 TO hlp_epos2-exempt   LEFT-JUSTIFIED.
    IF NOT par_vers IS INITIAL.
      IF tab_j_1adrver-j_1aproc = 1 OR
         tab_j_1adrver-j_1aproc = 3.     " input / purchase
        WRITE: TEXT-u33 TO hlp_epos2-earn_per RIGHT-JUSTIFIED.
      ELSE.
        CLEAR: hlp_epos2-earn_per.
      ENDIF.
    ENDIF.
  ENDIF.

  txt_zeile5 = hlp_epos1.
  txt_zeile6 = hlp_epos2.

  listsize = listsize - 1.
  txt_zeile1+listsize = sy-vline.
  txt_zeile2+listsize = sy-vline.
  txt_zeile3+listsize = sy-vline.
  txt_zeile4+listsize = sy-vline.
  txt_zeile5+listsize = sy-vline.
  txt_zeile6+listsize = sy-vline.
  listsize = listsize + 1 .

ENDFORM. " PREPARE_LIST_HEADER
*&---------------------------------------------------------------------*
*&      Form  PREPARE_PAGE_TOTAL
*&---------------------------------------------------------------------*
* Build page-totals ( Sums at top and bottom of page)
*----------------------------------------------------------------------*
* Parameter LIST_NUMBER :  Determines list:
* 1 - Document list
* Parameter FLG_PAGE_END:  1 = Sum for end of page / 0 = begin of page
*----------------------------------------------------------------------*
FORM prepare_page_total USING flg_page_end.
  CLEAR:
    txt_zeile9.
  PERFORM prepare_rahmen_sum_line.
  IF flg_page_end EQ  '1'.
    hlp_epos3-txt_sum    = TEXT-b01.   " list end
  ELSE.
    hlp_epos3-txt_sum    = TEXT-h40.   " list begin
  ENDIF.
  WRITE wa_history_over-j_1aamnt01 TO
        hlp_epos3-taxed CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt02 TO
        hlp_epos3-not_taxed CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt03 TO
        hlp_epos3-vat CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt04 TO
        hlp_epos3-rnr_vat CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt05 TO
         hlp_epos3-vat_percep CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt06 TO
          hlp_epos3-other_percep CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt07 TO
        hlp_epos3-exemption CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt08 TO
         hlp_epos3-line_total CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt09 TO
           hlp_epos3-exports CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt10 TO
           hlp_epos3-percepnoc CURRENCY tab_001-waers.
  WRITE wa_history_over-j_1aamnt11 TO                   " Note 685915
           hlp_epos3-earn_per CURRENCY tab_001-waers.
  txt_zeile9 = hlp_epos3.
  listsize = listsize - 1 .
  txt_zeile9+listsize = sy-vline.
  listsize = listsize + 1 .

ENDFORM. " PREPARE_PAGE_TOTAL

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN
*&---------------------------------------------------------------------*
*       Move sy-vline to help table
*----------------------------------------------------------------------*
FORM prepare_rahmen.
  CLEAR: hlp_epos1, hlp_epos2, hlp_epos1s.
  hlp_epos1-v1 =  hlp_epos1-v16 = sy-vline.
  hlp_epos1s-v1 =  hlp_epos1s-v10 = sy-vline.
  hlp_epos2-v1 = hlp_epos2-v13 = sy-vline.
  IF NOT par_rahm IS INITIAL.
    hlp_epos1-v2 = hlp_epos1-v3 = hlp_epos1-v4 =
    hlp_epos1-v5 = hlp_epos1-v6 = hlp_epos1-v7 = hlp_epos1-v8 =
    hlp_epos1-v9 = hlp_epos1-v10 = hlp_epos1-v11 = hlp_epos1-v12 =
    hlp_epos1-v13 = hlp_epos1-v14 = hlp_epos1-v15 = sy-vline.
    IF NOT par_magn IS INITIAL.      " list format for magnetic output
      hlp_epos1-total+15(1) = hlp_epos1-total+34(1) = sy-vline.
    ENDIF.
    hlp_epos2-v2 = hlp_epos2-v3 = hlp_epos2-v4 =
    hlp_epos2-v5 = hlp_epos2-v6 = hlp_epos2-v7 = hlp_epos2-v8 =
    hlp_epos2-v9 = hlp_epos2-v10 = sy-vline.
    hlp_epos2-v11 = hlp_epos2-v12 = sy-vline.
    hlp_epos1s-v2 = hlp_epos1s-v3 = hlp_epos1s-v4 =
    hlp_epos1s-v5 = hlp_epos1s-v6 = hlp_epos1s-v7 = sy-vline.
  ENDIF.

ENDFORM. " PREPARE_RAHMEN

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN_SUM_LINE
*&---------------------------------------------------------------------*
FORM prepare_rahmen_sum_line.
  CLEAR: hlp_epos3,hlp_epos3a.
* HLP_EPOS3-V1 =  HLP_EPOS3-V13 = SY-VLINE.            " Note 685915
  hlp_epos3-v1 =  hlp_epos3-v14 = sy-vline.            " Note 685915
  hlp_epos3a-v1 =  hlp_epos3a-v13 = sy-vline.
  IF NOT par_rahm IS INITIAL.
    hlp_epos3-v3 = hlp_epos3-v4 =
    hlp_epos3-v5 = hlp_epos3-v6 = hlp_epos3-v7 = hlp_epos3-v8 =
    hlp_epos3-v9 = hlp_epos3-v10 =
    hlp_epos3-v11 = hlp_epos3-v12 = sy-vline.
    hlp_epos3a-v2 = hlp_epos3a-v4 = hlp_epos3a-v5a =
    hlp_epos3a-v5 = hlp_epos3a-v6 = hlp_epos3a-v7 = hlp_epos3a-v8 =
    hlp_epos3a-v9 = hlp_epos3a-v10 =
    hlp_epos3-v11 = hlp_epos3-v12 =                    " Note 685915
    hlp_epos3-v13 = sy-vline.                          " Note 685915
*   HLP_EPOS3-V11 = HLP_EPOS3-V12 = SY-VLINE.          " Note 685915
  ENDIF.
ENDFORM. " PREPARE_RAHMEN_SUM_LINE

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN_TOTALS
*&---------------------------------------------------------------------*
FORM prepare_rahmen_totals USING number.
  CASE number.
    WHEN 4.
      CLEAR: hlp_epos4.
      hlp_epos4-v1 =  hlp_epos4-v4 = sy-vline.
    WHEN 5.
      CLEAR: hlp_epos5.
      hlp_epos5-v1 =  hlp_epos5-v3 = sy-vline.
    WHEN 6.
      CLEAR: hlp_epos6.
      hlp_epos6-v1 =  hlp_epos6-v6 = sy-vline.
  ENDCASE.

  IF NOT par_rahm IS INITIAL.
    CASE number.
      WHEN 4.
        hlp_epos4-v2 = hlp_epos4-v3 = sy-vline.
      WHEN 5.
        hlp_epos5-v2 = sy-vline.
      WHEN 6.
        hlp_epos6-v2 = hlp_epos6-v3 =
        hlp_epos6-v4 = hlp_epos6-v5 =  sy-vline.
    ENDCASE.
  ENDIF.

ENDFORM. " PREPARE_RAHMEN_TOTALS

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN_LOGS
*&---------------------------------------------------------------------*
FORM prepare_rahmen_logs.
  IF NOT par_rahm IS INITIAL.
    hlp_epos7-v1 = hlp_epos7-v2 = hlp_epos7-v3 =
    hlp_epos7-v4 = sy-vline.
    hlp_epos8-v1 = hlp_epos8-v2 = hlp_epos8-v3 = hlp_epos8-v4 =
    hlp_epos8-v5 = sy-vline.
  ENDIF.
ENDFORM. " PREPARE_RAHMEN_LOGS

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN_SUM_REGIO
*&---------------------------------------------------------------------*
FORM prepare_rahmen_sum_regio.
  CLEAR: hlp_epos3b.
  hlp_epos3b-v0 =  hlp_epos3b-v13 = sy-vline.
  IF NOT par_rahm IS INITIAL.
    hlp_epos3b-v1 =  hlp_epos3b-v2 = hlp_epos3b-v4 = hlp_epos3b-v5a =
    hlp_epos3b-v5 = hlp_epos3b-v6 = hlp_epos3b-v7 = hlp_epos3b-v8 =
    hlp_epos3b-v9 = hlp_epos3b-v10 =
    hlp_epos3b-v11 = hlp_epos3b-v12 = sy-vline.
  ENDIF.
ENDFORM. " PREPARE_RAHMEN_SUM_REGIO

*&---------------------------------------------------------------------*
*&      Form  PREPARE_MAGNETIC_OUTPUT
*&---------------------------------------------------------------------*
*       Prepare list and file output for former J_1AF106 functionality
*
*       Move amounts to fields following the mapping:
*       Other Perception: - If ET Perception move to Perception column
*                         - If GI Perception move to Not Taxed column
*       Exports:          - Move to Taxed column
*       Perc. not cat.:   - Move to Perception column
*
*       Fill records of type '1' for magnetic output
*----------------------------------------------------------------------*
FORM prepare_magnetic_output.
  ADD 1 TO rec_subsidiary.             " records per subsid.
  ADD 1 TO no_of_rec.                  " total number of records

  IF tab_j_1adrver-j_1aproc = '1' OR
     tab_j_1adrver-j_1aproc = '3'.     " input/purchase
    CLEAR i_record2.
    PERFORM fill_i_record2 CHANGING i_record2.
  ELSE.                                " output/sales
    CLEAR o_record2.
    PERFORM fill_o_record2 CHANGING o_record2.
  ENDIF.
ENDFORM. " PREPARE_MAGNETIC_OUTPUT
*----------------------------------------------------------------------*
* Subroutines for Filling Records of Magnetic Output
*----------------------------------------------------------------------*
* FILL_I_RECORD3   Purchase record of type '2'
* FILL_O_RECORD3   Sales record of type '2'
* FILL_I_RECORD2   Purchase record of type '1' input/purchase
* FILL_O_RECORD2   Sales record of type '1' output/sales
* FILL_P_RECORD    Perception record
* FILL_SUMS_I_RECORD3  Fill Purchase summary record (type 2)
* FILL_SUMS_O_RECORD3  Fill Sales summary record (type 2)
* TRANSFER_RECORD  Writes one record to the file
* FILL_CURR_INFO   Fill currency code for magnetic output
* FILL_EXCH_RATE   Fill exchange rate for magnetic output
* TRANSFER_LOCAL   Transfer file from application server to local file
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FILL_I_RECORD3
*&---------------------------------------------------------------------*
*       Fill Purchase record of type '2' for magnetic output
*----------------------------------------------------------------------*
FORM fill_i_record3 CHANGING p_i_record3 LIKE i_record3.

  p_i_record3-rectype          = '2'.
* BUPER is filled in EXTRACT_TAB_BELEG_TO_EP from BKPF-GJAHR and MONAT
  p_i_record3-period           = gf_rec3_buper(6). " yyyymm
  p_i_record3-no_records       = no_of_rec.
  p_i_record3-id_docu_no       = tab_001-stcd1.

* Amount fields have been filled into structure I_RECORD3_SUM
  p_i_record3-total_amnt       = i_record3_sum-total_amnt.
  p_i_record3-n_taxed_amnt     = i_record3_sum-n_taxed_amnt.
  p_i_record3-taxed_amnt       = i_record3_sum-taxed_amnt.
  p_i_record3-tax_amnt         = i_record3_sum-tax_amnt.
  p_i_record3-exempt_amnt      = i_record3_sum-exempt_amnt.
  p_i_record3-perce_amnt       = i_record3_sum-perce_amnt.
*  NOTE 728727 Starts
  p_i_record3-per_nat_amnt     = i_record3_sum-per_nat_amnt.
*  NOTE 728727 ends
  p_i_record3-perc_gi_amnt     = i_record3_sum-perc_gi_amnt.
  p_i_record3-perc_mp_amnt     = i_record3_sum-perc_mp_amnt.
  p_i_record3-int_tax_amnt     = i_record3_sum-int_tax_amnt.

* Note 1037893 Start
  DESCRIBE FIELD p_i_record3 LENGTH len IN CHARACTER MODE.
*  PERFORM transfer_record USING p_i_record3 369.
  PERFORM transfer_record USING p_i_record3 len.
* Note 1037893 End

ENDFORM. " FILL_I_RECORD3

*&---------------------------------------------------------------------*
*&      Form  FILL_O_RECORD3
*&---------------------------------------------------------------------*
*       Fill Purchase record of type '2' for magnetic output
*----------------------------------------------------------------------*
FORM fill_o_record3 CHANGING p_o_record3 LIKE o_record3.

  p_o_record3-rectype          = '2'.
* BUPER is filled in EXTRACT_TAB_BELEG_TO_EP from BKPF-GJAHR and MONAT
  p_o_record3-period           = gf_rec3_buper(6). " yyyymm
  p_o_record3-no_records       = no_of_rec.
  p_o_record3-id_docu_no       = tab_001-stcd1.
* Amount fields have been filled into structure O_RECORD3_SUM
  p_o_record3-total_amnt       = o_record3_sum-total_amnt.
  p_o_record3-n_taxed_amnt     = o_record3_sum-n_taxed_amnt.
  p_o_record3-taxed_amnt       = o_record3_sum-taxed_amnt.
  p_o_record3-tax_amnt         = o_record3_sum-tax_amnt.
  p_o_record3-exempt_amnt      = o_record3_sum-exempt_amnt.
  p_o_record3-surch_amnt       = o_record3_sum-surch_amnt.
  p_o_record3-perce_amnt       = o_record3_sum-perce_amnt.
  p_o_record3-perc_gi_amnt     = o_record3_sum-perc_gi_amnt.
  p_o_record3-perc_mp_amnt     = o_record3_sum-perc_mp_amnt.
  p_o_record3-int_tax_amnt     = o_record3_sum-int_tax_amnt.

* Note 1037893 Start
  DESCRIBE FIELD p_o_record3 LENGTH len IN CHARACTER MODE.
*  PERFORM transfer_record USING p_o_record3 375.
  PERFORM transfer_record USING p_o_record3 len.
* Note 1037893 End
ENDFORM. "

*&---------------------------------------------------------------------*
*&      Form  FILL_I_RECORD2
*&---------------------------------------------------------------------*
* Difference to 106 (therefore no factor 100 for amounts and for
* exchange rate no factor 10000 but 0.1 -> no fixed point arithmetic)
*----------------------------------------------------------------------*
*      <--P_I_RECORD2  text                                            *
*----------------------------------------------------------------------*
FORM fill_i_record2 CHANGING p_i_record2 LIKE i_record2.

  DATA: lcl_tfill      LIKE sy-tfill,
        lcl_cmp_exempt LIKE o_record2-total_amnt.  " Note 690007

  p_i_record2-rectype           = '1'.

  PERFORM read_customs_data.

  IF tab_ep-j_1aoftp = '14' AND
     NOT wa_custom-date IS INITIAL.
    p_i_record2-docu_date         = wa_custom-date.
  ELSE.
    p_i_record2-docu_date         = tab_ep-bldat.
  ENDIF.
  IF ep-j_1aoftp IS INITIAL.
    p_i_record2-docu_type = '00'.
  ELSE.
    p_i_record2-docu_type = tab_ep-j_1aoftp.
  ENDIF.
  p_i_record2-fisc_cont         = tab_ep-fisc_cont.
* Am besten globales Feld füllen, das mit in den Extrakt gegeben wird...
  p_i_record2-subsidiary        = tab_ep-xblnr(4).
  p_i_record2-docu_no           = tab_ep-xblnr+5(8).
  p_i_record2-post_date         = tab_ep-budat.  " yyyymmdd

* Achtung! Customs code jetzt nur noch sechsstellig
*  P_I_RECORD2-CUSTOMS_YEAR      = WA_CUSTOM-YEAR.
  p_i_record2-customs_code      = wa_custom-code.
  p_i_record2-customs_dest      = wa_custom-destcd.
  p_i_record2-customs_no        = wa_custom-number.
  p_i_record2-customs_check     = wa_custom-chksum.
* Customs date has format yyyymmdd
*  p_i_record2-customs_date      = wa_custom-date.
  p_i_record2-type_of_id        = tab_ep-stcdt.
  IF tab_ep-stcdt = '99'. " New as of Res. 1361/2002
    CLEAR: p_i_record2-id_docu_no.
    p_i_record2-name              = TEXT-u31.
  ELSE.
    p_i_record2-id_docu_no        = tab_ep-stcd1.
    p_i_record2-name              = tab_ep-name1.
  ENDIF.
  p_i_record2-total_amnt        = tab_ep-total.
  p_i_record2-n_taxed_amnt      = tab_ep-not_taxed.
  p_i_record2-taxed_amnt        = tab_ep-taxed.
  p_i_record2-tax_rate          = tab_ep-rate.
  p_i_record2-tax_amnt          = tab_ep-vat.
  p_i_record2-exempt_amnt       = tab_ep-exemption.
  p_i_record2-perce_amnt        = tab_ep-vat_percep.
  p_i_record2-per_nat_amnt      = tab_ep-earn_per.
  p_i_record2-perc_gi_amnt      = tab_ep-other_percep.
  p_i_record2-perc_mp_amnt      = tab_ep-munic_per.
  p_i_record2-int_tax_amnt      = tab_ep-vat_intern.
  p_i_record2-fityp             = tab_ep-fityp.
* Currency code was filled from database table TCURC
* Determine exchange rate from KURSF
  PERFORM fill_curr_info.
  PERFORM fill_exch_rate USING     tab_ep-waers
                         CHANGING  gf_exch_rate.
  p_i_record2-curr_code         = tab_curr-altkey.
  p_i_record2-exch_rate         = gf_exch_rate.
  p_i_record2-sev_vat_rates     = no_of_rates.
* Fill reason only, if either exempted amount or not taxed
* amount is the only field that is filled!
* Note: 690007
*  if P_I_RECORD2-EXEMPT_AMNT  eq P_I_RECORD2-TOTAL_AMNT or
*     P_I_RECORD2-N_TAXED_AMNT eq P_I_RECORD2-TOTAL_AMNT.
*    P_I_RECORD2-REASON_0_TAX      = TAB_EP-J_1ARFZ.
*  endif.

* Begin of change 988302
  IF find_exempted = 'X'.
    p_i_record2-reason_0_tax      = tab_ep-j_1arfz.
  ENDIF.

*  lcl_cmp_exempt = tab_ep-line_total.
*  IF p_i_record2-exempt_amnt  EQ lcl_cmp_exempt OR
*     p_i_record2-n_taxed_amnt EQ lcl_cmp_exempt.
*    p_i_record2-reason_0_tax      = tab_ep-j_1arfz.
*  ENDIF.
*  End of change 988302

  p_i_record2-cai               = tab_ep-cai.
  IF tab_ep-fityp = '01' AND
     tab_ep-stcdt NE '99'.                     " Note 645449
    p_i_record2-due_date        = tab_ep-due_date.
  ENDIF.
* Call form for filling the summation fields of i_record3
  PERFORM fill_sums_i_record3 USING    tab_ep
                              CHANGING i_record3_sum.
* Note 1037893 Start
  DESCRIBE FIELD p_i_record2 LENGTH len IN CHARACTER MODE.
*  PERFORM transfer_record USING p_i_record2 369.
  PERFORM transfer_record USING p_i_record2 len.
* Note 1037893 End

*  PERFORM transfer_p_record.  "Note 761049 Changes "Note - 1120234.

ENDFORM. " FILL_I_RECORD2

*&---------------------------------------------------------------------*
*&      Form  FILL_O_RECORD2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_O_RECORD2  text                                            *
*----------------------------------------------------------------------*
FORM fill_o_record2 CHANGING p_o_record2 LIKE o_record2.

  DATA: lcl_tfill      LIKE sy-tfill,
        lcl_stdat      LIKE bkpf-budat,           " Cancellation date
        lcl_cmp_exempt LIKE o_record2-total_amnt. " Note 690007

  p_o_record2-rectype           = '1'.
  p_o_record2-docu_date         = tab_ep-bldat.
  IF ep-j_1aoftp IS INITIAL.
    p_o_record2-docu_type = '00'.
  ELSE.
    p_o_record2-docu_type = tab_ep-j_1aoftp.
  ENDIF.
  p_o_record2-fisc_cont         = tab_ep-fisc_cont.
  p_o_record2-subsidiary        = tab_ep-xblnr(4).
  p_o_record2-docu_no           = tab_ep-xblnr+5(8).
  p_o_record2-last_docu_no      = tab_ep-xblnr+5(8).
  p_o_record2-type_of_id        = tab_ep-stcdt.
  IF tab_ep-stcdt = '99'. " New as of Res. 1361/2002
    CLEAR: p_o_record2-id_docu_no.
    p_o_record2-name              = TEXT-u32.
  ELSE.
    p_o_record2-id_docu_no        = tab_ep-stcd1.
    p_o_record2-name              = tab_ep-name1.
  ENDIF.
  p_o_record2-total_amnt        = tab_ep-total.
* not taxed: filled from tax id 'VL'
  p_o_record2-n_taxed_amnt      = tab_ep-not_taxed.
  p_o_record2-taxed_amnt        = tab_ep-taxed.
  p_o_record2-tax_rate          = tab_ep-rate.
  p_o_record2-tax_amnt          = tab_ep-vat.
* Surcharge = sum of RnR VAT and perception not categorize
  p_o_record2-surch_amnt       = tab_ep-rnr_vat + tab_ep-percepnoc.
  p_o_record2-exempt_amnt       = tab_ep-exemption.
  p_o_record2-perce_amnt        = tab_ep-vat_percep.
  p_o_record2-perc_gi_amnt       = tab_ep-other_percep.
  p_o_record2-perc_mp_amnt        = tab_ep-munic_per.
  p_o_record2-int_tax_amnt      = tab_ep-vat_intern.
  p_o_record2-fityp             = tab_ep-fityp.
* Currency code was filled from database table TCURC
* Determine exchange rate from KURSF
  PERFORM fill_curr_info.
  PERFORM fill_exch_rate USING     tab_ep-waers
                         CHANGING  gf_exch_rate.
  p_o_record2-curr_code         = tab_curr-altkey.
  p_o_record2-exch_rate         = gf_exch_rate.
  p_o_record2-sev_vat_rates     = no_of_rates.
* Fill reason only, if either exempted amount or not taxed
* amount is the only field that is filled!
* Note 690007
*  if P_O_RECORD2-EXEMPT_AMNT  eq P_O_RECORD2-TOTAL_AMNT or
*     P_O_RECORD2-N_TAXED_AMNT eq P_O_RECORD2-TOTAL_AMNT.
*    P_O_RECORD2-REASON_0_TAX      = TAB_EP-J_1ARFZ.
*  endif.
  lcl_cmp_exempt = tab_ep-line_total.
  IF p_o_record2-exempt_amnt  EQ lcl_cmp_exempt OR
     p_o_record2-n_taxed_amnt EQ lcl_cmp_exempt OR
     find_exempted = 'X'.                                   "1074703
    p_o_record2-reason_0_tax      = tab_ep-j_1arfz.
  ENDIF.
  p_o_record2-cai               = tab_ep-cai.
  IF tab_001-fityp = '01' AND                  " Note 636750
     tab_ep-stcdt NE '99'.                     " Note 645449
    p_o_record2-due_date          = tab_ep-due_date.
  ENDIF.
  IF par_canc IS INITIAL.
    IF NOT tab_ep-stblg IS INITIAL.
      SELECT SINGLE budat FROM bkpf INTO lcl_stdat
                                    WHERE bukrs = tab_ep-bukrs
                                    AND   belnr = tab_ep-stblg
                                    AND   gjahr = tab_ep-gjahr.
      p_o_record2-canc_date         = lcl_stdat.
    ENDIF.
  ENDIF.

* Call form for filling the summation fields of o_record3
  PERFORM fill_sums_o_record3 USING    tab_ep
                              CHANGING o_record3_sum.
* Note 1037893 Start
  DESCRIBE FIELD p_o_record2 LENGTH len IN CHARACTER MODE.
*  PERFORM transfer_record USING p_o_record2 375.
  PERFORM transfer_record USING p_o_record2 len.
* Note 1037893 End

*  PERFORM transfer_p_record.  "Note 761049 Changes "Note - 1120234.

ENDFORM. " FILL_O_RECORD2
*&---------------------------------------------------------------------*
*&      Form  FILL_P_RECORD
*&---------------------------------------------------------------------*
*       Fill Perception record
*----------------------------------------------------------------------*
*      <--P_P_RECORD  Perception information
*----------------------------------------------------------------------*
FORM fill_p_record CHANGING p_p_record TYPE type_p_record2.
  p_p_record-docu_date     = tab_ep-bldat.
  p_p_record-docu_type     = tab_ep-j_1aoftp.
  p_p_record-subsidiary    = tab_ep-xblnr(4).
  p_p_record-docu_no       = tab_ep-xblnr+5(8).

* J_1ATAXID was read just before calling this form routine
  p_p_record-regio_code    = tab_j_1ataxid-regio.
  p_p_record-perc_gi_amnt  = tab_ep-other_percep.
  p_p_record-munic_code    = tab_j_1ataxid-j_1amjc.
  p_p_record-perc_mp_amnt  = tab_ep-munic_per.

ENDFORM. " FILL_P_RECORD
*&---------------------------------------------------------------------*
*&      Form  FILL_SUMS_I_RECORD3
*&---------------------------------------------------------------------*
FORM fill_sums_i_record3 USING p_p_i_record2 TYPE type_ep
                         CHANGING p_i_record3   LIKE i_record3_sum.
  p_i_record3-total_amnt   = p_i_record3-total_amnt
                            + p_p_i_record2-total.
  p_i_record3-n_taxed_amnt = p_i_record3-n_taxed_amnt
                            + p_p_i_record2-not_taxed.
  p_i_record3-taxed_amnt   = p_i_record3-taxed_amnt
                            + p_p_i_record2-taxed.
  p_i_record3-tax_amnt     = p_i_record3-tax_amnt
                            + p_p_i_record2-vat.
  p_i_record3-exempt_amnt  = p_i_record3-exempt_amnt
                            + p_p_i_record2-exemption.
  p_i_record3-perce_amnt   = p_i_record3-perce_amnt
                            + p_p_i_record2-vat_percep.
  p_i_record3-perc_gi_amnt = p_i_record3-perc_gi_amnt
                            + p_p_i_record2-other_percep.
  p_i_record3-perc_mp_amnt = p_i_record3-perc_mp_amnt
                            + p_p_i_record2-munic_per.
  p_i_record3-int_tax_amnt = p_i_record3-int_tax_amnt
                            + p_p_i_record2-vat_intern.
*  NOTE 728727 Starts.
  p_i_record3-per_nat_amnt = p_i_record3-per_nat_amnt
                            + p_p_i_record2-earn_per.
*  NOTE 728727 ends.
ENDFORM. " FILL_SUMS_I_RECORD3
*&---------------------------------------------------------------------*
*&      Form  FILL_SUMS_O_RECORD3
*&---------------------------------------------------------------------*
FORM fill_sums_o_record3 USING p_p_o_record2 TYPE type_ep
                         CHANGING p_o_record3     LIKE o_record3_sum.
  p_o_record3-total_amnt   = p_o_record3-total_amnt
                            + p_p_o_record2-total.
  p_o_record3-n_taxed_amnt = p_o_record3-n_taxed_amnt
                            + p_p_o_record2-not_taxed.
  p_o_record3-taxed_amnt   = p_o_record3-taxed_amnt
                            + p_p_o_record2-taxed.
  p_o_record3-tax_amnt     = p_o_record3-tax_amnt
                            + p_p_o_record2-vat.
  p_o_record3-exempt_amnt  = p_o_record3-exempt_amnt
                            + p_p_o_record2-exemption.
* Field tab_ep-rnr_vat was modified in form FILL_O_RECORD2.
* Note 737661 changes starts
*  P_O_RECORD3-SURCH_AMNT   = P_O_RECORD3-SURCH_AMNT
*                            + P_P_O_RECORD2-RNR_VAT.
  p_o_record3-surch_amnt   = p_o_record3-surch_amnt
                            + p_p_o_record2-rnr_vat + tab_ep-percepnoc.
* Note 737661 changes ends

  p_o_record3-perce_amnt   = p_o_record3-perce_amnt
                            + p_p_o_record2-vat_percep.
  p_o_record3-perc_gi_amnt = p_o_record3-perc_gi_amnt
                            + p_p_o_record2-other_percep.
  p_o_record3-perc_mp_amnt = p_o_record3-perc_mp_amnt
                            + p_p_o_record2-munic_per.
  p_o_record3-int_tax_amnt = p_o_record3-int_tax_amnt
                            + p_p_o_record2-vat_intern.
ENDFORM. " FILL_SUMS_O_RECORD3
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_RECORD
*&---------------------------------------------------------------------*
*       Writes one record to the file on the application server
*----------------------------------------------------------------------*
*      -->P_RECORD     Record of any type                              *
*----------------------------------------------------------------------*
FORM transfer_record USING p_record
                              p_length TYPE i.
  IF NOT par_file IS INITIAL.
    TRANSFER p_record TO par_file LENGTH p_length.
*   Sales/Purchases File: Save locally
    IF NOT par_loc IS INITIAL.
      IF tab_j_1adrver-j_1aproc = '1' OR
         tab_j_1adrver-j_1aproc = '3'.    " input/purchase
        wa_i_record-record = p_record.
** Begin of Note 998965
*        WA_I_RECORD-CR_LF  = CL_ABAP_CHAR_UTILITIES=>CR_LF.
** End of Note 998965

        APPEND wa_i_record TO tab_i_record.
      ELSE.                                " output/sales
        wa_o_record-record = p_record.
** Begin of Note 998965
*        WA_O_RECORD-CR_LF  = CL_ABAP_CHAR_UTILITIES=>CR_LF.
** End of Note 998965

        APPEND wa_o_record TO tab_o_record.
      ENDIF.
    ENDIF.

* Note 761049 Changes Start - Commented here and moved to a new
* subroutine with some changes.
*   Perceptions File: Save locally
*    IF NOT PAR_LOC IS INITIAL.
*      WA_P_RECORD-RECORD = P_RECORD.
*      WA_P_RECORD-CR_LF  = cl_abap_char_utilities=>cr_lf.
*      APPEND WA_P_RECORD TO TAB_P_RECORD.
*    ENDIF.
* Note 761049 Changes ends

  ENDIF.
ENDFORM. " TRANSFER_RECORD
*&---------------------------------------------------------------------*
*&      Form  FILL_CURR_INFO
*&---------------------------------------------------------------------*
*       Fill internal table with currency key and exchange rate
*----------------------------------------------------------------------*
FORM fill_curr_info.
  CLEAR: tab_curr, tcurc, t028m.
  READ TABLE tab_curr WITH KEY tab_ep-waers.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM tcurc WHERE waers = tab_ep-waers.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING tcurc TO tab_curr.
    ELSE.
      tab_curr-waers = tab_ep-waers.
      CLEAR tab_log_entry10.
      tab_log_entry10-name    = 'TCURC'.
      tab_log_entry10-key     = tab_ep-waers.
      CONDENSE tab_log_entry10-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry10-name
                                           tab_log_entry10-key
                                           tab_log_entry10-belnr
                                           tab_ep-belnr.
      COLLECT tab_log_entry10.
    ENDIF.
    SELECT SINGLE * FROM t028m WHERE currkey = c_currkey
                               AND   waers   = tab_ep-waers.
    IF sy-subrc = 0.
      MOVE t028m-altwr TO tab_curr-altkey.
    ELSE.
      tab_curr-waers = tab_ep-waers.
      CLEAR tab_log_entry10.
      tab_log_entry10-name    = 'T028M'.
      tab_log_entry10-key     = tab_ep-waers.
      tab_log_entry10-key+3   = c_currkey.
      CONDENSE tab_log_entry10-key NO-GAPS.
      PERFORM insert_belnr_to_log CHANGING tab_log_entry10-name
                                           tab_log_entry10-key
                                           tab_log_entry10-belnr
                                           tab_ep-belnr.
      COLLECT tab_log_entry10.
    ENDIF.
    APPEND tab_curr.
  ENDIF.
ENDFORM. " FILL_CURR_INFO
*&---------------------------------------------------------------------*
*&      Form  FILL_EXCH_RATE
*&---------------------------------------------------------------------*
*       Fill exchange rate for foreign currencies.
*----------------------------------------------------------------------*
*       -->P_WAERS currency key
*       <--P_RATE  exchange rate
*----------------------------------------------------------------------*
FORM fill_exch_rate USING p_waers LIKE bkpf-waers
                    CHANGING p_rate     LIKE gf_exch_rate.
  DATA: lcl_wwert       LIKE bkpf-wwert,
        lcl_foreign     LIKE tcurr-ffact,
        lcl_local       LIKE tcurr-tfact,
        lcl_helpfac(16) TYPE p DECIMALS 6.

  CLEAR: p_rate.
  IF p_waers EQ tab_001-waers.
    p_rate = 1000000.
  ELSE.
    CLEAR: lcl_wwert, lcl_foreign, lcl_local, lcl_helpfac.
    IF lcl_wwert IS INITIAL.
      lcl_wwert = tab_ep-budat.
    ELSE.
      lcl_wwert = tab_ep-wwert.
    ENDIF.
*  IF TAB_EP-KURSF LT 0.   " Needed for releases higher than 4.5B
*    CALL FUNCTION 'KURS_IN_PREISNOTATION'
*         EXPORTING
*              DATE             = LCL_WWERT
*              FOREIGN_CURRENCY = P_WAERS
*              LOCAL_CURRENCY   = T001-WAERS
*              RATE             = TAB_EP-KURSF
*         IMPORTING
*              FOREIGN_FACTOR   = LCL_FOREIGN
*              LOCAL_FACTOR     = LCL_LOCAL
*         EXCEPTIONS
*              OTHERS           = 1.
*  ELSE.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        date             = lcl_wwert
        foreign_currency = tab_ep-waers
        local_currency   = t001-waers
      IMPORTING
        foreign_factor   = lcl_foreign
        local_factor     = lcl_local
      EXCEPTIONS
        OTHERS           = 1.
*  endif.
    IF sy-subrc NE 0.
      lcl_helpfac = 0.
    ELSE.
      lcl_helpfac = tab_ep-kursf * lcl_local.
      lcl_helpfac = lcl_helpfac * 10.
      lcl_helpfac = lcl_helpfac / lcl_foreign.
    ENDIF.
    p_rate = lcl_helpfac.
  ENDIF.
ENDFORM. " FILL_EXCH_RATE
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_LOCAL
*&---------------------------------------------------------------------*
*       Transfer magnetic output from application server to local file
*----------------------------------------------------------------------*
FORM transfer_local USING p_file LIKE rfpdo1-allgunix
                          p_local
                          p_tab_record TYPE table.
  DATA: lcl_file TYPE string.
  MOVE p_local TO lcl_file.
* Take the Unicode-enabled function module
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename         = lcl_file
      filetype         = 'ASC'
      dat_mode         = 'X'                "Note 786791
*     WRITE_LF         = ' '               "Note 786791
    TABLES
      data_tab         = p_tab_record
    EXCEPTIONS
      file_write_error = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE i812 WITH par_lfil.
  ENDIF.
  IF NOT par_dele IS INITIAL.
    DELETE DATASET par_file.

    IF sy-subrc = 0.
      MESSAGE s455.
    ELSE.
      MESSAGE i456.
    ENDIF.
  ENDIF.
ENDFORM. " TRANSFER_LOCAL
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOREIGN_SD_DOCS
*&---------------------------------------------------------------------*
*&      See notes 457591 and 530252
*&---------------------------------------------------------------------*
FORM check_foreign_sd_docs.
  DATA: lcl_awkey TYPE bkpf-awkey. " Note 554753

  rcode = 0.
  IF vbrk-land1 NE t001-land1.   " SD document to foreign country
    IF konv-kawrt NE 0.
      rcode = 0.
    ELSE.
      rcode = 4.
    ENDIF.
  ELSE.                          " Domestic SD document
* If batch item with initial value or main item with initial value: skip
    IF NOT konv-kwert IS INITIAL OR
       ( vbrp-uecha NE vbrp-posnr AND vbrp-xchar IS INITIAL ).
      rcode = 0.
    ELSE.
      rcode = 4.
    ENDIF.
  ENDIF.

* Begin note 554753: Further checks can be done in a Customer Exit
* Call was initiated in GET BKPF LATE
  lcl_awkey = bkpf-awkey.        " -> Keep current value of AWKEY
  " -> Pass position number to event
  CONCATENATE bkpf-awkey vbrp-posnr INTO bkpf-awkey.
  CALL FUNCTION 'J_1A_EXIT_J_1AF105'
    EXPORTING
      i_bkpf  = bkpf
      i_step  = '003'
    IMPORTING
      e_rcode = rcode.
  bkpf-awkey = lcl_awkey.       " Restore correct AWKEY value
* End note 554753

ENDFORM. " CHECK_FOREIGN_SD_DOCS

*&---------------------------------------------------------------------*
*&      Form  check_for_zero_line
*&---------------------------------------------------------------------*
*       If all amounts and the rate are zero: Don't save record to file
*----------------------------------------------------------------------*
FORM check_for_zero_line USING p_ep TYPE type_ep
                         CHANGING p_rcode TYPE sy-subrc.
  CLEAR: p_rcode.
  CHECK: p_ep-rate         IS INITIAL,
         p_ep-rate         IS INITIAL,
         p_ep-dspl_rate    IS INITIAL,
         p_ep-taxed        IS INITIAL,
         p_ep-not_taxed    IS INITIAL,
         p_ep-vat          IS INITIAL,
         p_ep-rnr_vat      IS INITIAL,
         p_ep-vat_percep   IS INITIAL,
         p_ep-other_percep IS INITIAL,
         p_ep-munic_per    IS INITIAL,
         p_ep-vat_intern   IS INITIAL,
         p_ep-exemption    IS INITIAL,
         p_ep-surcharge    IS INITIAL,
         p_ep-exports      IS INITIAL,
         p_ep-percepnoc    IS INITIAL,
         p_ep-line_total   IS INITIAL.

  p_rcode = 4.      " all amounts and the rate are zero

ENDFORM. " check_for_zero_line

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_P_RECORD
*&---------------------------------------------------------------------*
*       New Subroutine as per note 761049.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_p_record .

  IF NOT par_lfil IS INITIAL.

*   Perceptions File: Save locally
    IF NOT par_ploc IS INITIAL.
      wa_p_record-record = p_record2+6(94).
** Begin of Note 998965
*      WA_P_RECORD-CR_LF  = CL_ABAP_CHAR_UTILITIES=>CR_LF.
** End of Note 998965

*     Perceptions File: no initial records              " Note 875262
      IF NOT wa_p_record-record CO ' 0'.
        APPEND wa_p_record TO tab_p_record.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM. " TRANSFER_P_RECORD
*&---------------------------------------------------------------------*
*&      Form  collect_sum_account
*&---------------------------------------------------------------------*
*  Tab_sum_account - global value
FORM collect_sum_account .
* collect totals per tax account
  CLEAR: tab_sum_account.
  tab_sum_account-hkont = tab_ep-hkont.
*---> 09/06/2023 - Migração S4 - JS
*   tab_sum_account-saldo = tab_ep-hwste.
  tab_sum_account-saldo = CONV #( tab_ep-hwste ).
*<--- 09/06/2023 - Migração S4 - JS
* Note 685915
  IF tab_j_1adrver-j_1aproc EQ 1 OR
     tab_j_1adrver-j_1aproc EQ 3.
    IF tab_ep-hwste < 0.
      tab_sum_account-haben = tab_ep-hwste * -1.
    ELSE.
*---> 09/06/2023 - Migração S4 - JS
*   tab_sum_account-soll = tab_ep-hwste.
      tab_sum_account-soll = CONV #( tab_ep-hwste ).
*<--- 09/06/2023 - Migração S4 - JS
    ENDIF.
  ELSE.
    IF tab_ep-hwste < 0.
      tab_sum_account-soll = tab_ep-hwste * -1.
    ELSE.
*---> 09/06/2023 - Migração S4 - JS
*      tab_sum_account-haben = tab_ep-hwste.
      tab_sum_account-haben = CONV #( tab_ep-hwste ).
*<--- 09/06/2023 - Migração S4 - JS
    ENDIF.
  ENDIF.
  COLLECT tab_sum_account.

ENDFORM. " collect_sum_account

*[BEGIN] -- PM -- 18/08/09
**********************************************************************

FORM inicializar_alv CHANGING p_it_header TYPE STANDARD TABLE
                              p_gt_fieldcat TYPE STANDARD TABLE
                              p_gs_layout TYPE slis_layout_alv
                              p_t_sort    TYPE slis_t_sortinfo_alv.

  PERFORM f_header CHANGING p_it_header[].
  PERFORM f_layout CHANGING p_gs_layout p_t_sort.
  PERFORM init_fieldcat CHANGING p_gt_fieldcat[].

ENDFORM. " inicializar_alv

*&---------------------------------------------------------------------*
*&      Form  f_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HEADER  text
*----------------------------------------------------------------------*
FORM f_header CHANGING p_it_header TYPE STANDARD TABLE.

  DATA: wa_header TYPE slis_listheader.      " Work Area con el Titulo
  DATA: ls_event TYPE slis_alv_event.

  CLEAR wa_header.
  wa_header-typ = 'H'.
  wa_header-info = 'Subdiario de IVA'.
  APPEND  wa_header TO p_it_header.

  CLEAR t_events.
  REFRESH t_events.

  CLEAR ls_event.
  ls_event-name = slis_ev_top_of_page. "Evento de principio de pagina
  ls_event-form = 'HEADER_SHOW'.       "Subrutina que muestra la cab.
  APPEND ls_event TO t_events.        "Lo añado a la variable global

ENDFORM. "f_header

*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->GS_LAYOUT  text
*      -->P_T_SORT   text
*----------------------------------------------------------------------*
FORM f_layout CHANGING gs_layout TYPE slis_layout_alv
                       p_t_sort    TYPE slis_t_sortinfo_alv.

  DATA: x_sort    TYPE slis_sortinfo_alv.    " gt_events TYPE

* Layout
  CLEAR gs_layout.
*  gs_layout-zebra = 'X'.

  x_sort-spos = 1.
  x_sort-fieldname = 'BELNR'.
  x_sort-tabname   = 'T_ALV'.
  x_sort-subtot    = 'X'.
  x_sort-up        = 'X'.

*  x_sort-spos = 1.
*  x_sort-fieldname = 'MWSKZ'.
*  x_sort-tabname   = 'T_ALV'.
*  x_sort-up        = 'X'.


  gs_layout-zebra = 'X'.
  gs_layout-detail_popup = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  APPEND x_sort TO p_t_sort.

ENDFORM. " f_layout

*&---------------------------------------------------------------------*
*&      Form  init_fieldcat
*&---------------------------------------------------------------------*
FORM init_fieldcat CHANGING p_gt_fieldcat TYPE STANDARD TABLE.

  DATA: x_fieldcat TYPE slis_fieldcat_alv.
  CASE v_ucomm .
    WHEN '&T1'.
      PERFORM f_fieldcat USING 'Detalle '           'DESCRIP' '8' ' ' ' '.
      PERFORM f_fieldcat USING 'Alicuota'           'RATE'    '8'  ' ' ' '.
      PERFORM f_fieldcat USING 'Importe'            'IVAT'    '18' ' ' 'X'.
    WHEN '&T2'.
      PERFORM f_fieldcat USING 'Cuenta'             'HKONT'   '15' ' ' ' '.
      PERFORM f_fieldcat USING 'Detalle '           'DESCRIP' '20' ' ' ' '.
      PERFORM f_fieldcat USING 'Porc % '            'KBETR'   '8'  ' ' 'X'.
      PERFORM f_fieldcat USING 'Importe'            'IMP'     '18' ' ' 'X'.
    WHEN '&T3'.
      PERFORM f_fieldcat USING 'Cuenta'             'HKONT'   '15' ' ' ' ' .
      PERFORM f_fieldcat USING 'Detalle '           'DESCRIP' '20' ' ' ' ' .
      PERFORM f_fieldcat USING 'Cod. Prov'          'BLAND'   '8'  ' ' ' ' .
      PERFORM f_fieldcat USING 'Provincia'          'BEZEI'   '30'  ' ' ' '.
      PERFORM f_fieldcat USING 'Importe'            'IMP'     '18' ' ' 'X' .
    WHEN '&T4'.
      PERFORM f_fieldcat USING 'Código'           'J_1AFITP'  '8'  ' ' ' ' .
      PERFORM f_fieldcat USING 'Detalle '         'DESCRIP'   '40'  ' ' ' '.
*      PERFORM f_fieldcat USING 'Importe'          'IMP'       '18' ' ' 'X' .
      PERFORM f_fieldcat USING 'Alicuota'         'RATE'      '8'  ' ' ' '.
      PERFORM f_fieldcat USING 'Gravado'          'IMP_GRAV'  '18' ' ' 'X' .
      PERFORM f_fieldcat USING 'No Grav.'         'IMP_NGRAV' '18' ' ' 'X' .
      PERFORM f_fieldcat USING 'Exento'           'IMP_EXENT' '18' ' ' 'X' .
    WHEN '&T6'.
      PERFORM f_fieldcat USING 'Detalle '           'DESCRIP' '8' ' ' ' '.
      PERFORM f_fieldcat USING 'Alicuota'           'RATE'    '8'  ' ' ' '.

      CLEAR x_fieldcat.
      x_fieldcat-fieldname = 'ITAXED'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Gravado'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

*  IF par_cust IS INITIAL. "Compras
      x_fieldcat-fieldname = 'INOT_TAXED'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'No gravado'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.
*  ENDIF.
      x_fieldcat-fieldname = 'IEXEMPTION'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Exento'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'IVAT'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'IVA'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

    WHEN OTHERS.
      x_fieldcat-fieldname = 'BUDAT'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m =  'Fecha de Contabilización'.
      x_fieldcat-outputlen = 12.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'BLART'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Tipo de documento'.
      x_fieldcat-outputlen = 5.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'BRNCH'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = TEXT-003.
      x_fieldcat-outputlen = 5.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-no_out = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'BELNR'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Documento Sap'.
      x_fieldcat-outputlen = 14.
      x_fieldcat-hotspot = 'X'.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.

      x_fieldcat-hotspot = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'LIFNR'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Proveedor'.
      x_fieldcat-outputlen = 19.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'NAME1'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Razón Social'.
      x_fieldcat-outputlen = 24.
      x_fieldcat-just = 'L'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'STCD1'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Cuit'.
      x_fieldcat-outputlen = 19.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'BLDAT'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m =  'Fecha del documento'.
      x_fieldcat-outputlen = 12.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'XBLNR'. "Cl.doc.oficial
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Nº de documento'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'TPCBIO'. "Tipo de cambio
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Tpo de cambio'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'TOTAL'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Total'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'MWSKZ'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Indicador'.
      x_fieldcat-outputlen = 5.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'RATE'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Alicuota iva'.
      x_fieldcat-outputlen = 7.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'ITAXED'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Gravado'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

*  IF par_cust IS INITIAL. "Compras
      x_fieldcat-fieldname = 'INOT_TAXED'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'No gravado'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.
*  ENDIF.
      x_fieldcat-fieldname = 'IEXEMPTION'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Exento'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'IVAT'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m =  'Iva'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'IVAT_PERCEP'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Percepción de iva'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'IOTHER_PERCEP02'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_l = 'Percepción de IIBB CABA'.
      x_fieldcat-seltext_m = x_fieldcat-seltext_l.
      x_fieldcat-outputlen = 23.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

*  IF par_cust IS INITIAL. "Compras
      x_fieldcat-fieldname = 'IOTHER_PERCEP'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_l = 'Percepción de IIBB Buenos Aires'.
      x_fieldcat-seltext_m = x_fieldcat-seltext_l.
      x_fieldcat-outputlen = 27.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.
*  ENDIF.

      x_fieldcat-fieldname = 'IOTHER_PERCEP03'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_l = 'Percepción de IIBB Santa Fe'.
      x_fieldcat-seltext_m = x_fieldcat-seltext_l.
      x_fieldcat-outputlen = 27.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

* --- Begin ---- GGU -----
*  IF par_cust IS INITIAL. "Compras
      x_fieldcat-fieldname = 'IEARN_PER'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Percepción de Ganancias'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.
*  ENDIF.
* --- End   ---- GGU -----

*  IF par_cust IS INITIAL. "Compras
      x_fieldcat-fieldname = 'IIBB'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'IIBB'.
      x_fieldcat-outputlen = 15.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      x_fieldcat-no_out = 'X'.
      x_fieldcat-do_sum = 'X'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.
*  ENDIF.

      x_fieldcat-fieldname = 'STCDT'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Nº de identificación'.
      x_fieldcat-outputlen = 19.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'HKONT'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Cta contable'.
      x_fieldcat-outputlen = 14.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-no_out = 'X'.
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'TIPO_DOC'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_s = 'Cl.doc'(055).
      x_fieldcat-seltext_m = 'Clase de doc.'(054).
      x_fieldcat-seltext_l = 'Clase de documento'(053).
      x_fieldcat-outputlen = 10.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'FITYP'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Clase de impuesto'.
      x_fieldcat-outputlen = 10.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'CAI'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Nro de Cai'.
      x_fieldcat-outputlen = 22.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.

      x_fieldcat-fieldname = 'FECHA_CAI'.
      x_fieldcat-tabname = 'T_ALV'.
      x_fieldcat-seltext_m = 'Vto Cai o Cae'.
      x_fieldcat-outputlen = 10.
      x_fieldcat-just = 'R'. "s
      x_fieldcat-ddictxt = 'M'.
      APPEND x_fieldcat TO p_gt_fieldcat.
      CLEAR x_fieldcat.
  ENDCASE.




ENDFORM. " init_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM status_iva USING rt_extab TYPE slis_t_extab.

*  SET TITLEBAR  'TITULO'    . "-> Esto es apra un titulo en el ALV
  SET PF-STATUS 'STATUS_IVA'. "-> le pasas el nombre del STATUS


ENDFORM. " aSTATUS_ALV
*&---------------------------------------------------------------------*
*&      Form  muestra_alv
*&---------------------------------------------------------------------*

FORM muestra_alv USING p_gs_layout p_t_sort
                 p_gt_fieldcat TYPE INDEX TABLE
                 p_t_alv TYPE STANDARD TABLE.

  DATA: v_repid LIKE sy-repid.    " Nombre del programa

  v_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_repid
      is_layout                = p_gs_layout
      i_callback_user_command  = 'USER_COMMAND_CUS'
      it_fieldcat              = p_gt_fieldcat[]
      i_callback_pf_status_set = 'STATUS_IVA'
      i_save                   = 'A'
      it_sort                  = p_t_sort
      it_events                = t_events  "Añado los eventos al ALV
    TABLES
      t_outtab                 = p_t_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF sy-batch = 'X'.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM. "muestra_alv
*&---------------------------------------------------------------------*
*&      Form  user_command_cus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command_cus USING r_ucomm LIKE sy-ucomm
                       rs_selfield TYPE slis_selfield.

  DATA: x_alv LIKE t_alv.
  IF  r_ucomm NE '&IC1'.
    PERFORM f_init_layout.
*  CONSTANTS: c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE'.
  ENDIF.
  CLEAR v_ucomm.
  MOVE r_ucomm TO v_ucomm .

  CASE r_ucomm.
    WHEN '&IC1'.
      CHECK rs_selfield-sel_tab_field = 'T_ALV-BELNR'.
      SET PARAMETER ID 'BLN' FIELD rs_selfield-value.
      SET PARAMETER ID 'BUK' FIELD br_bukrs-low.
      SET PARAMETER ID 'GJR' FIELD br_gjahr-low.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN '&T1'.
      PERFORM muestro_total_iva.
    WHEN '&T2'.
      PERFORM muestro_perc_iva.
    WHEN '&T3'.
      PERFORM muestro_perc_iibb.
    WHEN '&T4'.
      PERFORM muestro_operaciones.
    WHEN '&T6'.
      PERFORM muestro_bases.
    WHEN '&T5'. "Print
*      PERFORM impresion_smartforms.
      PERFORM impresion.


  ENDCASE.

ENDFORM. "user_command


*&---------------------------------------------------------------------*
*&      Form  impresion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM impresion.
  CONSTANTS:
    c_spos  TYPE slis_spos VALUE '01', " Column position
    c_up    TYPE slis_soup VALUE 'X', " Sorting order
    c_table TYPE tabname VALUE 'I_FINAL', " Name of o/p table
    c_group TYPE slis_ctrls VALUE '*', " Sorting group
    c_fld2  TYPE fieldname VALUE 'IDCNGA04-IDCN037'. " Field name(Document no)





*Prepare sort table

  PERFORM prepare_event_table .
  PERFORM prepare_event_table.
  PERFORM f_print.
ENDFORM.                    "impresion


*&---------------------------------------------------------------------*
*&      Form  prepare_event_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_event_table .


ENDFORM. " prepare_event_table



*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.

ENDFORM. " top_of_page



*&---------------------------------------------------------------------*
*&      Form  f_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_print.
  DATA st_print TYPE slis_print_alv.

  DATA: t_event_exit  TYPE slis_t_event_exit,
        lt_event_exit TYPE slis_event_exit.
  DATA: t_event  TYPE slis_t_event,
        lt_event TYPE slis_alv_event.

  lt_event_exit-ucomm = '&ILT'.
  lt_event_exit-after = 'X'.
  APPEND lt_event_exit TO t_event_exit.
*
*
  CLEAR lt_event.
  lt_event-name = 'USER_COMMAND'.
  lt_event-form = 'USER_COMMAND'.
  APPEND lt_event TO t_event.
*
  CLEAR lt_event.
  lt_event-name = 'TOP_OF_PAGE'.
  lt_event-form = 'TOP_OF_PAGE'.
  APPEND lt_event TO t_event.
*
  CLEAR lt_event.
  lt_event-name = 'TOP_OF_LIST'.
  lt_event-form = 'TOP_OF_LIST'.
  APPEND lt_event TO t_event.
  st_print-print = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid  " Call back program
      it_fieldcat        = gt_fieldcat[] " Field catalog
      i_save             = 'A'
      it_events          = t_event
      is_print           = st_print " Print parameters
    TABLES
      t_outtab           = t_alv " Output table
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.                    "f_print


*&---------------------------------------------------------------------*
*&      Form  ELIMINA_DOCUMENTOS_NO_VIS
*&---------------------------------------------------------------------*
FORM elimina_documentos_no_vis.

* Elimino cabeceras
  DELETE t_alv WHERE flag = 'X'.
  DELETE ADJACENT DUPLICATES FROM t_alv COMPARING ALL FIELDS.

ENDFORM. " ELIMINA_DOCUMENTOS_NO_VIS
**---------------------------------------------------------------------*
** FORM TOP_OF_PAGE *
**---------------------------------------------------------------------*
FORM header_show.

* Seteo todos los datos correspondientes al encabezado del alv

  DATA: lt_commentary TYPE slis_t_listheader WITH HEADER LINE,
        periodo(140)  TYPE c,
        fecha1        TYPE char10,
        fecha2        TYPE char10,
        v_texto(140)  TYPE c,
        direccion     TYPE char100,
        stl_t001      TYPE t001,
        vl_cuit       TYPE char15,
        stl_t005t     TYPE t005t,
        stl_adrc      TYPE adrc.
  DATA:
    vl_mes     TYPE char2,
    vl_periodo TYPE char2.

** Header
  lt_commentary-typ = 'H'.
  IF NOT par_cust IS INITIAL. "Ventas
    lt_commentary-info = 'Subdiario IVA Ventas'.
  ELSE.
    lt_commentary-info = 'Subdiario IVA Compras'.
  ENDIF.
  APPEND lt_commentary. CLEAR lt_commentary.

*Javier titulo de la empresa
  SELECT SINGLE *
   FROM t001
   INTO stl_t001
   WHERE bukrs IN br_bukrs.

  SELECT SINGLE *
  FROM adrc
  INTO stl_adrc
  WHERE addrnumber EQ stl_t001-adrnr.
*-----------Direccion


  CONCATENATE stl_adrc-street stl_adrc-house_num1 stl_adrc-city1
  INTO direccion SEPARATED BY space.

  SELECT SINGLE *
  FROM t005t
  INTO stl_t005t
  WHERE spras EQ  stl_adrc-langu
    AND land1 EQ stl_adrc-country.

  SELECT SINGLE paval
  FROM t001z
  INTO vl_cuit
  WHERE bukrs EQ stl_t001-bukrs
  AND   party EQ 'J1AIDN'.


***
  lt_commentary-typ = 'S'.
  lt_commentary-info =  stl_t001-butxt.
  APPEND lt_commentary. CLEAR lt_commentary.


  lt_commentary-typ = 'S'.
  lt_commentary-info = direccion.
  APPEND lt_commentary. CLEAR lt_commentary.


***
  lt_commentary-typ  = 'S'.
  lt_commentary-info = stl_t005t-landx.
  APPEND lt_commentary. CLEAR lt_commentary.
*

  lt_commentary-typ = 'S'.
  CONCATENATE 'C.U.I.T.'
              vl_cuit
  INTO lt_commentary-info SEPARATED BY space.
  APPEND lt_commentary. CLEAR lt_commentary.

  lt_commentary-typ = 'S'.

  MOVE br_budat-low+4(2) TO vl_mes.

  PERFORM f_periodo USING vl_mes CHANGING vl_periodo.


  CONCATENATE 'Periodo' vl_periodo '-' br_gjahr-low INTO lt_commentary-info SEPARATED BY space.
  APPEND lt_commentary. CLEAR lt_commentary.

  lt_commentary-typ = 'S'.
  CLEAR lt_commentary-info.
*    data: fecha  type char10,
*          fecha2 type char10.

  CONCATENATE           br_budat-low+6(2) '/'
                        br_budat-low+4(2) '/'
                        br_budat-low(4)
INTO fecha1.
  CONCATENATE           br_budat-high+6(2) '/'
                        br_budat-high+4(2) '/'
                        br_budat-high(4)
INTO fecha2.

  CONCATENATE 'Fecha desde'
               fecha1
               'hasta'
               fecha2
  INTO lt_commentary-info SEPARATED BY space.
  APPEND lt_commentary. CLEAR lt_commentary.


**
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_commentary[].

ENDFORM. "header_show
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_initialization .
  s_init = 4.                          " each calendar month new lineno
  s_drver = '03'.                      " version of the daily reports
  s_numbc = 'X'.                       " Page numbering consecutively
  par_comp = 'X'.                      " display tax code compressed.
  par_sddi = ' '.                      " display SD Faktura
  par_rahm = 'X'.                      " display boxes
  par_updh = ' '.                      " History table update.
  par_sort = 1.                        " Sorting procedure
* par_cexi = ' '.                      " don't run with customer exit
  par_dspt = ' '.         " display tax base as zero for manual postings
  par_vers = 'X'.                      " run with version
  par_canc = 'X'.                      " Note 427832
  listsize = 245.
  par_lfil = 'C:\TEMP\FILE_DGI.TXT'.   " Magnetic output: Local file
  par_pfil = 'C:\TEMP\FILE_PER.TXT'.   " Magnetic output: Perc. file
  par_cust = 'X'.
  par_vend = 'X'.

  history_repid = sy-repid.
* initial BUKRS screen field
  GET PARAMETER ID 'BUK' FIELD br_bukrs-low.
  APPEND br_bukrs.

  br_budat-low  = sy-datum.             " initial budat current day 1069346
  br_budat-high = sy-datum.             " initial budat current day 1069346
  br_budat-option = 'EQ'.                                   "1069346
  br_budat-sign   = 'I'.                                    "1069346
  APPEND br_budat.                                          "1069346
* init title texts.
  MOVE: TEXT-u12 TO p_col2,
        TEXT-u13 TO p_col3,
        TEXT-u14 TO p_col4,
        TEXT-u15 TO p_col5,
        TEXT-u16 TO p_col6,
        TEXT-u17 TO p_col7,
        TEXT-u18 TO p_col8,
        TEXT-u21 TO p_col09,
        TEXT-u22 TO p_col10,
        TEXT-u19 TO p_col11,
        TEXT-u30 TO p_sdexp,
        TEXT-u21 TO hlp_col9,          " For the feature of the
        TEXT-u22 TO hlp_col10,         " changeable column titles:
        TEXT-u25 TO hlp_col9m,         " Manual changes are restored
        TEXT-u26 TO hlp_col10m.        " on changing the radiobutton

  p_txid1 = 'EX01'.
  p_txid2 = 'NC01'.

  DESCRIBE FIELD hlp_epos1 LENGTH tmp_length IN CHARACTER MODE.
  IF tmp_length > 245.
* Length of structure & too long for output
* Die Länge von Struktur  & ist zu lang für die Ausgabe
    MESSAGE e102(ar) WITH 'hlp_epos1' '245'.
  ENDIF.
  DESCRIBE FIELD hlp_epos2 LENGTH tmp_length IN CHARACTER MODE.
  IF tmp_length > 245.
* Die Länge von Struktur  & ist zu lang für die Ausgabe
    MESSAGE e102(ar) WITH 'hlp_epos2' '245'.
  ENDIF.
  DESCRIBE FIELD hlp_epos3 LENGTH tmp_length IN CHARACTER MODE.
  IF tmp_length > 245.
* Die Länge von Struktur  & ist zu lang für die Ausgabe
    MESSAGE e102(ar) WITH 'hlp_epos3' '245'.
  ENDIF.

  DESCRIBE FIELD hlp_epos3a LENGTH tmp_length IN CHARACTER MODE.
  IF tmp_length > 245.
* Die Länge von Struktur  & ist zu lang für die Ausgabe
    MESSAGE e102(ar) WITH 'hlp_epos3a' '245'.
  ENDIF.

  DESCRIBE FIELD hlp_epos3b LENGTH tmp_length IN CHARACTER MODE.
  IF tmp_length > 245.
* Die Länge von Struktur  & ist zu lang für die Ausgabe
    MESSAGE e102(ar) WITH 'hlp_epos3b' '245'.
  ENDIF.

* Set country for table J_1APACK1
  SELECT SINGLE land1 FROM t005 INTO gf_land1 WHERE intca = 'AR'.
ENDFORM. " F_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_input USING pi_entrada
                                  CHANGING po_salida.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pi_entrada
    IMPORTING
      output = po_salida.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TOTALES
*&---------------------------------------------------------------------*
FORM f_armo_totales .
  PERFORM f_total_por_iva     .
  PERFORM f_percep_iibb       .
  PERFORM f_condicion_fiscalia.
  PERFORM f_bases_imponibles  .
ENDFORM. " F_ARMO_TOTALES
*&---------------------------------------------------------------------*
*&      Form  F_TOTAL_POR_IVA
*&---------------------------------------------------------------------*
FORM f_total_por_iva .


  CLEAR t_iva-rate.
  WRITE '10.50' TO t_alv-rate LEFT-JUSTIFIED.
  CONDENSE t_iva-rate NO-GAPS..
  MOVE: 'IVA'      TO  t_iva-descrip ,
        t_alv-rate TO t_iva-rate.
  COLLECT t_iva.
  CLEAR t_iva-rate.
  WRITE '21.00' TO t_alv-rate LEFT-JUSTIFIED.
  CONDENSE t_iva-rate NO-GAPS..
  MOVE: 'IVA'      TO  t_iva-descrip ,
        t_alv-rate TO t_iva-rate.
  COLLECT t_iva.
  CLEAR t_iva-rate.
  WRITE '27.00' TO t_alv-rate LEFT-JUSTIFIED.
  CONDENSE t_iva-rate NO-GAPS..
  MOVE: 'IVA'      TO  t_iva-descrip ,
        t_alv-rate TO t_iva-rate.
  COLLECT t_iva.
  CLEAR t_iva-rate.

  SORT t_iva BY rate.

  LOOP AT t_alv
    WHERE rate NE '   0.00%'
    AND   rate NE '   0,00%'
    AND   rate NE '        ' .

    REPLACE ',' WITH '.' INTO t_alv-rate.
    MOVE: 'IVA'      TO t_iva-descrip ,
          t_alv-rate TO t_iva-rate.
    t_iva-ivat = t_alv-ivat.
    CONDENSE t_iva-rate NO-GAPS..

    CHECK: t_iva-rate NE '0.00',
           t_iva-rate NE '0,00'.

    COLLECT t_iva.
  ENDLOOP.
  SORT t_iva BY rate.
ENDFORM. " F_TOTAL_POR_IVA
*&---------------------------------------------------------------------*
*&      Form  CONDICION_FISCALIA
*&---------------------------------------------------------------------*
FORM f_condicion_fiscalia .
  DATA:
    stl_desc TYPE j_1afitpvt,
    stl_lfa1 TYPE lfa1,
    tl_desc  TYPE STANDARD TABLE OF j_1afitpvt.


  SELECT *
  FROM j_1afitpvt
  INTO TABLE tl_desc
  WHERE spras EQ sy-langu.
  SORT t_alv   BY lifnr.
  SORT tl_desc BY j_1afitp.
* Armo la tabla
  LOOP AT tl_desc INTO stl_desc.
    t_operaciones-j_1afitp = stl_desc-j_1afitp.
    t_operaciones-descrip  = stl_desc-text60.
* Iva 27%
    WRITE '  27.00' TO t_operaciones-rate.
    CLEAR: t_operaciones-imp_grav, t_operaciones-imp_ngrav, t_operaciones-imp_exent.
    COLLECT t_operaciones.
* Iva 21%
    WRITE '  21.00' TO t_operaciones-rate.
    CLEAR: t_operaciones-imp_grav, t_operaciones-imp_ngrav, t_operaciones-imp_exent.
    COLLECT t_operaciones.
* Iva 10.5%
    WRITE '  10.50' TO t_operaciones-rate.
    CLEAR: t_operaciones-imp_grav, t_operaciones-imp_ngrav, t_operaciones-imp_exent.
    COLLECT t_operaciones.
* Iva Exento
    WRITE 'Exento' TO t_operaciones-rate.
    CLEAR: t_operaciones-imp_grav, t_operaciones-imp_ngrav, t_operaciones-imp_exent.
    COLLECT t_operaciones.
* Iva No Gravado
    WRITE 'No Grav.' TO t_operaciones-rate.
    CLEAR: t_operaciones-imp_grav, t_operaciones-imp_ngrav, t_operaciones-imp_exent.
    COLLECT t_operaciones.
  ENDLOOP.

  IF t_lfa1[] IS NOT INITIAL.
    LOOP AT  t_alv.
      READ TABLE t_lfa1 INTO stl_lfa1
      WITH KEY lifnr = t_alv-lifnr.

      READ TABLE tl_desc INTO stl_desc
      WITH KEY j_1afitp = stl_lfa1-fityp.

      CHECK sy-subrc EQ 0.
      t_operaciones-j_1afitp = stl_desc-j_1afitp.
      t_operaciones-descrip  = stl_desc-text60.
      t_operaciones-rate     = t_alv-rate.
      IF t_alv-rate = '   0.00'.
        PERFORM f_agregar_exen_nograv USING t_alv-ktosl CHANGING t_operaciones-rate.
        IF t_operaciones-rate IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
      t_operaciones-imp_grav  =  t_alv-itaxed.
      t_operaciones-imp_ngrav =  t_alv-inot_taxed.
      t_operaciones-imp_exent =  t_alv-iexemption.
      COLLECT t_operaciones.
    ENDLOOP.
  ENDIF.

*  Descripciones
  SORT t_alv BY budat.
ENDFORM. " CONDICION_FISCALIA
*&---------------------------------------------------------------------*
*&      Form  MUESTRO_TOTAL_IVA
*&---------------------------------------------------------------------*
FORM muestro_total_iva .
  REFRESH t_fieltotal.
  PERFORM init_fieldcat USING t_fieltotal[].
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = layout
      it_fieldcat        = t_fieltotal[]
      i_save             = 'X'
      it_events          = t_events[]
    TABLES
      t_outtab           = t_iva[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM. " MUESTRO_TOTAL_IVA
*&---------------------------------------------------------------------*
*&      Form  MUESTRO_PERC_IVA
*&---------------------------------------------------------------------*
FORM muestro_perc_iva .
  REFRESH t_fieltotal.
  PERFORM init_fieldcat USING t_fieltotal[].
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = layout
      it_fieldcat        = t_fieltotal[]
      i_save             = 'X'
      it_events          = t_events[]
    TABLES
      t_outtab           = t_periva[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM. " MUESTRO_PERC_IVA
*&---------------------------------------------------------------------*
*&      Form  MUESTRO_PERC_IIBB
*&---------------------------------------------------------------------*
FORM muestro_perc_iibb .
  REFRESH t_fieltotal.
  PERFORM init_fieldcat USING t_fieltotal[].
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = layout
      it_fieldcat        = t_fieltotal[]
      i_save             = 'X'
      it_events          = t_events[]
    TABLES
      t_outtab           = t_periibb[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM. " MUESTRO_PERC_IIBB
*&---------------------------------------------------------------------*
*&      Form  MUESTRO_OPERACIONES
*&---------------------------------------------------------------------*
FORM muestro_operaciones .

  DATA: tl_sort TYPE slis_t_sortinfo_alv.

  REFRESH t_fieltotal.
  SORT t_operaciones BY j_1afitp descrip rate.
  PERFORM init_fieldcat USING t_fieltotal[].
  PERFORM f_init_sort_tot CHANGING tl_sort.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = layout
      it_fieldcat        = t_fieltotal[]
      i_save             = 'X'
      it_events          = t_events[]
      it_sort            = tl_sort
    TABLES
      t_outtab           = t_operaciones[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM. " MUESTRO_OPERACIONES
*&---------------------------------------------------------------------*
*&      Form  IMPRESION_SMARTFORMS
*&---------------------------------------------------------------------*
FORM impresion_smartforms .
  DATA: vl_ans       TYPE char1   .
  PERFORM f_popup_to_decide CHANGING vl_ans.
  IF vl_ans EQ 1.
    PERFORM f_llama_smartforms.
  ENDIF.
ENDFORM. " IMPRESION_SMARTFORMS
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_DECIDE
*&---------------------------------------------------------------------*
FORM f_popup_to_decide CHANGING po_ans.

  CALL FUNCTION 'POPUP_TO_DECIDE'
    EXPORTING
      defaultoption  = '1'
      textline1      = 'Se imprimirá el formulario'
      textline2      = '¿Desea continuar?'
      text_option1   = 'Si'
      text_option2   = 'No'
      titel          = ''
      start_column   = 25
      start_row      = 6
      cancel_display = ' '
    IMPORTING
      answer         = po_ans.
ENDFORM. " F_POPUP_TO_DECIDE
*&---------------------------------------------------------------------*
*&      Form  F_LLAMA_SMARTFORMS
*&---------------------------------------------------------------------*
FORM f_llama_smartforms .

  DATA: v_formulario(30)        TYPE c,
        v_flag                  TYPE c,
        v_nombre_modulo_funcion TYPE rs38l_fnam,
        v_active                TYPE tdbool,

        vl_imp                  TYPE p DECIMALS 2,
        vl_subtot_grav          TYPE p DECIMALS 2,
        vl_subtot_ngrav         TYPE p DECIMALS 2,
        vl_subtot_exent         TYPE p DECIMALS 2,
        vl_total_iva            TYPE p DECIMALS 2,
        vl_tot_ivat             TYPE p DECIMALS 2,
        vl_tot_itaxed           TYPE p DECIMALS 2,
        vl_tot_inot_taxed       TYPE p DECIMALS 2,
        vl_tot_iexemption       TYPE p DECIMALS 2,
        vl_tot_grav             TYPE p DECIMALS 2,
        vl_tot_ngrav            TYPE p DECIMALS 2,
        vl_tot_exent            TYPE p DECIMALS 2,
        vl_tot_piibb            TYPE p DECIMALS 2,
        vl_tot_piva             TYPE p DECIMALS 2,
        vl_ult_reg              TYPE char1,
        vl_new_fitp             TYPE char1,

        tl_posicion             TYPE zfiyt_libro_iva_posiciones,
        stl_posicion            TYPE zfiys_libro_iva_posiciones,
        st_libro_iva            TYPE zfiys_libro_iva,
        st_totalalicuota        TYPE zfiys_libro_iva_totales,
        st_suma                 TYPE zfiys_libro_iva_posiciones,
        st_suma_ind             TYPE zfiys_libro_iva_totales_ind,
        stl_newoptions          TYPE itcpo,

        tl_bases                TYPE zfiyt_libro_iva_totales_bases,
        tl_operaciones          TYPE zfiyt_libro_iva_tot_operacion,
        tl_periibb              TYPE zfiyt_libro_iva_percep_iibb,
        tl_periva               TYPE zfiyt_libro_iva_percep_iva,

        stl_bases               TYPE zfiys_libro_iva_totales_bases,
        stl_operaciones         TYPE zfiys_libro_iva_tot_operacion,
        stl_periibb             TYPE zfiys_libro_iva_percep_iibb,
        stl_periva              TYPE zfiys_libro_iva_percep_iva,
        el_bkpf                 TYPE bkpf,

        tl_indicdor             TYPE zfiyt_libro_iva_totales_indic,
        st_indicdor             TYPE zfiys_libro_iva_totales,
        stl_print_format        TYPE stxh-tdtexttype.

  TABLES itcpo.
  CONSTANTS: c_x          TYPE c VALUE 'X'.
  CONSTANTS: c_printer(7) TYPE c VALUE 'PRINTER'.

  IMPORT itcpo FROM MEMORY ID 'RFFORI01_ITCPO'.

* Seteo de la impresora
  MOVE-CORRESPONDING itcpo TO v_ssfcompop.

* Salida inmediata
  v_ssfcompop-tdimmed   = c_x.


  MOVE 'ZFIY0015'  TO v_formulario.

  PERFORM ssf_status_info USING v_formulario
                       CHANGING v_active.


  IF v_active IS INITIAL.
    STOP.
  ELSE.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = v_formulario
      IMPORTING
        fm_name            = v_nombre_modulo_funcion
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

* Si se produce un error, reportarlo.
    IF sy-subrc <> 0.
      PERFORM f_message USING  sy-msgid sy-msgty sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CLEAR: v_ssfctrlop,
           v_ssfcompop.
* LLamo al modulo de funciones para la impresion
    PERFORM f_get_text_print_parameters CHANGING stl_newoptions
                                                 stl_print_format.

    MOVE-CORRESPONDING: stl_newoptions TO v_ssfctrlop,
                        stl_newoptions TO v_ssfcompop.
* Seteo para que no muestre el dialogo
    v_ssfctrlop-no_dialog = 'X'.
    IF sy-ucomm EQ 'PREV'.
      v_ssfctrlop-preview = 'X'.
    ENDIF.


    PERFORM f_lleno_estructura_cabe CHANGING st_libro_iva.
*    Busco nombre del documento (FC - ND - NC )
    REFRESH t_tipo_doc.
    CLEAR   t_tipo_doc.
*    PERFORM f_armo_tabla_documento.

* --- Begin ---- GGU -----
    CLEAR st_totales.
* --- End   ---- GGU -----

    LOOP AT t_alv.
*     READ TABLE t_tipo_doc WITH KEY blart = t_alv-blart.
      MOVE: t_alv-budat         TO stl_posicion-budat,
            t_alv-belnr         TO stl_posicion-belnr,
            t_alv-koart         TO stl_posicion-koart,
            t_alv-hkont         TO stl_posicion-hkont,
            t_alv-name1         TO stl_posicion-name1,
            t_alv-stcd1         TO stl_posicion-stcd1,
            t_alv-bldat         TO stl_posicion-bldat,
            t_alv-tipo_doc(2)   TO stl_posicion-blart.
      CONCATENATE t_alv-bldat+6(4) t_alv-bldat+3(2) t_alv-bldat(2)
        INTO stl_posicion-bldat_d8.

      IF t_alv-blart = 'GB' OR t_alv-blart = 'KX'.
        stl_posicion-blart = t_alv-tipo_doc(2).
      ENDIF.

      READ TABLE t_bkpf
        INTO el_bkpf
        WITH KEY belnr = t_alv-belnr.
      IF sy-subrc = 0.
        IF  t_alv-blart EQ 'KA'.
          MOVE t_alv-belnr         TO stl_posicion-xblnr.
        ELSE.
          stl_posicion-xblnr = el_bkpf-xblnr.
        ENDIF.
      ENDIF.
      CLEAR t_alv-total.
      t_alv-total         = "t_alv-rate          +
                            t_alv-itaxed        +
                            t_alv-inot_taxed    +
                            t_alv-ivat          +
                            t_alv-irnr_vat      +
                            t_alv-ivat_percep   +
                            t_alv-iother_percep +
                            t_alv-iibb          +
* --- Begin ---- GGU -----
                            t_alv-iearn_per     +
* --- End   ---- GGU -----
                            t_alv-iexemption    .

      WRITE: t_alv-total         TO stl_posicion-total         RIGHT-JUSTIFIED,
             t_alv-rate          TO stl_posicion-rate          RIGHT-JUSTIFIED,
             t_alv-itaxed        TO stl_posicion-itaxed        RIGHT-JUSTIFIED,
             t_alv-inot_taxed    TO stl_posicion-inot_taxed    RIGHT-JUSTIFIED,
             t_alv-ivat          TO stl_posicion-ivat          RIGHT-JUSTIFIED,
             t_alv-irnr_vat      TO stl_posicion-irnr_vat      RIGHT-JUSTIFIED,
             t_alv-ivat_percep   TO stl_posicion-ivat_percep   RIGHT-JUSTIFIED,
             t_alv-iother_percep TO stl_posicion-iother_percep RIGHT-JUSTIFIED,
* --- Begin ---- GGU -----
             t_alv-iearn_per     TO stl_posicion-iearn_per     RIGHT-JUSTIFIED,
* --- End   ---- GGU -----
             t_alv-iibb          TO stl_posicion-iibb          RIGHT-JUSTIFIED,
             t_alv-iexemption    TO stl_posicion-iexemption    RIGHT-JUSTIFIED.

      APPEND stl_posicion TO tl_posicion.
*----------Armo la estructura del total de las posiciones
      CLEAR stl_posicion.
      st_totales-total         = t_alv-total         + st_totales-total        .
      st_totales-itaxed        = t_alv-itaxed        + st_totales-itaxed       .
      st_totales-inot_taxed    = t_alv-inot_taxed    + st_totales-inot_taxed   .
      st_totales-ivat          = t_alv-ivat          + st_totales-ivat         .
      st_totales-irnr_vat      = t_alv-irnr_vat      + st_totales-irnr_vat     .
      st_totales-ivat_percep   = t_alv-ivat_percep   + st_totales-ivat_percep  .
      st_totales-iother_percep = t_alv-iother_percep + st_totales-iother_percep.
* --- Begin ---- GGU -----
      st_totales-iearn_per    = t_alv-iearn_per    + st_totales-iearn_per   .
* --- End   ---- GGU -----
      st_totales-iibb          = t_alv-iibb          + st_totales-iibb         .
      st_totales-iexemption    = t_alv-iexemption    + st_totales-iexemption   .
      st_totales-koart         = t_alv-koart                                   .
    ENDLOOP.
    WRITE:
           st_totales-total         TO st_suma-total         RIGHT-JUSTIFIED,
           st_totales-itaxed        TO st_suma-itaxed        RIGHT-JUSTIFIED,
           st_totales-inot_taxed    TO st_suma-inot_taxed    RIGHT-JUSTIFIED,
           st_totales-ivat          TO st_suma-ivat          RIGHT-JUSTIFIED,
           st_totales-irnr_vat      TO st_suma-irnr_vat      RIGHT-JUSTIFIED,
           st_totales-ivat_percep   TO st_suma-ivat_percep   RIGHT-JUSTIFIED,
           st_totales-iother_percep TO st_suma-iother_percep RIGHT-JUSTIFIED,
* --- Begin ---- GGU -----
           st_totales-iearn_per    TO st_suma-iearn_per    RIGHT-JUSTIFIED,
* --- End   ---- GGU -----
           st_totales-iibb          TO st_suma-iibb          RIGHT-JUSTIFIED,
           st_totales-iexemption    TO st_suma-iexemption    RIGHT-JUSTIFIED,
           st_totales-koart         TO st_suma-koart.
* Tabla de indicadores
    CLEAR: vl_total_iva, vl_ult_reg, st_suma_ind.
    LOOP AT t_iva.

      AT LAST.
        vl_ult_reg = 'X'.
      ENDAT.

      CLEAR st_indicdor.
      CONCATENATE t_iva-descrip
                  t_iva-rate  '%'
      INTO st_indicdor-descripcion SEPARATED BY space.
      WRITE t_iva-ivat  TO st_indicdor-importe LEFT-JUSTIFIED.
      APPEND st_indicdor TO tl_indicdor .
      vl_total_iva = vl_total_iva + t_iva-ivat.

      IF vl_ult_reg = 'X'.
        WRITE vl_total_iva  TO st_suma_ind-total_iva LEFT-JUSTIFIED.
      ENDIF.

    ENDLOOP.

    CLEAR: vl_tot_ivat, vl_tot_itaxed, vl_tot_inot_taxed, vl_tot_iexemption, vl_ult_reg.
    LOOP AT t_bases.

      AT LAST.
        vl_ult_reg = 'X'.
      ENDAT.

      WRITE:  t_bases-descrip      TO  stl_bases-descrip    LEFT-JUSTIFIED ,
              t_bases-rate         TO  stl_bases-rate       LEFT-JUSTIFIED,
              t_bases-ivat         TO  stl_bases-ivat       LEFT-JUSTIFIED,
              t_bases-itaxed       TO  stl_bases-itaxed     LEFT-JUSTIFIED,
              t_bases-inot_taxed   TO  stl_bases-inot_taxed LEFT-JUSTIFIED,
              t_bases-iexemption   TO  stl_bases-iexemption LEFT-JUSTIFIED.
      APPEND stl_bases TO tl_bases.

      vl_tot_ivat = vl_tot_ivat + t_bases-ivat.
      vl_tot_itaxed = vl_tot_itaxed + t_bases-itaxed.
      vl_tot_inot_taxed = vl_tot_inot_taxed + t_bases-inot_taxed.
      vl_tot_iexemption = vl_tot_iexemption + t_bases-iexemption.

      IF vl_ult_reg = 'X'.
        WRITE: vl_tot_ivat         TO  st_suma_ind-total_ivat       LEFT-JUSTIFIED,
               vl_tot_itaxed       TO  st_suma_ind-total_itaxed     LEFT-JUSTIFIED,
               vl_tot_inot_taxed   TO  st_suma_ind-total_inot_taxed LEFT-JUSTIFIED,
               vl_tot_iexemption   TO  st_suma_ind-total_iexemption LEFT-JUSTIFIED.
      ENDIF.

    ENDLOOP.

    CLEAR: vl_tot_grav, vl_tot_ngrav, vl_tot_exent, vl_ult_reg, vl_subtot_grav, vl_subtot_ngrav,
           vl_subtot_exent, vl_new_fitp.
    LOOP AT t_operaciones.

      AT END OF j_1afitp.
        vl_new_fitp = 'X'.
      ENDAT.

      AT LAST.
        vl_ult_reg = 'X'.
      ENDAT.

      WRITE: t_operaciones-j_1afitp  TO stl_operaciones-j_1afitp  RIGHT-JUSTIFIED,
             t_operaciones-rate      TO stl_operaciones-rate      LEFT-JUSTIFIED,
             t_operaciones-descrip   TO stl_operaciones-descrip   LEFT-JUSTIFIED,
             t_operaciones-imp_grav  TO stl_operaciones-imp_grav  LEFT-JUSTIFIED,
             t_operaciones-imp_ngrav TO stl_operaciones-imp_ngrav LEFT-JUSTIFIED,
             t_operaciones-imp_exent TO stl_operaciones-imp_exent LEFT-JUSTIFIED.
      APPEND stl_operaciones TO tl_operaciones.

      vl_subtot_grav  = vl_subtot_grav  + t_operaciones-imp_grav.
      vl_subtot_ngrav = vl_subtot_ngrav + t_operaciones-imp_ngrav.
      vl_subtot_exent = vl_subtot_exent + t_operaciones-imp_exent.

      IF vl_new_fitp = 'X'.
        WRITE: t_operaciones-j_1afitp TO stl_operaciones-j_1afitp RIGHT-JUSTIFIED,
               t_operaciones-rate     TO stl_operaciones-rate     LEFT-JUSTIFIED,
               'Subtotal'             TO stl_operaciones-descrip  LEFT-JUSTIFIED,
               vl_subtot_grav         TO stl_operaciones-imp_grav  LEFT-JUSTIFIED,
               vl_subtot_ngrav        TO stl_operaciones-imp_ngrav LEFT-JUSTIFIED,
               vl_subtot_exent        TO stl_operaciones-imp_exent LEFT-JUSTIFIED.

        APPEND stl_operaciones TO tl_operaciones.
        vl_tot_grav  = vl_tot_grav  + vl_subtot_grav.
        vl_tot_ngrav = vl_tot_ngrav + vl_subtot_ngrav.
        vl_tot_exent = vl_tot_exent + vl_subtot_exent.
        CLEAR: vl_new_fitp, vl_subtot_grav, vl_subtot_ngrav, vl_subtot_exent.
      ENDIF.

      IF vl_ult_reg = 'X'.
        WRITE vl_tot_grav  TO st_suma_ind-total_opr_grav  LEFT-JUSTIFIED.
        WRITE vl_tot_ngrav TO st_suma_ind-total_opr_ngrav LEFT-JUSTIFIED.
        WRITE vl_tot_exent TO st_suma_ind-total_opr_exent LEFT-JUSTIFIED.
      ENDIF.

    ENDLOOP.

    CLEAR: vl_tot_piibb, vl_ult_reg.
    LOOP AT t_periibb.

      AT LAST.
        vl_ult_reg = 'X'.
      ENDAT.

      CONCATENATE '(' t_periibb-hkont ')' t_periibb-descrip
             INTO stl_periibb-descrip SEPARATED BY space.
      MOVE t_periibb-bezei TO stl_periibb-bezei.
      WRITE t_periibb-imp  TO stl_periibb-imp LEFT-JUSTIFIED.
      APPEND stl_periibb   TO  tl_periibb.
      vl_tot_piibb = vl_tot_piibb + t_periibb-imp.

      IF vl_ult_reg = 'X'.
        WRITE vl_tot_piibb TO st_suma_ind-total_piibb LEFT-JUSTIFIED.
      ENDIF.

    ENDLOOP.

    CLEAR: vl_tot_piva, vl_ult_reg.
    LOOP AT t_periva.

      AT LAST.
        vl_ult_reg = 'X'.
      ENDAT.

      CONCATENATE '(' t_periva-hkont ')' t_periva-descrip
             INTO stl_periva-descrip SEPARATED BY space.
      stl_periva-rate = t_periva-kbetr.
      WRITE t_periva-imp  TO stl_periva-imp LEFT-JUSTIFIED.
      CONDENSE stl_periva-imp NO-GAPS.
      APPEND stl_periva  TO  tl_periva.
      vl_tot_piva = vl_tot_piva + t_periva-imp.

      IF vl_ult_reg = 'X'.
        WRITE vl_tot_piva TO st_suma_ind-total_piva LEFT-JUSTIFIED.
      ENDIF.

    ENDLOOP.
*----------------------------------------------------*
*                  LIBRO IVA
*----------------------------------------------------*
* Enviar datos al formulario e imprimirlo.
    DATA:
      vl_mes     TYPE char2,
      vl_periodo TYPE char2.

    MOVE st_libro_iva-periodo TO vl_mes.

    PERFORM f_periodo USING vl_mes CHANGING vl_periodo.

    MOVE vl_periodo TO st_libro_iva-periodo.

    CALL FUNCTION v_nombre_modulo_funcion
      EXPORTING
        control_parameters  = v_ssfctrlop
        output_options      = v_ssfcompop
        user_settings       = ' '
        v_libro_iva         = 'X'
        cabecera            = st_libro_iva
        totales             = st_suma
        totales_indicadores = st_suma_ind
      TABLES
        t_posicion          = tl_posicion
        t_indicador         = tl_indicdor
        t_bases             = tl_bases
        t_operaciones       = tl_operaciones
        t_periibb           = tl_periibb
        t_periva            = tl_periva
      EXCEPTIONS
        formatting_error    = 1
        internal_error      = 2
        send_error          = 3
        user_canceled       = 4
        OTHERS              = 5.

* Si se produce un error, reportarlo.
    IF sy-subrc <> 0.
      PERFORM f_message USING  sy-msgid sy-msgty sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


**----------------------------------------------------*
**                  TOTALES
**----------------------------------------------------*
** Enviar datos al formulario e imprimirlo.
*    CLEAR   st_totales.
*    REFRESH tl_posicion.
*    CALL FUNCTION v_nombre_modulo_funcion
*      EXPORTING
*        control_parameters  = v_ssfctrlop
*        output_options      = v_ssfcompop
*        user_settings       = 'X'
*        v_libro_iva         = ' '
*        cabecera            = st_libro_iva
*        totales             = st_suma
*        totales_indicadores = st_suma_ind
*      TABLES
*        t_posicion          = tl_posicion
*        t_indicador         = tl_indicdor
*        t_bases             = tl_bases
*        t_operaciones       = tl_operaciones
*        t_periibb           = tl_periibb
*        t_periva            = tl_periva
*      EXCEPTIONS
*        formatting_error    = 1
*        internal_error      = 2
*        send_error          = 3
*        user_canceled       = 4
*        OTHERS              = 5.
*
** Si se produce un error, reportarlo.
*    IF sy-subrc <> 0.
*      PERFORM f_message USING  sy-msgid sy-msgty sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

  ENDIF.


ENDFORM. " F_LLAMA_SMARTFORMS

*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_fieldcat USING pi_nombre
                       pi_campo
                       pi_long
                       pi_vista
                       p_x.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

  ls_fieldcat-seltext_s = pi_nombre.
  ls_fieldcat-seltext_m = pi_nombre.
  ls_fieldcat-seltext_l = pi_nombre.
  ls_fieldcat-fieldname = pi_campo.
  ls_fieldcat-outputlen = pi_long.
  ls_fieldcat-no_out    = pi_vista.
  ls_fieldcat-just      = 'R'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-do_sum    = p_x.
  .

  APPEND ls_fieldcat TO t_fieltotal.

  CLEAR ls_fieldcat.
ENDFORM. " F_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  F_INIT_LAYOUT
*&---------------------------------------------------------------------*
FORM f_init_layout .

  layout-zebra               = 'X'.

ENDFORM. " F_INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SSF_STATUS_INFO
*&---------------------------------------------------------------------*
FORM ssf_status_info USING pi_formulario
                      CHANGING po_active.
  CALL FUNCTION 'SSF_STATUS_INFO'
    EXPORTING
      i_formname = pi_formulario
    IMPORTING
      o_active   = po_active.
ENDFORM. " SSF_STATUS_INFO
*&---------------------------------------------------------------------*
*&      Form  F_MESSAGE
*&---------------------------------------------------------------------*
FORM f_message USING pi_msgid TYPE sy-msgid
                         pi_msgty TYPE sy-msgty
                         pi_msgno TYPE sy-msgno
                         pi_msgv1 TYPE sy-msgv1
                         pi_msgv2 TYPE sy-msgv2
                         pi_msgv3 TYPE sy-msgv3
                         pi_msgv4 TYPE sy-msgv4.
  MESSAGE ID pi_msgid
      TYPE pi_msgty
      NUMBER pi_msgno
      WITH pi_msgv1 pi_msgv2 pi_msgv3 pi_msgv4.
ENDFORM. " F_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  F_GET_TEXT_PRINT_PARAMETERS
*&---------------------------------------------------------------------*
FORM f_get_text_print_parameters CHANGING po_newoptions TYPE itcpo
                                           po_print_format TYPE stxh-tdtexttype.

  CALL FUNCTION 'GET_TEXT_PRINT_PARAMETERS'
    IMPORTING
      newoptions    = po_newoptions
      print_format  = po_print_format
    EXCEPTIONS
      canceled      = 1
      archive_error = 2
      device        = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " F_GET_TEXT_PRINT_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  F_LLENO_ESTRUCTURA_CABE
*&---------------------------------------------------------------------*
FORM f_lleno_estructura_cabe CHANGING po_libro_iva TYPE zfiys_libro_iva.
  DATA: vl_fecha      TYPE char15,
        vl_street     TYPE adrc-street,
        vl_city1      TYPE adrc-city1,
        vl_country    TYPE adrc-country,
        vl_house_num1 TYPE adrc-house_num1,
        vl_adrnr      TYPE t001-adrnr,
        vl_landx50    TYPE t005t-landx50.

  CLEAR vl_fecha.
  WRITE: br_budat-low      TO vl_fecha  .
  CONCATENATE 'Desde:'
              vl_fecha
        INTO po_libro_iva-desde SEPARATED BY space.

  CLEAR vl_fecha.
  WRITE: br_budat-high     TO vl_fecha  .
  CONCATENATE 'Hasta:'
              vl_fecha
        INTO po_libro_iva-hasta SEPARATED BY space.

  MOVE: br_bukrs-low      TO po_libro_iva-bukrs  ,
        br_budat-low+4(2) TO po_libro_iva-periodo.

  IF r_cust EQ 'X'.
    po_libro_iva-titulo = 'SUBDIARIO DE I.V.A VENTAS'.
  ELSE.
    po_libro_iva-titulo = 'SUBDIARIO DE I.V.A COMPRAS'.
  ENDIF.

* Razón socual del iecsa
  SELECT SINGLE butxt adrnr
  FROM t001
  INTO (po_libro_iva-razonsocial, vl_adrnr)
  WHERE bukrs IN br_bukrs.

  CLEAR vl_fecha.
* Numero de cuit
  SELECT SINGLE paval
  FROM t001z
  INTO vl_fecha
  WHERE bukrs IN br_bukrs
  AND   party EQ 'J1AIDN'.
  CONCATENATE    'CUIT Nº:'
                 vl_fecha
            INTO po_libro_iva-cuit SEPARATED BY space.

* direccion
  SELECT SINGLE street city1 country house_num1
    FROM adrc
    INTO (vl_street, vl_city1, vl_country, vl_house_num1)
    WHERE addrnumber EQ vl_adrnr.

  SELECT SINGLE landx50
  FROM t005t
  INTO vl_landx50
  WHERE spras EQ  sy-langu
    AND land1 EQ vl_country.

  CONCATENATE vl_street vl_house_num1
  INTO po_libro_iva-direccion SEPARATED BY space..

  CONCATENATE vl_city1 vl_landx50
  INTO po_libro_iva-localidad SEPARATED BY space.

ENDFORM. " F_LLENO_ESTRUCTURA_CABE
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_DOCUMENTO
*&---------------------------------------------------------------------*

FORM f_armo_tabla_documento .
  DATA: tl_t003      TYPE STANDARD TABLE OF t003_i,
        tl_1aotdetr  TYPE STANDARD TABLE OF j_1aotdetr,
        tl_1aoftpt   TYPE STANDARD TABLE OF j_1aoftpt,
        stl_t003     TYPE t003_i,
        stl_1aotdetr TYPE j_1aotdetr,
        stl_1aoftpt  TYPE j_1aoftpt.

*tipo de documeno
  SELECT *
  FROM t003_i
  INTO TABLE tl_t003
  FOR ALL ENTRIES IN t_alv
  WHERE blart EQ t_alv-blart
    AND land1 EQ 'AR'.
*Determinación de clase de documento oficial
  SELECT *
  FROM j_1aotdetr
  INTO TABLE tl_1aotdetr
  FOR ALL ENTRIES IN tl_t003
  WHERE doccls    = tl_t003-doccls
    AND id_report = 'J_1AF016'
    AND land1     = 'AR'.
*Descripcion del documento
  SELECT *
  FROM j_1aoftpt
  INTO TABLE tl_1aoftpt
  FOR ALL ENTRIES IN tl_1aotdetr
  WHERE j_1aoftp  = tl_1aotdetr-j_1aoftp
    AND spras     =  'S'.

  LOOP AT tl_t003 INTO stl_t003.
    LOOP AT tl_1aotdetr INTO stl_1aotdetr
    WHERE doccls    = stl_t003-doccls.
      LOOP AT tl_1aoftpt INTO stl_1aoftpt
      WHERE j_1aoftp  = stl_1aotdetr-j_1aoftp.
        CLEAR t_tipo_doc.
        MOVE: stl_t003-blart          TO t_tipo_doc-blart     ,
              stl_t003-doccls         TO t_tipo_doc-doccls    ,
              stl_1aotdetr-j_1aprtchr TO t_tipo_doc-j_1aprtchr,
              stl_1aotdetr-j_1aoftp   TO t_tipo_doc-j_1aoftp  ,
              stl_1aoftpt-text5       TO t_tipo_doc-text5     .

        APPEND t_tipo_doc.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


ENDFORM. " F_ARMO_TABLA_DOCUMENTO
*&---------------------------------------------------------------------*
*&      Form  F_TOTALES
*&---------------------------------------------------------------------*
FORM f_totales .
  DATA: BEGIN OF tl_aux OCCURS 0,
          belnr(10)        TYPE c,
          mwskz(6)         TYPE c,
          lineno(6)        TYPE c,
          budat(10)        TYPE c,
          brnch            LIKE bkpf-brnch,
          koart            LIKE bseg-koart,
          hkont(10)        TYPE c,
          name1            LIKE lfa1-name1,
          stcdt            LIKE lfa1-stcdt,
          stcd1            LIKE lfa1-stcd1,
          bldat(10)        TYPE c,
          xblnr(14)        TYPE c,
          oftp_text        LIKE j_1aoftpt-text5,
          augdt(8)         TYPE c,
          augbl            LIKE bseg-augbl,
          total            TYPE dmbtr,
          rate(8)          TYPE c,
          taxed(18)        TYPE c,
          not_taxed(18)    TYPE c,
          vat(19)          TYPE c,
          rnr_vat(18)      TYPE c,
          vat_percep(18)   TYPE c,
          other_percep(18) TYPE c,
          exemption(19)    TYPE c,
          itaxed           TYPE p DECIMALS 2,
          inot_taxed       TYPE p DECIMALS 2,
          ivat             TYPE p DECIMALS 2,
          irnr_vat         TYPE p DECIMALS 2,
          ivat_percep      TYPE p DECIMALS 2,
          iother_percep    TYPE p DECIMALS 2,
          iother_percep02  TYPE p DECIMALS 2,
          iother_percep03  TYPE p DECIMALS 2,
          iibb             TYPE p DECIMALS 2,
          iexemption       TYPE p DECIMALS 2,
* --- Begin ---- GGU -----
          iearn_per        TYPE p DECIMALS 2,
* --- End   ---- GGU -----
          exports(18)      TYPE c,
          percepnoc(18)    TYPE c,
          line_total(19)   TYPE c,
          codmon(15)       TYPE c,
          tpcbio(18)       TYPE c,
          vaiva(18)        TYPE c,
          isalta           TYPE p DECIMALS 2,
          icorrien         TYPE p DECIMALS 2,
          ichaco           TYPE p DECIMALS 2,
          irione           TYPE p DECIMALS 2,
          isanlu           TYPE p DECIMALS 2,
          itucum           TYPE p DECIMALS 2,
          ibsas            TYPE p DECIMALS 2,
          istafe           TYPE p DECIMALS 2,
          icapi            TYPE p DECIMALS 2,
          isanju           TYPE p DECIMALS 2,
          lifnr            TYPE lifnr,
          blart            TYPE bkpf-blart,
          flag,
          kschl            TYPE bset-kschl,
          ktosl            TYPE bset-ktosl,
          fityp            TYPE lfa1-fityp,
          tipo_doc         TYPE text5,
        END OF tl_aux.

  DATA: BEGIN OF tl_alv OCCURS 0,
          belnr(10)        TYPE c,
          mwskz(6)         TYPE c,
          lineno(6)        TYPE c,
          budat(10)        TYPE c,
          gjahr            TYPE bkpf-gjahr,
          brnch            LIKE bkpf-brnch,
          koart            LIKE bseg-koart,
          hkont(10)        TYPE c,
          name1            LIKE lfa1-name1,
          stcdt            LIKE lfa1-stcdt,
          stcd1            LIKE lfa1-stcd1,
          bldat(10)        TYPE c,
          xblnr(14)        TYPE c,
          oftp_text        LIKE j_1aoftpt-text5,
          augdt(8)         TYPE c,
          augbl            LIKE bseg-augbl,
          total            TYPE dmbtr,
          rate(8)          TYPE c,
          taxed(18)        TYPE c,
          not_taxed(18)    TYPE c,
          vat(19)          TYPE c,
          rnr_vat(18)      TYPE c,
          vat_percep(18)   TYPE c,
          other_percep(18) TYPE c,
          exemption(19)    TYPE c,
          itaxed           TYPE p DECIMALS 2,
          inot_taxed       TYPE p DECIMALS 2,
          ivat             TYPE p DECIMALS 2,
          irnr_vat         TYPE p DECIMALS 2,
          ivat_percep      TYPE p DECIMALS 2,
          iother_percep    TYPE p DECIMALS 2,
          iother_percep02  TYPE p DECIMALS 2,
          iother_percep03  TYPE p DECIMALS 2,
          iibb             TYPE p DECIMALS 2,
          iexemption       TYPE p DECIMALS 2,
* --- Begin ---- GGU -----
          iearn_per        TYPE p DECIMALS 2,
* --- End   ---- GGU -----
          exports(18)      TYPE c,
          percepnoc(18)    TYPE c,
          line_total(19)   TYPE c,
          codmon(15)       TYPE c,
          tpcbio(18)       TYPE c,
          vaiva(18)        TYPE c,
          isalta           TYPE p DECIMALS 2,
          icorrien         TYPE p DECIMALS 2,
          ichaco           TYPE p DECIMALS 2,
          irione           TYPE p DECIMALS 2,
          isanlu           TYPE p DECIMALS 2,
          itucum           TYPE p DECIMALS 2,
          ibsas            TYPE p DECIMALS 2,
          istafe           TYPE p DECIMALS 2,
          icapi            TYPE p DECIMALS 2,
          isanju           TYPE p DECIMALS 2,
          lifnr            TYPE lifnr,
          blart            TYPE bkpf-blart,
          flag,
          kschl            TYPE bset-kschl,
          ktosl            TYPE bset-ktosl,
          fityp            TYPE lfa1-fityp,
          tipo_doc         TYPE text5,
        END OF tl_alv.
  DATA: tl_bset  TYPE STANDARD TABLE OF bset,
        stl_bset TYPE bset.

  DATA: stl_alv   LIKE t_alv,
        vl_tabix  TYPE sy-tabix,
        vl_tabix2 TYPE sy-tabix,
        vl_total  TYPE dmbtr.

  IF t_alv[] IS NOT INITIAL.
* Armo la tabla Percepcion de IIBB
    SELECT *
    FROM bset
    INTO TABLE tl_bset
    FOR ALL ENTRIES IN t_alv
    WHERE bukrs IN br_bukrs
    AND   belnr EQ t_alv-belnr
    AND   mwskz EQ t_alv-mwskz
    AND   gjahr EQ br_gjahr.


* Armo la tabla Percepcion de IIBB
    LOOP AT t_periibb.

      LOOP AT tl_bset INTO stl_bset
        WHERE hkont EQ t_periibb-hkont.
        IF   stl_bset-kschl NE 'MWVS'
* Inicio Modificación por Ariel M. Mascó el 10/1/2012
*        OR   stl_bset-kschl NE 'J1AY'.
        OR   stl_bset-kschl NE 'J1BG'
        OR   stl_bset-kschl NE 'J1BE'
        OR   stl_bset-kschl NE 'J1B7'
        OR   stl_bset-kschl NE 'J1AY'.
* Fin Modificación por Ariel M. Mascó el 10/1/2012
          IF stl_bset-shkzg EQ 'H'.
            stl_bset-hwste = stl_bset-hwste * - 1.
          ENDIF.

          t_periibb-imp = stl_bset-hwste + t_periibb-imp.
        ENDIF.
      ENDLOOP.
      COLLECT t_periibb.
    ENDLOOP.

*FIN Armo la tabla Percepcion de IIBB

* armo la tabla Percepciones de IVA
    REFRESH tl_bset.
    SORT t_alv BY belnr.
    SELECT *
     FROM bset
     INTO TABLE tl_bset
     FOR ALL ENTRIES IN t_alv
     WHERE bukrs IN br_bukrs
     AND   belnr EQ t_alv-belnr
     AND   mwskz EQ t_alv-mwskz
     AND   gjahr EQ br_gjahr.



    DATA: vl_kbetr  TYPE kbetr,
          vl_kbetrl TYPE char10.

    SORT tl_bset BY belnr.
    LOOP AT tl_bset INTO stl_bset
     WHERE ( ktosl EQ 'J1G'
     OR      ktosl EQ 'J14'
     OR      ktosl EQ 'J1F' ).

      SELECT SINGLE txt20
      FROM skat
      INTO  t_periva-descrip
      WHERE spras EQ 'S'
      AND   ktopl EQ 'INT'
      AND   saknr EQ stl_bset-hkont.

      IF stl_bset-shkzg EQ 'H'.
        stl_bset-hwste = stl_bset-hwste * - 1.
      ENDIF.

      t_periva-hkont    = stl_bset-hkont.
*---> 09/06/2023 - Migração S4 - JS
*        t_periva-imp      = stl_bset-hwste.
      t_periva-imp = CONV #( stl_bset-hwste ).
*<--- 09/06/2023 - Migração S4 - JS

      vl_kbetr          = stl_bset-kbetr / 10.

      WRITE vl_kbetr TO vl_kbetrl RIGHT-JUSTIFIED.

      CONCATENATE vl_kbetrl '%'
             INTO t_periva-kbetr.

      COLLECT t_periva.
      CLEAR  t_periva.

    ENDLOOP.


*FIN armo la tabla Percepciones de IVA

  ENDIF.

* --- Begin ---- GGU -----
* Se ingresa un nuevo indicador por si se quiere sacar el reporte con todos los indicadores.
  IF p_todos IS INITIAL.
* --- End   ---- GGU -----

    LOOP AT  t_alv
    WHERE ( kschl NE 'MWVS'
       AND  kschl NE 'J1AY').
*    MOVE-CORRESPONDING t_alv TO tl_alv.
      MOVE:       t_alv-belnr         TO tl_alv-belnr         ,
                  t_alv-ivat_percep   TO tl_alv-ivat_percep   ,"+ stl_alv-ivat_percep  .
                  t_alv-iother_percep TO tl_alv-iother_percep ,"+ stl_alv-iother_percep.
* --- Begin ---- GGU -----
                  t_alv-iearn_per     TO tl_alv-iearn_per ,
* --- End   ---- GGU -----
                  t_alv-iexemption    TO tl_alv-iexemption    .".+ stl_alv-iexemption   .


      COLLECT tl_alv.
      CLEAR tl_alv.
      MOVE-CORRESPONDING t_alv TO tl_aux.
      APPEND tl_aux.
    ENDLOOP.

    SORT t_alv  BY belnr ASCENDING.
    SORT tl_alv BY belnr ASCENDING  mwskz DESCENDING.

*  REFRESH t_alv.

    LOOP AT t_alv
        WHERE kschl NE 'MWVS'
          AND kschl NE 'J1AY'.
      DELETE t_alv INDEX sy-tabix.
    ENDLOOP.



    LOOP AT tl_alv.
      MOVE tl_alv-belnr  TO vl_belnr .
*        where belnr eq vl_belnr.
      vl_tabix2 = sy-tabix.
*        and   kschl NE 'MWVS'
*        and   mwskz ne stl_alv-mwskz.

      CLEAR: t_alv-itaxed        ,
             t_alv-inot_taxed    ,
             t_alv-ivat          ,
             t_alv-irnr_vat      .

      stl_alv-ivat_percep   = tl_alv-ivat_percep   ."+ stl_alv-ivat_percep  .
      stl_alv-iother_percep = tl_alv-iother_percep ."+ stl_alv-iother_percep.
* --- Begin ---- GGU -----
      stl_alv-iearn_per = tl_alv-iearn_per.
* --- End   ---- GGU -----
*            stl_alv-iexemption    = tl_alv-iexemption    .".+ stl_alv-iexemption   .

      LOOP AT t_alv
        WHERE belnr EQ vl_belnr.
        vl_tabix = sy-tabix.
* --- Begin ---- GGU -----
* Se modifica ya que se perdían algunos valores por no contemplar que tengan valor
*      MOVE:  stl_alv-ivat_percep   TO t_alv-ivat_percep   ,"+ stl_alv-ivat_percep  .
*            stl_alv-iother_percep TO t_alv-iother_percep ."+ stl_alv-iother_percep.
        t_alv-ivat_percep   = stl_alv-ivat_percep + t_alv-ivat_percep.
        t_alv-iother_percep = stl_alv-iother_percep + t_alv-iother_percep.
* --- Begin ---- GGU -----
        t_alv-iearn_per = stl_alv-iearn_per + t_alv-iearn_per.
* --- End   ---- GGU -----
* --- End   ---- GGU -----

*collect stl_alv  INTO t_alv.
        MODIFY t_alv INDEX sy-tabix.
        DELETE tl_alv INDEX vl_tabix2.
        CLEAR  vl_tabix2.
        EXIT.
      ENDLOOP.
    ENDLOOP.


    LOOP AT tl_alv.
      MOVE tl_alv-belnr  TO vl_belnr .
*        where belnr eq vl_belnr.
      vl_tabix2 = sy-tabix.
      CLEAR: t_alv-itaxed        ,
             t_alv-inot_taxed    ,
             t_alv-ivat          ,
             t_alv-irnr_vat      .

      stl_alv-ivat_percep   = tl_alv-ivat_percep   ."+ stl_alv-ivat_percep  .
      stl_alv-iother_percep = tl_alv-iother_percep ."+ stl_alv-iother_percep.
* --- Begin ---- GGU -----
      stl_alv-iearn_per = tl_alv-iearn_per.
* --- End   ---- GGU -----
*            stl_alv-iexemption    = tl_alv-iexemption    .".+ stl_alv-iexemption   .

      LOOP AT tl_aux
        WHERE belnr EQ vl_belnr.
        vl_tabix = sy-tabix.
        MOVE:  stl_alv-ivat_percep   TO t_alv-ivat_percep   ,"+ stl_alv-ivat_percep  .
* --- Begin ---- GGU -----
               stl_alv-iearn_per     TO t_alv-iearn_per,
* --- End   ---- GGU -----
               stl_alv-iother_percep TO t_alv-iother_percep ."+ stl_alv-iother_percep.
        IF tl_alv-lifnr EQ space.
*collect stl_alv  INTO t_alv.
          MOVE: tl_aux-belnr     TO t_alv-belnr,
                tl_aux-mwskz     TO t_alv-mwskz,
                tl_aux-lineno    TO t_alv-lineno,
                tl_aux-budat     TO t_alv-budat,
                tl_aux-budat(4)  TO t_alv-gjahr,
                tl_aux-brnch     TO t_alv-brnch,
                tl_aux-koart     TO t_alv-koart,
                tl_aux-hkont     TO t_alv-hkont,
                tl_aux-name1     TO t_alv-name1,
                tl_aux-stcdt     TO t_alv-stcdt,
                tl_aux-stcd1     TO t_alv-stcd1,
                tl_aux-bldat     TO t_alv-bldat,
                tl_aux-xblnr     TO t_alv-xblnr,
                tl_aux-oftp_text TO t_alv-oftp_text,
                tl_aux-augdt     TO t_alv-augdt,
                tl_aux-augbl     TO t_alv-augbl,
                tl_aux-lifnr     TO t_alv-lifnr,
                tl_aux-blart     TO t_alv-blart,
                tl_aux-kschl     TO t_alv-kschl,
                tl_aux-ktosl     TO t_alv-ktosl,
                tl_aux-fityp     TO t_alv-fityp,
                tl_aux-tipo_doc  TO t_alv-tipo_doc.
        ENDIF.



        APPEND t_alv ."INDEX sy-tabix.
        DELETE tl_alv INDEX vl_tabix2.
        CLEAR  vl_tabix2.
        EXIT.
      ENDLOOP.
    ENDLOOP.

    SORT t_alv BY budat belnr mwskz name1.

* --- Begin ---- GGU -----
  ENDIF.
* --- End   ---- GGU -----

*----------------------------------------------------------------------------------------------------------*
*  Muevo la tabla t_alv, para poder ordenarla por indicado y nro documento
*  para totalizar, uso una tabla auxiliar para no modificar los ordenes de
*  los campos en la tabla alv
  REFRESH tl_alv.
  CLEAR tl_alv.
  LOOP AT  t_alv .
    MOVE-CORRESPONDING t_alv TO tl_alv.
* --- End   ---- GGU -----
    IF NOT p_todos IS INITIAL AND
           tl_alv-kschl NE 'MWVS' AND
* Inicio Modificación por Ariel M. Mascó el 10/1/2012
*           tl_alv-kschl NE 'J1AY'.
           tl_alv-kschl NE 'J1BG' AND
           tl_alv-kschl NE 'J1BE' AND
           tl_alv-kschl NE 'J1B7' AND
           tl_alv-kschl NE 'J1AY'.
* Fin Modificación por Ariel M. Mascó el 10/1/2012
      CLEAR: tl_alv-itaxed        ,
             tl_alv-inot_taxed    ,
             tl_alv-ivat          ,
             tl_alv-irnr_vat      .
    ENDIF.
* --- End   ---- GGU -----
    APPEND tl_alv.
  ENDLOOP.

  SORT tl_alv BY belnr ASCENDING  mwskz DESCENDING.
  REFRESH t_alv.


*Armo el total y la tabla de salida del alv. t_alv
  LOOP AT tl_alv .
    MOVE-CORRESPONDING tl_alv  TO  stl_alv .
    vl_total =  stl_alv-itaxed
             +  stl_alv-inot_taxed
             +  stl_alv-ivat
             +  stl_alv-irnr_vat
             +  stl_alv-ivat_percep
             +  stl_alv-iother_percep
             +  stl_alv-iexemption
* --- Begin ---- GGU -----
             +  stl_alv-iearn_per
* --- End   ---- GGU -----
             +  vl_total.

    CLEAR: stl_alv-total  ,
           stl_alv-total .

    READ TABLE t_bset INTO bset
    WITH KEY belnr = tl_alv-belnr
             mwskz = tl_alv-mwskz.
    IF sy-subrc EQ 0.
      stl_alv-kschl = bset-kschl.
    ENDIF.

    MOVE-CORRESPONDING stl_alv TO t_alv.
    APPEND t_alv.
    vl_tabix = sy-tabix.


    AT END OF belnr.
      stl_alv-total = vl_total.
      MODIFY t_alv FROM stl_alv INDEX vl_tabix.
      CLEAR vl_total.
    ENDAT.
  ENDLOOP.

  SORT t_alv BY budat belnr mwskz name1.

ENDFORM. " F_TOTALES
*&---------------------------------------------------------------------*
*&      Form  MUESTRO_BASES
*&---------------------------------------------------------------------*
FORM muestro_bases .
  REFRESH t_fieltotal.
  PERFORM init_fieldcat USING t_fieltotal[].
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = layout
      it_fieldcat        = t_fieltotal[]
      i_save             = 'X'
      it_events          = t_events[]
    TABLES
      t_outtab           = t_bases[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM. " MUESTRO_BASES
*&---------------------------------------------------------------------*
*&      Form  F_BASES_IMPONIBLES
*&---------------------------------------------------------------------*
FORM f_bases_imponibles .

  LOOP AT t_alv
   WHERE mwskz EQ 'C1'
    OR   mwskz EQ 'C2'
    OR   mwskz EQ 'C3'
    OR   mwskz EQ 'C0'
    OR   mwskz EQ 'C9'.
    t_bases-descrip       =  'IVA'            .
    t_bases-rate          =  t_alv-rate       .
    t_bases-ivat          =  t_alv-ivat       .
    t_bases-itaxed        =  t_alv-itaxed     .
    t_bases-inot_taxed    =  t_alv-inot_taxed .
    t_bases-iexemption    =  t_alv-iexemption .
    COLLECT t_bases.
    CLEAR t_bases.
  ENDLOOP.

ENDFORM. " F_BASES_IMPONIBLES
*&---------------------------------------------------------------------*
*&      Form  F_PERCEP_IIBB
*&---------------------------------------------------------------------*
FORM f_percep_iibb .
  REFRESH t_periibb.

  SELECT *
  FROM zfiyt_perc_iibb
  INTO CORRESPONDING FIELDS OF TABLE t_periibb
  WHERE libro EQ 'C'.

  LOOP AT t_periibb.
    SELECT SINGLE bezei
    FROM t005u
    INTO t_periibb-bezei
    WHERE spras EQ 'S'
    AND   land1 EQ 'AR'
    AND   bland EQ t_periibb-bland.

    MODIFY t_periibb.
  ENDLOOP.
  SORT t_periibb BY bland.

ENDFORM. " F_PERCEP_IIBB

*&---------------------------------------------------------------------*
*&      Form  F_AGREGAR_EXEN_NOGRAV
*&---------------------------------------------------------------------*
FORM f_agregar_exen_nograv USING p_ktosl CHANGING p_rate.

  CASE p_ktosl.
    WHEN 'VST'.
      p_rate = 'Exento'.
    WHEN 'J1I'.
      p_rate = 'No Grav.'.
    WHEN OTHERS.
      CLEAR p_rate.
  ENDCASE.

ENDFORM. " F_AGREGAR_EXEN_NOGRAV

*&---------------------------------------------------------------------*
*&      Form  F_INIT_SORT_TOT
*&---------------------------------------------------------------------*
FORM f_init_sort_tot CHANGING p_tl_sort TYPE slis_t_sortinfo_alv.

  DATA le_sort TYPE slis_sortinfo_alv.

  le_sort-spos = 1.
  le_sort-fieldname = 'J_1AFITP'.
  le_sort-tabname   = 'T_OPERACIONES'.
  le_sort-subtot    = 'X'.
  le_sort-up        = 'X'.
  APPEND le_sort TO p_tl_sort.

ENDFORM. " F_INIT_SORT_TOT

*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_PROVEEDORES
*&---------------------------------------------------------------------*
FORM f_armo_tabla_proveedores .

  SELECT *
  FROM lfa1
  INTO TABLE t_lfa1
  FOR ALL ENTRIES IN t_alv
  WHERE lifnr EQ  t_alv-lifnr.
  IF sy-subrc EQ 0.
    SORT t_lfa1 BY lifnr.
  ENDIF.

ENDFORM. " F_ARMO_TABLA_PROVEEDORES

*&---------------------------------------------------------------------*
*&      Form  f_imp_gan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_imp_gan.
* LOOP AT t_alv.
*    IF t_alv-mwskz = 'CX'.
*      t_alv-iearn_per = t_alv-iother_percep.
*      CLEAR t_alv-iother_percep.
*      MODIFY t_alv.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    "f_imp_gan

*&---------------------------------------------------------------------*
*&      Form  F_AGREGAR_CAMPOS_ADIC
*&---------------------------------------------------------------------*
FORM f_agregar_campos_adic .


  DATA: v_xblnr LIKE bkpf-xblnr.
*Importes IIBB con indicador CX en columna ganancias
  PERFORM f_imp_gan.


  DATA: el_lfa1 TYPE lfa1.

  LOOP AT t_alv.

    IF t_alv-lifnr IS NOT INITIAL.
      READ TABLE t_lfa1 INTO el_lfa1
      WITH KEY lifnr = t_alv-lifnr.
      IF sy-subrc = 0.
        t_alv-fityp = el_lfa1-fityp.
      ENDIF.
    ENDIF.

    SELECT SINGLE xblnr
    INTO v_xblnr
      FROM bkpf
      WHERE belnr = t_alv-belnr.

    READ TABLE t_tipo_doc
      WITH KEY blart = t_alv-blart
               j_1aprtchr = v_xblnr+4(1).
    IF sy-subrc = 0.
      t_alv-tipo_doc = t_tipo_doc-text5.
    ELSE.
      PERFORM f_tipo_doc_anul.
    ENDIF.

    CASE t_alv-blart.
      WHEN 'AB' OR 'LM' OR 'SA'.
        LOOP AT tg_account_set.
          PERFORM f_atrib_vl_account USING t_alv tg_account_set-hkont.
        ENDLOOP.
    ENDCASE.

    MODIFY t_alv.

  ENDLOOP.

ENDFORM. " F_AGREGAR_CAMPOS_ADIC

*&---------------------------------------------------------------------*
*&      Form  F_TIPO_DOC_ANUL
*&---------------------------------------------------------------------*
FORM f_tipo_doc_anul.

  DATA: el_bkpf TYPE bkpf.

  CASE t_alv-blart.
    WHEN 'KX'.
      t_alv-tipo_doc = 'AC'.
    WHEN 'GB'.
* Obtengo los datos de cabecera del doc.
      READ TABLE t_bkpf
        INTO el_bkpf
        WITH KEY belnr = t_alv-belnr.
      CHECK sy-subrc = 0.

      IF el_bkpf-stblg IS INITIAL.
        t_alv-tipo_doc = 'GB'.
      ELSE.
        t_alv-tipo_doc = 'AB'.
      ENDIF.

    WHEN OTHERS.
      t_alv-tipo_doc = t_alv-blart.
  ENDCASE.

ENDFORM. " F_TIPO_DOC_ANUL
*&---------------------------------------------------------------------*
*&      Form  F_PERIODO
*&---------------------------------------------------------------------*
FORM f_periodo  USING    pi_mes
                CHANGING po_periodo.
  DATA  v_periv LIKE t001-periv.
  DATA v_periodo TYPE char3.

*    CASE pi_mes.
*    WHEN '07'.
*      po_periodo = '02'.
*    WHEN '08'.
*      po_periodo = '03'.
*    WHEN '09'.
*      po_periodo = '04'.
*    WHEN '10'.
*      po_periodo = '05'.
*    WHEN '11'.
*      po_periodo = '06'.
*    WHEN '12'.
*      po_periodo = '07'.
*    WHEN '01'.
*      po_periodo = '08'.
*    WHEN '02'.
*      po_periodo = '09'.
*    WHEN '03'.
*      po_periodo = '10'.
*    WHEN '04'.
*      po_periodo = '11'.
*    WHEN '05'.
*      po_periodo = '12'.
*    WHEN '06'.
*      po_periodo = '01'.
*  ENDCASE.

  SELECT SINGLE periv
  FROM t001
  INTO v_periv
  WHERE bukrs = br_bukrs-low.
  IF sy-subrc EQ 0.
    SELECT SINGLE poper
      FROM t009b
      INTO v_periodo
      WHERE periv = v_periv AND
            bumon = pi_mes.
  ENDIF.

  po_periodo = v_periodo+1(2).


ENDFORM.                    " F_PERIODO
*&---------------------------------------------------------------------*
*&      Form  READ_ACCOUNT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_account_set .

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZFIY0015'
    TABLES
      set_values    = t_set_account
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SORT t_set_account BY from.

  REFRESH tg_account_set.
  CLEAR: tg_account_set.

  LOOP AT t_set_account.
    IF ( t_set_account-from IS NOT INITIAL ).
      tg_account_set-hkont = t_set_account-from(10).
      APPEND tg_account_set.
    ENDIF.
    CLEAR: tg_account_set.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_RAZAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_armo_tabla_razao .

  IF ( tg_account_set[] IS NOT INITIAL ) AND ( t_alv[] IS NOT INITIAL ).

    SELECT *
      FROM bsas
      INTO TABLE t_bsas
     FOR ALL ENTRIES IN t_alv
     WHERE bukrs EQ br_bukrs-low
       AND belnr EQ t_alv-belnr
       AND gjahr EQ t_alv-gjahr.

    IF t_bsas[] IS NOT INITIAL.
      SORT t_bsas BY hkont.

      LOOP AT t_bsas.
        READ TABLE tg_account_set WITH KEY hkont = t_bsas-hkont.
        IF sy-subrc NE 0.
          DELETE t_bsas.
        ENDIF.
      ENDLOOP.

    ENDIF.

    SELECT *
      FROM bsis
      INTO TABLE t_bsis
     FOR ALL ENTRIES IN t_alv
     WHERE bukrs EQ br_bukrs-low
       AND belnr EQ t_alv-belnr
       AND gjahr EQ t_alv-gjahr.

    IF t_bsis[] IS NOT INITIAL.
      SORT t_bsis BY hkont.

      LOOP AT t_bsis.
        READ TABLE tg_account_set WITH KEY hkont = t_bsis-hkont.
        IF sy-subrc NE 0.
          DELETE t_bsis.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATRIB_VL_ACCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atrib_vl_account USING p_t_alv LIKE t_alv
                              p_hkont LIKE bsas-hkont.



  READ TABLE t_bsas WITH KEY belnr = p_t_alv-belnr
                             gjahr = p_t_alv-gjahr
                             hkont = p_hkont.
  IF sy-subrc = 0.

    CASE p_hkont.
      WHEN 113251.
*---> 09/06/2023 - Migração S4 - JS
*      WHEN 113252.
*        t_alv-iother_percep    = t_bsas-dmbtr.
*      WHEN 113253.
*        t_alv-iother_percep03  = t_bsas-dmbtr.
      WHEN 113252.
        t_alv-iother_percep   = CONV #( t_bsas-dmbtr ).
      WHEN 113253.
        t_alv-iother_percep03 = CONV #( t_bsas-dmbtr ).
*<--- 09/06/2023 - Migração S4 - JS
    ENDCASE.

  ELSE.

    READ TABLE t_bsis WITH KEY belnr = p_t_alv-belnr
                               gjahr = p_t_alv-gjahr
                               hkont = p_hkont.
    IF sy-subrc = 0.

      CASE p_hkont.
*---> 09/06/2023 - Migração S4 - JS
*        WHEN 113251.
*          t_alv-iother_percep02  = t_bsis-dmbtr.
*        WHEN 113252.
*          t_alv-iother_percep    = t_bsis-dmbtr.
*        WHEN 113253.
*          t_alv-iother_percep03  = t_bsis-dmbtr.
        WHEN 113251.
          t_alv-iother_percep02 = CONV #( t_bsis-dmbtr ).
        WHEN 113252.
          t_alv-iother_percep   = CONV #( t_bsis-dmbtr ).
        WHEN 113253.
          t_alv-iother_percep03  = CONV #( t_bsis-dmbtr ).
*<--- 09/06/2023 - Migração S4 - JS
      ENDCASE.

    ENDIF.

  ENDIF.

ENDFORM.

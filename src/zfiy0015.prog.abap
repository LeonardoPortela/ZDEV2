*&-----------------------------------------------------------------------*
*& Report: ZFIY0015                                              *
*& Transaccion: ZFIY0015                                      *
*& Copia Report: J_1AF205                                                *
*& Daily VAT Reporting for Argentina                                     *
*& This report will be used instead the old report J_1AF105              *
*&-----------------------------------------------------------------------*
REPORT ZFIY0015 NO STANDARD PAGE HEADING
*               LINE-SIZE 245
*               LINE-COUNT 57(3)
                MESSAGE-ID 8a.

INCLUDE ZFIY0015_top.
INCLUDE ZFIY0015_frm.

*----------------------------------------------------------------------*
* init selection screen
*----------------------------------------------------------------------*
INITIALIZATION.
  par_magn = ' '.
  PERFORM f_initialization.

*----------------------------------------------------------------------*
* input check for the selectionscreen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

* only 1 BUKRS is  obligatory
  DESCRIBE TABLE br_bukrs LINES tmp_cnt1.
  IF tmp_cnt1 NE  1                   OR
     br_bukrs-low IS INITIAL         OR
     ( br_bukrs-low NE br_bukrs-high AND
       NOT br_bukrs-high IS INITIAL ).
* enter one company code
    MESSAGE e821.
  ENDIF.
  IF br_budat-low EQ 0 AND br_budat-high EQ 0.              "1069346
    MESSAGE e116.
  ENDIF.

* Special checks for parameters of magnetic output
  IF NOT par_magn IS INITIAL.
    IF flg_change_sel IS INITIAL.
      MOVE p_col09 TO hlp_col9.
      MOVE p_col10 TO hlp_col10.
      MOVE hlp_col9m TO p_col09.
      MOVE hlp_col10m TO p_col10.
      flg_change_sel = 'X'.
    ENDIF.
    IF par_comp IS INITIAL.
* Magnetic output needs compressed data.
      MESSAGE e453.
    ENDIF.
* Open dataset for magnetic output - if given
    IF NOT par_file IS INITIAL.
      CLOSE DATASET par_file.
      OPEN DATASET par_file FOR OUTPUT IN TEXT MODE
                   ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.
* Local file can only be written if name is specified
      IF NOT par_file IS INITIAL AND
         ( par_lfil IS INITIAL AND NOT par_loc IS INITIAL ).
        MESSAGE e454.
      ENDIF.
    ELSE.
      IF NOT par_loc IS INITIAL AND
         NOT par_lfil IS INITIAL.
        MESSAGE e457.
      ENDIF.
    ENDIF.
* Open dataset for magnetic output - if Peception file was selected
    IF NOT par_perf IS INITIAL.
      CLOSE DATASET par_perf.
      OPEN DATASET par_perf FOR OUTPUT IN TEXT MODE
                   ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.
* Local file can only be written if name is specified
      IF NOT par_perf IS INITIAL AND
         ( par_pfil IS INITIAL AND NOT par_ploc IS INITIAL ).
        MESSAGE e454.
      ENDIF.
    ELSE.
      IF NOT par_ploc IS INITIAL AND
         NOT par_pfil IS INITIAL.
        MESSAGE e457.
      ENDIF.
    ENDIF.
  ENDIF.
  IF par_magn IS INITIAL AND flg_change_sel = 'X'.
    MOVE p_col09 TO hlp_col9m.
    MOVE p_col10 TO hlp_col10m.
    MOVE hlp_col9 TO p_col09.
    MOVE hlp_col10 TO p_col10.
    CLEAR: flg_change_sel.
  ENDIF.

* Check that time dependency for numbering is determined for regular run
  IF s_init IS INITIAL AND s_delete IS INITIAL.
*   An Stelle & muß eine Ziffer stehen
    MESSAGE e010(ar) WITH text-q01.
  ENDIF.

  IF par_cust IS INITIAL AND par_vend IS INITIAL.
    MESSAGE e460.
  ENDIF.

AT SELECTION-SCREEN ON s_init.
* Time dependency for page and doc. numbering must be between 1 and 4
  IF NOT s_init BETWEEN 1 AND 4.
    SET CURSOR FIELD 'S_INIT'.
    MESSAGE e854 WITH s_init.
  ENDIF.

AT SELECTION-SCREEN ON par_sort.
  IF NOT par_sort BETWEEN 1 AND 2.
* Do not specify a value larger than &
    MESSAGE e115(f7) WITH '2'.
  ENDIF.

AT SELECTION-SCREEN ON par_updh.
  IF NOT par_updh IS INITIAL AND
     NOT par_magn IS INITIAL.
    MESSAGE e458.
  ENDIF.
  IF NOT par_updh IS INITIAL AND
     par_comp IS INITIAL.
    SET CURSOR FIELD 'PAR_COMP'.
    MESSAGE e452.
  ENDIF.
  IF NOT par_updh IS INITIAL AND
     par_vers IS INITIAL.
    SET CURSOR FIELD 'PAR_VERS'.
    MESSAGE e451.
  ENDIF.

AT SELECTION-SCREEN ON p_txid1.
  PERFORM check_newtaxid USING p_txid1.
  IF flg_not_used_taxid IS INITIAL.
    SET CURSOR FIELD 'P_TAXID1'.
    MESSAGE w003.
  ENDIF.

AT SELECTION-SCREEN ON p_txid2.
  PERFORM check_newtaxid USING p_txid2.
  IF flg_not_used_taxid IS INITIAL.
    SET CURSOR FIELD 'P_TAXID2'.
    MESSAGE w003.
  ENDIF.

AT SELECTION-SCREEN ON s_drver.
  IF s_drver IS INITIAL.
    SET CURSOR FIELD 'S_DRVER'.
* Invalid time dependency: &
    MESSAGE e854 WITH s_drver.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sel_kts1-low.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = sel_kts1-low
    EXCEPTIONS
      OTHERS  = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sel_kts1-high.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = sel_kts1-high
    EXCEPTIONS
      OTHERS  = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sel_kts2-low.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = sel_kts2-low
    EXCEPTIONS
      OTHERS  = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sel_kts2-high.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = sel_kts2-high
    EXCEPTIONS
      OTHERS  = 1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_kts1.
*  CALL FUNCTION 'J_1A_HELP_KTOSL'
*    EXPORTING
*      display = ' '
*    IMPORTING
*      e_ktosl = par_kts1
*    EXCEPTIONS
*      OTHERS  = 1.

*----------------------------------------------------------------------*
*  Preperations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.
   perform f_percep_iibb       .
  CLEAR: par_cust, par_vend.
  IF r_cust   = 'X'.
    par_cust   = 'X'.
  ENDIF.
  IF r_vend EQ 'X'.
    par_vend   = 'X'.
  ENDIF.
  par_magn = ' '.
*[BEGIN] -- PM -- 18/08/09
* Cargo el catalogo e inicializo el alv
  PERFORM inicializar_alv CHANGING it_header[] gt_fieldcat[] gs_layout t_sort[].
*[ END ] -- PM -- 18/08/09

  COMMIT WORK.
  copy s_bldat  to br_bldat.
*  copy s_belnr  to br_belnr.                                  "1069346
*  copy s_blart  to br_blart.                                  "1069346
* read company code customizing data
  PERFORM read_t001 USING br_bukrs-low.
* read version of daily VAT-Reporting for selected bukrs.
  PERFORM read_j_1adrver USING br_bukrs-low s_drver.
* read text for version of daily VAT-Reporting for selected bukrs.
  PERFORM read_j_1adrvert USING br_bukrs-low.
* read historie for daily VAT reporting
  PERFORM read_tab_history USING br_bukrs-low tab_j_1adrver-j_1aproc.
* delete daily VAT report with the version "S_DRVER", "S_BUDAT"
  IF s_delete NE space.
    PERFORM delete_history_table.
  ENDIF.
* read account set maggi_zfiy0015
  PERFORM read_account_set.

* rules for checking of processing keys SEL_KTS1,SEL_KTS2
  DESCRIBE TABLE sel_kts1 LINES tmp_cnt1.
  DESCRIBE TABLE sel_kts2 LINES tmp_cnt2.

  IF tmp_cnt1 EQ 0.                    " no primary proc. key chosen
    IF tmp_cnt2 EQ 0.                  " no secondary proc. key chosen
      flg_checking_ktsol = '0'.        " no check for SEL_KTS1,SEL_KTS2.
    ELSE.
      flg_checking_ktsol = '1'.        " no check for SEL_KTS1 but KTS2
    ENDIF.
  ELSE.                                " primary  proc. key chosen
    IF tmp_cnt2 EQ 0.                  " no secondary proc. key chosen
      flg_checking_ktsol = '2'.        " no check f. SEL_KTS2 but KTS1
    ELSE.                              " other proc. key chosen
      flg_checking_ktsol = '3'.        " check f. SEL_KTS2 and SEL_KTS1.
    ENDIF.
  ENDIF.

* BEGIN: Note 675178
* Fill fiscal year for logical database.
* -> Get fiscal year from posting date, lower and upper limit

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date  = br_budat-low                                "1069346
      i_periv = tab_001-periv
    IMPORTING
      e_gjahr = br_gjahr-low
    EXCEPTIONS
      OTHERS  = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date  = br_budat-high                               "1069346
      i_periv = tab_001-periv
    IMPORTING
      e_gjahr = br_gjahr-high
    EXCEPTIONS
      OTHERS  = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MOVE: 'I'      TO br_gjahr-sign,
        'EQ'     TO br_gjahr-option.
  APPEND br_gjahr.
* END:   Note 675178

* reset return flags for customer exit.
  CLEAR: rcode_bkpf, rcode_bseg.

*----------------------------------------------------------------------*
* Document header
*----------------------------------------------------------------------*
GET bkpf.
  APPEND  bkpf TO t_bkpf.

* Report run with customer exit or not.
*  if not par_cexi is initial.
* customer exit 001 .
  CLEAR: rcode_bkpf, rcode_bseg.
  CALL FUNCTION 'J_1A_EXIT_J_1AF105'
    EXPORTING
      i_bkpf  = bkpf
      i_step  = '001'
    IMPORTING
      e_rcode = rcode_bkpf.
* check if the document has to be proceed ( returns from customer exits)
  CHECK: rcode_bkpf IS INITIAL.
*  endif.

* init internal tables for one document
  REFRESH:
     tab_bseg, tab_bseg_nodk,          " for GET BSEG
     tab_bset, ran_ktosl_kts1,sd_hkont," for GET BSET
     tab_log_entry1, tab_log_entry2,
     line_total.

*  the document has not to proceed : if flg_reject_doc =  'X'
  CLEAR: flg_reject_doc,rcode.

* Fields for magnetic output
  CLEAR: gf_cai, gf_fisc_cont.

* read text official document type in Argentinien  J_1AOTDETR
  SELECT SINGLE land1 INTO land1 FROM t001 WHERE bukrs = t001-bukrs.
  PERFORM read_official_doc_type USING land1 bkpf-blart bkpf-xblnr+4(1).
*  PERFORM READ_OFFICIAL_DOC_TYPE USING BKPF-BLART BKPF-XBLNR+4(1).

* Set flag for Industry Solutions Documents
  CLEAR: flg_sd_beleg, flg_is_beleg .

*set flag SD beleg
  IF bkpf-awtyp      = 'VBRK' OR       " SD document or
      bkpf-adisc = 'S'.                " SD discount
    flg_sd_beleg = 'X'.
    flg_is_beleg = 'S'.
  ENDIF.
*set flag IS-M/AM Document
  IF bkpf-awtyp      = 'JHTFK'  OR     "IS-M/AM Document
      bkpf-adisc = 'J'.                "IS-M/AM discount
    flg_is_beleg = 'M'.
  ENDIF.
*set flag FI Document
  IF bkpf-awtyp      = 'BKPF' .        "FI Document
    flg_is_beleg = 'F'.
  ENDIF.
*set flag MM Document
  IF bkpf-awtyp      = 'RMRP'.         "MM Document
    flg_is_beleg = 'R'.
  ENDIF.
* Note 910399 set the BKPFF document types
* as MM document. Note 1063638 change it to
* FI document
*set flag FI Document - BAPI
  IF bkpf-awtyp      = 'BKPFF'        "FI Document
  AND bkpf-adisc NE 'S'.               "Check not SD discount 1085024
    flg_is_beleg = 'F'.
  ENDIF.

* Note 1091024 Start
*set flag for external documents
  IF flg_is_beleg IS INITIAL.
    flg_is_beleg = 'F'.
  ENDIF.
* Note 1091024 End
*----------------------------------------------------------------------*
* Document items
*       append
*  BSEG---------> tab_bseg                ( Customer / Vendor Lines)
*      |--------> tab_bseg_nodk           ( no Customer / Vendor Lines)
*----------------------------------------------------------------------*
GET bseg.
  APPEND bseg TO t_bseg.

* customer exit 002 .
  IF rcode_bseg IS INITIAL.
    CALL FUNCTION 'J_1A_EXIT_J_1AF105'
      EXPORTING
        i_bkpf  = bkpf
        i_bseg  = bseg
        i_step  = '002'
      IMPORTING
        e_rcode = rcode_bseg.
  ENDIF.
  CHECK rcode_bseg IS INITIAL.

* Fill from bseg into tab_bseg  or tab_bseg_nodk.
  PERFORM fill_bseg_to_tab_bseg.

*----------------------------------------------------------------------*
* VAT items
*       append
*  BSET---------> tab_bset.
*----------------------------------------------------------------------*
GET bset.

  APPEND bset TO t_bset.

  PERFORM check_tax_code USING bset-mwskz.
  CHECK rcode = 0.

  IF bset-mwskz = 'A0'.
    bkpf-awtyp = 'BKPF'.
    flg_is_beleg = 'F'.
  ENDIF.

* fill table TAB_BSET
  IF bkpf-awtyp NE 'VBRK' AND       " if not SD document    Note 481536
     flg_is_beleg NE 'S'.           " or SD discount        Note 481536
    PERFORM: fill_bset_to_tab_bset.
  ELSE.                                " SD document or SD discount
    MOVE-CORRESPONDING bset TO sd_hkont.
    COLLECT sd_hkont.
  ENDIF.

*----------------------------------------------------------------------*
*  prepare the document for output (ep)
*  -->  p1        bkpf,
*                 tab_bseg,tab_bseg_nodk
*                 tab_bset,ran_ktosl_kts1,
*  <--  p2        tab_beleg
*----------------------------------------------------------------------*
GET bkpf LATE.
* check if the document has to be proceed ( returns from customer exits)
  CHECK: rcode_bseg IS INITIAL.
* if flg_reject_doc is set then LOG entry is written and doc can be ign.
  CHECK: flg_reject_doc IS INITIAL.
* prepare the tab_bseg and tab_bseg_nodk for (sd_docm,amnt) e.t.c
  PERFORM check_tab_bseg.
* prepare the tab_bseg_nodk for (dis_net_amount) e.t.c
  PERFORM check_tab_bseg_nodk.
* SD Beleg
  IF flg_is_beleg = 'S'.               " SD document
    PERFORM read_sd_invoice_data.
    CHECK: flg_reject_doc IS INITIAL.
  ENDIF.
* Check MM documents -> Reversed?
  IF flg_is_beleg = 'R'.               " MM document
    PERFORM read_mm_invoice_data.
*   The print authorization code (CAI) from MM is in text field BKTXT
  ENDIF.
  PERFORM read_cai_and_fisc_cont.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  PERFORM fill_tab_taxes.
  PERFORM fill_tab_bseg_taxed.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

* tab_beleg all the output lines for a document in no compress modus
  PERFORM fill_tab_beleg.
  CHECK: flg_reject_doc IS INITIAL.
* if check only selected Vendors and Customers
  IF NOT par_cust IS INITIAL.
    IF par_vend IS INITIAL AND tab_bseg-koart <> 'D'.
      REJECT.
    ENDIF.
  ENDIF.
  IF NOT par_vend IS INITIAL.
    IF par_cust IS INITIAL AND tab_bseg-koart <> 'K'.
      REJECT.
    ENDIF.
  ENDIF.
  DESCRIBE TABLE s_lifnr LINES tmp_cnt1.
  IF NOT tmp_cnt1 IS INITIAL.
    IF NOT tab_bseg-ktnra IN s_lifnr.
      REJECT.
    ENDIF.
  ENDIF.
* check valid document : version selection
  IF NOT par_vers IS INITIAL.
    IF xumsks IS INITIAL.
      IF tab_j_1adrver-j_1aproc EQ 1
         OR tab_j_1adrver-j_1aproc EQ 2.
        IF amnt-total > 0.
          REJECT.
        ENDIF.
      ENDIF.
    ELSE.
      IF tab_j_1adrver-j_1aproc EQ 1
         OR tab_j_1adrver-j_1aproc EQ 2.
* Note 795638 changes starts
        CLEAR tab_beleg.
        READ TABLE tab_beleg WITH KEY linetype = '1'.
        IF tab_beleg-total > 0.
          REJECT.
        ENDIF.
* Note 795638 changes ends
      ENDIF.
    ENDIF.
  ELSE.
* No limitation, no filters
* alles keine abgrenzung ; KEINE FILTERUNG
  ENDIF.
* check ktosl selection
  flg_reject_doc = 'X'.                " reject docu
  PERFORM  check_ktosl_selection.
  CHECK: flg_reject_doc IS INITIAL.
* extract tab_beleg into ep.
  PERFORM extract_tab_beleg_to_ep.
* Now we have one document to display, let's get the next

*----------------------------------------------------------------------*
* procedure of the extracted data
*----------------------------------------------------------------------*
END-OF-SELECTION.

  SORT tab_regio BY bukrs belnr gjahr mwskz.
* Document list has to be sorted by xblnr for version 4 (Sales VAT)
  IF tab_j_1adrver-j_1aproc = '4'.
    par_sort = 2.
  ENDIF.

  IF par_magn IS INITIAL.              " List output
    CASE par_sort.
      WHEN 1.
        SORT BY
          ep-bukrs                     "Buchungskreis
          ep-budat                     "Buchungsdatum
          ep-belnr                     "Belegnummer
          ep-cpudt
          ep-cputm.
      WHEN 2.
        SORT BY
          ep-bukrs                     "Buchungskreis
          ep-budat                     "Buchungsdatum
          ep-xblnr                     "X-Belegnummer
          ep-belnr                     "Belegnummer
          ep-cpudt
          ep-cputm.
    ENDCASE.
  ELSE.                                " Magnetic output
    IF tab_j_1adrver-j_1aproc = '1' OR
       tab_j_1adrver-j_1aproc = '3'.   " Purchases file
      SORT BY
        ep-bukrs
        ep-budat
        ep-j_1aoftp
        ep-xblnr
        ep-belnr
        ep-cpudt
        ep-cputm.
    ELSEIF tab_j_1adrver-j_1aproc = '2' OR
           tab_j_1adrver-j_1aproc = '4'.   " Sales file
      SORT BY
        ep-bukrs
        ep-bldat
        ep-j_1aoftp
        ep-xblnr
        ep-belnr
        ep-cpudt
        ep-cputm.
    ENDIF.
  ENDIF.

  REFRESH: tab_history, tab_ep.
  CLEAR: i_record3, o_record3.

  clear par_canc.

  LOOP.                                " extracted data

    AT FIRST.
      list_number = 1.                 " List number 1 Einzelposten
    ENDAT.
    rate = ep-rate.
    IF rate = 0.
      find_exempted = 'X'.
    ENDIF.
* WITHOUT magnetic output - for page totals and takeover for next page
    AT NEW ep-budat.
      IF par_magn IS INITIAL OR
         ( tab_j_1adrver-j_1aproc = '1' OR           " Note 658485
           tab_j_1adrver-j_1aproc = '3' ).           " Note 658485
        wa_history_over = wa_history.
        PERFORM set_history_lineno.
        wa_history-j_1adrdt   = ep-budat.
        IF wa_history-j_1apageno = 1.
          IF sy-linno < 55 AND
             sy-linno > 1.
            SKIP TO LINE 55.             " to call end-of-page
            RESERVE 3 LINES.
          ENDIF.
*            new-page.
        ENDIF.
      ENDIF.
    ENDAT.

* WITH magnetic output - for page totals and takeover for next page
    AT NEW ep-bldat.
      IF NOT par_magn IS INITIAL AND
         ( tab_j_1adrver-j_1aproc = '2' OR           " Note 658485
           tab_j_1adrver-j_1aproc = '4' ).           " Note 658485
        wa_history_over = wa_history.
        PERFORM set_history_lineno.
        wa_history-j_1adrdt   = ep-budat.
        IF wa_history-j_1apageno = 1.
          IF sy-linno < 55 AND
             sy-linno > 1.
            SKIP TO LINE 55.             " to call end-of-page
            RESERVE 3 LINES.
          ENDIF.
*            new-page.
        ENDIF.
      ENDIF.
    ENDAT.

    tab_ep = ep.
    APPEND tab_ep.

    AT END OF ep-belnr.                " each document
      REFRESH: tab_sum_mwskz.
      CLEAR t_alv.
      PERFORM: print_tab_ep.           " print list with all VAT infos
      " perform magn output if selected
      REFRESH: tab_ep.
      ADD 1 TO wa_history-j_1alineno.
    ENDAT.

    AT END OF ep-budat.                " end of posting date
      CLEAR t_alv.
      PERFORM add_entry_to_tab_history.
    ENDAT.

    AT END OF ep-buper.
      CLEAR t_alv.
      IF sy-linno < 55.
        SKIP TO LINE 55.               " to call end-of-page
      ENDIF.
      RESERVE 3 LINES.
    ENDAT.

    AT LAST.
      CLEAR t_alv.
      IF sy-linno < 55.
        SKIP TO LINE 55.               " to call end-of-page
      ENDIF.
      RESERVE 3 LINES.

      IF NOT par_magn IS INITIAL.
*     For records type 3: Calendar month of fiscal period needed
        gf_poper = tab_ep-monat.       " For import parameter
        CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
          EXPORTING
            i_gjahr        = tab_ep-gjahr
            i_periv        = tab_001-periv
            i_poper        = gf_poper
          IMPORTING
            e_date         = gf_rec3_buper
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        IF tab_j_1adrver-j_1aproc = '1' OR
           tab_j_1adrver-j_1aproc = '3'.    " input/purchase
          PERFORM fill_i_record3 CHANGING i_record3.
        ELSE.                          " output/sales
          PERFORM fill_o_record3 CHANGING o_record3.
        ENDIF.
      ENDIF.

      list_number = 2.                 " list number print totals
      IF s_numbc IS INITIAL.
        wa_history-j_1apageno = 1.     " start pageno with 1
      ENDIF.

      PERFORM print_tab_sum_mwskz.     " print totals of tax code
      PERFORM print_tab_sum_others.    " print totals of others
      PERFORM print_tab_sum_mwskz_new. " print totals of tax code
      PERFORM print_tab_sum_rate.      " print totals of vat rate
      PERFORM print_tab_sum_regio.     " print totals of regio
      PERFORM print_tab_sum_oftpt.     " print totals of amtl. belegart
      PERFORM print_tab_sum_account.   " print totals of account
    ENDAT.

  ENDLOOP.

  IF NOT par_magn IS INITIAL AND NOT par_file IS INITIAL.
    CLOSE DATASET par_file.
    IF NOT par_loc IS INITIAL.         " Transfer to local file required
      IF tab_j_1adrver-j_1aproc = '1' OR
         tab_j_1adrver-j_1aproc = '3'.    " input/purchase
        PERFORM transfer_local USING par_file par_lfil tab_i_record.
      ELSE.                                " output/sales
        PERFORM transfer_local USING par_file par_lfil tab_o_record.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT par_magn IS INITIAL AND NOT par_perf IS INITIAL.
    CLOSE DATASET par_perf.
    IF NOT par_ploc IS INITIAL.        " Transfer to local file required
      PERFORM transfer_local USING par_perf par_pfil tab_p_record.
    ENDIF.
  ENDIF.

  list_number = 3.                     " list number print log entries
  NEW-PAGE.
  PERFORM update_history_table.        " udate history table
  PERFORM print_tab_log_entry10.       " not found table entries
  PERFORM print_tab_log_entry20.       " not selected BELNR for KTOSL
  PERFORM print_tab_history.           " selected history

  CLEAR: tab_ep.

  PERFORM elimina_documentos_no_vis.

  PERFORM f_armo_tabla_proveedores.

  PERFORM f_armo_tabla_documento.

  PERFORM f_armo_tabla_razao.

  PERFORM f_armo_totales.

* Columna totales.
  PERFORM f_totales.

* Columna de Cl. de doc. y agregados / Importes IIBB con indicador CX en columna ganancias
  PERFORM f_agregar_campos_adic.

* Muestro el ALV
  PERFORM muestra_alv USING gs_layout t_sort gt_fieldcat[] t_alv[].

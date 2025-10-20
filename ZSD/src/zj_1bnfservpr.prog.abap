*&---------------------------------------------------------------------*
*& Report  J_1BNFPR                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Print of Nota Fiscal by SAPscript                                  *
*&  Should be used together with Message Control (NAST)                *
*&---------------------------------------------------------------------*

REPORT  zj_1bnfservpr MESSAGE-ID 8b.

*======================================================================*
*  TABLES, INCLUDES, STRUCTURES, DATAS, ...                            *
*======================================================================*

* Variaveis globais
DATA: v_quant(13),
      v_vrunit(13),
      v_vrtot(13),
      v_unit(15) TYPE p DECIMALS 2,
      v_ipirate(2) TYPE n,
      v_icmsrate(2) TYPE n,
      v_descr(45),
      v_sittrib(3),
      v_nftot(13),
      v_iss_rate(13),
      v_iss_val(13),
      v_vlext(512),
      v_cfop_text(46),
      v_texto1 TYPE char72,
      v_texto2 TYPE char72,
      v_texto3 TYPE char72,
      wa_in_words TYPE spell.

*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
* tables ---------------------------------------------------------------
TABLES: j_1bnfdoc,
        vbrk,                          " billing document header
        bkpf.                          " financial document header
*----------------------------------------------------------------------*
*  INCLUDES                                                            *
*----------------------------------------------------------------------*
* INCLUDE for General Table Descriptions for Print Programs ------------
INCLUDE rvadtabl.

*----------------------------------------------------------------------*
*  STRUCTURES                                                          *
*----------------------------------------------------------------------*
* Nota Fiscal header structure -----------------------------------------
DATA: BEGIN OF wk_header.
        INCLUDE STRUCTURE j_1bnfdoc.
DATA: END OF wk_header.

* Nota Fiscal header structure - add. segment --------------------------
DATA: BEGIN OF wk_header_add.
        INCLUDE STRUCTURE j_1bindoc.
DATA: END OF wk_header_add.

* Nota Fiscal partner structure ----------------------------------------
DATA: BEGIN OF wk_partner OCCURS 0.
        INCLUDE STRUCTURE j_1bnfnad.
DATA: END OF wk_partner.

* Nota Fiscal item structure -------------------------------------------
DATA: BEGIN OF wk_item OCCURS 0.
        INCLUDE STRUCTURE j_1bnflin.
DATA: END OF wk_item.

* Nota Fiscal item structure - add. segment ----------------------------
DATA: BEGIN OF wk_item_add OCCURS 0.
        INCLUDE STRUCTURE j_1binlin.
DATA: END OF wk_item_add.

* Nota Fiscal item tax structure ---------------------------------------
DATA: BEGIN OF wk_item_tax OCCURS 0.
        INCLUDE STRUCTURE j_1bnfstx.
DATA: END OF wk_item_tax.

* Nota Fiscal header message structure ---------------------------------
DATA: BEGIN OF wk_header_msg OCCURS 0.
        INCLUDE STRUCTURE j_1bnfftx.
DATA: END OF wk_header_msg.

* Nota Fiscal reference to header message structure -------------------
DATA: BEGIN OF wk_refer_msg OCCURS 0.
        INCLUDE STRUCTURE j_1bnfref.
DATA: END OF wk_refer_msg.

* Partner name and address ---------------------------------------------
DATA: BEGIN OF wk_parnad.
        INCLUDE STRUCTURE j_1binnad.
DATA: END OF wk_parnad.

* auxiliar structure for vbrk key (used to update FI) ------------------
DATA: BEGIN OF key_vbrk,
        vbeln LIKE vbrk-vbeln,
      END OF key_vbrk.

* auxiliar structure for bkpf key (used to update FI) ------------------
DATA: BEGIN OF key_bkpf,
        bukrs  LIKE   bkpf-bukrs,
        belnr  LIKE   bkpf-belnr,
        gjahr  LIKE   bkpf-gjahr,
      END OF key_bkpf.


*----------------------------------------------------------------------*
*  DATAS AND CONSTANTS                                                 *
*----------------------------------------------------------------------*
* datas ----------------------------------------------------------------
DATA: wk_docnum LIKE j_1bnfdoc-docnum, " Nota Fiscal document number
      retcode LIKE sy-subrc,           " return code indicator
      xscreen,                         " Output on printer or screen
      wk_xblnr LIKE bkpf-xblnr,        " use for financial docum. update
      subrc_upd_bi LIKE sy-subrc.
DATA: bi_subrc LIKE sy-subrc,          " indicates if BI doc is locked
      fi_subrc LIKE sy-subrc.          " indicates if FI doc is locked
*boi                        "note 768693
DATA: print_options(1) TYPE c,
      button1(12) TYPE c,
      button2(12) TYPE c.
CONSTANTS: c_adobe(12)      TYPE c VALUE 'Adobe',           "#EC NOTEXT
           c_smartforms(12) TYPE c VALUE 'SmartForms',      "#EC NOTEXT
           c_sapscript(12)  TYPE c VALUE 'Sapscript'.       "#EC NOTEXT

DATA: gs_nfeactive TYPE j_1bnfe_active.

*======================================================================*
*  Nota Fiscal print include                                           *
*======================================================================*
INCLUDE j_1bnfpr_printinc.
INCLUDE j_1bnfpr_sf.
INCLUDE j_1bnfpr_adobe.
*eoi
INCLUDE zj_1bnservfpi.
*INCLUDE zj_1bnfpi.

*======================================================================*
*  PROGRAM                                                             *
*======================================================================*

*&---------------------------------------------------------------------*
*&       FORM ENTRY  (MAIN FORM)                                       *
*&---------------------------------------------------------------------*
*       Form for Message Control                                       *
*----------------------------------------------------------------------*
FORM entry USING return_code us_screen.
* initialize retcode and xscreen----------------------------------------
  CLEAR retcode.
  xscreen = us_screen.

*boi                         "note 768693
  IF sy-batch <> 'X'.
*if not printing in batch job
* conditions to select the right output
*no output field selected
    IF tnapr-fonam IS INITIAL AND
       tnapr-funcname IS INITIAL AND
       tnapr-sform IS INITIAL.
      MESSAGE i569(icc_nf_writer).
    ENDIF.
*only smartform-field selected
    IF tnapr-fonam IS INITIAL AND
       tnapr-funcname IS INITIAL AND
       NOT tnapr-sform IS INITIAL.
      PERFORM smart_sub_printing.
    ENDIF.
*only sapscript-field selected
    IF NOT tnapr-fonam IS INITIAL AND
       tnapr-funcname IS INITIAL AND
       tnapr-sform IS INITIAL.
      PERFORM printing.
    ENDIF.
*only adobe-field selected
    IF tnapr-fonam IS INITIAL AND
         tnapr-funcname IS NOT INITIAL AND
         tnapr-sform IS INITIAL.
      PERFORM adobe_sub_printing.
    ENDIF.

*all fields filled
    IF tnapr-fonam IS NOT INITIAL AND
       tnapr-funcname IS NOT INITIAL AND
       tnapr-sform IS NOT INITIAL.
      PERFORM print-popup.
    ENDIF.
*adobe and smartform field selected
    IF tnapr-fonam IS INITIAL AND
       tnapr-funcname IS NOT INITIAL AND
       tnapr-sform IS NOT INITIAL.
      button1 = c_adobe.
      button2 = c_smartforms.
      PERFORM print-popup_2buttons USING button1 button2 print_options.
      IF print_options = '1'.
        PERFORM adobe_sub_printing.
      ENDIF.
      IF print_options = '2'.
        PERFORM smart_sub_printing.
      ENDIF.
    ENDIF.
*smartform and sapscript field selected
    IF tnapr-fonam IS NOT INITIAL AND
       tnapr-funcname IS INITIAL AND
       tnapr-sform IS NOT INITIAL.
      button1 = c_smartforms.
      button2 = c_sapscript.
      PERFORM print-popup_2buttons USING button1 button2 print_options.
      IF print_options = '1'.
        PERFORM smart_sub_printing.
      ENDIF.
      IF print_options = '2'.
        PERFORM printing.
      ENDIF.
    ENDIF.
*adobe and sapscript field selected
    IF tnapr-fonam IS NOT INITIAL AND
       tnapr-funcname IS NOT INITIAL AND
       tnapr-sform IS INITIAL.
      button1 = c_adobe.
      button2 = c_sapscript.
      PERFORM print-popup_2buttons USING button1 button2 print_options.
      IF print_options = '1'.
        PERFORM adobe_sub_printing.
      ENDIF.
      IF print_options = '2'.
        PERFORM printing.
      ENDIF.
    ENDIF.
  ELSE.
* in batch job
    IF NOT tnapr-fonam IS INITIAL.
      PERFORM printing.
    ELSEIF tnapr-sform IS NOT INITIAL.
      PERFORM smart_sub_printing.
    ELSEIF tnapr-funcname IS NOT INITIAL.
      PERFORM adobe_sub_printing.
    ENDIF.
  ENDIF.

*eoi                            "note 768693

* main -----------------------------------------------------------------
* PERFORM printing.                    " print the Nota Fiscal

* check retcode (return code) ------------------------------------------
  IF retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.

ENDFORM.                               " ENTRY

*======================================================================*
*  FORM ROUTINES USED IN THE MAIN FORM ENTRY                           *
*======================================================================*

*&---------------------------------------------------------------------*
*&      Form  PRINTING
*&---------------------------------------------------------------------*
*       Print the Nota Fiscal using the SAPscript                      *
*----------------------------------------------------------------------*
FORM printing.

* read the Nota Fiscal -------------------------------------------------
  PERFORM nota_fiscal_read.            " read nota fiscal

* allow print of NF only when NF is not canceled
  PERFORM check_nf_canceled.   " check nota fiscal canceled, note 442570

* number and update the Nota Fiscal ------------------------------------
  CHECK retcode IS INITIAL.

  IF wk_header-entrad = 'X' OR  "update only entradas or outgoing NF
     wk_header-direct  = '2'.
    IF wk_header-printd IS INITIAL AND " not printed.
       wk_header-nfnum IS INITIAL  AND " without NF number
       nast-nacha = '1'.               " sent to printer

      PERFORM enqueue_bi_fi.           " Lock check on FI and BI doc.
      " BEFORE getting next NF number!
      CHECK retcode IS INITIAL.
* begin change 28.01.97: perform lock already in RSNAST00
*      perform lock_nf.                 " lock NF
*
*      if retcode is initial.           " lock works.
* end change 28.01.97: perform lock already in RSNAST00
      PERFORM nota_fiscal_number.      " get the next number
      IF retcode IS INITIAL.
        PERFORM financial_doc_update.    " update in database
        PERFORM nota_fiscal_update.      " update in database
      ENDIF.
* begin change 28.01.97: perform lock already in RSNAST00
*      endif.
*      perform unlock_nf.               "unlock NF
* end change 28.01.97: perform lock already in RSNAST00
    ENDIF.
  ENDIF.

  IF retcode IS INITIAL.
*del    CALL FUNCTION 'DB_COMMIT'.                    "note 333844
  ELSE.
*   abend transaction in case of error (otherwise NF number can be lost)
    IF NOT sy-msgid IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE a114 WITH '' 'J_1BNFNUMB'.
    ENDIF.

  ENDIF.

* initialize SAPscript print--------------------------------------------
  CHECK retcode IS INITIAL.
* perform initialize_print.

* open form ------------------------------------------------------------
  CHECK retcode IS INITIAL.
  PERFORM form_open USING xscreen j_1bprnfis-land1.

* print main window ----------------------------------------------------
  CHECK retcode IS INITIAL.
* perform print_variable_form.
  PERFORM print_nota_fiscal.

* close form -----------------------------------------------------------
  CHECK retcode IS INITIAL.
  PERFORM form_close.

ENDFORM.                               " PRINTING

*&---------------------------------------------------------------------*
*&      Form  NOTA_FISCAL_READ
*&---------------------------------------------------------------------*
*       Read the Nota Fiscal based in the key giving by Message        *
*       Control.                                                       *
*----------------------------------------------------------------------*
FORM nota_fiscal_read.
* get the key ----------------------------------------------------------
  MOVE nast-objky TO wk_docnum.

* read the Nota Fiscal document ----------------------------------------
  CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
    EXPORTING
      doc_number         = wk_docnum
    IMPORTING
      doc_header         = wk_header
    TABLES
      doc_partner        = wk_partner
      doc_item           = wk_item
      doc_item_tax       = wk_item_tax
      doc_header_msg     = wk_header_msg
      doc_refer_msg      = wk_refer_msg
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

* check the sy-subrc ---------------------------------------------------
  PERFORM check_error.


  CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
    EXPORTING
      nf_header   = wk_header
    IMPORTING
      ext_header  = wk_header_add
    TABLES
      nf_item     = wk_item
      nf_item_tax = wk_item_tax
      ext_item    = wk_item_add.

ENDFORM.                               " NOTA_FISCAL_READ

*&---------------------------------------------------------------------*
*&      Form  NOTA_FISCAL_NUMBER
*&---------------------------------------------------------------------*
*       Get the next Nota Fiscal number                                *
*----------------------------------------------------------------------*
FORM nota_fiscal_number.
* get the next number with bukrs, branch and form ----------------------
  CALL FUNCTION 'J_1B_NF_NUMBER_GET_NEXT'
    EXPORTING
      bukrs                         = wk_header-bukrs
      branch                        = wk_header-branch
      form                          = wk_header-form
      headerdata                    = wk_header     " note 743361
    IMPORTING
      nf_number                     = wk_header-nfnum
    EXCEPTIONS
      print_number_not_found        = 1
      interval_not_found            = 2
      number_range_not_internal     = 3
      object_not_found              = 4
      other_problems_with_numbering = 5
      OTHERS                        = 6.

* check the sy-subrc ---------------------------------------------------
  PERFORM check_error.

ENDFORM.                               " NOTA_FISCAL_NUMBER

*&---------------------------------------------------------------------*
*&      Form  NOTA_FISCAL_UPDATE
*&---------------------------------------------------------------------*
*       Update NF date and number                                      *
*----------------------------------------------------------------------*
FORM nota_fiscal_update.
* move the system date to document date --------------------------------
* wk_header-docdat = sy-datum.    " not update docdat
  wk_header-printd = 'X'.

* update the nota fiscal in the database -------------------------------
  UPDATE j_1bnfdoc FROM wk_header.

* check the sy-subrc ---------------------------------------------------
  IF sy-subrc <> 0.
    retcode = sy-subrc.
    syst-msgid = '8B'.
    syst-msgno = '107'.
    syst-msgty = 'E'.
    syst-msgv1 = wk_header-docnum.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                               " NOTA_FISCAL_UPDATE

*&---------------------------------------------------------------------*
*&      Form  FORM_OPEN
*&---------------------------------------------------------------------*
*       This routine open the form                                     *
*----------------------------------------------------------------------*
*  -->  US_SCREEN  Output on screen                                    *
*                  ' ' = printer                                       *
*                  'X' = screen                                        *
*  -->  US_COUNTRY County for telecommunication and SET COUNTRY        *
*----------------------------------------------------------------------*
FORM form_open USING us_screen us_country.
* open form ------------------------------------------------------------
* INCLUDE for OPEN_FORM for SD Print Programs --------------------------
  INCLUDE rvadopfo.

ENDFORM.                               " FORM_OPEN

*&---------------------------------------------------------------------*
*&      Form  FORM_CLOSE
*&---------------------------------------------------------------------*
*       Close the form                                                 *
*----------------------------------------------------------------------*
FORM form_close.
* close form -----------------------------------------------------------

  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened = 01.

* check the sy-subrc ---------------------------------------------------
  PERFORM check_error.

ENDFORM.                               " FORM_CLOSE

*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR
*&---------------------------------------------------------------------*
*       Check return code                                              *
*----------------------------------------------------------------------*
FORM check_error.
  IF sy-subrc <> 0.
    retcode = sy-subrc.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                               " CHECK_ERROR

*&---------------------------------------------------------------------*
*&      Form  PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.        *
*----------------------------------------------------------------------*
FORM protocol_update.

  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                               " PROTOCOL_UPDATE

*&---------------------------------------------------------------------*
*&      Form  FINANCIAL_DOC_UPDATE
*&---------------------------------------------------------------------*
*       Update the sales document and Financial document with the      *
*       Nota Fiscal number and the Nota Fiscal with the financial      *
*       document                                                       *
*----------------------------------------------------------------------*
FORM financial_doc_update.

* sort the wk_item to get the first item -------------------------------
  SORT wk_item.
  READ TABLE wk_item INDEX 1.

* condense the nf number and series ------------------------------------
  CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
    EXPORTING
      nf_number  = wk_header-nfnum
      series     = wk_header-series
      subseries  = wk_header-subser
    IMPORTING
      ref_number = wk_xblnr
    EXCEPTIONS
      OTHERS     = 1.

* get the type of the document and update the documents ----------------
  CASE wk_item-reftyp.

    WHEN 'BI'.
      MOVE wk_item-refkey TO key_vbrk.
      PERFORM read_bi_document.
      CLEAR bkpf.
      IF NOT vbrk IS INITIAL.          " if find VBRK (Billing document)
        PERFORM get_fi_number.
      ENDIF.
      IF bkpf-belnr IS INITIAL.        " there is not FI document
        IF NOT vbrk IS INITIAL.        " if find VBRK (Billing document)
          PERFORM update_bi_document.
        ENDIF.
*ENHANCEMENT-POINT J_1BNFPR_01 SPOTS ES_J_1BNFPR.
      ELSE.                            " there is FI document
        PERFORM update_bi_document.
        IF  subrc_upd_bi IS INITIAL.   " update in billing ok.
          PERFORM update_fi_nf_document
                    USING bkpf-bukrs bkpf-belnr bkpf-gjahr.
          PERFORM update_bsid_nf_document
                  USING bkpf-bukrs bkpf-belnr bkpf-gjahr.
          "- KI3K050466 - 23.01.97 - BSID must also be updated
        ENDIF.
      ENDIF.

* begin change 23.01.97: KI3K050466
* no update of FI document in Entrada case
*    when 'IV'.
*      move wk_item-refkey to key_bkpf.
*      perform read_fi_document
*                using key_bkpf-bukrs key_bkpf-belnr key_bkpf-gjahr.
*      if not bkpf is initial.
*        perform update_fi_nf_document
*                  using key_bkpf-bukrs key_bkpf-belnr key_bkpf-gjahr.
*     endif.
* end change 23.01.97: KI3K050466

    WHEN OTHERS.  " for MD or <space> that means writer.
      wk_header-follow = 'X'.

  ENDCASE.

ENDFORM.                               " FINANCIAL_DOC_UPDATE

*&---------------------------------------------------------------------*
*&      Form  READ_BI_DOCUMENT
*&---------------------------------------------------------------------*
*       This form read the billing document                            *
*----------------------------------------------------------------------*
FORM read_bi_document.

  SELECT SINGLE * FROM  vbrk
         WHERE  vbeln       = key_vbrk-vbeln.

  IF sy-subrc <> 0.
    CLEAR vbrk.
  ENDIF.

ENDFORM.                               " READ_BI_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  READ_FI_DOCUMENT
*&---------------------------------------------------------------------*
*       read the fi_document                                           *
*----------------------------------------------------------------------*
FORM read_fi_document USING xbukrs xbelnr xgjahr.

  SELECT SINGLE * FROM  bkpf
         WHERE  bukrs       = xbukrs
         AND    belnr       = xbelnr
         AND    gjahr       = xgjahr.

  IF sy-subrc <> 0.
    CLEAR bkpf.
  ENDIF.

ENDFORM.                               " READ_FI_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BI_DOCUMENT
*&---------------------------------------------------------------------*
*       Update billing document                                        *
*----------------------------------------------------------------------*
FORM update_bi_document.

  IF bi_subrc = 0.                     " billing not lock

* OVERWRITE the value in xblnr -----------------------------------------
    UPDATE vbrk SET xblnr = wk_xblnr
                WHERE  vbeln = key_vbrk-vbeln.

    PERFORM check_error.

    subrc_upd_bi = sy-subrc.

    CALL FUNCTION 'DEQUEUE_EVVBRKE'
         EXPORTING
              mandt     = sy-mandt
              vbeln     = key_vbrk-vbeln
*         X_VBELN   = ' '
*         _SCOPE    = '3'
*         _SYNCHRON = ' '
         EXCEPTIONS
              OTHERS    = 1.

  ELSE.                                " billing lock

    subrc_upd_bi = sy-subrc.

  ENDIF.


ENDFORM.                               " UPDATE_BI_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_FI_NF_DOCUMENT
*&---------------------------------------------------------------------*
*       Update financial and nota fiscal document                      *
*----------------------------------------------------------------------*
FORM update_fi_nf_document USING xbukrs xbelnr xgjahr.

  IF fi_subrc = 0.                     " billing not lock

* OVERWRITE the value in xblnr -----------------------------------------
    UPDATE bkpf SET xblnr = wk_xblnr
                WHERE  bukrs       = xbukrs
                AND    belnr       = xbelnr
                AND    gjahr       = xgjahr.

    PERFORM check_error.
* begin change 23.01.97: KI3K050466 - dequeue of BKPF will only be done
* later, after BSID has been updated in Form update_bsid_nf_document,
* because BSID must stay locked
*    CALL FUNCTION 'DEQUEUE_EFBKPF'
*         EXPORTING
*              BUKRS  = XBUKRS
*              BELNR  = XBELNR
*              GJAHR  = XGJAHR
*         EXCEPTIONS
*              OTHERS = 1.

* Take out because does not work for all cases Stela   28/05/96 --------
*   if wk_header-belnr is initial.
*     wk_header-belnr = xbelnr.
*     wk_header-gjahr = xgjahr.
*   endif.
* end change 23.01.97: KI3K050466
* update the flag follow in header Nota Fiscal -------------------------
    wk_header-follow = 'X'.

  ENDIF.

ENDFORM.                               " UPDATE_FI_NF_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  GET_FI_NUMBER
*&---------------------------------------------------------------------*
*       Read financial document via Billing document number            *
*----------------------------------------------------------------------*
FORM get_fi_number.

  SELECT * FROM  bkpf
         WHERE  bukrs       = vbrk-bukrs
         AND    awtyp       = 'VBRK'   " billing document
         AND    awkey       = key_vbrk-vbeln.
    EXIT.                              " never will get more than one
  ENDSELECT.

  IF sy-subrc <> 0.
    CLEAR bkpf.
  ENDIF.

ENDFORM.                               " GET_FI_NUMBER

* begin change 28.01.97: perform lock already in RSNAST00
*&---------------------------------------------------------------------*
*&      Form  LOCK_NF
*&---------------------------------------------------------------------*
*       This form lock the Nota Fiscal                                 *
*----------------------------------------------------------------------*
*form lock_nf.
* call lock function (Exclusive, cumulating) ---------------------------
*  call function 'J_1B_NF_DOCUMENT_LOCK'
*       exporting
*           doc_number     = wk_header-docnum
*           lock_mode      = 'E'
*      exceptions
*           foreign_lock   = 1
*           system_failure = 2
*            others         = 3.
*
* check the sy-subrc ---------------------------------------------------
*  perform check_error.
*
*endform.                               " LOCK_NF
*
*&---------------------------------------------------------------------*
*&      Form  UNLOCK_NF
*&---------------------------------------------------------------------*
*       This form unlock the NF                                        *
*----------------------------------------------------------------------*
*form unlock_nf.
* call unlock function (Exclusive, cumulating) -------------------------
*  call function 'J_1B_NF_DOCUMENT_UNLOCK'
*      exporting
*           doc_number = wk_header-docnum
*           lock_mode  = 'E'
*      exceptions
*           others     = 1.
*
*endform.                               " UNLOCK_NF


*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSID_NF_DOCUMENT
*&---------------------------------------------------------------------*
*  update table BSID with external Nota Fiscal number - KI3K050466
*  change 23.01.97
*  Change 28.06.2000:
*  update also BSIS if G/L account with line item display exists
*  Change 09.08.2000: if cust account already cleared
*    (e.g. credit card sales) update BSAD instead of BSID
*----------------------------------------------------------------------*

FORM update_bsid_nf_document USING xbukrs xbelnr xgjahr.

  TABLES: bseg,
          bsid,
          bsis,
          bsad.

  SELECT * FROM bseg WHERE bukrs = xbukrs
                       AND belnr = xbelnr
                       AND gjahr = xgjahr
                       AND ( koart = 'D' OR koart = 'S' ).
    IF sy-subrc = '0'.
      IF bseg-koart = 'D'.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< start insert note  0324742
        IF NOT bseg-augbl IS INITIAL.  " account cleared --> update BSAD
          UPDATE bsad SET xblnr = wk_xblnr
                    WHERE bukrs = bseg-bukrs
                      AND kunnr = bseg-kunnr
                      AND umsks = bseg-umsks
                      AND umskz = bseg-umskz
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-zuonr
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.

        ELSE.   " open item --> update BSID
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< end   insert note  0324742
* Customer account --> update BSID
          UPDATE bsid SET xblnr = wk_xblnr
                    WHERE bukrs = bseg-bukrs
                      AND kunnr = bseg-kunnr
                      AND umsks = bseg-umsks
                      AND umskz = bseg-umskz
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-zuonr
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< start insert note 200576
          IF sy-subrc EQ 0.
            CALL FUNCTION 'OPEN_FI_PERFORM_00005010_P'
              EXPORTING
                i_chgtype     = 'U'
                i_origin      = 'J_1BNFPR UPDATE_BSID_NF_DOCUMENT'
                i_tabname     = 'BSID'
                i_where_bukrs = bseg-bukrs
                i_where_kunnr = bseg-kunnr
                i_where_umsks = bseg-umsks
                i_where_umskz = bseg-umskz
                i_where_augdt = bseg-augdt
                i_where_augbl = bseg-augbl
                i_where_zuonr = bseg-zuonr
                i_where_gjahr = bseg-gjahr
                i_where_belnr = bseg-belnr
                i_where_buzei = bseg-buzei
              EXCEPTIONS
                OTHERS        = 1.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ELSE.
            PERFORM check_error.                            " 793934
          ENDIF. " BSID update ok
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end insert note 200576
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< start insert note  0324742
        ENDIF. " Clearing status: BSID or BSAD
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< end   insert note  0324742
      ENDIF.  " Debitor Accounts, Note 793934


* Always try to udate BSIS, regardless of account type (Note 793934)

*  G/L account --> try to update BSIS
      UPDATE bsis SET xblnr = wk_xblnr
                WHERE bukrs = bseg-bukrs
*                    AND hkont = bseg-hkont                 "note 950968
*                    AND augdt = bseg-augdt                 "note 950968
*                    AND augbl = bseg-augbl                 "note 950968
*                    AND zuonr = bseg-zuonr                 "note 950968
                  AND gjahr = bseg-gjahr
                  AND belnr = bseg-belnr
                  AND buzei = bseg-buzei.
* Update only possible for G/L accounts with line item display
*   --> Print NF even for update failure in bsis
      CLEAR sy-subrc.
    ENDIF. " Reading BSEG
  ENDSELECT.

* perform unlocking of the fi document only after the update of BSID,
* because this table must also be locked during update - the following
* call function was before in the form update_fi_nf_document.

  CALL FUNCTION 'DEQUEUE_EFBKPF'
    EXPORTING
      bukrs  = xbukrs
      belnr  = xbelnr
      gjahr  = xgjahr
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                               " UPDATE_BSID_NF_DOCUMENT

*---------------------------------------------------------------------*
*       FORM ENQUEUE_BI_FI                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM enqueue_bi_fi.

  CLEAR bi_subrc.

* sort the wk_item to get the first item
  SORT wk_item.
  READ TABLE wk_item INDEX 1.

  CHECK wk_item-reftyp = 'BI'.
  MOVE wk_item-refkey TO key_vbrk.

  PERFORM read_bi_document.

  IF NOT vbrk IS INITIAL.              "call via SD
    CALL FUNCTION 'ENQUEUE_EVVBRKE'
      EXPORTING
        mandt          = sy-mandt
        vbeln          = key_vbrk-vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    bi_subrc = sy-subrc.
    PERFORM check_error.
    IF bi_subrc = 0.                   "BI document not locked
      CLEAR bkpf.
      PERFORM get_fi_number.
      IF NOT bkpf-belnr IS INITIAL.
        CALL FUNCTION 'ENQUEUE_EFBKPF'
          EXPORTING
            bukrs          = bkpf-bukrs
            belnr          = bkpf-belnr
            gjahr          = bkpf-gjahr
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        fi_subrc = sy-subrc.
        PERFORM check_error.
        IF fi_subrc <> 0.     "FI lock not successful -> release BI lock
          CALL FUNCTION 'DEQUEUE_EVVBRKE'
            EXPORTING
              mandt  = sy-mandt
              vbeln  = key_vbrk-vbeln
            EXCEPTIONS
              OTHERS = 1.
          PERFORM check_error.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " ENQUEUE_BI_FI

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< start insert note 442570
*&---------------------------------------------------------------------*
*&      Form  check_nf_canceled
*&---------------------------------------------------------------------*
*       allow print of NF only when NF is not canceled
*----------------------------------------------------------------------*
FORM check_nf_canceled.
  DATA: lv_dummy  TYPE c.

  IF NOT wk_header-cancel IS INITIAL AND wk_header-nfnum IS INITIAL.
    sy-subrc = 1.
    MESSAGE ID '8B'
            TYPE 'E'
            NUMBER '678'
            WITH wk_header-docnum
            INTO lv_dummy.

    PERFORM check_error.
    IF sy-batch IS INITIAL.                " corr. of note 442570
      MESSAGE e678 WITH wk_header-docnum.
    ENDIF.                                 " corr. of note 442570
  ENDIF.
ENDFORM.                    " check_nf_canceled
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< end   insert note   442570

*boi                     "note 768693
*&---------------------------------------------------------------------*
*&      Form  print-popup
*&---------------------------------------------------------------------*
*       shows a popup to selct output form of Nota Fiscal
*----------------------------------------------------------------------*

FORM print-popup.

  CALL FUNCTION 'POPUP_WITH_3_BUTTONS_TO_CHOOSE'
    EXPORTING
*   DEFAULTOPTION       = '1'
    diagnosetext1       = text-100
*   DIAGNOSETEXT2       = ' '
*   DIAGNOSETEXT3       = ' '
    textline1           = ''
*   TEXTLINE2           = ' '
*   TEXTLINE3           = ' '
    text_option1        = c_sapscript
    text_option2        = c_smartforms
    text_option3        = c_adobe
    titel               = text-102
   IMPORTING
      answer              = print_options.

*which form should be printed
  IF print_options = '1'.
    PERFORM printing.
  ENDIF.
  IF print_options = '2'.
    PERFORM smart_sub_printing.
  ENDIF.
  IF print_options = '3'.
    PERFORM adobe_sub_printing.
  ENDIF.
ENDFORM.                    "print-popup

*&---------------------------------------------------------------------*
*&      Form  print-popup_2buttons
*&---------------------------------------------------------------------*
*       shows a popup to selct output form of Nota Fiscal
*----------------------------------------------------------------------*

FORM print-popup_2buttons USING button1 button2 print_options.

  CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
    EXPORTING
*   DEFAULTOPTION       = '1'
      diagnosetext1       = text-100
*   DIAGNOSETEXT2       = ' '
*   DIAGNOSETEXT3       = ' '
      textline1           = ''
*   TEXTLINE2           = ' '
*   TEXTLINE3           = ' '
      text_option1        = button1
      text_option2        = button2
      titel               = text-102
   IMPORTING
      answer              = print_options.

ENDFORM.                    "print-popup
*eoi                      "note 768693
*&---------------------------------------------------------------------*
*&      Form  ACTIVE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM active_update .

  UPDATE j_1bnfe_active FROM gs_nfeactive.

  IF sy-subrc <> 0.
    MESSAGE a021(j1b_nfe) WITH gs_nfeactive-docnum.
  ENDIF.

ENDFORM.                    " active_update
*&---------------------------------------------------------------------*
*&      Form  CHECK_NFE_AUTHORIZED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_nfe_authorized.
  DATA: lv_dummy  TYPE c,
        lv_subrc  TYPE sy-subrc,
        obj_ref   TYPE REF TO if_ex_cl_nfe_print.

  CLEAR gs_nfeactive.
* only NFes
  CHECK wk_header-nfe = 'X'.
*
  SELECT SINGLE * FROM j_1bnfe_active INTO gs_nfeactive
  WHERE docnum = wk_header-docnum.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e012 WITH wk_header-docnum.
  ENDIF.

* don't print NF-e when ...
* ... rejected docsta = 2
* ... denied   docsta = 3
* ... switches manual to contingency
  IF gs_nfeactive-conting_s = 'X'
  OR gs_nfeactive-docsta    = '2'
  OR gs_nfeactive-docsta    = '3'.
*
    lv_subrc = 1.
  ELSE.
*-- don´t print not authorized NFes
    IF  wk_header-authcod IS INITIAL    "Nfe is not authorized
    AND wk_header-conting IS INITIAL.   "and not in contingency
      lv_subrc = 1.
    ENDIF.
  ENDIF.
*-- BADI for reset subrc
*-- When subrc is 0 NFes can be printed without aauthorization code
*
  IF obj_ref IS INITIAL.

    CALL METHOD cl_exithandler=>get_instance       " #EC CI_BADI_GETINST
    EXPORTING
      exit_name                     = 'CL_NFE_PRINT'
      null_instance_accepted        = seex_false
    CHANGING
      instance                      = obj_ref
    EXCEPTIONS
      no_reference                  = 1
      no_interface_reference        = 2
      no_exit_interface             = 3
      class_not_implement_interface = 4
      single_exit_multiply_active   = 5
      cast_error                    = 6
      exit_not_existing             = 7
      data_incons_in_exit_managem   = 8
      OTHERS                        = 9.

    IF sy-subrc IS INITIAL.
*- nothing to do
    ENDIF.

  ENDIF.
*
  IF obj_ref IS BOUND.
    CALL METHOD obj_ref->reset_subrc
      EXPORTING
        is_nfdoc = wk_header
      CHANGING
        ch_subrc = lv_subrc.
  ENDIF.

  sy-subrc = lv_subrc.
*
  IF sy-subrc IS NOT INITIAL.
    IF gs_nfeactive-conting_s = 'X'.
      MESSAGE ID 'J1B_NFE'
      TYPE 'E'
      NUMBER '040'
      WITH wk_header-docnum
      INTO lv_dummy.
    ELSE.
      MESSAGE ID 'J1B_NFE'
      TYPE 'E'
      NUMBER '039'
      WITH wk_header-docnum
      INTO lv_dummy.
    ENDIF.
    PERFORM check_error.
    IF sy-batch IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    gs_nfeactive-printd = 'X'.
  ENDIF.

ENDFORM.                    " check_nfe_authorized

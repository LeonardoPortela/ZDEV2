***********************************************************************
* Original zur Sicherheit beibehalten
* M. Osterloh 4.8.2004
***********************************************************************

REPORT ZFIY0006
  MESSAGE-ID fr
  LINE-SIZE 80 " 132
  LINE-COUNT 100(3)  " 65(3)
  NO STANDARD PAGE HEADING.

TYPE-POOLS: icon.
*---------------------------------------------------------------------*
*    Tables                                                           *
*---------------------------------------------------------------------*
TABLES: b0sg, bkpf, bseg, bsega, bsec, kna1, lfa1, ska1, skat,
        t001, t009, t009y, bhdgd, trvor, t003t, teurb.
TABLES: sscrfields.                                    "CSS 142437/99
TABLES: faglfreesel, fagl_trvor.
CONSTANTS gc_ok_dummy TYPE syucomm VALUE 'DUMMY'.
*---------------------------------------------------------------------*
DATA: BEGIN OF fimsg.
        INCLUDE STRUCTURE fimsg.
DATA: END   OF fimsg,
*---------------------------------------------------------------------*
      BEGIN OF t_fimsg OCCURS 100.
        INCLUDE STRUCTURE fimsg.
DATA: END   OF t_fimsg,
* T        contains all document positions
      BEGIN OF t        OCCURS  255,
        buzei(3)      TYPE c,          "position number
        koart         LIKE bseg-koart, "account type
        hkont(10)     TYPE c,          "G/L account
        konto         LIKE kna1-kunnr, "cust/vend account or alt. acc.
        kontm         LIKE bseg-hkont, "customer/Vendor account
        gsber         LIKE bseg-gsber, "business area
        bschl         LIKE bseg-bschl, "posting key
        xnegp         LIKE bseg-xnegp, "negativ posting
        umskz         LIKE bseg-umskz, "business volume indicator
        mwskz         LIKE bseg-mwskz, "VAT code
        sgtxt         LIKE bseg-sgtxt, "segment tex
        txt20         LIKE skat-txt20, "G/L account text
        waers         LIKE bkpf-waers, "currency
        wrshb(9)      TYPE p,          "credit/debit currency amount
        dmsol(9)      TYPE p,          "debit amount
        dmhab(9)      TYPE p,          "credit amount
        cpdkz(1)      TYPE c,
        name1         LIKE bsec-name1,
        name2         LIKE bsec-name2,
        stras         LIKE bsec-stras,
        pstlz         LIKE bsec-pstlz,
        ort01         LIKE bsec-ort01,
        regio         LIKE bsec-regio,
        docln         LIKE bseg-docln,
      END OF t,
* S        contains credit and debit amounts of various account types
*          sorted by: fiscal year, currency, company code, accounting
*                     period
      BEGIN OF s        OCCURS  400,
        jahr(4)       TYPE c,          "Fiscal year
        waers(5)      TYPE c,          "Currency
        bukrs(4)      TYPE c,          "Company code
        mon(3)        TYPE c,          "Accounting period
        ssol(9)       TYPE p,          "Debit G/L account
        shab(9)       TYPE p,          "Credit G/L account
        dsol(9)       TYPE p,          "Debit customer account
        dhab(9)       TYPE p,          "Credit customer account
        ksol(9)       TYPE p,          "Debit vendor account
        khab(9)       TYPE p,          "Credit vendor account
      END OF s,
* WRSUM keeps the totals of each foreign currency for one posting date
* and the corresponding debit and credit amounts in local currency
* table has to be cleared for each new posting date
      BEGIN OF wrsum OCCURS 30,
           waers  LIKE t-waers,
           fwbtr  LIKE t-wrshb,
           hwsol  LIKE t-dmsol,
           hwhab  LIKE t-dmhab,
      END OF wrsum,
*------- Record definition --------------------------------------------
      BEGIN OF k,                      "Header title
        bubl(17)  TYPE c VALUE ' ',
        space1(1),
        budat(6)  TYPE c VALUE ' ',
        space2(1),
        bldat(6)  TYPE c VALUE ' ',
        space3(1),
        blart     LIKE bkpf-blart VALUE ' ',
        space4(1),
        xblnr     LIKE bkpf-xblnr VALUE ' ',
        space5(1),
        bktxt     LIKE bkpf-bktxt VALUE ' ',
        space6(1),
        usnam     LIKE bkpf-usnam VALUE ' ',
        space7(1),
        xblnr_alt LIKE bkpf-xblnr VALUE ' ',
        space8(1),
        budat_long TYPE d,
        bldat_long TYPE d,
        cpudt_long TYPE d,
        belnr    LIKE bkpf-belnr,
        stblg    LIKE bkpf-stblg,
        stjah    LIKE bkpf-stjah,
        docnr    LIKE faglflexa-docnr,
        monat    LIKE bkpf-monat,                           "note979647
      END OF k,
*----------------------------------------------------------------------*
*     begin of p1,                     "detail line 1
*       text(25)  type c,
*       space1(1),
*       buzei(3)  type c,
*       space2(1),
*       koart     like bseg-koart,
*       space3(1),
*       konto(10) type c,
*       space4(1),
*       gsber     like bseg-gsber,
*       space5(1),
*       bschl     like bseg-bschl,
*       space6(1),
*       umskz     like bseg-umskz,
*       space7(1),
*       hkont(10) type c,
*       space8(1),
*       mwskz     like bseg-mwskz,
*     end of p1,
*----------------------------------------------------------------------*
      BEGIN OF p1,                     "detail line 1
        text(25)  TYPE c,
        space1(1),
        buzei(6)  TYPE c,
        space2(1),
        koart     LIKE bseg-koart,
        space3(1),
        konto(10) TYPE c,
        space4(1),
        gsber     LIKE bseg-gsber,
        space5(1),
        bschl     LIKE bseg-bschl,
        umskz     LIKE bseg-umskz,
        xnegp     LIKE bseg-xnegp,
        space6(1),
        hkont(10) TYPE c,
        space7(1),
        mwskz     LIKE bseg-mwskz,
      END OF p1,
*----------------------------------------------------------------------*
      BEGIN OF p2,                     "detail line 2
        text(35)  TYPE c,
        space(1),
        sgtxt     LIKE bseg-sgtxt,
      END OF p2,
*----------------------------------------------------------------------*
      BEGIN OF p3,                     "detail line 3
        text(35)  TYPE c,
      END OF p3,
*----------------------------------------------------------------------*
      BEGIN OF p4,                     "detail line 4
        text(35)  TYPE c,
      END OF p4,
*----------------------------------- detail information microfiche line
      BEGIN OF mikfi,
        bukrs        LIKE bkpf-bukrs,
        gjahr        LIKE bkpf-gjahr,
        belnr        LIKE bkpf-belnr,
        budat        LIKE bkpf-budat,
      END OF mikfi.
*---------------------------------------------------- data for OPEN FI
DATA: BEGIN OF bkpf_addon_tab.
        INCLUDE STRUCTURE bkpf_addon_tab.
DATA: END   OF bkpf_addon_tab,

      BEGIN OF t_bkpf_addon_tab OCCURS 0.
        INCLUDE STRUCTURE bkpf_addon_tab.
DATA: END   OF t_bkpf_addon_tab.

DATA: store_tab(1)     TYPE c VALUE ' ',
      check_open_fi(1) TYPE c VALUE 'X'.

DATA:      split_s(1) TYPE c VALUE ' '.
CONSTANTS: max_length TYPE p VALUE '999999999999999',
           max_l_page TYPE p VALUE '99999999999999'.
*------------------------------------------------------"CSS 142437/99
CONSTANTS: max_doc   TYPE i VALUE '1000'.              "CSS 142437/99
DATA:      count_doc TYPE i VALUE '0',                 "CSS 142437/99
           spool(1)  TYPE c VALUE ' '.                 "CSS 142437/99
*----------------------------------------------------------------------*
*    Fields                                                            *
*----------------------------------------------------------------------*
DATA: flag(1)        TYPE c,
      t_flag(1)      TYPE c,
      s_flag(1)      TYPE c VALUE '0',
      wbudat         TYPE d,
      highdat        TYPE d,
*     LOWBUDAT       TYPE D,
      z_flag(1)      TYPE c,
      vzaehl(8)      TYPE p,
      anzahl         TYPE p,
      znummu(8)      TYPE c VALUE ' ',
      ph_text1(132)  TYPE c VALUE ' ',
      ph_flag(1)     TYPE c,
      textflag(1)    TYPE c,
      wzaehl(3)      TYPE c,
      pzaehl(3)      TYPE p,
      entries_gjahr  LIKE bkpf-gjahr,
*     ENTRIES_BUKR   LIKE BKPF-BUKRS,
      entries_bukr   TYPE p,
      entries_dat    LIKE bkpf-budat,
      wssol(9)       TYPE p,
      wshab(9)       TYPE p,
      wasol(9)       TYPE p,
      wahab(9)       TYPE p,
      whsol(9)       TYPE p,
      whhab(9)       TYPE p,
      bukrs_bk       LIKE bkpf-bukrs,
      wjahr(4)       TYPE c,
      wmon(3)        TYPE c,
*     WKKONTO        LIKE BSEG-LIFNR,
*     WSKONTO        LIKE BSEG-HKONT,
      sumso(9)       TYPE p,
      sumha(9)       TYPE p,
      von_datum      TYPE d VALUE '19010101',
      bis_datum      TYPE d VALUE '99991231',
      budat_low      TYPE d,           "date parameter BTE 00003310
      budat_high     TYPE d,           "date parameter BTE 00003310
      h_konto        LIKE bseg-saknr,  "Aux. field for acc. no.
      h_kontm        LIKE bseg-hkont,
      h_altnf,                         "Return from call function
      h_monat LIKE bkpf-monat VALUE '01',"To find first day
      h_monat2       LIKE t001b-frpe1, "for period check
      h_poper LIKE glu1-poper VALUE '001',"To find first day
      h_year         LIKE bkpf-gjahr,
      firstday       LIKE sy-datum,
      firstday_sdoct LIKE sy-datum,    "for special doc. types (italy)
      lastday        LIKE sy-datum,    "last posting date of list
      prev_year      LIKE bkpf-gjahr,
      ww_date        LIKE bkpf-budat,
      h_ktext        LIKE skat-txt20,
      sw_chk.
DATA: fagl_active    TYPE boole_d,     "flexible GL active
      gd_rldnr       TYPE fagl_rldnr.  "used ledger
DATA: auditor        TYPE c.
DATA: h_date         LIKE trvor-budat. "<---INSERT 66911
DATA: bukrs_waers    LIKE t001-waers.
DATA: poland         TYPE c,
      listanfg       TYPE c.
DATA: italy          TYPE c,
      pagno          LIKE trvor-pagno,
      w_pagno        TYPE string,
      pagno_year     TYPE pageno_year.
DATA  turkey         TYPE c.                                "n1045456
CONSTANTS: slash     TYPE c VALUE '/'.

*---------------------------------------------------------------------*
*       Daten für NewGl                                               *
*---------------------------------------------------------------------*
DATA: gs_fagl_s_doc_details TYPE  fagl_s_doc_details.
DATA: gs_t001  LIKE t001.
DATA: gs_orginfo LIKE  glx_org_info.
DATA: BEGIN OF gt_periv OCCURS 0,
        periv LIKE t001-periv,
      END OF gt_periv.
DATA: gd_periv LIKE t001-periv.
DATA: gd_lines TYPE i.
*data: gd_rldnr type rldnr.
TYPE-POOLS: rsds.

DATA: gd_gjahr LIKE bkpf-gjahr.
DATA: gd_belnr LIKE bkpf-belnr.
DATA: gd_buzei LIKE glu1-docln.

DATA: gd_save_gjahr LIKE bkpf-gjahr.
DATA: gd_monat_low  LIKE glu1-poper.
DATA: gd_monat_high LIKE glu1-poper.
DATA: gd_budat_low  LIKE bkpf-budat.
DATA: gd_budat_high LIKE bkpf-budat.

DATA: gd_report LIKE rsvar-report.

DATA: gt_dynsel    TYPE rsds_trange.
DATA: gs_trange_tab_line TYPE rsds_range.
DATA: gs_frange_tab_line TYPE rsds_frange.
DATA: gs_selopt      TYPE rsdsselopt.
DATA: gs_selopt_t    TYPE rsds_selopt_t.

DATA: gd_xdel        TYPE xfeld.

DATA: it_bukrs TYPE bukrs OCCURS 0 WITH HEADER LINE.
DATA: gd_xbudat TYPE xfeld.

DATA: gd_buper  LIKE t009b-poper.
DATA: gd_curry  LIKE bkpf-gjahr.
RANGES: gt_gjahr FOR bkpf-gjahr.

DATA:   gs_variant LIKE disvariant.

DATA:   gd_name2 LIKE kna1-name2.
DATA:   gd_pstlz LIKE kna1-pstlz.
DATA:   gd_ort01 LIKE kna1-ort01.
DATA:   gd_stras LIKE kna1-stras.
DATA:   hlp_belnr(10)  TYPE c.              "Anzeigefeld Belegnummer

*     CHAR_LEN       TYPE P.                                      "KANJI
*----------------------------------------------------------------------*
*    Selections and Parameters                                         *
*----------------------------------------------------------------------*
begin_of_block 1.
* Include/exclude specific accounts
SELECT-OPTIONS s_hkont FOR bseg-hkont.
* Include/exclude statistical postings
SELECT-OPTIONS s_ktoks FOR ska1-ktoks.                      "n1149977
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS     s_belege LIKE rfpdo-allgstat.
SELECTION-SCREEN COMMENT 03(70) text-c04 FOR FIELD s_belege.
SELECTION-SCREEN END   OF LINE.
end_of_block 1.                        "accounts

* Processing options
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-f01.
* Test run or update run
PARAMETERS test     LIKE rfpdo1-allgtest DEFAULT 'X' MODIF ID tst.
SELECTION-SCREEN END   OF BLOCK b01.

* Output control: Header
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-f02.
* Switch Off Header Output
PARAMETERS noheader LIKE rfpdo2-umsvhead DEFAULT 'X'
  USER-COMMAND gc_ok_dummy.
* Posting Date in Title
PARAMETERS p_date   LIKE rfpdo2-beldates DEFAULT ' ' MODIF ID hdr NO-DISPLAY.
* VAT ID in header
PARAMETERS p_vat    NO-DISPLAY. "AS CHECKBOX.                            "note990329
* Additional header
PARAMETERS title    LIKE rfpdo-allgline   NO-DISPLAY."            MODIF ID hdr.
SELECTION-SCREEN END   OF BLOCK b02.

* Output control: List
*SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-f03.
PARAMETERS p_all    DEFAULT ' ' NO-DISPLAY. "RADIOBUTTON GROUP r1 " DEFAULT 'X'.
* Print totals only
PARAMETERS tabsum   LIKE rfpdo1-beljsumm DEFAULT ' ' NO-DISPLAY.
*                    RADIOBUTTON GROUP r1.
* Print items only
PARAMETERS posten   LIKE rfpdo1-beljpost DEFAULT 'X' NO-DISPLAY.
*                    RADIOBUTTON GROUP r1 DEFAULT 'X'.
*SELECTION-SCREEN SKIP.

* General ledger view
*SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS par_docc  LIKE b0sg-xdocc MODIF ID xgl DEFAULT 'X' NO-DISPLAY.
*SELECTION-SCREEN COMMENT 03(70) text-c01 FOR FIELD par_docc.
*SELECTION-SCREEN END OF LINE.

* Print Line Item Text
PARAMETERS segmtext LIKE rfpdo1-beljsetx DEFAULT ' ' NO-DISPLAY.
* Display Alternative Account Number
*SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_altkt  LIKE rfpdo1-allgaltk DEFAULT ' ' NO-DISPLAY.
*SELECTION-SCREEN COMMENT 03(70) text-c02 FOR FIELD p_altkt.
*SELECTION-SCREEN END OF LINE.

* Display Reconciliation Account instead of Customer/Vendor Account
*SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_accseq LIKE rfpdo2-beljhkto DEFAULT ' ' NO-DISPLAY.
*SELECTION-SCREEN COMMENT 03(70) text-c03 FOR FIELD p_accseq.
*SELECTION-SCREEN END OF LINE.
* Do not print CPU date
PARAMETERS no_cpu   LIKE rfpdo2-no_cpu   DEFAULT 'X' NO-DISPLAY.

*SELECTION-SCREEN SKIP.

* Number of Lines for Master Data Texts
PARAMETERS textline LIKE rfpdo1-beljtxli DEFAULT '1' NO-DISPLAY.
* Continuous Document Number
PARAMETERS z_nummer LIKE rfpdo1-beljznum DEFAULT ' ' NO-DISPLAY.
*SELECTION-SCREEN END   OF BLOCK b03.

*SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-f04.
* Totals Per Page
PARAMETERS seit_sum LIKE rfpdo1-beljsesu DEFAULT 'X' NO-DISPLAY.
* Totals Per Posting Date
PARAMETERS budatsum LIKE rfpdo1-beljbusu DEFAULT ' ' NO-DISPLAY.
SELECTION-SCREEN SKIP.
* Sort Key
PARAMETERS sort_key LIKE rfpdo1-beljsort DEFAULT '2' NO-DISPLAY.
SELECTION-SCREEN SKIP.
* Document Line Compression
PARAMETERS compress LIKE rfpdo1-beljcomp DEFAULT ' ' NO-DISPLAY.
* Business Area Compression
PARAMETERS gb_comp  LIKE rfpdo1-beljcogb DEFAULT 'X' NO-DISPLAY.
*SELECTION-SCREEN END   OF BLOCK b04.

*SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE text-f05.
SELECT-OPTIONS s_sblart  FOR t003t-blart NO-DISPLAY.          "needed for Italy
SELECT-OPTIONS s_scpudt  FOR sy-datum    NO-DISPLAY.             "needed for Italy
*SELECTION-SCREEN END   OF BLOCK b05.

*begin_of_block 8.
PARAMETERS:
  listsep  LIKE rfpdo1-allglise NO-DISPLAY,       "X - Listseparation
  p_miffl  LIKE rfpdo-allgmikf  NO-DISPLAY.        "X - Microfich-Information
*end_of_block 8.

*SELECTION-SCREEN PUSHBUTTON
*  1(70) gd_push USER-COMMAND alv VISIBLE LENGTH 30.
*----------------------------------------------------------------------*
*    Extract                                                           *
*----------------------------------------------------------------------*
FIELD-GROUPS: header,
              dates.
INSERT:
  bukrs_bk                             "Company code
  gd_belnr                             "Document number
  wjahr                                "Fiscal year
  wmon                                 "Accounting period
  wbudat                               "Posting date
  bkpf-cpudt                           "CPU date
  bkpf-bstat                           "Document status
  gd_buzei                              "Position number
  wzaehl                                                    "
  INTO header.
*----------------------------------------------------------------------*
INSERT:
  t001-waers                           "Company code currency
  whsol                                                     "
  whhab                                                     "
  t-dmsol                              "Debit position amount
  t-dmhab                              "Credit position amount
  t-koart                              "Account type position
  t-umskz                              "Business vol. indic. position
  t-wrshb                              "Currency debit position amount
  t-waers                              "Currency position
  p1                                   "Detail line 1
  p2                                   "Detail line 2
  p3                                   "Detail line 3
  p4                                   "Detail line 4
  k                                    "Header title
  bkpf-budat                           "Document posting date
  bkpf-mandt
  bkpf-bukrs
  bkpf-gjahr
  bkpf-belnr                           "Belegnummer
  bseg-buzei                           "Buchungszeile
  bkpf-waers
  bkpf-usnam
  bkpf-stblg
  bkpf-stjah
  gs_fagl_s_doc_details-ryear
  gs_fagl_s_doc_details-poper
  gs_fagl_s_doc_details-docnr
  bseg-docln
  bseg-sgtxt
  gd_name2
  gd_pstlz
  gd_ort01
  gd_stras
INTO dates.
*---------------------------------------------------------------------*
*    Ranges                                                           *
*---------------------------------------------------------------------*
RANGES: r_monat FOR bkpf-monat.
*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*---------------------------------------------------------------------*
INITIALIZATION.

* check if user is an auditor
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = 'CA_USER_EXISTS'
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc = 0.
    CALL FUNCTION 'CA_USER_EXISTS'
      EXPORTING
        i_user       = sy-uname
      EXCEPTIONS
        user_missing = 1.
    IF sy-subrc = 0.
      auditor = 'X'.
    ENDIF.
  ENDIF.

  get_frame_title: 1. ", 8.
* check if flexible GL is active
  CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIV_CLIENT'
    EXPORTING
      client          = sy-mandt
    IMPORTING
      e_glflex_active = fagl_active.
  IF sy-subrc <> 0.
    CLEAR fagl_active.
  ENDIF.

  GET PARAMETER ID 'BUK' FIELD br_bukrs-low.
  IF br_bukrs-low NE space.
    br_bukrs-option = 'EQ'.
    br_bukrs-sign   = 'I'.
    APPEND br_bukrs.
    IF fagl_active = 'X'.
      READ TABLE br_rldnr INDEX 1.
      IF br_rldnr-low IS INITIAL.
        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
*       EXPORTING
*         I_CLIENT            = SY-MANDT
        IMPORTING
          e_rldnr             = gd_rldnr
        EXCEPTIONS
          not_found           = 1
          more_than_one       = 2
          OTHERS              = 3
                .
      ELSE.
        gd_rldnr = br_rldnr-low.
      ENDIF.

      CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
        EXPORTING
          i_rldnr                        = gd_rldnr
          i_orgunit                      = br_bukrs-low
*         JOIN_OF_VALUTYP_AND_CURT       = 'X'
*         SEND_ERROR_WHEN_DEPLD          = ' '
        IMPORTING
          organizational_info            = gs_orginfo
*       EXCEPTIONS
*         NO_INFO_FOUND                  = 1
*         ERROR_IN_SETUP                 = 2
*         ERROR_IN_DEPLD                 = 3
*         OTHERS                         = 4
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
        EXPORTING
          i_date         = sy-datum
          i_periv        = gs_orginfo-periv
        IMPORTING
          e_buper        = gd_buper
          e_gjahr        = gd_curry
        EXCEPTIONS
          t009_notfound  = 4
          t009b_notfound = 4
          input_false    = 4.
      br_gjahr-low =    gd_curry.
      br_gjahr-option   = 'EQ'.
      br_gjahr-sign     = 'I'.
      APPEND br_gjahr.
      r_monat-low      = gd_buper.
      r_monat-option   = 'EQ'.
      r_monat-sign     = 'I'.
      APPEND r_monat.


      CALL FUNCTION 'G_POSTING_DATE_OF_PERIOD_GET'
        EXPORTING
          period                    = gd_buper
          variant                   = gs_orginfo-periv
          year                      = gd_curry
        IMPORTING
          from_date                 = von_datum
*         LAST_NORMAL_PERIOD        =
          to_date                   = bis_datum
*         FROM_DATE_ORIG            =
*       EXCEPTIONS
*         PERIOD_NOT_DEFINED        = 1
*         VARIANT_NOT_DEFINED       = 2
*         OTHERS                    = 3
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      br_budat-low = von_datum.
      br_budat-high = bis_datum.
      br_budat-option = 'BT'.
      br_budat-sign = 'I'.
      APPEND br_budat.
    ELSE.
      SELECT SINGLE * FROM  t001
             WHERE  bukrs       = br_bukrs-low   .
      CALL FUNCTION 'GET_CURRENT_YEAR'
        EXPORTING
          bukrs = br_bukrs-low
          date  = sy-datum
        IMPORTING
          currm = r_monat-low
          curry = br_gjahr-low.
      br_gjahr-option   = 'EQ'.
      br_gjahr-sign     = 'I'.
      APPEND br_gjahr.
      r_monat-option   = 'EQ'.
      r_monat-sign     = 'I'.
      APPEND r_monat.
      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
        EXPORTING
          i_gjahr = br_gjahr-low
          i_monat = r_monat-low
          i_periv = t001-periv
        IMPORTING
          e_fday  = von_datum
          e_lday  = bis_datum.
      br_budat-low = von_datum.
      br_budat-high = bis_datum.
      br_budat-option = 'BT'.
      br_budat-sign = 'I'.
      APPEND br_budat.
    ENDIF.
  ENDIF.

*  CALL FUNCTION 'ICON_CREATE'
*    EXPORTING
*      name       = icon_alv_variants
*      text       = text-b01
*      add_stdinf = space
*    IMPORTING
*      RESULT     = gd_push.


*---------------------------------------------------------------------*
*        SELECTION-SCREEN                                             *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* make testflag invisible for auditors
  LOOP AT SCREEN.
    IF auditor = 'X'.
      IF  screen-group1 EQ 'TST'.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    IF noheader = 'X'.
      IF screen-group1 EQ 'HDR'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 EQ 'HDR'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'ALV'.
    SUBMIT rfbelj10
      WITH br_bukrs IN br_bukrs
      WITH br_belnr IN br_belnr
      WITH br_gjahr IN br_gjahr
      WITH br_rldnr IN br_rldnr
      WITH br_budat IN br_budat
      WITH br_xblnr IN br_xblnr
      WITH s_hkont  IN s_hkont
      WITH par_docc =  par_docc
      WITH p_altkt  =  p_altkt
      WITH p_accseq =  p_accseq
      WITH textline =  textline
      WITH s_sblart IN s_sblart
      WITH s_scpudt IN s_scpudt
      VIA SELECTION-SCREEN AND RETURN.
  ENDIF.
  IF sscrfields-ucomm = 'PRIN'                         "CSS 142437/99
    OR sscrfields-ucomm = 'SJOB'.                      "CSS 142437/99
    spool = 'X'.                                     "CSS 142437/99
  ENDIF.                                               "CSS 142437/99

  gt_gjahr[] = br_gjahr[].

  DESCRIBE TABLE br_budat LINES entries_dat.
  IF entries_dat GT 1.
    LOOP AT br_budat.
      IF sy-tabix > 1.
        DELETE br_budat.
      ENDIF.
    ENDLOOP.
    CLEAR br_budat.
    READ TABLE br_budat INDEX 1.
    MESSAGE w304 WITH br_budat-low br_budat-high.
  ELSE.
    READ TABLE br_budat INDEX 1.
  ENDIF.
  budat_low  = br_budat-low.                              "BTE 00003310
  budat_high = br_budat-high.                             "BTE 00003310
  DESCRIBE TABLE br_bukrs LINES entries_bukr.
  IF entries_bukr > 1.
    LOOP AT br_bukrs.
      IF sy-tabix > 1.
        DELETE br_bukrs.
      ENDIF.
    ENDLOOP.
    READ TABLE br_bukrs INDEX 1.
    br_bukrs-high = space.
    br_bukrs-option = 'EQ'.
    br_bukrs-sign = 'I'.
    MODIFY br_bukrs INDEX 1.
    MESSAGE w303 WITH br_bukrs-low.
  ELSE.
    READ TABLE br_bukrs INDEX 1.
    IF sy-subrc EQ 0.
      IF br_bukrs-option NE 'EQ'
        OR br_bukrs-sign NE 'I'.
        br_bukrs-high = space.
        br_bukrs-option = 'EQ'.
        br_bukrs-sign = 'I'.
        MODIFY br_bukrs INDEX 1.
        MESSAGE w303 WITH br_bukrs-low.
      ENDIF.
    ELSE.
      MESSAGE e180.
    ENDIF.
  ENDIF.
  DESCRIBE TABLE br_gjahr LINES entries_gjahr.
  IF entries_gjahr NE 1.
    LOOP AT br_gjahr.
      IF sy-tabix > 1.
        DELETE br_gjahr.
      ENDIF.
    ENDLOOP.
    READ TABLE br_gjahr INDEX 1.
    br_gjahr-high = space.
    br_gjahr-option = 'EQ'.
    br_gjahr-sign = 'I'.
    MODIFY br_gjahr INDEX 1.
    MESSAGE w386 WITH br_gjahr-low.
  ELSE.
    READ TABLE br_gjahr INDEX 1.
    IF sy-subrc EQ 0.
      IF br_gjahr-option NE 'EQ'
        OR br_gjahr-sign NE 'I'.
        br_gjahr-high = space.
        br_gjahr-option = 'EQ'.
        br_gjahr-sign = 'I'.
        MODIFY br_gjahr INDEX 1.
        MESSAGE w386 WITH br_gjahr-low.
      ENDIF.
    ELSE.
      CALL FUNCTION 'GET_CURRENT_YEAR'
        EXPORTING
          bukrs = br_bukrs-low
          date  = sy-datum
        IMPORTING
          curry = br_gjahr-low.
      br_gjahr-high = space.
      br_gjahr-option = 'EQ'.
      br_gjahr-sign = 'I'.
      APPEND br_gjahr.
      MESSAGE w386 WITH br_gjahr-low.
    ENDIF.
  ENDIF.
*------------------ posting periods should be closed for production run
  h_monat2 = h_monat.
  IF br_gjahr-high IS INITIAL.
    h_year = br_gjahr-low.
  ELSE.
    h_year = br_gjahr-high.
  ENDIF.
  IF fagl_active = 'X'.
    READ TABLE br_rldnr INDEX 1.
    IF br_rldnr-low IS INITIAL.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
*       EXPORTING
*         I_CLIENT            = SY-MANDT
      IMPORTING
        e_rldnr             = gd_rldnr
      EXCEPTIONS
        not_found           = 1
        more_than_one       = 2
        OTHERS              = 3
              .
    ELSE.
      gd_rldnr = br_rldnr-low.
    ENDIF.

    CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
    EXPORTING
      i_rldnr                        = gd_rldnr
      i_orgunit                      = br_bukrs-low
*     JOIN_OF_VALUTYP_AND_CURT       = 'X'
*     SEND_ERROR_WHEN_DEPLD          = ' '
    IMPORTING
      organizational_info            = gs_orginfo
*       EXCEPTIONS
*         NO_INFO_FOUND                  = 1
*         ERROR_IN_SETUP                 = 2
*         ERROR_IN_DEPLD                 = 3
*         OTHERS                         = 4
            .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'FI_PERIOD_CHECK'
      EXPORTING
*       I_BUKRS                = BR_BUKRS-LOW
        i_opvar                = gs_orginfo-opvar
        i_gjahr                = h_year
        i_koart                = '+'
        i_monat                = h_monat2
        i_rldnr                = gd_rldnr
*       I_GLVOR                = 'RFBU'
*     IMPORTING
*       E_OPER                 =
      EXCEPTIONS
        error_period           = 1
        error_period_acc       = 2
*       INVALID_INPUT          = 3
        OTHERS                 = 4
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'FI_PERIOD_CHECK'
      EXPORTING
        i_bukrs          = br_bukrs-low
        i_gjahr          = h_year
        i_koart          = '+'
        i_monat          = h_monat2
      EXCEPTIONS
        error_period     = 1
        error_period_acc = 2
        OTHERS           = 3.
  ENDIF.
  IF sy-subrc = 0 AND test IS INITIAL.
    MESSAGE w485.
  ENDIF.
*----------------------------------------------------------------------

  flag = '0'.
  SELECT SINGLE * FROM  t001
         WHERE  bukrs       = br_bukrs-low.
*----------------------------------------- check special country codes
  CASE t001-land1.
    WHEN 'PL'.
      poland = 'X'.
    WHEN 'IT'.
      italy = 'X'.
    WHEN 'TR'.                                              "n1045456
      turkey = 'X'.                                         "n1045456
  ENDCASE.
* For flexGL check if ledger was inserted
  IF fagl_active = 'X'.
    IF br_rldnr[] IS INITIAL.
*     get leading ledger
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr   = gd_rldnr
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        MESSAGE ID   sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      gd_rldnr = br_rldnr-low.
    ENDIF.
    CALL FUNCTION 'G_POSTING_DATE_OF_PERIOD_GET'
      EXPORTING
        period    = h_poper
        variant   = gs_orginfo-periv
        year      = br_gjahr-low
      IMPORTING
        from_date = firstday.
  ELSE.
    CALL FUNCTION 'PERIOD_DAY_DETERMINE'
      EXPORTING
        i_gjahr = br_gjahr-low
        i_monat = h_monat
        i_periv = t001-periv
      IMPORTING
        e_fday  = firstday.
  ENDIF.

  IF fagl_active = 'X'.
    SELECT SINGLE * FROM fagl_trvor
           WHERE  repid       = sy-repid
           AND    rldnr       = gd_rldnr
           AND    bukrs       = br_bukrs-low
           AND    gjahr       = br_gjahr-low.
    IF sy-subrc NE 0.
      IF br_budat-low NE firstday .
        MESSAGE w439 WITH firstday.                         "note750331
      ENDIF.                                                "note750331
      IF test IS INITIAL.                                   "note750331
        br_budat-low = firstday.                            "note750331
        MODIFY br_budat INDEX 1.                            "note750331
      ENDIF.                                                "note750331
      flag = '1'.
    ELSEIF test NE 'X'.
      h_date = fagl_trvor-budat + 1.
      IF br_budat-low > h_date.
        MESSAGE e439 WITH h_date.
      ENDIF.
      IF br_budat-low < h_date.
        MESSAGE e440 WITH h_date.
      ENDIF.
      IF italy = 'X'.
        pagno = fagl_trvor-pagno.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM  trvor
           WHERE  repid       = sy-repid
           AND    bukrs       = br_bukrs-low
           AND    gjahr       = br_gjahr-low   .
    IF sy-subrc NE 0.
      IF br_budat-low NE firstday .
        MESSAGE w439 WITH firstday.                         "note750331
      ENDIF.                                                "note750331
      IF test IS INITIAL.                                   "note750331
        br_budat-low = firstday.                            "note750331
        MODIFY br_budat INDEX 1.                            "note750331
      ENDIF.                                                "note750331
      flag = '1'.
    ELSEIF test NE 'X'.                                "<---INSERT 66911
      h_date = trvor-budat + 1.          "<---INSERT 66911
      IF br_budat-low > h_date.          "<---INSERT 66911
        MESSAGE e439 WITH h_date.        "<---INSERT 66911
      ENDIF.                             "<---INSERT 66911
      IF br_budat-low < h_date.          "<---INSERT 66911
        MESSAGE e440 WITH h_date.        "<---INSERT 66911
      ENDIF.                             "<---INSERT 66911
      IF italy = 'X'.
        pagno = trvor-pagno.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  SET BLANK LINES OFF.
  MOVE listsep TO bhdgd-separ.
  MOVE p_miffl TO bhdgd-miffl.
  MOVE 'BUKRS' TO bhdgd-domai.
  MOVE '0'     TO bhdgd-inifl.
  MOVE sy-langu TO t003t-spras.
* NewGl view
  b0sg-xdocc = par_docc.
* clearing documents -------------------------------------------------*
  b0sg-xstaa = 'X'.
  b0sg-xstab = 'X'.
* Also statistical documents -----------------------------------------*
  b0sg-xstas = s_belege.

  PERFORM special_document.

*----------------------------------------------------------------------*
*        GET BKPF                                                     *
*----------------------------------------------------------------------*
GET bkpf.
  CHECK SELECT-OPTIONS.
*  CPD-KZ = '0'.

* Wenn ein Beleg aus dem Archiv kommt sollte auch der FAGL Baustein
* für die Details dies abhandeln
  CLEAR gs_fagl_s_doc_details.
  IF fagl_active = 'X'
  AND ( bkpf-bstat = ' ' OR bkpf-bstat = 'L' ).
    CALL FUNCTION 'FAGL_GET_DOC_DETAILS_OF_LEDGER'
      EXPORTING
        i_rldnr    = gd_rldnr
        i_bukrs    = bkpf-bukrs
        i_belnr    = bkpf-belnr
        i_gjahr    = bkpf-gjahr
*       I_ARCH     = 'X'
      IMPORTING
        es_details = gs_fagl_s_doc_details
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      gd_gjahr = bkpf-gjahr.
      gd_belnr = bkpf-belnr.
      CLEAR gd_buzei.
    ELSE.
      IF NOT par_docc IS INITIAL.
        gd_gjahr = gs_fagl_s_doc_details-ryear.
        gd_belnr = gs_fagl_s_doc_details-docnr.
        CLEAR gd_buzei.
      ELSE.
        gd_gjahr = bkpf-gjahr.
        gd_belnr = bkpf-belnr.
        CLEAR gd_buzei.
      ENDIF.
    ENDIF.
  ELSE.
    gd_gjahr = bkpf-gjahr.
    gd_belnr = bkpf-belnr.
    CLEAR gd_buzei.
  ENDIF.

  CLEAR sw_chk.
  PERFORM special_check.
  CHECK sw_chk IS INITIAL.

  bukrs_bk = bkpf-bukrs.

  pzaehl = 0.
  REFRESH t.
  t_flag = '0'.

  CLEAR k.

  IF NOT ( s_belege = 'X' OR bkpf-bstat = 'A' OR bkpf-bstat = 'B' ).
    CHECK ( bkpf-bstat EQ space OR bkpf-bstat = 'L' ).
  ENDIF.

  IF fagl_active = 'X'
  AND ( bkpf-bstat = ' ' OR bkpf-bstat = 'L' )
  AND NOT par_docc IS INITIAL.
    wjahr  = gs_fagl_s_doc_details-ryear.
    wmon   = gs_fagl_s_doc_details-poper.
    wbudat = bkpf-budat.
  ELSE.
    wjahr  = bkpf-gjahr.
    wmon   = bkpf-monat.
    wbudat = bkpf-budat.
  ENDIF.

  k-cpudt_long = bkpf-cpudt.

* Sort criteria: BELNR/BUDAT
  IF sort_key = '1'.
    MOVE: space TO k-bubl,
          gd_belnr TO k-bubl(10).
    IF no_cpu IS INITIAL.
      WRITE bkpf-cpudt DDMMYY TO k-bubl+11(6).
    ENDIF.
  ENDIF.

* Sort criteria: BUDAT/BELNR
  IF sort_key = '2'.
    MOVE space TO k-bubl.
    IF no_cpu IS INITIAL.
      WRITE bkpf-cpudt DDMMYY TO k-bubl(6).
      MOVE gd_belnr TO k-bubl+7(10).
    ELSE.
      MOVE gd_belnr TO k-bubl+0(10).
    ENDIF.
  ENDIF.

* Sort criteria: CPUDT/BELNR
  IF sort_key = '3' OR sort_key = '4'.
    MOVE space TO k-bubl.
    IF no_cpu IS INITIAL.
      WRITE bkpf-cpudt DDMMYY TO k-bubl(6).
      MOVE gd_belnr TO k-bubl+7(10).
    ELSE.
      MOVE gd_belnr TO k-bubl+0(10).
    ENDIF.
  ENDIF.

* Document text: BKTXT from T003T table
  MOVE sy-mandt   TO t003t-mandt.
  MOVE bkpf-blart TO t003t-blart.
  READ TABLE t003t.
  IF bkpf-bktxt NE space.
    MOVE bkpf-bktxt  TO k-bktxt.
  ELSE.
    MOVE t003t-ltext TO k-bktxt.
  ENDIF.
  IF poland = 'X'.                                          "n1053772
    MOVE bkpf-usnam  TO k-usnam.
  ENDIF.                                                    "n1053772

  MOVE bkpf-xblnr_alt TO k-xblnr_alt.

  WRITE: bkpf-bldat DDMMYY TO k-bldat,
*------------------ in wbudat there might be the special date (from
*------------------ parameter screen) or the actual posting date
         wbudat DDMMYY TO k-budat.
*        bkpf-budat ddmmyy to k-budat.
  MOVE:  bkpf-blart  TO k-blart,
         bkpf-xblnr  TO k-xblnr,
         wbudat      TO k-budat_long,
         bkpf-bldat  TO k-bldat_long.

  MOVE:  bkpf-belnr  TO k-belnr,
         bkpf-stblg  TO k-stblg,
         bkpf-stjah  TO k-stjah.
  MOVE:  gs_fagl_s_doc_details-docnr TO k-docnr.
  MOVE   bkpf-monat  TO k-monat.                            "note979647

  IF bkpf-budat > lastday.
    MOVE bkpf-budat TO lastday.
  ENDIF.

*---------------------------------------------------------------------*
*        GET BSEG                                                     *
*---------------------------------------------------------------------*
GET bseg.
  CHECK SELECT-OPTIONS.
  IF fagl_active = 'X'
  AND ( bkpf-bstat = ' ' OR bkpf-bstat = 'L' )
  AND NOT par_docc IS INITIAL.
    gd_buzei = bseg-docln.
  ELSE.
    gd_buzei = bseg-buzei.
  ENDIF.

* Prüfung der alternativen Kontonummer
  IF p_altkt = 'X'.

    CALL FUNCTION 'READ_SACHKONTO_ALTKT'
         EXPORTING
              bukrs           = bseg-bukrs
              saknr           = bseg-hkont
              spras           = sy-langu
              xmass           = 'X'
*             XSKAN           = 'X'
              xtext           = 'X'
         IMPORTING
              altkt           = h_konto
*             ALTKT_SAKAN     = H_KONTa
              altkt_not_found = h_altnf
              ktext           = h_ktext
         EXCEPTIONS
              bukrs_not_found = 02
              saknr_not_found = 03.

    h_kontm = h_konto.

    IF NOT h_altnf IS INITIAL.
      h_konto = bseg-hkont.
      h_kontm = bseg-hkont.
*      Schreiben Protokollsatz
      CLEAR fimsg.
      fimsg-msort = '0006'.
      fimsg-msgid = 'FR'.
      fimsg-msgty = 'I'.
      fimsg-msgno = '319'.
      fimsg-msgv1 = bseg-hkont.
      fimsg-msgv2 = bseg-bukrs.

      CALL FUNCTION 'FI_MESSAGE_COLLECT'
        EXPORTING
          i_fimsg = fimsg.
    ENDIF.

    IF sy-subrc = 2.
      h_konto = bseg-hkont.
      h_kontm = bseg-hkont.
*      Schreiben Protokollsatz
      CLEAR fimsg.
      fimsg-msort = '0001'.
      fimsg-msgid = 'FR'.
      fimsg-msgty = 'I'.
      fimsg-msgno = '321'.
      fimsg-msgv1 = bseg-bukrs.

      CALL FUNCTION 'FI_MESSAGE_COLLECT'
        EXPORTING
          i_fimsg = fimsg.
    ENDIF.

    IF sy-subrc = 3.
      h_konto = bseg-hkont.
      h_kontm = bseg-hkont.
*      Schreiben Protokollsatz
      CLEAR fimsg.
      fimsg-msort = '0005'.
      fimsg-msgid = 'FR'.
      fimsg-msgty = 'I'.
      fimsg-msgno = '322'.
      fimsg-msgv1 = bseg-hkont.
      fimsg-msgv2 = bseg-bukrs.

      CALL FUNCTION 'FI_MESSAGE_COLLECT'
        EXPORTING
          i_fimsg = fimsg.
    ENDIF.
  ELSE.
    h_konto = bseg-hkont.
    h_kontm = bseg-hkont.
  ENDIF.
* Fill credit/debit amounts table S
  IF bseg-koart = 'S'                    "begin n1226853
  OR bseg-koart = 'A'
  OR bseg-koart = 'M'.
    CALL FUNCTION 'READ_SKA1'
      EXPORTING
        xktopl         = t001-ktopl
        xsaknr         = h_kontm
      IMPORTING
        xska1          = ska1
        xskat          = skat
      EXCEPTIONS
        not_found      = 1
        key_incomplete = 2
        not_authorized = 3.
    IF ska1-ktoks IN s_ktoks.
      PERFORM ts_fill.
    ENDIF.
  ELSE.
    PERFORM ts_fill.
  ENDIF.                                  "end n1226853


* Fill document positions table T
  PERFORM tt_fill.

* Define data for one-time accounts
*  IF BSEG-KOART = 'D' OR BSEG-KOART = 'K'.
*    IF BSEG-XCPDD NE SPACE.
*      MOVE: BSEC-NAME1 TO CPD-NAME1,
*            BSEC-NAME2 TO CPD-NAME2,
*            BSEC-STRAS TO CPD-STRAS,
*            BSEC-PSTLZ TO CPD-PSTLZ,
*            BSEC-ORT01 TO CPD-ORT01,
*            BSEC-REGIO TO CPD-REGIO.
*      CPD-KZ = '1'.
*    ENDIF.
*  ENDIF.

*---------------------------------------------------------------------*
*        GET BKPF LATE                                              *
*---------------------------------------------------------------------*
GET bkpf LATE.

* Prepare field groups and extract data
  IF t_flag = '1'.
    LOOP AT t.
      bseg-buzei = t-buzei.
      bseg-docln = t-docln.
      IF fagl_active = 'X'
      AND ( bkpf-bstat = ' ' OR bkpf-bstat = 'L' )
      AND NOT par_docc IS INITIAL.
        gd_buzei = bseg-docln.
      ELSE.
        gd_buzei = bseg-buzei.
      ENDIF.
      PERFORM prep_dates.
      IF flag NE 'F'.
        EXTRACT dates.
      ENDIF.
    ENDLOOP.
  ENDIF.
* extract data for clearing documents
* (they have no bseg entry --> table t is not filled!)
  IF bkpf-bstat = 'A' OR bkpf-bstat = 'B'.
    CLEAR: p1, p2, p3, p4, t.
    CLEAR: whsol, whhab.
    EXTRACT dates.
  ENDIF.

*---------------------------------------------------------------------*
*        END OF SELECTION                                           *
*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM init_btchhdg.

  textflag = '1'.
* if not required only total values output of table S
  IF posten NE 'X'.
    IF s_flag = '1'.
      PERFORM output_stab.
    ENDIF.
  ENDIF.

* sort of extract data
  PERFORM sort_data.

* output of sorted data
  LOOP.

    AT NEW bukrs_bk.
      PERFORM output_newbukrs.
    ENDAT.

    AT NEW gd_belnr.
      PERFORM output_newbelnr.
*---- OPEN FI: append T_BKPF_ADDON_TAB -------------------------------*
      IF check_open_fi = 'X'.
        store_tab = ' '.
        PERFORM store_tab_open_fi TABLES t_bkpf_addon_tab
                                  USING  store_tab
                                         test
                                         budat_low
                                         budat_high.
      ENDIF.
    ENDAT.

    AT END OF wzaehl.
      PERFORM output_endwzaehl.
    ENDAT.

    AT END OF gd_belnr.
      PERFORM output_endbelnr.
    ENDAT.

    AT END OF wbudat.
      PERFORM output_endbudat.
    ENDAT.

    AT END OF bkpf-cpudt.
      PERFORM output_endbudat.
    ENDAT.

    AT END OF bukrs_bk.
      PERFORM output_endbukrs.
    ENDAT.

  ENDLOOP.

*--- OPEN FI: store T_BKPF_ADDON_TAB ---------------------------------*
  IF check_open_fi = 'X'.
    store_tab = 'X'.
    PERFORM store_tab_open_fi TABLES t_bkpf_addon_tab
                              USING  store_tab
                                     test
                                     budat_low
                                     budat_high.
  ENDIF.
*---------------------------------------------------------------------*
*       Fehlerprotokoll                                               *
*---------------------------------------------------------------------*
  CALL FUNCTION 'FI_MESSAGE_CHECK'
    EXCEPTIONS
      no_message = 4.
  IF sy-subrc = 0.
    tabsum = 'X'.
    textflag = 'X'.
    MOVE: listsep TO bhdgd-separ,
          p_miffl TO bhdgd-miffl,
          'BUKRS' TO bhdgd-domai,
          '0'     TO bhdgd-inifl,
          sy-langu TO t003t-spras,
          sy-linsz TO bhdgd-lines,
          sy-uname TO bhdgd-uname,
          sy-repid TO bhdgd-repid,
          sy-title TO bhdgd-line1,
          text-200 TO bhdgd-line2.
    PERFORM new-section(rsbtchh0).
    IF sy-batch EQ space.
      NEW-PAGE.
      FORMAT COLOR 6 INTENSIFIED.
      CALL FUNCTION 'FI_MESSAGE_SORT'.
      CALL FUNCTION 'FI_MESSAGE_PRINT'
        EXPORTING
          i_xskip = 'X'.
    ELSE.
      CALL FUNCTION 'FI_MESSAGE_GET'
        TABLES
          t_fimsg = t_fimsg.
      LOOP AT t_fimsg.
       MESSAGE ID t_fimsg-msgid TYPE t_fimsg-msgty NUMBER t_fimsg-msgno
           WITH t_fimsg-msgv1 t_fimsg-msgv2 t_fimsg-msgv3 t_fimsg-msgv4.
      ENDLOOP.
    ENDIF.
  ENDIF.
* only for Poland
  IF poland = 'X'.
    WRITE /39 text-008.
  ENDIF.
*---------------------------------------------------------------------*
*        TOP-OF-PAGE                                                  *
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM output_top.
*---------------------------------------------------------------------*
*        END-OF-PAGE                                                  *
*---------------------------------------------------------------------*
END-OF-PAGE.
  PERFORM output_eop.
*---------------------------------------------------------------------*
*        R O U T I N E S                                              *
*---------------------------------------------------------------------*
*         1. INIT_BTCHHDG                                             *
*         2. TS_FILL                                                  *
*         3. TT_FILL                                                  *
*         4. PREP_DATES                                               *
*         5. OUTPUT_NEWBUKRS                                          *
*         6. OUTPUT_NEWBELNR                                          *
*         7. OUTPUT_ENDWZAEHL                                         *
*         8. OUTPUT_ENDBELNR                                          *
*         9. OUTPUT_ENDBUDAT                                          *
*        10. OUTPUT_ENDCPUDT.                                         *
*        11. OUTPUT_ENDBUKRS                                          *
*        12. OUTPUT_TOP                                               *
*        13. OUTPUT_EOP                                               *
*        14. SORT_DATA                                                *
*        15. OUTPUT_STAB                                              *
*        16. OUTPUT_SBKTAB                                            *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*  1.   FORM INIT_BTCHHDG                                             *
*---------------------------------------------------------------------*
*       Initialization  for   BATCH-HEADING Routine                  *
*       called from :    1. S-O-S                                     *
*---------------------------------------------------------------------*
FORM init_btchhdg.

  DATA: start_date(8) TYPE c,
        end_date(8)   TYPE c,
        h_title(50)   TYPE c.
  DATA  ld_help1      TYPE char50.                          "note979647
  DATA  ld_help2      TYPE char50.                          "note979647

  IF p_date IS INITIAL.
    h_title = sy-title.
  ELSE.
    WRITE sy-title(25)  TO h_title.
    IF NOT br_budat-low IS INITIAL.
      WRITE br_budat-low  TO ld_help1.                      "note979647
    ELSE.
      WRITE firstday TO ld_help1.                           "note979647
    ENDIF.
*   only if special doc. types are used (for italy)
    IF NOT firstday_sdoct IS INITIAL.
      WRITE firstday_sdoct TO ld_help1.                     "note979647
    ENDIF.
    IF NOT br_budat-high IS INITIAL.
      WRITE br_budat-high TO ld_help2.                      "note979647
    ELSE.
      WRITE lastday TO ld_help2.                            "note979647
    ENDIF.
  ENDIF.

  CONCATENATE h_title ld_help1 '-' ld_help2                 "note979647
    INTO h_title SEPARATED BY space.                        "note979647

  CONDENSE h_title.

  MOVE: sy-linsz TO bhdgd-lines,
        sy-uname TO bhdgd-uname,
        sy-repid TO bhdgd-repid,
        h_title  TO bhdgd-line1,
        title    TO bhdgd-line2.
ENDFORM.                               "init_btchhdg
*---------------------------------------------------------------------*
*  2.   FORM TS_FILL                                                  *
*---------------------------------------------------------------------*
*       The table S is filled
*       at the event     1. GET BSEG
*---------------------------------------------------------------------*
FORM ts_fill.
* Table header initialization
  CLEAR s.
* Read table T001 to retry currency
  CLEAR t001.
  t001-mandt = sy-mandt.
  t001-bukrs = bukrs_bk.
  READ TABLE t001.
  IF alcur = 'X'.
    SELECT SINGLE * FROM teurb WHERE bukrs = t001-bukrs
                               AND   cprog = sy-repid
                               AND   land1 = t001-land1.
    IF sy-subrc = 0.
      t001-waers = teurb-waers.
    ELSE.
      SELECT SINGLE * FROM teurb WHERE bukrs = t001-bukrs
                                 AND   cprog = sy-repid.
      IF sy-subrc = 0.
        t001-waers = teurb-waers.
      ENDIF.
    ENDIF.
  ENDIF.
  IF posten(1) NE 'X'.
    IF bkpf-bstat = space
    OR bkpf-bstat = 'L'.
      s-jahr  = wjahr.
      s-waers = t001-waers.
      s-bukrs = bukrs_bk.
      s-mon   = wmon.
      s-ssol  = bsega-dmsol.
      s-shab  = bsega-dmhab.
      IF bseg-koart = 'D'.
*       IF BSEG-UMSKZ = SPACE.
        s-dsol = bsega-dmsol.
        s-dhab = bsega-dmhab.
*       ELSE.
*         S-DSOL = S-DHAB = 0.
*       ENDIF.
      ENDIF.
      IF bseg-koart = 'K'.
*       IF BSEG-UMSKZ = SPACE.
        s-ksol = bsega-dmsol.
        s-khab = bsega-dmhab.
*       ELSE.
*         S-KSOL = S-KHAB = 0.
*       ENDIF.
      ENDIF.
    ENDIF.
    COLLECT s.
    s_flag = '1'.
  ENDIF.
ENDFORM.                               "TS_FILL
*---------------------------------------------------------------------*
*  3.   FORM TT_FILL                                                  *
*---------------------------------------------------------------------*
*       The table T is filled
*       at the event     1. GET BSEG
*---------------------------------------------------------------------*
FORM tt_fill.
* Table header initialization
  CLEAR t.

* If no compression required: entry of document position
  IF compress(1) NE 'X'.
    WRITE: bseg-buzei TO t-buzei.
    WRITE: bseg-docln TO t-docln.
*    if segment text required
    IF segmtext(1) = 'X'.
      MOVE: bseg-sgtxt TO t-sgtxt.
    ENDIF.
*    if business volume area compression required
    IF gb_comp(1) NE 'X'.
      MOVE: bseg-gsber TO t-gsber.
    ENDIF.
  ENDIF.
* entry of correct master record
  IF bseg-koart = 'D'.
    MOVE: bseg-kunnr TO t-konto,
*         BSEG-HKONT TO T-HKONT.
          h_konto    TO t-hkont,
*         bseg-hkont to t-kontm.
          h_kontm    TO t-kontm.
  ELSE.
    IF bseg-koart = 'K'.
      MOVE: bseg-lifnr TO t-konto,
*           BSEG-HKONT TO T-HKONT.
            h_konto    TO t-hkont,
*           bseg-hkont to t-kontm.
            h_kontm    TO t-kontm.
    ELSE.
      MOVE: space      TO t-hkont,
*           BSEG-HKONT TO T-KONTO.
            h_konto    TO t-konto,
            h_ktext    TO t-txt20,
*           bseg-hkont to t-kontm.
            h_kontm    TO t-kontm.
    ENDIF.
  ENDIF.
  MOVE: bseg-koart  TO t-koart,
        bseg-bschl  TO t-bschl,
        bseg-xnegp  TO t-xnegp,
        bseg-umskz  TO t-umskz,
        bseg-mwskz  TO t-mwskz,
        bkpf-waers  TO t-waers,
        bsega-wrshb TO t-wrshb,
        bsega-dmsol TO t-dmsol,
        bsega-dmhab TO t-dmhab.
  IF t-xnegp = 'X'.
    t-xnegp = '-'.
  ENDIF.
* Define data for one-time accounts
  IF bseg-koart = 'D' OR bseg-koart = 'K'.
    IF bseg-xcpdd NE space.
      MOVE: bsec-name1 TO t-name1,
            bsec-name2 TO t-name2,
            bsec-stras TO t-stras,
            bsec-pstlz TO t-pstlz,
            bsec-ort01 TO t-ort01,
            bsec-regio TO t-regio.
      t-cpdkz = '1'.
    ENDIF.
  ENDIF.
  COLLECT t.
  t_flag = '1'.
ENDFORM.                               "TT_FILL
*---------------------------------------------------------------------*
*  4.   FORM PREP_DATES                                               *
*---------------------------------------------------------------------*
*       The field groups data are filled
*       at the event     1. GET BK LATE
*---------------------------------------------------------------------*
FORM prep_dates.
*---------------------------------------------------------------------*
  DATA: ld_subrc        TYPE sysubrc.                       "n1149977
*---------------------------------------------------------------------*
* clear: p1, p2, p3, p4.                               "<<DELETE: 68132
  CLEAR: p1, p2, p3, p4,flag.          "<<INSERT: 68132
  CLEAR: gd_name2, gd_pstlz, gd_ort01, gd_stras.

  IF t-koart = 'S'                          "begin n1149977
  OR t-koart = 'A'
  OR t-koart = 'M'.
    CALL FUNCTION 'READ_SKA1'
      EXPORTING
        xktopl         = t001-ktopl
        xsaknr         = t-kontm
      IMPORTING
        xska1          = ska1
        xskat          = skat
      EXCEPTIONS
        not_found      = 1
        key_incomplete = 2
        not_authorized = 3.
    ld_subrc = sy-subrc.
    IF NOT ska1-ktoks IN s_ktoks.
      flag = 'F'.
      EXIT.
    ENDIF.
  ENDIF.                                    "end n1149977

  UNPACK pzaehl TO wzaehl.
  pzaehl = pzaehl + 1.
  MOVE:  t-dmsol TO whsol,
         t-dmhab TO whhab.
*        T-KONTO TO WKKONTO,
*        T-KONTO TO WSKONTO,
  IF fagl_active = 'X'
  AND NOT par_docc IS INITIAL.
    MOVE:  t-docln TO p1-buzei.
  ELSE.
    MOVE:  t-buzei TO p1-buzei.
  ENDIF.
  MOVE:  t-koart TO p1-koart.
  IF p_accseq EQ 'X' AND  ( t-koart EQ 'D' OR t-koart EQ 'K' ).
    MOVE:  t-konto TO p1-hkont,        " triggers output of
           t-hkont TO p1-konto.        " before
  ELSE.
    MOVE:  t-konto TO p1-konto,
           t-hkont TO p1-hkont.
  ENDIF.
  MOVE:  t-gsber TO p1-gsber,
         t-bschl TO p1-bschl,
         t-umskz TO p1-umskz.
  IF t-umskz IS INITIAL.
    MOVE t-xnegp TO p1-umskz.
  ELSE.
    MOVE t-xnegp TO p1-xnegp.
  ENDIF.
  MOVE:  t-mwskz TO p1-mwskz,
         t-sgtxt TO p2-sgtxt.
  bseg-sgtxt = t-sgtxt.

  IF textline > 0.
    IF t-koart = 'D' OR t-koart = 'K'.
      IF t-cpdkz = '1'.
        gd_name2 = t-name2.
        gd_pstlz = t-pstlz.
        gd_ort01 = t-ort01.
        gd_stras = t-stras.
        IF textline = 1.
          MOVE: t-name1 TO p1-text(10),
                t-ort01 TO p1-text+11(14).
*         CHAR_LEN = CHARLEN( T-NAME1+9(2) ).                     "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*             P1-TEXT+9(1) = SPACE.                               "KANJI
*         ENDIF.                                                  "KANJI
*         CHAR_LEN = CHARLEN( T-ORT01+13(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P1-TEXT+24(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
        ENDIF.
        IF textline = 2.
          MOVE: t-name1 TO p1-text,
                t-pstlz TO p2-text(10),
                t-ort01 TO p2-text+11(24).
*         CHAR_LEN = CHARLEN( T-NAME1+24(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P1-TEXT+24(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
*         CHAR_LEN = CHARLEN( T-ORT01+23(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P2-TEXT+34(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
          CONDENSE p2.
        ENDIF.
        IF textline = 3.
          MOVE: t-name1 TO p1-text,
                t-stras TO p2-text,
                t-pstlz TO p3-text(10),
                t-ort01 TO p3-text+11(24).
*         CHAR_LEN = CHARLEN( T-NAME1+24(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P1-TEXT+24(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
*         CHAR_LEN = CHARLEN( T-ORT01+23(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P3-TEXT+34(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
          CONDENSE p2.
          CONDENSE p3.
        ENDIF.
        IF textline = 4.
          MOVE: t-name1 TO p1-text,
                t-name2 TO p2-text,
                t-stras TO p3-text,
                t-pstlz TO p4-text(10),
                t-ort01 TO p4-text+11(24).
*         CHAR_LEN = CHARLEN( T-NAME1+24(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P1-TEXT+24(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
*         CHAR_LEN = CHARLEN( T-ORT01+23(2) ).                    "KANJI
*         IF CHAR_LEN  = 1.                                       "KANJI
*           P4-TEXT+34(1) = SPACE.                                "KANJI
*         ENDIF.                                                  "KANJI
          CONDENSE p2.
          CONDENSE p3.
          CONDENSE p4.
        ENDIF.
      ELSE.
        IF t-koart = 'D'.
          CALL FUNCTION 'READ_KNA1'
            EXPORTING
              xkunnr         = t-konto
            IMPORTING
              xkna1          = kna1
            EXCEPTIONS              "<<INSERT:68132
              not_found      = 1 "<<INSERT:68132
              key_incomplete = 2 "<<INSERT:68132
              not_authorized = 3."<<INSERT:68132
          CASE sy-subrc.               "<<INSERT:68132
            WHEN 1.                    "<<INSERT:68132
              CLEAR fimsg.             "<<INSERT:68132
              fimsg-msort = '0001'.    "<<INSERT:68132
              fimsg-msgid = 'FR'.      "<<INSERT:68132
              fimsg-msgty = 'I'.       "<<INSERT:68132
              fimsg-msgno = '307'.     "<<INSERT:68132
              fimsg-msgv1 = t-konto.   "<<INSERT:68132
              fimsg-msgv2 = bkpf-belnr."<<INSERT:68132
              CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                   EXPORTING           "<<INSERT:68132
                        i_fimsg = fimsg.              "<<INSERT:68132
              flag = 'F'.              "<<INSERT:68132
            WHEN 2.                    "<<INSERT:68132
              CLEAR fimsg.             "<<INSERT:68132
              fimsg-msort = '0001'.    "<<INSERT:68132
              fimsg-msgid = 'FR'.      "<<INSERT:68132
              fimsg-msgty = 'I'.       "<<INSERT:68132
              fimsg-msgno = '441'.     "<<INSERT:68132
              fimsg-msgv1 = 'KEY_INCOMPLETE'.         "<<INSERT:68132
              fimsg-msgv2 = 'READ_KNA1'.              "<<INSERT:68132
              CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                   EXPORTING           "<<INSERT:68132
                        i_fimsg = fimsg.              "<<INSERT:68132
              flag = 'F'.              "<<INSERT:68132
            WHEN 3.                    "<<INSERT:68132
              CLEAR fimsg.             "<<INSERT:68132
              fimsg-msort = '0001'.    "<<INSERT:68132
              fimsg-msgid = 'FR'.      "<<INSERT:68132
              fimsg-msgty = 'I'.       "<<INSERT:68132
              fimsg-msgno = '399'.     "<<INSERT:68132
              fimsg-msgv1 = t-konto.   "<<INSERT:68132
              fimsg-msgv2 = bkpf-belnr."<<INSERT:68132
              CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                   EXPORTING           "<<INSERT:68132
                        i_fimsg = fimsg.              "<<INSERT:68132
              flag = 'F'.              "<<INSERT:68132
          ENDCASE.

          gd_name2 = kna1-name2.
          gd_pstlz = kna1-pstlz.
          gd_ort01 = kna1-ort01.
          gd_stras = kna1-stras.
*         if textline = 1.                            "<<DELETE:68132
          IF textline = 1 AND flag NE 'F'.            "<<INSERT:68132
            MOVE: kna1-sortl TO p1-text,
                  kna1-ort01 TO p1-text+11(14).
*           CHAR_LEN = CHARLEN( KNA1-ORT01+13(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
          ENDIF.
*         if textline = 2.                            "<<DELETE:68132
          IF textline = 2 AND flag NE 'F'.            "<<INSERT:68132
            MOVE: kna1-name1 TO p1-text,
                  kna1-pstlz TO p2-text(10),
                  kna1-ort01 TO p2-text+11(24).
*           CHAR_LEN = CHARLEN( KNA1-NAME1+24(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
*           CHAR_LEN = CHARLEN( KNA1-ORT01+23(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P2-TEXT+34(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
            CONDENSE p2.
          ENDIF.
*         if textline = 3.                            "<<DELETE:68132
          IF textline = 3 AND flag NE 'F'.            "<<INSERT:68132
            MOVE: kna1-name1 TO p1-text,
                  kna1-stras TO p2-text,
                  kna1-pstlz TO p3-text(10),
                  kna1-ort01 TO p3-text+11(24).
*           CHAR_LEN = CHARLEN( KNA1-NAME1+24(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
*           CHAR_LEN = CHARLEN( KNA1-ORT01+23(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P3-TEXT+34(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
            CONDENSE p2.
            CONDENSE p3.
          ENDIF.
*         if textline = 4.                            "<<DELETE:68132
          IF textline = 4 AND flag NE 'F'.            "<<INSERT:68132
            MOVE: kna1-name1 TO p1-text,
                  kna1-name2 TO p2-text,
                  kna1-stras TO p3-text,
                  kna1-pstlz TO p4-text(10),
                  kna1-ort01 TO p4-text+11(24).
*           CHAR_LEN = CHARLEN( KNA1-NAME1+24(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
*           CHAR_LEN = CHARLEN( KNA1-ORT01+23(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P4-TEXT+34(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
            CONDENSE p2.
            CONDENSE p3.
            CONDENSE p4.
          ENDIF.
        ENDIF.
        IF t-koart = 'K'.
          CALL FUNCTION 'READ_LFA1'
            EXPORTING
              xlifnr         = t-konto
            IMPORTING
              xlfa1          = lfa1
            EXCEPTIONS
              not_found      = 1
              key_incomplete = 2
              not_authorized = 3.

          CASE sy-subrc.
            WHEN 1.
*                   Schreiben Protokollsatz
              CLEAR fimsg.
*              READ TABLE TRVOR.
              fimsg-msort = '0001'.
              fimsg-msgid = 'FR'.
              fimsg-msgty = 'I'.
              fimsg-msgno = '307'.
              fimsg-msgv1 = t-konto.
              fimsg-msgv2 = bkpf-belnr.
              CALL FUNCTION 'FI_MESSAGE_COLLECT'
                EXPORTING
                  i_fimsg = fimsg.
              flag = 'F'.
            WHEN 2.                    "<<INSERT:68132
              CLEAR fimsg.             "<<INSERT:68132
              fimsg-msort = '0001'.    "<<INSERT:68132
              fimsg-msgid = 'FR'.      "<<INSERT:68132
              fimsg-msgty = 'I'.       "<<INSERT:68132
              fimsg-msgno = '441'.     "<<INSERT:68132
              fimsg-msgv1 = 'KEY_INCOMPLETE'.         "<<INSERT:68132
              fimsg-msgv2 = 'READ_LFA1'.              "<<INSERT:68132
              CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                   EXPORTING           "<<INSERT:68132
                        i_fimsg = fimsg.              "<<INSERT:68132
              flag = 'F'.              "<<INSERT:68132
            WHEN 3.                    "<<INSERT:68132
              CLEAR fimsg.             "<<INSERT:68132
              fimsg-msort = '0001'.    "<<INSERT:68132
              fimsg-msgid = 'FR'.      "<<INSERT:68132
              fimsg-msgty = 'I'.       "<<INSERT:68132
              fimsg-msgno = '399'.     "<<INSERT:68132
              fimsg-msgv1 = t-konto.   "<<INSERT:68132
              fimsg-msgv2 = bkpf-belnr."<<INSERT:68132
              CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                   EXPORTING           "<<INSERT:68132
                        i_fimsg = fimsg.              "<<INSERT:68132
              flag = 'F'.              "<<INSERT:68132
          ENDCASE.

          gd_name2 = lfa1-name2.
          gd_pstlz = lfa1-pstlz.
          gd_ort01 = lfa1-ort01.
          gd_stras = lfa1-stras.
          IF textline = 1 AND flag NE 'F'.
            MOVE: lfa1-sortl TO p1-text,
                  lfa1-ort01 TO p1-text+11(14).
*           CHAR_LEN = CHARLEN( LFA1-ORT01+13(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
          ENDIF.
          IF textline = 2 AND flag NE 'F'.
            MOVE: lfa1-name1 TO p1-text,
                  lfa1-pstlz TO p2-text(10),
                  lfa1-ort01 TO p2-text+11(24).
*           CHAR_LEN = CHARLEN( LFA1-NAME1+24(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
*           CHAR_LEN = CHARLEN( LFA1-ORT01+23(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P2-TEXT+34(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
            CONDENSE p2.
          ENDIF.
          IF textline = 3 AND flag NE 'F'.
            MOVE: lfa1-name1 TO p1-text,
                  lfa1-stras TO p2-text,
                  lfa1-pstlz TO p3-text(10),
                  lfa1-ort01 TO p3-text+11(24).
*           CHAR_LEN = CHARLEN( LFA1-NAME1+24(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
*           CHAR_LEN = CHARLEN( LFA1-ORT01+23(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P3-TEXT+34(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
            CONDENSE p2.
            CONDENSE p3.
          ENDIF.
          IF textline = 4 AND flag NE 'F'.
            MOVE: lfa1-name1 TO p1-text,
                  lfa1-name2 TO p2-text,
                  lfa1-stras TO p3-text,
                  lfa1-pstlz TO p4-text(10),
                  lfa1-ort01 TO p4-text+11(24).
*           CHAR_LEN = CHARLEN( LFA1-NAME1+24(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P1-TEXT+24(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
*           CHAR_LEN = CHARLEN( LFA1-ORT01+23(2) ).               "KANJI
*           IF CHAR_LEN  = 1.                                     "KANJI
*             P4-TEXT+34(1) = SPACE.                              "KANJI
*           ENDIF.                                                "KANJI
            CONDENSE p2.
            CONDENSE p3.
            CONDENSE p4.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF ( t-koart = 'S' OR t-koart = 'A' OR t-koart = 'M' )
        AND t-txt20 IS INITIAL.
*        CALL FUNCTION 'READ_SKA1'          "begin n1149977
*             EXPORTING
*                  xktopl = t001-ktopl
*                  xsaknr = t-kontm
*             IMPORTING
*                  xska1  = ska1
**                 xskat  = skat.                      ">>DELETE:68132
*                  xskat  = skat        "<<INSERT:68132
*             EXCEPTIONS                "<<INSERT:68132
*                  not_found      = 1   "<<INSERT:68132
*                  key_incomplete = 2   "<<INSERT:68132
*                  not_authorized = 3.  "<<INSERT:68132
*        CASE sy-subrc.                 "<<INSERT:68132
        CASE ld_subrc.                      "end n1149977
          WHEN 0.                      "<<INSERT:68132
            MOVE: skat-txt20 TO p1-text.            "<<INSERT:68132
          WHEN 1.                      "<<INSERT:68132
            CLEAR fimsg.               "<<INSERT:68132
            fimsg-msort = '0001'.      "<<INSERT:68132
            fimsg-msgid = 'FR'.        "<<INSERT:68132
            fimsg-msgty = 'I'.         "<<INSERT:68132
            fimsg-msgno = '316'.       "<<INSERT:68132
            fimsg-msgv1 = t-konto.     "<<INSERT:68132
            fimsg-msgv2 = t001-ktopl.  "<<INSERT:68132
            CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                 EXPORTING             "<<INSERT:68132
                      i_fimsg = fimsg. "<<INSERT:68132
          WHEN 2.                      "<<INSERT:68132
            CLEAR fimsg.               "<<INSERT:68132
            fimsg-msort = '0001'.      "<<INSERT:68132
            fimsg-msgid = 'FR'.        "<<INSERT:68132
            fimsg-msgty = 'I'.         "<<INSERT:68132
            fimsg-msgno = '441'.       "<<INSERT:68132
            fimsg-msgv1 = 'KEY_INCOMPLETE'.         "<<INSERT:68132
            fimsg-msgv2 = 'READ_SKA1'. "<<INSERT:68132
            CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                 EXPORTING             "<<INSERT:68132
                      i_fimsg = fimsg. "<<INSERT:68132
          WHEN 3.                      "<<INSERT:68132
            CLEAR fimsg.               "<<INSERT:68132
            fimsg-msort = '0001'.      "<<INSERT:68132
            fimsg-msgid = 'FR'.        "<<INSERT:68132
            fimsg-msgty = 'I'.         "<<INSERT:68132
            fimsg-msgno = '812'.       "<<INSERT:68132
            fimsg-msgv1 = t-konto.     "<<INSERT:68132
            fimsg-msgv2 = t001-ktopl.  "<<INSERT:68132
            CALL FUNCTION 'FI_MESSAGE_COLLECT'      "<<INSERT:68132
                 EXPORTING             "<<INSERT:68132
                      i_fimsg = fimsg. "<<INSERT:68132
        ENDCASE.                       "<<INSERT:68132
*    move: skat-txt20 to p1-text.                     "<<DELETE:68132

**   IF TEXTLINE = 1.                                             "KANJI
**     MOVE: SKAT-TXT50 TO P1-TEXT.                               "KANJI
**   ENDIF.                                                       "KANJI
**   IF TEXTLINE > 1.                                             "KANJI
**     MOVE: SKAT-TXT50 TO P1-TEXT,                               "KANJI
**           SKAT-TXT50+25(25) TO P2-TEXT.                        "KANJI
**   ENDIF.                                                       "KANJI
*    CASE TEXTLINE.                                               "KANJI
*      WHEN 1.                                                    "KANJI
*          MOVE: SKAT-TXT50 TO P1-TEXT.                           "KANJI
*          CHAR_LEN = CHARLEN( SKAT-TXT50+24(2) ).                "KANJI
*          IF CHAR_LEN  = 1.                                      "KANJI
*            P1-TEXT+24(1) = SPACE.                               "KANJI
*          ENDIF.                                                 "KANJI
*      WHEN OTHERS.                                               "KANJI
*          CHAR_LEN = CHARLEN( SKAT-TXT50+24(2) ).                "KANJI
*          IF CHAR_LEN EQ 1.                                      "KANJI
*            MOVE: SKAT-TXT50(24) TO P1-TEXT,                     "KANJI
*                  SKAT-TXT50+24 TO P2-TEXT.                      "KANJI
*          ELSE.                                                  "KANJI
*            MOVE: SKAT-TXT50(25) TO P1-TEXT,                     "KANJI
*                  SKAT-TXT50+25 TO P2-TEXT.                      "KANJI
*          ENDIF.                                                 "KANJI
*    ENDCASE.                                                     "KANJI
      ELSE.
        MOVE: t-txt20 TO p1-text.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "PREP_DATES
*---------------------------------------------------------------------*
*  5.   FORM OUTPUT_NEWBUKRS                                          *
*---------------------------------------------------------------------*
*       Statements executed in the loop after the sort of extract data
*       at the event     1. AT NEW BUKRS-BK
*---------------------------------------------------------------------*
FORM output_newbukrs.

  MOVE bukrs_bk     TO bhdgd-bukrs.
  MOVE bhdgd-bukrs  TO bhdgd-werte.
  PERFORM new-section(rsbtchh0).
  textflag = '2'.
  CLEAR t001.
  t001-mandt = sy-mandt.
  t001-bukrs = bukrs_bk.
  READ TABLE t001.
  IF alcur = 'X'.
    bukrs_waers = t001-waers.
    SELECT SINGLE * FROM teurb WHERE bukrs = t001-bukrs
                               AND   cprog = sy-repid
                               AND   land1 = t001-land1.
    IF sy-subrc = 0.
      t001-waers = teurb-waers.
    ELSE.
      SELECT SINGLE * FROM teurb WHERE bukrs = t001-bukrs
                                 AND   cprog = sy-repid.
      IF sy-subrc = 0.
        t001-waers = teurb-waers.
      ENDIF.
    ENDIF.
  ENDIF.
  z_flag = '0'.
  CLEAR: wasol, wahab,
         highdat, znummu.
  IF fagl_active = 'X' AND test NE 'X'.
    CLEAR fagl_trvor.
    SELECT SINGLE * FROM fagl_trvor WHERE rldnr = gd_rldnr
                                    AND   bukrs = bukrs_bk
                                    AND   repid = sy-repid
                                    AND   gjahr = wjahr.
    IF sy-subrc = 0.
      MOVE: fagl_trvor-sumso TO sumso,
            fagl_trvor-sumha TO sumha,
            fagl_trvor-sumso TO trvor-sumso,
            fagl_trvor-sumha TO trvor-sumha.
    ENDIF.
  ELSE.
    CLEAR trvor.
    MOVE: bukrs_bk TO trvor-bukrs,
          sy-repid TO trvor-repid,
          wjahr    TO trvor-gjahr.
    READ TABLE trvor.
  ENDIF.
  IF sy-subrc = 0.
    IF alcur = 'X'.
      IF teurb IS INITIAL.
        SELECT SINGLE * FROM teurb WHERE bukrs = t001-bukrs
                                   AND   cprog = sy-repid
                                   AND   land1 = t001-land1.
        IF sy-subrc <> 0.
          SELECT SINGLE * FROM teurb WHERE bukrs = t001-bukrs
                                     AND   cprog = sy-repid.
        ENDIF.
      ENDIF.
      IF excdt IS INITIAL.
        MOVE sy-datum TO excdt.
      ENDIF.
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          date             = excdt
          foreign_currency = t001-waers
          local_amount     = trvor-sumso
          local_currency   = bukrs_waers
          type_of_rate     = teurb-kurst
        IMPORTING
          foreign_amount   = trvor-sumso.
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          date             = excdt
          foreign_currency = t001-waers
          local_amount     = trvor-sumha
          local_currency   = bukrs_waers
          type_of_rate     = teurb-kurst
        IMPORTING
          foreign_amount   = trvor-sumha.
    ENDIF.

    MOVE: trvor-sumso TO sumso,
          trvor-sumha TO sumha.
    WRITE:
       /30  text-006.
    IF sumso GT max_length OR sumha GT max_length.
      WRITE:
         54(23) sumha NO-ZERO CURRENCY t001-waers,
        /37(23) sumso NO-ZERO CURRENCY t001-waers.
    ELSE.
      WRITE:
         44(16) sumso NO-ZERO CURRENCY t001-waers,
         62(16) sumha NO-ZERO CURRENCY t001-waers.
    ENDIF.
    ULINE.
    IF seit_sum = 'X'.
      wasol = sumso.
      wahab = sumha.
    ENDIF.
  ENDIF.
  wssol = wshab = 0.
ENDFORM.                    "OUTPUT_NEWBUKRS
*---------------------------------------------------------------------*
*  6.   FORM OUTPUT_NEWBELNR                                          *
*---------------------------------------------------------------------*
*       Statements executed in the loop after the sort of extract data
*       at the event     1. AT NEW BKPF_BELNR
*---------------------------------------------------------------------*
FORM output_newbelnr.
  IF tabsum NE 'X'.
* description of detail line and, if required, progressive number
    ph_flag = '1'.
*----------------------------------- fill details for microfiche line
    mikfi-bukrs = bukrs_bk.
    mikfi-gjahr = wjahr.
    mikfi-budat = wbudat.
    mikfi-belnr = gd_belnr.
    bhdgd-grpin = mikfi.

    IF z_nummer = 'X'.
      IF z_flag = '0'.
        IF br_budat-low  = firstday.
          MOVE '0' TO vzaehl.
        ELSE.
          IF fagl_active = 'X'.
            MOVE fagl_trvor-vzahl TO vzaehl.
          ELSE.
            MOVE trvor-vzahl TO vzaehl.
          ENDIF.
        ENDIF.
        z_flag = '1'.
      ENDIF.
      vzaehl = vzaehl + 1.
      UNPACK vzaehl   TO znummu.
      WRITE:
         /1 znummu.
      PERFORM write_k.
    ELSE.
      PERFORM write_k.
    ENDIF.
  ENDIF.
ENDFORM.                    "OUTPUT_NEWBELNR
*---------------------------------------------------------------------*
*  7.   FORM OUTPUT_ENDWZAEHL                                         *
*---------------------------------------------------------------------*
*       Statements executed in the loop after the sort of extract data
*       at the event     1. AT END OF WZAEHL
*---------------------------------------------------------------------*

FORM output_endwzaehl.
* if not required only total values
  IF tabsum NE 'X'.
    ph_flag = '0'.
*   not for clearing documents --> only header
    IF NOT ( bkpf-bstat = 'A' OR bkpf-bstat = 'B' ).
      PERFORM write_p1.
      IF italy IS INITIAL.                                  "note955881
        WRITE:
*           (16) t-wrshb NO-ZERO CURRENCY t-waers,
*                t-waers,
         44(16) t-dmsol NO-ZERO CURRENCY t001-waers,
         62(16) t-dmhab NO-ZERO CURRENCY t001-waers.
      ELSE.                                                 "note955881
        WRITE:                                              "note955881
*             (16) t-wrshb CURRENCY t-waers,                 "note955881
*                  t-waers,                                  "note955881
           44(16) t-dmsol CURRENCY t001-waers,              "note955881
           62(16) t-dmhab CURRENCY t001-waers.              "note955881
      ENDIF.                                                "note955881
      wssol = wssol + whsol.
      wshab = wshab + whhab.
      wasol = wasol + whsol.
      wahab = wahab + whhab.
      IF budatsum = 'X'.               " totals only needed in this case
        wrsum-fwbtr  = t-wrshb.
        wrsum-waers  = t-waers.
        wrsum-hwsol  = t-dmsol.
        wrsum-hwhab  = t-dmhab.
        COLLECT wrsum.                 " grouping by currency
      ENDIF.

      WRITE:
        /1     p2,
        /1     p3,
        /1     p4.
    ENDIF.
  ENDIF.
* memorize the highest posting or cpu date
  IF  sort_key = '1'  OR sort_key = '2'.
    IF bkpf-budat GT highdat.
      highdat = bkpf-budat.
    ENDIF.
  ENDIF.
  IF sort_key = '3' OR sort_key = '4'.
    IF bkpf-cpudt GT highdat.
      highdat = bkpf-cpudt.
    ENDIF.
  ENDIF.
ENDFORM.                    "OUTPUT_ENDWZAEHL
*---------------------------------------------------------------------*
*  8.   FORM OUTPUT_ENDBELNR                                          *
*---------------------------------------------------------------------*
*       Statements executed in the loop after the sort of extract data
*       at the event     1. AT END OF BKPF_BELNR
*---------------------------------------------------------------------*
FORM output_endbelnr.

  CLEAR bhdgd-grpin.

  IF tabsum NE 'X'.
    ph_flag = '1'.
    ULINE.
  ENDIF.

* clear TemSe (spool)                                  "CSS 142437/99
  IF spool = 'X'.                                      "CSS 142437/99
    count_doc = count_doc + 1.                         "CSS 142437/99
    IF count_doc GT max_doc.                           "CSS 142437/99
      CALL FUNCTION 'DB_COMMIT'.                       "CSS 142437/99
      CLEAR count_doc.                                 "CSS 142437/99
    ENDIF.                                             "CSS 142437/99
  ENDIF.                                               "CSS 142437/99

ENDFORM.                    "OUTPUT_ENDBELNR
*---------------------------------------------------------------------*
*  9.   FORM OUTPUT_ENDBUDAT                                          *
*---------------------------------------------------------------------*
*       Statements executed in the loop after the sort of extract data
*       at the event     1. AT END OF WBUDAT
*---------------------------------------------------------------------*
FORM output_endbudat.
* if sort for posting date
  IF sort_key = '2'.
*    if required detail
    IF tabsum NE 'X'.
*       if required total at end of posting date
      IF budatsum = 'X'.
        DESCRIBE TABLE wrsum LINES anzahl.
*       if there were foreign currency items print the corresponding
*       totals and delete them afterwards
        IF anzahl > 0.
          LOOP AT wrsum.
            IF wrsum-fwbtr NE 0.
              WRITE:
*              /(16) wrsum-fwbtr NO-ZERO CURRENCY wrsum-waers
*                               UNDER t-wrshb,
*                   wrsum-waers,
            44(16) wrsum-hwsol NO-ZERO CURRENCY t001-waers,
            62(16) wrsum-hwhab NO-ZERO CURRENCY t001-waers.
            ENDIF.
          ENDLOOP.
        ENDIF.
        CLEAR wrsum.
        REFRESH wrsum.

        WRITE: / text-003,
                 wbudat DD/MM/YY,
          44(16) sum(t-dmsol) NO-ZERO CURRENCY t001-waers,
          62(16) sum(t-dmhab) NO-ZERO CURRENCY t001-waers.
        ULINE.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "OUTPUT_ENDBUDAT
*---------------------------------------------------------------------*
* 11.   FORM OUTPUT_ENDBUKRS                                          *
*---------------------------------------------------------------------*
*       Statements executed in the loop after the sort of extract data
*       at the event     1. AT END OF BUKRS_BK
*---------------------------------------------------------------------*
FORM output_endbukrs.
* if required print page totals
  IF seit_sum = 'X'.
    SKIP TO LINE 98.
    IF wssol NE 0 OR wshab NE 0.
      WRITE: /25 text-002.
      IF wssol GT max_l_page OR wshab GT max_l_page.
        WRITE:
           42(17) wssol CURRENCY t001-waers NO-ZERO NO-SIGN,
           62(17) wshab CURRENCY t001-waers NO-ZERO.
      ELSE.
        WRITE:
           44(16) wssol CURRENCY t001-waers NO-ZERO,
           62(16) wshab CURRENCY t001-waers NO-ZERO.
      ENDIF.
    ENDIF.
    IF wasol NE 0 OR wahab NE 0.
      WRITE: /25 text-005.
      IF wasol GT max_length OR wahab GT max_length.
        WRITE:
           54(23) wahab CURRENCY t001-waers NO-ZERO,
          /37(23) wasol CURRENCY t001-waers NO-ZERO.
      ELSE.
        WRITE:
           44(16) wasol CURRENCY t001-waers NO-ZERO,
           62(16) wahab CURRENCY t001-waers NO-ZERO.
      ENDIF.
    ENDIF.
  ENDIF.
* if not test run and no altern. local curr. is used update table TRVOR
  IF test NE 'X' AND alcur NE 'X'.
    IF fagl_active = 'X'.
      CLEAR fagl_trvor.
      SELECT SINGLE * FROM fagl_trvor WHERE rldnr = gd_rldnr
                                      AND   bukrs = bukrs_bk
                                      AND   repid = sy-repid
                                      AND   gjahr = wjahr.
      IF sy-subrc = 0.
        MOVE: highdat  TO fagl_trvor-budat,
              znummu   TO fagl_trvor-vzahl,
              sy-datum TO fagl_trvor-cpudt,
              wasol    TO fagl_trvor-sumso,
              wahab    TO fagl_trvor-sumha.
        IF italy = 'X'.
          IF posten IS INITIAL.
*           last page still has to be printed for totals sheets
            fagl_trvor-pagno = pagno + 1.
          ELSE.
            fagl_trvor-pagno = pagno.
          ENDIF.
        ENDIF.
        MODIFY fagl_trvor.
      ELSE.
        DESCRIBE TABLE s_sblart LINES entries_dat.
        IF entries_dat GE 1.
          READ TABLE br_budat INDEX 1.
        ENDIF.
        IF br_budat-low  EQ firstday.
          MOVE: sy-repid TO fagl_trvor-repid,
                gd_rldnr TO fagl_trvor-rldnr,
                bukrs_bk TO fagl_trvor-bukrs,
                wjahr    TO fagl_trvor-gjahr,
                highdat  TO fagl_trvor-budat,
                znummu   TO fagl_trvor-vzahl,
                sy-datum TO fagl_trvor-cpudt,
                wasol    TO fagl_trvor-sumso,
                wahab    TO fagl_trvor-sumha.
          IF italy = 'X'.
            IF posten IS INITIAL.
*             last page still has to be printed for totals sheets
              fagl_trvor-pagno = pagno + 1.
            ELSE.
              fagl_trvor-pagno = pagno.
            ENDIF.
          ENDIF.
          MODIFY fagl_trvor.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR trvor.
      MOVE: bukrs_bk TO trvor-bukrs,
            sy-repid TO trvor-repid,
            wjahr    TO trvor-gjahr.
      READ TABLE trvor.
      IF sy-subrc = 0.
        MOVE: sy-repid TO trvor-repid,
              bukrs_bk TO trvor-bukrs,
              wjahr    TO trvor-gjahr,
              highdat  TO trvor-budat,
              znummu   TO trvor-vzahl,
              sy-datum TO trvor-cpudt.
        MOVE:
              wasol    TO trvor-sumso,
              wahab    TO trvor-sumha.
        IF italy = 'X'.
          IF posten IS INITIAL.
*           last page still has to be printed for totals sheets
            trvor-pagno = pagno + 1.
          ELSE.
            trvor-pagno = pagno.
          ENDIF.
        ENDIF.
        MODIFY trvor.
      ELSE.
        DESCRIBE TABLE s_sblart LINES entries_dat.
        IF entries_dat GE 1.
          READ TABLE br_budat INDEX 1.
        ENDIF.
        IF br_budat-low  EQ firstday.
          MOVE: sy-repid TO trvor-repid,
                bukrs_bk TO trvor-bukrs,
                wjahr    TO trvor-gjahr,
                highdat  TO trvor-budat,
                znummu   TO trvor-vzahl,
                sy-datum TO trvor-cpudt.
          MOVE:
                wasol    TO trvor-sumso,
                wahab    TO trvor-sumha.
          IF italy = 'X'.
            IF posten IS INITIAL.
*             last page still has to be printed for totals sheets
              trvor-pagno = pagno + 1.
            ELSE.
              trvor-pagno = pagno.
            ENDIF.
          ENDIF.
          MODIFY trvor.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  textflag = '1'.
  NEW-PAGE.
  wasol = wahab = 0.

* if not required only item lines output of table SBK
  IF posten NE 'X'.
    PERFORM output_stab.
  ENDIF.

ENDFORM.                    "OUTPUT_ENDBUKRS

*---------------------------------------------------------------------*
* 12.   FORM OUTPUT_TOP                                               *
*---------------------------------------------------------------------*
*       Statements executed
*       at the event     1. TOP_OF_PAGE
*---------------------------------------------------------------------*
FORM output_top.
*---------------------------------------------------------------------*
  DATA lt_dfies             TYPE STANDARD TABLE OF dfies.   "note990329
  DATA ls_dfies             TYPE dfies.                     "note990329
  DATA ld_strlen            TYPE i.                         "note990329
  DATA ld_no_standard_header.                               "n1053772
  DATA ld_no_page_number_italy.                             "n1053772
*---------------------------------------------------------------------*
INCLUDE ZFIY0006_HEADER.
*  INCLUDE rfbelj10_nacc_header.                             "n1053772
  IF ld_no_standard_header IS INITIAL.                      "n1053772
  IF noheader IS INITIAL.              "INSERT HMY030297
    PERFORM batch-heading(rsbtchh0).
  ELSE.                                "INSERT HMY030297
    WRITE bhdgd-line2 CENTERED.        "INSERT HMY030297
  ENDIF.                               "INSERT HMY030297
  ENDIF.                                                    "n1053772

* only for Italy
  IF  italy = 'X'
  AND ld_no_page_number_italy IS INITIAL.                   "n1053772
    PERFORM page_number_italy.
  ENDIF.

  IF p_vat = 'X'.                           "begin "note990329
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'T001'
        fieldname = 'STCEG'
      TABLES
        dfies_tab = lt_dfies.
    READ TABLE lt_dfies
      INTO ls_dfies
      INDEX 1.
    ld_strlen = strlen( ls_dfies-reptext ).
    WRITE: / ls_dfies-reptext(ld_strlen)  NO-GAP, ':'.
    IF  italy = 'X'
    AND t001-stceg(2) = 'IT'.
      WRITE t001-stceg+2.
    ELSE.
      WRITE t001-stceg.
    ENDIF.
  ENDIF.                                    "end "note990329

* only for Poland
  IF poland = 'X' AND listanfg IS INITIAL.
    WRITE: /39 text-007.
    listanfg = 'X'.
  ENDIF.
  ULINE.
* if required detail
  IF tabsum NE 'X'.
* detail line 1
    IF textflag = '2'.
      MOVE: text-110 TO ph_text1(8),
            text-111 TO ph_text1+9(6),
            text-112 TO ph_text1+16(10),
            text-113 TO ph_text1+27(6),
            text-114 TO ph_text1+34(6),
            text-115 TO ph_text1+41(2),
            text-116 TO ph_text1+44(16),
            text-117 TO ph_text1+61(26).
      IF sort_key = '1'.
        IF no_cpu IS INITIAL.
          MOVE: text-004 TO ph_text1+9(17).
        ELSE.
          MOVE: text-104 TO ph_text1+9(17).
        ENDIF.
      ENDIF.
      IF sort_key = '2' AND NOT no_cpu IS INITIAL.
        MOVE: text-104 TO ph_text1+9(17).
      ENDIF.
      IF sort_key = '3'
      OR sort_key = '4'.
        IF no_cpu = 'X'.
          MOVE: text-104 TO ph_text1+9(17).
        ENDIF.
      ENDIF.
      IF z_nummer NE 'X'.
        WRITE:
          /1   ph_text1+9(123).
      ELSE.
        WRITE:
          /1   ph_text1.
      ENDIF.
* detail line 2
* Ajuste Diego
*      WRITE: /1  text-120,
*              27 text-121.
**              40 text-122.
*      IF p_accseq NE 'X'.
*        WRITE 31 text-123.
*      ELSE.
*        WRITE 31 text-126.
*      ENDIF.
      IF p_accseq NE 'X'.
        WRITE /1 text-123.
      ELSE.
        WRITE /1 text-126.
      ENDIF.
      WRITE:  11 text-120,
              37 text-121.
*             40 text-122.
* Fin ajuste Diego
*      WRITE:  53 text-124,
*              58 text-125.
*      IF p_accseq NE 'X'.
*        WRITE 63 text-126.
*      ELSE.
*        WRITE 63 text-123.
*      ENDIF.
      WRITE:
*              74 text-127,
*              77 text-128,
*              94 text-129,
             44 text-130,
             62 text-131.
      ULINE.
* description of detail line
*     IF PH_FLAG = '0'.
*      IF Z_NUMMER = 'X'.
*        WRITE:
*          /1   ZNUMMU,
*               K.
*      ELSE.
*        WRITE:
*          /1   K.
*      ENDIF.
*    ENDIF.

* amount of previous pages
      IF seit_sum = 'X'.
        IF wasol NE 0 OR wahab NE 0.
          WRITE: /25 text-001.
          IF wasol GT max_length OR wahab GT max_length.
            WRITE:
               54(23) wahab CURRENCY t001-waers NO-ZERO,
              /37(23) wasol CURRENCY t001-waers NO-ZERO.
          ELSE.
            WRITE:
              44(16) wasol CURRENCY t001-waers NO-ZERO,
              62(16) wahab CURRENCY t001-waers NO-ZERO.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.
* description line 1 and 2 of initial values from table S
  IF textflag = '1'.
    WRITE:
        /1   text-102,
        /    text-103.
    ULINE.
  ENDIF.
ENDFORM.                    "OUTPUT_TOP
*---------------------------------------------------------------------*
* 13.   FORM OUTPUT_EOP                                               *
*---------------------------------------------------------------------*
*       Statements executed
*       at the event     1. END_OF_PAGE
*---------------------------------------------------------------------*
FORM output_eop.
* if required print page totals
  IF seit_sum = 'X'.
    IF wssol NE 0 OR wshab NE 0.
      WRITE: /25 text-002.
      IF wssol GT max_l_page OR wshab GT max_l_page.
        WRITE:
           42(17) wssol CURRENCY t001-waers NO-ZERO NO-SIGN,
           62(17) wshab CURRENCY t001-waers NO-ZERO.
      ELSE.
        WRITE:
          44(16) wssol CURRENCY t001-waers NO-ZERO,
          62(16) wshab CURRENCY t001-waers NO-ZERO.
      ENDIF.
    ENDIF.
    IF wasol NE 0 OR wahab NE 0.
      WRITE: /25 text-005.
      IF wasol GT max_length OR wahab GT max_length.
        WRITE:
           54(23) wahab CURRENCY t001-waers NO-ZERO,
          /37(23) wasol CURRENCY t001-waers NO-ZERO.
      ELSE.
        WRITE:
          44(16) wasol CURRENCY t001-waers NO-ZERO,
          62(16) wahab CURRENCY t001-waers NO-ZERO.
      ENDIF.
    ENDIF.
    wssol = wshab = 0.
  ENDIF.
ENDFORM.                    "OUTPUT_EOP
*---------------------------------------------------------------------*
* 14.   FORM SORT_DATA                                                *
*---------------------------------------------------------------------*
*       Statements executed
*       at the event     1. END_OF_SELECTION
*---------------------------------------------------------------------*
FORM sort_data.

  CASE sort_key.
    WHEN '1'.
      SORT.
    WHEN '2'.
      SORT BY bukrs_bk
              wjahr
              wmon
              wbudat
*              bkpf-cpudt                                   "n1053772
              gd_belnr
              gd_buzei
              wzaehl.
    WHEN '3'.
      SORT BY bukrs_bk
              wjahr
              wmon
              bkpf-cpudt
              gd_belnr
              gd_buzei
              wzaehl.
    WHEN '4'.
      SORT BY bukrs_bk
              bkpf-cpudt
              gd_belnr
              gd_buzei
              wzaehl.

  ENDCASE.
ENDFORM.                    "SORT_DATA
*---------------------------------------------------------------------*
* 15.   FORM OUTPUT_STAB                                              *
*---------------------------------------------------------------------*
*       Statements executed
*       at the event     1. END_OF_SELECTION
*---------------------------------------------------------------------*
FORM output_stab.
  SORT s.
  PERFORM check_table_s.
  LOOP AT s.

    AT NEW bukrs.
      MOVE s-bukrs      TO bhdgd-bukrs.
      MOVE bhdgd-bukrs  TO bhdgd-werte.
      PERFORM new-section(rsbtchh0).
    ENDAT.
    IF split_s IS INITIAL.
      WRITE:
        /        s-jahr,
                 s-waers,
                 s-bukrs,
                 s-mon,
         'S', 23(16) s-ssol CURRENCY s-waers,
              44(16) s-shab CURRENCY s-waers,
       56'D', 59(16) s-dsol CURRENCY s-waers,
              76(16) s-dhab CURRENCY s-waers,
       92'K', 95(16) s-ksol CURRENCY s-waers,
             112(16) s-khab CURRENCY s-waers.
      AT END OF bukrs.
        SUM.
        WRITE:
          /        s-jahr,
                   s-waers,
                   s-bukrs,
                   '**',
          'S', 23(16) s-ssol CURRENCY s-waers,
               44(16) s-shab CURRENCY s-waers,
        56'D', 59(16) s-dsol CURRENCY s-waers,
               76(16) s-dhab CURRENCY s-waers,
        92'K', 95(16) s-ksol CURRENCY s-waers,
              112(16) s-khab CURRENCY s-waers.
        SKIP 1.
      ENDAT.
      AT END OF waers.
        SUM.
        WRITE:
          /        s-jahr,
                   s-waers,
                   '****',
                   '**',
         'S', 23(16) s-ssol CURRENCY s-waers,
              44(16) s-shab CURRENCY s-waers,
       56'D', 59(16) s-dsol CURRENCY s-waers,
              76(16) s-dhab CURRENCY s-waers,
       92'K', 95(16) s-ksol CURRENCY s-waers,
             112(16) s-khab CURRENCY s-waers.
        SKIP 1.
      ENDAT.

    ELSE.             "IF SPLIT_S IS INITIAL.

      WRITE:
        /        s-jahr,
                 s-waers,
                 s-bukrs,
                 s-mon,
         'S', 23(24) s-ssol CURRENCY s-waers,
                (24) s-shab CURRENCY s-waers,

        /        s-jahr,
                 s-waers,
                 s-bukrs,
                 s-mon,
         'D', 23(24) s-dsol CURRENCY s-waers,
                (24) s-dhab CURRENCY s-waers,

        /        s-jahr,
                 s-waers,
                 s-bukrs,
                 s-mon,
         'K', 23(24) s-ksol CURRENCY s-waers,
                (24) s-khab CURRENCY s-waers.

      AT END OF mon.
        SKIP 1.
      ENDAT.

      AT END OF bukrs.
        SUM.
        SKIP 1.
        WRITE:
          /        s-jahr,
                   s-waers,
                   s-bukrs,
                   '**',
          'S', 23(24) s-ssol CURRENCY s-waers,
                 (24) s-shab CURRENCY s-waers,

          /        s-jahr,
                   s-waers,
                   s-bukrs,
                   '**',
          'D', 23(24) s-dsol CURRENCY s-waers,
                 (24) s-dhab CURRENCY s-waers,
          /        s-jahr,
                   s-waers,
                   s-bukrs,
                   '**',
          'K', 23(24) s-ksol CURRENCY s-waers,
                 (24) s-khab CURRENCY s-waers.
        SKIP 1.
      ENDAT.

      AT END OF waers.
        SUM.
        WRITE:
          /        s-jahr,
                   s-waers,
                   '****',
                   '**',
         'S', 23(24) s-ssol CURRENCY s-waers,
                (24) s-shab CURRENCY s-waers,

          /        s-jahr,
                   s-waers,
                   '****',
                   '**',
         'D', 23(24) s-dsol CURRENCY s-waers,
                (24) s-dhab CURRENCY s-waers,

          /        s-jahr,
                   s-waers,
                   '****',
                   '**',
         'K', 23(24) s-ksol CURRENCY s-waers,
                (24) s-khab CURRENCY s-waers.
        SKIP 1.
      ENDAT.

    ENDIF.              "IF SPLIT_S IS INITIAL.

  ENDLOOP.
ENDFORM.                    "OUTPUT_STAB

*&---------------------------------------------------------------------
*&      Form  SPECIAL_DOCUMENT
*&---------------------------------------------------------------------
*  Set automatically the select options for year (BR_GJAHR)
*  and posting date (BR_BUDAT) to add previous year and
*  posting date for last period.
*----------------------------------------------------------------------
FORM special_document.

  DATA: ld_monat   LIKE bkpf-monat.
  DATA: ld_poper   TYPE poper.
  DATA: prev_gjahr LIKE bkpf-gjahr.
  READ TABLE gt_gjahr INDEX 1.
  prev_gjahr = gt_gjahr-low - 1.
  MOVE prev_gjahr   TO prev_year.

  DESCRIBE TABLE s_sblart LINES entries_dat.
  IF entries_dat GE 1.
    IF fagl_active = 'X'.
      SELECT SINGLE * FROM t009 WHERE periv = gs_orginfo-periv.
      MOVE t009-anzbp TO ld_poper.
*   check if a shortened FY was used last FY?
      SELECT SINGLE * FROM t009y WHERE periv = gs_orginfo-periv
                                 AND   gjahr = prev_gjahr.
      IF sy-subrc = 0.
        MOVE t009y-anzbp TO ld_poper.
      ENDIF.
      firstday_sdoct = br_budat-low.
      CALL FUNCTION 'G_POSTING_DATE_OF_PERIOD_GET'
          EXPORTING
            period                    = ld_poper
            variant                   = gs_orginfo-periv
            year                      = prev_gjahr
          IMPORTING
*          FROM_DATE                 = VON_DATUM
*         LAST_NORMAL_PERIOD        =
            to_date                   = br_budat-low
*         FROM_DATE_ORIG            =
*       EXCEPTIONS
*         PERIOD_NOT_DEFINED        = 1
*         VARIANT_NOT_DEFINED       = 2
*         OTHERS                    = 3
  .
      CALL FUNCTION 'G_POSTING_DATE_OF_PERIOD_GET'
          EXPORTING
            period                    = 001
            variant                   = gs_orginfo-periv
            year                      = gt_gjahr-low
          IMPORTING
*          FROM_DATE                 = VON_DATUM
*         LAST_NORMAL_PERIOD        =
            to_date                   = br_budat-high
*         FROM_DATE_ORIG            =
*       EXCEPTIONS
*         PERIOD_NOT_DEFINED        = 1
*         VARIANT_NOT_DEFINED       = 2
*         OTHERS                    = 3
  .
    ELSE.
      SELECT SINGLE * FROM t009 WHERE periv = t001-periv.
      MOVE t009-anzbp TO ld_monat.
*   check if a shortened FY was used last FY?
      SELECT SINGLE * FROM t009y WHERE periv = t001-periv
                                 AND   gjahr = prev_gjahr.
      IF sy-subrc = 0.
        MOVE t009y-anzbp TO ld_monat.
      ENDIF.
      firstday_sdoct = br_budat-low.
      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
        EXPORTING
          i_gjahr = prev_gjahr
          i_monat = ld_monat
          i_periv = t001-periv
        IMPORTING
          e_fday  = br_budat-low.

      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
        EXPORTING
          i_gjahr = br_gjahr-low
          i_monat = 01
          i_periv = t001-periv
        IMPORTING
          e_fday  = br_budat-high.
    ENDIF.
    br_budat-high    = br_budat-high - 1.
    br_budat-sign    = 'I'.
    br_budat-option  = 'BT'.
    APPEND br_budat.
    br_gjahr-low     = prev_gjahr.
    br_gjahr-sign    = 'I'.
    br_gjahr-option  = 'EQ'.
    APPEND br_gjahr.
    CLEAR: br_budat, br_gjahr.
  ENDIF.

ENDFORM.                               " SPECIAL_DOCUMENT

*&----------------------------------------------------------------------
*&      Form  SPECIAL-CHECK
*&----------------------------------------------------------------------
* Check special document for previous year
* Check that the document posted in previous year was done
* using document type in new select-options S_SBLART and
* with entry date in new select-options S_SCPUDT.
*-----------------------------------------------------------------------
FORM special_check.

  DATA: x_poper LIKE t009b-poper,
        x_gjahr LIKE t009b-bdatj.

  IF bkpf-gjahr EQ prev_year.
    IF NOT bkpf-blart IN s_sblart.
      sw_chk = 'X'.
    ELSE.
      IF NOT bkpf-cpudt IN s_scpudt.
        sw_chk = 'X'.
      ELSE.
        MOVE: bkpf-bldat TO ww_date.
        MOVE: bkpf-budat TO bkpf-bldat.
        MOVE: ww_date    TO bkpf-budat.
        CLEAR: x_poper, x_gjahr.
        IF fagl_active = 'X'.
          CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
            EXPORTING
              i_date  = bkpf-budat
              i_periv = gs_orginfo-periv
            IMPORTING
              e_buper = x_poper
              e_gjahr = x_gjahr.
        ELSE.
          CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
            EXPORTING
              i_date  = bkpf-budat
              i_periv = t001-periv
            IMPORTING
              e_buper = x_poper
              e_gjahr = x_gjahr.
        ENDIF.
        MOVE x_poper TO bkpf-monat.
        MOVE x_gjahr TO bkpf-gjahr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " SPECIAL-CHECK
*&---------------------------------------------------------------------*
*&      Form  STORE_TAB_OPEN_FI
*&---------------------------------------------------------------------*
*       you can append or store table BKPF_ADD_ON_TAB with
*       business transaction event 00003310 (process event)
*----------------------------------------------------------------------*
*       STORE_TAB:
*         ' '  -->   append BKPF_ADD_ON_TAB
*         'X'  -->   store  BKPF_ADD_ON_TAB
*----------------------------------------------------------------------*
FORM store_tab_open_fi
         TABLES t_bkpf_addon_tab STRUCTURE t_bkpf_addon_tab
         USING  store_tab        LIKE store_tab
                test             LIKE test
                budat_low        LIKE budat_low
                budat_high       LIKE budat_high.

  MOVE-CORRESPONDING bkpf TO t_bkpf_addon_tab.
  MOVE vzaehl TO t_bkpf_addon_tab-belnr_alt.

  CALL FUNCTION 'OPEN_FI_PERFORM_00003310_P'
    EXPORTING
      i_land1         = t001-land1
      i_store_tab     = store_tab
      i_testlauf      = test
      i_budat_low     = budat_low
      i_budat_high    = budat_high
    TABLES
      t_bkpf_addon    = t_bkpf_addon_tab
    CHANGING
      i_check_open_fi = check_open_fi.

ENDFORM.                               " STORE_TAB_OPEN_FI

*&---------------------------------------------------------------------*
*&      Form  WRITE_K
*&---------------------------------------------------------------------*
*       Writes header title K.
*       Writing of K had to be separated for hebrew language 4.6C
*----------------------------------------------------------------------*
FORM write_k.

  IF sort_key = '1'.
    WRITE: k(10),         "BELNR
           k+11(6).       "CPUDAT
  ELSE.
    IF no_cpu IS INITIAL.
      WRITE: k(6),        "CPUDAT
             k+7(10).     "BELNR
    ELSE.
      WRITE: k(10),       "BELNR
             k+11(6).     "CPUDAT
    ENDIF.
  ENDIF.

  WRITE: k-budat,
         k-bldat,
         k-blart,
         k-xblnr,
         k-bktxt,
         k-usnam.           "only filled for Poland

ENDFORM.                    " WRITE_K
*&---------------------------------------------------------------------*
*&      Form  WRITE_P1
*&---------------------------------------------------------------------*
*       Writes structure P1
*       Writing of P1 had to be separated for hebrew language 4.6C
*----------------------------------------------------------------------*
FORM write_p1.

  shift p1-konto LEFT DELETING LEADING '0'.

* Ajuste Diego
*  WRITE: /1  p1-text,
*             p1-buzei.
**            p1-koart.                                      "n1045456
*  CASE turkey.                                              "n1045456
*    WHEN space.                                             "n1045456
*      WRITE  p1-konto.                                      "n1045456
*    WHEN OTHERS.                                            "n1045456
*      WRITE  p1-konto NO-ZERO.                              "n1045456
*  ENDCASE.                                                  "n1045456
  CASE turkey.                                              "n1045456
    WHEN space.                                             "n1045456
      WRITE /1 p1-konto.                                      "n1045456
    WHEN OTHERS.                                            "n1045456
      WRITE /1 p1-konto NO-ZERO.                              "n1045456
  ENDCASE.
  WRITE:    11 p1-text,
            37 p1-buzei.
* Fin Ajuste Diego

*  WRITE:     p1-gsber,                                      "n1045456
*             p1-bschl NO-GAP,
*             p1-umskz NO-GAP,
*             p1-xnegp.                                      "n1045456
*  CASE turkey.                                              "n1045456
*    WHEN space.                                             "n1045456
*      WRITE  p1-hkont.                                      "n1045456
*    WHEN OTHERS.                                            "n1045456
*      WRITE  p1-hkont NO-ZERO.                              "n1045456
*  ENDCASE.                                                  "n1045456
*  WRITE:     p1-mwskz.                                      "n1045456

ENDFORM.                                                    " WRITE_P1
*---------------------------------------------------------------------*
*       FORM CHECK_TABLE_S                                            *
*---------------------------------------------------------------------*
*       Checks if the amounts in table S are to large
*       SPLIT_S = 'X' --> list layout has to be changed for table S
*---------------------------------------------------------------------*
FORM check_table_s.

  CLEAR split_s.
  LOOP AT s.
    IF s-ssol GT max_length
      OR s-shab GT max_length
      OR s-dsol GT max_length
      OR s-dhab GT max_length
      OR s-ksol GT max_length
      OR s-khab GT max_length.
      split_s = 'X'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "CHECK_TABLE_S
*&---------------------------------------------------------------------*
*&      Form  page_number_italy
*&---------------------------------------------------------------------*
*       prints special page number for Italy
*----------------------------------------------------------------------*
FORM page_number_italy.

  pagno = pagno + 1.
  w_pagno = pagno.
* due to the italian law the printed year must be derived from the
* first posting period of the used fiscal year variant
  CONCATENATE firstday(4) slash w_pagno INTO pagno_year.
  IF pagno < 1000000.                                       "n1074027
    WRITE 122(11) pagno_year RIGHT-JUSTIFIED.               "n1074027
  ELSEIF pagno < 10000000.                                  "n1074027
    WRITE 120(13) pagno_year RIGHT-JUSTIFIED.               "n1074027
  ELSE.                                                     "n1074027
    WRITE 119(14) pagno_year RIGHT-JUSTIFIED.               "n1074027
  ENDIF.                                                    "n1074027

ENDFORM.                    "PAGE_NUMBER_ITALY

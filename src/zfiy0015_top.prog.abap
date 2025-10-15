*&---------------------------------------------------------------------*
*&  Include           ZFI_R_031_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* Tables                                                               *
*----------------------------------------------------------------------*
TABLES:
  bkpf, *bkpf,                  " Accounting document header------------
  bsec,                         " One-time account data document segment
  bseg,*bseg,                   " Accounting document segment-----------
  bset,*bset,                   " Tax data document segment-------------
  bsak,                         " Secondary index for vend.-cleared itms
  bsad,                         " Secondary index for cust.-cleared itms
  vbrk,vbrp,                    " Billing: Header Data------------------
  rbkp,*rbkp,                   " Document header: invoice receipt------
  konv,                         " Conditions (Procedure Data)-----------
  kna1,                         " General Data in Customer Master-------
  konp,                         " Conditions (Item)---------------------
  bkorm,                        " Accounting correspondence requests----
  lfa1,                         " Vendor master (general section)-------
  skat,                         " G/L account master record-------------
  t001,                         " Company Codes-------------------------
  t003,                         " Document types------------------------
  t003_i,                       " Document types per country------------
  t003t,                        " Document type texts-------------------
  t001z,                        " Additional specific. for company code
  t005,                         " Countries-----------------------------
  t005s,                        " Regions-------------------------------
  t007a,                        " Tax keys------------------------------
  t007b,                        " Tax Processing in Accounting----------
  t007s,                        " Tax Code Names------------------------
  t685t,                        " Conditions: Types: Texts--------------
  j_1ataxid,                    " AR: Tax identification----------------
  j_1afrid,                     " AR: Identification code for foreigners
  j_1adrver,                    " AR: Versions of VAT daily report------
  j_1adrvert,                   " AR: Texts for versions----------------
  j_1aoftpt,                    " AR: Texts for official document types-
  j_1aotdetr,                   " AR: Determination of off. doc. type---
  j_1a101,                      " AR: Historical data for daily reports-
  j_1arztx,                     " AR: Reason for zero VAT per tax code--
  bvor,                         " Intercompany posting procedures-------
  sadr,                         " Address Management: Company Data------
  addr1_sel,                    " Address selection parameter-----------
  tcurc,                        " Currency Codes------------------------
  t028m,                        " Class for alternative currency keys---
  idfiar205,
  vbpa,                         "Sales Document: Partner----------------
  adrc,                         "Addresses (Business Address Services)--
  vbpa3,                        "Tax Numbers for One-Time Customers-----
  sarkey.                       "Archieved documents  1069346

*----------------------------------------------------------------------*
* Class: Utilities for Processing Characters                           *
* Get "Carriage Return and Line Feed" Character Pair                   *
*----------------------------------------------------------------------*
CLASS cl_abap_char_utilities DEFINITION LOAD.

*----------------------------------------------------------------------*
* Selection parameters                                                 *
*----------------------------------------------------------------------*
* Note 1069346 Start
*SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-s01.
*SELECT-OPTIONS:  s_belnr      FOR  bkpf-belnr,
*                 s_blart      FOR  bkpf-blart,
*                 s_budat      FOR  bkpf-budat OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK 1.
* Note 1069346 End
DATA: par_cust TYPE idfiar205-fiar205acctp,
      par_vend TYPE idfiar205-fiar205acctp.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-s02.
  SELECT-OPTIONS: s_blart FOR bkpf-blart.                   "1069346
  SELECT-OPTIONS: s_bldat FOR bkpf-bldat,
                   s_taxcd      FOR  bseg-mwskz,
                   s_lifnr      FOR  bseg-lifnr.
*PARAMETERS:      par_cust     TYPE idfiar205-fiar205acctp,
*                 par_vend     TYPE idfiar205-fiar205acctp.
  PARAMETERS: r_cust TYPE idfiar205-fiar205acctp NO-DISPLAY,
              r_vend TYPE idfiar205-fiar205acctp DEFAULT 'X' NO-DISPLAY.
* Processing keys: sel_kts1: Primary processing key
*                  sel_kts2: Secondary processing key
  SELECT-OPTIONS: sel_kts1 FOR idfiar205-prkeyprin,
                   sel_kts2     FOR idfiar205-prkeyprin.
SELECTION-SCREEN END OF BLOCK 2.

SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-s05.

  PARAMETERS: par_vers LIKE idfiar205-compressflag NO-DISPLAY,
              s_drver  LIKE j_1adrver-j_1adrver OBLIGATORY,
              s_event  LIKE idfiar205-fiarcorrtyp
                                        DEFAULT 'SAPA1'.
*SELECTION-SCREEN ULINE.
*SELECTION-SCREEN COMMENT /1(79) text-c01.
  PARAMETERS: p_cai LIKE idfiar205-fiarcorrtyp.
  PARAMETERS: p_bktxt LIKE idfiar205-fiarpacflag.
* --- Begin --------
  PARAMETERS: p_todos AS CHECKBOX DEFAULT 'X'.
* --- End   --------
SELECTION-SCREEN END OF BLOCK 3.

SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE TEXT-s04.
  PARAMETERS: s_delete LIKE idfiar205-histtabdel NO-DISPLAY,
              par_updh LIKE idfiar205-histtabupd NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK 4.

*SELECTION-SCREEN BEGIN OF BLOCK 5 WITH FRAME TITLE text-s06.
* Default: Daily VAT report is run
PARAMETERS: par_magn LIKE idfiar205-fiar205radio DEFAULT ' ' NO-DISPLAY.
*SELECTION-SCREEN BEGIN OF BLOCK 51 WITH FRAME TITLE text-s07.
PARAMETERS: par_file     LIKE rfpdo1-allgunix NO-DISPLAY,
            par_loc      LIKE idfiar205-fiarlocfile NO-DISPLAY,
            par_lfil(70) NO-DISPLAY.
*                Create perception file
*SELECTION-SCREEN END OF BLOCK 51.
*SELECTION-SCREEN BEGIN OF BLOCK 52 WITH FRAME TITLE text-s08.
PARAMETERS: par_perf     LIKE rfpdo1-allgunix NO-DISPLAY,
            par_ploc     LIKE idfiar205-fiarlocfile NO-DISPLAY,
            par_pfil(70) NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK 52.
PARAMETERS: par_dele LIKE idfiar205-fiardelfile NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK 5.

SELECTION-SCREEN BEGIN OF BLOCK 6 WITH FRAME TITLE TEXT-s03.
  PARAMETERS: par_comp LIKE idfiar205-compressflag NO-DISPLAY,
              s_init   LIKE j_1afpdo-pgdocnumb     NO-DISPLAY,
              s_numbc  LIKE j_1afpdo-totpgnumbc    NO-DISPLAY,
              par_sort LIKE idfiar205-fiar205sort  NO-DISPLAY,
              par_kts1 LIKE idfiar205-prkeysdrt    NO-DISPLAY,
              par_sum  LIKE idfiar205-surchsum     NO-DISPLAY,
              par_rahm LIKE idfiar205-listbox      NO-DISPLAY,
              par_sddi LIKE idfiar205-dsplvf03     NO-DISPLAY,
              par_dspt LIKE idfiar205-dsplmtax     NO-DISPLAY,
              par_canc LIKE idfiar205-id205canc    NO-DISPLAY.   " Note 427832
  PARAMETERS: " p_col2 to p_col11: texts for column titles
    p_col2  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col3  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col4  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col5  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col6  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col7  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col8  LIKE idfiar205-coltitle   NO-DISPLAY,
    p_txid1 LIKE idfiar205-colprockey NO-DISPLAY, "TaxID Exports
    p_col09 LIKE idfiar205-coltitle   NO-DISPLAY,
    p_txid2 LIKE idfiar205-colprockey NO-DISPLAY, "TaxID PercNC
    p_col10 LIKE idfiar205-coltitle   NO-DISPLAY,
    p_col11 LIKE idfiar205-coltitle   NO-DISPLAY,
    p_sdexp LIKE t685t-vtext          NO-DISPLAY. " text: SD export total
SELECTION-SCREEN END OF BLOCK 6.

*SELECTION-SCREEN BEGIN OF BLOCK 7 WITH FRAME TITLE text-051.
*PARAMETERS:      p_pdf AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK 7.

*----------------------------------------------------------------------
* TYPE DEFINITION
*----------------------------------------------------------------------
* field structure for the first output list-----------------------------
TYPES:
  BEGIN OF type_ep,
    mwskz           LIKE bset-mwskz,
    bukrs           LIKE bkpf-bukrs,
    budat           LIKE bkpf-budat,
    buper(6)        TYPE n,
    brnch           LIKE bkpf-brnch,
    cpudt           LIKE bkpf-cpudt,
    cputm           LIKE bkpf-cputm,
    belnr           LIKE bkpf-belnr,
    blart           LIKE bkpf-blart,  " Document type
    stblg           LIKE bkpf-stblg,
    xblnr           LIKE bkpf-xblnr,
    waers           LIKE bkpf-waers,
    bldat           LIKE bkpf-bldat,
    wwert           LIKE bkpf-wwert,  " For magnetic output->Exch. rate
    kursf           LIKE bkpf-kursf,  " For magnetic output->Exch. rate
    gjahr           LIKE bkpf-gjahr,
    monat           LIKE bkpf-monat,
    flg_sd_beleg    TYPE c,
    flg_is_beleg    TYPE c,           " IS Media document
    sd_vbeln        LIKE vbrk-vbeln,
    bvorg           LIKE bkpf-bvorg,
*    mwskz            LIKE bset-mwskz,
    posnr           LIKE vbrp-posnr,
    ktosl           LIKE bset-ktosl,  " Processing key
    kschl           LIKE bset-kschl,
    linetype        TYPE c,
    ktnra           LIKE bseg-lifnr,
    hwbas           LIKE bset-hwbas,  " Tax base amount local currency
    hwste           LIKE bset-hwste,  " Tax amount in local currency
    name1           LIKE lfa1-name1,
    stcdt           LIKE lfa1-stcdt,
    stcd1           LIKE lfa1-stcd1,
    j_1aoftp        LIKE j_1aotdetr-j_1aoftp, " For magnetic output
    oftp_text       LIKE j_1aoftpt-text5,
    prtchr          LIKE j_1aotdetr-j_1aprtchr,
    augdt           LIKE bseg-augdt,
    augbl           LIKE bseg-augbl,
    koart           LIKE bseg-koart,
    hkont           LIKE bset-hkont,
    j_1arfz         LIKE j_1arztx-j_1arfz,
    fisc_cont       TYPE c,           " For magnetic output
    numpg           LIKE bkpf-numpg,  " For magnetic output
    fityp           LIKE kna1-fityp,  " For magnetic output
*   cai(14)          TYPE n,           " For magnetic output
    cai(14)         TYPE c,          " For magnetic o/p "RG1361 Changes
    due_date        TYPE d,           " For magnetic output
*---> 20/06/2023 - Migração S4 - MA
*    total(8)        TYPE p DECIMALS 2,           " End of first line
    total           TYPE dmbtr,
*<--- 20/06/2023 - Migração S4 - MA
* Begin of first line
    rate(5)         TYPE p DECIMALS 2,
    dspl_rate(4)    TYPE p DECIMALS 2,
    taxed(8)        TYPE p DECIMALS 2,
    not_taxed(8)    TYPE p DECIMALS 2,
    vat(8)          TYPE p DECIMALS 2,
    rnr_vat(8)      TYPE p DECIMALS 2,
    vat_percep(8)   TYPE p DECIMALS 2,
    other_percep(8) TYPE p DECIMALS 2,
    munic_per(8)    TYPE p DECIMALS 2, " Municipal Perception - magnetic output
    earn_per(8)     TYPE p DECIMALS 2, " Earning Perc. - magn. outp. - Note 645449
    vat_intern(8)   TYPE p DECIMALS 2, " Internal taxes       - magnetic output
    exemption(8)    TYPE p DECIMALS 2,
    surcharge(8)    TYPE p DECIMALS 2, " VAT Surcharge
    exports(8)      TYPE p DECIMALS 2,           " Exports
    percepnoc(8)    TYPE p DECIMALS 2,           " Perception not categorized
    line_total(8)   TYPE p DECIMALS 2,
  END OF type_ep.

TYPES:
  BEGIN OF type_tab_bseg,
    bukrs LIKE bseg-bukrs,
    belnr LIKE bseg-belnr,
    buzei LIKE bseg-buzei,
    koart LIKE bseg-koart,
    hkont LIKE bseg-hkont,
    wrbtr LIKE bseg-wrbtr,
    dmbtr LIKE bseg-dmbtr,
    wmwst LIKE bseg-wmwst,
    umskz LIKE bseg-umskz,
    umsks LIKE bseg-umsks,
    xzahl LIKE bseg-xzahl,
    rebzg LIKE bseg-rebzg,
    rebzj LIKE bseg-rebzj,
    rebzz LIKE bseg-rebzz,
    mwsts LIKE bseg-mwsts,
    txgrp LIKE bseg-txgrp,
    esrre LIKE bseg-esrre,
    mwskz LIKE bseg-mwskz,
    xauto LIKE bseg-xauto,
    ktosl LIKE bseg-ktosl,
    mwart LIKE bseg-mwart,
    buzid LIKE bseg-buzid,
    vorgn LIKE bseg-vorgn,
    xcpdd LIKE bseg-xcpdd,
    kunnr LIKE bseg-kunnr,
    lifnr LIKE bseg-lifnr,
    ktnra LIKE bseg-lifnr,
    augdt LIKE bseg-augdt,
    augbl LIKE bseg-augbl,
    xcpdk LIKE kna1-xcpdk,
    fityp LIKE kna1-fityp,
    zfbdt LIKE bseg-zfbdt, " Magnetic output - determine due date
    zterm LIKE bseg-zterm, " Magnetic output - determine due date
    zbd1t LIKE bseg-zbd1t, " Magnetic output - determine due date
    zbd2t LIKE bseg-zbd2t, " Magnetic output - determine due date
    zbd3t LIKE bseg-zbd3t, " Magnetic output - determine due date
    total LIKE bseg-dmbtr,
  END OF type_tab_bseg.

TYPES:
  BEGIN OF type_tab_bset,
    mwskz     LIKE bset-mwskz, " Tax code
    hwbas     LIKE bset-hwbas,
    hwste     LIKE bset-hwste,
    kbetr     LIKE bset-kbetr, " Rate
    knumh     LIKE bset-knumh,
    txgrp     LIKE bset-txgrp,
    shkzg     LIKE bset-shkzg,
    xkposn    LIKE vbrp-posnr,
    hkont     LIKE bset-hkont,
    ktosl     LIKE bset-ktosl, " Procesing key
    kschl     LIKE bset-kschl,
    txmod     LIKE bset-txmod,
    mwart     LIKE t007a-mwart, " Tax type
    zmwsk     LIKE t007a-zmwsk, " Target tax code
    stgrp     LIKE t007b-stgrp, " Tax type (1/2/3)
    stazf     LIKE t007b-stazf, " Tax not deductible
    j_1arfz   LIKE j_1arztx-j_1arfz, " Reason for zero tax
    regio     LIKE j_1ataxid-regio,
    j_1ataxid LIKE j_1ataxid-j_1ataxid,
  END OF type_tab_bset.

TYPES:
  BEGIN OF type_tab_taxes,
    mwskz      LIKE bset-mwskz, " Tax code
    posnr      LIKE vbrp-posnr,
    rate(5)    TYPE n,          " tax rate
    stgrp      LIKE t007b-stgrp, " Tax type (1/2/3)
    j_1arfz    LIKE j_1arztx-j_1arfz, " Reason for zero tax
    ktosl      LIKE bset-ktosl, " Procesing key
    hkont      LIKE bset-hkont,
    hwbas      LIKE bset-hwbas,
    hwste      LIKE bset-hwste,
    txmod      LIKE bset-txmod,
    netamount  LIKE bseg-dmbtr,
    amount     LIKE bseg-dmbtr,
    amount2    LIKE bseg-dmbtr, " Taxes
    kschl      LIKE bset-kschl,
    mwart      LIKE t007a-mwart,
    zmwsk      LIKE t007a-zmwsk, " Target tax type
    stazf      LIKE t007b-stazf, " Tax not deductible
    j_1ataxid  LIKE j_1ataxid-j_1ataxid,
* for display
    taxed      LIKE bseg-dmbtr, " Tax base amount
    vat_amount LIKE bseg-dmbtr, " RR VAT amount
    not_taxed  LIKE bseg-dmbtr, " RnR VAT amount
    vat_intern LIKE bseg-dmbtr, " Internal tax
    exemption  LIKE bseg-dmbtr, " Exemption
    surcharge  LIKE bseg-dmbtr, " VAT Surcharge
    perception LIKE bseg-dmbtr, " VAT Perception
    region_per LIKE bseg-dmbtr, " Region Perception
    munic_per  LIKE bseg-dmbtr, " Municipal Perception
    earn_per   LIKE bseg-dmbtr, " Earning Perception  Note 645449
    ertrag_per LIKE bseg-dmbtr, " Produce Perception
    exports    LIKE bseg-dmbtr, " Exports
    percepnoc  LIKE bseg-dmbtr, " Perception not categorized
  END OF type_tab_taxes.

TYPES:
  BEGIN OF type_tab_beleg,
    linetype        TYPE c,
    ktnra           LIKE bseg-lifnr,
    mwskz           LIKE bset-mwskz,
    posnr           LIKE vbrp-posnr,
    kschl           LIKE bset-kschl,
    ktosl           LIKE bset-ktosl,
    hkont           LIKE bset-hkont,
    hwbas           LIKE bset-hwbas,
    hwste           LIKE bset-hwste,
    name1           LIKE lfa1-name1,
    stcdt           LIKE lfa1-stcdt,
    stcd1           LIKE lfa1-stcd1,
    augdt           LIKE bseg-augdt,
    augbl           LIKE bseg-augbl,
    koart           LIKE bseg-koart,
    j_1arfz         LIKE j_1arztx-j_1arfz,
    fisc_cont       TYPE c,           " For magnetic output
    fityp           LIKE kna1-fityp,  " For magnetic output
*   cai(14)          TYPE n,           " For magnetic output
    cai(14)         TYPE c,          " For magnetic o/p "RG1361 Changes
    due_date        TYPE d,           " For magnetic output
*---> 20/06/2023 - Migração S4 - MA
*    total(8)         TYPE p decimals 2,           " End of first line
    total           TYPE dmbtr,
*<--- 20/06/2023 - Migração S4 - MA
* Begin of second line
    rate(5)         TYPE p DECIMALS 2,
    taxed(8)        TYPE p DECIMALS 2,
    not_taxed(8)    TYPE p DECIMALS 2,
    vat_intern(8)   TYPE p DECIMALS 2, " Internal tax
    vat(8)          TYPE p DECIMALS 2,
    rnr_vat(8)      TYPE p DECIMALS 2,
    vat_percep(8)   TYPE p DECIMALS 2,
    other_percep(8) TYPE p DECIMALS 2,
    munic_per(8)    TYPE p DECIMALS 2, " Municipal Perception
    earn_per(8)     TYPE p DECIMALS 2, " Earning Perception         - Note 645449
    exemption(8)    TYPE p DECIMALS 2,
    exports(8)      TYPE p DECIMALS 2,
    percepnoc(8)    TYPE p DECIMALS 2,
    line_total(8)   TYPE p DECIMALS 2,
  END OF type_tab_beleg.
* Type for fiscal period
TYPES:
  BEGIN OF type_fiscal_period,
    gjahr LIKE bkpf-gjahr,
    monat LIKE bkpf-monat,
  END OF type_fiscal_period.

* Type for Perception File
TYPES:
  BEGIN OF type_p_record2,
    regio            LIKE j_1ataxid-regio, " Region
    counc            LIKE j_1ataxid-counc, " Region
    docu_date(8)     TYPE n,           " document date yyyymmdd
    docu_type(2)     TYPE c,           " document type
    subsidiary(4)    TYPE n,           " subsidiary/branch
    docu_no(8)       TYPE n,           " official document no.
    regio_code(2)    TYPE n,           " gross income jurisdiction code
    perc_gi_amnt(15) TYPE n,           " gross income perception
    munic_code(40)   TYPE c,           " municipal jurisdiction code
    perc_mp_amnt(15) TYPE n,           " municipal perception
  END OF type_p_record2.

TYPES:
* Purchases File   - length 369
  BEGIN OF type_i_record,
    record(369) TYPE c,
    cr_lf       TYPE c,
  END OF type_i_record,
* Sales File       - length 375
  BEGIN OF type_o_record,
    record(375) TYPE c,
    cr_lf       TYPE c,
  END OF type_o_record,
* Perceptions file - length 94
  BEGIN OF type_p_record,
    record(94) TYPE c,
    cr_lf      TYPE c,
  END OF type_p_record,

  BEGIN OF ty_account_set,
    hkont TYPE bsis-hkont,
  END OF ty_account_set.

*----------------------------------------------------------------------*
* Internal tables (using as prefix: "tab_")                            *
*----------------------------------------------------------------------*
DATA:
* Company code data
  BEGIN OF tab_001 OCCURS 5,
    bukrs LIKE t001-bukrs,         " Company code
    land1 LIKE t001-land1,         " Country
    waers LIKE t001-waers,         " Local currency
    ktopl LIKE t001-ktopl,         " Chart of accounts
    periv LIKE t001-periv,         " Variant for fiscal year
    kalsm LIKE t005-kalsm,         " Calculation scheme
    stcdt LIKE lfa1-stcdt,         " Tax number type
    stcd1 LIKE lfa1-stcd1,         " Tax number 1
    name1 LIKE sadr-name1,         " Address Management: Name 1
    fityp LIKE kna1-fityp,         " Fiscal type - Note 636750
  END OF tab_001,

* Tax keys
  BEGIN OF tab_007a OCCURS 15.
    INCLUDE STRUCTURE t007a.
DATA: text1   LIKE t007s-text1,
    j_1arfz LIKE j_1arztx-j_1arfz,
  END OF tab_007a,

* reason for zero vat
  BEGIN OF xrztx OCCURS 10,
    kalsm   LIKE t005-kalsm,
    mwskz   LIKE bseg-mwskz,
    j_1arfz LIKE j_1arztx-j_1arfz,
  END OF xrztx,

* Table with processing keys
  BEGIN OF tab_007b OCCURS 5.
    INCLUDE STRUCTURE t007b.
DATA: END OF tab_007b,
* TAX Identification table
BEGIN OF tab_j_1ataxid OCCURS 5.
  INCLUDE STRUCTURE j_1ataxid.
DATA: END OF tab_j_1ataxid,
* Versions of daily VAT report
BEGIN OF tab_j_1adrver OCCURS 0.
  INCLUDE STRUCTURE j_1adrver.
DATA: END OF tab_j_1adrver,
* Text for versions of daily VAT report
BEGIN OF tab_j_1adrvert OCCURS 5.
  INCLUDE STRUCTURE j_1adrvert.
DATA: END OF tab_j_1adrvert,
* Regions per document
BEGIN OF tab_regio OCCURS 50,
  bukrs LIKE bkpf-bukrs,
  belnr LIKE bkpf-belnr,
  gjahr LIKE bkpf-gjahr,
  mwskz LIKE bset-mwskz,
  regio LIKE bseg-grirg,
  ktosl LIKE bset-ktosl,                                    "1083548
END OF tab_regio,
* Official document type in Argentina
BEGIN OF tab_j_1aotdet OCCURS 10,
  land1      LIKE t001-land1,           " For magnetic output
  id_report  LIKE j_1aotdetr-id_report, " For magnetic output
  doccls     LIKE j_1aotdetr-doccls,
  j_1aprtchr LIKE j_1aotdetr-j_1aprtchr,
  j_1aoftp   LIKE j_1aotdetr-j_1aoftp,   " For magnetic output
  text5      LIKE j_1aoftpt-text5,
  text30     LIKE j_1aoftpt-text30,
END OF tab_j_1aotdet,
* Document types
*  BEGIN OF TAB_T003 OCCURS 10,
*    BLART     LIKE T003-BLART,         " Document type
*    BLKLS     LIKE T003-BLKLS,         " Percentage
*    XAUSG     LIKE T003-XAUSG,         " Self-issued numbering?
*    LTEXT     LIKE T003T-LTEXT,
*  END OF TAB_T003,

* for release 46c and above, table T003_I should be used for official
* document numbering

BEGIN OF tab_t003_i OCCURS 10,
  land1  LIKE t003_i-land1,
  blart  LIKE t003_i-blart,       " Document type
  doccls LIKE t003_i-doccls,     " Document class
  xausg  LIKE t003_i-xausg,       " Self-issued numbering?
  ltext  LIKE t003t-ltext,
END OF tab_t003_i,

* Regions for jurisdiction code (Perception file)
BEGIN OF tab_t005s OCCURS 10,
  land1 LIKE t005s-land1,
  bland LIKE t005s-bland,
  fprcd LIKE t005s-fprcd,
END OF tab_t005s.

* Table for Print Authorization Code (engl. PAC or spanish CAI)
DATA: BEGIN OF tab_pac OCCURS 10.
        INCLUDE STRUCTURE j_1apacd.
DATA: END OF tab_pac.

DATA: BEGIN OF tab_pack OCCURS 10.
        INCLUDE STRUCTURE j_1apack1.
DATA: END OF tab_pack.

*  Table for logs: Table entries which have not been found
DATA:
  BEGIN OF tab_log_entry1 OCCURS 10,
    name(10),                          " name of table
    key(50),                           " key of table
    belnr    LIKE bkpf-belnr,             " reference docnr. as example
  END OF tab_log_entry1,
* Table for logs: Table entries which have not been found
  BEGIN OF tab_log_entry10 OCCURS 10,
    name(10),                          " name of table
    key(50),                           " key of table
    belnr    LIKE bkpf-belnr,             " reference docnr. as example
  END OF tab_log_entry10,

  BEGIN OF tab_log_entry2 OCCURS 10,
    belnr           LIKE bkpf-belnr,  " document no.
    blart           LIKE bkpf-blart, " document type
    budat           LIKE bkpf-budat,  " date of document
    description(50) TYPE c,
  END OF tab_log_entry2,

  BEGIN OF tab_log_entry20 OCCURS 10,
    belnr           LIKE bkpf-belnr,  " document no.
    blart           LIKE bkpf-blart, " document type
    budat           LIKE bkpf-budat,  " date of document
    description(50) TYPE c,
  END OF tab_log_entry20,

* Conditions (Item)
  BEGIN OF tab_konp OCCURS 15,
    knumh LIKE konp-knumh,         " Condition record number
    kopos LIKE konp-kopos,
    kschl LIKE konp-kschl,
    kbetr LIKE konp-kbetr,         " Percentage
  END OF tab_konp,

  BEGIN OF tab_vbrp OCCURS 5,
    vbeln   LIKE vbrp-vbeln,
    mwskz   LIKE vbrp-mwskz,
    posnr   LIKE vbrp-posnr,
    j_1arfz LIKE vbrp-j_1arfz,
    netwr   LIKE vbrp-netwr,
    mwsbp   LIKE vbrp-mwsbp,
  END OF tab_vbrp,

* Address data for business partners
  BEGIN OF tab_adrs OCCURS 100,
    koart LIKE bseg-koart,
    ktnra LIKE bseg-lifnr,         " Account customer/vendor
    name1 LIKE lfa1-name1,         " Name
    stcdt LIKE lfa1-stcdt,
    stcd1 LIKE lfa1-stcd1,
  END OF tab_adrs,

* Table with BSEG items (no 'D' or 'K' lines, no tax lines)
  tab_bseg      TYPE type_tab_bseg OCCURS 0 WITH HEADER LINE,
  tab_bseg_nodk TYPE type_tab_bseg OCCURS 0 WITH HEADER LINE,
* Table with BSET items
  tab_bset      TYPE type_tab_bset OCCURS 0 WITH HEADER LINE,
* All the taxes for one document after Argentina Tax Classification
  tab_taxes     TYPE type_tab_taxes OCCURS 0 WITH HEADER LINE,

  BEGIN OF sum_tab_taxes OCCURS 5,
    mwskz      LIKE bset-mwskz,
    posnr      LIKE vbrp-posnr,
    netamount  LIKE bseg-dmbtr,
    amount     LIKE bseg-dmbtr,
    amount2    LIKE bseg-dmbtr, " taxes
* for display
    taxed      LIKE bseg-dmbtr, " Tax base amount
    vat_amount LIKE bseg-dmbtr, " RR VAT amount
    not_taxed  LIKE bseg-dmbtr, " RnR VAT amount
    vat_intern LIKE bseg-dmbtr, " Internal tax
    exemption  LIKE bseg-dmbtr, " Exemption
    surcharge  LIKE bseg-dmbtr, " VAT Surcharge
    perception LIKE bseg-dmbtr, " VAT Perception
    region_per LIKE bseg-dmbtr, " Region Perception
    munic_per  LIKE bseg-dmbtr, " Municipal Perception
    ertrag_per LIKE bseg-dmbtr, " Produce Perception
    exports    LIKE bseg-dmbtr, " Exports
    percepnoc  LIKE bseg-dmbtr, " Perception not categorized
* Special requirement for MM docs: Cp. form CHECK_MM_BASE_AMOUNTS
    hwbas      LIKE bseg-dmbtr, " Base amount for MM docs
  END OF sum_tab_taxes,

* All taxed lines ( compress modus )
  BEGIN OF tab_bseg_taxed OCCURS 5,
    mwskz     LIKE bset-mwskz,
    posnr     LIKE vbrp-posnr,
    taxed     LIKE bseg-dmbtr, " Tax base amount
    not_taxed LIKE bseg-dmbtr, " not taxed
    exemption LIKE bseg-dmbtr, " Exemption
    exports   LIKE bseg-dmbtr, " Exports
  END OF tab_bseg_taxed,
  BEGIN OF tax_base OCCURS 5,
    mwskz LIKE bset-mwskz,
    posnr LIKE vbrp-posnr,
  END OF tax_base,
* Table with information for all lines of a document
  tab_beleg     TYPE type_tab_beleg OCCURS 0 WITH HEADER LINE,
  tmp_tab_beleg TYPE type_tab_beleg OCCURS 0 WITH HEADER LINE,
* Customs data
  BEGIN OF wa_custom,
    number(8) TYPE n,
    code(3)   TYPE n,
    date      TYPE d,
    year(4)   TYPE n,
    destcd(4) TYPE c,  " Destination code
    chksum(1) TYPE c,  " Shipment number check-sum
  END OF wa_custom,
* Table for output of one document
  tab_ep     TYPE type_ep OCCURS 0 WITH HEADER LINE,
  tab_ep_tmp TYPE type_ep OCCURS 0 WITH HEADER LINE,
* Table to store the detials of taxes other than VAT
  tab_ep_oth TYPE type_ep OCCURS 0 WITH HEADER LINE,        "992893

  BEGIN OF tab_ep_comp OCCURS 10,
    mwskz           LIKE bset-mwskz,
    posnr           LIKE vbrp-posnr,
    dspl_rate(4)    TYPE p DECIMALS 2,
    j_1arfz         LIKE j_1arztx-j_1arfz, " For magnetic output
    taxed(8)        TYPE p DECIMALS 2,
    not_taxed(8)    TYPE p DECIMALS 2,
    vat_intern(8)   TYPE p DECIMALS 2,           " For magnetic output
    vat(8)          TYPE p DECIMALS 2,
    rnr_vat(8)      TYPE p DECIMALS 2,
    vat_percep(8)   TYPE p DECIMALS 2,
    other_percep(8) TYPE p DECIMALS 2,
    munic_per(8)    TYPE p DECIMALS 2,           " For magnetic output
    earn_per(8)     TYPE p DECIMALS 2,           " Note 645449
    exemption(8)    TYPE p DECIMALS 2,
    surcharge(8)    TYPE p DECIMALS 2,           " VAT Surcharge - For magnetic output
    exports(8)      TYPE p DECIMALS 2,           " Exports
    percepnoc(8)    TYPE p DECIMALS 2,           " Perception not categorized
    line_total(8)   TYPE p DECIMALS 2,
    belnr           TYPE belnr_d,
  END OF tab_ep_comp,

* VAT totals per tax code
  BEGIN OF tab_sum_mwskz OCCURS 30,
    mwskz    LIKE bset-mwskz,
    hwste(8) TYPE p DECIMALS 2,
  END OF tab_sum_mwskz,

* VAT totals per tax code new version
  BEGIN OF tab_sum_mwskz_new OCCURS 30,
    mwskz           LIKE bset-mwskz,
    taxed(8)        TYPE p DECIMALS 2,           " taxed
    not_taxed(8)    TYPE p DECIMALS 2,           " not taxed - Note 454626
    vat(8)          TYPE p DECIMALS 2,           " vat
    rnr_vat(8)      TYPE p DECIMALS 2,           " rnr vat
    vat_percep(8)   TYPE p DECIMALS 2,           " vat perception
    other_percep(8) TYPE p DECIMALS 2,           " other perception
    exemption(8)    TYPE p DECIMALS 2,           " exemption
    exports(8)      TYPE p DECIMALS 2,           " exports
    percepnoc(8)    TYPE p DECIMALS 2,           " perception no cat.
    line_total(8)   TYPE p DECIMALS 2,           " total_line
  END OF tab_sum_mwskz_new,

* VAT totals per regio
  BEGIN OF tab_sum_regio OCCURS 30,
    regio           LIKE bseg-grirg,
    mwskz           LIKE bset-mwskz,
    taxed(8)        TYPE p DECIMALS 2,           " taxed
    not_taxed(8)    TYPE p DECIMALS 2,           " not taxed - Note 454626
    vat(8)          TYPE p DECIMALS 2,           " vat
    rnr_vat(8)      TYPE p DECIMALS 2,           " rnr vat
    vat_percep(8)   TYPE p DECIMALS 2,           " vat perception
    other_percep(8) TYPE p DECIMALS 2,           " other perception
    exemption(8)    TYPE p DECIMALS 2,           " exemption
    exports(8)      TYPE p DECIMALS 2,           " exports
    percepnoc(8)    TYPE p DECIMALS 2,           " perception not cat.
    line_total(8)   TYPE p DECIMALS 2,           " total_line
  END OF tab_sum_regio,

* VAT totals per für other tax code new version
  BEGIN OF tab_sum_mwskz_others OCCURS 30,
    mwskz           LIKE bset-mwskz,
    kschl           LIKE bset-kschl,         " condition type
    taxed(8)        TYPE p DECIMALS 2,           " taxed
    not_taxed(8)    TYPE p DECIMALS 2,           " not taxed
    vat(8)          TYPE p DECIMALS 2,           " vat
    rnr_vat(8)      TYPE p DECIMALS 2,           " rnr vat
    vat_percep(8)   TYPE p DECIMALS 2,           " vat perception
    other_percep(8) TYPE p DECIMALS 2,           " other perception
    exemption(8)    TYPE p DECIMALS 2,           " exemption
    exports(8)      TYPE p DECIMALS 2,           " exports
    percepnoc(8)    TYPE p DECIMALS 2,           " perception not cat.
    line_total(8)   TYPE p DECIMALS 2,           " total_line
  END OF tab_sum_mwskz_others,

* VAT totals per vat rate
  BEGIN OF tab_sum_rate OCCURS 10,
    rate(4)  TYPE n,
    hwste(8) TYPE p DECIMALS 2,
  END OF tab_sum_rate,

* VAT totals per official doument type
  BEGIN OF tab_sum_oftpt OCCURS 10,
    oftp_text       LIKE j_1aoftpt-text5,    " official doc. type
    taxed(8)        TYPE p DECIMALS 2,           " taxed
    not_taxed(8)    TYPE p DECIMALS 2,           " not taxed
    vat(8)          TYPE p DECIMALS 2,           " vat
    rnr_vat(8)      TYPE p DECIMALS 2,           " rnr vat
    vat_percep(8)   TYPE p DECIMALS 2,           " vat perception
    other_percep(8) TYPE p DECIMALS 2,           " other perception
    exemption(8)    TYPE p DECIMALS 2,           " exemption
    exports(8)      TYPE p DECIMALS 2,             " exports
    percepnoc(8)    TYPE p DECIMALS 2,          " perception not cat.
    line_total(8)   TYPE p DECIMALS 2,           " total_line
  END OF tab_sum_oftpt,

* VAT totals per account
  BEGIN OF tab_sum_account OCCURS 20,
    hkont    LIKE bseg-hkont,  " account number
    soll(8)  TYPE p DECIMALS 2,           " debit
    haben(8) TYPE p DECIMALS 2,           " credit
    saldo(8) TYPE p DECIMALS 2,           " saldo
  END OF tab_sum_account,

* Other VAT totals
  BEGIN OF tab_sum_others OCCURS 30,
    mwskz        LIKE bset-mwskz,
    kschl        LIKE bset-kschl,         " Condition type
    hwste(8)     TYPE p DECIMALS 2,
    flg_is_beleg,                      "like flg_is_beleg for SD exports
  END OF tab_sum_others,

* For magnetic output only: Branch information
* OBSOLETE with new magnetic output (introduced with note 597755)
  BEGIN OF tab_branch OCCURS 5,
    branch  LIKE bkpf-brnch,
    no_recs TYPE i,
  END OF tab_branch.

* table for history of the daily reporting
DATA: BEGIN OF tab_history OCCURS 30.
        INCLUDE STRUCTURE j_1a101.
DATA: END OF tab_history.

* work area for the historical table
DATA: BEGIN OF wa_history.
        INCLUDE STRUCTURE j_1a101.
DATA:   j_1aamnt11 TYPE j_1aamnt. " Note 685915
DATA: END OF wa_history.

* work area for the historical table (übertrag)
DATA: BEGIN OF wa_history_over.
        INCLUDE STRUCTURE j_1a101.
DATA:   j_1aamnt11 TYPE j_1aamnt. " Note 685915
DATA: END OF wa_history_over.

* text lines
DATA: BEGIN OF xtline OCCURS 10.
        INCLUDE STRUCTURE tline.
DATA: END OF xtline.
DATA: BEGIN OF tab_bkorm OCCURS 10.
        INCLUDE STRUCTURE bkorm.
DATA: END OF tab_bkorm.
DATA: text_name LIKE thead-tdname.
DATA: payer LIKE vbpa-parvw. " Note 948565


DATA: t_set_account  TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
      tg_account_set TYPE TABLE OF ty_account_set WITH HEADER LINE.


RANGES: ran_ktosl_kts1 FOR bset-ktosl. " Primary processing key

* Conditions (Procedure Data)
DATA: BEGIN OF tab_konv OCCURS 10,
        knumv LIKE konv-knumv,        " Number of the document condition
        kposn LIKE konv-kposn,         " Condition item number
        stunr LIKE konv-stunr,         " Step number
        zaehk LIKE konv-zaehk,         " Condition counter
        knumh LIKE konv-knumh,
        kinak LIKE konv-kinak,
        mwsk1 LIKE konv-mwsk1,
        kvsl1 LIKE konv-kvsl1,
        kschl LIKE konv-kschl,         " Condition type
        kbetr LIKE konv-kbetr,         " Tax rate
        kwert LIKE konv-kwert,
        kawrt LIKE konv-kawrt,
      END OF tab_konv.

*** tables and fields from 105*************************BEGIN
DATA: BEGIN OF tab_sd_bel OCCURS 2,
        belnr     LIKE bseg-belnr,
        gjahr     LIKE bseg-gjahr,
        buzei     LIKE bseg-buzei,
        disc_fact LIKE bkpf-kursf,  " SD discount perc.
      END OF tab_sd_bel.
* check if SD document reversed
DATA: BEGIN OF comwa.
        INCLUDE STRUCTURE vbco6.
DATA: END OF comwa.
DATA: BEGIN OF vbfa_tab OCCURS 10.
        INCLUDE STRUCTURE vbfa.
DATA: END OF vbfa_tab.
*check if mm document reversed
DATA BEGIN OF mm_doc OCCURS 1.
INCLUDE STRUCTURE acc_doc.
DATA END OF mm_doc.

* tax codes for manual posted taxes                "note 159314
DATA: BEGIN OF man_tax OCCURS 0, "note 159314
        mwskz LIKE bset-mwskz,                                 "note 159314
      END OF man_tax.                                          "note 159314
* vendor/customer document line data
DATA: BEGIN OF kd,
        koart LIKE bseg-koart,  " account type
        hkont LIKE bseg-lifnr,  " no.
        xcpdd LIKE bseg-xcpdd,  " cpd field
        buzei LIKE bseg-buzei,  " document line
      END OF kd,
* Amounts
      BEGIN OF amnt,
        exemption  LIKE bseg-dmbtr,  " exemption amount
        rnr_vat    LIKE bseg-dmbtr,  " rnr vat amount
        total      LIKE bseg-dmbtr,  " total amount
        line_total LIKE bseg-dmbtr,
      END OF amnt,
* Line totals per process
      BEGIN OF line_total OCCURS 5,
        mwskz LIKE bset-mwskz,
        kposn LIKE konv-kposn,
        dmbtr LIKE bseg-dmbtr,
      END OF line_total,
      BEGIN OF mm_taxed OCCURS 5,
        mwskz LIKE bseg-mwskz,
        txgrp LIKE bseg-txgrp,
        dmbtr LIKE bseg-dmbtr,
      END OF mm_taxed.
* Posted bseg lines per tax code to check if that is not a tax posting
DATA: BEGIN OF plines OCCURS 10,
        mwskz LIKE bset-mwskz,
        txgrp LIKE bseg-txgrp,
      END OF plines,
* Posted tax accounts for SD related documents
      BEGIN OF sd_hkont OCCURS 20,
        ktosl LIKE bset-ktosl,
        mwskz LIKE bset-mwskz,                              "989807
        hkont LIKE bset-hkont,
      END OF sd_hkont,
* Discount taxes
      BEGIN OF dis_taxes OCCURS 10,
        ktosl LIKE bset-ktosl,
        hwste LIKE bset-hwste,
      END OF dis_taxes,
* Discount SD reference document
      BEGIN OF sd_docm,
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr,
      END OF sd_docm,
      BEGIN OF sd_reasons OCCURS 10,
        posnr   LIKE vbrp-posnr,
        j_1arfz LIKE j_1arztx-j_1arfz,
      END OF sd_reasons.
* All tax codes containing surcharges (for correct display with PAR_SUM)
DATA: BEGIN OF tab_surcharge OCCURS 0,
        mwskz LIKE bset-mwskz,
      END OF tab_surcharge.
* Internal table for currency codes -> Needed for magnetic output
DATA: BEGIN OF tab_curr OCCURS 10,
        waers  LIKE tcurc-waers,
        altwr  LIKE tcurc-altwr,
        altkey LIKE t028m-altwr,  "alternative currency key
      END OF tab_curr.
* fields
DATA:
  dis_net_amount LIKE bseg-dmbtr,
  xumsks         TYPE c,            " Special G/L transaction type
  xvbeln         LIKE vbrk-vbeln,
  sd_reasons_set,
  xaugdt         LIKE bseg-augdt,   " first clearing date
  xaugbl         LIKE bseg-augbl,   " first clearing BELNR
  xkposn         LIKE konv-kposn,
  rcode          LIKE sy-subrc,     " return code
  xmbelnr        LIKE rbkp-belnr,
  xmgjahr        LIKE rbkp-gjahr,
  xmgjahr2       LIKE rbkp-stblg,
  not_dialog     LIKE boole,
  sd_vbeln       LIKE vbrk-vbeln,
  sd_disc_fact   LIKE bkpf-kursf,   " SD discount perc.
  xkursf         LIKE bkpf-kursf.
*** tables and fields from 105*************************END

* Note 789418 Starts
DATA: g_surc_disc TYPE c,
      g_sd_amt    LIKE bseg-dmbtr.
* Note 789418 Ends

* Begin of change 988302
DATA: find_exempted TYPE c,
      rate          TYPE i.
*  End of change 988302
DATA: t_bset TYPE STANDARD TABLE OF bset,
      t_bkpf TYPE STANDARD TABLE OF bkpf,
      t_bseg TYPE STANDARD TABLE OF bseg,

*----------------------------------------------------------------------*
* Field groups                                                         *
*----------------------------------------------------------------------*

* Feldleiste mit Einzelposten-Informationen
      ep     TYPE type_ep.

*----------------------------------------------------------------------*
* Fields                                                               *
*----------------------------------------------------------------------*
DATA:
* Printing lines, titles
  txt_gjahr(7)             TYPE c,              " Fiscal year for list title
  txt_zeile1(245)          TYPE c,              " Title line 1
  txt_zeile2(245)          TYPE c,              " Title line 2
  txt_zeile3(245)          TYPE c,              " Title line 3
  txt_zeile4(245)          TYPE c,              " Title line 4
  txt_zeile5(245)          TYPE c,              " Title line 5
  txt_zeile6(245)          TYPE c,              " Title line 6
  txt_zeile7(245)          TYPE c,              " Document line 1
  txt_zeile8(245)          TYPE c,              " Document line 2
  txt_zeile9(245)          TYPE c,              " Line sums

* Further fields
  hlp_cnt                  TYPE i,       " Counter for tab_bseg
  rcode_bkpf               LIKE sy-subrc, " return code customer exit001
  rcode_bseg               LIKE sy-subrc, " return code customer exit002
  flg_reject_doc(1)        TYPE c,       " reject this document
  flg_sd_beleg(1)          TYPE c,       " SD beleg
  flg_is_beleg             TYPE c,       " IS Media Document
  list_number              TYPE i,       " list number
  tmp_cnt1                 TYPE p,       " zaehler f. tmp prüfung
  tmp_cnt2                 TYPE p,       " zaehler f. tmp prüfung
  tmp_length               TYPE i,
  flg_new_page(1)          TYPE c,       " new-page accoured
  flg_checking_ktsol(1)    TYPE c,
  flg_print_tab_history(1) TYPE c,
  flg_not_used_taxid       TYPE c,
  history_repid            LIKE j_1a101-repid,
  from_date                TYPE d,       " selection date
  to_date                  TYPE d,       " selection date
  listsize                 TYPE i.

DATA:
* Hilfsstrukturen
  BEGIN OF hlp_repid,                  "structure for report name
    repid LIKE sy-repid,
    v1(1) TYPE c,
  END OF hlp_repid,
* Hilfsstrukturen for tax rate
  BEGIN OF hlp_rate,                   "structure for report name
    rate(6)    TYPE c,
    percent(1) TYPE c,
  END OF hlp_rate,

*----------------------------------------------------------------------*
* Structures for list output                                           *
*----------------------------------------------------------------------*
* Length of hlp_epos1 must be 245
  BEGIN OF hlp_epos1,    "print structure for line 1 items  ( ep )
    v1(1)     TYPE c,
    lineno(6) TYPE c,
    v2(1)     TYPE c,
    budat(8)  TYPE c,
    v3(1)     TYPE c,
    brnch     LIKE bkpf-brnch,
    v4(1)     TYPE c,
    belnr(10) TYPE c,
    v5(1)     TYPE c,
    koart     LIKE bseg-koart,
    v6(1)     TYPE c,
    hkont(10) TYPE c,
    v7(1)     TYPE c,
    name1     LIKE lfa1-name1,
    v8(1)     TYPE c,
    stcdt     LIKE lfa1-stcdt,
    v9(1)     TYPE c,
    stcd1     LIKE lfa1-stcd1,
    v10(1)    TYPE c,
    bldat(8)  TYPE c,
    v11(1)    TYPE c,
    xblnr(14) TYPE c,
    v12(1)    TYPE c,
    oftp_text LIKE j_1aoftpt-text5,
    v13(1)    TYPE c,
    augdt(8)  TYPE c,
    v14(1)    TYPE c,
    augbl     LIKE bseg-augbl,
    v15(1)    TYPE c,
    total(57) TYPE c,
    v16(1)    TYPE c,
    cai(14)   TYPE c, "For magn. output: Print Authorization Code
    fisc_cont TYPE c, "For magn. output:Fiscal Contrlr " RG1361
  END OF hlp_epos1,

  BEGIN OF hlp_epos1s,    "print structure for line 1 items storno (ep)
    v1(1)     TYPE c,
    lineno(6) TYPE c,
    v2(1)     TYPE c,
    text1(13) TYPE c,
    v3(1)     TYPE c,
    belnr(10) TYPE c,
    v4(1)     TYPE c,
    text2(20) TYPE c,
    stblg     LIKE bkpf-stblg,
    v5(1)     TYPE c,
    text3(29) TYPE c,
    v6(1)     TYPE c,
    xblnr(14) TYPE c,
    v7(1)     TYPE c,
    text4(15) TYPE c,           "  document type
    v8(1)     TYPE c,
    oftp_text LIKE j_1aoftpt-text5,
    dummy1(1) TYPE c,
    v9(1)     TYPE c,
    ltext     LIKE t003t-ltext,
    v10(1)    TYPE c,
  END OF hlp_epos1s,

* length of hlp_epos2 must be 245
  BEGIN OF hlp_epos2,      "print structure for line 2 items  ( ep )
    v1(1)            TYPE c,
    mwskz(6)         TYPE c,
    v2(1)            TYPE c,
    rate(8)          TYPE c,
    v3(1)            TYPE c,
    taxed(18)        TYPE c,
    v4(1)            TYPE c,
    not_taxed(18)    TYPE c,
    v5(1)            TYPE c,
    vat(19)          TYPE c,
    v6(1)            TYPE c,
    rnr_vat(18)      TYPE c,
    v7(1)            TYPE c,
    vat_percep(18)   TYPE c,
    v8(1)            TYPE c,
    other_percep(18) TYPE c,
    v9(1)            TYPE c,
    exemption(19)    TYPE c,
    v10(1)           TYPE c,
    exports(18)      TYPE c,
    v11(1)           TYPE c,
    percepnoc(18)    TYPE c,
    v12(1)           TYPE c,
    line_total(19)   TYPE c,
    v13(1)           TYPE c,
    exempt(16)       TYPE c, " Exempt reason for magnetic output
    earn_per(18)     TYPE c, " Note 645449
  END OF hlp_epos2,
* length of hlp_epos3 must be 245
  BEGIN OF hlp_epos3,                  "print structure for sum line
    v1(1)            TYPE c,
    txt_sum(15)      TYPE c,
    v3(1)            TYPE c,
    taxed(18)        TYPE c,
    v4(1)            TYPE c,
    not_taxed(18)    TYPE c,
    v5(1)            TYPE c,
    vat(19)          TYPE c,
    v6(1)            TYPE c,
    rnr_vat(18)      TYPE c,
    v7(1)            TYPE c,
    vat_percep(18)   TYPE c,
    v8(1)            TYPE c,
    other_percep(18) TYPE c,
    v9(1)            TYPE c,
    exemption(19)    TYPE c,
    v10(1)           TYPE c,
    exports(18)      TYPE c,
    v11(1)           TYPE c,
    percepnoc(18)    TYPE c,
    v12(1)           TYPE c,
    line_total(18)   TYPE c,
    v13(1)           TYPE c,
    filler(17)       TYPE c,                              " Note 685915
    earn_per(18)     TYPE c,                              " Note 685915
    v14(1)           TYPE c,                              " Note 685915
  END OF hlp_epos3,

  BEGIN OF hlp_epos3a  ,               "print structure for sum line
    v1(1)            TYPE c,
    txt_sum(12)      TYPE c,
    v2(1)            TYPE c,
    description(40)  TYPE c,
    v4(1)            TYPE c,
    taxed(18)        TYPE c,
    v5(1)            TYPE c,
    not_taxed(18)    TYPE c,                              " Note 454626
    v5a(1)           TYPE c,                              " Note 454626
    vat(18)          TYPE c,
    v6(1)            TYPE c,
    rnr_vat(18)      TYPE c,
    v7(1)            TYPE c,
    vat_percep(18)   TYPE c,
    v8(1)            TYPE c,
    other_percep(18) TYPE c,
    v9(1)            TYPE c,
    exemption(18)    TYPE c,
    v10(1)           TYPE c,
    exports(18)      TYPE c,
    v11(1)           TYPE c,
    percepnoc(18)    TYPE c,
    v12(1)           TYPE c,
    line_total(18)   TYPE c,
    v13(1)           TYPE c,
  END OF hlp_epos3a,

  BEGIN OF hlp_epos3b  ,               "print structure for sum regio
    v0(1)            TYPE c,
    regio(6)         TYPE c,
    v1(1)            TYPE c,
    txt_sum(5)       TYPE c,
    v2(1)            TYPE c,
    description(40)  TYPE c,
    v4(1)            TYPE c,
    taxed(18)        TYPE c,
    v5(1)            TYPE c,
    not_taxed(18)    TYPE c,                              " Note 454626
    v5a(1)           TYPE c,                              " Note 454626
    vat(18)          TYPE c,
    v6(1)            TYPE c,
    rnr_vat(18)      TYPE c,
    v7(1)            TYPE c,
    vat_percep(18)   TYPE c,
    v8(1)            TYPE c,
    other_percep(18) TYPE c,
    v9(1)            TYPE c,
    exemption(18)    TYPE c,
    v10(1)           TYPE c,
    exports(18)      TYPE c,
    v11(1)           TYPE c,
    percepnoc(18)    TYPE c,
    v12(1)           TYPE c,
    line_total(18)   TYPE c,
    v13(1)           TYPE c,
  END OF hlp_epos3b,

  BEGIN OF hlp_epos4,      "print structure for totals by tax code
    v1(1)       TYPE c,
    mwskz(15)   TYPE c,
    v2(1)       TYPE c,
    description LIKE t007s-text1,
    v3(1)       TYPE c,
    hwste(18)   TYPE c,
    v4(1)       TYPE c,
  END OF hlp_epos4,

  BEGIN OF hlp_epos5,      "print structure for totals by tax rate
    v1(1)     TYPE c,
    satz(15)  TYPE c,
    v2(1)     TYPE c,
    hwste(18) TYPE c,
    v3(1)     TYPE c,
  END OF hlp_epos5,

  BEGIN OF hlp_epos6,      "print structure for totals by tax rate
    v1(1)       TYPE c,
    hkont       LIKE bseg-hkont,  " account number
    v2(1)       TYPE c,
    description LIKE skat-txt20,
    v3(1)       TYPE c,
    soll(18)    TYPE c,
    v4(1)       TYPE c,
    haben(18)   TYPE c,
    v5(1)       TYPE c,
    saldo(18)   TYPE c,
    v6(1)       TYPE c,
  END OF hlp_epos6,

  BEGIN OF hlp_epos7,                  "print structure for log entry 1
    v1(1)           TYPE c,
    name(20)        TYPE c,           " name of table
    v2(1)           TYPE c,
    key(50)         TYPE c,           " key of table
    v3(1)           TYPE c,
    description(50) TYPE c,
    v4(1)           TYPE c,
    belnr(20)       TYPE c,           " reference docnr as example
    v5(1)           TYPE c,
  END OF hlp_epos7,

  BEGIN OF hlp_epos8,                  "print structure for log entry 2
    v1(1)           TYPE c,
    belnr           LIKE bkpf-belnr,  " document no.
    v2(1)           TYPE c,
    blart           LIKE bkpf-blart,  " document type
    v3(1)           TYPE c,
    budat(8)        TYPE c,
    v4(1)           TYPE c,
    description(50) TYPE c,
    v5(1)           TYPE c,
  END OF hlp_epos8,

  BEGIN OF hlp_epos9,    "print structure for line 1 items storno ( ep )
    v1(1)     TYPE c,
    text1(20) TYPE c,
    code(3)   TYPE n,
    v2(1)     TYPE c,
    text2(20) TYPE c,
    number(8) TYPE n,
    v3(1)     TYPE c,
    text3(20) TYPE c,
    destcd(4) TYPE c,  " Destination code
    v4(1)     TYPE c,
    text4(20) TYPE c,
    chksum(1) TYPE c,  " Shipment number check-sum
    v5(1)     TYPE c,
  END OF hlp_epos9,

* Structure for magnetic output: Print number of records per branch
  BEGIN OF hlp_epos10,  "print branch and number of printed records
    v1(1)       TYPE c,
    branch(20)  TYPE c,
    v2(2)       TYPE c,
    no_recs(20) TYPE n,
    v3(1)       TYPE c,
  END OF hlp_epos10,

  BEGIN OF hlp_epos11,  "print record of Perception File - magn. output
    v1(1)          TYPE c,
    regio_txt(40)  TYPE c,  " Text: 'Regional code'
    v2(1)          TYPE c,
    regio_code(2)  TYPE n,  " regional code
    v3(1)          TYPE c,
    regio_perc(18) TYPE c,
    v4(1)          TYPE c,
    munic_txt(40)  TYPE c,  " Text: 'Municipal code'
    v5(1)          TYPE c,
    munic_code(40) TYPE c,
    v6(1)          TYPE c,
    munic_perc(18) TYPE c,
    v7(1)          TYPE c,
  END OF hlp_epos11.

*----------------------------------------------------------------------*
* Structures for magnetic output: Taken from J_1AF106                  *
*----------------------------------------------------------------------*
DATA:
*  Record Type 1 for the Purchase File - Length 369
  BEGIN OF i_record2,
    rectype(1)       TYPE n,           " record type = 1
    docu_date(8)     TYPE n,           " document date yyyymmdd
    docu_type(2)     TYPE c,           " document type
    fisc_cont        TYPE c,           " Fiscal controller
    subsidiary(4)    TYPE n,           " subsidiary/branch
    docu_no(20)      TYPE n,           " official document no.
    post_date(8)     TYPE n,           " posting date yyyymmdd
    customs_code(3)  TYPE n,           " customs codease no.
    customs_dest(4)  TYPE c,           " customs destination code
    customs_no(6)    TYPE n,           " customs shipment no.
    customs_check(1) TYPE c,           " customs shipm. number check-sum
    type_of_id(2)    TYPE n,           " type of id
    id_docu_no(11)   TYPE n,           " cuit or id
    name(30)         TYPE c,           " vend./cust. name.)
    total_amnt(15)   TYPE n,           " total amount
    n_taxed_amnt(15) TYPE n,           " not taxed
    taxed_amnt(15)   TYPE n,           " taxedption
    tax_rate(4)      TYPE n,           " VAT tax rate (2 deci.)
    tax_amnt(15)     TYPE n,           " taxhange rt(4 deci.)
    exempt_amnt(15)  TYPE n,           " exemptionat rates
    perce_amnt(15)   TYPE n,           " national perception
    per_nat_amnt(15) TYPE n,           " other national perception
    perc_gi_amnt(15) TYPE n,           " gross income perception
    perc_mp_amnt(15) TYPE n,           " municipal perception
    int_tax_amnt(15) TYPE n,           " internal tax
    fityp(2)         TYPE n,           " fiscal type/category
    curr_code(3)     TYPE c,           " currency code
    exch_rate(10)    TYPE n,           " exchange rt(4 deci.)
    sev_vat_rates    TYPE c,           " number of vat rates
    reason_0_tax     TYPE c,           " reason for zero tax
    cai(14)          TYPE n,           " print authorization code (CAI)
    due_date(8)      TYPE n,           " due date
    filler(75)       TYPE n,           " filling field - zeros
  END OF i_record2.
* Record Type 2 for the Purchase File - Length 369
DATA:
  BEGIN OF i_record3,
    rectype(1)       TYPE n,           " record type = 2
    period(6)        TYPE n,           " yyyymm
    filler1(10)      TYPE c,           " filling field
    no_records(12)   TYPE n,           " no of type 1 records
    filler2(31)      TYPE c,           " filling field
    id_docu_no(11)   TYPE n,           " cuit or id
    filler3(30)      TYPE c,           " filling field
    total_amnt(15)   TYPE n,           " total amount
    n_taxed_amnt(15) TYPE n,           " not taxed
    taxed_amnt(15)   TYPE n,           " taxed
    filler4(4)       TYPE c,           " filling field
    tax_amnt(15)     TYPE n,           " tax
    exempt_amnt(15)  TYPE n,           " exemption
    perce_amnt(15)   TYPE n,           " national perception
    per_nat_amnt(15) TYPE n,           " other national perception
    perc_gi_amnt(15) TYPE n,           " gross income perception
    perc_mp_amnt(15) TYPE n,           " municipal perception
    int_tax_amnt(15) TYPE n,           " internal tax
    filler5(114)     TYPE c,           " filling field - blanks
  END OF i_record3.

* Structure for the Purchase File Summation
DATA:
  BEGIN OF i_record3_sum,
    total_amnt(8)   TYPE p DECIMALS 2,           " total amount
    n_taxed_amnt(8) TYPE p DECIMALS 2,           " not taxed
    taxed_amnt(8)   TYPE p DECIMALS 2,           " taxed
    tax_amnt(8)     TYPE p DECIMALS 2,           " tax
    exempt_amnt(8)  TYPE p DECIMALS 2,           " exemption
    perce_amnt(8)   TYPE p DECIMALS 2,           " national perception
    per_nat_amnt(8) TYPE p DECIMALS 2,           " other national perception
    "Note  728727
    perc_gi_amnt(8) TYPE p DECIMALS 2,           " gross income perception
    perc_mp_amnt(8) TYPE p DECIMALS 2,           " municipal perception
    int_tax_amnt(8) TYPE p DECIMALS 2,           " internal tax
  END OF i_record3_sum.

* Record Type 1 for the Sales File - Length 375
DATA:
  BEGIN OF o_record2,
    rectype(1)       TYPE n,           " record type = 1
    docu_date(8)     TYPE n,           " document date yyyymmdd
    docu_type(2)     TYPE c,           " document type
    fisc_cont        TYPE c,           " Fiscal controller
    subsidiary(4)    TYPE n,           " subsidiary/branch
    docu_no(20)      TYPE n,           " official document no.
    last_docu_no(20) TYPE n,           " last doc. no. (same as DOCU_NO)
    type_of_id(2)    TYPE n,           " type of id
    id_docu_no(11)   TYPE n,           " cuit or id
    name(30)         TYPE c,           " vend./cust. name
    total_amnt(15)   TYPE n,           " total amount
    n_taxed_amnt(15) TYPE n,           " not taxed
    taxed_amnt(15)   TYPE n,           " taxed
    tax_rate(4)      TYPE n,           " VAT tax rate (2 deci.)
    tax_amnt(15)     TYPE n,           " tax
    surch_amnt(15)   TYPE n,           " Surcharge (equation)
    exempt_amnt(15)  TYPE n,           " exemption
    perce_amnt(15)   TYPE n,           " national perception
    perc_gi_amnt(15) TYPE n,           " gross income perception
    perc_mp_amnt(15) TYPE n,           " municipal perception
    int_tax_amnt(15) TYPE n,           " internal tax
    fityp(2)         TYPE n,           " fiscal type/category
    curr_code(3)     TYPE c,           " currency code
    exch_rate(10)    TYPE n,           " exchange rt(4 deci.)
    sev_vat_rates    TYPE c,           " number of vat rates
    reason_0_tax     TYPE c,           " reason for zero tax
    cai(14)          TYPE n,           " print authorization code (CAI)
    due_date(8)      TYPE n,           " due date
    canc_date(8)     TYPE n,           " cancellation date
    filler(75)       TYPE n,           " filling field - zeros
  END OF o_record2,

* Record Type 2 for the Sales File - Length 375
  BEGIN OF o_record3,
    rectype(1)       TYPE n,           " record type = 2
    period(6)        TYPE n,           " yyyymm
    filler1(29)      TYPE c,           " filling field
    no_records(12)   TYPE n,           " no of type 1 records
    filler2(10)      TYPE c,           " filling field
    id_docu_no(11)   TYPE n,           " cuit or id
    filler3(30)      TYPE c,           " filling field
    total_amnt(15)   TYPE n,           " total amount
    n_taxed_amnt(15) TYPE n,           " not taxed
    taxed_amnt(15)   TYPE n,           " taxed
    filler4(4)       TYPE c,           " filling field
    tax_amnt(15)     TYPE n,           " tax
    surch_amnt(15)   TYPE n,           " Surcharge (equation)
    exempt_amnt(15)  TYPE n,           " exemption
    perce_amnt(15)   TYPE n,           " national perception
    perc_gi_amnt(15) TYPE n,           " gross income perception
    perc_mp_amnt(15) TYPE n,           " municipal perception
    int_tax_amnt(15) TYPE n,           " internal tax
    filler5(122)     TYPE c,           " filling field - blanks
  END OF o_record3.

* Structure for the Sales File Summation
DATA:
  BEGIN OF o_record3_sum,
    total_amnt(8)   TYPE p DECIMALS 2,           " total amount
    n_taxed_amnt(8) TYPE p DECIMALS 2,           " not taxed
    taxed_amnt(8)   TYPE p DECIMALS 2,           " taxed
    tax_amnt(8)     TYPE p DECIMALS 2,           " tax
    surch_amnt(8)   TYPE p DECIMALS 2,           " Surcharge (equation)
    exempt_amnt(8)  TYPE p DECIMALS 2,           " exemption
    perce_amnt(8)   TYPE p DECIMALS 2,           " national perception
    perc_gi_amnt(8) TYPE p DECIMALS 2,           " gross income perception
    perc_mp_amnt(8) TYPE p DECIMALS 2,           " municipal perception
    int_tax_amnt(8) TYPE p DECIMALS 2,           " internal tax
  END OF o_record3_sum.

* Record for the Perceptions File - Length 94
DATA: BEGIN OF tab_p_record2 OCCURS 0.
        INCLUDE TYPE type_p_record2.
DATA: END OF tab_p_record2.
DATA: p_record2 TYPE type_p_record2.

DATA: tab_i_record TYPE TABLE OF type_i_record, " For Purchases
      wa_i_record  TYPE type_i_record,            "     File - local
      tab_o_record TYPE TABLE OF type_o_record,   " For Sales
      wa_o_record  TYPE type_o_record,            "     File - local
      tab_p_record TYPE TABLE OF type_p_record,   " For Perceptions
      wa_p_record  TYPE type_p_record.            "     File - local

*----------------------------------------------------------------------*
* Fields for Magnetic Output                                           *
*----------------------------------------------------------------------*
DATA: no_of_rec      TYPE i, " records of type
      no_of_rates    TYPE i,           " per doc.: Number of VAT rates
      rec_subsidiary TYPE i,           " records of subsidiary
      tab_ep_lines   LIKE sy-tfill,    " Number of records per doc
      gf_cai(14)     TYPE n,           " Print Authorization Code (CAI)
      gf_fisc_cont   TYPE c,           " Fiscal Controller
      gf_due_date    TYPE j_1apacd-j_1apacvd, " CAI Due date
      gf_exch_rate   TYPE p DECIMALS 6, " Exchange rate
      gf_rec3_buper  TYPE sy-datum,    " First date of fiscal period
      gf_poper       TYPE t009b-poper, " Posting period/fiscal period
      gf_land1       TYPE t005-land1,  " Country
      hlp_col9       LIKE p_col09,         " On change of radiobutton
      hlp_col9m      LIKE p_col09,
      hlp_col10      LIKE p_col10,         " remember texts on sel.screen
      hlp_col10m     LIKE p_col10,
      flg_change_sel,                  " Selection screen changed
      gv_tabix       LIKE sy-tabix.          " table index for performance

* Currency class - determination of currency keys for Magnetic output
CONSTANTS: c_currkey LIKE t028m-currkey VALUE 'DAILYVAT'.
DATA: land1 LIKE t001-land1.
DATA: len TYPE i. " length of the file 1037893




DATA: wa_p_record2 LIKE LINE OF tab_p_record2.




*----------------------------------------------------------------------*
* Field-Groups                                                         *
*----------------------------------------------------------------------*
FIELD-GROUPS:
  header,
  daten.

INSERT
  ep-bukrs                             " Company code
  ep-budat                             " Posting date
  ep-bldat                             " Document date
  ep-buper                             " Postingperiod
  ep-brnch                             " Branch
  ep-belnr                             " Document number
  ep-xblnr                             " Official document number
  ep-cpudt
  ep-cputm
  ep-j_1aoftp
  ep-oftp_text                         " Official document type text
  ep-mwskz                             " Tax code
  ep-blart
  ep-flg_sd_beleg                      " Flag: SD document
  ep-sd_vbeln                          " SD document number
  ep-flg_is_beleg                      " IS-M/AM Document
INTO header.

INSERT ep INTO daten.

DATA : rev_monat LIKE bkpf-monat.                           "1069879

*[BEGIN] -- PM -- 18/08/09
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gs_layout   TYPE slis_layout_alv,      " Estructura de salida
      it_header   TYPE slis_t_listheader,    " Tabla con el Titulo
      t_events    TYPE slis_t_event,
      t_sort      TYPE slis_t_sortinfo_alv.  " Tabla de ordenamiento

DATA: BEGIN OF t_iva OCCURS 0,
        descrip TYPE char30,
        rate(8) TYPE c,
        ivat    TYPE p DECIMALS 2,
      END OF  t_iva.

DATA: BEGIN OF t_bases OCCURS 0,
        descrip    TYPE char30,
        rate(8)    TYPE c,
        ivat       TYPE p DECIMALS 2,
        itaxed     TYPE p DECIMALS 2,
        inot_taxed TYPE p DECIMALS 2,
        iexemption TYPE p DECIMALS 2,
      END OF  t_bases.

DATA: BEGIN OF t_operaciones OCCURS 0,
        j_1afitp  TYPE j_1afitp_d,
        descrip   TYPE char30,
        rate      TYPE char8,
        imp_grav  TYPE p DECIMALS 2,
        imp_ngrav TYPE p DECIMALS 2,
        imp_exent TYPE p DECIMALS 2,
      END OF  t_operaciones.

DATA: BEGIN OF t_tipo_doc OCCURS 0,
        blart      TYPE blart,
        doccls     TYPE doccls,
        j_1aprtchr TYPE j_1apchar,
        j_1aoftp   TYPE j_1aoftp_d,
        text5      TYPE text5,
      END OF  t_tipo_doc.

DATA: BEGIN OF st_totales,
        lineno(6)        TYPE c,
        budat(10)        TYPE c,
        gjahr            TYPE bkpf-gjahr,
        brnch            LIKE bkpf-brnch,
        belnr(10)        TYPE c,
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
        mwskz(6)         TYPE c,
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
        flag             TYPE char1,
      END OF  st_totales.

DATA: BEGIN OF t_alv OCCURS 0,
        lineno(6)        TYPE c,
        budat(10)        TYPE c,
        gjahr            TYPE bkpf-gjahr,
        brnch            LIKE bkpf-brnch,
        belnr(10)        TYPE c,
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
        mwskz            TYPE bset-mwskz,
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
        flag             TYPE char1,
        kschl            TYPE bset-kschl,
        ktosl            TYPE bset-ktosl,
        fityp            TYPE lfa1-fityp,
        tipo_doc         TYPE text5,
        cai              TYPE char22,
        fecha_cai        TYPE char10,
      END OF t_alv.

*tables lfa1.
DATA: BEGIN OF t_alv_indica OCCURS 0,
        indica(2),
        descri    TYPE t007s-text1,
        importe   TYPE p DECIMALS 2,
      END OF t_alv_indica.

DATA: BEGIN OF t_alv_cta OCCURS 0,
        hkont(10),
        descri    TYPE t007s-text1,
        debe      TYPE p DECIMALS 2,
        haber     TYPE p DECIMALS 2,
        saldo     TYPE p DECIMALS 2,
      END OF t_alv_cta.

DATA: BEGIN OF t_alv_porc OCCURS 0,
        porc(10),
        impor    TYPE p DECIMALS 2,
      END OF t_alv_porc.

DATA: BEGIN OF t_a003 OCCURS 0,
        mwskz TYPE a003-mwskz,
        knumh TYPE a003-knumh,
      END OF t_a003.

DATA: BEGIN OF t_konp OCCURS 0,
        knumh TYPE konp-knumh,
        kbetr TYPE konp-kbetr,
      END OF t_konp.

DATA: BEGIN OF t_periva OCCURS 0,
        hkont   TYPE hkont,
        descrip TYPE char50,
        kbetr   TYPE char8,
        imp     TYPE p DECIMALS 2,
      END OF t_periva.

DATA: BEGIN OF t_periibb OCCURS 0,
        hkont   TYPE hkont,
        libro   TYPE char1,
        bland   TYPE regio,
        descrip TYPE char50,
        imp     TYPE p DECIMALS 2,
        bezei   TYPE bezei20,
      END OF t_periibb.


DATA: t_kna1      TYPE STANDARD TABLE OF kna1,
      t_lfa1      TYPE STANDARD TABLE OF lfa1,
      t_bsas      TYPE STANDARD TABLE OF bsas WITH HEADER LINE,
      t_bsis      TYPE STANDARD TABLE OF bsis WITH HEADER LINE,
      vl_belnr    TYPE belnr_d,
      v_ucomm     TYPE sy-ucomm,
      layout      TYPE slis_layout_alv,
      t_fieltotal TYPE slis_t_fieldcat_alv,
      v_ssfctrlop TYPE ssfctrlop,
      v_ssfcompop TYPE ssfcompop.


***************************
DATA: gi_sort  TYPE  slis_t_sortinfo_alv,
      gi_event TYPE slis_t_event.
***************************

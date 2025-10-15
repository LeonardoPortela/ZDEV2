*&---------------------------------------------------------------------*
*& Report: J_1AF205                                                    *
*& Daily VAT Reporting for Argentina                                   *
*& This report will be used instead the old report J_1AF105            *
*&---------------------------------------------------------------------*
*& Date: 19.01.2001                                                    *
*& For general information: See note 327003                            *
*& Corrections:                                                        *
*& 17.04.2001: Add magnetic output - functionality of old J_1AF106     *
*&                                                                     *
*& 6.3.2003: Start adding functionality for Res. 1361/2002             *
*&           See note 597755                                           *
*&                                                                     *
*&                                                                     *
*& Change History:                                                     *
*& Note 712000 - Added POSNR as key in READ of table LOCAL_RATE.This   *
*&               was done so that if same rate is present in different *
*&               lines they are displayed in the report.Msg no 124268. *
*& Note 712973 - Extended the IF clause to include check on POSNR.This *
*&               was done because when Display Tax base for Man tax    *
*&               flag was selected for SD documents,the report was     *
*&               displaying wrong tax bases.Msg No 194134 2004         *
*& Note 727028 - Taxed column to be zero if there is only perceptions  *
*&               without any VAT amount.This amount to appear in       *
*&               Exemption column.Msg no 97613 2004                    *
*& Note 728727 - In file output the total for earnings perception was  *
*&               coming as zero.Added code to populate correct amount  *
*&               Msg no 804151 2004.                                   *
*& Note 737661 - In file o/p the total of perception not categorized   *
*&               coming as zero.Added code to populate correct amount  *
*&               Msg no 327820 2004.                                   *
*& Note 744713 - For documents posted with tax code having two account *
*&               keys where in which for one account key tax type is   *
*&               of type 1,2 or 3 while for the other account key      *
*&               tax type is maintained as 4 (not relevent for tax),   *
*&               the report is showing wrong values in list output.    *
*&               Msg No 201259 2004.                                   *
*& Note 761049 - This is only for rel 4.7 and above.The format of      *
*&               perception file was not as per govt.It was considering*
*&               internal table P_RECORD in place of P_RECORD2         *
*&               Msg No 654051 2004.                                   *
*& Note 766634 - The table TAB_BELEG was enhanced earlier to include   *
*&               new amounts, but the form CHECK_SD_FOREIGN_CURRENCY   *
*&               contained code for old version of TAB_BELEG.Changed   *
*&               this take care of extended TAB_BELEG.Msg No 446983 2004
*& Note 782267 - For Tax postings that are done manually in MM(Using GL*
*&               Account tab) the Daily VAT report (J_1AF205) does not *
*&               give correct output.The report displays the zero VAT  *
*&               lines by consolidating the lines.Some corrections of  *
*&               note 724506 was wrong. Msg no 266001 2004             *
*& Note 789418 - Incases of SD documents posted with tax code having a *
*&               class of type 'A' (Surcharge/Discount) the report     *
*&               gives wrong base amt output. Msg 1165596 2004         *
*& Note 795638 - The report J_1AF205 (Daily VAT Report) was reporting *
*&               documents which are down payments in nature in both  *
*&               purchase and sales file. But legally it should appear*
*&               in either purchase or sales file.Msg 375228 2004      *
*&                                                                     *
*& Note 804870 - The totals printed by the report for every page is    *
*&               wrong. This was because it was adding the amounts of *
*&               next record and then checked for availablity of      *
*&               lines and then print the record and the total        *
*&               Hence total contains value of the record that was    *
*&               printed as part of next page.                         *
*& Note 805475 - As per the Argentine Resolution 1361, companies using
*&               fiscal controller to issue documents should report the
*&               Print Authorisation Code (PAC) along with the letter *
*&               'C' in the Daily VAT Report J_1AF205. Before this    *
*&               resolution these companies had to report only the    *
*&               letter 'C' in the VAT Report.                        *
*&               Use key '1361' to search for the changes in the program
*& Note 926185 - Performance enhancement in loop at TAB_REGIO          *
*& Note 932719 - Note 804870 introduced the error that not all accounts*
*&               appear in the total sum per account. This has happened*
*&               because at the point where the perform sum is called  *
*&               after the changes of note 804870, not all relevant    *
*&               values necessary for the total sum are available      *
*&               anymore (after perform compres_tab_ep).This note takes*
*&               only the part related to the sum  per account back to *
*&               the old logic.                                        *
*& Note 992893 - The other tax totals summarized the taxes and         *
*&               displayed it in a single line when the report is      *
*&               esecuted in the compressed mode. But it has to display*
*&               the same way as in uncompressed mode.                 *
*&---------------------------------------------------------------------*

REPORT  j_1af205  NO STANDARD PAGE HEADING
              LINE-SIZE 355
              LINE-COUNT 57(3)
              MESSAGE-ID 8a.

INCLUDE z_j_1af205_top.

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
  sarkey,                      "Archieved documents  1069346
  bhdgd.

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

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-s02.
  SELECT-OPTIONS:  s_blart      FOR  bkpf-blart.            "1069346
  SELECT-OPTIONS:  s_bldat      FOR  bkpf-bldat,
                   s_taxcd      FOR  bseg-mwskz,
                   s_lifnr      FOR  bseg-lifnr.
  PARAMETERS: par_cust TYPE idfiar205-fiar205acctp,
              par_vend TYPE idfiar205-fiar205acctp.
* Processing keys: sel_kts1: Primary processing key
*                  sel_kts2: Secondary processing key
  SELECT-OPTIONS:  sel_kts1     FOR idfiar205-prkeyprin,
                   sel_kts2     FOR idfiar205-prkeyprin.
SELECTION-SCREEN END OF BLOCK 2.

SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-s05.

  PARAMETERS: par_vers LIKE idfiar205-compressflag NO-DISPLAY,
              s_drver  LIKE j_1adrver-j_1adrver OBLIGATORY,
              s_event  LIKE idfiar205-fiarcorrtyp
                                        DEFAULT 'SAPA1'.
*SELECTION-SCREEN ULINE.
*SELECTION-SCREEN COMMENT /1(79) text-c01.
  PARAMETERS:      p_cai        LIKE idfiar205-fiarcorrtyp.
  PARAMETERS:      p_bktxt      LIKE idfiar205-fiarpacflag.
SELECTION-SCREEN END OF BLOCK 3.

SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE TEXT-s04.
  PARAMETERS: s_delete LIKE idfiar205-histtabdel,
              par_updh LIKE idfiar205-histtabupd.
SELECTION-SCREEN END OF BLOCK 4.

SELECTION-SCREEN BEGIN OF BLOCK 5 WITH FRAME TITLE TEXT-s06.
* Default: Daily VAT report is run
  PARAMETERS:      par_magn LIKE idfiar205-fiar205radio.
  SELECTION-SCREEN BEGIN OF BLOCK 51 WITH FRAME TITLE TEXT-s07.
    PARAMETERS: par_file     LIKE rfpdo1-allgunix,
                par_loc      LIKE idfiar205-fiarlocfile,
                par_lfil(70).
*                Create perception file
  SELECTION-SCREEN END OF BLOCK 51.
  SELECTION-SCREEN BEGIN OF BLOCK 52 WITH FRAME TITLE TEXT-s08.
    PARAMETERS: par_perf     LIKE rfpdo1-allgunix,
                par_ploc     LIKE idfiar205-fiarlocfile,
                par_pfil(70).
  SELECTION-SCREEN END OF BLOCK 52.
  PARAMETERS:      par_dele LIKE idfiar205-fiardelfile.

  SELECTION-SCREEN BEGIN OF BLOCK 53 WITH FRAME TITLE TEXT-s12.
    PARAMETERS: pa_vd       LIKE idfiar205-fiar205radio,
                p_ph_vd(70).
  SELECTION-SCREEN END OF BLOCK 53.

  SELECTION-SCREEN BEGIN OF BLOCK 54 WITH FRAME TITLE TEXT-s13.
    PARAMETERS: pa_vda       LIKE idfiar205-fiar205radio,
                p_ph_vda(70).
  SELECTION-SCREEN END OF BLOCK 54.

  SELECTION-SCREEN BEGIN OF BLOCK 55 WITH FRAME TITLE TEXT-s14.
    PARAMETERS: pa_cp       LIKE idfiar205-fiar205radio,
                p_ph_cp(70).
  SELECTION-SCREEN END OF BLOCK 55.

  SELECTION-SCREEN BEGIN OF BLOCK 56 WITH FRAME TITLE TEXT-s15.
    PARAMETERS: pa_cpa       LIKE idfiar205-fiar205radio,
                p_ph_cpa(70).
  SELECTION-SCREEN END OF BLOCK 56.

SELECTION-SCREEN END OF BLOCK 5.

SELECTION-SCREEN BEGIN OF BLOCK 6 WITH FRAME TITLE TEXT-s03.
  PARAMETERS: par_comp LIKE idfiar205-compressflag,
              s_init   LIKE j_1afpdo-pgdocnumb,
              s_numbc  LIKE j_1afpdo-totpgnumbc,
              par_sort LIKE idfiar205-fiar205sort,
              par_kts1 LIKE idfiar205-prkeysdrt,
              par_sum  LIKE idfiar205-surchsum,
              par_rahm LIKE idfiar205-listbox,
              par_sddi LIKE idfiar205-dsplvf03,
              par_dspt LIKE idfiar205-dsplmtax,
              par_canc LIKE idfiar205-id205canc,   " Note 427832
              par_alv  LIKE idfiar205-fiar205radio.
  PARAMETERS:      " p_col2 to p_col11: texts for column titles
    p_col2   LIKE idfiar205-coltitle,
    p_col22  LIKE idfiar205-coltitle,
    p_col23  LIKE idfiar205-coltitle,
    p_col3   LIKE idfiar205-coltitle,
    p_col4   LIKE idfiar205-coltitle,
    p_col5   LIKE idfiar205-coltitle,
    p_col6   LIKE idfiar205-coltitle,
    p_col7   LIKE idfiar205-coltitle,
    p_col8   LIKE idfiar205-coltitle,
    p_txid1  LIKE idfiar205-colprockey, "TaxID Exports
    p_col09  LIKE idfiar205-coltitle,
    p_txid2  LIKE idfiar205-colprockey, "TaxID PercNC
    p_col10  LIKE idfiar205-coltitle,
    p_col101 LIKE idfiar205-coltitle,
    p_col102 LIKE idfiar205-coltitle,
    p_col103 LIKE idfiar205-coltitle,
*** US #181035 - MMSILVA - 02.06.2025 - Ini ***
    p_col104 LIKE idfiar205-coltitle, "Percepción de IIBB Córdoba
    p_col105 LIKE idfiar205-coltitle, "Percepción de IIBB Entre Rios
*** US #181035 - MMSILVA - 02.06.2025 - Fim ***
    p_col11  LIKE idfiar205-coltitle,
    p_sdexp  LIKE t685t-vtext. " text: SD export total
SELECTION-SCREEN END OF BLOCK 6.

*----------------------------------------------------------------------
* TYPE DEFINITION
*----------------------------------------------------------------------
* field structure for the first output list-----------------------------
TYPES:
  BEGIN OF type_ep,
    bukrs              LIKE bkpf-bukrs,
    budat              LIKE bkpf-budat,
    buper(6)           TYPE n,
    brnch              LIKE bkpf-brnch,
    cpudt              LIKE bkpf-cpudt,
    cputm              LIKE bkpf-cputm,
    belnr              LIKE bkpf-belnr,
    blart              LIKE bkpf-blart,  " Document type
    stblg              LIKE bkpf-stblg,
    xblnr              LIKE bkpf-xblnr,
    waers              LIKE bkpf-waers,
    bldat              LIKE bkpf-bldat,
    wwert              LIKE bkpf-wwert,  " For magnetic output->Exch. rate
    kursf              LIKE bkpf-kursf,  " For magnetic output->Exch. rate
    gjahr              LIKE bkpf-gjahr,
    monat              LIKE bkpf-monat,
    flg_sd_beleg       TYPE c,
    flg_is_beleg       TYPE c,           " IS Media document
    sd_vbeln           LIKE vbrk-vbeln,
    bvorg              LIKE bkpf-bvorg,
    mwskz              LIKE bset-mwskz,
    posnr              LIKE vbrp-posnr,
    ktosl              LIKE bset-ktosl,  " Processing key
    kschl              LIKE bset-kschl,
    linetype           TYPE c,
    ktnra              LIKE bseg-lifnr,
    hwbas              LIKE bset-hwbas,  " Tax base amount local currency
    hwste              LIKE bset-hwste,  " Tax amount in local currency
    name1              LIKE lfa1-name1,
    stcdt              LIKE lfa1-stcdt,
    stcd1              LIKE lfa1-stcd1,
    j_1aoftp           LIKE j_1aotdetr-j_1aoftp, " For magnetic output
    oftp_text          LIKE j_1aoftpt-text5,
    prtchr             LIKE j_1aotdetr-j_1aprtchr,
    augdt              LIKE bseg-augdt,
    augbl              LIKE bseg-augbl,
    koart              LIKE bseg-koart,
    hkont              LIKE bset-hkont,
    j_1arfz            LIKE j_1arztx-j_1arfz,
    fisc_cont          TYPE c,           " For magnetic output
    numpg              LIKE bkpf-numpg,  " For magnetic output
    fityp              LIKE kna1-fityp,  " For magnetic output
*   cai(14)          TYPE n,           " For magnetic output
    cai(14)            TYPE c,          " For magnetic o/p "RG1361 Changes
    due_date           TYPE d,           " For magnetic output
    total(8)           TYPE p,           " End of first line
* Begin of first line
    rate(5)            TYPE p DECIMALS 2,
    dspl_rate(4)       TYPE p DECIMALS 2,
    taxed(8)           TYPE p,
    not_taxed(8)       TYPE p,
    vat(8)             TYPE p,
    rnr_vat(8)         TYPE p,
    vat_percep(8)      TYPE p,
    other_percep(8)    TYPE p,
    munic_per(8)       TYPE p, " Municipal Perception - magnetic output
    earn_per(8)        TYPE p, " Earning Perc. - magn. outp. - Note 645449
    vat_intern(8)      TYPE p, " Internal taxes       - magnetic output
    exemption(8)       TYPE p,
    surcharge(8)       TYPE p, " VAT Surcharge
    exports(8)         TYPE p,           " Exports
    percepnoc(8)       TYPE p,           " Perception not categorized
    iother_percep01(8) TYPE p,
    iother_percep02(8) TYPE p,
    iother_percep03(8) TYPE p,
    line_total(8)      TYPE p,
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
    mwskz           LIKE bset-mwskz, " Tax code
    posnr           LIKE vbrp-posnr,
    rate(5)         TYPE n,          " tax rate
    stgrp           LIKE t007b-stgrp, " Tax type (1/2/3)
    j_1arfz         LIKE j_1arztx-j_1arfz, " Reason for zero tax
    ktosl           LIKE bset-ktosl, " Procesing key
    hkont           LIKE bset-hkont,
    hwbas           LIKE bset-hwbas,
    hwste           LIKE bset-hwste,
    txmod           LIKE bset-txmod,
    netamount       LIKE bseg-dmbtr,
    amount          LIKE bseg-dmbtr,
    amount2         LIKE bseg-dmbtr, " Taxes
    kschl           LIKE bset-kschl,
    mwart           LIKE t007a-mwart,
    zmwsk           LIKE t007a-zmwsk, " Target tax type
    stazf           LIKE t007b-stazf, " Tax not deductible
    j_1ataxid       LIKE j_1ataxid-j_1ataxid,
* for display
    taxed           LIKE bseg-dmbtr, " Tax base amount
    vat_amount      LIKE bseg-dmbtr, " RR VAT amount
    not_taxed       LIKE bseg-dmbtr, " RnR VAT amount
    vat_intern      LIKE bseg-dmbtr, " Internal tax
    exemption       LIKE bseg-dmbtr, " Exemption
    surcharge       LIKE bseg-dmbtr, " VAT Surcharge
    perception      LIKE bseg-dmbtr, " VAT Perception
    region_per      LIKE bseg-dmbtr, " Region Perception
    munic_per       LIKE bseg-dmbtr, " Municipal Perception
    earn_per        LIKE bseg-dmbtr, " Earning Perception  Note 645449
    ertrag_per      LIKE bseg-dmbtr, " Produce Perception
    exports         LIKE bseg-dmbtr, " Exports
    percepnoc       LIKE bseg-dmbtr, " Perception not categorized
    iother_percep01 LIKE bseg-dmbtr, "Percepción de IIBB Buenos Aires
    iother_percep02 LIKE bseg-dmbtr, "Percepción de IIBB CABA
    iother_percep03 LIKE bseg-dmbtr, "Percepción de IIBB Santa Fe

  END OF type_tab_taxes.

TYPES:
  BEGIN OF type_tab_beleg,
    linetype           TYPE c,
    ktnra              LIKE bseg-lifnr,
    mwskz              LIKE bset-mwskz,
    posnr              LIKE vbrp-posnr,
    kschl              LIKE bset-kschl,
    ktosl              LIKE bset-ktosl,
    hkont              LIKE bset-hkont,
    hwbas              LIKE bset-hwbas,
    hwste              LIKE bset-hwste,
    name1              LIKE lfa1-name1,
    stcdt              LIKE lfa1-stcdt,
    stcd1              LIKE lfa1-stcd1,
    augdt              LIKE bseg-augdt,
    augbl              LIKE bseg-augbl,
    koart              LIKE bseg-koart,
    j_1arfz            LIKE j_1arztx-j_1arfz,
    fisc_cont          TYPE c,           " For magnetic output
    fityp              LIKE kna1-fityp,  " For magnetic output
*   cai(14)          TYPE n,           " For magnetic output
    cai(14)            TYPE c,          " For magnetic o/p "RG1361 Changes
    due_date           TYPE d,           " For magnetic output
    total(12)          TYPE p,           " End of first line
* Begin of second line
    rate(5)            TYPE p DECIMALS 2,
    taxed(12)          TYPE p,
    not_taxed(12)      TYPE p,
    vat_intern(8)      TYPE p, " Internal tax
    vat(12)            TYPE p,
    rnr_vat(8)         TYPE p,
    vat_percep(8)      TYPE p,
    other_percep(8)    TYPE p,
    munic_per(8)       TYPE p, " Municipal Perception
    earn_per(8)        TYPE p, " Earning Perception         - Note 645449
    exemption(12)      TYPE p,
    exports(12)        TYPE p,
    percepnoc(8)       TYPE p,
    iother_percep01(8) TYPE p,
    iother_percep02(8) TYPE p,
    iother_percep03(8) TYPE p,
    line_total(12)     TYPE p,
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

  BEGIN OF type_v_record,
    record(267) TYPE c,
    cr_lf       TYPE c,
  END OF type_v_record,

  BEGIN OF type_va_record,
    record(62) TYPE c,
    cr_lf      TYPE c,
  END OF type_va_record,

  BEGIN OF type_c_record,
    record(325) TYPE c,
    cr_lf       TYPE c,
  END OF type_c_record,

  BEGIN OF type_ca_record,
    record(84) TYPE c,
    cr_lf      TYPE c,
  END OF type_ca_record,

  BEGIN OF ty_account_set,
    hkont TYPE bsis-hkont,
  END OF ty_account_set.

*----------------------------------------------------------------------*
* Internal tables (using as prefix: "tab_")                            *
*----------------------------------------------------------------------*
* Note start 1011642
DATA: lv_ktosl TYPE bset-ktosl.
* Note End 1011642
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
    ort01 LIKE t001-ort01,
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
DATA:  BEGIN OF tab_pac   OCCURS 10.
         INCLUDE STRUCTURE j_1apacd.
DATA: END OF tab_pac.

DATA:  BEGIN OF tab_pack   OCCURS 10.
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
    kzwi1   LIKE vbrp-kzwi1,
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
    mwskz           LIKE bset-mwskz,
    posnr           LIKE vbrp-posnr,
    netamount       LIKE bseg-dmbtr,
    amount          LIKE bseg-dmbtr,
    amount2         LIKE bseg-dmbtr, " taxes
* for display
    taxed           LIKE bseg-dmbtr, " Tax base amount
    vat_amount      LIKE bseg-dmbtr, " RR VAT amount
    not_taxed       LIKE bseg-dmbtr, " RnR VAT amount
    vat_intern      LIKE bseg-dmbtr, " Internal tax
    exemption       LIKE bseg-dmbtr, " Exemption
    surcharge       LIKE bseg-dmbtr, " VAT Surcharge
    perception      LIKE bseg-dmbtr, " VAT Perception
    region_per      LIKE bseg-dmbtr, " Region Perception
    munic_per       LIKE bseg-dmbtr, " Municipal Perception
    ertrag_per      LIKE bseg-dmbtr, " Produce Perception
    exports         LIKE bseg-dmbtr, " Exports
    percepnoc       LIKE bseg-dmbtr, " Perception not categorized
    iother_percep01 LIKE bseg-dmbtr, "Percepción de IIBB Buenos Aires
    iother_percep02 LIKE bseg-dmbtr, "Percepción de IIBB CABA
    iother_percep03 LIKE bseg-dmbtr, "Percepción de IIBB Santa Fe
* Special requirement for MM docs: Cp. form CHECK_MM_BASE_AMOUNTS
    hwbas           LIKE bseg-dmbtr, " Base amount for MM docs
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
    mwskz              LIKE bset-mwskz,
    posnr              LIKE vbrp-posnr,
    dspl_rate(4)       TYPE p DECIMALS 2,
    j_1arfz            LIKE j_1arztx-j_1arfz, " For magnetic output
    taxed(8)           TYPE p,
    not_taxed(8)       TYPE p,
    vat_intern(8)      TYPE p,           " For magnetic output
    vat(8)             TYPE p,
    rnr_vat(8)         TYPE p,
    vat_percep(8)      TYPE p,
    other_percep(8)    TYPE p,
    munic_per(8)       TYPE p,           " For magnetic output
    earn_per(8)        TYPE p,           " Note 645449
    exemption(8)       TYPE p,
    surcharge(8)       TYPE p, " VAT Surcharge - For magnetic output
    exports(8)         TYPE p,           " Exports
    percepnoc(8)       TYPE p,           " Perception not categorized
    iother_percep01(8) TYPE p, "Percepción de IIBB Buenos Aires
    iother_percep02(8) TYPE p, "Percepción de IIBB CABA
    iother_percep03(8) TYPE p, "Percepción de IIBB Santa Fe
    line_total(8)      TYPE p,
  END OF tab_ep_comp,

* VAT totals per tax code
  BEGIN OF tab_sum_mwskz OCCURS 30,
    mwskz    LIKE bset-mwskz,
    hwste(8) TYPE p,
  END OF tab_sum_mwskz,

* VAT totals per tax code new version
  BEGIN OF tab_sum_mwskz_new OCCURS 30,
    mwskz              LIKE bset-mwskz,
    taxed(8)           TYPE p,           " taxed
    not_taxed(8)       TYPE p,           " not taxed - Note 454626
    vat(8)             TYPE p,           " vat
    rnr_vat(8)         TYPE p,           " rnr vat
    vat_percep(8)      TYPE p,           " vat perception
    other_percep(8)    TYPE p,           " other perception
    exemption(8)       TYPE p,           " exemption
    exports(8)         TYPE p,           " exports
    percepnoc(8)       TYPE p,           " perception no cat.
    iother_percep01(8) TYPE p, "Percepción de IIBB Buenos Aires
    iother_percep02(8) TYPE p, "Percepción de IIBB CABA
    iother_percep03(8) TYPE p, "Percepción de IIBB Santa Fe
    line_total(8)      TYPE p,           " total_line
  END OF tab_sum_mwskz_new,

* VAT totals per regio
  BEGIN OF tab_sum_regio OCCURS 30,
    regio              LIKE bseg-grirg,
    mwskz              LIKE bset-mwskz,
    taxed(8)           TYPE p,           " taxed
    not_taxed(8)       TYPE p,           " not taxed - Note 454626
    vat(8)             TYPE p,           " vat
    rnr_vat(8)         TYPE p,           " rnr vat
    vat_percep(8)      TYPE p,           " vat perception
    other_percep(8)    TYPE p,           " other perception
    exemption(8)       TYPE p,           " exemption
    exports(8)         TYPE p,          " exports
    percepnoc(8)       TYPE p,          " perception not cat.
    iother_percep01(8) TYPE p, "Percepción de IIBB Buenos Aires
    iother_percep02(8) TYPE p, "Percepción de IIBB CABA
    iother_percep03(8) TYPE p, "Percepción de IIBB Santa Fe
    line_total(8)      TYPE p,           " total_line
  END OF tab_sum_regio,

* VAT totals per für other tax code new version
  BEGIN OF tab_sum_mwskz_others OCCURS 30,
    mwskz              LIKE bset-mwskz,
    kschl              LIKE bset-kschl,         " condition type
    taxed(8)           TYPE p,           " taxed
    not_taxed(8)       TYPE p,           " not taxed
    vat(8)             TYPE p,           " vat
    rnr_vat(8)         TYPE p,           " rnr vat
    vat_percep(8)      TYPE p,           " vat perception
    other_percep(8)    TYPE p,           " other perception
    exemption(8)       TYPE p,           " exemption
    exports(8)         TYPE p,           " exports
    percepnoc(8)       TYPE p,           " perception not cat.
    iother_percep01(8) TYPE p, "Percepción de IIBB Buenos Aires
    iother_percep02(8) TYPE p, "Percepción de IIBB CABA
    iother_percep03(8) TYPE p, "Percepción de IIBB Santa Fe
    line_total(8)      TYPE p,           " total_line
  END OF tab_sum_mwskz_others,

* VAT totals per vat rate
  BEGIN OF tab_sum_rate OCCURS 10,
    rate(4)  TYPE n,
    hwste(8) TYPE p,
  END OF tab_sum_rate,

* VAT totals per official doument type
  BEGIN OF tab_sum_oftpt OCCURS 10,
    oftp_text          LIKE j_1aoftpt-text5,    " official doc. type
    taxed(8)           TYPE p,           " taxed
    not_taxed(8)       TYPE p,           " not taxed
    vat(8)             TYPE p,           " vat
    rnr_vat(8)         TYPE p,           " rnr vat
    vat_percep(8)      TYPE p,           " vat perception
    other_percep(8)    TYPE p,           " other perception
    exemption(8)       TYPE p,           " exemption
    exports(8)         TYPE p,          " exports
    percepnoc(8)       TYPE p,          " perception not cat.
    iother_percep01(8) TYPE p, "Percepción de IIBB Buenos Aires
    iother_percep02(8) TYPE p, "Percepción de IIBB CABA
    iother_percep03(8) TYPE p, "Percepción de IIBB Santa Fe
    line_total(8)      TYPE p,           " total_line
  END OF tab_sum_oftpt,

* VAT totals per account
  BEGIN OF tab_sum_account OCCURS 20,
    hkont    LIKE bseg-hkont,  " account number
    soll(8)  TYPE p,           " debit
    haben(8) TYPE p,           " credit
    saldo(8) TYPE p,           " saldo
  END OF tab_sum_account,

* Other VAT totals
  BEGIN OF tab_sum_others OCCURS 30,
    mwskz        LIKE bset-mwskz,
    kschl        LIKE bset-kschl,         " Condition type
    hwste(8)     TYPE p,
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
DATA:   j_1aamnt11 TYPE j_1aamnt.                    " Note 685915
DATA: END OF wa_history.

* work area for the historical table (übertrag)
DATA: BEGIN OF wa_history_over.
        INCLUDE STRUCTURE j_1a101.
DATA:   j_1aamnt11 TYPE j_1aamnt.                    " Note 685915
DATA: END OF wa_history_over.

* text lines
DATA: BEGIN OF xtline OCCURS 10.
        INCLUDE STRUCTURE tline.
DATA: END OF xtline.
DATA: BEGIN OF tab_bkorm OCCURS 10.
        INCLUDE STRUCTURE bkorm.
DATA: END OF tab_bkorm.
DATA:  text_name        LIKE thead-tdname.
DATA: payer LIKE vbpa-parvw.                " Note 948565

DATA: t_set_account  TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
      tg_account_set TYPE TABLE OF ty_account_set WITH HEADER LINE.

DATA: t_bsas TYPE STANDARD TABLE OF bsas WITH HEADER LINE,
      t_bsis TYPE STANDARD TABLE OF bsis WITH HEADER LINE.

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
DATA  BEGIN OF mm_doc OCCURS 1.
INCLUDE STRUCTURE acc_doc.
DATA  END   OF mm_doc.

* tax codes for manual posted taxes                "note 159314
DATA: BEGIN OF man_tax OCCURS 0,                           "note 159314
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


*----------------------------------------------------------------------*
* Field groups                                                         *
*----------------------------------------------------------------------*
DATA:
* Feldleiste mit Einzelposten-Informationen
  ep             TYPE type_ep.

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
  txt_zeile41(245)         TYPE c,              " Title line 4
  txt_zeile5(245)          TYPE c,              " Title line 5
  txt_zeile6(245)          TYPE c,              " Title line 6
  txt_zeile7(245)          TYPE c,              " Document line 1
  txt_zeile8(245)          TYPE c,              " Document line 2
  txt_zeile9(245)          TYPE c,              " Line sums
  t_top                    TYPE slis_t_listheader,
  pagno                    LIKE sy-cpage,

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
    "V1(1)            TYPE C,
    "MWSKZ(6)         TYPE C,
    "V2(1)            TYPE C,
    "RATE(8)          TYPE C,
    v3(1)            TYPE c,
    taxed(18)        TYPE c,
    v4(1)            TYPE c,
*    TAXED2(18)       TYPE C,
*    V42(1)           TYPE C,
*    TAXED3(18)       TYPE C,
*    V43(1)           TYPE C,
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
*    TAXED2(18)       TYPE C,
*    V42(1)           TYPE C,
*    TAXED3(18)       TYPE C,
*    V43(1)           TYPE C,
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
*    TAXED2(18)       TYPE C,
*    V42(1)           TYPE C,
*    TAXED3(18)       TYPE C,
*    V43(1)           TYPE C,
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
*    TAXED2(18)       TYPE C,
*    V42(1)           TYPE C,
*    TAXED3(18)       TYPE C,
*    V43(1)           TYPE C,
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

DATA:
  BEGIN OF i_record4,
    docu_date(8)            TYPE n,           " document date yyyymmdd
    docu_type(3)            TYPE c,           " document type
    brnch(5)                TYPE c,           " branch
    docu_no(20)             TYPE n,           " official document no.
    dp_imp(16)              TYPE c,           " Despacho de importación
    doc_tp_vendor(2)        TYPE c,           " Código de documento del comprador
    id_docu_no(20)          TYPE n,           " cuit or id
    name(30)                TYPE c,           " vend./cust. name.)
    total_amnt(15)          TYPE n,           " total amount
    n_taxed_amnt(15)        TYPE n,           " not taxed
    exempt_amnt(15)         TYPE n,           " Importe de operaciones exentas (15)
    vat_percep(15)          TYPE n,           "
    perce_amnt(15)          TYPE n,           " national perception
    percept_buenos_caba(15) TYPE n,           " Importe de percepciones de Ingresos Brutos (15)
    perc_mp_amnt(15)        TYPE n,           " municipal perception
    int_tax_amnt(15)        TYPE n,           " internal tax
    curr_code(3)            TYPE c,           " currency code
    exch_rate(10)           TYPE n,           " exchange rt(4 deci.)
    sev_vat_rates           TYPE c,           " number of vat rates
    cd_operacao(1)          TYPE c,           "Código de operación (1)
    vat(15)                 TYPE n,           "IVA
    other_percept(15)       TYPE n,           "Otros Tributos
    id_emi(11)              TYPE c,           "CUIT emisor/corredor = Vazio
    name_emi(30)            TYPE c,           "Denominación del emisor/corredor
    vat_com(15)             TYPE n,            "VAT Comissio
  END OF i_record4.

DATA:
  BEGIN OF i_record5,
    docu_type(3)     TYPE c,           " document type
    brnch(5)         TYPE c,           " branch
    docu_no(20)      TYPE n,           " official document no.
    doc_tp_vendor(2) TYPE c,           " Código de documento del comprador
    id_docu_no(20)   TYPE n,           " cuit or id
    taxed(15)        TYPE n,           " total tax
    alic_iva(4)      TYPE c,           " Alic. IVA
    tax_liq(15)      TYPE n,           " Impuesto Liquidado
  END OF i_record5.

* Structure for the Purchase File Summation
DATA:
  BEGIN OF i_record3_sum,
    total_amnt(8)   TYPE p,           " total amount
    n_taxed_amnt(8) TYPE p,           " not taxed
    taxed_amnt(8)   TYPE p,           " taxed
    tax_amnt(8)     TYPE p,           " tax
    exempt_amnt(8)  TYPE p,           " exemption
    perce_amnt(8)   TYPE p,           " national perception
    per_nat_amnt(8) TYPE p,           " other national perception
    "Note  728727
    perc_gi_amnt(8) TYPE p,           " gross income perception
    perc_mp_amnt(8) TYPE p,           " municipal perception
    int_tax_amnt(8) TYPE p,           " internal tax
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

DATA:

  BEGIN OF o_record4,
    docu_date(8)            TYPE n,           " document date yyyymmdd
    docu_type(3)            TYPE c,           " document type
    brnch(5)                TYPE c,           " branch
    docu_no(20)             TYPE n,           " official document no.
    docu_no1(20)            TYPE n,           " official document no.
    doc_tp_vendor(2)        TYPE c,           " Código de documento del comprador
    id_docu_no(20)          TYPE n,           " cuit or id
    name(30)                TYPE c,           " vend./cust. name.)
    total_amnt(15)          TYPE n,           " total amount
    n_taxed_amnt(15)        TYPE n,           " not taxed
    percept_no_cat(15)      TYPE n,           " Percepción a no categorizados
    exempt_amnt(15)         TYPE n,           " Importe de operaciones exentas (15)
    perce_amnt(15)          TYPE n,           " national perception
    percept_buenos_caba(15) TYPE n,           " Importe de percepciones de Ingresos Brutos (15)
    perc_mp_amnt(15)        TYPE n,           " municipal perception
    int_tax_amnt(15)        TYPE n,           " internal tax
    curr_code(3)            TYPE c,           " currency code
    exch_rate(10)           TYPE n,           " exchange rt(4 deci.)
    sev_vat_rates           TYPE c,           " number of vat rates
    cd_operacao(1)          TYPE c,           "Código de operación (1)
    other_percept(15)       TYPE n,           "Otros Tributos (15)
    fecha_vcto(8)           TYPE c,           "Fecha de vencimiento de Pago (8)
  END OF o_record4,

  BEGIN OF o_record5,
    docu_type(3) TYPE c,           " document type
    brnch(5)     TYPE c,           " branch
    docu_no(20)  TYPE n,           " official document no.
    taxed(15)    TYPE n,           " total tax
    alic_iva(4)  TYPE c,           " Alic. IVA
    tax_liq(15)  TYPE n,           " Impuesto Liquidado
  END OF o_record5.

* Structure for the Sales File Summation
DATA:
  BEGIN OF o_record3_sum,
    total_amnt(8)   TYPE p,           " total amount
    n_taxed_amnt(8) TYPE p,           " not taxed
    taxed_amnt(8)   TYPE p,           " taxed
    tax_amnt(8)     TYPE p,           " tax
    surch_amnt(8)   TYPE p,           " Surcharge (equation)
    exempt_amnt(8)  TYPE p,           " exemption
    perce_amnt(8)   TYPE p,           " national perception
    perc_gi_amnt(8) TYPE p,           " gross income perception
    perc_mp_amnt(8) TYPE p,           " municipal perception
    int_tax_amnt(8) TYPE p,           " internal tax
  END OF o_record3_sum.

* Record for the Perceptions File - Length 94
DATA: BEGIN OF tab_p_record2 OCCURS 0.
        INCLUDE TYPE type_p_record2.
DATA: END OF tab_p_record2.
DATA: p_record2 TYPE type_p_record2.

DATA: tab_i_record TYPE TABLE OF type_i_record,   " For Purchases
      wa_i_record  TYPE type_i_record,            "     File - local
      tab_o_record TYPE TABLE OF type_o_record,   " For Sales
      wa_o_record  TYPE type_o_record,            "     File - local
      tab_p_record TYPE TABLE OF type_p_record,   " For Perceptions
      wa_p_record  TYPE type_p_record.            "     File - local


DATA: tab_c_record  TYPE TABLE OF type_c_record,   " For Purchases
      wa_c_record   TYPE type_c_record,            "     File - local

      tab_ca_record TYPE TABLE OF type_ca_record, " For Purchases Alic.
      wa_ca_record  TYPE type_ca_record,          "     File - local

      tab_v_record  TYPE TABLE OF type_v_record,   " For Sales
      wa_v_record   TYPE type_v_record,            "     File - local

      tab_va_record TYPE TABLE OF type_va_record, " For Sales Alic.
      wa_va_record  TYPE type_va_record.          "     File - local


*----------------------------------------------------------------------*
* Fields for Magnetic Output                                           *
*----------------------------------------------------------------------*
DATA: no_of_rec      TYPE i,           " records of type
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
DATA: len TYPE i.      " length of the file 1037893




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

*----------------------------------------------------------------------*
* init selection screen
*----------------------------------------------------------------------*
INITIALIZATION.
  s_init = 4.                          " each calendar month new lineno
  s_drver = '01'.                      " version of the daily reports
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
  p_ph_vd  = 'C:\TEMP\FILE_VENTAS.TXT'.
  p_ph_vda = 'C:\TEMP\FILE_VENTAS_ALIC.TXT'.
  p_ph_cp  = 'C:\TEMP\FILE_COMPRAS.TXT'.
  p_ph_cpa = 'C:\TEMP\FILE_COMPRAS_ALIC.TXT'.

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
        TEXT-u34 TO p_col22,
        TEXT-u35 TO p_col23,
        TEXT-u13 TO p_col3,
        TEXT-u14 TO p_col4,
        TEXT-u15 TO p_col5,
        TEXT-u16 TO p_col6,
        TEXT-u17 TO p_col7,
        TEXT-u18 TO p_col8,
        TEXT-u21 TO p_col09,
        TEXT-u22 TO p_col10,
        TEXT-u36 TO p_col101,
        TEXT-u37 TO p_col102,
        TEXT-u38 TO p_col103,
*** US #181035 - MMSILVA - 02.06.2025 - Ini ***
        TEXT-u39 TO p_col104, "Percep.IIBB CBA
        TEXT-u40 TO p_col105, "Percep.IIBB E.R
*** US #181035 - MMSILVA - 02.06.2025 - Fim ***
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
    MESSAGE e010(ar) WITH TEXT-q01.
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_kts1.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = par_kts1
    EXCEPTIONS
      OTHERS  = 1.

*----------------------------------------------------------------------*
*  Preperations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.

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
  IF bkpf-awtyp      = 'BKPFF'         "FI Document
  AND bkpf-adisc NE 'S'.               "Check not SD discount 1085024
    flg_is_beleg = 'F'.
  ENDIF.

* Note 1091024 Start
*set flag for external documents
  IF flg_is_beleg IS INITIAL.
    flg_is_beleg = 'F'.
  ENDIF.
* Note 1091024 End

* Event_001
*    <BEGIN OF ENHANCEMENT>
* call function 'OPEN_FI_PERFORM_XXXXXX001_X'
*      exporting
*                I_BKPF  = BKPF
*    <END OF ENHANCEMENT>

*For IDocs the reference procedure might be customized, see note 210137
*    <BEGIN OF MODIFICATION>
* if bkpf-awtyp      = 'ZZZZ'.         "template IDoc, see note 210137
*   flg_is_beleg = 'Z'.      "replace 'Z' by 'F' for FI, 'R' for MM etc.
* endif.
*    <END OF MODIFICATION>

*----------------------------------------------------------------------*
* Document items
*       append
*  BSEG---------> tab_bseg                ( Customer / Vendor Lines)
*      |--------> tab_bseg_nodk           ( no Customer / Vendor Lines)
*----------------------------------------------------------------------*
GET bseg.

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

* Event_002
*    <BEGIN OF ENHANCEMENT>
*  CALL FUNCTION 'OPEN_FI_PERFORM_XXXXXX002_X'
*       EXPORTING
*            I_BKPF = BKPF
*            I_BSET = BSET
*       CHANGING
*            RCODE  = RCODE
*       EXCEPTIONS
*            PROCESSED_DOCS = 1
*            OTHERS         = 2.
*  CHECK SY-SUBRC NE 1.
*    <END OF ENHANCEMENT>

  PERFORM check_tax_code USING bset-mwskz.
  CHECK rcode = 0.

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

* Event_003
*    <BEGIN OF ENHANCEMENT>
*  CALL FUNCTION 'OPEN_FI_PERFORM_XXXXXX003_X'
*       EXPORTING
*            I_PAR_VERS      = PAR_VERS
*            I_TAB_J_1ADRVER = TAB_J_1ADRVER
*            I_SD_DOCM       = SD_DOCM
*            I_SD_DISC_FACT  = SD_DISC_FACT
*       CHANGING
*            E_BKPF          = BKPF
*            E_BSET          = BSET
*            E_XKURSF        = XKURSF
*            E_FLG_REJECT_DOC = FLG_REJECT_DOC
*            E_RCODE         = RCODE
*            E_SD_REASONS_SET = SD_REASONS_SET
*            E_FLG_IS_BELEG  = FLG_IS_BELEG
*            E_FLG_SD_BELEG  = FLG_SD_BELEG
*            E_XVBELN        = XVBELN
*       TABLES
*            TAB_LOG_ENTRY2  = TAB_LOG_ENTRY2
*            TAB_VBRP        = TAB_VBRP
*            TAB_BSET        = TAB_BSET
*            LINE_TOTAL      = LINE_TOTAL
*            TAB_001         = TAB_001
*            TAB_KONV        = TAB_KONV
*            SD_REASONS      = SD_REASONS.
*  CHECK FLG_REJECT_DOC IS INITIAL.
*    <END OF ENHANCEMENT>

* Main part follows: Calculate tax base amount,
* distribute amounts to columns.
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

  LOOP.                                " extracted data

    AT FIRST.
      list_number = 1.                 " List number 1 Einzelposten
    ENDAT.


*  Begin of note 988302

*if ep-KTOSL = tab_bset-KTOSL.          " Note 1007703 "1013963
*   if ep-KSCHL = tab_bset-KSCHL.       " Note 1007703 "1013963

*  if ep-KSCHL = 'MWVS'. Note 1007703
    rate = ep-rate.
    IF rate = 0.
      find_exempted = 'X'.
    ENDIF.
*  endif.  " Note 1013963
*endif.    " Note 1013963
*endif. "1013963
*  End of note 988302

* OBSOLETE with change of magnetic output (note 597755)
*    AT NEW EP-BRNCH.                   " For magnetic output
*      IF NOT PAR_MAGN IS INITIAL.
*        CLEAR REC_SUBSIDIARY.
*      ENDIF.
*    ENDAT.

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
      PERFORM: print_tab_ep.           " print list with all VAT infos
      " perform magn output if selected
      REFRESH: tab_ep.
      ADD 1 TO wa_history-j_1alineno.
    ENDAT.

    AT END OF ep-budat.                " end of posting date
      PERFORM add_entry_to_tab_history.
    ENDAT.

    AT END OF ep-buper.
      IF sy-linno < 55.
        SKIP TO LINE 55.               " to call end-of-page
      ENDIF.
      RESERVE 3 LINES.
    ENDAT.

* OBSOLETE with change of magnetic output (note 597755)
*    AT END OF EP-BRNCH.
*      IF NOT PAR_MAGN IS INITIAL.
*        CLEAR: TAB_BRANCH.
*        TAB_BRANCH-BRANCH  = TAB_EP-BRNCH.
*        TAB_BRANCH-NO_RECS = REC_SUBSIDIARY.
*        APPEND TAB_BRANCH.
*        NEW-PAGE.
*      ENDIF.
*    ENDAT.

*    AT LAST.
*      IF sy-linno < 55.
*        SKIP TO LINE 55.               " to call end-of-page
*      ENDIF.
*      RESERVE 3 LINES.
*
*      IF NOT par_magn IS INITIAL.
**     For records type 3: Calendar month of fiscal period needed
*        gf_poper = tab_ep-monat.       " For import parameter
*        CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
*          EXPORTING
*            i_gjahr        = tab_ep-gjahr
*            i_periv        = tab_001-periv
*            i_poper        = gf_poper
*          IMPORTING
*            e_date         = gf_rec3_buper
*          EXCEPTIONS
*            input_false    = 1
*            t009_notfound  = 2
*            t009b_notfound = 3.
*        IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
*
*        IF tab_j_1adrver-j_1aproc = '1' OR
*           tab_j_1adrver-j_1aproc = '3'.    " input/purchase
*          PERFORM fill_i_record3 CHANGING i_record3.
*        ELSE.                          " output/sales
*          PERFORM fill_o_record3 CHANGING o_record3.
*        ENDIF.
*      ENDIF.
*
*      list_number = 2.                 " list number print totals
*      IF s_numbc IS INITIAL.
*        wa_history-j_1apageno = 1.     " start pageno with 1
*      ENDIF.
** OBSOLETE with change of magnetic output (note 597755)
**      IF NOT PAR_MAGN IS INITIAL.      " For magnetic output only
**        PERFORM PRINT_TAB_BRANCH.      " Records per branch
**      ENDIF.
*      PERFORM print_tab_sum_mwskz.     " print totals of tax code
*      PERFORM print_tab_sum_others.    " print totals of others
*      PERFORM print_tab_sum_mwskz_new. " print totals of tax code
*      PERFORM print_tab_sum_rate.      " print totals of vat rate
*      PERFORM print_tab_sum_regio.     " print totals of regio
*      PERFORM print_tab_sum_oftpt.     " print totals of amtl. belegart
*      PERFORM print_tab_sum_account.   " print totals of account
*    ENDAT.

  ENDLOOP.


*INCLUDE z_j_1af205_alv.
*INCLUDE z_j_1af205_form.

  PERFORM f_genera_alv.

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

  "CS2016001252
  IF ( pa_vd IS NOT INITIAL ) AND ( p_ph_vd IS NOT INITIAL ) AND ( tab_v_record[] IS NOT INITIAL ).
    CLOSE DATASET p_ph_vd.
    PERFORM transfer_local USING '' p_ph_vd tab_v_record.
  ENDIF.

  IF ( pa_vda IS NOT INITIAL ) AND ( p_ph_vda IS NOT INITIAL ) AND ( tab_va_record[] IS NOT INITIAL ).
    CLOSE DATASET p_ph_vda.
    PERFORM transfer_local USING '' p_ph_vda tab_va_record.
  ENDIF.

  IF ( pa_cp IS NOT INITIAL ) AND ( p_ph_cp IS NOT INITIAL ) AND ( tab_c_record[] IS NOT INITIAL ).
    CLOSE DATASET p_ph_cp.
    PERFORM transfer_local USING '' p_ph_cp tab_c_record.
  ENDIF.

  IF ( pa_cpa IS NOT INITIAL ) AND ( p_ph_cpa IS NOT INITIAL ) AND ( tab_ca_record[] IS NOT INITIAL ).
    CLOSE DATASET p_ph_cpa.
    PERFORM transfer_local USING '' p_ph_cpa tab_ca_record.
  ENDIF.
  "Fim CS201600125



*  list_number = 3.                     " list number print log entries
*  NEW-PAGE.
*  PERFORM update_history_table.        " udate history table
*  PERFORM print_tab_log_entry10.       " not found table entries
*  PERFORM print_tab_log_entry20.       " not selected BELNR for KTOSL
*  PERFORM print_tab_history.           " selected history

  CLEAR: tab_ep.

*----------------------------------------------------------------------*
* Batch-Heading und own List-Heading                                   *
*----------------------------------------------------------------------*
*TOP-OF-PAGE.

*  PERFORM prepare_list_header.
*  FORMAT COLOR 1 INTENSIFIED.
*
*  IF list_number EQ 1.                 " document items with detail
*    ULINE.
*    WRITE:  txt_zeile1.
*    ULINE.
*    WRITE:  txt_zeile2, txt_zeile3, txt_zeile4.
*    ULINE.
*    WRITE:  txt_zeile5, txt_zeile6.
*    ULINE.
** print sums line starting with page 2
*    IF wa_history-j_1apageno > 1.
*      PERFORM prepare_page_total USING '0'." for List Begin
*      FORMAT: INTENSIFIED OFF, COLOR 3.
*      WRITE:  txt_zeile9.
*      ULINE.
*      FORMAT COLOR OFF.
*    ENDIF.
*  ENDIF.
*  IF list_number EQ 2  OR              " vat totals
*     list_number EQ 3  OR              " tab_log_entry1
*     list_number EQ 4  .               " tab_history
*    ULINE.
*    WRITE:  txt_zeile1.
*    ULINE.
*    WRITE:  txt_zeile2, txt_zeile3, txt_zeile4.
*    ULINE.
*  ENDIF.

*  ADD 1 TO wa_history-j_1apageno.
*  flg_new_page = 'X'.                  " flg for new-page accoured

*----------------------------------------------------------------------*
* End of Page: print the SUM Line for each page
*----------------------------------------------------------------------*
END-OF-PAGE.

  IF list_number EQ 1.
    IF wa_history-j_1apageno NE 1.
      wa_history_over = wa_history.
    ENDIF.
*    PERFORM prepare_page_total USING '1'." for List end
*    FORMAT COLOR 3 INTENSIFIED.
*    ULINE.
*    WRITE:  txt_zeile9.
*    ULINE.
  ENDIF.
*  flg_new_page = 'X'.                  " flg for new-page accoured

*----------------------------------------------------------------------*
*   LINE-SELECTION
*----------------------------------------------------------------------*
*AT LINE-SELECTION.
*  CHECK: NOT tab_ep-bukrs IS INITIAL,
*         NOT tab_ep-gjahr IS INITIAL,
*         NOT tab_ep-belnr IS INITIAL.
** PAR_SDDI set? Then show SD documents in transaction VF03
*  IF NOT tab_ep-flg_sd_beleg IS INITIAL AND
*    NOT par_sddi IS INITIAL.
** Event_009
**    <BEGIN OF ENHANCEMENT>
**    CALL FUNCTION '/XXARISM/VAT_BOOK_EVENT_009'
**         CHANGING
**              E_TAB_EP       = TAB_EP
**         EXCEPTIONS
**              PROCESSED_DOC  = 1
**              OTHERS         = 2.
**    CHECK SY-SUBRC NE 1.
**    <END OF ENHANCEMENT>
*    SET PARAMETER ID 'VF' FIELD tab_ep-sd_vbeln.
*    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*  ELSE.
** display always FB03 ( FI Belege )
*    SET PARAMETER ID 'BLN' FIELD tab_ep-belnr.
*    SET PARAMETER ID 'BUK' FIELD tab_ep-bukrs.
*    SET PARAMETER ID 'GJR' FIELD tab_ep-gjahr.
*    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*  ENDIF.
  CLEAR tab_ep.

*----------------------------------------------------------------------*
* Subroutines to fill the tables with the tax information
*----------------------------------------------------------------------*
*     Called from GET BSEG:
* FILL_BSEG_TO_TAB_BSEG
*     Called from GET BSET:
* FILL_BSET_TO_TAB_BSET
*     Called from GET BKPF:
* READ_SD_INVOICE_DATA
* READ_MM_INVOICE_DATA
* FILL_TAB_TAXES
* FILL_TAB_BSEG_TAXED
* FILL_TAB_BELEG
* EXTRACT_TAB_BELEG_TO_EP
*     Nested calls:
* GET_FI_CANCEL_DOCM            from READ_SD_INVOICE_DATA
* GET_LOCAL_AMOUNT              from READ_SD_INVOICE_DATA
* FILL_BSET                     from READ_SD_INVOICE_DATA
* FILL_TAX_BASE_AMNT            from FILL_TAB_TAXES
* FILL_SURCHARGE_TAX_CODES      from FILL_TAB_TAXES
* GET_DOWNPAYMENT_REQUEST       from CHECK_TAB_BSEG
*----------------------------------------------------------------------*

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
ENDFORM.                               " FILL_BSEG_TO_TAB_BSEG

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
ENDFORM.                               " FILL_BSET_TO_TAB_BSET

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
* Begin Change - 30.10.2012 - Diego
            IF vbrk-vbtyp EQ 'O'.
* -----------------------------------
              IF g_sd_amt < 0.
                g_sd_amt = g_sd_amt * -1.
              ENDIF.
* ----------------------------------------
            ENDIF.
* End Change - 30.10.2012 - Diego
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
ENDFORM.                               " READ_SD_INVOICE_DATA

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
ENDFORM.                               " READ_MM_INVOICE_DATA

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

  DATA: vawkey  TYPE bkpf-awkey,
        f_belnr TYPE rbkp-belnr,
        f_gjahr TYPE rbkp-gjahr,
        i_xrech TYPE rbkp-xrech,
        vfwbas  TYPE rbtx-fwbas,
        vwrbtr  TYPE bseg-wrbtr,
        vshkzg  TYPE bseg-shkzg.


  REFRESH: lcl_tab_mwskz.
  REFRESH: tab_taxes, tax_base, dis_taxes, sum_tab_taxes.
  CLEAR: dis_taxes, tax_base.
* fill tab_taxes

  SORT tab_bset BY mwskz. "ALRS para que seja considerado a checagem se tem TAXED primeiro o code mais baixo

  LOOP AT tab_bset.

    CHECK: tab_bset-mwskz NE '**',
          tab_bset-mwskz NE space.

*    comentado em 03.06.2013
*    IF  'C1_C2' CS TAB_BSET-MWSKZ.
*      IF TAB_BSET-HWSTE < 0.
*        TAB_BSET-HWSTE = TAB_BSET-HWSTE * -1.
*        MODIFY TAB_BSET INDEX SY-TABIX TRANSPORTING HWSTE.
*      ENDIF.
*    ENDIF.

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

    "ALRS 02.07.2015
*    IF  TAB_TAXES-NETAMOUNT LT 0. " ALRS positivar BASE
*      TAB_TAXES-NETAMOUNT =  TAB_TAXES-NETAMOUNT  * -1.
*    ENDIF.
    IF tab_taxes-mwskz = 'C9' AND bkpf-tcode = 'MIRO'.
      vawkey  = bkpf-awkey.
      f_belnr = bkpf-awkey+0(10).
      f_gjahr = bkpf-awkey+10(4).
      SELECT SINGLE xrech
        INTO i_xrech
        FROM rbkp
       WHERE belnr = f_belnr
       AND   gjahr = f_gjahr.

      IF i_xrech = 'X'.
        tab_bset-shkzg = 'S'.
      ELSE.
        tab_bset-shkzg = 'H'.
      ENDIF.
      SELECT SUM( fwbas )
        INTO vfwbas
        FROM rbtx
       WHERE belnr = f_belnr
       AND   gjahr = f_gjahr
       AND   mwskz = 'C9'.
      IF vfwbas NE 0.
        tab_bset-hwbas = vfwbas.
        tab_bset-hwste = vfwbas.
      ELSE.

* ---> S4 Migration - 07/07/2023 - JP
*        SELECT SINGLE wrbtr shkzg
*          INTO (vwrbtr, vshkzg )
*          FROM bseg
*        WHERE belnr = f_belnr
*       AND   gjahr = f_gjahr
*       AND   mwskz = 'C9'.
*
        DATA: lv_rldnr TYPE  rldnr,
              lv_bukrs TYPE  bukrs,
              lv_belnr TYPE  belnr_d,
              lv_gjahr TYPE  gjahr.

        lv_bukrs = bkpf-bukrs.
        lv_belnr = f_belnr.
        lv_gjahr = f_gjahr.

        DATA: lt_bseg_aux TYPE fagl_t_bseg.

        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = lv_rldnr
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.

        IF sy-subrc = 0.

          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr   = lv_rldnr
              i_bukrs   = lv_bukrs
              i_belnr   = lv_belnr
              i_gjahr   = lv_gjahr
            IMPORTING
              et_bseg   = lt_bseg_aux
            EXCEPTIONS
              not_found = 1.
        ENDIF.

        IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.

          sy-dbcnt = lines( lt_bseg_aux ).
          LOOP AT lt_bseg_aux INTO DATA(w_bseg).
            IF w_bseg-mwskz = 'C9'.
              vwrbtr   = w_bseg-wrbtr.
              vshkzg   = w_bseg-shkzg.
              CONTINUE.
            ENDIF.
          ENDLOOP.

        ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

        IF sy-subrc = 0.
          tab_bset-hwbas = vwrbtr.
          tab_bset-hwste = vwrbtr.
          tab_bset-shkzg = vshkzg.
        ENDIF.
      ENDIF.

      IF tab_bset-shkzg = 'H' AND tab_bset-hwste NE 0. "ALRS
        tab_bset-hwbas = - tab_bset-hwbas.
        tab_bset-hwste = - tab_bset-hwste.
        tab_bset-shkzg = 'S'.
      ENDIF.

    ENDIF.

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
          tab_taxes-perception = tab_bset-hwste.           " Note 645449
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
ENDFORM.                               " FILL_TAB_TAXES

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
    IF  bkpf-xblnr+4(1) EQ 'E' AND par_magn IS INITIAL.
      tab_bseg_taxed-exports   =  sum_tab_taxes-netamount.
      CLEAR: tab_bseg_taxed-taxed,
             tab_bseg_taxed-not_taxed,
             tab_bseg_taxed-exemption.
    ENDIF.

*  Only VAT amount is checked for initial as zero VAT implies Zero VAT
*  surcharge and zero VAT Perception
*  Note 727028 Starts (Message 97613 2004)

    IF sum_tab_taxes-vat_amount IS INITIAL
          AND tab_bseg_taxed-exemption IS INITIAL.   "Note 768422
      IF NOT sum_tab_taxes-region_per IS INITIAL.
*        tab_bseg_taxed-exemption = tab_bseg_taxed-taxed. "Note 1034548
        CLEAR tab_bseg_taxed-taxed.
      ELSEIF NOT sum_tab_taxes-munic_per IS INITIAL.
*        tab_bseg_taxed-exemption = tab_bseg_taxed-taxed. "Note 1034548
        CLEAR tab_bseg_taxed-taxed.
      ENDIF.
    ENDIF.
*  Note 727028 Ends

    APPEND tab_bseg_taxed.
  ENDLOOP.                             " sum_tab_taxes
ENDFORM.                               " FILL_TAB_BSEG_TAXED

*&---------------------------------------------------------------------*
*&      Form  FILL_TAB_BELEG
* tab_beleg contains all lines for output in no compress modus
*----------------------------------------------------------------------*
*  -->  p1        bkpf,tab_taxes, tab_bseg_taxed, flg_is_beleg
*  <--  p2        tab_beleg
*----------------------------------------------------------------------*
FORM fill_tab_beleg.
  DATA: flg_gl        TYPE c,         " Check docs with special G/L item
        gl_amount(12) TYPE p.         " Total: Docs with special G/L item

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
    tab_beleg-total = 100 * tab_bseg-total.  " Summe Beleg

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


    tab_beleg-taxed =  100 * tab_bseg_taxed-taxed.
    tab_beleg-not_taxed = 100 * tab_bseg_taxed-not_taxed .
    tab_beleg-exemption = 100 * tab_bseg_taxed-exemption .
    tab_beleg-exports = 100 * tab_bseg_taxed-exports .

    tab_beleg-line_total =  100 * ( tab_bseg_taxed-taxed +
                           tab_bseg_taxed-not_taxed +
                           tab_bseg_taxed-exemption +
                           tab_bseg_taxed-exports ) .
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
    tab_beleg-total = 100 * tab_bseg-total.

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

    tab_beleg-taxed        = tab_taxes-taxed .
* Note 1041712 Start
*    IF par_magn IS INITIAL.
*      tab_beleg-not_taxed    = tab_taxes-not_taxed +
*                               tab_taxes-vat_intern.
*    ELSE.
*      tab_beleg-not_taxed    = tab_taxes-not_taxed.
*      tab_beleg-vat_intern   = tab_taxes-vat_intern.
*    ENDIF.
    tab_beleg-not_taxed    = 100 * tab_taxes-not_taxed.
    tab_beleg-vat_intern   = tab_taxes-vat_intern.
* Note 1041712 End
    tab_beleg-vat          = 100 * tab_taxes-vat_amount.
    tab_beleg-rnr_vat      = tab_taxes-surcharge.
    tab_beleg-vat_percep   = 100 * tab_taxes-perception.
    tab_beleg-other_percep = 100 * ( tab_taxes-region_per +
                             tab_taxes-ertrag_per ).
    tab_beleg-munic_per    = tab_taxes-munic_per.
    tab_beleg-earn_per     = tab_taxes-earn_per.
    tab_beleg-exemption    = tab_taxes-exemption.
    tab_beleg-exports      = tab_taxes-exports.
    tab_beleg-percepnoc    = tab_taxes-percepnoc.

    tab_beleg-line_total = (
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
                              tab_beleg-percepnoc ) .
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
ENDFORM.                               " FILL_TAB_BELEG

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

  CONDENSE bkpf-xblnr NO-GAPS.

  data(lv_i) = strlen( bkpf-xblnr ).

  IF lv_i > 13 .
    bkpf-xblnr = bkpf-xblnr+1(14).
    ep-prtchr   = bkpf-xblnr+4(1).
    ep-xblnr = bkpf-xblnr.
    CLEAR: ep-xblnr+4(1).
  ELSE.
    ep-prtchr   = bkpf-xblnr+4(1).
    CLEAR: ep-xblnr+4(1).
  ENDIF.


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
ENDFORM.                               " EXTRACT_TAB_BELEG_TO_EP

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
ENDFORM.                               " GET_FI_CANCEL_DOCM

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
ENDFORM.                               " GET_LOCAL_AMNT

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

  "===============================================Anderson Oenning
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
ENDFORM.                               " FILL_BSET

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

    "===============================================Anderson Oenning
    IF vbrk-fkart EQ 'YV21' OR vbrk-fkart EQ 'YV22'.

      tab_taxes-netamount = tab_vbrp-kzwi1.

      IF tab_vbrp-netwr < 0.
        DATA(vg_valor) = abs( tab_vbrp-netwr ).
        tab_taxes-not_taxed  = tab_vbrp-kzwi1 - vg_valor.

        IF tab_taxes-netamount > 0.
          tab_taxes-netamount  = tab_taxes-netamount - ( tab_taxes-netamount * 2 ). "Converter o valor para negativo.
        ENDIF.

      ELSE.
        tab_taxes-not_taxed  = tab_vbrp-kzwi1 - tab_vbrp-netwr.
      ENDIF.
    ELSE.
      tab_taxes-netamount = tab_vbrp-netwr.
    ENDIF.
    CLEAR: vg_valor.
    "===============================================Anderson Oenning


  ELSEIF flg_is_beleg = 'R'.
    CLEAR mm_taxed.

    READ TABLE mm_taxed WITH KEY mwskz = tab_taxes-mwskz
                                 txgrp = tab_taxes-posnr.
    IF sy-subrc NE 0.
*      IF tab_taxes-txmod = 0.                               "1074703
      IF tab_taxes-txmod <> 0.                              "1074703
        CLEAR tab_taxes-netamount.       " only tax posted
      ELSE.
        tab_taxes-netamount = tab_taxes-hwbas.
      ENDIF.
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
ENDFORM.                               " FILL_TAX_BASE_AMNT

*&---------------------------------------------------------------------*
*&      Form  FILL_SURCHARGE_TAX_CODES
*&---------------------------------------------------------------------*
*       Fill internal table with all tax codes containing TaxID 'VS',
*       i.e. all tax codes for which PAR_SUM should take effect.
*----------------------------------------------------------------------*
*      -->P_MWSKZ Tax code with TaxID 'VS'
*----------------------------------------------------------------------*
FORM fill_surcharge_tax_codes USING    p_mwskz LIKE bseg-mwskz.
  READ TABLE tab_surcharge WITH KEY mwskz = p_mwskz.
  CHECK sy-subrc <> 0.
  tab_surcharge-mwskz = p_mwskz.
  APPEND tab_surcharge.
ENDFORM.                               " FILL_SURCHARGE_TAX_CODES

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

* ---> S4 Migration - 07/07/2023 - JP
*
*      SELECT SINGLE * FROM bseg INTO *bseg
*                       WHERE bukrs = *bkpf-bukrs
*                       AND   belnr = *bkpf-belnr
*                       AND   gjahr = *bkpf-gjahr
*                       AND   buzei =  tab_bseg-rebzz.


      DATA: lv_rldnr TYPE  rldnr,
            lv_bukrs TYPE  bukrs,
            lv_belnr TYPE  belnr_d,
            lv_gjahr TYPE  gjahr,
            lv_buzei TYPE  buzei.

      lv_bukrs = *bkpf-bukrs.
      lv_belnr = *bkpf-belnr.
      lv_gjahr = *bkpf-gjahr.
      lv_buzei = tab_bseg-rebzz.

      DATA: lt_bseg_aux TYPE fagl_t_bseg.

      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = lv_rldnr
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.

      IF sy-subrc = 0.

        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr   = lv_rldnr
            i_bukrs   = lv_bukrs
            i_belnr   = lv_belnr
            i_gjahr   = lv_gjahr
            i_buzei   = lv_buzei
          IMPORTING
            et_bseg   = lt_bseg_aux
          EXCEPTIONS
            not_found = 1.
      ENDIF.

      IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ELSE.

        sy-dbcnt = lines( lt_bseg_aux ).
        READ TABLE lt_bseg_aux INTO DATA(*bseg) INDEX 1.

      ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

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

ENDFORM.                               " GET_DOWNPAYMENT_REQUESTS

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

            IF kna1-stkzn = abap_true.
              tab_adrs-stcd1 = kna1-stcd2.
            ELSE.
              tab_adrs-stcd1 = kna1-stcd1.
            ENDIF.


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

            IF lfa1-stkzn = abap_true.
              tab_adrs-stcd1 = lfa1-stcd2.
            ELSE.
              tab_adrs-stcd1 = lfa1-stcd1.
            ENDIF.

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
ENDFORM.                               " READ_ADRS
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
ENDFORM.                    " READ_CAI_AND_FISC_CONT
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
ENDFORM.                    " read_j_1apacd
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
ENDFORM.                    " READ_J_1APACK
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
ENDFORM.                    " read_cai_corr
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
ENDFORM.                               " READ_CUSTOMS_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_J_1ADRVER
*&---------------------------------------------------------------------*
*       Read version of daily VAT-Reportings for selected bukrs.
*----------------------------------------------------------------------*
*      -->BUKRS      company code                                      *
*      -->ADRVER     version                                           *
*----------------------------------------------------------------------*
FORM read_j_1adrver USING bukrs  LIKE t001-bukrs
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
ENDFORM.                               " READ_J_1ADRVER

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
ENDFORM.                               " READ_J_1ADRVERT

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
ENDFORM.                               " READ_J_1ARZTX

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
ENDFORM.                               " READ_J_1ATAXID

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
ENDFORM.                               " READ_KONP

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
ENDFORM.                               " READ_OFFICIAL_DOC_TYPE

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
ENDFORM.                               " READ_T007A

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
ENDFORM.                               " READ_T007B

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

ENDFORM.                               " CHECK_TAB_BSEG

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
    IF NOT 'C1_C2_C3' CS tab_bseg_nodk-mwskz .
      CHECK: tab_bseg_nodk-mwart      IS INITIAL,   " no tax accounts
                                                            "Note 782267
             bkpf-adisc      NE 'S'.     " deselect SD discount
    ENDIF.
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
        IF 'C1_C2_C3' CS tab_bseg_nodk-mwskz.
          IF tab_bseg_nodk-buzid <> ' '.
            mm_taxed-mwskz = tab_bseg_nodk-mwskz.
            mm_taxed-txgrp = tab_bseg_nodk-txgrp.
            COLLECT mm_taxed.
          ENDIF.
        ELSE.
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
          ENDCASE.
        ENDIF.
        " totals for manual taxes
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
ENDFORM.                               " CHECK_TAB_BSEG_NODK

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
ENDFORM.                               " CHECK_IF_EXEMPTED

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
ENDFORM.                               " CHECK_KTOSL_SELECTION

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
ENDFORM.                               " CHECK_MM_BASE_AMOUNTS

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
ENDFORM.                               " CHECK_NEWTAXID

*&---------------------------------------------------------------------*
*&      Form  CHECK_SD_AMOUNTS
*&---------------------------------------------------------------------*
FORM check_sd_amounts USING    f_subrc LIKE sy-subrc.
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

ENDFORM.                               " CHECK_SD_AMOUNTS

*&---------------------------------------------------------------------*
*&      Form  CHECK_SPECIAL_GL_AMOUNT
*&---------------------------------------------------------------------*
*&      Check sum of line totals against document total for
*&      documents with special G/L items only.
*&      Line totals are correct so far, document total not always
*&---------------------------------------------------------------------*
FORM check_special_gl_amount CHANGING p_flg_gl  TYPE c
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
ENDFORM.                               " CHECK_SPECIAL_GL_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  CHECK_SD_FOREIGN_CURRENCY
*&---------------------------------------------------------------------*
*       For SD documents only: Foreign currencies may lead to
*       rounding errors. Apply solution from FI to correct this.
*----------------------------------------------------------------------*
FORM check_sd_foreign_currency.
  DATA: max_amount(12)  TYPE p, " Biggest amount of all line entries
        comp_total(12)  TYPE p, " Sum of all line totals for document
        beleg_total(12) TYPE p, " Document total: Compare without sign
        diff_curr(12)   TYPE p, " Difference: beleg_total - comp_total
        lcl_index       LIKE sy-index,
        lcl_tabix       LIKE sy-tabix.
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
ENDFORM.                               " CHECK_SD_FOREIGN_CURRENCY

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
ENDFORM.                               " CHECK_TAX_CODE

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

ENDFORM.                               " READ_TAB_HISTORY

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

ENDFORM.                               " SET_HISTORY_LINENO

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
ENDFORM.                               " ADD_ENTRY_TO_TAB_HISTORY

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
ENDFORM.                               " UPDATE_HISTORY_TABLE

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
ENDFORM.                               " DELETE_HISTORY_TABLE

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

  DATA: vl_blart TYPE bkpf-blart.

  DATA: flg_firstline        TYPE c,
        lineno(6)            TYPE n,
        flg_print_customdata TYPE c,
        reserve_cnt1         TYPE p,
        value                LIKE tab_ep-total,
        lcl_tabix            LIKE sy-tabix.

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
    "    FORMAT: COLOR OFF,  INVERSE OFF.
    "    " write / txt_zeile7.
    "    HIDE: tab_ep-bukrs, tab_ep-gjahr, tab_ep-belnr,
    "             tab_ep-flg_sd_beleg, tab_ep-sd_vbeln,
    "***** BEGIN OF MODIFICATION *******
    "             tab_ep-flg_is_beleg.
    "***** END OF MODIFICATION *******
    "    IF NOT par_rahm IS INITIAL.
    "      ULINE.
    "    ENDIF.
    MOVE-CORRESPONDING hlp_epos1 TO e_sal.
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
    wa_history-j_1aamnt01 = wa_history-j_1aamnt01 / 100.
    ADD: tab_ep-not_taxed     TO wa_history-j_1aamnt02.
    ADD: tab_ep-vat           TO wa_history-j_1aamnt03.
    ADD: tab_ep-rnr_vat       TO wa_history-j_1aamnt04.
    ADD: tab_ep-vat_percep    TO wa_history-j_1aamnt05.
    ADD: tab_ep-other_percep  TO wa_history-j_1aamnt06.
    ADD: tab_ep-exemption     TO wa_history-j_1aamnt07.
    ADD: tab_ep-line_total    TO wa_history-j_1aamnt08.
* Note 1074703 Start
*    IF par_magn IS INITIAL.
*      ADD: tab_ep-exports       TO wa_history-j_1aamnt09.
*      ADD: tab_ep-percepnoc     TO wa_history-j_1aamnt10.
*    ELSE.
**   ADD: TAB_EP-EARN_PER      TO WA_HISTORY-J_1AAMNT06.    " Note
*685915
*      ADD: tab_ep-earn_per      TO wa_history-j_1aamnt11.
*      " Note 685915
*      ADD: tab_ep-vat_intern    TO wa_history-j_1aamnt09.
*      ADD: tab_ep-munic_per     TO wa_history-j_1aamnt10.
*    ENDIF.
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


* Note 1016337 Start
* Commenting the call to summarize the other t
*  LOOP AT tab_ep.
*    PERFORM collect_sum_tables.
*  ENDLOOP.
* Note 1016337 End

* Note 992893 start
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
      ADD:  tab_ep_oth-rnr_vat TO tab_sum_others-hwste,
            tab_ep_oth-vat_percep TO tab_sum_others-hwste,
            tab_ep_oth-exports TO tab_sum_others-hwste,
            tab_ep_oth-percepnoc TO tab_sum_others-hwste,
            tab_ep_oth-other_percep TO tab_sum_others-hwste,
            tab_ep_oth-vat_intern TO tab_sum_others-hwste,
            tab_ep_oth-munic_per TO tab_sum_others-hwste,
            tab_ep_oth-earn_per TO tab_sum_others-hwste.
      COLLECT tab_sum_others.
    ENDIF.
  ENDLOOP.
* Note 992893 end

  flg_firstline = 'X'.
  PERFORM prepare_rahmen.

  PERFORM f_armo_tabla_razao.

  LOOP AT tab_ep.
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

      "CS2016001252
      CLEAR: o_record4, o_record5,
             i_record4, i_record5.
      IF pa_vd IS NOT INITIAL.
        PERFORM fill_o_record4 CHANGING o_record4.
      ENDIF.

      IF pa_vda IS NOT INITIAL.
        PERFORM fill_o_record5 CHANGING o_record5.
      ENDIF.

      IF pa_cp IS NOT INITIAL.
        PERFORM fill_i_record4 CHANGING i_record4.
      ENDIF.

      IF pa_cpa IS NOT INITIAL.
        PERFORM fill_i_record5 CHANGING i_record5.
      ENDIF.
      "Fim CS2016001252

    ENDIF.




* Repair inconsistency for call of form collect_sum_tables:
* For compressed documents, this call was already done before
* reserve n lines was processed -> error described in note 497300

*                                                          " Note 685915
** Begin Note 658485
** Now fix error with erroneous tax totals for magn. output
*    if par_magn is initial.  " for magn. output call at different place
*      PERFORM COLLECT_SUM_TABLES.                         " Note 497300
*    endif.
** End   Note 658485

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
*      FORMAT: COLOR OFF,  INVERSE OFF.
*      " write / txt_zeile7.
** hide line infos for select document
*      HIDE: tab_ep-bukrs, tab_ep-gjahr, tab_ep-belnr,
*               tab_ep-flg_sd_beleg, tab_ep-sd_vbeln,
****** BEGIN OF MODIFICATION *******
*               tab_ep-flg_is_beleg.
***** END OF MODIFICATION *******
      MOVE-CORRESPONDING hlp_epos1 TO e_sal.
      CLEAR: flg_firstline.            " firstline is printed
    ENDIF.

* Write custom data
    IF tab_j_1adrver-j_1aproc = 1 OR tab_j_1adrver-j_1aproc = 3.
      IF flg_print_customdata IS INITIAL.
        PERFORM print_customs_data.
        flg_print_customdata = 'X'.
      ENDIF.
    ENDIF.

** Lines with the tax-code and VAT-amounts ( 1 or more for 1 document)
*    IF NOT PAR_COMP IS INITIAL. ALRS
*      WRITE TAB_EP-MWSKZ TO HLP_EPOS2-MWSKZ RIGHT-JUSTIFIED.
*    ELSE.
*      WRITE TAB_EP-MWSKZ TO HLP_EPOS2-MWSKZ.
*      WRITE TAB_EP-KTOSL TO HLP_EPOS2-MWSKZ+3.
*    ENDIF.
*
*    E_SAL-MWSKZ = HLP_EPOS2-MWSKZ.
*
** print rate
*    CLEAR: HLP_EPOS2-RATE.
*    IF PAR_COMP IS INITIAL.          " no compress ( detailed display )
*      IF TAB_EP-LINETYPE NE '1'.
*        WRITE: TAB_EP-RATE TO HLP_RATE-RATE NO-SIGN NO-GAP.
*        MOVE: '%' TO HLP_RATE-PERCENT.
*        WRITE: HLP_RATE TO HLP_EPOS2-RATE  RIGHT-JUSTIFIED.
*        E_SAL-RATE =  HLP_RATE.
*      ENDIF.
*    ELSE.                              " compress
*      IF  NOT TAB_EP-MWSKZ IS INITIAL.
*        IF TAB_EP-FLG_IS_BELEG = 'S'.
*          WRITE: TAB_EP-DSPL_RATE TO HLP_RATE-RATE NO-SIGN NO-GAP.
*        ELSE.
*          WRITE: TAB_EP-RATE TO HLP_RATE-RATE NO-SIGN NO-GAP.
*        ENDIF.
*        MOVE: '%' TO HLP_RATE-PERCENT.
*        WRITE: HLP_RATE TO HLP_EPOS2-RATE  RIGHT-JUSTIFIED.
*        E_SAL-RATE =  HLP_RATE.
*      ENDIF.
*    ENDIF.

* print tax base amount
    IF tab_ep-linetype NE '1'  AND
       par_comp IS INITIAL.
      CLEAR: hlp_epos2-taxed, e_sal-taxed.
*      write tab_ep-hwbas to hlp_epos2-taxed        no-zero
*      currency tab_001-waers.
    ELSE.
      WRITE tab_ep-taxed TO hlp_epos2-taxed        NO-ZERO
          CURRENCY tab_001-waers.
      e_sal-taxed =  tab_ep-taxed .
      e_sal-taxed =  e_sal-taxed / 100.
    ENDIF.

* Display tax base for direct VAT posting
    IF tab_ep-linetype NE '1'  AND
       NOT par_dspt IS INITIAL.
      WRITE tab_ep-hwbas TO hlp_epos2-taxed        NO-ZERO
          CURRENCY tab_001-waers.
      e_sal-taxed =  tab_ep-taxed.
    ENDIF.

* print taxes
    WRITE tab_ep-not_taxed TO hlp_epos2-not_taxed NO-ZERO
                                                 CURRENCY tab_001-waers.
    e_sal-not_taxed =  tab_ep-not_taxed .
    e_sal-not_taxed =  e_sal-not_taxed / 100 .

    WRITE tab_ep-vat TO hlp_epos2-vat NO-ZERO
                                                 CURRENCY tab_001-waers.
    e_sal-vat =  tab_ep-vat .
    e_sal-vat =  e_sal-vat / 100 .

    WRITE tab_ep-rnr_vat TO hlp_epos2-rnr_vat NO-ZERO
                                                 CURRENCY tab_001-waers.
    e_sal-rnr_vat =  tab_ep-rnr_vat .
    WRITE tab_ep-vat_percep TO hlp_epos2-vat_percep NO-ZERO
                                                 CURRENCY tab_001-waers.
    e_sal-vat_percep =  tab_ep-vat_percep.
    e_sal-vat_percep = e_sal-vat_percep / 100.

    WRITE tab_ep-other_percep TO hlp_epos2-other_percep NO-ZERO
                                                 CURRENCY tab_001-waers.
    e_sal-other_percep =  tab_ep-other_percep .
    WRITE tab_ep-exemption TO hlp_epos2-exemption NO-ZERO
                                                 CURRENCY tab_001-waers.
    e_sal-exemption =  tab_ep-exemption .
    e_sal-exemption =  e_sal-exemption / 100.

* Note 1055550 Start
* If magnetic file option internal & municiapl taxes are displayed
* If magnetic file option is not selected then export & other perception
* amount is displayed. Have commented this and displayed whichever
* amount is available.
*    IF par_magn IS INITIAL.
*      WRITE tab_ep-exports TO hlp_epos2-exports NO-ZERO
*                                                 CURRENCY tab_001-waers
*.
*      WRITE tab_ep-percepnoc TO hlp_epos2-percepnoc NO-ZERO
*                                                 CURRENCY tab_001-waers
*.
*    ELSE.
*      WRITE: tab_ep-vat_intern TO hlp_epos2-exports NO-ZERO
*                                                 CURRENCY tab_001-waers
*.
*      WRITE: tab_ep-munic_per  TO hlp_epos2-percepnoc NO-ZERO
*                                                 CURRENCY tab_001-waers
*.
*    ENDIF.
* For internal/export amount
    IF NOT tab_ep-vat_intern IS INITIAL.
      WRITE: tab_ep-vat_intern TO hlp_epos2-exports NO-ZERO
                                                 CURRENCY tab_001-waers.
      e_sal-exports = tab_ep-vat_intern.
    ELSE.
      WRITE tab_ep-exports TO hlp_epos2-exports NO-ZERO
                                                 CURRENCY tab_001-waers.
      e_sal-exports = tab_ep-exports.
    ENDIF.

    e_sal-exports = e_sal-exports / 100 .

* For municipal/other perception amount
    IF NOT tab_ep-munic_per IS INITIAL.
      WRITE: tab_ep-munic_per  TO hlp_epos2-percepnoc NO-ZERO
                                                 CURRENCY tab_001-waers.
      e_sal-percepnoc = tab_ep-munic_per .
    ELSE.
      WRITE tab_ep-percepnoc TO hlp_epos2-percepnoc NO-ZERO
                                                 CURRENCY tab_001-waers.
      e_sal-percepnoc = tab_ep-percepnoc .
    ENDIF.
* Note 1055550 End
    WRITE tab_ep-line_total TO hlp_epos2-line_total
                                                 CURRENCY tab_001-waers.
    e_sal-line_total = tab_ep-line_total.
    e_sal-line_total = e_sal-line_total / 100 .
    IF NOT par_magn IS INITIAL.
*     Note 690007
*     VALUE = TAB_EP-TOTAL * -1.
*     IF TAB_EP-EXEMPTION EQ VALUE OR
*        TAB_EP-NOT_TAXED EQ VALUE.

* Begin of note 988302
*      IF tab_ep-exemption EQ tab_ep-line_total OR
*         tab_ep-not_taxed EQ tab_ep-line_total.
*        WRITE tab_ep-j_1arfz TO hlp_epos2-exempt.
*      ELSE.
*        CLEAR hlp_epos2-exempt.
*      ENDIF.
* End of note 988302

* Begin of note 988302
      IF find_exempted = 'X'.
        WRITE tab_ep-j_1arfz TO hlp_epos2-exempt.
        e_sal-exempt = tab_ep-j_1arfz.
      ELSE.
        CLEAR hlp_epos2-exempt.
        CLEAR e_sal-exempt.
      ENDIF.
      CLEAR find_exempted .
* End of note 988302
      WRITE tab_ep-earn_per TO hlp_epos2-earn_per NO-ZERO
                                               CURRENCY tab_001-waers.
      e_sal-earn_per = tab_ep-earn_per.
    ENDIF.
    txt_zeile8 = hlp_epos2.
    listsize = listsize - 1 .
    txt_zeile8+listsize = sy-vline.
    listsize = listsize + 1 .
*    FORMAT: COLOR 2,  INVERSE ON.
*    " write / txt_zeile8.
*    FORMAT: COLOR OFF,  INVERSE OFF.
*
** print regio tab
*    AT END OF mwskz.
*      PERFORM print_tab_regio.
*    ENDAT.

    e_sal-buper = ep-buper.

    CLEAR: e_sal-taxed2, e_sal-taxed3.
    IF tab_ep-mwskz = 'C2'.
      e_sal-taxed2 = e_sal-taxed.
      CLEAR: e_sal-taxed.
    ELSEIF tab_ep-mwskz = 'C3'.
      e_sal-taxed3 = e_sal-taxed.
      CLEAR: e_sal-taxed.
    ENDIF.

    CLEAR: e_sal-iother_percep01, e_sal-iother_percep02, e_sal-iother_percep03,
*** US #181035 - MMSILVA - 10.06.2025 - Ini ***
    e_sal-iother_percep04, e_sal-iother_percep05.
*** US #181035 - MMSILVA - 10.06.2025 - Fim ***
    "CASE TAB_EP-BLART.
    "  WHEN 'AB' OR 'LM' OR 'SA'.
    LOOP AT tg_account_set.
      PERFORM f_atrib_vl_account USING e_sal tg_account_set-hkont.
    ENDLOOP.
    "ENDCASE.

    APPEND e_sal TO t_sal.

  ENDLOOP.

* Fill Perception File here
  IF NOT par_magn IS INITIAL AND
     NOT par_perf IS INITIAL.
    LOOP AT tab_p_record2 INTO p_record2.
      PERFORM print_p_record  USING p_record2.
      TRANSFER p_record2+6(94) TO par_perf LENGTH 94.
      PERFORM transfer_p_record.  "Moving Perception Record to file Note - 1120234.
    ENDLOOP.
  ENDIF.

*  IF NOT par_rahm IS INITIAL.
*    ULINE.
*  ENDIF.
ENDFORM.                               " PRINT_TAB_EP

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
ENDFORM.                               " COMPRES_TAB_EP

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
* Note 1074703 Start
*  IF par_magn IS INITIAL.
*    MOVE: tab_ep-exports       TO tab_sum_mwskz_new-exports.
*    MOVE: tab_ep-percepnoc     TO tab_sum_mwskz_new-percepnoc.
*  ELSE.
*    MOVE: tab_ep-vat_intern    TO tab_sum_mwskz_new-exports.
*    MOVE: tab_ep-munic_per     TO tab_sum_mwskz_new-percepnoc.
*  ENDIF.
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

* Note 1030388 and 1011642 has been released for
* displaying the VAT totals per region correctly.
* But note 1016337 has been released for all the
* summaries. Hence note 1042574 is released to
* revert the cahnges of the notes 1030388 and
* 1011642
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
* Note 1074703 Start
*      IF par_magn IS INITIAL.
*        MOVE: tab_ep-exports       TO tab_sum_regio-exports.
*        MOVE: tab_ep-percepnoc     TO tab_sum_regio-percepnoc.
*      ELSE.
*        MOVE: tab_ep-vat_intern    TO tab_sum_regio-exports.
*        MOVE: tab_ep-munic_per     TO tab_sum_regio-percepnoc.
*      ENDIF.
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

* Note 992893 start
**collect totals per other tax code
*  CLEAR: tab_sum_others.
*  IF NOT tab_ep-rnr_vat IS INITIAL OR
*     NOT tab_ep-vat_percep IS INITIAL OR
*     NOT tab_ep-exports IS INITIAL OR
*     NOT tab_ep-percepnoc IS INITIAL OR
*     NOT tab_ep-other_percep IS INITIAL OR
*     NOT tab_ep-vat_intern IS INITIAL OR   " magnetic output
*     NOT tab_ep-munic_per IS INITIAL OR    " magnetic output
*     NOT tab_ep-earn_per IS INITIAL.       " Note 645449
*    tab_sum_others-mwskz = tab_ep-mwskz.
*    tab_sum_others-kschl = tab_ep-kschl.
** Needed for access of text table t685t in totals for other tax totals:
*    tab_sum_others-flg_is_beleg = tab_ep-flg_is_beleg.
*    ADD:  tab_ep-rnr_vat TO tab_sum_others-hwste,
*          tab_ep-vat_percep TO tab_sum_others-hwste,
*          tab_ep-exports TO tab_sum_others-hwste,
*          tab_ep-percepnoc TO tab_sum_others-hwste,
*          tab_ep-other_percep TO tab_sum_others-hwste,
*          tab_ep-vat_intern TO tab_sum_others-hwste,
*          tab_ep-munic_per TO tab_sum_others-hwste,
*          tab_ep-earn_per TO tab_sum_others-hwste.
*    COLLECT tab_sum_others.
*  ENDIF.
* Note 992893 end

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

ENDFORM.                               " COLLECT_SUM_TABLES

*&---------------------------------------------------------------------*
*&      Form  PRINT_TAB_REGIO
*&---------------------------------------------------------------------*
FORM print_tab_regio.
  DATA: lcl_comma  TYPE c VALUE ',',
        lcl_first  TYPE c,
        lcl_pos    TYPE i,
        lcl_length TYPE i.
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
*      " write / txt_zeile8.
      PERFORM prepare_custom_rahmen.
      MOVE-CORRESPONDING hlp_epos2 TO e_sal.
      e_sal-buper = ep-buper.
      APPEND e_sal TO t_sal.
      CLEAR: lcl_first.
    ENDIF.

    listsize = listsize - 3.
    IF lcl_pos GE listsize.
*     " write / txt_zeile8.
      MOVE-CORRESPONDING hlp_epos2 TO e_sal.
      e_sal-buper = ep-buper.
      APPEND e_sal TO t_sal.
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
*    " write / txt_zeile8.
    MOVE-CORRESPONDING hlp_epos2 TO e_sal.
    e_sal-buper = ep-buper.
    APPEND e_sal TO t_sal.
  ENDIF.

ENDFORM.                               " PRINT_TAB_REGIO

*&---------------------------------------------------------------------*
*&      Form  PRINT_CUSTOMS_DATA
*&---------------------------------------------------------------------*
FORM print_customs_data.

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
*    " write / txt_zeile8.
    MOVE-CORRESPONDING hlp_epos2 TO e_sal.
    e_sal-buper = ep-buper.
    APPEND e_sal TO t_sal.
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
*    " write / txt_zeile8.
    MOVE-CORRESPONDING hlp_epos2 TO e_sal.
    e_sal-buper = ep-buper.
    APPEND e_sal TO t_sal.
  ENDIF.

ENDFORM.                               " PRINT_CUSTOMS_DATA
*&---------------------------------------------------------------------*
*&      Form  print_p_record
*&---------------------------------------------------------------------*
*       Print information from Perception record to screen
*----------------------------------------------------------------------*
*      -->p_perc_RECORD2  Perception record
*----------------------------------------------------------------------*
FORM print_p_record USING    p_perc_record TYPE type_p_record2.
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
*  " write / txt_zeile7.
  MOVE-CORRESPONDING hlp_epos1 TO e_sal.

ENDFORM.                    " print_p_record
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
      ULINE AT (up_length).
      CLEAR: hlp_epos4.
      hlp_epos4-v1 = sy-vline.
      WRITE: TEXT-t10 TO hlp_epos4+1 CENTERED.
      hlp_epos4-v4 = sy-vline.
      WRITE: / hlp_epos4.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_totals USING '4'.
      hlp_epos4-mwskz          = TEXT-v03.
      hlp_epos4-description    = TEXT-v04.
      WRITE: TEXT-v05 TO hlp_epos4-hwste RIGHT-JUSTIFIED.
      WRITE: / hlp_epos4.
      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    hlp_epos4-mwskz = tab_sum_mwskz-mwskz.
    PERFORM read_t007a USING  tab_001-kalsm tab_sum_mwskz-mwskz.
    hlp_epos4-description = tab_007a-text1.
    WRITE tab_sum_mwskz-hwste TO hlp_epos4-hwste CURRENCY tab_001-waers.
    WRITE: / hlp_epos4.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_MWSKZ

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
      ULINE AT (up_length).
      CLEAR: hlp_epos3a.
      hlp_epos3a-v1 = sy-vline.
      WRITE: TEXT-t10 TO hlp_epos3a+1 CENTERED.
      hlp_epos3a-v13 = sy-vline.
      WRITE: / hlp_epos3a.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_line.
      MOVE: TEXT-v03 TO hlp_epos3a-txt_sum .
      hlp_epos3a-description    = TEXT-v04.
      WRITE: p_col2 TO hlp_epos3a-taxed  RIGHT-JUSTIFIED.
*      WRITE: P_COL22 TO HLP_EPOS3A-TAXED2 RIGHT-JUSTIFIED.
*      WRITE: P_COL23 TO HLP_EPOS3A-TAXED3 RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3a-not_taxed RIGHT-JUSTIFIED."Note 454626
      WRITE: p_col4 TO hlp_epos3a-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3a-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3a-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3a-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3a-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3a-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3a-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3a-line_total RIGHT-JUSTIFIED.

      WRITE: / hlp_epos3a.
      ULINE AT /(up_length).
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
    WRITE: / hlp_epos3a.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_MWSKZ_NEW
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
      ULINE AT (up_length).
      CLEAR: hlp_epos5.
      hlp_epos5-v1 = sy-vline.
      WRITE: TEXT-t15 TO hlp_epos5+1 CENTERED.
      hlp_epos5-v3 = sy-vline.
      WRITE: / hlp_epos5.
      ULINE AT /(up_length).
      CLEAR: hlp_epos5.
      PERFORM prepare_rahmen_totals USING '5'.
      MOVE: TEXT-u11 TO hlp_epos5-satz .
      WRITE: TEXT-v05 TO hlp_epos5-hwste RIGHT-JUSTIFIED.
      WRITE: / hlp_epos5.
      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    rate = tab_sum_rate-rate.
    WRITE: rate TO hlp_rate-rate .
    MOVE: '%' TO hlp_rate-percent.
    WRITE: hlp_rate TO hlp_epos5-satz.
    WRITE tab_sum_rate-hwste TO hlp_epos5-hwste CURRENCY tab_001-waers.
    WRITE: / hlp_epos5.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_RATE
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
      ULINE AT (up_length).
      CLEAR: hlp_epos3.
      hlp_epos3-v1 = sy-vline.
      WRITE: TEXT-t20 TO hlp_epos3+1 CENTERED.
*     HLP_EPOS3-V13 = SY-VLINE.                    " Note 685915
      hlp_epos3-v14 = sy-vline.                    " Note 685915
      WRITE: / hlp_epos3.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_line.
      MOVE: TEXT-v06 TO hlp_epos3-txt_sum .
      WRITE: p_col2 TO hlp_epos3-taxed RIGHT-JUSTIFIED.
*      WRITE: P_COL22 TO HLP_EPOS3-TAXED2 RIGHT-JUSTIFIED.
*      WRITE: P_COL23 TO HLP_EPOS3-TAXED3 RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3-not_taxed RIGHT-JUSTIFIED.
      WRITE: p_col4 TO hlp_epos3-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3-line_total RIGHT-JUSTIFIED.

      WRITE: / hlp_epos3.
      ULINE AT /(up_length).
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
    WRITE: / hlp_epos3.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_OFTPT

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
      ULINE AT (up_length).
      CLEAR: hlp_epos6.
      hlp_epos6-v1 = sy-vline.
      WRITE: TEXT-t25 TO hlp_epos6+1 CENTERED.
      hlp_epos6-v6 = sy-vline.
      WRITE: / hlp_epos6.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_totals  USING '6'.
      hlp_epos6-hkont          = TEXT-v09.
      hlp_epos6-description    = TEXT-v04.
      WRITE: TEXT-v10 TO hlp_epos6-soll RIGHT-JUSTIFIED.
      WRITE: TEXT-v11 TO hlp_epos6-haben RIGHT-JUSTIFIED.
      WRITE: TEXT-v12 TO hlp_epos6-saldo RIGHT-JUSTIFIED.
      WRITE: / hlp_epos6.
      ULINE AT /(up_length).
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
    WRITE: / hlp_epos6.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_ACCOUNT

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
      ULINE AT (up_length).
      CLEAR: hlp_epos4.
      hlp_epos4-v1 = sy-vline.
      WRITE: TEXT-t30 TO hlp_epos4+1 CENTERED.
      hlp_epos4-v4 = sy-vline.
      WRITE: / hlp_epos4.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_totals USING '4'.
      hlp_epos4-mwskz          = TEXT-v03.
      hlp_epos4-description    = TEXT-v04.
      MOVE: TEXT-v05 TO hlp_epos4-hwste.
      WRITE: / hlp_epos4.
      ULINE AT /(up_length).
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
    WRITE: / hlp_epos4.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_OTHERS

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
      ULINE AT (up_length).
      CLEAR: hlp_epos3b.
      hlp_epos3b-v0 = sy-vline.
      WRITE: TEXT-t11 TO hlp_epos3b+1 CENTERED.
      hlp_epos3b-v13 = sy-vline.
      WRITE: / hlp_epos3b.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_sum_regio.
      MOVE: TEXT-p01 TO hlp_epos3b-regio .
      MOVE: TEXT-u10 TO hlp_epos3b-txt_sum .
      hlp_epos3b-description    = TEXT-v04.
      WRITE: p_col2  TO hlp_epos3b-taxed RIGHT-JUSTIFIED.
*      WRITE: P_COL22 TO HLP_EPOS3B-TAXED2 RIGHT-JUSTIFIED.
*      WRITE: P_COL23 TO HLP_EPOS3B-TAXED3 RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3b-not_taxed RIGHT-JUSTIFIED."Note 454626
      WRITE: p_col4 TO hlp_epos3b-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3b-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3b-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3b-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3b-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3b-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3b-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3b-line_total RIGHT-JUSTIFIED.

      WRITE: / hlp_epos3b.
      ULINE AT /(up_length).
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
    WRITE: / hlp_epos3b.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_SUM_REGIO

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
      ULINE AT (up_length).
      CLEAR: hlp_epos10.
      hlp_epos10-v1 = sy-vline.
      WRITE: TEXT-t40 TO hlp_epos10+1 CENTERED.
      hlp_epos10-v3 = sy-vline.
      WRITE: / hlp_epos10.
      ULINE AT /(up_length).
      CLEAR: hlp_epos10.
      hlp_epos10-v1 = hlp_epos10-v2 = hlp_epos10-v3 = sy-vline.
      WRITE: TEXT-t41 TO hlp_epos10-branch  LEFT-JUSTIFIED.
      WRITE: TEXT-t42 TO hlp_epos10-no_recs LEFT-JUSTIFIED.
      WRITE: / hlp_epos10.
      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 3 INTENSIFIED.
    WRITE: tab_branch-branch  TO hlp_epos10-branch RIGHT-JUSTIFIED.
    WRITE: tab_branch-no_recs TO hlp_epos10-no_recs NO-ZERO.
    WRITE: / hlp_epos10.
    ULINE AT /(up_length).
  ENDLOOP.
* no_of_rec contains total number of records
  FORMAT COLOR 1 INTENSIFIED.
  RESERVE 2 LINES.
  WRITE: TEXT-t43 TO hlp_epos10+1(30).
  WRITE: no_of_rec TO hlp_epos10+31(12) RIGHT-JUSTIFIED.
  WRITE: / hlp_epos10.
  ULINE AT /(up_length).
ENDFORM.                               " PRINT_TAB_BRANCH

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
ENDFORM.                               " INSERT_BELNR_TO_LOG

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
      ULINE AT (up_length).
      CLEAR: hlp_epos7.
      hlp_epos7-v1 = sy-vline.
      WRITE: TEXT-v94 TO hlp_epos7+1 CENTERED.
      hlp_epos7-v4 = sy-vline.
      hlp_epos7-v5 = sy-vline.
      WRITE: / hlp_epos7.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_logs.
      hlp_epos7-name           = TEXT-v76.
      hlp_epos7-key            = TEXT-v14.
      hlp_epos7-description    = TEXT-v04.
      hlp_epos7-belnr          = TEXT-u04.
      WRITE: / hlp_epos7.
      ULINE AT /(up_length).
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

    WRITE: / hlp_epos7.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_LOG_ENTRY10

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
      ULINE AT (up_length).
      CLEAR: hlp_epos8.
      hlp_epos8-v1 = sy-vline.
      WRITE: TEXT-v96 TO hlp_epos8+1 CENTERED.
      hlp_epos8-v5 = sy-vline.
      WRITE: / hlp_epos8.
      ULINE AT /(up_length).
      PERFORM prepare_rahmen_logs.
      hlp_epos8-belnr         = TEXT-u03.
      hlp_epos8-blart         = TEXT-v15.
      hlp_epos8-budat         = TEXT-u02.
      hlp_epos8-description   = TEXT-v04.
      WRITE: / hlp_epos8.
      ULINE AT /(up_length).
      CLEAR:   flg_new_page.
    ENDIF.
    FORMAT COLOR 2.
    hlp_epos8-belnr         = tab_log_entry20-belnr.
    hlp_epos8-blart         = tab_log_entry20-blart.
    WRITE: tab_log_entry20-budat TO hlp_epos8-budat DD/MM/YY NO-ZERO.
    hlp_epos8-description   = tab_log_entry20-description.
    WRITE: / hlp_epos8.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_LOG_ENTRY20
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
      ULINE AT (up_length).
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
      WRITE: / hlp_epos3.
      ULINE AT /(up_length).
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
      WRITE: hlp_epos3.
* 2nd line header
      CLEAR: hlp_epos3-txt_sum.
      WRITE: p_col2  TO hlp_epos3-taxed  RIGHT-JUSTIFIED.
*      WRITE: P_COL22 TO HLP_EPOS3-TAXED2 RIGHT-JUSTIFIED.
*      WRITE: P_COL23 TO HLP_EPOS3-TAXED3 RIGHT-JUSTIFIED.
      WRITE: p_col3 TO hlp_epos3-not_taxed RIGHT-JUSTIFIED.
      WRITE: p_col4 TO hlp_epos3-vat RIGHT-JUSTIFIED.
      WRITE: p_col5 TO hlp_epos3-rnr_vat RIGHT-JUSTIFIED.
      WRITE: p_col6 TO hlp_epos3-vat_percep RIGHT-JUSTIFIED.
      WRITE: p_col7 TO hlp_epos3-other_percep RIGHT-JUSTIFIED.
      WRITE: p_col8 TO hlp_epos3-exemption RIGHT-JUSTIFIED.
      WRITE: p_col09 TO hlp_epos3-exports RIGHT-JUSTIFIED.
      WRITE: p_col10 TO hlp_epos3-percepnoc RIGHT-JUSTIFIED.
      WRITE: p_col11 TO hlp_epos3-line_total RIGHT-JUSTIFIED.
      WRITE: / hlp_epos3.
      ULINE AT /(up_length).
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
    WRITE: / hlp_epos3.
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
    WRITE: / hlp_epos3.
    ULINE AT /(up_length).
  ENDLOOP.
  CLEAR: flg_new_page.

ENDFORM.                               " PRINT_TAB_HISTORY

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
ENDFORM.                               " PREPARE_CUSTOM_RAHMEN

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
    up_length(2)  TYPE p,
    up_offset(2)  TYPE p,
    vcompany(245).

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
  CONCATENATE tab_001-name1 '-' tab_001-ort01 INTO vcompany SEPARATED BY space.
  WRITE: vcompany TO txt_zeile2+up_offset.
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

** 5rd line
*  TXT_ZEILE41 = SY-VLINE.
*  WRITE:  TAB_001-ORT01 TO TXT_ZEILE41+1.
*  DESCRIBE FIELD TAB_001-ORT01 LENGTH UP_LENGTH IN CHARACTER MODE.
*  UP_LENGTH = UP_LENGTH + 1.
*  CONCATENATE 'Ledger:' '0l' into VLEDGER SEPARATED BY space.
*  WRITE: VLEDGER  TO TXT_ZEILE41+UP_LENGTH.

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

* Second header line ALRS
*  WRITE: TEXT-U10 TO HLP_EPOS2-MWSKZ RIGHT-JUSTIFIED.
*  WRITE: TEXT-U11 TO HLP_EPOS2-RATE RIGHT-JUSTIFIED.
  IF NOT par_comp IS INITIAL.
    WRITE: p_col2 TO hlp_epos2-taxed RIGHT-JUSTIFIED.
*    WRITE: P_COL22 TO HLP_EPOS2-TAXED2 RIGHT-JUSTIFIED.
*    WRITE: P_COL23 TO HLP_EPOS2-TAXED3 RIGHT-JUSTIFIED.
  ELSE.
    WRITE: p_col2 TO hlp_epos2-taxed RIGHT-JUSTIFIED.
*    WRITE: P_COL22 TO HLP_EPOS2-TAXED2 RIGHT-JUSTIFIED.
*    WRITE: P_COL23 TO HLP_EPOS2-TAXED3 RIGHT-JUSTIFIED.
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

ENDFORM.                               " PREPARE_LIST_HEADER
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

ENDFORM.                               " PREPARE_PAGE_TOTAL

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN
*&---------------------------------------------------------------------*
*       Move sy-vline to help table
*----------------------------------------------------------------------*
FORM prepare_rahmen.
  CLEAR: hlp_epos1, hlp_epos2, hlp_epos1s.
  hlp_epos1-v1 =  hlp_epos1-v16 = sy-vline.
  hlp_epos1s-v1 =  hlp_epos1s-v10 = sy-vline.
*  HLP_EPOS2-V1 = HLP_EPOS2-V13 = SY-VLINE. ALRS
  hlp_epos2-v13 = sy-vline.
  IF NOT par_rahm IS INITIAL.
    hlp_epos1-v2 = hlp_epos1-v3 = hlp_epos1-v4 =
    hlp_epos1-v5 = hlp_epos1-v6 = hlp_epos1-v7 = hlp_epos1-v8 =
    hlp_epos1-v9 = hlp_epos1-v10 = hlp_epos1-v11 = hlp_epos1-v12 =
    hlp_epos1-v13 = hlp_epos1-v14 = hlp_epos1-v15 = sy-vline.
    IF NOT par_magn IS INITIAL.      " list format for magnetic output
      hlp_epos1-total+15(1) = hlp_epos1-total+34(1) = sy-vline.
    ENDIF.
*    HLP_EPOS2-V2 = HLP_EPOS2-V3 = HLP_EPOS2-V4 = ALRS
    hlp_epos2-v3 = hlp_epos2-v4 =
    hlp_epos2-v5 = hlp_epos2-v6 = hlp_epos2-v7 = hlp_epos2-v8 =
    hlp_epos2-v9 = hlp_epos2-v10 = sy-vline.
    hlp_epos2-v11 = hlp_epos2-v12 = sy-vline.
    hlp_epos1s-v2 = hlp_epos1s-v3 = hlp_epos1s-v4 =
    hlp_epos1s-v5 = hlp_epos1s-v6 = hlp_epos1s-v7 = sy-vline.
  ENDIF.

ENDFORM.                               " PREPARE_RAHMEN

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
ENDFORM.                               " PREPARE_RAHMEN_SUM_LINE

*&---------------------------------------------------------------------*
*&      Form  PREPARE_RAHMEN_TOTALS
*&---------------------------------------------------------------------*
FORM prepare_rahmen_totals USING  number.
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

ENDFORM.                               " PREPARE_RAHMEN_TOTALS

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
ENDFORM.                               " PREPARE_RAHMEN_LOGS

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
ENDFORM.                               " PREPARE_RAHMEN_SUM_REGIO

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
ENDFORM.                               " PREPARE_MAGNETIC_OUTPUT
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

ENDFORM.                               " FILL_I_RECORD3

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
ENDFORM.                               "

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

ENDFORM.                               " FILL_I_RECORD2

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

ENDFORM.                               " FILL_O_RECORD2


FORM fill_o_record4 CHANGING p_o_record4 LIKE o_record4.

  DATA: lcl_tfill      LIKE sy-tfill,
        lcl_stdat      LIKE bkpf-budat,           " Cancellation date
        lcl_cmp_exempt LIKE o_record4-total_amnt. " Note 690007

  CHECK tab_ep-koart = 'D'.

  p_o_record4-docu_date   = tab_ep-bldat.

  "Se branch=0006 =(OFF=NCA=3 SENÃO 2) Se branch = 0005 = 19
  IF tab_ep-brnch = '0006'.
    IF tab_ep-oftp_text = 'NCA'.
      p_o_record4-docu_type = '003'.
    ELSE.
      p_o_record4-docu_type = '002'.
    ENDIF.
  ELSEIF tab_ep-brnch = '0005'.
    p_o_record4-docu_type = '019'.
  ENDIF.

  p_o_record4-brnch             = tab_ep-brnch.
  p_o_record4-docu_no           = tab_ep-xblnr+5(8).
  p_o_record4-docu_no1          = tab_ep-xblnr+5(8).
  p_o_record4-doc_tp_vendor     = '80'.

  IF tab_ep-stcdt = '99'.
    CLEAR: p_o_record4-id_docu_no.
    p_o_record4-name              = TEXT-u32.
  ELSE.
    p_o_record4-id_docu_no        = tab_ep-stcd1.
    p_o_record4-name              = tab_ep-name1.
  ENDIF.

  p_o_record4-total_amnt          = tab_ep-total.
  p_o_record4-n_taxed_amnt        = 0."TAB_EP-NOT_TAXED.
  p_o_record4-percept_no_cat      = 0.
  p_o_record4-exempt_amnt         = tab_ep-exemption.
  p_o_record4-perce_amnt          = 0."TAB_EP-VAT_PERCEP.
  p_o_record4-percept_buenos_caba = tab_ep-iother_percep01 + tab_ep-iother_percep02.
  p_o_record4-perc_mp_amnt        = 0."TAB_EP-MUNIC_PER
  p_o_record4-int_tax_amnt        = 0."TAB_EP-VAT_INTERN

  PERFORM fill_curr_info.
  PERFORM fill_exch_rate USING     tab_ep-waers
                         CHANGING  gf_exch_rate.
  p_o_record4-curr_code         = 'PES'. "TAB_CURR-ALTKEY..

  p_o_record4-exch_rate         = '1'. "GF_EXCH_RATE.
  p_o_record4-sev_vat_rates     = no_of_rates.

  IF p_o_record4-exempt_amnt IS NOT INITIAL.
    p_o_record4-cd_operacao       = 'E'.
  ENDIF.

  p_o_record4-other_percept      = 0."TAB_EP-OTHER_PERCEP.
  p_o_record4-fecha_vcto         = ''.

  wa_v_record-record = p_o_record4.
  APPEND wa_v_record TO tab_v_record.

ENDFORM.                               " FILL_O_RECORD4

FORM fill_i_record4 CHANGING p_i_record4 LIKE i_record4.

  DATA: lcl_tfill      LIKE sy-tfill,
        lcl_stdat      LIKE bkpf-budat,           " Cancellation date
        lcl_cmp_exempt LIKE o_record4-total_amnt. " Note 690007

  CHECK tab_ep-koart = 'K'.

  p_i_record4-docu_date   = tab_ep-bldat.

  "Se branch=0006 =(OFF=NCA=3 SENÃO 2) Se branch = 0005 = 19
  IF tab_ep-brnch = '0006'.
    IF tab_ep-oftp_text = 'NCA'.
      p_i_record4-docu_type = '003'.
    ELSE.
      p_i_record4-docu_type = '002'.
    ENDIF.
  ELSEIF tab_ep-brnch = '0005'.
    p_i_record4-docu_type = '019'.
  ENDIF.

  p_i_record4-brnch             = tab_ep-brnch.
  p_i_record4-docu_no           = tab_ep-xblnr+5(8).
  p_i_record4-dp_imp            = ''.
  p_i_record4-doc_tp_vendor     = '80'.

  IF tab_ep-stcdt = '99'.
    CLEAR: p_i_record4-id_docu_no.
    p_i_record4-name              = TEXT-u32.
  ELSE.
    p_i_record4-id_docu_no        = tab_ep-stcd1.
    p_i_record4-name              = tab_ep-name1.
  ENDIF.

  p_i_record4-total_amnt          = tab_ep-total.
  p_i_record4-n_taxed_amnt        = tab_ep-not_taxed.
  p_i_record4-exempt_amnt         = tab_ep-exemption.
  p_i_record4-vat_percep          = tab_ep-vat_percep.
  p_i_record4-perce_amnt          = 0."TAB_EP-VAT_PERCEP.
  p_i_record4-percept_buenos_caba = tab_ep-iother_percep01 + tab_ep-iother_percep02.
  p_i_record4-perc_mp_amnt        = 0."TAB_EP-MUNIC_PER
  p_i_record4-int_tax_amnt        = 0."TAB_EP-VAT_INTERN

  PERFORM fill_curr_info.
  PERFORM fill_exch_rate USING     tab_ep-waers
                         CHANGING  gf_exch_rate.
  p_i_record4-curr_code         = 'PES'. "TAB_CURR-ALTKEY.

  p_i_record4-exch_rate         = '1'. "GF_EXCH_RATE.
  p_i_record4-sev_vat_rates     = no_of_rates.

  IF p_i_record4-exempt_amnt IS NOT INITIAL.
    p_i_record4-cd_operacao       = 'E'.
  ENDIF.

  p_i_record4-vat = tab_ep-vat.

  p_i_record4-other_percept      = 0."TAB_EP-OTHER_PERCEP.

  p_i_record4-id_emi             = ''.
  p_i_record4-name_emi           = ''.
  p_i_record4-vat_com            = ''.

  wa_c_record-record = p_i_record4.
  APPEND wa_c_record TO tab_c_record.

ENDFORM.                               " FILL_O_RECORD4

FORM fill_o_record5 CHANGING p_o_record5 LIKE o_record5.

  DATA: lcl_tfill      LIKE sy-tfill,
        lcl_stdat      LIKE bkpf-budat,           " Cancellation date
        lcl_cmp_exempt LIKE o_record4-total_amnt. " Note 690007

  CHECK tab_ep-koart = 'D'.

  "Se branch=0006 =(OFF=NCA=3 SENÃO 2) Se branch = 0005 = 19
  IF tab_ep-brnch = '0006'.
    IF tab_ep-oftp_text = 'NCA'.
      p_o_record5-docu_type = '003'.
    ELSE.
      p_o_record5-docu_type = '002'.
    ENDIF.
  ELSEIF tab_ep-brnch = '0005'.
    p_o_record5-docu_type = '019'.
  ENDIF.

  p_o_record5-brnch             = tab_ep-brnch.
  p_o_record5-docu_no           = tab_ep-xblnr+5(8).
  p_o_record5-taxed             = tab_ep-taxed.

  IF tab_ep-mwskz = 'C1'.
    p_o_record5-alic_iva = '0005'.
  ELSEIF tab_ep-mwskz = 'C2'.
    p_o_record5-alic_iva = '0004'.
  ELSEIF tab_ep-mwskz = 'C3'.
    p_o_record5-alic_iva = '0006'.
  ENDIF.

  p_o_record5-tax_liq =  tab_ep-vat.

  wa_va_record-record = p_o_record5.
  APPEND wa_va_record TO tab_va_record.

ENDFORM.

FORM fill_i_record5 CHANGING p_i_record5 LIKE i_record5.

  DATA: lcl_tfill      LIKE sy-tfill,
        lcl_stdat      LIKE bkpf-budat,           " Cancellation date
        lcl_cmp_exempt LIKE o_record4-total_amnt. " Note 690007

  CHECK tab_ep-koart = 'K'.

  "Se branch=0006 =(OFF=NCA=3 SENÃO 2) Se branch = 0005 = 19
  IF tab_ep-brnch = '0006'.
    IF tab_ep-oftp_text = 'NCA'.
      p_i_record5-docu_type = '003'.
    ELSE.
      p_i_record5-docu_type = '002'.
    ENDIF.
  ELSEIF tab_ep-brnch = '0005'.
    p_i_record5-docu_type = '019'.
  ENDIF.

  p_i_record5-brnch             = tab_ep-brnch.
  p_i_record5-docu_no           = tab_ep-xblnr+5(8).
  p_i_record5-doc_tp_vendor     = '80'.
  IF tab_ep-stcdt = '99'.
    CLEAR: p_i_record5-id_docu_no.
  ELSE.
    p_i_record5-id_docu_no        = tab_ep-stcd1.
  ENDIF.
  p_i_record5-taxed             = tab_ep-taxed.

  IF tab_ep-mwskz = 'C1'.
    p_i_record5-alic_iva = '0005'.
  ELSEIF tab_ep-mwskz = 'C2'.
    p_i_record5-alic_iva = '0004'.
  ELSEIF tab_ep-mwskz = 'C3'.
    p_i_record5-alic_iva = '0006'.
  ENDIF.

  p_i_record5-tax_liq =  tab_ep-vat.

  wa_ca_record-record = p_i_record5.
  APPEND wa_ca_record TO tab_ca_record.

ENDFORM.


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

ENDFORM.                               " FILL_P_RECORD
*&---------------------------------------------------------------------*
*&      Form  FILL_SUMS_I_RECORD3
*&---------------------------------------------------------------------*
FORM fill_sums_i_record3 USING    p_p_i_record2 TYPE type_ep
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
ENDFORM.                    " FILL_SUMS_I_RECORD3
*&---------------------------------------------------------------------*
*&      Form  FILL_SUMS_O_RECORD3
*&---------------------------------------------------------------------*
FORM fill_sums_o_record3 USING    p_p_o_record2   TYPE type_ep
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
ENDFORM.                    " FILL_SUMS_O_RECORD3
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_RECORD
*&---------------------------------------------------------------------*
*       Writes one record to the file on the application server
*----------------------------------------------------------------------*
*      -->P_RECORD     Record of any type                              *
*----------------------------------------------------------------------*
FORM transfer_record USING    p_record
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
ENDFORM.                               " TRANSFER_RECORD
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
ENDFORM.                               " FILL_CURR_INFO
*&---------------------------------------------------------------------*
*&      Form  FILL_EXCH_RATE
*&---------------------------------------------------------------------*
*       Fill exchange rate for foreign currencies.
*----------------------------------------------------------------------*
*       -->P_WAERS currency key
*       <--P_RATE  exchange rate
*----------------------------------------------------------------------*
FORM fill_exch_rate USING    p_waers    LIKE bkpf-waers
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
ENDFORM.                               " FILL_EXCH_RATE
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
ENDFORM.                               " TRANSFER_LOCAL
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

ENDFORM.                               " CHECK_FOREIGN_SD_DOCS

*&---------------------------------------------------------------------*
*&      Form  check_for_zero_line
*&---------------------------------------------------------------------*
*       If all amounts and the rate are zero: Don't save record to file
*----------------------------------------------------------------------*
FORM check_for_zero_line USING    p_ep    TYPE type_ep
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

ENDFORM.                    " check_for_zero_line

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

ENDFORM.                    " TRANSFER_P_RECORD
*&---------------------------------------------------------------------*
*&      Form  collect_sum_account
*&---------------------------------------------------------------------*
*  Tab_sum_account - global value
FORM collect_sum_account .
* collect totals per tax account
  CLEAR: tab_sum_account.
  tab_sum_account-hkont = tab_ep-hkont.
  tab_sum_account-saldo = tab_ep-hwste.
* Note 685915
  IF tab_j_1adrver-j_1aproc EQ 1 OR
     tab_j_1adrver-j_1aproc EQ 3.
    IF tab_ep-hwste < 0.
      tab_sum_account-haben = tab_ep-hwste * -1.
    ELSE.
      tab_sum_account-soll = tab_ep-hwste.
    ENDIF.
  ELSE.
    IF tab_ep-hwste < 0.
      tab_sum_account-soll = tab_ep-hwste * -1.
    ELSE.
      tab_sum_account-haben = tab_ep-hwste.
    ENDIF.
  ENDIF.
  COLLECT tab_sum_account.

ENDFORM.                    " collect_sum_account

* GENERA SALIDA ALV
*----------------------------------------------------------------------*
*  Form  F_GENERA_ALV
*----------------------------------------------------------------------*
FORM f_genera_alv.

* Textos
  PERFORM prepare_list_header.

* Columnas ALV
  PERFORM f_cols_alv.

* Quiebra ALV
  PERFORM f_campos_quiebra_alv.

* Eventos ALV
  PERFORM f_eventos_alv.

* Tabla de salida ALV
  PERFORM f_tabla_alv.

* Layout ALV
  PERFORM f_layout_alv.

* Ejecución informe ALV
  PERFORM f_salida_alv.

ENDFORM.                    " F_GENERA_ALV

*----------------------------------------------------------------------*
* Form  f_cols_alv
*----------------------------------------------------------------------*
FORM f_cols_alv .

  DATA: v_text TYPE c LENGTH 35,
        t_cabe TYPE STANDARD TABLE OF string,
        e_cabe LIKE LINE OF t_cabe,
        v_line LIKE v_pos,
        v_chng TYPE c.

  SPLIT txt_zeile5 AT '|' INTO TABLE t_cabe.

* Columnas ALV
  CLEAR v_pos.
  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'LINENO' '' '' '' '' 'C' '' '' v_text '' 'X'
   '' '' '' '6' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'BUDAT' '' '' '' '' 'C' '' '' v_text '' 'X'
   '' '' '' '8' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'BRNCH' '' '' '' '' 'C' '' '' v_text '' 'X'
   '' '' '' '4' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'BELNR' '' '' '' '' 'C' '' '' v_text '' 'X'
   '' '' '' '10' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'KOART' '' '' '' '' 'C' '' '' v_text '' ''
   '' 'C300' '' '1' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'HKONT' '' '' '' '' 'C' '' '' v_text '' ''
   '' 'C300' '' '10' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'NAME1' '' '' '' '' 'L' '' '' v_text '' ''
   '' 'C300' '' '35' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'STCDT' '' '' '' '' 'C' '' '' v_text '' ''
   '' '' '' '2' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'STCD1' '' '' '' '' 'L' '' '' v_text '' ''
   '' '' '' '16' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'BLDAT' '' '' '' '' 'C' '' '' v_text '' ''
   '' '' '' '8' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'XBLNR' '' '' '' '' 'C' '' '' v_text '' ''
   '' '' '' '14' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'OFTP_TEXT' '' '' '' '' 'L' '' '' v_text '' ''
   '' '' '' '35' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'AUGDT' '' '' '' '' 'C' '' '' v_text '' ''
   '' '' '' '8' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'AUGBL' '' '' '' '' 'C' '' '' v_text '' ''
   '' '' '' '10' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      v_text = e_cabe.
      IF v_text IS INITIAL.
        SHIFT e_cabe LEFT DELETING LEADING space.
        v_text = e_cabe.
      ENDIF.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        v_text = e_cabe.
        IF v_text IS INITIAL.
          SHIFT e_cabe LEFT DELETING LEADING space.
          v_text = e_cabe.
        ENDIF.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  PERFORM f_carga_campos_col USING :
   v_pos 'TOTAL' '' '' '' '' 'R' '' '' v_text '' ''
   '' '' '' '20' '' '' '' '' ''.

*  ADD 1 TO v_pos.
*
*  DO.
*    ADD 1 TO v_line.
*    READ TABLE t_cabe INTO e_cabe INDEX v_line.
*    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
*      v_text = e_cabe.
*      IF v_text IS INITIAL.
*        SHIFT e_cabe LEFT DELETING LEADING space.
*        v_text = e_cabe.
*      ENDIF.
*      EXIT.
*    ELSEIF sy-subrc NE 0.
*      v_chng = 'X'.
*      CLEAR v_line.
*      EXIT.
*    ENDIF.
*  ENDDO.
*
*  IF v_chng EQ 'X'.
*    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
*    DO.
*      ADD 1 TO v_line.
*      READ TABLE t_cabe INTO e_cabe INDEX v_line.
*      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
*        v_text = e_cabe.
*        IF v_text IS INITIAL.
*          SHIFT e_cabe LEFT DELETING LEADING space.
*          v_text = e_cabe.
*        ENDIF.
*        EXIT.
*      ELSEIF sy-subrc NE 0.
*        v_chng = 'X'.
*        CLEAR v_line.
*        EXIT.
*      ENDIF.
*    ENDDO.
*  ENDIF.
*
*  PERFORM f_carga_campos_col USING :
*   v_pos 'CAI' '' '' '' '' 'R' '' '' v_text '' ''
*   '' '' '' '14' '' '' '' '' ''.
*
*  ADD 1 TO v_pos.
*
*  DO.
*    ADD 1 TO v_line.
*    READ TABLE t_cabe INTO e_cabe INDEX v_line.
*    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
*      v_text = e_cabe.
*      IF v_text IS INITIAL.
*        SHIFT e_cabe LEFT DELETING LEADING space.
*        v_text = e_cabe.
*      ENDIF.
*      EXIT.
*    ELSEIF sy-subrc NE 0.
*      v_chng = 'X'.
*      CLEAR v_line.
*      EXIT.
*    ENDIF.
*  ENDDO.
*
*  IF v_chng EQ 'X'.
*    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
*    DO.
*      ADD 1 TO v_line.
*      READ TABLE t_cabe INTO e_cabe INDEX v_line.
*      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
*        v_text = e_cabe.
*        IF v_text IS INITIAL.
*          SHIFT e_cabe LEFT DELETING LEADING space.
*          v_text = e_cabe.
*        ENDIF.
*        EXIT.
*      ELSEIF sy-subrc NE 0.
*        v_chng = 'X'.
*        CLEAR v_line.
*        EXIT.
*      ENDIF.
*    ENDDO.
*  ENDIF.
*
*  PERFORM f_carga_campos_col USING :
*   v_pos 'FISC_CONT' '' '' '' '' 'C' '' '' v_text '' ''
*   '' '' '' '1' '' '' '' '' ''.

  ADD 1 TO v_pos.

  DO.
    ADD 1 TO v_line.
    READ TABLE t_cabe INTO e_cabe INDEX v_line.
    IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
      SHIFT e_cabe LEFT DELETING LEADING space.
      v_text = e_cabe.
      EXIT.
    ELSEIF sy-subrc NE 0.
      v_chng = 'X'.
      CLEAR v_line.
      EXIT.
    ENDIF.
  ENDDO.

  IF v_chng EQ 'X'.
    SPLIT txt_zeile6 AT '|' INTO TABLE t_cabe.
    DO.
      ADD 1 TO v_line.
      READ TABLE t_cabe INTO e_cabe INDEX v_line.
      IF sy-subrc EQ 0 AND NOT e_cabe IS INITIAL.
        SHIFT e_cabe LEFT DELETING LEADING space.
        v_text = e_cabe.
        EXIT.
      ELSEIF sy-subrc NE 0.
        v_chng = 'X'.
        CLEAR v_line.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

*  PERFORM F_CARGA_CAMPOS_COL USING :
*   V_POS 'MWSKZ ' '' '' '' '' 'C' '' '' V_TEXT '' ''
*   '' '' '' '6' '' '' '' '' ''.
*  ADD 1 TO V_POS.
*  V_TEXT = 'Tipo'.
*  PERFORM F_CARGA_CAMPOS_COL USING :
*   V_POS 'RATE' '' '' '' '' 'R' '' '' V_TEXT '' ''
*   '' '' '' '8' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col2.
  PERFORM f_carga_campos_col USING :
   v_pos 'TAXED' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  "IF PAR_ALV = 'X'.
  ADD 1 TO v_pos.
  v_text = p_col22.
  PERFORM f_carga_campos_col USING :
   v_pos 'TAXED2' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col23.
  PERFORM f_carga_campos_col USING :
   v_pos 'TAXED3' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.
  " ENDIF.

  ADD 1 TO v_pos.
  v_text = p_col3.
  PERFORM f_carga_campos_col USING :
   v_pos 'NOT_TAXED' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col4.
  PERFORM f_carga_campos_col USING :
   v_pos 'VAT' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '19' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col5.
  PERFORM f_carga_campos_col USING :
   v_pos 'RNR_VAT' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col6.
  PERFORM f_carga_campos_col USING :
   v_pos 'VAT_PERCEP' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

*  ADD 1 TO V_POS.
*  V_TEXT = P_COL7.
*  PERFORM F_CARGA_CAMPOS_COL USING :
*   V_POS 'OTHER_PERCEP' '' '' '' 'X' 'R' '' '' V_TEXT '' ''
*   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col8.
  PERFORM f_carga_campos_col USING :
   v_pos 'EXEMPTION' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '19' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col09.
  PERFORM f_carga_campos_col USING :
   v_pos 'EXPORTS' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col10.
  PERFORM f_carga_campos_col USING :
   v_pos 'PERCEPNOC' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col101.
  PERFORM f_carga_campos_col USING :
   v_pos 'IOTHER_PERCEP02' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col102.
  PERFORM f_carga_campos_col USING :
   v_pos 'IOTHER_PERCEP01' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col103.
  PERFORM f_carga_campos_col USING :
   v_pos 'IOTHER_PERCEP03' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

*** US #181035 - MMSILVA - 10.06.2025 - Ini ***
  ADD 1 TO v_pos.
  v_text = p_col104.
  PERFORM f_carga_campos_col USING :
   v_pos 'IOTHER_PERCEP04' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text = p_col105.
  PERFORM f_carga_campos_col USING :
   v_pos 'IOTHER_PERCEP05' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.
*** US #181035 - MMSILVA - 10.06.2025 - Fim ***

*  ADD 1 TO v_pos.
*  v_text = p_col10.
*  PERFORM f_carga_campos_col USING :
*   v_pos 'EXEMPT' '' '' '' '' 'R' '' '' v_text '' ''
*   '' '' '' '18' '' '' '' '' ''.
*
*  ADD 1 TO v_pos.
*  v_text = p_col11.
*  PERFORM f_carga_campos_col USING :
*   v_pos 'EARN_PER' '' '' '' '' 'R' '' '' v_text '' ''
*   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text =  p_col11.
  PERFORM f_carga_campos_col USING :
   v_pos 'LINE_TOTAL' '' '' '' 'X' 'R' '' '' v_text '' ''
   '' '' '' '18' '' '' '' '' ''.

  ADD 1 TO v_pos.
  v_text =  'Periodo'.
  PERFORM f_carga_campos_col USING :
   v_pos 'BUPER' '' '' '' '' 'C' '' '' v_text '' ''
   '' '' '' '10' '' '' '' '' 'X'.

ENDFORM.                    " f_cols_alv
*----------------------------------------------------------------------*
* Form  f_eventos_alv
*----------------------------------------------------------------------*
FORM f_eventos_alv .

  PERFORM f_carga_eventos USING c_user 'F_AT_USER_COMMAND'.
  PERFORM f_carga_eventos USING c_top  'XTOP_OF_PAGE'.
  PERFORM f_carga_eventos USING c_list 'F_TOP_OF_LIST'.

ENDFORM.                    " f_eventos_alv
*----------------------------------------------------------------------*
* Form  f_campos_quiebra_alv
*----------------------------------------------------------------------*
FORM f_campos_quiebra_alv .

  CLEAR v_pos.
  ADD 1 TO v_pos.
  PERFORM f_carga_campos_quiebra USING : v_pos 'BUPER' '*' 'X' 'X' .

  ADD 1 TO v_pos.
  PERFORM f_carga_campos_quiebra USING : v_pos 'BUDAT' '' '' 'X' .

ENDFORM.                    " f_campos_quiebra_alv
*----------------------------------------------------------------------*
* Form  f_layout_alv
*----------------------------------------------------------------------*
FORM f_layout_alv .

  PERFORM f_carga_campos_layout USING :
   '' '' '' '' '' '' '' '' 'Total' 'Subtotal' '' ''.

ENDFORM.                    " f_layout_alv
*----------------------------------------------------------------------*
* Form  f_at_user_command
*----------------------------------------------------------------------*
FORM f_at_user_command USING
                       x_ucomm  LIKE sy-ucomm
                       x_selfld TYPE kkblo_selfield. "#EC NEEDED EC CALLED

  CASE x_ucomm.
    WHEN '&IC1'.
      x_ucomm = '&ETA'.
  ENDCASE.

ENDFORM.                    " f_at_user_command

*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_HEADER
*-----------------------------------------------------------------------
FORM f_carga_campos_header USING x_typ x_key x_info .       "#EC *

* Carga los datos de cabecera del report
  CLEAR t_head.
  t_head-typ  = x_typ.
  t_head-key  = x_key.
  t_head-info = x_info.
  APPEND t_head TO t_header.

ENDFORM.                    " F_CARGA_CAMPOS_HEADER
*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_COL
*-----------------------------------------------------------------------
FORM f_carga_campos_col USING x_pos x_field x_tab x_refc x_ref x_sum
                              x_just x_hot x_text_s x_text_m x_text_l
                              x_key x_fix_col x_emp x_curr x_len
                              x_box x_input x_icon x_type x_nout. "#EC *

* Completa la tabla interna con los valores para el layout.
  CLEAR t_afield.
  t_afield-col_pos       = x_pos.
  t_afield-fieldname     = x_field.
  t_afield-tabname       = x_tab.
  t_afield-ref_fieldname = x_refc.
  t_afield-ref_tabname   = x_ref.
  t_afield-do_sum        = x_sum.
  t_afield-just          = x_just.
  t_afield-hotspot       = x_hot.
  t_afield-seltext_s     = x_text_s.
  t_afield-seltext_m     = x_text_m.
  t_afield-seltext_l     = x_text_l.
  t_afield-key           = x_key.
  t_afield-fix_column    = x_fix_col.
  t_afield-emphasize     = x_emp.
  t_afield-cfieldname    = x_curr.
  t_afield-outputlen     = x_len.
  t_afield-checkbox      = x_box.
  t_afield-input         = x_input.
  t_afield-icon          = x_icon.
  t_afield-inttype       = x_type.
  t_afield-no_out        = x_nout.

  APPEND t_afield TO t_fieldcat.

ENDFORM.                 " F_CARGA_CAMPOS_COL
*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_QUIEBRA
*-----------------------------------------------------------------------
FORM f_carga_campos_quiebra USING x_pos x_field x_group x_subt x_up . "#EC *

* Ordenación - Quiebra
  CLEAR t_sort.
  t_sort-spos      = x_pos.
  t_sort-fieldname = x_field.
  t_sort-group     = x_group.
  t_sort-subtot    = x_subt.
  t_sort-up        = x_up.
  APPEND t_sort.

ENDFORM.                    " F_CARGA_CAMPOS_QUIEBRA
*-----------------------------------------------------------------------
* Form  F_CARGA_EVENTOS
*-----------------------------------------------------------------------
FORM f_carga_eventos USING x_even x_form.                   "#EC *

  IF t_event[] IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
      EXPORTING
        i_list_type = 0
      IMPORTING
        et_events   = t_event.
  ENDIF.

  READ TABLE t_event WITH KEY name = x_even INTO t_events.

  IF sy-subrc EQ 0.
    MOVE x_form     TO t_events-form.
    APPEND t_events TO t_event.
    DELETE t_event WHERE name = x_even
                     AND form = space.
  ENDIF.

ENDFORM.                    " F_CARGA_EVENTOS
*-----------------------------------------------------------------------
* Form  F_TABLA_ALV
*-----------------------------------------------------------------------
FORM f_tabla_alv.
*** RMNI - CS1035066 - Inverter sinais p/ chaves 40/50 - 23.11.2022 - Início
  DATA: lw_bseg_aux_ba     TYPE bseg,
        lw_bseg_aux_stfe   TYPE bseg,
        lw_bseg_aux_caba   TYPE bseg,
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Início
        lw_bseg_aux_ntaxed TYPE bseg,
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Fim
        lt_tvarvc          TYPE STANDARD TABLE OF tvarvc,
        lv_hkont           TYPE bseg-hkont,
        lv_type            TYPE dd01v-datatype.
*** RMNI - CS1035066 - Inverter sinais p/ chaves 40/50 - 23.11.2022 - Fim
  DATA: v_vat          LIKE bseg-wrbtr,
        wl_bkpf        TYPE bkpf,
        vl_obj_key     TYPE zmmt_ee_zgr-obj_key,
        vl_obj_key_aux TYPE zmmt_ee_zgr-obj_key,
        wl_ee_zgr_aux  TYPE zmmt_ee_zgr,
        wl_zgr_docs    TYPE zmmt_ee_zgr_docs,
        wl_zgr         TYPE zmmt_ee_zgr,
        vl_tam_str     TYPE i.

  CONSTANTS: c_181400 LIKE bseg-hkont VALUE '0000181400',
             c_182325 LIKE bseg-hkont VALUE '0000182325',
             c_0214   LIKE bseg-bukrs VALUE '0214'.

  t_sal_a[] = t_sal[].
  SORT: t_sal   BY  lineno budat brnch belnr,
        t_sal_a BY  lineno budat brnch belnr.

  DATA: vtaxed           LIKE bseg-wrbtr,
        vtaxed2          LIKE bseg-wrbtr,
        vtaxed3          LIKE bseg-wrbtr,
        vnot_taxed       LIKE bseg-wrbtr,
        vvat             LIKE bseg-wrbtr,
        vrnr_vat         LIKE bseg-wrbtr,
        vvat_percep      LIKE bseg-wrbtr,
        vother_percep    LIKE bseg-wrbtr,
        vexemption       LIKE bseg-wrbtr,
        vexports         LIKE bseg-wrbtr,
        vpercepnoc       LIKE bseg-wrbtr,
        viother_percep01 LIKE bseg-wrbtr,
        viother_percep02 LIKE bseg-wrbtr,
        viother_percep03 LIKE bseg-wrbtr,
        vline_total      LIKE bseg-wrbtr.

  DATA: vawkey  TYPE bkpf-awkey,
        f_belnr TYPE rbkp-belnr,
        f_gjahr TYPE rbkp-gjahr,
        vrmwwr  TYPE rbkp-rmwwr,
        vrmwwrp TYPE rbkp-rmwwr,
        vrmwwrd TYPE rbkp-rmwwr.

* Carga tabla para salida ALV
  LOOP AT t_sal INTO e_sal.
    CLEAR: t_out, v_vat, wl_bkpf.
    MOVE-CORRESPONDING e_sal TO t_out.

* Percepción IVA

* ---> S4 Migration - 07/07/2023 - JP
*  SELECT wrbtr
*      INTO v_vat
*      FROM bseg
*      UP TO 1 ROWS
*     WHERE bukrs EQ c_0214
*       AND belnr EQ t_out-belnr
*       AND gjahr EQ ep-gjahr
*       AND hkont EQ c_181400.
*    ENDSELECT.
*

    DATA: lv_rldnr TYPE  rldnr,
          lv_bukrs TYPE  bukrs,
          lv_belnr TYPE  belnr_d,
          lv_gjahr TYPE  gjahr,
          lv_buzei TYPE  buzei,
          lw_bseg  TYPE  bseg.

    lv_bukrs = c_0214.
    lv_belnr = t_out-belnr.
    lv_gjahr = tab_ep-gjahr.

    DATA: lt_bseg_aux TYPE fagl_t_bseg.

    CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
      IMPORTING
        e_rldnr       = lv_rldnr
      EXCEPTIONS
        not_found     = 1
        more_than_one = 2.

    IF sy-subrc = 0.

      CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
        EXPORTING
          i_rldnr   = lv_rldnr
          i_bukrs   = lv_bukrs
          i_belnr   = lv_belnr
          i_gjahr   = lv_gjahr
        IMPORTING
          et_bseg   = lt_bseg_aux
        EXCEPTIONS
          not_found = 1.
    ENDIF.

    IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ELSE.

      sy-dbcnt = lines( lt_bseg_aux ).
      LOOP AT lt_bseg_aux INTO lw_bseg.
        IF lw_bseg-hkont EQ c_182325.
          v_vat = lw_bseg-wrbtr.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

    IF sy-subrc EQ 0.
      t_out-rnr_vat  = v_vat.
      t_out-line_total = t_out-line_total + t_out-rnr_vat.
    ENDIF.

* Percepción Ingresos Brutos
    CLEAR v_vat.

* ---> S4 Migration - 07/07/2023 - JP
*
*  SELECT wrbtr
*      INTO v_vat
*      FROM bseg
*      UP TO 1 ROWS
*     WHERE bukrs EQ c_0214
*       AND belnr EQ t_out-belnr
*       AND gjahr EQ ep-gjahr
*       AND hkont EQ c_182325.
*    ENDSELECT.
*

    lv_bukrs = c_0214.
    lv_belnr = t_out-belnr.
    lv_gjahr = tab_ep-gjahr.

    CLEAR: lt_bseg_aux.

    CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
      IMPORTING
        e_rldnr       = lv_rldnr
      EXCEPTIONS
        not_found     = 1
        more_than_one = 2.

    IF sy-subrc = 0.

      CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
        EXPORTING
          i_rldnr   = lv_rldnr
          i_bukrs   = lv_bukrs
          i_belnr   = lv_belnr
          i_gjahr   = lv_gjahr
        IMPORTING
          et_bseg   = lt_bseg_aux
        EXCEPTIONS
          not_found = 1.
    ENDIF.

    IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ELSE.

      sy-dbcnt = lines( lt_bseg_aux ).
      LOOP AT lt_bseg_aux INTO lw_bseg.
        IF lw_bseg-hkont EQ c_182325.
          v_vat = lw_bseg-wrbtr.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

    IF sy-subrc EQ 0.
      t_out-vat_percep = v_vat.
      t_out-line_total = t_out-line_total + t_out-vat_percep.
    ENDIF.

*** RMNI - CS1035066 - Inverter sinais p/ chaves 40/50 - 23.11.2022 - Início
    REFRESH lt_tvarvc[].
    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc
      WHERE name LIKE 'ZSDY0004_HKONT%'.

    IF sy-subrc IS INITIAL.

      LOOP AT lt_tvarvc INTO DATA(lw_tvarvc).
        CLEAR lv_hkont.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in  = lw_tvarvc-low
          IMPORTING
            string_out = lv_hkont
            htype      = lv_type.

        IF     lw_tvarvc-name CS 'IIBB_BA'.
          DATA(lv_hkont_iibb_ba) = lw_tvarvc-low.
        ELSEIF lw_tvarvc-name CS 'IIBB_STFE'.
          DATA(lv_hkont_iibb_stfe) = lw_tvarvc-low.
        ELSEIF lw_tvarvc-name CS 'IIBB_CABA'.
          DATA(lv_hkont_iibb_caba) = lw_tvarvc-low.
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Início
        ELSEIF lw_tvarvc-name CS 'NTAXED'.
          DATA(lv_hkont_ntaxed)    = lw_tvarvc-low.
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Fim
        ENDIF.
      ENDLOOP.

      IF lv_hkont_iibb_ba IS NOT INITIAL.
        CLEAR lw_bseg_aux_ba.

* ---> S4 Migration - 07/07/2023 - JP
*
*      SELECT SINGLE *
*          FROM bseg
*          INTO lw_bseg_aux_ba
*          WHERE bukrs EQ tab_ep-bukrs
*            AND belnr EQ t_out-belnr
*            AND gjahr EQ tab_ep-gjahr
*            AND hkont EQ lv_hkont_iibb_ba.
*

        lv_bukrs = tab_ep-bukrs.
        lv_belnr = t_out-belnr.
        lv_gjahr = tab_ep-gjahr.

        CLEAR: lt_bseg_aux.

        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = lv_rldnr
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.

        IF sy-subrc = 0.

          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr   = lv_rldnr
              i_bukrs   = lv_bukrs
              i_belnr   = lv_belnr
              i_gjahr   = lv_gjahr
            IMPORTING
              et_bseg   = lt_bseg_aux
            EXCEPTIONS
              not_found = 1.
        ENDIF.

        IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.

          sy-dbcnt = lines( lt_bseg_aux ).
          LOOP AT lt_bseg_aux INTO lw_bseg.
            IF lw_bseg-hkont EQ lv_hkont_iibb_ba.
              lw_bseg_aux_ba = lw_bseg.
              CONTINUE.
            ENDIF.
          ENDLOOP.
        ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

      ENDIF.

      IF lv_hkont_iibb_stfe IS NOT INITIAL.
        CLEAR lw_bseg_aux_stfe.

* ---> S4 Migration - 07/07/2023 - JP
*
*        SELECT SINGLE *
*          FROM bseg
*          INTO lw_bseg_aux_stfe
*          WHERE bukrs EQ tab_ep-bukrs
*            AND belnr EQ t_out-belnr
*            AND gjahr EQ tab_ep-gjahr
*            AND hkont EQ lv_hkont_iibb_stfe.
*

        lv_bukrs = tab_ep-bukrs.
        lv_belnr = t_out-belnr.
        lv_gjahr = tab_ep-gjahr.

        CLEAR: lt_bseg_aux.

        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = lv_rldnr
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.

        IF sy-subrc = 0.

          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr   = lv_rldnr
              i_bukrs   = lv_bukrs
              i_belnr   = lv_belnr
              i_gjahr   = lv_gjahr
            IMPORTING
              et_bseg   = lt_bseg_aux
            EXCEPTIONS
              not_found = 1.
        ENDIF.

        IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.

          sy-dbcnt = lines( lt_bseg_aux ).
          LOOP AT lt_bseg_aux INTO lw_bseg.
            IF lw_bseg-hkont EQ lv_hkont_iibb_stfe.
              lw_bseg_aux_stfe = lw_bseg.
              CONTINUE.
            ENDIF.
          ENDLOOP.
        ENDIF.
* <--- S4 Migration - 07/07/2023 - JP


      ENDIF.

      IF lv_hkont_iibb_caba IS NOT INITIAL.
        CLEAR lw_bseg_aux_caba.

* ---> S4 Migration - 07/07/2023 - JP
*
*     SELECT SINGLE *
*          FROM bseg
*          INTO lw_bseg_aux_caba
*          WHERE bukrs EQ tab_ep-bukrs
*            AND belnr EQ t_out-belnr
*            AND gjahr EQ tab_ep-gjahr
*            AND hkont EQ lv_hkont_iibb_caba.
*

        lv_bukrs = tab_ep-bukrs.
        lv_belnr = t_out-belnr.
        lv_gjahr = tab_ep-gjahr.

        CLEAR: lt_bseg_aux.

        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = lv_rldnr
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.

        IF sy-subrc = 0.

          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr   = lv_rldnr
              i_bukrs   = lv_bukrs
              i_belnr   = lv_belnr
              i_gjahr   = lv_gjahr
            IMPORTING
              et_bseg   = lt_bseg_aux
            EXCEPTIONS
              not_found = 1.
        ENDIF.

        IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.

          sy-dbcnt = lines( lt_bseg_aux ).
          LOOP AT lt_bseg_aux INTO lw_bseg.
            IF lw_bseg-hkont EQ lv_hkont_iibb_caba.
              lw_bseg_aux_caba = lw_bseg.
              CONTINUE.
            ENDIF.
          ENDLOOP.
        ENDIF.
* <--- S4 Migration - 07/07/2023 - JP


      ENDIF.
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Início
      IF lv_hkont_ntaxed IS NOT INITIAL.
        CLEAR lw_bseg_aux_ntaxed.

* ---> S4 Migration - 07/07/2023 - JP
*
*        SELECT SINGLE *
*          FROM bseg
*          INTO lw_bseg_aux_ntaxed
*          WHERE bukrs EQ tab_ep-bukrs
*            AND belnr EQ t_out-belnr
*            AND gjahr EQ tab_ep-gjahr
*            AND hkont EQ lv_hkont_ntaxed.
*

        lv_bukrs = tab_ep-bukrs.
        lv_belnr = t_out-belnr.
        lv_gjahr = tab_ep-gjahr.

        CLEAR: lt_bseg_aux.

        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = lv_rldnr
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.

        IF sy-subrc = 0.

          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr   = lv_rldnr
              i_bukrs   = lv_bukrs
              i_belnr   = lv_belnr
              i_gjahr   = lv_gjahr
            IMPORTING
              et_bseg   = lt_bseg_aux
            EXCEPTIONS
              not_found = 1.
        ENDIF.

        IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.

          sy-dbcnt = lines( lt_bseg_aux ).
          LOOP AT lt_bseg_aux INTO lw_bseg.
            IF lw_bseg-hkont EQ lv_hkont_ntaxed.
              lw_bseg_aux_ntaxed = lw_bseg.
              CONTINUE.
            ENDIF.
          ENDLOOP.
        ENDIF.
* <--- S4 Migration - 07/07/2023 - JP


      ENDIF.
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Fim
    ENDIF.
*** RMNI - CS1035066 - Inverter sinais p/ chaves 40/50 - 23.11.2022 - Fim
    vtaxed          = 0.
    vtaxed2         = 0.
    vtaxed3         = 0.
    vnot_taxed      = 0.
    vvat            = 0.
    vrnr_vat        = 0.
    vvat_percep     = 0.
    vother_percep   = 0.
    vexemption      = 0.
    vexports        = 0.
    vpercepnoc      = 0.
    viother_percep01 = 0.
    viother_percep02 = 0.
    viother_percep03 = 0.
    vline_total     = 0.

    IF     e_sal_a-lineno = e_sal-lineno
      AND  e_sal_a-budat  = e_sal-budat
      AND  e_sal_a-brnch  = e_sal-brnch
      AND  e_sal_a-belnr  = e_sal-belnr.
      CONTINUE.
    ENDIF.

    LOOP AT t_sal_a INTO e_sal_a WHERE  lineno = e_sal-lineno
                                   AND  budat  = e_sal-budat
                                   AND  brnch  = e_sal-brnch
                                   AND  belnr  = e_sal-belnr.
      ADD e_sal_a-taxed         TO vtaxed.
      ADD e_sal_a-taxed2        TO vtaxed2.
      ADD e_sal_a-taxed3        TO vtaxed3.
      ADD e_sal_a-not_taxed     TO vnot_taxed.
      ADD e_sal_a-vat           TO vvat.
      ADD e_sal_a-rnr_vat       TO vrnr_vat.
      ADD e_sal_a-vat_percep    TO vvat_percep.
      ADD e_sal_a-other_percep  TO vother_percep.
      ADD e_sal_a-exemption     TO vexemption.
      ADD e_sal_a-exports       TO vexports.
      ADD e_sal_a-percepnoc     TO vpercepnoc.
      ADD e_sal_a-line_total    TO vline_total.

      viother_percep01 = e_sal_a-iother_percep01.
      viother_percep02 = e_sal_a-iother_percep02.
      viother_percep03 = e_sal_a-iother_percep03.

    ENDLOOP.
*** RMNI - CS1035066 - Inverter sinais p/ chaves 40/50 - 23.11.2022 - Início
    IF     lw_bseg_aux_ba-bschl EQ '40' AND viother_percep01 GT 0.
      viother_percep01 = viother_percep01 * -1.
    ELSEIF lw_bseg_aux_ba-bschl EQ '50' AND viother_percep01 LT 0.
      viother_percep01 = viother_percep01 * -1.
    ENDIF.

    IF     lw_bseg_aux_caba-bschl EQ '40' AND viother_percep02 GT 0.
      viother_percep02 = viother_percep02 * -1.
    ELSEIF lw_bseg_aux_caba-bschl EQ '50' AND viother_percep02 LT 0.
      viother_percep02 = viother_percep02 * -1.
    ENDIF.

    IF lw_bseg_aux_stfe-dmbtr IS NOT INITIAL.
      viother_percep03 = lw_bseg_aux_stfe-dmbtr.
    ENDIF.

    IF     lw_bseg_aux_stfe-bschl EQ '40' AND viother_percep03 GT 0.
      viother_percep03 = viother_percep03 * -1.
    ELSEIF lw_bseg_aux_stfe-bschl EQ '50' AND viother_percep03 LT 0.
      viother_percep03 = viother_percep03 * -1.
    ENDIF.
*** RMNI - CS1035066 - Inventer sinais p/ chaves 40/50 - 23.11.2022 - Fim
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Início
    IF lw_bseg_aux_ntaxed-dmbtr IS NOT INITIAL.
      vnot_taxed = lw_bseg_aux_ntaxed-dmbtr.
    ENDIF.

    IF     lw_bseg_aux_ntaxed-bschl EQ '40' AND vnot_taxed GT 0.
      vnot_taxed = vnot_taxed * -1.
    ELSEIF lw_bseg_aux_ntaxed-bschl EQ '50' AND vnot_taxed LT 0.
      vnot_taxed = vnot_taxed * -1.
    ENDIF.
*** RMNI - CS1034496 - Preencher campo no gravado com o sinal correto - 13.01.2023 - Fim
    MOVE:  vtaxed         TO t_out-taxed,
           vtaxed2        TO t_out-taxed2,
           vtaxed3        TO t_out-taxed3,
           vnot_taxed     TO t_out-not_taxed,
           vvat           TO t_out-vat,
           vrnr_vat       TO t_out-rnr_vat,
           vvat_percep    TO t_out-vat_percep,
           vother_percep  TO t_out-other_percep,
           vexemption     TO t_out-exemption,
           vexports       TO t_out-exports,
           vpercepnoc     TO t_out-percepnoc,
           viother_percep01 TO t_out-iother_percep01,
           viother_percep02 TO t_out-iother_percep02,
           viother_percep03 TO t_out-iother_percep03,
           vline_total    TO t_out-line_total.

* Verifica se é MIRO

    SELECT  SINGLE awkey
       INTO vawkey
       FROM bkpf
      WHERE bukrs EQ '0100' " Argentina
        AND belnr EQ t_out-belnr
        AND gjahr EQ ep-gjahr
        AND tcode EQ 'MIRO'
        AND waers EQ 'ARS'.
    IF sy-subrc = 0.
      f_belnr = vawkey+0(10).
      f_gjahr = vawkey+10(4).
      SELECT SINGLE rmwwr
        INTO vrmwwr
        FROM rbkp
       WHERE belnr = f_belnr
       AND   gjahr = f_gjahr.
      IF sy-subrc = 0.
        IF vrmwwr GT 0.
          vrmwwrp = abs( t_out-line_total ).
          IF vrmwwr NE vrmwwrp.
            vrmwwrd = vrmwwr - vrmwwrp.
            ADD vrmwwrd TO t_out-exemption.
            IF t_out-line_total LT 0.
              MULTIPLY vrmwwr BY -1.
            ENDIF.
            t_out-line_total = vrmwwr.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    SELECT SINGLE *
      FROM bkpf INTO wl_bkpf
     WHERE bukrs EQ '0100' " Argentina
       AND belnr EQ t_out-belnr
       AND gjahr EQ ep-gjahr.

    IF  sy-subrc = 0.
      CLEAR: f_belnr, f_gjahr, vl_obj_key, wl_zgr_docs, wl_zgr, vl_tam_str, vl_obj_key_aux.
      CASE wl_bkpf-blart.
        WHEN 'ZG'.
          f_belnr         = wl_bkpf-awkey+0(10).
          f_gjahr         = wl_bkpf-awkey+10(4).
          vl_obj_key_aux  = wl_bkpf-awkey.

          SELECT SINGLE *
            FROM zmmt_ee_zgr_docs INTO wl_zgr_docs
           WHERE bukrs    = wl_bkpf-bukrs
             AND ft_belnr = f_belnr
             AND ft_gjahr = f_gjahr.
          IF sy-subrc = 0.
            vl_obj_key = wl_zgr_docs-obj_key.
          ELSEIF vl_obj_key_aux IS NOT INITIAL.
            SELECT SINGLE *
              FROM zmmt_ee_zgr INTO wl_ee_zgr_aux
             WHERE obj_key EQ vl_obj_key_aux.

            IF ( sy-subrc = 0 ) AND ( wl_ee_zgr_aux-interface_miro = '54' ).
              vl_obj_key = wl_ee_zgr_aux-obj_key.
            ENDIF.
          ENDIF.

        WHEN 'ZY'.
          "Buscar a partir do 3º caractere e desconsiderar os 04 ultimos caracteres
          IF ( wl_bkpf-awkey IS NOT INITIAL ) AND
             ( strlen( wl_bkpf-awkey ) > 6 ).
            vl_tam_str = strlen( wl_bkpf-awkey ) - 6.
            vl_obj_key = wl_bkpf-awkey+2(vl_tam_str).
          ENDIF.
      ENDCASE.

      IF vl_obj_key IS NOT INITIAL.
        SELECT SINGLE *
          FROM zmmt_ee_zgr INTO wl_zgr
         WHERE obj_key = vl_obj_key.
        IF sy-subrc = 0.
          t_out-oftp_text = wl_zgr-text2.
        ELSE.
          SELECT SINGLE ltext INTO t_out-oftp_text FROM t003t
                           WHERE spras = sy-langu AND
                                 blart = wl_bkpf-blart.
        ENDIF.
      ELSE.
        SELECT SINGLE ltext INTO t_out-oftp_text FROM t003t
                         WHERE spras = sy-langu AND
                               blart = wl_bkpf-blart.
      ENDIF.
    ENDIF. "IF  SY-SUBRC = 0.


    APPEND t_out.
  ENDLOOP.

ENDFORM.                    " F_TABLA_ALV
*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_LAYOUT
*-----------------------------------------------------------------------
FORM f_carga_campos_layout USING x_group x_ntot x_tot x_cell x_opt
                                   x_box x_flex x_det x_text x_texs
                                   x_f2c x_zebra.           "#EC *

* Carga las configuraciones de layout
  t_layout-coltab_fieldname     = 'COLINFO'.
  t_layout-group_buttons        = x_group.
  t_layout-no_totalline         = x_ntot.
  t_layout-totals_only          = x_tot.
  t_layout-cell_merge           = x_cell.
  t_layout-colwidth_optimize    = x_opt.
  t_layout-box_fieldname        = x_box.
  t_layout-flexible_key         = x_flex.
  t_layout-detail_initial_lines = x_det.
  t_layout-totals_text          = x_text.
  t_layout-subtotals_text       = x_texs.
  t_layout-f2code               = x_f2c.
  t_layout-zebra                = x_zebra.

  CLEAR t_lay.
  REFRESH t_fcat.

* Ejecuta la función para carga de datos de columnas y layout
  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      it_fieldcat = t_fieldcat
      is_layout   = t_layout
    IMPORTING
      et_fieldcat = t_fcat
      es_layout   = t_lay.

ENDFORM.                    " F_CARGA_CAMPOS_LAYOUT
*-----------------------------------------------------------------------
* Form  F_SALIDA_ALV
*-----------------------------------------------------------------------
FORM f_salida_alv.

  l_print-print = ''.
  l_print-prnt_info = ' '.
  l_print-no_print_selinfos = 'X'.
  l_print-no_print_listinfos = 'X'.
*      l_print-no_coverpage = 'X'.
*      l_print-reserve_lines = 1.
*      l_print-print_ctrl-pri_params = l_params.


** Salida lista ALV
  IF par_alv = 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program         = 'ZJ_1AF205'
        "I_CALLBACK_PF_STATUS_SET   = 'PF_STATUS_SET'
        "I_CALLBACK_USER_COMMAND    = 'USER_COMMAND'
        it_fieldcat                = t_fcat
        is_layout                  = t_lay
        it_sort                    = t_sort[]
        it_events                  = t_event[]
        i_save                     = 'A'
        is_print                   = l_print
      TABLES
        t_outtab                   = t_out
      EXCEPTIONS
        program_error              = 1
        maximum_of_appends_reached = 2
        OTHERS                     = 3.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program         = 'ZJ_1AF205'
        "I_CALLBACK_PF_STATUS_SET   = 'PF_STATUS_SET'
        "I_CALLBACK_USER_COMMAND    = 'USER_COMMAND'
        it_fieldcat                = t_fcat
        is_layout                  = t_lay
        it_sort                    = t_sort[]
        it_events                  = t_event[]
        i_save                     = 'A'
        is_print                   = l_print
      TABLES
        t_outtab                   = t_out
      EXCEPTIONS
        program_error              = 1
        maximum_of_appends_reached = 2
        OTHERS                     = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.


ENDFORM.                    " F_SALIDA_ALV

*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->GT_EXTAB   text
*----------------------------------------------------------------------*
FORM pf_status_set USING gt_extab TYPE slis_t_extab.

  DATA: ls_extab TYPE slis_extab.

  ls_extab-fcode = '&ETA'.
  APPEND ls_extab TO gt_extab.

  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING gt_extab.
ENDFORM.                    "pf_status_set
*
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command.

ENDFORM.                    "user_command
*-----------------------------------------------------------------------
* Form  F_TOP_OF_PAGE
*-----------------------------------------------------------------------
FORM xtop_of_page .                                         "#EC CALLED

  FORMAT COLOR 1 INTENSIFIED.

*  CONCATENATE T_OUT-BUPER+4(2) '.' T_OUT-BUPER(4)
*         INTO TXT_ZEILE4+90(10).

  ULINE.
  WRITE:  txt_zeile1.
  ULINE.
  WRITE:  txt_zeile2, txt_zeile3, txt_zeile4.

  ADD 1 TO wa_history-j_1apageno.
  flg_new_page = 'X'.                  " flg for new-page accoured


ENDFORM.                    " F_TOP_OF_PAGE

*-----------------------------------------------------------------------
* Form  F_TOP_OF_LIST
*-----------------------------------------------------------------------
FORM f_top_of_list.                                         "#EC CALLED

ENDFORM.                    " F_TOP_OF_LIST
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

  REFRESH: t_bsas, t_bsis.

  IF ( tg_account_set[] IS NOT INITIAL ) AND ( tab_ep[] IS NOT INITIAL ).

    SELECT *
      FROM bsas
      INTO TABLE t_bsas
     FOR ALL ENTRIES IN tab_ep
     WHERE bukrs EQ '0100' " Argentina
       AND belnr EQ tab_ep-belnr
       AND gjahr EQ ep-gjahr.

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
     FOR ALL ENTRIES IN tab_ep
     WHERE bukrs EQ '0100' " Argentina
       AND belnr EQ tab_ep-belnr
       AND gjahr EQ ep-gjahr.

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

FORM f_atrib_vl_account USING p_t_alv TYPE ty_sal
                              p_hkont LIKE bsas-hkont.

  DATA: vl_belnr TYPE bsas-belnr.

  vl_belnr = p_t_alv-belnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_belnr
    IMPORTING
      output = vl_belnr.

  READ TABLE t_bsas WITH KEY belnr = vl_belnr
                             gjahr = ep-gjahr
                             hkont = p_hkont.
  IF sy-subrc = 0.

    CASE p_hkont.
*        WHEN 113251 OR 213051.            ">>RIM-SKM-IR114866
      WHEN 113251 OR 213051 OR 213054.   "<<RIM-SKM-IR114866
        p_t_alv-iother_percep02  = t_bsas-dmbtr.
      WHEN 113252 OR 213018.
        p_t_alv-iother_percep01  = t_bsas-dmbtr.
      WHEN 113253.
        p_t_alv-iother_percep03  = t_bsas-dmbtr.
*** US #181035 - MMSILVA - 10.06.2025 - Ini ***
      WHEN 113312.
        p_t_alv-iother_percep04  = t_bsas-dmbtr.
      WHEN 113275.
        p_t_alv-iother_percep05  = t_bsas-dmbtr.
*** US #181035 - MMSILVA - 10.06.2025 - Fim ***
    ENDCASE.

  ELSE.

    READ TABLE t_bsis WITH KEY belnr = vl_belnr
                               gjahr = ep-gjahr
                               hkont = p_hkont.
    IF sy-subrc = 0.

      CASE p_hkont.
*        WHEN 113251 OR 213051.            ">>RIM-SKM-IR114866
        WHEN 113251 OR 213051 OR 213054.   "<<RIM-SKM-IR114866
          p_t_alv-iother_percep02  = t_bsis-dmbtr.
        WHEN 113252 OR 213018.
          p_t_alv-iother_percep01  = t_bsis-dmbtr.
        WHEN 113253.
          p_t_alv-iother_percep03  = t_bsis-dmbtr.
*** US #181035 - MMSILVA - 10.06.2025 - Ini ***
        WHEN 113312.
          p_t_alv-iother_percep04  = t_bsis-dmbtr.
        WHEN 113275.
          p_t_alv-iother_percep05  = t_bsis-dmbtr.
*** US #181035 - MMSILVA - 10.06.2025 - Fim ***
      ENDCASE.

    ENDIF.

  ENDIF.

ENDFORM.

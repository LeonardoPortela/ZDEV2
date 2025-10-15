*&---------------------------------------------------------------------*
*&  Include           ZFIY0004_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
*  Tables
*----------------------------------------------------------------------
TABLES:
        bkpf, *bkpf,                        " document header
        bsad,                               " custom. cleared items
        bsak,                               " vendors cleared items
        bsec,                               " cpd document data
        bseg,                               " documents lines
        itcpo,                              " sapscript output interface
        j_1afitpt,                          " fiscal type of vat / texts
        j_1aotdetr,                         " blart -> off.doc.type
        j_1aoftpt,                          " off.doc.type texts
        j_1afrid,                           " ID-Code for foreigners
        fiwtform_01,                        " structure for sapscript
*       --------------------------------------- New -------------------
        t059z,                              " withhld. tax codes
        t059zt,                             " withhld. tax codes: texts
        with_item,                          " document withhld. segment
*        j_1awtpkey,   " partial key of j_1awith, included in with_item
        t059o,                              " off. withhld. codes
*        j_1a104,       " with. certificate data, included in with_item
        lfa1,                               " vendor master record
*        sadr,                               " company address
        spell,                              " amounts in words
        tbsl,                               " posting key master records
        tcurt,                              " currency texts
        t001,                               " company master record
        t001z,                              " company add. datas
        t003,                               " Document classes
        t005u,                              " region texts
        t059p,                              " withhld. tax types
        t059ot,                         " withhld. tax code texts
        t059u.                              " withhld. type texts
TABLES: b0sg. "Note 1048950

*----------------------------------------------------------------------
*  Fields & int. tables
*----------------------------------------------------------------------
DATA:
       copy_no                TYPE p,            " no. of copies
       no_clrd_docm,
       xkunnr                 LIKE bseg-kunnr,
       xlifnr                 LIKE bseg-lifnr,
       xlines                 TYPE p,
       xoffnum(8)             TYPE n,           " official document no.
       xstcd2                 LIKE lfa1-stcd2,
       xxcpdd,
       xblart                 LIKE bkpf-blart,
       w_xort01               type ort01_GP.
**Note 1009246 Begins
DATA: BEGIN OF xlifnr_struc,
      lifnr TYPE bseg-lifnr,
      END OF xlifnr_struc.

DATA: BEGIN OF xret OCCURS 0,
      belnr     type bkpf-belnr,
      bukrs     type bkpf-bukrs,
      WT_QBSHB  type WITH_ITEM-WT_QBSHB,
      WITHT     type WITH_ITEM-WITHT,
      wt_qsshh  type with_item-wt_qsshh ,
      wt_qbshh  type with_item-wt_qbshh  ,
      wt_accbs1 type with_item-wt_accbs1  ,
*      witht     type with_item-witht      ,
      wt_withcd type with_item-wt_withcd  ,
       END OF xret.


DATA: itab_xlifnr LIKE TABLE OF xlifnr_struc,
      wa_xlifnr LIKE xlifnr_struc.
**Note 1009246 Ends
* Down payment requests                     Corr. 3645505
DATA: BEGIN OF downpay_struc.
        INCLUDE STRUCTURE bsak.
DATA: END OF downpay_struc.
DATA: BEGIN OF downpay OCCURS 10.
        INCLUDE STRUCTURE bsak.
DATA: END OF downpay.
DATA: BEGIN OF already_selected OCCURS 10.
        INCLUDE STRUCTURE bsak.
DATA: END OF already_selected.

* Document header
DATA: BEGIN OF xbkpf OCCURS 100.
        INCLUDE STRUCTURE bkpf.
data: augbl like bseg-AUGBL.
DATA: END OF xbkpf.

DATA: BEGIN OF xt059o OCCURS 10.
        INCLUDE STRUCTURE t059o.
DATA: END OF xt059o.

DATA: BEGIN OF xt005u OCCURS 10.
        INCLUDE STRUCTURE t005u.
DATA: END OF xt005u.

DATA: BEGIN OF xaddr1_sel.
        INCLUDE STRUCTURE addr1_sel.
DATA: END OF xaddr1_sel.

DATA: BEGIN OF xsadr.
        INCLUDE STRUCTURE sadr.
DATA: END OF xsadr.
DATA: t_retencion   TYPE STANDARD TABLE OF zfiys_retencion_cab ,
      t_ret_pos     TYPE STANDARD TABLE OF zfiys_retencion_pos ,
      st_ret_pos    TYPE zfiys_retencion_pos                   ,
      st_retencion  TYPE zfiys_retencion_cab                   .

TYPES: BEGIN OF ty_codtab,
       empfg            TYPE bsec-empfg,
       lifnr            TYPE bseg-lifnr,           "Note 1009246
       ctnumber         TYPE with_item-ctnumber,        " Corr. 3645505
       witht            TYPE with_item-witht,
       wt_withcd        TYPE with_item-wt_withcd,
       qscod            TYPE t059z-qscod,
       qsbez            TYPE t059zt-text40,
       accbs            TYPE with_item-wt_accbs,
       accwt            TYPE with_item-wt_accwt,
       qbshb            TYPE with_item-wt_qbshb,
       qbshh            TYPE with_item-wt_qbshh,
       qsshb            TYPE with_item-wt_qsshb,
       qsshh            TYPE with_item-wt_qsshh,
       wdmbtr           TYPE with_item-wt_wdmbtr,          "Note 393173
       wwrbtr           TYPE with_item-wt_wwrbtr,          "Note 393173
       txt(40)          TYPE c,
       augbl            type augbl,

      END OF ty_codtab.
DATA:
* company code data
      BEGIN OF xt001,
       stcdt            LIKE lfa1-stcdt,
       stcd1            LIKE lfa1-stcd1,
       stcd2            LIKE fiwtform_01-stcd2_1,
       stcd3            LIKE lfa1-stcd3,
       fityp            LIKE lfa1-fityp,
       fitptxt          LIKE j_1afitpt-text30,
      END OF xt001,

* off.doc.type texts
      BEGIN OF xoftp OCCURS 20,
       doccls           LIKE j_1aotdetr-doccls,           "Note 511483
       j_1aprtchr       LIKE j_1aotdetr-j_1aprtchr,       "Note 511483
       j_1aoftp         LIKE j_1aotdetr-j_1aoftp,         "Note 511483
       text30           LIKE j_1aoftpt-text30,
      END OF xoftp,

* fiscal type texts
      BEGIN OF xfitpt OCCURS 20,
       koart            LIKE bseg-koart,
       fityp            LIKE lfa1-fityp,
       text30           LIKE j_1afitpt-text30,
      END OF xfitpt,

* payments with withholdings
      BEGIN OF paytab OCCURS 10,
       empfg            LIKE bsec-empfg,
       umsks            LIKE bseg-umsks,
       umskz            LIKE bseg-umskz,
       augdt            LIKE bseg-augdt,
       xzahl            LIKE tbsl-xzahl,
       rebzg            LIKE bseg-rebzg,
       rebzj            LIKE bseg-rebzj,
       rebzt            LIKE bseg-rebzt,
*                                                Corr. 3645505
*       belnrdpc         like bseg-belnrdpc,
*       gjahrdpc         like bseg-gjahrdpc,
       ctnumber         LIKE with_item-ctnumber,
*
       witht            LIKE with_item-witht,
      END OF paytab,

* cleared documents
      BEGIN OF clrdtab OCCURS 10,
       lifnr            LIKE bseg-lifnr,   "Note 1009246
       belnr            LIKE bkpf-belnr,
       gjahr            LIKE bkpf-gjahr,
       bldat            LIKE bkpf-bldat,
       empfg            LIKE bsec-empfg,
       witht            LIKE with_item-witht,
       blart            LIKE bkpf-blart,
       blkls            LIKE t003-blkls,
       xblnr            LIKE bkpf-xblnr,
*       j_1aisnr         like bkpf-j_1aisnr,
*       j_1aprtchr       like bkpf-j_1aprtchr,
*       j_1aoffnum       like bkpf-j_1aoffnum,
       accbs            TYPE with_item-wt_accbs,
       accwt            TYPE with_item-wt_accwt,
       qbshb            TYPE with_item-wt_qbshb,
       qbshh            TYPE with_item-wt_qbshh,
       qsshb            TYPE with_item-wt_qsshb,
       qsshh            TYPE with_item-wt_qsshh,
       wdmbtr           TYPE with_item-wt_wdmbtr,          "Note 393173
       wwrbtr           TYPE with_item-wt_wwrbtr,          "Note 393173
      END OF clrdtab,

* totals per off. withholding code



      BEGIN OF codtab OCCURS 30,
       empfg            LIKE bsec-empfg,
       lifnr            LIKE bseg-lifnr,           "Note 1009246
       ctnumber         LIKE with_item-ctnumber,        " Corr. 3645505
       witht            LIKE with_item-witht,
       wt_withcd        LIKE with_item-wt_withcd,
       qscod            LIKE t059z-qscod,
       qsbez            LIKE t059zt-text40,
       accbs            LIKE with_item-wt_accbs,
       accwt            LIKE with_item-wt_accwt,
       qbshb            LIKE with_item-wt_qbshb,
       qbshh            LIKE with_item-wt_qbshh,
       qsshb            LIKE with_item-wt_qsshb,
       qsshh            LIKE with_item-wt_qsshh,
       wdmbtr           LIKE with_item-wt_wdmbtr,          "Note 393173
       wwrbtr           LIKE with_item-wt_wwrbtr,          "Note 393173
       txt(40)          TYPE c,
       augbl            type augbl,
      END OF codtab,
BEGIN OF codtab2 OCCURS 30,
       empfg            LIKE bsec-empfg,
       lifnr            LIKE bseg-lifnr,           "Note 1009246
       ctnumber         LIKE with_item-ctnumber,        " Corr. 3645505
       witht            LIKE with_item-witht,
       wt_withcd        LIKE with_item-wt_withcd,
       qscod            LIKE t059z-qscod,
       qsbez            LIKE t059zt-text40,
       accbs            LIKE with_item-wt_accbs,
       accwt            LIKE with_item-wt_accwt,
       qbshb            LIKE with_item-wt_qbshb,
       qbshh            LIKE with_item-wt_qbshh,
       qsshb            LIKE with_item-wt_qsshb,
       qsshh            LIKE with_item-wt_qsshh,
       wdmbtr           LIKE with_item-wt_wdmbtr,          "Note 393173
       wwrbtr           LIKE with_item-wt_wwrbtr,          "Note 393173
       txt(40)          TYPE c,
       augbl            type augbl,
      END OF codtab2,
     t_bseg TYPE STANDARD TABLE OF bseg,
* cpd master data
      BEGIN OF xbsec OCCURS 5,
       empfg            LIKE bsec-empfg,
       stcdt            LIKE bsec-stcdt,
       stcd1            LIKE bsec-stcd1,
       lifnr            LIKE lfa1-lifnr,
       anred            LIKE lfa1-anred,
       name1            LIKE bsec-name1,
       stras            LIKE bsec-stras,
       pstlz            LIKE bsec-pstlz,
       ort01            LIKE bsec-ort01,
       pfach            LIKE bsec-pfach,
       stcd3            LIKE lfa1-stcd3,
       stcd2            LIKE fiwtform_01-stcd2_1,
       fitptxt          LIKE j_1afitpt-text30,
      END OF xbsec,

* vendor data
      BEGIN OF xlfa1 OCCURS 20,
       lifnr            LIKE lfa1-lifnr,
       anred            LIKE lfa1-anred,
       name1            LIKE lfa1-name1,
       stras            LIKE lfa1-stras,
       pstlz            LIKE lfa1-pstlz,
       ort01            LIKE lfa1-ort01,
       pfach            LIKE lfa1-pfach,
       stcdt            LIKE lfa1-stcdt,
       stcd1            LIKE lfa1-stcd1,
       stcd2            LIKE fiwtform_01-stcd2_1,
       stcd3            LIKE lfa1-stcd3,
       fitptxt          LIKE j_1afitpt-text30,
      END OF xlfa1,

* posting keys
      BEGIN OF xtbsl OCCURS 10,
       bschl            LIKE tbsl-bschl,
       xzahl            LIKE tbsl-xzahl,
      END OF xtbsl,

* withholding type data
      BEGIN OF xt059p OCCURS 10,
       land1            LIKE t059p-land1,
       witht            LIKE t059p-witht,
       regio            LIKE t059p-regio,
       wt_accpt         LIKE t059p-wt_accpt,
       text             LIKE t059u-text40,
       tdform           LIKE t059p-tdform,
       j_1aformat       LIKE t059p-j_1aformat,
      END OF xt059p,

* withholding code data
      BEGIN OF xt059z OCCURS 10,
       land1            LIKE t059z-land1,
       witht            LIKE t059z-witht,
       wt_withcd        LIKE t059z-wt_withcd,
       wt_posin         LIKE t059z-wt_posin,
       qscod            LIKE t059z-qscod,
      END OF xt059z.

* --> Begin OSS                                            Note 0393173
* already withheld amount
DATA:
* --> To include all the Withholding types that allow Already Withheld
      BEGIN OF x_t059p OCCURS 0 ,
       land1            LIKE t059p-land1 ,
       witht            LIKE t059p-witht ,
      END OF x_t059p .

DATA :
* --> To avoid wrong acummulation on partial-payment documents
       awheld_done(1)  TYPE  c VALUE space .
* <--  End OSS                                             Note 0393173
***Note 1009246 begins
DATA: BEGIN OF wa_lifnr,
lifnr LIKE codtab-lifnr,
END OF wa_lifnr.

DATA: itab_lifnr  LIKE TABLE OF wa_lifnr,
      itab_codtab LIKE TABLE OF codtab.

DATA:  V_PATH    TYPE RLGRAP-FILENAME.

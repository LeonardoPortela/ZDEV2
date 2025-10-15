*&---------------------------------------------------------------------*
*&  Include           ZFIY0005_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Tables
*----------------------------------------------------------------------
TABLES:
        addr1_sel,                     " address selection parameter
        bkpf, *bkpf,                   " document header
        bsad,                          " cleared documents
        bsak,                          " cleared documents
        bsec,                          " one-time-account data
        bseg, *bseg,                   " document lines
        dkadr,                         " address vendor/customer
        itcpo,                         " sapscript output interface
        j_1ai02,                       " structure for sapscript
        j_1afitpvt,                    " fiscal type of vat
        j_1aprtchr,                    " printing char.
        j_1afrid,                      " foreign id's
        t059z,                         " withholding codes
        with_item,                     " withholding segment
        kna1,                          " customer master
        rstxd,                         " spa/gpa for layout set
        sadr,                          " company code /address
        t001,                          " company code
        t001z,                         " company code /add. details
        t003t.                         " document type texts

TYPES:
  BEGIN OF ty_dkadr,
      augbl TYPE augbl,
      anred TYPE anred,
      name1 TYPE name1_gp,
      name2 TYPE name2_gp,
      name3 TYPE name3_gp,
      name4 TYPE name4_gp,
      stras TYPE stras_gp,
      pfach TYPE pfach,
      pstl2 TYPE pstl2,
      pfort TYPE pfort_gp,
      land1 TYPE land1,
      pstlz TYPE pstlz,
      ort01 TYPE ort01_gp,
      ort02 TYPE ort02_gp,
      regio TYPE regio,
      inlnd TYPE land1,
      konto TYPE f130kont,
      eikto TYPE eikto,
      zsabe TYPE dzsabe,
      adrnr TYPE adrnr,
END OF ty_dkadr,

      BEGIN OF ty_paytab,
       empfg LIKE bsec-empfg,
       kunnr LIKE kna1-kunnr,
       umsks LIKE bseg-umsks,
       augdt LIKE bseg-augdt,
       augbl LIKE bseg-augbl,  " Note 138001
       xzahl LIKE bseg-xzahl,
       rebzg LIKE bseg-rebzg,
       rebzj LIKE bseg-rebzj,
       dmbtr LIKE bseg-dmbtr,
       wrbtr LIKE bseg-wrbtr,
       mwsts LIKE bseg-mwsts,
       wmwst LIKE bseg-wmwst,
       qbshh LIKE with_item-wt_qbshh,
       qbshb LIKE with_item-wt_qbshb,
      END OF ty_paytab.
*----------------------------------------------------------------------
*  Fields & int. tables
*----------------------------------------------------------------------
DATA:
       copy_no          LIKE j_1ai02-copyno,     " number of copy
       docm_sel         TYPE c,        " flag docum. selected
       dp_paymnt,
       no_clrd_docm,
       payed_amnt       LIKE bseg-wrbtr,
       v_hoja TYPE sy-tabix,
       xbuzei           LIKE bseg-buzei,
       xxcpdd,
       xlifnr           LIKE bseg-lifnr,
       xlines           TYPE p,
       xstcd2           LIKE kna1-stcd2,
       xumsks           LIKE bseg-umsks,
       xwrbtr           LIKE bseg-wrbtr.

* document headers
DATA: BEGIN OF xbkpf OCCURS 100.
        INCLUDE STRUCTURE bkpf.
DATA: END OF xbkpf.

* cleared items
DATA: BEGIN OF xbsad OCCURS 100.
        INCLUDE STRUCTURE bsad.
DATA: END OF xbsad.

* cleared items
DATA: BEGIN OF xbsak OCCURS 100.
        INCLUDE STRUCTURE bsak.
DATA: END OF xbsak.

* cleared items 2
DATA: BEGIN OF ybsad OCCURS 100.
        INCLUDE STRUCTURE bsad.
DATA: END OF ybsad.

* fiscal vat types
DATA: BEGIN OF xfitpvt OCCURS 10.
        INCLUDE STRUCTURE j_1afitpvt.
DATA: END OF xfitpvt.

* printing char.
DATA: BEGIN OF xprtchr OCCURS 10.
        INCLUDE STRUCTURE j_1aprtchr.
DATA: END OF xprtchr.

* withholding codes
DATA: BEGIN OF xt059z OCCURS 10.
        INCLUDE STRUCTURE t059z.
DATA: END OF xt059z.

* Internal table to keep withholding tax data. Note 568390
DATA: itab_with_item TYPE TABLE OF with_item,
      wa_with_item TYPE  with_item.

DATA:
* cleared documents
      BEGIN OF clrdtab OCCURS 100,
       empfg            LIKE bsec-empfg,
       kunnr            LIKE kna1-kunnr,
       buzei            LIKE bseg-buzei,
       blart            LIKE bkpf-blart,
       belnr            LIKE bseg-belnr,
       xblnr            LIKE bkpf-xblnr,
       budat            LIKE bkpf-budat,
       dmbtr            LIKE bseg-dmbtr,
       wrbtr            LIKE bseg-wrbtr,
       mwsts            LIKE bseg-mwsts,
       wmwst            LIKE bseg-wmwst,
       qbshh            LIKE with_item-wt_qbshh,
       qbshb            LIKE with_item-wt_qbshb,
       gjahr            TYPE gjahr,
      END OF clrdtab,

* payment documents
      BEGIN OF paytab OCCURS 50,
       empfg            LIKE bsec-empfg,
       kunnr            LIKE kna1-kunnr,
       buzei            LIKE bseg-buzei,
       umsks            LIKE bseg-umsks,
       augdt            LIKE bseg-augdt,
       augbl            LIKE bseg-augbl,  " Note 138001
       xzahl            LIKE bseg-xzahl,
       rebzg            LIKE bseg-rebzg,
       rebzj            LIKE bseg-rebzj,
       dmbtr            LIKE bseg-dmbtr,
       wrbtr            LIKE bseg-wrbtr,
       mwsts            LIKE bseg-mwsts,
       wmwst            LIKE bseg-wmwst,
       qbshh            LIKE with_item-wt_qbshh,
       qbshb            LIKE with_item-wt_qbshb,
      END OF paytab,

* down payment document lines
      BEGIN OF dp_paytab OCCURS 50,
       empfg            LIKE bsec-empfg,
       kunnr            LIKE kna1-kunnr,
       buzei            LIKE bseg-buzei,
       dmbtr            LIKE bseg-dmbtr,
       wrbtr            LIKE bseg-wrbtr,
       mwsts            LIKE bseg-mwsts,
       wmwst            LIKE bseg-wmwst,
       qbshh            LIKE with_item-wt_qbshh,
       qbshb            LIKE with_item-wt_qbshb,
      END OF dp_paytab,

* payment document lines texts
      BEGIN OF paylinetxt OCCURS 5,
       empfg            LIKE bsec-empfg,
       kunnr            LIKE kna1-kunnr,
       sgtxt            LIKE bseg-sgtxt,
      END OF paylinetxt,

* payment document lines texts to be printed
      BEGIN OF p_paylinetxt OCCURS 5,
       empfg            LIKE bsec-empfg,
       kunnr            LIKE kna1-kunnr,
       sgtxt            LIKE bseg-sgtxt,
      END OF p_paylinetxt.

* cpd master data
DATA: BEGIN OF xbsec OCCURS 5.
        INCLUDE STRUCTURE bsec.
DATA: END OF xbsec.

* vendor master data
DATA: BEGIN OF xkna1 OCCURS 40.
        INCLUDE STRUCTURE kna1.
DATA: END OF xkna1.

DATA:
* company code data
      BEGIN OF xt001,
       stcdt            LIKE kna1-stcdt,         " cuit
       stcd1            LIKE kna1-stcd1,         " cuit
       stcd2            LIKE j_1ai02-stcd2_1,    " gross income
       fityp            LIKE kna1-fityp,         " fiscal type of vat
       fnd_date(10),                   " foundation date
      END OF xt001,

* document type definitions
      BEGIN OF xt003 OCCURS 10,
       blart            LIKE t003t-blart,
       ltext            LIKE t003t-ltext,
      END OF xt003.

*** RAD 23/06/97 - Tabelle für foreign persons id.
DATA: BEGIN OF xfrid OCCURS 10.
        INCLUDE STRUCTURE j_1afrid.
DATA: END OF xfrid.

***************Start of PDF conversion by C5062443 Dt: 9-2-05**********

TYPES: BEGIN OF ty_vendor_address,
        augbl TYPE j_1ai02-augbl.
        INCLUDE STRUCTURE dkadr.
TYPES: END OF ty_vendor_address.

DATA : gs_vendor_address TYPE dkadr,
       gs_vendor_address1 TYPE ty_vendor_address,
       gs_company_address TYPE sadr,
       gs_info_data TYPE j_1ai02,
       gs_docdata TYPE j_1ai02,
       gs_total TYPE j_1ai02,
       gs_payline TYPE j_1ai02,

       gt_vendor_address TYPE STANDARD TABLE OF ty_vendor_address,
       gt_info_data TYPE STANDARD TABLE OF j_1ai02,
       gt_docdata TYPE jt_1ai02,
       gt_docdata_all TYPE jt_1ai02,
       gt_total       TYPE STANDARD TABLE OF j_1ai02,
       gt_payline     TYPE STANDARD TABLE OF j_1ai02,
       t_bsegaux      TYPE STANDARD TABLE OF bseg   ,

       gv_vat_number TYPE j_1aprtchr-j_1adisvat,
       gv_transaction_type TYPE bseg-umsks,
       gv_compcurrency TYPE j_1ai02-waers.

DATA: fp_outputparams TYPE sfpoutputparams,
       gs_fp_docparams TYPE sfpdocparams,
       e_interface_type TYPE fpinterfacetype,

       fm_name(30),
       w_cx_root  TYPE REF TO cx_root,
       mesg TYPE string,

       form_name TYPE fpname,
       gv_fname(30),
       gv_copy_flag(1) TYPE c,
       v_logo          TYPE char100,

       t_recibo_pos TYPE STANDARD TABLE OF zfiys_recibo_pos   ,
       t_bseg       TYPE STANDARD TABLE OF bseg               ,
       t_dkadr      TYPE STANDARD TABLE OF ty_dkadr           ,
       tj_1ai02     TYPE STANDARD TABLE OF j_1ai02            ,
      st_dkadr      TYPE ty_dkadr                             ,
      st_recibo_pos TYPE zfiys_recibo_pos                     .

DATA: p_pdf ,
      s_pdate  LIKE j_1afpdo-prdate,
      p_ss_pdf TYPE c VALUE 'X'.
***************End of PDF conversion by C5062443 Dt: 9-2-05*************

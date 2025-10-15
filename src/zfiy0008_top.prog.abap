*&---------------------------------------------------------------------*
*&  Include           ZFIY0008_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS : slis. "ALV Addition

TYPES: BEGIN OF ty_compras,
      tipo_comp TYPE char2,
      nro_comp  TYPE char24,
      fecha     TYPE char8,
      cuit_emp  TYPE char11,
      razon_emp TYPE char25,
      imp_liq   TYPE char12,
      cuit_vend TYPE char11,
      razon_ven TYPE char25,
END OF ty_compras.

TYPES: BEGIN OF ty_auxiliar,
    belnr TYPE belnr_d,
    serialno TYPE srnm6,
    bukrs_accno TYPE txtbz,
    budat TYPE budat,
    blart TYPE blart,
    gjahr TYPE gjahr,
    bldat TYPE bldat,
    stcd1 TYPE stcd1,
    vatam TYPE dmbtr,
    stcdt TYPE j_1atoid,
    bukrs TYPE bukrs,
    calye TYPE gjahr,
    calmo TYPE monat,
    koart TYPE koart,
    accno TYPE lifnr,
    brnch TYPE j_1abrnch,
    offnum TYPE xblnr1,
    j_1aoftp TYPE j_1aoftp_d,
    xcpdd TYPE wnocpro,
    waers TYPE waers,
    buzei  TYPE buzei,
END OF ty_auxiliar.

TYPES: BEGIN OF ty_ventas,
      belnr         TYPE belnr_d,
      registro      TYPE c,
      f_compr       TYPE datum ,
      t_compr       TYPE char2 ,
      pto_vta       TYPE char4 ,
      nro_compr     TYPE char20,
      nro_comprh    TYPE char20,
      stcdt         TYPE char2 ,
      cuit          TYPE char11,
      name1         TYPE char30,
      importe       TYPE char15,
      total         TYPE char15,
      impneto       TYPE char15,
      iva           TYPE char4 ,
      impuesto      TYPE char15,
      exentas       TYPE char15,
      alicuota      TYPE char1 ,
      fecha_ret(8)  TYPE n     ,
      retencion(15) TYPE n     ,
END OF ty_ventas.
TYPES: BEGIN OF ty_doctab,
        bukrs  TYPE bukrs,
        calye  TYPE gjahr,
        calmo  TYPE monat,
        koart  TYPE koart,
        accno  TYPE lifnr,
        gjahr  TYPE gjahr,
        belnr  TYPE belnr_d,
        brnch  TYPE brnch,
        offnum  TYPE xblnr,
        budat  TYPE budat,
        bldat  TYPE bldat,
        blart  TYPE blart,
        stcdt  TYPE j_1atoid,
        stcd1  TYPE stcd1,
        j_1aoftp  TYPE j_1aoftp_d,
        xcpdd  TYPE xcpdd,
        vatam  TYPE dmbtr,
        waers  TYPE waers,
        xfile  TYPE char1,
        stblg  TYPE stblg,
        buzei  TYPE buzei,
        lifnr  TYPE lifnr,
        kunnr  TYPE kunnr,
END OF ty_doctab.
TYPES: BEGIN OF ty_gss_j_1af217_list1,
      serialno    TYPE srnm6,
      bukrs_accno TYPE txtbz,
      budat       TYPE budat,
      blart       TYPE blart,
      belnr       TYPE belnr_d,
      gjahr       TYPE gjahr,
      bldat       TYPE bldat,
      stcd1       TYPE stcd1,
      vatam       TYPE dmbtr,
      stcdt       TYPE j_1atoid,
      bukrs       TYPE bukrs,
      calye       TYPE gjahr,
      calmo       TYPE monat,
      koart       TYPE koart,
      accno       TYPE lifnr,
      brnch       TYPE j_1abrnch,
      offnum      TYPE xblnr1,
      j_1aoftp    TYPE j_1aoftp_d,
      xcpdd       TYPE wnocpro,
      waers       TYPE waers,
      buzei       TYPE buzei,
      lifnr  TYPE lifnr,
      kunnr  TYPE kunnr,
END OF ty_gss_j_1af217_list1.

TYPES: BEGIN OF ty_out_aux,
        linea(142) TYPE c  ,
END OF ty_out_aux .
TYPES: BEGIN OF ty_vta,
        linea(398) TYPE c  ,
END OF ty_vta .
TABLES:
        bhdgd,                         " batch header
        bkorm,                         " Correspondence info
        bkpf, *bkpf,                   " document header
        bsad,                          " cleared customer documents
        bsak,                          " cleared vendor documents
        bsec,                          " one-time data
        bseg, *bseg,                   " document lines
        bset,                          " document tax lines
        j_1ataxid,                     " Tax identification
        j_1afrid,                      " foreign Id´s

*       j_1aotdet,                     " off. document types

        j_1aotdetr,                    " off. document types (new table)
        kna1,                          " customer master
        lfa1,                          " vendor master
        rbkp,                          " MM document header
        t001,                          " company code data
        t001z,                         " company code add. data
        t003,                          " document types
        t003_i,                        " document classes  (new table)
        t005,                          " country def
        vbrk.                          " SD document header.



* ALV Changes starts
DATA: t_compras  TYPE STANDARD TABLE OF ty_compras,
      t_bkpf     TYPE STANDARD TABLE OF bkpf      ,
      t_bseg     TYPE STANDARD TABLE OF bseg      ,
      t_ventas   TYPE STANDARD TABLE OF ty_ventas ,
      t_bajada   TYPE STANDARD TABLE OF ty_out_aux,
      t_baj_vta  TYPE STANDARD TABLE OF ty_vta,
      t_bset     TYPE STANDARD TABLE OF bset,
      st_bset    TYPE bset,
      st_compras TYPE ty_compras.


*Table for the first check box
DATA: gt_doctab TYPE STANDARD TABLE OF ty_gss_j_1af217_list1,
      gs_doctab LIKE LINE OF gt_doctab.


*Table for the second check box
DATA: gt_record_1 TYPE STANDARD TABLE OF gss_j_1af217_list2,
      gs_record_1 LIKE LINE OF gt_record_1.

DATA: gv_repid TYPE sy-repid,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_event    TYPE slis_t_event,
      gs_variant TYPE disvariant,        " structure for variant
      gv_frame_tit TYPE  pifobjtext.     " Title for frame


* Structure for log data
DATA : gs_log TYPE bal_s_log.


* Work variable declarations.
DATA : g_aluser TYPE sy-uname, " For log header
       g_alprog      TYPE sy-cprog,         " For log header
       g_log_handle  TYPE balloghndl." Unique id for application log

DATA : gs_display_profile TYPE bal_s_prof. "For display profile

DATA: gs_msg TYPE bal_s_msg. "Application Log: Message Data

DATA: gc_setpfstatus TYPE slis_alv_event-form VALUE 'SET_PF_STATUS',
                                                           "Form Name.
      gc_user_command TYPE slis_formname      VALUE 'USER_COMMAND'.


*FOR HEIRARACHICAL
DATA: gt_tab_header TYPE slis_tabname VALUE 'GT_DOCTAB',
      gt_tab_item   TYPE slis_tabname VALUE 'GT_RECORD_1'.

DATA: gs_keyinfo TYPE slis_keyinfo_alv. "for key-info

CONSTANTS: gc_bukrs(10)    TYPE c VALUE 'BUKRS',
           gc_vatam(15)    TYPE c VALUE 'VATAM',
           gc_serialno(15) TYPE c VALUE 'SERIALNO'.

CONSTANTS: "gc_struct1 TYPE dd02l-tabname VALUE 'GSS_J_1AF217_LIST1',
           gc_struct1 TYPE dd02l-tabname VALUE 'GSS_J_1AF217_LIST1',
           gc_struct2 TYPE dd02l-tabname VALUE 'GSS_J_1AF217_LIST2',
           gc_inttab1 TYPE dd02l-tabname VALUE 'GT_DOCTAB',
           gc_inttab2 TYPE dd02l-tabname VALUE 'GT_RECORD_1'.

CONSTANTS: gc_value_x(1) TYPE c VALUE 'X', "for substituting X
           gc_value_m(1) TYPE c VALUE 'M', "for substituting M
           gc_value_a(1) TYPE c VALUE 'A', "for substituting A
           gc_layout TYPE elemgenkey   " To get title of variant
                    VALUE 'SLIS_VARI'.


*  Fields & internal tables
*  altern. payee
DATA BEGIN OF COMMON PART cpdkna1.
DATA: BEGIN OF cpdkna1 OCCURS 100.
        INCLUDE STRUCTURE kna1.
DATA: END OF cpdkna1.
DATA END OF COMMON PART cpdkna1.

DATA BEGIN OF COMMON PART cpdlfa1.
DATA: BEGIN OF cpdlfa1 OCCURS 100.
        INCLUDE STRUCTURE lfa1.
DATA: END OF cpdlfa1.
DATA END OF COMMON PART cpdlfa1.

DATA:  count_1(8)             TYPE n,
       reject_document,
       hlp_accno(11),
       hlp_sequential(6)      TYPE n,
       hlp_page(1),
       hlp_vatam              LIKE bseg-dmbtr,
       v_gb(4)                TYPE n,
       hlp_name(70),
       hlp_stcdt              LIKE lfa1-stcdt,
       text50(50),
       text_name              LIKE thead-tdname,
       xlines                 TYPE p,
       xprtchr                LIKE j_1aprtchr-j_1aprtchr,
       xvbeln                 LIKE vbrk-vbeln,
       xmbelnr                LIKE rbkp-belnr,
       xmgjahr                LIKE rbkp-gjahr,
      report_total_vatam  LIKE bseg-dmbtr.


* text lines
DATA: BEGIN OF xtline OCCURS 10.
        INCLUDE STRUCTURE tline.
DATA: END OF xtline.


* foreign person id
DATA: BEGIN OF xfrid OCCURS 10.
        INCLUDE STRUCTURE j_1afrid.
DATA: END OF xfrid.


* document types
DATA: BEGIN OF xt003 OCCURS 10.
        INCLUDE STRUCTURE t003.
DATA: END OF xt003.


* document class
DATA: BEGIN OF xt003_i OCCURS 10.
        INCLUDE STRUCTURE t003_i.
DATA: END OF xt003_i.


* tax identification
DATA: BEGIN OF xtaxid OCCURS 10.
        INCLUDE STRUCTURE j_1ataxid.
DATA: END OF xtaxid.


* official document
DATA: BEGIN OF xotdet OCCURS 10.
        INCLUDE STRUCTURE j_1aotdetr.
DATA: END OF xotdet.


DATA:

* customs data
      BEGIN OF custom,
       number(8)        TYPE n,
       date             TYPE d,
      END OF custom,

* selected documents
doctab TYPE ty_doctab OCCURS 0 WITH HEADER LINE,

      BEGIN OF logtab OCCURS 100,
       bukrs            LIKE bseg-bukrs,
       belnr            LIKE bseg-belnr,
       gjahr            LIKE bseg-gjahr,
       stext(11),
       ltext(85),
      END OF logtab,


* company code data
      BEGIN OF xt001,
       name1            LIKE lfa1-name1,
       todc             LIKE lfa1-stcdt,
       stcd1            LIKE lfa1-stcd1,
      END OF xt001,


* file record
      BEGIN OF record_1,               "Record type
       j_1aoftp(2)      TYPE n,        "Official document type
       brnch(4)         TYPE n,
       offnum(20)       TYPE n,
       bldat(8)         TYPE n,        "Document date (DDMMAAAA)
       stcd1(11)        TYPE n,
       name1(25)              ,
       vatam(12)        TYPE n,        "VAT amount
       stcd1v(11)       TYPE n,
       name1v(25)              ,
       vatamv(12)       TYPE n,        "VAT amount
      END OF record_1.

DATA : rev_monat LIKE bkpf-monat.                           "1061683


* Note 1087918 Start
* Structures for SD reversed document
DATA: BEGIN OF comwa.
        INCLUDE STRUCTURE vbco6.
DATA: END OF comwa.
DATA: BEGIN OF vbfa_tab OCCURS 10.
        INCLUDE STRUCTURE vbfa.
DATA: END OF vbfa_tab.

* Structure for MM reversed document
DATA BEGIN OF mm_doc OCCURS 1.
        INCLUDE STRUCTURE acc_doc.
DATA END OF mm_doc.

DATA : mm_rjahr   LIKE rbkp-stblg,
       sd_vbeln   LIKE vbrk-vbeln,
       not_dialog LIKE boole.

DATA : xblnr LIKE bkpf-xblnr.                               "1114951

DATA: n_vatam type BSEG-DMBTR.

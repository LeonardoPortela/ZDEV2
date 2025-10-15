*&---------------------------------------------------------------------*
*&  Include           ZFIY0014_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Tables
*----------------------------------------------------------------------
TABLES: b0sg. "Note 1063453
TABLES:
        bhdgd,                         " output format
        bkpf, *bkpf,                   " document header
        bsec,                          " cpd data
        bseg,                          " documents lines
        bset,                          " document tax lines
        kna1,                          " customer master record
        knat,                          " customer tax groupings
        lfa1,                          " vendor master record
        lfbw ,                         " vendor withholding master data
        j_1afrid,                      " foreign person id
        j_1atpkof,                     " proc.key -> off. code
        j_1atxoff,                     " off.code texts
        j_1ataxid ,                    " proc.key -> tax identification
        t059z,                         " withholding code
        with_item,                     " withhodling segment
        t059o,                         " off. withhld. codes
        t059ot,                        " off. withhld. code descriptions
        t001,                          " company master record
        t001z ,                        " add. company data
        t005 ,                         " country details
        t007f,                         " tax groups
        t007c ,                        " tax code groups
        t059p.                         " withholding types

TABLES: sscrfields. "ALV Addition


TYPES: BEGIN OF ty_bajada_per,
      cuit    TYPE char13,
      fecha   TYPE char10,
      t_comp  TYPE char1,
      letra   TYPE char1,
      nrosuc  TYPE char4,
      emision TYPE char8,
      monto   TYPE char12,
      perc    TYPE char11,
      tipo    TYPE char1,
END OF ty_bajada_per.

TYPES: BEGIN OF ty_bajada_ret,
    cod   TYPE char3,
    cuit  TYPE char13,
    fecha TYPE char10,
    ctnum TYPE char10,
    ret   TYPE char11,
    tipo  TYPE char1,
END OF ty_bajada_ret.

TYPES: BEGIN OF ty_out_aux,
        linea(47) TYPE c  ,
END OF ty_out_aux.

TYPES: BEGIN OF ty_out_aux1,
        linea(61) TYPE c  ,
END OF ty_out_aux1.


* ALV Changes ends
*----------------------------------------------------------------------
*  Fields & int. tables
*----------------------------------------------------------------------

DATA:
       t_bkpf    TYPE STANDARD TABLE OF bkpf,
       t_bseg    TYPE STANDARD TABLE OF bseg,
       t_lfa1    TYPE STANDARD TABLE OF lfa1,
       t_bset    TYPE STANDARD TABLE OF bset,
       t_kna1    TYPE STANDARD TABLE OF kna1,
       t_item    TYPE STANDARD TABLE OF with_item,
       st_kna1   TYPE STANDARD TABLE OF kna1,
       t_bajada  TYPE STANDARD TABLE OF ty_out_aux,
       t_bajada1 TYPE STANDARD TABLE OF ty_out_aux1,
       st_bajada TYPE ty_out_aux,
       st_bajada1 TYPE ty_out_aux1,
       st_item   TYPE with_item,
       st_lfa1   TYPE lfa1,
       st_bkpf   TYPE bkpf,
       st_bseg   TYPE bseg,
       st_bset   TYPE bset.

DATA:
       char21(21),
       f_budat                TYPE d,
       from_date(10),
       holding_act,
       holding_sel,
       ktosl_sel,
       set_date,
       t_budat                TYPE d,
       to_date(10),
       xheader,
       xhkont                 LIKE lfa1-lifnr,
       xlines                 TYPE p,
       xstcd1                 LIKE lfa1-stcd1,         " Tax Number
       ystcd1                 LIKE lfa1-stcd1.         " Tax Number

DATA: BEGIN OF xj1awtoff OCCURS 10.
        INCLUDE STRUCTURE t059o.
DATA: END OF xj1awtoff.


* tax proc.keys -> off. codes + texts

DATA: BEGIN OF xtpkof OCCURS 10.
        INCLUDE STRUCTURE j_1atpkof.
DATA: text40 LIKE j_1atxoff-text40,
      END OF xtpkof.

DATA:

* ---> Output table for Display

      BEGIN OF detail OCCURS 0,
        class                 LIKE j_1aotdetr-j_1aoftp ,
        offnr(16) ,
        belnr(10)                            ,
        gjahr                 LIKE bkpf-gjahr ,
        budat                 LIKE bkpf-budat ,

*       DMBTR(16)             TYPE C ,                  " Note 748649

        dmbtr                 LIKE bseg-dmbtr,          " Note 748649
        txcod(3)                              ,
        qscod                 LIKE j_1atxoff-j_1ataxcod ,
        xreversal             LIKE bkpf-xreversal,    "Note 1064177
        perception            TYPE c,                 "Note 1064177
        tcode(1) ,
        hwbas                 LIKE bseg-hwbas ,
        fityp                 LIKE lfa1-fityp ,
        whsub(1) ,                             " Note 1230326
        hwste                 LIKE bset-hwste ,
        exrt(6)  ,
        exdf(10) ,
        exdt(10) ,
        stcdt                 LIKE lfa1-stcdt ,
        stcd1                 LIKE lfa1-stcd1 ,
        stcd2                 LIKE lfa1-stcd2 ,
        ctnum                 LIKE with_item-ctnumber ,
        butxt                 LIKE t001-butxt ,
        grsup                 LIKE t059z-wt_posin ,
        fptid                 LIKE lfa1-stcd1 ,
        cctid                 LIKE lfa1-stcd1 ,
        hkont                 LIKE lfa1-lifnr,
        name1                 LIKE lfa1-name1,
        bukrs                 LIKE bseg-bukrs,
        date(10) ,
        posneg ,
        cancel ,

*Note # 984952 Begin

        crlf(2)      TYPE x,

*Note # 984952 End

      END OF detail,

      BEGIN OF output_file OCCURS 0,
        class(2)    ,
        date1(10)   ,
        offnr(16)   ,
        dmbtr(16)   ,
        txcod(3)    ,
        qscod(3)    ,
        tcode(1)    ,
        hwbas(14)   ,
        date2(10)   ,
        fityp(2)    ,
        whsub(1) ,                             " Note 1230326
        hwste(14)   ,
        exrt(6)     ,
        exdf(10)    ,
        stcdt(2)    ,
        stcd1(20)   ,
        ctnum(14)   ,
        butxt(30)   ,
        grsup(1)    ,
        fptid(11)   ,
        cctid(11)   ,
*Note # 984952 Begin
        crlf(2)     ,
*Note # 984952 End
      END   OF output_file ,

*Note # 984952 Begin

      BEGIN OF output_file1 OCCURS 0,
        class(2)    ,
        date1(10)   ,
        offnr(16)   ,
        dmbtr(16)   ,
        txcod(3)    ,
        qscod(3)    ,
        tcode(1)    ,
        hwbas(14)   ,
        date2(10)   ,
        fityp(2)    ,
        hwste(14)   ,
        exrt(6)     ,
        exdf(10)    ,
        stcdt(2)    ,
        stcd1(20)   ,
        ctnum(14)   ,
        butxt(30)   ,
        grsup(1)    ,
        fptid(11)   ,
        cctid(11)   ,
      END   OF output_file1 ,

*Note # 984952 End
* detail table for DGI Data (to collect for the export to the DGI)

      BEGIN OF detail_cl OCCURS 50,
        date(8),
        qscod(3),
        todc(2),
        stcd1(13),
        dmbtr                 LIKE bseg-dmbtr,
      END OF detail_cl,


* detail table for DGI Data (to export to the DGI)

      BEGIN OF detail_ds,
        date(8),
        filler1,
        qscod(3),
        filler2,
        todc(2),                       " of partner
        filler3,
        stcd1(13),                     " of partner
        filler4,
        dmbtr(12),
        filler5,
        posneg,
      END OF detail_ds,


* master data

      BEGIN OF mdata OCCURS 50,
       koart            LIKE bseg-koart,
       hkont            LIKE kna1-kunnr ,
       name1            LIKE kna1-name1,
       stcdt            LIKE kna1-stcdt,
       stcd1            LIKE kna1-stcd1,
       stcd2            LIKE kna1-stcd2,
       fityp            LIKE kna1-fityp ,
       land1            LIKE kna1-land1,
       stkzn            LIKE kna1-stkzn,
       adrnr            LIKE kna1-adrnr, "Note 1013585
      END OF mdata,


* totals by code

      BEGIN OF tot_code OCCURS 50,
        qscod                 LIKE detail-qscod,
        text40                LIKE t059o-text40,
        hwste                 LIKE detail-hwste ,
        belnr                 TYPE belnr_d,
      END OF tot_code.

DATA:


* withholding code data

      BEGIN OF xt059z OCCURS 10,
       land1            LIKE t059z-land1,
       witht            LIKE t059z-witht,
       wt_withcd        LIKE t059z-wt_withcd,
       qscod            LIKE t059z-qscod,
       wt_posin         LIKE t059z-wt_posin ,
      END OF xt059z.

DATA: BEGIN OF xt059p OCCURS 10.
        INCLUDE STRUCTURE t059p.
DATA: END OF xt059p.


**** Note 1013585 Begins

TABLES:
        addr1_sel,                     " address selection parameter
        sadr.                          " company address
DATA:
      BEGIN OF addr1_val.
        INCLUDE STRUCTURE addr1_val.
DATA: END OF addr1_val.

DATA:

*work area for people withheld

      BEGIN OF withheld OCCURS 0,
        stcd1                 LIKE t001z-paval,
        butxt                 LIKE t001-butxt ,
        street                LIKE addr1_val-street,
        house_num             LIKE addr1_val-house_num1,
        city                  LIKE addr1_val-city1,
        region                LIKE addr1_val-region,
        postal_cd             LIKE addr1_val-post_code1,
        doc_type              LIKE j_1aotdetr-j_1aoftp ,
        crlf(2)               TYPE x,
      END OF withheld,

*structure for application server output for people withheld

      BEGIN OF output_withheld OCCURS 0,
        stcd1(11)   ,
        butxt(20)   ,
        address(20) ,
        city(20)    ,
        region(2)   ,
        postal_cd(8),
        doc_type(2) ,
        crlf(2)     ,
      END OF output_withheld,

*structure for local GUI output for people withheld

      BEGIN OF output_withheld1 OCCURS 0,
        stcd1(11)   ,
        butxt(20)   ,
        address(20) ,
        city(20)    ,
        region(2)   ,
        postal_cd(8),
        doc_type(2) ,
      END OF output_withheld1.

**** Note 1013585 Ends

*----------------------------------------------------------------------
*  New Variables
*----------------------------------------------------------------------

DATA:
      k_wtt(1) TYPE c VALUE '1' ,       " Withholding Tax
      k_tax(1) TYPE c VALUE '2' .       " Tax Perception
DATA:
      x_number TYPE i          ,
      x_text(16) TYPE c        ,
      hd_cname LIKE t001-butxt ,
      hd_ccuit LIKE t001z-paval ,
      t_doctyp LIKE t003_i OCCURS 0 WITH HEADER LINE ,
      t_doclas LIKE j_1aotdetr OCCURS 0 WITH HEADER LINE ,
      t_concod LIKE j_1afitpvt OCCURS 0 WITH HEADER LINE ,
      t_txgrp  LIKE t007c      OCCURS 0 WITH HEADER LINE .

DATA BEGIN OF t_offtyp OCCURS 0 .
        INCLUDE STRUCTURE j_1atpkof .
DATA text40 LIKE j_1atxoff-text40 .
DATA END OF t_offtyp .


* foreign persons id.

DATA: BEGIN OF xfrid OCCURS 10.
        INCLUDE STRUCTURE j_1afrid.
DATA: END OF xfrid.


* ---> Auxiliar flag for credit memo                              502917

DATA: x_cm(1) TYPE c VALUE space . " INSERT 502917

DATA : flg_canc LIKE vbrk-fksto, "NOTE 738898
       canc_doc LIKE vbrk-sfakn.                           "NOTE 738898

*Note # 984952 Begin

CLASS cl_abap_char_utilities DEFINITION LOAD.

*Note # 984952 End

* ALV Changes starts

** data declarations to support convertion to alv
************************************************************************
*****                   ALV_POOL                                  *****
************************************************************************

TYPE-POOLS: slis.


************************************************************************
*****                   ALV_VARIABLES                              *****
************************************************************************


CONSTANTS: gc_variant_handle(4) TYPE c VALUE '0001', "Variant
           gc_save(1)           TYPE c VALUE 'X'   , " Constant for variant handling
           gc_y                 TYPE c VALUE 'Y'   , " Constant for Y
           gc_a                 TYPE c VALUE 'A'   , " commentary write application
           gc_s                 TYPE c VALUE 'S'   , " commentary write header
           gc_m                 TYPE c VALUE 'M'   , " Ddic medium text
           gc_x                 TYPE c VALUE 'X'   . " Constant for selected data

DATA: gc_setpfstatus TYPE slis_alv_event-form VALUE 'SET_PF_STATUS',
                                                           "Form Name.

* For double click

      gc_user_command TYPE slis_formname        VALUE 'USER_COMMAND'.

DATA: gs_layout TYPE slis_layout_alv, " Layout
      gt_events       TYPE slis_t_event,       " Events for list
      gt_events_1     TYPE slis_t_event,       " Events for simple list

* Field catalog for hierarchical list display

      gt_fieldcat      TYPE slis_t_fieldcat_alv,

* Field catalog for list display

      gt_fieldcat_1 TYPE slis_t_fieldcat_alv.


* Internal table for header data

DATA: gt_header TYPE STANDARD TABLE OF gss_j_1af016_list1.
DATA: gs_header LIKE LINE OF gt_header. " Work area for Header


* Internal table for item data

DATA: gt_item TYPE STANDARD TABLE OF gss_j_1af016_list2.
DATA: gs_item LIKE LINE OF gt_item. " Work area for Header


* Internal table for total code data

DATA gt_tot_code TYPE STANDARD TABLE OF gss_j_1af016_list3.
DATA: gs_tot_code LIKE LINE OF gt_tot_code. " Work area for tot_code

DATA: gs_variant TYPE disvariant," Structure for Variant
      gc_handle1  TYPE slis_handl VALUE 'HAN1',
      gc_handle2  TYPE slis_handl VALUE 'HAN2',
      gv_repid    TYPE sy-repid.   "Report name


* Declaration of the Header and Item tables.

DATA gt_tab_header TYPE slis_tabname VALUE 'GT_HEADER'.
DATA gt_tab_item TYPE slis_tabname VALUE 'GT_ITEM'.


* Declaration of the key-info required for maintainin a link
* between Hearder and the Item Tables.

DATA gs_keyinfo TYPE slis_keyinfo_alv.


* Decalration of the constants for passing in the function modules

CONSTANTS:
          gc_struct1 TYPE dd02l-tabname VALUE 'GSS_J_1AF016_LIST1',
          gc_struct2 TYPE dd02l-tabname VALUE 'GSS_J_1AF016_LIST2',

*          c_repid    type sy-repid      value 'J_1AF016'          ,

          gc_tot_code TYPE dd02l-tabname VALUE 'GSS_J_1AF016_LIST3'.


* Declaration of the constants for using while modification of the field
* catalog.

CONSTANTS:gc_inttab1 TYPE dd02l-tabname VALUE 'GT_HEADER',
          gc_inttab2 TYPE dd02l-tabname VALUE 'GT_ITEM'.

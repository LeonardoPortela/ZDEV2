*&---------------------------------------------------------------------*
*&  Include           ZFIY0009_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Tables
*----------------------------------------------------------------------
TABLES: B0SG.                          "Note 1063453
TABLES:
        BHDGD,                         " output format
        BKPF, *BKPF,                   " document header
        BSEC,                          " cpd data
        BSEG,                          " documents lines
        BSET,                          " document tax lines
        KNA1,                          " customer master record
        KNAT,                          " customer tax groupings
        LFA1,                          " vendor master record
        LFBW ,                         " vendor withholding master data
        J_1AFRID,                      " foreign person id
        J_1ATPKOF,                     " proc.key -> off. code
        J_1ATXOFF,                     " off.code texts
        J_1ATAXID ,                    " proc.key -> tax identification
        T059Z,                         " withholding code
        WITH_ITEM,                     " withhodling segment
        T059O,                         " off. withhld. codes
        T059OT,                        " off. withhld. code descriptions
        T001,                          " company master record
        T001Z ,                        " add. company data
        T005 ,                         " country details
        T007F,                         " tax groups
        T007C ,                        " tax code groups
        T059P.                         " withholding types

TABLES: sscrfields.      "ALV Addition


  TYPES: BEGIN OF ty_bajada,
      COMP      type char2 ,
      FEmision  type char10,
      Nrocomp   type char16,
      Impcomp   type char16,
      impuesto  type char3 ,
      regimen   type char3 ,
      operacion type char1 ,
      Base      type char14,
      fecharet  type char10,
      condicion type char2 ,
      suspen    type char1 ,
      IMP_RET   TYPE char14,
      porc      type char6 ,
      fBoletin  type char10,
      docret    type char2 ,
      nrodocret type char20,
      Certif    type char14,
      name      type char30,
  END OF ty_bajada.


TYPES: BEGIN OF ty_out_aux,
        linea(197) TYPE c  ,
END OF ty_out_aux         .

* ALV Changes ends
*----------------------------------------------------------------------
*  Fields & int. tables
*----------------------------------------------------------------------
data:
       t_bkpf    type STANDARD TABLE OF bkpf,
       t_bseg    type STANDARD TABLE OF bseg,
       t_lfa1    type STANDARD TABLE OF lfa1,
       T_bset    type STANDARD TABLE OF bset,
       t_KNA1    type STANDARD TABLE OF KNA1,
       t_item    type STANDARD TABLE OF with_item,
       st_KNA1   type STANDARD TABLE OF KNA1,
       t_bajada  TYPE STANDARD TABLE OF ty_out_aux,
       st_bajada TYPE ty_out_aux,
       st_item   type with_item,
       st_lfa1   type lfa1,
       st_bkpf   type bkpf,
       st_bseg   type bseg,
       sT_bset   type bset.

DATA:
       CHAR21(21),
       F_BUDAT                TYPE D,
       FROM_DATE(10),
       HOLDING_ACT,
       HOLDING_SEL,
       KTOSL_SEL,
       SET_DATE,
       T_BUDAT                TYPE D,
       TO_DATE(10),
       XHEADER,
       XHKONT                 LIKE LFA1-LIFNR,
       XLINES                 TYPE P,
       XSTCD1                 LIKE LFA1-STCD1,         " Tax Number
       YSTCD1                 LIKE LFA1-STCD1.         " Tax Number

DATA: BEGIN OF XJ1AWTOFF OCCURS 10.
        INCLUDE STRUCTURE T059O.
DATA: END OF XJ1AWTOFF.

* tax proc.keys -> off. codes + texts
DATA: BEGIN OF XTPKOF OCCURS 10.
        INCLUDE STRUCTURE J_1ATPKOF.
DATA:  TEXT40                 LIKE J_1ATXOFF-TEXT40,
      END OF XTPKOF.

DATA:
* ---> Output table for Display
      BEGIN OF DETAIL OCCURS 0,
        CLASS                 LIKE J_1AOTDETR-J_1AOFTP ,
        OFFNR(16) ,
        BELNR(10)                            ,
        GJAHR                 LIKE BKPF-GJAHR ,
        BUDAT                 LIKE BKPF-BUDAT ,
*       DMBTR(16)             TYPE C ,                  " Note 748649
        DMBTR                 LIKE BSEG-DMBTR,          " Note 748649
        TXCOD(3)                              ,
        QSCOD                 LIKE J_1ATXOFF-J_1ATAXCOD ,
        XREVERSAL             LIKE BKPF-XREVERSAL,    "Note 1064177
        PERCEPTION            TYPE C,                 "Note 1064177
        TCODE(1) ,
        HWBAS                 LIKE BSEG-HWBAS ,
        FITYP                 LIKE LFA1-FITYP ,
        whsub(1) ,                             " Note 1230326
        HWSTE                 LIKE BSET-HWSTE ,
        EXRT(6)  ,
        EXDF(10) ,
        EXDT(10) ,
        STCDT                 LIKE LFA1-STCDT ,
        STCD1                 LIKE LFA1-STCD1 ,
        STCD2                 LIKE LFA1-STCD2 ,
        CTNUM                 LIKE WITH_ITEM-CTNUMBER ,
        BUTXT                 LIKE T001-BUTXT ,
        GRSUP                 LIKE T059Z-WT_POSIN ,
        FPTID                 LIKE LFA1-STCD1 ,
        CCTID                 LIKE LFA1-STCD1 ,
        HKONT                 LIKE LFA1-LIFNR,
        NAME1                 LIKE LFA1-NAME1,
        BUKRS                 LIKE BSEG-BUKRS,
        DATE(10) ,
        POSNEG ,
        CANCEL ,
*Note # 984952 Begin
        crlf(2)      TYPE x,
*Note # 984952 End
      END OF DETAIL,
      BEGIN OF DETAILaux OCCURS 0,
        CLASS                 LIKE J_1AOTDETR-J_1AOFTP ,
        OFFNR(16) ,
        BELNR(10)                            ,
        GJAHR                 LIKE BKPF-GJAHR ,
        BUDAT                 LIKE BKPF-BUDAT ,
*       DMBTR(16)             TYPE C ,                  " Note 748649
        DMBTR                 LIKE BSEG-DMBTR,          " Note 748649
        TXCOD(3)                              ,
        QSCOD                 LIKE J_1ATXOFF-J_1ATAXCOD ,
        XREVERSAL             LIKE BKPF-XREVERSAL,    "Note 1064177
        PERCEPTION            TYPE C,                 "Note 1064177
        TCODE(1) ,
        HWBAS                 LIKE BSEG-HWBAS ,
        FITYP                 LIKE LFA1-FITYP ,
        whsub(1) ,                             " Note 1230326
        HWSTE                 LIKE BSET-HWSTE ,
        EXRT(6)  ,
        EXDF(10) ,
        EXDT(10) ,
        STCDT                 LIKE LFA1-STCDT ,
        STCD1                 LIKE LFA1-STCD1 ,
        STCD2                 LIKE LFA1-STCD2 ,
        CTNUM                 LIKE WITH_ITEM-CTNUMBER ,
        BUTXT                 LIKE T001-BUTXT ,
        GRSUP                 LIKE T059Z-WT_POSIN ,
        FPTID                 LIKE LFA1-STCD1 ,
        CCTID                 LIKE LFA1-STCD1 ,
        HKONT                 LIKE LFA1-LIFNR,
        NAME1                 LIKE LFA1-NAME1,
        BUKRS                 LIKE BSEG-BUKRS,
        DATE(10) ,
        POSNEG ,
        CANCEL ,
*Note # 984952 Begin
        crlf(2)      TYPE x,
*Note # 984952 End
      END OF DETAILaux,

      BEGIN OF OUTPUT_FILE OCCURS 0,
        CLASS(2)    ,
        DATE1(10)   ,
        OFFNR(16)   ,
        DMBTR(16)   ,
        TXCOD(3)    ,
        QSCOD(3)    ,
        TCODE(1)    ,
        HWBAS(14)   ,
        DATE2(10)   ,
        FITYP(2)    ,
        whsub(1) ,                             " Note 1230326
        HWSTE(14)   ,
        EXRT(6)     ,
        EXDF(10)    ,
        STCDT(2)    ,
        STCD1(20)   ,
        CTNUM(14)   ,
        BUTXT(30)   ,
        GRSUP(1)    ,
        FPTID(11)   ,
        CCTID(11)   ,
*Note # 984952 Begin
        crlf(2)     ,
*Note # 984952 End
      END   OF OUTPUT_FILE ,
*Note # 984952 Begin
      BEGIN OF OUTPUT_FILE1 OCCURS 0,
        CLASS(2)    ,
        DATE1(10)   ,
        OFFNR(16)   ,
        DMBTR(16)   ,
        TXCOD(3)    ,
        QSCOD(3)    ,
        TCODE(1)    ,
        HWBAS(14)   ,
        DATE2(10)   ,
        FITYP(2)    ,
        HWSTE(14)   ,
        EXRT(6)     ,
        EXDF(10)    ,
        STCDT(2)    ,
        STCD1(20)   ,
        CTNUM(14)   ,
        BUTXT(30)   ,
        GRSUP(1)    ,
        FPTID(11)   ,
        CCTID(11)   ,
      END   OF OUTPUT_FILE1 ,
*Note # 984952 End
* detail table for DGI Data (to collect for the export to the DGI)
      BEGIN OF DETAIL_CL OCCURS 50,
        DATE(8),
        QSCOD(3),
        TODC(2),
        STCD1(13),
        DMBTR                 LIKE BSEG-DMBTR,
      END OF DETAIL_CL,

* detail table for DGI Data (to export to the DGI)
      BEGIN OF DETAIL_DS,
        DATE(8),
        FILLER1,
        QSCOD(3),
        FILLER2,
        TODC(2),                       " of partner
        FILLER3,
        STCD1(13),                     " of partner
        FILLER4,
        DMBTR(12),
        FILLER5,
        POSNEG,
      END OF DETAIL_DS,

* master data
      BEGIN OF MDATA OCCURS 50,
       KOART            LIKE BSEG-KOART,
       HKONT            LIKE KNA1-KUNNR ,
       NAME1            LIKE KNA1-NAME1,
       STCDT            LIKE KNA1-STCDT,
       STCD1            LIKE KNA1-STCD1,
       STCD2            LIKE KNA1-STCD2,
       FITYP            LIKE KNA1-FITYP ,
       LAND1            LIKE KNA1-LAND1,
       STKZN            LIKE KNA1-STKZN,
       ADRNR            LIKE KNA1-ADRNR, "Note 1013585
      END OF MDATA,

* totals by code
*      BEGIN OF TOT_CODE OCCURS 50,
*        QSCOD                 LIKE DETAIL-QSCOD,
*        TEXT40                LIKE T059O-TEXT40,
*        HWSTE                 LIKE DETAIL-HWSTE ,
*      END OF TOT_CODE.
      BEGIN OF TOT_CODE OCCURS 50,
        QSCOD                 LIKE DETAIL-QSCOD,
        TEXT40                LIKE T059O-TEXT40,
        HWSTE                 LIKE DETAIL-HWSTE ,
        belnr                 type belnr_D,
      END OF TOT_CODE.
DATA:

* withholding code data
      BEGIN OF XT059Z OCCURS 10,
       LAND1            LIKE T059Z-LAND1,
       WITHT            LIKE T059Z-WITHT,
       WT_WITHCD        LIKE T059Z-WT_WITHCD,
       QSCOD            LIKE T059Z-QSCOD,
       WT_POSIN         LIKE T059Z-WT_POSIN ,
      END OF XT059Z.

DATA: BEGIN OF XT059P OCCURS 10.
        INCLUDE STRUCTURE T059P.
DATA: END OF XT059P.

**** Note 1013585 Begins
TABLES:
        ADDR1_SEL,                     " address selection parameter
        SADR.                          " company address
DATA:
      BEGIN OF ADDR1_VAL.
        INCLUDE STRUCTURE ADDR1_VAL.
DATA: END OF ADDR1_VAL.

DATA:
*work area for people withheld
      BEGIN OF WITHHELD OCCURS 0,
        STCD1                 LIKE T001Z-PAVAL,
        BUTXT                 LIKE T001-BUTXT ,
        STREET                LIKE ADDR1_VAL-STREET,
        HOUSE_NUM             LIKE ADDR1_VAL-HOUSE_NUM1,
        CITY                  LIKE ADDR1_VAL-CITY1,
        REGION                LIKE ADDR1_VAL-REGION,
        POSTAL_CD             LIKE ADDR1_VAL-POST_CODE1,
        DOC_TYPE              LIKE J_1AOTDETR-J_1AOFTP ,
        crlf(2)               TYPE x,
      END OF WITHHELD,
*structure for application server output for people withheld
      BEGIN OF OUTPUT_WITHHELD OCCURS 0,
        STCD1(11)   ,
        BUTXT(20)   ,
        ADDRESS(20) ,
        CITY(20)    ,
        REGION(2)   ,
        POSTAL_CD(8),
        DOC_TYPE(2) ,
        CRLF(2)     ,
      END OF OUTPUT_WITHHELD,
*structure for local GUI output for people withheld
      BEGIN OF OUTPUT_WITHHELD1 OCCURS 0,
        STCD1(11)   ,
        BUTXT(20)   ,
        ADDRESS(20) ,
        CITY(20)    ,
        REGION(2)   ,
        POSTAL_CD(8),
        DOC_TYPE(2) ,
      END OF OUTPUT_WITHHELD1.
**** Note 1013585 Ends

*----------------------------------------------------------------------
*  New Variables
*----------------------------------------------------------------------
DATA:
      v_tabix  type sy-tabix    ,
      K_WTT(1) TYPE C VALUE '1' ,       " Withholding Tax
      K_TAX(1) TYPE C VALUE '2' .       " Tax Perception
DATA:
      X_NUMBER TYPE I          ,
      X_TEXT(16) TYPE C        ,
      HD_CNAME LIKE T001-BUTXT ,
      HD_CCUIT LIKE T001Z-PAVAL ,
      T_DOCTYP LIKE T003_I OCCURS 0 WITH HEADER LINE ,
      T_DOCLAS LIKE J_1AOTDETR OCCURS 0 WITH HEADER LINE ,
      T_CONCOD LIKE J_1AFITPVT OCCURS 0 WITH HEADER LINE ,
      T_TXGRP  LIKE T007C      OCCURS 0 WITH HEADER LINE .

DATA  BEGIN OF T_OFFTYP OCCURS 0   .
            INCLUDE STRUCTURE J_1ATPKOF .
DATA        TEXT40  LIKE J_1ATXOFF-TEXT40 .
DATA  END   OF T_OFFTYP .

* foreign persons id.
DATA: BEGIN OF XFRID OCCURS 10.
        INCLUDE STRUCTURE J_1AFRID.
DATA: END OF XFRID.

* ---> Auxiliar flag for credit memo                              502917
DATA: X_CM(1) TYPE C VALUE SPACE .                "     INSERT    502917

DATA : FLG_CANC LIKE VBRK-FKSTO,                           "NOTE 738898
       CANC_DOC LIKE VBRK-SFAKN.                           "NOTE 738898
*Note # 984952 Begin
        CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.
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

DATA: gs_layout       TYPE slis_layout_alv,    " Layout
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
DATA  gt_tot_code TYPE STANDARD TABLE OF gss_j_1af016_list3.
DATA: gs_tot_code LIKE LINE OF gt_tot_code. " Work area for tot_code

DATA: gs_variant  TYPE disvariant," Structure for Variant
      gc_handle1  TYPE slis_handl VALUE 'HAN1',
      gc_handle2  TYPE slis_handl VALUE 'HAN2',
      gv_repid    TYPE sy-repid.   "Report name

* Declaration of the Header and Item tables.
DATA gt_tab_header TYPE slis_tabname VALUE 'GT_HEADER'.
DATA gt_tab_item   TYPE slis_tabname VALUE 'GT_ITEM'.

* Declaration of the key-info required for maintainin a link
* between Hearder and the Item Tables.
DATA gs_keyinfo TYPE slis_keyinfo_alv.

* Decalration of the constants for passing in the function modules
CONSTANTS:
          gc_struct1 TYPE dd02l-tabname VALUE 'GSS_J_1AF016_LIST1',
          gc_struct2 TYPE dd02l-tabname VALUE 'GSS_J_1AF016_LIST2',
*         c_repid    type sy-repid      value 'J_1AF016'          ,
          gc_tot_code TYPE dd02l-tabname VALUE 'GSS_J_1AF016_LIST3'.

* Declaration of the constants for using while modification of the field
* catalog.
CONSTANTS:gc_inttab1 TYPE dd02l-tabname VALUE 'GT_HEADER',
          gc_inttab2 TYPE dd02l-tabname VALUE 'GT_ITEM'.

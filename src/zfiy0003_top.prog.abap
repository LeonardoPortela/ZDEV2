*&---------------------------------------------------------------------*
*&  Include           ZFIY0003_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  TYPES
*----------------------------------------------------------------------
TYPES: BEGIN OF TY_BSEG,
         BUKRS    TYPE BUKRS,
         GJAHR    TYPE GJAHR,
         BLART    TYPE BLART,
         BELNR    TYPE BELNR_D,
         LIFNR    TYPE LIFNR,
         XBLNR    TYPE XBLNR,
         BLDAT    TYPE BLDAT,
         DMBTR    TYPE DMBTR,
         WAERS    TYPE WAERS,
         AUGBL    TYPE AUGBL,
         BSCHL    TYPE BSCHL,
         UMSKZ    TYPE UMSKZ,
         WT_QBSHH TYPE WT_WT,
         IMP_NETO TYPE DMBTR,
         ANTICPO  TYPE C,
       END OF TY_BSEG.

TYPES: BEGIN OF TY_EMPRESA,
         BUKRS   TYPE BUKRS,
         NAME1_1 TYPE AD_NAME1,
         STRAS_1 TYPE STRAS,
         ORT01_1 TYPE ORT01_GP,
         ORT02_1 TYPE ORT02_BAS,
         STCD1_1 TYPE STCD1,
         NAME2_1 TYPE AD_NAME2,
         NAME3_1 TYPE AD_NAME3,
         NAME4_1 TYPE AD_NAME4,
       END OF TY_EMPRESA.

TYPES: BEGIN OF TY_PAGO,
         HKONT  TYPE HKONT,
         DESRIP TYPE CHAR50,
         CHEQUE TYPE CHAR10,
         FECHA  TYPE CHAR10,
         DMBTR  TYPE CHAR20,
       END OF TY_PAGO.

TYPES: BEGIN OF TY_RET,
         AUGBL     TYPE AUGBL,
         BUKRS     TYPE BUKRS,
         BELNR     TYPE BELNR_D,
         GJAHR     TYPE GJAHR,
         BUZEI     TYPE BUZEI,
         WITHT     TYPE WITHT,
         WT_WITHCD TYPE WT_WITHCD,
         WT_QBSHH  TYPE WT_WT,
         CTNUMBER  TYPE CTNUMBER,
         TIPO      TYPE TEXT40,
         CONCEP    TYPE TEXT40,
*
         DMBTR     TYPE DMBTR,
         WRBTR     TYPE WRBTR,
*
       END OF TY_RET.

TYPES: BEGIN OF TY_RET_SUM,
         WITHT     TYPE WITHT,
         WT_WITHCD TYPE WT_WITHCD,
         WT_QBSHH  TYPE WT_WT,
       END OF TY_RET_SUM.

*----------------------------------------------------------------------
*  Tables
*----------------------------------------------------------------------
TABLES:
  ADDR1_SEL,                     " address selection parameter
  BKPF, *BKPF,                   " document header
  BSAD,                          " cleared documents
  BSAK,                          " cleared documents
  BSEC,                          " one-time-account data
  BSEG, *BSEG,                   " document lines
  DKADR,                         " address vendor/customer
  ITCPO,                         " sapscript output interface
  J_1AFRID,                      " foreign id
  J_1AI02,                       " structure for sapscript
  J_1AFITPVT,                    " fiscal type of vat
  J_1APRTCHR,                    " nrrg. for printing char.
  T059Z,                         " withholding codes
  WITH_ITEM,                     " withholding segment
  LFA1,                          " vendor master
  RSTXD,                         " spa/gpa for layout set
  SADR,                          " company code /address
  T001,                          " company coe
  T001Z,                         " company code /add. details
  T003T,                         " document type texts
  SSCRFIELDS.                    " Campos en las imágenes de selección

*----------------------------------------------------------------------
*  Fields & int. tables
*----------------------------------------------------------------------
DATA:  COPY_NO      LIKE J_1AI02-COPYNO,     " number of copy
       DOCM_SEL     TYPE C,
       DP_PAYMNT,
       NO_CLRD_DOCM,
       XXCPDD,
       XBUZEI       LIKE BSEG-BUZEI,
       XKUNNR       LIKE BSEG-KUNNR,
       XSTCD2       LIKE LFA1-STCD2,
       XUMSKS       LIKE BSEG-UMSKS,
       XWRBTR       LIKE BSEG-WRBTR.

* document header
DATA: BEGIN OF XBKPF OCCURS 100.
        INCLUDE STRUCTURE BKPF.
DATA: END OF XBKPF.

* cleared items
DATA: BEGIN OF XBSAK OCCURS 100.
        INCLUDE STRUCTURE BSAK.
DATA: END OF XBSAK.

* cleared items
DATA: BEGIN OF XBSAD OCCURS 100.
        INCLUDE STRUCTURE BSAD.
DATA: END OF XBSAD.

* cleared items 2
DATA: BEGIN OF YBSAK OCCURS 100.
        INCLUDE STRUCTURE BSAK.
DATA: END OF YBSAK.

* fiscal vat types
DATA: BEGIN OF XFITPVT OCCURS 10.
        INCLUDE STRUCTURE J_1AFITPVT.
DATA: END OF XFITPVT.

* withholding codes
DATA: BEGIN OF XT059Z OCCURS 10.
        INCLUDE STRUCTURE T059Z.
DATA: END OF XT059Z.

* printing char.
DATA: BEGIN OF XPRTCHR OCCURS 10.
        INCLUDE STRUCTURE J_1APRTCHR.
DATA: END OF XPRTCHR.

* cleared documents
DATA:  BEGIN OF CLRDTAB OCCURS 100,
         EMPFG LIKE BSEC-EMPFG,
         LIFNR LIKE LFA1-LIFNR,
         BLART LIKE BKPF-BLART,
         BELNR LIKE BSEG-BELNR,
         XBLNR LIKE BKPF-XBLNR,
         BUDAT LIKE BKPF-BUDAT,
         DMBTR LIKE BSEG-DMBTR,               " net
         WRBTR LIKE BSEG-WRBTR,
         MWSTS LIKE BSEG-MWSTS,               " tax
         WMWST LIKE BSEG-WMWST,
         QBSHH LIKE WITH_ITEM-WT_QBSHH,       " withheld
         QBSHB LIKE WITH_ITEM-WT_QBSHB,
* developer - wfr
         BUZEI LIKE BSEG-BUZEI,
* end developer - wfr

       END OF CLRDTAB,

* payment documents
       BEGIN OF PAYTAB OCCURS 50,
         EMPFG LIKE BSEC-EMPFG,
         LIFNR LIKE LFA1-LIFNR,
         UMSKS LIKE BSEG-UMSKS,
         AUGDT LIKE BSEG-AUGDT,
         XZAHL LIKE BSEG-XZAHL,
         REBZG LIKE BSEG-REBZG,
         REBZJ LIKE BSEG-REBZJ,
         REBZT LIKE BSEG-REBZT,
         DMBTR LIKE BSEG-DMBTR,               " net
         WRBTR LIKE BSEG-WRBTR,
         MWSTS LIKE BSEG-MWSTS,               " tax
         WMWST LIKE BSEG-WMWST,
         QBSHH LIKE WITH_ITEM-WT_QBSHH,       " withheld
         QBSHB LIKE WITH_ITEM-WT_QBSHB,
* developer - wfr
         BUZEI LIKE BSEG-BUZEI,
         BSCHL LIKE BSEG-BSCHL,
* end developer - wfr
       END OF PAYTAB,

* down payment document lines
       BEGIN OF DP_PAYTAB OCCURS 50,
         EMPFG LIKE BSEC-EMPFG,
         LIFNR LIKE LFA1-LIFNR,
         BUZEI LIKE BSEG-BUZEI,
         DMBTR LIKE BSEG-DMBTR,
         WRBTR LIKE BSEG-WRBTR,
         MWSTS LIKE BSEG-MWSTS,
         WMWST LIKE BSEG-WMWST,
         QBSHH LIKE WITH_ITEM-WT_QBSHH,
         QBSHB LIKE WITH_ITEM-WT_QBSHB,
       END OF DP_PAYTAB.

* cpd master data
DATA: BEGIN OF XBSEC OCCURS 5.
        INCLUDE STRUCTURE BSEC.
DATA: END OF XBSEC.

* vendor master data
DATA: BEGIN OF XLFA1 OCCURS 40.
        INCLUDE STRUCTURE LFA1.
DATA: END OF XLFA1.
DATA:
* company code data
  BEGIN OF XT001,
    STCDT        LIKE LFA1-STCDT,         " type of id
    STCD1        LIKE LFA1-STCD1,         " cuit
    STCD2        LIKE J_1AI02-STCD2_1,    " gross income
    FITYP        LIKE LFA1-FITYP,         " fiscal type of vat
    FND_DATE(10),                   " foundation date
  END OF XT001,

* document type definitions
  BEGIN OF XT003 OCCURS 10,
    BLART LIKE T003T-BLART,
    LTEXT LIKE T003T-LTEXT,
  END OF XT003.

* foreign id
DATA: BEGIN OF XFRID OCCURS 10.
        INCLUDE STRUCTURE J_1AFRID.
DATA: END OF XFRID.


TYPES: BEGIN OF TY_VENDOR_ADDRESS,
         AUGBL TYPE J_1AI02-AUGBL.
        INCLUDE STRUCTURE DKADR.
TYPES: END OF TY_VENDOR_ADDRESS.

*DATA :  BEGIN OF gs_docdata OCCURS 0.
*        INCLUDE STRUCTURE j_1ai02.
*DATA:   buzei TYPE bseg-buzei,
*        END OF   gs_docdata.

TYPES: BEGIN OF TY_DOCDATA.
        INCLUDE STRUCTURE J_1AI02.
TYPES : BUZEI TYPE BSEG-BUZEI.
TYPES : UMSKZ TYPE BSEG-UMSKZ.
TYPES: END OF TY_DOCDATA.
TYPES : TY_T_DOCDATA TYPE TABLE OF TY_DOCDATA.
*
DATA: GS_DOCDATA TYPE STANDARD TABLE OF TY_DOCDATA WITH HEADER LINE.
*develope-eca.

DATA:  GS_COMPANY_ADDRESS  TYPE SADR,
       GS_VENDOR_ADDRESS   TYPE DKADR,
       GS_VENDOR_ADDRESS1  TYPE TY_VENDOR_ADDRESS,
       GT_VENDOR_ADDRESS   TYPE STANDARD TABLE OF TY_VENDOR_ADDRESS,
       GS_INFO_DATA        TYPE J_1AI02,
       GT_INFO_DATA        TYPE STANDARD TABLE OF J_1AI02,
       T_BSAS              TYPE STANDARD TABLE OF BSAS,
       ST_BSAS             TYPE BSAS,
*       gs_docdata TYPE j_1ai02
*       gt_docdata TYPE jt_1ai02,
       GT_DOCDATA          TYPE TY_T_DOCDATA,
       GT_DOCDATA_ALL      TYPE TY_T_DOCDATA,
       GS_TOTAL            TYPE J_1AI02,
       GT_TOTAL            TYPE STANDARD TABLE OF J_1AI02,
       T_BSEG              TYPE STANDARD TABLE OF BSEG,

       GV_TRANSACTION_TYPE TYPE BSEG-UMSKS,
       GV_VAT_NUMBER       TYPE J_1APRTCHR-J_1ADISVAT,
       GV_COMPCURRENCY     TYPE WAERS.

DATA:  FP_OUTPUTPARAMS  TYPE SFPOUTPUTPARAMS,
       GS_FP_DOCPARAMS  TYPE SFPDOCPARAMS,
       E_INTERFACE_TYPE TYPE FPINTERFACETYPE.

DATA : FM_NAME(30),
       W_CX_ROOT   TYPE REF TO CX_ROOT,
       MESG        TYPE STRING.

DATA:
*       Variables
  VL_SUBRC      TYPE SY-SUBRC,
  S_FORM        LIKE RSSCF-TDFORM,
  V_CUIT        TYPE PAVAL,
  V_TEL         TYPE CHAR20,
  V_CUITC       TYPE CHAR20,
  V_NRO_PROV    TYPE CHAR50,
  V_XVORL       TYPE XVORL,
  V_VIA_DE_PAGO TYPE BSEG-ZLSCH,
  V_SPRAS       TYPE SPRAS,
  V_KTOPL       TYPE KTOPL,
  V_TABIX       TYPE SY-TABIX,
*      Tablas
  T_BSEG_AUX    TYPE STANDARD TABLE OF BSEG,
  T_EMPRESA     TYPE STANDARD TABLE OF TY_EMPRESA,
  T_RET         TYPE STANDARD TABLE OF TY_RET,
  T_MPAGOS      TYPE ZFIYT_ORDEN_PAGOS_MPG,
  T_PAGO_RET    TYPE ZFIYT_ORDEN_PAGOS_RET,
  T_PAGO_POS    TYPE ZFIYT_ORDEN_PAGOS_POS,
*      Estructuras
  ST_BSEG       TYPE TY_BSEG,
  ST_HEADER     TYPE BSEG,
  ST_PAGO       TYPE TY_PAGO,
  LS_MPAGOS     TYPE ZFIYS_ORDEN_PAGOS_MPG,
  ST_EMPRESA    TYPE TY_EMPRESA,
  ST_EMISOR     TYPE ZFIYS_ORDEN_PAGOS_CAB,
  ST_PAGO_POS   TYPE ZFIYS_ORDEN_PAGOS_POS,
  ST_PAGO_RET   TYPE ZFIYS_ORDEN_PAGOS_RET,
  ST_MPAGOS     TYPE ZFIYS_ORDEN_PAGOS_MPG,
  FORM_NAME     TYPE FPNAME,
  GV_FNAME(30).

data: vg_tabix TYPE sy-tabix.

DATA: V_PATH    TYPE RLGRAP-FILENAME.

DATA: FUNCTXT TYPE SMP_DYNTXT.

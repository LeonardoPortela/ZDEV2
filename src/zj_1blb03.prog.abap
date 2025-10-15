REPORT J_1BLB03 MESSAGE-ID 8B "#EC CI_USAGE_OK[2195701]
                LINE-SIZE 134
                NO STANDARD PAGE HEADING.
*ENHANCEMENT-POINT J_1BLB03_G4 SPOTS ES_J_1BLB03 STATIC.
*ENHANCEMENT-POINT J_1BLB03_G5 SPOTS ES_J_1BLB03.
*ENHANCEMENT-POINT J_1BLB03_G6 SPOTS ES_J_1BLB03 STATIC.
*ENHANCEMENT-POINT J_1BLB03_G7 SPOTS ES_J_1BLB03.

*----------------------------------------------------------------------*
*----------------------- Report j_1blb03 ------------------------------*
*----------------------------------------------------------------------*
*------- Creation of the legal book 'Registro de Controle da ----------*
*----------------------- Produção e do Estoque' -----------------------*
*------- Dez 2003: Redesign and Corrections - note 684417 -------------*
*-----------------------------(Modelo 3)-------------------------------*
*----------------------------------------------------------------------*

* declaration of material information tables used in main program to
* determine the relevant material information and movements
TABLES: T001W,                         "Plant/Locations
        MARA,                          "Material Master: General Data
        MARC,                          "Material Master: C Segment
        MBEW,                          "Material Valuation
        MKPF,                          "Material document - header
        MSEG,                          "Material document - items
        T156T,                         "Movement type text
        T001,                          "Company Codes
        MAKT.                          "Material description

* declaration of tables used in routine movement_info to determine the
* output information (NF Search etc)
TABLES: VBFA,                          "Sales document flow
        *VBFA,                         "Sales document flow
        M_MEBEL,                       "Matchcode Id MEBE-L - table
                                       "MM document flow
        BKPF,                          "Accounting document header
        RBKP,                          "Log. invoice - header
        RSEG,                          "Log. invoice - item
        VBRK,                          "Sales document: header data
        VBRP.                          "Sales document: item data

* declaration of tables used in routine nf_find to read the data from
* the dependant Nota Fiscal (if existing)
TABLES:  J_1BNFDOC,                    "NF header
         J_1BNFLIN,                    "NF item
        *J_1BNFLIN,                    "NF item
        *MSEG.                         "workarea for MSEG


*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 >>> INICIO
DATA: BRANCH_CENTRO LIKE J_1BBRANCH-BRANCH.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 <<< FIM
*----------------------------------------------------------------------*
* input parameters/select-options
SELECTION-SCREEN BEGIN OF BLOCK SEL1 WITH FRAME TITLE TEXT-111.
PARAMETERS: BUKRS  LIKE J_1BBRANCH-BUKRS OBLIGATORY,  "company code
            BRANCH LIKE J_1BBRANCH-BRANCH OBLIGATORY. "CGC branch
SELECTION-SCREEN END   OF BLOCK SEL1.

SELECTION-SCREEN BEGIN OF BLOCK SEL2 WITH FRAME TITLE TEXT-112.
SELECT-OPTIONS:
            MAT    FOR  MSEG-MATNR NO INTERVALS OBLIGATORY,    "material number
            MATCONS FOR MSEG-MATNR,    "material de consumo
            BWARTCON FOR MSEG-BWART,   "tipo de movimentacao de consumo
            MBLNR  FOR  MKPF-MBLNR,
            MVTYP  FOR  MSEG-BWART.
PARAMETERS: CHECKFI TYPE J_1B_FLAG_CHECKFI AS CHECKBOX DEFAULT 'X',
            INTDOC  TYPE C AS CHECKBOX DEFAULT ' ',   " note note 797729
            NF_INCL TYPE J_1B_FLAG_NF_INCL AS CHECKBOX DEFAULT 'X',
            ALLNFS  TYPE J_1B_ALLNFS RADIOBUTTON GROUP TYP3 DEFAULT 'X',
            REMOVNF TYPE J_1B_REMOVNF RADIOBUTTON GROUP TYP3,
            CLEARNF TYPE J_1B_CLEARNF RADIOBUTTON GROUP TYP3.
SELECTION-SCREEN END   OF BLOCK SEL2.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-114.
PARAMETERS: FIRSTPAG(6) TYPE N OBLIGATORY DEFAULT '9',
            BOOKSIZE(6) TYPE N OBLIGATORY DEFAULT '99',
            PAGEBRK AS CHECKBOX DEFAULT ' ',
            REPTITL TYPE J_1B_REPTITL AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK SEL3 WITH FRAME TITLE TEXT-113.
PARAMETERS: MONTH LIKE MARV-LFMON OBLIGATORY,
            LFGJA LIKE MARV-LFGJA OBLIGATORY.
SELECTION-SCREEN END OF BLOCK SEL3.

*----------------------------------------------------------------------*
* definition of constants
* a) constants for reference keys (find NF)
CONSTANTS: BEGIN OF CONST_REFTYP,
           MM(2)       TYPE C VALUE 'MD',
           SD(2)       TYPE C VALUE 'BI',
           FI(2)       TYPE C VALUE 'IV',
           LI(2)       TYPE C VALUE 'LI',
           END OF CONST_REFTYP.

* b) constant for country code Brazil (NBM code description)
CONSTANTS: COUNTRY_BR(2) TYPE C VALUE 'BR'.

* c) constant for calling report (check new period closing)
CONSTANTS: MODELO_3(1) TYPE C VALUE '3'.

* d) constants for taxgroups
CONSTANTS: BEGIN OF CONST_TAXGRP,
           IPI(4)         TYPE C VALUE 'IPI ',
           END OF CONST_TAXGRP.

* e) general constants
CONSTANTS: SAO_PAULO(2) VALUE 'SP',    "abbrev. for Sao Paulo state
           SANTA_CATARINA(2) VALUE 'SC', "abbrev. for St. Catarina state
           NO_DATA(13) VALUE 'Sem movimento',               "#EC NOTEXT
           COMP_NINE(20) TYPE C VALUE '09182736455463728190'.

*----------------------------------------------------------------------*
* declaration of internal tables
DATA: INTT604N LIKE T604N OCCURS 0 WITH HEADER LINE,   " NBM code texts
      INTJ_1BAJ LIKE J_1BAJ OCCURS 0 WITH HEADER LINE, " NF types
      MOVEMENTS LIKE T156T OCCURS 0 WITH HEADER LINE.  " movemt. types

DATA: WA_MAT LIKE LINE OF MAT.

DATA: BEGIN OF PLANTS OCCURS 0,        " plants
        WERKS LIKE T001W-WERKS,
      END OF PLANTS.

DATA: BEGIN OF REPORTED_NFS OCCURS 0,
      DOCNUM LIKE J_1BNFLIN-DOCNUM,
      ITMNUM LIKE J_1BNFLIN-ITMNUM,
END OF REPORTED_NFS.

DATA: BEGIN OF IT_REFDOC OCCURS 100,
        GJAHR LIKE M_MEBEL-GJAHR,
        BUZEI LIKE M_MEBEL-BUZEI,
        LFGJA LIKE M_MEBEL-LFGJA,
        LFBNR LIKE M_MEBEL-LFBNR,
        LFPOS LIKE M_MEBEL-LFPOS,
        ZEKKN LIKE M_MEBEL-ZEKKN,
        VGABE LIKE M_MEBEL-VGABE,
        BWART LIKE MSEG-BWART,
        SHKZG LIKE MSEG-SHKZG,
        SOBKZ LIKE MSEG-SOBKZ,
      END OF IT_REFDOC.

DATA: BEGIN OF IT_MEBEL OCCURS 10.
        INCLUDE STRUCTURE M_MEBEL.
DATA    OK.
DATA: END OF IT_MEBEL.

DATA: BEGIN OF INT_REPORTED_NFBT OCCURS 0,
      DOCNUM LIKE J_1BNFLIN-DOCNUM,
      MATNR  LIKE J_1BNFLIN-MATNR,
      MENGE  LIKE J_1BNFLIN-MENGE,
      CHARG  LIKE J_1BNFLIN-CHARG,
      REFKEY LIKE J_1BNFLIN-REFKEY,
      REFITM LIKE MSEG-ZEILE,
      END OF INT_REPORTED_NFBT,
      WA_REPORTED_NFBT LIKE INT_REPORTED_NFBT.

DATA: BEGIN OF IT156W OCCURS 100,
        BUSTW LIKE T156W-BUSTW,
        XBGBB LIKE T156W-XBGBB,
      END OF IT156W.

DATA: BEGIN OF IT_MAKT OCCURS 0,
      MATNR TYPE MARA-MATNR,
      MAKTX TYPE MAKT-MAKTX,
      END OF IT_MAKT.

DATA: IT_0028 TYPE TABLE OF ZFIT0028 WITH HEADER LINE.

*----------------------------------------------------------------------*
* declaration of structures coming from function modules
DATA: CGC_NUMBER    LIKE J_1BWFIELD-CGC_NUMBER,
      ADDRESS1      LIKE ADDR1_VAL,
      BRANCH_DATA   LIKE J_1BBRANCH,
      T_MESG        LIKE MESG OCCURS 0 WITH HEADER LINE. " errorlog

*----------------------------------------------------------------------*
* output line / structure
DATA: BEGIN OF OUT,
      BUDAT          LIKE MKPF-BUDAT,
      MBLNR          LIKE MKPF-MBLNR,
      SERIES         LIKE J_1BNFDOC-SERIES,
      SUBSER         LIKE J_1BNFDOC-SUBSER,
      NFNUM          LIKE J_1BNFDOC-NFNUM,
      NFENUM         LIKE J_1BNFDOC-NFENUM,     "NFE
      NFE            LIKE J_1BNFDOC-NFE,        "NFE
*      esp(2)         TYPE c,                           " note 736398
      ESP(3)         TYPE C,                            " note 736398
      DAY(2)         TYPE N,
      CFOP(4)        TYPE N,
      CFOP_EXT(2)    TYPE N,
      DIR(1)         TYPE C,
      MENGE          LIKE MSEG-MENGE,
      MEINS          LIKE MARA-MEINS,
      NFTOT          LIKE J_1BINLIN-NFTOT,
      IPIVAL         LIKE J_1BINLIN-IPIVAL,
      BWART          LIKE MSEG-BWART,
      SOBKZ          LIKE MSEG-SOBKZ,
      KZBEW          LIKE MSEG-KZBEW,
      KZZUG          LIKE MSEG-KZZUG,
      KZVBR          LIKE MSEG-KZVBR,
      IPIRATE(6)     TYPE C,
      OBS_TEXT(10)   TYPE C,
      SHKZG          LIKE MSEG-SHKZG,
      MATNR          LIKE MSEG-MATNR,
      MATNR2          LIKE MSEG-MATNR,
      WERKS          LIKE MSEG-WERKS,
      BWTAR          LIKE MSEG-BWTAR,
      CPUDT          LIKE MKPF-CPUDT,
      CPUTM          LIKE MKPF-CPUTM,
      ZEILE          LIKE MSEG-ZEILE,
      CODIGO         TYPE C,
      VAL_PROC_IND   TYPE C,
      ACC_ESTOQUE    LIKE MSEG-MENGE,
* boi note 821403
      CREDIT_MEMO_FLAG TYPE C,
      BELNR          LIKE BSEG-BELNR,
      OLDBELNR       LIKE BSEG-BELNR,
      FOUND          TYPE C,
      QUANT_ENTR_1   LIKE J_1BNFLIN-MENGE,
      QUANT_SAID_1   LIKE J_1BNFLIN-MENGE,
      NOMVT          TYPE C,                                "1487825
* eoi note 821403
      SUBCONTRACT    TYPE C,                                "1523305
END OF OUT.
* boi note 821403
DATA : PREV_FORMOUT_ESTOQUE LIKE MSEG-MENGE,
       ENTRDA       LIKE J_1BNFLIN-MENGE,
       SAIDA        LIKE J_1BNFLIN-MENGE,
       ALV_OUT      LIKE OUT OCCURS 0 WITH HEADER LINE.



DATA:  BEGIN OF IT_MKPF_NFE OCCURS 0,
        MBLNR TYPE MKPF-MBLNR,
        MJAHR TYPE MKPF-MJAHR,
        BKTXT TYPE MKPF-BKTXT,
        VGBEL TYPE VBRP-VGBEL,
       END OF IT_MKPF_NFE,

       BEGIN OF IT_VBRP_NFE OCCURS 0,
         VGBEL TYPE VBRP-VGBEL,
         VBELN TYPE VBRP-VBELN,
         REFKEY TYPE J_1BNFLIN-REFKEY,
       END OF IT_VBRP_NFE,

       BEGIN OF IT_BNFLIN_NFE OCCURS 0,
         REFKEY TYPE J_1BNFLIN-REFKEY,
         DOCNUM TYPE J_1BNFLIN-DOCNUM,
         CFOP   TYPE J_1BNFLIN-CFOP,
       END OF IT_BNFLIN_NFE,

       BEGIN OF IT_BNFDOC_NFE OCCURS 0,
         DOCNUM TYPE J_1BNFDOC-DOCNUM,
         NFENUM TYPE J_1BNFDOC-NFENUM,
       END OF IT_BNFDOC_NFE.

* eoi note 821403

*----------------------------------------------------------------------*
* internally used record data - contains sorting fields
DATA: BEGIN OF IT_MDATA OCCURS 0,
      MATNR          LIKE MSEG-MATNR,  "material number
      WERKS          LIKE MSEG-WERKS,  "valuation area/plant
      BWTAR          LIKE MSEG-BWTAR,  "valuation type
      BUDAT          LIKE MKPF-BUDAT,
      CPUDT          LIKE MKPF-CPUDT,
      CPUTM          LIKE MKPF-CPUTM,
      DAY(2)         TYPE N,
      BLDAT          LIKE MKPF-BLDAT,
      MBLNR          LIKE MSEG-MBLNR,
      ZEILE          LIKE MSEG-ZEILE,
      SHKZG          LIKE MSEG-SHKZG,
      MEINS          LIKE MSEG-MEINS,
      MENGE          LIKE MSEG-MENGE,
      BWART          LIKE MSEG-BWART,
      SOBKZ          LIKE MSEG-SOBKZ,
      KZBEW          LIKE MSEG-KZBEW,
      KZZUG          LIKE MSEG-KZZUG,
      KZVBR          LIKE MSEG-KZVBR,
      MJAHR          LIKE MKPF-MJAHR,
      AWSYS          LIKE MKPF-AWSYS,
      CODIGO         TYPE C,
      BUSTW          LIKE MSEG-BUSTW,
      XAUTO          TYPE C,
      VAL_PROC_IND   TYPE C,
      XBLNR          TYPE MKPF-XBLNR,                       " 773139
      SMBLN          TYPE MSEG-SMBLN,               " note 797729
      SMBLP          TYPE MSEG-SMBLP,               " note 942077
      UMWRK          TYPE MSEG-UMWRK,               " note 797729
      INT_DOC_IND    TYPE C,                        " note 797729
      ERFMG          TYPE MSEG-ERFMG,               " note 1137434
      ERFME          TYPE MSEG-ERFME,               " note 1137434
      NOMVT          TYPE C,                                "1487825
      EBELN          TYPE MSEG-EBELN,                       "1523305
      EBELP          TYPE MSEG-EBELP,                       "1523305
      SUBCONTRACT    TYPE C,                                "1523305
      MATNR2         TYPE MSEG-MATNR,
END OF IT_MDATA.

DATA: BEGIN OF IT_MAT_BUDAT OCCURS 0,
       MBLNR TYPE MSEG-SMBLN,
       BUDAT TYPE MKPF-BUDAT,
       MATNR TYPE MSEG-MATNR,
      END OF IT_MAT_BUDAT.

DATA: IT_MDATA_CONS LIKE TABLE OF IT_MDATA WITH HEADER LINE.
DATA: IT_MDATA_F50 LIKE TABLE OF IT_MDATA WITH HEADER LINE.
*----------------------------------------------------------------------*
* information to be held during LOOP to avoid double-database access
DATA: BEGIN OF OLDINFO OCCURS 1,
      MATNR          LIKE MBEW-MATNR,  "material number
      MAKTX          LIKE MAKT-MAKTX,  "material description
      WERKS          LIKE MSEG-WERKS,  "valuation area/plant
      BWTAR          LIKE MBEW-BWTAR,  "valuation type
      NBM(20)        TYPE C,           "nbm-code
      IPIRATE(6)     TYPE C,           "IPI tax rate
      TEXT1          LIKE T604N-TEXT1, "text for nbm code
      MEINS          LIKE MARA-MEINS,  "base unit of measure
      LBKUM          LIKE MBEW-LBKUM,  "Total valuated stock
      SALK3          LIKE MBEW-SALK3,  "Value of total valuated stock
      VMKUM          LIKE MBEW-VMKUM,  "Total val. stock in prev. period
      VMSAL          LIKE MBEW-VMSAL,  "Value of tot. val. st. in p. p.
END OF OLDINFO.

*----------------------------------------------------------------------*
* help variables
DATA: LINES         TYPE I,            "number of plants per cgc-company
      TEXT1         LIKE T604N-TEXT1,  "text for nbm code
      FOUND(1)      TYPE C,            "indicator: NF found
      REST          LIKE MSEG-MENGE,
      DATUM_LOW     LIKE SY-DATUM,
      DATUM_HIGH    LIKE SY-DATUM,
      PAGNO(6)      TYPE N,            "pagenumber within one book
      SKIP_MATERIAL(1) TYPE C,         "indicator: Material not to be
                                       "considered (asset, consumption)
      FIRST_RECORD_REP(1)  TYPE C VALUE 'X', "If 'X': nothing selected
      FIRST_RECORD_MAT(1)  TYPE C VALUE 'X', "If 'X': nothing selected
      MOVTAB(1)        TYPE C,         "output control - here: output of
                                       "movement type descriptions;
                                       "different header
      ONLY_MBEW(1)      TYPE C,
      LCL_TABIX         LIKE SY-TABIX,
      CHECK_ACC_ESTOQUE LIKE MSEG-MENGE,
      GV_TFILL          TYPE SY-TFILL,
      DUMMY             TYPE C,        " used for dummy-carry over
      PERIV             LIKE T001-PERIV,  " adjustm. shifted fiscal year
      BUPER             LIKE T009B-POPER, " adjustm. shifted fiscal year
      GJAHR             LIKE T009B-BDATJ, " adjustm. shifted fiscal year
      INVERTED_DATE     LIKE J_1BTXIP1-VALIDFROM,
      LT_TXIP1          LIKE J_1BTXIP1   OCCURS 0 WITH HEADER LINE,
      LT_TXIP2          LIKE J_1BTXIP2   OCCURS 0 WITH HEADER LINE,
      LT_TXIP3          LIKE J_1BTXIP3   OCCURS 0 WITH HEADER LINE,
      L_TABIX           LIKE SY-TABIX.                     " note 79772

DATA: POPER LIKE T009B-POPER.                              " note 720396

DATA: CFOP_VERSION TYPE J_1BCFOP_VER,
      EXT_LEN      TYPE J_1BCFOP_EXTLEN,
      CFOP_LENGTH  TYPE J_1BCFOP_LEN,
      START_DATE   TYPE D,
      C_CFOPN      TYPE I VALUE 45,
      C_CFOPX      TYPE I,
      LV_BWKEY     TYPE T001K-BWKEY.                        "1487825

DATA: BEGIN OF SUMS,
      QUANT_ENTR_1   LIKE J_1BNFLIN-MENGE,
      QUANT_SAID_1   LIKE J_1BNFLIN-MENGE,
      END OF SUMS.

* BADI definition for legal books                             nt. 617905
CLASS CL_EX_BADI_J_1BLEGALREPORT DEFINITION LOAD.           " nt. 617905
DATA IF_EX_BADI_J_1BLEGALREPORT TYPE REF TO IF_EX_BADI_J_1BLEGALREPORT.

* Store cumulated data from EBEWH and QBEWH here:           " nt. 862318
DATA EQBEWSAVE TYPE MBEW.                                   " nt. 862318

DATA: WG_CONSUMO.
DATA LV_EKPO TYPE EKPO.                                     "1523305

INCLUDE ZJ_1BLB03_IN68.

*----------------------------------------------------------------------*
*--------------------at selection-screen-------------------------------*
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

* authority check for BUKRS
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'ACTVT' FIELD '03'
    ID 'BUKRS' FIELD BUKRS.
  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'BUKRS'.
    MESSAGE E091 WITH BUKRS.
  ENDIF.

* check if booksize > number of first page
  IF FIRSTPAG > BOOKSIZE.
    MESSAGE E459.
  ENDIF.

* check if firstpag < 2.
  IF FIRSTPAG < 2.
    MESSAGE E467.
  ENDIF.

* The system will only report movements with FI posting or fiscal
* documents related. Some movement types maybe will not be showed.
  IF NOT MVTYP[] IS INITIAL OR NOT MBLNR[] IS INITIAL.
* Accumulated stock, Estoque inicial, Totals and Saldo not diplayed
    MESSAGE W497.
  ENDIF.

*----------------------------------------------------------------------*
*---------------------determine reporting period-----------------------*
*----------------------------------------------------------------------*
** Date from                                               " note 720396
*  CONCATENATE lfgja month '01' INTO datum_low.
*
** Date to
*  CONCATENATE lfgja month '31' INTO datum_high.
  SELECT SINGLE PERIV FROM T001 INTO PERIV WHERE BUKRS = BUKRS.
  POPER = MONTH.
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR        = LFGJA
      I_PERIV        = PERIV
      I_POPER        = POPER
    IMPORTING
      E_DATE         = DATUM_LOW
    EXCEPTIONS
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      OTHERS         = 4.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR        = LFGJA
      I_PERIV        = PERIV
      I_POPER        = POPER
    IMPORTING
      E_DATE         = DATUM_HIGH
    EXCEPTIONS
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      OTHERS         = 4.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.                                                  " note 720396

*----------------------------------------------------------------------*
*--------- determine cgc data for page header of report pages ---------*
*---------- and address data of company code/business place -----------*
*----------------------------------------------------------------------*

*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 >>> INICIO
  BRANCH_CENTRO = BRANCH.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 <<< FIM

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
       EXPORTING
            BRANCH      = BRANCH
            BUKRS       = BUKRS
       IMPORTING
*         address     =
            BRANCH_DATA = BRANCH_DATA
            CGC_NUMBER  = CGC_NUMBER
            ADDRESS1    = ADDRESS1
       EXCEPTIONS
            OTHERS      = 04.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 >>> INICIO
  IF SY-SUBRC <> 0.

    SELECT SINGLE J_1BBRANCH FROM T001W INTO BRANCH
      WHERE WERKS = BRANCH.

    CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
       EXPORTING
            BRANCH      = BRANCH
            BUKRS       = BUKRS
       IMPORTING
*         address     =
            BRANCH_DATA = BRANCH_DATA
            CGC_NUMBER  = CGC_NUMBER
            ADDRESS1    = ADDRESS1
       EXCEPTIONS
            OTHERS      = 04.

*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 <<< FIM
    IF SY-SUBRC NE 0.
      MESSAGE W453 WITH BUKRS BRANCH.
    ENDIF.
  ENDIF.
* determine currency from T001
  SELECT SINGLE * FROM T001 WHERE BUKRS = BUKRS.

* set page numbering
  PAGNO = FIRSTPAG - 1.

* determination of the CFOP-version
  CONCATENATE LFGJA MONTH '01' INTO START_DATE.
  CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
    EXPORTING
      LAND1             = ADDRESS1-COUNTRY
      REGION            = ADDRESS1-REGION
      DATE              = START_DATE
    IMPORTING
      VERSION           = CFOP_VERSION
      EXTENSION         = EXT_LEN
      CFOPLENGTH        = CFOP_LENGTH
    EXCEPTIONS
      DATE_MISSING      = 1
      VERSION_NOT_FOUND = 2
      OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE E035.          " Cannot find a valid CFOP version
  ENDIF.

*----------------------------------------------------------------------*
* Check if new period closing is active for selected period
* Furthermore, take care if a special fiscal year variant is used
*----------------------------------------------------------------------*
*  SELECT SINGLE periv FROM t001 INTO periv WHERE bukrs = bukrs. "720396
  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      I_DATE         = DATUM_LOW
      I_PERIV        = PERIV
    IMPORTING
      E_BUPER        = BUPER
      E_GJAHR        = GJAHR
    EXCEPTIONS
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      OTHERS         = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'J_1B_CHECK_NEW_PERIOD_CLOSING'
    EXPORTING
      I_BUKRS           = BUKRS
      I_LFGJA           = GJAHR
      I_LFMON           = BUPER+1(2)
      I_CALLER          = MODELO_3
    EXCEPTIONS
      PERIOD_NOT_CLOSED = 1
      NOT_POSSIBLE      = 2
      NO_MARV_FOR_BUKRS = 3
      ONLY_MBEW         = 4
      FUTURE_PERIOD     = 5
      OTHERS            = 6.
  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE W474 WITH MONTH LFGJA.
    WHEN 2.
      MESSAGE E475.
    WHEN 3.
      MESSAGE E476 WITH BUKRS.
    WHEN 4.
      ONLY_MBEW = 'X'.
    WHEN 5.
      MESSAGE E477 WITH MONTH LFGJA.
    WHEN OTHERS.
* ??????????????????
  ENDCASE.

*----------------------------------------------------------------------*
*-----------------------start-of-selection-----------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

  SET COUNTRY 'BR'.

*----------------------------------------------------------------------*
*-----Delete messages collected up to now, collect future messages-----*
*----------------------------------------------------------------------*
  CALL FUNCTION 'MESSAGES_INITIALIZE'.

*--------------------------------------------------- BOI note 617905 --*
* build an instance of the object -------------------------------------*
*----------------------------------------------------------------------*
  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    CHANGING
      INSTANCE = IF_EX_BADI_J_1BLEGALREPORT.       " EOI note 617905

*----------------------------------------------------------------------*
*-----------------------fill table intj_1baj---------------------------*
*----------------------------------------------------------------------*
  SELECT * FROM J_1BAJ INTO TABLE INTJ_1BAJ ORDER BY PRIMARY KEY.

  IF SY-SUBRC NE 0.
    MESSAGE A460.
  ENDIF.

*----------------------------------------------------------------------*
*--------fill table intt604n with texts of NBM codes ------------------*
*----------------------------------------------------------------------*
  SELECT * FROM T604N INTO TABLE INTT604N WHERE SPRAS = SY-LANGU
                                            AND LAND1 = COUNTRY_BR
                                            ORDER BY PRIMARY KEY.

  IF SY-SUBRC NE 0.
    MESSAGE W473. " ( An error occured while reading table T604N )
  ENDIF.

*----------------------------------------------------------------------*
*-------read t001w to determine plants assigned to chosen branch;------*
*----------------fill table 'plants' with these plants-----------------*
*----------------------------------------------------------------------*
  SELECT BWKEY FROM T001K INTO LV_BWKEY WHERE BUKRS = BUKRS. "1487825
    SELECT * FROM T001W WHERE J_1BBRANCH = BRANCH           "1487825
                               AND BWKEY = LV_BWKEY.        "1487825
      APPEND T001W-WERKS TO PLANTS.                         "1487825
    ENDSELECT.                                              "1487825
  ENDSELECT.                                                "1487825

*----------------------------------------------------------------------*
*-------------- fill table it156w (posting string values) -------------*
*----------------------------------------------------------------------*
* To find postings with valuation string, but without relevance for
* the valuated stock, Big-G recommended this logic:
* Take lines from MSEG where for the combination BUSTW/XAUTO=XBGBB
* there is an entry in T156W with key BSX.
  SELECT BUSTW XBGBB FROM T156W
                     INTO CORRESPONDING FIELDS OF TABLE IT156W
                     WHERE VORSL = 'BSX'.
  SORT IT156W BY BUSTW XBGBB.
  DELETE ADJACENT DUPLICATES FROM IT156W.
  DELETE IT156W WHERE BUSTW = SPACE.

*----------------------------------------------------------------------*
*---- fill tables lt_txip1, lt_txip2 and lt_txip3 (IPI tax rates) -----*
*----------------------------------------------------------------------*
* invert date
  MOVE DATUM_HIGH TO INVERTED_DATE.
  TRANSLATE INVERTED_DATE USING COMP_NINE.

* read IPI taxrate into internal tables
  SELECT * FROM J_1BTXIP1 INTO TABLE LT_TXIP1
      WHERE VALIDFROM GT INVERTED_DATE.                   "#EC PORTABLE
  SORT LT_TXIP1 BY NBMCODE VALIDFROM.
  DELETE ADJACENT DUPLICATES FROM LT_TXIP1 COMPARING NBMCODE.

  SELECT * FROM J_1BTXIP2 INTO TABLE LT_TXIP2
      WHERE VALIDFROM GT INVERTED_DATE.                   "#EC PORTABLE
  SORT LT_TXIP2 BY MATNR VALIDFROM.
  DELETE ADJACENT DUPLICATES FROM LT_TXIP2 COMPARING MATNR.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_TXIP3
              FROM J_1BTXIP3 AS A INNER JOIN J_1BTXGRUOP AS B
              ON A~GRUOP = B~GRUOP
              WHERE ( B~FIELD = 'MATNR' OR B~FIELD = 'NBM' )
                AND A~VALIDFROM GT INVERTED_DATE.         "#EC PORTABLE
  SORT LT_TXIP3 BY VALUE VALIDFROM.
  DELETE ADJACENT DUPLICATES FROM LT_TXIP3 COMPARING VALUE.

*----------------------------------------------------------------------*
*------------continue only if plants for chosen branch exist-----------*
*----------------------------------------------------------------------*
  DESCRIBE TABLE PLANTS LINES LINES.

  CHECK LINES > 0.

* begin of note 1043677
  RANGES JR_WERKS FOR T001W-WERKS.
* fill selection for Document type
  JR_WERKS-SIGN   = 'I'.
  JR_WERKS-OPTION = 'EQ'.
  LOOP AT PLANTS.
    JR_WERKS-LOW = PLANTS-WERKS.
    APPEND JR_WERKS.
  ENDLOOP.
* end of note 1043677

*----------------------------------------------------------------------*
*-- create internal table it_mdata of possible data to be reported ----*
*----------------------------------------------------------------------*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MDATA
        FROM MKPF AS A INNER JOIN MSEG AS B
           ON A~MBLNR = B~MBLNR AND A~MJAHR = B~MJAHR
*           FOR ALL ENTRIES IN plants                     " note 1043677
           WHERE A~BUDAT BETWEEN DATUM_LOW AND DATUM_HIGH
             AND A~MBLNR IN MBLNR
*             AND b~mjahr = lfgja                          " note 720396
             AND B~MJAHR = DATUM_LOW(4)                    " note 720396
             AND B~BUKRS = BUKRS
             AND B~MATNR IN MAT
             AND B~BWART IN MVTYP
             AND B~MENGE <> 0   " choose only movements with quantities
*             AND b~werks = plants-werks.   "only refering to sel. bupla
                                            " note 1043677
             AND B~WERKS IN JR_WERKS.   "only refering to sel. bupla
  " note 1043677

*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 >>> INICIO
*  DELETE IT_MDATA WHERE WERKS NE BRANCH.
  DELETE IT_MDATA WHERE WERKS NE BRANCH_CENTRO.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 <<< FIM

  SORT IT_MDATA.

  LOOP AT IT_MDATA.

    L_TABIX = SY-TABIX.                              " note 797729

* Eliminate material documents with valuation string, but without
* relevance to the valuated stock. IT156W contains all valuation
* strings with posting key BSX. XBGBB says: "I am an accrural posting".
* For more details please ask Big-G.
    READ TABLE IT156W        WITH KEY
                             BUSTW = IT_MDATA-BUSTW
                             XBGBB = IT_MDATA-XAUTO
                             TRANSPORTING NO FIELDS
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_MDATA-VAL_PROC_IND = 'X'.
    ELSE.
      CLEAR IT_MDATA-VAL_PROC_IND.
    ENDIF.
    IT_MDATA-NOMVT = ' '.                                   "1487825

    MOVE IT_MDATA-BLDAT+6(2)    TO IT_MDATA-DAY.

    MODIFY IT_MDATA.

* note 797729: Set an indicator for internal material material documents
* i.e. posted goods issues that are cancelled
    IF NOT IT_MDATA-SMBLN IS INITIAL.
      READ TABLE IT_MDATA WITH KEY MATNR = IT_MDATA-MATNR
                                   WERKS = IT_MDATA-WERKS
                                   BWTAR = IT_MDATA-BWTAR
                                   MBLNR = IT_MDATA-SMBLN
*                                   zeile = it_mdata-zeile " note 942077
                                   ZEILE = IT_MDATA-SMBLP  " note 942077
                                   SMBLN = ' '
                                   MENGE = IT_MDATA-MENGE.
      IF SY-SUBRC IS INITIAL.
        IT_MDATA-INT_DOC_IND = 'X'.
        MODIFY IT_MDATA INDEX SY-TABIX.
        READ TABLE IT_MDATA INDEX L_TABIX.
        IT_MDATA-INT_DOC_IND = 'X'.
        MODIFY IT_MDATA INDEX SY-TABIX.
      ENDIF.
    ENDIF.                                            " EOI note 797729

    " Subcontracting                                      "1523305
    CASE IT_MDATA-BWART.                                    "1523305
        " Outbound movements                                "1523305
      WHEN '541'.                                         "1523305 "1555110
        IT_MDATA-SUBCONTRACT = 'X'.                         "1523305
        MODIFY IT_MDATA.                                    "1523305
      WHEN '543'.                                         "1523305 "1555110
        " Remove symbolic return                          "1523305
        DELETE IT_MDATA.                                    "1523305
      WHEN OTHERS.                                          "1523305
        " Identify inbound movements that correspond to a "1523305
        " subcontracting operation                        "1523305
        IF NOT IT_MDATA-EBELP IS INITIAL.                   "1523305
          SELECT SINGLE * FROM EKPO                         "1523305
            INTO LV_EKPO                                    "1523305
          WHERE EBELN = IT_MDATA-EBELN AND                  "1523305
                EBELP = IT_MDATA-EBELP AND                  "1523305
                PSTYP = 3.                                  "1523305
                                                            "1523305
          IF SY-SUBRC = 0.                                  "1523305
            IT_MDATA-SUBCONTRACT = 'X'.                     "1523305
            MODIFY IT_MDATA.                                "1523305
          ENDIF.                                            "1523305
        ENDIF.                                              "1523305
    ENDCASE.                                                "1523305

  ENDLOOP.

***** Material de consumo *******

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MDATA_CONS
         FROM MKPF AS A INNER JOIN MSEG AS B
            ON A~MBLNR = B~MBLNR AND A~MJAHR = B~MJAHR
*           FOR ALL ENTRIES IN plants                     " note 1043677
            WHERE A~BUDAT BETWEEN DATUM_LOW AND DATUM_HIGH
              AND A~MBLNR IN MBLNR
*             AND b~mjahr = lfgja                          " note 720396
              AND B~MJAHR = DATUM_LOW(4)                    " note 720396
              AND B~BUKRS = BUKRS
              AND B~MATNR IN MATCONS
              AND B~BWART IN BWARTCON
              AND B~MENGE <> 0   " choose only movements with quantities
*             AND b~werks = plants-werks.   "only refering to sel. bupla
                                             " note 1043677
              AND B~WERKS IN JR_WERKS.   "only refering to sel. bupla
  " note 1043677

*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 >>> INICIO
*  DELETE IT_MDATA_CONS WHERE WERKS NE BRANCH.
  DELETE IT_MDATA_CONS WHERE WERKS NE BRANCH_CENTRO.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 <<< FIM
  SORT IT_MDATA_CONS.

  LOOP AT IT_MDATA_CONS.

    L_TABIX = SY-TABIX.                              " note 797729

* Eliminate material documents with valuation string, but without
* relevance to the valuated stock. IT156W contains all valuation
* strings with posting key BSX. XBGBB says: "I am an accrural posting".
* For more details please ask Big-G.
    READ TABLE IT156W        WITH KEY
                             BUSTW = IT_MDATA_CONS-BUSTW
                             XBGBB = IT_MDATA_CONS-XAUTO
                             TRANSPORTING NO FIELDS
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_MDATA_CONS-VAL_PROC_IND = 'X'.
    ELSE.
      CLEAR IT_MDATA_CONS-VAL_PROC_IND.
    ENDIF.
    IT_MDATA_CONS-NOMVT = ' '.                              "1487825

    MOVE IT_MDATA_CONS-BLDAT+6(2)    TO IT_MDATA_CONS-DAY.

    MODIFY IT_MDATA_CONS.

* note 797729: Set an indicator for internal material material documents
* i.e. posted goods issues that are cancelled
    IF NOT IT_MDATA_CONS-SMBLN IS INITIAL.
      READ TABLE IT_MDATA_CONS WITH KEY MATNR = IT_MDATA_CONS-MATNR
                                   WERKS = IT_MDATA_CONS-WERKS
                                   BWTAR = IT_MDATA_CONS-BWTAR
                                   MBLNR = IT_MDATA_CONS-SMBLN
*                                   zeile = it_mdata-zeile " note 942077
                                   ZEILE = IT_MDATA_CONS-SMBLP  " note 942077
                                   SMBLN = ' '
                                   MENGE = IT_MDATA_CONS-MENGE.
      IF SY-SUBRC IS INITIAL.
        IT_MDATA_CONS-INT_DOC_IND = 'X'.
        MODIFY IT_MDATA_CONS INDEX SY-TABIX.
        READ TABLE IT_MDATA_CONS INDEX L_TABIX.
        IT_MDATA_CONS-INT_DOC_IND = 'X'.
        MODIFY IT_MDATA_CONS INDEX SY-TABIX.
      ENDIF.
    ENDIF.                                            " EOI note 797729

    " Subcontracting                                      "1523305
    CASE IT_MDATA_CONS-BWART.                               "1523305
        " Outbound movements                                "1523305
      WHEN '541'.                                         "1523305 "1555110
        IT_MDATA_CONS-SUBCONTRACT = 'X'.                    "1523305
        MODIFY IT_MDATA_CONS.                               "1523305
      WHEN '543'.                                         "1523305 "1555110
        " Remove symbolic return                          "1523305
        DELETE IT_MDATA_CONS.                               "1523305
      WHEN OTHERS.                                          "1523305
        " Identify inbound movements that correspond to a "1523305
        " subcontracting operation                        "1523305
        IF NOT IT_MDATA_CONS-EBELP IS INITIAL.              "1523305
          SELECT SINGLE * FROM EKPO                         "1523305
            INTO LV_EKPO                                    "1523305
          WHERE EBELN = IT_MDATA_CONS-EBELN AND             "1523305
                EBELP = IT_MDATA_CONS-EBELP AND             "1523305
                PSTYP = 3.                                  "1523305
                                                            "1523305
          IF SY-SUBRC = 0.                                  "1523305
            IT_MDATA_CONS-SUBCONTRACT = 'X'.                "1523305
            MODIFY IT_MDATA_CONS.                           "1523305
          ENDIF.                                            "1523305
        ENDIF.                                              "1523305
    ENDCASE.                                                "1523305

  ENDLOOP.


*----------------------------------------------------------------------*
*-------------------------sort extract---------------------------------*
*----------------------------------------------------------------------*
  PERFORM WITHOUT_MOVEMENT.                                 "1487825
                                                            "1487825
  SORT IT_MDATA.
  SORT IT_MDATA_CONS.

  CLEAR OLDINFO.
  LOOP AT IT_MDATA.
    MOVE-CORRESPONDING IT_MDATA TO IT_MAT_BUDAT.
    APPEND IT_MAT_BUDAT.
  ENDLOOP.

  LOOP AT IT_MDATA_CONS.
    MOVE-CORRESPONDING IT_MDATA_CONS TO IT_MAT_BUDAT.
    APPEND IT_MAT_BUDAT.
  ENDLOOP.

  DATA:WL_TABIX TYPE SY-TABIX.
  SORT: IT_MAT_BUDAT BY BUDAT,
       IT_MDATA BY MATNR BUDAT.
  CLEAR WL_TABIX.

  LOOP AT MAT.
    LOOP AT IT_MDATA_CONS.
      LOOP AT IT_MDATA
        TRANSPORTING NO FIELDS
         WHERE MATNR EQ MAT-LOW
           AND BUDAT GE IT_MDATA_CONS-BUDAT.

        WL_TABIX = SY-TABIX.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ADD 1 TO WL_TABIX.
      IF IT_MDATA_CONS-MENGE LT 0.
        MULTIPLY IT_MDATA_CONS-MENGE BY -1.
      ENDIF.
      MOVE-CORRESPONDING: IT_MDATA_CONS TO IT_MDATA.
      MOVE: MAT-LOW             TO IT_MDATA-MATNR,
            IT_MDATA_CONS-MATNR TO IT_MDATA-MATNR2.
      INSERT IT_MDATA INDEX  WL_TABIX.
    ENDLOOP.
  ENDLOOP.

  IF IT_MDATA[] IS NOT INITIAL.
    SELECT MATNR MAKTX
      FROM MAKT
      INTO TABLE IT_MAKT
       FOR ALL ENTRIES IN IT_MDATA
        WHERE MATNR EQ IT_MDATA-MATNR.

    SELECT *
      FROM ZFIT0028
      INTO TABLE IT_0028
        FOR ALL ENTRIES IN IT_MDATA
        WHERE BWART EQ IT_MDATA-BWART.

  ENDIF.

  IF IT_MDATA_CONS[] IS NOT INITIAL.
    SELECT MATNR MAKTX
      FROM MAKT
      APPENDING TABLE IT_MAKT
       FOR ALL ENTRIES IN IT_MDATA_CONS
        WHERE MATNR EQ IT_MDATA_CONS-MATNR.
  ENDIF.
  IT_MDATA_F50[] = IT_MDATA[].
  DELETE IT_MDATA_F50 WHERE BWART NE 'F50'.
  IF IT_MDATA_F50[] IS NOT INITIAL.
    SELECT MBLNR MJAHR BKTXT BKTXT
      FROM MKPF
      INTO TABLE IT_MKPF_NFE
       FOR ALL ENTRIES IN IT_MDATA_F50
        WHERE MBLNR EQ IT_MDATA_F50-MBLNR
          AND MJAHR EQ IT_MDATA_F50-MJAHR
          AND BKTXT NE SPACE.

    IF SY-SUBRC IS INITIAL.
      SELECT VGBEL VBELN VBELN
        FROM VBRP
        INTO TABLE IT_VBRP_NFE
         FOR ALL ENTRIES IN IT_MKPF_NFE
          WHERE VGBEL EQ IT_MKPF_NFE-VGBEL AND DRAFT = SPACE .

      IF SY-SUBRC IS INITIAL .
        SELECT REFKEY DOCNUM CFOP
          FROM J_1BNFLIN
          INTO TABLE IT_BNFLIN_NFE
           FOR ALL ENTRIES IN IT_VBRP_NFE
            WHERE REFKEY EQ IT_VBRP_NFE-REFKEY.

        IF SY-SUBRC IS INITIAL.
          SELECT DOCNUM NFENUM
             FROM J_1BNFDOC
             INTO TABLE IT_BNFDOC_NFE
              FOR ALL ENTRIES IN IT_BNFLIN_NFE
               WHERE DOCNUM EQ IT_BNFLIN_NFE-DOCNUM.


        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.
  SORT: IT_MKPF_NFE    BY MBLNR MJAHR,
        IT_VBRP_NFE    BY VGBEL,
        IT_BNFLIN_NFE  BY REFKEY,
        IT_BNFDOC_NFE  BY DOCNUM.
*----------------------------------------------------------------------*
*--------------------generation/output of Modelo 3:--------------------*
*-------(1) material info (material description + initial stock)-------*
*---(2) per material (on val. type level): movements in chosen month---*
*------ taken from the internal table it_mdata (mkpf/mseg data) -------*
*----------------------------------------------------------------------*
  LOOP AT IT_MDATA.
    LCL_TABIX = SY-TABIX.

    IF IT_MDATA-MATNR2 IS NOT INITIAL.
      WG_CONSUMO = 'X'.
    ELSE.
      CLEAR WG_CONSUMO.
    ENDIF.
*----------------------------------------------------------------------*
*--- at new matnr: Reading of relevant material information -----------*
*----------------------------------------------------------------------*
    READ TABLE IT_MDATA_CONS TRANSPORTING NO FIELDS
      WITH KEY MATNR = IT_MDATA-MATNR.
    IF SY-SUBRC IS NOT INITIAL.
      AT NEW MATNR.

        READ TABLE IT_MDATA INDEX LCL_TABIX.

* read MARA information to determine unit of measure
        CLEAR MARA.
        SELECT SINGLE * FROM MARA WHERE MATNR = IT_MDATA-MATNR.

* read MAKT for material description
        CLEAR MAKT.
        SELECT SINGLE * FROM MAKT WHERE MATNR = IT_MDATA-MATNR
                                     AND SPRAS = SY-LANGU.

* read MARC information to determine nbm code
        CLEAR MARC.
        SELECT SINGLE * FROM MARC WHERE MATNR = IT_MDATA-MATNR
                                  AND WERKS = IT_MDATA-WERKS.

        IF MARC-STEUC IS INITIAL.
* write message to errorlog
          CALL FUNCTION 'MESSAGE_STORE'
            EXPORTING
              ARBGB = '8B'
              MSGTY = 'E'
              MSGV1 = IT_MDATA-MATNR
              MSGV2 = IT_MDATA-WERKS
              TXTNR = '472'.
        ENDIF.

* read NBM code description from internal table intt604n
        READ TABLE INTT604N WITH KEY SPRAS = SY-LANGU
                                     LAND1 = COUNTRY_BR
                                     STEUC = MARC-STEUC.
        IF SY-SUBRC = 0.
          MOVE INTT604N-TEXT1 TO TEXT1.
        ELSE.
* write message to errorlog
          CALL FUNCTION 'MESSAGE_STORE'
            EXPORTING
              ARBGB = '8B'
              MSGTY = 'E'
              MSGV1 = MARC-STEUC
              MSGV2 = SY-LANGU
              TXTNR = '471'.
          CLEAR TEXT1.
        ENDIF.

* read IPI tax rate for material header information
* access order: J_1BTXIP2, J_1BTXIP3, J_1BTXIP1
        CLEAR: OLDINFO-IPIRATE.
        READ TABLE LT_TXIP2 WITH KEY MATNR = IT_MDATA-MATNR.
        IF SY-SUBRC = 0.
          OLDINFO-IPIRATE = LT_TXIP2-RATE.
        ELSE.
          LOOP AT LT_TXIP3.
            IF ( LT_TXIP3-VALUE = MARC-STEUC
                              OR LT_TXIP3-VALUE = IT_MDATA-MATNR ).
              OLDINFO-IPIRATE = LT_TXIP3-RATE.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF OLDINFO-IPIRATE IS INITIAL.
          READ TABLE LT_TXIP1 WITH KEY NBMCODE = MARC-STEUC.
          IF SY-SUBRC = 0.
            OLDINFO-IPIRATE = LT_TXIP2-RATE.
          ENDIF.
        ENDIF.

        MOVE-CORRESPONDING MARA TO OLDINFO.
        IF NOT MAKT IS INITIAL.
          MOVE-CORRESPONDING MAKT TO OLDINFO.
        ENDIF.
        IF IT_MDATA-VAL_PROC_IND <> 'X'.                    "1487825
          CONCATENATE MARC-STEUC(10) '-' OLDINFO-IPIRATE '%' "1487825
                                           INTO OLDINFO-NBM. "1487825
          MOVE TEXT1 TO OLDINFO-TEXT1.                      "1487825
        ELSE.                                               "1487825
          MOVE MARC-STEUC(10) TO OLDINFO-NBM.               "1487825
        ENDIF.                                              "1487825
        MOVE TEXT1 TO OLDINFO-TEXT1.
      ENDAT.
    ENDIF.

*----------------------------------------------------------------------*
*--- at new bwtar: Reading of material valuation data -----------------*
*----------------------------------------------------------------------*
    AT NEW BWTAR.

* set first_record_mat to 'X'; clearing after the first printed record
      FIRST_RECORD_MAT = 'X'.

* initialisation of summaries
      CLEAR SUMS.

      READ TABLE IT_MDATA INDEX LCL_TABIX.

      CLEAR SKIP_MATERIAL.

* read MBEW information to determine initial/final quantities
      SELECT SINGLE * FROM MBEW
                  WHERE MATNR = IT_MDATA-MATNR
                    AND BWKEY = IT_MDATA-WERKS
                    AND BWTAR = IT_MDATA-BWTAR.

      IF MBEW-MTUSE = 2                "exclude consumption materials
        OR MBEW-MTUSE = 3.             "exclude assets
        SKIP_MATERIAL = 'X'.
      ENDIF.

      IF SKIP_MATERIAL IS INITIAL.

        IF ONLY_MBEW IS INITIAL.
* read mbewh information (if available)
*          CALL FUNCTION 'J_1B_READ_MBEW_MBEWH'      " begin note 862318
*               EXPORTING
*                    i_mbew       = mbew
*               IMPORTING
*                    e_mbew       = mbew
          CALL FUNCTION 'J_1B_READ_MBEW_MBEWH_NEW'
            EXPORTING
              I_MBEW       = MBEW
              I_B_SVS      = 'X'   " display_line-b_svs
            IMPORTING
              E_MBEW       = MBEW
              E_EQBEW      = EQBEWSAVE         " end note 862318
            EXCEPTIONS
              NO_SELECTION = 1
              OTHERS       = 2.
        ENDIF.
        IF SY-SUBRC = 0.

          IF EQBEWSAVE-LBKUM <> 0 OR EQBEWSAVE-VMKUM <> 0.  " nt. 862318
            MBEW-VMKUM = MBEW-VMKUM + EQBEWSAVE-VMKUM.      " nt. 862318
            MBEW-LBKUM = MBEW-LBKUM + EQBEWSAVE-LBKUM.      " nt. 862318
          ENDIF.                                            " nt. 862318
          IF EQBEWSAVE-VMSAL <> 0 OR EQBEWSAVE-SALK3 <> 0.  " nt. 869152
            MBEW-VMSAL = MBEW-VMSAL + EQBEWSAVE-VMSAL.      " nt. 869152
            MBEW-SALK3 = MBEW-SALK3 + EQBEWSAVE-SALK3.      " nt. 869152
          ENDIF.                                            " nt. 869152

          MOVE: IT_MDATA-WERKS TO OLDINFO-WERKS.
          MOVE: MBEW-LBKUM TO OLDINFO-LBKUM,
                MBEW-SALK3 TO OLDINFO-SALK3.
          IF IT_MDATA-NOMVT <> 'X'.                         "1487825
            MOVE: MBEW-VMKUM TO OLDINFO-VMKUM,              "1487825
                  MBEW-VMSAL TO OLDINFO-VMSAL.              "1487825
          ELSE.                                             "1487825
            MOVE: MBEW-LBKUM TO OLDINFO-VMKUM,              "1487825
                  MBEW-SALK3 TO OLDINFO-VMSAL.              "1487825
          ENDIF.                                            "1487825
          MOVE: MBEW-BWTAR TO OLDINFO-BWTAR.

* initial and final stock
          IF PAGEBRK    =    'X'.
            NEW-PAGE.
          ENDIF.

* procedure above delivers wrong initial value in case of (1) no further
* MBEWH entries from upper limit of reporting period until today (->
* MBEW record) and (2) only unvaluated movements in the reporting period
          READ TABLE IT_MDATA WITH KEY MATNR = IT_MDATA-MATNR
                                       WERKS = IT_MDATA-WERKS
                                       BWTAR = IT_MDATA-BWTAR
                                       VAL_PROC_IND = 'X'
                               TRANSPORTING NO FIELDS.
          IF SY-SUBRC <> 0.
            OLDINFO-VMKUM = OLDINFO-LBKUM.
            OLDINFO-VMSAL = OLDINFO-SALK3.
          ENDIF.
* bod note 821403
* printing of the material header (incl. initial/final quantities)
*          PERFORM output_head.

* printing of the initial values (in separate line)
*         FORMAT INTENSIFIED COLOR COL_GROUP. " accessability
*          FORMAT INTENSIFIED COLOR COL_TOTAL. " accessability
*          PERFORM carry_over USING text-290 oldinfo-vmkum
*                                   oldinfo-vmsal oldinfo-vmkum.
* eod note 821403
        ELSE.
          SKIP_MATERIAL = 'X'.
        ENDIF.
      ENDIF.
    ENDAT.

*----------------------------------------------------------------------*
*------------- movement info: Display of the line items ---------------*
*----------------------------------------------------------------------*
    IF SKIP_MATERIAL IS INITIAL.
      PERFORM MOVEMENT_INFO USING IT_MDATA.
    ENDIF.

*----------------------------------------------------------------------*
*------------------- at end of bwtar: report of -----------------------*
* (a) total de entradas, (b) saidas and (c) saldo (= final quantities) *
*----------------------------------------------------------------------*
    AT END OF BWTAR.
* boi note 821403
      CLEAR: SAIDA,ENTRDA.

      PERFORM ALV_OUTPUT.

      CLEAR : ALV_OUT, PREV_FORMOUT_ESTOQUE.
      REFRESH ALV_OUT .
* eoi note 821403
* bod note 821403
*      IF skip_material IS INITIAL.
*       FORMAT COLOR COL_GROUP.                     " accessability
*        FORMAT INTENSIFIED OFF COLOR COL_TOTAL.     " accessability
*       PERFORM carry_over USING text-292           " Total de entradas:
*                                 sums-quant_entr_1 dummy dummy.
*        PERFORM carry_over USING text-293           " Total de saídas:
*                                 sums-quant_said_1 dummy dummy.
*       FORMAT INTENSIFIED COLOR COL_GROUP.         " accessability
*        FORMAT INTENSIFIED COLOR COL_TOTAL .        " accessability
*        PERFORM carry_over USING text-291           " Saldo:
*                            oldinfo-lbkum oldinfo-salk3 oldinfo-lbkum.
*        WRITE:/(134) sy-uline.                      " accessability

*        IF check_acc_estoque <> oldinfo-lbkum AND
*           first_record_mat IS INITIAL AND
*           mblnr[] IS INITIAL AND mvtyp[] IS INITIAL.
* Incorrect reported stocks for material &1
*          CALL FUNCTION 'MESSAGE_STORE'
*               EXPORTING
*                    arbgb = '8B'
*                    msgty = 'E'
*                    msgv1 = it_mdata-matnr
*                    msgv2 = check_acc_estoque
*                    msgv3 = oldinfo-lbkum
*                    txtnr = '498'.
*        ENDIF.
*      ENDIF.
* eod note 821403
    ENDAT.

  ENDLOOP.

*----------------------------------------------------------------------*
*-------------------------- check if book is empty --------------------*
*----------------------------------------------------------------------*
  IF FIRST_RECORD_REP = 'X'.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE AT (SY-LINSZ) NO_DATA.          "no values were selected
    CHECK '0' = '1'.
  ENDIF.

*----------------------------------------------------------------------*
*--------- write texts for movements at the end of the report ---------*
*----------------------------------------------------------------------*
  NEW-PAGE.

  SORT MOVEMENTS.

  DESCRIBE TABLE MOVEMENTS LINES LINES.

  IF LINES > 0.
    MOVTAB = 'X'.
    ULINE.
    SKIP.                                           " accessability
    ULINE.                                          " accessability
    FORMAT INTENSIFIED OFF COLOR COL_HEADING.
*   WRITE:/ text-280.                           " boi accessability
    WRITE:/ SY-VLINE,
            2 TEXT-280,                                     "1487825
            134 SY-VLINE.                       " eoi accessability
    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
    ULINE.
    LOOP AT MOVEMENTS.
      SELECT SINGLE * FROM T156T WHERE SPRAS = SY-LANGU
                                   AND BWART = MOVEMENTS-BWART
                                   AND SOBKZ = MOVEMENTS-SOBKZ
                                   AND KZBEW = MOVEMENTS-KZBEW
                                   AND KZZUG = MOVEMENTS-KZZUG
                                   AND KZVBR = MOVEMENTS-KZVBR.

      WRITE: /5(5)  T156T-BWART,
              11(1)  '|',
              13(1)  T156T-SOBKZ,
              15(1)  '|',
              17(1)  T156T-KZBEW,
              19(1)  '|',
              21(1)   T156T-KZZUG,
              23(1)  '|',
              25(1)   T156T-KZVBR,
              27(1)  '|',
              29(30) T156T-BTEXT,
              134    SY-VLINE.                      " accessability

    ENDLOOP.
    ULINE.                                           " accessability
  ENDIF.

*----------------------------------------------------------------------*
*---------------------------top-of-page--------------------------------*
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  PAGNO = PAGNO + 1.
  IF PAGNO > BOOKSIZE.
    PAGNO = 2.
  ENDIF.
* boi accessability
*  FORMAT INTENSIFIED COLOR COL_HEADING.
** Administrative information
*  IF reptitl IS INITIAL.
*    WRITE: /20  text-200,   " LIVRO REGISTRO CONTROLE DA PRODUÇÃO E ...
*           /20  text-201.                                   "
*  ELSE.
*    WRITE: /20  text-202,   " CONTROLE QUANTITATIVO DA PRODUÇÃO E DO ..
*.
*           /20  text-201.                                   "
*  ENDIF.
*  WRITE: /1   text-210,                " Firma             :  .......
*          22  branch_data-name.
*  WRITE: /1   text-220,                " Inscrição Estadual:
*          22  branch_data-state_insc,
*          46  text-230,   " CNPJ    :  | 2 - Em outro estabelecimento..
*.
*          66(18)  cgc_number.
*  WRITE: /1   text-240,
*         22   pagno,
*         46   text-250,
*         66(2) month,
*         68(1) '/',
*         69(4) lfgja.
*  FORMAT INTENSIFIED COLOR COL_HEADING .
*          IF movtab IS INITIAL.
*         ULINE.
*    WRITE:/ text-270,   "  Documento | Lançamento | Entrad...........
*          / text-271,   " ---------------------------------------....
*          / text-272,   "   |Serie|    |     |   |  Codificação  |...
*          / text-273,   " Especie| Sub-|Número    |   Data   |Día|....
*          / text-274.   "   |Serie|     |     |   |Contábil|Fiscal|...

  FORMAT INTENSIFIED COLOR COL_NORMAL .
  WRITE: /(134) SY-ULINE.
* Administrative information
  IF REPTITL IS INITIAL.
    WRITE: /    SY-VLINE,
            20  TEXT-620 COLOR COL_NORMAL INTENSIFIED ,
                         " LIVRO REGISTRO CONTROLE DA PRODUÇÃO E ...
            91  SY-VLINE,
            92  TEXT-613 COLOR COL_HEADING INTENSIFIED ,
            134 SY-VLINE.
    FORMAT COLOR COL_GROUP INTENSIFIED OFF.
    WRITE: / SY-VLINE ,
            20  TEXT-201 ,
            134 SY-VLINE.
  ELSE.
    WRITE: / SY-VLINE,
            20  TEXT-621,  " CONTROLE QUANTITATIVO DA PRODUÇÃO E DO
            91 SY-VLINE,
            92 TEXT-614.
    FORMAT COLOR COL_GROUP INTENSIFIED OFF.
    WRITE: / SY-VLINE,
            20  TEXT-201 COLOR COL_GROUP INTENSIFIED OFF,
            134 SY-VLINE.
  ENDIF.

  WRITE: /    SY-VLINE,
          2   TEXT-610 COLOR COL_GROUP INTENSIFIED,         "1487825
                         " Firma    :  .......
          22  BRANCH_DATA-NAME COLOR COL_GROUP INTENSIFIED OFF,
          91  SY-VLINE,
          92  TEXT-614 COLOR COL_HEADING INTENSIFIED,
          134 SY-VLINE.

  WRITE: /       SY-VLINE,
          2     TEXT-220 COLOR COL_GROUP INTENSIFIED,       "1487825
                         " Inscrição Estadual:
          22     BRANCH_DATA-STATE_INSC COLOR COL_GROUP INTENSIFIED OFF,
          46     TEXT-611 COLOR COL_GROUP INTENSIFIED,
                         " CNPJ    :  | 2 - Em outro estabelecimento...
          66(18) CGC_NUMBER COLOR COL_GROUP INTENSIFIED OFF,
          91     SY-VLINE,
          92     TEXT-615 COLOR COL_HEADING INTENSIFIED ,
          134    SY-VLINE.

  WRITE: /     SY-VLINE,
          2    TEXT-240 COLOR COL_GROUP INTENSIFIED,        "1487825
         22    PAGNO COLOR COL_GROUP INTENSIFIED OFF,
         46    TEXT-612 COLOR COL_GROUP INTENSIFIED,
*         66(2) month COLOR COL_GROUP INTENSIFIED OFF,     " note 804806
         66(2) DATUM_HIGH+4(2) COLOR COL_GROUP INTENSIFIED OFF, " 804806
         68(1) '/',
*         69(4) lfgja COLOR COL_GROUP INTENSIFIED OFF,     " note 804806
         69(4) DATUM_HIGH(4) COLOR COL_GROUP INTENSIFIED OFF, " 804806
         91    SY-VLINE,
         92    TEXT-616 COLOR COL_HEADING INTENSIFIED ,
         134   SY-VLINE.
  FORMAT INTENSIFIED COLOR COL_HEADING .

  IF MOVTAB IS INITIAL.
    ULINE.

    WRITE:/ SY-VLINE,
          2 TEXT-270,   "  Documento | Lançamento | Entrad...........
          134 SY-VLINE,
          / SY-VLINE,
          2 TEXT-271 COLOR COL_KEY INTENSIFIED,
                       " ---------------------------------------....
          134 SY-VLINE,
          / SY-VLINE,
          2 TEXT-272 COLOR COL_KEY INTENSIFIED,
                       "   |Serie|    |     |   |  Codificação  |...
          134 SY-VLINE,
          / SY-VLINE,
          2 TEXT-273 COLOR COL_KEY INTENSIFIED,
                       " Especie| Sub-|Número    |   Data   |Día|....
          134 SY-VLINE,
          / SY-VLINE,
           2 TEXT-274 COLOR COL_KEY INTENSIFIED,
                       "   |Serie|     |     |   |Contábil|Fiscal|...
           134 SY-VLINE.

    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ULINE.
  ENDIF.
* eoi accessability
*----------------------------------------------------------------------*
* end-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

*----------------------------------------------------------------------*
*------output of errorlog or creation of a table with errorlist--------*
*----------------------------------------------------------------------*
  IF SY-PDEST NE ' ' AND SY-BATCH = ' '.
    CALL FUNCTION 'MESSAGES_GIVE'
      TABLES
        T_MESG = T_MESG
      EXCEPTIONS
        OTHERS = 1.
  ELSE.
    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        CORRECTIONS_OPTION    = ' '
        CORRECTIONS_FUNC_TEXT = ' '
        OBJECT                = TEXT-390
      EXCEPTIONS
        NO_MESSAGES           = 2.
  ENDIF.

*----------------------------------------------------------------------*
*      Form   OUTPUT_HEAD
*----------------------------------------------------------------------*
* creates outputline for Modelo 3: material description + initial stock
*----------------------------------------------------------------------*
*  --> structure mat_tab
*  <-- output
*----------------------------------------------------------------------*
FORM OUTPUT_HEAD.

* FORMAT INTENSIFIED COLOR COL_TOTAL.           " accessability
  FORMAT INTENSIFIED OFF COLOR COL_GROUP.       " accessability

  IF    SYST-LINNO   NE   1.
    ULINE.
  ENDIF.
* Materialnumber, -description, unit of measure, NBM Code and descript.
* boi accessability
*  WRITE:/ text-260,                    " produto
*        10(19)     oldinfo-matnr, '/',
*        30(41)     oldinfo-maktx, '/',
*        72 text-261,                   " unidade
*        92(3)      oldinfo-meins,
*        105(15)    oldinfo-werks,
*        120(10)    oldinfo-bwtar.
*
*  WRITE:/ text-262,                    " Classificação Fiscal:
*          25(20)     oldinfo-nbm,
*          47(35)     oldinfo-text1.
  WRITE:/0 SY-VLINE,
          2        TEXT-606 COLOR COL_GROUP INTENSIFIED,  "1487825 - produto
          12(19)   OLDINFO-MATNR COLOR COL_GROUP INTENSIFIED OFF , '/',
          32(41)   OLDINFO-MAKTX COLOR COL_GROUP INTENSIFIED OFF , '/',
          74       TEXT-605 COLOR COL_GROUP INTENSIFIED,     " unidade
          94(3)    OLDINFO-MEINS COLOR COL_GROUP INTENSIFIED OFF ,
          107(15)  OLDINFO-WERKS COLOR COL_GROUP INTENSIFIED OFF,
          122(10)  OLDINFO-BWTAR COLOR COL_GROUP INTENSIFIED OFF,
          134      SY-VLINE.

  WRITE:/0 SY-VLINE,
          2        TEXT-604 COLOR COL_GROUP INTENSIFIED,  "1487825 - Classificação Fiscal:
          27       OLDINFO-NBM COLOR COL_GROUP INTENSIFIED OFF,
*          49       OLDINFO-TEXT1 COLOR COL_GROUP INTENSIFIED OFF,
          134      SY-VLINE.
* eoi accessability


* Initial quantity and value
  IF MBLNR[] IS INITIAL AND MVTYP[] IS INITIAL.
* boi accessability
*    WRITE:/ text-263.  " Quantidade inicial:           Valor inicial:
*    WRITE:   21(15)    oldinfo-vmkum UNIT oldinfo-meins,
*             60(18)    oldinfo-vmsal CURRENCY t001-waers.

    WRITE:/0  SY-VLINE,
          2         TEXT-600  COLOR COL_GROUP INTENSIFIED.  "1487825
    " Quantidade inicial:           Valor inicial:

    WRITE: 23(15)   OLDINFO-VMKUM UNIT OLDINFO-MEINS,
*           46       TEXT-601 COLOR COL_GROUP INTENSIFIED,
*           61(18)   OLDINFO-VMSAL CURRENCY T001-WAERS,
           134      SY-VLINE.
* eoi accessability
  ENDIF.

* Final quantity and value
  IF MBLNR[] IS INITIAL AND MVTYP[] IS INITIAL.
* boi accessability
*    WRITE:/ text-264.  " Quantidade final  :         Valor final:
*    WRITE:   21(15)    oldinfo-lbkum UNIT oldinfo-meins,
*             60(18)    oldinfo-salk3 CURRENCY t001-waers.
    WRITE:/0 SY-VLINE,
           2        TEXT-602 COLOR COL_GROUP INTENSIFIED.   "1487825
    " Quantidade final  :         Valor final:

    WRITE: 23(15)  OLDINFO-LBKUM UNIT OLDINFO-MEINS,
*           46      TEXT-603 COLOR COL_GROUP INTENSIFIED,
*           61      OLDINFO-SALK3 CURRENCY T001-WAERS,
           134     SY-VLINE.

  ENDIF.
* eoi accessability
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

ENDFORM.                               " OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MOVEMENT_INFO
*&---------------------------------------------------------------------*
* determines relevant movement informations and issues output
*----------------------------------------------------------------------*
*  -->  current document info
*  <--  output movement info
*----------------------------------------------------------------------*
FORM MOVEMENT_INFO USING WA_MDATA STRUCTURE IT_MDATA.

  FIELD-SYMBOLS: <ITAB1>.

  CLEAR FOUND.

* fill outputline out with mseg/mkpf information
  CLEAR OUT.
  MOVE-CORRESPONDING IT_MDATA TO OUT.


  REST = OUT-MENGE.

  PERFORM DETERMINE_DIRECTION USING WA_MDATA-SHKZG.

* fill the codigo (1: own stock; 2: other stock; 3: Diversas)
  PERFORM FILL_CODIGO.

*----------------------------------------------------------------------*
* check if the movement results in a Nota Fiscal and fill up the output
* line with the corresponding Nota Fiscal information (if existing)
*----------------------------------------------------------------------*
  ASSIGN OUT TO <ITAB1>.

  IF NF_INCL = 'X'.
    IF NOT REMOVNF IS INITIAL.
      CHECK NOT WA_MDATA-VAL_PROC_IND IS INITIAL.
    ENDIF.
    IF INTDOC IS INITIAL.                         " note 797729
      CHECK WA_MDATA-INT_DOC_IND IS INITIAL.      " note 797729
    ENDIF.                                        " note 797729
    IF WA_MDATA-INT_DOC_IND IS INITIAL.           " note 797729
      PERFORM NF USING  WA_MDATA-MATNR
                      WA_MDATA-WERKS
                      WA_MDATA-BWTAR
                      WA_MDATA-MBLNR
                      WA_MDATA-ZEILE
                      WA_MDATA-MEINS
                      WA_MDATA-MENGE
                        WA_MDATA-ERFMG            " note 1137434
                        WA_MDATA-ERFME            " note 1137434
                      WA_MDATA-XBLNR                        "  773139
                    BUKRS
                      WA_MDATA-MJAHR              " note 1085056
                    'MODELO_3'
                    <ITAB1>.
    ENDIF.                                        " note 797729
  ENDIF.

* rest ? - print it!
  IF REST > 0.

    CLEAR FOUND.
    CLEAR OUT-ESP.
    CLEAR OUT-SERIES.
    CLEAR OUT-SUBSER.
    CLEAR OUT-NFNUM.
    CLEAR OUT-NFENUM.      "NFE
    CLEAR OUT-CFOP.
    CLEAR OUT-CFOP_EXT.
    CLEAR OUT-NFTOT.
    CLEAR OUT-IPIVAL.
    CLEAR OUT-IPIRATE.

    OUT-MENGE = REST.

    IF NOT CHECKFI IS INITIAL.
      CHECK NOT IT_MDATA-VAL_PROC_IND IS INITIAL.
    ENDIF.

    IF INTDOC IS INITIAL.                          " note 797729
      CHECK IT_MDATA-INT_DOC_IND IS INITIAL.       " note 797729
    ENDIF.                                         " note 797729
* bod note 821403
* summarize the quantites/values of the material documents
*    PERFORM sum_values.

*    PERFORM line_output USING out.
* eod note 821403
* boi note 821403
    MOVE-CORRESPONDING OUT TO ALV_OUT.
    APPEND ALV_OUT.
* eoi note 821403
  ENDIF.

ENDFORM.                               " MOVEMENT_INFO

*&---------------------------------------------------------------------*
*&      Form  LINE_OUTPUT
*&---------------------------------------------------------------------*
*       creates output of one line                                     *
*----------------------------------------------------------------------*
*  --> out (structure)
*  <-- outputline
*----------------------------------------------------------------------*
FORM LINE_OUTPUT USING FORMOUT STRUCTURE OUT.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  IF FORMOUT-ESP IS INITIAL.
    READ TABLE IT_0028
      WITH KEY BWART = FORMOUT-BWART.

    IF SY-SUBRC IS INITIAL.
      FORMOUT-ESP = IT_0028-ESPECIE.
    ENDIF.
  ENDIF.


  IF FORMOUT-NOMVT = 'X'.                                   "1487825
    WRITE: / SY-VLINE.                                      "1487825
    WRITE: 12 'Sem Movimento'.                              "1487825
    CHECK_ACC_ESTOQUE = OLDINFO-VMKUM.                      "1487825
    CLEAR FIRST_RECORD_REP.                                 "1487825
    CLEAR FIRST_RECORD_MAT.                                 "1487825
    WRITE: 134 SY-VLINE.                                    "1487825
    EXIT.                                                   "1487825
  ENDIF.
                                                            "1487825
*  IF found = 'X' AND NOT formout-nfnum IS INITIAL. " 821403
  IF NOT FORMOUT-FOUND IS INITIAL .                         " 821403
* boi accessability
*   WRITE: /1           formout-esp,
*           4(5)        formout-series, formout-subser,
*           10(10)      formout-nfnum.
*  ELSE.
*   WRITE: /10(10)      formout-mblnr.

    IF FORMOUT-NFE IS INITIAL.                    "NFE
      WRITE: /            SY-VLINE,
              2           FORMOUT-ESP,                      "1487825
              6(5)        FORMOUT-SERIES, FORMOUT-SUBSER,
             12(10)       FORMOUT-NFNUM.
    ELSE.                                          "NFE
* for NFe serie is always numeric (-> access key!) "NFE
      IF FORMOUT-SERIES = '000'.                   "NFE
        CLEAR FORMOUT-SERIES.                      "NFE
      ENDIF.                                       "NFE
*                                                  "NFE
      WRITE: /            SY-VLINE,                "NFE
              2           FORMOUT-ESP,             "NFE   1487825
              6(5)        FORMOUT-SERIES,          "NFE
                          FORMOUT-SUBSER,          "NFE
             12(10)       FORMOUT-NFENUM.          "NFE
    ENDIF.                                         "NFE
  ELSE.
    WRITE: / SY-VLINE,
            2           FORMOUT-ESP,             "NFE   1487825
            12(10)      FORMOUT-MBLNR.

  ENDIF.

*  WRITE: 21(10)      formout-budat,
*         32(2)       formout-day.
                                                            "1487825
  WRITE: 23(10)      FORMOUT-BUDAT.
*         34(2)       FORMOUT-DAY.
                                                            "1487825
                                                            "1487825
                                                            "1487825
* eoi accessability

*   IF found = 'X'.                                       " 821403
  IF NOT FORMOUT-FOUND IS INITIAL.                          " 821403
    SHIFT FORMOUT-CFOP LEFT DELETING LEADING '0'. " begin of note 553750
    CASE CFOP_LENGTH.
      WHEN 3.
        WRITE AT C_CFOPN(CFOP_LENGTH)    FORMOUT-CFOP.
      WHEN 4.
        WRITE AT C_CFOPN(CFOP_LENGTH)    FORMOUT-CFOP(1).
        C_CFOPX = C_CFOPN + 1.
        WRITE: AT C_CFOPX '.' NO-GAP, FORMOUT-CFOP+1(3).
    ENDCASE.
    IF NOT FORMOUT-CFOP_EXT IS INITIAL.
      C_CFOPX = C_CFOPN + CFOP_LENGTH.
      CASE EXT_LEN.
        WHEN 1.
          WRITE: AT C_CFOPX(1) '.' NO-GAP, FORMOUT-CFOP_EXT+1(1).
        WHEN 2.
          WRITE: AT C_CFOPX(1) '.' NO-GAP, FORMOUT-CFOP_EXT(2).
      ENDCASE.
    ENDIF.                                         " end of note 553750
  ENDIF.

* WRITE:  51(1)      formout-dir.                 " boi accessability
* determine codigo
*  WRITE 53(1) formout-codigo.
                                                            "1487825
  WRITE:  53(1)      FORMOUT-DIR.
* determine codigo
  WRITE 55(1) FORMOUT-CODIGO.                    " eoi accessability
                                                            "1487825

  IF ALV_OUT-MATNR2 IS INITIAL.
    READ TABLE IT_MAKT
      WITH KEY MATNR = ALV_OUT-MATNR.

    WRITE:  72(29)     IT_MAKT-MAKTX .
  ELSE.
    READ TABLE IT_MAKT
      WITH KEY MATNR = ALV_OUT-MATNR2.

    WRITE:  72(29)     IT_MAKT-MAKTX.
  ENDIF.


** if related flag on selection screen marked: clearing the NF quantity,
** valor and IPI value, when the NF doesn't affect the valuated stock.
  IF CLEARNF IS INITIAL OR NOT FORMOUT-VAL_PROC_IND IS INITIAL.
** WRITE:  56(13)   formout-menge UNIT formout-meins.      "1487825 boi accessability
    WRITE:  58(13)     FORMOUT-MENGE UNIT FORMOUT-MEINS.    "1487825
*
**    IF found = 'X'.
**      WRITE:  70(15)   formout-nftot CURRENCY t001-waers.
**      IF formout-ipival GE 0.
**        WRITE:  85(15)   formout-ipival CURRENCY t001-waers.
**      ENDIF.
**    ENDIF.
**  ENDIF.
*
**    IF found = 'X'.                            " 821403
*    IF NOT FORMOUT-FOUND IS INITIAL .                       " 821403
*      WRITE:  72(15)   FORMOUT-NFTOT CURRENCY T001-WAERS.
*      IF FORMOUT-IPIVAL GE 0.
*        WRITE:  87(15)   FORMOUT-IPIVAL CURRENCY T001-WAERS.
*      ENDIF.
*    ENDIF.
  ENDIF.                                           " eoi accessability


  IF ALV_OUT-MATNR2 IS NOT INITIAL.
    CLEAR: ALV_OUT-QUANT_ENTR_1.
  ENDIF.

* boi note 821403
* printing of the accumulated stock
*  if prev_formout_estoque is initial.          "1424817
  IF NOT FIRST_RECORD_MAT IS INITIAL.                       "1424817
    PREV_FORMOUT_ESTOQUE = OLDINFO-VMKUM.
  ENDIF.
  FORMOUT-ACC_ESTOQUE =
        PREV_FORMOUT_ESTOQUE + ALV_OUT-QUANT_ENTR_1
                                    - ALV_OUT-QUANT_SAID_1.
  PREV_FORMOUT_ESTOQUE = FORMOUT-ACC_ESTOQUE.
* eoi note 821403
* bod note 821403
* printing of the accumulated stock
*  formout-acc_estoque =
*           oldinfo-vmkum + sums-quant_entr_1 - sums-quant_said_1.
* eod note 821403

* check_acc_estoque used for control differences between MBEW final
* stock and internal accumulation
  CHECK_ACC_ESTOQUE = FORMOUT-ACC_ESTOQUE.
***  INCLUDE <icon>.
  IF FORMOUT-VAL_PROC_IND IS INITIAL.
***    WRITE: 55(2)   icon_incomplete AS ICON.
  ENDIF.

  IF ALV_OUT-MATNR2 IS INITIAL.
    IF MBLNR[] IS INITIAL AND MVTYP[] IS INITIAL.
* WRITE: 101(13)  formout-acc_estoque UNIT formout-meins. "1487825 accessability
      WRITE: 103(13) FORMOUT-ACC_ESTOQUE UNIT FORMOUT-MEINS."1487825 accessability
    ENDIF.
  ENDIF.

* bod note 821403
*  IF found = 'X' AND
*     NOT formout-nfnum IS INITIAL
*     AND oldinfo-ipirate <> formout-ipirate .
*    CONCATENATE 'IPI=' formout-ipirate '%' INTO formout-obs_text.
* eod note 821403

  IF FORMOUT-NFE IS INITIAL.                      "NFE
* boi note 821403
    IF NOT FORMOUT-FOUND IS INITIAL AND
     NOT FORMOUT-NFNUM IS INITIAL
     AND OLDINFO-IPIRATE <> FORMOUT-IPIRATE .
      CONCATENATE 'IPI=' FORMOUT-IPIRATE '%' INTO FORMOUT-OBS_TEXT.
* eoi note 821403
    ELSE.
      CLEAR FORMOUT-OBS_TEXT.
    ENDIF.
  ELSE.                                           "NFE
    IF NOT FORMOUT-FOUND IS INITIAL AND           "NFE
       NOT FORMOUT-NFENUM IS INITIAL              "NFE
       AND OLDINFO-IPIRATE <> FORMOUT-IPIRATE .   "NFE
      CONCATENATE 'IPI=' FORMOUT-IPIRATE '%'      "NFE
      INTO FORMOUT-OBS_TEXT.                      "NFE
    ELSE.                                         "NFE
      CLEAR FORMOUT-OBS_TEXT.                     "NFE
    ENDIF.                                        "NFE
  ENDIF.                                          "NFE

*  WRITE:  115(4)     formout-bwart,   " boi accessability
*          119(1)     formout-sobkz,
*          120(1)     formout-kzbew,
*          121(1)     formout-kzzug,
*          122(1)     formout-kzvbr,
*          123(10)    formout-obs_text.

*****
  WRITE:  117(4)     FORMOUT-BWART.
*****          121(1)     FORMOUT-SOBKZ,
*****          122(1)     FORMOUT-KZBEW,
*****          123(1)     FORMOUT-KZZUG,
*****          124(1)     FORMOUT-KZVBR,
*****          125(10)    FORMOUT-OBS_TEXT.  " eoi accessability

  CLEAR FIRST_RECORD_REP.
  CLEAR FIRST_RECORD_MAT.

* update table with movements codes           " note 797729
  IF NOT FORMOUT-BWART IS INITIAL.
    MOVE-CORRESPONDING FORMOUT TO MOVEMENTS.
    COLLECT MOVEMENTS.
  ENDIF.
  WRITE: 134 SY-VLINE.                  " accessability   "1487825
ENDFORM.                               " LINE_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  NF_FIND
*&---------------------------------------------------------------------*
*  -->  application, document number, material
*  <--  Nota Fiscal data for the NF that refers to the input data
*       if existing + sy-subrc 'found' - if found ne 0: no NF found
*----------------------------------------------------------------------*
FORM NF_FIND  USING NFREFKEY
                    NFREFTYP
                    NFREFITM
                    MSEGMEINS
                    NFMATNR
                    MAT_QUANT        " note 1137434
                    ERFMG            " note 1137434
                    ERFME            " note 1137434
                    FORMOUT STRUCTURE OUT.

*----------------------------------------------------------------------*
* local data
*----------------------------------------------------------------------*
  DATA: WK_INDOC   LIKE J_1BINDOC,
        WK_LIN     LIKE J_1BNFLIN OCCURS 0 WITH HEADER LINE,
        WK_INLIN   LIKE J_1BINLIN OCCURS 0 WITH HEADER LINE,
        WK_STX     LIKE J_1BNFSTX OCCURS 0 WITH HEADER LINE,
        MSEGMENGE  LIKE J_1BNFLIN-MENGE,
        IPIBASE    LIKE J_1BNFSTX-BASE.

  DATA: BEGIN OF LIV_DOC_KEY,
          BELNR LIKE RBKP-BELNR,
          GJAHR LIKE RBKP-GJAHR,
        END OF LIV_DOC_KEY.

*----------------------------------------------------------------------*

* find NF lines refering to the current line of a standard document
  SELECT * FROM  J_1BNFLIN WHERE  REFKEY = NFREFKEY
                              AND REFTYP = NFREFTYP
                              AND REFITM = NFREFITM
                              AND MATNR  = NFMATNR.

    IF SY-SUBRC = 0.

      FOUND = 'X'.
      FORMOUT-FOUND = FOUND .                     " note 821403
* check if this NF line has already been reported
      READ TABLE REPORTED_NFS WITH KEY DOCNUM = J_1BNFLIN-DOCNUM
                                       ITMNUM = J_1BNFLIN-ITMNUM.
      IF SY-SUBRC = 0 AND GV_BATCHSPLIT = 'X'.       " note 1162428
        SY-SUBRC = 4.                             " note 1162428
      ENDIF.                                      " note 1162428
      IF SY-SUBRC = 0.
* convert unit in NF to base unit of measure if necessary
        IF MSEGMEINS NE J_1BNFLIN-MEINS.
          IF MAT_QUANT <> 0.                          " note 1137434
            REST = REST - MAT_QUANT.                  " note 1137434
          ELSE.                                       " note 1137434
            CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
              EXPORTING
                INPUT                = J_1BNFLIN-MENGE
                KZMEINH              = 'X'
                MATNR                = J_1BNFLIN-MATNR
                MEINH                = J_1BNFLIN-MEINS
                MEINS                = MSEGMEINS
              IMPORTING
                OUTPUT               = MSEGMENGE
              EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                INPUT_INVALID        = 2
                MATERIAL_NOT_FOUND   = 3
                MEINH_NOT_FOUND      = 4
                MEINS_MISSING        = 5
                NO_MEINH             = 6
                OUTPUT_INVALID       = 7
                OVERFLOW             = 8
                OTHERS               = 9.
            REST = REST - MSEGMENGE.
          ENDIF.                                      " note 1137434
        ELSE.
          REST = REST - J_1BNFLIN-MENGE.
        ENDIF.
        EXIT.
      ENDIF.
* boi note 821403
*      CHECK rest > 0.
      IF REST =< 0.
        FORMOUT-CREDIT_MEMO_FLAG = 'X'.
      ENDIF.
* eoi note 821403
      CHECK J_1BNFLIN-TMISS NE 'X'.
      CLEAR FORMOUT-SERIES.
      CLEAR FORMOUT-NFNUM.
      CLEAR FORMOUT-NFENUM.                      "NFE
      CLEAR FORMOUT-CFOP.
      CLEAR FORMOUT-CFOP_EXT.
      CLEAR FORMOUT-NFTOT.
      CLEAR FORMOUT-IPIVAL.
      CLEAR IPIBASE.

* read necessary document information (if not already in workarea)
      IF J_1BNFDOC-DOCNUM <> J_1BNFLIN-DOCNUM.
        SELECT SINGLE * FROM J_1BNFDOC WHERE DOCNUM = J_1BNFLIN-DOCNUM.
      ENDIF.

* confirmation that NF found is related to the correct business place
      CHECK J_1BNFDOC-BRANCH = BRANCH.                  " note 908884

* don't report documents that were only used to cancel other documents
      CHECK J_1BNFDOC-DOCTYP NE '5'.

* don't report corrections
      CHECK J_1BNFDOC-DOCTYP NE '3'.

* don't report Conhecimentos
      CHECK J_1BNFDOC-DOCTYP NE '4'.

* don't report Conhecimentos w/o reference
      CHECK J_1BNFDOC-DOCTYP NE '7'.

* don't report unprinted NFs
* for normal NFs the number is necessary         "NFE
* NFes must be authorized!                       "NFE
      IF J_1BNFDOC-NFE IS INITIAL.               "NFE
        CHECK NOT J_1BNFDOC-NFNUM IS INITIAL.
      ELSE.                                      "NFE
        IF J_1BNFDOC-DOCSTAT NE '1'.                        "1629597
* Don't mark the found flag if this NF-e is cancelled,    "1629597
* as another NF-e for this outbound delivery might        "1629597
* exist and has to be reported                            "1629597
          IF J_1BNFDOC-CANCEL = 'X'.                        "1629597
            CLEAR FOUND.                                    "1629597
          ENDIF.                                            "1629597
          CONTINUE.                                         "1629597
        ENDIF.                                              "1629597
      ENDIF.                                     "NFE

* Clear the cancel flag in case of cancellation of the " BOI note 617905
* NF (CANDAT) after end of selected reporting period
      IF J_1BNFDOC-CANCEL = 'X'.
        IF J_1BNFDOC-CANDAT GT DATUM_HIGH.
          CLEAR J_1BNFDOC-CANCEL.
        ENDIF.                                         " EOI note 617905
* enhancement of note 617905: BADI offered for modification of the
* cancel-flag for specific purposes
        CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->CANCEL_TREATMENT
          EXPORTING
            IS_NFDOC  = J_1BNFDOC
          IMPORTING
            CANCEL_ID = J_1BNFDOC-CANCEL.
      ENDIF.

* don't report cancelled documents, but in this case: don't mark
* the 'found' flag, since a new document, refering to the same
* application document as this cancelled NF might exist and has to
* be reported
      IF J_1BNFDOC-CANCEL = 'X'.
        CLEAR FOUND.
      ENDIF.

      CHECK J_1BNFDOC-CANCEL NE 'X'.

      SELECT * FROM *J_1BNFLIN INTO WK_LIN
                              WHERE DOCNUM = J_1BNFDOC-DOCNUM
                                AND ITMNUM = J_1BNFLIN-ITMNUM.
        APPEND WK_LIN.
      ENDSELECT.

      SELECT * FROM J_1BNFSTX INTO WK_STX
                              WHERE DOCNUM = J_1BNFDOC-DOCNUM
                                AND ITMNUM = J_1BNFLIN-ITMNUM.
        APPEND WK_STX.
      ENDSELECT.

      CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
        EXPORTING
          NF_HEADER   = J_1BNFDOC
        IMPORTING
          EXT_HEADER  = WK_INDOC
        TABLES
          NF_ITEM     = WK_LIN
          NF_ITEM_TAX = WK_STX
          EXT_ITEM    = WK_INLIN
        EXCEPTIONS
          OTHERS      = 1.
      FORMOUT-OLDBELNR        = IT_MEBEL-BELNR.       " note 821403
      MOVE-CORRESPONDING J_1BNFDOC TO FORMOUT.

* Conhecimentos will not be reported according to the law, thus every
* document that passes this form must be a Nota Fiscal
      IF J_1BNFDOC-NFE IS INITIAL.                      "NFE
        MOVE 'NF' TO FORMOUT-ESP.
      ELSE.                                             "NFE
        MOVE 'NFe' TO FORMOUT-ESP.                      "NFE
      ENDIF.                                            "NFE
* note 736398: BADI offered for modification of the NF species
      CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->ESPECIE_INDICATOR_CHANGE
        EXPORTING
          IS_J_1BNFDOC = J_1BNFDOC
          IV_ESPECIE   = FORMOUT-ESP
        IMPORTING
          EV_ESPECIE   = FORMOUT-ESP.

      MOVE-CORRESPONDING J_1BNFLIN TO FORMOUT.
      MOVE-CORRESPONDING WK_INLIN TO FORMOUT.

* exclude statistical taxes
      IF WK_INLIN-IPISTAT = 'X'.
        FORMOUT-IPIVAL = -1.
      ENDIF.

* determine the value to be printed in the value column
      LOOP AT WK_STX.
        READ TABLE INTJ_1BAJ WITH KEY TAXTYP = WK_STX-TAXTYP
          BINARY SEARCH.
        IF INTJ_1BAJ-TAXGRP = CONST_TAXGRP-IPI AND WK_STX-BASE > 0.
          IPIBASE = IPIBASE + WK_STX-BASE.
          FORMOUT-IPIRATE = WK_STX-RATE.
        ENDIF.
      ENDLOOP.

      IF IPIBASE > 0.
        FORMOUT-NFTOT = IPIBASE.
      ELSE.
        FORMOUT-IPIVAL = 0.
      ENDIF.

* convert CFOP for SC into 6 digit form (e.g. j917a into 19917a)
      IF J_1BNFLIN-CFOP(1) CN '0123456789'.
        WRITE J_1BNFLIN-CFOP TO J_1BNFLIN-CFOP.
      ENDIF.

* CFOP to be reported with 3 characters only
      MOVE J_1BNFLIN-CFOP(CFOP_LENGTH) TO FORMOUT-CFOP.

* determine, if new reporting for cfop codes X99 must be used
* decreto is valid for Sao Paulo state from 01.01.2000 onwards
      IF ( J_1BNFLIN-CFOP+1(3) = '991' OR J_1BNFLIN-CFOP+1(3) = '999' )
         AND ADDRESS1-REGION = SAO_PAULO
         AND EXT_LEN > 0.
        MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO FORMOUT-CFOP_EXT.
      ENDIF.
* determine, if new reporting for Santa Catarina must be used
      IF J_1BNFLIN-CFOP+1(2) = '99'
       AND ADDRESS1-REGION = SANTA_CATARINA
       AND EXT_LEN > 0.
        MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO FORMOUT-CFOP_EXT.
      ENDIF.

* convert unit in NF to base unit of measure if necessary(always called
* in bach split case)
      IF ( MSEGMEINS NE J_1BNFLIN-MEINS )
      OR GV_BATCHSPLIT = 'X'.                         "note 1162428

        IF MAT_QUANT <> 0.                            " note 1137434
          FORMOUT-MEINS = MSEGMEINS.                  " note 1137434
          FORMOUT-MENGE = MAT_QUANT.                  " note 1137434
        ELSE.                                         " note 1137434
          CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
            EXPORTING
              INPUT                = J_1BNFLIN-MENGE
              KZMEINH              = 'X'
              MATNR                = J_1BNFLIN-MATNR
              MEINH                = J_1BNFLIN-MEINS
              MEINS                = MSEGMEINS
            IMPORTING
              OUTPUT               = MSEGMENGE
            EXCEPTIONS
              CONVERSION_NOT_FOUND = 1
              INPUT_INVALID        = 2
              MATERIAL_NOT_FOUND   = 3
              MEINH_NOT_FOUND      = 4
              MEINS_MISSING        = 5
              NO_MEINH             = 6
              OUTPUT_INVALID       = 7
              OVERFLOW             = 8
              OTHERS               = 9.
          FORMOUT-MEINS = MSEGMEINS.
          FORMOUT-MENGE = MSEGMENGE.
        ENDIF.                                        " note 1137434
      ENDIF.

* In case of Complementars from MM-LIV - Check if it is only an amount
* related Complementar - in this case the quantity must be set to zero
      IF J_1BNFDOC-DOCTYP = '2' AND NFREFTYP = CONST_REFTYP-LI.
        MOVE NFREFKEY TO LIV_DOC_KEY.
        CLEAR RSEG.
        SELECT SINGLE * FROM RSEG WHERE BELNR = LIV_DOC_KEY-BELNR
                                    AND GJAHR = LIV_DOC_KEY-GJAHR
                                    AND BUZEI = NFREFITM.
        IF SY-SUBRC = 0.
          IF RSEG-TBTKZ = 'X'.
            CLEAR FORMOUT-MENGE.
          ENDIF.
        ENDIF.
      ENDIF.

* is there a rest?
      REST = REST - FORMOUT-MENGE.
* boi note 821403
* summarize the quantites/values of the Nota Fiscais and output
      IF NOT FOUND IS INITIAL.         " and rest >= 0.
*        PERFORM sum_values.
*        PERFORM line_output USING formout.
        MOVE-CORRESPONDING FORMOUT TO ALV_OUT.
        APPEND ALV_OUT.
      ENDIF.
* eoi note 821403

* note that this NF line has been reported - only SD
      IF NFREFTYP = CONST_REFTYP-SD OR NFREFTYP = CONST_REFTYP-LI.
        MOVE-CORRESPONDING J_1BNFLIN TO REPORTED_NFS.
        APPEND REPORTED_NFS.
      ENDIF.

* note that this NF line has been reported - internal table
      IF NFREFTYP = CONST_REFTYP-MM.
        MOVE-CORRESPONDING J_1BNFLIN TO INT_REPORTED_NFBT.
        APPEND INT_REPORTED_NFBT.
      ENDIF.

    ENDIF.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    IF NFREFTYP = CONST_REFTYP-MM.
      LOOP AT INT_REPORTED_NFBT INTO WA_REPORTED_NFBT.
        IF WA_REPORTED_NFBT-REFKEY = NFREFKEY AND
           WA_REPORTED_NFBT-MATNR  = NFMATNR AND
           WA_REPORTED_NFBT-REFITM NE NFREFITM.
          REST = REST - INT_REPORTED_NFBT-MENGE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                               " NF_FIND

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_DIRECTION
*&---------------------------------------------------------------------*
*   determines if the movement is an entry (entrada) or an issue (saída)
*----------------------------------------------------------------------*
FORM DETERMINE_DIRECTION USING DIRECT.
  CONSTANTS: E(1) VALUE 'E',
             S(1) VALUE 'S'.
  CASE DIRECT.
    WHEN 'S'.
      OUT-DIR = E.   "E...ntrada
    WHEN 'H'.
      OUT-DIR = S.   "S...aída
  ENDCASE.

ENDFORM.                               " DETERMINE_DIRECTION

*&---------------------------------------------------------------------*
*&      Form  carry_over
*&---------------------------------------------------------------------*
*       printing the totals
*----------------------------------------------------------------------*
FORM CARRY_OVER USING CARRY_OVER_TEXT CARRY_OVER_QUANT
                      CARRY_OVER_VALUE CARRY_OVER_QUANT2.
  IF MBLNR[] IS INITIAL AND MVTYP[] IS INITIAL.
*    WRITE: /16 carry_over_text,   " boi accessability
*            56(13)  carry_over_quant UNIT oldinfo-meins,
*            70(15)  carry_over_value CURRENCY t001-waers,
*            101(13) carry_over_quant2 UNIT oldinfo-meins.
    WRITE: / SY-VLINE,
            17      CARRY_OVER_TEXT ,
            57(13)  CARRY_OVER_QUANT UNIT OLDINFO-MEINS,
*            71(15)  CARRY_OVER_VALUE CURRENCY T001-WAERS,
            102(13) CARRY_OVER_QUANT2 UNIT OLDINFO-MEINS,
            134     SY-VLINE.       " eoi accessability
  ENDIF.

ENDFORM.                               " carry_over

*&---------------------------------------------------------------------*
*&      Form  fill_codigo
*&---------------------------------------------------------------------*
*       fill the codigo (1: own stock; 2: other stock; 3: Diversas)
*----------------------------------------------------------------------*
FORM FILL_CODIGO.
  CASE OUT-SOBKZ.
    WHEN 'M'.                          "Ret.trpt pckg vendor
      MOVE '3' TO OUT-CODIGO.
    WHEN 'O'.                          "Parts prov. vendor
      MOVE '2' TO OUT-CODIGO.
    WHEN 'V'.                          "Ret. pckg with cust.
      MOVE '2' TO OUT-CODIGO.
    WHEN OTHERS.
      IF OUT-SUBCONTRACT = 'X'.                             "1523305
        MOVE '2' TO OUT-CODIGO.                             "1523305
      ELSE.                                                 "1523305
        MOVE '1' TO OUT-CODIGO.                             "1523305
      ENDIF.                                                "1523305
  ENDCASE.

ENDFORM.                               " fill_codigo

*&---------------------------------------------------------------------*
*&      Form  sum_values
*&---------------------------------------------------------------------*
*       summarize the quantites/values
*----------------------------------------------------------------------*
FORM SUM_VALUES.
  CLEAR: SUMS-QUANT_ENTR_1, SUMS-QUANT_SAID_1.              " 821403
  CASE OUT-VAL_PROC_IND.
    WHEN 'X'.
      IF OUT-DIR = 'E'.
        SUMS-QUANT_ENTR_1 = SUMS-QUANT_ENTR_1 + OUT-MENGE.
      ELSEIF OUT-DIR = 'S'.
        SUMS-QUANT_SAID_1 = SUMS-QUANT_SAID_1 + OUT-MENGE.
      ENDIF.
  ENDCASE.

ENDFORM.                               " sum_values
*&---------------------------------------------------------------------*
*&      Form  ALV_OUTPUT
*&---------------------------------------------------------------------*

FORM ALV_OUTPUT .

  DATA : CSYTABIX LIKE SY-TABIX,
           DIR(1)   TYPE C,
           ITZEILE    LIKE MSEG-ZEILE.

* processing the output list for 'correcting' credit memos
  LOOP AT ALV_OUT WHERE CREDIT_MEMO_FLAG IS INITIAL.
    CLEAR: CSYTABIX,DIR,ITZEILE.
    CSYTABIX = SY-TABIX.
    ITZEILE  = ALV_OUT-ZEILE.
    DIR      = ALV_OUT-DIR.

    IF ALV_OUT-NFE IS INITIAL.                   "NFE
      IF NOT ALV_OUT-FOUND IS INITIAL AND ALV_OUT-NFNUM IS INITIAL.
        CLEAR ALV_OUT-FOUND.
        MODIFY ALV_OUT INDEX SY-TABIX.
      ENDIF.
    ELSE.                                        "NFE
      IF NOT ALV_OUT-FOUND IS INITIAL           "NFE
      AND ALV_OUT-NFENUM IS INITIAL.            "NFE
        CLEAR ALV_OUT-FOUND.                     "NFE
        MODIFY ALV_OUT INDEX SY-TABIX.           "NFE
      ENDIF.                                     "NFE
    ENDIF.                                       "NFE

    READ TABLE ALV_OUT
     WITH KEY OLDBELNR = ALV_OUT-MBLNR
              MENGE    = ALV_OUT-MENGE.
    IF SY-SUBRC IS INITIAL.
      IF CSYTABIX = SY-TABIX
        OR ITZEILE  NE ALV_OUT-ZEILE.

        " do nothing "
      ELSE.
        ALV_OUT-DIR = DIR.
        ALV_OUT-CREDIT_MEMO_FLAG = ''.
        MODIFY ALV_OUT INDEX CSYTABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.

* send it to the screen -----------------------------------------------*
* check if entries are available
  READ TABLE ALV_OUT INDEX 1 TRANSPORTING NO FIELDS.
  CHECK SY-SUBRC = 0.

* printing of the material header (incl. initial/final quantities)
  PERFORM OUTPUT_HEAD.
* printing of the initial values (in separate line)
  FORMAT INTENSIFIED COLOR COL_GROUP.
  PERFORM CARRY_OVER USING TEXT-290 OLDINFO-VMKUM
                           OLDINFO-VMSAL OLDINFO-VMKUM.
* printing all items:
  LOOP AT ALV_OUT WHERE CREDIT_MEMO_FLAG IS INITIAL.
    IF ( (  NOT CHECKFI = 'X' ) OR ( ( CHECKFI = 'X' ) AND  "1487825 1157708
    ( ( ALV_OUT-VAL_PROC_IND = 'X' ) OR ( ALV_OUT-SUBCONTRACT = 'X' ) ) ) ) AND OUT-NOMVT <> 'X'."1487825 1157708 1523305
      IF ALV_OUT-MATNR2 IS INITIAL.
        IF ALV_OUT-DIR = 'E'.
          ALV_OUT-QUANT_ENTR_1 =
                      ALV_OUT-QUANT_ENTR_1 + ALV_OUT-MENGE.
        ELSEIF ALV_OUT-DIR = 'S'.
          ALV_OUT-QUANT_SAID_1 = ALV_OUT-QUANT_SAID_1 + ALV_OUT-MENGE.
        ENDIF.
        ENTRDA = ENTRDA +  ALV_OUT-QUANT_ENTR_1.
        SAIDA  = SAIDA  +  ALV_OUT-QUANT_SAID_1.
      ENDIF.                                                "note 1157708
    ENDIF.                                                "note 1157708
    MOVE-CORRESPONDING ALV_OUT TO OUT.

    IF OUT-BWART EQ 'F50'.
      READ TABLE IT_MKPF_NFE
        WITH KEY MBLNR = OUT-MBLNR
                 BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_VBRP_NFE
          WITH KEY VGBEL = IT_MKPF_NFE-VGBEL
                   BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          READ TABLE IT_BNFLIN_NFE
            WITH KEY REFKEY = IT_VBRP_NFE-REFKEY
                      BINARY SEARCH.

          IF SY-SUBRC IS INITIAL.
            READ TABLE IT_BNFDOC_NFE
              WITH KEY DOCNUM = IT_BNFLIN_NFE-DOCNUM
                        BINARY SEARCH.

            IF SY-SUBRC IS INITIAL.
              OUT-NFENUM =  IT_BNFDOC_NFE-NFENUM.
              OUT-FOUND =  OUT-NFE = 'X'.
              OUT-ESP = 'NFe'.
              OUT-CFOP = IT_BNFLIN_NFE-CFOP.

            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.
    PERFORM LINE_OUTPUT USING OUT.
  ENDLOOP.

  FORMAT COLOR COL_GROUP.
  FORMAT INTENSIFIED OFF COLOR COL_TOTAL.     " accessibility
  PERFORM CARRY_OVER USING TEXT-292           " Total de entradas:
                            ENTRDA DUMMY DUMMY.
  PERFORM CARRY_OVER USING TEXT-293           " Total de saídas:
                            SAIDA DUMMY DUMMY.

  OLDINFO-LBKUM = CHECK_ACC_ESTOQUE.          " Note 1144454

  FORMAT INTENSIFIED COLOR COL_GROUP.
  FORMAT INTENSIFIED COLOR COL_TOTAL .        " accessability
  PERFORM CARRY_OVER USING TEXT-291           " Saldo:
                      OLDINFO-LBKUM OLDINFO-SALK3 OLDINFO-LBKUM.

  IF CHECK_ACC_ESTOQUE <> OLDINFO-LBKUM AND
     FIRST_RECORD_MAT IS INITIAL AND
     MBLNR[] IS INITIAL AND MVTYP[] IS INITIAL.
* Incorrect reported stocks for material &1
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        ARBGB = '8B'
        MSGTY = 'E'
        MSGV1 = IT_MDATA-MATNR
        MSGV2 = CHECK_ACC_ESTOQUE
        MSGV3 = OLDINFO-LBKUM
        TXTNR = '498'.
  ENDIF.


ENDFORM.                    " ALV_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MODELO7
*&---------------------------------------------------------------------*
*       Material STOCK                                                 *
*----------------------------------------------------------------------*
FORM WITHOUT_MOVEMENT.                                      "1487825
  TABLES  T134M.                                            "1487825
  CONSTANTS: ON       VALUE 'X',                            "1487825
             OFF      VALUE SPACE.                          "1487825
  TYPES: BEGIN OF TDISPLAY,                                 "1487825
          B_SVS    TYPE C,                                  "1487825
          B_CUCON  TYPE C,                                  "1487825
          B_VECON  TYPE C,                                  "1487825
         END OF TDISPLAY,                                   "1487825
         BEGIN OF T_ERROR_MESSAGE,                          "1487825
           BWKEY    LIKE MBEW-BWKEY,                        "1487825
           BKLAS    LIKE MBEW-BKLAS,                        "1487825
           STEUC    LIKE MARC-STEUC,                        "1487825
           BWTAR    LIKE MBEW-BWTAR,                        "1487825
           MATNR    LIKE MBEW-MATNR,                        "1487825
           TEXT     TYPE STRING,                            "1487825
         END OF T_ERROR_MESSAGE.                            "1487825
  DATA DISPLAY_LINE TYPE TDISPLAY.                          "1487825
  DISPLAY_LINE-B_SVS   = ON.                                "1487825
  DISPLAY_LINE-B_CUCON = ON.                                "1487825
  DISPLAY_LINE-B_VECON = ON.                                "1487825
*Store cumulated data from MSKUH (cust.consignment) here: "1487825
  DATA MSKUSAVE TYPE MBEW.                                  "1487825
*Store cumulated data from MSLBH (vend.consignment) here: "1487825
  DATA MSLBSAVE TYPE MBEW.                                  "1487825
*                                                         "1487825
  DATA: WA_ERROR_MESSAGE TYPE T_ERROR_MESSAGE,              "1487825
        ERROR_MESSAGES   TYPE TABLE OF T_ERROR_MESSAGE.     "1487825
*---------------------------------------------------------"1487825
  SELECT * FROM  MBEW                                       "1487825
             WHERE MATNR IN MAT       "Material ID          "1487825
             AND   BWKEY IN JR_WERKS  "Valuation Area       "1487825
             AND   LBKUM <> 0.                              "1487825
*-- Material with split valuation (sum record) -----------"1487825
    IF MBEW-BWTAR IS INITIAL AND                            "1487825
      NOT MBEW-BWTTY IS INITIAL.                            "1487825
      CONTINUE.                                             "1487825
    ENDIF.                                                  "1487825
    CALL FUNCTION 'J_1B_READ_MBEW_MBEWH_NEW'                "1487825
         EXPORTING                                          "1487825
              I_MBEW       = MBEW                           "1487825
              I_B_SVS      = DISPLAY_LINE-B_SVS             "1487825
         IMPORTING                                          "1487825
              E_MBEW       = MBEW                           "1487825
              E_EQBEW      = EQBEWSAVE                      "1487825
         EXCEPTIONS                                         "1487825
              NO_SELECTION = 1                              "1487825
              OTHERS       = 2.                             "1487825
    IF SY-SUBRC <> 0.                                       "1487825
*     normally not possible                               "1487825
      MOVE-CORRESPONDING MBEW TO WA_ERROR_MESSAGE.          "1487825
      WA_ERROR_MESSAGE-STEUC = MARC-STEUC.                  "1487825
      CASE SY-SUBRC.                                        "1487825
        WHEN 1.                                             "1487825
          WA_ERROR_MESSAGE-TEXT = TEXT-390.                 "1487825
        WHEN OTHERS.                                        "1487825
          WA_ERROR_MESSAGE-TEXT = TEXT-391.                 "1487825
      ENDCASE.                                              "1487825
      APPEND WA_ERROR_MESSAGE TO ERROR_MESSAGES.            "1487825
    ENDIF.                                                  "1487825
* ------------------------------------------------------- "1487825
* Read customer consignment tables MSKU or history MSKUH  "1487825
* for the current material and plant.                     "1487825
* The case that there is an entry in MSKU for a certain   "1487825
* material/plant and not in MBEW is  not valid in Brazil  "1487825
* since every material has to be valuated.                "1487825
* So we can happily relate to the current MBEW entry when "1487825
* looking in MSKU:                                        "1487825
* ------------------------------------------------------- "1487825
    IF DISPLAY_LINE-B_CUCON = ON.                           "1487825
      CALL FUNCTION 'J_1B_READ_MSKU_MSKUH_OF_MBEW'          "1487825
           EXPORTING                                        "1487825
                I_MBEW = MBEW                               "1487825
           IMPORTING                                        "1487825
                E_MBEW = MSKUSAVE.                          "1487825
    ENDIF.                                                  "1487825
* ------------------------------------------------------- "1487825
* Read vendor consignment tables MSLB or history MSLBH    "1487825
* for the current material and plant.                     "1487825
* It's the same case as for MSKU/MSKUH:                   "1487825
* ------------------------------------------------------- "1487825
    IF DISPLAY_LINE-B_VECON = ON.                           "1487825
      CALL FUNCTION 'J_1B_READ_MSLB_MSLBH_OF_MBEW'          "1487825
           EXPORTING                                        "1487825
                I_MBEW = MBEW                               "1487825
           IMPORTING                                        "1487825
                E_MBEW = MSLBSAVE.                          "1487825
    ENDIF.                                                  "1487825
* If quantity is zero don't extract: not interesting.     "1487825
*    CHECK ( mbew-vmkum      <> 0                         "1487825
*      OR  ( eqbewsave-lbkum <> 0                         "1487825
*           AND display_line-b_svs = on )                 "1487825
*      OR    mskusave-vmkum  <> 0                         "1487825
*      OR    mslbsave-vmkum  <> 0 ).                      "1487825
* ------------------------------------------------------- "1487825
* Subtract data which is displayed separately from        "1487825
* corresponding records in MBEW, where everything         "1487825
* is collected.                                           "1487825
* ------------------------------------------------------- "1487825
    MBEW-VMKUM = MBEW-VMKUM - MSKUSAVE-VMKUM - MSLBSAVE-VMKUM. "1487825
    MBEW-VMSAL = MBEW-VMSAL - MSKUSAVE-VMSAL - MSLBSAVE-VMSAL. "1487825
*--  Material A-Segment when changed--------------------- "1487825
    ON CHANGE OF MBEW-MATNR.                                "1487825
      PERFORM READ_MARA                                     "1487825
              USING MBEW-MATNR PLANTS-WERKS MBEW-BWKEY.     "1487825
    ENDON.                                                  "1487825
    CHECK T134M-WERTU = ON.                                 "1487825
* mseg-matnr - Material ID                                "1487825
    IT_MDATA-MATNR = MBEW-MATNR.                            "1487825
    IT_MDATA-MBLNR = '00000000'.                            "1487825
* mseg-werks - Valuation area/plant                       "1487825
    IT_MDATA-WERKS = MBEW-BWKEY.                            "1487825
* mseg-bwtar - Valuation type                             "1487825
    IT_MDATA-BWTAR = MBEW-BWTAR.                            "1487825
* mseg-menge - Quantity                                   "1487825
    IT_MDATA-MENGE = MBEW-LBKUM.                            "1487825
* flag for valuated stock                                 "1487825
    IT_MDATA-VAL_PROC_IND = ON.                             "1487825
* flag to identify materials w/o movement                 "1487825
    IT_MDATA-NOMVT = 'X'.                                   "1487825
    READ TABLE IT_MDATA WITH KEY                            "1487825
               MATNR = MBEW-MATNR                           "1487825
               WERKS = MBEW-BWKEY                           "1487825
               BWTAR = MBEW-BWTAR.                          "1644498
    IF SY-SUBRC <> 0.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 >>> INICIO
*      IF IT_MDATA-WERKS EQ BRANCH.                          "1487825
        IF IT_MDATA-WERKS EQ BRANCH_CENTRO.
*** Modificação - Eduardo Ruttkowski Tavares - 21.08.2013 <<< FIM
          APPEND IT_MDATA.                                  "1487825
        ENDIF.                                              "1487825
      ENDIF.                                                "1487825
    ENDSELECT.                                              "1487825
  ENDFORM.                                                  "1487825
*&---------------------------------------------------------------------*
*&      Form  READ_MARA
*&---------------------------------------------------------------------*
*       Material plant                                                 *
*----------------------------------------------------------------------*
FORM READ_MARA USING MATNR WRK XBWKEY.                      "1487825
  SELECT SINGLE * FROM  MARA                                "1487825
                 WHERE  MATNR = MATNR.                      "1487825
                                                            "1487825
  SELECT SINGLE * FROM T134M                                "1487825
   WHERE BWKEY = XBWKEY                                     "1487825
   AND   MTART = MARA-MTART.                                "1487825
  IF NOT SY-SUBRC IS INITIAL.                               "1487825
    WRITE: / TEXT-014, XBWKEY,                              "1487825
             TEXT-015, MARA-MTART,                          "1487825
             TEXT-016, MARA-MATNR.                          "1487825
    STOP.                                                   "1487825
  ENDIF.                                                    "1487825
*--  Material Shorttext --------------------------------- "1487825
  SELECT SINGLE * FROM MAKT                                 "1487825
                 WHERE MATNR = MARA-MATNR                   "1487825
                   AND SPRAS = SY-LANGU.                    "1487825
  SELECT SINGLE * FROM MARC                                 "1487825
                 WHERE MATNR = MATNR                        "1487825
                   AND WERKS = WRK.                         "1487825
ENDFORM.                                                    "1487825

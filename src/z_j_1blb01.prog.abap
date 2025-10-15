REPORT Z_J_1BLB01 MESSAGE-ID 8B LINE-SIZE 199
                                LINE-COUNT 65(1)
                                NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
*------------------------Report J_1BLB01-------------------------------*
*----------------------------------------------------------------------*
*-------Creation of the legal book 'Registro de Entradas'--------------*
*-----------------------------(Modelo 1)-------------------------------*
*-------and the legal book 'Lista de Códigos de Emitentes'-------------*
*-----------------------------(Modelo 10)------------------------------*
*----------------------------------------------------------------------*

* declaration of the nota fiscal database tables

TABLES: J_1BNFDOC,                     "nota fiscal header,
       *J_1BNFDOC,                     "nota fiscal header,
        J_1BINDOC,                "nota fiscal header - add. segment,
        J_1BNFLIN,                     "nota fiscal line items,
        J_1BINLIN,                "nota fiscal line items - add. segment
        J_1BNFSTX,                     "nota fiscal tax per item,
       *J_1BNFSTX,                     "nota fiscal tax per item,
*        j_1baj,                        "nota fiscal types,
        T001.                          "Company Codes

*----------------------------------------------------------------------*

* parameters for selection of services                          "955768
                                                            "955768
SELECTION-SCREEN BEGIN OF BLOCK SRV WITH FRAME TITLE TEXT-130."955768
PARAMETERS INCL_SRV AS CHECKBOX DEFAULT 'X'.                "955768
SELECTION-SCREEN END OF BLOCK SRV.                          "955768

*----------------------------------------------------------------------*

* parameters for pagenumbering and booksize

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
PARAMETERS: FIRSTPAG(6) TYPE N OBLIGATORY,
            BOOKSIZE(6) TYPE N OBLIGATORY.
PARAMETERS: MOD_1   AS CHECKBOX DEFAULT 'X',
            MOD_10  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*

* parameters for building subtotals (Decêndio, Quinzena, Mês)

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-120.
PARAMETERS: DECENDIO TYPE J_1BREP_PERIOD10 RADIOBUTTON GROUP TYP3
                                                            DEFAULT 'X',
            QUINZENA TYPE J_1BREP_PERIOD15 RADIOBUTTON GROUP TYP3,
            MES      TYPE J_1BREP_MONTH    RADIOBUTTON GROUP TYP3.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*

* definition of constants for taxgroups

CONSTANTS: BEGIN OF CONST_TAXGRP,
           ICMS(4)        TYPE C VALUE 'ICMS',
           IPI(4)         TYPE C VALUE 'IPI ',
           SUBTRIB(4)     TYPE C VALUE 'ICST',
           ICSTFREIGHT(4) TYPE C VALUE 'ICFS', "ICMS frght/ST"
           ICMSFREIGHT(4) TYPE C VALUE 'ICFR', "ICMS frght/ST"
           ISSS(4)        TYPE C VALUE 'ISSS',              "955768
           ISSP(4)        TYPE C VALUE 'ISSP',              "955768
           ISS(4)         TYPE C VALUE 'ISS',               "955768
           END OF CONST_TAXGRP.

* definition of constants for taxtypes - handling of the 'all bases = 0'

CONSTANTS: BEGIN OF CONST_TAXTYPE,
           ICZF(4)     TYPE C VALUE 'ICZF',
           ICMF(4)     TYPE C VALUE 'ICMF',
           ICZG(4)     TYPE C VALUE 'ICZG',                 "622370
           END OF CONST_TAXTYPE.

CONSTANTS: CONST_EX(2) VALUE 'EX'.  "code for region for foreign partner
CONSTANTS: SAO_PAULO(2) VALUE 'SP'.  "abbreviation for Sao Paulo state
CONSTANTS: SANTA_CATARINA(2) VALUE 'SC'. "abbrev. for St. Catarina state
*----------------------------------------------------------------------*

* key for internally used record data - contains sorting fields

DATA: BEGIN OF KEY,
      MONTHYEAR(6)  TYPE N,      " used for total/subtotals; note 697076
      PSTDAT        LIKE J_1BNFDOC-PSTDAT,"posting date
*     species(2)    type c,            "species of fiscal document:
      SPECIES(3)    TYPE C,   "species of fiscal document:  note 736398
                                       "NF = Nota Fiscal
                                       "CO = Conhecimento
      REGIO         LIKE J_1BINNAD-REGIO, "region
      PARID         LIKE J_1BNFDOC-PARID, "partner identification
      SERIES        LIKE J_1BNFDOC-SERIES,"series
                                       "of the fiscal document
      SUBSER        LIKE J_1BNFDOC-SUBSER,"subseries of the fiscal doc.
      NFNUM         LIKE J_1BNFDOC-NFNUM, "number of fiscal document
      DOCNUM        LIKE J_1BNFDOC-DOCNUM,"to distinguish different
                                       "documents (summing)
*      cfop_noext(3) type c,            "cfop no. without extension
      CFOP_NOEXT(4) TYPE C,            "cfop no. without extension
*      CFOP_EXT(2)   TYPE c,            "cfop numberfirst digit of ext
      CFOP_EXT(2)   TYPE N,            "cfop numberfirst digit of ext
      TAXGRP        LIKE J_1BAJ-TAXGRP,"taxgroup
      CODIGO(1)     TYPE C,            "fiscal code
      RATE          LIKE J_1BNFSTX-RATE,  "taxrate
      OBS_DOC(50)   TYPE C,            "document related observation
      OBS_LIN(100)  TYPE C,            "line related observation
      OBS_REF       LIKE J_1BNFDOC-DOCNUM,"internal documentnumber of a
                                       "referenced document
END OF KEY.

* internally used record data

DATA: BEGIN OF DATA,
      PARTYP       LIKE J_1BNFDOC-PARTYP, "partner type (vend., cust.)
      CANCEL       LIKE J_1BNFDOC-CANCEL, "indicator for cancelled
                                       "documents
      DOCDAT       LIKE J_1BNFDOC-DOCDAT, "document date
      NFTOT        LIKE J_1BINLIN-NFTOT,  "total value per item,
                                       "incl. ICMS/ISS, IPI, freight,
                                       "insurance, other expenses
      BASE         LIKE J_1BNFSTX-BASE,"calculation base
      TAXVAL       LIKE J_1BNFSTX-TAXVAL, "tax amount
      STATTX       LIKE J_1BNFSTX-STATTX, "statistical tax indicator
      RECTYPE      LIKE J_1BNFSTX-RECTYPE,"IPI Pauta ?
      OBS_IPIPAUTA LIKE J_1BNFSTX-TAXVAL, "IPI Pauta amount
      OBS_ICFRBASE LIKE J_1BNFSTX-BASE,"ICMS freight/ST
      OBS_ICFRTAX  LIKE J_1BNFSTX-TAXVAL, "ICMS freight/ST
      ITMTYP       LIKE J_1BNFLIN-ITMTYP, "itemtype
      DOCTOT       LIKE J_1BINDOC-NFTOT,  "for state totals
      FREE_TEXT    LIKE J_1BNFDOC-OBSERVAT,
*     START OF NOTE 386073, B
      ST_CONSUMO   TYPE C, "SUBTRIB and consumption and interstate
*     END OF NOTE 386073, B
END OF DATA.

*----------------------------------------------------------------------*

* declaration of internal table for nota fiscal types ( = j_1baj)
DATA: INTJ_1BAJ LIKE J_1BAJ OCCURS 0 WITH HEADER LINE,
      INTJ_1BAA LIKE J_1BAA OCCURS 0 WITH HEADER LINE.

RANGES: IPI_TYPES FOR J_1BAJ-TAXTYP.
*----------------------------------------------------------------------*

* declaration of vendor and customer table - for creation of
* Lista de Códigos de Emitentes (Modelo 10)

DATA: BEGIN OF LIST OCCURS 0,
      PARID    LIKE J_1BNFDOC-PARID,   "partner identification
      PARTYP   LIKE J_1BNFDOC-PARTYP,  "partner type (vendor, customer)
END OF LIST.

* declaration of ONE TIME vendor and customer table - for creation of
* Lista de Códigos de Emitentes (Modelo 10)

DATA: BEGIN OF OT_LIST OCCURS 0,
      PARID   LIKE J_1BNFDOC-PARID,    "partner identification
      PARTYP  LIKE J_1BNFDOC-PARTYP,   "partner type (vendor, customer)
      PSTDAT  LIKE J_1BNFDOC-PSTDAT,   "posting date
      NFNUM   LIKE J_1BNFDOC-NFNUM,    "Nota Fiscal number
      CGC     LIKE J_1BINNAD-CGC,
      CPF     LIKE J_1BINNAD-CPF,
      NAME1   LIKE J_1BINNAD-NAME1,
      REGIO   LIKE J_1BINNAD-REGIO,
      STAINS  LIKE J_1BINNAD-STAINS,
END OF OT_LIST.

*----------------------------------------------------------------------*

* summary per state ICMS/ICST

DATA: BEGIN OF STATE_SUMMARY_ICMS OCCURS 0,
      REGIO       LIKE J_1BINNAD-REGIO,
      TAXGRP      LIKE J_1BAJ-TAXGRP,
      CODIGO      LIKE KEY-CODIGO,
      NFTOT       LIKE J_1BINDOC-NFTOT,
      BASE        LIKE J_1BNFSTX-BASE,
      TAXVAL      LIKE J_1BNFSTX-TAXVAL,
END OF STATE_SUMMARY_ICMS.

*----------------------------------------------------------------------*

* declaration of structures coming from function modules

DATA: CGC_NUMBER  LIKE J_1BWFIELD-CGC_NUMBER,
      ADDRESS1    LIKE ADDR1_VAL,
      BRANCH_DATA LIKE J_1BBRANCH,
      PARNAD      LIKE J_1BINNAD,
      T_MESG      LIKE MESG OCCURS 0 WITH HEADER LINE.
DATA: WG_FIRMA(80).
DATA: SAVE_PARNAD_REGIO LIKE PARNAD-REGIO.
*----------------------------------------------------------------------*
* start P45K005034
* layout of output: placement of columns
DATA: C_POSTD TYPE I,
      C_SPECI TYPE I,
      C_SUBSE TYPE I,
      C_SERIE TYPE I,
      C_NFNUM TYPE I,
      C_DOCDA TYPE I,
      C_PARID TYPE I,
      C_REGIO TYPE I, L_REGIO TYPE I,
      C_NFTOT TYPE I, L_NFTOT TYPE I,
      C_EMPTY TYPE I, L_EMPTY TYPE I,
      C_CFOPN TYPE I, L_CFOPN TYPE I,
      C_CFOPX TYPE I,
      C_TAXGR TYPE I,
      C_CODIG TYPE I, L_CODIG TYPE I,
      C_BASE  TYPE I, L_BASE TYPE I,
      C_RATE  TYPE I, L_RATE TYPE I,
      C_TAXVA TYPE I, L_TAXVA TYPE I,
      C_OBSER TYPE I,
      C_TOTAL TYPE I,
      C_DECEN TYPE I.
DATA CONCATEXT(255).
DATA: WL_CONTADOR TYPE I,
      BEGIN OF WA_POS,
        C_POSTD TYPE I,
        C_SPECI TYPE I,
        C_SUBSE TYPE I,
        C_SERIE TYPE I,
        C_NFNUM TYPE I,
        C_DOCDA TYPE I,
        C_PARID TYPE I,
        C_REGIO TYPE I, L_REGIO TYPE I,
        C_NFTOT TYPE I, L_NFTOT TYPE I,
        C_EMPTY TYPE I, L_EMPTY TYPE I,
        C_CFOPN TYPE I, L_CFOPN TYPE I,
        C_CFOPX TYPE I,
        C_TAXGR TYPE I,
        C_CODIG TYPE I, L_CODIG TYPE I,
        C_BASE  TYPE I, L_BASE TYPE I,
        C_RATE  TYPE I, L_RATE TYPE I,
        C_TAXVA TYPE I, L_TAXVA TYPE I,
        C_OBSER TYPE I,
        C_TOTAL TYPE I,
        C_DECEN TYPE I,
        ULTIMO  TYPE I,
      END OF WA_POS.

* end P45K005034
DATA: WG_BLANKS(255).

* definition of the field-groups for the extract

FIELD-GROUPS: HEADER,
              EXDATA.

*insert key-pstdat                   " note 697076
INSERT KEY-MONTHYEAR                 " note 697076
       KEY-PSTDAT                    " note 697076
       KEY-SPECIES
       KEY-REGIO
       KEY-PARID
       KEY-SERIES
       KEY-SUBSER
       KEY-NFNUM
       KEY-DOCNUM
       KEY-CFOP_NOEXT
       KEY-CFOP_EXT
       KEY-TAXGRP
       KEY-CODIGO
       KEY-RATE
       KEY-OBS_DOC
       KEY-OBS_LIN
       KEY-OBS_REF
       INTO HEADER.
INSERT DATA INTO EXDATA.
*M001 - BEGIN
DATA: BEGIN OF IT_KEY OCCURS 0,
        MONTHYEAR   LIKE KEY-MONTHYEAR  ,
        PSTDAT      LIKE KEY-PSTDAT ,
        SPECIES     LIKE KEY-SPECIES ,
        REGIO       LIKE KEY-REGIO ,
        PARID       LIKE KEY-PARID ,
        SERIES      LIKE KEY-SERIES ,
        SUBSER      LIKE KEY-SUBSER ,
        NFNUM       LIKE KEY-NFNUM ,
        DOCNUM      LIKE KEY-DOCNUM ,
        CFOP_NOEXT  LIKE KEY-CFOP_NOEXT ,
        CFOP_EXT    LIKE KEY-CFOP_EXT ,
        TAXGRP      LIKE KEY-TAXGRP ,
        CODIGO      LIKE KEY-CODIGO ,
        RATE        LIKE KEY-RATE ,
        OBS_DOC     LIKE KEY-OBS_DOC ,
        OBS_LIN     LIKE KEY-OBS_LIN ,
        OBS_REF     LIKE KEY-OBS_REF ,
END OF IT_KEY.

DATA: IT_DATA LIKE DATA OCCURS 0 WITH HEADER LINE.

DATA: W_MWSKZ TYPE MWSKZ.
*M001 - END
*----------------------------------------------------------------------*

* declaration of internally used help-fields

DATA: FIRST_RECORD TYPE C VALUE 'X',
      FIRST_RECORD_MONTHYEAR TYPE C VALUE 'X',         " note 697076
      LAST_TOTAL   TYPE C VALUE ' ',
      TABIX        LIKE SY-TABIX,
      PAGNO(6)     TYPE N,             "pagenumber within one book
      BOOKS(2)     TYPE N VALUE '0',   "number of printed books (Mod. 1)
      PAGES(6)     TYPE N VALUE '0',   "total number of printed pages
                                                            "(Mod. 1)
      MODELO(2)    TYPE N VALUE 1,     "to maintain output:
                                       "1  = Registro de Entradas
                                     "10 = Lista de Códigos de Emitentes
      ERRORLIST    TYPE C VALUE ' ',
      DOCNUM       LIKE J_1BNFDOC-DOCNUM, "to avoid duplications in the
                                       "output
      OLDDAY(2)    TYPE N.             "10 days totals

DATA: LINE_WITH_NONSTAT_TAX.
"indicator:tax line with non-statistical tax exists
*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed (summed up) by taxgroup, taxrate and/or CFOP number

DATA: BEGIN OF TAXSUMS,
      NFTOT   LIKE J_1BINLIN-NFTOT,
      BASE    LIKE J_1BNFSTX-BASE,
      TAXVAL  LIKE J_1BNFSTX-TAXVAL,
      OBS_IPIPAUTA LIKE J_1BNFSTX-TAXVAL, "IPI Pauta amount
      END OF TAXSUMS.

*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed by day

DATA: BEGIN OF TOTAL,
      VALORCONT    LIKE J_1BINLIN-NFTOT,"valor contabil
      ICMSBASE1    LIKE J_1BNFSTX-BASE,"calculation base ICMS, codigo 1
      ICMSBASE2    LIKE J_1BNFSTX-BASE,"calculation base ICMS, codigo 2
      ICMSBASE3    LIKE J_1BNFSTX-BASE,"calculation base ICMS, codigo 3
      IPIBASE1     LIKE J_1BNFSTX-BASE,"calculation base IPI, codigo 1
      IPIBASE2     LIKE J_1BNFSTX-BASE,"calculation base IPI, codigo 2
      IPIBASE3     LIKE J_1BNFSTX-BASE,"calculation base IPI, codigo 3
      SUBTRIBBASE1 LIKE J_1BNFSTX-BASE,"calculation base ST, codigo 1
      SUBTRIBBASE3 LIKE J_1BNFSTX-BASE,"calculation base ST, codigo 3
      ICMSVAL1     LIKE J_1BNFSTX-TAXVAL, "tax value ICMS, codigo 1
      IPIVAL1      LIKE J_1BNFSTX-TAXVAL, "tax value IPI, codigo 1
      SUBTRIBVAL1  LIKE J_1BNFSTX-TAXVAL, "tax value ST, codigo 1
* START OF NOTE 386073, A
      ICOPBASE1    LIKE J_1BNFSTX-BASE,"calculation base ICOP, codigo 1
      ICOPVAL1     LIKE J_1BNFSTX-TAXVAL, "tax value ICOP, codigo 1
* END OF NOTE 386073, A
      END OF TOTAL.

*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed by 10 days and per month

DATA: BEGIN OF TOTAL_10 OCCURS 5,                          " note 697076
      NUMBER(1)    TYPE C,             "first, second or third
                                       "10-day total of a period
      VALORCONT    LIKE J_1BINLIN-NFTOT,      "valor contabil
      ICMSBASE1    LIKE J_1BNFSTX-BASE,"calc. base ICMS, codigo 1
      ICMSBASE2    LIKE J_1BNFSTX-BASE,"calc. base ICMS, codigo 2
      ICMSBASE3    LIKE J_1BNFSTX-BASE,"calc. base ICMS, codigo 3
      IPIBASE1     LIKE J_1BNFSTX-BASE,"calc. base IPI, codigo 1
      IPIBASE2     LIKE J_1BNFSTX-BASE,"calc. base IPI, codigo 2
      IPIBASE3     LIKE J_1BNFSTX-BASE,"calc. base IPI, codigo 3
      SUBTRIBBASE1 LIKE J_1BNFSTX-BASE,"calc. base ST, codigo 1
      SUBTRIBBASE3 LIKE J_1BNFSTX-BASE,"calc. base ST, codigo 3
      ICMSVAL1     LIKE J_1BNFSTX-TAXVAL,     "tax value ICMS, codigo 1
      IPIVAL1      LIKE J_1BNFSTX-TAXVAL,     "tax value IPI, codigo 1
      SUBTRIBVAL1  LIKE J_1BNFSTX-TAXVAL,     "tax value ST, codigo 1
* START OF NOTE 386073, A
      ICOPBASE1    LIKE J_1BNFSTX-BASE,"calculation base ICOP, codigo 1
      ICOPVAL1     LIKE J_1BNFSTX-TAXVAL, "tax value ICOP, codigo 1
* END OF NOTE 386073, A
      END OF TOTAL_10.

*----------------------------------------------------------------------*

* cfop summaries

DATA: BEGIN OF CFOP_LINE OCCURS 0,
*   cfop_noext(3) type c,               " note 553750
   CFOP_NOEXT(4) TYPE C,               " note 553750
   CFOP_EXT(2)   TYPE C,
   VALORCONT    LIKE J_1BINLIN-NFTOT,  "valor contabil
   ICMSBASE1    LIKE J_1BNFSTX-BASE,   "calculation base ICMS, codigo 1
   ICMSBASE2    LIKE J_1BNFSTX-BASE,   "calculation base ICMS, codigo 2
   ICMSBASE3    LIKE J_1BNFSTX-BASE,   "calculation base ICMS, codigo 3
   ICMSVAL1     LIKE J_1BNFSTX-TAXVAL, "tax value ICMS, codigo 1
END OF CFOP_LINE.

DATA: BEGIN OF CFOP_LINE_TOT OCCURS 3,
   CFOP_FIRST(1) TYPE C,
   VALORCONT    LIKE J_1BINLIN-NFTOT,  "valor contabil
   ICMSBASE1    LIKE J_1BNFSTX-BASE,   "calculation base ICMS, codigo 1
   ICMSBASE2    LIKE J_1BNFSTX-BASE,   "calculation base ICMS, codigo 2
   ICMSBASE3    LIKE J_1BNFSTX-BASE,   "calculation base ICMS, codigo 3
   ICMSVAL1     LIKE J_1BNFSTX-TAXVAL, "tax value ICMS, codigo 1
END OF CFOP_LINE_TOT.

*----------------------------------------------------------------------*

* declaration of output-line

DATA: BEGIN OF OUT,
      CANCEL           LIKE J_1BNFDOC-CANCEL,
      PSTDAT           LIKE J_1BNFDOC-PSTDAT,
      SPECIES          LIKE KEY-SPECIES,
      SERIES           LIKE J_1BNFDOC-SERIES,
      SUBSER           LIKE J_1BNFDOC-SUBSER,
      NFNUM            LIKE J_1BNFDOC-NFNUM,
      DOCDAT           LIKE J_1BNFDOC-DOCDAT,
      PARID            LIKE J_1BNFDOC-PARID,
      REGIO            LIKE KEY-REGIO,
      NFTOT            LIKE J_1BINLIN-NFTOT,
      CFOP_NOEXT       LIKE KEY-CFOP_NOEXT,
      CFOP_EXT         LIKE KEY-CFOP_EXT,
      TAXGRP           LIKE J_1BAJ-TAXGRP,
      CODIGO           LIKE KEY-CODIGO,
      BASE             LIKE J_1BNFSTX-BASE,
      RATE             LIKE J_1BNFSTX-RATE,
      TAXVAL           LIKE J_1BNFSTX-TAXVAL,
      DUPL             TYPE C,        "indicator: to avoid repetition of
                                       "header information in output
      STATTX           LIKE J_1BNFSTX-STATTX,
      RECTYPE          LIKE J_1BNFSTX-RECTYPE, "IPI Pauta
      ITMTYP           LIKE J_1BNFLIN-ITMTYP,
      OBS_DOC          LIKE KEY-OBS_DOC,
      OBS_LIN          LIKE KEY-OBS_LIN,
      OBS_REF          LIKE KEY-OBS_REF,
      OBS_IPIPAUTA     LIKE J_1BNFSTX-TAXVAL,  "IPI Pauta
      OBS_ICFRBASE     LIKE J_1BNFSTX-BASE,   "ICMS freight/ST
      OBS_ICFRTAX      LIKE J_1BNFSTX-TAXVAL, "ICMS freight/ST
      FREE_TEXT        LIKE J_1BNFDOC-OBSERVAT,
*     START OF NOTE 386073, B
      ST_CONSUMO       TYPE C, "SUBTRIB and consumption and interstate
*     END OF NOTE 386073, B
END OF OUT.

TYPES: TAB_NFITMRULE TYPE STANDARD TABLE OF       " begin of note 553750
                                       J_1BNFITMRULE WITH DEFAULT KEY,
       TAB_J_1BMODTEXT TYPE STANDARD TABLE OF
                                         J_1BMODTEXT WITH DEFAULT KEY.

CONSTANTS: C_TEXT(5) VALUE 'TEXT-'.
DATA:     LV_NAME(8) TYPE C.
FIELD-SYMBOLS: <A>.

DATA:  GT_NFITMRULE TYPE TAB_NFITMRULE,
       GS_NFITMRULE TYPE J_1BNFITMRULE,
       GT_J_1BMODTEXT TYPE TAB_J_1BMODTEXT,
       GS_J_1BMODTEXT TYPE J_1BMODTEXT,
       CFOP_VERSION TYPE J_1BCFOP_VER,
       EXT_LEN      TYPE J_1BCFOP_EXTLEN,
       CFOP_LENGTH  TYPE J_1BCFOP_LEN.              " end of note 553750
DATA:  REP_DATE_HIGH        TYPE D.                 " note 617905

* BADI definition for legal books                             nt. 617905
CLASS CL_EX_BADI_J_1BLEGALREPORT DEFINITION LOAD.           " nt. 617905
DATA IF_EX_BADI_J_1BLEGALREPORT TYPE REF TO IF_EX_BADI_J_1BLEGALREPORT.

*----------------------------------------------------------------------*
*--------------------at selection-screen-------------------------------*
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.

* check if booksize > number of first page

  IF FIRSTPAG > BOOKSIZE.
    MESSAGE E459.
  ENDIF.

* check if firstpag < 2.

  IF FIRSTPAG < 2.
    MESSAGE E467.
  ENDIF.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      BRANCH      = J5_BRNCH
      BUKRS       = J5_BUKRS
    IMPORTING
      BRANCH_DATA = BRANCH_DATA
      CGC_NUMBER  = CGC_NUMBER
      ADDRESS1    = ADDRESS1
    EXCEPTIONS
      OTHERS      = 04.

  IF SY-SUBRC NE 0.
    MESSAGE W453 WITH J5_BUKRS J5_BRNCH.
  ENDIF.

* determine currency from T001

  SELECT SINGLE * FROM T001 WHERE BUKRS = J5_BUKRS.

* set page numbering

  PAGNO = FIRSTPAG - 1.

*---> Begin of insertion note 553750
* determination of the CFOP-version
  CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
    EXPORTING
      LAND1             = ADDRESS1-COUNTRY
      REGION            = ADDRESS1-REGION
      DATE              = J5_PDATE-LOW
    IMPORTING
      VERSION           = CFOP_VERSION
      EXTENSION         = EXT_LEN
      CFOPLENGTH        = CFOP_LENGTH
    EXCEPTIONS
      DATE_MISSING      = 1
      VERSION_NOT_FOUND = 2
      OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*---> End of insertion note 553750
  DEFINE ZULINE.
    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
            29 SY-ULINE(255).
*  uline at 29.
  END-OF-DEFINITION. "uline

*----------------------------------------------------------------------*
*---------------------start-of-selection-------------------------------*
*----------------------------------------------------------------------*

START-OF-SELECTION.

  SET COUNTRY 'BR'.

*----------------------------------------------------------------------*
*-----Delete messages collected up to now, collect future messages-----*
*----------------------------------------------------------------------*

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

*----------------------------------------------------------------------*
*-----------------------fill table intj_1baj---------------------------*
*----------------------------------------------------------------------*

  SELECT * FROM J_1BAJ INTO TABLE INTJ_1BAJ ORDER BY PRIMARY KEY.

  IF SY-SUBRC NE 0.
    MESSAGE A460.
  ENDIF.

  LOOP AT INTJ_1BAJ WHERE TAXGRP = CONST_TAXGRP-IPI.
    IPI_TYPES-SIGN   = 'I'.
    IPI_TYPES-OPTION = 'EQ'.
    IPI_TYPES-LOW    = INTJ_1BAJ-TAXTYP.
    APPEND IPI_TYPES.
  ENDLOOP.

*-----------------------fill table intj_1baa---------------------------*
  SELECT * FROM J_1BAA INTO TABLE INTJ_1BAA ORDER BY PRIMARY KEY.

  SELECT * FROM J_1BNFITMRULE  INTO TABLE GT_NFITMRULE.   "note 553750

  SELECT * FROM J_1BMODTEXT    INTO TABLE GT_J_1BMODTEXT. "note 553750

*--------------------------------------------------- BOI note 617905 --*
* build an instance of the object -------------------------------------*
*----------------------------------------------------------------------*
  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    CHANGING
      INSTANCE = IF_EX_BADI_J_1BLEGALREPORT.        " EOI note 617905

*----------------------------------------------------------------------*
  IF J5_PDATE-HIGH IS INITIAL.                         " BOI note 697076
    REP_DATE_HIGH = J5_PDATE-LOW.
  ELSE.
    REP_DATE_HIGH = J5_PDATE-HIGH.
  ENDIF.                                               " EOI note 697076

*----------------------------------------------------------------------*
*----------------------------get j_1bnfdoc-----------------------------*
*----------------------------------------------------------------------*

GET J_1BNFDOC.

* selection of the incoming movements

  CHECK J_1BNFDOC-DIRECT = '1'.

* Clear the cancel flag in case of cancellation of the " BOI note 617905
* NF (CANDAT) after end of selected reporting period
*  IF j5_pdate-high IS INITIAL.
*    rep_date_high = j5_pdate-low.
*  ELSE.
*    rep_date_high = j5_pdate-high.
*  ENDIF.
  IF J_1BNFDOC-CANCEL = 'X'.
    IF J_1BNFDOC-CANDAT GT REP_DATE_HIGH.
      CLEAR J_1BNFDOC-CANCEL.
    ENDIF.                                             " EOI note 617905
* enhancement of note 617905: BADI offered for modification of the
* cancel-flag for specific purposes
    CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->CANCEL_TREATMENT
      EXPORTING
        IS_NFDOC  = J_1BNFDOC
      IMPORTING
        CANCEL_ID = J_1BNFDOC-CANCEL.
  ENDIF.

* exclude incoming NF's that were cancelled for technical reasons /
* this means: include a cancelled NF only in Modelo 1 if it is a
* Nota Fiscal Entrada

  IF J_1BNFDOC-CANCEL = 'X'.
    CHECK J_1BNFDOC-ENTRAD = 'X'.
  ENDIF.

* exclude documents with external document number (NF number) '000000'

  CHECK J_1BNFDOC-NFNUM NE '000000'.

* exclude documents that were only created to cancel another document
* indicator: documenttype (j_1bnfdoc-doctyp) = 5

  CHECK J_1BNFDOC-DOCTYP NE '5'.

  CLEAR: KEY,
         DATA,
         PARNAD.

* determine region

  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      PARTNER_TYPE     = J_1BNFDOC-PARTYP
      PARTNER_ID       = J_1BNFDOC-PARID
      DOC_NUMBER       = J_1BNFDOC-DOCNUM
      PARTNER_FUNCTION = J_1BNFDOC-PARVW
    IMPORTING
      PARNAD           = PARNAD
    EXCEPTIONS
      OTHERS           = 04.

  IF SY-SUBRC NE 0.
    CLEAR PARNAD.
  ENDIF.

* relevant region for conhecimento is the one from the original NF
  SAVE_PARNAD_REGIO = PARNAD-REGIO.
  IF J_1BNFDOC-DOCTYP = '4'.
    PERFORM DETERMINE_REGION_CONHECIMENTO.                  "note 216289
  ENDIF.

* if the partner is not brazilian, the literal 'EX' is assigned to regio
  IF PARNAD-LAND1 NE ADDRESS1-COUNTRY.
    MOVE CONST_EX TO PARNAD-REGIO.
  ENDIF.

* determine species: nf / co

  CASE J_1BNFDOC-DOCTYP.
    WHEN '1'.
      MOVE 'NF' TO KEY-SPECIES.
    WHEN '4'.
      MOVE 'CO' TO KEY-SPECIES.
    WHEN '6'.
      MOVE 'NF' TO KEY-SPECIES.        "return is always NF
    WHEN '7'.                       "con w/o ref. "Dev_45B_ConwoREF
      MOVE 'CO' TO KEY-SPECIES.     "con w/o ref. "Dev_45B_ConwoREF
    WHEN OTHERS.
      CLEAR *J_1BNFDOC.
      SELECT SINGLE * FROM J_1BNFDOC INTO *J_1BNFDOC
             WHERE DOCNUM = J_1BNFDOC-DOCREF.
      CASE *J_1BNFDOC-DOCTYP.
        WHEN '1'.
          MOVE 'NF' TO KEY-SPECIES.
        WHEN '4'.
          MOVE 'CO' TO KEY-SPECIES.
        WHEN '7'.                       "con w/o ref. "Dev_45B_ConwoREF
          MOVE 'CO' TO KEY-SPECIES.     "con w/o ref. "Dev_45B_ConwoREF
      ENDCASE.
  ENDCASE.

* note 736398: BADI offered for modification of the NF species
  CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->ESPECIE_INDICATOR_CHANGE
    EXPORTING
      IS_J_1BNFDOC = J_1BNFDOC
      IV_ESPECIE   = KEY-SPECIES
    IMPORTING
      EV_ESPECIE   = KEY-SPECIES.

  MOVE-CORRESPONDING PARNAD TO KEY.
  MOVE-CORRESPONDING J_1BNFDOC TO KEY.
  MOVE-CORRESPONDING J_1BNFDOC TO DATA.
  MOVE J_1BNFDOC-PSTDAT(6) TO KEY-MONTHYEAR.               " note 697076

* check if document was cancelled

  IF J_1BNFDOC-CANCEL = 'X'.
    MOVE: J_1BNFDOC-CANCEL  TO DATA-CANCEL,
           TEXT-101          TO KEY-OBS_DOC.
    CLEAR KEY-PARID.
  ELSEIF J_1BNFDOC-OBSERVAT(1) = '*'.
    MOVE J_1BNFDOC-OBSERVAT+1 TO DATA-FREE_TEXT.
  ENDIF.
*M001 - BEGIN
*Move os dados de observação conforme especificação.
  CLEAR W_MWSKZ.
  SELECT SINGLE MWSKZ FROM EKBE INTO W_MWSKZ
    WHERE BELNR = J_1BNFDOC-BELNR.
  IF SY-SUBRC = 0.
    CASE W_MWSKZ.
      WHEN 'C' OR 'Y'.
        MOVE TEXT-001 TO KEY-OBS_DOC.
      WHEN 'N' OR 'S'.
        MOVE TEXT-002 TO KEY-OBS_DOC.
      WHEN 'I'.
        MOVE TEXT-003 TO KEY-OBS_DOC.
      WHEN 'P'.
        MOVE TEXT-004 TO KEY-OBS_DOC.
      WHEN 'K'.
        MOVE TEXT-005 TO KEY-OBS_DOC.
    ENDCASE.

  ENDIF.
*M001 - END

*----------------------------------------------------------------------*
*-------------------------- get j_1bindoc -----------------------------*
*----------------------------------------------------------------------*

GET J_1BINDOC.

  MOVE J_1BINDOC-NFTOT TO DATA-DOCTOT.

*----------------------------------------------------------------------*
*----------------------------get j_1bnflin-----------------------------*
*----------------------------------------------------------------------*

GET J_1BNFLIN.

  CLEAR: KEY-CFOP_NOEXT.
  CLEAR: KEY-CFOP_EXT.
  CLEAR: KEY-OBS_LIN.                               " corr. note 803426

* Check in case of ISS-taxed service                            "955768
  IF J_1BNFLIN-TMISS = 'X'.                                 "955768
    CHECK J_1BNFLIN-CFOP <> ' '.                            "955768
    CHECK INCL_SRV = 'X'.                                   "955768
  ENDIF.                                                    "955768

* convert CFOP into 6 digit form (e.g. j917a into 19917a)  - nt. 589718
  IF J_1BNFLIN-CFOP(1) CN '0123456789'.
    WRITE J_1BNFLIN-CFOP TO J_1BNFLIN-CFOP.
  ENDIF.

* no data will be extracted if document was cancelled
  IF J_1BNFDOC-CANCEL = ' '.

*    move j_1bnflin-cfop+0(3) to key-cfop_noext.           " note 553750
    MOVE J_1BNFLIN-CFOP+0(CFOP_LENGTH) TO KEY-CFOP_NOEXT.  " note 553750

*   determine, if new reporting for cfop codes X99 must be used
*   decreto is valid for Sao Paulo state from 01.01.2001 onwards
*    IF ( J_1BNFLIN-CFOP+1(3) = '991' OR          " BEGIN OF NOTE 553750
*         J_1BNFLIN-CFOP+1(3) = '999'    ) AND         "X99.9
*       ADDRESS1-REGION = SAO_PAULO  AND
*       J_1BNFDOC-PSTDAT > '20001231'.
*         MOVE J_1BNFLIN-CFOP+3(1) TO KEY-CFOP_EXT.
*    ENDIF.
    IF ( J_1BNFLIN-CFOP+1(3) = '991' OR J_1BNFLIN-CFOP+1(3) = '999'  )
       AND ADDRESS1-REGION = SAO_PAULO
       AND EXT_LEN > 0.
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.
*   determine, if new reporting for Santa Catarina must be used
*    IF ( J_1BNFLIN-CFOP(3) = '199' OR J_1BNFLIN-CFOP(3) = '299' OR
*         J_1BNFLIN-CFOP(3) = '599' OR J_1BNFLIN-CFOP(3) = '699' ) AND
*        ADDRESS1-REGION = SANTA_CATARINA AND
*        J_1BNFDOC-PSTDAT > '20001231'.
*        MOVE J_1BNFLIN-CFOP+3(2) TO KEY-CFOP_EXT.
*    ENDIF.
    IF J_1BNFLIN-CFOP+1(2) = '99'
       AND ADDRESS1-REGION = SANTA_CATARINA
       AND EXT_LEN > 0.
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.                                          " END OF NOTE 553750

* observations
    MOVE J_1BNFDOC-DOCREF TO KEY-OBS_REF.
*-------------------------------------------------------------------
*---> Begin of deletion note 553750
*   case j_1bnflin-itmtyp.
*     when '31'. "subcontr. IV of manufactured products (subsequent NF
*                                      "in subcontracting process)
*       move text-111 to key-obs_lin.
*     when '32'. "subcontracting GR of components (first NF in subc. pr)
*       move text-110 to key-obs_lin.
*     when '33'. "subcontr. IV of manufactured products (subsequent NF
*                                      "in subcontracting process
*       move text-111 to key-obs_lin.
*    when '41'. "future delivery IV item (first NF in fut. del. process)
*                                      "with IPI
*       move text-105 to key-obs_lin.
*     when '42'. "future delivery GR item (second NF in fut. del. proc.)
*                                      "with IPI
*       move text-113 to key-obs_lin.
*    when '43'. "future delivery IV item (first NF in fut. del. process)
*                                      "without IPI
*       move text-105 to key-obs_lin.
*     when '44'. "future delivery GR item (second NF in fut. del. proc.)
*                                      "without IPI
*       move text-113 to key-obs_lin.
*     when '51'. "Consignment IV item (second NF in consignment process)
*       move text-107 to key-obs_lin.
*     when '52'. "Consignment GR item (first NF in consignment process)
*       move text-108 to key-obs_lin.
*     when '62'. "Third party IV item supplier to seller (case A - NF 3)
*       concatenate text-109 j_1bnfdoc-observat
*                   into key-obs_lin separated by space.
*       clear key-obs_ref.
*   when '63'. "Third party GR item supplier to customer (case A,B -NF2)
*       concatenate text-113 j_1bnfdoc-observat
*                   into key-obs_lin separated by space.
*       clear key-obs_ref.
*     when '64'.  "Third party IV item supplier to seller (case B - NF0)
*       move text-105 to key-obs_lin.
*     when '65'.  "Third party  symbolic GR item supplier to seller
*                                      " (case B - NF 3)
*       concatenate text-109 j_1bnfdoc-observat
*                   into key-obs_lin separated by space.
*       concatenate key-obs_lin  '/' text-113
*                   into key-obs_lin.
*   endcase.
*---> End of deletion note 553750
*
*---> Begin of insertion note 553750
    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE
         WITH KEY ITMTYP = J_1BNFLIN-ITMTYP.
    IF SY-SUBRC IS INITIAL.
*---> text number for reporting filled
      IF NOT GS_NFITMRULE-TEXTMOD1 IS INITIAL.
*---> get special texts from customizing table J_1BMODTEXT
        READ TABLE GT_J_1BMODTEXT INTO GS_J_1BMODTEXT
          WITH KEY SPRAS   = SY-LANGU
                   TEXTNUM = GS_NFITMRULE-TEXTMOD1.
*---> text defined take it from customizing
        IF SY-SUBRC IS INITIAL.
          MOVE GS_J_1BMODTEXT-TEXT TO KEY-OBS_LIN.
*---> text not defined in customizing -> use report text elements
        ELSE.
          CONCATENATE C_TEXT GS_NFITMRULE-TEXTMOD1 INTO LV_NAME.
          ASSIGN (LV_NAME) TO <A>.
          WRITE <A> TO KEY-OBS_LIN.
        ENDIF.
*---> search if a place holder is defined in the text symbol
        SEARCH KEY-OBS_LIN FOR '&1'.
*---> if yes replace it with observations
        IF SY-SUBRC IS INITIAL.
          REPLACE '&1'  IN KEY-OBS_LIN WITH J_1BNFDOC-OBSERVAT.
        ENDIF.
      ENDIF.
*---> clear key-obs_ref when flag is set
      IF GS_NFITMRULE-CLEARREF1 = 'X'.
        CLEAR KEY-OBS_REF.
      ENDIF.
    ENDIF.
*---> End of insertion note 553750
*-----------------------------------------------------------------------
*---> Begin of deletion note 553750
**    cfop-numbers 191, 291, 391 indicate fixed assets
*
*    if key-cfop_noext = '191' or
*       key-cfop_noext = '291' or
*       KEY-CFOP_NOEXT = '391' OR
*       KEY-CFOP_NOEXT = '173' OR                   " enh. note 178891
*       KEY-CFOP_NOEXT = '273'.                     " enh. note 178891
*      move text-102 to key-obs_lin.    "ativo fixo
*    endif.
*
**    cfop-numbers 197, 297, 397 indicate consumption
*
*    if key-cfop_noext = '197' or
*       key-cfop_noext = '297' or
*       KEY-CFOP_NOEXT = '397' OR
*       KEY-CFOP_NOEXT = '174' OR                       " note 178891
*       KEY-CFOP_NOEXT = '274'.                         " note 178891
*      move text-114 to key-obs_lin.    "consumo
*    endif.
*---> End of deletion note 553750
*
*---> Begin of insertion note 553750
    CASE J_1BNFLIN-MATUSE.
      WHEN '2'.
*        move text-114 to key-obs_lin.    "consumo         " note 803426
        CONCATENATE TEXT-114 KEY-OBS_LIN INTO              " note 803426
               KEY-OBS_LIN SEPARATED BY SPACE.  "consumo   " note 803426
      WHEN '3'.
*        move text-102 to key-obs_lin.    "ativo fixo      " note 803426
        CONCATENATE TEXT-102 KEY-OBS_LIN INTO              " note 803426
               KEY-OBS_LIN SEPARATED BY SPACE. "ativo fixo " note 803426
    ENDCASE.
*---> End of insertion note 553750

* determine observations text in case of returns or complementars

    CASE J_1BNFDOC-DOCTYP.
      WHEN '6'.
        MOVE TEXT-103 TO KEY-OBS_LIN.
        IF NOT J_1BNFLIN-DOCREF IS INITIAL.
          MOVE J_1BNFLIN-DOCREF TO KEY-OBS_REF.
        ELSE.
          MOVE J_1BNFDOC-DOCREF TO KEY-OBS_REF.
        ENDIF.
      WHEN '2'.
        MOVE TEXT-104 TO KEY-OBS_LIN.
    ENDCASE.

    MOVE J_1BNFLIN-ITMTYP TO DATA-ITMTYP.
  ENDIF.

*----------------------------------------------------------------------*
*----------------------------get j_1binlin-----------------------------*
*----------------------------------------------------------------------*

GET J_1BINLIN.

  CLEAR: DATA-NFTOT.

* no data will be extracted if document was cancelled

  IF J_1BNFDOC-CANCEL = ' '.

    MOVE-CORRESPONDING J_1BINLIN TO DATA.

  ENDIF.

*----------------------------------------------------------------------*
*----------------------------get j_1bnfstx-----------------------------*
*----------------------------------------------------------------------*

GET J_1BNFSTX.

*ENHANCEMENT-POINT J_1BLB01_01 SPOTS ES_J_1BLB01.

*--> Do not consider incoming tax items for        "622370
*--> Zona Franca offsetting                        "622370
  CHECK J_1BNFSTX-TAXTYP <> CONST_TAXTYPE-ICZG.             "622370

  CLEAR: KEY-RATE,
         KEY-TAXGRP,
         KEY-CODIGO,
         DATA-BASE,
         DATA-TAXVAL,
         DATA-STATTX,
         DATA-OBS_IPIPAUTA.

  CLEAR INTJ_1BAJ.
  READ TABLE INTJ_1BAJ WITH KEY TAXTYP = J_1BNFSTX-TAXTYP
                       BINARY SEARCH.
  IF SY-SUBRC NE 0.
* write message to errorlog
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        ARBGB = '8B'
        MSGTY = 'E'
        MSGV1 = J_1BNFLIN-REFTYP
        MSGV2 = J_1BNFLIN-REFKEY
        MSGV3 = J_1BNFDOC-NFNUM
        MSGV4 = J_1BNFSTX-TAXTYP
        TXTNR = '456'.
    REJECT.
  ENDIF.

*---> Begin of deletion note 553750
** begin of Note 178891
*  IF ( KEY-CFOP_NOEXT = '197' OR
*       KEY-CFOP_NOEXT = '297' OR
*       KEY-CFOP_NOEXT = '397' OR
*       KEY-CFOP_NOEXT = '174' OR           " enlargement of note 178891
*       KEY-CFOP_NOEXT = '274' OR           " enlargement of note 178891
** further enlargement of note 178891 to purchasing of fixed assets
*       KEY-CFOP_NOEXT = '173' OR KEY-CFOP_NOEXT = '273' OR " nt. 178891
*       KEY-CFOP_NOEXT = '191' OR KEY-CFOP_NOEXT = '291' OR " nt. 178891
*       KEY-CFOP_NOEXT = '391' ) AND                        " nt. 178891
*       INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS.
*---> End of deletion note 553750
  IF ( J_1BNFLIN-MATUSE = '2' OR                        " note 553750
                         J_1BNFLIN-MATUSE = '3' ) AND   " note 553750
     INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS.              " note 553750
*      consumption material and ICMS tax line
*      => ICMS base must be reported with IPI tax value subtracted
*      => Modelo 1 and NF Writer show different values

    SELECT * FROM  *J_1BNFSTX
           WHERE  DOCNUM  = J_1BNFSTX-DOCNUM
           AND    ITMNUM  = J_1BNFSTX-ITMNUM
           AND    TAXTYP  IN IPI_TYPES.
      EXIT.                             "only one IPI tax line possible
    ENDSELECT.

    IF SY-SUBRC = 0.
      IF *J_1BNFSTX-TAXVAL < J_1BNFSTX-OTHBAS.
        SUBTRACT *J_1BNFSTX-TAXVAL FROM J_1BNFSTX-OTHBAS.
      ENDIF.
    ENDIF.
  ENDIF.
* end of note 178891

* ICMS freight/ST:
* base to be printed is ICFR base, if no ICFS exists; otherwise it's
* the ICFS base - sorting is ICFR -> ICFS -> ICMS -> ICST -> IPI

  IF J_1BNFDOC-CANCEL = ' '.
    IF INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICSTFREIGHT OR
       INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMSFREIGHT.
      IF J_1BNFSTX-BASE NE 0.
        DATA-OBS_ICFRBASE = J_1BNFSTX-BASE.
      ELSE.
        DATA-OBS_ICFRBASE = J_1BNFSTX-OTHBAS.
      ENDIF.
      DATA-OBS_ICFRTAX = DATA-OBS_ICFRTAX + J_1BNFSTX-TAXVAL.
    ENDIF.
  ENDIF.

  CHECK INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS
     OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-IPI
     OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-SUBTRIB
     OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISSS                "955768
     OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISSP                "955768
     OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISS                 "955768

* START OF NOTE 386073, A
     OR  ( INTJ_1BAJ-TAXGRP = 'ICOP' AND  "ICMS Complementar
           J_1BNFLIN-MATUSE  = 2     AND  "Consumption
           J_1BNFLIN-CFOP(1) = 2 )        "interstate

     OR  ( INTJ_1BAJ-TAXGRP = 'ICOP' AND  "ICMS Complementar
           J_1BNFLIN-MATUSE  = 3     AND  "Asset
           J_1BNFLIN-CFOP(1) = 2 ).       "interstate
* END OF NOTE 386073, A

* START OF NOTE 386073, B
  IF INTJ_1BAJ-TAXGRP  = CONST_TAXGRP-SUBTRIB AND
     ( J_1BNFLIN-CFOP(1) = 1 OR            "intrastate, 270502
       J_1BNFLIN-CFOP(1) = 2 ).            "interstate
    DATA-ST_CONSUMO = 'X'.
  ELSE.
    CLEAR DATA-ST_CONSUMO.
  ENDIF.
* END OF NOTE 386073, B

*ENHANCEMENT-SECTION     J_1BLB01_02 SPOTS ES_J_1BLB01.
  IF INTJ_1BAJ-TAXGRP NE CONST_TAXGRP-ICMS.
* clear observations fields, so that output happens only once
    CLEAR KEY-OBS_LIN.
    CLEAR KEY-OBS_REF.
  ENDIF.
*END-ENHANCEMENT-SECTION.

* no further data will be extracted if document was cancelled
  IF J_1BNFDOC-CANCEL = 'X'.
    EXTRACT EXDATA.
* Reporting of ISS-taxed services                               "955768
  ELSEIF J_1BNFDOC-CANCEL = ' ' AND                         "955768
         J_1BNFLIN-TMISS = 'X'.                             "955768
    KEY-TAXGRP = 'ICMS'.                                    "955768
    MOVE '2' TO KEY-CODIGO.                                 "955768
    MOVE-CORRESPONDING J_1BNFSTX TO KEY.                    "955768
    MOVE-CORRESPONDING J_1BNFSTX TO DATA.                   "955768
    CLEAR KEY-RATE.                                         "955768
    IF J_1BNFSTX-EXCBAS <> 0.                               "955768
      MOVE J_1BNFSTX-EXCBAS TO DATA-BASE.                   "955768
    ENDIF.                                                  "955768
    IF J_1BNFSTX-OTHBAS <> 0.                               "955768
      MOVE J_1BNFSTX-OTHBAS TO DATA-BASE.                   "955768
    ENDIF.                                                  "955768
    EXTRACT EXDATA.                                         "955768
    KEY-TAXGRP = 'IPI'.                                     "955768
    EXTRACT EXDATA.                                         "955768
  ELSE.                                "j_1bnfdoc-cancel = ' '.

    MOVE INTJ_1BAJ-TAXGRP TO KEY-TAXGRP.
    MOVE-CORRESPONDING J_1BNFSTX TO KEY.
    MOVE-CORRESPONDING J_1BNFSTX TO DATA.

* Don't print rate in case of IPI

    IF KEY-TAXGRP = CONST_TAXGRP-IPI.
      CLEAR KEY-RATE.
    ENDIF.

*    note 210543
*    IF J_1BNFSTX-RECTYPE = '1'.
*      CLEAR DATA-BASE.
*    ENDIF.

    IF J_1BNFSTX-STATTX = 'X'.
      CLEAR DATA-TAXVAL.
      CLEAR DATA-BASE.
    ENDIF.

* the base value and codigo will be filled according to the following
* rule: if base > 0        => one line with codigo = 1 + base
*       if exempt base > 0 => one line with codigo = 2 + ex. base
*       if other base > 0  => one line with codigo = 3 + oth. base
*       several bases > 0  => several lines with different codigos
*       all bases = 0      AND
* a)      taxtyp = ICZF (Zona Franca) or taxtyp = ICMF (Sub. Trib. on
*         freight)         => NO output/no line will be extracted
* b)      taxtyp not ICZF or ICMF and NO item reference to orig. doc. +
*         tax amount > 0   => one line with codigo = 1
* c)      taxtyp not ICZF or ICMF and item ref. to orig. doc. exists +
*         tax amount > 0   => one line with codigo 1 or 2 or 3,
*         depending on the original codigo
* d)      taxtyp not ICZF or ICMF and item ref. to orig. doc. exists +
*         tax amount = 0   => one line with codigo 3
* EXAMPLES: Complementar do IPI, NF simples remessa

    IF J_1BNFSTX-BASE > 0.
      MOVE '1' TO KEY-CODIGO.
      EXTRACT EXDATA.
    ENDIF.
    IF J_1BNFSTX-EXCBAS > 0.
      MOVE '2' TO KEY-CODIGO.
      IF J_1BNFSTX-STATTX NE 'X'.
        MOVE J_1BNFSTX-EXCBAS TO DATA-BASE.
      ENDIF.
      CLEAR DATA-TAXVAL.
* to avoid duplicate output of valor contabil
      IF J_1BNFSTX-BASE > 0.
        CLEAR DATA-NFTOT.
      ENDIF.
      EXTRACT EXDATA.
    ENDIF.
    IF J_1BNFSTX-OTHBAS > 0.
      MOVE '3' TO KEY-CODIGO.
*      note 210543
*      if j_1bnfstx-rectype is initial  and             "no IPI Pauta
*         j_1bnfstx-stattx ne 'X'.
      IF J_1BNFSTX-STATTX NE 'X'.
        MOVE J_1BNFSTX-OTHBAS TO DATA-BASE.
      ENDIF.
* START OF NOTE 386073, B
      IF J_1BNFSTX-TAXTYP(3) NE CONST_TAXGRP-SUBTRIB(3).
        CLEAR DATA-TAXVAL.
      ENDIF.
*      clear data-taxval.
* END OF NOTE 386073, B
      CLEAR KEY-RATE.
      IF J_1BNFSTX-RECTYPE = '1'.      "IPI Pauta
        DATA-OBS_IPIPAUTA = J_1BNFSTX-TAXVAL.
      ENDIF.
* to avoid duplicate output of valor contabil
      IF J_1BNFSTX-BASE > 0 OR J_1BNFSTX-EXCBAS > 0.
        CLEAR DATA-NFTOT.
      ENDIF.
      EXTRACT EXDATA.
    ENDIF.
*     'all bases = 0' case
    IF NOT J_1BNFSTX-TAXTYP = CONST_TAXTYPE-ICZF AND
       NOT J_1BNFSTX-TAXTYP = CONST_TAXTYPE-ICMF AND
       J_1BNFSTX-BASE       = 0 AND
       J_1BNFSTX-EXCBAS     = 0 AND
       J_1BNFSTX-OTHBAS     = 0.
*        check if at least the tax amount is <> 0
      IF J_1BNFSTX-TAXVAL <> 0.
*           check if NF reference for this line exists
        IF NOT J_1BNFLIN-DOCREF IS INITIAL AND
           NOT J_1BNFLIN-ITMREF IS INITIAL.
*              read the taxes from original document
          SELECT SINGLE * FROM *J_1BNFSTX
                        WHERE DOCNUM = J_1BNFLIN-DOCREF
                        AND   ITMNUM = J_1BNFLIN-ITMREF
                        AND   TAXTYP = J_1BNFSTX-TAXTYP.
          IF SY-SUBRC = 0.
            IF *J_1BNFSTX-BASE > 0.
              MOVE '1' TO KEY-CODIGO.
            ENDIF.
            IF *J_1BNFSTX-EXCBAS > 0.
              MOVE '2' TO KEY-CODIGO.
            ENDIF.
            IF *J_1BNFSTX-OTHBAS > 0.
              MOVE '3' TO KEY-CODIGO.
            ENDIF.
          ENDIF.
        ENDIF.         "not j_1bnflin-docref is initial...
*           NF item reference wasn't filled or not correct...
        IF KEY-CODIGO IS INITIAL.
          MOVE '1' TO KEY-CODIGO.
        ENDIF.
      ELSE.             " => j_1bnfstx-taxval = 0  and all bases = 0
        MOVE '3' TO KEY-CODIGO.
      ENDIF.                           "j_1bnfstx-taxval <> 0
      IF KEY-CODIGO = '2' OR KEY-CODIGO = '3'.
        CLEAR DATA-TAXVAL.
        CLEAR KEY-RATE.
      ENDIF.
      EXTRACT EXDATA.
    ENDIF.               "not j_1bnfstx-taxtyp = const_taxtype-iczf...
    IF J_1BNFSTX-BASE < 0.                  " boi note 865692
      MOVE '1' TO KEY-CODIGO.
      EXTRACT EXDATA.
    ENDIF.                                  " eoi note 865692

  ENDIF.                               "j_1bnfdoc-cancel = ...

* if main partner in the document was a one time partner:
* move additional document information to the list ot_list of
* one time partners
  IF J_1BNFDOC-PARXCPDK = 'X' AND J_1BNFDOC-CANCEL = ' '.
    CLEAR OT_LIST.
    READ TABLE OT_LIST WITH KEY PARID  = J_1BNFDOC-PARID
                                PARTYP = J_1BNFDOC-PARTYP
                                CGC    = PARNAD-CGC
                                CPF    = PARNAD-CPF.
    IF SY-SUBRC NE 0.
      PARNAD-REGIO = SAVE_PARNAD_REGIO.         "readjust correct region
      MOVE-CORRESPONDING: J_1BNFDOC TO OT_LIST,
                          PARNAD    TO OT_LIST.
      APPEND OT_LIST.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*-----------------------end-of-selection-------------------------------*
*----------------------------------------------------------------------*

END-OF-SELECTION.
* P45K005034
  PERFORM INI_OUTPUT.

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
*------------------------------- sort ---------------------------------*
*------------------ the extract according to the key ------------------*
*----------------------------------------------------------------------*

  SORT.

*----------------------------------------------------------------------*
*-------------sort the list of one time partners ----------------------*
*----------------------------------------------------------------------*

  SORT OT_LIST.

*----------------------------------------------------------------------*
*--- condense records belonging to one nota fiscal if cfops (without --*
*- extensions) and taxrates correspond and write the condensed record -*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-------------------------------- loop --------------------------------*
*----------------------------------------------------------------------*

  LOOP.

*----------------------------------------------------------------------*
*---------------------- at new key-obs_ref ----------------------------*
*----------------------------------------------------------------------*

* write condensed line

    AT NEW KEY-OBS_REF.
      IF FIRST_RECORD  = ' '.
        IF MOD_1 = 'X'.
          PERFORM OUTPUT USING OUT.
        ENDIF.
        IF DOCNUM = KEY-DOCNUM.    "to avoid duplication of header info
          "within the output of one document
          OUT-DUPL = 'X'.
        ELSE.
          OUT-DUPL = ' '.
        ENDIF.
        CLEAR TAXSUMS.
        CLEAR LINE_WITH_NONSTAT_TAX.
      ENDIF.
    ENDAT.

*----------------------------------------------------------------------*
*------------------------- at new key-parid  --------------------------*
*----------------------------------------------------------------------*

* Update of Lista de Códigos de Emitentes (Modelo 10)

    AT NEW KEY-PARID.
      IF KEY-PARID NE SPACE.
        READ TABLE LIST WITH KEY PARID = KEY-PARID BINARY SEARCH.

        IF SY-SUBRC NE 0.
          TABIX = SY-TABIX.
          MOVE-CORRESPONDING: KEY TO LIST,
                              DATA TO LIST.
          INSERT LIST INDEX TABIX.
        ENDIF.
      ENDIF.
    ENDAT.

*----------------------------------------------------------------------*
*-------------------------- at new key-pstdat -------------------------*
*----------------------------------------------------------------------*

* write daily totals

    AT NEW KEY-PSTDAT.
      CHECK MOD_1 = 'X'.
      IF FIRST_RECORD  = ' '.
        MOVE-CORRESPONDING TOTAL TO TOTAL_10.
        IF QUINZENA IS INITIAL.                            " note 697076
          IF OLDDAY LE 10.               "day 1-10: period 1
            TOTAL_10-NUMBER = '1'.
            COLLECT TOTAL_10.
          ELSE.
            IF OLDDAY LE 20.             "day 11-20: period 2
              TOTAL_10-NUMBER = '2'.
              COLLECT TOTAL_10.
            ELSE.
              TOTAL_10-NUMBER = '3'.     "day > 20: period 3
              COLLECT TOTAL_10.
            ENDIF.
          ENDIF.
        ENDIF.                                         " BOI note 697076
        IF QUINZENA = 'X'.
          IF OLDDAY LE 15.               "day 1-15: period 1b
            TOTAL_10-NUMBER = '1'.
            COLLECT TOTAL_10.
          ELSE.
            TOTAL_10-NUMBER = '2'.       "day > 15: period 2b
            COLLECT TOTAL_10.
          ENDIF.
        ENDIF.                                         " EOI note 697076
        PERFORM TOTAL_OUTPUT USING TOTAL.
*       write 10 days totals
        IF DECENDIO = 'X'.                                 " note 697076
          IF OLDDAY LE 10 AND KEY-PSTDAT+6(2) > 10.
            READ TABLE TOTAL_10 WITH KEY NUMBER = '1'.
            MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*          SKIP.                                           " note 768128
            WRITE: /1 WG_BLANKS(28) COLOR OFF,
                  29  SY-VLINE.                              " note 768128
            WRITE: AT C_DECEN TOTAL_10-NUMBER, '.', TEXT-380.
            WRITE: 199 SY-VLINE.                             " note 768128
            PERFORM TOTAL_OUTPUT USING TOTAL.
*          skip.                                           " note 768128

          ENDIF.
          IF OLDDAY LE 20 AND KEY-PSTDAT+6(2) > 20.
            READ TABLE TOTAL_10 WITH KEY NUMBER = '2'.
            MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*          skip.                                            "note 768128
            WRITE: /1 WG_BLANKS(28) COLOR OFF,  29  SY-VLINE.                               "note 768128
*            WRITE: AT /C_DECEN TOTAL_10-NUMBER, '.', TEXT-380."note 768128
            WRITE: AT C_DECEN TOTAL_10-NUMBER, '.', TEXT-380. "note 768128
            WRITE: 199 SY-VLINE.
            PERFORM TOTAL_OUTPUT USING TOTAL.
*            skip.                                          "note 768128
          ENDIF.
        ENDIF.                                         " BOI note 697076
*       write 15 days totals
        IF QUINZENA = 'X'.
          IF OLDDAY LE 15 AND KEY-PSTDAT+6(2) > 15.
            READ TABLE TOTAL_10 WITH KEY NUMBER = '1'.
            MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*           skip.
            WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.
*          WRITE: AT /c_decen total_10-number, '.',text-381."note 768128
            WRITE: AT C_DECEN TOTAL_10-NUMBER, '.',TEXT-381. "note 768128
            WRITE: 199 SY-VLINE.                             "note 768128
            PERFORM TOTAL_OUTPUT USING TOTAL.
*           skip.                                           "note 768128
          ENDIF.
        ENDIF.                                         " EOI note 697076
      ENDIF.

      OLDDAY = KEY-PSTDAT+6(2).
      FIRST_RECORD     = ' '.
    ENDAT.

    AT NEW KEY-MONTHYEAR.                              " BOI note 697076
      IF FIRST_RECORD_MONTHYEAR = ' '.
*   At last day of month: printing subtotal
        READ TABLE TOTAL_10 WITH KEY NUMBER = TOTAL_10-NUMBER.
        MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*      SKIP.                                                "note 768128
        IF DECENDIO = 'X'.
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
*       WRITE: AT /c_decen total_10-number, '.', text-380.  "note 768128
          WRITE: AT C_DECEN TOTAL_10-NUMBER, '.', TEXT-380.   "note 768128
          WRITE: 199 SY-VLINE.                                "note 768128
          PERFORM TOTAL_OUTPUT USING TOTAL.
        ELSEIF QUINZENA = 'X'.
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
*       WRITE: AT /c_decen total_10-number, '.', text-381.  "note 768128
          WRITE: AT C_DECEN TOTAL_10-NUMBER, '.', TEXT-381.   "note 768128
          WRITE: 199 SY-VLINE.                                "note 768128
          PERFORM TOTAL_OUTPUT USING TOTAL.
        ENDIF.
*      SKIP.                                                "note 768128
*   Calculate monthly total
        CLEAR TOTAL_10.
        LOOP AT TOTAL_10 WHERE NUMBER CA '123'.
          TOTAL_10-NUMBER = '4'.
          COLLECT TOTAL_10.
        ENDLOOP.
*   Calculate running monthly total
        CLEAR TOTAL_10.
        READ TABLE TOTAL_10 WITH KEY NUMBER = '4'.
        TOTAL_10-NUMBER = '5'.
        COLLECT TOTAL_10.
*   Printing monthly total
        MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                              " note 768128
        WRITE: AT C_DECEN TEXT-382.   " mês
        WRITE: 199 SY-VLINE.                                "note 768128
        PERFORM TOTAL_OUTPUT USING TOTAL.
        LOOP AT TOTAL_10 WHERE NUMBER CA '1234'.
          DELETE TOTAL_10.
        ENDLOOP.
      ENDIF.
    ENDAT.
    CLEAR FIRST_RECORD_MONTHYEAR.                      " EOI note 697076

*----------------------------------------------------------------------*
*   condense on documento fiscal-level per cfop and taxgroup and
*   taxrate

    TAXSUMS-NFTOT  = TAXSUMS-NFTOT  + DATA-NFTOT.
    TAXSUMS-BASE   = TAXSUMS-BASE   + DATA-BASE.
    TAXSUMS-TAXVAL = TAXSUMS-TAXVAL + DATA-TAXVAL.
    TAXSUMS-OBS_IPIPAUTA = TAXSUMS-OBS_IPIPAUTA + DATA-OBS_IPIPAUTA.

*   build daily totals for calculation bases and taxvalues per
*   taxgroup and codigo de valores fiscais

    CASE KEY-CODIGO.
      WHEN '1'.
        CASE KEY-TAXGRP.
          WHEN CONST_TAXGRP-ICMS.
            TOTAL-ICMSBASE1     = TOTAL-ICMSBASE1    + DATA-BASE.
            TOTAL-ICMSVAL1       = TOTAL-ICMSVAL1     + DATA-TAXVAL.
          WHEN CONST_TAXGRP-IPI.
            TOTAL-IPIBASE1       = TOTAL-IPIBASE1     + DATA-BASE.
            TOTAL-IPIVAL1        = TOTAL-IPIVAL1      + DATA-TAXVAL.
          WHEN CONST_TAXGRP-SUBTRIB.
            TOTAL-SUBTRIBBASE1   = TOTAL-SUBTRIBBASE1 + DATA-BASE.
            TOTAL-SUBTRIBVAL1    = TOTAL-SUBTRIBVAL1  + DATA-TAXVAL.
* START OF NOTE 386073, A
          WHEN 'ICOP'.
            TOTAL-ICOPBASE1   = TOTAL-ICOPBASE1 + DATA-BASE.
            TOTAL-ICOPVAL1    = TOTAL-ICOPVAL1  + DATA-TAXVAL.
* END OF NOTE 386073, A
        ENDCASE.
      WHEN '2'.
        CASE KEY-TAXGRP.
          WHEN CONST_TAXGRP-ICMS.
            TOTAL-ICMSBASE2      = TOTAL-ICMSBASE2    + DATA-BASE.
          WHEN CONST_TAXGRP-IPI.
            TOTAL-IPIBASE2       = TOTAL-IPIBASE2     + DATA-BASE.
        ENDCASE.
      WHEN '3'.
        CASE KEY-TAXGRP.
          WHEN CONST_TAXGRP-ICMS.
            TOTAL-ICMSBASE3      = TOTAL-ICMSBASE3    + DATA-BASE.
          WHEN CONST_TAXGRP-IPI.
            TOTAL-IPIBASE3       = TOTAL-IPIBASE3     + DATA-BASE.
          WHEN CONST_TAXGRP-SUBTRIB.
            TOTAL-SUBTRIBBASE3   = TOTAL-SUBTRIBBASE3 + DATA-BASE.
        ENDCASE.
    ENDCASE.

    DOCNUM = KEY-DOCNUM.
    MOVE-CORRESPONDING KEY     TO OUT.
    MOVE-CORRESPONDING DATA    TO OUT.
    MOVE-CORRESPONDING TAXSUMS TO OUT.

*   only if all tax lines of tax group are statistical
*   no bases to be printed                                   note 206361
    CLEAR OUT-STATTX.
    IF DATA-STATTX IS INITIAL.
      LINE_WITH_NONSTAT_TAX = 'X'. "one non-statistical tax line found
    ELSEIF LINE_WITH_NONSTAT_TAX IS INITIAL.
      OUT-STATTX = 'X'.          "only, if all tax lines are statistical
    ENDIF.

* fill state summary tables
    IF ( KEY-TAXGRP = CONST_TAXGRP-ICMS
      OR KEY-TAXGRP = CONST_TAXGRP-SUBTRIB )
      AND KEY-CODIGO NE '2'.
      CLEAR STATE_SUMMARY_ICMS.
      MOVE-CORRESPONDING KEY TO STATE_SUMMARY_ICMS.
      MOVE DATA-BASE         TO STATE_SUMMARY_ICMS-BASE.
      MOVE DATA-TAXVAL       TO STATE_SUMMARY_ICMS-TAXVAL.
      COLLECT STATE_SUMMARY_ICMS.
    ENDIF.

* cfop summaries
* fill cfop_line
    IF DATA-CANCEL = ' '.
*---> Start of deletion note 553750
*      if key-taxgrp = const_taxgrp-icms" Suppress valor contabil for
*     and data-itmtyp ne '41'          " future delivery IV item (first
*                                      " NF in fut. del. process) w.IPI
*     and data-itmtyp ne '43'          " future delivery IV item (first
*                                      " NF in fut. del. process) no IPI
*    and data-itmtyp ne '63'           " Third party GR item supplier to
*                                      " customer (case A,B - NF2)
*      AND DATA-ITMTYP NE '64'         " Third party IV item supplier
*                                      "  to seller (case B - NF0)
*      AND DATA-ITMTYP NE '51'.        " Consignment IV item (2nd NF in
*                                      " consign. process); note 450610
*---> End of deletion note 553750
      READ TABLE GT_NFITMRULE INTO GS_NFITMRULE        "note 553750
           WITH KEY ITMTYP = DATA-ITMTYP.              "note 553750
      IF SY-SUBRC IS INITIAL                               " note 782912
      AND KEY-TAXGRP = CONST_TAXGRP-ICMS                   " note 782912
*      if key-taxgrp = const_taxgrp-icms      " note 571316:  nt. 782912
      AND GS_NFITMRULE-IGNORETOTAL <> 'X'.             "note 553750
        CLEAR CFOP_LINE.
        CLEAR CFOP_LINE_TOT.
        MOVE-CORRESPONDING KEY  TO CFOP_LINE.
        MOVE DATA-NFTOT  TO CFOP_LINE-VALORCONT.
        CASE KEY-CODIGO.
          WHEN '1'.
            MOVE DATA-TAXVAL TO CFOP_LINE-ICMSVAL1.
            MOVE DATA-BASE   TO CFOP_LINE-ICMSBASE1.
          WHEN '2'.
            MOVE DATA-BASE   TO CFOP_LINE-ICMSBASE2.
          WHEN '3'.
            MOVE DATA-BASE   TO CFOP_LINE-ICMSBASE3.
        ENDCASE.
        COLLECT CFOP_LINE.
        MOVE CFOP_LINE-CFOP_NOEXT(1) TO CFOP_LINE_TOT-CFOP_FIRST.
        MOVE-CORRESPONDING CFOP_LINE TO CFOP_LINE_TOT.
        COLLECT CFOP_LINE_TOT.
        MOVE '9' TO CFOP_LINE_TOT-CFOP_FIRST.
        MOVE-CORRESPONDING CFOP_LINE TO CFOP_LINE_TOT.
        COLLECT CFOP_LINE_TOT.
      ENDIF.
    ENDIF.

  ENDLOOP.

*----------------------------------------------------------------------*
*-------------------------- check if book is empty --------------------*
*----------------------------------------------------------------------*
  IF MOD_1 = 'X'.
    IF FIRST_RECORD = 'X'.
**      PERFORM F_FAKE.
      WRITE: /1 WG_BLANKS(28) COLOR OFF,
             29 SY-VLINE.                                   "note 768128
*    WRITE:/ text-270.            "no values were selected "note 768128
      WRITE: TEXT-270.             "no values were selected "note 768128
      WRITE: 199 SY-VLINE.                                  "note 768128
      ZULINE.                                                "note 768128
      CHECK '0' = '1'.
    ELSE.                           "book is not empty



*----------------------------------------------------------------------*
*--------------write last record coming from the extract---------------*
*----------------------------------------------------------------------*

      PERFORM OUTPUT USING OUT.

*----------------------------------------------------------------------*
*---------------------write last daily total---------------------------*
*----------------------------------------------------------------------*

      MOVE-CORRESPONDING TOTAL TO TOTAL_10.
      IF QUINZENA IS INITIAL.                                " note 697076
        IF OLDDAY LE 10.                   "day 1-10: period 1
          TOTAL_10-NUMBER = '1'.
          COLLECT TOTAL_10.
        ELSE.
          IF OLDDAY LE 20.                 "day 11-20: period 2
            TOTAL_10-NUMBER = '2'.
            COLLECT TOTAL_10.
          ELSE.
            TOTAL_10-NUMBER = '3'.         "day > 20: period 3
            COLLECT TOTAL_10.
          ENDIF.
        ENDIF.
      ENDIF.                                             " BOI note 697076
      IF QUINZENA  = 'X'.
        IF OLDDAY LE 15.                   "day 1-15: period 1b
          TOTAL_10-NUMBER = '1'.
          COLLECT TOTAL_10.
        ELSE.
          TOTAL_10-NUMBER = '2'.           "day > 15: period 2b
          COLLECT TOTAL_10.
        ENDIF.
      ENDIF.                                             " EOI note 697076

      PERFORM TOTAL_OUTPUT USING TOTAL.

      READ TABLE TOTAL_10 WITH KEY NUMBER = TOTAL_10-NUMBER.
      MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*    skip.                                                  "note 768128
*    write: at /c_decen total_10-number, '.', text-380." DEL note 697076
*    perform total_output using total.                 " DEL note 697076
      IF DECENDIO = 'X'.                                 " BOI note 697076
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                   "note 768128
*     WRITE: AT /c_decen total_10-number, '.', text-380.    "note 768128
        WRITE: AT C_DECEN TOTAL_10-NUMBER, '.', TEXT-380.     "note 768128
        WRITE: 199 SY-VLINE.
        PERFORM TOTAL_OUTPUT USING TOTAL.
      ELSEIF QUINZENA = 'X'.
        WRITE: 1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                   "note 768128
*     WRITE: AT /c_decen total_10-number, '.', text-381.    "note 768128
        WRITE: AT C_DECEN TOTAL_10-NUMBER, '.', TEXT-381.     "note 768128
        WRITE: 199 SY-VLINE.                                  "note 768128
        PERFORM TOTAL_OUTPUT USING TOTAL.
      ENDIF.                                             " EOI note 697076
*    skip.                                                  "note 768128

*     calculate monthly total
      CLEAR TOTAL_10.
      LOOP AT TOTAL_10 WHERE NUMBER CA '123'.                " note 697076
        TOTAL_10-NUMBER = '4'.
        COLLECT TOTAL_10.
      ENDLOOP.
*     output monthly total
      IF REP_DATE_HIGH(6) <> J5_PDATE-LOW(6).                " note 697076
        READ TABLE TOTAL_10 WITH KEY NUMBER = '4'.           " note 697076
        MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*    write: text-385.                                      " note 697076
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                  " note 768128
        WRITE: TEXT-382.      " mês                            note 697076
        WRITE: 199 SY-VLINE.                                 " note 768128
        PERFORM TOTAL_OUTPUT USING TOTAL.
      ENDIF.                                             " BOI note 697076
* Calculate total geral
      READ TABLE TOTAL_10 WITH KEY NUMBER = '4'.
      TOTAL_10-NUMBER = '5'.
      COLLECT TOTAL_10.
* Output total geral
      READ TABLE TOTAL_10 WITH KEY NUMBER = '5'.
      MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                  " note 768128
      WRITE: TEXT-385.
      WRITE: 199 SY-VLINE.                                 " note 768128
      PERFORM TOTAL_OUTPUT USING TOTAL.                  " EOI note 697076
* START OF NOTE 386073, A
      MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                    " note 768128
      WRITE: TEXT-271,                   "Diferencial de alíquota
             AT C_TOTAL 'ICMS',
             AT C_NFTOT(L_NFTOT) TOTAL-ICOPVAL1
                CURRENCY T001-WAERS.
      WRITE: 199 SY-VLINE.                                    "note 768128
*    SKIP.                                                  "note 768128
* END OF NOTE 386073, A
*    skip.                                                  "note 768128


*----------------------------------------------------------------------*
*----------------------write summary per cfop--------------------------*
*----------------------------------------------------------------------*

      SORT CFOP_LINE.
      SORT CFOP_LINE_TOT.
*    skip 2.                                                "note 768128
      FORMAT INTENSIFIED COLOR COL_NORMAL.
      ZULINE.                                                  "note 768128
      RESERVE 4 LINES.
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                     "note 768128
*   WRITE: /40 text-395.                                    "note 768128
      WRITE: 68 TEXT-395.                                     "note 768128
      WRITE: 199 SY-VLINE.                                    "note 768128

      LOOP AT CFOP_LINE_TOT WHERE CFOP_FIRST NE '9'.
        CASE CFOP_LINE_TOT-CFOP_FIRST.
          WHEN '1'.
            PERFORM DISPLAY_CFOP_SUMMARY_HEADER USING TEXT-396."nt 585040
          WHEN '2'.
            PERFORM DISPLAY_CFOP_SUMMARY_HEADER USING TEXT-397."nt 585040
          WHEN '3'.
            PERFORM DISPLAY_CFOP_SUMMARY_HEADER USING TEXT-398."nt 585040
        ENDCASE.
        LOOP AT CFOP_LINE.
          IF CFOP_LINE-CFOP_NOEXT(1) = CFOP_LINE_TOT-CFOP_FIRST.
            PERFORM CFOP_OUTPUT USING CFOP_LINE.
            DELETE CFOP_LINE INDEX SY-TABIX.
          ELSE.
            FORMAT INTENSIFIED COLOR COL_NORMAL.
            WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
*         WRITE:/24(17) text-399.                           "note 768128
            WRITE: 52(17) TEXT-399.                           "note 768128
            WRITE: 199 SY-VLINE.                              "note 768128
            CLEAR CFOP_LINE.
            MOVE-CORRESPONDING CFOP_LINE_TOT TO CFOP_LINE.
            PERFORM CFOP_OUTPUT USING CFOP_LINE.
            FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*          skip.                                            "note 768128
            EXIT.
          ENDIF.
        ENDLOOP.                         "cfop_line.

      ENDLOOP.                           "cfop_line_tot.
      FORMAT INTENSIFIED COLOR COL_NORMAL.
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                     "note 768128
*   WRITE:/24(17) text-399.                                 "note 768128
      WRITE: 52(17) TEXT-399.                                 "note 768128
      WRITE: 199 SY-VLINE.                                    "note 768128
      CLEAR CFOP_LINE.
      MOVE-CORRESPONDING CFOP_LINE_TOT TO CFOP_LINE.
      PERFORM CFOP_OUTPUT USING CFOP_LINE.
*    skip.                                                  "note 768128

      READ TABLE CFOP_LINE_TOT WITH KEY CFOP_FIRST = '9'.   "overall total
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                     "note 768128
*   WRITE:/24(17) text-400.                                 "note 768128
      WRITE: 52(17) TEXT-400.                                 "note 768128
      WRITE: 199 SY-VLINE.                                    "note 768128
      CLEAR CFOP_LINE.
      MOVE-CORRESPONDING CFOP_LINE_TOT TO CFOP_LINE.
      PERFORM CFOP_OUTPUT USING CFOP_LINE.
      FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*    skip.                                                  "note 768128
      ZULINE.                                                  "note 768128

*----------------------------------------------------------------------*
*--------------------write summaries per state-------------------------*
*----------------------------------------------------------------------*

*    skip 3.                                                "note 768128
      FORMAT INTENSIFIED COLOR COL_NORMAL.
      RESERVE 3 LINES.
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                     "note 768128
*   WRITE: /40 text-360.                                    "note 768128
      WRITE: 68 TEXT-360.                                     "note 768128
      WRITE: 199 SY-VLINE.                                    "note 768128
      FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
      SORT STATE_SUMMARY_ICMS.

      FORMAT INTENSIFIED COLOR COL_NORMAL.
      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
*       WRITE: AT /c_regio text-372,                        "note 768128
      WRITE: AT  C_REGIO TEXT-372,                        "note 768128
           AT  C_NFTOT TEXT-373,                          "note 768128
           AT  C_BASE  TEXT-374,                          "note 768128
           AT  C_TAXVA TEXT-375.                          "note 768128
      WRITE: 199 SY-VLINE.                                  "note 768128
*      SKIP.                                                "note 768128

      FORMAT INTENSIFIED OFF COLOR COL_NORMAL.

      LOOP AT STATE_SUMMARY_ICMS.

        CHECK ADDRESS1-REGION NE STATE_SUMMARY_ICMS-REGIO.

        AT NEW REGIO.
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                   "note 768128
*     WRITE: AT /c_regio(l_regio) state_summary_icms-regio. "note 768128
          WRITE: AT  C_REGIO(L_REGIO) STATE_SUMMARY_ICMS-REGIO. "note 768128
        ENDAT.

        IF STATE_SUMMARY_ICMS-CODIGO = '0'.
          WRITE AT C_NFTOT(L_NFTOT) STATE_SUMMARY_ICMS-NFTOT
                   CURRENCY T001-WAERS.
        ENDIF.

        IF STATE_SUMMARY_ICMS-CODIGO = '1' OR
           STATE_SUMMARY_ICMS-CODIGO = '3'.

          CASE STATE_SUMMARY_ICMS-TAXGRP.
            WHEN CONST_TAXGRP-ICMS.
              WRITE AT C_TAXGR TEXT-152.
            WHEN CONST_TAXGRP-SUBTRIB.
              WRITE AT C_TAXGR TEXT-154.
          ENDCASE.

          WRITE: AT C_CODIG(L_CODIG) STATE_SUMMARY_ICMS-CODIGO,
                 AT C_BASE(L_BASE)   STATE_SUMMARY_ICMS-BASE
                    CURRENCY T001-WAERS.

          IF STATE_SUMMARY_ICMS-TAXGRP = CONST_TAXGRP-SUBTRIB.
            WRITE: AT C_TAXVA(L_TAXVA) STATE_SUMMARY_ICMS-TAXVAL
                      CURRENCY T001-WAERS.
          ENDIF.
*       new-line.                                          "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                "note 768128
        ENDIF.
        WRITE: 199 SY-VLINE.                                 "note 768128
      ENDLOOP.
    ENDIF.
    ZULINE.                                                 "note 768128
    BOOKS = BOOKS + 1.
    LAST_TOTAL = 'X'.

  ENDIF.
*----------------------------------------------------------------------*
*--------Last page of Registro de Entradas (Statistics)----------------*
*----------------------------------------------------------------------*

  IF MOD_1 = 'X'.
    NEW-PAGE NO-TITLE.
*  skip 5.                                                  "note 768128
    ZULINE.
    WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
* WRITE: / text-300.                                        "note 768128
    WRITE:  TEXT-300.                                         "note 768128
    WRITE: 199 SY-VLINE.                                      "note 768128
*  SKIP 3.                                                  "note 768128
    WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
    WRITE:  TEXT-301, BOOKS.
    WRITE: 199 SY-VLINE.                                      "note 768128
*  SKIP 3.                                                  "note 768128
    WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
    WRITE: TEXT-302, PAGES.
    WRITE: 199 SY-VLINE.                                      "note 768128
*    SKIP.                                                  "note 768128
    ZULINE.
    PAGNO = 0.
  ENDIF.

*----------------------------------------------------------------------*
*------------Creation of Lista de Códigos de Emitentes-----------------*
*----------------------------------------------------------------------*

  IF MOD_10 ='X'.
    NEW-PAGE NO-TITLE.
    LOOP AT LIST.
      MODELO = '10'.
*   determine partner data
      CLEAR PARNAD.
      CALL FUNCTION 'J_1B_NF_PARTNER_READ'
        EXPORTING
          PARTNER_TYPE = LIST-PARTYP
          PARTNER_ID   = LIST-PARID
        IMPORTING
          PARNAD       = PARNAD
        EXCEPTIONS
          OTHERS       = 04.

      IF SY-SUBRC NE 0.
        CLEAR PARNAD.
      ENDIF.

* if the partner is not brazilian, the literal 'EX' is assigned to regio
      IF PARNAD-LAND1 NE ADDRESS1-COUNTRY.                   " note 841854
        MOVE CONST_EX TO PARNAD-REGIO.                       " note 841854
      ENDIF.                                                 " note 841854

      WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.
*   WRITE: /1 list-parid COLOR COL_KEY INTENSIFIED.         "note 768128
      WRITE: 30 LIST-PARID COLOR COL_KEY INTENSIFIED.          "note 768128
      FORMAT INTENSIFIED OFF COLOR COL_NORMAL.                "note 768128
      IF PARNAD-XCPDK = ' '.
*        WRITE: 28  parnad-name1.             " note 768128  note 835262
*              63  parnad-regio.                            "note 768128
        WRITE: 57  PARNAD-NAME1,                            "note 768128
               92  PARNAD-REGIO.                            "note 768128
        IF NOT PARNAD-CGC IS INITIAL.
*        WRITE: 76  parnad-cgc.               " note 768128  note 835262
          WRITE: 105  PARNAD-CGC.                              "note 768128
        ELSE.
*        WRITE: 76  parnad-cpf.               " note 768128  note 835262
          WRITE: 105  PARNAD-CPF.                              "note 768128
        ENDIF.
*        WRITE:   98 parnad-stains.           " note 768128  note 835262
        WRITE:  127 PARNAD-STAINS.                          "note 768128

      ELSE.                              "one time partner
        FIRST_RECORD = 'X'.
        LOOP AT OT_LIST WHERE PARID  = LIST-PARID
                          AND PARTYP = LIST-PARTYP.
          IF FIRST_RECORD = ' '.
*           NEW-LINE.                                       "note 768128
            WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                             "note 768128
*           WRITE: /1 list-parid COLOR COL_KEY INTENSIFIED. "note 768128
            WRITE: 30 LIST-PARID COLOR COL_KEY INTENSIFIED.  "note 768128
          ENDIF.
*        WRITE: 28  ot_list-name1,            " note 768128  note 835262
*                63  ot_list-regio.                         "note 768128
          WRITE: 57  OT_LIST-NAME1,            " note 768128  note 835262
                  92  OT_LIST-REGIO.                         "note 768128
          IF NOT OT_LIST-CGC IS INITIAL.
*          WRITE: 76  ot_list-cgc.            " note 768128  note 835262
            WRITE: 105 OT_LIST-CGC.                           "note 768128
          ELSE.
*          WRITE: 76  ot_list-cpf.            " note 768128  note 835262
            WRITE: 105 OT_LIST-CPF.                           "note 768128
          ENDIF.
*        write:   98  ot_list-stains.         " note 768128  note 835262
          WRITE:  127  OT_LIST-STAINS.                        "note 768128
          FIRST_RECORD = ' '.
        ENDLOOP.
      ENDIF.
**      WRITE: 199 SY-VLINE.                                   "note 768128
      WRITE:  56 SY-VLINE,
              91 SY-VLINE,
             104 SY-VLINE,
             126 SY-VLINE,
             199 SY-VLINE.
    ENDLOOP.
    ZULINE.                                                   "note 768128
  ENDIF.

*----------------------------------------------------------------------*
*---------------------------output errorlist---------------------------*
*----------------------------------------------------------------------*

  NEW-PAGE.

  ERRORLIST = 'X'.

  LOOP AT T_MESG.
    WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                     "note 768128
*   WRITE / t_mesg.                                         "note 768128
    WRITE: 30 T_MESG.                                       "note 768128
    WRITE: 199 SY-VLINE.                                    "note 768128
  ENDLOOP.
*----------------------------------------------------------------------*
*----------------------------Form  INI_OUTPUT -----------------------*
*----------------------------------------------------------------------*
*-------- Layout of heading: determination of starting position
*--------                    for each column
*----------------------------------------------------------------------*
FORM INI_OUTPUT.                                            "P45K005034
  C_POSTD = 30.
  C_SPECI = C_POSTD + 8.
  C_SERIE = C_SPECI + 3 + 1.                                "change +1
  C_SUBSE = C_SERIE + 3.
  C_NFNUM = C_SERIE + 6.
  C_DOCDA = C_NFNUM + 7.
  C_PARID = C_DOCDA + 9.
  C_TOTAL = C_PARID + 5.               "position of total
  C_DECEN = C_TOTAL.                   "position of decendio line
  C_REGIO = C_PARID + 11.
  L_REGIO = 3.                         "old:2, change +1
  C_NFTOT = C_REGIO + L_REGIO + 1.
  L_NFTOT = 19. "old:15                      "old:15, change +4
  C_EMPTY = C_NFTOT + L_NFTOT + 1.
  L_EMPTY = 3.                         "new empty column, change +3 +|
  C_CFOPN = C_EMPTY + L_EMPTY + 1.
  L_CFOPN = 7.                         "old: 3, change +4
  C_TAXGR = C_CFOPN + L_CFOPN + 1.
  C_CODIG = C_TAXGR + 5.
  L_CODIG = 1.
  C_BASE  = C_CODIG + L_CODIG + 1.
  L_BASE  = 18.                        "old: 14, change +4
  C_RATE  = C_BASE +  L_BASE  + 1.
  L_RATE  = 6.
  C_TAXVA = C_RATE +  L_RATE  + 1.
  L_TAXVA = 19.                        "old: 14, change +5
  C_OBSER = C_TAXVA + L_TAXVA + 1.
  ADD 1 TO C_SPECI. ADD 1 TO C_SERIE. ADD 1 TO C_SUBSE.     "note 768128
  ADD 1 TO C_NFNUM. ADD 1 TO C_DOCDA.                       "note 768128
  ADD 1 TO C_PARID. ADD 1 TO C_REGIO.                       "note 768128
  ADD 1 TO C_NFTOT.                                         "note 768128
  ADD 1 TO C_CFOPN.                                         "note 768128
  ADD 1 TO C_TAXGR.                                         "note 768128
  ADD 1 TO C_CODIG. ADD 1 TO C_BASE.                        "note 768128
  ADD 1 TO C_RATE.                                          "note 768128
  ADD 1 TO C_TAXVA.                                         "note 768128
  ADD 1 TO C_OBSER.                                         "note 768128

ENDFORM.                    "ini_output
*----------------------------------------------------------------------*
*----------------------------Form  OUTPUT------------------------------*
*----------------------------------------------------------------------*
*----------------performs output of lineelements-----------------------*
*----------------------------------------------------------------------*
*   -> out: single outputline
*  <-   output
*----------------------------------------------------------------------*

FORM OUTPUT USING FORMOUT STRUCTURE OUT.
*M001 - BEGIN
*M001 - END
  DATA: REF_NUMBER LIKE J_1BINTERF-XBLNR.  "condensed NF number
  "(Number-SeriesSubseries)
  DATA: OBSERVATION(255), STRING(30),
        SEPARATOR(1) VALUE ' ',        "for observations
        DATUM LIKE SY-DATUM.

  WA_POS-C_POSTD  = C_POSTD  - 1 .
  WA_POS-C_SPECI  = C_SPECI  - 1 .
  WA_POS-C_SUBSE  = C_SUBSE  - 1 .
  WA_POS-C_SERIE  = C_SERIE  - 1 .
  WA_POS-C_NFNUM  = C_NFNUM  - 1 .
  WA_POS-C_DOCDA  = C_DOCDA  - 1 .
  WA_POS-C_PARID  = C_PARID  - 1 .
  WA_POS-C_REGIO  = C_REGIO  - 1 .
  WA_POS-L_REGIO  = L_REGIO  - 1 .
  WA_POS-C_NFTOT  = C_NFTOT  - 1 .
  WA_POS-L_NFTOT  = L_NFTOT  - 1 .
  WA_POS-C_EMPTY  = C_EMPTY  - 1 .
  WA_POS-L_EMPTY  = L_EMPTY  - 1 .
  WA_POS-C_CFOPN  = C_CFOPN  - 1 .
  WA_POS-L_CFOPN  = L_CFOPN  - 1 .
  WA_POS-C_CFOPX  = C_CFOPX  - 1 .
  WA_POS-C_TAXGR  = C_TAXGR  - 1 .
  WA_POS-C_CODIG  = C_CODIG  - 1 .
  WA_POS-L_CODIG  = L_CODIG  - 1 .
  WA_POS-C_BASE   = C_BASE   - 1 .
  WA_POS-L_BASE   = L_BASE   - 1 .
  WA_POS-C_RATE   = C_RATE   - 1 .
  WA_POS-L_RATE   = L_RATE   - 1 .
  WA_POS-C_TAXVA  = C_TAXVA  - 1 .
  WA_POS-L_TAXVA  = L_TAXVA  - 1 .
  WA_POS-C_OBSER  = C_OBSER  - 1 .
  WA_POS-C_TOTAL  = C_TOTAL  - 1 .
  WA_POS-C_DECEN  = C_DECEN  - 1 .
  WA_POS-ULTIMO   = C_DOCDA + 8.

  FORMAT INTENSIFIED COLOR COL_KEY.
  IF FORMOUT-DUPL =' '.
    WRITE:  /30      FORMOUT-PSTDAT DD/MM/YY,    "P45K005034 "note 768128
     AT C_SPECI   FORMOUT-SPECIES,              "old: 10    "note 768128
     AT C_SERIE   FORMOUT-SERIES,               "old: 13    "note 768128
     AT C_SUBSE   FORMOUT-SUBSER,               "old: 16    "note 768128
     AT C_NFNUM   FORMOUT-NFNUM,                "old: 19    "note 768128
     AT C_DOCDA   FORMOUT-DOCDAT DD/MM/YY.      "old: 26    "note 768128
  ELSE.
    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
    WRITE: /29 SY-VLINE.                                    "note 768128
  ENDIF.
*M001 - END
  IF FORMOUT-CANCEL = ' '.
    IF FORMOUT-DUPL =' '.
      WRITE: AT C_PARID FORMOUT-PARID,
             AT C_REGIO FORMOUT-REGIO.
    ENDIF.
**********************************************************************
    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.

    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE          "note 553750
         WITH KEY ITMTYP = FORMOUT-ITMTYP.             "note 553750
* write valor contabil only once
    IF SY-SUBRC IS INITIAL AND                             " note 782912
     FORMOUT-TAXGRP = CONST_TAXGRP-ICMS AND                " note 782912
*    if formout-taxgrp = const_taxgrp-icms and             " note 782912
                       FORMOUT-NFTOT > 0  AND
                       GS_NFITMRULE-IGNORETOTAL <> 'X'.  "note 553750
*                   formout-itmtyp ne '41' and           "note 553750
*                   formout-itmtyp ne '43' and           "note 553750
*                   FORMOUT-ITMTYP NE '51' AND " note 450610   553750
*                   formout-itmtyp ne '63' and           "note 553750
*                   formout-itmtyp ne '64'.              "note 553750

      WRITE: AT C_NFTOT(L_NFTOT) FORMOUT-NFTOT CURRENCY T001-WAERS.
      TOTAL-VALORCONT = TOTAL-VALORCONT + FORMOUT-NFTOT.
      CLEAR STATE_SUMMARY_ICMS.
      MOVE '0' TO STATE_SUMMARY_ICMS-CODIGO.
      MOVE OUT-REGIO TO STATE_SUMMARY_ICMS-REGIO.
      MOVE FORMOUT-NFTOT TO STATE_SUMMARY_ICMS-NFTOT.
      COLLECT STATE_SUMMARY_ICMS.
    ENDIF.

    SHIFT FORMOUT-CFOP_NOEXT LEFT DELETING LEADING '0'. " beg. of 553750
    CASE CFOP_LENGTH.
      WHEN 3.
        WRITE: AT C_CFOPN(L_CFOPN) FORMOUT-CFOP_NOEXT.
      WHEN 4.
        WRITE: AT C_CFOPN(L_CFOPN) FORMOUT-CFOP_NOEXT(1).
        C_CFOPX = C_CFOPN + 1.
        WRITE: AT C_CFOPX '.' NO-GAP, FORMOUT-CFOP_NOEXT+1(3).
    ENDCASE.
* print the CFOP extension for SP and SC
    IF NOT FORMOUT-CFOP_EXT IS INITIAL.
      CASE EXT_LEN.
        WHEN 1.
          C_CFOPX = C_CFOPN + CFOP_LENGTH.
          WRITE: AT C_CFOPX '.' NO-GAP, FORMOUT-CFOP_EXT+1(1).
        WHEN 2.
          C_CFOPX = C_CFOPN + CFOP_LENGTH.
          WRITE: AT C_CFOPX '.' NO-GAP, FORMOUT-CFOP_EXT(2).
      ENDCASE.
    ENDIF.                                         " end of note 553750

    CASE FORMOUT-TAXGRP.
      WHEN CONST_TAXGRP-ICMS.
        WRITE: AT C_TAXGR TEXT-152.
      WHEN CONST_TAXGRP-IPI.
        WRITE: AT C_TAXGR TEXT-153.
      WHEN CONST_TAXGRP-SUBTRIB.
        WRITE: AT C_TAXGR TEXT-154.
    ENDCASE.

    IF FORMOUT-STATTX = ' ' .
      WRITE: AT C_CODIG      FORMOUT-CODIGO.
*      note 210543
*      if formout-rectype is initial.   "IPI Pauta
      WRITE: AT C_BASE(L_BASE)  FORMOUT-BASE  CURRENCY T001-WAERS.
      WRITE AT 199 SY-VLINE.
*      endif.
* no output in other base case
* no output in exempt base case
      IF FORMOUT-CODIGO = '1'.
*      never print rate for IPI
        IF FORMOUT-TAXGRP NE CONST_TAXGRP-IPI.
          WRITE: AT C_RATE(L_RATE)   FORMOUT-RATE.
          WRITE AT 199 SY-VLINE.
        ENDIF.
        WRITE: AT C_TAXVA(L_TAXVA) FORMOUT-TAXVAL CURRENCY T001-WAERS.
        WRITE AT 199 SY-VLINE.
      ENDIF.
    ENDIF.
  ENDIF.

  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.

  IF NOT FORMOUT-FREE_TEXT IS INITIAL
* START OF NOTE 386073, A
     AND FORMOUT-TAXGRP NE 'ICOP'
* END OF NOTE 386073, A
* START OF NOTE 386073, B
     AND FORMOUT-TAXGRP NE 'ICST'.
* END OF NOTE 386073, B
    IF FORMOUT-DUPL = ' '.
      OBSERVATION = FORMOUT-FREE_TEXT.
      PERFORM OUTPUT_OBSERVA USING OBSERVATION.
    ENDIF.
    CLEAR FORMOUT.
* START OF NOTE 386073, A
    CLEAR OBSERVATION.
* END OF NOTE 386073, A
    EXIT.
  ENDIF.

  IF FORMOUT-DUPL = ' ' AND NOT FORMOUT-OBS_DOC IS INITIAL.
    CONCATENATE OBSERVATION FORMOUT-OBS_DOC INTO OBSERVATION.
  ENDIF.

  IF FORMOUT-OBS_DOC IS INITIAL AND NOT FORMOUT-OBS_LIN IS INITIAL.
    IF OBSERVATION IS INITIAL.
      CONCATENATE OBSERVATION FORMOUT-OBS_LIN INTO OBSERVATION.
    ELSE.
      CONCATENATE OBSERVATION FORMOUT-OBS_LIN
             INTO OBSERVATION
             SEPARATED BY SEPARATOR.
    ENDIF.
  ENDIF.

  IF NOT FORMOUT-OBS_REF IS INITIAL AND NOT
     ( FORMOUT-OBS_LIN IS INITIAL AND FORMOUT-OBS_DOC IS INITIAL ).
    SELECT SINGLE * FROM J_1BNFDOC WHERE DOCNUM = FORMOUT-OBS_REF.
    CLEAR REF_NUMBER.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
        EXPORTING
          NF_NUMBER  = J_1BNFDOC-NFNUM
          SERIES     = J_1BNFDOC-SERIES
        IMPORTING
          REF_NUMBER = REF_NUMBER
        EXCEPTIONS
          OTHERS     = 1.
      IF SY-SUBRC = 0.
        DATUM = J_1BNFDOC-PSTDAT.
        CLEAR STRING.
        WRITE DATUM TO STRING.
        IF OBSERVATION IS INITIAL.
*          concatenate ref_number(9) string                " note 641546
          CONCATENATE REF_NUMBER(10) STRING                " note 641546
                 INTO OBSERVATION
                 SEPARATED BY SEPARATOR.
        ELSE.
*          concatenate observation ref_number(9) string    " note 641546
          CONCATENATE OBSERVATION REF_NUMBER(10) STRING    " note 641546
                 INTO OBSERVATION
                 SEPARATED BY SEPARATOR.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* IPI Pauta related observations
  IF FORMOUT-CODIGO = '3' AND FORMOUT-RECTYPE = '1'.

    WRITE FORMOUT-OBS_IPIPAUTA CURRENCY T001-WAERS TO STRING.
    SHIFT STRING LEFT DELETING LEADING SPACE.
    IF OBSERVATION IS INITIAL.
      CONCATENATE TEXT-394 STRING
                  INTO OBSERVATION
                  SEPARATED BY SEPARATOR.
    ELSE.
      CONCATENATE OBSERVATION TEXT-394 STRING
             INTO OBSERVATION
             SEPARATED BY SEPARATOR.
    ENDIF.
  ENDIF.

* ICMS frght/ST related observations
  IF FORMOUT-DUPL = ' ' AND
    ( FORMOUT-OBS_ICFRTAX NE 0 OR FORMOUT-OBS_ICFRBASE NE 0 ).
    IF NOT FORMOUT-OBS_DOC IS INITIAL OR NOT FORMOUT-OBS_LIN IS INITIAL.
      NEW-LINE.                                            "note 768128
      WRITE: /29 SY-VLINE.                                 "note 768128
    ENDIF.
    WRITE FORMOUT-OBS_ICFRTAX CURRENCY T001-WAERS TO STRING.
    SHIFT STRING LEFT DELETING LEADING SPACE.
    "Valor ICMS Sub. trib. frete
    IF OBSERVATION IS INITIAL.
      CONCATENATE TEXT-391 STRING
            INTO  OBSERVATION
            SEPARATED BY SEPARATOR.
    ELSE.
      CONCATENATE OBSERVATION TEXT-391 STRING
             INTO OBSERVATION
             SEPARATED BY SEPARATOR.
    ENDIF.
  ENDIF.
  IF FORMOUT-OBS_ICFRBASE NE 0.

    WRITE FORMOUT-OBS_ICFRBASE CURRENCY T001-WAERS TO STRING.
    SHIFT STRING LEFT DELETING LEADING SPACE.
    "Base de cálculo ICMS Sub. trib. frete
    IF OBSERVATION IS INITIAL.
      CONCATENATE TEXT-392 STRING
             INTO OBSERVATION
             SEPARATED BY SEPARATOR.
    ELSE.
      CONCATENATE OBSERVATION TEXT-392 STRING
             INTO OBSERVATION
             SEPARATED BY SEPARATOR.
    ENDIF.
  ENDIF.
* START OF NOTE 386073, A
  IF FORMOUT-TAXGRP = 'ICOP'.
    "clear the values already printed
    WRITE AT C_CFOPN '                                                '.
    WRITE AT C_TAXVA '                                               '.

    "no base amount, only tax value
    WRITE FORMOUT-TAXVAL CURRENCY T001-WAERS TO STRING.
    WRITE: 199 SY-VLINE.                                "note 768128
    SHIFT STRING LEFT DELETING LEADING SPACE.
    CONCATENATE OBSERVATION TEXT-272 STRING INTO OBSERVATION.
  ENDIF.
* END OF NOTE 386073, A

* START OF NOTE 386073, B
  IF FORMOUT-ST_CONSUMO = 'X'.
    "no base amount, only tax value
    WRITE FORMOUT-TAXVAL CURRENCY T001-WAERS TO STRING.
    SHIFT STRING LEFT DELETING LEADING SPACE.
    CONCATENATE OBSERVATION TEXT-273 STRING INTO OBSERVATION.
    WRITE: 199 SY-VLINE.                                "note 768128
  ENDIF.
* END OF NOTE 386073, B

  CLEAR FORMOUT.

  PERFORM OUTPUT_OBSERVA USING OBSERVATION.
  WRITE: 199 SY-VLINE.                                      "note 768128

  WRITE:  38 SY-VLINE,
          42 SY-VLINE,
          48 SY-VLINE,
          55 SY-VLINE,
          64 SY-VLINE,
          75 SY-VLINE,
          79 SY-VLINE,
          99 SY-VLINE,
         103 SY-VLINE,
         111 SY-VLINE,
         116 SY-VLINE,
         118 SY-VLINE,
         137 SY-VLINE,
         144 SY-VLINE,
         164 SY-VLINE.
ENDFORM.                               " OUTPUT

*----------------------------------------------------------------------*
*--------------------------Form  TOTAL_OUTPUT--------------------------*
*----------------------------------------------------------------------*
*----------------performs output of totallines-------------------------*
*----------------------------------------------------------------------*
*  --> total: totals per day
*  <-- output
*----------------------------------------------------------------------*

FORM TOTAL_OUTPUT USING FORMTOTAL STRUCTURE TOTAL.
  RESERVE 8 LINES.
  FORMAT INTENSIFIED COLOR COL_TOTAL.
*  skip.                                                    "note 768128
* P45K005034                                                "note 768128
  ZULINE.                                                    "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
* WRITE:  AT /c_total text-151,        "TOTAL               "note 768128
  WRITE: AT  C_TOTAL TEXT-151,        "TOTAL                "note 768128
         AT  C_TAXGR TEXT-152,        "ICMS                 "note 768128

         AT  C_NFTOT(L_NFTOT) FORMTOTAL-VALORCONT           "note 768128
             CURRENCY T001-WAERS,                           "note 768128
         AT  C_CODIG      '1',                              "note 768128
         AT  C_BASE(L_BASE) FORMTOTAL-ICMSBASE1             "note 768128
             CURRENCY T001-WAERS,                           "note 768128
         AT  C_TAXVA(L_TAXVA) FORMTOTAL-ICMSVAL1            "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
*        AT /c_codig      '2',                              "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
  WRITE: AT  C_CODIG      '2',                              "note 768128
         AT  C_BASE(L_BASE) FORMTOTAL-ICMSBASE2             "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
*        AT /c_codig      '3',                              "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
  WRITE: AT  C_CODIG      '3',                              "note 768128
         AT  C_BASE(L_BASE) FORMTOTAL-ICMSBASE3             "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
* write: AT /c_total       text-151,                        "note 768128
  WRITE: AT  C_TOTAL       TEXT-151,                        "note 768128
         AT  C_TAXGR       TEXT-153,                        "note 768128
         AT  C_CODIG      '1',                              "note 768128
         AT  C_BASE(L_BASE) FORMTOTAL-IPIBASE1              "note 768128
             CURRENCY T001-WAERS,                           "note 768128
         AT  C_TAXVA(L_TAXVA) FORMTOTAL-IPIVAL1             "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
*        AT  /c_codig      '2',                             "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
  WRITE: AT  C_CODIG      '2',                              "note 768128
         AT  C_BASE(L_BASE)  FORMTOTAL-IPIBASE2             "note 768128
              CURRENCY T001-WAERS.                          "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
*        AT  /c_codig      '3',                             "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
  WRITE: AT  C_CODIG      '3',                              "note 768128
         AT  C_BASE(L_BASE)  FORMTOTAL-IPIBASE3             "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
*        AT /c_total     text-151,                          "note 768128
  WRITE: AT  C_TOTAL     TEXT-151,                          "note 768128
         AT  C_TAXGR     TEXT-154,                          "note 768128
         AT  C_CODIG      '1',                              "note 768128
         AT  C_BASE(L_BASE)  FORMTOTAL-SUBTRIBBASE1         "note 768128
             CURRENCY T001-WAERS,                           "note 768128
         AT  C_TAXVA(L_TAXVA) FORMTOTAL-SUBTRIBVAL1         "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
*        AT /c_codig      '3',                              "note 768128
  WRITE: AT  C_CODIG      '3',                              "note 768128
         AT  C_BASE(L_BASE)  FORMTOTAL-SUBTRIBBASE3         "note 768128
             CURRENCY T001-WAERS.                           "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
* SKIP.                                                     "note 768128
  ZULINE.                                                    "note 768128
  CLEAR FORMTOTAL.                                          "note 768128

ENDFORM.                               " TOTAL_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CFOP_OUTPUT
*&---------------------------------------------------------------------*
*   outout of CFOP totals                                              *
*----------------------------------------------------------------------*

FORM CFOP_OUTPUT USING LINE STRUCTURE CFOP_LINE.

  SHIFT LINE-CFOP_NOEXT LEFT DELETING LEADING '0'.   " begin note 553750
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                      "note 768128
*  WRITE:    AT /c_nftot(l_nftot) line-valorcont            "note 768128
*                 CURRENCY t001-waers.                      "note 768128
  WRITE:    AT  C_NFTOT(L_NFTOT) LINE-VALORCONT             "note 768128
                 CURRENCY T001-WAERS.                       "note 768128
  IF NOT LINE-CFOP_NOEXT IS INITIAL.
    CASE CFOP_LENGTH.
      WHEN 3.
        WRITE:    AT C_CFOPN(L_CFOPN)    LINE-CFOP_NOEXT.
      WHEN 4.
        WRITE:    AT C_CFOPN(L_CFOPN)    LINE-CFOP_NOEXT(1).
        C_CFOPX = C_CFOPN + 1.
        WRITE: AT C_CFOPX '.' NO-GAP, LINE-CFOP_NOEXT+1(3).
    ENDCASE.
  ENDIF.
* print the CFOP extension for SP and SC
  IF NOT LINE-CFOP_EXT IS INITIAL.                        " note 585040
*  IF LINE-CFOP_EXT <> '00'.                              " note 585040
    CASE EXT_LEN.
      WHEN 1.
        IF NOT LINE-CFOP_EXT+1(1) = '0'.
          WRITE: AT C_CFOPX '.' NO-GAP, LINE-CFOP_EXT+1(1).
        ENDIF.
      WHEN 2.
        WRITE: AT C_CFOPX '.' NO-GAP, LINE-CFOP_EXT.
    ENDCASE.
  ENDIF.                                            " end of note 553750

  WRITE:    AT C_CODIG     '1',
            AT C_BASE(L_BASE)      LINE-ICMSBASE1
               CURRENCY T001-WAERS,
            AT C_TAXVA(L_TAXVA)    LINE-ICMSVAL1
               CURRENCY T001-WAERS.
  WRITE: 199 SY-VLINE.                                      "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
*          AT /c_codig     '2',                             "note 768128
  WRITE:    AT C_CODIG     '2',                             "note 768128
            AT C_BASE(L_BASE)      LINE-ICMSBASE2           "note 768128
                CURRENCY T001-WAERS.                        "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
*          AT /c_codig     '3',                             "note 768128
  WRITE:    AT C_CODIG     '3',                             "note 768128
            AT C_BASE(L_BASE)      LINE-ICMSBASE3           "note 768128
                CURRENCY T001-WAERS.                        "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128

ENDFORM.                               " CFOP_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_OBSERVA
*&---------------------------------------------------------------------*
*       Output of observation column
*       If there is too much info for one line
*       the text will be displayed in a second line
*----------------------------------------------------------------------*
*  -->  p1        observation: info to be displayed
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTPUT_OBSERVA USING STRING.
  DATA: AVAIL TYPE I, AVAIL2 TYPE I,
        SEPARATOR TYPE C VALUE ' ',    "split line at last separator
        CHARACTER,
        LENGTH TYPE I,
        PART1 TYPE I,
        LINE(255), REST(255),
        BLANKS(255) VALUE ' '.
  DATA: BLANKSTART TYPE I, FILLER TYPE I. "fill line to the end
  AVAIL = SY-LINSZ - C_OBSER + 1.      "available length for observation
  LINE = STRING.
  LENGTH = STRLEN( LINE ).
  IF LENGTH > AVAIL.                   "more than one line
    PART1 = AVAIL.
    AVAIL2 = AVAIL / 2.

* search last separator in line
    DO.
      CHARACTER = LINE+PART1(1).
      IF CHARACTER = SEPARATOR.
        EXIT.
      ELSEIF PART1 LE AVAIL2.          " in case wrong separator
        PART1 = AVAIL.
        EXIT.
      ENDIF.
      PART1 = PART1 - 1.
    ENDDO.

    WRITE: AT C_OBSER(PART1) LINE.      "write first line
    WRITE AT 199 SY-VLINE.

    FILLER = AVAIL - PART1.
    BLANKSTART = C_OBSER + PART1.
    IF FILLER <> 0.
      WRITE AT BLANKSTART(FILLER) BLANKS.
      WRITE AT 199 SY-VLINE.
    ENDIF.
    NEW-LINE.
    LENGTH = LENGTH - PART1.
    IF LENGTH <> 0.                      "should always be true
      REST = LINE+PART1(LENGTH).         "remaining observation
      PERFORM OUTPUT_OBSERVA USING REST. "same procedure
    ENDIF.
  ELSE.
    WRITE: AT 1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.
    WRITE: AT C_OBSER(AVAIL) LINE.
    WRITE AT 199 SY-VLINE.
  ENDIF.
ENDFORM.                                 " OUTPUT_OBSERVA

*----------------------------------------------------------------------*
*---------------------------top-of-page--------------------------------*
*----------------------------------------------------------------------*

TOP-OF-PAGE.
  FORMAT RESET.
  SKIP 5.
  IF ERRORLIST = ' '.
    PAGNO = PAGNO + 1.
    FORMAT INTENSIFIED COLOR COL_HEADING.

    CASE MODELO.
      WHEN '1'.

*  check if last total ( = end of the Registro de Entradas) was reached
        IF LAST_TOTAL = ' '.
*     check maximum pages
          IF PAGNO > BOOKSIZE.
            BOOKS = BOOKS + 1.
            PAGNO = 2.
          ENDIF.
          PAGES = PAGES + 1.
*     header for Modelo 1 (Registro de Entradas)
          ZULINE.                                             "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                "note 768128
*        write: /1   sy-datum,                              "note 768128
*                 at 50(sy-linsz) text-200.                 "note 768128
          WRITE: "30   SY-DATUM,                               "note 768128
                  AT 78(SY-LINSZ) TEXT-200.                  "note 768128
          WRITE: 199 SY-VLINE.                               "note 768128
          ZULINE.

          FORMAT INTENSIFIED OFF COLOR COL_GROUP.           "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
*         write at /93(sy-linsz) text-201.                  "note 768128
          WRITE AT 121(SY-LINSZ) TEXT-201 INTENSIFIED  COLOR COL_HEADING.
          "note 768128
          WRITE: 199 SY-VLINE.                              "note 768128

*         write: /1  text-202,                              "note 768128
*                 14  branch_data-name,                     "note 768128
*                 at 93(sy-linsz)  text-203.                "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
          CONCATENATE ADDRESS1-NAME1 '-' BRANCH_DATA-NAME
                 INTO WG_FIRMA SEPARATED BY SPACE.
          CONDENSE WG_FIRMA.
          WRITE: 30  TEXT-202 INTENSIFIED  COLOR COL_GROUP,  "note 768128
                 "note 768128
                 42  WG_FIRMA INTENSIFIED OFF COLOR COL_GROUP,
               "note 768128
               AT 121(SY-LINSZ)  TEXT-203 INTENSIFIED  COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128
*          write: /1  text-204,                             "note 768128
*                 14  branch_data-state_insc,               "note 768128
*                 40  text-205,                             "note 768128
*                 60  cgc_number,                           "note 768128
*                 at 93(sy-linsz) text-206.                 "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
          WRITE: 30  TEXT-204 INTENSIFIED  COLOR COL_GROUP,  "note 768128
             "note 768128
             42  BRANCH_DATA-STATE_INSC INTENSIFIED OFF COLOR COL_GROUP,
             "note 768128
             68  TEXT-205 INTENSIFIED  COLOR COL_GROUP,     "note 768128
             88  CGC_NUMBER INTENSIFIED OFF COLOR COL_GROUP,"note 768128
             "note 768128
             AT 121(SY-LINSZ) TEXT-206 INTENSIFIED  COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128

          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
*         write at /93(sy-linsz)  text-207.                 "note 768128
          "note 768128
          WRITE AT 121(SY-LINSZ) TEXT-207 INTENSIFIED  COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128

*          write: /1  text-208,                             "note 768128
*                 14  pagno,                                "note 768128
*                 40  text-209,                             "note 768128
*                 60  j5_pdate-low, '-', j5_pdate-high,     "note 768128
*                 at 93(sy-linsz)  text-210.                "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
          WRITE: 30 TEXT-208 INTENSIFIED  COLOR COL_GROUP,  "note 768128
            42  PAGNO INTENSIFIED OFF COLOR COL_GROUP,      "note 768128
            68  TEXT-209 INTENSIFIED  COLOR COL_GROUP,      "note 768128
            "note 768128
            88  J5_PDATE-LOW INTENSIFIED OFF COLOR COL_GROUP, '-',
                "note 768128
                J5_PDATE-HIGH INTENSIFIED OFF COLOR COL_GROUP,
            "note 768128
            AT 121(SY-LINSZ)  TEXT-210 INTENSIFIED  COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128

          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                               "note 768128
*          write at /93(sy-linsz)  text-211.                "note 768128
          "note 768128
          WRITE AT 121(SY-LINSZ) TEXT-211 INTENSIFIED  COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128
          ZULINE.

* P45K005034
*         CONCATENATE text-221 text-231 INTO concatext.     "note 768128
*         WRITE / concatext . CLEAR concatext.              "note 768128
          "note 768128
          CONCATENATE SY-VLINE TEXT-221 TEXT-231 INTO CONCATEXT.
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 CONCATEXT INTENSIFIED  COLOR COL_HEADING. "note 768128
          WRITE: 199 SY-VLINE.                              "note 768128
          CLEAR CONCATEXT.
*         CONCATENATE text-222 text-232 INTO concatext.     "note 768128
*         WRITE / concatext. CLEAR concatext.               "note 768128
          "note 768128
          CONCATENATE SY-VLINE TEXT-222 TEXT-232 INTO CONCATEXT.
          "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 CONCATEXT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128
          CLEAR CONCATEXT.
*         CONCATENATE text-223 text-233 INTO concatext.     "note 768128
*         WRITE / concatext. CLEAR concatext.               "note 768128
          "note 768128
          CONCATENATE SY-VLINE TEXT-223 TEXT-233 INTO CONCATEXT.
          "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 CONCATEXT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128
          CLEAR CONCATEXT.
*         CONCATENATE text-224 text-234 INTO concatext.     "note 768128
*         WRITE / concatext. CLEAR concatext.               "note 768128
          "note 768128
          CONCATENATE SY-VLINE TEXT-224 TEXT-234 INTO CONCATEXT.
          "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 CONCATEXT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128
          CLEAR CONCATEXT.
*          CONCATENATE text-225 text-235 INTO concatext.    "note 768128
*          WRITE / concatext. CLEAR concatext.              "note 768128
          "note 768128
          CONCATENATE SY-VLINE TEXT-225 TEXT-235 INTO CONCATEXT.
          "note 768128
          WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 CONCATEXT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: 199 SY-VLINE.                              "note 768128
          CLEAR CONCATEXT.
          ZULINE.
        ENDIF.
      WHEN '10'.
*     header for Modelo 10 (Lista de Códigos de Emitentes)
        ZULINE.                                              "note 768128
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
*       WRITE: /1   sy-datum,                               "note 768128
*              50  text-250 .                               "note 768128
        WRITE: 30  SY-DATUM,                                "note 768128
               88  TEXT-250 .                               "note 768128
        WRITE AT SY-LINSZ ' '.           "intensified until end of line
        WRITE: 199 SY-VLINE.                                "note 768128
        ZULINE.
        FORMAT INTENSIFIED OFF COLOR COL_GROUP.             "note 768128
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
*        WRITE: /1  text-202,                               "note 768128
*               14  branch_data-name.                       "note 768128
        WRITE: 30 TEXT-202 INTENSIFIED COLOR COL_GROUP,     "note 768128
               "note 768128
               42  BRANCH_DATA-NAME INTENSIFIED OFF COLOR COL_GROUP.
        WRITE AT SY-LINSZ ' '.

        WRITE: 199 SY-VLINE.                                "note 768128
*        WRITE: /1  text-204,                               "note 768128
*               14  branch_data-state_insc,                 "note 768128
*               60  text-205,                               "note 768128
*               80  cgc_number.                             "note 768128
*        WRITE AT sy-linsz ' '.                             "note 768128
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
        WRITE: 30 TEXT-204 INTENSIFIED COLOR COL_GROUP,     "note 768128
            "note 768128
            42  BRANCH_DATA-STATE_INSC INTENSIFIED OFF COLOR COL_GROUP,
            88  TEXT-205 INTENSIFIED COLOR COL_GROUP,       "note 768128
            108  CGC_NUMBER INTENSIFIED OFF COLOR COL_GROUP. "note 768128
        WRITE AT SY-LINSZ ' '.                              "note 768128
        WRITE: 199 SY-VLINE.                                "note 768128


        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                 "note 768128
        WRITE: 30 TEXT-208 INTENSIFIED COLOR COL_GROUP,     "note 768128
          42  PAGNO INTENSIFIED OFF COLOR COL_GROUP,        "note 768128
          88  TEXT-209 INTENSIFIED COLOR COL_GROUP,         "note 768128
         "note 768128
          108  J5_PDATE-LOW INTENSIFIED OFF COLOR COL_GROUP, '-',
              J5_PDATE-HIGH INTENSIFIED OFF COLOR COL_GROUP."note 768128
        WRITE AT SY-LINSZ ' '.                              "note 768128
        WRITE: 199 SY-VLINE.    "p                          "note 768128

        ZULINE.
*        WRITE AT /(sy-linsz) text-251.                     "note 768128
        FORMAT INTENSIFIED COLOR COL_HEADING.               "note 768128
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE,30 TEXT-251.                      "note 768128
        WRITE: 199 SY-VLINE.                                "note 768128
*       WRITE AT /(sy-linsz) text-252.                      "note 768128
        WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE,30 TEXT-252.                      "note 768128
        WRITE: 199 SY-VLINE.                                "note 768128
        ZULINE.

    ENDCASE.
  ELSE.
    FORMAT INTENSIFIED OFF COLOR COL_HEADING.               "note 768128
    ZULINE.                                                  "note 768128
    WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                     "note 768128
*   WRITE: AT /50(sy-linsz) text-350.                       "note 768128
    WRITE: AT 78(SY-LINSZ) TEXT-350.                        "note 768128
    WRITE: 199 SY-VLINE.                                    "note 768128
    ZULINE.                                                  "note 768128
  ENDIF.

END-OF-PAGE.
  ZULINE.

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_REGION_CONHECIMENTO
*&---------------------------------------------------------------------*
FORM DETERMINE_REGION_CONHECIMENTO.                         "note 216289
  DATA: WA_J1BNFDOC LIKE J_1BNFDOC.
  DATA: WA_PARNAD LIKE PARNAD.

  WA_J1BNFDOC = J_1BNFDOC.
  WA_PARNAD   = PARNAD.

* note 506564: after implementation of note 381607, it was possible to
* take the "ship from" from the carrier (and not from the vendor) for
* conhecimentos (triggered by J_1BAA-SHIPFROM = '1')
  READ TABLE INTJ_1BAA WITH KEY NFTYPE   = J_1BNFDOC-NFTYPE
                                                 BINARY SEARCH.
  CHECK INTJ_1BAA-SHIPFROM <> '1'.

*  for conhecimento: region of vendor must be reported,
*  NOT the region of the carrier

* read partner information from original nota fiscal
  IF NOT J_1BNFDOC-DOCREF IS INITIAL.
    SELECT SINGLE * FROM J_1BNFDOC INTO WA_J1BNFDOC
                            WHERE DOCNUM = J_1BNFDOC-DOCREF.
    IF SY-SUBRC NE 0.
*     if original nf cannot be found, use partner of current nf
      WA_J1BNFDOC = J_1BNFDOC.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      PARTNER_TYPE     = WA_J1BNFDOC-PARTYP
      PARTNER_ID       = WA_J1BNFDOC-PARID
      DOC_NUMBER       = WA_J1BNFDOC-DOCNUM
      PARTNER_FUNCTION = WA_J1BNFDOC-PARVW
    IMPORTING
      PARNAD           = WA_PARNAD
    EXCEPTIONS
      OTHERS           = 04.

  IF SY-SUBRC NE 0.
    CLEAR PARNAD.
  ELSE.
    PARNAD-REGIO = WA_PARNAD-REGIO.

* special condition, if partner is not Brazilian: region = EX
    IF WA_PARNAD-LAND1 NE ADDRESS1-COUNTRY.
      MOVE CONST_EX TO PARNAD-REGIO.
    ENDIF.
  ENDIF.
ENDFORM.                    " DETERMINE_REGION_CONHECIMENTO

*&--------------------------------------------------- BOI note 585040 -*
*&      Form  display_cfop_summary_header
*&---------------------------------------------------------------------*
*       Display of the header for the CFOP summary
*----------------------------------------------------------------------*
FORM DISPLAY_CFOP_SUMMARY_HEADER  USING  TEXT_ELEMENT.
  DATA: TEXT_CFOP_SUMMARY(60) TYPE C.
  FORMAT INTENSIFIED COLOR COL_NORMAL.
  MOVE TEXT_ELEMENT TO TEXT_CFOP_SUMMARY.
  IF CFOP_LENGTH = '4'.
    REPLACE '00' IN TEXT_CFOP_SUMMARY WITH '000'.
  ENDIF.
  WRITE: /1 WG_BLANKS(28) COLOR OFF, 29 SY-VLINE.                                       "note 768128
* WRITE:/ text_cfop_summary.                                "note 768128
  WRITE: TEXT_CFOP_SUMMARY.                                 "note 768128
  WRITE: 199 SY-VLINE.                                      "note 768128
  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
ENDFORM.                  " display_cfop_summary_header, EOI note 585040

*&---------------------------------------------------------------------*
*&      Form  f_fake
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_FAKE.
  IF SY-UNAME = 'ABAP'.
    OUT-CANCEL = ''.
    OUT-PSTDAT = '20070628'.
    OUT-SPECIES = 'NF'.
    OUT-SERIES = ''.
    OUT-SUBSER = ''.
    OUT-NFNUM = '096874'.
    OUT-DOCDAT = '20070628'.
    OUT-PARID = '0000100201'.
    OUT-REGIO = 'SP'.
    OUT-NFTOT = '62.68'.
    OUT-CFOP_NOEXT = '2556'.
    OUT-CFOP_EXT = '00'.
    OUT-TAXGRP = 'ICMS'.
    OUT-CODIGO = '3'.
    OUT-BASE = '62.68'.
    OUT-RATE = '0.00'.
    OUT-TAXVAL = '0.00'.
    OUT-DUPL = ''.
    OUT-STATTX = ''.
    OUT-RECTYPE = ''.
    OUT-ITMTYP = '01'.
    OUT-OBS_DOC = ''.
    OUT-OBS_LIN = 'Consumo'.
    OUT-OBS_REF = '0000000000'.
    OUT-OBS_IPIPAUTA = '0.00'.
    OUT-OBS_ICFRBASE = '0.00'.
    OUT-OBS_ICFRTAX = '0.00'.
    OUT-FREE_TEXT = ''.
    OUT-ST_CONSUMO = ''.
    PERFORM OUTPUT USING OUT.
    OUT-CANCEL = ''.
    OUT-PSTDAT = '20070628'.
    OUT-SPECIES = 'NF'.
    OUT-SERIES = ''.
    OUT-SUBSER = ''.
    OUT-NFNUM = '096874'.
    OUT-DOCDAT = '20070628'.
    OUT-PARID = '0000100201'.
    OUT-REGIO = 'SP'.
    OUT-NFTOT = '62.68'.
    OUT-CFOP_NOEXT = '2556'.
    OUT-CFOP_EXT = '00'.
    OUT-TAXGRP = 'ICOP'.
    OUT-CODIGO = '1'.
    OUT-BASE = '62.68'.
    OUT-RATE = '10.00'.
    OUT-TAXVAL = '6.27'.
    OUT-DUPL = 'X'.
    OUT-STATTX = ''.
    OUT-RECTYPE = ''.
    OUT-ITMTYP = '01'.
    OUT-OBS_DOC = ''.
    OUT-OBS_LIN = ''.
    OUT-OBS_REF = '0000000000'.
    OUT-OBS_IPIPAUTA = '0.00'.
    OUT-OBS_ICFRBASE = '0.00'.
    OUT-OBS_ICFRTAX = '0.00'.
    OUT-FREE_TEXT = ''.
    OUT-ST_CONSUMO = ''.
    PERFORM OUTPUT USING OUT.
    OUT-CANCEL = ''.
    OUT-PSTDAT = '20070628'.
    OUT-SPECIES = 'NF'.
    OUT-SERIES = ''.
    OUT-SUBSER = ''.
    OUT-NFNUM = '096874'.
    OUT-DOCDAT = '20070628'.
    OUT-PARID = '0000100201'.
    OUT-REGIO = 'SP'.
    OUT-NFTOT = '62.68'.
    OUT-CFOP_NOEXT = '2556'.
    OUT-CFOP_EXT = '00'.
    OUT-TAXGRP = 'IPI'.
    OUT-CODIGO = '2'.
    OUT-BASE = '62.68'.
    OUT-RATE = '0.00'.
    OUT-TAXVAL = '0.00'.
    OUT-DUPL = 'X'.
    OUT-STATTX = ''.
    OUT-RECTYPE = ''.
    OUT-ITMTYP = '01'.
    OUT-OBS_DOC = ''.
    OUT-OBS_LIN = ''.
    OUT-OBS_REF = '0000000000'.
    OUT-OBS_IPIPAUTA = '0.00'.
    OUT-OBS_ICFRBASE = '0.00'.
    OUT-OBS_ICFRTAX = '0.00'.
    OUT-FREE_TEXT = ''.
    OUT-ST_CONSUMO = ''.
    PERFORM OUTPUT USING OUT.

  ENDIF.

ENDFORM.                    "f_fake

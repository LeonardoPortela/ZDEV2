REPORT Z_J_1BLB02 MESSAGE-ID 8B
                   LINE-SIZE 199
                   LINE-COUNT 65(1)
                   NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
*------------------------Report J_1BLB02-------------------------------*
*----------------------------------------------------------------------*
*-------Creation of the legal book 'Registro de Saídas'----------------*
*-----------------------------(Modelo 2)-------------------------------*
*----------------------------------------------------------------------*

* declaration of the nota fiscal database tables

TABLES: J_1BNFDOC,                     "nota fiscal header,
        J_1BINDOC,                "nota fiscal header - add. segment
       *J_1BNFDOC,                     "nota fiscal header,
        J_1BNFLIN,                     "nota fiscal line items,
        J_1BINLIN,                "nota fiscal line items - add. segment
        J_1BNFSTX,                     "nota fiscal tax per item,
       *J_1BNFSTX,
        J_1BAJ,                        "nota fiscal types,
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
SELECTION-SCREEN END OF BLOCK B1.

* additional parameter: zfmanaus: abbreviation for Zona Franca de Manaus

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-110.
PARAMETERS: ZFMANAUS(3) TYPE C OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*

* parameters for building subtotals (Decêndio, Quinzena, Mês)

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-120.
PARAMETERS: DECENDIO TYPE J_1BREP_PERIOD10 RADIOBUTTON GROUP TYP3
                                                            DEFAULT 'X',
            QUINZENA TYPE J_1BREP_PERIOD15 RADIOBUTTON GROUP TYP3,
            MES      TYPE J_1BREP_MONTH    RADIOBUTTON GROUP TYP3.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*

* definition of constants for taxgroups/taxtypes

CONSTANTS: BEGIN OF CONST_TAXGRP,
           ICMS(4)        TYPE C VALUE 'ICMS',
           IPI(4)         TYPE C VALUE 'IPI ',
           SUBTRIB(4)     TYPE C VALUE 'ICST',
           ICZONAF(4)     TYPE C VALUE 'ICZF',
           ICZONAG(4)     TYPE C VALUE 'ICZG',          " note 815174
           ICMF(4)        TYPE C VALUE 'ICMF',
           ICSTFREIGHT(4) TYPE C VALUE 'ICFS', "ICMS frght/ST
           ICMSFREIGHT(4) TYPE C VALUE 'ICFR', "ICMS frght/ST
           ISSS(4)        TYPE C VALUE 'ISSS',              "955768
           ISSP(4)        TYPE C VALUE 'ISSP',              "955768
           ISS(4)         TYPE C VALUE 'ISS',               "955768
           END OF CONST_TAXGRP.
CONSTANTS: BLANK VALUE ' '.

CONSTANTS: CONST_EX(2) VALUE 'EX'.
CONSTANTS: SAO_PAULO(2) VALUE 'SP'.  "abbreviation for Sao Paulo state
CONSTANTS: SANTA_CATARINA(2) VALUE 'SC'. "abbrev. for St. Catarina state
*----------------------------------------------------------------------*

* key for internally used record data - contains sorting fields
DATA: WG_BLANKS(255).
DATA: BEGIN OF KEY,
      MONTHYEAR(6)  TYPE N,      " used for total/subtotals; note 697076
      PSTDAT        LIKE J_1BNFDOC-PSTDAT,"posting date
*      species(2)    type c,            "species of fiscal document
      SPECIES(3)    TYPE C,     "species of fiscal document  note 736398
                                       "NF = Nota Fiscal
                                       "CO = Conhecimento
      SERIES        LIKE J_1BNFDOC-SERIES,"series of the fiscal document
      SUBSER        LIKE J_1BNFDOC-SUBSER,"subseries of the fisc. doc.
      NFNUM         LIKE J_1BNFDOC-NFNUM, "number of fiscal document
      REGIO         LIKE J_1BINNAD-REGIO, "region
      DOCNUM        LIKE J_1BNFDOC-DOCNUM,"to distinguish different
                                       "documents (summing)
*      cfop_noext(3) type c,            "cfop number without extension
      CFOP_NOEXT(4) TYPE C,            "cfop number without extension
*      CFOP_EXT(2)   TYPE c,            "cfop numberfirst digit of ext
      CFOP_EXT(2)   TYPE N,            "cfop numberfirst digit of ext
      TAXGRP        LIKE J_1BAJ-TAXGRP,"taxgroup
      RATE          LIKE J_1BNFSTX-RATE,  "taxrate
      OBS_DOC(50)   TYPE C,            "document related observation
      OBS_LIN(100)  TYPE C,            "line related observation
      OBS_REF       LIKE J_1BNFDOC-DOCNUM,"internal documentnumber of a
                                       "referenced document
END OF KEY.

* internally used record data

DATA: BEGIN OF DATA,
      CANCEL           LIKE J_1BNFDOC-CANCEL,"indicator for cancelled
                                       "documents
      NFTOT            LIKE J_1BINLIN-NFTOT, "total value per item,
                                       "incl. ICMS/ISS, IPI,
                                       "freight, insurance, other
                                       "expenses
      BASE             LIKE J_1BNFSTX-BASE,  "calculation base
      EXCBAS           LIKE J_1BNFSTX-EXCBAS,"excluded base amount
      OTHBAS           LIKE J_1BNFSTX-OTHBAS,"other base amount
      TAXVAL           LIKE J_1BNFSTX-TAXVAL,"tax amount
      WITH_STAINS(1)    TYPE C,        "indicator: if 'X', then partner
                                       "possesses reg. tax code - used
                                       "for state summary
      STATTX           LIKE J_1BNFSTX-STATTX, "statistical tax indicator
      RECTYPE          LIKE J_1BNFSTX-RECTYPE,"IPI Pauta
      OBS_IPIPAUTA     LIKE J_1BNFSTX-TAXVAL,"IPI Pauta amt
      OBS_ICFRBASE     LIKE J_1BNFSTX-BASE,   "ICMS freight/ST
      OBS_ICFRTAX      LIKE J_1BNFSTX-TAXVAL, "ICMS freight/ST
      ITMTYP           LIKE J_1BNFLIN-ITMTYP, "itemtype
      FREE_TEXT        LIKE J_1BNFDOC-OBSERVAT,
END OF DATA.

*----------------------------------------------------------------------*

* declaration of internal table for nota fiscal types ( = j_1baj)

DATA: INTJ_1BAJ LIKE J_1BAJ OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*

* cfop summaries

DATA: BEGIN OF CFOP_LINE OCCURS 0,
*      cfop_noext(3) type c,               " note 553750
      CFOP_NOEXT(4) TYPE C,                " note 553750
      CFOP_EXT(2)   TYPE C,
      NFTOT    LIKE J_1BINLIN-NFTOT,
      BASE     LIKE J_1BNFSTX-BASE,
      TAXVAL   LIKE J_1BNFSTX-TAXVAL,
      EXCBAS   LIKE J_1BNFSTX-EXCBAS,
      OTHBAS   LIKE J_1BNFSTX-OTHBAS,
END OF CFOP_LINE.

DATA: BEGIN OF CFOP_LINE_TOT OCCURS 3,
      CFOP_FIRST(1) TYPE C,
      NFTOT    LIKE J_1BINLIN-NFTOT,
      BASE     LIKE J_1BNFSTX-BASE,
      TAXVAL   LIKE J_1BNFSTX-TAXVAL,
      EXCBAS   LIKE J_1BNFSTX-EXCBAS,
      OTHBAS   LIKE J_1BNFSTX-OTHBAS,
END OF CFOP_LINE_TOT.

*----------------------------------------------------------------------*

* summaries per state ICMS/ICST

DATA: BEGIN OF STATE_SUMMARY_ICMS OCCURS 0,
      WITH_STAINS LIKE DATA-WITH_STAINS,"indicator: if 'X', then partner
                                       "possesses reg. tax. code, used
                                       "for state summary
      REGIO    LIKE J_1BINNAD-REGIO,
      TAXGRP   LIKE J_1BAJ-TAXGRP,
      NFTOT    LIKE J_1BINLIN-NFTOT,
      BASE     LIKE J_1BNFSTX-BASE,
      TAXVAL LIKE J_1BNFSTX-TAXVAL,
END OF STATE_SUMMARY_ICMS.

*----------------------------------------------------------------------*

* declaration of structures coming from function modules

DATA: CGC_NUMBER  LIKE J_1BWFIELD-CGC_NUMBER,
      ADDRESS1    LIKE ADDR1_VAL,
      BRANCH_DATA LIKE J_1BBRANCH,
      PARNAD      LIKE J_1BINNAD,
      T_MESG      LIKE MESG OCCURS 0 WITH HEADER LINE,
      WG_FIRMA(80).

* layout variables:
* c_name = start column for output of variable called "name"
* l_name = reserved length for output of variable "name"
DATA: C_SERIE TYPE I,
      C_SPECI TYPE I,
      C_SUBSE TYPE I,
      C_NFNUM TYPE I,
      C_DOCDA TYPE I,
      C_REGIO TYPE I, L_REGIO TYPE I,
      C_NFTOT TYPE I, L_NFTOT TYPE I,
      C_EMPTY TYPE I, L_EMPTY TYPE I,
      C_CFOPN TYPE I, L_CFOPN TYPE I,
      C_CFOPX TYPE I,
      C_TAXGR TYPE I, L_TAXGR TYPE I,
      C_BASE  TYPE I, L_BASE  TYPE I,
      C_RATE  TYPE I, L_RATE  TYPE I,
      C_TAXVA TYPE I, L_TAXVA TYPE I,
      C_EXCBA TYPE I, L_EXCBA TYPE I,
      C_OTHBA TYPE I, L_OTHBA TYPE I,
      C_OBSER TYPE I, L_OBSER TYPE I.
DATA: C_HELP TYPE I.

* definition of the field-groups for the extract

FIELD-GROUPS: HEADER,
              EXDATA.

*insert key-pstdat                   " note 697076
INSERT KEY-MONTHYEAR                 " note 697076
       KEY-PSTDAT                    " note 697076
       KEY-SPECIES
       KEY-SERIES
       KEY-SUBSER
       KEY-NFNUM
       KEY-REGIO
       KEY-DOCNUM
       KEY-CFOP_NOEXT
       KEY-CFOP_EXT
       KEY-TAXGRP
       KEY-RATE
       KEY-OBS_DOC
       KEY-OBS_LIN
       KEY-OBS_REF
       INTO HEADER.

INSERT DATA INTO EXDATA.

*----------------------------------------------------------------------*

* declaration of internally used help-fields

DATA: FIRST_RECORD      TYPE C VALUE 'X',
      FIRST_RECORD_MONTHYEAR TYPE C VALUE 'X',         " note 697076
      LAST_TOTAL        TYPE C VALUE ' ',
      PAGNO(6)          TYPE N,        "pagenumber within one book
      BOOKS(2)          TYPE N VALUE '0', "number of printed books
      PAGES(6)          TYPE N VALUE '0', "total number of printed pages
      ERRORLIST         TYPE C VALUE ' ',
      CORRLIST          TYPE C VALUE ' ',
      DOCNUM       LIKE J_1BNFDOC-DOCNUM, "to avoid duplications in the
                                       "output
      OLDDAY(2)         TYPE N,        "10 days totals
      INDIC_FIRST_ICMS_PER_ITEM    TYPE C.

DATA: LINE_WITH_NONSTAT_TAX.
"indicator:tax line with non-statistical tax exists
*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed (summed up) by taxrate and/or CFOP number

DATA: BEGIN OF TAXSUMS,
      NFTOT         LIKE J_1BINLIN-NFTOT,
      BASE          LIKE J_1BNFSTX-BASE,
      EXCBAS        LIKE J_1BNFSTX-EXCBAS,
      OTHBAS        LIKE J_1BNFSTX-OTHBAS,
      TAXVAL        LIKE J_1BNFSTX-TAXVAL,
      OBS_IPIPAUTA  LIKE J_1BNFSTX-TAXVAL,  "IPI Pauta amt
      END OF TAXSUMS.

*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed by day

DATA: BEGIN OF TOTAL,
      VALORCONT   LIKE J_1BINLIN-NFTOT,"valor contabil
      ICMSBASE    LIKE J_1BNFSTX-BASE, "calculation base ICMS
      IPIBASE     LIKE J_1BNFSTX-BASE, "calculation base IPI
      SUBTRIBBASE LIKE J_1BNFSTX-BASE, "calculation base ST
      ICMSEXCB    LIKE J_1BNFSTX-BASE, "excluded base amount ICMS
      IPIEXCB     LIKE J_1BNFSTX-BASE, "excluded base amount IPI
      SUBTRIBEXCB LIKE J_1BNFSTX-BASE, "excluded base amount ST
      ICMSOTHB    LIKE J_1BNFSTX-BASE, "other base amount ICMS
      IPIOTHB     LIKE J_1BNFSTX-BASE, "other base amount IPI
      SUBTRIBOTHB LIKE J_1BNFSTX-BASE, "other base amount ST
      ICMSVAL     LIKE J_1BNFSTX-TAXVAL, "tax value ICMS
      IPIVAL      LIKE J_1BNFSTX-TAXVAL, "tax value IPI
      SUBTRIBVAL  LIKE J_1BNFSTX-TAXVAL, "tax value ST
      END OF TOTAL.

DATA: CIAP_TOTAL LIKE TOTAL. "save total for CIAP: calculation of fator
*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed by 10 days or per month

DATA: BEGIN OF TOTAL_10 OCCURS 5,                          " note 697076
      NUMBER(1)   TYPE C,              "first, second or third 10-day
                                       "total of a period
      VALORCONT   LIKE J_1BINLIN-NFTOT,"valor contabil
      ICMSBASE    LIKE J_1BNFSTX-BASE, "calculation base ICMS
      IPIBASE     LIKE J_1BNFSTX-BASE, "calculation base IPI
      SUBTRIBBASE LIKE J_1BNFSTX-BASE, "calculation base ST
      ICMSEXCB    LIKE J_1BNFSTX-BASE, "excluded base amount ICMS
      IPIEXCB     LIKE J_1BNFSTX-BASE, "excluded base amount IPI
      SUBTRIBEXCB LIKE J_1BNFSTX-BASE, "excluded base amount ST
      ICMSOTHB    LIKE J_1BNFSTX-BASE, "other base amount ICMS
      IPIOTHB     LIKE J_1BNFSTX-BASE, "other base amount IPI
      SUBTRIBOTHB LIKE J_1BNFSTX-BASE, "other base amount ST
      ICMSVAL     LIKE J_1BNFSTX-TAXVAL,  "tax value ICMS
      IPIVAL      LIKE J_1BNFSTX-TAXVAL,  "tax value IPI
      SUBTRIBVAL  LIKE J_1BNFSTX-TAXVAL,  "tax value ST
      END OF TOTAL_10.

*----------------------------------------------------------------------*

* table with corrections per month

DATA: BEGIN OF CORR OCCURS 0,
      PSTDAT        LIKE J_1BNFDOC-PSTDAT,"posting date
      SERIES        LIKE J_1BNFDOC-SERIES,"series of the fiscal document
      NFNUM         LIKE J_1BNFDOC-NFNUM, "number of fiscal document
      DOCNUM        LIKE J_1BNFDOC-DOCNUM,"internal document number
                                       "of the correction
      DOCREF        LIKE J_1BNFDOC-DOCREF,"internal document number
                                       "of the corrected document
      CANCEL        LIKE J_1BNFDOC-CANCEL,"indicator: doc was cancelled
END OF CORR.

*----------------------------------------------------------------------*

* declaration of output-line

DATA: BEGIN OF OUT,
      CANCEL           LIKE J_1BNFDOC-CANCEL,
      SPECIES          LIKE KEY-SPECIES,
      SERIES           LIKE J_1BNFDOC-SERIES,
      SUBSER           LIKE J_1BNFDOC-SUBSER,
      NFNUM            LIKE J_1BNFDOC-NFNUM,
      DOCDAY(2)        TYPE N,         "posting date; only day
      REGIO            LIKE KEY-REGIO,
      NFTOT            LIKE J_1BINLIN-NFTOT,
      CFOP_NOEXT       LIKE KEY-CFOP_NOEXT,
      CFOP_EXT         LIKE KEY-CFOP_EXT,
      TAXGRP           LIKE J_1BAJ-TAXGRP,
      BASE             LIKE J_1BNFSTX-BASE,
      RATE             LIKE J_1BNFSTX-RATE,
      TAXVAL           LIKE J_1BNFSTX-TAXVAL,
      EXCBAS           LIKE J_1BNFSTX-EXCBAS,
      OTHBAS           LIKE J_1BNFSTX-OTHBAS,
      DUPL             TYPE C,  "indicator: to avoid repetition of
                                       "header information in output
      STATTX           LIKE J_1BNFSTX-STATTX,
      RECTYPE          LIKE J_1BNFSTX-RECTYPE,   "IPI Pauta
      ITMTYP           LIKE J_1BNFLIN-ITMTYP,
      OBS_DOC          LIKE KEY-OBS_DOC,
      OBS_LIN          LIKE KEY-OBS_LIN,
      OBS_REF          LIKE KEY-OBS_REF,
      OBS_IPIPAUTA     LIKE J_1BNFSTX-TAXVAL,    "IPI Pauta amt
      OBS_ICFRBASE     LIKE J_1BNFSTX-BASE,      "ICMS freight/ST
      OBS_ICFRTAX      LIKE J_1BNFSTX-TAXVAL,    "ICMS freight/ST
      FREE_TEXT        LIKE J_1BNFDOC-OBSERVAT,
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
*           address     =
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

*----------------------------------------------------------------------*
*---------------------start-of-selection-------------------------------*
*----------------------------------------------------------------------*
  DEFINE ZULINE.
    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
            29 SY-ULINE(255).
*  uline at 29.
  END-OF-DEFINITION. "uline

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

  SELECT * FROM J_1BNFITMRULE INTO TABLE GT_NFITMRULE.   "note 553750

  SELECT * FROM J_1BMODTEXT   INTO TABLE GT_J_1BMODTEXT. "note 553750

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

  CLEAR: KEY,
         DATA,
         PARNAD.

* selection of the outgoing movements

  CHECK J_1BNFDOC-DIRECT = '2'.

* exclude documents with external document number (NF number) '000000'

  CHECK J_1BNFDOC-NFNUM NE '000000'.

* exclude documents that were only created to cancel another document
* indicator: documenttype (j_1bnfdoc-doctyp) = 5

  CHECK J_1BNFDOC-DOCTYP NE '5'.

* table with corrections per month
  IF J_1BNFDOC-DOCTYP = '3'.
    MOVE-CORRESPONDING J_1BNFDOC TO CORR.
    APPEND CORR.
  ENDIF.

* don't report corrections
  CHECK J_1BNFDOC-DOCTYP NE '3'.

* determine data of partner for currently processed fiscal document
* in order to check if document refers to sale to Zona Franca de Manaus
* check the taxregion against input parameter zfmanaus
* and to determine the destination region

  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      PARTNER_TYPE     = J_1BNFDOC-PARTYP
      PARTNER_ID       = J_1BNFDOC-PARID
      PARTNER_FUNCTION = J_1BNFDOC-PARVW
      DOC_NUMBER       = J_1BNFDOC-DOCNUM
    IMPORTING
      PARNAD           = PARNAD
    EXCEPTIONS
      OTHERS           = 04.

* if the partner is not brazilian, the literal 'EX' is assigned to regio
* Note 162933
  IF PARNAD-LAND1 NE ADDRESS1-COUNTRY.
    MOVE CONST_EX TO PARNAD-REGIO.
  ENDIF.

*  determine observation later: together with observ. for Complementar
*  if parnad-txjcd = zfmanaus.
*    move text-102 to key-obs_doc.
*  endif.

* Partner has a regional tax code? => taxpayer
  IF NOT PARNAD-STAINS IS INITIAL.
    MOVE 'X' TO DATA-WITH_STAINS.
  ENDIF.

* determine species: nf / co

  CASE J_1BNFDOC-DOCTYP.
    WHEN '1'.
      MOVE 'NF' TO KEY-SPECIES.
    WHEN '4'.
      MOVE 'CO' TO KEY-SPECIES.
    WHEN '6'.
      MOVE 'NF' TO KEY-SPECIES.        "Return is always NF
    WHEN '7'.                       "con w/o ref. "Dev_45B_ConwoREF
      MOVE 'CO' TO KEY-SPECIES.     "con w/o ref. "Dev_45B_ConwoREF
    WHEN OTHERS.
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
      CLEAR *J_1BNFDOC.
  ENDCASE.

* note 736398: BADI offered for modification of the NF species
  CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->ESPECIE_INDICATOR_CHANGE
    EXPORTING
      IS_J_1BNFDOC = J_1BNFDOC
      IV_ESPECIE   = KEY-SPECIES
    IMPORTING
      EV_ESPECIE   = KEY-SPECIES.

* Clear the cancel flag in case of cancellation of the " BOI note 617905
* NF (CANDAT) after end of selected reporting period
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

* check if document was cancelled

  IF J_1BNFDOC-CANCEL = 'X'.
    MOVE:  J_1BNFDOC-CANCEL  TO DATA-CANCEL,
             TEXT-101          TO KEY-OBS_DOC.
  ELSEIF J_1BNFDOC-OBSERVAT(1) = '*'.
    MOVE J_1BNFDOC-OBSERVAT+1 TO DATA-FREE_TEXT.
  ENDIF.

  MOVE-CORRESPONDING PARNAD TO KEY.
  MOVE-CORRESPONDING J_1BNFDOC TO KEY.
  MOVE-CORRESPONDING J_1BNFDOC TO DATA.
  MOVE J_1BNFDOC-PSTDAT(6) TO KEY-MONTHYEAR.               " note 697076

*----------------------------------------------------------------------*
*----------------------------get j_1bnflin-----------------------------*
*----------------------------------------------------------------------*

GET J_1BNFLIN.

  CLEAR: KEY-CFOP_NOEXT, KEY-CFOP_EXT.

* indicator is initialised; will be cleared after first ICMS tax line
  INDIC_FIRST_ICMS_PER_ITEM  = 'X'.

* Check in case of ISS-taxed service                            "955768
  IF J_1BNFLIN-TMISS = 'X'.                                 "955768
    CHECK J_1BNFLIN-CFOP <> ' '.                            "955768
    CHECK INCL_SRV = 'X'.                                   "955768
  ENDIF.                                                    "955768

*----------------------------------------------------------------------*
*----------------------------get j_1binlin-----------------------------*
*----------------------------------------------------------------------*

GET J_1BINLIN.

  CLEAR: DATA-NFTOT.

* nothing will be extracted if document was cancelled

  IF J_1BNFDOC-CANCEL = ' '.

* convert CFOP for SC into 6 digit form (e.g. j917a into 19917a)
    IF J_1BNFLIN-CFOP(1) CN '0123456789'.
      WRITE J_1BNFLIN-CFOP TO J_1BNFLIN-CFOP.
    ENDIF.

*    move j_1bnflin-cfop+0(3) to key-cfop_noext.           " note 553750
    MOVE J_1BNFLIN-CFOP+0(CFOP_LENGTH) TO KEY-CFOP_NOEXT.  " note 553750

*   determine, if new reporting for cfop codes X99 must be used
*   decreto is valid for Sao Paulo state from 01.01.2000 onwards
*    IF ( J_1BNFLIN-CFOP+1(3) = '991' OR          " BEGIN OF NOTE 553750
*         J_1BNFLIN-CFOP+1(3) = '999'    ) AND         "X99.9
*       ADDRESS1-REGION = SAO_PAULO  AND
*       J_1BNFDOC-PSTDAT > '20001231'.
*         MOVE J_1BNFLIN-CFOP+3(1) TO KEY-CFOP_EXT.
*    ENDIF.
    IF ( J_1BNFLIN-CFOP+1(3) = '991' OR J_1BNFLIN-CFOP+1(3) = '999' )
       AND ADDRESS1-REGION = SAO_PAULO
       AND EXT_LEN > 0.
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.
*   determine, if new reporting for Santa Catarina must be used
*    IF ( J_1BNFLIN-CFOP(3) = '199' OR J_1BNFLIN-CFOP(3) = '299' OR
*         J_1BNFLIN-CFOP(3) = '599' OR J_1BNFLIN-CFOP(3) = '699' ) AND
*       ADDRESS1-REGION = SANTA_CATARINA AND
*       J_1BNFDOC-PSTDAT > '20001231'.
*       MOVE J_1BNFLIN-CFOP+3(2) TO KEY-CFOP_EXT.
*    ENDIF.
    IF J_1BNFLIN-CFOP+1(2) = '99'
       AND ADDRESS1-REGION = SANTA_CATARINA
       AND EXT_LEN > 0.
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.                                          " END OF NOTE 553750

* observations
    MOVE J_1BNFDOC-DOCREF TO KEY-OBS_REF.
    MOVE J_1BNFLIN-MAKTX  TO KEY-OBS_LIN.
*-------------------------------------------------------------------
*---> Begin of deletion note 553750
*   case j_1bnflin-itmtyp.
*     when '31'. "subcontr. IV of manufactured products (subsequent NF
*                                      "in subcontracting process
*       move text-088 to key-obs_lin.
*     when '32'. "subcontracting GR of components (first NF in subc. pr)
*       move text-087 to key-obs_lin.
*     when '33'. "subcontr. IV of manufactured products (subsequent NF
*                                      "in subcontracting process
*       move text-088 to key-obs_lin.
*    when '41'. "future delivery IV item (first NF in fut. del. process)
*                                      "with IPI
*       move text-082 to key-obs_lin.
*     when '42'. "future delivery GR item (second NF in fut. del. proc.)
*                                      "with IPI
*       move text-090 to key-obs_lin.
*    when '43'. "future delivery IV item (first NF in fut. del. process)
*                                      "without IPI
*       move text-082 to key-obs_lin.
*     when '44'. "future delivery GR item (second NF in fut. del. proc.)
*                                      "without IPI
*       move text-090 to key-obs_lin.
*     when '51'. "Consignment IV item (second NF in consignment process)
*       move text-084 to key-obs_lin.
*     when '52'. "Consignment GR item (first NF in consignment process)
*       move text-085 to key-obs_lin.
*    when '62'. "Third party IV item supplier to seller  (case A - NF 3)
*       move text-086 to key-obs_lin.
*    when '63'. "Third party GR item supplier to customer (case A -NF 2)
*       concatenate text-090 j_1bnfdoc-observat
*                   into key-obs_lin separated by space.
*       clear key-obs_ref.
*     when '64'.                       "Third party NF3
* Third party case B is currently not supported in SD
*     when '65'.                       "Third party NF0
* Third party case B is currently not supported in SD
*   endcase.
*---> End of deletion note 553750
*
*---> Begin of insertion note 553750
    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE
         WITH KEY ITMTYP = J_1BNFLIN-ITMTYP.
    IF SY-SUBRC IS INITIAL.
*---> text number for reporting filled
      IF NOT GS_NFITMRULE-TEXTMOD2 IS INITIAL.
*---> get special texts from customizing table J_1BMODTEXT
        READ TABLE GT_J_1BMODTEXT INTO GS_J_1BMODTEXT
          WITH KEY SPRAS   = SY-LANGU
                   TEXTNUM = GS_NFITMRULE-TEXTMOD2.
*---> text defined take it from customizing
        IF SY-SUBRC IS INITIAL.
          MOVE GS_J_1BMODTEXT-TEXT TO KEY-OBS_LIN.
*---> text not defined in customizing -> use report text elements
        ELSE.
          CONCATENATE C_TEXT GS_NFITMRULE-TEXTMOD2 INTO LV_NAME.
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
      IF GS_NFITMRULE-CLEARREF2 = 'X'.
        CLEAR KEY-OBS_REF.
      ENDIF.
    ENDIF.
*---> End of insertion note 553750

* determine observations text in case of returns or complementars
* and for zona franca

*  Zona Franca without Complementar
    IF J_1BNFDOC-DOCTYP <> '2' AND PARNAD-TXJCD = ZFMANAUS.
      MOVE TEXT-102 TO KEY-OBS_DOC.
    ENDIF.

    CASE J_1BNFDOC-DOCTYP.
      WHEN '6'.
        MOVE TEXT-080 TO KEY-OBS_LIN.  "return
        IF NOT J_1BNFLIN-DOCREF IS INITIAL.
          MOVE J_1BNFLIN-DOCREF TO KEY-OBS_REF.
        ELSE.
          MOVE J_1BNFDOC-DOCREF TO KEY-OBS_REF.
        ENDIF.
      WHEN '2'.
**       move text-081 to key-obs_doc.                     "complementar
*       concatenate key-obs_doc text-081 into key-obs_doc  "complementar
*           separated by space.
        IF PARNAD-TXJCD = ZFMANAUS.
*         Zona Franca and Complementar
          CONCATENATE TEXT-102 TEXT-081
                      INTO KEY-OBS_DOC SEPARATED BY '  '.
        ELSE.
*          Complementar without Zona Franca
          MOVE TEXT-081 TO KEY-OBS_DOC.
        ENDIF.
    ENDCASE.

    MOVE-CORRESPONDING J_1BINLIN TO DATA.
    MOVE J_1BNFLIN-ITMTYP TO DATA-ITMTYP.
  ENDIF.

*----------------------------------------------------------------------*
*----------------------------get j_1bnfstx-----------------------------*
*----------------------------------------------------------------------*

GET J_1BNFSTX.

  CHECK J_1BNFSTX-TAXTYP <> CONST_TAXGRP-ICZONAG.      " note 815174

*ENHANCEMENT-POINT J_1BLB02_01 SPOTS ES_J_1BLB02.

  CLEAR: KEY-RATE,
         KEY-TAXGRP,
         DATA-BASE,
         DATA-EXCBAS,
         DATA-OTHBAS,
         DATA-TAXVAL,
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
   OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISSS                  "955768
   OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISSP                  "955768
   OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISS.                  "955768

* clear observations fields, so that output happens only once
  IF INTJ_1BAJ-TAXGRP NE CONST_TAXGRP-ICMS.
    CLEAR KEY-OBS_LIN.
    CLEAR KEY-OBS_REF.
  ENDIF.

* nothing will be extracted if document was cancelled
  IF J_1BNFDOC-CANCEL = 'X'.                                "955768
                                                            "955768
* Reporting of ISS-taxed services                               "955768
  ELSEIF J_1BNFDOC-CANCEL = ' ' AND                         "955768
         J_1BNFLIN-TMISS = 'X'.                             "955768
    KEY-TAXGRP = 'ICMS'.                                    "955768
    MOVE-CORRESPONDING J_1BNFSTX TO KEY.                    "955768
    MOVE-CORRESPONDING J_1BNFSTX TO DATA.                   "955768
    IF DATA-BASE <> 0.                                      "955768
      DATA-EXCBAS = DATA-EXCBAS + DATA-BASE.                "955768
      CLEAR DATA-BASE.                                      "955768
    ENDIF.                                                  "955768
    IF DATA-OTHBAS <> 0.                                    "955768
      DATA-EXCBAS = DATA-EXCBAS + DATA-OTHBAS.              "955768
      CLEAR DATA-OTHBAS.                                    "955768
    ENDIF.                                                  "955768
    CLEAR KEY-RATE.                                         "955768
    CLEAR DATA-TAXVAL.                                      "955768
  ELSE.                                                     "955768
*  if j_1bnfdoc-cancel = ' '.                                   "955768

    MOVE INTJ_1BAJ-TAXGRP TO KEY-TAXGRP.

    MOVE-CORRESPONDING J_1BNFSTX TO KEY.
    MOVE-CORRESPONDING J_1BNFSTX TO DATA.

* If there more than one ICMS tax lines per item: remain " BOI nt 773242
* nftot only for the first ICMS line and delete nftot for the others
    IF KEY-TAXGRP = CONST_TAXGRP-ICMS AND
                                   INDIC_FIRST_ICMS_PER_ITEM IS INITIAL.
      CLEAR DATA-NFTOT.
    ENDIF.
    IF J_1BNFSTX-TAXTYP <> CONST_TAXGRP-ICZONAF.      " corr note 773242
      CLEAR INDIC_FIRST_ICMS_PER_ITEM.                   " EOI nt 773242
    ENDIF.                                            " corr note 773242
*  adjust total line value for ICMS-taxes in case of Zona Franca
    IF J_1BNFSTX-TAXTYP = CONST_TAXGRP-ICZONAF.
      CLEAR DATA-NFTOT.
*     reset to j_1binlin-nftot after the extract - in case of other
*     taxes for the same line !
    ENDIF.

* Don't print rate in case of IPI                             "IPI Pauta

    IF KEY-TAXGRP = CONST_TAXGRP-IPI.
      CLEAR KEY-RATE.
    ENDIF.

    IF J_1BNFSTX-STATTX = 'X'.
      CLEAR DATA-TAXVAL.
      CLEAR DATA-BASE.
      CLEAR DATA-EXCBAS.
      CLEAR DATA-OTHBAS.
    ENDIF.

    IF J_1BNFSTX-BASE = 0.
* note 526526: for intrastate transactions between two branches the ICMS
* taxvalue is reported
      IF ( J_1BNFSTX-EXCBAS <> 0                 OR
           J_1BNFSTX-OTHBAS <> 0                 OR
           INTJ_1BAJ-TAXGRP <> CONST_TAXGRP-ICMS OR
           ADDRESS1-REGION  <> PARNAD-REGIO      OR
           J_1BNFDOC-PARTYP <> 'B' ).
        CLEAR DATA-TAXVAL.
      ENDIF.
      CLEAR KEY-RATE.
    ENDIF.

    IF J_1BNFSTX-OTHBAS > 0.
      IF J_1BNFSTX-RECTYPE = '1' AND J_1BNFDOC-DOCTYP NE '6'.    "return
        DATA-OBS_IPIPAUTA = J_1BNFSTX-TAXVAL.                 "IPI Pauta
      ENDIF.
    ENDIF.

*   do not print bases < 0
    IF DATA-BASE < 0.
      CLEAR DATA-BASE.
    ENDIF.
    IF DATA-EXCBAS < 0.
      CLEAR DATA-EXCBAS.
    ENDIF.
    IF DATA-OTHBAS < 0.
      CLEAR DATA-OTHBAS.
    ENDIF.

* CHANGES FOR THE 'ALL BASES = 0 CASE
    IF NOT J_1BNFSTX-TAXTYP = CONST_TAXGRP-ICZONAF AND
       NOT J_1BNFSTX-TAXTYP = CONST_TAXGRP-ICMF    AND
       J_1BNFSTX-BASE       = 0 AND
       J_1BNFSTX-EXCBAS     = 0 AND
       J_1BNFSTX-OTHBAS     = 0.
*      check if at least the tax amount is <> 0
      IF J_1BNFSTX-TAXVAL <> 0.
*         check if NF reference for this line exists
        IF NOT J_1BNFLIN-DOCREF IS INITIAL AND
           NOT J_1BNFLIN-ITMREF IS INITIAL.
*            read the taxes from original document

          SELECT SINGLE * FROM *J_1BNFSTX
                        WHERE DOCNUM = J_1BNFLIN-DOCREF
                        AND   ITMNUM = J_1BNFLIN-ITMREF
                        AND   TAXTYP = J_1BNFSTX-TAXTYP.
*            if reference data was found, check the 'base'
          IF SY-SUBRC = 0 AND *J_1BNFSTX-BASE > 0
                 AND J_1BNFSTX-STATTX NE 'X'.
            DATA-TAXVAL = J_1BNFSTX-TAXVAL.
          ENDIF.
        ENDIF.         "not j_1bnflin-docref is initial...
      ENDIF.
    ENDIF.               "not j_1bnfstx-taxtyp = const_taxtype-iczonaf..
* End of "All Bases = 0

  ENDIF.

* NOTE 210543: no special treatment for IPI pauta anymore
*  if j_1bnfstx-rectype = '1'.          "IPI Pauta
*    clear data-base.
*    clear data-excbas.
*    clear data-othbas.
*    key-rate = j_1bnfstx-rate.
*  endif.

* create extract exdata
  EXTRACT EXDATA.
* Creation of additional IPI line for ISS-taxed services        "955768
  IF J_1BNFLIN-TMISS = 'X'.                                 "955768
    KEY-TAXGRP = 'IPI'.                                     "955768
    EXTRACT EXDATA.                                         "955768
  ENDIF.                                                    "955768

* readjust data-nftot to j_1binlin-nftot in case of Zona Franca in
* outgoing movements

  IF J_1BNFDOC-CANCEL = ' '.
    IF J_1BNFSTX-TAXTYP = CONST_TAXGRP-ICZONAF.
      DATA-NFTOT = J_1BINLIN-NFTOT.
    ENDIF.
  ENDIF.

* clear observations fields, so that output happens only once
*  clear key-obs_lin.
*  clear key-obs_ref.

*----------------------------------------------------------------------*
*-----------------------end-of-selection-------------------------------*
*----------------------------------------------------------------------*

END-OF-SELECTION.

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
        OBJECT                = TEXT-400
      EXCEPTIONS
        NO_MESSAGES           = 2.
  ENDIF.

*----------------------------------------------------------------------*
*------------------------------- sort ---------------------------------*
*------------------ the extract according to the key ------------------*
*----------------------------------------------------------------------*

  SORT.

*----------------------------------------------------------------------*
*--- condense records belonging to one nota fiscal if cfops (without --*
*- extensions) and taxrates correspond and write the condensed record -*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-------------------------------- loop --------------------------------*
*----------------------------------------------------------------------*

  LOOP.

*----------------------------------------------------------------------*
*---------------------at new key-observations--------------------------*
*----------------------------------------------------------------------*

* write condensed line

    AT NEW KEY-OBS_REF.

*   at new key-rate.
      IF FIRST_RECORD  = ' '.
        PERFORM OUTPUT USING OUT.
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
*-------------------------- at new key-pstdat -------------------------*
*----------------------------------------------------------------------*

* write daily totals
* and create/write 10 days totals

    AT NEW KEY-PSTDAT.
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
*        write 10 days totals
        IF DECENDIO = 'X'.                                 " note 697076
          IF OLDDAY LE 10 AND KEY-PSTDAT+6(2) > 10.
            READ TABLE TOTAL_10 WITH KEY NUMBER = '1'.
            MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*            SKIP.                                    " BOM note 768128
            ZULINE.
*            WRITE: /40 total_10-number, '.', text-380.
            WRITE: /1 WG_BLANKS(28)  COLOR OFF,
              AT 29 SY-VLINE, 68 TOTAL_10-NUMBER,
                    '.', TEXT-380,
                    AT SY-LINSZ SY-VLINE.             " EOM note 768128
            PERFORM TOTAL_OUTPUT USING TOTAL.
*          skip.
            ZULINE.
          ENDIF.
          IF OLDDAY LE 20 AND KEY-PSTDAT+6(2) > 20.
            READ TABLE TOTAL_10 WITH KEY NUMBER = '2'.
            MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*            SKIP.                                    " BOM note 768128
            ZULINE.

*            WRITE: /40 total_10-number, '.', text-380.
            WRITE: /1 WG_BLANKS(28)  COLOR OFF,
                    AT 29 SY-VLINE, 68 TOTAL_10-NUMBER,
                    '.', TEXT-380,
                    AT SY-LINSZ SY-VLINE.
            " EOM note 768128
            PERFORM TOTAL_OUTPUT USING TOTAL.
*            skip.                                     " BOM note 768128
*            WRITE : /1 sy-vline, AT sy-linsz sy-vline. "EOM note 768128
          ENDIF.
        ENDIF.                                         " BOI note 697076
*       write 15 days totals
        IF QUINZENA = 'X'.
          IF OLDDAY LE 15 AND KEY-PSTDAT+6(2) > 15.
            READ TABLE TOTAL_10 WITH KEY NUMBER = '1'.
            MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
*            SKIP.                                     " BOM note 768128
            ZULINE.
*            WRITE: AT 40 total_10-number, '.', text-381.
            WRITE: /1 WG_BLANKS(28)  COLOR OFF,
                    AT 29 SY-VLINE, 68 TOTAL_10-NUMBER,
                    '.', TEXT-381,
                    AT SY-LINSZ SY-VLINE.              " EOM note 768128

            PERFORM TOTAL_OUTPUT USING TOTAL.
*            skip.                                     " BOM note 768128
*            WRITE : /1 sy-vline, AT sy-linsz sy-vline. "EOM note 768128

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
*        SKIP.                                         " BOM note 768128
        ZULINE.
        WRITE : /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE, AT SY-LINSZ SY-VLINE.     " EOM note 768128
        IF DECENDIO = 'X'.
          WRITE: AT 40 TOTAL_10-NUMBER, '.', TEXT-380.
          PERFORM TOTAL_OUTPUT USING TOTAL.
        ELSEIF QUINZENA = 'X'.
          WRITE: AT 40 TOTAL_10-NUMBER, '.', TEXT-381.
          PERFORM TOTAL_OUTPUT USING TOTAL.
        ENDIF.
*      SKIP.                                          " BOM note 768128
        ZULINE.                                          " EOM note 768128
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
*        WRITE: text-382.   " mês
        WRITE: 29 SY-VLINE, 2 TEXT-382, AT SY-LINSZ SY-VLINE.   " mês
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

    TAXSUMS-NFTOT        = TAXSUMS-NFTOT        + DATA-NFTOT.
    TAXSUMS-BASE         = TAXSUMS-BASE         + DATA-BASE.
    TAXSUMS-EXCBAS       = TAXSUMS-EXCBAS       + DATA-EXCBAS.
    TAXSUMS-OTHBAS       = TAXSUMS-OTHBAS       + DATA-OTHBAS.
    TAXSUMS-TAXVAL       = TAXSUMS-TAXVAL       + DATA-TAXVAL.
    TAXSUMS-OBS_IPIPAUTA = TAXSUMS-OBS_IPIPAUTA + DATA-OBS_IPIPAUTA.

* build daily totals for calculation bases and taxvalues per taxgroup

    CASE KEY-TAXGRP.
      WHEN CONST_TAXGRP-ICMS.
        TOTAL-ICMSBASE      = TOTAL-ICMSBASE    + DATA-BASE.
        TOTAL-ICMSEXCB      = TOTAL-ICMSEXCB    + DATA-EXCBAS.
        TOTAL-ICMSOTHB      = TOTAL-ICMSOTHB    + DATA-OTHBAS.
        TOTAL-ICMSVAL       = TOTAL-ICMSVAL     + DATA-TAXVAL.
      WHEN CONST_TAXGRP-IPI.
        TOTAL-IPIBASE       = TOTAL-IPIBASE     + DATA-BASE.
        TOTAL-IPIEXCB       = TOTAL-IPIEXCB     + DATA-EXCBAS.
        TOTAL-IPIOTHB       = TOTAL-IPIOTHB     + DATA-OTHBAS.
        TOTAL-IPIVAL        = TOTAL-IPIVAL      + DATA-TAXVAL.
      WHEN CONST_TAXGRP-SUBTRIB.
        TOTAL-SUBTRIBBASE   = TOTAL-SUBTRIBBASE + DATA-BASE.
        TOTAL-SUBTRIBEXCB   = TOTAL-SUBTRIBEXCB + DATA-EXCBAS.
        TOTAL-SUBTRIBOTHB   = TOTAL-SUBTRIBOTHB + DATA-OTHBAS.
        TOTAL-SUBTRIBVAL    = TOTAL-SUBTRIBVAL  + DATA-TAXVAL.
    ENDCASE.

    DOCNUM = KEY-DOCNUM.
    MOVE KEY-PSTDAT+6(2) TO OUT-DOCDAY.
    MOVE-CORRESPONDING KEY     TO OUT.
    MOVE-CORRESPONDING DATA    TO OUT.
    MOVE-CORRESPONDING TAXSUMS TO OUT.

*   only if all tax lines of tax group are statistical
*   no bases to be printed                                  "note 206361
    CLEAR OUT-STATTX.
    IF DATA-STATTX IS INITIAL.
      LINE_WITH_NONSTAT_TAX = 'X'. "one non-statistical tax line found
    ELSEIF LINE_WITH_NONSTAT_TAX IS INITIAL.
      OUT-STATTX = 'X'.          "only, if all tax lines are statistical
    ENDIF.

* fill state summary tables
*---> begin of deletion note 553750
*    if ( key-taxgrp = const_taxgrp-icms
*     or key-taxgrp = const_taxgrp-subtrib )" Suppress val. contabil for
*       and data-itmtyp ne '41'        " future delivery IV item (first
*                                      " NF in fut. del. process) w. IPI
*       and data-itmtyp ne '43'        " future delivery IV item (first
*                                      " NF in fut. del. process) no IPI
*      and data-itmtyp ne '63'         " Third party GR item supplier to
*                                      " customer (case A,B - NF2)
*       and data-itmtyp ne '64'        " Third party IV item supplier
*                                      " to seller (case B - NF0)
*       and data-itmtyp ne '51'.       " consignm. IV item, note 450610
*      clear state_summary_icms.
*      move-corresponding key to state_summary_icms.
*      move-corresponding data to state_summary_icms.
*      collect state_summary_icms.
*    endif.
*---> end of deletion note 553750
*
    IF ( KEY-TAXGRP = CONST_TAXGRP-ICMS                 "note 553750
     OR KEY-TAXGRP = CONST_TAXGRP-SUBTRIB ).            "note 553750
      READ TABLE GT_NFITMRULE INTO GS_NFITMRULE        "note 553750
           WITH KEY ITMTYP = DATA-ITMTYP.              "note 553750
      IF SY-SUBRC = 0 AND GS_NFITMRULE-IGNORETOTAL <> 'X'. " note 782912
*      if gs_nfitmrule-ignoretotal <> 'X'.    " note 571316; note 782912
        CLEAR STATE_SUMMARY_ICMS.                       "note 553750
        MOVE-CORRESPONDING KEY TO STATE_SUMMARY_ICMS.   "note 553750
        MOVE-CORRESPONDING DATA TO STATE_SUMMARY_ICMS.  "note 553750
        COLLECT STATE_SUMMARY_ICMS.                     "note 553750
      ENDIF.                                            "note 553750
    ENDIF.                                              "note 553750
*
* fill cfop_line
    IF DATA-CANCEL = ' '.
*      if key-taxgrp = const_taxgrp-icms                "note 553750
*      and data-itmtyp ne '41'                          "note 553750
*      and data-itmtyp ne '43'                          "note 553750
*      and data-itmtyp ne '63'                          "note 553750
*      and data-itmtyp ne '64'                          "note 553750
*      and data-itmtyp ne '51'.         " note 450610   "note 553750
*
      IF KEY-TAXGRP = CONST_TAXGRP-ICMS.                 "note 553750
        READ TABLE GT_NFITMRULE INTO GS_NFITMRULE        "note 553750
             WITH KEY ITMTYP = DATA-ITMTYP.              "note 553750
*       if gs_nfitmrule-ignoretotal <> 'X'.   " note 571316; note 782912
        IF SY-SUBRC = 0 AND GS_NFITMRULE-IGNORETOTAL <> 'X'. " note 782912
          MOVE-CORRESPONDING KEY  TO CFOP_LINE.
          MOVE-CORRESPONDING DATA TO CFOP_LINE.
          COLLECT CFOP_LINE.
          MOVE CFOP_LINE-CFOP_NOEXT(1) TO CFOP_LINE_TOT-CFOP_FIRST.
          MOVE-CORRESPONDING CFOP_LINE TO CFOP_LINE_TOT.
          COLLECT CFOP_LINE_TOT.
          MOVE '9' TO CFOP_LINE_TOT-CFOP_FIRST.
          MOVE-CORRESPONDING CFOP_LINE TO CFOP_LINE_TOT.
          COLLECT CFOP_LINE_TOT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

*----------------------------------------------------------------------*
*------------------------check if book is empty------------------------*
*----------------------------------------------------------------------*

  IF FIRST_RECORD = 'X'.
**    PERFORM F_FAKE_DATA.
*    WRITE:/ text-270.                                  "BOM note 768128
    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE, 30 TEXT-270, AT SY-LINSZ SY-VLINE."EOM note 768128
    "no values were selected
    ZULINE.

    CHECK '1' = '0'.
  ELSE.                                "book is not empty

*----------------------------------------------------------------------*
*--------------write last record coming from the extract---------------*
*----------------------------------------------------------------------*

    PERFORM OUTPUT USING OUT.

*    WRITE AT (SY-LINSZ) SY-ULINE.      "note 768128
    ZULINE.
*----------------------------------------------------------------------*
*---------------------write last daily total---------------------------*
*----------------------------------------------------------------------*

* 10 days totals
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

*    WRITE AT (SY-LINSZ) SY-ULINE.                     " note 768128
    ZULINE.

    PERFORM TOTAL_OUTPUT USING TOTAL.
    READ TABLE TOTAL_10 WITH KEY NUMBER = TOTAL_10-NUMBER.
    MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
    SKIP.
    WRITE 1 WG_BLANKS(28)  COLOR OFF.
*    write: /40 total_10-number, '.', text-380.        " DEL note 697076
*    perform total_output using total.                 " DEL note 697076
    IF DECENDIO = 'X'.                                 " BOI note 697076
*      WRITE AT (SY-LINSZ) SY-ULINE.
      ZULINE.
      WRITE: AT /1  WG_BLANKS(28)  COLOR OFF,
          68 TOTAL_10-NUMBER, '.', TEXT-380.    " BOM note 768128
      WRITE : AT 29 SY-VLINE,
            AT SY-LINSZ SY-VLINE.
      PERFORM TOTAL_OUTPUT USING TOTAL.
    ELSEIF QUINZENA = 'X'.
*      WRITE AT (SY-LINSZ) SY-ULINE.
      ZULINE.
      WRITE: AT /1  WG_BLANKS(28)  COLOR OFF,
          AT 68 TOTAL_10-NUMBER, '.', TEXT-381.
      WRITE : AT 29 SY-VLINE,
            AT SY-LINSZ SY-VLINE.                      "EOM note 768128

      PERFORM TOTAL_OUTPUT USING TOTAL.
    ENDIF.                                             " EOI note 697076
    SKIP.
    WRITE 1 WG_BLANKS(28)  COLOR OFF.

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
*    CIAP_TOTAL = TOTAL.       "save total for CIAP calculation
*      WRITE AT (SY-LINSZ) SY-ULINE.                        " note 768128
      ZULINE.
      WRITE : AT /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
      AT 30 TEXT-382,
      AT SY-LINSZ SY-VLINE.
      PERFORM TOTAL_OUTPUT USING TOTAL.
    ENDIF.                                             " BOI note 697076
* Calculate total geral
    READ TABLE TOTAL_10 WITH KEY NUMBER = '4'.
    TOTAL_10-NUMBER = '5'.
    COLLECT TOTAL_10.
* Output total geral
    READ TABLE TOTAL_10 WITH KEY NUMBER = '5'.
    MOVE-CORRESPONDING TOTAL_10 TO TOTAL.
    FORMAT INTENSIFIED COLOR COL_TOTAL.                 "BOM note 768128
*    WRITE AT (SY-LINSZ) SY-ULINE.
    ZULINE.
*    WRITE: text-385.
    WRITE : AT /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
      AT 30 TEXT-385,
      AT SY-LINSZ SY-VLINE.                             "EOM note 768128

    CIAP_TOTAL = TOTAL.       "save total for CIAP calculation
    PERFORM TOTAL_OUTPUT USING TOTAL.                  " EOI note 697076
    SKIP.
    WRITE 1 WG_BLANKS(28)  COLOR OFF.

*----------------------------------------------------------------------*
*----------------------write summary per cfop--------------------------*
*----------------------------------------------------------------------*

    SORT CFOP_LINE.
    SORT CFOP_LINE_TOT.

    SKIP 2.
    FORMAT INTENSIFIED COLOR COL_NORMAL.
    RESERVE 4 LINES.
    ZULINE.
*    ULINE AT (SY-LINSZ).                           "BOM note 768128
*    WRITE: /40 text-390.
    WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
            68 TEXT-390,
            AT SY-LINSZ SY-VLINE.
    FORMAT COLOR OFF.
    WRITE : /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
             AT SY-LINSZ SY-VLINE.                 "EOM note 768128

    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*    skip.                                         " note 768128

    LOOP AT CFOP_LINE_TOT WHERE CFOP_FIRST NE '9'.
      CASE CFOP_LINE_TOT-CFOP_FIRST.
        WHEN '5'.
          PERFORM DISPLAY_CFOP_SUMMARY_HEADER USING TEXT-391."nt 585040
        WHEN '6'.
          PERFORM DISPLAY_CFOP_SUMMARY_HEADER USING TEXT-392."nt 585040
        WHEN '7'.
          PERFORM DISPLAY_CFOP_SUMMARY_HEADER USING TEXT-393."nt 585040
      ENDCASE.

      LOOP AT CFOP_LINE.
        IF CFOP_LINE-CFOP_NOEXT(1) = CFOP_LINE_TOT-CFOP_FIRST.
          PERFORM CFOP_OUTPUT USING CFOP_LINE.
          DELETE CFOP_LINE INDEX SY-TABIX.
        ELSE.
*          FORMAT INTENSIFIED COLOR COL_NORMAL.      "BOM note 768128
*          WRITE:/24(19) text-395.
          FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
          WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
                 52(19) TEXT-395,
                 AT SY-LINSZ SY-VLINE.               "EOM note 768128

          CLEAR CFOP_LINE.
          MOVE-CORRESPONDING CFOP_LINE_TOT TO CFOP_LINE.
          PERFORM CFOP_OUTPUT USING CFOP_LINE.
          FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*          SKIP.
*          FORMAT RESET.
*          WRITE: /1 WG_BLANKS(28)  COLOR OFF,
*                 AT 29 SY-VLINE,
*                 AT SY-LINSZ SY-VLINE.               "EOM note 768128
          FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
          EXIT.
        ENDIF.
      ENDLOOP.                         "cfop_line.

    ENDLOOP.                           "cfop_line_tot.
*    FORMAT INTENSIFIED COLOR COL_NORMAL.            "BOM note 768128
*    WRITE:/24(19) text-395.
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
           52(19) TEXT-395,
           AT SY-LINSZ SY-VLINE.                     "EOM note 768128

    CLEAR CFOP_LINE.
    MOVE-CORRESPONDING CFOP_LINE_TOT TO CFOP_LINE.
    PERFORM CFOP_OUTPUT USING CFOP_LINE.
    FORMAT COLOR OFF.                                "BOM note 768128
*    SKIP.
*    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
*          AT 29 SY-VLINE,
*           AT SY-LINSZ SY-VLINE.                     "EOM note 768128

    READ TABLE CFOP_LINE_TOT WITH KEY CFOP_FIRST = '9'. "overall total
*    WRITE:/24(17) text-396.                         "BOM note 768128
    FORMAT INTENSIFIED COLOR COL_TOTAL.
    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
           52(17) TEXT-396,
           AT SY-LINSZ SY-VLINE.                     "EOM note 768128

    CLEAR CFOP_LINE.
    MOVE-CORRESPONDING CFOP_LINE_TOT TO CFOP_LINE.
    PERFORM CFOP_OUTPUT USING CFOP_LINE.
    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
    FORMAT COLOR OFF.                                 "BOM note 768128
*    SKIP.
*    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
*          AT 29 SY-VLINE,
*           AT SY-LINSZ SY-VLINE.
*    ULINE AT (SY-LINSZ).                              "EOM note 768128
    ZULINE.


*----------------------------------------------------------------------*
*----------------------write summary per state-------------------------*
*----------------------------------------------------------------------*

    SKIP 3.
    FORMAT INTENSIFIED COLOR COL_NORMAL.
    RESERVE 4 LINES.
*    ULINE AT (SY-LINSZ).                      " note 768128
    ZULINE.
    WRITE: /68 TEXT-360.
    WRITE: 29 SY-VLINE,                        "BOM note 768128
           AT SY-LINSZ SY-VLINE.
    FORMAT COLOR OFF.
    WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
           AT SY-LINSZ SY-VLINE.
*    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*    SKIP.                                    "EOM note 768128

    SORT STATE_SUMMARY_ICMS.

    LOOP AT STATE_SUMMARY_ICMS.
      AT NEW WITH_STAINS.
        RESERVE 4 LINES.
*        SKIP.                                "BOM note 768128
        WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
               AT SY-LINSZ SY-VLINE.          "EOM note 768128

        FORMAT INTENSIFIED COLOR COL_NORMAL.
        CASE STATE_SUMMARY_ICMS-WITH_STAINS.
          WHEN 'X'.
            WRITE: AT /1 WG_BLANKS(28)  COLOR OFF,
                   AT C_BASE TEXT-361.
            WRITE: 29 SY-VLINE,                " note 768128
                   AT SY-LINSZ SY-VLINE.
          WHEN ' '.
            WRITE: AT /1 WG_BLANKS(28)  COLOR OFF,
                   AT C_BASE TEXT-362.
            WRITE: 29 SY-VLINE,                " note 768128
                   AT SY-LINSZ SY-VLINE.
        ENDCASE.
        C_HELP = C_TAXVA + STRLEN( TEXT-366 ) - 1.
        WRITE AT C_HELP BLANK.
        WRITE: AT /1 WG_BLANKS(28)  COLOR OFF,
               AT  C_REGIO TEXT-363,
               AT  C_NFTOT TEXT-364,
               AT  C_BASE  TEXT-365,
               AT  C_TAXVA TEXT-366.

        WRITE: 29 SY-VLINE,                   "BOM note 768128
              AT SY-LINSZ SY-VLINE.
*        SKIP.
        WRITE: /1 WG_BLANKS(28)  COLOR OFF,
               AT 29 SY-VLINE,
               AT SY-LINSZ SY-VLINE.          "EOM note 768128
        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
      ENDAT.

      AT NEW REGIO.
        WRITE: AT /1 WG_BLANKS(28)  COLOR OFF,
               AT C_REGIO(L_REGIO)   STATE_SUMMARY_ICMS-REGIO.
        WRITE: 29 SY-VLINE,                     "note 768128
               AT SY-LINSZ SY-VLINE.

      ENDAT.

      WRITE: AT C_NFTOT(L_NFTOT)  STATE_SUMMARY_ICMS-NFTOT
                  CURRENCY T001-WAERS.
*                using edit mask 'RR___________,__' currency t001-waers.

      CASE STATE_SUMMARY_ICMS-TAXGRP.
        WHEN CONST_TAXGRP-ICMS.
          WRITE AT C_TAXGR TEXT-152.
          WRITE: 29 SY-VLINE,                " note 768128
                 AT SY-LINSZ SY-VLINE.
        WHEN CONST_TAXGRP-SUBTRIB.
          WRITE AT C_TAXGR TEXT-154.
          WRITE: 29 SY-VLINE,                " note 768128
                 AT SY-LINSZ SY-VLINE.

      ENDCASE.

      WRITE AT C_BASE(L_BASE) STATE_SUMMARY_ICMS-BASE
               CURRENCY T001-WAERS.
      WRITE: 29 SY-VLINE,                    " note 768128
             AT SY-LINSZ SY-VLINE.

      IF STATE_SUMMARY_ICMS-TAXGRP = CONST_TAXGRP-SUBTRIB.
        WRITE AT C_TAXVA(L_TAXVA) STATE_SUMMARY_ICMS-TAXVAL
              CURRENCY T001-WAERS.
        WRITE: 29 SY-VLINE,                   " note 768128
             AT SY-LINSZ SY-VLINE.

      ELSE.
        NEW-LINE.
      ENDIF.

    ENDLOOP.

    ZULINE.                    " note 768128

    BOOKS = BOOKS + 1.
    LAST_TOTAL = 'X'.

  ENDIF.

*----------------------------------------------------------------------*
*----------Last page of Registro de Saídas (Statistics)----------------*
*----------------------------------------------------------------------*

  NEW-PAGE NO-TITLE.
  SKIP 5.
  ZULINE.                      "BOM note 768128

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*  WRITE: / text-500.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT 30 TEXT-500,
          AT SY-LINSZ SY-VLINE.
*  SKIP 1.
  FORMAT COLOR OFF.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
         AT 29 SY-VLINE,
         AT SY-LINSZ SY-VLINE.

  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*  WRITE: / text-398.
*  WRITE 20 text-501.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT 30 TEXT-398,
          AT 50 TEXT-501,
          AT SY-LINSZ SY-VLINE.

*  WRITE: / text-399.
*  WRITE 20 text-502.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT 30 TEXT-399,
          AT 50 TEXT-502,
          AT SY-LINSZ SY-VLINE.

  ZULINE.

  SKIP.
  ZULINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*  WRITE: / text-300.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT 30 TEXT-300,
          AT SY-LINSZ SY-VLINE.

*  SKIP 3.
  FORMAT COLOR OFF.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE,
       /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE,
       /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE.

  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*  WRITE:  text-301, books.
  WRITE: 29 SY-VLINE,
       30 TEXT-301, BOOKS,
       AT SY-LINSZ SY-VLINE.

*  SKIP 3.
  FORMAT COLOR OFF.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE,
       /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE,
       /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE.

  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*  WRITE: text-302, pages.
  WRITE: 29 SY-VLINE,
       30 TEXT-302, PAGES,
       AT SY-LINSZ SY-VLINE.

*  SKIP.
  FORMAT COLOR OFF.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE.                      "EOM note 768128

* CIAP information
  PERFORM CIAP_FATOR USING CIAP_TOTAL.
  ZULINE.


*----------------------------------------------------------------------*
*---------------------------output correctionlist----------------------*
*----------------------------------------------------------------------*

  NEW-PAGE.

  CORRLIST = 'X'.
  SORT CORR.
  "BOM note 768128
  IF NOT CORR[] IS INITIAL.
    ZULINE.
  ENDIF.
  LOOP AT CORR.
*    WRITE:  /1      corr-pstdat DD/MM/YYYY,
    WRITE:  /1 WG_BLANKS(28)  COLOR OFF,
          AT 29      SY-VLINE,
          AT 30     CORR-PSTDAT DD/MM/YYYY,
          AT 50     CORR-NFNUM,
          AT 46     '-',
          AT 47     CORR-SERIES,
          AT 53     CORR-DOCNUM,
          AT 73     CORR-DOCREF.
    WRITE : AT SY-LINSZ SY-VLINE.
    IF CORR-CANCEL = 'X'.
*      WRITE 65 text-101.
      WRITE : 29 SY-VLINE, 93 TEXT-101, AT SY-LINSZ SY-VLINE.
      "EOM note 768128
    ENDIF.
  ENDLOOP.
  CORRLIST = ' '.


*----------------------------------------------------------------------*
*---------------------------output errorlist---------------------------*
*----------------------------------------------------------------------*

  NEW-PAGE.

  ERRORLIST = 'X'.

  LOOP AT T_MESG.
    WRITE / T_MESG.
  ENDLOOP.

*----------------------------------------------------------------------*
*---------------------------- Form  INI_OUTPUT ------------------------*
*----------------------------------------------------------------------*
*-------- Layout of heading: determination of starting position
*--------                    for each column
*----------------------------------------------------------------------*
FORM INI_OUTPUT.                                            "PLFK002083
  "BOM note 768128
*Positions changed because of the vertical lines introduced.
*  c_speci = 1.                         "especie
  C_SPECI = 30.                         "especie      "EOM note 768128

  C_SERIE = C_SPECI + 3 + 1.           "serie          "change +1
  C_SUBSE = C_SERIE + 3.               "subserie
  C_NFNUM = C_SERIE + 6.               "numero
  C_DOCDA = C_NFNUM + 7.               "data do documento
  C_REGIO = C_DOCDA + 4.               "UF dest
  L_REGIO = 4.                         "old:2, change +2
  C_NFTOT = C_REGIO + L_REGIO + 1.     "valor contabil
  L_NFTOT = 19. "old:14                      "old:14, change +5
  C_EMPTY = C_NFTOT + L_NFTOT + 1.     "codificacao contabil
  L_EMPTY = 3.                         "new empty column, change +3 +|
  C_CFOPN = C_EMPTY + L_EMPTY + 1.     "codificacao fiscal
  L_CFOPN = 7.                         "old: 3, change +5
  C_TAXGR = C_CFOPN + L_CFOPN + 1.
  L_TAXGR = 4.
  C_BASE  = C_TAXGR + L_TAXGR + 1.
  L_BASE  = 19.                        "old: 14, change +5
  C_RATE  = C_BASE +  L_BASE  + 1.
  L_RATE  = 6.
  C_TAXVA = C_RATE +  L_RATE  + 1.
  L_TAXVA = 19.                        "old: 14, change +5
  C_EXCBA = C_TAXVA + L_TAXVA + 1.
  L_EXCBA = 19.                        "old: 14, change +5
  C_OTHBA = C_EXCBA + L_EXCBA + 1.
  L_OTHBA = 19.                        "old: 14, change +5
  C_OBSER = C_OTHBA + L_OTHBA + 1.
  L_OBSER = SY-LINSZ - C_OBSER + 1.
ENDFORM.                    "ini_output

*----------------------------------------------------------------------*
*----------------------------Form  OUTPUT------------------------------*
*----------------------------------------------------------------------*
*----------------performs output of lineelements-----------------------*
*----------------------------------------------------------------------*
*  --> out: single outputline
*  <-- output
*----------------------------------------------------------------------*

FORM OUTPUT USING FORMOUT STRUCTURE OUT.

  DATA: REF_NUMBER LIKE J_1BINTERF-XBLNR.  "condensed NF number
  "(Number-SeriesSubseries)
  DATA: C_HELP LIKE C_OBSER.

  FORMAT INTENSIFIED COLOR COL_KEY.
  IF FORMOUT-DUPL =' '.

    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          29 SY-VLINE.                           "BOM note 768128
*    WRITE:  AT /c_speci  formout-species,
    WRITE:  AT  C_SPECI  FORMOUT-SPECIES,         "EOM note 768128
            AT  C_SERIE   FORMOUT-SERIES,
            AT  C_SUBSE   FORMOUT-SUBSER,
            AT  C_NFNUM   FORMOUT-NFNUM,
            AT  C_DOCDA   FORMOUT-DOCDAY,
            AT  C_REGIO   FORMOUT-REGIO.
*    WRITE at 170 sy-vline.              " note 768128
  ELSE.
    NEW-LINE.
    WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          29 SY-VLINE.                           " note 768128
  ENDIF.
  IF FORMOUT-CANCEL = ' '.
    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
    IF FORMOUT-TAXGRP = CONST_TAXGRP-ICMS.
      READ TABLE GT_NFITMRULE INTO GS_NFITMRULE         "note 553750
           WITH KEY ITMTYP = FORMOUT-ITMTYP.            "note 553750
*      if gs_nfitmrule-ignoretotal <> 'X'.    " note 571316; note 782912
      IF SY-SUBRC = 0 AND GS_NFITMRULE-IGNORETOTAL <> 'X'. " note 782912
* in case of second NF in third party process, a valor contabil mustn't
* be printed
        WRITE AT C_NFTOT(L_NFTOT) FORMOUT-NFTOT CURRENCY T001-WAERS.
        TOTAL-VALORCONT = TOTAL-VALORCONT + FORMOUT-NFTOT.
      ENDIF.                                            "note 553750
    ENDIF.

    SHIFT FORMOUT-CFOP_NOEXT LEFT DELETING LEADING '0'. " beg. nt 553750
    CASE CFOP_LENGTH.
      WHEN 3.
        WRITE AT C_CFOPN(L_CFOPN) FORMOUT-CFOP_NOEXT.
      WHEN 4.
        WRITE AT C_CFOPN(L_CFOPN) FORMOUT-CFOP_NOEXT(1).
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
        WRITE AT C_TAXGR(L_TAXGR) TEXT-152.
      WHEN CONST_TAXGRP-IPI.
        WRITE AT C_TAXGR(L_TAXGR) TEXT-153.
      WHEN CONST_TAXGRP-SUBTRIB.
        WRITE AT C_TAXGR(L_TAXGR) TEXT-154.
    ENDCASE.
    IF FORMOUT-STATTX = ' ' .
*     note 210543: no special treatment for ipi pauta any more
*     if formout-rectype is initial.   "IPI Pauta
      WRITE AT C_BASE(L_BASE) FORMOUT-BASE CURRENCY T001-WAERS.
*     endif.
* note 0105005
**  no output in other base case "note 0105005
*      IF FORMOUT-OTHBAS = 0.
** never print rate for IPI, and print rate only if base > 0
*        IF FORMOUT-TAXGRP NE CONST_TAXGRP-IPI AND FORMOUT-BASE > 0.
*          WRITE: 62(6) FORMOUT-RATE.
*        ENDIF.
*        IF FORMOUT-OBS_IPIPAUTA IS INITIAL
*      AND NOT FORMOUT-TAXVAL IS INITIAL.                     "IPI Pauta
*          WRITE: 69(14) FORMOUT-TAXVAL USING EDIT MASK
*                        'RR___________,__' CURRENCY T001-WAERS.
*        ENDIF.
*      ENDIF.

* non ipi
      IF FORMOUT-TAXGRP NE CONST_TAXGRP-IPI.
        IF FORMOUT-BASE > 0.
          WRITE AT C_RATE(L_RATE) FORMOUT-RATE CURRENCY T001-WAERS.
        ENDIF.
        IF NOT FORMOUT-TAXVAL IS INITIAL.
          WRITE AT C_TAXVA(L_TAXVA) FORMOUT-TAXVAL CURRENCY T001-WAERS.
        ENDIF.
      ENDIF.

* ipi
      IF FORMOUT-TAXGRP = CONST_TAXGRP-IPI  AND
         FORMOUT-OBS_IPIPAUTA IS INITIAL    AND
         NOT FORMOUT-TAXVAL IS INITIAL      AND
         ( FORMOUT-BASE > 0  OR ( FORMOUT-BASE = 0
                                  AND FORMOUT-OTHBAS = 0
                                  AND FORMOUT-EXCBAS = 0 ) ).
        WRITE AT C_TAXVA(L_TAXVA) FORMOUT-TAXVAL CURRENCY T001-WAERS.
      ENDIF.

*   note 210543: no special treatment for ipi pauta any more
*   if formout-rectype is initial.     "IPI Pauta
      WRITE: AT C_EXCBA(L_EXCBA) FORMOUT-EXCBAS CURRENCY T001-WAERS,
             AT C_OTHBA(L_OTHBA) FORMOUT-OTHBAS CURRENCY T001-WAERS.
    ENDIF.
*   endif.
  ENDIF.
  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.


  IF NOT FORMOUT-FREE_TEXT IS INITIAL.
    IF FORMOUT-DUPL = ' '.
      WRITE: AT C_OBSER(L_OBSER) FORMOUT-FREE_TEXT.
      PERFORM F_VLINE.
      IF FORMOUT-FREE_TEXT+18 CN ' '.
        WRITE:/1 WG_BLANKS(28)  COLOR OFF,
              AT C_OBSER(L_OBSER) FORMOUT-FREE_TEXT+18.
        PERFORM F_VLINE.
        IF FORMOUT-FREE_TEXT+36 CN ' '.
          WRITE:/1 WG_BLANKS(28)  COLOR OFF,
                AT C_OBSER(L_OBSER) FORMOUT-FREE_TEXT+36.
          PERFORM F_VLINE.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR FORMOUT.
    PERFORM F_VLINE.
    EXIT.
  ENDIF.

  IF FORMOUT-DUPL = ' '.
    WRITE: AT C_OBSER(L_OBSER) FORMOUT-OBS_DOC.
    IF FORMOUT-OBS_DOC+19(11) CN ' '.
      WRITE:/1 WG_BLANKS(28)  COLOR OFF,
                AT C_OBSER(L_OBSER) FORMOUT-OBS_DOC+19.
      PERFORM F_VLINE.
    ENDIF.
  ENDIF.

  IF FORMOUT-OBS_DOC IS INITIAL.
    WRITE:   AT C_OBSER(L_OBSER) FORMOUT-OBS_LIN.
    PERFORM F_VLINE.
*    WRITE at 170 sy-vline.                " note 768128
    IF FORMOUT-OBS_LIN+19(10) CN ' '.
      WRITE:/1 WG_BLANKS(28)  COLOR OFF,
                AT C_OBSER(L_OBSER) FORMOUT-OBS_LIN+19.
      PERFORM F_VLINE.
      IF FORMOUT-OBS_LIN+38(10) CN ' '.
        WRITE:/1 WG_BLANKS(28)  COLOR OFF,
                AT C_OBSER(19) FORMOUT-OBS_LIN+38.
        PERFORM F_VLINE.
        IF FORMOUT-OBS_LIN+57(10) CN ' '.
          WRITE:/1 WG_BLANKS(28)  COLOR OFF,
                AT C_OBSER(19) FORMOUT-OBS_LIN+57.
          PERFORM F_VLINE.
        ENDIF.
      ENDIF.
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
* note 670104: doc. date (instead of posting date) at observations
*        write: at /c_obser ref_number(10),j_1bnfdoc-pstdat dd/mm/yy.
        WRITE:/1 WG_BLANKS(28)  COLOR OFF,
               AT 29 SY-VLINE,
               AT C_OBSER REF_NUMBER(9),J_1BNFDOC-DOCDAT DD/MM/YY,
               AT SY-LINSZ SY-VLINE.
      ENDIF.
    ENDIF.
  ENDIF.

* ICMS freight/ST related observations
  IF FORMOUT-DUPL = ' ' AND
    ( FORMOUT-OBS_ICFRTAX NE 0 OR FORMOUT-OBS_ICFRBASE NE 0 ).
    IF NOT FORMOUT-OBS_DOC IS INITIAL OR NOT FORMOUT-OBS_LIN IS INITIAL.
      NEW-LINE.
    ENDIF.
    C_HELP = C_OBSER + 5.
    WRITE: AT C_OBSER(5) TEXT-398,
           AT C_HELP(14) FORMOUT-OBS_ICFRTAX USING EDIT MASK
                   'RR___________,__' CURRENCY T001-WAERS.
    IF FORMOUT-OBS_ICFRBASE NE 0.
      WRITE:/1   WG_BLANKS(28)  COLOR OFF,
             AT  C_OBSER(5) TEXT-399,
             AT  C_HELP(14) FORMOUT-OBS_ICFRBASE USING EDIT MASK
                   'RR___________,__' CURRENCY T001-WAERS.
    ENDIF.
  ENDIF.

  CLEAR FORMOUT.
  PERFORM F_VLINE.
ENDFORM.                               " OUTPUT

*---------------------------------------------------------------------*
*--------------------------Form  TOTAL_OUTPUT-------------------------*
*---------------------------------------------------------------------*
*----------------performs output of totallines-----------------------*
*----------------------------------------------------------------------*
*  --> total: totals per day
*  <-- output
*----------------------------------------------------------------------*

FORM TOTAL_OUTPUT USING FORMTOTAL STRUCTURE TOTAL.
  RESERVE 4 LINES.
  FORMAT INTENSIFIED COLOR COL_TOTAL.
*  SKIP.                                          "BOM note 768128
*  ULINE AT (SY-LINSZ) .     " note 768128
  ZULINE.
*  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
*          AT 29 SY-VLINE,
*          AT SY-LINSZ SY-VLINE.
*
  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE.

*  WRITE: AT /c_docda text-151,
  WRITE:  AT C_DOCDA TEXT-151,
          AT C_TAXGR(L_TAXGR) TEXT-152,
          AT C_NFTOT(L_NFTOT) FORMTOTAL-VALORCONT CURRENCY T001-WAERS,
          AT C_BASE(L_BASE)   FORMTOTAL-ICMSBASE  CURRENCY T001-WAERS,
          AT C_TAXVA(L_TAXVA) FORMTOTAL-ICMSVAL   CURRENCY T001-WAERS,
          AT C_EXCBA(L_EXCBA) FORMTOTAL-ICMSEXCB  CURRENCY T001-WAERS,
          AT C_OTHBA(L_OTHBA) FORMTOTAL-ICMSOTHB  CURRENCY T001-WAERS,

        AT /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE,
*         AT /c_taxgr(l_taxgr) text-153,
          AT C_TAXGR(L_TAXGR) TEXT-153,
          AT C_BASE(L_BASE)   FORMTOTAL-IPIBASE  CURRENCY T001-WAERS,
          AT C_TAXVA(L_TAXVA) FORMTOTAL-IPIVAL   CURRENCY T001-WAERS,
          AT C_EXCBA(L_EXCBA) FORMTOTAL-IPIEXCB  CURRENCY T001-WAERS,
          AT C_OTHBA(L_OTHBA) FORMTOTAL-IPIOTHB  CURRENCY T001-WAERS,

        AT /1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
          AT SY-LINSZ SY-VLINE,
*         AT /c_taxgr(l_taxgr) text-154,
         AT C_TAXGR(L_TAXGR) TEXT-154,
          AT C_BASE(L_BASE)   FORMTOTAL-SUBTRIBBASE CURRENCY T001-WAERS,
          AT C_TAXVA(L_TAXVA) FORMTOTAL-SUBTRIBVAL  CURRENCY T001-WAERS,
          AT C_EXCBA(L_EXCBA) FORMTOTAL-SUBTRIBEXCB CURRENCY T001-WAERS,
          AT C_OTHBA(L_OTHBA) FORMTOTAL-SUBTRIBOTHB CURRENCY T001-WAERS.

*  SKIP.
*  ULINE AT (SY-LINSZ) .
  "EOM note 768128
  ZULINE.
  CLEAR FORMTOTAL.

ENDFORM.                               " TOTAL_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CFOP_OUTPUT
*&---------------------------------------------------------------------*
*-----------performs output of lines with totals per cfop--------------*
*----------------------------------------------------------------------*
*  -->  cfop total information
*  <--  output of a line
*----------------------------------------------------------------------*

FORM CFOP_OUTPUT USING LINE STRUCTURE CFOP_LINE.

  SHIFT LINE-CFOP_NOEXT LEFT DELETING LEADING '0'.  " begin note 553750

  WRITE: AT /1 WG_BLANKS(28)  COLOR OFF,
          AT C_NFTOT(L_NFTOT) LINE-NFTOT CURRENCY T001-WAERS.
  WRITE : AT 29 SY-VLINE,                            " note 768128
        AT SY-LINSZ SY-VLINE.
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
*  IF line-cfop_ext <> '00'.
    CASE EXT_LEN.
      WHEN 1.
        IF NOT LINE-CFOP_EXT+1(1) = '0'.
          WRITE: AT C_CFOPX '.' NO-GAP, LINE-CFOP_EXT+1(1).
        ENDIF.
      WHEN 2.
        WRITE: AT C_CFOPX '.' NO-GAP, LINE-CFOP_EXT.
    ENDCASE.
  ENDIF.                                            " end of note 553750

  WRITE: AT  C_BASE(L_BASE)   LINE-BASE CURRENCY T001-WAERS,
         AT  C_TAXVA(L_TAXVA) LINE-TAXVAL CURRENCY T001-WAERS,
         AT  C_EXCBA(L_EXCBA) LINE-EXCBAS CURRENCY T001-WAERS,
         AT  C_OTHBA(L_OTHBA) LINE-OTHBAS CURRENCY T001-WAERS.

ENDFORM.                               " CFOP_OUTPUT

END-OF-PAGE.
  ZULINE.
*----------------------------------------------------------------------*
*---------------------------top-of-page--------------------------------*
*----------------------------------------------------------------------*

TOP-OF-PAGE.
  SKIP 7.
  IF ERRORLIST = ' ' AND CORRLIST = ' '.

    PAGNO = PAGNO + 1.
*    format intensified color col_heading.            " not 768128

*    check if last total ( = end of the Registro de Saídas) was reached
    IF LAST_TOTAL = ' '.

*   check maximum pages
      IF PAGNO > BOOKSIZE.
        BOOKS = BOOKS + 1.
        PAGNO = 2.
      ENDIF.
      PAGES = PAGES + 1.
*       header for Modelo 2 (Registro de Saídas)
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.      " BOM note 768128
      ZULINE.
*      WRITE:/1 WG_BLANKS(28)  COLOR OFF,
*              29 SY-ULINE(255).
      WRITE: /1 WG_BLANKS(28)  COLOR OFF,
              29 SY-VLINE,
*              31 SY-DATUM,
             AT 79(SY-LINSZ)  TEXT-200,
             AT SY-LINSZ SY-VLINE.                    "EOM note 768128
      ZULINE.

      FORMAT COLOR COL_GROUP INTENSIFIED OFF.      "BOM note 768128

      CONCATENATE ADDRESS1-NAME1 '-' BRANCH_DATA-NAME
             INTO WG_FIRMA SEPARATED BY SPACE.
      CONDENSE WG_FIRMA.

      WRITE: /1 WG_BLANKS(28)  COLOR OFF,
              29  SY-VLINE, 30 TEXT-202 COLOR COL_GROUP INTENSIFIED,
             AT 43(SY-LINSZ)  wg_firma,
             AT SY-LINSZ SY-VLINE.
      WRITE: /1 WG_BLANKS(28)  COLOR OFF,
              29  SY-VLINE, 30 TEXT-204 COLOR COL_GROUP INTENSIFIED,
             43  BRANCH_DATA-STATE_INSC,
             90  TEXT-205 COLOR COL_GROUP INTENSIFIED,
            111  CGC_NUMBER,
             AT SY-LINSZ BLANK,
             AT SY-LINSZ SY-VLINE.
      WRITE: /1 WG_BLANKS(28)  COLOR OFF,
              29  SY-VLINE, 30 TEXT-208 COLOR COL_GROUP INTENSIFIED,
             43  PAGNO,
             AT 90 TEXT-209 COLOR COL_GROUP INTENSIFIED,
             111  J5_PDATE-LOW, '-', J5_PDATE-HIGH,
             AT SY-LINSZ BLANK,
             AT SY-LINSZ SY-VLINE.
      "EOM note 768128
      ZULINE.
      PERFORM PRINT_HEADER.
      ZULINE.
    ENDIF.                             "last_total = ' '

  ELSE.                                 "errorlist = 'X' or corrlist = 'X'

    FORMAT INTENSIFIED COLOR COL_NEGATIVE.
    IF ERRORLIST = 'X'.
      WRITE: /80 TEXT-350.
    ENDIF.
    IF CORRLIST = 'X'.
      WRITE: /80 TEXT-340.
      WRITE TEXT-341.
    ENDIF.

  ENDIF.                                "errorlist = 'X' or corrlist = 'X'

*&---------------------------------------------------------------------*
*&      Form  PRINT_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_HEADER.
  DATA CONCATEXT(255).

  FORMAT COLOR COL_HEADING INTENSIFIED ON.              "BOM note 768128

*  WRITE: / text-221, '        ', text-231, text-241.
  WRITE: /1 WG_BLANKS(28)  COLOR OFF,
         29 SY-VLINE, 30 TEXT-221, '        ', TEXT-231, TEXT-241.
  WRITE AT SY-LINSZ SY-VLINE.
  CLEAR CONCATEXT.

*  CONCATENATE text-222 text-232 text-242 INTO concatext.
  CONCATENATE SY-VLINE TEXT-222 TEXT-232 TEXT-242 INTO CONCATEXT.
  FORMAT COLOR OFF.

  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
        29 CONCATEXT.
  WRITE AT SY-LINSZ SY-VLINE.
  CLEAR CONCATEXT.

*  CONCATENATE text-223 text-233 text-243 INTO concatext.
  CONCATENATE SY-VLINE TEXT-223 TEXT-233 TEXT-243 INTO CONCATEXT.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
        29 CONCATEXT.
  WRITE AT SY-LINSZ SY-VLINE.
  CLEAR CONCATEXT.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*  CONCATENATE text-224 text-234 text-244 INTO concatext.
  CONCATENATE SY-VLINE TEXT-224 TEXT-234 TEXT-244 INTO CONCATEXT.
  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
        29 CONCATEXT.
  WRITE AT SY-LINSZ SY-VLINE.
  CLEAR CONCATEXT.

*  CONCATENATE text-225 text-235 text-245 INTO concatext.
  CONCATENATE SY-VLINE TEXT-225 TEXT-235 TEXT-245 INTO CONCATEXT.
  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
        29 CONCATEXT.
  WRITE AT SY-LINSZ SY-VLINE.

  CLEAR CONCATEXT.

*  CONCATENATE text-226 text-236 text-246 INTO concatext.
  CONCATENATE SY-VLINE TEXT-226 TEXT-236 TEXT-246 INTO CONCATEXT.
  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
        29 CONCATEXT.
  WRITE AT SY-LINSZ SY-VLINE.                      "EOM note 768128

ENDFORM.                               " PRINT_HEADER
*&---------------------------------------------------------------------*
*&      Form  CIAP_FATOR
*&---------------------------------------------------------------------*
FORM CIAP_FATOR USING FORMOUT STRUCTURE TOTAL.
  DATA: TAX_FREE LIKE J_1BNFSTX-BASE,
        SUM LIKE J_1BNFSTX-BASE,
        FATOR TYPE P DECIMALS 6.

  TAX_FREE = FORMOUT-ICMSEXCB + FORMOUT-ICMSOTHB.
  SUM = FORMOUT-ICMSBASE + FORMOUT-ICMSEXCB + FORMOUT-ICMSOTHB.

  IF SUM NE 0.
    FATOR = TAX_FREE / SUM.
  ELSE.
    FATOR = 0.
  ENDIF.
*  PERFORM PRINT_FATOR USING FATOR                         " note 843108
*                            FORMOUT.                      " note 843108
  PERFORM PRINT_FATOR USING SUM TAX_FREE FATOR.            " note 843108

ENDFORM.                    " CIAP_FATOR

*&---------------------------------------------------------------------*
*&      Form  PRINT_FATOR
*&---------------------------------------------------------------------*
*FORM PRINT_FATOR USING P_FATOR                            " note 843108
*                       AMOUNT STRUCTURE TOTAL.            " note 843108
FORM PRINT_FATOR USING P_SUM P_TAXFREE P_FATOR.            " note 843108
  ZULINE.
  SKIP.                                              "BOM note 768128
  ZULINE.
  FORMAT INTENSIFIED OFF COLOR COL_HEADING.
*  WRITE: / text-600, /.
  WRITE: /29 SY-VLINE, 30 TEXT-600, AT SY-LINSZ SY-VLINE.
  FORMAT COLOR OFF.
  WRITE: /29 SY-VLINE, AT SY-LINSZ SY-VLINE.

  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
*  WRITE: / text-601, amount-icmsbase CURRENCY t001-waers.
*  WRITE: /1 sy-vline, 2 text-601, amount-icmsbase CURRENCY t001-waers,
*          AT sy-linsz sy-vline.

*  WRITE: / text-602, amount-icmsothb CURRENCY t001-waers.
*  WRITE: /1 sy-vline, 2 text-602, amount-icmsothb CURRENCY t001-waers.
  WRITE: /29 SY-VLINE, 30 TEXT-605, P_SUM  CURRENCY T001-WAERS."843108

  WRITE AT SY-LINSZ SY-VLINE.

*  WRITE: / text-603, amount-icmsexcb CURRENCY t001-waers.
*  WRITE: /1 sy-vline, 2 text-603, amount-icmsexcb CURRENCY t001-waers.
  WRITE: /29 SY-VLINE, 30 TEXT-606, P_TAXFREE CURRENCY T001-WAERS."843108

  WRITE AT SY-LINSZ SY-VLINE.

*  WRITE: / text-604, p_fator.
  WRITE: /29 SY-VLINE, 30 TEXT-604, P_FATOR.

  WRITE AT SY-LINSZ SY-VLINE.                        "EOM note 768128

  ZULINE.
ENDFORM.                    " PRINT_FATOR

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
*  WRITE:/ text_cfop_summary.                      "BOM note 768128
  WRITE:/1 WG_BLANKS(28)  COLOR OFF,
          AT 29 SY-VLINE,
           TEXT_CFOP_SUMMARY,
         AT SY-LINSZ SY-VLINE.
  "EOM note 768128
  FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
ENDFORM.                  " display_cfop_summary_header, EOI note 585040

**********************************************************************
* FORM    :  f_fake_data
* Created :  06.03.2008 00:53:06
**********************************************************************
FORM F_FAKE_DATA.

  OUT-CANCEL = ''.
  OUT-SPECIES = 'NF'.
  OUT-SERIES = '2'.
  OUT-SUBSER = ''.
  OUT-NFNUM = '1235'.
  OUT-DOCDAY = '01'.
  OUT-REGIO = 'MT'.
  OUT-NFTOT = '18613.28'.
  OUT-CFOP_NOEXT = '5102'.
  OUT-CFOP_EXT = '00'.
  OUT-TAXGRP = 'ICMS'.
  OUT-BASE = '0.00'.
  OUT-RATE = '0.00'.
  OUT-TAXVAL = '0.00'.
  OUT-EXCBAS = '18613.28'.
  OUT-OTHBAS = '0.00'.
  OUT-DUPL = ''.
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
  PERFORM OUTPUT USING OUT.
  OUT-CANCEL = ''.
  OUT-SPECIES = 'NF'.
  OUT-SERIES = 'U'.
  OUT-SUBSER = ''.
  OUT-NFNUM = '1234'.
  OUT-DOCDAY = '01'.
  OUT-REGIO = 'MT'.
  OUT-NFTOT = '419.10'.
  OUT-CFOP_NOEXT = '5102'.
  OUT-CFOP_EXT = '00'.
  OUT-TAXGRP = 'ICMS'.
  OUT-BASE = '0.00'.
  OUT-RATE = '0.00'.
  OUT-TAXVAL = '0.00'.
  OUT-EXCBAS = '419.10'.
  OUT-OTHBAS = '0.00'.
  OUT-DUPL = ''.
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
  PERFORM OUTPUT USING OUT.
  OUT-CANCEL = ''.
  OUT-SPECIES = 'NF'.
  OUT-SERIES = 'U'.
  OUT-SUBSER = ''.
  OUT-NFNUM = '1236'.
  OUT-DOCDAY = '02'.
  OUT-REGIO = 'MT'.
  OUT-NFTOT = '4191.00'.
  OUT-CFOP_NOEXT = '5102'.
  OUT-CFOP_EXT = '00'.
  OUT-TAXGRP = 'ICMS'.
  OUT-BASE = '0.00'.
  OUT-RATE = '0.00'.
  OUT-TAXVAL = '0.00'.
  OUT-EXCBAS = '4191.00'.
  OUT-OTHBAS = '0.00'.
  OUT-DUPL = ''.
  OUT-STATTX = ''.
  OUT-RECTYPE = ''.
  OUT-ITMTYP = '01'.
  OUT-OBS_DOC = ''.
  OUT-OBS_LIN = 'jksad hsjhdjskad hsakjhdskjd hksjd sakjd haskd aslk'.
  OUT-OBS_LIN = '123456789012345678901234567890123456789012345678901'.
  OUT-OBS_REF = '0000000000'.
  OUT-OBS_IPIPAUTA = '0.00'.
  OUT-OBS_ICFRBASE = '0.00'.
  OUT-OBS_ICFRTAX = '0.00'.
  OUT-FREE_TEXT = ''.
  PERFORM OUTPUT USING OUT.
  OUT-CANCEL = ''.
  OUT-SPECIES = 'NF'.
  OUT-SERIES = 'U'.
  OUT-SUBSER = ''.
  OUT-NFNUM = '1237'.
  OUT-DOCDAY = '02'.
  OUT-REGIO = 'MT'.
  OUT-NFTOT = '4191.00'.
  OUT-CFOP_NOEXT = '5102'.
  OUT-CFOP_EXT = '00'.
  OUT-TAXGRP = 'ICMS'.
  OUT-BASE = '0.00'.
  OUT-RATE = '0.00'.
  OUT-TAXVAL = '0.00'.
  OUT-EXCBAS = '4191.00'.
  OUT-OTHBAS = '0.00'.
  OUT-DUPL = ''.
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
  PERFORM OUTPUT USING OUT.

ENDFORM. "f_fake_data
*&---------------------------------------------------------------------*
*&      Form  f_vline
*&---------------------------------------------------------------------*
FORM F_VLINE .
**  EXIT.
  WRITE: 29 SY-VLINE,
         33 SY-VLINE,
         39 SY-VLINE,
         46 SY-VLINE,
         50 SY-VLINE,
         55 SY-VLINE,
         75 SY-VLINE,
         79 SY-VLINE,
         87 SY-VLINE,
         92 SY-VLINE,
        112 SY-VLINE,
        119 SY-VLINE,
        139 SY-VLINE,
        159 SY-VLINE,
        179 SY-VLINE,
        199 SY-VLINE.
ENDFORM.                    " f_vline

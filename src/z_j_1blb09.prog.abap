* Accessability
* REPORT J_1BLB09 MESSAGE-ID 8B LINE-SIZE 126 NO STANDARD PAGE HEADING.
  REPORT Z_J_1BLB09 MESSAGE-ID 8B
                    LINE-SIZE 132
                    LINE-COUNT 65(1)
                    NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
*------------------------Report J_1BLB09-------------------------------*
*----------------------------------------------------------------------*
*-------Creation of the legal book 'Registro de Apuração do ICMS'------*
*-----------------------------(Modelo 9)-------------------------------*
*----------------------------------------------------------------------*

* declaration of the nota fiscal database tables
  DATA: WG_BLANKS(132),
        WG_CANCEL,
        WG_LIFNR   LIKE LFA1-LIFNR,
        WG_NAME1   LIKE LFA1-NAME1.

  TABLES: J_1BNFDOC,                "nota fiscal header,
          J_1BINDOC,                "nota fiscal header - add. segment
         *J_1BNFDOC,                "nota fiscal header,
          J_1BNFLIN,                "nota fiscal line items,
          J_1BINLIN,                "nota fiscal line items - add. segment
          J_1BNFSTX,                "nota fiscal tax per item,
         *J_1BNFSTX,                "nota fiscal tax per item,
          J_1BAJ,                   "nota fiscal types,
          J_1BLB09,  "#EC CI_USAGE_OK[2195701]               "additional accounts for Modelo 9
          T001.                     "Company Codes

  TABLES: SKAT.

*----------------------------------------------------------------------*

* declaration of the FI-table - contains open items (used for third
* part of the book)

  TABLES: BSIS,
          BKPF.                                              " note 780594

*----------------------------------------------------------------------*

* selection of services with CFOP code                          "955768
                                                            "955768
  SELECTION-SCREEN BEGIN OF BLOCK SRV WITH FRAME TITLE TEXT-130."955768
  PARAMETERS INCL_SRV AS CHECKBOX DEFAULT 'X'.              "955768
  SELECTION-SCREEN END OF BLOCK SRV.                        "955768

*----------------------------------------------------------------------*

* definition of constants for taxgroups/taxtypes

  CONSTANTS: BEGIN OF CONST_TAXGRP,
             ICMS(4)     TYPE C VALUE 'ICMS',
             ICST(4)     TYPE C VALUE 'ICST',
             ICZONAF(4)  TYPE C VALUE 'ICZF',
             ICMF(4)     TYPE C VALUE 'ICMF',               "03.12.97
             IPI(4)      TYPE C VALUE 'IPI ',
             ICZG(4)     TYPE C VALUE 'ICZG',               "622370
             ISSS(4)     TYPE C VALUE 'ISSS',               "955768
             ISSP(4)     TYPE C VALUE 'ISSP',               "955768
             ISS(4)      TYPE C VALUE 'ISS',                "955768
             END OF CONST_TAXGRP.

* definition of constants for books

  CONSTANTS: BEGIN OF CONST_BOOK,
             ICMS(1)     TYPE N VALUE '0',
             STPROP(1)   TYPE N VALUE '1',
             STOUTR(1)   TYPE N VALUE '2',
             END OF CONST_BOOK.

  CONSTANTS: SAO_PAULO(2) VALUE 'SP'.  "abbreviation for Sao Paulo state
  CONSTANTS: SANTA_CATARINA(2) VALUE 'SC'. "abbrev. for St. Catarina state
*----------------------------------------------------------------------*

* parameters for pagenumbering and booksize

  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-100.
  PARAMETERS: FIRSTPAG(6) TYPE N OBLIGATORY.
  PARAMETERS: OBSERV AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*

* parameters for output on screen: choose which parts to be printed

  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-101.
  SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-102.
  PARAMETERS: ENTRADAS  RADIOBUTTON GROUP OUTP,
              SAIDAS    RADIOBUTTON GROUP OUTP,
              ENTRSAID  RADIOBUTTON GROUP OUTP,
              MODELO9   RADIOBUTTON GROUP OUTP DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK B3.
  SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-103.
  PARAMETERS: ICMS    AS CHECKBOX DEFAULT 'X',
              STPROP  AS CHECKBOX DEFAULT 'X',
              STOUTR  AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK B4.

  SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-T01.
  PARAMETERS: CB_COMPL AS CHECKBOX USER-COMMAND CHKBOX.
  PARAMETERS: PM_NUM(6) TYPE N          MODIF ID CPL,
              PM_DATE   LIKE SY-DATUM   MODIF ID CPL,
              PM_VALUE  LIKE BSIS-DMBTR MODIF ID CPL,
              PM_ORG(50)                MODIF ID CPL,
              PM_DELDT  LIKE SY-DATUM   MODIF ID CPL,
              PM_DELOC(50)              MODIF ID CPL.
  SELECTION-SCREEN SKIP.
  PARAMETERS: PM_ARECO  LIKE BSIS-DMBTR.       " debit saldo

  SELECTION-SCREEN END OF BLOCK B5.
  SELECTION-SCREEN END OF BLOCK B1.
*----------------------------------------------------------------------*

* key for internally used record data - contains sorting fields

  DATA: BEGIN OF KEY,
        BOOK(1)        TYPE N,                              "0: ICMS
                                               "1: Subtrib, own state
                                               "2: Subtrib, other states
        FIRST_DIGIT(1) TYPE C,                 "first digit of cfop number
*      CFOP_NOEXT(3)  TYPE N,      "cfop number without ext. note 553750
        CFOP_NOEXT(4)  TYPE N,       "cfop number without ext. note 553750
        CFOP_EXT(2)    TYPE C,                 "cfop number extension
        REGIO          LIKE J_1BINNAD-REGIO,   "region
  END OF KEY.

* internally used record data

  DATA: BEGIN OF DATA,
        NFTOT   LIKE J_1BINLIN-NFTOT,       "total value per item,
                                            "incl. ICMS/ISS, IPI, freight,
                                            "insurance, other expenses
        BASE    LIKE J_1BNFSTX-BASE,        "calculation base
        TAXVAL  LIKE J_1BNFSTX-TAXVAL,      "tax amount
        EXCBAS  LIKE J_1BNFSTX-EXCBAS,      "excluded base amount
        OTHBAS  LIKE J_1BNFSTX-OTHBAS,      "other base amount
  END OF DATA.

*----------------------------------------------------------------------*
* declaration of internally used help-fields (note 325438)

  DATA: HELP_NFTOT     LIKE J_1BINLIN-NFTOT,
        NF_WITH_ICMS   TYPE C.

*----------------------------------------------------------------------*
* declaration of internal table for nota fiscal types ( = j_1baj)

  DATA: INTJ_1BAJ LIKE J_1BAJ OCCURS 0 WITH HEADER LINE.
  RANGES: IPI_TYPES FOR J_1BAJ-TAXTYP.

*----------------------------------------------------------------------*
* declaration of internal table for additional accounts ( = j_1blb09)

  DATA: INTJ_1BLB09 LIKE J_1BLB09 OCCURS 0 WITH HEADER LINE. "#EC CI_FLDEXT_OK[2610650]

*----------------------------------------------------------------------*

* declaration of structures coming from function modules

  DATA: ADDRESS     LIKE SADR,                              "SADR40A
        PARNAD      LIKE J_1BINNAD,
        CGC_NUMBER  LIKE J_1BWFIELD-CGC_NUMBER,
        ADDRESS1    LIKE ADDR1_VAL,
        BRANCH_DATA LIKE J_1BBRANCH,
        T_MESG      LIKE MESG OCCURS 0 WITH HEADER LINE,
        WG_FIRMA(80).

*----------------------------------------------------------------------*

* definition of the field-groups for the extract

  FIELD-GROUPS: HEADER,
                EXDATA.

  INSERT KEY-BOOK
         KEY-FIRST_DIGIT
         KEY-CFOP_NOEXT
         KEY-CFOP_EXT
         KEY-REGIO
         INTO HEADER.

  INSERT DATA INTO EXDATA.

*----------------------------------------------------------------------*

* declaration of internally used help-fields

  DATA: FIRST_RECORD  TYPE C VALUE 'X',
        BOOK_PART(2)  TYPE N VALUE '1',   " 1  = Entradas,
                                          " 2  = Saídas,
                                          " 3x = other debits/credits etc.
        PAGNO(6)      TYPE N,
        H_FIRST(1)    TYPE C,             "first digit of cfop number
*      H_CFOP(3)     TYPE N,  "cfop number without extension note 553750
        H_CFOP(4)     TYPE N,   "cfop number without extension note 553750
        H_CFOP_EXT(2) TYPE C,             "cfop number extension
        H_BOOK(1)     TYPE N,
        ERRORLIST     TYPE C VALUE ' ',
        PLANTS_EXISTING TYPE C.  " nec. for branches without plant; 505845

* declaration of internally used help-table with fields that have to be
* condensed (summed up) by CFOP number

  DATA: BEGIN OF TAXSUMS,
        NFTOT   LIKE J_1BINLIN-NFTOT,
        BASE    LIKE J_1BNFSTX-BASE,
        TAXVAL  LIKE J_1BNFSTX-TAXVAL,
        EXCBAS  LIKE J_1BNFSTX-EXCBAS,
        OTHBAS  LIKE J_1BNFSTX-OTHBAS,
        END OF TAXSUMS.

* declaration of internally used help-table with fields that have to be
* condensed (summed up) by first digit of CFOP number

  DATA: BEGIN OF TOTAL OCCURS 6,
        BOOK(1)     TYPE N,
        FIRST_DIGIT LIKE KEY-FIRST_DIGIT,
        NFTOT       LIKE J_1BINLIN-NFTOT,
        BASE        LIKE J_1BNFSTX-BASE,
        TAXVAL      LIKE J_1BNFSTX-TAXVAL,
        EXCBAS      LIKE J_1BNFSTX-EXCBAS,
        OTHBAS      LIKE J_1BNFSTX-OTHBAS,
        END OF TOTAL.

*----------------------------------------------------------------------*

* declaration of output-line (first two parts of the book)

  DATA: BEGIN OF OUT,
        CFOP_NOEXT LIKE KEY-CFOP_NOEXT,
        CFOP_EXT   LIKE KEY-CFOP_EXT,
        NFTOT      LIKE J_1BINLIN-NFTOT,
        BASE       LIKE J_1BNFSTX-BASE,
        TAXVAL     LIKE J_1BNFSTX-TAXVAL,
        EXCBAS     LIKE J_1BNFSTX-EXCBAS,
        OTHBAS     LIKE J_1BNFSTX-OTHBAS,
        BOOK(1)    TYPE N,
        REGIO      LIKE J_1BINNAD-REGIO,
  END OF OUT.

*----------------------------------------------------------------------*

* data for third part of the book

  DATA: DEBIT001  LIKE J_1BNFSTX-TAXVAL, " tax debit to be transported
                                         " from second part of the book
        DEBIT002  LIKE BSIS-DMBTR,       " sum of other tax debits
        ESCRDT003 LIKE BSIS-DMBTR,       " sum of reversed tax credits
        SUBTOT004 LIKE BSIS-DMBTR,       " subtotal tax debits
        CREDIT005 LIKE J_1BNFSTX-TAXVAL, " tax credit to be transported
                                         " from first part of the book
        CREDIT006 LIKE BSIS-DMBTR,       " sum of other tax credits
        ESDEBT007 LIKE BSIS-DMBTR,       " sum of reversed tax debits
        SUBTOT008 LIKE BSIS-DMBTR,       " subtotal tax credits
        SALDO009  LIKE BSIS-DMBTR,       " tax credits previous period
        CRETOT010 LIKE BSIS-DMBTR,       " total tax credits
        SALDEB011 LIKE BSIS-DMBTR,       " debit saldo
        DEDU012   LIKE BSIS-DMBTR,       " sum tax deductions
        SALDEB013 LIKE BSIS-DMBTR,       " debit saldo
        SALCRE014 LIKE BSIS-DMBTR.       " credit saldo


  TYPES: TAB_NFITMRULE TYPE STANDARD TABLE OF               "note 553750
         J_1BNFITMRULE WITH DEFAULT KEY.                    "note 553750
  DATA:  GT_NFITMRULE TYPE TAB_NFITMRULE,                   "note 553750
         GS_NFITMRULE TYPE J_1BNFITMRULE,                   "note 553750
         CFOP_VERSION TYPE J_1BCFOP_VER,                    "note 553750
         EXT_LEN      TYPE J_1BCFOP_EXTLEN,                 "note 553750
         CFOP_LENGTH  TYPE J_1BCFOP_LEN,                    "note 553750
         C_CFOPN      TYPE I,                               "note 553750
         C_CFOPX      TYPE I.                               "note 553750

  RANGES: PLANTS FOR T001W-WERKS.
  DATA:  REP_DATE_HIGH        TYPE D.                 " note 617905

* BADI definition for legal books                             nt. 663255
  CLASS CL_EX_BADI_J_1BLEGALREPORT DEFINITION LOAD.           " nt. 663255
  DATA IF_EX_BADI_J_1BLEGALREPORT TYPE REF TO IF_EX_BADI_J_1BLEGALREPORT.

  AT SELECTION-SCREEN OUTPUT.

    LOOP AT SCREEN.
      CHECK SCREEN-GROUP1 = 'CPL'.
      IF CB_COMPL IS INITIAL.
        SCREEN-ACTIVE = '0'.
      ELSE.
        SCREEN-ACTIVE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
*----------------------------------------------------------------------*
*--------------------at selection-screen-------------------------------*
*----------------------------------------------------------------------*

  AT SELECTION-SCREEN.

    CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
         EXPORTING
              BRANCH      = J5_BRNCH
              BUKRS       = J5_BUKRS
         IMPORTING
*            address     = address
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

* Filling the upper limit of the selected period if not already filled
    IF J5_PDATE-HIGH IS INITIAL.
      REP_DATE_HIGH = J5_PDATE-LOW.
    ELSE.
      REP_DATE_HIGH = J5_PDATE-HIGH.
    ENDIF.

*----------------------------------------------------------------------*
*---------------------start-of-selection-------------------------------*
*----------------------------------------------------------------------*
    DEFINE ZULINE.
      WRITE:/1 WG_BLANKS(5)  COLOR OFF,
              6 SY-ULINE(255).
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

    LOOP AT INTJ_1BAJ WHERE TAXGRP = CONST_TAXGRP-IPI.
      IPI_TYPES-SIGN   = 'I'.
      IPI_TYPES-OPTION = 'EQ'.
      IPI_TYPES-LOW    = INTJ_1BAJ-TAXTYP.
      APPEND IPI_TYPES.
    ENDLOOP.

    SELECT * FROM J_1BNFITMRULE INTO TABLE GT_NFITMRULE."note 553750

*----------------------------------------------------------------------*
*-----------------------fill table intj_1blb09-------------------------*
*----------------------------------------------------------------------*

    IF MODELO9 = 'X'.

      SELECT * FROM J_1BLB09 INTO TABLE INTJ_1BLB09 ORDER BY PRIMARY KEY. "#EC CI_FLDEXT_OK[2610650] "#EC CI_USAGE_OK[2195701]

      IF SY-SUBRC NE 0.
        MESSAGE A462 WITH 'j_1blb09'.
      ENDIF.

    ENDIF.

*   get all plants, that are assigned to this branch

    CLEAR: PLANTS_EXISTING.                  " note 505845
    PLANTS-OPTION = 'EQ'.
    PLANTS-SIGN   = 'I'.
    SELECT WERKS  FROM  T001W INTO PLANTS-LOW
           WHERE  J_1BBRANCH  = J5_BRNCH.
      APPEND PLANTS.
      PLANTS_EXISTING = 'X'.                 " note 505845
    ENDSELECT.

*--------------------------------------------------- BOI note 663255 --*
* build an instance of the object -------------------------------------*
*----------------------------------------------------------------------*
*  IF OBSERV = 'X'.
    CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
      CHANGING
        INSTANCE = IF_EX_BADI_J_1BLEGALREPORT.
*  ENDIF.                                           " EOI note 663255

*----------------------------------------------------------------------*
*-----------First two parts of the book: Entradas and Saídas-----------*
*--------------Data coming from the Nota Fiscal Database---------------*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*----------------------------get j_1bnfdoc-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BNFDOC.

    IF ENTRADAS = 'X'.
      CHECK J_1BNFDOC-DIRECT = '1'.
    ENDIF.

    IF SAIDAS = 'X'.
      CHECK J_1BNFDOC-DIRECT = '2'.
    ENDIF.

* exclude documents with external document number (NF number) '000000'

    CHECK J_1BNFDOC-NFNUM NE '000000'.

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

* exclude cancelled docmuents

    CHECK J_1BNFDOC-CANCEL = ' '.

* exclude documents that were only created to cancel another document
* indicator: documenttype (j_1bnfdoc-doctyp) = 5

    CHECK J_1BNFDOC-DOCTYP NE '5'.

* don't report corrections
    CHECK J_1BNFDOC-DOCTYP NE '3'.

    CLEAR PARNAD.

* determine data of partner for currently processed fiscal document,
* needed for second and third Modelo 9 book, containing the Substituição
* tributária data

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

*----------------------------------------------------------------------*
*----------------------------get j_1bnflin-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BNFLIN.

* Check in case of ISS-taxed service                            "955768
    IF J_1BNFLIN-TMISS = 'X'.                               "955768
      CHECK J_1BNFLIN-CFOP <> ' '.                          "955768
      CHECK INCL_SRV = 'X'.                                 "955768
    ENDIF.                                                  "955768

* second NF in third party process - itemtype 63 - (third party GR item
* supplier to customer), Third party IV item supplier to seller - item-
* type 64 - (case B - NF0) and future delivery IV item - itemtype 41 -
* (first NF in fut. del. process) are reported without any ICMS values
* in Modelo 1 and 2. - WILL THEREFORE NOT BE REPORTED HERE
* To deal with the case 'Future delivery invoiving without IPI',
* itemtype 43 has been introduced, which behaves (in terms of reporting)
* just like itemtype 41

*  CHECK J_1BNFLIN-ITMTYP NE '41'.                  "note 553750
*  CHECK J_1BNFLIN-ITMTYP NE '43'.                  "note 553750
*  CHECK J_1BNFLIN-ITMTYP NE '63'.                  "note 553750
*  CHECK J_1BNFLIN-ITMTYP NE '64'.                  "note 553750
*---> was ist mit 51???
    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE         "note 553750
         WITH KEY ITMTYP = J_1BNFLIN-ITMTYP.          "note 553750
    IF   GS_NFITMRULE-IGNORETOTAL = 'X'               "note 553750
*--> ist beim 51er = 1 bei allen anderen mit ignoretotal = x ist
*--> checkdir initial.
    AND  GS_NFITMRULE-CHECKDIR    = ' '. "51          "note 553750
      REJECT.                                         "note 553750
    ENDIF.                                            "note 553750

    CLEAR: KEY-FIRST_DIGIT,
           KEY-CFOP_NOEXT,
           KEY-CFOP_EXT.
*----------------------------------------------------------------------*
*----------------------------get j_1binlin-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BINLIN.

    CLEAR: DATA-NFTOT, HELP_NFTOT.          " note 450610

* convert CFOP for SC into 6 digit form (e.g. j917a into 19917a)
    IF J_1BNFLIN-CFOP(1) CN '0123456789'.
      WRITE J_1BNFLIN-CFOP TO J_1BNFLIN-CFOP.
    ENDIF.

    MOVE J_1BNFLIN-CFOP+0(1) TO KEY-FIRST_DIGIT.
    CHECK KEY-FIRST_DIGIT CA '123567'.
*  MOVE J_1BNFLIN-CFOP+0(3) TO KEY-CFOP_NOEXT.             " note 553750
    MOVE J_1BNFLIN-CFOP+0(CFOP_LENGTH) TO KEY-CFOP_NOEXT.    " note 553750

* determine, if new reporting for cfop codes X99 must be used
* decreto is valid for Sao Paulo state from 01.01.2001 onwards
*    IF ( J_1BNFLIN-CFOP+1(3) = '991' OR          " BEGIN OF NOTE 553750
*         J_1BNFLIN-CFOP+1(3) = '999'    ) AND         "X99.9
*       ADDRESS1-REGION = SAO_PAULO  AND
*       J_1BNFDOC-PSTDAT > '20001231'.
*         MOVE J_1BNFLIN-CFOP+3(1) TO KEY-CFOP_EXT.
*    ENDIF.
    IF ( J_1BNFLIN-CFOP+1(3) = '991' OR J_1BNFLIN-CFOP+1(3) = '999' )
        AND ADDRESS1-REGION = SAO_PAULO AND EXT_LEN > 0.   " note 586120
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.
*   determine, if new reporting for Santa Catarina must be used
*   determine, if new reporting for Santa Catarina must be used
*    IF ( J_1BNFLIN-CFOP(3) = '199' OR J_1BNFLIN-CFOP(3) = '299' OR
*         J_1BNFLIN-CFOP(3) = '599' OR J_1BNFLIN-CFOP(3) = '699' ) AND
*       ADDRESS1-REGION = SANTA_CATARINA AND
*       J_1BNFDOC-PSTDAT > '20001231'.
*         MOVE J_1BNFLIN-CFOP+3(2) TO KEY-CFOP_EXT.
*    ENDIF.
    IF J_1BNFLIN-CFOP+1(2) = '99'
          AND ADDRESS1-REGION = SANTA_CATARINA AND EXT_LEN > 0."nt586120
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.                                          " END OF NOTE 553750

    MOVE-CORRESPONDING J_1BINLIN TO DATA.

    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE        "note 553750
         WITH KEY ITMTYP = J_1BNFLIN-ITMTYP.         "note 553750
    IF  GS_NFITMRULE-IGNORETOTAL IS INITIAL.         "note 553750
*del  if J_1BNFLIN-itmtyp NE '51'.   " note 450610 "note 553750
      MOVE DATA-NFTOT TO HELP_NFTOT.
    ENDIF.                              " note 450610
    CLEAR NF_WITH_ICMS.

*----------------------------------------------------------------------*
*----------------------------get j_1bnfstx-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BNFSTX.

*--> Do not consider incoming tax items for        "622370
*--> Zona Franca offsetting                        "622370
    CHECK J_1BNFSTX-TAXTYP <> CONST_TAXGRP-ICZG.            "622370

    CLEAR: KEY-BOOK,
           KEY-REGIO,
           DATA-BASE,
           DATA-TAXVAL,
           DATA-EXCBAS,
           DATA-OTHBAS.

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

* select only items with ICMS tax values

    CHECK INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS
       OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICST
* select also items with ISS tax (services)                     "955768
       OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISSS              "955768
       OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISSP              "955768
       OR INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ISS.              "955768

    CHECK J_1BNFSTX-TAXTYP NE CONST_TAXGRP-ICMF.            "03.12.97

    IF INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS.
      KEY-BOOK = CONST_BOOK-ICMS.
    ELSE.                            "intj_1baj-taxgrp = const_taxgrp-icst
      IF PARNAD-REGIO = ADDRESS1-REGION.
        CHECK KEY-FIRST_DIGIT = '1' OR KEY-FIRST_DIGIT = '5'.
        KEY-BOOK = CONST_BOOK-STPROP.
      ELSE.
        H_FIRST = KEY-FIRST_DIGIT.
        H_CFOP = KEY-CFOP_NOEXT.
        H_CFOP_EXT = KEY-CFOP_EXT.
        IF KEY-FIRST_DIGIT CA '123'.
          KEY-FIRST_DIGIT = '2'.
        ELSE.
          KEY-FIRST_DIGIT = '6'.
        ENDIF.
        CLEAR KEY-CFOP_NOEXT.
        KEY-REGIO = PARNAD-REGIO.
        KEY-BOOK = CONST_BOOK-STOUTR.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING J_1BNFSTX TO DATA.

* Reporting of ISS-taxed services                               "955768
    IF J_1BNFLIN-TMISS = 'X'.                               "955768
      KEY-BOOK = 0.                   "ISS to be reported as ICMS "955768
      CLEAR DATA-TAXVAL.                                    "955768
      IF DATA-BASE <> 0.                                    "955768
        DATA-EXCBAS = DATA-EXCBAS + DATA-BASE.              "955768
        CLEAR DATA-BASE.                                    "955768
      ENDIF.                                                "955768
      IF DATA-OTHBAS <> 0.                                  "955768
        DATA-EXCBAS = DATA-EXCBAS + DATA-OTHBAS.            "955768
        CLEAR DATA-OTHBAS.                                  "955768
      ENDIF.                                                "955768
    ENDIF.                                                  "955768

* begin of Note 325438
* ICMS: valor contabil only to be extracted once
* ICZF: no reporting of valor contabil
* ICST: reporting of valor contabil
    IF ( INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS
                 OR  J_1BNFLIN-TMISS = 'X' )                "955768
                 AND NF_WITH_ICMS IS INITIAL
                 AND J_1BNFSTX-TAXTYP NE CONST_TAXGRP-ICZONAF.

      DATA-NFTOT = HELP_NFTOT.
      NF_WITH_ICMS = 'X'.
    ELSE.
      CLEAR DATA-NFTOT.
    ENDIF.

    IF INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICST.
      DATA-NFTOT = HELP_NFTOT.
    ENDIF.
* end of Note 325438

    IF J_1BNFSTX-STATTX = 'X'.
      CLEAR DATA-TAXVAL.
      CLEAR DATA-BASE.
      CLEAR DATA-EXCBAS.
      CLEAR DATA-OTHBAS.
    ENDIF.

* tax-credit = 0 for non-deductible taxes
* old rule: taxvalue = 0, if otherbase > 0
* new rule: taxvalue = 0, if base = 0.
*  if j_1bnfstx-othbas > 0.
    IF J_1BNFSTX-BASE = 0.
      DATA-TAXVAL = 0.
    ENDIF.

* All bases = 0 case:
* tax-credit > 0 for deductible taxes if ALL BASES = 0 and if a line
* reference to another NF exists: then the tax base values of the refe-
* rence NF are used to determine if 'BASE > 0'. In this case, also the
* tax amount of the actual document is deductible (e.g. Complementar
* of IPI)
    IF J_1BNFSTX-BASE   = 0            AND
       J_1BNFSTX-EXCBAS = 0            AND
       J_1BNFSTX-OTHBAS = 0            AND
       J_1BNFSTX-TAXVAL <> 0           AND
       NOT J_1BNFLIN-DOCREF IS INITIAL AND
       NOT J_1BNFLIN-ITMREF IS INITIAL.
*     read taxes of original document
      SELECT SINGLE * FROM *J_1BNFSTX
                    WHERE DOCNUM = J_1BNFLIN-DOCREF
                    AND   ITMNUM = J_1BNFLIN-ITMREF
                    AND   TAXTYP = J_1BNFSTX-TAXTYP.
*     if reference data was found, check the 'other base'
      IF SY-SUBRC = 0 AND *J_1BNFSTX-BASE > 0
                      AND J_1BNFSTX-STATTX NE 'X'.
        DATA-TAXVAL = J_1BNFSTX-TAXVAL.
      ENDIF.
    ENDIF.                     " j_1bnfstx-base   = 0            and  ...

* note 526526: for intrastate transactions between two branches the ICMS
* taxvalue is reported
    IF J_1BNFSTX-BASE   = 0                 AND
       J_1BNFSTX-EXCBAS = 0                 AND
       J_1BNFSTX-OTHBAS = 0                 AND
*      J_1BNFSTX-TAXVAL <> 0                AND       " note 771235
       J_1BNFSTX-TAXVAL > 0                 AND        " note 771235
       J_1BNFSTX-TAXTYP NE CONST_TAXGRP-ICZONAF AND    " note 771235
       J_1BNFSTX-STATTX <> 'X'              AND   " corr note 771235
       INTJ_1BAJ-TAXGRP = CONST_TAXGRP-ICMS.
      IF J_1BNFDOC-DIRECT = '1'.
        DATA-TAXVAL = J_1BNFSTX-TAXVAL.
      ELSE.
        IF J_1BNFDOC-PARTYP = 'B'           AND
           ADDRESS1-REGION  = PARNAD-REGIO.
          DATA-TAXVAL = J_1BNFSTX-TAXVAL.
        ENDIF.
      ENDIF.
    ENDIF.



* do not print bases < 0
    IF DATA-EXCBAS < 0.
      CLEAR DATA-EXCBAS.
    ENDIF.
    IF DATA-BASE < 0.
      CLEAR DATA-BASE.
    ENDIF.
    IF DATA-OTHBAS < 0.
      CLEAR DATA-OTHBAS.
    ENDIF.

* create extract exdata

    EXTRACT EXDATA.

* readjust key-first_digit, key-cfop_noext in case of subtrib/interstate
    IF KEY-BOOK = CONST_BOOK-STOUTR.
      KEY-FIRST_DIGIT = H_FIRST.
      KEY-CFOP_NOEXT = H_CFOP.
      KEY-CFOP_EXT   = H_CFOP_EXT.
    ENDIF.


*----------------------------------------------------------------------*
*-----------------------end-of-selection-------------------------------*
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
*----------------------------------------------------------------------*
*------------------------------- sort ---------------------------------*
*------------------ the extract according to the key ------------------*
*----------------------------------------------------------------------*

    SORT.

*----------------------------------------------------------------------*
*-------------------------- First book: ICMS --------------------------*
*----- condense records if cfops correspond;                      -----*
*-------------------- write the condensed record ----------------------*
*----------------------------------------------------------------------*

    IF ICMS = 'X'.
      H_BOOK = CONST_BOOK-ICMS.
      PERFORM CREATE_BOOK USING CONST_BOOK-ICMS.
    ENDIF.

*----------------------------------------------------------------------*
*--------------------- 2nd book: ICMS - intrastata---------------------*
*----- condense records if cfops (without extensions) correspond; -----*
*-------------------- write the condensed record ----------------------*
*----------------------------------------------------------------------*

    IF STPROP = 'X'.
      H_BOOK = CONST_BOOK-STPROP.
      NEW-PAGE.
      PERFORM CREATE_BOOK USING CONST_BOOK-STPROP.
    ENDIF.

*----------------------------------------------------------------------*
*----------------- Third book: ICST - interstate ----------------------*
*------------ condense records if regions correspond; -----------------*
*-------------------- write the condensed record ----------------------*
*----------------------------------------------------------------------*

    IF STOUTR = 'X'.
      H_BOOK = CONST_BOOK-STOUTR.
      NEW-PAGE.
      PERFORM CREATE_BOOK USING CONST_BOOK-STOUTR.
    ENDIF.

*----------------------------------------------------------------------*
*---------------------------output errorlist---------------------------*
*----------------------------------------------------------------------*
    PERFORM F_COMPL_INFO.
    NEW-PAGE.

    ERRORLIST = 'X'.
    ZULINE.
    LOOP AT T_MESG.
* boi accessability
* WRITE / T_MESG.
      FORMAT INTENSIFIED OFF COLOR COL_POSITIVE.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 T_MESG, 132 SY-VLINE.
      FORMAT COLOR OFF.
* eoi accessability

    ENDLOOP.
    ZULINE.


*&---------------------------------------------------------------------*
*&      Form  CREATE_BOOK
*&---------------------------------------------------------------------*
*   creates one of the different books that Modelo 9 is consisting of:
*   - ICMS information
*   - Substituição tributária - own state - information
*   - Substituição tributária - other states - information
*----------------------------------------------------------------------*
*  --> book
*  <-- output
*----------------------------------------------------------------------*

  FORM CREATE_BOOK USING BOOK.

    BOOK_PART = '1'.
    FIRST_RECORD  = 'X'.
    CLEAR: TAXSUMS,
           TOTAL,
           CREDIT005,
           DEBIT001.
    REFRESH TOTAL.

*----------------------------------------------------------------------*
*-------------------------------- loop --------------------------------*
*----------------------------------------------------------------------*

    LOOP.

      CHECK KEY-BOOK = BOOK.

*----------------------------------------------------------------------*
*-------------------------- at new key-regio---------------------------*
*----------------------------------------------------------------------*

* write condensed line

      AT NEW KEY-REGIO.
        IF FIRST_RECORD  = ' '.
          PERFORM OUTPUT USING OUT.
          CLEAR TAXSUMS.
        ENDIF.
      ENDAT.

*----------------------------------------------------------------------*
*-------------------------- at new key-first_digit---------------------*
*----------------------------------------------------------------------*

* move totals to table total
* write totals depending on key-first_digit

      AT NEW KEY-FIRST_DIGIT.
        IF FIRST_RECORD  = ' '.
          APPEND TOTAL.
*     write totals for key-first_digit = 1, 2, 3
*     determine tax credit to be transported to book_part 32 (field 005)
          IF KEY-FIRST_DIGIT CA '567' AND BOOK_PART = '1'.
            PERFORM TOTAL_OUTPUT TABLES TOTAL
                                 USING BOOK_PART
                                 CHANGING CREDIT005.
            MOVE '2' TO BOOK_PART.
            NEW-PAGE.
          ENDIF.
        ELSE.                        "first_record = 'X'...
          IF KEY-FIRST_DIGIT CA '567'.  "...and is a Saída
* no incoming movements, but outgoing ones were selected - print 'dummy'
* incoming movements page with remark 'sem movimento'
* boi accessability
*           FORMAT INTENSIFIED COLOR COL_KEY.
*           WRITE:/ TEXT-270.
            PERFORM F_FAKE_DATA.
            FORMAT INTENSIFIED OFF COLOR COL_POSITIVE.
            WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-270, 132 SY-VLINE.
            FORMAT COLOR OFF.
            ZULINE.
* eoi accessability
            NEW-PAGE.
            MOVE '2' TO BOOK_PART.
          ENDIF.
        ENDIF.
        CLEAR TOTAL.
        FIRST_RECORD     = ' '.
      ENDAT.

*----------------------------------------------------------------------*

*   condense on documento fiscal-level per cfop

      TAXSUMS-NFTOT  = TAXSUMS-NFTOT  + DATA-NFTOT.
      TAXSUMS-BASE   = TAXSUMS-BASE   + DATA-BASE.
      TAXSUMS-TAXVAL = TAXSUMS-TAXVAL + DATA-TAXVAL.
      TAXSUMS-EXCBAS = TAXSUMS-EXCBAS + DATA-EXCBAS.
      TAXSUMS-OTHBAS = TAXSUMS-OTHBAS + DATA-OTHBAS.

      MOVE-CORRESPONDING KEY     TO OUT.
      MOVE-CORRESPONDING TAXSUMS TO OUT.
      MOVE               BOOK    TO OUT-BOOK.

*   build totals per first digit of cfop number

      MOVE BOOK            TO TOTAL-BOOK.
      MOVE KEY-FIRST_DIGIT TO TOTAL-FIRST_DIGIT.
      TOTAL-NFTOT  = TOTAL-NFTOT  + DATA-NFTOT.
      TOTAL-BASE   = TOTAL-BASE   + DATA-BASE.
      TOTAL-TAXVAL = TOTAL-TAXVAL + DATA-TAXVAL.
      TOTAL-EXCBAS = TOTAL-EXCBAS + DATA-EXCBAS.
      TOTAL-OTHBAS = TOTAL-OTHBAS + DATA-OTHBAS.

    ENDLOOP.

*--------------write last record coming from the extract---------------*

* perform output only if there was any movement at all
    IF FIRST_RECORD NE 'X'.
      PERFORM OUTPUT USING OUT.
    ENDIF.

*----------------------------------------------------------------------*
*-------------------------write last totals----------------------------*
*-------------for the first two parts of the book    and---------------*
*--determine tax debit to be transported to book_part 31 (field 001)---*
*-------- or to book_part 32 (field 005) - change on 30.04.97 ---------*
*----------------------------------------------------------------------*

* perform output only if there was any movement at all
    IF FIRST_RECORD NE 'X'.
      APPEND TOTAL.
      CASE BOOK_PART.
        WHEN '1'.
          PERFORM TOTAL_OUTPUT TABLES TOTAL
                               USING BOOK_PART
                               CHANGING CREDIT005.
        WHEN '2'.
          PERFORM TOTAL_OUTPUT TABLES TOTAL
                               USING BOOK_PART
                               CHANGING DEBIT001.
      ENDCASE.
    ENDIF.

* no outgoing movements, but incoming ones were selected - print 'dummy'
* outgoing movements page with remark 'sem movimento'
* or: no movements at all were selected  - print 'dummy' outgoing and
* incoming movements pages with remark 'sem movimento'

    IF BOOK_PART = '1'.        "i.e. no Saídas
      FORMAT INTENSIFIED COLOR COL_KEY.
      IF FIRST_RECORD = 'X'.  "and no Entradas
* boi accessability
*       WRITE:/ TEXT-270.
        PERFORM F_FAKE_DATA.
        FORMAT INTENSIFIED OFF COLOR COL_POSITIVE.
        WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-270, 132 SY-VLINE.
        FORMAT COLOR OFF.
        ZULINE.
* eoi accessability
      ENDIF.
      NEW-PAGE.
      BOOK_PART = '2'.
* boi accessability
*    WRITE:/ TEXT-270.
      FORMAT INTENSIFIED OFF COLOR COL_POSITIVE.
      PERFORM F_FAKE_DATA.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-270, 132 SY-VLINE.
      FORMAT COLOR OFF.
      ZULINE.
* eoi accessability
      NEW-PAGE.
      FORMAT INTENSIFIED COLOR COL_TOTAL.
    ENDIF.

*----------------------------------------------------------------------*
*-----------------------Third part of the book:------------------------*
*----------------------------------------------------------------------*

    IF MODELO9 = 'X'.
      PERFORM THIRD_PART USING BOOK.
    ENDIF.

* print empty forms, if selected on selection screen
    IF OBSERV = 'X'.
      PERFORM INFORMACOES_COMPLEMENTARES.
    ENDIF.
  ENDFORM.                    " CREATE_BOOK

*----------------------------------------------------------------------*
*----------------------------Form OUTPUT ------------------------------*
*----------------------------------------------------------------------*
*----------------performs output of lineelements-----------------------*
*----------------------------------------------------------------------*
*  --> formout: single outputline
*  <-- output
*----------------------------------------------------------------------*

  FORM OUTPUT USING FORMOUT STRUCTURE OUT.
    C_CFOPN = 13.                                " begin of note 553750
    FORMAT INTENSIFIED COLOR COL_KEY.

    SHIFT FORMOUT-CFOP_NOEXT LEFT DELETING LEADING '0'.
    CASE FORMOUT-BOOK.
      WHEN CONST_BOOK-STOUTR.
        WRITE: /31(5)    FORMOUT-REGIO.
        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
      WHEN OTHERS.
        CASE CFOP_LENGTH.
* bod accessability
*       WHEN 3.
*         WRITE:  /13       FORMOUT-CFOP_NOEXT.
*       WHEN 4.
*         WRITE:  /13       FORMOUT-CFOP_NOEXT(1).
*         WRITE: AT 14 '.' NO-GAP, FORMOUT-CFOP_NOEXT+1(3).
* eod accessability
* boi accessability
          WHEN 3.
            WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 19 FORMOUT-CFOP_NOEXT.
          WHEN 4.
            WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 19 FORMOUT-CFOP_NOEXT(1).
            WRITE: AT 20 '.' NO-GAP, FORMOUT-CFOP_NOEXT+1(3).
* eoi accessability
        ENDCASE.

        IF NOT FORMOUT-CFOP_EXT IS INITIAL.
          C_CFOPX = C_CFOPN + CFOP_LENGTH.
          CASE EXT_LEN.
            WHEN 1.
              WRITE: AT C_CFOPX '.' NO-GAP, FORMOUT-CFOP_EXT+1(1).
            WHEN 2.
              WRITE: AT C_CFOPX '.' NO-GAP, FORMOUT-CFOP_EXT(2).
          ENDCASE.
        ENDIF.                                         " end of note 553750

        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
        WRITE: 31(20)   FORMOUT-NFTOT CURRENCY T001-WAERS.
    ENDCASE.

    WRITE:   51(20)   FORMOUT-BASE CURRENCY T001-WAERS,
             71(20)   FORMOUT-TAXVAL CURRENCY T001-WAERS,
             91(21)   FORMOUT-EXCBAS CURRENCY T001-WAERS,
             112(20)  FORMOUT-OTHBAS CURRENCY T001-WAERS.
    WRITE: 132 SY-VLINE.                                 " accessability
    CLEAR FORMOUT.
    WRITE: 1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE,
          18 SY-VLINE,
          31 SY-VLINE,
          51 SY-VLINE,
          71 SY-VLINE,
          91 SY-VLINE,
         112 SY-VLINE,
         132 SY-VLINE.
  ENDFORM.                               " OUTPUT

*----------------------------------------------------------------------*
*--------------------------Form  TOTAL_OUTPUT--------------------------*
*----------------------------------------------------------------------*
*----------------performs output of totallines-------------------------*
*----------------------------------------------------------------------*
*  --> total: table with totals per first cfop digit,
*      book_part: '1'  = first part:  Entradas,
*                 '2'  = second part: Saídas,
*                 '3x' = other transactions
*  <-- output depending on the direction
*----------------------------------------------------------------------*

  FORM TOTAL_OUTPUT TABLES FORMTOTAL STRUCTURE TOTAL
                    USING  FORMBOOK_PART
                    CHANGING VALUE(TRANSPORT) LIKE J_1BNFSTX-TAXVAL.

    DATA: GRAND_TOTAL LIKE TOTAL.

    CLEAR: GRAND_TOTAL,
           TRANSPORT.

    GRAND_TOTAL-BOOK = FORMTOTAL-BOOK.
    RESERVE 5 LINES.
* boi accessability
* FORMAT INTENSIFIED COLOR COL_TOTAL.
* SKIP.
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    ZULINE.
* eoi accessability

    CASE FORMBOOK_PART.
      WHEN '1'.
        IF FORMTOTAL-BOOK NE CONST_BOOK-STOUTR.
*    WRITE: /1      TEXT-150.                     " accessability
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-150, 132 SY-VLINE." accessability
        ENDIF.
        LOOP AT FORMTOTAL.
          IF FORMTOTAL-BOOK NE CONST_BOOK-STOUTR.
            CASE FORMTOTAL-FIRST_DIGIT.
              WHEN '1'.
*          WRITE: /1 TEXT-151.                    " accessability
                WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-151.        " accessability
                PERFORM WRITE_TOTALS USING FORMTOTAL.
              WHEN '2'.
*          WRITE: /1 TEXT-152.                    " accessability
                WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-152.        " accessability
                PERFORM WRITE_TOTALS USING FORMTOTAL.
              WHEN '3'.
*          WRITE: /1 TEXT-153.                    " accessability
                WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-153.        " accessability
                PERFORM WRITE_TOTALS USING FORMTOTAL.
            ENDCASE.
          ENDIF.
          GRAND_TOTAL-NFTOT  = GRAND_TOTAL-NFTOT  + FORMTOTAL-NFTOT.
          GRAND_TOTAL-BASE   = GRAND_TOTAL-BASE   + FORMTOTAL-BASE.
          GRAND_TOTAL-TAXVAL = GRAND_TOTAL-TAXVAL + FORMTOTAL-TAXVAL.
          GRAND_TOTAL-EXCBAS = GRAND_TOTAL-EXCBAS + FORMTOTAL-EXCBAS.
          GRAND_TOTAL-OTHBAS = GRAND_TOTAL-OTHBAS + FORMTOTAL-OTHBAS.
        ENDLOOP.

*     write grand_total
        FORMAT INTENSIFIED ON COLOR COL_TOTAL.        " accessability
*   WRITE: /1 TEXT-154.                           " accessability
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-154.               " accessability
        PERFORM WRITE_TOTALS USING GRAND_TOTAL.

      WHEN '2'.
        IF FORMTOTAL-BOOK NE CONST_BOOK-STOUTR.
*    WRITE: /1      TEXT-155.                     " accessability
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-155, 132 SY-VLINE. " accessability
        ENDIF.
        LOOP AT FORMTOTAL.
          IF FORMTOTAL-BOOK NE CONST_BOOK-STOUTR.
            CASE FORMTOTAL-FIRST_DIGIT.
              WHEN '5'.
*         WRITE: /1 TEXT-156.                     " accessability
                WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-156.         " accessability
                PERFORM WRITE_TOTALS USING FORMTOTAL.
              WHEN '6'.
*         WRITE: /1 TEXT-157.                     " accessability
                WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-157.         " accessability
                PERFORM WRITE_TOTALS USING FORMTOTAL.
              WHEN '7'.
*         WRITE: /1 TEXT-158.                     " accessability
                WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-158.         " accessability
                PERFORM WRITE_TOTALS USING FORMTOTAL.
            ENDCASE.
          ENDIF.
          IF FORMTOTAL-FIRST_DIGIT GE 5.
            GRAND_TOTAL-NFTOT  = GRAND_TOTAL-NFTOT  + FORMTOTAL-NFTOT.
            GRAND_TOTAL-BASE   = GRAND_TOTAL-BASE   + FORMTOTAL-BASE.
            GRAND_TOTAL-TAXVAL = GRAND_TOTAL-TAXVAL + FORMTOTAL-TAXVAL.
            GRAND_TOTAL-EXCBAS = GRAND_TOTAL-EXCBAS + FORMTOTAL-EXCBAS.
            GRAND_TOTAL-OTHBAS = GRAND_TOTAL-OTHBAS + FORMTOTAL-OTHBAS.
          ENDIF.
        ENDLOOP.

*     write grand_total
        FORMAT INTENSIFIED ON COLOR COL_TOTAL.   " accessability
*       WRITE: /1 TEXT-154.                      " accessability
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-154.          " accessability
        PERFORM WRITE_TOTALS USING GRAND_TOTAL.
    ENDCASE.                             "formbook_part
    MOVE GRAND_TOTAL-TAXVAL TO TRANSPORT.

  ENDFORM.                               " TOTAL_OUTPUT

*----------------------------------------------------------------------*
*                   Form  WRITE_TOTALS
*----------------------------------------------------------------------*
* writes totals lines
*----------------------------------------------------------------------*
*  -->  line-nftot, -base, -taxval -excbas, -othbas:
*            accumulated taxvalues
*  <--  output of one line with total values
*----------------------------------------------------------------------*

  FORM WRITE_TOTALS USING LINE STRUCTURE TOTAL.

    IF LINE-BOOK NE CONST_BOOK-STOUTR.
      WRITE: 31(20)   LINE-NFTOT CURRENCY T001-WAERS.
    ENDIF.
    WRITE: 51(20)   LINE-BASE CURRENCY T001-WAERS,
           71(20)   LINE-TAXVAL CURRENCY T001-WAERS,
           91(21)   LINE-EXCBAS CURRENCY T001-WAERS,
           112(20)  LINE-OTHBAS CURRENCY T001-WAERS.

    WRITE: 132 SY-VLINE.                        " accessability
    ZULINE.                                      " accessability

  ENDFORM.                    " WRITE_TOTALS

*&---------------------------------------------------------------------*
*&      Form  THIRD_PART
*&---------------------------------------------------------------------*
* Third parts of the books: transactions that are NOT generated via SD *
* or MM. Data coming from special G/L accounts.
* Account determination via table j_1blb09
*----------------------------------------------------------------------*

  FORM THIRD_PART USING BOOK.

* other debits
**break adyna.
    MOVE '31' TO BOOK_PART.
    NEW-PAGE.
* boi accessability
* WRITE:/ TEXT-245, "001 - Por Saídas...
*          112(20) DEBIT001 CURRENCY T001-WAERS.
* WRITE:/ TEXT-246. "002 - Outros Débitos
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-245, "001 - Por Saídas...
            117(20) DEBIT001 CURRENCY T001-WAERS, 132 SY-VLINE.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-246, 132 SY-VLINE. "002 - Outros Débitos
    PERFORM F_VLINE_SUM.
* eoi accessability
    PERFORM GET_DATA USING BOOK
                           '002'
                     CHANGING DEBIT002.
* boi accessability
* WRITE:/ TEXT-247. "003 - Estorno de Créditos
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-247, 132 SY-VLINE. "003 - Estorno de
* eoi accessability                                      Créditos
*    perform f_vline_sum.
    PERFORM GET_DATA USING BOOK
                           '003'
                     CHANGING ESCRDT003.

    SUBTOT004 = DEBIT001 + DEBIT002 + ESCRDT003.
* boi accessability
* WRITE:/ TEXT-248, "004 - Subtotal
*          112(20) SUBTOT004 CURRENCY T001-WAERS.
    FORMAT INTENSIFIED ON COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-248, "004 - Subtotal
            117(20) SUBTOT004 CURRENCY T001-WAERS, 132 SY-VLINE.
    ZULINE.
* eoi accessability

* other credits

    MOVE '32' TO BOOK_PART.
    NEW-PAGE.
* boi accessability
*  WRITE:/ TEXT-255, "005 - Por Entradas...
*          112(20) CREDIT005 CURRENCY T001-WAERS.
*  WRITE:/ TEXT-256. "006 - Outros Créditos
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-255, "005 - Por Entradas...
            117(20) CREDIT005 CURRENCY T001-WAERS, 132 SY-VLINE.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-256, 132 SY-VLINE. "006 - Outros Créditos
* eoi accessability

    PERFORM GET_DATA USING BOOK
                           '006'
                     CHANGING CREDIT006.
* boi accessability
* WRITE:/ TEXT-257. "007 - Estorno de Débitos
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-257, 132 SY-VLINE. "007 - Estorno de
* eoi accessability                                      Débitos

    PERFORM GET_DATA USING BOOK
                           '007'
                     CHANGING ESDEBT007.

    SUBTOT008 = CREDIT005 + CREDIT006 + ESDEBT007.
* boi accessability
* WRITE:/ TEXT-258, "008 - Subtotal
*          112(20) SUBTOT008 CURRENCY T001-WAERS.
* WRITE:/ TEXT-259. "009 - Saldo Credor do Período Anterior
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-258, "008 - Subtotal
            117(20) SUBTOT008 CURRENCY T001-WAERS, 132 SY-VLINE.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-259, 132 SY-VLINE. "009 - Saldo Credor do
* eoi accessability                                     Período Anterior


    PERFORM GET_DATA USING BOOK
                           '009'
                     CHANGING SALDO009.

    CRETOT010 = SUBTOT008 + SALDO009.
* boi accessability
* WRITE:/ TEXT-260, "010 - Total
*          112(20) CRETOT010 CURRENCY T001-WAERS.
    FORMAT INTENSIFIED ON COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-260, "010 - Total
            117(20) CRETOT010 CURRENCY T001-WAERS, 132 SY-VLINE.
    ZULINE.
* eoi accessability
* saldo

    MOVE '33' TO BOOK_PART.
    NEW-PAGE.

    SALDEB011 = SUBTOT004 - CRETOT010.
* boi accessability
* WRITE:/ TEXT-265. "011 - Saldo Devedor
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-265, 132 SY-VLINE. "011 - Saldo Devedor
* eoi accessability

    IF SALDEB011 GE 0.
* boi accessability
* WRITE 112(20) SALDEB011 CURRENCY T001-WAERS.
      WRITE: 1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 117(20) SALDEB011 CURRENCY T001-WAERS, 132 SY-VLINE.
    ENDIF.
* WRITE:/ TEXT-266. "012 - Deduções
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-266, 132 SY-VLINE. "012 - Deduções
* eoi accessability

    PERFORM GET_DATA USING BOOK
                           '012'
                     CHANGING DEDU012.

    SALDEB013 = SALDEB011 - DEDU012.
* boi accessability
* WRITE:/ TEXT-267. "013 - Imposto a Recolher
    FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-267, 132 SY-VLINE. "013 - Imposto a Recolher
**    if saldeb013 ge 0.
*** WRITE 112(20) SALDEB013 CURRENCY T001-WAERS.
**      write: 1 wg_blanks(5) color off, 6  sy-vline, 117(20) saldeb013 currency t001-waers, 132 sy-vline.
**    endif.
    IF PM_ARECO GE 0.
      WRITE: 1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 117(20) PM_ARECO CURRENCY T001-WAERS, 132 SY-VLINE.
    ENDIF.

    SALCRE014 = CRETOT010 + DEDU012 - SUBTOT004.
* WRITE:/ TEXT-268. "014 - Saldo Credor
    FORMAT INTENSIFIED ON COLOR COL_TOTAL.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-268, 132 SY-VLINE. "014 - Saldo Credor
    IF SALCRE014 GE 0.
* WRITE 112(20) SALCRE014 CURRENCY T001-WAERS.
      WRITE: 1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 117(20) SALCRE014 CURRENCY T001-WAERS, 132 SY-VLINE.
    ENDIF.
    ZULINE.
* eoi accessability
  ENDFORM.                    " THIRD_PART

*----------------------------------------------------------------------*
*         Form  GET_DATA
*----------------------------------------------------------------------*
* reads manually maintained FI-accounts (items) and creates and writes
* totals
*----------------------------------------------------------------------*
*  --> row  : row identifier in Modelo 9 form; determines which
*             accounts have to be read
*  <-- total: .....per row identity
*----------------------------------------------------------------------*

  FORM GET_DATA USING BOOK            TYPE N "#EC CI_USAGE_OK[2195701]
                      ROW             LIKE J_1BLB09-ROWDESCR "#EC CI_USAGE_OK[2195701]
                CHANGING VALUE(TOTAL) LIKE BSIS-DMBTR.

    DATA: INDI LIKE J_1BLB09-DRCRK, "#EC CI_USAGE_OK[2195701]
          NEWROW(3) TYPE N.

    DATA: ACCOUNT_TOTAL LIKE BSIS-DMBTR.
    CONSTANTS: BRANCH_SUM(4)  VALUE '++++',
               BRANCH_EACH(4) VALUE '****'.

    NEWROW = ROW.
    CONCATENATE BOOK NEWROW+1(2) INTO NEWROW.

    CLEAR ACCOUNT_TOTAL.
    CLEAR TOTAL.

* combination of two methods to recognize the posting for
* selected branch
* - old solution
*     account assigned to branch via customizing
* - new possiblity
*     for customizing entry '****' and '++++', the branch is to be
*     determined by information for FI document


    LOOP AT INTJ_1BLB09 WHERE BUKRS  = J5_BUKRS
                        AND  ( BRANCH = J5_BRNCH      OR "old
                               BRANCH = BRANCH_EACH   OR "new
                               BRANCH = BRANCH_SUM )     "new
                        AND  ROWDESCR = NEWROW.

      CASE INTJ_1BLB09-DRCRK.
        WHEN '1'.
          MOVE 'H' TO INDI.
        WHEN '2'.
          MOVE 'S' TO INDI.
        WHEN OTHERS.
          MESSAGE E463 WITH 'j1blb09'.
      ENDCASE.
      CLEAR BSIS.
      SELECT * FROM BSIS WHERE BUKRS = J5_BUKRS
                         AND   HKONT = INTJ_1BLB09-ACCOUNT
                         AND   SHKZG = INDI
                         AND   BUDAT IN J5_PDATE.

*     branch according to old solution:
*       all postings to this account are relevant
*       improvement: if branch is also given in posting, check
*                    consistency
*     new solution: check, whether branch is assigned in posting
*                   via the fields for branch or plant
*
        IF INTJ_1BLB09-BRANCH = J5_BRNCH.         "old solution
*       improvement of old solution
          IF NOT BSIS-BUPLA IS INITIAL OR
             NOT BSIS-WERKS IS INITIAL.
*          document contains business place or plant
            CHECK BSIS-BUPLA = J5_BRNCH OR
                  BSIS-WERKS IN PLANTS  .
*          check, if data on document
*          do not contradict customizing of account
          ENDIF.
        ELSE.                                     "new solution
          CHECK BSIS-BUPLA = J5_BRNCH OR
         ( PLANTS_EXISTING = 'X' AND BSIS-WERKS IN PLANTS ). " note 505845
        ENDIF.

        SELECT SINGLE * FROM BKPF WHERE BUKRS = J5_BUKRS " BOI note 780594
                                  AND   BELNR = BSIS-BELNR
                                  AND   GJAHR = J5_PDATE+3(4).
        CHECK BKPF-STBLG IS INITIAL.                     " EOI note 780594
        CLEAR: WG_CANCEL, WG_LIFNR.
        SELECT CANCEL PARID
          FROM J_1BNFDOC UP TO 1 ROWS
          INTO (WG_CANCEL, WG_LIFNR)
         WHERE BUKRS EQ BKPF-BUKRS
           AND BELNR EQ BKPF-AWKEY(10)
           AND GJAHR EQ BKPF-AWKEY+10(4).
        ENDSELECT.
        IF SY-SUBRC IS INITIAL AND NOT WG_CANCEL IS INITIAL.
          CONTINUE.
        ENDIF.
        PERFORM F_BUILD_SGTXT.
        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.

        IF INTJ_1BLB09-BRANCH NE BRANCH_SUM.
* boi accessability
*       WRITE: /7(50) BSIS-SGTXT,
*              88(20) BSIS-DMBTR CURRENCY T001-WAERS.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 12(50) BSIS-SGTXT,
                 93(20) BSIS-DMBTR CURRENCY T001-WAERS, 132 SY-VLINE.
          PERFORM F_VLINE_SUM.
* eoi accessability
        ELSE.
*       do not print each postings, if branch = '++++'.
          ACCOUNT_TOTAL = ACCOUNT_TOTAL + BSIS-DMBTR.
        ENDIF.
        TOTAL = TOTAL + BSIS-DMBTR.
      ENDSELECT.
*   print total amount of account, if not each posting was printed
      IF INTJ_1BLB09-BRANCH EQ BRANCH_SUM AND
         ACCOUNT_TOTAL NE 0.
*     get name of account
        CLEAR SKAT.
        SELECT SINGLE * FROM  SKAT
               WHERE  SPRAS  = SY-LANGU
               AND    KTOPL  = T001-KTOPL
               AND    SAKNR  = INTJ_1BLB09-ACCOUNT.
* boi accessability
*      write: /7(50) skat-txt50,
*             88(20) account_total currency t001-waers.
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 12(50) SKAT-TXT50,
               93(20) ACCOUNT_TOTAL CURRENCY T001-WAERS, 132 SY-VLINE.
        PERFORM F_VLINE_SUM.
* eoi accessability
        CLEAR ACCOUNT_TOTAL.
      ENDIF.

    ENDLOOP.
    FORMAT INTENSIFIED COLOR COL_TOTAL.
* boi accessability
*  WRITE 112(20) TOTAL CURRENCY T001-WAERS.
    WRITE: 1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 117(20) TOTAL CURRENCY T001-WAERS, 132 SY-VLINE.
* eoi accessability
    PERFORM F_VLINE_SUM.
  ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  INFORMACOES_COMPLEMENTARES
*&---------------------------------------------------------------------*
  FORM INFORMACOES_COMPLEMENTARES.
    DATA: SAVE_BOOK_PART LIKE BOOK_PART,
          SAVE_H_BOOK    LIKE H_BOOK.

    SAVE_H_BOOK = H_BOOK.                      "save old status
    SAVE_BOOK_PART = BOOK_PART.

    MOVE '40' TO BOOK_PART.                    "top-of-page variables
* h_book = 0.     " * -> use same subheader as proceeding pages

    FORMAT RESET.
    NEW-PAGE.
* boi accessability
* FORMAT INTENSIFIED COLOR COL_HEADING.
* WRITE: AT /43(89) TEXT-400.
* zuline.
*
* WRITE: AT /13 TEXT-401,
*        AT  79(53) TEXT-402.
* zuline.
* WRITE: AT /1   TEXT-410,
*        AT (68) TEXT-411.
* zuline.
    FORMAT INTENSIFIED OFF COLOR COL_HEADING.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, AT 48(89) TEXT-400, 132 SY-VLINE.
    ZULINE.

    FORMAT INTENSIFIED COLOR COL_HEADING.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, AT 18 TEXT-401,
          AT  79(53) TEXT-402, 132 SY-VLINE.
    ZULINE.
    FORMAT INTENSIFIED OFF COLOR COL_HEADING.
    WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, AT 7 TEXT-410,
          AT (65) TEXT-411, 132 SY-VLINE.
    ZULINE.
* eoi accessability
*------------- fill table int_addinfo - part 1 ---- BOI note 663255 ---*
    DATA:  INT_ADDINF1(120) TYPE C OCCURS 0,
           WA_ADDINFO1(125) TYPE C,
           LIN_ADDINF1      TYPE I,
           COUNTER1         TYPE I.

    CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->CRE_MOD9_ADDINFO1
      EXPORTING
        IS_BOOK    = H_BOOK
      IMPORTING
        ET_ADDINFO = INT_ADDINF1.
    IF SY-SUBRC = 0.
      DESCRIBE TABLE INT_ADDINF1 LINES LIN_ADDINF1.
      WHILE COUNTER1 < LIN_ADDINF1.
        CLEAR: WA_ADDINFO1.
        COUNTER1 = COUNTER1 + 1.
        READ TABLE INT_ADDINF1 INDEX COUNTER1 INTO WA_ADDINFO1.
*      WRITE: / wa_addinfo1.        * boi accessability
        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 WA_ADDINFO1, 132 SY-VLINE.
* eoi accessability
      ENDWHILE.
    ELSE.
*    SKIP 15.                                        " boi accessability
      DO 15 TIMES.
        WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 132 SY-VLINE.
      ENDDO.                                           " eoi accessability
    ENDIF.                                               " EOI note 663255

    ZULINE.
* WRITE: AT (126) TEXT-500.                          " boi accessability
    FORMAT INTENSIFIED OFF COLOR COL_HEADING.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, AT 8 TEXT-500, 132 SY-VLINE." eoi accessability
    ZULINE.

*------------- fill table int_addinfo - part 2 ---- BOI note 663255 ---*
    DATA:  INT_ADDINF2(120) TYPE C OCCURS 0,
           WA_ADDINFO2(125) TYPE C,
           LIN_ADDINF2      TYPE I,
           COUNTER2         TYPE I.

    CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->CRE_MOD9_ADDINFO2
      EXPORTING
        IS_BOOK    = H_BOOK
      IMPORTING
        ET_ADDINFO = INT_ADDINF2.
    IF SY-SUBRC = 0.
      DESCRIBE TABLE INT_ADDINF2 LINES LIN_ADDINF2.
      WHILE COUNTER2 < LIN_ADDINF2.
        CLEAR: WA_ADDINFO2.
        COUNTER2 = COUNTER2 + 1.
        READ TABLE INT_ADDINF2 INDEX COUNTER2 INTO WA_ADDINFO2.
*      WRITE: / wa_addinfo2.                         " boi accessability
        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 WA_ADDINFO2, 132 SY-VLINE. " eoi accessability
      ENDWHILE.
    ENDIF.                                               " EOI note 663255

    H_BOOK = SAVE_H_BOOK.                     "restore old status
    BOOK_PART = SAVE_BOOK_PART.
    FORMAT RESET.
  ENDFORM.                    " INFORMACOES_COMPLEMENTARES

  END-OF-PAGE.
    ZULINE.
*----------------------------------------------------------------------*
*---------------------------top-of-page--------------------------------*
*----------------------------------------------------------------------*

  TOP-OF-PAGE.

    IF ERRORLIST = ' '.
      FORMAT INTENSIFIED COLOR COL_HEADING.
      PAGNO = PAGNO + 1.

* common header
* boi accessability
*  WRITE: /1   SY-DATUM,
*          50  TEXT-200.
*  CASE H_BOOK.
*  WHEN '1'.
*    WRITE: /50 TEXT-280.
*  WHEN '2'.
*    WRITE: /50 TEXT-290.
*  ENDCASE.
*  zuline.
*  WRITE: /1  TEXT-202,
*          14  BRANCH_DATA-NAME.
*  WRITE: /1  TEXT-204,
*         14  BRANCH_DATA-STATE_INSC,
*         40  TEXT-205,
*         60  CGC_NUMBER.
*  WRITE: /1  TEXT-208,
*         14  PAGNO,
*         40  TEXT-209,
*         60  J5_PDATE-LOW, '-', J5_PDATE-HIGH.
*  CASE BOOK_PART.
*  WHEN '1'.                 "Entradas
*      WRITE / TEXT-221.
*      zuline.
*      WRITE / TEXT-222.
*      WRITE / TEXT-223.
*      WRITE / TEXT-224.
*      WRITE / TEXT-225.
*      WRITE / TEXT-226.
*  WHEN '2'.                 "Saídas
*      WRITE / TEXT-231.
*      zuline.
*      WRITE / TEXT-222.
*      WRITE / TEXT-223.
*      WRITE / TEXT-233.
*      WRITE / TEXT-225.
*      WRITE / TEXT-234.
*  WHEN '31'.                "other debits
*      WRITE / TEXT-241.
*      WRITE / TEXT-242.
*  WHEN '32'.                "other credits
*      WRITE / TEXT-251.
*      WRITE / TEXT-242.
*  WHEN '33'.                "saldo
*      WRITE / TEXT-261.
*      WRITE / TEXT-242.
*  ENDCASE.
*    IF BOOK_PART <> 40.
*      zuline.
*    ENDIF.
*  ELSE.
*    WRITE: /50 TEXT-300.
*  ENDIF.

      ZULINE.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE,
*                7   SY-DATUM,
               55  TEXT-200,
               132 SY-VLINE.
      CASE H_BOOK.
        WHEN '1'.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 55 TEXT-280, 132 SY-VLINE.
        WHEN '2'.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 55 TEXT-290, 132 SY-VLINE.
      ENDCASE.

      ZULINE.
      CONCATENATE ADDRESS1-NAME1 '-' BRANCH_DATA-NAME
             INTO WG_FIRMA SEPARATED BY SPACE.
      CONDENSE WG_FIRMA.
      FORMAT INTENSIFIED OFF COLOR COL_GROUP.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE,
              7  TEXT-202 INTENSIFIED COLOR COL_GROUP,
              19 WG_FIRMA,
              132 SY-VLINE.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE,
              7  TEXT-204 INTENSIFIED COLOR COL_GROUP,
             19  BRANCH_DATA-STATE_INSC,
             45  TEXT-205 INTENSIFIED  COLOR COL_GROUP,
             65  CGC_NUMBER,
            132  SY-VLINE.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE,
              7  TEXT-208 INTENSIFIED COLOR COL_GROUP,
             19  PAGNO,
             45  TEXT-209 INTENSIFIED COLOR COL_GROUP,
             65  J5_PDATE-LOW, '-', J5_PDATE-HIGH,
            132  SY-VLINE.
* eoi accessability
      ZULINE.
* subheaders for the different parts of the book
      CASE BOOK_PART.
        WHEN '1'.                 "Entradas
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-221, 132 SY-VLINE.
          ZULINE.
          FORMAT INTENSIFIED COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-222, 132 SY-VLINE.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-223, 132 SY-VLINE.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-224, 132 SY-VLINE.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-225, 132 SY-VLINE.
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-226, 132 SY-VLINE.
        WHEN '2'.                 "Saídas
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-231, 132 SY-VLINE.
          ZULINE.
          FORMAT INTENSIFIED COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-222, 132 SY-VLINE.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-223, 132 SY-VLINE.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-233, 132 SY-VLINE.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-225, 132 SY-VLINE.
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-234, 132 SY-VLINE.
        WHEN '31'.                "other debits
          FORMAT INTENSIFIED COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-241, 132 SY-VLINE.
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-242, 132 SY-VLINE.
        WHEN '32'.                "other credits
          FORMAT INTENSIFIED COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-251, 132 SY-VLINE.
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-242, 132 SY-VLINE.
        WHEN '33'.                "saldo
          FORMAT INTENSIFIED COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-261, 132 SY-VLINE.
          FORMAT INTENSIFIED OFF COLOR COL_HEADING.
          WRITE: / WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-242, 132 SY-VLINE.
      ENDCASE.
      IF BOOK_PART <> 40.
        ZULINE.
      ENDIF.
    ELSE.
      FORMAT INTENSIFIED OFF COLOR COL_HEADING.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 55 TEXT-300, 132 SY-VLINE.
    ENDIF.

    FORMAT COLOR OFF.
* eoi accessability

**********************************************************************
*   FORM    :  f_fake_data
*   Created :  06.03.2008 01:24:13
*  *********************************************************************
  FORM F_FAKE_DATA .
    exit.
    OUT-CFOP_NOEXT = '1101'.
    OUT-CFOP_EXT = ''.
    OUT-NFTOT = '5695762.71'.
    OUT-BASE = '745762.71'.
    OUT-TAXVAL = '126779.66'.
    OUT-EXCBAS = '0.00'.
    OUT-OTHBAS = '4950000.00'.
    OUT-BOOK = '0'.
    OUT-REGIO = ''.
    PERFORM OUTPUT USING OUT.
    OUT-CFOP_NOEXT = '1101'.
    OUT-CFOP_EXT = ''.
    OUT-NFTOT = '5695762.71'.
    OUT-BASE = '745762.71'.
    OUT-TAXVAL = '126779.66'.
    OUT-EXCBAS = '0.00'.
    OUT-OTHBAS = '4950000.00'.
    OUT-BOOK = '0'.
    OUT-REGIO = ''.
    PERFORM OUTPUT USING OUT.
    perform informacoes_complementares.
  ENDFORM. "f_fake_data
*&---------------------------------------------------------------------*
*&      Form  f_vline_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM F_VLINE_SUM .
    WRITE: 1 WG_BLANKS(5) COLOR OFF,
           6  SY-VLINE,
          93 SY-VLINE,
         116 SY-VLINE,
         132 SY-VLINE.

  ENDFORM.                    " f_vline_sum
*&---------------------------------------------------------------------*
*&      Form  f_build_sgtxt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM F_BUILD_SGTXT .
**    CHECK BSIS-SGTXT IS INITIAL.
    DATA: WL_TXT20   LIKE SKAT-TXT20.
    SELECT SINGLE TXT20
             FROM SKAT
             INTO WL_TXT20
            WHERE SPRAS = SY-LANGU
              AND KTOPL = '0050'
              AND SAKNR = BSIS-HKONT.
    CHECK SY-SUBRC IS INITIAL.
*    IF NOT SY-SUBRC IS INITIAL.
*      CLEAR WL_TXT20.
*    ENDIF.
    IF NOT WG_LIFNR IS INITIAL.
      SELECT SINGLE NAME1
               FROM LFA1
               INTO WG_NAME1
              WHERE LIFNR EQ WG_LIFNR.
    ENDIF.
    CONCATENATE WL_TXT20 's/NF' BKPF-XBLNR WG_NAME1
           INTO BSIS-SGTXT SEPARATED BY SPACE.

  ENDFORM.                    " f_build_sgtxt
*&---------------------------------------------------------------------*
*&      Form  f_compl_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM F_COMPL_INFO .
    CHECK NOT CB_COMPL IS INITIAL.
    FORMAT RESET.
    SKIP 2.
    ZULINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-T01, 132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE,             132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 'Número...........:', PM_NUM,   132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 'Data.............:', PM_DATE,  132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 'Valor............:', PM_VALUE, 132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 'Órgão Arrecadador:', PM_ORG,   132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 'Data da Entrega..:', PM_DELDT, 132 SY-VLINE.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 'Local da Entrega.:', PM_DELOC, 132 SY-VLINE.
    ZULINE.
  ENDFORM.                    " f_compl_info

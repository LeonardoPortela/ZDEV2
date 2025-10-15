* accessability
* REPORT J_1BLB08 MESSAGE-ID 8B LINE-SIZE 126 NO STANDARD PAGE HEADING.
  REPORT Z_J_1BLB08 MESSAGE-ID 8B
                     LINE-SIZE 132
                     LINE-COUNT 65(1)
                     NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*------------------------Report J_1BLB08-------------------------------*
*----------------------------------------------------------------------*
*-------Creation of the legal book 'Registro de Apuração do IPI'-------*
*-----------------------------(Modelo 8)-------------------------------*
*----------------------------------------------------------------------*

* declaration of the nota fiscal database tables
  DATA: WG_BLANKS(132).
  TABLES: J_1BNFDOC,                     "nota fiscal header,
          J_1BINDOC,                "nota fiscal header - add. segment
         *J_1BNFDOC,                     "nota fiscal header,
          J_1BNFLIN,                     "nota fiscal line items,
          J_1BINLIN,                "nota fiscal line items - add. segment
          J_1BNFSTX,                     "nota fiscal tax per item,
         *J_1BNFSTX,                     "nota fiscal tax per item,
          J_1BAJ,                        "nota fiscal types,
          J_1BLB08,   "#EC CI_USAGE_OK[2195701]                   "additional accounts for Modelo 8
          T001.                          "Company Codes
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

* definition of constants for taxgroups

  CONSTANTS: IPI(4)     TYPE C VALUE 'IPI ',
             ISSS(4)    TYPE C VALUE 'ISSS',                "955768
             ISSP(4)    TYPE C VALUE 'ISSP',                "955768
             ISS(4)     TYPE C VALUE 'ISS'.                 "955768

  CONSTANTS: SAO_PAULO(2) VALUE 'SP'.  "abbreviation for Sao Paulo state
  CONSTANTS: SANTA_CATARINA(2) VALUE 'SC'. "abbrev. for St. Catarina state
*----------------------------------------------------------------------*
* parameters for pagenumbering

  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-100.
  PARAMETERS: FIRSTPAG(6) TYPE N OBLIGATORY.
  PARAMETERS: OBSERV AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*

* parameters for output on screen: choose which parts to be printed

  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-101.
  PARAMETERS: ENTRADAS  RADIOBUTTON GROUP OUTP,
              SAIDAS    RADIOBUTTON GROUP OUTP,
              ENTRSAID  RADIOBUTTON GROUP OUTP,
              MODELO8   RADIOBUTTON GROUP OUTP DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*

* key for internally used record data - contains sorting fields

  DATA: BEGIN OF KEY,
        FIRST_DIGIT(1) TYPE C,           "first digit of cfop number
*      CFOP_NOEXT(3) TYPE N,  "cfop number without extension nt 553750
        CFOP_NOEXT(4) TYPE N,   "cfop number without extension nt 553750
        CFOP_EXT(2)    TYPE C,           "cfop extension
  END OF KEY.

* internally used record data

  DATA: BEGIN OF DATA,
        NFTOT   LIKE J_1BINLIN-NFTOT,    "total value per item,
                                         "incl. ICMS/ISS, IPI, freight,
                                         "insurance, other expenses
        BASE    LIKE J_1BNFSTX-BASE,     "calculation base
        TAXVAL  LIKE J_1BNFSTX-TAXVAL,   "tax amount
        EXCBAS  LIKE J_1BNFSTX-EXCBAS,   "excluded base amount
        OTHBAS  LIKE J_1BNFSTX-OTHBAS,   "other base amount
  END OF DATA.

*----------------------------------------------------------------------*

* declaration of internal table for nota fiscal types ( = j_1baj)

  DATA: INTJ_1BAJ LIKE J_1BAJ OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*

* declaration of internal table for additional accounts ( = j_1blb08)

  DATA: INTJ_1BLB08 LIKE J_1BLB08 OCCURS 0 WITH HEADER LINE. "#EC CI_USAGE_OK[2195701]

*----------------------------------------------------------------------*

* declaration of structures coming from function modules

  DATA: CGC_NUMBER  LIKE J_1BWFIELD-CGC_NUMBER,
        BRANCH_DATA LIKE J_1BBRANCH,
        ADDRESS1    LIKE ADDR1_VAL,
        T_MESG      LIKE MESG OCCURS 0 WITH HEADER LINE,
        WG_FIRMA(80).

*----------------------------------------------------------------------*

* definition of the field-groups for the extract

  FIELD-GROUPS: HEADER,
                EXDATA.

  INSERT KEY-FIRST_DIGIT
         KEY-CFOP_NOEXT
         KEY-CFOP_EXT
         INTO HEADER.

  INSERT DATA INTO EXDATA.

*----------------------------------------------------------------------*

* declaration of internally used help-fields

  DATA: FIRST_RECORD      TYPE C VALUE 'X',
        BOOK_PART(2)      TYPE N VALUE '1',    " 1  = Entradas,
                                         " 2  = Saídas,
                                         " 3x = other debits/credits
        PAGNO(6)          TYPE N,
        ERRORLIST         TYPE C VALUE ' ',
        PLANTS_EXISTING TYPE C.  " nec. for branches without plant; 505845

*----------------------------------------------------------------------*

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
  END OF OUT.

*----------------------------------------------------------------------*

* data for third part of the book

  DATA: CREDIT001 LIKE J_1BNFSTX-TAXVAL, " tax credit/inc. goods (within
                                         " Brazil) to be transported
                                         " from first part of the book
        CREDIT002 LIKE J_1BNFSTX-TAXVAL, " tax credit/inc. goods (within
                                         " Brazil) to be transported
                                         " from first part of the book
        CREDIT003 LIKE J_1BNFSTX-TAXVAL, " tax credit/outg. goods
                                         " (exports) to be transported
                                         " from second part of the book
        ESDEBT004 LIKE BSIS-DMBTR,       " sum of reversed tax debits
        CREDIT005 LIKE BSIS-DMBTR,       " sum of other tax credits
        SUBTOT006 LIKE BSIS-DMBTR,       " subtotal tax credits
        SALDO007  LIKE BSIS-DMBTR,       " tax credits previous period
        CRETOT008 LIKE BSIS-DMBTR,       " total tax credits
        DEBIT009  LIKE J_1BNFSTX-TAXVAL, " tax debit /outg. goods (within
                                         " Brazil) to be transported
                                         " from second part of the book
        ESCRDT010 LIKE BSIS-DMBTR,       " sum of reversed tax credits
        RESS011   LIKE BSIS-DMBTR,       " sum reversals
        DEBIT012  LIKE BSIS-DMBTR,       " sum of other tax debits
        DEBTOT013 LIKE BSIS-DMBTR,       " total tax debits
        SALDEB016 LIKE BSIS-DMBTR,       " debit saldo
        SALCRE017 LIKE BSIS-DMBTR.       " credit saldo

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
  DATA:  WG_NOMOV.
  DATA:  REP_DATE_HIGH        TYPE D.                 " note 617905

* BADI definition for legal books                             nt. 663255
  CLASS CL_EX_BADI_J_1BLEGALREPORT DEFINITION LOAD.           " nt. 663255
  DATA IF_EX_BADI_J_1BLEGALREPORT TYPE REF TO IF_EX_BADI_J_1BLEGALREPORT.

*----------------------------------------------------------------------*
*--------------------at selection-screen-------------------------------*
*----------------------------------------------------------------------*

  AT SELECTION-SCREEN.

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

* initialize pagenumbering

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

*----------------------------------------------------------------------*
*-----------------------fill table intj_1blb08-------------------------*
*----------------------------------------------------------------------*

    IF MODELO8 = 'X'.

      SELECT * FROM J_1BLB08 INTO TABLE INTJ_1BLB08 ORDER BY PRIMARY KEY. "#EC CI_FLDEXT_OK[2610650]

      IF SY-SUBRC NE 0.
        MESSAGE I462 WITH 'j_1blb08'.
        EXIT.
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

    ENDIF.

*---> fill item type table

    SELECT * FROM J_1BNFITMRULE INTO TABLE GT_NFITMRULE."note 553750

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

*----------------------------------------------------------------------*
*----------------------------get j_1bnflin-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BNFLIN.

    CLEAR: KEY-FIRST_DIGIT,
           KEY-CFOP_NOEXT,
           KEY-CFOP_EXT.

* Check in case of ISS-taxed service                            "955768
    IF J_1BNFLIN-TMISS = 'X'.                               "955768
      CHECK J_1BNFLIN-CFOP <> ' '.                          "955768
      CHECK INCL_SRV = 'X'.                                 "955768
    ENDIF.                                                  "955768

* second NF in third party process (itemtype 63) (third party GR item
* supplier to customer) is reported without any values in Modelo 1
* and 2. - Will therefore not be reported here

*del  CHECK J_1BNFLIN-ITMTYP NE '63'.              "note 553750
    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE        "note 553750
         WITH KEY ITMTYP = J_1BNFLIN-ITMTYP.         "note 553750
    "note 553750
    IF SY-SUBRC = 0.                                 "note 648768
      CHECK  GS_NFITMRULE-TRDPARTY IS INITIAL.       "note 553750
    ENDIF.                                           "note 648768

*----------------------------------------------------------------------*
*----------------------------get j_1binlin-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BINLIN.

    CLEAR: DATA-NFTOT.

* convert CFOP for SC into 6 digit form (e.g. j917a into 19917a)
    IF J_1BNFLIN-CFOP(1) CN '0123456789'.
      WRITE J_1BNFLIN-CFOP TO J_1BNFLIN-CFOP.
    ENDIF.

    MOVE J_1BNFLIN-CFOP+0(1) TO KEY-FIRST_DIGIT.
    CHECK KEY-FIRST_DIGIT CA '123567'.
*  MOVE J_1BNFLIN-CFOP+0(3) TO KEY-CFOP_NOEXT.             " note 553750
    MOVE J_1BNFLIN-CFOP+0(CFOP_LENGTH) TO KEY-CFOP_NOEXT.    " note 553750

*   determine, if new reporting for cfop codes X99 must be used
*   decreto is valid for Sao Paulo state from 01.01.2001 onwards
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
*         MOVE J_1BNFLIN-CFOP+3(2) TO KEY-CFOP_EXT.
*    ENDIF.
    IF J_1BNFLIN-CFOP+1(2) = '99'
       AND ADDRESS1-REGION = SANTA_CATARINA
       AND EXT_LEN > 0.
      MOVE J_1BNFLIN-CFOP+3(EXT_LEN) TO KEY-CFOP_EXT.
    ENDIF.                                          " END OF NOTE 553750

    MOVE-CORRESPONDING J_1BINLIN TO DATA.

* Third party IV item supplier to seller - itemype 64 - (case B - NF0)
* and future delivery IV item - itemtype 41 - (first NF in fut. del.
* process) are reported without valor total (valor contabil) - in Modelo
* 1 and 2; therefore this value will be cleared in these two cases
* To deal with the case 'Future delivery invoiving without IPI',
* itemtype 43 has been introduced, which behaves (in terms of reporting)
* just like itemtype 41
* note 450610: Additionally for itemtype 51 (Consignment invoice item)
* is reported without valor total (valor contabil) as in modelo 1/2/9

*  IF J_1BNFLIN-ITMTYP = '41' OR                   "note 553750
*     J_1BNFLIN-ITMTYP = '43' OR                   "note 553750
*     J_1BNFLIN-ITMTYP = '64' OR                   "note 553750
*     J_1BNFLIN-ITMTYP = '51'.      " note 450610  "note 553750
*    CLEAR DATA-NFTOT.                             "note 553750
*  ENDIF.                                          "note 553750

    READ TABLE GT_NFITMRULE INTO GS_NFITMRULE        "note 553750
         WITH KEY ITMTYP = J_1BNFLIN-ITMTYP.         "note 553750
    IF GS_NFITMRULE-IGNORETOTAL = 'X'.               "note 553750
      CLEAR DATA-NFTOT.                              "note 553750
    ENDIF.                                           "note 553750
*----------------------------------------------------------------------*
*----------------------------get j_1bnfstx-----------------------------*
*----------------------------------------------------------------------*

  GET J_1BNFSTX.

    CLEAR: DATA-BASE,
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

* select only items with IPI tax values

    CHECK INTJ_1BAJ-TAXGRP = IPI
* select also items with ISS tax (services)                     "955768
       OR INTJ_1BAJ-TAXGRP = ISSS                           "955768
       OR INTJ_1BAJ-TAXGRP = ISSP                           "955768
       OR INTJ_1BAJ-TAXGRP = ISS.                           "955768


    MOVE-CORRESPONDING J_1BNFSTX TO DATA.

* Reporting of ISS-taxed services                               "955768
    IF J_1BNFLIN-TMISS = 'X'.                               "955768
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

* tax-credit = 0 for non-deductible taxes
* old rule: taxvalue = 0, if otherbase > 0
* new rule: taxvalue = 0, if base = 0.
* if j_1bnfstx-othbas > 0.
    IF J_1BNFSTX-BASE = 0.
      DATA-TAXVAL = 0.
    ENDIF.

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
*     if reference data was found, check the 'base'
      IF SY-SUBRC = 0 AND *J_1BNFSTX-BASE > 0
                      AND J_1BNFSTX-STATTX NE 'X'.
        DATA-TAXVAL = J_1BNFSTX-TAXVAL.
      ENDIF.
    ENDIF.      " j_1bnfstx-base   = 0            and  ...

    IF J_1BNFSTX-STATTX = 'X'.
      CLEAR DATA-TAXVAL.
      CLEAR DATA-BASE.
      CLEAR DATA-EXCBAS.
      CLEAR DATA-OTHBAS.
    ENDIF.

*  note 210543: no special treatment for ipi pauta any more
*  if j_1bnfstx-rectype = '1'.                     "IPI Pauta
*    clear data-base.
*  endif.

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

* if several IPI conditions have been assigned to one line in the
* Nota Fiscal, the respective Valor total for this line  may only be
* reported once - this can happen in the case of a 50% IPI split
* (Compra de Comerciante não contribuente de IPI)

    CLEAR DATA-NFTOT.

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
*------------------------------- sort ---------------------------------*
*------------------ the extract according to the key ------------------*
*----------------------------------------------------------------------*

    SORT.

*----------------------------------------------------------------------*
*--------- condense records at cfop level                    ; --------*
*-------------------- write the condensed record ----------------------*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-------------------------------- loop --------------------------------*
*----------------------------------------------------------------------*

    LOOP.

*----------------------------------------------------------------------*
*-------------------------- at new key-cfop_noext----------------------*
*----------------------------------------------------------------------*

* write condensed line

      AT NEW KEY-CFOP_EXT.
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
      CLEAR WG_NOMOV.
      AT NEW KEY-FIRST_DIGIT.

        IF FIRST_RECORD  = ' '.
          APPEND TOTAL.
*       write totals for key-first_digit = 1, 2, 3
*       determine tax credits to be transported to book_part 31
*       (fields 001 and 002)
          IF KEY-FIRST_DIGIT CA '567' AND BOOK_PART = '1'.
            PERFORM TOTAL_OUTPUT TABLES TOTAL
                                 USING BOOK_PART
                                 CHANGING CREDIT001
                                          CREDIT002.
            MOVE '2' TO BOOK_PART.
            NEW-PAGE.
          ENDIF.
        ELSE.                            "first_record = 'X'...
          IF KEY-FIRST_DIGIT CA '567'.   "...and is a Saída
*         no incoming movements, but outgoing ones were selected:
*         print 'dummy' incoming movements page with remark
*         'sem movimento'
* boi accessability
*         FORMAT INTENSIFIED COLOR COL_KEY.
*         WRITE:/ TEXT-270.
            WG_NOMOV = 'X'.
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

      ENDAT.                             "at new key-first_digit.

*----------------------------------------------------------------------*

*   condense on documento fiscal-level per cfop

      TAXSUMS-NFTOT  = TAXSUMS-NFTOT  + DATA-NFTOT.
      TAXSUMS-BASE   = TAXSUMS-BASE   + DATA-BASE.
      TAXSUMS-TAXVAL = TAXSUMS-TAXVAL + DATA-TAXVAL.
      TAXSUMS-EXCBAS = TAXSUMS-EXCBAS + DATA-EXCBAS.
      TAXSUMS-OTHBAS = TAXSUMS-OTHBAS + DATA-OTHBAS.

      MOVE-CORRESPONDING KEY     TO OUT.
      MOVE-CORRESPONDING TAXSUMS TO OUT.

*   build totals per first digit of cfop number

      MOVE KEY-FIRST_DIGIT TO TOTAL-FIRST_DIGIT.
      TOTAL-NFTOT  = TOTAL-NFTOT  + DATA-NFTOT.
      TOTAL-BASE   = TOTAL-BASE   + DATA-BASE.
      TOTAL-TAXVAL = TOTAL-TAXVAL + DATA-TAXVAL.
      TOTAL-EXCBAS = TOTAL-EXCBAS + DATA-EXCBAS.
      TOTAL-OTHBAS = TOTAL-OTHBAS + DATA-OTHBAS.

    ENDLOOP.

**    IF NOT SY-SUBRC IS INITIAL.
**      ZULINE.
**    ENDIF.
*--------------write last record coming from the extract---------------*

* perform output only if there was any movement at all


    IF FIRST_RECORD NE 'X'.
      PERFORM OUTPUT USING OUT.
***    ELSE.
***      zuline.
    ENDIF.

*----------------------------------------------------------------------*
*------------------------ write last totals----------------------------*
*-------------for the first two parts of the book    and---------------*
*-------determine tax debits to be transported to book_part 32 --------*
*--------------------------(fields 003 and 009)------------------------*
* or tax credits to be transported to  book_part 31 (if only entradas) *
*--------------------------(fields 001 and 002) -----------------------*
*----------------------------------------------------------------------*

* perform output only if there was any movement at all

    IF FIRST_RECORD NE 'X'.
      APPEND TOTAL.
      CASE BOOK_PART.
        WHEN '1'.
          PERFORM TOTAL_OUTPUT TABLES TOTAL
                               USING BOOK_PART
                               CHANGING CREDIT001
                                        CREDIT002.
        WHEN '2'.
          PERFORM TOTAL_OUTPUT TABLES TOTAL
                               USING BOOK_PART
                               CHANGING DEBIT009
                                       CREDIT003.
      ENDCASE.
    ENDIF.

* no outgoing movements, but incoming ones were selected - print 'dummy'
* outgoing movements page with remark 'sem movimento'
* or: no movements at all were selected  - print 'dummy' outgoing and
* incoming movements pages with remark 'sem movimento'
* boi accessability
*  IF BOOK_PART = '1'.                  "i.e. no Saídas
*    FORMAT INTENSIFIED COLOR COL_KEY.
*    IF FIRST_RECORD = 'X'.             "and no Entradas
*      WRITE:/ TEXT-270.
*    ENDIF.
*    NEW-PAGE.
*    BOOK_PART = '2'.
*    WRITE:/ TEXT-270.
*    NEW-PAGE.
*    FORMAT INTENSIFIED COLOR COL_TOTAL.
*  ENDIF.
    IF BOOK_PART = '1'.                  "i.e. no Saídas
      FORMAT INTENSIFIED COLOR COL_KEY.
**      PERFORM F_FAKE_DATA.
      IF FIRST_RECORD = 'X'.           "and no Entradas
        FORMAT INTENSIFIED OFF COLOR COL_POSITIVE.
        WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-270, 132 SY-VLINE.
        FORMAT COLOR OFF.
        ZULINE.
      ENDIF.
      NEW-PAGE.
      BOOK_PART = '2'.
      FORMAT INTENSIFIED OFF COLOR COL_POSITIVE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-270, 132 SY-VLINE.
      FORMAT COLOR OFF.
      ZULINE.
      NEW-PAGE.
      FORMAT INTENSIFIED COLOR COL_TOTAL.
    ENDIF.
* eoi accessability

*----------------------------------------------------------------------*
*-----------------------Third part of the book:------------------------*
*-------------transactions that are NOT generated via SD or MM---------*
*-----------------Data coming from special G/L accounts----------------*
*-------------------other debits, other credits...---------------------*
*--------------account determination via table j_1blb08----------------*
*----------------------------------------------------------------------*

    IF MODELO8 = 'X'.

* other credits

      MOVE '31' TO BOOK_PART.
      NEW-PAGE.
* boi accessability
*    WRITE:/ TEXT-243, "001 - Por Entradas do Mercado Nacional
*            112(20) CREDIT001 CURRENCY T001-WAERS.
*
*    WRITE:/ TEXT-244, "002 - Por Entradas do Mercado Externo
*            112(20) CREDIT002 CURRENCY T001-WAERS.
*
*    WRITE:/ TEXT-245, "003 - Por Saídas para o Mercado Externo
*            112(20) CREDIT003 CURRENCY T001-WAERS.
*
*    WRITE:/ TEXT-246.                  "004 - Estorno de Débitos
*    PERFORM GET_DATA USING '004'
*                     CHANGING ESDEBT004.
*
*    WRITE:/ TEXT-247.                  "005 - Outros Créditos
*    PERFORM GET_DATA USING '005'
*                     CHANGING CREDIT005.

      FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
      MOVE '31' TO BOOK_PART.
      NEW-PAGE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-243 , "001-Por Entradas do Mercado Nacional
              118(20) CREDIT001 CURRENCY T001-WAERS,
              132  SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-244 , "002-Por Entradas do Mercado Externo
               118(20) CREDIT002 CURRENCY T001-WAERS,
               132  SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-245 ,"003-Por Saídas para o Mercado Externo
              118(20) CREDIT003 CURRENCY T001-WAERS,
              132  SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-246  INTENSIFIED OFF COLOR COL_TOTAL.
      "004 - Estorno de Débitos
      WRITE:132 SY-VLINE.
      PERFORM GET_DATA USING '004'
                      CHANGING ESDEBT004.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-247  INTENSIFIED OFF COLOR COL_TOTAL.
      "005 - Outros Créditos
      WRITE:132 SY-VLINE.
      PERFORM GET_DATA USING '005'
                      CHANGING CREDIT005.
      FORMAT INTENSIFIED ON.
* eoi accessability

* subtot006 = credit001 + credit002 + credit003 + esdebt004 + credit005.
* credit003 is no credit, but a debit; it appears in the
* wrong part of the report (by law), and has to be subtracted from the
* sum of all credits instead of being added to it
      SUBTOT006 = CREDIT001 + CREDIT002 - CREDIT003 + ESDEBT004 + CREDIT005.
* boi accessability
*   WRITE:/ TEXT-248,                  "006 - Subtotal
*           112(20) SUBTOT006 CURRENCY T001-WAERS.
*
*   WRITE:/ TEXT-249. "007 - Saldo Credor do Período Anterior
*   PERFORM GET_DATA USING '007'
*                     CHANGING SALDO007.
*
*   CRETOT008 = SUBTOT006 + SALDO007.
*   WRITE:/ TEXT-250,                  "008 - Total
*           112(20) CRETOT008 CURRENCY T001-WAERS.

      FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
      ZULINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-248,       "006 - Subtotal
             118(20) SUBTOT006 CURRENCY T001-WAERS.
      WRITE:132 SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-249  INTENSIFIED OFF COLOR COL_TOTAL.
      "007  - Saldo Credor do Período Anterior
      WRITE:132 SY-VLINE.
      PERFORM GET_DATA USING '007'
                       CHANGING SALDO007.
      CRETOT008 = SUBTOT006 + SALDO007.

      FORMAT INTENSIFIED ON.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-250,                  "008 - Total
            118(20) CRETOT008 CURRENCY T001-WAERS,
            132 SY-VLINE.
      ZULINE.
* eoi accessability



* other debits

      MOVE '32' TO BOOK_PART.
      NEW-PAGE.
* boi accessability
      FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-255,"009-PorSaídas para o Mercado Nacional
             118(20) DEBIT009 CURRENCY T001-WAERS,
             132 SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-256 INTENSIFIED OFF COLOR COL_TOTAL.
      "010 - Estorno de Créditos
      WRITE:132 SY-VLINE.
      PERFORM GET_DATA USING '010'
                      CHANGING ESCRDT010.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-257 INTENSIFIED OFF COLOR COL_TOTAL.
      "011 - Ressarcimento de Créditos
      WRITE:132 SY-VLINE.
      PERFORM GET_DATA USING '011'
                      CHANGING RESS011.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-258 INTENSIFIED OFF COLOR COL_TOTAL.
      "012 - Outros Débitos
      WRITE:132 SY-VLINE.
      PERFORM GET_DATA USING '012'
                      CHANGING DEBIT012.
      ZULINE.
      DEBTOT013 = DEBIT009 + ESCRDT010 + RESS011 + DEBIT012.
      FORMAT INTENSIFIED ON COLOR COL_TOTAL.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-259,      "013 - Total
              118(20) DEBTOT013 CURRENCY T001-WAERS,
              132 SY-VLINE.
      ZULINE.
* eoi accessability

* saldo

      MOVE '33' TO BOOK_PART.
      NEW-PAGE.

* boi accessability
      FORMAT INTENSIFIED ON COLOR COL_TOTAL.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-265,      "014 - Débito Total
            94(20) DEBTOT013 CURRENCY T001-WAERS,
            132 SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-266,       "015 - Crédito Total
           94(20) CRETOT008 CURRENCY T001-WAERS,
           132 SY-VLINE.
      SALDEB016 = DEBTOT013 - CRETOT008.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-267.      "016 - Saldo Devedor
      IF SALDEB016 GE 0.
        WRITE: 118(20) SALDEB016 CURRENCY T001-WAERS.
      ENDIF.
      WRITE  132 SY-VLINE.
      WRITE:/1 WG_BLANKS(5) COLOR OFF, 6  SY-VLINE, 7 TEXT-268.        "017 - Saldo Credor
      IF SALDEB016 < 0.
        SALCRE017 = - SALDEB016.
        WRITE: 118(20) SALCRE017 CURRENCY T001-WAERS,
              132 SY-VLINE.
      ENDIF.
      WRITE 132 SY-VLINE.
      ZULINE.
    ENDIF.
* eoi accessability
* print empty forms, if selected on selection screen
    IF OBSERV = 'X'.
      PERFORM DISTRIBUTION_OF_BALANCES.
      PERFORM OBSERVATION_TABLE.
    ENDIF.


*----------------------------------------------------------------------*
*---------------------------output errorlist---------------------------*
*----------------------------------------------------------------------*

    NEW-PAGE.

    ERRORLIST = 'X'.

    LOOP AT T_MESG.
      WRITE / T_MESG.
    ENDLOOP.

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
* FORMAT INTENSIFIED COLOR COL_KEY.                      " accessability
    FORMAT INTENSIFIED COLOR COL_KEY .                     " accessability

    SHIFT FORMOUT-CFOP_NOEXT LEFT DELETING LEADING '0'.
    CASE CFOP_LENGTH.
      WHEN 3.
        WRITE:  /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 19       FORMOUT-CFOP_NOEXT.  " accessability
      WHEN 4.
        WRITE:  /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 19       FORMOUT-CFOP_NOEXT(1)." accessability
        WRITE: AT 20 '.'
                         NO-GAP, FORMOUT-CFOP_NOEXT+1(3).  " accessability
        WRITE '      '.
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
    FORMAT INTENSIFIED OFF COLOR COL_NORMAL.       " note 854266
    WRITE: 31(20)   FORMOUT-NFTOT CURRENCY T001-WAERS,
           51(20)   FORMOUT-BASE CURRENCY T001-WAERS,
           71(20)   FORMOUT-TAXVAL CURRENCY T001-WAERS,
           91(21)   FORMOUT-EXCBAS CURRENCY T001-WAERS,
           112(20)  FORMOUT-OTHBAS CURRENCY T001-WAERS.
    CLEAR FORMOUT.
    WRITE: 18 SY-VLINE,
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
                    CHANGING VALUE(TRANSPORT1) LIKE J_1BNFSTX-TAXVAL
                             VALUE(TRANSPORT2) LIKE J_1BNFSTX-TAXVAL.

    DATA: GRAND_TOTAL LIKE TOTAL.

    CLEAR: GRAND_TOTAL,
           TRANSPORT1,
           TRANSPORT2.

    RESERVE 5 LINES.
    FORMAT INTENSIFIED COLOR COL_TOTAL.
    NEW-LINE.
    CASE FORMBOOK_PART.
      WHEN '1'.
        ZULINE.
        FORMAT INTENSIFIED OFF .
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-150, 132 SY-VLINE.
        LOOP AT FORMTOTAL.
          CASE FORMTOTAL-FIRST_DIGIT.
            WHEN '1'.
              FORMAT INTENSIFIED OFF .
              WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-151, 132 SY-VLINE..
              PERFORM WRITE_TOTALS USING FORMTOTAL.
              TRANSPORT1 = TRANSPORT1 + FORMTOTAL-TAXVAL.
            WHEN '2'.
              FORMAT INTENSIFIED OFF .
              WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-152, 132 SY-VLINE..
              PERFORM WRITE_TOTALS USING FORMTOTAL.
              TRANSPORT1 = TRANSPORT1 + FORMTOTAL-TAXVAL.
            WHEN '3'.
              FORMAT INTENSIFIED OFF .
              WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-153, 132 SY-VLINE..
              PERFORM WRITE_TOTALS USING FORMTOTAL.
              MOVE FORMTOTAL-TAXVAL TO TRANSPORT2.
          ENDCASE.
**          ZULINE.
* eoi accessability


          GRAND_TOTAL-NFTOT  = GRAND_TOTAL-NFTOT  + FORMTOTAL-NFTOT.
          GRAND_TOTAL-BASE   = GRAND_TOTAL-BASE   + FORMTOTAL-BASE.
          GRAND_TOTAL-TAXVAL = GRAND_TOTAL-TAXVAL + FORMTOTAL-TAXVAL.
          GRAND_TOTAL-EXCBAS = GRAND_TOTAL-EXCBAS + FORMTOTAL-EXCBAS.
          GRAND_TOTAL-OTHBAS = GRAND_TOTAL-OTHBAS + FORMTOTAL-OTHBAS.
        ENDLOOP.
        ZULINE.
*     write grand_total
* boi accessability
        FORMAT INTENSIFIED ON .
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-154, 132 SY-VLINE..
        PERFORM WRITE_TOTALS USING GRAND_TOTAL.
*        ZULINE.
      WHEN '2'.
        FORMAT INTENSIFIED OFF .
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-155, 132 SY-VLINE..
        LOOP AT FORMTOTAL.
          CASE FORMTOTAL-FIRST_DIGIT.
            WHEN '5'.
              FORMAT INTENSIFIED OFF .
              WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-156, 132 SY-VLINE..
              PERFORM WRITE_TOTALS USING FORMTOTAL.
              TRANSPORT1 = TRANSPORT1 + FORMTOTAL-TAXVAL.
            WHEN '6'.
              FORMAT INTENSIFIED OFF .
              WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-157, 132 SY-VLINE..
              PERFORM WRITE_TOTALS USING FORMTOTAL.
              TRANSPORT1 = TRANSPORT1 + FORMTOTAL-TAXVAL.
            WHEN '7'.
              FORMAT INTENSIFIED OFF .
              WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-158, 132 SY-VLINE..
              PERFORM WRITE_TOTALS USING FORMTOTAL.
              MOVE FORMTOTAL-TAXVAL TO TRANSPORT2.
          ENDCASE.
* eoi accessability

          IF FORMTOTAL-FIRST_DIGIT GE 5.
            GRAND_TOTAL-NFTOT  = GRAND_TOTAL-NFTOT  + FORMTOTAL-NFTOT.
            GRAND_TOTAL-BASE   = GRAND_TOTAL-BASE   + FORMTOTAL-BASE.
            GRAND_TOTAL-TAXVAL = GRAND_TOTAL-TAXVAL + FORMTOTAL-TAXVAL.
            GRAND_TOTAL-EXCBAS = GRAND_TOTAL-EXCBAS + FORMTOTAL-EXCBAS.
            GRAND_TOTAL-OTHBAS = GRAND_TOTAL-OTHBAS + FORMTOTAL-OTHBAS.
          ENDIF.
        ENDLOOP.
        ZULINE.
*     write grand_total
* boi accessability
*      WRITE: /1 TEXT-154.
        FORMAT INTENSIFIED ON.
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-154, 132 SY-VLINE.
* eoi accessability
        PERFORM WRITE_TOTALS USING GRAND_TOTAL.
    ENDCASE.                             "formbook_part
    ZULINE.
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

    WRITE: 31(20)   LINE-NFTOT CURRENCY T001-WAERS,
           51(20)   LINE-BASE CURRENCY T001-WAERS,
           71(20)   LINE-TAXVAL CURRENCY T001-WAERS,
           91(21)   LINE-EXCBAS CURRENCY T001-WAERS,
           112(20)  LINE-OTHBAS CURRENCY T001-WAERS.
  ENDFORM.                               " WRITE_TOTALS

*----------------------------------------------------------------------*
*         Form  GET_DATA
*----------------------------------------------------------------------*
* reads manually maintained FI-accounts (items) and creates and writes
* totals
*----------------------------------------------------------------------*
*  --> row  : row identifier in Modelo 8 form; determines which
*             accounts have to be read
*  <-- total: .....per row identity
*----------------------------------------------------------------------*

  FORM GET_DATA USING ROW             LIKE J_1BLB08-ROWDESCR "#EC CI_USAGE_OK[2195701]
                CHANGING VALUE(TOTAL) LIKE BSIS-DMBTR.

    DATA: INDI LIKE J_1BLB08-DRCRK. "#EC CI_USAGE_OK[2195701]

    DATA: ACCOUNT_TOTAL LIKE BSIS-DMBTR.

    CONSTANTS: BRANCH_SUM(4)  VALUE '++++',
               BRANCH_EACH(4) VALUE '****'.

    CLEAR ACCOUNT_TOTAL.
    CLEAR TOTAL.

* combination of two methods to recognize the posting for
* selected branch
* - old solution
*     account assigned to branch via customizing
* - new possiblity
*     for customizing entry '****' and '++++', the branch is to be
*     determined by information for FI document

    LOOP AT INTJ_1BLB08 WHERE BUKRS  = J5_BUKRS
                        AND  ( BRANCH = J5_BRNCH      OR "old
                               BRANCH = BRANCH_EACH   OR "new
                               BRANCH = BRANCH_SUM )     "new
                        AND  ROWDESCR = ROW.
      CASE INTJ_1BLB08-DRCRK.
        WHEN '1'.
          MOVE 'H' TO INDI.
        WHEN '2'.
          MOVE 'S' TO INDI.
        WHEN OTHERS.
          MESSAGE E463 WITH 'j1blb08'.
      ENDCASE.
      CLEAR BSIS.
      SELECT * FROM BSIS WHERE BUKRS = J5_BUKRS
                         AND   HKONT = INTJ_1BLB08-ACCOUNT
                         AND   SHKZG = INDI
                         AND   BUDAT IN J5_PDATE.

*     branch according to old solution:
*       all postings to this account are relevant
*       improvement: if branch is also given in posting, check
*                    consistency
*     new solution: check, whether branch is assigned in posting
*                   via the fields for branch or plant
*
        IF INTJ_1BLB08-BRANCH = J5_BRNCH.         "old solution
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

        FORMAT INTENSIFIED OFF COLOR COL_NORMAL.

        IF INTJ_1BLB08-BRANCH NE BRANCH_SUM.
* boi accessability
*        WRITE: /7(50) BSIS-SGTXT,
*               88(20) BSIS-DMBTR CURRENCY T001-WAERS.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 12(50) BSIS-SGTXT,
                 93(20) BSIS-DMBTR CURRENCY T001-WAERS,
                 132 SY-VLINE.
          WRITE 132 SY-VLINE.
* eoi accessability
        ELSE.
*       do not print each postings, if branch = '++++'.
          ACCOUNT_TOTAL = ACCOUNT_TOTAL + BSIS-DMBTR.
        ENDIF.
        TOTAL = TOTAL + BSIS-DMBTR.
      ENDSELECT.
*   print total amount of account, if not each posting was printed
      IF INTJ_1BLB08-BRANCH EQ BRANCH_SUM AND
         ACCOUNT_TOTAL NE 0.
*     get name of account
        CLEAR SKAT.
        SELECT SINGLE * FROM  SKAT
               WHERE  SPRAS  = SY-LANGU
               AND    KTOPL  = T001-KTOPL
               AND    SAKNR  = INTJ_1BLB08-ACCOUNT.
* boi accessability
*      write: /7(50) skat-txt50,
*             88(20) account_total currency t001-waers.
        FORMAT INTENSIFIED OFF COLOR COL_TOTAL.
        WRITE: /12(50) SKAT-TXT50,
                93(20) ACCOUNT_TOTAL CURRENCY T001-WAERS.
* eoi accessability
        CLEAR ACCOUNT_TOTAL.
      ENDIF.

    ENDLOOP.
    FORMAT INTENSIFIED COLOR COL_TOTAL.
    WRITE 117(20) TOTAL CURRENCY T001-WAERS.
    WRITE 132 SY-VLINE.                     " accessability

  ENDFORM.                               " GET_DATA


  END-OF-PAGE.
    ZULINE.
*----------------------------------------------------------------------*
*---------------------------top-of-page--------------------------------*
*----------------------------------------------------------------------*

  TOP-OF-PAGE.

    IF ERRORLIST = ' '.

      FORMAT INTENSIFIED OFF COLOR COL_HEADING.
      PAGNO = PAGNO + 1.
      ZULINE.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE.
      WRITE:" 7   SY-DATUM,
             56  TEXT-200.
      WRITE:132 SY-VLINE.
      ZULINE.
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE.

      FORMAT INTENSIFIED OFF COLOR COL_GROUP.
      CONCATENATE ADDRESS1-NAME1 '-' BRANCH_DATA-NAME
             INTO WG_FIRMA SEPARATED BY SPACE.
      CONDENSE WG_FIRMA.

      WRITE: 7  TEXT-202 COLOR COL_GROUP INTENSIFIED ON  .
      WRITE  19 WG_FIRMA.

      WRITE: 132 SY-VLINE.

      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE.
      WRITE: 7  TEXT-204 INTENSIFIED  COLOR COL_GROUP,
             19  BRANCH_DATA-STATE_INSC ,
             45  TEXT-205 INTENSIFIED  COLOR COL_GROUP,
             65  CGC_NUMBER .
      WRITE: 132 SY-VLINE.

      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE.
      WRITE: 7  TEXT-208 INTENSIFIED  COLOR COL_GROUP,
             19  PAGNO ,
             45  TEXT-209 INTENSIFIED  COLOR COL_GROUP.
      WRITE: 65  J5_PDATE-LOW ,'-' ,J5_PDATE-HIGH .
      WRITE: 132 SY-VLINE.
      ZULINE.

* subheaders for the different parts of the book
      FORMAT INTENSIFIED COLOR COL_HEADING.

      CASE BOOK_PART.
        WHEN '1'.                        "Entradas
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-221 INTENSIFIED OFF, 132 SY-VLINE.
          ZULINE.

          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-222, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-223, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-224, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-225, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-226  INTENSIFIED OFF, 132 SY-VLINE.
        WHEN '2'.                        "Saídas
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-231 INTENSIFIED OFF, 132 SY-VLINE.
          ZULINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-222, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-223, 132 SY-VLINE.

          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-233, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-225,132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-234 INTENSIFIED OFF, 132 SY-VLINE.
        WHEN '31'.                           "other credits
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-241 , 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-242 INTENSIFIED OFF, 132 SY-VLINE.
        WHEN '32'.                          "other debits
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-251, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-242 INTENSIFIED OFF, 132 SY-VLINE.
        WHEN '33'.                       "saldo
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-261, 132 SY-VLINE.
          WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 TEXT-242 INTENSIFIED OFF, 132 SY-VLINE.
      ENDCASE.
      FORMAT INTENSIFIED OFF COLOR OFF.
      IF BOOK_PART <> 99.
        ZULINE.
      ENDIF.
    ELSE.                                "errorlist = 'X'
      WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 56 TEXT-300, 132 SY-VLINE.
    ENDIF.
* eoi accessability
*&---------------------------------------------------------------------*
*&      Form  DISTRIBUTION_OF_BALANCES
*&---------------------------------------------------------------------*
*       creates empty table                                            *
*----------------------------------------------------------------------*
  FORM DISTRIBUTION_OF_BALANCES.
    DATA SAVE_BOOK_PART.

    SAVE_BOOK_PART = BOOK_PART.          "control top-of-page output
    BOOK_PART = 99.

    NEW-PAGE.
* boi accessability
* FORMAT COLOR COL_HEADING.
* WRITE: AT 30(126) TEXT-401.
* zuline.
* WRITE: AT (62) TEXT-400 NO-GAP,
*                     '||' NO-GAP,
*         AT (62) TEXT-400.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE,  AT 35(132) TEXT-401, 132 SY-VLINE.
    ZULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: 1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE , AT 7(62) TEXT-400 NO-GAP,
                       '||' NO-GAP,
           AT (67) TEXT-400,
           132 SY-VLINE.
* eoi accessability
    ZULINE.

    BOOK_PART = SAVE_BOOK_PART.

*------------- fill table int_addinfo - part 1 ---- BOI note 663255 ---*
    DATA:  INT_ADDINF1(60) TYPE C OCCURS 0,
           WA_ADDINFO1(62) TYPE C,
           WA_ADDINFO2(62) TYPE C,
           LIN_ADDINF1     TYPE I,
           COUNTER1        TYPE I,
           COUNTER2        TYPE I.

    CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->CRE_MOD8_ADDINFO1
      IMPORTING
        ET_ADDINFO = INT_ADDINF1.

    IF SY-SUBRC = 0.
      DESCRIBE TABLE INT_ADDINF1 LINES LIN_ADDINF1.
      LIN_ADDINF1 = ( LIN_ADDINF1 ) / 2.
      WHILE COUNTER1 < LIN_ADDINF1.
        CLEAR: WA_ADDINFO1, WA_ADDINFO2.
        COUNTER1 = COUNTER1 + 1.
        COUNTER2 = COUNTER1 + LIN_ADDINF1.
        READ TABLE INT_ADDINF1 INDEX COUNTER1 INTO WA_ADDINFO1.
        READ TABLE INT_ADDINF1 INDEX COUNTER2 INTO WA_ADDINFO2.
*     WRITE: / wa_addinfo1 NO-GAP, '||' NO-GAP, wa_addinfo2.
        WRITE: /1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 WA_ADDINFO1 NO-GAP, '||' NO-GAP, WA_ADDINFO2,
                 132 SY-VLINE.                            " accessability
      ENDWHILE.
    ENDIF.                                              " EOI note 663255

  ENDFORM.                               " DISTRIBUTION_OF_BALANCES

*&---------------------------------------------------------------------*
*&      Form  OBSERVATION_TABLE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM OBSERVATION_TABLE.
    DATA SAVE_BOOK_PART.

    SAVE_BOOK_PART = BOOK_PART.          "control top-of-page output
    BOOK_PART = 99.

* new-page. "new requirement: same page as "distribution_of_balances
    SKIP 15.
    FORMAT COLOR COL_HEADING.
    ZULINE.
* boi accessability
* WRITE AT (126) TEXT-410.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: 1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE.
    WRITE AT 6(132) TEXT-410.
    WRITE 132 SY-VLINE.
* eoi accessability
    ZULINE.

*------------- fill table int_addinfo - part 2 ---- BOI note 663255 ---*
    DATA:  INT_ADDINF2(120) TYPE C OCCURS 0,
           WA_ADDINFO3(126) TYPE C,
           LIN_ADDINF2      TYPE I,
           COUNTER3         TYPE I.

    CALL METHOD IF_EX_BADI_J_1BLEGALREPORT->CRE_MOD8_ADDINFO2
      IMPORTING
        ET_ADDINFO = INT_ADDINF2.

    IF SY-SUBRC = 0.
      DESCRIBE TABLE INT_ADDINF2 LINES LIN_ADDINF2.
      WHILE COUNTER3 < LIN_ADDINF2.
        CLEAR: WA_ADDINFO3.
        COUNTER3 = COUNTER3 + 1.
        READ TABLE INT_ADDINF2 INDEX COUNTER3 INTO WA_ADDINFO3.
* boi accessability
*      WRITE: / wa_addinfo3.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        WRITE:/1 WG_BLANKS(5) COLOR OFF, 6 SY-VLINE, 7 WA_ADDINFO3, 132 SY-VLINE.
* eoi accessability
      ENDWHILE.
    ENDIF.                                              " EOI note 663255

    BOOK_PART = SAVE_BOOK_PART.
  ENDFORM.                               " OBSERVATION_TABLE

*&---------------------------------------------------------------------*
*&      Form  f_fake_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM F_FAKE_DATA.
    OUT-CFOP_NOEXT = '1250'.
    OUT-CFOP_EXT = ''.
    OUT-NFTOT = '11019.29'.
    OUT-BASE = '0.00'.
    OUT-TAXVAL = '0.00'.
    OUT-EXCBAS = '0.00'.
    OUT-OTHBAS = '11019.28'.
    PERFORM OUTPUT USING OUT.
    OUT-CFOP_NOEXT = '2101'.
    OUT-CFOP_EXT = ''.
    OUT-NFTOT = '2388.05'.
    OUT-BASE = '0.00'.
    OUT-TAXVAL = '0.00'.
    OUT-EXCBAS = '0.00'.
    OUT-OTHBAS = '2388.06'.
    PERFORM OUTPUT USING OUT.
    OUT-CFOP_NOEXT = '2551'.
    OUT-CFOP_EXT = ''.
    OUT-NFTOT = '94976.40'.
    OUT-BASE = '0.00'.
    OUT-TAXVAL = '0.00'.
    OUT-EXCBAS = '0.00'.
    OUT-OTHBAS = '14928.58'.
    PERFORM OUTPUT USING OUT.

  ENDFORM.                    "f_fake_data

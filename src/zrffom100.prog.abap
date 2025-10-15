************************************************************************
*                                                                      *
*  Datenträger-Druckprogramm RFFOM100 (MT100 International)            *
*  DME print program RFFOM100 (MT100 International)                    *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* Das Programm includiert:                                             *
*                                                                      *
* RFFORI0M  Makrodefinition für den Selektionsbildaufbau               *
* RFFORI00  Deklarationsteil der Zahlungsträger-Druckprogramme         *
* RFFORI06  Avis                                                       *
* RFFORI07  Zahlungsbegleitliste                                       *
* RFFORI10  MT100 Routine zur Erzeugung von MT100-Files                *
* RFFORI99  Allgemeine Unterroutinen der Zahlungsträger-Druckprogramme *
*----------------------------------------------------------------------*
* The program includes:                                                *
*                                                                      *
* RFFORI0M  Definition of macros                                       *
* RFFORI00  international data definitions                             *
* RFFORI06  remittance advice                                          *
* RFFORI07  payment summary list                                       *
* RFFORI10  MT100 routine to generate the MT100-files                  *
* RFFORI99  international subroutines                                  *
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Report Header                                                        *
*----------------------------------------------------------------------*
REPORT RFFOM100
  LINE-SIZE 132
  MESSAGE-ID F0
  NO STANDARD PAGE HEADING.


*----------------------------------------------------------------------*
*  Segments                                                            *
*----------------------------------------------------------------------*
TABLES:
  REGUH,
  REGUP,
  T042OFI,
  DTAM100,
  DTAM100H,
  DTAM100S,
  DTAM100T.

DATA: BEGIN OF DTA_MT100 OCCURS 12.
        INCLUDE STRUCTURE DTAMT100.
DATA: END OF DTA_MT100.

DATA: BEGIN OF DTA_FILECNT OCCURS 12,
        ZBUKR        LIKE REGUH-ZBUKR,
        UBNKS        LIKE REGUH-UBNKS,     "Bankland
        UBNKL        LIKE REGUH-UBNKL,     "Bankleitzahl
        ANZAHL       TYPE I,               "Anzahl der reguh's/File
        SZBNKN(12)   TYPE P,               "Summe Kontonr/File
        SVBETR(9)    TYPE P,               "Summe Vorkommastellen/File
      END OF DTA_FILECNT,

      UP_WSCHL(1)    TYPE C,
      DTA_ZEILEN     TYPE I,
      UP_USREX(1)    TYPE C,
      UP_DUAL        TYPE I,
      UP_BANKS       LIKE BNKA-BANKS.      "Bank country for SWIFT check

DATA  BEGIN OF TAB_DTAM100V OCCURS 12.
        INCLUDE STRUCTURE DTAM100V.
DATA  END OF TAB_DTAM100V.

DATA  BEGIN OF NAMETAB OCCURS 50.
        INCLUDE STRUCTURE DNTAB.
DATA  END   OF NAMETAB.

DATA: BEGIN OF TAB_SUM_PER_CURRENCY_EXT OCCURS 0.
        INCLUDE STRUCTURE DTAM100C.
DATA:   ZBUKR LIKE REGUH-ZBUKR,
        UBNKS LIKE REGUH-UBNKS,
        UBNKY LIKE REGUH-UBNKY,
      END OF TAB_SUM_PER_CURRENCY_EXT.

DATA: TAB_SUM_PER_CURRENCY LIKE DTAM100C OCCURS 0 WITH HEADER LINE.


*---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  Makrodefinitionen                                                   *
*----------------------------------------------------------------------*
INCLUDE ZRFFORI0M.
*INCLUDE RFFORI0M.

INITIALIZATION.

*----------------------------------------------------------------------*
*  Parameters / Select-Options                                         *
*----------------------------------------------------------------------*
  BLOCK 1.
  SELECT-OPTIONS:
    SEL_ZAWE FOR  REGUH-RZAWE,         "Zahlwege / payment methods
    SEL_UZAW FOR  REGUH-UZAWE,         "Zahlwegzusatz
    SEL_HBKI FOR  REGUH-HBKID,         "house bank short key
    SEL_HKTI FOR  REGUH-HKTID,         "account data short key
    SEL_WAER FOR  REGUH-WAERS,         "currency
    SEL_VBLN FOR  REGUH-VBLNR.         "payment document number
  SELECTION-SCREEN END OF BLOCK 1.

  BLOCK 2.
  PARAMETERS:
    PAR_XDTA LIKE RFPDO-FORDXDTA.      "DME in MT100 format
  AUSWAHL: AVIS A, BEGL B.
  SPOOL_AUTHORITY.                     "Spoolberechtigung
  SELECTION-SCREEN END OF BLOCK 2.

  BLOCK 3.
  PARAMETERS:
    PAR_MOFI LIKE T042OFI-FORMT,       "format for modification via OFI
    PAR_SBNK LIKE RFPDO2-FORDSBNK MODIF ID GR1,
                                       "sending bank of dme format MT101
    PAR_UNIX LIKE RFPDO2-FORDNAME,     "Dateiname für DTA und TemSe
    PAR_DTYP LIKE RFPDO-FORDDTYP,      "Ausgabeformat und -medium
    PAR_ANZP LIKE RFPDO-FORDANZP,      "number of test prints
    PAR_MAXP LIKE RFPDO-FORDMAXP,      "number of items in summary list
    PAR_ZEIL LIKE RFPDO2-FORDZEIL,     "number of lines '70'
    PAR_CBXX LIKE RFPDO2-FORDCBXX,     "mehrere Zahlungen pro File
    PAR_BELP LIKE RFPDO-FORDBELP,      "payment doc. validation
    PAR_IBAN LIKE RFPDO1-FORDIBAN,     "transfer IBAN
    PAR_ESPR LIKE RFPDO-FORDESPR,      "texts in reciepient's lang.
    PAR_ISOC LIKE RFPDO-FORDISOC.      "currency in ISO code

  SELECTION-SCREEN END OF BLOCK 3.

  PARAMETERS:
    PAR_PRIW LIKE RFPDO-FORDPRIW  NO-DISPLAY,
    PAR_SOFW LIKE RFPDO1-FORDSOFW NO-DISPLAY,
    PAR_PRIZ LIKE RFPDO-FORDPRIZ  NO-DISPLAY,
    PAR_SOFZ LIKE RFPDO1-FORDSOFZ NO-DISPLAY,
    PAR_VARI(12) TYPE C           NO-DISPLAY,
    PAR_SOFO(1)  TYPE C           NO-DISPLAY,
    PAR_ZDRU LIKE RFPDO-FORDZDRU  NO-DISPLAY.
*----------------------------------------------------------------------*
*  Vorbelegung der Parameter und Select-Options                        *
*  default values for parameters and select-options                    *
*----------------------------------------------------------------------*
  PERFORM INIT.
  SEL_ZAWE-LOW     = 'U'.
  SEL_ZAWE-OPTION  = 'EQ'.
  SEL_ZAWE-SIGN    = 'I'.
  APPEND SEL_ZAWE.

  PAR_BELP = SPACE.
  PAR_XDTA = 'X'.
  PAR_DTYP = '0'.
  PAR_AVIS = 'X'.
  PAR_BEGL = 'X'.
  PAR_ANZP = 2.
  PAR_ESPR = SPACE.
  PAR_ISOC = SPACE.
  PAR_MAXP = 9999.
  PAR_ZEIL = 4.
  PAR_CBXX = 'X'.

*----------------------------------------------------------------------*
*  tables / fields / field-groups / at selection-screen                *
*----------------------------------------------------------------------*
INCLUDE ZRFFORI00.
*INCLUDE RFFORI00.

*- Prüfungen bei DTA (speziell für Deutschland) ------------------------
*- special checks for German DME ---------------------------------------
IF PAR_XDTA EQ 'X'.                    "Datenträgeraustausch
  IF PAR_DTYP EQ SPACE.
    PAR_DTYP = '0'.                    "Diskettenformat in TemSe
  ENDIF.
  IF PAR_DTYP NA '012'.
    SET CURSOR FIELD 'PAR_DTYP'.
    MESSAGE E068.
  ENDIF.
ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF PAR_MOFI = 'MT101'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1' AND SCREEN-ACTIVE = '0'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1' AND SCREEN-ACTIVE = '1'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON PAR_MOFI.
  IF PAR_MOFI NE SPACE.
    SELECT SINGLE * FROM T042OFI WHERE FORMT EQ PAR_MOFI.
    IF SY-SUBRC NE 0.
      MESSAGE E318 WITH PAR_MOFI.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON PAR_SBNK.
  IF NOT PAR_SBNK IS INITIAL.
    UP_BANKS = PAR_SBNK+4(2).
    CALL FUNCTION 'SWIFT_CODE_CHECK'
         EXPORTING BANK_COUNTRY = UP_BANKS
                   SWIFT_CODE   = PAR_SBNK.
  ENDIF.


*----------------------------------------------------------------------*
*  Kopfzeilen (nur bei der Zahlungsbegleitliste)                       *
*  batch heading (for the payment summary list)                        *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  IF FLG_BEGLEITL EQ 1.
    PERFORM KOPF_ZEILEN.               "RFFORI07
  ENDIF.



*----------------------------------------------------------------------*
*  Felder vorbelegen                                                   *
*  preparations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  FLG_ZETTEL = 0.
  HLP_TEMSE  = '0---------'.           "Reportspezif. TemSe-Parameter
  HLP_AUTH   = PAR_AUTH.                "spool authority

  PERFORM VORBEREITUNG.

* check and read format modification for Open FI
  CLEAR T042OFI.
  IF PAR_MOFI NE SPACE.
    SELECT SINGLE * FROM T042OFI WHERE FORMT EQ PAR_MOFI.
    IF SY-SUBRC EQ 0.
      MESSAGE S319 WITH PAR_MOFI.
    ELSE.
      MESSAGE A318 WITH PAR_MOFI.
    ENDIF.
  ENDIF.

  IF PAR_IBAN IS INITIAL.
* don't fill IBAN fields in REGUH
    CLEAR FLG_IBAN.
  ENDIF.

*----------------------------------------------------------------------*
*  Daten prüfen und extrahieren                                        *
*  check and extract data                                              *
*----------------------------------------------------------------------*
GET REGUH.
  CHECK SEL_ZAWE.
  CHECK SEL_UZAW.
  CHECK SEL_HBKI.
  CHECK SEL_HKTI.
  CHECK SEL_WAER.
  CHECK SEL_VBLN.
  PERFORM PRUEFUNG.
  DTA_FILECNT-ZBUKR  = REGUH-ZBUKR.
  DTA_FILECNT-UBNKS  = REGUH-UBNKS.
  DTA_FILECNT-UBNKL  = REGUH-UBNKL.
  DTA_FILECNT-ANZAHL = 1.
  HLP_ZBNKN = REGUH-ZBNKN.                                 "Konv. C->N
  DTA_FILECNT-SZBNKN = HLP_ZBNKN.
  PERFORM DTA_VORKOMMA(RFFOD__L) USING REGUH-WAERS REGUH-RWBTR.
  DTA_FILECNT-SVBETR = SPELL-NUMBER.
  COLLECT DTA_FILECNT.
  IF PAR_CBXX EQ 'X'.
    TAB_SUM_PER_CURRENCY_EXT-ZBUKR = REGUH-ZBUKR.
    TAB_SUM_PER_CURRENCY_EXT-UBNKS = REGUH-UBNKS.
    TAB_SUM_PER_CURRENCY_EXT-UBNKY = REGUH-UBNKY.
    TAB_SUM_PER_CURRENCY_EXT-WAERS = REGUH-WAERS.
    TAB_SUM_PER_CURRENCY_EXT-RWBTR = REGUH-RWBTR.
    COLLECT TAB_SUM_PER_CURRENCY_EXT.
  ENDIF.

  PERFORM EXTRACT_VORBEREITUNG.

GET REGUP.

  PERFORM EXTRACT.



*----------------------------------------------------------------------*
*  Bearbeitung der extrahierten Daten                                  *
*  DME, remittance advices and lists                                   *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF FLG_SELEKTIERT NE 0.
    IF PAR_XDTA EQ 'X'.
      PERFORM MT100.                   "RFFORI10
    ENDIF.

    IF PAR_AVIS EQ 'X'.
      PERFORM AVIS.                    "RFFORI06
    ENDIF.

    IF PAR_BEGL EQ 'X' AND PAR_MAXP GT 0.
      FLG_BANKINFO = 2.
      PERFORM BEGLEITLISTE.            "RFFORI07
    ENDIF.

  ENDIF.

  PERFORM FEHLERMELDUNGEN.

  PERFORM INFORMATION.



*----------------------------------------------------------------------*
*  Allgemeine Unterprogramme                                           *
*  international subroutines                                           *
*----------------------------------------------------------------------*
INCLUDE ZRFFORI99.
*INCLUDE RFFORI99.
INCLUDE ZRFFORI06.
*INCLUDE RFFORI06.
INCLUDE ZRFFORI07.
*INCLUDE RFFORI07.
INCLUDE ZRFFORI10.
*INCLUDE RFFORI10.

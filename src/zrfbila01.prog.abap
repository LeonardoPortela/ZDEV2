REPORT ZRFBILA01 MESSAGE-ID FR
                 LINE-SIZE   201
                 NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Corrections/ repair
* wes140906 140906 note 980527: Pass trig-kz to konz_extract(RGCBILA0)
*                  for enhancement of wes190202
* ECC500
* xrp290904 290904 note 758747: Add functionality for SEM-BCS
* 4.70
* xrp260704 260704 note 758742: Activate Sel Screen 2000 for FI-LC
* xrp040803 040803 note 647723: Unicode enabling for extract to cons
* wms086855 110603 Note 630825: performance enhancement/ subsequent
*                  error after note 623476 in case of restriction to
*                  business areas
* wes190202 190202 Extract: Handle values with LC = 0 and GC <> 0
* wms068863 211101 Unicode decoupling
* 4.0C
* xrp250698 Add functionality to determine path and location of
*           extract file for EC-CS
* 4.0B
* xfm064986 110298  Bilanztyp = Eröffnungsbilanz, wegen EURO nun
*                   möglich
* 3.1I
* VSK047132 051197  Datenextrakt für die Konsolidierung nun auch
*                   möglich für Bilanz/GuV-Strukturen auf der Basis
*                   von Konzernkontonummern (Annahme: in GLT3 steht
*                   immer das operative Konto).

CLASS: CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.

TYPE-POOLS: SLIS.                                                  "ALV

TABLES:   T001,  SKA1,  BHDGD,
          T011,  SKAT,
          T011T, SKB1,
          T009,  ZSKC1A,
          T004,  SKC1C,
          T009B,
          T009Y.

TABLES:   RFSDO.
TABLES:   TGSB.
TABLES:   FIMSG.
TABLES:   FAGLFREESEL.

UC_DATA.

FIELD-SYMBOLS: <F1>.

*---------------------------------------------------------------------*
*     D A T E N - D E F I N I T I O N E N                             *
*---------------------------------------------------------------------*

*     'H' =   Hilfsfelder, die jederzeit fuer Berechnungen
*     verwendet werden koennen.
*     ----------------------------------------------------
DATA: BEGIN OF H,
        UM01S      LIKE SKC1C-UM01S,
        UM01H      LIKE SKC1C-UM01H,
        UM01S2     LIKE ZSKC1A-UM01S2,
        UM01H2     LIKE ZSKC1A-UM01H2,
        WM01S      LIKE SKC1C-WM01S,
        WM01H      LIKE SKC1C-WM01H,
*...... reference field for internal calculation fields
        AMOUNT(9)  TYPE P DECIMALS 2,
        SALDO      LIKE H-AMOUNT,
        USALD      LIKE H-AMOUNT,
        SALDO2     LIKE H-AMOUNT,
        USALD2     LIKE H-AMOUNT,
        RELAB(16)  TYPE P DECIMALS 1,
        RELAB2(16) TYPE P DECIMALS 1,
        UMONAT(2)  TYPE P,
        OMONAT(2)  TYPE P,
        SIGN(1)    TYPE P,
        PRKEY      LIKE RF011P-PRKEY,
        ERGSL      LIKE T011-ERGAK,
        TEXT(15),
        SUBRC      LIKE SY-SUBRC,
        WAERS      LIKE T001-WAERS,
        FNAME(7)   TYPE C VALUE 'H-USALD',
      END   OF H.

*..... data declarations for output via ALV
CONSTANTS: CON_GRID(4) TYPE C        VALUE 'GRID',              " ALV
           CON_TREE(4) TYPE C        VALUE 'TREE',              " ALV
           CON_REPID   LIKE SY-REPID VALUE 'ZRFBILA01'.          " ALV
CONSTANTS: CON_FAGL_BHDGD_RLDNR(16) TYPE C VALUE 'FAGL_BHDGD_RLDNR'.
CONSTANTS: CON_FAGL_BHDGD_RLDN2(16) TYPE C VALUE 'FAGL_BHDGD_RLDN2'.
*TYPES: TT_BSPL          LIKE ZRFBILA_ALV_DATA OCCURS 0.           " ALV
TYPES: TT_BSPL          LIKE ZRFBILA_ALV_PERI OCCURS 0.           " ALV
DATA:  GT_BSPL          TYPE TT_BSPL WITH HEADER LINE.           " ALV
DATA:  GT_BSPLTOP       TYPE SLIS_T_LISTHEADER..                 " ALV
DATA:  GS_BSPL_SETTINGS LIKE RFBILA_ALV_SETTINGS.                " ALV
DATA:  GS_GRID_SETTINGS TYPE LVC_S_GLAY.                         " ALV
DATA:  G_GRID_TITLE     TYPE LVC_TITLE.                          " ALV
DATA:  G_RLDNR          LIKE SKC1A-RLDNR.
DATA:  G_RYEAR          LIKE RFBILA_ALV_DATA-RYEAR.
DATA:  G_POPER          LIKE RFBILA_ALV_DATA-POPER.

*     Übergabestruktur an SAP-EIS
*     ---------------------------
DATA: BEGIN OF EIS_REC,
        VERSN LIKE T011-VERSN,         " Bilanzversion
        PVERS LIKE RFPDO-BILAPVER,     " Planversion
        BASIS LIKE RF011P-ERGSL,       " Basisgröße
        KONTO LIKE SKA1-SAKNR,         " Kontonummer
        BUKRS LIKE SKB1-BUKRS,         " Buchungskreis
        GSBER LIKE SKC1A-GSBER,        " Gesch.bereich
        BJAHR LIKE BKPF-GJAHR,         " Berichtsjahr
        BMVON LIKE BKPF-MONAT,         " Berichtsmonat VON
        BMBIS LIKE BKPF-MONAT,         " Berichtsmonat BIS
        WAERS LIKE T001-WAERS,         " Währung
        BETRG LIKE H-AMOUNT,           " Betrag
        WAER2 LIKE T001-WAERS,         " Währung
        BETR2 LIKE H-AMOUNT,           " Betrag
      END   OF EIS_REC.
DATA: EIS_SUBRC LIKE SY-SUBRC,
      GRPID     LIKE T242X-GRPID.

DATA: READ_TABIX      LIKE SY-TABIX,
      ALTKT_NOT_FOUND TYPE C,
      ALTTX_NOT_FOUND TYPE C.
DATA: SAVE_SAKAN       LIKE SKA1-SAKAN.
DATA: LIST_CREATET     TYPE C VALUE ' '.
DATA: BEGIN OF HIGH_VALUE,
        SPACE TYPE C,
        HIGH  TYPE C,
      END   OF HIGH_VALUE.
HIGH_VALUE-HIGH = CL_ABAP_CHAR_UTILITIES=>MAXCHAR.
DATA: BEGIN OF X_FF_GSBER,
        FF(4) TYPE C,
      END   OF X_FF_GSBER.
TRANSLATE X_FF_GSBER USING HIGH_VALUE.

*     CTYP_WAERS enthällt den Währungsschlüssel gem. dem gewählten
*     Währungstyp auf den Selektionsbild.
*     ------------------------------------------------------------
DATA: CTYP_WAERS  LIKE T001-WAERS,
      CTYP_WAERS2 LIKE T001-WAERS.

*     Zwischenspeicher fuer Suchargument fuer Tabelle 011Q.
*     -----------------------------------------------------
DATA: BEGIN OF OLD,
        SUARG1 LIKE RF011Q-ERGSL,
      END   OF OLD.

*     Ergebnisschluessel der Aktiva repraesentiert
*     --------------------------------------------
DATA: BEGIN OF AKTVA,
        PRKEY LIKE RF011P-PRKEY,
      END   OF AKTVA.

*     Ergebnisschluessel der Passiva repraesentiert
*     ---------------------------------------------
DATA: BEGIN OF PSSVA,
        PRKEY LIKE RF011P-PRKEY,
      END   OF PSSVA.

*     Ergebnisschluessel der den Bilanzanhang repraesentiert
*     ------------------------------------------------------
DATA: BEGIN OF ANHNG,
        PRKEY LIKE RF011P-PRKEY,
      END   OF ANHNG.

*     Ergebnisschluessel fuer die nicht zugeordneten Konten
*     -----------------------------------------------------
DATA: BEGIN OF NZUON,
        PRKEY LIKE RF011P-PRKEY,
      END   OF NZUON.

*     Position des Ergebnisses in der Bilanz (positiv)
*     ------------------------------------------------
DATA: BEGIN OF E1POS,
        PRKEY LIKE RF011P-PRKEY,
      END   OF E1POS.

*     Position des Ergebnisses in der Bilanz (negativ)
*     ------------------------------------------------
DATA: BEGIN OF E1NEG,
        PRKEY LIKE RF011P-PRKEY,
      END   OF E1NEG.

*     Position des Ergebnisses in der G u V  (positiv)
*     ------------------------------------------------
DATA: BEGIN OF E2POS,
        PRKEY LIKE RF011P-PRKEY,
      END   OF E2POS.

*     Position des Ergebnisses in der G u V  (negativ)
*     ------------------------------------------------
DATA: BEGIN OF E2NEG,
        PRKEY LIKE RF011P-PRKEY,
      END   OF E2NEG.

*     length according to the level in fsv
DATA: AKTVA_LEN TYPE I VALUE 2,
      PSSVA_LEN TYPE I VALUE 2,
      NZUON_LEN TYPE I VALUE 2,
      ANHNG_LEN TYPE I VALUE 2.

*     Programmschluessel gemaess ausgewaehlter Version
*     ------------------------------------------------
DATA: BEGIN OF PRKEY,
        SOLL  LIKE RF011P-PRKEY,
        HABEN LIKE RF011P-PRKEY,
        VERD  LIKE RF011Z-XVERD,
      END   OF PRKEY.

*     Ergebnisschluessel gemaess ausgewaehlter Version
*     ------------------------------------------------
DATA: BEGIN OF ERGSL,
        SOLL  LIKE RF011Z-ERGSO,
        HABEN LIKE RF011Z-ERGHB,
      END   OF ERGSL.

*     In der Feldleiste PER werden die Berichts- und
*     Vergleichsperioden gesammelt.
*     ----------------------------------------------
DATA: BEGIN OF PER,
        BJJV LIKE BKPF-GJAHR,          " Ber.Jahr von
        BMMV LIKE BKPF-MONAT,          " Ber.Monat von
        BJJB LIKE BKPF-GJAHR,          " Ber.Jahr bis
        BMMB LIKE BKPF-MONAT,          " Ber.Monat bis
        VJJV LIKE BKPF-GJAHR,          " Ver.Jahr von
        VMMV LIKE BKPF-MONAT,          " Ver.Monat von
        VJJB LIKE BKPF-GJAHR,          " Ver.Jahr bis
        VMMB LIKE BKPF-MONAT,          " Ver.Monat bis
      END   OF PER.

*     Daten der Berichtsperiode
*     -------------------------
DATA: BEGIN OF BPER,
        EMBIL(2) TYPE P,               " Erster  Monat Bilanz
        LMBIL(2) TYPE P,               " Letzter Monat Bilanz
        EMGUV(2) TYPE P,               " Erster  Monat GuV
        LMGUV(2) TYPE P,               " Letzter Monat GuV
        ANZMO(2) TYPE P,               " Anzahl  Monate
      END   OF BPER.

*     Daten der Vergleichsperiode
*     ---------------------------
DATA: BEGIN OF VPER,
        EMBIL(2) TYPE P,               " Erster  Monat Bilanz
        LMBIL(2) TYPE P,               " Letzter Monat Bilanz
        EMGUV(2) TYPE P,               " Erster  Monat GuV
        LMGUV(2) TYPE P,               " Letzter Monat GuV
        ANZMO(2) TYPE P,               " Anzahl  Monate
      END   OF VPER.

*     ZUON_SAKNR
*     wird abhaengig von der Bilanzversion entweder mit mit der
*     normalen Kontonummer (SKA1-SAKNR) oder mit der Konzernkonto-
*     nummer (SKA1-BILKT) versorgt.
*     Mit ihr wird dann die Zuordnung eines Kontos zu einer Bilanz-
*     position getroffen.
DATA: ZUON_SAKNR  LIKE SKA1-SAKNR.

*     ZUON_KTOPL
*     wird abhaengig von der Bilanzversion entweder mit mit dem
*     normalen Kontenplan  (T001-KTOPL) oder mit der Konzernkonten-
*     plan (T004-KKTPL) versorgt.
*     Mit ihm wird dann die Zuordnung eines Kontos zu einer Bilanz-
*     position getroffen.
DATA: ZUON_KTOPL  LIKE T001-KTOPL.

*     TRIG-KZ
*     Bei zu verdichtenden Ergebnisschluesseln  entscheidet nicht
*     Saldo eines Kontos sondern der Gesamtsaldo der Gruppe ob die
*     Konten auf der Aktiv- oder Passivseite gezeigt werden.
*     Deshalb wird dieses Kennzeichen bei AT TRIGGER gesetzt um ggf
*     Saetze bei AT KONTEN zu unterdruecken.
*     -------------------------------------------------------------
DATA: TRIGKZ(1) TYPE C.

*     X011P Interne Tabelle der Bilanzpositionen
*     ------------------------------------------
DATA: BEGIN OF X011P OCCURS 250.
        INCLUDE STRUCTURE RF011P.
DATA: END   OF X011P.

*     X011Q Interne Tabelle der Bilanzpositionstexte
*     ----------------------------------------------
DATA: BEGIN OF X011Q OCCURS 900.
        INCLUDE STRUCTURE RF011Q.
DATA: END   OF X011Q.

*     X011Z Interne Tabelle der Bilanzzuordnung
*     -----------------------------------------
DATA: BEGIN OF X011Z OCCURS 1100.
        INCLUDE STRUCTURE RF011Z.
DATA: END   OF X011Z.

*     SAVE_X011Z Merker für Eintrag aus X011Z
*     ---------------------------------------
DATA: BEGIN OF SAVE_X011Z.
        INCLUDE STRUCTURE RF011Z.
DATA: END   OF SAVE_X011Z.

*     X011V Interne Tabelle der Verdichtungsgruppen
*     ---------------------------------------------
DATA: BEGIN OF X011V OCCURS 50.
        INCLUDE STRUCTURE RF011V.
DATA: END   OF X011V.

*     X011ZKEY Schluessel zum Zugriff auf Tabelle X011Z
*     -------------------------------------------------
DATA: BEGIN OF X011ZKEY,
        KTOPL LIKE RF011Z-KTOPL,
        BILKT LIKE RF011Z-BILKT,
      END   OF X011ZKEY.

*     X011VKEY Schluessel zum Zugriff auf Tabelle X011V
*     -------------------------------------------------
DATA: BEGIN OF X011VKEY,
        ERGS1 LIKE RF011V-ERGS1,
        ERGS2 LIKE RF011V-ERGS2,
      END   OF X011VKEY.

*     GTAB enthaelt die Berichts- und Vergleichssalden
*     pro Geschaeftsbereich.
*     ------------------------------------------------
DATA: BEGIN OF GTAB OCCURS 60,
        KTOPL     LIKE SKA1-KTOPL,
        SAKNR     LIKE SKA1-SAKNR,
        BUKRS     LIKE SKC1A-BUKRS,
        GSBER     LIKE SKC1A-GSBER,
        SAKAN     LIKE SKA1-SAKAN,
        PRKYS     LIKE RF011P-PRKEY,
        PRKYH     LIKE RF011P-PRKEY,
        VERD      LIKE RF011Z-XVERD,
        BSIGN(3)  TYPE P,

        "Moeda Relatório
        BSALD     LIKE H-AMOUNT,
        VSIGN(3)  TYPE P,
        VSALD     LIKE H-AMOUNT,

        "Moeda Paridade
        BSALD2    LIKE H-AMOUNT,
        VSIGN2(3) TYPE P,
        VSALD2    LIKE H-AMOUNT,

      END   OF GTAB.

*     Verdichtungstabelle
*     -------------------
DATA: BEGIN OF VTAB OCCURS 200,
        BUKRS     LIKE SKB1-BUKRS,
        GSBER     LIKE SKC1A-GSBER,
        PRKYS     LIKE RF011P-PRKEY,
        PRKYH     LIKE RF011P-PRKEY,
        VERDS     LIKE RF011Z-XVERD,
        WAERS     LIKE T001-WAERS,
        WAER2     LIKE T001-WAERS,

        "Moeda Relatório
        BSIGN(3)  TYPE P,
        BSALD     LIKE H-AMOUNT,
        VSIGN(3)  TYPE P,
        VSALD     LIKE H-AMOUNT,

        "Moeda Paridade
        BSIGN2(3) TYPE P,
        BSALD2    LIKE H-AMOUNT,
        VSIGN2(3) TYPE P,
        VSALD2    LIKE H-AMOUNT,

      END   OF VTAB.

*     Verdichtungstabelle (Schlüssel)
*     -------------------------------
DATA: BEGIN OF VTABKEY,
        BUKRS LIKE SKB1-BUKRS,
        GSBER LIKE SKC1A-GSBER,
        PRKYS LIKE RF011P-PRKEY,
        PRKYH LIKE RF011P-PRKEY,
        VERDS LIKE RF011Z-XVERD,
        WAERS LIKE T001-WAERS,
        WAER2 LIKE T001-WAERS,
      END   OF VTABKEY.

*     Sortfelder
*     ----------
DATA: BEGIN OF SORT,
        BUK1    LIKE SKB1-BUKRS,
        GSB1    LIKE SKC1A-GSBER,
        LIST(2) TYPE C,
        PKY1(2) TYPE C,
        PKY2(2) TYPE C,
        PKY3(2) TYPE C,
        PKY4(2) TYPE C,
        PKY5(2) TYPE C,
        PKY6(2) TYPE C,
        PKY7(2) TYPE C,
        PKY8(2) TYPE C,
        PKY9(2) TYPE C,
        PKY0(2) TYPE C,
        VERD    LIKE RF011Z-XVERD,
        KTNR    LIKE SKB1-SAKNR,
        BUK2    LIKE SKB1-BUKRS,
        GSB2    LIKE SKC1A-GSBER,
      END   OF SORT.

*     restliche EXTRACT-Felder
*     ------------------------
DATA: BEGIN OF DATE,
        PRKEY     LIKE RF011P-PRKEY,
        ERGSL     LIKE RF011Z-ERGSO,
        SAKNR     LIKE SKA1-SAKNR,                          "VSK047132
        SAKAN     LIKE SKA1-SAKAN,
        SKBEZ     LIKE SKAT-TXT50,

        "Moeda Relatório
        BKLAU(1)  TYPE C,
        BSALD     LIKE H-AMOUNT,
        BKLZU(1)  TYPE C,
        VKLAU(1)  TYPE C,
        VSALD     LIKE H-AMOUNT,
        VKLZU(1)  TYPE C,
        WAERS     LIKE T001-WAERS,

        "Moeda Paridade
        BKLAU2(1) TYPE C,
        BSALD2    LIKE H-AMOUNT,
        BKLZU2(1) TYPE C,
        VKLAU2(1) TYPE C,
        VSALD2    LIKE H-AMOUNT,
        VKLZU2(1) TYPE C,
        WAER2     LIKE T001-WAERS,
      END   OF DATE.

DATA BEGIN OF LT_FAGL_011PC OCCURS 5000.
        INCLUDE STRUCTURE FAGL_011PC.
DATA END OF LT_FAGL_011PC.


DATA: BEGIN OF LT_PARENT OCCURS 1000,
        PARENT LIKE FAGL_011PC-PARENT,
        COUNT  TYPE I,
      END OF LT_PARENT.

*     SORTBKRS = enthaelt den Sortierbegriff fuer Buchungskreis
*     Das Feld wird in Abhaengigkeit des Parameters BILABKON gefuellt
*     -----------------------------------------------------------------
DATA: SORTBKRS LIKE SKB1-BUKRS.

*     SORTGBER = enthaelt den Sortierbegriff fuer Geschaeftsbereich
*     Das Feld wird in Abhaengigkeit des Parameters BILAGKON gefuellt
*     -----------------------------------------------------------------
DATA: SORTGBER LIKE SKC1A-GSBER.

FIELD-GROUPS:
             HEADER,
             KONTEN,
             TRIGGER.
INSERT
  SORT-BUK1                            " Buchungskreis
  SORT-GSB1                            " Geschaeftsbereich
  SORT-LIST                            " Listenteil
  SORT-PKY1                                                 " Stufe 1
  SORT-PKY2                                                 " Stufe 2
  SORT-PKY3                                                 " Stufe 3
  SORT-PKY4                                                 " Stufe 4
  SORT-PKY5                                                 " Stufe 5
  SORT-PKY6                                                 " Stufe 6
  SORT-PKY7                                                 " Stufe 7
  SORT-PKY8                                                 " Stufe 8
  SORT-PKY9                                                 " Stufe 9
  SORT-PKY0                                                 " Stufe 10
  SORT-VERD                            " Verdichtungsschluessel
  SORT-KTNR                            " Sachkonto
  SORT-BUK2                            " Buchungskreis     (KONS)
  SORT-GSB2                            " Geschaeftsbereich (KONS)
INTO HEADER.

INSERT
  DATE-PRKEY                           " Programmschluessel
  DATE-ERGSL                           " Ergebnisschluessel
  DATE-SAKNR                           " Kontonr           "VSK047132
  DATE-SAKAN                           " Kontonummer in relev. Länge
  DATE-SKBEZ                           " Kontobezeichnung

  "Moeda Relatório
  DATE-BKLAU                           " Klammer auf    (B)
  DATE-BSALD                           " Saldo Berichtszeitraum
  DATE-BKLZU                           " Klammer zu     (B)
  DATE-VKLAU                           " Klammer auf    (V)
  DATE-VSALD                           " Saldo Vergleichszeitraum
  DATE-VKLZU                           " Klammer zu     (V)
  DATE-WAERS                           " Buchungskreis-Waehrung

  "Moeda Paridade
  DATE-BKLAU2                          " Klammer auf    (B)
  DATE-BSALD2                          " Saldo Berichtszeitraum
  DATE-BKLZU2                          " Klammer zu     (B)
  DATE-VKLAU2                          " Klammer auf    (V)
  DATE-VSALD2                          " Saldo Vergleichszeitraum
  DATE-VKLZU2                          " Klammer zu     (V)
  DATE-WAER2                           " Buchungskreis-Waehrung

  ERGSL-SOLL                           " for consolidation only
  ERGSL-HABEN                          " for consolidation only
INTO KONTEN.

INSERT
  DATE-PRKEY                           " Programmschluessel
  DATE-SKBEZ                           " Kontobezeichnung
  "Moeda Relatório
  DATE-BKLAU                           " Klammer auf    (B)
  DATE-BSALD                           " Saldo Berichtszeitraum
  DATE-BKLZU                           " Klammer zu     (B)
  DATE-VKLAU                           " Klammer auf    (V)
  DATE-VSALD                           " Saldo Vergleichszeitraum
  DATE-VKLZU                           " Klammer zu     (V)
  DATE-WAERS                           " Buchungskreis-Waehrung
  "Moeda Paridade
  DATE-BKLAU2                          " Klammer auf    (B)
  DATE-BSALD2                          " Saldo Berichtszeitraum
  DATE-BKLZU2                          " Klammer zu     (B)
  DATE-VKLAU2                          " Klammer auf    (V)
  DATE-VSALD2                          " Saldo Vergleichszeitraum
  DATE-VKLZU2                          " Klammer zu     (V)
  DATE-WAER2                           " Buchungskreis-Waehrung
INTO TRIGGER.

*     Schleifenzaehler
*     ----------------
DATA: LOOPCTR(3) TYPE P.

*     RESERVE = enthaelt die Anzahl der zu reservierenden
*     Zeilen am Seitenende
*     ---------------------------------------------------
DATA: RESERVE(3) TYPE P.

*     I011Q dient zur Zwischenspeicherung von Texten aus Tabelle X011Q.
*     Zur Gruppenanfangszeit werden alle fuer die jeweilige(n)
*     Gruppenstufe(n) benoetigten Texte in dieser Tabelle gesammelt
*     und vor Ausgabe der 1. Kontenzeile gedruckt. Damit soll
*     erreicht werden, dass die Gruppenanfangstexte nicht am
*     Seitenende auseinandergerissen, sondern ggf. auf eine neue
*     Seite gedruckt werden.
*     Zur Gruppenendezeit gilt das gleiche allerdings nur fuer die
*     jeweils zu Ende gehende Gruppenstufe. Summen fuer Berichts-
*     und Vergleichszeitraum werden dann ebenfalls in I011Q gesammelt.
*     ----------------------------------------------------------------
DATA: BEGIN OF I011Q OCCURS 20,
        SKIP(2)  TYPE P,               " Anzahl ZeilenVorschub.
        TEXT(45) TYPE C,               " Text aus T011Q
        BSUM     LIKE H-AMOUNT,        " Summe Berichtszeitraum
        VSUM     LIKE H-AMOUNT,        " Summe Vergleichszeitraum
        BSUM2    LIKE H-AMOUNT,        " Summe Berichtszeitraum
        VSUM2    LIKE H-AMOUNT,        " Summe Vergleichszeitraum
        STAR(5)  TYPE C,               " Summensterne
      END   OF I011Q.

*     I011P enthaelt die Programmschluessel sortiert nach Ergebnis-
*     rechnungsschluessel.
*     Aus ihr werden am Programmanfang Programmschluessel fuer
*       - das Bilanzergebnis in der Bilanz (positiv)
*       - das Bilanzergebnis in der Bilanz (negativ)
*       - das Bilanzergebnis in der GuV    (positiv)
*       - das Bilanzergebnis in der GuV    (negativ)
*       - die Konten, die Aktiva repraesentieren
*       - die Konten, die Passiva repraesentieren
*       - die Konten, die nicht zugeordnet sind
*     besorgt.
*     Waehrend der Selektion werden den Konten die Programmschluessel
*     zur Sortierung zugeordnet.
*     --------------------------------------------------------------
DATA: BEGIN OF I011P OCCURS 250,
        ERGSL LIKE RF011P-ERGSL,    " Bilanzsteuerungsschluessel
        PRKEY LIKE RF011P-PRKEY,    " Programmschluessel
        SUMME LIKE RF011P-SUMME,    " Summenkennzeichen
        STUFE LIKE RF011P-STUFE,    " Hierarchiestufe
      END   OF I011P.

*     Variable Ueberschrift
*     ---------------------
DATA: VARUEB(201).

*     Summenfelder fuer Staffelsummen
*     -------------------------------
DATA: BEGIN OF SS,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF SS.

*     Summenfelder fuer Programmschluessel Stufe 1
*     --------------------------------------------
DATA: BEGIN OF S1,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S1.

*     Summenfelder fuer Programmschluessel Stufe 2
*     --------------------------------------------
DATA: BEGIN OF S2,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S2.

*     Summenfelder fuer Programmschluessel Stufe 3
*     --------------------------------------------
DATA: BEGIN OF S3,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S3.

*     Summenfelder fuer Programmschluessel Stufe 4
*     --------------------------------------------
DATA: BEGIN OF S4,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S4.

*     Summenfelder fuer Programmschluessel Stufe 5
*     --------------------------------------------
DATA: BEGIN OF S5,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S5.

*     Summenfelder fuer Programmschluessel Stufe 6
*     --------------------------------------------
DATA: BEGIN OF S6,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S6.

*     Summenfelder fuer Programmschluessel Stufe 7
*     --------------------------------------------
DATA: BEGIN OF S7,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S7.

*     Summenfelder fuer Programmschluessel Stufe 8
*     --------------------------------------------
DATA: BEGIN OF S8,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S8.

*     Summenfelder fuer Programmschluessel Stufe 9
*     --------------------------------------------
DATA: BEGIN OF S9,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S9.

*     Summenfelder fuer Programmschluessel Stufe 10
*     ---------------------------------------------
DATA: BEGIN OF S0,
        "Moeda Relatório
        BSUM  LIKE H-AMOUNT,
        VSUM  LIKE H-AMOUNT,
        "Moeda Paridade
        BSUM2 LIKE H-AMOUNT,
        VSUM2 LIKE H-AMOUNT,
      END   OF S0.

INCLUDE ZBILAI01_1.
*INCLUDE ZBILAI00_1.                                         "SCRIPT

DATA: GD_FNAME         LIKE FC03TAB-PL00_LFILE,             "xrp250698
      GD_PRES          LIKE FCINTAB-FLG_PRES,               "xrp250698
      GD_LEAVE_PROGRAM.                                     "xrp250698

DATA: BILVJAHR LIKE BKPF-GJAHR,
      V-MONATE TYPE RANGE OF RFSDO-BILAVMON WITH HEADER LINE,
      BILATREE TYPE C,
*      BILATVAR TYPE TABLE OF SLIS_VARI WITH HEADER LINE,
      PLANVERS LIKE SKC1A-RVERS.

*EJECT

BEGIN_OF_SCREEN 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(30) TEXT-030 FOR FIELD BILAVERS.
SELECTION-SCREEN POSITION       35.
PARAMETERS:     BILAVERS LIKE T011T-VERSN    MEMORY ID BIL
                                             OBLIGATORY.
SELECTION-SCREEN COMMENT 40(8) TEXT-031 FOR FIELD BILASPRA.
SELECTION-SCREEN POSITION       49.
PARAMETERS:     BILASPRA LIKE T011-DSPRA     DEFAULT SY-LANGU
                                             OBLIGATORY.
SELECTION-SCREEN END   OF LINE.
PARAMETERS:     BILBJAHR LIKE BKPF-GJAHR OBLIGATORY.
SELECT-OPTIONS: B-MONATE FOR  RFSDO-BILABMON NO-EXTENSION
                                             OBLIGATORY.

*PARAMETERS:     BILVJAHR LIKE BKPF-GJAHR.
*SELECT-OPTIONS: V-MONATE FOR  RFSDO-BILAVMON NO-EXTENSION
*                                             OBLIGATORY.
*PARAMETERS:     planvers LIKE rfpdo-bilapver DEFAULT space.
*PARAMETERS:     PLANVERS LIKE SKC1A-RVERS DEFAULT SPACE.

SELECTION-SCREEN SKIP 1.                                           "ALV
* list layout frame:
SELECTION-SCREEN BEGIN OF BLOCK LIST WITH FRAME TITLE TEXT-204.    "ALV

"SELECTION-SCREEN BEGIN OF LINE.
"PARAMETERS: bilalist LIKE rfbila_alv_settings-classic RADIOBUTTON GROUP alv.
"SELECTION-SCREEN COMMENT 3(20) text-205 FOR FIELD bilalist.
"SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: BILAGRID LIKE RFBILA_ALV_SETTINGS-GRID DEFAULT 'X'.
*                                       CHE GROUP ALV.
SELECTION-SCREEN COMMENT  3(20) TEXT-206 FOR FIELD BILAGRID.
SELECTION-SCREEN COMMENT 25(10) TEXT-209 FOR FIELD BILAGVAR.
SELECTION-SCREEN POSITION  40.
PARAMETERS: BILAGVAR TYPE SLIS_VARI
                         MEMORY ID GL_BSPL_ALV_GRID_VAR.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: BILATREE LIKE RFBILA_ALV_SETTINGS-TREE
*                                       RADIOBUTTON GROUP ALV.
*SELECTION-SCREEN COMMENT  3(20) TEXT-207 FOR FIELD BILATREE.
*SELECTION-SCREEN COMMENT 25(10) TEXT-209 FOR FIELD BILATVAR.
*SELECTION-SCREEN COMMENT  3(20) FOR FIELD BILATREE.
*SELECTION-SCREEN COMMENT 25(10) FOR FIELD BILATVAR.
*SELECTION-SCREEN POSITION  40.
*PARAMETERS: BILATVAR TYPE SLIS_VARI
*                         MEMORY ID GL_BSPL_ALV_TREE_VAR.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION   4.
*PARAMETERS: BILASTSL LIKE RFBILA_ALV_SETTINGS-STRUCBLNCE
*                                        AS CHECKBOX.
*SELECTION-SCREEN COMMENT 6(40) TEXT-208 FOR FIELD BILASTSL.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END OF BLOCK LIST.                                "ALV
*... end of selection screen layout.
* BEGIN Insert n1594124
*ENHANCEMENT-POINT SELE01 SPOTS FI_BILA_PRINT STATIC .
* END Insert n1594124
END_OF_SCREEN 1.
BEGIN_OF_SCREEN 2.
PARAMETERS:
  BILABKON  LIKE RFPDO-BILABKON OBLIGATORY
                                DEFAULT '1',
  BILAGKON  LIKE RFPDO-BILAGKON OBLIGATORY
                                DEFAULT '2',
  BILASUMM  LIKE RFPDO-BILASUMM DEFAULT SPACE,
  P_SUMM(1) NO-DISPLAY,                                     "n1486648
  BILAVART  LIKE RFPDO-BILAVTYP DEFAULT '1',
  BILASKAL  LIKE RFPDO-BILASKAL DEFAULT '0/0',
  ALLGLSEP  LIKE RFPDO-ALLGLSEP  DEFAULT SPACE,
  MIKFICHE  LIKE RFPDO-BILAMIKF  DEFAULT SPACE,
  ALLGLINE  LIKE RFPDO-ALLGLINE DEFAULT SPACE.
PARAMETERS:     BILAPAGE LIKE RFPDO3-BILAPAGNO DEFAULT 1.
INCLUDE ZBILAI01_2.
*INCLUDE ZBILAI00_2.                                       "SCRIPT
END_OF_SCREEN 2.

SELECTION-SCREEN BEGIN OF SCREEN 0030 AS SUBSCREEN.
PARAMETERS:     BILABTYP LIKE RFPDO-BILABTYP DEFAULT '1',
                ALLGALTK LIKE RFPDO1-ALLGALTK DEFAULT SPACE,
                HOCHRECH LIKE RFPDO-BILAHOCH DEFAULT SPACE,
                BILANULL LIKE RFPDO-BILANULL DEFAULT SPACE,
                LOE-VORM LIKE RFPDO-BILALOVM DEFAULT SPACE,
                REPWAERS LIKE RFPDO-BILAWAER DEFAULT SPACE,
                STICHTAG LIKE RFPDO-BILASTID DEFAULT SY-DATUM,
                KURS_TYP LIKE TCURR-KURST    DEFAULT 'M   ',
                KALZEITR LIKE RFPDO-BILAKALZ DEFAULT SPACE.
SELECT-OPTIONS: KALZEITB FOR  RFSDO-BILAKALB NO-EXTENSION,
                KALZEITV FOR  RFSDO-BILAKALV NO-EXTENSION.
PARAMETERS:     FI_LC_EX LIKE RFPDO-BILAFILC  DEFAULT SPACE,
                BILA_EIS LIKE RFPDO-BILA_EIS  DEFAULT SPACE.
PARAMETER:      BILAINFL LIKE J_1ASINFF-COMPPERIOD DEFAULT SPACE.
SELECTION-SCREEN END   OF SCREEN 0030.

* Definition of TabStrips
*BEGIN_OF_TABBED_BLOCK: 1 14.
SELECTION-SCREEN BEGIN OF TABBED BLOCK TABBL1 FOR 14 LINES.
TABSTRIP: 1 (25).
SELECTION-SCREEN TAB (25) TAB30
                      USER-COMMAND UCOM30
                      DEFAULT SCREEN 0030.
TABSTRIP: 2 (25).
SELECTION-SCREEN END OF BLOCK TABBL1.

INCLUDE ZRKASMAWF01.
*INCLUDE ZRKASMAWF.

SELECTION-SCREEN BEGIN OF SCREEN 2000 AS WINDOW TITLE TEXT-S01.
                                                            "xrp250698
SELECTION-SCREEN COMMENT /1(80) TEXT-S02.                   "xrp250698
*  SELECTION-SCREEN COMMENT /1(80) TEXT-S03.  "xrp250698   "xrp260704
SELECTION-SCREEN SKIP.                                      "xrp250698
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-F01.
                                                            "xrp250698
SELECTION-SCREEN BEGIN OF LINE.                             "xrp250698
SELECTION-SCREEN POSITION 01.                               "xrp250698
PARAMETERS  PA_PRES RADIOBUTTON GROUP R01.                  "xrp250698
SELECTION-SCREEN COMMENT 03(30) TEXT-S04                    "xrp250698
            FOR FIELD PA_PRES.                              "xrp250698
SELECTION-SCREEN END OF LINE.                               "xrp250698
SELECTION-SCREEN BEGIN OF LINE.                             "xrp250698
SELECTION-SCREEN POSITION 01.                               "xrp250698
PARAMETERS  PA_APPL RADIOBUTTON GROUP R01.                  "xrp250698
SELECTION-SCREEN COMMENT 03(30) TEXT-S05                    "xrp250698
            FOR FIELD PA_APPL.                              "xrp250698
SELECTION-SCREEN END OF LINE.                               "xrp250698
SELECTION-SCREEN BEGIN OF LINE.                             "xrp250698
SELECTION-SCREEN COMMENT 01(30) TEXT-S06                    "xrp250698
            FOR FIELD PA_FNAME.                             "xrp250698
PARAMETERS  PA_FNAME LIKE FC03TAB-PL00_LFILE OBLIGATORY.    "xrp250698
SELECTION-SCREEN END OF LINE.                               "xrp250698
SELECTION-SCREEN END OF BLOCK B01.                          "xrp250698
PARAMETERS: PA_RBCS TYPE FC_FLG AS CHECKBOX.                "xrp290904
                                                            "xrp250698
SELECTION-SCREEN END OF SCREEN 2000.                        "xrp250698

*BILA&I00 replaced by BILAI00_1 and BILAI00_2
*INCLUDE BILA&I00.                      "SCRIPT

*EJECT
*--------------------------------------------------------------------*
*        INITIALIZATION                                              *
*--------------------------------------------------------------------*
INITIALIZATION.
  GET_TABSTRIP_TITLE: 1,2.
  TAB30 = TEXT-051.
* Vorschlagswerte Berichts- und Vergleichsjahr
* --------------------------------------------
  BILVJAHR = BILBJAHR.

  GET PARAMETER ID 'BUK' FIELD SORTBKRS.
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = SORTBKRS
    IMPORTING
      CURRY = BILBJAHR
      PREVY = BILVJAHR.

  IF BILBJAHR IS INITIAL.
    BILBJAHR = SY-DATUM(4).
*    BILVJAHR = BILBJAHR - 1.
  ENDIF.

* Vorschlagswerte Berichtsperioden
* --------------------------------
  CALL FUNCTION 'BUILD_DEFAULT_PERIOD'
    EXPORTING
      I_USE_MSGTY = 'E'
    TABLES
      XMONAT      = B-MONATE.

* Vorschlagswerte Vergleichsperioden
* ----------------------------------
  CALL FUNCTION 'BUILD_DEFAULT_PERIOD'
    EXPORTING
      I_USE_MSGTY = 'E'
    TABLES
      XMONAT      = V-MONATE.

* Pruefen Berichtsperioden
* ------------------------
AT SELECTION-SCREEN ON B-MONATE.
  CALL FUNCTION 'BUILD_DEFAULT_PERIOD'
    EXPORTING
      I_USE_MSGTY = 'E'
    TABLES
      XMONAT      = B-MONATE.

* Pruefen Vergleichsperioden
* --------------------------
*AT SELECTION-SCREEN ON V-MONATE.
*  CALL FUNCTION 'BUILD_DEFAULT_PERIOD'
*    EXPORTING
*      I_USE_MSGTY = 'E'
*    TABLES
*      XMONAT      = V-MONATE.

*AT SELECTION-SCREEN ON RADIOBUTTON GROUP ALV.
*  IF bilalist = 'X'.
*    REFRESH lt_fagl_011pc. CLEAR lt_fagl_011pc.
*    REFRESH lt_parent. CLEAR lt_parent.
*    SELECT * FROM fagl_011pc CLIENT SPECIFIED INTO TABLE lt_fagl_011pc
*              WHERE mandt = sy-mandt
*                AND versn = bilavers.
*
*    LOOP AT lt_fagl_011pc.
**      mehr als 10 Hierarchiestufen? -> ALV
*      IF lt_fagl_011pc-stufe > 11.
*        MESSAGE e752(fe) WITH bilavers lt_fagl_011pc-ergsl.
*      ENDIF.
*      lt_parent-count = 1.
*      lt_parent-parent = lt_fagl_011pc-parent.
*      COLLECT lt_parent.
*    ENDLOOP.
**    mehr als 99 Unterpositionen? -> ALV
*    LOOP AT lt_parent WHERE count > 99.
*      READ TABLE lt_fagl_011pc WITH KEY id = lt_parent-parent.
*      MESSAGE e753(fe) WITH lt_fagl_011pc-ergsl bilavers.
*    ENDLOOP.
*  ENDIF.

AT SELECTION-SCREEN ON BILAVERS.
* check fin.statement version
  SELECT SINGLE * FROM  T011 WHERE VERSN = BILAVERS.
  IF SY-SUBRC NE 0.
    MESSAGE E714(FE) WITH BILAVERS.
  ENDIF..
  AUTHORITY-CHECK OBJECT 'F_T011'
    ID 'VERSN' FIELD BILAVERS
    ID 'ACTVT' FIELD '03'.
  IF SY-SUBRC <> 0.
    MESSAGE E031(FE).
  ENDIF.

AT SELECTION-SCREEN ON BILASUMM.                            "n1486648
  IF BILASUMM CN ' 0123456789'.                             "n1486648
    MESSAGE E052(00).                                       "n1486648
  ENDIF.                                                    "n1486648

AT SELECTION-SCREEN OUTPUT.                                      "ALV
  IF BILAGRID IS INITIAL                                         "ALV
 AND BILATREE IS INITIAL.                                        "ALV
*    IF syst-slset IS INITIAL.                              "n1060564
*      bilagrid = 'X'.                                      "n1060564
*    ELSE.                                                  "n1060564
    BILAGRID = 'X'.                                            "ALV
*    ENDIF.                                                 "n1060564
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR BILAGVAR.                "ALV
  PERFORM BSPL_ALV_VARIANT_F4 USING    CON_REPID                  "ALV
                                       CON_GRID                   "ALV
                              CHANGING BILAGVAR.                  "ALV

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR BILATVAR.                "ALV
*  PERFORM BSPL_ALV_VARIANT_F4 USING    CON_REPID                  "ALV
*                                       CON_TREE                   "ALV
*                              CHANGING BILATVAR.                  "ALV

AT SELECTION-SCREEN.
  IF BILA_EIS EQ 'X' AND FI_LC_EX <> SPACE.
    MESSAGE E394.
  ENDIF.
  IF BILA_EIS EQ 'X'.
    BILABKON = '1'.
    BILAGKON = '2'.
    BILASUMM = ' '.
*   BILABTYP = '3'.
  ENDIF.

  IF FI_LC_EX = '1' OR FI_LC_EX = '2'.
    IF SD_CURTP <> SPACE AND SD_CURTP <> '10'.
      MESSAGE E395.
    ENDIF.
  ENDIF.
  IF FI_LC_EX = '1' OR FI_LC_EX = '2'.
    IF BILABTYP <> '1' AND BILABTYP <> '4'.                 "xfm064986
      MESSAGE E396.
    ENDIF.
  ENDIF.
  IF FI_LC_EX = '1'.
*   Extrakt an FI_LC
*   Geschaeftsbereichskonsolidierung muß gesetzt werden.
*   >> ein Satz pro Buchungskreis <<
*   ---------------------------------------------------
    BILABKON  = '1'.
    BILAGKON  = '3'.
    BILASUMM  = ' '.
  ENDIF.

  IF FI_LC_EX = '2'.
*   Extrakt an FI_LC
*   Geschaeftsbereiche einzeln muß gesetzt werden.
*   >> ein Satz pro globalem Geschäftsbereich <<
*   ----------------------------------------------
    BILABKON  = '1'.
    BILAGKON  = '1'.
    BILASUMM  = ' '.
  ENDIF.

  PERFORM CHECK_OUTPUT_OPTIONS.                             "n1060564

  INCLUDE ZBILA_I0101.
*  INCLUDE ZBILA_I01.                    "SCRIPT

*EJECT
START-OF-SELECTION.

  BILVJAHR = BILBJAHR.

  IF NOT (  FI_LC_EX IS INITIAL                                 "ALV
        AND BILA_EIS IS INITIAL ).                              "ALV
    CLEAR: BILATREE,                                            "ALV
           BILAGRID.                                            "ALV
  ENDIF.                                                        "ALV

  P_SUMM = BILASUMM.                                        "n1486648

* determine fisc. year, period for ALV output
  IF BILBJAHR = BILVJAHR.
    G_RYEAR = BILBJAHR.
  ENDIF.
  IF B-MONATE-LOW  = V-MONATE-LOW
 AND B-MONATE-HIGH = V-MONATE-HIGH.
    G_POPER = B-MONATE-HIGH.
  ENDIF.

* registration for shedule manager
  PERFORM SCHEDMAN_START_STOP USING 'START'.

* Extract to consolidation required                        "xrp250698
  IF FI_LC_EX <> SPACE.                                     "xrp250698
*   get logical filename for extract file through popup    "xrp250698
*   if EC-CS or FI-LC is active                "xrp250698  "xrp260704
    PERFORM FNAME_FOR_ECCS_GET CHANGING GD_FNAME            "xrp250698
                                        GD_PRES             "xrp250698
                                        GD_LEAVE_PROGRAM.   "xrp250698
*   GD_LEAVE_PROGRAM = 'X' => User cancelled file name entry "xrp250698
    IF GD_LEAVE_PROGRAM = 'X'.                              "xrp250698
      EXIT.                                                 "xrp250698
    ENDIF.                                                  "xrp250698
  ENDIF.                                                    "xrp250698

  IF SY-LANGU <> BILASPRA.
    SET LANGUAGE BILASPRA.
    PERFORM SET_LANGUAGE(RSBTCHH0) USING BILASPRA IF FOUND.
  ENDIF.

  CLEAR T004.
* Geschaeftsjahre in die DB-Abgrenzung SD_GJAHR
* uebernehmen um DB-Zugriffe zu sparen.
* ---------------------------------------------
  IF KALZEITR = SPACE.
    SD_GJAHR-SIGN   = 'I'.
    SD_GJAHR-OPTION = 'EQ'.
    SD_GJAHR-LOW    = BILBJAHR.
    APPEND SD_GJAHR.
    IF PLANVERS = SPACE.
      SD_GJAHR-SIGN   = 'I'.
      SD_GJAHR-OPTION = 'EQ'.
      SD_GJAHR-LOW    = BILVJAHR.
      APPEND SD_GJAHR.
    ENDIF.
  ELSE.
*   Datum in Kopreintrag bereitstellen
*   ----------------------------------
    READ TABLE KALZEITB INDEX 1.
    READ TABLE KALZEITV INDEX 1.
  ENDIF.

* Planversion in die DB-Abgrenzung SD_VERS
* uebernehmen.
* ----------------------------------------
  IF PLANVERS <> SPACE.
    SD_VERS-SIGN   = 'I'.
    SD_VERS-OPTION = 'EQ'.
    SD_VERS-LOW    = PLANVERS.
    APPEND SD_VERS.
  ENDIF.

* Standardseitenkopf fuellen.
* ---------------------------
  PERFORM FILL_BHDGD.

  SELECT SINGLE * FROM  T011
                  WHERE VERSN = BILAVERS.
  IF SY-SUBRC NE 0.
    MESSAGE E714(FE) WITH BILAVERS.
  ELSE.
*   Import der Daten von RFDT
*   -------------------------
    CALL FUNCTION 'FI_IMPORT_BALANCE_SHEET_POS'
      EXPORTING
        VERSION           = T011-VERSN
      TABLES
        X011P             = X011P
        X011V             = X011V
        I011Z             = X011Z
      EXCEPTIONS
        NEW_BALANCE_SHEET = 04.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'FI_IMPORT_BALANCE_SHEET_TEXT'
        EXPORTING
          SPRACHE        = BILASPRA
          VERSION        = T011-VERSN
        TABLES
          X011Q          = X011Q
        EXCEPTIONS
          TEXT_NOT_FOUND = 04.
      IF SY-SUBRC <> 0.
        CALL FUNCTION 'FI_IMPORT_BALANCE_SHEET_TEXT'
          EXPORTING
            SPRACHE        = T011-DSPRA
            VERSION        = T011-VERSN
          TABLES
            X011Q          = X011Q
          EXCEPTIONS
            TEXT_NOT_FOUND = 04.
      ENDIF.
    ELSE.
      MESSAGE W714(FE) WITH BILAVERS.
    ENDIF.
  ENDIF.

* Programmschluessel fuer die Position des Bilanzergebnisses in der
* Bilanz und GuV ermitteln.
* -----------------------------------------------------------------
  PERFORM SUCHEN_ERGEBNIS_POSITION USING BILAVERS.

  IF NOT SY-BATCH IS INITIAL AND SCR_PRINT_TO_FORM = YES.
    PERFORM SCR_CHECK_PRINTER_FILLED.
  ENDIF.

* Uebernahme der eventuell abgeaenderten Parameter
* in die Feldleiste PER.
* ------------------------------------------------
  CLEAR PER.
  MOVE: BILBJAHR         TO PER-BJJV," Ber.Jahr  von
        B-MONATE-LOW     TO PER-BMMV," Ber.Monat von
        BILBJAHR         TO PER-BJJB," Ber.Jahr  bis
        B-MONATE-HIGH    TO PER-BMMB," Ber.Monat bis
        BILVJAHR         TO PER-VJJV," Ver.Jahr  von
        V-MONATE-LOW     TO PER-VMMV," Ver.Monat von
        BILVJAHR         TO PER-VJJB," Ver.Jahr  bis
        V-MONATE-HIGH    TO PER-VMMB." Ver.Monat bis
  IF BILABTYP = '4'.
*.. Eröffnungsbilanz
    CLEAR: PER-BMMV,                 " Ber.Monat von
           PER-BMMB,                 " Ber.Monat bis
           PER-VMMV,                 " Ver.Monat von
           PER-VMMB.                 " Ver.Monat bis
  ENDIF.

  IF PLANVERS <> SPACE.
*   Planversion, Vergleichsperiode gleich Berichtsperiode
*   -----------------------------------------------------
    PER-VMMV = PER-BMMV.
    PER-VMMB = PER-BMMB.
  ENDIF.


*EJECT
  GET SKA1.
  CHECK SELECT-OPTIONS.

  REFRESH: GTAB.
  CLEAR:   CTYP_WAERS.
  CLEAR:   PRKEY.

  IF T011-XERGS = 'X'.
*   Konzernkontonummern verwenden
*   -----------------------------
    IF T004-KTOPL NE SKA1-KTOPL.
      SELECT SINGLE * FROM T004 WHERE "#EC CI_DB_OPERATION_OK[2389136]
                                KTOPL = SKA1-KTOPL.
    ENDIF.
    ZUON_SAKNR = SKA1-BILKT.
    ZUON_KTOPL = T004-KKTPL.

*   Sachkonto auf relevante Länge stutzen
*   ------------------------------------
    SKA1-SAKAN = SKA1-BILKT.
    IF T004-SAKLN > 0.
      DESCRIBE FIELD SKA1-BILKT LENGTH SY-FDPOS IN CHARACTER MODE.
      SY-FDPOS = SY-FDPOS - T004-SAKLN.
      IF SKA1-SAKAN CO '0123456789'.
        WHILE SY-INDEX LE SY-FDPOS AND SKA1-SAKAN(1) = '0'.
          SHIFT SKA1-SAKAN.
        ENDWHILE.
      ENDIF.
    ENDIF.
  ELSE.
*   normaler Kontenplan
*   -------------------
    ZUON_SAKNR = SKA1-SAKNR.
    ZUON_KTOPL = SKA1-KTOPL.
  ENDIF.

  IF T011-XERGS = 'X' OR SY-LANGU  <> BILASPRA.
*   Bezeichnung Konzernkontonummer
*   ------------------------------
    CALL FUNCTION 'READ_HAUPTBUCH_TEXT'
      EXPORTING
        KONTENPLAN     = ZUON_KTOPL
        SACHKONTO      = ZUON_SAKNR
        SPRACHE        = BILASPRA
      IMPORTING
        TEXT_WA        = SKAT
      EXCEPTIONS
        TEXT_NOT_FOUND = 04.
  ENDIF.

  GET SKB1.
  CHECK SELECT-OPTIONS.

  CLEAR:  GTAB, H.
  IF BILABKON <= '2'.
    CLEAR:  CTYP_WAERS.
    REFRESH GTAB.
  ENDIF.

* Zu den selektierten SKB1 Saetzen die zugehoerigen Ergebnis- und
* Programmschluessel aus den Tabellen X011Z und I011P besorgen.
* ---------------------------------------------------------------

* Kontenplan aus Tabelle T001
* ---------------------------
  IF SKB1-BUKRS NE T001-BUKRS.
    SELECT SINGLE * FROM T001 WHERE BUKRS = SKB1-BUKRS.
  ENDIF.

  IF ALLGALTK = 'X'.
*   Alternative Kontonummer gewünscht
*   ---------------------------------
    SAVE_SAKAN = SKA1-SAKAN.
    CALL FUNCTION 'READ_SACHKONTO_ALTKT'
      EXPORTING
        ALTKT_I         = SKB1-ALTKT
        BUKRS           = SKB1-BUKRS
        SAKNR           = SKA1-SAKNR
        SPRAS           = BILASPRA
        XSKAN           = 'X'
        XTEXT           = 'X'
      IMPORTING
        ALTKT           = SKB1-ALTKT
        ALTKT_SAKAN     = SKA1-SAKAN
        LTEXT           = SKAT-TXT50
        ALTKT_NOT_FOUND = ALTKT_NOT_FOUND
        TEXT_NOT_FOUND  = ALTTX_NOT_FOUND.

    IF ALTKT_NOT_FOUND <> 'X' AND ALTTX_NOT_FOUND <> 'X'.
*     Alles Ok.
*     ---------
      ZUON_SAKNR = SKB1-ALTKT.
      ZUON_KTOPL = T001-KTOP2.
    ELSE.
      IF ALTKT_NOT_FOUND = 'X'.
*       Alternative Kontonummer nicht gepflegt
*       --------------------------------------
        CLEAR FIMSG.
        FIMSG-MSORT = '0006'.
        FIMSG-MSGID = 'FR'.
        FIMSG-MSGTY = 'E'.
        FIMSG-MSGNO = '319'.
        FIMSG-MSGV1 = SKA1-SAKNR.
        FIMSG-MSGV2 = SKB1-BUKRS.
        CALL FUNCTION 'FI_MESSAGE_COLLECT'
          EXPORTING
            I_FIMSG = FIMSG.
        SKA1-SAKAN = SAVE_SAKAN.
      ENDIF.

      IF ALTTX_NOT_FOUND = 'X'.
*       Text zur alternativen Kontonummer nicht gepflegt
*       ------------------------------------------------
        CLEAR FIMSG.
        FIMSG-MSORT = '0007'.
        FIMSG-MSGID = 'FR'.
        FIMSG-MSGTY = 'E'.
        FIMSG-MSGNO = '316'.
        FIMSG-MSGV1 = SKB1-ALTKT.
        FIMSG-MSGV2 = T001-KTOP2.
        CALL FUNCTION 'FI_MESSAGE_COLLECT'
          EXPORTING
            I_FIMSG = FIMSG.
        ZUON_SAKNR = SKB1-ALTKT.
        ZUON_KTOPL = T001-KTOP2.
      ENDIF.
    ENDIF.
  ENDIF.

* Ergebnisschluessel Soll und Haben aus Tabelle X011Z
* ---------------------------------------------------
  CLEAR PRKEY.
  X011ZKEY-KTOPL = ZUON_KTOPL.
  X011ZKEY-BILKT = ZUON_SAKNR.
  READ TABLE X011Z WITH KEY X011ZKEY BINARY SEARCH.

  CASE SY-SUBRC.
    WHEN 0.
*   OK, nichts machen
*   -----------------
    WHEN 4.
*   nicht eindeutig, den nächst größeren nehmen
*   -------------------------------------------
      READ TABLE X011Z INDEX SY-TABIX.
    WHEN 8.
*   erzwinge Nicht-Zuordenbarkeit
*   -----------------------------
      CLEAR X011Z.
  ENDCASE.

  IF ZUON_SAKNR LE X011Z-BILKT AND ZUON_SAKNR GE X011Z-VONKT AND ZUON_KTOPL EQ X011Z-KTOPL.
*   Konto liegt im gefundenen Intervall
*   -----------------------------------
    IF X011Z-ERGSO = SPACE
    OR X011Z-ERGHB = SPACE.
*     Soll- oder Haben-Zuordnung fehlen
*     ---------------------------------
      SAVE_X011Z = X011Z.
      READ_TABIX = SY-TABIX.
      DO.
        READ_TABIX = READ_TABIX + 1.
        READ TABLE X011Z INDEX READ_TABIX.
        IF SY-SUBRC = 0.
          IF ZUON_SAKNR LE X011Z-BILKT AND
             ZUON_SAKNR GE X011Z-VONKT AND
             ZUON_KTOPL EQ X011Z-KTOPL.
*           Konto liegt im gefundenen Intervall
*           -----------------------------------
            IF SAVE_X011Z-ERGSO =  SPACE AND X011Z-ERGSO <> SPACE.
*             Soll-Zuordnung ergänzen
*             -----------------------
              SAVE_X011Z-ERGSO = X011Z-ERGSO.
            ENDIF.
            IF SAVE_X011Z-ERGHB =  SPACE AND X011Z-ERGHB <> SPACE.
*             Haben-Zuordnung ergänzen
*             ------------------------
              SAVE_X011Z-ERGHB = X011Z-ERGHB.
            ENDIF.
            IF SAVE_X011Z-ERGSO <> SPACE AND SAVE_X011Z-ERGHB <> SPACE.
*             alle Zuordnungen gefunden
*             -------------------------
              EXIT.                    ">>>>>>>>>>>>>>>>>
            ENDIF.
          ELSE.
*           keine passenden Einträge mehr vorhanden
*           ---------------------------------------
*           EXIT.                      ">>>>>>>>>>>>>>>>>       "H68224
          ENDIF.
        ELSE.
*         keine Einträge mehr vorhanden
*         -----------------------------
          EXIT.                        ">>>>>>>>>>>>>>>>>
        ENDIF.
      ENDDO.
*     Restore X011z-Eintrag
*     ----------------------
      X011Z = SAVE_X011Z.
    ENDIF.
    SY-SUBRC = 0.
  ELSE.
    SY-SUBRC = 4.
  ENDIF.

  IF SY-SUBRC = 0.
*   Pruefen, ob Verdichtungsgruppe vorliegt
*   ---------------------------------------
    X011VKEY-ERGS1 = X011Z-ERGSO.
    X011VKEY-ERGS2 = X011Z-ERGHB.
    READ TABLE X011V WITH KEY X011VKEY BINARY SEARCH.
    IF SY-SUBRC = 0.
      PRKEY-VERD = 'X'.
    ENDIF.

*   Programmschluessel Soll  aus Tabelle I011P
*   Ergebnisschluessel Soll  aus Tabelle X011Z
*   ------------------------------------------
    READ TABLE I011P WITH KEY X011Z-ERGSO BINARY SEARCH.
    IF SY-SUBRC = 0.
      PRKEY-SOLL  = I011P-PRKEY.
      ERGSL-SOLL  = X011Z-ERGSO.
    ELSE.
      PRKEY-SOLL  = NZUON-PRKEY.
      ERGSL-SOLL  = T011-ZUORD.
    ENDIF.

*   Programmschluessel Haben aus Tabelle I011P
*   Ergebnisschluessel Haben aus Tabelle X011Z
*   ------------------------------------------
    READ TABLE I011P WITH KEY X011Z-ERGHB BINARY SEARCH.
    IF SY-SUBRC = 0.
      PRKEY-HABEN = I011P-PRKEY.
      ERGSL-HABEN = X011Z-ERGHB.
    ELSE.
      PRKEY-HABEN = NZUON-PRKEY.
      ERGSL-HABEN = T011-ZUORD.
    ENDIF.
  ELSE.
*   Konto ist nicht zugeordnet
*   --------------------------
    PRKEY-SOLL  = NZUON-PRKEY.
    PRKEY-HABEN = NZUON-PRKEY.
    ERGSL-SOLL  = T011-ZUORD.
    ERGSL-HABEN = T011-ZUORD.
  ENDIF.

  IF BILABKON = '1'.
*   Buchungskreise einzeln
*   ----------------------
    SORTBKRS = SKB1-BUKRS.
  ELSE.
*   Buchungskreiskonsolidierung
*   ---------------------------
    SORTBKRS = '0000'.
*   BILAGKON = '3'.           " Meldung 1039323 1996
  ENDIF.

  IF KALZEITR = 'X'.
*   Berichts- und Vergleichsperioden aus  Datum bereitstellen
*   ---------------------------------------------------------
    PERFORM GET_PER_VIA_DATE.

    IF PLANVERS <> SPACE.
*     Planversion, Vergleichsperiode gleich Berichtsperiode
*     -----------------------------------------------------
      PER-VMMV = PER-BMMV.
      PER-VMMB = PER-BMMB.
    ENDIF.
  ENDIF.

* Berichts- und Vergleichsperioden gemaess Bilanztyp anpassen
* -----------------------------------------------------------
  CASE BILABTYP.
    WHEN '1'.
*   aufgelaufene Bilanz
*   -------------------
*     Berichtsperiode
*     ---------------
      PER-BMMV   = 1.
      BPER-EMBIL = PER-BMMV.           " Erster  Monat Bilanz
      BPER-LMBIL = PER-BMMB.           " Letzter Monat Bilanz
      BPER-EMGUV = PER-BMMV.           " Erster  Monat GuV
      BPER-LMGUV = PER-BMMB.           " Letzter Monat GuV
*     Vergleichsperiode
*     -----------------
      PER-VMMV   = 1.
      VPER-EMBIL = PER-VMMV.           " Erster  Monat Bilanz
      VPER-LMBIL = PER-VMMB.           " Letzter Monat Bilanz
      VPER-EMGUV = PER-VMMV.           " Erster  Monat GuV
      VPER-LMGUV = PER-VMMB.           " Letzter Monat GuV

    WHEN '2'.
*   Bewegungsbilanz
*   ---------------
*     Berichtsperiode
*     ---------------
      BPER-EMBIL = PER-BMMV.           " Erster  Monat Bilanz
      BPER-LMBIL = PER-BMMB.           " Letzter Monat Bilanz
      BPER-EMGUV = PER-BMMV.           " Erster  Monat GuV
      BPER-LMGUV = PER-BMMB.           " Letzter Monat GuV
*     Vergleichsperiode
*     -----------------
      VPER-EMBIL = PER-VMMV.           " Erster  Monat Bilanz
      VPER-LMBIL = PER-VMMB.           " Letzter Monat Bilanz
      VPER-EMGUV = PER-VMMV.           " Erster  Monat GuV
      VPER-LMGUV = PER-VMMB.           " Letzter Monat GuV

    WHEN '3'.
*   aufgelaufene Bilanz ( bei Bilanzkonten )
*   Bewegungsbilanz     ( bei GuV - Konten )
*   ----------------------------------------
*     Berichtsperiode
*     ---------------
      BPER-EMBIL = 1.                  " Erster  Monat Bilanz
      BPER-LMBIL = PER-BMMB.           " Letzter Monat Bilanz
      BPER-EMGUV = PER-BMMV.           " Erster  Monat GuV
      BPER-LMGUV = PER-BMMB.           " Letzter Monat GuV
*     Vergleichsperiode
*     -----------------
      VPER-EMBIL = 1.                  " Erster  Monat Bilanz
      VPER-LMBIL = PER-VMMB.           " Letzter Monat Bilanz
      VPER-EMGUV = PER-VMMV.           " Erster  Monat GuV
      VPER-LMGUV = PER-VMMB.           " Letzter Monat GuV

    WHEN '4'.
*   Eroeffnungsbilanz
*   -----------------
*     Berichtsperiode
*     ---------------
      BPER-EMBIL = 0.                  " Erster  Monat Bilanz
      BPER-LMBIL = 0.                  " Letzter Monat Bilanz
      BPER-EMGUV = 0.                  " Erster  Monat GuV
      BPER-LMGUV = 0.                  " Letzter Monat GuV
*     Vergleichsperiode
*     -----------------
      VPER-EMBIL = 0.                  " Erster  Monat Bilanz
      VPER-LMBIL = 0.                  " Letzter Monat Bilanz
      VPER-EMGUV = 0.                  " Erster  Monat GuV
      VPER-LMGUV = 0.                  " Letzter Monat GuV

  ENDCASE.

* Anzahl der Monate fuer Hochrechnung der GuV-Werte berechnen
* -----------------------------------------------------------
* Berichtsjahr
  IF PER-BJJV = PER-BJJB.
*   Abgrenzung in einem G-Jahr
    BPER-ANZMO = BPER-LMGUV - BPER-EMGUV + 1.
  ELSE.
*   Abgrenzung in mehreren G-Jahren
    BPER-ANZMO =     12     - BPER-EMGUV + 1.
    BPER-ANZMO = BPER-ANZMO + BPER-LMGUV.
  ENDIF.

* Vergleichsjahr
  IF PER-VJJV = PER-VJJB.
*   Abgrenzung in einem G-Jahr
    VPER-ANZMO = VPER-LMGUV - VPER-EMGUV + 1.
  ELSE.
*   Abgrenzung in mehreren G-Jahren
    VPER-ANZMO =     12     - VPER-EMGUV + 1.
    VPER-ANZMO = VPER-ANZMO + VPER-LMGUV.
  ENDIF.

*EJECT
  GET ZSKC1A.
* BEGIN Insert n1594124
*ENHANCEMENT-POINT GET_SKC1A SPOTS FI_BILA_PRINT .
* END Insert n1594124
  IF NOT ( REPWAERS IS INITIAL ).
    CTYP_WAERS  = REPWAERS.
  ELSE.
    CTYP_WAERS  = ZSKC1A-HWAER.
    CTYP_WAERS2 = ZSKC1A-HWAE2.
  ENDIF.
  IF FI_LC_EX = '2'.
*   Extrakt an FI_LC, Geschäftsbereich austauschen
*   ----------------------------------------------
    SELECT SINGLE * FROM TGSB WHERE GSBER = ZSKC1A-GSBER.
*   IF SY-SUBRC = 0 AND TGSB-GSBER_KONS <> SPACE.           "vhs134295
    IF SY-SUBRC = 0.                                        "vhs134295
      ZSKC1A-GSBER = TGSB-GSBER_KONS.
    ELSE.
      IF NOT ZSKC1A-GSBER IS INITIAL.                       "vhs134295
*       Fehler, Gsber auf HexFF setzen
*       ------------------------------
        ZSKC1A-GSBER = X_FF_GSBER.
      ENDIF.                                                "vhs134295
    ENDIF.
  ENDIF.
  DO 2 TIMES.
    CASE SY-INDEX.
      WHEN 1.
* Berichtsjahr
* ------------
        CHECK ZSKC1A-RRCTY =  '0'.      " Nur Ist-Daten
        CHECK ZSKC1A-GJAHR GE PER-BJJV. " Berichtsjahr von
        CHECK ZSKC1A-GJAHR LE PER-BJJB. " Berichtsjahr bis
* -----------------------------------------------------------------
* Ermittlung des Saldos pro Geschaeftsbereich fuer das Berichts-
* jahr und abspeichern desselben in der Tabelle GTAB.
* Gehoert ein Konto zu einer Verdichtungsgruppe, so wird der Saldo
* zusaetzlich in der Verdichtungstabelle VTAB gesammelt.
* Zur Berechnung des Bilanzergebnisses wird der Saldo nochmals
* unter dem besonderen Programmschluessel fuer Ergebnis-Bilanz oder
* -GuV in Tabelle VTAB gesammelt.
* -----------------------------------------------------------------
        CLEAR H.
        IF KALZEITR = 'X'.
*   Wenn Kalenderzeitraum gewaehlt wird erfolgt Umrechnung
*   zur Obergrenze des Zeitraums.
*   ------------------------------------------------------
          MOVE KALZEITB-HIGH TO STICHTAG.
        ENDIF.

*        PERFORM SALDO_GSBER USING 'B' ZSKC1A-GJAHR H-SIGN.
        IF BILAGRID = 'X'
        OR BILATREE = 'X'.
*.. fill table for ALV Grid- and ALV Tree-Control
          G_RLDNR = ZSKC1A-RLDNR.
*          IF BILANULL = 'X' OR ( H-SALDO <> 0 OR H-SALDO2 <> 0 ).
*            IF ( H-SALDO <> 0 OR H-SALDO2 <> 0 ) OR LOE-VORM = 'X' OR ( SKA1-XLOEV = SPACE AND SKB1-XLOEB = SPACE ).
          PERFORM GT_BSPL_FILL USING 'B'.
*            ENDIF.                                          "n1267794
*          ENDIF.
          CONTINUE.
        ENDIF.

* Konto signifikant ?????????
* ---------------------------
        CHECK H-SIGN NE 0.

        IF BILAGKON = '3'.
*   Geschaeftsbereichs Verdichtung
          SORTGBER = '****'.
        ELSE.
*   Geschaeftsbereiche einzeln/konsol.
          SORTGBER = ZSKC1A-GSBER.
        ENDIF.

* Aufnahme des Saldos in GTAB
* ---------------------------
        CLEAR GTAB.
        READ TABLE GTAB WITH KEY
                        BUKRS = SKB1-BUKRS
                        GSBER = SORTGBER.
        IF SY-SUBRC = 0.
          MOVE SKB1-BUKRS  TO GTAB-BUKRS.
          MOVE SORTGBER    TO GTAB-GSBER.
          MOVE ZUON_KTOPL  TO GTAB-KTOPL.
          MOVE ZUON_SAKNR  TO GTAB-SAKNR.
          MOVE SKA1-SAKAN  TO GTAB-SAKAN.
          MOVE PRKEY-SOLL  TO GTAB-PRKYS.
          MOVE PRKEY-HABEN TO GTAB-PRKYH.
          MOVE PRKEY-VERD  TO GTAB-VERD.
          ADD  H-SALDO     TO GTAB-BSALD.
          ADD  H-SALDO2    TO GTAB-BSALD2.
          MOVE H-SIGN      TO GTAB-BSIGN.
          MODIFY GTAB INDEX SY-TABIX.
        ELSE.
          MOVE SKB1-BUKRS TO GTAB-BUKRS.
          MOVE SORTGBER   TO GTAB-GSBER.
          MOVE ZUON_KTOPL  TO GTAB-KTOPL.
          MOVE ZUON_SAKNR  TO GTAB-SAKNR.
          MOVE SKA1-SAKAN  TO GTAB-SAKAN.
          MOVE PRKEY-SOLL  TO GTAB-PRKYS.
          MOVE PRKEY-HABEN TO GTAB-PRKYH.
          MOVE PRKEY-VERD  TO GTAB-VERD.
          ADD  H-SALDO     TO GTAB-BSALD.
          ADD  H-SALDO2    TO GTAB-BSALD2.
          MOVE H-SIGN   TO GTAB-BSIGN.
          APPEND GTAB.
        ENDIF.

* Aufnahme in Verdichtungstabelle falls erforderlich
* --------------------------------------------------
        IF PRKEY-VERD = 'X'.
          CLEAR VTAB.
          MOVE: SORTBKRS      TO VTABKEY-BUKRS,
                PRKEY-SOLL    TO VTABKEY-PRKYS,
                PRKEY-HABEN   TO VTABKEY-PRKYH,
                PRKEY-VERD    TO VTABKEY-VERDS,
                CTYP_WAERS    TO VTABKEY-WAERS,
                CTYP_WAERS2   TO VTABKEY-WAER2.
          IF BILAGKON = '1'.
*     Geschaeftsbereiche  e i n z e l n  ???
            MOVE  ZSKC1A-GSBER TO VTABKEY-GSBER.
          ELSE.
            MOVE  '****'      TO VTABKEY-GSBER.
          ENDIF.

          READ TABLE VTAB WITH KEY VTABKEY.
          IF SY-SUBRC = 0.
            MOVE  H-SIGN      TO VTAB-BSIGN.
            ADD   H-SALDO     TO VTAB-BSALD.
            ADD   H-SALDO2    TO VTAB-BSALD2.
            MODIFY VTAB INDEX SY-TABIX.
          ELSE.
            MOVE-CORRESPONDING VTABKEY TO VTAB.
            MOVE  H-SIGN      TO VTAB-BSIGN.
            ADD   H-SALDO     TO VTAB-BSALD.
            ADD   H-SALDO2    TO VTAB-BSALD2.
            COLLECT VTAB.
          ENDIF.
        ENDIF.

*EJECT
      WHEN 2.
* Vergleichsjahr
* --------------
        IF PLANVERS = SPACE.
          CHECK ZSKC1A-RRCTY =  '0'.    " Nur Ist-Daten
          CHECK ZSKC1A-GJAHR GE PER-VJJV.       " Vergl.jahr von
          CHECK ZSKC1A-GJAHR LE PER-VJJB.       " Vergl.jahr bis
        ELSE.
          CHECK ZSKC1A-RRCTY =  '1'.    " Nur Plan-Daten
          CHECK ZSKC1A-GJAHR GE PER-BJJV.       " Ber.jahr von
          CHECK ZSKC1A-GJAHR LE PER-BJJB.       " Ber.jahr bis
          CHECK ZSKC1A-RVERS =  PLANVERS.       " richtige Planversion
        ENDIF.
* -------------------------------------------------------------------
* Ermittlung des Saldos pro Geschaeftsbereich fuer das Vergleichs-
* jahr und abspeichern desselben in der Tabelle GTAB.
* Gehoert ein Konto zu einer Verdichtungsgruppe, so wird der Saldo
* zusaetzlich in der Verdichtungstabelle VTAB gesammelt.
* Zur Berechnung des Bilanzergebnisses wird der Saldo nochmals
* unter dem besonderen Programmschluessel fuer Ergebnis-Bilanz oder
* -GuV in Tabelle VTAB gesammelt.
* -------------------------------------------------------------------
        CLEAR H.
        IF KALZEITR = 'X'.
*   Wenn Kalenderzeitraum gewaehlt wird erfolgt Umrechnung
*   zur Obergrenze des Zeitraums.
*   ------------------------------------------------------
          MOVE KALZEITB-HIGH TO STICHTAG.
        ENDIF.

* Inflation adjustment
        IF BILAINFL = 'X'.
          CALL FUNCTION 'ZID_INFLATION_BAL_SHEET'
            EXPORTING
              I_SKB1                  = SKB1
              I_BILBJAHR              = BILBJAHR
              I_BILVJAHR              = BILVJAHR
            CHANGING
              C_ZSKC1A                = ZSKC1A
            EXCEPTIONS
              ADJUSTMENT_NOT_POSSIBLE = 1
              OTHERS                  = 2.

          IF SY-SUBRC <> 0.
            CLEAR FIMSG.
            FIMSG-MSORT = '0010'.
            FIMSG-MSGID = 'FR'.
            FIMSG-MSGTY = 'E'.
            FIMSG-MSGNO = '530'.
            FIMSG-MSGV1 = SKA1-SAKNR.
            FIMSG-MSGV2 = SKB1-BUKRS.
            CALL FUNCTION 'FI_MESSAGE_COLLECT'
              EXPORTING
                I_FIMSG = FIMSG.
          ENDIF.
        ENDIF.

*        PERFORM SALDO_GSBER USING 'V' ZSKC1A-GJAHR H-SIGN.
        IF BILAGRID = 'X'
        OR BILATREE = 'X'.
*.. fill table for ALV Grid- and ALV Tree-Control
          G_RLDNR = ZSKC1A-RLDNR.
*          IF BILANULL = 'X' OR ( H-SALDO <> 0 OR H-SALDO2 <> 0 ).
*            IF ( H-SALDO <> 0 OR H-SALDO2 <> 0 )
*            OR LOE-VORM = 'X' OR ( SKA1-XLOEV = SPACE AND SKB1-XLOEB = SPACE ).
          PERFORM GT_BSPL_FILL USING 'V'.
*            ENDIF.                                          "n1267794
*          ENDIF.
          CONTINUE.
        ENDIF.

* Konto signifikant ?????????
* ---------------------------
        CHECK H-SIGN NE 0.

        IF BILAGKON = '3'.
*   Geschaeftsbereichs Verdichtung
          SORTGBER = '****'.
        ELSE.
*   Geschaeftsbereiche einzeln/konsol.
          SORTGBER = ZSKC1A-GSBER.
        ENDIF.

* Aufnahme des Saldos in GTAB
* ---------------------------
        CLEAR GTAB.
        READ TABLE GTAB WITH KEY
                        BUKRS = SKB1-BUKRS
                        GSBER = SORTGBER.
        IF SY-SUBRC = 0.
          MOVE SKB1-BUKRS  TO GTAB-BUKRS.
          MOVE SORTGBER    TO GTAB-GSBER.
          MOVE ZUON_KTOPL  TO GTAB-KTOPL.
          MOVE ZUON_SAKNR  TO GTAB-SAKNR.
          MOVE SKA1-SAKAN  TO GTAB-SAKAN.
          MOVE PRKEY-SOLL  TO GTAB-PRKYS.
          MOVE PRKEY-HABEN TO GTAB-PRKYH.
          MOVE PRKEY-VERD  TO GTAB-VERD.
          ADD  H-SALDO     TO GTAB-VSALD.
          ADD  H-SALDO2    TO GTAB-VSALD2.
          MOVE H-SIGN      TO GTAB-VSIGN.
          MODIFY GTAB INDEX SY-TABIX.
        ELSE.
          MOVE SKB1-BUKRS  TO GTAB-BUKRS.
          MOVE SORTGBER    TO GTAB-GSBER.
          MOVE ZUON_KTOPL  TO GTAB-KTOPL.
          MOVE ZUON_SAKNR  TO GTAB-SAKNR.
          MOVE SKA1-SAKAN  TO GTAB-SAKAN.
          MOVE PRKEY-SOLL  TO GTAB-PRKYS.
          MOVE PRKEY-HABEN TO GTAB-PRKYH.
          MOVE PRKEY-VERD  TO GTAB-VERD.
          ADD  H-SALDO     TO GTAB-VSALD.
          ADD  H-SALDO2    TO GTAB-VSALD2.
          MOVE H-SIGN      TO GTAB-VSIGN.
          APPEND GTAB.
        ENDIF.

* Aufnahme in Verdichtungstabelle falls erforderlich
* --------------------------------------------------
        IF PRKEY-VERD = 'X'.
          CLEAR VTAB.
          MOVE: SORTBKRS      TO VTABKEY-BUKRS,
                PRKEY-SOLL    TO VTABKEY-PRKYS,
                PRKEY-HABEN   TO VTABKEY-PRKYH,
                PRKEY-VERD    TO VTABKEY-VERDS,
                CTYP_WAERS    TO VTABKEY-WAERS,
                CTYP_WAERS2   TO VTABKEY-WAER2.
          IF BILAGKON = '1'.
*     Geschaeftsbereiche  e i n z e l n  ???
            MOVE  ZSKC1A-GSBER TO VTABKEY-GSBER.
          ELSE.
            MOVE  '****'      TO VTABKEY-GSBER.
          ENDIF.

          READ TABLE VTAB WITH KEY VTABKEY.
          IF SY-SUBRC = 0.
            MOVE  H-SIGN      TO VTAB-VSIGN.
            ADD   H-SALDO     TO VTAB-VSALD.
            ADD   H-SALDO2    TO VTAB-VSALD2.
            MODIFY VTAB INDEX SY-TABIX.
          ELSE.
            MOVE-CORRESPONDING VTABKEY TO VTAB.
            MOVE  H-SIGN      TO VTAB-VSIGN.
            ADD   H-SALDO     TO VTAB-VSALD.
            ADD   H-SALDO2    TO VTAB-VSALD2.
            COLLECT VTAB.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDDO.

*EJECT
  GET SKB1 LATE.
* begin "n1399773
* Classical list: Process only if company code summarization <= 2
  IF  BILAGRID = SPACE
  AND BILATREE = SPACE.
    CHECK: BILABKON <= '2'.
* Grid or Tree control output
  ELSE.
* Processing this event is only relevant if the balance of the current
* account was zero and accounts with zero balance are supposed to be
* shown.
    CHECK BILANULL = 'X'.
    DESCRIBE TABLE GT_BSPL LINES SY-TFILL.
    READ TABLE GT_BSPL     INDEX SY-TFILL.
    IF  GT_BSPL-RBUKRS = SKB1-BUKRS
    AND GT_BSPL-RACCT  = SKB1-SAKNR.
*   The last entry in the table was created for the current company
*   code and account
*   >> No processing necessary
      RETURN.
    ENDIF.
  ENDIF.
* end "n1399773

  DO 2 TIMES.
    CASE SY-INDEX.
      WHEN 1.
* -----------------------------------------------------------------
* Die in der Tabelle GTAB gesammelten Geschaeftsbereichs-Salden
* werden um Kontonummer, Bezeichnung etc. ergaenzt und auf der
* Zwischendatenbestand geschrieben. (mit EXTRACT).
* -----------------------------------------------------------------
        SORT GTAB.
        LOOP AT GTAB.
          CLEAR SORT.
          CLEAR DATE.
          IF BILAGKON <> '1'.
            AT FIRST.
              SUM.
              PERFORM BERECHNE_ERGEBNIS.
            ENDAT.
          ELSE.
            PERFORM BERECHNE_ERGEBNIS.
          ENDIF.

          IF BILANULL NE 'X'.
            IF FI_LC_EX IS INITIAL.
*       nur Konten mit Saldo <> Null
              CHECK GTAB-BSALD NE 0
                 OR GTAB-VSALD NE 0.
            ENDIF.
          ELSE.
*     auch Konten mit Saldo Null
            IF GTAB-BSALD EQ 0 AND GTAB-VSALD EQ 0.
*       Defaultmäßig Konten ohne Löschvormerkung
              IF LOE-VORM NE 'X'.
                CHECK SKA1-XLOEV = SPACE.
                CHECK SKB1-XLOEB = SPACE.
              ENDIF.
            ENDIF.
          ENDIF.

*   Merken, ob Konto im Berichts- oder Vergl.zeitraum signifikant war
*   -----------------------------------------------------------------
          IF GTAB-BSIGN NE 0.
            H-SIGN = GTAB-BSIGN.
          ENDIF.
          IF GTAB-VSIGN NE 0.
            H-SIGN = GTAB-VSIGN.
          ENDIF.

*   Pruefen, ob Konto zu einer Verdichtungsgruppe gehoehrt
*   ------------------------------------------------------
          CASE PRKEY-VERD.
            WHEN ' '.                  " <=== dann nicht

*     --------------------------------------------
*     Bilanzschluessel Soll- und Haben sind gleich
*     --------------------------------------------
              IF PRKEY-SOLL = PRKEY-HABEN.
                PERFORM EXTRACT_SOLL_POSITION.
                EXTRACT KONTEN.
              ELSE.

*     --------------------------------------------------------
*     Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
*     --------------------------------------------------------
*       Berichts- und Vergleichssaldo sind positiv
*       ------------------------------------------
                IF GTAB-BSALD GE 0 AND GTAB-VSALD GE 0.
                  PERFORM EXTRACT_SOLL_POSITION.
                  EXTRACT KONTEN.
                ENDIF.

*       Berichts- und Vergleichssaldo sind negativ
*       ------------------------------------------
                IF GTAB-BSALD LE 0 AND GTAB-VSALD LE 0.
                  PERFORM EXTRACT_HABEN_POSITION.
                  EXTRACT KONTEN.
                ENDIF.

*       Berichtssaldo ist negativ und Vergleichssaldo ist positiv
*       ---------------------------------------------------------
                IF GTAB-BSALD < 0 AND GTAB-VSALD > 0.
                  PERFORM EXTRACT_SOLL_POSITION.
                  MOVE: '('        TO DATE-BKLAU,
                        ')'        TO DATE-BKLZU.
                  EXTRACT KONTEN.
                  CLEAR SORT.
                  CLEAR DATE.

                  PERFORM EXTRACT_HABEN_POSITION.
                  MOVE: '('        TO DATE-VKLAU,
                        ')'        TO DATE-VKLZU.
                  EXTRACT KONTEN.
                ENDIF.

*       Berichtssaldo ist positiv und Vergleichssaldo ist negativ
*       ---------------------------------------------------------
                IF GTAB-BSALD > 0 AND GTAB-VSALD < 0.
                  PERFORM EXTRACT_SOLL_POSITION.
                  MOVE: '('        TO DATE-VKLAU,
                        ')'        TO DATE-VKLZU.
                  EXTRACT KONTEN.
                  CLEAR SORT.
                  CLEAR DATE.

                  PERFORM EXTRACT_HABEN_POSITION.
                  MOVE: '('        TO DATE-BKLAU,
                        ')'        TO DATE-BKLZU.
                  EXTRACT KONTEN.
                ENDIF.
              ENDIF.
            WHEN OTHERS.

*     --------------------------------------------
*     Bilanzschluessel Soll- und Haben sind gleich
*     --------------------------------------------
              IF PRKEY-SOLL = PRKEY-HABEN.
                PERFORM EXTRACT_SOLL_POSITION.
                EXTRACT KONTEN.
              ELSE.
*     --------------------------------------------------------
*     Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
*     --------------------------------------------------------
                PERFORM EXTRACT_SOLL_POSITION.
                EXTRACT KONTEN.
                CLEAR SORT.
                CLEAR DATE.
                PERFORM EXTRACT_HABEN_POSITION.
                EXTRACT KONTEN.
              ENDIF.
          ENDCASE.
        ENDLOOP.

*EJECT
      WHEN 2.
* -------------------------------------------------------------------
* Hier gehts nur weiter, wenn Konten mit Saldo = 0 gewuenscht werden,
* fuer die aber keine GLDB-Saetze vorhanden sind.
* -------------------------------------------------------------------

* Saldo = 0 erwuenscht ??
        CHECK BILANULL = 'X'.

* Defaultmäßig Konten ohne Löschvormerkung
        IF LOE-VORM NE 'X'.
          CHECK SKA1-XLOEV = SPACE.
          CHECK SKB1-XLOEB = SPACE.
        ENDIF.

* Konto darf nicht signifikant sein, keine GLDB-Saetze haben.
        CHECK H-SIGN =  0.

* Felder fuer die Extract-Routine vorbereiten
        CLEAR: GTAB,
               SORT,
               CTYP_WAERS2,
               CTYP_WAERS,                                  "n1399773
               DATE.

* Gesber kennzeichnen, dass keine GLDB-Saetze vorhanden sind
        MOVE '****' TO GTAB-GSBER.
        IF CTYP_WAERS IS INITIAL.
          PERFORM CURRENCY_INFORMATION_GET CHANGING CTYP_WAERS.
        ENDIF.

        IF CTYP_WAERS2 IS INITIAL.
          PERFORM CURRENCY_INFORMATION_GET2 CHANGING CTYP_WAERS2.
        ENDIF.

        IF NOT ( REPWAERS IS INITIAL ).
          CTYP_WAERS  = REPWAERS.
        ENDIF.

        IF BILAGRID = 'X'
        OR BILATREE = 'X'.
*.. prepare fields for dummy records
          CLEAR: H-SALDO, H-SALDO2.
*         skc1a-gsber = '****'.                             "n1399773
          ZSKC1A-GSBER = SPACE.                             "n1399773
          ZSKC1A-CURTP = SD_CURTP.
          ZSKC1A-CURT2 = SD_CURT2.
          ZSKC1A-RLDNR = G_RLDNR.
          IF SD_CURTP IS INITIAL.
            ZSKC1A-CURTP = '10'.
          ENDIF.
          IF SD_CURT2 IS INITIAL.
            ZSKC1A-CURT2 = '10'.
          ENDIF.
*.. fill table for ALV Grid- and ALV Tree-Control
          PERFORM GT_BSPL_FILL USING 'B'.
          CONTINUE.
        ENDIF.

* --------------------------------------------
* Bilanzschluessel Soll- und Haben sind gleich
* --------------------------------------------
        IF PRKEY-SOLL = PRKEY-HABEN.
          PERFORM EXTRACT_SOLL_POSITION.
          EXTRACT KONTEN.
        ELSE.
* --------------------------------------------------------
* Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
* --------------------------------------------------------
          PERFORM EXTRACT_SOLL_POSITION.
          EXTRACT KONTEN.

          PERFORM EXTRACT_HABEN_POSITION.
          EXTRACT KONTEN.
        ENDIF.

* Aufnahme in Verdichtungstabelle falls erforderlich
* --------------------------------------------------
        IF PRKEY-VERD = 'X'.
          CLEAR VTAB.
          MOVE: SORTBKRS      TO VTAB-BUKRS,
                PRKEY-SOLL    TO VTAB-PRKYS,
                PRKEY-HABEN   TO VTAB-PRKYH,
                PRKEY-VERD    TO VTAB-VERDS,
                  '****'      TO VTAB-GSBER,
                CTYP_WAERS    TO VTAB-WAERS,
                CTYP_WAERS2   TO VTAB-WAER2.

          READ TABLE VTAB.

*   Berichts-Periode signifikant machen, da sonst kein Triggersatz
*   extrahiert wird und demzufolge die nachfolgenden Kontensaetze
*   ebenfalls uebergangen werden.
*   --------------------------------------------------------------
          IF SY-SUBRC = 0.
            MOVE:   1         TO VTAB-BSIGN,
                    1         TO VTAB-VSIGN.
            MODIFY VTAB INDEX SY-TABIX.
          ELSE.
            MOVE:   1         TO VTAB-BSIGN,
                    1         TO VTAB-VSIGN.
            COLLECT VTAB.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDDO.

*EJECT
END-OF-SELECTION.

* Tabelle I011P freigeben, wird nicht mehr benoetigt
* --------------------------------------------------
  FREE I011P.
*  BREAK ABAP.
  IF BILAGRID = 'X'                                               " ALV
  OR BILATREE = 'X'.                                              " ALV
*.. output via ALV
    IF BILANULL IS INITIAL.
*.... delete accounts with zero balance
*      PERFORM BSPL_ACCT_ZERO_BLNCE_DELETE TABLES GT_BSPL.
      PERFORM BSPL_ACCT_ZERO_DELETE TABLES GT_BSPL.
    ENDIF.


    PERFORM BSPL_SETTINGS_CREATE
                        CHANGING GS_BSPL_SETTINGS.

    CALL FUNCTION 'ZBSPL_DATA_PREPARE'
      EXPORTING
        IS_SETTINGS        = GS_BSPL_SETTINGS
      TABLES
        IT_RFBILA_ALV_DATA = GT_BSPL.

*.. fill table of list commentary
    PERFORM BSPL_LIST_COMMENTARY_CREATE TABLES GT_BSPLTOP.
*.. create title of Grid Control
*   PERFORM BSPL_GRID_TITLE_CREATE
*                                CHANGING G_GRID_TITLE.
*.. make headerlines invisible
*   GS_GRID_SETTINGS-COLL_TOP_P = 'X'.
    IF BILAGRID = 'X'.
*.... create bs p+l
      CALL FUNCTION 'ZBSPL_GRID_CREATE'
        EXPORTING
          IS_SETTINGS        = GS_BSPL_SETTINGS
          IT_LIST_COMMENTARY = GT_BSPLTOP
*         I_GRID_TITLE       = G_GRID_TITLE
          IS_GRID_SETTINGS   = GS_GRID_SETTINGS
          P_BUKRS            = SD_BUKRS-LOW
          SD_CURT            = SD_CURTP
          SD_CURT2           = SD_CURT2
          I_DIR              = 'W'
          MONATINI           = B-MONATE-LOW
          MONATFIM           = B-MONATE-HIGH.

*     back to selection screen
      LIST_CREATET = 'X'.
    ENDIF.

    IF BILATREE = 'X'.
*.... create structured balance list
      CALL FUNCTION 'ZBSPL_TREE_CREATE'
        EXPORTING
          IS_SETTINGS        = GS_BSPL_SETTINGS
          IT_LIST_COMMENTARY = GT_BSPLTOP
          P_BUKRS            = SD_BUKRS-LOW
          SD_CURT            = SD_CURTP
          SD_CURT2           = SD_CURT2.

*     back to selection screen
      LIST_CREATET = 'X'.
    ENDIF.
  ENDIF.                                                          " ALV

*---------------------------------------------------------------------*
*  Die in der Tabelle VTAB gesammelten Summen pro Verdichtungsgruppe  *
*  werden analog den Kontensaetzen extrahiert.                        *
*---------------------------------------------------------------------*
  SORT VTAB.
  LOOP AT VTAB.
    CLEAR SORT.
    CLEAR DATE.
*   --------------------------------------------
*   Bilanzschluessel Soll- und Haben sind gleich
*   --------------------------------------------
    IF VTAB-PRKYS = VTAB-PRKYH.
      PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
      EXTRACT TRIGGER.
    ELSE.
*   --------------------------------------------------------
*   Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
*   --------------------------------------------------------
*     Berichts- und Vergleichssaldo sind positiv
*     ------------------------------------------
      IF VTAB-BSALD GT 0 AND VTAB-VSALD GT 0.
        PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
        EXTRACT TRIGGER.
      ENDIF.

*     Berichts- und Vergleichssaldo sind negativ
*     ------------------------------------------
      IF VTAB-BSALD LT 0 AND VTAB-VSALD LT 0.
        PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
        EXTRACT TRIGGER.
      ENDIF.

*     Berichtssaldo ist negativ und Vergleichssaldo ist positiv
*     ---------------------------------------------------------
      IF VTAB-BSALD < 0 AND VTAB-VSALD > 0.
        PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
        MOVE: '('        TO DATE-BKLAU,
              ')'        TO DATE-BKLZU.
        EXTRACT TRIGGER.
        CLEAR SORT.
        CLEAR DATE.

        PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
        MOVE: '('        TO DATE-VKLAU,
              ')'        TO DATE-VKLZU.
        EXTRACT TRIGGER.
      ENDIF.

*     Berichtssaldo ist positiv und Vergleichssaldo ist negativ
*     ---------------------------------------------------------
      IF VTAB-BSALD > 0 AND VTAB-VSALD < 0.
        PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
        MOVE: '('        TO DATE-VKLAU,
              ')'        TO DATE-VKLZU.
        EXTRACT TRIGGER.
        CLEAR SORT.
        CLEAR DATE.

        PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
        MOVE: '('        TO DATE-BKLAU,
              ')'        TO DATE-BKLZU.
        EXTRACT TRIGGER.
      ENDIF.

*     Berichtssaldo ist negativ und Vergleichssaldo null
*     --------------------------------------------------
      IF VTAB-BSALD < 0 AND VTAB-VSALD = 0.
*       aber signifikant
*       ----------------
        IF VTAB-VSIGN = 1.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          MOVE: '('        TO DATE-BKLAU,
                ')'        TO DATE-BKLZU.
          EXTRACT TRIGGER.
          CLEAR SORT.
          CLEAR DATE.

          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          EXTRACT TRIGGER.
*       aber nicht signifikant
*       ----------------------
        ELSE.
          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          EXTRACT TRIGGER.
        ENDIF.
      ENDIF.

*     Berichtssaldo ist positiv und Vergleichssaldo null
*     --------------------------------------------------
      IF VTAB-BSALD > 0 AND VTAB-VSALD = 0.
*       aber signifikant
*       ----------------
        IF VTAB-VSIGN = 1.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          EXTRACT TRIGGER.
          CLEAR SORT.
          CLEAR DATE.

          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          MOVE: '('        TO DATE-BKLAU,
                ')'        TO DATE-BKLZU.
          EXTRACT TRIGGER.
*       aber nicht signifikant
*       ----------------------
        ELSE.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          EXTRACT TRIGGER.
        ENDIF.
      ENDIF.

*     Vergleichssaldo ist negativ und der Berichtssaldo ist null
*     ----------------------------------------------------------
      IF VTAB-VSALD < 0 AND VTAB-BSALD = 0.
*       aber signifikant
*       ----------------
        IF VTAB-BSIGN = 1.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          MOVE: '('        TO DATE-VKLAU,
                ')'        TO DATE-VKLZU.
          EXTRACT TRIGGER.
          CLEAR SORT.
          CLEAR DATE.

          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          EXTRACT TRIGGER.
*       aber nicht signifikant
*       ----------------------
        ELSE.
          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          EXTRACT TRIGGER.
        ENDIF.
      ENDIF.

*     Vergleichssaldo ist positiv und Berichtssaldo ist null
*     ------------------------------------------------------
      IF VTAB-VSALD > 0 AND VTAB-BSALD = 0.
*       aber signifikant
*       ----------------
        IF VTAB-BSIGN = 1.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          EXTRACT TRIGGER.
          CLEAR SORT.
          CLEAR DATE.

          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          MOVE: '('        TO DATE-VKLAU,
                ')'        TO DATE-VKLZU.
          EXTRACT TRIGGER.
*       aber nicht signifikant
*       ----------------------
        ELSE.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          EXTRACT TRIGGER.
        ENDIF.
      ENDIF.

*     Berichtssaldo ist null und Vergleichssaldo ist null
*     ---------------------------------------------------
      IF VTAB-BSALD = 0 AND VTAB-VSALD = 0.
*       aber signifikant
*       ----------------
        IF VTAB-VSIGN = 1.
          PERFORM EXTRACT_TRIGGER_HABEN_POSITION.
          MOVE: '('        TO DATE-BKLAU,
                ')'        TO DATE-BKLZU.
          EXTRACT TRIGGER.
          CLEAR SORT.
          CLEAR DATE.
        ENDIF.

*       aber signifikant
*       ----------------
        IF VTAB-BSIGN = 1.
          PERFORM EXTRACT_TRIGGER_SOLL_POSITION.
          MOVE: '('        TO DATE-VKLAU,
                ')'        TO DATE-VKLZU.
          EXTRACT TRIGGER.
          CLEAR SORT.
          CLEAR DATE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*EJECT
* Die extrahierten Daten werden sortiert und ausgegeben.
* ------------------------------------------------------

  IF BILA_EIS = 'X'.
* Datenextract an SAP-EIS
* -----------------------
    PERFORM CUSTOMIZE_DATA_TRANSFER(RKCFISEL) USING GRPID SD_CURTP
                                                    EIS_SUBRC .
    IF EIS_SUBRC GT 2 .
      MESSAGE ID 'KX' TYPE 'E' NUMBER '809'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'KCD_MAPPING_INIT'
      EXPORTING
        REPID                 = 'ZRFBILA00'
        GRPID                 = GRPID
*       NOLOCK                = ' '
*       NO_GENERATION         = ' '
*       NEW_PROTOCOL          = ' '
      EXCEPTIONS
        INITIALIZATION_FAILED = 1
        OTHERS                = 2.
    IF SY-SUBRC <> 0.
*   Fehler in der Übertragungsstruktur / Übertragung
*   ------------------------------------------------
      MESSAGE ID 'KX' TYPE 'E' NUMBER '913' WITH 'ZRFBILA00' GRPID .
      EXIT.
    ENDIF.

    SORT.
    LOOP.
      AT NEW SORT-BUK1.
        T001-BUKRS = SORT-BUK1.
        READ TABLE T001.
      ENDAT.

      AT NEW SORT-VERD.
*     Trigger-Kennzeichen loeschen
        CLEAR TRIGKZ.
      ENDAT.

      AT TRIGGER.
        IF DATE-PRKEY = E1POS-PRKEY
        OR DATE-PRKEY = E1NEG-PRKEY
        OR DATE-PRKEY = E2POS-PRKEY
        OR DATE-PRKEY = E2NEG-PRKEY.

          IF DATE-PRKEY     = E1POS-PRKEY.
            DATE-ERGSL = T011-ERGAK.
          ELSEIF DATE-PRKEY = E1NEG-PRKEY.
            DATE-ERGSL = T011-ERGPA.
          ELSE.
            DATE-ERGSL = T011-ERGGV.
          ENDIF.

          CTYP_WAERS  = DATE-WAERS.
          CTYP_WAERS2 = DATE-WAER2.
          IF PLANVERS EQ SPACE.
            IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.
              SORT-KTNR = '*ERGEBNIS*'.        " fix
              PERFORM BILA_EIS_TRANSFER USING DATE-BSALD DATE-BSALD2.
            ENDIF.
          ELSE.      " Plandaten
            IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.
              SORT-KTNR = '*ERGEBNIS*'.        " fix
              PERFORM BILA_EIS_TRANSFER USING DATE-VSALD DATE-VSALD2.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR TRIGKZ.
        IF PLANVERS EQ SPACE.
          IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.
*         Trigger-Kennzeichen merken
*         --------------------------
            MOVE  'T' TO TRIGKZ.
          ENDIF.
        ELSE.          " Plandaten
          IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.
*         Trigger-Kennzeichen merken
*         --------------------------
            MOVE  'T' TO TRIGKZ.
          ENDIF.
        ENDIF.
      ENDAT.

      AT KONTEN.
        CTYP_WAERS  = DATE-WAERS.
        CTYP_WAERS2 = DATE-WAER2.
        IF SORT-VERD = SPACE.
          IF PLANVERS EQ SPACE.
            IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.
              PERFORM BILA_EIS_TRANSFER USING DATE-BSALD DATE-BSALD2.
            ENDIF.
          ELSE.      " Plandaten
            IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.
              PERFORM BILA_EIS_TRANSFER USING DATE-VSALD DATE-VSALD2.
            ENDIF.
          ENDIF.
        ELSE.
          IF TRIGKZ = 'T'.
*         Bei Verdichtungs-Schluessel ungleich Space muss der
*         erste Satz der Gruppe ein Triggersatz gewesen sein.
*         ----------------------------------------------------
            IF PLANVERS EQ SPACE.
              IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.
                PERFORM BILA_EIS_TRANSFER USING DATE-BSALD DATE-BSALD2.
              ENDIF.
            ELSE.      " Plandaten
              IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.
                PERFORM BILA_EIS_TRANSFER USING DATE-VSALD DATE-VSALD2.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDAT.
    ENDLOOP.

    CALL FUNCTION 'KCD_MAPPING_EXECUTE'                   "QEH
*      EXPORTING                                        "QEH
*         TEST_RUN      = ' '                           "QEH
*         RUN_MODE      = ' '                           "QEH
*    TABLES                                             "QEH
*         VARVL         =                               "QEH
      EXCEPTIONS                                       "QEH
        ERROR_OCCURED = 1                           "QEH
        OTHERS        = 2.                          "QEH
    EIS_SUBRC = SY-SUBRC.                                 "QEH

    SKIP 3.
    MOVE TEXT-040 TO SY-LISEL.
    REPLACE '$' WITH BILBJAHR      INTO SY-LISEL.
    REPLACE '$' WITH B-MONATE-LOW  INTO SY-LISEL.
    REPLACE '$' WITH BILBJAHR      INTO SY-LISEL.
    REPLACE '$' WITH B-MONATE-HIGH INTO SY-LISEL.
    WRITE: / SY-LISEL.
    IF EIS_SUBRC = 0.                                         "QEH
      MESSAGE ID 'KX' TYPE 'S' NUMBER '832'.                "QEH
    ELSE.                                                   "QEH
      MESSAGE ID 'KX' TYPE 'S' NUMBER '819'.                "QEH
    ENDIF.                                                    "QEH
    STOP.                ">>>>>>>>>>  S T O P  >>>>>>>>>>>>>>
  ENDIF.
* BEGIN Insert n1594124
*ENHANCEMENT-POINT BEFORE_LIST SPOTS FI_BILA_PRINT .
* END Insert n1594124
*EJECT
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  SORT.
  LOOP.
    AT NEW SORT-BUK1.
*   Micro-Fiche Info
*   ----------------
      MOVE SORT(4)     TO BHDGD-GRPIN.
      MOVE SORT-BUK1   TO BHDGD-BUKRS.
      IF BILABKON = '1'.
*     Buchungskreise einzeln
*     ----------------------
        CLEAR: SS.                               " INSERT - NOTE<126410>
        MOVE BHDGD-BUKRS TO BHDGD-WERTE.
        PERFORM NEW-SECTION(RSBTCHH0).
      ENDIF.
      SELECT SINGLE * FROM T001
                      WHERE BUKRS = SORT-BUK1.

      CLEAR H-TEXT.
      IF BILASKAL(1) GT '0'.
        MOVE '1' TO H-TEXT.
        WHILE SY-INDEX LT 10 AND SY-INDEX LE BILASKAL(1).
          ASSIGN H-TEXT+SY-INDEX(1) TO <F1>.
          MOVE '0' TO <F1>.
        ENDWHILE.
      ENDIF.

      IF REPWAERS = SPACE.
        CONCATENATE DATE-WAERS '/' DATE-WAER2 INTO H-TEXT.
        "MOVE date-waers TO h-text+10.
      ELSE.
        MOVE REPWAERS   TO H-TEXT+10.
      ENDIF.
      CONDENSE H-TEXT.

      IF FI_LC_EX = '1'.
*     Extract an LC gewuenscht.
*     Gewinn/Verlust aus VTAB holen.
*     ------------------------------
        MOVE: SPACE       TO VTAB,
              SORT-BUK1   TO VTAB-BUKRS,
              '****'      TO VTAB-GSBER,
              E1POS-PRKEY TO VTAB-PRKYS,
              E1NEG-PRKEY TO VTAB-PRKYH,
              'X'         TO VTAB-VERDS.
        READ TABLE VTAB.
        IF SY-SUBRC = 0.
          IF PLANVERS = SPACE.
*         Ist-Daten uebergeben
*         --------------------
            IF VTAB-BSALD GE 0.
              H-ERGSL = T011-ERGPA.
            ELSE.
              H-ERGSL = T011-ERGAK.
            ENDIF.
            H-SALDO = VTAB-BSALD.
          ELSE.
*         Plan-Daten uebergeben
*         VTAB-VSALD enthaelt Planzahlen
*         ------------------------------
            IF VTAB-VSALD GE 0.
              H-ERGSL = T011-ERGPA.
            ELSE.
              H-ERGSL = T011-ERGAK.
            ENDIF.
            H-SALDO = VTAB-VSALD.
          ENDIF.
          PERFORM KONZ_EXTRACT(RGCBILA0)
                       USING  H-ERGSL    " Ergebnisschluessel
                              SORT-BUK1  " Buchungskreis
                              SD_GSB_S[]                    "wms086855
                              PLANVERS   " Planversion
                             'ERGEBNIS'  " Kontonummer
                              SORT-GSB1  " Geschäftsbereich
                              PER-BJJV   " Berichtjahr
                              H-SALDO    " Saldo
                              PER-BMMV   " Berichtsmonat -von-
                              PER-BMMB   " Berichtsmonat -bis-
                              GD_FNAME   " name for extract  "xrp250698
                                         " to EC-CS          "xrp250698
                              GD_PRES    " flg: pres server  "xrp250698
                              PA_RBCS    " receiver SEM-BCS  "xrp290904
                              FI_LC_EX                      "vhs134295
                              ERGSL-SOLL
                              ERGSL-HABEN
                              SPACE.                        "wes140906
        ENDIF.
      ENDIF.
    ENDAT.

    AT NEW SORT-GSB1.
*   Micro-Fiche Info
*   ----------------
      MOVE SORT(8)   TO BHDGD-GRPIN.

      IF BILABKON = '1'                          " INSERT - NOTE<126410>
     AND BILAGKON = '1'.                         " INSERT - NOTE<126410>
        CLEAR: SS.                       " INSERT - NOTE<126410>
      ENDIF.                             " INSERT - NOTE<126410>

      IF KALZEITR = 'X'.
        CONCATENATE TEXT-023 TEXT-213 INTO VARUEB.
        REPLACE 'BERPERVO' WITH KALZEITB-LOW   INTO VARUEB.
        REPLACE 'BERPERBI' WITH KALZEITB-HIGH  INTO VARUEB.

        REPLACE 'BERPERVO' WITH KALZEITB-LOW   INTO VARUEB.
        REPLACE 'BERPERBI' WITH KALZEITB-HIGH  INTO VARUEB.
        IF PLANVERS = SPACE.
          REPLACE 'VERPERVO' WITH KALZEITV-LOW   INTO VARUEB.
          REPLACE 'VERPERBI' WITH KALZEITV-HIGH  INTO VARUEB.

          REPLACE 'VERPERVO' WITH KALZEITV-LOW   INTO VARUEB.
          REPLACE 'VERPERBI' WITH KALZEITV-HIGH  INTO VARUEB.
        ELSE.
          REPLACE 'VERPERVO' WITH KALZEITB-LOW   INTO VARUEB.
          REPLACE 'VERPERBI' WITH KALZEITB-HIGH  INTO VARUEB.

          REPLACE 'VERPERVO' WITH KALZEITB-LOW   INTO VARUEB.
          REPLACE 'VERPERBI' WITH KALZEITB-HIGH  INTO VARUEB.
        ENDIF.
      ELSE.
        CONCATENATE TEXT-022 TEXT-212 INTO VARUEB.
        "MOVE text-022 TO .
        REPLACE 'B1'    WITH PER-BMMV          INTO VARUEB.
        REPLACE 'B2JJ'  WITH PER-BJJV          INTO VARUEB.
        REPLACE 'B3'    WITH PER-BMMB          INTO VARUEB.
        REPLACE 'B4JJ'  WITH PER-BJJB          INTO VARUEB.

        REPLACE 'B1'    WITH PER-BMMV          INTO VARUEB.
        REPLACE 'B2JJ'  WITH PER-BJJV          INTO VARUEB.
        REPLACE 'B3'    WITH PER-BMMB          INTO VARUEB.
        REPLACE 'B4JJ'  WITH PER-BJJB          INTO VARUEB.

        IF PLANVERS = SPACE.
          REPLACE 'V1'    WITH PER-VMMV        INTO VARUEB.
          REPLACE 'V2JJ'  WITH PER-VJJV        INTO VARUEB.
          REPLACE 'V3'    WITH PER-VMMB        INTO VARUEB.
          REPLACE 'V4JJ'  WITH PER-VJJB        INTO VARUEB.

          REPLACE 'V1'    WITH PER-VMMV        INTO VARUEB.
          REPLACE 'V2JJ'  WITH PER-VJJV        INTO VARUEB.
          REPLACE 'V3'    WITH PER-VMMB        INTO VARUEB.
          REPLACE 'V4JJ'  WITH PER-VJJB        INTO VARUEB.

        ELSE.
          REPLACE 'V1'    WITH PER-BMMV        INTO VARUEB.
          REPLACE 'V2JJ'  WITH PER-BJJV        INTO VARUEB.
          REPLACE 'V3'    WITH PER-BMMB        INTO VARUEB.
          REPLACE 'V4JJ'  WITH PER-BJJB        INTO VARUEB.

          REPLACE 'V1'    WITH PER-BMMV        INTO VARUEB.
          REPLACE 'V2JJ'  WITH PER-BJJV        INTO VARUEB.
          REPLACE 'V3'    WITH PER-BMMB        INTO VARUEB.
          REPLACE 'V4JJ'  WITH PER-BJJB        INTO VARUEB.
        ENDIF.
      ENDIF.

      IF FI_LC_EX = '2'.
*     Extract an LC gewuenscht.
*     Gewinn/Verlust aus VTAB holen.
*     ------------------------------
        MOVE: SPACE       TO VTAB,
              SORT-BUK1   TO VTAB-BUKRS,
              SORT-GSB1   TO VTAB-GSBER,
              E1POS-PRKEY TO VTAB-PRKYS,
              E1NEG-PRKEY TO VTAB-PRKYH,
              'X'         TO VTAB-VERDS.
        READ TABLE VTAB.
        IF SY-SUBRC = 0.
          IF PLANVERS = SPACE.
*         Ist-Daten uebergeben
*         --------------------
            IF VTAB-BSALD GE 0.
              H-ERGSL = T011-ERGPA.
            ELSE.
              H-ERGSL = T011-ERGAK.
            ENDIF.
            H-SALDO  = VTAB-BSALD.
            H-SALDO2 = VTAB-BSALD2.
          ELSE.
*         Plan-Daten uebergeben
*         VTAB-VSALD enthaelt Planzahlen
*         ------------------------------
            IF VTAB-VSALD GE 0.
              H-ERGSL = T011-ERGPA.
            ELSE.
              H-ERGSL = T011-ERGAK.
            ENDIF.
            H-SALDO  = VTAB-VSALD.
            H-SALDO2 = VTAB-VSALD2.
          ENDIF.
          PERFORM KONZ_EXTRACT(RGCBILA0)
                       USING  H-ERGSL    " Ergebnisschluessel
                              SORT-BUK1  " Buchungskreis
                              SD_GSB_S[]                    "wms086855
                              PLANVERS   " Planversion
                             'ERGEBNIS'  " Kontonummer
                              SORT-GSB1  " Geschäftsbereich
                              PER-BJJV   " Berichtjahr
                              H-SALDO    " Saldo
                              PER-BMMV   " Berichtsmonat -von-
                              PER-BMMB   " Berichtsmonat -bis-
                              GD_FNAME                      "xrp250698
                              GD_PRES                       "xrp250698
                              PA_RBCS    " receiver SEM-BCS  "xrp290904
                              FI_LC_EX                      "vhs134295
                              ERGSL-SOLL
                              ERGSL-HABEN
                              SPACE.                        "wes140906
        ENDIF.
      ENDIF.
    ENDAT.

    AT NEW SORT-LIST.
      NEW-PAGE.
    ENDAT.

    AT NEW SORT-PKY1.
*   Summenzaehler loeschen
      CLEAR S1.
*   Texte fuer Gruppenanfang aus T011Q etc. besorgen
      PERFORM TEXT_GRUPPENANFANG USING '1'.
    ENDAT.

    AT NEW SORT-PKY2.
*   Summenzaehler loeschen
      CLEAR S2.
      IF SORT-PKY2 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '2'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY3.
*   Summenzaehler loeschen
      CLEAR S3.
      IF SORT-PKY3 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '3'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY4.
*   Summenzaehler loeschen
      CLEAR S4.
      IF SORT-PKY4 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '4'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY5.
*   Summenzaehler loeschen
      CLEAR S5.
      IF SORT-PKY5 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '5'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY6.
*   Summenzaehler loeschen
      CLEAR S6.
      IF SORT-PKY6 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '6'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY7.
*   Summenzaehler loeschen
      CLEAR S7.
      IF SORT-PKY7 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '7'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY8.
*   Summenzaehler loeschen
      CLEAR S8.
      IF SORT-PKY8 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '8'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY9.
*   Summenzaehler loeschen
      CLEAR S9.
      IF SORT-PKY9 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '9'.
      ENDIF.
    ENDAT.

    AT NEW SORT-PKY0.
*   Summenzaehler loeschen
      CLEAR S0.
      IF SORT-PKY0 NE '00'.
*     Texte fuer Gruppenanfang aus T011Q etc. besorgen
        PERFORM TEXT_GRUPPENANFANG USING '0'.
      ENDIF.
    ENDAT.

    AT NEW SORT-VERD.
*   Trigger-Kennzeichen loeschen
      CLEAR TRIGKZ.
      IF SORT-VERD = 'X'.
*     Zeile fuer Gruppenanfang reservieren
        RESERVE = RESERVE + 1.
      ENDIF.
    ENDAT.

    AT TRIGGER.
      CTYP_WAERS  = DATE-WAERS.
      CTYP_WAERS2 = DATE-WAER2.
*   Trigger-Kennzeichen merken
*   --------------------------
      MOVE 'T' TO TRIGKZ.

*   Addition fuer unterste Summenstufe hier nur fuer Verdichtungs-
*   gruppe gleich 'X'.  Fuer ' ' bei AT KONTEN.
*   Aber nur, wenn die Betraege nicht eingeklammert sind
*   ----------------------------------------------------
      IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.
        S0-BSUM  = S0-BSUM  + DATE-BSALD.
        S0-BSUM2 = S0-BSUM2 + DATE-BSALD2.
        IF NOT ( DATE-PRKEY(AKTVA_LEN) = AKTVA-PRKEY(AKTVA_LEN) OR
                 DATE-PRKEY(PSSVA_LEN) = PSSVA-PRKEY(PSSVA_LEN) OR
                 DATE-PRKEY(NZUON_LEN) = NZUON-PRKEY(NZUON_LEN) ).
*       Staffelsummen addieren
*       ----------------------
          SS-BSUM  = SS-BSUM  + DATE-BSALD.
          SS-BSUM2 = SS-BSUM2 + DATE-BSALD2.
        ENDIF.
      ELSE.
*     Trigger-Kennzeichen merken
*     --------------------------
        MOVE 'B' TO TRIGKZ.
      ENDIF.

      IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.
        S0-VSUM  = S0-VSUM  + DATE-VSALD.
        S0-VSUM2 = S0-VSUM2 + DATE-VSALD2.
        IF NOT ( DATE-PRKEY(AKTVA_LEN) = AKTVA-PRKEY(AKTVA_LEN) OR
                 DATE-PRKEY(PSSVA_LEN) = PSSVA-PRKEY(PSSVA_LEN) OR
                 DATE-PRKEY(NZUON_LEN) = NZUON-PRKEY(NZUON_LEN) ).
*       Staffelsummen addieren
*       ----------------------
          SS-VSUM  = SS-VSUM  + DATE-VSALD.
          SS-VSUM2 = SS-VSUM2 + DATE-VSALD2.
        ENDIF.
      ELSE.
*     Trigger-Kennzeichen merken
*     --------------------------
        MOVE 'V' TO TRIGKZ.
      ENDIF.

*   Textausgabe Gruppenanfang
      PERFORM TEXTAUSGABE_ANFANG.

*      Ausgabe eines Triggersatzes
*(DEL) IF BILASUMM CN '0123456789'.
*        Einzelkontenbericht
*(DEL)   PERFORM TRIGGER_AUSGABE.
*(DEL) ENDIF.
    ENDAT.

    AT KONTEN.
      CTYP_WAERS  = DATE-WAERS.
      CTYP_WAERS2 = DATE-WAER2.
      IF SORT-VERD NE 'X'.

*     Addition fuer unterste Summenstufe hier nur fuer Verdichtungs
*     gruppe ' '. Fuer Verdichtungsgruppe 'X' bei AT TRIGGER
*     -------------------------------------------------------------
        IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.
          S0-BSUM  = S0-BSUM  + DATE-BSALD.
          S0-BSUM2 = S0-BSUM2 + DATE-BSALD2.
          IF NOT ( DATE-PRKEY(AKTVA_LEN) = AKTVA-PRKEY(AKTVA_LEN) OR
                   DATE-PRKEY(PSSVA_LEN) = PSSVA-PRKEY(PSSVA_LEN) OR
                   DATE-PRKEY(NZUON_LEN) = NZUON-PRKEY(NZUON_LEN) ).
*         Staffelsummen addieren
*         ----------------------
            SS-BSUM  = SS-BSUM  + DATE-BSALD.
            SS-BSUM2 = SS-BSUM2 + DATE-BSALD2.
          ENDIF.
        ENDIF.

        IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.
          S0-VSUM  = S0-VSUM  + DATE-VSALD.
          S0-VSUM2 = S0-VSUM2 + DATE-VSALD2.
          IF NOT ( DATE-PRKEY(AKTVA_LEN) = AKTVA-PRKEY(AKTVA_LEN) OR
                   DATE-PRKEY(PSSVA_LEN) = PSSVA-PRKEY(PSSVA_LEN) OR
                   DATE-PRKEY(NZUON_LEN) = NZUON-PRKEY(NZUON_LEN) ).
*         Staffelsummen addieren
*         ----------------------
            SS-VSUM  = SS-VSUM  + DATE-VSALD.
            SS-VSUM2 = SS-VSUM2 + DATE-VSALD2.
          ENDIF.
        ENDIF.

*     Textausgabe Gruppenanfang
*     -------------------------
        PERFORM TEXTAUSGABE_ANFANG.
*     Ausgabe einer Kontenzeile
*     -------------------------
        IF P_SUMM CN '0123456789'.
*       Einzelkontenbericht
          PERFORM KONTEN_AUSGABE.
        ENDIF.
      ELSE.

*     Textausgabe Gruppenanfang
        PERFORM TEXTAUSGABE_ANFANG.

*     Bei Verdichtungs-Schluessel gleich 'X'  muss der erste Satz
*     der Gruppe ein Triggersatz gewesen sein.
*     -----------------------------------------------------------
        IF TRIGKZ <> SPACE.
*       Betraege ausklammern
          IF TRIGKZ = 'B'.
            MOVE: '(' TO DATE-BKLAU,
                  ')' TO DATE-BKLZU.
          ENDIF.
          IF TRIGKZ = 'V'.
            MOVE: '(' TO DATE-VKLAU,
                  ')' TO DATE-VKLZU.
          ENDIF.

*       Ausgabe einer Kontenzeile
          IF P_SUMM CN '0123456789'.
*         Einzelkontenbericht
            PERFORM KONTEN_AUSGABE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDAT.

    AT END OF SORT-PKY0.
      REFRESH I011Q.
      IF SORT-PKY0 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '0' S0-BSUM S0-VSUM S0-BSUM2 S0-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S0 TO S9.
    ENDAT.

    AT END OF SORT-PKY9.
      IF SORT-PKY9 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '9' S9-BSUM S9-VSUM S9-BSUM2 S9-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S9 TO S8.
    ENDAT.

    AT END OF SORT-PKY8.
      IF SORT-PKY8 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '8' S8-BSUM S8-VSUM S8-BSUM2 S8-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S8 TO S7.
    ENDAT.

    AT END OF SORT-PKY7.
      IF SORT-PKY7 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '7' S7-BSUM S7-VSUM S7-BSUM2 S7-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S7 TO S6.
    ENDAT.

    AT END OF SORT-PKY6.
      IF SORT-PKY6 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '6' S6-BSUM S6-VSUM S6-BSUM2 S6-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S6 TO S5.
    ENDAT.

    AT END OF SORT-PKY5.
      IF SORT-PKY5 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '5' S5-BSUM S5-VSUM S5-BSUM2 S5-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S5 TO S4.
    ENDAT.

    AT END OF SORT-PKY4.
      IF SORT-PKY4 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '4' S4-BSUM S4-VSUM S4-BSUM2 S4-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S4 TO S3.
    ENDAT.

    AT END OF SORT-PKY3.
      IF SORT-PKY3 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '3' S3-BSUM S3-VSUM S3-BSUM2 S3-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S3 TO S2.
    ENDAT.

    AT END OF SORT-PKY2.
      IF SORT-PKY2 NE '00'.
*     Summenzeilen ausgeben
        PERFORM TEXT_GRUPPENENDE USING '2' S2-BSUM S2-VSUM S2-BSUM2 S2-VSUM2.
      ENDIF.
      ADD-CORRESPONDING S2 TO S1.
    ENDAT.

    AT END OF SORT-PKY1.
*   Summenzeilen ausgeben
      PERFORM TEXT_GRUPPENENDE USING '1' S1-BSUM S1-VSUM S1-BSUM2 S1-VSUM2.
    ENDAT.

    AT END OF SORT-LIST.
      CHECK SCR_PRINT_TO_FORM = NO.    "SCRIPT
      ULINE.
    ENDAT.

    AT END OF SORT-GSB1.
*   Micro-Fiche Info
*   ----------------
      MOVE SORT(4)   TO BHDGD-GRPIN.
    ENDAT.

    AT END OF SORT-BUK1.
*   Micro-Fiche Info
*   ----------------
      MOVE SPACE     TO BHDGD-GRPIN.
    ENDAT.

* Hinweise ausgeben
    AT LAST.
      IF FI_LC_EX = '1' OR FI_LC_EX = '2'.
        PERFORM KONZ_EXTRACT(RGCBILA0)
                     USING  SPACE        " Ergebnisschluessel
                            SPACE        " Buchungskreis
                            SD_GSB_S[]                      "wms086855
                            SPACE        " Planversion
                           '**ENDE**'    " Kontonummer
                            SPACE        " Geschäftsbereich
                            SPACE        " Berichtjahr
                            SPACE        " Saldo
                            SPACE        " Berichtsmonat -von-
                            SPACE        " Berichtsmonat -bis-
                            GD_FNAME                        "xrp250698
                            GD_PRES                         "xrp250698
                            PA_RBCS      " receiver SEM-BCS  "xrp290904
                            FI_LC_EX                        "vhs134295
                            ERGSL-SOLL
                            ERGSL-HABEN
                            SPACE.                          "wes140906
      ENDIF.

      LIST_CREATET = 'X'.
    ENDAT.
  ENDLOOP.

  IF LIST_CREATET IS INITIAL.
    CALL FUNCTION 'POPUP_NO_LIST'.
  ENDIF.

* eventuelle Fehlerausgabe.
* -------------------------
  CALL FUNCTION 'FI_MESSAGE_CHECK'
    EXCEPTIONS
      NO_MESSAGE = 04.

  IF SY-SUBRC = 0.
    MOVE TEXT-114 TO BHDGD-LINE2+30(70).
    NEW-PAGE.

    CALL FUNCTION 'FI_MESSAGE_SORT'.
    CALL FUNCTION 'FI_MESSAGES_ALV'.
  ENDIF.
* BEGIN Insert n1594124
*ENHANCEMENT-POINT BEFORE_PRINT SPOTS FI_BILA_PRINT .
* END Insert n1594124
  IF SCR_PRINT_TO_FORM = YES.                      "SCRIPT
    EXPORT SCR_DATA X011Q TO MEMORY ID 'RFBILA00TOFORM'.
    SUBMIT RFBIFOPR
       WITH SCR_FORM = SCR_FORM
       WITH SCR_SPRA = BILASPRA
       WITH SCR_DEVI = SCR_DEVI
       WITH BILABKON = BILABKON
       WITH BILAGKON = BILAGKON
       WITH BILASKAL = BILASKAL
       WITH PLANVERS = PLANVERS
       WITH PER_BJJV = PER-BJJV
       WITH PER_BMMV = PER-BMMV
       WITH PER_BJJB = PER-BJJB
       WITH PER_BMMB = PER-BMMB
       WITH PER_VJJV = PER-VJJV
       WITH PER_VMMV = PER-VMMV
       WITH PER_VJJB = PER-VJJB
       WITH PER_VMMB = PER-VMMB
    AND  RETURN.
  ENDIF.

* notice of departure from schedule manager
  PERFORM SCHEDMAN_START_STOP USING 'STOP'.
*EJECT
*--------------------------------------------------------------------*
*   U N T E R R O U T I N E N                                        *
*--------------------------------------------------------------------*
TOP-OF-PAGE.

  DATA: VG_TEXTO(202).

  CHECK SCR_PRINT_TO_FORM = NO.        "SCRIPT
* Standard-Seitenkopf drucken
* ---------------------------

  FORMAT COLOR COL_HEADING INVERSE.
  PERFORM BATCH-HEADING(RSBTCHH0).

  SET BLANK LINES ON.
  WRITE: / SPACE.                      " Leerzeile
  WRITE: / TEXT-024.                   " Buchungskreis
  IF BILABKON >= '2'.
    WRITE '****'    COLOR COL_BACKGROUND INTENSIFIED OFF
                                         INVERSE OFF.
  ELSE.
    WRITE SORT-BUK1 COLOR COL_BACKGROUND INTENSIFIED OFF
                                         INVERSE OFF.
  ENDIF.

  WRITE:   TEXT-025.                   " Geschäftsbereich
  IF BILAGKON = '1'.
    WRITE SORT-GSB1 COLOR COL_BACKGROUND INTENSIFIED OFF
                                         INVERSE OFF.
  ELSE.
    WRITE '****'    COLOR COL_BACKGROUND INTENSIFIED OFF
                                         INVERSE OFF.
  ENDIF.

  WRITE:  99 TEXT-026,                 " Beträge in
             H-TEXT COLOR COL_BACKGROUND INTENSIFIED OFF
                                         INVERSE OFF.
  ULINE.

  FORMAT COLOR COL_HEADING INTENSIFIED INVERSE OFF.
  UC_INITIALIZE.
  UC_VLINE_OFFSET: 0, 2, 7, 12, 63, 82, 101, 118, 126, 131, 150, 169, 187, 195, 200.
  CONCATENATE TEXT-021 TEXT-211 INTO VG_TEXTO.
  UC_HEADERLINE VG_TEXTO.
  WRITE: / L_LINE.
  UC_HEADERLINE VARUEB.
  WRITE: / L_LINE.

  IF PLANVERS = SPACE.
    ULINE.
  ELSE.
    WRITE: TEXT-006.
  ENDIF.

*--------------------------------------------------------------------
*        FORM SUCHEN_ERGEBNISPOSITION
*--------------------------------------------------------------------
* Aus Tabelle T011  werden zunaechst die Ergebnisschluessel fuer
*   - das Bilanzergebnis in der Bilanz (positiv)
*   - das Bilanzergebnis in der Bilanz (negativ)
*   - das Bilanzergebnis in der GuV    (positiv)
*   - das Bilanzergebnis in der GuV    (negativ)
*   - die Konten, die Aktiva repraesentieren
*   - die Konten, die Passiva repraesentieren
*   - die Konten, die nicht zugeordnet sind
* besorgt.
* Mit den Ergebnisschluesseln werden dann die dazugehoeringen Pro-
* grammschluessel aus der Tabelle I011P gelesen.
* Die Programmschluessel, die Aktiva, Passiva und 'Nicht zugeord-
* neten Konten' repraesentieren werden zur Berechnung des Bilanz-
* ergebnisses herangezogen.
* Und zwar werden die Salden der Konten, deren Programmschluessel in
* den ersten beiden Stellen mit den Programmschluesseln von Aktiva,
* Passiva oder 'Nicht zugeordneten Konten' uebereinstimmen zur Be-
* rechnung des Ergebnisses Bilanz, alle anderen Konten zur Berechnung
* des Ergebnisses in der GuV herangezogen.
*--------------------------------------------------------------------

FORM SUCHEN_ERGEBNIS_POSITION USING ERG_VERSION.
* Tabelle X011P in umgekehrter Sortierfolge nach I011P uebernehmen
* ----------------------------------------------------------------
  LOOP AT X011P.
    MOVE-CORRESPONDING X011P TO I011P.
    APPEND I011P.
  ENDLOOP.
  SORT I011P.

* dann aus Tabelle I011P den Programmschluessel fuer Bilanz besorgen
* ------------------------------------------------------------------
  READ TABLE I011P WITH KEY T011-ERGAK BINARY SEARCH.
  IF SY-SUBRC = 0.
*   Schluessel fuer positiv
*   -----------------------
    MOVE: I011P-PRKEY TO E1POS-PRKEY.
  ENDIF.

  READ TABLE I011P WITH KEY T011-ERGPA BINARY SEARCH.
  IF SY-SUBRC = 0.
*   Schluessel fuer negativ
*   -----------------------
    MOVE: I011P-PRKEY TO E1NEG-PRKEY.
  ENDIF.

* dann aus Tabelle I011P den Programmschluessel fuer G u V  besorgen
* ------------------------------------------------------------------
  READ TABLE I011P WITH KEY T011-ERGGV BINARY SEARCH.
  IF SY-SUBRC = 0.
*   Schluessel fuer positiv
*   -----------------------
    MOVE: I011P-PRKEY TO E2POS-PRKEY.
  ENDIF.

  READ TABLE I011P WITH KEY T011-ERGGV BINARY SEARCH.
  IF SY-SUBRC = 0.
*   Schluessel fuer negativ
*   -----------------------
    MOVE: I011P-PRKEY TO E2NEG-PRKEY.
  ENDIF.

* dann aus Tabelle I011P den Programmschluessel fuer die nicht
* zugeordneten Konten besorgen
* ------------------------------------------------------------
  READ TABLE I011P WITH KEY T011-ZUORD BINARY SEARCH.
  IF SY-SUBRC = 0.
    MOVE: I011P-PRKEY TO NZUON-PRKEY.
    NZUON_LEN = I011P-STUFE * 2.
  ENDIF.

* dann aus Tabelle I011P den Programmschluessel welcher Aktiva
* repraesentiert besorgen.
* ------------------------------------------------------------
  READ TABLE I011P WITH KEY T011-AKTVA BINARY SEARCH.
  IF SY-SUBRC = 0.
    MOVE: I011P-PRKEY TO AKTVA-PRKEY.
    AKTVA_LEN = I011P-STUFE * 2.
  ENDIF.

* dann aus Tabelle I011P den Programmschluessel welcher Passiva
* repraesentiert besorgen.
* -------------------------------------------------------------
  READ TABLE I011P WITH KEY T011-PSSVA BINARY SEARCH.
  IF SY-SUBRC = 0.
    MOVE: I011P-PRKEY TO PSSVA-PRKEY.
    PSSVA_LEN = I011P-STUFE * 2.
  ENDIF.

* aus Tabelle I011P den Programmschluessel welcher den
* Bilanzanhang repraesentiert besorgen.
* ------------------------------------------------------
  READ TABLE I011P WITH KEY T011-ANHNG BINARY SEARCH.
  IF SY-SUBRC = 0.
    MOVE: I011P-PRKEY TO ANHNG-PRKEY.
    ANHNG_LEN = I011P-STUFE * 2.
  ENDIF.
ENDFORM.                    "SUCHEN_ERGEBNIS_POSITION

*EJECT
*--------------------------------------------------------------------*
*        FORM FILL_BHDGD                                             *
*--------------------------------------------------------------------*
*  Standardseitenkopf fuellen
*----------------------------
FORM FILL_BHDGD.
  MOVE:    '0'      TO BHDGD-INIFL,
        SY-LINSZ    TO BHDGD-LINES,
        SY-UNAME    TO BHDGD-UNAME,
        SY-REPID    TO BHDGD-REPID.
  CASE FI_LC_EX.
    WHEN '1'.
      MOVE TEXT-010   TO BHDGD-LINE2.
    WHEN '2'.
      MOVE TEXT-011   TO BHDGD-LINE2.
    WHEN OTHERS.
      MOVE ALLGLINE   TO BHDGD-LINE2.
  ENDCASE.
  MOVE: SPACE       TO BHDGD-BUKRS,
        MIKFICHE    TO BHDGD-MIFFL,
        ALLGLSEP    TO BHDGD-SEPAR,
        'BUKRS'     TO BHDGD-DOMAI.
  SELECT SINGLE * FROM T011T WHERE
                       SPRAS = BILASPRA
                   AND VERSN = BILAVERS.
  IF SY-SUBRC = 0.
    MOVE T011T-VSTXT TO BHDGD-LINE1.
  ELSE.
    MOVE SY-TITLE    TO BHDGD-LINE1.
  ENDIF.

  IF BILAPAGE > 0.
    BILAPAGE = BILAPAGE - 1.
    MOVE BILAPAGE    TO BHDGD-START_PAGNO.
  ENDIF.

  IF BILABKON >= '2'.
    MOVE BHDGD-BUKRS TO BHDGD-WERTE.
    PERFORM NEW-SECTION(RSBTCHH0).
  ENDIF.
ENDFORM.                    "FILL_BHDGD


*EJECT
*--------------------------------------------------------------------*
*        FORM SALDO_GSBER                                            *
*--------------------------------------------------------------------*
* Saldoermittlung pro Geschaeftsbereich
*--------------------------------------
FORM SALDO_GSBER USING SALDO_PERIODE SALDO_JAHRC SALDO_SIGN .
*  BREAK ABAP.
  CASE SALDO_PERIODE.
    WHEN 'B'.
* Berichts-Periode
* ----------------
*   Bilanzkonten
*   ------------
      IF PRKEY-SOLL(AKTVA_LEN)  =  AKTVA-PRKEY(AKTVA_LEN) OR
         PRKEY-SOLL(PSSVA_LEN)  =  PSSVA-PRKEY(PSSVA_LEN) OR
         PRKEY-HABEN(AKTVA_LEN) =  AKTVA-PRKEY(AKTVA_LEN) OR
         PRKEY-HABEN(PSSVA_LEN) =  PSSVA-PRKEY(PSSVA_LEN) OR
        ( PRKEY-SOLL(NZUON_LEN) =  NZUON-PRKEY(NZUON_LEN) AND
         PRKEY-HABEN(NZUON_LEN) =  NZUON-PRKEY(NZUON_LEN) ).
*     Zeitraum bewegt sich innerhalb eines Jahres
*     -------------------------------------------
        IF PER-BJJV = PER-BJJB.
          H-UMONAT = BPER-EMBIL.
          H-OMONAT = BPER-LMBIL.
*       Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*       --------------------------------------------------
          IF BILABTYP NE '2'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS

          ENDIF.
        ELSE.
*       Zeitraum bewegt sich ueber Jahresgrenzen hinweg
*       -----------------------------------------------
*       Anfangsjahr
*       -----------
          IF SALDO_JAHRC = PER-BJJV.
            H-UMONAT = BPER-EMBIL.
            H-OMONAT =    16.
*         Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*         --------------------------------------------------
            IF BILABTYP NE '2'.
*---> 15/06/2023 - Migração S4 - JS
*               H-SALDO  = ZSKC1A-UMSAV.
*               H-SALDO2 = ZSKC1A-UMSAV2.
            H-SALDO = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
            ENDIF.
          ENDIF.
*       Endejahr
*       --------
          IF SALDO_JAHRC = PER-BJJB.
            H-UMONAT =     1.
            H-OMONAT = BPER-LMBIL.
          ENDIF.
*       Jahr ist irgendwo zwischen Ober- und Untergrenze
*       ------------------------------------------------
          IF SALDO_JAHRC > PER-BJJV.
            IF SALDO_JAHRC < PER-BJJB.
              H-UMONAT =     1.
              H-OMONAT =    16.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
*   GuV Konten
*   ----------
*     Zeitraum bewegt sich innerhalb eines Jahres
*     -------------------------------------------
        IF PER-BJJV = PER-BJJB.
          H-UMONAT = BPER-EMGUV.
          H-OMONAT = BPER-LMGUV.
*       Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*       --------------------------------------------------
          IF BILABTYP NE '2' AND BILABTYP NE '3'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
          ENDIF.
        ELSE.
*       Zeitraum bewegt sich ueber Jahresgrenzen hinweg
*       -----------------------------------------------
*       Anfangsjahr
*       -----------
          IF SALDO_JAHRC = PER-BJJV.
            H-UMONAT = BPER-EMGUV.
            H-OMONAT =    16.
*         Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*         --------------------------------------------------
            IF BILABTYP NE '2' AND BILABTYP NE '3'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
            ENDIF.
          ENDIF.
*       Endejahr
*       --------
          IF SALDO_JAHRC = PER-BJJB.
            H-UMONAT =     1.
            H-OMONAT = BPER-LMGUV.
          ENDIF.
*       Jahr ist irgendwo zwischen Ober- und Untergrenze
*       ------------------------------------------------
          IF SALDO_JAHRC > PER-BJJV.
            IF SALDO_JAHRC < PER-BJJB.
              H-UMONAT =     1.
              H-OMONAT =    16.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'V'.
* Vergleichs-Periode
* ------------------
*   Bilanzkonten
*   ------------
      IF PRKEY-SOLL(AKTVA_LEN)  =  AKTVA-PRKEY(AKTVA_LEN) OR
         PRKEY-SOLL(PSSVA_LEN)  =  PSSVA-PRKEY(PSSVA_LEN) OR
         PRKEY-HABEN(AKTVA_LEN) =  AKTVA-PRKEY(AKTVA_LEN) OR
         PRKEY-HABEN(PSSVA_LEN) =  PSSVA-PRKEY(PSSVA_LEN) OR
        ( PRKEY-SOLL(NZUON_LEN) =  NZUON-PRKEY(NZUON_LEN) AND
         PRKEY-HABEN(NZUON_LEN) =  NZUON-PRKEY(NZUON_LEN) ).
*     Zeitraum bewegt sich innerhalb eines Jahres
*     -------------------------------------------
        IF PER-VJJV = PER-VJJB.
          H-UMONAT = VPER-EMBIL.
          H-OMONAT = VPER-LMBIL.
*       Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*       --------------------------------------------------
          IF BILABTYP NE '2'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
          ENDIF.
        ELSE.
*       Zeitraum bewegt sich ueber Jahresgrenzen hinweg
*       -----------------------------------------------
*       Anfangsjahr
*       -----------
          IF SALDO_JAHRC = PER-VJJV.
            H-UMONAT = VPER-EMBIL.
            H-OMONAT =    16.
*         Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*         --------------------------------------------------
            IF BILABTYP NE '2'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
            ENDIF.
          ENDIF.
*       Endejahr
*       --------
          IF SALDO_JAHRC = PER-VJJB.
            H-UMONAT =     1.
            H-OMONAT = VPER-LMBIL.
          ENDIF.
*       Jahr ist irgendwo zwischen Ober- und Untergrenze
*       ------------------------------------------------
          IF SALDO_JAHRC > PER-VJJV.
            IF SALDO_JAHRC < PER-VJJB.
              H-UMONAT =     1.
              H-OMONAT =    16.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
*   GuV Konten
*   ----------
*     Zeitraum bewegt sich innerhalb eines Jahres
*     -------------------------------------------
        IF PER-VJJV = PER-VJJB.
          H-UMONAT = VPER-EMGUV.
          H-OMONAT = VPER-LMGUV.
*       Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*       --------------------------------------------------
          IF BILABTYP NE '2' AND BILABTYP NE '3'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
          ENDIF.
        ELSE.
*       Zeitraum bewegt sich ueber Jahresgrenzen hinweg
*       -----------------------------------------------
*       Anfangsjahr
*       -----------
          IF SALDO_JAHRC = PER-VJJV.
            H-UMONAT = VPER-EMGUV.
            H-OMONAT =    16.
*         Wenn keine Bewegungsbilanz, dann mit Saldo-Vortrag
*         --------------------------------------------------
            IF BILABTYP NE '2' AND BILABTYP NE '3'.
*---> 15/06/2023 - Migração S4 - JS
*            H-SALDO  = ZSKC1A-UMSAV.
*            H-SALDO2 = ZSKC1A-UMSAV2.
           H-SALDO  = CONV #( ZSKC1A-UMSAV  ).
           H-SALDO2 = CONV #( ZSKC1A-UMSAV2 ).
*<--- 15/06/2023 - Migração S4 - JS
            ENDIF.
          ENDIF.
*       Endejahr
*       --------
          IF SALDO_JAHRC = PER-VJJB.
            H-UMONAT =     1.
            H-OMONAT = VPER-LMGUV.
          ENDIF.
*       Jahr ist irgendwo zwischen Ober- und Untergrenze
*       ------------------------------------------------
          IF SALDO_JAHRC > PER-VJJV.
            IF SALDO_JAHRC < PER-VJJB.
              H-UMONAT =     1.
              H-OMONAT =    16.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

* Keinen Saldo ermitteln, wenn Eroeffnungsbilanz gewuenscht
* ---------------------------------------------------------
  IF BILABTYP NE '4'.
    DO 16 TIMES VARYING H-UM01S  FROM ZSKC1A-UM01S  NEXT ZSKC1A-UM02S
                VARYING H-UM01H  FROM ZSKC1A-UM01H  NEXT ZSKC1A-UM02H
                VARYING H-UM01S2 FROM ZSKC1A-UM01S2 NEXT ZSKC1A-UM02S2
                VARYING H-UM01H2 FROM ZSKC1A-UM01H2 NEXT ZSKC1A-UM02H2.
      IF SY-INDEX >= H-UMONAT AND SY-INDEX <= H-OMONAT.
        H-SALDO = H-SALDO + ( H-UM01S - H-UM01H ).
        IF ZSKC1A-UMSAV NE 0 OR H-UM01S     NE 0 OR H-UM01H     NE 0.
          SALDO_SIGN = 1.
        ENDIF.
        H-SALDO2 = H-SALDO2 + ( H-UM01S2 - H-UM01H2 ).
        IF ZSKC1A-UMSAV2 NE 0 OR H-UM01S2     NE 0 OR H-UM01H2     NE 0.
          SALDO_SIGN = 1.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSE.
    IF ZSKC1A-UMSAV NE 0.
*.... mark account as posted
      SALDO_SIGN = 1.
    ENDIF.
    IF ZSKC1A-UMSAV2 NE 0.
*.... mark account as posted
      SALDO_SIGN = 1.
    ENDIF.
  ENDIF.

* Normierung der GuV-Werte falls erforderlich
* -------------------------------------------
  IF HOCHRECH     =  'X'            AND
    PRKEY-SOLL(AKTVA_LEN) NE AKTVA-PRKEY(AKTVA_LEN) AND
    PRKEY-SOLL(PSSVA_LEN) NE PSSVA-PRKEY(PSSVA_LEN) AND
    PRKEY-SOLL(NZUON_LEN) NE NZUON-PRKEY(NZUON_LEN).

    CASE SALDO_PERIODE.
      WHEN 'B'.
*     Berichtsperiode
*     ---------------
        IF BPER-ANZMO > 0 AND BPER-ANZMO < 12.
          H-SALDO  = ( H-SALDO  * 12 ) / BPER-ANZMO.
          H-SALDO2 = ( H-SALDO2 * 12 ) / BPER-ANZMO.
        ENDIF.
      WHEN 'V'.
*     Vergleichsperiode
*     -----------------
        IF VPER-ANZMO > 0 AND VPER-ANZMO < 12.
          H-SALDO  = ( H-SALDO  * 12 ) / VPER-ANZMO.
          H-SALDO2 = ( H-SALDO2 * 12 ) / VPER-ANZMO.
        ENDIF.
    ENDCASE.
  ENDIF.

* Waehrungsumrechnung falls erforderlich
* --------------------------------------
  IF REPWAERS NE SPACE AND REPWAERS NE ZSKC1A-HWAER.
    MOVE H-SALDO  TO H-USALD.
*.. try at first china specific conversion
    CALL FUNCTION 'OUTBOUND_CALL_00003220_P'
      EXPORTING
        I_LAND1          = T001-LAND1
        DATE             = STICHTAG
        PERIODE          = SALDO_PERIODE
        FOREIGN_CURRENCY = REPWAERS
        LOCAL_AMOUNT     = H-USALD
        LOCAL_CURRENCY   = ZSKC1A-HWAER
        RATE             = 0
        TYPE_OF_RATE     = KURS_TYP
        READ_TCURR       = 'X'
      IMPORTING
        FOREIGN_AMOUNT   = H-SALDO
      EXCEPTIONS
        NOTHING_FOUND    = 1
        OTHERS           = 2.

    IF SY-SUBRC <> 0.
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          DATE             = STICHTAG
          LOCAL_CURRENCY   = ZSKC1A-HWAER
          LOCAL_AMOUNT     = H-USALD
          FOREIGN_CURRENCY = REPWAERS
          TYPE_OF_RATE     = KURS_TYP
        IMPORTING
          FOREIGN_AMOUNT   = H-SALDO.
    ENDIF.

  ENDIF.

  IF REPWAERS NE SPACE AND REPWAERS NE ZSKC1A-HWAE2.
    MOVE H-SALDO2 TO H-USALD2.
*.. try at first china specific conversion
    CALL FUNCTION 'OUTBOUND_CALL_00003220_P'
      EXPORTING
        I_LAND1          = T001-LAND1
        DATE             = STICHTAG
        PERIODE          = SALDO_PERIODE
        FOREIGN_CURRENCY = REPWAERS
        LOCAL_AMOUNT     = H-USALD2
        LOCAL_CURRENCY   = ZSKC1A-HWAE2
        RATE             = 0
        TYPE_OF_RATE     = KURS_TYP
        READ_TCURR       = 'X'
      IMPORTING
        FOREIGN_AMOUNT   = H-SALDO2
      EXCEPTIONS
        NOTHING_FOUND    = 1
        OTHERS           = 2.

    IF SY-SUBRC <> 0.
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          DATE             = STICHTAG
          LOCAL_CURRENCY   = ZSKC1A-HWAE2
          LOCAL_AMOUNT     = H-USALD2
          FOREIGN_CURRENCY = REPWAERS
          TYPE_OF_RATE     = KURS_TYP
        IMPORTING
          FOREIGN_AMOUNT   = H-SALDO2.
    ENDIF.
  ENDIF.

ENDFORM.                    "SALDO_GSBER

*EJECT
*--------------------------------------------------------------------*
*        FORM EXTRACT_SOLL_POSITION                                  *
*--------------------------------------------------------------------*
* Bestueckung eines EXTRACT-Satzes, wenn der Saldo positiv ist.
*--------------------------------------------------------------
FORM EXTRACT_SOLL_POSITION.
  MOVE: SORTBKRS           TO SORT-BUK1,  " Buchungskreis
        GTAB-GSBER         TO SORT-GSB1,  " Geschaeftsbereich
        PRKEY-SOLL(2)      TO SORT-LIST,  " Listenteil
        PRKEY-SOLL+00(2)   TO SORT-PKY1,                    " Stufe 1
        PRKEY-SOLL+02(2)   TO SORT-PKY2,                    " Stufe 2
        PRKEY-SOLL+04(2)   TO SORT-PKY3,                    " Stufe 3
        PRKEY-SOLL+06(2)   TO SORT-PKY4,                    " Stufe 4
        PRKEY-SOLL+08(2)   TO SORT-PKY5,                    " Stufe 5
        PRKEY-SOLL+10(2)   TO SORT-PKY6,                    " Stufe 6
        PRKEY-SOLL+12(2)   TO SORT-PKY7,                    " Stufe 7
        PRKEY-SOLL+14(2)   TO SORT-PKY8,                    " Stufe 8
        PRKEY-SOLL+16(2)   TO SORT-PKY9,                    " Stufe 9
        PRKEY-SOLL+18(2)   TO SORT-PKY0,                    " Stufe 10
        PRKEY-VERD         TO SORT-VERD.  " Verdichtungsschluessel
  IF BILABKON <= '2'.
    MOVE SKB1-BUKRS        TO SORT-BUK2.  " Buchungskreis  (KONS)
    MOVE ZUON_SAKNR        TO SORT-KTNR.  " Sachkonto
    MOVE SKA1-SAKAN        TO DATE-SAKAN. " Kontonummer
  ELSE.
    MOVE GTAB-BUKRS        TO SORT-BUK2.  " Buchungskreis  (KONS)
    MOVE GTAB-SAKNR        TO SORT-KTNR.  " Sachkonto
    MOVE GTAB-SAKAN        TO DATE-SAKAN. " Kontonummer
    IF GTAB-SAKAN IS INITIAL.
      MOVE ZUON_SAKNR      TO SORT-KTNR.  " Sachkonto
      MOVE SKA1-SAKAN      TO DATE-SAKAN. " Kontonummer
    ENDIF.
  ENDIF.
  MOVE:
        GTAB-GSBER         TO SORT-GSB2,  " Geschaeftsber. (KONS)
        PRKEY-SOLL         TO DATE-PRKEY, " Programmschluessel
        ERGSL-SOLL         TO DATE-ERGSL, " Ergebnisschluessel
        SKAT-TXT50         TO DATE-SKBEZ, " Kontobezeichnung
        SKA1-SAKNR         TO DATE-SAKNR, " Kontonummer    "VSK047132
        GTAB-BSALD         TO DATE-BSALD, " Saldo Berichtszeitr.
        GTAB-VSALD         TO DATE-VSALD, " Saldo Verglzeitr.
        CTYP_WAERS         TO DATE-WAERS, " Kontonummer    "VSK047132
        GTAB-BSALD2        TO DATE-BSALD2, " Saldo Berichtszeitr.
        GTAB-VSALD2        TO DATE-VSALD2, " Saldo Verglzeitr.
        CTYP_WAERS2        TO DATE-WAER2. " Waehrung
* Geschaeftsbreichs-Konsolidierung ??
* -----------------------------------
  IF BILAGKON = '2'.
    MOVE '****' TO SORT-GSB1.          " Geschaeftsbereich
  ENDIF.
ENDFORM.                    "EXTRACT_SOLL_POSITION

*EJECT
*--------------------------------------------------------------------*
*        FORM EXTRACT_HABEN_POSITION                                 *
*--------------------------------------------------------------------*
* Bestueckung eines EXTRACT-Satzes, wenn der Saldo negativ ist.
*--------------------------------------------------------------
FORM EXTRACT_HABEN_POSITION.
  MOVE: SORTBKRS           TO SORT-BUK1,  " Buchungskreis
        GTAB-GSBER         TO SORT-GSB1,  " Geschaeftsbereich
        PRKEY-HABEN(2)     TO SORT-LIST,  " Listenteil
        PRKEY-HABEN+00(2)  TO SORT-PKY1,                    " Stufe 1
        PRKEY-HABEN+02(2)  TO SORT-PKY2,                    " Stufe 2
        PRKEY-HABEN+04(2)  TO SORT-PKY3,                    " Stufe 3
        PRKEY-HABEN+06(2)  TO SORT-PKY4,                    " Stufe 4
        PRKEY-HABEN+08(2)  TO SORT-PKY5,                    " Stufe 5
        PRKEY-HABEN+10(2)  TO SORT-PKY6,                    " Stufe 6
        PRKEY-HABEN+12(2)  TO SORT-PKY7,                    " Stufe 7
        PRKEY-HABEN+14(2)  TO SORT-PKY8,                    " Stufe 8
        PRKEY-HABEN+16(2)  TO SORT-PKY9,                    " Stufe 9
        PRKEY-HABEN+18(2)  TO SORT-PKY0,                    " Stufe 10
        PRKEY-VERD         TO SORT-VERD.  " Verdichtungsschluessel
  IF BILABKON <= '2'.
    MOVE SKB1-BUKRS        TO SORT-BUK2.  " Buchungskreis  (KONS)
    MOVE ZUON_SAKNR        TO SORT-KTNR.  " Sachkonto
    MOVE SKA1-SAKAN        TO DATE-SAKAN. " Kontonummer
  ELSE.
    MOVE GTAB-BUKRS        TO SORT-BUK2.  " Buchungskreis  (KONS)
    MOVE GTAB-SAKNR        TO SORT-KTNR.  " Sachkonto
    MOVE GTAB-SAKAN        TO DATE-SAKAN. " Kontonummer
    IF GTAB-SAKAN IS INITIAL.
      MOVE ZUON_SAKNR      TO SORT-KTNR.  " Sachkonto
      MOVE SKA1-SAKAN      TO DATE-SAKAN. " Kontonummer
    ENDIF.
  ENDIF.
  MOVE:

        GTAB-GSBER         TO SORT-GSB2,  " Geschaeftsber. (KONS)
        PRKEY-HABEN        TO DATE-PRKEY, " Programmschluessel
        ERGSL-HABEN        TO DATE-ERGSL, " Ergebnisschluessel
        SKAT-TXT50         TO DATE-SKBEZ, " Kontobezeichnung
        SKA1-SAKNR         TO DATE-SAKNR, " Kontonummer    "VSK047132
        GTAB-BSALD         TO DATE-BSALD, " Saldo Berichtszeitr.
        GTAB-VSALD         TO DATE-VSALD, " Saldo Verglzeitr.
        CTYP_WAERS         TO DATE-WAERS, " Kontonummer    "VSK047132
        GTAB-BSALD2        TO DATE-BSALD2, " Saldo Berichtszeitr.
        GTAB-VSALD2        TO DATE-VSALD2, " Saldo Verglzeitr.
        CTYP_WAERS2        TO DATE-WAER2. " Waehrung
* Geschaeftsbreichs-Konsolidierung ??
* -----------------------------------
  IF BILAGKON = '2'.
    MOVE '****' TO SORT-GSB1.          " Geschaeftsbereich
  ENDIF.
ENDFORM.                    "EXTRACT_HABEN_POSITION

*EJECT
*--------------------------------------------------------------------*
*        FORM EXTRACT_TRIGGER_SOLL_POSITION                          *
*--------------------------------------------------------------------*
* Bestueckung eines EXTRACT-Satzes, wenn der Saldo positiv ist.
*--------------------------------------------------------------
FORM EXTRACT_TRIGGER_SOLL_POSITION.
  MOVE: VTAB-BUKRS         TO SORT-BUK1,  " Buchungskreis
        VTAB-GSBER         TO SORT-GSB1,  " Geschaeftsbereich
        VTAB-PRKYS(2)      TO SORT-LIST,  " Listenteil
        VTAB-PRKYS+00(2)   TO SORT-PKY1,                    " Stufe 1
        VTAB-PRKYS+02(2)   TO SORT-PKY2,                    " Stufe 2
        VTAB-PRKYS+04(2)   TO SORT-PKY3,                    " Stufe 3
        VTAB-PRKYS+06(2)   TO SORT-PKY4,                    " Stufe 4
        VTAB-PRKYS+08(2)   TO SORT-PKY5,                    " Stufe 5
        VTAB-PRKYS+10(2)   TO SORT-PKY6,                    " Stufe 6
        VTAB-PRKYS+12(2)   TO SORT-PKY7,                    " Stufe 7
        VTAB-PRKYS+14(2)   TO SORT-PKY8,                    " Stufe 8
        VTAB-PRKYS+16(2)   TO SORT-PKY9,                    " Stufe 9
        VTAB-PRKYS+18(2)   TO SORT-PKY0,                    " Stufe 10
        VTAB-VERDS         TO SORT-VERD,  " Verdichtungsschluessel
        VTAB-BUKRS         TO SORT-BUK2,  " Buchungskreis  (KONS)
        VTAB-GSBER         TO SORT-GSB2,  " Geschaeftsber. (KONS)
        VTAB-PRKYS         TO DATE-PRKEY, " Programmschluessel
        VTAB-BSALD         TO DATE-BSALD, " Saldo Berichtszeitr.
        VTAB-VSALD         TO DATE-VSALD, " Saldo Verglzeitr.
        VTAB-WAERS         TO DATE-WAERS, " Programmschluessel
        VTAB-BSALD2        TO DATE-BSALD2, " Saldo Berichtszeitr.
        VTAB-VSALD2        TO DATE-VSALD2, " Saldo Verglzeitr.
        VTAB-WAER2         TO DATE-WAER2. " Währung
* Geschaeftsbreichs-Konsolidierung ??
* -----------------------------------
  IF BILAGKON = '2'.
    MOVE '****' TO SORT-GSB1.          " Geschaeftsbereich
  ENDIF.
ENDFORM.                    "EXTRACT_TRIGGER_SOLL_POSITION

*EJECT
*--------------------------------------------------------------------*
*        FORM EXTRACT_TRIGGER_HABEN_POSITION                         *
*--------------------------------------------------------------------*
* Bestueckung eines EXTRACT-Satzes, wenn der Saldo negativ ist.
*--------------------------------------------------------------
FORM EXTRACT_TRIGGER_HABEN_POSITION.
  MOVE: VTAB-BUKRS         TO SORT-BUK1,  " Buchungskreis
        VTAB-GSBER         TO SORT-GSB1,  " Geschaeftsbereich
        VTAB-PRKYH(2)      TO SORT-LIST,  " Listenteil
        VTAB-PRKYH+00(2)   TO SORT-PKY1,                    " Stufe 1
        VTAB-PRKYH+02(2)   TO SORT-PKY2,                    " Stufe 2
        VTAB-PRKYH+04(2)   TO SORT-PKY3,                    " Stufe 3
        VTAB-PRKYH+06(2)   TO SORT-PKY4,                    " Stufe 4
        VTAB-PRKYH+08(2)   TO SORT-PKY5,                    " Stufe 5
        VTAB-PRKYH+10(2)   TO SORT-PKY6,                    " Stufe 6
        VTAB-PRKYH+12(2)   TO SORT-PKY7,                    " Stufe 7
        VTAB-PRKYH+14(2)   TO SORT-PKY8,                    " Stufe 8
        VTAB-PRKYH+16(2)   TO SORT-PKY9,                    " Stufe 9
        VTAB-PRKYH+18(2)   TO SORT-PKY0,                    " Stufe 10
        VTAB-VERDS         TO SORT-VERD,  " Verdichtungsschluessel
        VTAB-BUKRS         TO SORT-BUK2,  " Buchungskreis  (KONS)
        VTAB-GSBER         TO SORT-GSB2,  " Geschaeftsber. (KONS)
        VTAB-PRKYH         TO DATE-PRKEY, " Programmschluessel
        VTAB-BSALD         TO DATE-BSALD, " Saldo Berichtszeitr.
        VTAB-VSALD         TO DATE-VSALD, " Saldo Verglzeitr.
        VTAB-WAERS         TO DATE-WAERS, " Programmschluessel
        VTAB-BSALD2        TO DATE-BSALD2, " Saldo Berichtszeitr.
        VTAB-VSALD2        TO DATE-VSALD2, " Saldo Verglzeitr.
        VTAB-WAERS         TO DATE-WAERS. " Währung
* Geschaeftsbreichs-Konsolidierung ??
* -----------------------------------
  IF BILAGKON = '2'.
    MOVE '****' TO SORT-GSB1.          " Geschaeftsbereich
  ENDIF.
ENDFORM.                    "EXTRACT_TRIGGER_HABEN_POSITION

*EJECT
*---------------------------------------------------------------------*
*        FORM TEXT_GRUPPENANFANG                                      *
*---------------------------------------------------------------------*
*  Gruppenanfangstexte besorgen
*  ----------------------------
FORM TEXT_GRUPPENANFANG USING TEXT_STUFE.
* Ergebnisschluessel in Abhaengigkeit der Gruppenstufe
* aus der internen Tabelle X011P besorgen.
* ----------------------------------------------------
  CHECK P_SUMM = ' '
     OR P_SUMM = '0'
     OR ( TEXT_STUFE GE '1' AND
          TEXT_STUFE LE P_SUMM ).

  CLEAR X011P.
  CASE TEXT_STUFE.
    WHEN '1'.
      MOVE DATE-PRKEY(02)  TO X011P-PRKEY(02).
    WHEN '2'.
      MOVE DATE-PRKEY(04)  TO X011P-PRKEY(04).
    WHEN '3'.
      MOVE DATE-PRKEY(06)  TO X011P-PRKEY(06).
    WHEN '4'.
      MOVE DATE-PRKEY(08)  TO X011P-PRKEY(08).
    WHEN '5'.
      MOVE DATE-PRKEY(10)  TO X011P-PRKEY(10).
    WHEN '6'.
      MOVE DATE-PRKEY(12)  TO X011P-PRKEY(12).
    WHEN '7'.
      MOVE DATE-PRKEY(14)  TO X011P-PRKEY(14).
    WHEN '8'.
      MOVE DATE-PRKEY(16)  TO X011P-PRKEY(16).
    WHEN '9'.
      MOVE DATE-PRKEY(18)  TO X011P-PRKEY(18).
    WHEN '0'.
      MOVE DATE-PRKEY      TO X011P-PRKEY.
  ENDCASE.
  READ TABLE X011P WITH KEY X011P-PRKEY BINARY SEARCH.
* nur weitermache wenn Eintrag gefunden wurde, da sonst
* auch kein Text in Tabelle X011Q vorhanden ist.
* ----------------------------------------------
  CHECK SY-SUBRC = 0.

* Texte nur bei neuem Suchargument ausgeben
* -----------------------------------------
  CHECK X011P-ERGSL NE OLD-SUARG1.
  MOVE  X011P-ERGSL TO OLD-SUARG1.

  LOOP AT X011Q WHERE ERGSL = X011P-ERGSL
                  AND TXTYP = 'A'.
    CLEAR I011Q.
    MOVE X011Q-TXT45 TO I011Q-TEXT.
    APPEND I011Q.
    RESERVE = RESERVE + 1.
    IF SCR_PRINT_TO_FORM = YES.        "SCRIPT
      PERFORM FILL_SCR_DATA USING  'A' X011Q-ERGSL.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "TEXT_GRUPPENANFANG

*EJECT
*---------------------------------------------------------------------*
*  FORM TEXT_GRUPPENENDE                                              *
*---------------------------------------------------------------------*
*  Gruppenendetexte besorgen
*  -------------------------
FORM TEXT_GRUPPENENDE USING TEXT_STUFE TEXT_BSUM TEXT_VSUM TEXT_BSUM2 TEXT_VSUM2 .
* Ergebnisschluessel in Abhaengigkeit der Gruppenstufe
* aus der internen Tabelle X011P besorgen und ausgeben.
* ----------------------------------------------------
  CHECK P_SUMM = ' '
     OR P_SUMM = '0'
     OR ( TEXT_STUFE GE '1' AND
          TEXT_STUFE LE P_SUMM ).

  CLEAR X011P.
  CASE TEXT_STUFE.
    WHEN '1'.
      MOVE DATE-PRKEY(02)  TO X011P-PRKEY(02).
    WHEN '2'.
      MOVE DATE-PRKEY(04)  TO X011P-PRKEY(04).
    WHEN '3'.
      MOVE DATE-PRKEY(06)  TO X011P-PRKEY(06).
    WHEN '4'.
      MOVE DATE-PRKEY(08)  TO X011P-PRKEY(08).
    WHEN '5'.
      MOVE DATE-PRKEY(10)  TO X011P-PRKEY(10).
    WHEN '6'.
      MOVE DATE-PRKEY(12)  TO X011P-PRKEY(12).
    WHEN '7'.
      MOVE DATE-PRKEY(14)  TO X011P-PRKEY(14).
    WHEN '8'.
      MOVE DATE-PRKEY(16)  TO X011P-PRKEY(16).
    WHEN '9'.
      MOVE DATE-PRKEY(18)  TO X011P-PRKEY(18).
    WHEN '0'.
      MOVE DATE-PRKEY      TO X011P-PRKEY.
  ENDCASE.
  READ TABLE X011P WITH KEY X011P-PRKEY BINARY SEARCH.
* nur weitermachen wenn Eintrag gefunden wurde, da sonst
* auch kein Text in Tabelle X011Q vorhanden ist.
* ------------------------------------------------------
  CHECK SY-SUBRC    =  0.
* nur weitermachen wenn Summenausgabe erwuenscht ist.
* ---------------------------------------------------
  CHECK X011P-SUMME <> SPACE.
  CASE X011P-SUMME.
    WHEN 'E'.
      CLEAR LOOPCTR.
      PERFORM FILL_I011Q USING 'E' TEXT_STUFE TEXT_BSUM TEXT_VSUM TEXT_BSUM2 TEXT_VSUM2.

    WHEN 'S'.
      CLEAR LOOPCTR.
      PERFORM FILL_I011Q USING 'S' TEXT_STUFE SS-BSUM SS-VSUM SS-BSUM2 SS-VSUM2.

    WHEN 'X'.
      CLEAR LOOPCTR.
      PERFORM FILL_I011Q USING 'E' TEXT_STUFE TEXT_BSUM TEXT_VSUM TEXT_BSUM2 TEXT_VSUM2.
      CLEAR LOOPCTR.
      PERFORM FILL_I011Q USING 'S' TEXT_STUFE SS-BSUM SS-VSUM SS-BSUM2 SS-VSUM2.
  ENDCASE.

* Zeilenvorschuebe in Abhaengigkeit der Gruppenstufe
* --------------------------------------------------
  IF LOOPCTR NE 0.
    CLEAR I011Q.
    CASE TEXT_STUFE.
      WHEN '1'.
        MOVE    2   TO I011Q-SKIP.
        MOVE 'SKIP' TO I011Q-TEXT.
        APPEND I011Q.
        RESERVE = RESERVE + 2.
      WHEN '2'.
        MOVE    1   TO I011Q-SKIP.
        MOVE 'SKIP' TO I011Q-TEXT.
        APPEND I011Q.
        RESERVE = RESERVE + 1.
      WHEN '3'.
        MOVE    1   TO I011Q-SKIP.
        MOVE 'SKIP' TO I011Q-TEXT.
        APPEND I011Q.
        RESERVE = RESERVE + 1.
    ENDCASE.
  ENDIF.

* Texte und Summen ausgeben
* -------------------------
  RESERVE RESERVE LINES.
  LOOP AT I011Q.
    CHECK SCR_PRINT_TO_FORM = NO.      "SCRIPT
    FORMAT COLOR COL_KEY INTENSIFIED.
    IF I011Q-TEXT(4) = 'SKIP'.
      DO I011Q-SKIP TIMES.
        WRITE:     / SPACE,
              64(138) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
        PERFORM FILL_V_LINE.
      ENDDO.
    ELSE.
      IF I011Q-STAR(1) = '*'.
*       Berechnung absolute Abweichung
*       ------------------------------
        H-SALDO  = I011Q-BSUM - I011Q-VSUM.
        H-SALDO2 = I011Q-BSUM2 - I011Q-VSUM2.

*       Berechnung relative Abweichung
*       ------------------------------
        CLEAR: H-RELAB,
               H-RELAB2.
        IF I011Q-VSUM NE 0 OR I011Q-VSUM2 NE 0.            " <= keine Division durch Null
          PERFORM REL_ABW_BERECHNEN USING    I011Q-BSUM
                                             I011Q-VSUM
                                             I011Q-BSUM2
                                             I011Q-VSUM2
                                    CHANGING H-RELAB
                                             H-RELAB2.
        ENDIF.

        WRITE: /14(50) I011Q-TEXT.

        IF TEXT_STUFE = '1'.
          FORMAT COLOR COL_TOTAL  INTENSIFIED.
        ELSE.
          FORMAT COLOR COL_TOTAL  INTENSIFIED OFF.
        ENDIF.
        PERFORM  BETRAGS_AUSGABE
          USING  SPACE                 " Klammer auf '('
                 I011Q-BSUM            " Summe Berichtszeitraum
                 I011Q-BSUM2           " Summe Berichtszeitraum
                 SPACE                 " Klammer zu  ')'
                 SPACE                 " Klammer auf '('
                 I011Q-VSUM            " Summe Vergleichszeitraum
                 I011Q-VSUM2           " Summe Vergleichszeitraum
                 SPACE                 " Klammer zu  ')'
                 H-SALDO               " Absol. Abweichung
                 H-SALDO2               " Absol. Abweichung
                 H-RELAB               " Rel. Abweichung
                 H-RELAB2              " Rel. Abweichung
                 I011Q-STAR.           " Summensterne
      ELSE.
        WRITE: /14(50) I011Q-TEXT,
                64(138) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
      PERFORM FILL_V_LINE.
    ENDIF.
  ENDLOOP.

  REFRESH I011Q.
  CLEAR:  I011Q,
          RESERVE.
ENDFORM.                    "TEXT_GRUPPENENDE

*EJECT
*---------------------------------------------------------------------*
*  FORM FILL_I011Q                                                    *
*---------------------------------------------------------------------*
FORM FILL_I011Q USING FILL_TXTYP FILL_STUFE FILL_BSUM FILL_VSUM FILL_BSUM2 FILL_VSUM2.
  LOOP AT X011Q WHERE ERGSL = X011P-ERGSL
                  AND TXTYP = FILL_TXTYP.

    LOOPCTR = LOOPCTR + 1.
    CLEAR I011Q.
    IF LOOPCTR = 1.
      CASE FILL_STUFE.
        WHEN '1'.
          MOVE  '*1*'      TO I011Q-STAR.
        WHEN '2'.
          MOVE  '*2*'      TO I011Q-STAR.
        WHEN '3'.
          MOVE  '*3*'      TO I011Q-STAR.
        WHEN '4'.
          MOVE  '*4*'      TO I011Q-STAR.
        WHEN '5'.
          MOVE  '*5*'      TO I011Q-STAR.
        WHEN '6'.
          MOVE  '*6*'      TO I011Q-STAR.
        WHEN '7'.
          MOVE  '*7*'      TO I011Q-STAR.
        WHEN '8'.
          MOVE  '*8*'      TO I011Q-STAR.
        WHEN '9'.
          MOVE  '*9*'      TO I011Q-STAR.
        WHEN '0'.
          MOVE  '*10*'     TO I011Q-STAR.
      ENDCASE.
      IF X011Q-TXT45 NE SPACE.
        MOVE X011Q-TXT45 TO I011Q-TEXT.
      ELSE.
        MOVE X011Q-ERGSL TO I011Q-TEXT.
      ENDIF.
      MOVE: FILL_BSUM  TO I011Q-BSUM,  " Summe Berichtszeitraum
            FILL_VSUM  TO I011Q-VSUM.  " Summe Vergleichszeitraum

      MOVE: FILL_BSUM2 TO I011Q-BSUM2,  " Summe Berichtszeitraum
            FILL_VSUM2 TO I011Q-VSUM2.  " Summe Vergleichszeitraum

      IF SCR_PRINT_TO_FORM = YES.      "SCRIPT
        PERFORM FILL_SCR_DATA USING FILL_TXTYP X011P-ERGSL.
      ENDIF.

    ELSE.
      IF X011Q-TXT45 NE SPACE.
        MOVE X011Q-TXT45 TO I011Q-TEXT.
      ELSE.
        MOVE X011Q-ERGSL TO I011Q-TEXT.
      ENDIF.
    ENDIF.
    APPEND I011Q.
    RESERVE = RESERVE + 1.
  ENDLOOP.

* Summenausgabe, wenn kein Eintrag gefunden wurde.
* ------------------------------------------------
  CLEAR I011Q.
  IF LOOPCTR = 0.
    MOVE: FILL_BSUM TO I011Q-BSUM,
          FILL_VSUM TO I011Q-VSUM.

    MOVE: FILL_BSUM2 TO I011Q-BSUM2,
          FILL_VSUM2 TO I011Q-VSUM2.

    CASE FILL_STUFE.
      WHEN '1'.
        MOVE  '*1*'      TO I011Q-STAR.
      WHEN '2'.
        MOVE  '*2*'      TO I011Q-STAR.
      WHEN '3'.
        MOVE  '*3*'      TO I011Q-STAR.
      WHEN '4'.
        MOVE  '*4*'      TO I011Q-STAR.
      WHEN '5'.
        MOVE  '*5*'      TO I011Q-STAR.
      WHEN '6'.
        MOVE  '*6*'      TO I011Q-STAR.
      WHEN '7'.
        MOVE  '*7*'      TO I011Q-STAR.
      WHEN '8'.
        MOVE  '*8*'      TO I011Q-STAR.
      WHEN '9'.
        MOVE  '*9*'      TO I011Q-STAR.
      WHEN '0'.
        MOVE  '*10*'     TO I011Q-STAR.
    ENDCASE.
    APPEND I011Q.
    RESERVE = RESERVE + 1.
    IF SCR_PRINT_TO_FORM = YES.        "SCRIPT
      PERFORM FILL_SCR_DATA USING FILL_TXTYP X011P-ERGSL.
    ENDIF.
  ENDIF.
ENDFORM.                                                    "FILL_I011Q

*EJECT
*---------------------------------------------------------------------*
*  FORM TEXTAUSGABE_ANFANG                                            *
*---------------------------------------------------------------------*
FORM TEXTAUSGABE_ANFANG.
  CHECK SCR_PRINT_TO_FORM = NO.        "SCRIPT
* Eine Zeile fuer den ersten Posten reservieren
* ---------------------------------------------
  RESERVE = RESERVE + 1.
  RESERVE RESERVE LINES.

  LOOP AT I011Q.
    FORMAT COLOR COL_KEY INTENSIFIED.
    IF I011Q-TEXT(4) = 'SKIP'.
      DO I011Q-SKIP TIMES.
        WRITE:    / SPACE,
             64(138) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
        PERFORM FILL_V_LINE.
      ENDDO.
    ELSE.
      WRITE: /14(45) I011Q-TEXT,       " Text
              64(138) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
      PERFORM FILL_V_LINE.
    ENDIF.
  ENDLOOP.

  REFRESH I011Q.
  CLEAR:  I011Q,
          RESERVE.
ENDFORM.                    "TEXTAUSGABE_ANFANG

*EJECT
*--------------------------------------------------------------------*
*        FORM GET_BUPE_VIA_DATE                                      *
*--------------------------------------------------------------------*
* Berichts- und Vergleichsperioden ueber Datum besorgen
*------------------------------------------------------
FORM GET_PER_VIA_DATE.
* Berichtszeitraum VON besorgen
* -----------------------------
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = SKB1-BUKRS
      DATE  = KALZEITB-LOW
    IMPORTING
      CURRY = PER-BJJV
      CURRM = PER-BMMV.

* Berichtszeitraum BIS besorgen
* -----------------------------
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = SKB1-BUKRS
      DATE  = KALZEITB-HIGH
    IMPORTING
      CURRY = PER-BJJB
      CURRM = PER-BMMB.

  SELECT SINGLE * FROM T009Y WHERE PERIV = T001-PERIV AND
                                   GJAHR = PER-BJJB.
  IF ( SY-SUBRC <> 0 AND PER-BMMB = '12' ) OR
     ( SY-SUBRC = 0 AND PER-BMMB = T009Y-ANZBP ).
    PER-BMMB = '16'.
  ENDIF.

* Vergleichszeitraum VON besorgen
* -------------------------------
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = SKB1-BUKRS
      DATE  = KALZEITV-LOW
    IMPORTING
      CURRY = PER-VJJV
      CURRM = PER-VMMV.

* Vergleichszeitraum BIS besorgen
* -------------------------------
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = SKB1-BUKRS
      DATE  = KALZEITV-HIGH
    IMPORTING
      CURRY = PER-VJJB
      CURRM = PER-VMMB.

  SELECT SINGLE * FROM T009Y WHERE PERIV = T001-PERIV AND
                                   GJAHR = PER-VJJB.
  IF ( SY-SUBRC <> 0 AND PER-VMMB = '12' ) OR
     ( SY-SUBRC = 0 AND PER-VMMB = T009Y-ANZBP ).
    PER-VMMB = '16'.
  ENDIF.
ENDFORM.                    "GET_PER_VIA_DATE

*EJECT
*--------------------------------------------------------------------*
*        FORM KONTEN_AUSGABE                                         *
*--------------------------------------------------------------------*

FORM KONTEN_AUSGABE.
  CHECK SCR_PRINT_TO_FORM = NO.        "SCRIPT
* Berechnung absolute Abweichung
* ------------------------------
  H-SALDO  = DATE-BSALD - DATE-VSALD.
  H-SALDO2 = DATE-BSALD2 - DATE-VSALD2.

* Berechnung relative Abweichung
* ------------------------------
  CLEAR: H-RELAB,
         H-RELAB2.
  IF DATE-VSALD NE 0 OR DATE-VSALD2 NE 0.                  " <= keine Division durch Null
    PERFORM REL_ABW_BERECHNEN USING    DATE-BSALD
                                       DATE-VSALD
                                       DATE-BSALD2
                                       DATE-VSALD2
                              CHANGING H-RELAB
                                       H-RELAB2.
  ENDIF.

  FORMAT COLOR COL_KEY INTENSIFIED.
  WRITE:
        /2 SORT-VERD,                  " Verdichtungsschluessel
           SORT-BUK2,                  " Buchungskreis     (KONS)
           SORT-GSB2,                  " Geschaeftsbereich (KONS)
           DATE-SAKAN,                 " Sachkonto
      (40) DATE-SKBEZ.                 " Kontobezeichnung

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  PERFORM  BETRAGS_AUSGABE
    USING  DATE-BKLAU                  " Klammer auf '('
           DATE-BSALD                  " Saldo Berichtszeitraum
           DATE-BSALD2                 " Saldo Berichtszeitraum
           DATE-BKLZU                  " Klammer zu  ')'
           DATE-VKLAU                  " Klammer auf '('
           DATE-VSALD                  " Saldo Vergleichszeitraum
           DATE-VSALD2                 " Saldo Vergleichszeitraum
           DATE-VKLZU                  " Klammer zu  ')'
              H-SALDO                  " Absol. Abweichung
              H-SALDO2                 " Absol. Abweichung
              H-RELAB                  " Rel. Abweichung
              H-RELAB2                 " Rel. Abweichung
                SPACE.                 " Summensterne

  PERFORM FILL_V_LINE.

  IF FI_LC_EX = '1'
  OR FI_LC_EX = '2'.
*   Extract an LC gewuenscht.
*   -------------------------
    IF PLANVERS = SPACE.
*     IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')'.             "wes190202
      IF DATE-BKLAU NE '(' AND DATE-BKLZU NE ')' OR         "wes190202
         DATE-BSALD IS INITIAL.                             "wes190202
*       Ist-Daten uebergeben
*       --------------------
        PERFORM KONZ_EXTRACT(RGCBILA0)
                     USING  DATE-ERGSL " Ergebnisschluessel
                            SORT-BUK1  " Buchungskreis
                            SD_GSB_S[]                      "wms086855
                            PLANVERS   " Planversion
*                          sort-ktnr  " Kontonummer        "VSK047132
                            DATE-SAKNR                      "VSK047132
                            SORT-GSB1  " Geschäftsbereich
                            PER-BJJV   " Berichtjahr
                            DATE-BSALD " Saldo
                            PER-BMMV   " Berichtsmonat -von-
                            PER-BMMB   " Berichtsmonat -bis-
                            GD_FNAME                        "xrp250698
                            GD_PRES                         "xrp250698
                            PA_RBCS    " receiver SEM-BCS  "xrp290904
                            FI_LC_EX                        "vhs134295
                            ERGSL-SOLL
                            ERGSL-HABEN
                            TRIGKZ.                         "wes140906
      ENDIF.
    ELSE.
*     IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')'.             "wes190202
      IF DATE-VKLAU NE '(' AND DATE-VKLZU NE ')' OR         "wes190202
         DATE-VSALD  IS INITIAL OR
         DATE-VSALD2 IS INITIAL.                            "wes190202
*       Plan-Daten uebergeben
*       DATE-VSALD enthaelt Planzahlen
*       ------------------------------
        PERFORM KONZ_EXTRACT(RGCBILA0)
                     USING  DATE-ERGSL " Ergebnisschluessel
                            SORT-BUK1  " Buchungskreis
                            SD_GSB_S[]                      "wms086855
                            PLANVERS   " Planversion
*                          sort-ktnr  " Kontonummer        "VSK047132
                            DATE-SAKNR                      "VSK047132
                            SORT-GSB1  " Geschäftsbereich
                            PER-BJJV   " Berichtjahr
                            DATE-VSALD " Saldo
                            PER-BMMV   " Berichtsmonat -von-
                            PER-BMMB   " Berichtsmonat -bis-
                            GD_FNAME                        "xrp250698
                            GD_PRES                         "xrp250698
                            PA_RBCS    " receiver SEM-BCS  "xrp290904
                            FI_LC_EX                        "vhs134295
                            ERGSL-SOLL
                            ERGSL-HABEN
                            TRIGKZ.                         "wes140906
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "KONTEN_AUSGABE

*EJECT
*--------------------------------------------------------------------*
*        FORM TRIGGER_AUSGABE                                        *
*--------------------------------------------------------------------*
FORM TRIGGER_AUSGABE.
* Berechnung absolute Abweichung
* ------------------------------
  H-SALDO  = DATE-BSALD  - DATE-VSALD.
  H-SALDO2 = DATE-BSALD2 - DATE-VSALD2.

* Berechnung relative Abweichung
* ------------------------------
  CLEAR: H-RELAB,
         H-RELAB2.
  IF DATE-VSALD NE 0 OR DATE-VSALD2 NE 0.                  " <= keine Division durch Null
    PERFORM REL_ABW_BERECHNEN USING    DATE-BSALD
                                       DATE-VSALD
                                       DATE-BSALD2
                                       DATE-VSALD2
                              CHANGING H-RELAB
                                       H-RELAB2.
  ENDIF.

  FORMAT COLOR COL_KEY INTENSIFIED.
  WRITE:
        /  SORT-VERD.                  " Verdichtungsschluessel

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  PERFORM  BETRAGS_AUSGABE
    USING  DATE-BKLAU                  " Klammer auf '('
           DATE-BSALD                  " Saldo Berichtszeitraum
           DATE-BSALD2                 " Saldo Berichtszeitraum
           DATE-BKLZU                  " Klammer zu  ')'
           DATE-VKLAU                  " Klammer auf '('
           DATE-VSALD                  " Saldo Vergleichszeitraum
           DATE-VSALD2                 " Saldo Vergleichszeitraum
           DATE-VKLZU                  " Klammer zu  ')'
              H-SALDO                  " Absol. Abweichung
              H-SALDO2                 " Absol. Abweichung
              H-RELAB                  " Rel. Abweichung
              H-RELAB2                 " Rel. Abweichung
                SPACE.                 " Summensterne
  PERFORM FILL_V_LINE.
ENDFORM.                    "TRIGGER_AUSGABE

*EJECT
*--------------------------------------------------------------------*
*        FORM BETRAGS_AUSGABE                                        *
*--------------------------------------------------------------------*
FORM BETRAGS_AUSGABE USING BETR_BKLAU  " Klammer auf '('
                           BETR_BSALD  " Saldo Berichtszeitraum
                           BETR_BSALD2 " Saldo Berichtszeitraum
                           BETR_BKLZU  " Klammer zu  ')'
                           BETR_VKLAU  " Klammer auf '('
                           BETR_VSALD  " Saldo Vergleichszeitraum
                           BETR_VSALD2 " Saldo Vergleichszeitraum
                           BETR_VKLZU  " Klammer zu  ')'
                           BETR_SALDO  " Absol. Abweichung
                           BETR_SALDO2 " Absol. Abweichung
                           BETR_RELAB  " Rel. Abweichung
                           BETR_RELAB2 " Rel. Abweichung
                           BETR_STERN. " Summensterne

  CASE BILASKAL.
    WHEN '0/0'.
      IF REPWAERS = SPACE.
*   Wenn keine Skalierung und keine Konsolidierungswaehrung
*   gewaehlt wurde werden die Betraege in Hauswaehrung ausgegeben.
*   --------------------------------------------------------------
        WRITE:   65  BETR_BKLAU,       " Klammer auf '('
              66(16) BETR_BSALD CURRENCY CTYP_WAERS, " Saldo Ber.zeitr.
                 82  BETR_BKLZU,       " Klammer zu  ')'
                 84  BETR_VKLAU,       " Klammer auf '('
              85(16) BETR_VSALD CURRENCY CTYP_WAERS, "Saldo Vergl.zeitr.
                101  BETR_VKLZU,       " Klammer zu  ')'
             103(16) BETR_SALDO CURRENCY CTYP_WAERS, " Absol. Abweichung
              120(7) BETR_RELAB NO-ZERO,             " Rel. Abweichung
              128(4) BETR_STERN, " Absol. Abweichung

                133  BETR_BKLAU,       " Klammer auf '('
             134(16) BETR_BSALD2 CURRENCY CTYP_WAERS2, " Saldo Ber.zeitr.
                150  BETR_BKLZU,       " Klammer zu  ')'
                152  BETR_VKLAU,       " Klammer auf '('
             153(16) BETR_VSALD2 CURRENCY CTYP_WAERS2, "Saldo Vergl.zeitr.
                170  BETR_VKLZU,       " Klammer zu  ')'
             172(16) BETR_SALDO2 CURRENCY CTYP_WAERS2, " Absol. Abweichung
              189(7) BETR_RELAB2 NO-ZERO,             " Rel. Abweichung
              197(4) BETR_STERN.

      ELSE.

*   Wenn keine Skalierung aber Konsolidierungswaehrung gewaehlt
*   wurde werden die Betraege in der im Parameter 'Konsolidierungs-
*   waehrung gewaehlten Waehrung ausgegeben.
*   ---------------------------------------------------------------
        WRITE:   65  BETR_BKLAU,       " Klammer auf '('
              66(16) BETR_BSALD CURRENCY REPWAERS,   " Saldo Ber.zeitr.
                 82  BETR_BKLZU,       " Klammer zu  ')'
                 84  BETR_VKLAU,       " Klammer auf '('
              85(16) BETR_VSALD CURRENCY REPWAERS,   "Saldo Vergl.zeitr.
                101  BETR_VKLZU,       " Klammer zu  ')'
             103(16) BETR_SALDO CURRENCY REPWAERS,   " Absol. Abweichung
              120(7) BETR_RELAB NO-ZERO,             " Rel. Abweichung
              128(4) BETR_STERN, " Absol. Abweichung

                133  BETR_BKLAU,       " Klammer auf '('
             134(16) BETR_BSALD2 CURRENCY REPWAERS, " Saldo Ber.zeitr.
                150  BETR_BKLZU,       " Klammer zu  ')'
                152  BETR_VKLAU,       " Klammer auf '('
             153(16) BETR_VSALD2 CURRENCY REPWAERS, "Saldo Vergl.zeitr.
                170  BETR_VKLZU,       " Klammer zu  ')'
             172(16) BETR_SALDO2 CURRENCY REPWAERS, " Absol. Abweichung
              189(7) BETR_RELAB2 NO-ZERO,             " Rel. Abweichung
              197(4) BETR_STERN.

      ENDIF.

    WHEN OTHERS.
      IF REPWAERS = SPACE.
*   Wird Skalierung aber keine Konsolidierungswaehrung gewaehlt,
*   werden die Betraege in Hauswaehrung gemaess Skalierung ausgegeben.
*   ------------------------------------------------------------------
        WRITE:   65  BETR_BKLAU,       " Klammer auf '('
              66(16) BETR_BSALD CURRENCY CTYP_WAERS  " Saldo Ber.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                 82  BETR_BKLZU,       " Klammer zu  ')'
                 84  BETR_VKLAU,       " Klammer auf '('
              85(16) BETR_VSALD CURRENCY CTYP_WAERS  "Saldo Vergl.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                101  BETR_VKLZU,       " Klammer zu  ')'
             103(16) BETR_SALDO CURRENCY CTYP_WAERS  " Absol. Abweichung
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
              120(7) BETR_RELAB NO-ZERO,             " Rel. Abweichung
              128(4) BETR_STERN, " Absol. Abweichung

                133  BETR_BKLAU,       " Klammer auf '('
             134(16) BETR_BSALD2 CURRENCY CTYP_WAERS2 " Saldo Ber.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                150  BETR_BKLZU,       " Klammer zu  ')'
                152  BETR_VKLAU,       " Klammer auf '('
             153(16) BETR_VSALD2 CURRENCY CTYP_WAERS2 "Saldo Vergl.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                170  BETR_VKLZU,       " Klammer zu  ')'
             172(16) BETR_SALDO2 CURRENCY CTYP_WAERS2 " Absol. Abweichung
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
              189(7) BETR_RELAB2 NO-ZERO,             " Rel. Abweichung
              197(4) BETR_STERN.

      ELSE.

*   Wenn Skalierung und Konsolidierungswaehrung gewaehlt wurde
*   werden die Betraege in der im Parameter 'Konsolidierungswaehrung'
*   gewaehlten Waehrung gemaess Skalierung ausgegeben.
*   -----------------------------------------------------------------
        WRITE:   65  BETR_BKLAU,       " Klammer auf '('
              66(16) BETR_BSALD CURRENCY REPWAERS    " Saldo Ber.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                 82  BETR_BKLZU,       " Klammer zu  ')'
                 84  BETR_VKLAU,       " Klammer auf '('
              85(16) BETR_VSALD CURRENCY REPWAERS    "Saldo Vergl.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                101  BETR_VKLZU,       " Klammer zu  ')'
             103(16) BETR_SALDO CURRENCY REPWAERS    " Absol. Abweichung
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
              120(7) BETR_RELAB NO-ZERO,             " Rel. Abweichung
              128(4) BETR_STERN, " Absol. Abweichung

                133  BETR_BKLAU,       " Klammer auf '('
             134(16) BETR_BSALD2 CURRENCY REPWAERS " Saldo Ber.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                150  BETR_BKLZU,       " Klammer zu  ')'
                152  BETR_VKLAU,       " Klammer auf '('
             153(16) BETR_VSALD2 CURRENCY REPWAERS "Saldo Vergl.zeitr.
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
                170  BETR_VKLZU,       " Klammer zu  ')'
             172(16) BETR_SALDO2 CURRENCY REPWAERS " Absol. Abweichung
                     ROUND BILASKAL(1) DECIMALS BILASKAL+2(1),
              189(7) BETR_RELAB2 NO-ZERO,             " Rel. Abweichung
              197(4) BETR_STERN.
      ENDIF.
  ENDCASE.
ENDFORM.                    "BETRAGS_AUSGABE

*EJECT
*---------------------------------------------------------------------*
*        FORM REL_ABW_BERECHNEN                                       *
*---------------------------------------------------------------------*
FORM REL_ABW_BERECHNEN USING    VALUE(REL_BSUM)
                                VALUE(REL_VSUM)
                                VALUE(REL_BSUM2)
                                VALUE(REL_VSUM2)
                       CHANGING VALUE(REL_ABW)
                                VALUE(REL_ABW2).
  DATA: VBETR LIKE H-AMOUNT.
  VBETR = REL_VSUM.
  IF VBETR < 0.
    VBETR = VBETR * -1.
  ENDIF.

  IF VBETR NE 0.
    IF BILAVART = '1'.
      REL_ABW = ( REL_BSUM - REL_VSUM ) * 1000 / VBETR.
    ELSE.
      REL_ABW = ( REL_BSUM * 1000 ) / VBETR.
    ENDIF.
  ELSE.
    REL_ABW = 0.
  ENDIF.

  VBETR = REL_VSUM2.
  IF VBETR < 0.
    VBETR = VBETR * -1.
  ENDIF.

  IF VBETR NE 0.
    IF BILAVART = '1'.
      REL_ABW2 = ( REL_BSUM2 - REL_VSUM2 ) * 1000 / VBETR.
    ELSE.
      REL_ABW2 = ( REL_BSUM2 * 1000 ) / VBETR.
    ENDIF.
  ELSE.
    REL_ABW2 = 0.
  ENDIF.

ENDFORM.                    "REL_ABW_BERECHNEN

*---------------------------------------------------------------------*
*        FORM FILL_V_LINE                                             *
*---------------------------------------------------------------------*
FORM FILL_V_LINE.
  WRITE:   1(1) SY-VLINE INTENSIFIED OFF,
           3(1) SY-VLINE INTENSIFIED OFF,
           8(1) SY-VLINE INTENSIFIED OFF,
          13(1) SY-VLINE INTENSIFIED OFF,
          64(1) SY-VLINE INTENSIFIED OFF,
          83(1) SY-VLINE INTENSIFIED OFF,
         102(1) SY-VLINE INTENSIFIED OFF,
         119(1) SY-VLINE INTENSIFIED OFF,
         127(1) SY-VLINE INTENSIFIED OFF,
         132(1) SY-VLINE INTENSIFIED OFF,

         151(1) SY-VLINE INTENSIFIED OFF,
         170(1) SY-VLINE INTENSIFIED OFF,
         188(1) SY-VLINE INTENSIFIED OFF,
         196(1) SY-VLINE INTENSIFIED OFF,
         201(1) SY-VLINE INTENSIFIED OFF.
ENDFORM.                    "FILL_V_LINE

*---------------------------------------------------------------------*
*        FORM ERGEBNIS_BERECHNEN                                      *
*---------------------------------------------------------------------*
FORM BERECHNE_ERGEBNIS.
*   Berechnung Ergebnis (Berichtsjahr)
  IF GTAB-BSALD GE 0.
    MOVE  PRKEY-SOLL    TO H-PRKEY.
  ELSE.
    MOVE  PRKEY-HABEN   TO H-PRKEY.
  ENDIF.

  IF H-PRKEY    NE NZUON-PRKEY
 AND H-PRKEY(ANHNG_LEN) NE ANHNG-PRKEY(ANHNG_LEN).
    CLEAR VTAB.
    MOVE: SORTBKRS       TO VTABKEY-BUKRS,
          GTAB-GSBER     TO VTABKEY-GSBER,
          CTYP_WAERS     TO VTABKEY-WAERS,
          CTYP_WAERS2    TO VTABKEY-WAER2.

    IF H-PRKEY(AKTVA_LEN) =  AKTVA-PRKEY(AKTVA_LEN) OR
       H-PRKEY(PSSVA_LEN) =  PSSVA-PRKEY(PSSVA_LEN).
*       Ergebnis Bilanz
      MOVE: E1POS-PRKEY  TO VTABKEY-PRKYS,
            E1NEG-PRKEY  TO VTABKEY-PRKYH,
               'X'       TO VTABKEY-VERDS.
    ELSE.
*       Ergebnis G u V
      MOVE: E2POS-PRKEY  TO VTABKEY-PRKYS,
            E2NEG-PRKEY  TO VTABKEY-PRKYH,
               'X'       TO VTABKEY-VERDS.
    ENDIF.

    READ TABLE VTAB WITH KEY VTABKEY.
    IF SY-SUBRC = 0.
      IF NOT ( H-SIGN IS INITIAL ).
        MOVE  H-SIGN     TO VTAB-BSIGN.
      ENDIF.
*       * -1 Ergebnis erscheint als Saldo auf der anderen Bilanzseite
      H-SALDO = GTAB-BSALD * -1.
      ADD   H-SALDO     TO VTAB-BSALD.

      H-SALDO2 = GTAB-BSALD2 * -1.
      ADD   H-SALDO2     TO VTAB-BSALD2.

      MODIFY VTAB INDEX SY-TABIX.
    ELSE.
      MOVE-CORRESPONDING VTABKEY TO VTAB.
      IF NOT ( H-SIGN IS INITIAL ).
        MOVE  H-SIGN     TO VTAB-BSIGN.
      ENDIF.
*       * -1 Ergebnis erscheint als Saldo auf der anderen Bilanzseite
      H-SALDO = GTAB-BSALD * -1.
      ADD   H-SALDO     TO VTAB-BSALD.

      H-SALDO2 = GTAB-BSALD2 * -1.
      ADD   H-SALDO2     TO VTAB-BSALD2.

      COLLECT VTAB.
    ENDIF.
  ENDIF.

*   Berechnung Ergebnis (Vergleichsjahr)
  IF GTAB-VSALD GE 0.
    MOVE  PRKEY-SOLL    TO H-PRKEY.
  ELSE.
    MOVE  PRKEY-HABEN   TO H-PRKEY.
  ENDIF.

  IF H-PRKEY    NE NZUON-PRKEY
 AND H-PRKEY(ANHNG_LEN) NE ANHNG-PRKEY(ANHNG_LEN).
    CLEAR VTAB.
    MOVE: SORTBKRS       TO VTABKEY-BUKRS,
          GTAB-GSBER     TO VTABKEY-GSBER,
          CTYP_WAERS     TO VTABKEY-WAERS,
          CTYP_WAERS2    TO VTABKEY-WAER2.

    IF H-PRKEY(AKTVA_LEN) =  AKTVA-PRKEY(AKTVA_LEN) OR
       H-PRKEY(PSSVA_LEN) =  PSSVA-PRKEY(PSSVA_LEN).
*       Ergebnis Bilanz
      MOVE: E1POS-PRKEY  TO VTABKEY-PRKYS,
            E1NEG-PRKEY  TO VTABKEY-PRKYH,
               'X'       TO VTABKEY-VERDS.
    ELSE.
*        Ergebnis G u V
      MOVE: E2POS-PRKEY  TO VTABKEY-PRKYS,
            E2NEG-PRKEY  TO VTABKEY-PRKYH,
               'X'       TO VTABKEY-VERDS.
    ENDIF.

    READ TABLE VTAB WITH KEY VTABKEY.
    IF SY-SUBRC = 0.
      IF NOT ( H-SIGN IS INITIAL ).
        MOVE  H-SIGN     TO VTAB-VSIGN.
      ENDIF.
*       * -1 Ergebnis erscheint als Saldo auf der anderen Bilanzseite
      H-SALDO = GTAB-VSALD * -1.
      ADD   H-SALDO     TO VTAB-VSALD.

      H-SALDO2 = GTAB-VSALD2 * -1.
      ADD   H-SALDO2    TO VTAB-VSALD2.

      MODIFY VTAB INDEX SY-TABIX.
    ELSE.
      MOVE-CORRESPONDING VTABKEY TO VTAB.
      IF NOT ( H-SIGN IS INITIAL ).
        MOVE  H-SIGN     TO VTAB-VSIGN.
      ENDIF.
*       * -1 Ergebnis erscheint als Saldo auf der anderen Bilanzseite
      H-SALDO = GTAB-VSALD * -1.
      ADD   H-SALDO     TO VTAB-VSALD.

      H-SALDO2 = GTAB-VSALD2 * -1.
      ADD   H-SALDO2     TO VTAB-VSALD2.

      COLLECT VTAB.
    ENDIF.
  ENDIF.
ENDFORM.                    "BERECHNE_ERGEBNIS

*---------------------------------------------------------------------*
*        FORM BILA_EIS_TRANSFER                                       *
*---------------------------------------------------------------------*
FORM BILA_EIS_TRANSFER USING EIS_SALDO EIS_SALDO2.
  EIS_REC-VERSN = BILAVERS.
  EIS_REC-PVERS = PLANVERS.
  EIS_REC-BASIS = DATE-ERGSL.
  EIS_REC-KONTO = SORT-KTNR.
  EIS_REC-BUKRS = SORT-BUK2.
  EIS_REC-GSBER = SORT-GSB2.
  EIS_REC-BJAHR = BILBJAHR.
  EIS_REC-BMVON = B-MONATE-LOW.
  EIS_REC-BMBIS = B-MONATE-HIGH.
  EIS_REC-WAERS = CTYP_WAERS.
  EIS_REC-BETRG = EIS_SALDO.
  EIS_REC-WAER2 = CTYP_WAERS2.
  EIS_REC-BETR2 = EIS_SALDO2.
  PERFORM TRANSFER_FI_DATA_TO_EIS(RKCFISEL) USING  EIS_REC .
ENDFORM.                    "BILA_EIS_TRANSFER

*---------------------------------------------------------------------*
*        FORM SCHEDMAN_START_STOP                                     *
*---------------------------------------------------------------------*
FORM SCHEDMAN_START_STOP USING P_COMMAND.
* local statics
  STATICS: LS_KEY_STATIC LIKE SCHEDMAN_KEY.
*local data declaration
  DATA: GS_KEY      LIKE SCHEDMAN_KEY.
  DATA: GT_SPONO    LIKE SCHEDMAN_SPOOL.

  DATA: LD_WORKLIST_FLAG(1).
  DATA: LS_DETAIL   LIKE SCHEDMAN_DETAIL_USER.
  DATA: LT_SELKRIT  LIKE SCHEDMAN_SELKRIT OCCURS 0 WITH HEADER LINE.
  DATA: LT_PARAM    LIKE SCHEDMAN_SELKRIT OCCURS 0 WITH HEADER LINE.
  DATA: LS_WITEM    LIKE SCMA_WITEM.
  DATA: LS_EVENT    LIKE SCMA_EVENT.
  DATA: LS_EXT      LIKE SCHEDMAN_EXT.
  DATA: LS_MESSAGE LIKE SCHEDMAN_MESSAGE,
        LD_OBJECTS LIKE SMMAIN-NR_OF_OBJECTS,
        LD_APLSTAT LIKE SMMAIN-APLSTAT.

* muss in scmatasks
  LS_DETAIL-REPID       = SY-REPID.
  LS_DETAIL-VARIANTE    = SY-SLSET.      "<<die variante
  LS_DETAIL-APPLICATION = 'FI-GL'.
* save some select-options
  CLEAR LT_SELKRIT.
  LT_SELKRIT-STRUCTURE  = 'BKPF'.
  LT_SELKRIT-FIELD      = 'BUKRS'.
  LOOP AT SD_BUKRS.
    MOVE-CORRESPONDING SD_BUKRS TO LT_SELKRIT.
    APPEND LT_SELKRIT.
  ENDLOOP.

  LT_PARAM-ENTRY     = 1.
  LT_PARAM-OPTIO     = 'EQ'.
  LT_PARAM-STRUCTURE = 'BKPF'.
  LT_PARAM-FIELD     = 'GJAHR'.
  LT_PARAM-LOW       = BILBJAHR.
  APPEND LT_PARAM.

  LT_PARAM-OPTIO     = 'BT'.
  LT_PARAM-STRUCTURE = 'RFSDO'.
  LT_PARAM-FIELD     = 'BILABMON'.
  LT_PARAM-LOW       = B-MONATE-LOW.
  LT_PARAM-HIGH      = B-MONATE-HIGH.
  APPEND LT_PARAM.

  IF ALLGLINE NE SPACE.
    LT_PARAM-STRUCTURE = 'RFPDO'.
    LT_PARAM-FIELD     = 'ALLGLINE'.
    LT_PARAM-LOW       = ALLGLINE.
    APPEND LT_PARAM.
  ENDIF.


  IF P_COMMAND = 'START'.
    LS_WITEM-WF_WITEM = WF_WITEM.
    LS_WITEM-WF_WLIST = WF_WLIST.
    CALL FUNCTION 'KPEP_MONI_INIT_RECORD'
      EXPORTING
        LS_DETAIL  = LS_DETAIL
        LS_WITEM   = LS_WITEM
      IMPORTING
        LS_KEY     = LS_KEY_STATIC
      TABLES
        LT_SELKRIT = LT_SELKRIT
        LT_PARAM   = LT_PARAM.

  ELSEIF P_COMMAND = 'STOP'.
    LD_APLSTAT  = '0'.
    LS_EVENT-WF_WITEM = WF_WITEM.
    LS_EVENT-WF_OKEY  = WF_OKEY.
    IF LIST_CREATET IS INITIAL.
      LS_EVENT-WF_EVENT = 'ERROR'.
    ELSE.
      LS_EVENT-WF_EVENT = 'FINISHED'.
    ENDIF.
    CALL FUNCTION 'KPEP_MONI_CLOSE_RECORD'
      EXPORTING
        LS_KEY        = LS_KEY_STATIC
        LS_SCMA_EVENT = LS_EVENT
      CHANGING
        LD_APLSTAT    = LD_APLSTAT
      EXCEPTIONS
*       NO_ID_GIVEN   = 1
        OTHERS        = 0.

    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  COMMIT WORK.           " <<<<<<<<<<  C O M M I T  W O R K  >>>>>>>

ENDFORM.                    "SCHEDMAN_START_STOP

INCLUDE ZBILA_I0102.
*INCLUDE ZBILA_I02.                      "SCRIPT
*&---------------------------------------------------------------------*
*&      Form  FNAME_FOR_ECCS_GET
*&---------------------------------------------------------------------*
*       get file name for extract file to EC-CS
*----------------------------------------------------------------------*
*      <--CD_FNAME          name of file for extract
*      <--CD_PRES           flag: presentation server
*      <--CD_LEAVE_PROGRAM  flag: leave program
*----------------------------------------------------------------------*
FORM FNAME_FOR_ECCS_GET                                     "xrp250698
                        CHANGING CD_FNAME LIKE FC03TAB-PL00_LFILE
                                 CD_PRES  LIKE FCINTAB-FLG_PRES
                                 CD_LEAVE_PROGRAM.
*- local data ---------------------------------------------------------*
  DATA: LD_INDMC LIKE T000K-INDMC.
  DATA: LD_INDLC LIKE T000K-INDLC.                          "xrp260704
*----------------------------------------------------------------------*
** begin xrp060400                     "begin xrp040803
*  CALL FUNCTION 'FC_T000K_READ'
*    IMPORTING
*      E_INDMC                  = LD_INDMC
*    EXCEPTIONS
*      OTHERS                   = 1.
*  IF SY-SUBRC <> 0.
**    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
**  SELECT SINGLE INDMC FROM T000K INTO LD_INDMC.
** end xrp060400
*  SELECT SINGLE INDMC                                      "xrp260704
*    FROM T000k                                             "xrp260704
*    INTO LD_INDMC.                     "end xrp040803      "xrp260704
*                                                           "xrp260704
*  CHECK LD_INDMC = 'X'.                                    "xrp260704

  CALL FUNCTION 'FC_T000K_READ'                             "xrp260704
    IMPORTING                                               "xrp260704
      E_INDMC = LD_INDMC                                    "xrp260704
      E_INDLC = LD_INDLC.                                   "xrp260704

  CHECK LD_INDMC = 'X'                                      "xrp260704
  OR    LD_INDLC = 'X'.                                     "xrp260704

  CALL SELECTION-SCREEN 2000 STARTING AT 2 2.
* SY-SUBRC <> 0. => User cancelled popup
  IF SY-SUBRC <> 0.
    CLEAR: PA_FNAME, PA_PRES, PA_APPL.
    CD_LEAVE_PROGRAM = 'X'.
    EXIT.
  ENDIF.
* User didn't cancel => use filename supplied
  CD_FNAME = PA_FNAME.
  IF PA_PRES = 'X'.
    CD_PRES = 'X'.
  ELSE.
    CD_PRES = ' '.
  ENDIF.
ENDFORM.                    " FNAME_FOR_ECCS_GET

*&---------------------------------------------------------------------*
*&      Form  CURRENCY_INFORMATION_GET
*&---------------------------------------------------------------------*
FORM CURRENCY_INFORMATION_GET CHANGING P_CTYP_WAERS.
* local data declaration
  STATICS: LS_X001 LIKE X001.

  IF SD_CURTP IS INITIAL
  OR SD_CURTP = '10'.
    P_CTYP_WAERS = T001-WAERS.
  ELSE.
    IF SKB1-BUKRS IS INITIAL.                               "n1255009
      SKB1-BUKRS = T001-BUKRS.                              "n1255009
    ENDIF.                                                  "n1255009
    IF LS_X001-BUKRS <> SKB1-BUKRS.
      CLEAR: LS_X001.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          I_BUKRS                = SKB1-BUKRS
        IMPORTING
          E_X001                 = LS_X001
        EXCEPTIONS
          CURRENCY_2_NOT_DEFINED = 1
          CURRENCY_3_NOT_DEFINED = 2
          OTHERS                 = 3.
    ENDIF.

    IF SD_CURTP = LS_X001-CURT2.
      P_CTYP_WAERS = LS_X001-HWAE2.
    ENDIF.
    IF SD_CURTP = LS_X001-CURT3.
      P_CTYP_WAERS = LS_X001-HWAE3.
    ENDIF.
  ENDIF.
ENDFORM.                    " CURRENCY_INFORMATION_GET

*&---------------------------------------------------------------------*
*&      Form  CURRENCY_INFORMATION_GET
*&---------------------------------------------------------------------*
FORM CURRENCY_INFORMATION_GET2 CHANGING P_CTYP_WAERS2.
* local data declaration
  STATICS: LS_X001 LIKE X001.

  IF SD_CURT2 IS INITIAL
  OR SD_CURT2 = '10'.
    P_CTYP_WAERS2 = T001-WAERS.
  ELSE.
    IF SKB1-BUKRS IS INITIAL.                               "n1255009
      SKB1-BUKRS = T001-BUKRS.                              "n1255009
    ENDIF.                                                  "n1255009
    IF LS_X001-BUKRS <> SKB1-BUKRS.
      CLEAR: LS_X001.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          I_BUKRS                = SKB1-BUKRS
        IMPORTING
          E_X001                 = LS_X001
        EXCEPTIONS
          CURRENCY_2_NOT_DEFINED = 1
          CURRENCY_3_NOT_DEFINED = 2
          OTHERS                 = 3.
    ENDIF.

    IF SD_CURT2 = LS_X001-CURT2.
      P_CTYP_WAERS2 = LS_X001-HWAE2.
    ENDIF.
    IF SD_CURT2 = LS_X001-CURT3.
      P_CTYP_WAERS2 = LS_X001-HWAE3.
    ENDIF.
  ENDIF.
ENDFORM.                    " CURRENCY_INFORMATION_GET

*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_COMMENTARY_CREATE
*&---------------------------------------------------------------------*
FORM BSPL_LIST_COMMENTARY_CREATE
                          TABLES PT_TOP TYPE SLIS_T_LISTHEADER.
* local data declarations
  DATA: LS_TOP   TYPE SLIS_LISTHEADER.
  DATA: LS_TCURT LIKE TCURT.
  DATA: LF_TEXT(200) TYPE C.

  DATA: VG_KEY(20).
  DATA: VG_INFO(60).

* header line
  CLEAR LS_TOP.
  LS_TOP-TYP  = 'H'.
  LS_TOP-INFO = T011T-VSTXT.
  APPEND LS_TOP TO PT_TOP.

  "select SINGLE * into VG_INFO from t001 where BUKRS eq .

  IF T001 IS NOT INITIAL.
    LS_TOP-TYP  = 'S'.
    LS_TOP-KEY  = TEXT-214. "'Empresa'.
    LS_TOP-INFO = T001-BUTXT.
    APPEND LS_TOP TO PT_TOP.
  ENDIF.

  IF SD_RLDNR[] IS INITIAL.
    IMPORT FAGL_BHDGD_RLDNR TO VG_KEY FROM MEMORY ID CON_FAGL_BHDGD_RLDNR.
  ELSE.
    READ TABLE SD_RLDNR INDEX 1.
    VG_KEY = SD_RLDNR-LOW.
  ENDIF.

  DATA: L_CURTPVALUE LIKE DD07L-DOMVALUE_L.
  DATA: L_CURTPTEXT  LIKE DD07T-DDTEXT.

  IF SD_CURTP IS INITIAL.
    SD_CURTP = '10'.
  ENDIF.

  L_CURTPVALUE = SD_CURTP.
  CALL FUNCTION 'FI_CUST_READ_DOMVALUETEXT'
    EXPORTING
      DOMNAME        = 'CURTP'
      DOMVALUE       = L_CURTPVALUE
      SPRAS          = BILASPRA
    IMPORTING
      DDTEXT         = L_CURTPTEXT
    EXCEPTIONS
      TEXT_NOT_FOUND = 1.

  IF SY-SUBRC <> 0.
    CLEAR L_CURTPTEXT.
  ENDIF.

  CONCATENATE VG_KEY '-' SD_CURTP INTO VG_KEY.
  VG_INFO = TEXT-202.
  REPLACE '$' WITH L_CURTPTEXT INTO VG_INFO.

* currency
  READ TABLE GT_BSPL INDEX 1.
  SELECT SINGLE * FROM TCURT INTO LS_TCURT
                 WHERE SPRAS = BILASPRA
                   AND WAERS = GT_BSPL-WAERS.
  CONCATENATE VG_KEY '-' GT_BSPL-WAERS INTO VG_KEY.
  CONCATENATE VG_INFO '-' LS_TCURT-LTEXT INTO VG_INFO SEPARATED BY SPACE.

  LS_TOP-TYP  = 'S'.
  LS_TOP-KEY  = VG_KEY.
  LS_TOP-INFO = VG_INFO.
  APPEND LS_TOP TO PT_TOP.


  "----------------------------------------------------------------------------

  IF SD_RLDN2[] IS INITIAL.
    IMPORT FAGL_BHDGD_RLDN2 TO VG_KEY FROM MEMORY ID CON_FAGL_BHDGD_RLDN2.
  ELSE.
    READ TABLE SD_RLDN2 INDEX 1.
    VG_KEY = SD_RLDN2-LOW.
  ENDIF.

  IF SD_CURT2 IS INITIAL.
    SD_CURT2 = '10'.
  ENDIF.

  L_CURTPVALUE = SD_CURT2.
  CALL FUNCTION 'FI_CUST_READ_DOMVALUETEXT'
    EXPORTING
      DOMNAME        = 'CURTP'
      DOMVALUE       = L_CURTPVALUE
      SPRAS          = BILASPRA
    IMPORTING
      DDTEXT         = L_CURTPTEXT
    EXCEPTIONS
      TEXT_NOT_FOUND = 1.

  IF SY-SUBRC <> 0.
    CLEAR L_CURTPTEXT.
  ENDIF.

  CONCATENATE VG_KEY '-' SD_CURT2 INTO VG_KEY.
  VG_INFO = TEXT-202.
  REPLACE '$' WITH L_CURTPTEXT INTO VG_INFO.

* currency
  READ TABLE GT_BSPL INDEX 1.
  SELECT SINGLE * FROM TCURT INTO LS_TCURT
                 WHERE SPRAS = BILASPRA
                   AND WAERS = GT_BSPL-WAER2.
  CONCATENATE VG_KEY '-' GT_BSPL-WAER2 INTO VG_KEY.
  CONCATENATE VG_INFO '-' LS_TCURT-LTEXT INTO VG_INFO SEPARATED BY SPACE.

  LS_TOP-TYP  = 'S'.
  LS_TOP-KEY  = VG_KEY.
  LS_TOP-INFO = VG_INFO.
  APPEND LS_TOP TO PT_TOP.
  "----------------------------------------------------------------------------

* reporting period
  CLEAR LS_TOP.
  LS_TOP-TYP  = 'S'.
  CONCATENATE PER-BJJV '.'    " Ber.Jahr  von
              PER-BMMV ' - '  " Ber.Monat von
              PER-BJJB '.'    " Ber.Jahr  bis
              PER-BMMB        " Ber.Monat bis
  INTO LS_TOP-KEY.
  CONCATENATE TEXT-200 '(PerRel)' INTO LS_TOP-INFO SEPARATED BY SPACE.
  "LS_TOP-INFO = TEXT-200.
  APPEND LS_TOP TO PT_TOP.

* comparison period
  CLEAR LS_TOP.
*  LS_TOP-TYP  = 'S'.
*  IF PLANVERS IS INITIAL.
*    CONCATENATE PER-VJJV '.'    " Ver.Jahr  von
*                PER-VMMV ' - '  " Ver.Monat von
*                PER-VJJB '.'    " Ver.Jahr  bis
*                PER-VMMB        " Ver.Monat bis
*    INTO LS_TOP-KEY.
*  ELSE.
*    CONCATENATE PER-BJJV '.'    " Ber.Jahr  von
*                PER-BMMV ' - '  " Ber.Monat von
*                PER-BJJB '.'    " Ber.Jahr  bis
*                PER-BMMB        " Ber.Monat bis
*    INTO LS_TOP-KEY.
*  ENDIF.

*  CONCATENATE TEXT-201 '(PerComp)' INTO LS_TOP-INFO SEPARATED BY SPACE.
  "LS_TOP-INFO = TEXT-201.
*  IF NOT PLANVERS IS INITIAL.
*    LF_TEXT = TEXT-006.
*    REPLACE ALL OCCURRENCES OF '-' IN LF_TEXT WITH SPACE.
*    CONDENSE LF_TEXT.
*    CONCATENATE LS_TOP-INFO ' ('
*                LF_TEXT     ')'
*    INTO LS_TOP-INFO.
*  ENDIF.
*  APPEND LS_TOP TO PT_TOP. CLEAR LS_TOP.

* additional heading
  IF NOT ( ALLGLINE IS INITIAL ).
    CLEAR LS_TOP.
    LS_TOP-TYP  = 'S'.
    LS_TOP-INFO = ALLGLINE.
    APPEND LS_TOP TO PT_TOP.
  ENDIF.

  LS_TOP-TYP  = 'S'.
  LS_TOP-KEY  = TEXT-215. "'Usuário'.
  LS_TOP-INFO = SY-UNAME.
  APPEND LS_TOP TO PT_TOP.

  LS_TOP-TYP  = 'S'.
  LS_TOP-KEY  = TEXT-216. "'Data/Hora'.
  CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM(4) INTO LS_TOP-INFO.
  CONCATENATE SY-UZEIT(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2) INTO LF_TEXT.
  CONCATENATE LS_TOP-INFO LF_TEXT INTO LS_TOP-INFO SEPARATED BY SPACE.

  APPEND LS_TOP TO PT_TOP.

ENDFORM.                    " BSPL_LIST_COMMENTARY_CREATE

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_TITLE_CREATE
*&---------------------------------------------------------------------*
FORM BSPL_GRID_TITLE_CREATE
                   CHANGING VALUE(P_GRID_TITLE).
* local data declaration
  DATA: LS_TCURT LIKE TCURT.

  READ TABLE GT_BSPL INDEX 1.
  SELECT SINGLE * FROM TCURT INTO LS_TCURT
                 WHERE SPRAS = BILASPRA
                   AND WAERS = GT_BSPL-WAERS.
  CONCATENATE T011T-VSTXT
               '-'
              TEXT-026
              GT_BSPL-WAERS
              LS_TCURT-LTEXT
         INTO P_GRID_TITLE SEPARATED BY ' '.
ENDFORM.                    " BSPL_GRID_TITLE_CREATE

*&---------------------------------------------------------------------*
*&      Form  BSPL_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM BSPL_ALV_VARIANT_F4 USING    VALUE(P_REPID)
                                  VALUE(P_HANDLE)
                         CHANGING P_VARIANT.
* local data declarations
  DATA: LS_VARIANT LIKE DISVARIANT.

  LS_VARIANT-REPORT   = P_REPID.
  LS_VARIANT-HANDLE   = P_HANDLE.
  LS_VARIANT-USERNAME = SY-UNAME.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT         = LS_VARIANT
      I_SAVE             = 'A'
      I_DISPLAY_VIA_GRID = 'X'
    IMPORTING
      ES_VARIANT         = LS_VARIANT.
  IF SY-SUBRC = 0.
    P_VARIANT = LS_VARIANT-VARIANT.
  ENDIF.
ENDFORM.                    " BSPL_ALV_VARIANT_F4

*&---------------------------------------------------------------------*
*&      Form  BSPL_SETTINGS_CREATE
*&---------------------------------------------------------------------*
FORM BSPL_SETTINGS_CREATE
                 CHANGING PS_BSPL_SETTINGS LIKE RFBILA_ALV_SETTINGS.
  DESCRIBE TABLE SD_BUKRS LINES SY-TFILL.
  IF SY-TFILL = 1.
    READ TABLE SD_BUKRS INDEX 1.
    IF SD_BUKRS-OPTION = 'EQ'
   AND SD_BUKRS-SIGN   = 'I'.
      PS_BSPL_SETTINGS-BUKRS   = SD_BUKRS-LOW.
    ENDIF.
  ENDIF.
  PS_BSPL_SETTINGS-FS_VERSION  = BILAVERS.
  PS_BSPL_SETTINGS-FS_LANGUAGE = BILASPRA.
  PS_BSPL_SETTINGS-ALTACCT     = ALLGALTK.
  PS_BSPL_SETTINGS-COMPTYPE    = BILAVART.
  PS_BSPL_SETTINGS-CLASSIC     = ABAP_FALSE."bilalist.
  PS_BSPL_SETTINGS-GRID        = BILAGRID.
  PS_BSPL_SETTINGS-TREE        = BILATREE.
*  PS_BSPL_SETTINGS-STRUCBLNCE  = BILASTSL.
  PS_BSPL_SETTINGS-REPID       = CON_REPID.
  PS_BSPL_SETTINGS-TITLE       = SY-TITLE.
  PS_BSPL_SETTINGS-ALLGLINE    = ALLGLINE.
  PS_BSPL_SETTINGS-PAGE_NO     = BILAPAGE.
  PS_BSPL_SETTINGS-GRID_VARI   = BILAGVAR.
*  PS_BSPL_SETTINGS-TREE_VARI   = BILATVAR.
  PS_BSPL_SETTINGS-TOTALS_LEVEL = BILASUMM.                 "n1486648

ENDFORM.                    " BSPL_SETTINGS_CREATE

*&---------------------------------------------------------------------*
*&      Form  GT_BSPL_FILL
*&---------------------------------------------------------------------*
FORM GT_BSPL_FILL USING VALUE(P_PERIOD).
  IF T004-KTOPL NE SKA1-KTOPL.
    SELECT SINGLE * FROM T004 WHERE "#EC CI_DB_OPERATION_OK[2389136]
                              KTOPL = SKA1-KTOPL.
  ENDIF.
  CLEAR: GT_BSPL.
  GT_BSPL-RLDNR  =  ZSKC1A-RLDNR.
  GT_BSPL-KTOPL  =  SKA1-KTOPL.
  GT_BSPL-RACCT  =  SKA1-SAKNR.
  GT_BSPL-KKTPL  =  T004-KKTPL.
  GT_BSPL-BILKT  =  SKA1-BILKT.
  IF T001-KTOP2 IS NOT INITIAL.                             "n1349761
    GT_BSPL-KTOP2  =  T001-KTOP2.
  ELSE.                                                     "n1349761
    GT_BSPL-KTOP2  =  SKA1-KTOPL.                           "n1349761
  ENDIF.                                                    "n1349761
  GT_BSPL-ALTKT   =  SKB1-ALTKT.
  GT_BSPL-RBUKRS  =  SKB1-BUKRS.
  GT_BSPL-RBUSA   =  ZSKC1A-GSBER.
  GT_BSPL-CURTP   =  ZSKC1A-CURTP.
  GT_BSPL-CURT2   =  ZSKC1A-CURT2.
  GT_BSPL-WAERS   =  CTYP_WAERS.
  GT_BSPL-WAER2   =  CTYP_WAERS2.
  GT_BSPL-SALDO   =  H-SALDO.
  GT_BSPL-SALDO2  =  H-SALDO2.
  GT_BSPL-RYEAR   =  G_RYEAR.
  GT_BSPL-POPER   =  G_POPER.
  GT_BSPL-PER01   = ZSKC1A-UM01K.
  GT_BSPL-PER02   = ZSKC1A-UM02K.
  GT_BSPL-PER03   = ZSKC1A-UM03K.
  GT_BSPL-PER04   = ZSKC1A-UM04K.
  GT_BSPL-PER05   = ZSKC1A-UM05K.
  GT_BSPL-PER06   = ZSKC1A-UM06K.
  GT_BSPL-PER07   = ZSKC1A-UM07K.
  GT_BSPL-PER08   = ZSKC1A-UM08K.
  GT_BSPL-PER09   = ZSKC1A-UM09K.
  GT_BSPL-PER10   = ZSKC1A-UM10K.
  GT_BSPL-PER11   = ZSKC1A-UM11K.
  GT_BSPL-PER12   = ZSKC1A-UM12K.
  GT_BSPL-PER13   = ZSKC1A-UM13K.
  GT_BSPL-PER14   = ZSKC1A-UM14K.
  GT_BSPL-PER15   = ZSKC1A-UM15K.
  GT_BSPL-PER16   = ZSKC1A-UM16K.

  CASE P_PERIOD.
    WHEN 'B'.
*.... sign for reporting period
      GT_BSPL-PERIOD_SIGN = '1'.
    WHEN 'V'.
*.... sign for comparison perid
      GT_BSPL-PERIOD_SIGN = '2'.
  ENDCASE.
  COLLECT GT_BSPL.

ENDFORM.                    " GT_BSPL_FILL

*&---------------------------------------------------------------------*
*&      Form  BSPL_ACCT_ZERO_BLNCE_DELETE
*&---------------------------------------------------------------------*
FORM BSPL_ACCT_ZERO_BLNCE_DELETE TABLES PT_BSPL TYPE TT_BSPL.
* local data declarations
  RANGES: LR_RACCT FOR SKA1-SAKNR.
  DATA:   BEGIN OF LT_BALANCE OCCURS 10000,
            RBUKRS      TYPE BUKRS,
            L_BALANCE11 LIKE H-AMOUNT,
            L_BALANCE12 LIKE H-AMOUNT,
            L_BALANCE21 LIKE H-AMOUNT,
            L_BALANCE22 LIKE H-AMOUNT,
          END OF LT_BALANCE.
  DATA:   L_RACCT        LIKE SKA1-SAKNR.

  SORT PT_BSPL BY RACCT.
  LOOP AT PT_BSPL.

    IF L_RACCT <> PT_BSPL-RACCT
   AND NOT ( L_RACCT IS INITIAL ).
*....save accounts with zero balances
      LOOP AT LT_BALANCE WHERE L_BALANCE11 <> 0
                            OR L_BALANCE12 <> 0
                            OR L_BALANCE21 <> 0
                            OR L_BALANCE22 <> 0.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        LR_RACCT-SIGN    = 'I'.
        LR_RACCT-OPTION  = 'EQ'.
        LR_RACCT-LOW     = L_RACCT.
        APPEND LR_RACCT.
      ENDIF.
      REFRESH LT_BALANCE.
    ENDIF.

    L_RACCT = PT_BSPL-RACCT.
    CLEAR LT_BALANCE.
    IF PT_BSPL-PERIOD_SIGN = '1'.
*---> 15/06/2023 - Migração S4 - JS
*     LT_BALANCE-L_BALANCE11 = PT_BSPL-SALDO.
*     LT_BALANCE-L_BALANCE21 = PT_BSPL-SALDO2.
     LT_BALANCE-L_BALANCE11 = CONV #( PT_BSPL-SALDO  ).
     LT_BALANCE-L_BALANCE21 = CONV #( PT_BSPL-SALDO2 ).
*<--- 15/06/2023 - Migração S4 - JS

      LT_BALANCE-RBUKRS     = PT_BSPL-RBUKRS.
      COLLECT LT_BALANCE.
    ELSEIF PT_BSPL-PERIOD_SIGN = '2'.
*---> 15/06/2023 - Migração S4 - JS
*     LT_BALANCE-L_BALANCE11 = PT_BSPL-SALDO.
*     LT_BALANCE-L_BALANCE21 = PT_BSPL-SALDO2.
     LT_BALANCE-L_BALANCE11 = CONV #( PT_BSPL-SALDO  ).
     LT_BALANCE-L_BALANCE21 = CONV #( PT_BSPL-SALDO2 ).
*<--- 15/06/2023 - Migração S4 - JS
      LT_BALANCE-RBUKRS     = PT_BSPL-RBUKRS.
      COLLECT LT_BALANCE.
    ENDIF.

  ENDLOOP.

* handle the last account
  LOOP AT LT_BALANCE WHERE L_BALANCE11 <> 0
                        OR L_BALANCE12 <> 0
                        OR L_BALANCE21 <> 0
                        OR L_BALANCE22 <> 0.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    LR_RACCT-SIGN    = 'I'.
    LR_RACCT-OPTION  = 'EQ'.
    LR_RACCT-LOW     = L_RACCT.
    APPEND LR_RACCT.
  ENDIF.

  IF NOT ( LR_RACCT[] IS INITIAL ).
    DELETE PT_BSPL WHERE RACCT IN LR_RACCT.
  ENDIF.

ENDFORM.                    " BSPL_ACCT_ZERO_BLNCE_DELETE

GET SKA1 LATE.
CHECK: BILABKON = '3'.
DATA: L_BSALD  LIKE GTAB-BSALD,
      L_VSALD  LIKE GTAB-VSALD,
      L_BSALD2 LIKE GTAB-BSALD2,
      L_VSALD2 LIKE GTAB-VSALD2.

DO 2 TIMES.
  CASE SY-INDEX.
    WHEN 1.
*
* Die in der Tabelle GTAB gesammelten Geschaeftsbereich
* werden um Kontonummer, Bezeichnung etc. ergaenzt und
* Zwischendatenbestand geschrieben. (mit EXTRACT).
*
      SORT GTAB.
      LOOP AT GTAB.
        CLEAR SORT.
        CLEAR DATE.
*         restore PRKEY's
        PRKEY-SOLL  = GTAB-PRKYS.
        PRKEY-HABEN = GTAB-PRKYH.
        PRKEY-VERD  = GTAB-VERD.

        IF BILAGKON <> '1'.
          AT NEW SAKNR.
            SUM.
            PERFORM BERECHNE_ERGEBNIS.
            L_BSALD  = GTAB-BSALD.
            L_VSALD  = GTAB-VSALD.
            L_BSALD2 = GTAB-BSALD2.
            L_VSALD2 = GTAB-VSALD2.
          ENDAT.
        ELSE.
          PERFORM BERECHNE_ERGEBNIS.
          L_BSALD = GTAB-BSALD.
          L_VSALD = GTAB-VSALD.
          L_BSALD2 = GTAB-BSALD2.
          L_VSALD2 = GTAB-VSALD2.
        ENDIF.

        IF BILANULL NE 'X'.
          IF FI_LC_EX IS INITIAL.
*       nur Konten mit Saldo <> Null
            CHECK GTAB-BSALD  NE 0
               OR GTAB-VSALD  NE 0
               OR GTAB-BSALD2 NE 0
               OR GTAB-VSALD2 NE 0.
          ENDIF.
        ELSE.
*     auch Konten mit Saldo Null
          IF GTAB-BSALD EQ 0 AND GTAB-VSALD EQ 0 AND GTAB-BSALD2 EQ 0 AND GTAB-VSALD2 EQ 0.
*       Defaultmäßig Konten ohne Löschvormerkung
            IF LOE-VORM NE 'X'.
              CHECK SKA1-XLOEV = SPACE.
            ENDIF.
          ENDIF.
        ENDIF.

*   Merken, ob Konto im Berichts- oder Vergl.zeitraum signifikant war
*
        IF GTAB-BSIGN NE 0.
          H-SIGN = GTAB-BSIGN.
        ENDIF.
        IF GTAB-VSIGN NE 0.
          H-SIGN = GTAB-VSIGN.
        ENDIF.

*   Pruefen, ob Konto zu einer Verdichtungsgruppe gehoehrt
*
        CASE PRKEY-VERD.
          WHEN ' '.                  " <=== dann nicht

*
*     Bilanzschluessel Soll- und Haben sind gleich
*
            IF PRKEY-SOLL = PRKEY-HABEN.
              PERFORM EXTRACT_SOLL_POSITION.
              EXTRACT KONTEN.
            ELSE.

*
*     Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
*
*       Berichts- und Vergleichssaldo sind positiv
*
              IF L_BSALD GE 0 AND L_VSALD GE 0.
                PERFORM EXTRACT_SOLL_POSITION.
                EXTRACT KONTEN.
              ENDIF.

*       Berichts- und Vergleichssaldo sind negativ
*
              IF L_BSALD LE 0 AND L_VSALD LE 0.
                PERFORM EXTRACT_HABEN_POSITION.
                EXTRACT KONTEN.
              ENDIF.

*       Berichtssaldo ist negativ und Vergleichssaldo ist positiv
*
              IF L_BSALD < 0 AND L_VSALD > 0.
                PERFORM EXTRACT_SOLL_POSITION.
                MOVE: '('        TO DATE-BKLAU,
                      ')'        TO DATE-BKLZU.
                EXTRACT KONTEN.
                CLEAR SORT.
                CLEAR DATE.

                PERFORM EXTRACT_HABEN_POSITION.
                MOVE: '('        TO DATE-VKLAU,
                      ')'        TO DATE-VKLZU.
                EXTRACT KONTEN.
              ENDIF.

*       Berichtssaldo ist positiv und Vergleichssaldo ist negativ
*
              IF L_BSALD > 0 AND L_VSALD < 0.
                PERFORM EXTRACT_SOLL_POSITION.
                MOVE: '('        TO DATE-VKLAU,
                      ')'        TO DATE-VKLZU.
                EXTRACT KONTEN.
                CLEAR SORT.
                CLEAR DATE.

                PERFORM EXTRACT_HABEN_POSITION.
                MOVE: '('        TO DATE-BKLAU,
                      ')'        TO DATE-BKLZU.
                EXTRACT KONTEN.
              ENDIF.
            ENDIF.
          WHEN OTHERS.

*
*     Bilanzschluessel Soll- und Haben sind gleich
*
            IF PRKEY-SOLL = PRKEY-HABEN.
              PERFORM EXTRACT_SOLL_POSITION.
              EXTRACT KONTEN.
            ELSE.
*
*     Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
*
              PERFORM EXTRACT_SOLL_POSITION.
              EXTRACT KONTEN.
              CLEAR SORT.
              CLEAR DATE.
              PERFORM EXTRACT_HABEN_POSITION.
              EXTRACT KONTEN.
            ENDIF.
        ENDCASE.
      ENDLOOP.

*EJECT
    WHEN 2.
* -------------------------------------------------------------------
* Hier gehts nur weiter, wenn Konten mit Saldo = 0 gewuenscht werden,
* fuer die aber keine GLDB-Saetze vorhanden sind.
* -------------------------------------------------------------------

* Saldo = 0 erwuenscht ??
      CHECK BILANULL = 'X'.

* Defaultmäßig Konten ohne Löschvormerkung
      IF LOE-VORM NE 'X'.
        CHECK SKA1-XLOEV = SPACE.
      ENDIF.

* Konto darf nicht signifikant sein, keine GLDB-Saetze haben.
      CHECK H-SIGN =  0.

* GL Account must be well known in at least one Ccode
      CHECK NOT ( PRKEY IS INITIAL ).

* Felder fuer die Extract-Routine vorbereiten
      CLEAR: GTAB,
             SORT,
             DATE.

* Gesber kennzeichnen, dass keine GLDB-Saetze vorhanden sind
      MOVE '****' TO GTAB-GSBER.

      IF REPWAERS IS NOT INITIAL.                           "n1397380
        CTYP_WAERS   = REPWAERS.                            "n1397380
        CTYP_WAERS2  = REPWAERS.                            "n1397380
      ENDIF.                                                "n1397380

      IF CTYP_WAERS IS INITIAL.
        PERFORM CURRENCY_INFORMATION_GET
                              CHANGING CTYP_WAERS.
      ENDIF.

      IF CTYP_WAERS2 IS INITIAL.
        PERFORM CURRENCY_INFORMATION_GET2
                              CHANGING CTYP_WAERS2.
      ENDIF.

      IF BILAGRID = 'X'
      OR BILATREE = 'X'.
*.. prepare fields for dummy records
        CLEAR: H-SALDO.
*          skc1a-gsber = '****'.                            "n1399773
        ZSKC1A-GSBER = SPACE.                               "n1399773
        ZSKC1A-CURTP = SD_CURTP.
        ZSKC1A-RLDNR = G_RLDNR.
        IF SD_CURTP IS INITIAL.
          ZSKC1A-CURTP = '10'.
        ENDIF.
        IF SD_CURT2 IS INITIAL.
          ZSKC1A-CURT2 = '10'.
        ENDIF.
*.. fill table for ALV Grid- and ALV Tree-Control
*         PERFORM gt_bspl_fill USING 'B'.                   "n1399773
        CONTINUE.
      ENDIF.

* --------------------------------------------
* Bilanzschluessel Soll- und Haben sind gleich
* --------------------------------------------
      IF PRKEY-SOLL = PRKEY-HABEN.
        PERFORM EXTRACT_SOLL_POSITION.
        EXTRACT KONTEN.
      ELSE.
* --------------------------------------------------------
* Bilanzschluessel Soll- und Haben sind  n i c h t  gleich
* --------------------------------------------------------
        PERFORM EXTRACT_SOLL_POSITION.
        EXTRACT KONTEN.

        PERFORM EXTRACT_HABEN_POSITION.
        EXTRACT KONTEN.
      ENDIF.

* Aufnahme in Verdichtungstabelle falls erforderlich
* --------------------------------------------------
      IF PRKEY-VERD = 'X'.
        CLEAR VTAB.
        MOVE: SORTBKRS      TO VTAB-BUKRS,
              PRKEY-SOLL    TO VTAB-PRKYS,
              PRKEY-HABEN   TO VTAB-PRKYH,
              PRKEY-VERD    TO VTAB-VERDS,
                '****'      TO VTAB-GSBER,
              CTYP_WAERS    TO VTAB-WAERS,
              CTYP_WAERS2   TO VTAB-WAER2.

        READ TABLE VTAB.

*   Berichts-Periode signifikant machen, da sonst kein Triggersatz
*   extrahiert wird und demzufolge die nachfolgenden Kontensaetze
*   ebenfalls uebergangen werden.
*   --------------------------------------------------------------
        IF SY-SUBRC = 0.
          MOVE:   1         TO VTAB-BSIGN,
                  1         TO VTAB-VSIGN,
                  1         TO VTAB-BSIGN2,
                  1         TO VTAB-VSIGN2.
          MODIFY VTAB INDEX SY-TABIX.
        ELSE.
          MOVE:   1         TO VTAB-BSIGN,
                  1         TO VTAB-VSIGN,
                  1         TO VTAB-BSIGN2,
                  1         TO VTAB-VSIGN2.
          COLLECT VTAB.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDDO.

*EJECT

* Pruefen Berichtsperioden
* ------------------------
*&---------------------------------------------------------------------*
*&      Form  check_output_options
*&---------------------------------------------------------------------*
FORM CHECK_OUTPUT_OPTIONS .       "new since note 1060564
*----------------------------------------------------------------------*
  DATA LD_MSGTY                 TYPE MSGTS.                 "n1360969
*----------------------------------------------------------------------*
  DEFINE MSG_ADD.
    IF 1 = 2.
      MESSAGE E098(FR) WITH SPACE SPACE SPACE SPACE.
    ENDIF.
    IF LD_MSGTY <> '-'.                                     "n1360969
      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          ARBGB = 'FR'
          MSGTY = 'W'
          MSGV1 = SY-MSGV1
          MSGV2 = SY-MSGV2
          MSGV3 = &1
          MSGV4 = SY-MSGV4
          TXTNR = 098.
    ENDIF.                                                  "n1360969
  END-OF-DEFINITION. "msg_add
*----------------------------------------------------------------------*

* begin "n1360969
  CALL FUNCTION 'READ_CUSTOMIZED_MESSAGE'
    EXPORTING
      I_ARBGB = 'FR'
      I_DTYPE = 'W'
      I_MSGNR = '098'
    IMPORTING
      E_MSGTY = LD_MSGTY.
* end "n1360969

* begin "n1486648
  CALL FUNCTION 'MESSAGES_INITIALIZE'.

*  IF BILALIST IS NOT INITIAL.
*    SY-TABIX = BILASUMM.
*    IF SY-TABIX > 9.
*      BILASUMM = 9.
*      PERFORM GET_TEXTPOOL
*        USING
*          'BILALIST'
*        CHANGING
*          SY-MSGV1.
*      PERFORM GET_TEXTPOOL
*        USING
*          'BILASUMM'
*        CHANGING
*          SY-MSGV2.
*      MSG_ADD BILASUMM.
*    ENDIF.
*  ENDIF.
* end "n1486648

* Processing only necessary if ALV Grid or ALV Tree is selected
  IF BILAGRID <> SPACE
  OR BILATREE <> SPACE.

* Get text for ALV Grid
    IF BILAGRID = ABAP_TRUE.
      PERFORM GET_TEXTPOOL
        USING
          'BILAGRID'
        CHANGING
          SY-MSGV1.
* Get text for ALV Tree
    ELSE.
      PERFORM GET_TEXTPOOL
        USING
          'BILATREE'
        CHANGING
          SY-MSGV1.
    ENDIF.
* Get text for Classical List
    PERFORM GET_TEXTPOOL
      USING
        'BILALIST'
      CHANGING
        SY-MSGV4.

* Check Company code summarization
    IF BILABKON <> '3'.
      BILABKON = '3'.
      PERFORM GET_TEXTPOOL
        USING
          'BILABKON'
        CHANGING
          SY-MSGV2.
      MSG_ADD BILABKON.
    ENDIF.

* For ALV Grid
* For ALV Tree
* Check Business area summarization
    IF BILAGRID <> SPACE
    OR BILATREE <> SPACE.
      IF BILAGKON <> '3'.
        BILAGKON = '3'.
        PERFORM GET_TEXTPOOL
          USING
            'BILAGKON'
          CHANGING
            SY-MSGV2.
        MSG_ADD BILAGKON.
      ENDIF.
    ENDIF.

* Check Scaling
    IF BILASKAL <> '0/0'.
      BILASKAL = '0/0'.
      PERFORM GET_TEXTPOOL
        USING
          'BILASKAL'
        CHANGING
          SY-MSGV2.
      MSG_ADD BILASKAL.
    ENDIF.

* Check List Separation
    IF ALLGLSEP <> SPACE.
      ALLGLSEP = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'ALLGLSEP'
        CHANGING
          SY-MSGV2.
      MSG_ADD ALLGLSEP.
    ENDIF.

* Check Microfiche Line
    IF MIKFICHE <> SPACE.
      MIKFICHE = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'MIKFICHE'
        CHANGING
          SY-MSGV2.
      MSG_ADD ALLGLSEP.
    ENDIF.

* begin "n1486648
* Summary Report
*  IF bilasumm <> space.
*    bilasumm = space.
*    PERFORM get_textpool
*      USING
*        'BILASUMM'
*      CHANGING
*        sy-msgv2.
*    msg_add bilasumm.
*  ENDIF.
* end "n1486648

* Extrakt to Consolidation
    IF FI_LC_EX <> SPACE.
      FI_LC_EX = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'FI_LC_EX'
        CHANGING
          SY-MSGV2.
      MSG_ADD FI_LC_EX.
    ENDIF.

* Extrakt to EIS
    IF BILA_EIS <> SPACE.
      BILA_EIS = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'BILA_EIS'
        CHANGING
          SY-MSGV2.
      MSG_ADD BILA_EIS.
    ENDIF.

* Start Page Numbering
    IF BILAPAGE <> SPACE.
      BILAPAGE = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'BILAPAGE'
        CHANGING
          SY-MSGV2.
      MSG_ADD BILAPAGE.
    ENDIF.

* Print on Form
    IF SCR_FORM <> SPACE.
      SCR_FORM = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'SCR_FORM'
        CHANGING
          SY-MSGV2.
      MSG_ADD SCR_FORM.
    ENDIF.

* Print on Printer
    IF SCR_DEVI <> SPACE.
      SCR_DEVI = SPACE.
      PERFORM GET_TEXTPOOL
        USING
          'SCR_DEVI'
        CHANGING
          SY-MSGV2.
      MSG_ADD SCR_DEVI.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      SHOW_LINNO = ABAP_FALSE
    EXCEPTIONS
      OTHERS     = 0.

ENDFORM.                    " check_output_options
*&---------------------------------------------------------------------*
*&      Form  get_textpool
*&---------------------------------------------------------------------*
FORM GET_TEXTPOOL                 "new since note 1060564
  USING
    ID_KEY         TYPE TEXTPOOLKY
  CHANGING
    CD_MSGV        TYPE SYMSGV.
*----------------------------------------------------------------------*
  STATICS ST_TEXTPOOL           TYPE STANDARD TABLE OF TEXTPOOL.
*----------------------------------------------------------------------*
  DATA    LS_TEXTPOOL           TYPE TEXTPOOL.
*----------------------------------------------------------------------*
  IF ST_TEXTPOOL IS INITIAL.
    READ TEXTPOOL SY-REPID INTO ST_TEXTPOOL LANGUAGE SY-LANGU.
    DELETE ST_TEXTPOOL WHERE ID <> 'S'.
  ENDIF.

  READ TABLE ST_TEXTPOOL
    INTO LS_TEXTPOOL
    WITH KEY
      KEY = ID_KEY
    BINARY SEARCH.
  IF SY-SUBRC = 0.
    CD_MSGV = LS_TEXTPOOL-ENTRY+8.
  ELSE.
    CD_MSGV = ID_KEY.
  ENDIF.


ENDFORM.                    " get_textpool

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR PLANVERS.          "n1434376
*  PERFORM VALUE_REQUEST_PLANVERS.                           "n1434376

*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST_PLANVERS
*&---------------------------------------------------------------------*
FORM VALUE_REQUEST_PLANVERS . "new since "n1434376
*----------------------------------------------------------------------*
  DATA LT_DYNPFIELDS          TYPE STANDARD TABLE OF DYNPREAD.
  DATA LS_DYNPFIELDS          TYPE DYNPREAD.
  DATA LD_GLFLEX_ACTIVE       TYPE BOOLE_D.
*----------------------------------------------------------------------*
  CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
    IMPORTING
      E_GLFLEX_ACTIVE = LD_GLFLEX_ACTIVE
    EXCEPTIONS
      OTHERS          = 0.

  IF LD_GLFLEX_ACTIVE IS NOT INITIAL.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME             = 'ZRFBILA01'
        DYNUMB             = '1000'
        TRANSLATE_TO_UPPER = 'X'
        REQUEST            = 'A'
      TABLES
        DYNPFIELDS         = LT_DYNPFIELDS.
    READ TABLE LT_DYNPFIELDS
      INTO LS_DYNPFIELDS
      WITH KEY
        FIELDNAME = 'SD_RLDNR-LOW'.
    IF SY-SUBRC = 0.
      ZSKC1A-RLDNR = LS_DYNPFIELDS-FIELDVALUE.
    ELSE.

      ZSKC1A-RLDNR = SD_RLDNR-LOW.
    ENDIF.
  ELSE.
    ZSKC1A-RLDNR = '0'.
  ENDIF.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      TABNAME     = 'ZSKC1A'
      FIELDNAME   = 'RVERS'
      DYNPPROG    = 'ZRFBILA01'
      DYNPNR      = '1'
      DYNPROFIELD = 'PLANVERS'.

ENDFORM.                    " VALUE_REQUEST_PLANVERS

FORM BSPL_ACCT_ZERO_DELETE TABLES PT_BSPL TYPE TT_BSPL.

  LOOP AT PT_BSPL INTO DATA(WA).

    IF GT_BSPL-RACCT IS INITIAL.
      CONTINUE.
    ENDIF.

    IF WA-PER01 IS INITIAL AND WA-PER02 IS INITIAL AND
       WA-PER03 IS INITIAL AND WA-PER04 IS INITIAL AND
       WA-PER05 IS INITIAL AND WA-PER06 IS INITIAL AND
       WA-PER07 IS INITIAL AND WA-PER08 IS INITIAL AND
       WA-PER09 IS INITIAL AND WA-PER10 IS INITIAL AND
       WA-PER11 IS INITIAL AND WA-PER12 IS INITIAL AND
       WA-PER13 IS INITIAL AND WA-PER14 IS INITIAL AND
       WA-PER15 IS INITIAL AND WA-PER16 IS INITIAL.
      DELETE PT_BSPL INDEX SY-TABIX.
    ENDIF.

  ENDLOOP.

ENDFORM.

REPORT SAPFGVTR MESSAGE-ID GU LINE-SIZE 132
       NO STANDARD PAGE HEADING.

*Type pools
TYPE-POOLS: GD13.

*Tables
TABLES: SKA1,                          " Sachkonto-Kontenplan
        SKB1,                          " Sachkonto-Buchungskreis
        GLU1,
        *GLU1.

TABLES: TRDIR,                         " Report-Verzeichnis (RGIMVxxx)
        T001,                          " Buchungskreise
        T004,                          " Konzernkontenpläne
        T030,                          " Kontenfindung lokal  (BUKRS)
        T030C,                         " Kontenfindung global (GESNR)
        T800A,                         " DB-Tabellen ->lokal/global
        T858,                          " Kons-Versionen
        T894,                          " GLX-Versionen
        T880,                          " Gesellschaft
        T881,                          " Ledger
        T882,                          " Buchungskreis-Ledger ->KTOPL
        T882C,                         " Gesellschaft-Ledger  ->KTOPL
*       T884C,                         " SAVOR-Kontierung ->Feld-Modif
        T800M,                         " feste    Feld-Modifs
        T888M,                         " variable Feld-Modifs
        T888G,                         " FMODs für feste ledger GLFLEXT
        DFIES.                         " GET_FIELDS

*Selection parameters
SELECTION-SCREEN BEGIN OF BLOCK PARAM WITH FRAME TITLE TEXT-300.
PARAMETER:
    LEDGER    LIKE GLU1-RLDNR.         "MEMORY ID GLN.    " Ledger
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
         BUKRS     FOR GLU1-BUKRS MEMORY ID BUK.     " Buchungskreis
SELECTION-SCREEN COMMENT /1(4) TEXT-203 MODIF ID GL.
SELECT-OPTIONS:
         RCOMP     FOR GLU1-RCOMP MEMORY ID GCC MODIF ID GL.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
         SATZTYP   FOR GLU1-RRCTY DEFAULT 0 MODIF ID GL,
         VERSION   FOR GLU1-RVERS DEFAULT '001' MEMORY ID GVS
                   MODIF ID GL.
SELECTION-SCREEN SKIP 1.
PARAMETER:
    NEWJR     LIKE GLU1-RYEAR          "in Geschäftsjahr vortragen
              MEMORY    ID GJR OBLIGATORY.
SELECTION-SCREEN END OF BLOCK PARAM.

SELECTION-SCREEN BEGIN OF BLOCK PROC WITH FRAME TITLE TEXT-301.
PARAMETER:  XTEST     DEFAULT 'X' AS CHECKBOX. "Test/ Echtlauf (update)
PARAMETER:  XACCT     DEFAULT ' ' MODIF ID GL AS CHECKBOX.
*Parameter:  XACCT     DEFAULT 'X' modif id GL no-display.
SELECTION-SCREEN END OF BLOCK PROC.

SELECTION-SCREEN BEGIN OF BLOCK LIST WITH FRAME TITLE TEXT-302.
PARAMETER:  XLIST     DEFAULT 'X' AS CHECKBOX.      "Listausgabe
PARAMETER:  XTRAC LIKE RFPDO1-F011TRAC AS CHECKBOX. "Trace accounts
SELECTION-SCREEN BEGIN OF BLOCK ADD_FIELDS WITH FRAME TITLE TEXT-303.
PARAMETER:  P_FIELD1  LIKE GLXCOMP-COMP_FIELD MODIF ID GLF.
PARAMETER:  P_FIELD2  LIKE GLXCOMP-COMP_FIELD MODIF ID GLF.
PARAMETER:  P_FIELD3  LIKE GLXCOMP-COMP_FIELD MODIF ID GLF.
SELECTION-SCREEN END OF BLOCK ADD_FIELDS.
SELECTION-SCREEN END OF BLOCK LIST.
PARAMETERS: P_APPL(5) NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK B9 WITH FRAME TITLE TEXT-999.
PARAMETERS: P_KOSTL LIKE BSEG-KOSTL MATCHCODE OBJECT KOST OBLIGATORY,
            P_PRCTR LIKE BSEG-PRCTR OBLIGATORY,
            P_BEWAR LIKE BSEG-BEWAR OBLIGATORY,
            P_AUFNR LIKE BSEG-AUFNR OBLIGATORY,
            P_BLART LIKE BKPF-BLART DEFAULT 'XS' OBLIGATORY,
            P_BLDAT LIKE BKPF-BLDAT DEFAULT SY-DATUM OBLIGATORY,
            P_MONAT LIKE BKPF-MONAT OBLIGATORY,
            P_SGTXT LIKE BSEG-SGTXT.
SELECTION-SCREEN END OF BLOCK B9.
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S02.
PARAMETERS: P_VARIA LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B0.

*Generated data
*INCLUDE fgvtrdit.                      "gen. mandantenabh.int.Tabellen
INCLUDE ZFGVTRDIT.
*INCLUDE lglinta9.                      "alle EP-Tabellen ohne GLU1; gen.
INCLUDE ZLGLINTA9.

*Type pools for selection and list display
TYPE-POOLS: GUSL, SLIS.

* ------ Fehlerliste: fehlende Vortrags-Bewar -------------------------
DATA: BEGIN OF IT_MESG  OCCURS 1.
        INCLUDE STRUCTURE MESG.
DATA: END   OF IT_MESG.

DATA: BEGIN OF MESG_MOV OCCURS 10,
        BUKGES   LIKE T880-RCOMP,      " gesnr/Bukrs
        RACCT    LIKE GLU1-RACCT,      " Sachkonto
        TEXT     LIKE MESG-TEXT,       " feli-text
END OF MESG_MOV.

DATA: LIN_IT_MESG  TYPE P,             "akt.Anzahl in IT_MESG
      ZLIN_IT_MESG TYPE P.             "zwsp.akt.Anzahl in IT_MESG
DATA: XERR_MOV(1).                     "X: Fehler bei Movements

*-----------------------------------------------------------------------
*        Interne Tabellen
*-----------------------------------------------------------------------

* ------ Übergabe-Parameter an G_GLDB_POSTING --------------------------
DATA: BEGIN OF TAB_ACTIVITY OCCURS 1,
        ACTIVITY(4),
      END OF TAB_ACTIVITY.

DATA: BEGIN OF USED_V1  OCCURS 1.
        INCLUDE STRUCTURE RGIUSE.
DATA: END   OF USED_V1.

* ------ Übergabe-Parameter an G_READ_VALUE / G_VALIDATE_RECORD --------
DATA: BEGIN OF PERIOD OCCURS 1.
        INCLUDE STRUCTURE PERIODS.
DATA: END OF PERIOD.
DATA: MD_KEY   LIKE RGUMD-RECORD_KEY,
      MD_VALUE LIKE RGUMD-VALUE.

* ------ Kontoplan Gesellschaft-Ledger --------------------------------
*ATA:   BEGIN OF GESTAB OCCURS 20,
*         RCOMP      LIKE T882C-RCOMP,  " Gesellschaft
*         RLDNR      LIKE T882C-RLDNR,  " Ledger
*         KTOPL      LIKE T882C-KTOPL,  " Kontenplan
*       END   OF GESTAB.

* ------ Alle BUKRS/GESNR mit Zusatzinformationen ---------------------
DATA:   BEGIN OF BUKGESTAB OCCURS 100,
          BUKGES     LIKE T880-RCOMP,  " Buchungskreis/Gesellschaft
          LCCUR      LIKE T881-LCCUR,  " KZ HWAER (T881+T882/C)
          HWAER      LIKE T880-CURR,   " Hauswährung Bukrs/Gesnr
          RCCUR      LIKE T881-RCCUR,  " KZ KWAER (T881+T882/C)
          KWAER      LIKE T880-CURR,   " Konz.währung Bukrs/Gesnr
          OCCUR      LIKE T881-OCCUR,  " KZ OWAER (T881+T882/C)
          OWAER      LIKE T880-CURR,   " And.Währung Bukrs/Gesnr
          KTOPL      LIKE T001-KTOPL,  " aktueller Ledger-Ktopl (T881)
          KTOPB      LIKE T001-KTOPL,  " Bukrs- oder eigener Ktopl
          VTRHJ      LIKE T882-VTRHJ,  " höchstes vorgetragenes GJ
          ALTSV      LIKE T882-ALTSV,  " KZ: alternativer Ktopl.
          PERIV      LIKE T882-PERIV,
          UPDATE(1),                   " KZ:Daten für BUKGES vorgetragen
        END   OF BUKGESTAB.
DATA:   LIN_BUKGESTAB    TYPE I.

* ------ pro Bukrs in SKA1 vorhandene GVTYP'en und T030-Einträge ------
DATA: BEGIN OF GVTYP_BUKRS       OCCURS 0,
        BUKRS LIKE SKB1-BUKRS,
        KTOPB LIKE SKA1-KTOPL,
        GVTYP LIKE SKA1-GVTYP,
        XERROR(1).
        INCLUDE STRUCTURE T030.
DATA: END OF GVTYP_BUKRS.

* ------ für alle Ktopl die relevanten GVTYP's ------------------------
DATA:   GVTYP_T030 LIKE T030 OCCURS 10 WITH HEADER LINE.

* ------ von allen Ktopl die Sachkonten SKA1 --------------------------
DATA: BEGIN OF INT_SKA1          OCCURS 0,
        KTOPL LIKE SKA1-KTOPL,
        SAKNR LIKE SKA1-SAKNR,
        GVTYP LIKE SKA1-GVTYP,
      END OF INT_SKA1.

* ------ Vortragskonten für GuV-Konten, um dopp.update zu sparen ------
DATA:   BEGIN OF GUVTAB OCCURS 5,
          KTOPL      LIKE T030-KTOPL,  " Kontenplan
          GVTYP      LIKE SKA1-GVTYP,  " Gewinnverwendungstyp
          VTKON      LIKE T030-KONTS,  " Vortragskonto
        END   OF GUVTAB.
DATA:   Z_KTOPL      LIKE T030-KTOPL.  " akt.SKA1-Ktopl.für MITTAB-Eintr

* ------ Vortragskonten für Bukrs-Ledger ohne TWAER vortragen (XSALH='X'
DATA:   BEGIN OF VTWTAB   OCCURS 5,
          BUKRS      LIKE SKB1-BUKRS,  " Bukrs
          VTKON      LIKE T030-KONTS,  " Vortragskonto
        END   OF VTWTAB.

* ------ Fehlende Tabelleneinträge oder undefinierte Konten
DATA:   BEGIN OF MITTAB OCCURS 10,
          RACCT      LIKE GLU1-RACCT,  " Sachkonto
          BUKGES     LIKE T880-RCOMP,  " Bukrs/Gesnr
          KTOPL      LIKE SKA1-KTOPL,  " Kontenplan
          GVTYP      LIKE SKA1-GVTYP,  " GVTYP
          TEXT1(50)  TYPE C,           " Erläuterung 1
          TEXT2(30)  TYPE C,           " Erläuterung 2
        END   OF MITTAB.

* ------ ZWSP. der Feldmodif-Namen aus Tab.884C -----------------------
DATA:   BEGIN OF FMOD,
          GUV   LIKE T884C-FELDMODIF,
          BIL   LIKE T884C-FELDMODIF,
          AKT   LIKE T884C-FELDMODIF,  "aktuelle
        END OF FMOD.

* ------ Tab-Name fuer konkreten COLLECT (int. Tab SV?TAB_fmod) -------
DATA:   SVGTAB_NAME(11),               "für G+V
        SVBTAB_NAME(11).               "für Bilanz

* ------ Tab. LED_PAR (LEDGER aus LGLINTOP) für UPDATE_TABLES --------
*INCLUDE FGLIND01.
DATA: BEGIN OF LED_PAR.
        INCLUDE STRUCTURE GLEDGER.
DATA: END   OF LED_PAR.

*------ List of record types
TYPES: BEGIN OF S_RRCTY,
        RRCTY TYPE RRCTY,
       END OF S_RRCTY,
       T_RRCTY TYPE STANDARD TABLE OF S_RRCTY.

*------ List of versions
TYPES: T_VERSION_DATA TYPE STANDARD TABLE OF GLX_VERSION_INFO.

*-----------------------------------------------------------------------
*        Strukturen (BEGIN OF)
*-----------------------------------------------------------------------

* ------ Fehler bei der Prüfung der Buchungskreise auf Geschäftsjahr ---
DATA:   BEGIN OF ERROR,
          BUKRS      LIKE T001-BUKRS,
          GJAHR      LIKE BKPF-GJAHR,
        END   OF ERROR.

* ------ Arbeitsfelder ------------------------------------------------
DATA:   BEGIN OF WF,
          RC         LIKE SY-SUBRC,    " Returncode
          FORM(20)   TYPE C,           " Form variabler Felduebertrag
          PROGRAM(8) TYPE C,           " Programm-Name mit Mandant
          LCCUR LIKE T881-LCCUR,       " max. HW über alle Bukrs/Gesnr
          RCCUR LIKE T881-RCCUR.       " max. KW über alle Bukrs/Gesnr
        INCLUDE STRUCTURE RGVALUE.     "WERTV13, MENGE13, ...
DATA:   END   OF WF.

*-----------------------------------------------------------------------
*        Daten
*-----------------------------------------------------------------------

DATA:   CNTCW      TYPE I,             " Zähler für COMMIT WORK
        GJAHR      LIKE BKPF-GJAHR,    " Geschäftsjahr
        MAXCW      TYPE I              " Maximalwert von CNTCW
          VALUE 10000,
        MONAT      LIKE BKPF-MONAT,    " Monat
        OLDJR      LIKE GLU1-RYEAR,    " Altes Geschäftsjahr
        XCOMPTAB   LIKE T800A-COMPTAB, " X=Gesnr-, ' '=Bukrs-Ledger
        PRTYP(1)   TYPE C,             " Protokolltyp (S=SUC, E=ERR)
        XAPLSTAT(1) TYPE C VALUE '0',  " Kennz.: LD_APLSTAT für Schedman
        XCHNG(1)   TYPE C,             " Kennz.: Änderungen?
        XEXIS(1)   TYPE C,             " Kennz.: GL??/NEWJR existiert?
        XKONS(1)   TYPE C,             " Kennz.: Konsistenz o.k?
        XKONT(1)   TYPE C.             " Kennz.: Konten zu verarbeiten?

DATA:   CURS TYPE CURSOR,              " Performance
        COUNT_DELETE LIKE SY-TABIX,
        COUNT_DELETE_MAX LIKE SY-TABIX VALUE 10000.

*------selbst def. Daten-----------------------------------------------
*------Key-Felder aus angegebenen Parametern --------------------------

DATA: BEGIN OF KEY_FIELDS OCCURS 20,   "key-felder
        FIELD(10),                     "feld-name
        FROM_VALUE(24) TYPE C,         "von-wert
        TO_VALUE(24) TYPE C,           "bis-wert
      END OF KEY_FIELDS.

*------ Tabelle mit allen upgedateden logischen Datenbaken ------------
DATA: BEGIN OF USED  OCCURS 10.
        INCLUDE STRUCTURE RGIUSE.
DATA: END   OF USED.

*----- ZWSP. fuer GL-Felder -------------------------------------------
DATA: BEGIN OF AKT,
        RACCT  LIKE GLU1-RACCT,
        BUKRS  LIKE GLU1-BUKRS,        "Zusammengeführt: BUKRS/RBUKRS
        RCOMP  LIKE GLU1-RCOMP,
        BUKGES LIKE GLU1-RCOMP,
        RYEAR  LIKE GLU1-RYEAR,
      END OF AKT.

DATA: BEGIN OF ZFMOD,                  "FMOD aus T888G zusammenstellen
       RLDNR     LIKE T888G-RLDNR,
       INTERFACE LIKE T888G-INTERFACE VALUE '05',
      END   OF ZFMOD.

*----- allgemeine Felder / Kennzeichen --------------------------------
DATA: XBILK LIKE T884C-XBILK,          "X: Bilanzkonto
      XTW,                             "X: Mit T-Währung vortragen
      DRCRK LIKE GLU1-DRCRK,           "ermitteltes SH-KZ
      USEDT LIKE T800A-TAB.            "für Loeschen_periode_null

DATA: XGLU1  LIKE BOOLE.               "X: GLU1 ist gefüllt (Standard)


*Global Ledger and table information
DATA: H_800A LIKE T800A.
DATA: H_881  LIKE T881.
DATA: T_T800A_KEYFIG LIKE T800A_KEYFIG OCCURS 0 WITH HEADER LINE.
DATA: T_T881_KEYFIG LIKE T881_KEYFIG OCCURS 0 WITH HEADER LINE.
*General field symbols for Orgunit and Account
FIELD-SYMBOLS: <GLU1_ORGUNIT>, <GLU1_ACCOUNT>,
               <*GLU1_ORGUNIT>, <*GLU1_ACCOUNT>.
*Field symbols for list display
FIELD-SYMBOLS: <S_LIST_GLU1_ORGUNIT>,
               <S_LIST_GLU1_ACCOUNT>,
               <S_LIST_GLU1_FIELD1>,
               <S_LIST_GLU1_FIELD2>,
               <S_LIST_GLU1_FIELD3>,
               <S_LIST_GLU1_KEYFIG01>,
               <S_LIST_GLU1_KEYFIG02>,
               <S_LIST_GLU1_KEYFIG03>,
               <S_LIST_GLU1_KEYFIG04>,
               <S_LIST_GLU1_KEYFIG05>,
               <S_LIST_GLU1_KEYFIG06>,
               <S_LIST_GLU1_KEYFIG07>,
               <S_LIST_GLU1_KEYFIG08>,
               <S_LIST_GLU1_KEYFIG09>,
               <S_LIST_GLU1_KEYFIG10>.
FIELD-SYMBOLS: <GLU1_FIELD1>,
               <GLU1_FIELD2>,
               <GLU1_FIELD3>,
               <GLU1_KEYFIG01>,
               <GLU1_KEYFIG02>,
               <GLU1_KEYFIG03>,
               <GLU1_KEYFIG04>,
               <GLU1_KEYFIG05>,
               <GLU1_KEYFIG06>,
               <GLU1_KEYFIG07>,
               <GLU1_KEYFIG08>,
               <GLU1_KEYFIG09>,
               <GLU1_KEYFIG10>.

*Counter for updates
DATA  COMMIT_COUNT TYPE I .
*Constants for selection and commit
DATA:  C_MAX_COMMIT TYPE I VALUE 1000,
       C_PACKAGE_SIZE_DELETE TYPE I VALUE 1000,
       C_PACKAGE_SIZE_SELECT TYPE I VALUE 1000.
*Ranges for accounts for select
DATA: BEGIN OF T_ACCOUNTS OCCURS 0,
        SAKNR LIKE SKA1-SAKNR,
      END OF T_ACCOUNTS.
DATA: BEGIN OF T_ACCOUNT_RANGE OCCURS 0,
        MIN LIKE SKA1-SAKNR,
        MAX LIKE SKA1-SAKNR,
      END OF T_ACCOUNT_RANGE.
DATA: C_MAX_ACCOUNTS TYPE I VALUE 100.
DATA: C_PROCESS_ACCOUNT_RANGES.
DATA: H_SELECT_FLAG.
DATA: H_ACCOUNT_RANGE_INDEX TYPE I.
*List tables
DATA: C_LIST_MODE.      "0: no list, 1: summarized, 2: full detail
DATA: S_LIST_GLU1 TYPE GD13_S_GLU1_CUM.

*>>>Cris - Inclusão do campo MARK

TYPES: BEGIN OF TY_ZGL011.
        INCLUDE STRUCTURE GLU1.
TYPES: TSL_CUM LIKE GLU1_XSL_CUM-TSL_CUM,
       HSL_CUM LIKE GLU1_XSL_CUM-HSL_CUM,
       KSL_CUM LIKE GLU1_XSL_CUM-KSL_CUM,
       OSL_CUM LIKE GLU1_XSL_CUM-OSL_CUM,
       MSL_CUM LIKE GLU1_XSL_CUM-MSL_CUM,
       ASL_CUM LIKE GLU1_XSL_CUM-ASL_CUM,
       AR_DATA LIKE GLU1_XSL_CUM-AR_DATA,
*
       TEXT_RACCT LIKE T831A-LTEXT,
       TEXT_RFAREA LIKE T831A-LTEXT,
       TEXT_RBUSA LIKE T831A-LTEXT,
       TEXT_RCNTR LIKE T831A-LTEXT,
       TEXT_PRCTR LIKE T831A-LTEXT,
*
       ORG_ACCOUNT LIKE GLU1_XSL_CUM-ORG_ACCOUNT,
       MARK,
       END OF TY_ZGL011.

DATA: T_LIST_GLU1 TYPE STANDARD TABLE OF TY_ZGL011,
      WA_LIST_GLU1 LIKE LINE OF T_LIST_GLU1.
DATA: TI_CSKB TYPE STANDARD TABLE OF CSKB,
      WA_CSKB LIKE LINE OF TI_CSKB,
      TI_SKB1 TYPE STANDARD TABLE OF SKB1,
      WA_SKB1 LIKE LINE OF TI_SKB1.
DATA: V_CHAVE(2) TYPE C,
    V_CONTR(2) TYPE C.
**********Declarações Novo ALV*****************************
*Pool de declarações do ALV.
TYPE-POOLS: KKBLO.

*Estrutura
DATA: ST_SELFIELD TYPE KKBLO_SELFIELD.

*Variáveis do AVL
DATA: V_REPID       LIKE SY-REPID,
      V_SAVE        TYPE C.

*Works Area do AVL
DATA: WA_VARIANT    TYPE DISVARIANT,
      WA_AFIELD     TYPE KKBLO_FIELDCAT,
      WA_COLINFO    TYPE KKBLO_SPECIALCOL,
      WA_LAYOUT     TYPE KKBLO_LAYOUT,
      WA_HEADER     TYPE KKBLO_LISTHEADER,
      WA_LAY        TYPE SLIS_LAYOUT_ALV.

*Tabelas internas do ALV.
DATA: IT_FCAT       TYPE SLIS_T_FIELDCAT_ALV,
      IT_FIELDCAT   TYPE KKBLO_T_FIELDCAT,
      IT_HEADER     TYPE KKBLO_T_LISTHEADER,
      IT_SORT       TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

**********Declarações Batch_input**********************************
* Recebe os dados configurados de ERRO do Batch Input + campos chaves.
TYPES: BEGIN OF TY_LOG,
       RYEAR         TYPE GLU1-RYEAR,
       RBUKRS        TYPE GLU1-RBUKRS,
       RACCT         TYPE GLU1-RACCT,
       ORG_ACCOUNT   TYPE GLU1_XSL_CUM-ORG_ACCOUNT,
       RTCUR         TYPE GLU1-RTCUR,
       RUNIT         TYPE GLU1-RUNIT,
       TIPO(1)       TYPE C,
       MSG_ERRO(100) TYPE C,
       END OF TY_LOG.

* Tabelas Internas auxiliares no processo de batch input.
DATA: TI_BDCDATA    TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      TI_BDCMSGCOLL TYPE STANDARD TABLE OF BDCMSGCOLL, "Guarda as mensagens
      TI_LOG        TYPE STANDARD TABLE OF TY_LOG.     "Relatório de mensagens

* Work Areas
DATA: WA_BDCDATA    LIKE LINE OF TI_BDCDATA ,
      WA_BDCMSGCOLL LIKE LINE OF TI_BDCMSGCOLL,
      WA_LOG        LIKE LINE OF TI_LOG.

*Variáveis
DATA: V_MSG         LIKE T100-TEXT,
      V_MSGV1       LIKE BALM-MSGV1,
      V_MSGV2       LIKE BALM-MSGV2,
      V_MSGV3       LIKE BALM-MSGV3,
      V_MSGV4       LIKE BALM-MSGV4.


*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
DATA: VG_REPID           LIKE SY-REPID,
      VG_VARIANT        TYPE DISVARIANT.

DATA: VARIANTE        LIKE DISVARIANT,
      DEF_VARIANTE    LIKE DISVARIANT.
*<<<Cris

DATA: T_LIST_BS_GLU1 LIKE S_LIST_GLU1 OCCURS 0 WITH HEADER LINE.
DATA: T_LIST_PL_GLU1 LIKE S_LIST_GLU1 OCCURS 0 WITH HEADER LINE.
DATA  C_LIST_FIELD1 LIKE DFIES-FIELDNAME.
DATA  C_LIST_FIELD2 LIKE DFIES-FIELDNAME.
DATA  C_LIST_FIELD3 LIKE DFIES-FIELDNAME.
DATA: BEGIN OF T_LIST_RVERS OCCURS 0,
        RVERS LIKE GLU1-RVERS,
      END OF T_LIST_RVERS.

DATA: GT_VERSION_DATA TYPE T_VERSION_DATA. "All versions processed

DATA: BEGIN OF T_LIST_RRCTY OCCURS 0,
        RRCTY LIKE GLU1-RRCTY,
      END OF T_LIST_RRCTY.
DATA: H_FIRST_TIME.

DATA: G_LIST_MODE(2),      "BS or PL accounts
      G_LIST_BUILD_SUM(1), "Build sum for retained earnings acc
      G_LIST_LIST_APPEND(1), "Append list
      G_LIST_SPECIAL_NAME LIKE SY-REPID VALUE 'SAPFGVTR  '.

*table for inactive ok-codes
DATA: BEGIN OF INACTIVE OCCURS 7,
        FCODE LIKE RSMPE-FUNC,
      END   OF INACTIVE.
*Variables for messages
DATA: H_MESS_COUNT LIKE SY-TABIX,
      H_MAX_SEVERITY LIKE SY-SUBRC.
*Variables for processing control
DATA: BEGIN OF APPL,
        FISL(5) VALUE 'FISL',
        FIGL(5) VALUE 'FIGL',
        FIGLX(5) VALUE 'FIGLX',
        FIGLF(5) VALUE 'FIGLF',
        ECPCA(5) VALUE 'ECPCA',
        SE38(5) VALUE 'SE38',
        ISSR(5) VALUE 'ISSR',
        ISRT(5) VALUE 'ISRT',                               "AL0K091914
      END OF APPL.
DATA: C_TAB LIKE T800A-TAB.
DATA: T_LEDGERS LIKE T881 OCCURS 0 WITH HEADER LINE.
DATA  G_V_PARALLEL_LEDGERS.
*Global variable for mode
DATA: G_MODE.  "'E' = Execute, 'D' = Delete
*Date of the last generator version YYYYMMDD
*(see also c_gendat in RGUGVTR0!!)
DATA: C_GENDAT(8) TYPE C VALUE '20020109'.

* Dunkle Eingabe-Parameter für Workflow Schedman - werden übergeben
*INCLUDE RKASMAWF.
INCLUDE ZRKASMAWF.

DATA: TAB_RT LIKE T881-TAB VALUE 'PCRRETAILT'.              "AL0K091914

DATA: G_GLFLEX TYPE BOOLE_D.

CONSTANTS: CON_RRCTY_ACTUALS TYPE RRCTY VALUE '0'.          "Reccordtype actuals

*Global variables for line item update
DATA: GD_WRITE_SI.                                          "write line items?

* Gravar em ZIB_CONTABIL
DATA: VSEQITEM TYPE ZIB_CONTABIL-SEQITEM,
      VSEQTOTA TYPE ZIB_CONTABIL-SEQITEM,
      VNUM(10)    TYPE C,
      VSEQ(10)    TYPE P,
      VOBJ_KEY    TYPE ZGL012_AVM-OBJ_KEY.

TYPES: BEGIN OF TY_ZIB_CONTABIL.
        INCLUDE STRUCTURE ZIB_CONTABIL.
TYPES:  MARK   TYPE C,
END OF TY_ZIB_CONTABIL.

TYPES: BEGIN OF TY_ZIB_CONTABIL_ERR.
        INCLUDE STRUCTURE ZIB_CONTABIL_ERR.
TYPES:  MARK   TYPE C,
END OF TY_ZIB_CONTABIL_ERR.

TYPES: BEGIN OF TY_ZIB_CONTABIL_CHV.
        INCLUDE STRUCTURE ZIB_CONTABIL_CHV.
TYPES:  MARK   TYPE C,
END OF TY_ZIB_CONTABIL_CHV.

data: WA_ZIB_CONTABIL     TYPE TY_ZIB_CONTABIL,
      WA_ZIB_CONTABIL_ERR TYPE TY_ZIB_CONTABIL_ERR,
      WA_ZIB_CONTABIL_CHV TYPE TY_ZIB_CONTABIL_CHV,
      IT_ZIB_CONTABIL     TYPE TABLE OF TY_ZIB_CONTABIL,
      IT_ZIB_CONTABIL_ERR TYPE TABLE OF TY_ZIB_CONTABIL_ERR,
      IT_ZIB_CONTABIL_CHV TYPE TABLE OF TY_ZIB_CONTABIL_CHV.

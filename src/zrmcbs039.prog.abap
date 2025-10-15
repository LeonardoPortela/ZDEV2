REPORT zrmcbs039 NO STANDARD PAGE HEADING MESSAGE-ID m5.
*----------------------------------------------------------------------*
* Programa..: ZMMR0170                                                 *
* Descrição.: Cópia do Programa RMCBS039 (com retirada das mensagens   *
*             de retorno ) para chamada no programa ZMMR0170           *
*             (Relatório de Estoques - Giro, Valor Estoque e Cobertura *
*             de Estoque para que seja exportado para o B.I., onde     *
*             onde serão efetivamente criados relatórios de análises   *
*             de estoques em nível de depósitos)                       *
*             Obs. Retirada a exibição das mensagens de retorno        *
* Autor.....: Sara Oikawa                                              *
* Data......: 07.10.2020                                               *
*----------------------------------------------------------------------*
*ENHANCEMENT-POINT RMCBS039_G4 SPOTS ES_RMCBS039 STATIC.
*ENHANCEMENT-POINT RMCBS039_G5 SPOTS ES_RMCBS039.
*ENHANCEMENT-POINT RMCBS039_G6 SPOTS ES_RMCBS039 STATIC.
*ENHANCEMENT-POINT RMCBS039_G7 SPOTS ES_RMCBS039.


* Startreport für das Füllen der Infostruktur S039 mit Istdaten in
* Abhängigkeit der Periodizität der Info-Struktur S039.
* Bisher wird die Infostruktur S039 nur mit Plandaten versorgt.
* Damit aber Funktionen wie 'Prognose' durchgeführt werden können,
* werden aus den Infostrukturen S031 und S032 die Istdaten gelesen
* und nachträglich in der S039 verbucht.

*----------------------------------------------------------------------*
*             GLOBALE DATENDEKLARATIONEN
*----------------------------------------------------------------------*
INCLUDE rmcs0tp1.

TABLES: s031, s039, t001w, t001, mbew.

CONSTANTS: CON_MCAPP_BC LIKE TMCW3-MCAPP VALUE '03'.

DATA: con_mcinf  LIKE tmc4-mcinf VALUE 'S039',
      bukrs          LIKE t001-bukrs,
      periv          LIKE mcs0-periv,
      perivar        LIKE mcs0-periv,
      bw_bestand_alt LIKE s032-mbwbest,
      ko_bestand_alt LIKE s032-mkobest,
      bw_bestand_neu LIKE s032-mbwbest,
      ko_bestand_neu LIKE s032-mkobest,
      bw_wert_neu    LIKE s032-wbwbest,
      bw_wert_alt    LIKE s032-wbwbest,
      gesamtbestand  LIKE s039-gsbest,
      min_perio_tag  LIKE s031-sptag,
      min_perio_woc  LIKE s031-spwoc,
      min_perio_mon  LIKE s031-spmon,
      min_perio_bup  LIKE s031-spbup,
      perio_tag      LIKE s031-sptag,
      perio_woc      LIKE s031-spwoc,
      perio_mon      LIKE s031-spmon,
      perio_bup      LIKE s031-spbup,
      flg_periotab,
      flg_periv,
      mzubb LIKE s031-mzubb,
      magbb LIKE s031-magbb,
      mzukb LIKE s031-mzukb,
      magkb LIKE s031-magkb,
      wzubb LIKE s031-wzubb,
      wagbb LIKE s031-wagbb,
      hlp_lines TYPE i,
      mcper VALUE '3',
      mcexc VALUE 'X',
      periodenlaenge LIKE sy-tabix,
      jahresperioden LIKE sy-tabix,
      int_s031 LIKE s031 OCCURS 0 WITH HEADER LINE,
      int_s032 LIKE s032 OCCURS 0 WITH HEADER LINE,
      int_s000 LIKE s039 OCCURS 0 WITH HEADER LINE,
      del_s039 LIKE s039 OCCURS 0 WITH HEADER LINE,
      package_size TYPE I VALUE 10000,
      BEGIN OF hlp_tab OCCURS 0,
        bwkey       LIKE t001w-bwkey,
        werks       LIKE t001w-werks,
        periv       LIKE t001-periv,
        anzbp       LIKE t009-anzbp,
        periolaenge LIKE sy-tabix,
      END OF hlp_tab,
      BEGIN OF feldleiste,
       mandt LIKE int_s032-mandt,
       ssour LIKE int_s032-ssour,
       vrsio LIKE int_s032-vrsio,
       spmon LIKE int_s031-spmon,
       sptag LIKE int_s031-sptag,
       spwoc LIKE int_s031-spwoc,
       spbup LIKE int_s031-spbup,
       werks LIKE int_s032-werks,
       matnr LIKE int_s032-matnr,
       lgort LIKE int_s032-lgort,
 END OF feldleiste.

 DATA: xmbew LIKE mbew OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
*           Selektionsparameter für Periodizität Tag
*----------------------------------------------------------------------*
SELECTION-SCREEN SKIP 1.
* "Merkmale"
SELECTION-SCREEN BEGIN OF BLOCK merkmale WITH FRAME TITLE text-029.
SELECT-OPTIONS: sl_werks FOR s039-werks,
                sl_lgort FOR s039-lgort,
                sl_matnr FOR s039-matnr MATCHCODE OBJECT mat1.
SELECTION-SCREEN END OF BLOCK merkmale.
SELECTION-SCREEN SKIP 1.
* "Analysezeitraum"
SELECTION-SCREEN BEGIN OF BLOCK zeitraum WITH FRAME TITLE text-030.
SELECT-OPTIONS:  sl_sptag FOR s031-sptag MODIF ID per.
SELECT-OPTIONS:  sl_spwoc FOR s031-spwoc MODIF ID per.
SELECT-OPTIONS:  sl_spmon FOR s031-spmon MODIF ID per.
SELECT-OPTIONS:  sl_spbup FOR s031-spbup MODIF ID per.
SELECTION-SCREEN END OF BLOCK zeitraum.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK valuation WITH FRAME TITLE text-033.
PARAMETERS: va LIKE mcbeleg-valuation DEFAULT 'X'
            RADIOBUTTON GROUP hit.
PARAMETERS: ova LIKE mcbeleg-nosplit RADIOBUTTON GROUP hit.
SELECTION-SCREEN END OF BLOCK valuation.

* Analysezeitraum für Abmischung S031/S032
RANGES:            zeitr    FOR mcs0-sptag,
                   sl_zeitr  FOR mcs0-sptag,
                   periotab FOR mcs0-sptag,
                   bwkey    FOR t001w-bwkey.

*----------------------------------------------------------------------*
*                      Hauptprogrammm                                  *
*----------------------------------------------------------------------*

INITIALIZATION.
* Periodizität der Info-Struktur ermitteln
  hlp_mcinf = 'S039'.
  PERFORM periodizitaet_ermitteln(rmcs1000)
          USING    hlp_mcinf
          CHANGING sav_speri.

  CASE sav_speri.
    WHEN con_speri_sptag.
      periodenlaenge = 1.
      jahresperioden = 360.
    WHEN con_speri_spmon.
      periodenlaenge = 30.
      jahresperioden = 12.
    WHEN con_speri_spwoc.
      periodenlaenge = 7.
      jahresperioden = 52.
    WHEN con_speri_spbup.
  ENDCASE.
  PERFORM sel_zeitraum_initialisieren USING mcexc
                                            mcper
                                            sav_speri.

AT SELECTION-SCREEN OUTPUT.
*--> Welche Periodizität erscheint auf dem Screen?
  PERFORM selektionsbild_vorbereiten.

AT SELECTION-SCREEN.
*Einlesen der Datenbanktabellen S031 und S032
* perform interne_tabellen_fuellen.

START-OF-SELECTION.
*Prüfung, ob Berechtigung zum Neuaufbau für Infostrukturen vorliegt
  PERFORM check_berechtigung USING con_mcapp_bc.
*einlesen der datenbanktabellen s031 und s032
  PERFORM interne_tabellen_fuellen.
*Füllen interne Tabelle Int_S000
  PERFORM int_s000_fuellen.
*Verbuchung der Datensätze in die S039
  PERFORM verbuchung_s039.

  DESCRIBE TABLE int_s000 LINES hlp_lines.
* Início - Sara Oikawa - 07.10.2020
*  MESSAGE i369 WITH hlp_lines.
* Fim - Sara Oikawa - 07.10.2020

*----------------------------------------------------------------------*
*                        Unterprogramme                                *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM INTERNE_TABELLEN_FUELLEN                                 *
*---------------------------------------------------------------------*
FORM interne_tabellen_fuellen.

* Ranges für VRSIO füllen
  REFRESH sl_vrsio.
  sl_vrsio-low    = selektions_vrsio.
  sl_vrsio-high   = selektions_vrsio.
  sl_vrsio-option = 'EQ'.
  sl_vrsio-sign   = 'I'.
  APPEND sl_vrsio.
* Tabelle ZEITR konvertieren.
  PERFORM sl_zeitr_fuellen.
* Rangestabelle ZEITR füllen für Selektion in S031
  PERFORM zeitr_fuellen.
  PERFORM zeitr_konvertieren.
* Selektion aller Bewegungen im Zeitraum Periotab-low bis Sy-Datum
* Es müssen auch die Bewegungen selektiert werden, die nicht auf dem
* Selektionsscreen erscheinen, aber zum Rückrechnen der Bestände be-
* nötigt werden.
  SELECT * FROM s031 INTO TABLE int_s031
    WHERE ssour =  space
      AND vrsio IN sl_vrsio
      AND spmon IN sl_spmon
      AND sptag IN sl_sptag
      AND spwoc IN sl_spwoc
      AND spbup IN sl_spbup
      AND werks IN sl_werks
      AND matnr IN sl_matnr
      AND lgort IN sl_lgort
      ORDER BY PRIMARY KEY.
* Einlesen aller Bestände aus der Datenbanktabelle S032 in eine
* interne Tabelle Int_S032
  SELECT * FROM s032 INTO TABLE int_s032
     WHERE ssour =  space
       AND vrsio IN sl_vrsio
       AND werks IN sl_werks
       AND lgort IN sl_lgort
       AND matnr IN sl_matnr
       ORDER BY PRIMARY KEY.
ENDFORM.                    "interne_tabellen_fuellen

*---------------------------------------------------------------------*
*       FORM INT_S000_FUELLEN                                         *
*---------------------------------------------------------------------*
* 1. Übertragen der Bestände und Bewegungen in die Int_S000 durch      *
*    Abmischung.                                                       *
* 2. Abmischen bedeutet: Da wir in der S032 nur den aktuellen Bestand  *
*    haben, müssen die Bestände für die selektierten Perioden          *
*    zurückgerechnet werden. Außerdem muß in einer selektierten        *
*    Periode, die keine Bewegungen enthält, dennoch eine Zeile         *
*    in die Int_S000, die den Bestand ausweist.                        *
*--------------------------------------------------------------------- *
FORM int_s000_fuellen.
*Füllen Tabelle Periotab
  IF sav_speri NE con_speri_spbup.
* Füllen Tabelle Periotab unabhängig von Geschäftsjahresvariante
    PERFORM periotab_fuellen.
  ELSE.
    PERFORM anzahl_gjvarianten_pruefen.
    IF flg_periv = true.
*Gjvariante voreingestellt in Tabelle TMC4.
      PERFORM periotab_fuellen.
    ENDIF.
  ENDIF.
* Rückrechnen der Bestände
  PERFORM bestand_zurueckrechnen.
*Nach erfolgter Abmischung wird geprüft, ob INT_S000 gefüllt ist, d.h.,
*ob zur gewählten Selektion Daten vorhanden sind.
  READ TABLE int_s000 INDEX 1.
* Início - Sara Oikawa - 07.10.2020
*  IF sy-subrc <> 0.
*   "Keine Daten zur gewählten Selektion vorhanden"
*    MESSAGE i017.
*    IF sy-batch IS INITIAL.
*      SUBMIT (sy-repid) VIA SELECTION-SCREEN.
*    ENDIF.
*  ENDIF.
* Fim - Sara Oikawa - 07.10.2020

ENDFORM.                    "int_s000_fuellen

*---------------------------------------------------------------------*
*       FORM VERBUCHUNG_S039                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM verbuchung_s039.

DO.
  SELECT * FROM s039 INTO TABLE del_s039
                          UP TO package_size ROWS
    WHERE ssour =  space
      AND vrsio =  '000'
      AND werks IN sl_werks
      AND matnr IN sl_matnr
      AND lgort IN sl_lgort
      AND spmon IN sl_spmon
      AND sptag IN sl_sptag
      AND spwoc IN sl_spwoc
      AND spbup IN sl_spbup
      %_HINTS ORACLE 'FIRST_ROWS'.
     IF SY-SUBRC <> 0.
        EXIT.
     ENDIF.
    DELETE S039 FROM TABLE DEL_S039.
    CALL FUNCTION 'DB_COMMIT'.
    CLEAR DEL_S039. REFRESH DEL_S039.
ENDDO.

  INSERT S039 FROM TABLE INT_S000.
COMMIT WORK.

ENDFORM.                    "verbuchung_s039

*---------------------------------------------------------------------*
*       FORM ZEITR_FUELLEN                                            *
*---------------------------------------------------------------------*
*       Füllen der Rangestabelle ZEITR für Selektion der Datenbank    *
*       S031 für alle Perioden bis zum aktuellen Zeitpunkt, da        *
*       Bestände zurückgerechnet werden müssen.                      *
*---------------------------------------------------------------------*
FORM zeitr_fuellen.

  DATA: zf_lines TYPE i.

  DESCRIBE TABLE sl_zeitr LINES zf_lines.
  READ TABLE sl_zeitr INDEX zf_lines.
* Zunächst wird davon geprüft, inwieweit die Rangestabelle SL_ZEITR
* einfach zu interpretieren ist.
  CASE zf_lines.
    WHEN '0'.
*SL_ZEITR nicht gefüllt
      CLEAR zeitr.
      REFRESH zeitr.
    WHEN '1'.
*SL_ZEITR enthält genau eine Zeile.
      IF sl_zeitr-sign = 'I'.
        IF sl_zeitr-option = 'BT' OR sl_zeitr-option = 'EQ' OR
           sl_zeitr-option = 'GE'.
          zeitr-low = sl_zeitr-low.
          CLEAR zeitr-high.
          zeitr-option = 'GE'.
          zeitr-sign = 'I'.
          APPEND zeitr.
        ENDIF.
        IF sl_zeitr-option = 'LE'.
          CLEAR zeitr.
          REFRESH zeitr.
        ENDIF.
        IF sl_zeitr-option = 'CP'.
          PERFORM zeitr_minimum_bestimmen.
        ENDIF.
      ELSE.
        IF sl_zeitr-option = 'BT' OR sl_zeitr-option = 'EQ' OR
           sl_zeitr-option = 'GE'.
          CLEAR zeitr.
          REFRESH zeitr.
        ENDIF.
        IF sl_zeitr-option = 'LE'.
          zeitr-low = sl_zeitr-low.
          CLEAR zeitr-high.
          zeitr-option = 'GE'.
          zeitr-sign = 'I'.
          APPEND zeitr.
        ENDIF.
        IF sl_zeitr-option = 'CP'.
          PERFORM zeitr_minimum_bestimmen.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      PERFORM zeitr_minimum_bestimmen.
  ENDCASE.
ENDFORM.                    "zeitr_fuellen

*---------------------------------------------------------------------*
*      FORM SL_ZEITR_FUELLEN                                          *
*---------------------------------------------------------------------*
*      Selektionszeitraum: SL_ZEITR-LOW bis SY-DATUM für Selektion
*      aller Materialbewegungen aus der S031.                         *
*      Die Konvertierung ist notwendig, damit beim Lesen einer Daten- *
*      bank die richtige Periodizität abgefragt werden kann.          *
*---------------------------------------------------------------------*
FORM sl_zeitr_fuellen.
  REFRESH: zeitr.
  CASE sav_speri.
    WHEN con_speri_spmon.
      LOOP AT sl_spmon.
        MOVE-CORRESPONDING sl_spmon TO sl_zeitr.
        APPEND sl_zeitr.
      ENDLOOP.
    WHEN con_speri_sptag.
      LOOP AT sl_sptag.
        MOVE-CORRESPONDING sl_sptag TO sl_zeitr.
        APPEND sl_zeitr.
      ENDLOOP.
    WHEN con_speri_spwoc.
      LOOP AT sl_spwoc.
        MOVE-CORRESPONDING sl_spwoc TO sl_zeitr.
        APPEND sl_zeitr.
      ENDLOOP.
    WHEN con_speri_spbup.
      LOOP AT sl_spbup.
        MOVE-CORRESPONDING sl_spbup TO sl_zeitr.
        APPEND sl_zeitr.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "sl_zeitr_fuellen

*---------------------------------------------------------------------*
*       FORM ZEITR_MINIMUM_BESTIMMEN                                  *
*---------------------------------------------------------------------*
*Rangestabelle SL_Zeitr hat mehrere Einträge. Welcher Low-Wert ist der*
*kleinste ? Lösung durch Herantasten mit Hilfe eines Startdatums.     *
*---------------------------------------------------------------------*
FORM zeitr_minimum_bestimmen.

  DATA: zb_start_datum TYPE d VALUE '19910101',
        zb_anz_perio TYPE i,
        zb_tag LIKE mcs0-sptag,
        zb_woc LIKE mcs0-spwoc,
        zb_mon LIKE mcs0-spmon,
        zb_bup LIKE mcs0-spbup.

  PERFORM periode_ermitteln(rmcs1000) USING  zb_start_datum
                                             con_mcinf
                                             bukrs
                                   CHANGING  zb_tag
                                             zb_woc
                                             zb_mon
                                             zb_bup
                                             periv.

  CASE  sav_speri.
    WHEN con_speri_sptag.
      WHILE NOT zb_tag IN zeitr.
        zb_tag = zb_tag + 1.
      ENDWHILE.
      IF zb_tag = zb_start_datum.
        CLEAR zeitr. REFRESH zeitr.
      ELSE.
        zeitr-low = zb_tag.
        CLEAR zeitr-high.
        zeitr-option = 'GE'.
        zeitr-sign = 'I'.
        APPEND zeitr.
      ENDIF.
    WHEN con_speri_spmon.
      WHILE NOT zb_mon IN zeitr.
        zb_mon = zb_mon + 1.
        IF zb_mon+4(2) = '13'.
          zb_mon+4(2) = '01'.
          zb_mon(4) = zb_mon(4) + 1.
        ENDIF.
      ENDWHILE.
      IF zb_mon = zb_start_datum(6).
        CLEAR zeitr. REFRESH zeitr.
      ELSE.
        zeitr-low(6) = zb_mon.
        CLEAR zeitr-high.
        zeitr-option = 'GE'.
        zeitr-sign = 'I'.
        APPEND zeitr.
      ENDIF.
    WHEN con_speri_spwoc.
      WHILE NOT zb_woc IN zeitr.
        IF zb_woc+4(2) < '52'.
          zb_woc+4(2) = zb_woc+4(2) + 1.
        ENDIF.
        IF zb_woc+4(2) = '52'.
          PERFORM kalenderwoche_ermitteln USING zb_woc.
        ENDIF.
      ENDWHILE.
      IF zb_woc = zb_start_datum(6).
        CLEAR zeitr. REFRESH zeitr.
      ELSE.
        zeitr-low(6) = zb_woc.
        CLEAR zeitr-high.
        zeitr-option = 'GE'.
        zeitr-sign = 'I'.
        APPEND zeitr.
      ENDIF.
    WHEN con_speri_spbup.
      WHILE NOT zb_bup IN zeitr.
        zb_anz_perio = zb_bup+4(2) + 1.
        IF zb_anz_perio LE 99.
          zb_bup+4(2) = zb_bup+4(2) + 1.
        ELSE.
          zb_bup+4(2) = '01'.
          zb_bup(4) = zb_bup(4) + 1.
        ENDIF.
      ENDWHILE.
      IF zb_bup = zb_start_datum(6).
        CLEAR zeitr. REFRESH zeitr.
      ELSE.
        zeitr-low(6) = zb_bup.
        CLEAR zeitr-high.
        zeitr-option = 'GE'.
        zeitr-sign = 'I'.
        APPEND zeitr.
      ENDIF.
  ENDCASE.

ENDFORM.                    "zeitr_minimum_bestimmen

*---------------------------------------------------------------------*
*       FORM KALENDERWOCHE_ERMITTELN                                  *
*---------------------------------------------------------------------*
*       Hier wird ermittelt, ob auf die 52.Kalenderwoche noch eine    *
*       53.Woche folgt oder nicht. Dabei wird wie folgt vorgegangen:  *
*       Solange die 52.Woche nicht erreicht ist, kann zur Woche immer *
*       eine Woche addiert werden, ohne daß es Probleme mit dem       *
*       Jahreswechsel gibt. Ist die 52.Woche erreicht, wird geprüft,  *
*       ob es eine 53.Woche gibt. Wenn ja, wird sie noch gesetzt.     *
*       Wenn nein, folgt auf die 52.Woche die 1.Woche des neuen       *
*       Jahres.
*---------------------------------------------------------------------*
FORM kalenderwoche_ermitteln USING week LIKE mcs0-spwoc.
  DATA: ke_week LIKE s031-spwoc,
        ke_datum LIKE sy-datum.

  ke_week = week.

  IF flg_periotab = true.
    ke_week = periotab-low.
  ELSE.
    ke_week = zeitr-low.
  ENDIF.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week = ke_week
    IMPORTING
      date = ke_datum.

  ke_datum = ke_datum + 7.
  CLEAR ke_week.

  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date = ke_datum
    IMPORTING
      week = ke_week.

  IF flg_periotab = true.
    periotab-low = ke_week.
  ELSE.
    zeitr-low = ke_week.
  ENDIF.
ENDFORM.                    "kalenderwoche_ermitteln

*---------------------------------------------------------------------*
*       FORM BESTAND_ZURUECKRECHNEN.                                  *
*---------------------------------------------------------------------*
*       Bestandsermittlung für alle Perioden vom aktuellen            *
*       Bestand an.                                                   *
*---------------------------------------------------------------------*
FORM bestand_zurueckrechnen.

  CASE sav_speri.
    WHEN con_speri_spbup.
*Bestandsrechnung mit Sonderlogik Geschäftsjahresvariante
      PERFORM bestand_zurueckrechnen_buper.
    WHEN OTHERS.
*Bestandsrechnung für Periodizitäten Tag, Monat, Woche
      PERFORM bestand_zurueckrechnen_allg.
*Anzahl selektierter Perioden_feststellen für Berechnung Zusatzkennz.
      PERFORM anzahl_perioden_feststellen.
  ENDCASE.
  PERFORM korrektur_bestandswert TABLES int_s000.

ENDFORM.                    "bestand_zurueckrechnen

*---------------------------------------------------------------------*
*       FORM ANZAHL_GJVARIANTEN_PRUEFEN                               *
*---------------------------------------------------------------------*
*       Es wird geprüft, ob bei der Periodizität 'Buchungsperiode'    *
*       eine oder mehrere Geschäftsvarianten zu beachten sind         *
*---------------------------------------------------------------------*
FORM anzahl_gjvarianten_pruefen.

*Prüfen, ob Geschäftsjahresvariante voreingestellt.
  SELECT SINGLE * FROM tmc4 WHERE mcinf = hlp_mcinf.
  IF sy-subrc = 0.
    perivar = tmc4-periv.
    IF perivar IS INITIAL.
*GjVariante(n) aus T001 nachlesen.
      PERFORM gjvar_nachlesen.
    ELSE.
      flg_periv = true.
      SELECT SINGLE * FROM t009 WHERE periv = perivar.
      IF sy-subrc = 0.
*     Periodenlaenge merken für Zusatzkennzahlen
        periodenlaenge = 360 / t009-anzbp.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM gjvar_nachlesen.
  ENDIF.
ENDFORM.                    "anzahl_gjvarianten_pruefen

*---------------------------------------------------------------------*
*       FORM PERIOTAB_FUELLEN                                         *
*---------------------------------------------------------------------*
*       Füllen der Ranges-Tabelle Periotab                            *
*---------------------------------------------------------------------*
FORM periotab_fuellen.
  flg_periotab = true.
*Bestimmen Periotab-Low aus Rangestabelle ZEITR
  READ TABLE zeitr INDEX 1.
  IF sy-subrc = 0.
    periotab-low = zeitr-low.
    APPEND periotab.
  ELSE.
*Kleinste Periode wird aus Datenbank S031 gelesen.
    CASE sav_speri.
      WHEN con_speri_sptag.
        PERFORM kleinste_periode_allgemein.
        periotab-low = min_perio_tag.
      WHEN con_speri_spwoc.
        PERFORM kleinste_periode_allgemein.
        periotab-low = min_perio_woc.
      WHEN con_speri_spmon.
        PERFORM kleinste_periode_allgemein.
        periotab-low = min_perio_mon.
      WHEN con_speri_spbup.
        PERFORM kleinste_periode_buperiode.
        periotab-low = min_perio_bup.
    ENDCASE.
    APPEND periotab.
  ENDIF.
* Ermitteln des Wertes PERIOTAB-HIGH = SY-DATUM

*Für Periodizität Buchungsperiode Geschäftsjahresvariante  übergeben
  IF sav_speri = con_speri_spbup.
    IF flg_periv = true.
* Geschjvariante aus Voreinstellung TMC4
      periv = perivar.
    ELSE.
* Es gibt mehrere Geschjvarianten - Ermittlung über BUKRS
      periv = hlp_tab-periv.
    ENDIF.
  ENDIF.

  PERFORM periode_ermitteln(rmcs1000) USING  sy-datum
                                             hlp_mcinf
                                             bukrs
                                   CHANGING  perio_tag
                                             perio_woc
                                             perio_mon
                                             perio_bup
                                             periv.
  CASE sav_speri.
    WHEN con_speri_sptag.
      periotab-high = perio_tag.
    WHEN con_speri_spwoc.
      periotab-high = perio_woc.
    WHEN con_speri_spmon.
      periotab-high = perio_mon.
    WHEN con_speri_spbup.
      periotab-high = perio_bup.
  ENDCASE.
  CLEAR: perio_tag, perio_woc, perio_mon, perio_bup.
*Aufnahme aller Perioden von PERIOTAB-LOW bis SY-DATUM
  WHILE periotab-low LT periotab-high.
    CASE sav_speri.
      WHEN con_speri_sptag.
        periotab-low = periotab-low + 1.
        APPEND periotab.
      WHEN con_speri_spmon.
        periotab-low+4(2) = periotab-low+4(2) + 1.
        IF periotab-low+4(2) = '13'.
          periotab-low+4(2) = '01'.
          periotab-low(4)   = periotab-low(4) + 1.
        ENDIF.
        APPEND periotab.
      WHEN con_speri_spwoc.
        IF periotab-low+4(2) LT '52'.
          periotab-low+4(2) = periotab-low+4(2) + 1.
        ELSEIF periotab-low+4(2) = '52'.
          PERFORM kalenderwoche_ermitteln USING perio_woc.
        ELSEIF periotab-low+4(2) = '53'.
          periotab-low(4) = periotab-low(4) + 1.
          periotab-low+4(2) = '01'.

        ENDIF.
        APPEND periotab.
      WHEN con_speri_spbup.
*Lesen Geschjvariante
        IF flg_periv = true.
          periv = perivar.
        ELSE.
          periv = hlp_tab-periv.
        ENDIF.
        SELECT SINGLE * FROM t009 WHERE periv = periv.
        IF sy-subrc = 0.
          PERFORM tabelle_t009_pruefen.
        ELSE.
          periotab-low+4(2) = periotab-low+4(2) + 1.
          IF periotab-low+4(2) = '13'.
            periotab-low+4(2) = '01'.
            periotab-low(4) = periotab-low(4) + 1.
          ENDIF.
          APPEND periotab.
        ENDIF.
    ENDCASE.
  ENDWHILE.
  SORT periotab BY low DESCENDING.
ENDFORM.                    "periotab_fuellen

*---------------------------------------------------------------------*
*       FORM TABELLE_T009_PRUEFEN                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM tabelle_t009_pruefen.

TABLES: T009B.

DATA: anz_per LIKE t009-anzbp.

DATA: XT009B LIKE T009B OCCURS 0 WITH HEADER LINE,
      LV_GJAHR LIKE T009B-BDATJ,
      LV_POPER LIKE T009B-POPER.

DATA: T_POPER LIKE T009B-POPER.

STATICS: BEGIN OF XT009B_NEW OCCURS 0,
           PERIV LIKE T009B-PERIV,
           BDATJ LIKE T009B-BDATJ,
           POPER LIKE T009B-POPER,
           RELJR LIKE T009B-RELJR,
           GJAHR LIKE T009B-BDATJ,
         END OF   XT009B_NEW.

  Clear T_POPER.
  Move PERIOTAB-LOW+4(2) to T_POPER.

  ANZ_PER = T009-ANZBP.

  IF ANZ_PER GT 99.
    ANZ_PER = 99.
  ENDIF.

  SELECT SINGLE  * FROM T009B WHERE
                         PERIV = T009-PERIV AND NOT reljr = '0'.

  IF sy-subrc = 0.
************non-calendar fiscal year**********************************
   IF NOT XT009B_NEW IS INITIAL.
    READ TABLE XT009B_NEW INDEX 1.
    IF Xt009B_NEW-PERIV NE T009-PERIV.
     CLEAR XT009B_NEW.
     REFRESH xt009B_NEW.
    ENDIF.
   ENDIF.

   IF XT009B_NEW[] IS INITIAL.
    SELECT * FROM T009B INTO CORRESPONDING FIELDS OF TABLE XT009B_NEW
      WHERE PERIV = T009-PERIV.
    IF sy-subrc = 0.
     LOOP AT XT009B_NEW.
      IF xt009b_new-reljr = '+1'.
         xt009b_new-GJAHR = xt009B_new-bdatj + 1.
      ELSEIF xt009b_new-reljr = '-1'.
         xt009b_new-GJAHR = xt009B_new-bdatj - 1.
      ELSE.
        xt009b_new-GJAHR = xt009B_new-bdatj.
      ENDIF.
      MODIFY xt009B_new.
     ENDLOOP.
     SORT xt009B_NEW BY PERIV GJAHR POPER DESCENDING.
    ENDIF.
   ENDIF.
   CLEAR: LV_GJAHR, LV_POPER.
   LOOP AT xt009B_new WHERE GJAHR = PERIOTAB-LOW(4).
    IF xt009b_new-GJAHR NE LV_GJAHR OR
       LV_GJAHR IS INITIAL.
     CLEAR: LV_GJAHR, LV_POPER.
     LV_GJAHR = xt009b_new-GJAHR.
     IF LV_POPER IS INITIAL OR
      xt009b_new-poper GT LV_POPER.
      LV_POPER = xt009b_new-poper.
     ENDIF.
    ENDIF.
   ENDLOOP.
   IF LV_POPER LT ANZ_PER.
    ANZ_PER = LV_POPER.
   ENDIF.

ELSE.
****************************normal fiscal year**************************

 SELECT * FROM T009B INTO TABLE XT009B WHERE
                          PERIV = T009-PERIV AND
                          BDATJ = PERIOTAB-LOW(4).
  SORT XT009B BY PERIV BDATJ POPER DESCENDING.
  READ TABLE XT009B INDEX 1.
  IF XT009B-POPER LT ANZ_PER.
    ANZ_PER = XT009B-POPER.
  ENDIF.
ENDIF.
IF anz_per IS INITIAL.
*T009 nicht gepflegt ==> Behandlung Buchungsperiode wie Kalenderjahr
  periotab-low+4(2) = periotab-low+4(2) + 1.
  IF periotab-low+4(2) = '13'.
    periotab-low+4(2) = '01'.
    periotab-low(4) = periotab-low(4) + 1.
  ENDIF.
ELSE.
  periotab-low+4(2) = periotab-low+4(2) + 1.
  IF periotab-low+4(2) GT anz_per+1(2).
    periotab-low+4(2) = '01'.
    periotab-low(4) = periotab-low(4) + 1.
  ENDIF.
ENDIF.
APPEND periotab.
ENDFORM.                    "tabelle_t009_pruefen

*---------------------------------------------------------------------*
*       FORM KLEINSTE_PERIODE_BUPERIODE                               *
*---------------------------------------------------------------------*
*       In der Int_S031 wird die kleinste Periode festgestellt.       *
*       Es sollen nur soviele Dummy-Bewegungszeilen auf den           *
*       Bildschirm wie nötig. Da in der Periotab u.U. alle Perioden   *
*       ab dem 1.1.1991 aufgelistet sind, können u.U. sehr viele      *
*       Dummy-Bewegungszeilen auf den Bildschirm kommen. Deshalb wird *
*       als älteste Periode die aus der Int_S031 berücksichtigt.      *
*---------------------------------------------------------------------*
FORM kleinste_periode_buperiode.
* Bei mehreren Geschäftsjahresvarianten hängt kleinste Periode von
* entsprechender Geschäftsjahresvariante ab.
  READ TABLE sl_zeitr INDEX 1.
  IF  sy-subrc <> 0.
* Feststellen der kleinsten Periode nur notwendig, wenn der Anwender
* keinen Selektionszeitraum angegeben hat oder den vorgeschlagenen
* auf 'blank' setzt.
    READ TABLE int_s031 INDEX 1.
    IF sy-subrc = 0.
*Bewegungstabelle ist gefüllt - Zuweisung eines Anfangswertes
      PERFORM periode_ermitteln(rmcs1000) USING  sy-datum
                                                 hlp_mcinf
                                                 bukrs
                                       CHANGING  min_perio_tag
                                                 min_perio_woc
                                                 min_perio_mon
                                                 min_perio_bup
                                                 hlp_tab-periv.

      LOOP AT int_s031 WHERE periv = hlp_tab-periv.
*Hilfstabelle HLP_TAB enthält alle Geschäftsjahresvarianten
        IF int_s031-spbup LE min_perio_bup AND NOT
        ( int_s031-spbup IS INITIAL ).
          min_perio_bup = int_s031-spbup.
        ENDIF.
      ENDLOOP.
    ELSE.
*Es gibt keine Bewegungen , d.h. S031 ist leer. Anwender kann nur den
*Bestand der aktuellen Periode angezeigt bekommen.
      PERFORM periode_ermitteln(rmcs1000) USING  sy-datum
                                                 hlp_mcinf
                                                 bukrs
                                       CHANGING  min_perio_tag
                                                 min_perio_woc
                                                 min_perio_mon
                                                 min_perio_bup
                                                 hlp_tab-periv.
    ENDIF.
  ENDIF.
ENDFORM.                    "kleinste_periode_buperiode

*---------------------------------------------------------------------*
*       FORM KLEINSTE_PERIODE_ALLGEMEIN                               *
*---------------------------------------------------------------------*
*       In der Int_S031 wird die kleinste Periode festgestellt.       *
*       Es sollen nur soviele Dummy-Bewegungszeilen auf den           *
*       Bildschirm wie nötig. Da in der Periotab u.U. alle Perioden   *
*       ab dem 1.1.1991 aufgelistet sind, können u.U. sehr viele      *
*       Dummy-Bewegungszeilen auf den Bildschirm kommen. Deshalb wird *
*       als älteste Periode die aus der Int_S031 berücksichtigt.      *
*---------------------------------------------------------------------*
FORM kleinste_periode_allgemein.
*Periodizität Monat, Woche und Tag
  READ TABLE sl_zeitr INDEX 1.
  IF  sy-subrc <> 0.
* Feststellen der kleinsten Periode nur notwendig, wenn der Anwender
* keinen Selektionszeitraum angegeben hat oder den vorgeschlagenen
* auf 'blank' setzt.
    READ TABLE int_s031 INDEX 1.
    IF sy-subrc = 0.
*Bewegungstabelle ist gefüllt - Zuweisung eines Anfangswertes
      PERFORM periode_ermitteln(rmcs1000) USING  sy-datum
                                                 hlp_mcinf
                                                 bukrs
                                       CHANGING  min_perio_tag
                                                 min_perio_woc
                                                 min_perio_mon
                                                 min_perio_bup
                                                 periv.
*Bewegungstabelle ist gefüllt - Zuweisung eines Anfangswertes
      LOOP AT int_s031.
        CASE sav_speri.
          WHEN con_speri_sptag.
            IF int_s031-sptag LE min_perio_tag AND NOT
           ( int_s031-sptag IS INITIAL ).
              min_perio_tag = int_s031-sptag.
            ENDIF.
          WHEN con_speri_spwoc.
            IF int_s031-spwoc LE min_perio_woc AND NOT
            ( int_s031-spwoc IS INITIAL ).
              min_perio_woc = int_s031-spwoc.
            ENDIF.
          WHEN con_speri_spmon.
            IF int_s031-spmon LE min_perio_mon AND NOT
            ( int_s031-spmon IS INITIAL ).
              min_perio_mon = int_s031-spmon.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ELSE.
*Es gibt keine Bewegungen , d.h. S031 ist leer. Anwender kann nur den
*Bestand der aktuellen Periode angezeigt bekommen.
      PERFORM periode_ermitteln(rmcs1000) USING  sy-datum
                                                 hlp_mcinf
                                                 bukrs
                                       CHANGING  min_perio_tag
                                                 min_perio_woc
                                                 min_perio_mon
                                                 min_perio_bup
                                                 periv.
    ENDIF.
  ENDIF.
ENDFORM.                    "kleinste_periode_allgemein

*---------------------------------------------------------------------*
*       FORM GJVAR_NACHLESEN                                          *
*---------------------------------------------------------------------*
*       Aus Buchungskreistabelle T001 GjVariante(n) nachlesen         *
*---------------------------------------------------------------------*
FORM gjvar_nachlesen.

  SELECT * FROM t001w WHERE werks IN sl_werks.
*Aufbau Rangestabelle mit Bewertungskreisen
    bwkey-low = t001w-bwkey.
    CLEAR bwkey-high.
    bwkey-option = 'EQ'.
    bwkey-sign = 'I'.
    APPEND bwkey.
* Füllen Hilfstabelle für Aufbau Periotab in Abh. von Geschäftsjahres-
* variante
    hlp_tab-bwkey = t001w-bwkey.
    hlp_tab-werks = t001w-werks.
    APPEND hlp_tab.
  ENDSELECT.

  SELECT * FROM t001 WHERE bukrs IN bwkey.
    LOOP AT hlp_tab WHERE bwkey = t001-bukrs.
      hlp_tab-periv = t001-periv.
      MODIFY hlp_tab.
    ENDLOOP.
  ENDSELECT.
*Zu jeder Geschäftsjahresvariante Anzahl Perioden merken
  LOOP AT hlp_tab.
    SELECT SINGLE * FROM t009 WHERE periv = hlp_tab-periv.
    hlp_tab-anzbp = t009-anzbp.
    hlp_tab-periolaenge = 360 / hlp_tab-anzbp.
    MODIFY hlp_tab.
  ENDLOOP.

ENDFORM.                    "gjvar_nachlesen

*---------------------------------------------------------------------*
*       FORM ANZAHL_PERIODEN_FESTSTELLEN                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM anzahl_perioden_feststellen.

ENDFORM.                    "anzahl_perioden_feststellen

*---------------------------------------------------------------------*
*       FORM BESTAND_ZURUECKRECHNEN_ALLG                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM bestand_zurueckrechnen_allg.

  LOOP AT int_s032.
*INITIALISIERUNG DER BESTÄNDE
    bw_bestand_neu = bw_bestand_alt   = int_s032-mbwbest.
    ko_bestand_neu = ko_bestand_alt   = int_s032-mkobest.
    bw_wert_neu    = bw_wert_alt      = int_s032-wbwbest.
    gesamtbestand = bw_bestand_neu + ko_bestand_neu.
    PERFORM feldleiste_fuellen.
    CLEAR int_s031.
    LOOP AT periotab.
*Ermitteln der Periodizität für Feldleiste
      CASE sav_speri.
        WHEN con_speri_spmon.
          feldleiste-spmon = periotab-low.
        WHEN con_speri_sptag.
          feldleiste-sptag = periotab-low.
        WHEN con_speri_spwoc.
          feldleiste-spwoc = periotab-low.
        WHEN con_speri_spbup.
          feldleiste-spbup = periotab-low.
      ENDCASE.
*Sichern der Bewegungen zur Bestandsrückrechnung
      mzubb = int_s031-mzubb.
      magbb = int_s031-magbb.
      mzukb = int_s031-mzukb.
      magkb = int_s031-magkb.
      wzubb = int_s031-wzubb.
      wagbb = int_s031-wagbb.
*Bewerteter Bestand
      bw_bestand_neu   = bw_bestand_neu - mzubb  + magbb.
*Konsignationsbestand
      ko_bestand_neu   = ko_bestand_neu - mzukb + magkb.
      bw_wert_neu      = bw_wert_neu    - wzubb  + wagbb.
*Gesamtbestand der aktuellen gelesenen Periode in der Periotab
      gesamtbestand = bw_bestand_neu + ko_bestand_neu.
      READ TABLE int_s031 WITH KEY feldleiste BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR int_s031.
        IF ( gesamtbestand <> 0 OR bw_wert_neu <> 0 )
        AND periotab-low IN sl_zeitr.
* Wenn keine Bewegungen - gibt es einen Bestand > 0 ?
* Eintrag in die Int_S000 erfolgt, wenn Periode im Selektionszeitraum
          PERFORM daten_zuweisen.
          PERFORM zusatzkennzahlen_bestaende.
          PERFORM zusatzkennzahlen_bewegungen.
          PERFORM zusatzkennzahlen_reichweite.
          PERFORM zusatzkennzahlen_umschlag.
          APPEND int_s000.
          CLEAR int_s000.
        ENDIF.
      ELSE.
        IF periotab-low IN sl_zeitr.
* Eintrag in die Int_S000 erfolgt, wenn Periode im Selektionszeitraum
          PERFORM daten_zuweisen.
          PERFORM zusatzkennzahlen_bestaende.
          PERFORM zusatzkennzahlen_bewegungen.
          PERFORM zusatzkennzahlen_reichweite.
          PERFORM zusatzkennzahlen_umschlag.
          APPEND int_s000.
          CLEAR int_s000.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "bestand_zurueckrechnen_allg

*---------------------------------------------------------------------*
*       FORM BESTAND_ZURUECKRECHNEN_BUPER                             *
*---------------------------------------------------------------------*
*       Bei Periodizität Buchungsperiode muß unterschieden werden     *
*       zwischen einer Geschäftsjahresvariante, die voreingestellt ist*
*       in der TMC4 und mehreren Geschäftsjahresvarianten.            *
*---------------------------------------------------------------------*

FORM bestand_zurueckrechnen_buper.
  IF flg_periv = false.
* Es gibt mehrere Geschäftsjahresvarianten: Periotab_fuellen und
* Bestandsrückrechnung müssen f. jede Gjvariante neu durchgeführt
* werden
    DO.
      READ TABLE hlp_tab INDEX 1.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

*     Festhalten Periodenlänge für Berechnung Reichweite in Tagen
      periodenlaenge = hlp_tab-periolaenge.
      jahresperioden = hlp_tab-anzbp.
      CLEAR sl_werks.
      REFRESH sl_werks.
      sl_werks-low = hlp_tab-werks.
      CLEAR sl_werks-high.
      sl_werks-option = 'EQ'.
      sl_werks-sign = 'I'.
      APPEND sl_werks.

      CLEAR periotab.
      REFRESH periotab.
      PERFORM periotab_fuellen.
      PERFORM anzahl_perioden_feststellen.

      LOOP AT int_s032 WHERE werks IN sl_werks.
*Initialisieren Hilfsvariablen für Bestände und Bestandswerte
        bw_bestand_neu = bw_bestand_alt   = int_s032-mbwbest.
        ko_bestand_neu = ko_bestand_alt   = int_s032-mkobest.
        bw_wert_alt    = int_s032-wbwbest.
        gesamtbestand = bw_bestand_neu + ko_bestand_neu.
        PERFORM feldleiste_fuellen.
        CLEAR int_s031.
        LOOP AT periotab.
*Ermitteln der Periodizität für Feldleiste
          CASE sav_speri.
            WHEN con_speri_spmon.
              feldleiste-spmon = periotab-low.
            WHEN con_speri_sptag.
              feldleiste-sptag = periotab-low.
            WHEN con_speri_spwoc.
              feldleiste-spwoc = periotab-low.
            WHEN con_speri_spbup.
              feldleiste-spbup = periotab-low.
          ENDCASE.
*Sichern der Bewegungen zur Bestandsrückrechnung
          mzubb = int_s031-mzubb.
          magbb = int_s031-magbb.
          mzukb = int_s031-mzukb.
          magkb = int_s031-magkb.
          wzubb = int_s031-wzubb.
          wagbb = int_s031-wagbb.
*Bewerteter Bestand
          bw_bestand_neu   = bw_bestand_neu - mzubb + magbb.
*Konsignationsbestand
          ko_bestand_neu   = ko_bestand_neu - mzukb + magkb.
          bw_wert_neu      = bw_wert_neu    - wzubb + wagbb.
*Gesamtbestand der aktuellen gelesenen Periode in der Periotab
          gesamtbestand = bw_bestand_neu + ko_bestand_neu.
          READ TABLE int_s031 WITH KEY feldleiste BINARY SEARCH.
          IF sy-subrc <> 0.
            CLEAR int_s031.
            IF ( gesamtbestand <> 0 OR bw_wert_neu <> 0 )
            AND periotab-low IN sl_zeitr.
* Wenn keine Bewegungen - gibt es einen Bestand > 0 ?
* Eintrag in die Int_S000 erfolgt, wenn Periode im Selektionszeitraum
              PERFORM daten_zuweisen.
              PERFORM zusatzkennzahlen_bestaende.
              PERFORM zusatzkennzahlen_bewegungen.
              PERFORM zusatzkennzahlen_reichweite.
              PERFORM zusatzkennzahlen_umschlag.
              APPEND int_s000.
              CLEAR int_s000.
            ENDIF.
          ELSE.
            IF periotab-low IN sl_zeitr.
* Eintrag in die Int_S000 erfolgt, wenn Periode im Selektionszeitraum
              PERFORM daten_zuweisen.
              PERFORM zusatzkennzahlen_bestaende.
              PERFORM zusatzkennzahlen_bewegungen.
              PERFORM zusatzkennzahlen_reichweite.
              PERFORM zusatzkennzahlen_umschlag.
              APPEND int_s000.
              CLEAR int_s000.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      DELETE hlp_tab INDEX 1.
    ENDDO.
  ELSE.
    PERFORM bestand_zurueckrechnen_allg.
*Es gibt nur eine Geschjahresvariante, d.h. die Bestandsrückrechnung
*läuft wie bei allen anderen Periodizitäten auch.
  ENDIF.
ENDFORM.                    "bestand_zurueckrechnen_buper

*---------------------------------------------------------------------*
*       FORM DATEN_ZUWEISEN.                                          *
*---------------------------------------------------------------------*
*       In Abhängigkeit vom gewählten Selektionszeitraum wird         *
*       die Tabelle Int_S000 gefüllt                                  *
*---------------------------------------------------------------------*
FORM daten_zuweisen.

* Vor dem Read über die Int_S031 wird der aktuelle Bestand in
* die Int_S039 geschrieben. Außerdem muß die Bestandsermittlung an
* dieser Stelle erfolgen, weil sich der Bestand der Vorperiode nur unter
* Berücksichtigung der Zu-und Abgänge der aktuellen Periode ergibt.
  PERFORM bestaende_zuweisen.
  PERFORM periode_zuweisen.
  PERFORM felder_int_s032_zuweisen.
  PERFORM felder_int_s031_zuweisen.
ENDFORM.                    "daten_zuweisen

*---------------------------------------------------------------------*
*       FORM FELDLEISTE_FUELLEN                                       *
*---------------------------------------------------------------------*
*       Füllen einer Feldleiste mit dem kompletten Schlüssel der      *
*       S031. Damit kann die INT_S031 gezielt mit Binary search       *
*       gelesen werden.                                               *
*---------------------------------------------------------------------*
FORM feldleiste_fuellen.

  feldleiste-mandt = int_s032-mandt.
  feldleiste-ssour = int_s032-ssour.
  feldleiste-vrsio = int_s032-vrsio.

  feldleiste-werks = int_s032-werks.
  feldleiste-matnr = int_s032-matnr.
  feldleiste-lgort = int_s032-lgort.

ENDFORM.                    "feldleiste_fuellen

*---------------------------------------------------------------------*
*       FORM FELDER_INT_S031_ZUWEISEN.                        *
*---------------------------------------------------------------------*
*       Hier werden den Feldern der Int_S000 die Werte der            *
*       entsprechenden Key- und Datenfelder aus der Int_S031  *
*       zugewiesen.                                                   *
*---------------------------------------------------------------------*
FORM felder_int_s031_zuweisen.
  int_s000-mzubb   = int_s031-mzubb.
  int_s000-wzubb   = int_s031-wzubb.
  int_s000-azubb   = int_s031-azubb.
  int_s000-mzukb   = int_s031-mzukb.
  int_s000-azukb   = int_s031-azukb.
  int_s000-magbb   = int_s031-magbb.
  int_s000-wagbb   = int_s031-wagbb.
  int_s000-aagbb   = int_s031-aagbb.
  int_s000-magkb   = int_s031-magkb.
  int_s000-aagkb   = int_s031-aagkb.
  int_s000-ambwg   = int_s031-ambwg.
  int_s000-mgvbr   = int_s031-mgvbr.
  int_s000-wgvbr   = int_s031-wgvbr.
  int_s000-agvbr   = int_s031-agvbr.
  int_s000-muvbr   = int_s031-muvbr.
  int_s000-wuvbr   = int_s031-wuvbr.
  int_s000-auvbr   = int_s031-auvbr.
  int_s000-astor   = int_s031-astor.
  int_s000-bbnull  = int_s031-bbnull.
  int_s000-kbnull  = int_s031-kbnull.
  int_s000-gbnull  = int_s031-gbnull.
ENDFORM.                    "felder_int_s031_zuweisen

*---------------------------------------------------------------------*
*       FORM FELDER_INT_S032_ZUWEISEN.                                *
*---------------------------------------------------------------------*
*       Hier werden den den Key-u. Datenfeldern der Int_S000 die      *
*       Werte der entsprechenden Felder aus Int_S032                  *
*       zugewiesen. Ausnahme sind die separat zugewiesenen Bestands-  *
*       felder.                                                       *
*---------------------------------------------------------------------*
FORM felder_int_s032_zuweisen.
  int_s000-mandt = int_s032-mandt.
  int_s000-ssour = int_s032-ssour.
  int_s000-vrsio = int_s032-vrsio.
  int_s000-werks = int_s032-werks.
  int_s000-lgort = int_s032-lgort.
  int_s000-matnr = int_s032-matnr.
  int_s000-dispo = int_s032-dispo.
  int_s000-mtart = int_s032-mtart.
  int_s000-matkl = int_s032-matkl.
  int_s000-dismm = int_s032-dismm.
  int_s000-gsber = int_s032-gsber.
  int_s000-spart = int_s032-spart.
  int_s000-bklas = int_s032-bklas.
  int_s000-basme = int_s032-basme.
  int_s000-hwaer = int_s032-hwaer.
  int_s000-eisbe = int_s032-eisbe.
ENDFORM.                    "felder_int_s032_zuweisen

*---------------------------------------------------------------------*
*       FORM PERIODE_ZUWEISEN                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM periode_zuweisen.
  CASE sav_speri.
    WHEN con_speri_sptag.
      int_s000-sptag = periotab-low.
    WHEN con_speri_spwoc.
      int_s000-spwoc = periotab-low.
    WHEN con_speri_spmon.
      int_s000-spmon = periotab-low.
    WHEN con_speri_spbup.
      int_s000-spbup = periotab-low.
  ENDCASE.
ENDFORM.                    "periode_zuweisen

*---------------------------------------------------------------------*
*       FORM BESTAENDE_ZUWEISEN                                       *
*---------------------------------------------------------------------*
*  1.   Zuweisung der Bestände in die Int_S000                        *
*---------------------------------------------------------------------*
FORM bestaende_zuweisen.
  int_s000-wbwbest = bw_wert_neu.
  int_s000-mbwbest = bw_bestand_neu.
  int_s000-mkobest = ko_bestand_neu.
ENDFORM.                    "bestaende_zuweisen

*---------------------------------------------------------------------*
*       FORM ZUSATZKENNZAHLEN_BESTAENDE                               *
*---------------------------------------------------------------------*
*       Berechnung Zusatzkennzahlen zum Bestand                       *
*---------------------------------------------------------------------*
FORM zusatzkennzahlen_bestaende.

*Gesamtbestand
  int_s000-gsbest = int_s000-mbwbest + int_s000-mkobest.
*Mittlerer Bestand bewertet
  int_s000-dbwbest =
  int_s000-mbwbest - ( ( int_s000-mzubb - int_s000-magbb ) / 2 ).
*Mittlerer Bestand Konsi
  int_s000-dkobest =
  int_s000-mkobest - ( ( int_s000-mzukb - int_s000-magkb ) / 2 ).
*Mittlerer Bestand Gesamt
  int_s000-dgsbest = int_s000-dbwbest + int_s000-dkobest.
*Mittlerer Bestandswert bewerteter Bestand
* if int_s000-mbwbest <> 0.
  int_s000-dwbwbest =
  int_s000-wbwbest - ( ( int_s000-wzubb - int_s000-wagbb ) / 2 ).
*   int_s000-dbwbest * int_s000-wbwbest / int_s000-mbwbest.
* endif.
ENDFORM.                    "zusatzkennzahlen_bestaende


*---------------------------------------------------------------------*
*       FORM ZUSATZKENNZAHLEN_BEWEGUNGEN                              *
*---------------------------------------------------------------------*
*       Berechnung Zusatzkennzahlen zu Lagerbewegungen                *
*---------------------------------------------------------------------*
FORM zusatzkennzahlen_bewegungen.
  IF int_s000-azubb <> 0.
*Mittlerer Zugang bewerteter Bestand
    int_s000-dzubb = int_s000-mzubb / int_s000-azubb.
*Wert mittlerer Zugang bewerteter Bestand
    int_s000-dzwbb = int_s000-wzubb / int_s000-azubb.
  ENDIF.
  IF int_s000-aagbb <> 0.
*Mittlerer Abgang bewerter Bestand
    int_s000-dagbb = int_s000-magbb / int_s000-aagbb.
*Wert mittlerer Abgang bewerteter Bestand
    int_s000-dawbb = int_s000-wagbb / int_s000-aagbb.
  ENDIF.
  IF int_s000-azukb <> 0.
*Mittlerer Zugang Konsi-Bestand
    int_s000-dzukb = int_s000-mzukb / int_s000-azukb.
  ENDIF.
  IF int_s000-aagkb <> 0.
*Mittlerer Abgang Konsi-Bestand
    int_s000-dagkb = int_s000-magkb / int_s000-aagkb.
  ENDIF.
  IF int_s000-agvbr <> 0.
*Mittlere Gesamtverbrauchsmenge
    int_s000-dgvbr = int_s000-mgvbr / int_s000-agvbr.
*Mittlerer Gesamtverbrauchswert
    int_s000-dwgvbr = int_s000-wgvbr / int_s000-agvbr.
  ENDIF.
* Mittlere Ungeplante Verbrauchsmenge
  IF int_s000-auvbr <> 0.
    int_s000-duvbr = int_s000-muvbr / int_s000-auvbr.
* Mittlerer ungeplanter Verbrauchswert
    int_s000-dwuvbr = int_s000-wuvbr / int_s000-auvbr.
  ENDIF.
ENDFORM.                    "zusatzkennzahlen_bewegungen


*---------------------------------------------------------------------*
*       FORM ZUSATZKENNZAHLEN_REICHWEITE                              *
*---------------------------------------------------------------------*
*       Berechnung der Kennzahlen zur Reichweite                      *
*       Diese wird in Tagen angegeben                                 *
*---------------------------------------------------------------------*
FORM zusatzkennzahlen_reichweite.

  DATA: zr_verbr TYPE f,
        zr_verbrwert TYPE f.

  zr_verbr     = int_s000-mgvbr / periodenlaenge.
  zr_verbrwert = int_s000-wgvbr / periodenlaenge.
*Reichweiten der Endbestände bezogen auf den Gesamtverbrauch pro Periode
  PERFORM berechne_zahl USING int_s000-mbwbest
                              zr_verbr
                     CHANGING int_s000-rwbewbest.
  PERFORM berechne_zahl USING int_s000-mkobest
                              zr_verbr
                     CHANGING int_s000-rwkobest.
  PERFORM berechne_zahl USING int_s000-gsbest
                              zr_verbr
                     CHANGING int_s000-rwgsbest.
*Reichweiten der mittl.Bestände bezogen auf den Gesamtverbrauch
  PERFORM berechne_zahl USING int_s000-dbwbest
                              int_s000-mgvbr
                     CHANGING int_s000-drwbwbest.
  PERFORM berechne_zahl USING int_s000-dkobest
                              int_s000-mgvbr
                     CHANGING int_s000-drwkobest.
  PERFORM berechne_zahl USING int_s000-dgsbest
                              int_s000-mgvbr
                     CHANGING int_s000-drwgsbest.
  PERFORM berechne_zahl USING int_s000-wbwbest
                              zr_verbrwert
                     CHANGING int_s000-rwwbwbest.
  int_s000-rwwgsbest = int_s000-rwwbwbest.
  PERFORM berechne_zahl USING int_s000-dwbwbest
                              zr_verbrwert
                     CHANGING int_s000-drwwbwbest.
  int_s000-drwwgsbest = int_s000-drwwbwbest.
ENDFORM.                    "zusatzkennzahlen_reichweite


*---------------------------------------------------------------------*
*       FORM ZUSATZKENNZAHLEN_UMSCHLAG                                *
*---------------------------------------------------------------------*
*       Berechnung der Kennzahlen zur Umschlagshäufigkeit             *
*---------------------------------------------------------------------*
FORM zusatzkennzahlen_umschlag.
  IF int_s000-mgvbr LT 0 AND int_s000-wgvbr LT 0.
*   Verbrauch Null bzw. negativ, somit ziehen die Initialwerte
    EXIT.
  ENDIF.
  IF int_s000-dbwbest LE 0.
* Bewerteter Bestand = 0 oder negativ
    int_s000-uhbwbest    =  999.
    int_s000-duhbwbest   =  999.
    int_s000-juhbwbest   =  999.
    int_s000-uhwbwbest   =  999.
    int_s000-duhwbwbest  =  999.
    int_s000-juhwbwbest  =  999.
    int_s000-juhwgsbest  =  999.
    int_s000-uhgsbest    =  999.
    int_s000-duhgsbest   =  999.
    int_s000-juhgsbest   =  999.
    int_s000-uhwgsbest   =  999.
  ENDIF.
  IF int_s000-dkobest LE 0.
* Konsibestand = 0 oder negativ
    int_s000-uhkobest    =  999.
    int_s000-juhkobest   =  999.
    int_s000-duhkobest   =  999.
    int_s000-juhkobest   =  999.
  ENDIF.
  IF int_s000-mgvbr GT 0 AND ( int_s000-dbwbest GT 0 OR
                                     int_s000-dkobest GT 0 ).
* Der genaue Umschlag kann errechnet werden.
    PERFORM umschlag_berechnen.
  ENDIF.
ENDFORM.                    "zusatzkennzahlen_umschlag

*---------------------------------------------------------------------*
*       FORM UMSCHLAG_BERECHNEN                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM umschlag_berechnen.
  DATA: zu_verbrjahr     TYPE f,
        zu_verbrwertjahr TYPE f.

  zu_verbrjahr     = int_s000-mgvbr * jahresperioden.
  zu_verbrwertjahr = int_s000-wgvbr * jahresperioden.

* Umschlagshäufigkeit-Gesamt bewerteter Bestand
  PERFORM berechne_zahl USING int_s000-mgvbr
                              int_s000-dbwbest
                     CHANGING int_s000-uhbwbest.
  int_s000-duhbwbest = int_s000-uhbwbest.
* Umschlagshäufigkeit-Gesamt  Konsi-Bestand
  PERFORM berechne_zahl USING int_s000-mgvbr
                              int_s000-dkobest
                     CHANGING int_s000-uhkobest.
  int_s000-duhkobest = int_s000-uhkobest.
* Umschlagshäufigkeit-Gesamt Gesamtbestand
  PERFORM berechne_zahl USING int_s000-mgvbr
                              int_s000-dgsbest
                     CHANGING int_s000-uhgsbest.
  int_s000-duhgsbest = int_s000-uhgsbest.

* Umschlagshäufigkeit-Gesamt Wert bewerteter Bestand
  PERFORM berechne_zahl USING int_s000-wgvbr
                              int_s000-dwbwbest
                     CHANGING int_s000-uhwbwbest.
  int_s000-duhwbwbest = int_s000-uhwbwbest.
  int_s000-uhwgsbest  = int_s000-uhwbwbest.
  int_s000-duhwgsbest = int_s000-uhwbwbest.
* Umschlagshäufigkeit des bewerteten Bestandes pro Jahr
  PERFORM berechne_zahl USING zu_verbrjahr
                              int_s000-dbwbest
                     CHANGING int_s000-juhbwbest.
  PERFORM berechne_zahl USING zu_verbrjahr
                              int_s000-dkobest
                     CHANGING int_s000-juhkobest.
  PERFORM berechne_zahl USING zu_verbrjahr
                              int_s000-dgsbest
                     CHANGING int_s000-juhgsbest.
* Umschlagshäufigkeit gesamt pro Jahr Wert bewerteter Bestand/Gesamtbst
  PERFORM berechne_zahl USING zu_verbrwertjahr
                              int_s000-dwbwbest
                     CHANGING int_s000-juhwbwbest.
  int_s000-juhwbwbest = int_s000-juhwgsbest.

ENDFORM.                    "umschlag_berechnen

*---------------------------------------------------------------------*
*       FORM SEL_ZEITRAUM_INITIALISIEREN                              *
*---------------------------------------------------------------------*
FORM sel_zeitraum_initialisieren
  USING si_mcexc TYPE c
        si_mcper TYPE c
        value(si_speri) TYPE tmc4-speri.

  FIELD-SYMBOLS: <sl_zeitr_sign>   TYPE c,
                 <sl_zeitr_option> TYPE c,
                 <sl_zeitr_high>   TYPE clike,
                 <sl_zeitr_low>    TYPE clike.

  DATA: si_hlp_bukrs       LIKE t001-bukrs,
        si_periv           LIKE mcs0-periv,
        si_von_datum       LIKE sy-datum,
        si_bis_datum       LIKE sy-datum,
        si_perioden        LIKE sy-tabix,
        si_perioden_jahre  LIKE sy-tabix,
        si_perioden_monate LIKE sy-tabix,
        si_endperiode(2)   TYPE n.

* Ranges initial füllen, damit DB-Optimizer DB-Index vollständig nutzt
  REFRESH: sl_sptag,
           sl_spwoc,
           sl_spmon,
           sl_spbup.

  CLEAR sl_sptag.
  sl_sptag-sign   = 'I'.
  sl_sptag-option = 'EQ'.
  APPEND sl_sptag.
  CLEAR sl_spmon.
  sl_spmon-sign   = 'I'.
  sl_spmon-option = 'EQ'.
  APPEND sl_spmon.
  CLEAR sl_spwoc.
  sl_spwoc-sign   = 'I'.
  sl_spwoc-option = 'EQ'.
  APPEND sl_spwoc.
  CLEAR sl_spbup.
  sl_spbup-sign   = 'I'.
  sl_spbup-option = 'EQ'.
  APPEND sl_spbup.

  CASE si_speri.
    WHEN con_speri_sptag.
      ASSIGN sl_sptag-option TO <sl_zeitr_option>.
      ASSIGN sl_sptag-sign   TO <sl_zeitr_sign>.
      ASSIGN sl_sptag-low    TO <sl_zeitr_low>.
      ASSIGN sl_sptag-high   TO <sl_zeitr_high>.
      REFRESH sl_sptag.
    WHEN con_speri_spwoc.
      ASSIGN sl_spwoc-option TO <sl_zeitr_option>.
      ASSIGN sl_spwoc-sign   TO <sl_zeitr_sign>.
      ASSIGN sl_spwoc-low    TO <sl_zeitr_low>.
      ASSIGN sl_spwoc-high   TO <sl_zeitr_high>.
      REFRESH sl_spwoc.
    WHEN con_speri_spmon.
      ASSIGN sl_spmon-option TO <sl_zeitr_option>.
      ASSIGN sl_spmon-sign   TO <sl_zeitr_sign>.
      ASSIGN sl_spmon-low    TO <sl_zeitr_low>.
      ASSIGN sl_spmon-high   TO <sl_zeitr_high>.
      REFRESH sl_spmon.
    WHEN con_speri_spbup.
      ASSIGN sl_spbup-option TO <sl_zeitr_option>.
      ASSIGN sl_spbup-sign   TO <sl_zeitr_sign>.
      ASSIGN sl_spbup-low    TO <sl_zeitr_low>.
      ASSIGN sl_spbup-high   TO <sl_zeitr_high>.
      REFRESH sl_spbup.
  ENDCASE.

  IF si_mcper = 0.
*   Vorschlagswert für Analysezeitraum ist nicht gewünscht
    EXIT.
  ENDIF.

  <sl_zeitr_sign>   = 'I'.
  <sl_zeitr_option> = 'BT'.

* Bis-Periode ermitteln
  IF si_mcexc = false.
*   aktuelle Periode wird nicht in die Bis-Periode einbezogen
    CASE si_speri.
      WHEN con_speri_sptag.
        si_bis_datum = sy-datum - 1.
      WHEN con_speri_spwoc.
        si_bis_datum = sy-datum - 7.
      WHEN con_speri_spmon.
        si_bis_datum = sy-datum.
      WHEN con_speri_spbup.
        si_bis_datum = sy-datum.
    ENDCASE.
    PERFORM periode_ermitteln(rmcs1000)
      USING    si_bis_datum
               hlp_mcinf
               si_hlp_bukrs
      CHANGING sl_sptag-high
               sl_spwoc-high
               sl_spmon-high
               sl_spbup-high
               si_periv.
    CASE si_speri.
      WHEN con_speri_spbup.
        IF NOT si_periv IS INITIAL.
*         Geschäftsjahresvariante ist zur Info-Struktur angegeben
          SELECT SINGLE * FROM  t009
                 WHERE  periv       = si_periv.
        ENDIF.
    ENDCASE.

    IF si_speri = con_speri_spmon OR
       si_speri = con_speri_spbup.
*     Sonderberechnung bei Buchungsperiode bzw. Monat
      IF si_speri = con_speri_spbup AND
         NOT si_periv IS INITIAL.
*         Geschäftsjahresvariante ist zur Info-Struktur angegeben =>
*         Endperiode berechnen
        si_endperiode = t009-anzbp.
      ELSE.
*       Monat bzw. Buchungsperiode ohne Geschäftsjahresvariante
        si_endperiode = '12'.
      ENDIF.
      CASE si_speri.
        WHEN con_speri_spmon.
          IF sl_spmon-high+4(2) = '01'.
*           Aktuelle Periode ist '01' => Vorperiode liegt im Vorjahr
            sl_spmon-high+4(2) = si_endperiode.
            sl_spmon-high(4) = sl_spmon-high(4) - 1.
          ELSE.
*           Vorperiode liegt im gleichen Jahr
            sl_spmon-high     = sl_spmon-high - 1.
          ENDIF.
        WHEN con_speri_spbup.
          IF sl_spbup-high+4(2) = '01'.
*           Aktuelle Periode ist '01' => Vorperiode liegt im Vorjahr
            sl_spbup-high+4(2) = si_endperiode.
            sl_spbup-high(4) = sl_spbup-high(4) - 1.
          ELSE.
*           Vorperiode liegt im gleichen Jahr
            sl_spbup-high     = sl_spbup-high - 1.
          ENDIF.
      ENDCASE.
    ENDIF.
  ELSE.
*   aktuelle Periode in die Bis-Periode einbeziehen
    si_bis_datum = sy-datum.
    PERFORM periode_ermitteln(rmcs1000)
      USING    si_bis_datum
               hlp_mcinf
               si_hlp_bukrs
      CHANGING sl_sptag-high
               sl_spwoc-high
               sl_spmon-high
               sl_spbup-high
               si_periv.
    CASE si_speri.
      WHEN con_speri_spbup.
        IF NOT si_periv IS INITIAL.
*         Geschäftsjahresvariante ist zur Info-Struktur angegeben
          SELECT SINGLE * FROM  t009
                 WHERE  periv       = si_periv.
        ENDIF.
    ENDCASE.
  ENDIF.

* Von-Zeitraum ermitteln
  IF si_mcper IS INITIAL.
    <sl_zeitr_low> = <sl_zeitr_high>.
  ELSE.
    IF si_speri = con_speri_spmon OR
       si_speri = con_speri_spbup.
*     Sonderberechnung bei Buchungsperiode bzw. Monat
      IF si_speri = con_speri_spbup AND
         NOT si_periv IS INITIAL.
*         Geschäftsjahresvariante ist zur Info-Struktur angegeben =>
*         Endperiode berechnen
        si_endperiode = t009-anzbp.
      ELSE.
*       Monat bzw. Buchungsperiode ohne Geschäftsjahresvariante
        si_endperiode = '12'.
      ENDIF.
      <sl_zeitr_low>     = <sl_zeitr_high>.
      si_perioden        = si_mcper - 1.
      si_perioden_jahre  = si_perioden DIV si_endperiode.
      si_perioden_monate = si_perioden MOD si_endperiode.
      CASE si_speri.
        WHEN con_speri_spmon.
          IF si_perioden_monate >= sl_spmon-low+4(2).
*           zusätzlicher Jahreswechsel
            sl_spmon-low+4(2) = si_endperiode + sl_spmon-low+4(2) -
                                si_perioden_monate.
            sl_spmon-low(4)   = sl_spmon-low(4) - si_perioden_jahre - 1.
          ELSE.
*           kein zusätzlicher Jahreswechsel
            sl_spmon-low+4(2) = sl_spmon-low+4(2) - si_perioden_monate.
            sl_spmon-low(4)   = sl_spmon-low(4) - si_perioden_jahre.
          ENDIF.
        WHEN con_speri_spbup.
          IF si_perioden_monate >= sl_spbup-low+4(2).
*           zusätzlicher Jahreswechsel
            sl_spbup-low+4(2) = si_endperiode + sl_spbup-low+4(2) -
                                si_perioden_monate.
            sl_spbup-low(4)   = sl_spbup-low(4) - si_perioden_jahre - 1.
          ELSE.
*           kein zusätzlicher Jahreswechsel
            sl_spbup-low+4(2) = sl_spbup-low+4(2) - si_perioden_monate.
            sl_spbup-low(4)   = sl_spbup-low(4) - si_perioden_jahre.
          ENDIF.
      ENDCASE.
    ELSE.
*     Periode Tag und Woche
      CASE si_speri.
        WHEN con_speri_sptag.
          si_von_datum = si_bis_datum - si_mcper + 1.
        WHEN con_speri_spwoc.
          si_von_datum = si_bis_datum - ( si_mcper - 1 ) * 7.
      ENDCASE.
      PERFORM periode_ermitteln(rmcs1000)
        USING    si_von_datum
                 hlp_mcinf
                 si_hlp_bukrs
      CHANGING sl_sptag-low
               sl_spwoc-low
               sl_spmon-low
               sl_spbup-low
               si_periv.
    ENDIF.
  ENDIF.
  CASE si_speri.
    WHEN con_speri_sptag.
      APPEND sl_sptag.
    WHEN con_speri_spwoc.
      APPEND sl_spwoc.
    WHEN con_speri_spmon.
      APPEND sl_spmon.
    WHEN con_speri_spbup.
      APPEND sl_spbup.
  ENDCASE.
ENDFORM.                    "sel_zeitraum_initialisieren

*&---------------------------------------------------------------------*
*&      Form  SELEKTIONSBILD_VORBEREITEN
*&---------------------------------------------------------------------*
FORM selektionsbild_vorbereiten.

  DATA: sv_selectoption_name(8). "Nicht auszublendende Select-option

  CASE sav_speri.
    WHEN con_speri_sptag.
      sv_selectoption_name = 'SL_SPTAG'.
    WHEN con_speri_spwoc.
      sv_selectoption_name = 'SL_SPWOC'.
    WHEN con_speri_spmon.
      sv_selectoption_name = 'SL_SPMON'.
    WHEN con_speri_spbup.
      sv_selectoption_name = 'SL_SPBUP'.
  ENDCASE.
  LOOP AT SCREEN.
    IF screen-group1 = 'PER'.
      IF NOT screen-name CS sv_selectoption_name.
        screen-active = off.
        screen-invisible = on.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " SELEKTIONSBILD_VORBEREITEN

*&---------------------------------------------------------------------*
*&      Form  ZEITR_KONVERTIEREN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zeitr_konvertieren.
  CASE sav_speri.
    WHEN con_speri_sptag.
      LOOP AT zeitr.
        MOVE-CORRESPONDING zeitr TO sl_sptag.
        APPEND sl_sptag.
      ENDLOOP.
    WHEN con_speri_spmon.
      LOOP AT zeitr.
        sl_spmon-low = zeitr-low.
        CLEAR sl_spmon-high.
        sl_spmon-option = zeitr-option.
        sl_spmon-sign = zeitr-sign.
        APPEND sl_spmon.
      ENDLOOP.
    WHEN con_speri_spwoc.
      LOOP AT zeitr.
        sl_spwoc-low = zeitr-low.
        CLEAR sl_spwoc-high.
        sl_spwoc-option = zeitr-option.
        sl_spwoc-sign = zeitr-sign.
        APPEND sl_spwoc.
      ENDLOOP.
    WHEN con_speri_spbup.
      LOOP AT zeitr.
        sl_spbup-low = zeitr-low.
        CLEAR sl_spbup-high.
        sl_spbup-option = zeitr-option.
        sl_spbup-sign = zeitr-sign.
        APPEND sl_spbup.
      ENDLOOP.
  ENDCASE.
ENDFORM.                               " ZEITR_KONVERTIEREN


*---------------------------------------------------------------------*
*       FORM berechne_zahl                                            *
*---------------------------------------------------------------------*
*       ergebnis = zaehler / nenner, max. 999                         *
*---------------------------------------------------------------------*
FORM berechne_zahl
  USING    u_zaehler TYPE numeric
           u_nenner  TYPE numeric
  CHANGING c_ergebnis TYPE p.
  DATA: l_quotient TYPE f.

  IF u_zaehler GT 0 AND u_nenner GT 0.
    l_quotient = u_zaehler / u_nenner.
    IF l_quotient GT 999.
      c_ergebnis = 999.
    ELSE.
      c_ergebnis = l_quotient.
    ENDIF.
  ELSEIF u_zaehler GT 0 AND u_nenner LE 0.
    c_ergebnis = 999.
  ELSE.
    CLEAR c_ergebnis.
  ENDIF.
ENDFORM.                    "berechne_zahl
*&---------------------------------------------------------------------*
*&      Form  check_berechtigung
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CON_MCAPP_BC  text
*----------------------------------------------------------------------*
FORM check_berechtigung USING cb_app LIKE tmcw3-mcapp.

  AUTHORITY-CHECK OBJECT 'M_NEUA_AUS' ID 'MCAPP' FIELD cb_app.
  IF sy-subrc <> 0.
*   Keine Berechtigung
    MESSAGE e619(m2) WITH sy-title.
  ENDIF.
ENDFORM.                               " check_berechtigung


*&---------------------------------------------------------------------*
*&      Form  KORREKTUR_BESTANDSWERT
*&---------------------------------------------------------------------*
* Bestandswerte werden von Lagerort blank auf andere Lagerorte         *
* konsistent verteilt
* Vorgehensweise:
* In Korrekturtabelle wird über Lagerort aggregiert. Daraus wird ein
* einheitlicher Preis berechnet
* Bestandswert = Bestandsmenge * Preis
* Zugangs- und Abgangswert werden konsistent berechnet: Wenn keine
* Preisänderung stattgefunden hat, einfach Menge * Preis, ansonsten
* wird Zugangsmenge bei Preiserhöhung bzw. Abgangsmenge bei Preis-
* minderung so geändert, dass Bestandswert = Bestandswert vorherige
* Periode + Zugangswert - Abgangswert
*----------------------------------------------------------------------*
FORM korrektur_bestandswert TABLES inttab STRUCTURE int_s000.

  DATA: BEGIN OF l_korr_tab OCCURS 0,
          matnr   LIKE s039-matnr,
          werks   LIKE s039-werks,
          spmon   LIKE s039-spmon,
          sptag   LIKE s039-sptag,
          spwoc   LIKE s039-spwoc,
          spbup   LIKE s039-spbup,
          mbwbest LIKE s039-mbwbest,
          wbwbest LIKE s039-wbwbest,
          wzubb   LIKE s039-wzubb,
          wzubb0  LIKE s039-wzubb,     " Zugang ohne Menge
          wagbb   LIKE s039-wagbb,
          wagbb0  LIKE s039-wagbb,     " Abgang ohne Menge
          mzubb   LIKE s039-mzubb,
          magbb   LIKE s039-magbb,
          mgvbr   LIKE s039-mgvbr,
          wgvbr   LIKE s039-wgvbr,
          wuvbr   LIKE s039-wuvbr,
          muvbr   LIKE s039-muvbr,
          anz     TYPE i,
          flg_mbwbest,
          index_mbwbest TYPE i,
        END   OF l_korr_tab.

  DATA: l_preis       TYPE f,
        l_preis_old   TYPE f,
        l_preis_vbr   TYPE f,
        l_int_s000    LIKE int_s000,
        l_tabix       LIKE sy-tabix,
        l_tabix_mbew  LIKE sy-tabix,
        l_mbwbest_old LIKE s039-mbwbest,
        l_mbwbest_lg  LIKE s039-mbwbest,
        l_delta_1     LIKE s039-wzubb,
        l_delta_2     LIKE l_delta_1,
        l_sum         LIKE s039-wbwbest,
        l_anz         TYPE i.

  FIELD-SYMBOLS: <int_s000> LIKE LINE OF int_s000.

  CHECK va EQ true.

* Lagerort space soll erster Eintrag sein, damit auf diesen keine
* Rundungsdifferenz gebucht wird.
  CASE sav_speri.
    WHEN con_speri_spmon.
      SORT inttab BY matnr werks spmon lgort.
    WHEN con_speri_sptag.
      SORT inttab BY matnr werks sptag lgort.
    WHEN con_speri_spwoc.
      SORT inttab BY matnr werks spwoc lgort.
    WHEN con_speri_spbup.
      SORT inttab BY matnr werks spbup lgort.
  ENDCASE.


  LOOP AT inttab ASSIGNING <int_s000>.
    IF NOT l_int_s000 IS INITIAL AND
      ( l_int_s000-matnr NE <int_s000>-matnr OR
        l_int_s000-werks NE <int_s000>-werks OR
        l_int_s000-spmon NE <int_s000>-spmon OR
        l_int_s000-sptag NE <int_s000>-sptag OR
        l_int_s000-spwoc NE <int_s000>-spwoc OR
        l_int_s000-spbup NE <int_s000>-spbup ).
      APPEND l_korr_tab.
      CLEAR  l_korr_tab.
    ENDIF.
    IF l_korr_tab IS INITIAL.
      MOVE-CORRESPONDING <int_s000> TO l_korr_tab.
      l_korr_tab-anz = 1.
    ELSE.
      ADD <int_s000>-mbwbest TO l_korr_tab-mbwbest.
      ADD <int_s000>-wbwbest TO l_korr_tab-wbwbest.
      ADD <int_s000>-wzubb   TO l_korr_tab-wzubb.
      ADD <int_s000>-wagbb   TO l_korr_tab-wagbb.
      ADD <int_s000>-mzubb   TO l_korr_tab-mzubb.
      ADD <int_s000>-magbb   TO l_korr_tab-magbb.
      ADD <int_s000>-mgvbr   TO l_korr_tab-mgvbr.
      ADD <int_s000>-wgvbr   TO l_korr_tab-wgvbr.
      ADD <int_s000>-muvbr   TO l_korr_tab-muvbr.
      ADD <int_s000>-wuvbr   TO l_korr_tab-wuvbr.
      ADD 1                  TO l_korr_tab-anz.
    ENDIF.
    IF NOT <int_s000>-mbwbest IS INITIAL.
      l_korr_tab-flg_mbwbest = true.
      MOVE l_korr_tab-anz TO l_korr_tab-index_mbwbest.
    ENDIF.

    l_int_s000 = <int_s000>.
  ENDLOOP.

  IF NOT l_korr_tab IS INITIAL.
    APPEND l_korr_tab.
  ENDIF.

  CLEAR l_korr_tab.
  CLEAR l_tabix.
  LOOP AT inttab ASSIGNING <int_s000>.
    sy-subrc = 0.
* Korrektureintrag, Preis und alten Preis ermitteln
    WHILE sy-subrc EQ 0 AND
        ( l_korr_tab-matnr NE <int_s000>-matnr OR
          l_korr_tab-werks NE <int_s000>-werks OR
          l_korr_tab-spmon NE <int_s000>-spmon OR
          l_korr_tab-sptag NE <int_s000>-sptag OR
          l_korr_tab-spwoc NE <int_s000>-spwoc OR
          l_korr_tab-spbup NE <int_s000>-spbup ).
      ADD 1 TO l_tabix.
      READ TABLE l_korr_tab INDEX l_tabix.
      IF sy-subrc NE 0.
* Sollte nicht vorkommen können
        EXIT.
      ENDIF.
      CLEAR l_sum.
      CLEAR l_anz.
* Preis für Merkmalskombination ermitteln
* Ist das überhaupt notwendig?
      IF l_korr_tab-mbwbest IS INITIAL AND
         l_korr_tab-mzubb   IS INITIAL AND
         l_korr_tab-magbb   IS INITIAL AND
         l_korr_tab-mgvbr   IS INITIAL AND
         l_korr_tab-muvbr   IS INITIAL AND
         l_korr_tab-flg_mbwbest NE true.
        CLEAR l_preis.
        CLEAR l_preis_old.
* 1. Versuch: Wert Bestand / Menge Bestand
      ELSEIF l_korr_tab-mbwbest NE 0.
        l_preis = l_korr_tab-wbwbest / l_korr_tab-mbwbest.
      ELSE.
* 2. Versuch: Berechne Bestand der Vorperiode, um aus diesem
* den Preis zu ermitteln
        l_mbwbest_old = l_korr_tab-mbwbest - l_korr_tab-mzubb +
                        l_korr_tab-magbb.
        IF NOT l_mbwbest_old IS INITIAL.
* Vorperiode hatte Bestand
          l_preis = ( l_korr_tab-wbwbest - l_korr_tab-wzubb -
                      l_korr_tab-wzubb0  + l_korr_tab-wagbb +
                      l_korr_tab-wagbb0 ) / l_mbwbest_old.
        ELSE.
* Vorperiode hatte keinen Bestand
* 3. Versuch: verwende Verbrauchswerte und -mengen als Basis
          IF NOT l_korr_tab-mgvbr IS INITIAL AND
             NOT l_korr_tab-wgvbr IS INITIAL.
            l_preis = l_korr_tab-wgvbr / l_korr_tab-mgvbr.
          ELSEIF NOT l_korr_tab-muvbr IS INITIAL AND
                 NOT l_korr_tab-wuvbr IS INITIAL.
* 4. Versuch: verwende ungeplante Verbräuche
            l_preis = l_korr_tab-wuvbr / l_korr_tab-muvbr.
          ELSE.
* Letzter Versuch: verwende Standardpreis
            READ TABLE xmbew WITH KEY matnr = l_korr_tab-matnr
                                      bwkey = l_korr_tab-werks
                                      bwtar = space
                                      BINARY SEARCH.
            l_tabix_mbew = sy-tabix.
            IF sy-subrc NE 0.
              SELECT SINGLE * FROM mbew
                WHERE matnr = l_korr_tab-matnr
                  AND bwkey = l_korr_tab-werks
                  AND bwtar = space.
              IF sy-subrc NE 0.
                CLEAR l_preis.
                CLEAR xmbew.
              ELSE.
* sortiert einfügen
                INSERT mbew INTO xmbew INDEX l_tabix_mbew.
                xmbew = mbew.
              ENDIF.
            ENDIF.                     " subrc nach READ TABLE
            IF NOT xmbew IS INITIAL.
              IF xmbew-lbkum NE 0.
                l_preis   = xmbew-salk3 / xmbew-lbkum.
              ELSE.
                IF xmbew-vprsv = 'S'.
                  l_preis = xmbew-stprs / xmbew-peinh.
                ELSE.
                  l_preis = xmbew-verpr / xmbew-peinh.
                ENDIF.
              ENDIF.
            ENDIF.                     " xmbew gefüllt
          ENDIF.                       " letzter Versuch
        ENDIF. " Bestand Vorperiode (nicht) vorhanden
      ENDIF.                           " 1. Versuch

      IF l_korr_tab-wzubb0 IS INITIAL AND
       l_korr_tab-wagbb0 IS INITIAL.
* prüfen, ob wirklich keine verteilung notwendig
        l_delta_1 = ( l_korr_tab-mzubb - l_korr_tab-magbb ) * l_preis.
        l_delta_2 = l_korr_tab-wzubb - l_korr_tab-wagbb.
        IF l_delta_1 NE l_delta_2.
          l_korr_tab-wzubb = l_korr_tab-mzubb * l_preis.
          l_korr_tab-wagbb = l_korr_tab-magbb * l_preis.
          IF l_delta_1 LT l_delta_2.
            l_korr_tab-wzubb0 = l_delta_2 - l_delta_1.
          ELSE.
            l_korr_tab-wagbb0 = l_delta_1 - l_delta_2.
          ENDIF.
        ENDIF.
      ENDIF.

* Noch alten Preis berechnen, falls Verteilung notwendig
      IF NOT l_korr_tab-wzubb0 IS INITIAL OR
         NOT l_korr_tab-wagbb0 IS INITIAL.
        l_mbwbest_old = l_korr_tab-mbwbest - l_korr_tab-mzubb +
                        l_korr_tab-magbb.
        IF NOT l_mbwbest_old IS INITIAL.
* alter Preis: alter wbwbest / alter mbwbest
          l_preis_old = ( l_korr_tab-wbwbest - l_korr_tab-wzubb -
                          l_korr_tab-wzubb0  + l_korr_tab-wagbb +
                          l_korr_tab-wagbb0 ) / l_mbwbest_old.
        ELSE.
          l_preis_old = l_preis.
        ENDIF.
      ENDIF.
      sy-subrc = 0.
    ENDWHILE.

    IF sy-subrc EQ 0.
      ADD 1 TO l_anz.
* Wert Bestand korrigieren
      IF l_korr_tab-flg_mbwbest EQ true.
        IF l_anz EQ l_korr_tab-index_mbwbest.  " letzter Eintrag MBWBEST
          <int_s000>-wbwbest = l_korr_tab-wbwbest - l_sum.
        ELSE.
          <int_s000>-wbwbest = <int_s000>-mbwbest * l_preis.
        ENDIF.
      ELSEIF l_korr_tab-wbwbest LT 20 AND
             l_korr_tab-wbwbest GT -20.
        CLEAR <int_s000>-wbwbest.
      ENDIF.
      ADD <int_s000>-wbwbest TO l_sum.
      IF l_korr_tab-wzubb0 IS INITIAL.
* Keine Verteilung für Zugang notwendig
        IF l_korr_tab-mzubb NE 0.
          <int_s000>-wzubb = <int_s000>-mzubb * l_preis.
        ELSE.
          CLEAR <int_s000>-wzubb.
        ENDIF.
      ENDIF.
      IF l_korr_tab-wagbb0 IS INITIAL.
* Keine Verteilung für Abgang notwendig
        IF l_korr_tab-magbb NE 0.
          <int_s000>-wagbb = <int_s000>-magbb * l_preis.
        ELSE.
          CLEAR <int_s000>-wagbb.
        ENDIF.
      ENDIF.
      IF NOT l_korr_tab-wzubb0 IS INITIAL.
* Verteilung für Zugang notwendig
        l_mbwbest_lg = <int_s000>-mbwbest - <int_s000>-mzubb +
                       <int_s000>-magbb.
        <int_s000>-wzubb = <int_s000>-wbwbest + <int_s000>-wagbb -
                           l_mbwbest_lg * l_preis_old.
      ENDIF.
      IF NOT l_korr_tab-wagbb0 IS INITIAL.
* Verteilung für Abgang notwendig
        l_mbwbest_lg = <int_s000>-mbwbest - <int_s000>-mzubb +
                       <int_s000>-magbb.
        <int_s000>-wagbb = l_mbwbest_lg * l_preis_old -
                           <int_s000>-wbwbest + <int_s000>-wzubb.
      ENDIF.

* Verbrauchskennzahlen korrigieren, keine Verteilung notwendig
      IF l_korr_tab-mgvbr NE 0.
        l_preis_vbr = l_korr_tab-wgvbr / l_korr_tab-mgvbr.
         IF l_preis_vbr LT 0.
            l_preis_vbr = L_preis.
         ENDIF.
        <int_s000>-wgvbr = <int_s000>-mgvbr * l_preis_vbr.
      ELSEIF l_korr_tab-wgvbr IS INITIAL.
        CLEAR <int_s000>-wgvbr.
      ENDIF.
      IF l_korr_tab-muvbr <> 0.
        l_preis_vbr = l_korr_tab-wuvbr / l_korr_tab-muvbr.
         IF l_preis_vbr LT 0.
            l_preis_vbr = L_preis.
         ENDIF.
        <int_s000>-wuvbr = <int_s000>-muvbr * l_preis_vbr.
      ELSEIF l_korr_tab-wuvbr IS INITIAL.
        CLEAR <int_s000>-wuvbr.
      ENDIF.
    ENDIF.
  ENDLOOP.                             " inttab

  CLEAR   l_korr_tab.
  REFRESH l_korr_tab.

* Lagerort ' ' wird gelöscht, wenn Bestaende = 0 und Werte
  DELETE inttab
    WHERE lgort   = space
      AND mbwbest = 0
      AND wbwbest = 0
      AND mzubb   = 0
      AND wzubb   = 0
      AND magbb   = 0
      AND wagbb   = 0
      AND mgvbr   = 0
      AND wgvbr   = 0
      AND muvbr   = 0
      AND wuvbr   = 0
      AND eisbe   = 0.

  IF NOT sl_lgort[] IS INITIAL.
    DELETE inttab
      WHERE NOT lgort IN sl_lgort.
  ENDIF.

  REFRESH xmbew.

ENDFORM.                               " KORREKTUR_BESTANDSWERT

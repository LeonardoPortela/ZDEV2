
************************************************************************
* Includebaustein RFFORI10 zum Zahlungsträgerdruckprogramm RFFOM100    *
* Unterprogrammen für den Datenträgeraustausch MT100                   *
* Include RFFORI10, used in the payment print program RFFOM100         *
* with subroutines for MT100                                           *
*                                                                      *
* subroutine                         called by report / in subroutine  *
* -------------------------------------------------------------------  *
* MT100                                                      RFFOM100  *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* FORM MT100                                                           *
*----------------------------------------------------------------------*
* Ausgabe der MT100-Files                                              *
* gerufen von END-OF-SELECTION (RFFOM100)                              *
*----------------------------------------------------------------------*
* program produces MT100-files                                         *
* called by END-OF-SELECTION (RFFOM100)                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM mt100.
*----------------------------------------------------------------------*
* Vorbereitung zum Datenträgeraustausch                                *
* preparations for DME                                                 *
*----------------------------------------------------------------------*

* Sortieren des Datenbestandes unter Beachtung von Gut-/Lastschrift
* sort of extract considering incoming and outgoing payments
  DATA: up_len              TYPE i,
        up_waers(3)         TYPE c,
        up_auftr_geb_1(35),
        up_auftr_geb_2(35),
        up_auftr_geb_3(35),
        up_auftr_geb_4(35),
        up_zahl_empf_1(35),
        up_zahl_empf_2(35),
        up_zahl_empf_3(35),
        up_zahl_empf_4(35),
        up_lfdnr(8) TYPE n,
        up_filecnt  TYPE i,
        flg_unix(1) TYPE c,
        _open_fi(1) TYPE c.

*---------------------------------------------------------------------*
  SORT BY
    reguh-zbukr                        "paying company code
    reguh-ubnks                        "country of house bank
    reguh-ubnky                        "bank key (for sort)
    reguh-ubnkl                        "bank number of house bank
    reguh-rzawe                        "X - incoming payment
    reguh-ubknt                        "account number at house bank
    reguh-zbnks                        "country of payee's bank
    reguh-zbnky                        "bank key (for sort)
    reguh-zbnkl                        "bank number of payee's bank
    reguh-zbnkn                        "account number of payee
    reguh-lifnr                        "creditor number
    reguh-kunnr                        "debitor number
    reguh-empfg                        "payee is CPD / alternative payee
    reguh-vblnr                        "payment document number
    hlp_sortp1                         "sort field for single items
    hlp_sortp2                         "sort field for single items
    hlp_sortp3                         "sort field for single items
    regup-belnr.                       "invoice document number

* Dateiformat bestimmen
  IF t042ofi-formt IS INITIAL.
    hlp_dtfor      = 'MT100'.
    hlp_dtfor_long = 'MT100'.
  ELSE.
    hlp_dtfor      = t042ofi-formt.
    hlp_dtfor_long = t042ofi-formt.
  ENDIF.

* Falls kein TemSe-Eintrag und falls kein Dateiname angegeben, Namen
* der sequentiellen Files vorbelegen: DTAUS0.Datum.Uhrzeit.lfdNr
* If no file-name is specified and no name will be generated later
* (because of TemSe), a new name is generated here: DTAUS0.Date.Time.nn
  IF par_unix NE space.
    flg_unix = 1.
  ENDIF.
  IF hlp_temse NA par_dtyp.            "Kein TemSe-Format / No TemSe
    IF par_unix EQ space.              "kein Name   / unspecified name
      par_unix    = hlp_dtfor.
      par_unix+6  = '.'.
      WRITE sy-datum TO par_unix+7(6) DDMMYY.
      par_unix+13 = '.'.
      par_unix+14 = sy-uzeit.
      par_unix+20 = '.'.
    ELSE.
      IF par_cbxx IS INITIAL.          "Einzelzahlung
        CLEAR up_filecnt.
        LOOP AT dta_filecnt.
          up_filecnt = up_filecnt + dta_filecnt-anzahl.
        ENDLOOP.
        dta_filecnt-anzahl = up_filecnt.
      ELSE.                            "Sammelzahlung
        DESCRIBE TABLE dta_filecnt LINES dta_filecnt-anzahl.
      ENDIF.

      CALL FUNCTION 'GET_SHORTKEY_FOR_FEBKO'
           EXPORTING
                i_tname             = 'MT100'
                i_anznr             = dta_filecnt-anzahl
           IMPORTING
                e_kukey             = up_lfdnr
           EXCEPTIONS
                febkey_update_error = 1.

      up_lfdnr = up_lfdnr - dta_filecnt-anzahl.
      IF sy-subrc = 1.
        IF sy-batch EQ space.
          MESSAGE a228 WITH 'FEBKEY'.
        ELSE.
          MESSAGE s228 WITH 'FEBKEY'.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  cnt_filenr = 0.

  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            tabname = 'DTAM100'
       TABLES
            nametab = nametab.

*----------------------------------------------------------------------*
* Abarbeiten der extrahierten Daten                                    *
* loop at extracted data                                               *
*----------------------------------------------------------------------*
  LOOP.

*-- Neuer zahlender Buchungskreis --------------------------------------
*-- new paying company code --------------------------------------------
    AT NEW reguh-zbukr.

      PERFORM buchungskreis_daten_lesen.

    ENDAT.                             "AT NEW REGUH-ZBUKR


*-- Neue Hausbank ------------------------------------------------------
*-- new house bank -----------------------------------------------------
    AT NEW reguh-ubnkl.

      PERFORM hausbank_daten_lesen.
      IF NOT par_cbxx IS INITIAL.
        PERFORM zusatzfeld_fuellen USING *regut-dtkey 'D  '.
        IF hlp_temse NA par_dtyp AND   "Kein TemSe-Format / No TemSe
           flg_unix NE space.          "kein Name   / unspecified name
          PERFORM datei_oeffnen_1 USING up_lfdnr.
          up_lfdnr = up_lfdnr + 1.
        ELSE.
          PERFORM datei_oeffnen.
        ENDIF.

*------ Prepare Open FI und User-Exit for multi payments
        CLEAR dta_filecnt.
        dta_filecnt-zbukr = reguh-zbukr.
        dta_filecnt-ubnks = reguh-ubnks.
        dta_filecnt-ubnkl = reguh-ubnkl.
        READ TABLE dta_filecnt.
        dtam100s-s00   = dta_filecnt-szbnkn.
        dtam100s-s01   = dta_filecnt-svbetr.
        dtam100s-s02   = dta_filecnt-anzahl.
        dtam100s-s03   = hlp_resultat.
        dtam100s-s04   = par_sbnk.     "sending bank of MT101
        dtam100h-xcrlf_supp = space.

*       Open FI / BTE (multi payments)
        IF par_mofi NE space.
          IF NOT t042ofi-xactive1 IS INITIAL.
            REFRESH tab_sum_per_currency.
            LOOP AT tab_sum_per_currency_ext
                               WHERE zbukr EQ reguh-zbukr
                                 AND ubnks EQ reguh-ubnks
                                 AND ubnky EQ reguh-ubnky.
              tab_sum_per_currency = tab_sum_per_currency_ext.
              APPEND tab_sum_per_currency.
              DELETE tab_sum_per_currency_ext.
            ENDLOOP.
            CALL FUNCTION 'OPEN_FI_PERFORM_00002010_P'
                 EXPORTING
                      i_format           = t042ofi-formt
                      i_reguh            = reguh
                      i_dtam100s         = dtam100s
                      i_dtam100h         = dtam100h
                      i_cbxx             = par_cbxx
                 IMPORTING
                      e_dtam100h         = dtam100h
                 TABLES
                      t_sum_per_currency = tab_sum_per_currency
                 EXCEPTIONS
                      no_add_on_found    = 1.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.
              _open_fi = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

*       User-Exit for header (multi payments)
        PERFORM exit_901(rffoexit)
                USING reguh
                      dtam100s
                      dtam100h
                      par_cbxx
                      up_usrex.
        IF NOT up_usrex IS INITIAL OR  "modified by user
           NOT _open_fi IS INITIAL.
           *regut-usrex = up_usrex.
          CLEAR: up_usrex, _open_fi.
          IF dtam100h-h01 IS INITIAL.
            up_len = strlen( dtam100h-h00 ).
          ELSE.
            up_len = dtam100h-h01.
          ENDIF.
          IF up_len GT 0.
            IF NOT dtam100h-xcrlf_supp IS INITIAL.
              PERFORM store_on_file USING dtam100h-h00(up_len).
            ELSE.
              PERFORM store_on_file USING:
                      dtam100h-h00(up_len), hlp_crlf.
            ENDIF.
          ENDIF.
          CLEAR dtam100h.
        ENDIF.

      ENDIF.

*     Lesen des Default-Weisungsschlüssels der Hausbank
*     Read parameters in T012D
      SELECT SINGLE * FROM t012d
        WHERE bukrs EQ reguh-zbukr
        AND   hbkid EQ reguh-hbkid.
      IF sy-subrc NE 0.
        CLEAR t012d.
      ENDIF.

      CLEAR sum_regut.

    ENDAT.


*-- Neuer Zahlweg ------------------------------------------------------
*-- new payment method -------------------------------------------------
    AT NEW reguh-rzawe.

      PERFORM zahlweg_daten_lesen.

    ENDAT.


*-- Neue Empfängerbank -------------------------------------------------
*-- new bank of payee --------------------------------------------------
    AT NEW reguh-zbnkl.

      PERFORM empfbank_daten_lesen.

    ENDAT.


*-- Neue Kontonummer bei der Empfängerbank------------------------------
*-- new bank account number of payee -----------------------------------
    AT NEW reguh-zbnkn.

      hlp_zbnkn = reguh-zbnkn.

    ENDAT.


*-- Neue Zahlungsbelegnummer -------------------------------------------
*-- new payment document number ----------------------------------------
    AT NEW reguh-vblnr.

      IF par_cbxx IS INITIAL.          "Einzelzahlung
        PERFORM zusatzfeld_fuellen USING *regut-dtkey 'D  '.
        IF hlp_temse NA par_dtyp AND   "Kein TemSe-Format / No TemSe
           flg_unix NE space.          "kein Name   / unspecified name
          PERFORM datei_oeffnen_1 USING up_lfdnr.
          up_lfdnr = up_lfdnr + 1.
        ELSE.
          PERFORM datei_oeffnen.
        ENDIF.
        dtam100h-xcrlf_supp = space.

*------ Open FI / BTE and User-Exit for single payments
*       Update tab_sum_per_currency for single payments
        REFRESH tab_sum_per_currency.
        tab_sum_per_currency-waers = reguh-waers.
        tab_sum_per_currency-rwbtr = reguh-rwbtr.
        APPEND tab_sum_per_currency.
*       Update DTAM100S for single payments
        dtam100s-s00 = reguh-zbnkn.
        PERFORM dta_vorkomma(rffod__l) USING reguh-waers reguh-rwbtr.
        dtam100s-s01 = spell-number.
        dtam100s-s02 = 1.
        dtam100s-s03 = hlp_resultat.
        dtam100s-s04 = par_sbnk.       "Sending bank of MT101

*       Open-FI / BTE (single payments)
        IF par_mofi NE space.
          IF NOT t042ofi-xactive1 IS INITIAL.
            CALL FUNCTION 'OPEN_FI_PERFORM_00002010_P'
                 EXPORTING
                      i_format           = t042ofi-formt
                      i_reguh            = reguh
                      i_dtam100s         = dtam100s
                      i_dtam100h         = dtam100h
                      i_cbxx             = par_cbxx
                 IMPORTING
                      e_dtam100h         = dtam100h
                 TABLES
                      t_sum_per_currency = tab_sum_per_currency
                 EXCEPTIONS
                      no_add_on_found    = 1.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.
              _open_fi = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

*       User-Exit for header (single payments)
        PERFORM exit_901(rffoexit)
                USING reguh
                      dtam100s
                      dtam100h
                      par_cbxx
                      up_usrex.
        IF NOT up_usrex IS INITIAL OR  "modified by user
           NOT _open_fi IS INITIAL.
           *regut-usrex = up_usrex.
          CLEAR: up_usrex, _open_fi.
          IF dtam100h-h01 IS INITIAL.
            up_len = strlen( dtam100h-h00 ).
          ELSE.
            up_len = dtam100h-h01.
          ENDIF.
          IF up_len GT 0.
            IF NOT dtam100h-xcrlf_supp IS INITIAL.
              PERFORM store_on_file USING dtam100h-h00(up_len).
            ELSE.
              PERFORM store_on_file USING:
                      dtam100h-h00(up_len), hlp_crlf.
            ENDIF.
          ENDIF.
          CLEAR dtam100h.
        ENDIF.

        CLEAR sum_regut.
      ENDIF.

      PERFORM zahlungs_daten_lesen.
      PERFORM summenfelder_initialisieren.
      PERFORM belegdaten_schreiben.
      SET LANGUAGE hlp_sprache.        " Buchungskreis-/Empfängersprache
      IF sy-subrc <> 0.
        SET LANGUAGE sy-langu.         " Anmeldesprache
      ENDIF.

*     Verwendungszweck auf Segmenttext untersuchen
*     examine whether note to payee has to be filled with segment text
      flg_sgtxt = 0.
      IF text-703 CS '&SGTXT'.
        flg_sgtxt = 1.                 "Global für Segmenttext existiert
      ENDIF.                           "global for segment text exists

*     Weisungsschlüssel lesen
*     Read instruction key
      IF NOT ( t012d-dtaws IS INITIAL AND reguh-dtaws IS INITIAL ).
        PERFORM weisungsschluessel_lesen.
      ELSE.
        CLEAR t015w.
      ENDIF.

      up_auftr_geb_1  = regud-aust1.   "Name des Auftraggebers
      IF regud-abstx EQ space.
        up_auftr_geb_2 = regud-aust2.
        up_auftr_geb_3 = regud-aust3.
        up_auftr_geb_4 = regud-austo.
      ELSE.
        up_auftr_geb_2 = regud-austo.
        up_auftr_geb_3 = regud-abstx.
        up_auftr_geb_4 = regud-absor.
      ENDIF.

      up_zahl_empf_1 = reguh-koinh.
      IF reguh-koinh EQ reguh-znme1 AND
         NOT reguh-znme2 IS INITIAL AND
         hlp_laufk NE 'P'.
        up_zahl_empf_2 = reguh-znme2.
      ELSE.
        CLEAR up_zahl_empf_2.
      ENDIF.
      up_zahl_empf_3 = regud-zpfst.
      up_zahl_empf_4 = regud-zplor.
      CLEAR up_wschl.
*     interne Tabelle DTA_MT100 initialisieren
*     initialize internal table DTA_MT100
      PERFORM mt100_init.

*     Interne Tabelle DTA_MT100 füllen
*     fill internal table DTA_MT100
      PERFORM get_value_date.   " Set reguh-valut, if initial
      PERFORM isocode_umsetzen USING reguh-waers up_waers.
      PERFORM put_mt100 USING: '20'    reguh-vblnr     1,
                               '32A'   reguh-valut     1,
                               '32A'   up_waers        2,
                               '32A'   reguh-rwbtr     3,
                               '50_1'  up_auftr_geb_1  1,
                               '50_2'  up_auftr_geb_2  1,
                               '50_3'  up_auftr_geb_3  1,
                               '50_4'  up_auftr_geb_4  1,
                               '53_1'  reguh-ubknt     1.
      IF NOT reguh-zswif IS INITIAL.
        PERFORM put_mt100 USING '57A' reguh-zswif 1.
      ELSEIF NOT reguh-zbnkl IS INITIAL.
        PERFORM put_mt100 USING '57A' reguh-zbnkl 2.
      ELSE.
        PERFORM put_mt100 USING: '57_1' bnka-banka 1,
                                 '57_2' bnka-stras 1,
                                 '57_3' bnka-ort01 1,
                                 '57_4' bnka-provz 1.
      ENDIF.
      PERFORM put_mt100 USING: '59_1'  reguh-zbnkn     1,
                               '59_2'  up_zahl_empf_1  1,
                               '59_3'  up_zahl_empf_2  1,
                               '59_4'  up_zahl_empf_3  1,
                               '59_5'  up_zahl_empf_4  1,
                               '71A'   t015w-dtkvs     1,
                               '72_1'  t015w-dtws1     1,
                               '72_2'  t015w-dtws2     1,
                               '72_3'  t015w-dtws3     1,
                               '72_4'  t015w-dtws4     1,
                               '99'    '-'             1.

*     Fill DME fields for sender's and receiver's correspondent
*     and intermediary
      CALL FUNCTION 'FI_GET_CORRESP_INTERMED_BANKS'
           EXPORTING
                i_reguh     = reguh
                i_dtaformat = hlp_dtfor_long
           TABLES
                t_dtamt100  = dta_mt100.


*     Prüfung, ob Avishinweis erforderlich
*     check if advice note is necessary
      IF flg_sgtxt = 1.
        cnt_zeilen = reguh-rpost + reguh-rtext.
      ELSE.
        cnt_zeilen = reguh-rpost.
      ENDIF.
      CLEAR dta_zeilen.
      REFRESH tab_dtam100v.
      IF cnt_zeilen GT par_zeil.       "Avishinweis ausgeben
                                       "print advice note
        PERFORM dta_erweiterungsteil USING text-704.
        PERFORM dta_erweiterungsteil USING text-705.
        ADD 1 TO cnt_hinweise.
        dtam100-xavis_req = 'X'.
      ELSE.
        dtam100-xavis_req = ' '.
      ENDIF.

    ENDAT.


*-- Verarbeitung der Einzelposten-Informationen ------------------------
*-- single item information --------------------------------------------
    AT daten.

      PERFORM einzelpostenfelder_fuellen.

*     Externe Belegnummer mit interner füllen, falls externe leer ist
*     fill external doc.no. with internal, if external is empty
      IF regup-xblnr EQ space.
        regup-xblnr = regup-belnr.
      ENDIF.

*     Ausgabe der Einzelposten, falls kein Avishinweis augegeben wurde
*     single item information if no advice note
      IF cnt_zeilen LE par_zeil.
        IF hlp_laufk NA 'JP'           "keine Rechungsinfo bei HR und IS
            AND regup-vertn EQ space.  "HR/IS: no invoice information
          PERFORM dta_erweiterungsteil USING text-702.
        ENDIF.
        IF flg_sgtxt = 1 AND regup-sgtxt NE space.
          PERFORM dta_erweiterungsteil USING text-703.
        ENDIF.
      ENDIF.

      PERFORM summenfelder_fuellen.

    ENDAT.


*-- Ende der Zahlungsbelegnummer ---------------------------------------
*-- end of payment document number -------------------------------------
    AT END OF reguh-vblnr.

*---- Prepare Open FI and User-Exits
      PERFORM fill_dtam100.
      CLEAR dtam100-xcrlf_supp.
      CLEAR dtam100-xchar_nrep.

*     Open FI / BTE (transaction record)
      IF par_mofi NE space.
        IF NOT t042ofi-xactive2 IS INITIAL.
          CALL FUNCTION 'OPEN_FI_PERFORM_00002020_P'
               EXPORTING
                    i_format        = t042ofi-formt
                    i_reguh         = reguh
                    i_dtam100       = dtam100
               IMPORTING
                    e_dtam100       = dtam100
               TABLES
                    t_regup         = tab_regup
                    t_dtam100v      = tab_dtam100v
               EXCEPTIONS
                    no_add_on_found = 1.
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            _open_fi = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

*     User-Exit (transaction record)
      PERFORM exit_900(rffoexit)
              TABLES tab_regup
                     tab_dtam100v
              USING  reguh
                     dtam100
                     up_usrex.
      IF NOT up_usrex IS INITIAL OR    "modifiziert / modified by user
         NOT _open_fi IS INITIAL.
         *regut-usrex+1(1) = up_usrex.
        IF up_usrex EQ '1' OR up_usrex EQ '3' OR
           NOT _open_fi IS INITIAL.
          PERFORM read_dtam100.
        ENDIF.
        IF up_usrex EQ '2' OR up_usrex EQ '3' OR
           NOT _open_fi IS INITIAL.
          PERFORM read_dtam100v.
        ENDIF.
      ENDIF.

*     Sortierung nach Tag (Für Format MT101 nicht erwünscht)
      IF t042ofi-formt <> 'MT101'.
        SORT dta_mt100 BY tag.
      ENDIF.

*     Kein Avis gefordert
      IF dtam100-xavis_req IS INITIAL.
        MOVE-CORRESPONDING reguh TO tab_kein_avis.
        APPEND tab_kein_avis.
      ENDIF.

*     Aufbereitung und Schreiben der Daten aus DTA_MT100
      LOOP AT dta_mt100.
        IF dtam100-xchar_nrep IS INITIAL.
          PERFORM: dta_text_aufbereiten USING dta_mt100-value,
                   mt100_gueltige_zeichen USING dta_mt100-value.
        ENDIF.
        IF dta_mt100-len IS INITIAL.
          dta_mt100-len = strlen( dta_mt100-value ).
        ENDIF.
        IF  dta_mt100-maxlen gt 0
        AND dta_mt100-len GT dta_mt100-maxlen.
*       maximum length is known (standard SWIFT fields only)
*       and field is too long
          IF par_mofi = space
          OR par_mofi = 'MT100'
          OR par_mofi = 'MT101'.
            dta_mt100-len = dta_mt100-maxlen.
          ENDIF.
        ENDIF.

        IF dta_mt100-len GT 5 OR
           dta_mt100-len GT 0 AND dta_mt100-value NP ':*:'.
          IF NOT dtam100-xcrlf_supp IS INITIAL.
            PERFORM store_on_file USING dta_mt100-value(dta_mt100-len).
          ELSE.
            PERFORM store_on_file USING:
              dta_mt100-value(dta_mt100-len), hlp_crlf.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: up_usrex, _open_fi.

      ADD reguh-rbetr TO sum_regut.

      IF par_cbxx IS INITIAL.          "Einzelzahlung
        dtam100t-xcrlf_supp = space.

*       Open FI / BTE (trailer for single payments)
        IF par_mofi NE space.
          IF NOT t042ofi-xactive3 IS INITIAL.
            CALL FUNCTION 'OPEN_FI_PERFORM_00002030_P'
                 EXPORTING
                      i_format        = t042ofi-formt
                      i_reguh         = reguh
                      i_dtam100s      = dtam100s
                      i_dtam100t      = dtam100t
                 IMPORTING
                      e_dtam100t      = dtam100t
                 EXCEPTIONS
                      no_add_on_found = 1.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.
              _open_fi = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

*       User-Exit (trailer for single payments)
        PERFORM exit_902(rffoexit)
               TABLES tab_regup
               USING  reguh
                      dtam100s
                      dtam100t
                      par_cbxx
                      up_usrex.
        IF NOT up_usrex IS INITIAL OR  "modifiziert / modified by user
           NOT _open_fi IS INITIAL.
           *regut-usrex+2 = up_usrex.
          CLEAR: up_usrex, _open_fi.
          IF dtam100t-t01 IS INITIAL.
            up_len = strlen( dtam100t-t00 ).
          ELSE.
            up_len = dtam100t-t01.
          ENDIF.
          IF up_len GT 0.
            IF NOT dtam100t-xcrlf_supp IS INITIAL.
              PERFORM store_on_file USING dtam100t-t00(up_len).
            ELSE.
              PERFORM store_on_file USING:
                      dtam100t-t00(up_len), hlp_crlf.
            ENDIF.
          ENDIF.
          CLEAR dtam100t.
        ENDIF.

        PERFORM datei_schliessen.
      ENDIF.
      SET LANGUAGE sy-langu.           " Anmeldesprache
    ENDAT.

    AT END OF reguh-ubnkl.
      IF NOT par_cbxx IS INITIAL.      "mehrere Zahlungen
        dtam100t-xcrlf_supp = space.

*       Open FI / BTE (trailer for multi payments)
        IF par_mofi NE space.
          IF NOT t042ofi-xactive3 IS INITIAL.
            CALL FUNCTION 'OPEN_FI_PERFORM_00002030_P'
                 EXPORTING
                      i_format        = t042ofi-formt
                      i_reguh         = reguh
                      i_dtam100s      = dtam100s
                      i_dtam100t      = dtam100t
                 IMPORTING
                      e_dtam100t      = dtam100t
                 EXCEPTIONS
                      no_add_on_found = 1.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.
              _open_fi = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

*       User-Exit for trailer (trailer for multi payment)
        PERFORM exit_902(rffoexit)
                TABLES tab_regup
                USING  reguh
                       dtam100s
                       dtam100t
                       par_cbxx
                       up_usrex.
        IF NOT up_usrex IS INITIAL OR  "modifiziert / modified by user
           NOT _open_fi IS INITIAL.
           *regut-usrex+2 = up_usrex.
          CLEAR: up_usrex, _open_fi.
          IF dtam100t-t01 IS INITIAL.
            up_len = strlen( dtam100t-t00 ).
          ELSE.
            up_len = dtam100t-t01.
          ENDIF.
          IF up_len GT 0.
            IF NOT dtam100t-xcrlf_supp IS INITIAL.
              PERFORM store_on_file USING dtam100t-t00(up_len).
            ELSE.
              PERFORM store_on_file USING:
                      dtam100t-t00(up_len), hlp_crlf.
            ENDIF.
          ENDIF.
          CLEAR dtam100t.
        ENDIF.

        PERFORM datei_schliessen.
      ENDIF.
    ENDAT.

    AT END OF reguh-zbukr.
    ENDAT.

  ENDLOOP.

ENDFORM.                                                    "MT100


*----------------------------------------------------------------------*
* Form MT100_INIT
*----------------------------------------------------------------------*
FORM mt100_init.

  CLEAR dta_mt100.
  REFRESH dta_mt100.
  PERFORM mt100_init_work
          USING: '00'    ''      '' '0'  ,                       "620311
                 '20'    ':20:'  '' '20' ,
                 '21'    ':21:'  '' '20' ,       "MT101 only
                 '23E_1' ':23E:' '' '40' ,       "MT101 only
                 '23E_2' ':23E:' '' '40' ,       "MT101 only
                 '23E_3' ':23E:' '' '40' ,       "MT101 only
                 '23E_4' ':23E:' '' '40' ,       "MT101 only
                 '32A'   ':32A:' '' '29' ,
                 '32B'   ':32B:' '' '23' ,       "MT101 only
                 '50_1'  ':50:'  '' '39' ,
                 '50_2'  ''      '' '35' ,
                 '50_3'  ''      '' '35' ,
                 '50_4'  ''      '' '35' ,
                 '50L'   ':50L:' '' '40' ,       "MT101 only
                 '50H_1' ':50H:' '' '40' ,       "MT101 only
                 '50H_2' ''      '' '35' ,       "MT101 only
                 '50H_3' ''      '' '35' ,       "MT101 only
                 '50H_4' ''      '' '35' ,       "MT101 only
                 '50H_5' ''      '' '35' ,       "MT101 only
                 '52_1'  ''      '' '40' ,
                 '52_2'  ''      '' '35' ,
                 '52_3'  ''      '' '35' ,
                 '52_4'  ''      '' '35' ,
                 '53_1'  ':53B:' '' '40' ,
                 '53_2'  ''      '' '35' ,
                 '53_3'  ''      '' '35' ,
                 '53_4'  ''      '' '35' ,
                 '54_1'  ''      '' '40' ,
                 '54_2'  ''      '' '35' ,
                 '54_3'  ''      '' '35' ,
                 '54_4'  ''      '' '35' ,
                 '54_5'  ''      '' '35' ,
                 '56_1'  ''      '' '40' ,
                 '56_2'  ''      '' '35' ,
                 '56_3'  ''      '' '35' ,
                 '56_4'  ''      '' '35' ,
                 '56_5'  ''      '' '35' ,
                 '57A'   ':57A:' '' '40' ,
                 '57_1'  ':57D:' '' '40' ,
                 '57_2'  ''      '' '35' ,
                 '57_3'  ''      '' '35' ,
                 '57_4'  ''      '' '35' ,
                 '59_1'  ':59:'  '' '39' ,
                 '59_2'  ''      '' '35' ,
                 '59_3'  ''      '' '35' ,
                 '59_4'  ''      '' '35' ,
                 '59_5'  ''      '' '35' ,
                 '70_1'  ''      '' '39' ,
                 '70_2'  ''      '' '35' ,
                 '70_3'  ''      '' '35' ,
                 '70_4'  ''      '' '35' ,
                 '77B_1' ':77B:' '' '40' ,       "MT101 only
                 '77B_2' ''      '' '35' ,       "MT101 only
                 '77B_3' ''      '' '35' ,       "MT101 only
                 '71A'   ':71A:' '' '8'  ,
                 '25A'   ':25A:' '' '40' ,       "MT101 only
                 '72_1'  ':72:'  '' '39' ,       "For User-Exit
                 '72_2'  ''      '' '35' ,       "For User-Exit
                 '72_3'  ''      '' '35' ,       "For User-Exit
                 '72_4'  ''      '' '35' ,       "For User-Exit
                 '72_5'  ''      '' '35' ,       "For User-Exit
                 '72_6'  ''      '' '35' ,       "For User-Exit
                 '99'    ''      '' '1'  .

ENDFORM.                               "MT100_INIT


*----------------------------------------------------------------------*
* Form MT100_INIT_WORK
*----------------------------------------------------------------------*
FORM mt100_init_work USING tag wert len maxlen.

  CLEAR dta_mt100.
  dta_mt100-tag    = tag.
  dta_mt100-value  = wert.
  dta_mt100-len    = len.
  dta_mt100-maxlen = maxlen.
  APPEND dta_mt100.

ENDFORM.                               "MT100_INIT_WORK


*----------------------------------------------------------------------*
* Form DTA_ERWEITERUNGSTEIL                                            *
*----------------------------------------------------------------------*
* Füllen des Erweiterungsteils eines C-Satzes im DTA Inland            *
* fill file extension field with note to payee                         *
*----------------------------------------------------------------------*
* KZ      - Kennzeichen des Erweiterungsteils                          *
*           file extension indicator                                   *
* ERWTEIL - Erweiterungsteil                                           *
*           file extension                                             *
*----------------------------------------------------------------------*
FORM dta_erweiterungsteil USING p_text.

  DATA:
    up_tag    LIKE dta_mt100-tag VALUE '70_',
    up_tabix  LIKE sy-tabix.

  txt_zeile = p_text.
  PERFORM dta_globals_ersetzen USING txt_zeile.

  ADD 1 TO dta_zeilen.
  up_tag+3 = dta_zeilen.
  CONDENSE up_tag NO-GAPS.
  dta_mt100-tag = up_tag.
  READ TABLE dta_mt100 WITH KEY up_tag.
  up_tabix = sy-tabix.
  IF dta_zeilen EQ 1.
    dta_mt100-value   = ':70:'.
    dta_mt100-value+4 = txt_zeile.
    dta_mt100-maxlen  = 39.
  ELSE.
    dta_mt100-value  = txt_zeile.
    dta_mt100-maxlen = 35.
  ENDIF.
  IF up_tabix NE 0.
    MODIFY dta_mt100 INDEX up_tabix.
  ENDIF.

* Fill internal table TAB_DTAM100V for user-exit / BTE with the
* further payment references
  IF par_zeil GT 4 AND cnt_zeilen LE par_zeil.
    tab_dtam100v-tag   = dta_mt100-tag.
    tab_dtam100v-value = dta_mt100-value.
    APPEND tab_dtam100v.
  ENDIF.

ENDFORM.                               "DTA_ERWEITERUNGSTEIL


*----------------------------------------------------------------------*
* Form DTA_GUELTIGE_ZEICHEN                                            *
*----------------------------------------------------------------------*
* Untersucht, ob ein Textstring nur gültige Zeichen enthält            *
* deletes invalid letters                                              *
*----------------------------------------------------------------------*
* TEXTFELD - zu untersuchendes Feld                                    *
*            text that is to be checked                                *
*----------------------------------------------------------------------*
FORM dta_gueltige_zeichen USING textfeld.

  WHILE textfeld CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 .,&-/+*$%'.
    WRITE space TO textfeld+sy-fdpos(1).
  ENDWHILE.

ENDFORM.                               "DTA_GUELTIGE_ZEICHEN


*----------------------------------------------------------------------*
* Form PUT_MT100                                                       *
*----------------------------------------------------------------------*
* Füllt die interne Tabelle DTA_MT100                                  *
*----------------------------------------------------------------------*
FORM put_mt100 USING p_tag     LIKE dta_mt100-tag
                     p_value   TYPE any
                     p_comp    TYPE i.

  DATA:
    up_num(40)          TYPE n,
    up_off              TYPE i,
    up_buffer(6)        TYPE c,
    up_len              LIKE dta_mt100-len,
    up_value            LIKE dta_mt100-value,
    up_clearing_code(4) TYPE c,
    up_sytabix          LIKE sy-tabix,
    ls_t012k            LIKE t012k,
    lc_bankn            LIKE reguh_bf-zbnkn,
    up_account_def_len  TYPE i,
    up_account_val_len  TYPE i.

  DESCRIBE FIELD lc_bankn LENGTH up_account_def_len IN CHARACTER MODE.

  READ TABLE dta_mt100 WITH KEY p_tag.
  DESCRIBE FIELD p_value OUTPUT-LENGTH up_len.
  up_off = strlen( dta_mt100-value ).
  up_sytabix = sy-tabix.

  CASE p_tag.
*   Field 32A
    WHEN '32A'.
      CASE p_comp.
        WHEN 1.
          WRITE p_value TO up_buffer YYMMDD.
          up_len = 6.
          dta_mt100-value+up_off(up_len) = up_buffer.
        WHEN 2.
          up_len = 3.
          dta_mt100-value+up_off(up_len) = p_value.
        WHEN 3.
          CALL FUNCTION 'SPELL_AMOUNT'
               EXPORTING
                    amount    = reguh-rwbtr
                    currency  = reguh-waers
                    filler    = space
                    language  = space
               IMPORTING
                    in_words  = spell
               EXCEPTIONS
                    not_found = 01
                    too_large = 02.
          up_value                       = spell-number.
          up_value+15                    = ','.
          IF spell-currdec GT 0.
            up_value+16(spell-currdec)   = spell-decimal.
          ENDIF.
          WHILE up_value(1) EQ '0'.
            SHIFT up_value LEFT BY 1 PLACES.
          ENDWHILE.
          up_len = strlen( up_value ).
          dta_mt100-value+up_off(up_len) = up_value.
      ENDCASE.
      MODIFY dta_mt100 INDEX up_sytabix.

*   Field 53, first line
    WHEN '53_1'.
      dta_mt100-value+up_off = '/'.
      ADD 1 TO up_off.
      CALL FUNCTION 'GET_EXT_BANKACCOUNT_NO'
           EXPORTING
                i_bank_country     = reguh-ubnks
                i_blz              = reguh-ubnkl
                i_bank_account     = reguh-ubknt
                i_control_key      = reguh-ubkon
           IMPORTING
                e_ext_bank_account = up_value.

      up_account_val_len = numofchar( up_value ).
      IF up_account_val_len <= up_account_def_len.
        lc_bankn = up_value.
        CALL FUNCTION 'FI_HOUSEBANK_ACCOUNT_READ'
             EXPORTING
                  ic_bukrs = reguh-zbukr
                  ic_hbkid = reguh-hbkid
                  ic_hktid = reguh-hktid
             IMPORTING
                  es_t012k = ls_t012k.
        CALL FUNCTION 'CONVERT_HOUSEBANK_ACCOUNT_NUM'
             EXPORTING
                  i_land1      = reguh-ubnks
                  i_bankk      = reguh-ubnky
                  i_bankn      = lc_bankn
                  i_bkont      = reguh-ubkon
                  i_refzl      = ls_t012k-refzl
                  i_bankl      = reguh-ubnkl
             IMPORTING
                  e_bankn_long = up_value.
      ENDIF.
      dta_mt100-value+up_off(34) = up_value.
      MODIFY dta_mt100 INDEX up_sytabix.

*   Field 57A
    WHEN '57A'.
      IF p_comp = 2.
        PERFORM get_clearing_code USING reguh-zbnks
                                        reguh-zbnkl
                                        up_clearing_code+2(2).
        IF NOT up_clearing_code IS INITIAL.
          up_clearing_code(2) = '//'.
          dta_mt100-value+up_off(4) = up_clearing_code.
          CLEAR up_clearing_code.
          ADD 4 TO up_off.
          ADD 4 TO up_len.
          WRITE reguh-zbnkl TO dta_mt100-value+up_off.
          CONDENSE dta_mt100-value NO-GAPS.
        ELSE.
          WRITE p_value TO dta_mt100-value+up_off.
          CONDENSE dta_mt100-value NO-GAPS.
        ENDIF.
      ELSE.
        dta_mt100-value+up_off = p_value.
      ENDIF.
      MODIFY dta_mt100 INDEX up_sytabix.

*   Field 59, first line
    WHEN '59_1'.
      dta_mt100-value+up_off = '/'.
      ADD 1 TO up_off.
      IF regud-ziban IS INITIAL.
*     no IBAN; write bank account number to field 59_1
        CALL FUNCTION 'GET_EXT_BANKACCOUNT_NO'
             EXPORTING
                  i_bank_country     = reguh-zbnks
                  i_blz              = reguh-zbnkl
                  i_bank_account     = reguh-zbnkn
                  i_control_key      = reguh-zbkon
             IMPORTING
                  e_ext_bank_account = up_value.

        up_account_val_len = numofchar( up_value ).
        IF up_account_val_len <= up_account_def_len.
          lc_bankn = up_value.
          CALL FUNCTION 'CONVERT_BANK_ACCOUNT_NUMBER'
               EXPORTING
                    i_banks      = reguh-zbnks
                    i_bankk      = reguh-zbnky
                    i_bankn      = lc_bankn
                    i_bkont      = reguh-zbkon
                    i_bkref      = reguh-bkref
                    i_bankl      = reguh-zbnkl
               IMPORTING
                    e_bankn_long = up_value.
        ENDIF.
      ELSE.
*     transfer IBAN
        up_value = regud-ziban.
      ENDIF.

      dta_mt100-value+up_off(34) = up_value.
      MODIFY dta_mt100 INDEX up_sytabix.

*   Field 71A
    WHEN '71A'.
      IF p_value = '01'.
        dta_mt100-value+up_off = 'OUR'.
        MODIFY dta_mt100 INDEX up_sytabix.
      ELSEIF p_value = '02'.
        dta_mt100-value+up_off = 'BEN'.
        MODIFY dta_mt100 INDEX up_sytabix.
      ENDIF.

*   Field 72, first line
    WHEN '72_1'.
      PERFORM weisungsschluessel_umsetzen USING '100' '1'
                                                p_value
                                                txt_zeile
                                                txt_zeile+40(*).
      IF txt_zeile(40) IS INITIAL.
        IF NOT p_value IS INITIAL.
          fimsg-msgv1 = sy-repid.
          fimsg-msgv2 = p_value.
          fimsg-msgv3 = reguh-ubnks.
          fimsg-msgv4 = reguh-rzawe.
          PERFORM message USING '460'.
        ENDIF.
      ELSE.
        CONDENSE txt_zeile.
        dta_mt100-value+up_off = txt_zeile.
        MODIFY dta_mt100 INDEX up_sytabix.
      ENDIF.

*   Field 72, second line
    WHEN '72_2'.
      PERFORM weisungsschluessel_umsetzen USING '100' '2'
                                                p_value
                                                txt_zeile
                                                txt_zeile+40(*).
      IF txt_zeile(40) IS INITIAL.
        IF NOT p_value IS INITIAL.
          fimsg-msgv1 = sy-repid.
          fimsg-msgv2 = p_value.
          fimsg-msgv3 = reguh-ubnks.
          fimsg-msgv4 = reguh-rzawe.
          PERFORM message USING '460'.
        ENDIF.
      ELSE.
        CONDENSE txt_zeile.
        dta_mt100-value+up_off = txt_zeile.
        MODIFY dta_mt100 INDEX up_sytabix.
      ENDIF.

*   Field 72, third line
    WHEN '72_3'.
      PERFORM weisungsschluessel_umsetzen USING '100' '3'
                                                p_value
                                                txt_zeile
                                                txt_zeile+40(*).
      IF txt_zeile(40) IS INITIAL.
        IF NOT p_value IS INITIAL.
          fimsg-msgv1 = sy-repid.
          fimsg-msgv2 = p_value.
          fimsg-msgv3 = reguh-ubnks.
          fimsg-msgv4 = reguh-rzawe.
          PERFORM message USING '460'.
        ENDIF.
      ELSE.
        CONDENSE txt_zeile.
        dta_mt100-value+up_off = txt_zeile.
        MODIFY dta_mt100 INDEX up_sytabix.
      ENDIF.

*   Field 72, forth line
    WHEN '72_4'.
      PERFORM weisungsschluessel_umsetzen USING '100' '4'
                                                p_value
                                                txt_zeile
                                                txt_zeile+40(*).
      IF txt_zeile(40) IS INITIAL.
        IF NOT p_value IS INITIAL.
          fimsg-msgv1 = sy-repid.
          fimsg-msgv2 = p_value.
          fimsg-msgv3 = reguh-ubnks.
          fimsg-msgv4 = reguh-rzawe.
          PERFORM message USING '460'.
        ENDIF.
      ELSE.
        CONDENSE txt_zeile.
        dta_mt100-value+up_off = txt_zeile.
        MODIFY dta_mt100 INDEX up_sytabix.
      ENDIF.

*   Alle other fields or lines
    WHEN OTHERS.
      IF up_len GT 0.
        dta_mt100-value+up_off(up_len) = p_value.
        MODIFY dta_mt100 INDEX up_sytabix.
      ENDIF.

  ENDCASE.

ENDFORM.                               "PUT_MT100


*----------------------------------------------------------------------*
* Form MT100_GUELTIGE_ZEICHEN
*----------------------------------------------------------------------*
FORM mt100_gueltige_zeichen USING textfeld.

  WHILE textfeld CN
        'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890?().,''''+ /:@-'.
    WRITE space TO textfeld+sy-fdpos(1).
  ENDWHILE.

ENDFORM.                               "MT100_GUELTIGE_ZEICHEN


*----------------------------------------------------------------------*
* Form FILL_DTAM100
*----------------------------------------------------------------------*
FORM fill_dtam100.

  FIELD-SYMBOLS:
    <up_field>.

  DATA:
    up_field(20),
    up_xavis LIKE dtam100-xavis_req.

  up_xavis = dtam100-xavis_req.
  CLEAR dtam100.
  dtam100-xavis_req = up_xavis.
  up_field = 'DTAM100-'.
  LOOP AT nametab.
    CHECK nametab-fieldname NE 'XAVIS_REQ'.
    CHECK nametab-fieldname NE 'XCRLF_SUPP'.
    CHECK nametab-fieldname NE 'XCHAR_NREP'.
    up_field+8 = nametab-fieldname.
    ASSIGN  (up_field) TO <up_field>.
    CLEAR dta_mt100.
    dta_mt100-tag = nametab-fieldname.
    READ TABLE dta_mt100.
    <up_field> = dta_mt100-value.
  ENDLOOP.

ENDFORM.                               "FILL_DTAM100


*----------------------------------------------------------------------*
* Form READ_DTAM100
*----------------------------------------------------------------------*
FORM read_dtam100.

  FIELD-SYMBOLS:
    <up_field>.

  DATA: up_field(20),
        up_lfdnr LIKE dta_mt100-tag.

  up_field = 'DTAM100-'.
  LOOP AT nametab.
    up_field+8 = nametab-fieldname.
    ASSIGN  (up_field) TO <up_field>.
    CLEAR dta_mt100.
    dta_mt100-tag = nametab-fieldname.
    READ TABLE dta_mt100.
    dta_mt100-value = <up_field>.
*   DTA_MT100-LEN   = STRLEN( DTA_MT100-VALUE ).
    IF nametab-fieldname(3) EQ '70_'.
      up_lfdnr = nametab-fieldname+3.
      IF up_lfdnr = 1
      OR up_lfdnr = 2
      OR up_lfdnr LE par_zeil.
        IF sy-subrc EQ 0.
          MODIFY dta_mt100 INDEX sy-tabix.
        ELSE.
          APPEND dta_mt100.
        ENDIF.
      ENDIF.
    ELSE.
      IF sy-subrc EQ 0.
        MODIFY dta_mt100 INDEX sy-tabix.
      ELSE.
        IF nametab-fieldname NE 'XAVIS_REQ' AND
           nametab-fieldname NE 'XCRLF_SUPP' AND
           nametab-fieldname NE 'XCHAR_NREP'.
          APPEND dta_mt100.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               "READ_DTAM100


*----------------------------------------------------------------------*
* Form DATEI_OEFFNEN_1
*----------------------------------------------------------------------*
FORM datei_oeffnen_1 USING p_cnt_filenr.

  IF hlp_temse CA par_dtyp.            "TemSe-Format
    PERFORM temse_oeffnen.
  ELSE.                                "disk-/tape-fmt on file-system
    PERFORM naechster_index USING hlp_renum.
    PERFORM fuellen_regut USING *regut-dtkey.
    hlp_filename    = par_unix.
    hlp_filename+39 = p_cnt_filenr.
    CONDENSE hlp_filename NO-GAPS.
    IF cl_abap_char_utilities=>charsize = 1.
      OPEN DATASET hlp_filename IN BINARY MODE FOR OUTPUT.
    ELSE.  " unicode system
      IF P_UNICO IS NOT INITIAL               " note 970892
       OR p_codep = '4102' OR p_codep = '4103'.  "write file in unicode
        OPEN DATASET hlp_filename IN BINARY MODE FOR OUTPUT.
      ELSEIF p_codep = '4110'.   " UTF-8
        OPEN DATASET hlp_filename IN TEXT MODE FOR OUTPUT
                                          ENCODING DEFAULT.
      ELSEIF p_codep is not initial.   "write file in codepage
        OPEN DATASET hlp_filename FOR OUTPUT
                               IN LEGACY BINARY MODE
                               CODE PAGE p_codep.
      ELSE.
        OPEN DATASET hlp_filename FOR OUTPUT
                               IN LEGACY BINARY MODE.
      ENDIF.
    ENDIF.
    IF sy-subrc NE 0.
      IF sy-batch EQ space.
        MESSAGE a182(fr) WITH hlp_filename.
      ELSE.
        MESSAGE s182(fr) WITH hlp_filename.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

* Referenznr für RFDT sichern, Tabelle für Zahlungsbelege löschen
* store reference-number, refresh table for document-numbers
  CALL FUNCTION 'COMPUTE_CONTROL_NUMBER'
       EXPORTING
            i_refno  = hlp_renum
       IMPORTING
            e_result = hlp_resultat.
  regud-label = hlp_dta_id-refnr = hlp_resultat.
  CLEAR   tab_belege30a.
  REFRESH tab_belege30a.

ENDFORM.                               "DATEI_OEFFNEN_1


*----------------------------------------------------------------------*
* Form READ_DTAM100V
*----------------------------------------------------------------------*
* Read internal table tab_dtam100v filled by the user-exit
*----------------------------------------------------------------------*
FORM read_dtam100v.

  LOOP AT tab_dtam100v.
    CLEAR dta_mt100.
    dta_mt100-tag = tab_dtam100v-tag.
    READ TABLE dta_mt100.
    dta_mt100-value = tab_dtam100v-value.
    IF NOT tab_dtam100v-length IS INITIAL.
      dta_mt100-len = tab_dtam100v-length.
    ENDIF.
    IF sy-subrc EQ 0.
      MODIFY dta_mt100 INDEX sy-tabix.
    ELSE.
      APPEND dta_mt100.
    ENDIF.
  ENDLOOP.

ENDFORM.                               "READ_DTAM100V

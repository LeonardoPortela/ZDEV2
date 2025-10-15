*ACC ALV
*** INCLUDE FGVTRF10
*eject
*---------------------------------------------------------------------*
*       FORM PARAMETER_PRUEFEN                                        *
*---------------------------------------------------------------------*
*       Die Eingabe-Parameter werden auf Konsistenz geprüft
*       und Informationen zwischengespeichert.
*---------------------------------------------------------------------*
FORM parameter_pruefen.

  REFRESH: bukgestab,
           guvtab.
  PERFORM check_ledger.                "-> USED_V1 / T800A-Eintrag
  PERFORM check_bukges.              "BUKRS/GESNR pruefen + ->BUKGESTAB
  PERFORM check_newgl_migration.       "Check NewGL migr.plan/status
  PERFORM get_ledger_and_tab_info TABLES t_t800a_keyfig
                                         t_t881_keyfig
                                  USING ledger
                                        h_881
                                        h_800a.
  PERFORM check_satztyp.
  PERFORM check_version CHANGING gt_version_data.
  PERFORM check_ryear.
* perform check_archive using gt_version_data."done later, because of timeout
* perform check_line_items."not needed anymore, because leads only to problems
  PERFORM fuellen_fmod.                "T884C/T888G-FMODs zwsp.
  PERFORM fuellen_led_par.             "Übergabe-Param. LED_TAB füllen
  PERFORM check_authority USING gt_version_data."Berechtigung prüfen
  PERFORM check_add_fields.            "Zusätzliche Felder prüfen
ENDFORM.                    "PARAMETER_PRUEFEN


*eject
*---------------------------------------------------------------------*
*       FORM CHECK_LEDGER                                             *
*---------------------------------------------------------------------*
*       Das eingegebene Ledger RLDNR wird gegen Tab.881 verprobt.     *
*       Es darf nicht das Muss-Ledger (=00) sein.                     *
*       Das SAVOR-KZ muss angeschaltet sein.                          *
*       Lesen Tabelle T800A.                                          *
*       Aus T800A die Verbuchungsstruktur lesen und für die speziellen*
*       VB-Routinen  G_GLDB_POSTING_A,1,2,3 in USED_V1 zwsp.          *
*       zur.: aktueller T800A-Eintrag von T881-TAB
*---------------------------------------------------------------------*
FORM check_ledger.
  CLEAR: error.

* ------ Muss-Ledger ? ------------------------------------------------
* IF LEDGER = '00'.
*   MESSAGE W601.
* ENDIF.

* ------ Ledger in Tab.881 und SAVOR-KZ an ? --------------------------
*---> S4 Migration - 21/06/2023 - MA
*  SELECT SINGLE * FROM t881
*         WHERE rldnr = ledger.
  cl_fins_acdoc_util=>get_t881_emu(
  EXPORTING
  iv_rldnr  = ledger " Ledger
  EXCEPTIONS
  not_found = 1
  OTHERS    = 2 ).
*<--- S4 Migration - 21/06/2023 - MA
  IF sy-subrc NE 0.
    MESSAGE e602 WITH ledger.
  ENDIF.
  IF t881-vortrag = ' '.
    MESSAGE e603 WITH ledger.
  ENDIF.

* ------ Ueberpruefen ob Ledger von Transaktion bearbeitet werden darf
  CASE p_appl.
    WHEN 'FIGL'.
      IF t881-fix IS INITIAL OR t881-tab NE 'GLT0'.
        SET CURSOR FIELD 'LEDGER'.
        MESSAGE e631 WITH ledger.
      ENDIF.
    WHEN 'FIGLX'.
      IF t881-fix IS INITIAL OR t881-tab NE 'V_GLFLEXT'.
        SET CURSOR FIELD 'LEDGER'.
        MESSAGE e631 WITH ledger.
      ENDIF.
    WHEN 'FISL'.
      IF t881-fix NE space AND t881-tab NE 'GLT3'.
        MESSAGE e631 WITH ledger.
      ENDIF.
    WHEN 'FIGLF'.
      IF t881-glflex IS INITIAL.
        SET CURSOR FIELD 'LEDGER'.
        MESSAGE e631 WITH ledger.
      ENDIF.
  ENDCASE.

  IF NOT g_glflex IS INITIAL.
    CALL FUNCTION 'FAGL_CHECK_IF_LEDGER_IS_GLFLEX'
      EXPORTING
        i_rldnr                 = ledger
      EXCEPTIONS
        not_found               = 1
        no_glflex_ledger        = 2
        glflex_rollup_ledger    = 3
        wrong_application       = 4
        glflex_dependent_ledger = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* ------ Ueberpruefen Tab.800A ----------------------------------------
  SELECT SINGLE * FROM t800a
         WHERE tab =  t881-tab.        "Ledger-Tabelle da ?
  IF sy-subrc NE 0.
    MESSAGE e604 WITH ledger t881-tab.
  ENDIF.
  IF t800a-ttype NE 'TT'.              "Tabellen-Typ muss TT sein
    MESSAGE e605 WITH ledger t881-tab t800a-ttype.
  ENDIF.
  IF t800a-progroup = space.           "Programmgruppe muss da sein
    MESSAGE e615 WITH ledger t881-tab.
  ENDIF.
  xcomptab = t800a-comptab.            "Gesnr oder Bukrs

* ------ Verbuchungsstruktur aus T800A holen -> zwsp. in USED_V1 ------
  REFRESH used_v1.
  used_v1-progroup = t800a-progroup.   "von T881-Tab.

  IF t800a-ntable NE space.
    used_v1-tab    = t800a-ntable.     "Tab. hat SI-Tabelle
  ENDIF.

  IF t800a-ntable EQ space.            "Tab. hat keine SI-Tabelle:
    SELECT * FROM t800a
           WHERE ntable = t881-tab     "Tab. als Korr.Tab. suchen
           AND   ttype  = 'ST'.        "EP-Struktur lesen (ab 4.0)
      EXIT.                            "1. Eintrag verwenden
    ENDSELECT.
    IF sy-subrc EQ 0.
      used_v1-tab   = t800a-tab.       "Tab. hat nur Struktur ST
    ELSE.
      used_v1-tab   = 'GLU1'.          "Tab. hat nichts -> GLU1
    ENDIF.
* T800A wieder auf Original-Eintrag (T881-TAB) stellen
    SELECT SINGLE * FROM t800a
           WHERE tab =  t881-tab.
  ENDIF.

  APPEND used_v1.

* Müssen Einzelposten geschrieben werden?
  IF t800a-glflex NE space AND
     t800a-write_si_for_bcf NE space.
    gd_write_si = 'X'.
  ELSE.
    CLEAR gd_write_si.
  ENDIF.

ENDFORM.                    "CHECK_LEDGER

*eject
*---------------------------------------------------------------------*
*       FORM CHECK_BUKGES                                             *
*---------------------------------------------------------------------*
*       Die eingegebenen Buchungskreise werden gegen Tab.T001,        *
*       die eingegebenen Gesellschaften gegen Tab.T880 verprobt       *
*       und mit Zusatzinformationen in BUKGESTAB zwsp.                *
*---------------------------------------------------------------------*
FORM check_bukges.
  DATA: t_h_t001 LIKE t001 OCCURS 0 WITH HEADER LINE.
  DATA: t_h_t880 LIKE t880 OCCURS 0 WITH HEADER LINE.
  DATA: h_glx_org_info   LIKE  glx_org_info,
        ld_glflex_active TYPE boole_d,
        ld_bukrs         TYPE bukrs.

  CLEAR: error.

* ------ Parameter BUKRS angegeben ------------------------------------
  IF xcomptab = ' '.
* Check RCOMP, BUKRS
    IF NOT rcomp[] IS INITIAL.
      IF NOT bukrs[] IS INITIAL.
        MESSAGE e919.
      ENDIF.
      MESSAGE w606 WITH ledger.
    ENDIF.
* ------ Buchungskreise in T001 vorgesehen? ----------------------------
    SELECT * FROM t001 INTO TABLE t_h_t001
             WHERE bukrs IN bukrs.
    IF sy-subrc NE 0.
      MESSAGE e607.
    ENDIF.

    LOOP AT t_h_t001.
      CLEAR h_glx_org_info.
      CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
        EXPORTING
          i_rldnr             = ledger
          i_orgunit           = t_h_t001-bukrs
        IMPORTING
          organizational_info = h_glx_org_info
        EXCEPTIONS
          no_info_found       = 1
          error_in_setup      = 2
          OTHERS              = 3.
      CHECK sy-subrc = 0.
      CLEAR bukgestab.
*Währungen
      bukgestab-bukges = h_glx_org_info-bukrs.
      bukgestab-lccur  = h_glx_org_info-lccur.
      bukgestab-hwaer  = h_glx_org_info-curr1.
      bukgestab-rccur  = h_glx_org_info-rccur.
      bukgestab-kwaer  = h_glx_org_info-curr2.
      bukgestab-occur  = h_glx_org_info-occur.
      bukgestab-owaer  = h_glx_org_info-curr3.
      bukgestab-vtrhj = h_glx_org_info-vtrhj.
      bukgestab-periv = h_glx_org_info-periv.
* ------ Kontenpläne bestimmen ----------------------------
      bukgestab-altsv = h_glx_org_info-altsv.  "KZ alternat. Ktopl.
      IF t_h_t001-ktopl IS INITIAL.
        MESSAGE e616 WITH t_h_t001-bukrs.
      ELSE.
        bukgestab-ktopb  = t_h_t001-ktopl. "Bukrs-Ktopl für T030
      ENDIF.
* operativer (Bukrs-) Ktopl. aus T001
      IF h_glx_org_info-altsv = ' '.
        bukgestab-ktopl = t_h_t001-ktopl.
      ENDIF.
* Konzernkontenplan aus T004 lesen
      IF h_glx_org_info-altsv = '1'.
        SELECT SINGLE * FROM t004 "#EC CI_DB_OPERATION_OK[2389136] unterdrückt werden
                       WHERE ktopl = t_h_t001-ktopl.
        IF sy-subrc   NE 0  OR         "Konz-Ktopl nicht da
           t004-kktpl EQ space.
          MESSAGE e619 WITH t_h_t001-ktopl t_h_t001-bukrs.
        ELSE.
          bukgestab-ktopl = t004-kktpl.
        ENDIF.
      ENDIF.
* Landeskontenplan T001-KTOP2
      IF h_glx_org_info-altsv = '2'.
        IF t_h_t001-ktop2 IS INITIAL.
          MESSAGE e624 WITH t_h_t001-bukrs.
        ELSE.
          bukgestab-ktopl  = t_h_t001-ktop2.
        ENDIF.
      ENDIF.
* eigener Ktopl in T882
      IF h_glx_org_info-altsv = '9'.
        IF h_glx_org_info-ktopl IS INITIAL.
          MESSAGE e618 WITH h_glx_org_info-bukrs h_glx_org_info-rldnr.
        ELSE.
          bukgestab-ktopl  = h_glx_org_info-ktopl.        "eigener Ktopl
          bukgestab-ktopb  = h_glx_org_info-ktopl.        "kein Bukrs-Kt
        ENDIF.
      ENDIF.

      APPEND bukgestab.
* ------ Wurde Bukrs/Ledger bereits auf EURO umgesetzt -> Fehler
      PERFORM check_euro USING t_h_t001-bukrs
                               h_glx_org_info-periv.
    ENDLOOP.

* ------ Parameter GESNR angegeben ? ----------------------------------
  ELSE.
*  Check BUKRS and RCOMP
    IF NOT bukrs[] IS INITIAL.
      IF NOT rcomp[] IS INITIAL.
        MESSAGE e919.
      ENDIF.
      MESSAGE w608 WITH ledger.
    ENDIF.

* ------ Gesellschaften in T880 vorgesehen? ---------------------------
    SELECT * FROM t880 INTO TABLE t_h_t880
             WHERE rcomp IN rcomp.
    IF sy-subrc NE 0.
      MESSAGE e609.
    ENDIF.

    LOOP AT t_h_t880.
      CLEAR h_glx_org_info.
      CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
        EXPORTING
          i_rldnr             = ledger
          i_orgunit           = t_h_t880-rcomp
        IMPORTING
          organizational_info = h_glx_org_info
        EXCEPTIONS
          no_info_found       = 1
          error_in_setup      = 2
          OTHERS              = 3.
      CHECK sy-subrc = 0.
      CLEAR bukgestab.
      bukgestab-bukges = h_glx_org_info-rcomp.
      bukgestab-lccur  = h_glx_org_info-lccur.
      bukgestab-hwaer  = h_glx_org_info-curr1.
      bukgestab-rccur  = h_glx_org_info-rccur.
      bukgestab-kwaer  = h_glx_org_info-curr2.
      bukgestab-occur  = h_glx_org_info-occur.
      bukgestab-owaer  = h_glx_org_info-curr3.
      bukgestab-vtrhj = h_glx_org_info-vtrhj.
      bukgestab-periv = h_glx_org_info-periv.
      IF h_glx_org_info-ktopl IS INITIAL.
        MESSAGE e617 WITH h_glx_org_info-rcomp ledger.
      ELSE.
        bukgestab-ktopl = h_glx_org_info-ktopl.
      ENDIF.

      APPEND bukgestab.
    ENDLOOP.

  ENDIF.

* ------ mindestens 1 Bukrs/Gesnr fuer angeg. Ledger in BUKRS/BUKGESTAB?
  DESCRIBE TABLE bukgestab LINES lin_bukgestab.
  IF lin_bukgestab = 0.
    IF xcomptab = ' '.
      MESSAGE e610 WITH ledger.
    ELSE.
      MESSAGE e611 WITH ledger.
    ENDIF.
  ENDIF.

  IF p_appl = appl-figlf.
    LOOP AT bukgestab.
      MOVE bukgestab-bukges TO ld_bukrs.
      CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
        EXPORTING
          id_bukrs        = ld_bukrs
        IMPORTING
          e_glflex_active = ld_glflex_active.

      CHECK ld_glflex_active = space.
      MESSAGE i135(fagl_ledger_cust) WITH ld_bukrs.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "CHECK_BUKGES


*eject
*---------------------------------------------------------------------*
*       FORM SATZTYP                                                  *
*---------------------------------------------------------------------*
*       Die eingegebenen Satztypen werden auf Zulaessigkeit verprobt. *
*---------------------------------------------------------------------*
FORM check_satztyp.
  CLEAR: error.
  LOOP AT satztyp.
    IF satztyp-low  < '0' OR                              "#EC PORTABLE
       satztyp-low  > '3'.                                "#EC PORTABLE
      MESSAGE e612.
    ENDIF.
    IF satztyp-high NE ' '.
      IF satztyp-high < '0' OR                            "#EC PORTABLE
         satztyp-high > '3'.                              "#EC PORTABLE
        MESSAGE e612.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "CHECK_SATZTYP

*eject
*---------------------------------------------------------------------*
*       FORM VERSION                                                  *
*---------------------------------------------------------------------*
*       Die eingegebenen Versionen werden verprobt:                   *
*       - beim KONS-Ledger gegen Tabelle T858 (T881-FIX = 'X',        *
*                        T881-APPL= 'FI',  T881-SUBAPPL = 'LC')       *
*       - beim GLX-Ledger gegen Tabelle T894                          *
*         Eine extra Verprobung mit Satztyp und bei Plandaten         *
*         mit GJAHR in T895/C erfolgt nicht. Müßte auch weiter hinten *
*         beim Einlesen der Daten erfolgen, weil erst dort bekannt    *
*         wird (wegen Von-Bis-Angabe),welche Satztypen und Versionen  *
*         wirklich existieren (Satztyp X Version).                    *
*---------------------------------------------------------------------*
FORM check_version CHANGING ct_version_data TYPE t_version_data.
  DATA: t_sel_rvers LIKE rsdsselopt OCCURS 0 WITH HEADER LINE.
  CLEAR: error.
  CLEAR ct_version_data. REFRESH ct_version_data.
  LOOP AT version.
    MOVE-CORRESPONDING version TO t_sel_rvers.
    APPEND t_sel_rvers.
  ENDLOOP.
  CALL FUNCTION 'G_GET_VERSIONS_FOR_LEDGER'
    EXPORTING
      i_rldnr        = ledger
    TABLES
      t_sel_rvers    = t_sel_rvers
      t_version_data = ct_version_data.
ENDFORM.                    "CHECK_VERSION

*eject
*---------------------------------------------------------------------*
*       FORM CHECK_RYEAR                                              *
*---------------------------------------------------------------------*
*       Numerisch-Test.                                               *
*       Altes Jahr setzen  -> OLDJR                                   *
*---------------------------------------------------------------------*
FORM check_ryear.
  IF newjr CN ' 0123456789'.
    MESSAGE e614.
  ENDIF.
  oldjr = newjr - 1.
ENDFORM.                    "CHECK_RYEAR

*eject
*---------------------------------------------------------------------*
*       FORM FUELLEN_GUVTAB  (neu: nur relevante GVTYP)               *
*---------------------------------------------------------------------*
*       Die Ergebnisvortragskonten werden in GUVTAB gestellt.
*       1. Für Bukrs erfolgt die Selektion aus T030.
*          Hierfür werden nur genau die GVTYP'en mit den entsprechenden
*          Vortragskonten gelesen und überprüft, die für die
*          vorzutragenden Bukrs relevant sind -> GVTYP_BUKRS.
*       2. Für Gesellschaften RCOMP erfolgt die Selektion aus T030C.
*                                                                     *
*       Beim Bukrs kann es alternative Konten geben:                  *
*       In Abhängigkeit, ob im Ledger alternative Konten abgelegt     *
*       sind (T882-ALTSV), wird das entsprechende Ergebnisvortrags-   *
*       konto für den alternativen Kontenplan verwendet.              *
*       In GUVTAB stehen der entsprechende (altern.) Ktopl. und das   *
*       Vortragskonto. Der GVTYP ist noch der originale aus dem       *
*       Bukrs-Vortragskonto (T030), weil ein anderer sowieso nicht    *
*       berücksichtigt wird -> da die Vortragskonten grundsätzlich    *
*       aus dem Bukrs-Vortragskonto aus T030 abgeleitet werden.       *
*       (nur das ist sinnvoll, weil auch alle anderen Kontenn, z.B.   *
*        in MM, abgeleitet werden.)                                   *
*                                                                     *
*       Füllen VTWTAB:                                                *
*       Für im Bukrs abgelegte Konten (ALTSV=' ' oder '2') wird       *
*       das Flag SKB1-XSALH pro Ergebnisvortragskonto überprüft.      *
*       Ist es nicht gesetzt, soll das Ergebnisvortragskonto
*       mit TWAER vorgetragen werden (Standard ist ohne TWAER, d.h.
*       Zusammenfassung auf HWAER). Diese Konten werden in der
*       Tabelle VTWTAB zwgesp.
*       Bei allen anderen Lokalen Ledgern und den globalen Ledgern
*       werden keine TWAER vorgetragen.                               *
*
*       Folgende Kontrollen des Ergebnisvortragskontos werden         *
*       durchgeführt:                                                 *
*       Bukrs:                                                        *
*       a) T882-ALTSV = space/2    ->     operativer Ktopl (Bukrs)    *
*         1. T030-KONT in SKA1 (T001-KTOPL)                           *
*         2. T030-KONT in SKB1 (T001-Bukrs)                           *
*         3. T030-KONT = Bilanzkonto                                  *
*       b) T882-ALTSV = 1          ->     Konzern-Ktopl               *
*         1. SKA1-BILKT vom T030-KONT in SKA1 (T004-KKTPL)            *
*         3. SKA1-BILKT = Bilanzkonto                                 *
*       c) T882-ALTSV = 2          ->     Landes-Ktopl (lokal)        *
*         1. SKB1-ALTKT vom T030-KONT in SKA1 (T001-KTOP2)            *
*         3. SKB1-ALTKT = Bilanzkonto                                 *
*       d) T882-ALTSV = 9          ->     eigener Ktopl               *
*         1. T030-KONT in SKA1 (T882-KTOPL)                           *
*         3. T030-KONT = Bilanzkonto                                  *
*       Gesnr:                                                        *
*         1. T030C-KONT in SKA1 (T882C-KTOPL)                          *
*         3. T030C-KONT = Bilanzkonto                                 *
*---------------------------------------------------------------------*
FORM fuellen_guvtab.

* selektieren der GVTYP aller Bukrs -> GVTYP_BUKRS + T030
  PERFORM fuellen_gvtyp_bukrs.

  LOOP AT bukgestab.

* ------ Buchungskreis-Inf. zwischenspeichern -------------------------
* -----( Fehler nicht sofort ausgeben, sondern erst als Liste in ------
* ------ CHECK_KONSISTENZ ) -------------------------------------------

    IF xcomptab = ' '.

* ------ Prüfungen nur noch auf Ktopl-Ebene nötig und möglich:---------
* ------ Ergebnisvortragskonto mit Bukrs- oder eigenem Ktopl ----------
      LOOP AT gvtyp_bukrs WHERE bukrs = bukgestab-bukges.
*     SELECT * FROM  T030              "Vortragskonten lesen
        CHECK gvtyp_bukrs-xerror IS INITIAL.  "T030-GVTYP vorhanden

        MOVE-CORRESPONDING gvtyp_bukrs TO t030.

        guvtab-ktopl = t030-ktopl.
        guvtab-gvtyp = t030-komok.
        IF t030-konts NE ' '.
          guvtab-vtkon = t030-konts.
        ELSE.
          IF t030-konth NE ' '.
            guvtab-vtkon = t030-konth.
          ELSE.                        "kein VT-Konto angegeben
            CHECK bukgestab-altsv NE space  AND
                  bukgestab-altsv NE '2'.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = t030-ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-014.
            mittab-text2  = TEXT-114.
            COLLECT mittab.
            guvtab-vtkon = '*'.
          ENDIF.
        ENDIF.

* ------ Bukrs-Vortragskonto in SKA1 (Kontenplan)   oder  --------------
* ------ Vortragskonto aus eigenem Ktopl im Kontenplan angelegt ? ------
* ------ zurück: SKA1, bei Fehler: MITTAB, GUVTAB-VTKON='*' -----------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          z_ktopl =  bukgestab-ktopb.  "evtl. für Feli-MITTAB
          PERFORM check_vortragskonto USING  bukgestab-bukges
                                             bukgestab-ktopb
                                             t030-komok
                                             guvtab-vtkon
                                             bukgestab-altsv.
        ENDIF.

* ------ Bukrs-Vortragskonto im Bukrs angelegt ? ----------------------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          IF bukgestab-altsv = space  OR
             bukgestab-altsv = 2.
            SELECT SINGLE * FROM skb1 "#EC CI_DB_OPERATION_OK[2431747] unterdrückt werden
                            WHERE bukrs = bukgestab-bukges
                            AND   saknr = guvtab-vtkon.
            IF sy-subrc NE 0.          "VKonto ohne Stammsatz
              mittab-racct  = guvtab-vtkon.     "im Bukrs.
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = t030-komok.
              mittab-text1  = TEXT-008.
              mittab-text2  = TEXT-108.
              COLLECT mittab.
              guvtab-vtkon = '*'.      "Fehler
            ELSE.

* ------ Bukrs-Vortragskonto mit TWAER vortragen -> VTWTAB -----------
              IF skb1-xsalh = space.
                IF bukgestab-altsv = space.
                  vtwtab-bukrs = skb1-bukrs.
                  vtwtab-vtkon = guvtab-vtkon.
                ELSE.
                  vtwtab-bukrs = skb1-bukrs.
                  vtwtab-vtkon = skb1-altkt.                "ALTSV='2'
                ENDIF.
                COLLECT vtwtab.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

* ------ alternative Konten - Konzernkontenplan ----------------------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          IF bukgestab-altsv = '1'.
            IF ska1-bilkt = space.
              mittab-racct  = guvtab-vtkon.     "SKA1-BILKT leer
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = t030-komok.
              mittab-text1  = TEXT-026.
              mittab-text2  = TEXT-114.
              COLLECT mittab.
              guvtab-vtkon = '*'.      "Fehler
            ELSE.
              guvtab-vtkon = ska1-bilkt.
              guvtab-ktopl = bukgestab-ktopl.

* Konzernkonto (T004) in SKA1 angelegt ?
              z_ktopl = bukgestab-ktopl.      "evtl. für Feli-MITTAB
              PERFORM check_vortragskonto USING  bukgestab-bukges
                                                 bukgestab-ktopl
                                                 t030-komok
                                                 guvtab-vtkon
                                                 bukgestab-altsv.
            ENDIF.
          ENDIF.

* ------ alternative Konten - Landeskontenplan (lokaler) -------------
          IF bukgestab-altsv = '2'.
            SELECT SINGLE * FROM skb1 "#EC CI_DB_OPERATION_OK[2431747] unterdrückt werden
                            WHERE bukrs = bukgestab-bukges
                            AND   saknr = guvtab-vtkon.
            IF skb1-altkt = space.
              mittab-racct  = guvtab-vtkon.     "SKB1-ALTKT leer
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = t030-komok.
              mittab-text1  = TEXT-027.
              mittab-text2  = TEXT-114.
              COLLECT mittab.
              guvtab-vtkon = '*'.      "Fehler
            ELSE.
              guvtab-vtkon = skb1-altkt.
              guvtab-ktopl = bukgestab-ktopl.

* Landes-Vortragkonto in SKA1 angelegt ?
              z_ktopl = bukgestab-ktopl.      "evtl. für Feli-MITTAB
              PERFORM check_vortragskonto USING  bukgestab-bukges
                                                 bukgestab-ktopl
                                                 t030-komok
                                                 guvtab-vtkon
                                                 bukgestab-altsv.
            ENDIF.
          ENDIF.
        ENDIF.

* ------ Vortragskonto  im Kontenplan als Bilanzkonto  angelegt? -----
        IF guvtab-vtkon NE '*'.        "bisher kein Fehler
          IF ska1-xbilk = space.       "kein Bilanzkonto
            mittab-racct  = guvtab-vtkon.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = z_ktopl.   "akt. SKA1-Ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-020.
            mittab-text2  = TEXT-120.
            COLLECT mittab.
            guvtab-vtkon = '*'.        "VKonto kein Bilanzkonto
          ENDIF.
        ENDIF.

* ---------------------
        COLLECT guvtab.
*       ENDSELECT.                     "T030
      ENDLOOP.                         "GVTYP_BUKRS + T030

* ------ Gesellschaft-Inf. zwischenspeichern -------------------------
    ELSE.

      SELECT * FROM  t030c
               WHERE ktopl = bukgestab-ktopl
               AND   ktosl = 'BIL'.
        guvtab-ktopl = t030c-ktopl.
        guvtab-gvtyp = t030c-komok.
        IF t030c-konts NE ' '.
          guvtab-vtkon = t030c-konts.
        ELSE.
          IF t030c-konth NE ' '.
            guvtab-vtkon = t030c-konth.
          ELSE.
*           MESSAGE E616 WITH T030C-KTOPL T030C-KOMOK.
            mittab-bukges = bukgestab-bukges.    "VKonto ist leer
            mittab-ktopl  = t030c-ktopl.
            mittab-gvtyp  = t030c-komok.
            mittab-text1  = TEXT-014.
            mittab-text2  = TEXT-114.
            COLLECT mittab.
            guvtab-vtkon = '*'.
          ENDIF.
        ENDIF.

* ------ - Gesnr-VKonto in SKA1 angelegt ? ---------------------------
        IF guvtab-vtkon NE '*'.
          PERFORM check_vortragskonto USING  bukgestab-bukges
                                             bukgestab-ktopl
                                             t030c-komok
*                                            T001-KTOP2       "??
*                                            T030-KOMOK       "??
                                             guvtab-vtkon
                                             bukgestab-altsv.
        ENDIF.

* ------ Vortragskonten im Kontenplan als Bilanzkonten angelegt? -----
        IF guvtab-vtkon NE '*'.        "bisher kein Fehler
          IF ska1-xbilk = space.       "kein Bilanzkonto
            mittab-racct  = guvtab-vtkon.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = bukgestab-ktopl.
            mittab-gvtyp  = t030c-komok.
            mittab-text1  = TEXT-020.
            mittab-text2  = TEXT-120.
            COLLECT mittab.
            guvtab-vtkon = '*'.        "VKonto kein Bilanzkonto
          ENDIF.
        ENDIF.

* -----------------
        COLLECT guvtab.
      ENDSELECT.                                            "T030C

    ENDIF.

  ENDLOOP.                             "BUKGESTAB.

ENDFORM.                    "FUELLEN_GUVTAB

*eject
*---------------------------------------------------------------------*
*       FORM FUELLEN_GVTYP_BUKRS                                      *
*---------------------------------------------------------------------*
*       Pro Bukrs werden SKA1 und SKB1 nach den relevanten GVTYP'en
*       durchsucht und in die int. Tabelle GVTYP_BUKRS abgespeichert.
*       Aus T030 werden die dazugehörigen Ergebnisvortragskonten
*       dazugeschrieben. Fehlt für einen GVTYP der T030-Eintrag,
*       wird das in die fehlerliste geschrieben.
*       Die Kontrolle erfolgt nur bei Bukrs (XCOMPTAB = ' ')
*       <--  GVTYP_BUKRS
*-----------------------------------------------------------------------
FORM fuellen_gvtyp_bukrs.

  CHECK xcomptab = ' '.                "nur für Bukrs

* ------ von allen Ktopl die G+V-Konten aus SKA1 ----------------------
  DATA: BEGIN OF int_ska1          OCCURS 0,
          ktopl LIKE ska1-ktopl,
          saknr LIKE ska1-saknr,
          gvtyp LIKE ska1-gvtyp,
        END OF int_ska1.

  REFRESH: gvtyp_bukrs,
           mittab.

* von allen Ktopl. die G+V-Konten zwsp. -> INT_SKA1
  LOOP AT bukgestab.
    LOOP AT int_ska1 WHERE ktopl = bukgestab-ktopb.
      EXIT.
    ENDLOOP.
    CHECK sy-subrc NE 0.               "Ktopl. noch nicht da
    SELECT * FROM ska1 WHERE ktopl =  bukgestab-ktopb "#EC CI_DB_OPERATION_OK[2431747]
                       AND   gvtyp NE space. "#EC CI_DB_OPERATION_OK[2389136]
      int_ska1-ktopl = ska1-ktopl.
      int_ska1-saknr = ska1-saknr.
      int_ska1-gvtyp = ska1-gvtyp.
      APPEND int_ska1.
    ENDSELECT.
  ENDLOOP.
  SORT int_ska1.

* Check which account uses which GVTYP.
* über alle Konten der Bukrs selektieren und Gvtyp holen -> GVTYP_BUKRS
  LOOP AT bukgestab.
    CLEAR gvtyp_bukrs.
    IF bukgestab-altsv = ' '  OR
       bukgestab-altsv = '2'.
* die bukrs-relevanten GVTYP selektieren
      SELECT * FROM skb1 WHERE bukrs = bukgestab-bukges. "#EC CI_DB_OPERATION_OK[2431747]
        READ TABLE int_ska1 WITH KEY ktopl = bukgestab-ktopb
                                     saknr = skb1-saknr   BINARY SEARCH.
        IF sy-subrc = 0.                 "nur G+V-Konten
          CLEAR gvtyp_bukrs.
          gvtyp_bukrs-bukrs = skb1-bukrs.
          gvtyp_bukrs-ktopb = int_ska1-ktopl.
          gvtyp_bukrs-gvtyp = int_ska1-gvtyp.
          COLLECT gvtyp_bukrs.
        ENDIF.
      ENDSELECT.

    ELSE.
* die Ktopl-relevanten GVTYP selektieren (ALTSV='1'/'9')
      LOOP AT int_ska1 WHERE ktopl = bukgestab-ktopb.
        gvtyp_bukrs-bukrs = bukgestab-bukges.
        gvtyp_bukrs-ktopb = int_ska1-ktopl.
        gvtyp_bukrs-gvtyp = int_ska1-gvtyp.
        COLLECT gvtyp_bukrs.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

* get all GVTYP's occuring for the set of given company codes.
* zwsp. aller relevanten T030-Einträge für die Bukrs -> GVTYP_T030
* Sind alle SKA1-GVTYP's in T030 definiert ? -> Fehlerprotokoll
  LOOP AT gvtyp_bukrs.
    SELECT SINGLE * FROM t030
           WHERE ktopl = gvtyp_bukrs-ktopb   "für alle GVTYP
           AND   ktosl = 'BIL'
           AND   bwmod = ' '
           AND   komok = gvtyp_bukrs-gvtyp.
    IF sy-subrc = 0.                   "zwsp.relevanter T030-Einträge
      MOVE-CORRESPONDING t030 TO gvtyp_bukrs.
      MODIFY gvtyp_bukrs.
    ELSE.                              "GVTYP fehlt in T030
      gvtyp_bukrs-xerror = 'X'.
      MODIFY gvtyp_bukrs.
*     MITTAB-BUKGES = GVTYP_BUKRS-BUKRS.
      mittab-ktopl  = gvtyp_bukrs-ktopb.
      mittab-gvtyp  = gvtyp_bukrs-gvtyp.
      mittab-text1  = TEXT-034.
      mittab-text2  = TEXT-134.
      COLLECT mittab.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "FUELLEN_GVTYP_BUKRS

*eject
*---------------------------------------------------------------------*
*       FORM FUELLEN_FMOD
*---------------------------------------------------------------------*
*      Es werden erzeugt und zwischengespeichert:
*      - die Feldmodifs aus T884C/T888G      ->FMOD-GUV, FMOD-BIL
*      - die Tab-Namen für COLLECT           ->SVGTAB_NAME, SVBTAB_NAME
*      - die Tab-Namen für Listenkopf-Felder ->LIST-G_NAME, LIST-B_NAME
*---------------------------------------------------------------------*
FORM fuellen_fmod.

  DATA: it_t884c LIKE t884c OCCURS 2 WITH HEADER LINE.

  usedt = t800a-tab.                   "für Löschen_periode_null
  CLEAR   fmod.

  CLEAR svgtab_name.
  CLEAR svbtab_name.

  CALL FUNCTION 'G_GIVE_CARRY_FORWARD_FMOD'
    EXPORTING
      i_rldnr       = ledger
    TABLES
      t_t884c       = it_t884c
    EXCEPTIONS
      no_info_found = 1
      OTHERS        = 2.

* Tabellennamen aufbauen
  LOOP AT it_t884c.
    IF it_t884c-xbilk = ' '.
      fmod-guv = it_t884c-feldmodif.
      svgtab_name(6)   = 'SVGTAB'.     "G+V-Konto
      svgtab_name+6(1) = '_'.
      svgtab_name+7(4) = it_t884c-feldmodif.
*      LIST_G_NAME(4)   = 'LIST'.
*      LIST_G_NAME+4(1) = '_'.
*      LIST_G_NAME+5(4) = it_T884C-FELDMODIF.
    ELSE.
      fmod-bil = it_t884c-feldmodif.
      svbtab_name(6)   = 'SVBTAB'.     "Bilanz-Konto
      svbtab_name+6(1) = '_'.
      svbtab_name+7(4) = it_t884c-feldmodif.
*      LIST_B_NAME(4)   = 'LIST'.
*      LIST_B_NAME+4(1) = '_'.
*      LIST_B_NAME+5(4) = it_T884C-FELDMODIF.
    ENDIF.
  ENDLOOP.

  IF svgtab_name = space.              "Standardfall (keine FMOD da):
    svgtab_name(6)   = 'SVGTAB'.       "G+V-Konto
    svgtab_name+6(1) = '_'.
*    LIST_G_NAME(4)   = 'LIST'.
*    LIST_G_NAME+4(1) = '_'.
  ENDIF.
  IF svbtab_name = space.              "Standardfall:
    svbtab_name(6)   = 'SVBTAB'.       "Bilanz-Konto
    svbtab_name+6(1) = '_'.
*    LIST_B_NAME(4)   = 'LIST'.
*    LIST_B_NAME+4(1) = '_'.
  ENDIF.

ENDFORM.                    "FUELLEN_FMOD

*eject
*---------------------------------------------------------------------*
*       FORM FUELLEN_LED_PAR (Tab.LEDGER)
*---------------------------------------------------------------------*
*       Für die Benutzung der Forms UPDATE_GLDB und UPDATE_DB
*       wird die Tab. LED_PAR (LEDGER) analog SAPLGLIN benötigt.
*       Hier werden die notwendigsten Informationen pro aktuellen
*       Buchungskreis/Gesellschaft eingespeichert.
*       (aktueller BUKRS/RCOMP wird später vor Update eingespeichert)
*---------------------------------------------------------------------*
FORM fuellen_led_par.
  CLEAR led_par.
  led_par-rldnr   = ledger.
  led_par-shkz    = t881-shkz.
  led_par-fldgr   = t800a-fldgr.
  led_par-tab     = t800a-tab.
  led_par-comptab = t800a-comptab.

*----- speziellen Namen des Movement-Programmes zwsp. ----------------
**+++++ wenn es möglich ist, daß Tabelle teilweise bei den MT steht
**+++++ und teilweide unter MT 0, dann hier erst prüfen, ob Programm
**+++++ existiert, wenn nicht Mandant 0
  led_par-mandt     = sy-mandt.
  led_par-prog      = 'RGIMVxxx'.
  led_par-prog+5(3) = sy-mandt.
  IF sy-mandt NE '000'.
    CLEAR trdir.
    SELECT SINGLE * FROM trdir WHERE name = led_par-prog.
    IF sy-subrc > 0.
      led_par-prog+5(3) = '000'.
    ENDIF.
  ENDIF.
ENDFORM.                    "FUELLEN_LED_PAR

*eject
*---------------------------------------------------------------------*
*       FORM CHECK_KONSISTENZ                                         *
*---------------------------------------------------------------------*
*       Prüfungen, ob Regeln und Kontenfindung gepflegt sind.         *
*       Fehler aus Fehler-Tab. MITTAB ausgeben.                       *
*---------------------------------------------------------------------*
FORM check_konsistenz.
  DATA: string(132).
  DATA: lt_outtab LIKE STANDARD TABLE OF fagl_acc_s_sapfgvtr_list,
        ls_outtab LIKE  fagl_acc_s_sapfgvtr_list.
  DATA: ls_layout   TYPE slis_layout_alv,
        lt_fieldcat TYPE slis_t_fieldcat_alv,
        lt_events   TYPE slis_t_event.
  DATA: g_repid     TYPE sy-repid.

  xkons = 'X'.                         "X: bisher alles richtig
  prtyp = 'M'.

* ------ Fehler beim Ergebnisvortragskonto (T030/T030C, SKA1, SKB1) ---
  READ TABLE mittab INDEX 1.
  IF sy-subrc = 0.
    MESSAGE s629.
    LOOP AT mittab.
      CLEAR: xkons.
      MOVE: mittab-bukges TO ls_outtab-rcomp,
            mittab-racct  TO ls_outtab-racct,
            mittab-ktopl  TO ls_outtab-ktopl,
            mittab-gvtyp  TO ls_outtab-gvtyp.
      CONCATENATE mittab-text1 mittab-text2 INTO ls_outtab-text.
      APPEND ls_outtab TO lt_outtab.
    ENDLOOP.

* build the field catalog.
    PERFORM set_alv_fieldcat1 CHANGING lt_fieldcat.
* build the event table.
    PERFORM set_alv_events1 CHANGING lt_events.
* output the table.
    MOVE sy-repid TO g_repid.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = g_repid
*       i_callback_pf_status_set = gc_setpfstatus4
        it_fieldcat        = lt_fieldcat
        it_events          = lt_events
      TABLES
        t_outtab           = lt_outtab
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* ------ Fehler bei Konsistenzcheck? -----------------------------------
* -> see FORM end_of_list1
ENDFORM.                    "CHECK_KONSISTENZ

*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*       Datenbankberechtigung G_GLTP prüfen                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM check_authority USING lt_version_data TYPE t_version_data.
  DATA: BEGIN OF help_satztyp,
          low  LIKE glu1-rrcty,
          high LIKE glu1-rrcty,
        END OF help_satztyp.
  DATA: h_bukrs LIKE t001-bukrs.
  DATA: h_rcomp LIKE t001-rcomp.                         "Note 1006150
  DATA: t_sel_rvers LIKE rsdsselopt OCCURS 0 WITH HEADER LINE.
  DATA: ls_version_data TYPE glx_version_info.
  DATA: ls_rrcty TYPE s_rrcty,
        lt_rrcty TYPE t_rrcty.

*For FI-SL and flexible GL check ledger, record type and version
  CASE p_appl.
    WHEN 'FIPSM'.                       " Note 1095503
* check company code and company
      LOOP AT bukgestab.
        IF NOT rcomp IS INITIAL.
          h_rcomp = bukgestab-bukges.
          AUTHORITY-CHECK OBJECT 'F_LC_COM'
               ID 'RCOMP' FIELD h_rcomp.
          IF sy-subrc NE 0.
            MESSAGE e115(gc) WITH h_rcomp.
          ENDIF.
        ELSE.
          h_bukrs = bukgestab-bukges.
          CALL FUNCTION 'FAGL_AUTHORITY_LEDGER'
            EXPORTING
              i_bukrs      = h_bukrs
              i_rldnr      = ledger
              i_actvt      = '02'
            EXCEPTIONS
              no_authority = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN 'FISL'
      OR 'FIGLX'.
      PERFORM get_reccordtypes CHANGING lt_rrcty.
      LOOP AT lt_version_data INTO ls_version_data.
        LOOP AT lt_rrcty INTO ls_rrcty.
          CHECK ls_rrcty-rrcty IN satztyp.
          CALL FUNCTION 'G_DATABASE_AUTHORITY_CHECK'
            EXPORTING
              actvt = '02'
              rldnr = ledger
              rrcty = ls_rrcty-rrcty
              rvers = ls_version_data-rvers.
        ENDLOOP.
      ENDLOOP.
* check company code and company
      LOOP AT bukgestab.
        IF NOT rcomp IS INITIAL.                        "Note 1006150
          h_rcomp = bukgestab-bukges.
          AUTHORITY-CHECK OBJECT 'F_LC_COM'
               ID 'RCOMP' FIELD h_rcomp.
          IF sy-subrc NE 0.
            MESSAGE e115(gc) WITH h_rcomp.
          ENDIF.
        ELSE.
          h_bukrs = bukgestab-bukges.
          AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                ID 'BUKRS' FIELD h_bukrs
                ID 'ACTVT' FIELD '01'.
          IF sy-subrc NE 0.
            MESSAGE e800(fr) WITH h_bukrs.
          ENDIF.
        ENDIF.
      ENDLOOP.

*  For FI-GL check company code
    WHEN 'FIGL'.
      LOOP AT bukgestab.
        h_bukrs = bukgestab-bukges.
        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                ID 'BUKRS' FIELD h_bukrs
                ID 'ACTVT' FIELD '01'.
        IF sy-subrc NE 0.
          MESSAGE e800(fr) WITH h_bukrs.
        ENDIF.
      ENDLOOP.
    WHEN 'FIGLF'.
      PERFORM get_reccordtypes CHANGING lt_rrcty.
      LOOP AT bukgestab.
        h_bukrs = bukgestab-bukges.
        LOOP AT lt_rrcty INTO ls_rrcty.
          CHECK ls_rrcty-rrcty IN satztyp.
          LOOP AT lt_version_data INTO ls_version_data.
            CALL FUNCTION 'FAGL_AUTHORITY_LEDGER'
              EXPORTING
                i_bukrs      = h_bukrs
                i_rldnr      = ledger
                i_actvt      = '02'
                i_rrcty      = ls_rrcty-rrcty
                i_rvers      = ls_version_data-rvers
              EXCEPTIONS
                no_authority = 1
                OTHERS       = 2.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.
ENDFORM.                               " CHECK_AUTHORITY
*---------------------------------------------------------------------*
*       FORM CHECK_add_fields                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM check_add_fields.
  IF NOT p_field1 IS INITIAL.
    CALL FUNCTION 'G_FIELD_READ'
      EXPORTING
        fieldname = p_field1
        table     = h_800a-tab.
  ENDIF.
  IF NOT p_field2 IS INITIAL.
    CALL FUNCTION 'G_FIELD_READ'
      EXPORTING
        fieldname = p_field2
        table     = h_800a-tab.
  ENDIF.
  IF NOT p_field3 IS INITIAL.
    CALL FUNCTION 'G_FIELD_READ'
      EXPORTING
        fieldname = p_field3
        table     = h_800a-tab.
  ENDIF.
ENDFORM.                    "CHECK_add_fields

*eject
*---------------------------------------------------------------------*
*       FORM CHECK_EURO                                               *
*---------------------------------------------------------------------*
*       Test, ob der Buchungskreis für das vorzutragende Geschäftsjahr
*       umgesetzt wurde.                                              *
*       Wenn Ja, wird für dieses und die vorhergehenden Gjahre der    *
*       Saldovortrag verhindert.                                      *
*       -> T001-BUKRS
*---------------------------------------------------------------------*
FORM check_euro USING ce_bukrs LIKE t001-bukrs
                      ce_periv LIKE t882-periv.

  DATA: euro_datum LIKE sy-datum,
        euro_gjahr LIKE bkpf-gjahr.

  CALL FUNCTION 'FI_EMU_GET_CONVERSION_DATE'
    EXPORTING
      i_bukrs = ce_bukrs
    IMPORTING
      e_date  = euro_datum.

* ------ Aktuelles Geschäftsjahr der Euro-Umsetzung ermitteln ----------
  CHECK NOT euro_datum IS INITIAL.

  CALL FUNCTION 'G_PERIOD_GET'
    EXPORTING
      date    = euro_datum
      variant = ce_periv
    IMPORTING
      year    = euro_gjahr.

* ------ Saldovortrag für umgesetztes oder vorherige GJahre ? ---------
  IF newjr <= euro_gjahr.
    MESSAGE e063(fu) WITH ce_bukrs euro_gjahr.
  ENDIF.

ENDFORM.                    "CHECK_EURO

*eject
*---------------------------------------------------------------------*
*       FORM FUELLEN_GUVTAB_NEW                                       *
*---------------------------------------------------------------------*
*       Die Ergebnisvortragskonten werden in GUVTAB gestellt.         *
*       Beim Bukrs kann es alternative Konten geben:
*       In Abhängigkeit, ob im Ledger alternative Konten abgelegt     *
*       sind (T882-ALTSV), wird das entsprechende Ergebnisvortrags-   *
*       konto für den alternativen Kontenplan verwendet.              *
*       In GUVTAB stehen der entsprechende (altern.) Ktopl. und das   *
*       Vortragskonto. Der GVTYP ist noch der originale aus dem       *
*       Bukrs-Vortragskonto (T030), weil ein anderer sowieso nicht    *
*       berücksichtigt wird -> da die Vortragskonten grundsätzlich    *
*       aus dem Bukrs-Vortragskonto aus T030 abgeleitet werden.       *
*       (nur das ist sinnvoll, weil auch alle anderen Kontenn, z.B.   *
*        in MM, abgeleitet werden.)                                   *
*       Füllen VTWTAB:
*       Für im Bukrs abgelegte Konten (ALTSV=' ' oder '2') wird       *
*       das Flag SKB1-XSALH pro Ergebnisvortragskonto überprüft.      *
*       Ist es nicht gesetzt, soll das Ergebnisvortragskonto
*       mit TWAER vorgetragen werden (Standard ist ohne TWAER,
*       Zusammenfassung auf HWAER). Diese Konten werden in der
*       Tabelle VTWTAB zwgesp.
*       Bei allen anderen Lokalen Ledgern und den globalen Ledgern
*       werden keine TWAER vorgetragen.                               *
*
*       Folgende Kontrollen des Ergebnisvortragskontos werden         *
*       durchgeführt:                                                 *
*       Bukrs:                                                        *
*       a) T882-ALTSV = space/2    ->     operativer Ktopl (Bukrs)    *
*         1. T030-KONT in SKA1 (T001-KTOPL)                           *
*         2. T030-KONT in SKB1 (T001-Bukrs)                           *
*         3. T030-KONT = Bilanzkonto                                  *
*       b) T882-ALTSV = 1          ->     Konzern-Ktopl               *
*         1. SKA1-BILKT vom T030-KONT in SKA1 (T004-KKTPL)            *
*         3. SKA1-BILKT = Bilanzkonto                                 *
*       c) T882-ALTSV = 2          ->     Landes-Ktopl (lokal)        *
*         1. SKB1-ALTKT vom T030-KONT in SKA1 (T001-KTOP2)            *
*         3. SKB1-ALTKT = Bilanzkonto                                 *
*       d) T882-ALTSV = 9          ->     eigener Ktopl               *
*         1. T030-KONT in SKA1 (T882-KTOPL)                           *
*         3. T030-KONT = Bilanzkonto                                  *
*       Gesnr:                                                        *
*         1. T030C-KONT in SKA1 (T882C-KTOPL)                          *
*         3. T030C-KONT = Bilanzkonto                                 *
*---------------------------------------------------------------------*
FORM fuellen_guvtab_new.
  DATA: gvtyp_list LIKE t030       OCCURS 0 WITH HEADER LINE.
  DATA: int_ska1   LIKE ska1       OCCURS 0 WITH HEADER LINE.
*
* First get all GVTYP's occuring for the set of given company codes.
*
  LOOP AT bukgestab.
    SELECT * FROM t030
             WHERE ktopl = bukgestab-ktopb   "für alle GVTYP
             AND   ktosl = 'BIL'
             AND   bwmod = ' '.
      gvtyp_list = t030.
      COLLECT gvtyp_list.
    ENDSELECT.
  ENDLOOP.
*
* Check which account uses which GVTYP.
*
  LOOP AT gvtyp_list.
    SELECT * FROM ska1           "#EC CI_DB_OPERATION_OK[2389136]
       WHERE gvtyp = gvtyp_list. "#EC CI_DB_OPERATION_OK[2431747]
      MOVE ska1 TO int_ska1.
      APPEND int_ska1.
    ENDSELECT.
  ENDLOOP.
*
* Check if these account are used in company code
*
  LOOP AT bukgestab.
    IF bukgestab-altsv = space  OR
       bukgestab-altsv = 2.

      LOOP AT int_ska1.
        SELECT SINGLE * FROM skb1 WHERE bukrs = bukgestab-bukges "#EC CI_DB_OPERATION_OK[2431747]
                                  AND   saknr = int_ska1-saknr.
        IF sy-subrc EQ 0.
          READ TABLE gvtyp_list WITH KEY komok = int_ska1-gvtyp.
          SELECT SINGLE * FROM skb1 WHERE bukrs = bukgestab-bukges "#EC CI_DB_OPERATION_OK[2431747]
                                    AND   saknr = gvtyp_list-konts.
          IF sy-subrc NE 0.
            mittab-racct  = guvtab-vtkon."im Bukrs.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = t030-ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-008.
            mittab-text2  = TEXT-108.
            APPEND mittab.
            guvtab-vtkon = '*'.        "Fehler
          ELSE.
* ------ Bukrs-Vortragskonto mit TWAER vortragen -> VTWTAB -----------
            IF skb1-xsalh = space.
              IF bukgestab-altsv = space.
                vtwtab-bukrs = skb1-bukrs.
                vtwtab-vtkon = guvtab-vtkon.
              ELSE.
                vtwtab-bukrs = skb1-bukrs.
                vtwtab-vtkon = skb1-altkt.                  "ALTSV='2'
              ENDIF.
              COLLECT vtwtab.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.




  LOOP AT bukgestab.

* ------ Buchungskreis-Inf. zwischenspeichern -------------------------
* -----( Fehler nicht sofort ausgeben, sondern erst als Liste in ------
* ------ CHECK_KONSISTENZ ) -------------------------------------------
    IF xcomptab = ' '.

* ------ Ergebnisvortragskonto mit Bukrs- oder eigenem Ktopl ----------
      SELECT * FROM  t030              "Vortragskonten lesen
             WHERE ktopl = bukgestab-ktopb   "für alle GVTYP
             AND   ktosl = 'BIL'
             AND   bwmod = ' '.
        guvtab-ktopl = t030-ktopl.     "??
        guvtab-gvtyp = t030-komok.
        IF t030-konts NE ' '.
          guvtab-vtkon = t030-konts.
        ELSE.
          IF t030-konth NE ' '.
            guvtab-vtkon = t030-konth.
          ELSE.                        "kein VT-Konto angegeben
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = t030-ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-014.
            mittab-text2  = TEXT-114.
            APPEND mittab.
            guvtab-vtkon = '*'.
          ENDIF.
        ENDIF.

* ------ Bukrs-Vortragskonto in SKA1 (Kontenplan)   oder  --------------
* ------ Vortragskonto aus eigenem Kontenplan angelegt ? --------------
* ------ zurück: SKA1, bei Fehler: MITTAB, GUVTAB-VTKON='*' -----------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          z_ktopl =  bukgestab-ktopb.  "evtl. für Feli-MITTAB
          PERFORM check_vortragskonto USING  bukgestab-bukges
                                             bukgestab-ktopb
                                             t030-komok
                                             guvtab-vtkon
                                             bukgestab-altsv.
        ENDIF.

* ------ Bukrs-Vortragskonto im Bukrs angelegt ? ----------------------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei
*
* AUSGESCHALTET DURCH PETER KUERPICK, 5.11.98
* ===========================================
*          if bukgestab-altsv = space  or
*             bukgestab-altsv = 2.
*            select single * from skb1
*                            where bukrs = bukgestab-bukges
*                            and   saknr = guvtab-vtkon.
*            if sy-subrc ne 0.          "VKonto ohne Stammsatz
*              mittab-racct  = guvtab-vtkon.     "im Bukrs.
*              mittab-bukges = bukgestab-bukges.
*              mittab-ktopl  = t030-ktopl.
*              mittab-gvtyp  = t030-komok.
*              mittab-text1  = text-008.
*              mittab-text2  = text-108.
*              append mittab.
*              guvtab-vtkon = '*'.      "Fehler
*            else.
*
* ------ Bukrs-Vortragskonto mit TWAER vortragen -> VTWTAB -----------
*              if skb1-xsalh = space.
*                if bukgestab-altsv = space.
*                  vtwtab-bukrs = skb1-bukrs.
*                  vtwtab-vtkon = guvtab-vtkon.
*                else.
*                  vtwtab-bukrs = skb1-bukrs.
*                  vtwtab-vtkon = skb1-altkt.     "ALTSV='2'
*                endif.
*                collect vtwtab.
*              endif.
*            endif.
*          endif.
        ENDIF.

* ------ alternative Konten - Konzernkontenplan ----------------------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          IF bukgestab-altsv = '1'.
            IF ska1-bilkt = space.
              mittab-racct  = guvtab-vtkon.     "SKA1-BILKT leer
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = t030-komok.
              mittab-text1  = TEXT-026.
              mittab-text2  = TEXT-114.
              APPEND mittab.
              guvtab-vtkon = '*'.      "Fehler
            ELSE.
              guvtab-vtkon = ska1-bilkt.
              guvtab-ktopl = bukgestab-ktopl.

* Konzernkonto (T004) in SKA1 angelegt ?
              z_ktopl = bukgestab-ktopl.      "evtl. für Feli-MITTAB
              PERFORM check_vortragskonto USING  bukgestab-bukges
                                                 bukgestab-ktopl
                                                 t030-komok
                                                 guvtab-vtkon
                                                 bukgestab-altsv.
            ENDIF.
          ENDIF.

* ------ alternative Konten - Landeskontenplan (lokaler) -------------
          IF bukgestab-altsv = '2'.
            IF skb1-altkt = space.
              mittab-racct  = guvtab-vtkon.     "SKB1-ALTKT leer
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = t030-komok.
              mittab-text1  = TEXT-027.
              mittab-text2  = TEXT-114.
              APPEND mittab.
              guvtab-vtkon = '*'.      "Fehler
            ELSE.
              guvtab-vtkon = skb1-altkt.
              guvtab-ktopl = bukgestab-ktopl.

* Landeskonto in SKA1 angelegt ?
              z_ktopl = bukgestab-ktopl.      "evtl. für Feli-MITTAB
              PERFORM check_vortragskonto USING  bukgestab-bukges
                                                 bukgestab-ktopl
                                                 t030-komok
                                                 guvtab-vtkon
                                                 bukgestab-altsv.
            ENDIF.
          ENDIF.
        ENDIF.

* ------ Vortragskonto  im Kontenplan als Bilanzkonto  angelegt? -----
        IF guvtab-vtkon NE '*'.        "bisher kein Fehler
          IF ska1-xbilk = space.       "kein Bilanzkonto
            mittab-racct  = guvtab-vtkon.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = z_ktopl.   "akt. SKA1-Ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-020.
            mittab-text2  = TEXT-120.
            APPEND mittab.
            guvtab-vtkon = '*'.        "VKonto kein Bilanzkonto
          ENDIF.
        ENDIF.

* ---------------------
        COLLECT guvtab.
      ENDSELECT.                                            "T030

      IF sy-subrc NE 0.                "kein Eintrag gefunden
        MESSAGE e625 WITH bukgestab-ktopb bukgestab-bukges.
      ENDIF.

* ------ Gesellschaft-Inf. zwischenspeichern -------------------------
    ELSE.

      SELECT * FROM  t030c
               WHERE ktopl = bukgestab-ktopl
               AND   ktosl = 'BIL'.
        guvtab-ktopl = t030c-ktopl.
        guvtab-gvtyp = t030c-komok.
        IF t030c-konts NE ' '.
          guvtab-vtkon = t030c-konts.
        ELSE.
          IF t030c-konth NE ' '.
            guvtab-vtkon = t030c-konth.
          ELSE.
*           MESSAGE E616 WITH T030C-KTOPL T030C-KOMOK.
            mittab-bukges = bukgestab-bukges.    "VKonto ist leer
            mittab-ktopl  = t030c-ktopl.
            mittab-gvtyp  = t030c-komok.
            mittab-text1  = TEXT-014.
            mittab-text2  = TEXT-114.
            APPEND mittab.
            guvtab-vtkon = '*'.
          ENDIF.
        ENDIF.

* ------ - Gesnr-VKonto in SKA1 angelegt ? ---------------------------
        IF guvtab-vtkon NE '*'.
          PERFORM check_vortragskonto USING  bukgestab-bukges
                                             bukgestab-ktopl
                                             t030c-komok
*                                            T001-KTOP2       "??
*                                            T030-KOMOK       "??
                                             guvtab-vtkon
                                             bukgestab-altsv.
        ENDIF.

* ------ Vortragskonten im Kontenplan als Bilanzkonten angelegt? -----
        IF guvtab-vtkon NE '*'.        "bisher kein Fehler
          IF ska1-xbilk = space.       "kein Bilanzkonto
            mittab-racct  = guvtab-vtkon.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = bukgestab-ktopl.
            mittab-gvtyp  = t030c-komok.
            mittab-text1  = TEXT-020.
            mittab-text2  = TEXT-120.
            APPEND mittab.
            guvtab-vtkon = '*'.        "VKonto kein Bilanzkonto
          ENDIF.
        ENDIF.

* -----------------
        COLLECT guvtab.
      ENDSELECT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    "FUELLEN_GUVTAB_NEW

*eject
*---------------------------------------------------------------------*
*       FORM FUELLEN_GUVTAB_RITA                                      *
*---------------------------------------------------------------------*
*       Die Ergebnisvortragskonten werden in GUVTAB gestellt.         *
*       Beim Bukrs kann es alternative Konten geben:
*       In Abhängigkeit, ob im Ledger alternative Konten abgelegt     *
*       sind (T882-ALTSV), wird das entsprechende Ergebnisvortrags-   *
*       konto für den alternativen Kontenplan verwendet.              *
*       In GUVTAB stehen der entsprechende (altern.) Ktopl. und das   *
*       Vortragskonto. Der GVTYP ist noch der originale aus dem       *
*       Bukrs-Vortragskonto (T030), weil ein anderer sowieso nicht    *
*       berücksichtigt wird -> da die Vortragskonten grundsätzlich    *
*       aus dem Bukrs-Vortragskonto aus T030 abgeleitet werden.       *
*       (nur das ist sinnvoll, weil auch alle anderen Kontenn, z.B.   *
*        in MM, abgeleitet werden.)                                   *
*       Füllen VTWTAB:
*       Für im Bukrs abgelegte Konten (ALTSV=' ' oder '2') wird       *
*       das Flag SKB1-XSALH pro Ergebnisvortragskonto überprüft.      *
*       Ist es nicht gesetzt, soll das Ergebnisvortragskonto
*       mit TWAER vorgetragen werden (Standard ist ohne TWAER,
*       Zusammenfassung auf HWAER). Diese Konten werden in der
*       Tabelle VTWTAB zwgesp.
*       Bei allen anderen Lokalen Ledgern und den globalen Ledgern
*       werden keine TWAER vorgetragen.                               *
*
*       Folgende Kontrollen des Ergebnisvortragskontos werden         *
*       durchgeführt:                                                 *
*       Bukrs:                                                        *
*       a) T882-ALTSV = space/2    ->     operativer Ktopl (Bukrs)    *
*         1. T030-KONT in SKA1 (T001-KTOPL)                           *
*         2. T030-KONT in SKB1 (T001-Bukrs)                           *
*         3. T030-KONT = Bilanzkonto                                  *
*       b) T882-ALTSV = 1          ->     Konzern-Ktopl               *
*         1. SKA1-BILKT vom T030-KONT in SKA1 (T004-KKTPL)            *
*         3. SKA1-BILKT = Bilanzkonto                                 *
*       c) T882-ALTSV = 2          ->     Landes-Ktopl (lokal)        *
*         1. SKB1-ALTKT vom T030-KONT in SKA1 (T001-KTOP2)            *
*         3. SKB1-ALTKT = Bilanzkonto                                 *
*       d) T882-ALTSV = 9          ->     eigener Ktopl               *
*         1. T030-KONT in SKA1 (T882-KTOPL)                           *
*         3. T030-KONT = Bilanzkonto                                  *
*       Gesnr:                                                        *
*         1. T030C-KONT in SKA1 (T882C-KTOPL)                          *
*         3. T030C-KONT = Bilanzkonto                                 *
*---------------------------------------------------------------------*
FORM fuellen_guvtab_rita.

*
* First get all GVTYP's occuring for the set of given company codes.
*
  LOOP AT bukgestab.
    SELECT * FROM t030
             WHERE ktopl = bukgestab-ktopb   "für alle GVTYP
             AND   ktosl = 'BIL'
             AND   bwmod = ' '.
      gvtyp_t030 = t030.
      COLLECT gvtyp_t030.
    ENDSELECT.
  ENDLOOP.
*
  LOOP AT bukgestab.

* ------ Buchungskreis-Inf. zwischenspeichern -------------------------
* -----( Fehler nicht sofort ausgeben, sondern erst als Liste in ------
* ------ CHECK_KONSISTENZ ) -------------------------------------------
    IF xcomptab = ' '.

* ------ Bukrs-Vortragskonto im Bukrs angelegt ? ----------------------
* ------ in GVTYP_T030 nur die relevanten GVTYP's zurückgeben ---------
      IF guvtab-vtkon NE '*'.          "VKonto bisher fehlerfrei

        IF bukgestab-altsv = space  OR
           bukgestab-altsv = '2'.
          PERFORM check_vkonto_in_bukrs.
        ENDIF.

      ENDIF.

* ------ Prüfungen nur noch auf Ktopl-Ebene nötig und möglich:---------
* ------ Ergebnisvortragskonto mit Bukrs- oder eigenem Ktopl ----------
      LOOP AT gvtyp_t030 WHERE ktosl NE '*'.    "nur relevante GVTYP's
        t030 = gvtyp_t030.

        guvtab-ktopl = t030-ktopl.     "??
        guvtab-gvtyp = t030-komok.
        IF t030-konts NE ' '.
          guvtab-vtkon = t030-konts.
        ELSE.
          IF t030-konth NE ' '.
            guvtab-vtkon = t030-konth.
          ELSE.                        "kein VT-Konto angegeben
            CHECK bukgestab-altsv NE space  AND    "bereits gesendet
                  bukgestab-altsv NE '2'.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = t030-ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-014.
            mittab-text2  = TEXT-114.
            APPEND mittab.
            guvtab-vtkon = '*'.
          ENDIF.
        ENDIF.

* ------ Bukrs-Vortragskonto in SKA1 (Kontenplan)   oder  --------------
* ------ Vortragskonto aus eigenem Ktopl im Kontenplan angelegt ? ------
* ------ zurück: SKA1, bei Fehler: MITTAB, GUVTAB-VTKON='*' -----------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          z_ktopl =  bukgestab-ktopb.  "evtl. für Feli-MITTAB
          PERFORM check_vortragskonto USING  bukgestab-bukges
                                             bukgestab-ktopb
                                             t030-komok
                                             guvtab-vtkon
                                             bukgestab-altsv.
        ENDIF.

* ------ alternative Konten - Konzernkontenplan ----------------------
        IF guvtab-vtkon NE '*'.        "VKonto bisher fehlerfrei

          IF bukgestab-altsv = '1'.
            IF ska1-bilkt = space.
              mittab-racct  = guvtab-vtkon.     "SKA1-BILKT leer
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = t030-komok.
              mittab-text1  = TEXT-026.
              mittab-text2  = TEXT-114.
              APPEND mittab.
              guvtab-vtkon = '*'.      "Fehler
            ELSE.
              guvtab-vtkon = ska1-bilkt.
              guvtab-ktopl = bukgestab-ktopl.

* Konzernkonto (T004) in SKA1 angelegt ?
              z_ktopl = bukgestab-ktopl.      "evtl. für Feli-MITTAB
              PERFORM check_vortragskonto USING  bukgestab-bukges
                                                 bukgestab-ktopl
                                                 t030-komok
                                                 guvtab-vtkon
                                                 bukgestab-altsv.
            ENDIF.
          ENDIF.

* ------ alternative Konten - Landeskontenplan (lokaler) -------------
          IF bukgestab-altsv = '2'.
            SELECT SINGLE * FROM skb1 "#EC CI_DB_OPERATION_OK[2431747]
                            WHERE bukrs = bukgestab-bukges
                            AND   saknr = guvtab-vtkon.
            IF skb1-altkt = space.
              guvtab-vtkon = '*'.      "relev. Fehler bereits ausgeg.
            ELSE.
              guvtab-vtkon = skb1-altkt.
              guvtab-ktopl = bukgestab-ktopl.
* Landes-Vortragkonto in SKA1 angelegt ?
              z_ktopl = bukgestab-ktopl.      "evtl. für Feli-MITTAB
              PERFORM check_vortragskonto USING  bukgestab-bukges
                                                 bukgestab-ktopl
                                                 t030-komok
                                                 guvtab-vtkon
                                                 bukgestab-altsv.
            ENDIF.
          ENDIF.
        ENDIF.

* ------ Vortragskonto  im Kontenplan als Bilanzkonto  angelegt? -----
        IF guvtab-vtkon NE '*'.        "bisher kein Fehler
          IF ska1-xbilk = space.       "kein Bilanzkonto
            mittab-racct  = guvtab-vtkon.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = z_ktopl.   "akt. SKA1-Ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-020.
            mittab-text2  = TEXT-120.
            APPEND mittab.
            guvtab-vtkon = '*'.        "VKonto kein Bilanzkonto
          ENDIF.
        ENDIF.

* ---------------------
        COLLECT guvtab.
      ENDLOOP.                         "T030 (evtl.bereinigt)

      IF sy-subrc NE 0.                "kein Eintrag gefunden
        MESSAGE e625 WITH bukgestab-ktopb bukgestab-bukges.
      ENDIF.

* ------ Gesellschaft-Inf. zwischenspeichern -------------------------
    ELSE.

      SELECT * FROM  t030c
               WHERE ktopl = bukgestab-ktopl
               AND   ktosl = 'BIL'.
        guvtab-ktopl = t030c-ktopl.
        guvtab-gvtyp = t030c-komok.
        IF t030c-konts NE ' '.
          guvtab-vtkon = t030c-konts.
        ELSE.
          IF t030c-konth NE ' '.
            guvtab-vtkon = t030c-konth.
          ELSE.
*           MESSAGE E616 WITH T030C-KTOPL T030C-KOMOK.
            mittab-bukges = bukgestab-bukges.    "VKonto ist leer
            mittab-ktopl  = t030c-ktopl.
            mittab-gvtyp  = t030c-komok.
            mittab-text1  = TEXT-014.
            mittab-text2  = TEXT-114.
            APPEND mittab.
            guvtab-vtkon = '*'.
          ENDIF.
        ENDIF.

* ------ - Gesnr-VKonto in SKA1 angelegt ? ---------------------------
        IF guvtab-vtkon NE '*'.
          PERFORM check_vortragskonto USING  bukgestab-bukges
                                             bukgestab-ktopl
                                             t030c-komok
*                                            T001-KTOP2       "??
*                                            T030-KOMOK       "??
                                             guvtab-vtkon
                                             bukgestab-altsv.
        ENDIF.

* ------ Vortragskonten im Kontenplan als Bilanzkonten angelegt? -----
        IF guvtab-vtkon NE '*'.        "bisher kein Fehler
          IF ska1-xbilk = space.       "kein Bilanzkonto
            mittab-racct  = guvtab-vtkon.
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = bukgestab-ktopl.
            mittab-gvtyp  = t030c-komok.
            mittab-text1  = TEXT-020.
            mittab-text2  = TEXT-120.
            APPEND mittab.
            guvtab-vtkon = '*'.        "VKonto kein Bilanzkonto
          ENDIF.
        ENDIF.

* -----------------
        COLLECT guvtab.
      ENDSELECT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    "FUELLEN_GUVTAB_RITA

*----------------- begin of insert -------------------------------------
*   -> aktuelle BUKGESTAB-Zeile
*   -> GVTYP_T030 = alle T030-Einträge für die relevanten Kontenpläne
*   <- GVTYP_T030 = nur noch relevante Einträge
*-----------------------------------------------------------------------
FORM check_vkonto_in_bukrs.

  DATA: BEGIN OF gvtyp_bukrs       OCCURS 0,
          bukrs LIKE skb1-bukrs,
          ktopl LIKE int_ska1-ktopl,
          gvtyp LIKE int_ska1-gvtyp,
        END OF gvtyp_bukrs.
*
* First get all GVTYP's occuring for the set of given company codes.
* Check which account uses which GVTYP.
* Gvtyp's + Konten aller relevanten Ktopl -> INT_SKA1
*
**IF XCOMPTAB = ' '.
**  LOOP AT BUKGESTAB.
**
**    LOOP AT GVTYP_T030 WHERE KTOPL = BUKGESTAB-KTOPB.
**      SELECT * FROM SKA1 WHERE KTOPL = GVTYP_T030-KTOPL
**                         AND   GVTYP = GVTYP_T030-KOMOK.
**        MOVE-CORRESPONDING SKA1 TO INT_SKA1.
**        APPEND INT_SKA1.
**      ENDSELECT.
**    ENDLOOP.

**  ENDLOOP.
**  SORT INT_SKA1.
**ENDIF.

** über alle Konten des Bukrs selektieren und gvtyp holen -> GVTYP_BUKRS
  SELECT * FROM skb1 WHERE bukrs = bukgestab-bukges. "#EC CI_DB_OPERATION_OK[2431747]

    READ TABLE int_ska1 WITH KEY ktopl = bukgestab-ktopb
                                 saknr = skb1-saknr     BINARY SEARCH.
    IF sy-subrc = 0.
      gvtyp_bukrs-bukrs = skb1-bukrs.
      gvtyp_bukrs-ktopl = int_ska1-ktopl.
      gvtyp_bukrs-gvtyp = int_ska1-gvtyp.
      COLLECT gvtyp_bukrs.
    ENDIF.
  ENDSELECT.

** prüfen, ob alle GVTYP_T030 für den Bukrs relevant sind.
** Nicht-relevante Gvtyp in GVTYP_T030 erhalten Lösch-KZ
** prüfen, ob Bukrs-Gvtyp in T030 ein Konto zugeordnet ist
  LOOP AT gvtyp_t030.

    READ TABLE gvtyp_bukrs WITH KEY ktopl = gvtyp_t030-ktopl
                                    gvtyp = gvtyp_t030-komok.
    IF sy-subrc NE 0.
      gvtyp_t030-ktosl = '*'.          "Eintrag gelöscht
      MODIFY gvtyp_t030.
      guvtab-vtkon = '*'.              "nächster Eintrag
    ELSE.
      t030 = gvtyp_t030.
      guvtab-ktopl = t030-ktopl.       "??
      guvtab-gvtyp = t030-komok.
      IF t030-konts NE ' '.
        guvtab-vtkon = t030-konts.
      ELSE.
        IF t030-konth NE ' '.
          guvtab-vtkon = t030-konth.
        ELSE.                          "kein VT-Konto angegeben
          mittab-bukges = bukgestab-bukges.
          mittab-ktopl  = t030-ktopl.
          mittab-gvtyp  = t030-komok.
          mittab-text1  = TEXT-014.
          mittab-text2  = TEXT-114.
          APPEND mittab.
          guvtab-vtkon = '*'.
        ENDIF.
      ENDIF.
    ENDIF.

* ------ Bukrs-Vortragskonto im Bukrs angelegt ? ----------------------
    IF guvtab-vtkon NE '*'.            "VKonto bisher fehlerfrei

      SELECT SINGLE * FROM skb1 "#EC CI_DB_OPERATION_OK[2431747]
                      WHERE bukrs = bukgestab-bukges
                      AND   saknr = guvtab-vtkon.
      IF sy-subrc NE 0.                "VKonto ohne Stammsatz im Bukrs
        mittab-racct  = guvtab-vtkon.
        mittab-bukges = bukgestab-bukges.
        mittab-ktopl  = t030-ktopl.
        mittab-gvtyp  = t030-komok.
        mittab-text1  = TEXT-008.
        mittab-text2  = TEXT-108.
        APPEND mittab.
        guvtab-vtkon = '*'.            "Fehler
      ELSE.

* ------ alternative Konten - Landeskontenplan (lokaler) angegeben ? -
        IF bukgestab-altsv = '2'.
          IF skb1-altkt = space.
            mittab-racct  = guvtab-vtkon.     "SKB1-ALTKT leer
            mittab-bukges = bukgestab-bukges.
            mittab-ktopl  = t030-ktopl.
            mittab-gvtyp  = t030-komok.
            mittab-text1  = TEXT-027.
            mittab-text2  = TEXT-114.
            APPEND mittab.
          ENDIF.
        ENDIF.

* ------ Bukrs-Vortragskonto mit TWAER vortragen -> VTWTAB -----------
        IF skb1-xsalh = space.
          IF bukgestab-altsv = space.
            vtwtab-bukrs = skb1-bukrs.
            vtwtab-vtkon = guvtab-vtkon.
          ELSE.
            vtwtab-bukrs = skb1-bukrs.
            vtwtab-vtkon = skb1-altkt.                      "ALTSV='2'
          ENDIF.
          COLLECT vtwtab.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.                             "GVTYP_BUKRS

ENDFORM.                    "CHECK_VKONTO_IN_BUKRS
* ----------------- end of insert ----------------------------------

*---------------------------------------------------------------------*
*       FORM set_alv_fieldcat1                                        *
*---------------------------------------------------------------------*
FORM set_alv_fieldcat1  CHANGING xt_fieldcat TYPE slis_t_fieldcat_alv.
  CONSTANTS :
    lc_strname_list LIKE dd02d-strname
                    VALUE 'FAGL_ACC_S_SAPFGVTR_LIST',
    lc_coltext      TYPE c VALUE 'M'.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

* get the field catalog from the structure
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = lc_strname_list
    CHANGING
      ct_fieldcat            = xt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Modify the Coloum Heading of the Output.
  READ TABLE xt_fieldcat INTO ls_fieldcat
    WITH KEY fieldname = 'RCOMP'.
  IF sy-subrc = 0.
    IF h_800a-comptab IS INITIAL.
      ls_fieldcat-seltext_m = TEXT-036 .         "Company code
    ELSE.
      ls_fieldcat-seltext_m = TEXT-037.          "Company
    ENDIF.
    ls_fieldcat-ddictxt = lc_coltext.
    MODIFY xt_fieldcat FROM ls_fieldcat
     TRANSPORTING seltext_m ddictxt WHERE fieldname = 'RCOMP'.
  ENDIF.

  READ TABLE xt_fieldcat INTO ls_fieldcat
    WITH KEY fieldname = 'GVTYP'.
  IF sy-subrc = 0.
    ls_fieldcat-seltext_m = TEXT-040 .
    ls_fieldcat-ddictxt = lc_coltext.
    MODIFY xt_fieldcat FROM ls_fieldcat
     TRANSPORTING seltext_m ddictxt WHERE fieldname = 'GVTYP'.
  ENDIF.

  READ TABLE xt_fieldcat INTO ls_fieldcat
    WITH KEY fieldname = 'TEXT'.
  IF sy-subrc = 0.
    ls_fieldcat-seltext_m = TEXT-041.
    ls_fieldcat-ddictxt = lc_coltext.
    MODIFY xt_fieldcat FROM ls_fieldcat
     TRANSPORTING seltext_m ddictxt WHERE fieldname = 'TEXT'.
  ENDIF.
ENDFORM.                    "set_alv_fieldcat1

*---------------------------------------------------------------------*
*       FORM set_alv_events1                                          *
*---------------------------------------------------------------------*
*       subroutine of Handling Events END_OF_LIST.                    *
*---------------------------------------------------------------------*
FORM set_alv_events1  CHANGING xt_events TYPE slis_t_event.
  DATA: ls_events TYPE slis_alv_event.
  CONSTANTS: lc_endoflist TYPE slis_alv_event-form VALUE
                                     'END_OF_LIST1'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = xt_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*Pass the subroutine name to the END-OF-LIST event when the end of list
*is triggered
  READ TABLE xt_events INTO ls_events WITH
       KEY name = slis_ev_end_of_list.
  IF sy-subrc = 0.
    ls_events-form = lc_endoflist.
    MODIFY xt_events FROM ls_events TRANSPORTING form
           WHERE name = slis_ev_end_of_list.
  ENDIF.
ENDFORM.                    "set_alv_events1

*---------------------------------------------------------------------*
*       FORM END_OF_LIST1                                             *
*---------------------------------------------------------------------*
*       Form Routine for END_OF_LIST.                                 *
*---------------------------------------------------------------------*
FORM end_of_list1 .                                         "#EC CALLED
  DATA: lt_list TYPE slis_t_listheader,
        ls_line TYPE slis_listheader.

* ------ Fehler bei Konsistenzcheck? -----------------------------------
  IF xkons = space.
    xaplstat = '4'.
    CLEAR ls_line.
    ls_line-typ  = 'H'.
    ls_line-info = TEXT-018.
    APPEND ls_line TO lt_list.
    CLEAR ls_line.
    APPEND ls_line TO lt_list.
    CLEAR ls_line.
    ls_line-typ  = 'H'.
    ls_line-info = TEXT-019.
    APPEND ls_line TO lt_list.
  ENDIF.

*To display END_OF_LIST.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list.
ENDFORM.                    " event_end_of_list
* ----------------- end of insert ----------------------------------

*---------------------------------------------------------------------*
*       FORM fill_guvtab                                              *
*---------------------------------------------------------------------*
* 1) GUVTAB has the 'real' T030 entries depending on T882-ALTSV
* (operative, local, group chart of accounts)
* They are checked in
* form process_record and posted to the database
*
* 2) The checks in this form here are ALWAYS done for the operative
* chart of account. For further info see OSS note 49994
*---------------------------------------------------------------------*
FORM fill_guvtab.
  DATA: ls_ska1 LIKE ska1.
  DATA: ls_ska1_alt LIKE ska1.  "Alternative account (ALTKT or BILKT)
  DATA: lt_skb1 LIKE skb1 OCCURS 0 WITH HEADER LINE.
  DATA: lt_ska1 LIKE ska1 OCCURS 0 WITH HEADER LINE.

  LOOP AT bukgestab.

********************1) BUKRS***************************
    IF xcomptab = ' '.
*Operative, local or group chart of account
      IF bukgestab-altsv NE '9'.
        SELECT * FROM skb1 INTO TABLE lt_skb1 "#EC CI_DB_OPERATION_OK[2431747]
           WHERE bukrs = bukgestab-bukges.
*Own chart of account
      ELSE.
        SELECT * FROM ska1 INTO TABLE lt_ska1 "#EC CI_DB_OPERATION_OK[2431747]
           WHERE ktopl = bukgestab-ktopb. "#EC CI_DB_OPERATION_OK[2389136]
      ENDIF.

*Now handle those 4 cases individually
      CASE bukgestab-altsv.

*******************Operative chart of accounts*******************
        WHEN ' '.
          LOOP AT lt_skb1.
*Get GVTYP for operative account
            CALL FUNCTION 'READ_KONTENPLAN'
              EXPORTING
                kontenplan    = bukgestab-ktopb
                sachkonto     = lt_skb1-saknr
              IMPORTING
                kontenplan_wa = ls_ska1
              EXCEPTIONS
                OTHERS        = 1.
            CHECK sy-subrc = 0 AND ls_ska1-gvtyp NE space.
*read operative T030 entry
            SELECT SINGLE * FROM t030
            WHERE ktopl = bukgestab-ktopb
            AND   ktosl = 'BIL'
            AND   bwmod = ' '
            AND   komok = ls_ska1-gvtyp.
            IF sy-subrc = 0.
*Check if RE account is OK
              SELECT SINGLE * FROM skb1 "#EC CI_DB_OPERATION_OK[2431747]
                          WHERE bukrs = bukgestab-bukges
                          AND   saknr = t030-konts.
              IF sy-subrc = 0.
                CALL FUNCTION 'READ_KONTENPLAN'
                  EXPORTING
                    kontenplan    = bukgestab-ktopb
                    sachkonto     = t030-konts
                  IMPORTING
                    kontenplan_wa = ska1
                  EXCEPTIONS
                    OTHERS        = 1.
*Is it a balance sheet account
                IF sy-subrc = 0 AND ska1-xbilk NE space.
                  guvtab-ktopl = bukgestab-ktopb.
                  guvtab-gvtyp = t030-komok.
                  guvtab-vtkon = t030-konts.
                  COLLECT guvtab.
                  IF skb1-xsalh = space.
                    vtwtab-bukrs = skb1-bukrs.
                    vtwtab-vtkon = t030-konts.
                  ENDIF.
*No balance sheet account
                ELSE.
                  mittab-racct  = t030-konts.
                  mittab-bukges = bukgestab-bukges.
                  mittab-ktopl  = t030-ktopl.
                  mittab-gvtyp  = t030-komok.
                  mittab-text1  = TEXT-020.
                  mittab-text2  = TEXT-120.
                  COLLECT mittab.
                ENDIF.
*Account not defined in BUKRS
              ELSE.
                mittab-racct  = t030-konts.
                mittab-bukges = bukgestab-bukges.
                mittab-ktopl  = t030-ktopl.
                mittab-gvtyp  = t030-komok.
                mittab-text1  = TEXT-008.
                mittab-text2  = TEXT-108.
                COLLECT mittab.
              ENDIF.
*No T030 entry
            ELSE.
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = t030-ktopl.
              mittab-gvtyp  = ls_ska1-gvtyp.
              mittab-text1  = TEXT-034.
              mittab-text2  = TEXT-134.
              COLLECT mittab.
            ENDIF.
          ENDLOOP.

********************Group chart of accounts**********************
        WHEN '1'.
          LOOP AT lt_skb1.
*Read SKA1 to see if account has BILKT filled
            CALL FUNCTION 'READ_KONTENPLAN'
              EXPORTING
                kontenplan    = bukgestab-ktopb
                sachkonto     = lt_skb1-saknr
              IMPORTING
                kontenplan_wa = ls_ska1
              EXCEPTIONS
                OTHERS        = 1.
            CHECK sy-subrc = 0 AND ls_ska1-bilkt NE space.
*get GVTYP for group account
            CALL FUNCTION 'READ_KONTENPLAN'
              EXPORTING
                kontenplan    = bukgestab-ktopl
                sachkonto     = ls_ska1-bilkt
              IMPORTING
                kontenplan_wa = ls_ska1_alt
              EXCEPTIONS
                OTHERS        = 1.
*Get GVTYP for operative account (for reading T030)
            CHECK sy-subrc = 0 AND ls_ska1_alt-gvtyp NE space.
            CALL FUNCTION 'READ_KONTENPLAN'
              EXPORTING
                kontenplan    = bukgestab-ktopb
                sachkonto     = lt_skb1-saknr
              IMPORTING
                kontenplan_wa = ls_ska1
              EXCEPTIONS
                OTHERS        = 1.
            CHECK sy-subrc = 0 AND ls_ska1-gvtyp NE space.
*read T030 for operative account with operative GVTYP
            SELECT SINGLE * FROM t030
            WHERE ktopl = bukgestab-ktopb
            AND   ktosl = 'BIL'
            AND   bwmod = ' '
            AND   komok = ls_ska1-gvtyp.
            IF sy-subrc = 0.
*now get the group account for operative RE account
              CALL FUNCTION 'READ_KONTENPLAN'
                EXPORTING
                  kontenplan    = bukgestab-ktopb
                  sachkonto     = t030-konts
                IMPORTING
                  kontenplan_wa = ska1
                EXCEPTIONS
                  OTHERS        = 1.
              IF sy-subrc = 0 AND ska1-xbilk NE space.
*Now fill GUVTAB with LOCAL informations
                IF ska1-bilkt NE space.
                  guvtab-ktopl = bukgestab-ktopl.
                  guvtab-gvtyp = ls_ska1_alt-gvtyp.
                  guvtab-vtkon = ska1-bilkt.
                  COLLECT guvtab.
                ELSE.
*No group account for RE account in SKA1-BILKT
                  mittab-racct  = t030-konts.
                  mittab-bukges = bukgestab-bukges.
                  mittab-ktopl  = bukgestab-ktopb.
                  mittab-gvtyp  = ls_ska1-gvtyp.
                  mittab-text1  = TEXT-026.
                  mittab-text2  = TEXT-114.
                  COLLECT mittab.
                ENDIF.
              ELSE.
*No balance sheet account
                mittab-racct  = t030-konts.
                mittab-bukges = bukgestab-bukges.
                mittab-ktopl  = t030-ktopl.
                mittab-gvtyp  = t030-komok.
                mittab-text1  = TEXT-020.
                mittab-text2  = TEXT-120.
                COLLECT mittab.
              ENDIF.
            ELSE.
*No entry in T030 for operative account
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = bukgestab-ktopb.
              mittab-gvtyp  = ls_ska1-gvtyp.
              mittab-text1  = TEXT-034.
              mittab-text2  = TEXT-134.
              COLLECT mittab.
            ENDIF..

          ENDLOOP.

********************Local chart of accounts**********************
        WHEN '2'.
          LOOP AT lt_skb1.
            CHECK lt_skb1-altkt NE space.
*get GVTYP for local account
            CALL FUNCTION 'READ_KONTENPLAN'
              EXPORTING
                kontenplan    = bukgestab-ktopl
                sachkonto     = lt_skb1-altkt
              IMPORTING
                kontenplan_wa = ls_ska1_alt
              EXCEPTIONS
                OTHERS        = 1.
*Get GVTYP for operative account (for reading T030)
            CHECK sy-subrc = 0 AND ls_ska1_alt-gvtyp NE space.
            CALL FUNCTION 'READ_KONTENPLAN'
              EXPORTING
                kontenplan    = bukgestab-ktopb
                sachkonto     = lt_skb1-saknr
              IMPORTING
                kontenplan_wa = ls_ska1
              EXCEPTIONS
                OTHERS        = 1.
            CHECK sy-subrc = 0 AND ls_ska1-gvtyp NE space.
*read T030 for operative account with operative GVTYP
            SELECT SINGLE * FROM t030
            WHERE ktopl = bukgestab-ktopb
            AND   ktosl = 'BIL'
            AND   bwmod = ' '
            AND   komok = ls_ska1-gvtyp.
            IF sy-subrc = 0.
*now get the local account for operative RE account
              SELECT SINGLE * FROM skb1 WHERE bukrs = bukgestab-bukges "#EC CI_DB_OPERATION_OK[2431747]
                                          AND saknr = t030-konts.
              IF sy-subrc = 0.
*Check if it is a balance sheet account
                CALL FUNCTION 'READ_KONTENPLAN'
                  EXPORTING
                    kontenplan    = bukgestab-ktopb
                    sachkonto     = t030-konts
                  IMPORTING
                    kontenplan_wa = ska1
                  EXCEPTIONS
                    OTHERS        = 1.
*It is a balance sheet account
                IF sy-subrc = 0 AND ska1-xbilk NE space.
*Now fill GUVTAB with LOCAL informations
                  IF skb1-altkt NE space.
                    guvtab-ktopl = bukgestab-ktopl.
                    guvtab-gvtyp = ls_ska1_alt-gvtyp.
                    guvtab-vtkon = skb1-altkt.
                    COLLECT guvtab.
                    IF skb1-xsalh = space.
                      vtwtab-bukrs = skb1-bukrs.
                      vtwtab-vtkon = skb1-altkt.
                    ENDIF.
                  ELSE.
*No local account for RE account in SKB1-ALTKT
                    mittab-racct  = skb1-saknr.
                    mittab-bukges = bukgestab-bukges.
                    mittab-ktopl  = bukgestab-ktopb.
                    mittab-gvtyp  = ls_ska1-gvtyp.
                    mittab-text1  = TEXT-027.
                    mittab-text2  = TEXT-114.
                    COLLECT mittab.
                  ENDIF.
                ELSE.
*No balance sheet account
                  mittab-racct  = t030-konts.
                  mittab-bukges = bukgestab-bukges.
                  mittab-ktopl  = t030-ktopl.
                  mittab-gvtyp  = t030-komok.
                  mittab-text1  = TEXT-020.
                  mittab-text2  = TEXT-120.
                  COLLECT mittab.
                ENDIF.
              ELSE.
*Not defined in BUKRS
                mittab-racct  = t030-konts.  "not defined in BUKRS
                mittab-bukges = bukgestab-bukges.
                mittab-ktopl  = t030-ktopl.
                mittab-gvtyp  = t030-komok.
                mittab-text1  = TEXT-008.
                mittab-text2  = TEXT-108.
                COLLECT mittab.
              ENDIF.
            ELSE.
*No entry in T030 for operative account
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = bukgestab-ktopb.
              mittab-gvtyp  = ls_ska1-gvtyp.
              mittab-text1  = TEXT-034.
              mittab-text2  = TEXT-134.
              COLLECT mittab.
            ENDIF..
          ENDLOOP.
*********************Own chart of accounts*******************
        WHEN '9'.
          LOOP AT lt_ska1 WHERE gvtyp NE space.
*read T030 for own account
            SELECT SINGLE * FROM t030
            WHERE ktopl = bukgestab-ktopb
            AND   ktosl = 'BIL'
            AND   bwmod = ' '
            AND   komok = lt_ska1-gvtyp.
            IF sy-subrc = 0.
*Check if RE account is a balance sheet account
              CALL FUNCTION 'READ_KONTENPLAN'
                EXPORTING
                  kontenplan    = bukgestab-ktopb
                  sachkonto     = t030-konts
                IMPORTING
                  kontenplan_wa = ls_ska1
                EXCEPTIONS
                  OTHERS        = 1.
*It is a balance sheet account
              IF sy-subrc = 0 AND ls_ska1-xbilk NE space.
*Now fill GUVTAB with LOCAL informations
                guvtab-ktopl = bukgestab-ktopb.
                guvtab-gvtyp = lt_ska1-gvtyp.
                guvtab-vtkon = t030-konts.
                COLLECT guvtab.
              ELSE.
*No balance sheet account
                mittab-racct  = t030-konts.
                mittab-bukges = bukgestab-bukges.
                mittab-ktopl  = t030-ktopl.
                mittab-gvtyp  = t030-komok.
                mittab-text1  = TEXT-020.
                mittab-text2  = TEXT-120.
                COLLECT mittab.
              ENDIF.
            ELSE.
*No entry in T030 for operative account
              mittab-bukges = bukgestab-bukges.
              mittab-ktopl  = bukgestab-ktopb.
              mittab-gvtyp  = lt_ska1-gvtyp.
              mittab-text1  = TEXT-034.
              mittab-text2  = TEXT-134.
              COLLECT mittab.
            ENDIF..
          ENDLOOP.
      ENDCASE.
********************1) RCOMP***************************
    ELSE.
      SELECT * FROM  t030c
               WHERE ktopl = bukgestab-ktopl
               AND   ktosl = 'BIL'.

*Check if RE account is a balance sheet account
        CALL FUNCTION 'READ_KONTENPLAN'
          EXPORTING
            kontenplan    = bukgestab-ktopl
            sachkonto     = t030c-konts
          IMPORTING
            kontenplan_wa = ls_ska1
          EXCEPTIONS
            OTHERS        = 1.

        IF sy-subrc = 0 AND ls_SKA1-xbilk NE space.
          guvtab-ktopl = t030c-ktopl.
          guvtab-gvtyp = t030c-komok.
          guvtab-vtkon = t030c-konts.
          COLLECT guvtab.
        ELSE.
          mittab-racct  = t030c-konts.
          mittab-bukges = bukgestab-bukges.
          mittab-ktopl  = bukgestab-ktopl.
          mittab-gvtyp  = t030c-komok.
          mittab-text1  = TEXT-020.
          mittab-text2  = TEXT-120.
          COLLECT mittab.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_NEWGL_MIGRATION
*&---------------------------------------------------------------------*
*       Checks regarding NewGL migration plans/status:
*       The logic takes into consideration, that there are several
*       migration plans for one ledger/comp.code combination
*----------------------------------------------------------------------*
FORM check_newgl_migration.
  DATA: lt_t001  LIKE t001 OCCURS 0 WITH HEADER LINE,
        lt_bukrs TYPE fagl_t_bukrs.
  DATA: lt_fagl_mgpln           TYPE fagl_t_mgpln,
        ls_fagl_mgpln           TYPE fagl_mgpln,
        lt_fagl_mig_001         TYPE fagl_t_mig_001,
        ls_fagl_mig_001         TYPE fagl_s_mig_001,
        lt_fagl_mig_001_mgpln   TYPE fagl_t_mig_001,
        lt_fagl_mig_002         TYPE fagl_t_mig_002,
        ls_fagl_mig_002         TYPE fagl_s_mig_002,
        lt_fagl_mig_001_started TYPE fagl_t_mig_001,
        ls_fagl_mig_001_started TYPE fagl_s_mig_001.
  DATA: lf_mig_gjahr            LIKE bkpf-gjahr.
  DATA: lf_fagl_write_glt0    TYPE fagl_write_glt0,
        lf_fagl_glflex_active TYPE c.
  DATA: lf_xmgpln_started_exist TYPE c,
        lf_xfbcb_exist          TYPE c.

  CHECK xcomptab IS INITIAL.
  CHECK p_appl = appl-figlf OR p_appl = appl-figl.

* Select company codes
  SELECT * FROM t001 INTO TABLE lt_t001
                     WHERE bukrs IN bukrs.
  IF sy-subrc NE 0.
    MESSAGE e607.
  ENDIF.
*
* ---------------------------------------------------------------------
* Checks for New G/L (transaction FAGLGVTR)
* ---------------------------------------------------------------------
*
  IF p_appl = appl-figlf.
    LOOP AT lt_t001.
* Get migration plan(s) for ledger/company code
      REFRESH: lt_fagl_mig_001_started.
      CALL FUNCTION 'FAGL_GET_ACTIVE_MGPLN'
        EXPORTING
          i_bukrs             = lt_t001-bukrs
          i_rldnr             = ledger
        IMPORTING
          et_fagl_mig_001     = lt_fagl_mig_001   "mig.plan attribts.
          et_fagl_mig_002     = lt_fagl_mig_002   "ledger/comp.code
          ef_active_mgpln     = ls_fagl_mgpln
        EXCEPTIONS
          invalid_input       = 1
          no_data_found       = 2
          invalid_customizing = 3
          OTHERS              = 4.
* No migration plan data found: Continue with next company code
      IF sy-subrc = 2.
        CONTINUE.
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* Migration plan data found:
* Only migration plans with flag MG_START = 'X' are relevant
      REFRESH: lt_fagl_mig_001_started.
      CLEAR: ls_fagl_mig_001_started,
             lf_xmgpln_started_exist.
      LOOP AT lt_fagl_mig_001 INTO  ls_fagl_mig_001_started
                              WHERE mg_start IS NOT INITIAL.
        APPEND ls_fagl_mig_001_started TO lt_fagl_mig_001_started.
        lf_xmgpln_started_exist = 'X'.
      ENDLOOP.

* If no migration plan with mg_start='X' in ldgr/comp.code
* -> continue with next company code
      IF lf_xmgpln_started_exist IS INITIAL.
        CONTINUE.
      ENDIF.


* Sort mig.plan data descending by migr.date to get latest plan first.
      SORT lt_fagl_mig_001_started BY migdt DESCENDING.
      CLEAR ls_fagl_mig_001_started.

      LOOP AT lt_fagl_mig_001_started INTO ls_fagl_mig_001_started.
* Determine fiscal year of mig.date
        CALL FUNCTION 'FAGL_GET_INFO_FROM_LEDGER'
          EXPORTING
            i_budat        = ls_fagl_mig_001_started-migdt
            i_rldnr        = ledger
            i_bukrs        = lt_t001-bukrs
          IMPORTING
            e_gjahr        = lf_mig_gjahr
          EXCEPTIONS
            no_info_found  = 1
            error_in_setup = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

* Do not allow b/c forward for years less/equal year of mig.date
        IF newjr <= lf_mig_gjahr.
          MESSAGE e400(fagl_mig) WITH ledger
                                      lt_t001-bukrs
                                      ls_fagl_mig_001_started-migdt
                                      lf_mig_gjahr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
*
* ---------------------------------------------------------------------
* Checks for Classic G/L (transaction F.16)
* ---------------------------------------------------------------------
*
  IF p_appl = appl-figl.
* No b/c foward in GLT0 in case 'write GLT0' is not active
    CALL FUNCTION 'FAGL_GET_GLT0_SETTING'
      IMPORTING
        ed_write_glt0 = lf_fagl_write_glt0.
    IF lf_fagl_write_glt0 IS INITIAL.
      MESSAGE e401(fagl_mig).
    ENDIF.


* Process specified company codes
    LOOP AT lt_t001.
      REFRESH: lt_fagl_mgpln,
               lt_fagl_mig_001_mgpln,
               lt_fagl_mig_001,
               lt_fagl_mig_002,
               lt_bukrs.
      CLEAR: lf_fagl_glflex_active.

* Get flag whether NewGL is active
      CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
        EXPORTING
          id_bukrs        = lt_t001-bukrs
        IMPORTING
          e_glflex_active = lf_fagl_glflex_active
*         E_GLFLEX_MIG_ACTIVE       =
        EXCEPTIONS
          error_in_setup  = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* Get mig.plans and mig.plan data for company code
      APPEND lt_t001-bukrs TO lt_bukrs.
      CALL FUNCTION 'FAGL_MIG_GET_MGPLN_FOR_BUKRS'
        EXPORTING
          it_bukrs        = lt_bukrs
        IMPORTING
          et_fagl_mig_002 = lt_fagl_mig_002
          et_mgpln        = lt_fagl_mgpln
        EXCEPTIONS
          no_data_found   = 1
          OTHERS          = 2.
* No migration plan data found: Continue with next company code
      IF sy-subrc = 1.
        CONTINUE.
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* Get migration plan data
      LOOP AT lt_fagl_mgpln INTO ls_fagl_mgpln.
        CALL FUNCTION 'FAGL_GET_DATA_FOR_MGPLN'
          EXPORTING
            i_mgpln         = ls_fagl_mgpln
          IMPORTING
            et_fagl_mig_001 = lt_fagl_mig_001_mgpln
*           E_MIGDT         =
          EXCEPTIONS
            mgpln_not_found = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        APPEND LINES OF lt_fagl_mig_001_mgpln TO lt_fagl_mig_001.
      ENDLOOP.

* Only migration plans with flag MG_START = 'X' are relevant
      REFRESH: lt_fagl_mig_001_started.
      CLEAR: ls_fagl_mig_001_started,
             lf_xmgpln_started_exist.
      LOOP AT lt_fagl_mig_001 INTO  ls_fagl_mig_001_started
                              WHERE mg_start IS NOT INITIAL.
        APPEND ls_fagl_mig_001_started TO lt_fagl_mig_001_started.
        lf_xmgpln_started_exist = 'X'.
      ENDLOOP.

* If no migration plan with mg_start='X' in ldgr/comp.code
* -> continue with next company code
      IF lf_xmgpln_started_exist IS INITIAL.
        CONTINUE.
      ENDIF.

* Sort mig.plan data descending by migr.date to get latest plan first.
      SORT lt_fagl_mig_001_started BY migdt DESCENDING.
      CLEAR ls_fagl_mig_001_started.

      LOOP AT lt_fagl_mig_001_started INTO ls_fagl_mig_001_started.
* Determine fiscal year of mig.date
        CALL FUNCTION 'FAGL_GET_INFO_FROM_LEDGER'
          EXPORTING
            i_budat        = ls_fagl_mig_001_started-migdt
            i_rldnr        = ledger
            i_bukrs        = lt_t001-bukrs
          IMPORTING
            e_gjahr        = lf_mig_gjahr
          EXCEPTIONS
            no_info_found  = 1
            error_in_setup = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
* Do not allow b/c forward for years less/equal year of mig.date if
* NewGL is active
        IF lf_fagl_glflex_active IS NOT INITIAL.
          IF newjr <= lf_mig_gjahr.
            MESSAGE e403(fagl_mig) WITH lt_t001-bukrs
                                        ls_fagl_mig_001_started-migdt
                                        lf_mig_gjahr.
          ENDIF.
        ELSE.
* Do not allow b/c forward if FBCB postings in period 0 of the
* new year are already exising in NewGL
          IF newjr <= lf_mig_gjahr.
            PERFORM check_fbcb_posting_exist USING lt_t001-bukrs
                                                   newjr
                                                   lf_xfbcb_exist.
            IF lf_xfbcb_exist IS NOT INITIAL.
              MESSAGE e402(fagl_mig) WITH lt_t001-bukrs newjr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHECK_NEWGL_MIGRATION

*&---------------------------------------------------------------------*
*&      Form  check_fbcb_posting_exist
*&---------------------------------------------------------------------*
FORM check_fbcb_posting_exist  USING p_bukrs      TYPE bukrs
                                     p_gjahr      TYPE gjahr
                            CHANGING p_pstg_exist TYPE c.
  DATA: ls_bkpf TYPE bkpf.

* Select FBCB documents (Posting period 0) from BSEG
  SELECT SINGLE * FROM bkpf INTO  ls_bkpf
                  WHERE bukrs = p_bukrs
                  AND   gjahr = p_gjahr
                  AND ( bstat = space OR bstat = 'L' )
                  AND   monat = 00         "FBCB posts in month 00
                  AND   stblg = space.
  IF sy-subrc = 0.
    p_pstg_exist = 'X'.
  ELSE.
    CLEAR p_pstg_exist.
  ENDIF.
ENDFORM.                    " check_fbcb_posting_exist
*&---------------------------------------------------------------------*
*&      Form  GET_RECCORDTYPES
*&---------------------------------------------------------------------*
*       Get List of reccordtypes
*----------------------------------------------------------------------*
*      <-- CT_RRCTY  List of reccord types
*----------------------------------------------------------------------*
FORM get_reccordtypes CHANGING ct_rrcty TYPE t_rrcty.

  DATA:
    ls_dd07v TYPE dd07v,
    lt_dd07v TYPE STANDARD TABLE OF dd07v,
    ls_rrcty TYPE s_rrcty.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'RRCTY'
    TABLES
      dd07v_tab     = lt_dd07v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  LOOP AT lt_dd07v INTO ls_dd07v.
    MOVE ls_dd07v-domvalue_l TO ls_rrcty-rrcty.
    APPEND ls_rrcty TO ct_rrcty.
  ENDLOOP.

ENDFORM.                    " GET_RECCORDTYPES
*---------------------------------------------------------------------*
*       FORM CHECK_ARCHIVE                                            *
*---------------------------------------------------------------------*
*       Prüfen, ob im alten Jahr archiviert wurde                     *
*---------------------------------------------------------------------*
FORM CHECK_archive USING ct_version_data TYPE t_version_data..
  DATA: ld_plank,
        ld_archived,
        ld_bukrs    LIKE t001-bukrs,
        ld_rcomp    LIKE t880-rcomp.
  DATA: ls_version_data TYPE glx_version_info.
  DATA: ls_rrcty TYPE s_rrcty,
        lt_rrcty TYPE t_rrcty.

  PERFORM get_reccordtypes CHANGING lt_rrcty.

  CASE p_appl.
    WHEN appl-figlf OR appl-figl.
      LOOP AT lt_rrcty INTO ls_rrcty
                       WHERE rrcty = '0'
                          OR rrcty = '1'.
        CHECK ls_rrcty-rrcty IN satztyp.
        IF ls_rrcty = '0'.
          ld_PLANK = space.
        ELSE.
          ld_plank = 'X'.
        ENDIF.
        CALL FUNCTION 'FI_TF_CHECK_SELECTION'
          EXPORTING
            i_bukrs = bukrs[]
            i_gjahr = oldjr
            i_sakon = 'X'
            i_PLANK = ld_plank.
      ENDLOOP.
    WHEN appl-fisl OR appl-ecpca.
      LOOP AT lt_rrcty INTO ls_rrcty.
        CHECK ls_rrcty-rrcty IN satztyp.
        LOOP AT ct_version_data INTO ls_version_data.
          LOOP AT bukgestab.
            IF h_800a-comptab IS INITIAL.
              ld_bukrs = bukgestab-bukges.
            ELSE.
              ld_rcomp = bukgestab-bukges.
            ENDIF.
            CALL FUNCTION 'SUMMARY_REC_ARCHIVED_CHECK'
              EXPORTING
                iv_rldnr              = h_881-rldnr
                iv_bukrs              = ld_bukrs
                iv_rcomp              = ld_rcomp
                iv_ryear              = oldjr
                iv_rrcty              = ls_rrcty-rrcty
                iv_rvers              = ls_version_data-rvers
              EXCEPTIONS
                archiving_in_progress = 1.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "CHECK_archive

*---------------------------------------------------------------------*
*       FORM CHECK_line_items                                         *
*---------------------------------------------------------------------*
*       Prüfen, ob im neuen Jahr in Periode 0 Einzelposten da         *
*---------------------------------------------------------------------*
FORM CHECK_line_items.
  DATA: ld_plank,
        ld_bukrs LIKE t001-bukrs,
        ld_rcomp LIKE t880-rcomp,
        ld_docnr LIKE glu1-docnr.

  EXIT. "not done anymore because of performance problems

  CHECK h_800a-dim_orgunit = 'RBUKRS'
    OR  h_800a-dim_orgunit = 'RCOMP'.

  CHECK '0' IN satztyp
     OR '2' IN satztyp.

  IF h_800a-comptab IS INITIAL.
    SELECT SINGLE docnr FROM (h_800a-ntable) INTO ld_docnr
           WHERE rldnr = h_881-rldnr
             AND rrcty IN satztyp
             AND rvers IN version
             AND rbukrs IN bukrs
             AND ryear = newjr
             AND poper = '000'.
  ELSE.
    SELECT SINGLE docnr FROM (h_800a-ntable) INTO ld_docnr
           WHERE rldnr = h_881-rldnr
             AND rrcty IN satztyp
             AND rvers IN version
             AND rcomp IN rcomp
             AND ryear = newjr
             AND poper = '000'.
  ENDIF.
  IF sy-subrc = 0.
    MESSAGE w634 WITH newjr h_881-rldnr ld_docnr.
  ENDIF.
ENDFORM.                    "CHECK_line_items

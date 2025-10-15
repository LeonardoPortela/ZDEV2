************************************************************************
*                                                                      *
*           Verwaltung der (Tabellen) Protokoll Datenbank              *
*                     (Funktion: Löschen)                              *
*                                                                      *
************************************************************************

************************************************************************
* Änderungshistorie
*
* Kennung Datum      Name         Beschreibung
*-----------------------------------------------------------------------
* CHNG01  08.08.1997 KOLLNISCHKO  Umstellung von DBTABPRT (Release 3.X)
*                                 nach DBTABLOG (Release 4.0)
* UF270598           FRENZEL      COMMIT WORK nach 2000 gelöschten
*                                 Sätzen analog Hinweis 41300
*                                 "Änderungsbeleg" in allen Meldungen
*                                 durch "Änderungsprotokoll" ersetzt.
* 05.11.2003         FRENZELU     Abfragen bei Batch Input unterdrückt.
*-----------------------------------------------------------------------
************************************************************************

REPORT zrstbpdel MESSAGE-ID tb.
TABLES: dbtablog,                                           "chng01
        strmpar,
        usr21,
        tddat.
TYPES: BEGIN OF dbtablog_key_type,
         logdate LIKE dbtablog-logdate,
         logtime LIKE dbtablog-logtime,
         logid   LIKE dbtablog-logid,
       END OF dbtablog_key_type.

TYPES: BEGIN OF dbtablog_type,
         logdate   LIKE dbtablog-logdate,
         logtime   LIKE dbtablog-logtime,
         logid     LIKE dbtablog-logid,
         username  LIKE dbtablog-username,
       END OF dbtablog_type.

DATA: antwort,
      i              TYPE i,
      cnt_loops      TYPE i,                                "UF270598
      i_dbtablog     TYPE TABLE OF dbtablog_type,
      i_dbtablog_key TYPE TABLE OF dbtablog_key_type
       INITIAL SIZE 2000.                                   "UF270598
DATA:    subrc LIKE sy-subrc.

TYPES:
  BEGIN OF gs_tabname,
    tabname  TYPE dbtablog-tabname,
  END OF gs_tabname,
  BEGIN OF gs_syslog,
    program TYPE c LENGTH 8,      "only short report name - 8 char.
    text    TYPE c LENGTH 24,
  END OF gs_syslog.

DATA:
  gt_tabn      TYPE STANDARD TABLE OF gs_tabname,
  gr_tabn      TYPE REF TO gs_tabname,
  gd_tbx       TYPE sy-tabix,
  gd_full_auth TYPE boole_d,
  gs_syslog    TYPE gs_syslog,
  ld_descr     TYPE c LENGTH 85,
  lv_subrc     TYPE sy-subrc.


SELECT-OPTIONS: edatum FOR strmpar-tbscdlda. "DEFAULT '19920706'. UF270598
SELECT-OPTIONS: euser  FOR usr21-bname no INTERVALS. "DEFAULT '19920706'. UF270598
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
         tabellen            FOR dbtablog-tabname.          "chng01

************************************************************************
*                            zeitpunkte                                *
***********************************************************alphabetisch*


********************************************************* initialization

INITIALIZATION.

  edatum = sy-datum - 365.                                  "UF270598

***************************************************** start-of-selection

START-OF-SELECTION.

*  IF sy-tcode <> 'SCU3_DEL'.
*    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
*      EXPORTING
*        tcode  = 'SCU3_DEL'
*      EXCEPTIONS
*        ok     = 1
*        not_ok = 2
*        OTHERS = 3.
*
*    IF sy-subrc <> 1.
*      MESSAGE e172(00) WITH 'SCU3_DEL'.
*    ENDIF.
*  ENDIF.

* Start of Insertion YI3K092288.
*  CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
*    EXPORTING
*      view_action                    = 'U'
*      view_name                      = 'DBTABLOG'
*    EXCEPTIONS
*      invalid_action                 = 1
*      no_authority                   = 2
*      no_clientindependent_authority = 3
*      table_not_found                = 4
*      no_linedependent_authority     = 5
*      OTHERS                         = 6.
*
*  IF sy-subrc = 0.
*    gd_full_auth = abap_true.
*  ENDIF. " IF sy-subrc = 0

  "we need a detailed authorization check
  SELECT DISTINCT tabname FROM dbtablog INTO TABLE gt_tabn
    WHERE tabname  IN tabellen
      and logdate  IN edatum
      AND username in euser.

*  IF gt_tabn[] IS NOT INITIAL AND gd_full_auth = abap_false. " X
*    LOOP AT gt_tabn REFERENCE INTO gr_tabn.
*      gd_tbx = sy-tabix.
*      CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
*        EXPORTING
*          view_action                    = 'U'
*          view_name                      = gr_tabn->tabname
*        EXCEPTIONS
*          invalid_action                 = 1
*          no_authority                   = 2
*          no_clientindependent_authority = 3
*          table_not_found                = 4
*          no_linedependent_authority     = 5
*          OTHERS                         = 6.
*      IF sy-subrc <> 0.
*        "create a messsage for joblog
*        "in dialog break after 3 failures
*
*        DELETE gt_tabn INDEX gd_tbx.
*      ENDIF.
*    ENDLOOP.
*
*    IF gt_tabn IS INITIAL.
*      MESSAGE s338.
*      RETURN.
*    ENDIF.
*
*  ENDIF. " IF sy-subrc = 0 AND gd_full_auth = abap_false


  IF sy-batch IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Exclusão logs'
        text_question         = 'Confirma exclusão logs'
        default_button        = '2'
        display_cancel_button = ' '
      IMPORTING
        answer                = antwort
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.


  IF antwort = '1' OR sy-batch IS NOT INITIAL.

    CLEAR: sy-subrc, gd_tbx.
    LOOP AT gt_tabn REFERENCE INTO gr_tabn.

      i = 0.
      WHILE lv_subrc = 0.                                   "IG 955319
        SELECT logdate logtime logid username FROM dbtablog
         INTO TABLE i_dbtablog
          UP TO 20000 ROWS
         WHERE tabname  EQ gr_tabn->tabname
           AND logdate  IN edatum
           AND username in euser.

        IF i_dbtablog[] is NOT INITIAL.

          MOVE-CORRESPONDING i_dbtablog to i_dbtablog_key.

          lv_subrc = 0.
          DELETE dbtablog FROM TABLE i_dbtablog_key.
          COMMIT WORK.
          i = i + sy-dbcnt.

        ELSE.
          lv_subrc = 4.
        ENDIF.
      ENDWHILE.

      CLEAR: lv_subrc.

      IF i > 0.
        "SysLog: &1 Change Records of Table &2 deleted (Deleted till &3)
        "AuditLog: &1 Change Records of Table &2 deleted (Deleted till &3)
        "Perf.Indicator or JobLog
        ld_descr = 'DBTABLOG - Table &1'.
        REPLACE '&1' IN ld_descr WITH gr_tabn->tabname.

        CALL FUNCTION 'RSAU_WRITE_CHDOC_EVTS'
          EXPORTING
            id_event   = 'EU3'
            id_cnt     = i
            id_txt_arg = ld_descr.


        CALL FUNCTION 'RSLG_WRITE_SYSLOG_ENTRY'
          EXPORTING
            sl_message_area  = 'EU'
            sl_message_subid = '3'
            data_area        = ld_descr.

        ADD i TO gd_tbx.
      ENDIF.

    ENDLOOP.
    MESSAGE i400 WITH gd_tbx.               "UF270598end
  ENDIF.

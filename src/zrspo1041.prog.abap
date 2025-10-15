REPORT zrspo1041 MESSAGE-ID po LINE-SIZE 80 LINE-COUNT 0(1).
INCLUDE ZRSPOTIME.
INCLUDE ZRSPOERROR.
INCLUDE Z<ICON>.
INCLUDE ZRSPOOPT.
****INCLUDE: rspotime, rspoerror, <icon>, rspoopt.
TYPE-POOLS: sp01r.

TABLES: tsp01, tsp01_sp0r, sscrfields.
DATA: syslist LIKE alsysid OCCURS 10 WITH HEADER LINE.
DATA: tsp01_list LIKE tsp01sys OCCURS 1000 WITH HEADER LINE,
      tsp01_result LIKE tsp01sys OCCURS 1000 WITH HEADER LINE,
      tsp02_list LIKE tsp02sys OCCURS 100 WITH HEADER LINE.
DATA: utc_now LIKE tsp01-rqmodtime,
      idlist TYPE sp01r_id_list WITH HEADER LINE.
DATA: text(132), num(10), ldate LIKE sy-datum, ltime LIKE sy-uzeit,
      none, tno TYPE i, tno_total(10), sclient, suser.
DATA: facdate LIKE scal-facdate, opt, cal LIKE sy-subrc.

SELECTION-SCREEN BEGIN OF BLOCK how_to_sel WITH FRAME TITLE text-100.

SELECTION-SCREEN BEGIN OF BLOCK without WITH FRAME TITLE text-114.
PARAMETERS age1 TYPE c DEFAULT ' ' AS CHECKBOX.
PARAMETERS min_alt1(3) DEFAULT ' 10'.
PARAMETERS and1 RADIOBUTTON GROUP and1.
PARAMETERS or1 DEFAULT 'X' RADIOBUTTON GROUP and1.
PARAMETERS old1 DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK without.

SELECTION-SCREEN BEGIN OF BLOCK working WITH FRAME TITLE text-116.
PARAMETERS age2 TYPE c DEFAULT ' ' AS CHECKBOX.
PARAMETERS min_alt2(3) DEFAULT ' 30'.
PARAMETERS and2 RADIOBUTTON GROUP and2.
PARAMETERS or2 DEFAULT 'X' RADIOBUTTON GROUP and2.
PARAMETERS old2 DEFAULT ' ' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK working.

SELECTION-SCREEN BEGIN OF BLOCK ready WITH FRAME TITLE text-113.
PARAMETERS age3 TYPE c DEFAULT ' ' AS CHECKBOX.
PARAMETERS min_alt3(3) DEFAULT ' 10'.
PARAMETERS and3 RADIOBUTTON GROUP and3.
PARAMETERS or3 DEFAULT 'X' RADIOBUTTON GROUP and3.
PARAMETERS old3 DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK ready.

SELECTION-SCREEN BEGIN OF BLOCK error WITH FRAME TITLE text-115.
PARAMETERS age4 TYPE c DEFAULT ' ' AS CHECKBOX.
PARAMETERS min_alt4(3) DEFAULT ' 30'.
PARAMETERS and4 RADIOBUTTON GROUP and4.
PARAMETERS or4 DEFAULT 'X' RADIOBUTTON GROUP and4.
PARAMETERS old4 DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK error.

SELECTION-SCREEN BEGIN OF BLOCK calendar WITH FRAME TITLE text-132.
PARAMETERS pcalid LIKE scal-fcalid DEFAULT '01'.
PARAMETERS working DEFAULT 'X' RADIOBUTTON GROUP w.
PARAMETERS days RADIOBUTTON GROUP w.
SELECTION-SCREEN END OF BLOCK calendar.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK name WITH FRAME TITLE text-110.
SELECT-OPTIONS system FOR sy-sysid NO INTERVALS DEFAULT sy-sysid.
SELECT-OPTIONS client FOR sy-mandt NO INTERVALS DEFAULT sy-mandt
               MODIF ID cli.
SELECT-OPTIONS uname FOR sy-uname NO INTERVALS DEFAULT sy-uname
               MODIF ID usr.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS title FOR tsp01-rqtitle NO INTERVALS.
SELECT-OPTIONS suffix0 FOR tsp01-rq0name NO INTERVALS.
SELECT-OPTIONS suffix1 FOR tsp01-rq1name NO INTERVALS.
SELECT-OPTIONS suffix2 FOR tsp01-rq2name NO INTERVALS.
SELECT-OPTIONS printer FOR tsp01_sp0r-rqdestl NO INTERVALS.

PARAMETERS nonex TYPE c DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK name.

SELECTION-SCREEN END OF BLOCK how_to_sel.

PARAMETERS: listonly             DEFAULT ' '      AS CHECKBOX.
PARAMETERS: commit_a(5) TYPE n      DEFAULT '50'.

INITIALIZATION. "at selection-screen output.
*  SSCRFIELDS-FUNCTXT_01 = 'Weitere Optionen'(036).
*  if opt = ' ' and and1 = ' ' and and2 = ' ' and
*                   and3 = ' ' and and4 = ' '.
*    loop at screen.
*      if screen-group1 = 'AND'.
*        screen-active = 0.
*        modify screen.
*      endif.
*    endloop.
*  endif.
  CLEAR: suser, sclient.
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
                  ID     'S_ADMI_FCD'
                  FIELD  'SPAD'.
  IF sy-subrc = 0.
    sclient = suser = 'X'.
    REFRESH: client, uname.
  ELSE.
    AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
                    ID     'S_ADMI_FCD'
                    FIELD  'SPAR'.
    IF sy-subrc = 0.
      suser = 'X'.
      REFRESH uname.
    ENDIF.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'CLI' AND sclient IS INITIAL OR
       screen-group1 = 'USR' AND suser IS INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN OUTPUT.
*  if opt = ' ' and and1 = ' ' and and2 = ' ' and
*                   and3 = ' ' and and4 = ' '.
*    loop at screen.
*      if screen-group1 = 'AND'.
*        screen-active = 0.
*        modify screen.
*      endif.
*    endloop.
*  endif.
  CLEAR: suser, sclient.
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
                  ID     'S_ADMI_FCD'
                  FIELD  'SPAD'.
  IF sy-subrc = 0.
    sclient = suser = 'X'.
*    refresh: client, uname.
  ELSE.
    AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
                    ID     'S_ADMI_FCD'
                    FIELD  'SPAR'.
    IF sy-subrc = 0.
      suser = 'X'.
*      refresh uname.
    ENDIF.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'CLI' AND sclient IS INITIAL OR
       screen-group1 = 'USR' AND suser IS INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*AT SELECTION-SCREEN.
*  IF SSCRFIELDS-UCOMM = 'FC01'.
*    opt = 'X'.
*    loop at screen.
*      if screen-group1 = 'AND'.
**        screen-input = 1.
*        screen-active = 1.
*        modify screen.
*      endif.
*    endloop.
*  endif.

START-OF-SELECTION.
  IF ( and1 = 'X' AND NOT ( old1 = 'X' AND age1 = 'X' ) ) OR
     ( and2 = 'X' AND NOT ( old2 = 'X' AND age2 = 'X' ) ) OR
     ( and3 = 'X' AND NOT ( old3 = 'X' AND age3 = 'X' ) ) OR
     ( and4 = 'X' AND NOT ( old4 = 'X' AND age4 = 'X' ) ).
    WRITE: text-124.
    EXIT.
  ENDIF.
  CLEAR tno_total.

* note 999638
  IF system-low IS INITIAL.
    system-sign = 'I'.
    system-option = 'EQ'.
    system-low = sy-sysid.
    APPEND system.
  ENDIF.

  CALL FUNCTION 'RSPO_RESOLVE_SYSTEM_NAMES'
    TABLES
      syssel  = system
      syslist = syslist.
  REFRESH tsp01_result.
  PERFORM get_timestamp CHANGING utc_now.
  PERFORM map_to_local_time USING utc_now CHANGING ldate ltime.
  IF working = 'X'.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
*       CORRECT_OPTION               = '+'
        date                         = ldate   "local date
        factory_calendar_id          = pcalid
      IMPORTING
*       DATE                         =
        factorydate                  = facdate
*       WORKINGDAY_INDICATOR         =
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        correct_option_invalid       = 2
        date_after_range             = 3
        date_before_range            = 4
        date_invalid                 = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.
    cal = sy-subrc.
  ENDIF.

  PERFORM write_select.
  IF cal <> 0.
    WRITE: 'Fabrikkalender kann nicht gelesen werden (Code ='(029)
           NO-GAP, cal NO-GAP, ')'.
    NEW-LINE.
    WRITE: 'Use all days for further calculations.'(133).
    CLEAR working.
  ENDIF.
  LOOP AT syslist.
    CALL FUNCTION 'RSPO_ISELECT_SPOOLREQS'
      EXPORTING
        rfcsystem    = syslist-sysid
        access       = 'DELE'
        nonex_dev    = nonex
      IMPORTING
        rq_no_access = tno
*       RQ_ACCESS    =
      TABLES
*       S_RQIDEN     =
        s_rqclie     = client
        s_rq0nam     = suffix0
        s_rq1nam     = suffix1
        s_rq2nam     = suffix2
        s_rqowne     = uname
*       S_RQ1DIS     =
*       S_RQ2DIS     =
*       S_RQFIN      =
*       S_RQCOPI     =
*       S_RQPRIO     =
*       S_RQPAPE     =
*       S_RQPJRE     =
        s_rqtitl     = title
*       S_RQRECE     =
*       S_RQDIVI     =
*       S_RQAUTH     =
*       S_RQTLAN     =
*       S_RQTNUM     =
*       S_RQCRED     =
*       S_RQDELD     =
        s_rqdestl    = printer
*       S_RQPAGE     =
*       S_RQSAPT     =
*       S_RQUNXT     =
*       S_RQADES     =
*       S_RQSYST     =
        result_tsp01 = tsp01_list
      EXCEPTIONS
        OTHERS       = 1.
    text_message text.
    ADD tno TO tno_total.
  ENDLOOP.

  WRITE sy-uline.
  IF tno_total > 0.                                       "#EC PORTABLE
    FORMAT COLOR COL_NEGATIVE.
    text = text-025.
    REPLACE '&1' WITH tno_total INTO text.
    WRITE text.
    FORMAT COLOR COL_NORMAL.
  ENDIF.
  "IF age1 = 'X' OR old1 = 'X'.
    PERFORM delete1.
  "ENDIF.
*  IF age2 = 'X' OR old2 = 'X'.
*    PERFORM delete2.
*  ENDIF.
*  IF age3 = 'X' OR old3 = 'X'.
*    PERFORM delete3.
*  ENDIF.
*  IF age4 = 'X' OR old4 = 'X'.
*    PERFORM delete4.
*  ENDIF.
  PERFORM delete_proto.
  IF listonly IS INITIAL.
    PERFORM delete_selected.
    CALL FUNCTION 'RSPO_CLEAN_STATISTIC'
*      EXPORTING
*           GENERAL_AGE     = 7
*           PROCESSED_AGE   = 1
*      IMPORTING
*           UNFINISHED_JOBS =
*           PROCESSED_JOBS  =
*           OLD_QUERY       =
*           PROCESSED_QUERY =
            .

*   delete TSPEVDEV entries
    DATA: itsploms LIKE tsploms OCCURS 10 WITH HEADER LINE.

    SELECT * FROM tsploms INTO TABLE itsploms.
    LOOP AT itsploms.
      CALL FUNCTION 'RSPO_DELETE_LOMS_EVENTS'
        EXPORTING
          loms         = itsploms-name
          days         = 7
        EXCEPTIONS
          unknown_loms = 1
          OTHERS       = 2         ##FM_SUBRC_OK.
    ENDLOOP.

  ELSE.
    IF sy-batch IS INITIAL.
      LOOP AT tsp01_result.
        idlist-id = tsp01_result-rqident.
        idlist-sysid = tsp01_result-sys.
        APPEND idlist.
      ENDLOOP.
      CALL FUNCTION 'RSPO_RID_SPOOLREQ_LIST'
        EXPORTING
          id_list = idlist[]
*         SUMMARY = ' '
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc <> 0.
        out_message 'I'.
      ENDIF.
    ENDIF.
  ENDIF.

* Delete TSPFGUID entries
  CL_APC_WS_FRONTEND_PRINT=>CLEANUP_TSPFGUID( ).

*&---------------------------------------------------------------------*
*&      Form  delete_proto
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_proto.
  DATA: match, counter TYPE i.
  counter = 0.
  text = 'Löschen verwaister Protokolle:'(031).
  NEW-PAGE.
  none = 'X'.
  REFRESH tsp01_list.
  LOOP AT syslist.
    CALL FUNCTION 'RSPO_ISELECT_SPOOLREQS'
       EXPORTING
            rfcsystem    = syslist-sysid
            access       = 'DELE'
            proto        = 'X'
*      IMPORTING
*           RQ_NO_ACCESS =
*           RQ_ACCESS    =
       TABLES
            result_tsp01 = tsp01_list
       EXCEPTIONS
            OTHERS       = 1.
    text_message text.
  ENDLOOP.
  LOOP AT tsp01_list.
    REFRESH tsp02_list.
    CALL FUNCTION 'RSPO_ISELECT_TSP02_ID'
      EXPORTING
        rfcsystem  = tsp01_list-sys
        pjident    = tsp01_list-rqident
        is_prot_no = 'X'
      TABLES
        tsp02_list = tsp02_list
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      text_message text.
    ELSE.
      CLEAR match.
      LOOP AT tsp02_list.
        IF tsp02_list-pjerrprot = tsp01_list-rqident.
          match = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF match IS INITIAL.
*       delete protocol
        CLEAR none.
        WRITE: /1(3) tsp01_list-sys, 8(10) tsp01_list-rqident,
                          25(12) tsp01_list-rqowner.
        CALL FUNCTION 'RSPO_IDELETE_SPOOLREQ'
             EXPORTING
                  spoolreq = tsp01_list
*            IMPORTING
*                 RC       =
*                 STATUS   =
             EXCEPTIONS
                  OTHERS   = 1.
        text_message text.
        ADD 1 TO counter.
        IF counter = commit_a.
          counter = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  PERFORM write_none.
ENDFORM.                    "delete_proto


*&---------------------------------------------------------------------*
*&      Form  DELETE_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_selected.
  DATA: counter TYPE i.
  DATA: del_status TYPE sy-subrc.
  DATA: del_rc TYPE rspotype-rc.
  DATA: try_again.
  DATA: rq_tabix TYPE sy-tabix.
  DATA: rq_index TYPE sy-index.

  CHECK lines( tsp01_result ) > 0.

  DO 2 TIMES.
    rq_index = sy-index.
    LOOP AT tsp01_result.
      IF sy-batch IS NOT INITIAL.
        CHECK tsp01_result-rqident NE sy-spono.
      ENDIF.
      CLEAR try_again.
      rq_tabix = sy-tabix.
      CALL FUNCTION 'RSPO_IDELETE_SPOOLREQ'
           EXPORTING
                spoolreq = tsp01_result
                allow_commit = abap_true
*        IMPORTING
*             RC       =
*             STATUS   =
           EXCEPTIONS
                OTHERS    = 1.
      IF sy-subrc <> 0.
        del_status = sy-msgv3.
        del_rc = sy-msgv4.
        IF ( del_status = 20 AND del_rc = 256 AND rq_index = 1 ).
          try_again = 'X'.
        ELSE.
          text_message text.
          text = text-003.
          REPLACE '&1' WITH tsp01_result-sys INTO text.
          num = tsp01_result-rqident.
          REPLACE '&2' WITH num INTO text.
          WRITE: / text.
        ENDIF.
      ENDIF.
      IF try_again IS INITIAL.
        DELETE tsp01_result INDEX rq_tabix.
        ADD 1 TO counter.
        IF counter = commit_a.
          counter = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDDO.

ENDFORM.                    "DELETE_SELECTED

*&---------------------------------------------------------------------*
*&      Form  CALC_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->MIN_ALT    text
*      -->TNOW       text
*----------------------------------------------------------------------*
FORM calc_time USING min_alt CHANGING tnow LIKE tsp01-rqmodtime.
  DATA: alt_sec TYPE i, lfacdate LIKE facdate.
  IF working = 'X'.
    lfacdate = facdate - min_alt.
    CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
      EXPORTING
        factorydate                  = lfacdate
        factory_calendar_id          = pcalid
      IMPORTING
        date                         = ldate   "local
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        factorydate_after_range      = 2
        factorydate_before_range     = 3
        factorydate_invalid          = 4
        factory_calendar_id_missing  = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.
    IF sy-subrc <> 0.
      WRITE: 'Datum kann nicht umgesetzt werden (Code = '(028)
             NO-GAP, sy-subrc NO-GAP, ')'.
      NEW-LINE.
*   ENDIF.
*   TEXT_MESSAGE TEXT.
*   TNOW = LDATE.
*   TNOW+8(6) = LTIME.
*   TNOW+14(2) = '00'.
*   PERFORM MAP_TO_GLOBAL_TIME CHANGING TNOW.
      CLEAR tnow.
    ELSE.
      text_message text.
      tnow = ldate.
      tnow+8(6) = ltime.
      tnow+14(2) = '00'.
      PERFORM map_to_global_time CHANGING tnow.
    ENDIF.
  ELSE.
    tnow = utc_now.
    alt_sec = - ( min_alt * 86400 ).
    PERFORM add_to_timestamp USING alt_sec CHANGING tnow.
  ENDIF.
ENDFORM.                    "CALC_TIME


*&---------------------------------------------------------------------*
*&      Form  DELETE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete1.
  DATA: lutc_now LIKE utc_now.
*  IF age1 = 'X'.
*    PERFORM calc_time USING min_alt1 CHANGING lutc_now.
*    IF and1 = ' '.
*      text = text-002.
*    ELSE.
*      text = text-032.
*    ENDIF.
*    REPLACE '&1' WITH min_alt1 INTO text.
*    NEW-PAGE.
*    none = 'X'.
    LOOP AT tsp01_list.
    " IF tsp01_list-rqpjreq = 0.
     "   IF tsp01_list-rqcretime < lutc_now.
      "    IF and1 = ' ' OR ( tsp01_list-rqdeltime < utc_now ).
            PERFORM update.
      "    ENDIF.
      "  ENDIF.
     " ENDIF.
    ENDLOOP.
    PERFORM write_none.
 " ENDIF.
*  IF old1 = 'X' AND and1 = ' '.
*    text = text-001.
*    NEW-PAGE.
*    none = 'X'.
*    LOOP AT tsp01_list.
*      IF tsp01_list-rqpjreq = 0.
*        IF tsp01_list-rqdeltime < utc_now.
*          PERFORM update.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    PERFORM write_none.
*  ENDIF.
ENDFORM.                                                    "DELETE1


*&---------------------------------------------------------------------*
*&      Form  DELETE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete2.
  DATA: lutc_now LIKE tsp01-rqdeltime.
  IF age2 = 'X'.
    PERFORM calc_time USING min_alt2 CHANGING lutc_now.
    IF and2 = ' '.
      text = text-005.
    ELSE.
      text = text-034.
    ENDIF.
    REPLACE '&1' WITH min_alt2 INTO text.
    NEW-PAGE.
    none = 'X'.
    LOOP AT tsp01_list.
      IF tsp01_list-rqpjdone < tsp01_list-rqpjreq.
        IF tsp01_list-rqcretime < lutc_now.
          IF and2 = ' ' OR ( tsp01_list-rqdeltime < utc_now ).
            PERFORM update.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    PERFORM write_none.
  ENDIF.
  IF old2 = 'X' AND and2 = ' '.
    text = text-006.
    NEW-PAGE.
    none = 'X'.
    LOOP AT tsp01_list.
      IF tsp01_list-rqpjdone < tsp01_list-rqpjreq.
        IF tsp01_list-rqdeltime < utc_now.
          PERFORM update.
        ENDIF.
      ENDIF.
    ENDLOOP.
    PERFORM write_none.
  ENDIF.
ENDFORM.                                                    "DELETE2

*&---------------------------------------------------------------------*
*&      Form  DELETE3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete3.
  DATA: lutc_now LIKE tsp01-rqdeltime, err_sum TYPE i.
  IF age3 = 'X'.
    PERFORM calc_time USING min_alt3 CHANGING lutc_now.
    IF and3 = ' '.
      text = text-004.
    ELSE.
      text = text-033.
    ENDIF.
    REPLACE '&1' WITH min_alt3 INTO text.
    NEW-PAGE.
    none = 'X'.
    LOOP AT tsp01_list.
      err_sum = tsp01_list-rqpjherr + tsp01_list-rqpjserr.
      IF tsp01_list-rqpjdone > err_sum AND
         tsp01_list-rqpjdone >= tsp01_list-rqpjreq AND
         ( tsp01_list-rqarchstat = '0' OR tsp01_list-rqarchstat = '3' ).
        IF tsp01_list-rqcretime < lutc_now.
          IF and3 = ' ' OR ( tsp01_list-rqdeltime < utc_now ).
            PERFORM update.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    PERFORM write_none.
  ENDIF.
  IF old3 = 'X' AND and3 = ' '.
    text = text-007.
    NEW-PAGE.
    none = 'X'.
    LOOP AT tsp01_list.
      err_sum = tsp01_list-rqpjherr + tsp01_list-rqpjserr.
      IF tsp01_list-rqpjdone > err_sum AND
         tsp01_list-rqpjdone >= tsp01_list-rqpjreq AND
         ( tsp01_list-rqarchstat = '0' OR tsp01_list-rqarchstat = '3' ).
        IF tsp01_list-rqdeltime < utc_now.
          PERFORM update.
        ENDIF.
      ENDIF.
    ENDLOOP.
    PERFORM write_none.
  ENDIF.
ENDFORM.                                                    "DELETE3


*&---------------------------------------------------------------------*
*&      Form  DELETE4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete4.
  DATA: lutc_now LIKE tsp01-rqdeltime, err_sum TYPE i.
  IF age4 = 'X'.
    PERFORM calc_time USING min_alt4 CHANGING lutc_now.
    IF and4 = ' '.
      text = text-008.
    ELSE.
      text = text-035.
    ENDIF.
    REPLACE '&1' WITH min_alt4 INTO text.
    NEW-PAGE.
    none = 'X'.
    LOOP AT tsp01_list.
      err_sum = tsp01_list-rqpjserr + tsp01_list-rqpjherr.
      IF tsp01_list-rqpjreq > 0 AND err_sum > 0 AND
         tsp01_list-rqpjdone >= tsp01_list-rqpjreq AND
         tsp01_list-rqpjdone >= err_sum.
        IF tsp01_list-rqcretime < lutc_now.
          IF and4 = ' ' OR ( tsp01_list-rqdeltime < utc_now ).
            PERFORM update.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    PERFORM write_none.
  ENDIF.
  IF old4 = 'X' AND and4 = ' '.
    text = text-009.
    NEW-PAGE.
    none = 'X'.
    LOOP AT tsp01_list.
      err_sum = tsp01_list-rqpjserr + tsp01_list-rqpjherr.
      IF tsp01_list-rqpjreq > 0 AND err_sum > 0 AND
         tsp01_list-rqpjdone >= tsp01_list-rqpjreq AND
         tsp01_list-rqpjdone >= err_sum.
        IF tsp01_list-rqdeltime < utc_now.
          PERFORM update.
        ENDIF.
      ENDIF.
    ENDLOOP.
    PERFORM write_none.
  ENDIF.
ENDFORM.                                                    "DELETE4

*&---------------------------------------------------------------------*
*&      Form  WRITE_NONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_none.
  IF none = 'X'.
*   WRITE: / 'Kein passender Spoolauftrag gefunden'(010).
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    WRITE: / sy-vline,
      'Kein passender Spoolauftrag gefunden'(010),
           80 sy-vline.
    FORMAT COLOR OFF.
  ENDIF.
  ULINE.
ENDFORM.                    "WRITE_NONE

*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update.
  DATA: ltime LIKE sy-uzeit,
        ldate LIKE sy-datum,
        l(19).
  APPEND tsp01_list TO tsp01_result.
  DELETE tsp01_list.
*  WRITE: /1(3) TSP01_LIST-SYS, 8(10) TSP01_LIST-RQIDENT,
*                           25(12) TSP01_LIST-RQOWNER.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: / sy-vline,
           2(6) tsp01_list-sys,
           9  sy-vline,
           10(13) tsp01_list-rqident,
           24 sy-vline,
           25(12) tsp01_list-rqowner,
           38 sy-vline.
  PERFORM map_to_local_time USING tsp01_list-rqcretime
                                  CHANGING ldate ltime.
*  WRITE: 40 LDATE, LTIME.
  WRITE ldate TO l.
  WRITE ltime TO l+12.
  WRITE: 39 l,
         59 sy-vline.
  PERFORM map_to_local_time USING tsp01_list-rqdeltime
                                  CHANGING ldate ltime.
*  WRITE: 61 LDATE, LTIME.
  WRITE ldate TO l.
  WRITE ltime TO l+12.
  WRITE: 60 l,
         80 sy-vline.
  CLEAR none.
ENDFORM.                    "UPDATE

*&---------------------------------------------------------------------*
*&      Form  WRITE_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_select.
  FORMAT COLOR COL_HEADING.
  WRITE: 'Systemzeit:'(011), ldate, ltime.
  WRITE sy-uline.
  WRITE: 'Folgende Spool-Aufträge sollen gelöscht werden:'(012).
  FORMAT COLOR COL_NORMAL.
  IF and1 = 'X'.
    text = text-032.
    REPLACE '&1' WITH min_alt1 INTO text.
    WRITE text.
  ELSE.
    IF age1 = 'X'.
      text = text-002.
      REPLACE '&1' WITH min_alt1 INTO text.
      WRITE text.
    ENDIF.
    IF old1 = 'X'.
      WRITE text-001.
    ENDIF.
  ENDIF.
  IF and2 = 'X'.
    text = text-034.
    REPLACE '&1' WITH min_alt2 INTO text.
    WRITE text.
  ELSE.
    IF age2 = 'X'.
      text = text-005.
      REPLACE '&1' WITH min_alt2 INTO text.
      WRITE text.
    ENDIF.
    IF old2 = 'X'.
      WRITE text-006.
    ENDIF.
  ENDIF.
  IF and3 = 'X'.
    text = text-033.
    REPLACE '&1' WITH min_alt3 INTO text.
    WRITE text.
  ELSE.
    IF age3 = 'X'.
      text = text-004.
      REPLACE '&1' WITH min_alt3 INTO text.
      WRITE text.
    ENDIF.
    IF old3 = 'X'.
      WRITE text-007.
    ENDIF.
  ENDIF.
  IF and4 = 'X'.
    text = text-035.
    REPLACE '&1' WITH min_alt4 INTO text.
    WRITE text.
  ELSE.
    IF age4 = 'X'.
      text = text-008.
      REPLACE '&1' WITH min_alt4 INTO text.
      WRITE text.
    ENDIF.
    IF old4 = 'X'.
      WRITE text-009.
    ENDIF.
  ENDIF.
  IF days = 'X'.
    WRITE: 'Interpretation der Tageszahlen: Alle Tage zählen'(026).
  ELSE.
    text = text-027.
    REPLACE '&1' WITH pcalid INTO text.
    WRITE text.
  ENDIF.
  NEW-LINE.
  WRITE sy-uline.
  FORMAT COLOR COL_HEADING.
  WRITE: 'Folgende weitere Bedingungen wurden gewählt:'(013).
  FORMAT COLOR COL_NORMAL.
  WRITE: / 'Systemnamen:'(016).
  NEW-LINE.
  LOOP AT syslist.
    WRITE: syslist.
  ENDLOOP.
  WRITE: / 'Mandant:'(017).
  PERFORM print_option TABLES client.
  WRITE: / 'Erzeuger:'(014).
  PERFORM print_option TABLES uname.
  WRITE: / 'Titel:'(018).
  PERFORM print_option TABLES title.
  WRITE: / 'Spool-Auftragsname:'(019).
  PERFORM print_option TABLES suffix0.
  WRITE: / 'Spool-Auftragsname (Suffix1):'(020).
  PERFORM print_option TABLES suffix1.
  WRITE: / 'Spool-Auftragsname (Suffix2):'(021).
  PERFORM print_option TABLES suffix2.
  WRITE: / 'Ausgabegerät:'(030).
  PERFORM print_option TABLES printer.
  IF nonex = 'X'.
    WRITE: /
          'Aufträge mit nicht (mehr) existierendem Ausgabegerät'(126).
  ENDIF.
  WRITE sy-uline.
  IF listonly = 'X'.
    WRITE: 'Nur protokollieren, nicht löschen.'(022).
  ELSE.
    WRITE: 'Protokollieren und löschen'(023).
  ENDIF.
  IF commit_a <= 0.
    commit_a = 1.
  ENDIF.
  text = text-024.
  REPLACE '&1' WITH commit_a INTO text.
  WRITE text.
ENDFORM.                    "WRITE_SELECT


*FORM PRINT_OPTION TABLES SEL_OPT USING LEN TYPE I.
*DATA: BUF LIKE RQTITL_T.
*FIELD-SYMBOLS: <FS>.
*  LOOP AT SEL_OPT.
*    BUF = SEL_OPT.
*    NEW-LINE.
*    IF BUF-SIGN = 'I'.
*      CASE BUF-OPTION.
*        WHEN 'EQ'. WRITE ICON_EQUAL_GREEN AS ICON.
*        WHEN 'NE'. WRITE ICON_NOT_EQUAL_GREEN AS ICON.
*        WHEN 'NB'. WRITE ICON_INTERVAL_EXCLUDE_GREEN AS ICON.
*        WHEN 'BT'. WRITE ICON_INTERVAL_INCLUDE_GREEN AS ICON.
*        WHEN 'GT'. WRITE ICON_GREATER_GREEN AS ICON.
*        WHEN 'LT'. WRITE ICON_LESS_GREEN AS ICON.
*        WHEN 'GE'. WRITE ICON_GREATER_EQUAL_GREEN AS ICON.
*        WHEN 'LE'. WRITE ICON_LESS_EQUAL_GREEN AS ICON.
*        WHEN 'CP'. WRITE ICON_PATTERN_INCLUDE_GREEN AS ICON.
*        WHEN 'NP'. WRITE ICON_PATTERN_EXCLUDE_GREEN AS ICON.
*      ENDCASE.
*    ELSE.
*      CASE BUF-OPTION.
*        WHEN 'EQ'. WRITE ICON_EQUAL_RED AS ICON.
*        WHEN 'NE'. WRITE ICON_NOT_EQUAL_RED AS ICON.
*        WHEN 'NB'. WRITE ICON_INTERVAL_EXCLUDE_RED AS ICON.
*        WHEN 'BT'. WRITE ICON_INTERVAL_INCLUDE_RED AS ICON.
*        WHEN 'GT'. WRITE ICON_GREATER_RED AS ICON.
*        WHEN 'LT'. WRITE ICON_LESS_RED AS ICON.
*        WHEN 'GE'. WRITE ICON_GREATER_EQUAL_RED AS ICON.
*        WHEN 'LE'. WRITE ICON_LESS_EQUAL_RED AS ICON.
*        WHEN 'CP'. WRITE ICON_PATTERN_INCLUDE_RED AS ICON.
*        WHEN 'NP'. WRITE ICON_PATTERN_EXCLUDE_RED AS ICON.
*      ENDCASE.
*    ENDIF.
*    ASSIGN BUF-LOW(LEN) TO <FS>.
*    WRITE <FS>.
*    IF BUF-OPTION = 'NB' OR BUF-OPTION = 'BT'.
*     ASSIGN BUF-LOW+LEN(LEN) TO <FS>.
*     WRITE <FS>.
*    ENDIF.
*  ENDLOOP.
*  IF SY-SUBRC = 4.
*    WRITE: / '*'.
*  ENDIF.
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRINT_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->SEL_OPT    text
*      -->LEN        text
*----------------------------------------------------------------------*
FORM print_option TABLES sel_opt.

  FIELD-SYMBOLS: <f1> TYPE any,
  <fs> TYPE any,
  <fo> TYPE any,
  <fl> TYPE any,
  <fh> TYPE any.


  LOOP AT sel_opt ASSIGNING <f1>.
    NEW-LINE.

    ASSIGN COMPONENT 1 OF STRUCTURE <f1> TO <fs>.
    ASSIGN COMPONENT 2 OF STRUCTURE <f1> TO <fo>.
    ASSIGN COMPONENT 3 OF STRUCTURE <f1> TO <fl>.
    ASSIGN COMPONENT 4 OF STRUCTURE <f1> TO <fh>.

    IF <fs> = 'I'.
      CASE <fo>.
        WHEN 'EQ'. WRITE icon_equal_green AS ICON.
        WHEN 'NE'. WRITE icon_not_equal_green AS ICON.
        WHEN 'NB'. WRITE icon_interval_exclude_green AS ICON.
        WHEN 'BT'. WRITE icon_interval_include_green AS ICON.
        WHEN 'GT'. WRITE icon_greater_green AS ICON.
        WHEN 'LT'. WRITE icon_less_green AS ICON.
        WHEN 'GE'. WRITE icon_greater_equal_green AS ICON.
        WHEN 'LE'. WRITE icon_less_equal_green AS ICON.
        WHEN 'CP'. WRITE icon_pattern_include_green AS ICON.
        WHEN 'NP'. WRITE icon_pattern_exclude_green AS ICON.
      ENDCASE.
    ELSE.
      CASE <fo>.
        WHEN 'EQ'. WRITE icon_equal_red AS ICON.
        WHEN 'NE'. WRITE icon_not_equal_red AS ICON.
        WHEN 'NB'. WRITE icon_interval_exclude_red AS ICON.
        WHEN 'BT'. WRITE icon_interval_include_red AS ICON.
        WHEN 'GT'. WRITE icon_greater_red AS ICON.
        WHEN 'LT'. WRITE icon_less_red AS ICON.
        WHEN 'GE'. WRITE icon_greater_equal_red AS ICON.
        WHEN 'LE'. WRITE icon_less_equal_red AS ICON.
        WHEN 'CP'. WRITE icon_pattern_include_red AS ICON.
        WHEN 'NP'. WRITE icon_pattern_exclude_red AS ICON.
      ENDCASE.
    ENDIF.

    WRITE: <fl>, <fh>.

  ENDLOOP.
  IF sy-subrc = 4.
    WRITE: / '*'.
  ENDIF.

ENDFORM.                    "PRINT_OPTION


TOP-OF-PAGE.
  IF NOT text IS INITIAL.
*    WRITE: TEXT.
*    WRITE: / TEXT-015.
    ULINE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    WRITE: / sy-vline,
           (78) text,
             80 sy-vline.
    ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE: / sy-vline,
             2(6) 'System'(127),
             9 sy-vline,
             10(13) 'Spool-ID'(128),
             24 sy-vline,
             25(12) 'Eigentümer'(129),
             38 sy-vline,
             39(19) 'Erzeugungszeit'(130),
             59 sy-vline,
             60(19) 'Löschzeit'(131),
             80 sy-vline.
    FORMAT COLOR OFF.
    ULINE.
  ENDIF.

END-OF-PAGE.
  IF NOT text IS INITIAL.
    ULINE.
  ENDIF.

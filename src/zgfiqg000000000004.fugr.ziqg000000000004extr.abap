FUNCTION ZIQG000000000004EXTR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      %SELOPT STRUCTURE  RSPARAMS
*"      DTAB STRUCTURE  ZIQG000000000004
*"  CHANGING
*"     VALUE(%RTMODE) TYPE  AQLIMODE
*"  EXCEPTIONS
*"      NO_DATA
*"      NO_AUTHORIZATION
*"      ILLEGAL_PACKAGE
*"      CURSOR_NOT_OPEN
*"----------------------------------------------------------------------

  CALL FUNCTION 'RSAQRT_SET_IDENTIFICATION'
    EXPORTING
      IQID        = %IQID
      SSCR_REPORT = SY-REPID
    CHANGING
      RTMODE      = %RTMODE.

  IF %RTMODE-PACK_ON = SPACE OR %RTMODE-FIRST_CALL = 'X'.
    CALL FUNCTION 'RSAQRT_FILL_SELECTIONS'
      TABLES
        SELOPT = %SELOPT
      CHANGING
        RTMODE = %RTMODE.
  ENDIF.

  CALL FUNCTION 'RSAQRT_INIT_TEXTHANDLING'
    EXPORTING
      CLASS   = 'CL_TEXT_IDENTIFIER'
      WSID    = 'G'
      INFOSET = '/SAPQUERY/AM27'.

  IF %RTMODE-NO_AUTHCHK = SPACE
     AND ( %RTMODE-PACK_ON = SPACE OR %RTMODE-FIRST_CALL = 'X' ).
    REFRESH %AUTH_TABS.
    APPEND 'ANLP' TO %AUTH_TABS.
    APPEND 'T093B' TO %AUTH_TABS.
    CALL FUNCTION 'RSAQRT_AUTHORITY_CHECK'
      EXPORTING
        AUTH_TABS        = %AUTH_TABS
        AUTH_CLAS        = 'CL_QUERY_TAB_ACCESS_AUTHORITY'
      CHANGING
        RTMODE           = %RTMODE
      EXCEPTIONS
        NO_AUTHORIZATION = 1.
    IF SY-SUBRC = 1.
      RAISE NO_AUTHORIZATION.
    ENDIF.
  ENDIF.

  DATA: %L_NO_FURTHER_FETCH TYPE FLAG, " stop fetching
        %L_HITS_CNT         TYPE I.    " cnt for %dbtab entries

  IF %RTMODE-PACK_ABORT = 'X'.
    IF NOT %DBCURSOR IS INITIAL.
      CLOSE CURSOR %DBCURSOR.
    ENDIF.
    EXIT.
  ENDIF.


  IF %RTMODE-PACK_ON = SPACE OR %RTMODE-FIRST_CALL = 'X'.
    IF NOT %DBCURSOR IS INITIAL.
      CLOSE CURSOR %DBCURSOR.
    ENDIF.

    IF RECAP IS NOT INITIAL OR DEPRE IS NOT INITIAL  OR DEPREAC  IS NOT INITIAL. .

      IF RECAP IS NOT INITIAL AND DEPRE IS INITIAL AND DEPREAC IS INITIAL.

        OPEN CURSOR WITH HOLD %DBCURSOR FOR
        SELECT A~BUKRS A~ANLN1 A~ANLN2 A~PERAF A~NAFAZ A~AAFAZ A~SAFAZ A~ZINSZ A~MAFAZ A~KOSTL A~AFABER A~GJAHR
               A~KTOGR
               FROM ANLP AS A
               WHERE A~BUKRS IN BUKRS
                 AND A~ANLN1 IN ANLAGE
                 AND A~ANLN2 IN UNTNR
                 AND A~KOSTL IN SO_KOSTL
                 AND GSBER   IN GSBER
                 AND A~AFABER IN AFABE
                 AND A~GJAHR IN GJAHR
                 AND A~PERAF IN AFBPLE
                 AND EXISTS ( SELECT *
                               FROM ANLA
                              WHERE BUKRS EQ A~BUKRS
                                AND ANLN1 EQ A~ANLN1
                                AND ANLN2 EQ A~ANLN2
                                AND ORD43 EQ 'R' )
               ORDER BY A~BUKRS
                        A~ANLN1
                        A~ANLN2
                        A~PERAF.

      ELSEIF RECAP IS INITIAL AND DEPRE IS NOT INITIAL  AND DEPREAC IS INITIAL.

        OPEN CURSOR WITH HOLD %DBCURSOR FOR
        SELECT A~BUKRS A~ANLN1 A~ANLN2 A~PERAF A~NAFAZ A~AAFAZ A~SAFAZ A~ZINSZ A~MAFAZ A~KOSTL A~AFABER A~GJAHR
               A~KTOGR
               FROM ANLP AS A
               WHERE A~BUKRS IN BUKRS
                 AND A~ANLN1 IN ANLAGE
                 AND A~ANLN2 IN UNTNR
                 AND A~KOSTL IN SO_KOSTL
                 AND GSBER   IN GSBER
                 AND A~AFABER IN AFABE
                 AND A~GJAHR IN GJAHR
                 AND A~PERAF IN AFBPLE
                 AND EXISTS ( SELECT *
                               FROM ANLA
                              WHERE BUKRS EQ A~BUKRS
                                AND ANLN1 EQ A~ANLN1
                                AND ANLN2 EQ A~ANLN2
                                AND ORD42 EQ 'D' )
               ORDER BY A~BUKRS
                        A~ANLN1
                        A~ANLN2
                        A~PERAF.

      ELSEIF RECAP IS  INITIAL AND DEPRE IS  INITIAL AND DEPREAC IS NOT INITIAL .

        OPEN CURSOR WITH HOLD %DBCURSOR FOR
        SELECT A~BUKRS A~ANLN1 A~ANLN2 A~PERAF A~NAFAZ A~AAFAZ A~SAFAZ A~ZINSZ A~MAFAZ A~KOSTL A~AFABER A~GJAHR
               A~KTOGR
               FROM ANLP AS A
               WHERE A~BUKRS IN BUKRS
                 AND A~ANLN1 IN ANLAGE
                 AND A~ANLN2 IN UNTNR
                 AND A~KOSTL IN SO_KOSTL
                 AND GSBER   IN GSBER
                 AND A~AFABER IN AFABE
                 AND A~GJAHR IN GJAHR
                 AND A~PERAF IN AFBPLE
                 AND EXISTS ( SELECT *
                               FROM ANLA
                              WHERE BUKRS  EQ A~BUKRS
                                AND ANLN1  EQ A~ANLN1
                                AND ANLN2  EQ A~ANLN2
                                AND GDLGRP EQ 'A' )
               ORDER BY A~BUKRS
                        A~ANLN1
                        A~ANLN2
                        A~PERAF.



      ELSEIF RECAP IS NOT INITIAL AND DEPRE IS NOT INITIAL AND DEPREAC IS NOT INITIAL .

        OPEN CURSOR WITH HOLD %DBCURSOR FOR
        SELECT A~BUKRS A~ANLN1 A~ANLN2 A~PERAF A~NAFAZ A~AAFAZ A~SAFAZ A~ZINSZ A~MAFAZ A~KOSTL A~AFABER A~GJAHR
               A~KTOGR
               FROM ANLP AS A
               WHERE A~BUKRS IN BUKRS
                 AND A~ANLN1 IN ANLAGE
                 AND A~ANLN2 IN UNTNR
                 AND A~KOSTL IN SO_KOSTL
                 AND GSBER   IN GSBER
                 AND A~AFABER IN AFABE
                 AND A~GJAHR IN GJAHR
                 AND A~PERAF IN AFBPLE
                 AND EXISTS ( SELECT *
                               FROM ANLA
                              WHERE BUKRS  EQ A~BUKRS
                                AND ANLN1  EQ A~ANLN1
                                AND ANLN2  EQ A~ANLN2
                                AND ORD42  EQ 'D'
                                AND ORD43  EQ 'R'
                                AND GDLGRP EQ 'A' )
               ORDER BY A~BUKRS
                        A~ANLN1
                        A~ANLN2
                        A~PERAF.
      ENDIF.

    ELSE.

      OPEN CURSOR WITH HOLD %DBCURSOR FOR
      SELECT BUKRS ANLN1 ANLN2 PERAF NAFAZ AAFAZ SAFAZ ZINSZ MAFAZ KOSTL AFABER GJAHR
             KTOGR
             FROM ANLP
             WHERE BUKRS IN BUKRS
               AND ANLN1 IN ANLAGE
               AND ANLN2 IN UNTNR
               AND KOSTL IN SO_KOSTL
               AND GSBER   IN GSBER
               AND AFABER IN AFABE
               AND GJAHR IN GJAHR
               AND PERAF IN AFBPLE
             ORDER BY BUKRS
                      ANLN1
                      ANLN2
                      PERAF.
    ENDIF.

  ENDIF.

  IF %DBCURSOR IS INITIAL.
    RAISE CURSOR_NOT_OPEN.
  ENDIF.

  WHILE %L_NO_FURTHER_FETCH = SPACE.
    FETCH NEXT CURSOR %DBCURSOR
          INTO CORRESPONDING FIELDS OF ANLP.
    IF ( ( %RTMODE-ACC_CHECK = 'X' AND
           SY-DBCNT > %RTMODE-ACC_NUMBER )
        OR SY-SUBRC <> 0 ).
      %L_NO_FURTHER_FETCH = 'X'.
    ELSE.
      CALL FUNCTION 'RSAQRT_TEXTFIELD_REFRESH'.
*** additional coding:
      ON CHANGE OF ANLP-BUKRS.
        GD_T093C-BUKRS = ANLP-BUKRS.
        CALL FUNCTION 'T093C_READ'
          EXPORTING
            F_T093C = GD_T093C
          IMPORTING
            F_T093C = GD_T093C.
      ENDON.
*ON CHANGE OF ANLP-AFABER.
      IF ANLP-BUKRS <> T093B-BUKRS OR                       "> 1103249
         ANLP-AFABER <> GD_T093-AFABER.                     "> 1103249
        GD_T093-AFABER = ANLP-AFABER.
        GD_T093-AFAPL = GD_T093C-AFAPL.
        CALL FUNCTION 'T093_READ'
          EXPORTING
            F_T093 = GD_T093
*           F_DELFLG  =
          IMPORTING
            F_T093 = GD_T093.
        T093B-BUKRS = ANLP-BUKRS.
        IF GD_T093-XSTORE EQ 'X'.
          T093B-AFABE = GD_T093-AFABER.
        ELSE.
          T093B-AFABE = GD_T093-AFABE1.
        ENDIF.
        CALL FUNCTION 'T093B_READ'
          EXPORTING
            F_T093B = T093B
          IMPORTING
            F_T093B = T093B.
*ENDON.
      ENDIF.                                                "> 1103249
      AUTHORITY-CHECK   OBJECT   'A_S_KOSTL'
                                   ID       'BUKRS'        FIELD ANLP-BUKRS
                                   ID       'KOSTL'        FIELD ANLP-KOSTL.
      IF SY-SUBRC NE 0.
        CHECK 0 = 1.
      ENDIF.

      DTAB-BUKRS = ANLP-BUKRS .
      DTAB-ANLN1 = ANLP-ANLN1 .
      DTAB-ANLN2 = ANLP-ANLN2 .
      DTAB-PERAF = ANLP-PERAF .
      DTAB-NAFAZ = ANLP-NAFAZ .
      DTAB-WAERS = T093B-WAERS .
      DTAB-AAFAZ = ANLP-AAFAZ .
      DTAB-WAERS001 = T093B-WAERS .
      DTAB-SAFAZ = ANLP-SAFAZ .
      DTAB-WAERS002 = T093B-WAERS .
      DTAB-ZINSZ = ANLP-ZINSZ .
      DTAB-WAERS003 = T093B-WAERS .
      DTAB-MAFAZ = ANLP-MAFAZ .
      DTAB-WAERS004 = T093B-WAERS .
      DTAB-KTOGR    = ANLP-KTOGR.

      SELECT SINGLE KTANSW
        FROM T095
        INTO DTAB-KTANSW
         WHERE KTOGR EQ DTAB-KTOGR
           AND AFABE EQ '01'.

      DATA: IT_ANLZ TYPE TABLE OF ANLZ,
            WA_ANLZ TYPE ANLZ.

      SELECT *
        FROM ANLZ
       INTO TABLE IT_ANLZ
       WHERE BUKRS  EQ DTAB-BUKRS
        AND  ANLN1  EQ DTAB-ANLN1
        AND  ANLN2  EQ DTAB-ANLN2.

      SORT IT_ANLZ BY BDATU DESCENDING.

      READ TABLE IT_ANLZ INTO WA_ANLZ WITH KEY BUKRS = DTAB-BUKRS
                                               ANLN1 = DTAB-ANLN1
                                               ANLN2 = DTAB-ANLN2.
      IF SY-SUBRC = 0.
        DTAB-KOSTL = WA_ANLZ-KOSTL.
        DTAB-GSBER = WA_ANLZ-GSBER.
      ENDIF.

      SELECT SINGLE TXT50 TXA50 AKTIV ZUGDT ORD43 ORD42 ORD41 ORD44 GDLGRP
        FROM ANLA
        INTO (DTAB-TXT50, DTAB-TXA50, DTAB-AKTIV, DTAB-ZUGDT, DTAB-ORD43, DTAB-ORD42, DTAB-ORD41, DTAB-ORD44, DTAB-GDLGRP)
        WHERE BUKRS EQ DTAB-BUKRS
        AND   ANLN1 EQ DTAB-ANLN1
        AND   ANLN2 EQ DTAB-ANLN2.

      APPEND DTAB.
      %L_HITS_CNT = %L_HITS_CNT + 1.
      IF %RTMODE-PACK_ON = 'X'
         AND %L_HITS_CNT >= %RTMODE-PACK_SIZE.
        %L_NO_FURTHER_FETCH = 'X'.
      ENDIF.
    ENDIF.
  ENDWHILE.

  IF %L_HITS_CNT = 0.
    IF NOT %DBCURSOR IS INITIAL.
      CLOSE CURSOR %DBCURSOR.
    ENDIF.
    RAISE NO_DATA.
  ENDIF.

  IF %RTMODE-PACK_ON <> 'X'.
    CLOSE CURSOR %DBCURSOR.
  ENDIF.
  READ TABLE DTAB INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    RAISE NO_DATA.
  ENDIF.

ENDFUNCTION.

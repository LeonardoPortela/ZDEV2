*----------------------------------------------------------------------*
***INCLUDE LBSPLF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_TOTALS_CALCULATE
*&---------------------------------------------------------------------*
FORM BSPL_GRID_TOTALS_CALCULATE
                      TABLES PT_NODE     TYPE      TT_RSTHIE
                             PT_DATA     TYPE      TT_BSPL_DATA
                             PT_TOTALS   TYPE      TT_GRID_TOTALS
                       USING IS_SETTINGS STRUCTURE RFBILA_ALV_SETTINGS.
* local data declarations
  DATA: L_TFILL_FROM      LIKE SY-TABIX,
        L_TFILL_TO        LIKE SY-TABIX,
        L_NODE_LEVEL      LIKE RSTHIE-TLEVEL,
        L_HIGH_NODE_LEVEL LIKE RSTHIE-TLEVEL,
        LS_NODE_ERGSL     TYPE TS_NODE_ERGSL.

* notice highest node level
  LOOP AT PT_NODE ASSIGNING <RSTHIE>.
    IF L_HIGH_NODE_LEVEL < <RSTHIE>-TLEVEL.
      L_HIGH_NODE_LEVEL = <RSTHIE>-TLEVEL.
    ENDIF.
  ENDLOOP.

* calculate totals
  L_NODE_LEVEL = L_HIGH_NODE_LEVEL + 1.
  DO L_HIGH_NODE_LEVEL TIMES.
    L_NODE_LEVEL = L_NODE_LEVEL - 1.
    LOOP AT PT_NODE ASSIGNING <RSTHIE>.
      CHECK <RSTHIE>-TYPE   = CON_LEAF.
      CHECK <RSTHIE>-TLEVEL = L_NODE_LEVEL.

      READ TABLE PT_DATA WITH KEY
                              ID = <RSTHIE>-ID
                              BINARY SEARCH.
      IF SY-SUBRC = 0.
        PT_TOTALS-ID       = PT_DATA-ID.
        PT_TOTALS-ERGSL    = PT_DATA-ERGSL.
        PT_TOTALS-TYPE     = <RSTHIE>-TYPE.
        PT_TOTALS-TLEVEL   = <RSTHIE>-TLEVEL.
        PT_TOTALS-CURTP    = PT_DATA-CURTP.
        PT_TOTALS-WAERS    = PT_DATA-WAERS.
        PT_TOTALS-REPVAL   = PT_DATA-REPVAL.
        PT_TOTALS-COMPVAL  = PT_DATA-COMPVAL.
        PT_TOTALS-WAER2    = PT_DATA-WAER2.
        PT_TOTALS-REPVAL2  = PT_DATA-REPVAL2.
        PT_TOTALS-COMPVAL2 = PT_DATA-COMPVAL2.
        "pt_totals-repvalv  = pt_data-repvalv.
        "pt_totals-repval2v = pt_data-repval2v.
        PT_TOTALS-PER01 = PT_DATA-PER01.
        PT_TOTALS-PER02 = PT_DATA-PER02.
        PT_TOTALS-PER03 = PT_DATA-PER03.
        PT_TOTALS-PER04 = PT_DATA-PER04.
        PT_TOTALS-PER05 = PT_DATA-PER05.
        PT_TOTALS-PER06 = PT_DATA-PER06.
        PT_TOTALS-PER07 = PT_DATA-PER07.
        PT_TOTALS-PER08 = PT_DATA-PER08.
        PT_TOTALS-PER09 = PT_DATA-PER09.
        PT_TOTALS-PER10 = PT_DATA-PER10.
        PT_TOTALS-PER11 = PT_DATA-PER11.
        PT_TOTALS-PER12 = PT_DATA-PER12.
        PT_TOTALS-PER13 = PT_DATA-PER13.
        PT_TOTALS-PER14 = PT_DATA-PER14.
        PT_TOTALS-PER15 = PT_DATA-PER15.
        PT_TOTALS-PER16 = PT_DATA-PER16.

        APPEND PT_TOTALS.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE PT_TOTALS LINES SY-TFILL.
    IF SY-TFILL > L_TFILL_TO.
      L_TFILL_FROM = L_TFILL_TO + 1.
      L_TFILL_TO   = SY-TFILL.
    ELSE.
      CONTINUE.               " >>>>  Next DO  >>>>
    ENDIF.
    LOOP AT PT_TOTALS FROM L_TFILL_FROM
                      TO   L_TFILL_TO.
*     Parent of node ID get
      READ TABLE PT_NODE INDEX PT_TOTALS-ID.
      READ TABLE PT_NODE INDEX PT_NODE-PARENT.
      IF PT_NODE-TYPE = CON_BPOS
      OR PT_NODE-TYPE = CON_TOP.
        LS_NODE_ERGSL = PT_NODE-NAME.
        PT_TOTALS-ERGSL = LS_NODE_ERGSL-ERGSL.
      ENDIF.
      PT_TOTALS-ID     = PT_NODE-ID.
      PT_TOTALS-TYPE   = PT_NODE-TYPE.
      PT_TOTALS-TLEVEL = PT_NODE-TLEVEL.
      COLLECT PT_TOTALS.
    ENDLOOP.
  ENDDO.
  SORT PT_TOTALS.

  IF IS_SETTINGS-ZEROACCT = CON_X.
*.. accounts with zero balance required
    L_NODE_LEVEL = L_HIGH_NODE_LEVEL + 1.
    DO L_HIGH_NODE_LEVEL TIMES.
      L_NODE_LEVEL = L_NODE_LEVEL - 1.
      LOOP AT PT_NODE ASSIGNING <RSTHIE>.
        CHECK <RSTHIE>-TYPE   = CON_ACCT
           OR <RSTHIE>-TYPE   = CON_FBER.
        CHECK <RSTHIE>-TLEVEL = L_NODE_LEVEL.

        READ TABLE PT_DATA WITH KEY
                                ID = <RSTHIE>-ID
                                BINARY SEARCH.
        IF SY-SUBRC = 0.
          PT_TOTALS-ID        = PT_DATA-ID.
          PT_TOTALS-ERGSL     = PT_DATA-ERGSL.
          PT_TOTALS-TYPE      = <RSTHIE>-TYPE.
          PT_TOTALS-TLEVEL    = <RSTHIE>-TLEVEL.
          PT_TOTALS-CURTP     = PT_DATA-CURTP.
          PT_TOTALS-WAERS     = PT_DATA-WAERS.
          PT_TOTALS-REPVAL    = PT_DATA-REPVAL.
          PT_TOTALS-COMPVAL   = PT_DATA-COMPVAL.
          PT_TOTALS-WAER2     = PT_DATA-WAER2.
          PT_TOTALS-REPVAL2   = PT_DATA-REPVAL2.
          PT_TOTALS-COMPVAL2  = PT_DATA-COMPVAL2.
          "pt_totals-repvalv   = pt_data-repvalv.
          "pt_totals-repval2v  = pt_data-repval2v.
          PT_TOTALS-PER01 = PT_DATA-PER01.
          PT_TOTALS-PER02 = PT_DATA-PER02.
          PT_TOTALS-PER03 = PT_DATA-PER03.
          PT_TOTALS-PER04 = PT_DATA-PER04.
          PT_TOTALS-PER05 = PT_DATA-PER05.
          PT_TOTALS-PER06 = PT_DATA-PER06.
          PT_TOTALS-PER07 = PT_DATA-PER07.
          PT_TOTALS-PER08 = PT_DATA-PER08.
          PT_TOTALS-PER09 = PT_DATA-PER09.
          PT_TOTALS-PER10 = PT_DATA-PER10.
          PT_TOTALS-PER11 = PT_DATA-PER11.
          PT_TOTALS-PER12 = PT_DATA-PER12.
          PT_TOTALS-PER13 = PT_DATA-PER13.
          PT_TOTALS-PER14 = PT_DATA-PER14.
          PT_TOTALS-PER15 = PT_DATA-PER15.
          PT_TOTALS-PER16 = PT_DATA-PER16.
          APPEND PT_TOTALS.
        ENDIF.
      ENDLOOP.

      DESCRIBE TABLE PT_TOTALS LINES SY-TFILL.
      IF SY-TFILL > L_TFILL_TO.
        L_TFILL_FROM = L_TFILL_TO + 1.
        L_TFILL_TO   = SY-TFILL.
      ELSE.
        CONTINUE.               " >>>>  Next DO  >>>>
      ENDIF.
      LOOP AT PT_TOTALS FROM L_TFILL_FROM
                        TO   L_TFILL_TO.
*.....  Parent of node ID get
        READ TABLE PT_NODE INDEX PT_TOTALS-ID.
        READ TABLE PT_NODE INDEX PT_NODE-PARENT.
        IF PT_NODE-TYPE = CON_BPOS
        OR PT_NODE-TYPE = CON_TOP.
          LS_NODE_ERGSL = PT_NODE-NAME.
          PT_TOTALS-ERGSL = LS_NODE_ERGSL-ERGSL.
        ENDIF.
        PT_TOTALS-ID     = PT_NODE-ID.
        PT_TOTALS-TYPE   = PT_NODE-TYPE.
        PT_TOTALS-TLEVEL = PT_NODE-TLEVEL.
        COLLECT PT_TOTALS.
      ENDLOOP.
    ENDDO.
    SORT PT_TOTALS.
  ENDIF.
ENDFORM.                    " BSPL_GRID_TOTALS_CALCULATE

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_OUTTAB_FILL
*&---------------------------------------------------------------------*
FORM BSPL_GRID_OUTTAB_FILL
                        TABLES PT_NODE      TYPE TT_RSTHIE
                               PT_DATA      TYPE TT_BSPL_DATA
                               PT_TOTALS    TYPE TT_GRID_TOTALS
                               PT_TEXT      TYPE TT_ERGSL_TEXT
                               PT_EDIT      TYPE TT_EDIT_SETTINGS
                               PT_OUTTAB    TYPE TT_GRID_OUTTAB
                   USING VALUE(PS_SETTINGS) LIKE RFBILA_ALV_SETTINGS.

*     local data declaration
  DATA: LT_PREDECESSOR TYPE TT_GRID_PREDECESSOR WITH HEADER LINE.
  DATA: L_ID           LIKE RSTHIE-ID.
  DATA: L_FBERFLG      LIKE CON_X.
  DATA: L_FBER_TLEVEL  LIKE RSTHIE-TLEVEL.
  DATA: LS_NODE_SAKNR  TYPE TS_NODE_SAKNR.
  DATA: LF_NODE_BUKRS  LIKE T001-BUKRS.
  DATA: LF_NODE_FKBER  LIKE TFKB-FKBER.

  LOOP AT PT_NODE ASSIGNING <RSTHIE>.
    CASE <RSTHIE>-TYPE.
      WHEN CON_TOP.
      WHEN CON_BPOS.
        CLEAR: L_FBERFLG.
        PERFORM BSPL_GRID_BPOS_TO_OUTTAB TABLES PT_TOTALS
                                                PT_TEXT
                                                PT_EDIT
                                                PT_OUTTAB
                                                LT_PREDECESSOR
                                         USING  <RSTHIE>
                                                PS_SETTINGS
                                       CHANGING L_ID.
      WHEN CON_ACCT.
        LS_NODE_SAKNR = <RSTHIE>-NAME.
        CHECK: G_LIST_STATE = CON_ACCT.
        CHECK: L_FBERFLG     IS INITIAL
           OR  L_FBER_TLEVEL =  <RSTHIE>-TLEVEL.
        PERFORM BSPL_GRID_ACCT_TO_OUTTAB TABLES PT_TOTALS
                                                PT_OUTTAB
                                                PT_DATA
                                         USING  <RSTHIE>
                                                PS_SETTINGS
                                       CHANGING L_ID.

      WHEN CON_FBER.
        LF_NODE_FKBER = <RSTHIE>-NAME.
        L_FBERFLG = CON_X.
        L_FBER_TLEVEL = <RSTHIE>-TLEVEL.
        CHECK: G_LIST_STATE = CON_ACCT.
        PERFORM BSPL_GRID_FBER_TO_OUTTAB TABLES PT_TOTALS
                                                PT_OUTTAB
                                                PT_DATA
                                         USING  <RSTHIE>
                                                PS_SETTINGS
                                       CHANGING L_ID.
      WHEN CON_CCOD.
        LF_NODE_BUKRS = <RSTHIE>-NAME.
        CHECK: G_LIST_STATE = CON_CCOD.
        PERFORM BSPL_GRID_CCOD_TO_OUTTAB TABLES PT_TOTALS
                                                PT_OUTTAB
                                                PT_DATA
                                         USING  <RSTHIE>
                                                PS_SETTINGS
                                                LS_NODE_SAKNR
                                                LF_NODE_FKBER
                                                L_FBERFLG
                                       CHANGING L_ID.
      WHEN CON_BUSA.
      WHEN CON_LEAF.
        CHECK: G_LIST_STATE = CON_LEAF.
        PERFORM BSPL_GRID_LEAF_TO_OUTTAB TABLES PT_TOTALS
                                                PT_OUTTAB
                                                PT_DATA
                                         USING  <RSTHIE>
                                                PS_SETTINGS
                                                LS_NODE_SAKNR
                                                LF_NODE_BUKRS
                                                LF_NODE_FKBER
                                                L_FBERFLG
                                       CHANGING L_ID.
    ENDCASE.
  ENDLOOP.

  IF NOT ( LT_PREDECESSOR[] IS INITIAL ).
*   put totals of remaining predecessors to OUTTAB
    PERFORM BSPL_GRID_PREDECESSOR_2_OUTTAB
                                    TABLES PT_TOTALS
                                           PT_TEXT
                                           PT_EDIT
                                           PT_OUTTAB
                                           LT_PREDECESSOR
                                    USING  '01'      "<<< TLEVEL
                                           PS_SETTINGS
                                 CHANGING  L_ID.
  ENDIF.
ENDFORM.                    " BSPL_GRID_OUTTAB_FILL

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_ACCT_TO_OUTTAB
*&---------------------------------------------------------------------*
FORM BSPL_GRID_ACCT_TO_OUTTAB
                       TABLES PT_TOTALS    TYPE TT_GRID_TOTALS
                              PT_OUTTAB    TYPE TT_GRID_OUTTAB
                              PT_DATA      TYPE TT_BSPL_DATA
                 USING  VALUE(PS_NODE)     LIKE RSTHIE
                        VALUE(PS_SETTINGS) LIKE RFBILA_ALV_SETTINGS
              CHANGING  VALUE(P_ID)        LIKE RSTHIE-ID.

*     local data declarations
  DATA: LS_NODE_SAKNR       TYPE TS_NODE_SAKNR.

  CLEAR: PT_OUTTAB.
  LS_NODE_SAKNR = PS_NODE-NAME.
  CALL FUNCTION 'READ_HAUPTBUCH_TEXT'
    EXPORTING
      KONTENPLAN     = LS_NODE_SAKNR-KTOPL
      SACHKONTO      = LS_NODE_SAKNR-SAKNR
      SPRACHE        = IS_SETTINGS-FS_LANGUAGE
    IMPORTING
      TEXT_WA        = SKAT
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC = 0.
*.. fix account number to relevant length
    PERFORM BSPL_ACCOUNT_LENGTH_FIX
                           CHANGING LS_NODE_SAKNR.
    CONCATENATE LS_NODE_SAKNR-SAKNR
                SKAT-TXT50
           INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
  ELSE.
*.. fix account number to relevant length
    PERFORM BSPL_ACCOUNT_LENGTH_FIX
                           CHANGING LS_NODE_SAKNR.
    CONCATENATE LS_NODE_SAKNR-KTOPL
                LS_NODE_SAKNR-SAKNR
           INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
  ENDIF.

  P_ID = P_ID + 1.
  PT_OUTTAB-ID      = P_ID.        .
  PT_OUTTAB-SUBID   = 1.
* read totals of account number
  READ TABLE PT_TOTALS WITH KEY ID = PS_NODE-ID
                                     BINARY SEARCH.
  IF SY-SUBRC = 0.
    PT_OUTTAB-ERGSL   = PT_TOTALS-ERGSL.
    PT_OUTTAB-CURTP   = PT_TOTALS-CURTP.

    PT_OUTTAB-WAERS   = PT_TOTALS-WAERS.
    PT_OUTTAB-REPVAL  = PT_TOTALS-REPVAL.
    PT_OUTTAB-COMPVAL = PT_TOTALS-COMPVAL.

    PT_OUTTAB-WAER2    = PT_TOTALS-WAER2.
    PT_OUTTAB-REPVAL2  = PT_TOTALS-REPVAL2.
    PT_OUTTAB-COMPVAL2 = PT_TOTALS-COMPVAL2.

    PT_OUTTAB-PER01 = PT_TOTALS-PER01.
    PT_OUTTAB-PER02 = PT_TOTALS-PER02.
    PT_OUTTAB-PER03 = PT_TOTALS-PER03.
    PT_OUTTAB-PER04 = PT_TOTALS-PER04.
    PT_OUTTAB-PER05 = PT_TOTALS-PER05.
    PT_OUTTAB-PER06 = PT_TOTALS-PER06.
    PT_OUTTAB-PER07 = PT_TOTALS-PER07.
    PT_OUTTAB-PER08 = PT_TOTALS-PER08.
    PT_OUTTAB-PER09 = PT_TOTALS-PER09.
    PT_OUTTAB-PER10 = PT_TOTALS-PER10.
    PT_OUTTAB-PER11 = PT_TOTALS-PER11.
    PT_OUTTAB-PER12 = PT_TOTALS-PER12.
    PT_OUTTAB-PER13 = PT_TOTALS-PER13.
    PT_OUTTAB-PER14 = PT_TOTALS-PER14.
    PT_OUTTAB-PER15 = PT_TOTALS-PER15.
    PT_OUTTAB-PER16 = PT_TOTALS-PER16.

    "pt_outtab-repvalv  = pt_totals-repvalv.
    "pt_outtab-repval2v = pt_totals-repval2v.

    PT_OUTTAB-TLEVEL  = PT_TOTALS-TLEVEL.
*.. calculate variance
    PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                          PT_OUTTAB-REPVAL
                                          PT_OUTTAB-COMPVAL
                                 CHANGING PT_OUTTAB-ABSVAR
                                          PT_OUTTAB-RELVAR.

    PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                          PT_OUTTAB-REPVAL2
                                          PT_OUTTAB-COMPVAL2
                                 CHANGING PT_OUTTAB-ABSVAR2
                                          PT_OUTTAB-RELVAR2.

    PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_OUTTAB-REPVAL
                                          PT_OUTTAB-REPVAL2
                                          PT_OUTTAB-COMPVAL
                                          PT_OUTTAB-COMPVAL2
                                          PT_OUTTAB-ABSVAR
                                          PT_OUTTAB-ABSVAR2
                                 CHANGING PT_OUTTAB-REPVALV
                                          PT_OUTTAB-REPVAL2V
                                          PT_OUTTAB-REPVAL3V.

*.. provide fields like ccode, busarea, funcarea, account no etc.
    LS_NODE_SAKNR = PS_NODE-NAME.
    IF NOT ( PS_SETTINGS-ALTACCT IS INITIAL ).
*.... alternative account number is used
      ASSIGN: PT_DATA-KTOP2 TO <KTOPL>,
              PT_DATA-ALTKT TO <RACCT>.
    ELSEIF NOT ( T011-XERGS IS INITIAL ).
*.... group account number is used
      ASSIGN: PT_DATA-KKTPL TO <KTOPL>,
              PT_DATA-BILKT TO <RACCT>.
    ELSE.
*.... default normal accounts used
      ASSIGN: PT_DATA-KTOPL TO <KTOPL>,
              PT_DATA-RACCT TO <RACCT>.
    ENDIF.

    LOOP AT PT_DATA WHERE ERGSL = PT_TOTALS-ERGSL.
*     CHECK <KTOPL> = LS_NODE_SAKNR-KTOPL. "n1559627
      CHECK <RACCT> = LS_NODE_SAKNR-SAKNR.
      PT_OUTTAB-KTOPL  = PT_DATA-KTOPL.
*     provide operational account no
      IF PT_OUTTAB-RACCT IS INITIAL.
        PT_OUTTAB-RACCT  = PT_DATA-RACCT.
      ELSEIF PT_OUTTAB-RACCT <> PT_DATA-RACCT.
*       1:n assignment between group account no
*                    and operational account no
        PT_OUTTAB-RACCT = CON_STARS.
      ENDIF.
      PT_OUTTAB-KKTPL  = PT_DATA-KKTPL.
      PT_OUTTAB-BILKT  = PT_DATA-BILKT.
      PT_OUTTAB-KTOP2  = PT_DATA-KTOP2.
      PT_OUTTAB-ALTKT  = PT_DATA-ALTKT.
      PT_OUTTAB-RYEAR  = PT_DATA-RYEAR.
      PT_OUTTAB-POPER  = PT_DATA-POPER.
*     provide company code
      IF PT_OUTTAB-RBUKRS IS INITIAL.
        PT_OUTTAB-RBUKRS = PT_DATA-RBUKRS.
      ELSEIF PT_OUTTAB-RBUKRS <> PT_DATA-RBUKRS.
*       more than one ccode, mark it with stars
        PT_OUTTAB-RBUKRS = CON_STARS.
        CLEAR X_BUKRS_UNIQUE.
      ENDIF.
*     provide business area
      IF PT_OUTTAB-RBUSA IS INITIAL
     AND PT_DATA-RBUSA   IS INITIAL.
        PT_OUTTAB-RBUSA  = SY-VLINE.
      ELSEIF PT_OUTTAB-RBUSA IS INITIAL.
        PT_OUTTAB-RBUSA  = PT_DATA-RBUSA.
      ELSEIF PT_OUTTAB-RBUSA <> PT_DATA-RBUSA.
*       more than one busarea, mark it with stars
        PT_OUTTAB-RBUSA = CON_STARS.
        CLEAR X_RBUSA_UNIQUE.
      ENDIF.
*     privide function area
      IF PT_OUTTAB-RFAREA IS INITIAL.
        PT_OUTTAB-RFAREA = PT_DATA-RFAREA.
      ELSEIF PT_OUTTAB-RFAREA <> PT_DATA-RFAREA.
*       more than one function area, mark it with stars
        PT_OUTTAB-RFAREA = CON_STARS.
      ENDIF.
    ENDLOOP.
    IF PT_OUTTAB-RBUSA  = SY-VLINE.
*.... only initial business area
      CLEAR: PT_OUTTAB-RBUSA.
    ENDIF.
    PERFORM BSPL_GRID_CELL_COLOR_SET
                            TABLES PT_OUTTAB-CELL_COLOR.
  ENDIF.

  APPEND PT_OUTTAB.

ENDFORM.                    " BSPL_GRID_ACCT_TO_OUTTAB

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_BPOS_TO_OUTTAB
*&---------------------------------------------------------------------*
FORM BSPL_GRID_BPOS_TO_OUTTAB
                       TABLES PT_TOTALS      TYPE TT_GRID_TOTALS
                              PT_TEXT        TYPE TT_ERGSL_TEXT
                              PT_EDIT        TYPE TT_EDIT_SETTINGS
                              PT_OUTTAB      TYPE TT_GRID_OUTTAB
                              PT_PREDECESSOR TYPE TT_GRID_PREDECESSOR
                   USING  VALUE(PS_NODE)     LIKE RSTHIE
                          VALUE(PS_SETTINGS) LIKE RFBILA_ALV_SETTINGS
                CHANGING  VALUE(P_ID)        LIKE RSTHIE-ID.
*     local data declarations
  DATA: LS_PREDECESSOR_INFO LIKE SNODETEXT,
        LS_NODE_ERGSL       TYPE TS_NODE_ERGSL,
        L_TABIX             LIKE SY-TABIX.

* check if current node (PS_NODE) has a predecessor ?
* ---------------------------------------------------
  CALL FUNCTION 'RS_TREE_GET_PREDECESSOR'
    EXPORTING
      NODE_ID          = PS_NODE-ID
    IMPORTING
      PREDECESSOR_INFO = LS_PREDECESSOR_INFO
    EXCEPTIONS
      ID_NOT_FOUND     = 1
      NO_PREDECESSOR   = 2
      OTHERS           = 3.

  IF SY-SUBRC = 0.
*   save predecessor's info
    LS_NODE_ERGSL         = LS_PREDECESSOR_INFO-NAME.
    PT_PREDECESSOR-TLEVEL = LS_PREDECESSOR_INFO-TLEVEL.
    PT_PREDECESSOR-ID     = LS_PREDECESSOR_INFO-ID.
    PT_PREDECESSOR-ERGSL  = LS_NODE_ERGSL-ERGSL.
    APPEND PT_PREDECESSOR.
*   put totals of predecessors to OUTTAB
    PERFORM BSPL_GRID_PREDECESSOR_2_OUTTAB
                                    TABLES PT_TOTALS
                                           PT_TEXT
                                           PT_EDIT
                                           PT_OUTTAB
                                           PT_PREDECESSOR
                                    USING  PS_NODE-TLEVEL
                                           PS_SETTINGS
                                 CHANGING  P_ID.
  ENDIF.

* check if current node (PS_NODE) has a successor ?
* -------------------------------------------------
  IF PS_NODE-NEXT IS INITIAL.
*   there is no successor
*   save the ID because you don't get it as a predecessor
    LS_NODE_ERGSL = PS_NODE-NAME.
    PT_PREDECESSOR-TLEVEL = PS_NODE-TLEVEL.
    PT_PREDECESSOR-ID     = PS_NODE-ID.
    PT_PREDECESSOR-ERGSL  = LS_NODE_ERGSL-ERGSL.
    APPEND PT_PREDECESSOR.
  ENDIF.

* read group begin text of current node
  LS_NODE_ERGSL = PS_NODE-NAME.
  READ TABLE PT_TEXT WITH KEY ERGSL = LS_NODE_ERGSL-ERGSL
                              TXTYP = CON_A
                              BINARY SEARCH.
  L_TABIX = SY-TABIX + 1.
  IF SY-SUBRC = 0.
*   add first line (group begin text) of actual node
    P_ID = P_ID + 1.
    CLEAR: PT_OUTTAB.
    PT_OUTTAB-ID         = P_ID.        .
    PT_OUTTAB-SUBID      = PT_TEXT-ZEILE.
    PT_OUTTAB-ERGSL      = LS_NODE_ERGSL-ERGSL.
    PT_OUTTAB-TEXT       = PT_TEXT-TXT45.
    PT_OUTTAB-TLEVEL     = PS_NODE-TLEVEL.
    PERFORM BSPL_GRID_CELL_COLOR_SET
                          TABLES PT_OUTTAB-CELL_COLOR.
    APPEND PT_OUTTAB.

*   add the rest lines (group begin text) of actual node
    CLEAR: PT_OUTTAB.
    LOOP AT PT_TEXT FROM L_TABIX
                   WHERE ERGSL = LS_NODE_ERGSL-ERGSL
                     AND TXTYP = CON_A.
      PT_OUTTAB-ID         = P_ID.
      PT_OUTTAB-SUBID      = PT_TEXT-ZEILE.
      PT_OUTTAB-ERGSL      = LS_NODE_ERGSL-ERGSL.
      PT_OUTTAB-TEXT       = PT_TEXT-TXT45.
      PT_OUTTAB-TLEVEL     = PS_NODE-TLEVEL.

      PERFORM BSPL_GRID_CELL_COLOR_SET
                          TABLES PT_OUTTAB-CELL_COLOR.
      APPEND PT_OUTTAB.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BSPL_GRID_BPOS_TO_OUTTAB

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_FBER_TO_OUTTAB
*&---------------------------------------------------------------------*
FORM BSPL_GRID_FBER_TO_OUTTAB
                       TABLES PT_TOTALS    TYPE TT_GRID_TOTALS
                              PT_OUTTAB    TYPE TT_GRID_OUTTAB
                              PT_DATA      TYPE TT_BSPL_DATA
                 USING  VALUE(PS_NODE)     LIKE RSTHIE
                        VALUE(PS_SETTINGS) LIKE RFBILA_ALV_SETTINGS
              CHANGING  VALUE(P_ID)        LIKE RSTHIE-ID.

  CLEAR: PT_OUTTAB.
  SELECT SINGLE FKBTX INTO PT_OUTTAB-TEXT
                      FROM TFKBT WHERE SPRAS = IS_SETTINGS-FS_LANGUAGE
                                   AND FKBER = PS_NODE-NAME.
  IF SY-SUBRC = 0.
    CONCATENATE PS_NODE-NAME
                PT_OUTTAB-TEXT
           INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
  ELSE.
    PT_OUTTAB-TEXT = PS_NODE-NAME.
  ENDIF.

  P_ID = P_ID + 1.
  PT_OUTTAB-ID      = P_ID.        .
  PT_OUTTAB-SUBID   = 1.
* read totals of function area
  READ TABLE PT_TOTALS WITH KEY ID = PS_NODE-ID
                                     BINARY SEARCH.
  PT_OUTTAB-ERGSL   = PT_TOTALS-ERGSL.
  PT_OUTTAB-CURTP   = PT_TOTALS-CURTP.
  PT_OUTTAB-WAERS   = PT_TOTALS-WAERS.
  PT_OUTTAB-REPVAL  = PT_TOTALS-REPVAL.
  PT_OUTTAB-COMPVAL = PT_TOTALS-COMPVAL.

  PT_OUTTAB-WAER2    = PT_TOTALS-WAER2.
  PT_OUTTAB-REPVAL2  = PT_TOTALS-REPVAL2.
  PT_OUTTAB-COMPVAL2 = PT_TOTALS-COMPVAL2.

  PT_OUTTAB-PER01 = PT_TOTALS-PER01.
  PT_OUTTAB-PER02 = PT_TOTALS-PER02.
  PT_OUTTAB-PER03 = PT_TOTALS-PER03.
  PT_OUTTAB-PER04 = PT_TOTALS-PER04.
  PT_OUTTAB-PER05 = PT_TOTALS-PER05.
  PT_OUTTAB-PER06 = PT_TOTALS-PER06.
  PT_OUTTAB-PER07 = PT_TOTALS-PER07.
  PT_OUTTAB-PER08 = PT_TOTALS-PER08.
  PT_OUTTAB-PER09 = PT_TOTALS-PER09.
  PT_OUTTAB-PER10 = PT_TOTALS-PER10.
  PT_OUTTAB-PER11 = PT_TOTALS-PER11.
  PT_OUTTAB-PER12 = PT_TOTALS-PER12.
  PT_OUTTAB-PER13 = PT_TOTALS-PER13.
  PT_OUTTAB-PER14 = PT_TOTALS-PER14.
  PT_OUTTAB-PER15 = PT_TOTALS-PER15.
  PT_OUTTAB-PER16 = PT_TOTALS-PER16.

  "pt_outtab-repvalv  = pt_totals-repvalv.
  "pt_outtab-repval2v = pt_totals-repval2v.

  PT_OUTTAB-TLEVEL  = PT_TOTALS-TLEVEL.
* calculate variance
  PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                        PT_OUTTAB-REPVAL
                                        PT_OUTTAB-COMPVAL
                               CHANGING PT_OUTTAB-ABSVAR
                                        PT_OUTTAB-RELVAR.

  PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                        PT_OUTTAB-REPVAL2
                                        PT_OUTTAB-COMPVAL2
                               CHANGING PT_OUTTAB-ABSVAR2
                                        PT_OUTTAB-RELVAR2.

  PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_OUTTAB-REPVAL
                                        PT_OUTTAB-REPVAL2
                                        PT_OUTTAB-COMPVAL
                                        PT_OUTTAB-COMPVAL2
                                        PT_OUTTAB-ABSVAR
                                        PT_OUTTAB-ABSVAR2
                               CHANGING PT_OUTTAB-REPVALV
                                        PT_OUTTAB-REPVAL2V
                                        PT_OUTTAB-REPVAL3V.

* provide fields like ccode, busarea, funcarea, account no etc.
  LOOP AT PT_DATA WHERE ERGSL  = PT_TOTALS-ERGSL
                    AND RFAREA = PS_NODE-NAME.
    PT_OUTTAB-RFAREA = PT_DATA-RFAREA.
    PT_OUTTAB-RYEAR  = PT_DATA-RYEAR.
    PT_OUTTAB-POPER  = PT_DATA-POPER.
*   provide group gl account
    PT_OUTTAB-KTOPL  = PT_DATA-KTOPL.
    IF PT_OUTTAB-RACCT IS INITIAL.
      PT_OUTTAB-RACCT  = PT_DATA-RACCT.
    ELSEIF PT_OUTTAB-RACCT <> PT_DATA-RACCT.
*     more than one, mark it with stars
      PT_OUTTAB-RACCT  = CON_STARS.
    ENDIF.
*   provide gl account
    PT_OUTTAB-KKTPL  = PT_DATA-KKTPL.
    IF PT_OUTTAB-BILKT IS INITIAL.
      PT_OUTTAB-BILKT  = PT_DATA-BILKT.
    ELSEIF PT_OUTTAB-BILKT <> PT_DATA-BILKT.
*     more than one, mark it with stars
      PT_OUTTAB-BILKT  = CON_STARS.
    ENDIF.
*   provide alternative gl account
    PT_OUTTAB-KTOP2  = PT_DATA-KTOP2.
    IF PT_OUTTAB-ALTKT IS INITIAL.
      PT_OUTTAB-ALTKT  = PT_DATA-ALTKT.
    ELSEIF PT_OUTTAB-ALTKT <> PT_DATA-ALTKT.
*     more than one, mark it with stars
      PT_OUTTAB-ALTKT  = CON_STARS.
    ENDIF.
*   provide company code
    IF PT_OUTTAB-RBUKRS IS INITIAL.
      PT_OUTTAB-RBUKRS = PT_DATA-RBUKRS.
    ELSEIF PT_OUTTAB-RBUKRS <> PT_DATA-RBUKRS.
*     more than one ccode, mark it with stars
      PT_OUTTAB-RBUKRS = CON_STARS.
      CLEAR X_BUKRS_UNIQUE.
    ENDIF.
*   provide business area
    IF PT_OUTTAB-RBUSA IS INITIAL
   AND PT_DATA-RBUSA   IS INITIAL.
      PT_OUTTAB-RBUSA  = SY-VLINE.
    ELSEIF PT_OUTTAB-RBUSA IS INITIAL.
      PT_OUTTAB-RBUSA  = PT_DATA-RBUSA.
    ELSEIF PT_OUTTAB-RBUSA <> PT_DATA-RBUSA.
*     more than one busarea, mark it with stars
      PT_OUTTAB-RBUSA = CON_STARS.
      CLEAR X_RBUSA_UNIQUE.
    ENDIF.
  ENDLOOP.
  IF PT_OUTTAB-RBUSA  = SY-VLINE.
*   only initial business area
    CLEAR: PT_OUTTAB-RBUSA.
  ENDIF.

  PERFORM BSPL_GRID_CELL_COLOR_SET
                          TABLES PT_OUTTAB-CELL_COLOR.
  APPEND PT_OUTTAB.

ENDFORM.                    " BSPL_GRID_FBER_TO_OUTTAB

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_PREDECESSOR_2_OUTTAB
*&---------------------------------------------------------------------*
FORM BSPL_GRID_PREDECESSOR_2_OUTTAB
                       TABLES PT_TOTALS       TYPE TT_GRID_TOTALS
                              PT_TEXT         TYPE TT_ERGSL_TEXT
                              PT_EDIT         TYPE TT_EDIT_SETTINGS
                              PT_OUTTAB       TYPE TT_GRID_OUTTAB
                              PT_PREDECESSOR  TYPE TT_GRID_PREDECESSOR
                    USING  VALUE(P_TLEVEL)    LIKE RSTHIE-TLEVEL
                           VALUE(PS_SETTINGS) LIKE RFBILA_ALV_SETTINGS
                 CHANGING  VALUE(P_ID)        LIKE RSTHIE-ID.

*     local data declarations
  DATA: L_TABIX LIKE SY-TABIX.

  SORT PT_PREDECESSOR BY TLEVEL DESCENDING.
  LOOP AT PT_PREDECESSOR WHERE TLEVEL GE P_TLEVEL.
*   read edit infos of predecessor
    READ TABLE PT_EDIT WITH KEY ERGSL = PT_PREDECESSOR-ERGSL
                                        BINARY SEARCH.
*   check, if totals output is required ??
    IF PT_EDIT-TOTAL = CON_X.
*     read totals of predecessor
      READ TABLE PT_TOTALS WITH KEY ID = PT_PREDECESSOR-ID
                                         BINARY SEARCH.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
*     read group end text of predecessor
      CLEAR: PT_OUTTAB.
      READ TABLE PT_TEXT WITH KEY ERGSL = PT_PREDECESSOR-ERGSL
                                  TXTYP = CON_E
                                  BINARY SEARCH.
      L_TABIX = SY-TABIX + 1.

      IF SY-SUBRC <> 0.
*       READ TABLE PT_TEXT WITH KEY ERGSL = PT_PREDECESSOR-ERGSL
*                                   TXTYP = CON_K
*                                   BINARY SEARCH.
*       CONCATENATE TEXT-004 PT_TEXT-TXT45
*              INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
      ELSE.
        PT_OUTTAB-TEXT = PT_TEXT-TXT45.
      ENDIF.
*     add first line (group end text) and totals
*     of predecessor to PT_OUTTAB
      P_ID = P_ID + 1.
      PT_OUTTAB-ID         = P_ID.        .
      PT_OUTTAB-SUBID      = PT_TEXT-ZEILE.
      PT_OUTTAB-ERGSL      = PT_PREDECESSOR-ERGSL.
      PT_OUTTAB-CURTP      = PT_TOTALS-CURTP.
      PT_OUTTAB-WAERS      = PT_TOTALS-WAERS.
      PT_OUTTAB-REPVAL     = PT_TOTALS-REPVAL.
      PT_OUTTAB-COMPVAL    = PT_TOTALS-COMPVAL.

      PT_OUTTAB-WAER2       = PT_TOTALS-WAER2.
      PT_OUTTAB-REPVAL2     = PT_TOTALS-REPVAL2.
      PT_OUTTAB-COMPVAL2    = PT_TOTALS-COMPVAL2.

      PT_OUTTAB-PER01 = PT_TOTALS-PER01.
      PT_OUTTAB-PER02 = PT_TOTALS-PER02.
      PT_OUTTAB-PER03 = PT_TOTALS-PER03.
      PT_OUTTAB-PER04 = PT_TOTALS-PER04.
      PT_OUTTAB-PER05 = PT_TOTALS-PER05.
      PT_OUTTAB-PER06 = PT_TOTALS-PER06.
      PT_OUTTAB-PER07 = PT_TOTALS-PER07.
      PT_OUTTAB-PER08 = PT_TOTALS-PER08.
      PT_OUTTAB-PER09 = PT_TOTALS-PER09.
      PT_OUTTAB-PER10 = PT_TOTALS-PER10.
      PT_OUTTAB-PER11 = PT_TOTALS-PER11.
      PT_OUTTAB-PER12 = PT_TOTALS-PER12.
      PT_OUTTAB-PER13 = PT_TOTALS-PER13.
      PT_OUTTAB-PER14 = PT_TOTALS-PER14.
      PT_OUTTAB-PER15 = PT_TOTALS-PER15.
      PT_OUTTAB-PER16 = PT_TOTALS-PER16.

      PT_OUTTAB-TLEVEL      = PT_PREDECESSOR-TLEVEL.
*     calculate variance
      PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                            PT_OUTTAB-REPVAL
                                            PT_OUTTAB-COMPVAL
                                   CHANGING PT_OUTTAB-ABSVAR
                                            PT_OUTTAB-RELVAR.

      PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                            PT_OUTTAB-REPVAL2
                                            PT_OUTTAB-COMPVAL2
                                   CHANGING PT_OUTTAB-ABSVAR2
                                            PT_OUTTAB-RELVAR2.
      IF PT_OUTTAB-TLEVEL IS INITIAL.
        PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_OUTTAB-REPVAL
                                              PT_OUTTAB-REPVAL2
                                              PT_OUTTAB-COMPVAL
                                              PT_OUTTAB-COMPVAL2
                                              PT_OUTTAB-ABSVAR
                                              PT_OUTTAB-ABSVAR2
                                     CHANGING PT_OUTTAB-REPVALV
                                              PT_OUTTAB-REPVAL2V
                                              PT_OUTTAB-REPVAL3V.
      ENDIF.

      PERFORM BSPL_GRID_LINE_COLOR_SET CHANGING PT_OUTTAB-LINE_COLOR.
      PT_OUTTAB-TOTALS_LEVEL = PT_OUTTAB-TLEVEL - 1.        "n1486648
      PERFORM BSPL_GRID_CELL_COLOR_SET
                            TABLES PT_OUTTAB-CELL_COLOR.
      APPEND PT_OUTTAB.

*     add the rest lines  (group end text)
*     of predecessor to PT_OUTTAB
      CLEAR: PT_OUTTAB.
      LOOP AT PT_TEXT FROM L_TABIX
                     WHERE ERGSL = PT_PREDECESSOR-ERGSL
                       AND TXTYP = CON_E.
        PT_OUTTAB-ID         = P_ID.
        PT_OUTTAB-SUBID      = PT_TEXT-ZEILE.
        PT_OUTTAB-ERGSL      = PT_PREDECESSOR-ERGSL.
        PT_OUTTAB-TEXT       = PT_TEXT-TXT45.
        PT_OUTTAB-TLEVEL     = PT_PREDECESSOR-TLEVEL.
*       PERFORM BSPL_GRID_LINE_COLOR_SET
*                           CHANGING PT_OUTTAB-LINE_COLOR.
        PERFORM BSPL_GRID_CELL_COLOR_SET
                              TABLES PT_OUTTAB-CELL_COLOR.

        IF PT_OUTTAB-TEXT IS INITIAL.
          CLEAR: PT_OUTTAB-REPVALV, PT_OUTTAB-REPVAL2V.
        ENDIF.

        APPEND PT_OUTTAB.
      ENDLOOP.
    ENDIF.

*   check, if graduated totals output is required ??
    IF PT_EDIT-GRADT = CON_X.
      PERFORM BSPL_GRID_GRADUATED_T_2_OUTTAB
                                      TABLES PT_TOTALS
                                             PT_TEXT
                                             PT_OUTTAB
                                      USING  PT_PREDECESSOR
                                             PT_EDIT
                                             P_TLEVEL
                                             PS_SETTINGS
                                   CHANGING  P_ID.
    ENDIF.
  ENDLOOP.

  DELETE PT_PREDECESSOR
         WHERE TLEVEL GE P_TLEVEL.
ENDFORM.                    " BSPL_GRID_PREDECESSOR_2_OUTTAB

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_ALV_INTERFACE_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_ALV_INTERFACE_SET
              TABLES PT_FIELDCAT         TYPE      SLIS_T_FIELDCAT_ALV
               USING VALUE(PS_SETTINGS)  STRUCTURE RFBILA_ALV_SETTINGS
                     VALUE(MOEDA_01)     TYPE      WAERS
                     VALUE(MOEDA_02)     TYPE      WAERS
            CHANGING VALUE(P_CB_PROGRAM) LIKE      SY-REPID
                     VALUE(PS_VARIANT)   STRUCTURE DISVARIANT
                     VALUE(PS_LAYOUT)    TYPE      SLIS_LAYOUT_ALV.

  DATA: PCOL_POS LIKE SY-CUCOL.

* call back settings
  P_CB_PROGRAM       = 'SAPLZBSPL'.
* variant settings
  PS_VARIANT-REPORT   = PS_SETTINGS-REPID.
  PS_VARIANT-HANDLE   = CON_GRID.
  PS_VARIANT-USERNAME = SY-UNAME.
  PS_VARIANT-VARIANT  = PS_SETTINGS-GRID_VARI.
* layout settings
  PS_LAYOUT-INFO_FIELDNAME   = 'LINE_COLOR'.
  PS_LAYOUT-COLTAB_FIELDNAME = 'CELL_COLOR'.
* fieldcat settings
  PCOL_POS = 15.
  LOOP AT PT_FIELDCAT ASSIGNING <FIELDCAT>.

    CASE <FIELDCAT>-FIELDNAME.
      WHEN 'ERGSL'.
        <FIELDCAT>-COL_POS = 1.
      WHEN 'TEXT'.
        <FIELDCAT>-COL_POS = 2.
      WHEN 'RACCT'.
        <FIELDCAT>-COL_POS = 3.
      WHEN 'WAERS'.
        <FIELDCAT>-SELTEXT_L = TEXT-007."'Moeda Per'.
        <FIELDCAT>-SELTEXT_M = TEXT-007."'Moeda Per'.
        <FIELDCAT>-SELTEXT_S = TEXT-007."'Moeda Per'.
        <FIELDCAT>-COL_POS = 4.
      WHEN 'WAER2'.
        <FIELDCAT>-SELTEXT_L = TEXT-008."'Moeda Comp'.
        <FIELDCAT>-SELTEXT_M = TEXT-008."'Moeda Comp'.
        <FIELDCAT>-SELTEXT_S = TEXT-008."'Moeda Comp'.
        <FIELDCAT>-COL_POS = 5.
      WHEN 'REPVAL'.
        CONCATENATE '(' MOEDA_01 ')' INTO <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-009 <FIELDCAT>-SELTEXT_L INTO <FIELDCAT>-SELTEXT_L SEPARATED BY SPACE. "'Total período relatório'
        <FIELDCAT>-REPTEXT_DDIC = <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-010 MOEDA_01  INTO <FIELDCAT>-SELTEXT_S SEPARATED BY SPACE. "'PerRel'
        CONCATENATE TEXT-011 MOEDA_01  INTO <FIELDCAT>-SELTEXT_M SEPARATED BY SPACE. "'Per.Rel.'
        <FIELDCAT>-COL_POS   = 6.
      WHEN 'REPVAL2'.

        CONCATENATE '(' MOEDA_02 ')' INTO <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-009 <FIELDCAT>-SELTEXT_L INTO <FIELDCAT>-SELTEXT_L SEPARATED BY SPACE. "'Total período relatório'
        <FIELDCAT>-REPTEXT_DDIC = <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-010 MOEDA_02  INTO <FIELDCAT>-SELTEXT_S SEPARATED BY SPACE. "'PerRel'
        CONCATENATE TEXT-011 MOEDA_02  INTO <FIELDCAT>-SELTEXT_M SEPARATED BY SPACE. "'Per.Rel.'

        <FIELDCAT>-COL_POS = 7.
      WHEN 'REPVALV'.
        <FIELDCAT>-SELTEXT_L = TEXT-012."'Taxa Período Relatório'.
        <FIELDCAT>-SELTEXT_S = TEXT-013."'TxRel'.
        <FIELDCAT>-SELTEXT_M = TEXT-014."'Tx.Rel.'.
        <FIELDCAT>-COL_POS = 8.
      WHEN 'COMPVAL'.

        CONCATENATE '(' MOEDA_01 ')' INTO <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-015 <FIELDCAT>-SELTEXT_L INTO <FIELDCAT>-SELTEXT_L SEPARATED BY SPACE. "'Total período comparação'
        <FIELDCAT>-REPTEXT_DDIC = <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-016 MOEDA_01  INTO <FIELDCAT>-SELTEXT_S. "'PerComp'
        CONCATENATE TEXT-017 MOEDA_01  INTO <FIELDCAT>-SELTEXT_M SEPARATED BY SPACE. "'Per.Comp.'

        <FIELDCAT>-COL_POS = 9.
      WHEN 'COMPVAL2'.

        CONCATENATE '(' MOEDA_02 ')' INTO <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-015 <FIELDCAT>-SELTEXT_L INTO <FIELDCAT>-SELTEXT_L SEPARATED BY SPACE. "'Total período comparação'
        <FIELDCAT>-REPTEXT_DDIC = <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-016 MOEDA_02  INTO <FIELDCAT>-SELTEXT_S. "'PerComp'
        CONCATENATE TEXT-017 MOEDA_02  INTO <FIELDCAT>-SELTEXT_M SEPARATED BY SPACE. "'Per.Comp.'

        <FIELDCAT>-COL_POS = 10.
      WHEN 'REPVAL2V'.
        <FIELDCAT>-SELTEXT_L = TEXT-018."'Taxa Período Comparação'.
        <FIELDCAT>-SELTEXT_S = TEXT-019."'TxComp'.
        <FIELDCAT>-SELTEXT_M = TEXT-020."'Tx.Comp.'.
        <FIELDCAT>-COL_POS = 11.
      WHEN 'ABSVAR'.

        CONCATENATE '(' MOEDA_01 ')' INTO <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-021 <FIELDCAT>-SELTEXT_L INTO <FIELDCAT>-SELTEXT_L SEPARATED BY SPACE. "'Desvio Absoluto'
        <FIELDCAT>-REPTEXT_DDIC = <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-022 MOEDA_01  INTO <FIELDCAT>-SELTEXT_S SEPARATED BY SPACE. "'Desvio'
        CONCATENATE TEXT-022 MOEDA_01  INTO <FIELDCAT>-SELTEXT_M SEPARATED BY SPACE. "'Desvio'
        <FIELDCAT>-COL_POS = 12.
      WHEN 'ABSVAR2'.

        CONCATENATE '(' MOEDA_02 ')' INTO <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-021 <FIELDCAT>-SELTEXT_L INTO <FIELDCAT>-SELTEXT_L SEPARATED BY SPACE. "'Desvio Absoluto'
        <FIELDCAT>-REPTEXT_DDIC = <FIELDCAT>-SELTEXT_L.
        CONCATENATE TEXT-022 MOEDA_02  INTO <FIELDCAT>-SELTEXT_S SEPARATED BY SPACE. "'Desvio'
        CONCATENATE TEXT-022 MOEDA_02  INTO <FIELDCAT>-SELTEXT_M SEPARATED BY SPACE. "'Desvio'
        <FIELDCAT>-COL_POS = 13.
      WHEN 'REPVAL3V'.
        <FIELDCAT>-COL_POS = 14.
        <FIELDCAT>-SELTEXT_L = TEXT-024.
        <FIELDCAT>-SELTEXT_M = TEXT-023.
        <FIELDCAT>-SELTEXT_S = TEXT-023.
      WHEN OTHERS.
        <FIELDCAT>-COL_POS = PCOL_POS.
        ADD 1 TO PCOL_POS.
    ENDCASE.

    CASE <FIELDCAT>-FIELDNAME.
      WHEN 'ID'
        OR 'SUBID'
        OR 'NPAGE'
        OR 'TLEVEL'
        OR 'LINE_COLOR'.
        <FIELDCAT>-NO_OUT = CON_X.

      WHEN 'TEXT'.
        PERFORM BSPL_GRID_COLUMN_COLOR_SET CHANGING <FIELDCAT>-EMPHASIZE.
      WHEN 'RELVAR' OR 'RELVAR2'.
        <FIELDCAT>-JUST   = 'R'.
        <FIELDCAT>-NO_OUT = CON_X.
      WHEN 'REPVALV' OR 'REPVAL2V' OR 'REPVAL3V'.
        <FIELDCAT>-JUST   = 'R'.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " BSPL_GRID_ALV_INTERFACE_SET

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_CELL_COLOR_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_CELL_COLOR_SET
                   TABLES PT_CELL_COLOR TYPE SHP_SLIS_T_SPECIALCOL_ALV.
*     local data declarations
  DATA: LS_CELL_COLOR TYPE SHP_SLIS_SPECIALCOL_ALV.

  LS_CELL_COLOR-FIELDNAME = 'TEXT'.
  LS_CELL_COLOR-COLOR-COL =  4.
  LS_CELL_COLOR-COLOR-INT =  0.
  APPEND LS_CELL_COLOR TO PT_CELL_COLOR.

ENDFORM.                    " BSPL_GRID_CELL_COLOR_SET

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_COLUMN_COLOR_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_COLUMN_COLOR_SET
                   CHANGING VALUE(P_EMPHASIZE).
* momentan nich aktiv, da bei Spalteneinfärbung
* die Zelleneinfärbung nicht funktioniert hat
* P_EMPHASIZE  = 'C410'.

ENDFORM.                    " BSPL_GRID_COLUMN_COLOR_SET

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_LINE_COLOR_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_LINE_COLOR_SET
                 CHANGING VALUE(P_OUTTAB_LINE_COLOR).

  P_OUTTAB_LINE_COLOR = 'C30'.

ENDFORM.                    " BSPL_GRID_LINE_COLOR_SET

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_TOP
*&---------------------------------------------------------------------*
FORM BSPL_GRID_TOP.
* local adata declaration
  STATICS: LT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER.
  DATA:    LS_LIST_COMMENTARY TYPE SLIS_LISTHEADER.
  DATA:    LF_XNEWGL   TYPE C,
           LF_TABLINES TYPE I,
           LF_START    TYPE I   VALUE 2.

  IF LT_LIST_COMMENTARY[] IS INITIAL.
*.. check if list commentary is filled by caller
    IF NOT IT_LIST_COMMENTARY[] IS INITIAL.
*.... yes, take it over
      LT_LIST_COMMENTARY[] = IT_LIST_COMMENTARY[].
    ELSE.
*.... no, then set default list commentary
      LS_LIST_COMMENTARY-TYP  = 'H'.
      LS_LIST_COMMENTARY-INFO = T011T-VSTXT.
      APPEND LS_LIST_COMMENTARY
          TO LT_LIST_COMMENTARY.
    ENDIF.
  ENDIF.

* fill standard page header
  BHDGD-INIFL = CON_0.
  BHDGD-LINES = SY-LINSZ.
  BHDGD-UNAME = SY-UNAME.
  BHDGD-REPID = IS_SETTINGS-REPID.
  READ TABLE LT_LIST_COMMENTARY INDEX 1
                                 INTO LS_LIST_COMMENTARY.
  BHDGD-LINE1 = LS_LIST_COMMENTARY-INFO.
  BHDGD-LINE2 = IS_SETTINGS-ALLGLINE.
  BHDGD-BUKRS = IS_SETTINGS-BUKRS.
  BHDGD-DOMAI = 'BUKRS'.
  BHDGD-START_PAGNO = IS_SETTINGS-PAGE_NO.

* write standard page header
  PERFORM BATCH-HEADING1(RSBTCHH0) USING BHDGD.

* If form routine is called from RFBILA00 and NewGL is active:
* Do not write the ledger in the header of the list (ledger is
* on index 2 in table LT_LIST_COMMENTARY in this case). The ledger
* is already displayed by the BATCH-HEADING routine from log. DB.
  IF IS_SETTINGS-REPID = 'ZRFBILA00'.
    CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
      EXPORTING
        ID_BUKRS        = IS_SETTINGS-BUKRS
      IMPORTING
        E_GLFLEX_ACTIVE = LF_XNEWGL.
    IF NOT LF_XNEWGL IS INITIAL.
      LF_START = 2.                                         "n1489749
    ENDIF.
  ENDIF.
  SKIP 1.
  DESCRIBE TABLE LT_LIST_COMMENTARY LINES LF_TABLINES.
  IF LF_START > LF_TABLINES. LF_START = LF_TABLINES. ENDIF.
  LOOP AT LT_LIST_COMMENTARY FROM LF_START TO LF_TABLINES
                             INTO LS_LIST_COMMENTARY.
    CHECK LS_LIST_COMMENTARY-INFO <> IS_SETTINGS-ALLGLINE.  "n1489749
    WRITE: / LS_LIST_COMMENTARY-KEY,
             LS_LIST_COMMENTARY-INFO.
  ENDLOOP.

ENDFORM.                    " BSPL_GRID_TOP
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_TOP_HTML
*&---------------------------------------------------------------------*
FORM BSPL_GRID_TOP_HTML USING P_DYNDOC_ID
                              TYPE REF TO CL_DD_DOCUMENT.
* local adata declaration
  STATICS: LT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER.
  DATA:    LS_LIST_COMMENTARY TYPE SLIS_LISTHEADER.

  IF LT_LIST_COMMENTARY[] IS INITIAL.
*.. check if list commentary is filled by caller
    IF NOT IT_LIST_COMMENTARY[] IS INITIAL.
*.... yes, take it over
      LT_LIST_COMMENTARY[] = IT_LIST_COMMENTARY[].
    ELSE.
*.... no, then set default list commentary
      LS_LIST_COMMENTARY-TYP  = 'H'.
      LS_LIST_COMMENTARY-INFO = T011T-VSTXT.
      APPEND LS_LIST_COMMENTARY
          TO LT_LIST_COMMENTARY.
    ENDIF.
  ENDIF.

  EXPORT IT_LIST_COMMENTARY
    FROM LT_LIST_COMMENTARY TO MEMORY ID 'DYNDOS_FOR_ALV'.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      DOCUMENT = P_DYNDOC_ID
      BOTTOM   = SPACE.

ENDFORM.                    " BSPL_GRID_TOP_HTML

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_GRADUATED_T_2_OUTTAB
*&---------------------------------------------------------------------*
FORM BSPL_GRID_GRADUATED_T_2_OUTTAB
                       TABLES PT_TOTALS       TYPE TT_GRID_TOTALS
                              PT_TEXT         TYPE TT_ERGSL_TEXT
                              PT_OUTTAB       TYPE TT_GRID_OUTTAB
                  USING VALUE(PS_PREDECESSOR) TYPE TS_GRID_PREDECESSOR
                        VALUE(PS_EDIT)        TYPE TS_EDIT_SETTINGS
                        VALUE(P_TLEVEL)       LIKE RSTHIE-TLEVEL
                        VALUE(PS_SETTINGS)    LIKE RFBILA_ALV_SETTINGS
               CHANGING VALUE(P_ID)           LIKE RSTHIE-ID.

* local data declarations
  DATA: L_TABIX LIKE SY-TABIX.

  CLEAR: PT_OUTTAB.
* calculate graduated totals
  PERFORM BSPL_GRID_GRADUATED_T_CALC TABLES PT_TOTALS
                                      USING PS_PREDECESSOR
                                            P_TLEVEL
                                   CHANGING PT_OUTTAB-REPVAL
                                            PT_OUTTAB-COMPVAL.

* read text of graduated totals
  READ TABLE PT_TEXT WITH KEY ERGSL = PS_PREDECESSOR-ERGSL
                              TXTYP = CON_S
                              BINARY SEARCH.
  IF SY-SUBRC = 0.
    PT_OUTTAB-TEXT = PT_TEXT-TXT45.
  ENDIF.
  L_TABIX = SY-TABIX + 1.

* add first line (text) and totals
* of graduated totals to PT_OUTTAB
  P_ID = P_ID + 1.
  PT_OUTTAB-ID         = P_ID.        .
  PT_OUTTAB-SUBID      = PT_TEXT-ZEILE.
  PT_OUTTAB-ERGSL      = PS_PREDECESSOR-ERGSL.
  PT_OUTTAB-CURTP      = PT_TOTALS-CURTP.
  PT_OUTTAB-WAERS      = PT_TOTALS-WAERS.
  PT_OUTTAB-WAER2      = PT_TOTALS-WAER2.
  PT_OUTTAB-TLEVEL     = PS_PREDECESSOR-TLEVEL.             "n1486648
* PT_OUTTAB-REPVAL     = PT_TOTALS-REPVAL.
* PT_OUTTAB-COMPVAL    = PT_TOTALS-COMPVAL.
* calculate variance
  PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                        PT_OUTTAB-REPVAL
                                        PT_OUTTAB-COMPVAL
                               CHANGING PT_OUTTAB-ABSVAR
                                        PT_OUTTAB-RELVAR.

  PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                        PT_OUTTAB-REPVAL2
                                        PT_OUTTAB-COMPVAL2
                               CHANGING PT_OUTTAB-ABSVAR2
                                        PT_OUTTAB-RELVAR2.

  PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_OUTTAB-REPVAL
                                        PT_OUTTAB-REPVAL2
                                        PT_OUTTAB-COMPVAL
                                        PT_OUTTAB-COMPVAL2
                                        PT_OUTTAB-ABSVAR
                                        PT_OUTTAB-ABSVAR2
                               CHANGING PT_OUTTAB-REPVALV
                                        PT_OUTTAB-REPVAL2V
                                        PT_OUTTAB-REPVAL3V.

  PERFORM BSPL_GRID_LINE_COLOR_SET
                      CHANGING PT_OUTTAB-LINE_COLOR.
  PT_OUTTAB-TOTALS_LEVEL = PT_OUTTAB-TLEVEL - 1.            "n1486648
  PERFORM BSPL_GRID_CELL_COLOR_SET
                        TABLES PT_OUTTAB-CELL_COLOR.
  APPEND PT_OUTTAB.

* add the rest lines  (text)
* of graduated totals to PT_OUTTAB
  CLEAR: PT_OUTTAB.
  LOOP AT PT_TEXT FROM L_TABIX
                 WHERE ERGSL = PS_PREDECESSOR-ERGSL
                   AND TXTYP = CON_S.
    PT_OUTTAB-ID         = P_ID.
    PT_OUTTAB-SUBID      = PT_TEXT-ZEILE.
    PT_OUTTAB-ERGSL      = PS_PREDECESSOR-ERGSL.
    PT_OUTTAB-TEXT       = PT_TEXT-TXT45.
    PT_OUTTAB-TLEVEL     = P_TLEVEL.
*   PERFORM BSPL_GRID_LINE_COLOR_SET
*                       CHANGING PT_OUTTAB-LINE_COLOR.
    IF PT_OUTTAB-TEXT IS INITIAL.
      CLEAR: PT_OUTTAB-REPVALV, PT_OUTTAB-REPVAL2V.
    ENDIF.

    PERFORM BSPL_GRID_CELL_COLOR_SET
                          TABLES PT_OUTTAB-CELL_COLOR.
    APPEND PT_OUTTAB.
  ENDLOOP.

ENDFORM.                    " BSPL_GRID_GRADUATED_T_2_OUTTAB


*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_GRADUATED_T_CALC
*&---------------------------------------------------------------------*
FORM BSPL_GRID_GRADUATED_T_CALC
                       TABLES PT_TOTALS       TYPE TT_GRID_TOTALS
                  USING VALUE(PS_PREDECESSOR) TYPE TS_GRID_PREDECESSOR
                        VALUE(P_TLEVEL)       LIKE RSTHIE-TLEVEL
               CHANGING VALUE(P_REPVAL)
                        VALUE(P_COMPVAL).

* local data declarations
  DATA: LS_PREDECESSOR_INFO LIKE SNODETEXT,
        LS_NODE_INFO        LIKE SNODETEXT,
        L_SUBRC             LIKE SY-SUBRC.
  STATICS: LR_BLNCE_SHEET_ID LIKE RANGE OF SNODETEXT-ID.

  IF LR_BLNCE_SHEET_ID[] IS INITIAL.
*   fill balance sheet ID's to LR_BLNCE_SHEET_ID
*   dieser Aufruf muss bei Gelegenheit durch einen FB
*   ersetzt werden ( die Zeit , die Zeit)
    PERFORM GR_BLNCE_SHEET_ID_FILL(SAPLRGRE)
                                   TABLES GT_RSTHIE
                                          LR_BLNCE_SHEET_ID
                                    USING T011-VERSN.
  ENDIF.

* read totals of current predecessor
  READ TABLE PT_TOTALS WITH KEY ID = PS_PREDECESSOR-ID
                                     BINARY SEARCH.
  P_REPVAL  = P_REPVAL  + PT_TOTALS-REPVAL.
  P_COMPVAL = P_COMPVAL + PT_TOTALS-COMPVAL.

* get totals of all other predecessors
  LS_PREDECESSOR_INFO-ID = PS_PREDECESSOR-ID.
  WHILE L_SUBRC = 0.
    CALL FUNCTION 'RS_TREE_GET_PREDECESSOR'
      EXPORTING
        NODE_ID          = LS_PREDECESSOR_INFO-ID
      IMPORTING
        PREDECESSOR_INFO = LS_PREDECESSOR_INFO
      EXCEPTIONS
        ID_NOT_FOUND     = 1
        NO_PREDECESSOR   = 2
        OTHERS           = 3.
    IF SY-SUBRC = 0.
      IF NOT ( LS_PREDECESSOR_INFO-ID IN LR_BLNCE_SHEET_ID )
         OR LR_BLNCE_SHEET_ID IS INITIAL.
*       take only totals of p&l area
*       read totals of predecessor
        READ TABLE PT_TOTALS WITH KEY ID = LS_PREDECESSOR_INFO-ID
                                           BINARY SEARCH.
        P_REPVAL  = P_REPVAL  + PT_TOTALS-REPVAL.
        P_COMPVAL = P_COMPVAL + PT_TOTALS-COMPVAL.
      ELSE.
        L_SUBRC = 4.
      ENDIF.
    ELSE.
*     look, if there is a parent
      CALL FUNCTION 'RS_TREE_GET_NODE'
        EXPORTING
          NODE_ID      = LS_PREDECESSOR_INFO-ID
        IMPORTING
          NODE_INFO    = LS_NODE_INFO
        EXCEPTIONS
          ID_NOT_FOUND = 1
          OTHERS       = 2.

      IF SY-SUBRC = 0.
        LS_PREDECESSOR_INFO-ID = LS_NODE_INFO-PARENT.
      ELSE.
        L_SUBRC = SY-SUBRC.
      ENDIF.
    ENDIF.
  ENDWHILE.

ENDFORM.                    " BSPL_GRID_GRADUATED_T_CALC
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_SORTINFOS_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_SORTINFOS_SET
                      TABLES PT_SORT TYPE SLIS_T_SORTINFO_ALV.
* local data declaration
  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.

  CLEAR: LS_SORT.
  LS_SORT-SPOS       = CON_01.
  LS_SORT-FIELDNAME  = 'NPAGE'.
  LS_SORT-TABNAME    = 'GT_GRIDOUTTAB'.
  LS_SORT-UP         = CON_X.
  LS_SORT-OBLIGATORY = CON_X.
* set new page
  LS_SORT-GROUP      = '*'.
  APPEND LS_SORT TO PT_SORT.

  CLEAR: LS_SORT.
  LS_SORT-SPOS       = CON_02.
  LS_SORT-FIELDNAME  = 'ID'.
  LS_SORT-TABNAME    = 'GT_GRIDOUTTAB'.
  LS_SORT-UP         = CON_X.
  LS_SORT-OBLIGATORY = CON_X.
  APPEND LS_SORT TO PT_SORT.

  CLEAR: LS_SORT.
  LS_SORT-SPOS       = CON_03.
  LS_SORT-FIELDNAME  = 'SUBID'.
  LS_SORT-TABNAME    = 'GT_GRIDOUTTAB'.
  LS_SORT-UP         = CON_X.
  LS_SORT-OBLIGATORY = CON_X.
  APPEND LS_SORT TO PT_SORT.

ENDFORM.                    " BSPL_GRID_SORTINFOS_SET
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_NEWPAGE_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_NEWPAGE_SET TABLES PT_OUTTAB TYPE TT_GRID_OUTTAB.
* local data declarations
  DATA: L_ERGSL LIKE RF011P-ERGSL.
  DATA: L_NPAGE TYPE ZBSPL_GRID_FIELDCAT-NPAGE.

  LOOP AT PT_OUTTAB ASSIGNING <OUTTAB>.
*.. skip to a new page on every level 02
    IF <OUTTAB>-TLEVEL =  CON_02
   AND <OUTTAB>-ERGSL <> L_ERGSL.
      L_ERGSL = <OUTTAB>-ERGSL.
      L_NPAGE = L_NPAGE + 1.
    ENDIF.
    <OUTTAB>-NPAGE = L_NPAGE.
  ENDLOOP.

ENDFORM.                    " BSPL_GRID_NEWPAGE_SET
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_PRINTINFOS_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_PRINTINFOS_SET USING PS_PRINT TYPE SLIS_PRINT_ALV.
  PS_PRINT-NO_PRINT_LISTINFOS = CON_X.
ENDFORM.                    " BSPL_GRID_PRINTINFOS_SET
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_CCOD_TO_OUTTAB
*&---------------------------------------------------------------------*
FORM BSPL_GRID_CCOD_TO_OUTTAB
                       TABLES PT_TOTALS      TYPE TT_GRID_TOTALS
                              PT_OUTTAB      TYPE TT_GRID_OUTTAB
                              PT_DATA        TYPE TT_BSPL_DATA
                 USING  VALUE(PS_NODE)       LIKE RSTHIE
                        VALUE(PS_SETTINGS)   LIKE RFBILA_ALV_SETTINGS
                        VALUE(PS_NODE_SAKNR) TYPE TS_NODE_SAKNR
                        VALUE(P_NODE_FKBER)  LIKE GT_FKBER_ID-FKBER
                        VALUE(P_FBERFLG)     LIKE CON_X
              CHANGING  VALUE(P_ID)          LIKE RSTHIE-ID.

*     local data declarations
  DATA: LS_NODE_SAKNR       TYPE TS_NODE_SAKNR.
  DATA: L_BUKRS             LIKE T001-BUKRS.

  CLEAR: PT_OUTTAB.
  L_BUKRS = PS_NODE-NAME.

  P_ID = P_ID + 1.
  PT_OUTTAB-ID      = P_ID.        .
  PT_OUTTAB-SUBID   = 1.
* read totals of account number
  READ TABLE PT_TOTALS WITH KEY ID = PS_NODE-ID
                                     BINARY SEARCH.
  IF SY-SUBRC = 0.
    PT_OUTTAB-ERGSL   = PT_TOTALS-ERGSL.
    PT_OUTTAB-CURTP   = PT_TOTALS-CURTP.
    PT_OUTTAB-WAERS   = PT_TOTALS-WAERS.
    PT_OUTTAB-REPVAL  = PT_TOTALS-REPVAL.
    PT_OUTTAB-COMPVAL = PT_TOTALS-COMPVAL.

    PT_OUTTAB-WAER2    = PT_TOTALS-WAER2.
    PT_OUTTAB-REPVAL2  = PT_TOTALS-REPVAL2.
    PT_OUTTAB-COMPVAL2 = PT_TOTALS-COMPVAL2.

    "pt_outtab-repvalv  = pt_totals-repvalv.
    "pt_outtab-repval2v = pt_totals-repval2v.

    PT_OUTTAB-PER01 = PT_TOTALS-PER01.
    PT_OUTTAB-PER02 = PT_TOTALS-PER02.
    PT_OUTTAB-PER03 = PT_TOTALS-PER03.
    PT_OUTTAB-PER04 = PT_TOTALS-PER04.
    PT_OUTTAB-PER05 = PT_TOTALS-PER05.
    PT_OUTTAB-PER06 = PT_TOTALS-PER06.
    PT_OUTTAB-PER07 = PT_TOTALS-PER07.
    PT_OUTTAB-PER08 = PT_TOTALS-PER08.
    PT_OUTTAB-PER09 = PT_TOTALS-PER09.
    PT_OUTTAB-PER10 = PT_TOTALS-PER10.
    PT_OUTTAB-PER11 = PT_TOTALS-PER11.
    PT_OUTTAB-PER12 = PT_TOTALS-PER12.
    PT_OUTTAB-PER13 = PT_TOTALS-PER13.
    PT_OUTTAB-PER14 = PT_TOTALS-PER14.
    PT_OUTTAB-PER15 = PT_TOTALS-PER15.
    PT_OUTTAB-PER16 = PT_TOTALS-PER16.

    PT_OUTTAB-TLEVEL  = PT_TOTALS-TLEVEL.
*.. calculate variance
    PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                          PT_OUTTAB-REPVAL
                                          PT_OUTTAB-COMPVAL
                                 CHANGING PT_OUTTAB-ABSVAR
                                          PT_OUTTAB-RELVAR.

    PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                          PT_OUTTAB-REPVAL2
                                          PT_OUTTAB-COMPVAL2
                                 CHANGING PT_OUTTAB-ABSVAR2
                                          PT_OUTTAB-RELVAR2.

    PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_OUTTAB-REPVAL
                                          PT_OUTTAB-REPVAL2
                                          PT_OUTTAB-COMPVAL
                                          PT_OUTTAB-COMPVAL2
                                          PT_OUTTAB-ABSVAR
                                          PT_OUTTAB-ABSVAR2
                                 CHANGING PT_OUTTAB-REPVALV
                                          PT_OUTTAB-REPVAL2V
                                          PT_OUTTAB-REPVAL3V.

*.. provide fields like ccode, busarea, funcarea, account no etc.

    LOOP AT PT_DATA WHERE ERGSL = PT_TOTALS-ERGSL.
      CHECK PT_DATA-RBUKRS = L_BUKRS.

      IF NOT ( PS_SETTINGS-ALTACCT IS INITIAL ).
*...... alternative account number is used
        ASSIGN: PT_DATA-KTOP2 TO <KTOPL>,
                PT_DATA-ALTKT TO <RACCT>.
      ELSEIF NOT ( T011-XERGS IS INITIAL ).
*...... group account number is used
        ASSIGN: PT_DATA-KKTPL TO <KTOPL>,
                PT_DATA-BILKT TO <RACCT>.
      ELSE.
*...... default normal accounts used
        ASSIGN: PT_DATA-KTOPL TO <KTOPL>,
                PT_DATA-RACCT TO <RACCT>.
      ENDIF.

      LS_NODE_SAKNR-KTOPL = <KTOPL>.
      LS_NODE_SAKNR-SAKNR = <RACCT>.
*     CHECK <KTOPL> = PS_NODE_SAKNR-KTOPL. "n1559627
      CHECK <RACCT> = PS_NODE_SAKNR-SAKNR.
      IF P_FBERFLG NE SPACE.
        CHECK PT_DATA-RFAREA = P_NODE_FKBER.
      ENDIF.
*...... get name of account
      IF P_FBERFLG = SPACE.
        CALL FUNCTION 'READ_HAUPTBUCH_TEXT'
          EXPORTING
            KONTENPLAN     = <KTOPL>
            SACHKONTO      = <RACCT>
            SPRACHE        = IS_SETTINGS-FS_LANGUAGE
          IMPORTING
            TEXT_WA        = SKAT
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC = 0.
*...... fix account number to relevant length
          PERFORM BSPL_ACCOUNT_LENGTH_FIX
                                 CHANGING LS_NODE_SAKNR.
          CONCATENATE LS_NODE_SAKNR-SAKNR
                      SKAT-TXT50
                 INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
        ELSE.
*...... fix account number to relevant length
          PERFORM BSPL_ACCOUNT_LENGTH_FIX
                                 CHANGING LS_NODE_SAKNR.
          CONCATENATE LS_NODE_SAKNR-KTOPL
                      LS_NODE_SAKNR-SAKNR
                   INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
        ENDIF.
*...... get name of functional area
      ELSE.
        SELECT SINGLE FKBTX INTO PT_OUTTAB-TEXT
                            FROM TFKBT
                            WHERE SPRAS = IS_SETTINGS-FS_LANGUAGE
                              AND FKBER = P_NODE_FKBER.
        IF SY-SUBRC = 0.
          CONCATENATE P_NODE_FKBER
                      PT_OUTTAB-TEXT
                 INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
        ELSE.
          PT_OUTTAB-TEXT = PS_NODE-NAME.
        ENDIF.
      ENDIF.

      PT_OUTTAB-KTOPL  = PT_DATA-KTOPL.
*     provide operational account no
      IF PT_OUTTAB-RACCT IS INITIAL.
        PT_OUTTAB-RACCT  = PT_DATA-RACCT.
      ELSEIF PT_OUTTAB-RACCT <> PT_DATA-RACCT.
*       1:n assignment between group account no
*                    and operational account no
        PT_OUTTAB-RACCT = CON_STARS.
      ENDIF.
      PT_OUTTAB-KKTPL  = PT_DATA-KKTPL.
      PT_OUTTAB-BILKT  = PT_DATA-BILKT.
      PT_OUTTAB-KTOP2  = PT_DATA-KTOP2.
      PT_OUTTAB-ALTKT  = PT_DATA-ALTKT.
      PT_OUTTAB-RYEAR  = PT_DATA-RYEAR.
      PT_OUTTAB-POPER  = PT_DATA-POPER.
*     provide company code
      IF PT_OUTTAB-RBUKRS IS INITIAL.
        PT_OUTTAB-RBUKRS = PT_DATA-RBUKRS.
      ELSEIF PT_OUTTAB-RBUKRS <> PT_DATA-RBUKRS.
*       more than one ccode, mark it with stars
        PT_OUTTAB-RBUKRS = CON_STARS.
      ENDIF.
*     provide business area
      IF PT_OUTTAB-RBUSA IS INITIAL
     AND PT_DATA-RBUSA   IS INITIAL.
        PT_OUTTAB-RBUSA  = SY-VLINE.
      ELSEIF PT_OUTTAB-RBUSA IS INITIAL.
        PT_OUTTAB-RBUSA  = PT_DATA-RBUSA.
      ELSEIF PT_OUTTAB-RBUSA <> PT_DATA-RBUSA.
*       more than one busarea, mark it with stars
        PT_OUTTAB-RBUSA = CON_STARS.
      ENDIF.
*     privide function area
      IF PT_OUTTAB-RFAREA IS INITIAL.
        PT_OUTTAB-RFAREA = PT_DATA-RFAREA.
      ELSEIF PT_OUTTAB-RFAREA <> PT_DATA-RFAREA.
*       more than one function area, mark it with stars
        PT_OUTTAB-RFAREA = CON_STARS.
      ENDIF.
    ENDLOOP.
    IF PT_OUTTAB-RBUSA  = SY-VLINE.
*.... only initial business area
      CLEAR: PT_OUTTAB-RBUSA.
    ENDIF.
    PERFORM BSPL_GRID_CELL_COLOR_SET
                            TABLES PT_OUTTAB-CELL_COLOR.
  ENDIF.

  APPEND PT_OUTTAB.

ENDFORM.                    " BSPL_GRID_CCOD_TO_OUTTAB

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM BSPL_GRID_PF_STATUS_SET USING T_EXUC TYPE SLIS_T_EXTAB.
* local data declarations
  DATA: LT_EXUC TYPE SLIS_T_EXTAB.
  DATA: LS_EXUC TYPE SLIS_EXTAB.

  APPEND '&ALL'     TO LT_EXUC.
  APPEND '&SAL'     TO LT_EXUC.
  APPEND '&REFRESH' TO LT_EXUC.
  APPEND '&EB9'     TO LT_EXUC.

  LOOP AT T_EXUC INTO LS_EXUC WHERE FCODE = '&XINT'.
    COLLECT LS_EXUC INTO LT_EXUC.
  ENDLOOP.

  CASE G_LIST_STATE.
    WHEN CON_ACCT.
      APPEND 'COMP'          TO LT_EXUC.
      APPEND 'COMP_BUSA'     TO LT_EXUC.
      IF X_BUKRS_UNIQUE <> SPACE.
        APPEND 'EXPA'        TO LT_EXUC.
        IF X_RBUSA_UNIQUE <> SPACE.
          APPEND 'EXPA_BUSA' TO LT_EXUC.
        ENDIF.
      ELSE.
        APPEND 'EXPA_BUSA'   TO LT_EXUC.
      ENDIF.

    WHEN CON_CCOD.
      APPEND 'EXPA'          TO LT_EXUC.
      APPEND 'COMP_BUSA'     TO LT_EXUC.
      IF X_BUKRS_UNIQUE <> SPACE.
        APPEND 'COMP'        TO LT_EXUC.
      ENDIF.
      IF X_RBUSA_UNIQUE <> SPACE.
        APPEND 'EXPA_BUSA'   TO LT_EXUC.
      ENDIF.

    WHEN CON_LEAF.
      IF X_BUKRS_UNIQUE <> SPACE.
        APPEND 'COMP'          TO LT_EXUC.
      ENDIF.
      APPEND 'EXPA'          TO LT_EXUC.
      APPEND 'EXPA_BUSA'     TO LT_EXUC.
  ENDCASE.

  CALL FUNCTION 'FUNCTION_EXISTS'                           "n1489749
    EXPORTING                                               "n1489749
      FUNCNAME = 'FB_SELECTIONS_DISPLAY'                    "n1489749
    EXCEPTIONS                                              "n1489749
      OTHERS   = 1.                                         "n1489749
  IF SY-SUBRC <> 0.                                         "n1489749
    APPEND 'SEL' TO LT_EXUC.                                "n1489749
  ENDIF.                                                    "n1489749


  SET PF-STATUS 'GRID_FULLSCREEN'
                           EXCLUDING LT_EXUC.


ENDFORM.                    " BSPL_GRID_PF_STATUS_SET

*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_USER_COMMAND
*&---------------------------------------------------------------------*
FORM BSPL_GRID_USER_COMMAND USING P_UCOMM     LIKE SY-UCOMM
                             PS_SELFIELD TYPE SLIS_SELFIELD.

  CASE P_UCOMM.
    WHEN 'EXPA'.
      G_LIST_STATE        = CON_CCOD.
      PS_SELFIELD-REFRESH = CON_X.

    WHEN 'COMP'.
      G_LIST_STATE        = CON_ACCT.
      PS_SELFIELD-REFRESH = CON_X.

    WHEN 'EXPA_BUSA'.
      G_LIST_STATE        = CON_LEAF.
      PS_SELFIELD-REFRESH = CON_X.

    WHEN 'COMP_BUSA'.
      G_LIST_STATE        = CON_CCOD.
      PS_SELFIELD-REFRESH = CON_X.

    WHEN 'SEL'.                                             "n1489749
      CALL FUNCTION 'FB_SELECTIONS_DISPLAY'                 "n1489749
        EXPORTING                                           "n1489749
          ED_PROGRAM = SY-CPROG                             "n1489749
          ED_MODE    = '1'.                                 "n1489749
    WHEN OTHERS. EXIT.
  ENDCASE.

  REFRESH: GT_GRIDOUTTAB.
  CLEAR:   GT_GRIDOUTTAB.
  PERFORM BSPL_GRID_OUTTAB_FILL      TABLES GT_RSTHIE
                                            GT_BSPLDATA
                                            GT_GRIDTOTALS
                                            GT_ERGSL_TEXT
                                            GT_EDIT_SETTINGS
                                            GT_GRIDOUTTAB
                                      USING GS_SETTINGS.

  PERFORM BSPL_GRID_NEWPAGE_SET      TABLES GT_GRIDOUTTAB.

ENDFORM.                    " BSPL_GRID_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  BSPL_GRID_LEAF_TO_OUTTAB  (Geschäftsbereich)
*&---------------------------------------------------------------------*
FORM BSPL_GRID_LEAF_TO_OUTTAB
                       TABLES PT_TOTALS      TYPE TT_GRID_TOTALS
                              PT_OUTTAB      TYPE TT_GRID_OUTTAB
                              PT_DATA        TYPE TT_BSPL_DATA
                 USING  VALUE(PS_NODE)       LIKE RSTHIE
                        VALUE(PS_SETTINGS)   LIKE RFBILA_ALV_SETTINGS
                        VALUE(PS_NODE_SAKNR) TYPE TS_NODE_SAKNR
                        VALUE(P_NODE_BUKRS)  LIKE T001-BUKRS
                        VALUE(P_NODE_FKBER)  LIKE GT_FKBER_ID-FKBER
                        VALUE(P_FBERFLG)     LIKE CON_X
              CHANGING  VALUE(P_ID)          LIKE RSTHIE-ID.

*     local data declarations
  DATA: LS_NODE_SAKNR       TYPE TS_NODE_SAKNR.
  DATA: L_BUSA              TYPE GSBER.

  CLEAR: PT_OUTTAB.
  L_BUSA = PS_NODE-NAME.

  P_ID = P_ID + 1.
  PT_OUTTAB-ID      = P_ID.        .
  PT_OUTTAB-SUBID   = 1.
* read totals of account number
  READ TABLE PT_TOTALS WITH KEY ID = PS_NODE-ID
                                     BINARY SEARCH.
  IF SY-SUBRC = 0.
    PT_OUTTAB-ERGSL   = PT_TOTALS-ERGSL.
    PT_OUTTAB-CURTP   = PT_TOTALS-CURTP.
    PT_OUTTAB-WAERS   = PT_TOTALS-WAERS.
    PT_OUTTAB-REPVAL  = PT_TOTALS-REPVAL.
    PT_OUTTAB-COMPVAL = PT_TOTALS-COMPVAL.

    PT_OUTTAB-WAER2    = PT_TOTALS-WAER2.
    PT_OUTTAB-REPVAL2  = PT_TOTALS-REPVAL2.
    PT_OUTTAB-COMPVAL2 = PT_TOTALS-COMPVAL2.

    "pt_outtab-repvalv  = pt_totals-repvalv.
    "pt_outtab-repval2v = pt_totals-repval2v.

    PT_OUTTAB-PER01 = PT_TOTALS-PER01.
    PT_OUTTAB-PER02 = PT_TOTALS-PER02.
    PT_OUTTAB-PER03 = PT_TOTALS-PER03.
    PT_OUTTAB-PER04 = PT_TOTALS-PER04.
    PT_OUTTAB-PER05 = PT_TOTALS-PER05.
    PT_OUTTAB-PER06 = PT_TOTALS-PER06.
    PT_OUTTAB-PER07 = PT_TOTALS-PER07.
    PT_OUTTAB-PER08 = PT_TOTALS-PER08.
    PT_OUTTAB-PER09 = PT_TOTALS-PER09.
    PT_OUTTAB-PER10 = PT_TOTALS-PER10.
    PT_OUTTAB-PER11 = PT_TOTALS-PER11.
    PT_OUTTAB-PER12 = PT_TOTALS-PER12.
    PT_OUTTAB-PER13 = PT_TOTALS-PER13.
    PT_OUTTAB-PER14 = PT_TOTALS-PER14.
    PT_OUTTAB-PER15 = PT_TOTALS-PER15.
    PT_OUTTAB-PER16 = PT_TOTALS-PER16.

    PT_OUTTAB-TLEVEL  = PT_TOTALS-TLEVEL.
*.. calculate variance
    PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                          PT_OUTTAB-REPVAL
                                          PT_OUTTAB-COMPVAL
                                 CHANGING PT_OUTTAB-ABSVAR
                                          PT_OUTTAB-RELVAR.

    PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                          PT_OUTTAB-REPVAL2
                                          PT_OUTTAB-COMPVAL2
                                 CHANGING PT_OUTTAB-ABSVAR2
                                          PT_OUTTAB-RELVAR2.

    PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_OUTTAB-REPVAL
                                          PT_OUTTAB-REPVAL2
                                          PT_OUTTAB-COMPVAL
                                          PT_OUTTAB-COMPVAL2
                                          PT_OUTTAB-ABSVAR
                                          PT_OUTTAB-ABSVAR2
                                 CHANGING PT_OUTTAB-REPVALV
                                          PT_OUTTAB-REPVAL2V
                                          PT_OUTTAB-REPVAL3V.

*.. provide fields like ccode, busarea, funcarea, account no etc.

    LOOP AT PT_DATA WHERE ERGSL = PT_TOTALS-ERGSL.
      CHECK PT_DATA-RBUSA = L_BUSA.

      IF NOT ( PS_SETTINGS-ALTACCT IS INITIAL ).
*...... alternative account number is used
        ASSIGN: PT_DATA-KTOP2 TO <KTOPL>,
                PT_DATA-ALTKT TO <RACCT>.
      ELSEIF NOT ( T011-XERGS IS INITIAL ).
*...... group account number is used
        ASSIGN: PT_DATA-KKTPL TO <KTOPL>,
                PT_DATA-BILKT TO <RACCT>.
      ELSE.
*...... default normal accounts used
        ASSIGN: PT_DATA-KTOPL TO <KTOPL>,
                PT_DATA-RACCT TO <RACCT>.
      ENDIF.

      LS_NODE_SAKNR-KTOPL = <KTOPL>.
      LS_NODE_SAKNR-SAKNR = <RACCT>.
*     CHECK <KTOPL> = PS_NODE_SAKNR-KTOPL. "n1559627
      CHECK <RACCT> = PS_NODE_SAKNR-SAKNR.
      CHECK PT_DATA-RBUKRS = P_NODE_BUKRS.
      IF P_FBERFLG NE SPACE.
        CHECK PT_DATA-RFAREA = P_NODE_FKBER.
      ENDIF.
*...... get name of account
      IF P_FBERFLG = SPACE.
        CALL FUNCTION 'READ_HAUPTBUCH_TEXT'
          EXPORTING
            KONTENPLAN     = <KTOPL>
            SACHKONTO      = <RACCT>
            SPRACHE        = IS_SETTINGS-FS_LANGUAGE
          IMPORTING
            TEXT_WA        = SKAT
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC = 0.
*...... fix account number to relevant length
          PERFORM BSPL_ACCOUNT_LENGTH_FIX
                                 CHANGING LS_NODE_SAKNR.
          CONCATENATE LS_NODE_SAKNR-SAKNR
                      SKAT-TXT50
                 INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
        ELSE.
*...... fix account number to relevant length
          PERFORM BSPL_ACCOUNT_LENGTH_FIX
                                 CHANGING LS_NODE_SAKNR.
          CONCATENATE LS_NODE_SAKNR-KTOPL
                      LS_NODE_SAKNR-SAKNR
                 INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
        ENDIF.
*...... get name of functional area
      ELSE.
        SELECT SINGLE FKBTX INTO PT_OUTTAB-TEXT
                            FROM TFKBT
                            WHERE SPRAS = IS_SETTINGS-FS_LANGUAGE
                              AND FKBER = P_NODE_FKBER.
        IF SY-SUBRC = 0.
          CONCATENATE P_NODE_FKBER
                      PT_OUTTAB-TEXT
                 INTO PT_OUTTAB-TEXT SEPARATED BY ' '.
        ELSE.
          PT_OUTTAB-TEXT = PS_NODE-NAME.
        ENDIF.
      ENDIF.

      PT_OUTTAB-KTOPL  = PT_DATA-KTOPL.
*     provide operational account no
      IF PT_OUTTAB-RACCT IS INITIAL.
        PT_OUTTAB-RACCT  = PT_DATA-RACCT.
      ELSEIF PT_OUTTAB-RACCT <> PT_DATA-RACCT.
*       1:n assignment between group account no
*                    and operational account no
        PT_OUTTAB-RACCT = CON_STARS.
      ENDIF.
      PT_OUTTAB-KKTPL  = PT_DATA-KKTPL.
      PT_OUTTAB-BILKT  = PT_DATA-BILKT.
      PT_OUTTAB-KTOP2  = PT_DATA-KTOP2.
      PT_OUTTAB-ALTKT  = PT_DATA-ALTKT.
      PT_OUTTAB-RYEAR  = PT_DATA-RYEAR.
      PT_OUTTAB-POPER  = PT_DATA-POPER.
*     provide company code
      IF PT_OUTTAB-RBUKRS IS INITIAL.
        PT_OUTTAB-RBUKRS = PT_DATA-RBUKRS.
      ELSEIF PT_OUTTAB-RBUKRS <> PT_DATA-RBUKRS.
*       more than one ccode, mark it with stars
        PT_OUTTAB-RBUKRS = CON_STARS.
      ENDIF.

*     provide business area
      IF PT_OUTTAB-RBUSA IS INITIAL
     AND PT_DATA-RBUSA   IS INITIAL.
        PT_OUTTAB-RBUSA  = SY-VLINE.
      ELSEIF PT_OUTTAB-RBUSA IS INITIAL.
        PT_OUTTAB-RBUSA  = PT_DATA-RBUSA.
      ELSEIF PT_OUTTAB-RBUSA <> PT_DATA-RBUSA.
*       more than one busarea, mark it with stars
        PT_OUTTAB-RBUSA = CON_STARS.
      ENDIF.

*     privide function area
      IF PT_OUTTAB-RFAREA IS INITIAL.
        PT_OUTTAB-RFAREA = PT_DATA-RFAREA.
      ELSEIF PT_OUTTAB-RFAREA <> PT_DATA-RFAREA.
*       more than one function area, mark it with stars
        PT_OUTTAB-RFAREA = CON_STARS.
      ENDIF.
    ENDLOOP.
    IF PT_OUTTAB-RBUSA  = SY-VLINE.
*.... only initial business area
      CLEAR: PT_OUTTAB-RBUSA.
    ENDIF.
    PERFORM BSPL_GRID_CELL_COLOR_SET
                            TABLES PT_OUTTAB-CELL_COLOR.
  ENDIF.

  APPEND PT_OUTTAB.

ENDFORM.                    " BSPL_GRID_BUSA_TO_OUTTAB

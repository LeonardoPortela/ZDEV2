*----------------------------------------------------------------------*
***INCLUDE LBSPLF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BSPL_NOT_ASSIGNED_ACC_FIND
*&---------------------------------------------------------------------*
FORM BSPL_NOT_ASSIGNED_ACC_FIND
             TABLES PT_BILADATA STRUCTURE ZRFBILA_ALV_DATA
             USING VALUE(PS_SETTINGS) STRUCTURE RFBILA_ALV_SETTINGS.
  .
*.... local data declaration
  DATA: L_ASSIGNMENT_OK   LIKE CON_X VALUE SPACE.
  DATA: LS_BILADATA       LIKE ZRFBILA_ALV_DATA.
  DATA: LT_NOT_ASSIGNED   TYPE TT_NOT_ASS         WITH HEADER LINE.
  CONSTANTS:
        CON_MEMORY_KEY_NOT_ASSGND_ACC(21)
                            VALUE 'ACCOUNTS_NOT_ASSIGNED'.

* delete not assigned accounts of former run
  FREE MEMORY ID CON_MEMORY_KEY_NOT_ASSGND_ACC.

* get infos about the fin.statement version
  CALL FUNCTION 'RGRE_FINSTATEMENT_INFOS_GET'
    EXPORTING
      I_BS_VERSION         = T011-VERSN
    IMPORTING
      ES_NODE_NOT_ASSIGNED = GS_NODE_NOT_ASSIGNED
    TABLES
      ET_RSTHIE            = GT_RSTHIE
      ET_CHANG_ID          = GT_CHANGE_ID
      ET_SAKNR_ID          = GT_SAKNR_ID
      ET_FKBER_ID          = GT_FKBER_ID
      ET_BLNCE_SHEET_ID    = GR_BLNCE_SHEET_ID
      ET_PROF_LOSS_ID      = GR_PROF_LOSS_ID
      ET_BS_NOTES_ID       = GR_BS_NOTES_ID
      ET_EDIT_SETTINGS     = GT_EDIT_SETTINGS
    EXCEPTIONS
      BS_VERSION_NOT_EXIST = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF NOT ( PS_SETTINGS-ALTACCT IS INITIAL ).
*... alternative account number is used
    SORT    PT_BILADATA BY KTOP2 ALTKT PERIOD_SIGN.
    ASSIGN: LS_BILADATA-KTOP2 TO <KTOPL>,
            LS_BILADATA-ALTKT TO <RACCT>.
  ELSEIF NOT ( T011-XERGS IS INITIAL ).
*... group account number is used
    SORT    PT_BILADATA BY KKTPL BILKT PERIOD_SIGN.
    ASSIGN: LS_BILADATA-KKTPL TO <KTOPL>,
            LS_BILADATA-BILKT TO <RACCT>.
  ELSE.
*... default normal accounts used
    SORT    PT_BILADATA BY KTOPL RACCT PERIOD_SIGN.
    ASSIGN: LS_BILADATA-KTOPL TO <KTOPL>,
            LS_BILADATA-RACCT TO <RACCT>.
  ENDIF.

  LOOP AT PT_BILADATA
                  INTO LS_BILADATA.
    CLEAR L_ASSIGNMENT_OK.

    IF ( LS_BILADATA-RFAREA IS INITIAL )
    OR ( T011-XFBER          IS INITIAL ).
*.... read parent of account
      READ TABLE GT_SAKNR_ID WITH KEY KTOPL = <KTOPL>
                                      SAKNR = <RACCT>
                                      BINARY SEARCH.
      IF SY-SUBRC = 0.
*...... assignment is ok
        L_ASSIGNMENT_OK  = CON_X.
      ENDIF.
    ELSE.
*.... read parent of function area
      READ TABLE GT_FKBER_ID
                 WITH KEY FKBER = LS_BILADATA-RFAREA
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
*...... assignment is ok
        L_ASSIGNMENT_OK  = CON_X.
      ENDIF.
    ENDIF.

    IF NOT ( L_ASSIGNMENT_OK = CON_X ).
      IF PS_SETTINGS-TREE       = CON_X
     AND PS_SETTINGS-STRUCBLNCE = CON_X.
*       structured balance list, no not assigned accounts.
        DELETE PT_BILADATA.
      ELSE.
*...... notice not assigned accounts
        LT_NOT_ASSIGNED-KTOPL = <KTOPL>.
        LT_NOT_ASSIGNED-SAKNR = <RACCT>.
*   what goes on with empty group or alternative account ?????
*       IF <KTOPL> IS INITIAL
*       OR <RACCT> IS INITIAL.
*........ in case of empty group or alternative account
*         LT_NOT_ASSIGNED-KTOPL = LS_BILADATA-KTOPL.
*         LT_NOT_ASSIGNED-SAKNR = LS_BILADATA-RACCT.
*       ENDIF.
        COLLECT LT_NOT_ASSIGNED.
      ENDIF.
    ENDIF.
  ENDLOOP.

* export not assigned accounts to memory
  DESCRIBE TABLE LT_NOT_ASSIGNED LINES SY-TFILL.
  IF SY-TFILL <> 0.
    EXPORT LT_NOT_ASSIGNED TO MEMORY
                           ID CON_MEMORY_KEY_NOT_ASSGND_ACC.

*.. get infos about the fin.statement version
*.. including the not assigned accounts.
    CALL FUNCTION 'RGRE_FINSTATEMENT_INFOS_GET'
      EXPORTING
        I_BS_VERSION         = T011-VERSN
      IMPORTING
        ES_NODE_NOT_ASSIGNED = GS_NODE_NOT_ASSIGNED
      TABLES
        ET_RSTHIE            = GT_RSTHIE
        ET_CHANG_ID          = GT_CHANGE_ID
        ET_SAKNR_ID          = GT_SAKNR_ID
        ET_FKBER_ID          = GT_FKBER_ID
        ET_BLNCE_SHEET_ID    = GR_BLNCE_SHEET_ID
        ET_PROF_LOSS_ID      = GR_PROF_LOSS_ID
        ET_BS_NOTES_ID       = GR_BS_NOTES_ID
      EXCEPTIONS
        BS_VERSION_NOT_EXIST = 1
        OTHERS               = 2.

    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " BSPL_NOT_ASSIGNED_ACC_FIND

*&---------------------------------------------------------------------*
*&      Form  BSPL_ITEMS_ADD
*&---------------------------------------------------------------------*
FORM BSPL_ITEMS_ADD
             TABLES PT_BILADATA STRUCTURE ZRFBILA_ALV_DATA
             USING VALUE(P_ALTACCT).

*.... local data declaration
  DATA: L_ASSIGNMENT_OK   LIKE CON_X VALUE SPACE.
  DATA: L_TABIX           LIKE SY-TABIX.
  DATA: LS_BILADATA       LIKE ZRFBILA_ALV_DATA.

  LOOP AT PT_BILADATA ASSIGNING <BILADATA>.
    IF NOT ( P_ALTACCT IS INITIAL ).
*.... alternative account number is used
      ASSIGN: <BILADATA>-KTOP2 TO <KTOPL>,
              <BILADATA>-ALTKT TO <RACCT>.
    ELSEIF NOT ( T011-XERGS IS INITIAL ).
*.... group account number is used
      ASSIGN: <BILADATA>-KKTPL TO <KTOPL>,
              <BILADATA>-BILKT TO <RACCT>.
    ELSE.
*.... default normal accounts used
      ASSIGN: <BILADATA>-KTOPL TO <KTOPL>,
              <BILADATA>-RACCT TO <RACCT>.
    ENDIF.

    CLEAR:  L_ASSIGNMENT_OK.

    IF ( <BILADATA>-RFAREA IS INITIAL )
    OR ( T011-XFBER        IS INITIAL ).
*.... read parent of account
      READ TABLE GT_SAKNR_ID WITH KEY KTOPL = <KTOPL>
                                      SAKNR = <RACCT>
                                      BINARY SEARCH.
      IF SY-SUBRC = 0.
*...... assignment is ok
        L_ASSIGNMENT_OK  = CON_X.
*...... save sy-tabix
        L_TABIX = SY-TABIX.
*...... complete LS_BILADATA
        <BILADATA>-ERGSL   = GT_SAKNR_ID-ERGSL.
        <BILADATA>-CHANGID = GT_SAKNR_ID-PARENT.
        <BILADATA>-PARENT  = GT_SAKNR_ID-PARENT.
        <BILADATA>-ID      = GT_SAKNR_ID-ID.

        IF <BILADATA>-PARENT = <BILADATA>-ID.
*........ this is a single account change item
*........ get the right parent
          READ TABLE GT_RSTHIE INDEX <BILADATA>-ID.
          <BILADATA>-PARENT = GT_RSTHIE-PARENT.
        ENDIF.

        IF NOT ( GT_SAKNR_ID-PLUMI IS INITIAL ).
*........ account of a change position
*........ create it to table GT_CHANGE_POS_DATA
          L_TABIX = L_TABIX + 1.
          READ TABLE GT_SAKNR_ID INDEX L_TABIX.
          IF GT_SAKNR_ID-KTOPL = <KTOPL>
         AND GT_SAKNR_ID-SAKNR = <RACCT>
         AND SY-SUBRC = 0.
            LS_BILADATA = <BILADATA>.
            LS_BILADATA-ERGSL   = GT_SAKNR_ID-ERGSL.
            LS_BILADATA-CHANGID = GT_SAKNR_ID-PARENT.
            LS_BILADATA-PARENT  = GT_SAKNR_ID-PARENT.
            LS_BILADATA-ID      = GT_SAKNR_ID-ID.

            IF LS_BILADATA-PARENT = LS_BILADATA-ID.
*............ this is a single account change item
*............ get the right parent
              READ TABLE GT_RSTHIE INDEX LS_BILADATA-ID.
              LS_BILADATA-PARENT = GT_RSTHIE-PARENT.
            ENDIF.

            APPEND LS_BILADATA TO GT_CHANGE_POS_DATA.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
*.... read parent of function area
      READ TABLE GT_FKBER_ID
                 WITH KEY FKBER = <BILADATA>-RFAREA
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
*...... assignment is ok
        L_ASSIGNMENT_OK  = CON_X.
*...... save sy-tabix
        L_TABIX = SY-TABIX.
*...... complete PT_BILADATA
        <BILADATA>-ERGSL   = GT_FKBER_ID-ERGSL.
        <BILADATA>-CHANGID = GT_FKBER_ID-PARENT.
        <BILADATA>-PARENT  = GT_FKBER_ID-PARENT.
        <BILADATA>-ID      = GT_FKBER_ID-ID.
      ENDIF.
    ENDIF.

    IF NOT ( L_ASSIGNMENT_OK = CON_X ).
*.... account not assigned
      <BILADATA>-ERGSL  = T011-ZUORD.
      <BILADATA>-PARENT = GS_NODE_NOT_ASSIGNED-ID.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " BSPL_ITEMS_ADD

*&---------------------------------------------------------------------*
*&      Form  BSPL_RESULT_CALCULATE
*&---------------------------------------------------------------------*
FORM BSPL_RESULT_CALCULATE
             TABLES PT_BILADATA  STRUCTURE ZRFBILA_ALV_DATA.
*.... local data declaration
  DATA: LS_BILADATA      LIKE ZRFBILA_ALV_DATA.
  DATA: LS_BS_PROF       TYPE TS_SAKNR_PARENT.
  DATA: LS_BS_LOSS       TYPE TS_SAKNR_PARENT.
  DATA: LS_PL_PROF       TYPE TS_SAKNR_PARENT.

  READ TABLE GT_SAKNR_ID INTO LS_BS_PROF
                         WITH KEY ERGSL = T011-ERGPA.
  READ TABLE GT_SAKNR_ID INTO LS_BS_LOSS
                         WITH KEY ERGSL = T011-ERGAK.
  READ TABLE GT_SAKNR_ID INTO LS_PL_PROF
                         WITH KEY ERGSL = T011-ERGGV.

  SORT PT_BILADATA
       BY CHANGID PERIOD_SIGN.

  LOOP AT PT_BILADATA INTO LS_BILADATA
                      WHERE ERGSL <> T011-ZUORD.

    CLEAR: LS_BILADATA-KTOPL,
           LS_BILADATA-KKTPL,
           LS_BILADATA-KTOP2,
           LS_BILADATA-RBUKRS,
           LS_BILADATA-RBUSA,
           LS_BILADATA-RFAREA.
    LS_BILADATA-RACCT = CON_RESULT.
    LS_BILADATA-BILKT = CON_RESULT.
    LS_BILADATA-ALTKT = CON_RESULT.

    LS_BILADATA-SALDO  = LS_BILADATA-SALDO  * -1.
    LS_BILADATA-SALDO2 = LS_BILADATA-SALDO2 * -1.

    IF LS_BILADATA-PARENT IN GR_BLNCE_SHEET_ID.
*.... calculate the result of balance sheet
      LS_BILADATA-CHANGID = LS_BS_PROF-PARENT.
      LS_BILADATA-PARENT  = LS_BS_PROF-PARENT.
      LS_BILADATA-ID      = LS_BS_PROF-ID.
      LS_BILADATA-ERGSL   = LS_BS_PROF-ERGSL.
      COLLECT  LS_BILADATA INTO GT_RESULT.

      LS_BILADATA-CHANGID = LS_BS_LOSS-PARENT.
      LS_BILADATA-PARENT  = LS_BS_LOSS-PARENT.
      LS_BILADATA-ID      = LS_BS_LOSS-ID.
      LS_BILADATA-ERGSL   = LS_BS_LOSS-ERGSL.
      COLLECT  LS_BILADATA INTO GT_RESULT.

    ELSEIF LS_BILADATA-PARENT IN GR_PROF_LOSS_ID.
*.... calculate the result of p & l
      LS_BILADATA-CHANGID = LS_PL_PROF-PARENT.
      LS_BILADATA-PARENT  = LS_PL_PROF-PARENT.
      LS_BILADATA-ID      = LS_PL_PROF-ID.
      LS_BILADATA-ERGSL   = LS_PL_PROF-ERGSL.
      COLLECT  LS_BILADATA INTO GT_RESULT.
    ENDIF.
  ENDLOOP.
  SORT GT_RESULT.

ENDFORM.                    " BSPL_RESULT_CALCULATE

*&---------------------------------------------------------------------*
*&      Form  CHANGE_POSITIONS_PREPARE
*&---------------------------------------------------------------------*
FORM BSPL_CHANGE_POSITIONS_PREPARE
             TABLES PT_BILADATA STRUCTURE ZRFBILA_ALV_DATA
              USING VALUE(P_PERIOD_SIGN).
* local data declarations
  DATA: L_BALANCE        LIKE ZRFBILA_ALV_DATA-SALDO.
  DATA: L_BALANCE2       LIKE ZRFBILA_ALV_DATA-SALDO2.
  DATA: L_START_TABIX    LIKE SY-TABIX.
  DATA: L_END_TABIX      LIKE SY-TABIX.

  SORT PT_BILADATA BY CHANGID
                      ERGSL
                      PERIOD_SIGN.

  LOOP AT GT_CHANGE_ID.
    READ TABLE PT_BILADATA WITH KEY CHANGID = GT_CHANGE_ID-ID
                                              BINARY SEARCH.
    CHECK SY-SUBRC = 0.

    CLEAR: L_BALANCE,
           L_BALANCE2,
           L_START_TABIX,
           L_END_TABIX.

    L_START_TABIX = SY-TABIX.
    LOOP AT PT_BILADATA ASSIGNING <BILADATA>
                         FROM L_START_TABIX
                        WHERE CHANGID     = GT_CHANGE_ID-ID
                          AND PERIOD_SIGN = P_PERIOD_SIGN.

      IF <BILADATA>-CHANGID <> GT_CHANGE_ID-ID.
        EXIT.           " >>>>>>>>>>>>>>  E X I T  >>>>>>>>>>>
      ENDIF.
      L_END_TABIX  = SY-TABIX.
*.... calculate balance of change position
      L_BALANCE  = L_BALANCE  + <BILADATA>-SALDO.
      L_BALANCE2 = L_BALANCE2 + <BILADATA>-SALDO2.
    ENDLOOP.

    IF ( GT_CHANGE_ID-PLUMI = CON_PLUS  AND ( L_BALANCE <  0  AND L_BALANCE2 <  0 ) )
    OR ( GT_CHANGE_ID-PLUMI = CON_MINUS AND ( L_BALANCE >  0  AND L_BALANCE2 >  0  ) ). "n1067905
*    OR ( gt_change_id-plumi = con_minus AND l_balance >= 0 )."n1067905
*.... delete records belonging to that position
      DELETE PT_BILADATA FROM L_START_TABIX
                            TO L_END_TABIX
                         WHERE CHANGID     = GT_CHANGE_ID-ID
                           AND PERIOD_SIGN = P_PERIOD_SIGN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHANGE_POSITIONS_PREPARE

*&---------------------------------------------------------------------*
*&      Form  BSPL_DATA_PREPARE
*&---------------------------------------------------------------------*
FORM BSPL_DATA_PREPARE
             TABLES PT_BILADATA STRUCTURE ZRFBILA_ALV_DATA
                    PT_BSPLDATA TYPE TT_BSPL_DATA
             USING  PS_SETTINGS STRUCTURE RFBILA_ALV_SETTINGS.

  SORT PT_BILADATA.
  LOOP AT PT_BILADATA ASSIGNING <BILADATA>.
    AT NEW WAERS.
      CLEAR: PT_BSPLDATA-REPVAL,
             PT_BSPLDATA-COMPVAL,
             PT_BSPLDATA-REPVAL2,
             PT_BSPLDATA-COMPVAL2.
    ENDAT.

    IF <BILADATA>-PERIOD_SIGN = '1'.
      PT_BSPLDATA-REPVAL  = <BILADATA>-SALDO.
      PT_BSPLDATA-REPVAL2 = <BILADATA>-SALDO2.
    ENDIF.
    IF <BILADATA>-PERIOD_SIGN = '2'.
      PT_BSPLDATA-COMPVAL  = <BILADATA>-SALDO.
      PT_BSPLDATA-COMPVAL2 = <BILADATA>-SALDO2.
    ENDIF.

    AT END OF WAERS.
      MOVE-CORRESPONDING <BILADATA> TO PT_BSPLDATA.
*.... calculate variance
      PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                            PT_BSPLDATA-REPVAL
                                            PT_BSPLDATA-COMPVAL
                                   CHANGING PT_BSPLDATA-ABSVAR
                                            PT_BSPLDATA-RELVAR.

      PERFORM BSPL_VARIANCE_CALCULATE USING PS_SETTINGS-COMPTYPE
                                            PT_BSPLDATA-REPVAL2
                                            PT_BSPLDATA-COMPVAL2
                                   CHANGING PT_BSPLDATA-ABSVAR2
                                            PT_BSPLDATA-RELVAR2.

      PERFORM BSPL_VARIANCE_CALC_TAXA USING PT_BSPLDATA-REPVAL
                                            PT_BSPLDATA-REPVAL2
                                            PT_BSPLDATA-COMPVAL
                                            PT_BSPLDATA-COMPVAL2
                                            PT_BSPLDATA-ABSVAR
                                            PT_BSPLDATA-ABSVAR2
                                   CHANGING PT_BSPLDATA-REPVALV
                                            PT_BSPLDATA-REPVAL2V
                                            PT_BSPLDATA-REPVAL3V.

      APPEND PT_BSPLDATA.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " BSPL_DATA_PREPARE

*&---------------------------------------------------------------------*
*&      Form  BSPL_NODES_THIN_OUT
*&---------------------------------------------------------------------*
FORM BSPL_NODES_THIN_OUT TABLES PT_BSPLDATA TYPE TT_BSPL_DATA
                                PT_RSTHIE   TYPE TT_RSTHIE
                          USING VALUE(P_ALTACCT).
* local data declaration
  DATA: LT_NODE       LIKE SNODETEXT OCCURS 0 WITH HEADER LINE.
  DATA: L_LAST_ID     LIKE SNODETEXT-ID,
        L_NEW_ID      LIKE SNODETEXT-ID.
* local fields for group change
  DATA: L_OLD_ID      LIKE SNODETEXT-ID.
  DATA: L_OLD_RACCT   LIKE PT_BSPLDATA-RACCT.
  DATA: L_OLD_RBUKRS  LIKE PT_BSPLDATA-RBUKRS.
  DATA: L_OLD_RFAREA  LIKE PT_BSPLDATA-RFAREA.
  DATA: LS_NODE_SAKNR TYPE TS_NODE_SAKNR.

  FIELD-SYMBOLS: <NODE> LIKE LINE OF LT_NODE.

  SORT PT_BSPLDATA.

  LOOP AT PT_BSPLDATA ASSIGNING <BSPLDATA>.
    AT NEW PARENT.
*.... add the path which leads to the current leaf
      PERFORM BSPL_NODE_PATH_ADD TABLES LT_NODE
                                        PT_RSTHIE
                                  USING <BSPLDATA>-PARENT
                                        L_LAST_ID
                               CHANGING L_NEW_ID.
      L_LAST_ID = <BSPLDATA>-ID.
      CLEAR L_OLD_RACCT.
    ENDAT.

    IF NOT ( P_ALTACCT IS INITIAL ).
*.... alternative account number used
      ASSIGN: <BSPLDATA>-KTOP2 TO <KTOPL>,
              <BSPLDATA>-ALTKT TO <RACCT>.
    ELSEIF NOT ( T011-XERGS IS INITIAL ).
*.... group account number used
      ASSIGN: <BSPLDATA>-KKTPL TO <KTOPL>,
              <BSPLDATA>-BILKT TO <RACCT>.
    ELSE.
*.... default normal accounts used
      ASSIGN: <BSPLDATA>-KTOPL TO <KTOPL>,
              <BSPLDATA>-RACCT TO <RACCT>.
    ENDIF.
*   what goes on with empty group or alternative account ?????
*   IF <KTOPL> IS INITIAL
*   OR <RACCT> IS INITIAL.
*.... in case of empty group or alternative account
*     ASSIGN: <BSPLDATA>-KTOPL TO <KTOPL>,
*             <BSPLDATA>-RACCT TO <RACCT>.
*   ENDIF.

    IF <BSPLDATA>-ID <> L_OLD_ID.
      L_OLD_ID = <BSPLDATA>-ID.
      READ TABLE PT_RSTHIE INDEX <BSPLDATA>-ID.
    ENDIF.

    IF NOT ( <BSPLDATA>-RFAREA IS INITIAL )
   AND NOT ( T011-XFBER        IS INITIAL )
   AND NOT ( <BSPLDATA>-RFAREA CO CON_STARS ).  "acc with zero blnce
      IF <BSPLDATA>-RFAREA <> L_OLD_RFAREA.
        L_OLD_RFAREA = <BSPLDATA>-RFAREA.
        CLEAR: L_OLD_RACCT.
        L_NEW_ID       = L_NEW_ID + 1.
        LT_NODE-ID     = L_NEW_ID.
        LT_NODE-TYPE   = CON_FBER.
        LT_NODE-NAME   = <BSPLDATA>-RFAREA.
        LT_NODE-TLEVEL = PT_RSTHIE-TLEVEL.
        APPEND LT_NODE.
      ENDIF.
    ENDIF.

    IF <RACCT> <> L_OLD_RACCT
    OR <RACCT>  = CON_RESULT.
      L_OLD_RACCT = <RACCT>.
      CLEAR: L_OLD_RBUKRS.
      L_NEW_ID       = L_NEW_ID + 1.
      LT_NODE-ID     = L_NEW_ID.
      IF <RACCT> <> CON_RESULT.
        LT_NODE-TYPE   = CON_ACCT.
        IF NOT ( <BSPLDATA>-RFAREA IS INITIAL )
       AND NOT ( T011-XFBER        IS INITIAL )
       AND NOT ( <BSPLDATA>-RFAREA CO CON_STARS ). "acc with zero blnce
          CLEAR: LS_NODE_SAKNR.
          LS_NODE_SAKNR-KTOPL = <KTOPL>.
          LS_NODE_SAKNR-SAKNR = <RACCT>.
          LT_NODE-NAME   = LS_NODE_SAKNR.
          LT_NODE-TLEVEL = PT_RSTHIE-TLEVEL + 1.
        ELSE.
          LT_NODE-NAME   = PT_RSTHIE-NAME.
          LT_NODE-TLEVEL = PT_RSTHIE-TLEVEL.
        ENDIF.
        APPEND LT_NODE.
      ELSE.
*...... record of calculated result
        LT_NODE-TYPE   = CON_LEAF.
        LT_NODE-NAME   = CON_RESULT.
        LT_NODE-TLEVEL = PT_RSTHIE-TLEVEL.
        APPEND LT_NODE.
        <BSPLDATA>-ID  = L_NEW_ID.
        CONTINUE.        " >>>>> next loop
      ENDIF.
    ENDIF.

    IF       <BSPLDATA>-RBUKRS <> L_OLD_RBUKRS
   AND NOT ( <BSPLDATA>-RBUKRS CO CON_STARS ).    "acc with zero blnce
      L_OLD_RBUKRS = <BSPLDATA>-RBUKRS.
      L_NEW_ID       = L_NEW_ID + 1.
      LT_NODE-ID     = L_NEW_ID.
      LT_NODE-TYPE   = CON_CCOD.
      LT_NODE-NAME   = <BSPLDATA>-RBUKRS.
      LT_NODE-TLEVEL = PT_RSTHIE-TLEVEL + 1.
      IF NOT ( <BSPLDATA>-RFAREA IS INITIAL )
     AND NOT ( T011-XFBER        IS INITIAL )
     AND NOT ( <BSPLDATA>-RFAREA CO CON_STARS ). "acc with zero blnce
        LT_NODE-TLEVEL = LT_NODE-TLEVEL + 1.
      ENDIF.
      APPEND LT_NODE.
    ENDIF.

*.. RBUSA
    L_NEW_ID       = L_NEW_ID + 1.
    LT_NODE-ID     = L_NEW_ID.
    LT_NODE-TYPE   = CON_LEAF.
    LT_NODE-NAME   = <BSPLDATA>-RBUSA.
    LT_NODE-TLEVEL = PT_RSTHIE-TLEVEL + 2.
    IF NOT ( <BSPLDATA>-RFAREA IS INITIAL )
   AND NOT ( T011-XFBER        IS INITIAL )
   AND NOT ( <BSPLDATA>-RFAREA CO CON_STARS ). "acc with zero blnce
      LT_NODE-TLEVEL = LT_NODE-TLEVEL + 1.
    ENDIF.
    APPEND LT_NODE.

*.. modify PT_BSPLDATA-ID
*.. this modify requires the manual
*.. groub change handling above
    <BSPLDATA>-ID = L_NEW_ID.
  ENDLOOP.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    TABLES
      NODETAB      = LT_NODE
    EXCEPTIONS
      TREE_FAILURE = 04.
  IF SY-SUBRC <> 0.
    MESSAGE ID 'FE' TYPE 'A' NUMBER 710 WITH SY-REPID '01'.
  ENDIF.

  REFRESH PT_RSTHIE.
  LOOP AT LT_NODE ASSIGNING <NODE>.
    MOVE-CORRESPONDING <NODE> TO PT_RSTHIE.
    APPEND PT_RSTHIE.
  ENDLOOP.

ENDFORM.                    " BSPL_NODES_THIN_OUT

*&---------------------------------------------------------------------*
*&      Form  BSPL_NODE_PATH_ADD
*&---------------------------------------------------------------------*
*&      Add the path which leads to the current leaf
*&---------------------------------------------------------------------*
FORM BSPL_NODE_PATH_ADD TABLES   PT_NODE   STRUCTURE SNODETEXT
                                 PT_RSTHIE TYPE      TT_RSTHIE
                        USING    VALUE(P_DATA_PARENT)
                                 VALUE(P_LAST_ID)
                        CHANGING VALUE(P_NEW_ID).
* local data declaration
  DATA: LT_RSTHIE TYPE TT_RSTHIE WITH HEADER LINE.

  READ TABLE PT_RSTHIE INTO LT_RSTHIE INDEX P_DATA_PARENT.
  APPEND LT_RSTHIE.

  WHILE   LT_RSTHIE-PARENT >  P_LAST_ID
  AND NOT LT_RSTHIE-PARENT IS INITIAL.
    READ TABLE PT_RSTHIE INTO LT_RSTHIE INDEX LT_RSTHIE-PARENT.
    APPEND LT_RSTHIE.
  ENDWHILE.

  SORT LT_RSTHIE.
  LOOP AT LT_RSTHIE.
    P_NEW_ID       = P_NEW_ID + 1.
    PT_NODE-ID     = P_NEW_ID.
    PT_NODE-TYPE   = LT_RSTHIE-TYPE.
    PT_NODE-NAME   = LT_RSTHIE-NAME.
    PT_NODE-TLEVEL = LT_RSTHIE-TLEVEL.
    APPEND PT_NODE.
  ENDLOOP.
ENDFORM.                    " BSPL_NODE_PATH_ADD

*&---------------------------------------------------------------------*
*&      Form  BSPL_ZERO_BLNCE_ACCOUNTS_ADD
*&---------------------------------------------------------------------*
FORM BSPL_ZERO_BLNCE_ACCOUNTS_ADD
                              TABLES PT_BSPLDATA TYPE TT_BSPL_DATA
                                     PT_RSTHIE   TYPE TT_RSTHIE
                                     PT_RANGE_ACCOUNTS
                               USING VALUE(P_ALTACCT).
* local data declaration
  DATA: LT_BSPLDATA   TYPE TT_BSPL_DATA.
* local structures
  DATA: LS_NODE_SAKNR    TYPE TS_NODE_SAKNR.
  DATA: LS_NODE_ERGSL    TYPE TS_NODE_ERGSL.
  DATA: LS_BSPLDATA      LIKE ZBSPL_TREE_FIELDCAT.
  DATA: LS_BSPLDATA_SAVE LIKE ZBSPL_TREE_FIELDCAT.
* local ranges
  RANGES: LR_RACCT       FOR  SKA1-SAKNR.

* take over accounts which were selcted by log. DB SDF
  LR_RACCT[] = PT_RANGE_ACCOUNTS[].

  SORT PT_BSPLDATA.
* save different fields like CURTP, WAERS ...
  READ TABLE PT_BSPLDATA INDEX 1 INTO LS_BSPLDATA_SAVE.

* PT_RSTHIE contains the complete fin. statmnt structure
  LOOP AT PT_RSTHIE ASSIGNING <RSTHIE>.
    CASE <RSTHIE>-TYPE.
      WHEN CON_TOP
        OR CON_BPOS.
*...... top node or node of fin.statement item
        LS_NODE_ERGSL = <RSTHIE>-NAME.

      WHEN CON_LEAF.
*...... node of account or function area
        LOOP AT PT_BSPLDATA ASSIGNING <BSPLDATA>
                            WHERE ID = <RSTHIE>-ID.
        ENDLOOP.
        LS_NODE_SAKNR = <RSTHIE>-NAME.

        IF SY-SUBRC <> 0.
*........ no entries found;
*........ add account with zero balance to LT_BSPLDATA
          CLEAR: LS_BSPLDATA.
          LS_BSPLDATA-PARENT = <RSTHIE>-PARENT.
          LS_BSPLDATA-ID     = <RSTHIE>-ID.
          LS_BSPLDATA-ERGSL  = LS_NODE_ERGSL-ERGSL.
          LS_BSPLDATA-KTOPL  = CON_STARS.
          LS_BSPLDATA-RACCT  = CON_STARS.
          LS_BSPLDATA-KTOP2  = CON_STARS.
          LS_BSPLDATA-ALTKT  = CON_STARS.
          LS_BSPLDATA-KKTPL  = CON_STARS.
          LS_BSPLDATA-BILKT  = CON_STARS.
          LS_BSPLDATA-RFAREA = CON_STARS.
          LS_BSPLDATA-RBUKRS = CON_STARS.
          LS_BSPLDATA-RBUSA  = CON_STARS.
          LS_BSPLDATA-CURTP  = LS_BSPLDATA_SAVE-CURTP.
          LS_BSPLDATA-WAERS  = LS_BSPLDATA_SAVE-WAERS.

          IF LS_NODE_SAKNR-KTOPL IS INITIAL.
*.......... function area
            LS_BSPLDATA-RFAREA = LS_NODE_SAKNR-SAKNR.
          ELSE.
*.......... gl account
*.......... take only accounts which were selcted by log. DB SDF
            CHECK LS_NODE_SAKNR-SAKNR IN LR_RACCT.

            IF NOT ( P_ALTACCT IS INITIAL ).
*............ alternative account number used
              LS_BSPLDATA-KTOP2  = LS_NODE_SAKNR-KTOPL.
              LS_BSPLDATA-ALTKT  = LS_NODE_SAKNR-SAKNR.
            ELSEIF NOT ( T011-XERGS IS INITIAL ).
*............ group account number used
              LS_BSPLDATA-KKTPL  = LS_NODE_SAKNR-KTOPL.
              LS_BSPLDATA-BILKT  = LS_NODE_SAKNR-SAKNR.
            ELSE.
*............ default normal accounts used
              LS_BSPLDATA-KTOPL  = LS_NODE_SAKNR-KTOPL.
              LS_BSPLDATA-RACCT  = LS_NODE_SAKNR-SAKNR.
            ENDIF.
          ENDIF.
          APPEND LS_BSPLDATA TO LT_BSPLDATA.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  APPEND LINES OF LT_BSPLDATA TO PT_BSPLDATA.
  SORT PT_BSPLDATA.

ENDFORM.                    " BSPL_ZERO_BLNCE_ACCOUNTS_ADD

*&---------------------------------------------------------------------*
*&      Form  BSPL_VARIANCE_CALCULATE
*&---------------------------------------------------------------------*
FORM BSPL_VARIANCE_CALCULATE USING VALUE(P_COMPTYPE)
                                   VALUE(P_REPVAL)
                                   VALUE(P_COMPVAL)
                          CHANGING VALUE(P_ABSVAR)
                                   VALUE(P_RELVAR).
* local data declarations
  DATA: L_COMPVAL    LIKE BSPL_TREE_FIELDCAT-COMPVAL.
  DATA: L_RELVAR(16) TYPE P DECIMALS 1.

* absolute variance calculate
  P_ABSVAR = P_REPVAL - P_COMPVAL.

* relative variance calculate
  IF P_COMPVAL <> 0.     " avoid zero divide
    L_COMPVAL = P_COMPVAL.
    IF L_COMPVAL < 0.
      L_COMPVAL = L_COMPVAL * -1.
    ENDIF.
    IF P_COMPTYPE = '1'.
      L_RELVAR = ( P_REPVAL - P_COMPVAL ) * 100 / L_COMPVAL.
    ELSE.
      L_RELVAR = ( P_REPVAL * 100 ) / L_COMPVAL.
    ENDIF.
    WRITE: L_RELVAR TO P_RELVAR.
  ELSE.
    CLEAR: P_RELVAR.
  ENDIF.
ENDFORM.                    " BSPL_VARIANCE_CALCULATE

*&---------------------------------------------------------------------*
*&      Form  BSPL_ACCOUNT_LENGTH_FIX
*&---------------------------------------------------------------------*
FORM BSPL_ACCOUNT_LENGTH_FIX
                  CHANGING VALUE(PS_NODE) TYPE TS_NODE_SAKNR.
* local data declarations
  STATICS: LS_T004       LIKE T004,
           L_SHIFTLENGTH LIKE SY-INDEX.
  DATA:    L_SAKNR_LEN   TYPE I.

  IF LS_T004-KTOPL <> PS_NODE-KTOPL.
    SELECT SINGLE * FROM T004 INTO LS_T004  "#EC CI_DB_OPERATION_OK[2389136]
                         WHERE KTOPL = PS_NODE-KTOPL.

    IF LS_T004-SAKLN > 0.
      DESCRIBE FIELD PS_NODE-SAKNR LENGTH L_SAKNR_LEN
                     IN CHARACTER MODE.
      L_SHIFTLENGTH = L_SAKNR_LEN - LS_T004-SAKLN.
    ELSE.
      L_SHIFTLENGTH =  0.
    ENDIF.
  ENDIF.

  IF PS_NODE-SAKNR CO '0123456789'.
    WHILE SY-INDEX LE L_SHIFTLENGTH
      AND PS_NODE-SAKNR(1) = '0'.
      SHIFT PS_NODE-SAKNR.
    ENDWHILE.
  ENDIF.
ENDFORM.                    " BSPL_ACCOUNT_LENGTH_FIX

*&---------------------------------------------------------------------*
*&      Form  BSPL_ACCT_ZERO_BLNCE_DELETE
*&---------------------------------------------------------------------*
FORM BSPL_ACCT_ZERO_BLNCE_DELETE
                          TABLES PT_BSPLDATA TYPE TT_BSPL_DATA.
* local data declarations
  RANGES: LR_RACCT FOR SKA1-SAKNR.
  DATA:   L_REPVAL     LIKE BSPL_TREE_FIELDCAT-REPVAL.
  DATA:   L_COMPVAL    LIKE BSPL_TREE_FIELDCAT-COMPVAL.
  DATA:   L_RACCT      LIKE SKA1-SAKNR.
  DATA:   L_XRFAREA_BAL     TYPE C.

  SORT PT_BSPLDATA BY RACCT.
* racct 'CON_RESULT' balance to zero, exclude it
  LOOP AT PT_BSPLDATA WHERE RACCT <> CON_RESULT.

    IF L_RACCT <> PT_BSPLDATA-RACCT
   AND NOT ( L_RACCT IS INITIAL ).

      IF L_REPVAL  = 0
     AND L_COMPVAL = 0
     AND L_XRFAREA_BAL IS INITIAL.
*...... save accounts with zero balances
        LR_RACCT-SIGN    = 'I'.
        LR_RACCT-OPTION  = 'EQ'.
        LR_RACCT-LOW     = L_RACCT.
        APPEND LR_RACCT.
      ENDIF.

      CLEAR: L_REPVAL,
             L_COMPVAL.
      CLEAR: L_XRFAREA_BAL.
    ENDIF.

    L_RACCT   = PT_BSPLDATA-RACCT.
    L_REPVAL  = L_REPVAL  + PT_BSPLDATA-REPVAL.
    L_COMPVAL = L_COMPVAL + PT_BSPLDATA-COMPVAL.
    IF ( NOT PT_BSPLDATA-RFAREA IS INITIAL ) AND
       ( NOT T011-XFBER IS INITIAL ).
      IF PT_BSPLDATA-REPVAL <> 0 OR PT_BSPLDATA-COMPVAL <> 0.
        L_XRFAREA_BAL = 'X'.
      ENDIF.
    ENDIF.

  ENDLOOP.

* handle the last account
  IF L_REPVAL  = 0
 AND L_COMPVAL = 0
 AND L_XRFAREA_BAL IS INITIAL.
    LR_RACCT-SIGN    = 'I'.
    LR_RACCT-OPTION  = 'EQ'.
    LR_RACCT-LOW     = L_RACCT.
    APPEND LR_RACCT.
  ENDIF.

  IF NOT ( LR_RACCT[] IS INITIAL ).
    DELETE PT_BSPLDATA WHERE RACCT IN LR_RACCT.
  ENDIF.

ENDFORM.                    " BSPL_ACCT_ZERO_BLNCE_DELETE

*&---------------------------------------------------------------------*
*&      Form  BSPL_VARIANCE_CALC_TAXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BSPL_VARIANCE_CALC_TAXA  USING    VALUE(P_REPVAL)
                                       VALUE(P_REPVAL2)
                                       VALUE(P_COMPVAL)
                                       VALUE(P_COMPVAL2)
                                       VALUE(P_DESVIO)
                                       VALUE(P_DESVIO2)
                              CHANGING VALUE(P_REPVALV)
                                       VALUE(P_REPVAL2V)
                                       VALUE(P_REPVAL3V).

  DATA: VP_REPVALV(15)  TYPE P DECIMALS 10,
        VP_REPVALV2(15) TYPE P DECIMALS 10,
        VP_REPVALV3(15) TYPE P DECIMALS 10.

  DATA: GVP_REPVALV(15)  TYPE P DECIMALS 4,
        GVP_REPVALV2(15) TYPE P DECIMALS 4,
        GVP_REPVALV3(15) TYPE P DECIMALS 4.

  IF P_REPVAL2 NE 0.
    VP_REPVALV = P_REPVAL / P_REPVAL2.
  ELSE.
    VP_REPVALV = 0.
  ENDIF.

  IF P_DESVIO2 NE 0.
    VP_REPVALV3 = P_DESVIO / P_DESVIO2.
  ELSE.
    VP_REPVALV3 = 0.
  ENDIF.

  IF P_COMPVAL2 NE 0.
    VP_REPVALV2 = P_COMPVAL / P_COMPVAL2.
  ELSE.
    VP_REPVALV2 = 0.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 4
      INPUT         = VP_REPVALV
    IMPORTING
      OUTPUT        = GVP_REPVALV
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 4
      INPUT         = VP_REPVALV2
    IMPORTING
      OUTPUT        = GVP_REPVALV2
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 4
      INPUT         = VP_REPVALV3
    IMPORTING
      OUTPUT        = GVP_REPVALV3
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  WRITE: GVP_REPVALV  TO P_REPVALV,
         GVP_REPVALV2 TO P_REPVAL2V,
         GVP_REPVALV3 TO P_REPVAL3V.

ENDFORM.                    " BSPL_VARIANCE_CALC_TAXA

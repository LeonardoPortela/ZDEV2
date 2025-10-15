*------- Declaration for Application Log ------------------------------
DATA: GT_LOG_HANDLE  TYPE BAL_T_LOGH,
      G_S_LOG_HANDLE TYPE BALLOGHNDL,
      GS_LOG         TYPE BAL_S_LOG.

DATA: TEXT1(120).                  "for Concatenating the messages.

*------- All messages for application log in one Internal table -------
DATA: BEGIN OF GS_MESSAGE,
        MESSAGE(120) TYPE C,
        MSGTY TYPE BAL_S_MSG-MSGTY,
      END OF GS_MESSAGE.

DATA: GT_MESSAGE       LIKE GS_MESSAGE OCCURS 20.
DATA: GS_ACTIVE_STATUS LIKE INACTIVE.

*------- For handling the FormRoutine name for UserCommand ------------
DATA   GC_CALLBACK_FORM    TYPE SLIS_FORMNAME
                                    VALUE 'BAL_CALLBACK_UCOMM'.

*------- Internal Table for ALV List display --------------------------
DATA: GT_OUTTAB1 LIKE STANDARD TABLE OF FAGL_ACC_S_SAPFGVTR_LIST,
      GS_OUTTAB1 LIKE  FAGL_ACC_S_SAPFGVTR_LIST.

CONSTANTS:
      GC_SETPFSTATUS1 TYPE SLIS_ALV_EVENT-FORM VALUE 'SET_PF_STATUS1'.

*---------------------------------------------------------------------*
*       FORM PROTOKOLL                                                *
*---------------------------------------------------------------------*
*       Print protocol
*---------------------------------------------------------------------*
FORM PROTOKOLL.
  DATA: T_H_LIST_GLU1 LIKE S_LIST_GLU1 OCCURS 0 WITH HEADER LINE.

  PERFORM MESSAGES_STOP USING H_MESS_COUNT
                              H_MAX_SEVERITY.

* ------ No accounts found to be processed         ---------------------
  IF XKONT = ' '.
    XAPLSTAT = '2'.
    MESSAGE S620.
  ELSE.

* ------ Give S-message       ------------------------------------------
    IF XCHNG = ' '.
      XAPLSTAT = '2'.
      MESSAGE S621.
    ELSE.
      IF H_MESS_COUNT = 0.             "Keine Nachrichten
        MESSAGE S622 WITH NEWJR.
      ELSE.
        XAPLSTAT = '2'.
        MESSAGE S623.                  "Nachrichten traten auf
      ENDIF.
    ENDIF.
  ENDIF.

*Set PF-Status
  CLEAR INACTIVE. REFRESH INACTIVE.
  IF C_LIST_MODE = '0'
  OR XKONT IS INITIAL.
    INACTIVE-FCODE = 'BILA'.
    APPEND INACTIVE.
    INACTIVE-FCODE = 'PRLO'.
    APPEND INACTIVE.
  ENDIF.
  IF H_MESS_COUNT = 0.
    INACTIVE-FCODE = 'MESS'.
    APPEND INACTIVE.
  ENDIF.

*In online mode print general list. From this list you can go
*to the ALV lists
  IF SY-BATCH IS INITIAL.
*Print general information
    PERFORM PROTOCOL_HEADER.
  ELSE.
*In batch print all messages and ALV lists at once
    IF C_LIST_MODE NE 0.
*If ALV is called the general information is given out in the form
*TOP_OF_LIST
*The P&L accounts list is given out at end_of_list
*The messages are finally also given out at end_of_list
*Set global variables of the actual list status
      G_LIST_MODE = 'BS'.
      G_LIST_BUILD_SUM = ' '.
      G_LIST_LIST_APPEND = ' '.
      PERFORM DISPLAY_LIST TABLES T_LIST_BS_GLU1.
    ELSE.
*If no ALV is called print them directly
      PERFORM DISPLAY_MESSAGES.
    ENDIF.
  ENDIF.
ENDFORM.                    "PROTOKOLL

*---------------------------------------------------------------------*
*       FORM messages_stop                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ms_mess_count                                                 *
*  -->  ms_max_severity                                               *
*---------------------------------------------------------------------*
FORM MESSAGES_STOP USING MS_MESS_COUNT LIKE SY-TABIX
                         MS_MAX_SEVERITY LIKE SY-SUBRC.
  DATA: L_MESS_COUNT LIKE SY-TABIX,
        L_MAX_SEVERITY LIKE SY-SUBRC.

  CLEAR: MS_MESS_COUNT, MS_MAX_SEVERITY.

*Stop collecting messages
  CALL FUNCTION 'MESSAGES_STOP'
    EXCEPTIONS
      I_MESSAGE = 1
      W_MESSAGE = 2
      E_MESSAGE = 3
      A_MESSAGE = 4
      OTHERS    = 8.

*At the moment message handler is not supported
  IF 1 = 2.
    CALL FUNCTION 'MESSAGES_COUNT'
      IMPORTING
        COUNT                       = L_MESS_COUNT
        MAX_SEVERITY                = L_MAX_SEVERITY
      EXCEPTIONS
        INCONSISTENT_RANGE          = 1
        INCONSISTENT_RANGE_SEVERITY = 2
        OTHERS                      = 3.
    ADD L_MESS_COUNT TO MS_MESS_COUNT.
  ENDIF.

*Messages from MITTAB
  DESCRIBE TABLE MITTAB LINES L_MESS_COUNT.
  ADD L_MESS_COUNT TO MS_MESS_COUNT.

*Messages from MESGMOV
  DESCRIBE TABLE MESG_MOV LINES L_MESS_COUNT.
  ADD L_MESS_COUNT TO MS_MESS_COUNT.

ENDFORM.                    "messages_stop
*---------------------------------------------------------------------*
*       FORM display_list                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  t_list_glu1                                                   *
*  -->  dl_mode                                                       *
*---------------------------------------------------------------------*
FORM DISPLAY_LIST TABLES DL_T_LIST_GLU1 TYPE GD13_T_GLU1_CUM.

*>>> Cris - Transferência das Declarações
* para o include TOP
*  DATA: S_LIST_GLU1 TYPE GD13_S_GLU1_CUM.
*  DATA: T_LIST_GLU1 TYPE GD13_T_GLU1_CUM.

* Limpar tabela e estrutura
  CLEAR : T_LIST_GLU1[],  S_LIST_GLU1.
*<<<Cris

  DATA: T_FIELDS TYPE GUSL_T_FIELDS,
        T_SELECTION TYPE GUSL_T_SELECTION.
  DATA: VARIANT(12) TYPE C.
  DATA: T_LISTCAT TYPE  SLIS_T_FIELDCAT_ALV,
        T_SELECTION_FOR_SET TYPE  GUSL_T_SELECTION,
        T_TOTALS_PER TYPE  GUSL_T_GLU1.
  DATA: T_FIELDCAT TYPE  GUSL_T_FIELDS.
  DATA: H_TEXT LIKE  RSMPE-TITTEXT.
  DATA: T_FIELDS_NO_SUM TYPE GUSL_T_FIELDS.

  T_LIST_GLU1[] = DL_T_LIST_GLU1[].
  SORT T_LIST_GLU1 BY RLDNR
                      RRCTY
                      RVERS
                      RYEAR
                      (H_800A-DIM_ORGUNIT)
                      (C_LIST_FIELD1)
                      (C_LIST_FIELD2)
                      (C_LIST_FIELD3)
                      (H_800A-DIM_ACCOUNT)
                      ORG_ACCOUNT
                      RTCUR
                      RUNIT.

*Determine fields to be displayed
  PERFORM GET_LIST_FIELDS TABLES T_FIELDS
                           USING C_LIST_MODE.
*Title
  IF G_LIST_MODE = 'BS'.
    H_TEXT = TEXT-015.                 "Bilanzkonten
  ELSEIF G_LIST_MODE = 'PL'.
    H_TEXT = TEXT-016.                 "Ergebnisvortragskonten
  ELSEIF G_LIST_MODE = 'AL'.
    H_TEXT = TEXT-304.                 "Alles
  ENDIF.

*Set variable G_LIST_SPECIAL_NAME, so that G_TOTALS_GLU1_LIST_DISPLAY
*will handle list_append and build sum for retained earnings account
  G_LIST_SPECIAL_NAME = 'SAPFGVTR  '.
  G_LIST_SPECIAL_NAME+8(1) = G_LIST_BUILD_SUM.
  G_LIST_SPECIAL_NAME+9(1) = G_LIST_LIST_APPEND.

*Fill field names into table T_FIELDS_NO_SUM to suppress display of
*sum for particular fields
  REFRESH T_FIELDS_NO_SUM.
  IF G_LIST_MODE = 'PL'.
    PERFORM APPEND_LIST_FIELD TABLES T_FIELDS_NO_SUM
                              USING  'MSL'.
    PERFORM APPEND_LIST_FIELD TABLES T_FIELDS_NO_SUM
                              USING  'ASL'.
  ENDIF.

*>>>Cris - comentário da Função de ALV
**Call function module to display records
*  CALL FUNCTION 'G_TOTALS_GLU1_LIST_DISPLAY'
*    EXPORTING
*      I_LEDGER              = H_881-RLDNR
*      I_SELECTION           = T_SELECTION
*      I_DRILL_DOWN          = SPACE
*      I_LEDGERFIELD         = T_FIELDS
*      I_VARIANT             = VARIANT
*      I_TEXT                = H_TEXT
*      I_PROMPT              = SPACE
*      I_CWCODE              = 'X'
*      I_BUFFER_ACTIVE       = SPACE
*      I_TOP_LIST_ROUTINE    = 'TOP_OF_LIST'
*      I_END_LIST_ROUTINE    = 'END_OF_LIST'
*      I_LIST_PROGNAME       = 'SAPFGVTR'
*      I_SPECIAL_NAME        = G_LIST_SPECIAL_NAME
*      I_ACCUMULATE          = 'X'
*    IMPORTING
*      E_T_LISTCAT           = T_LISTCAT
*      E_T_SELECTION_FOR_SET = T_SELECTION_FOR_SET
*      E_T_TOTALS_PER        = T_TOTALS_PER
*      E_T_FIELDCAT          = T_FIELDCAT
*    TABLES
*      E_T_TOTALS_ACC        = T_LIST_GLU1
*      I_T_NO_SUM            = T_FIELDS_NO_SUM
*    EXCEPTIONS
*      INTERNAL_ERROR        = 1
*      OTHERS                = 2.

* Novo Relatório
  PERFORM: F_NOVO_ALV.

*<<<Cris
ENDFORM.                    "display_list

*---------------------------------------------------------------------*
*       FORM get_list_fields                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  glf_t_fields                                                  *
*  -->  glf_list_mode                                                 *
*---------------------------------------------------------------------*
FORM GET_LIST_FIELDS TABLES GLF_T_FIELDS  TYPE GUSL_T_FIELDS
                           USING GLF_LIST_MODE.
  DATA: T_FIELDTAB LIKE DFIES OCCURS 0 WITH HEADER LINE.

*Display only required fields
  IF GLF_LIST_MODE = '1'.
    PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                              USING 'RLDNR'.
    IF P_APPL NE 'FIGL' AND P_APPL NE APPL-FIGLF.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING 'RRCTY'.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING 'RVERS'.
    ENDIF.
    PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                              USING 'RYEAR'.
    PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                              USING H_800A-DIM_ORGUNIT.
    IF NOT C_LIST_FIELD1 IS INITIAL.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING C_LIST_FIELD1.
    ENDIF.
    IF NOT C_LIST_FIELD2 IS INITIAL.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING C_LIST_FIELD2.
    ENDIF.
    IF NOT C_LIST_FIELD3 IS INITIAL.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING C_LIST_FIELD3.
    ENDIF.
    PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                              USING H_800A-DIM_ACCOUNT.
    IF NOT G_LIST_BUILD_SUM IS INITIAL.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING 'ORG_ACCOUNT'.
    ENDIF.
    IF NOT H_800A-TRCUR IS INITIAL.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING 'RTCUR'.
    ENDIF.
    IF NOT H_800A-QUANT IS INITIAL.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING 'RUNIT'.
    ENDIF.
*    IF p_appl = appl-figlf.
*      PERFORM APPEND_LIST_FIELDS_GLF TABLES GLF_T_FIELDS
*                                USING T881-RLDNR.
*    ENDIF.
    LOOP AT T_T881_KEYFIG.
      PERFORM APPEND_LIST_FIELD TABLES GLF_T_FIELDS
                                USING T_T881_KEYFIG-KEYFIG.
    ENDLOOP.
  ELSE.
*Get all the fields to be displayed
    CALL FUNCTION 'G_FIELDTAB_GET'
      EXPORTING
        TABLE      = H_800A-NTABLE
      TABLES
        T_FIELDTAB = T_FIELDTAB
      EXCEPTIONS
        NOT_FOUND  = 1
        OTHERS     = 2.
    CHECK SY-SUBRC = 0.

    LOOP AT T_FIELDTAB.
      CHECK T_FIELDTAB-FIELDNAME NE 'DOCCT'
        AND T_FIELDTAB-FIELDNAME NE 'DOCNR'
        AND T_FIELDTAB-FIELDNAME NE 'DOCLN'.
      APPEND T_FIELDTAB-FIELDNAME TO GLF_T_FIELDS.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "get_list_fields

*---------------------------------------------------------------------*
*       FORM append_list_field                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  alf_t_fields                                                  *
*  -->  alf_field                                                     *
*---------------------------------------------------------------------*
FORM APPEND_LIST_FIELD TABLES ALF_T_FIELDS TYPE GUSL_T_FIELDS
                       USING ALF_FIELD.
  APPEND ALF_FIELD TO ALF_T_FIELDS.

ENDFORM.                    "append_list_field

*---------------------------------------------------------------------*
*       FORM glu1_to_list_table                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  gtlt_glu1                                                     *
*  -->  rep_xbilk                                                     *
*---------------------------------------------------------------------*
FORM GLU1_TO_LIST_TABLE USING REP_XBILK.

*Find all record types and Versions that have been processed
  T_LIST_RRCTY-RRCTY = GLU1-RRCTY.
  COLLECT T_LIST_RRCTY.
  T_LIST_RVERS-RVERS = GLU1-RVERS.
  COLLECT T_LIST_RVERS.
*Only further processing if list has to be displayed
  CHECK C_LIST_MODE NE '0'.
  CLEAR S_LIST_GLU1.
  IF C_LIST_MODE = '1'.
    S_LIST_GLU1-RLDNR = GLU1-RLDNR.
    S_LIST_GLU1-RRCTY = GLU1-RRCTY.
    S_LIST_GLU1-RVERS = GLU1-RVERS.
    S_LIST_GLU1-RYEAR = GLU1-RYEAR.
    S_LIST_GLU1-RTCUR = GLU1-RTCUR.
    S_LIST_GLU1-RUNIT = GLU1-RUNIT.
    S_LIST_GLU1-POPER = GLU1-POPER.
    <S_LIST_GLU1_ORGUNIT> = <GLU1_ORGUNIT>.
    <S_LIST_GLU1_ACCOUNT> = <GLU1_ACCOUNT>.
    IF REP_XBILK IS INITIAL AND XTRAC NE SPACE.
      S_LIST_GLU1-ORG_ACCOUNT  = <*GLU1_ACCOUNT>. "Store original acct
    ENDIF.
    IF NOT C_LIST_FIELD1 IS INITIAL.
      <S_LIST_GLU1_FIELD1> = <GLU1_FIELD1>.
    ENDIF.
    IF NOT C_LIST_FIELD2 IS INITIAL.
      <S_LIST_GLU1_FIELD2> = <GLU1_FIELD2>.
    ENDIF.
    IF NOT C_LIST_FIELD3 IS INITIAL.
      <S_LIST_GLU1_FIELD3> = <GLU1_FIELD3>.
    ENDIF.
    IF NOT <GLU1_KEYFIG01> IS INITIAL.
      <S_LIST_GLU1_KEYFIG01> = <GLU1_KEYFIG01>.
    ENDIF.
    IF NOT <GLU1_KEYFIG02> IS INITIAL.
      <S_LIST_GLU1_KEYFIG02> = <GLU1_KEYFIG02>.
    ENDIF.
    IF NOT <GLU1_KEYFIG03> IS INITIAL.
      <S_LIST_GLU1_KEYFIG03> = <GLU1_KEYFIG03>.
    ENDIF.
    IF NOT <GLU1_KEYFIG04> IS INITIAL.
      <S_LIST_GLU1_KEYFIG04> = <GLU1_KEYFIG04>.
    ENDIF.
    IF NOT <GLU1_KEYFIG05> IS INITIAL.
      <S_LIST_GLU1_KEYFIG05> = <GLU1_KEYFIG05>.
    ENDIF.
    IF NOT <GLU1_KEYFIG06> IS INITIAL.
      <S_LIST_GLU1_KEYFIG06> = <GLU1_KEYFIG06>.
    ENDIF.
    IF NOT <GLU1_KEYFIG07> IS INITIAL.
      <S_LIST_GLU1_KEYFIG07> = <GLU1_KEYFIG07>.
    ENDIF.
    IF NOT <GLU1_KEYFIG08> IS INITIAL.
      <S_LIST_GLU1_KEYFIG08> = <GLU1_KEYFIG08>.
    ENDIF.
    IF NOT <GLU1_KEYFIG09> IS INITIAL.
      <S_LIST_GLU1_KEYFIG09> = <GLU1_KEYFIG09>.
    ENDIF.
    IF NOT <GLU1_KEYFIG10> IS INITIAL.
      <S_LIST_GLU1_KEYFIG10> = <GLU1_KEYFIG10>.
    ENDIF.
  ELSEIF C_LIST_MODE = '2'.
    S_LIST_GLU1 = GLU1.
    IF REP_XBILK IS INITIAL AND XTRAC NE SPACE.
      S_LIST_GLU1-ORG_ACCOUNT  = <*GLU1_ACCOUNT>. "Store original acct
    ENDIF.
  ENDIF.
*Currencies
  IF NOT BUKGESTAB-LCCUR IS INITIAL.
    S_LIST_GLU1-LCURR = BUKGESTAB-HWAER.
  ENDIF.
  IF NOT BUKGESTAB-RCCUR IS INITIAL.
    S_LIST_GLU1-GCURR = BUKGESTAB-KWAER.
  ENDIF.
  IF NOT BUKGESTAB-OCCUR IS INITIAL.
    S_LIST_GLU1-OCURR = BUKGESTAB-OWAER.
  ENDIF.
*Store record in list tables
  IF REP_XBILK IS INITIAL.
    COLLECT S_LIST_GLU1 INTO T_LIST_PL_GLU1.
  ELSE.
    COLLECT S_LIST_GLU1 INTO T_LIST_BS_GLU1.
  ENDIF.
ENDFORM.                    "glu1_to_list_table
*---------------------------------------------------------------------*
*       FORM display_messages                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_MESSAGES.
  DATA: STRING(132).

*------- Data Declarations for list display of message.----------------
  DATA  FT_TAB      TYPE STANDARD TABLE OF FAGL_ACC_S_SAPFGVTR_LIST.
  DATA  FS_TAB      TYPE FAGL_ACC_S_SAPFGVTR_LIST.
  DATA: LS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
        LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        LT_EVENTS   TYPE SLIS_T_EVENT.
  DATA: G_REPID     TYPE SY-REPID.

  INACTIVE-FCODE = 'BILA'.
  APPEND INACTIVE.
  INACTIVE-FCODE = 'PRLO'.
  APPEND INACTIVE.
  INACTIVE-FCODE = 'MESS'.
  APPEND INACTIVE.

* ------ (MITTAB) ---------------------------------------------
  LOOP AT MITTAB.
    MOVE: MITTAB-BUKGES TO FS_TAB-RCOMP,
          MITTAB-RACCT  TO FS_TAB-RACCT,
          MITTAB-KTOPL  TO FS_TAB-KTOPL,
          MITTAB-GVTYP  TO FS_TAB-GVTYP.
    CONCATENATE MITTAB-TEXT1 MITTAB-TEXT2 INTO FS_TAB-TEXT.
    APPEND FS_TAB TO FT_TAB.
  ENDLOOP.
* ------ (MESG_MOV) ---------------------------------------------
  LOOP AT MESG_MOV.
    MOVE: MESG_MOV-BUKGES TO FS_TAB-RCOMP,
          MESG_MOV-RACCT  TO FS_TAB-RACCT,
          MESG_MOV-TEXT   TO FS_TAB-TEXT.
    APPEND FS_TAB TO FT_TAB.
  ENDLOOP.

  READ TABLE FT_TAB INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC = 0.
* build the field catalog.
    PERFORM SET_ALV_FIELDCAT2 CHANGING LT_FIELDCAT.
* build the event table.
    PERFORM SET_ALV_EVENTS2 CHANGING LT_EVENTS.
* output the table.
    MOVE SY-REPID TO G_REPID.
    IF NOT SY-BATCH IS INITIAL.
      LS_LAYOUT-LIST_APPEND = 'X'.
    ENDIF.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM       = G_REPID
        I_CALLBACK_PF_STATUS_SET = GC_SETPFSTATUS1
        I_CALLBACK_USER_COMMAND  = GC_CALLBACK_FORM
        IT_FIELDCAT              = LT_FIELDCAT
        IT_EVENTS                = LT_EVENTS
        IS_LAYOUT                = LS_LAYOUT
      TABLES
        T_OUTTAB                 = FT_TAB
      EXCEPTIONS
        PROGRAM_ERROR            = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    PERFORM PROTOCOL_HEADER.
  ENDIF.

* ------         Errorsfrom field movements (User-Exits) -------------
* -> see in FORM end_of_list2
ENDFORM.                    "display_messages

*&---------------------------------------------------------------------*
*&      Form  protocol_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROTOCOL_HEADER.

  DATA: LF_COUNT_ORGUNIT   TYPE I,
        LF_COUNT_MAX       TYPE I  VALUE 10.

  REFRESH GT_MESSAGE.

*Protocol for balance carry forward
  MOVE TEXT-003 TO GS_MESSAGE-MESSAGE.
  MOVE 'I'      TO GS_MESSAGE-MSGTY.
  APPEND GS_MESSAGE TO GT_MESSAGE.

*Information: Test/Productive
  MOVE TEXT-307 TO GS_MESSAGE-MESSAGE.
  IF XTEST NE SPACE.
    GS_MESSAGE-MESSAGE+20 = TEXT-001.
  ELSE.
    GS_MESSAGE-MESSAGE+25(10) = TEXT-002.
  ENDIF.
  MOVE 'I' TO GS_MESSAGE-MSGTY.
  APPEND GS_MESSAGE TO GT_MESSAGE.

*Year
  TEXT1 = TEXT-312.
  TEXT1+25(4) = NEWJR.
  MOVE TEXT1 TO GS_MESSAGE-MESSAGE.
  MOVE 'I'   TO GS_MESSAGE-MSGTY.
  APPEND GS_MESSAGE TO GT_MESSAGE.
  CLEAR TEXT1.

*Ledger
  MOVE TEXT-306 TO TEXT1.
  TEXT1+27 = LEDGER.
  MOVE TEXT1 TO GS_MESSAGE-MESSAGE.
  MOVE 'I'   TO GS_MESSAGE-MSGTY.
  APPEND GS_MESSAGE TO GT_MESSAGE.
  CLEAR TEXT1.

*Record types
  IF P_APPL NE 'FIGL'.
    IF NOT T_LIST_RRCTY[] IS INITIAL.
      H_FIRST_TIME = 'X'.
      MOVE TEXT-308 TO TEXT1.
      LOOP AT T_LIST_RRCTY.
        IF H_FIRST_TIME = 'X'.
          CLEAR H_FIRST_TIME.
          TEXT1+25(1) = T_LIST_RRCTY-RRCTY.
        ELSE.
          CONCATENATE TEXT1 T_LIST_RRCTY-RRCTY INTO TEXT1
                                               SEPARATED BY ', '.
        ENDIF.
      ENDLOOP.
      MOVE TEXT1 TO GS_MESSAGE-MESSAGE.
      MOVE 'I'   TO GS_MESSAGE-MSGTY.
      APPEND GS_MESSAGE TO GT_MESSAGE.
      CLEAR TEXT1.
    ENDIF.
  ENDIF.
*Version
  IF P_APPL NE 'FIGL'.
    IF NOT T_LIST_RVERS[] IS INITIAL.
      H_FIRST_TIME = 'X'.
      TEXT1 = TEXT-309.
      LOOP AT T_LIST_RVERS.
        IF H_FIRST_TIME = 'X'.
          CLEAR H_FIRST_TIME.
          TEXT1+26 = T_LIST_RVERS-RVERS.
        ELSE.
          CONCATENATE TEXT1 T_LIST_RVERS-RVERS INTO TEXT1
                                               SEPARATED BY ', '.
        ENDIF.
      ENDLOOP.
      MOVE TEXT1 TO GS_MESSAGE-MESSAGE.
      MOVE 'I'   TO GS_MESSAGE-MSGTY.
      APPEND GS_MESSAGE TO GT_MESSAGE.
      CLEAR TEXT1.
    ENDIF.
  ENDIF.
*Orgunit
  IF NOT BUKGESTAB[] IS INITIAL.
    H_FIRST_TIME = 'X'.
    CLEAR LF_COUNT_ORGUNIT.
    IF NOT SY-BATCH IS INITIAL. LF_COUNT_MAX = 5. ENDIF.
    LOOP AT BUKGESTAB.
      IF H_FIRST_TIME = 'X'.
        IF H_800A-DIM_ORGUNIT = 'RCOMP'.
          TEXT1 = TEXT-311.
        ELSE.
          TEXT1 = TEXT-310.
        ENDIF.
        CLEAR H_FIRST_TIME.
        TEXT1+17 = BUKGESTAB-BUKGES.
      ELSE.
        IF NOT SY-BATCH IS INITIAL AND SY-TABIX > 200.
          CONCATENATE TEXT1 ',' INTO TEXT1.
          CONCATENATE TEXT1 '(...)' INTO TEXT1.
          EXIT.
        ELSE.
          CONCATENATE TEXT1 BUKGESTAB-BUKGES INTO TEXT1
                                             SEPARATED BY ', '.
        ENDIF.
      ENDIF.
      ADD 1 TO LF_COUNT_ORGUNIT.
* Create new line in log after max. company codes/companies
      IF LF_COUNT_ORGUNIT >= LF_COUNT_MAX.
        MOVE TEXT1 TO GS_MESSAGE-MESSAGE.
        MOVE 'I'   TO GS_MESSAGE-MSGTY.
        APPEND GS_MESSAGE TO GT_MESSAGE.
        CLEAR TEXT1.
        CLEAR LF_COUNT_ORGUNIT.
        H_FIRST_TIME = 'X'.
      ENDIF.
    ENDLOOP.
    IF NOT TEXT1 IS INITIAL.
      MOVE TEXT1 TO GS_MESSAGE-MESSAGE.
      MOVE 'I'   TO GS_MESSAGE-MSGTY.
      APPEND GS_MESSAGE TO GT_MESSAGE.
      CLEAR TEXT1.
    ENDIF.
  ENDIF.

* ------ No accounts to be processed? ----------------------
  IF XKONT = ' '.                      "Keine Konten selektiert
    MOVE TEXT-316 TO GS_MESSAGE-MESSAGE.
    MOVE 'I'      TO GS_MESSAGE-MSGTY.
    APPEND GS_MESSAGE TO GT_MESSAGE.
  ELSE.
    IF XCHNG = ' '.                    "Keine Änderungen
      MOVE TEXT-317 TO GS_MESSAGE-MESSAGE.
      MOVE 'I'      TO GS_MESSAGE-MSGTY.
      APPEND GS_MESSAGE TO GT_MESSAGE.
    ELSE.
      IF H_MESS_COUNT = 0.             "Keine Nachrichten
        MOVE TEXT-314 TO GS_MESSAGE-MESSAGE.
        MOVE 'S'      TO GS_MESSAGE-MSGTY.
        APPEND GS_MESSAGE TO GT_MESSAGE.
      ELSE.                            "Nachrichten traten auf
        IF SY-BATCH IS INITIAL.
          MOVE TEXT-315 TO GS_MESSAGE-MESSAGE.
          MOVE 'I'      TO GS_MESSAGE-MSGTY.
          APPEND GS_MESSAGE TO GT_MESSAGE.
        ELSE.
          MOVE TEXT-322 TO GS_MESSAGE-MESSAGE.
          MOVE 'I'      TO GS_MESSAGE-MSGTY.
          APPEND GS_MESSAGE TO GT_MESSAGE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*For FI-GL: Carry forward also the other parallel currency ledgers
  IF G_V_PARALLEL_LEDGERS NE SPACE.
    MOVE TEXT-325 TO GS_MESSAGE-MESSAGE.
    MOVE 'I'      TO GS_MESSAGE-MSGTY.
    APPEND GS_MESSAGE TO GT_MESSAGE.
  ENDIF.

*Text line that created records can be displayed via FCODE
  IF C_LIST_MODE NE '0'
  AND XKONT NE SPACE
  AND SY-BATCH IS INITIAL.
    MOVE TEXT-321 TO GS_MESSAGE-MESSAGE.
    MOVE 'I'      TO GS_MESSAGE-MSGTY.
    APPEND GS_MESSAGE TO GT_MESSAGE.
  ENDIF.

  PERFORM LOG_CREATE_ALV.
  PERFORM LOG_SHOW_ALV.
ENDFORM.                               " protocol_header

*---------------------------------------------------------------------*
*       FORM top_of_list                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_LIST TABLES T_LIST_TAB.
  DATA: LT_LIST TYPE SLIS_T_LISTHEADER,
        LS_LINE TYPE SLIS_LISTHEADER.
*Print whole list only in batch
  CHECK SY-BATCH NE SPACE.
  CLEAR LS_LINE.
  REFRESH LT_LIST.
*In batch a title must be printed for the ALV lists
  IF NOT C_LIST_MODE = '0'.
    IF G_LIST_MODE = 'BS'.
      LS_LINE-TYP  = 'A'.
      CONCATENATE TEXT-305 TEXT-323 INTO LS_LINE-INFO
                                    SEPARATED BY SPACE.
      APPEND LS_LINE TO LT_LIST.
    ELSEIF G_LIST_MODE = 'PL'.
      LS_LINE-TYP  = 'A'.
      CONCATENATE TEXT-305 TEXT-324 INTO LS_LINE-INFO
                                    SEPARATED BY SPACE.
      APPEND LS_LINE TO LT_LIST.
    ENDIF.
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        IT_LIST_COMMENTARY = LT_LIST.
  ENDIF.
ENDFORM.                    "top_of_list

*---------------------------------------------------------------------*
*       FORM end_of_list                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM END_OF_LIST TABLES T_LIST_TAB.
*Print whole list only in batch
  CHECK SY-BATCH NE SPACE.
  IF C_LIST_MODE NE 0.
*If actual status is 'BS accounts' then display PL accounts
    IF G_LIST_MODE = 'BS'.
      G_LIST_MODE = 'PL'.
      IF XTRAC NE SPACE.
        G_LIST_BUILD_SUM = 'X'.
      ELSE.
        G_LIST_BUILD_SUM = ' '.
      ENDIF.
      G_LIST_LIST_APPEND = 'X'.
      PERFORM DISPLAY_LIST TABLES T_LIST_PL_GLU1.
*If actual status is 'PL accounts' then display messages
    ELSEIF G_LIST_MODE = 'PL'.
      PERFORM DISPLAY_MESSAGES.
    ENDIF.
*No list: print messages directly
  ELSE.
    PERFORM DISPLAY_MESSAGES.
  ENDIF.
ENDFORM.                    "end_of_list

*&--------------------------------------------------------------------*
*&      Form  log_create_alv
*&--------------------------------------------------------------------*
FORM LOG_CREATE_ALV .
  GS_LOG-EXTNUMBER = SY-TITLE.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG                 = GS_LOG
    IMPORTING
      E_LOG_HANDLE            = G_S_LOG_HANDLE
    EXCEPTIONS
      LOG_HEADER_INCONSISTENT = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
    EXPORTING
      I_LOG_HANDLE            = G_S_LOG_HANDLE
      I_S_LOG                 = GS_LOG
    EXCEPTIONS
      LOG_NOT_FOUND           = 1
      LOG_HEADER_INCONSISTENT = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT GT_MESSAGE INTO GS_MESSAGE.
    PERFORM ADD_MESSAGE_ALV USING GS_MESSAGE-MESSAGE GS_MESSAGE-MSGTY.
  ENDLOOP.
ENDFORM.                    " log_create_alv

*&--------------------------------------------------------------------*
*&      Form  log_show_alv
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM LOG_SHOW_ALV .

  DATA: L_S_DISPLAY_PROFILE TYPE BAL_S_PROF.
  DATA: L_S_PUSH            TYPE BAL_S_PUSH.
  CONSTANTS: LC_ACTIVE     VALUE 'X',
             LC_BUTTON3(8) VALUE 'Messages'.                "#EC NOTEXT

* to Get Profile.
*  CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
*    IMPORTING
*      e_s_display_profile = l_s_display_profile.
  CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
    IMPORTING
      E_S_DISPLAY_PROFILE = L_S_DISPLAY_PROFILE.

* Set title
  L_S_DISPLAY_PROFILE-TITLE = SY-TITLE.

* use grid for display if wanted
  L_S_DISPLAY_PROFILE-USE_GRID = LC_ACTIVE .
  L_S_DISPLAY_PROFILE-NO_TOOLBAR        = LC_ACTIVE .

* display the messages immediately on execution
  L_S_DISPLAY_PROFILE-SHOW_ALL = LC_ACTIVE.

** ADDING BUTTONS TO APPLICATION LOG.
  L_S_DISPLAY_PROFILE-EXT_PUSH1-ACTIVE = LC_ACTIVE .
  L_S_DISPLAY_PROFILE-EXT_PUSH1-POSITION = SPACE.
  L_S_DISPLAY_PROFILE-EXT_PUSH1-DEF-TEXT = TEXT-323.
  L_S_DISPLAY_PROFILE-EXT_PUSH1-DEF-ICON_TEXT = TEXT-323.

  L_S_DISPLAY_PROFILE-EXT_PUSH2-ACTIVE = LC_ACTIVE .
  L_S_DISPLAY_PROFILE-EXT_PUSH2-POSITION = SPACE.
  L_S_DISPLAY_PROFILE-EXT_PUSH2-DEF-TEXT = TEXT-324.
  L_S_DISPLAY_PROFILE-EXT_PUSH2-DEF-ICON_TEXT = TEXT-324.

  L_S_DISPLAY_PROFILE-EXT_PUSH3-ACTIVE = LC_ACTIVE .
  L_S_DISPLAY_PROFILE-EXT_PUSH3-POSITION = SPACE.
  L_S_DISPLAY_PROFILE-EXT_PUSH3-DEF-TEXT = LC_BUTTON3.
  L_S_DISPLAY_PROFILE-EXT_PUSH3-DEF-ICON_TEXT = LC_BUTTON3.

  LOOP AT INACTIVE.
    CASE INACTIVE-FCODE.
      WHEN 'MESS'.
        L_S_DISPLAY_PROFILE-EXT_PUSH3-ACTIVE = SPACE.
      WHEN 'BILA'.
        L_S_DISPLAY_PROFILE-EXT_PUSH1-ACTIVE = SPACE.
      WHEN 'PRLO'.
        L_S_DISPLAY_PROFILE-EXT_PUSH2-ACTIVE = SPACE.
    ENDCASE.
  ENDLOOP.

* Call Back Routine For Push Button.
  L_S_DISPLAY_PROFILE-CLBK_UCOM-USEREXITT     =  SPACE.
  L_S_DISPLAY_PROFILE-CLBK_UCOM-USEREXITP     =  SY-REPID.
  L_S_DISPLAY_PROFILE-CLBK_UCOM-USEREXITF     =  GC_CALLBACK_FORM .

  IF SY-BATCH IS INITIAL.
* call display function module
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        I_S_DISPLAY_PROFILE = L_S_DISPLAY_PROFILE
        I_T_LOG_HANDLE      = GT_LOG_HANDLE
      EXCEPTIONS
        OTHERS              = 1.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAL_DSP_LOG_PRINT'
      EXPORTING
        I_S_DISPLAY_PROFILE  = L_S_DISPLAY_PROFILE
        I_T_LOG_HANDLE       = GT_LOG_HANDLE
        I_S_LIST_APPEND      = 'X'
      EXCEPTIONS
        PROFILE_INCONSISTENT = 1
        INTERNAL_ERROR       = 2
        NO_DATA_AVAILABLE    = 3
        NO_AUTHORITY         = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    "log_show_alv

*&---------------------------------------------------------------------
*&      Form  add_message_alv
*&---------------------------------------------------------------------
FORM ADD_MESSAGE_ALV  USING    IT_MESSAGE IT_MSG.
  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      I_LOG_HANDLE     = G_S_LOG_HANDLE
      I_MSGTY          = IT_MSG
      I_TEXT           = IT_MESSAGE
    EXCEPTIONS
      LOG_NOT_FOUND    = 1
      MSG_INCONSISTENT = 2
      LOG_IS_FULL      = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " add_message_alv

*&--------------------------------------------------------------------*
*&      Form  bal_callback_ucomm
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->C_S_USER_COtextD_DATA
*---------------------------------------------------------------------*
FORM BAL_CALLBACK_UCOMM                                     "#EC CALLED
                        CHANGING
                          C_S_USER_COMMAND_DATA TYPE BAL_S_CBUC.
  CASE C_S_USER_COMMAND_DATA-UCOMM.
    WHEN '%EXT_PUSH1' OR 'BILA'.
      G_LIST_MODE = 'BS'.
      G_LIST_BUILD_SUM = ' '.
      G_LIST_LIST_APPEND = ' '.
      PERFORM DISPLAY_LIST TABLES T_LIST_BS_GLU1.
    WHEN '%EXT_PUSH2' OR 'PRLO'.
      G_LIST_MODE = 'PL'.
      IF XTRAC IS INITIAL.
        G_LIST_BUILD_SUM = ' '.
      ELSE.
        G_LIST_BUILD_SUM = 'X'.
      ENDIF.
      G_LIST_LIST_APPEND = ' '.
      PERFORM DISPLAY_LIST TABLES T_LIST_PL_GLU1.
    WHEN '%EXT_PUSH3' OR 'MESS'.
      PERFORM DISPLAY_MESSAGES.
  ENDCASE.
ENDFORM.                    "bal_callback_ucomm


** Subroutine for Field catalog Build
FORM SET_ALV_FIELDCAT2  CHANGING XT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  CONSTANTS:
    LC_STRNAME_LIST LIKE DD02D-STRNAME VALUE 'FAGL_ACC_S_SAPFGVTR_LIST'.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CONSTANTS: LC_COLTEXT TYPE C VALUE 'M'.

* get the field catalog from the structure
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = LC_STRNAME_LIST
    CHANGING
      CT_FIELDCAT            = XT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

** Modify the Column Heading of the display as per List Display.
  READ TABLE XT_FIELDCAT INTO LS_FIELDCAT
                         WITH KEY FIELDNAME = 'RCOMP'.
  IF SY-SUBRC = 0.
    IF H_800A-COMPTAB IS INITIAL.
      LS_FIELDCAT-SELTEXT_M = TEXT-036 .         "Company code
    ELSE.
      LS_FIELDCAT-SELTEXT_M = TEXT-037.          "Company
    ENDIF.
    LS_FIELDCAT-DDICTXT = LC_COLTEXT.
    MODIFY XT_FIELDCAT FROM LS_FIELDCAT
      TRANSPORTING SELTEXT_M DDICTXT WHERE FIELDNAME = 'RCOMP'.
  ENDIF.

  READ TABLE XT_FIELDCAT INTO LS_FIELDCAT
                         WITH KEY FIELDNAME = 'GVTYP'.
  IF SY-SUBRC = 0.
    LS_FIELDCAT-SELTEXT_M = TEXT-040 .
    LS_FIELDCAT-DDICTXT = LC_COLTEXT.
    MODIFY XT_FIELDCAT FROM LS_FIELDCAT
     TRANSPORTING SELTEXT_M DDICTXT WHERE FIELDNAME = 'GVTYP'.
  ENDIF.

  READ TABLE XT_FIELDCAT INTO LS_FIELDCAT
                         WITH KEY FIELDNAME = 'TEXT'.
  IF SY-SUBRC = 0.
    LS_FIELDCAT-SELTEXT_M = TEXT-041.
    LS_FIELDCAT-DDICTXT = LC_COLTEXT.
    MODIFY XT_FIELDCAT FROM LS_FIELDCAT
     TRANSPORTING SELTEXT_M DDICTXT WHERE FIELDNAME = 'TEXT'.
  ENDIF.
ENDFORM.                    "set_alv_fieldcat2

** Subroutine for EVENTS
FORM SET_ALV_EVENTS2  CHANGING XT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENTS TYPE SLIS_ALV_EVENT.
  CONSTANTS:
        LC_ENDOFLIST TYPE SLIS_ALV_EVENT-FORM VALUE 'END_OF_LIST2',
        LC_TOPOFLIST TYPE SLIS_ALV_EVENT-FORM VALUE 'TOP_OF_LIST1'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE     = 0
    IMPORTING
      ET_EVENTS       = XT_EVENTS
    EXCEPTIONS
      LIST_TYPE_WRONG = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*Pass the subroutine name to the END-OF-LIST event when the end of list
*is triggered
  READ TABLE XT_EVENTS INTO LS_EVENTS WITH
       KEY NAME = SLIS_EV_END_OF_LIST.
  IF SY-SUBRC = 0.
    LS_EVENTS-FORM = LC_ENDOFLIST.
    MODIFY XT_EVENTS FROM LS_EVENTS TRANSPORTING FORM
           WHERE NAME = SLIS_EV_END_OF_LIST.
  ENDIF.

*Pass the subroutine name to the TOP-OF-LIST event when the top of list
*is triggered
  READ TABLE XT_EVENTS INTO LS_EVENTS WITH
     KEY NAME = SLIS_EV_TOP_OF_LIST.
  IF SY-SUBRC = 0.
    LS_EVENTS-FORM = LC_TOPOFLIST.
    MODIFY XT_EVENTS FROM LS_EVENTS TRANSPORTING FORM
           WHERE NAME = SLIS_EV_TOP_OF_LIST.
  ENDIF.

*pass formRoutine name for SET PF-status
  READ TABLE XT_EVENTS INTO LS_EVENTS WITH
     KEY NAME = SLIS_EV_PF_STATUS_SET.
  IF SY-SUBRC = 0.
    LS_EVENTS-FORM =  GC_SETPFSTATUS1.
    MODIFY XT_EVENTS FROM LS_EVENTS TRANSPORTING FORM
           WHERE NAME = SLIS_EV_PF_STATUS_SET.
  ENDIF.
ENDFORM.                    "set_alv_events2


** Subroutine for TOP_OF_LIST.
FORM TOP_OF_LIST1.                                          "#EC CALLED
  DATA: LT_LIST TYPE SLIS_T_LISTHEADER,
        LS_LINE TYPE SLIS_LISTHEADER.
  CONSTANTS: LC_TYPE_H VALUE 'H'.

  CLEAR LS_LINE.
  LS_LINE-TYP  = LC_TYPE_H.
  LS_LINE-INFO = TEXT-005.
  APPEND LS_LINE TO LT_LIST.

** function call for Top Of List OUTPUT
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_LIST.

ENDFORM.                    "TOP_of_list1


** Subroutine for END_OF_LIST2.
FORM END_OF_LIST2.                                          "#EC CALLED

  IF NOT SY-BATCH IS INITIAL.
    PERFORM PROTOCOL_HEADER.
  ENDIF.

ENDFORM.                    " event_end_of_list


** form Routine for SET PF-STATUS.
FORM SET_PF_STATUS1  USING RT_EXTAB TYPE SLIS_T_EXTAB .     "#EC CALLED

  SET PF-STATUS 'STLI' EXCLUDING INACTIVE.

ENDFORM.                    "set_pf_status1

*&---------------------------------------------------------------------*
*&      Form  APPEND_LIST_FIELDS_GLF
*&---------------------------------------------------------------------*
*       Get additional NewGL fields
*----------------------------------------------------------------------*
FORM APPEND_LIST_FIELDS_GLF  TABLES ALF_T_FIELDS TYPE GUSL_T_FIELDS
                             USING  ALF_RLDNR    LIKE T881-RLDNR.
  DATA:  ALF_LEDGERFIELDS TYPE GUSL_T_FIELDS,
         ALF_FIELDNAME    LIKE DFIES-FIELDNAME.

  CALL FUNCTION 'FAGL_GET_ACTIVE_TOT_FIELDS'
    EXPORTING
      I_RLDNR         = ALF_RLDNR
*     I_CLIENT        = SY-MANDT
    IMPORTING
      ET_FIELDS       = ALF_LEDGERFIELDS.

  LOOP AT ALF_LEDGERFIELDS INTO ALF_FIELDNAME.
    COLLECT ALF_FIELDNAME INTO ALF_T_FIELDS.
  ENDLOOP.

ENDFORM.                    " APPEND_LIST_FIELD_GLF
*&---------------------------------------------------------------------*
*&      Form  F_NOVO_ALV
*&---------------------------------------------------------------------*
*       Novo ALV - Cris
*       com nova coluna de marcação de linha
*       para execução do Batch input.
*----------------------------------------------------------------------*
FORM F_NOVO_ALV .

  PERFORM: F_ALV_SORT,
           F_ALV_FIELDCAT,
           F_ALV_TRANSF,
           F_ALV_IMPRIME,
           F_LIMPA.

ENDFORM.                    " F_NOVO_ALV
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Verifica classe de custo
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS .

  CLEAR:  TI_CSKB[], TI_SKB1[].

* Busca Classes custos (dados dependentes da área de contab.custos)
  SELECT * INTO TABLE  TI_CSKB  FROM  CSKB "#EC CI_DB_OPERATION_OK[2389136] unterdrückt werden
    FOR ALL ENTRIES IN T_LIST_GLU1
    WHERE KOKRS = 'MAGI'
      AND KSTAR = T_LIST_GLU1-ORG_ACCOUNT.
  IF SY-SUBRC = 0 .
    SELECT * INTO TABLE TI_SKB1 FROM SKB1 "#EC CI_DB_OPERATION_OK[2431747] unterdrückt werden
      FOR ALL ENTRIES IN TI_CSKB
      WHERE BUKRS IN BUKRS
        AND SAKNR = TI_CSKB-KSTAR.
    IF SY-SUBRC = 0 .
      SORT TI_SKB1 BY SAKNR.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_BATCH_INPUT
*&---------------------------------------------------------------------*
*    Exxecução do SHDB
*----------------------------------------------------------------------*
FORM F_BATCH_INPUT .
  DATA VACCOUNT TYPE GLU1-RACCT.
  CLEAR TI_LOG[].
  SORT T_LIST_GLU1 BY ORG_ACCOUNT.
  VSEQTOTA = 0.
  LOOP AT T_LIST_GLU1 INTO WA_LIST_GLU1 WHERE MARK = 'X'.
*    IF VACCOUNT = WA_LIST_GLU1-ORG_ACCOUNT.
*      CONTINUE.
*    ENDIF.
*Define chave de lançamento
    PERFORM: F_DEFINE_CHAVE.
*Executa F-02 para Centro de custo ou Ordem interna.
    READ TABLE TI_CSKB INTO WA_CSKB WITH KEY KSTAR = WA_LIST_GLU1-ORG_ACCOUNT.
    IF SY-SUBRC = 0.
      PERFORM   F_GRAVA_ZIB USING WA_CSKB-KATYP.
    ELSE.
      PERFORM   F_GRAVA_ZIB USING '00'.
    ENDIF.
    VACCOUNT = WA_LIST_GLU1-ORG_ACCOUNT.
  ENDLOOP.
  MESSAGE I398(00) WITH VSEQTOTA
                            'registro(s) executado(s)'
                            ' com sucesso.'
                            '(gravados na ZIB_CONTABIL).'.
* Imprime LOG
*  PERFORM F_IMPRIME_LOG.

ENDFORM.                    " F_BATCH_INPUT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*    Ordenação dos campos do Relatório
*----------------------------------------------------------------------*
FORM F_ALV_SORT .
  DATA I TYPE I.

  I = I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = I.
  IT_SORT-FIELDNAME = 'RYEAR'.
  IT_SORT-UP        = 'X'.
  APPEND IT_SORT.

  I = I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = I.
  IT_SORT-FIELDNAME = 'RBUKRS'.
  IT_SORT-UP        = 'X'.
  APPEND IT_SORT.

  I = I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = I.
  IT_SORT-FIELDNAME = 'RACCT'.
  IT_SORT-UP        = 'X'.
  APPEND IT_SORT.

  I = I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = I.
  IT_SORT-FIELDNAME = 'ORG_ACCOUNT'.
  IT_SORT-UP        = 'X'.
  APPEND IT_SORT.

  I = I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = I.
  IT_SORT-FIELDNAME = 'RTCUR'.
  IT_SORT-UP        = 'X'.
  APPEND IT_SORT.

  I = I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = I.
  IT_SORT-FIELDNAME = 'RUNIT'.
  IT_SORT-UP        = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*     Características dos campos
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  DATA I TYPE I.

*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'MARK'.
*  WA_AFIELD-CHECKBOX      = 'X'.
*  WA_AFIELD-EDIT          = 'X'.
*  WA_AFIELD-KEY           = 'X'.
*  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RYEAR'.
  WA_AFIELD-REF_FIELDNAME = 'RYEAR'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RBUKRS'.
  WA_AFIELD-REF_FIELDNAME = 'RBUKRS'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RACCT'.
  WA_AFIELD-REF_FIELDNAME = 'RACCT'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ORG_ACCOUNT'.
  WA_AFIELD-REF_FIELDNAME = 'ORG_ACCOUNT'.
  WA_AFIELD-REF_TABNAME   = 'GLU1_XSL_CUM'.
  WA_AFIELD-HOTSPOT       = 'X'.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RTCUR'.
  WA_AFIELD-REF_FIELDNAME = 'RTCUR'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RUNIT'.
  WA_AFIELD-REF_FIELDNAME = 'RUNIT'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TSL'.
  WA_AFIELD-REF_FIELDNAME = 'TSL'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'HSL'.
  WA_AFIELD-REF_FIELDNAME = 'HSL'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LCURR'.
  WA_AFIELD-REF_FIELDNAME = 'LCURR'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KSL'.
  WA_AFIELD-REF_FIELDNAME = 'KSL'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'GCURR'.
  WA_AFIELD-REF_FIELDNAME = 'GCURR'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'OSL'.
  WA_AFIELD-REF_FIELDNAME = 'OSL'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'OCURR'.
  WA_AFIELD-REF_FIELDNAME = 'OCURR'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MSL'.
  WA_AFIELD-REF_FIELDNAME = 'MSL'.
  WA_AFIELD-REF_TABNAME   = 'GLU1'.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_TRANSF
*&---------------------------------------------------------------------*
*      Configura informações
*----------------------------------------------------------------------*
FORM F_ALV_TRANSF .

*  wa_layout-coltab_fieldname    = 'COLINFO'.
  WA_LAYOUT-NO_ZEBRA            = 'X'.
*  wa_layout-cell_merge          = 'X'.    "Desneccessário
  WA_LAYOUT-COLWIDTH_OPTIMIZE   = 'X'.
  WA_LAYOUT-BOX_FIELDNAME       = 'MARK'.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      IT_FIELDCAT = IT_FIELDCAT
      IS_LAYOUT   = WA_LAYOUT
    IMPORTING
      ET_FIELDCAT = IT_FCAT
      ES_LAYOUT   = WA_LAY.

ENDFORM.                    " F_ALV_TRANSF
*&---------------------------------------------------------------------*
*&      Form  F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*       Apresenta Relatório
*----------------------------------------------------------------------*

FORM F_ALV_IMPRIME .

  CALL FUNCTION 'K_KKB_SAVE_MODE_GET'
    IMPORTING
      E_SAVE = V_SAVE.

  MOVE SY-REPID TO V_REPID.
  MOVE: V_REPID TO WA_VARIANT-REPORT,
        P_VARIA TO WA_VARIANT-VARIANT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = V_REPID
      IS_LAYOUT                = WA_LAY
      IT_FIELDCAT              = IT_FCAT
*      i_callback_top_of_page   = 'F_TOP_OF_PAGE'
*      i_background_id          = 'ALV_BACKGROUND'
      I_GRID_TITLE             = 'Zeramento de contas de Resultado Exercício'
      I_CALLBACK_USER_COMMAND  = 'F_AT_USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'F_PF_STATUS_NOVO'
      IT_SORT                  = IT_SORT[]
      I_SAVE                   = 'A'
      IS_VARIANT               = WA_VARIANT
    TABLES
      T_OUTTAB                 = T_LIST_GLU1[]
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

ENDFORM.                    " F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_NOVO
*&---------------------------------------------------------------------*
*     Força um PF Status com o botão para salvar layout
*----------------------------------------------------------------------*
FORM F_PF_STATUS_NOVO USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'F_SET_PF'.
ENDFORM.                    " F_PF_STATUS_NOVO
*&---------------------------------------------------------------------*
*&      Form  F_AT_USER_COMMAND
*&---------------------------------------------------------------------*
*     Tratamento de comandos do usuário - Salvar e Duplo clique
*----------------------------------------------------------------------*
FORM F_AT_USER_COMMAND USING UCOMM LIKE SY-UCOMM
                             SELFIELD TYPE KKBLO_SELFIELD.
  ST_SELFIELD = SELFIELD.

  SELFIELD-REFRESH = 'X'.

  READ TABLE T_LIST_GLU1 INTO WA_LIST_GLU1 INDEX SELFIELD-TABINDEX.

  CASE SELFIELD-FIELDNAME.
    WHEN 'ORG_ACCOUNT'.
      TYPES: BEGIN OF TY_ITAB ,
                 NAME(80)     TYPE C,
               END OF TY_ITAB.

      DATA: MSG_ALV TYPE CHAR80,
            ITAB_MSG TYPE TABLE OF TY_ITAB,
            WTAB_MSG TYPE  TY_ITAB.
      DATA: L_DATA TYPE CHAR10.
      WRITE P_BLDAT TO L_DATA.
      DATA: L_WRBTR TYPE ZIB_CONTABIL-WRBTR.
*       Tirar valor negativo
      IF WA_LIST_GLU1-TSL < 0.
        WA_LIST_GLU1-TSL = WA_LIST_GLU1-TSL * -1.
      ENDIF.
      L_WRBTR = WA_LIST_GLU1-TSL.
      SELECT *
         FROM ZIB_CONTABIL
         INTO  TABLE IT_ZIB_CONTABIL
        WHERE OBJ_KEY LIKE 'ZGL011%'
        AND BUKRS     = WA_LIST_GLU1-RBUKRS
        AND BLDAT     = L_DATA
        AND GJAHR     = P_BLDAT+0(4)
        AND MONAT     = P_MONAT
        AND BLART     = P_BLART
        AND HKONT     = WA_LIST_GLU1-ORG_ACCOUNT
        AND WRBTR     = L_WRBTR
        AND SGTXT     = P_SGTXT
        AND BEWAR     = P_BEWAR.
      SORT IT_ZIB_CONTABIL BY OBJ_KEY.
      DELETE ADJACENT DUPLICATES FROM IT_ZIB_CONTABIL COMPARING OBJ_KEY.
      SORT IT_ZIB_CONTABIL BY OBJ_KEY DESCENDING.
      CLEAR WA_ZIB_CONTABIL.
      " pega o ultimo gerado, pois pode haver estorno
      LOOP AT IT_ZIB_CONTABIL INTO WA_ZIB_CONTABIL.
        SELECT SINGLE *
           FROM ZIB_CONTABIL_CHV
           INTO  WA_ZIB_CONTABIL_CHV
           WHERE OBJ_KEY  = WA_ZIB_CONTABIL-OBJ_KEY.
        IF SY-SUBRC = 0.
          WTAB_MSG-NAME    = '------------------------DOCUMENTO GERADO-------------------------------------'.
          APPEND WTAB_MSG TO ITAB_MSG .
          CLEAR WTAB_MSG.
          WTAB_MSG-NAME    = WA_ZIB_CONTABIL_CHV-BELNR.
          APPEND WTAB_MSG TO ITAB_MSG .
          CLEAR WTAB_MSG.
        ELSE.
          SELECT *
             FROM ZIB_CONTABIL_ERR
             INTO TABLE IT_ZIB_CONTABIL_ERR
             WHERE OBJ_KEY  = WA_ZIB_CONTABIL-OBJ_KEY.
          IF SY-SUBRC = 0.
            WTAB_MSG-NAME    = '------------------------MENSAGEM ERRO---------------------------------------'.
            APPEND WTAB_MSG TO ITAB_MSG .
            CLEAR WTAB_MSG.
            LOOP AT IT_ZIB_CONTABIL_ERR INTO WA_ZIB_CONTABIL_ERR.
              WTAB_MSG-NAME = WA_ZIB_CONTABIL_ERR-MESSAGE.
              APPEND WTAB_MSG TO ITAB_MSG .
              CLEAR WTAB_MSG.
            ENDLOOP.
          ENDIF.
        ENDIF.
        EXIT.
      ENDLOOP.
      IF IT_ZIB_CONTABIL[] IS  INITIAL.
        WTAB_MSG-NAME    = 'Não gerou ZIB_CONTABIL'.
        APPEND WTAB_MSG TO ITAB_MSG .
        CLEAR WTAB_MSG.
        MSG_ALV = 'Erro gravação'.
      ELSEIF ITAB_MSG[] IS INITIAL.
        WTAB_MSG-NAME    = 'Não processou, aguarde....'.
        APPEND WTAB_MSG TO ITAB_MSG .
        CLEAR WTAB_MSG.
        CONCATENATE 'Chave ZIB ' WA_ZIB_CONTABIL-OBJ_KEY INTO MSG_ALV SEPARATED BY SPACE.
      ELSE.
        CONCATENATE 'Chave ZIB ' WA_ZIB_CONTABIL-OBJ_KEY INTO MSG_ALV SEPARATED BY SPACE.
      ENDIF.
      CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
        EXPORTING
          ENDPOS_COL   = 140
          ENDPOS_ROW   = 20
          STARTPOS_COL = 60
          STARTPOS_ROW = 15
          TITLETEXT    = MSG_ALV
        TABLES
          VALUETAB     = ITAB_MSG
        EXCEPTIONS
          BREAK_OFF    = 1
          OTHERS       = 2.

  ENDCASE.

  CASE UCOMM.
    WHEN 'BOTAONOVO' .    "EXECUTAR NOVO PROCESSO.
      PERFORM: F_BUSCA_DADOS,
               F_BATCH_INPUT.
    WHEN 'TODOS'  .         "MARCAR TUDD.
      LOOP AT T_LIST_GLU1 INTO WA_LIST_GLU1.
        WA_LIST_GLU1-MARK = 'X'.
        MODIFY T_LIST_GLU1 FROM WA_LIST_GLU1.
      ENDLOOP.
    WHEN 'NENHUM'  .         "DESMARCAR TUDO.
      LOOP AT T_LIST_GLU1 INTO WA_LIST_GLU1.
        WA_LIST_GLU1-MARK = ' '.
        MODIFY T_LIST_GLU1 FROM WA_LIST_GLU1.
      ENDLOOP.
  ENDCASE.


ENDFORM.                    " F_AT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA
*&---------------------------------------------------------------------*
*     Limpar estruturas para evitar lixo.
*----------------------------------------------------------------------*
FORM F_LIMPA .
  CLEAR: ST_SELFIELD, V_REPID, V_SAVE, WA_VARIANT, WA_AFIELD,
         WA_COLINFO, WA_LAYOUT, WA_HEADER, WA_LAY, IT_FCAT[],
         IT_FIELDCAT[], IT_HEADER[], IT_SORT[].
ENDFORM.                    " F_LIMPA
*&---------------------------------------------------------------------*
*&      Form  F_CALL_T
*&---------------------------------------------------------------------*
*     CHAMA A TRANSAÇÃO
*----------------------------------------------------------------------*
FORM F_CALL_T .
  CLEAR TI_BDCMSGCOLL[].

  DATA: OPT TYPE CTU_PARAMS.
  " Padrão N S X ' '
  OPT-DISMODE = 'N'. " OU 'N'.
  OPT-UPDMODE = 'S'.
  OPT-NOBINPT = 'X'.
  OPT-NOBIEND = ' '.

  CALL TRANSACTION 'F-02'
    USING    TI_BDCDATA
    OPTIONS FROM OPT
    MESSAGES INTO TI_BDCMSGCOLL.

  IF SY-SUBRC <> 0.
    OPT-DISMODE = 'A'. " OU 'N'.
    OPT-UPDMODE = 'S'.
    OPT-NOBINPT = 'X'.
    OPT-NOBIEND = ' '.

    CALL TRANSACTION 'F-02'
      USING    TI_BDCDATA
      OPTIONS FROM OPT
      MESSAGES INTO TI_BDCMSGCOLL.
  ENDIF.

ENDFORM.                    " F_CALL_T
*&---------------------------------------------------------------------*
*&      Form  F_MSG
*&---------------------------------------------------------------------*
*     Apresenta possíveis erros no relatório
*----------------------------------------------------------------------*
FORM F_MSG .

  WA_LOG-RYEAR        = WA_LIST_GLU1-RYEAR.
  WA_LOG-RBUKRS       = WA_LIST_GLU1-RBUKRS.
  WA_LOG-RACCT        = WA_LIST_GLU1-RACCT.
  WA_LOG-ORG_ACCOUNT  = WA_LIST_GLU1-ORG_ACCOUNT.
  WA_LOG-RTCUR        = WA_LIST_GLU1-RTCUR.
  WA_LOG-RUNIT        = WA_LIST_GLU1-RUNIT.

* Monta mensagem standard
  LOOP AT TI_BDCMSGCOLL INTO WA_BDCMSGCOLL.

    V_MSGV1 = WA_BDCMSGCOLL-MSGV1.
    V_MSGV2 = WA_BDCMSGCOLL-MSGV2.
    V_MSGV3 = WA_BDCMSGCOLL-MSGV3.
    V_MSGV4 = WA_BDCMSGCOLL-MSGV4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        LANGUAGE               = WA_BDCMSGCOLL-MSGSPRA
        MSG_ID                 = WA_BDCMSGCOLL-MSGID
        MSG_NO                 = WA_BDCMSGCOLL-MSGNR
        MSG_VAR1               = V_MSGV1
        MSG_VAR2               = V_MSGV2
        MSG_VAR3               = V_MSGV3
        MSG_VAR4               = V_MSGV4
      IMPORTING
        MSG_TEXT               = V_MSG
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.
    IF SY-SUBRC <> 0 .
      CLEAR WA_LOG-TIPO.
      V_MSG = 'Não houve mensagem'.
    ENDIF.
    WA_LOG-TIPO     = WA_BDCMSGCOLL-MSGTYP.
    WA_LOG-MSG_ERRO = V_MSG.
    APPEND WA_LOG TO TI_LOG.

  ENDLOOP.
ENDFORM.                    " F_MSG
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
* O Mapeamento feito no SHDB é passado para o Código no formato abaixo.
* O parâmetro USING transporta cada conteúdo pra dentro do form f_bdc_data.
*      Centro de Custo
*----------------------------------------------------------------------*

FORM F_SHDB_CCUSTO .

  DATA: WL_GSBER(4).
  CONCATENATE  WA_LIST_GLU1-RBUKRS+2(2) '01' INTO WL_GSBER.

  CLEAR TI_BDCDATA[].
  DATA: L_MES TYPE CHAR2, L_DATA TYPE CHAR10.
  WRITE P_BLDAT TO L_DATA.
  L_MES = P_BLDAT+4(2). "Mês da data
  DATA: L_WRBTR TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-TSL < 0.
    WA_LIST_GLU1-TSL = WA_LIST_GLU1-TSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-TSL TO L_WRBTR.
  DATA: L_DMBE2 TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-KSL < 0.
    WA_LIST_GLU1-KSL = WA_LIST_GLU1-KSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-KSL TO L_DMBE2.
  DATA: L_DMBE3 TYPE CHAR15.
  WRITE WA_LIST_GLU1-OSL TO L_DMBE3.

  CLEAR L_DMBE3.
  IF WA_LIST_GLU1-KSL IS INITIAL AND
    NOT  WA_LIST_GLU1-TSL  IS INITIAL.
    L_DMBE2 = '0,01'.
  ENDIF.

  PERFORM F_BDC_DATA USING:
'SAPMF05A'  '0100'  'X'  ''             ' ',
''          ''      ''   'BDC_OKCODE'	  '/00',
''          ''      ''   'BKPF-BLDAT'	  L_DATA,
''          ''      ''   'BKPF-BLART'	  P_BLART,
''          ''      ''   'BKPF-BUKRS'	  WA_LIST_GLU1-RBUKRS, "bukrs,
''          ''      ''   'BKPF-BUDAT'	  L_DATA,
*''          ''      ''   'BKPF-MONAT'    L_MES,
''          ''      ''   'BKPF-MONAT'	  P_MONAT,
''          ''      ''   'BKPF-WAERS'	  'BRL',
''          ''      ''   'RF05A-NEWBS'  V_CHAVE,
''          ''      ''   'RF05A-NEWKO'  WA_LIST_GLU1-ORG_ACCOUNT,

'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR' L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-KOSTL' P_KOSTL, "<< Centro de Custo
''          ''      ''   'COBL-GSBER' WL_GSBER(4),

'SAPMF05A'  '0330'  'X'  ''             '',
''          ''      ''   'BDC_OKCODE'	  '/00',
''          ''      ''   'BSEG-DMBE2'	  L_DMBE2, "WA_LIST_GLU1-KSL,
*''          ''      ''   'BSEG-DMBE3'    l_dmbe3,
''          ''      ''   'RF05A-NEWBS'  V_CONTR,
''          ''      ''   'RF05A-NEWKO'  WA_LIST_GLU1-RACCT,

'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR'	L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'/00',
''          ''      ''   'BSEG-DMBE2' L_DMBE2,
*''          ''      ''   'BSEG-DMBE3' L_DMBE3,

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'BU'.

ENDFORM.                    " F_SHDB_CCUSTO


*&---------------------------------------------------------------------*
*&      Form  F_ZIB_CCUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GRAVA_ZIB USING P_KATYP.

  DATA: WL_GSBER(4).
  CONCATENATE  WA_LIST_GLU1-RBUKRS+2(2) '01' INTO WL_GSBER.

  CLEAR TI_BDCDATA[].
  DATA: L_MES TYPE CHAR2, L_DATA TYPE CHAR10.
  WRITE P_BLDAT TO L_DATA.
  L_MES = P_BLDAT+4(2). "Mês da data
  DATA: L_WRBTR TYPE ZIB_CONTABIL-WRBTR.
* Tirar valor negativo
  IF WA_LIST_GLU1-TSL < 0.
    WA_LIST_GLU1-TSL = WA_LIST_GLU1-TSL * -1.
  ENDIF.
  L_WRBTR = WA_LIST_GLU1-TSL.
  DATA: L_DMBE2 TYPE ZIB_CONTABIL-DMBE2.
* Tirar valor negativo
  IF WA_LIST_GLU1-KSL < 0.
    WA_LIST_GLU1-KSL = WA_LIST_GLU1-KSL * -1.
  ENDIF.
  L_DMBE2 = WA_LIST_GLU1-KSL.

  DATA: L_DMBE3 TYPE ZIB_CONTABIL-DMBE3.
  IF WA_LIST_GLU1-OSL < 0.
    WA_LIST_GLU1-OSL = WA_LIST_GLU1-OSL * -1.
  ENDIF.
  L_DMBE3 = WA_LIST_GLU1-OSL .

  IF WA_LIST_GLU1-KSL IS INITIAL AND
    NOT  WA_LIST_GLU1-TSL  IS INITIAL.
    L_DMBE2 = 1 / 100.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZID_GL11'
    IMPORTING
      NUMBER      = VSEQ.
  VNUM = VSEQ .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VNUM
    IMPORTING
      OUTPUT = VNUM.
  VSEQITEM = 0.
  ADD 1 TO VSEQTOTA.
  " Primeira Perna lançamento contábil
  CONCATENATE 'ZGL011' VNUM  P_BLDAT+0(4) INTO WA_ZIB_CONTABIL-OBJ_KEY.
  VOBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY.
  ADD 1 TO VSEQITEM.
  WA_ZIB_CONTABIL-SEQITEM   = VSEQITEM.
  WA_ZIB_CONTABIL-BSCHL     = V_CHAVE.
  IF WA_LIST_GLU1-RBUKRS = '0100'.
    WA_ZIB_CONTABIL-GSBER     = 'T001'.
  ELSE.
    WA_ZIB_CONTABIL-GSBER     = WL_GSBER(4).
  ENDIF.
  WA_ZIB_CONTABIL-BUKRS     = WA_LIST_GLU1-RBUKRS.
  WA_ZIB_CONTABIL-INTERFACE = '35'.
  WA_ZIB_CONTABIL-BLDAT     = L_DATA.
  WA_ZIB_CONTABIL-BUDAT     = L_DATA.
  WA_ZIB_CONTABIL-GJAHR     = P_BLDAT+0(4).
  WA_ZIB_CONTABIL-MONAT     = P_MONAT.
  WA_ZIB_CONTABIL-BLART     = P_BLART.
  WA_ZIB_CONTABIL-HKONT     = WA_LIST_GLU1-ORG_ACCOUNT.
  WA_ZIB_CONTABIL-UMSKZ     = SPACE.
  WA_ZIB_CONTABIL-WRBTR     = L_WRBTR.
  IF WA_LIST_GLU1-RBUKRS = '0100'.
    WA_ZIB_CONTABIL-WAERS     = 'ARS'.
    WA_ZIB_CONTABIL-BUPLA     = 'T001'.
    WA_ZIB_CONTABIL-SGTXT     = P_SGTXT.
    WA_ZIB_CONTABIL-WAERS_I   = 'ARS'.
    WA_ZIB_CONTABIL-DMBTR     = L_WRBTR.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = L_DMBE2.
    WA_ZIB_CONTABIL-WAERS_G	  =	'BRL'.
    WA_ZIB_CONTABIL-DMBE3     = L_DMBE3.
  ELSE.
    WA_ZIB_CONTABIL-WAERS     = 'BRL'.
    WA_ZIB_CONTABIL-BUPLA     = WL_GSBER(4).
    WA_ZIB_CONTABIL-SGTXT     = P_SGTXT.
    WA_ZIB_CONTABIL-WAERS_I   = 'BRL'.
    WA_ZIB_CONTABIL-DMBTR     = L_WRBTR.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = L_DMBE2.
  ENDIF.
  WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
  WA_ZIB_CONTABIL-BEWAR     = P_BEWAR.
  IF   P_KATYP = '01'.
    WA_ZIB_CONTABIL-KOSTL     = P_KOSTL.
  ELSEIF   P_KATYP = '22'.
    WA_ZIB_CONTABIL-AUFNR     = P_AUFNR.
  ELSE.
    READ TABLE TI_SKB1 INTO WA_SKB1 WITH KEY FSTAG = 'YB09'.
    IF SY-SUBRC <> 0.
      SELECT SINGLE * FROM SKB1 INTO WA_SKB1 "#EC CI_DB_OPERATION_OK[2431747] unterdrückt werden
        WHERE BUKRS = WA_LIST_GLU1-RBUKRS AND
              SAKNR = WA_LIST_GLU1-ORG_ACCOUNT AND
              FSTAG = 'YB09'.
    ENDIF.
    IF SY-SUBRC = 0.
      WA_ZIB_CONTABIL-PRCTR     = P_PRCTR.
    ENDIF.
  ENDIF.

  INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    "P_ERRO = 'X'.
    CLEAR VOBJ_KEY.
  ELSE.
    COMMIT WORK.
  ENDIF.
  CLEAR  WA_ZIB_CONTABIL.

*  " segunda perna lançamento contábil
  WA_ZIB_CONTABIL-OBJ_KEY = VOBJ_KEY .
  ADD 1 TO VSEQITEM.
  WA_ZIB_CONTABIL-SEQITEM   = VSEQITEM.
  WA_ZIB_CONTABIL-BSCHL     = V_CONTR.
  IF WA_LIST_GLU1-RBUKRS = '0100'.
    WA_ZIB_CONTABIL-GSBER     = 'T001'.
  ELSE.
    WA_ZIB_CONTABIL-GSBER     = WL_GSBER(4).
  ENDIF.
  WA_ZIB_CONTABIL-BUKRS     = WA_LIST_GLU1-RBUKRS.
  WA_ZIB_CONTABIL-INTERFACE = '35'.
  WA_ZIB_CONTABIL-BLDAT     = L_DATA.
  WA_ZIB_CONTABIL-BUDAT     = L_DATA.
  WA_ZIB_CONTABIL-GJAHR     = P_BLDAT+0(4).
  WA_ZIB_CONTABIL-MONAT     = P_MONAT.
  WA_ZIB_CONTABIL-BLART     = P_BLART.
  WA_ZIB_CONTABIL-HKONT     = WA_LIST_GLU1-RACCT.
  WA_ZIB_CONTABIL-UMSKZ     = SPACE.
  WA_ZIB_CONTABIL-WRBTR     = L_WRBTR.

  IF WA_LIST_GLU1-RBUKRS = '0100'.
    WA_ZIB_CONTABIL-WAERS     = 'ARS'.
    WA_ZIB_CONTABIL-BUPLA     = 'T001'.
    WA_ZIB_CONTABIL-SGTXT     = P_SGTXT.
    WA_ZIB_CONTABIL-WAERS_I   = 'ARS'.
    WA_ZIB_CONTABIL-DMBTR     = L_WRBTR.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = L_DMBE2.
    WA_ZIB_CONTABIL-WAERS_G	  =	'BRL'.
    WA_ZIB_CONTABIL-DMBE3     = L_DMBE3.
  ELSE.
    WA_ZIB_CONTABIL-WAERS     = 'BRL'.
    WA_ZIB_CONTABIL-BUPLA     = WL_GSBER(4).
    WA_ZIB_CONTABIL-SGTXT     = P_SGTXT.
    WA_ZIB_CONTABIL-WAERS_I   = 'BRL'.
    WA_ZIB_CONTABIL-DMBTR     = L_WRBTR.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = L_DMBE2.
  ENDIF.

  WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
  WA_ZIB_CONTABIL-BEWAR     = P_BEWAR.
  IF   P_KATYP = '01'.
    WA_ZIB_CONTABIL-KOSTL     = P_KOSTL.
  ELSEIF   P_KATYP = '22'.
    WA_ZIB_CONTABIL-AUFNR     = P_AUFNR.
  ELSE.
    READ TABLE TI_SKB1 INTO WA_SKB1 WITH KEY FSTAG = 'YB09'.
    IF SY-SUBRC <> 0.
      SELECT SINGLE * FROM SKB1 INTO WA_SKB1 "#EC CI_DB_OPERATION_OK[2431747]
        WHERE BUKRS = WA_LIST_GLU1-RBUKRS AND
              SAKNR = WA_LIST_GLU1-ORG_ACCOUNT AND
              FSTAG = 'YB09'.
    ENDIF.
    IF SY-SUBRC = 0.
      WA_ZIB_CONTABIL-PRCTR     = P_PRCTR.
    ENDIF.
  ENDIF.

  INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    "P_ERRO = 'X'.
    CLEAR VOBJ_KEY.
  ELSE.
    COMMIT WORK.
  ENDIF.
  CLEAR  WA_ZIB_CONTABIL.


ENDFORM.                    " F_ZIB_CCUSTO
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_CLUCRO
*&---------------------------------------------------------------------*
*      Centro de Lucro
*----------------------------------------------------------------------*

FORM F_SHDB_CLUCRO .

  DATA: WL_GSBER(4).
  CONCATENATE  WA_LIST_GLU1-RBUKRS+2(2) '01' INTO WL_GSBER.
  CLEAR TI_BDCDATA[].
  DATA: L_MES TYPE CHAR2, L_DATA TYPE CHAR10.
  WRITE P_BLDAT TO L_DATA.
  L_MES = P_BLDAT+4(2). "Mês da data
  DATA: L_WRBTR TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-TSL < 0.
    WA_LIST_GLU1-TSL = WA_LIST_GLU1-TSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-TSL TO L_WRBTR.
  DATA: L_DMBE2 TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-KSL < 0.
    WA_LIST_GLU1-KSL = WA_LIST_GLU1-KSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-KSL TO L_DMBE2.
  DATA: L_DMBE3 TYPE CHAR15.
  WRITE WA_LIST_GLU1-OSL TO L_DMBE3.

  CLEAR L_DMBE3.
  IF WA_LIST_GLU1-KSL IS INITIAL AND
    NOT  WA_LIST_GLU1-TSL  IS INITIAL.
    L_DMBE2 = '0,01'.
  ENDIF.
  PERFORM F_BDC_DATA USING:

'SAPMF05A'  '0100'  'X'  ''             ' ',
''          ''      ''   'BDC_OKCODE'	  '/00',
''          ''      ''   'BKPF-BLDAT'	  L_DATA,
''          ''      ''   'BKPF-BLART'	  P_BLART,
''          ''      ''   'BKPF-BUKRS'	  WA_LIST_GLU1-RBUKRS, "bukrs,
''          ''      ''   'BKPF-BUDAT'	  L_DATA,
*''          ''      ''   'BKPF-MONAT'    L_MES,
''          ''      ''   'BKPF-MONAT'	  P_MONAT,
''          ''      ''   'BKPF-WAERS'	  'BRL',
''          ''      ''   'RF05A-NEWBS'  V_CHAVE,
''          ''      ''   'RF05A-NEWKO'  WA_LIST_GLU1-ORG_ACCOUNT.

  IF V_CHAVE = '40'.
    V_CHAVE = '50'.
  ELSE.
    V_CHAVE = '40'.
  ENDIF.
  PERFORM F_BDC_DATA USING:
'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR' L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-GSBER' WL_GSBER(4),
''          ''      ''   'COBL-PRCTR' P_PRCTR, "<< Centro de Lucro

'SAPMF05A'  '0330'  'X'  ''            '',
''          ''      ''   'BDC_OKCODE'	 '/00',
''          ''      ''   'BSEG-DMBE2'	 L_DMBE2, "WA_LIST_GLU1-KSL,
*''          ''      ''   'BSEG-DMBE3'   l_dmbe3,"'',
''          ''      ''   'RF05A-NEWBS' V_CHAVE,
''          ''      ''   'RF05A-NEWKO' WA_LIST_GLU1-RACCT,

'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR'	L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-GSBER'	 WL_GSBER(4),

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'/00',
''          ''      ''   'BSEG-DMBE2' L_DMBE2,
*''          ''      ''   'BSEG-DMBE3' L_DMBE3,

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'BU'.

ENDFORM.                    " F_SHDB_CLUCRO
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_OI
*&---------------------------------------------------------------------*
*       Ordem Interna
*----------------------------------------------------------------------*
FORM F_SHDB_OI .
  DATA: WL_GSBER(4).
  CONCATENATE  WA_LIST_GLU1-RBUKRS+2(2) '01' INTO WL_GSBER.
  CLEAR TI_BDCDATA[].
  DATA: L_MES TYPE CHAR2, L_DATA TYPE CHAR10.
  WRITE P_BLDAT TO L_DATA.
  L_MES = P_BLDAT+4(2). "Mês da data
  DATA: L_WRBTR TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-TSL < 0.
    WA_LIST_GLU1-TSL = WA_LIST_GLU1-TSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-TSL TO L_WRBTR.
  DATA: L_DMBE2 TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-KSL < 0.
    WA_LIST_GLU1-KSL = WA_LIST_GLU1-KSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-KSL TO L_DMBE2.
  DATA: L_DMBE3 TYPE CHAR15.
  WRITE WA_LIST_GLU1-OSL TO L_DMBE3.

  CLEAR L_DMBE3.
  IF WA_LIST_GLU1-KSL IS INITIAL AND
    NOT  WA_LIST_GLU1-TSL  IS INITIAL.
    L_DMBE2 = '0,01'.
  ENDIF.

  PERFORM F_BDC_DATA USING:

'SAPMF05A'  '0100'  'X'  ''             ' ',
''          ''      ''   'BDC_OKCODE'	  '/00',
''          ''      ''   'BKPF-BLDAT'	  L_DATA,
''          ''      ''   'BKPF-BLART'	  P_BLART,
''          ''      ''   'BKPF-BUKRS'	  WA_LIST_GLU1-RBUKRS, "bukrs,
''          ''      ''   'BKPF-BUDAT'	  L_DATA,
*''          ''      ''   'BKPF-MONAT'    L_MES,
''          ''      ''   'BKPF-MONAT'	  P_MONAT,
''          ''      ''   'BKPF-WAERS'	  'BRL',
''          ''      ''   'RF05A-NEWBS'  V_CHAVE,
''          ''      ''   'RF05A-NEWKO'  WA_LIST_GLU1-ORG_ACCOUNT.

  IF V_CHAVE = '50'.
    V_CHAVE = '40'.
  ELSE.
    V_CHAVE = '50'.
  ENDIF.

  PERFORM F_BDC_DATA USING:
'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR' L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-GSBER' WL_GSBER,
''          ''      ''   'COBL-aufnr' P_AUFNR, "<< Ordem interna

'SAPMF05A'  '0330'  'X'  ''            '',
''          ''      ''   'BDC_OKCODE'	 '/00',
''          ''      ''   'BSEG-DMBE2'	 L_DMBE2, "WA_LIST_GLU1-KSL,
*''          ''      ''   'BSEG-DMBE3'   l_dmbe3,
''          ''      ''   'RF05A-NEWBS' V_CHAVE,
''          ''      ''   'RF05A-NEWKO' WA_LIST_GLU1-RACCT,

'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR'	L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-GSBER'	 WL_GSBER,


'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'/00',
''          ''      ''   'BSEG-DMBE2' L_DMBE2,
*''          ''      ''   'BSEG-DMBE3' L_DMBE3,

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'BU'.

ENDFORM.                    " F_SHDB_OI

*&---------------------------------------------------------------------*
*&      Form  F_ZIB_OI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ZIB_OI.

ENDFORM.                    "F_ZIB_OI


*&---------------------------------------------------------------------*
*&      Form  F_SHDB_SEM
*&---------------------------------------------------------------------*
*     SEM Centro de Custo e de Lucro
*----------------------------------------------------------------------*

FORM F_SHDB_SEM .

  DATA: WL_GSBER(4).
  CONCATENATE  WA_LIST_GLU1-RBUKRS+2(2) '01' INTO WL_GSBER.

  CLEAR TI_BDCDATA[].
  DATA: L_MES TYPE CHAR2, L_DATA TYPE CHAR10.
  WRITE P_BLDAT TO L_DATA.
  L_MES = P_BLDAT+4(2). "Mês da data
  DATA: L_WRBTR TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-TSL < 0.
    WA_LIST_GLU1-TSL = WA_LIST_GLU1-TSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-TSL TO L_WRBTR.
  DATA: L_DMBE2 TYPE CHAR15.
* Tirar valor negativo
  IF WA_LIST_GLU1-KSL < 0.
    WA_LIST_GLU1-KSL = WA_LIST_GLU1-KSL * -1.
  ENDIF.
  WRITE WA_LIST_GLU1-KSL TO L_DMBE2.
  DATA: L_DMBE3 TYPE CHAR15.
  WRITE WA_LIST_GLU1-OSL TO L_DMBE3.

  PERFORM F_BDC_DATA USING:

'SAPMF05A'  '0100'  'X'  ''             ' ',
''          ''      ''   'BDC_OKCODE'	  '/00',
''          ''      ''   'BKPF-BLDAT'	  L_DATA,
''          ''      ''   'BKPF-BLART'	  P_BLART,
''          ''      ''   'BKPF-BUKRS'	  WA_LIST_GLU1-RBUKRS, "bukrs,
''          ''      ''   'BKPF-BUDAT'	  L_DATA,
*''          ''      ''   'BKPF-MONAT'    L_MES,
''          ''      ''   'BKPF-MONAT'	  P_MONAT,
''          ''      ''   'BKPF-WAERS'	  'BRL',
''          ''      ''   'RF05A-NEWBS'  V_CHAVE,
''          ''      ''   'RF05A-NEWKO'  WA_LIST_GLU1-ORG_ACCOUNT.

  IF V_CHAVE = '50'.
    V_CHAVE = '40'.
  ELSE.
    V_CHAVE = '50'.
  ENDIF.

  PERFORM F_BDC_DATA USING:
'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR' L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-GSBER' WL_GSBER,

'SAPMF05A'  '0330'  'X'  ''            '',
''          ''      ''   'BDC_OKCODE'	 '/00',
''          ''      ''   'BSEG-DMBE2'	 L_DMBE2, "WA_LIST_GLU1-KSL,
''          ''      ''   'BSEG-DMBE3'	 '',
''          ''      ''   'RF05A-NEWBS' V_CHAVE,
''          ''      ''   'RF05A-NEWKO' WA_LIST_GLU1-RACCT,

'SAPMF05A'  '0300'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'=ZK',
''          ''      ''   'BSEG-WRBTR'	L_WRBTR, "WA_LIST_GLU1-TSL,
''          ''      ''   'BSEG-BUPLA'	WL_GSBER(4),
''          ''      ''   'BSEG-SGTXT'	P_SGTXT,
''          ''      ''   'COBL-GSBER'	WL_GSBER,

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'/00',
''          ''      ''   'BSEG-DMBE2' L_DMBE2,
*''          ''      ''   'BSEG-DMBE3' L_DMBE3,

'SAPMF05A'  '0330'  'X'  ''           '',
''          ''      ''   'BDC_OKCODE'	'BU'.

ENDFORM.                    " F_SHDB_SEM
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_Chave
*&---------------------------------------------------------------------*
*       Compõe a Chave de Lançamento
*----------------------------------------------------------------------*

FORM F_DEFINE_CHAVE .

  CLEAR: V_CHAVE, V_CONTR.

*Regra Chave de lançamento
  IF WA_LIST_GLU1-TSL < 0.
* Chave de lançamento A
    V_CHAVE = '40'. V_CONTR = '50'.
  ELSE.
* Chave de lançamento B
    V_CHAVE = '50'. V_CONTR = '40'.
  ENDIF.

ENDFORM.                    " F_DEFINE_Chave
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_LOG
*&---------------------------------------------------------------------*
*      vISUALIZAÇÃO DE ERROS
*----------------------------------------------------------------------*
FORM F_IMPRIME_LOG .

  ULINE.
  LOOP AT TI_LOG INTO WA_LOG.
    WRITE:/ '|',  WA_LOG-RYEAR        ,'|',
                  WA_LOG-RBUKRS       ,'|',
                  WA_LOG-RACCT        ,'|',
                  WA_LOG-ORG_ACCOUNT  ,'|',
                  WA_LOG-RTCUR        ,'|',
                  WA_LOG-RUNIT        ,'|',
                  WA_LOG-TIPO         ,'|',
                  WA_LOG-MSG_ERRO     ,'|'.
    ULINE.
  ENDLOOP.

ENDFORM.                    " F_IMPRIME_LOG

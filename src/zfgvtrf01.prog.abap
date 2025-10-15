*ACC ALV
*Initialization
INITIALIZATION.
  IF P_APPL IS INITIAL.
    PERFORM INIT_APPL USING 'FAGLGVTR'      "sy-tcode
                            P_APPL.
  ENDIF.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'XTEST'.
      SCREEN-INPUT = ''.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*Modify output fields
AT SELECTION-SCREEN OUTPUT.
  PERFORM INIT_TAB USING P_APPL
                         C_TAB.

  PERFORM MODIF_SCREEN USING P_APPL
                             C_TAB.

*Checks on selection screen
AT SELECTION-SCREEN.
  PERFORM PARAMETER_PRUEFEN.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
  ENDIF.

*Start of processing
START-OF-SELECTION.

* ------ registration for shedule manager -----------------------------
  PERFORM SCHEDMAN_START_STOP USING 'START'.

*Check if generated includes (e.g. FGVTRF64) have to be generated
  PERFORM CHECK_NEW_GENERATION.

*Build title
  SY-TITLE+20(06) = TEXT-100.
  SY-TITLE+27     = LEDGER.
  SY-TITLE+30(19) = TEXT-011.
  SY-TITLE+50     = NEWJR.

  IF XTEST = SPACE.
    SY-TITLE+55 = TEXT-002.            "Echtlauf
  ELSE.
    SY-TITLE+55 = TEXT-001.            "Testlauf
  ENDIF.
  CONDENSE SY-TITLE.

*Check whether summary records of the previous year have already
*been archived
  PERFORM CHECK_ARCHIVE USING GT_VERSION_DATA.

*Retained earnings accounts in GUVTAB
  PERFORM FUELLEN_GUVTAB.              "GVTYP, Fix-Kto., VTWTAB
* perform fill_guvtab.                 "OSS Note 907758

*List of errors from parameters check
  PERFORM CHECK_KONSISTENZ.            "Kontrolle der Tab-Einträge->Feli

  IF XKONS NE ' '.                     "X: Konsistenz richtig

******Begin of real processing***************************************
    CALL FUNCTION 'MESSAGES_INITIALIZE'."Meldungen in Feli

*Set list mode
    PERFORM SET_LIST_FIELDS.
*Global assigns for Orgunit and Account (*GLU1 and GLU1)
    PERFORM GLOBAL_ASSIGNS USING H_881
                                 H_800A.
*Process one organizational unit after another
    LOOP AT BUKGESTAB.
*Reset period 0 to 0 for new year
      PERFORM DELETE_PERIOD_ZERO USING BUKGESTAB-BUKGES.
*Get all accounts for orgunit
      PERFORM GET_ACCOUNTS USING BUKGESTAB.
*Get range of accounts to be selected
      PERFORM GET_RANGE_OF_ACCOUNTS.
*Select records for each account range and process them
*Get the first account range entry
      PERFORM GET_NEXT_ACCOUNT_ENTRY USING 1
                                           H_SELECT_FLAG.
*Clear message handler queue
      CALL FUNCTION 'MESSAGES_INITIALIZE'.
*First interval must always be selected (even if empty)
      H_SELECT_FLAG = 'X'.
      H_ACCOUNT_RANGE_INDEX = 1.
      WHILE H_SELECT_FLAG = 'X'.
*select records from old year, process them and store them
*summarized in internal tables T_BSxxx and T_PL_xxx
        PERFORM SELECT_RECORDS USING BUKGESTAB-BUKGES.
*Append internal tables T_BS_xxx and T_PL_xxx to update_tables T_xxx
        PERFORM LOOP.
*Post records from update_tables T_xxx for last records
        IF XTEST =  SPACE.               "Echtlauf
          PERFORM UPDATE_DB.
          PERFORM CLEAR_UPDATE_TABLES.
          COMMIT WORK.
        ENDIF.
*Get the next account range entry
        ADD 1 TO H_ACCOUNT_RANGE_INDEX.
        PERFORM GET_NEXT_ACCOUNT_ENTRY USING H_ACCOUNT_RANGE_INDEX
                                             H_SELECT_FLAG.
      ENDWHILE.
*Update protocol tables T800HIST*
      PERFORM UPDATE_T800HIST.
*Set new year in T882, T882C
      PERFORM UPDATE_T882.
    ENDLOOP.

*Protocol
    PERFORM PROTOKOLL.                 "Liste, Fehler-Protokoll
  ENDIF.

*End processing for schedman
  PERFORM SCHEDMAN_START_STOP USING 'STOP'.

  CASE P_APPL.
    WHEN APPL-FIGLF.
      SET PARAMETER ID 'GLN_FLEX' FIELD LEDGER.
    WHEN APPL-FISL.
      SET PARAMETER ID 'GLN' FIELD LEDGER.
  ENDCASE.
*F1-Help
AT SELECTION-SCREEN ON HELP-REQUEST FOR XACCT.
  PERFORM POPUP_FOR_F1HELP USING 'XACCT'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR P_FIELD1.
  PERFORM POPUP_FOR_F1HELP USING 'P_FIELD'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR P_FIELD2.
  PERFORM POPUP_FOR_F1HELP USING 'P_FIELD'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR P_FIELD3.
  PERFORM POPUP_FOR_F1HELP USING 'P_FIELD'.

*F4-Help

AT SELECTION-SCREEN ON VALUE-REQUEST FOR LEDGER.
  PERFORM F4_FOR_RLDNR USING P_APPL C_TAB.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FIELD1.
  PERFORM GET_ADD_FIELDS USING 'P_FIELD1'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FIELD2.
  PERFORM GET_ADD_FIELDS USING 'P_FIELD2'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FIELD3.
  PERFORM GET_ADD_FIELDS USING 'P_FIELD3'.

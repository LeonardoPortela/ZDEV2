
*---------------------------------------------------------------------*
*       FORM UPDATE_T882                                              *
*---------------------------------------------------------------------*
FORM update_t882.
* ------ Not in test run  ----------------------------------------------
* ------ and updating actuals
  CHECK xtest =  space
    AND con_rrcty_actuals IN satztyp.

  CALL FUNCTION 'G_SET_CARRY_FORWARD_YEAR'
    EXPORTING
      i_rldnr         = ledger
      i_orgunit       = bukgestab-bukges
      i_vtrhj         = newjr
    EXCEPTIONS
      no_info_found   = 1
      error_in_update = 2
      OTHERS          = 3.
  IF sy-subrc NE 0.
    xaplstat = '4'.
    MESSAGE i628 WITH bukgestab-bukges ledger.
  ENDIF.
  COMMIT WORK.
ENDFORM.                    "UPDATE_T882


*eject
*---------------------------------------------------------------------*
*        FORM SCHEDMAN_START_STOP                                     *
*---------------------------------------------------------------------*
FORM schedman_start_stop USING p_command.

* local statics
  STATICS: ls_key_static LIKE schedman_key.
*local data declaration
  DATA: gs_key      LIKE schedman_key.
  DATA: gt_spono    LIKE schedman_spool.

  DATA: ld_worklist_flag(1).
  DATA: ls_detail   LIKE schedman_detail_user.
  DATA: lt_selkrit  LIKE schedman_selkrit OCCURS 0 WITH HEADER LINE.
  DATA: lt_param    LIKE schedman_selkrit OCCURS 0 WITH HEADER LINE.
  DATA: ls_witem    LIKE scma_witem.
  DATA: ls_ext      LIKE schedman_ext.
  DATA: ls_message LIKE schedman_message,
        ld_objects LIKE smmain-nr_of_objects,
        ld_aplstat LIKE smmain-aplstat.
  DATA: ls_scma_event LIKE scma_event.
* Konstanten für Workflow
*  INCLUDE schedman_events.
  INCLUDE zschedman_events.

* muss in scmatasks
  ls_detail-repid       = 'SAPFGVTR'.
  ls_detail-variante    = sy-slset.    "<<die variante
  ls_detail-application = 'FI-SL'.     "standard
* ------ Konkrete Applikation einsetzen -------------------------------
  CASE p_appl.
    WHEN 'FIGL'.                       "Trans: F.16 -> Hauptbuch GL
      ls_detail-application = 'FI-GL'.
    WHEN 'FIGLX'.                      "Trans. GLGVTR -> flex. GL
      ls_detail-application = 'FI-GL'.
    WHEN 'FISL'.                       "Trans: GVTR -> SL
      ls_detail-application = 'FI-SL'.
    WHEN 'FIGLF'.                      "Trans. FAGLGVTR -> Hauptbuch ERP
      ls_detail-application = 'FI-GL'.
  ENDCASE.

* set flag for test/update run to display in schedman monitor
  IF xtest NE space.
    ls_detail-testflag = 'X'.
    IF xaplstat = '4'.    "wenn Test, weitere Programme starten lassen
      xaplstat = '2'.                  "nur Warnung
    ENDIF.
  ELSE.
    CLEAR ls_detail-testflag.
  ENDIF.
* save some select-options
  CLEAR lt_selkrit.
* BUKRS
  IF xcomptab = ' '.
    lt_selkrit-structure  = 'GLU1'.
    lt_selkrit-field      = 'BUKRS'.
    LOOP AT bukrs.
      MOVE-CORRESPONDING bukrs TO lt_selkrit.
      APPEND lt_selkrit.
    ENDLOOP.
  ELSE.
* RCOMP
    lt_selkrit-structure = 'GLU1'.
    lt_selkrit-field     = 'RCOMP'.
    LOOP AT rcomp.
      MOVE-CORRESPONDING rcomp TO lt_selkrit.
      APPEND lt_selkrit.
    ENDLOOP.
  ENDIF.

  lt_param-optio     = 'EQ'.
  lt_param-structure = 'GLU1'.
  lt_param-field     = 'RLDNR'.
  lt_param-low       = ledger.
  APPEND lt_param.

  lt_param-entry     = 1.
  lt_param-optio     = 'EQ'.
  lt_param-structure = 'GLU1'.
  lt_param-field     = 'RYEAR'.
  lt_param-low       = newjr.
  APPEND lt_param.


  IF p_command = 'START'.
    ls_witem-wf_witem  = wf_witem.
    CALL FUNCTION 'KPEP_MONI_INIT_RECORD'
      EXPORTING
        ls_detail  = ls_detail
        ls_witem   = ls_witem
      IMPORTING
        ls_key     = ls_key_static
      TABLES
        lt_selkrit = lt_selkrit
        lt_param   = lt_param.

  ELSEIF p_command = 'STOP'.
    IF ( xaplstat = '4' ) OR
       ( xaplstat = 'A' ).
      ls_scma_event-wf_event = cs_wf_events-error.
    ELSE.
      ls_scma_event-wf_event = cs_wf_events-finished.
    ENDIF.

    ld_aplstat  = xaplstat.
    ls_scma_event-wf_witem = wf_witem.
    ls_scma_event-wf_okey  = wf_okey.

    CALL FUNCTION 'KPEP_MONI_CLOSE_RECORD'
      EXPORTING
        ls_key        = ls_key_static
        ls_scma_event = ls_scma_event
      CHANGING
        ld_aplstat    = ld_aplstat
      EXCEPTIONS
        no_id_given   = 1
        OTHERS        = 0.

    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  COMMIT WORK.           " <<<<<<<<<<  C O M M I T  W O R K  >>>>>>>

ENDFORM.                    "SCHEDMAN_START_STOP
*eject
*---------------------------------------------------------------------*
*       FORM READ_BUKGESTAB                                           *
*---------------------------------------------------------------------*
*       Die interne Tabelle BUKGESTAB wird nach Bukrs/Gesnr-Wechsel   *
*       auf den aktuellsten Stand eingestellt.                        *
*       Int.Zwsp. AKT wird gefüllt.
*       Notwendig bei AUSGABE_ZEILE für die aktuelle HWAER/KWAER      *
*---------------------------------------------------------------------*
FORM read_bukgestab.
  IF t800a-comptab = ' '.
    IF t800a-objtable = ' '.
      akt-bukges = glu1-bukrs.
      akt-bukrs  = glu1-bukrs.
    ELSE.
      akt-bukges = glu1-rbukrs.
      akt-bukrs  = glu1-rbukrs.
    ENDIF.
  ELSE.
    akt-bukges = glu1-rcomp.
    akt-rcomp  = glu1-rcomp.
  ENDIF.
  akt-racct = glu1-racct.

  IF bukgestab-bukges NE akt-bukges.
    READ TABLE bukgestab WITH KEY bukges = akt-bukges.       "AKT. HWAER
  ENDIF.

ENDFORM.                    "READ_BUKGESTAB
*eject
*---------------------------------------------------------------------*
*       FORM CHECK_FEHLER_MOVEMENTS                                   *
*---------------------------------------------------------------------*
*       Die interne Fehler-Tabelle IT_MESG wird überprüft,            *
*       ob ein neuer Eintrag durch STORE_MESSAGE hineingestellt       *
*       wurde. Wenn ja, dann XERR = 'X'.                              *
*---------------------------------------------------------------------*
FORM check_fehler_movements USING cfm_xerr.

  cfm_xerr = ' '.
  CALL FUNCTION 'MESSAGES_GIVE'
    TABLES
      t_mesg = it_mesg.
  DESCRIBE TABLE it_mesg LINES lin_it_mesg.   "War Fehler ?
  IF lin_it_mesg NE 0.                 "...ja
    mesg_mov-bukges = akt-bukges.
    mesg_mov-racct  = akt-racct.
    mesg_mov-text   = it_mesg-text.
    COLLECT mesg_mov.                  "nur falsche BEWAR ausgeb.
    REFRESH it_mesg.
    CALL FUNCTION 'MESSAGES_INITIALIZE'."Meldungen in Feli löschen
    cfm_xerr = 'X'.                    "Satz nicht verarbeiten
  ENDIF.
ENDFORM.                    "CHECK_FEHLER_MOVEMENTS

*EJECT
*---------------------------------------------------------------------*
*       FORM VALIDATE_RECORD                                          *
*---------------------------------------------------------------------*
*       Verprobt die Objektnummernkombination                         *
*---------------------------------------------------------------------*
*  -->  TAB       Tabelle                                             *
*  -->  SATZ      Satz, der zu verproben ist                          *
*  <--  SY-SUBRC  ne 0: Fehler, Satz nicht verarbeiten  ???           *
*---------------------------------------------------------------------*
FORM validate_record USING tab satz.   " SY-SUBRC.     ???

  REFRESH  period. CLEAR period.
  period-buper = '000'.
  APPEND period.

  CALL FUNCTION 'G_VALIDATE_RECORD'
    EXPORTING
      tabname           = tab
      popup_unvalid_rec = ' '
      write_unvalid_rec = ' '
      record            = satz
    TABLES
      perioden          = period
    EXCEPTIONS
      no_period         = 01
      no_valid_tabname  = 02
      no_valid_records  = 03
      OTHERS            = 04.

  CASE sy-subrc.                       "?? überarbeiten
    WHEN 00.
    WHEN 03.
*     PERFORM STORE_MESSAGE USING 'P' 'E' '148' LEDGER
*                                          TAB   '' '' WF-RC.
*     IF WF-RC > 0.
*       MESSAGE E148 WITH LEDGER       TAB.
*     ENDIF.
*     WF-EXIT = 'X'.
    WHEN OTHERS.
*     PERFORM STORE_MESSAGE USING 'P' 'E' '149' SY-SUBRC '' '' '' WF-RC.
*     IF WF-RC > 0.
*       MESSAGE E149 WITH SY-SUBRC.
*     ENDIF.
*     WF-EXIT = 'X'.
  ENDCASE.

ENDFORM.                    "VALIDATE_RECORD

*---------------------------------------------------------------------*
*       FORM GET_ADD_FIELDS                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  GAF_FIELD                                                     *
*---------------------------------------------------------------------*
FORM get_add_fields USING gaf_field.
  DATA: t_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF t_dis_fields OCCURS 0,
          fieldname LIKE dfies-fieldname,
          scrtext   LIKE dfies-scrtext_m,
        END OF t_dis_fields.
  DATA: BEGIN OF t_fields_lst OCCURS 5.
          INCLUDE  STRUCTURE help_value.
  DATA: END   OF t_fields_lst.
  DATA: h_selectfield LIKE help_info-fieldname,
        h_ind         LIKE sy-tabix.
  DATA: h_subrc LIKE sy-subrc.

*Get input value for Ledger
  t_dynpfields-fieldname = 'LEDGER'.
  APPEND t_dynpfields.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = 'SAPFGVTR'
      dynumb             = '1000'
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.
  READ TABLE t_dynpfields INDEX 1.
  CALL FUNCTION 'G_CONVERT_INPUT'
    EXPORTING
      convexit        = 'ALPHA'
      input_value     = t_dynpfields-fieldvalue
      input_length    = 2
    IMPORTING
      converted_value = t881-rldnr.
*---> S4 Migration - 21/06/2023 - MA
*  SELECT SINGLE * FROM t881 WHERE rldnr = t881-rldnr.
  cl_fins_acdoc_util=>get_t881_emu(
    EXPORTING
      iv_rldnr            = t881-rldnr
*    it_filter           =
*    it_field_range      =
*    it_projection       =
*    ib_keep_t881_tab    =
*    ib_ignore_finsc_del =
*    ib_ignore_appendix  =
*    ib_client_specified =
*  IMPORTING
*    es_t881             =
*    et_t881             =
    EXCEPTIONS
      not_found           = 1
      OTHERS              = 2
  ).
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*<--- S4 Migration - 21/06/2023 - MA
*No error messages possible in F4 processing!!!
  CHECK sy-subrc = 0.
*Get all the fields for the totals table
  CALL FUNCTION 'G_FIELD_SET'
    EXPORTING
      table     = t881-tab
      text_flag = 'X'.
  DO.
    CALL FUNCTION 'G_FIELD_GET'
      IMPORTING
        field_attr = dfies
        subrc      = h_subrc.
    IF dfies-fieldname EQ 'RPMAX'.     "Letztes Feld
      EXIT.
    ENDIF.
    CHECK dfies-fieldname NE 'RCLNT'       AND
          dfies-fieldname NE 'RLDNR'       AND
          dfies-fieldname NE 'RRCTY'       AND
          dfies-fieldname NE 'RVERS'       AND
          dfies-fieldname NE 'RYEAR'       AND
          dfies-fieldname NE 'RBUKRS'      AND
          dfies-fieldname NE 'BUKRS'       AND
          dfies-fieldname NE 'RCOMP'       AND
          dfies-fieldname NE 'RACCT'       AND
          dfies-fieldname NE 'RTCUR'       AND
          dfies-fieldname NE 'RUNIT'       .

    t_dis_fields-fieldname = dfies-fieldname.
    t_dis_fields-scrtext   = dfies-scrtext_m.
    APPEND t_dis_fields.
  ENDDO.

  CLEAR t_fields_lst.
  t_fields_lst-tabname = 'DFIES'.
  t_fields_lst-fieldname = 'FIELDNAME'.
  t_fields_lst-selectflag = 'X'.
  APPEND t_fields_lst.
  CLEAR t_fields_lst.
  t_fields_lst-tabname = 'DFIES'.
  t_fields_lst-fieldname = 'SCRTEXT_M'.
  APPEND t_fields_lst.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
    EXPORTING
      selectfield = h_selectfield
      display     = ' '
      titel       = TEXT-313
    IMPORTING
      ind         = h_ind
    TABLES
      fields      = t_fields_lst
      full_table  = t_dis_fields
    EXCEPTIONS
      OTHERS      = 1.
  IF h_ind NE 0 AND sy-subrc = 0.
    READ TABLE t_dis_fields INDEX h_ind.
    CASE gaf_field.
      WHEN 'P_FIELD1'.
        p_field1 = t_dis_fields-fieldname.
      WHEN 'P_FIELD2'.
        p_field2 = t_dis_fields-fieldname.
      WHEN 'P_FIELD3'.
        p_field3 = t_dis_fields-fieldname.
    ENDCASE.
  ENDIF.
ENDFORM.                               " GET_ADD_FIELDS

*&---------------------------------------------------------------------*
*&      Form  POPUP_FOR_F1HELP
*&---------------------------------------------------------------------*
FORM popup_for_f1help USING source_read.
  DATA: text_object LIKE  dokhl-object.
  DATA: popup_title(40).
  CASE source_read.
    WHEN 'XACCT'.
      text_object = 'FI_GLX_GVTR_XACCT'.
      popup_title = TEXT-319.
    WHEN 'P_FIELD'.
      text_object = 'FI_GLX_GVTR_P_FIELD'.
      popup_title = TEXT-320.
  ENDCASE.
  CALL FUNCTION 'POPUP_DISPLAY_TEXT'
    EXPORTING
      language       = sy-langu
      popup_title    = popup_title
      text_object    = text_object
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                               " POPUP_FOR_F1HELP
*&---------------------------------------------------------------------*
*&      Form  INIT_APPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_TCODE  text
*      -->P_C_APPL  text
*----------------------------------------------------------------------*
FORM init_appl USING    ia_tcode
                        ia_appl.

  DATA: ls_appl TYPE fagl_s_application.

  CLEAR g_glflex.

  CASE ia_tcode.
    WHEN 'GVTR'.
      ia_appl = appl-fisl.
      GET PARAMETER ID 'GLN' FIELD ledger.
    WHEN 'GLGVTR'.
      ia_appl = appl-figlx.
    WHEN 'F.16'.
      ia_appl = appl-figl.
    WHEN '2KES' OR '7KES'.
      ia_appl = appl-ecpca.
    WHEN 'ISSR_MAIN_GVTR'.
      ia_appl = appl-issr.
    WHEN 'RETAIL_GVTR'.     " AL0K091914
      ia_appl = appl-isrt.
    WHEN 'FAGLGVTR'.
*     ia_appl = appl-figlf.
      CALL FUNCTION 'FAGL_GET_APPLICATION'                  "ERP2005
        IMPORTING
          es_appl = ls_appl.
      ia_appl = ls_appl.
      GET PARAMETER ID 'GLN_FLEX' FIELD ledger.
      g_glflex = 'X'.
    WHEN OTHERS.
      IF sy-batch IS INITIAL.
        MESSAGE e632 WITH sy-tcode.
      ENDIF.
  ENDCASE.
ENDFORM.                               " INIT_APPL

*&---------------------------------------------------------------------*
*&      Form  INIT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_TCODE  text
*      -->P_C_APPL  text
*----------------------------------------------------------------------*
FORM init_tab USING  it_appl
                     it_tab.

  CASE it_appl.
    WHEN 'FIGL'.
      it_tab = 'GLT0'.
      CLEAR t_ledgers.
      CALL FUNCTION 'G_READ_FIXED_LEDGERS'
        EXPORTING
          table                 = it_tab
          with_parallel_ledgers = ' '
        TABLES
          ledgers               = t_ledgers.
*Several currency ledgers used -> message not to forget the others
      DESCRIBE TABLE t_ledgers LINES sy-tfill.
      IF sy-tfill > 1.
        g_v_parallel_ledgers = 'X'.
        MESSAGE s633.
      ENDIF.
    WHEN 'FIGLX'.
      it_tab = 'V_GLFLEXT'.
    WHEN 'FIGLF'.
*      it_tab = 'FAGLFLEXT'.
  ENDCASE.
ENDFORM.                               " INIT_APPL

*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modif_screen USING ms_appl
                        ms_tab.
*I FI-GL the ledger can be inactivated if only one ledger (00) is used
  IF ms_appl = 'FIGL'.
    LOOP AT SCREEN.
      IF screen-group1 = 'GL' OR screen-group1 = 'GLF'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE t_ledgers LINES sy-tfill.
    IF sy-tfill <= 1.
*Set fix ledger
      ledger = '00'.
      LOOP AT SCREEN.
        IF screen-name = 'LEDGER'
        OR screen-name = '%_LEDGER_%_APP_%-TEXT'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
*Now set the other fix values
    satztyp = '0'.
    version = '001'.
    p_field1 = 'RBUSA'.
  ENDIF.

*------- Application FI-GLF: New GL in mySAP-ERP-----------------------
  IF ms_appl = 'FIGLF'.
    LOOP AT SCREEN.
      IF screen-group1 = 'GL'.
        IF screen-name EQ 'XACCT'
        OR screen-name = '%_XACCT_%_APP_%-TEXT'.
        ELSE.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
* Set fix values for RRCTY and RVERS
    satztyp = '0'.
    version = '001'.
  ENDIF.

ENDFORM.                               " MODIF_SCREEN

*&---------------------------------------------------------------------*
*&      Form  F4_FOR_RLDNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_APPL  text
*      -->P_C_TAB  text
*----------------------------------------------------------------------*
FORM f4_for_rldnr USING    f4_appl
                           f4_tab.
  DATA: BEGIN OF h_appl,
          appl    LIKE t800a-appl,
          subappl LIKE t800a-subappl,
        END OF h_appl.
  DATA: allowed_tables LIKE rgall_tab OCCURS 0 WITH HEADER LINE.
  DATA: h_with_dependent_ledgers,
        h_only_free_ledgers,
        h_only_fixed_ledgers.
*H_APPL must only have first 4 characters of F4_APPL because the ledgers
*of flexible GL are stored with FI GL in T881 (former flexible GL)
  IF NOT g_glflex IS INITIAL.
    h_appl    = f4_appl.      "New GL in ERP -> all char. into H_APPL
    h_only_fixed_ledgers = space.
    h_with_dependent_ledgers = ' '.
  ELSE.
    h_appl(4) = f4_appl.
  ENDIF.

  IF NOT c_tab IS INITIAL.
    allowed_tables-table = c_tab.
    APPEND allowed_tables.
  ENDIF.

  CASE h_appl.
    WHEN appl-fisl.
      h_only_free_ledgers = 'X'.
      h_with_dependent_ledgers = 'X'.
    WHEN appl-figl.
      h_only_fixed_ledgers = 'X'.
      h_with_dependent_ledgers = ' '.
    WHEN appl-figlx.
      h_only_fixed_ledgers = 'X'.
      h_with_dependent_ledgers = ' '.
    WHEN appl-isrt.
      h_only_free_ledgers = space.
      h_with_dependent_ledgers = 'X'.
      allowed_tables = tab_rt.
      APPEND allowed_tables.
      h_appl-appl = 'FI'.
      h_appl-subappl = 'SL'.
    WHEN appl-figlf.
      h_only_fixed_ledgers = space.
      h_with_dependent_ledgers = ' '.
      h_appl-appl = 'FI'.
      h_appl-subappl = 'GLF'.
  ENDCASE.

  CALL FUNCTION 'G_DISPLAY_LEDGERS_FOR_TABLE'
    EXPORTING
      gappl                  = h_appl-appl
      gsubappl               = h_appl-subappl
      only_free_ledgers      = h_only_free_ledgers
      only_fixed_ledgers     = h_only_fixed_ledgers
      only_display           = ' '
      with_dependent_ledgers = h_with_dependent_ledgers
    IMPORTING
      picked_ledger          = ledger
    TABLES
      allowed_tables         = allowed_tables.


ENDFORM.                               " F4_FOR_RLDNR

*&---------------------------------------------------------------------*
*&      Form  check_new_generation
*&---------------------------------------------------------------------*
*      Check whether program has to be regenerated
*      (has to be generated whenever there was a code change in
*       the generation program RGUGVTR0)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_new_generation.

  DATA: BEGIN OF c OCCURS 200,           "Report-Zeilen
          l(200),
        END   OF c.

  READ REPORT 'FGVTRF64' INTO c.
  READ TABLE c INDEX 1.
  IF c+10(8) NE c_gendat.
    PERFORM generate_forward IN PROGRAM rgugvtr0.
    COMMIT WORK.
    PERFORM log_almsg_var USING 'SY' 'A' '028'
                                'FGVTRF64' ' ' ' ' ' '.
    MESSAGE a028(sy) WITH 'FGVTRF64'. "Please start program again
  ENDIF.

ENDFORM.                    " check_new_generation
*&---------------------------------------------------------------------*
*&      Form  log_almsg_var
*&---------------------------------------------------------------------*
*       .......
*----------------------------------------------------------------------*
FORM log_almsg_var  USING i_msgid
                          i_msgty
                          i_msgno
                          i_msgv1
                          i_msgv2
                          i_msgv3
                          i_msgv4.

* set status variable for schedman
  CASE i_msgty.
    WHEN 'A' OR 'X'.
      xaplstat = 'A'.
    WHEN 'E'.
      xaplstat = '4'.
    WHEN 'W'.
      xaplstat = '2'.
  ENDCASE.

* end schedman processing
  IF i_msgty = 'X' OR
     i_msgty = 'A' OR
     i_msgty = 'E'.
    PERFORM schedman_start_stop USING 'STOP'.
  ENDIF.

ENDFORM.                    " log_almsg_var

*---------------------------------------------------------------------*
*       FORM UPDATE_T800HIST                                          *
*---------------------------------------------------------------------*
FORM update_T800HIST.

  DATA: lt_rsparams TYPE STANDARD TABLE OF rsparams.

*Update only in production run
  CHECK xtest = space.

*Get selection parameters
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = sy-repid
    TABLES
      selection_table = lt_rsparams.

* Delete all initial paremeters and select options
  DELETE lt_rsparams WHERE kind = 'P'
                       AND low  IS INITIAL.

  DELETE lt_rsparams WHERE kind = 'S'
                       AND sign IS INITIAL.

* Delete all unnecessary selection parameters
  DELETE lt_rsparams WHERE
         selname NE 'BUKRS'   AND
         selname NE 'LEDGER'  AND
         selname NE 'NEWJR'   AND
         selname NE 'P_APPL'  AND
         selname NE 'RCOMP'   AND
         selname NE 'SATZTYP' AND
         selname NE 'VERSION' AND
         selname NE 'XACCT'.

*Update them in T800HISTH and T800HISTP
  CALL FUNCTION 'G_UPDATE_T800HIST'
    EXPORTING
      i_objname  = sy-repid
    TABLES
      t_rsparams = lt_rsparams.

ENDFORM.

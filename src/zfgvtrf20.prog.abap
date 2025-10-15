*eject
*---------------------------------------------------------------------*
*       FORM get_ledger_and_tab_info                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ledger                                                        *
*  -->  h_881                                                         *
*  -->  h_800a                                                        *
*---------------------------------------------------------------------*
FORM get_ledger_and_tab_info TABLES
                             t_t800a_keyfig STRUCTURE t800a_keyfig
                             t_t881_keyfig STRUCTURE t881_keyfig
                             USING ledger LIKE t881-rldnr
                                   h_881  LIKE t881
                                   h_800a LIKE t800a.
*Get the ledger informations
  CALL FUNCTION 'G_GET_LEDGER_INFO'
    EXPORTING
      i_rldnr       = ledger
    IMPORTING
      e_ledger_info = h_881
    TABLES
      e_keyfig_info = t_t881_keyfig.

*Get the table informations
  CALL FUNCTION 'G_GET_TABLE_INFO'
    EXPORTING
      i_tab         = h_881-tab
    IMPORTING
      e_tab_info    = h_800a
    TABLES
      e_keyfig_info = t_t800a_keyfig.

*Check if pool table is used
  CALL FUNCTION 'G_CHECK_POOL_TABLE'
    EXPORTING
      i_tab = h_881-tab.
ENDFORM.                    "get_ledger_and_tab_info

*---------------------------------------------------------------------*
*       FORM global_assigns                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  h_881                                                         *
*  -->  h_800a                                                        *
*---------------------------------------------------------------------*
FORM global_assigns USING h_881 LIKE t881
                          h_800a LIKE t800a.
  DATA: t_keyfig LIKE t800a_keyfig OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_GET_TABLE_INFO'
    EXPORTING
      i_tab         = h_800a-tab
    TABLES
      e_keyfig_info = t_keyfig.
*Field symbols for general processing
  ASSIGN COMPONENT  h_800a-dim_orgunit
         OF STRUCTURE glu1 TO <glu1_orgunit>.
  ASSIGN COMPONENT  h_800a-dim_orgunit
         OF STRUCTURE *glu1 TO <*glu1_orgunit>.
  ASSIGN COMPONENT  h_800a-dim_account
         OF STRUCTURE glu1 TO <glu1_account>.
  ASSIGN COMPONENT  h_800a-dim_account
         OF STRUCTURE *glu1 TO <*glu1_account>.
*Field symbols for list display
  CHECK c_list_mode = '1'.
*Orgunit and account in s_list_glu1
  ASSIGN COMPONENT  h_800a-dim_orgunit
         OF STRUCTURE s_list_glu1 TO <s_list_glu1_orgunit>.
  ASSIGN COMPONENT  h_800a-dim_account
         OF STRUCTURE s_list_glu1 TO <s_list_glu1_account>.
*Variable key fields
  IF NOT c_list_field1 IS INITIAL.
    ASSIGN COMPONENT c_list_field1
           OF STRUCTURE glu1 TO <glu1_field1>.
    ASSIGN COMPONENT c_list_field1
           OF STRUCTURE s_list_glu1 TO <s_list_glu1_field1>.
  ENDIF.
  IF NOT c_list_field2 IS INITIAL.
    ASSIGN COMPONENT c_list_field2
           OF STRUCTURE glu1 TO <glu1_field2>.
    ASSIGN COMPONENT c_list_field2
           OF STRUCTURE s_list_glu1 TO <s_list_glu1_field2>.
  ENDIF.
  IF NOT c_list_field3 IS INITIAL.
    ASSIGN COMPONENT c_list_field3
           OF STRUCTURE glu1 TO <glu1_field3>.
    ASSIGN COMPONENT c_list_field3
           OF STRUCTURE s_list_glu1 TO <s_list_glu1_field3>.
  ENDIF.
*Now amounts and quantities
  LOOP AT t_keyfig.
    CASE sy-tabix.
      WHEN 01.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig01>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig01>.
      WHEN 02.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig02>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig02>.
      WHEN 03.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig03>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig03>.
      WHEN 04.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig04>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig04>.
      WHEN 05.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig05>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig05>.
      WHEN 06.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig06>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig06>.
      WHEN 07.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig07>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig07>.
      WHEN 08.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig08>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig08>.
      WHEN 09.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig09>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig09>.
      WHEN 10.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE glu1 TO <glu1_keyfig10>.
        ASSIGN COMPONENT t_keyfig-keyfig
               OF STRUCTURE s_list_glu1 TO <s_list_glu1_keyfig10>.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "global_assigns

*---------------------------------------------------------------------*
*       FORM set_list_mode                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  slm_list_mode                                                 *
*---------------------------------------------------------------------*
FORM set_list_fields.
  IF xlist IS INITIAL.
    c_list_mode = '0'.
  ELSE.
    c_list_mode = '1'.
  ENDIF.
  IF c_list_mode = '1'.
    c_list_field1 = p_field1.
    c_list_field2 = p_field2.
    c_list_field3 = p_field3.
  ENDIF.
ENDFORM.                    "set_list_fields
*&---------------------------------------------------------------------*
*&      Form  DELETE_PERIOD_ZERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_period_zero USING dpz_orgunit.
  DATA: t_selection TYPE gusl_t_selection.
  DATA:   h_cursor TYPE gusl_cursor,
          h_subrc LIKE sy-subrc.
*Internal table for GLU1
  DATA: t_glu1 TYPE gusl_t_glu1.
  DATA: s_glu1 TYPE gusl_s_glu1.

*Reset only if no test run
  CHECK xtest IS INITIAL.

*Clear all internal tables
  PERFORM clear_update_tables.
  PERFORM clear_bs_pl_tables.

*Where clause
  PERFORM fill_selection_table TABLES t_selection
                               USING h_800a
                                     dpz_orgunit
                                     'D'.
*Open cursor
  CALL FUNCTION 'G_TABLE_OPEN_CURSOR'
    EXPORTING
      i_tabname   = h_800a-tab
      i_selection = t_selection
      i_with_hold = 'X'
    IMPORTING
      e_cursor    = h_cursor.
*Get the data into physical table
  h_subrc = 0.
  WHILE h_subrc = 0.
    REFRESH t_glu1.
    CALL FUNCTION 'G_TABLE_FETCH'
      EXPORTING
        i_cursor       = h_cursor
        i_package_size = c_package_size_delete
      CHANGING
        c_t_glu1       = t_glu1
      EXCEPTIONS
        no_more_data   = 1.
    h_subrc = sy-subrc.

*Inversion of period 0 and storing in internal table
    IF h_subrc = 0.
      LOOP AT t_glu1 INTO s_glu1.
        s_glu1-tsl = s_glu1-tsl * -1.
        s_glu1-hsl = s_glu1-hsl * -1.
        s_glu1-ksl = s_glu1-ksl * -1.
        s_glu1-osl = s_glu1-osl * -1.
        s_glu1-msl = s_glu1-msl * -1.
        s_glu1-asl = s_glu1-asl * -1.
        PERFORM collect_t_bs USING s_glu1
                                   h_800a-tab.
      ENDLOOP.
      CALL FUNCTION 'DB_COMMIT'.
    ENDIF.
  ENDWHILE.

*Update of the internal table
*Initialize commit counter
  CLEAR commit_count.
*Set mode to 'D' (Delete)
  g_mode = 'D'.
*Loop at balance sheet accounts
  PERFORM loop_t_bs USING h_800a-tab.
*Post records from update_tables T_xxx for last records
  PERFORM update_db.
  PERFORM clear_update_tables.
*Commit work at the end of processing
  COMMIT WORK.
ENDFORM.                               " DELETE_PERIOD_ZERO

*&---------------------------------------------------------------------*
*&      Form  select_records
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_records USING sr_orgunit.
  DATA: t_selection TYPE gusl_t_selection.
  DATA:   h_cursor TYPE gusl_cursor,
          h_subrc LIKE sy-subrc.
*Internal table for GLU1
  DATA: t_glu1 TYPE gusl_t_glu1.
  DATA: s_glu1 TYPE gusl_s_glu1.

*Clear all internal tables
  PERFORM clear_update_tables.
  PERFORM clear_bs_pl_tables.

*Where clause
  PERFORM fill_selection_table TABLES t_selection
                               USING h_800a
                                     sr_orgunit
                                     'E'.
*Open cursor
*(I_PERIOD_AGGREGATION must BE 'G': For each RPMAX a sum is given back
*in POPER. Since POPER is no key field in the internal collect tables
*T_BS_xxx and T_PL_xxx the collect works.
*I_CUMULATED must be SPACE, otherwise an order by primary key would be
*done which kills the performance)
  CALL FUNCTION 'G_TABLE_OPEN_CURSOR'
    EXPORTING
      i_tabname            = h_800a-tab
      i_selection          = t_selection
      i_period_aggregation = 'G' "GROUP period aggregation
      i_cumulated          = ' ' "Cumulate values
      i_with_hold          = 'X'
    IMPORTING
      e_cursor             = h_cursor.
*Get the data into physical table
  h_subrc = 0.
  WHILE h_subrc = 0.
    REFRESH t_glu1.
    CALL FUNCTION 'G_TABLE_FETCH'
      EXPORTING
        i_cursor       = h_cursor
        i_package_size = c_package_size_select
      CHANGING
        c_t_glu1       = t_glu1
      EXCEPTIONS
        no_more_data   = 1.
    h_subrc = sy-subrc.
*Process record further
    IF h_subrc = 0.
      LOOP AT t_glu1 INTO s_glu1.
        PERFORM process_record USING s_glu1
                                     h_881
                                     h_800a.
      ENDLOOP.
    ENDIF.
  ENDWHILE.
  COMMIT WORK.
ENDFORM.                               " DELETE_PERIOD_ZERO

*&---------------------------------------------------------------------*
*&      Form  FILL_SELECTION_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_SELECTION  text
*      -->fst_mode       'D': Delete, 'E' = Execute
*----------------------------------------------------------------------*
FORM fill_selection_table TABLES fst_t_selection TYPE gusl_t_selection
                          USING  h_800a LIKE t800a
                                 fst_orgunit
                                 fst_mode.
  DATA: h_range TYPE gusl_s_range.
  DATA: h_t009 LIKE t009.

*Ledger
  CLEAR: h_range, fst_t_selection-t_range[].
  fst_t_selection-fieldname = 'RLDNR'.
  h_range-sign = 'I'.
  h_range-option = 'EQ'.
  h_range-low = ledger.
  h_range-high = ledger.
  APPEND h_range TO fst_t_selection-t_range.
  APPEND fst_t_selection.

*Organizational unit
  CLEAR: h_range, fst_t_selection-t_range[].
  fst_t_selection-fieldname = h_800a-dim_orgunit.
  h_range-sign = 'I'.
  h_range-option = 'EQ'.
  h_range-low = fst_orgunit.
  h_range-high = fst_orgunit.
  APPEND h_range TO fst_t_selection-t_range.
  APPEND fst_t_selection.
*Record type
  CLEAR: h_range, fst_t_selection-t_range[].
  fst_t_selection-fieldname = 'RRCTY'.
  LOOP AT satztyp.
    MOVE-CORRESPONDING satztyp TO h_range.
    APPEND h_range TO fst_t_selection-t_range.
  ENDLOOP.
  IF sy-subrc = 0.
    APPEND fst_t_selection.
  ENDIF.
*Version
  CLEAR: h_range, fst_t_selection-t_range[].
  fst_t_selection-fieldname = 'RVERS'.
  LOOP AT version.
    MOVE-CORRESPONDING version TO h_range.
    APPEND h_range TO fst_t_selection-t_range.
  ENDLOOP.
  IF sy-subrc = 0.
    APPEND fst_t_selection.
  ENDIF.
*Fiscal year
  CLEAR: h_range, fst_t_selection-t_range[].
  fst_t_selection-fieldname = 'RYEAR'.
  h_range-sign = 'I'.
  h_range-option = 'EQ'.
  IF fst_mode = 'D'.
    h_range-low = newjr.
    h_range-high = newjr.
  ELSE.
    h_range-low = oldjr.
    h_range-high = oldjr.
  ENDIF.
  APPEND h_range TO fst_t_selection-t_range.
  APPEND fst_t_selection.
*Posting period
  IF fst_mode = 'D'.
    CLEAR: h_range, fst_t_selection-t_range[].
    fst_t_selection-fieldname = 'POPER'.
    h_range-sign = 'I'.
    h_range-option = 'EQ'.
    h_range-low = '000'.
    h_range-high = '000'.
    APPEND h_range TO fst_t_selection-t_range.
    APPEND fst_t_selection.
  ENDIF.
*Account (only if header line of t_account_range is filled)
  IF fst_mode = 'E' AND NOT t_account_range IS INITIAL.
    CLEAR: h_range, fst_t_selection-t_range[].
    fst_t_selection-fieldname = h_800a-dim_account.
    h_range-sign = 'I'.
    h_range-option = 'BT'.
    h_range-low = t_account_range-min.
    h_range-high = t_account_range-max.
    APPEND h_range TO fst_t_selection-t_range.
    APPEND fst_t_selection.
  ENDIF.

ENDFORM.                               " FILL_SELECTION_TABLE

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDLIST_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_SELECTION  text
*      -->fst_mode       'D': Delete, 'E' = Execute
*----------------------------------------------------------------------*
FORM fill_fieldlist_table TABLES fft_t_fields TYPE gusl_t_fields
                          USING  h_800a LIKE t800a
                                 fst_mode.
  DATA: t_fieldtab LIKE dfies OCCURS 0 WITH HEADER LINE.

*If period 0 is reset for new year only period 0 has to be selected
  IF fst_mode = 'D'.
    CALL FUNCTION 'G_FIELDTAB_GET'
      EXPORTING
        table                 = h_800a-tab
        no_glx_obj_processing = 'X'
      TABLES
        t_fieldtab            = t_fieldtab.
    LOOP AT t_fieldtab.
      APPEND t_fieldtab-fieldname TO fft_t_fields.
    ENDLOOP.
    LOOP AT t_t800a_keyfig.
      APPEND t_t800a_keyfig-keyfig TO fft_t_fields.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "FILL_fieldlist_TABLE

*---------------------------------------------------------------------*
*       FORM get_accounts                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  t_ska1                                                        *
*  -->  ga_bukgestab                                                  *
*---------------------------------------------------------------------*
FORM get_accounts USING ga_bukgestab LIKE bukgestab.
  DATA: ivalue LIKE gmd04 OCCURS 1 WITH HEADER LINE,
        igmd00 LIKE gmd00 OCCURS 0 WITH HEADER LINE,
        igmdtt LIKE gmdtt OCCURS 0 WITH HEADER LINE,
        period LIKE periods OCCURS 1 WITH HEADER LINE.
  DATA: u_fill_min LIKE gmd04-from,
        u_fill_max LIKE gmd04-to.
  DATA: BEGIN OF u_fill_hexff,
          x1(1) TYPE x VALUE 'FF',
        END OF u_fill_hexff.
  DATA: BEGIN OF u_fill_hex00,
          x1(1) TYPE x VALUE '00',
        END OF u_fill_hex00.
  DATA: u_fill_max_length TYPE i,
        u_fill_offset TYPE i.
  FIELD-SYMBOLS: <orgunit>.
  DATA: work(30).

  CLASS cl_abap_char_utilities DEFINITION LOAD.

*Process only account ranges if required
  CHECK xacct NE space.

*Initialize global internal accounts table
  CLEAR t_accounts. REFRESH t_accounts.
  CLEAR t_account_range. REFRESH t_account_range.

*First get lowest and highest values
  CLEAR u_fill_min
        WITH cl_abap_char_utilities=>minchar
        IN CHARACTER MODE.
  CLEAR u_fill_max
        WITH cl_abap_char_utilities=>maxchar
        IN CHARACTER MODE.

*Set superior field
  CLEAR glu1.
  CONCATENATE 'GLU1' '-' h_800a-dim_orgunit INTO work.
  ASSIGN (work) TO <orgunit>.
  <orgunit> = bukgestab-bukges.

* Fill processed ledger
  MOVE ledger TO glu1-rldnr.

*Fill from and to values for g_read_area_tab
  MOVE: space      TO ivalue-from,
        u_fill_max TO ivalue-to.
  APPEND ivalue.

*Fill period
  CALL FUNCTION 'G_PERIODS_OF_YEAR_GET'
    EXPORTING
      variant   = bukgestab-periv
      year      = oldjr
    TABLES
      i_periods = period
    EXCEPTIONS
      OTHERS    = 1.

*Read all accounts
  CALL FUNCTION 'G_READ_AREA_TAB'
    EXPORTING
      activity                      = ' '
      check_typ                     = '1'
      field                         = h_800a-dim_account
      langu                         = sy-langu
      record_key                    = glu1
      t800d_table                   = h_800a-tab
      table                         = 'GLU1'
      text_flag                     = ' '
      allow_initvalue_without_check = ' '
    TABLES
      igmd00                        = igmd00
      igmdtt                        = igmdtt
      ivalue                        = ivalue
      period                        = period
    EXCEPTIONS
      not_found                     = 1
      OTHERS                        = 2.

  IF sy-subrc = 0.
    LOOP AT igmd00.
      t_accounts-saknr = igmd00-value.
      collect T_ACCOUNTS.
    ENDLOOP.
    SORT t_accounts.
  ELSE.
    IF h_800a-comptab IS INITIAL.
      MESSAGE i630 WITH text-310 bukgestab-bukges
                        h_800a-dim_account h_800a-tab.
    ELSE.
      MESSAGE i630 WITH text-311 bukgestab-bukges
                        h_800a-dim_account h_800a-tab.
    ENDIF.
  ENDIF.
*Clear GLU1 for later processing (e.g. select_records)
  CLEAR glu1.
ENDFORM.                    "get_accounts

*---------------------------------------------------------------------*
*       FORM get_range_of_accounts                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_range_of_accounts.
  DATA: h_counter TYPE i.
  DATA: h_new_record.
*How many entries are stored in t_accounts?
  DESCRIBE TABLE t_accounts LINES sy-tfill.
  CHECK sy-tfill NE 0.
*Now build ranges
  CLEAR t_account_range. REFRESH t_account_range.
  t_account_range-min = space.         "Lowest account possible
  LOOP AT t_accounts.
    ADD 1 TO h_counter.
    IF h_counter GE c_max_accounts.
      h_counter = 0.
      t_account_range-max = t_accounts-saknr.
      APPEND t_account_range.
      h_new_record = 'X'.
    ELSE.
      IF h_new_record = 'X'.
        t_account_range-min = t_accounts-saknr.
        CLEAR h_new_record.
      ENDIF.
    ENDIF.
  ENDLOOP.
  if h_new_record is initial.
    t_account_range-max = t_accounts-saknr.
    APPEND t_account_range.
  endif.
ENDFORM.                    "get_range_of_accounts

*---------------------------------------------------------------------*
*       FORM get_next_account_entry                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  gnae_index                                                    *
*  -->  gnae_select_flag                                              *
*---------------------------------------------------------------------*
FORM get_next_account_entry USING gnae_index
                                  gnae_select_flag.
  READ TABLE t_account_range INDEX gnae_index.
  IF sy-subrc = 0.
    gnae_select_flag = 'X'.
  ELSE.
    CLEAR gnae_select_flag.
  ENDIF.
ENDFORM.                    "get_next_account_entry

*---------------------------------------------------------------------*
*       FORM process_record                                       *
*---------------------------------------------------------------------*
*       Process the selected record and store them in internal        *
*       line item table                                               *
*---------------------------------------------------------------------*
FORM process_record USING pr_glu1 LIKE glu1
                          pr_881  LIKE t881
                          pr_800a LIKE t800a.
* Logic for creating the record in the new year:
*    1) *GLU1 contains the selected record
*    2)  GLU1 contains at the end the new record
   *glu1 = pr_glu1.                    "Save selected record

*Fill fix fields (RACCT,Orgunit,BUKGES,RYEAR)
  CLEAR akt.
  MOVE-CORRESPONDING *glu1 TO akt.
  akt-bukges = <*glu1_orgunit>.
  akt-racct  = <*glu1_account>.

*Read chart of account for orgunit
  READ TABLE bukgestab WITH KEY bukges = akt-bukges.
  IF sy-subrc NE 0.                    "BUKRS/Gesnr nicht in T001/T880
    CHECK 1 = 2.                "satz nicht weiter verarbeiten->Ende
  ENDIF.

* ------ Find account classification with Ktopl -> XBILK ------------
* ------ Process only accounts that have to be carried forward---------
  PERFORM check_konto.                 "SKA1 lesen, evtl. manipulieren

  IF xbilk NE '*'.                     "Konto zum Vortragen ?
* Fill GLU1 according to field movement for P&L accounts
* Fill GLU1 out of *GLU1 for BS accounts

    CLEAR glu1.                        "Vortrags-Satz initialisieren
    CLEAR xglu1.                       "GLU1-Satz ist leer

*For PL accounts fill retained earnings account first so that
*it can be manipulated later in an user exit
*Attention: The manipulated account must also be defined in the
*chart of accounts
    IF xbilk = ' '.                    "PL account
      LOOP AT guvtab WHERE ktopl = ska1-ktopl
                     AND   gvtyp = ska1-gvtyp.
        <glu1_account> = guvtab-vtkon. "Fix account
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.                "Fix-account not in orgunit
        mittab-racct  = <*glu1_account>.
        mittab-bukges = akt-bukges.
        mittab-ktopl  = ska1-ktopl.
        mittab-gvtyp  = ska1-gvtyp.
        mittab-text1  = text-034.      "A: VT/GVTYP nicht in T030
        mittab-text2  = text-134.
        COLLECT mittab.
        guvtab-vtkon = '*'.            "Satz nicht verarbeiten
      ENDIF.

*        IF BS account is a fix account- > EXTR-XBILK=' '
*        then treat it like an PL account
    ELSE.
      IF <*glu1_account> NE ' '.
        CLEAR guvtab.
        LOOP AT guvtab WHERE ktopl = bukgestab-ktopl
                       AND   vtkon = <*glu1_account>.
          xbilk = ' '.
          <glu1_account> = <*glu1_account>.
        ENDLOOP.
      ENDIF.
    ENDIF.

* ------ - No FMOD -> standard field movement from T800M --------------
* ---------- PL-account -----------------------------------------------
    IF xbilk    = ' '  AND             "G+V-Konto
       fmod-guv = ' '.                 "keine FMOD

      wf-rc  = 4.                      "zurück 0, wenn Form gefunden
      PERFORM e01_movements(rgimf000)                       "Tab.800M
                            USING 'GLU1' 'GLU1' led_par wf-rc.
      IF wf-rc  > 0.
        PERFORM log_almsg_var USING 'GI' 'E' '177'
                                    'GLU1-GLU1' sy-mandt ' ' ' '.
        MESSAGE e177(gi) WITH 'GLU1-GLU1'  sy-mandt.
      ENDIF.
      PERFORM check_fehler_movements USING xerr_mov.
      IF xerr_mov = 'X'.
        EXIT.                          "Satz nicht verarbeiten
      ENDIF.
      xglu1 = 'X'.                     "GLU1 wurde gefüllt
    ENDIF.

* ------ - No FMOD -> standard field movement
* ---------- BS account   ---------------------------------------------
    IF xbilk   NE ' '  AND             "Bilanz-Konto
       fmod-bil = ' '.                 "keine FMOD
      glu1 = *glu1.                    "alle Kontierungen übertragen
      xglu1 = 'X'.                     "GLU1 wurde gefüllt
    ENDIF.

* ------ - Movement acc. FMOD from *GLU1 to GLU1 for BS/PL
* ------  . FMOD from T800M - fix movements
    IF xglu1 = ' '.                    "GLU1 ist noch nicht gefüllt
      wf-rc  = 4.                      "zurück 0, wenn Form gefunden
      PERFORM e01_movements(rgimf000)
                            USING 'GLU1' 'GLU1' led_par wf-rc.
      IF wf-rc  > 0.
        PERFORM log_almsg_var USING 'GI' 'E' '177'
                                    'GLU1-GLU1' sy-mandt ' ' ' '.
        MESSAGE e177(gi) WITH 'GLU1-GLU1'  sy-mandt.
      ENDIF.
      PERFORM check_fehler_movements USING xerr_mov.
      IF xerr_mov = 'X'.
        EXIT.                          "Satz nicht verarbeiten
      ENDIF.

* ------  . FMOD from T888M - variable field movements -----------------
      IF xbilk = space.
        fmod-akt = fmod-guv.           "für Fehlermeldung
        CONCATENATE 'MODIF' '_' fmod-guv '_' sy-mandt INTO wf-form.
      ELSE.
        fmod-akt = fmod-bil.           "für Fehlermeldung
        CONCATENATE 'MODIF' '_' fmod-bil '_' sy-mandt INTO wf-form.
      ENDIF.

      wf-rc  = 4.                      "zurück 0, wenn Form gefunden
      PERFORM (wf-form) IN PROGRAM sapfgimv                 "Tab.888M
                           USING wf-rc   led_par IF FOUND.
      IF wf-rc  > 0.
        PERFORM log_almsg_var USING 'GI' 'E' '168'
                                    fmod-akt  sy-mandt ' ' ' '.
        MESSAGE e168(gi) WITH fmod-akt  sy-mandt.
      ENDIF.
      PERFORM check_fehler_movements USING xerr_mov.
      IF xerr_mov = 'X'.
        EXIT.                          "Satz nicht verarbeiten
      ENDIF.

    ENDIF.

*Set new year and period 0, initialize DRCRK
    CLEAR glu1-drcrk.
    glu1-ryear = newjr.
    glu1-poper = '000'.

*Process record only if no errors have occurred
    CHECK guvtab-vtkon NE '*'.

* Process retained earnings account with transaction currency?
    IF xbilk = ' '.                    "G+V- oder Vortragskonto
      CLEAR xtw.
      LOOP AT vtwtab  WHERE bukrs = akt-bukges
                      AND   vtkon = guvtab-vtkon.
        xtw = 'X'.                     "vortragen mit TWAER
        EXIT.
      ENDLOOP.
      IF xtw   = space.
        glu1-tsl   = glu1-hsl.         "Vortragsktn. ohne TWAER
        glu1-rtcur = bukgestab-hwaer.
      ENDIF.
    ENDIF.

*Clear currency fields if ledger doesn't lead them
*!!!Must not be done, because customizing of T881 for parallel ledgers
*of table GLT0 is not correct!!!!
*   if h_881-trcur is initial.
*     clear glu1-rtcur. clear glu1-tsl.
*   endif.
*   if h_881-lccur is initial.
*     clear glu1-hsl.
*   endif.
*   if h_881-rccur is initial.
*     clear glu1-ksl.
*   endif.
*   if h_881-occur is initial.
*     clear glu1-osl.
*   endif.
*   if h_881-quant is initial.
*     clear glu1-msl.
*   endif.
*   if h_881-atqnt is initial.
*     clear glu1-asl.
*   endif.

*Clear GLU1-LOGSYS if GLU1-LOGSYS is the own LOGSYS
    IF NOT glu1-logsys IS INITIAL.
      PERFORM check_and_clear_logsys USING glu1-logsys.
    ENDIF.
*Now store this GLU1 record in the internal tables
*1) BS-accounts: Append into T_BS_xxxxxx
*2) PL-accounts: COLLECT into T_PL_xxxxxx
    IF xbilk = 'X'.
      PERFORM glu1_to_list_table USING 'X'.
      PERFORM collect_t_bs USING glu1
                                 pr_800a-tab.
    ELSE.
      IF xtrac IS INITIAL.
* Sonderfall: Bestandsledger 3I mit Mengen vorgetragen      "829888
        IF glu1-rldnr NE '3I'.                              "829888
          CLEAR: glu1-msl, glu1-runit,
                 glu1-asl, glu1-aunit.
        ENDIF.                                              "829888
      ENDIF.
      PERFORM glu1_to_list_table USING ' '.
      IF glu1-rldnr NE '3I'.                                "829888
        CLEAR: glu1-msl, glu1-runit,
               glu1-asl, glu1-aunit.
      ENDIF.                                                "829888
      PERFORM collect_t_pl USING glu1
                                 pr_800a-tab.
    ENDIF.

*Account not defined and therefore not processed further
  ELSE.
    mittab-racct  = <*glu1_account>.
    mittab-bukges = akt-bukges.
    mittab-ktopl  = ska1-ktopl.
    mittab-gvtyp  = space.
    mittab-text1  = text-035.          "Account not defined
    mittab-text2  = text-115.
    COLLECT mittab.
  ENDIF.
ENDFORM.                    "process_record

*eject
*---------------------------------------------------------------------*
*       FORM CHECK_KONTO
*---------------------------------------------------------------------*
*       Im Sachkontenstammsatz ueberpruefen, ob das Konto ein
*       Bilanz-, G+V- oder gar kein Konto zum Vortragen ist.
*       Ansprung eines User-Exits: evtl. Manipulation von XBILK,
*                                  SKA1-XBILK, SKA1-KTOPL.
*       --> *GLU1      = gefüllt mit aktuellem Ledger-Satz
*       --> AKT-RACCT  = Konto
*       --> BUKGESTAB-KTOPL = gültiger Kontoplan für akt. Bukges
*       <-- SKA1 : Stammsatz auf Ktopl-Ebene (GVTYP, ...)
*       <-- XBILK: 'X' = Bilanz-Konto (auch, wenn Konto=' ')
*                  ' ' = G+V-Konto
*                  '*' = kein Konto zum Vortragen (= RK)
*---------------------------------------------------------------------*
FORM check_konto.

  CLEAR ska1.
  IF akt-racct NE ' '.                 "Konto gefuellt ?
    CALL FUNCTION 'READ_KONTENPLAN'
      EXPORTING
        kontenplan           = bukgestab-ktopl
        sachkonto            = akt-racct
      IMPORTING
        kontenplan_wa        = ska1
      EXCEPTIONS
        kontenplan_not_found = 1
        OTHERS               = 2.
    CASE sy-subrc.
      WHEN  0.
        xbilk = ska1-xbilk.            "Konto für SAVOR
      WHEN  1.
        xbilk = '*'.                   "kein SAVOR-Konto (evtl.RK)
      WHEN OTHERS.
        xbilk = '*'.                   "fehler
    ENDCASE.
  ELSE.
    xbilk = 'X'.                       "Konto leer -> wie Bilanzkto.
  ENDIF.

* User-Exit (Rahmen) für evtl. Änderung der Konto-Merkmale aufrufen
  CALL FUNCTION 'CALL_EXIT_SAPLGVTR_001'
    EXPORTING
      ledger_data       = *glu1
    CHANGING
      account_indicator = xbilk
      account_type      = ska1-gvtyp
      chart_of_accounts = ska1-ktopl.

ENDFORM.                    "CHECK_KONTO

*eject
*---------------------------------------------------------------------*
*       FORM CHECK_VORTRAGSKONTO
*---------------------------------------------------------------------*
*       Im Sachkontenstammsatz SKA1 ueberpruefen, ob das  Konto
*       im Ktopl angelegt ist.
*       --> Bukrs, Ktopl, GuVTyp, Vortragskonto, T882-ALTSV
*       <-- SKA1
*       <-- Vortragskonto = '*' bei Fehler
*       <-- MITTAB         Fehler-Tab.
*---------------------------------------------------------------------*
FORM check_vortragskonto USING  cv_bukrs
                                cv_ktopl
                                cv_komok
                                cv_konto
                                cv_altsv.

  CALL FUNCTION 'READ_KONTENPLAN'
    EXPORTING
      kontenplan           = cv_ktopl
      sachkonto            = cv_konto
    IMPORTING
      kontenplan_wa        = ska1
    EXCEPTIONS
      kontenplan_not_found = 1
      OTHERS               = 2.

  CASE sy-subrc.
    WHEN  0.
    WHEN  1.
      mittab-racct  = cv_konto.
      mittab-bukges = cv_bukrs.
      mittab-ktopl  = cv_ktopl.
      mittab-gvtyp  = cv_komok.
      IF cv_altsv = space.
        mittab-text1  = text-007.
        mittab-text2  = text-107.
      ENDIF.
      IF cv_altsv = '1'.
        mittab-text1  = text-031.
        mittab-text2  = text-107.
      ENDIF.
      IF cv_altsv = '2'.
        mittab-text1  = text-032.
        mittab-text2  = text-107.
      ENDIF.
      IF cv_altsv = '9'.
        mittab-text1  = text-033.
        mittab-text2  = text-107.
      ENDIF.
      APPEND mittab.
      guvtab-vtkon = '*'.
    WHEN OTHERS.
      mittab-racct  = cv_konto.
      mittab-bukges = cv_bukrs.
      mittab-ktopl  = cv_ktopl.
      mittab-gvtyp  = cv_komok.
      mittab-text1  = text-028.
      APPEND mittab.
      guvtab-vtkon = '*'.
  ENDCASE.

ENDFORM.                    "CHECK_VORTRAGSKONTO

*eject
*---------------------------------------------------------------------*
*       FORM LOOP                                                     *
*---------------------------------------------------------------------*
*       Loop at internal tables T_BS_xxx and T_PL_xxx                 *
*---------------------------------------------------------------------*
FORM loop.
*Initialize commit counter
  CLEAR commit_count.
*Set mode to 'E' (Execute)
  g_mode = 'E'.
*Loop at balance sheet accounts
  clear glu1.
  PERFORM LOOP_T_BS USING H_800A-TAB.
*Loop at profit and loss accounts
  clear glu1.
  PERFORM loop_t_pl USING h_800a-tab.
ENDFORM.                    "LOOP

*---------------------------------------------------------------------*
*       FORM record_end_process                                       *
*---------------------------------------------------------------------*
*       ...                                                      *
*---------------------------------------------------------------------*
*  -->  rep_glu1                                                      *
*  -->  rep_xbilk
*  -->  rep_mode    'D' Löschen 'E' Execute
*---------------------------------------------------------------------*
FORM record_end_process USING rep_glu1 LIKE glu1
                              rep_xbilk.

*Process GLU1
  glu1 = rep_glu1.

*In Update mode DRCRK has to be set and additional flags have to be set
  IF g_mode = 'E'.
*Process only records ne zero and set DRCRK
    CHECK glu1-tsl NE 0
       OR glu1-hsl NE 0
       OR glu1-ksl NE 0
       OR glu1-osl NE 0
       OR glu1-msl NE 0
       OR glu1-asl NE 0.
    IF h_881-shkz IS INITIAL.
      CLEAR glu1-drcrk.
    ELSEIF glu1-tsl > 0.
      glu1-drcrk = 'S'.
    ELSEIF glu1-tsl < 0.
      glu1-drcrk = 'H'.
    ELSEIF glu1-hsl > 0.
      glu1-drcrk = 'S'.
    ELSEIF glu1-hsl < 0.
      glu1-drcrk = 'H'.
    ELSEIF glu1-ksl > 0.
      glu1-drcrk = 'S'.
    ELSEIF glu1-ksl < 0.
      glu1-drcrk = 'H'.
    ELSEIF glu1-osl > 0.
      glu1-drcrk = 'S'.
    ELSEIF glu1-osl < 0.
      glu1-drcrk = 'H'.
    ELSEIF glu1-msl > 0.
      glu1-drcrk = 'S'.
    ELSEIF glu1-msl < 0.
      glu1-drcrk = 'H'.
    ELSEIF glu1-asl > 0.
      glu1-drcrk = 'S'.
    ELSEIF glu1-asl < 0.
      glu1-drcrk = 'H'.
    ENDIF.

*Set flag that records have been processed
    xkont = 'X'.
*Set flag that records have to be updated
    xchng = 'X'.

* Store fix fields in AKT
    CLEAR akt.
    MOVE-CORRESPONDING glu1 TO akt.
    akt-bukges = <glu1_orgunit>.
    akt-racct  = <glu1_account>.
  ENDIF.

*Update posting tables
  IF xtest =  space.                   "Echtlauf
    if gd_write_si = 'X'.
      CALL FUNCTION 'G_FILL_SI_FIELDS_FOR_BCF'
        CHANGING
          CS_GLU1       = glu1.
    endif.
    PERFORM update_tables_savor USING h_800a-tab.
    ADD 1 TO commit_count.
*After commit_count records call update task
    IF commit_count GE c_max_commit.
      CLEAR commit_count.
*Post records from update_tables T_xxx
      PERFORM update_db.
      PERFORM clear_update_tables.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    "record_end_process

*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_CLEAR_LOGSYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GLU1_LOGSYS  text
*----------------------------------------------------------------------*
FORM check_and_clear_logsys USING cacl_logsys.
  STATICS: c_logsys LIKE glu1-logsys,
           c_subrc LIKE sy-subrc.
  CHECK c_subrc IS INITIAL.
  IF c_logsys IS INITIAL.
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = c_logsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.
    c_subrc = sy-subrc.
  ENDIF.
  CHECK c_subrc = 0 AND c_logsys NE space.
  IF cacl_logsys = c_logsys.
    CLEAR cacl_logsys.
  ENDIF.
ENDFORM.                               " CHECK_AND_CLEAR_LOGSYS

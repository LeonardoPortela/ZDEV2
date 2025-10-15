class ZCL_FAGL_LIB definition
  public
  final
  create public .

public section.
  type-pools ABAP .
  type-pools RSDS .

  interfaces IF_BADI_INTERFACE .
  interfaces IF_FAGL_LIB .

  constants:
    BEGIN OF gc_s_hdb_ddic.
    CONSTANTS   gl_view           TYPE rsldb_name
                                  VALUE 'V_GLPOS_N_CT'.
    CONSTANTS   gl_view_ext       TYPE rsldb_name
                                  VALUE 'V_GLPOS_N_GL_CT'.
    CONSTANTS   entry_view        TYPE rsldb_name
                                  VALUE 'V_GLPOS_C_CT'.
    CONSTANTS   gl_line_items     TYPE rsldb_name
                                  VALUE 'V_GLPOS_C_GL_CT'.
    CONSTANTS   ar_line_items     TYPE rsldb_name
                                  VALUE 'V_GLPOS_C_DEB_CT'.
    CONSTANTS   ap_line_items     TYPE rsldb_name
                                  VALUE 'V_GLPOS_C_VEN_CT'.
    CONSTANTS   pca_line_items    TYPE rsldb_name
                                  VALUE 'V_GLPCA_CT'.
    CONSTANTS   pca_line_items_ext TYPE rsldb_name
                                  VALUE 'V_GLPCA_T_CT'.
    CONSTANTS END OF gc_s_hdb_ddic .
protected section.
private section.

  types:
    tt_t001                 TYPE STANDARD TABLE OF t001 .

  data MD_ARCHIVE_RELEVANT type BOOLE_D value '-' ##NO_TEXT.
  data MT_DFIES_BKPF type DFIES_TABLE .
  data MT_DFIES_BSEG type DFIES_TABLE .
  data MT_DFIES_INDEX type DFIES_TABLE .
  data MT_DFIES_GLPOS type DFIES_TABLE .
  data MT_DFIES_T001 type DFIES_TABLE .
  data MD_ARCHIVE_OPTION type BOOLE_D value '0' ##NO_TEXT.
  data MT_T001 type TT_T001 .
  data MD_READ_MASTER_DATA type BOOLE_D value 'X' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_FAGL_LIB IMPLEMENTATION.


  METHOD if_fagl_lib~add_fields.
* This method is obsolete. Instead of manipulating the field catalog
* in this BADI method, add it to the DDIC object which the relevant
* view is based on.
  ENDMETHOD.


  method IF_FAGL_LIB~ADD_SECONDARY_FIELDS.
  endmethod.


  method IF_FAGL_LIB~COLUMN_VISIBILITY_CHANGE.
  endmethod.


  METHOD if_fagl_lib~create_default_restrictions.
*----------------------------------------------------------------------*
    DATA lt_dd26v               TYPE STANDARD TABLE OF dd26v.
    DATA ls_dd26v               TYPE dd26v.

    DATA lt_t003                TYPE STANDARD TABLE OF t003.
    DATA ls_t003                TYPE t003.
    DATA ld_brobj               TYPE brobj.
    DATA ld_where               TYPE string.

    DATA lsx_range              TYPE rsds_range.
    DATA lsx_frange             TYPE rsds_frange.
    DATA ls_range               TYPE rsdsselopt.

    DATA lt_ra_blart            TYPE rsdsselopt_t.

    DATA ld_table_range_initial.
*----------------------------------------------------------------------*
    FIELD-SYMBOLS <lsx_range>           TYPE rsds_range.
    FIELD-SYMBOLS <lsx_frange>          TYPE rsds_frange.
*----------------------------------------------------------------------*
    AUTHORITY-CHECK OBJECT 'F_BKPF_BLA'
          ID 'BRGRU' FIELD '*'
          ID 'ACTVT' FIELD '03'.
* User is authorized for all document types
    CHECK sy-subrc > 0.

* Exclude document types based on account type
    CASE id_sel_view.
      WHEN gc_s_hdb_ddic-gl_view
        OR gc_s_hdb_ddic-gl_view_ext
        OR gc_s_hdb_ddic-entry_view
        OR gc_s_hdb_ddic-gl_line_items.
        ld_where = 'XKOAS > SPACE'.
      WHEN gc_s_hdb_ddic-ar_line_items.
        ld_where = 'XKOAD > SPACE'.
      WHEN gc_s_hdb_ddic-ap_line_items.
        ld_where = 'XKOAK > SPACE'.
    ENDCASE.

* Only add restriction to document type for FI line item browsers
    CASE id_sel_view.
      WHEN gc_s_hdb_ddic-gl_view
        OR gc_s_hdb_ddic-gl_view_ext
        OR gc_s_hdb_ddic-entry_view
        OR gc_s_hdb_ddic-gl_line_items
        OR gc_s_hdb_ddic-ar_line_items
        OR gc_s_hdb_ddic-ap_line_items.
* Are restrictions available?
        IF ct_table_range IS NOT INITIAL.
          READ TABLE ct_table_range
            ASSIGNING <lsx_range>
            INDEX 1.
* Are restrictions for document types available?
          READ TABLE <lsx_range>-frange_t
            ASSIGNING <lsx_frange>
            WITH KEY
              fieldname = 'BLART'.
          IF sy-subrc = 0.
            lt_ra_blart = <lsx_frange>-selopt_t.
          ELSE.
            ASSIGN lsx_frange TO <lsx_frange>.
          ENDIF.
        ELSE.
          ASSIGN lsx_range  TO <lsx_range>.
          ASSIGN lsx_frange TO <lsx_frange>.
        ENDIF.

* Get all relevant authorization groups
        SELECT *
          FROM t003
          INTO TABLE lt_t003
          WHERE brgru NE space
          AND (ld_where)
          AND blart IN lt_ra_blart.

* No relevant document types with assigned auth groups
        CHECK lt_t003 IS NOT INITIAL.

        SORT lt_t003.

        ls_range-sign   = 'E'.
        ls_range-option = 'EQ'.
* Check authorization
        LOOP AT lt_t003
          INTO  ls_t003.
          ld_brobj = 'F_BKPF_BLA'.
          AUTHORITY-CHECK OBJECT 'F_BKPF_BLA'
              ID 'BRGRU' FIELD ls_t003-brgru
              ID 'ACTVT' FIELD '03'.
* Exclude document types which user is not authorized for
          IF sy-subrc <> 0.
            ls_range-low = ls_t003-blart.
            COLLECT ls_range
              INTO <lsx_frange>-selopt_t.
          ENDIF.
        ENDLOOP.

* No (additional) restrictions necessary
        CHECK NOT <lsx_frange>-selopt_t IS INITIAL.

* Get table name from view definition
        IF ct_table_range IS INITIAL.
          ld_table_range_initial = abap_true.
          CALL FUNCTION 'DDIF_VIEW_GET'
            EXPORTING
              name      = id_sel_view
            TABLES
              dd26v_tab = lt_dd26v
            EXCEPTIONS
              OTHERS    = 1.

          IF sy-subrc > 0
          OR lines( lt_dd26v ) > 1.
            ls_dd26v-tabname = id_sel_view.
          ELSE.
            READ TABLE lt_dd26v
              INTO ls_dd26v
              INDEX 1.
          ENDIF.
          <lsx_range>-tablename = ls_dd26v-tabname.
        ENDIF.

* Set initial values (in case there was no restriction for document types
        IF <lsx_frange>-fieldname IS INITIAL.
          <lsx_frange>-fieldname = 'BLART'.
        ENDIF.
        IF <lsx_frange>-selopt_t <> lt_ra_blart.
          APPEND <lsx_frange>
            TO <lsx_range>-frange_t.
        ENDIF.

        IF ld_table_range_initial = abap_true.
          APPEND <lsx_range>
            TO ct_table_range.
        ENDIF.

        RETURN.

      WHEN OTHERS.
    ENDCASE.




  ENDMETHOD.


  method IF_FAGL_LIB~MAIN_TOOLBAR_HANDLE.
  endmethod.


  method IF_FAGL_LIB~MAIN_TOOLBAR_HANDLE_MENU.

  endmethod.


  method IF_FAGL_LIB~MAIN_TOOLBAR_SET.
  endmethod.


  METHOD if_fagl_lib~modify_rri_restrictions.
*----------------------------------------------------------------------*
    DATA ls_sel                   TYPE rstisel.
    DATA ls_view_field            TYPE dfies.
    DATA ls_range                 TYPE rsdsselopt.
    DATA lsx_trange               TYPE rsds_range.
    DATA lsx_frange               TYPE rsds_frange.
*----------------------------------------------------------------------*
    FIELD-SYMBOLS <ls_range>      TYPE any.
    FIELD-SYMBOLS <lt_range>      TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ld_value>      TYPE any.
*----------------------------------------------------------------------*
    FIELD-SYMBOLS <lsx_trange>    TYPE rsds_range.
    FIELD-SYMBOLS <lsx_frange>    TYPE rsds_frange.
*----------------------------------------------------------------------*
* Processing depending on receiving report (if necessary)
    CASE is_receiver-ronam.
* Transaction code was assigned
      WHEN 'FAGLL03H'
* Report was assigned
        OR 'FAGL_LINE_ITEM_BROWSER'.
* Processing depending on calling report (if necessary)
        CASE is_sender-ronam.
          WHEN 'RFBILA00'
            OR 'RFBILA00N'
            OR 'FC10'
            OR 'FC10N'.
* Implement logic here as required
        ENDCASE.
    ENDCASE.

* Restriction table is empty >> set it up with a first entry.
* In the standard this table may contain additional restrictions entered
* on the selection screen already.
    IF ctx_table_range IS INITIAL.
      READ TABLE it_view_fields
        INTO ls_view_field
        INDEX 1.
      lsx_trange-tablename = ls_view_field-tabname.
      APPEND lsx_trange
        TO ctx_table_range.
      READ TABLE ctx_table_range
        ASSIGNING <lsx_trange>
        INDEX 1.
      ASSIGN lsx_frange TO <lsx_frange>.
    ENDIF.

* Work on the selections passed from the sender application.
* This part of the implementation may need to happen depending on
* sender <-> receiver in the case structure above.
    LOOP AT ct_selections_sender
      INTO ls_sel.
* Processing depending on field name
      CASE ls_sel-field.
* Sender report uses field name FKBER
        WHEN 'FKBER'.
* Map to receiver field FKBER_LONG
          READ TABLE <lsx_trange>-frange_t
            ASSIGNING <lsx_frange>
            WITH KEY
              fieldname = 'FKBER_LONG'.
* There are no restrictions for FKBER_LONG yet
          IF sy-subrc <> 0.
* Create an entry for restrictions of FKBER_LONG
            CLEAR lsx_frange.
            lsx_frange-fieldname = 'FKBER_LONG'.
            APPEND lsx_frange TO <lsx_trange>-frange_t.
            READ TABLE <lsx_trange>-frange_t
              ASSIGNING <lsx_frange>
              INDEX sy-tabix.
          ENDIF.
* Transfer sender application restrictions to range structure
          MOVE-CORRESPONDING ls_sel TO ls_range.
* Add restriction to restrictions to be applied
          COLLECT ls_range
            INTO <lsx_frange>-selopt_t.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_fagl_lib~select_data.
*----------------------------------------------------------------------*
    DATA lt_sel                   TYPE STANDARD TABLE OF rsparams.
    DATA ls_sel                   TYPE rsparams.
    DATA ls_range                 TYPE rsdsselopt.
    DATA lt_ra_ryear              TYPE rsdsselopt_t.
    DATA lt_ra_period             TYPE rsdsselopt_t.
    DATA lt_ra_pdate              TYPE rsdsselopt_t.
    DATA lt_ra_cdate              TYPE rsdsselopt_t.
    DATA lt_ra_xopvw              TYPE rsdsselopt_t.
    DATA lt_ra_ccode              TYPE rsdsselopt_t.
    DATA lt_ra_account            TYPE rsdsselopt_t.
    DATA lt_ra_lcurr              TYPE rsdsselopt_t.
    DATA ld_field                 TYPE fieldname.
    DATA ld_date                  TYPE sydatum.
    DATA lt_where                 TYPE STANDARD TABLE OF string.
    DATA ld_index_table           TYPE tabname.
    DATA ld_bukrs                 TYPE bukrs.
    DATA ld_field_list(72).
    DATA ld_help(72).
    DATA lt_field_list_bseg       TYPE sci_srchstr.
    DATA lt_field_list_bkpf       TYPE sci_srchstr.
    DATA lt_field_list_unknown    TYPE sci_srchstr.
    DATA lt_field_list_t001       TYPE sci_srchstr.
    DATA lt_field_sel             TYPE sci_srchstr.
    DATA lt_field_list            TYPE sci_srchstr.
    DATA lt_group                 TYPE sci_srchstr.
    DATA lr_data                  TYPE REF TO data.
    DATA ls_t001                  TYPE t001.
    DATA ls_t001_help             TYPE t001.
    DATA BEGIN OF ls_curr_help.
    DATA   waers                  TYPE waers.
    DATA   currkey_0d TYPE fagl_currkey_0d.                 "n2655899
    DATA   currval_0d TYPE fagl_currval_0d.                 "n2655899
    DATA   currkey_00 TYPE fagl_currkey_00.                 "n2655899
    DATA   currval_00 TYPE fagl_currval_00.                 "n2655899
    DATA   currkey_10             TYPE fagl_currkey_10.
    DATA   currval_10 TYPE fagl_currval_10.                 "n2655899
    DATA   currkey_30             TYPE fagl_currkey_30.     "3138877
    DATA   currval_30             TYPE fagl_currval_30.     "3138877
    DATA   currkey_40             TYPE fagl_currkey_40.     "3138877
    DATA   currval_40             TYPE fagl_currval_40.     "3138877
    DATA   zzxarch                TYPE xarch.
    DATA   counter TYPE sy-dbcnt.                           "n2655899
    DATA END OF ls_curr_help.
    DATA ld_first.
    DATA ls_component             TYPE abap_compdescr.
    DATA ld_string                TYPE string.
    DATA ld_subrc                 TYPE sysubrc.
    DATA ld_where                 TYPE string.
    DATA lt_filter_where          TYPE STANDARD TABLE OF string.
    DATA lo_typedescr             TYPE REF TO cl_abap_typedescr.
    DATA lo_structdescr           TYPE REF TO cl_abap_structdescr.
    DATA ld_question              TYPE string.
    DATA ld_answer.
    DATA lt_param                 TYPE STANDARD TABLE OF spar.
    DATA ls_param                 TYPE spar.
    DATA ls_dfies                 TYPE dfies.
    DATA ls_dfies_other           TYPE dfies.               "n2883106
    DATA ld_count                 TYPE numc2.
    DATA ld_lines                 TYPE i.
    DATA lt_options               TYPE STANDARD TABLE OF spopli.
    DATA ls_options               TYPE spopli.
    DATA ld_read_from_archive.
    DATA ltx_trange               TYPE rsds_trange.
    DATA lsx_trange               TYPE rsds_range.
    DATA ltx_frange_bkpf          TYPE rsds_frange_t.
    DATA ltx_frange_bseg          TYPE rsds_frange_t.
    DATA lsx_frange               TYPE rsds_frange.
    DATA ld_tabix                 TYPE i.
    DATA lt_bkpf                  TYPE STANDARD TABLE OF bkpf.
    DATA ls_bkpf                  TYPE bkpf.
    DATA lt_bseg                  TYPE STANDARD TABLE OF bseg.
    DATA ls_bseg                  TYPE bseg.
    DATA ld_show_errors           TYPE boole.
    DATA ld_show_errors_d         TYPE boole_d.             "n2770409
    DATA ls_message               TYPE bapiret2.
    DATA ld_date_init             TYPE sydatum.

    DATA lt_doc_data TYPE as_t_tablebuffer.                 "n2738710
    DATA ls_t881 TYPE t881.                                 "n2738710
    DATA ls_t800a TYPE t800a.                               "n2738710
    DATA lr_data_sel TYPE REF TO data.                      "n2738710
    DATA lt_accit TYPE TABLE OF accit.                      "n2738710
    DATA lt_acccr TYPE TABLE OF acccr.                      "n2738710
    DATA ld_skip  TYPE boole_d.                             "n2738710

    DATA lo_descr                 TYPE REF TO cl_abap_typedescr.
    DATA lo_str_descr_in          TYPE REF TO cl_abap_structdescr.
    DATA lo_str_descr_add         TYPE REF TO cl_abap_structdescr.
    DATA lo_str_descr_result      TYPE REF TO cl_abap_structdescr.
    DATA ls_abap_comp_descr       TYPE abap_compdescr.
    DATA lr_data_struc            TYPE REF TO data.
    DATA lr_data_tab              TYPE REF TO data.
    DATA lt_abap_component_descr  TYPE abap_component_tab.
    DATA ls_abap_component_descr  TYPE abap_componentdescr.
    DATA ld_debit_credit          TYPE shkzg.
    DATA ld_rldnr                 TYPE rldnr.
    DATA ls_orginfo               TYPE glx_org_info.
    DATA ld_curtp                 TYPE curtp.
    DATA ld_debit_credit_exist.
    DATA BEGIN OF ls_debit_credit_ind.
    DATA   shkzg                  TYPE shkzg.
    DATA END OF ls_debit_credit_ind.
    DATA ltx_dyn_where            TYPE rsds_twhere.
    DATA lsx_dyn_where            TYPE rsds_where.
    DATA ltx_dyn_restrictions     TYPE rsds_trange.
    DATA ltx_where TYPE rsds_twhere.                        "n2450963
    DATA lsx_where TYPE rsds_where.                         "n2450963
    DATA ld_include_non_oi.
    DATA BEGIN OF ls_layout.
    DATA   bukrs.
    DATA   belnr.
    DATA   buzei.
    DATA   xarch.
    DATA END OF ls_layout.
    DATA ld_no_collect.
    DATA ld_bstatumskz TYPE string.                         "n2738710
    DATA lt_bstatumskz TYPE TABLE OF string.                "n2770409
    DATA ld_tab        TYPE ddobjname.                      "n2883106
    data refdate type augdt.                                "3138877
    data lv_hint like id_hint.                              "3138877
    data lt_hint_split type table of string.                "3138877

*----------------------------------------------------------------------*
    FIELD-SYMBOLS <lt_data>                 TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_data>                 TYPE any.
    FIELD-SYMBOLS <ld_value>                TYPE any.

    FIELD-SYMBOLS <ld_data>                 TYPE any.
    FIELD-SYMBOLS <ls_data_result>          TYPE any.
    FIELD-SYMBOLS <lt_data_result>          TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_abap_component_descr> TYPE abap_componentdescr.
    FIELD-SYMBOLS <lt_line_items> TYPE ANY TABLE.           "n2738710

    FIELD-SYMBOLS <lsx_range>               TYPE rsds_range.
    FIELD-SYMBOLS <lsx_frange>              TYPE rsds_frange.
    FIELD-SYMBOLS <lsx_trange>              TYPE rsds_trange.
    FIELD-SYMBOLS <lsx_dyn_where>           TYPE rsds_where.
    FIELD-SYMBOLS <lsx_where> TYPE rsds_where.              "n2450963

    field-symbols <augdt> type augdt.                        "3138877
    field-symbols <cleared> type char1.                      "3138877

*----------------------------------------------------------------------*
* First call of this method
    CHECK md_archive_relevant = abap_undefined
* It has already been determined that archive access is potentially
* relevant
    OR    md_archive_relevant = abap_true.

* This will only work for FI line item browsers:
* FBL1H
* FBL3H
* FBL5H
* FAGLL03H
    CASE sy-cprog.
      WHEN 'FAGL_LINE_ITEM_BROWSER_AP'
        OR 'FAGL_LINE_ITEM_BROWSER_AR'
        OR 'FAGL_LINE_ITEM_BROWSER_CGL'
        OR 'FAGL_LINE_ITEM_BROWSER_EV'                      "n2712558
        OR 'FAGL_LINE_ITEM_BROWSER'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

* Get selection restrictions of the main program
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = sy-cprog
      TABLES
        selection_table = lt_sel.

    SORT lt_sel BY selname.

    READ TABLE lt_sel
      INTO ls_sel
      WITH KEY
        selname = 'P_XOIM'
      BINARY SEARCH.
    IF sy-subrc = 0 and ls_sel-low = 'X'. "3138877
      ld_include_non_oi = abap_true.
    ENDIF.

* Was the program started for open items with a key date?
    READ TABLE lt_sel
      INTO ls_sel
      WITH KEY
        selname = 'P_OI'
      BINARY SEARCH.
    IF sy-subrc = 0
    AND ls_sel-low > space.
      ld_date = ls_sel-low.
      IF  ld_date >= sy-datum
      AND ld_include_non_oi = abap_false.
* User wants to work on currently open items
* >> Open items cannot be archived
* >> Archive irrelevant!
        RETURN.
      ELSE.
        READ TABLE lt_sel
          INTO ls_sel
          WITH KEY
            selname = 'P_KEYDO'
          BINARY SEARCH.
        IF sy-subrc = 0.
          IF ls_sel-low <> ld_date_init.
* Restrictions for posting date
            ls_range-sign   = 'I'.                "begin "n3009331
            ls_range-option = 'BT'.
            ls_range-low  = '00010101'.
            ls_range-high = ls_sel-low.
            APPEND ls_range
              TO lt_ra_pdate.
* Restrictions for clearing date
            ld_date = ls_sel-low + 1.
            ls_range-low  = ld_date.
            ls_range-high = '99991231'.           "end "n3009331
            APPEND ls_range
              TO lt_ra_cdate.
          ENDIF.
        ENDIF.

* Select data from accounts without open item management?
        IF ld_include_non_oi = abap_false.
          ls_range-sign   = 'I'.
          ls_range-option = 'EQ'.
          ls_range-low    = abap_true.
          APPEND ls_range
            TO lt_ra_xopvw.
          APPEND 'xopvw IN lt_ra_xopvw'                     "#EC NOTEXT
            TO lt_where.
        ENDIF.
      ENDIF.
    ENDIF.

* Was the program started for all items?
    READ TABLE lt_sel
      INTO ls_sel
      WITH KEY
        selname = 'P_AI'
      BINARY SEARCH.
    IF sy-subrc = 0
    AND ls_sel-low > space.
* Add fiscal year
      READ TABLE lt_sel
        INTO ls_sel
        WITH KEY
          selname = 'S_RYEAR'
        BINARY SEARCH.
      IF sy-subrc = 0
      AND ls_sel-sign > space.
        LOOP AT lt_sel
          INTO  ls_sel
          FROM sy-tabix.
          IF ls_sel-selname <> 'S_RYEAR'.
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING ls_sel
            TO ls_range.
          APPEND ls_range
            TO lt_ra_ryear.
        ENDLOOP.
      ENDIF.

* Add posting period
      READ TABLE lt_sel
        INTO ls_sel
        WITH KEY
          selname = 'S_PERIOD'
        BINARY SEARCH.
      IF sy-subrc = 0
      AND ls_sel-sign > space.
        LOOP AT lt_sel
          INTO  ls_sel
          FROM sy-tabix.
          IF ls_sel-selname <> 'S_PERIOD'.
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING ls_sel
            TO ls_range.
          APPEND ls_range
            TO lt_ra_period.
        ENDLOOP.
      ENDIF.

* Add posting date
      READ TABLE lt_sel
        INTO ls_sel
        WITH KEY
          selname = 'S_PDATE'
        BINARY SEARCH.
      IF sy-subrc = 0
      AND ls_sel-sign > space.
        LOOP AT lt_sel
          INTO  ls_sel
          FROM sy-tabix.
          IF ls_sel-selname <> 'S_PDATE'.
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING ls_sel
            TO ls_range.
          APPEND ls_range
            TO lt_ra_pdate.
        ENDLOOP.
      ENDIF.
    ENDIF.

* Was the program started for cleared items?
    READ TABLE lt_sel
      INTO ls_sel
      WITH KEY
        selname = 'P_CI'
      BINARY SEARCH.
    IF sy-subrc = 0
    AND ls_sel-low > space.
* Add key date for open item selection
      READ TABLE lt_sel
        INTO ls_sel
        WITH KEY
          selname = 'P_KEYDC'
        BINARY SEARCH.
      IF sy-subrc = 0.
        IF ls_sel-low <> ld_date_init.
* Restrictions for posting date
          ls_range-sign   = 'I'.                  "begin "n3009331
          ls_range-option = 'BT'.
          ls_range-low    = '00010101'.
          ls_range-high   = ls_sel-low.
          APPEND ls_range
            TO lt_ra_pdate.
* Restrictions for clearing date
          ld_date = ls_sel-low + 1.
          ls_range-low  = ld_date.
          ls_range-high = '99991231'.
          APPEND ls_range
            TO lt_ra_cdate.                       "end "n3009331
        ENDIF.
      ENDIF.

* Add clearing date
      READ TABLE lt_sel
        INTO ls_sel
        WITH KEY
          selname = 'S_CDATE'
        BINARY SEARCH.
      IF sy-subrc = 0
      AND ls_sel-sign > space.
        LOOP AT lt_sel
          INTO  ls_sel
          FROM sy-tabix.
          IF ls_sel-selname <> 'S_CDATE'.
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING ls_sel
            TO ls_range.
          APPEND ls_range
            TO lt_ra_cdate.
        ENDLOOP.
      ENDIF.
    ENDIF.

* Add company code
    READ TABLE lt_sel
      INTO ls_sel
      WITH KEY
        selname = 'S_CCODE'
      BINARY SEARCH.
    IF sy-subrc = 0
    AND ls_sel-sign > space.
      LOOP AT lt_sel
        INTO  ls_sel
        FROM sy-tabix.
        IF ls_sel-selname <> 'S_CCODE'.
          EXIT.
        ENDIF.
        MOVE-CORRESPONDING ls_sel
          TO ls_range.
        APPEND ls_range
          TO lt_ra_ccode.
      ENDLOOP.
    ENDIF.

* Get restrictions for main account
    CASE sy-cprog.
      WHEN 'FAGL_LINE_ITEM_BROWSER_AP'.
        ld_field = 'S_VEND'.
      WHEN 'FAGL_LINE_ITEM_BROWSER_AR'.
        ld_field = 'S_CUST'.
      WHEN 'FAGL_LINE_ITEM_BROWSER_CGL'
        OR 'FAGL_LINE_ITEM_BROWSER_EV'                      "n2712558
        OR 'FAGL_LINE_ITEM_BROWSER'.
        ld_field = 'S_RACCT'.
    ENDCASE.

* Add account numbers
    READ TABLE lt_sel
      INTO ls_sel
      WITH KEY
        selname = ld_field
      BINARY SEARCH.
    IF sy-subrc = 0
    AND ls_sel-sign > space.
      LOOP AT lt_sel
        INTO  ls_sel
        FROM sy-tabix.
        IF ls_sel-selname <> ld_field.
          EXIT.
        ENDIF.
        MOVE-CORRESPONDING ls_sel
          TO ls_range.
        APPEND ls_range
          TO lt_ra_account.
      ENDLOOP.
    ENDIF.

* Note 3138877: Add default restriction to main account
    if lt_ra_account is initial.
      ls_range-sign = 'I'.
      ls_range-option = 'NE'.
      ls_range-low = ''.
      append ls_range to lt_ra_account.
    endif.

* Adjust name of relevant index table and restriction of main account
    CASE sy-cprog.
      WHEN 'FAGL_LINE_ITEM_BROWSER_AP'.
        ld_index_table = 'BSAK'.
        APPEND 'lifnr IN lt_ra_account'                     "#EC NOTEXT
          TO lt_where.
      WHEN 'FAGL_LINE_ITEM_BROWSER_AR'.
        ld_index_table = 'BSAD'.
        APPEND 'kunnr IN lt_ra_account'                     "#EC NOTEXT
          TO lt_where.
      WHEN 'FAGL_LINE_ITEM_BROWSER_CGL'
        OR 'FAGL_LINE_ITEM_BROWSER_EV'                      "n2712558
        OR 'FAGL_LINE_ITEM_BROWSER'.
        ld_index_table = 'BSAS'.
        IF lt_where IS INITIAL.
          APPEND 'hkont IN lt_ra_account'                   "#EC NOTEXT
            TO lt_where.
        ELSE.
          APPEND 'AND hkont IN lt_ra_account'               "#EC NOTEXT
            TO lt_where.
        ENDIF.
    ENDCASE.

    IF mt_dfies_index IS INITIAL.
* Get field list of index table
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = ld_index_table
        TABLES
          dfies_tab = mt_dfies_index.
      SORT mt_dfies_index BY fieldname.

* Get field list of BSEG
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = 'BSEG'
        TABLES
          dfies_tab = mt_dfies_bseg.
      SORT mt_dfies_bseg BY fieldname.

* Get field list of BKPF
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = 'BKPF'
        TABLES
          dfies_tab = mt_dfies_bkpf.
      SORT mt_dfies_bkpf BY fieldname.

* Get field list of T001
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = 'T001'
        TABLES
          dfies_tab = mt_dfies_t001.
      SORT mt_dfies_t001 BY fieldname.

* Get field list of generated view
      ld_tab = id_sel_view.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = ld_tab
        TABLES
          dfies_tab = mt_dfies_glpos.
      SORT mt_dfies_glpos BY fieldname.

    ENDIF.

* Determine sender table for fields
    LOOP AT it_field_list
      INTO  ld_field_list.
      CASE ld_field_list.
* Special handling for some fields...
* Document currency values
        WHEN 'CURRVAL_00'.
          APPEND 'SUM( WRBTR ) as CURRVAL_00'               "#EC NOTEXT
            TO lt_field_sel.
          APPEND 'WRBTR'
            TO lt_field_list.
* Document currency values
        WHEN 'CURRVAL_0D'.
          APPEND 'SUM( WRBTR ) as CURRVAL_0D'               "#EC NOTEXT
            TO lt_field_sel.
          APPEND 'WRBTR'
            TO lt_field_list.
* Local currency values
        WHEN 'CURRVAL_10'.
          APPEND 'SUM( DMBTR ) as CURRVAL_10'               "#EC NOTEXT
            TO lt_field_sel.
          APPEND 'DMBTR'
            TO lt_field_list.
* Local currency key
        WHEN 'CURRKEY_10'.
          APPEND 'WAERS'
            TO lt_field_list_t001.
* Document currency key
        WHEN 'CURRKEY_00'
          OR 'CURRKEY_0D'.
          ld_help = 'WAERS'.
          COLLECT ld_help
            INTO lt_field_list.
          ld_help = 'WAERS as CURRKEY_00'.                  "#EC NOTEXT
          COLLECT ld_help
            INTO lt_field_sel.
          ld_help = 'WAERS as CURRKEY_0D'.                  "#EC NOTEXT
          COLLECT ld_help
            INTO lt_field_sel.
          ld_help = 'WAERS'.
          COLLECT ld_help
            INTO lt_group.
* Counter
        WHEN 'COUNTER'.
          APPEND 'COUNT( * ) as COUNTER'                    "#EC NOTEXT
            TO lt_field_sel.
        WHEN 'X_CLEARED_ITEM'.
* Calculated field from view
        WHEN 'ZZXARCH'
          OR 'XARCH'.
          ls_layout-xarch = abap_true.
* Customer field >> no action necessary
        WHEN OTHERS.
          CASE ld_field_list.
            WHEN 'BUKRS'
              OR 'RBUKRS'.
              ls_layout-bukrs = abap_true.
            WHEN 'BELNR'
              OR 'DOCNR'.
              ls_layout-belnr = abap_true.
            WHEN 'BUZEI'
              OR 'DOCLN'.
              ls_layout-buzei = abap_true.
          ENDCASE.
* Find field in index table
          READ TABLE mt_dfies_index
            WITH KEY
              fieldname = ld_field_list
            INTO ls_dfies                           "begin of "n2883106
            BINARY SEARCH
            TRANSPORTING ALL FIELDS.
          IF sy-subrc = 0.
            READ TABLE mt_dfies_glpos
              WITH KEY fieldname = ld_field_list
              INTO ls_dfies_other
              BINARY SEARCH
              TRANSPORTING ALL FIELDS.
            IF sy-subrc = 0.
              IF ls_dfies_other-intlen <> ls_dfies-intlen.
                "Field with the same name, but different length
                "-> exclude from selection (e.g. PRODPER)
                CONTINUE. "Loop at it_field_list
              ENDIF.
            ENDIF.                                    "end of "n2883106
* Field found in index table
            APPEND ld_field_list
              TO lt_field_sel.
            APPEND ld_field_list
              TO lt_group.
            APPEND ld_field_list
              TO lt_field_list.
          ELSE.
* Find field in BSEG
            READ TABLE mt_dfies_bseg
              WITH KEY
                fieldname = ld_field_list
              BINARY SEARCH
              TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
* Field found in BSEG
              APPEND ld_field_list
                TO lt_field_list_bseg.
            ELSE.
* Find field in BKPF
              READ TABLE mt_dfies_bkpf
                WITH KEY
                  fieldname = ld_field_list
                BINARY SEARCH
                TRANSPORTING NO FIELDS.
              IF sy-subrc = 0.
* Field found in BKPF
                APPEND ld_field_list
                  TO lt_field_list_bkpf.
              ELSE.
* Find field in T001
                READ TABLE mt_dfies_t001
                  WITH KEY
                    fieldname = ld_field_list
                  BINARY SEARCH
                  TRANSPORTING NO FIELDS.
                IF sy-subrc = 0.
* Field found in T001
                  APPEND ld_field_list
                    TO lt_field_list_t001.
* Field unknown...
                ELSE.
                  APPEND ld_field_list
                    TO lt_field_list_unknown.
* Determine what to do for unknown fields
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF  (     ls_layout-bukrs = abap_true
          AND ls_layout-belnr = abap_true
          AND ls_layout-buzei = abap_true )
    OR ls_layout-xarch = abap_true.
      ld_no_collect = abap_true.
    ENDIF.

    CLEAR ls_range.
    ls_range-sign   = 'I'.
    ls_range-option = 'EQ'.

* Transform entries selected in UI to WHERE clause
    SORT lt_field_list.
    SORT lt_field_list_bseg.
    SORT lt_field_list_bkpf.
    SORT lt_field_list_t001.

* Apply retrictions from dynamic selections
    ltx_dyn_restrictions = it_dyn_restrictions.
    LOOP AT ltx_dyn_restrictions
      ASSIGNING <lsx_range>.
      <lsx_range>-tablename = ld_index_table.
      LOOP AT <lsx_range>-frange_t
        ASSIGNING <lsx_frange>.
        READ TABLE mt_dfies_index
          WITH KEY
            fieldname = <lsx_frange>-fieldname
          BINARY SEARCH
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE <lsx_range>-frange_t.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF ltx_dyn_restrictions IS NOT INITIAL.
      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
        EXPORTING
          field_ranges  = ltx_dyn_restrictions
        IMPORTING
          where_clauses = ltx_dyn_where.

      READ TABLE ltx_dyn_where
        ASSIGNING <lsx_dyn_where>
        INDEX 1.
    ENDIF.

    IF <lsx_dyn_where> IS NOT ASSIGNED.
      ASSIGN lsx_dyn_where TO <lsx_dyn_where>.
    ENDIF.

    IF NOT it_where IS INITIAL.                   "begin "n2450963
* Set table name
      lsx_trange-tablename = ld_index_table.
* Set restrictions
      lsx_trange-frange_t = it_where.
* Check field names
      LOOP AT lsx_trange-frange_t
        ASSIGNING <lsx_frange>.
        READ TABLE mt_dfies_index
          WITH KEY
            fieldname = <lsx_frange>-fieldname
          BINARY SEARCH
          TRANSPORTING NO FIELDS.
* Field doesn't exist >> Delete
        IF sy-subrc <> 0.
          DELETE lsx_trange-frange_t.
        ENDIF.
      ENDLOOP.

      APPEND lsx_trange
        TO ltx_trange.
      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
        EXPORTING
          field_ranges  = ltx_trange
        IMPORTING
          where_clauses = ltx_where.

      READ TABLE ltx_where
        ASSIGNING <lsx_where>
        INDEX 1.
    ENDIF.

    IF <lsx_where> IS NOT ASSIGNED.
      ASSIGN lsx_where TO <lsx_where>.
    ENDIF.                                        "end "n2450963


    ld_bstatumskz = id_bstatumskz.                "begin of "n2738710
    CASE sy-cprog.
      WHEN 'FAGL_LINE_ITEM_BROWSER_AP'
        OR 'FAGL_LINE_ITEM_BROWSER_AR'.
      WHEN OTHERS.                                "begin of "n2770409
**      Build consistent where clause for BSTAT
        CLEAR ld_bstatumskz.
        READ TABLE lt_sel
          INTO ls_sel
          WITH KEY selname = 'P_TY_NOR'.
        IF sy-subrc = 0 AND ls_sel-low > space.
          IF ld_bstatumskz IS INITIAL.
            ld_bstatumskz = `( bstat = space OR bstat = 'L')`.
          ELSE.
            CONCATENATE
              ld_bstatumskz
              `OR ( bstat = space OR bstat = 'L')`
              INTO ld_bstatumskz SEPARATED BY space.
          ENDIF.
        ENDIF.

        READ TABLE lt_sel
          INTO ls_sel
          WITH KEY selname = 'P_TY_NIT'.
        IF sy-subrc = 0 AND ls_sel-low > space.
           IF ld_bstatumskz IS INITIAL.
            ld_bstatumskz = `( bstat = 'S')`.
          ELSE.
            CONCATENATE
              ld_bstatumskz
              `OR ( bstat = 'S')`
              INTO ld_bstatumskz SEPARATED BY space.
          ENDIF.
        ENDIF.

**      All options turned off -> nothing to do
        IF ld_bstatumskz IS INITIAL.
          RETURN.
        ENDIF.                                      "end of "n2770409
    ENDCASE.                                        "end of "n2738710

* Check whether any matching entries exist in the index table
    SELECT bukrs
      FROM (ld_index_table)
      INTO ld_bukrs
      UP TO 1 ROWS
      WHERE bukrs IN lt_ra_ccode
      AND   budat IN lt_ra_pdate
      AND   augdt IN lt_ra_cdate
      AND   gjahr IN lt_ra_ryear
      AND   monat IN lt_ra_period
      AND   xarch >  space
      AND   (ld_bstatumskz)                                 "n2738710
      AND   (lt_where)
      AND   (<lsx_dyn_where>-where_tab)
      AND   (<lsx_where>-where_tab)                         "n2450963
      AND   (lt_filter_where).
      EXIT.
    ENDSELECT.

    IF sy-subrc <> 0.
      CASE sy-cprog.
        WHEN 'FAGL_LINE_ITEM_BROWSER_AP'
          OR 'FAGL_LINE_ITEM_BROWSER_AR'.
* For AP/AR all items which could be archived are in BSAD/BSAK
* No relevant items found
* >> No archive access
          IF md_archive_relevant = abap_undefined.
            md_archive_relevant = abap_false.
            RETURN.                                         "n2655899
          ENDIF.
*          RETURN.                                            "n2655899
        WHEN 'FAGL_LINE_ITEM_BROWSER_CGL'
          OR 'FAGL_LINE_ITEM_BROWSER_EV'                    "n2712558
          OR 'FAGL_LINE_ITEM_BROWSER'.
* Check for matching entries in BSIS
          ld_index_table = 'BSIS'.
          SELECT bukrs
            FROM (ld_index_table)
            INTO ld_bukrs
            UP TO 1 ROWS
            WHERE bukrs IN lt_ra_ccode
            AND   budat IN lt_ra_pdate
            AND   augdt IN lt_ra_cdate
            AND   gjahr IN lt_ra_ryear
            AND   monat IN lt_ra_period
            AND   xarch >  space
            AND   (ld_bstatumskz)                           "n2770409
            AND   (lt_where)
            AND   (<lsx_dyn_where>-where_tab)
            AND   (<lsx_where>-where_tab)                   "n2450963
            AND   (lt_filter_where).
            EXIT.
          ENDSELECT.
          IF sy-subrc <> 0.

            IF md_archive_relevant = abap_undefined.
*             check if there is a recon account     "begin of "n2770409
              READ TABLE it_where
                WITH KEY fieldname = 'HKONT'
                INTO lsx_frange
                TRANSPORTING ALL FIELDS.
              IF sy-subrc = 0.
**              Accounts without Line item display are only in archive
                SELECT SINGLE COUNT( * )
                  FROM skb1
                  WHERE bukrs IN lt_ra_ccode
                  AND   saknr IN lsx_frange-selopt_t
                  AND   xkres = abap_false. "#EC CI_BYPASS "#EC CI_GENBUFF
                IF sy-subrc = 0.
                  md_archive_relevant = abap_true.
**                Need to read archive in this case
                  md_archive_option = 1.
                ELSE.
                  md_archive_relevant = abap_false.
                  RETURN.
                ENDIF.
              ELSE.
                md_archive_relevant = abap_false.
                RETURN.                                     "n2655899
              ENDIF.                                  "end of "n2770409
            ENDIF.
*            RETURN.                                       "n2655899
          ENDIF.
          ld_index_table = 'BSAS'.
      ENDCASE.
    ENDIF.

* Archive access is relevant
    md_archive_relevant = abap_true.

* First time or user requested to be asked every time.
    IF md_archive_option = 0
    OR md_archive_option = 3.
* Some information is not available in the index table - but it is
* available in the archive
      IF lt_field_list_bkpf IS NOT INITIAL
      OR lt_field_list_bseg IS NOT INITIAL.

* Create list of fields which are not in index table
        LOOP AT lt_field_list_bkpf
          INTO  ld_field_list.
          READ TABLE mt_dfies_bkpf
            INTO ls_dfies
            WITH KEY
              fieldname = ld_field_list
            BINARY SEARCH.
          IF sy-subrc = 0.
            ld_count = ld_count + 1.
            ls_param-param = ld_count.
            CONCATENATE '(' ld_field_list ')'
              INTO ls_param-value.
            CONCATENATE ls_dfies-scrtext_l ls_param-value
              INTO ls_param-value SEPARATED BY space.
            APPEND ls_param
              TO lt_param.
          ENDIF.
        ENDLOOP.

* Create list of fields which are not in index table
        LOOP AT lt_field_list_bseg
          INTO  ld_field_list.
          READ TABLE mt_dfies_bseg
            INTO ls_dfies
            WITH KEY
              fieldname = ld_field_list
            BINARY SEARCH.
          IF sy-subrc = 0.
            ld_count = ld_count + 1.
            ls_param-param = ld_count.
            CONCATENATE '(' ld_field_list ')'
              INTO ls_param-value.
            CONCATENATE ls_dfies-scrtext_l ls_param-value
              INTO ls_param-value SEPARATED BY space.
            APPEND ls_param
              TO lt_param.
          ENDIF.
        ENDLOOP.

* Set placeholders
        IF lines( lt_param ) > 10.
          DELETE lt_param
            FROM 10.
          ls_param-value = '...'.
          APPEND ls_param
            TO lt_param.
        ELSE.
          ld_lines = 10 - ld_count.
          CLEAR ls_param.
          DO ld_lines TIMES.
            ld_count = ld_count + 1.
            ls_param-param = ld_count.
            APPEND ls_param
              TO lt_param.
          ENDDO.
        ENDIF.

* Ask user whether archive should be read
        ld_question = 'Do you want to select data from the archive?'(001).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Access Archive?'(002)
            diagnose_object       = 'FAGL_LIB_ARCHIVE_VIA_INDEX'
            text_question         = ld_question
            display_cancel_button = space
            default_button        = 2
          IMPORTING
            answer                = ld_answer
          TABLES
            parameter             = lt_param
          EXCEPTIONS
            OTHERS                = 0.

        md_archive_option = ld_answer.
      ENDIF.
    ENDIF.

* Access archive?
    CASE md_archive_option.
      WHEN 1.
        ld_read_from_archive = abap_true.
      WHEN 2.
        ld_read_from_archive = abap_false.
    ENDCASE.

    CREATE DATA lr_data LIKE LINE OF ct_data.
    ASSIGN lr_data->* TO <ls_data>.

    CREATE DATA lr_data LIKE ct_data.
    ASSIGN lr_data->* TO <lt_data>.

* Get structure description of data table
    CALL METHOD cl_abap_structdescr=>describe_by_data
      EXPORTING
        p_data      = <ls_data>
      RECEIVING
        p_descr_ref = lo_descr.

    lo_str_descr_in ?= lo_descr.

* Go over the components of the structure
    LOOP AT lo_str_descr_in->components
      INTO ls_abap_comp_descr.
* Get a field symbol with the same type as the component
      ASSIGN COMPONENT ls_abap_comp_descr-name
        OF STRUCTURE <ls_data>
        TO <ld_data>.
      CHECK sy-subrc = 0.
* Use the same name
      ls_abap_component_descr-name = ls_abap_comp_descr-name.
* Get the typing information of the structure field
      CALL METHOD cl_abap_typedescr=>describe_by_data
        EXPORTING
          p_data      = <ld_data>
        RECEIVING
          p_descr_ref = lo_descr.
* Move the typing information to a field used as a new component list
      ls_abap_component_descr-type ?= lo_descr.
* Add the field to the component list
      APPEND ls_abap_component_descr
        TO lt_abap_component_descr.
      IF ls_abap_component_descr-name = 'SHKZG'.
        ld_debit_credit_exist = abap_true.
      ENDIF.
    ENDLOOP.

    IF ld_debit_credit_exist = abap_false.
      ls_abap_component_descr-name = 'SHKZG'.
* Get typing information
      CALL METHOD cl_abap_typedescr=>describe_by_data
        EXPORTING
          p_data      = ld_debit_credit
        RECEIVING
          p_descr_ref = lo_descr.
      ls_abap_component_descr-type ?= lo_descr.
* Add the field to the component list
      APPEND ls_abap_component_descr
        TO lt_abap_component_descr.
    ENDIF.

* Create a description of the target structure containing all fields
    CALL METHOD cl_abap_structdescr=>create
      EXPORTING
        p_components = lt_abap_component_descr
      RECEIVING
        p_result     = lo_str_descr_result.

* Create data reference and assign to field symbol
    CREATE DATA lr_data_struc TYPE HANDLE lo_str_descr_result.
    ASSIGN lr_data_struc->* TO <ls_data_result>.

    CREATE DATA lr_data_tab
      LIKE STANDARD TABLE OF <ls_data_result>.
    IF sy-subrc = 0.
      ASSIGN lr_data_tab->* TO <lt_data_result>.
    ENDIF.

    IF ld_debit_credit_exist = abap_false.
      APPEND 'SHKZG' TO lt_field_sel.
      APPEND 'SHKZG' TO lt_group.
    ENDIF.

* Get ccode properties                              "begin of "n2655899
    IF mt_t001[] IS INITIAL.
      APPEND 'BUKRS'
          TO lt_field_list_t001.
      SELECT (lt_field_list_t001)
        FROM t001
        INTO CORRESPONDING FIELDS OF TABLE mt_t001
        ORDER BY bukrs.
    ENDIF.                                            "end of "n2655899

    IF ld_read_from_archive = abap_false.
*      SELECT (lt_field_sel)                        "begin of "n2738710
*        FROM (ld_index_table)
*        INTO CORRESPONDING FIELDS OF TABLE <lt_data_result>
*        WHERE bukrs IN lt_ra_ccode
*        AND   budat IN lt_ra_pdate
*        AND   augdt IN lt_ra_cdate
*        AND   gjahr IN lt_ra_ryear
*        AND   monat IN lt_ra_period
*        AND   xarch >  space
*        AND   (lt_where)
*        AND   (<lsx_dyn_where>-where_tab)
*        AND   (<lsx_where>-where_tab)                       "n2450963
*        AND   (lt_filter_where)
*        GROUP BY (lt_group).

      CASE sy-cprog.
        WHEN 'FAGL_LINE_ITEM_BROWSER_AP'
          OR 'FAGL_LINE_ITEM_BROWSER_AR'.

          SELECT (lt_field_sel)
            FROM (ld_index_table)
            INTO CORRESPONDING FIELDS OF TABLE <lt_data_result>
            WHERE bukrs IN lt_ra_ccode
            AND   budat IN lt_ra_pdate
            AND   augdt IN lt_ra_cdate
            AND   gjahr IN lt_ra_ryear
            AND   monat IN lt_ra_period
            AND   xarch >  space
            AND   (lt_where)
        AND   (ld_bstatumskz) "UMSKZ only exists in BS*D, BS*K "n2770409
            AND   (<lsx_dyn_where>-where_tab)
            AND   (<lsx_where>-where_tab)                   "n2450963
            AND   (lt_filter_where)
            GROUP BY (lt_group).

* For AP/AR all items which could be archived are in BSAD/BSAK:
* >> All relevant items found already selected
        WHEN 'FAGL_LINE_ITEM_BROWSER_CGL'
          OR 'FAGL_LINE_ITEM_BROWSER_EV'                    "n2712558
          OR 'FAGL_LINE_ITEM_BROWSER'.

          SELECT (lt_field_sel)
            FROM (ld_index_table)
            INTO CORRESPONDING FIELDS OF TABLE <lt_data_result>
            WHERE bukrs IN lt_ra_ccode
            AND   budat IN lt_ra_pdate
            AND   augdt IN lt_ra_cdate
            AND   gjahr IN lt_ra_ryear
            AND   monat IN lt_ra_period
            AND   xarch >  space
            AND   (ld_bstatumskz)                           "n2770409
            AND   (lt_where)
            AND   (<lsx_dyn_where>-where_tab)
            AND   (<lsx_where>-where_tab)                   "n2450963
            AND   (lt_filter_where)
            GROUP BY (lt_group).                        "end of "n2738710

* For GL open items there may also still be relevant entries in BSIS:
* >> Select additional items...
          ld_index_table = 'BSIS'.
          SELECT (lt_field_sel)
            FROM (ld_index_table)
            APPENDING CORRESPONDING FIELDS OF TABLE <lt_data_result>
            WHERE bukrs IN lt_ra_ccode
            AND   budat IN lt_ra_pdate
            AND   augdt IN lt_ra_cdate
            AND   gjahr IN lt_ra_ryear
            AND   monat IN lt_ra_period
            AND   xarch >  space
            AND   (ld_bstatumskz)                           "n2770409
            AND   (lt_where)
            AND   (<lsx_dyn_where>-where_tab)
            AND   (<lsx_where>-where_tab)                   "n2450963
            AND   (lt_filter_where)
            GROUP BY (lt_group).
      ENDCASE.

** Get ccode properties                             "begin of "n2655899
*      IF mt_t001 IS INITIAL.
*        APPEND 'BUKRS'
*          TO lt_field_list_t001.
*        SELECT (lt_field_list_t001)
*          FROM t001
*          INTO CORRESPONDING FIELDS OF TABLE mt_t001
*          ORDER BY bukrs.
*      ENDIF.                                         "end of "n2655899

* Add ccode properties to data table
      LOOP AT <lt_data_result>
        ASSIGNING <ls_data_result>.
        MOVE-CORRESPONDING <ls_data_result>
          TO ls_t001_help.
        IF ls_t001_help-bukrs <> ls_t001-bukrs.
          READ TABLE mt_t001
            INTO ls_t001
            WITH KEY
              bukrs = ls_t001_help-bukrs.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          IF ls_t001-waers > space.
            CHECK ls_t001-waers
              IN lt_ra_lcurr.
          ENDIF.
* Determine ledger
          ASSIGN COMPONENT 'RLDNR'
            OF STRUCTURE <ls_data_result>
            TO <ld_value>.
          IF sy-subrc = 0.
            ld_rldnr = <ld_value>.
          ELSE.
            ld_rldnr = '00'.
          ENDIF.

* Determine ccode info
          CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
            EXPORTING
              i_rldnr             = ld_rldnr
              i_orgunit           = ls_t001-bukrs
            IMPORTING
              organizational_info = ls_orginfo
            EXCEPTIONS
              OTHERS              = 0.
        ENDIF.

        CLEAR ls_curr_help.                                 "n2738710
        MOVE-CORRESPONDING <ls_data_result>                 "n2738710
          TO ls_curr_help.                                  "n2738710
* Set local currency key
        IF ls_t001_help-bukrs = ls_t001-bukrs.
          ls_curr_help-currkey_10 = ls_t001-waers.
          MOVE-CORRESPONDING ls_t001
            TO <ls_data_result>.
          MOVE-CORRESPONDING ls_curr_help
            TO <ls_data_result>.
        ENDIF.
        MOVE-CORRESPONDING <ls_data_result>
          TO ls_curr_help.

* Set additional currency keys
        LOOP AT lt_abap_component_descr
          ASSIGNING <ls_abap_component_descr>.
          CASE <ls_abap_component_descr>-name.
            WHEN 'CURRKEY_00'
              OR 'CURRKEY_0D'
              OR 'CURRKEY_10'.
* Currency keys are set already
            WHEN OTHERS.
* Process other currency key fields
              IF <ls_abap_component_descr>-name CS 'CURRKEY'.
* Position on field of structure
                ASSIGN COMPONENT <ls_abap_component_descr>-name
                  OF STRUCTURE <ls_data_result>
                  TO <ld_value>.
                IF sy-subrc = 0.
* Get currency type
                  ld_curtp = <ls_abap_component_descr>-name+8.
* Compare to org info and set currency key field accordingly
                  CASE ld_curtp.
                    WHEN ls_orginfo-curt2.
                      <ld_value> = ls_orginfo-curr2.
                    WHEN ls_orginfo-curt3.
                      <ld_value> = ls_orginfo-curr3.
                  ENDCASE.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDLOOP.

        MOVE-CORRESPONDING <ls_data_result>
          TO ls_debit_credit_ind.
* Determine sign of amount
        CASE ls_debit_credit_ind-shkzg.
          WHEN 'H'.
            LOOP AT lt_abap_component_descr
              ASSIGNING <ls_abap_component_descr>.
              IF <ls_abap_component_descr>-name CS 'CURRVAL'.
                ASSIGN COMPONENT <ls_abap_component_descr>-name
                  OF STRUCTURE <ls_data_result>
                  TO <ld_value>.
                IF sy-subrc = 0.
                  TRY.
                      <ld_value> = 0 - <ld_value>.
                  ENDTRY.
                ENDIF.

                ASSIGN COMPONENT <ls_abap_component_descr>-name "n2738710
                  OF STRUCTURE ls_curr_help                 "n2738710
                  TO <ld_value>.                            "n2738710
                IF sy-subrc = 0.                            "n2738710
                  TRY.                                      "n2738710
                      <ld_value> = 0 - <ld_value>.          "n2738710
                  ENDTRY.                                   "n2738710
                ENDIF.                                      "n2738710
              ENDIF.
            ENDLOOP.
        ENDCASE.
        ls_curr_help-zzxarch    = abap_true.
        MOVE-CORRESPONDING ls_curr_help
          TO <ls_data_result>.
        MOVE-CORRESPONDING <ls_data_result>
          TO <ls_data>.
        COLLECT <ls_data>
          INTO <lt_data>.
      ENDLOOP.
    ELSE.
      READ TABLE ltx_trange                         "begin of "n2712558
        INTO lsx_trange
        WITH KEY tablename = ld_index_table
        TRANSPORTING ALL FIELDS.
      IF sy-subrc = 0.
        LOOP AT lsx_trange-frange_t INTO lsx_frange.
*         Check if field is in BKPF
          READ TABLE mt_dfies_bkpf
            WITH KEY
              fieldname = lsx_frange-fieldname
            BINARY SEARCH
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND lsx_frange TO ltx_frange_bkpf.
            CONTINUE.
          ENDIF.

*         Check if field is in BSEG
          READ TABLE mt_dfies_bseg
            WITH KEY
              fieldname = lsx_frange-fieldname
            BINARY SEARCH
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND lsx_frange TO ltx_frange_bseg.
            CONTINUE.
          ENDIF.
        ENDLOOP.
      ENDIF.                                           "end of "2712558

      lsx_trange-tablename = 'BKPF'.
      lsx_trange-frange_t  = ltx_frange_bkpf.
      APPEND lsx_trange
        TO ltx_trange.

      lsx_trange-tablename = 'BSEG'.
      lsx_trange-frange_t  = ltx_frange_bseg.
      APPEND lsx_trange
        TO ltx_trange.

      ld_show_errors    = abap_true.  "n2770409
      ld_show_errors_d  = abap_true.  "n2770409

*     FAGLL03H in G/L view needs to read archived G/L view
      IF sy-cprog = 'FAGL_LINE_ITEM_BROWSER'.       "begin of "n2738710
        READ TABLE it_where
          WITH KEY fieldname = 'RLDNR'
          ASSIGNING <lsx_frange>.
        IF sy-subrc = 0.
          READ TABLE <lsx_frange>-selopt_t
            INTO ls_range
            INDEX 1.

          ld_rldnr = ls_range-low.
        ENDIF.

*       Get line item table for ledger
        SELECT SINGLE * FROM t881
          INTO ls_t881 WHERE rldnr = ld_rldnr.
        SELECT SINGLE *
          FROM t800a
          INTO ls_t800a
          WHERE ntable = ls_t881-tab
          AND   ttype = 'SI'.

        CLEAR lsx_trange.                           "begin of "n2770409
        lsx_trange-tablename = ls_t800a-tab.
        CLEAR lsx_frange.
        lsx_frange-fieldname = 'RLDNR'.
        CLEAR ls_range.
        ls_range-sign   = 'I'.
        ls_range-option = 'EQ'.
        ls_range-low = ld_rldnr.
        APPEND ls_range TO lsx_frange-selopt_t.
        APPEND lsx_frange TO lsx_trange-frange_t.
        CLEAR lsx_frange.                              "begin "n2848347
        lsx_frange-fieldname = 'RCLNT'.
        ls_range-low = sy-mandt. APPEND ls_range TO lsx_frange-selopt_t.
        APPEND lsx_frange TO lsx_trange-frange_t.

        "Add selection restrictions for archiving module
        LOOP AT it_where INTO lsx_frange
          WHERE fieldname = 'RBUKRS'
             OR fieldname = 'BUKRS'
             OR fieldname = 'RACCT'
             OR fieldname = 'HKONT'
             OR fieldname = 'BSTAT'
             OR fieldname = 'BUDAT'.
          CASE lsx_frange-fieldname.
            WHEN 'BUKRS'.
              lsx_frange-fieldname = 'RBUKRS'.
            WHEN 'HKONT'.
              lsx_frange-fieldname = 'RACCT'.
          ENDCASE.

          APPEND lsx_frange TO lsx_trange-frange_t.
        ENDLOOP.                                      "end of "n2848347
        APPEND lsx_trange TO ltx_trange.              "end of "n2770409

        CALL FUNCTION 'FI_DOCUMNT_ARCH_READ_DOCS_GLV'
          EXPORTING
            i_selections  = ltx_trange
            i_show_errors = ld_show_errors_d
          CHANGING
            ct_doc_data   = lt_doc_data
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc <> 0.
          ls_message-type       = sy-msgty.
          ls_message-id         = sy-msgid.
          ls_message-number     = sy-msgno.
          ls_message-message_v1 = sy-msgv1.
          ls_message-message_v2 = sy-msgv2.
          ls_message-message_v3 = sy-msgv3.
          ls_message-message_v4 = sy-msgv4.
          APPEND ls_message
            TO ct_message.
          RETURN.
        ENDIF.

* note 3149797: keep document header and document entry view
        data bkpf_ref type ref to data.
        data bseg_ref type ref to data.
        data bseg_org type standard table of bseg.
        field-symbols <bseg> type standard table.
        field-symbols <bkpf> type standard table.

        if line_exists( lt_doc_data[ tabname = 'BKPF' ] ).
          bkpf_ref = lt_doc_data[ tabname = 'BKPF' ]-tabref.
          assign bkpf_ref->* to <bkpf>.
          lt_bkpf = <bkpf>.
        endif.

        if line_exists( lt_doc_data[ tabname = 'BSEG' ] ).
          bseg_ref = lt_doc_data[ tabname = 'BSEG' ]-tabref.
          assign bseg_ref->* to <bseg>.
          bseg_org = <bseg>.
        endif.

*       Only keep the relevant line items for the table
        delete lt_doc_data
          where tabname <> cond #( when line_exists( lt_doc_data[ tabname = 'ACDOCA' ] )
                                    then 'ACDOCA'
                                    else ls_t800a-tab ).

        check lt_doc_data is not initial.                   "n2969566

        assign lt_doc_data[ 1 ]-tabref->* to <lt_line_items>.

* get collected fields
        data collected_fields type ttfieldname.

* get output components
        loop at it_component assigning field-symbol(<component>).
          append <component>-name to collected_fields.
        endloop.

* add fields for restrictions
        loop at ltx_trange into lsx_trange.
          loop at lsx_trange-frange_t assigning <lsx_frange>.
            collect <lsx_frange>-fieldname into collected_fields.
          endloop.
        endloop.

* fill global table
        data lt_glu1 type table of glu1.
        loop at <lt_line_items> assigning <ld_data>.
* check ledger
          assign component 'rldnr' of structure <ld_data> to field-symbol(<ledger>).
          check <ledger> is assigned and <ledger> = ld_rldnr.

* gl view information (overwrites entry view if contained in both)
          append corresponding #( <ld_data> ) to lt_glu1.
        endloop.

*       Field names in CT_DATA are BSEG fieldnames
        CALL FUNCTION 'G_GLU1_TO_AC_DOC_TRANSFORM'
          TABLES
            t_glu1  = lt_glu1
            t_accit = lt_accit
            t_acccr = lt_acccr.

        CALL FUNCTION 'FAGL_ACC_TO_DOC_TRANSFORM'
          EXPORTING
            iv_rldnr           = ld_rldnr
            it_accit           = lt_accit
            it_acccr           = lt_acccr
            i_collect_to_buzei = space
          IMPORTING
            et_bseg            = lt_bseg.

*       Do not sort BSEG as the logic relies on the numbering
*       I_COLLECT_TO_BUZEI = space!
        SORT lt_bkpf BY bukrs gjahr belnr ASCENDING.

        CREATE DATA lr_data LIKE LINE OF ct_data.
        ASSIGN lr_data->* TO <ls_data>.

        data(skip_document) = abap_false.

        loop at lt_bkpf into ls_bkpf.

          clear skip_document.

          loop at lt_bseg into ls_bseg where bukrs = ls_bkpf-bukrs
                                         and gjahr = ls_bkpf-gjahr
                                         and belnr = ls_bkpf-belnr.
            clear ld_skip.
            check skip_document is initial.

**          complement with entry view -> original BSEG line
            "get bseg key
            assign component 'bukrs'  of structure ls_bseg to field-symbol(<bkey_bukrs>).
            assign component 'belnr'  of structure ls_bseg to field-symbol(<bkey_belnr>).
            assign component 'gjahr'  of structure ls_bseg to field-symbol(<bkey_gjahr>).
            assign component 'buzei'  of structure ls_bseg to field-symbol(<bkey_buzei>).

            "complete key?
            if    <bkey_bukrs> is assigned
              and <bkey_belnr> is assigned
              and <bkey_gjahr> is assigned
              and <bkey_buzei> is assigned.
              read table bseg_org with key bukrs = <bkey_bukrs>
                                           belnr = <bkey_belnr>
                                           gjahr = <bkey_gjahr>
                                           buzei = <bkey_buzei>
                                   assigning field-symbol(<bseg_org_line>).

              if sy-subrc = 0.
                loop at collected_fields assigning field-symbol(<field>).
                  assign component <field> of structure ls_bseg to field-symbol(<bseg_converted_field>).
                  if sy-subrc = 0 and <bseg_converted_field> is initial.
                    assign component <field> of structure <bseg_org_line> to field-symbol(<bseg_org_field>).
                    <bseg_converted_field> = <bseg_org_field>.
                  endif.
                endloop.
              endif.

              unassign: <bkey_bukrs>, <bkey_belnr>, <bkey_gjahr>, <bkey_buzei>, <bseg_org_line>.
            endif.

**          apply selection criteria
            loop at ltx_trange into lsx_trange.
              check ld_skip is initial.

              loop at lsx_trange-frange_t assigning <lsx_frange>.
                check ld_skip is initial.

                check <lsx_frange>-fieldname <> 'RLDNR'.

**              restrictions on bkpf fields
                assign component <lsx_frange>-fieldname
                  of structure ls_bkpf to <ld_value>.
                if sy-subrc = 0 and <ld_value> not in <lsx_frange>-selopt_t.
                  ld_skip = abap_true.
                  skip_document = abap_true.
                else.
**                restrictions on converted and original bseg fields.
                  assign component <lsx_frange>-fieldname
                    of structure ls_bseg to <ld_value>.
                  if sy-subrc = 0 and <ld_value> not in <lsx_frange>-selopt_t.
                    ld_skip = abap_true.
                  endif.
                endif.
              endloop.
            endloop.

            check ld_skip is initial.

** SHKZG & XNEGP                                  "begin of note 3086636
            IF   ( ls_bseg-xnegp IS NOT INITIAL AND ls_bseg-shkzg = 'H' )
              OR ( ls_bseg-xnegp IS INITIAL AND ls_bseg-shkzg = 'S' ).
              ls_bseg-shkzg = 'S'.
            ELSE.
              ls_bseg-shkzg = 'H'.
            ENDIF.                                  "end of note 3086636

**          write currency information
            CLEAR ls_curr_help.

            ls_curr_help-zzxarch = abap_true.
            ls_curr_help-counter = 1.

            IF ls_bkpf-bukrs <> ls_t001-bukrs.
              READ TABLE mt_t001
                    INTO ls_t001
                    WITH KEY
                      bukrs = ls_bkpf-bukrs.
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.
              IF ls_t001-waers > space.
                CHECK ls_t001-waers
                  IN lt_ra_lcurr.
              ENDIF.
            ENDIF.

            IF ls_bkpf-bukrs = ls_t001-bukrs.
**            WRBTR -> CURRVAL_0D
              ls_curr_help-currkey_0d = ls_bkpf-waers.
              ls_curr_help-currval_0d = ls_bseg-wrbtr.
              IF ls_bseg-shkzg = 'H'.
                ls_curr_help-currval_0d = -1 * ls_curr_help-currval_0d.
              ENDIF.

**            PSWBT -> CURRVAL_00
              ls_curr_help-currkey_00 = ls_bseg-pswsl.
              ls_curr_help-currval_00 = ls_bseg-pswbt.
              IF ls_bseg-shkzg = 'H'.
                ls_curr_help-currval_00 = -1 * ls_curr_help-currval_00.
              ENDIF.

**            DMBTR -> CURRVAL_10
              ls_curr_help-currkey_10 = ls_t001-waers.
              ls_curr_help-currval_10 = ls_bseg-dmbtr.
              IF ls_bseg-shkzg = 'H'.
                ls_curr_help-currval_10 = -1 * ls_curr_help-currval_10.
              ENDIF.

*             begin of note 3138877
              if ls_bkpf-curt2 = '30'.
**              DMBE2 -> CURRVAL_30
                ls_curr_help-currkey_30 = ls_bkpf-hwae2.
                ls_curr_help-currval_30 = ls_bseg-dmbe2.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_30 = -1 * ls_curr_help-currval_30.
                endif.
              elseif ls_bkpf-curt2 = '40'.
**              DMBE2 -> CURRVAL_40
                ls_curr_help-currkey_40 = ls_bkpf-hwae2.
                ls_curr_help-currval_40 = ls_bseg-dmbe2.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_40 = -1 * ls_curr_help-currval_40.
                endif.
              endif.

              if ls_bkpf-curt3 = '30'.
**              DMBE2 -> CURRVAL_30
                ls_curr_help-currkey_30 = ls_bkpf-hwae3.
                ls_curr_help-currval_30 = ls_bseg-dmbe3.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_30 = -1 * ls_curr_help-currval_30.
                endif.
              elseif ls_bkpf-curt3 = '40'.
**              DMBE2 -> CURRVAL_40
                ls_curr_help-currkey_40 = ls_bkpf-hwae3.
                ls_curr_help-currval_40 = ls_bseg-dmbe3.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_40 = -1 * ls_curr_help-currval_40.
                endif.
              endif.
*             * end of note 3138877
            ENDIF.

**          insert data
            " order: BKPF -> BSEG -> T001 -> CURRENCIES!
            MOVE-CORRESPONDING ls_bkpf TO <ls_data>.
            MOVE-CORRESPONDING ls_bseg TO <ls_data>.
            MOVE-CORRESPONDING ls_t001 TO <ls_data>.
            MOVE-CORRESPONDING ls_curr_help TO <ls_data>.

**          fill ledger
            assign component 'RLDNR' of structure <ls_data> to field-symbol(<rldnr>).
            if sy-subrc = 0.
              <rldnr> = ld_rldnr.
            endif.

**          aggregate
            COLLECT <ls_data> INTO <lt_data>.                   "n2770409
          ENDLOOP.
        endloop.
      ELSE.                                           "end of "n2738710
        CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
          EXPORTING
            i_selections  = ltx_trange
            i_show_errors = ld_show_errors
          TABLES
            e_bkpf        = lt_bkpf
            e_bseg        = lt_bseg
          EXCEPTIONS
            OTHERS        = 1.

        IF sy-subrc <> 0.
          ls_message-type       = sy-msgty.
          ls_message-id         = sy-msgid.
          ls_message-number     = sy-msgno.
          ls_message-message_v1 = sy-msgv1.
          ls_message-message_v2 = sy-msgv2.
          ls_message-message_v3 = sy-msgv3.
          ls_message-message_v4 = sy-msgv4.
          APPEND ls_message
            TO ct_message.
          RETURN.
        ENDIF.

        SORT lt_bkpf.
        SORT lt_bseg.

        CREATE DATA lr_data LIKE LINE OF ct_data.
        ASSIGN lr_data->* TO <ls_data>.

        LOOP AT lt_bkpf
          INTO  ls_bkpf.
          READ TABLE lt_bseg
            WITH KEY
              bukrs = ls_bkpf-bukrs
              belnr = ls_bkpf-belnr
              gjahr = ls_bkpf-gjahr
            BINARY SEARCH
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            LOOP AT lt_bseg
              INTO  ls_bseg
              FROM  sy-tabix.

*           Note 3138877: check main account
            case sy-cprog.
              when 'FAGL_LINE_ITEM_BROWSER_AP'.
                check ls_bseg-koart = 'K'.
              when 'FAGL_LINE_ITEM_BROWSER_AR'.
                check ls_bseg-koart = 'D'.
            endcase.

              IF ls_bseg-bukrs <> ls_bkpf-bukrs
              OR ls_bseg-belnr <> ls_bkpf-belnr
              OR ls_bseg-gjahr <> ls_bkpf-gjahr.
                EXIT.
              ENDIF.
              MOVE-CORRESPONDING ls_bkpf
                TO <ls_data>.

** SHKZG & XNEGP                                  "begin of note 3086636
              IF   ( ls_bseg-xnegp IS NOT INITIAL AND ls_bseg-shkzg = 'H' )
                OR ( ls_bseg-xnegp IS INITIAL AND ls_bseg-shkzg = 'S' ).
                ls_bseg-shkzg = 'S'.
              ELSE.
                ls_bseg-shkzg = 'H'.
              ENDIF.                              "end of note 3086636

              MOVE-CORRESPONDING ls_bseg
                TO <ls_data>.
              IF ls_bkpf-bukrs <> ls_t001-bukrs.
                READ TABLE mt_t001
                  INTO ls_t001
                  WITH KEY
                    bukrs = ls_bkpf-bukrs.
                IF sy-subrc <> 0.
                  CONTINUE.
                ENDIF.
                IF ls_t001-waers > space.
                  CHECK ls_t001-waers
                    IN lt_ra_lcurr.
                ENDIF.
              ENDIF.
              IF ls_bkpf-bukrs = ls_t001-bukrs.
**            WRBTR -> CURRVAL_0D                         "begin of "n2655899
                ls_curr_help-currkey_0d = ls_bkpf-waers.
                ls_curr_help-currval_0d = ls_bseg-wrbtr.
                IF ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_0d = -1 * ls_curr_help-currval_0d.
                ENDIF.

**            PSWBT -> CURRVAL_00
                ls_curr_help-currkey_00 = ls_bseg-pswsl.
                ls_curr_help-currval_00 = ls_bseg-pswbt.
                IF ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_00 = -1 * ls_curr_help-currval_00.
                ENDIF.

**            DMBTR -> CURRVAL_10
                ls_curr_help-currkey_10 = ls_t001-waers.
                ls_curr_help-currval_10 = ls_bseg-dmbtr.
                IF ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_10 = -1 * ls_curr_help-currval_10.
                ENDIF.                                        "end of "n2655899

*             begin of note 3138877
              if ls_bkpf-curt2 = '30'.
*  *            DMBE2 -> CURRVAL_30
                ls_curr_help-currkey_30 = ls_bkpf-hwae2.
                ls_curr_help-currval_30 = ls_bseg-dmbe2.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_30 = -1 * ls_curr_help-currval_30.
                endif.
              elseif ls_bkpf-curt2 = '40'.
*  *            DMBE2 -> CURRVAL_40
                ls_curr_help-currkey_40 = ls_bkpf-hwae2.
                ls_curr_help-currval_40 = ls_bseg-dmbe2.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_40 = -1 * ls_curr_help-currval_40.
                endif.
              endif.

              if ls_bkpf-curt3 = '30'.
*  *            DMBE2 -> CURRVAL_30
                ls_curr_help-currkey_30 = ls_bkpf-hwae3.
                ls_curr_help-currval_30 = ls_bseg-dmbe3.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_30 = -1 * ls_curr_help-currval_30.
                endif.
              elseif ls_bkpf-curt3 = '40'.
*  *            DMBE2 -> CURRVAL_40
                ls_curr_help-currkey_40 = ls_bkpf-hwae3.
                ls_curr_help-currval_40 = ls_bseg-dmbe3.
                if ls_bseg-shkzg = 'H'.
                  ls_curr_help-currval_40 = -1 * ls_curr_help-currval_40.
                endif.
              endif.
*             end of note 3138877
                MOVE-CORRESPONDING ls_t001
                  TO <ls_data>.
                MOVE-CORRESPONDING ls_curr_help
                  TO <ls_data>.
              ENDIF.
              ls_curr_help-zzxarch = abap_true.
              ls_curr_help-counter = 1.                     "n2655899
              MOVE-CORRESPONDING ls_curr_help
                TO <ls_data>.
              COLLECT <ls_data>
                INTO <lt_data>.

            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

**  Read master data if contained in layout         "begin of "n2770409
    DATA ls_struc         TYPE glposnct_std.
    DATA ld_process_texts TYPE boole_d.

    DATA ls_ska1  TYPE ska1.
    DATA ls_skb1  TYPE skb1.
    DATA ls_lfa1  TYPE lfa1.
    DATA ls_lfb1  TYPE lfb1.
    DATA ls_kna1  TYPE kna1.
    DATA ls_knb1  TYPE knb1.
    DATA ls_skat  TYPE skat.
    DATA ls_cepct TYPE cepct.
    DATA ls_cskt  TYPE cskt.
    DATA ls_makt  TYPE makt.
    DATA ls_tfkbt TYPE tfkbt.
    DATA ls_vbak  TYPE vbak.

    DATA ld_sel_ska1    TYPE boole_d.
    DATA ld_sel_skb1    TYPE boole_d.
    DATA ld_sel_lfa1    TYPE boole_d.
    DATA ld_sel_lfb1    TYPE boole_d.
    DATA ld_sel_kna1    TYPE boole_d.
    DATA ld_sel_knb1    TYPE boole_d.
    DATA ld_sel_gl_text TYPE boole_d.
    DATA ld_sel_sk_text TYPE boole_d.
    DATA ld_sel_rp_text TYPE boole_d.
    DATA ld_sel_sp_text TYPE boole_d.
    DATA ld_sel_cc_text TYPE boole_d.
    DATA ld_sel_rf_text TYPE boole_d.
    DATA ld_sel_sf_text TYPE boole_d.
    DATA ld_sel_or_text TYPE boole_d.
    DATA ld_sel_mm_text TYPE boole_d.

    IF md_read_master_data = abap_true.
*   Check for master data fields
      LOOP AT lo_str_descr_in->components
        INTO ls_abap_comp_descr
        WHERE name CP 'SKA1_*'
           OR name CP 'SKB1_*'
           OR name CP 'LFA1_*'
           OR name CP 'LFB1_*'
           OR name CP 'KNA1_*'
           OR name CP 'KNB1_*'
           OR name CP 'GA_*'  "Text for HKONT
           OR name CP 'SK_*'  "Text for SAKNR
           OR name CP 'RP_*'  "Text for Profit Center
           OR name CP 'SP_*'  "Text for Partner Profit Center
           OR name CP 'CC_*'  "Text for Cost Center
           OR name CP 'RF_*'  "Text for Functional Area
           OR name CP 'SF_*'  "Text for Partner Functional Area
           OR name CP 'OR_*'  "Text for Order
           OR name CP 'MM_*'. "Text for Material

        CASE ls_abap_comp_descr-name+0(5).
          WHEN 'SKA1_'.
            ld_sel_ska1       = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'SKB1_'.
            ld_sel_skb1       = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'LFA1_'.
            ld_sel_lfa1       = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'LFB1_'.
            ld_sel_lfb1       = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'KNA1_'.
            ld_sel_kna1       = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'KNB1_'.
            ld_sel_knb1       = abap_true.
            ld_process_texts  = abap_true.
        ENDCASE.

        CASE ls_abap_comp_descr-name+0(3).
          WHEN 'GA_'.
            ld_sel_gl_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'SK_'.
            ld_sel_sk_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'RP_'.
            ld_sel_rp_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'SP_'.
            ld_sel_sp_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'CC_'.
            ld_sel_cc_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'RF_'.
            ld_sel_rf_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'SF_'.
            ld_sel_sf_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'OR_'.
            ld_sel_or_text    = abap_true.
            ld_process_texts  = abap_true.
          WHEN 'MM_'.
            ld_sel_mm_text    = abap_true.
            ld_process_texts  = abap_true.
        ENDCASE.
      ENDLOOP.

    IF ld_process_texts = abap_true.
      LOOP AT <lt_data> ASSIGNING <ls_data>.
**      Fields from SKA1
        IF ld_sel_ska1 IS NOT INITIAL.
          CLEAR: ls_ska1, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          IF ls_struc-ktopl IS INITIAL.
            SELECT SINGLE ktopl
              FROM t001
              INTO ls_struc-ktopl
              WHERE bukrs = ls_struc-bukrs.
          ENDIF.
          SELECT SINGLE *
            FROM ska1
            INTO ls_ska1
            WHERE ktopl = ls_struc-ktopl
            AND   saknr = ls_struc-hkont.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name CS 'SKA1_'.
              ld_where = ls_abap_comp_descr-name.
              REPLACE FIRST OCCURRENCE OF 'SKA1_'
                IN ld_where WITH ''.
              ASSIGN COMPONENT ld_where OF STRUCTURE ls_ska1
                TO <ld_value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT ls_abap_comp_descr-name
                  OF STRUCTURE <ls_data> TO <ld_data>.
                IF sy-subrc = 0.
                  <ld_data> = <ld_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Texts for HKONT
        IF ld_sel_gl_text IS NOT INITIAL.
          CLEAR: ls_skat, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          IF ls_struc-ktopl IS INITIAL.
            SELECT SINGLE ktopl
              FROM t001
              INTO ls_struc-ktopl
              WHERE bukrs = ls_struc-bukrs.
          ENDIF.
          SELECT SINGLE *
            FROM skat
            INTO ls_skat
            WHERE spras = sy-langu
            AND   ktopl = ls_struc-ktopl
            AND   saknr = ls_struc-hkont.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'GA_TXT20' OR name = 'GA_TXT50'.
              ASSIGN COMPONENT 'GA_TXT20' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_skat-txt20.
              ENDIF.
              ASSIGN COMPONENT 'GA_TXT50' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_skat-txt50.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Texts for SAKNR
        IF ld_sel_sk_text IS NOT INITIAL.
          CLEAR: ls_skat, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          IF ls_struc-ktopl IS INITIAL.
            SELECT SINGLE ktopl
              FROM t001
              INTO ls_struc-ktopl
              WHERE bukrs = ls_struc-bukrs.
          ENDIF.
          SELECT SINGLE *
            FROM skat
            INTO ls_skat
            WHERE spras = sy-langu
            AND   ktopl = ls_struc-ktopl
            AND   saknr = ls_struc-saknr.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'SK_TXT20' OR name = 'SK_TXT50'.
              ASSIGN COMPONENT 'SK_TXT20' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_skat-txt20.
              ENDIF.
              ASSIGN COMPONENT 'SK_TXT50' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_skat-txt50.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Fields from SKB1
        IF ld_sel_skb1 IS NOT INITIAL.
          CLEAR: ls_skb1, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          SELECT SINGLE *
            FROM skb1
            INTO ls_skb1
            WHERE bukrs = ls_struc-bukrs
            AND   saknr = ls_struc-hkont.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name CS 'SKB1_'.
              ld_where = ls_abap_comp_descr-name.
              REPLACE FIRST OCCURRENCE OF 'SKB1_'
                IN ld_where WITH ''.
              ASSIGN COMPONENT ld_where OF STRUCTURE ls_skb1
                TO <ld_value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT ls_abap_comp_descr-name
                  OF STRUCTURE <ls_data> TO <ld_data>.
                IF sy-subrc = 0.
                  <ld_data> = <ld_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Fields from KNA1
        IF ld_sel_kna1 IS NOT INITIAL.
          CLEAR: ls_kna1, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          SELECT SINGLE *
            FROM kna1
            INTO ls_kna1
            WHERE kunnr = ls_struc-kunnr.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name CS 'KNA1_'.
              ld_where = ls_abap_comp_descr-name.
              REPLACE FIRST OCCURRENCE OF 'KNA1_'
                IN ld_where WITH ''.
              ASSIGN COMPONENT ld_where OF STRUCTURE ls_kna1
                TO <ld_value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT ls_abap_comp_descr-name
                  OF STRUCTURE <ls_data> TO <ld_data>.
                IF sy-subrc = 0.
                  <ld_data> = <ld_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Fields from KNB1
        IF ld_sel_knb1 IS NOT INITIAL.
          CLEAR: ls_knb1, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          SELECT SINGLE *
            FROM knb1
            INTO ls_knb1
            WHERE bukrs = ls_struc-bukrs
            AND   kunnr = ls_struc-kunnr.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name CS 'KNB1_'.
              ld_where = ls_abap_comp_descr-name.
              REPLACE FIRST OCCURRENCE OF 'KNB1_'
                IN ld_where WITH ''.
              ASSIGN COMPONENT ld_where OF STRUCTURE ls_knb1
                TO <ld_value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT ls_abap_comp_descr-name
                  OF STRUCTURE <ls_data> TO <ld_data>.
                IF sy-subrc = 0.
                  <ld_data> = <ld_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Fields from LFA1
        IF ld_sel_lfa1 IS NOT INITIAL.
          CLEAR: ls_kna1, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          SELECT SINGLE *
            FROM lfa1
            INTO ls_lfa1
            WHERE lifnr = ls_struc-lifnr.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name CS 'LFA1_'.
              ld_where = ls_abap_comp_descr-name.
              REPLACE FIRST OCCURRENCE OF 'LFA1_'
                IN ld_where WITH ''.
              ASSIGN COMPONENT ld_where OF STRUCTURE ls_lfa1
                TO <ld_value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT ls_abap_comp_descr-name
                  OF STRUCTURE <ls_data> TO <ld_data>.
                IF sy-subrc = 0.
                  <ld_data> = <ld_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Fields from LFB1
        IF ld_sel_lfb1 IS NOT INITIAL.
          CLEAR: ls_knb1, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          SELECT SINGLE *
            FROM lfb1
            INTO ls_lfb1
            WHERE lifnr = ls_struc-lifnr
            AND   bukrs = ls_struc-bukrs.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name CS 'LFB1_'.
              ld_where = ls_abap_comp_descr-name.
              REPLACE FIRST OCCURRENCE OF 'LFB1_'
                IN ld_where WITH ''.
              ASSIGN COMPONENT ld_where OF STRUCTURE ls_lfb1
                TO <ld_value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT ls_abap_comp_descr-name
                  OF STRUCTURE <ls_data> TO <ld_data>.
                IF sy-subrc = 0.
                  <ld_data> = <ld_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Profit Center
        IF ld_sel_rp_text IS NOT INITIAL.
          CLEAR: ls_cepct, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          IF ls_struc-kokrs IS INITIAL.
            SELECT SINGLE kokrs
              FROM tka02
              INTO ls_struc-kokrs
              WHERE bukrs = ls_struc-bukrs.
          ENDIF.

          CLEAR ld_date.
          SELECT SINGLE MAX( datbi )
            FROM cepct
            INTO ld_date
            WHERE spras = sy-langu
            AND   prctr = ls_struc-prctr
            AND   kokrs = ls_struc-kokrs. "#EC CI_BYPASS

          SELECT SINGLE *
            FROM cepct
            INTO ls_cepct
            WHERE spras = sy-langu
            AND   prctr = ls_struc-prctr
            AND   datbi = ld_date
            AND   kokrs = ls_struc-kokrs.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'RP_KTEXT' OR name = 'RP_LTEXT'.
              ASSIGN COMPONENT 'RP_KTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_cepct-ktext.
              ENDIF.
              ASSIGN COMPONENT 'RP_LTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_cepct-ltext.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Partner Profit Center
        IF ld_sel_sp_text IS NOT INITIAL.
          CLEAR: ls_cepct, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          IF ls_struc-kokrs IS INITIAL.
            SELECT SINGLE kokrs
              FROM tka02
              INTO ls_struc-kokrs
              WHERE bukrs = ls_struc-bukrs.
          ENDIF.

          CLEAR ld_date.
          SELECT SINGLE MAX( datbi )
            FROM cepct
            INTO ld_date
            WHERE spras = sy-langu
            AND   prctr = ls_struc-pprct
            AND   kokrs = ls_struc-kokrs. "#EC CI_BYPASS

          SELECT SINGLE *
            FROM cepct
            INTO ls_cepct
            WHERE spras = sy-langu
            AND   prctr = ls_struc-pprct
            AND   datbi = ld_date
            AND   kokrs = ls_struc-kokrs. "#EC CI_BYPASS
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'SP_KTEXT' OR name = 'SP_LTEXT'.
              ASSIGN COMPONENT 'SP_KTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_cepct-ktext.
              ENDIF.
              ASSIGN COMPONENT 'SP_LTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_cepct-ltext.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Cost Center
        IF ld_sel_cc_text IS NOT INITIAL.
          CLEAR: ls_cskt, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.
          IF ls_struc-kokrs IS INITIAL.
            SELECT SINGLE kokrs
              FROM tka02
              INTO ls_struc-kokrs
              WHERE bukrs = ls_struc-bukrs.
          ENDIF.

          CLEAR ld_date.
          SELECT SINGLE MAX( datbi )
            FROM cskt
            INTO ld_date
            WHERE spras = sy-langu
            AND   kostl = ls_struc-kostl
            AND   kokrs = ls_struc-kokrs. "#EC CI_BYPASS.

          SELECT SINGLE *
            FROM cskt
            INTO ls_cskt
            WHERE spras = sy-langu
            AND   kostl = ls_struc-kostl
            AND   datbi = ld_date
            AND   kokrs = ls_struc-kokrs.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'CC_KTEXT' OR name = 'CC_LTEXT'.
              ASSIGN COMPONENT 'CC_KTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_cskt-ktext.
              ENDIF.
              ASSIGN COMPONENT 'CC_LTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_cskt-ltext.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Functional Area
        IF ld_sel_rf_text IS NOT INITIAL.
          CLEAR: ls_tfkbt, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.

          SELECT SINGLE *
            FROM tfkbt
            INTO ls_tfkbt
            WHERE spras = sy-langu
            AND   fkber = ls_struc-fkber_long.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'RF_FKBTX'.
              ASSIGN COMPONENT 'RF_FKBTX' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_tfkbt-fkbtx.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Partner Functional Area
        IF ld_sel_sf_text IS NOT INITIAL.
          CLEAR: ls_tfkbt, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.

          SELECT SINGLE *
            FROM tfkbt
            INTO ls_tfkbt
            WHERE spras = sy-langu
            AND   fkber = ls_struc-pfkber.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'SF_FKBTX'.
              ASSIGN COMPONENT 'SF_FKBTX' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_tfkbt-fkbtx.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Order
        IF ld_sel_or_text IS NOT INITIAL.
          CLEAR: ls_vbak, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.

          SELECT SINGLE *
            FROM vbak
            INTO ls_vbak
            WHERE vbeln = ls_struc-vbeln.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'OR_KTEXT'.
              ASSIGN COMPONENT 'OR_KTEXT' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_vbak-ktext.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

**      Material
        IF ld_sel_mm_text IS NOT INITIAL.
          CLEAR: ls_makt, ls_struc.
          MOVE-CORRESPONDING <ls_data> TO ls_struc.

          SELECT SINGLE *
            FROM makt
            INTO ls_makt
            WHERE spras = sy-langu
            AND   matnr = ls_struc-matnr.
          IF sy-subrc = 0.
            LOOP AT lo_str_descr_in->components
              INTO ls_abap_comp_descr
              WHERE name = 'MM_MAKTX'.
              ASSIGN COMPONENT 'MM_MAKTX' OF STRUCTURE <ls_data>
                TO <ld_value>.
              IF sy-subrc = 0.
                <ld_value> = ls_makt-maktx.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
       ENDLOOP.
    ENDIF.
  ENDIF.
**                                                    "end of "n2770409

    IF ld_no_collect = abap_true.
      APPEND LINES OF ct_data
        TO <lt_data>.
    ELSE.
      LOOP AT ct_data
        ASSIGNING <ls_data>.
        COLLECT <ls_data>
          INTO <lt_data>.
      ENDLOOP.
    ENDIF.
    ct_data = <lt_data>.

* Note 3138877: Flag Cleared Items
  read table it_field_list with table key table_line = 'X_CLEARED_ITEM' transporting no fields.

  if sy-subrc = 0.
    " get ref date
    lv_hint = id_hint.

    split lv_hint at `'` into table lt_hint_split.

    read table lt_hint_split with key table_line = '$$refdate$$' transporting no fields.

    if sy-subrc = 0.
      read table lt_hint_split index sy-tabix + 2 into refdate.
    endif.

    if refdate is initial.
      refdate = '99991231'.
    endif.

    loop at ct_data assigning <ls_data>.
      assign component 'X_CLEARED_ITEM' of structure <ls_data> to <cleared>.
      assign component 'AUGDT' of structure <ls_data> to <augdt>.

      if <augdt> is assigned.
        if <augdt> = '00000000' or <augdt> > refdate.
          <cleared> = ''.
        else.
          <cleared> = 'X'.
        endif.
      else.
        "augdt not provided? we assume archived data is cleared
        <cleared> = 'X'.
      endif.
    endloop.
  endif.

  ENDMETHOD.


  method IF_FAGL_LIB~SIDEBAR_ACTIONS_HANDLE.
  endmethod.


  method IF_FAGL_LIB~SIDEBAR_ACTIONS_SET.
  endmethod.
ENDCLASS.

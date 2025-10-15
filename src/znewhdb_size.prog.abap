*&---------------------------------------------------------------------*
*& Report  /SDF/HDB_SIZING // ZNEWHDB_SIZE
*&
*&---------------------------------------------------------------------*
*& Estimates the memory requirement in HANA of non-HANA databases      *
*& When run on HANA, size the database using real memory consumption
*& values.
*&---------------------------------------------------------------------*
*
*
REPORT znewhdb_size     LINE-SIZE 101 LINE-COUNT 90
                        NO STANDARD PAGE HEADING.
DATA: l_version TYPE c LENGTH 6 VALUE '85'.

TYPES: ty_para(2) TYPE n,
       ty_top(3)  TYPE n.
TYPE-POOLS: abap.
DATA: funcname  TYPE rs38l_fnam,
      tabname   TYPE tabname,
      subrc     TYPE sy-subrc,
      p_prf     TYPE abap_bool,
      outln(80) TYPE c,
      do_soh    TYPE boolean,
      do_sfin   TYPE boolean,
      do_slog   TYPE boolean,
      do_sml    TYPE boolean,
      in_sfin   TYPE boolean,
      in_slog   TYPE boolean,
      in_sml    TYPE boolean,
      p_mshl    TYPE boolean,
      p_lraw    TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK scope WITH FRAME TITLE c_t_t0.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (48) c_t_000 FOR FIELD so_tab.
    SELECT-OPTIONS: so_tab FOR tabname.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_t_019 FOR FIELD p_load MODIF ID hdb.
    PARAMETERS: p_load AS CHECKBOX DEFAULT abap_false
                                   TYPE abap_bool MODIF ID hdb.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_t_020 FOR FIELD p_hl MODIF ID hdb.
    PARAMETERS: p_hl AS CHECKBOX DEFAULT abap_true
                                   TYPE abap_bool MODIF ID hdb.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_t_021 FOR FIELD p_mac MODIF ID hdb.
    PARAMETERS: p_mac AS CHECKBOX DEFAULT abap_false
                                   TYPE abap_bool MODIF ID hdb.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_t_023 FOR FIELD p_empt MODIF ID hdb.
    PARAMETERS: p_empt AS CHECKBOX DEFAULT abap_false
                                   TYPE abap_bool MODIF ID hdb.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_norm TYPE c RADIOBUTTON GROUP sce USER-COMMAND hid.
    SELECTION-SCREEN COMMENT (79) c_t_040 FOR FIELD p_norm.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_disp TYPE c RADIOBUTTON GROUP sce.
    SELECTION-SCREEN COMMENT (49) c_t_042 FOR FIELD p_disp.
    PARAMETERS: p_guid TYPE guid_16.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS: p_ocpu TYPE c RADIOBUTTON GROUP sce.
    SELECTION-SCREEN COMMENT (49) c_t_041 FOR FIELD p_ocpu.
    PARAMETERS: p_guid_c TYPE guid_16.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK scope.

SELECTION-SCREEN BEGIN OF BLOCK scenario WITH FRAME TITLE c_t_t2.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_soh TYPE c RADIOBUTTON GROUP sol.
    SELECTION-SCREEN COMMENT (79) c_4_001 FOR FIELD p_soh.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_sfin TYPE c RADIOBUTTON GROUP sol MODIF ID fin.
    SELECTION-SCREEN COMMENT (79) c_4_002 FOR FIELD p_sfin MODIF ID fin.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_slog TYPE c RADIOBUTTON GROUP sol MODIF ID log.
    SELECTION-SCREEN COMMENT (79) c_4_003 FOR FIELD p_slog MODIF ID log.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK scenario.

SELECTION-SCREEN BEGIN OF BLOCK db_version WITH FRAME TITLE c_t_t3.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_hdb1 TYPE c RADIOBUTTON GROUP any MODIF ID any.
    SELECTION-SCREEN COMMENT (79) c_h_001 FOR FIELD p_hdb1 MODIF ID any.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_hdb2 TYPE c DEFAULT 'X'
                                     RADIOBUTTON GROUP any MODIF ID any.
    SELECTION-SCREEN COMMENT (79) c_h_002 FOR FIELD p_hdb2 MODIF ID any.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK db_version.

SELECTION-SCREEN BEGIN OF BLOCK basis_version WITH FRAME TITLE c_t_t5.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_bas1 TYPE c RADIOBUTTON GROUP bas MODIF ID any.
    SELECTION-SCREEN COMMENT (79) c_b_001 FOR FIELD p_bas1 MODIF ID any.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_bas2 TYPE c DEFAULT 'X'
                                     RADIOBUTTON GROUP bas MODIF ID any.
    SELECTION-SCREEN COMMENT (79) c_b_002 FOR FIELD p_bas2 MODIF ID any.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK basis_version.

SELECTION-SCREEN BEGIN OF BLOCK warm_data WITH FRAME TITLE c_t_t4.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: p_dag TYPE c RADIOBUTTON GROUP nse.
*SELECTION-SCREEN COMMENT (79) c_f_011 FOR FIELD p_dag.
*SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_f_008 FOR FIELD p_daag.
    PARAMETERS: p_daag(3) TYPE n DEFAULT 365.
  SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: p_nse TYPE c DEFAULT 'X'
*                                 RADIOBUTTON GROUP nse.
*SELECTION-SCREEN COMMENT (79) c_f_012 FOR FIELD p_nse.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT (51) c_f_013 FOR FIELD p_nser.
*PARAMETERS: p_nser TYPE i DEFAULT 500000000.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK warm_data.

SELECTION-SCREEN BEGIN OF BLOCK paral WITH FRAME TITLE c_t_t1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_f_003 FOR FIELD p_paral.
    PARAMETERS: p_paral TYPE ty_para DEFAULT 1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_f_010 FOR FIELD p_group.
    PARAMETERS: p_group TYPE rzllitab-classname
            MATCHCODE OBJECT spta_server_group.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_f_004 FOR FIELD p_topobj.
    PARAMETERS: p_topobj TYPE ty_top DEFAULT 30.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_s_001 FOR FIELD p_forc MODIF ID sta.
    PARAMETERS p_forc AS CHECKBOX TYPE boolean
      DEFAULT abap_false USER-COMMAND mode MODIF ID sta.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_s_002 FOR FIELD p_sage MODIF ID sta.
    PARAMETERS p_sage TYPE i DEFAULT 5 MODIF ID sta.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_s_003 FOR FIELD p_mrec MODIF ID sta.
    PARAMETERS p_mrec TYPE i DEFAULT 10000000 MODIF ID sta.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK paral.

SELECTION-SCREEN BEGIN OF BLOCK accuracy WITH FRAME TITLE c_t_t11.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: prec_hi TYPE c RADIOBUTTON GROUP prec MODIF ID any.
    SELECTION-SCREEN COMMENT (12) c_f_005 FOR FIELD prec_hi MODIF ID any.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: prec_med TYPE c RADIOBUTTON GROUP prec DEFAULT 'X'
      MODIF ID any.
    SELECTION-SCREEN COMMENT (12) c_f_006 FOR FIELD prec_med MODIF ID any.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: prec_low TYPE c RADIOBUTTON GROUP prec MODIF ID any.
    SELECTION-SCREEN COMMENT (12) c_f_007 FOR FIELD prec_low MODIF ID any.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (51) c_t_022 FOR FIELD p_clus MODIF ID any.
    PARAMETERS: p_clus TYPE c DEFAULT 'S' MODIF ID any.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK accuracy.

SELECTION-SCREEN BEGIN OF BLOCK distribution WITH FRAME TITLE c_h_003.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (48) c_t_008 FOR FIELD so_rstab MODIF ID any.
    SELECT-OPTIONS: so_rstab FOR tabname NO INTERVALS MODIF ID any.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (48) c_t_009 FOR FIELD so_cstab MODIF ID any.
    SELECT-OPTIONS: so_cstab FOR tabname NO INTERVALS MODIF ID any.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK distribution.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (48) c_t_013 FOR FIELD p_calib.
  PARAMETERS: p_calib AS CHECKBOX DEFAULT abap_false MODIF ID cal.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (48) c_t_015 FOR FIELD p_db.
  PARAMETERS: p_db AS CHECKBOX DEFAULT abap_false MODIF ID db.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (48) c_t_018 FOR FIELD p_ful.
  PARAMETERS: p_ful AS CHECKBOX DEFAULT abap_false MODIF ID ful.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (48) c_t_016 FOR FIELD p_buk MODIF ID prf.
  PARAMETERS: p_buk TYPE i DEFAULT 100 MODIF ID prf.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (48) c_t_017 FOR FIELD p_dump MODIF ID prf.
  PARAMETERS: p_dump TYPE i DEFAULT 20 MODIF ID prf.
SELECTION-SCREEN END OF LINE.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'CAL' AND sy-dbsys(3) = 'HDB'.
    p_calib = abap_true. p_topobj = 50.
    p_mac   = abap_true. p_load = abap_true.
    p_empt  = abap_true.
    CLEAR: prec_hi, prec_low. prec_med = abap_true.
    CLEAR: p_sfin,  p_slog. p_soh = abap_true.
  ENDIF.
  IF sy-ucomm = 'DB' OR sy-ucomm = 'CAL'.
    p_db = abap_true.
  ENDIF.
  IF sy-ucomm = 'FUL'.
    p_ful = abap_true.
  ENDIF.
  IF sy-ucomm = 'PRF'.
    p_prf = abap_true.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  do_soh = p_soh.
  do_sfin = p_sfin.
  do_slog = p_slog.

  CALL FUNCTION 'DD_EXIST_TABLE'
    EXPORTING
      tabname      = 'BSIS'
      status       = 'A'
    IMPORTING
      subrc        = subrc
    EXCEPTIONS
      wrong_status = 1
      OTHERS       = 2.
  IF subrc = 0 AND sy-subrc = 0.
    do_sfin = abap_true.
  ENDIF.
  CALL FUNCTION 'DD_EXIST_TABLE'
    EXPORTING
      tabname      = 'VBAK'
      status       = 'A'
    IMPORTING
      subrc        = subrc
    EXCEPTIONS
      wrong_status = 1
      OTHERS       = 2.
  IF subrc = 0 AND sy-subrc = 0.
    do_slog = abap_true.
  ENDIF.
  CALL FUNCTION 'DD_EXIST_TABLE'
    EXPORTING
      tabname      = 'ACDOCA'
      status       = 'A'
    IMPORTING
      subrc        = subrc
    EXCEPTIONS
      wrong_status = 1
      OTHERS       = 2.
  IF subrc = 0 AND sy-subrc = 0.
    in_sfin = abap_true.
  ENDIF.
  CALL FUNCTION 'DD_EXIST_TABLE'
    EXPORTING
      tabname      = 'MATDOC'
      status       = 'A'
    IMPORTING
      subrc        = subrc
    EXCEPTIONS
      wrong_status = 1
      OTHERS       = 2.
  IF subrc = 0 AND sy-subrc = 0.
    in_slog = abap_true.
    CALL FUNCTION 'DD_EXIST_TABLE'
      EXPORTING
        tabname      = 'MLDOC'
        status       = 'A'
      IMPORTING
        subrc        = subrc
      EXCEPTIONS
        wrong_status = 1
        OTHERS       = 2.
    IF subrc = 0 AND sy-subrc = 0.
      in_sml = abap_true.
    ELSE.
      do_sml = abap_true.
    ENDIF.
  ENDIF.

  "grey out if not relevant
  IF p_ocpu = abap_true OR p_disp = abap_true.
    LOOP AT SCREEN.
      IF    screen-name <> 'P_GUID'
        AND screen-name <> 'P_GUID_C'
        AND screen-name <> 'P_OCPU'
        AND screen-name <> 'P_DISP'
        AND screen-name <> 'P_NORM'
        AND screen-name <> 'P_TOPOBJ'.
        screen-input = 0. MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "make invisible if not relevant.
  LOOP AT SCREEN.
    IF sy-dbsys(3) <> 'HDB' AND screen-group1 = 'HDB'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF sy-dbsys(3) <> 'ORA' AND screen-group1 = 'STA'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF p_calib <> abap_true AND screen-group1 = 'CAL'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF p_ful <> abap_true AND screen-group1 = 'FUL'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF sy-dbsys(3) = 'HDB' AND p_calib <> abap_true
                           AND screen-group1 = 'ANY'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF p_db <> abap_true AND screen-group1 = 'DB'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF p_prf <> abap_true AND screen-group1 = 'PRF'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF ( in_slog = abap_true OR do_slog = abap_false )
        AND screen-group1 = 'LOG'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF ( in_sfin = abap_true OR do_sfin = abap_false )
        AND screen-group1 = 'FIN'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'P_DISP'.
      CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
        EXPORTING
          function_module = '/SDF/READ_HDB_SIZING_RESULTS_X'
        EXCEPTIONS
          not_existent    = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        screen-active = '0'. MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name = 'P_GUID' OR screen-name = 'P_GUID_C'.
      CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
        EXPORTING
          function_module = '/SDF/READ_HDB_SIZING_RESULTS_X'
        EXCEPTIONS
          not_existent    = 1
          OTHERS          = 2.
      IF sy-subrc = 0.
        IF p_ocpu = abap_true OR p_disp = abap_true.
          screen-input = '1'. MODIFY SCREEN.
        ELSE.
          screen-input = '0'. MODIFY SCREEN.
          CLEAR p_guid.
        ENDIF.
      ELSE.
        screen-active = '0'. MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF p_forc = abap_false
      AND ( screen-name = 'P_MREC' OR screen-name = 'P_SAGE' ).
      screen-input = '0'. MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.
  c_t_t0  = 'Scope:'.                                       "#EC NOTEXT
  c_t_t2  = 'Choice of the sizing scenario:'.               "#EC NOTEXT
  c_t_t3  = 'Choice of the target HANA version:'.           "#EC NOTEXT
  c_t_t4  = 'Warm store estimations on technical objects:'. "#EC NOTEXT
  c_t_t1  = 'Technical options:'.                           "#EC NOTEXT
  c_t_t5  = 'Choice of the target ABAP kernel version:'.    "#EC NOTEXT
  c_t_000 = 'List of tables (Leave empty for full sizing)'. "#EC NOTEXT
  c_t_040 = 'Calculate a new memory, disk and CPU sizing'.  "#EC NOTEXT
  c_t_041 = 'Renew CPU Sizing using a stored result:'.      "#EC NOTEXT
  c_t_042 = 'Display a stored sizing result:'.              "#EC NOTEXT
  c_f_008 = 'Data aging residence time in days'.            "#EC NOTEXT
  c_f_003 = 'Number of parallel dialog processes'.          "#EC NOTEXT
  c_f_010 = 'Server group (Leave empty to use all servers)'. "#EC NOTEXT
*  c_f_011 = 'Data aging'.                                   "#EC NOTEXT
*  c_f_012 = 'Near Storage Extension'.                       "#EC NOTEXT
*  c_f_013 = 'NSE Minimum number of rows'.                   "#EC NOTEXT
  c_f_004 = 'Number of tables displayed in output'.         "#EC NOTEXT
  c_f_005 = ' 1.000.000'.                                   "#EC NOTEXT
  c_f_006 = ' 100.000'.                                     "#EC NOTEXT
  c_f_007 = ' 10.000'.                                      "#EC NOTEXT
  CONCATENATE 'Force a count if statistics are old'
    '(Slow)' INTO c_s_001 SEPARATED BY space.               "#EC NOTEXT
  c_s_002 = 'Maximum age in years of database statistics'.  "#EC NOTEXT
  c_s_003 = 'Minimum number of estimated records for count'. "#EC NOTEXT
  c_t_t11 = 'Maximum size of samples: (Keep default values)'.
                                                            "#EC NOTEXT
  CONCATENATE 'Changes to stores distribution:'             "#EC NOTEXT
              '(Leave empty to use default distribution)'   "#EC NOTEXT
              INTO c_h_003 SEPARATED BY space.
  c_t_008 = 'List of tables to add to standard row store'.  "#EC NOTEXT
  c_t_009 = 'List of tables to add to standard column store'.
                                                            "#EC NOTEXT
  IF sy-dbsys(3) = 'HDB'.
    CONCATENATE 'Check and verify the memory Sizing of this HANA'
                'system' INTO c_4_001 SEPARATED BY space.   "#EC NOTEXT
  ELSE.
    c_4_001 = 'Perform Business Suite on HANA Sizing'.      "#EC NOTEXT
  ENDIF.
  c_4_002 = 'Perform Sizing of S/4HANA Finance'.            "#EC NOTEXT
  c_4_003 = 'Perform Sizing of S/4HANA'.                    "#EC NOTEXT
  c_h_001 = 'HANA 1.0'.                                     "#EC NOTEXT
  c_h_002 = 'HANA 2.0'.                                     "#EC NOTEXT
  c_b_001 = 'SAP_BASIS <= 7.52'.                            "#EC NOTEXT
  c_b_002 = 'SAP_BASIS >= 7.53'.                            "#EC NOTEXT
  c_t_013 = 'Calibrate Sizing report on HANA'.              "#EC NOTEXT
  c_t_015 = 'Write down full details to database'.          "#EC NOTEXT
  c_t_016 = 'Package Size'.                                 "#EC NOTEXT
  c_t_017 = 'Maximum number of dumps allowed'.              "#EC NOTEXT
  c_t_018 = 'Display more details'.                         "#EC NOTEXT
  c_t_019 = 'Size data that is currently unloaded '.        "#EC NOTEXT
  c_t_020 = 'Size hybrid lobs (Slow on HANA 1.0 SP11 and lower)'.
                                                            "#EC NOTEXT
  c_t_021 = 'Merge and compress if necessary'.              "#EC NOTEXT
  c_t_022 = 'Cluster sample size'.                          "#EC NOTEXT
  c_t_023 = 'Include empty tables'.                         "#EC NOTEXT

START-OF-SELECTION.
  "Start of alignement with FM include
  TYPES:
    BEGIN OF ty_rs_size,
      column_name(256) TYPE c,
      keyflag          TYPE keyflag,
      has_index(1)     TYPE c,
      record_count     TYPE dec20,
      variable_size    TYPE dec20,
      fixed_size       TYPE dec20,
      est_ms_total     TYPE dec20,
      ms_hl            TYPE dec20,
      est_ms_hl        TYPE dec20,
      typekind         TYPE typekind,
      size_byte        TYPE p LENGTH 9 DECIMALS 2,
      lob_size_byte    TYPE p LENGTH 9 DECIMALS 2,
      dlob_size_byte   TYPE p LENGTH 9 DECIMALS 2,
      dlob_count       TYPE dec20,
      mlob_size_byte   TYPE p LENGTH 9 DECIMALS 2,
      mlob_count       TYPE dec20,
    END OF ty_rs_size,

    BEGIN OF ty_cs_size,
      column_name(256)      TYPE c,
      part_id               TYPE int4,
      keyflag               TYPE keyflag,
      has_index(1)          TYPE c,
      "U=>Unique Inverted
      "N=>Non-Unique Inverted
      "P=>Part of concat (unique or not)
      client                TYPE boolean,
*      nopk_type(1)          TYPE c,
      ms_total              TYPE dec20,
      ms_main               TYPE dec20,
      est_ms_main           TYPE dec20,
      record_count          TYPE dec20,
      distinct_count        TYPE dec20,
      low_distinct          TYPE p LENGTH 9 DECIMALS 2,
      loaded(5)             TYPE c,
      index_loaded(16)      TYPE c,
      compression_type(16)  TYPE c,
      cs_data_type_name(16) TYPE c,
      load_unit(6)          TYPE c,
      ms_dict               TYPE dec20,
      est_ms_dict           TYPE dec20,
      ms_index              TYPE dec20,
      est_ms_index          TYPE dec20,
      ms_data               TYPE dec20,
      est_ms_data           TYPE dec20,
      ms_misc               TYPE dec20,
      est_ms_misc           TYPE dec20,
      est_ms_hl             TYPE dec20,
      ms_hl                 TYPE dec20,
      typekind              TYPE typekind,
      size_byte             TYPE p LENGTH 9 DECIMALS 2,
      opt_size_byte         TYPE p LENGTH 9 DECIMALS 2,
      lob_size_byte         TYPE p LENGTH 9 DECIMALS 2,
      dlob_size_byte        TYPE p LENGTH 9 DECIMALS 2,
      dlob_count            TYPE dec20,
      mlob_size_byte        TYPE p LENGTH 9 DECIMALS 2,
      mlob_count            TYPE dec20,
      spec_count            TYPE dec20,
    END OF ty_cs_size,

    BEGIN OF ty_hana_db_size,
      table_name        TYPE tabname,
      part_id           TYPE int4,
      record_count      TYPE dec20,
      source_size       TYPE p LENGTH 9 DECIMALS 1,
      est_size          TYPE p LENGTH 9 DECIMALS 1,
      est_hl_size       TYPE p LENGTH 9 DECIMALS 1,
      est_age_size      TYPE p LENGTH 9 DECIMALS 1,
      est_pk_size       TYPE p LENGTH 9 DECIMALS 1,
      est_nuk_size      TYPE p LENGTH 9 DECIMALS 1,
      real_size         TYPE p LENGTH 9 DECIMALS 1,
      real_hl_size      TYPE p LENGTH 9 DECIMALS 1,
      real_jec_size     TYPE p LENGTH 9 DECIMALS 1,
      real_esh_size     TYPE p LENGTH 9 DECIMALS 1,
      real_age_size     TYPE p LENGTH 9 DECIMALS 1,
      real_pk_size      TYPE p LENGTH 9 DECIMALS 1,
      real_nuk_size     TYPE p LENGTH 9 DECIMALS 1,
      real_rs_exsize    TYPE p LENGTH 9 DECIMALS 1,
      real_hlrs_exsize  TYPE p LENGTH 9 DECIMALS 1,
      opt_size          TYPE p LENGTH 9 DECIMALS 1,
      return_code       TYPE sysubrc,
      error_message     TYPE char80,
      doubt_reason(2)   TYPE n,
      count_method(1)   TYPE c,
      stat_age          TYPE i,
      records_sampled   TYPE p LENGTH 12 DECIMALS 0,
      documents_sampled TYPE p LENGTH 12 DECIMALS 4,
      db_table          TYPE tabname,
      tab_type          TYPE tabclass,
      nopk_type(1)      TYPE c,
      load_unit(6)      TYPE c,
      bucket            TYPE int2,
      store(2)          TYPE c,
      sogroup(30)       TYPE c,
      clitab            TYPE abap_bool,
      rs_cols           TYPE STANDARD TABLE OF ty_rs_size
                   WITH DEFAULT KEY,
      cs_cols           TYPE STANDARD TABLE OF ty_cs_size
                   WITH DEFAULT KEY,
    END OF ty_hana_db_size,
    ty_t_hana_db_size TYPE STANDARD TABLE OF ty_hana_db_size,

    BEGIN OF ty_columns,
      table_name       TYPE tabname,
      column_name(256) TYPE c,
      keyflag          TYPE keyflag,
      has_index(1)     TYPE c,
      typekind         TYPE typekind,
      client           TYPE boolean,
      distinct_count   TYPE dec20,
      low_distinct     TYPE p LENGTH 9 DECIMALS 2,
      size_byte        TYPE p LENGTH 9 DECIMALS 2,
      lob_size_byte    TYPE p LENGTH 9 DECIMALS 2,
      opt_size_byte    TYPE p LENGTH 9 DECIMALS 2,
      dlob_size_byte   TYPE p LENGTH 9 DECIMALS 2,
      dlob_count       TYPE dec20,
      mlob_size_byte   TYPE p LENGTH 9 DECIMALS 2,
      mlob_count       TYPE dec20,
      spec_count       TYPE dec20,
    END OF ty_columns,
    "End of alignment with FM include

    BEGIN OF ty_error,
      table_name      TYPE tabname,
      db_table        TYPE tabname,
      return_code     TYPE sysubrc,
      error_message   TYPE char80,
      source_size     TYPE dec20,
      records_sampled TYPE ty_hana_db_size-records_sampled,
    END OF ty_error,
    ty_t_error TYPE STANDARD TABLE OF ty_error,

    " HDB Sizing
    BEGIN OF ty_realcs,
      table_name            TYPE tabname,
      column_name(256)      TYPE c,
      part_id               TYPE int4,
      ms_total              TYPE dec20,
      ms_main               TYPE dec20,
      record_count          TYPE dec20,
      distinct_count        TYPE dec20,
      loaded(5)             TYPE c,
      index_loaded(16)      TYPE c,
      compression_type(16)  TYPE c,
      cs_data_type_name(16) TYPE c,
      load_unit(6)          TYPE c,
      ms_dict               TYPE dec20,
      ms_index              TYPE dec20,
      ms_data               TYPE dec20,
      ms_misc               TYPE dec20,
      ms_hl                 TYPE dec20,
      spec_count            TYPE dec20,
    END OF ty_realcs,

    BEGIN OF ty_realrs,
      table_name    TYPE tabname,
      record_count  TYPE dec20,
      variable_size TYPE dec20,
      fixed_size    TYPE dec20,
      ms_hl         TYPE dec20,
    END OF ty_realrs,

    BEGIN OF ty_realrsindex,
      table_name TYPE tabname,
      index_name TYPE ty_columns-column_name,
      index_size TYPE dec20, "rounding?
    END OF ty_realrsindex,

    BEGIN OF ty_index,
      sqltab     TYPE tabname,
      indexname  TYPE indexid,
      fieldname  TYPE fieldname,
      position   TYPE tabfdpos,
      uniqueflag TYPE uniqueflag,
    END OF ty_index,

    BEGIN OF ty_possindex,
      indexname TYPE indexid,
      position  TYPE tabfdpos,
    END OF ty_possindex,

    typ_t_realrsindex TYPE STANDARD TABLE OF ty_realrsindex,
    typ_t_realrs      TYPE STANDARD TABLE OF ty_realrs,
    typ_t_realcs      TYPE STANDARD TABLE OF ty_realcs,

    BEGIN OF ty_toptables, " top tables
      col1(22)  TYPE c,
      col2p     TYPE p LENGTH 9 DECIMALS 1,
      col2c(15) TYPE c,
      col3p     TYPE p LENGTH 9 DECIMALS 1,
      col3d     TYPE dec20,
      col3c(15) TYPE c,
      col4c(15) TYPE c,
      col5c(15) TYPE c,
    END OF ty_toptables,
    ty_t_toptables TYPE STANDARD TABLE OF ty_toptables,

    BEGIN OF ty_ssol,
      table_name   TYPE tabname,
      group(16)    TYPE c,
      size         TYPE p LENGTH 9 DECIMALS 1,
      ssize        TYPE p LENGTH 9 DECIMALS 1, "change after mig.
      osize        TYPE p LENGTH 9 DECIMALS 1, "change after cleanup
      est_pk_size  TYPE p LENGTH 9 DECIMALS 1,
      real_pk_size TYPE p LENGTH 9 DECIMALS 1,
      record_count TYPE dec20,
      spec_count   TYPE dec20,
      cs_cols      TYPE STANDARD TABLE OF ty_cs_size WITH DEFAULT KEY,
    END OF ty_ssol,

    "Start of types aligned with /SDF/READ_HDB_SIZING_RESULTS_X
    BEGIN OF ty_hdbsizing,
      sizing_guid       TYPE guid_16,
      crdate            TYPE sy-datum,
      report_name(40)   TYPE c,
      report_version(6) TYPE c,
      db_vendor(10)     TYPE c,
      db_release(20)    TYPE c,
      sizing_variant    TYPE xstring,
      nb_tables         TYPE int4,
      nb_errors         TYPE int4,
      error_list        TYPE xstring,
      anydb_size        TYPE dec20,
    END OF ty_hdbsizing,

    BEGIN OF ty_hdbtabsizes,
      sizing_guid     TYPE guid_16,
      table_name(30)  TYPE c,
      part_id         TYPE int4,
      tab_type(5)     TYPE c,
      store_type(2)   TYPE c,
      record_count    TYPE dec20,
      records_sampled TYPE dec20,
      data_size       TYPE dec20,
      opt_size        TYPE dec20,
      lob_size        TYPE dec20,
    END OF ty_hdbtabsizes,

    "also aligned with "and /SDF/READ_HDB_SIZING_RESULTS_2 now
    BEGIN OF ty_techd,
      crdate            TYPE sydats,
      report_name       TYPE progname,
      report_version(6) TYPE c,
      db_vendor         TYPE sydbsys,
      db_release        TYPE bdl_dbrel,
      unicode(3)        TYPE c,
      sid               LIKE sy-sysid,
      sapversion(30)    TYPE c,
      kernversion       TYPE thlines-thline,
      osversion         TYPE thlines-thline,
      success_cnt       TYPE i,
      partial_cnt       TYPE i,
      error_cnt         TYPE i,
      count_1           TYPE i,
      count_2           TYPE i,
      mss1691513        TYPE boolean,
      lcerror           TYPE boolean,
      prec(1)           TYPE c,
      daag(3)           TYPE n,
      paral(2)          TYPE n,
      hdbload           TYPE boolean,
      hl                TYPE boolean,
      mac               TYPE boolean,
      hdbv(3)           TYPE c,
      calib             TYPE boolean,
      do_soh            TYPE c,
      do_sfin           TYPE c,
      do_slog           TYPE c,
      in_sfin           TYPE c,
      in_slog           TYPE c,
      duration          TYPE i,
      hdb_empty_tables  TYPE boolean,
      anydbsize_n       TYPE p LENGTH 9,
      anydbsizedate     TYPE sydats,
      basis_753         TYPE c,
      clustprec         TYPE c,
    END OF ty_techd,

    BEGIN OF ty_subtotals,
      name  TYPE rollname,
      value TYPE dec20,
    END OF ty_subtotals,

    BEGIN OF ty_aging_runs,
      table_name   TYPE tabname,
      agobject(30) TYPE c,
      agobact      TYPE abap_bool,
      aglrun       TYPE dats,
      fspec        TYPE ty_columns-column_name,
    END OF ty_aging_runs,

    BEGIN OF ty_obj_arch_runs,
      object  TYPE objct_tr01,
      arclrun TYPE dats,
    END OF ty_obj_arch_runs,

    BEGIN OF ty_obj_arch_def,
      object TYPE objct_tr01,
    END OF ty_obj_arch_def,

    BEGIN OF ty_arch_runs,
      table_name TYPE tabname,
      runs       TYPE STANDARD TABLE OF ty_obj_arch_runs WITH DEFAULT KEY,
      objects    TYPE STANDARD TABLE OF ty_obj_arch_def WITH DEFAULT KEY,
    END OF ty_arch_runs,

    BEGIN OF ty_sogrp,
      group(30) TYPE c,
      size      TYPE p LENGTH 9 DECIMALS 1,
      opt_size  TYPE p LENGTH 9 DECIMALS 1,
    END OF ty_sogrp,

    BEGIN OF ty_aging,
      table_name   TYPE tabname,
      size         TYPE p LENGTH 9 DECIMALS 1,
      hist_size    TYPE p LENGTH 9 DECIMALS 1,
      record_count TYPE dec20,
    END OF ty_aging,

    BEGIN OF ty_cpu_max,
      datecnt          TYPE dats,
      percnt           TYPE c,
      max_hloadcnt     TYPE p LENGTH 12,
      max_factcnt      TYPE p LENGTH 4 DECIMALS 2,
      max_factcnt_date TYPE dats,
      est_sapscnt      TYPE i,
      est_sapsts(3)    TYPE c,
      bkgratio         TYPE p LENGTH 4 DECIMALS 2,
    END OF ty_cpu_max,

    BEGIN OF ty_cpu_per,
      per       TYPE c,
      firstdate TYPE dats,
      lastdate  TYPE dats,
    END OF ty_cpu_per,
    "End of types aligned with /SDF/READ_HDB_SIZING_*

    BEGIN OF ty_checkcol,
      col TYPE ty_cs_size-column_name,
      src TYPE tabname,
      tgt TYPE tabname,
    END OF ty_checkcol,
    ty_t_checkcol TYPE STANDARD TABLE OF ty_checkcol,

    BEGIN OF ty_upgrade,
      table_name   TYPE tabname,
      size         TYPE p LENGTH 9 DECIMALS 1,
      pksize       TYPE p LENGTH 9 DECIMALS 1,
      disksize     TYPE p LENGTH 9 DECIMALS 1,
      record_count TYPE p LENGTH 9 DECIMALS 1,
    END OF ty_upgrade,
    ty_t_upgrade TYPE STANDARD TABLE OF ty_upgrade,

    BEGIN OF ty_keyfields,
      column_name TYPE ty_cs_size-column_name,
      client      TYPE ty_cs_size-client,
      typekind    TYPE ty_cs_size-typekind,
    END OF ty_keyfields,
    ty_t_keyfields TYPE STANDARD TABLE OF ty_keyfields,

    "select option
    BEGIN OF ty_nso,
      sign(1)   TYPE c,
      option(2) TYPE c,
      low(30)   TYPE c,
      high(30)  TYPE c,
    END OF ty_nso,

    BEGIN OF ty_cpu_avg,
      date            TYPE dats,
      per             TYPE c,
      avg_hloadcnt    TYPE p LENGTH 12,
      sum_ploadcnt    TYPE p LENGTH 12,
      sum_ploadcntbkg TYPE p LENGTH 12,
      peak_hloadcnt   TYPE p LENGTH 12,
      hcount          TYPE i,
      peak_factcnt    TYPE p LENGTH 4 DECIMALS 2,
*      peak_factrec  TYPE p LENGTH 4 DECIMALS 2,
    END OF  ty_cpu_avg.

  DATA: " Statistics collection.
    data            TYPE xstring,
    gt_input        TYPE TABLE OF ty_hana_db_size, "all tables
    gt_tables       TYPE TABLE OF ty_hana_db_size, "all data
    table_item      TYPE ty_hana_db_size,
    gs_rs_size      TYPE ty_rs_size,
    gs_cs_size      TYPE ty_cs_size,
    gt_columns      TYPE TABLE OF ty_columns,
    gs_dd02l        TYPE dd02l,
    gt_cstab        TYPE TABLE OF tabname,
    gt_rstab        TYPE TABLE OF tabname,
    tt_t_multi      TYPE TABLE OF tabname,
    p_db_table      TYPE tabname,
    ls_tablename    TYPE tabname,
    gt_row_tab      TYPE TABLE OF tabname
                         WITH DEFAULT KEY, "all RS tables
    gt_rownots4_tab LIKE gt_row_tab,
    gt_ddfields     TYPE TABLE OF x031l,
    where_clause    TYPE string.

  DATA: "index sizing
    gt_index         TYPE TABLE OF ty_index,
    gs_nu_index      TYPE ty_index,
    gt_nu_index      TYPE TABLE OF ty_index,
    pr_indexname     TYPE ty_index-indexname,
    pr_sqltab        TYPE ty_index-sqltab,
    tabix            TYPE sy-tabix,
    gt_possindex     TYPE TABLE OF ty_possindex,
    gs_possindex     TYPE ty_possindex,
    fdpos            TYPE sy-fdpos,
    searchfield(256) TYPE c,
    lv_pos           TYPE tabfdpos,
    lv_maxpos        TYPE tabfdpos.

  DATA: "error handling
    gt_error    TYPE ty_t_error,
    gt_partial  TYPE ty_t_error,
    ls_error    TYPE ty_error,
    ls_partial  TYPE ty_error,
    max_dump    TYPE i,
    error_ratio TYPE p LENGTH 5 DECIMALS 4.

  DATA: " parallelism
    taskname    TYPE string,
    max_act     TYPE i,
    maxrfc      TYPE i,
    bucket      TYPE ty_hana_db_size-bucket,
    gt_bucket   TYPE TABLE OF ty_hana_db_size,
    lt_retry    TYPE TABLE OF ty_hana_db_size,
    bucket_size TYPE i,
    "nb_tables   TYPE i,
    nb_buckets  TYPE i,
    mode        TYPE string,
    l_dump      TYPE i,
    l_retrycnt  TYPE i,
    l_sent      TYPE i,
    l_act       TYPE i,
    l_recvd     TYPE i.

  DATA: "Sizing on HANA
    gt_realrsindex TYPE typ_t_realrsindex,
    gt_realrs      TYPE typ_t_realrs,
    gt_realcs      TYPE typ_t_realcs,
    index          TYPE sy-fdpos,
    dollar_count   TYPE i.

  DATA: "CPU Sizing
    l_cpu_avg  TYPE TABLE OF ty_cpu_avg,
    wa_cpu_x   TYPE ty_cpu_max,
    gt_cpu_per TYPE TABLE OF ty_cpu_per,
    cpu_err    TYPE abap_bool.

  DATA: "data model changes
    gt_sfin2   TYPE TABLE OF ty_ssol,
    gt_slog    TYPE TABLE OF ty_ssol,
    gt_ml      TYPE TABLE OF ty_ssol,
    "other sizing relevant
    gt_aging   TYPE TABLE OF ty_aging,
    gt_sogrp   TYPE TABLE OF ty_sogrp,
    gt_inplace TYPE ty_t_upgrade,
    gt_shadow  TYPE ty_t_upgrade.

  DATA: "DVM
    gt_aging_runs TYPE TABLE OF ty_aging_runs,
    gt_arch_runs  TYPE TABLE OF ty_arch_runs.

  DATA: "technical data
    jobname        TYPE tbtcm-jobname,
    jobcount       TYPE tbtcm-jobcount,
    wa_nso         TYPE ty_nso,
    gt_nso         TYPE TABLE OF ty_nso,
    wa_techd       TYPE ty_techd,
    wa_sizing      TYPE ty_hdbsizing,
    gt_hdbtabsizes TYPE TABLE OF ty_hdbtabsizes,
    gt_subtotals   TYPE TABLE OF ty_subtotals,
    wa_subtotals   TYPE ty_subtotals,
    is_cloud       TYPE abap_bool.

  DATA: " Subtotals
    cs_col          TYPE p LENGTH 9 DECIMALS 1,
    est_cs_col      TYPE p LENGTH 9 DECIMALS 1,
    cs_mlob         TYPE p LENGTH 9 DECIMALS 1,
    est_cs_mlob     TYPE p LENGTH 9 DECIMALS 1,
    cs_k            TYPE p LENGTH 9 DECIMALS 1,
    est_cs_k        TYPE p LENGTH 9 DECIMALS 1,
    cs_pk           TYPE p LENGTH 9 DECIMALS 1,
    est_cs_pk       TYPE p LENGTH 9 DECIMALS 1,
    est_cs_hcard    TYPE p LENGTH 9 DECIMALS 1,
    cs_hcard        TYPE p LENGTH 9 DECIMALS 1,
    est_cs_mcard    TYPE p LENGTH 9 DECIMALS 1,
    cs_mcard        TYPE p LENGTH 9 DECIMALS 1,
    est_cs_lcard    TYPE p LENGTH 9 DECIMALS 1,
    cs_lcard        TYPE p LENGTH 9 DECIMALS 1,
    est_cs_ucard    TYPE p LENGTH 9 DECIMALS 1,
    cs_ucard        TYPE p LENGTH 9 DECIMALS 1,
    cs_rid          TYPE p LENGTH 9 DECIMALS 1,
    est_cs_rid      TYPE p LENGTH 9 DECIMALS 1,
    cs_udiv         TYPE p LENGTH 9 DECIMALS 1,
    est_cs_udiv     TYPE p LENGTH 9 DECIMALS 1,
    cs_uk           TYPE p LENGTH 9 DECIMALS 1,
    est_cs_uk       TYPE p LENGTH 9 DECIMALS 1,
    cs_nuk          TYPE p LENGTH 9 DECIMALS 1,
    est_cs_nuk      TYPE p LENGTH 9 DECIMALS 1,             "always 0.
    cs_aging        TYPE p LENGTH 9 DECIMALS 1,
    est_cs_aging    TYPE p LENGTH 9 DECIMALS 1,             "always 0.
    rs_col          TYPE p LENGTH 9 DECIMALS 1,
    est_rs_col      TYPE p LENGTH 9 DECIMALS 1,
    rs_mlob         TYPE p LENGTH 9 DECIMALS 1,
    est_rs_mlob     TYPE p LENGTH 9 DECIMALS 1,
    rs_pk           TYPE p LENGTH 9 DECIMALS 1,
    est_rs_pk       TYPE p LENGTH 9 DECIMALS 1,
    rs_sk           TYPE p LENGTH 9 DECIMALS 1,
    est_rs_sk       TYPE p LENGTH 9 DECIMALS 1,
    rs_total        TYPE p LENGTH 9 DECIMALS 1,
    est_rs_total    TYPE p LENGTH 9 DECIMALS 1,
    cs_total        TYPE p LENGTH 9 DECIMALS 1,
    est_cs_total    TYPE p LENGTH 9 DECIMALS 1,
    init_data       TYPE p LENGTH 9 DECIMALS 1,
    est_init_data   TYPE p LENGTH 9 DECIMALS 1,
    init_ws         TYPE p LENGTH 9 DECIMALS 1,
    est_init_ws     TYPE p LENGTH 9 DECIMALS 1,
    opt_ws          TYPE p LENGTH 9 DECIMALS 1,
    est_opt_ws      TYPE p LENGTH 9 DECIMALS 1,
    cshl            TYPE p LENGTH 9 DECIMALS 1,
    est_cshl        TYPE p LENGTH 9 DECIMALS 1,
    cs_jec          TYPE p LENGTH 9 DECIMALS 1,
    est_cs_jec      TYPE p LENGTH 9 DECIMALS 1,             "always 0
    rshl            TYPE p LENGTH 9 DECIMALS 1,
    est_rshl        TYPE p LENGTH 9 DECIMALS 1,
    hl              TYPE p LENGTH 9 DECIMALS 1,
    est_hl          TYPE p LENGTH 9 DECIMALS 1,
    hlch            TYPE p LENGTH 9 DECIMALS 1,
    est_hlch        TYPE p LENGTH 9 DECIMALS 1,
    est_sfin_inv    TYPE p LENGTH 9 DECIMALS 1,
    est_initdisk    TYPE p LENGTH 9 DECIMALS 1,
    initdisk        TYPE p LENGTH 9 DECIMALS 1,
    est_twocs       TYPE p LENGTH 9 DECIMALS 1,
    twocs           TYPE p LENGTH 9 DECIMALS 1,
    opt_twocs       TYPE p LENGTH 9 DECIMALS 1,
    init_size       TYPE p LENGTH 9 DECIMALS 1,
    est_init_size   TYPE p LENGTH 9 DECIMALS 1,
    est_lc          TYPE p LENGTH 9 DECIMALS 1,
    cs_ft           TYPE p LENGTH 9 DECIMALS 1,
    est_cs_ft       TYPE p LENGTH 9 DECIMALS 1,
    est_opt_data    TYPE p LENGTH 9 DECIMALS 1,
    opt_data        TYPE p LENGTH 9 DECIMALS 1,
    est_opt_size    TYPE p LENGTH 9 DECIMALS 1,
    opt_size        TYPE p LENGTH 9 DECIMALS 1,
    est_optdisk     TYPE p LENGTH 9 DECIMALS 1,
    optdisk         TYPE p LENGTH 9 DECIMALS 1,
    est_rs2cs       TYPE p LENGTH 9 DECIMALS 1,
    "arh. and aging
    est_aged        TYPE p LENGTH 9 DECIMALS 1,
    est_agedch      TYPE p LENGTH 9 DECIMALS 1,
    real_elaging    TYPE p LENGTH 9 DECIMALS 1,
    est_elaging     TYPE p LENGTH 9 DECIMALS 1,
    real_elarch     TYPE p LENGTH 9 DECIMALS 1,
    est_ageddisk    TYPE p LENGTH 9 DECIMALS 1,
    "sfin
    est_elarch      TYPE p LENGTH 9 DECIMALS 1,
    est_obs_fin     TYPE p LENGTH 9 DECIMALS 1,
    est_obs_log     TYPE p LENGTH 9 DECIMALS 1,
    est_fin_unl     TYPE p LENGTH 9 DECIMALS 1,
    est_sfincold    TYPE p LENGTH 9 DECIMALS 1,
    est_sfincoldch  TYPE p LENGTH 9 DECIMALS 1,
    est_sfintab     TYPE p LENGTH 9 DECIMALS 1,
    est_sfincol     TYPE p LENGTH 9 DECIMALS 1,
    est_sfinnew     TYPE p LENGTH 9 DECIMALS 1,
    est_slogtab     TYPE p LENGTH 9 DECIMALS 1,
    est_slognew     TYPE p LENGTH 9 DECIMALS 1,
    est_indx        TYPE p LENGTH 9 DECIMALS 1,
    est_pca         TYPE p LENGTH 9 DECIMALS 1,
    est_sl          TYPE p LENGTH 9 DECIMALS 1,
    est_delcoep     TYPE p LENGTH 9 DECIMALS 1,
    "slog
    est_s4_trans    TYPE p LENGTH 9 DECIMALS 1,
    est_log_unl     TYPE p LENGTH 9 DECIMALS 1,
    est_log_les     TYPE p LENGTH 9 DECIMALS 1,
    est_slogcol     TYPE p LENGTH 9 DECIMALS 1,
    est_log_oldvbfa TYPE p LENGTH 9 DECIMALS 1,
    est_log_newvbfa TYPE p LENGTH 9 DECIMALS 1,
    est_log_chgvbfa TYPE p LENGTH 9 DECIMALS 1,
    "ml
    est_mlnew       TYPE p LENGTH 9 DECIMALS 1,
    est_obs_ml      TYPE p LENGTH 9 DECIMALS 1,
    "persistent memory
    init_pers       TYPE p LENGTH 9 DECIMALS 1,
    init_dram       TYPE p LENGTH 9 DECIMALS 1,
    est_init_pers   TYPE p LENGTH 9 DECIMALS 1,
    est_init_dram   TYPE p LENGTH 9 DECIMALS 1,
    opt_pers        TYPE p LENGTH 9 DECIMALS 1,
    opt_dram        TYPE p LENGTH 9 DECIMALS 1,
    est_opt_pers    TYPE p LENGTH 9 DECIMALS 1,
    est_opt_dram    TYPE p LENGTH 9 DECIMALS 1,
    "inplace
    est_updata      TYPE p LENGTH 9 DECIMALS 1,
    est_upchrec     TYPE p LENGTH 9 DECIMALS 1,
    est_updisk      TYPE p LENGTH 9 DECIMALS 1,
    est_uptotal     TYPE p LENGTH 9 DECIMALS 1,
    est_shadisk     TYPE p LENGTH 9 DECIMALS 1,
    est_sha         TYPE p LENGTH 9 DECIMALS 1,
    est_shatotal    TYPE p LENGTH 9 DECIMALS 1,
    est_cl_lis      TYPE p LENGTH 9 DECIMALS 1,
    "anydb
    "anydbsize_n     TYPE p LENGTH 9,
    udiv_default    TYPE p,
    diskadmin       TYPE p DECIMALS 1 VALUE 26843545600,
*    statistics               ~ 20 GB
*    metadata                 ~ 5 GB
    codestack       TYPE p DECIMALS 1 VALUE 53687091200,
*    shared cursor cache      - 8 GB
*    code + stack             ~ 12 GB
*    statistics               ~ 10-20 GB
*    other non-indexserver services ~ 8 GB
*    empty tables             ~ 8 GB    .

    "calibration sub totals
    lt_cs_cal       TYPE TABLE OF ty_cs_size,
    ls_cs_cal       TYPE ty_cs_size.

  "Report runtime
  DATA:
    startstamp TYPE timestamp,
    endstamp   TYPE timestamp.

  CONSTANTS: " Sizing constants
    max_partition_rc     TYPE i VALUE 500000000, "Theorical is 2billion
    rowid_index_comp     TYPE p DECIMALS 2 VALUE '1.5',
    idx_dict_comp        TYPE p DECIMALS 2 VALUE '4.2',
*    inv_index            TYPE p DECIMALS 2 VALUE '0.40',
    size_in_misc_default TYPE p VALUE 1200, "bytes
    pk_min_size          TYPE p VALUE 256,  "bytes
    cs_comp              TYPE p LENGTH 3 DECIMALS 2 VALUE '14',
    cs_n_comp            TYPE p LENGTH 3 DECIMALS 2 VALUE '5',
    cs_p_comp            TYPE p LENGTH 3 DECIMALS 2 VALUE '6.6',
    rs_comp              TYPE p LENGTH 3 DECIMALS 2 VALUE '1.13',
    disk_comp_lost       TYPE p LENGTH 3 DECIMALS 2 VALUE '1.2',
    rs_pk_comp           TYPE p DECIMALS 2 VALUE '2.4',
    rs_sk_comp           TYPE p DECIMALS 2 VALUE '1.4',
    lob_dict_comp        TYPE p DECIMALS 2 VALUE '1.4',
    c_hlcache            TYPE p DECIMALS 2 VALUE '0.2',
    c_chg_cpu_factor     TYPE i VALUE 3,
    c_sapscnt_factor     TYPE i VALUE 2000,
    c_hrcjobname         TYPE tbtcm-jobname VALUE '/SDF/HDB_SIZING_SM',
    c_max_err            TYPE p LENGTH 5 DECIMALS 2 VALUE '0.05',
    cos_util_class       TYPE syrepid VALUE 'CL_COS_UTILITIES'.

  DATA: " output
    bigoutln(121) TYPE c,
    realsize_cnt  TYPE i,
    gs_top        TYPE ty_toptables,
    gt_top        TYPE ty_t_toptables,
    count         TYPE i,
    lv_status     TYPE c.

  FIELD-SYMBOLS:
    <item>         TYPE ty_hana_db_size,
    <sfin>         TYPE ty_ssol,
    <slog>         TYPE ty_ssol,
    <ml>           TYPE ty_ssol,
    <aging>        TYPE ty_aging,
    <aging_runs>   TYPE ty_aging_runs,
    <arch_runs>    TYPE ty_arch_runs,
    <sogrp>        TYPE ty_sogrp,
    <objarchruns>  TYPE ty_obj_arch_runs,
    <objarchdef>   TYPE ty_obj_arch_def,
    <cs_size>      TYPE ty_cs_size,
    <rs_size>      TYPE ty_rs_size,
    <columns>      TYPE ty_columns,
    <index>        TYPE ty_index,
    <idx>          TYPE ty_index,
    <nu_idx>       TYPE ty_index,
    <po_idx>       TYPE ty_possindex,
    <real_cs>      TYPE ty_realcs,
    <real_rs>      TYPE ty_realrs,
    <real_rsindex> TYPE ty_realrsindex,
    <upgrade>      TYPE ty_upgrade,
    <cscal>        TYPE ty_cs_size,
    <hstat>        TYPE ty_cpu_avg.

  " Macros
  DEFINE b2gb.
    &1 = &1 / 1024 / 1024 / 1024.
  END-OF-DEFINITION.

  DEFINE k2gb.
    &1 = &1 / 1024 / 1024.
  END-OF-DEFINITION.

  DEFINE mb2gb.
    &1 = &1 / 1024.
  END-OF-DEFINITION.

  DEFINE mc_topheader.
    IF gt_top[] IS NOT INITIAL.
      WRITE /.
      PERFORM add_line.
      WRITE:
        / '|',
        AT 3  &1,
        AT 32 &3 CENTERED,
        AT 63 &5 CENTERED,
        AT 80 '|'.
      WRITE:
        / '|',
        AT 4  &2,
        AT 31 &4 CENTERED,
        AT 62 &6 CENTERED,
        AT 80 '|'.
      PERFORM add_line.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_sp. "2 columns inside a table
    IF &2 IS NOT INITIAL.
      WRITE: / '|', AT 3 &1, AT 62(17) &2 RIGHT-JUSTIFIED,  "#EC NOTEXT
        AT 80 '|'.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_spn. "2 columns inside a table for numeric
    IF &2 IS NOT INITIAL.
      WRITE: / '|', AT 3 &1, AT 63(17) &2 RIGHT-JUSTIFIED,  "#EC NOTEXT
        AT 80 '|'.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_sp1. "2 columns inside a table.
    IF &2 IS NOT INITIAL.
      MOVE &2 TO outln. CONDENSE outln.
      CONCATENATE &1 '(Total:' outln INTO outln SEPARATED BY space.
      CONCATENATE outln 'GB)' INTO outln.
      WRITE: / '|', AT 3 outln , AT 80 '|'.                 "#EC NOTEXT
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_spi. "2 columns inside a table indented
    IF &2 IS NOT INITIAL.
      WRITE: / '|', AT 7 &1, AT 62(17) &2 RIGHT-JUSTIFIED,  "#EC NOTEXT
        AT 80 '|'.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_s. "2 columns, no table
    IF &2 IS NOT INITIAL.
      WRITE: / '', AT 2 &1, AT 62(17) &2 RIGHT-JUSTIFIED.   "#EC NOTEXT
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_sn. "2 columns, no table with numeric
    IF &2 IS NOT INITIAL.
      WRITE: / '', AT 2 &1, AT 63(17) &2 RIGHT-JUSTIFIED.   "#EC NOTEXT
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mc_sp_t."1 column inside a table
    WRITE: / '|', AT 3(77) &1, AT 80 '|'.                   "#EC NOTEXT
  END-OF-DEFINITION.

  DEFINE mc_t2. "2 columns inside a table. Bigger 2nd table
    WRITE: / '|', AT 3 &1, AT 55(24) &2 RIGHT-JUSTIFIED, AT 80 '|'.
                                                            "#EC NOTEXT
  END-OF-DEFINITION.

  DEFINE mc_dollar_count. "FIND ALL OCCURRENCES OF does not work on 620
    index = 1.
    CLEAR dollar_count .
    DO.
      SEARCH &1 FOR '$' STARTING AT index.
      IF sy-subrc = 0.
        dollar_count = dollar_count + 1.
        index = index + sy-fdpos + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CLEAR index.
  END-OF-DEFINITION.

  DEFINE mc_per. " Cpu sizing details
    READ TABLE gt_cpu_per WITH KEY per = &1 ASSIGNING <cpu_per>.
    IF sy-subrc = 0.
      WRITE: / '|',
            AT 3 &2,
            AT 56(10) <cpu_per>-firstdate ,                 "#EC NOTEXT
            AT 67(1)  '-',
            AT 69(10) <cpu_per>-lastdate,                   "#EC NOTEXT
            AT 80 '|'.
    ENDIF.
  END-OF-DEFINITION.

  GET TIME STAMP FIELD startstamp.

  "It is not allowed to run the sizing report in Cloud system
  TRY.
      CALL METHOD (cos_util_class)=>is_cloud
        EXPORTING
          rv_is_cloud = is_cloud.
      IF is_cloud = abap_true.
        EXIT.
      ENDIF.
    CATCH cx_root.                                      "#EC NO_HANDLER
  ENDTRY.

  IF p_guid IS NOT INITIAL.
    p_guid_c = p_guid.
  ENDIF.
  IF p_guid_C IS NOT INITIAL.
    p_guid = p_guid_c.
  ENDIF.

  IF p_disp = abap_true OR p_ocpu = abap_true..
    PERFORM read_selected_sizing USING p_guid.
    IF p_ocpu = abap_true.
      PERFORM sizing_calculation_cpu USING cpu_err.
    ENDIF.
    PERFORM b2gb_subtotals.
    PERFORM call_output USING 'OUTPUT_ON_SCREEN'.
    MESSAGE s090(sada) WITH 'Sizing completed.'.            "#EC NOTEXT
    EXIT.
  ENDIF.

  IF p_soh = abap_true.
    do_soh = abap_true. do_sfin = abap_false.
    do_slog = abap_false. do_sml = abap_false.
  ELSEIF p_sfin = abap_true.
    do_soh = abap_false. do_sfin = abap_true.
    do_slog = abap_false. do_sml = abap_false.
  ELSEIF p_slog = abap_true.
    do_soh = abap_false.
    IF in_sfin = abap_true.
      do_sfin = abap_false.
    ELSE.
      do_sfin = abap_true.
    ENDIF.
    do_slog = abap_true.
    IF in_sml = abap_true.
      do_sml = abap_false.
    ELSE.
      do_sml = abap_true.
    ENDIF.
  ENDIF.

  IF p_hdb2 = abap_true.
    p_mshl = abap_true.
    udiv_default = 43696. "bytes
  ELSE.
    udiv_default = 272648. "bytes
  ENDIF.

  IF p_bas1 = abap_true.
    p_lraw = 5000. "bytes
  ELSE.
    p_lraw = 1000. "bytes
  ENDIF.

  IF p_paral < 1.
    MESSAGE e090(sada) WITH
        'Unacceptable value for parallel degree:' p_paral   "#EC NOTEXT
        'Please enter value bigger than 0'.                 "#EC NOTEXT
  ENDIF.

  IF prec_low = abap_true.
    wa_techd-prec = 'L'.
  ELSEIF prec_hi = abap_true.
    wa_techd-prec = 'H'.
  ELSE.
    wa_techd-prec = 'M'.
  ENDIF.
  bucket_size = p_buk.
  max_dump    = p_dump.

  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name                     = p_group
    IMPORTING
      free_pbt_wps                   = maxrfc
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.

  CASE sy-subrc.
    WHEN 0.
      IF p_paral >= maxrfc.
        p_paral = maxrfc - 2.
        IF p_paral < 1. p_paral = 1. ENDIF.
      ENDIF.
    WHEN 1.
      MESSAGE e090(sada) WITH 'Server group'                "#EC NOTEXT
                              p_group
                              'is invalid'.                 "#EC NOTEXT
    WHEN 4.
      MESSAGE e090(sada) WITH
                            'There is currently no'         "#EC NOTEXT
                            'available resources on'        "#EC NOTEXT
                            'server group'                  "#EC NOTEXT
                            p_group.
    WHEN OTHERS.
      MESSAGE e090(sada) WITH
                      'An error occurred with server group' "#EC NOTEXT
                       p_group.
  ENDCASE.

  " Check authority
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD' ID 'S_ADMI_FCD' FIELD 'PADM'.
  IF sy-subrc <> 0.
    MESSAGE e090(sada) WITH
                     'You do not have the authorization'    "#EC NOTEXT
                     'to run this report. Refer to the FAQ' "#EC NOTEXT
                     'of Note 1872170.'.                    "#EC NOTEXT
  ENDIF.

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      jobname         = jobname
      jobcount        = jobcount
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.

  "Check installation (Z report)
  CASE sy-repid.
    WHEN 'ZNEWHDB_SIZE'.
      funcname = 'Z_COLLECT_STATS'.
      PERFORM check_install.
    WHEN '/SDF/HDB_SIZING'.
      funcname = '/SDF/COLLECT_DB_STATS'.
    WHEN OTHERS.
      funcname = 'Z_COLLECT_STATS'.
  ENDCASE.

  "Start work

  IF sy-dbsys(3) <> 'HDB'.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = 'DD02L'
      TABLES
        x031l_tab = gt_ddfields.
    READ TABLE gt_ddfields WITH KEY fieldname = 'IS_GTT'
                        TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      where_clause = 'IS_GTT = SPACE'.                      "#EC NOTEXT
    ENDIF.
  ENDIF.

  SELECT * FROM dd02l INTO gs_dd02l
    WHERE as4local = 'A'
      AND tabname IN so_tab
      AND tabclass IN ('TRANSP', 'CLUSTER' , 'POOL')
      AND (where_clause).
    table_item-table_name = gs_dd02l-tabname.
    table_item-tab_type = gs_dd02l-tabclass.
    table_item-db_table = gs_dd02l-sqltab.
    IF table_item-db_table IS INITIAL.
      table_item-db_table = table_item-table_name.
    ENDIF.
    APPEND table_item TO gt_input.
  ENDSELECT.
  CLEAR table_item.
  SORT gt_input BY table_name.

*  " check if the input tables are existing.
*  IF so_tab IS NOT INITIAL.
*    LOOP AT so_tab WHERE option = 'EQ' AND sign = 'I'.
*      READ TABLE gt_input WITH KEY table_name = so_tab-low
*                     TRANSPORTING NO FIELDS BINARY SEARCH .
*      IF sy-subrc <> 0.
*        WRITE: 'The following table was not found:',        "#EC NOTEXT
*                so_tab-low, /.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  IF gt_input IS INITIAL.
    RETURN. "no valid tables in selection.
  ENDIF.

  SORT gt_input BY db_table.

  " identify multiplex tables and adapt their db name.
  IF gt_input IS NOT INITIAL.
    SELECT tabname FROM dd02l INTO TABLE tt_t_multi
      FOR ALL ENTRIES IN gt_input
      WHERE as4local = 'A'
       AND tabclass = 'VIEW'
    AND tabname = gt_input-db_table.

    LOOP AT tt_t_multi INTO ls_tablename.
      READ TABLE gt_input ASSIGNING <item>
            WITH KEY db_table = ls_tablename BINARY SEARCH.
      IF sy-subrc = 0.
        <item>-db_table = <item>-table_name. "keep sort.
      ENDIF.
    ENDLOOP.
    CLEAR: tt_t_multi[], ls_tablename.
  ENDIF.

  IF p_calib = abap_true.
    mode = 'CAL'.
  ELSE.
    mode = 'NOR'.
  ENDIF.

  MESSAGE s090(sada) WITH 'Start of statistics collection'. "#EC NOTEXT
  PERFORM collect_stats USING mode.

  MESSAGE s090(sada) WITH 'Start of sizing calculation'.    "#EC NOTEXT
*
* All results now in gt_tables and gt_columns / gt_real*
*
  IF sy-dbsys(3) <> 'HDB' OR ( sy-dbsys(3) = 'HDB'
                           AND p_calib = abap_true ).

    SORT gt_columns BY table_name column_name.

* Now distinguish row store and column store tables,
* eliminate tables with errors:

    " User-defined distribution.
    IF so_cstab IS NOT INITIAL.
      SELECT tabname FROM dd02l INTO TABLE gt_cstab
        WHERE as4local = 'A'
          AND tabname IN so_cstab
          AND tabclass IN ('TRANSP', 'CLUSTER' , 'POOL')
      ORDER BY tabname.
    ENDIF.
    IF so_rstab IS NOT INITIAL.
      SELECT tabname FROM dd02l INTO TABLE gt_rstab
        WHERE as4local = 'A'
          AND tabname IN so_rstab
          AND tabclass IN ('TRANSP', 'CLUSTER' , 'POOL')
      ORDER BY tabname.
    ENDIF.
    " Does the dictionary contain the store information?
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = 'DD09L'
      TABLES
        x031l_tab = gt_ddfields.
    READ TABLE gt_ddfields WITH KEY fieldname = 'ROWORCOLST'
                        TRANSPORTING NO FIELDS.
    IF sy-subrc = 0 AND gt_tables IS NOT INITIAL.
      where_clause = 'roworcolst = ''R'''.                  "#EC NOTEXT
      SELECT tabname FROM dd09l INTO TABLE gt_row_tab
         FOR ALL ENTRIES IN gt_tables
         WHERE tabname = gt_tables-table_name
         AND as4local = 'A'
         AND (where_clause).
    ELSE.
      " If entry does not exist, create the list.
      PERFORM fill_row_list.
    ENDIF.

    LOOP AT gt_tables ASSIGNING <item>.
      IF    <item>-return_code = 0   "success
        AND <item>-record_count <> -1
        OR ( <item>-return_code = 98
         AND <item>-records_sampled >= 10000 ). "partial success
        READ TABLE gt_row_tab WITH KEY <item>-table_name
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 AND <item>-record_count < 500000000.
          "this is an RS table in ddic
          READ TABLE gt_cstab WITH KEY <item>-table_name
           TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. "but user forced CS for this table.
            <item>-store = 'CS'.
          ELSE.
            <item>-store = 'RS'.
          ENDIF.
        ELSE. "this is a CS table in ddic or an RS > 0,5 billion records
          READ TABLE gt_rstab WITH KEY <item>-table_name
           TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. "but user forced RS for this table.
            <item>-store = 'RS'.
          ELSE.
            <item>-store = 'CS'.
          ENDIF.
        ENDIF.
        IF <item>-return_code = 98. "partial success
          MOVE-CORRESPONDING <item> TO ls_partial.
          APPEND ls_partial TO gt_partial.
        ENDIF.
      ELSE.
        CLEAR ls_error.
        MOVE-CORRESPONDING <item> TO ls_error.
        IF <item>-record_count = -1 AND <item>-return_code = 0.
          ls_error-return_code = -1."should not happen
        ENDIF.
        "Error 98 with less than 10000 sampled records is logged as
        "error
        APPEND ls_error TO gt_error.
      ENDIF.
    ENDLOOP.

* fill embedded tables.
    LOOP AT gt_columns ASSIGNING <columns>.
      READ TABLE gt_tables ASSIGNING <item>
                            WITH KEY table_name = <columns>-table_name
                            BINARY SEARCH.
      IF <item>-store = 'CS'.
        MOVE-CORRESPONDING <columns> TO gs_cs_size.
        APPEND gs_cs_size TO <item>-cs_cols.
      ELSE.
        MOVE-CORRESPONDING <columns> TO gs_rs_size.
        APPEND gs_rs_size TO <item>-rs_cols.
      ENDIF.
      CLEAR: gs_cs_size, gs_rs_size.
    ENDLOOP.

    REFRESH gt_columns.
    PERFORM sizing_calculation CHANGING gt_tables.

*start of calibration specific code.
    SORT gt_realcs      BY table_name column_name part_id.
    SORT gt_realrsindex BY table_name index_name.
    SORT gt_realrs      BY table_name.

    LOOP AT gt_realcs ASSIGNING <real_cs>.
      READ TABLE gt_tables ASSIGNING <item>
           WITH KEY table_name = <real_cs>-table_name BINARY SEARCH.
      IF sy-subrc = 0.
        IF <item>-store = 'CS'.
          READ TABLE <item>-cs_cols ASSIGNING <cs_size>
                          WITH KEY column_name = <real_cs>-column_name.
          IF sy-subrc = 0.
            <cs_size>-part_id  = <real_cs>-part_id.
            <cs_size>-ms_total = <cs_size>-ms_total
                               + <real_cs>-ms_total.
            <cs_size>-ms_main  = <cs_size>-ms_main  + <real_cs>-ms_main.
            <cs_size>-distinct_count = <cs_size>-distinct_count
                                         + <real_cs>-distinct_count.
            " wrong if partitioned !
            <cs_size>-loaded           = <real_cs>-loaded.
            <cs_size>-index_loaded     = <real_cs>-index_loaded.
            <cs_size>-compression_type = <real_cs>-compression_type.
            <cs_size>-ms_dict = <cs_size>-ms_dict + <real_cs>-ms_dict.
            <cs_size>-ms_index = <cs_size>-ms_index
                              + <real_cs>-ms_index.
            <cs_size>-ms_data  = <cs_size>-ms_data + <real_cs>-ms_data.
            <cs_size>-ms_misc  = <cs_size>-ms_misc + <real_cs>-ms_misc.
            <cs_size>-ms_hl    = <cs_size>-ms_hl + <real_cs>-ms_hl.
          ELSE."non-unique from JE. Aging columns. ESH. FT.
            IF <item>-record_count > 0.
              mc_dollar_count <real_cs>-column_name.
              IF dollar_count > 2
                    OR <real_cs>-column_name(1) = '_'
                    OR <real_cs>-column_name(13) = '$_SYS_SHADOW_'
                    OR <real_cs>-column_name(5) = '$esh:'.
                CLEAR gs_cs_size.
                MOVE-CORRESPONDING <real_cs> TO gs_cs_size.
                APPEND gs_cs_size TO <item>-cs_cols.
              ELSE.
                WRITE: / 'Could not find column',           "#EC NOTEXT
                          <real_cs>-table_name, <real_cs>-column_name.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          WRITE: / 'Table in RS, expected in CS',           "#EC NOTEXT
          <real_cs>-table_name, <real_cs>-column_name.
        ENDIF.
      ELSE.
        WRITE: / 'Table not found in gt_tables',            "#EC NOTEXT
        <real_cs>-table_name, <real_cs>-column_name.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_realrs ASSIGNING <real_rs>.
      READ TABLE gt_tables ASSIGNING <item>
                          WITH KEY table_name = <real_rs>-table_name
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        IF <item>-store = 'RS'.
          CLEAR gs_rs_size.
          MOVE-CORRESPONDING <real_rs> TO gs_rs_size.
          APPEND gs_rs_size TO <item>-rs_cols.
        ELSE.
          WRITE: / 'Table in CS, expected in RS',           "#EC NOTEXT
          <real_rs>-table_name.
        ENDIF.
      ELSE.
        WRITE: / 'Table not found in gt_tables',            "#EC NOTEXT
        <real_rs>-table_name.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_realrsindex ASSIGNING <real_rsindex>.
      READ TABLE gt_tables ASSIGNING <item>
        WITH KEY table_name = <real_rsindex>-table_name BINARY SEARCH.
      IF sy-subrc = 0.
        IF <item>-store = 'RS'.
          IF <real_rsindex>-index_name(13) = '_SYS_TREE_RS_'.
            READ TABLE <item>-rs_cols ASSIGNING <rs_size>
                 WITH KEY column_name = <real_rsindex>-index_name(13).
            IF sy-subrc = 0.
              <rs_size>-fixed_size = <real_rsindex>-index_size.
            ELSE.
              WRITE: / <real_rsindex>-table_name,
                   'no RS PK sized. Table is maybe CS!?'.   "#EC NOTEXT
            ENDIF.
          ELSEIF <real_rsindex>-index_name CS '~0'
              OR <real_rsindex>-index_name CS '^0'.
            READ TABLE <item>-rs_cols ASSIGNING <rs_size>
               WITH KEY column_name = '_SYS_TREE_RS_'.
            IF sy-subrc = 0.
              <rs_size>-fixed_size = <real_rsindex>-index_size.
            ENDIF.
          ELSEIF <real_rsindex>-index_name CS '~'.
            READ TABLE <item>-rs_cols ASSIGNING <rs_size>
               WITH KEY column_name = <real_rsindex>-index_name.
            IF sy-subrc = 0.
              <rs_size>-fixed_size = <real_rsindex>-index_size.
            ENDIF.
          ELSE. "something else found  "should not happen
            WRITE: / <real_rsindex>-table_name,
               <real_rsindex>-index_name,
               'found but not sized'.                       "#EC NOTEXT
          ENDIF.
        ELSE.
          WRITE: / 'Table in CS, expected in RS',           "#EC NOTEXT
          <item>-table_name.
        ENDIF.
      ELSE.
        WRITE: / 'Table not found in gt_tables',            "#EC NOTEXT
        <item>-table_name.
      ENDIF.
    ENDLOOP.
*end of calibration specific code.
  ELSE. "simple run on HANA
    LOOP AT gt_realcs ASSIGNING <real_cs>.
      CLEAR gs_cs_size.
      MOVE-CORRESPONDING <real_cs> TO gs_cs_size.
      READ TABLE gt_tables ASSIGNING <item>
           WITH KEY table_name = <real_cs>-table_name BINARY SEARCH.
      IF sy-subrc = 0.
        <item>-store   = 'CS'.
        <item>-load_unit = <real_cs>-load_unit.
        APPEND gs_cs_size TO <item>-cs_cols.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_realrs ASSIGNING <real_rs>.
      CLEAR gs_rs_size.
      MOVE-CORRESPONDING <real_rs> TO gs_rs_size.
      READ TABLE gt_tables ASSIGNING <item>
           WITH KEY table_name = <real_rs>-table_name BINARY SEARCH.
      IF sy-subrc = 0.
        <item>-store = 'RS'.
        APPEND gs_rs_size TO <item>-rs_cols.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_realrsindex ASSIGNING <real_rsindex>.
      CLEAR gs_rs_size.
      gs_rs_size-column_name   = <real_rsindex>-index_name.
      gs_rs_size-fixed_size    = <real_rsindex>-index_size.
      READ TABLE gt_tables ASSIGNING <item>
                      WITH KEY table_name = <real_rsindex>-table_name
                      BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND gs_rs_size TO <item>-rs_cols.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE gt_tables LINES realsize_cnt.

    "idendify DDIC secondary keys.
    PERFORM fill_index_list.
    SORT gt_index BY sqltab fieldname position.

    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      LOOP AT <item>-cs_cols ASSIGNING <cs_size>
                            WHERE column_name(1) = '$'.
        mc_dollar_count <cs_size>-column_name.
        IF dollar_count > 2.
          searchfield = <cs_size>-column_name.
          DO.
            SEARCH searchfield+1 FOR '$'.
            IF sy-subrc = 0.
              IF sy-fdpos = 0. sy-fdpos = 1. ENDIF. "double $$
              gs_nu_index-fieldname = searchfield+1(sy-fdpos).
              gs_nu_index-sqltab    = <item>-table_name.
              gs_nu_index-position  = gs_nu_index-position + 1.
              APPEND gs_nu_index TO gt_nu_index.
              fdpos = sy-fdpos + 1.
              searchfield = searchfield+fdpos.
            ELSE.
              CLEAR: gs_nu_index, searchfield.
              EXIT.
            ENDIF.
          ENDDO.
          DESCRIBE TABLE gt_nu_index LINES lv_maxpos.
          LOOP AT gt_nu_index ASSIGNING <nu_idx>.
            LOOP AT gt_index ASSIGNING <idx>
                          WHERE sqltab    = <nu_idx>-sqltab
                            AND fieldname = <nu_idx>-fieldname
                            AND position  = <nu_idx>-position.
              IF <nu_idx>-position = '1'.
                MOVE-CORRESPONDING <idx> TO gs_possindex.
                APPEND gs_possindex TO gt_possindex.
              ELSE.
                lv_pos = <idx>-position - 1.
                LOOP AT gt_possindex ASSIGNING <po_idx>.
                  IF <po_idx>-indexname = <idx>-indexname AND
                     <po_idx>-position  = lv_pos.
                    <po_idx>-position = <po_idx>-position + 1.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
            IF gt_possindex IS INITIAL.
              EXIT.
            ENDIF.
          ENDLOOP.
          READ TABLE gt_possindex WITH KEY position = lv_maxpos
                    TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            <cs_size>-has_index = 'P'.
          ENDIF.
          CLEAR: gt_nu_index, gt_possindex, lv_pos, lv_maxpos.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    REFRESH: gt_realrs, gt_realcs, gt_realrsindex.
  ENDIF.

  "Estimate move to row store
  IF do_slog = abap_true AND sy-dbsys(3) = 'HDB'.
    PERFORM rs_changes_s4.
  ENDIF.

  PERFORM calculate_subtotals.
  PERFORM get_anydb_size CHANGING wa_techd-anydbsize_n
                                  wa_techd-anydbsizedate.
  PERFORM sizing_calculation_cpu USING cpu_err.
  PERFORM fill_tech_details.
  "Persist results.
  IF sy-batch = abap_true OR p_db = abap_true.
    PERFORM call_output USING 'WRITE_TO_DB'.
  ENDIF.
  "convert to GB for display
  PERFORM b2gb_subtotals.

  "Check reliability
  IF est_init_data > 0 AND gt_error IS NOT INITIAL..
    PERFORM check_reliability USING gt_error est_init_data
                         CHANGING error_ratio.
  ENDIF.

  " Write the output
  PERFORM call_output USING 'OUTPUT_ON_SCREEN'.

  IF p_calib = abap_true.
    WRITE: / 'Reality:', 'main', 'dict',                    "#EC NOTEXT
              'data', 'index', 'misc'.                      "#EC NOTEXT
    WRITE: / 'Estimation:', 'main', 'dict',                 "#EC NOTEXT
              'data', 'index', 'misc'.                      "#EC NOTEXT
    LOOP AT lt_cs_cal ASSIGNING <cscal>.
      WRITE: / <cscal>-column_name, <cscal>-ms_main, <cscal>-ms_dict,
             <cscal>-ms_data, <cscal>-ms_index, <cscal>-ms_misc.
      WRITE: / <cscal>-column_name, <cscal>-est_ms_main,
             <cscal>-est_ms_dict, <cscal>-est_ms_data,
             <cscal>-est_ms_index, <cscal>-est_ms_misc.
    ENDLOOP.
    WRITE /.
  ENDIF.

  MESSAGE s090(sada) WITH 'End of sizing calculation'.      "#EC NOTEXT
  MESSAGE s090(sada)
    WITH 'Sizing done. Check result in the spool request'.  "#EC NOTEXT
*&---------------------------------------------------------------------*
*& FORM  do_rfc
*& Call RFC to read stats
*&
*&---------------------------------------------------------------------*

FORM do_rfc TABLES gt_bucket TYPE ty_t_hana_db_size
            USING  bucket    TYPE ty_hana_db_size-bucket
                   prec      TYPE c
                   mode      TYPE string.

  EXPORT p1 = gt_bucket TO DATA BUFFER data.
  DO.
    IF mode(3) = '1ST'.
      taskname = bucket."convert to c
      CONCATENATE mode taskname INTO taskname.
    ELSE. " retry with only one db table.
      READ TABLE gt_bucket INTO table_item INDEX 1.
      CONCATENATE mode table_item-db_table INTO taskname.
    ENDIF.
    CALL FUNCTION funcname
      STARTING NEW TASK taskname
      DESTINATION IN GROUP p_group
      PERFORMING collect_all ON END OF TASK
      EXPORTING
        precision             = prec
        mode                  = mode
        load                  = p_load
        hl                    = p_hl
        mac                   = p_mac
        clustprec             = p_clus
        mshl                  = p_mshl
        daag                  = p_daag
        force_count           = p_forc
        stat_age              = p_sage
        min_rec_cnt           = p_mrec
        hdb_empty_tables      = p_empt
        lraw                  = p_lraw
      CHANGING
        data                  = data
      EXCEPTIONS
        system_error          = 1
        system_failure        = 2
        communication_failure = 3
        resource_failure      = 4
        OTHERS                = 5.

    CASE sy-subrc.
      WHEN 0.
        l_sent = l_sent + 1.
        EXIT.
      WHEN 4.
        max_act = l_act - 1.
        WAIT UNTIL l_act < max_act. "wait for free wp
      WHEN OTHERS.
        MESSAGE x000(00).
    ENDCASE.
  ENDDO.
  l_act  = l_sent - l_recvd.

ENDFORM.                    "do_rfc


*&---------------------------------------------------------------------*
*& FORM  collect_all
*& Callback for RFC to collect results
*&
*&---------------------------------------------------------------------*

FORM collect_all                                            "#EC CALLED
  USING ret TYPE clike.                                     "#EC NEEDED

  DATA:
    data_part           TYPE xstring,
    lt_tables_part      TYPE TABLE OF ty_hana_db_size,
    lt_columns_part     TYPE TABLE OF ty_columns,
    lt_realrsindex_part TYPE typ_t_realrsindex,
    lt_realrs_part      TYPE typ_t_realrs,
    lt_realcs_part      TYPE typ_t_realcs,
    l_progress          TYPE p DECIMALS 1.

  RECEIVE RESULTS FROM FUNCTION funcname
    CHANGING
      data                    = data_part
      EXCEPTIONS
        system_error          = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 5.

  CASE sy-subrc.
    WHEN 0.
      IMPORT p1 = lt_tables_part p2 = lt_columns_part
             p3 = lt_realrsindex_part
             p4 = lt_realrs_part
             p5 = lt_realcs_part
             FROM DATA BUFFER data_part.
      APPEND LINES OF lt_tables_part  TO gt_tables.
      APPEND LINES OF lt_columns_part TO gt_columns.
      APPEND LINES OF lt_realrsindex_part TO gt_realrsindex.
      APPEND LINES OF lt_realrs_part TO gt_realrs.
      APPEND LINES OF lt_realcs_part TO gt_realcs.
    WHEN 2.
      IF ret(3) = 'RET'. "this is a retry => Log error.
        LOOP AT gt_input INTO table_item WHERE db_table = ret+6.
          ls_error-table_name  = table_item-table_name.
          ls_error-return_code = 99.
          IF table_item-tab_type = 'POOL' AND sy-dbsys(3) = 'DB6'.
            CONCATENATE 'DB2 DBSL Error. Check FAQ document'
                        'attached to SAP Note 1872170'
                        INTO outln.                         "#EC NOTEXT
            ls_error-error_message = outln.
          ELSE.
            ls_error-error_message = 'Check ST22'.          "#EC NOTEXT
          ENDIF.
          APPEND ls_error TO gt_error.
        ENDLOOP.
        MESSAGE s090(sada) WITH
                            'An error occurred while processing table'
                            table_item-table_name
                            'Check ST22 for details'.       "#EC NOTEXT

      ELSE. " this is the first time => retry.
        l_dump = l_dump + 1.
        MESSAGE s090(sada) WITH 'An error occurred in RFC call.'
                                'Check ST22 for details'
                                'The RFC call will be re-executed'.
                                                            "#EC NOTEXT
        IF l_dump > max_dump.
          " Too many RFC calls terminated in error. Check in ST22
          " the dumps that occurred before the MESSAGE_TYPE_X.
          " The MESSAGE_TYPE_X dump itself is not the problem.
          MESSAGE x000(00).
        ENDIF.
        LOOP AT gt_input INTO table_item WHERE bucket = ret+6.
          APPEND table_item TO lt_retry.
        ENDLOOP.
      ENDIF.
    WHEN OTHERS.
      MESSAGE x000(00).
  ENDCASE.
  l_recvd = l_recvd + 1.
  l_act = l_sent - l_recvd.
  IF mode(3) = '1ST'.
    l_progress = l_recvd / nb_buckets * 100.
    MESSAGE s001(sada) WITH l_progress '% of statistics collection'
                                       'is done'.           "#EC NOTEXT
  ENDIF.
  IF mode(3) = 'RET'.
    l_progress = l_recvd / l_retrycnt * 100.
    MESSAGE s090(sada) WITH l_progress '% of reprocessing is done'.
                                                            "#EC NOTEXT
  ENDIF.
ENDFORM.                    "collect_all

*&---------------------------------------------------------------------*
*& FORM  fill_row_list
*& Append list of Row Store tables (See SAP Note 1850112)
*&
*&---------------------------------------------------------------------*

FORM fill_row_list.
  DATA: l_rsnotins4 TYPE abap_bool.
  DEFINE mc_rs.
    IF l_rsnotins4 = abap_false.
      ls_tablename = &1. APPEND ls_tablename TO gt_row_tab.
    ELSE.
      IF do_slog = abap_false.
        ls_tablename = &1. APPEND ls_tablename TO gt_row_tab.
      ELSE.
        ls_tablename = &1. APPEND ls_tablename TO gt_rownots4_tab.
      ENDIF.
    ENDIF.
  END-OF-DEFINITION.

  "RS tables in 1610.
  mc_rs '/1DF/__SO000'. mc_rs '/OSP/T_REPINFO'.
  mc_rs '/SAPPO/ORDER_DAT'. mc_rs '/SDF/STAD'.
  mc_rs 'ABAPDOCU_BUFF'. mc_rs 'ABAPDOCU_TREE'. mc_rs 'ABAPHTML'.
  mc_rs 'ABDBG_ACTIVATION'. mc_rs 'ABDOCMODE'. mc_rs 'ABTREE'.
  mc_rs 'ADMI_FILE_RULES'. mc_rs 'ADQUINDX'.
  mc_rs 'ADR11S'. mc_rs 'ADR11S2'.
  mc_rs 'ADR12S'. mc_rs 'ADR12S2'. mc_rs 'AGDB'. mc_rs 'AGR_HIER_BOR'.
  mc_rs 'AII_PROFILES'. mc_rs 'AKB_FREEZE_SRC'. mc_rs 'AKB_INDX'.
  mc_rs 'AKB_USAGE_INFO'. mc_rs 'AKB_USAGE_INFO2'.
  mc_rs 'ALV_T_REFDATA1'.
  mc_rs 'ALV_T_TESTDATA1'. mc_rs 'ALV_T_TSCENARIO'.
  mc_rs 'APB_LAUNCHPAD'. mc_rs 'APB_LAUNCHPAD_V'. mc_rs 'APB_LPD_PATHS'.
  mc_rs 'APB_LPD_PICTURE'. mc_rs 'APB_LPD_SEARCH'.
  mc_rs 'ARC_ALT_KEY_CONV'.  mc_rs 'ARFCRDATA'.
  mc_rs 'ARFCRSTATE'. mc_rs 'ARFCSDATA'. mc_rs 'ARFCSSTATE'.
  mc_rs 'AROSTRINGTEST01'. mc_rs 'ASAFBCLUSTER'. mc_rs 'ATAB'.
  mc_rs 'AUNIT_PSA_MD_CP'. mc_rs 'AUODESC'. mc_rs 'AUT_D_DBTABLOG'.
  mc_rs 'AUT_D_ILM_CD_STR'. mc_rs 'AUT_I_STXL'.
  mc_rs 'BADI_STRING_COND'. mc_rs 'BADIIMPL_SORT'.
  mc_rs 'BAPISDXTH'. mc_rs 'BCF_DT_HIST_LOC'. mc_rs 'BCF_JAVA2TMS02'.
  mc_rs 'BCF_PCD_ID_MAP'. mc_rs 'BCF_WOC'. mc_rs 'BCF_WOC_MAP_OBJ'.
  mc_rs 'BCF_WOC_SUBOBJ'. mc_rs 'BCFBECONNHTTPPAR'.
  mc_rs 'BCFBECONNRFCPAR'. mc_rs 'BCFCR_EXIM'. mc_rs 'BCFCRLOGDTL'.
  mc_rs 'BCFCRLOGDTLT'. mc_rs 'BCFCROBJS'. mc_rs 'BCFCRPARA'.
  mc_rs 'BCFCRSRVS'. mc_rs 'BCFDPARAM'. mc_rs 'BCFINST'.
  mc_rs 'BCFINSTCLNT'. mc_rs 'BCFLACOBJ'. mc_rs 'BCFOBJECT'.
  mc_rs 'BCFPARCELS'. mc_rs 'BCFPPRTSTR'. mc_rs 'BCFPPRTXSTR'.
  mc_rs 'BCFRTCRAC'. mc_rs 'BCFRTCROBJS'. mc_rs 'BCFRTCRSRVS'.
  mc_rs 'BCFSPCONNHTTPPAR'. mc_rs 'BCFSPCONNRFCPAR'. mc_rs 'BCFTEMP'.
  mc_rs 'BCFUP'. mc_rs 'BCFUPOBJS'. mc_rs 'BDSCONT3'.
  mc_rs 'BENCHINDX'.
  mc_rs 'BGRFC_N_RUNNABLE'. mc_rs 'BGRFC_SRV_STATE'. mc_rs 'BIDT_T_ALL'.
  mc_rs 'BRR_ATTRIB'. mc_rs 'BRR_ATTRIB_C'.
  mc_rs 'BRR_CONT'. mc_rs 'BRR_CONT_C'. mc_rs 'BRR_HEADER'.
  mc_rs 'BRR_HEADER_C'. mc_rs 'BRR_TEXT'. mc_rs 'BRR_TEXT_C'.
  mc_rs 'BSA_D_REP_LOAD'. mc_rs 'BSA_I_VERSION'.
  mc_rs 'BSA_LOAD_MIRROR'. mc_rs 'BSA_LOAD_TEST'.
  mc_rs 'BSA_PLUGIN_REG'.
  mc_rs 'BTCCTL'.  mc_rs 'BWCONTMAST'.
  mc_rs 'BWCONTTMPL'. mc_rs 'CCMS_EDS_M'. mc_rs 'CCMSEDSMAP'.
  mc_rs 'CCMSEDSMSH'. mc_rs 'CCMSEDSSITERULE'. mc_rs 'CCSEC_ENC'.
  mc_rs 'CCSEC_ENCV'. mc_rs 'CDCLS'.
  mc_rs 'CFG_CONT_DATA'. mc_rs 'CFG_CONT_USER_CT'.
  mc_rs 'CGPLT_OPT_INDX'.
  mc_rs 'CLS_FAVORITES'. mc_rs 'CLU4'. mc_rs 'CLU5'.
  mc_rs 'CLUSTERVER'. mc_rs 'CLUTAB'.
  mc_rs 'CNS_CP_IMG'.  mc_rs 'CNVCDMCUCIA_KSP8'.
  mc_rs 'CNVCDMCUCIA_KWBS'.
  mc_rs 'CONFIG_ARCHIVE'. mc_rs 'CONFIGC_CTRL_CTX'.
  mc_rs 'COV_MEASUREMENTS'. mc_rs 'COV_RESULTS'.
  mc_rs 'COV_SERIES'. mc_rs 'COVDETVAR'.mc_rs 'COVMVHIS'.
  mc_rs 'COVMVSET'. mc_rs 'COVREF'. mc_rs 'COVRES'. mc_rs 'COVRES0'.
  mc_rs 'COVSETTING'.  mc_rs 'CRM_BSP_SYSINDX'.mc_rs 'CRMC_BL_CONTXT'.
  mc_rs 'CRMC_BL_DBXCHG'. mc_rs 'CRMC_BL_DTXCHG'.
  mc_rs 'CRMC_BL_QUE_VALS'. mc_rs 'CRMC_BL_QUERY'.
  mc_rs 'CRMC_BL_VAR_VALS'. mc_rs 'CRMC_BL_VARIANTS'.
  mc_rs 'CRMC_USABILITY'. mc_rs 'CRMCACHE'.
  mc_rs 'CTSOBJLIST'. mc_rs 'CTSOBJLISTT'. mc_rs 'CTSOBJPROS'.
  mc_rs 'DB2COPYI'.
  mc_rs 'DB2PROC'. mc_rs 'DB2UTILS_PARMS'. mc_rs 'DBA_DBH_REPORT'.
  mc_rs 'DBDIFF'. mc_rs 'DBMAPS'. mc_rs 'DBTABPRT'.
  mc_rs 'DDACL'. mc_rs 'DDALIAS'.
  mc_rs 'DDART'. mc_rs 'DDBUF'. mc_rs 'DDCDIM'. mc_rs 'DDCHKMSG'.
  mc_rs 'DDCNVCOUNT'. mc_rs 'DDCNVEXIT'. mc_rs 'DDCNVSTAT'.
  mc_rs 'DDCNVTABL'. mc_rs 'DDCNVTIMES'. mc_rs 'DDCNVUSR'.
  mc_rs 'DDCPRO'. mc_rs 'DDCPROT'. mc_rs 'DDCPROTAB'.
  mc_rs 'DDCQUEUE'. mc_rs 'DDCSTA'. mc_rs 'DDCSTAT'. mc_rs 'DDDTRENUPG'.
  mc_rs 'DDF4GP'. mc_rs 'DDF4PSINDX'. mc_rs 'DDFTX'. mc_rs 'DDICNVADM'.
  mc_rs 'DDICNVCTRL'. mc_rs 'DDICNVDAYS'. mc_rs 'DDICNVDIST'.
  mc_rs 'DDICNVEXC'. mc_rs 'DDICNVLST'. mc_rs 'DDLBBUFTST'.
  mc_rs 'DDLOADD'. mc_rs 'DDLOADH'. mc_rs 'DDLOG'. mc_rs 'DDMODSDB6'.
  mc_rs 'DDMTF'. mc_rs 'DDMTT'. mc_rs 'DDNTDONE'. mc_rs 'DDNTF'.
  mc_rs 'DDNTF_CONV_UC'. mc_rs 'DDNTF_HIST'. mc_rs 'DDNTLANG'.
  mc_rs 'DDNTT'. mc_rs 'DDNTT_CONV_UC'. mc_rs 'DDNTT_HIST'.
  mc_rs 'DDPART'. mc_rs 'DDPATH'.  mc_rs 'DDPROF'.
  mc_rs 'DDPRTUVAR'. mc_rs 'DDSERVPERF'.
  mc_rs 'DDSHENTITY'. mc_rs 'DDSHHVALUE'.
  mc_rs 'DDSHLPVERS'. mc_rs 'DDSHPVALUE'.
  mc_rs 'DDSPAR'. mc_rs 'DDSPAR1'. mc_rs 'DDSPAR2'. mc_rs 'DDSTATHIST'.
  mc_rs 'DDSTORAGE'. mc_rs 'DDSTORAGE1'. mc_rs 'DDSTORAGE2'.
  mc_rs 'DDTEST_DBTAB1'. mc_rs 'DDTEST_DBTAB2'. mc_rs 'DDTPOOLCNV'.
  mc_rs 'DDTYPES'. mc_rs 'DDTYPET'. mc_rs 'DDXTF'.
  mc_rs 'DDXTF_CONV_UC'. mc_rs 'DDXTT'. mc_rs 'DDXTT_CONV_UC'.
  mc_rs 'DDYTF'. mc_rs 'DDYTT'. mc_rs 'DEMO_BLOB_TABLE'.
  mc_rs 'DEMO_CLOB_TABLE'. mc_rs 'DEMO_LOB_TABLE'. mc_rs 'DF52A'.
  mc_rs 'DFKKSUM'. mc_rs 'DFKKCODCLUST'. mc_rs 'DFKKCORRRFDT'.
  mc_rs 'DOCCLV'. mc_rs 'DOE_HB_DEVICE_ID'. mc_rs 'DOKCL'.
  mc_rs 'DOKCLU'. mc_rs 'DSYCLV'. mc_rs 'DSYO1'. mc_rs 'DSYO2'.
  mc_rs 'DSYO3'. mc_rs 'DYNPFIELDS'.
  mc_rs 'DYNPS'.  mc_rs 'E071KS_PREVIEW'. mc_rs 'ECATT_TEST_TABLE'.
  mc_rs 'ECCRED_DATA'. mc_rs 'ECCRED_GEN_SDC'. mc_rs 'ECCRED_XML_STR'.
  mc_rs 'ECET_ARGS'. mc_rs 'ECET_ARTMP'. mc_rs 'ECET_BLOBS'.
  mc_rs 'ECET_BLOBS_EN'. mc_rs 'ECET_LOG'. mc_rs 'ECLOG_EXT'.
  mc_rs 'ECLOG_FAIL_ESF'. mc_rs 'ECLOG_FAIL_MSG'.
  mc_rs 'ECLOG_FAIL_VAR'. mc_rs 'ECLOG_MSG'. mc_rs 'ECLOG_RESTAB'.
  mc_rs 'ECLOG_XDAT'. mc_rs 'ECOBJUSE'. mc_rs 'ECOQ_LOG'.
  mc_rs 'ECSCEN_SP'. mc_rs 'ECSCEN_VER'. mc_rs 'ECSCR_BF'.
  mc_rs 'ECSCR_DATA'. mc_rs 'ECSCR_PEXT'. mc_rs 'ECSCR_TSVAR'.
  mc_rs 'ECSCR_TTAB'. mc_rs 'ECSCR_XML_STR'. mc_rs 'ECSCR_XML_STR_WD'.
  mc_rs 'ECSCR_XSD'. mc_rs 'ECSP_DEF'. mc_rs 'ECSP_PAR_DATA'.
  mc_rs 'ECSP_PAR_XML_STR'. mc_rs 'ECSP_USR'. mc_rs 'ECTAT_CC_DATA'.
  mc_rs 'ECTC_ATTACH'. mc_rs 'ECTC_DATA'. mc_rs 'ECTC_XML_STR'.
  mc_rs 'ECTD_BF'. mc_rs 'ECTD_DATA'. mc_rs 'ECTD_XML_STR'.
  mc_rs 'ECTP_JOB_DATA'. mc_rs 'ECTR_ACTIVATE_CS'. mc_rs 'ECTRACE'.
  mc_rs 'ECTRACE_CONTENT'. mc_rs 'ECVO_BUS_MSG'. mc_rs 'ECWD_DATA'.
  mc_rs 'ECWD_RECORD'. mc_rs 'ECWD_RECORD2'. mc_rs 'ECWD_UI'.
  mc_rs 'EDI30C'. mc_rs 'EDI40'. mc_rs 'EDIDOC'. mc_rs 'EFG_FORMX'.
  mc_rs 'ENH4DELTAB'. mc_rs 'ENH4DELTABD'.
  mc_rs 'ENHCONTRACTCONT'. mc_rs 'ENHCONTRACTTOOL'.
  mc_rs 'ENHCONTRACTTYP'. mc_rs 'ENHCROSS'. mc_rs 'ENHCROSSINXSPOT'.
  mc_rs 'ENHHEADER'. mc_rs 'ENHHEADERD'. mc_rs 'ENHINCINX'.
  mc_rs 'ENHLOG'. mc_rs 'ENHLOGD'. mc_rs 'ENHOBJ'.
  mc_rs 'ENHOBJCONTRACT'. mc_rs 'ENHOBJCONTRACTD'.
  mc_rs 'ENHSPOTCONTRACT'. mc_rs 'ENHSPOTHEADER'.
  mc_rs 'ESA_SAMPLE_OMCFG'. mc_rs 'ESA_SAMPLE_PD'.
  mc_rs 'ESA_SAMPLE_SO_I'. mc_rs 'ESI_CDT_BINOBJ'.
  mc_rs 'EUP_TSP_NY_REQ_C'. mc_rs 'EXPLAIN_INDX'.
  mc_rs 'FEH_MESS_PERS'. mc_rs 'FINPL'. mc_rs 'FKKDIHDTMP'.
  mc_rs 'FPCONTEXT'. mc_rs 'FPCONTEXTR'. mc_rs 'FPINTERFACE'.
  mc_rs 'FPLAYOUT'. mc_rs 'FPLAYOUTT'. mc_rs 'FPM_SAVED_SEARCH'.
  mc_rs 'FPP_CLUST'. mc_rs 'FPTESTDATA'.
  mc_rs 'FSI_CREDE_DT'. mc_rs 'FSI_CREDE_FC'.
  mc_rs 'FTA_DEFS'. mc_rs 'FTA_HEADER'. mc_rs 'GAMD_DSD'.
  mc_rs 'GAMD_FAST'. mc_rs 'GAMD_FAST_MAP'. mc_rs 'GAMD_FAST_WC'.
  mc_rs 'GAMD_FIELDS'. mc_rs 'GAMD_FILTER'. mc_rs 'GAMD_OMISSION'.
  mc_rs 'GAMD_PATH'. mc_rs 'GAMD_STAT'. mc_rs 'GBA_FIELDS'.
  mc_rs 'GBA_PERFORMANCE'. mc_rs 'GBA_ZOO_KEEP'.
  mc_rs 'GLOSSARY'. mc_rs 'GOSDIR'. mc_rs 'GVD_DATAFILE'.
  mc_rs 'HPDDICCHK'. mc_rs 'HTMLB_STYLES'. mc_rs 'IDEFIX'.
  mc_rs 'INDX_ESS'. mc_rs 'INDX_VER'.
  mc_rs 'INDXNC'. mc_rs 'IWB_SYNCPPMS_LOG'. mc_rs 'IWB3CONT1'.
  mc_rs 'J_TRANS_FULL01'.
  mc_rs 'J_TRANS_GEN01'. mc_rs 'J_TRANS_SINGLE01'. mc_rs 'J_TRANS01'.
  mc_rs 'J_TRANSALL'. mc_rs 'J_VEHICLE'. mc_rs 'J_VEHICLETYPE'.
  mc_rs 'KTEST11'. mc_rs 'KWPWCONT1'. mc_rs 'KWTCONT'.
  mc_rs 'LCA_EXST_ROUTINE'. mc_rs 'LCA02_BTC'. mc_rs 'LCCONNMODEBUFFER'.
  mc_rs 'LCRT_CHANGELOG'. mc_rs 'LCRT_CLASS'. mc_rs 'LCRT_CLASS_LU'.
  mc_rs 'LCRT_INDX'. mc_rs 'LCRT_INST'. mc_rs 'LCRT_INST_LU'.
  mc_rs 'LCRT_OBJ_REF_LU'. mc_rs 'LCRT_QUAL'. mc_rs 'LCRT_QUAL_LU'.
  mc_rs 'LCRT_ROLE_LU'. mc_rs 'LCRT_UPDATE'.
  mc_rs 'LICENSE_DATA'. mc_rs 'LTCX'. mc_rs 'LTDX'. mc_rs 'LTDXB'.
  mc_rs 'LTEX'. mc_rs 'LXE_REMRKS_LTE'. mc_rs 'LXE_REMRKS_STE'.
  mc_rs 'LXELOGS'. mc_rs 'M_ZUM0'. mc_rs 'MEREP_10100'.
  mc_rs 'MEREP_10700D'. mc_rs 'MEREP_201'. mc_rs 'MEREP_207'.
  mc_rs 'MEREP_210'. mc_rs 'MEREP_504'. mc_rs 'MEREP_504I'.
  mc_rs 'MEREP_504O'. mc_rs 'MEREP_510'. mc_rs 'MEREP_810'.
  mc_rs 'MEREP_811'. mc_rs 'MEREP_816'. mc_rs 'MESEC_STORAGE'.
  mc_rs 'MESYGENBODY'.
  mc_rs 'MMAP_SCE_COOKIE'. mc_rs 'MMW_BUF_DTL'. mc_rs 'MMW_IN_MSG_BUF'.
  mc_rs 'MMW_SES_ERR_INFO'. mc_rs 'MSFW_CACHE'.
  mc_rs 'MSFW_MAP_COOKIE'. mc_rs 'MSFW_SCENARIO_DT'. mc_rs 'NRIV'.
  mc_rs 'NRIV_LOKAL'. mc_rs 'NRIVSHADOW'. mc_rs 'NWBC_CONFIG'.
  mc_rs 'NWBC_NAV_TREE'. mc_rs 'NWBC_OBN_PARAM'.
  mc_rs 'OAPRE'. mc_rs 'ODQDATA'. mc_rs 'ODQDATA_C'. mc_rs 'ODQDATA_F'.
  mc_rs 'ODQQUEDES'. mc_rs 'ODQREQQUE'. mc_rs 'ODQRESP'.
  mc_rs 'ODQSSNQUE'. mc_rs 'ODQSSNREQQUE'. mc_rs 'OJDEF'.
  mc_rs 'OJDEFL'. mc_rs 'ORA_SQLC_DATA'. mc_rs 'ORA_STT_DATA_700'.
  mc_rs 'ORA_STT_HEAD_700'. mc_rs 'ORAPARTGENDDL'. mc_rs 'ORASTATS'.
  mc_rs 'OSVERIK02'. mc_rs 'OSVERIK02A'. mc_rs 'OSVERIK02B'.
  mc_rs 'OSVERIK03'. mc_rs 'OSVERIK03A'. mc_rs 'OSVERIK03B'.
  mc_rs 'OXT_PICTURE'. mc_rs 'PAK_ACCO_HINT'. mc_rs 'PAKDATBUF'.
  mc_rs 'PBR_INDX'. mc_rs 'PCA_SECURITY'. mc_rs 'PCA_SECURITY_RAW'.
  mc_rs 'PCDCLS'. mc_rs 'PCDPOS_STR'. mc_rs 'PKRT_ENV'.
  mc_rs 'PKRT_ENV_I'. mc_rs 'PKRT_ID_COUNTER'. mc_rs 'PKRT_INACTIVE'.
  mc_rs 'PKRT_INTFPACKIDX'. mc_rs 'PKRT_LOAD'. mc_rs 'PKRT_LOAD_DEP'.
  mc_rs 'PKRT_LOAD_DEP_I'. mc_rs 'PKRT_LOAD_I'. mc_rs 'PKRT_LOAD_STAT'.
  mc_rs 'PKRT_OBJECT_IDS'. mc_rs 'PKRT_RESULT'. mc_rs 'PKRT_USE_KINDS'.
  mc_rs 'PKRT_USE_KINDS_T'. mc_rs 'PLAN_TABLE'.
  mc_rs 'POWL_CHECK_LOG'. mc_rs 'POWL_FIELDCAT'. mc_rs 'POWL_RESULT'.
  mc_rs 'POWL_SELCRIT'. mc_rs 'PPF_TST_HISTORY'.
  mc_rs 'PRTTEST'. mc_rs 'PTEST1'.
  mc_rs 'PUTTB_SHD_IDX'. mc_rs 'PVER1'. mc_rs 'QIWKTAB'.
  mc_rs 'QRETDATA'. mc_rs 'QRFC_I_QIN_LOCK'. mc_rs 'QRFC_I_SDATA'.
  mc_rs 'QRFC_I_UNIT'. mc_rs 'QRFC_N_EXE_STATE'. mc_rs 'QRFC_N_QOUT'.
  mc_rs 'QRFC_N_QOUT_TOP'. mc_rs 'QRFC_N_REF_UNIT'.
  mc_rs 'QRFC_N_SDATA'. mc_rs 'QRFC_N_UNIT'. mc_rs 'QRFC_N_UNIT_LOCK'.
  mc_rs 'QRFC_O_SDATA'. mc_rs 'QRFC_O_UNIT'. mc_rs 'QRFCEVENT'.
  mc_rs 'QSENDDEST'. mc_rs 'QUEUESTATE_INDX'. mc_rs 'RBAMCONDITION'.
  mc_rs 'RBAMDIVINDX'. mc_rs 'RBAMFILTER'. mc_rs 'RBAMMATCHOPER'.
  mc_rs 'RBAMMATCHRESO'. mc_rs 'RBAMMATCHSUBJ'. mc_rs 'RBAMMSGSTATUS'.
  mc_rs 'RBAMNOTIFICATION'. mc_rs 'RBAMPOLICY'. mc_rs 'RBAMPROPERTY'.
  mc_rs 'RBAMRCVQUEUE'. mc_rs 'RBAMTGROUP'.  mc_rs 'RBAMTRACE'.
  mc_rs 'RBAMTROLE'. mc_rs 'RBAMXP_BACKJOB'. mc_rs 'RBAMXP_C_SVCFIL'.
  mc_rs 'RBAMXP_E_RTDATA'. mc_rs 'RBAMXP_E_SVCFIL'.
  mc_rs 'RBAMXP_RD_SVCFIL'. mc_rs 'RBAMXP_SVCFILCUS'.
  mc_rs 'RBAMXP_TD_ERRWAR'. mc_rs 'RBAMXP_TD_SVC_D'.
  mc_rs 'RBAMXP_TD_UD_AC'. mc_rs 'RBAMXP_TD_UD_PPS'.
  mc_rs 'RBAMXP_TD_UNIT_D'. mc_rs 'RBAMXP_TD_UPPS_I'. mc_rs 'RCFCONT03'.
  mc_rs 'RCFCONT04'. mc_rs 'RCMCONT01'. mc_rs 'RENCONT01'.
  mc_rs 'REST_SAMPLE_VFS'.
  mc_rs 'RMPSGENSPHISTS'. mc_rs 'RMPSPOIDCR_CLNT'.
  mc_rs 'RMPSPRO_MAILMIME'. mc_rs 'RMPSXML_FIELDCAT'.
  mc_rs 'RMPSXML_NMSPACE'.
  mc_rs 'RODPS_REPL_RID'. mc_rs 'ROOSESRMAP'.
  mc_rs 'RPRCONT01'. mc_rs 'RPRCONT02'. mc_rs 'RPRCONT03'.
  mc_rs 'RPRCONT04'. mc_rs 'RPRCONT05'. mc_rs 'RPRCONT06'.
  mc_rs 'RPRCONT08'.  mc_rs 'RRAMIOBJSHM'.
  mc_rs 'RRT_MDX_SCHEMA'. mc_rs 'RS38T_VAR'. mc_rs 'RSANT_PROCESS'.
  mc_rs 'RSD99'.
  mc_rs 'RZLLITAB'. mc_rs 'SABP_ACC_CACHE'. mc_rs 'SACONT01'.
  mc_rs 'SACONTURL'. mc_rs 'SAICACHE'. mc_rs 'SAICACHE_ERROR_I'.
  mc_rs 'SAICACHE_PERFDAT'. mc_rs 'SALV_BS_ADMIN'.
  mc_rs 'SALV_BS_BLOB_CUS'. mc_rs 'SALV_BS_BLOB_SAP'.
  mc_rs 'SALV_BS_TT_FILE'.
  mc_rs 'SALV_WD_BL_APPL'. mc_rs 'SALV_WD_BL_USER'. mc_rs 'SAMCO_MSG'.
  mc_rs 'SAML2_ARTIFACT'. mc_rs 'SAML2_ASSERT'. mc_rs 'SAML2_ENT_PARM'.
  mc_rs 'SAML2_ENT_RELST'. mc_rs 'SAML2_ENT_SRVC'.
  mc_rs 'SAML2_ENTITY'. mc_rs 'SAML2_OPM_PARM'. mc_rs 'SAML2_PIDFED'.
  mc_rs 'SAML2_REQUEST'. mc_rs 'SAML2_SESSION'. mc_rs 'SAMX_CONTROL'.
  mc_rs 'SAMX_REGISTRY'. mc_rs 'SAMX_TRACE_D'. mc_rs 'SAMXEVENTCONSREG'.
  mc_rs 'SAMXEVENTREGISTR'. mc_rs 'SAMXEVENTS'.
  mc_rs 'SAP_ARC_IDX_FLDS'. mc_rs 'SAPGUIHC_FAQS'.
  mc_rs 'SAPGUIHC_LINKS'. mc_rs 'SAPGUIHC_NOTES'.
  mc_rs 'SAPHC_FAQS_TEXT'. mc_rs 'SAPHC_LINKS_HEAD'.
  mc_rs 'SAPHC_NOTES'.  mc_rs 'SAPLIKEY'.
  mc_rs 'SAPWLSFIHD'. mc_rs 'SASACONT1'. mc_rs 'SATC_AC_ASPERR_B'.
  mc_rs 'SATC_AC_ASPERR_S'. mc_rs 'SATC_AC_CONFIG'.
  mc_rs 'SATC_AC_MODULE_B'. mc_rs 'SATC_AC_MSGDEF_B'.
  mc_rs 'SATC_AC_OBJLOG_B'. mc_rs 'SATC_AC_PLNERR_B'.
  mc_rs 'SATC_AC_PLNERR_S'. mc_rs 'SATC_AC_RESULT'.
  mc_rs 'SATC_AC_RESULTG'. mc_rs 'SATC_AC_RESULTMG'.
  mc_rs 'SATC_AC_RESULTMO'. mc_rs 'SATC_AC_RESULTP'.
  mc_rs 'SATC_AC_RESULTVT'. mc_rs 'SATC_AC_RSLT_AE'.
  mc_rs 'SATC_AC_RSLT_VT'. mc_rs 'SATC_AC_S_CONFIG'.
  mc_rs 'SATC_AC_S_STATE'. mc_rs 'SATC_AC_STATE'.
  mc_rs 'SATC_AC_VERDCT_B'.
  mc_rs 'SATC_AC_VERDCT_S'. mc_rs 'SATC_AC_VRDCT_B'.
  mc_rs 'SATC_AC_VRDCT_S'. mc_rs 'SATC_MD_CONF_VAL'.
  mc_rs 'SATC_MD_SRLZD'. mc_rs 'SATC_RT_RUN'. mc_rs 'SATC_RT_RUN_KEY'.
  mc_rs 'SATC_RT_RUN_LOG'. mc_rs 'SATC_RT_RUN_NTS'.
  mc_rs 'SATC_STD_TEST_B'. mc_rs 'SATC_UI_CFG_DATA'.
  mc_rs 'SATR_TAB'. mc_rs 'SAUNIT_FAV'.
  mc_rs 'SAUNIT_FAV_DATA'. mc_rs 'SBCTYPTAB3'. mc_rs 'SBCTYPTAB4'.
  mc_rs 'SBF_RU_USAGE_XML'. mc_rs 'SBFADXTH'. mc_rs 'SBTM_BO_REFBO'.
  mc_rs 'SBTM_BO_TNOTES'. mc_rs 'SBTM_LOG_ATTRBEF'.
  mc_rs 'SBTM_LOG_ATTRCUR'. mc_rs 'SBTM_MTA_ACT_BND'.
  mc_rs 'SBTM_RES_DTL'. mc_rs 'SBTM_RES_HDR'. mc_rs 'SBTM_TC_BND'.
  mc_rs 'SBTM_TCA_BND'. mc_rs 'SBTM_TD_BND'. mc_rs 'SBTM_TRC_DAT'.
  mc_rs 'SBTM_TRC_SEL'. mc_rs 'SBTM_TST_PERFDAT'.
  mc_rs 'SBTM_TST_RES_HDR'. mc_rs 'SBTM_TYP_CONT'. mc_rs 'SBTMCNTXML'.
  mc_rs 'SC2_CFG_ASSEMBLY'. mc_rs 'SC2_PG_QUERYC'.
  mc_rs 'SC2REP_CONFDATA'. mc_rs 'SC2TEFB889BB4AE'.
  mc_rs 'SCERT_BTM_BUFFER'.
  mc_rs 'SCERT_COUNTRY'. mc_rs 'SCERT_DUMP'. mc_rs 'SCERT_INDX_BUFF'.
  mc_rs 'SCIINS_OBJ'. mc_rs 'SCIINS_VAR'. mc_rs 'SCOL_ADAPTATION'.
  mc_rs 'SCOL_BO_ALTKEYS'. mc_rs 'SCOL_BO_NODE_ID'.
  mc_rs 'SCOL_BO_PROP'. mc_rs 'SCOL_BO_PROP_EXT'.
  mc_rs 'SCOL_ND_SCHEMA'.
  mc_rs 'SCOL_ND_STATEV'. mc_rs 'SCOL_TRACE_EXT'.
  mc_rs 'SDB_CLASS_SYSTEM'.
  mc_rs 'SDB_PUB_RULE'. mc_rs 'SDB_SEL_STMT'. mc_rs 'SDBCCMS'.
  mc_rs 'SDENDPOINTS'. mc_rs 'SDOE_DEVUPLDSTAT'.
  mc_rs 'SDOE_DISC_USR_IP'.  mc_rs 'SDOE_DISC_USR_S'.
  mc_rs 'SDOE_DOBJ_WSDL'. mc_rs 'SDOE_ESD_RESRC'.
  mc_rs 'SDOE_INBOUND_MSG'. mc_rs 'SDOE_PSH_MSGSTAT'.
  mc_rs 'SDOE_PUSH_ERRMSG'. mc_rs 'SDOE_PUSH_MSGS'.
  mc_rs 'SDOE_REGRESPONSE'. mc_rs 'SDOE_RESTART_LOG'.
  mc_rs 'SDOE_RSS_CSRV'. mc_rs 'SDOE_RSS_FDE'. mc_rs 'SDOE_RSS_ITME'.
  mc_rs 'SDOE_RSS_ITMT'. mc_rs 'SDOE_RSS_ITMTT'. mc_rs 'SDOE_RSS_SPEC'.
  mc_rs 'SDOE_RSS_XML'. mc_rs 'SDOE_SMS_TEXT'. mc_rs 'SDOE_WS_BUNDLE'.
  mc_rs 'SDOE_WS_MSG'. mc_rs 'SDOE_WS_SOAP_LOG'. mc_rs 'SDOE_WS_SUBSCR'.
  mc_rs 'SDOE_WS_WSDL_REG'. mc_rs 'SDSRPCDAPPL'. mc_rs 'SDSRPCDDB'.
  mc_rs 'SDSRPCDTMP'.
  mc_rs 'SEFS_CR_DIR_EXCL'.mc_rs 'SEFS_CR_FILE_EXT'.
  mc_rs 'SEFS_CR_HOST'. mc_rs 'SEFS_CR_REG_EXPR'.
  mc_rs 'SEFS_CR_SETTINGS'. mc_rs 'SEFS_LDAP_GROUP'.
  mc_rs 'SEFS_LDAP_USER'.  mc_rs 'SESA_SAFE_DRAFT'.
  mc_rs 'SESF_BSA_ATTRIB'.
  mc_rs 'SESF_BSA_BO_MAP'. mc_rs 'SESF_BSA_ENTITY'.
  mc_rs 'SESF_CL_VERSION'. mc_rs 'SESF_ERROR_LOG_I'.
  mc_rs 'SESF_FLOORPLANS'. mc_rs 'SESF_HC_DB'. mc_rs 'SESF_REP_LOAD'.
  mc_rs 'SESF_SHOBJ_TRACE'. mc_rs 'SESF_WB_SETTINGS'.
  mc_rs 'SESFTS_SETTINGS'. mc_rs 'SEUITEST'. mc_rs 'SEUITESTCL'.
  mc_rs 'SFCPL'. mc_rs 'SFHOA'. mc_rs 'SFHYT'.
  mc_rs 'SFS_SEARCHDEF_IR'. mc_rs 'SFS_SEARCHDEF_R'.
  mc_rs 'SFSG_ACT_VIEW'. mc_rs 'SFSG_BO_CNSTCY'.
  mc_rs 'SFSG_FSPB_C'. mc_rs 'SFSG_FSPB_D'. mc_rs 'SFSG_FSPB_N'.
  mc_rs 'SFSG_FSPB_W'. mc_rs 'SFSG_LOAD_ACTN'. mc_rs 'SFSG_NOTE_META_I'.
  mc_rs 'SFSG_OLTP_ALIASD'. mc_rs 'SFSG_PERFORMANCE'.
  mc_rs 'SFSG_PF_OPTION'. mc_rs 'SFSG_QR_BUF'.
  mc_rs 'SFSG_QR_RT'. mc_rs 'SFSG_RELOAD_REQ'.
  mc_rs 'SFSG_REPL_BTC_RT'. mc_rs 'SFSG_REPL_LOG'.
  mc_rs 'SFSG_REPL_META_H'. mc_rs 'SFSG_REPL_PERF'.
  mc_rs 'SFSG_REPL_PROXY'. mc_rs 'SFSG_TREX_JDX_JP'.
  mc_rs 'SFWPL'. mc_rs 'SI_RQ_FILECNTRL'. mc_rs 'SILN_DO_OWNERSHP'.
  mc_rs 'SKWF_SHBUF'. mc_rs 'SKWF_URL_IO_CH'. mc_rs 'SKWF_URLIO'.
  mc_rs 'SLEI_BINDING'. mc_rs 'SLEI_BINDING_C'. mc_rs 'SLEI_CONDITION'.
  mc_rs 'SLEI_CONDITION_C'. mc_rs 'SLEI_TRC_DAT'.
  mc_rs 'SLIC_SERVER_LOG'. mc_rs 'SLIN_CACHE'. mc_rs 'SLIN_CACHE_RES'.
  mc_rs 'SLM_PROC_CON'. mc_rs 'SLMAUTO_CNT'.
  mc_rs 'SLMAUTO_COMP_P'. mc_rs 'SLMAUTO_CONTENT'.
  mc_rs 'SLMAUTO_CONTEXT'. mc_rs 'SLMAUTO_CTXT'.
  mc_rs 'SLMAUTO_CTXT_CI'. mc_rs 'SLMAUTO_EXT_CNTT'.
  mc_rs 'SLMAUTO_PROC_CON'. mc_rs 'SLMAUTO_PROTOCOL'.
  mc_rs 'SLMAUTO_RESOURCE'. mc_rs 'SLMAUTO_S_CTXT'.
  mc_rs 'SLMAUTO_S_PROT'. mc_rs 'SLMAUTO_STATE'.
  mc_rs 'SMGF_RPSST'. mc_rs 'SMGF_STMNT'. mc_rs 'SMMW_IN_MSG_BUF'.
  mc_rs 'SMMW_INBOX_DATA'. mc_rs 'SMMW_LDQ_PAR_MSG'.
  mc_rs 'SMMW_LOG_LOB'. mc_rs 'SMMW_MC_STORE'. mc_rs 'SMMW_MCD_DMY_AVE'.
  mc_rs 'SMMW_MSG_BODY'. mc_rs 'SMMW_STORE_ZIP'. mc_rs 'SMODISRC'.
  mc_rs 'SMODISRCI'. mc_rs 'SMPPBIND'. mc_rs 'SMPPCONT'.
  mc_rs 'SMPPREL3'. mc_rs 'SMPPXLOG'. mc_rs 'SMTC_UDEF'.
  mc_rs 'SNMPREQSTAT'. mc_rs 'SNMPRESULTS'.
  mc_rs 'SNWD_BPA'. mc_rs 'SNWD_COMPANY'. mc_rs 'SNWD_DG_PARAMS'.
  mc_rs 'SNWD_ECMA'. mc_rs 'SNWD_EPM_INDX'. mc_rs 'SNWD_SNAPSHOTS'.
  mc_rs 'SOTR_TEXTU'. mc_rs 'SOUTT_FCCTX'. mc_rs 'SOUTT_QATT'.
  mc_rs 'SOUTT_QUEUE'. mc_rs 'SOUTT_USGRIGHT'. mc_rs 'SPAF_CONF_TRCDAT'.
  mc_rs 'SPAF_ERC_RSLTN'. mc_rs 'SPAF_ERCTD_OPBND'.
  mc_rs 'SPAF_ERD_OBJ_INA'. mc_rs 'SPAF_ERR_APPL_BO'.
  mc_rs 'SPAF_ERR_BO_INST'. mc_rs 'SPAF_ERR_ENVLOC'.
  mc_rs 'SPAF_ERR_LOC_ATT'. mc_rs 'SPAF_ERR_LOG_LOC'.
  mc_rs 'SPAF_ERR_LOG_MSG'. mc_rs 'SPAF_ERR_MSG'.
  mc_rs 'SPAF_ERR_RD_CNT'. mc_rs 'SPAF_PADED_ACTNS'.
  mc_rs 'SPAF_PADED_CNDS'. mc_rs 'SPAF_PADED_ENTRY'.
  mc_rs 'SPAF_PADED_EVRNS'. mc_rs 'SPAF_PADED_MPS'.
  mc_rs 'SPAF_TEST_DOCS'. mc_rs 'SPAF_WFTRC_DAT'.
  mc_rs 'SPAK_HYBRID_LOG'. mc_rs 'SPROCONT1'.
  mc_rs 'SRM_INDX1'. mc_rs 'SRM_INDX2'. mc_rs 'SRM_INDX3'.
  mc_rs 'SRM_INDX4'. mc_rs 'SRM_INDX5'.
  mc_rs 'SRMCOMPID'. mc_rs 'SRMCOMPIFID'. mc_rs 'SRMLOCSRTTBL'.
  mc_rs 'SRMPOIDCR'.
  mc_rs 'SRMPOIDCR_CLNT'. mc_rs 'SRTFT_ASSIGN'.
  mc_rs 'SRTFT_ASSIGN_SEC'. mc_rs 'SRTFT_QDATA_LOG'.
  mc_rs 'SRTFT_QUEUE'. mc_rs 'SRTFT_QUEUE_DATA'.
  mc_rs 'SRTFT_QUEUE_LOG'. mc_rs 'SRTFT_SA_APPL'.
  mc_rs 'SSPRTCONFIG'. mc_rs 'SSPRTPROVREPORT'.
  mc_rs 'SSPRTPROVREPORT1'. mc_rs 'STERM_INDX'. mc_rs 'STK_STYLES'.
  mc_rs 'STODOINF'. mc_rs 'SVER_64BIT_INDX'. mc_rs 'SVER_ABAP_COV'.
  mc_rs 'SVER_DECF_DFPRES'. mc_rs 'SVET_COV_PCKG_OB'.
  mc_rs 'SVET_MAIL_CACHE'. mc_rs 'SVET_RUNTIME'.
  mc_rs 'SVET_VERI_DATA'. mc_rs 'SVMCRT_TEST_DATA'.
  mc_rs 'SWF_SLS_TRC_DAT'. mc_rs 'SWLN3WORKL'.
  mc_rs 'SWNCCOLLAGGFILT'. mc_rs 'SWNCMOMIGR'. mc_rs 'SWNCMONI'.
  mc_rs 'SXICACHE'. mc_rs 'SXIPERF_INDX'. mc_rs 'SXISLDAEC'.
  mc_rs 'SXMS_LMS_EXTR'. mc_rs 'SXMSAGGDAT'. mc_rs 'SXMSAGGMAP'.
  mc_rs 'SXMSCLUP'. mc_rs 'SXMSCLUP2'. mc_rs 'SXMSCLUR'.
  mc_rs 'SXMSCLUR2'. mc_rs 'SXMSSYERRT'. mc_rs 'SXMSTRC_DAT'.
  mc_rs 'SYSTABSTMT'. mc_rs 'T7KW_SDOKCONT1'. mc_rs 'T7XP_TEXT'.
  mc_rs 'T7XSSSERSTRING'. mc_rs 'T7XSSSERSTRLAN'. mc_rs 'TAAN_DATA'.
  mc_rs 'TAGTREEVIEW'.
  mc_rs 'TATGPC'. mc_rs 'TATGPGB'. mc_rs 'TATOPC'. mc_rs 'TATOPGA'.
  mc_rs 'TATOPX'. mc_rs 'TATPPC'. mc_rs 'TATPPGB'. mc_rs 'TATVPC'.
  mc_rs 'TATVPGA'. mc_rs 'TAUNIT_PROG_INFO'. mc_rs 'TBL_PUB_RULE_CLS'.
  mc_rs 'TBOOKSHOP'. mc_rs 'TBRF_TRACE'. mc_rs 'TBRF134'.
  mc_rs 'TBRF911'. mc_rs 'TBTC_EXT_SDL'. mc_rs 'TBTC_SPOOLID'.
  mc_rs 'TBTC_TASK'. mc_rs 'TBTC_TASK_DET'. mc_rs 'TBTCA'.
  mc_rs 'TBTCB'. mc_rs 'TBTCCNTXT'. mc_rs 'TBTCCTXTT'.
  mc_rs 'TBTCCTXTTP'. mc_rs 'TBTCI'. mc_rs 'TBTCJSTEP'. mc_rs 'TBTCO'.
  mc_rs 'TBTCP'. mc_rs 'TBTCR'. mc_rs 'TBTCS'. mc_rs 'TCPSPTL'.
  mc_rs 'TERCL'. mc_rs 'TERCL2'. mc_rs 'TERCL3'. mc_rs 'TERM'.
  mc_rs 'TERRD'. mc_rs 'TESTFB'.
  mc_rs 'TFKK_CORRSPNDPBP'. mc_rs 'TFPPDATASETS'.
  mc_rs 'TFPPFORMS'. mc_rs 'TFPPMEASURE'. mc_rs 'TFSAVE'.
  mc_rs 'THDS_NODE'. mc_rs 'THDS_NODE_A'. mc_rs 'THDS_NODE_S'.
  mc_rs 'THDS_TREE_ACT_A'. mc_rs 'THDS_TREE_ACT_S'.
  mc_rs 'THDS_TREE_ACTIVE'. mc_rs 'THDS_TREE_HIST'.
  mc_rs 'THDS_TREE_HIST_A'. mc_rs 'THDS_TREE_HIST_S'.
  mc_rs 'TIDYVERI'. mc_rs 'TLANINDX'.
  mc_rs 'TMSBCIXBOX'. mc_rs 'TPDA_TABLE_CONFC'. mc_rs 'TPDAINDX'.
  mc_rs 'TRD_DYN'. mc_rs 'TRD_EXTRACTOR'.
  mc_rs 'TRD_LOG'. mc_rs 'TREX_DELTA_QUEUE'. mc_rs 'TREXBCREQUESTTAB'.
  mc_rs 'TREXBCTEST_BIN'. mc_rs 'TREXBCTESTTAB2'.
  mc_rs 'TREXBCTESTTAB4'. mc_rs 'TREXBCTESTTAB7'.
  mc_rs 'TREXBCTESTTAB8'. mc_rs 'TRFC_I_SDATA'. mc_rs 'TRFC_I_UNIT'.
  mc_rs 'TRFC_O_SDATA'. mc_rs 'TRFC_O_UNIT'. mc_rs 'TRFC_O_UNIT_LOCK'.
  mc_rs 'TRFCQDATA'. mc_rs 'TRFCQIN'. mc_rs 'TRFCQOUT'.
  mc_rs 'TRFCQSTATE'. mc_rs 'TSIW_HISTORY'. mc_rs 'TSP0B'.
  mc_rs 'TSPOPTIONS'.
  mc_rs 'TSPOVERPDL'. mc_rs 'TST01'. mc_rs 'TST02'. mc_rs 'TST03'.
  mc_rs 'TTSTRCOMP'. mc_rs 'TUPROTS'. mc_rs 'TUWS_DATA'.
  mc_rs 'TUWS_FORMSERVICE'. mc_rs 'TUWS_PDF'. mc_rs 'TUWS_SURVEY'.
  mc_rs 'TUWS_SURVEY_GRP'. mc_rs 'TUWS_TARGETHIER'.
  mc_rs 'TVEP6'. mc_rs 'TVERCCQCOV'. mc_rs 'TVERCCQCOV_F'.
  mc_rs 'TVERCCQCOV_T'.
  mc_rs 'TXV_BATCH_LOG'. mc_rs 'TXV_BATCHLOG'. mc_rs 'TXV_CONF_JOB_LOG'.
  mc_rs 'TXV_CONF_LOG'. mc_rs 'TXV_CONFJOB_LOG'. mc_rs 'TXV_CONFJOBLOG'.
  mc_rs 'TXV_CONFLOG'. mc_rs 'TXV_JOB'. mc_rs 'TXV_JOB_LOG'.
  mc_rs 'TXV_JOB_MEMORY'. mc_rs 'TXV_JOB_STEP'.
  mc_rs 'TXV_JOB_STEP_LOG'. mc_rs 'TXV_JOB_TEXT_C'.
  mc_rs 'TXV_JOB_TEXT_CN'. mc_rs 'TXV_JOB_TEXT_CR1'.
  mc_rs 'TXV_JOB_TEXT_CW'. mc_rs 'TXV_JOB_TEXT_KEY'.
  mc_rs 'TXV_JOB_TEXT_SEQ'. mc_rs 'TXV_JOB_TEXT_WP'.
  mc_rs 'TXV_JOBDIR_HIST'. mc_rs 'TXV_LCHAIN_LOG'. mc_rs 'TXV_MEMORY'.
  mc_rs 'TXV_MEMORY_EXC'. mc_rs 'TXV_MEMORY_EXC_L'.
  mc_rs 'TXV_SLD_PROD'. mc_rs 'TXV_SLD_PRODUCT'.
  mc_rs 'TXV_SLD_SWC'. mc_rs 'TXV_SLD_SWCV'.
  mc_rs 'TXV_SLD_SWCV_ADD'. mc_rs 'TXV_TOOL_CONFIG'.
  mc_rs 'TXV_TRCO_ASS'. mc_rs 'TXV_TRDS_ABBR'.
  mc_rs 'TXV_TRDS_FRULE'. mc_rs 'TXV_TRDS_GRULE'.
  mc_rs 'TXV_TRDS_HEADER'. mc_rs 'TXV_TRDS_INFL'.
  mc_rs 'TXV_TRDS_RULE'. mc_rs 'TXV_TROA_ASS'. mc_rs 'TXV_TRSC_HEADER'.
  mc_rs 'TXV_TRSC_SCV'. mc_rs 'TXV_TRSC_SDHEAD'.
  mc_rs 'TXV_TRSC_SDOSEL'. mc_rs 'TXV_TRSD_HEADER'.
  mc_rs 'TXV_TRSD_OBJSEL'. mc_rs 'TXV_TRXX_INT'.
  mc_rs 'TXVCACHEDATA'. mc_rs 'TZRR_ANNOT'. mc_rs 'TZRR_DEP_PROP'.
  mc_rs 'TZRR_DEP_TST'. mc_rs 'TZRR_EVATTR'. mc_rs 'TZRR_EVCMD'.
  mc_rs 'TZRR_EVPAR'. mc_rs 'TZRR_EXPROP'. mc_rs 'TZRR_IFRAG'.
  mc_rs 'TZRR_PROP'. mc_rs 'TZRR_RSATTR'. mc_rs 'TZRR_XML'.
  mc_rs 'UTAB'. mc_rs 'UXS_DATA'. mc_rs 'V40_DDXTF'. mc_rs 'V40_DDXTT'.
  mc_rs 'V46_DDXTF'. mc_rs 'V46_DDXTT'.
  mc_rs 'VBDATA'. mc_rs 'VBHDR'. mc_rs 'VBLOG'. mc_rs 'VBMOD'.
  mc_rs 'VER_ACC_OUTPUT'. mc_rs 'VER_CLUSTR'.
  mc_rs 'VER_TIDY_HTML'. mc_rs 'VER01969_MASTER'.
  mc_rs 'VER03984_STRING'.
  mc_rs 'VER05706_RESULT'. mc_rs 'VER07781'. mc_rs 'VER07814_T'.
  mc_rs 'VER07858'. mc_rs 'VER07858_1'. mc_rs 'VER07958'.
  mc_rs 'VER07958_1'.mc_rs 'VERCLUSTER'. mc_rs 'VERCRTAB'.
  mc_rs 'VERI_INDX'. mc_rs 'VERI_STMTS'. mc_rs 'VERI_STRG'.
  mc_rs 'VERI_STRG2'. mc_rs 'VERI_TYPES'.
  mc_rs 'WBCROSS_HASH'. mc_rs 'WBCROSSGTL'.

  "RS tables in SoH and S/4 HANA Finance but no longer in S/4 HANA
  "  IF do_slog = abap_false.
  l_rsnotins4 = abap_true.
  mc_rs '/ASU/CONTENTNT'.
  mc_rs '/ASU/CONTENTNVT'. mc_rs '/ASU/CONTENTT'.
  mc_rs '/ASU/CONTENTXML'. mc_rs '/ASU/STEPST'. mc_rs '/ASU/STEPST_V'.
  mc_rs '/BDL/_CLUSTL'. mc_rs '/BDL/BDLFUPDEF'. mc_rs '/BDL/TMPDAT'.
  mc_rs '/SAPDMC/LSODOC'. mc_rs'/ASU/CONTENTV'.
  mc_rs '/SAPDMC/LSOPRT'. mc_rs '/GC1/TACTDEL'.
  mc_rs '/GC1/TCB_BUFF'.  mc_rs '/GC1/TSCOPE_BUFF'.
  mc_rs '/SCMB/DF_CC_TRAN'. mc_rs '/SCMB/DF_DOC_VAL'.
  mc_rs '/SCMB/TREEVIEW2'. mc_rs '/SDF/CLI_TRC'.
  mc_rs '/SDF/CMO_IACJSCR'. mc_rs '/SDF/INDX'. mc_rs '/SDF/LC_DBCON'.
  mc_rs '/SDF/MON'. mc_rs '/SDF/MSS_CUST'.
  mc_rs 'ACO_GROUP'. mc_rs 'ADR11'. mc_rs 'ADR12'.
  mc_rs 'ALCSMCONF'.
  mc_rs 'ALIMCDATA'. mc_rs 'ALMONIDEF'. mc_rs 'ALMONISETS'.
  mc_rs 'ALMONITORS'. mc_rs 'ALMRULES'. mc_rs 'ALMSETSV2'.
  mc_rs 'ALPERFDB'. mc_rs 'ALPERFDB_C'. mc_rs 'ALPERSONEL'.
  mc_rs 'ALPF_TESTDATA'. mc_rs 'ALPFDBVAR'. mc_rs 'ALPFREP_CAT'.
  mc_rs 'ALPFREPDEF'. mc_rs 'ALPFREPORTDEF'. mc_rs 'ALPFWSCHEM'.
  mc_rs 'ALV_T_PROG'.
  mc_rs 'ALV_T_T2'. mc_rs 'APQD'. mc_rs 'ARC_DESTR_RUN'.
  mc_rs 'AUDDD_FOR_LOHS'. mc_rs 'AUDDD_FOR_XINX'.
  mc_rs 'BANK_PACK_PARAMS'. mc_rs 'BANK_PACKMAN_ADD'.
  mc_rs 'BANK_PP_PAR_PCR'. mc_rs 'BANK_PP_PARAMS'.
  mc_rs 'BANK_PP_RUNPARM'. mc_rs 'BANK_PP_SRV_PCK'.
  mc_rs 'BDL_CLUSTL'. mc_rs 'BDLDATCOL'. mc_rs 'BDLFUPDEF'.
  mc_rs 'BDLTMPDAT'. mc_rs 'BDS_CONT1'.  mc_rs 'BDSCONT11'.
  mc_rs 'BDSCONT10'. mc_rs 'BDSCONT12'.    mc_rs 'BDSCONT8'.
  mc_rs 'BDSCONT9'. mc_rs 'BDSCONT14'. mc_rs 'BDSCONT16'.
  mc_rs 'BDSCONT2'. mc_rs 'BDSCONT20'. mc_rs 'BDSCONT21'.
  mc_rs 'BDSCONT24A'.mc_rs 'BDSCONT31'.
  mc_rs 'BDSCONT6'. mc_rs 'BDSCONT7'.  mc_rs 'BGRFC_EVENT_PARM'.
  mc_rs 'BIZC_TASK_INDEX'. mc_rs 'BIZCDOCBUF'. mc_rs 'BIZCDOCBUFASC'.
  mc_rs 'BIZCDOCBUFBIN'. mc_rs 'BIZCRCFGT'. mc_rs 'BIZCRDC'.
  mc_rs 'BIZCROBJ'. mc_rs 'BIZCROBJT'. mc_rs 'BL01_INDX'.
  mc_rs 'BRATEXT'. mc_rs 'BSPDEFPAG'. mc_rs 'BSPTESTSUITE'.
  mc_rs 'BWCONTHIER'.  mc_rs 'CAT_DP_TAB'. mc_rs 'CAT_SVARS'.
  mc_rs 'CATX'.mc_rs 'CC2S_CONSTRAINTS'.
  mc_rs 'CC2S_CUSMDATA'. mc_rs 'CCMSBI_TESTCASES'. mc_rs 'CDTESTKEY'.
  mc_rs 'CFONT_CHAR_WIDTH'. mc_rs 'CFONT_TEMP_FILE'.
  mc_rs 'CHIP_CACHE_HDR'.
  mc_rs 'CHIP_CACHE_HDRM'. mc_rs 'CHIP_CACHE_HDRMT'.
  mc_rs 'CHIP_CACHE_HDRT'. mc_rs 'CLBUF'. mc_rs 'CLS_ASSIGNMENT'.
  mc_rs 'CLS_LINKED_OBJ'.
  mc_rs 'CLS_RE_CONDITION'. mc_rs 'CLS_RUN_ASSGNMNT'.
  mc_rs 'CLS_RUN_PARAMS'.  mc_rs 'CMLTEX'. mc_rs 'CNTLSTREAM'.
  mc_rs 'COL_INDX_CACHE'.  mc_rs 'CONTCOMPX'. mc_rs 'CONTEXTB'.
  mc_rs 'CPT_ACTION'. mc_rs 'CPT_CORR_DEF'.
  mc_rs 'CPT_CORRELATOR'. mc_rs 'CPT_LAYER'. mc_rs 'CPT_METRIC'.
  mc_rs 'CPT_METRIC_DEF'. mc_rs 'CPT_MODULE'. mc_rs 'CPT_ORIGIN'.
  mc_rs 'CPT_PROVIDER'. mc_rs 'CPT_REQ_META'. mc_rs 'CRATEXT'.
  mc_rs 'CREP_CERT'.  mc_rs 'CRM_CHECK_CONFIG'.
  mc_rs 'CRM_ECATT'. mc_rs 'CRMADPTST1'. mc_rs 'CSM_CNTXT'.
  mc_rs 'CSM_EXTVAL'. mc_rs 'CSMBK_CL'. mc_rs 'CSMBK_CL2'.
  mc_rs 'CSMBK_TK'. mc_rs 'CSMBK_TK2'. mc_rs 'CSUM_EV_DATA'.
  mc_rs 'CSUM_EV_SELE'.  mc_rs 'CUEX'. mc_rs 'CUKN'. mc_rs 'CURSCOD'.
  mc_rs 'CWB_HIST_D'. mc_rs 'CWBCIDATA'. mc_rs 'CWBCIDATAOBJ'.
  mc_rs 'CWBNTDATA'. mc_rs 'CWBRFCCACH'.
  mc_rs 'CVEP1'. mc_rs 'CVEP2'. mc_rs 'CVER1'. mc_rs 'CVER2'.
  mc_rs 'CVER3'. mc_rs 'CVER4'. mc_rs 'CVER5'.
  mc_rs 'D010INC'. mc_rs 'D010TAB'. mc_rs 'D342L'.
  mc_rs 'D345T'. mc_rs 'D346T'. mc_rs 'D347T'.  mc_rs 'DB4_CLUSTER'.
  mc_rs 'DB6_RESET_DATA'. mc_rs 'DB6ALRTCT'. mc_rs 'DB6CSTRACE'.
  mc_rs 'DB6PMSQ_DBS'. mc_rs 'DB6SCRIPT'.
  mc_rs 'DBDATA'. mc_rs 'DD01L'. mc_rs 'DD01T'.
  mc_rs 'DD02L'. mc_rs 'DD02T'.  mc_rs 'DD02DB6'. mc_rs 'DD02INF'.
  mc_rs 'DD02MSS'. mc_rs 'DD03L'.
  mc_rs 'DD03L_SAVE'.  mc_rs 'DD03L_SHEAD'. mc_rs 'DD07L'.
  mc_rs 'DD07T'. mc_rs 'DD03T'. mc_rs 'DD04L'.
  mc_rs 'DD04T'. mc_rs 'DD05S'. mc_rs 'DD06L'. mc_rs 'DD06T'.
  mc_rs 'DD08L'. mc_rs 'DD08T'.
  mc_rs 'DD09C'. mc_rs 'DD09L'. mc_rs 'DD10L'. mc_rs 'DD12DB6'.
  mc_rs 'DD12INF'. mc_rs 'DD12L'. mc_rs 'DD12MSS'. mc_rs 'DD12T'.
  mc_rs 'DD14S'. mc_rs 'DD15L'. mc_rs 'DD15T'. mc_rs 'DD16S'.
  mc_rs 'DD17S'. mc_rs 'DD20L'. mc_rs 'DD20T'. mc_rs 'DD21S'.
  mc_rs 'DD23L'. mc_rs 'DD23T'. mc_rs 'DD24S'. mc_rs 'DD25L'.
  mc_rs 'DD25T'. mc_rs 'DD26S'. mc_rs 'DD27S'. mc_rs 'DD28S'.
  mc_rs 'DD29L'. mc_rs 'DD29T'. mc_rs 'DD30L'. mc_rs 'DD30T'.
  mc_rs 'DD31S'. mc_rs 'DD32S'. mc_rs 'DD33S'. mc_rs 'DD35L'.
  mc_rs 'DD36S'. mc_rs 'DD40L'. mc_rs 'DD40T'. mc_rs 'DD42S'.
  mc_rs 'DD43L'. mc_rs 'DD43T'. mc_rs 'DD90L'. mc_rs 'DD90T'.
  mc_rs 'DD91S'. mc_rs 'DD92S'. mc_rs 'DD93S'. mc_rs 'DD94S'.
  mc_rs 'DD96S'. mc_rs 'DD97S'.
  mc_rs 'DDPRH'. mc_rs 'DDPRS'. mc_rs 'DDSHDEFSH'. mc_rs 'DDSHPVAL50'.
  mc_rs 'DRATEXT'. mc_rs 'DWTREE'.  mc_rs 'DYNPLOAD'.
  mc_rs 'DYNPSOURCE'. mc_rs 'DYNPTXTLD'.
  mc_rs 'E071K_STR'.   mc_rs 'ERCLASS_LOCAL'.
  mc_rs 'ERCLASS_NONSAP'. mc_rs 'ERCLASS_SAP'.
  mc_rs 'ESH_ADM_BIN_DATA'. mc_rs 'ESH_ADM_BIN_ERR'.
  mc_rs 'ESH_ADM_GEN_CONN'. mc_rs 'ESH_ADM_SC_MON'.
  mc_rs 'ESH_CA_CLUSTER'. mc_rs 'ESH_CM_SYS_COUP'.
  mc_rs 'ESH_CO_CPOINTER'. mc_rs 'ESH_CO_QLOGQ'.
  mc_rs 'ESH_CRT_INDEXING'. mc_rs 'ESH_EX_CPOINTER'.
  mc_rs 'ESH_GEN_SWC_DATA'. mc_rs 'ESH_GEN_SWC_DESC'.
  mc_rs 'ESH_OM_AAUTHCVPB'. mc_rs 'ESH_OM_AOTYPNBN'.
  mc_rs 'ESH_OM_ARELATFV'. mc_rs 'ESH_OM_CAUTHCVPB'.
  mc_rs 'ESH_OM_COTYPNBN'. mc_rs 'ESH_OM_CRELATFV'.
  mc_rs 'ESH_OM_IAUTHCVPB'. mc_rs 'ESH_OM_IE_BUFFER'.
  mc_rs 'ESH_OM_IOTYPNBN'. mc_rs 'ESH_OM_IRELATFV'.
  mc_rs 'ESH_OM_PC_EXEMPT'. mc_rs 'ESH_OM_SOTYPNBN'.
  mc_rs 'ESH_OM_TRANSPORT'. mc_rs 'ESH_QL_QLOGQ'.
  mc_rs 'ESH_QL_QLOGQB'.
  mc_rs 'ESH_QP_QUE_GRAM'. mc_rs 'ESH_SE_RUNTIME'.
  mc_rs 'ESH_TAU_CL_PDTV'. mc_rs 'ESH_TAU_CL_PDTVN'.
  mc_rs 'ESH_TAU_CL_PN'. mc_rs 'ESH_TAU_CL_PVPD'.
  mc_rs 'ESH_TAU_UPL_IMG'. mc_rs 'ESH_TST_DATA_VAL'.
  mc_rs 'ESH_TST_HBT_SIM'. mc_rs 'ESH_TST_PROP_VAL'.
  mc_rs 'ESH_TST_VAL_NAME'. mc_rs 'EUDB'.
  mc_rs 'EUDIAL'. mc_rs 'EUF4VALUES'. mc_rs 'EUFUNC'. mc_rs 'EUINFO'.
  mc_rs 'EUINFOLI'. mc_rs 'EUP_BSP_IV_CTX'. mc_rs 'EUP_BSP_OV_CTX'.
  mc_rs 'EUP_TSP_LTXT_DE'. mc_rs 'EUP_TSP_LTXT_EN'.
  mc_rs 'EUP_TSP_REQ_CONT'. mc_rs 'FDT_ACTN_6000'.
  mc_rs 'FDT_ACTN_6000A'. mc_rs 'FDT_ACTN_6000S'.
  mc_rs 'FDT_ACTN_6000T'. mc_rs 'FDT_ADMN_0112'.
  mc_rs 'FDT_ADMN_0112S'. mc_rs 'FDT_ADMN_0112T'.
  mc_rs 'FDT_ADMN_0113'. mc_rs 'FDT_ADMN_0113A'.
  mc_rs 'FDT_ADMN_0113S'. mc_rs 'FDT_ADMN_0113T'.
  mc_rs 'FDT_ADMN_0114'. mc_rs 'FDT_ADMN_0114A'.
  mc_rs 'FDT_ADMN_0114S'. mc_rs 'FDT_ADMN_0114T'.
  mc_rs 'FDT_ADMN_0115'. mc_rs 'FDT_ADMN_0115A'.
  mc_rs 'FDT_ADMN_0115S'. mc_rs 'FDT_ADMN_0116'.
  mc_rs 'FDT_ADMN_0116A'. mc_rs 'FDT_ADMN_0116S'.
  mc_rs 'FDT_ADMN_0116T'. mc_rs 'FDT_ADMN_0117'.
  mc_rs 'FDT_ADMN_0117A'. mc_rs 'FDT_ADMN_0117S'.
  mc_rs 'FDT_ADMN_0117T'. mc_rs 'FDT_ADMN_0119'.
  mc_rs 'FDT_ADMN_0119A'. mc_rs 'FDT_ADMN_0119S'.
  mc_rs 'FDT_ADMN_0119T'. mc_rs 'FDT_DEBUG_INDX'.
  mc_rs 'FDT_ADMN_0115T'. mc_rs 'FDT_DOBJ_0400'.
  mc_rs 'FDT_DOBJ_0400A'. mc_rs 'FDT_DOBJ_0400S'.
  mc_rs 'FDT_DOBJ_0400T'. mc_rs 'FDT_EXPR_0301'.
  mc_rs 'FDT_EXPR_0301A'. mc_rs 'FDT_EXPR_0301S'.
  mc_rs 'FDT_EXPR_0301T'. mc_rs 'FDT_EXPR_4001'.
  mc_rs 'FDT_EXPR_4001A'. mc_rs 'FDT_EXPR_4001S'.
  mc_rs 'FDT_EXPR_4001T'. mc_rs 'FDT_HELPERS_TEXT'.
  mc_rs 'FDT_INTRP_0000'. mc_rs 'FDT_TEST_XML'.
  mc_rs 'FDT_TRACE_0000'. mc_rs 'FOTTDCLCON'.
  mc_rs 'FDT_TRACE_0100'. mc_rs 'FDT_WD_INFRA_001'.
  mc_rs 'FDT_XML_EXPORT'. mc_rs 'FDT_XML_IMPORT'.
  mc_rs 'FEHT_MESS_PERS'. mc_rs 'FIMA_TRACE_DATA'.
  mc_rs 'FINI'. mc_rs 'FSBP_CIF_LOG'.
  mc_rs 'FSYSCONT'. mc_rs 'GENSETC'. mc_rs 'HCSKW_DIR'.
  mc_rs 'HCSKW_TEST_DIR'. mc_rs 'HCSKW_TEST_FILE'.
  mc_rs 'HCSKW_TEST_RES'. mc_rs 'HCSKW_TEXT'. mc_rs 'HRPADNN'.
  mc_rs 'HRSCONT'.
  mc_rs 'I18NSRHRES'. mc_rs 'ICF_EXTALIAS_TF'. mc_rs 'ICF_SERVICE_TF'.
  mc_rs 'ICFALIAS'. mc_rs 'ICFAPPLCUST'. mc_rs 'ICFATTRIB'.
  mc_rs 'ICFMEMORY'. mc_rs 'ICFRECORDER'.  mc_rs 'INDTEX2'.
  mc_rs 'INDTEXT'. mc_rs 'INDX_HIER'. mc_rs 'INDX_HSRCH'.
  mc_rs 'INDXBCSET'. mc_rs 'IWP_DATA_DELIVER'. mc_rs 'IWP_SNMETA'.
  mc_rs 'IWP_WP'. mc_rs 'IWP_WP_IOBJ'. mc_rs 'IWP_WP_IPRV'.
  mc_rs 'IWP_WP_METAURI'. mc_rs 'IWP_WP_PATH'. mc_rs 'IWP_WP_RELA'.
  mc_rs 'IWP_WP_STRUC'. mc_rs 'IWP_WP_URI'. mc_rs 'J_CLU'.
  mc_rs 'KDM_CONT1'. mc_rs 'KWBCONT'. mc_rs 'LAWDIVINDX'.
  mc_rs 'LDQ_DATA'. mc_rs 'LXE_ETSTAT'. mc_rs 'LXE_NONABAP_ATTR'.
  mc_rs 'LXE_PERF_LOG'. mc_rs 'LXE_COLLSM'. mc_rs 'LXE_CONT1'.
  mc_rs 'LXE_CSN_TXL'. mc_rs 'LXE_KTP_CHOL_ERR'.
  mc_rs 'LXE_LOG_DATA_TRP'. mc_rs 'LXE_PC_AD'.
  mc_rs 'LXE_PC_INI'. mc_rs 'LXE_PC_TTX'. mc_rs 'LXE_PC_TTX_A'.
  mc_rs 'LXE_PC_TTX_B'. mc_rs 'LXE_PC_TTX_C'. mc_rs 'LXE_PC_TTX_D'.
  mc_rs 'LXE_PC_TTX_E'. mc_rs 'LXE_PC_TTX_F'. mc_rs 'LXE_PC_TTX_G'.
  mc_rs 'LXE_PC_TTX_H'. mc_rs 'LXE_PC_TTX_I'. mc_rs 'LXE_PC_TTX_L'.
  mc_rs 'LXE_PC_TTX_M'. mc_rs 'LXE_PC_TTX_N'. mc_rs 'LXE_PC_TTX_O'.
  mc_rs 'LXE_PC_TTX_P'. mc_rs 'LXE_PC_TTX_Q'. mc_rs 'LXE_PC_TTX_R'.
  mc_rs 'LXE_PC_TTX_S'. mc_rs 'LXE_PC_TTX_T'. mc_rs 'LXE_PC_TTX_U'.
  mc_rs 'LXE_PC_TTX_V'. mc_rs 'LXE_PC_TTX_W'. mc_rs 'LXE_PC_TTX_X'.
  mc_rs 'LXE_PC_TTX_Y'. mc_rs 'LXE_PC_TTX_Z'. mc_rs 'LXE_PC2_AP'.
  mc_rs 'LXE_PC2_HD'. mc_rs 'LXE_PC2_HS'. mc_rs 'LXE_PC2_KEY'.
  mc_rs 'LXE_PCZ_BI'. mc_rs 'LXE_PCZ_BI_A'. mc_rs 'LXE_PCZ_BI_F'.
  mc_rs 'LXE_PCZ_BI_H'. mc_rs 'LXE_PCZ_BI_I'. mc_rs 'LXE_PCZ_BI_S'.
  mc_rs 'LXE_PCZ_BI_Z'. mc_rs 'LXE_PCZ_TT'. mc_rs 'LXE_PCZ_TT_A'.
  mc_rs 'LXE_PCZ_TT_F'. mc_rs 'LXE_PCZ_TT_H'. mc_rs 'LXE_PCZ_TT_I'.
  mc_rs 'LXE_PCZ_TT_S'. mc_rs 'LXE_PCZ_TT_Z'. mc_rs 'LXE_PCZ_TX'.
  mc_rs 'LXE_PCZ_TX_A'. mc_rs 'LXE_PCZ_TX_F'. mc_rs 'LXE_PCZ_TX_H'.
  mc_rs 'LXE_PCZ_TX_I'. mc_rs 'LXE_PCZ_TX_S'. mc_rs 'LXE_PCZ_TX_Z'.
  mc_rs 'LXE_TDC_VERSION'. mc_rs 'LXE_TMP_ST'. mc_rs 'LXE_TTX'.
  mc_rs 'LXE_XT_OBJ_ATTR'. mc_rs 'LXE_XT_OBJ_MAP'.
  mc_rs 'LXE_XT_TKEY_MAP'. mc_rs 'LXE_XT_TXT_ATTR'.
  mc_rs 'LXE_XT_YTEXT'. mc_rs 'LXE_XT_ZBIN'. mc_rs 'LXE_XT_ZTEXT'.
  mc_rs 'LXE_PERF_LOG_AGG'. mc_rs 'LXE_UPSETS'. mc_rs 'MACID'.
  mc_rs 'MACOB'. mc_rs 'MDM_CS_FIELD_TAB'. mc_rs 'MDMGXC2'.
  mc_rs 'MIGRDDL'. mc_rs 'MIGREXTAB'. mc_rs 'MONI'.
  mc_rs 'NWECMD_CNTNT'. mc_rs 'NWECMD_CONTENT'. mc_rs 'NWECMD_NODE'.
  mc_rs 'NWECMD_PROPERTYS'. mc_rs 'NWECMD_PRPTVLS'.
  mc_rs 'NWECMD_RELATIONS'. mc_rs 'NWECMD_SESSION'.
  mc_rs 'NWECMD_VALUE'. mc_rs 'O2PAGCON'. mc_rs 'O2PAGRT'.
  mc_rs 'O2XMLDESC'. mc_rs 'O2XSLTDESC'. mc_rs 'ORA_STT_DATA'.
  mc_rs 'ORA_STT_HEAD'.  mc_rs 'PLOG'. mc_rs 'PROC_VERSIONS'.
  mc_rs 'PTRV_WAFLOG'. mc_rs 'REPOLOAD'. mc_rs 'REPOTEXT'.
  mc_rs 'RLIB_CONT'. mc_rs 'RLIB_TREES'. mc_rs 'RMPS_OCSP_HEADER'.
  mc_rs 'RMPS_OCSP_ITEM'. mc_rs 'RMPS_T_RESUB'. mc_rs 'RMPSC_TRF_XSD'.
  mc_rs 'RMPSC_TRF_XSL'. mc_rs 'RMPSD_TRF_EX_IT'.
  mc_rs 'RMPSD_TRF_IM_ID'. mc_rs 'RMPSD_TRF_IM_IT'.
  mc_rs 'RMPSPOIDCR'. mc_rs 'RMPSPRO_DP_BATCH'.
  mc_rs 'RMPSPRO_DP_REP2'. mc_rs 'RNDPGSCANRESULT'.
  mc_rs 'RODPSCLUSTER'. mc_rs 'RPDCONT01'. mc_rs 'RPDCONT02'.
  mc_rs 'RPDCONT03'. mc_rs 'RPDCONT04'. mc_rs 'RPDCONT05'.
  mc_rs 'RPDCONT06'. mc_rs 'RPRCONT09'. mc_rs 'RPSCONT01'.
  mc_rs 'RPSCONT02'. mc_rs 'RPSCONT03'. mc_rs 'RPSCONT04'.
  mc_rs 'RPSCONT05'. mc_rs 'RRT_MDX_LOG'. mc_rs 'RSAPOADM'.
  mc_rs 'RSANT_PRP_TREXF'. mc_rs 'RSANT_UT_RSLTX'. mc_rs 'RSAPCDAFSIP'.
  mc_rs 'RSAR_BUFFER'. mc_rs 'RSAUPROF'. mc_rs 'RSAUTODCTDATA'.
  mc_rs 'RSBKCMD'. mc_rs 'RSBKCMDH'. mc_rs 'RSBKDATA'.
  mc_rs 'RSBKDATAINFO'. mc_rs 'RSBKDATAPAKID'. mc_rs 'RSBKDATAPAKSEL'.
  mc_rs 'RSBMLOG'. mc_rs 'RSBMLOGPAR'. mc_rs 'RSBMNODES'.
  mc_rs 'RSBMONMESS'. mc_rs 'RSBMREQ_DTP'. mc_rs 'RSBOBJDS_FLOWS'.
  mc_rs 'RSBODSLOG'. mc_rs 'RSBODSLOGSTATE'. mc_rs 'RSC_AUTOTEST'.
  mc_rs 'RSC_FIDTXT'. mc_rs 'RSCATT_T_BI_EP'. mc_rs 'RSCATTCSV'.
  mc_rs 'RSCNVTSTDATA'. mc_rs 'RSCRTMONDATA'.
  mc_rs 'RSDCHA'. mc_rs 'RSDCHABAS'. mc_rs 'RSDCHABASLOC'.
  mc_rs 'RSDD_MU_SETH'. mc_rs 'RSDD_TIP_DSR'. mc_rs 'RSDD_TMPNM_ADM'.
  mc_rs 'RSDDBOLDVERS'.  mc_rs 'RSDDLOGITIP'. mc_rs 'RSDDSTATAGGRDEF'.
  mc_rs 'RSDDSTATDM'. mc_rs 'RSDDSTATEVDATA'.
  mc_rs 'RSDDSTATHEADER'. mc_rs 'RSDDSTATINFO'. mc_rs 'RSDDSTATTREX_MM'.
  mc_rs 'RSDFDMODDOCU'. mc_rs 'RSDFNODEATTRLONG'. mc_rs 'RSDFNODEDOCU'.
  mc_rs 'RSDIOBJESR'. mc_rs 'RSDIX'. mc_rs 'RSDM_TC'.
  mc_rs 'RSDMTCCLUSTER'. mc_rs 'RSDMTD_PMML'.
  mc_rs 'RSDRC_TC'. mc_rs 'RSDRC_TC_BACK'. mc_rs 'RSDRDTC'.
  mc_rs 'RSDRI_DS_TC'. mc_rs 'RSDRI_DS_TC_BACK'. mc_rs 'RSDRM_TC'.
  mc_rs 'RSDRT_TRACE'. mc_rs 'RSDRV_TC'. mc_rs 'RSDRV_TC_BACK'.
  mc_rs 'RSDSACCESSATTR'. mc_rs 'RSDSACCESSATTRSH'.
  mc_rs 'RSDSESRMAP'. mc_rs 'RSDSEXPORT'. mc_rs 'RSDSQOBJ'.
  mc_rs 'RSDUPARTTC'. mc_rs 'RSECLOG'.
  mc_rs 'RSECLOPDLOGC'. mc_rs 'RSECTAB'. mc_rs 'RSEVENTCHAIN'.
  mc_rs 'RSFOBUEV000'.
  mc_rs 'RSHASHDATA'. mc_rs 'RSISCPC'. mc_rs 'RSISISVD'.  mc_rs 'RSIX'.
  mc_rs 'RSIX_DATA'. mc_rs 'RSIX_MANDTINDEP'. mc_rs 'RSIXRA'.
  mc_rs 'RSL_BACKUP_D'. mc_rs 'RSLDPACCESS'.
  mc_rs 'RSLDPACCESSSH'. mc_rs 'RSLPO_DFG_DTPTPL'. mc_rs 'RSLTIP'.
  mc_rs 'RSLTIPT'. mc_rs 'RSMIGRATEPRXML'. mc_rs 'RSMIME'.
  mc_rs 'RSMONFACT'. mc_rs 'RSMONICDP'. mc_rs 'RSMONIPTAB'.
  mc_rs 'RSMONMESS'.
  mc_rs 'RSOBI_MF_MODEL'. mc_rs 'RSOBIXP'. mc_rs 'RSODCONTENT'.
  mc_rs 'RSODSO_ROLLBACK'.  mc_rs 'RSPC_BUFFER'.
  mc_rs 'RSPCINSTANCE'. mc_rs 'RSPCLOGCHAIN'. mc_rs 'RSPCLOGCROSS'.
  mc_rs 'RSPCLOGS'. mc_rs 'RSPCLOGTIMECACHE'. mc_rs 'RSPCPROCESSLOG'.
  mc_rs 'RSPLS_ENQ_MASTER'. mc_rs 'RSR_CACHE_CLU'.
  mc_rs 'RSR_CACHE_DAT_SH'.
  mc_rs 'RSR_CACHE_DATA_B'. mc_rs 'RSR_CACHE_DATA_C'.
  mc_rs 'RSR_CACHE_DB_BL'. mc_rs 'RSR_CACHE_DB_IX'.
  mc_rs 'RSR_CACHE_DBS_BL'. mc_rs 'RSR_CACHE_DBS_IX'.
  mc_rs 'RSR_CACHE_IX'. mc_rs 'RSR_CACHE_LOG'.
  mc_rs 'RSR_CACHE_STATS'.
  mc_rs 'RSR_MDX_UI_MODEL'. mc_rs 'RSR_MDX_UI_STMT'.
  mc_rs 'RSR_TRACE'. mc_rs 'RSR_TRACE_POS'. mc_rs 'RSRA_ER_LOG_IX'.
  mc_rs 'RSRA_ER_PARAMT'. mc_rs 'RSRA_IXDS'. mc_rs 'RSRA_MX_DATA'.
  mc_rs 'RSRA_MX_PARAMT'.
  mc_rs 'RSREQARCHDATA'. mc_rs 'RSREQDONE'. mc_rs 'RSREQHIER'.
  mc_rs 'RSREQICODS'. mc_rs 'RSRWBSTORE'.
  mc_rs 'RSSELDONE'. mc_rs 'RSSTATMANPART'.
  mc_rs 'RSSTATMANPARTT'. mc_rs 'RSSTATMANREQMDEL'.
  mc_rs 'RSTRAN_RTO'. mc_rs 'RSTRANCLUST'. mc_rs 'RSTSODS'.
  mc_rs 'RSTSODSPART'. mc_rs 'RSTSODSREQUEST'.
  mc_rs 'RSTSODSREQUESTPG'. mc_rs 'RSTT_CHECKSEL'.
  mc_rs 'RSTT_CHECKSTACK'. mc_rs 'RSTT_CMNT'. mc_rs 'RSTT_ERR'.
  mc_rs 'RSTT_ERROR'. mc_rs 'RSTT_ERROR_T'. mc_rs 'RSTT_INST'.
  mc_rs 'RSTT_LOGSEQ'. mc_rs 'RSTT_LOGTEMP'. mc_rs 'RSTT_MSGARC'.
  mc_rs 'RSTT_OBJHIST'. mc_rs 'RSTT_OBJTMP'. mc_rs 'RSTT_OLAPTRACE'.
  mc_rs 'RSTT_PARAM'. mc_rs 'RSTT_REFDATA'. mc_rs 'RSTT_TDATA'.
  mc_rs 'RSTT_TDATAB'. mc_rs 'RSTT_TEST'. mc_rs 'RSTT_TJOB'.
  mc_rs 'RSTT_TPACKAGE'. mc_rs 'RSTT_TPAT'. mc_rs 'RSTT_TREQ'.
  mc_rs 'RSTT_TREQ_T'. mc_rs 'RSTT_TREQSTA'. mc_rs 'RSTT_TXT'.
  mc_rs 'RSTT_USERAREA'. mc_rs 'RSTT_VARIANT'. mc_rs 'RSTT_VARIANT2'.
  mc_rs 'RSUICDONE'. mc_rs 'RSUPDSIMULD'. mc_rs 'RSWQ_DATASOURCE'.
  mc_rs 'RSXCLSDATA'. mc_rs 'RSZCOMPDIR'.
  mc_rs 'RSBAIPROV'.  mc_rs 'RSBATCHADM'.
  mc_rs 'RSBATCHCTRL'. mc_rs 'RSBATCHCTRL_PAR'. mc_rs 'RSBATCHDATA'.
  mc_rs 'RSBATCHDEBUG'. mc_rs 'RSBATCHDEBUGTYPE'.
  mc_rs 'RSBATCHDEBUGVAL'. mc_rs 'RSBATCHDEBUGWAIT'.
  mc_rs 'RSBATCHDELDATA'. mc_rs 'RSBATCHENQ'. mc_rs 'RSBATCHHEADER'.
  mc_rs 'RSBATCHLOGSTAT'. mc_rs 'RSBATCHLOGWRITE'.
  mc_rs 'RSBATCHPARALLEL'. mc_rs 'RSBATCHPROT'.
  mc_rs 'RSBATCHSERVER'. mc_rs 'RSBATCHSTACK'. mc_rs 'RSBATCHWAITVAL'.
  mc_rs 'RSBBSCUBE'. mc_rs 'RSBBSIX'.
  mc_rs 'RSBBSQUERY'. mc_rs 'RSCDSSHMCLUSTTAB'. mc_rs 'RSCDSTC'.
  mc_rs 'RSCNVACTDATA'. mc_rs 'RSCRMVIEWSTORE'.
  mc_rs 'RSCRT_RDA_MONDAT'.mc_rs 'RSDAARCHREQ'. mc_rs 'RSDADAPPART'.
  mc_rs 'RSDANLREQ'. mc_rs 'RSDAREFTAB'. mc_rs 'RSDAREFTABSDB_OR'.
  mc_rs 'RSDCHABASDELLOG'. mc_rs 'RSDDCOPR'. mc_rs 'RSDDCOPRT'.
  mc_rs 'RSDDICREPARTREQ'.mc_rs 'RSDDSTATBIAUSE'.
  mc_rs 'RSDMTC_PMML'. mc_rs 'RSDMTPN_COL_PARA'.
  mc_rs 'RSDMTPN_COL_VAL'. mc_rs 'RSDMTPN_MOD_PARA'.
  mc_rs 'RSDMTPN_MOD_XML'. mc_rs 'RSDMTPN_OBJ_XML'.
  mc_rs 'RSDMTPN_SRV_PAR'. mc_rs 'RSDRBATCHPARA'.
  mc_rs 'RSDRCRM_SEG_TEST'. mc_rs 'RSDRHLRUBUFFER'. mc_rs 'RSDRI_TC'.
  mc_rs 'RSDRI_TC_BACK'.mc_rs 'RSDRS_TC'.  mc_rs 'RSDRS_TC_BACK'.
  mc_rs 'RSDS_QUEUE_DATA'. mc_rs 'RSECACTB'. mc_rs 'RSER_ELEM_DATA'.
  mc_rs 'RSERDATA'. mc_rs 'RSIM_HC_INCIDENT'. mc_rs 'RSINPCONFIG'.
  mc_rs 'RSISISVFA'. mc_rs 'RSIXWWW'. mc_rs 'RSNORMDB'.
  mc_rs 'RSO_MMR_MIMES'. mc_rs 'RSODPLOAD'. mc_rs 'RSODPLOADSEL'.
  mc_rs 'RSODPSEL'. mc_rs 'RSODPSELHASH'. mc_rs 'RSODSACTDATA'.
  mc_rs 'RSODSMPPSQLLOG'. mc_rs 'RSODSO_RUNTIME'. mc_rs 'RSOHTML'.
  mc_rs 'RSOTLOGOHISTORY'. mc_rs 'RSOXMLIMPCONT'.
  mc_rs 'RSPLS_RECOVERY'. mc_rs 'RSPLS_REPCAT'. mc_rs 'RSQONCOND'.
  mc_rs 'RSQRDB'.
  mc_rs 'RSQWCOND'. mc_rs 'RSR_CACHE_FFB'. mc_rs 'RSR_CACHE_VARSHB'.
  mc_rs 'RSRA_BB_PARAMT'. mc_rs 'RSRA_BT_PARAMT'.
  mc_rs 'RSRD_ER_DOCLAY'. mc_rs 'RSRD_HASHSTRING'.
  mc_rs 'RSRD_PERS_STRING'. mc_rs 'RSRDE_DOC_ATTR'.
  mc_rs 'RSRDE_DOC_CONT'.
  mc_rs 'RSRPARAMETRIZA'. mc_rs 'RSRVAPPLLOGIX'.
  mc_rs 'RSSBAUTVAL'. mc_rs 'RSSDKGENMETA'.  mc_rs 'RSSEM_RFCPACK'.
  mc_rs 'RSTSODSFIELD'.
  mc_rs 'RSTTIX'.  mc_rs 'RSWR_DATA'. mc_rs 'RSWSPT'.
  mc_rs 'RSZWBITMDATA'.
  mc_rs 'RSZWBTMPDATA'. mc_rs 'RSZWITEMDATA'. mc_rs 'RSZWMDITEMDATA'.
  mc_rs 'RSZWMDITEMDOCU'. mc_rs 'RSZWOBJ'. mc_rs 'RSZWREPITEMSTORE'.
  mc_rs 'RSZWTEMPLATEDATA'. mc_rs 'SABP_LISTTR_M'.
  mc_rs 'SABP_LISTTR_M_LI'. mc_rs 'SALV_CSQ_TR_DATA'.
  mc_rs 'SALV_CSQ_TR_HEAD'. mc_rs 'SALV_CSQ_TR_REC'.
  mc_rs 'SALV_CSQ_TR_TYPE'. mc_rs 'SALV_EX_STORAGE'.
  mc_rs 'SAPHC_NOTES_APP'.
  mc_rs 'SATC_AC_USER'.  mc_rs 'SAUNIT_CM_BUFFER'.
  mc_rs 'SC2_AP_IDE_P_CON'. mc_rs 'SC2_IDE_PROT_CON'.
  mc_rs 'SCC_ATL_USER'. mc_rs 'SCCD_ASP_PARAM'.
  mc_rs 'SCDTSYSRFC_HTTP'. mc_rs 'SCG_CODE_TEMPL'.
  mc_rs 'SCGCLASSIFICATON'. mc_rs 'SCGDETAIL'.
  mc_rs 'SCGR_CLASS'. mc_rs 'SCI_USERS'. mc_rs 'SCICHKV_PA'.
  mc_rs 'SCIINS_ADD'. mc_rs 'SCIOBJ_SEL'. mc_rs 'SCIREST_STR'.
  mc_rs 'SCOL_ND_ASSOC'. mc_rs 'SCOL_ND_ASSOCMAP' .mc_rs 'SCOL_TRACE'.
  mc_rs 'SCOL_TYPEMANAGER'. mc_rs 'SCOL_WB_DATASET'.
  mc_rs 'SCOOLOBJECTS'. mc_rs 'SCPRTEMP'.
  mc_rs 'SCTC_TESTTABL_3'. mc_rs 'SCWB_INDX'. mc_rs 'SCWBINDXVS'.
  mc_rs 'SDB_CLSYSTEM_VAL'. mc_rs 'SDOK_Q_TRACE_DTL'.
  mc_rs 'SDOKCONT1'. mc_rs 'SDOKCPAGEHD'. mc_rs 'SDOKCPAGEHDC'.
  mc_rs 'SDOKMSHBUF'.
  mc_rs 'SDOKPHCBUF'. mc_rs 'SDOKPHVBUF'. mc_rs 'SDOKRELOC'.
  mc_rs 'SDOLCONT1'. mc_rs 'SDTSHD'. mc_rs 'SEC_TEST_MAP'.
  mc_rs 'SEOCOMPODF'. mc_rs 'SEOCOMPOSRC'.
  mc_rs 'SEPM_TEST_CON'. mc_rs 'SEPM_TEST_MAN'. mc_rs 'SEPM_TEST_RES'.
  mc_rs 'SERPLISTS'.  mc_rs 'SFDG'.
  mc_rs 'SFSG_DEF_RT'. mc_rs 'SFSG_Q_COND'. mc_rs 'SFSG_Q_GROUP'.
  mc_rs 'SFSG_Q_JOIN'. mc_rs 'SFSG_Q_RESULT'. mc_rs 'SFSG_Q_TABLE'.
  mc_rs 'SFSG_QR_INPUT'. mc_rs 'SFSG_QR_RESULT'.
  mc_rs 'SFSRFW_ALIAS_DIR'. mc_rs 'SFSRFW_ENGINE_CS'.
  mc_rs 'SFSRFW_JDX_JP'. mc_rs 'SFSRFW_LOAD_HDL'.
  mc_rs 'SFSRFW_REQ_JPATH'.  mc_rs 'SHDIRSCRD'. mc_rs 'SIC_SIM_DATA'.
  mc_rs 'SIDL_TEST_DATA'. mc_rs 'SIGNDO'. mc_rs 'SIGNS'.
  mc_rs 'SIXMLCE'. mc_rs 'SIXMLTST'.
  mc_rs 'SKWG_DOCS'. mc_rs 'SKWS_CACHE'. mc_rs 'SLCM_EDIS_RUN'.
  mc_rs 'SLINCACHE'. mc_rs 'SLS_CEVENT'.
  mc_rs 'SLS_CPET'. mc_rs 'SLS_CRITEM'. mc_rs 'SLS_CXET'.
  mc_rs 'SLS_LOGL'. mc_rs 'SLS_RESS3'. mc_rs 'SLS_TEXT'.
  mc_rs 'SLS_YESS3'. mc_rs 'SMAICONT1'. mc_rs 'SMASCONT1'.
  mc_rs 'SMET01'. mc_rs 'SCMGCONT01'.
  mc_rs 'SCMGPOIDCR'. mc_rs 'SCMGPOIDCR_CLNT'.
  mc_rs 'SMIFS_LOOKUP'. mc_rs 'SMIMCONT1'.
  mc_rs 'SNAP'. mc_rs 'SNCSYSACL'.
  mc_rs 'SPROX_TEST_DATA'. mc_rs 'SPROXINDX'. mc_rs 'SPROXSETX'.
  mc_rs 'SPROXTST'. mc_rs 'SPROXWSDL'. mc_rs 'SPROXXSL'.
  mc_rs 'SPROXXSLT'. mc_rs 'SPROXXSLV'.
  mc_rs 'SPROXCLASS'. mc_rs 'SPROXDAT'. mc_rs 'SPROXHDR'.
  mc_rs 'SPROXLPT'. mc_rs 'SPROXSVARDAT'.
  mc_rs 'SQLT_INDX'. mc_rs 'SQTEXT_DB4'. mc_rs 'SRAPI_CONFIG'.
  mc_rs 'SRAPI_MAP_OBJ'. mc_rs 'SRAPI_PIC_OBJ'. mc_rs 'SRM_ECM_HASH'.
  mc_rs 'SRMBSPPREF'. mc_rs 'SRMCONT_MN'.
  mc_rs 'SRMCONT_MT'. mc_rs 'SRMCONT_UN'. mc_rs 'SRMCONT_UT'.
  mc_rs 'SRMGSPROTO'. mc_rs 'SRMHLELEMD'. mc_rs 'SRMHLELEML'.
  mc_rs 'SRMHLELEMS'. mc_rs 'SRMHLTYPES'. mc_rs 'SRMORGHIS'.
  mc_rs 'SRMORGRES'. mc_rs 'SRMPROTOCOL'. mc_rs 'SRMRGCONTC'.
  mc_rs 'SRMUSVALUE'. mc_rs 'SRT_AGS_E2E_TRC'. mc_rs 'SRT_CFG_VALUES'.
  mc_rs 'SRT_EVENT_OP'. mc_rs 'SRT_IDP_BD'.mc_rs 'SRT_LTRC_DETAIL'.
  mc_rs 'SRT_SEQ_MSG_USR'. mc_rs 'SRT_SEQ_TST_DT'.
  mc_rs 'SRT_TEST_CASE'. mc_rs 'SRT_TEST_EVENT'.
  mc_rs 'SRT_TEST_PING'.
  mc_rs 'SRT_TEST_RESULTS'. mc_rs 'SRT_TF_LP'.
  mc_rs 'SRT_TF_TST_DATA'. mc_rs 'SRT_UTIL_ERRLOG'.
  mc_rs 'SRT_UTIL_LOG'.
  mc_rs 'SRT_UTIL_PERF'. mc_rs 'SRT_UTIL_TRCFUNC'.
  mc_rs 'SRT_UTIL_TRCPERF'. mc_rs 'SRT_UTIL_TRCPLOA'.
  mc_rs 'SRTFT_ASSIGN_IF'. mc_rs 'SRTM_DATAX'.
  mc_rs 'SRT_ASSIGN_DATA'.
  mc_rs 'SRT_CFG_BNDG'. mc_rs 'SRT_CFG_CFGVAL'.
  mc_rs 'SRT_CFG_CLI_ASGN'. mc_rs 'SRT_CFG_DATA'.
  mc_rs 'SRT_CFG_ICF_DET'.  mc_rs 'SRT_CFG_ICF_USE'.
  mc_rs 'SRT_CFG_SRV_ASGN'. mc_rs 'SRT_CFG_STS_ASGN'.
  mc_rs 'SRT_CFG_TRC'. mc_rs 'SRT_CFG_VAL_DET'.
  mc_rs 'SRT_CFG_VAL_DIR'. mc_rs 'SRT_CG_WSDL_CONT'.
  mc_rs 'SRT_CG_WSDL_URL'. mc_rs 'SRT_DEST_CFGV'.
  mc_rs 'SRT_DEST_DATA'. mc_rs 'SRT_DT_CFG_DATA'.
  mc_rs 'SRT_DT_CHG_STAT'. mc_rs 'SRT_DT_CHG_STATL'.
  mc_rs 'SRT_GEN_CLNT_DIR'. mc_rs 'SRT_LP_FEAT'.
  mc_rs 'SRT_LP_OP_FEAT'. mc_rs 'SRT_LP_URL_ASGN'.
  mc_rs 'SRT_MIG_HIST'. mc_rs 'SRT_MONI_NAVI'.
  mc_rs 'SRT_MONI_NAVI2'. mc_rs 'SRT_MONI_NAVIL'.
  mc_rs 'SRT_MONI_NAVIL2'. mc_rs 'SRT_MONI_NAVIS'.
  mc_rs 'SRT_MONI_NAVIS2'. mc_rs 'SRT_MONI_PAYLTRC'.
  mc_rs 'SRT_MONILOG_DATA'.  mc_rs 'SRT_MONLOG_DATA2'.
  mc_rs 'SRT_NAME_CACHE'. mc_rs 'SRT_OBSERV_DATA'.
  mc_rs 'SRT_PEND_TASK'. mc_rs 'SRT_PRF_CFGVAL'. mc_rs 'SRT_PRF_DATA'.
  mc_rs 'SRT_PRO_WKL'. mc_rs 'SRT_PROF_ASSIGN'. mc_rs 'SRT_PT_INDEX'.
  mc_rs 'SRT_PX_MSG_ASGN'. mc_rs 'SRT_REG_BIND'.
  mc_rs 'SRT_REG_CONFIG'.
  mc_rs 'SRT_RTC_CFGVAL'. mc_rs 'SRT_RTC_CG_DATAL'.
  mc_rs 'SRT_RTC_DATA_RT'. mc_rs 'SRT_RTC_DATAL'.
  mc_rs 'SRT_RTC_WSILL'. mc_rs 'SRT_SA_APPL'. mc_rs 'SRT_SA_APPL_XI'.
  mc_rs 'SRT_SA_PRF'. mc_rs 'SRT_SA_PRF_XI'. mc_rs 'SRT_SEQ_CX_INFO'.
  mc_rs 'SRT_SYST_APPL'. mc_rs 'SRT_SYST_APPL_HT'.
  mc_rs 'SRT_SYST_CL_DATA'. mc_rs 'SRT_TMP_CG_DATAL'.
  mc_rs 'SRT_WSDL_CACHE_P'. mc_rs 'SRT_WSIL_CACHE_P'.
  mc_rs 'SRT_WSIL_TMP'.
  mc_rs 'SRTM_LOG'. mc_rs 'STRUSTCAB'. mc_rs 'STRUSTCERT'.
  mc_rs 'STRX_LIXS'. mc_rs 'STXFCONT'. mc_rs 'STXFCONTR'.
  mc_rs 'STXFCONTS'. mc_rs 'STXFCONTV'. mc_rs 'STXTRACE'.
  mc_rs 'SVMCRT_BINDINGS'.
  mc_rs 'SVMCRT_CONF_NVLB'. mc_rs 'SVMCRT_CONF_NVLS'.
  mc_rs 'SVMCRT_MODULES'. mc_rs 'SVMCRT_RES_TRANS'.
  mc_rs 'SVMCRT_RESOURCES'. mc_rs 'SWD_CONT'. mc_rs 'SWD_PROPTS'.
  mc_rs 'SWDSCONT'. mc_rs 'SWDSPROPTS'. mc_rs 'SWF_BAM_TRC_DAT'.
  mc_rs 'SWF_MLOBJ'.
  mc_rs 'SWF_TRC_CONT'. mc_rs 'SWF_TRC_DEMO_DAT'.
  mc_rs 'SWF_TRC_HEAD'.
  mc_rs 'SWF_TRC_SELOPT'. mc_rs 'SWF_TRC_WF_DAT'. mc_rs 'SWFDCRLHCN'.
  mc_rs 'SWFGPDEPLOY'. mc_rs 'SWFMIG_INST_DATA'. mc_rs 'SWFRCNTXML'.
  mc_rs 'SWFRCNTXMLS'. mc_rs 'SWFRCRLINST'. mc_rs 'SWFREVTCNT'.
  mc_rs 'SWFWSPIMPL'.  mc_rs 'SWFWSPINTF'. mc_rs 'SWFWSPPORT'.
  mc_rs 'SWFXCNTXML'. mc_rs 'SWJ_CONT'. mc_rs 'SWJPCNT'.
  mc_rs 'SWR_BPML'.mc_rs 'SWFSLSCNT'. mc_rs 'SWFSLSINDX'.
  mc_rs 'SWTRQTRACE'. mc_rs 'SWU_CONT1'.
  mc_rs 'SWUOCONT1'. mc_rs 'SWWINDX'. mc_rs 'SWXML_CONT'.
  mc_rs 'TABADRX'. mc_rs 'TADWP001'. mc_rs 'TADWP002'.
  mc_rs 'TADWP003'. mc_rs 'TADWP004'. mc_rs 'TADWP005'.
  mc_rs 'TADWP006'. mc_rs 'TADWP007'. mc_rs 'TADWP008'.
  mc_rs 'TADWP009'. mc_rs 'TADWP00A'. mc_rs 'TADWP00B'.
  mc_rs 'TARCHENGINEVRS01'. mc_rs 'TARCHUSERSETTING'.
  mc_rs 'TBTCY'. mc_rs 'TCPCHECK'. mc_rs 'TECHED02_UNICODE'.
  mc_rs 'TECHED03_RESULT2'. mc_rs 'TECHED03_UTEXT'.
  mc_rs 'TFAWC'. mc_rs 'TFAWL'. mc_rs 'TFO11'. mc_rs 'TLDA_LOG'.
  mc_rs 'TPRI_DEF'.  mc_rs 'TSIW_IMPL_PIECE'. mc_rs 'TSIW_TESTDATA'.
  mc_rs 'TVARIND'. mc_rs 'TWBMAN_RECO_FAV'. mc_rs 'TXO_GEN_DB'.
  mc_rs 'UMG_TEST_12'. mc_rs 'UMG_TEST_INDX'. mc_rs 'UMG_TEST_INDX1'.
  mc_rs 'UMGCHKP'. mc_rs 'UMGCONDITION'.
  mc_rs 'UMGHISTORY'. mc_rs 'UMGINDX'. mc_rs 'UMGINFO'.
  mc_rs 'UMGINITCLUST'. mc_rs 'UMGPMBAT'. mc_rs 'UMGPMCNV'.
  mc_rs 'UMGPMST'. mc_rs 'UMGR3LLOG'. mc_rs 'UMGSETT'.
  mc_rs 'UMGTESTCASE'. mc_rs 'UMGTWININDX'. mc_rs 'UMGXMLFILES'.
  mc_rs 'UMG_TEST_C'. mc_rs 'UMG_TEST_F'. mc_rs 'UMG_TEST_P'.
  mc_rs 'UMG_TEST_S'. mc_rs 'UPB_PM_POFO_XML'. mc_rs 'UPC_GREPCAT'.
  mc_rs 'UPC_STATISTIC2'.
  mc_rs 'UPC_TRACE_DATA'. mc_rs 'UPS_HASH'. mc_rs 'UPS_LINKS'.
  mc_rs 'UPWB_DATA'. mc_rs 'UPX_CLUSTER'. mc_rs 'USARC_CD'.
  mc_rs 'USERINFO_STORAGE'. mc_rs 'USH04_ARC_TMP'.
  mc_rs 'USH10_ARC_TMP'. mc_rs 'USH12_ARC_TMP'. mc_rs 'USOBHASH'.
  mc_rs 'USR04'. mc_rs 'USR10'. mc_rs 'USR12'. mc_rs 'USR16'.
  mc_rs 'USRACL'. mc_rs 'USRACLEXT'. mc_rs 'USRATTR'. mc_rs 'USRBF'.
  mc_rs 'USRCERTMAP'. mc_rs 'USRCERTRULE'. mc_rs 'USREXTIDH'.
  mc_rs 'VARI'. mc_rs 'VEPENDPOINT'. mc_rs 'VER05155_TEST'.
  mc_rs 'VORINDX'.
  mc_rs 'VRS_CMP_RESULT'. mc_rs 'VRSMODISRC'. mc_rs 'VRSSC_ATTR'.
  mc_rs 'VRSSC_TAB_RESULT'. mc_rs 'VRSTC_CONFIGS'.
  mc_rs 'VRSTC_RESULTS'. mc_rs 'VRSX'. mc_rs 'VRSX2'.
  mc_rs 'VRSX3'. mc_rs 'VRSX4'. mc_rs 'WDG_GEN_LOG'.
  mc_rs 'WDG_UR_CONTROL'. mc_rs 'WDG_UR_ENUM'.
  mc_rs 'WDG_UR_ENUM_VAL'.
  mc_rs 'WDG_UR_EVENT'. mc_rs 'WDG_UR_EVENT_PAR'.
  mc_rs 'WDG_UR_PROPERTY'. mc_rs 'WDG_UR_WEB_ICONT'.
  mc_rs 'WDKACTGRPSTATS'.
  mc_rs 'WDR_ACF_WL_CERT'. mc_rs 'WDR_ADP_AGGR_MP'.
  mc_rs 'WDR_ADP_CHNGLOG'. mc_rs 'WDR_ADP_CONST_MP'.
  mc_rs 'WDR_ADP_UR_EVENT'. mc_rs 'WDR_ADP_UR_EVPAR'.
  mc_rs 'WDR_RR_LOAD'. mc_rs 'WDR_TEST_UIELEM'. mc_rs 'WDRCSFCHECK'.
  mc_rs 'WDT_ECATT_TRCS'.
  mc_rs 'WDT_TRCS_RAW'. mc_rs 'WDY_APP_PROP_DEF'.
  mc_rs 'WDY_CONF_APPLU'. mc_rs 'WDY_CONF_USER'.
  mc_rs 'WDY_CONFIG_APPL'. mc_rs 'WDY_CONFIG_DATA'.
  mc_rs 'WDY_SETTINGS'. mc_rs 'WDY_UI_LIBRARY'.
  mc_rs 'WDY_UI_PROP_DEF'. mc_rs 'WSI_OCI_BGSEARCH'.
  mc_rs 'WDKSNAPSHOTS'. mc_rs 'WDRDEMOIFBAEMAIL'.
  mc_rs 'WDY_APP_PROPERTY'. mc_rs 'WDY_APPLICATION'.
  mc_rs 'WDY_CTLR_COMPO'. mc_rs 'WDY_VD_GUIDDELTA'.
  mc_rs 'WDY_VD_HTML'.
  mc_rs 'WDY_VIEW'. mc_rs 'WDY_VS_PROP_DEF'.  mc_rs 'WDY_WB_C_TEMPL'.
  mc_rs 'WDY_WB_SOURCEMAP'.
  mc_rs 'WSRM_CREA_SID'. mc_rs 'WSRM_CSR'. mc_rs 'WSRM_ERROR'.
  mc_rs 'WSRM_IO_PROCESS'. mc_rs 'WSRM_SEQU_STATUS'.
  mc_rs 'WSRMB_ID_MAPPING'. mc_rs 'WSS_PROFILE'.
  mc_rs 'WSS_TEMPLATE'. mc_rs 'WWWDATA'.
  " ENDIF.
  SORT gt_row_tab.
  DELETE ADJACENT DUPLICATES FROM gt_row_tab.
ENDFORM.                    "fill_row_list

*&---------------------------------------------------------------------*
*&      Form  check_install
*&---------------------------------------------------------------------*
*      Check the installation if Z version is correctly installed.
*----------------------------------------------------------------------*

FORM check_install.

  DATA:
    l_remoteflag  TYPE rs38l-remote,
    lt_exeplist   TYPE TABLE OF rsexc,
    lt_import     TYPE TABLE OF rsimp,
    lt_changing   TYPE TABLE OF rscha,
    lt_doku       TYPE TABLE OF funct,
    lt_export     TYPE TABLE OF rsexp,
    lt_tabparam   TYPE TABLE OF rstbl,
    lines         TYPE i,
    l_import_ko   TYPE boolean,
    l_changing_ko TYPE boolean,
    l_fixpt       TYPE progdir-fixpt.

  DEFINE mc_parchk.
    READ TABLE lt_import TRANSPORTING NO FIELDS
          WITH KEY parameter = &1
                   typ       = &2
                   optional   = abap_true.
    IF sy-subrc <> 0.
      l_import_ko = abap_true.
    ENDIF.
  END-OF-DEFINITION.

  SELECT SINGLE fixpt FROM progdir INTO l_fixpt
         WHERE name = sy-repid
  AND state = 'A'.

  IF sy-subrc <> 0.
    MESSAGE e090(sada) WITH
     'Report ZNEWHDBSIZE is not installed properly.'        "#EC NOTEXT
     'No entry found in REPOSRC'.                           "#EC NOTEXT
  ENDIF.

  IF l_fixpt IS INITIAL.
    MESSAGE e090(sada) WITH
     'The Fixed point arithmetic flag must not'             "#EC NOTEXT
     'be unticked.'                                         "#EC NOTEXT
     'Tick the flag and execute the report once again'.     "#EC NOTEXT
  ENDIF.
  CALL FUNCTION 'FUNCTION_IMPORT_DOKU'
    EXPORTING
      funcname           = funcname
    IMPORTING
      remote_call        = l_remoteflag
    TABLES
      dokumentation      = lt_doku
      export_parameter   = lt_export
      exception_list     = lt_exeplist
      import_parameter   = lt_import
      changing_parameter = lt_changing
      tables_parameter   = lt_tabparam
    EXCEPTIONS
      error_message      = 1
      function_not_found = 2
      invalid_name       = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE e090(sada) WITH
     'Function module Z_COLLECT_STATS was not found.'       "#EC NOTEXT
     'Check the installation instructions in SAP'           "#EC NOTEXT
     'Note 1872170.'.                                       "#EC NOTEXT
  ENDIF.
  IF l_remoteflag IS INITIAL.
    MESSAGE e090(sada) WITH
    'The remote flag of Z_COLLECT_STATS is not set'         "#EC NOTEXT
     ' Check the installation instructions in SAP'          "#EC NOTEXT
     ' Note 1872170.'.                                      "#EC NOTEXT
  ENDIF.
  IF lt_export IS NOT INITIAL.
    MESSAGE e090(sada) WITH
    'Function module Z_COLLECT_STATS has incorrect'         "#EC NOTEXT
    'export parameters. Check the installation '            "#EC NOTEXT
    'instructions in SAP Note 1872170.'.                    "#EC NOTEXT
  ENDIF.

  DESCRIBE TABLE lt_import LINES lines.
  IF lines <> 13.
    l_import_ko = abap_true.
  ENDIF.

  mc_parchk 'PRECISION' 'DTPRECINT'.
  mc_parchk 'MODE' 'STRING'.
  mc_parchk 'LOAD' 'BOOLEAN'.
  mc_parchk 'HL' 'BOOLEAN'.
  mc_parchk 'MAC' 'BOOLEAN'.
  mc_parchk 'CLUSTPREC' 'DTPRECINT'.
  mc_parchk 'MSHL' 'BOOLEAN'.
  mc_parchk 'DAAG' 'NUMC3'.
  mc_parchk 'FORCE_COUNT' 'BOOLEAN'.
  mc_parchk 'STAT_AGE' 'INT4'.
  mc_parchk 'MIN_REC_CNT' 'INT4'.
  mc_parchk 'HDB_EMPTY_TABLES' 'BOOLEAN'.
  mc_parchk 'LRAW' 'INT4'.

  IF l_import_ko = abap_true.
    MESSAGE e090(sada) WITH
    'Function module Z_COLLECT_STATS has incorrect'         "#EC NOTEXT
    'import parameters. Check the installation '            "#EC NOTEXT
    'instructions in SAP Note 1872170.'.                    "#EC NOTEXT
  ENDIF.
  DESCRIBE TABLE lt_changing LINES lines.
  IF lines <> 1.
    l_changing_ko = abap_true.
  ENDIF.
  READ TABLE lt_changing TRANSPORTING NO FIELDS
          WITH KEY parameter = 'DATA'
                   typ       = 'XSTRING'.
  IF sy-subrc <> 0.
    l_changing_ko = abap_true.
  ENDIF.
  IF l_changing_ko = abap_true.
    MESSAGE e090(sada) WITH
    'Function module Z_COLLECT_STATS has incorrect'         "#EC NOTEXT
    'changing parameters. Check the installation '          "#EC NOTEXT
    'instructions in SAP Note 1872170.'.                    "#EC NOTEXT
  ENDIF.
  IF lt_tabparam IS NOT INITIAL.
    MESSAGE e090(sada) WITH
    'Function module Z_COLLECT_STATS has incorrect'         "#EC NOTEXT
    'table parameters. Check the installation '             "#EC NOTEXT
    'instructions in SAP Note 1872170.'.                    "#EC NOTEXT
  ENDIF.
ENDFORM.                    " check_install

*&---------------------------------------------------------------------*
*&      Form  output_on_screen
*&---------------------------------------------------------------------*
*       Create output spool
*----------------------------------------------------------------------*

FORM output_on_screen USING outmode
                            "totals
                            l_cs_total l_rs_total l_hl l_hlch
                            l_disk l_init_data l_init_ws l_init_size
                            l_twocs
                            "Additional components
                            l_indx l_lc l_s4_tr
                            "Simple Finance
                            l_sfincold l_coldsfinch l_sfintab l_sfincol
                            l_sfinnew l_fin_unl l_obs_fin
                            l_pca l_delcoep l_sl
                            "Simple Logistics
                            l_slogtab l_slogcol
                            l_log_chgvbfa l_log_oldvbfa l_log_newvbfa
l_slognew
                            l_log_unl l_log_les l_obs_log
                            l_rs2cs
                            "Material ledger actual costing
                            l_mlnew l_obs_ml
                            "aging
                            l_agedch l_aged l_ageddisk
                            l_elaging l_elarch
                            "S/4 optimization
                            l_opt_data l_opt_ws l_optdisk l_opt_size
                            l_opt_twocs
                            "persistent memory
                            l_init_pers l_init_dram
                            l_opt_pers l_opt_dram
                            "details
                            l_cs_col l_cs_mlob l_cs_k   l_cs_pk
                            l_cs_hcard l_cs_mcard
                            l_cs_lcard l_cs_ucard
                            l_cs_rid l_cs_udiv l_cs_uk  l_cs_nuk
                            l_rs_col l_rs_mlob l_rs_pk  l_rs_sk
                            l_cshl   l_rshl
                            l_cs_jec l_cs_aging l_cs_ft
                            "in-place upgrade
                            l_updisk l_updata l_upchrec l_uptotal
                            l_shadisk l_sha l_shatotal
                            "technical details
                            l_wa_techd TYPE ty_techd
                            "business data
                            l_cl_lis
                            "errors
                            l_error_ratio.

  DATA: sizefield     TYPE fieldname,
        hlfield       TYPE fieldname,
        pkfield       TYPE fieldname,
        agefield      TYPE fieldname,
        arobjcnt      TYPE i,
        arobjcnt_c(2) TYPE c.

  FIELD-SYMBOLS:
    <sizefs>  TYPE ty_hana_db_size-est_size,
    <hlfs>    TYPE ty_hana_db_size-est_hl_size,
    <cpu_per> TYPE ty_cpu_per.

  MESSAGE s090(sada) WITH 'Creating spool output'.          "#EC NOTEXT
  "TOP TABLE
  WRITE /. PERFORM add_line.
  mc_sp_t 'SIZING RESULTS IN GB'.                           "#EC NOTEXT
  PERFORM add_line.
  CONCATENATE 'Based on the selected table(s), '            "#EC NOTEXT
            'the anticipated maximum requirements are'      "#EC NOTEXT
               INTO outln SEPARATED BY space.
  mc_sp_t outln.
  WRITE: / '|', AT 80 '|'.
  IF l_wa_techd-do_soh = abap_true.
    IF l_wa_techd-db_vendor(3) = 'HDB'.
      mc_sp_t 'for the analysed HANA system:'.              "#EC NOTEXT
    ELSE.
      mc_sp_t 'for Business Suite on HANA:'.                "#EC NOTEXT
    ENDIF.
  ELSEIF l_wa_techd-do_sfin = abap_true
     AND l_wa_techd-do_slog = abap_false.
    mc_sp_t 'for S/4HANA Finance:'.                         "#EC NOTEXT
  ELSEIF l_wa_techd-do_slog = abap_true.
    mc_sp_t 'for S/4HANA:'.                                 "#EC NOTEXT
  ENDIF.
  IF l_wa_techd-do_soh = abap_true.
    mc_spn '- Memory requirement' l_init_size.              "#EC NOTEXT
  ELSE.
    mc_spn '- Memory requirement for the initial installation'
                                 l_init_size.               "#EC NOTEXT
  ENDIF.
  IF l_wa_techd-do_soh = abap_true.
    mc_spn '- Net data size on disk' l_disk.                "#EC NOTEXT
  ELSE.
    mc_spn '- Net data size on disk for the initial installation'
                                    l_disk.                 "#EC NOTEXT
  ENDIF.
  mc_sp '- Estimated SAPS category of the database server'
                                    wa_cpu_x-est_sapsts.    "#EC NOTEXT

  IF l_opt_size IS NOT INITIAL AND l_opt_size < l_init_size.
    WRITE: / '|', AT 80 '|'.
    mc_spn '- Estimated memory requirement after data clean-up'
                                   l_opt_size.              "#EC NOTEXT
    mc_spn '- Estimated net data size on disk after data clean-up'
                                   l_optdisk.               "#EC NOTEXT
  ENDIF.

  IF l_cl_lis IS NOT INITIAL.
    WRITE: / '|', AT 80 '|'.
    mc_sp_t 'for S/4HANA Cloud:'.                           "#EC NOTEXT
    mc_spn '- Estimated memory size of business data'       "#EC NOTEXT
                  l_cl_lis.                                 "#EC NOTEXT
  ENDIF.

  IF l_shatotal > 0 AND p_ful = abap_true.
    WRITE: / '|', AT 80 '|'.
    mc_sp_t 'for an upgrade shadow instance:'.              "#EC NOTEXT
    mc_spn '- Estimated memory requirement' l_shatotal.     "#EC NOTEXT
    mc_spn '- Estimated disk requirement' l_shadisk.        "#EC NOTEXT
  ENDIF.
  IF l_uptotal > 0 AND p_ful = abap_true.
    WRITE: / '|', AT 80 '|'.
    mc_sp_t 'for sMIG conversion to S/4HANA:'.              "#EC NOTEXT
    mc_spn '- Estimated total additional memory requirement '
                                                 l_uptotal. "#EC NOTEXT
    mc_spn '- Estimated total additional disk requirement'
                                                 l_updisk.  "#EC NOTEXT
  ENDIF.

  IF l_indx IS NOT INITIAL OR l_lc IS NOT INITIAL
                           OR l_s4_tr IS NOT INITIAL
                           OR l_shatotal IS NOT INITIAL.
    WRITE: / '|', AT 80 '|'.
    mc_sp_t 'Other possible additional memory requirement:'. "#EC NOTEXT
    IF l_indx IS NOT INITIAL.
      mc_spn '- for de-clustering of PCLx tables as per Note 1774918'
          l_indx.                                           "#EC NOTEXT
    ENDIF.
    IF l_lc IS NOT INITIAL.
      mc_spn '- for the Live Cache' l_lc.                   "#EC NOTEXT
    ENDIF.
    IF l_s4_tr IS NOT INITIAL.
      mc_spn '- during the transition to S/4HANA (See FAQ)'
                                                  l_s4_tr.  "#EC NOTEXT
    ENDIF.
    IF l_shatotal IS NOT INITIAL.
      mc_spn '- for an upgrade shadow instance'  l_shatotal. "#EC NOTEXT
    ENDIF.
  ENDIF.
  PERFORM add_line.

  "GENERAL INFORMATION.
  CONCATENATE ' Check the FAQ document attached to SAP'     "#EC NOTEXT
              'Note 1872170 for explanations on how'        "#EC NOTEXT
              INTO outln SEPARATED BY space.
  WRITE / outln.
  WRITE: ' to interpret the sizing terms and calculations.', /.
                                                            "#EC NOTEXT
  IF wa_cpu_x-est_sapscnt >= 500000.
    CONCATENATE ' The CPU Sizing is larger than 500.000 '   "#EC NOTEXT
    'SAPS. Automatic CPU sizing is not'
    INTO outln SEPARATED BY space.
    WRITE: / outln.
    WRITE: / ' available.', /.
  ENDIF.

  IF wa_cpu_x-est_sapscnt IS INITIAL.
    outln = ' No statistics were found. CPU sizing was not possible'.
                                                            "#EC NOTEXT
    WRITE: / outln, /.
  ENDIF.

  IF p_norm = abap_false.
    CONCATENATE ' This output has been recreated from a'    "#EC NOTEXT
     'previous execution of the sizing report.'
      INTO outln SEPARATED BY space.
    WRITE / outln.
    WRITE: / ' Some information is missing.', /.            "#EC NOTEXT
  ENDIF.

  IF l_wa_techd-db_vendor(3) = 'ADA' AND wa_techd-prec <> 'H'.
    WRITE: / ' The sizing report has been executed on MaxDB',
                                                            "#EC NOTEXT
    'with low or medium accuracy.'.                         "#EC NOTEXT
    WRITE: / ' Since sampling is not possible with MaxDB, we'.
                                                            "#EC NOTEXT
    WRITE: 'recommend chosing a sample'.                    "#EC NOTEXT
    WRITE: / ' size of 1.000.000.', /.                      "#EC NOTEXT
  ENDIF.

  mc_s 'Sizing report:' l_wa_techd-report_name.             "#EC NOTEXT
  mc_s 'Version of the report:' l_wa_techd-report_version.  "#EC NOTEXT
  mc_s 'Date of analysis:' l_wa_techd-crdate.               "#EC NOTEXT
  IF p_ocpu = abap_true.
    mc_s 'Date of CPU analysis:' sy-datum.                  "#EC NOTEXT
  ELSE.
    mc_s 'Date of CPU analysis:' l_wa_techd-crdate.         "#EC NOTEXT
  ENDIF.
  IF l_wa_techd-db_vendor(3) <> 'HDB'
    OR ( l_wa_techd-db_vendor(3) = 'HDB' AND p_calib = abap_true ).
    mc_s 'Selected sample size:' wa_techd-prec.             "#EC NOTEXT
  ENDIF.
  IF l_wa_techd-db_vendor(3) <> 'HDB'
    OR ( l_wa_techd-db_vendor(3) = 'HDB' AND p_calib = abap_true ).
    mc_s 'Cluster sampling mode:' wa_techd-clustprec.       "#EC NOTEXT
  ENDIF.

  mc_s 'Data aging residence time in days for technical objects:'
                                        l_wa_techd-daag.    "#EC NOTEXT
  mc_s 'Number of work processes used:' l_wa_techd-paral.   "#EC NOTEXT
  mc_sn 'Duration of the analysis in seconds:'
                                       l_wa_techd-duration. "#EC NOTEXT

  IF l_wa_techd-db_vendor(3) = 'HDB'.
    IF l_wa_techd-hdbload = abap_true.
      mc_s 'Unloaded columns were loaded:' 'Yes'.           "#EC NOTEXT
    ELSE.
      mc_s 'Unloaded columns were loaded:' 'No'.            "#EC NOTEXT
    ENDIF.
    IF l_wa_techd-hl = abap_true.
      mc_s 'The size of Hybrid LOB on disk was read:'
            'Yes'.                                          "#EC NOTEXT
    ELSE.
      mc_s 'The size of Hybrid LOB on disk was read:'
           'No'.                                            "#EC NOTEXT
    ENDIF.
    IF l_wa_techd-mac = abap_true.
      mc_s 'Tables were merged and compressed if necessary:'
           'Yes'.                                           "#EC NOTEXT
    ELSE.
      mc_s 'Tables were merged and compressed if necessary:'
           'No'.                                            "#EC NOTEXT
    ENDIF.
    IF l_wa_techd-hdb_empty_tables = abap_false
      AND l_wa_techd-report_version >= '78'. "param new with V78
      mc_s 'Empty tables are included in the sizing:'
           'No'.                                            "#EC NOTEXT
    ELSE.
      mc_s 'Empty tables are included in the sizing:'
           'Yes'.                                           "#EC NOTEXT
    ENDIF.
  ELSE.
    mc_s 'Target HANA version:' l_wa_techd-hdbv.            "#EC NOTEXT
    IF wa_techd-basis_753 = abap_true.
      mc_s 'Target ABAP BASIS version higher or equal to 753:'
                                                   'Yes'.   "#EC NOTEXT
    ELSE.
      mc_s 'Target ABAP BASIS version higher or equal to 753:'
                                                   'No'.    "#EC NOTEXT
    ENDIF.
  ENDIF.
  IF l_wa_techd-calib = abap_true.
    mc_s 'Calibration:' outmode.                            "#EC NOTEXT
  ENDIF.
  WRITE /.
  mc_s 'SID' l_wa_techd-sid.                                "#EC NOTEXT
  mc_s 'NW release:' l_wa_techd-sapversion.                 "#EC NOTEXT
  mc_s 'Kernel version' l_wa_techd-kernversion.             "#EC NOTEXT
  mc_s 'Operating system on AS' l_wa_techd-osversion(17).   "#EC NOTEXT
  mc_s 'Type of analysed database:' l_wa_techd-db_vendor.   "#EC NOTEXT
  mc_s 'Database version:' l_wa_techd-db_release.           "#EC NOTEXT
  mc_s 'Unicode system:' l_wa_techd-unicode.                "#EC NOTEXT
  CONCATENATE 'Data used size on disk of the'
               ' analysed database in GB:'     INTO outln.  "#EC NOTEXT
  mc_sn outln wa_techd-anydbsize_n.                         "#EC NOTEXT
  outln = 'Date of last disk size analysis (See FAQ):'.     "#EC NOTEXT
  mc_s outln wa_techd-anydbsizedate.                        "#EC NOTEXT

  WRITE /.
  mc_sn 'Number of tables successfully analysed:'           "#EC NOTEXT
        l_wa_techd-success_cnt.                             "#EC NOTEXT
  mc_sn 'Number of tables partially analysed:'              "#EC NOTEXT
        l_wa_techd-partial_cnt.                             "#EC NOTEXT
  "Always write error count even if initial.
  WRITE: / '', AT 2 'Number of tables with error:',         "#EC NOTEXT
               AT 63(17) l_wa_techd-error_cnt RIGHT-JUSTIFIED.

  IF l_wa_techd-lcerror = abap_true.
    WRITE /.
    mc_s 'Live Cache sizing was not possible:' 'See FAQ'.   "#EC NOTEXT
  ENDIF.

  IF l_wa_techd-mss1691513 = abap_true.
    CONCATENATE ' IMPORTANT: On MSSQL and NW 700,'
                'SAP Note 1691513 MUST be implemented before'
                INTO outln SEPARATED BY space.              "#EC NOTEXT
    WRITE: /, outln.
    WRITE / ' running the sizing report.'.                  "#EC NOTEXT
  ENDIF.

  "ERRORS TABLE
  IF gt_error IS NOT INITIAL.
    SORT gt_error BY db_table.
    IF l_error_ratio > 0.
      mc_sn 'Error estimation in % due to tables with errors:'
            l_error_ratio.                                  "#EC NOTEXT
    ENDIF.
    IF l_error_ratio > c_max_err OR l_init_data = 0.
      WRITE /.
      WRITE: / '', AT 2
      'WARNING! The size of tables in error is',            "#EC NOTEXT
  'too large. The sizing result is NOT reliable. Check the', "#EC NOTEXT
  ' error log, correct the errors and re-execute the report.'
                                         RIGHT-JUSTIFIED.   "#EC NOTEXT
    ENDIF.
    WRITE /.

    " Write errors
    PERFORM add_big_line.
    WRITE:
     / '|',
     (30) 'TABLES NAMES',                                   "#EC NOTEXT
     (67) 'ERROR MESSAGE/CODE (See FAQ)' LEFT-JUSTIFIED,    "#EC NOTEXT
     AT 101 '|'.
    PERFORM add_big_line.
    LOOP AT gt_error INTO ls_error.
      WRITE: / '|', (30) ls_error-table_name.
      IF ls_error-error_message IS NOT INITIAL.
        WRITE: (67) ls_error-error_message LEFT-JUSTIFIED, AT 101 '|'.
      ELSE.
        WRITE: (67) ls_error-return_code   LEFT-JUSTIFIED, AT 101 '|'.
      ENDIF.
    ENDLOOP.
    PERFORM add_big_line.
  ENDIF.

  "Partial collection
  IF gt_partial IS NOT INITIAL.
    SORT gt_partial BY db_table.
    WRITE /.
    PERFORM add_line.
    WRITE:
      / '|',
      AT 3  'TABLES PARTIALLY READ',                        "#EC NOTEXT
      AT 53 'NUMBER OF RECORDS SAMPLED' CENTERED,           "#EC NOTEXT
      AT 80 '|'.
    PERFORM add_line.
    LOOP AT gt_partial INTO ls_partial.
      WRITE: / '|', AT 3  ls_partial-table_name,
        AT 53 ls_partial-records_sampled LEFT-JUSTIFIED, AT 80 '|'.
    ENDLOOP.
    PERFORM add_line.
  ENDIF.

  "INIT DETAILS TABLE
  WRITE /. PERFORM add_line.
  mc_t2 'MEMORY SIZING CALCULATION DETAILS'
        'HANA SIZE IN GB'.                                  "#EC NOTEXT
  PERFORM add_line.
  mc_sp '  Column store data' l_cs_total.                   "#EC NOTEXT
  mc_sp '+ Row store data' l_rs_total.                      "#EC NOTEXT
  mc_sp '+ Changes in FI tables and columns' l_sfinnew.     "#EC NOTEXT
  mc_sp '+ Changes in MM/SD tables and columns' l_slognew.  "#EC NOTEXT
  mc_sp '+ Changes in ML tables and columns' l_mlnew.       "#EC NOTEXT
  PERFORM add_sep_line.

  mc_sp '= Anticipated memory requirement for the initial data'
          l_init_data.                                      "#EC NOTEXT
  mc_sp '+ Cached Hybrid LOB (20%)'
          l_hlch.                                           "#EC NOTEXT
  mc_sp '+ Work space' l_init_ws.                           "#EC NOTEXT
  mc_sp ' (Including concatenated attributes not from DDIC)'
           l_cs_jec.                                        "#EC NOTEXT
  mc_sp '+ Fixed size for code, stack and other services'
          codestack.                                        "#EC NOTEXT
  PERFORM add_sep_line.

  mc_sp '= Anticipated initial memory requirement for HANA'
            l_init_size.                                    "#EC NOTEXT
  PERFORM add_line.

  "DISK SIZING DETAILS.
  WRITE /. PERFORM add_line.
  mc_t2 'DISK SIZING CALCULATION DETAILS' 'HANA SIZE IN GB'. "#EC NOTEXT
  PERFORM add_line.
  mc_sp '  Column store data' l_cs_total.                   "#EC NOTEXT
  mc_sp '+ Row store data' l_rs_total.                      "#EC NOTEXT
  mc_sp '+ Changes in FI tables' l_sfinnew.                 "#EC NOTEXT
  mc_sp '+ Changes in MM/SD tables' l_slognew.              "#EC NOTEXT
  mc_sp '+ Changes in ML tables' l_mlnew.                   "#EC NOTEXT
  mc_sp '+ hybrid LOBs' l_hl.                               "#EC NOTEXT
  mc_sp '+ Space required for merges' l_twocs.              "#EC NOTEXT
  mc_sp '+ Metadata and statistics' diskadmin.              "#EC NOTEXT
  PERFORM add_sep_line.
  mc_sp '= Initial net data size on disk' l_disk.           "#EC NOTEXT
  PERFORM add_line. WRITE /.

  "CLEAN-UP DETAILS TABLE
  IF l_opt_size <> l_init_size.
    PERFORM add_line.
    mc_t2 'DATA MODEL CHANGES AND CLEAN UP DETAILS'
          'HANA SIZE IN GB'.                                "#EC NOTEXT
    PERFORM add_line.
    mc_sp '  Anticipated memory requirement for the initial data'
              l_init_data.                                  "#EC NOTEXT
    mc_sp '- Obsolete financial data (Aggregates, indexes...)'
              l_obs_fin.                                    "#EC NOTEXT
    mc_sp '- Obsolete logistics data (Aggregates, indexes...)'
              l_obs_log.                                    "#EC NOTEXT
    mc_sp '- Obsolete material ledger actual costing data'
              l_obs_ml.                                     "#EC NOTEXT
    mc_sp '- Basis Data aged to disk' l_aged.               "#EC NOTEXT
    mc_sp '- Obsolete non-unique secondary indexes'         "#EC NOTEXT
              l_cs_nuk.                                     "#EC NOTEXT
    PERFORM add_sep_line.
    mc_sp '= Anticipated memory requirement of data after clean-up'
              l_opt_data.                                   "#EC NOTEXT
    mc_sp '+ Cached Hybrid LOB (20%)'
            l_hlch.                                         "#EC NOTEXT
    mc_sp '+ Cached data from aged partitions (20%)'
              l_agedch.                                     "#EC NOTEXT
    mc_sp '+ work space'
              l_opt_ws.                                     "#EC NOTEXT
    mc_sp '+ Fixed size for code, stack and other services'
              codestack.                                    "#EC NOTEXT
    PERFORM add_sep_line.
    mc_sp '= Anticipated total memory requirement after clean-up'
              l_opt_size.                                   "#EC NOTEXT
    PERFORM add_line. WRITE /.
    "END OF CLEAN-UP DETAILS TABLE

    "OPT DISK SIZING DETAILS.
    PERFORM add_line.
    mc_t2 'DISK SIZING AFTER CLEAN-UP' 'HANA SIZE IN GB'.   "#EC NOTEXT
    PERFORM add_line.
    mc_sp ' Anticipated disk requirement for the data after clean-up'
                l_opt_data.                                 "#EC NOTEXT
    mc_sp '+ Data aged on disk (20% overhead)' l_ageddisk.  "#EC NOTEXT
    mc_sp '+ hybrid LOBs' l_hl.                             "#EC NOTEXT
    mc_sp '+ Space required for merges' l_opt_twocs.        "#EC NOTEXT
    mc_sp '+ Metadata and statistics' diskadmin.            "#EC NOTEXT
    PERFORM add_sep_line.
    mc_sp '= Net data size on disk after clean-up'
                                                 l_optdisk. "#EC NOTEXT
    PERFORM add_line. WRITE /. WRITE /.

    "START OF S/4 DETAILS TABLE
    PERFORM add_line.
    mc_t2 'CLEAN UP CALCULATION DETAILS' 'HANA SIZE IN GB'. "#EC NOTEXT
    PERFORM add_line.
    " To Simple Finance
    mc_sp1 'Changes in FI tables and columns' l_sfinnew.    "#EC NOTEXT
    mc_sp1 '  New financial tables:'          l_sfintab.    "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE table_name(6) = 'ACDOCA'.
      CONCATENATE <sfin>-table_name <sfin>-group INTO outln
        SEPARATED BY space.
      mc_spi outln <sfin>-ssize.
    ENDLOOP.
    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE group = 'FI-AA'.
      mc_spi <sfin>-table_name  <sfin>-ssize.
    ENDLOOP.
    mc_sp1 '  New columns in existing tables:' l_sfincol.   "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE group = 'NEWCOL'
        AND size > 0.
      mc_spi <sfin>-table_name <sfin>-ssize.
    ENDLOOP.
    IF l_sfinnew IS NOT INITIAL.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "To Simple Logistics
    mc_sp1 'Changes in logistics tables and columns' l_slognew. "#EC *
    mc_sp1 ' - New tables:' l_slogtab.                      "#EC NOTEXT
    LOOP AT gt_slog ASSIGNING <slog> WHERE group = 'TAB' AND ssize > 0.
      mc_spi <slog>-table_name <slog>-ssize.
    ENDLOOP.
    mc_sp1 ' - New columns in existing tables:' l_slogcol.  "#EC NOTEXT
    LOOP AT gt_slog ASSIGNING <slog>
      WHERE group = 'NEWCOL' AND ssize > 0.
      mc_spi <slog>-table_name <slog>-ssize.
    ENDLOOP.
    mc_sp1 ' - Changes in VBFA:' l_log_chgvbfa.             "#EC NOTEXT
    IF l_log_chgvbfa IS NOT INITIAL.
      mc_spi 'Old VBFA table' l_log_oldvbfa.                "#EC NOTEXT
      mc_spi 'New VBFA table' l_log_newvbfa.                "#EC NOTEXT
    ENDIF.
    IF l_slognew IS NOT INITIAL.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "To new actual costing
    mc_sp1 'Changes in actual costing tables' l_mlnew.      "#EC NOTEXT
    mc_sp1 ' - New tables:' l_mlnew.                        "#EC NOTEXT
    LOOP AT gt_ml ASSIGNING <ml> WHERE group = 'AC' AND ssize > 0.
      mc_spi <ml>-table_name <ml>-ssize.
    ENDLOOP.
    IF l_mlnew IS NOT INITIAL.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "Saving potential with Simple Finance
    mc_sp1 'Obsolete financial data' l_obs_fin.             "#EC NOTEXT
    mc_sp1 ' - Obsolete tables to unload:' l_fin_unl.       "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin>
        WHERE group = 'UNLOAD' OR group = 'NEWGL'
           OR group = 'AA'     OR group = 'FAAT' AND size > 0.
      mc_spi <sfin>-table_name <sfin>-size.
    ENDLOOP.
    mc_sp1 ' - Tables moved to historical partitions:'
                                          l_sfincold.       "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin>
        WHERE group = 'INDEX' AND size > 0.
      mc_spi <sfin>-table_name <sfin>-size.
    ENDLOOP.
    mc_sp1 ' - Deletion of obsolete CO documents:'
              l_delcoep.                                    "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin>
        WHERE group = 'OBSCO' OR table_name = 'COEP' AND size > 0.
      mc_spi <sfin>-table_name <sfin>-osize.
    ENDLOOP.
    mc_sp1 ' - Phase-out of Profit Center Accounting (EC-PCA):'
             l_pca.                                         "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE group = 'EC-PCA'
                                        AND size > 0.
      mc_spi <sfin>-table_name <sfin>-size.
    ENDLOOP.
    mc_sp1 ' - Phase-out of Special Ledgers (FI-SL):' l_sl. "#EC NOTEXT
    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE group = 'FI-SL'
                                        AND size > 0.
      mc_spi <sfin>-table_name <sfin>-size.
    ENDLOOP.
    IF l_obs_fin IS NOT INITIAL.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "Saving potential with Simple Logistics
    mc_sp1 'Obsolete logistics data' l_obs_log.             "#EC NOTEXT
    mc_sp1 ' - Obsolete tables:' l_log_unl.                 "#EC NOTEXT
    LOOP AT gt_slog ASSIGNING <slog>
        WHERE group = 'UNLOAD' AND size > 0.
      mc_spi <slog>-table_name <slog>-size.
    ENDLOOP.
    mc_sp1 ' - Obsolete columns in tables:' l_log_les.      "#EC NOTEXT
    LOOP AT gt_slog ASSIGNING <slog>
        WHERE group = 'LESS' AND ssize > 0.
      mc_spi <slog>-table_name <slog>-ssize.
    ENDLOOP.
    IF l_obs_log IS NOT INITIAL.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "Saving potential with actual costing
    mc_sp1 'Obsolete actual costing data' l_obs_ml.         "#EC NOTEXT
    mc_sp1 ' - Obsolete tables:' l_obs_ml.                  "#EC NOTEXT
    LOOP AT gt_ml ASSIGNING <ml>
        WHERE group = 'OBS' AND ssize > 0.
      mc_spi <ml>-table_name <ml>-ssize.
    ENDLOOP.
    IF l_obs_ml IS NOT INITIAL.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "Data aging
    IF l_aged IS NOT INITIAL.
      mc_sp1 'Basis Data aged to disk:'   l_aged.           "#EC NOTEXT
      LOOP AT gt_aging ASSIGNING <aging> WHERE hist_size > 0.
        mc_spi <aging>-table_name <aging>-hist_size.
      ENDLOOP.
      WRITE: / '|', AT 80 '|'.
    ENDIF.

    "Non-unique keys in DDIC
    IF l_cs_nuk IS NOT INITIAL.
      mc_sp1 'Obsolete non-unique secondary indexes'        "#EC NOTEXT
                                                l_cs_nuk.   "#EC NOTEXT
      IF p_disp = abap_false.
        CONCATENATE '- The list of non-unique indexes is'   "#EC NOTEXT
                'displayed in a separate table.'            "#EC NOTEXT
        INTO outln SEPARATED BY space.
      ELSE.
        CONCATENATE '- The list of non-unique indexes is'   "#EC NOTEXT
                'not available with older reports.'         "#EC NOTEXT
        INTO outln SEPARATED BY space.
      ENDIF.
      mc_sp_t outln.
    ENDIF.

    PERFORM add_line. WRITE /.
    "END OF S/4 DETAILS TABLE
  ENDIF.

  IF l_shatotal > 0. "UPGRADE
    PERFORM add_line.
    mc_t2 'ESTIMATED REQUIREMENT FOR UPGRADE SHADOW INSTANCES'
          'HANA SIZE IN GB'.                                "#EC NOTEXT
    PERFORM add_line.
    mc_sp '  Estimated size of tables cloned to shadow instances'
                                                     l_sha. "#EC NOTEXT
    mc_sp '+ Estimated corresponding work space requirement'
                                                     l_sha. "#EC NOTEXT
    PERFORM add_sep_line.
    mc_sp '= Total memory requirement for shadow instances'
                                                l_shatotal. "#EC NOTEXT
    mc_sp '= Total disk requirement for shadow instances'
                                                l_shadisk.  "#EC NOTEXT
    IF l_uptotal <> l_shatotal.
      WRITE: / '|', AT 80 '|'.
      mc_sp '+ Estimated data size of cloned tables for sMIG conversion'
                                                   l_updata. "#EC NOTEXT
      mc_sp '+ Estimated size of change recording and replay tables'
                                                 l_upchrec. "#EC NOTEXT
      mc_sp '+ Estimated work space requirement for cloned tables'
                                                   l_updata. "#EC NOTEXT
      PERFORM add_sep_line.
      mc_sp '= Total additional memory requirement for sMIG conversion'
                                                 l_uptotal. "#EC NOTEXT
    ENDIF.
    PERFORM add_line. WRITE /.
  ENDIF.

  "CPU sizing details
  IF wa_cpu_x IS NOT INITIAL.
    PERFORM add_line.
    mc_sp_t 'CPU SIZING DETAILS'.                           "#EC NOTEXT
    PERFORM add_line.
    mc_sp 'Available aggregated statistics basis:'          "#EC NOTEXT
          'Start dates'.                                    "#EC NOTEXT
    mc_per 'H' 'Hourly'.                                    "#EC NOTEXT
    mc_per 'D' 'Daily'.                                     "#EC NOTEXT
    mc_per 'W' 'Weekly'.                                    "#EC NOTEXT
    mc_per 'M' 'Monthly'.                                   "#EC NOTEXT
    WRITE: / '|', AT 80 '|'.
    mc_spn 'Peak (Select equivalent/h)'                     "#EC NOTEXT
            wa_cpu_x-max_hloadcnt.                          "#EC NOTEXT
    mc_sp 'Peak period basis' wa_cpu_x-datecnt.             "#EC NOTEXT
    mc_spn 'Peak Factor' wa_cpu_x-max_factcnt.              "#EC NOTEXT
    "  mc_sp 'Peak Factor date' wa_cpu_x-max_factcnt_date.  "#EC NOTEXT
    IF wa_cpu_x-est_sapscnt > 500000.
      mc_spn 'Estimated SAPS' '>500.000'.                   "#EC NOTEXT
    ELSE.
      mc_spn 'Estimated SAPS' wa_cpu_x-est_sapscnt.         "#EC NOTEXT
    ENDIF.
    mc_sp 'Estimated SAPS category' wa_cpu_x-est_sapsts.    "#EC NOTEXT
    mc_spn 'Peak period background tasks percentage'        "#EC NOTEXT
                                         wa_cpu_x-bkgratio. "#EC NOTEXT
    PERFORM add_line.
    IF wa_cpu_x-bkgratio >= 50.
      CONCATENATE ' The share of background jobs in the load'
       'is above 50%. The CPU Sizing'                       "#EC NOTEXT
            INTO outln SEPARATED BY space.                  "#EC NOTEXT
      WRITE / outln.
      WRITE: / ' estimation is possibly too high.', /.      "#EC NOTEXT
    ENDIF.
    CONCATENATE ' Check the FAQ document attached to SAP'   "#EC NOTEXT
                'Note 1872170 for explanations on how'      "#EC NOTEXT
                INTO outln SEPARATED BY space.
    WRITE / outln.
    WRITE: ' to interpret the CPU sizing calculations.', /. "#EC NOTEXT
  ENDIF.

  "SIZING FOR NVRAM
  IF l_init_pers IS NOT INITIAL AND wa_techd-hdbv <> '1.0'
      AND sy-dbsys(3) <> 'HDB'.
    PERFORM add_line.
    mc_t2 'MEMORY SIZING FOR PERSISTENT MEMORY (NVRAM)'
          'HANA SIZE IN GB'.                                "#EC NOTEXT
    PERFORM add_line.
    mc_sp '  Column store data' l_cs_total.                 "#EC NOTEXT
    mc_sp '+ Changes in FI tables and columns' l_sfinnew.   "#EC NOTEXT
    mc_sp '+ Changes in MM/SD tables and columns' l_slognew. "#EC NOTEXT
    mc_sp '+ Changes in ML tables and columns' l_mlnew.     "#EC NOTEXT
    PERFORM add_sep_line.
    mc_sp
    '= Anticipated initial requirement for the Persistent Memory'
            l_init_pers.                                    "#EC NOTEXT
    WRITE: / '|', AT 80 '|'.
    mc_sp '  Row store data' l_rs_total.                    "#EC NOTEXT
    mc_sp '+ Cached Hybrid LOB (20%)' l_hlch.               "#EC NOTEXT
    mc_sp '+ Work space' l_init_ws.                         "#EC NOTEXT
    mc_sp '+ Fixed size for code, stack and other services'
            codestack.                                      "#EC NOTEXT
    PERFORM add_sep_line.
    mc_sp '= Anticipated initial requirement for the DRAM'
            l_init_dram.                                    "#EC NOTEXT

    PERFORM add_line.
    CONCATENATE ' Check SAP Note 2618154 and 2786237 for more '
            'information on Persistent Memory.'             "#EC NOTEXT
            INTO outln SEPARATED BY space.
    WRITE / outln.
  ENDIF.

  IF outmode = 'ANY'.
    sizefield = 'EST_SIZE'. hlfield = 'EST_HL_SIZE'.
    pkfield = 'EST_PK_SIZE'. agefield = 'EST_AGE_SIZE'.
  ELSE.
    sizefield = 'REAL_SIZE'. hlfield = 'REAL_HL_SIZE'.
    pkfield = 'REAL_PK_SIZE'. agefield = 'REAL_AGE_SIZE'.
  ENDIF.

  " LARGEST CS TABLES
  CLEAR: gt_top, gs_top,  count.
  SORT gt_tables BY (sizefield) DESCENDING record_count DESCENDING.
  LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
    count = count + 1.
    ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
    IF count > p_topobj OR ( gt_nso IS INITIAL AND <sizefs> = 0 ).
      EXIT.
    ENDIF.
    gs_top-col1 = <item>-table_name.
    gs_top-col2p = <sizefs>.
    gs_top-col3d = <item>-record_count.
    APPEND gs_top TO gt_top.
  ENDLOOP.
  mc_topheader 'LARGEST COLUMN' 'STORE TABLES'              "#EC NOTEXT
     'ESTIMATED HANA' 'MEMORY SIZE IN GB'                   "#EC NOTEXT
     'ESTIMATED RECORD' '      COUNT'.                      "#EC NOTEXT
  PERFORM list_top_objects TABLES gt_top USING 'pd'.

  "Existing data aging
  CLEAR: gt_top, gs_top,  count.
  LOOP AT gt_tables ASSIGNING <item> WHERE load_unit = 'PAGE'.
    count = count + 1.
    ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
    IF count > p_topobj OR ( gt_nso IS INITIAL AND <sizefs> = 0 )..
      EXIT.
    ENDIF.
    gs_top-col1 = <item>-table_name.
    gs_top-col2p = <sizefs>.
    gs_top-col3d = <item>-record_count.
    APPEND gs_top TO gt_top.
  ENDLOOP.
  IF count IS NOT INITIAL.
    mc_topheader 'LARGEST PAGED' ' TABLES'                  "#EC NOTEXT
     'ESTIMATED HANA' 'MEMORY SIZE IN GB'                   "#EC NOTEXT
       'ESTIMATED RECORD' '      COUNT'.                    "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pd'.
    CONCATENATE ' Note: Paged attributes are used on the'   "#EC NOTEXT
                'above objects, the report only shows the'  "#EC NOTEXT
                INTO outln SEPARATED BY space.
    WRITE / outln.
    WRITE ' current consumption at time of measurement.' .  "#EC NOTEXT
  ENDIF.

  " LARGEST CS PK
  IF l_cs_pk > 0.
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY (pkfield) DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      ASSIGN COMPONENT pkfield OF STRUCTURE <item> TO <sizefs>.
      IF count > p_topobj OR <sizefs> = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <sizefs>.
      ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
      gs_top-col3p = <sizefs>.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader 'LARGEST COLUMN STORE' '  PRIMARY KEYS'    "#EC NOTEXT
       ' PRIMARY KEY ' 'MEMORY SIZE IN GB'                  "#EC NOTEXT
       '    ENTIRE TABLE' '      SIZE IN GB'.               "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pp'.
  ENDIF.

  IF l_cs_jec > 0. "Concatenated attributes
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY real_jec_size DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      IF count > p_topobj OR <item>-real_jec_size = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <item>-real_jec_size.
      ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
      gs_top-col3p = <sizefs>.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST NON-DDIC' 'CONCAT. ATTRIBUTES'   "#EC NOTEXT
                 'CONCAT. ATTRIBUTES' '     SIZE IN GB'     "#EC NOTEXT
                 '    OTHER COLUMNS' '      SIZE IN GB'.    "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pp'.
    IF gt_top IS NOT INITIAL. "read older report case
      CONCATENATE ' Check with SAP Note 1986747 if the above'
                'concatenated attributes are necessary.'
                INTO outln SEPARATED BY space.              "#EC NOTEXT
      WRITE outln.
    ENDIF.
  ENDIF.

  IF l_cs_nuk > 0. "non-unique indexes
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY real_nuk_size DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      IF count > p_topobj OR <item>-real_nuk_size = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <item>-real_nuk_size.
      ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
      gs_top-col3p = <sizefs>.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST NON'    'UNIQUE INDEXES'         "#EC NOTEXT
                 'CONCAT. ATTRIBUTES' '     SIZE IN GB'     "#EC NOTEXT
                 '    ENTIRE TABLE' '      SIZE IN GB'.     "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pp'.
  ENDIF.

  IF l_cs_ft > 0. "ESH attributes
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY real_esh_size DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      IF count > p_topobj OR <item>-real_esh_size = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <item>-real_esh_size.
      ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
      gs_top-col3p = <sizefs>.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST ENTERPRISE' 'SEARCH ATTRIBUTES'  "#EC NOTEXT
                 'MEMORY SIZE IN GB' ''                     "#EC NOTEXT
                 '    ENTIRE TABLE' '      SIZE IN GB'.     "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pp'.
  ENDIF.

  " LARGEST RS TABLES
  CLEAR: gt_top, gs_top,  count.
  SORT gt_tables BY (sizefield) DESCENDING record_count DESCENDING.
  LOOP AT gt_tables ASSIGNING <item> WHERE store = 'RS'.
    count = count + 1.
    ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
    IF count > p_topobj OR ( gt_nso IS INITIAL AND <sizefs> = 0 ).
      EXIT.
    ENDIF.
    gs_top-col1 = <item>-table_name.
    gs_top-col2p = <sizefs>.
    gs_top-col3d = <item>-record_count.
    APPEND gs_top TO gt_top.
  ENDLOOP.
  mc_topheader ' LARGEST ROW' 'STORE TABLES'                "#EC NOTEXT
     'ESTIMATED HANA' 'MEMORY SIZE IN GB'                   "#EC NOTEXT
     'ESTIMATED RECORD' '      COUNT'.                      "#EC NOTEXT
  PERFORM list_top_objects TABLES gt_top USING 'pd'.

  IF l_rs2cs IS NOT INITIAL.
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY real_rs_exsize DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      IF count > p_topobj OR <item>-real_rs_exsize = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <item>-real_rs_exsize.
      gs_top-col3p = <item>-real_size.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader '  ROW STORE TABLES' 'MOVED TO ROW STORE'  "#EC NOTEXT
       ' CURENT ROW STORE' '  MEMORY SIZE IN GB'            "#EC NOTEXT
       '   EST. COL. SIZE' '      SIZE IN GB'.              "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pp'.
    CONCATENATE ' Note: During the transition to S/4HANA'   "#EC NOTEXT
                'some tables will be moved to the Column'   "#EC NOTEXT
                INTO outln SEPARATED BY space.
    WRITE / outln.
    CONCATENATE ' Store. The table above shows their '      "#EC NOTEXT
                'estimated size in the column store.'       "#EC NOTEXT
                INTO outln SEPARATED BY space.
    WRITE / outln.
  ENDIF.

  IF l_cshl > 0." largest CS LOB objects.
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY (hlfield) DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      ASSIGN COMPONENT hlfield OF STRUCTURE <item> TO <hlfs>.
      IF count > p_topobj OR <hlfs> = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <hlfs>.
      gs_top-col3d = <item>-record_count.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST COLUMN STORE' '  TABLES WITH LOB' "#EC NOTEXT
       'ESTIMATED LOB' ' SIZE ON DISK'                      "#EC NOTEXT
       'ESTIMATED RECORD' '      COUNT'.                    "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pd'.
  ENDIF.

  IF l_rshl > 0." largest RS LOB objects.
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY (hlfield) DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'RS'.
      count = count + 1.
      ASSIGN COMPONENT hlfield OF STRUCTURE <item> TO <hlfs>.
      IF count > p_topobj OR <hlfs> = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <hlfs>.
      gs_top-col3d = <item>-record_count.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST ROW STORE' ' TABLES WITH LOB'    "#EC NOTEXT
       'ESTIMATED LOB' ' SIZE ON DISK'                      "#EC NOTEXT
       'ESTIMATED RECORD' '      COUNT'.                    "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pd'.
  ENDIF.

  " LARGEST TABLES AFTER CLEAN UP
  IF l_opt_data <> l_init_data.
    CLEAR: gt_top, gs_top,  count.
    SORT gt_tables BY opt_size DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item>.
      count = count + 1.
      IF count > p_topobj
        OR ( gt_nso IS INITIAL AND <item>-opt_size = 0 ).
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2p = <item>-opt_size.
      gs_top-col3c = <item>-store.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST TABLES' 'AFTER CLEAN-UP '        "#EC NOTEXT
       'ESTIMATED HANA' 'MEMORY SIZE IN GB'                 "#EC NOTEXT
       '         STORE' ''.                                 "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pc'.
  ENDIF.

  IF l_shatotal > 0.
    CLEAR: gt_top, gs_top, count.
    SORT gt_shadow BY size DESCENDING record_count DESCENDING.
    LOOP AT gt_shadow ASSIGNING <upgrade>.
      count = count + 1.
      IF count > p_topobj OR <upgrade>-size = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <upgrade>-table_name.
      gs_top-col2p = <upgrade>-size.
      gs_top-col3d = <upgrade>-record_count.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader
       ' LARGEST TABLES CLONED' ' DURING UPGRADES '         "#EC NOTEXT
       'ESTIMATED HANA' 'MEMORY SIZE IN GB'                 "#EC NOTEXT
       'ESTIMATED RECORD' '      COUNT'.                    "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pd'.
  ENDIF.

  IF l_uptotal > 0.
    CLEAR: gt_top, gs_top, count.
    SORT gt_inplace BY size DESCENDING record_count DESCENDING.
    LOOP AT gt_inplace ASSIGNING <upgrade>.
      count = count + 1.
      IF count > p_topobj OR <upgrade>-size = 0.
        EXIT.
      ENDIF.
      gs_top-col1 = <upgrade>-table_name.
      gs_top-col2p = <upgrade>-size.
      gs_top-col3d = <upgrade>-record_count.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST CLONED' '  TABLES '              "#EC NOTEXT
       'ESTIMATED HANA' 'MEMORY SIZE IN GB'                 "#EC NOTEXT
       'ESTIMATED RECORD' '      COUNT'.                    "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pd'.
  ENDIF.

  IF gt_sogrp IS NOT INITIAL.
    "Scale Out groups
    CLEAR: gt_top, gs_top, count.
    SORT gt_sogrp BY size DESCENDING.
    LOOP AT gt_sogrp ASSIGNING <sogrp>.
      IF <sogrp>-size = 0.
        EXIT.
      ENDIF.
      gs_top-col1  = <sogrp>-group.
      gs_top-col2p = <sogrp>-size.
      gs_top-col3p = <sogrp>-opt_size.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader '    SCALE OUT' 'TABLE GROUP NAMES'        "#EC NOTEXT
       'ESTIMATED MEMORY' 'SIZE BEFORE CLEANUP'             "#EC NOTEXT
       'ESTIMATED MEMORY' 'SIZE AFTER CLEANUP'.             "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'pp'.

    IF p_disp = abap_false. "info not stored for later display
      "top tables and their Scale Out group
      CLEAR: gt_top, gs_top, count.
      SORT gt_tables BY (sizefield) DESCENDING record_count DESCENDING.
      LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
        count = count + 1.
        ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
        IF count > p_topobj OR <sizefs> = 0.
          EXIT.
        ENDIF.
        gs_top-col1 = <item>-table_name.
        gs_top-col2p = <sizefs>.
        gs_top-col3c = <item>-sogroup.
        APPEND gs_top TO gt_top.
      ENDLOOP.
      mc_topheader 'LARGEST COLUMN' 'STORE TABLES'          "#EC NOTEXT
         'ESTIMATED HANA' 'MEMORY SIZE IN GB'               "#EC NOTEXT
         '   SCALE OUT' 'TABLE GROUP NAMES'.                "#EC NOTEXT
      PERFORM list_top_objects TABLES gt_top USING 'pc'.
    ENDIF.
    IF gt_top IS NOT INITIAL.
      CONCATENATE ' Check SAP Note 2408419 and 2428711'     "#EC NOTEXT
                  'for more information about Scale-Out.'   "#EC NOTEXT
                  INTO outln SEPARATED BY space.            "#EC NOTEXT
      WRITE outln.
    ENDIF.
  ENDIF.

  " SIZING DETAILS TABLE 2.
  WRITE /.
  PERFORM add_line.
  mc_sp 'DETAILED INITIAL SIZING PER COMPONENT'             "#EC NOTEXT
             'MEMORY SIZE IN GB'.                           "#EC NOTEXT
  PERFORM add_line.
  mc_sp 'Column Store tables:' l_cs_col.                    "#EC NOTEXT
  IF l_wa_techd-db_vendor(3) <> 'HDB'
      OR ( l_wa_techd-db_vendor(3) = 'HDB' AND p_calib = abap_true ).
    mc_sp '  - High cardinality:'    l_cs_hcard.            "#EC NOTEXT
    mc_sp '  - Med. cardinality:'    l_cs_mcard.            "#EC NOTEXT
    mc_sp '  - Low cardinality:'     l_cs_lcard.            "#EC NOTEXT
    mc_sp '  - Others:'              l_cs_ucard.            "#EC NOTEXT
  ENDIF.
  IF l_cs_aging > 0.
    mc_sp '  including aging columns:' l_cs_aging.          "#EC NOTEXT
  ENDIF.
  mc_sp 'Column Store keys:' l_cs_k.                        "#EC NOTEXT
  mc_sp '  - Primary keys:' l_cs_pk.                        "#EC NOTEXT
  mc_sp '  - Row ID:' l_cs_rid.                             "#EC NOTEXT
  mc_sp '  - Udiv:' l_cs_udiv.                              "#EC NOTEXT
  mc_sp '  - Secondary unique keys:' l_cs_uk.               "#EC NOTEXT
  IF l_cs_nuk > 0.
    mc_sp '  - Secondary non-unique keys:' l_cs_nuk.        "#EC NOTEXT
  ENDIF.
  IF l_cs_ft IS NOT INITIAL.
    mc_sp '  - Full text indexes (esh):' l_cs_ft.           "#EC NOTEXT
  ENDIF.
  IF l_cs_jec IS NOT INITIAL.
    mc_sp 'Concatenated attributes not from DDIC:'          "#EC NOTEXT
                                             l_cs_jec.      "#EC NOTEXT
  ENDIF.
  IF l_rs_total IS NOT INITIAL.
    WRITE: / '|', AT 80 '|'.
    mc_sp 'Row Store tables:' l_rs_col.                     "#EC NOTEXT
    mc_sp 'Row Store primary keys:' l_rs_pk.                "#EC NOTEXT
    mc_sp 'Row Store secondary keys:' l_rs_sk.              "#EC NOTEXT
  ENDIF.
  IF l_cshl IS NOT INITIAL OR l_rshl IS NOT INITIAL.
    WRITE: / '|', AT 80 '|'.
    mc_sp 'LOB CS data stored on disk:' l_cshl.             "#EC NOTEXT
    mc_sp 'LOB RS data stored on disk:' l_rshl.             "#EC NOTEXT
    WRITE: / '|', AT 80 '|'.
  ENDIF.
  IF p_ful = abap_true.
    WRITE: / '|', AT 80 '|'.
    mc_sp 'Total pageable parts on tables with aging:' l_elaging. "#EC *
    mc_sp 'Total tables eligible to archiving:' l_elarch.   "#EC NOTEXT
    WRITE: / '|', AT 80 '|'.
  ENDIF.
  WRITE: / '|', AT 80 '|'.
  mc_sp_t 'Check SAP Note 1986747 for more information'.    "#EC NOTEXT
  PERFORM add_line.

  CLEAR: gt_top, gs_top, count.
  SORT gt_tables BY (sizefield) DESCENDING record_count DESCENDING.
  SORT gt_aging_runs BY table_name.
  LOOP AT gt_tables ASSIGNING <item> WHERE record_count > 0.
    count = count + 1.
    IF count > p_topobj.
      EXIT.
    ENDIF.
    gs_top-col1  = <item>-table_name.
    READ TABLE gt_aging_runs ASSIGNING <aging_runs>
      WITH KEY table_name = <item>-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      gs_top-col2c = <aging_runs>-agobject.
      IF <aging_runs>-aglrun IS NOT INITIAL.
        gs_top-col3c = <aging_runs>-aglrun.
      ENDIF.
    ENDIF.
    APPEND gs_top TO gt_top. CLEAR gs_top.
  ENDLOOP.
  mc_topheader 'LARGEST TABLES' ''                          "#EC NOTEXT
   '   AGING' '    OBJECT'                                  "#EC NOTEXT
   '    LAST AGING' '        RUN'.                          "#EC NOTEXT
  PERFORM list_top_objects TABLES gt_top USING 'cc'.

  IF gt_arch_runs IS NOT INITIAL.
    CLEAR: gt_top, gs_top, count.
    LOOP AT gt_tables ASSIGNING <item> WHERE record_count > 0.
      count = count + 1.
      IF count > p_topobj OR count > 100.
        EXIT.
      ENDIF.
      gs_top-col1  = <item>-table_name.
      READ TABLE gt_arch_runs ASSIGNING <arch_runs>
        WITH KEY table_name = <item>-table_name BINARY SEARCH.
      IF sy-subrc = 0.
        DESCRIBE TABLE <arch_runs>-objects LINES arobjcnt.
        IF arobjcnt > 0.
          READ TABLE <arch_runs>-runs ASSIGNING <objarchruns> INDEX 1.
          IF sy-subrc = 0.
            gs_top-col2c = <objarchruns>-object.
            gs_top-col3c = <objarchruns>-arclrun.
          ELSE.
            IF arobjcnt > 1.
              arobjcnt_c = arobjcnt.
              CONCATENATE arobjcnt_c 'OBJECTS' INTO
              gs_top-col2c SEPARATED BY space.
            ELSE.
              READ TABLE <arch_runs>-objects
                ASSIGNING <objarchdef> INDEX 1.
              gs_top-col2c = <objarchdef>-object.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND gs_top TO gt_top. CLEAR gs_top.
    ENDLOOP.
    mc_topheader 'LARGEST TABLES' ''                        "#EC NOTEXT
     '  ARCHIVING' '    OBJECT'                             "#EC NOTEXT
     '  LAST ARCHIVING' '        RUN'.                      "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'cc'.
  ENDIF.

  PERFORM selection_info.

  "Statistic collection details
  IF ( l_wa_techd-count_1 IS NOT INITIAL
     OR l_wa_techd-count_2 IS NOT INITIAL )
    AND p_norm = abap_true.
    CONCATENATE ' The below information is only used by'    "#EC NOTEXT
              'SAP Support and is not sizing relevant.'     "#EC NOTEXT
              INTO outln SEPARATED BY space.                "#EC NOTEXT
    WRITE outln.
    mc_s 'Count(*) forced:'  p_forc.                        "#EC NOTEXT
    mc_sn 'Maximum statistics age in years:'  p_sage.       "#EC NOTEXT
    mc_sn 'Minimum number of records for count:' p_mrec.    "#EC NOTEXT
    mc_sn 'Number of times statistics were inconsistent:'   "#EC NOTEXT
          l_wa_techd-count_1.                               "#EC NOTEXT
    mc_sn 'Number of times a record count was forced:'      "#EC NOTEXT
          l_wa_techd-count_2.                               "#EC NOTEXT
    CLEAR: gt_top, gs_top, count.
    SORT gt_tables BY (sizefield) DESCENDING record_count DESCENDING.
    LOOP AT gt_tables ASSIGNING <item> WHERE count_method = 3.
      count = count + 1.
      IF count > p_topobj.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2c = <item>-count_method.
      gs_top-col3c = <item>-doubt_reason.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader ' LARGEST TABLES WITH' 'FORCED COUNT'      "#EC NOTEXT
       '  COUNT ' '  METHOD'                                "#EC NOTEXT
       '           DOUBT' '           REASON'.              "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'cc'.

    CLEAR: gt_top, gs_top, count.
    LOOP AT gt_tables ASSIGNING <item> WHERE doubt_reason = 1.
      count = count + 1.
      IF count > p_topobj.
        EXIT.
      ENDIF.
      gs_top-col1 = <item>-table_name.
      gs_top-col2c = <item>-count_method.
      gs_top-col3c = <item>-doubt_reason.
      APPEND gs_top TO gt_top.
    ENDLOOP.
    mc_topheader 'LARGEST TABLES WITH' 'NEGATIVE STATS'     "#EC NOTEXT
       '  COUNT ' '  METHOD'                                "#EC NOTEXT
       '           DOUBT' '           REASON'.              "#EC NOTEXT
    PERFORM list_top_objects TABLES gt_top USING 'cc'.
  ENDIF.
ENDFORM.                    " output_on_anydb

*&---------------------------------------------------------------------*
*&      Form  selection_info
*&---------------------------------------------------------------------*
*       Add to report output selection information chosen by user
*----------------------------------------------------------------------*

FORM selection_info.

  IF gt_nso IS NOT INITIAL.
    WRITE /.
    PERFORM add_line.
    WRITE: / '|', 'SELECT-OPTIONS CHOSEN',                  "#EC NOTEXT
       AT 80 '|'.
    PERFORM add_line.
    WRITE: / '|', 'SIGN', AT 20 'OPTION', AT 40 'LOW',      "#EC NOTEXT
       AT 60 'HIGH', AT 80 '|'.                             "#EC NOTEXT
    PERFORM add_line.
    LOOP AT gt_nso INTO wa_nso.
      WRITE: / '|', wa_nso-sign, AT 20 wa_nso-option, AT 40 wa_nso-low,
          AT 60 wa_nso-high, AT 80 '|'.
    ENDLOOP.
    PERFORM add_line.
  ENDIF.
ENDFORM.                    "selection_info

*&---------------------------------------------------------------------*
*&      Form  LIST_TOP_OBJECTS
*&---------------------------------------------------------------------*

FORM list_top_objects  TABLES   lt_top   TYPE ty_t_toptables
                        USING   l_tabletyp TYPE string.

  FIELD-SYMBOLS: <top> TYPE ty_toptables.

  IF lt_top[] IS NOT INITIAL.
    LOOP AT lt_top ASSIGNING <top>.
      CASE l_tabletyp.
        WHEN 'pd'.
          WRITE: / '|',
           (25) <top>-col1 LEFT-JUSTIFIED,
           (20) <top>-col2p RIGHT-JUSTIFIED,
           (29) <top>-col3d RIGHT-JUSTIFIED,
            AT 80 '|'.
        WHEN 'pp'.
          WRITE: / '|',
           (25) <top>-col1 LEFT-JUSTIFIED,
           (20) <top>-col2p RIGHT-JUSTIFIED,
           (29) <top>-col3p RIGHT-JUSTIFIED,
            AT 80 '|'.
        WHEN 'cc'.
          WRITE: / '|',
           (22) <top>-col1 LEFT-JUSTIFIED,
           (26) <top>-col2c CENTERED,
           (26) <top>-col3c RIGHT-JUSTIFIED,
            AT 80 '|'.
        WHEN 'pc'.
          WRITE: / '|',
           (25) <top>-col1 LEFT-JUSTIFIED,
           (20) <top>-col2p RIGHT-JUSTIFIED,
           (28) <top>-col3c RIGHT-JUSTIFIED,
          AT  80 '|'.
      ENDCASE.
    ENDLOOP.
    PERFORM add_line.
    WRITE /.
  ENDIF.

ENDFORM.                    " LIST_TOP_OBJECTS

*&---------------------------------------------------------------------*
*& FORM  add_line
*& Write a line in the output.
*&
*&---------------------------------------------------------------------*
FORM add_line.
  outln   = '|-----------------------------------------------------'.
  outln+45 = '-----------------------------------------------------'.
  outln+79 = '|'.
  WRITE / outln.
  CLEAR outln.
ENDFORM.                    "flush

*&---------------------------------------------------------------------*
*& FORM  add_big_line
*& Write a line in the output.
*&
*&---------------------------------------------------------------------*
FORM add_big_line.
  DATA: nb TYPE i.
  DO 100 TIMES.
    bigoutln+nb = '-'.
    nb = nb + 1.
  ENDDO.
  bigoutln(1)  = '|'.
  bigoutln+100 = '|'.
  WRITE / bigoutln.
  CLEAR bigoutln.
ENDFORM.                    "flush

*&---------------------------------------------------------------------*
*&      Form  check_indx
*&---------------------------------------------------------------------*
*       Check size of INDX tables and perform alternate sizing
*       See SAP Note 1774918
*----------------------------------------------------------------------*

FORM check_indx  USING l_data_total CHANGING l_indx.

  DATA: indx_share  TYPE p,
        indx_factor TYPE p DECIMALS 1 VALUE '0.6'.

  LOOP AT gt_tables ASSIGNING <item>
    WHERE table_name = 'PCL1'
       OR table_name = 'PCL2'
       OR table_name = 'PCL3'
       OR table_name = 'PCL4'
       OR table_name = 'PCL5'.
    l_indx = l_indx + <item>-est_size.
  ENDLOOP.

  IF l_indx IS NOT INITIAL AND l_data_total IS NOT INITIAL.
    indx_share = l_indx / l_data_total * 100.
    IF indx_share > 5. "INDX tables are more than 5% of data stores
      l_indx = l_indx * indx_factor * 2.
    ELSE.
      CLEAR l_indx.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_indx

*&---------------------------------------------------------------------*
*&      Form  lc_sizing
*&      Live cache sizing - Requires SAP Note 1774918
*&---------------------------------------------------------------------*

FORM lc_sizing CHANGING l_lc_size lv_error.
  DATA: l_lcinstalled(1) TYPE c,
        l_lcavailable(1) TYPE c,
        i_lc_size        TYPE int4,
        persist_lc       TYPE p LENGTH 9 DECIMALS 1,
        heap_lc          TYPE p LENGTH 9 DECIMALS 1.

  TRY.
      CALL FUNCTION 'LCA_LIVECACHE_AVAILABLE'
        IMPORTING
          ev_installed = l_lcinstalled
          ev_available = l_lcavailable.
    CATCH cx_sy_dyn_call_illegal_func.
      RETURN.
  ENDTRY.

  IF sy-dbsys = 'HDB'.
    EXEC SQL.
      SELECT SUM(PAGE_SIZE_SUM) into :persist_lc
        FROM SYS.M_LIVECACHE_CONTAINER_STATISTICS
    ENDEXEC.                                            "#EC CI_EXECSQL
    EXEC SQL.
      SELECT SUM(INCLUSIVE_SIZE_IN_USE) into :heap_lc
        FROM SYS.M_HEAP_MEMORY
        WHERE CATEGORY = 'Pool/LVCAllocator'
    ENDEXEC.                                            "#EC CI_EXECSQL
    l_lc_size = persist_lc + heap_lc.
  ELSE.
    IF l_lcinstalled = abap_true AND l_lcavailable = abap_true.
      TRY.
          CALL FUNCTION '/SAPAPO/OM_ESTIMATE_LC_MEM'
            IMPORTING
              ev_estimated_memory_mb = i_lc_size.
          l_lc_size = i_lc_size.
          "convert to bytes
          l_lc_size = l_lc_size * 1024 * 1024.
        CATCH cx_sy_dyn_call_illegal_func.
          lv_error = abap_true.
        CATCH cx_sql_exception.
          lv_error = abap_true.
      ENDTRY.
    ENDIF.
  ENDIF.

ENDFORM.                    " lc_sizing

*&---------------------------------------------------------------------*
*& FORM  check_reliability
*& Check the size of tables that encountered errors and compare to total
*& size.
*&---------------------------------------------------------------------*
FORM check_reliability USING gt_error TYPE ty_t_error
                             l_est_init
                       CHANGING l_error_ratio.

  DATA:
    wa_dbstatam TYPE dbstatam,
    lt_dbstatam TYPE TABLE OF dbstatam,
    lt_lob      TYPE TABLE OF tabname,
    p_db_table  TYPE tabname,
    error_size  TYPE p LENGTH 10 DECIMALS 2.

  CONSTANTS:
    cs_factor TYPE p VALUE '4'. "Rough compression factor...

  FIELD-SYMBOLS:
    <error>     TYPE ty_error.

  " look up missing sizes of tables with error in source system.
  LOOP AT gt_error ASSIGNING <error>.
    IF <error>-source_size LE 0.
      wa_dbstatam-tname = <error>-db_table.
      APPEND wa_dbstatam TO lt_dbstatam.
    ENDIF.
  ENDLOOP.

  IF lt_dbstatam IS NOT INITIAL.
    CALL FUNCTION 'DB_STATISTICS_DATA_READ'
      EXPORTING
        tableclass          = 'I'
        activ_data          = 'A'
      TABLES
        dbstat              = lt_dbstatam
      EXCEPTIONS
        input_inconsistence = 1
        no_data_found       = 2
        OTHERS              = 3.
    CASE sy-subrc.
      WHEN 0.
        LOOP AT lt_dbstatam INTO wa_dbstatam.
          READ TABLE gt_error ASSIGNING <error>
              WITH KEY db_table = wa_dbstatam-tname BINARY SEARCH.
          <error>-source_size = wa_dbstatam-occtb + wa_dbstatam-occix.
        ENDLOOP.
      WHEN OTHERS.
        "continue...
    ENDCASE.
  ENDIF.

  READ TABLE gt_error WITH KEY return_code = -1
                               source_size = 0
                               TRANSPORTING NO FIELDS.      "#EC *
  IF sy-subrc = 0.
    WRITE /.
    WRITE: / '', AT 2
    'WARNING! Some tables have no statistics.',             "#EC NOTEXT
    'Check if the sizes of tables with error code "1-" could',
                                                            "#EC NOTEXT
 ' influence the sizing result significantly.'              "#EC NOTEXT
                            RIGHT-JUSTIFIED.
  ENDIF.

  " too risky to use the FM since we might have inconsistent tables!
  SELECT tabname FROM dd03l INTO TABLE lt_lob
    FOR ALL ENTRIES IN gt_error
    WHERE tabname  = gt_error-table_name
      AND as4local = 'A'
      AND ( datatype IN ('RSTR', 'STRG' )
       OR ( datatype IN ('LRAW', 'LCHR' ) AND leng >= 5000 ) ).
                                                        "#EC CI_NOFIELD
*   Calculate/count total size of erroneous tables
  LOOP AT gt_error ASSIGNING <error>.
    IF p_db_table <> <error>-db_table."Only 1 table from cluster/pool
      p_db_table = <error>-db_table.
      READ TABLE lt_lob WITH KEY <error>-table_name
          TRANSPORTING NO FIELDS.
      IF sy-subrc = 0. " lob data
        error_size = error_size + <error>-source_size * c_hlcache.
      ELSE.
        READ TABLE gt_row_tab WITH KEY <error>-table_name
                                    TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. " Row store table
          error_size = error_size + <error>-source_size.
        ELSE. "Column Store
          error_size = error_size + <error>-source_size
                        / cs_factor.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  k2gb error_size. "DB_STATISTICS_DATA_READ returns kb!

  l_error_ratio = error_size / l_est_init * 100. " all in GB.

ENDFORM.                    "check_reliability

*&---------------------------------------------------------------------*
*&      Form  sizing_calculation
*&---------------------------------------------------------------------*
*        Perform the sizing calculations
*
*----------------------------------------------------------------------*
FORM sizing_calculation CHANGING lt_tables TYPE ty_t_hana_db_size.

  DATA:
    keyfields       TYPE ty_keyfields,
    lt_keyfields    TYPE ty_t_keyfields,
    keycount        TYPE i,
    length          TYPE ty_cs_size-size_byte,
    k_length        TYPE ty_cs_size-size_byte,
    floating_number TYPE f.

  "P => Field part of index
  "U => Unique field
  "N => Near Unique field

  "Size CS virtual attributes
  LOOP AT lt_tables ASSIGNING <item> WHERE store = 'CS'.
    CLEAR: keycount, length, keyfields, lt_keyfields[].
    LOOP AT <item>-cs_cols ASSIGNING <cs_size>
       WHERE keyflag    = abap_true.
      length = length + <cs_size>-size_byte + <cs_size>-lob_size_byte.
      keycount  = keycount + 1.
      MOVE-CORRESPONDING <cs_size> TO keyfields.
      APPEND keyfields TO lt_keyfields.
    ENDLOOP.
    CLEAR gs_cs_size.
    IF keycount > 1 AND <item>-nopk_type = abap_false
      AND <item>-table_name <> 'ACDOCA'
      AND <item>-table_name <> 'MATDOC'
      AND <item>-table_name <> 'MATDOC_EXTRACT'.
      "Table will have a $trexexternalkey$. Size it.
      gs_cs_size-column_name = '$trexexternalkey$'.
      gs_cs_size-est_ms_dict = length *
        <item>-record_count / idx_dict_comp.
      IF <item>-record_count > 0.
        floating_number = <item>-record_count *
          log( <item>-record_count  ) / log( 2 ) / 8.
        gs_cs_size-est_ms_data = floating_number.
      ENDIF.
      gs_cs_size-est_ms_index = gs_cs_size-est_ms_data.
      gs_cs_size-est_ms_misc  = size_in_misc_default.
      gs_cs_size-est_ms_main  = gs_cs_size-est_ms_data
                                    + gs_cs_size-est_ms_dict
                                    + gs_cs_size-est_ms_index
                                    + gs_cs_size-est_ms_misc.
      gs_cs_size-size_byte  = length.
      APPEND gs_cs_size TO <item>-cs_cols.
      CLEAR: gs_cs_size.
    ENDIF.

    "At least one field of the PK has high distinct.
    IF <item>-nopk_type = abap_false.
      PERFORM search_high_card USING lt_keyfields keycount.
    ENDIF.

    " Size $rowid$
    gs_cs_size-column_name  = '$rowid$'.
    gs_cs_size-est_ms_data  = 2 * <item>-record_count.
    gs_cs_size-est_ms_index = gs_cs_size-est_ms_data * rowid_index_comp.
    gs_cs_size-est_ms_misc = size_in_misc_default.
    gs_cs_size-est_ms_main = gs_cs_size-est_ms_data
                             + gs_cs_size-est_ms_dict
                             + gs_cs_size-est_ms_index
                             + gs_cs_size-est_ms_misc.
*      gs_cs_size-distinct_count  = <item>-record_count.
    APPEND gs_cs_size TO <item>-cs_cols.
    CLEAR: gs_cs_size.
    " Size $trex_udiv$
    gs_cs_size-column_name = '$trex_udiv$'.
    gs_cs_size-est_ms_misc  = <item>-record_count / 4.
    IF gs_cs_size-est_ms_misc < udiv_default
      AND <item>-record_count > 0.
      gs_cs_size-est_ms_misc = udiv_default.
    ENDIF.
    gs_cs_size-est_ms_main = gs_cs_size-est_ms_misc.
    APPEND gs_cs_size TO <item>-cs_cols.
    CLEAR: gs_cs_size.
  ENDLOOP.

  "Size CS indexes.
  PERFORM fill_index_list.
  SORT gt_index BY sqltab indexname position fieldname.
  LOOP AT gt_index ASSIGNING <index>.
    READ TABLE lt_tables ASSIGNING <item>
        WITH KEY table_name = <index>-sqltab BINARY SEARCH. "ok sorted
    IF sy-subrc = 0 AND <item>-record_count > 0 AND <item>-store = 'CS'.
      IF ( <index>-indexname <> pr_indexname
                  AND <index>-sqltab = pr_sqltab )
                   OR <index>-sqltab <> pr_sqltab.
        pr_indexname = <index>-indexname.
        pr_sqltab = <index>-sqltab.
        CLEAR: gs_cs_size, keycount, lt_keyfields[], keyfields.
        gs_cs_size-record_count = <item>-record_count.
        " calculate the length of the index.
        LOOP AT gt_index ASSIGNING <idx>
            WHERE sqltab    = <index>-sqltab
              AND indexname = <index>-indexname.
          READ TABLE <item>-cs_cols ASSIGNING <cs_size>
            WITH KEY column_name = <idx>-fieldname.
          gs_cs_size-size_byte = gs_cs_size-size_byte
                                     + <cs_size>-size_byte.
          gs_cs_size-lob_size_byte = gs_cs_size-lob_size_byte
                                     + <cs_size>-lob_size_byte.
          keycount  = keycount + 1.
          IF <cs_size>-has_index IS INITIAL.
            <cs_size>-has_index = 'P'.
          ENDIF.
          MOVE-CORRESPONDING <cs_size> TO keyfields.
          APPEND keyfields TO lt_keyfields.
        ENDLOOP.
        IF keycount > 1. "only create $uc_* if more than 1 field.
          IF <index>-uniqueflag = abap_true. "unique keys
            IF strlen( <item>-table_name ) = 15.
              CONCATENATE '$uc_' <index>-sqltab '~'
                                 <index>-indexname(2) '$'
                  INTO gs_cs_size-column_name.
            ELSEIF strlen( <item>-table_name ) = 16.
              CONCATENATE '$uc_' <index>-sqltab
                                 <index>-indexname(2) '$'
                  INTO gs_cs_size-column_name.
            ELSE.
              CONCATENATE '$uc_' <index>-sqltab '~'
                                 <index>-indexname '$'
                  INTO gs_cs_size-column_name.
            ENDIF.
            IF <item>-record_count > 0.
              gs_cs_size-est_ms_dict = ( gs_cs_size-size_byte +
                gs_cs_size-lob_size_byte ) *
                  <item>-record_count / idx_dict_comp.
              floating_number = <item>-record_count *
                log( <item>-record_count  ) / log( 2 ) / 8.
              gs_cs_size-est_ms_data = floating_number.
              gs_cs_size-est_ms_index = gs_cs_size-est_ms_data.
            ENDIF.
            gs_cs_size-est_ms_misc = size_in_misc_default.
            gs_cs_size-est_ms_main  = gs_cs_size-est_ms_data
                                    + gs_cs_size-est_ms_dict
                                    + gs_cs_size-est_ms_index
                                    + gs_cs_size-est_ms_misc.
            gs_cs_size-has_index = 'U'.
            APPEND gs_cs_size TO <item>-cs_cols.
            "At least one field of the sec. unique key has high card.
            PERFORM search_high_card USING lt_keyfields keycount.
          ELSE. "Search for non-unique indexes with more than one field
            "Calibration mode only. (Not tables moved from RS to CS)
            IF sy-dbsys(3) = 'HDB' AND <item>-rs_cols IS INITIAL.
              LOOP AT gt_index ASSIGNING <nu_idx>
                  WHERE sqltab = <index>-sqltab
                        AND indexname = <index>-indexname.
                IF gs_cs_size-column_name IS INITIAL.
                  gs_cs_size-column_name = '$'.
                ENDIF.
                CONCATENATE gs_cs_size-column_name
                            <nu_idx>-fieldname '$'
                  INTO gs_cs_size-column_name.
              ENDLOOP.
              gs_cs_size-est_ms_main = gs_cs_size-size_byte *
                          <item>-record_count / cs_n_comp.
              gs_cs_size-has_index = 'P'.
              APPEND gs_cs_size TO <item>-cs_cols.
            ENDIF.
          ENDIF.
        ELSE. "estimate the entire column with an inverted index.
          IF <index>-uniqueflag = abap_true. "unique keys
            IF <item>-record_count > 0.
              "<cs_size> already assigned to the one and only column
              <cs_size>-est_ms_dict = ( <cs_size>-opt_size_byte +
                <cs_size>-lob_size_byte ) *
                  <item>-record_count / idx_dict_comp.
              floating_number = <item>-record_count *
                log( <item>-record_count  ) / log( 2 ) / 8.
              <cs_size>-est_ms_data = floating_number.
              <cs_size>-est_ms_index = <cs_size>-est_ms_data.
            ENDIF.
            <cs_size>-est_ms_misc = size_in_misc_default.
            <cs_size>-est_ms_main  = <cs_size>-est_ms_data
                                            + <cs_size>-est_ms_dict
                                            + <cs_size>-est_ms_index
                                            + <cs_size>-est_ms_misc.
            <cs_size>-has_index = 'U'.
          ELSE. "non-unique indexes with one field.
            IF <cs_size>-has_index <> 'U'. "2 fields w. client PK opt.
              <cs_size>-est_ms_main = <cs_size>-size_byte *
                       <item>-record_count / cs_n_comp.
              <cs_size>-has_index = 'N'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  " Size the rest of the CS
  LOOP AT lt_tables ASSIGNING <item> WHERE store = 'CS'.
*    WHERE column_name(1) <> '$'.
    LOOP AT <item>-cs_cols ASSIGNING <cs_size>.
      "Only do the estimation if not done already.
      IF <cs_size>-est_ms_main IS INITIAL.
        IF <cs_size>-lob_size_byte > 100 AND "Big In-memory LOBS.
           <cs_size>-size_byte = 0 AND
           <cs_size>-dlob_count   = 0.
          floating_number = <item>-record_count *
                  log( <item>-record_count  ) / log( 2 ) / 8.
          <cs_size>-est_ms_data = floating_number.
          <cs_size>-est_ms_dict = <item>-record_count *
              <cs_size>-lob_size_byte / lob_dict_comp.
          <cs_size>-est_ms_misc = size_in_misc_default.
          <cs_size>-est_ms_main  = <cs_size>-est_ms_data
                                    + <cs_size>-est_ms_dict
                                    + <cs_size>-est_ms_index
                                    + <cs_size>-est_ms_misc.
        ELSEIF <cs_size>-dlob_count  > 0.  "Disk-based LOBs.
          "First size the in-memory part.
          floating_number = <item>-record_count *
                  log( <item>-record_count  ) / log( 2 ) / 8.
          <cs_size>-est_ms_data = floating_number.
          <cs_size>-est_ms_dict = <cs_size>-mlob_count *
              <cs_size>-mlob_size_byte / lob_dict_comp.
          <cs_size>-est_ms_misc = size_in_misc_default.
          <cs_size>-est_ms_main  = <cs_size>-est_ms_data
                                    + <cs_size>-est_ms_dict
                                    + <cs_size>-est_ms_index
                                    + <cs_size>-est_ms_misc.
*        cs_mlob_size = cs_mlob_size + <cs_size>-est_ms_main.
          " rest LOB is added to HL. (chunks of 4kb)
          <cs_size>-est_ms_hl = <cs_size>-dlob_count
                          * <cs_size>-dlob_size_byte.
          est_cshl = est_cshl + <cs_size>-est_ms_hl.
        ELSEIF <cs_size>-lob_size_byte < 100 AND
               <cs_size>-lob_size_byte > 0. "Small in-memoryLOBs
          <cs_size>-est_ms_main = <cs_size>-lob_size_byte  *
                                  <item>-record_count / cs_comp.
        ELSE. " regular columns
          "Optimization V55. Use optimal size for regular col.
          "Only here as RS and concatenated column do not benefit
          IF <cs_size>-low_distinct IS NOT INITIAL "fixed values
            OR <cs_size>-opt_size_byte IS INITIAL "low distincts
            OR <cs_size>-size_byte IS INITIAL. "low distincts
            IF <cs_size>-opt_size_byte IS INITIAL
              OR <cs_size>-size_byte IS INITIAL.
              <cs_size>-low_distinct = 1. "empty
            ENDIF.
            " IF client, low_dist = 1.1.
            " IF empty col, low_dist = 1.
            " IF fixed val = 2.
            floating_number = <item>-record_count *
                    log( <cs_size>-low_distinct ) / log( 2 ) / 8.
            <cs_size>-est_ms_data = floating_number.
            <cs_size>-est_ms_dict =
                <cs_size>-low_distinct * <cs_size>-size_byte.
            <cs_size>-est_ms_misc = size_in_misc_default.
            <cs_size>-est_ms_main  = <cs_size>-est_ms_data
                                      + <cs_size>-est_ms_dict
                                      + <cs_size>-est_ms_index
                                      + <cs_size>-est_ms_misc.
          ELSE. "large number of distincts
            IF <cs_size>-keyflag = abap_true
                OR <cs_size>-has_index = 'P'.
              <cs_size>-est_ms_main = <cs_size>-size_byte *
                                    <item>-record_count / cs_p_comp.
            ELSE.
              <cs_size>-est_ms_main = <cs_size>-size_byte *
                                  <item>-record_count / cs_comp.
            ENDIF.
          ENDIF.
        ENDIF.
        IF <cs_size>-est_ms_main < size_in_misc_default. "still initial
          <cs_size>-est_ms_main = size_in_misc_default.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  " Add indexes of the RS
  CLEAR: pr_sqltab, pr_indexname, gs_rs_size.
  LOOP AT lt_tables ASSIGNING <item> WHERE store = 'RS'.
    " add Primary Key
    CLEAR: k_length.
    LOOP AT <item>-rs_cols ASSIGNING <rs_size>
                          WHERE keyflag = abap_true.
      k_length = k_length
               + <rs_size>-size_byte
               + <rs_size>-lob_size_byte.
    ENDLOOP.
    gs_rs_size-column_name  = '_SYS_TREE_RS_'.
    gs_rs_size-size_byte = k_length.
    APPEND gs_rs_size TO <item>-rs_cols.
    " add secondary indexes
    LOOP AT gt_index ASSIGNING <index>
       WHERE sqltab = <item>-table_name.
      tabix = sy-tabix.
      IF ( pr_indexname <> <index>-indexname AND
          pr_sqltab = <index>-sqltab ) OR pr_sqltab <> <index>-sqltab.
        pr_indexname = <index>-indexname.
        pr_sqltab = <index>-sqltab.
        CLEAR k_length.
        LOOP AT gt_index ASSIGNING <idx>
          FROM tabix WHERE sqltab = <index>-sqltab
                        AND indexname = <index>-indexname.
          READ TABLE <item>-rs_cols ASSIGNING <rs_size>
             WITH KEY column_name = <idx>-fieldname.
          IF sy-subrc = 0.
            k_length = k_length + <rs_size>-size_byte +
                       <rs_size>-lob_size_byte.
          ENDIF.
        ENDLOOP.
        CLEAR gs_rs_size.
        IF strlen( <index>-sqltab ) = 15.
          CONCATENATE <index>-sqltab '~' <index>-indexname(2)
              INTO gs_rs_size-column_name.
        ELSEIF strlen( <index>-sqltab ) = 16.
          CONCATENATE <index>-sqltab '~' <index>-indexname(1)
              INTO gs_rs_size-column_name.
        ELSE.
          CONCATENATE <index>-sqltab '~' <index>-indexname
              INTO gs_rs_size-column_name.
        ENDIF.
        gs_rs_size-size_byte = k_length.
        APPEND gs_rs_size TO <item>-rs_cols.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  " Do the RS sizing
  LOOP AT lt_tables ASSIGNING <item> WHERE store = 'RS'.
    LOOP AT <item>-rs_cols ASSIGNING <rs_size>.
      IF <rs_size>-column_name = '_SYS_TREE_RS_'. " primary key
        <rs_size>-est_ms_total = <item>-record_count
          * ( <rs_size>-size_byte + <rs_size>-lob_size_byte )
          / rs_pk_comp.
        IF <rs_size>-est_ms_total < pk_min_size.
          <rs_size>-est_ms_total = pk_min_size.
        ENDIF.
      ELSEIF <rs_size>-column_name CS '~'. " secondary key
        <rs_size>-est_ms_total = <item>-record_count *
         ( <rs_size>-size_byte + <rs_size>-lob_size_byte ) / rs_sk_comp.
      ELSE. " other columns
        IF <rs_size>-lob_size_byte = 0."Non-LOB
          <rs_size>-est_ms_total = <item>-record_count
            * <rs_size>-size_byte / rs_comp.
        ELSEIF <rs_size>-dlob_size_byte > 0."HL.
          <rs_size>-est_ms_total = <rs_size>-mlob_count
                                 * <rs_size>-mlob_size_byte.
          <rs_size>-est_ms_hl = <rs_size>-dlob_count
                                 * <rs_size>-dlob_size_byte.
          est_rshl = est_rshl + <rs_size>-est_ms_hl.
        ELSEIF <rs_size>-lob_size_byte > 0 AND "In-memory LOB.
               <rs_size>-dlob_size_byte = 0.
          <rs_size>-est_ms_total = <item>-record_count
                                 * <rs_size>-lob_size_byte.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " sizing_calculation

*&---------------------------------------------------------------------*
*&      Form  CALCULATE_SUBTOTALS
*&---------------------------------------------------------------------*
*       Calculate subtotals from gt_tables
*----------------------------------------------------------------------*

FORM calculate_subtotals.

  MESSAGE s090(sada) WITH 'Calculate subtotals'.            "#EC NOTEXT
  LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
    LOOP AT <item>-cs_cols ASSIGNING <cs_size>.
      <item>-est_size  = <item>-est_size  + <cs_size>-est_ms_main.
      <item>-real_size = <item>-real_size + <cs_size>-ms_main.
      <item>-est_hl_size  = <item>-est_hl_size + <cs_size>-est_ms_hl.
      <item>-real_hl_size = <item>-real_hl_size + <cs_size>-ms_hl.
      IF <cs_size>-column_name(1) <> '$'
          AND <cs_size>-column_name(1) <> '_'.
        cs_col = cs_col + <cs_size>-ms_main.
        cshl   = cshl   + <cs_size>-ms_hl.
        est_cs_col = est_cs_col + <cs_size>-est_ms_main.
        IF <cs_size>-low_distinct IS INITIAL.
          CASE <cs_size>-has_index.
            WHEN 'U'.
              est_cs_hcard  = est_cs_hcard  + <cs_size>-est_ms_main.
              cs_hcard = cs_hcard + <cs_size>-ms_main.
            WHEN 'N'.
              est_cs_mcard  = est_cs_mcard  + <cs_size>-est_ms_main.
              cs_mcard = cs_mcard + <cs_size>-ms_main.
            WHEN '' OR 'P'.
              est_cs_ucard  = est_cs_ucard  + <cs_size>-est_ms_main.
              cs_ucard = cs_ucard + <cs_size>-ms_main.
          ENDCASE.
        ELSE.
          est_cs_lcard  = est_cs_lcard  + <cs_size>-est_ms_main.
          cs_lcard = cs_lcard + <cs_size>-ms_main.
        ENDIF.
      ELSEIF <cs_size>-column_name = '$trexexternalkey$'.
        cs_pk  = cs_pk + <cs_size>-ms_main.
        est_cs_pk  = est_cs_pk + <cs_size>-est_ms_main.
        <item>-real_pk_size = <item>-real_pk_size + <cs_size>-ms_main.
        <item>-est_pk_size = <item>-est_pk_size + <cs_size>-est_ms_main.
      ELSEIF <cs_size>-column_name = '$rowid$'.
        cs_rid = cs_rid + <cs_size>-ms_main.
        est_cs_rid = est_cs_rid + <cs_size>-est_ms_main.
      ELSEIF <cs_size>-column_name = '$trex_udiv$'.
        cs_udiv = cs_udiv + <cs_size>-ms_main.
        est_cs_udiv = est_cs_udiv + <cs_size>-est_ms_main.
      ELSEIF <cs_size>-column_name(4) = '$uc_'. "unique key
        cs_uk = cs_uk + <cs_size>-ms_main.
        est_cs_uk = est_cs_uk + <cs_size>-est_ms_main.
      ELSEIF <cs_size>-column_name(13) = '$_SYS_SHADOW_'
          OR <cs_size>-column_name(5) = '$esh:'. "esh
        cs_ft = cs_ft + <cs_size>-ms_main.
        <item>-real_esh_size = <item>-real_esh_size + <cs_size>-ms_main.
      ELSE.
        IF <cs_size>-column_name(1) = '$'.
          IF <cs_size>-has_index = 'P'. "non unique key
            <item>-real_nuk_size =
                <item>-real_nuk_size + <cs_size>-ms_main.
            <item>-est_nuk_size =
                <item>-est_nuk_size + <cs_size>-est_ms_main.
            cs_nuk = cs_nuk + <cs_size>-ms_main.
            est_cs_nuk = est_cs_nuk + <cs_size>-est_ms_main.
          ELSE. "join engine attribute
            cs_jec = cs_jec + <cs_size>-ms_main.
            <item>-real_jec_size = <item>-real_jec_size +
                                    <cs_size>-ms_main.
            "total size should not contain jec size.
            <item>-real_size = <item>-real_size - <cs_size>-ms_main.
          ENDIF.
        ELSEIF <cs_size>-column_name(1) <> '_'. "aging column
          cs_aging = cs_aging + <cs_size>-ms_main.
        ENDIF.
      ENDIF.
    ENDLOOP.
    "Table is in RS but estimated in CS. Only on HANA in S4 scenario.
    LOOP AT <item>-rs_cols ASSIGNING <rs_size>.
      <item>-real_rs_exsize = <item>-real_rs_exsize +
                       <rs_size>-fixed_size + <rs_size>-variable_size.
      <item>-real_hlrs_exsize =
                       <item>-real_hlrs_exsize + <rs_size>-ms_hl.
      est_rs2cs = est_rs2cs + <item>-real_rs_exsize.
    ENDLOOP.
  ENDLOOP.

  LOOP AT gt_tables ASSIGNING <item> WHERE store = 'RS'.
    LOOP AT <item>-rs_cols ASSIGNING <rs_size>.
      <item>-est_size  = <item>-est_size  + <rs_size>-est_ms_total.
      <item>-real_size = <item>-real_size + <rs_size>-fixed_size +
                                            <rs_size>-variable_size.
      <item>-est_hl_size  = <item>-est_hl_size + <rs_size>-est_ms_hl.
      <item>-real_hl_size = <item>-real_hl_size + <rs_size>-ms_hl.
      IF <rs_size>-column_name(13) = '_SYS_TREE_RS_'. "PK
        rs_pk     = rs_pk + <rs_size>-fixed_size.
        est_rs_pk = est_rs_pk + <rs_size>-est_ms_total.
      ELSEIF <rs_size>-column_name CS '~'. "SK
        rs_sk     = rs_sk   + <rs_size>-fixed_size.
        est_rs_sk = est_rs_sk + <rs_size>-est_ms_total.
      ELSEIF <rs_size>-column_name IS INITIAL.
        rs_col = rs_col + <rs_size>-fixed_size +
                          <rs_size>-variable_size.
        rshl   = rshl   + <rs_size>-ms_hl.
      ELSE.
        est_rs_col = est_rs_col + <rs_size>-est_ms_total.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  " The column store size must be calculated before data
  " model changes will change the subtotals.
  cs_k           = cs_pk      + cs_rid     + cs_udiv     + cs_uk +
                   cs_nuk     + cs_ft.
  est_cs_k       = est_cs_pk  + est_cs_rid + est_cs_udiv + est_cs_uk +
                   est_cs_nuk.

  "Data model changes
  IF do_sfin = abap_true OR in_sfin = abap_true.
    PERFORM sfin_sizing.
  ENDIF.
  IF do_slog = abap_true OR in_slog = abap_true.
    PERFORM slog_sizing.
    IF do_sml = abap_true OR in_sml = abap_true.
      PERFORM sml_sizing.
    ENDIF.
  ENDIF.

  "estimate aging (contains sort on gt_tables)
  PERFORM get_dvm_info.
  SORT gt_tables BY table_name.

  "size in-place S/4 Migration
  IF do_slog = abap_true AND p_ful = abap_true.
    PERFORM fill_inplace_list USING gt_inplace.
    LOOP AT gt_inplace ASSIGNING <upgrade>.
      est_updata  = est_updata  + <upgrade>-size.
      est_upchrec = est_upchrec + <upgrade>-pksize / 100.
      est_updisk  = est_updisk  + <upgrade>-disksize.
    ENDLOOP.
  ENDIF.
  "Shadow instance sizing
  PERFORM fill_shadow_list USING gt_shadow.
  LOOP AT gt_shadow ASSIGNING <upgrade>.
    est_sha  = est_sha  + <upgrade>-size.
    est_shadisk  = est_shadisk  + <upgrade>-disksize.
  ENDLOOP.

  PERFORM upd_opt_size.
  PERFORM calculate_scaleout_group.
  IF in_slog = abap_true OR do_slog = abap_true. "only S/4 HANA
    PERFORM calculate_business_data CHANGING est_cl_lis.
  ENDIF.

  "disk subtotal on anydb - new sorts on gt_tables.
  PERFORM get_topcs USING 'EST_SIZE' est_twocs.
  PERFORM get_topcs USING 'REAL_SIZE' twocs.
  PERFORM get_topcs USING 'OPT_SIZE' opt_twocs.

  IF p_calib = abap_true.
    PERFORM calculate_subtotals_calib.
  ENDIF.

  cs_total       = cs_col     + cs_k + cs_aging.
  est_cs_total   = est_cs_col + est_cs_k.
  rs_total       = rs_col     + rs_pk     + rs_sk.
  est_rs_total   = est_rs_col + est_rs_pk + est_rs_sk.
  hl             = cshl       + rshl.
  est_hl         = est_cshl   + est_rshl.

  "Initial data is the data size on day 1 after S/4 upgrade
  "Size of new data (columns + tables)
  est_log_chgvbfa = est_log_newvbfa - est_log_oldvbfa.
  est_slognew    = est_slogtab + est_slogcol + est_log_chgvbfa.
  est_sfinnew    = est_sfintab + est_sfincol.

  init_data      = rs_total   + cs_total
                 + est_sfinnew  + est_slognew + est_mlnew.
  est_init_data  = est_rs_total + est_cs_total
                 + est_sfinnew  + est_slognew + est_mlnew.
  "initial disk requirement
  est_initdisk   = est_init_data  + est_hl + diskadmin + est_twocs.
  initdisk       = init_data      + hl     + diskadmin + twocs.

  "initial temporary memory requirements.
  hlch          = hl * c_hlcache.
  est_hlch      = est_hl * c_hlcache.
  init_ws       = init_data.
  est_init_ws   = est_init_data.

  "Initial Sizing
  init_size = init_data + init_ws + hlch + codestack.
  est_init_size = est_init_data + est_init_ws + est_hlch + codestack.

  "Persistent memory
  IF p_hdb2 = abap_true.
    est_init_pers = est_init_data - est_rs_total.
    est_init_dram = est_rs_total + est_hlch + codestack + est_init_ws.
    init_pers = init_data - rs_total.
    init_dram = rs_total + hlch + codestack + init_ws.
  ENDIF.

  "Cleaning potential
  est_obs_fin = est_sfincold  + est_fin_unl + est_delcoep
              + est_pca + est_sl.
  est_obs_log = est_log_unl + est_log_les.

  "Data size in-memory after clean up.
  est_opt_data = est_init_data - est_obs_fin - est_obs_log - est_aged
                 - est_cs_nuk - est_obs_ml.
  opt_data     = init_data     - est_obs_fin - est_obs_log - est_aged
                 - cs_nuk     - est_obs_ml.

  est_opt_ws = est_opt_data.
  opt_ws     = opt_data.

  est_agedch = est_aged * 20 / 100.

  est_opt_size = est_opt_data + est_agedch + est_opt_ws
               + est_hlch     + codestack.
  opt_size     = opt_data     + est_agedch + opt_ws
               + hlch         + codestack.

  "Persistent memory
  IF p_hdb2 = abap_true OR sy-dbsys(3) = 'HDB'.
    est_opt_pers = est_opt_data - est_rs_total.
    est_opt_dram = est_rs_total + est_agedch + est_opt_ws + hlch
                 + codestack.
    opt_pers = opt_data - rs_total.
    opt_dram = rs_total + est_agedch + opt_ws + hlch + codestack.
  ENDIF.

  "disk after cleanup
  est_ageddisk = est_aged * disk_comp_lost. "aged data on disk.
  est_optdisk  = est_opt_data + est_hl + diskadmin + opt_twocs +
                 est_ageddisk.
  optdisk      = opt_data + hl + diskadmin + opt_twocs
                + est_ageddisk.

  "standard conversion
  est_shatotal = est_sha * 2.
  "inplace upgrade
  est_updisk = est_updisk + est_upchrec + est_shadisk.
  est_uptotal = ( est_updata + est_upchrec ) * 2 + est_shatotal.

  DESCRIBE TABLE gt_tables LINES wa_techd-success_cnt.
  DESCRIBE TABLE gt_error LINES wa_techd-error_cnt.
  DESCRIBE TABLE gt_partial LINES wa_techd-partial_cnt.
  wa_techd-success_cnt = wa_techd-success_cnt - wa_techd-error_cnt.

  LOOP AT gt_tables ASSIGNING <item>.
    CASE <item>-doubt_reason.
      WHEN 1.
        ADD 1 TO wa_techd-count_1.
      WHEN 2.
        ADD 1 TO wa_techd-count_2.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  PERFORM lc_sizing CHANGING est_lc wa_techd-lcerror.
  IF sy-dbsys(3) <> 'HDB'.
    PERFORM check_indx USING est_init_data CHANGING est_indx.
  ENDIF.

ENDFORM.                    " CALCULATE_SUBTOTALS


*&---------------------------------------------------------------------*
*&      Form  FILL_INDEX_LIST
*&---------------------------------------------------------------------*
*       Look up indexes for the RS and unique indexes of CS.
*       Fills gt_index.
*----------------------------------------------------------------------*

FORM fill_index_list.

  IF gt_tables IS NOT INITIAL.
    SELECT b~sqltab b~indexname b~fieldname b~position a~uniqueflag
    INTO TABLE gt_index FROM dd12l AS a
         JOIN dd17s AS b
           ON b~sqltab    = a~sqltab
          AND b~indexname = a~indexname
    FOR ALL ENTRIES IN gt_tables
    WHERE a~sqltab    = gt_tables-table_name
    AND   a~as4local  = 'A'
    AND  ( ( dbsyssel1 = '' AND dbsyssel2 = '' AND dbsyssel3 = ''
             AND dbsyssel4 = ''  AND dbstate = '' )
     OR  ( dbstate = 'D' AND dbinclexcl = 'I' AND ( dbsyssel1 = 'HDB'
           OR dbsyssel2 = 'HDB' OR dbsyssel3 = 'HDB'
           OR dbsyssel4 = 'HDB' ) )
     OR  ( dbstate = 'D' AND dbinclexcl = 'E' AND NOT
          ( dbsyssel1 = 'HDB' OR dbsyssel2 = 'HDB'
           OR dbsyssel3 = 'HDB' OR dbsyssel4 = 'HDB' ) )
    ).
  ENDIF.

ENDFORM.                    "fill_index_list
*&---------------------------------------------------------------------*
*&      Form  WRITE_DB
*&---------------------------------------------------------------------*
*       Write down results in database if structures exist
*----------------------------------------------------------------------*

FORM write_to_db USING      outmode
                            "totals
                            l_cs_total l_rs_total l_hl l_hlch
                            l_disk l_init_data l_init_ws l_init_size
                            l_twocs
                            "Additional components
                            l_indx l_lc l_s4_tr
                            "Simple Finance
                            l_sfincold l_coldsfinch l_sfintab l_sfincol
                            l_sfinnew l_fin_unl l_obs_fin
                            l_pca l_delcoep l_sl
                            "Simple Logistics
                            l_slogtab l_slogcol
                            l_log_chgvbfa l_log_oldvbfa l_log_newvbfa
                            l_slognew
                            l_log_unl l_log_les l_obs_log
                            l_rs2cs
                            "Material ledger actual costing
                            l_mlnew l_obs_ml
                            "aging
                            l_agedch l_aged l_ageddisk
                            l_elaging l_elarch
                            "S/4 optimization
                            l_opt_data l_opt_ws l_optdisk l_opt_size
                            l_opt_twocs
                            "Persistent memory
                            l_init_pers l_init_dram
                            l_opt_pers l_opt_dram
                            "details
                            l_cs_col l_cs_mlob l_cs_k   l_cs_pk
                            l_cs_hcard l_cs_mcard
                            l_cs_lcard l_cs_ucard
                            l_cs_rid l_cs_udiv l_cs_uk  l_cs_nuk
                            l_rs_col l_rs_mlob l_rs_pk  l_rs_sk
                            l_cshl   l_rshl
                            l_cs_jec l_cs_aging l_cs_ft
                            "in-place upgrade
                            l_updisk l_updata l_upchrec l_uptotal
                            l_shadisk l_sha l_shatotal
                            "technical details
                            l_wa_techd TYPE ty_techd
                            "business data
                            l_cl_lis
                            "errors
                            l_error_ratio.

  CONSTANTS:
    c_sumtable TYPE tabname VALUE '/SDF/HDBSIZING',
    c_cstable  TYPE tabname VALUE '/SDF/CSSIZING',
    c_rstable  TYPE tabname VALUE '/SDF/RSSIZING',
    c_tabtable TYPE tabname VALUE '/SDF/HDBTABSIZES',
    c_indx     TYPE tabname VALUE '/SDF/INDX'.
  DATA:
    wa_dref            TYPE REF TO data,
    tab_dref           TYPE REF TO data,
    rs_table           TYPE tabname,
    cs_table           TYPE tabname,
    sum_table          TYPE tabname,
    tab_table          TYPE tabname,
    lv_sizing_guid     TYPE guid_16,
    lv_sizing_char(22) TYPE c,
    prog               TYPE string,
    tab                TYPE STANDARD TABLE OF string.
  FIELD-SYMBOLS:
    <t_rs>    TYPE STANDARD TABLE,
    <t_cs>    TYPE STANDARD TABLE,
    <t_tab>   TYPE STANDARD TABLE,
    <wa_rs>   TYPE any,
    <wa_cs>   TYPE any,
    <wa_sum>  TYPE any,
    <wa_tab>  TYPE any,
    <wa_comp> TYPE any.

  DEFINE mc_add_sub.
    wa_subtotals-name  = &1. wa_subtotals-value = &2.
    APPEND wa_subtotals TO gt_subtotals.
  END-OF-DEFINITION.

  MESSAGE s090(sada) WITH 'Writing results to database'.    "#EC NOTEXT
  "fill in gt_subtotals - aligned with read_selected_sizing
  mc_add_sub 'l_cs_total' l_cs_total.
  mc_add_sub 'l_rs_total' l_rs_total.
  mc_add_sub 'l_hl' l_hl.
  mc_add_sub 'l_hlch' l_hlch.
  mc_add_sub 'l_disk' l_disk.
  mc_add_sub 'l_init_data' l_init_data.
  mc_add_sub 'l_init_ws' l_init_ws.
  mc_add_sub 'l_init_size' l_init_size.
  mc_add_sub 'l_twocs' l_twocs.
  mc_add_sub 'l_indx' l_indx.
  mc_add_sub 'l_lc' l_lc.
  mc_add_sub 'l_s4_tr' l_s4_tr.
  mc_add_sub 'l_sfincold' l_sfincold.
  mc_add_sub 'l_coldsfinch' l_coldsfinch.
  mc_add_sub 'l_sfintab' l_sfintab.
  mc_add_sub 'l_sfincol' l_sfincol.
  mc_add_sub 'l_sfinnew' l_sfinnew.
  mc_add_sub 'l_fin_unl' l_fin_unl.
  mc_add_sub 'l_obs_fin' l_obs_fin.
  mc_add_sub 'l_pca' l_pca.
  mc_add_sub 'l_delcoep' l_delcoep.
  mc_add_sub 'l_sl' l_sl.
  mc_add_sub 'l_slogtab' l_slogtab.
  mc_add_sub 'l_slogcol' l_slogcol.
  mc_add_sub 'l_log_chgvbfa' l_log_chgvbfa.
  mc_add_sub 'l_log_oldvbfa' l_log_oldvbfa.
  mc_add_sub 'l_log_newvbfa' l_log_newvbfa.
  mc_add_sub 'l_slognew' l_slognew.
  mc_add_sub 'l_log_unl' l_log_unl.
  mc_add_sub 'l_log_les' l_log_les.
  mc_add_sub 'l_obs_log' l_obs_log.
  mc_add_sub 'l_agedch' l_agedch.
  mc_add_sub 'l_aged' l_aged.
  mc_add_sub 'l_elaging' l_elaging.
  mc_add_sub 'l_elarch' l_elarch.
  mc_add_sub 'l_opt_data' l_opt_data.
  mc_add_sub 'l_opt_ws' l_opt_ws.
  mc_add_sub 'l_optdisk' l_optdisk.
  mc_add_sub 'l_opt_size' l_opt_size.
  mc_add_sub 'l_opt_twocs' l_opt_twocs.
  mc_add_sub 'l_cs_col' l_cs_col.
  mc_add_sub 'l_cs_mlob' l_cs_mlob.
  mc_add_sub 'l_cs_k' l_cs_k.
  mc_add_sub 'l_cs_pk' l_cs_pk.
  mc_add_sub 'l_cs_hcard' l_cs_hcard.
  mc_add_sub 'l_cs_mcard' l_cs_mcard.
  mc_add_sub 'l_cs_lcard' l_cs_lcard.
  mc_add_sub 'l_cs_ucard' l_cs_ucard.
  mc_add_sub 'l_cs_rid' l_cs_rid.
  mc_add_sub 'l_cs_udiv' l_cs_udiv.
  mc_add_sub 'l_cs_uk' l_cs_uk.
  mc_add_sub 'l_cs_nuk' l_cs_nuk.
  mc_add_sub 'l_rs_col' l_rs_col.
  mc_add_sub 'l_rs_mlob' l_rs_mlob.
  mc_add_sub 'l_rs_pk' l_rs_pk.
  mc_add_sub 'l_rs_sk' l_rs_sk.
  mc_add_sub 'l_cshl ' l_cshl .
  mc_add_sub 'l_rshl' l_rshl.
  mc_add_sub 'l_cs_jec' l_cs_jec.
  mc_add_sub 'l_cs_aging' l_cs_aging.
  mc_add_sub 'l_cs_ft' l_cs_ft.
  mc_add_sub 'l_updisk ' l_updisk .
  mc_add_sub 'l_updata' l_updata.
  mc_add_sub 'l_upchrec' l_upchrec.
  mc_add_sub 'l_uptotal' l_uptotal.
  mc_add_sub 'l_shadisk' l_shadisk.
  mc_add_sub 'l_sha' l_sha.
  mc_add_sub 'l_shatotal' l_shatotal.
  mc_add_sub 'codestack' codestack.
  mc_add_sub 'diskadmin' diskadmin.
  mc_add_sub 'l_ageddisk' l_ageddisk.
  mc_add_sub 'l_cl_lis' l_cl_lis.
  mc_add_sub 'l_mlnew' l_mlnew.
  mc_add_sub 'l_obs_ml' l_obs_ml.
  mc_add_sub 'l_init_pers' l_init_pers.
  mc_add_sub 'l_init_dram' l_init_dram.
  mc_add_sub 'l_opt_pers' l_opt_pers.
  mc_add_sub 'l_opt_dram' l_opt_dram.
  mc_add_sub 'l_error_ratio' l_error_ratio.
  "end of alignment with read_selected_sizing

  PERFORM create_objects_from_db  USING c_sumtable
                                  CHANGING sum_table wa_dref tab_dref.
  ASSIGN wa_dref->* TO <wa_sum>.

  IF sum_table IS NOT INITIAL.
    "Create GUID
    CONCATENATE sy-datum sy-uzeit sy-mandt jobcount
      INTO lv_sizing_char.
    lv_sizing_guid = lv_sizing_char.
    lv_sizing_char = lv_sizing_guid. "add trailing zeros
  ENDIF.

  PERFORM create_objects_from_db  USING c_cstable
                               CHANGING cs_table wa_dref tab_dref.
  ASSIGN wa_dref->* TO <wa_cs>. ASSIGN tab_dref->* TO <t_cs>.

  PERFORM create_objects_from_db  USING c_rstable
                               CHANGING rs_table wa_dref tab_dref.
  ASSIGN wa_dref->* TO <wa_rs>.ASSIGN tab_dref->* TO <t_rs>.

  PERFORM create_objects_from_db  USING c_tabtable
                                  CHANGING tab_table wa_dref tab_dref.
  ASSIGN wa_dref->* TO <wa_tab>. ASSIGN tab_dref->* TO <t_tab>.

  IF p_db = abap_true. "db mode
    LOOP AT gt_tables ASSIGNING <item>.
      IF rs_table IS NOT INITIAL.
        DELETE FROM (c_rstable) WHERE table_name = <item>-table_name.
        LOOP AT <item>-rs_cols ASSIGNING <rs_size>.
          ASSIGN COMPONENT 'TABLE_NAME'
                    OF STRUCTURE <wa_rs> TO <wa_comp>.
          <wa_comp> = <item>-table_name.
          MOVE-CORRESPONDING <rs_size> TO <wa_rs>.
          ASSIGN COMPONENT 'RECORD_COUNT'
                    OF STRUCTURE <wa_rs> TO <wa_comp>.
          <wa_comp> = <item>-record_count.
          ASSIGN COMPONENT 'SIZE_IN_BYTE'
                    OF STRUCTURE <wa_rs> TO <wa_comp>.
          IF sy-subrc = 0.
            <wa_comp> = <rs_size>-size_byte.
          ENDIF.
          ASSIGN COMPONENT 'LOB_SIZE_IN_BYTE'
                    OF STRUCTURE <wa_rs> TO <wa_comp>.
          IF sy-subrc = 0.
            <wa_comp> = <rs_size>-lob_size_byte.
          ENDIF.
          APPEND <wa_rs> TO <t_rs>.
        ENDLOOP.
      ENDIF.
      IF cs_table IS NOT INITIAL.
        DELETE FROM (c_cstable) WHERE table_name = <item>-table_name.
        LOOP AT <item>-cs_cols ASSIGNING <cs_size>.
          ASSIGN COMPONENT 'TABLE_NAME'
                    OF STRUCTURE <wa_cs> TO <wa_comp>.
          <wa_comp> = <item>-table_name.
          MOVE-CORRESPONDING <cs_size> TO <wa_cs>.
          ASSIGN COMPONENT 'COLUMN_NAME' OF STRUCTURE
                                                <wa_cs> TO <wa_comp>.
          TRANSLATE <wa_comp> TO UPPER CASE.
          ASSIGN COMPONENT 'RECORD_COUNT'
                    OF STRUCTURE <wa_cs> TO <wa_comp>.
          <wa_comp> = <item>-record_count.
          ASSIGN COMPONENT 'SIZE_IN_BYTE'
                    OF STRUCTURE <wa_cs> TO <wa_comp>.
          IF sy-subrc = 0.
            <wa_comp> = <cs_size>-size_byte.
          ENDIF.
          ASSIGN COMPONENT 'LOB_SIZE_IN_BYTE'
                    OF STRUCTURE <wa_cs> TO <wa_comp>.
          IF sy-subrc = 0.
            <wa_comp> = <cs_size>-lob_size_byte.
          ENDIF.
          APPEND <wa_cs> TO <t_cs>.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    TRY.
        IF <t_rs> IS ASSIGNED. "IF <> ZNEWHDB_SIZE
          MODIFY (c_rstable) FROM TABLE <t_rs>.
        ENDIF.
        IF <t_cs> IS ASSIGNED.
          MODIFY (c_cstable) FROM TABLE <t_cs>.
        ENDIF.
      CATCH cx_sy_dynamic_osql_semantics."tables are not existing.
    ENDTRY.
  ENDIF.

  IF sum_table IS NOT INITIAL.
    ASSIGN COMPONENT 'SIZING_GUID' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = lv_sizing_guid.
    ASSIGN COMPONENT 'CRDATE' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = sy-datum.
    ASSIGN COMPONENT 'CRTIME' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = sy-uzeit.
    ASSIGN COMPONENT 'REPORT_NAME' OF STRUCTURE <wa_sum> TO <wa_comp>.
    IF sy-batch = abap_true.
      IF jobname = c_hrcjobname.
        "Use job name for SolMan integration
        <wa_comp> = jobname.
      ENDIF.
    ENDIF.
    IF <wa_comp> IS INITIAL.
      <wa_comp> = sy-repid.
    ENDIF.
    IF gt_subtotals IS NOT INITIAL.
      CALL FUNCTION 'DD_EXIST_TABLE'
        EXPORTING
          tabname      = c_indx
          status       = 'A'
        IMPORTING
          subrc        = subrc
        EXCEPTIONS
          wrong_status = 1
          OTHERS       = 2.
      IF subrc = 0 AND sy-subrc = 0.
        "/SDF/INDX might not exist
        APPEND 'PROGRAM subpool.'           TO tab.         "#EC NOTEXT
        APPEND 'FORM WRITE_TO_INDX USING'   TO tab.         "#EC NOTEXT
        APPEND 'lt_subtotals lt_aging_runs' TO tab.         "#EC NOTEXT
        APPEND 'lt_arch_runs l_techd'       TO tab.         "#EC NOTEXT
        APPEND 'lt_sogrp lt_aging'          TO tab.         "#EC NOTEXT
        APPEND 'lt_cpu_per l_cpu_x'         TO tab.         "#EC NOTEXT
        APPEND 'l_id.'                      TO tab.         "#EC NOTEXT
        APPEND 'DELETE FROM DATABASE '      TO tab.         "#EC NOTEXT
        APPEND '/SDF/INDX(si) ID l_id.'     TO tab.         "#EC NOTEXT
        APPEND 'EXPORT subtotals = lt_subtotals' TO tab.    "#EC NOTEXT
        APPEND 'aging_runs  = lt_aging_runs'     TO tab.    "#EC NOTEXT
        APPEND 'arch_runs   = lt_arch_runs'      TO tab.    "#EC NOTEXT
        APPEND 'techd       = l_techd'           TO tab.    "#EC NOTEXT
        APPEND 'sogrps      = lt_sogrp'          TO tab.    "#EC NOTEXT
        APPEND 'aging       = lt_aging'          TO tab.    "#EC NOTEXT
        APPEND 'cpu_per     = lt_cpu_per'        TO tab.    "#EC NOTEXT
        APPEND 'cpu_max     = l_cpu_x'           TO tab.    "#EC NOTEXT
        APPEND 'TO DATABASE /SDF/INDX(si) ID l_id.' TO tab. "#EC NOTEXT
        APPEND 'ENDFORM.' TO tab.                           "#EC NOTEXT

        GENERATE SUBROUTINE POOL tab NAME prog.
*                                  MESSAGE DATA(mess) "downport
*                            SHORTDUMP-ID DATA(sid).  "downport
        IF sy-subrc = 0.
          PERFORM ('WRITE_TO_INDX') IN PROGRAM (prog) IF FOUND
              USING gt_subtotals gt_aging_runs
                    gt_arch_runs l_wa_techd
                    gt_sogrp gt_aging gt_cpu_per wa_cpu_x
                    lv_sizing_char.
        ENDIF.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'REPORT_VERSION'
                                   OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = l_version.
    ASSIGN COMPONENT 'DB_VENDOR' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = l_wa_techd-db_vendor.
    ASSIGN COMPONENT 'DB_RELEASE' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = l_wa_techd-db_release.
    ASSIGN COMPONENT 'NB_TABLES' OF STRUCTURE <wa_sum> TO <wa_comp>.
    IF realsize_cnt IS NOT INITIAL.
      <wa_comp> = realsize_cnt.
    ELSE.
      <wa_comp> = l_wa_techd-success_cnt.
    ENDIF.
    ASSIGN COMPONENT 'NB_ERRORS' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = l_wa_techd-error_cnt.
    IF gt_error IS NOT INITIAL.
      ASSIGN COMPONENT 'ERROR_LIST' OF STRUCTURE <wa_sum> TO <wa_comp>.
      EXPORT p1 = gt_error TO DATA BUFFER <wa_comp> COMPRESSION ON.
    ENDIF.
    ASSIGN COMPONENT 'ANYDB_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
    <wa_comp> = wa_techd-anydbsize_n.
    IF gt_nso IS NOT INITIAL.
      ASSIGN COMPONENT 'SIZING_VARIANT'
                                  OF STRUCTURE <wa_sum> TO <wa_comp>.
      EXPORT p1 = gt_nso TO DATA BUFFER <wa_comp> COMPRESSION ON.
    ENDIF.
    IF sy-dbsys(3) = 'HDB'.
      ASSIGN COMPONENT 'RS_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = rs_total.
      ASSIGN COMPONENT 'CS_SIZE_MASTER'
                                 OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = cs_total.
      ASSIGN COMPONENT 'RS_HL_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = rshl.
      ASSIGN COMPONENT 'CS_HL_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = cshl.
      ASSIGN COMPONENT 'DISK_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = initdisk.
      ASSIGN COMPONENT 'TOTAL_SIZING1'
                                  OF STRUCTURE <wa_sum> TO  <wa_comp>.
      <wa_comp> = init_size.
      ASSIGN COMPONENT 'TOTAL_SIZING3'
                                  OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = opt_size.
    ELSE.
      ASSIGN COMPONENT 'RS_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = est_rs_total.
      ASSIGN COMPONENT 'CS_SIZE_MASTER'
                                 OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = est_cs_total.
      ASSIGN COMPONENT 'RS_HL_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = est_rshl.
      ASSIGN COMPONENT 'CS_HL_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = est_cshl.
      ASSIGN COMPONENT 'DISK_SIZE' OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = est_initdisk.
      ASSIGN COMPONENT 'TOTAL_SIZING1' OF STRUCTURE <wa_sum> TO
        <wa_comp>.
      <wa_comp> = est_init_size.
      ASSIGN COMPONENT 'TOTAL_SIZING3'
                                  OF STRUCTURE <wa_sum> TO <wa_comp>.
      <wa_comp> = est_opt_size.
    ENDIF.
    INSERT (c_sumtable) FROM <wa_sum>.
  ENDIF.

  IF tab_table IS NOT INITIAL.
    LOOP AT gt_tables ASSIGNING <item>.
      "move table_name, record_count, records_sampled, tab_type.
      MOVE-CORRESPONDING <item> TO <wa_tab>.
      ASSIGN COMPONENT 'SIZING_GUID' OF STRUCTURE <wa_tab> TO <wa_comp>.
      <wa_comp> = lv_sizing_guid.
      "src_db_size, abap_size not given.
      ASSIGN COMPONENT 'STORE_TYPE' OF STRUCTURE <wa_tab> TO <wa_comp>.
      <wa_comp> = <item>-store.
      IF sy-dbsys(3) = 'HDB'.
        ASSIGN COMPONENT 'DATA_SIZE' OF STRUCTURE <wa_tab> TO <wa_comp>.
        <wa_comp> = <item>-real_size.
        ASSIGN COMPONENT 'LOB_SIZE' OF STRUCTURE <wa_tab> TO <wa_comp>.
        <wa_comp> = <item>-real_hl_size.
      ELSE.
        ASSIGN COMPONENT 'DATA_SIZE' OF STRUCTURE <wa_tab> TO <wa_comp>.
        <wa_comp> = <item>-est_size.
        ASSIGN COMPONENT 'LOB_SIZE' OF STRUCTURE <wa_tab> TO <wa_comp>.
        <wa_comp> = <item>-est_hl_size.
      ENDIF.
      ASSIGN COMPONENT 'OPT_SIZE' OF STRUCTURE <wa_tab> TO <wa_comp>.
      IF sy-subrc = 0. "new field. Does not exist in all SP.
        <wa_comp> = <item>-opt_size.
      ENDIF.
      APPEND <wa_tab> TO <t_tab>.
    ENDLOOP.
    INSERT (c_tabtable) FROM TABLE <t_tab>.
  ENDIF.

ENDFORM.                    " WRITE_DB
*&---------------------------------------------------------------------*
*&      Form  create_objects_from_db
*&---------------------------------------------------------------------*

FORM create_objects_from_db USING c_object
                         CHANGING object wa_dref tab_dref.

  FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

  CALL FUNCTION 'DD_EXIST_TABLE'
    EXPORTING
      tabname      = c_object
      status       = 'A'
    IMPORTING
      subrc        = subrc
    EXCEPTIONS
      wrong_status = 1
      OTHERS       = 2.
  IF subrc = 0 AND sy-subrc = 0.
    TRY.
        object = c_object.
        CREATE DATA tab_dref TYPE STANDARD TABLE OF (object).
        ASSIGN tab_dref->* TO <table>.
        CREATE DATA wa_dref LIKE LINE OF <table>.
      CATCH cx_sy_create_data_error.
        CLEAR object.
    ENDTRY.
  ELSE.
    CLEAR object.
  ENDIF.
ENDFORM.                    " create_objects_from_db
*&---------------------------------------------------------------------*
*&      Form  slog_sizing
*&---------------------------------------------------------------------*
*       Simple Logistic sizing
*----------------------------------------------------------------------*

FORM slog_sizing .

  DATA: ls_slog         TYPE ty_ssol,
        floating_number TYPE f,
        l_colsize       TYPE ty_ssol-ssize,
        lt_checkcol     TYPE ty_t_checkcol,
        ls_sfin2        TYPE ty_ssol.
  FIELD-SYMBOLS:
    <chk>    TYPE ty_checkcol,
    <slog_t> TYPE ty_ssol.
  CONSTANTS:
    avg_acdoca_ext TYPE i VALUE '40'.

  LOOP AT gt_tables ASSIGNING <item>.
    CASE <item>-table_name.
      WHEN 'MKPF' OR 'MSEG'
        "historical stock data
        OR 'MARCH' OR 'MARDH' OR 'MCHBH' OR 'MKOLH'
        OR 'MSKAH' OR 'MSKUH' OR 'MSLBH' OR 'MSPRH' OR 'MSSAH'
        OR 'MSSQH' OR 'MSTBH' OR 'MSTEH' OR 'MSTQH'
        "aggregates
        OR 'MSSA' OR 'MSSL' OR 'MSSQ' OR 'MSTB' OR 'MSTE' OR 'MSTQ'
        OR 'MKOL' OR 'MSKA' OR 'MSKU' OR 'MSLB' OR 'MSPR'
        OR 'MARC' OR 'MARD' OR 'MCHB'
        "SD
        OR 'VBUK' OR 'VBUP'
        OR 'VAPMA' OR 'VAKPA' OR 'VLPMA' OR 'VLKPA' OR 'VRPMA'
        OR 'VRKPA'
        OR 'VBOX'.
        ls_slog-group = 'UNLOAD'.
      WHEN 'VBAK' OR 'LIKP' OR 'VBRK' OR 'VBAP' OR 'LIPS' OR 'VBEP'.
        ls_slog-group = 'NEWCOL'. "more columns
      WHEN 'MBEW' OR 'OBEW' OR 'QBEW' OR 'EBEW' OR 'MBEWH' OR 'OBEWH'
        OR 'QBEWH' OR 'EBEWH'.  "less columns
        ls_slog-group = 'LESS'.
      WHEN 'VBFA'.
        ls_slog-group = <item>-table_name.
      WHEN 'KONV'.
        ls_slog-group = <item>-table_name.
    ENDCASE.
    IF ls_slog-group IS NOT INITIAL.
      MOVE-CORRESPONDING <item> TO ls_slog.
      IF sy-dbsys(3) = 'HDB'.
        ls_slog-size = <item>-real_size.
      ELSE.
        ls_slog-size = <item>-est_size.
      ENDIF.
      APPEND ls_slog TO gt_slog. CLEAR ls_slog.
    ENDIF.
  ENDLOOP.

  IF do_slog IS NOT INITIAL.
    "MATDOC
    LOOP AT gt_slog ASSIGNING <slog> WHERE table_name = 'MKPF' OR
                                           table_name = 'MSEG'.
      ls_slog-ssize = ls_slog-ssize + ( <slog>-size * 120 / 100 ).
      IF <slog>-table_name = 'MSEG'.
        ls_slog-record_count = <slog>-record_count.
      ENDIF.
    ENDLOOP.
    IF ls_slog-ssize IS NOT INITIAL.
      ls_slog-table_name = 'MATDOC'.
      ls_slog-group = 'TAB'.
      PERFORM add2gt_tables USING ls_slog.
      APPEND ls_slog TO gt_slog.
      ls_slog-ssize = ls_slog-ssize * 20 / 100.
      ls_slog-table_name = 'MATDOC_EXTRACT'.
      ls_slog-group = 'TAB'.
      PERFORM add2gt_tables USING ls_slog.
      APPEND ls_slog TO gt_slog. CLEAR ls_slog.
    ENDIF.

    "PRCD_ELEMENTS
    READ TABLE gt_tables ASSIGNING <item> WITH KEY table_name = 'KONV'.
    IF sy-subrc = 0.
      "We consider KONV = PRCD_ELEMENTS
      <item>-table_name = 'PRCD_ELEMENTS'.
      SORT gt_tables BY table_name.
      "conversion space
      IF <item>-record_count >  max_partition_rc.
        IF sy-dbsys(3) = 'HDB'.
          est_s4_trans = 2 * <item>-real_size /
             ( <item>-record_count / max_partition_rc ).
        ELSE.
          est_s4_trans = 2 * <item>-est_size /
                       ( <item>-record_count / max_partition_rc ).
        ENDIF.
      ELSE.
        IF sy-dbsys(3) = 'HDB'.
          est_s4_trans = 2 * <item>-real_size.
        ELSE.
          est_s4_trans = 2 * <item>-est_size.
        ENDIF.
      ENDIF.
    ENDIF.

    "VBFA
    READ TABLE gt_tables ASSIGNING <item> WITH KEY table_name = 'VBFA'.
    IF sy-subrc = 0.
      "get rid of stufe > 1 records.
      READ TABLE <item>-cs_cols ASSIGNING <cs_size>
        WITH KEY column_name = 'STUFE'.
      IF sy-subrc = 0 AND <item>-record_count > 0.
        "recalculate table size in gt_table
        <item>-real_size  = <item>-real_size - <item>-real_size
         * <cs_size>-spec_count / <item>-record_count.
        <item>-real_pk_size = <item>-real_pk_size - <item>-real_pk_size
         * <cs_size>-spec_count / <item>-record_count.
        <item>-est_size  = <item>-est_size - <item>-est_size
         * <cs_size>-spec_count / <item>-record_count.
        <item>-est_pk_size = <item>-est_pk_size - <item>-est_pk_size
         * <cs_size>-spec_count / <item>-record_count.
        "adapt record count.
        <item>-record_count =
            <item>-record_count - <cs_size>-spec_count.
      ENDIF.
      "Add RAW16 unique guid.
      CLEAR gs_cs_size.
      gs_cs_size-est_ms_dict =
            16 * <item>-record_count / idx_dict_comp.
      IF <item>-record_count > 0.
        floating_number = <item>-record_count *
          log( <item>-record_count  ) / log( 2 ) / 8.
        gs_cs_size-est_ms_data = floating_number.
      ENDIF.
      gs_cs_size-est_ms_index = gs_cs_size-est_ms_data.
      gs_cs_size-est_ms_misc  = size_in_misc_default.
      gs_cs_size-est_ms_main  = gs_cs_size-est_ms_data
                                    + gs_cs_size-est_ms_dict
                                    + gs_cs_size-est_ms_index
                                    + gs_cs_size-est_ms_misc.
      gs_cs_size-column_name = 'RUUID'.
      "add RUUID to gt_tables and update totals and subtotals.
      APPEND gs_cs_size TO <item>-cs_cols.
      IF sy-dbsys(3) = 'HDB'.
        <item>-real_size =
            <item>-real_size + gs_cs_size-est_ms_main.
      ELSE.
        <item>-est_size =
            <item>-est_size + gs_cs_size-est_ms_main.
      ENDIF.
      "recalculate primary key size (Client+RUUID)
      CLEAR gs_cs_size.
      gs_cs_size-est_ms_dict =
            24 * <item>-record_count / idx_dict_comp.
      IF <item>-record_count > 0.
        floating_number = <item>-record_count *
          log( <item>-record_count  ) / log( 2 ) / 8.
        gs_cs_size-est_ms_data = floating_number.
      ENDIF.
      gs_cs_size-est_ms_index = gs_cs_size-est_ms_data.
      gs_cs_size-est_ms_misc  = size_in_misc_default.
      gs_cs_size-est_ms_main  = gs_cs_size-est_ms_data
                                    + gs_cs_size-est_ms_dict
                                    + gs_cs_size-est_ms_index
                                    + gs_cs_size-est_ms_misc.
      gs_cs_size-column_name = 'new_$trexexternalkey$'.
      APPEND gs_cs_size TO <item>-cs_cols.
      IF sy-dbsys(3) = 'HDB'.
        <item>-real_size = <item>-real_size
                         - <item>-real_pk_size "old
                         + gs_cs_size-est_ms_main. "new
        <item>-real_pk_size = gs_cs_size-est_ms_main.
      ELSE.
        <item>-est_size = <item>-est_size
                        - <item>-est_pk_size "old
                        + gs_cs_size-est_ms_main. "new
        <item>-est_pk_size = gs_cs_size-est_ms_main.
      ENDIF.
      CLEAR gs_cs_size.
      READ TABLE gt_slog ASSIGNING <slog>
        WITH KEY table_name = <item>-table_name.
      IF sy-dbsys(3) = 'HDB'.
        <slog>-ssize = <item>-real_size.
      ELSE.
        <slog>-ssize = <item>-est_size.
      ENDIF.
    ENDIF.

    "ACDOCA_M_EXTRACT (fin relevant but only in S4)
    LOOP AT gt_tables ASSIGNING <item>.
      CASE <item>-table_name.
        WHEN 'MARD' OR 'MCHB' OR 'MKOL' OR 'MSKA' OR 'MSKU' OR 'MSLB'
          OR 'MSPR' OR 'MSSA' OR 'MSSQ' OR 'MSSL' OR 'MSTB' OR 'MSTE'
          OR 'MSTQ'.
          ls_sfin2-record_count =
                        ls_sfin2-record_count + <item>-record_count.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    ls_sfin2-table_name = 'ACDOCA_M_EXTRACT'.               "#EC NOTEXT
    ls_sfin2-group = 'Lower estimation'.                    "#EC NOTEXT
    ls_sfin2-ssize = ls_sfin2-record_count * avg_acdoca_ext.
    APPEND ls_sfin2 TO gt_sfin2.
    ls_sfin2-group = 'Upper estimation'.                    "#EC NOTEXT
    ls_sfin2-ssize = ls_sfin2-record_count * avg_acdoca_ext * 5.
    PERFORM add2gt_tables USING ls_sfin2.
    est_sfintab = est_sfintab + ls_sfin2-ssize.
    APPEND ls_sfin2 TO gt_sfin2. CLEAR ls_sfin2.

    "Columns changed.
    PERFORM fill_slog_col TABLES lt_checkcol.
    LOOP AT lt_checkcol ASSIGNING <chk>.
      READ TABLE gt_slog ASSIGNING <slog>
              WITH KEY table_name = <chk>-src.
      IF sy-subrc = 0.
        READ TABLE <slog>-cs_cols ASSIGNING <cs_size>
          WITH KEY column_name = <chk>-col.
        IF sy-subrc = 0.
          IF <chk>-tgt IS NOT INITIAL. "add col from src to tgt
            READ TABLE gt_slog ASSIGNING <slog_t>
                WITH KEY table_name = <chk>-tgt.
            IF sy-subrc = 0.
              READ TABLE gt_tables ASSIGNING <item>
                WITH KEY table_name = <chk>-tgt BINARY SEARCH.
              IF sy-dbsys(3) = 'HDB'.
                "check this formula.
                l_colsize = <cs_size>-ms_misc + <cs_size>-ms_dict
                  + ( <cs_size>-ms_index + <cs_size>-ms_data )
                  * ( <slog_t>-record_count / <slog>-record_count ).
                <slog_t>-ssize = <slog_t>-ssize + l_colsize.
                <item>-real_size = <item>-real_size + l_colsize.
              ELSE.
                l_colsize = <cs_size>-est_ms_main
                   * ( <slog_t>-record_count / <slog>-record_count ).
                <slog_t>-ssize = <slog_t>-ssize + l_colsize.
                <item>-est_size = <item>-est_size + l_colsize.
              ENDIF.
            ENDIF.
          ELSE. "take out columns.
            READ TABLE gt_tables ASSIGNING <item>
                WITH KEY table_name = <chk>-src BINARY SEARCH.
            IF sy-dbsys(3) = 'HDB'.
              <slog>-ssize = <slog>-ssize + <cs_size>-ms_main.
              <item>-real_size = <item>-real_size - <cs_size>-ms_main.
            ELSE.
              <slog>-ssize = <slog>-ssize + <cs_size>-est_ms_main.
              <item>-est_size = <item>-est_size - <item>-est_size.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT gt_slog ASSIGNING <slog>.
    CASE <slog>-group.
      WHEN 'UNLOAD'.
        est_log_unl = est_log_unl + <slog>-size.
      WHEN 'NEWCOL'.
        est_slogcol = est_slogcol + <slog>-ssize.
      WHEN 'LESS'.
        est_log_les = est_log_les - <slog>-ssize.
      WHEN 'TAB'.
        est_slogtab = est_slogtab +  <slog>-ssize.
      WHEN 'VBFA'.
        IF do_slog IS NOT INITIAL.
          est_log_oldvbfa = <slog>-size.
          est_log_newvbfa = <slog>-ssize.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  SORT gt_slog BY size DESCENDING.
ENDFORM.                    " slog_sizing

*&---------------------------------------------------------------------*
*&      Form  get_slog_col
*&---------------------------------------------------------------------*
*       Create list of columns changed by slog
*----------------------------------------------------------------------*

FORM fill_slog_col  TABLES lt_checkcol TYPE ty_t_checkcol.
  DATA ls_checkcol LIKE LINE OF lt_checkcol.

  DEFINE mc_add.
    ls_checkcol-col = &1.
    ls_checkcol-src = &2.
    ls_checkcol-tgt = &3.
    APPEND ls_checkcol TO lt_checkcol.
  END-OF-DEFINITION.

  mc_add 'ABSTK' 'VBUK' 'VBAK'.
  mc_add 'COSTA' 'VBUK' 'VBAK'.
  mc_add 'DCSTK' 'VBUK' 'VBAK'.
  mc_add 'FKSAK' 'VBUK' 'VBAK'.
  mc_add 'FMSTK' 'VBUK' 'VBAK'.
  mc_add 'FSSTK' 'VBUK' 'VBAK'.
  mc_add 'LFGSK' 'VBUK' 'VBAK'.
  mc_add 'LFSTK' 'VBUK' 'VBAK'.
  mc_add 'LSSTK' 'VBUK' 'VBAK'.
  mc_add 'MANEK' 'VBUK' 'VBAK'.
  mc_add 'RFGSK' 'VBUK' 'VBAK'.
  mc_add 'RFSTK' 'VBUK' 'VBAK'.
  mc_add 'BESTK' 'VBUK' 'VBAK'.
  mc_add 'CMPSC' 'VBUK' 'VBAK'.
  mc_add 'CMPSD' 'VBUK' 'VBAK'.
  mc_add 'CMPS_CM' 'VBUK' 'VBAK'.
  mc_add 'CMPS_TE' 'VBUK' 'VBAK'.
  mc_add 'CMGST' 'VBUK' 'VBAK'.
  mc_add 'RRSTA' 'VBUK' 'VBAK'.
  mc_add 'SPSTG' 'VBUK' 'VBAK'.
  mc_add 'TRSTA' 'VBUK' 'VBAK'.
  mc_add 'UVALL' 'VBUK' 'VBAK'.
  mc_add 'UVFAK' 'VBUK' 'VBAK'.
  mc_add 'UVFAS' 'VBUK' 'VBAK'.
  mc_add 'UVVLK' 'VBUK' 'VBAK'.
  mc_add 'UVVLS' 'VBUK' 'VBAK'.
  mc_add 'GBSTK' 'VBUK' 'VBAK'.
  mc_add 'UVALS' 'VBUK' 'VBAK'.
  mc_add 'UVPRS' 'VBUK' 'VBAK'.

  mc_add 'FKIVK' 'VBUK' 'LIKP'.
  mc_add 'FKSTK' 'VBUK' 'LIKP'.
  mc_add 'HDALL' 'VBUK' 'LIKP'.
  mc_add 'HDALS' 'VBUK' 'LIKP'.
  mc_add 'KOQUK' 'VBUK' 'LIKP'.
  mc_add 'KOSTK' 'VBUK' 'LIKP'.
  mc_add 'LVSTK' 'VBUK' 'LIKP'.
  mc_add 'PDSTK' 'VBUK' 'LIKP'.
  mc_add 'PKSTK' 'VBUK' 'LIKP'.
  mc_add 'SPE_TMPID' 'VBUK' 'LIKP'.
  mc_add 'UVPAK' 'VBUK' 'LIKP'.
  mc_add 'UVPAS' 'VBUK' 'LIKP'.
  mc_add 'UVPIK' 'VBUK' 'LIKP'.
  mc_add 'UVPIS' 'VBUK' 'LIKP'.
  mc_add 'UVWAK' 'VBUK' 'LIKP'.
  mc_add 'UVWAS' 'VBUK' 'LIKP'.
  mc_add 'VESTK' 'VBUK' 'LIKP'.
  mc_add 'VLSTK' 'VBUK' 'LIKP'.
  mc_add 'WBSTK' 'VBUK' 'LIKP'.
  mc_add 'BESTK' 'VBUK' 'LIKP'.
  mc_add 'CMPSC' 'VBUK' 'LIKP'.
  mc_add 'CMPSD' 'VBUK' 'LIKP'.
  mc_add 'CMPS_CM' 'VBUK' 'LIKP'.
  mc_add 'CMPS_TE' 'VBUK' 'LIKP'.
  mc_add 'CMGST' 'VBUK' 'LIKP'.
  mc_add 'RRSTA' 'VBUK' 'LIKP'.
  mc_add 'SPSTG' 'VBUK' 'LIKP'.
  mc_add 'TRSTA' 'VBUK' 'LIKP'.
  mc_add 'UVALL' 'VBUK' 'LIKP'.
  mc_add 'UVFAK' 'VBUK' 'LIKP'.
  mc_add 'UVFAS' 'VBUK' 'LIKP'.
  mc_add 'UVVLK' 'VBUK' 'LIKP'.
  mc_add 'UVVLS' 'VBUK' 'LIKP'.
  mc_add 'GBSTK' 'VBUK' 'LIKP'.
  mc_add 'UVALS' 'VBUK' 'LIKP'.

  mc_add 'GBSTK' 'VBUK' 'VBRK'.
  mc_add 'UVALS' 'VBUK' 'VBRK'.
  mc_add 'UVPRS' 'VBUK' 'VBRK'.
  mc_add 'BUCHK' 'VBUK' 'VBRK'.
  mc_add 'RELIK' 'VBUK' 'VBRK'.

  mc_add 'ABSTA' 'VBUP' 'VBAP'.
  mc_add 'COSTA' 'VBUP' 'VBAP'.
  mc_add 'DCSTA' 'VBUP' 'VBAP'.
  mc_add 'FKSAA' 'VBUP' 'VBAP'.
  mc_add 'FSSTA' 'VBUP' 'VBAP'.
  mc_add 'LFGSA' 'VBUP' 'VBAP'.
  mc_add 'LFSTA' 'VBUP' 'VBAP'.
  mc_add 'LSSTA' 'VBUP' 'VBAP'.
  mc_add 'MANEK' 'VBUP' 'VBAP'.
  mc_add 'RFGSA' 'VBUP' 'VBAP'.
  mc_add 'RFSTA' 'VBUP' 'VBAP'.
  mc_add 'RRSTA' 'VBUP' 'VBAP'.
  mc_add 'UVPRS' 'VBUP' 'VBAP'.
  mc_add 'BESTA' 'VBUP' 'VBAP'.
  mc_add 'CMPPI' 'VBUP' 'VBAP'.
  mc_add 'CMPPJ' 'VBUP' 'VBAP'.
  mc_add 'GBSTA' 'VBUP' 'VBAP'.
  mc_add 'UVALL' 'VBUP' 'VBAP'.
  mc_add 'UVFAK' 'VBUP' 'VBAP'.
  mc_add 'UVVLK' 'VBUP' 'VBAP'.

  mc_add 'FKIVP' 'VBUK' 'LIPS'.
  mc_add 'FKSTA' 'VBUK' 'LIPS'.
  mc_add 'HDALL' 'VBUK' 'LIPS'.
  mc_add 'KOQUA' 'VBUK' 'LIPS'.
  mc_add 'KOSTA' 'VBUK' 'LIPS'.
  mc_add 'LVSTA' 'VBUK' 'LIPS'.
  mc_add 'PDSTA' 'VBUK' 'LIPS'.
  mc_add 'PKSTA' 'VBUK' 'LIPS'.
  mc_add 'RRSTA' 'VBUK' 'LIPS'.
  mc_add 'UVPAK' 'VBUK' 'LIPS'.
  mc_add 'UVPIK' 'VBUK' 'LIPS'.
  mc_add 'UVWAK' 'VBUK' 'LIPS'.
  mc_add 'VLSTP' 'VBUK' 'LIPS'.
  mc_add 'WBSTA' 'VBUK' 'LIPS'.
  mc_add 'BESTA' 'VBUK' 'LIPS'.
  mc_add 'CMPPI' 'VBUK' 'LIPS'.
  mc_add 'CMPPJ' 'VBUK' 'LIPS'.
  mc_add 'GBSTA' 'VBUK' 'LIPS'.
  mc_add 'UVALL' 'VBUK' 'LIPS'.
  mc_add 'UVFAK' 'VBUK' 'LIPS'.
  mc_add 'UVVLK' 'VBUK' 'LIPS'.

  mc_add 'SALK3' 'MBEW' ''.
  mc_add 'SALKV' 'MBEW' ''.
  mc_add 'TIMESTAMP' 'MBEW' ''.
  mc_add 'VJKUM' 'MBEW' ''.
  mc_add 'VJSAL' 'MBEW' ''.
  mc_add 'VJSAV' 'MBEW' ''.
  mc_add 'VKSAL' 'MBEW' ''.
  mc_add 'VMKUM' 'MBEW' ''.
  mc_add 'VMSAL' 'MBEW' ''.
  mc_add 'VMSAV' 'MBEW' ''.
  mc_add 'VVJLB' 'MBEW' ''.
  mc_add 'VVJSL' 'MBEW' ''.
  mc_add 'VVMLB' 'MBEW' ''.
  mc_add 'VVSAL' 'MBEW' ''.

  mc_add 'SALK3' 'OBEW' ''.
  mc_add 'SALKV' 'OBEW' ''.
  mc_add 'TIMESTAMP' 'OBEW' ''.
  mc_add 'VJKUM' 'OBEW' ''.
  mc_add 'VJSAL' 'OBEW' ''.
  mc_add 'VJSAV' 'OBEW' ''.
  mc_add 'VKSAL' 'OBEW' ''.
  mc_add 'VMKUM' 'OBEW' ''.
  mc_add 'VMSAL' 'OBEW' ''.
  mc_add 'VMSAV' 'OBEW' ''.
  mc_add 'VVJLB' 'OBEW' ''.

  mc_add 'SALK3' 'QBEW' ''.
  mc_add 'SALKV' 'QBEW' ''.
  mc_add 'TIMESTAMP' 'QBEW' ''.
  mc_add 'VJKUM' 'QBEW' ''.
  mc_add 'VJSAL' 'QBEW' ''.
  mc_add 'VJSAV' 'QBEW' ''.
  mc_add 'VKSAL' 'QBEW' ''.
  mc_add 'VMKUM' 'QBEW' ''.
  mc_add 'VMSAL' 'QBEW' ''.
  mc_add 'VMSAV' 'QBEW' ''.
  mc_add 'VVJLB' 'QBEW' ''.

  mc_add 'SALK3' 'EBEW' ''.
  mc_add 'SALKV' 'EBEW' ''.
  mc_add 'TIMESTAMP' 'EBEW' ''.
  mc_add 'VJKUM' 'EBEW' ''.
  mc_add 'VJSAL' 'EBEW' ''.
  mc_add 'VJSAV' 'EBEW' ''.
  mc_add 'VKSAL' 'EBEW' ''.
  mc_add 'VMKUM' 'EBEW' ''.
  mc_add 'VMSAL' 'EBEW' ''.
  mc_add 'VMSAV' 'EBEW' ''.
  mc_add 'VVJLB' 'EBEW' ''.

  mc_add 'SALK3' 'MBEWH' ''.
  mc_add 'SALKV' 'MBEWH' ''.
  mc_add 'VKSAL' 'MBEWH' ''.

  mc_add 'SALK3' 'OBEWH' ''.
  mc_add 'SALKV' 'OBEWH' ''.
  mc_add 'VKSAL' 'OBEWH' ''.

  mc_add 'SALK3' 'QBEWH' ''.
  mc_add 'SALKV' 'QBEWH' ''.
  mc_add 'VKSAL' 'QBEWH' ''.

  mc_add 'SALK3' 'EBEWH' ''.
  mc_add 'SALKV' 'EBEWH' ''.
  mc_add 'VKSAL' 'EBEWH' ''.

ENDFORM.                    " get_slog_col
*&---------------------------------------------------------------------*
*&      Form  SFIN_SIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM sfin_sizing .
  DATA: ls_sfin2     TYPE ty_ssol,
        lv_newgl     TYPE dec20,
        lv_aa        TYPE dec20,
        lv_faat      TYPE dec20,
        ls_bseg      TYPE ty_ssol,
        ls_coep      TYPE ty_ssol,
        l_len        TYPE i,
        actual_ratio TYPE p LENGTH 9 DECIMALS 3,
        avg_acdoca   TYPE p LENGTH 9 DECIMALS 2.
  CONSTANTS: newgl_t881(4) VALUE 't881'.

  LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
    CASE <item>-table_name.
        "sum tables and others..
      WHEN 'GLT0'  OR 'GLT0_BCK' OR 'GLT0_DIF'
        OR 'FAGLFLEXT' OR 'FAGLFLEXT_BCK'
        OR 'KNC1' OR 'KNC3' OR 'LFC1' OR 'LFC3'
        OR 'BSIM' OR 'T012K'
        OR 'FMGLFLEXT' OR 'PSGLFLEXT' OR 'JVGLFLEXT'
        OR 'KNC1_BCK' OR 'KNC3_BCK' OR 'LFC1_BCK'
        OR 'LFC3_BCK'.
        ls_sfin2-group = 'UNLOAD'.
      WHEN 'BSID' OR 'BSIK' OR 'BSAD' OR 'BSAK' OR 'FAGLBSIS'
        OR 'FAGLBSAS' OR 'BSIS' OR 'BSAS'. "indexes tables
        ls_sfin2-group = 'INDEX'.
      WHEN  'BSEG'.
        ls_sfin2-group = 'NEWCOL'.
      WHEN 'COEP'.
        READ TABLE <item>-cs_cols ASSIGNING <cs_size>
          WITH KEY column_name = 'WRTTP'.
        IF sy-subrc = 0.
          ls_sfin2-spec_count = <cs_size>-spec_count.
        ENDIF.
        ls_sfin2-group = 'NEWCOL'.
      WHEN 'COSP' OR 'COSS' OR 'COSS_BAK' OR 'COSP_BAK'.
        ls_sfin2-group = 'OBSCO'.
      WHEN 'ANEA' OR 'ANLC' OR 'ANEP'. "Asset Acc.
        ls_sfin2-group = 'AA'.
      WHEN 'ANEK' OR 'ANLP' .
        ls_sfin2-group = 'FAAT'.
      WHEN 'GLPCA' OR 'GLPCC' OR 'GLPCO' OR 'GLPCP' OR 'GLPCT'.
        ls_sfin2-group = 'EC-PCA'.
    ENDCASE.
    IF ls_sfin2-group IS NOT INITIAL.
      MOVE-CORRESPONDING <item> TO ls_sfin2.
      IF sy-dbsys(3) = 'HDB'.
        ls_sfin2-size = <item>-real_size.
      ELSE.
        ls_sfin2-size = <item>-est_size.
      ENDIF.
      APPEND ls_sfin2 TO gt_sfin2.
      CLEAR ls_sfin2.
    ENDIF.
  ENDLOOP.


  SELECT DISTINCT tab FROM (newgl_t881) INTO ls_sfin2-table_name  "#EC CI_DB_OPERATION_OK[2431747]
    WHERE appl = 'FI' AND subappl IN ('GLF', 'PSM', 'PSX', 'JVA').
  IF ls_sfin2-table_name IS NOT INITIAL.
    l_len = strlen( ls_sfin2-table_name ) - 1.
    ls_sfin2-table_name+l_len = 'A'.
    READ TABLE gt_tables ASSIGNING <item>
      WITH KEY table_name = ls_sfin2-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <item> TO ls_sfin2.
      IF sy-dbsys(3) = 'HDB'.
        ls_sfin2-size = <item>-real_size.
      ELSE.
        ls_sfin2-size = <item>-est_size.
      ENDIF.
      ls_sfin2-group = 'NEWGL'.
      APPEND ls_sfin2 TO gt_sfin2.
    ENDIF.
  ENDIF.
  CLEAR ls_sfin2.
  ENDSELECT.

  SELECT DISTINCT tab FROM (newgl_t881) INTO ls_sfin2-table_name  "#EC CI_DB_OPERATION_OK[2431747]
    WHERE appl = 'FI' AND subappl = 'SL'.
    IF ls_sfin2-table_name IS NOT INITIAL.
      l_len = strlen( ls_sfin2-table_name ) - 1.
      ls_sfin2-table_name+l_len = 'A'.
      READ TABLE gt_tables ASSIGNING <item>
        WITH KEY table_name = ls_sfin2-table_name BINARY SEARCH.
      IF sy-subrc = 0.
        "check that table is not already in gt_sfin2
        READ TABLE gt_sfin2 WITH KEY table_name = <item>-table_name
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MOVE-CORRESPONDING <item> TO ls_sfin2.
          IF sy-dbsys(3) = 'HDB'.
            ls_sfin2-size = <item>-real_size.
          ELSE.
            ls_sfin2-size = <item>-est_size.
          ENDIF.
          ls_sfin2-group = 'FI-SL'.
          APPEND ls_sfin2 TO gt_sfin2.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR ls_sfin2.
  ENDSELECT.


  LOOP AT gt_sfin2 ASSIGNING <sfin>.
    CASE <sfin>-group.
      WHEN 'INDEX'.
        est_sfincold = est_sfincold + <sfin>-size.
        LOOP AT <sfin>-cs_cols ASSIGNING <cs_size>.
          IF sy-dbsys(3) <> 'HDB'.
            est_sfin_inv = est_sfin_inv + <cs_size>-est_ms_index
                                        + <cs_size>-est_ms_misc.
          ELSE.
            est_sfin_inv = est_sfin_inv + <cs_size>-ms_index
                                        + <cs_size>-ms_misc.
          ENDIF.
        ENDLOOP.
      WHEN 'UNLOAD'.
        est_fin_unl = est_fin_unl + <sfin>-size.
      WHEN 'NEWGL'.
        est_fin_unl = est_fin_unl + <sfin>-size.
        lv_newgl  = lv_newgl  + <sfin>-record_count.
      WHEN 'AA'.
        est_fin_unl = est_fin_unl + <sfin>-size.
        lv_aa   = lv_aa + <sfin>-record_count.
        lv_faat = lv_faat + <sfin>-size.
      WHEN 'FAAT'.
        est_fin_unl = est_fin_unl + <sfin>-size.
        lv_faat = lv_faat + <sfin>-size.
      WHEN 'EC-PCA'.
        est_pca = est_pca + <sfin>-size.
      WHEN 'FI-SL'.
        est_sl  = est_sl  + <sfin>-size.
    ENDCASE.
  ENDLOOP.
  est_sfincoldch = est_sfincold * 20 / 100 + est_sfin_inv.

  IF do_sfin = abap_true.
    READ TABLE gt_sfin2 INTO ls_bseg WITH KEY table_name = 'BSEG'.
    READ TABLE gt_sfin2 INTO ls_coep WITH KEY table_name = 'COEP'.
    IF ls_bseg-record_count > 0 OR ls_coep-record_count > 0.
      avg_acdoca = ( ls_bseg-size + ls_coep-size )
                 / ( ls_bseg-record_count +  ls_coep-record_count )
                 * 75 / 100.
    ENDIF.
    "Largest # FI/NewGL/COEP WRTTP 04 + ANEA + ANEP + ANLC
    IF ls_bseg-record_count > lv_newgl.
      IF ls_coep-spec_count > ls_bseg-record_count.
        ls_sfin2-record_count = lv_aa + ls_coep-spec_count. "AA+COEP
      ELSE.
        ls_sfin2-record_count = lv_aa + ls_bseg-record_count. "AA+BSEG
      ENDIF.
    ELSE.
      IF lv_newgl > ls_coep-spec_count.
        ls_sfin2-record_count = lv_aa + lv_newgl. "AA+NewGL
      ELSE.
        ls_sfin2-record_count = lv_aa + ls_coep-spec_count. "AA+COEP
      ENDIF.
    ENDIF.
    ls_sfin2-ssize  = avg_acdoca * ls_sfin2-record_count.
    ls_sfin2-table_name = 'ACDOCA'.                         "#EC NOTEXT
    ls_sfin2-group = 'Lower estimation'.                    "#EC NOTEXT
    APPEND ls_sfin2 TO gt_sfin2. CLEAR ls_sfin2.

    "Largest # FI/NewGL + ANEA + ANEP + ANLC + COEP WRTTP 04
    IF ls_bseg-record_count > lv_newgl.
      ls_sfin2-record_count = lv_aa + ls_coep-spec_count
                            + ls_bseg-record_count. "AA+COEP+BSEG
    ELSE.
      ls_sfin2-record_count = lv_aa + ls_coep-spec_count + lv_newgl.
      "AA+COEP+NEWGL
    ENDIF.
    ls_sfin2-table_name = 'ACDOCA'.                         "#EC NOTEXT
    ls_sfin2-group = 'Upper estimation'.                    "#EC NOTEXT
    ls_sfin2-ssize  = avg_acdoca * ls_sfin2-record_count.
    PERFORM add2gt_tables USING ls_sfin2.
    APPEND ls_sfin2 TO gt_sfin2. CLEAR ls_sfin2.

    ls_sfin2-table_name = 'FAAT_DOC_IT'.                    "#EC NOTEXT
    ls_sfin2-group = 'FI-AA'.                               "#EC NOTEXT
    ls_sfin2-ssize = lv_faat * 15 / 100.
    PERFORM add2gt_tables USING ls_sfin2.
    APPEND ls_sfin2 TO gt_sfin2. CLEAR ls_sfin2.
    ls_sfin2-table_name = 'FAAT_PLAN_VALUES'.               "#EC NOTEXT
    ls_sfin2-group = 'FI-AA'.                               "#EC NOTEXT
    ls_sfin2-ssize = lv_faat * 25 / 100.
    PERFORM add2gt_tables USING ls_sfin2.
    APPEND ls_sfin2 TO gt_sfin2. CLEAR ls_sfin2.
    ls_sfin2-table_name = 'FAAT_YDDA'.                      "#EC NOTEXT
    ls_sfin2-group = 'FI-AA'.                               "#EC NOTEXT
    ls_sfin2-ssize = lv_faat * 10 / 100.
    PERFORM add2gt_tables USING ls_sfin2.
    APPEND ls_sfin2 TO gt_sfin2. CLEAR ls_sfin2.

    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE group = 'FI-AA'
                                         OR group = 'Upper estimation'.
      est_sfintab = est_sfintab + <sfin>-ssize.
    ENDLOOP.

    READ TABLE gt_sfin2 ASSIGNING <sfin> WITH KEY table_name = 'BSEG'.
    IF sy-subrc = 0.
      <sfin>-ssize = <sfin>-size * 13 / 100.
      est_sfincol = est_sfincol + <sfin>-ssize.
      READ TABLE gt_tables ASSIGNING <item>
           WITH KEY table_name = <sfin>-table_name BINARY SEARCH.
      IF sy-dbsys(3) = 'HDB'.
        <item>-real_size = <item>-real_size + <sfin>-ssize.
      ELSE.
        <item>-est_size = <item>-est_size + <sfin>-ssize.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE gt_sfin2 ASSIGNING <sfin> WITH KEY table_name = 'COEP'.
  IF sy-subrc = 0.
    IF do_sfin = abap_true.
      <sfin>-ssize = <sfin>-size * 15 / 100.
      est_sfincol = est_sfincol + <sfin>-ssize.
      READ TABLE gt_tables ASSIGNING <item>
           WITH KEY table_name = <sfin>-table_name BINARY SEARCH.
      IF sy-dbsys(3) = 'HDB'.
        <item>-real_size = <item>-real_size + <sfin>-ssize.
      ELSE.
        <item>-est_size = <item>-est_size + <sfin>-ssize.
      ENDIF.
    ENDIF.
    actual_ratio = <sfin>-spec_count / <sfin>-record_count.
    <sfin>-osize = ( <sfin>-size + <sfin>-ssize ) * actual_ratio.
    est_delcoep = est_delcoep + <sfin>-osize.
    LOOP AT gt_sfin2 ASSIGNING <sfin> WHERE group = 'OBSCO'.
      <sfin>-osize = <sfin>-size * actual_ratio.
      est_delcoep = est_delcoep + <sfin>-osize.
    ENDLOOP.
  ENDIF.

  SORT gt_sfin2 BY size DESCENDING.
ENDFORM.                    "sfin_sizing

*&---------------------------------------------------------------------*
*&      Form  ADD2GT_TABLES
*&---------------------------------------------------------------------*
*       add new data model tables to tables list
*----------------------------------------------------------------------*

FORM add2gt_tables  USING ls_snew TYPE ty_ssol.
  READ TABLE gt_tables WITH KEY table_name = ls_snew-table_name
       TRANSPORTING NO FIELDS BINARY SEARCH.
  tabix = sy-tabix.
  IF sy-subrc = 0.
  ELSE.
    CLEAR table_item.
    MOVE-CORRESPONDING ls_snew TO table_item.
    table_item-db_table = ls_snew-table_name.
    table_item-part_id = 1.
    table_item-tab_type = 'TRANSP'.
    table_item-store    = 'CS'.
    table_item-clitab   = abap_true.
    " Do not add to subtotal as this is later added
    IF sy-dbsys(3) = 'HDB'.
      table_item-real_size = ls_snew-ssize.
    ELSE.
      table_item-est_size = ls_snew-ssize.
    ENDIF.
    INSERT table_item INTO gt_tables INDEX tabix.
  ENDIF.

ENDFORM.                    "add2gt_tables
*&---------------------------------------------------------------------*
*&      Form  UPD_OPT_SIZE
*&---------------------------------------------------------------------*
*       update optimal size in gt_tables
*----------------------------------------------------------------------*

FORM upd_opt_size.

  LOOP AT gt_tables ASSIGNING <item>.
    "delete non-unique indexes
    IF sy-dbsys(3) = 'HDB'.
      <item>-opt_size = <item>-real_size - <item>-real_nuk_size.
    ELSE.
      <item>-opt_size = <item>-est_size - <item>-est_nuk_size.
    ENDIF.
    "simple finance changes
    READ TABLE gt_sfin2 ASSIGNING <sfin>
      WITH KEY table_name = <item>-table_name.
    IF sy-subrc = 0.
      CASE <sfin>-group.
        WHEN 'NEWCOL'. "=> already added at migration
*          <item>-opt_size = <item>-opt_size + <sfin>-ssize.
          IF <item>-table_name = 'COEP'.
            <item>-opt_size = <item>-opt_size - <sfin>-osize.
          ENDIF.
        WHEN 'OBSCO'.
          <item>-opt_size = <item>-opt_size - <sfin>-osize.
        WHEN 'FI-AA'.
        WHEN OTHERS.
          IF <item>-table_name <> 'ACDOCA'.
            CLEAR <item>-opt_size.
          ENDIF.
      ENDCASE.
    ENDIF.
    "simple logistics changes
    READ TABLE gt_slog ASSIGNING <slog>
      WITH KEY table_name = <item>-table_name.
    IF sy-subrc = 0.
      CASE <slog>-group.
        WHEN 'NEWCOL'. "=> already added at migration
*          <item>-opt_size = <item>-opt_size + <slog>-ssize.
        WHEN 'VBFA' OR 'TAB'.
        WHEN OTHERS.
          CLEAR <item>-opt_size.
      ENDCASE.
    ENDIF.
    "data aging
    READ TABLE gt_aging  ASSIGNING <aging>
      WITH KEY table_name = <item>-table_name.
    IF sy-subrc = 0.
      <item>-opt_size = <item>-opt_size - <aging>-hist_size.
    ENDIF.
    "ml
    READ TABLE gt_ml ASSIGNING <ml>
      WITH KEY table_name = <item>-table_name.
    IF sy-subrc = 0.
      IF <ml>-group = 'OBS'.
        CLEAR <item>-opt_size.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "upd_opt_size

*&---------------------------------------------------------------------*
*&      Form  SEARCH_HIGH_CARD
*&---------------------------------------------------------------------*
*       Search for the column with high cardinality
*       This routine might not find it
*----------------------------------------------------------------------*

FORM search_high_card USING lt_keyfields TYPE ty_t_keyfields
                            keycount.

  DATA: floating_number TYPE f.
  FIELD-SYMBOLS: <key> LIKE LINE OF lt_keyfields.

  DEFINE mc_size_u.
    IF <item>-record_count > 0.
      <cs_size>-est_ms_dict =
        <cs_size>-opt_size_byte * <item>-record_count / idx_dict_comp.
      floating_number = <item>-record_count *
        log( <item>-record_count ) / log( 2 ) / 8.
      <cs_size>-est_ms_data  = floating_number.
    ENDIF.
    <cs_size>-est_ms_index = <cs_size>-est_ms_data.
    <cs_size>-est_ms_misc  = size_in_misc_default.
    <cs_size>-est_ms_main  = <cs_size>-est_ms_data
                                    + <cs_size>-est_ms_dict
                                    + <cs_size>-est_ms_index
                                    + <cs_size>-est_ms_misc.
  END-OF-DEFINITION.

  IF keycount = 1. "one field key.
    mc_size_u. <cs_size>-has_index = 'U'.
  ENDIF.
  IF keycount = 2.
    "2 fields in the key and 1 is client.
    READ TABLE lt_keyfields TRANSPORTING NO FIELDS
                          WITH KEY client  = abap_true.
    IF sy-subrc = 0.
      READ TABLE lt_keyfields ASSIGNING <key>
                            WITH KEY client  = abap_false.
      IF sy-subrc = 0."case if all fields are of type mandt.
        READ TABLE <item>-cs_cols ASSIGNING <cs_size>
                              WITH KEY column_name = <key>-column_name.
        mc_size_u. <cs_size>-has_index = 'U'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF keycount > 2.
    "Search for GUIDs fields
    LOOP AT lt_keyfields ASSIGNING <key>.
      IF <key>-typekind = cl_abap_typedescr=>typekind_hex.
        READ TABLE <item>-cs_cols ASSIGNING <cs_size>
                              WITH KEY column_name = <key>-column_name.
        IF <cs_size>-has_index <> 'U'.
          mc_size_u. <cs_size>-has_index = 'G'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "search_high_card

*&---------------------------------------------------------------------*
*&      Form fill_inplace_list
*&---------------------------------------------------------------------*
*       collect list of cloned tables
*----------------------------------------------------------------------*

FORM fill_inplace_list USING lt_inplace TYPE ty_t_upgrade.
  DATA: ls_inplace TYPE ty_upgrade,
        l_len      TYPE i.
  CONSTANTS: newgl_t800a(5) VALUE 't800a'.

  DEFINE mc_add.
    ls_inplace-table_name = &1.
    READ TABLE gt_tables ASSIGNING <item>
              WITH KEY table_name = ls_inplace-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      IF sy-dbsys(3) = 'HDB'.
        ls_inplace-size = <item>-real_size.
        ls_inplace-pksize = <item>-real_pk_size.
        ls_inplace-disksize = <item>-real_size + <item>-real_hl_size.
      ELSE.
        ls_inplace-size = <item>-est_size.
        ls_inplace-pksize = <item>-est_pk_size.
        ls_inplace-disksize = <item>-est_size + <item>-est_hl_size.
      ENDIF.
      ls_inplace-record_count = <item>-record_count.
      APPEND ls_inplace TO lt_inplace.
    ENDIF.
  END-OF-DEFINITION.

  IF in_sfin = abap_false.
    TRY.
        SELECT DISTINCT tab FROM (newgl_t800a)
          INTO ls_inplace-table_name
          WHERE appl = 'FI' AND glflex = 'X' AND inactive = space
           AND ( subappl = 'GLF' OR subappl = 'PSM'
              OR subappl = 'PSX' OR subappl = 'JVA' ).
          mc_add ls_inplace-table_name.
        ENDSELECT.
      CATCH cx_sy_dynamic_osql_semantics.
    ENDTRY.
    SELECT tabname FROM dd02l INTO ls_inplace-table_name
      WHERE as4local = 'A' AND tabname LIKE 'CE4%'.
      l_len = strlen( ls_inplace-table_name ).
      IF l_len = 7.
        mc_add ls_inplace-table_name.
      ENDIF.
    ENDSELECT.
  ENDIF.

*mc_add 'ACDOCA'.
  mc_add 'AFKO'. mc_add 'AFPO'. mc_add 'AFVC'. mc_add 'AFVU'.
  mc_add 'ANEA'. mc_add 'ANEK'. mc_add 'ANEP'. mc_add 'ANLA'.
  mc_add 'ANLB'. mc_add 'ANLBZA'. mc_add 'ANLC'. mc_add 'ANLH'.
  mc_add 'ANLP'. mc_add 'ANLU'. mc_add 'ANLV'. mc_add 'ANLZ'.
  mc_add 'APERB_ITEMS'. mc_add 'APERB_PROT'. mc_add 'AUFK'.
  mc_add 'BKPF'. mc_add 'BSAD'. mc_add 'BSAK'. mc_add 'BSAS'.
  mc_add 'BSE_OIH'. mc_add 'BSEG'. mc_add 'BSEG_ADD'. mc_add 'BSID'.
  mc_add 'BSIK'. mc_add 'BSIM'. mc_add 'BSIS'. mc_add 'CBPR'.
  mc_add 'CEPC'. mc_add 'CKMI1'. mc_add 'CKMLAB02'. mc_add 'CKMLCR'.
  mc_add 'CKMLCT'. mc_add 'CKMLCUR'. mc_add 'CKMLHD'. mc_add 'CKMLPP'.
  mc_add 'CKMLV'. mc_add 'CKPHS'. mc_add 'CKPHT'. mc_add 'CKPST'.
  mc_add 'COBK'. mc_add 'COEP'. mc_add 'COKEY'. mc_add 'COSP'.
  mc_add 'COSS'. mc_add 'CSKA'. mc_add 'CSKB'. mc_add 'CSKS'.
  mc_add 'CSKU'. mc_add 'EBEW'. mc_add 'EWUCUR'.
*mc_add 'FAAT_PLAN_VALUES'. *mc_add 'FAAT_YDDA'. "mc_add 'FAAT_DOC_IT'.
  mc_add 'FAGL_ACTIVEC'. mc_add 'FAGL_BCF_FIELDS'.
  mc_add 'FAGL_CARRY_FORW'. mc_add 'FAGL_CHECK_SETUP'.
  mc_add 'FAGL_CUS_GENINFO'. mc_add 'FAGL_CUS_GENLEVL'.
  mc_add 'FAGL_CUST_FIELDS'. mc_add 'FAGL_FCBAL_FIELD'.
  mc_add 'FAGL_FIELD_MOVEC'. mc_add 'FAGL_LEDGER_SCEN'.
  mc_add 'FAGL_MIG_001'. mc_add 'FAGL_MIG_001_S'. mc_add 'FAGL_MIG_002'.
  mc_add 'FAGL_MIG_BUKRS'. mc_add 'FAGL_OPT_FIELDS'.
  mc_add 'FAGL_ORG_INFO_CT'.  mc_add 'FAGL_SAP_GENLEVL'.
  mc_add 'FAGL_SCEN_FIELDS'. mc_add 'FAGL_SETTINGS'.
  mc_add 'FAGL_SLL_PLAN'. mc_add 'FAGL_SLL_STATUS'.
  mc_add 'FAGL_SPLIT_ACTC'. mc_add 'FAGL_SPLIT_DEVSY'.
  mc_add 'FAGL_SPLIT_FIELD'. mc_add 'FAGL_SPLIT_FLD_S'.
  mc_add 'FAGL_TLDGRP'. mc_add 'FAGL_TLDGRP_MAP'. mc_add 'FAGL_TLDGRPT'.
  mc_add 'FAGLBSAS'. mc_add 'FAGLBSIS'. mc_add 'FAGLCOFICCODEC'.
  mc_add 'FAGLCOFIVARC'. mc_add 'FAGLFLEXA'. mc_add 'FAGLFLEXDF'.
  mc_add 'FAGLFLEXT'. mc_add 'FCML_MLCO'. mc_add 'FCOC_MDAT'.
  mc_add 'FCOC_TKEOE_OLD'. mc_add 'FCOT_COPAACDOCA'.
  mc_add 'FGL_MIG_BCF'. mc_add 'FGL_MIG_SOURCE'. mc_add 'FGL_SFIN_MIG'.
  mc_add 'FGLT_GLTAB'. mc_add 'FIN_AR_VIEW_FLD'.
  mc_add 'FIN_AR_VIEW_REDU'.
  mc_add 'FINS_ACCEPT_MSG'. mc_add 'FINS_ACCEPT_TOK'.
  mc_add 'FINS_ACCEPT_TRA'. mc_add 'FINS_ACDOC_ACT'.
  mc_add 'FINS_ACDOC_ACT_H'. mc_add 'FINS_BCF_FY'.
  mc_add 'FINS_BELNR_NR'. mc_add 'FINS_CO_DOCT_CC'.
  mc_add 'FINS_CO_DOCT_MAP'. mc_add 'FINS_CO_DOCT_VAR'.
  mc_add 'FINS_CO_DOCT_VRT'. mc_add 'FINS_MASS_CTRL'.
  mc_add 'FINS_MASS_CTRL_C'. mc_add 'FINS_MASSPACKAGE'.
  mc_add 'FINS_MIG_ANEK'. mc_add 'FINS_MIG_ANLC'.
  mc_add 'FINS_MIG_ANLP'. mc_add 'FINS_MIG_BSEG'.
  mc_add 'FINS_MIG_COEP'. mc_add 'FINS_MIG_CUST'.
  mc_add 'FINS_MIG_PERIO'.  mc_add 'FINS_MIG_PERIODS'.
  mc_add 'FINS_MIG_RUN'. mc_add 'FINS_MIG_RUN_ACT'.
  mc_add 'FINS_MIG_SKA1'. mc_add 'FINS_MIG_STATUS'.
  mc_add 'FINS_ML_ACCOUNT'.  mc_add 'FINSC_001A'.
  mc_add 'FINSC_ACDOC_FCT'. mc_add 'FINSC_ACTVE_APPL'.
  mc_add 'FINSC_AP_VALUTYP'. mc_add 'FINSC_CMP_VERSNC'.
  mc_add 'FINSC_CMP_VERSND'. mc_add 'FINSC_CMP_VRSNCH'.
  mc_add 'FINSC_CURTYPE'. mc_add 'FINSC_CURTYPE_C'.
  mc_add 'FINSC_CURTYPET'. mc_add 'FINSC_LD_CMP'.
  mc_add 'FINSC_LD_CMP_AP'. mc_add 'FINSC_LEDGER'.
  mc_add 'FINSC_LEDGER_REP'. mc_add 'FINSC_LEDGER_T'.
  mc_add 'FINSC_MIG_CURR'. mc_add 'FINSC_PERIOD'.
  mc_add 'FINSC_VERSN_LD'.  mc_add 'FINST_RECERR_ACD'.
  mc_add 'FINST_RECERR_CT'. mc_add 'FINST_RECERR_CTG'.
  mc_add 'FINST_RECERR_FI'. mc_add 'FINST_RECERR_GEN'.
  mc_add 'FINSTS_ACDOC_FCT'. mc_add 'FINSTS_CUSTF_FCT'.
  mc_add 'FM01'. mc_add 'FM01D'. mc_add 'FM01T'. mc_add 'FMGLFLEXA'.
  mc_add 'FMGLFLEXT'. mc_add 'FMISPS'. mc_add 'FMISPSUSER'.
  mc_add 'FMLT_CURTP_ML'. mc_add 'GB01'. mc_add 'GB31'. mc_add 'GLT0'.
  mc_add 'GLT0_DIF'. mc_add 'GLT3'. mc_add 'HDB_STATUS'.
  mc_add 'J_3RFTAX_ASSIGN'. mc_add 'JEST'. mc_add 'JVGLFLEXA'.
  mc_add 'JVGLFLEXT'. mc_add 'KNC1'. mc_add 'KNC3'. mc_add 'KNCX_DIF'.
  mc_add 'LFC1'. mc_add 'LFC3'. mc_add 'LFCX_DIF'. mc_add 'MARA'.
  mc_add 'MARC'. mc_add 'MARCH'. mc_add 'MARD'. mc_add 'MARDH'.
  mc_add 'MARV'. mc_add 'MBEW'. mc_add 'MCHB'. mc_add 'MCHBH'.
  mc_add 'MCSD'. mc_add 'MCSDH'. mc_add 'MCSS'. mc_add 'MCSSH'.
  mc_add 'MKOL'. mc_add 'MKOLH'. mc_add 'MKPF'. mc_add 'MLCD'.
  mc_add 'MLCR'. mc_add 'MLHD'. mc_add 'MLIT'. mc_add 'MLPP'.
  mc_add 'MSCD'. mc_add 'MSCDH'. mc_add 'MSCS'. mc_add 'MSEG'.
  mc_add 'MSFD'. mc_add 'MSFDH'. mc_add 'MSFS'. mc_add 'MSID'.
  mc_add 'MSIDH'. mc_add 'MSIS'. mc_add 'MSKA'. mc_add 'MSKAH'.
  mc_add 'MSKU'. mc_add 'MSKUH'. mc_add 'MSLB'. mc_add 'MSLBH'.
  mc_add 'MSPR'. mc_add 'MSPRH'. mc_add 'MSRD'. mc_add 'MSRDH'.
  mc_add 'MSRS'. mc_add 'MSSA'. mc_add 'MSSAH'. mc_add 'MSSL'.
  mc_add 'MSSQ'. mc_add 'MSSQH'. mc_add 'MSTB'. mc_add 'MSTBH'.
  mc_add 'MSTE'. mc_add 'MSTEH'. mc_add 'MSTQ'. mc_add 'MSTQH'.
  mc_add 'N2CONTROLPARA'. mc_add 'OBEW'. mc_add 'ONRAO'. mc_add 'PROJ'.
  mc_add 'PRPS'. mc_add 'PSGLFLEXA'. mc_add 'PSGLFLEXT'. mc_add 'QBEW'.
  mc_add 'RESB'. mc_add 'SCMATREED'. mc_add 'SKA1'. mc_add 'SKAT'.
  mc_add 'SKB1'. mc_add 'SMBATCH'. mc_add 'SMMAIN'. mc_add 'SMPARAM'.
  mc_add 'SMSCMAID'. mc_add 'SMSELKRIT'. mc_add 'SMWF'. mc_add 'T000C'.
  mc_add 'T001'. mc_add 'T001A'. mc_add 'T001K'. mc_add 'T001W'.
  mc_add 'T001Z'. mc_add 'T003'. mc_add 'T003T'. mc_add 'T004'.
  mc_add 'T004F'. mc_add 'T004G'. mc_add 'T004T'. mc_add 'T009'.
  mc_add 'T009B'. mc_add 'T009Y'. mc_add 'T012K'. mc_add 'T012T'.
  mc_add 'T021D'. mc_add 'T021F'. mc_add 'T021G'. mc_add 'T021Q'.
  mc_add 'T022'. mc_add 'T030'. mc_add 'T030B'. mc_add 'T030R'.
  mc_add 'T077S'. mc_add 'T077Z'. mc_add 'T090NA'. mc_add 'T090NAT'.
  mc_add 'T090NAZ'. mc_add 'T090ND'. mc_add 'T090NDT'.
  mc_add 'T090NH_METH'. mc_add 'T090NHT'. mc_add 'T090NP'.
  mc_add 'T090NPT'. mc_add 'T090NR'. mc_add 'T090NRT'. mc_add 'T090NSG'.
  mc_add 'T090NST'. mc_add 'T091'. mc_add 'T091T'. mc_add 'T093'.
  mc_add 'T093_BSN_FUNC'. mc_add 'T093_MAP_ACC'. mc_add 'T093A'.
  mc_add 'T093B'. mc_add 'T093C'. mc_add 'T093D'. mc_add 'T093G'.
  mc_add 'T093R'. mc_add 'T093S'. mc_add 'T093T'. mc_add 'T093U'.
  mc_add 'T093Y'. mc_add 'T095'. mc_add 'T095_ACI'. mc_add 'T095A'.
  mc_add 'T095A_MAP'. mc_add 'T095B'. mc_add 'T095P'. mc_add 'T095T'.
  mc_add 'T096'. mc_add 'T096T'. mc_add 'T100C'. mc_add 'T100S'.
  mc_add 'T134M'. mc_add 'T150'. mc_add 'T156'. mc_add 'T156B'.
  mc_add 'T156C'. mc_add 'T156M'. mc_add 'T156S'. mc_add 'T169P'.
  mc_add 'T258I'. mc_add 'T800A'. mc_add 'T800A_KEYFIG'. mc_add 'T800B'.
  mc_add 'T800D'. mc_add 'T800HISTH'. mc_add 'T800HISTP'.
  mc_add 'T800M'. mc_add 'T800O'. mc_add 'T80D'. mc_add 'T80I'.
  mc_add 'T811T'. mc_add 'T880'. mc_add 'T881'. mc_add 'T881_KEYFIG'.
  mc_add 'T881T'. mc_add 'T882'. mc_add 'T882C'. mc_add 'T882G'.
  mc_add 'T888'. mc_add 'T888M'. mc_add 'T895'. mc_add 'T8G30A'.
  mc_add 'T8G30B'. mc_add 'T8G40'. mc_add 'T8G50'. mc_add 'TA1TV'.
  mc_add 'TA1TVB'. mc_add 'TABAS'. mc_add 'TABW'. mc_add 'TABWG'.
  mc_add 'TABWK'. mc_add 'TACC_PRINCIPLE'. mc_add 'TACC_PRINCIPLET'.
  mc_add 'TACC_TRGT_LDGR'. mc_add 'TBSL'. mc_add 'TBSLT'.
  mc_add 'TCKM2'. mc_add 'TCKMHD'. mc_add 'TCKMIT'. mc_add 'TCOBD'.
  mc_add 'TCOBF'. mc_add 'TCOBM'. mc_add 'TCOBN'. mc_add 'TCURM'.
  mc_add 'TCVAL'. mc_add 'TCVPROF'. mc_add 'TCVPROFD'.
  mc_add 'TCVPROFT'.  mc_add 'TFAA_PC_SM'. mc_add 'TFAAD_BSN_FUNC'.
  mc_add 'TFBUF'. mc_add 'TFIN_XML_DATA'. mc_add 'TFIN_XML_TABNAME'.
  mc_add 'TISSR_AKTIV'. mc_add 'TISSR_MAB_LEDGER'. mc_add 'TJ01'.
  mc_add 'TKA00'. mc_add 'TKA01'. mc_add 'TKA02'. mc_add 'TKA07'.
  mc_add 'TKA09'. mc_add 'TKA3A'. mc_add 'TKEAE'. mc_add 'TKEB'.
  mc_add 'TKEBA'. mc_add 'TKEBB'. mc_add 'TKEBC'. mc_add 'TKEBL'.
  mc_add 'TKEBT'. mc_add 'TKECCU'. mc_add 'TKEF'. mc_add 'TKEL'.
  mc_add 'TKELT'. mc_add 'TKEOE'. mc_add 'TKEVS'. mc_add 'TKSKA'.
  mc_add 'TKT09'. mc_add 'TKVS'. mc_add 'TKVST'. mc_add 'TR01T'.
  mc_add 'TTZCU'. mc_add 'TVGA'. mc_add 'TVGAI'. mc_add 'TVGAP'.
  mc_add 'TVKO'. mc_add 'VBAK'. mc_add 'VBAP'. mc_add 'VBEP'.
  mc_add 'VBKPF'. mc_add 'VIBEAM'. mc_add 'VIBEBE'.
  mc_add 'VICAINTRENO'. mc_add 'VICN01'. mc_add 'VISCCC'.
  mc_add 'VSAFKO_CN'. mc_add 'VSAFVC_CN'. mc_add 'VSAUFK_CN'.
  mc_add 'VSPROJ_CN'. mc_add 'VSRESB_CN'.
  SORT lt_inplace BY table_name.
  DELETE ADJACENT DUPLICATES FROM lt_inplace COMPARING table_name.
  SORT lt_inplace BY size DESCENDING.

ENDFORM.                    "fill_inplace_list

*&---------------------------------------------------------------------*
*&      Form  fill_shadow_list
*&---------------------------------------------------------------------*
*       fill list of tables cloned to shadow instance.
*----------------------------------------------------------------------*

FORM fill_shadow_list USING lt_shadow TYPE ty_t_upgrade.
  DATA: ls_shadow TYPE ty_upgrade.

  DEFINE mc_add.
    ls_shadow-table_name = &1.
    READ TABLE gt_tables ASSIGNING <item>
              WITH KEY table_name = ls_shadow-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      IF sy-dbsys(3) = 'HDB'.
        ls_shadow-size = <item>-real_size.
        ls_shadow-pksize = <item>-real_pk_size.
        ls_shadow-disksize = <item>-real_size + <item>-real_hl_size.
      ELSE.
        ls_shadow-size = <item>-est_size.
        ls_shadow-pksize = <item>-est_pk_size.
        ls_shadow-disksize = <item>-est_size + <item>-est_hl_size.
      ENDIF.
      ls_shadow-record_count = <item>-record_count.
      APPEND ls_shadow TO lt_shadow.
    ENDIF.
  END-OF-DEFINITION.

  mc_add '/1BS/TEST_T_FK'. mc_add '/1BS/TEST_TABLE'.
  mc_add '/BOBF/ACF_MAP'. mc_add '/BOBF/ACT_CONF'.
  mc_add '/BOBF/ACT_LIST'. mc_add '/BOBF/ACT_LISTT'.
  mc_add '/BOBF/AUTH_OBJ'. mc_add '/BOBF/BUF_LOG'.
  mc_add '/BOBF/CONF_CHKEX'. mc_add '/BOBF/CONF_CLIMP'.
  mc_add '/BOBF/CONF_CONV'. mc_add '/BOBF/CONF_CONVT'.
  mc_add '/BOBF/CONF_DDIC'. mc_add '/BOBF/CONF_GEN'.
  mc_add '/BOBF/CONF_LIB'. mc_add '/BOBF/CONF_LIBC'.
  mc_add '/BOBF/CONF_LIBCL'. mc_add '/BOBF/CONF_LIBCT'.
  mc_add '/BOBF/CONF_LIBMR'. mc_add '/BOBF/CONF_LIBT'.
  mc_add '/BOBF/CONF_LOG'. mc_add '/BOBF/CONF_TIME'.
  mc_add '/BOBF/CONF_TIMET'. mc_add '/BOBF/CONF_UI_E'.
  mc_add '/BOBF/CONF_UI_ET'. mc_add '/BOBF/CONF_UI_G'.
  mc_add '/BOBF/CONF_UI_O'. mc_add '/BOBF/CONF_UI_OT'.
  mc_add '/BOBF/CONF_UI_T'. mc_add '/BOBF/DET_CONF'.
  mc_add '/BOBF/DET_LIST'. mc_add '/BOBF/DET_LISTT'.
  mc_add '/BOBF/DET_NET'. mc_add '/BOBF/FRW_ADDON'.
  mc_add '/BOBF/OBM_AK_FLD'. mc_add '/BOBF/OBM_ALTKET'.
  mc_add '/BOBF/OBM_ALTKEY'. mc_add '/BOBF/OBM_ASSOC'.
  mc_add '/BOBF/OBM_ASSOCB'. mc_add '/BOBF/OBM_ASSOCC'.
  mc_add '/BOBF/OBM_ASSOCT'. mc_add '/BOBF/OBM_BO'.
  mc_add '/BOBF/OBM_CODE_L'. mc_add '/BOBF/OBM_DISEL'.
  mc_add '/BOBF/OBM_FINORD'. mc_add '/BOBF/OBM_GEN_IN'.
  mc_add '/BOBF/OBM_GROUP'. mc_add '/BOBF/OBM_GROUPC'.
  mc_add '/BOBF/OBM_GROUPT'. mc_add '/BOBF/OBM_MAP'.
  mc_add '/BOBF/OBM_NCAT'. mc_add '/BOBF/OBM_NCATT'.
  mc_add '/BOBF/OBM_NODE'. mc_add '/BOBF/OBM_NODET'.
  mc_add '/BOBF/OBM_OBJ'. mc_add '/BOBF/OBM_OBJT'.
  mc_add '/BOBF/OBM_PROPTY'. mc_add '/BOBF/OBM_PRXPTY'.
  mc_add '/BOBF/OBM_QUERY'. mc_add '/BOBF/OBM_QUERYT'.
  mc_add '/BOBF/OBM_RTW'. mc_add '/BOBF/OBM_RTW_A'.
  mc_add '/BOBF/OBM_SUBSCR'. mc_add '/BOBF/OBM_VRS'.
  mc_add '/BOBF/OBM_VSET'. mc_add '/BOBF/OBM_VSETT'.
  mc_add '/BOBF/STA_DERIV'. mc_add '/BOBF/STA_DERIVT'.
  mc_add '/BOBF/STA_SCHEMA'. mc_add '/BOBF/STA_SCHEMT'.
  mc_add '/BOBF/STA_VAR'. mc_add '/BOBF/STA_VART'.
  mc_add '/BOBF/TOOL_CCCHA'. mc_add '/BOBF/TOOL_CCCHK'.
  mc_add '/BOBF/TOOL_CCCHM'. mc_add '/BOBF/TOOL_CCIFO'.
  mc_add '/BOBF/TOOL_MSGMA'. mc_add '/BOBF/TRA_PI'.
  mc_add '/BOBF/TRA_PI_ACT'. mc_add '/BOBF/TUI_QRYPAR'.
  mc_add '/BOBF/TUI_QRYVAR'. mc_add '/BOBF/TUI_SELFLD'.
  mc_add '/BOBF/TUI_SETTIN'. mc_add '/BOBF/VAL_CONF'.
  mc_add '/BOBF/VAL_LIST'. mc_add '/BOBF/VAL_LISTT'.
  mc_add '/BOBF/VAL_NET'. mc_add '/IWBEP/C_CCMS'.
  mc_add '/IWBEP/C_CCMST'. mc_add '/IWBEP/I_ANA_CON'.
  mc_add '/IWBEP/I_ANA_COT'. mc_add '/IWBEP/I_MGW_OHD'.
  mc_add '/IWBEP/I_MGW_OHT'. mc_add '/IWBEP/I_MGW_SRG'.
  mc_add '/IWBEP/I_MGW_SRH'. mc_add '/IWBEP/I_MGW_SRT'.
  mc_add '/IWBEP/I_MGW_VAA'. mc_add '/IWBEP/I_MGW_VAH'.
  mc_add '/IWBEP/I_MGW_VAT'. mc_add '/IWBEP/I_MGW_VOH'.
  mc_add '/IWBEP/I_MGW_VOT'. mc_add '/IWBEP/I_SBD_AMD'.
  mc_add '/IWBEP/I_SBD_AMT'. mc_add '/IWBEP/I_SBD_AT'.
  mc_add '/IWBEP/I_SBD_D'. mc_add '/IWBEP/I_SBD_DS'.
  mc_add '/IWBEP/I_SBD_DST'. mc_add '/IWBEP/I_SBD_DT'.
  mc_add '/IWBEP/I_SBD_GA'. mc_add '/IWBEP/I_SBD_GAT'.
  mc_add '/IWBEP/I_SBD_GP'. mc_add '/IWBEP/I_SBD_GS'.
  mc_add '/IWBEP/I_SBD_GST'. mc_add '/IWBEP/I_SBD_MA'.
  mc_add '/IWBEP/I_SBD_MAP'. mc_add '/IWBEP/I_SBD_MD'.
  mc_add '/IWBEP/I_SBD_MDT'. mc_add '/IWBEP/I_SBD_MH'.
  mc_add '/IWBEP/I_SBD_MP'. mc_add '/IWBEP/I_SBD_MR'.
  mc_add '/IWBEP/I_SBD_N'. mc_add '/IWBEP/I_SBD_NC'.
  mc_add '/IWBEP/I_SBD_NOI'. mc_add '/IWBEP/I_SBD_NR'.
  mc_add '/IWBEP/I_SBD_NT'. mc_add '/IWBEP/I_SBD_OP'.
  mc_add '/IWBEP/I_SBD_OPT'. mc_add '/IWBEP/I_SBD_P'.
  mc_add '/IWBEP/I_SBD_PR'. mc_add '/IWBEP/I_SBD_PRT'.
  mc_add '/IWBEP/I_SBD_PT'. mc_add '/IWBEP/I_SBD_SE'.
  mc_add '/IWBEP/I_SBD_SET'. mc_add '/IWBEP/I_SBD_SV'.
  mc_add '/IWBEP/I_SBD_SVT'. mc_add '/IWBEP/I_SBD_US'.
  mc_add '/IWBEP/I_SBD_VL'. mc_add '/IWBEP/I_SBD_VM'.
  mc_add '/IWBEP/I_SBD_XP'. mc_add '/IWBEP/I_SBD_XPT'.
  mc_add '/IWBEP/I_SBD_XT'. mc_add '/IWBEP/I_SBD_XTT'.
  mc_add '/IWBEP/I_SBO_AN'. mc_add '/IWBEP/I_SBO_ASO'.
  mc_add '/IWBEP/I_SBO_AST'. mc_add '/IWBEP/I_SBO_AT'.
  mc_add '/IWBEP/I_SBO_ATT'. mc_add '/IWBEP/I_SBO_CH'.
  mc_add '/IWBEP/I_SBO_CT'. mc_add '/IWBEP/I_SBO_CTT'.
  mc_add '/IWBEP/I_SBO_DRT'. mc_add '/IWBEP/I_SBO_DSR'.
  mc_add '/IWBEP/I_SBO_EC'. mc_add '/IWBEP/I_SBO_ECT'.
  mc_add '/IWBEP/I_SBO_ENM'. mc_add '/IWBEP/I_SBO_ES'.
  mc_add '/IWBEP/I_SBO_EST'. mc_add '/IWBEP/I_SBO_ET'.
  mc_add '/IWBEP/I_SBO_ETT'. mc_add '/IWBEP/I_SBO_EX'.
  mc_add '/IWBEP/I_SBO_EXT'. mc_add '/IWBEP/I_SBO_FI'.
  mc_add '/IWBEP/I_SBO_FIT'. mc_add '/IWBEP/I_SBO_FP'.
  mc_add '/IWBEP/I_SBO_FPT'. mc_add '/IWBEP/I_SBO_MEM'.
  mc_add '/IWBEP/I_SBO_MR'. mc_add '/IWBEP/I_SBO_MRT'.
  mc_add '/IWBEP/I_SBO_NBT'. mc_add '/IWBEP/I_SBO_NP'.
  mc_add '/IWBEP/I_SBO_NPB'. mc_add '/IWBEP/I_SBO_NPT'.
  mc_add '/IWBEP/I_SBO_PE'. mc_add '/IWBEP/I_SBO_PR'.
  mc_add '/IWBEP/I_SBO_PRT'. mc_add '/IWBEP/I_SBO_RC'.
  mc_add '/IWBEP/I_SBO_RCT'. mc_add '/IWBEP/I_SBO_RFC'.
  mc_add '/IWBEP/I_SBO_RH'. mc_add '/IWBEP/I_SBO_RM'.
  mc_add '/IWBEP/I_SBO_SE'. mc_add '/IWBEP/I_SBO_SM'.
  mc_add '/IWBEP/I_SBO_SMT'. mc_add '/IWBEP/I_SBO_TM'.
  mc_add '/IWBEP/I_SBO_TY'. mc_add '/IWBEP/I_SBU_A'.
  mc_add '/IWBEP/I_SBU_AT'. mc_add '/IWBEP/I_SBU_C'.
  mc_add '/IWBEP/I_SBU_CT'. mc_add '/IWBEP/I_SBU_F'.
  mc_add '/IWBEP/I_SBU_FT'. mc_add '/IWBEP/I_SBU_G'.
  mc_add '/IWBEP/I_SBU_GT'. mc_add '/IWBEP/I_SBU_P'.
  mc_add '/IWBEP/I_SBU_PS'. mc_add '/IWBEP/I_SBU_PSF'.
  mc_add '/IWBEP/I_SBU_PST'. mc_add '/IWBEP/I_SBU_PSV'.
  mc_add '/IWBEP/I_SBU_PT'. mc_add '/IWBEP/I_SBU_RP'.
  mc_add '/IWBEP/I_SBU_XP'. mc_add '/IWBEP/I_SBU_XPT'.
  mc_add '/IWBEP/I_SBU_XT'. mc_add '/IWBEP/I_SBU_XTT'.
  mc_add '/IWBEP/I_V4_MSGA'. mc_add '/IWBEP/I_V4_MSGR'.
  mc_add '/IWBEP/I_V4_MSGT'. mc_add '/IWBEP/I_V4_MSGZ'.
  mc_add '/IWBEP/I_V4_MSRT'. mc_add '/IWBEP/I_V4_MSRV'.
  mc_add '/IWFND/C_CCMS'. mc_add '/IWFND/C_CCMST'.
  mc_add '/IWFND/I_COCCOL'. mc_add '/IWFND/I_COCCOLT'.
  mc_add '/IWFND/I_COCONT'. mc_add '/IWFND/I_COCONTT'.
  mc_add '/IWFND/I_COGSDO'. mc_add '/IWFND/I_COGSDOT'.
  mc_add '/IWFND/I_DST_TRA'. mc_add '/IWFND/I_IMGSIM'.
  mc_add '/IWFND/I_MED_ADF'. mc_add '/IWFND/I_MED_ADM'.
  mc_add '/IWFND/I_MED_ADT'. mc_add '/IWFND/I_MED_AEN'.
  mc_add '/IWFND/I_MED_ALE'. mc_add '/IWFND/I_MED_ALK'.
  mc_add '/IWFND/I_MED_ANS'. mc_add '/IWFND/I_MED_ANT'.
  mc_add '/IWFND/I_MED_APC'. mc_add '/IWFND/I_MED_AVS'.
  mc_add '/IWFND/I_MED_CDK'. mc_add '/IWFND/I_MED_CDL'.
  mc_add '/IWFND/I_MED_CTD'. mc_add '/IWFND/I_MED_CTT'.
  mc_add '/IWFND/I_MED_EMM'. mc_add '/IWFND/I_MED_EPC'.
  mc_add '/IWFND/I_MED_ESM'. mc_add '/IWFND/I_MED_INH'.
  mc_add '/IWFND/I_MED_MEX'. mc_add '/IWFND/I_MED_MHD'.
  mc_add '/IWFND/I_MED_MIN'. mc_add '/IWFND/I_MED_MPR'.
  mc_add '/IWFND/I_MED_NOD'. mc_add '/IWFND/I_MED_OHD'.
  mc_add '/IWFND/I_MED_OHT'. mc_add '/IWFND/I_MED_OP'.
  mc_add '/IWFND/I_MED_PRP'. mc_add '/IWFND/I_MED_REF'.
  mc_add '/IWFND/I_MED_SIN'. mc_add '/IWFND/I_MED_SRG'.
  mc_add '/IWFND/I_MED_SRH'. mc_add '/IWFND/I_MED_SRK'.
  mc_add '/IWFND/I_MED_SRS'. mc_add '/IWFND/I_MED_SRT'.
  mc_add '/IWFND/I_MED_TG'. mc_add '/IWFND/I_MED_TGT'.
  mc_add '/IWFND/I_MED_TXK'. mc_add '/IWFND/I_MED_TXR'.
  mc_add '/IWFND/I_MED_TXT'. mc_add '/IWFND/I_MED_VEH'.
  mc_add '/IWFND/I_MET_EXE'. mc_add '/IWFND/I_MGDPIM'.
  mc_add '/IWFND/I_RMI_MTF'. mc_add '/IWFND/I_SM_CNRL'.
  mc_add '/IWFND/I_SM_CNRT'. mc_add '/IWFND/SU_GWC_CU'.
  mc_add '/IWFND/SU_GWC_OP'. mc_add '/IWFND/SU_GWTEST'.
  mc_add '/IWNGW/B_REG'. mc_add '/IWNGW/B_REG_T'.
  mc_add '/IWNGW/NPUSHR'. mc_add '/IWNGW/NPUSHR_T'.
  mc_add '/IWWRK/I_WF_PACT'. mc_add '/IWWRK/I_WF_PADP'.
  mc_add '/IWWRK/I_WF_PROV'. mc_add '/IWWRK/I_WF_PSRV'.
  mc_add '/OSP/T_MAPREPORT'. mc_add '/SAPDMC/LISCREEN'.
  mc_add '/SAPDMC/LSCDOCU'. mc_add '/SAPDMC/LSCDOCUT'.
  mc_add '/SAPDMC/LSCPROGS'. mc_add '/SAPDMC/LSCRULE'.
  mc_add '/SAPDMC/LSCRULET'. mc_add '/SAPDMC/LSCSTEPS'.
  mc_add '/SAPDMC/LSCSTEPT'. mc_add '/SAPDMC/LSCVERSN'.
  mc_add '/SAPDMC/LSGBDC'. mc_add '/SAPDMC/LSGBDCA'.
  mc_add '/SAPDMC/LSGBDCS'. mc_add '/SAPDMC/LSGBDCT'.
  mc_add '/SAPDMC/LSGCUST'. mc_add '/SAPDMC/LSGPRO'.
  mc_add '/SAPDMC/LSGPROT'. mc_add '/SAPDMC/LSGSUB'.
  mc_add '/SAPDMC/LSGSUBT'. mc_add '/SAPDMC/LSGUSDFI'.
  mc_add '/SAPDMC/LSOALG'. mc_add '/SAPDMC/LSOATT'.
  mc_add '/SAPDMC/LSOCOD'. mc_add '/SAPDMC/LSODOC'.
  mc_add '/SAPDMC/LSOFIL'. mc_add '/SAPDMC/LSOFIS'.
  mc_add '/SAPDMC/LSOFIW'. mc_add '/SAPDMC/LSOFLD'.
  mc_add '/SAPDMC/LSOINF'. mc_add '/SAPDMC/LSOINP'.
  mc_add '/SAPDMC/LSOINS'. mc_add '/SAPDMC/LSOLOG'.
  mc_add '/SAPDMC/LSOMAP'. mc_add '/SAPDMC/LSOPAR'.
  mc_add '/SAPDMC/LSOPRT'. mc_add '/SAPDMC/LSOREC'.
  mc_add '/SAPDMC/LSOREL'. mc_add '/SAPDMC/LSORUL'.
  mc_add '/SAPDMC/LSOSTR'. mc_add '/SAPDMC/LSOTXT'.
  mc_add '/SAPDMC/LSRATT'. mc_add '/SAPDMC/LSRCOD'.
  mc_add '/SAPDMC/LSRTXT'. mc_add '/SAPDMC/LSRVLI'.
  mc_add '/SAPDMC/LSRVLO'. mc_add '/SAPDMC/LSUCUST'.
  mc_add '/SAPDMC/LSUMENU'. mc_add '/SAPDMC/LSUPROF'.
  mc_add '/SAPQUERY/T_T_01'. mc_add '/SAPQUERY/TA07'.
  mc_add '/SAPQUERY/TEXT01'. mc_add '/UI2/AD_MM_CATLG'.
  mc_add '/UI2/C_CACHE_SUP'. mc_add '/UI2/CACHE_CONF'.
  mc_add '/UI2/CACHE_SUP'. mc_add '/UI2/CHIP_CHDR'.
  mc_add '/UI2/CHIP_CHDRT'. mc_add '/UI2/CHIP_CPAR'.
  mc_add '/UI2/CHIP_CSTAT'. mc_add '/UI2/CHIP_CTAGS'.
  mc_add '/UI2/CHIP_PROV'. mc_add '/UI2/CHIP_PROVT'.
  mc_add '/UI2/EDM_GADGET'. mc_add '/UI2/EDM_GADGETT'.
  mc_add '/UI2/EDM_PAGE'. mc_add '/UI2/EDM_PAGET'.
  mc_add '/UI2/EDM_USAGE'. mc_add '/UI2/EDM_USAGET'.
  mc_add '/UI2/EMB_SUP_CON'. mc_add '/UI2/LPD_FEEDER'.
  mc_add '/UI2/NAVPROV_REG'. mc_add '/UI2/NAVPROV_TXT'.
  mc_add '/UI2/NWBC_CFG'. mc_add '/UI2/NWBC_CFG_GT'.
  mc_add '/UI2/NWBC_CFG_N'. mc_add '/UI2/NWBC_CFG_NT'.
  mc_add '/UI2/NWBC_CFG_S'. mc_add '/UI2/NWBC_CFGT'.
  mc_add '/UI2/NWBC_L_GT'. mc_add '/UI2/NWBC_S_GT_T'.
  mc_add '/UI2/PB_C_BAG'. mc_add '/UI2/PB_C_CHIP'.
  mc_add '/UI2/PB_C_CHIPT'. mc_add '/UI2/PB_C_PAGE'.
  mc_add '/UI2/PB_C_PAGET'. mc_add '/UI2/PB_C_PROP'.
  mc_add '/UI2/PB_C_PROPT'. mc_add '/UI2/POWL_APPLID'.
  mc_add '/UI2/POWL_QUERY'. mc_add '/UI2/POWL_QUERYK'.
  mc_add '/UI2/SEMOBJ'. mc_add '/UI2/SEMOBJC'. mc_add '/UI2/SEMOBJCT'.
  mc_add '/UI2/SEMOBJT'. mc_add '/UI2/SYSALIASMAP'. mc_add '/UI2/SYST'.
  mc_add '/UI2/SYSTEMALIAS'. mc_add '/UI2/SYSTEMS'.
  mc_add '/UI2/SYSTEMST'. mc_add '/UI2/SYSTT'. mc_add '/UI2/T002_BCP47'.
  mc_add '/UI2/TM'. mc_add '/UI5/APP_DEPTYPE'. mc_add '/UI5/APPIDX'.
  mc_add '/UI5/APPIDX_COMP'. mc_add '/UI5/APPIDX_REF'.
  mc_add '/UI5/APPIDX_SRV'. mc_add '/UI5/APPIDX_TXT'.
  mc_add '/UI5/BUNDLEC'. mc_add '/UI5/BUNDLECFILE'.
  mc_add '/UI5/MIME_FILE'. mc_add '/UI5/MIME_PCKG'.
  mc_add '/UI5/MIME_PCKGDP'. mc_add '/UI5/THEME_SETUP'.
  mc_add '/UI5/THEMES_F4'. mc_add '/UI5/TREP_FILES'.
  mc_add '/UI5/TREP_TEXT'. mc_add '/UI5/TREP_TEXT_T'.
  mc_add '/UIF/LREPDATTR'. mc_add '/UIF/LREPDCONT'.
  mc_add '/UIF/LREPDCONTV'. mc_add '/UIF/LREPDEREF'.
  mc_add '/UIF/LREPDLTXT'. mc_add '/UIF/LREPDRCNT'.
  mc_add '/UIF/LREPDREF'. mc_add '/UIF/LREPDSTXT'.
  mc_add '/UIF/LREPDTEXT'. mc_add '/UIF/LREPDTYPE'. mc_add 'AAACD1'.
  mc_add 'AAACD2'. mc_add 'AAB_ID_PROP'. mc_add 'AAB_ID_PROPT'.
  mc_add 'AAB_TRANS_LINK'. mc_add 'AAB_VAR_ID'. mc_add 'AAB_VAR_PROP'.
  mc_add 'AAB_VAR_PROPT'. mc_add 'AAUSORG'. mc_add 'AAUSVAR'.
  mc_add 'ABAPDOCU_TEMP'. mc_add 'ABAPDOCU_TREE'. mc_add 'ABAPDOCUSRCH'.
  mc_add 'ABAPDOCUSTATS'. mc_add 'ABAPTREE'. mc_add 'ABAPTREET'.
  mc_add 'ABDBG_IDEID_USR'. mc_add 'ABDBG_TRACE'.
  mc_add 'ABDOC_CDS_ANNOS'. mc_add 'ABDOC_COMP_ANNOS'.
  mc_add 'ABDOCMODE'. mc_add 'ABDOCSUBJECTS'. mc_add 'ABDOCUSUBJECTS'.
  mc_add 'ABTREE'. mc_add 'ACC_INPH3_DPR'. mc_add 'ACC_INPH3_DYNPRO'.
  mc_add 'ACC_INPH3_PAIRS'. mc_add 'ACG_GEN_DEV_REQ1'.
  mc_add 'ACHTTPSRVATTR'. mc_add 'ACI_PO_SO_LINK'. mc_add 'ACL_ACTGR'.
  mc_add 'ACL_ACTGRE'. mc_add 'ACL_ACTGRT'. mc_add 'ACL_ACTVT'.
  mc_add 'ACL_ACTVTT'. mc_add 'ACL_OBJTP'. mc_add 'ACL_OBJTPA'.
  mc_add 'ACL_OBJTPG'. mc_add 'ACL_OBJTPT'. mc_add 'ACLPERMIS'.
  mc_add 'ACM_DTLOG'. mc_add 'ACM_DTLOGROOT'. mc_add 'ACM_LOCK'.
  mc_add 'ACM_OBJECT_DATA'. mc_add 'ACMACAUTHVIEW'.
  mc_add 'ACMACAVIEWDETAIL'. mc_add 'ACMACCONDITION'.
  mc_add 'ACMACFIELD'. mc_add 'ACMAGGREGATE'. mc_add 'ACMASPECT'.
  mc_add 'ACMASPECTPFCG'. mc_add 'ACMASPECTVIEW'.
  mc_add 'ACMASPPFCGDETAIL'. mc_add 'ACMATDT2RTRSTRMR'.
  mc_add 'ACMATMR'. mc_add 'ACMATMRASSIGASP'. mc_add 'ACMCOND2COND'.
  mc_add 'ACMDCLSRC'. mc_add 'ACMDCLSRCT'. mc_add 'ACMDTCONDITION'.
  mc_add 'ACMGENHIERVIEW'. mc_add 'ACMHIERARCHYKEY'.
  mc_add 'ACMHIERARDATAACC'. mc_add 'ACMHIERARDATADTL'.
  mc_add 'ACMHIERARDATATAB'. mc_add 'ACMMAPPINGDETAIL'.
  mc_add 'ACMNESTEDROLE'. mc_add 'ACMPFCGMAPPING'. mc_add 'ACMROLE'.
  mc_add 'ACMROLE2ROLE'. mc_add 'ACMRTMRCONDITION'.
  mc_add 'ACMRTMRCONTENT'. mc_add 'ACMRTMRRESTRICT'. mc_add 'ACMRULE'.
  mc_add 'ACMTST_ID_HASHES'. mc_add 'ACMTST_LOG'.
  mc_add 'ACMTSTACAUTHVIEW'. mc_add 'ACMTSTACAVIEWDET'.
  mc_add 'ACMTSTACCONDITIO'. mc_add 'ACMTSTACFIELD'.
  mc_add 'ACMTSTAD2RRSTRMR'. mc_add 'ACMTSTAGGREGATE'.
  mc_add 'ACMTSTASPECT'. mc_add 'ACMTSTASPECTPFCG'.
  mc_add 'ACMTSTASPECTVIEW'. mc_add 'ACMTSTASPPFCGDET'.
  mc_add 'ACMTSTATMR'. mc_add 'ACMTSTATMRASSASP'.
  mc_add 'ACMTSTDTCONDITIO'. mc_add 'ACMTSTGENHIERVIE'.
  mc_add 'ACMTSTHIERARKEY'. mc_add 'ACMTSTHIERDATACC'.
  mc_add 'ACMTSTHIERDATDTL'. mc_add 'ACMTSTHIERDATTAB'.
  mc_add 'ACMTSTMAPPINGDET'. mc_add 'ACMTSTPFCGMAPPIN'.
  mc_add 'ACMTSTROLE'. mc_add 'ACMTSTRTMRCONDIT'.
  mc_add 'ACMTSTRTMRCONTEN'. mc_add 'ACMTSTRTMRRESTR'.
  mc_add 'ACMTSTRULE'. mc_add 'ACRFCSRVATTR'. mc_add 'ADAA'.
  mc_add 'ADACACHE'. mc_add 'ADATABSIZE'. mc_add 'ADIRACCESS'.
  mc_add 'ADMI_APPLI'. mc_add 'ADMI_BUFFC'. mc_add 'ADMI_BUFFI'.
  mc_add 'ADMI_FILE_DAVLNK'. mc_add 'ADMI_FILE_RULES'.
  mc_add 'ADMI_FILES'. mc_add 'ADMI_JOBS'. mc_add 'ADMI_ORIGINS'.
  mc_add 'ADMI_PROPI'. mc_add 'ADMI_RJOBS'. mc_add 'ADMI_RUN'.
  mc_add 'ADMI_SKIP'. mc_add 'ADMI_STATS'. mc_add 'ADMI_URIS'.
  mc_add 'ADMI_VARIA'. mc_add 'ADMMSG'. mc_add 'ADRDYCONTR'.
  mc_add 'ADT_EXCEPTIONLOG'. mc_add 'AESERVICES'. mc_add 'AEXMNT'.
  mc_add 'AEXPM'. mc_add 'AEXPRN'. mc_add 'AEXUP'. mc_add 'AEXUSR'.
  mc_add 'AGR_MARK'. mc_add 'AII_PROFILES'. mc_add 'AIMEXPERT'.
  mc_add 'AIMTRKORR_SHD'. mc_add 'AIND_CONN'. mc_add 'AIND_CONN_STR'.
  mc_add 'AIND_STATU'. mc_add 'AIND_STR1'. mc_add 'AIND_STR10'.
  mc_add 'AIND_STR1T'. mc_add 'AIND_STR2'. mc_add 'AIND_STR3'.
  mc_add 'AIND_STR4'. mc_add 'AIND_STR5'. mc_add 'AIND_STR5T'.
  mc_add 'AIND_STR6'. mc_add 'AIND_STR7'. mc_add 'AIND_STR8'.
  mc_add 'AIND_STR9'. mc_add 'AKB_CHECK_PROFT'. mc_add 'AKB_CHKCONF'.
  mc_add 'AKB_DLVUNITS'. mc_add 'AKB_EXCEPT'. mc_add 'AKB_EXCEPT_TEST'.
  mc_add 'AKB_EXCEPT2'. mc_add 'AKB_EXCEPTIONS'.
  mc_add 'AKB_EXCEPTIONS2'. mc_add 'AKB_EXPT_TO_SEND'.
  mc_add 'AKB_FREEZE'. mc_add 'AKB_FREEZE_EXT'. mc_add 'AKB_FREEZE_SRC'.
  mc_add 'AKB_INCOMP_INH'. mc_add 'AKB_INDX'. mc_add 'AKB_JOBSTATE'.
  mc_add 'AKB_JOBSTATES'. mc_add 'AKB_JOBSTATEST'. mc_add 'AKB_LOGBOOK'.
  mc_add 'AKB_OBJECT_DIR'. mc_add 'AKB_PRODCOMP'.
  mc_add 'AKB_PRODUCT_CHK'. mc_add 'AKB_PRODUCT_COMP'.
  mc_add 'AKB_PRODUCT_INTF'. mc_add 'AKB_PRODUCTS'.
  mc_add 'AKB_PROJECTS'. mc_add 'AKB_PROJECTST'. mc_add 'AKB_RELOBJS'.
  mc_add 'AKB_RUNLIST'. mc_add 'AKB_SCR_DIR'. mc_add 'AKB_SCR_JOBS'.
  mc_add 'AKB_SCR_OP'. mc_add 'AKB_SCRIPT'. mc_add 'AKB_STATISTICS'.
  mc_add 'AKB_SYSLIST'. mc_add 'AKB_TRANS_CHECK'.
  mc_add 'AKB_TRANS_HEAD'. mc_add 'AKB_USAGE_INFO'.
  mc_add 'AKB_USAGE_INFO2'. mc_add 'AKB_USG'. mc_add 'AKB_USG_EDGE'.
  mc_add 'AKB_USG_EXT'. mc_add 'AKB_USG_INFO'. mc_add 'AKB_VERS_OBJ'.
  mc_add 'ALALERTDB'. mc_add 'ALALERTX'. mc_add 'ALALRTGUID'.
  mc_add 'ALAVLAGENT'. mc_add 'ALAVLCTRL'. mc_add 'ALAVLGCNTL'.
  mc_add 'ALAVLGRP_C'. mc_add 'ALAVLGUID'. mc_add 'ALAVLPING'.
  mc_add 'ALAVLPTRC'. mc_add 'ALAVLWORK'. mc_add 'ALBTCMON'.
  mc_add 'ALCACHECNF'. mc_add 'ALCCMCTRC'. mc_add 'ALCCMCTRCFIL'.
  mc_add 'ALCCMCUST'. mc_add 'ALCCMCUSTP'. mc_add 'ALCCMCUSTT'.
  mc_add 'ALCCMCUSTV'. mc_add 'ALCCMSPING_TRACE'. mc_add 'ALCLASTOOL'.
  mc_add 'ALCONSEG'. mc_add 'ALCSMCONF'. mc_add 'ALCSMCONF_LOC'.
  mc_add 'ALCUSTSET'. mc_add 'ALDBSCTX'. mc_add 'ALDBSMSEG'.
  mc_add 'ALDBSMTE'. mc_add 'ALDBSPERF'. mc_add 'ALDBSSMES'.
  mc_add 'ALDBSTOOL'. mc_add 'ALDTSCACHE'. mc_add 'ALDTSCTRL'.
  mc_add 'ALDTSMAP'. mc_add 'ALDWNTIME'. mc_add 'ALF1DEFLT'.
  mc_add 'ALGLOBSYSLGFIL'. mc_add 'ALGRPCUSGE'. mc_add 'ALGRPCUSMC'.
  mc_add 'ALGRPCUSMG'. mc_add 'ALGRPCUSPF'. mc_add 'ALGRPMCFIL'.
  mc_add 'ALIINCL'. mc_add 'ALIMCDATA'. mc_add 'ALLAZYMT'.
  mc_add 'ALLAZYMT2'. mc_add 'ALLONGJOB_CTRL'. mc_add 'ALMBCADM'.
  mc_add 'ALMBCDATA'. mc_add 'ALMBRADM'. mc_add 'ALMBRDATA'.
  mc_add 'ALMDRLDSC'. mc_add 'ALMDRULES'. mc_add 'ALMONIDEF'.
  mc_add 'ALMONISETS'. mc_add 'ALMONITORS'. mc_add 'ALMRULES'.
  mc_add 'ALMSETS'. mc_add 'ALMSETSV2'. mc_add 'ALMTCUSGEN'.
  mc_add 'ALMTCUSMSC'. mc_add 'ALMTCUSPER'. mc_add 'ALMTCUSSMG'.
  mc_add 'ALMTECLSET'. mc_add 'ALMTMSCFIL'. mc_add 'ALNAMEDUSERS'.
  mc_add 'ALNODEKEY'. mc_add 'ALPERFDB'. mc_add 'ALPERFDB_C'.
  mc_add 'ALPERFOB'. mc_add 'ALPERSONEL'. mc_add 'ALPF_TESTDATA'.
  mc_add 'ALPFASSIGN'. mc_add 'ALPFAUTOREPORT'.
  mc_add 'ALPFCOLLREORGSCH'. mc_add 'ALPFCOLLSYS'. mc_add 'ALPFCOLLTID'.
  mc_add 'ALPFDBVAR'. mc_add 'ALPFDBVARI'. mc_add 'ALPFDBVDES'.
  mc_add 'ALPFPARAM'. mc_add 'ALPFPOLICY'. mc_add 'ALPFREORG'.
  mc_add 'ALPFREP_CAT'. mc_add 'ALPFREP_DAT'. mc_add 'ALPFREPDEF'.
  mc_add 'ALPFREPORTDEF'. mc_add 'ALPFSFTPLY'. mc_add 'ALPFTHRHIST_RWS'.
  mc_add 'ALPFTHRHIST_TB'. mc_add 'ALPFWSCHEM'. mc_add 'ALQRFCMON'.
  mc_add 'ALQRFCMONA'. mc_add 'ALQRFCMONO'. mc_add 'ALQRFCMONQ'.
  mc_add 'ALSLDCTRL'. mc_add 'ALSLDUPD'. mc_add 'ALSYSAL'.
  mc_add 'ALSYSGRPS'. mc_add 'ALSYSTEMS'. mc_add 'ALTEXTASSIGN'.
  mc_add 'ALTEXTDB'. mc_add 'ALTEXTSCHEMA'. mc_add 'ALTIDTOOL'.
  mc_add 'ALTLDESCR'. mc_add 'ALTOOLCHEK'. mc_add 'ALTOOLDP'.
  mc_add 'ALTOOLEXEC'. mc_add 'ALTRAMONI'. mc_add 'ALTSTLOD'.
  mc_add 'ALV_T_PROG'. mc_add 'ALV_T_REFDATA1'.
  mc_add 'ALV_T_TESTDATA1'. mc_add 'ALV_T_TSCENARIO'. mc_add 'ALVFOBMO'.
  mc_add 'ALVFOBTY'. mc_add 'ALXMBALERT'. mc_add 'ALXMBEMAIL'.
  mc_add 'ALXMBINHERITKEY'. mc_add 'ALXMBPFALERT'. mc_add 'AMASCH'.
  mc_add 'AMASTA'. mc_add 'AMASUR'. mc_add 'AMATR'. mc_add 'AMATRT'.
  mc_add 'AMC_APPL'. mc_add 'AMC_APPL_TEXT'. mc_add 'AMC_CHANNEL'.
  mc_add 'AMC_CHNL_AUTH'. mc_add 'AMC_LOAD_TEST'.
  mc_add 'AMC_MESSAGE_TYPE'. mc_add 'AMC_MSG_TYPE_TXT'.
  mc_add 'AMC_RECEIVER'. mc_add 'AMC_RECEIVER2'. mc_add 'AMCSTATATTR'.
  mc_add 'AMCUST'. mc_add 'AMDES'. mc_add 'AMDP_CLEANUP_LOG'.
  mc_add 'AMDP_ID'. mc_add 'AMDP_SCHEMA_DEF'. mc_add 'AMDP_SCHEMA_DEFT'.
  mc_add 'AMDP_SCHEMA_MAP'. mc_add 'AMDPDB'. mc_add 'AMDS'.
  mc_add 'AMDT'. mc_add 'AMESISTA'. mc_add 'AMESISUB'. mc_add 'AMHD'.
  mc_add 'AMHDT'. mc_add 'AMIDC'. mc_add 'AMIDT'. mc_add 'AMIDTP'.
  mc_add 'AMPRO'. mc_add 'AMRFC'. mc_add 'AMRH'. mc_add 'AMSK'.
  mc_add 'AMSP'. mc_add 'AMSSU'. mc_add 'AMSTA'. mc_add 'AMSTANO'.
  mc_add 'AMSUR'. mc_add 'AMTINFO'. mc_add 'AMTSU'.
  mc_add 'ANONYMIZATION'. mc_add 'ANONYMIZATIOND'.
  mc_add 'ANONYMIZATIONX'. mc_add 'ANST_CUST_OBJ'.
  mc_add 'ANST_SETTINGS'. mc_add 'ANST_TRACE_ENQ'. mc_add 'APADEF'.
  mc_add 'APADEFT'. mc_add 'APADEVC'. mc_add 'APAEXCLUDE'.
  mc_add 'APAFOREIGN'. mc_add 'APARELA'. mc_add 'APAUSED'.
  mc_add 'APAUSED_TEST'. mc_add 'APB_LPD_PATHS'.
  mc_add 'APB_LPD_PATHS_C'. mc_add 'APB_LPD_SEARCH'.
  mc_add 'APB_LPD_SH_TEXTS'. mc_add 'APB_LPD_UNIQ_ID'.
  mc_add 'APC_APPL'. mc_add 'APC_APPL_TEXT'. mc_add 'APC_WSP_TYPE'.
  mc_add 'APC_WSP_TYPE_TXT'. mc_add 'APCAPPL_CFG'. mc_add 'APCSTATATTR'.
  mc_add 'APJ_S_DT_DBA'. mc_add 'APJ_W_JCE_GR'. mc_add 'APJ_W_JCE_GR_T'.
  mc_add 'APJ_W_JCE_PAR'. mc_add 'APJ_W_JCE_RO_T'.
  mc_add 'APJ_W_JCE_ROOT'. mc_add 'APJ_W_JCE_ROOT_T'.
  mc_add 'APJ_W_JCE_SCT'. mc_add 'APJ_W_JCE_SCT_T'.
  mc_add 'APJ_W_JCE_SEQ'. mc_add 'APJ_W_JCE_SEQ_T'.
  mc_add 'APJ_W_JT_RO_T'. mc_add 'APJ_W_JT_ROOT'. mc_add 'APJ_W_JT_VAL'.
  mc_add 'APL_E_LCE'. mc_add 'APL_W_LCE_RO_T'. mc_add 'APL_W_LCE_ROOT'.
  mc_add 'APP_FCTR'. mc_add 'APQD'. mc_add 'APQI'. mc_add 'APQL'.
  mc_add 'APS_INSTADDSW'. mc_add 'APS_INSTADDSW_T'.
  mc_add 'APS_T_CGKE_REP'. mc_add 'APSRV'. mc_add 'APTREE'.
  mc_add 'APTREET'. mc_add 'AQGDB'. mc_add 'AQGDBBG'. mc_add 'AQGDBBN'.
  mc_add 'AQGDBBS'. mc_add 'AQGIDOC'. mc_add 'AQGQCAT'.
  mc_add 'AQGQDOC'. mc_add 'AQGQSTRUC'. mc_add 'AQGSBWMAP'.
  mc_add 'AQGSBWPROP'. mc_add 'AQGSCAT'. mc_add 'AQGTB'. mc_add 'AQGTQ'.
  mc_add 'AQGTS'. mc_add 'AQHTMLRAW'. mc_add 'AQHTMLTMPL'.
  mc_add 'AQIF_C'. mc_add 'AQIF_S'. mc_add 'AQRDB'. mc_add 'AQREPTRASH'.
  mc_add 'AQSGDB'. mc_add 'AQTDB'. mc_add 'AQTXTBS'. mc_add 'AQTXTQ'.
  mc_add 'AQXINT'. mc_add 'AQXINTA'. mc_add 'AQXINTT'.
  mc_add 'AQXINTUG'. mc_add 'ARC_CHGR_STRUN'. mc_add 'ARC_DESTR_OBJ'.
  mc_add 'ARC_DESTR_OBJTXT'. mc_add 'ARC_DESTR_STRUC'.
  mc_add 'ARCH_CHECK_DATE'. mc_add 'ARCH_CHECK_EXC'.
  mc_add 'ARCH_CLASS'. mc_add 'ARCH_CTXT'. mc_add 'ARCH_DEF'.
  mc_add 'ARCH_DELE'. mc_add 'ARCH_ENQUE'. mc_add 'ARCH_ETXT'.
  mc_add 'ARCH_EVENT'. mc_add 'ARCH_GENER'. mc_add 'ARCH_IDX'.
  mc_add 'ARCH_IDX_S'. mc_add 'ARCH_NET'. mc_add 'ARCH_NUM'.
  mc_add 'ARCH_OBJ'. mc_add 'ARCH_OCLAS'. mc_add 'ARCH_OHEAD'.
  mc_add 'ARCH_RPRG'. mc_add 'ARCH_SERV'. mc_add 'ARCH_TCODE'.
  mc_add 'ARCH_TXT'. mc_add 'ARCH_USR'. mc_add 'ARCHLNK_TOAAR'.
  mc_add 'ARDB_STA3T'. mc_add 'ARDB_STA3TN'. mc_add 'ARDB_STA4T'.
  mc_add 'ARDB_STAT0'. mc_add 'ARDB_STAT1'. mc_add 'ARDB_STAT2'.
  mc_add 'ARDB_STAT3'. mc_add 'ARDB_STAT3N'. mc_add 'ARDB_STAT4'.
  mc_add 'ARDB_STAT5'. mc_add 'ARDB_STAT6'. mc_add 'ARFCLOG'.
  mc_add 'ARFCRCONTEXT'. mc_add 'ARFCRDATA'. mc_add 'ARFCRSTATE'.
  mc_add 'ARFCSDATA'. mc_add 'ARFCSSTATE'. mc_add 'ASEXDOM'.
  mc_add 'ASGRP'. mc_add 'ASHGETS'. mc_add 'ASHMETHODS'.
  mc_add 'ASIX_INVALID_RUN'. mc_add 'ASIX_SETTINGS'.
  mc_add 'ASPART_ACT'. mc_add 'ASPART_CONF'. mc_add 'ASPART_DB'.
  mc_add 'ASPART_FILES'. mc_add 'ASTAT_OKDF'. mc_add 'ASTAT_OKTX'.
  mc_add 'ASTAT_TYP1'. mc_add 'ASTAT_TYP2'. mc_add 'ASTAT_TYPT'.
  mc_add 'ATAB'. mc_add 'ATCKONTEXT'. mc_add 'ATO_ADAPT_TYPES'.
  mc_add 'ATO_ADAPT_TYPEST'. mc_add 'ATO_CHANGEL_BOM'.
  mc_add 'ATO_CHANGEL_ITEM'. mc_add 'ATO_CHANGEL_ZIP'.
  mc_add 'ATO_CHANGELISTS'. mc_add 'ATO_COL_ITEMS'.
  mc_add 'ATO_COL_VERSIONS'. mc_add 'ATO_COLS'. mc_add 'ATO_CONTI_BOM'.
  mc_add 'ATO_CONTI_EXCLUD'. mc_add 'ATO_DRAFT_BOM'. mc_add 'ATO_E07'.
  mc_add 'ATO_E07PREVIEW'. mc_add 'ATO_ITEMS'. mc_add 'ATO_MESSAGE_MAP'.
  mc_add 'ATO_PK_ITEM_TR'. mc_add 'ATO_PK_ITEMS'.
  mc_add 'ATO_SETUP_HIST'. mc_add 'ATO_TADIR_FLAG'.
  mc_add 'ATO_TADIR_MAP'. mc_add 'ATRA_TATOPB'. mc_add 'ATRATST_BUF'.
  mc_add 'ATRATST_NOBUF'. mc_add 'AUBIDS'. mc_add 'AUCATEGORY'.
  mc_add 'AUDDD_FOR_LOHS'. mc_add 'AUDDD_FOR_XINX'. mc_add 'AUL_ADMIN'.
  mc_add 'AUL_CHILD_GROUP'. mc_add 'AUL_GROUP'. mc_add 'AUL_PATTERN'.
  mc_add 'AUL_SETTINGS'. mc_add 'AUL_SYSTEM'. mc_add 'AUL_TABLE'.
  mc_add 'AUL_TRKORR_CHECK'. mc_add 'AUL_TRKORR_GREY'.
  mc_add 'AUL_TRKORR_OPEN'. mc_add 'AUL_WHITE_TABLES'.
  mc_add 'AUL_WHITELIST'. mc_add 'AUODESC'. mc_add 'AUOFFER'.
  mc_add 'AUSOBT'. mc_add 'AUSOBT_C'. mc_add 'AUT_C_DTELLOG'.
  mc_add 'AUT_C_DTELLOGT'. mc_add 'AUT_C_TABLOG'.
  mc_add 'AUT_C_TABLOGT'. mc_add 'AUT_C_TEST_0'. mc_add 'AUT_C_TEST_1'.
  mc_add 'AUT_C_TEST_2'. mc_add 'AUT_C_TRANSLOG'.
  mc_add 'AUT_D_DBTABLOG'. mc_add 'AUTH_FLDINFO_TMP'. mc_add 'AUTHX'.
  mc_add 'AUUSER'. mc_add 'AVERS_EXT'. mc_add 'AVMOVES'.
  mc_add 'AVMOVESACT'. mc_add 'BADI_CHAR_COND'. mc_add 'BADI_FILTER'.
  mc_add 'BADI_IMPL'. mc_add 'BADI_INT_COND'. mc_add 'BADI_MAIN'.
  mc_add 'BADI_MENU_ELEM'. mc_add 'BADI_NUM_COND'.
  mc_add 'BADI_PACKED_COND'. mc_add 'BADI_SCREEN_DEF'.
  mc_add 'BADI_SPOT'. mc_add 'BADI_SPOT_FILTER'.
  mc_add 'BADI_STRING_COND'. mc_add 'BADIIMPL_ENH'.
  mc_add 'BADIIMPL_MENU'. mc_add 'BADIIMPL_SCREEN'.
  mc_add 'BADIIMPL_SORT'. mc_add 'BADIIMPLENHCUST'.
  mc_add 'BADIISIMPLED'. mc_add 'BALOBJ'. mc_add 'BALOBJT'.
  mc_add 'BALSUB'. mc_add 'BALSUBT'. mc_add 'BAPI_PRIO'.
  mc_add 'BAPICHKLST'. mc_add 'BAPIF4T'. mc_add 'BAPITOOLS'.
  mc_add 'BAPIXMLOGR'. mc_add 'BATMANDPT'. mc_add 'BATMANSQT'.
  mc_add 'BATMANTDT'. mc_add 'BC505EX'. mc_add 'BC505IN'.
  mc_add 'BC505TS'. mc_add 'BC510DS'. mc_add 'BC510IN'.
  mc_add 'BC510X1'. mc_add 'BC510X2'. mc_add 'BCFG_D_CONT_LANG'.
  mc_add 'BCOS_CUST'. mc_add 'BCOS_PROT'. mc_add 'BCSD_BLMODULE'.
  mc_add 'BCSETT'. mc_add 'BCST_ASSIGNMENT'. mc_add 'BCST_ATFCD'.
  mc_add 'BCST_ATFLT'. mc_add 'BCST_FLTVL'. mc_add 'BCST_FLTVT'.
  mc_add 'BCSTTATFCD'. mc_add 'BCSTTATFLT'. mc_add 'BCUSEDMIGR'.
  mc_add 'BDL_CLUSTL'. mc_add 'BDL_GENER'. mc_add 'BDL2TRANS'.
  mc_add 'BDL2TRANS2'. mc_add 'BDLBADHOST'. mc_add 'BDLCOES'.
  mc_add 'BDLCONTAB'. mc_add 'BDLCTEXT'. mc_add 'BDLCTX2EXE'.
  mc_add 'BDLCUST'. mc_add 'BDLCUSTSE2'. mc_add 'BDLCUSTSES'.
  mc_add 'BDLDATCOL'. mc_add 'BDLDEST'. mc_add 'BDLDIRFUNC'.
  mc_add 'BDLDTOC'. mc_add 'BDLENDFUNC'. mc_add 'BDLERRORS'.
  mc_add 'BDLEXCEPT'. mc_add 'BDLFUGRPS'. mc_add 'BDLFUNC'.
  mc_add 'BDLFUNCMAP'. mc_add 'BDLFUPDAT'. mc_add 'BDLFUPDEF'.
  mc_add 'BDLFUPEXP'. mc_add 'BDLFUPIMP'. mc_add 'BDLFUPTYP'.
  mc_add 'BDLFUVER2'. mc_add 'BDLFUVER3'. mc_add 'BDLFUVERS'.
  mc_add 'BDLGROUPS'. mc_add 'BDLHEAD'. mc_add 'BDLIBUS'.
  mc_add 'BDLIFCHA'. mc_add 'BDLIFDEF'. mc_add 'BDLIFEXC'.
  mc_add 'BDLIFEXP'. mc_add 'BDLIFFIELD'. mc_add 'BDLIFHEAD'.
  mc_add 'BDLIFIMP'. mc_add 'BDLIFTBL'. mc_add 'BDLINTSESS'.
  mc_add 'BDLLOCKED'. mc_add 'BDLMSGLOG'. mc_add 'BDLNOHOSTS'.
  mc_add 'BDLOWNSESS'. mc_add 'BDLREFSERV'. mc_add 'BDLSADATA'.
  mc_add 'BDLSAIF'. mc_add 'BDLSCC'. mc_add 'BDLSCCT'.
  mc_add 'BDLSCOUNTALL'. mc_add 'BDLSERGRPS'. mc_add 'BDLSERTRAN'.
  mc_add 'BDLSERVICE'. mc_add 'BDLSESDEST'. mc_add 'BDLSESS'.
  mc_add 'BDLSESSIONHEAD'. mc_add 'BDLSEXT'. mc_add 'BDLSEXZ'.
  mc_add 'BDLST14KEY'. mc_add 'BDLSTATUS'. mc_add 'BDLTESTSES'.
  mc_add 'BDLTMPDAT'. mc_add 'BDLTRHOSTS'. mc_add 'BDLTYPEDEF'.
  mc_add 'BDLVARCON'. mc_add 'BDLVIEWREP'. mc_add 'BDS_BAR_EX'.
  mc_add 'BDS_BAR_IN_REF'. mc_add 'BDS_CLASS'. mc_add 'BDS_CLASSL'.
  mc_add 'BDS_CONN01'. mc_add 'BDS_CONN02'. mc_add 'BDS_CONN03'.
  mc_add 'BDS_CONN05'. mc_add 'BDS_CONN09'. mc_add 'BDS_CONN21'.
  mc_add 'BDS_LOCL'. mc_add 'BDS_LOG'. mc_add 'BDS_LORE'.
  mc_add 'BDS_T_PHIO'. mc_add 'BDSCHKF2'. mc_add 'BDSCHKF21'.
  mc_add 'BDSCHKF3'. mc_add 'BDSCHKO2'. mc_add 'BDSCHKO21'.
  mc_add 'BDSCHKO3'. mc_add 'BDSCONT2'. mc_add 'BDSCONT21'.
  mc_add 'BDSCONT3'. mc_add 'BDSLOIO2'. mc_add 'BDSLOIO21'.
  mc_add 'BDSLOIO3'. mc_add 'BDSLOIOT2'. mc_add 'BDSLOIOT21'.
  mc_add 'BDSLOIOT3'. mc_add 'BDSLOPR2'. mc_add 'BDSLOPR21'.
  mc_add 'BDSLOPR3'. mc_add 'BDSLORE2'. mc_add 'BDSLORE21'.
  mc_add 'BDSLORE3'. mc_add 'BDSLORI2'. mc_add 'BDSLORI21'.
  mc_add 'BDSLORI3'. mc_add 'BDSPHF2'. mc_add 'BDSPHF21'.
  mc_add 'BDSPHF3'. mc_add 'BDSPHHR2'. mc_add 'BDSPHHR21'.
  mc_add 'BDSPHHR3'. mc_add 'BDSPHIO2'. mc_add 'BDSPHIO21'.
  mc_add 'BDSPHIO3'. mc_add 'BDSPHNM2'. mc_add 'BDSPHNM21'.
  mc_add 'BDSPHNM3'. mc_add 'BDSPHPR2'. mc_add 'BDSPHPR21'.
  mc_add 'BDSPHPR3'. mc_add 'BDSPHRE2'. mc_add 'BDSPHRE21'.
  mc_add 'BDSPHRE3'. mc_add 'BDSPHRI2'. mc_add 'BDSPHRI21'.
  mc_add 'BDSPHRI3'. mc_add 'BDSRE2'. mc_add 'BDSRE21'. mc_add 'BDSRE3'.
  mc_add 'BDSREPR2'. mc_add 'BDSREPR21'. mc_add 'BDSREPR3'.
  mc_add 'BDSX_CON04'. mc_add 'BEISPIEL'. mc_add 'BF4INDX'.
  mc_add 'BFUSER_TYP'. mc_add 'BGPROT'. mc_add 'BGRFC_CCMS_DATA'.
  mc_add 'BGRFC_CUST_I_DST'. mc_add 'BGRFC_CUST_I_SRV'.
  mc_add 'BGRFC_CUST_I_SYS'. mc_add 'BGRFC_CUST_O_DST'.
  mc_add 'BGRFC_CUST_O_SRV'. mc_add 'BGRFC_CUST_O_SYS'.
  mc_add 'BGRFC_CUST_SUPER'. mc_add 'BGRFC_EVENT_PARM'.
  mc_add 'BGRFC_I_DEST_ERR'. mc_add 'BGRFC_I_DESTLOCK'.
  mc_add 'BGRFC_I_RUNNABLE'. mc_add 'BGRFC_IUNIT_HIST'.
  mc_add 'BGRFC_LOCK_TIME'. mc_add 'BGRFC_MAIN_I_DST'.
  mc_add 'BGRFC_N_RUNNABLE'. mc_add 'BGRFC_NOTI_ERR'.
  mc_add 'BGRFC_O_CONV_ERR'. mc_add 'BGRFC_O_CONVERT'.
  mc_add 'BGRFC_O_DEST_ERR'. mc_add 'BGRFC_O_DESTLOCK'.
  mc_add 'BGRFC_O_RUNNABLE'. mc_add 'BGRFC_OUNIT_HIST'.
  mc_add 'BGRFC_SRV_CFM'. mc_add 'BGRFC_SRV_STATE'.
  mc_add 'BGRFC_UNIT_TIME'. mc_add 'BGRFC_UTASK_KEY'.
  mc_add 'BIBS_RPST'. mc_add 'BIBS_SCR'. mc_add 'BIDT_T_ALL'.
  mc_add 'BIDT_T_CHBIDI'. mc_add 'BIDT_T_CHLTR'. mc_add 'BIDT_T_DECLTR'.
  mc_add 'BIDT_T_DECN'. mc_add 'BIDT_T_INTLTR'. mc_add 'BIDT_T_INTN'.
  mc_add 'BIW_PREFS'. mc_add 'BIZC_CONF_STAT'. mc_add 'BIZC_CPROV_REG'.
  mc_add 'BIZC_CPROV_SOLMA'. mc_add 'BL01_INDX'. mc_add 'BMA_CONS'.
  mc_add 'BMA_EXCE'. mc_add 'BMA_PROT'. mc_add 'BMA_WLST'.
  mc_add 'BMENTTREE'. mc_add 'BMENTTREET'. mc_add 'BMTNODE01'.
  mc_add 'BMTNODE01R'. mc_add 'BMTNODE01T'. mc_add 'BORTST_INT_MAT'.
  mc_add 'BORTST_MAP_MATNR'. mc_add 'BORTST_MATERIAL'.
  mc_add 'BPABAPJOB'. mc_add 'BPCA_MAPPING'. mc_add 'BPCA_MAPPING_K'.
  mc_add 'BPDTRG'. mc_add 'BPDTTRG'. mc_add 'BPETRG'. mc_add 'BPEVTRG'.
  mc_add 'BPJTRG'. mc_add 'BPOTRG'. mc_add 'BPTRGTYP'.
  mc_add 'BPXCMDJOB'. mc_add 'BPXPRGJOB'. mc_add 'BRACNTL'.
  mc_add 'BRALOG'. mc_add 'BRAN'. mc_add 'BRAN1'. mc_add 'BRANT'.
  mc_add 'BRATEXT'. mc_add 'BRFWBREGISTRY'. mc_add 'BRFWBTOOLTYPE'.
  mc_add 'BRR_ACTIVE'. mc_add 'BRR_ATTRIB'. mc_add 'BRR_CONT'.
  mc_add 'BRR_HEADER'. mc_add 'BRR_TEXT'. mc_add 'BSP_APPLC'.
  mc_add 'BSPARTICLE'. mc_add 'BSPBINCL'. mc_add 'BSPDEFPAG'.
  mc_add 'BSPDETAIL'. mc_add 'BSPEJBCL'. mc_add 'BSPEXTSKIN'.
  mc_add 'BSPGLOBALSETTING'. mc_add 'BSPTEMPXSRFSTORE'.
  mc_add 'BSPTESTSUITE'. mc_add 'BTC_CRITERIA'. mc_add 'BTC_CRITNODES'.
  mc_add 'BTC_CRITPROFILES'. mc_add 'BTC_CRITTYPES'.
  mc_add 'BTC_TYPEFIELDS'. mc_add 'BTC1011'. mc_add 'BTCCTL'.
  mc_add 'BTCD2N0130'. mc_add 'BTCDELAY'. mc_add 'BTCEVC'.
  mc_add 'BTCEVTHISTORY'. mc_add 'BTCEVTJOB'. mc_add 'BTCH2050'.
  mc_add 'BTCJN'. mc_add 'BTCJOBEPP'. mc_add 'BTCJSTAT'.
  mc_add 'BTCOMSDL'. mc_add 'BTCOMSET'. mc_add 'BTCOPTIONS'.
  mc_add 'BTCSED'. mc_add 'BTCSEV'. mc_add 'BTCUED'. mc_add 'BTCUEV'.
  mc_add 'BTFR_OBJ_ATTR_S'. mc_add 'BTFR_OBJ_ATTR_T'.
  mc_add 'BTFR_OBJ_IDS'. mc_add 'BTFR_TREX_COMP'.
  mc_add 'BTFR_TREX_PACK'. mc_add 'BTFR_TYPES_S'. mc_add 'BTFR_TYPES_T'.
  mc_add 'BTM_TDATA'. mc_add 'BUFCHECK'. mc_add 'BUMF_COMPNTS'.
  mc_add 'BUMF_DECO'. mc_add 'BWAFCCMSCOMPO'. mc_add 'BWAFDEVTYP'.
  mc_add 'BWAFINSLOG'. mc_add 'BWAFMAPP'. mc_add 'BWWF_MIME'.
  mc_add 'CAPTREE'. mc_add 'CAPTREET'. mc_add 'CAT_DP_FLD'.
  mc_add 'CAT_DP_FOR'. mc_add 'CAT_DP_PRO'. mc_add 'CAT_DP_TAB'.
  mc_add 'CAT_FDATA'. mc_add 'CAT_FINFO'. mc_add 'CAT_PARAMS'.
  mc_add 'CAT_SVARS'. mc_add 'CAT_VARS'. mc_add 'CAT_VERBS'.
  mc_add 'CATA'. mc_add 'CATF'. mc_add 'CATFC'. mc_add 'CATFF'.
  mc_add 'CATG'. mc_add 'CATJ'. mc_add 'CATK'. mc_add 'CATL'.
  mc_add 'CATM'. mc_add 'CATN'. mc_add 'CATO'. mc_add 'CATP'.
  mc_add 'CATP_GLOB'. mc_add 'CATQ'. mc_add 'CATR'. mc_add 'CATT_ABLNR'.
  mc_add 'CATU'. mc_add 'CATV'. mc_add 'CATW'. mc_add 'CATX'.
  mc_add 'CCCEXIT'. mc_add 'CCCFLOW'. mc_add 'CCCINFO'.
  mc_add 'CCDBCHECK'. mc_add 'CCM_ZOS'. mc_add 'CCMSBI_MTE_DATA'.
  mc_add 'CCMSBI_MTE_METH'. mc_add 'CCMSBI_MTE_THRH'.
  mc_add 'CCMSBIDATA'. mc_add 'CCMSBIMETH'. mc_add 'CCMSBITHRH'.
  mc_add 'CCONTAINER'. mc_add 'CCOPTION'. mc_add 'CCOPTT'.
  mc_add 'CCPARPARM'. mc_add 'CCPROF'. mc_add 'CCPROFSR'.
  mc_add 'CCPROFT'. mc_add 'CCUPEAKAG'. mc_add 'CCUPEAKLG'.
  mc_add 'CD1016'. mc_add 'CD1251'. mc_add 'CD1252'.
  mc_add 'CDC_ACTIV_CLIENT'. mc_add 'CDC_LOGTAB_ID'.
  mc_add 'CDC_OWNERTAB'. mc_add 'CDC_RECORDER'. mc_add 'CDC_TABLE'.
  mc_add 'CDC_TRIGGER'. mc_add 'CEBN_ANNO_XSLT'.
  mc_add 'CEBN_EXT_GROUPS'. mc_add 'CEBN_SEM_DEFMAP'.
  mc_add 'CEBN_SEM_FNAMES'. mc_add 'CEBN_SO'. mc_add 'CEBN_SO_ODATA'.
  mc_add 'CEBN_SOESH'. mc_add 'CEBN_SOESH_FMAP'.
  mc_add 'CFD_L_CHANGE_LOG'. mc_add 'CFD_L_DEL'. mc_add 'CFD_L_DEL_BUS'.
  mc_add 'CFD_L_DEL_CDS'. mc_add 'CFD_L_DEL_CODE'.
  mc_add 'CFD_L_DEL_CODE_T'. mc_add 'CFD_L_DEL_ENH'.
  mc_add 'CFD_L_DEL_ODATA'. mc_add 'CFD_L_DEL_SCN'.
  mc_add 'CFD_L_DEL_SCN_SP'. mc_add 'CFD_L_DEL_T'.
  mc_add 'CFD_L_DEL_TRF'. mc_add 'CFD_L_ENH_REF'.
  mc_add 'CFD_L_FIELD_STAT'. mc_add 'CFD_L_GEN_LOG'. mc_add 'CFD_L_LOG'.
  mc_add 'CFD_L_LOG_F'. mc_add 'CFD_L_RT'. mc_add 'CFD_L_RT_BC_FLDS'.
  mc_add 'CFD_L_RT_CDS_INV'. mc_add 'CFD_L_RT_CDS_USG'.
  mc_add 'CFD_L_RT_CONTEXT'. mc_add 'CFD_L_RT_ENTITY'.
  mc_add 'CFD_L_RT_FIELDS'. mc_add 'CFD_L_RT_FLD_INV'.
  mc_add 'CFD_L_RT_TRF'. mc_add 'CFD_L_RT_TRF_CF'.
  mc_add 'CFD_L_RT_TRF_FLD'. mc_add 'CFD_METRICS_BUFR'.
  mc_add 'CFD_W_BADI'. mc_add 'CFD_W_BADI_CTXT'.
  mc_add 'CFD_W_BUS_CTXT'. mc_add 'CFD_W_BUS_CTXT_T'.
  mc_add 'CFD_W_CDS'. mc_add 'CFD_W_CDS_CTXT'. mc_add 'CFD_W_CDS_CXT'.
  mc_add 'CFD_W_CDS_T'. mc_add 'CFD_W_ODATA'. mc_add 'CFD_W_ODATA_CT_T'.
  mc_add 'CFD_W_ODATA_CTXT'. mc_add 'CFD_W_ODATA_T'. mc_add 'CFD_W_REP'.
  mc_add 'CFD_W_REP_BUS'. mc_add 'CFD_W_REP_CDS'.
  mc_add 'CFD_W_REP_CODE'. mc_add 'CFD_W_REP_CODE_T'.
  mc_add 'CFD_W_REP_ENH'. mc_add 'CFD_W_REP_ODATA'.
  mc_add 'CFD_W_REP_SCN'. mc_add 'CFD_W_REP_SCN_SP'.
  mc_add 'CFD_W_REP_T'. mc_add 'CFD_W_REP_TRF'. mc_add 'CFD_W_SCEN'.
  mc_add 'CFD_W_SCEN_STEP'. mc_add 'CFD_W_SCEN_T'. mc_add 'CFD_W_TRANS'.
  mc_add 'CFD_W_TRANS_T'. mc_add 'CFONT_CHAR_WIDTH'.
  mc_add 'CFONT_INFO'. mc_add 'CFONT_MAINLOG'. mc_add 'CFONT_MAPPING'.
  mc_add 'CFONT_TEMP_FILE'. mc_add 'CFUNICODE_BLOCK'. mc_add 'CHGRP'.
  mc_add 'CHIP_CACHE_HDR'. mc_add 'CHIP_CACHE_HDRT'.
  mc_add 'CHIP_CACHE_PAGE'. mc_add 'CHIP_CACHE_PAGEH'.
  mc_add 'CHIP_CACHE_PARA'. mc_add 'CHIP_CACHE_STAT'.
  mc_add 'CHIP_CACHE_TAGS'. mc_add 'CHIP_PROVIDER'.
  mc_add 'CHIP_PROVIDERT'. mc_add 'CHIP_TEST_TAGS'. mc_add 'CIMHIS'.
  mc_add 'CIMSYN'. mc_add 'CLASS_DEF'. mc_add 'CLASS_DELE'.
  mc_add 'CLASS_INCL'. mc_add 'CLB2C_APPL'. mc_add 'CLB2C_APPL_CONT'.
  mc_add 'CLB2C_APPL_CONTT'. mc_add 'CLB2C_APPL_T'.
  mc_add 'CLB2C_AUTH_CONT'. mc_add 'CLB2C_AUTH_CONTT'.
  mc_add 'CLB2C_AUTH_METH'. mc_add 'CLB2C_AUTH_METHT'.
  mc_add 'CLB2C_ENT_TYPE'. mc_add 'CLB2C_ENT_TYPE_T'.
  mc_add 'CLB2C_IC_DISP'. mc_add 'CLB2C_IC_DISP_T'.
  mc_add 'CLB2C_PLATF_DEF'. mc_add 'CLB2C_PLATF_DEFT'.
  mc_add 'CLB2C_PLATF_METH'. mc_add 'CLB2C_PTYPE'.
  mc_add 'CLB2C_PTYPE_CF'. mc_add 'CLB2C_PTYPE_METH'.
  mc_add 'CLB2C_PTYPE_METT'. mc_add 'CLB2C_PTYPE_VERS'.
  mc_add 'CLB2S_GW_NOTIFY'. mc_add 'CLB2S_GW_SRV'.
  mc_add 'CLB2S_GW_SRV_REL'. mc_add 'CLB2S_SERVICE'.
  mc_add 'CLB2S_SRV_PTYPE'. mc_add 'CLBC_APPLI'.
  mc_add 'CLBC_APPLI_CONT'. mc_add 'CLBC_APPLI_CONTT'.
  mc_add 'CLBC_APPLI_T'. mc_add 'CLBC_AUTH_CONT'.
  mc_add 'CLBC_AUTH_CONT_T'. mc_add 'CLBC_AUTH_METH'.
  mc_add 'CLBC_AUTH_METH_T'. mc_add 'CLBC_ENTITY_TYPE'.
  mc_add 'CLBC_ENTITY_TYPT'. mc_add 'CLBC_IC_DISP'.
  mc_add 'CLBC_IC_DISP_T'. mc_add 'CLBC_PLATF_DEF'.
  mc_add 'CLBC_PLATF_DEF_T'. mc_add 'CLBC_PLATF_METH'.
  mc_add 'CLBC_PTYPE'. mc_add 'CLBC_PTYPE_CF'. mc_add 'CLBC_PTYPE_METH'.
  mc_add 'CLBC_PTYPE_METHT'. mc_add 'CLBC_PTYPE_VERS'.
  mc_add 'CLBS_GW_NOTIFY'. mc_add 'CLBS_GW_SRV'.
  mc_add 'CLBS_SRV_PTYPE'. mc_add 'CLEANUP_STATUS'. mc_add 'CLMCUS'.
  mc_add 'CLMS_CRP_CMP_REG'. mc_add 'CLMS_CRP_CMP_RGL'.
  mc_add 'CLMS_TM_VTR'. mc_add 'CLMS_TM_VTR_NUM'.
  mc_add 'CLMS_TM_VTR_OBJ'. mc_add 'CLMS_W_HC'.
  mc_add 'CLMS_W_HC_EVENT'. mc_add 'CLMS_W_HC_EVENTT'.
  mc_add 'CLMS_W_HC_T'. mc_add 'CLMS_W_TM'. mc_add 'CLNT_CVRS2'.
  mc_add 'CLS_ASSIGNMENT'. mc_add 'CLS_ATTR_GROUP'.
  mc_add 'CLS_ATTR_GROUPT'. mc_add 'CLS_ATTR_VALUE'.
  mc_add 'CLS_ATTR_VALUET'. mc_add 'CLS_ATTRIBUTE'.
  mc_add 'CLS_ATTRIBUTET'. mc_add 'CLS_FAVORITES'.
  mc_add 'CLS_LINKED_OBJ'. mc_add 'CLS_RE_ATTRIBUTE'.
  mc_add 'CLS_RE_CONDITION'. mc_add 'CLS_REPORT_DEF'.
  mc_add 'CLS_REPORT_DEFT'. mc_add 'CLS_RUN'. mc_add 'CLS_RUN_ASSGNMNT'.
  mc_add 'CLS_RUN_COUNTERS'. mc_add 'CLS_RUN_DISTR'.
  mc_add 'CLS_RUN_OBJECTS'. mc_add 'CLS_RUN_PARAMS'.
  mc_add 'CLS_SETTINGS'. mc_add 'CLS_SOURCE_TYPE'.
  mc_add 'CLS_SOURCE_TYPET'. mc_add 'CLS_TRANSL_DEPTH'.
  mc_add 'CLS_TRANSL_DEPTT'. mc_add 'CLS_TRANSL_GRAPH'.
  mc_add 'CLS_TYGR_ELEMENT'. mc_add 'CLS_TYPE_GROUP'.
  mc_add 'CLS_TYPE_GROUPT'. mc_add 'CMOJOBS'. mc_add 'CMP_EXCEPT'.
  mc_add 'CMP_RUNS'. mc_add 'CMPOBJ'. mc_add 'CMPPARAM'. mc_add 'CMPWL'.
  mc_add 'CMPWLE'. mc_add 'CMPWLEC'. mc_add 'CMPWLH'. mc_add 'CMPWLIMP'.
  mc_add 'CMPWLK'. mc_add 'CMSALOGAR'. mc_add 'CMSALOGHDR'.
  mc_add 'CMSALOGT'. mc_add 'CMSDCOMP'. mc_add 'CMSDLIFECYCLE'.
  mc_add 'CMSDPUBLISH'. mc_add 'CMSDSTATE'. mc_add 'CMSDSUBSCRIBE'.
  mc_add 'CMSDSYS'. mc_add 'CMSDSYSCOMP'. mc_add 'CMSDSYSVERS'.
  mc_add 'CMSDTRANSITION'. mc_add 'CMSEFLOW'. mc_add 'CMSEWORKLIST'.
  mc_add 'CMSIDES'. mc_add 'CMSIDOM'. mc_add 'CMSIGUID'.
  mc_add 'CMSISYS'. mc_add 'CMSSACTFILES'. mc_add 'CMSSDEPLOYINFO'.
  mc_add 'CMSSSDMSTAGING'. mc_add 'CMSSSDMSYS'. mc_add 'CMSSSTAGING'.
  mc_add 'CMSSSYSPROP'. mc_add 'CMSSYSTEMT'. mc_add 'CMSUANOTES'.
  mc_add 'CMSUCFGUSER'. mc_add 'CMTXLOIO'. mc_add 'CMTXLOIOT'.
  mc_add 'CMTXLOPR'. mc_add 'CMTXLORE'. mc_add 'CMTXLOREPR'.
  mc_add 'CMTXLORI'. mc_add 'CMTXLORIPR'. mc_add 'CNTLREM'.
  mc_add 'CNTLSTRASC'. mc_add 'CNTLSTREAM'. mc_add 'CNTLSTRINF'.
  mc_add 'CNTLSTRLIS'. mc_add 'CNTLWARN'. mc_add 'COL_INDX_CACHE'.
  mc_add 'COL_VERSION'. mc_add 'COLORSEXC'. mc_add 'COMP_LAYER'.
  mc_add 'COMP_SYS'. mc_add 'COMP_TYPES'. mc_add 'CONNECT_XI_BS_IS'.
  mc_add 'CONNECT_XI_IS_BS'. mc_add 'CONTAINERINFO'. mc_add 'CONTCOMP'.
  mc_add 'CONTCOMPT'. mc_add 'CONTCOMPX'. mc_add 'CONTCOMPXT'.
  mc_add 'CONTROLS'. mc_add 'COV_SHM_FLUSH'. mc_add 'COVDATCOL'.
  mc_add 'COVDATVOL'. mc_add 'COVDETVAR'. mc_add 'COVLOG'.
  mc_add 'COVMODSET'. mc_add 'COVMVDAT'. mc_add 'COVMVHID'.
  mc_add 'COVMVHIS'. mc_add 'COVMVSET'. mc_add 'COVMVTIM'.
  mc_add 'COVREG'. mc_add 'COVRES'. mc_add 'COVRES0'.
  mc_add 'COVRES0_CHECK'. mc_add 'COVRES0_SYNC'. mc_add 'COVRES0_SYNCH'.
  mc_add 'COVRFCERR'. mc_add 'COVSETTING'. mc_add 'COVTKEY'.
  mc_add 'COVTKEYDEL'. mc_add 'CPETASKLOG'. mc_add 'CPH_TRACE'.
  mc_add 'CPT_ACTION'. mc_add 'CPT_ALLOC_ID'. mc_add 'CPT_CORR_DEF'.
  mc_add 'CPT_CORRELATOR'. mc_add 'CPT_LAYER'. mc_add 'CPT_METRIC'.
  mc_add 'CPT_METRIC_DEF'. mc_add 'CPT_MODULE'. mc_add 'CPT_ORIGIN'.
  mc_add 'CPT_PROVIDER'. mc_add 'CPT_RECORD'. mc_add 'CPT_RECORD_CORR'.
  mc_add 'CPT_REQ_META'. mc_add 'CPT_THREAD'. mc_add 'CRATEXT'.
  mc_add 'CREP'. mc_add 'CREP_CERT'. mc_add 'CREP_FILE'.
  mc_add 'CREP_FNGP'. mc_add 'CREP_FTP'. mc_add 'CREP_HTTP'.
  mc_add 'CREP_LREP'. mc_add 'CREP_R3DB'. mc_add 'CREP_RACL'.
  mc_add 'CREP_RFC'. mc_add 'CREP_RPC'. mc_add 'CREP_STOR'.
  mc_add 'CREP_TREE'. mc_add 'CREPDESCR'. mc_add 'CREPDOCSP'.
  mc_add 'CRM_CATJ'. mc_add 'CRM_CATL'. mc_add 'CRM_CATM'.
  mc_add 'CRM_CATT'. mc_add 'CRM_CATT_LOGS'. mc_add 'CRM_CATT_MSG'.
  mc_add 'CRM_CHECK_CONFIG'. mc_add 'CRM_EC_SESSIONS'.
  mc_add 'CRM_ECATT'. mc_add 'CRM_ECATT_LOGS'. mc_add 'CRM_ECATT_MSG'.
  mc_add 'CRM_ECLOG_DATA'. mc_add 'CRM_ECLOG_HEAD'.
  mc_add 'CRM_ECLOG_SCNT'. mc_add 'CRM_ECLOG_SCR'.
  mc_add 'CRM_ECLOG_XDAT'. mc_add 'CRM_TCKADDCOMPL'.
  mc_add 'CRM_TCKADDCOMPS'. mc_add 'CRM_TCKCOMP4PROD'.
  mc_add 'CRM_TCKCOMPS'. mc_add 'CRM_TCKDBSYS'. mc_add 'CRM_TCKHOSTS'.
  mc_add 'CRM_TCKINSTANCES'. mc_add 'CRM_TCKITERAPROD'.
  mc_add 'CRM_TCKPRODUCTS'. mc_add 'CRM_TCKSOLPROD'.
  mc_add 'CRM_TCKSOLUTIONS'. mc_add 'CRM_TCKSYSTEMS'.
  mc_add 'CRM_VERI_LOGS'. mc_add 'CRMAHR'. mc_add 'CRMAHRA'.
  mc_add 'CRMAHRROL'. mc_add 'CRMAHRROLT'. mc_add 'CRMCACHE'.
  mc_add 'CRMCHK'. mc_add 'CRMCHKCOMP'. mc_add 'CRMCHKCSN'.
  mc_add 'CRMCHKENV'. mc_add 'CRMCHKENVE'. mc_add 'CRMCHKENVL'.
  mc_add 'CRMCHKEXC'. mc_add 'CRMCHKHST'. mc_add 'CRMCHKMSG'.
  mc_add 'CRMCHKMSG_LOCAL'. mc_add 'CRMCHKMSG_REMOTE'.
  mc_add 'CRMCHKMSGP'. mc_add 'CRMCHKMSGT'. mc_add 'CRMCHKOBCO'.
  mc_add 'CRMCHKOBJ'. mc_add 'CRMCHKOBJA'. mc_add 'CRMCHKOBJH'.
  mc_add 'CRMCHKOBJR'. mc_add 'CRMCHKOBJT'. mc_add 'CRMCHKOBJU'.
  mc_add 'CRMCHKOBTY'. mc_add 'CRMCHKPRF'. mc_add 'CRMCHKPRFH'.
  mc_add 'CRMCHKPRFI'. mc_add 'CRMCHKPRFT'. mc_add 'CRMCHKRES'.
  mc_add 'CRMCHKRESA'. mc_add 'CRMCHKRESL'. mc_add 'CRMCHKRUN'.
  mc_add 'CRMCHKRUNC'. mc_add 'CRMCHKRUNHOOK'. mc_add 'CRMCHKRUNJ'.
  mc_add 'CRMCHKRUNM'. mc_add 'CRMCHKRUNO'. mc_add 'CRMCHKRUNT'.
  mc_add 'CRMCHKST'. mc_add 'CRMCHKT'. mc_add 'CRMEMTCID'.
  mc_add 'CRMEXCRSN'. mc_add 'CRMEXCRSNT'. mc_add 'CRMEXPVERS'.
  mc_add 'CRMLOG'. mc_add 'CRMLOGA'. mc_add 'CRMLOGAT'.
  mc_add 'CRMMSGGRP'. mc_add 'CRMMSGGRPT'. mc_add 'CRMOBJCOL'.
  mc_add 'CRMOBJCOLT'. mc_add 'CRMPRIOCL'. mc_add 'CRMPRIOS'.
  mc_add 'CRMQUERYPACKAGE'. mc_add 'CRMRDDOKIL'. mc_add 'CRMRDDOKTL'.
  mc_add 'CRMRDT100'. mc_add 'CRMRDTPOOL'. mc_add 'CRMRDTRMSG'.
  mc_add 'CRMREQ'. mc_add 'CRMREQRUL'. mc_add 'CRMREQSEL'.
  mc_add 'CRMRSFACMP'. mc_add 'CRMSDPCOLLOG'. mc_add 'CRMSDPOBJCACHE'.
  mc_add 'CRMSTAT'. mc_add 'CRMSTATFLD'. mc_add 'CROSS'.
  mc_add 'CRR_ACCESS_PLAN'. mc_add 'CRR_DELIM_FIELD'.
  mc_add 'CRR_MDS_SOBJ'. mc_add 'CRR_MDS_SOBJK'.
  mc_add 'CRR_MDS_TRANSP'. mc_add 'CRR_SBCS_E05'. mc_add 'CRR_SBCS_E06'.
  mc_add 'CRR_SBCS_E08'. mc_add 'CRR_SBCS005'. mc_add 'CRR_SBCS006'.
  mc_add 'CRR_SBCS008'. mc_add 'CRR_SBCSE05'. mc_add 'CRR_SBCSE06'.
  mc_add 'CRR_SBCSE08'. mc_add 'CRR4TABLES'. mc_add 'CRRERROR'.
  mc_add 'CRRPARAMETERS'. mc_add 'CRRPARATEST'. mc_add 'CRRRTI'.
  mc_add 'CRRRTIT'. mc_add 'CRRSCENARIOS'. mc_add 'CRRTASKCOUNT'.
  mc_add 'CRRTASKHIST'. mc_add 'CRRTASKINFO'. mc_add 'CSL_AO'.
  mc_add 'CSL_CCL'. mc_add 'CSL_CCP'. mc_add 'CSL_EOAC'.
  mc_add 'CSL_EOME'. mc_add 'CSL_EOMP'. mc_add 'CSL_EOTY'.
  mc_add 'CSL_M'. mc_add 'CSL_MEOM'. mc_add 'CSL_MTLK'.
  mc_add 'CSL_UBR'. mc_add 'CSM_ABOSCR'. mc_add 'CSM_ASC'.
  mc_add 'CSM_CLSINH'. mc_add 'CSM_CLST'. mc_add 'CSM_CNTXT'.
  mc_add 'CSM_EVENTS'. mc_add 'CSM_EVNTST'. mc_add 'CSM_EXTVAL'.
  mc_add 'CSM_OBJ'. mc_add 'CSM_PRPINH'. mc_add 'CSM_TEXTS'.
  mc_add 'CSM_TEXTS2'. mc_add 'CSM_TOKT'. mc_add 'CSMBK'.
  mc_add 'CSMBK_ASC'. mc_add 'CSMBK_CL'. mc_add 'CSMBK_CL2'.
  mc_add 'CSMBK_CLST'. mc_add 'CSMBK_OB2'. mc_add 'CSMBK_OBJ'.
  mc_add 'CSMBK_PROT'. mc_add 'CSMBK_SEM'. mc_add 'CSMBK_SEMT'.
  mc_add 'CSMBK_TK'. mc_add 'CSMBK_TK2'. mc_add 'CSMBK_TOKT'.
  mc_add 'CSMCENTOOL'. mc_add 'CSMCLDISPT'. mc_add 'CSMCLGUID2'.
  mc_add 'CSMCLSGUID'. mc_add 'CSMCLSINH2'. mc_add 'CSMCLSMAP'.
  mc_add 'CSMCLSMAP2'. mc_add 'CSMCONTX'. mc_add 'CSMNIPINGCFG'.
  mc_add 'CSMNSDIC'. mc_add 'CSMNUDATA'. mc_add 'CSMNUMETH'.
  mc_add 'CSMPRPINH2'. mc_add 'CSMSCRMETH'. mc_add 'CSMSEGM'.
  mc_add 'CSMSEGMX'. mc_add 'CSMSYS'. mc_add 'CSMSYSAS'.
  mc_add 'CSMTOOLASG'. mc_add 'CSMVIEWDT'. mc_add 'CTIMPEXC'.
  mc_add 'CTIMPORT'. mc_add 'CTS_AIM_TRACE'. mc_add 'CTS_AIM_TRC_OBJ'.
  mc_add 'CTS_AIM_TRC_PTSH'. mc_add 'CTS_AIM_TRC_STM'.
  mc_add 'CTS_AIM_TRC_UPG'. mc_add 'CTS_APPLIDENTS'.
  mc_add 'CTS_APPLIDENTST'. mc_add 'CTS_APPLTYPE'.
  mc_add 'CTS_APPLTYPET'. mc_add 'CTS_COL_ASSIGNS'.
  mc_add 'CTS_DEPL_USERS'. mc_add 'CTS_EXT_PS'. mc_add 'CTS_EXT_PT'.
  mc_add 'CTS_FILELOCK'. mc_add 'CTS_GROUPS'. mc_add 'CTS_HOT_OBJECT'.
  mc_add 'CTS_HOT_OTEXTS_H'. mc_add 'CTS_HOT_OTEXTS_L'.
  mc_add 'CTS_HOT_OTEXTS_S'. mc_add 'CTS_HOT_PACKAGE'.
  mc_add 'CTS_HOT_PARAMS'. mc_add 'CTS_HOT_PREWORK'.
  mc_add 'CTS_ILL_ACC_AIM'. mc_add 'CTS_ILL_ACC_DETA'.
  mc_add 'CTS_ILL_ACC_OBJ'. mc_add 'CTS_ILL_ACC_STAC'.
  mc_add 'CTS_IMEXP_LOCK'. mc_add 'CTS_IMP_Q_STATE'.
  mc_add 'CTS_IMPREQ_LAST'. mc_add 'CTS_IMPREQ_RELAT'.
  mc_add 'CTS_ORG_INSTALL'. mc_add 'CTS_PLUGIN'.
  mc_add 'CTS_PLUGIN_POOL'. mc_add 'CTS_PLUGIN_REG'.
  mc_add 'CTS_PLUGINS'. mc_add 'CTS_PRSTAT'. mc_add 'CTS_PRSW'.
  mc_add 'CTS_PRSWT'. mc_add 'CTS_PV_ALL'. mc_add 'CTS_PV_CMP'.
  mc_add 'CTS_PV_PPP'. mc_add 'CTS_QA_APPROVED'. mc_add 'CTS_QA_LOCK'.
  mc_add 'CTS_QA_MAIL_NOTE'. mc_add 'CTS_QA_SYSTEMS'.
  mc_add 'CTS_QA_WORK'. mc_add 'CTS_QA_WORKL'. mc_add 'CTS_QUEUE_CNTRL'.
  mc_add 'CTS_REQ_OVERLAPS'. mc_add 'CTS_RQ_APPLID'.
  mc_add 'CTS_RQ_APPLIDT'. mc_add 'CTS_SCHED_LOCK'. mc_add 'CTS_SERIAL'.
  mc_add 'CTS_SY_DETAILS'. mc_add 'CTS_TPROPS'.
  mc_add 'CTSFILECONTAINER'. mc_add 'CTSFILESETTINGS'.
  mc_add 'CTSOBJLIST'. mc_add 'CTSOBJLISTT'. mc_add 'CTSOBJPROS'.
  mc_add 'CTSPROJECT'. mc_add 'CTSUSRCUST'. mc_add 'CTWFACTLOG'.
  mc_add 'CTWFNUMBER'. mc_add 'CTWFPROPOS'. mc_add 'CTWFREQUST'.
  mc_add 'CTWFTARGET'. mc_add 'CUATEXTS'. mc_add 'CUS_ACTEXT'.
  mc_add 'CUS_ACTH'. mc_add 'CUS_ACTOBJ'. mc_add 'CUS_ACTOBT'.
  mc_add 'CUS_ACTT'. mc_add 'CUS_AKH'. mc_add 'CUS_ATRCOU'.
  mc_add 'CUS_ATRH'. mc_add 'CUS_ATRT'. mc_add 'CUS_ATRVCO'.
  mc_add 'CUS_ATRVSE'. mc_add 'CUS_COUNT'. mc_add 'CUS_IMGACH'.
  mc_add 'CUS_IMGACT'. mc_add 'CUS_INDU'. mc_add 'CUS_SYST'.
  mc_add 'CUSAB'. mc_add 'CUSAC'. mc_add 'CUSAC1'. mc_add 'CUSAC1T'.
  mc_add 'CUSAC2'. mc_add 'CUSAC2T'. mc_add 'CUSAC3'. mc_add 'CUSAC3T'.
  mc_add 'CUSADP'. mc_add 'CUSADP40'. mc_add 'CUSAH'. mc_add 'CUSAL'.
  mc_add 'CUSAMEN'. mc_add 'CUSAMENT'. mc_add 'CUSDEFAULT'.
  mc_add 'CUSDVCLANG'. mc_add 'CUSDVCORIG'. mc_add 'CUSTCHKF'.
  mc_add 'CUSTCHKO'. mc_add 'CUSTCONT1'. mc_add 'CUSTIDXSTA'.
  mc_add 'CUSTLOIO'. mc_add 'CUSTLOIOT'. mc_add 'CUSTLOPR'.
  mc_add 'CUSTLORE'. mc_add 'CUSTLORI'. mc_add 'CUSTNODE'.
  mc_add 'CUSTNODER'. mc_add 'CUSTNODET'. mc_add 'CUSTPHF'.
  mc_add 'CUSTPHHR'. mc_add 'CUSTPHIO'. mc_add 'CUSTPHNM'.
  mc_add 'CUSTPHPR'. mc_add 'CUSTPHRE'. mc_add 'CUSTPHRI'.
  mc_add 'CVERS'. mc_add 'CVERS_ACT'. mc_add 'CVERS_LAN'.
  mc_add 'CVERS_REF'. mc_add 'CVERS_SUB'. mc_add 'CVERS_TXT'.
  mc_add 'CVERS1'. mc_add 'CWB_HIST_D'. mc_add 'CWB_HIST_H'.
  mc_add 'CWB_HIST_O'. mc_add 'CWB_SP_WOV'. mc_add 'CWBCICATTR'.
  mc_add 'CWBCICONFIRMLOC'. mc_add 'CWBCIDATA'. mc_add 'CWBCIDATAOBJ'.
  mc_add 'CWBCIDPNDC'. mc_add 'CWBCIFIXED'. mc_add 'CWBCIHEAD'.
  mc_add 'CWBCIINVLD'. mc_add 'CWBCINSTATTR'. mc_add 'CWBCIOBJ'.
  mc_add 'CWBCIVALID'. mc_add 'CWBCMLAST'. mc_add 'CWBCMPNT'.
  mc_add 'CWBCMTEXT'. mc_add 'CWBDEEQUIV'. mc_add 'CWBDEHEAD'.
  mc_add 'CWBDEPRDC'. mc_add 'CWBDETRACK'. mc_add 'CWBMODILOG'.
  mc_add 'CWBNTCI'. mc_add 'CWBNTCONT'. mc_add 'CWBNTCUST'.
  mc_add 'CWBNTDATA'. mc_add 'CWBNTFIXED'. mc_add 'CWBNTGATTR'.
  mc_add 'CWBNTHEAD'. mc_add 'CWBNTLOG'. mc_add 'CWBNTMSG'.
  mc_add 'CWBNTSTATT'. mc_add 'CWBNTSTATV'. mc_add 'CWBNTSTXT'.
  mc_add 'CWBNTVALID'. mc_add 'CWBPRSTATT'. mc_add 'CWBPRSTATV'.
  mc_add 'CWBRFCCACH'. mc_add 'CWBRFCUSR'. mc_add 'CWBVDBSCFG'.
  mc_add 'D010CDSTFUNCDEL'. mc_add 'D010CDSTFUNCDEP'.
  mc_add 'D010CDSTFUNCLOCK'. mc_add 'D010DBOBJ'. mc_add 'D010DBOBJDEL'.
  mc_add 'D010DBOBJDEP'. mc_add 'D010DBOBJGEN'. mc_add 'D010INC'.
  mc_add 'D010TAB'. mc_add 'D020S'. mc_add 'D020T'. mc_add 'D021T'.
  mc_add 'D0GEN'. mc_add 'D1GEN'. mc_add 'D301T'. mc_add 'D344L'.
  mc_add 'D347T'. mc_add 'DAAG_BRF_ID_INFO'. mc_add 'DAAG_GRP'.
  mc_add 'DAAG_GRP_TAB'. mc_add 'DAAG_GRP_TXT'. mc_add 'DAAG_OBJ_REUSE'.
  mc_add 'DAAG_OBJ_RUL_FLD'. mc_add 'DAAG_OBJ_TAB_REL'.
  mc_add 'DAAG_OBJECT_BRF'. mc_add 'DAAG_OBJECTS'.
  mc_add 'DAAG_OBJECTS_BAD'. mc_add 'DAAG_OBJECTS_DEF'.
  mc_add 'DAAG_OBJECTS_TXT'. mc_add 'DAAG_PTO'. mc_add 'DAAG_PTO_LOG'.
  mc_add 'DAAG_PTO_TAB'. mc_add 'DAAG_PTO_TXT'. mc_add 'DAAG_TEMP_DATA'.
  mc_add 'DARTT'. mc_add 'DB02_COLL_LOG'. mc_add 'DB02_COLL_PLAN'.
  mc_add 'DB02N_ANA'. mc_add 'DB02N_ANAT'. mc_add 'DB2_OPTIM_PROFIL'.
  mc_add 'DB2ALERTSLIMITS'. mc_add 'DB2BPTUNE'. mc_add 'DB2CCACCOR'.
  mc_add 'DB2CCACCOX'. mc_add 'DB2CCDAILY'. mc_add 'DB2CCDAILYX'.
  mc_add 'DB2CCDL_OUTTS'. mc_add 'DB2CCDL_PARMS'. mc_add 'DB2CCDS_IN'.
  mc_add 'DB2CCDS_PARMS'. mc_add 'DB2CCHOUR'. mc_add 'DB2CCMO_ERROR'.
  mc_add 'DB2CCMO_IN1'. mc_add 'DB2CCMO_IN2'. mc_add 'DB2CCMO_OBJERR'.
  mc_add 'DB2CCMO_OUTTS'. mc_add 'DB2CCMO_PARMS'. mc_add 'DB2CONVTOOL'.
  mc_add 'DB2COPYI'. mc_add 'DB2DB02CONTROL'. mc_add 'DB2DB02EXTENTS'.
  mc_add 'DB2DB02IX_SUM'. mc_add 'DB2DB02IXDYN'. mc_add 'DB2DB02IXSIZE'.
  mc_add 'DB2DB02MISSOBJ'. mc_add 'DB2DB02SUMMARY'.
  mc_add 'DB2DB02TBDYN'. mc_add 'DB2DB02TBSIZE'.
  mc_add 'DB2DB02TBTOPGROW'. mc_add 'DB2DB02TS_HIS'.
  mc_add 'DB2DB02TS_SUM'. mc_add 'DB2DB02TSDYN'. mc_add 'DB2DB02TSSIZE'.
  mc_add 'DB2DB02VOLFREE'. mc_add 'DB2IREORG'. mc_add 'DB2IXBACK'.
  mc_add 'DB2JOB'. mc_add 'DB2JOBIDMAP'. mc_add 'DB2JSINF'.
  mc_add 'DB2JSTATUS'. mc_add 'DB2JU'. mc_add 'DB2LODLK'.
  mc_add 'DB2LODRS'. mc_add 'DB2LOGS'. mc_add 'DB2LOTHW'.
  mc_add 'DB2LOTRS'. mc_add 'DB2LRUR'. mc_add 'DB2LSCONFIG'.
  mc_add 'DB2MISC'. mc_add 'DB2MON'. mc_add 'DB2NETSTATS'.
  mc_add 'DB2NORUN'. mc_add 'DB2REOIX'. mc_add 'DB2REOTS'.
  mc_add 'DB2RFCCONN'. mc_add 'DB2SAPSYS'. mc_add 'DB2TBLX'.
  mc_add 'DB2THREAD2'. mc_add 'DB2TREORG'. mc_add 'DB2TSBACK'.
  mc_add 'DB2TSSIZE_T'. mc_add 'DB2UTILS_OUT'. mc_add 'DB2UTILS_PARMS'.
  mc_add 'DB2UTPARM'. mc_add 'DB2ZPARMCONFIG'. mc_add 'DB4_CLUSTER'.
  mc_add 'DB4BACKUP'. mc_add 'DB4INDEX_HIST'. mc_add 'DB4INDEX_STAT'.
  mc_add 'DB4OX'. mc_add 'DB4TABLE_HIST'. mc_add 'DB4TABLE_STAT'.
  mc_add 'DB6_BW_HEALTH'. mc_add 'DB6_MDC_ADV_PART'.
  mc_add 'DB6_MDC_ADV_RES'. mc_add 'DB6_MDC_ADV_STAT'.
  mc_add 'DB6_MDC_ADV_TMPT'. mc_add 'DB6_MDC_ADV_TSHL'.
  mc_add 'DB6_OPTPROFILE'. mc_add 'DB6_PC_INTENDED'.
  mc_add 'DB6_RESET_DATA'. mc_add 'DB6ALRTCFG'. mc_add 'DB6ALRTCT'.
  mc_add 'DB6ALRTMSG'. mc_add 'DB6AUDITDT'. mc_add 'DB6AUDITHD'.
  mc_add 'DB6CSTRACE'. mc_add 'DB6GSDTBS'. mc_add 'DB6HISTBS'.
  mc_add 'DB6INSPECT'. mc_add 'DB6IREORG'. mc_add 'DB6JOBTAB'.
  mc_add 'DB6MON'. mc_add 'DB6NAVSYST'. mc_add 'DB6PM_CLN'.
  mc_add 'DB6PM_DLK'. mc_add 'DB6PM_TIME'. mc_add 'DB6PMCK_P'.
  mc_add 'DB6PMHCDM'. mc_add 'DB6PMHSB'. mc_add 'DB6PMHSD'.
  mc_add 'DB6PMHST'. mc_add 'DB6PMHT'. mc_add 'DB6PMHT_HD'.
  mc_add 'DB6PMHXP'. mc_add 'DB6PMHXPHD'. mc_add 'DB6PMHXT'.
  mc_add 'DB6PMHXTHD'. mc_add 'DB6PMPROT'. mc_add 'DB6PMSQ_DBS'.
  mc_add 'DB6SCRIPT'. mc_add 'DB6TREORG'. mc_add 'DBA_ALRT_MET'.
  mc_add 'DBA_ALRT_MET_C'. mc_add 'DBA_ALRT_MET_R'.
  mc_add 'DBA_CHK_IMPACTS'. mc_add 'DBA_CHK_MSG_VARS'.
  mc_add 'DBA_CHK_RESULTS'. mc_add 'DBA_CONFIG'.
  mc_add 'DBA_DBH_CAT_TEXT'. mc_add 'DBA_DBH_CATEGORY'.
  mc_add 'DBA_DBH_COLL_CFG'. mc_add 'DBA_DBH_COLL_JOB'.
  mc_add 'DBA_DBH_COLL_OPT'. mc_add 'DBA_DBH_COLL_RUN'.
  mc_add 'DBA_DBH_COLL_WRK'. mc_add 'DBA_DBH_REP_TEXT'.
  mc_add 'DBA_DBH_REPORT'. mc_add 'DBA_DBH_SRV_DBSL'.
  mc_add 'DBA_DBH_TPL'. mc_add 'DBA_DBH_TPL_COLL'.
  mc_add 'DBA_DBH_TPL_OPT'. mc_add 'DBA_DBH_TPL_TEXT'.
  mc_add 'DBA_GEN_DYN'. mc_add 'DBA_LOG_DETAIL'.
  mc_add 'DBA_LOG_HEADER'. mc_add 'DBA_TEST_DATA'.
  mc_add 'DBA_TEST_RESULTS'. mc_add 'DBA_TEST_SET'.
  mc_add 'DBA_TEST_SYSTEMS'. mc_add 'DBA_XCHG_DATA'. mc_add 'DBABARL'.
  mc_add 'DBABD'. mc_add 'DBABL'. mc_add 'DBACUST'. mc_add 'DBADFL'.
  mc_add 'DBAERR'. mc_add 'DBAEXTL'. mc_add 'DBAFID'. mc_add 'DBAFIZ'.
  mc_add 'DBAGRP'. mc_add 'DBAML'. mc_add 'DBAN'. mc_add 'DBAOBJL'.
  mc_add 'DBAOPTL'. mc_add 'DBAPHAL'. mc_add 'DBARCL'. mc_add 'DBAREOL'.
  mc_add 'DBASPAL'. mc_add 'DBATID'. mc_add 'DBATL'. mc_add 'DBATRIAL'.
  mc_add 'DBCHECKDB2'. mc_add 'DBCHECKORA'. mc_add 'DBCHK'.
  mc_add 'DBCON'. mc_add 'DBCONUSR'. mc_add 'DBDATA'.
  mc_add 'DBDCDEPTB'. mc_add 'DBDCGENTB'. mc_add 'DBDIFF'.
  mc_add 'DBDIFFINDX'. mc_add 'DBDIFFINFO'. mc_add 'DBH_CONFIG'.
  mc_add 'DBMAPS'. mc_add 'DBMSGDB2'. mc_add 'DBMSGORA'. mc_add 'DBOBJ'.
  mc_add 'DBPROPERTIES'. mc_add 'DBSMSGORA'. mc_add 'DBSNP'.
  mc_add 'DBSTAIHADA'. mc_add 'DBSTAIHDB2'. mc_add 'DBSTAIHDB4'.
  mc_add 'DBSTAIHORA'. mc_add 'DBSTATC'. mc_add 'DBSTATHADA'.
  mc_add 'DBSTATHDB2'. mc_add 'DBSTATHDB4'. mc_add 'DBSTATHDB6'.
  mc_add 'DBSTATHINF'. mc_add 'DBSTATHORA'. mc_add 'DBSTATIADA'.
  mc_add 'DBSTATIDB2'. mc_add 'DBSTATIDB4'. mc_add 'DBSTATIDB6'.
  mc_add 'DBSTATIORA'. mc_add 'DBSTATS'. mc_add 'DBSTATTADA'.
  mc_add 'DBSTATTDB2'. mc_add 'DBSTATTDB4'. mc_add 'DBSTATTDB6'.
  mc_add 'DBSTATTINF'. mc_add 'DBSTATTMSS'. mc_add 'DBSTATTORA'.
  mc_add 'DBSYCHKDB2'. mc_add 'DBTAB_LONGNAMES'. mc_add 'DBTABLOG'.
  mc_add 'DBTABPRT'. mc_add 'DBTMPORA'. mc_add 'DBVSE'.
  mc_add 'DCDDLSSTATDET'. mc_add 'DCDDLSSTATH'. mc_add 'DCDDLSSTATSUM'.
  mc_add 'DCDEPTB4LOG'. mc_add 'DCGENTB4LOG'. mc_add 'DCIMPDD'.
  mc_add 'DD01L'. mc_add 'DD01T'. mc_add 'DD02ALL'. mc_add 'DD02B'.
  mc_add 'DD02BND'. mc_add 'DD02BNDT'. mc_add 'DD02BT'.
  mc_add 'DD02DB2'. mc_add 'DD02INF'. mc_add 'DD02L'. mc_add 'DD02MSS'.
  mc_add 'DD02T'. mc_add 'DD03L'. mc_add 'DD03ND'. mc_add 'DD03NT'.
  mc_add 'DD03T'. mc_add 'DD04L'. mc_add 'DD04T'. mc_add 'DD05B'.
  mc_add 'DD05F'. mc_add 'DD05S'. mc_add 'DD06L'. mc_add 'DD06T'.
  mc_add 'DD07L'. mc_add 'DD07T'. mc_add 'DD08B'. mc_add 'DD08BT'.
  mc_add 'DD08L'. mc_add 'DD08T'. mc_add 'DD09B'. mc_add 'DD09C'.
  mc_add 'DD09L'. mc_add 'DD10B'. mc_add 'DD10BT'. mc_add 'DD10L'.
  mc_add 'DD12B'. mc_add 'DD12BT'. mc_add 'DD12DB2'. mc_add 'DD12INF'.
  mc_add 'DD12L'. mc_add 'DD12MSS'. mc_add 'DD12T'. mc_add 'DD14S'.
  mc_add 'DD15L'. mc_add 'DD15T'. mc_add 'DD16S'. mc_add 'DD17B'.
  mc_add 'DD17S'. mc_add 'DD20L'. mc_add 'DD20T'. mc_add 'DD21S'.
  mc_add 'DD23L'. mc_add 'DD23T'. mc_add 'DD24S'. mc_add 'DD25L'.
  mc_add 'DD25T'. mc_add 'DD26S'. mc_add 'DD27S'. mc_add 'DD28S'.
  mc_add 'DD29L'. mc_add 'DD29T'. mc_add 'DD30L'. mc_add 'DD30T'.
  mc_add 'DD31S'. mc_add 'DD32S'. mc_add 'DD33S'. mc_add 'DD35L'.
  mc_add 'DD36S'. mc_add 'DD40L'. mc_add 'DD40T'. mc_add 'DD42S'.
  mc_add 'DD43L'. mc_add 'DD43T'. mc_add 'DD90L'. mc_add 'DD90T'.
  mc_add 'DD91S'. mc_add 'DD92S'. mc_add 'DD93S'. mc_add 'DD94S'.
  mc_add 'DD96S'. mc_add 'DD97S'. mc_add 'DDACL'. mc_add 'DDALIAS'.
  mc_add 'DDANNOENTITY'. mc_add 'DDANNOENTITY_C'. mc_add 'DDANNOLOAD'.
  mc_add 'DDART'. mc_add 'DDBUF'. mc_add 'DDCDIM'. mc_add 'DDCNVCOUNT'.
  mc_add 'DDCNVEXIT'. mc_add 'DDCNVSTAT'. mc_add 'DDCNVTABL'.
  mc_add 'DDCNVTIMES'. mc_add 'DDCNVUSR'. mc_add 'DDCPRO'.
  mc_add 'DDCPROT'. mc_add 'DDCPROTAB'. mc_add 'DDCQUEUE'.
  mc_add 'DDCSTA'. mc_add 'DDCSTAT'. mc_add 'DDDBFEATURES'.
  mc_add 'DDDBFEATUREST'. mc_add 'DDDBFUNC'. mc_add 'DDDBSTMTHD'.
  mc_add 'DDDBSTMTPAR'. mc_add 'DDDBSTMTS'. mc_add 'DDDDLCHARTYPES'.
  mc_add 'DDDDLCURRTYPES'. mc_add 'DDDDLDECTYPES'.
  mc_add 'DDDDLQUANTYPES'. mc_add 'DDDDLSRC'. mc_add 'DDDDLSRC02BNDT'.
  mc_add 'DDDDLSRC02BT'. mc_add 'DDDDLSRC03NT'. mc_add 'DDDDLSRC08BT'.
  mc_add 'DDDDLSRC09B'. mc_add 'DDDDLSRC09BV'. mc_add 'DDDDLSRC10BT'.
  mc_add 'DDDDLSRC12B'. mc_add 'DDDDLSRC12BT'. mc_add 'DDDDLSRC17B'.
  mc_add 'DDDDLSRCT'. mc_add 'DDF4GP'. mc_add 'DDF4PSINDX'.
  mc_add 'DDFIELDANNO'. mc_add 'DDFIELDANNOT'. mc_add 'DDHEADANNO'.
  mc_add 'DDHEADANNOT'. mc_add 'DDICNVADM'. mc_add 'DDICNVCTRL'.
  mc_add 'DDICNVDAYS'. mc_add 'DDICNVDIST'. mc_add 'DDICNVEXC'.
  mc_add 'DDICNVLST'. mc_add 'DDLBBUFTST'. mc_add 'DDLDEPENDENCY'.
  mc_add 'DDLOADD'. mc_add 'DDLOADH'. mc_add 'DDLOG'.
  mc_add 'DDLS_RIS_INDEX'. mc_add 'DDLX_RT_DATA'.
  mc_add 'DDLX_RT_DATA_T'. mc_add 'DDLX_RT_EXTENDS'.
  mc_add 'DDLX_RT_HEADER'. mc_add 'DDLX_RT_STATUS'. mc_add 'DDLXSRC'.
  mc_add 'DDLXSRC_SRC'. mc_add 'DDLXSRCT'. mc_add 'DDMIMETYPES'.
  mc_add 'DDMIMETYPES_T'. mc_add 'DDMTF'. mc_add 'DDMTF_ISU'.
  mc_add 'DDMTF_SAV'. mc_add 'DDMTT'. mc_add 'DDMTT_ISU'.
  mc_add 'DDMTT_SAV'. mc_add 'DDNTDONE'. mc_add 'DDNTF'.
  mc_add 'DDNTF_CONV_UC'. mc_add 'DDNTF_HIST'. mc_add 'DDNTLANG'.
  mc_add 'DDNTT'. mc_add 'DDNTT_CONV_UC'. mc_add 'DDNTT_HIST'.
  mc_add 'DDPAMSCTRL'. mc_add 'DDPARAMETERANNO'.
  mc_add 'DDPARAMETERANNOT'. mc_add 'DDPART'. mc_add 'DDPATH'.
  mc_add 'DDPRH'. mc_add 'DDPROF'. mc_add 'DDPRS'.
  mc_add 'DDREPLICATOR'. mc_add 'DDREPLICATORVI'.
  mc_add 'DDREPLICNVDATA'. mc_add 'DDREPLICREF'. mc_add 'DDREPLINDATA'.
  mc_add 'DDREPLINTABFD'. mc_add 'DDREPLINTABHD'. mc_add 'DDREPLITST1'.
  mc_add 'DDSCHEMAMAP'. mc_add 'DDSERVPERF'. mc_add 'DDSHENTITY'.
  mc_add 'DDSHLPVERS'. mc_add 'DDSPAR'. mc_add 'DDSPAR1'.
  mc_add 'DDSPAR2'. mc_add 'DDSQLSCCAT'. mc_add 'DDSQLSCDDIC'.
  mc_add 'DDSQLSCSRC'. mc_add 'DDSQLSCT'. mc_add 'DDSTATHIST'.
  mc_add 'DDSTORAGE'. mc_add 'DDSTORAGE1'. mc_add 'DDSTORAGE2'.
  mc_add 'DDSYN'. mc_add 'DDTBFUNCDEP'. mc_add 'DDTEST_DBTAB1'.
  mc_add 'DDTEST_DBTAB2'. mc_add 'DDTPOOLCNV'. mc_add 'DDTYPES'.
  mc_add 'DDTYPET'. mc_add 'DDXTF'. mc_add 'DDXTF_CONV_UC'.
  mc_add 'DDXTF_SAV'. mc_add 'DDXTF_SHD'. mc_add 'DDXTF_STD'.
  mc_add 'DDXTT'. mc_add 'DDXTT_CONV_UC'. mc_add 'DDXTT_MIG'.
  mc_add 'DDXTT_SAV'. mc_add 'DDXTT_SHD'. mc_add 'DDXTT_STD'.
  mc_add 'DELDYNPS'. mc_add 'DELM'. mc_add 'DELREPS'.
  mc_add 'DELWBCROSS'. mc_add 'DEMO_BIG_TABLE'.
  mc_add 'DEMO_BLOB_TABLE'. mc_add 'DEMO_CLOB_TABLE'. mc_add 'DEMO_GTT'.
  mc_add 'DEMO_JOIN1'. mc_add 'DEMO_JOIN2'. mc_add 'DEMO_JOIN3'.
  mc_add 'DEMO_JOIN4'. mc_add 'DEMO_LOB_TABLE'. mc_add 'DEMOTREE'.
  mc_add 'DEMOTREET'. mc_add 'DEVACCESS'. mc_add 'DEVC_APP'.
  mc_add 'DEVC_APX'. mc_add 'DEVL'. mc_add 'DEVLT'. mc_add 'DF03L'.
  mc_add 'DF03T'. mc_add 'DF04L'. mc_add 'DF04T'. mc_add 'DF06L'.
  mc_add 'DF06T'. mc_add 'DF07L'. mc_add 'DF07T'. mc_add 'DF08L'.
  mc_add 'DF08T'. mc_add 'DF10L'. mc_add 'DF10T'. mc_add 'DF11L'.
  mc_add 'DF11T'. mc_add 'DF12L'. mc_add 'DF12T'. mc_add 'DF13L'.
  mc_add 'DF13T'. mc_add 'DF14A'. mc_add 'DF14L'. mc_add 'DF14T'.
  mc_add 'DF15L'. mc_add 'DF15T'. mc_add 'DF16L'. mc_add 'DF16T'.
  mc_add 'DF17L'. mc_add 'DF17T'. mc_add 'DF18L'. mc_add 'DF18T'.
  mc_add 'DF30S'. mc_add 'DF31S'. mc_add 'DF32S'. mc_add 'DF33S'.
  mc_add 'DF34S'. mc_add 'DF35S'. mc_add 'DF36S'. mc_add 'DF40D'.
  mc_add 'DF41S'. mc_add 'DF42S'. mc_add 'DF43S'. mc_add 'DF50D'.
  mc_add 'DF50L'. mc_add 'DF50O'. mc_add 'DF50T'. mc_add 'DF52A'.
  mc_add 'DF52L'. mc_add 'DF52T'. mc_add 'DF53S'. mc_add 'DF54S'.
  mc_add 'DF55L'. mc_add 'DF55T'. mc_add 'DF62S'. mc_add 'DF63S'.
  mc_add 'DFBNT'. mc_add 'DFDOC'. mc_add 'DFPRX'. mc_add 'DFTNODE01'.
  mc_add 'DFTNODE01R'. mc_add 'DFTNODE01T'. mc_add 'DGKAT'.
  mc_add 'DIAPAR'. mc_add 'DIRTREE'. mc_add 'DITAT_D_CCDATA'.
  mc_add 'DITAT_D_CCNAME'. mc_add 'DITAT_D_COLLDEF'.
  mc_add 'DITAT_D_DELTA'. mc_add 'DITAT_D_FIPR'. mc_add 'DITAT_D_GRAPH'.
  mc_add 'DITAT_D_LOHD'. mc_add 'DITAT_D_POFLHD'.
  mc_add 'DITAT_D_POTCMNT'. mc_add 'DITAT_D_POVLHD'.
  mc_add 'DITAT_D_PRED'. mc_add 'DITAT_D_PROJ'.
  mc_add 'DITAT_D_PROJNAME'. mc_add 'DITAT_D_SDLXLIFF'.
  mc_add 'DITAT_D_SOHD'. mc_add 'DITAT_D_STATE'. mc_add 'DITAT_D_TBMOL'.
  mc_add 'DITAT_D_TBMTL'. mc_add 'DITAT_D_TMCNT'. mc_add 'DITAT_D_TMFD'.
  mc_add 'DITAT_D_TMFDR'. mc_add 'DITAT_D_TMHD'.
  mc_add 'DITAT_D_TMTRLTR'. mc_add 'DITAT_D_TPRJBM'.
  mc_add 'DITAT_D_TPRJDL'. mc_add 'DITAT_D_TPRJHD'.
  mc_add 'DITAT_D_TTX'. mc_add 'DITAT_D_TTX2'. mc_add 'DITAT_D_TXT'.
  mc_add 'DITAT_D_VERSION'. mc_add 'DLV_SYSTC'. mc_add 'DM02L'.
  mc_add 'DM02S'. mc_add 'DM02T'. mc_add 'DM03S'. mc_add 'DM25L'.
  mc_add 'DM26L'. mc_add 'DM40L'. mc_add 'DM40T'. mc_add 'DM41S'.
  mc_add 'DM42S'. mc_add 'DM42T'. mc_add 'DM43T'. mc_add 'DM45L'.
  mc_add 'DM45T'. mc_add 'DM46S'. mc_add 'DM48L'. mc_add 'DM48T'.
  mc_add 'DM50S'. mc_add 'DM50T'. mc_add 'DM99L'. mc_add 'DOBTC_JOB'.
  mc_add 'DOBTC_JOB_PROP'. mc_add 'DOBTC_SCHED'.
  mc_add 'DOBTC_SCHED_SRS'. mc_add 'DOBTC_TMPL'.
  mc_add 'DOBTC_TMPL_PROP'. mc_add 'DOBTC_TMPL_SRS'.
  mc_add 'DOCALTLANG'. mc_add 'DOCCLV'. mc_add 'DOCM_ART_PROC'.
  mc_add 'DOCM_BUILD_ORDER'. mc_add 'DOCM_BUILD_PROC'.
  mc_add 'DOCM_CONFIG'. mc_add 'DOCM_EXIM_BSP'.
  mc_add 'DOCM_EXIM_OBJECT'. mc_add 'DOCM_STG_ARCHIVE'.
  mc_add 'DOCM_STG_CONFIG'. mc_add 'DOCM_STG_DATA'.
  mc_add 'DOCM_STG_HEADER'. mc_add 'DOCM_STG_HISTORY'.
  mc_add 'DOCM_STG_PREREQ'. mc_add 'DOCM_STG_TR_CMD'.
  mc_add 'DOCM_TEST'. mc_add 'DOCR_CFG_LONGVAL'. mc_add 'DOCR_CLUSTER'.
  mc_add 'DOCR_CONFIG'. mc_add 'DOCR_ENV'. mc_add 'DOCR_EXTENSION'.
  mc_add 'DOCR_LOG_DATA'. mc_add 'DOCR_LOG_ENV'.
  mc_add 'DOCR_LOG_HEADER'. mc_add 'DOCR_LOG_RECORD'.
  mc_add 'DOCR_LONG_VALUE'. mc_add 'DOCR_NUM_RANGES'.
  mc_add 'DOCR_RELAY'. mc_add 'DOCR_REPO_CDATA'.
  mc_add 'DOCR_REPO_FILE'. mc_add 'DOCR_TRC_DATA'.
  mc_add 'DOCR_TRC_HEADER'. mc_add 'DOCR_TRC_RECORD'.
  mc_add 'DOCRP_FLDBMP'. mc_add 'DOCRP_SOURCE'. mc_add 'DOCRP_SRCBMP'.
  mc_add 'DOCT_GEN_OBJ'. mc_add 'DODBO_OBJ_HEADER'.
  mc_add 'DODBO_OBJ_PROP'. mc_add 'DODBO_SET_HEADER'.
  mc_add 'DODBO_SET_OBJECT'. mc_add 'DODC_DOCUMENT'.
  mc_add 'DODC_REVIEW'. mc_add 'DODP_ASSOC'. mc_add 'DODP_ASSOC_FIELD'.
  mc_add 'DODP_ASSOCN'. mc_add 'DODP_ASSOCN_CTXT'.
  mc_add 'DODP_ASSOCN_FLD'. mc_add 'DODP_ASSOCN_FLD1'.
  mc_add 'DODP_ASSOCR'. mc_add 'DODP_ASSOCR_FLD'.
  mc_add 'DODP_CONV_EXIT'. mc_add 'DODP_CONV_EXIT_V'.
  mc_add 'DODP_FIELD'. mc_add 'DODP_FIELD_GROUP'.
  mc_add 'DODP_FIELD_LABEL'. mc_add 'DODP_FIELD_VALUE'.
  mc_add 'DODP_FIELD1'. mc_add 'DODP_FUNCTION'. mc_add 'DODP_MAP_FIELD'.
  mc_add 'DODP_MAP_SOURCE'. mc_add 'DODP_PROCESSOR'.
  mc_add 'DODP_PROVIDER'. mc_add 'DODP_SERVICE'. mc_add 'DODP_VALIDITY'.
  mc_add 'DODR_DB_STMT'. mc_add 'DOFNDSOUNDEX'. mc_add 'DOFNDTARGET'.
  mc_add 'DOFNDTARGET2'. mc_add 'DOFNDTARGET3'. mc_add 'DOFNDTARGET4'.
  mc_add 'DOFNDTARGET5'. mc_add 'DOFNDWDTRGTASS'.
  mc_add 'DOFNDWDTRGTASS2'. mc_add 'DOFNDWDTRGTASS3'.
  mc_add 'DOFNDWORD'. mc_add 'DOFNDWORD2'. mc_add 'DOKALTLANG'.
  mc_add 'DOKCL'. mc_add 'DOKCLU'. mc_add 'DOKCR'. mc_add 'DOKENTRY'.
  mc_add 'DOKHL'. mc_add 'DOKID'. mc_add 'DOKIL'. mc_add 'DOKSH'.
  mc_add 'DOKSI'. mc_add 'DOKTL'. mc_add 'DOOD_ENTITY_SET'.
  mc_add 'DOOD_ENTITY_TYPE'. mc_add 'DOOD_PROPERTY'.
  mc_add 'DOOD_SERVICE'. mc_add 'DORAN_FIELD'. mc_add 'DORAN_SOURCE'.
  mc_add 'DORDS_ACCESS'. mc_add 'DORDS_ANSIJOIN'. mc_add 'DORDS_FIELD'.
  mc_add 'DORDS_FIELD_OPT'. mc_add 'DORDS_FUNCTION'.
  mc_add 'DORDS_INVIEW'. mc_add 'DORDS_JOIN_COND'.
  mc_add 'DORDS_JOIN_EQ'. mc_add 'DORDS_JOIN_PART'.
  mc_add 'DORDS_OPTION'. mc_add 'DORDS_PARAMETER'.
  mc_add 'DORDS_PROCEDURE'. mc_add 'DORDS_SET'. mc_add 'DORDS_SET_EQ'.
  mc_add 'DORDS_SET_PART'. mc_add 'DORDS_SET_UNITE'.
  mc_add 'DORDS_SOURCE'. mc_add 'DORDS_TABLE'. mc_add 'DORDS_VIRTUAL'.
  mc_add 'DORMO_DOMAIN'. mc_add 'DORMO_FIELD'. mc_add 'DORMO_OBJECT'.
  mc_add 'DORMO_OBJECT_PRP'. mc_add 'DORMO_OBJECT_SRC'.
  mc_add 'DORMO_SET'. mc_add 'DORQ_FIELD'. mc_add 'DORQ_OPTION'.
  mc_add 'DORQ_PROP'. mc_add 'DORQ_REQUEST'. mc_add 'DOSSH'.
  mc_add 'DOSSI'. mc_add 'DOSSO'. mc_add 'DOST_CASE_HEADER'.
  mc_add 'DOST_CASE_WORD'. mc_add 'DOST_JS_RESULTS'.
  mc_add 'DOST_TEST_PROP'. mc_add 'DOT_OS_WSC_LP_L'.
  mc_add 'DOT_OS_WSC_LP_P'. mc_add 'DOTM_CON'. mc_add 'DOTM_CON_A_PROP'.
  mc_add 'DOTM_CON_ASSOC'. mc_add 'DOTM_CON_PROP'. mc_add 'DOTM_SYS'.
  mc_add 'DOTM_SYS_A_PROP'. mc_add 'DOTM_SYS_ASSOC'.
  mc_add 'DOTM_SYS_PROP'. mc_add 'DOTM_VAL_SUBTYPE'.
  mc_add 'DOTM_VAL_SYSTYPE'. mc_add 'DOUI_DOMAIN'.
  mc_add 'DOUI_DYNPROFIELD'. mc_add 'DOUI_ELEM'. mc_add 'DOUI_ELEM_ALV'.
  mc_add 'DOUI_ELEM_CONV'. mc_add 'DOUI_ELEM_PROP'.
  mc_add 'DOUI_ELEM_SORT'. mc_add 'DOUI_ELEM_VALUE'.
  mc_add 'DOUI_FLOORPLAN'. mc_add 'DOUI_FP_SLOT'. mc_add 'DOUI_ICON'.
  mc_add 'DOUI_MAP_EVENT'. mc_add 'DOUI_MAP_FIELD'.
  mc_add 'DOUI_MAP_FIELD1'. mc_add 'DOUI_MAP_KEYVAL'.
  mc_add 'DOUI_MAP_RQST'. mc_add 'DOUI_MAP_UI_ELEM'.
  mc_add 'DOUI_NAV_CLUSTER'. mc_add 'DOUI_NAV_CONTEXT'.
  mc_add 'DOUI_NAV_CONTXT1'. mc_add 'DOUI_NAV_LINK'.
  mc_add 'DOUI_NAV_REQUEST'. mc_add 'DOUI_RT_SCREEN'.
  mc_add 'DOUI_SAPUI5_RSRC'. mc_add 'DOUI_SCREEN'.
  mc_add 'DOUI_SCREEN_GRP'. mc_add 'DOUI_USR_SESSION'.
  mc_add 'DOUI_VALIDITY'. mc_add 'DOUI5_RESOURCE'. mc_add 'DRATEXT'.
  mc_add 'DSH_GEOCITY'. mc_add 'DSHCONFIG'. mc_add 'DSTCL'.
  mc_add 'DSVAS_STAB'. mc_add 'DSYAA'. mc_add 'DSYAD'. mc_add 'DSYADEF'.
  mc_add 'DSYAH'. mc_add 'DSYAI'. mc_add 'DSYAM'. mc_add 'DSYAS'.
  mc_add 'DSYAT'. mc_add 'DSYAV'. mc_add 'DSYAW'. mc_add 'DSYAX'.
  mc_add 'DSYBA'. mc_add 'DSYBB'. mc_add 'DSYCLV'. mc_add 'DSYCR'.
  mc_add 'DSYDL'. mc_add 'DSYDS'. mc_add 'DSYGH'. mc_add 'DSYGI'.
  mc_add 'DSYGL'. mc_add 'DSYLI'. mc_add 'DSYLT'. mc_add 'DSYO1'.
  mc_add 'DSYO2'. mc_add 'DSYO3'. mc_add 'DSYOH'. mc_add 'DSYOI'.
  mc_add 'DSYOL'. mc_add 'DSYOT'. mc_add 'DSYSH'. mc_add 'DSYSI'.
  mc_add 'DSYSL'. mc_add 'DSYST'. mc_add 'DSYSX'. mc_add 'DTELDOKI'.
  mc_add 'DTELDOKTL'. mc_add 'DTELDOKTL1'. mc_add 'DTELLINKS'.
  mc_add 'DTELPROT'. mc_add 'DTELRENAME'. mc_add 'DTLIDOKCR'.
  mc_add 'DTLIDOKI'. mc_add 'DTRULES'. mc_add 'DVS1CHKF'.
  mc_add 'DVS1CHKO'. mc_add 'DVS1IDXSTA'. mc_add 'DVS1LOIO'.
  mc_add 'DVS1LOIOT'. mc_add 'DVS1LOPR'. mc_add 'DVS1LORE'.
  mc_add 'DVS1LORI'. mc_add 'DVS1NODE'. mc_add 'DVS1NODER'.
  mc_add 'DVS1NODET'. mc_add 'DVS1PHF'. mc_add 'DVS1PHHR'.
  mc_add 'DVS1PHIO'. mc_add 'DVS1PHNM'. mc_add 'DVS1PHPR'.
  mc_add 'DVS1PHRE'. mc_add 'DVS1PHRI'. mc_add 'DVSEQDIR'.
  mc_add 'DWINACTIV'. mc_add 'DWTREE'. mc_add 'DWWASYNC'.
  mc_add 'DXLFT_COMMENT'. mc_add 'DXLFT_DELTA_REG'.
  mc_add 'DXLFT_DEV_CMNT'. mc_add 'DXLFT_FIPR'. mc_add 'DXLFT_LTEXT'.
  mc_add 'DXLFT_ORIG'. mc_add 'DXLFT_ORIG_PSEUD'.
  mc_add 'DXLFT_ORIG_TECH'. mc_add 'DXLFT_SDLXLIFF'.
  mc_add 'DXLFT_STEXT'. mc_add 'DXLFT_STTHRSHLD'.
  mc_add 'DXLFT_TTX_SNIP'. mc_add 'DXMAP'. mc_add 'DXMAP_T'.
  mc_add 'DXOBJTCODE'. mc_add 'DYNABAPHDR'. mc_add 'DYNABAPSRC'.
  mc_add 'DYNP_MSK'. mc_add 'DYNPFIELDS'. mc_add 'DYNPS'.
  mc_add 'DYNPSOURCE'. mc_add 'E070'. mc_add 'E070___PREVIEW'.
  mc_add 'E070_BC'. mc_add 'E070A'. mc_add 'E070A__PREVIEW'.
  mc_add 'E070A_BC'. mc_add 'E070AS'. mc_add 'E070AS_PREVIEW'.
  mc_add 'E070C'. mc_add 'E070C__PREVIEW'. mc_add 'E070C_BC'.
  mc_add 'E070CREATE'. mc_add 'E070DEP'. mc_add 'E070M'.
  mc_add 'E070M__PREVIEW'. mc_add 'E070TAG'. mc_add 'E070TC'.
  mc_add 'E070TC_BC'. mc_add 'E070TS'. mc_add 'E070TS_PREVIEW'.
  mc_add 'E070USE'. mc_add 'E071'. mc_add 'E071___PREVIEW'.
  mc_add 'E071_BC'. mc_add 'E071C'. mc_add 'E071E'. mc_add 'E071K'.
  mc_add 'E071K__PREVIEW'. mc_add 'E071K_BC'. mc_add 'E071K_STR'.
  mc_add 'E071K_STR_BC'. mc_add 'E071K_STR_SHADOW'. mc_add 'E071KF'.
  mc_add 'E071KF_BC'. mc_add 'E071KF_PREVIEW'. mc_add 'E071KFINI'.
  mc_add 'E071KS_PREVIEW'. mc_add 'E071S'. mc_add 'E071S__PREVIEW'.
  mc_add 'E07T'. mc_add 'E07T___PREVIEW'. mc_add 'E07T_BC'.
  mc_add 'E07X_BC_HEADER'. mc_add 'E07X_BC_RESTORE'. mc_add 'EARLY'.
  mc_add 'ECAT_CVERS'. mc_add 'ECATT_DEF'. mc_add 'ECATT_EXTVAR'.
  mc_add 'ECATT_MCV_HEAD'. mc_add 'ECATT_MCV_OP'. mc_add 'ECATT_TEXT'.
  mc_add 'ECATT_TEXT_N'. mc_add 'ECATT_VER'. mc_add 'ECBR_NODE'.
  mc_add 'ECBR_NODER'. mc_add 'ECBR_NODET'. mc_add 'ECCOMP'.
  mc_add 'ECCOMP_TXT'. mc_add 'ECCUST'. mc_add 'ECCUST_ET'.
  mc_add 'ECCUST_TIPS'. mc_add 'ECCUSTOM'. mc_add 'ECCVERS'.
  mc_add 'ECDBG_USER'. mc_add 'ECET_ARGS'. mc_add 'ECET_ARTMP'.
  mc_add 'ECET_BLOBS'. mc_add 'ECET_BLOBS_EN'. mc_add 'ECET_LOG'.
  mc_add 'ECET_SDC_BUFFER'. mc_add 'ECET_SDC_TSAT_BF'.
  mc_add 'ECET_SECURE'. mc_add 'ECEVT_DAT'. mc_add 'ECGUI_STA'.
  mc_add 'ECLOG_AIDX'. mc_add 'ECLOG_AIDX_SPLIT'. mc_add 'ECLOG_CALL'.
  mc_add 'ECLOG_CONF'. mc_add 'ECLOG_DATA'. mc_add 'ECLOG_EXEC'.
  mc_add 'ECLOG_EXT'. mc_add 'ECLOG_FAIL_CMD'. mc_add 'ECLOG_FAIL_ESF'.
  mc_add 'ECLOG_FAIL_EXC'. mc_add 'ECLOG_FAIL_MSG'.
  mc_add 'ECLOG_FAIL_VAR'. mc_add 'ECLOG_HEAD'. mc_add 'ECLOG_HYDRA'.
  mc_add 'ECLOG_MEMORY_TRC'. mc_add 'ECLOG_MSG'. mc_add 'ECLOG_RESTAB'.
  mc_add 'ECLOG_SCNT'. mc_add 'ECLOG_SCR'. mc_add 'ECLOG_SEL'.
  mc_add 'ECLOG_SET'. mc_add 'ECLOG_SPOC'. mc_add 'ECLOG_XDAT'.
  mc_add 'ECMSG_COLL'. mc_add 'ECOBJUSE'. mc_add 'ECOD_LOG_CLASS'.
  mc_add 'ECOD_LOG_HEAD'. mc_add 'ECOD_LOG_METHOD'.
  mc_add 'ECOD_LOG_OP_CALL'. mc_add 'ECOD_LOG_OP_HTTP'.
  mc_add 'ECOD_LOG_OP_MSG'. mc_add 'ECOQ_LOG'. mc_add 'ECRP_DEF'.
  mc_add 'ECRP_TOLER'. mc_add 'ECRP_TRACE'. mc_add 'ECSCR_ARG'.
  mc_add 'ECSCR_BF'. mc_add 'ECSCR_BF_VER'. mc_add 'ECSCR_CMD'.
  mc_add 'ECSCR_DATA'. mc_add 'ECSCR_LINE'. mc_add 'ECSCR_PAR'.
  mc_add 'ECSCR_PEXT'. mc_add 'ECSCR_SUT_MD'. mc_add 'ECSCR_TFLD'.
  mc_add 'ECSCR_TFOR'. mc_add 'ECSCR_TPAR'. mc_add 'ECSCR_TPRO'.
  mc_add 'ECSCR_TSVAR'. mc_add 'ECSCR_TTAB'. mc_add 'ECSCR_TVAR'.
  mc_add 'ECSCR_TVER'. mc_add 'ECSCR_XML'. mc_add 'ECSCR_XML_STR'.
  mc_add 'ECSCR_XML_STR_WD'. mc_add 'ECSCR_XSD'. mc_add 'ECSD_DEF'.
  mc_add 'ECSD_SYS'. mc_add 'ECSP_DEF'. mc_add 'ECSP_PAR'.
  mc_add 'ECSP_PAR_DATA'. mc_add 'ECSP_PAR_VAR'. mc_add 'ECSP_PAR_XML'.
  mc_add 'ECSP_PAR_XML_STR'. mc_add 'ECSP_USR'. mc_add 'ECSTAT_FGROUP'.
  mc_add 'ECSTAT_FGROUPT'. mc_add 'ECSTAT_FUNC'. mc_add 'ECSTAT_LOC'.
  mc_add 'ECSTAT_MAIN'. mc_add 'ECSTAT_OBJECT'. mc_add 'ECSTAT_SC_EXEC'.
  mc_add 'ECSTAT_TC_EXEC'. mc_add 'ECSTAT_TESTDRV'. mc_add 'ECSYS_COMP'.
  mc_add 'ECSYS_REL'. mc_add 'ECTC_ATTACH'. mc_add 'ECTC_DATA'.
  mc_add 'ECTC_DEF'. mc_add 'ECTC_TD'. mc_add 'ECTC_TDW_ASS_PAR'.
  mc_add 'ECTC_TDW_ASS_VAR'. mc_add 'ECTC_TDW_SEL_VAR'.
  mc_add 'ECTC_TDW_TDC_FAV'. mc_add 'ECTC_TDW_TDC_SEL'.
  mc_add 'ECTC_TDW_VAR_TXT'. mc_add 'ECTC_VAR'. mc_add 'ECTC_VER'.
  mc_add 'ECTC_XML'. mc_add 'ECTC_XML_STR'. mc_add 'ECTD_BF'.
  mc_add 'ECTD_BF_VER'. mc_add 'ECTD_DATA'. mc_add 'ECTD_DATAX'.
  mc_add 'ECTD_DEF'. mc_add 'ECTD_PAR'. mc_add 'ECTD_SYNCX'.
  mc_add 'ECTD_SYS_COMP'. mc_add 'ECTD_SYS_REL'. mc_add 'ECTD_TEXT'.
  mc_add 'ECTD_TEXT_N'. mc_add 'ECTD_VAR'. mc_add 'ECTD_VER'.
  mc_add 'ECTD_XML'. mc_add 'ECTD_XML_STR'. mc_add 'ECTOLERANZ'.
  mc_add 'ECTP_JOB_DATA'. mc_add 'ECTP_JOB_LIST'. mc_add 'ECTP_TP_LIST'.
  mc_add 'ECTR_ACTIVATE'. mc_add 'ECTR_ACTIVATE_CS'.
  mc_add 'ECTR_ACTIVATE_ES'. mc_add 'ECTRACE'. mc_add 'ECTRACE_CONTENT'.
  mc_add 'ECTRACETYP'. mc_add 'ECTWB_DEST'. mc_add 'ECVO_BUS_MSG'.
  mc_add 'ECVO_DEF'. mc_add 'ECVO_VER'. mc_add 'ECWD_APP_TERM'.
  mc_add 'ECWD_APP_TERM2'. mc_add 'ECWD_RESOURCES'. mc_add 'ECWD_TRACE'.
  mc_add 'ECWS_SECURE'. mc_add 'EDBAS'. mc_add 'EDBAST'. mc_add 'EDCIM'.
  mc_add 'EDCIMT'. mc_add 'EDCNVAPLRL'. mc_add 'EDCRYPTDISPLAY'.
  mc_add 'EDFI2'. mc_add 'EDFIL'. mc_add 'EDI_CNVBAS'.
  mc_add 'EDI_CNVDOC'. mc_add 'EDI_CNVEXT'. mc_add 'EDI_CNVMES'.
  mc_add 'EDI_MSG_MDMP'. mc_add 'EDI_SPRAS_MDMP'. mc_add 'EDI30C'.
  mc_add 'EDIDO'. mc_add 'EDIDOC'. mc_add 'EDIDOCMAP'. mc_add 'EDIDOT'.
  mc_add 'EDIE5'. mc_add 'EDIFCT'. mc_add 'EDIFM'. mc_add 'EDIFMT'.
  mc_add 'EDIMAP'. mc_add 'EDIMSG'. mc_add 'EDIMSGT'. mc_add 'EDIPO'.
  mc_add 'EDISDEF'. mc_add 'EDISDEFOPTIONS'. mc_add 'EDISEG'.
  mc_add 'EDISEGMAP'. mc_add 'EDISEGMENT'. mc_add 'EDISEGT'.
  mc_add 'EDISYN'. mc_add 'EDMA'. mc_add 'EDMAT'. mc_add 'EDMSG'.
  mc_add 'EDPO_QUEUE'. mc_add 'EDPO_QUEUE_EOIO'.
  mc_add 'EDPO_QUEUE_INB'. mc_add 'EDS_VERI_TEST'. mc_add 'EDSAPPL'.
  mc_add 'EDSEA'. mc_add 'EDSEG'. mc_add 'EDSVRS'. mc_add 'EDSYN'.
  mc_add 'EDVIEW'. mc_add 'EDVIEWT'. mc_add 'ELEMDIR'. mc_add 'EMCHECK'.
  mc_add 'ENH_DY_FCODES'. mc_add 'ENH_DY_GROUP'. mc_add 'ENH_DY_SUBS'.
  mc_add 'ENH4DELTAB'. mc_add 'ENHA_TMDIR'. mc_add 'ENHCOMPCHILDCOMP'.
  mc_add 'ENHCOMPCHILDENH'. mc_add 'ENHCOMPHEADER'.
  mc_add 'ENHCONTRACTCONT'. mc_add 'ENHCONTRACTTOOL'.
  mc_add 'ENHCONTRACTTYP'. mc_add 'ENHCROSS'. mc_add 'ENHCROSSINXSPOT'.
  mc_add 'ENHDEPENDENT'. mc_add 'ENHHEADER'. mc_add 'ENHHOOKMIGRTEMP'.
  mc_add 'ENHINCINX'. mc_add 'ENHKNOWNENHS'. mc_add 'ENHLOADINVAL'.
  mc_add 'ENHLOG'. mc_add 'ENHNAME_TYPE'. mc_add 'ENHOBJ'.
  mc_add 'ENHOBJCONTRACT'. mc_add 'ENHSORT'. mc_add 'ENHSPOTCOMPCOMP'.
  mc_add 'ENHSPOTCOMPHEAD'. mc_add 'ENHSPOTCOMPSPOT'.
  mc_add 'ENHSPOTCONTRACT'. mc_add 'ENHSPOTHEADER'.
  mc_add 'ENHSPOTNAME_TYPE'. mc_add 'ENHSPOTOBJ'. mc_add 'ENHSPOTTOOLS'.
  mc_add 'ENHSPOTTOOLST'. mc_add 'ENHTAB'. mc_add 'ENHTEXT'.
  mc_add 'ENHTOOLS'. mc_add 'ENHTOOLS_UVL'. mc_add 'ENHTOOLST'.
  mc_add 'ENLFDIR'. mc_add 'EPSSCHRFRM'. mc_add 'EPSSU1'.
  mc_add 'ERCLASS_NONSAP'. mc_add 'ERCLASS_SAP'. mc_add 'ERG001'.
  mc_add 'ESD_ADD_DATA_REG'. mc_add 'ESH_ADM_MDM_CONN'.
  mc_add 'ESH_ADM_RFCDEST'. mc_add 'ESH_ADM_SC'. mc_add 'ESH_CU_IE_FWK'.
  mc_add 'ESH_CU_OM_TO_SC'. mc_add 'ESH_EX_EXTP'. mc_add 'ESH_EX_EXTPT'.
  mc_add 'ESH_OM_DDL'. mc_add 'ESH_OM_ODPID'. mc_add 'ESH_OM_PC_EXEMPT'.
  mc_add 'ESI_MEAL'. mc_add 'ESOA_FND_PACKAGE'.
  mc_add 'ETALLPAIRS_DATA'. mc_add 'ETALLPAIRS_HEAD'.
  mc_add 'ETALLPAIRS_USAGE'. mc_add 'ETOD_TEST_GENOBJ'.
  mc_add 'ETWB_SETTINGS'. mc_add 'EUDB'. mc_add 'EUDIAL'.
  mc_add 'EUF4VALUES'. mc_add 'EUFUNC'. mc_add 'EUGUITEXT'.
  mc_add 'EUIMPORT'. mc_add 'EUINFO'. mc_add 'EUINFOLI'. mc_add 'EUOBJ'.
  mc_add 'EUOBJEDIT'. mc_add 'EUOBJT'. mc_add 'EUP_BSP_CFG_PROP'.
  mc_add 'EUP_BSP_IV_CTX'. mc_add 'EUP_BSP_OV_CTX'.
  mc_add 'EUP_BSP_RSL_STS'. mc_add 'EWAGG'. mc_add 'EWCIAPCLCU'.
  mc_add 'EWCIAPINCU'. mc_add 'EWCIAPSECU'. mc_add 'EWEXC'.
  mc_add 'EWJOBSCHED'. mc_add 'EWOSS'. mc_add 'EWREL'. mc_add 'EWTABB'.
  mc_add 'EWTXT'. mc_add 'EXCEPT1'. mc_add 'EXCEPT1T'. mc_add 'EXCEPT2'.
  mc_add 'EXCEPT2T'. mc_add 'EXCOMP'. mc_add 'EXP_TRAN'.
  mc_add 'EXP_USASCI'. mc_add 'EXP_VERS'. mc_add 'EXTMAP'.
  mc_add 'F4EXTACC'. mc_add 'F4TMP'. mc_add 'FAVSELS'.
  mc_add 'FDC_SEGMENT'. mc_add 'FDC_UPG_METADATA'.
  mc_add 'FDT_ACTN_0000S'. mc_add 'FDT_ACTN_0000T'.
  mc_add 'FDT_ACTN_1000S'. mc_add 'FDT_ACTN_1000T'.
  mc_add 'FDT_ACTN_1001S'. mc_add 'FDT_ACTN_1001T'.
  mc_add 'FDT_ACTN_1010S'. mc_add 'FDT_ACTN_1010T'.
  mc_add 'FDT_ACTN_1100S'. mc_add 'FDT_ACTN_1100T'.
  mc_add 'FDT_ACTN_1106S'. mc_add 'FDT_ACTN_1106T'.
  mc_add 'FDT_ACTN_1107S'. mc_add 'FDT_ACTN_1107T'.
  mc_add 'FDT_ACTN_1110S'. mc_add 'FDT_ACTN_1110T'.
  mc_add 'FDT_ACTN_1200S'. mc_add 'FDT_ACTN_1200T'.
  mc_add 'FDT_ACTN_1210S'. mc_add 'FDT_ACTN_1210T'.
  mc_add 'FDT_ACTN_1220S'. mc_add 'FDT_ACTN_1220T'.
  mc_add 'FDT_ACTN_1300S'. mc_add 'FDT_ACTN_1300T'.
  mc_add 'FDT_ACTN_1310S'. mc_add 'FDT_ACTN_1310T'.
  mc_add 'FDT_ACTN_1400S'. mc_add 'FDT_ACTN_1400T'.
  mc_add 'FDT_ACTN_1410S'. mc_add 'FDT_ACTN_1410T'.
  mc_add 'FDT_ACTN_5000S'. mc_add 'FDT_ACTN_5000T'.
  mc_add 'FDT_ACTN_6000S'. mc_add 'FDT_ACTN_6000T'.
  mc_add 'FDT_ACTN_6100S'. mc_add 'FDT_ACTN_6100T'.
  mc_add 'FDT_ACTN_6110S'. mc_add 'FDT_ACTN_6110T'.
  mc_add 'FDT_ACTN_6120S'. mc_add 'FDT_ACTN_6120T'.
  mc_add 'FDT_ACTN_6200S'. mc_add 'FDT_ACTN_6200T'.
  mc_add 'FDT_ACTN_6210S'. mc_add 'FDT_ACTN_6210T'.
  mc_add 'FDT_ADMN_0000S'. mc_add 'FDT_ADMN_0000T'.
  mc_add 'FDT_ADMN_0001S'. mc_add 'FDT_ADMN_0001T'.
  mc_add 'FDT_ADMN_0010S'. mc_add 'FDT_ADMN_0010T'.
  mc_add 'FDT_ADMN_0020S'. mc_add 'FDT_ADMN_0020T'.
  mc_add 'FDT_ADMN_0030S'. mc_add 'FDT_ADMN_0030T'.
  mc_add 'FDT_ADMN_0031S'. mc_add 'FDT_ADMN_0031T'.
  mc_add 'FDT_ADMN_0070S'. mc_add 'FDT_ADMN_0070T'.
  mc_add 'FDT_ADMN_0100S'. mc_add 'FDT_ADMN_0100T'.
  mc_add 'FDT_ADMN_0101S'. mc_add 'FDT_ADMN_0101T'.
  mc_add 'FDT_ADMN_0102S'. mc_add 'FDT_ADMN_0102T'.
  mc_add 'FDT_ADMN_0103S'. mc_add 'FDT_ADMN_0103T'.
  mc_add 'FDT_ADMN_0104S'. mc_add 'FDT_ADMN_0104T'.
  mc_add 'FDT_ADMN_0109S'. mc_add 'FDT_ADMN_0109T'.
  mc_add 'FDT_ADMN_0111S'. mc_add 'FDT_ADMN_0111T'.
  mc_add 'FDT_ADMN_0112S'. mc_add 'FDT_ADMN_0112T'.
  mc_add 'FDT_ADMN_0113S'. mc_add 'FDT_ADMN_0113T'.
  mc_add 'FDT_ADMN_0114S'. mc_add 'FDT_ADMN_0114T'.
  mc_add 'FDT_ADMN_0115S'. mc_add 'FDT_ADMN_0115T'.
  mc_add 'FDT_ADMN_0116S'. mc_add 'FDT_ADMN_0116T'.
  mc_add 'FDT_ADMN_0117S'. mc_add 'FDT_ADMN_0117T'.
  mc_add 'FDT_ADMN_0119S'. mc_add 'FDT_ADMN_0119T'.
  mc_add 'FDT_ADMN_0730S'. mc_add 'FDT_ADMN_0730T'.
  mc_add 'FDT_ADMN_0901S'. mc_add 'FDT_ADMN_0901T'.
  mc_add 'FDT_ADMN_E0031S'. mc_add 'FDT_ADMN_E0031T'.
  mc_add 'FDT_ADMN_E0070S'. mc_add 'FDT_ADMN_E0070T'.
  mc_add 'FDT_ADMN_L0070S'. mc_add 'FDT_ADMN_L0070T'.
  mc_add 'FDT_ADMN_L0730S'. mc_add 'FDT_ADMN_L0730T'.
  mc_add 'FDT_APPL_0000S'. mc_add 'FDT_APPL_0000T'.
  mc_add 'FDT_APPL_TADIR'. mc_add 'FDT_BCF_CLIENT'.
  mc_add 'FDT_BRMS_CONN_1'. mc_add 'FDT_BRMS_CONN_2'.
  mc_add 'FDT_BRS_CC_0010S'. mc_add 'FDT_CC_0000S'.
  mc_add 'FDT_CC_DELE'. mc_add 'FDT_CC_H_0000'. mc_add 'FDT_CI_SETTING'.
  mc_add 'FDT_CI_TEXT_SET'. mc_add 'FDT_CTLG_0000S'.
  mc_add 'FDT_CTLG_0000T'. mc_add 'FDT_CTLG_0100S'.
  mc_add 'FDT_CTLG_0100T'. mc_add 'FDT_CTLG_0200S'.
  mc_add 'FDT_CTLG_0200T'. mc_add 'FDT_CTLG_0300S'.
  mc_add 'FDT_CTLG_0300T'. mc_add 'FDT_CTLG_0400S'.
  mc_add 'FDT_CTLG_0400T'. mc_add 'FDT_CTLG_0500S'.
  mc_add 'FDT_CTLG_0500T'. mc_add 'FDT_CTLG_0600S'.
  mc_add 'FDT_CTLG_0600T'. mc_add 'FDT_CTLG_E0000S'.
  mc_add 'FDT_CTLG_E0000T'. mc_add 'FDT_CTLG_E0200S'.
  mc_add 'FDT_CTLG_E0200T'. mc_add 'FDT_CTLG_E0300S'.
  mc_add 'FDT_CTLG_E0300T'. mc_add 'FDT_CTLG_E0500S'.
  mc_add 'FDT_CTLG_E0500T'. mc_add 'FDT_CTLG_E0600S'.
  mc_add 'FDT_CTLG_E0600T'. mc_add 'FDT_DBRU_0000S'.
  mc_add 'FDT_DBRU_0000T'. mc_add 'FDT_DBRU_0010S'.
  mc_add 'FDT_DBRU_0010T'. mc_add 'FDT_DBVO_0010S'.
  mc_add 'FDT_DBVO_0010T'. mc_add 'FDT_DBVO_0020S'.
  mc_add 'FDT_DBVO_0020T'. mc_add 'FDT_DBVO_0030S'.
  mc_add 'FDT_DBVO_0030T'. mc_add 'FDT_DBVO_0040S'.
  mc_add 'FDT_DBVO_0040T'. mc_add 'FDT_DDBV_0000S'.
  mc_add 'FDT_DDBV_0000T'. mc_add 'FDT_DDBV_0010S'.
  mc_add 'FDT_DDBV_0010T'. mc_add 'FDT_DDBV_0012S'.
  mc_add 'FDT_DDBV_0012T'. mc_add 'FDT_DDBV_0020S'.
  mc_add 'FDT_DDBV_0020T'. mc_add 'FDT_DEMO_0001'.
  mc_add 'FDT_DEPL_0000'. mc_add 'FDT_DOBJ_0000S'.
  mc_add 'FDT_DOBJ_0000T'. mc_add 'FDT_DOBJ_0001S'.
  mc_add 'FDT_DOBJ_0001T'. mc_add 'FDT_DOBJ_0010S'.
  mc_add 'FDT_DOBJ_0010T'. mc_add 'FDT_DOBJ_0100S'.
  mc_add 'FDT_DOBJ_0100T'. mc_add 'FDT_DOBJ_0110S'.
  mc_add 'FDT_DOBJ_0110T'. mc_add 'FDT_DOBJ_0200S'.
  mc_add 'FDT_DOBJ_0200T'. mc_add 'FDT_DOBJ_0300S'.
  mc_add 'FDT_DOBJ_0300T'. mc_add 'FDT_DOBJ_0400S'.
  mc_add 'FDT_DOBJ_0400T'. mc_add 'FDT_E071'. mc_add 'FDT_E071K'.
  mc_add 'FDT_EMEX_0000S'. mc_add 'FDT_EMEX_0000T'.
  mc_add 'FDT_EMEX_0001'. mc_add 'FDT_ENHCAT_TEXT'.
  mc_add 'FDT_ENQUEUE_ID'. mc_add 'FDT_EXPR_0000S'.
  mc_add 'FDT_EXPR_0000T'. mc_add 'FDT_EXPR_0001S'.
  mc_add 'FDT_EXPR_0001T'. mc_add 'FDT_EXPR_0010S'.
  mc_add 'FDT_EXPR_0010T'. mc_add 'FDT_EXPR_0020S'.
  mc_add 'FDT_EXPR_0020T'. mc_add 'FDT_EXPR_0200S'.
  mc_add 'FDT_EXPR_0200T'. mc_add 'FDT_EXPR_0210S'.
  mc_add 'FDT_EXPR_0210T'. mc_add 'FDT_EXPR_0212S'.
  mc_add 'FDT_EXPR_0212T'. mc_add 'FDT_EXPR_0220S'.
  mc_add 'FDT_EXPR_0220T'. mc_add 'FDT_EXPR_0230S'.
  mc_add 'FDT_EXPR_0230T'. mc_add 'FDT_EXPR_0231S'.
  mc_add 'FDT_EXPR_0231T'. mc_add 'FDT_EXPR_0240S'.
  mc_add 'FDT_EXPR_0240T'. mc_add 'FDT_EXPR_0241S'.
  mc_add 'FDT_EXPR_0241T'. mc_add 'FDT_EXPR_0242S'.
  mc_add 'FDT_EXPR_0242T'. mc_add 'FDT_EXPR_0244S'.
  mc_add 'FDT_EXPR_0244T'. mc_add 'FDT_EXPR_0247S'.
  mc_add 'FDT_EXPR_0247T'. mc_add 'FDT_EXPR_0248S'.
  mc_add 'FDT_EXPR_0248T'. mc_add 'FDT_EXPR_0301S'.
  mc_add 'FDT_EXPR_0301T'. mc_add 'FDT_EXPR_1000S'.
  mc_add 'FDT_EXPR_1000T'. mc_add 'FDT_EXPR_1100S'.
  mc_add 'FDT_EXPR_1100T'. mc_add 'FDT_EXPR_1101S'.
  mc_add 'FDT_EXPR_1101T'. mc_add 'FDT_EXPR_1104S'.
  mc_add 'FDT_EXPR_1104T'. mc_add 'FDT_EXPR_1200S'.
  mc_add 'FDT_EXPR_1200T'. mc_add 'FDT_EXPR_1201S'.
  mc_add 'FDT_EXPR_1201T'. mc_add 'FDT_EXPR_1202S'.
  mc_add 'FDT_EXPR_1202T'. mc_add 'FDT_EXPR_1300S'.
  mc_add 'FDT_EXPR_1300T'. mc_add 'FDT_EXPR_1302S'.
  mc_add 'FDT_EXPR_1302T'. mc_add 'FDT_EXPR_1303S'.
  mc_add 'FDT_EXPR_1303T'. mc_add 'FDT_EXPR_1304S'.
  mc_add 'FDT_EXPR_1304T'. mc_add 'FDT_EXPR_1400S'.
  mc_add 'FDT_EXPR_1400T'. mc_add 'FDT_EXPR_1401S'.
  mc_add 'FDT_EXPR_1401T'. mc_add 'FDT_EXPR_1500S'.
  mc_add 'FDT_EXPR_1500T'. mc_add 'FDT_EXPR_1505S'.
  mc_add 'FDT_EXPR_1505T'. mc_add 'FDT_EXPR_1506S'.
  mc_add 'FDT_EXPR_1506T'. mc_add 'FDT_EXPR_1507S'.
  mc_add 'FDT_EXPR_1507T'. mc_add 'FDT_EXPR_1600S'.
  mc_add 'FDT_EXPR_1600T'. mc_add 'FDT_EXPR_1601S'.
  mc_add 'FDT_EXPR_1601T'. mc_add 'FDT_EXPR_1700S'.
  mc_add 'FDT_EXPR_1700T'. mc_add 'FDT_EXPR_1800S'.
  mc_add 'FDT_EXPR_1800T'. mc_add 'FDT_EXPR_1801S'.
  mc_add 'FDT_EXPR_1801T'. mc_add 'FDT_EXPR_1900S'.
  mc_add 'FDT_EXPR_1900T'. mc_add 'FDT_EXPR_1910S'.
  mc_add 'FDT_EXPR_1910T'. mc_add 'FDT_EXPR_1920S'.
  mc_add 'FDT_EXPR_1920T'. mc_add 'FDT_EXPR_1930S'.
  mc_add 'FDT_EXPR_1930T'. mc_add 'FDT_EXPR_2000S'.
  mc_add 'FDT_EXPR_2000T'. mc_add 'FDT_EXPR_2100S'.
  mc_add 'FDT_EXPR_2100T'. mc_add 'FDT_EXPR_2101S'.
  mc_add 'FDT_EXPR_2101T'. mc_add 'FDT_EXPR_2200S'.
  mc_add 'FDT_EXPR_2200T'. mc_add 'FDT_EXPR_2201S'.
  mc_add 'FDT_EXPR_2201T'. mc_add 'FDT_EXPR_2202S'.
  mc_add 'FDT_EXPR_2202T'. mc_add 'FDT_EXPR_2203S'.
  mc_add 'FDT_EXPR_2203T'. mc_add 'FDT_EXPR_2400S'.
  mc_add 'FDT_EXPR_2400T'. mc_add 'FDT_EXPR_2401S'.
  mc_add 'FDT_EXPR_2401T'. mc_add 'FDT_EXPR_2402S'.
  mc_add 'FDT_EXPR_2402T'. mc_add 'FDT_EXPR_2500S'.
  mc_add 'FDT_EXPR_2500T'. mc_add 'FDT_EXPR_2501S'.
  mc_add 'FDT_EXPR_2501T'. mc_add 'FDT_EXPR_2502S'.
  mc_add 'FDT_EXPR_2502T'. mc_add 'FDT_EXPR_2600S'.
  mc_add 'FDT_EXPR_2600T'. mc_add 'FDT_EXPR_2601S'.
  mc_add 'FDT_EXPR_2601T'. mc_add 'FDT_EXPR_2602S'.
  mc_add 'FDT_EXPR_2602T'. mc_add 'FDT_EXPR_2603S'.
  mc_add 'FDT_EXPR_2603T'. mc_add 'FDT_EXPR_2604S'.
  mc_add 'FDT_EXPR_2604T'. mc_add 'FDT_EXPR_2610S'.
  mc_add 'FDT_EXPR_2610T'. mc_add 'FDT_EXPR_2650S'.
  mc_add 'FDT_EXPR_2650T'. mc_add 'FDT_EXPR_2651S'.
  mc_add 'FDT_EXPR_2651T'. mc_add 'FDT_EXPR_2652S'.
  mc_add 'FDT_EXPR_2652T'. mc_add 'FDT_EXPR_2653S'.
  mc_add 'FDT_EXPR_2653T'. mc_add 'FDT_EXPR_2654S'.
  mc_add 'FDT_EXPR_2654T'. mc_add 'FDT_EXPR_2655S'.
  mc_add 'FDT_EXPR_2655T'. mc_add 'FDT_EXPR_3000S'.
  mc_add 'FDT_EXPR_3000T'. mc_add 'FDT_EXPR_3001S'.
  mc_add 'FDT_EXPR_3001T'. mc_add 'FDT_EXPR_3002S'.
  mc_add 'FDT_EXPR_3002T'. mc_add 'FDT_EXPR_4000S'.
  mc_add 'FDT_EXPR_4000T'. mc_add 'FDT_EXPR_4001S'.
  mc_add 'FDT_EXPR_4001T'. mc_add 'FDT_EXPR_4002S'.
  mc_add 'FDT_EXPR_4002T'. mc_add 'FDT_EXPR_4003S'.
  mc_add 'FDT_EXPR_4003T'. mc_add 'FDT_EXPR_8000S'.
  mc_add 'FDT_EXPR_8000T'. mc_add 'FDT_EXPR_8001S'.
  mc_add 'FDT_EXPR_8001T'. mc_add 'FDT_EXPR_9000S'.
  mc_add 'FDT_EXPR_9000T'. mc_add 'FDT_EXPR_9001S'.
  mc_add 'FDT_EXPR_9001T'. mc_add 'FDT_EXPR_9002S'.
  mc_add 'FDT_EXPR_9002T'. mc_add 'FDT_EXPR_9003S'.
  mc_add 'FDT_EXPR_9003T'. mc_add 'FDT_EXPR_E0242S'.
  mc_add 'FDT_EXPR_E0242T'. mc_add 'FDT_EXPR_L0242S'.
  mc_add 'FDT_EXPR_L0242T'. mc_add 'FDT_EXTY_0000S'.
  mc_add 'FDT_EXTY_0000T'. mc_add 'FDT_EXTY_0100S'.
  mc_add 'FDT_EXTY_0100T'. mc_add 'FDT_EXTY_0200S'.
  mc_add 'FDT_EXTY_0200T'. mc_add 'FDT_EXTY_0201S'.
  mc_add 'FDT_EXTY_0201T'. mc_add 'FDT_FILTER_0000S'.
  mc_add 'FDT_FILTER_0000T'. mc_add 'FDT_FILTER_0001S'.
  mc_add 'FDT_FILTER_0001T'. mc_add 'FDT_FILTER_0002S'.
  mc_add 'FDT_FILTER_0002T'. mc_add 'FDT_FNCT_0100S'.
  mc_add 'FDT_FNCT_0100T'. mc_add 'FDT_FNCT_0110S'.
  mc_add 'FDT_FNCT_0110T'. mc_add 'FDT_FNCT_0150S'.
  mc_add 'FDT_FNCT_0150T'. mc_add 'FDT_GEN_ENQUEUE'.
  mc_add 'FDT_HELPERS_TEXT'. mc_add 'FDT_INTRP_0000'.
  mc_add 'FDT_ISSU_0010S'. mc_add 'FDT_ISSU_0010T'.
  mc_add 'FDT_JOB_0000'. mc_add 'FDT_JOB_0010'. mc_add 'FDT_JOB_0020'.
  mc_add 'FDT_JOB_0030'. mc_add 'FDT_PROC_ENQUEUE'.
  mc_add 'FDT_Q_ENQUEUE'. mc_add 'FDT_QUERY_0000S'.
  mc_add 'FDT_QUERY_0000T'. mc_add 'FDT_REPO_0000'.
  mc_add 'FDT_REPO_0010'. mc_add 'FDT_REPO_0020'.
  mc_add 'FDT_REPO_0025'. mc_add 'FDT_REPO_0030'.
  mc_add 'FDT_RLST_0000S'. mc_add 'FDT_RLST_0000T'.
  mc_add 'FDT_RLST_0001S'. mc_add 'FDT_RLST_0001T'.
  mc_add 'FDT_RLST_1000S'. mc_add 'FDT_RLST_1000T'.
  mc_add 'FDT_RLST_1100S'. mc_add 'FDT_RLST_1100T'.
  mc_add 'FDT_RLST_1200S'. mc_add 'FDT_RLST_1200T'.
  mc_add 'FDT_TEST_PROF'. mc_add 'FDT_TEST_PROFILE'.
  mc_add 'FDT_TEST_XML'. mc_add 'FDT_TEST_XML_CON'.
  mc_add 'FDT_TR_MAP_GUID'. mc_add 'FDT_TRANS_Q_0000'.
  mc_add 'FDT_TRANS_Q_0001'. mc_add 'FDT_TRANS_RT_000'.
  mc_add 'FDT_TRREQUEST'. mc_add 'FDT_TRREQUEST_TY'.
  mc_add 'FDT_TRSYSTEM'. mc_add 'FDT_WD_INFRA_000'.
  mc_add 'FDT_WD_INFRA_001'. mc_add 'FDT_WD_ISL_TEXT'.
  mc_add 'FDT_WD_ISLAND_UT'. mc_add 'FDT_XML_EXPORT'.
  mc_add 'FDT_XML_IMPORT'. mc_add 'FDTRESERVED'. mc_add 'FIELDDIFTB'.
  mc_add 'FILECMCUST'. mc_add 'FILECMCUSTP'. mc_add 'FILECMCUSTT'.
  mc_add 'FILENAMECI'. mc_add 'FILEPATH'. mc_add 'FILESYS'.
  mc_add 'FILETEXTCI'. mc_add 'FINI'. mc_add 'FINK'. mc_add 'FINP'.
  mc_add 'FINPL'. mc_add 'FINS'. mc_add 'FINT'. mc_add 'FINV'.
  mc_add 'FLAPP'. mc_add 'FLEMP'. mc_add 'FLIGHTB'. mc_add 'FLIGHTM'.
  mc_add 'FLIGHTP'. mc_add 'FLIGHTS'. mc_add 'FLJET'. mc_add 'FLLOG'.
  mc_add 'FLPAS'. mc_add 'FPB_DIALOG'. mc_add 'FPB_DIALOGT'.
  mc_add 'FPB_PERSAPPL'. mc_add 'FPB_PERSAPPLFLD'.
  mc_add 'FPB_PERSAPPLT'. mc_add 'FPB_PERSARR'. mc_add 'FPB_PERSARRT'.
  mc_add 'FPB_PERSDIALOG'. mc_add 'FPB_PERSTAB'. mc_add 'FPB_PERSTABT'.
  mc_add 'FPB_PERSVAR'. mc_add 'FPB_PERSVART'. mc_add 'FPB_SUBCONTEXT'.
  mc_add 'FPB_SUBCONTEXTT'. mc_add 'FPCHKIMPLMNTORS'.
  mc_add 'FPCONNECT'. mc_add 'FPCONTEXT'. mc_add 'FPCONTEXTI'.
  mc_add 'FPCONTEXTR'. mc_add 'FPCONTEXTT'. mc_add 'FPFONTREPL'.
  mc_add 'FPINTERFACE'. mc_add 'FPINTERFACET'. mc_add 'FPJPMAPPING'.
  mc_add 'FPLAYOUT'. mc_add 'FPLAYOUTT'. mc_add 'FPLOG'.
  mc_add 'FPM_ADAPT_DIM'. mc_add 'FPM_ADAPT_LINK'.
  mc_add 'FPM_ADAPT_RULE'. mc_add 'FPM_ADAPT_SCHEMA'.
  mc_add 'FPM_ADAPT_SCHEMT'. mc_add 'FPM_ADAPT_SET'.
  mc_add 'FPM_APPL_AREA'. mc_add 'FPM_APPL_AREA_T'.
  mc_add 'FPM_IDA_TEST_EVT'. mc_add 'FPM_IDA_TEST_REF'.
  mc_add 'FPM_IDA_TEST_RES'. mc_add 'FPM_T_ATTACHMENT'.
  mc_add 'FPM_T_MSG_CATEGO'. mc_add 'FPM_T_NOTES'.
  mc_add 'FPM_TEST_ALPHA'. mc_add 'FPM_UT_ALPHA'.
  mc_add 'FPMATTACHCHKF'. mc_add 'FPMATTACHCHKO'.
  mc_add 'FPMATTACHIDXSTA'. mc_add 'FPMATTACHLOIO'.
  mc_add 'FPMATTACHLOIOT'. mc_add 'FPMATTACHLOPR'.
  mc_add 'FPMATTACHLORE'. mc_add 'FPMATTACHLOREPR'.
  mc_add 'FPMATTACHLORI'. mc_add 'FPMATTACHLORIPR'.
  mc_add 'FPMATTACHPHF'. mc_add 'FPMATTACHPHHR'.
  mc_add 'FPMATTACHPHHRPR'. mc_add 'FPMATTACHPHIO'.
  mc_add 'FPMATTACHPHNM'. mc_add 'FPMATTACHPHNMPR'.
  mc_add 'FPMATTACHPHPR'. mc_add 'FPMATTACHPHRE'.
  mc_add 'FPMATTACHPHREPR'. mc_add 'FPMATTACHPHRI'.
  mc_add 'FPMATTACHPHRIPR'. mc_add 'FPMNOTES1CHKF'.
  mc_add 'FPMNOTES1CHKO'. mc_add 'FPMNOTES1IDXSTA'.
  mc_add 'FPMNOTES1LOIO'. mc_add 'FPMNOTES1LOIOT'.
  mc_add 'FPMNOTES1LOPR'. mc_add 'FPMNOTES1LORE'.
  mc_add 'FPMNOTES1LOREPR'. mc_add 'FPMNOTES1LORI'.
  mc_add 'FPMNOTES1LORIPR'. mc_add 'FPMNOTES1PHF'.
  mc_add 'FPMNOTES1PHHR'. mc_add 'FPMNOTES1PHHRPR'.
  mc_add 'FPMNOTES1PHIO'. mc_add 'FPMNOTES1PHNM'.
  mc_add 'FPMNOTES1PHNMPR'. mc_add 'FPMNOTES1PHPR'.
  mc_add 'FPMNOTES1PHRE'. mc_add 'FPMNOTES1PHREPR'.
  mc_add 'FPMNOTES1PHRI'. mc_add 'FPMNOTES1PHRIPR'.
  mc_add 'FPMNOTESPHIO'. mc_add 'FPNUMBERLEVEL'. mc_add 'FPXFP'.
  mc_add 'FREEZEWARN'. mc_add 'FREF'. mc_add 'FRODIR'.
  mc_add 'FSYSCHKF'. mc_add 'FSYSCHKO'. mc_add 'FSYSCONT'.
  mc_add 'FSYSIDXSTA'. mc_add 'FSYSLOIO'. mc_add 'FSYSLOIOT'.
  mc_add 'FSYSLOPR'. mc_add 'FSYSLORE'. mc_add 'FSYSLOREPR'.
  mc_add 'FSYSLORI'. mc_add 'FSYSPHF'. mc_add 'FSYSPHHR'.
  mc_add 'FSYSPHIO'. mc_add 'FSYSPHNM'. mc_add 'FSYSPHPR'.
  mc_add 'FSYSPHRE'. mc_add 'FSYSPHREPR'. mc_add 'FSYSPHRI'.
  mc_add 'FSYSTXT'. mc_add 'FUNCT'. mc_add 'FUPARAREF'.
  mc_add 'FUPARAREF_ENHA'. mc_add 'FUSEQDIR'. mc_add 'FVERSIONDB'.
  mc_add 'GBADI_IMPL'. mc_add 'GCLIENTS'. mc_add 'GCTAB'.
  mc_add 'GE071KF'. mc_add 'GE071KF_ARCHIVE'. mc_add 'GE071KF_LOCAL'.
  mc_add 'GE071KF_OLD_NT'. mc_add 'GEN_HISTORY'. mc_add 'GENSETC'.
  mc_add 'GENSETM'. mc_add 'GEOCD2CLS'. mc_add 'GEOCDRLFLD'.
  mc_add 'GEOMAPPERS'. mc_add 'GEOOBJ2CLS'. mc_add 'GEOZ5GOLD'.
  mc_add 'GFBDIR'. mc_add 'GGRP'. mc_add 'GINTERF'. mc_add 'GLOSSARY'.
  mc_add 'GLOSSARY0'. mc_add 'GLOSSARY1'. mc_add 'GLOSSARY2'.
  mc_add 'GLOSSARY3'. mc_add 'GLOSSARY4'. mc_add 'GLOSSARYA'.
  mc_add 'GODIR'. mc_add 'GODIRLOCK'. mc_add 'GOSDIR'. mc_add 'GOSTYPE'.
  mc_add 'GRMG_AWS_CTRL'. mc_add 'GRMG_COMPONENT_T'.
  mc_add 'GRMG_COMPONENTS'. mc_add 'GRMG_CONTROL'.
  mc_add 'GRMG_HTML_TAGS'. mc_add 'GRMG_MESSAGES'.
  mc_add 'GRMG_PROPERTIES'. mc_add 'GRMG_SCENARIO_T'.
  mc_add 'GRMG_SCENARIOS'. mc_add 'GSODIR'. mc_add 'GST04SRVRS'.
  mc_add 'GSUBT_CONFLICTS'. mc_add 'GSUBT_EXCEPT'. mc_add 'GSUBT_LOCAL'.
  mc_add 'GSUBTYPES'. mc_add 'GSWCGRP'. mc_add 'GSWCGRP_SERVER'.
  mc_add 'GSWCGRP_SERVER_A'. mc_add 'GSWCGRPC'. mc_add 'GTABKEY'.
  mc_add 'GTABKEY_ADM'. mc_add 'GTABKEY_ADM_COMP'.
  mc_add 'GTABKEY_ARCHIVE'. mc_add 'GTABKEY_C_SYS'.
  mc_add 'GTABKEY_CASCADE'. mc_add 'GTABKEY_COMPEXCL'.
  mc_add 'GTABKEY_COMPRESS'. mc_add 'GTABKEY_COMPSETS'.
  mc_add 'GTABKEY_CUST'. mc_add 'GTABKEY_EXT'. mc_add 'GTABKEY_LOCAL'.
  mc_add 'GTABKEY_MAIL'. mc_add 'GTABKEY_MAILCUST'.
  mc_add 'GTABKEY_MAPPING'. mc_add 'GTABKEY_SYSTEMS'.
  mc_add 'GTABKEY_TABEXCL'. mc_add 'GTABKEY_TOOLBO'.
  mc_add 'GTABKEY_TOOLBT'. mc_add 'GTABKEYEXC_ARCH'.
  mc_add 'GTABKEYEXC20'. mc_add 'GTABLES'. mc_add 'GTACLIENTS'.
  mc_add 'GTADIR'. mc_add 'GTADIRA'. mc_add 'GTODIR'.
  mc_add 'GUI_E2E_TRACES'. mc_add 'GUI_E2E_TRACES_R'.
  mc_add 'GUI_E2E_XML'. mc_add 'GUI_FKEY'. mc_add 'GUI_FKEYT'.
  mc_add 'GUIDIR'. mc_add 'GUIDIRT'. mc_add 'GUINODE'. mc_add 'GUINODT'.
  mc_add 'GUPDLAYOUT'. mc_add 'GUSER'. mc_add 'GVD_BGPROCESS'.
  mc_add 'GVD_BUFF_POOL_ST'. mc_add 'GVD_CURR_BLKSRV'.
  mc_add 'GVD_DATABASE'. mc_add 'GVD_DATAFILE'.
  mc_add 'GVD_DATAGUARD_ST'. mc_add 'GVD_DB_CACHE_ADV'.
  mc_add 'GVD_ENQUEUE_STAT'. mc_add 'GVD_FILESTAT'.
  mc_add 'GVD_INSTANCE'. mc_add 'GVD_LATCH'. mc_add 'GVD_LATCH_MISSES'.
  mc_add 'GVD_LATCH_PARENT'. mc_add 'GVD_LATCHCHILDS'.
  mc_add 'GVD_LATCHHOLDER'. mc_add 'GVD_LATCHNAME'.
  mc_add 'GVD_LIBRARYCACHE'. mc_add 'GVD_LOCK'.
  mc_add 'GVD_LOCK_ACTIVTY'. mc_add 'GVD_LOCKED_OBJEC'.
  mc_add 'GVD_LOGFILE'. mc_add 'GVD_MANGD_STANBY'.
  mc_add 'GVD_OBJECT_DEPEN'. mc_add 'GVD_PARAMETER'.
  mc_add 'GVD_PARAMETER2'. mc_add 'GVD_PGA_TARGET_A'.
  mc_add 'GVD_PGA_TARGET_H'. mc_add 'GVD_PGASTAT'. mc_add 'GVD_PROCESS'.
  mc_add 'GVD_PX_SESSION'. mc_add 'GVD_ROWCACHE'.
  mc_add 'GVD_SEGMENT_STAT'. mc_add 'GVD_SEGSTAT'.
  mc_add 'GVD_SERVERLIST'. mc_add 'GVD_SESS_IO'. mc_add 'GVD_SESSION'.
  mc_add 'GVD_SESSION_EVT'. mc_add 'GVD_SESSION_WAIT'.
  mc_add 'GVD_SESSTAT'. mc_add 'GVD_SGA'. mc_add 'GVD_SGACURRRESIZ'.
  mc_add 'GVD_SGADYNCOMP'. mc_add 'GVD_SGADYNFREE'.
  mc_add 'GVD_SGARESIZEOPS'. mc_add 'GVD_SGASTAT'.
  mc_add 'GVD_SHAR_P_ADV'. mc_add 'GVD_SPPARAMETER'. mc_add 'GVD_SQL'.
  mc_add 'GVD_SQL_WA_ACTIV'. mc_add 'GVD_SQL_WA_HISTO'.
  mc_add 'GVD_SQL_WORKAREA'. mc_add 'GVD_SQLAREA'. mc_add 'GVD_SQLTEXT'.
  mc_add 'GVD_SYSSTAT'. mc_add 'GVD_SYSTEM_EVENT'.
  mc_add 'GVD_TEMPFILE'. mc_add 'GVD_UNDOSTAT'. mc_add 'GVD_WAITSTAT'.
  mc_add 'GVD_WPTOTALINFO'. mc_add 'GWFEODE'. mc_add 'HALOAD_CALLCHECK'.
  mc_add 'HALOAD_DB_CHECK'. mc_add 'HALOAD_DB_CUST'.
  mc_add 'HALOAD_DB_MAT'. mc_add 'HALOAD_DB_ORDER'.
  mc_add 'HALOAD_DB_ORDITM'. mc_add 'HALOAD_ENQ_TAB'.
  mc_add 'HALOAD_INITCHECK'. mc_add 'HALOAD_RUN'.
  mc_add 'HALOAD_RUN_ELEM'. mc_add 'HALOAD_SUBPARAMS'.
  mc_add 'HCSKW_APPL'. mc_add 'HCSKW_APPLASS'. mc_add 'HCSKW_CLOS'.
  mc_add 'HCSKW_COMPASS'. mc_add 'HCSKW_DIR'. mc_add 'HCSKW_DOC'.
  mc_add 'HCSKW_REFLOIO'. mc_add 'HCSKW_REL'. mc_add 'HCSKW_RELH'.
  mc_add 'HCSKW_STRUCASS'. mc_add 'HCSKW_STRUCTURE'.
  mc_add 'HCSKW_TEST_DIR'. mc_add 'HCSKW_TEST_FILE'.
  mc_add 'HCSKW_TEST_RES'. mc_add 'HCSKW_TEXT'. mc_add 'HDB_ABAPCLIENT'.
  mc_add 'HDB_ABAPCLIENT2'. mc_add 'HIT_CMP_AUS'. mc_add 'HLPINDX'.
  mc_add 'HLPREF'. mc_add 'HLPREFEXT'. mc_add 'HLPREFM'.
  mc_add 'HLPRESOL'. mc_add 'HLPRESOLS'. mc_add 'HLPSETTING'.
  mc_add 'HLPSTAT_F1'. mc_add 'HLPTXT'. mc_add 'HOKRU_TTAB'.
  mc_add 'HOTIEXPERT'. mc_add 'HOTPACKEXC'. mc_add 'HOTPACKMOD'.
  mc_add 'HOTPACKMODLOG'. mc_add 'HOTPACKWRN'. mc_add 'HOTSPOT_DEF'.
  mc_add 'HRC1002'. mc_add 'HRC1206'. mc_add 'HRC1212'.
  mc_add 'HRS1000'. mc_add 'HRS1002'. mc_add 'HRS1200'.
  mc_add 'HRS1201'. mc_add 'HRS1202'. mc_add 'HRS1203'.
  mc_add 'HRS1205'. mc_add 'HRS1206'. mc_add 'HRS1210'.
  mc_add 'HRS1211'. mc_add 'HRS1212'. mc_add 'HRS1213'.
  mc_add 'HRS1214'. mc_add 'HRS1216'. mc_add 'HRS1220'.
  mc_add 'HRS1221'. mc_add 'HRSCONT'. mc_add 'HRSOBJECT'.
  mc_add 'HTTP_ADMIN_LIST'. mc_add 'HTTP_CHW_LOG_ADM'.
  mc_add 'HTTP_CHW_LOG_SET'. mc_add 'HTTP_CHW_LOG_WHI'.
  mc_add 'HTTP_CHW_SETUP'. mc_add 'HTTP_ENTRY_CONV'.
  mc_add 'HTTP_LOG_LIST'. mc_add 'HTTP_SUPP_LIST'.
  mc_add 'HTTP_WHITE_LIST'. mc_add 'I18NSRHGR'. mc_add 'I18NSRHGRU'.
  mc_add 'I18NSRHLOG'. mc_add 'I18NSRHRES'. mc_add 'I18NSRHWL'.
  mc_add 'IACFL'. mc_add 'IACFLOW'. mc_add 'IACHTML'. mc_add 'IACHTMLL'.
  mc_add 'IACHTMLLP'. mc_add 'IACHTMLP'. mc_add 'IACHTRC'.
  mc_add 'IACHTRCL'. mc_add 'IACHTRCP'. mc_add 'IACJSCR'.
  mc_add 'IACKEYCOLD'. mc_add 'IACKEYCONV'. mc_add 'IACMIME'.
  mc_add 'IACMIMEL'. mc_add 'IACMIMELP'. mc_add 'IACMIMEP'.
  mc_add 'IACML'. mc_add 'IACML_C'. mc_add 'IACMTXTEND'. mc_add 'IACMU'.
  mc_add 'IACMU_C'. mc_add 'IACMUT'. mc_add 'IACORDES'.
  mc_add 'IACORDEST'. mc_add 'IACORSITE'. mc_add 'IACR'.
  mc_add 'IACR_C'. mc_add 'IACRT'. mc_add 'IACS'. mc_add 'IACS_C'.
  mc_add 'IACSERVICE'. mc_add 'IACSERVP'. mc_add 'IACST'.
  mc_add 'IACTL'. mc_add 'IACTL_C'. mc_add 'IACTL_C_TEMP'.
  mc_add 'IACTL_C1'. mc_add 'IACTU'. mc_add 'IACTU_C'.
  mc_add 'IACTU_C_TEMP'. mc_add 'IACTU_C1'. mc_add 'IACTUT'.
  mc_add 'IACVERSTAB'. mc_add 'IACXU'. mc_add 'IACXU_C_TEMP'.
  mc_add 'IACXU_C1'. mc_add 'IACXUT'. mc_add 'IADB2'. mc_add 'IADB6'.
  mc_add 'IAINF'. mc_add 'IALINKS'. mc_add 'IALINKST'.
  mc_add 'IALINKTRAN'. mc_add 'IALINKURLS'. mc_add 'IALSAPFILE'.
  mc_add 'IALSAPFILT'. mc_add 'IAMSS'. mc_add 'IAORA'.
  mc_add 'IATNODE01'. mc_add 'IATNODE01R'. mc_add 'IATNODE01T'.
  mc_add 'ICF_EXTALIAS_TF'. mc_add 'ICF_SERV_STAT'.
  mc_add 'ICF_SERVICE_TF'. mc_add 'ICFADMINVH'. mc_add 'ICFALIAS'.
  mc_add 'ICFAPPLCUST'. mc_add 'ICFAPPLICATION'. mc_add 'ICFBUFFER'.
  mc_add 'ICFDELTA'. mc_add 'ICFDOCU'. mc_add 'ICFHANDLER'.
  mc_add 'ICFHANDTREE'. mc_add 'ICFIMPORT'. mc_add 'ICFINSTACT'.
  mc_add 'ICFKATEGORIE'. mc_add 'ICFLOGORDER'. mc_add 'ICFMEMORY'.
  mc_add 'ICFNOHANDLER'. mc_add 'ICFSECPASSWD'. mc_add 'ICFSERVICE'.
  mc_add 'ICFSERVLOC'. mc_add 'ICFSRV4ADM_LOLOG'. mc_add 'ICFSRV4ADMIN'.
  mc_add 'ICFSRV4ADMIN_LOG'. mc_add 'ICFVIRHOST'. mc_add 'ICO_P'.
  mc_add 'ICON'. mc_add 'ICON_CL'. mc_add 'ICON_GR'. mc_add 'ICONB'.
  mc_add 'ICONP'. mc_add 'ICONT'. mc_add 'ICP_DBSTATS'.
  mc_add 'ICUTRANSID'. mc_add 'IDES_LINKS'. mc_add 'IDES_TEXTS'.
  mc_add 'IDM_BADI_FILTER'. mc_add 'IDM_BADI_TEXT'. mc_add 'IDOCHIS'.
  mc_add 'IDOCSTYLE'. mc_add 'IDOCSYN'. mc_add 'IDXEDISDEF'.
  mc_add 'IDXEDSAPPL'. mc_add 'IDXGUIDDOC'. mc_add 'IDXIDOCINB'.
  mc_add 'IDXIDOCSYN'. mc_add 'IDXLNKOBJ'. mc_add 'IDXMDMPLANGU'.
  mc_add 'IDXMDMPRCV'. mc_add 'IDXNOALE'. mc_add 'IDXPORSM59'.
  mc_add 'IDXQUEUE'. mc_add 'IDXRCVPOR'. mc_add 'IDXSLOAD'.
  mc_add 'IDXSNDBSI'. mc_add 'IDXSNDPOR'. mc_add 'IDXSTRUCT'.
  mc_add 'IDXTRCMONI'. mc_add 'IFOBJLONG'. mc_add 'IFOBJSHORT'.
  mc_add 'IGDB2'. mc_add 'IGINF'. mc_add 'IGMSS'. mc_add 'IGORA'.
  mc_add 'ILM_T_SPOOL_PAR'. mc_add 'IMCSERVICE'. mc_add 'INDEX_STAT'.
  mc_add 'INDHEAD'. mc_add 'INDLAPR'. mc_add 'INDPROT'.
  mc_add 'INDTABS'. mc_add 'INDTEX2'. mc_add 'INDTEXT'.
  mc_add 'INDX_HIER'. mc_add 'INDX_HSRCH'. mc_add 'INDXBCSET'.
  mc_add 'INFCFGAL'. mc_add 'INFCFGALRS'. mc_add 'INFCFGDBG'.
  mc_add 'INFCFGRESD'. mc_add 'INFCFGRESM'. mc_add 'INFCFGTHRE'.
  mc_add 'INFREQ4US'. mc_add 'INPUT_TAB'. mc_add 'INSTCHECK'.
  mc_add 'INSTCHKTAB'. mc_add 'INSTCHKTBT'. mc_add 'INSTCNTL'.
  mc_add 'INSTCNTLJ'. mc_add 'INSTCNTLT'. mc_add 'INSTPARA'.
  mc_add 'INSTVERS'. mc_add 'INTF'. mc_add 'INTFTEXT'. mc_add 'IODIR'.
  mc_add 'IQICATDB'. mc_add 'IQICATDBX'. mc_add 'IQQCATDB'.
  mc_add 'IQQCATDBX'. mc_add 'IRM_T_BS_ARC_OBJ'.
  mc_add 'IRM_T_BS_BOR_OBJ'. mc_add 'IRM_T_BS_FLD_BVD'.
  mc_add 'IRM_T_BS_FLD_DVD'. mc_add 'IRM_T_BS_KFD_BVD'.
  mc_add 'IRM_T_BS_KOW_BVD'. mc_add 'IRM_T_BS_STT_BVD'.
  mc_add 'IRM_T_BS_STT_DVD'. mc_add 'IRM_T_CON'. mc_add 'IRM_T_CON_T'.
  mc_add 'IRM_T_EXIT_CUST'. mc_add 'IRM_T_EXIT_POL'.
  mc_add 'IRM_T_OBJ_CAT'. mc_add 'IRM_T_OBJ_CAT_MF'.
  mc_add 'IRM_T_OBJ_CAT_T'. mc_add 'IRM_T_OBJ_TYPE'.
  mc_add 'IRM_T_OBJ_TYPE_T'. mc_add 'IRM_T_OT_POL_CAT'.
  mc_add 'IRM_T_OT_POL_FLD'. mc_add 'IRM_T_OT_POL_STT'.
  mc_add 'IRM_T_POL_CAT'. mc_add 'IRM_T_POL_CAT_T'.
  mc_add 'IRM_T_POL_TYPE'. mc_add 'IRM_T_POL_TYPE_T'.
  mc_add 'IRM_T_ROOT_NODE'. mc_add 'IRW_ARCH_OBJ'.
  mc_add 'IRW_ARCH_RELA'. mc_add 'IRW_ARCH_STRUCT'.
  mc_add 'IRW_LTARCHDEF'. mc_add 'IRW_LTARCHOBJ'. mc_add 'IRW_LTCDE'.
  mc_add 'IRW_LTCLUSTBL'. mc_add 'IRW_LTCONFIG'. mc_add 'IRW_LTJOINMAP'.
  mc_add 'IRW_LTMAPTABLE'. mc_add 'IRW_LTMID_INFO'. mc_add 'IRW_LTRUN'.
  mc_add 'IRW_LTSTAT'. mc_add 'IRW_OBJ_SEL'. mc_add 'IRW_STRUC_ALIAS'.
  mc_add 'ISSUE_ATTRIB'. mc_add 'ISSUE_REL'. mc_add 'ISSUE_RELATION'.
  mc_add 'ITS_CSS'. mc_add 'ITS_F_LIST'. mc_add 'ITS_FRAMES'.
  mc_add 'ITS_POS'. mc_add 'ITS_SHOW'. mc_add 'ITS_STYLES'.
  mc_add 'ITS_TAGS'. mc_add 'ITSTEST'. mc_add 'ITSTESTMOB'.
  mc_add 'ITSTESTMOBART'. mc_add 'ITSTESTRBX'. mc_add 'IWADM'.
  mc_add 'IWAREADEST'. mc_add 'IWAUTHFCT'. mc_add 'IWAUTHFCTT'.
  mc_add 'IWB_BF_EHP_IOA'. mc_add 'IWB_BF_EHP_IOH'.
  mc_add 'IWB_BF_EHP_IOV'. mc_add 'IWB_BF_F4'. mc_add 'IWB_BF_SID'.
  mc_add 'IWB_EHP_CONTEXTH'. mc_add 'IWB_EHP_CONTEXTV'.
  mc_add 'IWB_EHP_F4'. mc_add 'IWB_EHP_SWC'. mc_add 'IWB_MACRO_PARA'.
  mc_add 'IWB_NEWREL'. mc_add 'IWB_SID_E18'. mc_add 'IWB_WF_IO'.
  mc_add 'IWB0CHKF'. mc_add 'IWB0CHKO'. mc_add 'IWB0COMP'.
  mc_add 'IWB0CONT1'. mc_add 'IWB0IDXSTA'. mc_add 'IWB0LOIO'.
  mc_add 'IWB0LOIOT'. mc_add 'IWB0LOPR'. mc_add 'IWB0LORE'.
  mc_add 'IWB0LORI'. mc_add 'IWB0NODE'. mc_add 'IWB0NODER'.
  mc_add 'IWB0NODET'. mc_add 'IWB0PHF'. mc_add 'IWB0PHHR'.
  mc_add 'IWB0PHHRPR'. mc_add 'IWB0PHIO'. mc_add 'IWB0PHNM'.
  mc_add 'IWB0PHNMPR'. mc_add 'IWB0PHPR'. mc_add 'IWB0PHRE'.
  mc_add 'IWB0PHREPR'. mc_add 'IWB0PHRI'. mc_add 'IWB0PHRIPR'.
  mc_add 'IWB0RE'. mc_add 'IWB0REPR'. mc_add 'IWB1CHKF'.
  mc_add 'IWB1CHKO'. mc_add 'IWB1CONT1'. mc_add 'IWB1IDXSTA'.
  mc_add 'IWB1LOIO'. mc_add 'IWB1LOIOT'. mc_add 'IWB1LOPR'.
  mc_add 'IWB1LORE'. mc_add 'IWB1LORI'. mc_add 'IWB1NODE'.
  mc_add 'IWB1NODER'. mc_add 'IWB1NODET'. mc_add 'IWB1PDEL'.
  mc_add 'IWB1PHF'. mc_add 'IWB1PHHR'. mc_add 'IWB1PHIO'.
  mc_add 'IWB1PHNM'. mc_add 'IWB1PHPR'. mc_add 'IWB1PHRE'.
  mc_add 'IWB1PHRI'. mc_add 'IWB1RDEL'. mc_add 'IWB1RE'.
  mc_add 'IWB1REPR'. mc_add 'IWB2CHKF'. mc_add 'IWB2CHKO'.
  mc_add 'IWB2CONT1'. mc_add 'IWB2IDXSTA'. mc_add 'IWB2LOIO'.
  mc_add 'IWB2LOIOT'. mc_add 'IWB2LOPR'. mc_add 'IWB2LORE'.
  mc_add 'IWB2LORI'. mc_add 'IWB2NODE'. mc_add 'IWB2NODER'.
  mc_add 'IWB2NODET'. mc_add 'IWB2PDEL'. mc_add 'IWB2PHF'.
  mc_add 'IWB2PHHR'. mc_add 'IWB2PHIO'. mc_add 'IWB2PHNM'.
  mc_add 'IWB2PHPR'. mc_add 'IWB2PHRE'. mc_add 'IWB2PHRI'.
  mc_add 'IWB2RDEL'. mc_add 'IWB2RE'. mc_add 'IWB2REPR'.
  mc_add 'IWB3CONT1'. mc_add 'IWB4CHKF'. mc_add 'IWB4CHKO'.
  mc_add 'IWB4IDXSTA'. mc_add 'IWB4LOIO'. mc_add 'IWB4LOIOT'.
  mc_add 'IWB4LOPR'. mc_add 'IWB4LORE'. mc_add 'IWB4LORI'.
  mc_add 'IWB4PHF'. mc_add 'IWB4PHHR'. mc_add 'IWB4PHIO'.
  mc_add 'IWB4PHNM'. mc_add 'IWB4PHPR'. mc_add 'IWB4PHRE'.
  mc_add 'IWB4PHRI'. mc_add 'IWB5CHKF'. mc_add 'IWB5CHKO'.
  mc_add 'IWB5IDXSTA'. mc_add 'IWB5LOIO'. mc_add 'IWB5LOIOT'.
  mc_add 'IWB5LOPR'. mc_add 'IWB5LORE'. mc_add 'IWB5LORI'.
  mc_add 'IWB5PHF'. mc_add 'IWB5PHHR'. mc_add 'IWB5PHHRPR'.
  mc_add 'IWB5PHIO'. mc_add 'IWB5PHNM'. mc_add 'IWB5PHNMPR'.
  mc_add 'IWB5PHPR'. mc_add 'IWB5PHRE'. mc_add 'IWB5PHREPR'.
  mc_add 'IWB5PHRI'. mc_add 'IWB5PHRIPR'. mc_add 'IWB7CHKF'.
  mc_add 'IWB7CHKO'. mc_add 'IWB7IDXSTA'. mc_add 'IWB7LOIO'.
  mc_add 'IWB7LOIOT'. mc_add 'IWB7LOPR'. mc_add 'IWB7LORE'.
  mc_add 'IWB7LORI'. mc_add 'IWB7NODE'. mc_add 'IWB7NODER'.
  mc_add 'IWB7NODET'. mc_add 'IWB7PHF'. mc_add 'IWB7PHHR'.
  mc_add 'IWB7PHHRPR'. mc_add 'IWB7PHIO'. mc_add 'IWB7PHNM'.
  mc_add 'IWB7PHNMPR'. mc_add 'IWB7PHPR'. mc_add 'IWB7PHRE'.
  mc_add 'IWB7PHREPR'. mc_add 'IWB7PHRI'. mc_add 'IWB7PHRIPR'.
  mc_add 'IWBAPDEL'. mc_add 'IWBARDEL'. mc_add 'IWBENTRY_M'.
  mc_add 'IWBENTRY_S'. mc_add 'IWBENTRY_U'. mc_add 'IWBENTRY_V'.
  mc_add 'IWBKEP'. mc_add 'IWBKEP2'. mc_add 'IWBKEPDEST'.
  mc_add 'IWBKEYWRD'. mc_add 'IWBKEYWRDT'. mc_add 'IWBKMIG'.
  mc_add 'IWBREORG'. mc_add 'IWBREORG_S'. mc_add 'IWBSETTING'.
  mc_add 'IWBSTATEHIER'. mc_add 'IWBUUIDVAL'. mc_add 'IWBWFATTR'.
  mc_add 'IWBWFHEAD'. mc_add 'IWCLTYPE'. mc_add 'IWCODEPAGE'.
  mc_add 'IWCOMP_OBJ'. mc_add 'IWCOMP_RES'. mc_add 'IWDISPLAY'.
  mc_add 'IWDISPLAYT'. mc_add 'IWDOCFRMT'. mc_add 'IWEXADD'.
  mc_add 'IWEXDEST'. mc_add 'IWEXDONE'. mc_add 'IWEXEXOPTS'.
  mc_add 'IWEXEXOPTT'. mc_add 'IWEXINDX'. mc_add 'IWEXPHIO'.
  mc_add 'IWEXRTAB'. mc_add 'IWEXRTABT'. mc_add 'IWEXRULES'.
  mc_add 'IWEXSERVER'. mc_add 'IWEXSTATUS'. mc_add 'IWEXTEND'.
  mc_add 'IWEXTENDT'. mc_add 'IWEXTODO'. mc_add 'IWEXTSYMBS'.
  mc_add 'IWEXTSYMBT'. mc_add 'IWFLDRGRP'. mc_add 'IWFLDRGRPT'.
  mc_add 'IWFTRANS'. mc_add 'IWH_31_LNK'. mc_add 'IWICON'.
  mc_add 'IWIKS2STAT'. mc_add 'IWIKSVAR'. mc_add 'IWIKSVART'.
  mc_add 'IWINDUSTRY'. mc_add 'IWNODEICON'. mc_add 'IWNODETDEF'.
  mc_add 'IWOBJECT'. mc_add 'IWPROPVAL'. mc_add 'IWPROPVALT'.
  mc_add 'IWR_31_PRM'. mc_add 'IWR3LFOLD'. mc_add 'IWR3LRES'.
  mc_add 'IWREADLOIO'. mc_add 'IWREADPHIO'. mc_add 'IWREFERENC'.
  mc_add 'IWRELEASE'. mc_add 'IWRELFRZE'. mc_add 'IWSTDGRAF'.
  mc_add 'IWSTDGRAFT'. mc_add 'IWSTRANS'. mc_add 'IWSUBSCR'.
  mc_add 'IWSYSTEM'. mc_add 'IWSYSTEMT'. mc_add 'IWTESTFCTS'.
  mc_add 'IWTESTFCTT'. mc_add 'IWTESTUSER'. mc_add 'IWTREFERENC'.
  mc_add 'IXTYP'. mc_add 'IXTYT'. mc_add 'KDL_LOIO'. mc_add 'KDL_LOIOT'.
  mc_add 'KDL_LOPR'. mc_add 'KDL_LORE'. mc_add 'KDL_LORI'.
  mc_add 'KDP_CHKF'. mc_add 'KDP_CHKO'. mc_add 'KDP_IDXSTA'.
  mc_add 'KDP_PHF'. mc_add 'KDP_PHHR'. mc_add 'KDP_PHIO'.
  mc_add 'KDP_PHNM'. mc_add 'KDP_PHPR'. mc_add 'KDP_PHRE'.
  mc_add 'KDP_PHRI'. mc_add 'KDS_NODE'. mc_add 'KDS_NODER'.
  mc_add 'KDS_NODET'. mc_add 'KHE071K'. mc_add 'KHTEST001'.
  mc_add 'KHTEST71E'. mc_add 'KHTSTPOOL'. mc_add 'KPROCHKF'.
  mc_add 'KPROCHKO'. mc_add 'KPROIDXSTA'. mc_add 'KPROPHF'.
  mc_add 'KPROPHHR'. mc_add 'KPROPHIO'. mc_add 'KPROPHNM'.
  mc_add 'KPROPHPR'. mc_add 'KPROPHRE'. mc_add 'KPROPHRI'.
  mc_add 'KTADIR'. mc_add 'KTEST09L'. mc_add 'KTEST09L1'.
  mc_add 'KTEST09L2'. mc_add 'KTEST09L3'. mc_add 'KTEST09L8'.
  mc_add 'KTEST11'. mc_add 'KTEST12343456789'. mc_add 'KWBCHKF'.
  mc_add 'KWBCHKO'. mc_add 'KWBCONT'. mc_add 'KWBIDXSTA'.
  mc_add 'KWBLOIO'. mc_add 'KWBLOIOT'. mc_add 'KWBLOPR'.
  mc_add 'KWBLORE'. mc_add 'KWBLOREPR'. mc_add 'KWBLORI'.
  mc_add 'KWBLORIPR'. mc_add 'KWBPHF'. mc_add 'KWBPHHR'.
  mc_add 'KWBPHHRPR'. mc_add 'KWBPHIO'. mc_add 'KWBPHNM'.
  mc_add 'KWBPHNMPR'. mc_add 'KWBPHPR'. mc_add 'KWBPHRE'.
  mc_add 'KWBPHREPR'. mc_add 'KWBPHRI'. mc_add 'KWBPHRIPR'.
  mc_add 'KWCHAIN'. mc_add 'KWD_CL_VSP'. mc_add 'KWD_RETROFIT1'.
  mc_add 'KWD_RETROFIT2'. mc_add 'KWD_RETROFIT3'.
  mc_add 'KWD_RETROFIT4'. mc_add 'KWD_RETROFITH'. mc_add 'KWD_RFC2BI'.
  mc_add 'KWDEL_HEAD'. mc_add 'KWDEL_RES_OBJ'. mc_add 'KWDELTA_TR'.
  mc_add 'KWDELTA_WL'. mc_add 'KWFOLDERS'. mc_add 'KWH_BG'.
  mc_add 'KWMCHKF'. mc_add 'KWMCHKO'. mc_add 'KWMIDXSTA'.
  mc_add 'KWMLOIO'. mc_add 'KWMLOIOT'. mc_add 'KWMLOPR'.
  mc_add 'KWMLORE'. mc_add 'KWMLORI'. mc_add 'KWMPHF'. mc_add 'KWMPHHR'.
  mc_add 'KWMPHIO'. mc_add 'KWMPHNM'. mc_add 'KWMPHPR'.
  mc_add 'KWMPHRE'. mc_add 'KWMPHRI'. mc_add 'KWN_NODE'.
  mc_add 'KWN_NODER'. mc_add 'KWN_NODET'. mc_add 'KWNCHKF'.
  mc_add 'KWNCHKO'. mc_add 'KWNIDXSTA'. mc_add 'KWNLOIO'.
  mc_add 'KWNLOIOT'. mc_add 'KWNLOPR'. mc_add 'KWNLORE'.
  mc_add 'KWNLORI'. mc_add 'KWNPHF'. mc_add 'KWNPHHR'.
  mc_add 'KWNPHHRPR'. mc_add 'KWNPHIO'. mc_add 'KWNPHNM'.
  mc_add 'KWNPHNMPR'. mc_add 'KWNPHPR'. mc_add 'KWNPHRE'.
  mc_add 'KWNPHREPR'. mc_add 'KWNPHRI'. mc_add 'KWNPHRIPR'.
  mc_add 'KWPWCHKF'. mc_add 'KWPWCHKO'. mc_add 'KWPWCONT1'.
  mc_add 'KWPWIDXSTA'. mc_add 'KWPWLOIO'. mc_add 'KWPWLOIOT'.
  mc_add 'KWPWLOPR'. mc_add 'KWPWLORE'. mc_add 'KWPWLORI'.
  mc_add 'KWPWNODE'. mc_add 'KWPWNODER'. mc_add 'KWPWNODET'.
  mc_add 'KWPWPHF'. mc_add 'KWPWPHHR'. mc_add 'KWPWPHHRPR'.
  mc_add 'KWPWPHIO'. mc_add 'KWPWPHNM'. mc_add 'KWPWPHNMPR'.
  mc_add 'KWPWPHPR'. mc_add 'KWPWPHRE'. mc_add 'KWPWPHREPR'.
  mc_add 'KWPWPHRI'. mc_add 'KWPWPHRIPR'. mc_add 'KWTCHKF'.
  mc_add 'KWTCHKO'. mc_add 'KWTCONT'. mc_add 'KWTFLOIO'.
  mc_add 'KWTFLOIOT'. mc_add 'KWTFLOPR'. mc_add 'KWTFLORE'.
  mc_add 'KWTFLOREPR'. mc_add 'KWTFLORI'. mc_add 'KWTIDXSTA'.
  mc_add 'KWTLOIO'. mc_add 'KWTLOIOT'. mc_add 'KWTLOPR'.
  mc_add 'KWTLORE'. mc_add 'KWTLOREPR'. mc_add 'KWTLORI'.
  mc_add 'KWTPHF'. mc_add 'KWTPHHR'. mc_add 'KWTPHIO'. mc_add 'KWTPHNM'.
  mc_add 'KWTPHPR'. mc_add 'KWTPHRE'. mc_add 'KWTPHREPR'.
  mc_add 'KWTPHRI'. mc_add 'LADB6'. mc_add 'LANG_FTTEC'.
  mc_add 'LANG_LEXPT'. mc_add 'LANG_LOCK'. mc_add 'LANG_MANDT'.
  mc_add 'LANG_PEXPT'. mc_add 'LANGGRU'. mc_add 'LANGGRUT'.
  mc_add 'LANGOBJ'. mc_add 'LANGOBJT'. mc_add 'LANGTR1'.
  mc_add 'LANGTR2'. mc_add 'LANGUMS'. mc_add 'LANGUSR'.
  mc_add 'LANGZUG'. mc_add 'LANGZUS'. mc_add 'LAW_ATTR'.
  mc_add 'LAW_CONT'. mc_add 'LAW_DESCR_INST'. mc_add 'LAW_DESCR_SYSTEM'.
  mc_add 'LAW_ENGINE'. mc_add 'LAW_ERES'. mc_add 'LAW_ERROR'.
  mc_add 'LAW_GROUP'. mc_add 'LAW_GROUP_SYS'. mc_add 'LAW_GROUPING'.
  mc_add 'LAW_GROUPT'. mc_add 'LAW_PARAM'. mc_add 'LAW_PERS'.
  mc_add 'LAW_PROT'. mc_add 'LAW_REST'. mc_add 'LAW_RESULT'.
  mc_add 'LAW_RFCERROR'. mc_add 'LAW_S_URES2'. mc_add 'LAW_STAT'.
  mc_add 'LAW_TEXT'. mc_add 'LAW_TIME'. mc_add 'LAW_U_RES'.
  mc_add 'LAW_UNIQUE'. mc_add 'LAW_UPGR'. mc_add 'LAW_URES2'.
  mc_add 'LAW_UTYPES'. mc_add 'LAW2C_PRESTAT'. mc_add 'LAW2C_REASON'.
  mc_add 'LAW2C_REASON_T'. mc_add 'LAW2C_STATUS'.
  mc_add 'LAW2C_STATUS_T'. mc_add 'LAW2D_TUAPP'. mc_add 'LAW2D_TUAPP_T'.
  mc_add 'LAW2D_TUUNT'. mc_add 'LAW2D_TUUNT_T'. mc_add 'LAWDESTCUS'.
  mc_add 'LAWDIVINDX'. mc_add 'LAWINBOX'. mc_add 'LAWINBOXDATA'.
  mc_add 'LAWPL'. mc_add 'LAWPLT'. mc_add 'LC4AIM_CLASSIFY'.
  mc_add 'LCA_PROREC'. mc_add 'LCA_PROSES'. mc_add 'LCA_WARNINGS'.
  mc_add 'LCCONNMODEBUFFER'. mc_add 'LCINIT'. mc_add 'LCINIT_LOG'.
  mc_add 'LCOLOBJ'. mc_add 'LCRT_ASSINST'. mc_add 'LCRT_ASSOC'.
  mc_add 'LCRT_CHANGELOG'. mc_add 'LCRT_CLASS'. mc_add 'LCRT_CLASS_LU'.
  mc_add 'LCRT_CLNTCACHE'. mc_add 'LCRT_INDX'. mc_add 'LCRT_INST'.
  mc_add 'LCRT_INST_LU'. mc_add 'LCRT_NAMESP_LU'.
  mc_add 'LCRT_OBJ_REF_LU'. mc_add 'LCRT_QUAL'. mc_add 'LCRT_QUAL_LU'.
  mc_add 'LCRT_ROLE_LU'. mc_add 'LCRT_SERVADDR'. mc_add 'LCRT_STATUS'.
  mc_add 'LCRT_UPDATE'. mc_add 'LCTABLES'. mc_add 'LDAPAPPL'.
  mc_add 'LDAPAPPLT'. mc_add 'LDAPGATEW'. mc_add 'LDAPPROP1'.
  mc_add 'LDAPPROP2'. mc_add 'LDAPPROP3'. mc_add 'LDAPPROP4'.
  mc_add 'LDAPPROP5'. mc_add 'LDAPPROP6'. mc_add 'LDBA'. mc_add 'LDBB'.
  mc_add 'LDBD'. mc_add 'LDBH'. mc_add 'LDBN'. mc_add 'LDBNT'.
  mc_add 'LDBQUAN'. mc_add 'LDBS'. mc_add 'LDBT'. mc_add 'LDERA'.
  mc_add 'LDQ_DATA'. mc_add 'LDQ_PROGRESS'. mc_add 'LDQ_STATE'.
  mc_add 'LDSPT'. mc_add 'LHM_EDIS_REPORTS'. mc_add 'LHM_METHODS'.
  mc_add 'LICENSE_DATA'. mc_add 'LICENSE_HASH'. mc_add 'LISCOMDES'.
  mc_add 'LISCOMDESC'. mc_add 'LISCOMDESP'. mc_add 'LISCOMDESU'.
  mc_add 'LISCONNECM'. mc_add 'LISCONNECR'. mc_add 'LISCONNECT'.
  mc_add 'LISDOMAIN'. mc_add 'LISDOMAINT'. mc_add 'LISERVICES'.
  mc_add 'LISLOGSYS'. mc_add 'LISPARAMS'. mc_add 'LISPROD'.
  mc_add 'LISSERVICE'. mc_add 'LISSYSCOM'. mc_add 'LISSYSTEM'.
  mc_add 'LISSYSTEMT'. mc_add 'LISUCONF'. mc_add 'LMSEMAPHORE'.
  mc_add 'LMSEMAPHORE_LOG'. mc_add 'LMSEMAPHOREALLOW'.
  mc_add 'LOGIC_DEST'. mc_add 'LRM_ADK_SWITCH'.
  mc_add 'LRM_RE_RES_STRUC'. mc_add 'LRM_T_BS_ARC_OBJ'.
  mc_add 'LRM_T_BS_BOR_OBJ'. mc_add 'LRM_T_BS_DES_OBJ'.
  mc_add 'LRM_T_BS_FLD_BVD'. mc_add 'LRM_T_BS_FLD_DVD'.
  mc_add 'LRM_T_BS_HND_SPL'. mc_add 'LRM_T_BS_JOINS'.
  mc_add 'LRM_T_BS_KFD_BVD'. mc_add 'LRM_T_BS_KOW_BVD'.
  mc_add 'LRM_T_BS_STT_BVD'. mc_add 'LRM_T_BS_STT_DVD'.
  mc_add 'LRM_T_BS_TO'. mc_add 'LRM_T_CON'. mc_add 'LRM_T_CON_T'.
  mc_add 'LRM_T_EXIT_CUST'. mc_add 'LRM_T_OC'. mc_add 'LRM_T_OC_MF'.
  mc_add 'LRM_T_OC_MF_T'. mc_add 'LRM_T_OC_PC'. mc_add 'LRM_T_OC_RF'.
  mc_add 'LRM_T_OC_RF_T'. mc_add 'LRM_T_OC_SF'. mc_add 'LRM_T_OC_SF_T'.
  mc_add 'LRM_T_OC_T'. mc_add 'LRM_T_OT'. mc_add 'LRM_T_OT_FLD'.
  mc_add 'LRM_T_OT_FLD_T'. mc_add 'LRM_T_OT_PC'.
  mc_add 'LRM_T_OT_PC_STAT'. mc_add 'LRM_T_OT_SF'.
  mc_add 'LRM_T_OT_STT'. mc_add 'LRM_T_OT_STT_TO'. mc_add 'LRM_T_OT_T'.
  mc_add 'LRM_T_PC'. mc_add 'LRM_T_PC_T'. mc_add 'LRM_T_RE_CON'.
  mc_add 'LRM_T_RM_MODE'. mc_add 'LRM_T_SIM'. mc_add 'LRSE_CLASSES'.
  mc_add 'LSELCONDS'. mc_add 'LST_WDA_DDIC_REP'.
  mc_add 'LST_WDA_OTR_REP'. mc_add 'LTABEXITS'. mc_add 'LTCONV'.
  mc_add 'LTDHTRAW'. mc_add 'LTDHTTMPL'. mc_add 'LTODIR'.
  mc_add 'LTYPESTECH'. mc_add 'LVC_CALM'. mc_add 'LVC_CHECKT'.
  mc_add 'LWRKKEY'. mc_add 'LWRKLID'. mc_add 'LWRKOBJ'.
  mc_add 'LWRKOTR'. mc_add 'LWRKSTAOTR'. mc_add 'LWRKSTASOU'.
  mc_add 'LWRKSTAT'. mc_add 'LWRKTRANS'. mc_add 'LXE_ALOG'.
  mc_add 'LXE_ATTGRT'. mc_add 'LXE_ATTOB'. mc_add 'LXE_ATTOBR'.
  mc_add 'LXE_ATTOBRT'. mc_add 'LXE_ATTOBT'. mc_add 'LXE_ATTR_DEF'.
  mc_add 'LXE_ATTR_PROP'. mc_add 'LXE_AV'. mc_add 'LXE_BTC_H'.
  mc_add 'LXE_BTC_OB'. mc_add 'LXE_CHKF'. mc_add 'LXE_CHKO'.
  mc_add 'LXE_CO_2'. mc_add 'LXE_CO_2H'. mc_add 'LXE_CO_GR'.
  mc_add 'LXE_CO_LK'. mc_add 'LXE_COL71'. mc_add 'LXE_COLCO'.
  mc_add 'LXE_COLCP_A'. mc_add 'LXE_COLL'. mc_add 'LXE_COLLFC'.
  mc_add 'LXE_COLLT'. mc_add 'LXE_COLLTL'. mc_add 'LXE_COLLX'.
  mc_add 'LXE_COLMC'. mc_add 'LXE_COLML'. mc_add 'LXE_COLML_A'.
  mc_add 'LXE_COLMT'. mc_add 'LXE_COLOB'. mc_add 'LXE_COLOB_AGG'.
  mc_add 'LXE_COLOB_ATTR'. mc_add 'LXE_COLOB_EX'. mc_add 'LXE_COLOB2'.
  mc_add 'LXE_COLTA'. mc_add 'LXE_COLTA_A'. mc_add 'LXE_COLTB'.
  mc_add 'LXE_COLTY'. mc_add 'LXE_CONT1'. mc_add 'LXE_COPY'.
  mc_add 'LXE_COPYL'. mc_add 'LXE_COPYL2'. mc_add 'LXE_COPYO'.
  mc_add 'LXE_CSN'. mc_add 'LXE_CSN_BK'. mc_add 'LXE_CSN_BYD_FP'.
  mc_add 'LXE_CSN_HD'. mc_add 'LXE_CSN_HD2'. mc_add 'LXE_CSN_LOG'.
  mc_add 'LXE_CSN_PRIO'. mc_add 'LXE_CSN_PROD_LD'. mc_add 'LXE_CSN_TXL'.
  mc_add 'LXE_CSN_TXS'. mc_add 'LXE_CSN_UPDATE'. mc_add 'LXE_CUST'.
  mc_add 'LXE_CUSTFC'. mc_add 'LXE_CUSTMNR'. mc_add 'LXE_CUSTNAME'.
  mc_add 'LXE_CUSTNUMMER'. mc_add 'LXE_DEMO'. mc_add 'LXE_DEMO_FLAG'.
  mc_add 'LXE_DEMOT'. mc_add 'LXE_DIS_O'. mc_add 'LXE_DIST_O'.
  mc_add 'LXE_DOMA'. mc_add 'LXE_DOMA_2'. mc_add 'LXE_DOMA_P'.
  mc_add 'LXE_DOMAIN_EX'. mc_add 'LXE_DOMAT'. mc_add 'LXE_DURA'.
  mc_add 'LXE_ETCONF'. mc_add 'LXE_FIPR'. mc_add 'LXE_GENFL'.
  mc_add 'LXE_GRAPH'. mc_add 'LXE_GRAPH2'. mc_add 'LXE_IDX_LOG'.
  mc_add 'LXE_IDX_LOG_HEAD'. mc_add 'LXE_IDXSTA'. mc_add 'LXE_INEWS'.
  mc_add 'LXE_INEWST'. mc_add 'LXE_KEY_VALUE'.
  mc_add 'LXE_KTP_CHOL_ERR'. mc_add 'LXE_KTP_COLL'.
  mc_add 'LXE_KTP_GUID_MAP'. mc_add 'LXE_KTP_REL_DEP'.
  mc_add 'LXE_KW_TRANSL'. mc_add 'LXE_KWT_COMP_LOG'.
  mc_add 'LXE_KWT_COPY_LOG'. mc_add 'LXE_KWT_TTX_REP'.
  mc_add 'LXE_LA_CL'. mc_add 'LXE_LA_DR'. mc_add 'LXE_LA_MS'.
  mc_add 'LXE_LA_OB'. mc_add 'LXE_LA_OB_X'. mc_add 'LXE_LANG'.
  mc_add 'LXE_LANG_PRESETS'. mc_add 'LXE_LANGTR1'. mc_add 'LXE_LANGTR2'.
  mc_add 'LXE_LEXPT'. mc_add 'LXE_LGRU'. mc_add 'LXE_LIST'.
  mc_add 'LXE_LIST_D'. mc_add 'LXE_LIST_SR'. mc_add 'LXE_LIST_SR2'.
  mc_add 'LXE_LIST_SX'. mc_add 'LXE_LIST_SX2'. mc_add 'LXE_LIST2'.
  mc_add 'LXE_LOG'. mc_add 'LXE_LOG_DATA_TRP'. mc_add 'LXE_LOG_TC'.
  mc_add 'LXE_LOIO'. mc_add 'LXE_LOIOT'. mc_add 'LXE_LOPR'.
  mc_add 'LXE_LORE'. mc_add 'LXE_LORI'. mc_add 'LXE_LTYPES'.
  mc_add 'LXE_LZUS'. mc_add 'LXE_MAIL'. mc_add 'LXE_MT_SUPPORT'.
  mc_add 'LXE_NONABAP_ATTR'. mc_add 'LXE_NUMBER'.
  mc_add 'LXE_OBJ_EXP_HEAD'. mc_add 'LXE_OBJ_EXP_LANG'.
  mc_add 'LXE_OBJ_EXP_LOG'. mc_add 'LXE_OBJ_EXP_OBJ'. mc_add 'LXE_OBJL'.
  mc_add 'LXE_PARAM_BDL'. mc_add 'LXE_PARAM_J'. mc_add 'LXE_PARAM_KTP'.
  mc_add 'LXE_PARAM_NW'. mc_add 'LXE_PARAM_P4'. mc_add 'LXE_PARAM_S2X'.
  mc_add 'LXE_PARAM_XI'. mc_add 'LXE_PARAM_ZTYPE'. mc_add 'LXE_PC_AD'.
  mc_add 'LXE_PC_AP'. mc_add 'LXE_PC_FIP'. mc_add 'LXE_PC_FIP_ONE'.
  mc_add 'LXE_PC_HD'. mc_add 'LXE_PC_INI'. mc_add 'LXE_PC_TTX'.
  mc_add 'LXE_PC_TTX_A'. mc_add 'LXE_PC_TTX_B'. mc_add 'LXE_PC_TTX_C'.
  mc_add 'LXE_PC_TTX_D'. mc_add 'LXE_PC_TTX_E'. mc_add 'LXE_PC_TTX_F'.
  mc_add 'LXE_PC_TTX_G'. mc_add 'LXE_PC_TTX_H'. mc_add 'LXE_PC_TTX_I'.
  mc_add 'LXE_PC_TTX_L'. mc_add 'LXE_PC_TTX_M'. mc_add 'LXE_PC_TTX_N'.
  mc_add 'LXE_PC_TTX_O'. mc_add 'LXE_PC_TTX_P'. mc_add 'LXE_PC_TTX_Q'.
  mc_add 'LXE_PC_TTX_R'. mc_add 'LXE_PC_TTX_S'. mc_add 'LXE_PC_TTX_T'.
  mc_add 'LXE_PC_TTX_U'. mc_add 'LXE_PC_TTX_V'. mc_add 'LXE_PC_TTX_W'.
  mc_add 'LXE_PC_TTX_X'. mc_add 'LXE_PC_TTX_Y'. mc_add 'LXE_PC_TTX_Z'.
  mc_add 'LXE_PC_TTXH'. mc_add 'LXE_PC2_AP'. mc_add 'LXE_PC2_HD'.
  mc_add 'LXE_PC2_HS'. mc_add 'LXE_PC2_KEY'. mc_add 'LXE_PC2_XI_HD'.
  mc_add 'LXE_PCT_TY'. mc_add 'LXE_PCX_HD'. mc_add 'LXE_PCX_HD_A'.
  mc_add 'LXE_PCX_HD_B'. mc_add 'LXE_PCX_HD_C'. mc_add 'LXE_PCX_HD_D'.
  mc_add 'LXE_PCX_HD_F'. mc_add 'LXE_PCX_HD_G'. mc_add 'LXE_PCX_HD_H'.
  mc_add 'LXE_PCX_HD_L'. mc_add 'LXE_PCX_HD_M'. mc_add 'LXE_PCX_HD_R'.
  mc_add 'LXE_PCX_HD_S'. mc_add 'LXE_PCX_HD_T'. mc_add 'LXE_PCX_HD_U'.
  mc_add 'LXE_PCX_TX'. mc_add 'LXE_PCX_TX_A'. mc_add 'LXE_PCX_TX_B'.
  mc_add 'LXE_PCX_TX_C'. mc_add 'LXE_PCX_TX_D'. mc_add 'LXE_PCX_TX_F'.
  mc_add 'LXE_PCX_TX_G'. mc_add 'LXE_PCX_TX_H'. mc_add 'LXE_PCX_TX_L'.
  mc_add 'LXE_PCX_TX_M'. mc_add 'LXE_PCX_TX_R'. mc_add 'LXE_PCX_TX_S'.
  mc_add 'LXE_PCX_TX_T'. mc_add 'LXE_PCX_TX_U'. mc_add 'LXE_PCY_HD'.
  mc_add 'LXE_PCY_HD_A'. mc_add 'LXE_PCY_HD_B'. mc_add 'LXE_PCY_HD_D'.
  mc_add 'LXE_PCY_HD_F'. mc_add 'LXE_PCY_HD_G'. mc_add 'LXE_PCY_HD_I'.
  mc_add 'LXE_PCY_HD_L'. mc_add 'LXE_PCY_HD_M'. mc_add 'LXE_PCY_HD_T'.
  mc_add 'LXE_PCY_TX'. mc_add 'LXE_PCY_TX_A'. mc_add 'LXE_PCY_TX_B'.
  mc_add 'LXE_PCY_TX_D'. mc_add 'LXE_PCY_TX_F'. mc_add 'LXE_PCY_TX_G'.
  mc_add 'LXE_PCY_TX_I'. mc_add 'LXE_PCY_TX_L'. mc_add 'LXE_PCY_TX_M'.
  mc_add 'LXE_PCY_TX_T'. mc_add 'LXE_PCY_VN'. mc_add 'LXE_PCY_VN_A'.
  mc_add 'LXE_PCY_VN_B'. mc_add 'LXE_PCY_VN_D'. mc_add 'LXE_PCY_VN_F'.
  mc_add 'LXE_PCY_VN_G'. mc_add 'LXE_PCY_VN_I'. mc_add 'LXE_PCY_VN_L'.
  mc_add 'LXE_PCY_VN_M'. mc_add 'LXE_PCY_VN_T'. mc_add 'LXE_PCZ_BI'.
  mc_add 'LXE_PCZ_BI_A'. mc_add 'LXE_PCZ_BI_F'. mc_add 'LXE_PCZ_BI_H'.
  mc_add 'LXE_PCZ_BI_I'. mc_add 'LXE_PCZ_BI_S'. mc_add 'LXE_PCZ_BI_Z'.
  mc_add 'LXE_PCZ_HD'. mc_add 'LXE_PCZ_HD_A'. mc_add 'LXE_PCZ_HD_F'.
  mc_add 'LXE_PCZ_HD_H'. mc_add 'LXE_PCZ_HD_I'. mc_add 'LXE_PCZ_HD_S'.
  mc_add 'LXE_PCZ_HD_Z'. mc_add 'LXE_PCZ_TT'. mc_add 'LXE_PCZ_TT_A'.
  mc_add 'LXE_PCZ_TT_F'. mc_add 'LXE_PCZ_TT_H'. mc_add 'LXE_PCZ_TT_I'.
  mc_add 'LXE_PCZ_TT_S'. mc_add 'LXE_PCZ_TT_Z'. mc_add 'LXE_PCZ_TX'.
  mc_add 'LXE_PCZ_TX_A'. mc_add 'LXE_PCZ_TX_F'. mc_add 'LXE_PCZ_TX_H'.
  mc_add 'LXE_PCZ_TX_I'. mc_add 'LXE_PCZ_TX_S'. mc_add 'LXE_PCZ_TX_Z'.
  mc_add 'LXE_PCZ_VN'. mc_add 'LXE_PCZ_VN_A'. mc_add 'LXE_PCZ_VN_F'.
  mc_add 'LXE_PCZ_VN_H'. mc_add 'LXE_PCZ_VN_I'. mc_add 'LXE_PCZ_VN_S'.
  mc_add 'LXE_PCZ_VN_Z'. mc_add 'LXE_PERF_LOG'.
  mc_add 'LXE_PERF_LOG_AGG'. mc_add 'LXE_PEXPT'. mc_add 'LXE_PHF'.
  mc_add 'LXE_PHHR'. mc_add 'LXE_PHIO'. mc_add 'LXE_PHNM'.
  mc_add 'LXE_PHPR'. mc_add 'LXE_PHRE'. mc_add 'LXE_PHRI'.
  mc_add 'LXE_POPO'. mc_add 'LXE_PP'. mc_add 'LXE_PP___1'.
  mc_add 'LXE_PP___2'. mc_add 'LXE_PP___3'. mc_add 'LXE_PP___4'.
  mc_add 'LXE_PP_ABB'. mc_add 'LXE_PP_IMP'. mc_add 'LXE_PP_IMPL'.
  mc_add 'LXE_PP_MAP'. mc_add 'LXE_PP_TEXT'. mc_add 'LXE_PPA__1'.
  mc_add 'LXE_PPAT_ANLYS'. mc_add 'LXE_PPAT_JOB'. mc_add 'LXE_PPAT_RUN'.
  mc_add 'LXE_PPX__1'. mc_add 'LXE_PROJ_DATA'. mc_add 'LXE_PROJ_HEAD'.
  mc_add 'LXE_REMRKS'. mc_add 'LXE_REMRKS_LTE'. mc_add 'LXE_REMRKS_STE'.
  mc_add 'LXE_REVIEW_FLAG'. mc_add 'LXE_RLTP_LOG'.
  mc_add 'LXE_SETTINGS_SC'. mc_add 'LXE_SLLS_OWT'.
  mc_add 'LXE_SLLS_PREFS'. mc_add 'LXE_SW_LOCK'. mc_add 'LXE_SYST'.
  mc_add 'LXE_T002'. mc_add 'LXE_T002T'. mc_add 'LXE_T002X'.
  mc_add 'LXE_TAB_TEST'. mc_add 'LXE_TAMO_ABORT'.
  mc_add 'LXE_TAMO_ERROR'. mc_add 'LXE_TAREA_LOCK'. mc_add 'LXE_TAXX'.
  mc_add 'LXE_TC_SW_COMP'. mc_add 'LXE_TCS_TTX_REG'.
  mc_add 'LXE_TD_ALIGN'. mc_add 'LXE_TD_EXCLUDES'.
  mc_add 'LXE_TD_FILEEXT'. mc_add 'LXE_TD_INCLUDES'.
  mc_add 'LXE_TD_REMO_CFG'. mc_add 'LXE_TD_SWITCH'.
  mc_add 'LXE_TDC_GWHOST'. mc_add 'LXE_TDC_VERSION'. mc_add 'LXE_TDEVC'.
  mc_add 'LXE_TEAM'. mc_add 'LXE_TECH'. mc_add 'LXE_TERMO_DATA'.
  mc_add 'LXE_TGROUP'. mc_add 'LXE_THREAD'. mc_add 'LXE_THREADX'.
  mc_add 'LXE_TM'. mc_add 'LXE_TM5'. mc_add 'LXE_TM5_MT'.
  mc_add 'LXE_TM9'. mc_add 'LXE_TMP_ST'. mc_add 'LXE_TMWARE_ALIAS'.
  mc_add 'LXE_TMWARE_CFG'. mc_add 'LXE_TMWARE_LCID'.
  mc_add 'LXE_TRDELTA'. mc_add 'LXE_TRLANG'. mc_add 'LXE_TRP_OBJ_X'.
  mc_add 'LXE_TSCON_LOOKUP'. mc_add 'LXE_TTX'. mc_add 'LXE_UGROUP'.
  mc_add 'LXE_UPSETS'. mc_add 'LXE_US_CO'. mc_add 'LXE_US_COL'.
  mc_add 'LXE_USER'. mc_add 'LXE_USER_PRESETS'. mc_add 'LXE_WHITELIST'.
  mc_add 'LXE_WL'. mc_add 'LXE_WL2'. mc_add 'LXE_WRK_1'.
  mc_add 'LXE_WRK_1B'. mc_add 'LXE_WRK_2'. mc_add 'LXE_WRK_2X'.
  mc_add 'LXE_WRK_3'. mc_add 'LXE_WRK_3X'. mc_add 'LXE_WRK_4'.
  mc_add 'LXE_WRK_4X'. mc_add 'LXE_WRK_APD'. mc_add 'LXE_WRK_ARC'.
  mc_add 'LXE_WRK_GR'. mc_add 'LXE_WRK_LA'. mc_add 'LXE_WRK_LOG'.
  mc_add 'LXE_WRK_LOG_DBG'. mc_add 'LXE_WRK_LOG_RLT'.
  mc_add 'LXE_WRK_OL'. mc_add 'LXE_WRK_TY'. mc_add 'LXE_WRKKEY'.
  mc_add 'LXE_WRKLID'. mc_add 'LXE_WRKOB'. mc_add 'LXE_WRKOB1'.
  mc_add 'LXE_WRKOBD'. mc_add 'LXE_WRKOBK'. mc_add 'LXE_WRKOBO'.
  mc_add 'LXE_WRKOBS'. mc_add 'LXE_WRKOBX'. mc_add 'LXE_WRKOBY'.
  mc_add 'LXE_WRKOBZ'. mc_add 'LXE_WSPROXY'. mc_add 'LXE_XT_ATTRIBUTE'.
  mc_add 'LXE_XT_HEAD_LANG'. mc_add 'LXE_XT_HEADER'.
  mc_add 'LXE_XT_OBJ_ATTR'. mc_add 'LXE_XT_OBJ_MAP'.
  mc_add 'LXE_XT_TEXTTYPE'. mc_add 'LXE_XT_TKEY_MAP'.
  mc_add 'LXE_XT_TXT_ATTR'. mc_add 'LXE_XT_TXTTYPE_T'.
  mc_add 'LXE_XT_XTEXT'. mc_add 'LXE_XT_YTEXT'. mc_add 'LXE_XT_ZBIN'.
  mc_add 'LXE_XT_ZHEAD'. mc_add 'LXE_XT_ZTEXT'.
  mc_add 'LXE_XTRANSSWITCH'. mc_add 'LXEAUTH'. mc_add 'LXEAUTHT'.
  mc_add 'LXELOGS'. mc_add 'LXEPROF'. mc_add 'LXEPROFV'.
  mc_add 'LXESEQUENCE'. mc_add 'LXETXT0005'. mc_add 'LXETXT0010'.
  mc_add 'LXETXT0015'. mc_add 'LXETXT0020'. mc_add 'LXETXT0030'.
  mc_add 'LXETXT0040'. mc_add 'LXETXT0050'. mc_add 'LXETXT0060'.
  mc_add 'LXETXT0080'. mc_add 'LXETXT0255'. mc_add 'LXETXTCNT1'.
  mc_add 'MACID'. mc_add 'MACOB'. mc_add 'MAPPARAREF'. mc_add 'MCDEFID'.
  mc_add 'MCOBJECTS'. mc_add 'MCSTATLIST'. mc_add 'MCUSREXITS'.
  mc_add 'MDOBLACLA'. mc_add 'MDOBLAOBJ'. mc_add 'MDOBLPROP'.
  mc_add 'MDOBLREL'. mc_add 'MDOBLRO'. mc_add 'MDOBLROL'.
  mc_add 'MECLIENTCOMP'. mc_add 'MEM_ALLOC'. mc_add 'MEMGMT_COND_TYPE'.
  mc_add 'MEMGMT_MAP_TYPES'. mc_add 'MEMGMT_MNTR_FM'.
  mc_add 'MEMGMT_OBJ_TYPE'. mc_add 'MEMGMT_PARA_COND'.
  mc_add 'MEMGMT_PARA_PATT'. mc_add 'MEMGMT_PARA_PROP'.
  mc_add 'MEMGMT_PARA_TEXT'. mc_add 'MEMGMT_PARA_VAL'.
  mc_add 'MEMGMT_SERVICE'. mc_add 'MEMGMT_SERVICET'.
  mc_add 'MEMGMT_TYPE_VAL'. mc_add 'MEMSD'. mc_add 'MEMSD_APPLTYPE'.
  mc_add 'MEMSD_CUSTATT'. mc_add 'MEMSD_DEP'. mc_add 'MEMSD_DEPTYPE'.
  mc_add 'MEMSD_PROP'. mc_add 'MEMSD_PROPTYPE'.
  mc_add 'MEMSD_RUNTIMTYPE'. mc_add 'MEMSD_TEXT'. mc_add 'MEN_EXITS'.
  mc_add 'MENTREE1'. mc_add 'MENTREE1T'. mc_add 'MENU_RELE'.
  mc_add 'MEPLATFORM_TEXT'. mc_add 'MEREP_401'. mc_add 'MEREP_401T'.
  mc_add 'MEREP_405'. mc_add 'MEREP_406'. mc_add 'MEREP_406T'.
  mc_add 'MEREP_408'. mc_add 'MEREP_409'. mc_add 'MEREP_410'.
  mc_add 'MEREP_411'. mc_add 'MEREP_414'. mc_add 'MEREP_415'.
  mc_add 'MEREP_416'. mc_add 'MEREP_801'. mc_add 'MEREP_801T'.
  mc_add 'MEREP_802'. mc_add 'MEREP_802T'. mc_add 'MEREP_803'.
  mc_add 'MEREP_804'. mc_add 'MEREP_804T'. mc_add 'MEREP_805'.
  mc_add 'MEREP_806'. mc_add 'MEREP_806T'. mc_add 'MEREP_807'.
  mc_add 'MEREP_808'. mc_add 'MEREP_808T'. mc_add 'MEREP_809'.
  mc_add 'MEREP_810'. mc_add 'MEREP_811'. mc_add 'MEREP_816'.
  mc_add 'MEREP_817'. mc_add 'MEREP_817T'. mc_add 'MEREP_888'.
  mc_add 'MEREP_CONFIG'. mc_add 'MEREP_DSD001'. mc_add 'MEREP_DSD002'.
  mc_add 'MEREP_DSD003'. mc_add 'MEREP_DSD004'. mc_add 'MEREP_DSD005'.
  mc_add 'MEREP_DSD006'. mc_add 'MEREP_DSD007'. mc_add 'MEREP_ENQ'.
  mc_add 'MEREP_PARAM_CONF'. mc_add 'MERGE_CONF_ATTR'.
  mc_add 'MERGE_CONF_EXCL'. mc_add 'MERGE_CONF_HEAD'.
  mc_add 'MERGE_CONF_INCL'. mc_add 'MERGE_CONF_OPTS'.
  mc_add 'MERGE_CONF_PARTS'. mc_add 'MERGE_CONF_REST'.
  mc_add 'MERGE_CUST_PARTS'. mc_add 'MERGE_CUST_REL'.
  mc_add 'MERGECLIL'. mc_add 'MERGECLIL_CHK'. mc_add 'MERGEDIR'.
  mc_add 'MERGEH'. mc_add 'MERGEMAPS'. mc_add 'MERGENEGL'.
  mc_add 'MERGENEGLN'. mc_add 'MERGENEGLX'. mc_add 'MERGEOBJ'.
  mc_add 'MERGEPREPC'. mc_add 'MERGETA'. mc_add 'MERGEUSERS'.
  mc_add 'METHODTR'. mc_add 'METRACE_SYNC'. mc_add 'METRACESET'.
  mc_add 'MIGRDDL'. mc_add 'MIGREXTAB'. mc_add 'MIGTRIGGER'.
  mc_add 'MIME_DSNC'. mc_add 'MIME_FROLES'. mc_add 'MIME_FROLEST'.
  mc_add 'MIME_LOG'. mc_add 'MIMETYPES'. mc_add 'MKMTESTACT'.
  mc_add 'MLTTSTLOGP'. mc_add 'MLTTSTTEM1'. mc_add 'MLTTSTTEM2'.
  mc_add 'MLTTSTTEM3'. mc_add 'MLTTSTWRK1'. mc_add 'MLTTSTWRK2'.
  mc_add 'MLTTSTWRK3'. mc_add 'MOB_DEVCAP'. mc_add 'MOB_DEVCFG'.
  mc_add 'MOB_PIC_NOTES'. mc_add 'MOB_PICTURE'. mc_add 'MODACT'.
  mc_add 'MODATTR'. mc_add 'MODSAP'. mc_add 'MODSAPA'. mc_add 'MODSAPT'.
  mc_add 'MODTEXT'. mc_add 'MODWRD'. mc_add 'MONI'. mc_add 'MQDES'.
  mc_add 'MSQSNAP'. mc_add 'MSSCNTDEAD'. mc_add 'MSSDBSTATT'.
  mc_add 'MSSDEADLCK'. mc_add 'MSSDWDLLS'. mc_add 'MSSPROCS'.
  mc_add 'MSSSOURCE'. mc_add 'MSSSPVERS'. mc_add 'MSSSTORAGE'.
  mc_add 'MTOBLREL'. mc_add 'MTOBLROL'. mc_add 'NAVERS_EXT'.
  mc_add 'NAVERS2'. mc_add 'NCVERS2'. mc_add 'NKPARMS'. mc_add 'NOIND'.
  mc_add 'NOTRA'. mc_add 'NPRDVERS'. mc_add 'NPUT'. mc_add 'NR3TEXT'.
  mc_add 'NR3XTEXT'. mc_add 'NSWFEATINC'. mc_add 'NSWFEATURE'.
  mc_add 'NTECHUSAGE'. mc_add 'NTESTTTTTT'. mc_add 'NULLCOLS'.
  mc_add 'NWBC_CFG_GUI_TAG'. mc_add 'NWBC_CFG_SAP'.
  mc_add 'NWBC_CONFIG'. mc_add 'NWBC_S_GUI_TAG_T'.
  mc_add 'NWECM_APPCON_DEF'. mc_add 'NWECM_APPREP_DEF'.
  mc_add 'NWECM_D_ACL_CD'. mc_add 'NWECM_T_SAPPSTAT'.
  mc_add 'NWECMD_SESSION'. mc_add 'NWECMS_RPSTRS'. mc_add 'O2APPL'.
  mc_add 'O2APPLT'. mc_add 'O2BROWSERS'. mc_add 'O2HTMLATTR'.
  mc_add 'O2HTMLDOC'. mc_add 'O2HTMLTAG'. mc_add 'O2LANGTYPE'.
  mc_add 'O2NAVGRAPH'. mc_add 'O2NAVMAP'. mc_add 'O2PAGCON'.
  mc_add 'O2PAGDIR'. mc_add 'O2PAGDIRI'. mc_add 'O2PAGDIRT'.
  mc_add 'O2PAGEVH'. mc_add 'O2PAGPAR'. mc_add 'O2PAGPART'.
  mc_add 'O2PAGTLIB'. mc_add 'O2TAG'. mc_add 'O2TAGATT'.
  mc_add 'O2TAGLIB'. mc_add 'O2TAGOPTS'. mc_add 'O2TAGREG'.
  mc_add 'O2THEMEREL'. mc_add 'O2THEMES'. mc_add 'O2THEMEST'.
  mc_add 'O2XMLDESC'. mc_add 'O2XSLTDESC'. mc_add 'O2XSLTOTR'.
  mc_add 'O2XSLTTEXT'. mc_add 'OA2_SD_SC'. mc_add 'OA2_SD_SCT'.
  mc_add 'OA2C_PROFILES'. mc_add 'OA2C_STATE_AC'. mc_add 'OA2C_TYPES'.
  mc_add 'OA2P_SCOPES'. mc_add 'OAPRI'. mc_add 'OBJ_COMP'.
  mc_add 'OBJH'. mc_add 'OBJLAN'. mc_add 'OBJM'. mc_add 'OBJM_EXT'.
  mc_add 'OBJM_REF'. mc_add 'OBJM_SREL'. mc_add 'OBJMAPI'.
  mc_add 'OBJMAUTH'. mc_add 'OBJMT'. mc_add 'OBJMTTX'. mc_add 'OBJS'.
  mc_add 'OBJSL'. mc_add 'OBJSUB'. mc_add 'OBJSUBRESN'.
  mc_add 'OBJSUBT'. mc_add 'OBJT'. mc_add 'OBJTYPEGROUPS'.
  mc_add 'OBJTYPEGROUPS_T'. mc_add 'OCS_ALLTABS_INFO'.
  mc_add 'OCS_CP_E070'. mc_add 'OCS_CP_E070A'. mc_add 'OCS_CP_E070C'.
  mc_add 'OCS_CP_E071'. mc_add 'OCS_CP_E071K'.
  mc_add 'OCS_CP_E071K_STR'. mc_add 'OCS_CP_E071KF'.
  mc_add 'OCS_CP_E07T'. mc_add 'OCS_CP_PAT_BLOB'. mc_add 'OCS_CP_PAT03'.
  mc_add 'OCS_CP_PAT07'. mc_add 'OCS_CP_PAT08'. mc_add 'OCS_HTML'.
  mc_add 'OCS_TESTFREE_INF'. mc_add 'OCSCCOMPAT'. mc_add 'OCSCMPLOBJ'.
  mc_add 'OCSPATNTCI'. mc_add 'OCSSORTT'. mc_add 'OCSSPCOMPR'.
  mc_add 'ODQRESP'. mc_add 'ODQTASKDATA'. mc_add 'ODQTASKMERGE'.
  mc_add 'ODQTASKQUEUE'. mc_add 'ODQTQLISTENER'. mc_add 'ODQTSN'.
  mc_add 'OJDEF'. mc_add 'OJDEFL'. mc_add 'OJDEFS'. mc_add 'OLC_CHECK'.
  mc_add 'OLC_CHECK_T'. mc_add 'OLC_PROFILE'. mc_add 'OLC_PROFILE_T'.
  mc_add 'OLELOAD'. mc_add 'OMD_SBOOK_TMPKEY'. mc_add 'OMT100_LINK_MAP'.
  mc_add 'OMT100_LINK_MAPT'. mc_add 'ONSTATOPT'.
  mc_add 'OPMODE_EXT_TOOL'. mc_add 'OPSYSTEM'. mc_add 'OPTEXT'.
  mc_add 'ORA_ASHGSET'. mc_add 'ORA_ASHGSETID'.
  mc_add 'ORA_DBA_EXTENTS'. mc_add 'ORA_DBA_OBJECTS'.
  mc_add 'ORA_DBA_SEGMENTS'. mc_add 'ORA_DBA_TABLES'.
  mc_add 'ORA_DBA_TCOL'. mc_add 'ORA_DBA_TCOLUMNS'.
  mc_add 'ORA_DBH_CONFIG'. mc_add 'ORA_FEAT_USED'. mc_add 'ORA_HISTPAR'.
  mc_add 'ORA_IDLE_EVENTS'. mc_add 'ORA_IND_QLTY'.
  mc_add 'ORA_ISQ_DETAIL'. mc_add 'ORA_ISQ_HEADER'.
  mc_add 'ORA_ISQ_HISTORY'. mc_add 'ORA_ISQ_PARAM'.
  mc_add 'ORA_JSEL_STMT'. mc_add 'ORA_JSEL_STMTID'.
  mc_add 'ORA_MON_DBCON'. mc_add 'ORA_RESUMABLE'.
  mc_add 'ORA_SAP_AUXSTATS'. mc_add 'ORA_SAPKCBFWAIT'.
  mc_add 'ORA_SNAPSHOT'. mc_add 'ORA_SQLC_DATA'. mc_add 'ORA_SQLC_HEAD'.
  mc_add 'ORA_SQLC_STMT'. mc_add 'ORA_SQLCN_STMT'.
  mc_add 'ORA_SQLCN_STMTID'. mc_add 'ORA_STT_DATA'.
  mc_add 'ORA_STT_HEAD'. mc_add 'ORA_TABLESPACES'. mc_add 'ORAINDINFO'.
  mc_add 'ORAISQCTRL'. mc_add 'ORAPARTADMDAT'. mc_add 'ORAPARTADMIN'.
  mc_add 'ORAPARTAL'. mc_add 'ORAPARTGENDDL'. mc_add 'ORATSPINFO'.
  mc_add 'ORBRELTPA'. mc_add 'ORBRELTYP'. mc_add 'ORGCRATT'.
  mc_add 'ORGCRATT_T'. mc_add 'ORGCRFLD'. mc_add 'ORGCRIT'.
  mc_add 'ORGCRIT_T'. mc_add 'OROBJROLES'. mc_add 'ORROLETYP'.
  mc_add 'ORTBRELTPA'. mc_add 'ORTBRELTYP'. mc_add 'ORTROLETYP'.
  mc_add 'OSEXNODE01'. mc_add 'OSEXTREE01'. mc_add 'OSEXXMLNODE'.
  mc_add 'OSMON'. mc_add 'OTR_BVOC'. mc_add 'OTR_COMM'.
  mc_add 'OTR_VOTE'. mc_add 'PACKCHECK_EXCEPT'. mc_add 'PAD03'.
  mc_add 'PAD71'. mc_add 'PAD71K'. mc_add 'PAHI'.
  mc_add 'PAHI_PARAM_SEL'. mc_add 'PAK_INTERVALS'. mc_add 'PAKCHEKTYP'.
  mc_add 'PAKDATBUF'. mc_add 'PAKEFFSEVR'. mc_add 'PAKPARAM'.
  mc_add 'PAKPARAM_LOG'. mc_add 'PARAMVALUE'. mc_add 'PAT00'.
  mc_add 'PAT01'. mc_add 'PAT01_ERR'. mc_add 'PAT02'. mc_add 'PAT03'.
  mc_add 'PAT03_AIMREP'. mc_add 'PAT03_PRE'. mc_add 'PAT04'.
  mc_add 'PAT05'. mc_add 'PAT06'. mc_add 'PAT07'. mc_add 'PAT07_ACP'.
  mc_add 'PAT08'. mc_add 'PAT08_ACP'. mc_add 'PAT09'. mc_add 'PAT10'.
  mc_add 'PAT10B'. mc_add 'PAT11'. mc_add 'PAT13'. mc_add 'PAT13_SDA'.
  mc_add 'PATACP'. mc_add 'PATALLOWED'. mc_add 'PATBACKUPDATA'.
  mc_add 'PATBACKUPHDR'. mc_add 'PATCHECK'. mc_add 'PATCHHIST'.
  mc_add 'PATCONFL'. mc_add 'PATCONT'. mc_add 'PATEXCPT'. mc_add 'PATH'.
  mc_add 'PATHISTORY'. mc_add 'PATHISTQ'. mc_add 'PATHTEXT'.
  mc_add 'PATLOG'. mc_add 'PATLOG3'. mc_add 'PATPRDVRS'.
  mc_add 'PATRTCONF'. mc_add 'PATRTPHASE'. mc_add 'PATRTVERS'.
  mc_add 'PATRTVERS2'. mc_add 'PATSTOP'. mc_add 'PATSTOPS'.
  mc_add 'PATSWFEATR'. mc_add 'PATSWFTINC'. mc_add 'PATTPCALL'.
  mc_add 'PATTPIMPLOG'. mc_add 'PATTPLOGPTR'. mc_add 'PATTPOPTIONS'.
  mc_add 'PATTPSTDOUT'. mc_add 'PATUSEDOBJ'. mc_add 'PBELEMTYPS'.
  mc_add 'PBELEMTYPT'. mc_add 'PBLAYOUT'. mc_add 'PCHKPFHEAD'.
  mc_add 'PCHKPROF'. mc_add 'PCHKPROFT'. mc_add 'PERMISSION'.
  mc_add 'PERS_CLCP'. mc_add 'PERS_DEF'. mc_add 'PERS_FUNC'.
  mc_add 'PERS_OFF'. mc_add 'PERS_TXT'. mc_add 'PFL_INDX'.
  mc_add 'PHTMLB_FLI'. mc_add 'PHTMLB_FLI_TEXT'. mc_add 'POWL_EASY_ACT'.
  mc_add 'POWL_EASY_DEF'. mc_add 'POWL_EASY_DEFT'.
  mc_add 'POWL_EASY_HDR'. mc_add 'POWL_TYPE'. mc_add 'POWL_TYPE_T'.
  mc_add 'PPFTFLTC1T'. mc_add 'PPFTFLTC2T'. mc_add 'PPFTFLTCO1'.
  mc_add 'PPFTFLTCO2'. mc_add 'PPFTFLTVAL'. mc_add 'PPFTFLTVAT'.
  mc_add 'PRD_ASSIGN'. mc_add 'PRD_SUBSTR'. mc_add 'PRDVERS'.
  mc_add 'PRGN_CORR'. mc_add 'PRGN_CORR2'. mc_add 'PRGN_CORR3'.
  mc_add 'PRGN_CUST'. mc_add 'PRGN_STAT'. mc_add 'PROD_D_SUB'.
  mc_add 'PROD_DETLD'. mc_add 'PROG_TCD'. mc_add 'PROP_ELTYP'.
  mc_add 'PRV_LOG_CP'. mc_add 'PUMK'. mc_add 'PUSHTPL'.
  mc_add 'PUTTB_SHD_1'. mc_add 'PUTTB_SHD_2'. mc_add 'PUTTBC'.
  mc_add 'PUTTBC_S4T'. mc_add 'PUTTBC_ZDM'. mc_add 'PUTTBCEXCP'.
  mc_add 'PUTTBEXCPT'. mc_add 'PUTTBH'. mc_add 'PUTTBH_ZDM'.
  mc_add 'PUTTBX'. mc_add 'PUTTBX_ZDM'. mc_add 'PWSTATE2'.
  mc_add 'PWTGROUP'. mc_add 'QADB_SCOPE'. mc_add 'QCONFIRM'.
  mc_add 'QMM_CHKF'. mc_add 'QMM_CHKO'. mc_add 'QMM_CONT1'.
  mc_add 'QMM_IDXSTA'. mc_add 'QMM_LOIO'. mc_add 'QMM_LOIOT'.
  mc_add 'QMM_LOPR'. mc_add 'QMM_LORE'. mc_add 'QMM_LORI'.
  mc_add 'QMM_NODE'. mc_add 'QMM_NODER'. mc_add 'QMM_NODET'.
  mc_add 'QMM_PHF'. mc_add 'QMM_PHHR'. mc_add 'QMM_PHHRPR'.
  mc_add 'QMM_PHIO'. mc_add 'QMM_PHNM'. mc_add 'QMM_PHNMPR'.
  mc_add 'QMM_PHPR'. mc_add 'QMM_PHRE'. mc_add 'QMM_PHREPR'.
  mc_add 'QMM_PHRI'. mc_add 'QMM_PHRIPR'. mc_add 'QREFTID'.
  mc_add 'QRETDATA'. mc_add 'QRETSTATE'. mc_add 'QRFC_CFG'.
  mc_add 'QRFC_CUST_I_DEST'. mc_add 'QRFC_I_ERR_STATE'.
  mc_add 'QRFC_I_EXE_STATE'. mc_add 'QRFC_I_QIN'.
  mc_add 'QRFC_I_QIN_LOCK'. mc_add 'QRFC_I_QIN_TOP'.
  mc_add 'QRFC_I_SDATA'. mc_add 'QRFC_I_UNIT'.
  mc_add 'QRFC_I_UNIT_LOCK'. mc_add 'QRFC_I_UNIT_SESS'.
  mc_add 'QRFC_N_EXE_STATE'. mc_add 'QRFC_N_QOUT'.
  mc_add 'QRFC_N_QOUT_TOP'. mc_add 'QRFC_N_REF_UNIT'.
  mc_add 'QRFC_N_SDATA'. mc_add 'QRFC_N_UNIT'.
  mc_add 'QRFC_N_UNIT_LOCK'. mc_add 'QRFC_O_ERR_STATE'.
  mc_add 'QRFC_O_EXE_STATE'. mc_add 'QRFC_O_QIN'. mc_add 'QRFC_O_QOUT'.
  mc_add 'QRFC_O_QOUT_LOCK'. mc_add 'QRFC_O_QOUT_TOP'.
  mc_add 'QRFC_O_REF_UNIT'. mc_add 'QRFC_O_SDATA'. mc_add 'QRFC_O_UNIT'.
  mc_add 'QRFC_O_UNIT_LOCK'. mc_add 'QRFCEVENT'. mc_add 'QRFCLOG'.
  mc_add 'QRFCTRACE'. mc_add 'QUEUESTATE_INDX'. mc_add 'QUICK_MEAL'.
  mc_add 'REORGJOBS'. mc_add 'REPOSRC'. mc_add 'REPOTEXT'.
  mc_add 'RESP'. mc_add 'RESPT'. mc_add 'RFC_BL_SERVER'.
  mc_add 'RFC_BL_SERVERC'. mc_add 'RFC2SOAPS'. mc_add 'RFCATTRIB'.
  mc_add 'RFCCAT'. mc_add 'RFCCBWHITELIST'. mc_add 'RFCCBWHITELIST_A'.
  mc_add 'RFCCHECK'. mc_add 'RFCCMC'. mc_add 'RFCDES'.
  mc_add 'RFCDESSECU'. mc_add 'RFCDOC'. mc_add 'RFCGO'.
  mc_add 'RFCSTXTAB'. mc_add 'RFCSYSACL'. mc_add 'RFCTA'.
  mc_add 'RFCTRUST'. mc_add 'RFCTYPE'. mc_add 'RGDEFDB'.
  mc_add 'RGDETAILS'. mc_add 'RGLISTMEM'. mc_add 'RGPARAM'.
  mc_add 'RIS_DATA_MODEL'. mc_add 'RIS_DM_COMPAT'.
  mc_add 'RIS_DM_LISTS'. mc_add 'RIS_DM_TYPES'.
  mc_add 'RIS_INDEX_MISSED'. mc_add 'RIS_INDEX_UPDATE'.
  mc_add 'RIS_PROG_TADIR'. mc_add 'RIS_SEARCH_QUERY'.
  mc_add 'RIS_UI_MODEL'. mc_add 'RLB_DB_HL'. mc_add 'RLB_DB_LB'.
  mc_add 'RLB_DB_LBT'. mc_add 'RLB_DB_PR'. mc_add 'RLB_DB_PRT'.
  mc_add 'RLB_DB_SC'. mc_add 'RLB_DB_TR'. mc_add 'RLB_DB_TRR'.
  mc_add 'RLB_DB_TRT'. mc_add 'RLB_DB_VST'. mc_add 'RLFW_LOGGING'.
  mc_add 'RLFW_NEXT_SCHED'. mc_add 'RLFW_RL_MAPPING'.
  mc_add 'RLFW_RUNLEVELS'. mc_add 'RLFW_SCHEDULES'.
  mc_add 'RLFW_SERVICES'. mc_add 'RLFW_SYS_RL'. mc_add 'RLFW_TEST_RL'.
  mc_add 'RLIB_COMPS'. mc_add 'RLIB_CONT'. mc_add 'RLIB_DOCU'.
  mc_add 'RLIB_OBJS'. mc_add 'RLIB_STAT'. mc_add 'RLIB_TEXTS'.
  mc_add 'RLIB_TREES'. mc_add 'RNDPGSCANRESULT'. mc_add 'ROAPPL'.
  mc_add 'ROAPPLT'. mc_add 'RODCHA'. mc_add 'RODCHABAS'.
  mc_add 'RODCHABGEN'. mc_add 'RODIOBJ'. mc_add 'RODIOBJCMP'.
  mc_add 'RODIR'. mc_add 'RODKYF'. mc_add 'RODTIM'. mc_add 'RODUNI'.
  mc_add 'ROHIEBAS'. mc_add 'ROHIEOBJ'. mc_add 'ROIDOCPRMS'.
  mc_add 'ROIS'. mc_add 'ROISGEN'. mc_add 'ROISIOBJ'. mc_add 'ROIST'.
  mc_add 'ROMSDIOBJ'. mc_add 'RPPRINTFONTCONF'. mc_add 'RS38T_VAR'.
  mc_add 'RSADMIN'. mc_add 'RSAU_BUF_DATA'. mc_add 'RSAUFILES'.
  mc_add 'RSAUFILES_STAT'. mc_add 'RSAUPROF'. mc_add 'RSAUPROFEX'.
  mc_add 'RSBASIDOC'. mc_add 'RSBENFTAB'. mc_add 'RSBENFTABT'.
  mc_add 'RSBIWBENCH'. mc_add 'RSCONREL'. mc_add 'RSDCX'.
  mc_add 'RSDSQCAT'. mc_add 'RSDSQGT'. mc_add 'RSDSQOBJ'.
  mc_add 'RSECACHK'. mc_add 'RSECACTB'. mc_add 'RSECKEYMETA'.
  mc_add 'RSEUINC'. mc_add 'RSEUMOD'. mc_add 'RSEUPACTRL'.
  mc_add 'RSEUTAB'. mc_add 'RSFBTYPEIN'. mc_add 'RSFBTYPEIT'.
  mc_add 'RSHIEOBJ'. mc_add 'RSINFDIR'. mc_add 'RSJOBTAB'.
  mc_add 'RSJOBTABM'. mc_add 'RSMDDELTA'. mc_add 'RSMMETYPE'.
  mc_add 'RSMPCHECK'. mc_add 'RSMPCHECKT'. mc_add 'RSMPTEXTS'.
  mc_add 'RSMPTEXTSI'. mc_add 'RSMPTRNSL'. mc_add 'RSMREPLED'.
  mc_add 'RSMVTDETL'. mc_add 'RSMVTSTAT'. mc_add 'RSMVTYPE'.
  mc_add 'RSNORMDB'. mc_add 'RSNORMTXT'. mc_add 'RSNTR'.
  mc_add 'RSPO_SAMPLE_TEXT'. mc_add 'RSSGTBID'. mc_add 'RSSGTPCLA'.
  mc_add 'RSSGTPCLAT'. mc_add 'RSSGTPDIR'. mc_add 'RSSGTPDIRSUB'.
  mc_add 'RSTMETYPE'. mc_add 'RSTODIRD'. mc_add 'RSTREPLED'.
  mc_add 'RSTVTDETL'. mc_add 'RSTVTSTAT'. mc_add 'RSTVTYPE'.
  mc_add 'RTAB'. mc_add 'RTMRES'. mc_add 'RTMTESTS'. mc_add 'RTXCS'.
  mc_add 'RZLLICLASS'. mc_add 'RZLLITAB'. mc_add 'SA01CHKF'.
  mc_add 'SA01CHKO'. mc_add 'SA01DOKLOREPR'. mc_add 'SA01DOKLORIPR'.
  mc_add 'SA01IDXSTA'. mc_add 'SA01LOIO'. mc_add 'SA01LOIOT'.
  mc_add 'SA01LOPR'. mc_add 'SA01LORE'. mc_add 'SA01LORI'.
  mc_add 'SA01PHF'. mc_add 'SA01PHHR'. mc_add 'SA01PHIO'.
  mc_add 'SA01PHNM'. mc_add 'SA01PHPR'. mc_add 'SA01PHRE'.
  mc_add 'SA01PHRI'. mc_add 'SAACOMP'. mc_add 'SAACOMPT'.
  mc_add 'SAACONT'. mc_add 'SAACONTA'. mc_add 'SAACONTC'.
  mc_add 'SAACONTC2'. mc_add 'SAACONTS'. mc_add 'SAACONTT'.
  mc_add 'SAAHELP'. mc_add 'SAAHELPT'. mc_add 'SAAMONSET'.
  mc_add 'SAASWO'. mc_add 'SAASWOT'. mc_add 'SAASYST'.
  mc_add 'SAASYSTT'. mc_add 'SAAURL'. mc_add 'SABAP_ALV_CHECKS'.
  mc_add 'SABAP_CHK_CNTRL'. mc_add 'SABAP_GUI_CHECKS'.
  mc_add 'SABAP_INACTIVE'. mc_add 'SABAP_LIST_ACC'.
  mc_add 'SABAPCG_SLDCRED'. mc_add 'SABP_ACC_CACHE'.
  mc_add 'SABP_ACC_CLASSES'. mc_add 'SABP_LISTTR_M'.
  mc_add 'SABP_LISTTR_M_LI'. mc_add 'SABP_LISTTR_P'.
  mc_add 'SACC_STANDARDS'. mc_add 'SACC_STANDARDST'.
  mc_add 'SACC_STATES'. mc_add 'SACC_STATEST'.
  mc_add 'SACC_STD_DEFAULT'. mc_add 'SACC_VPATS'. mc_add 'SACF_ALERT'.
  mc_add 'SACL_ACT'. mc_add 'SACONT01'. mc_add 'SACONTURL'.
  mc_add 'SADA_FEATURES'. mc_add 'SADL_ATO_DEPEND'.
  mc_add 'SADL_ATO_HEADER'. mc_add 'SADL_ATO_ITEMS'.
  mc_add 'SADL_DO_NOT_US'. mc_add 'SADL_DRFT_ART'.
  mc_add 'SADL_DRFT_COMP'. mc_add 'SADL_DRFT_COMPO'.
  mc_add 'SADL_DRFT_FIELDS'. mc_add 'SADL_DRFT_SLLIST'.
  mc_add 'SADL_ENTITY_LOAD'. mc_add 'SADL_GTK_LOG'.
  mc_add 'SADL_GW_CONF'. mc_add 'SADL_PREVIEW'.
  mc_add 'SADL_QE_V_HEADER'. mc_add 'SADL_QE_V_ITEM'.
  mc_add 'SADL_V_DATATYPE'. mc_add 'SADLGW_XML_STORE'.
  mc_add 'SADOCATTRIBUTES'. mc_add 'SADOCGENERIC'.
  mc_add 'SADOCMANDATORATT'. mc_add 'SADT_BADI_ACCESS'.
  mc_add 'SADT_CT_DEVEL'. mc_add 'SADT_CT_EDGE'. mc_add 'SADT_CT_EDGET'.
  mc_add 'SADT_CT_NODE'. mc_add 'SADT_CT_NODET'.
  mc_add 'SADT_OAUTH2SCOPE'. mc_add 'SADT_SRL_DATA'. mc_add 'SAICACHE'.
  mc_add 'SAICACHE_ERROR_H'. mc_add 'SAICACHE_ERROR_O'.
  mc_add 'SAIS_CUST'. mc_add 'SAIS_F4_CRIT'. mc_add 'SAIS_F4_CRITT'.
  mc_add 'SALV_BS_BLOB_SAP'. mc_add 'SALV_BS_TT_FILE'.
  mc_add 'SALV_IDA_TEST_TY'. mc_add 'SALV_WD_BL_APPL'. mc_add 'SALVTMS'.
  mc_add 'SAM_ELEMENTS'. mc_add 'SAM_HEADER'. mc_add 'SAM_RELATIONS'.
  mc_add 'SAM_STAT_VALUE'. mc_add 'SAMDT_CONFIG'.
  mc_add 'SAMDT_ELEMENTS'. mc_add 'SAMDT_RELATIONS'.
  mc_add 'SAMDT_SCHEMAS'. mc_add 'SAML2_STATUS'. mc_add 'SAML2_STATUST'.
  mc_add 'SAMSD2BO'. mc_add 'SAMSDELEM'. mc_add 'SAMSDREL'.
  mc_add 'SAMSDVAR'. mc_add 'SAMSDVVAL'. mc_add 'SAMT'.
  mc_add 'SAMT_PSINF'. mc_add 'SAMTE'. mc_add 'SAMTT'. mc_add 'SAMTTR'.
  mc_add 'SAMTX'. mc_add 'SAPACOREPOSITORY'. mc_add 'SAPBWPRFLS'.
  mc_add 'SAPGUIHC_FAQS'. mc_add 'SAPGUIHC_LINKS'.
  mc_add 'SAPGUIHC_LINKST'. mc_add 'SAPGUIHC_NOTES'.
  mc_add 'SAPHASFILT'. mc_add 'SAPHC_REG_NSP'. mc_add 'SAPHC_REG_NSP_T'.
  mc_add 'SAPI_ORG_AC'. mc_add 'SAPI_ORG_AC_REL'. mc_add 'SAPI_ORG_UPD'.
  mc_add 'SAPLIKEY'. mc_add 'SAPWLCOLPA'. mc_add 'SAPWLDMTHD'.
  mc_add 'SAPWLGTCTRL'. mc_add 'SAPWLMOIDX'. mc_add 'SAPWLREORG'.
  mc_add 'SAPWLSERV'. mc_add 'SAPWLSFIDX'. mc_add 'SAPWLSFIHD'.
  mc_add 'SAPWLT000'. mc_add 'SAPWLT001'. mc_add 'SAPWLT002'.
  mc_add 'SAPWLTREEG'. mc_add 'SAPWLTREET'. mc_add 'SAPWLWAMAI'.
  mc_add 'SASACONT1'. mc_add 'SASAPBCSET'. mc_add 'SASAPCUDOC'.
  mc_add 'SASAPCUURL'. mc_add 'SASAPDOM'. mc_add 'SASAPIAKW'.
  mc_add 'SASAPIAPD'. mc_add 'SASAPIAPP'. mc_add 'SATC_AC_ASPERR_B'.
  mc_add 'SATC_AC_ASPERR_S'. mc_add 'SATC_AC_CHK'.
  mc_add 'SATC_AC_CHK_MSG'. mc_add 'SATC_AC_CHK_MSGG'.
  mc_add 'SATC_AC_CHKCFG'. mc_add 'SATC_AC_CONTACTS'.
  mc_add 'SATC_AC_NAMESPCS'. mc_add 'SATC_AC_OBJLOG_B'.
  mc_add 'SATC_AC_PLNERR_B'. mc_add 'SATC_AC_PLNERR_S'.
  mc_add 'SATC_AC_R3TR'. mc_add 'SATC_AC_R3TR_B'.
  mc_add 'SATC_AC_R3TR_REL'. mc_add 'SATC_AC_RESULTG'.
  mc_add 'SATC_AC_RESULTH'. mc_add 'SATC_AC_RESULTHM'.
  mc_add 'SATC_AC_RESULTP'. mc_add 'SATC_AC_RESULTPG'.
  mc_add 'SATC_AC_RESULTVC'. mc_add 'SATC_AC_RESULTVP'.
  mc_add 'SATC_AC_RESULTVR'. mc_add 'SATC_AC_RESULTVT'.
  mc_add 'SATC_AC_RSLT_AE'. mc_add 'SATC_AC_RSLT_VP'.
  mc_add 'SATC_AC_RSLT_VT'. mc_add 'SATC_AC_RSLT4MRG'.
  mc_add 'SATC_AC_S_CONFIG'. mc_add 'SATC_AC_S_STATE'.
  mc_add 'SATC_AC_STATUSH'. mc_add 'SATC_AC_VERDCT_B'.
  mc_add 'SATC_AC_VERDCT_S'. mc_add 'SATC_AC_VRDCT_B'.
  mc_add 'SATC_AC_VRDCT_S'. mc_add 'SATC_ATTRIB_INFO'.
  mc_add 'SATC_CI_APPR_UI'. mc_add 'SATC_CI_APPROVER'.
  mc_add 'SATC_CI_CFG_SRIE'. mc_add 'SATC_CI_CKEPOL'.
  mc_add 'SATC_CI_CUSTOM'. mc_add 'SATC_CI_EX_CHLOG'.
  mc_add 'SATC_CI_EXEMPT'. mc_add 'SATC_CI_GTT_CNTC'.
  mc_add 'SATC_CI_GTT_OBJ'. mc_add 'SATC_CI_REASONS'.
  mc_add 'SATC_CI_REASONST'. mc_add 'SATC_COMP'.
  mc_add 'SATC_DOMAIN_INFO'. mc_add 'SATC_ENVE'. mc_add 'SATC_ENVL'.
  mc_add 'SATC_MD_CONF_DEF'. mc_add 'SATC_MD_CONF_VAL'.
  mc_add 'SATC_MD_DATATYPE'. mc_add 'SATC_MD_DIR'.
  mc_add 'SATC_MD_MAPPING'. mc_add 'SATC_MD_PHS'.
  mc_add 'SATC_MD_PHS_EXT'. mc_add 'SATC_MD_PHS_UNIT'.
  mc_add 'SATC_MD_PRI_DEF'. mc_add 'SATC_MD_PRJ'.
  mc_add 'SATC_MD_PRJ_EXT'. mc_add 'SATC_MD_SRLZD'.
  mc_add 'SATC_MD_STEP'. mc_add 'SATC_MD_STP_USE'. mc_add 'SATC_Q_GRP'.
  mc_add 'SATC_Q_GRP_TX'. mc_add 'SATC_Q_STD'. mc_add 'SATC_Q_STD_TX'.
  mc_add 'SATC_RT_RUN'. mc_add 'SATC_RT_RUN_EXE'.
  mc_add 'SATC_RT_RUN_KEY'. mc_add 'SATC_RT_RUN_LOG'.
  mc_add 'SATC_RT_RUN_PEU'. mc_add 'SATC_RT_RUN_PHS'.
  mc_add 'SATC_RT_RUN_SET'. mc_add 'SATC_SAP_MAPPING'.
  mc_add 'SATC_SET_ATTR'. mc_add 'SATC_SET_HEAD'.
  mc_add 'SATC_STD_TEST'. mc_add 'SATC_STD_TEST_B'.
  mc_add 'SATC_WL_FINDING'. mc_add 'SATC_WL_HEAD'.
  mc_add 'SATC_WL_OBJECT'. mc_add 'SATC_WL_OBJSET'.
  mc_add 'SATC_WL_RAW_CAT'. mc_add 'SATC_WL_RAW_SET'.
  mc_add 'SATCCHKCOMP'. mc_add 'SATCCHKENVE'. mc_add 'SATCEXPVERS'.
  mc_add 'SATR_ASSIGN_CLAS'. mc_add 'SATR_FCODES'.
  mc_add 'SATR_FCODES_FORM'. mc_add 'SATR_HEAD_MAIN'.
  mc_add 'SATR_HEAD_MAIN_T'. mc_add 'SATR_HEAD_MIDL'.
  mc_add 'SATR_HEAD_MIDL_T'. mc_add 'SATR_HIT_HIER'.
  mc_add 'SATR_ID_SUBID'. mc_add 'SATR_ID_SUBID_SD'.
  mc_add 'SATR_PRFLRECS'. mc_add 'SATR_PRFLRECST'. mc_add 'SATR_PRFLT'.
  mc_add 'SATR_ZUO'. mc_add 'SATRELEASE'. mc_add 'SATTEXIT'.
  mc_add 'SATTUSE'. mc_add 'SAUNIT_NO_LEGACY'. mc_add 'SAUNIT_TASK_01'.
  mc_add 'SBF_RU_USAGE_XML'. mc_add 'SBLE_BADI_DRAFTN'.
  mc_add 'SBLE_BADI_DRAFTT'. mc_add 'SBLE_RT_ERROR'.
  mc_add 'SBLE_SRC_VERSION'. mc_add 'SBLM_ACTIVATION'.
  mc_add 'SBLM_ADMIN_LOG'. mc_add 'SBLM_BLACKLIST'. mc_add 'SBLM_LOG'.
  mc_add 'SBM132'. mc_add 'SBMIGEXCEP'. mc_add 'SBMIGNAMES'.
  mc_add 'SBMIGNEWNM'. mc_add 'SBMIGONAME'. mc_add 'SBMIGOPROT'.
  mc_add 'SBMIGOTIMP'. mc_add 'SBMIGTTYPE'. mc_add 'SBPT_CMNDS'.
  mc_add 'SBRANCHEEX'. mc_add 'SBRANCHEIN'. mc_add 'SBRN_EXCL'.
  mc_add 'SBRNTTTTAB'. mc_add 'SC2_AP_CO_HEAD_T'.
  mc_add 'SC2_AP_CO_HEADER'. mc_add 'SC2_AP_IDE_P_CON'.
  mc_add 'SC2_AP_IDE_PAR'. mc_add 'SC2_AP_IDE_PHEAD'.
  mc_add 'SC2_AP_IDE_POCOL'. mc_add 'SC2_BCO_ASSOC_F'.
  mc_add 'SC2_BCO_EXIT'. mc_add 'SC2_BCO_EXIT_DF'.
  mc_add 'SC2_BCO_EXIT_DFT'. mc_add 'SC2_BCO_HEADER'.
  mc_add 'SC2_BCO_NODE'. mc_add 'SC2_BCO_NODE_TAB'.
  mc_add 'SC2_BCO_PROP_DF'. mc_add 'SC2_BCO_PROP_DFT'.
  mc_add 'SC2_BCO_PROPERTY'. mc_add 'SC2_BO_ASSOC'.
  mc_add 'SC2_BO_ASSOC_F'. mc_add 'SC2_BO_BCSIF_REG'.
  mc_add 'SC2_BO_EXIT'. mc_add 'SC2_BO_FLD_MAP'. mc_add 'SC2_BO_HEADER'.
  mc_add 'SC2_BO_NODE'. mc_add 'SC2_BO_NODE_TAB'.
  mc_add 'SC2_BO_PROPERTY'. mc_add 'SC2_IDE_PAR'.
  mc_add 'SC2_IDE_PROT_CON'. mc_add 'SC2_IDE_PROTHEAD'.
  mc_add 'SC2_IDE_PROTOCOL'. mc_add 'SC2_IPG_ACCESS'.
  mc_add 'SC2_IPG_METHOD'. mc_add 'SC2_IPG_METHODT'.
  mc_add 'SC2_IPG_NODE'. mc_add 'SC2_IPG_PARAM'.
  mc_add 'SC2_IPG_PARAMT'. mc_add 'SC2_IPG_QUERY'.
  mc_add 'SC2_PG_ACCESS'. mc_add 'SC2_PG_METHOD'.
  mc_add 'SC2_PG_METHODT'. mc_add 'SC2_PG_NODE'. mc_add 'SC2_PG_PARAM'.
  mc_add 'SC2_PG_PARAMT'. mc_add 'SC2_PG_QUERY'. mc_add 'SC2_PG_QUERYC'.
  mc_add 'SC2_PGL_ACCESS'. mc_add 'SC2_PGL_FLDMAP'.
  mc_add 'SC2_PGL_METHODS'. mc_add 'SC2_PGL_METHODST'.
  mc_add 'SC2_PGL_NODE'. mc_add 'SC2_PGL_NODE_TAB'.
  mc_add 'SCA_DS_DEST'. mc_add 'SCA_DS_GRP'. mc_add 'SCA_DS_SRC'.
  mc_add 'SCAI_PARAMS'. mc_add 'SCBO_BL_ETAG'. mc_add 'SCBO_BL_EVAL'.
  mc_add 'SCBO_BL_KU_HIST'. mc_add 'SCBO_BL_MAPPING'.
  mc_add 'SCBO_BL_SRC_VERS'. mc_add 'SCBO_BL_TEST_VAR'.
  mc_add 'SCBO_ELEMENT'. mc_add 'SCBO_ELEMENT_D'.
  mc_add 'SCBO_GEN_FLAGS'. mc_add 'SCBO_GEN_REPORTS'.
  mc_add 'SCBO_GEN_STATUS'. mc_add 'SCBO_NODE'. mc_add 'SCBO_NODE_D'.
  mc_add 'SCBO_OBJECT'. mc_add 'SCBO_OBJECT_D'. mc_add 'SCBO_SYSTEM'.
  mc_add 'SCCD_ASP_PARAM'. mc_add 'SCCD_ASP_PARAM_T'.
  mc_add 'SCCD_ASP_REG_OBJ'. mc_add 'SCCD_ASPECT'.
  mc_add 'SCCD_ASPECT_T'. mc_add 'SCCD_CFG_ELEM'. mc_add 'SCCD_CONFIG'.
  mc_add 'SCCD_CONFIG_T'. mc_add 'SCCD_RULE'. mc_add 'SCCD_RULE_ELEM'.
  mc_add 'SCCD_RULE_T'. mc_add 'SCCL_CODE'. mc_add 'SCCL_CODE_T'.
  mc_add 'SCCL_D_CODE'. mc_add 'SCCL_D_CODE_T'. mc_add 'SCCL_D_LIST'.
  mc_add 'SCCL_LIST'. mc_add 'SCDTCMPDEF'. mc_add 'SCDTCOMP'.
  mc_add 'SCDTCOMPRL'. mc_add 'SCDTINSTAN'. mc_add 'SCDTLOGCMP'.
  mc_add 'SCDTLOGCMT'. mc_add 'SCDTMAPFLD'. mc_add 'SCDTMAPMOD'.
  mc_add 'SCDTOBJECT'. mc_add 'SCDTOBJMAP'. mc_add 'SCDTOBJOIN'.
  mc_add 'SCDTOBJSEL'. mc_add 'SCDTSOURCE'. mc_add 'SCDTSYSCMP'.
  mc_add 'SCDTSYSRFC_BIZ'. mc_add 'SCDTSYSRFC_HTTP'.
  mc_add 'SCDTTARGET'. mc_add 'SCET_TEST'. mc_add 'SCG_CODE_TEMPL'.
  mc_add 'SCGCLASSIFICATON'. mc_add 'SCGDETAIL'. mc_add 'SCGHEADER'.
  mc_add 'SCGR_CLASS'. mc_add 'SCI_CALL_GRAPH'.
  mc_add 'SCI_CALL_GRAPH_D'. mc_add 'SCI_CALL_GRAPH_E'.
  mc_add 'SCI_CALL_GRAPH_H'. mc_add 'SCI_CHKMAN_HIGH'.
  mc_add 'SCI_DCTHDB_T'. mc_add 'SCI_DCTHDB_W'. mc_add 'SCI_DD28S'.
  mc_add 'SCI_DD28S_E'. mc_add 'SCI_HANA_TEST1'.
  mc_add 'SCI_HANA_TEST1_E'. mc_add 'SCI_HANA_TEST2'.
  mc_add 'SCI_HANA_TEST2_E'. mc_add 'SCI_HANA_TEST2A'.
  mc_add 'SCI_HANA_TEST2B'. mc_add 'SCI_HANA_TEST2C'.
  mc_add 'SCI_HANA_TEST3'. mc_add 'SCI_HANA_TST2A_E'.
  mc_add 'SCI_HANA_TST2B_E'. mc_add 'SCI_OCSASSIGN'.
  mc_add 'SCI_OCSLINES'. mc_add 'SCI_OCSNAME'. mc_add 'SCI_PROG_INFO'.
  mc_add 'SCI_REM_OBJ'. mc_add 'SCI_T_BUF_FUL_E'.
  mc_add 'SCI_T_BUF_GEN_E'. mc_add 'SCI_T_BUF_SGL_E'.
  mc_add 'SCI_TEMPT'. mc_add 'SCI_TEST_DB_I_E'.
  mc_add 'SCI_TEST_DBOPS02'. mc_add 'SCI_TEST_DBOPS04'.
  mc_add 'SCI_TEST_DBTAB_I'. mc_add 'SCI_TESTS'.
  mc_add 'SCI_TST_BUF_CGEE'. mc_add 'SCI_TST_BUF_FUL'.
  mc_add 'SCI_TST_BUF_GEN'. mc_add 'SCI_TST_BUF_GENE'.
  mc_add 'SCI_TST_BUF_NO'. mc_add 'SCI_TST_BUF_SGL'.
  mc_add 'SCI_TST_DBOPS2_E'. mc_add 'SCI_TST_INDX'.
  mc_add 'SCI_TST_INDX_E'. mc_add 'SCI_TXT_INDX_E'. mc_add 'SCI_USERS'.
  mc_add 'SCICHKV_ALTER'. mc_add 'SCICHKV_HD'. mc_add 'SCICHKV_PA'.
  mc_add 'SCICHKV_TX'. mc_add 'SCICOLLS'. mc_add 'SCIERRTY'.
  mc_add 'SCIEXCEPTN'. mc_add 'SCIEXCEPTN_APPL'. mc_add 'SCIINS_ADD'.
  mc_add 'SCIINS_FREE'. mc_add 'SCIINS_INF'. mc_add 'SCIINS_OBJ'.
  mc_add 'SCIINS_VAR'. mc_add 'SCIOBJ_EXTRACT'. mc_add 'SCIOBJ_INF'.
  mc_add 'SCIOBJ_SEL'. mc_add 'SCIOBJ_TP'. mc_add 'SCIOBJ_TPT'.
  mc_add 'SCIPE'. mc_add 'SCIPRIORITIES'. mc_add 'SCIPRTCL'.
  mc_add 'SCIPT'. mc_add 'SCIREST_AGGR'. mc_add 'SCIREST_HD'.
  mc_add 'SCIREST_PS'. mc_add 'SCIREST_PS_BUF'.
  mc_add 'SCIREST_PS_BUF_E'. mc_add 'SCIREST_RT'.
  mc_add 'SCIREST_RTOBJ'. mc_add 'SCIREST_STR'. mc_add 'SCIREST_TMP'.
  mc_add 'SCITESTS'. mc_add 'SCMC_AM_GUID'. mc_add 'SCMC_CACHE_ACC'.
  mc_add 'SCMC_CACHE_CHG'. mc_add 'SCMC_CACHE_PRC'.
  mc_add 'SCMC_CACHE_UPD'. mc_add 'SCMC_LANDSCAPES'.
  mc_add 'SCMC_LNDKEY'. mc_add 'SCMC_NODE'. mc_add 'SCMC_NODE_MSC'.
  mc_add 'SCMC_NODE_PERF'. mc_add 'SCMC_NODE_SMES'. mc_add 'SCMC_SETUP'.
  mc_add 'SCMC_SYSTEMS'. mc_add 'SCMGARCHATTRSRCH'.
  mc_add 'SCMGARCHTEMP'. mc_add 'SCMGATTRHELP'. mc_add 'SCMGAUT_ROLE'.
  mc_add 'SCMGAUT_ROLET'. mc_add 'SCMGAUTACTIVITY'.
  mc_add 'SCMGAUTACTIVITYT'. mc_add 'SCMGFUNCTION'.
  mc_add 'SCMGFUNCTIONT'. mc_add 'SCMGPDIR'. mc_add 'SCMGPOIDCR'.
  mc_add 'SCMGPREL'. mc_add 'SCMGSTAT'. mc_add 'SCMGSTATT'.
  mc_add 'SCMGTERMSAP'. mc_add 'SCMGTERMSAPT'. mc_add 'SCMLP'.
  mc_add 'SCMON_CONFIG'. mc_add 'SCMON_DATA'. mc_add 'SCMON_LOG'.
  mc_add 'SCMON_PROC'. mc_add 'SCMON_PROG'. mc_add 'SCMON_RDATA'.
  mc_add 'SCMON_SLICE'. mc_add 'SCMON_SLICE_S'. mc_add 'SCMON_SLICE_SE'.
  mc_add 'SCMON_SUB'. mc_add 'SCMS_SERCA'. mc_add 'SCMS_SERCS'.
  mc_add 'SCMS_SERPX'. mc_add 'SCMS_STACA'. mc_add 'SCMS_STACS'.
  mc_add 'SCMS_STAPX'. mc_add 'SCMSCACHE'. mc_add 'SCMSCACHT'.
  mc_add 'SCMSCSPL'. mc_add 'SCMSCSPT'. mc_add 'SCMSCSPX'.
  mc_add 'SCMSHOST'. mc_add 'SCMSHOSTT'. mc_add 'SCMSIPNET'.
  mc_add 'SCMSIPNTT'. mc_add 'SCMSIPV6NET'. mc_add 'SCMSLOPA'.
  mc_add 'SCODE_CONTEXT'. mc_add 'SCODE_REGISTRY'. mc_add 'SCOL_ACTION'.
  mc_add 'SCOL_ACTIONT'. mc_add 'SCOL_ASP_ACTION'.
  mc_add 'SCOL_ASP_ACTIONT'. mc_add 'SCOL_ASP_RELAT'.
  mc_add 'SCOL_ASP_RELATT'. mc_add 'SCOL_ASP_VARIANT'.
  mc_add 'SCOL_ASPECT'. mc_add 'SCOL_ASPECT_USE'. mc_add 'SCOL_ASPECTT'.
  mc_add 'SCOL_BO_ADMIN'. mc_add 'SCOL_BO_ADMINT'.
  mc_add 'SCOL_BO_HEADER'. mc_add 'SCOL_BO_HEADERT'.
  mc_add 'SCOL_BO_NODE'. mc_add 'SCOL_BO_NODET'.
  mc_add 'SCOL_BO_NS_DECLS'. mc_add 'SCOL_BO_PROP_DF'.
  mc_add 'SCOL_BO_PROP_DFT'. mc_add 'SCOL_BO_PROPERTY'.
  mc_add 'SCOL_BO_QUERY'. mc_add 'SCOL_BO_QUERYT'. mc_add 'SCOL_BO_USE'.
  mc_add 'SCOL_CLIENT_INDX'. mc_add 'SCOL_CONFIG'.
  mc_add 'SCOL_CONFIGT'. mc_add 'SCOL_GROUP'. mc_add 'SCOL_GROUPMEMBER'.
  mc_add 'SCOL_GROUPT'. mc_add 'SCOL_ND_ACTION'.
  mc_add 'SCOL_ND_ACTIONT'. mc_add 'SCOL_ND_ASSOC'.
  mc_add 'SCOL_ND_ASSOCMAP'. mc_add 'SCOL_ND_ASSOCT'.
  mc_add 'SCOL_ND_QUERY'. mc_add 'SCOL_ND_QUERYT'. mc_add 'SCOL_PMDATA'.
  mc_add 'SCOL_PMSTATE'. mc_add 'SCOL_PROP_DEF'.
  mc_add 'SCOL_PROP_DEFT'. mc_add 'SCOL_PROPERTY'. mc_add 'SCOL_QUERY'.
  mc_add 'SCOL_QUERYT'. mc_add 'SCOL_RELATION'. mc_add 'SCOL_RELATIONT'.
  mc_add 'SCOL_SVC_MODULE'. mc_add 'SCOL_SVC_MODULET'.
  mc_add 'SCOL_TRACE'. mc_add 'SCOL_TYPEMANAGER'.
  mc_add 'SCOL_VALUE_SET'. mc_add 'SCOL_VALUE_SETS'.
  mc_add 'SCOL_VALUE_SETT'. mc_add 'SCOL_WB_DATASET'.
  mc_add 'SCONTAINER'. mc_add 'SCONTEXCL'. mc_add 'SCONTIDENT'.
  mc_add 'SCONTNOSTRUC'. mc_add 'SCOOLOBJECTS'. mc_add 'SCOOLOBJECTST'.
  mc_add 'SCOOLOBJECTVAL'. mc_add 'SCOOLREL'. mc_add 'SCOOLRELATIONS'.
  mc_add 'SCOOLRELATIONS2'. mc_add 'SCOOLRELT'. mc_add 'SCOUNTRYEX'.
  mc_add 'SCOUNTRYIN'. mc_add 'SCOVSRVERR'. mc_add 'SCPRACPM'.
  mc_add 'SCPRACPP'. mc_add 'SCPRACPR'. mc_add 'SCPRACTST'.
  mc_add 'SCPRACTV'. mc_add 'SCPRATTR'. mc_add 'SCPRCCSV'.
  mc_add 'SCPRCOBJ'. mc_add 'SCPRCSVV'. mc_add 'SCPRDATA'.
  mc_add 'SCPRDATAC'. mc_add 'SCPRDOCU'. mc_add 'SCPRENTY'.
  mc_add 'SCPREXCPT'. mc_add 'SCPRFAVO'. mc_add 'SCPRFEAT'.
  mc_add 'SCPRFEATT'. mc_add 'SCPRFLAG'. mc_add 'SCPRFLDV'.
  mc_add 'SCPRKEYS'. mc_add 'SCPRMRPH'. mc_add 'SCPRMRPM'.
  mc_add 'SCPRPBUF'. mc_add 'SCPRPIMG'. mc_add 'SCPRPPRL'.
  mc_add 'SCPRRECA'. mc_add 'SCPRSACTBC'. mc_add 'SCPRSACTID'.
  mc_add 'SCPRSALI'. mc_add 'SCPRSATTR'. mc_add 'SCPRSEXACT'.
  mc_add 'SCPRSEXPANDEDKEY'. mc_add 'SCPRSKEYS'. mc_add 'SCPRSMLI'.
  mc_add 'SCPRSPREDREL'. mc_add 'SCPRSRECA'. mc_add 'SCPRSREVERT'.
  mc_add 'SCPRSREVERTI'. mc_add 'SCPRSREVERTOWNER'.
  mc_add 'SCPRSREVERTOWNRI'. mc_add 'SCPRSREVIEWI'. mc_add 'SCPRSTAM'.
  mc_add 'SCPRSTAT'. mc_add 'SCPRSTEXT'. mc_add 'SCPRSTRANSP'.
  mc_add 'SCPRSTRANSPT'. mc_add 'SCPRSVALL'. mc_add 'SCPRSVALS'.
  mc_add 'SCPRTEMP'. mc_add 'SCPRTEXT'. mc_add 'SCPRUSRC'.
  mc_add 'SCPRUSRV'. mc_add 'SCPRVALL'. mc_add 'SCPRVALS'.
  mc_add 'SCPRVVAR'. mc_add 'SCPRXTAB'. mc_add 'SCQADB'.
  mc_add 'SCQADBAPP'. mc_add 'SCQADBHIER'. mc_add 'SCQADBNODE'.
  mc_add 'SCQADBRECV'. mc_add 'SCR_ABAP_AST'. mc_add 'SCR_ABAP_SCAN'.
  mc_add 'SCR_ABAP_SYMB'. mc_add 'SCR_ABAP_SYMB_SL'.
  mc_add 'SCR_REM_OBJ'. mc_add 'SCR_SRCID'. mc_add 'SCR_SYN_DUMPS'.
  mc_add 'SCR_TEST_DBTAB'. mc_add 'SCR_TEST_DBTAB_E'.
  mc_add 'SCRM_ARF_HEAD'. mc_add 'SCRM_ARF_RESP'.
  mc_add 'SCRM_ARF_UPDT'. mc_add 'SCRM_Q_PHASE'.
  mc_add 'SCRM_Q_PHASE_TX'. mc_add 'SCRM_Q_STD'. mc_add 'SCRM_Q_STD_TX'.
  mc_add 'SCRP_UTIL_TODT'. mc_add 'SCRR_IUUC_STATS'.
  mc_add 'SCSET_REDIST'. mc_add 'SCSM_CHECK_CFG'.
  mc_add 'SCSM_CHECK_CUST'. mc_add 'SCSM_CHECK_DCODE'.
  mc_add 'SCSM_CHECK_ITF'. mc_add 'SCSM_CHECK_MAIL'.
  mc_add 'SCSM_CHECK_MLCUS'. mc_add 'SCSM_CHECK_RELAT'.
  mc_add 'SCSM_CHECK_RLCUS'. mc_add 'SCSM_CHECK_USR'.
  mc_add 'SCSM_MONSYS_BUF'. mc_add 'SCTS_ANALYSIS'.
  mc_add 'SCTS_ATPALOG'. mc_add 'SCTS_COMMON_META'. mc_add 'SCTS_LOGS'.
  mc_add 'SCTS_PROJECT'. mc_add 'SCTS_PROJECTT'.
  mc_add 'SCTS_PROJECTTEAM'. mc_add 'SCTS_PROJECTTYPE'.
  mc_add 'SCTS_PROJECTTYPS'. mc_add 'SCTS_PROJECTTYPT'.
  mc_add 'SCTS_REQ_SW_STEP'. mc_add 'SCTS_SYSGROUPS'.
  mc_add 'SCTS_SYSROLEASSI'. mc_add 'SCTS_SYSROLES'.
  mc_add 'SCTS_TRACK_CAT'. mc_add 'SCTS_TRACK_DGPON'.
  mc_add 'SCTS_TRACK_HEADR'. mc_add 'SCTS_TRACK_KEY'.
  mc_add 'SCTS_TRACK_LOCK'. mc_add 'SCTS_TRACK_MAIN'.
  mc_add 'SCTS_TRACK_META'. mc_add 'SCTS_TRACK_SEM'.
  mc_add 'SCTS_TRACK_UPQUE'. mc_add 'SCTS_USERDEFAULT'.
  mc_add 'SCTS_ZDM_TABLES'. mc_add 'SCURX'. mc_add 'SCUS_HIER'.
  mc_add 'SCUS_HIERT'. mc_add 'SCWB_HIST'. mc_add 'SCWB_INDX'.
  mc_add 'SCWB_RFC_GEN_EXC'. mc_add 'SCWBINDXVS'.
  mc_add 'SDB_CL_FILE_MAP'. mc_add 'SDB_DOCU_SERVER'.
  mc_add 'SDB_DS_TABL_DIFF'. mc_add 'SDB_INDCODE_T'.
  mc_add 'SDB_INDUSTRYCODE'. mc_add 'SDB_SEL_STMT'.
  mc_add 'SDB_SIZE_SCALES'. mc_add 'SDBAADAUPD'. mc_add 'SDBAC'.
  mc_add 'SDBAC_DATA'. mc_add 'SDBAC_PAT'. mc_add 'SDBAC_REC'.
  mc_add 'SDBAC_TEXT'. mc_add 'SDBAC_VAR'. mc_add 'SDBAD'.
  mc_add 'SDBADDB2'. mc_add 'SDBADSDB'. mc_add 'SDBAH'.
  mc_add 'SDBAHSDB'. mc_add 'SDBAP'. mc_add 'SDBAR'. mc_add 'SDBCCMS'.
  mc_add 'SDBCTRL'. mc_add 'SDBDBMRFC'. mc_add 'SDBDBMRFC2'.
  mc_add 'SDBINDING2'. mc_add 'SDBMON'. mc_add 'SDBMONDDDB'.
  mc_add 'SDBPAHI'. mc_add 'SDBSYSTEMS'. mc_add 'SDBTREEDEF'.
  mc_add 'SDBTREETXT'. mc_add 'SDBUPDEXCL'. mc_add 'SDBUSRSET'.
  mc_add 'SDCHANNEL2'. mc_add 'SDDIC_DDLS_M_TXT'.
  mc_add 'SDDIC_DDLS_MOCK'. mc_add 'SDENDPOINTS'. mc_add 'SDMECLS'.
  mc_add 'SDMECLSAGH'. mc_add 'SDMECLSAGI'. mc_add 'SDMECLSATR'.
  mc_add 'SDMECLSMTD'. mc_add 'SDMECLSREL'. mc_add 'SDMECOMTD'.
  mc_add 'SDMECOMTDT'. mc_add 'SDMEDPCH'. mc_add 'SDMEDPCHT'.
  mc_add 'SDMEDPDA'. mc_add 'SDMEDPDC'. mc_add 'SDMEDPDCT'.
  mc_add 'SDMEDPDF'. mc_add 'SDMEDPDH'. mc_add 'SDMEDPDJ'.
  mc_add 'SDMEDPDTC'. mc_add 'SDMEDPDTN'. mc_add 'SDMEDPDTT'.
  mc_add 'SDMEDPFF'. mc_add 'SDMEDPFFT'. mc_add 'SDMEDPFGT'.
  mc_add 'SDMEDPFH'. mc_add 'SDMEDPHDR'. mc_add 'SDMEDPHDRT'.
  mc_add 'SDMEDPMF1'. mc_add 'SDMEDPMF1T'. mc_add 'SDMEDPMF2'.
  mc_add 'SDMEDPMF2T'. mc_add 'SDMEDPSC'. mc_add 'SDMEDPSH'.
  mc_add 'SDMEDPTF'. mc_add 'SDMEDPTFT'. mc_add 'SDMEDPTH'.
  mc_add 'SDMEDPVF'. mc_add 'SDMEDPVH'. mc_add 'SDMEDPVO'.
  mc_add 'SDOC_SETTINGS'. mc_add 'SDOK_CODEPAGES'.
  mc_add 'SDOK_Q_ACTIVE'. mc_add 'SDOK_Q_BATCHUSER'.
  mc_add 'SDOK_Q_BTC_ALERT'. mc_add 'SDOK_Q_BTCENVIRO'.
  mc_add 'SDOK_Q_CLASS_TRX'. mc_add 'SDOK_Q_CLASSES'.
  mc_add 'SDOK_Q_CLASSES_T'. mc_add 'SDOK_Q_CONTROL'.
  mc_add 'SDOK_Q_DEMOENVIR'. mc_add 'SDOK_Q_ERRACTION'.
  mc_add 'SDOK_Q_ERROR'. mc_add 'SDOK_Q_INACTIVE'.
  mc_add 'SDOK_Q_INDEXMTYP'. mc_add 'SDOK_Q_JOBACTALL'.
  mc_add 'SDOK_Q_JOBACTIVE'. mc_add 'SDOK_Q_JOBCTRL'.
  mc_add 'SDOK_Q_MASTERENV'. mc_add 'SDOK_Q_P1_ACTIVE'.
  mc_add 'SDOK_Q_P1_ERROR'. mc_add 'SDOK_Q_P1_SWAP'.
  mc_add 'SDOK_Q_REPNAME'. mc_add 'SDOK_Q_SWAP'. mc_add 'SDOK_Q_TRACE'.
  mc_add 'SDOK_Q_TRACE_DTL'. mc_add 'SDOK_Q_TRACE_ONE'.
  mc_add 'SDOK_Q_TRACELVEL'. mc_add 'SDOKCHKF'. mc_add 'SDOKCHKO'.
  mc_add 'SDOKCLPROP'. mc_add 'SDOKCONT1'. mc_add 'SDOKCPAGEHD'.
  mc_add 'SDOKCPAGEHDC'. mc_add 'SDOKDISTR'. mc_add 'SDOKDOCSP'.
  mc_add 'SDOKDOCSPC'. mc_add 'SDOKDOCSPCIDCAT'.
  mc_add 'SDOKDOCSPCIDCATL'. mc_add 'SDOKDOCSPCIDCATT'.
  mc_add 'SDOKDOCSPH'. mc_add 'SDOKDOCSPT'. mc_add 'SDOKFEXT'.
  mc_add 'SDOKFEXT_C'. mc_add 'SDOKFEXTT'. mc_add 'SDOKFORM'.
  mc_add 'SDOKFORMT'. mc_add 'SDOKIDXATR'. mc_add 'SDOKIDXJOB'.
  mc_add 'SDOKIDXSP'. mc_add 'SDOKIDXSPA'. mc_add 'SDOKIDXSPT'.
  mc_add 'SDOKIDXSPW'. mc_add 'SDOKIDXSTA'. mc_add 'SDOKIOCHNG'.
  mc_add 'SDOKIOCHNG2'. mc_add 'SDOKIOCL'. mc_add 'SDOKIOCLPR'.
  mc_add 'SDOKIOCLRE'. mc_add 'SDOKIOCLT'. mc_add 'SDOKIOTYPE'.
  mc_add 'SDOKLNPR'. mc_add 'SDOKLOC'. mc_add 'SDOKLOCL'.
  mc_add 'SDOKLOCL_C'. mc_add 'SDOKLOCT'. mc_add 'SDOKLOIO'.
  mc_add 'SDOKLOIOT'. mc_add 'SDOKLONM'. mc_add 'SDOKLOPR'.
  mc_add 'SDOKLORE'. mc_add 'SDOKLOREPR'. mc_add 'SDOKLORI'.
  mc_add 'SDOKLORIPR'. mc_add 'SDOKLORTAB'. mc_add 'SDOKLOTAB'.
  mc_add 'SDOKMCLCL'. mc_add 'SDOKME'. mc_add 'SDOKMEP'.
  mc_add 'SDOKMEPC'. mc_add 'SDOKMESA'. mc_add 'SDOKMET'.
  mc_add 'SDOKMEXT'. mc_add 'SDOKMEXTT'. mc_add 'SDOKMIA'.
  mc_add 'SDOKMIAC'. mc_add 'SDOKMIAP'. mc_add 'SDOKMIAPC'.
  mc_add 'SDOKMIME'. mc_add 'SDOKMIME_C'. mc_add 'SDOKMIMS'.
  mc_add 'SDOKMIMS_C'. mc_add 'SDOKMITAB'. mc_add 'SDOKMREL'.
  mc_add 'SDOKMSHBUF'. mc_add 'SDOKMSRC'. mc_add 'SDOKMTAR'.
  mc_add 'SDOKMXA'. mc_add 'SDOKMXAP'. mc_add 'SDOKNEWINDEXING'.
  mc_add 'SDOKNODE'. mc_add 'SDOKNODER'. mc_add 'SDOKNODET'.
  mc_add 'SDOKPHCBUF'. mc_add 'SDOKPHCL'. mc_add 'SDOKPHCL_C'.
  mc_add 'SDOKPHF'. mc_add 'SDOKPHHR'. mc_add 'SDOKPHHRPR'.
  mc_add 'SDOKPHIO'. mc_add 'SDOKPHNM'. mc_add 'SDOKPHNMPR'.
  mc_add 'SDOKPHPR'. mc_add 'SDOKPHRE'. mc_add 'SDOKPHREPR'.
  mc_add 'SDOKPHRI'. mc_add 'SDOKPHRIPR'. mc_add 'SDOKPHRTAB'.
  mc_add 'SDOKPHTAB'. mc_add 'SDOKPHVBUF'. mc_add 'SDOKPROF'.
  mc_add 'SDOKPROP'. mc_add 'SDOKPROPCL'. mc_add 'SDOKPROPT'.
  mc_add 'SDOKPRVL'. mc_add 'SDOKPRVLT'. mc_add 'SDOKRE'.
  mc_add 'SDOKRECL'. mc_add 'SDOKRELOC'. mc_add 'SDOKREPOS'.
  mc_add 'SDOKREPR'. mc_add 'SDOKRETAB'. mc_add 'SDOKSSRTORFC'.
  mc_add 'SDOKSSRTORFCT'. mc_add 'SDOKST'. mc_add 'SDOKSTCA'.
  mc_add 'SDOKSTCAE'. mc_add 'SDOKSTCAT'. mc_add 'SDOKSTCDSP'.
  mc_add 'SDOKSTCL'. mc_add 'SDOKSTPR'. mc_add 'SDOKSTPROP'.
  mc_add 'SDOKSTRE'. mc_add 'SDOKSTRET'. mc_add 'SDOKSTT'.
  mc_add 'SDOKSTTAB'. mc_add 'SDOKTRANSL'. mc_add 'SDOKWSDS'.
  mc_add 'SDOKWSLC'. mc_add 'SDOLCHKO'. mc_add 'SDOLCONT1'.
  mc_add 'SDOLIDXSTA'. mc_add 'SDOLPHF'. mc_add 'SDOLPHHR'.
  mc_add 'SDOLPHIO'. mc_add 'SDOLPHNM'. mc_add 'SDOLPHPR'.
  mc_add 'SDOLPHRE'. mc_add 'SDOLPHRI'. mc_add 'SDRAFT_LC_CONFIG'.
  mc_add 'SDRBGETSEXCEPT'. mc_add 'SDRBPERSOBJTYPES'.
  mc_add 'SDSECURITY'. mc_add 'SDSERVICE2'. mc_add 'SDTSCLNT'.
  mc_add 'SDTSCOMPS'. mc_add 'SDURL2'. mc_add 'SDV_VCL_MIME'.
  mc_add 'SEAPOBJECT'. mc_add 'SEC_POLICY_ATTR'.
  mc_add 'SEC_ST_CACHE_DB'. mc_add 'SEC_ST_REPLAY_DB'.
  mc_add 'SEC_TEST_MAP'. mc_add 'SECM_BTL_EVENTS'.
  mc_add 'SECM_CDL_EVENTS'. mc_add 'SECM_CDLOG_FILT'.
  mc_add 'SECM_CONFIG'. mc_add 'SECM_CUSTOMIZING'.
  mc_add 'SECM_ESP_CONFIG'. mc_add 'SECM_EVT_STRUCT'.
  mc_add 'SECM_LOG_CACHE'. mc_add 'SECM_LOG_COOKIE'.
  mc_add 'SECM_LOG_EVENTS'. mc_add 'SECM_LOG_STAT'.
  mc_add 'SECM_LOG_TR'. mc_add 'SECM_LOG_TS'. mc_add 'SECM_LOGS'.
  mc_add 'SECM_RAL_CFG'. mc_add 'SECM_SCENARIO'.
  mc_add 'SECM_SHLP_EVENTS'. mc_add 'SECM_SHLP_SCEN'.
  mc_add 'SECM_TRACE'. mc_add 'SECM_TRC_STATUS'.
  mc_add 'SECM_UCL_CLIENTS'. mc_add 'SECM_UCL_EVENTS'.
  mc_add 'SEDDD_LOBH_CLASS'. mc_add 'SEDI_ADT_TEST_TA'.
  mc_add 'SEDI_NAV_OO_LOCA'. mc_add 'SEGEXTCONV'. mc_add 'SEGSYB'.
  mc_add 'SEO_CPAK_ENABLE'. mc_add 'SEO_CS_CACHE'.
  mc_add 'SEO_HELP_READ'. mc_add 'SEO_SRC_POS_IND'. mc_add 'SEOCLASS'.
  mc_add 'SEOCLASSDF'. mc_add 'SEOCLASSEX'. mc_add 'SEOCLASSTX'.
  mc_add 'SEOCOMPO'. mc_add 'SEOCOMPODF'. mc_add 'SEOCOMPOEX'.
  mc_add 'SEOCOMPOSRC'. mc_add 'SEOCOMPOTX'. mc_add 'SEOFRIENDS'.
  mc_add 'SEOIMPLREL'. mc_add 'SEOMAPATT'. mc_add 'SEOMAPCLS'.
  mc_add 'SEOMETAREL'. mc_add 'SEOREDEF'. mc_add 'SEOREFPROT'.
  mc_add 'SEOREFTODO'. mc_add 'SEORELAT'. mc_add 'SEORELATTX'.
  mc_add 'SEORELCOMP'. mc_add 'SEORLCMPTX'. mc_add 'SEOSPECTYP'.
  mc_add 'SEOSUBCO'. mc_add 'SEOSUBCODF'. mc_add 'SEOSUBCOEX'.
  mc_add 'SEOSUBCOTX'. mc_add 'SEOTYPEPLS'. mc_add 'SEPM_DG_TESTDATA'.
  mc_add 'SEPMH_DG_LOCK'. mc_add 'SEPP__REGISTRY'.
  mc_add 'SEPP_REGISTRY'. mc_add 'SERIAL_PREVIEW'. mc_add 'SERVTYPE'.
  mc_add 'SERVTYPEM'. mc_add 'SESC_REG_REPS'. mc_add 'SESC_REPS_T'.
  mc_add 'SESI_DEMO_MAT'. mc_add 'SESI_DEMO_MAT_GR'. mc_add 'SEUDEPEND'.
  mc_add 'SEUDEPENTX'. mc_add 'SEUDEPTOBJ'. mc_add 'SEUDEPTTX'.
  mc_add 'SEUDEPTYPE'. mc_add 'SEUERROR'. mc_add 'SEWB_SETTINGS'.
  mc_add 'SFBCONVEXIT'. mc_add 'SFBMETHSIG'. mc_add 'SFCPL'.
  mc_add 'SFDG'. mc_add 'SFHH'. mc_add 'SFHI'. mc_add 'SFHIM'.
  mc_add 'SFHOA'. mc_add 'SFHOT'. mc_add 'SFHX'. mc_add 'SFHYT'.
  mc_add 'SFIA'. mc_add 'SFIAT'. mc_add 'SFICD'. mc_add 'SFICL'.
  mc_add 'SFICT'. mc_add 'SFINO'. mc_add 'SFITH'. mc_add 'SFIWN'.
  mc_add 'SFIWP'. mc_add 'SFLAVOR'. mc_add 'SFLAVOREX'.
  mc_add 'SFLAVORIN'. mc_add 'SFLAVORT'. mc_add 'SFLAW'.
  mc_add 'SFLAWT'. mc_add 'SFORM_S_CHANNEL'. mc_add 'SFORM_S_CHANNELT'.
  mc_add 'SFORM_S_GRATYPE'. mc_add 'SFORM_S_GRATYPET'.
  mc_add 'SFORM_S_ORGTP'. mc_add 'SFORM_S_ORGTPT'.
  mc_add 'SFORM_S_ORGUTP'. mc_add 'SFORM_S_ORGUTPT'.
  mc_add 'SFORM_S_TEXTTYPE'. mc_add 'SFORM_S_TEXTTYPT'. mc_add 'SFREAC'.
  mc_add 'SFRECOU'. mc_add 'SFREIM'. mc_add 'SFRELN'. mc_add 'SFRETREE'.
  mc_add 'SFS_LOG_LEVEL'. mc_add 'SFS_REPL_UNITS'.
  mc_add 'SFS_SYSTEM_GUID'. mc_add 'SFSG_DEF_D'. mc_add 'SFSG_DEF_DT'.
  mc_add 'SFSG_DEF_RT'. mc_add 'SFSG_FREQ'. mc_add 'SFSG_Q_COND'.
  mc_add 'SFSG_Q_GROUP'. mc_add 'SFSG_Q_JOIN'. mc_add 'SFSG_Q_RESULT'.
  mc_add 'SFSG_Q_TABLE'. mc_add 'SFSG_QR_DEF'. mc_add 'SFSG_QR_INPUT'.
  mc_add 'SFSG_QR_RESULT'. mc_add 'SFSG_TAB_POS'. mc_add 'SFSQB_QUERY'.
  mc_add 'SFSQB_QUERY_T'. mc_add 'SFSQB_TAB_POS'.
  mc_add 'SFSRFW_ALIAS_DIR'. mc_add 'SFSRFW_ALIAS_DT'.
  mc_add 'SFSRFW_ALIAS_H'. mc_add 'SFSRFW_BATCH_RT'.
  mc_add 'SFSRFW_BTC_RT'. mc_add 'SFSRFW_ENGINE_CS'.
  mc_add 'SFSRFW_IDX_D'. mc_add 'SFSRFW_IDX_DT'.
  mc_add 'SFSRFW_IDX_IF_RT'. mc_add 'SFSRFW_IDX_IFD'.
  mc_add 'SFSRFW_IDX_KEY_D'. mc_add 'SFSRFW_IDX_RT'.
  mc_add 'SFSRFW_IDX_STMTD'. mc_add 'SFSRFW_IDX_SYS'.
  mc_add 'SFSRFW_IDX_TASKS'. mc_add 'SFSRFW_IDX_TRG_D'.
  mc_add 'SFSRFW_JDX_CONS'. mc_add 'SFSRFW_JDX_D'.
  mc_add 'SFSRFW_JDX_JFD'. mc_add 'SFSRFW_JDX_JP'.
  mc_add 'SFSRFW_JDX_JV'. mc_add 'SFSRFW_JDX_RT'.
  mc_add 'SFSRFW_LOAD_HDL'. mc_add 'SFSRFW_NUMBER'.
  mc_add 'SFSRFW_REPL_CS'. mc_add 'SFSRFW_REQ_CONS'.
  mc_add 'SFSRFW_REQ_H'. mc_add 'SFSRFW_REQ_H_T'.
  mc_add 'SFSRFW_REQ_JOIN'. mc_add 'SFSRFW_REQ_JPATH'.
  mc_add 'SFSRFW_REQ_JVIEW'. mc_add 'SFSRFW_REQ_OBJS'.
  mc_add 'SFSRFW_SCHED_RT'. mc_add 'SFSRFW_TIMES'. mc_add 'SFSYSP'.
  mc_add 'SFUNCUNITREL'. mc_add 'SFW_ACTIVE_B1'. mc_add 'SFW_ACTIVE_B2'.
  mc_add 'SFW_ACTIVE_BFUNC'. mc_add 'SFW_ACTIVE_BSET'.
  mc_add 'SFW_AIM_LOG'. mc_add 'SFW_AIM_WORKLIST'. mc_add 'SFW_BCSET'.
  mc_add 'SFW_BF'. mc_add 'SFW_BF_BF'. mc_add 'SFW_BF_BS'.
  mc_add 'SFW_BF_CHANGE'. mc_add 'SFW_BF_COMPONENT'.
  mc_add 'SFW_BF_DEPEND'. mc_add 'SFW_BF_SW'. mc_add 'SFW_BF_TMP'.
  mc_add 'SFW_BFC_CTC'. mc_add 'SFW_BFC_CUSTTC'. mc_add 'SFW_BFC_KW'.
  mc_add 'SFW_BFC_RN'. mc_add 'SFW_BFC_TC'. mc_add 'SFW_BFSDEP'.
  mc_add 'SFW_BFSSMM'. mc_add 'SFW_BFT'. mc_add 'SFW_BS'.
  mc_add 'SFW_BS_BF'. mc_add 'SFW_BS_BS'. mc_add 'SFW_BS_BS_PARENT'.
  mc_add 'SFW_BS_CHANGE'. mc_add 'SFW_BS_PARENTBS'. mc_add 'SFW_BST'.
  mc_add 'SFW_COMPONENT'. mc_add 'SFW_CONFL'. mc_add 'SFW_DDIC_WRKLIST'.
  mc_add 'SFW_DEACTIVE_BF'. mc_add 'SFW_ENH'. mc_add 'SFW_ENH_CROSS'.
  mc_add 'SFW_ENH_CROSS1'. mc_add 'SFW_ENH_CROSS2'.
  mc_add 'SFW_ENHIMPL'. mc_add 'SFW_EXCL_BF'. mc_add 'SFW_IA_BF_IV'.
  mc_add 'SFW_IA_BF_PR'. mc_add 'SFW_IA_BF_RP'. mc_add 'SFW_IA_BF_TX'.
  mc_add 'SFW_IA_BF_WDA'. mc_add 'SFW_IA_SW'. mc_add 'SFW_IA_SW_IV'.
  mc_add 'SFW_IA_SW_PR'. mc_add 'SFW_IA_SW_RP'. mc_add 'SFW_IA_SW_TX'.
  mc_add 'SFW_IA_SW_WDA'. mc_add 'SFW_INST_XPRA'.
  mc_add 'SFW_INST_XPRA_EX'. mc_add 'SFW_LOGFILE'. mc_add 'SFW_PACKAGE'.
  mc_add 'SFW_SAVE_SF02'. mc_add 'SFW_SNAPSHOT'.
  mc_add 'SFW_STATECHANGE'. mc_add 'SFW_SW_BF'. mc_add 'SFW_SW_CHANGE'.
  mc_add 'SFW_SW_PK_REMOVE'. mc_add 'SFW_SWITCH'.
  mc_add 'SFW_SWITCH_STATE'. mc_add 'SFW_SWITCHT'. mc_add 'SFW_SYSTEM'.
  mc_add 'SFW_USER'. mc_add 'SFW_VIEWFIELD'. mc_add 'SFW_XPRA'.
  mc_add 'SFW5_HISTORY'. mc_add 'SFWPARAM'. mc_add 'SFWPL'.
  mc_add 'SGLWLPRFLS'. mc_add 'SGOSATTR'. mc_add 'SGOSSTXT'.
  mc_add 'SGOSTOJTTR'. mc_add 'SHADOW'. mc_add 'SHDB_INDEX_RES'.
  mc_add 'SHDB_M_FEATURES'. mc_add 'SHDC'. mc_add 'SHDCF'.
  mc_add 'SHDDC'. mc_add 'SHDDCDE'. mc_add 'SHDDCDO'. mc_add 'SHDDF'.
  mc_add 'SHDDFDE'. mc_add 'SHDDFDO'. mc_add 'SHDFOLTREE'.
  mc_add 'SHDFVCI'. mc_add 'SHDFVGUICI'. mc_add 'SHDFVGUICU'.
  mc_add 'SHDGUIXT'. mc_add 'SHDGXTCODE'. mc_add 'SHDIPICATR'.
  mc_add 'SHDIRSCRD'. mc_add 'SHDSCCI'. mc_add 'SHDSTCI'.
  mc_add 'SHDSTCIU'. mc_add 'SHDSTNS'. mc_add 'SHDSTNST'.
  mc_add 'SHDSVCI'. mc_add 'SHDSVFVCI'. mc_add 'SHDSVTXCI'.
  mc_add 'SHDTTCI'. mc_add 'SHDTTCIU'. mc_add 'SHDTVCI'.
  mc_add 'SHDTVCIU'. mc_add 'SHDTVSVCI'. mc_add 'SHDTVSVCIU'.
  mc_add 'SHDUSR'. mc_add 'SHELPTYPE'. mc_add 'SHELPTYPET'.
  mc_add 'SHIE_PROP'. mc_add 'SHIE_PROPT'. mc_add 'SHLPADM1'.
  mc_add 'SHLPADM2T'. mc_add 'SHLPADMP'. mc_add 'SHLPUSRVAL'.
  mc_add 'SHMA_ATTR_RTS'. mc_add 'SHMA_ATTRIBUTES'. mc_add 'SHMA_START'.
  mc_add 'SHMA_START_RTS'. mc_add 'SHMM_TRC_GLOBAL'.
  mc_add 'SHMM_TRC_VARIANT'. mc_add 'SHMSERVICE'.
  mc_add 'SI_RQ_APPL_NAME'. mc_add 'SI_RQ_APPL_NAMET'.
  mc_add 'SI_RQ_ATTR_NAME'. mc_add 'SI_RQ_ATTR_NAMET'.
  mc_add 'SI_RQ_ATTR_VAL'. mc_add 'SI_RQ_ATTR_VALT'.
  mc_add 'SI_RQ_CONTENT'. mc_add 'SI_RQ_CONTENTP'.
  mc_add 'SI_RQ_FILECNTRL'. mc_add 'SI_RQ_FILECNTRLP'. mc_add 'SIAROLE'.
  mc_add 'SIAROLEEX'. mc_add 'SIAROLEIN'. mc_add 'SIAROLET'.
  mc_add 'SIAROLEWP'. mc_add 'SIAROLR'. mc_add 'SIAROLRT'.
  mc_add 'SIC_CROSS_INDEX'. mc_add 'SIC_NORM_MSG'.
  mc_add 'SIDL_TEST_DATA'. mc_add 'SILM_PARAMETERS'.
  mc_add 'SIMDQ_ADDR_FMT'. mc_add 'SIMDQ_CFG_CN'.
  mc_add 'SIMDQ_CFG_NWCN'. mc_add 'SIMDQ_CON_INFO'.
  mc_add 'SIMDQ_DESCRIP'. mc_add 'SIMDQ_FLD_TYP'.
  mc_add 'SIMDQ_GTYP_VAL'. mc_add 'SIMDQ_LOG_LEVEL'.
  mc_add 'SIMDQ_MOD_COL'. mc_add 'SIMDQ_MOD_DEF'.
  mc_add 'SIMDQ_MOD_TAB'. mc_add 'SIMDQ_NAME'.
  mc_add 'SIMDQ_PROJTABNAM'. mc_add 'SIMDQ_PSTL_FMT'.
  mc_add 'SIMDQ_REG_FMT'. mc_add 'SIMDQ_SOURCE_TYP'.
  mc_add 'SIMDQ_SVC_MAP'. mc_add 'SIMDQ_TRAN_FLD'.
  mc_add 'SIMDQCLNSINFLD'. mc_add 'SIQMSOURCETYPES'.
  mc_add 'SITS_AWRT_DEBUG'. mc_add 'SITS_KERNEL_LOG'.
  mc_add 'SITS_KERNEL_LOGN'. mc_add 'SITS_SERVICE_PAR'.
  mc_add 'SITSICONUPLOAD'. mc_add 'SIXMLCE'. mc_add 'SIXMLCT'.
  mc_add 'SIXMLTST'. mc_add 'SKTYREMK'. mc_add 'SKWACTGRP'.
  mc_add 'SKWALOGTXT'. mc_add 'SKWATREE'. mc_add 'SKWATREET'.
  mc_add 'SKWF_APPLS'. mc_add 'SKWF_APPLT'. mc_add 'SKWF_CHPRS'.
  mc_add 'SKWF_LOG'. mc_add 'SKWF_PDYN'. mc_add 'SKWF_SHBUF'.
  mc_add 'SKWF_TRANS'. mc_add 'SKWFIO'. mc_add 'SKWG_DOCS'.
  mc_add 'SKWG_EVENT'. mc_add 'SKWG_EXTT'. mc_add 'SKWG_WFCTS'.
  mc_add 'SKWG_WFCTT'. mc_add 'SKWG_WFCTX'. mc_add 'SKWG_WFEXT'.
  mc_add 'SKWGURLPRT'. mc_add 'SKWPLTF'. mc_add 'SKWPLTFT'.
  mc_add 'SKWR_IXFCS'. mc_add 'SKWR_IXTXS'. mc_add 'SKWS_ACTN'.
  mc_add 'SKWS_ACTNT'. mc_add 'SKWS_AEVT'. mc_add 'SKWS_AEVTT'.
  mc_add 'SKWS_CACHE'. mc_add 'SKWS_CHART'. mc_add 'SKWS_CHRTT'.
  mc_add 'SKWS_DEVT'. mc_add 'SKWS_DEVTT'. mc_add 'SKWS_DMETH'.
  mc_add 'SKWS_EVENT'. mc_add 'SKWS_EVTT'. mc_add 'SKWS_GUARD'.
  mc_add 'SKWS_PROP'. mc_add 'SKWS_STATE'. mc_add 'SKWS_STATT'.
  mc_add 'SKWS_TRANS'. mc_add 'SKWS_TRNST'. mc_add 'SLAD_FAV_OBJSET'.
  mc_add 'SLAD_FAV_PROFILE'. mc_add 'SLAD_OBJECT_SET'.
  mc_add 'SLAD_OBJECT_SETT'. mc_add 'SLAD_OBJSET_ENT'.
  mc_add 'SLAD_OBJSET_ENTL'. mc_add 'SLAD_OBJSET_EXP'.
  mc_add 'SLAD_PROFILE'. mc_add 'SLAD_PROFILE_ENT'.
  mc_add 'SLAD_PROFILET'. mc_add 'SLAPI'. mc_add 'SLAPI_REF'.
  mc_add 'SLAPI_RES'. mc_add 'SLAPI_SREL'. mc_add 'SLAPIAUTH'.
  mc_add 'SLAPIT'. mc_add 'SLAPITTX'. mc_add 'SLAPITX'.
  mc_add 'SLCM_GETS_RW'. mc_add 'SLCM_OBJ_CAT'. mc_add 'SLDAGADM'.
  mc_add 'SLDAGDID'. mc_add 'SLDW_ALERT'. mc_add 'SLDW_ELEMENTS'.
  mc_add 'SLDW_ELEMENTS_RT'. mc_add 'SLDW_HEADER'.
  mc_add 'SLDW_HEADER_RT'. mc_add 'SLDW_HEADERT'. mc_add 'SLEI_BINDING'.
  mc_add 'SLEI_CONDITION'. mc_add 'SLEI_LNKG'. mc_add 'SLEI_TRC_DAT'.
  mc_add 'SLEI_TRC_SEL'. mc_add 'SLGSK'. mc_add 'SLGSM'. mc_add 'SLGSR'.
  mc_add 'SLGST'. mc_add 'SLGSY'. mc_add 'SLIBUVAL'. mc_add 'SLICORDER'.
  mc_add 'SLIN_AUTHORS'. mc_add 'SLIN_CACHE_00'. mc_add 'SLIN_CACHE_01'.
  mc_add 'SLIN_CACHE_02'. mc_add 'SLIN_CACHE_03'.
  mc_add 'SLIN_CACHE_04'. mc_add 'SLIN_CACHE_05'.
  mc_add 'SLIN_CACHE_06'. mc_add 'SLIN_CACHE_07'.
  mc_add 'SLIN_CACHE_08'. mc_add 'SLIN_CACHE_09'.
  mc_add 'SLIN_CACHE_10'. mc_add 'SLIN_CACHE_11'.
  mc_add 'SLIN_CACHE_12'. mc_add 'SLIN_CACHE_VERS'. mc_add 'SLIN_CHCK'.
  mc_add 'SLIN_CHCK_T'. mc_add 'SLIN_DESC'. mc_add 'SLIN_DESC_CUSTOM'.
  mc_add 'SLIN_DESC_T'. mc_add 'SLIN_LOG'. mc_add 'SLIN_REMOTE_INFO'.
  mc_add 'SLIN_TRMSG_KEY'. mc_add 'SLIN_UI_SETTINGS'.
  mc_add 'SLINCACHE'. mc_add 'SLINLOG_R_DETAIL'.
  mc_add 'SLINLOG_R_HEAD'. mc_add 'SLINSEC_LICENSE'.
  mc_add 'SLINSEC_LOG'. mc_add 'SLINSEC_LOG_CFD'. mc_add 'SLINSEC_LOG0'.
  mc_add 'SLINSEC_LOG2'. mc_add 'SLINSEC_PARAM'. mc_add 'SLLS_HEAD'.
  mc_add 'SLLS_STATS_ATTR'. mc_add 'SLLS_STATS_ATTRV'.
  mc_add 'SLLS_STATSDATA'. mc_add 'SLLS_TMSTATSTAB'.
  mc_add 'SLOGICALDEST'. mc_add 'SLSSO'. mc_add 'SLSST'.
  mc_add 'SMAICHKO'. mc_add 'SMAICONT1'. mc_add 'SMAIIDXSTA'.
  mc_add 'SMAILOIO'. mc_add 'SMAILOIOT'. mc_add 'SMAILOPR'.
  mc_add 'SMAILORE'. mc_add 'SMAILORI'. mc_add 'SMAIPHF'.
  mc_add 'SMAIPHHR'. mc_add 'SMAIPHIO'. mc_add 'SMAIPHPR'.
  mc_add 'SMAIPHRE'. mc_add 'SMAIPHRI'. mc_add 'SMAPBP0'.
  mc_add 'SMAPBP0APP'. mc_add 'SMAPBP0T'. mc_add 'SMAPCOMP0'.
  mc_add 'SMAPCOMP0T'. mc_add 'SMAPDOCNO'. mc_add 'SMAPPA0'.
  mc_add 'SMAPPA0T'. mc_add 'SMAPPD0'. mc_add 'SMAPPD0T'.
  mc_add 'SMAPPI0'. mc_add 'SMAPPI0T'. mc_add 'SMAPPR0'.
  mc_add 'SMAPPR0COV'. mc_add 'SMAPPR0T'. mc_add 'SMAPTNODE'.
  mc_add 'SMAPTNODER'. mc_add 'SMAPTNODET'. mc_add 'SMASCHKO'.
  mc_add 'SMASCONT1'. mc_add 'SMASIDXSTA'. mc_add 'SMASLOIO'.
  mc_add 'SMASLOIOT'. mc_add 'SMASLOPR'. mc_add 'SMASLORE'.
  mc_add 'SMASLOREPR'. mc_add 'SMASLORI'. mc_add 'SMASLORIPR'.
  mc_add 'SMASPHF'. mc_add 'SMASPHHR'. mc_add 'SMASPHHRPR'.
  mc_add 'SMASPHIO'. mc_add 'SMASPHNM'. mc_add 'SMASPHNMPR'.
  mc_add 'SMASPHPR'. mc_add 'SMASPHRE'. mc_add 'SMASPHREPR'.
  mc_add 'SMASPHRI'. mc_add 'SMASPHRIPR'. mc_add 'SMDOCDIRE'.
  mc_add 'SMDTSCATEG'. mc_add 'SMDTSCATEG_TXT'. mc_add 'SMDTSCOMPTP'.
  mc_add 'SMDTSCOMPTP_TXT'. mc_add 'SMDTSREASON'.
  mc_add 'SMDTSREASON_TXT'. mc_add 'SMEC_CRIT'. mc_add 'SMEC_META'.
  mc_add 'SMEC_META_WARN'. mc_add 'SMEC_META_WARNT'.
  mc_add 'SMEC_METAT'. mc_add 'SMEC_PROF'. mc_add 'SMEN_BUFF'.
  mc_add 'SMEN_CONEW'. mc_add 'SMEN_COUNT'. mc_add 'SMEN_DATEC'.
  mc_add 'SMEN_DATES'. mc_add 'SMEN_MIGRA'. mc_add 'SMEN_OBLIG'.
  mc_add 'SMEN_OBNEW'. mc_add 'SMEN_UPGR'. mc_add 'SMENAKTNEW'.
  mc_add 'SMENAKTT'. mc_add 'SMENCA'. mc_add 'SMENCA_DEL'.
  mc_add 'SMENCA_DUP'. mc_add 'SMENCA_NEW'. mc_add 'SMENCUSNEW'.
  mc_add 'SMENCUST'. mc_add 'SMENENTNEW'. mc_add 'SMENENTT'.
  mc_add 'SMENF'. mc_add 'SMENFT'. mc_add 'SMENINTNEW'.
  mc_add 'SMENINTT'. mc_add 'SMENLONG'. mc_add 'SMENMANUAL'.
  mc_add 'SMENSAP'. mc_add 'SMENSAPNEW'. mc_add 'SMENSAPT'.
  mc_add 'SMENSAPV'. mc_add 'SMENSELAKT'. mc_add 'SMENSELECT'.
  mc_add 'SMENUSER'. mc_add 'SMGENDOC_SET'. mc_add 'SMID_CONFIG_NAME'.
  mc_add 'SMIFHDR'. mc_add 'SMIFS'. mc_add 'SMIFS_LOOKUP'.
  mc_add 'SMIG_TRACE_META'. mc_add 'SMIMCHKF'. mc_add 'SMIMCHKO'.
  mc_add 'SMIMCONT1'. mc_add 'SMIMIDXSTA'. mc_add 'SMIMLOIO'.
  mc_add 'SMIMLOIOT'. mc_add 'SMIMLOPR'. mc_add 'SMIMLORE'.
  mc_add 'SMIMLOREPR'. mc_add 'SMIMLORI'. mc_add 'SMIMLORIPR'.
  mc_add 'SMIMPHF'. mc_add 'SMIMPHHR'. mc_add 'SMIMPHIO'.
  mc_add 'SMIMPHNM'. mc_add 'SMIMPHPR'. mc_add 'SMIMPHRE'.
  mc_add 'SMIMPHRI'. mc_add 'SMM_ASYNC_RETURN'. mc_add 'SMOB_APPL_VARI'.
  mc_add 'SMOB_APPL_VARIT'. mc_add 'SMOB_APPL_VARIV'. mc_add 'SMODILOG'.
  mc_add 'SMODILOG_TEST'. mc_add 'SMODILOGI'. mc_add 'SMODIPROJ'.
  mc_add 'SMODISRC'. mc_add 'SMODISRC_TEST'. mc_add 'SMODISRCI'.
  mc_add 'SMODITRAN'. mc_add 'SMODIUSER'. mc_add 'SMODIUSER_TEST'.
  mc_add 'SMOI_WS_PROF_T'. mc_add 'SMOI_WS_PROFILE'.
  mc_add 'SMOI_WS_RFCLOGON'. mc_add 'SMPPAUTO'. mc_add 'SMPPBIND'.
  mc_add 'SMPPCASE'. mc_add 'SMPPCONT'. mc_add 'SMPPMAP'.
  mc_add 'SMPPPAR'. mc_add 'SMPPTYP'. mc_add 'SMPPTYPT'.
  mc_add 'SMPPXLOG'. mc_add 'SMTG_TMPL_CONT'. mc_add 'SMTG_TMPL_HDR'.
  mc_add 'SMTG_TMPL_HDR_T'. mc_add 'SMUL_AREA_ALLWED'.
  mc_add 'SMUL_CLASS_ALLWD'. mc_add 'SMUL_MARK_INT'.
  mc_add 'SMUM_ERDEC'. mc_add 'SMUM_ERNUM'. mc_add 'SMUM_XMAP'.
  mc_add 'SNAP'. mc_add 'SNAP_ADT'. mc_add 'SNAP_KRN_PROJ'.
  mc_add 'SNAPT'. mc_add 'SNAPTID'. mc_add 'SNAPTTREX'.
  mc_add 'SNCSYSACL'. mc_add 'SNHI_DU_LANGUAGE'. mc_add 'SNHI_DU_PROXY'.
  mc_add 'SNHI_DU_PUSHDOWN'. mc_add 'SNHI_DUP_PREWORK'.
  mc_add 'SNHI_DUP_PW_OVER'. mc_add 'SNHI_VENDOR_MAPP'.
  mc_add 'SNWD_CEI_B2C'. mc_add 'SNWD_CNTCT_BPROP'.
  mc_add 'SNWD_IACT_BPROP'. mc_add 'SODSAT'. mc_add 'SODSATST'.
  mc_add 'SOEP'. mc_add 'SOMF'. mc_add 'SOMFT'. mc_add 'SORAMON'.
  mc_add 'SOTR_ALIA'. mc_add 'SOTR_ALIAU'. mc_add 'SOTR_EDIT1'.
  mc_add 'SOTR_EDITT'. mc_add 'SOTR_HEAD'. mc_add 'SOTR_HEADU'.
  mc_add 'SOTR_INDEX'. mc_add 'SOTR_LINK'. mc_add 'SOTR_TEXT'.
  mc_add 'SOTR_TEXTU'. mc_add 'SOTR_USE'. mc_add 'SOTR_USER'.
  mc_add 'SOTR_USEU'. mc_add 'SOTRIDCAT'. mc_add 'SOUR'.
  mc_add 'SPA_ESI_PROXYMAP'. mc_add 'SPAK_HOME_NO_CHK'.
  mc_add 'SPAK_TABLE_TEST'. mc_add 'SPAK_UT_DB_1'.
  mc_add 'SPAK_UT_DB_2'. mc_add 'SPAR_ASS'. mc_add 'SPAR_DATA'.
  mc_add 'SPAR_GKDB'. mc_add 'SPAR_METH'. mc_add 'SPAR_NTDB'.
  mc_add 'SPAR_SDAT'. mc_add 'SPAU_SETTINGS'. mc_add 'SPAU_TEST_ADMIN'.
  mc_add 'SPAU_TRANSPORTS'. mc_add 'SPAU_UPL_DATA'. mc_add 'SPERS_EXT'.
  mc_add 'SPERS_GMA'. mc_add 'SPERS_GMAP'. mc_add 'SPERS_GMAS'.
  mc_add 'SPERS_REG'. mc_add 'SPERS_REGA'. mc_add 'SPERS_REGT'.
  mc_add 'SPERSFCAT'. mc_add 'SPERSFLTR'. mc_add 'SPERSQADB'.
  mc_add 'SPERSSORT'. mc_add 'SPN_REPLAY_CACHE'. mc_add 'SPR_ADM'.
  mc_add 'SPR_ADMT'. mc_add 'SPRELEASE'. mc_add 'SPRELEASET'.
  mc_add 'SPROCHKF'. mc_add 'SPROCHKO'. mc_add 'SPROCONT1'.
  mc_add 'SPROIDXSTA'. mc_add 'SPROLOIO'. mc_add 'SPROLOIOT'.
  mc_add 'SPROLOPR'. mc_add 'SPROLORE'. mc_add 'SPROLORI'.
  mc_add 'SPROPHF'. mc_add 'SPROPHHR'. mc_add 'SPROPHIO'.
  mc_add 'SPROPHNM'. mc_add 'SPROPHPR'. mc_add 'SPROPHRE'.
  mc_add 'SPROPHRI'. mc_add 'SPROSTATKW'. mc_add 'SPROSTATSHEMA'.
  mc_add 'SPROSTATSHEMAT'. mc_add 'SPROUPGRADE_T'.
  mc_add 'SPROX_SWITCHES'. mc_add 'SPROX_TEST_DATA'.
  mc_add 'SPROX_WSDL_TEST'. mc_add 'SPROXCAT'. mc_add 'SPROXCATT'.
  mc_add 'SPROXCLASS'. mc_add 'SPROXDAT'. mc_add 'SPROXDEL'.
  mc_add 'SPROXHDR'. mc_add 'SPROXKEY'. mc_add 'SPROXLPT'.
  mc_add 'SPROXMATCHINTF'. mc_add 'SPROXREG'. mc_add 'SPROXSET'.
  mc_add 'SPROXSETX'. mc_add 'SPROXSIG'. mc_add 'SPROXSVARDAT'.
  mc_add 'SPROXTST'. mc_add 'SPROXUITEXTS'. mc_add 'SPROXWSDL'.
  mc_add 'SPROXXSL'. mc_add 'SPROXXSLC'. mc_add 'SPROXXSLP'.
  mc_add 'SPROXXSLQ'. mc_add 'SPROXXSLT'. mc_add 'SPROXXSLTN'.
  mc_add 'SPROXXSLV'. mc_add 'SPRXSWCV'. mc_add 'SPTH'. mc_add 'SPTHB'.
  mc_add 'SPTHT'. mc_add 'SPWSE_DTELEXITRG'. mc_add 'SPWSE_DTELMAP'.
  mc_add 'SPXNMIGRATION'. mc_add 'SQ1000_DB4'. mc_add 'SQ1000_RST'.
  mc_add 'SQ3000_DB4'. mc_add 'SQ3001_DB4'. mc_add 'SQ3002_DB4'.
  mc_add 'SQ3003_DB4'. mc_add 'SQ3004_DB4'. mc_add 'SQ3007_DB4'.
  mc_add 'SQ3008_DB4'. mc_add 'SQABRANCH'. mc_add 'SQABRANCHT'.
  mc_add 'SQACITF'. mc_add 'SQACITOT'. mc_add 'SQACYCLE'.
  mc_add 'SQADB01'. mc_add 'SQADB01A'. mc_add 'SQADB01CAT'.
  mc_add 'SQADB01CON'. mc_add 'SQADB01COV'. mc_add 'SQADB01DOC'.
  mc_add 'SQADB01REV'. mc_add 'SQADB01S'. mc_add 'SQADB01T'.
  mc_add 'SQADB02CH'. mc_add 'SQADB02CHT'. mc_add 'SQADB03CH'.
  mc_add 'SQADB04'. mc_add 'SQADB04CH'. mc_add 'SQADB04PRO'.
  mc_add 'SQADBCAT'. mc_add 'SQADBCATT'. mc_add 'SQADBCON'.
  mc_add 'SQADBCONT'. mc_add 'SQAKONF'. mc_add 'SQAKONFC'.
  mc_add 'SQAKONFG'. mc_add 'SQAKONFT'. mc_add 'SQF_TOOL_GROUP'.
  mc_add 'SQF_TOOL_GROUPT'. mc_add 'SQHVAR_DB4'.
  mc_add 'SQLM_ACTIVITIES'. mc_add 'SQLM_ADMIN'. mc_add 'SQLMD'.
  mc_add 'SQLMHCD'. mc_add 'SQLMHCI'. mc_add 'SQLMHCK'.
  mc_add 'SQLMHCL'. mc_add 'SQLMI'. mc_add 'SQLML'. mc_add 'SQLMM'.
  mc_add 'SQLMQ'. mc_add 'SQLMS'. mc_add 'SQLMSA'. mc_add 'SQLMSHCD'.
  mc_add 'SQLMSHCI'. mc_add 'SQLMSM'. mc_add 'SQLMSQ'. mc_add 'SQLMST'.
  mc_add 'SQLMT'. mc_add 'SQLMZA'. mc_add 'SQLMZD'. mc_add 'SQLMZI'.
  mc_add 'SQLMZM'. mc_add 'SQLMZQ'. mc_add 'SQLMZR'. mc_add 'SQLMZS'.
  mc_add 'SQLMZZ'. mc_add 'SQLRRELE'. mc_add 'SQLRTAB'.
  mc_add 'SQLRTEXT1'. mc_add 'SQLRTEXT2'. mc_add 'SQLRUSER'.
  mc_add 'SQLRUSER1'. mc_add 'SQLRUSER2'. mc_add 'SQLT_INDX'.
  mc_add 'SQSC_RIS_INDEX'. mc_add 'SQTEXT_DB4'. mc_add 'SRAL_CHANNEL'.
  mc_add 'SRAL_CHANNEL_T'. mc_add 'SRAL_DAEMON_RUN'.
  mc_add 'SRAL_FRAME_CFGS'. mc_add 'SRAL_SAVEDSEARCH'.
  mc_add 'SRAPI_DEST'. mc_add 'SRAPI_SWC_DEST'. mc_add 'SRELEASEEX'.
  mc_add 'SRELEASEIN'. mc_add 'SREPOATH'. mc_add 'SREPOCNTR'.
  mc_add 'SREPOTTYPE'. mc_add 'SREPOTYPES'. mc_add 'SRESETLOG'.
  mc_add 'SRESETLOG2'. mc_add 'SRETAPEXIT'. mc_add 'SRETCLASS'.
  mc_add 'SRETCLCRWL'. mc_add 'SRETDOCS'. mc_add 'SRETGELOG'.
  mc_add 'SRETIDATTR'. mc_add 'SRETIDCAT'. mc_add 'SRETIDCATT'.
  mc_add 'SRETIDCON'. mc_add 'SRETIDCTAT'. mc_add 'SRETIDLACI'.
  mc_add 'SRETIDX'. mc_add 'SRETIDXADM'. mc_add 'SRETIDXJOB'.
  mc_add 'SRETIDXSTA'. mc_add 'SRETINBOX'. mc_add 'SRETPROF'.
  mc_add 'SRETPROFD'. mc_add 'SRETPROFS'. mc_add 'SRETSEIF'.
  mc_add 'SRETSOURCE'. mc_add 'SRETSRCERRMATCH'.
  mc_add 'SRETSRCERRMITVL'. mc_add 'SRETSRFCD'. mc_add 'SRETSRFCL'.
  mc_add 'SRETSRSVR'. mc_add 'SRETSRSVRT'. mc_add 'SRETUSER'.
  mc_add 'SREVDOC'. mc_add 'SREVDOCT'. mc_add 'SREVSUBC'.
  mc_add 'SREVSUBCT'. mc_add 'SREVTOP'. mc_add 'SREVTOPT'.
  mc_add 'SRMACPSD'. mc_add 'SRMACPSDT'. mc_add 'SRMACPSV1'.
  mc_add 'SRMACPSV1T'. mc_add 'SRMACREP'. mc_add 'SRMACREP_CLNT'.
  mc_add 'SRMACTAREA'. mc_add 'SRMACTARET'. mc_add 'SRMADE'.
  mc_add 'SRMADET'. mc_add 'SRMALLINK'. mc_add 'SRMAPDIR_T'.
  mc_add 'SRMAPPLOG'. mc_add 'SRMAPREL_T'. mc_add 'SRMAPRTY'.
  mc_add 'SRMAPRTYT'. mc_add 'SRMASPTY'. mc_add 'SRMCHKF01'.
  mc_add 'SRMCHKF03'. mc_add 'SRMCHKF05'. mc_add 'SRMCHKFP02'.
  mc_add 'SRMCHKO01'. mc_add 'SRMCHKO03'. mc_add 'SRMCHKO05'.
  mc_add 'SRMCHKOP02'. mc_add 'SRMCLRODE'. mc_add 'SRMCLRODEI'.
  mc_add 'SRMCLRODET'. mc_add 'SRMCOMPID'. mc_add 'SRMCOMPIFID'.
  mc_add 'SRMCONT_UN'. mc_add 'SRMCONT_UT'. mc_add 'SRMCORODE'.
  mc_add 'SRMCORODEI'. mc_add 'SRMCORODET'. mc_add 'SRMDOC05'.
  mc_add 'SRMDOCP05'. mc_add 'SRMDOCSPSPS'. mc_add 'SRMFPL03'.
  mc_add 'SRMFPLP03'. mc_add 'SRMGSATNAM'. mc_add 'SRMGSP_ATTR_CUST'.
  mc_add 'SRMHLTYPES'. mc_add 'SRMIDXSP02'. mc_add 'SRMIDXST01'.
  mc_add 'SRMIDXST03'. mc_add 'SRMIDXST05'. mc_add 'SRMLOIOT01'.
  mc_add 'SRMLOIOT02'. mc_add 'SRMLOIOT03'. mc_add 'SRMLOIOT05'.
  mc_add 'SRMLOPR01'. mc_add 'SRMLOPR02'. mc_add 'SRMLOPR03'.
  mc_add 'SRMLOPR05'. mc_add 'SRMLORE01'. mc_add 'SRMLORE02'.
  mc_add 'SRMLORE03'. mc_add 'SRMLORE05'. mc_add 'SRMLORI01'.
  mc_add 'SRMLORI02'. mc_add 'SRMLORI03'. mc_add 'SRMLORI05'.
  mc_add 'SRMMIMESPS'. mc_add 'SRMMOD02'. mc_add 'SRMMODP02'.
  mc_add 'SRMORGTYP'. mc_add 'SRMPHF01'. mc_add 'SRMPHF03'.
  mc_add 'SRMPHF05'. mc_add 'SRMPHFP02'. mc_add 'SRMPHHR01'.
  mc_add 'SRMPHHR03'. mc_add 'SRMPHHR05'. mc_add 'SRMPHHRP02'.
  mc_add 'SRMPHNM01'. mc_add 'SRMPHNM03'. mc_add 'SRMPHNM05'.
  mc_add 'SRMPHNMP02'. mc_add 'SRMPHPR01'. mc_add 'SRMPHPR03'.
  mc_add 'SRMPHPR05'. mc_add 'SRMPHPRP02'. mc_add 'SRMPHRE01'.
  mc_add 'SRMPHRE03'. mc_add 'SRMPHRE05'. mc_add 'SRMPHREP02'.
  mc_add 'SRMPHRI01'. mc_add 'SRMPHRI03'. mc_add 'SRMPHRI05'.
  mc_add 'SRMPHRIP02'. mc_add 'SRMPOIDCR'. mc_add 'SRMREC01'.
  mc_add 'SRMRECP01'. mc_add 'SRMRGETYPE'. mc_add 'SRMRGETYPT'.
  mc_add 'SRMRGETYPU'. mc_add 'SRMRGRTYPE'. mc_add 'SRMRGRTYPT'.
  mc_add 'SRMRMSPDIR'. mc_add 'SRMRMSPREL'. mc_add 'SRMSEARCH'.
  mc_add 'SRMSPCL'. mc_add 'SRMSPCLR'. mc_add 'SRMSPCO'.
  mc_add 'SRMSPCOR'. mc_add 'SRMSPDE'. mc_add 'SRMSPDET'.
  mc_add 'SRMSPPADE'. mc_add 'SRMSPSACPV'. mc_add 'SRMSPSDE'.
  mc_add 'SRMSPSDET'. mc_add 'SRMSPSFOREXT'. mc_add 'SRMSPSPV1'.
  mc_add 'SRMSPTYCLR'. mc_add 'SRMSPTYCOR'. mc_add 'SRMSPTYDE'.
  mc_add 'SRMSPTYDET'. mc_add 'SRMTYPPOS'. mc_add 'SRMUSPRO'.
  mc_add 'SRMUSPROFA'. mc_add 'SROADMAP'. mc_add 'SROADMAPEX'.
  mc_add 'SROADMAPIN'. mc_add 'SROADMAPT'. mc_add 'SROADROLE'.
  mc_add 'SROADSUBJ'. mc_add 'SROADTYP'. mc_add 'SROADTYPT'.
  mc_add 'SRPSO'. mc_add 'SRPST'. mc_add 'SRT_AGS_E2E_TRC'.
  mc_add 'SRT_CDTC'. mc_add 'SRT_CDTC_C_DET'. mc_add 'SRT_CDTC_CF_DET'.
  mc_add 'SRT_CDTC_CHG_ID'. mc_add 'SRT_CDTC_CI_DET'.
  mc_add 'SRT_CDTC_CO_DET'. mc_add 'SRT_CDTC_DESCR_T'.
  mc_add 'SRT_CDTC_EP_DET'. mc_add 'SRT_CDTC_G_DET'.
  mc_add 'SRT_CDTC_IS_DET'. mc_add 'SRT_CDTC_S_DET'.
  mc_add 'SRT_CDTC_SC_DET'. mc_add 'SRT_CFG_TPLC_DAT'.
  mc_add 'SRT_CFG_TPLC_DIR'. mc_add 'SRT_CFG_TPLS_DAT'.
  mc_add 'SRT_CFG_TPLS_DIR'. mc_add 'SRT_CM_CHANGE'.
  mc_add 'SRT_CM_CHANGE_CD'. mc_add 'SRT_CM_DELTA'.
  mc_add 'SRT_CSD_DIR'. mc_add 'SRT_DT_CFG_DATA'.
  mc_add 'SRT_DT_CFG_DIR'. mc_add 'SRT_DT_CFG_DIR_T'.
  mc_add 'SRT_DT_CHG_DIR'. mc_add 'SRT_DT_CHG_DIR_L'.
  mc_add 'SRT_DT_IMAGE'. mc_add 'SRT_ENTITY'. mc_add 'SRT_EVENT_OP'.
  mc_add 'SRT_EVENT_QUEUE'. mc_add 'SRT_EVENTS'.
  mc_add 'SRT_LTRC_DETAIL'. mc_add 'SRT_MONI_NAVI_A'.
  mc_add 'SRT_MONI_NAVI_C'. mc_add 'SRT_MONI_TS'.
  mc_add 'SRT_MONILOG_ADM'. mc_add 'SRT_MONILOG_CUST'.
  mc_add 'SRT_MONILOG_DATA'. mc_add 'SRT_MONLOG_DATA2'.
  mc_add 'SRT_PRX_EX_CHECK'. mc_add 'SRT_PT_INDEX'.
  mc_add 'SRT_RTC_DT_IF_SD'. mc_add 'SRT_RTC_DT_RT'.
  mc_add 'SRT_RTC_DT_RT_MD'. mc_add 'SRT_SA_APPL'.
  mc_add 'SRT_SA_APPL_T'. mc_add 'SRT_SA_APPL_XI'.
  mc_add 'SRT_SA_APPL_XI_T'. mc_add 'SRT_SA_CACHE'. mc_add 'SRT_SA_PRF'.
  mc_add 'SRT_SA_PRF_T'. mc_add 'SRT_SA_PRF_XI'.
  mc_add 'SRT_SA_PRF_XI_T'. mc_add 'SRT_SAVED_SEARCH'.
  mc_add 'SRT_SDN_ART_IDS'. mc_add 'SRT_SDN_URL'.
  mc_add 'SRT_SDN_URL_C'. mc_add 'SRT_SEQ_CPENSATE'.
  mc_add 'SRT_SEQ_CPS_PSRV'. mc_add 'SRT_SEQ_SCD_CONF'.
  mc_add 'SRT_SEQ_SCD_STAT'. mc_add 'SRT_SEQ_SDC_ASFK'.
  mc_add 'SRT_SOAM_APPL_ID'. mc_add 'SRT_SYS_NR'.
  mc_add 'SRT_SYS_SETTINGS'. mc_add 'SRT_TEST_DESTINA'.
  mc_add 'SRT_TEST_DIR'. mc_add 'SRT_TEST_ENDPT'.
  mc_add 'SRT_TEST_EVENT'. mc_add 'SRT_TEST_OBJECTS'.
  mc_add 'SRT_TEST_PERF'. mc_add 'SRT_TEST_PING'.
  mc_add 'SRT_TEST_RESULTS'. mc_add 'SRT_TEST_WSA_CL'.
  mc_add 'SRT_TF_BAPI_TES1'. mc_add 'SRT_TF_LP'.
  mc_add 'SRT_TF_TST_DATA'. mc_add 'SRT_TF_USER'.
  mc_add 'SRT_TRACE_USERS'. mc_add 'SRT_TRACEKEYS'.
  mc_add 'SRT_TRC_COUNTER'. mc_add 'SRT_UTIL_LOG'.
  mc_add 'SRT_UTIL_PERF'. mc_add 'SRT_WS_MSG_STATE'.
  mc_add 'SRT_WS_SAVE_MODE'. mc_add 'SRT_WSICF_PREF'.
  mc_add 'SRT_WSP_ASSERTS'. mc_add 'SRT_WSP_LDT_DAT'. mc_add 'SRT2RTTY'.
  mc_add 'SRTCM_ACTIVITIES'. mc_add 'SRTCM_ADMIN'.
  mc_add 'SRTCM_SNAPSHOT_C'. mc_add 'SRTCM_SNAPSHOT_D'.
  mc_add 'SRTCM_SNAPSHOT_F'. mc_add 'SRTCM_SNAPSHOT_H'.
  mc_add 'SRTCM_SNAPSHOT_S'. mc_add 'SRTFT_SA_APPL'.
  mc_add 'SRTFT_SA_APPL_T'. mc_add 'SRTM_ACT'. mc_add 'SRTM_ACT_HIST'.
  mc_add 'SRTM_DAAB'. mc_add 'SRTM_DATA'. mc_add 'SRTM_DATAX'.
  mc_add 'SRTM_LOG'. mc_add 'SRTM_PARAMS'. mc_add 'SRTM_PROC'.
  mc_add 'SRTM_PROG'. mc_add 'SRTM_SUB'. mc_add 'SRTM_TEST'.
  mc_add 'SRTM_TRIG'. mc_add 'SRTMP_DATA'. mc_add 'SRTMP_PROC'.
  mc_add 'SRTMP_PROG'. mc_add 'SRTMP_SUB'. mc_add 'SRTSERMTCH'.
  mc_add 'SRTSERMTVL'. mc_add 'SRTVCLIENT'. mc_add 'SRTVCORPAR'.
  mc_add 'SRTVCORPUS'. mc_add 'SRTVCORREL'. mc_add 'SRTVEXTID'.
  mc_add 'SRTVLEXENT'. mc_add 'SRTVLEXICO'. mc_add 'SRTVOCCUR'.
  mc_add 'SRTVONTMAP'. mc_add 'SRTVSTOPLS'. mc_add 'SSF_PSE_D'.
  mc_add 'SSF_PSE_H'. mc_add 'SSF_PSE_HIST'. mc_add 'SSF_PSE_L'.
  mc_add 'SSF_PSE_T'. mc_add 'SSFAPPLIC'. mc_add 'SSFAPPLICT'.
  mc_add 'SSFOBJSCR'. mc_add 'SSFSET'. mc_add 'SSFVARGS'.
  mc_add 'SSFVARGST'. mc_add 'SSFXSFBMP'. mc_add 'SSFXSFCSS'.
  mc_add 'SSHM_DAEMON_RUN'. mc_add 'SSHM_PARA_SYS'. mc_add 'SSM_CID'.
  mc_add 'SSM_CIDT'. mc_add 'SSM_COL'. mc_add 'SSM_CUST'.
  mc_add 'SSM_EX'. mc_add 'SSM_LANGU'. mc_add 'SSM_LINKS'.
  mc_add 'SSM_RELE'. mc_add 'SSM_RFC'. mc_add 'SSM_START'.
  mc_add 'SSM_STAT'. mc_add 'SSM_SYST'. mc_add 'SSM_USR'.
  mc_add 'SSM_VAR'. mc_add 'SSM_VART'. mc_add 'SSPI_OV_ATT'.
  mc_add 'SSPI_OV_ATT_T'. mc_add 'SSPIMOALERTS'.
  mc_add 'SSPIMOATMSCTAB'. mc_add 'SSPIMOATPERFTAB'.
  mc_add 'SSPIMODSSTOR'. mc_add 'SSPIRD'. mc_add 'SSPIRD_T'.
  mc_add 'SSPITRCLVL'. mc_add 'SSPUMK'. mc_add 'SSRVBINDINGATT'.
  mc_add 'SSRVCHANNELATT'. mc_add 'SSUBJECT'. mc_add 'SSUBJECTEX'.
  mc_add 'SSUBJECTIN'. mc_add 'SSUBJECTT'. mc_add 'SSYNTAXSTRUCTURE'.
  mc_add 'ST04N_ANA'. mc_add 'ST04N_ANAT'. mc_add 'ST04N_LIM'.
  mc_add 'ST05_VOID_TABLES'. mc_add 'ST05RESULT'. mc_add 'STABULOG'.
  mc_add 'STACKCOMPS'. mc_add 'STACKHEADR'. mc_add 'START_GUID'.
  mc_add 'STAT_XPRA'. mc_add 'STATS_STATISTICS'.
  mc_add 'STC_BASSCN_HDR'. mc_add 'STC_BASSCN_HDR_T'.
  mc_add 'STC_CONT_CLM_PAR'. mc_add 'STC_CUST_PHASE'.
  mc_add 'STC_CUST_PHASET'. mc_add 'STC_SCN_ATTR'. mc_add 'STC_SCN_HDR'.
  mc_add 'STC_SCN_HDR_T'. mc_add 'STC_SCN_TASKS'. mc_add 'STCCONT_CE'.
  mc_add 'STCCONT_CE_T'. mc_add 'STCCONT_CEO'. mc_add 'STCCONT_CEP'.
  mc_add 'STCCONT_CEP_T'. mc_add 'STCCONT_CET'. mc_add 'STCCONT_CRH'.
  mc_add 'STCCONT_CRN'. mc_add 'STCCONT_CRN_T'. mc_add 'STCCONT_CRP'.
  mc_add 'STCCONT_CRP_T'. mc_add 'STCCONT_CRS'. mc_add 'STERM_ADDC'.
  mc_add 'STERM_ADDT'. mc_add 'STERM_COMM'. mc_add 'STERM_COMP'.
  mc_add 'STERM_COMP_WEB'. mc_add 'STERM_COUN_LANG'.
  mc_add 'STERM_DWN1'. mc_add 'STERM_DWN2'. mc_add 'STERM_EXTD'.
  mc_add 'STERM_HEAD'. mc_add 'STERM_INDX'. mc_add 'STERM_LINK'.
  mc_add 'STERM_LOG'. mc_add 'STERM_MULT'. mc_add 'STERM_PRJ_DATA'.
  mc_add 'STERM_PRJ_HEAD'. mc_add 'STERM_PRJ_SNAP'.
  mc_add 'STERM_PRJ_TGROUP'. mc_add 'STERM_PROPOSALS'.
  mc_add 'STERM_Q_CHECK'. mc_add 'STERM_Q_CHECK_H'.
  mc_add 'STERM_Q_MAIL'. mc_add 'STERM_REF'. mc_add 'STERM_RELEASES'.
  mc_add 'STERM_RQST'. mc_add 'STERM_SEARCHES'. mc_add 'STERM_SYS'.
  mc_add 'STERM_SYSTEMID'. mc_add 'STERM_SYSTEMID_T'.
  mc_add 'STERM_TERM_CAT'. mc_add 'STERM_TEXT'.
  mc_add 'STERM_TRA_ALERT'. mc_add 'STERM_TRA_ALTEXT'.
  mc_add 'STERM_TRANSL'. mc_add 'STERM_TRL'. mc_add 'STERM_TXTC'.
  mc_add 'STERM_USAGE'. mc_add 'STERM_USER'. mc_add 'STERM_XTND'.
  mc_add 'STERM_XTNT'. mc_add 'STFUTRACEORA'. mc_add 'STIMEDEVENTS'.
  mc_add 'STJR_JOBD_ROOT'. mc_add 'STJR_JOBD_TREP'. mc_add 'STORE_HELP'.
  mc_add 'STORE_LINK_IND'. mc_add 'STPCYCLE'. mc_add 'STRACEORA'.
  mc_add 'STRING_TEST'. mc_add 'STRJ_WRK1'. mc_add 'STRUSTCAB'.
  mc_add 'STRUSTCABEMAIL'. mc_add 'STRUSTCERT'. mc_add 'STRUSTSSL'.
  mc_add 'STRUSTSSLS'. mc_add 'STRUSTSSLST'. mc_add 'STRUSTSSLT'.
  mc_add 'STRUSTWSSE'. mc_add 'STRUSTWSSET'. mc_add 'STRX_IXDAS'.
  mc_add 'STRX_IXPTS'. mc_add 'STRX_LIXS'. mc_add 'STRX_LPIXS'.
  mc_add 'STU3COTABC'. mc_add 'STU3COTABS'. mc_add 'STWB_SET'.
  mc_add 'STXBITMAPS'. mc_add 'STXFADM'. mc_add 'STXFADMI'.
  mc_add 'STXFADMT'. mc_add 'STXFCONT'. mc_add 'STXFCONTR'.
  mc_add 'STXFCONTS'. mc_add 'STXFCONTV'. mc_add 'STXFIMP'.
  mc_add 'STXFOBJT'. mc_add 'STXFPCUST'. mc_add 'STXFSTDPAR'.
  mc_add 'STXFTXT'. mc_add 'STXFTXTA'. mc_add 'STXFTXTV'.
  mc_add 'STXFVAR'. mc_add 'STXFVARI'. mc_add 'STXFVART'.
  mc_add 'STXITFD'. mc_add 'STXITFR'. mc_add 'STXOTFD'.
  mc_add 'STXOTFR'. mc_add 'STXSADM'. mc_add 'STXSADMT'.
  mc_add 'STXSCHAR'. mc_add 'STXSFPATT'. mc_add 'STXSFREPL'.
  mc_add 'STXSHEAD'. mc_add 'STXSOBJT'. mc_add 'STXSPARA'.
  mc_add 'STXSTAB'. mc_add 'STXSVAR'. mc_add 'STXSVARL'.
  mc_add 'STXSVART'. mc_add 'STXTRACE'. mc_add 'STXTRACEHD'.
  mc_add 'STXTRCUSR'. mc_add 'SUBST_SLANA_HDR'.
  mc_add 'SUBST_SLANA_POS'. mc_add 'SUBST_UPGEV_HDR'.
  mc_add 'SUBST_UPGEV_POS'. mc_add 'SUBT_LOCAL_CHECK'.
  mc_add 'SUI_TM_MM_APP'. mc_add 'SUI_TM_MM_APPT'.
  mc_add 'SUICS_VH_ENABLED'. mc_add 'SUIM_PARA_TCD'. mc_add 'SUKRI'.
  mc_add 'SUKRIT'. mc_add 'SUSAGE'. mc_add 'SUSAGE_TEST'.
  mc_add 'SUSR_UST12'. mc_add 'SVALT'. mc_add 'SVERS'.
  mc_add 'SVMCRT_BINDINGS'. mc_add 'SVMCRT_COMP_CAT'.
  mc_add 'SVMCRT_CONF'. mc_add 'SVMCRT_CONF_NODE'.
  mc_add 'SVMCRT_CONF_NPRP'. mc_add 'SVMCRT_CONF_NVLB'.
  mc_add 'SVMCRT_CONF_NVLI'. mc_add 'SVMCRT_CONF_NVLS'.
  mc_add 'SVMCRT_FB_DEST'. mc_add 'SVMCRT_MOD_CAT'.
  mc_add 'SVMCRT_MOD_DEP'. mc_add 'SVMCRT_MOD_TR_T'.
  mc_add 'SVMCRT_MOD_TRANS'. mc_add 'SVMCRT_MODULES'.
  mc_add 'SVMCRT_RES_TRANS'. mc_add 'SVMCRT_RESOURCES'.
  mc_add 'SVRS_VCNT_DATA'. mc_add 'SVRS_VCNT_HEAD'.
  mc_add 'SVRS_VCNT_LANG'. mc_add 'SW3M_ADMIN'. mc_add 'SW3M_ADMT'.
  mc_add 'SW3M_ATTR'. mc_add 'SW3M_PARAM'. mc_add 'SW3M_PARAT'.
  mc_add 'SWASYSTFLD'. mc_add 'SWBRULEBIN'. mc_add 'SWBRULECOU'.
  mc_add 'SWBRULEHDR'. mc_add 'SWBRULETXT'. mc_add 'SWBSAPDFLT'.
  mc_add 'SWBTESTCUR'. mc_add 'SWD_HRS1205'. mc_add 'SWD_LEVTTYPS'.
  mc_add 'SWD_PROFILE'. mc_add 'SWD_PROPS'. mc_add 'SWD_RTYPES'.
  mc_add 'SWD_RUSED'. mc_add 'SWD_WFPERS'. mc_add 'SWDDUSRATT'.
  mc_add 'SWDDUSRATTTXT'. mc_add 'SWDILOG'. mc_add 'SWDPCMD'.
  mc_add 'SWDPCMDT'. mc_add 'SWDPEXCP'. mc_add 'SWDPEXCPT'.
  mc_add 'SWDPGRPHDR'. mc_add 'SWDPGRPPRT'. mc_add 'SWDPGRPTXT'.
  mc_add 'SWDPHEADER'. mc_add 'SWDPOPERS'. mc_add 'SWDPPRCMD'.
  mc_add 'SWDPPRCMEX'. mc_add 'SWDPTEXTS'. mc_add 'SWDSBINDEF'.
  mc_add 'SWDSCNTIDX'. mc_add 'SWDSCONDEF'. mc_add 'SWDSCONT'.
  mc_add 'SWDSDSTEPS'. mc_add 'SWDSEVNTS'. mc_add 'SWDSEXPR'.
  mc_add 'SWDSFORMS'. mc_add 'SWDSFUNCS'. mc_add 'SWDSGLOBAL'.
  mc_add 'SWDSHEADER'. mc_add 'SWDSLEVNTS'. mc_add 'SWDSLOCKS'.
  mc_add 'SWDSMETHOD'. mc_add 'SWDSMLINES'. mc_add 'SWDSMNODES'.
  mc_add 'SWDSPROPTS'. mc_add 'SWDSSTEPS'. mc_add 'SWDSTASKS'.
  mc_add 'SWDSTEXT'. mc_add 'SWDSWFCONT'. mc_add 'SWDSWFCTXT'.
  mc_add 'SWECDFIELD'. mc_add 'SWECDOBJ'. mc_add 'SWECDOBTYP'.
  mc_add 'SWECDVALUE'. mc_add 'SWF_BAM_TRC_DAT'.
  mc_add 'SWF_BAM_TRC_SEL'. mc_add 'SWF_CATIDS'.
  mc_add 'SWF_FLEX_SCREG'. mc_add 'SWF_PF_REL_EVT'. mc_add 'SWF_POPORD'.
  mc_add 'SWF_POPUP1'. mc_add 'SWF_SLS_TRC_DAT'.
  mc_add 'SWF_SLS_TRC_SEL'. mc_add 'SWF_TRC_ACONF'.
  mc_add 'SWF_TRC_CMP'. mc_add 'SWF_TRC_CMP_TEXT'.
  mc_add 'SWF_TRC_CONF'. mc_add 'SWF_TRC_CONT'.
  mc_add 'SWF_TRC_DEMO_DAT'. mc_add 'SWF_TRC_DEMO_SEL'.
  mc_add 'SWF_TRC_HFILT'. mc_add 'SWF_TRC_PCK'.
  mc_add 'SWF_TRC_PCK_TEXT'. mc_add 'SWF_TRC_SELOPT'.
  mc_add 'SWF_TRC_TEXT'. mc_add 'SWF_TRC_WF_DAT'.
  mc_add 'SWF_TRC_WF_SEL'. mc_add 'SWF_WFPDSC'. mc_add 'SWF_WFPREC'.
  mc_add 'SWF_WFPTAB'. mc_add 'SWFBND_OPCONF'. mc_add 'SWFBRULECOU'.
  mc_add 'SWFCNTBUFMON'. mc_add 'SWFCTASK'. mc_add 'SWFDCNTSDT'.
  mc_add 'SWFDCRLCBN'. mc_add 'SWFDCRLCMP'. mc_add 'SWFDCRLHCN'.
  mc_add 'SWFDCRLHCT'. mc_add 'SWFDCRLHDR'. mc_add 'SWFDCRLHDT'.
  mc_add 'SWFDEBUGF4HELP'. mc_add 'SWFDEXPVER'. mc_add 'SWFDLOGCMP'.
  mc_add 'SWFDLOGHDR'. mc_add 'SWFDLOGPRO'. mc_add 'SWFDLOGPRT'.
  mc_add 'SWFDLOGTYP'. mc_add 'SWFDWISTAT'. mc_add 'SWFDXMPCFGCAT'.
  mc_add 'SWFDXMPCNT'. mc_add 'SWFDXMPCNTT'. mc_add 'SWFDXMPEAC'.
  mc_add 'SWFDXMPHDR'. mc_add 'SWFDXMPHDRT'. mc_add 'SWFDXMPINDX'.
  mc_add 'SWFDXMPNSP'. mc_add 'SWFDXMPXI'. mc_add 'SWFDXMPXIRT'.
  mc_add 'SWFEATURE'. mc_add 'SWFEATUREINC'. mc_add 'SWFGPGENOBJNUM'.
  mc_add 'SWFMIG_CONTROL'. mc_add 'SWFMIG_REF_CORR'.
  mc_add 'SWFMIG_REF_MESG'. mc_add 'SWFMIG_REF_TASK'.
  mc_add 'SWFMIG_WFDID'. mc_add 'SWFPFMSECTION'.
  mc_add 'SWFPFMSECTIONT'. mc_add 'SWFRCRLINST'.
  mc_add 'SWFREXTPROCTYP'. mc_add 'SWFRLSOPER'. mc_add 'SWFRRSTST'.
  mc_add 'SWFSLSAPPLCUS'. mc_add 'SWFSLSAPPLCUST'.
  mc_add 'SWFSNOTIF_ACT'. mc_add 'SWFSNOTIF_DECI'.
  mc_add 'SWFSWRSYSTEM'. mc_add 'SWFVGVTP'. mc_add 'SWFVO'.
  mc_add 'SWFVOV'. mc_add 'SWFVT'. mc_add 'SWFVTV'. mc_add 'SWFVVTP'.
  mc_add 'SWFVW'. mc_add 'SWFVWVT'. mc_add 'SWFXIADM_TSK'.
  mc_add 'SWFXICLOG'. mc_add 'SWFXIINB_TRC_DAT'.
  mc_add 'SWFXIINB_TRC_SEL'. mc_add 'SWFXIINBENQ'. mc_add 'SWFXIKEYMAP'.
  mc_add 'SWFXIOBJS'. mc_add 'SWFXST'. mc_add 'SWL_ST_TRA'.
  mc_add 'SWL_ST_TRN'. mc_add 'SWLFBASHOW'. mc_add 'SWLN3CMPIF'.
  mc_add 'SWLN3CMPIT'. mc_add 'SWLN3MOIDX'. mc_add 'SWLN3PARAM'.
  mc_add 'SWLN3WORKL'. mc_add 'SWLT_PARAMS'. mc_add 'SWLT_SQLM_D'.
  mc_add 'SWLT_SQLM_H'. mc_add 'SWLT_STATIC_C_D'.
  mc_add 'SWLT_STATIC_C_H'. mc_add 'SWLT_STATIC_D'.
  mc_add 'SWLT_STATIC_H'. mc_add 'SWLT_TABLE_MD'.
  mc_add 'SWLT_TEST_DBTAB'. mc_add 'SWN_SETTINGSS'.
  mc_add 'SWN_SETTINGSST'. mc_add 'SWNC_FIELDS'. mc_add 'SWNC_GROUPS'.
  mc_add 'SWNC_SETTINGS'. mc_add 'SWNC_TCOLL_LOG'. mc_add 'SWNC_TEXTS'.
  mc_add 'SWNCCOLLCOMPRESS'. mc_add 'SWNCCOLLPACKSIZE'.
  mc_add 'SWNCCOLLPARAM'. mc_add 'SWNCCOLLPATXT'. mc_add 'SWNCCOLLPERF'.
  mc_add 'SWNCCOLLPERFTOT'. mc_add 'SWNCCOLLSTORAGE'.
  mc_add 'SWNCGLOBDEST'. mc_add 'SWNCM_AHITLISTDB'.
  mc_add 'SWNCM_AHITLISTRT'. mc_add 'SWNCM_APPSTAT'.
  mc_add 'SWNCM_DBCON'. mc_add 'SWNCM_DBPROC'. mc_add 'SWNCM_EXTSYST'.
  mc_add 'SWNCM_FRONTEND'. mc_add 'SWNCM_HITLISTDB'.
  mc_add 'SWNCM_HITLISTRT'. mc_add 'SWNCM_MEMORY'.
  mc_add 'SWNCM_RFCCLI'. mc_add 'SWNCM_RFCCLID'. mc_add 'SWNCM_RFCSRV'.
  mc_add 'SWNCM_RFCSRVD'. mc_add 'SWNCM_SPOOLACT'.
  mc_add 'SWNCM_TABREC'. mc_add 'SWNCM_TASKTIMES'.
  mc_add 'SWNCM_TASKTYPE'. mc_add 'SWNCM_TCDET'. mc_add 'SWNCM_TIMES'.
  mc_add 'SWNCM_TREX'. mc_add 'SWNCM_USERTCODE'.
  mc_add 'SWNCM_USERWLOAD'. mc_add 'SWNCM_VMC'. mc_add 'SWNCM_WEBC'.
  mc_add 'SWNCM_WEBCD'. mc_add 'SWNCM_WEBS'. mc_add 'SWNCM_WEBSD'.
  mc_add 'SWNCMONI'. mc_add 'SWNCMONIINDEX'. mc_add 'SWNCUI_SEL'.
  mc_add 'SWNUWLTIMESTTEST'. mc_add 'SWO_SEMAPH'. mc_add 'SWOT21'.
  mc_add 'SWOTDI'. mc_add 'SWOTDO'. mc_add 'SWOTDQ'. mc_add 'SWOTDV'.
  mc_add 'SWOTICE'. mc_add 'SWOTIP'. mc_add 'SWOTLI'. mc_add 'SWOTLQ'.
  mc_add 'SWOTLV'. mc_add 'SWOTOL'. mc_add 'SWOTOLE'. mc_add 'SWOTOO'.
  mc_add 'SWOTTO'. mc_add 'SWOTTOLE'. mc_add 'SWOTTQ'. mc_add 'SWOTTV'.
  mc_add 'SWR_BPML'. mc_add 'SWT_ACT'. mc_add 'SWT_ACTT'.
  mc_add 'SWU_CHKF'. mc_add 'SWU_CHKO'. mc_add 'SWU_CONT1'.
  mc_add 'SWU_FORM'. mc_add 'SWU_LOIO_D'. mc_add 'SWU_LOPR'.
  mc_add 'SWU_PHF'. mc_add 'SWU_PHIO_D'. mc_add 'SWU_PHPR'.
  mc_add 'SWU_PHRE'. mc_add 'SWU5_DYNP'. mc_add 'SWUOCHKF'.
  mc_add 'SWUOCHKO'. mc_add 'SWUOCONT1'. mc_add 'SWUOLOIO'.
  mc_add 'SWUOLOIOT'. mc_add 'SWUOLOPR'. mc_add 'SWUOLORE'.
  mc_add 'SWUOLORI'. mc_add 'SWUOPHF'. mc_add 'SWUOPHHR'.
  mc_add 'SWUOPHIO'. mc_add 'SWUOPHNM'. mc_add 'SWUOPHPR'.
  mc_add 'SWUOPHRE'. mc_add 'SWUOPHRI'. mc_add 'SWUORE'.
  mc_add 'SWUOREPR'. mc_add 'SWWDHSTEXT'. mc_add 'SWWFBATEXT'.
  mc_add 'SWWMIGCNT'. mc_add 'SWWRUNCNT'. mc_add 'SWWSTATEXT'.
  mc_add 'SWWSTATUS'. mc_add 'SWWTYPTEXT'. mc_add 'SXBNFNODE'.
  mc_add 'SXBNFROLE'. mc_add 'SXBNFRULE'. mc_add 'SXC_ATTR'.
  mc_add 'SXC_ATTRT'. mc_add 'SXC_CLASS'. mc_add 'SXC_COCO'.
  mc_add 'SXC_EXIT'. mc_add 'SXC_FCODE'. mc_add 'SXC_FCODET'.
  mc_add 'SXC_IMPSWH'. mc_add 'SXC_SCRN'. mc_add 'SXC_TABLE'.
  mc_add 'SXC_TABLET'. mc_add 'SXCC_ACT'. mc_add 'SXCC_FLT'.
  mc_add 'SXDA0'. mc_add 'SXDA0T'. mc_add 'SXDA1'. mc_add 'SXDA1T'.
  mc_add 'SXDA2'. mc_add 'SXDA2T'. mc_add 'SXDA3'.
  mc_add 'SXI_AC_CACHE_IS'. mc_add 'SXIPERF_INDX'.
  mc_add 'SXIPERF_REG_CFG'. mc_add 'SXIPERF_REG_JOB'.
  mc_add 'SXIPERF_REG_LGC'. mc_add 'SXIPERF_REG_LGS'.
  mc_add 'SXIPERF_REG_LOG'. mc_add 'SXIPERF_REG_RSA'.
  mc_add 'SXIPERF_REG_RSR'. mc_add 'SXIPERF_REG_STA'.
  mc_add 'SXIPERF_REG_STJ'. mc_add 'SXIPERF_REG_STR'.
  mc_add 'SXIPERF_REG_TAB'. mc_add 'SXIPERF_REG_TCC'.
  mc_add 'SXIPERF_REG_TGA'. mc_add 'SXIPERF_REG_TGC'.
  mc_add 'SXIPERFWSLOG'. mc_add 'SXISWCV'. mc_add 'SXIVERI_MSGSTORE'.
  mc_add 'SXMS_AE_AUDITCNF'. mc_add 'SXMS_BCKGND_JOBS'.
  mc_add 'SXMS_SEQUENCE'. mc_add 'SXMS_TCMP_TEST'.
  mc_add 'SXMSAEADPMOD'. mc_add 'SXMSAEADPMODCHN'. mc_add 'SXMSAEAGG'.
  mc_add 'SXMSAERAW'. mc_add 'SXMSAGGERRLABEL'.
  mc_add 'SXMSAGGERRLBLTXT'. mc_add 'SXMSAGGERRMAP'.
  mc_add 'SXMSAGGMAP'. mc_add 'SXMSAGGPERIOD'. mc_add 'SXMSAGGPROF'.
  mc_add 'SXMSAGGPROFR'. mc_add 'SXMSAGGPROFT'.
  mc_add 'SXMSALERTCONTROL'. mc_add 'SXMSALERTRULES'.
  mc_add 'SXMSALERTSTEPS'. mc_add 'SXMSCONFDF'. mc_add 'SXMSDCONF'.
  mc_add 'SXMSEPBUFFER'. mc_add 'SXMSFTEST'. mc_add 'SXMSGLOBAL'.
  mc_add 'SXMSHELP'. mc_add 'SXMSHELPT'. mc_add 'SXMSINTF'.
  mc_add 'SXMSINTFT'. mc_add 'SXMSITF'. mc_add 'SXMSJINFO'.
  mc_add 'SXMSMONSEL'. mc_add 'SXMSMONSET'. mc_add 'SXMSMSTAT'.
  mc_add 'SXMSMSTATT'. mc_add 'SXMSPADM'. mc_add 'SXMSPCONF'.
  mc_add 'SXMSPERFT'. mc_add 'SXMSPFADDRESS'. mc_add 'SXMSPFAGG'.
  mc_add 'SXMSPFCOMPONENT'. mc_add 'SXMSPFRAWD'. mc_add 'SXMSPFRAWH'.
  mc_add 'SXMSPIPE'. mc_add 'SXMSPIPEEL'. mc_add 'SXMSPIPET'.
  mc_add 'SXMSPLELT'. mc_add 'SXMSPLSRV'. mc_add 'SXMSPLSRVT'.
  mc_add 'SXMSPMIRAW'. mc_add 'SXMSQUEUESTATUS'. mc_add 'SXMSRECVT'.
  mc_add 'SXMSSPDIAGS'. mc_add 'SXMSSYERR'. mc_add 'SXMSSYERRT'.
  mc_add 'SXMSTESTCHECK'. mc_add 'SXMSTRANS_CONFIG'.
  mc_add 'SXMSTRC_DAT'. mc_add 'SXMSTRC_SEL'. mc_add 'SXMSTSACT'.
  mc_add 'SXMSTSTRIG'. mc_add 'SXPGBTCINF'. mc_add 'SXPGCOSTAB'.
  mc_add 'SXPGCOTABE'. mc_add 'SXPGHISTOR'. mc_add 'SXS_ATTR'.
  mc_add 'SXS_ATTRT'. mc_add 'SXS_COCO'. mc_add 'SXS_FCODE'.
  mc_add 'SXS_FCODET'. mc_add 'SXS_INTER'. mc_add 'SXS_MLCO'.
  mc_add 'SXS_SCRN'. mc_add 'SXS_SCRNT'. mc_add 'SXS_STAT'.
  mc_add 'SXS_TABLE'. mc_add 'SXS_TABLET'. mc_add 'SXSLTERR'.
  mc_add 'SXVIEW_MOCK_COL'. mc_add 'SXVIEW_MOCK_MAIN'.
  mc_add 'SYBTRACE'. mc_add 'SYBUCMAPPING'. mc_add 'SYCH_VERSION'.
  mc_add 'SYCH_VERSION_T'. mc_add 'SYCM_CUST_HEAD'.
  mc_add 'SYCM_CUST_REFS'. mc_add 'SYCM_CUST_SIMPL'.
  mc_add 'SYCM_CUST_STATE'. mc_add 'SYCM_JOB_DATA_ID'.
  mc_add 'SYCM_PIECELIST'. mc_add 'SYCM_PLIST_HEAD'.
  mc_add 'SYCM_PLIST_PARAM'. mc_add 'SYCM_PLST_SUBOBJ'.
  mc_add 'SYCM_SAP_SIMPL'. mc_add 'SYCM_SITEM'.
  mc_add 'SYCM_SITEM_AREAS'. mc_add 'SYCM_SITEM_ENPOI'.
  mc_add 'SYCM_SITEM_PACKS'. mc_add 'SYCM_SITEM_PLIST'.
  mc_add 'SYCM_SITEM_STATS'. mc_add 'SYCM_UPL'. mc_add 'SYS_MENU'.
  mc_add 'SYSCOMP'. mc_add 'SYSCOMP_DATA'. mc_add 'SYSCOMP_RUN'.
  mc_add 'SYSTABSTMT'. mc_add 'SZDM_AIMREP'. mc_add 'SZDM_CC_CFG_CUST'.
  mc_add 'SZDM_CC_CFG_SAP'. mc_add 'SZDM_CC_CR_REPO'.
  mc_add 'SZDM_CC_CR_TIME'. mc_add 'SZDM_CC_CR_VECTR'.
  mc_add 'SZDM_CC_CR_VERS'. mc_add 'SZDM_CC_CUSTOMER'.
  mc_add 'SZDM_CC_ERR_LOG'. mc_add 'SZDM_CC_FUPD_TAB'.
  mc_add 'SZDM_CC_HUGE_TAB'. mc_add 'SZDM_CC_LR_TIME'.
  mc_add 'SZDM_CC_MSGCONTD'. mc_add 'SZDM_CC_MSGCONTT'.
  mc_add 'SZDM_CC_NAMETAB'. mc_add 'SZDM_CC_PRD_TABS'.
  mc_add 'SZDM_CC_PUTTBX'. mc_add 'SZDM_CC_PUTTBX_Z'.
  mc_add 'SZDM_CC_TAB_PKG'. mc_add 'SZDM_CC_TAB_STAT'.
  mc_add 'SZDM_CC_UPG_EVT'. mc_add 'SZDM_CCL_CHG_TAB'.
  mc_add 'SZDM_CCL_OBJECT'. mc_add 'SZDM_CCL_OBJH'.
  mc_add 'SZDM_CCL_OBJM'. mc_add 'SZDM_CCL_OBJMT'.
  mc_add 'SZDM_CCL_OBJS'. mc_add 'SZDM_CCL_OBJSL'.
  mc_add 'SZDM_CCL_TADIRTJ'. mc_add 'SZDM_TRACE'.
  mc_add 'SZDM_TRACE_META'. mc_add 'SZDM_TRACE_ST05'. mc_add 'T000'.
  mc_add 'T002'. mc_add 'T002C'. mc_add 'T002T'. mc_add 'T002TX'.
  mc_add 'T002X'. mc_add 'T002XT'. mc_add 'T022A'. mc_add 'T022B'.
  mc_add 'T022D'. mc_add 'T022Q'. mc_add 'T100'. mc_add 'T100A'.
  mc_add 'T100O'. mc_add 'T100T'. mc_add 'T100U'. mc_add 'T100V'.
  mc_add 'T100X'. mc_add 'T164A'. mc_add 'T164C'. mc_add 'T164O'.
  mc_add 'T164P'. mc_add 'T164U'. mc_add 'T164Y'. mc_add 'T246'.
  mc_add 'T247'. mc_add 'T248'. mc_add 'T777D'. mc_add 'T777DITOPER'.
  mc_add 'T777IBO'. mc_add 'T777IFPROPS'. mc_add 'T779W'.
  mc_add 'T779X'. mc_add 'T77ACOL'. mc_add 'T77ACOLINI'. mc_add 'T77AD'.
  mc_add 'T77ARRAYTP'. mc_add 'T77ARRAYTT'. mc_add 'T77CDOC'.
  mc_add 'T77COL'. mc_add 'T77COLHEAD'. mc_add 'T77COLHTYP'.
  mc_add 'T77COLOTYP'. mc_add 'T77COLT'. mc_add 'T77FL'.
  mc_add 'T77FLC'. mc_add 'T77FLT'. mc_add 'T77FNODORD'.
  mc_add 'T77FNODORT'. mc_add 'T77FOBJMAN'. mc_add 'T77FRIEND'.
  mc_add 'T77FRIENDT'. mc_add 'T77FSEAA'. mc_add 'T77FSEAN'.
  mc_add 'T77FSEANT'. mc_add 'T77FSEAS'. mc_add 'T77FSEAST'.
  mc_add 'T77FSEAT'. mc_add 'T77FSEATGB'. mc_add 'T77FSEATT'.
  mc_add 'T77GC'. mc_add 'T77GF'. mc_add 'T77GMFO'. mc_add 'T77GMFS'.
  mc_add 'T77GMOTYPE'. mc_add 'T77GMOTYPT'. mc_add 'T77GMS'.
  mc_add 'T77GMSG'. mc_add 'T77GMSO'. mc_add 'T77GMST'. mc_add 'T77GT'.
  mc_add 'T77GV'. mc_add 'T77HIERATP'. mc_add 'T77ID'. mc_add 'T77ITEX'.
  mc_add 'T77LA'. mc_add 'T77LC'. mc_add 'T77LF'. mc_add 'T77LT'.
  mc_add 'T77LV'. mc_add 'T77MC'. mc_add 'T77MWBBWS'. mc_add 'T77MWBD'.
  mc_add 'T77MWBDT'. mc_add 'T77MWBDTC'. mc_add 'T77MWBFCC'.
  mc_add 'T77MWBFCD'. mc_add 'T77MWBFCH'. mc_add 'T77MWBFCO'.
  mc_add 'T77MWBFCT'. mc_add 'T77MWBFHC'. mc_add 'T77MWBFTC'.
  mc_add 'T77MWBK'. mc_add 'T77MWBKS'. mc_add 'T77MWBS'.
  mc_add 'T77MWBSC'. mc_add 'T77MWBT'. mc_add 'T77NI'.
  mc_add 'T77OBJDEF'. mc_add 'T77OBJSER'. mc_add 'T77OMATGT'.
  mc_add 'T77OMATTDB'. mc_add 'T77OMBUFEV'. mc_add 'T77OMFRAM'.
  mc_add 'T77OMFRAS'. mc_add 'T77OMFRAST'. mc_add 'T77OMGTJ'.
  mc_add 'T77OMGTJF'. mc_add 'T77OMIA'. mc_add 'T77OMICON'.
  mc_add 'T77OMICONT'. mc_add 'T77OMIFDET'. mc_add 'T77OMIFGT'.
  mc_add 'T77OMIFOVE'. mc_add 'T77OMIFSTA'. mc_add 'T77OMIFTIM'.
  mc_add 'T77OMKEY_A'. mc_add 'T77OMKEY_C'. mc_add 'T77OMREG_T'.
  mc_add 'T77OMREGIS'. mc_add 'T77OMSCEN'. mc_add 'T77OMSCENF'.
  mc_add 'T77OMSCENT'. mc_add 'T77OMTABS'. mc_add 'T77OMTABST'.
  mc_add 'T77OMTABUS'. mc_add 'T77ZA'. mc_add 'T77ZC'. mc_add 'T77ZT'.
  mc_add 'T77ZZ'. mc_add 'T787I'. mc_add 'T787O'. mc_add 'T787T'.
  mc_add 'T787U'. mc_add 'T787V'. mc_add 'T788E'. mc_add 'T788O'.
  mc_add 'T788T'. mc_add 'T788U'. mc_add 'T788V'. mc_add 'T78NR'.
  mc_add 'T78NRI'. mc_add 'T9COM'. mc_add 'T9DEV'. mc_add 'T9PRO'.
  mc_add 'TABDIRDEVC'. mc_add 'TABDIRLANG'. mc_add 'TABDIRORIG'.
  mc_add 'TABDYNPS'. mc_add 'TABREFINE'. mc_add 'TACO1'. mc_add 'TACOB'.
  mc_add 'TACT'. mc_add 'TACTT'. mc_add 'TACTZ'. mc_add 'TADB2'.
  mc_add 'TADB6'. mc_add 'TADBM'. mc_add 'TADIR'. mc_add 'TADIR_MOD'.
  mc_add 'TADIR_TMP'. mc_add 'TADMI'. mc_add 'TADMT'.
  mc_add 'TAG_PROVIDER'. mc_add 'TAG_PROVIDERT'. mc_add 'TAGATTT'.
  mc_add 'TAGLIBT'. mc_add 'TAGT'. mc_add 'TAINF'. mc_add 'TALIM'.
  mc_add 'TAM0T'. mc_add 'TAME0'. mc_add 'TAMO_STATUS'. mc_add 'TAMSS'.
  mc_add 'TAORA'. mc_add 'TAPLP'. mc_add 'TAPLT'. mc_add 'TAPPL'.
  mc_add 'TAPPL_LOCK'. mc_add 'TAPPLCLASS'. mc_add 'TAPPLCLAST'.
  mc_add 'TAQTS'. mc_add 'TARCH01'. mc_add 'TARCH01R'.
  mc_add 'TARCH01T'. mc_add 'TAREF'. mc_add 'TASK_VITA_RUN'.
  mc_add 'TASYBNEW'. mc_add 'TASYS'. mc_add 'TATAF'. mc_add 'TATAF_MIG'.
  mc_add 'TATAF_S'. mc_add 'TATAF_SAV'. mc_add 'TATAF_SHD'.
  mc_add 'TATAF_STD'. mc_add 'TATRADT_SQLTRACE'.
  mc_add 'TATRAPI_FILES2'. mc_add 'TAUNIT'. mc_add 'TAUNIT_KEY'.
  mc_add 'TAUNIT_META_DATA'. mc_add 'TAUNIT_PROG_INFO'.
  mc_add 'TAUNIT_SRV_SETUP'. mc_add 'TAUTL'. mc_add 'TB039'.
  mc_add 'TB055'. mc_add 'TB055A'. mc_add 'TB055T'. mc_add 'TBATG'.
  mc_add 'TBATG_MIG'. mc_add 'TBATG_SAV'. mc_add 'TBATG_SHD'.
  mc_add 'TBATG_STD'. mc_add 'TBCICPT1'. mc_add 'TBCS_RECIP'.
  mc_add 'TBD10'. mc_add 'TBD11'. mc_add 'TBD12'. mc_add 'TBD13'.
  mc_add 'TBD14'. mc_add 'TBD16'. mc_add 'TBD17'. mc_add 'TBD18'.
  mc_add 'TBD19'. mc_add 'TBD21'. mc_add 'TBD22'. mc_add 'TBD23'.
  mc_add 'TBD24'. mc_add 'TBD32'. mc_add 'TBD33'. mc_add 'TBD51'.
  mc_add 'TBD53'. mc_add 'TBD54'. mc_add 'TBDBA'. mc_add 'TBDBDLSALL'.
  mc_add 'TBDBDLSUARG'. mc_add 'TBDBDLSUKEY'. mc_add 'TBDBDLSUMIX'.
  mc_add 'TBDBE'. mc_add 'TBDBH'. mc_add 'TBDBRMF'. mc_add 'TBDBS'.
  mc_add 'TBDFA'. mc_add 'TBDLS'. mc_add 'TBDLST'. mc_add 'TBDME'.
  mc_add 'TBDMS'. mc_add 'TBDSS'. mc_add 'TBDTP'. mc_add 'TBDTPB'.
  mc_add 'TBDTPBDF'. mc_add 'TBDTPBDR'. mc_add 'TBDTPBDRF'.
  mc_add 'TBDTPBR'. mc_add 'TBDTPM'. mc_add 'TBDTPMD'. mc_add 'TBDTPP'.
  mc_add 'TBDTPPT'. mc_add 'TBDTPT'. mc_add 'TBDTPV'. mc_add 'TBDTPVT'.
  mc_add 'TBEKR'. mc_add 'TBER'. mc_add 'TBERX'.
  mc_add 'TBIZC_CONF_STAT'. mc_add 'TBIZC_CPROV_REG'.
  mc_add 'TBIZC_CPROV_SOLM'. mc_add 'TBOOKSHOP'. mc_add 'TBPROGREF'.
  mc_add 'TBRF000'. mc_add 'TBRF000T'. mc_add 'TBRF181L'.
  mc_add 'TBRF192'. mc_add 'TBRF193'. mc_add 'TBRF193T'.
  mc_add 'TBRF330'. mc_add 'TBRF330T'. mc_add 'TBRF331'.
  mc_add 'TBRF331T'. mc_add 'TBRG_AUTH'. mc_add 'TBRG_AUTHT'.
  mc_add 'TBRG_OBJ'. mc_add 'TBSP1'. mc_add 'TBSPCSETC'.
  mc_add 'TBSPJSDBG'. mc_add 'TBTC_CHAIN'. mc_add 'TBTC_EXT_SDL'.
  mc_add 'TBTC_RUN_STAT'. mc_add 'TBTC_RUN_WHITE'.
  mc_add 'TBTC_SPOOLID'. mc_add 'TBTC_TASK'. mc_add 'TBTC_TASK_DET'.
  mc_add 'TBTCA'. mc_add 'TBTCB'. mc_add 'TBTCCNTXT'.
  mc_add 'TBTCCTXTT'. mc_add 'TBTCCTXTTP'. mc_add 'TBTCI'.
  mc_add 'TBTCJOBLOG'. mc_add 'TBTCJOBLOG0'. mc_add 'TBTCJOBLOG1'.
  mc_add 'TBTCJOBLOG2'. mc_add 'TBTCJOBLOG3'. mc_add 'TBTCJOBLOG4'.
  mc_add 'TBTCJOBLOG5'. mc_add 'TBTCJOBLOG6'. mc_add 'TBTCJOBLOG7'.
  mc_add 'TBTCJOBLOG8'. mc_add 'TBTCJOBLOG9'. mc_add 'TBTCJSTEP'.
  mc_add 'TBTCO'. mc_add 'TBTCP'. mc_add 'TBTCR'. mc_add 'TBTCS'.
  mc_add 'TBTCY'. mc_add 'TBTFFONTMAP'. mc_add 'TBTFFONTMAPDEF'.
  mc_add 'TBVIEWER'. mc_add 'TCAFTJTRG'. mc_add 'TCALS'.
  mc_add 'TCBRCUSTOM'. mc_add 'TCBRGROUP'. mc_add 'TCBRGROUPT'.
  mc_add 'TCDATETRG'. mc_add 'TCDCOUPLES'. mc_add 'TCDCOUPLES_S'.
  mc_add 'TCDIR'. mc_add 'TCDOB'. mc_add 'TCDOBS'. mc_add 'TCDOBT'.
  mc_add 'TCDOBTS'. mc_add 'TCDRP'. mc_add 'TCDRPS'. mc_add 'TCDRS'.
  mc_add 'TCECLILY'. mc_add 'TCECPSTAT'. mc_add 'TCEDELI'.
  mc_add 'TCEDEPLOYTARGETS'. mc_add 'TCEDP21'. mc_add 'TCEQUAS'.
  mc_add 'TCERELE'. mc_add 'TCESYST'. mc_add 'TCESYSTT'.
  mc_add 'TCETARG'. mc_add 'TCETARGHDR'. mc_add 'TCETARGT'.
  mc_add 'TCETESTTARG'. mc_add 'TCETRAL'. mc_add 'TCETRALT'.
  mc_add 'TCEVERS'. mc_add 'TCEVERST'. mc_add 'TCEVNTTRG'.
  mc_add 'TCHLP'. mc_add 'TCMP'. mc_add 'TCMP_DAT'.
  mc_add 'TCMP_DEBUG_BPS'. mc_add 'TCMP_DEBUG_TOOL'.
  mc_add 'TCMP_DEBUG_TRANS'. mc_add 'TCMP_DEBUG_X'. mc_add 'TCMP_SUTY'.
  mc_add 'TCMP_TEMPLATE_C'. mc_add 'TCMP_TEMPLATE_H'.
  mc_add 'TCMP_TYPE'. mc_add 'TCMP_WB_MARKER'. mc_add 'TCNETTASK'.
  mc_add 'TCNTASKREL'. mc_add 'TCNV'. mc_add 'TCNVCAUSE'.
  mc_add 'TCNVDATA'. mc_add 'TCODE_MOD'. mc_add 'TCOLL'. mc_add 'TCONT'.
  mc_add 'TCOPMDTRG'. mc_add 'TCP00'. mc_add 'TCP00A'. mc_add 'TCP01'.
  mc_add 'TCP02'. mc_add 'TCP03'. mc_add 'TCP04'. mc_add 'TCP05'.
  mc_add 'TCP06'. mc_add 'TCP07'. mc_add 'TCP08'. mc_add 'TCP09'.
  mc_add 'TCP0A'. mc_add 'TCP0C'. mc_add 'TCP0D'. mc_add 'TCP0F'.
  mc_add 'TCP0G'. mc_add 'TCP0H'. mc_add 'TCP0I'. mc_add 'TCP0IA'.
  mc_add 'TCP11'. mc_add 'TCPCHARSET'. mc_add 'TCPDB'.
  mc_add 'TCPFILREAD'. mc_add 'TCPIC'. mc_add 'TCPLANG'.
  mc_add 'TCPLANGGRP'. mc_add 'TCPSATTR'. mc_add 'TCPSBUILD'.
  mc_add 'TCPSCHEMA'. mc_add 'TCPSEG'. mc_add 'TCPSPTL'.
  mc_add 'TCPSPTS'. mc_add 'TCPSRULE'. mc_add 'TCPT1'. mc_add 'TCPT2'.
  mc_add 'TCPUC'. mc_add 'TCPUCATTR'. mc_add 'TCS_GSETID'.
  mc_add 'TCS_GSETTING'. mc_add 'TCSTC'. mc_add 'TCTM_AT_PR'.
  mc_add 'TCTM_ATTR'. mc_add 'TCTM_ATTRT'. mc_add 'TCTM_AV_PR'.
  mc_add 'TCTM_COTY'. mc_add 'TCTM_COTYT'. mc_add 'TCTM_LCVAL'.
  mc_add 'TCTM_LOOB'. mc_add 'TCTM_LPVAL'. mc_add 'TCTM_PAAT'.
  mc_add 'TCTM_PCR'. mc_add 'TCTM_PHOB'. mc_add 'TCTM_PPV'.
  mc_add 'TCUPOSN'. mc_add 'TCURX'. mc_add 'TCUS_XREF'.
  mc_add 'TCUS_XREF4'. mc_add 'TCUSCUST'. mc_add 'TCUSDAT'.
  mc_add 'TCUSFUBA'. mc_add 'TCUSGENINF'. mc_add 'TCUSGLOBAL'.
  mc_add 'TCUSHIST'. mc_add 'TCUSINF'. mc_add 'TCUSINFT'.
  mc_add 'TCUSINFU'. mc_add 'TCUSNOTES'. mc_add 'TCUSP'.
  mc_add 'TCUSP40'. mc_add 'TCUSP40T'. mc_add 'TCUSPT'.
  mc_add 'TCUSPXREF'. mc_add 'TCUSPXREF4'. mc_add 'TCUSQ'.
  mc_add 'TCUSR'. mc_add 'TCUSR40'. mc_add 'TCUSS'. mc_add 'TCUSSYSL'.
  mc_add 'TCUSTEMP'. mc_add 'TCUSTEMPC'. mc_add 'TCUSTEMPT'.
  mc_add 'TCUSTYPTXT'. mc_add 'TCUSU'. mc_add 'TCWDAYTRG'.
  mc_add 'TDATS'. mc_add 'TDCLD'. mc_add 'TDCLT'. mc_add 'TDCT'.
  mc_add 'TDDAT'. mc_add 'TDDIR'. mc_add 'TDDIRS'. mc_add 'TDEVC'.
  mc_add 'TDEVCT'. mc_add 'TDEVOBJCL'. mc_add 'TDEVOBJP'.
  mc_add 'TDI01'. mc_add 'TDKZ'. mc_add 'TDM01'. mc_add 'TDM02'.
  mc_add 'TDM03'. mc_add 'TDM04'. mc_add 'TDM05'. mc_add 'TDM06'.
  mc_add 'TDM07'. mc_add 'TDM08'. mc_add 'TDM09'. mc_add 'TDM10'.
  mc_add 'TDM11'. mc_add 'TDM12'. mc_add 'TDM13'. mc_add 'TDM14'.
  mc_add 'TDM19'. mc_add 'TDM20'. mc_add 'TDM21'. mc_add 'TDM22'.
  mc_add 'TDM23'. mc_add 'TDM25'. mc_add 'TDM26'. mc_add 'TDM27'.
  mc_add 'TDM28'. mc_add 'TDM29'. mc_add 'TDM30'. mc_add 'TDM31'.
  mc_add 'TDM99'. mc_add 'TDMFC'. mc_add 'TDMV1'. mc_add 'TDOCU'.
  mc_add 'TDOKU'. mc_add 'TDOKUIMG'. mc_add 'TDOKUIMGR'.
  mc_add 'TDOKUIMGT'. mc_add 'TDSGH'. mc_add 'TDSGL'. mc_add 'TDSIU'.
  mc_add 'TDSOB'. mc_add 'TDSOF'. mc_add 'TDSOT'. mc_add 'TDSYH'.
  mc_add 'TDSYI'. mc_add 'TDSYL'. mc_add 'TDSYT'. mc_add 'TDUMY'.
  mc_add 'TDXLOADP'. mc_add 'TDXTTPT'. mc_add 'TDXTTPT_T'.
  mc_add 'TE07F'. mc_add 'TECHED02_COLORS'. mc_add 'TECHED02_NUMBERS'.
  mc_add 'TECHED02_UNICODE'. mc_add 'TECHED03_COLORS'.
  mc_add 'TECHED03_RESULT1'. mc_add 'TECHED03_RESULT2'.
  mc_add 'TECHED03_UTEXT'. mc_add 'TECHUSAGES'. mc_add 'TEDCT'.
  mc_add 'TEDE3'. mc_add 'TEDS1'. mc_add 'TEDS2'. mc_add 'TEDS3'.
  mc_add 'TEDST'. mc_add 'TEDTT'. mc_add 'TEEF_MIG_STATE'.
  mc_add 'TEMSG'. mc_add 'TEMSGU'. mc_add 'TEMSI'. mc_add 'TENABP'.
  mc_add 'TEPSCUS'. mc_add 'TEPSDELI'. mc_add 'TEPSIN'.
  mc_add 'TEPSNUM'. mc_add 'TEPSOUT'. mc_add 'TEQUIVDOM'.
  mc_add 'TEQUIVDOMT'. mc_add 'TER10'. mc_add 'TER11'. mc_add 'TER12'.
  mc_add 'TER13'. mc_add 'TER14'. mc_add 'TER15'. mc_add 'TER16'.
  mc_add 'TER17'. mc_add 'TERCL'. mc_add 'TERCL2'. mc_add 'TERCL3'.
  mc_add 'TERM'. mc_add 'TERM1'. mc_add 'TERM2'. mc_add 'TERM3'.
  mc_add 'TERM4'. mc_add 'TERM5'. mc_add 'TERM6'. mc_add 'TERM7'.
  mc_add 'TERM8'. mc_add 'TERM9'. mc_add 'TERMB'. mc_add 'TERMC1'.
  mc_add 'TERMC2'. mc_add 'TERMC3'. mc_add 'TERMP'. mc_add 'TERMU'.
  mc_add 'TEST932DUP'. mc_add 'TESTCIAEW'. mc_add 'TESTCIAEWF'.
  mc_add 'TESTCIAEWZ'. mc_add 'TESTCLUST1'. mc_add 'TESTHCH4'.
  mc_add 'TETYP'. mc_add 'TETYT'. mc_add 'TEUDB'. mc_add 'TEXC_DE'.
  mc_add 'TEXC_DE_C'. mc_add 'TEXC_DOM'. mc_add 'TEXC_DOM_C'.
  mc_add 'TEXC_FLD'. mc_add 'TEXC_FLD_C'. mc_add 'TEXT_T_CATALOG'.
  mc_add 'TEXTLOG'. mc_add 'TEXTTRL'. mc_add 'TFACD'. mc_add 'TFACS'.
  mc_add 'TFACT'. mc_add 'TFAIN'. mc_add 'TFAIT'. mc_add 'TFARSIMONTH'.
  mc_add 'TFAWT'. mc_add 'TFBKT'. mc_add 'TFBLK'. mc_add 'TFDIR'.
  mc_add 'TFDIR_INIM'. mc_add 'TFIA'. mc_add 'TFIAT'. mc_add 'TFIC'.
  mc_add 'TFICT'. mc_add 'TFIT'. mc_add 'TFITH'. mc_add 'TFITT'.
  mc_add 'TFKT'. mc_add 'TFM01'. mc_add 'TFM02'. mc_add 'TFM03'.
  mc_add 'TFM04'. mc_add 'TFM05'. mc_add 'TFM06'. mc_add 'TFM07'.
  mc_add 'TFM08'. mc_add 'TFM09'. mc_add 'TFM10'. mc_add 'TFM11'.
  mc_add 'TFM12'. mc_add 'TFM13'. mc_add 'TFM14'. mc_add 'TFM16'.
  mc_add 'TFM18'. mc_add 'TFM18X'. mc_add 'TFO_BUFFER'. mc_add 'TFO01'.
  mc_add 'TFO02'. mc_add 'TFO03'. mc_add 'TFO04'. mc_add 'TFO05'.
  mc_add 'TFO06'. mc_add 'TFO07'. mc_add 'TFO08'. mc_add 'TFO09'.
  mc_add 'TFO10'. mc_add 'TFO11'. mc_add 'TFO12'. mc_add 'TFO13'.
  mc_add 'TFO14'. mc_add 'TFO15'. mc_add 'TFO16'. mc_add 'TFO17'.
  mc_add 'TFO18'. mc_add 'TFORM01'. mc_add 'TFORM01R'.
  mc_add 'TFORM01T'. mc_add 'TFRT_BLOB'. mc_add 'TFRT_HISTORY'.
  mc_add 'TFRT_INFO'. mc_add 'TFRT_NAME'. mc_add 'TFRT_SRCH_CHARC'.
  mc_add 'TFRT_SRCH_CHARC1'. mc_add 'TFRT_SRCH_CHARCG'.
  mc_add 'TFRT_SRCH_CHARCM'. mc_add 'TFRT_SRCH_CHART'.
  mc_add 'TFRT_SRCH_CHART1'. mc_add 'TFRT_SRCH_CHARTG'.
  mc_add 'TFRT_SRCH_CHARTM'. mc_add 'TFRT_SRCH_DATA'.
  mc_add 'TFRT_SRCH_KEY'. mc_add 'TFRT_USER'. mc_add 'TFTIT'.
  mc_add 'TGDB2'. mc_add 'TGINF'. mc_add 'TGLIF'. mc_add 'TGMSS'.
  mc_add 'TGORA'. mc_add 'TGPM'. mc_add 'TGPMT'. mc_add 'TGPN'.
  mc_add 'TGPNT'. mc_add 'TGRAPH'. mc_add 'TGRAPHDEVC'. mc_add 'TGSU0'.
  mc_add 'TGSU1'. mc_add 'TGSU2'. mc_add 'TGSU3'. mc_add 'TGSU4'.
  mc_add 'TGSU5'. mc_add 'TGSU6'. mc_add 'TGSU7'. mc_add 'TGSUF'.
  mc_add 'TGSUI'. mc_add 'TGSUO'. mc_add 'TGSUS'. mc_add 'TGSUT'.
  mc_add 'TGTB'. mc_add 'TGTY'. mc_add 'TGTYT'. mc_add 'TGUI'.
  mc_add 'THELP'. mc_add 'THELPI'. mc_add 'THEXC'. mc_add 'THEXS'.
  mc_add 'THLDYNS'. mc_add 'THLPC'. mc_add 'THLPF'. mc_add 'THLPG'.
  mc_add 'THLPM'. mc_add 'THLPV'. mc_add 'THLPX'. mc_add 'THOC'.
  mc_add 'THOCD'. mc_add 'THOCI'. mc_add 'THOCS'. mc_add 'THOCT'.
  mc_add 'THOL'. mc_add 'THOLT'. mc_add 'THOLU'. mc_add 'THOST'.
  mc_add 'THSTC'. mc_add 'THSTM'. mc_add 'TICNVDB4'.
  mc_add 'TICNVPTABS'. mc_add 'TILM_STOR'. mc_add 'TILM_STOR_AL'.
  mc_add 'TILM_STOR_AUDIT'. mc_add 'TILM_STOR_BLOB'.
  mc_add 'TILM_STOR_CERT'. mc_add 'TILM_STOR_CL_ADM'.
  mc_add 'TILM_STOR_CL_OA'. mc_add 'TILM_STOR_CLIENT'.
  mc_add 'TILM_STOR_COL'. mc_add 'TILM_STOR_CUS'.
  mc_add 'TILM_STOR_CUS_A'. mc_add 'TILM_STOR_CUS_AT'.
  mc_add 'TILM_STOR_CUS_O'. mc_add 'TILM_STOR_CUS_OT'.
  mc_add 'TILM_STOR_CUS_P'. mc_add 'TILM_STOR_CUS_PT'.
  mc_add 'TILM_STOR_EVENT'. mc_add 'TILM_STOR_EVENTC'.
  mc_add 'TILM_STOR_EVENTR'. mc_add 'TILM_STOR_HDP_A'.
  mc_add 'TILM_STOR_HDP_AT'. mc_add 'TILM_STOR_HDP_C'.
  mc_add 'TILM_STOR_HDP_CO'. mc_add 'TILM_STOR_HDP_CT'.
  mc_add 'TILM_STOR_HDP_PU'. mc_add 'TILM_STOR_MON_A'.
  mc_add 'TILM_STOR_MON_C'. mc_add 'TILM_STOR_MON_CD'.
  mc_add 'TILM_STOR_MON_CE'. mc_add 'TILM_STOR_MON_CR'.
  mc_add 'TILM_STOR_MON_K'. mc_add 'TILM_STOR_MON_KE'.
  mc_add 'TILM_STOR_MON_KH'. mc_add 'TILM_STOR_MON_KL'.
  mc_add 'TILM_STOR_MON_KO'. mc_add 'TILM_STOR_MON_M'.
  mc_add 'TILM_STOR_MON_MO'. mc_add 'TILM_STOR_MON_MT'.
  mc_add 'TILM_STOR_MON_OP'. mc_add 'TILM_STOR_MON_RC'.
  mc_add 'TILM_STOR_MONC'. mc_add 'TILM_STOR_MONCA'.
  mc_add 'TILM_STOR_MONCAT'. mc_add 'TILM_STOR_MONCG'.
  mc_add 'TILM_STOR_MONCGP'. mc_add 'TILM_STOR_MONCGT'.
  mc_add 'TILM_STOR_MONCP'. mc_add 'TILM_STOR_MONCPT'.
  mc_add 'TILM_STOR_MONGEX'. mc_add 'TILM_STOR_POOL'.
  mc_add 'TILM_STOR_PROP'. mc_add 'TILM_STOR_RTI'.
  mc_add 'TILM_STOR_RTM'. mc_add 'TILMSTORRDBLOB_2'.
  mc_add 'TIME_STAT_AWR'. mc_add 'TIMG1'. mc_add 'TIMG2'.
  mc_add 'TIMG3'. mc_add 'TIMG4'. mc_add 'TIMGIR'. mc_add 'TIMGLOIO'.
  mc_add 'TIMIG'. mc_add 'TISLCAL'. mc_add 'TISLMONTH'. mc_add 'TITLE'.
  mc_add 'TK11'. mc_add 'TKEYCL'. mc_add 'TKEYEPR'. mc_add 'TKEYP'.
  mc_add 'TKEYTP'. mc_add 'TKEYTPR'. mc_add 'TLAN_CALE'.
  mc_add 'TLANEXIT'. mc_add 'TLANEXITT'. mc_add 'TLANEXP'.
  mc_add 'TLANEXPPARA'. mc_add 'TLANIMP'. mc_add 'TLANINDX'.
  mc_add 'TLANNOTTA'. mc_add 'TLANOCS'. mc_add 'TLANOCSTA'.
  mc_add 'TLANSUP'. mc_add 'TLANSUPTA'. mc_add 'TLANTEST'.
  mc_add 'TLDA_LOG'. mc_add 'TLDA_MSG'. mc_add 'TLDB'. mc_add 'TLDBT'.
  mc_add 'TLDOC_ICONS'. mc_add 'TLDOC_ITFCHARCON'.
  mc_add 'TLDOC_ITFPARACON'. mc_add 'TLIBG'. mc_add 'TLIBT'.
  mc_add 'TLM_BAS_SCEN'. mc_add 'TLM_BAS_SCENTEXT'.
  mc_add 'TLM_BASSCEN_DEF'. mc_add 'TLM_BASSCENEL'.
  mc_add 'TLM_BASSCENELTEX'. mc_add 'TLM_EXITTYPE'.
  mc_add 'TLM_EXITTYPETEXT'. mc_add 'TLM_PHASES'. mc_add 'TLM_PROC_DEF'.
  mc_add 'TLM_PROC_PARAM'. mc_add 'TLM_PROC_PARAMTE'.
  mc_add 'TLM_PROC_SWCHG_D'. mc_add 'TLM_PROCESSES'.
  mc_add 'TLM_PROCTYPE'. mc_add 'TLM_PROCTYPE_PAT'.
  mc_add 'TLM_PROCTYPETEXT'. mc_add 'TLM_RTO'. mc_add 'TLM_RUNS'.
  mc_add 'TLM_SOLUTION'. mc_add 'TLM_SOLUTIONACT'.
  mc_add 'TLM_SOLUTIONPROC'. mc_add 'TLM_SOLUTIONTEXT'.
  mc_add 'TLM_STEPMESSAGE'. mc_add 'TLM_STEPS'. mc_add 'TLM_STEPTYPE'.
  mc_add 'TLM_STEPTYPETEXT'. mc_add 'TLM_SWCHG_DEF'.
  mc_add 'TLM_VAR_DUMP'. mc_add 'TLM_VAR_DUMP_ARC'.
  mc_add 'TLM_VAR_DUMP_ARV'. mc_add 'TLM_VARIANTS'.
  mc_add 'TLM_VARIANTS_VAL'. mc_add 'TLOCK'. mc_add 'TLOCKCIO'.
  mc_add 'TLOCKP'. mc_add 'TLOCKPC'. mc_add 'TLOCKTST'. mc_add 'TLOGO'.
  mc_add 'TLOGOS'. mc_add 'TLOGOT'. mc_add 'TLSU1'. mc_add 'TLSU2'.
  mc_add 'TLSU3'. mc_add 'TLSU4'. mc_add 'TLSU5'. mc_add 'TLSU6'.
  mc_add 'TLSU7'. mc_add 'TLSUF'. mc_add 'TLSUI'. mc_add 'TLSUT'.
  mc_add 'TLSY2'. mc_add 'TLSY3'. mc_add 'TLSY5'. mc_add 'TLSY7'.
  mc_add 'TLSYOBJ'. mc_add 'TLSYOBJTB'. mc_add 'TLSYS'.
  mc_add 'TLSYTAB'. mc_add 'TMAC'. mc_add 'TMAGE'. mc_add 'TMAS_D_DD'.
  mc_add 'TMAS_D_DFP'. mc_add 'TMAS_D_DFP14'. mc_add 'TMAS_D_DFP7'.
  mc_add 'TMAS_D_DIP'. mc_add 'TMAS_D_DIP14'. mc_add 'TMAS_D_DIP7'.
  mc_add 'TMAS_D_JOB'. mc_add 'TMAS_D_RERR'. mc_add 'TMAS_D_RHD'.
  mc_add 'TMAS_D_RT2007'. mc_add 'TMAS_D_RT2014'.
  mc_add 'TMAS_D_RTD2014'. mc_add 'TMAS_D_TMHOST'.
  mc_add 'TMAS_D_TMMEM'. mc_add 'TMAS_D_TMPRIMARY'.
  mc_add 'TMAS_D_TMUSER'. mc_add 'TMAS_D_UDSD'. mc_add 'TMAS_D_UDXD'.
  mc_add 'TMAS_OLA_D_AER'. mc_add 'TMAS_OLA_D_ASR14'.
  mc_add 'TMAS_OLA_D_ASR7'. mc_add 'TMAS_OLA_D_BGJOB'.
  mc_add 'TMAS_OLA_D_ER'. mc_add 'TMAS_OLA_D_GRAPH'.
  mc_add 'TMAS_OLA_D_JOB'. mc_add 'TMAS_OLA_D_LANG'.
  mc_add 'TMAS_OLA_D_LOG'. mc_add 'TMAS_OLA_D_LOGCS'.
  mc_add 'TMAS_OLA_D_OBJ'. mc_add 'TMAS_OLA_D_OBJL'.
  mc_add 'TMAS_OLA_D_OBJT'. mc_add 'TMAS_OLA_D_SR14'.
  mc_add 'TMAS_OLA_D_SR7'. mc_add 'TMAS_OLA_D_TO'.
  mc_add 'TMAS_PRE_D_AER'. mc_add 'TMAS_PRE_D_ASR14'.
  mc_add 'TMAS_PRE_D_BGJOB'. mc_add 'TMAS_PRE_D_ER'.
  mc_add 'TMAS_PRE_D_JOB'. mc_add 'TMAS_PRE_D_LANG'.
  mc_add 'TMAS_PRE_D_LOG'. mc_add 'TMAS_PRE_D_LOGCS'.
  mc_add 'TMAS_PRE_D_OBJ'. mc_add 'TMAS_PRE_D_PT'.
  mc_add 'TMAS_PRE_D_SR14'. mc_add 'TMAS_PRE_D_WRKL'. mc_add 'TMCDF'.
  mc_add 'TMCHAR'. mc_add 'TMDIR'. mc_add 'TMEDS'. mc_add 'TMEMO'.
  mc_add 'TMENU01'. mc_add 'TMENU01R'. mc_add 'TMENU01T'.
  mc_add 'TMENUBK01'. mc_add 'TMENUBK01R'. mc_add 'TMENUBK01T'.
  mc_add 'TMENUTAB'. mc_add 'TMETA'. mc_add 'TMLVW'.
  mc_add 'TMPOBJRES1'. mc_add 'TMSACTDAT'. mc_add 'TMSALOG'.
  mc_add 'TMSALOGAR'. mc_add 'TMSALRTSYS'. mc_add 'TMSBCIBOX'.
  mc_add 'TMSBCIIBOX'. mc_add 'TMSBCIJOY'. mc_add 'TMSBCINEX'.
  mc_add 'TMSBCINTAB'. mc_add 'TMSBCIOBJ'. mc_add 'TMSBCIXBOX'.
  mc_add 'TMSBUFCNT'. mc_add 'TMSBUFPRO'. mc_add 'TMSBUFREQ'.
  mc_add 'TMSBUFTXT'. mc_add 'TMSCDES'. mc_add 'TMSCDES_S'.
  mc_add 'TMSCDESD'. mc_add 'TMSCDIST'. mc_add 'TMSCDOL'.
  mc_add 'TMSCDOM'. mc_add 'TMSCDOMT'. mc_add 'TMSCFGTRACE'.
  mc_add 'TMSCNFS'. mc_add 'TMSCNFST'. mc_add 'TMSCROUTE'.
  mc_add 'TMSCSYS'. mc_add 'TMSCSYST'. mc_add 'TMSCTASK'.
  mc_add 'TMSCTOK'. mc_add 'TMSDEPLOYTARGETS'. mc_add 'TMSEXTSTAT'.
  mc_add 'TMSFSYSH'. mc_add 'TMSFSYSL'. mc_add 'TMSMCONF'.
  mc_add 'TMSPCONF'. mc_add 'TMSPVERS'. mc_add 'TMSQAMTREE'.
  mc_add 'TMSQAMTRET'. mc_add 'TMSQASTEPS'. mc_add 'TMSQASTEPT'.
  mc_add 'TMSQASTEPZ'. mc_add 'TMSQCONFRM'. mc_add 'TMSQLASTWL'.
  mc_add 'TMSQNOTES'. mc_add 'TMSQNOTESH'. mc_add 'TMSQWL'.
  mc_add 'TMSQWLF'. mc_add 'TMSQWLFH'. mc_add 'TMSQWLH'.
  mc_add 'TMSQWLN'. mc_add 'TMSSRV'. mc_add 'TMSSRVT'. mc_add 'TMSTCRI'.
  mc_add 'TMSTLOCKNP'. mc_add 'TMSTLOCKNR'. mc_add 'TMSTLOCKP'.
  mc_add 'TMSTLOCKR'. mc_add 'TMSTOUT'. mc_add 'TMSTPALOG'.
  mc_add 'TMSUCONF'. mc_add 'TMW_TMSANA'. mc_add 'TMW_TMSSTATS'.
  mc_add 'TMW_TMSSTATS_LOG'. mc_add 'TNETMAC'. mc_add 'TNETMONINF'.
  mc_add 'TNMAP'. mc_add 'TNOBAPI'. mc_add 'TNODE01'. mc_add 'TNODE01R'.
  mc_add 'TNODE01T'. mc_add 'TNODE02'. mc_add 'TNODE02_A'.
  mc_add 'TNODE02_AC'. mc_add 'TNODE02_AP'. mc_add 'TNODE02_AS'.
  mc_add 'TNODE02_AT'. mc_add 'TNODE02A'. mc_add 'TNODE02R'.
  mc_add 'TNODE02T'. mc_add 'TNODEE'. mc_add 'TNODEH'.
  mc_add 'TNODEIMG'. mc_add 'TNODEIMGR'. mc_add 'TNODEIMGT'.
  mc_add 'TNODEPR'. mc_add 'TNODEPRR'. mc_add 'TNODEPRT'.
  mc_add 'TNODERELN'. mc_add 'TNODERELNR'. mc_add 'TNODERELNT'.
  mc_add 'TNODEREV'. mc_add 'TNODEREVR'. mc_add 'TNODEREVT'.
  mc_add 'TNODERTYPE'. mc_add 'TNODES'. mc_add 'TNODET'.
  mc_add 'TNONOCL'. mc_add 'TNOTATW'. mc_add 'TNOTECL'.
  mc_add 'TNOTEED'. mc_add 'TNOTEEPR'. mc_add 'TNOTEFPR'.
  mc_add 'TNOTEP'. mc_add 'TNOTESAV'. mc_add 'TNOTETEM'.
  mc_add 'TNOTETPR'. mc_add 'TNOTFPR'. mc_add 'TNOTFTW'. mc_add 'TNRO'.
  mc_add 'TNROS'. mc_add 'TNROST'. mc_add 'TNROT'. mc_add 'TNRS6'.
  mc_add 'TNRSO'. mc_add 'TNRSO4'. mc_add 'TNRT2'. mc_add 'TNRT2T'.
  mc_add 'TNRT2X'. mc_add 'TNRT2XT'. mc_add 'TNRT3'. mc_add 'TNRT3T'.
  mc_add 'TNRT5'. mc_add 'TNRT5T'. mc_add 'TNRT6'. mc_add 'TNRT6T'.
  mc_add 'TNRT7'. mc_add 'TNRT7T'. mc_add 'TNRT8'. mc_add 'TNRT8A'.
  mc_add 'TNRT8AT'. mc_add 'TNRT8OT'. mc_add 'TNRT8T'. mc_add 'TOAAM'.
  mc_add 'TOAAP'. mc_add 'TOAAR'. mc_add 'TOAAS'. mc_add 'TOAATTCUST'.
  mc_add 'TOABA'. mc_add 'TOAC1'. mc_add 'TOACA'. mc_add 'TOACB'.
  mc_add 'TOACB_HEAD'. mc_add 'TOACL'. mc_add 'TOACO'. mc_add 'TOACR'.
  mc_add 'TOACR_HEAD'. mc_add 'TOADD'. mc_add 'TOADL_SPLT'.
  mc_add 'TOADOCSP'. mc_add 'TOADOCSP_N'. mc_add 'TOADSACT'.
  mc_add 'TOADY'. mc_add 'TOAER'. mc_add 'TOAEX'. mc_add 'TOAFSATT'.
  mc_add 'TOAFSATX'. mc_add 'TOAFSDS'. mc_add 'TOAFSMAP'.
  mc_add 'TOAGOS'. mc_add 'TOAIN'. mc_add 'TOAJAVA'. mc_add 'TOAKT'.
  mc_add 'TOAMS'. mc_add 'TOAOS'. mc_add 'TOAPI'. mc_add 'TOARS'.
  mc_add 'TOASA'. mc_add 'TOASB'. mc_add 'TOASD'. mc_add 'TOASK'.
  mc_add 'TOASO'. mc_add 'TOASR'. mc_add 'TOAV1'. mc_add 'TOAV2'.
  mc_add 'TOAVK'. mc_add 'TOAVK_S'. mc_add 'TOAWFLIMPLACE'.
  mc_add 'TOAXC'. mc_add 'TOBC'. mc_add 'TOBCT'. mc_add 'TOBJ'.
  mc_add 'TOBJ_CHK_CTRL_D'. mc_add 'TOBJ_CHK_CTRL_DH'.
  mc_add 'TOBJ_CHK_CTRL_DT'. mc_add 'TOBJ_CHK_CTRL_R'.
  mc_add 'TOBJ_CHK_CTRL_RH'. mc_add 'TOBJ_OFF_NEW'. mc_add 'TOBJ_TPL'.
  mc_add 'TOBJ_TPO'. mc_add 'TOBJ_TPT'. mc_add 'TOBJC'.
  mc_add 'TOBJECTP'. mc_add 'TOBJECTT'. mc_add 'TOBJT'.
  mc_add 'TOBJVOR'. mc_add 'TOBJVORDAT'. mc_add 'TOBJVORFLG'.
  mc_add 'TODOK'. mc_add 'TODOS'. mc_add 'TOJMB'. mc_add 'TOJMT'.
  mc_add 'TOJRB'. mc_add 'TOJRT'. mc_add 'TOJTB'. mc_add 'TOJTD'.
  mc_add 'TOJTL'. mc_add 'TOJTT'. mc_add 'TOLE'. mc_add 'TOLET'.
  mc_add 'TPALOG'. mc_add 'TPALOGHDR'. mc_add 'TPALOGHDRHISTORY'.
  mc_add 'TPARA'. mc_add 'TPARAAPPL'. mc_add 'TPARAT'.
  mc_add 'TPDA_SCRIPT_CLS'. mc_add 'TPDA_SCRIPT_CLST'.
  mc_add 'TPDA_TABLE_CONFC'. mc_add 'TPDA_TABLE_CONFH'.
  mc_add 'TPDA_VARIABLES'. mc_add 'TPDACUSTOMTOOLS'.
  mc_add 'TPDACUSTOMTOOLST'. mc_add 'TPDAHINTS'. mc_add 'TPDAOPTIONS'.
  mc_add 'TPDAPI_TEST'. mc_add 'TPDAPRESUI'. mc_add 'TPDASESSIONBP'.
  mc_add 'TPDASESSIONS'. mc_add 'TPDASESSIONSSETT'.
  mc_add 'TPDASESSIONST'. mc_add 'TPDASESSIONWP'. mc_add 'TPDASETTINGS'.
  mc_add 'TPDASUBSC'. mc_add 'TPDASUBSCAREA'. mc_add 'TPDATABTEXT'.
  mc_add 'TPDB2'. mc_add 'TPF02'. mc_add 'TPF03'. mc_add 'TPFBA'.
  mc_add 'TPFES'. mc_add 'TPFET'. mc_add 'TPFHS'. mc_add 'TPFHT'.
  mc_add 'TPFID'. mc_add 'TPFST'. mc_add 'TPFVA'. mc_add 'TPFVD'.
  mc_add 'TPFVE'. mc_add 'TPFVI'. mc_add 'TPFVP'. mc_add 'TPFVQ'.
  mc_add 'TPFVR'. mc_add 'TPFVX'. mc_add 'TPFWE'. mc_add 'TPFYDOC'.
  mc_add 'TPFYPROPTY'. mc_add 'TPGP'. mc_add 'TPGPT'. mc_add 'TPHASEP'.
  mc_add 'TPHASETP'. mc_add 'TPIND'. mc_add 'TPLOG'.
  mc_add 'TPLOGNAMES'. mc_add 'TPLOGNAMESS'. mc_add 'TPM_COUNTER'.
  mc_add 'TPM_DIR'. mc_add 'TPM_DIRT'. mc_add 'TPM_VERSION'.
  mc_add 'TPMONI'. mc_add 'TPOTB'. mc_add 'TPR_EXCL'.
  mc_add 'TPR_INDEX'. mc_add 'TPR_VINDX'. mc_add 'TPR_XREF_D'.
  mc_add 'TPR_XREF_P'. mc_add 'TPRCLASS'. mc_add 'TPRCLASST'.
  mc_add 'TPRI_PAR'. mc_add 'TPRIOCL'. mc_add 'TPRIOEPR'.
  mc_add 'TPRIOP'. mc_add 'TPRIOTPR'. mc_add 'TPROJ_GEN'.
  mc_add 'TPROJDIST'. mc_add 'TPROJECT'. mc_add 'TPROJECTF'.
  mc_add 'TPROJECTT'. mc_add 'TPROJMILESTONE'. mc_add 'TPROJMILESTONET'.
  mc_add 'TPRORGUNITS'. mc_add 'TPROT'. mc_add 'TPRVOR'.
  mc_add 'TPRVORDAT'. mc_add 'TPRVORT'. mc_add 'TPRX_DEMO_ADDCBE'.
  mc_add 'TPRX_DEMO_ADDDPU'. mc_add 'TPSQL_DB_SID_LOG'.
  mc_add 'TPSQL_IMPORT'. mc_add 'TPSTAT'. mc_add 'TPTAB'.
  mc_add 'TPTAPP'. mc_add 'TPTAPPT'. mc_add 'TPTGSFRM'.
  mc_add 'TPTGSLYT'. mc_add 'TPTGSLYTS'. mc_add 'TPTGSLYTT'.
  mc_add 'TPTGSOBJ'. mc_add 'TPTGSOBJT'. mc_add 'TPTGSSCR'.
  mc_add 'TPTLYTUTY'. mc_add 'TPTLYTUTYT'. mc_add 'TPTSP'.
  mc_add 'TPTUSLYT'. mc_add 'TPTUSLYTS'. mc_add 'TPTUSLYTT'.
  mc_add 'TPTUSOBJ'. mc_add 'TPTUSOBJT'. mc_add 'TQLOG'. mc_add 'TQLOT'.
  mc_add 'TQOP'. mc_add 'TQOPT'. mc_add 'TQSTQ1'. mc_add 'TQSTQS'.
  mc_add 'TRACOLH'. mc_add 'TRACOLR'. mc_add 'TRACOLSYST'.
  mc_add 'TRACOLT'. mc_add 'TRANSFDESC'. mc_add 'TRANSFTEXT'.
  mc_add 'TRANSL_ASSIGNMNT'. mc_add 'TRANSL_GRAPH'. mc_add 'TRBAT'.
  mc_add 'TRBAT2'. mc_add 'TRBAT3'. mc_add 'TRBATC'. mc_add 'TRBATS'.
  mc_add 'TRBATSOPM'. mc_add 'TRCHECK'. mc_add 'TRCL'. mc_add 'TRCLT'.
  mc_add 'TRD_COBJPREFIX'. mc_add 'TRD_COBJXPATH'. mc_add 'TRD_LINK'.
  mc_add 'TRD_LOG'. mc_add 'TRDIRE'. mc_add 'TRDIRT'. mc_add 'TRDIRTI'.
  mc_add 'TREELOG'. mc_add 'TREEROAD'. mc_add 'TRELTABS'.
  mc_add 'TRESC'. mc_add 'TRESE'. mc_add 'TRESEPR'. mc_add 'TRESN'.
  mc_add 'TRESNT'. mc_add 'TRESSCL'. mc_add 'TRESSP'. mc_add 'TRESTP'.
  mc_add 'TRESTPR'. mc_add 'TREX_ACT'. mc_add 'TREX_ACTT'.
  mc_add 'TREX_ASYNC_ADMIN'. mc_add 'TREX_BC_MEASURE'.
  mc_add 'TREX_HOST_SWITCH'. mc_add 'TREX_INDEX_SERV'.
  mc_add 'TREX_IS_CACHE'. mc_add 'TREX_SEARCH_SERV'.
  mc_add 'TREX_TEST_RUN'. mc_add 'TREX_TRACE_D'. mc_add 'TREX_TRACE_H'.
  mc_add 'TREXBCREQUESTTAB'. mc_add 'TREXBCTEST_BIN'.
  mc_add 'TREXBCTEST_DATA'. mc_add 'TREXBCTESTTAB1'.
  mc_add 'TREXBCTESTTAB2'. mc_add 'TREXBCTESTTAB3'.
  mc_add 'TREXBCTESTTAB4'. mc_add 'TREXBCTESTTAB5'.
  mc_add 'TREXBCTESTTAB6'. mc_add 'TREXBCTESTTAB7'.
  mc_add 'TREXBCTESTTAB8'. mc_add 'TREXBCTESTTAB9'.
  mc_add 'TREXHDB_GLOBAL'. mc_add 'TREXICM'. mc_add 'TREXICM_GLOBAL'.
  mc_add 'TREXICM_GLOBAL2'. mc_add 'TREXINTTAB'. mc_add 'TRFC_I_DEST'.
  mc_add 'TRFC_I_ERR_STATE'. mc_add 'TRFC_I_EXE_STATE'.
  mc_add 'TRFC_I_SDATA'. mc_add 'TRFC_I_UNIT'.
  mc_add 'TRFC_I_UNIT_LOCK'. mc_add 'TRFC_I_UNIT_SESS'.
  mc_add 'TRFC_O_DEST'. mc_add 'TRFC_O_ERR_STATE'.
  mc_add 'TRFC_O_EXE_STATE'. mc_add 'TRFC_O_SDATA'.
  mc_add 'TRFC_O_UNIT'. mc_add 'TRFC_O_UNIT_LOCK'. mc_add 'TRFCQDATA'.
  mc_add 'TRFCQSTATE'. mc_add 'TRFUNCSEL'. mc_add 'TRFUNCSELT'.
  mc_add 'TRJOB'. mc_add 'TRJOBS'. mc_add 'TRM020'. mc_add 'TRM040'.
  mc_add 'TRM060'. mc_add 'TRM080'. mc_add 'TRM255'. mc_add 'TRMABB'.
  mc_add 'TRMAC'. mc_add 'TRMAUT'. mc_add 'TRMCOM'. mc_add 'TRMFDIR'.
  mc_add 'TRMFRFC'. mc_add 'TRMIDX'. mc_add 'TRMINT'. mc_add 'TRMMAPP'.
  mc_add 'TRMOIX'. mc_add 'TRMSG'. mc_add 'TRMSGT'. mc_add 'TRMSTA'.
  mc_add 'TRMSTMA'. mc_add 'TRMTRA'. mc_add 'TRMWRK'. mc_add 'TRMWUL'.
  mc_add 'TRNL_AUTH'. mc_add 'TRNL_AUTHT'. mc_add 'TRNL_AV'.
  mc_add 'TRNL_BTC_H'. mc_add 'TRNL_DC_2'. mc_add 'TRNL_DC_2H'.
  mc_add 'TRNL_DIS_O'. mc_add 'TRNL_E071'. mc_add 'TRNL_FB'.
  mc_add 'TRNL_FB_DC'. mc_add 'TRNL_LA_CL'. mc_add 'TRNL_LA_DR'.
  mc_add 'TRNL_LANG'. mc_add 'TRNL_OL'. mc_add 'TRNL_OTR'.
  mc_add 'TRNL_OTR_S'. mc_add 'TRNL_PROF'. mc_add 'TRNL_PROFV'.
  mc_add 'TRNL_SSYS'. mc_add 'TRNL_TR'. mc_add 'TRNL_USER'.
  mc_add 'TRNL_WL'. mc_add 'TRNL_WLI'. mc_add 'TRNL_WLII'.
  mc_add 'TRNL_WLIII'. mc_add 'TRNL_WLIV'. mc_add 'TRNSPACE'.
  mc_add 'TRNSPACEL'. mc_add 'TRNSPACET'. mc_add 'TRNSPACETT'.
  mc_add 'TROADMAP'. mc_add 'TROADMAPIR'. mc_add 'TROADMAPT'.
  mc_add 'TROADMAPVM'. mc_add 'TRPRAGMA'. mc_add 'TRPRAGMAT'.
  mc_add 'TRPUT'. mc_add 'TRS35'. mc_add 'TRSE_CLASSES'. mc_add 'TRSLT'.
  mc_add 'TRSTI0'. mc_add 'TRSTI0T'. mc_add 'TRSTT'. mc_add 'TRSTTT'.
  mc_add 'TRSYSCOMP'. mc_add 'TRTYPEH'. mc_add 'TRTYPET'.
  mc_add 'TRX_BC_RESET_INF'. mc_add 'TS3RMAPPFU'. mc_add 'TSAC'.
  mc_add 'TSACT'. mc_add 'TSAD_FORM_TEXT'. mc_add 'TSAD_FORM_TEXT_T'.
  mc_add 'TSAD10'. mc_add 'TSAD11'. mc_add 'TSAD11T'. mc_add 'TSAD14'.
  mc_add 'TSAD14T'. mc_add 'TSAD7'. mc_add 'TSAD7T'. mc_add 'TSAD8'.
  mc_add 'TSAD8T'. mc_add 'TSADC'. mc_add 'TSADCORR'. mc_add 'TSADOBJ'.
  mc_add 'TSADOBJT'. mc_add 'TSADRU'. mc_add 'TSADRUT'. mc_add 'TSADRV'.
  mc_add 'TSADRVGRP'. mc_add 'TSADRVGRPE'. mc_add 'TSADRVGRPT'.
  mc_add 'TSADV'. mc_add 'TSADVC'. mc_add 'TSADVT'. mc_add 'TSALQ'.
  mc_add 'TSAOB'. mc_add 'TSAPPLSUB'. mc_add 'TSAPPLSUBT'.
  mc_add 'TSARQ'. mc_add 'TSAV'. mc_add 'TSAVT'. mc_add 'TSCICAL'.
  mc_add 'TSCIT'. mc_add 'TSCMSTAT'. mc_add 'TSCON_PROJ_ASY'.
  mc_add 'TSCON_PROJ_DET'. mc_add 'TSCON_PROJ_DET2'.
  mc_add 'TSCON_PROJ_LOG'. mc_add 'TSCON_SDLXLIFF'.
  mc_add 'TSCON_T_CON'. mc_add 'TSCON_TEMPLATE'. mc_add 'TSCTFGROUP'.
  mc_add 'TSCTFGRT'. mc_add 'TSDB6'. mc_add 'TSDDC'. mc_add 'TSDDD'.
  mc_add 'TSDDT'. mc_add 'TSDIR'. mc_add 'TSDIRT'. mc_add 'TSDO1'.
  mc_add 'TSDOC'. mc_add 'TSDUM'. mc_add 'TSE_CTXT_MENU'.
  mc_add 'TSE_CTXT_MENUT'. mc_add 'TSE00'. mc_add 'TSE01'.
  mc_add 'TSE02'. mc_add 'TSE03'. mc_add 'TSE04'. mc_add 'TSE05'.
  mc_add 'TSE06'. mc_add 'TSE061N'. mc_add 'TSE07'. mc_add 'TSE08'.
  mc_add 'TSECLOG_SETTINGS'. mc_add 'TSEXC'. mc_add 'TSFDEVTY'.
  mc_add 'TSFGH'. mc_add 'TSFGL'. mc_add 'TSFST'. mc_add 'TSH01'.
  mc_add 'TSHCL'. mc_add 'TSHM0'. mc_add 'TSINF'. mc_add 'TSIW_CONFIG'.
  mc_add 'TSIW_CONFIGT'. mc_add 'TSIW_CONTEXT'.
  mc_add 'TSIW_CONTEXT_VAR'. mc_add 'TSIW_HISTORY'. mc_add 'TSIW_IMPL'.
  mc_add 'TSIW_IMPL_PIECE'. mc_add 'TSIW_LANDSCAPE'. mc_add 'TSIW_LINK'.
  mc_add 'TSIW_LRU'. mc_add 'TSIW_MAPPING'. mc_add 'TSIW_MM_MATCHES'.
  mc_add 'TSIW_MM_REGISTRY'. mc_add 'TSIW_MM_TEAPOTS'.
  mc_add 'TSIW_MM_TENTACLE'. mc_add 'TSIW_MM_TYPEDEFS'.
  mc_add 'TSIW_NAME_RESERV'. mc_add 'TSIW_PROJ'. mc_add 'TSIW_PROJT'.
  mc_add 'TSIW_SXF_API'. mc_add 'TSIW_SXF_FIELD'.
  mc_add 'TSIW_SXF_FIELDRL'. mc_add 'TSIW_SXF_TYPE'.
  mc_add 'TSIW_TEST_TAB'. mc_add 'TSIW_TEST_TAB2'.
  mc_add 'TSIW_TESTDATA'. mc_add 'TSKT2'. mc_add 'TSKT4'.
  mc_add 'TSKTF'. mc_add 'TSKTL'. mc_add 'TSKTU'. mc_add 'TSKTV'.
  mc_add 'TSKTW'. mc_add 'TSKTX'. mc_add 'TSKTY'. mc_add 'TSKTZ'.
  mc_add 'TSL1D'. mc_add 'TSL1T'. mc_add 'TSL2D'. mc_add 'TSL2T'.
  mc_add 'TSL3D'. mc_add 'TSL3T'. mc_add 'TSL4T'. mc_add 'TSL7T'.
  mc_add 'TSLE1'. mc_add 'TSLE2'. mc_add 'TSLE3'. mc_add 'TSLE4'.
  mc_add 'TSLLT'. mc_add 'TSM01'. mc_add 'TSOEX'. mc_add 'TSOFA'.
  mc_add 'TSOPA'. mc_add 'TSOPT'. mc_add 'TSORA'. mc_add 'TSOS'.
  mc_add 'TSOTD'. mc_add 'TSOTT'. mc_add 'TSOTX'. mc_add 'TSOVM'.
  mc_add 'TSOVT'. mc_add 'TSP01'. mc_add 'TSP01P'. mc_add 'TSP02'.
  mc_add 'TSP02A'. mc_add 'TSP02F'. mc_add 'TSP02FX'. mc_add 'TSP02JT'.
  mc_add 'TSP02L'. mc_add 'TSP02T'. mc_add 'TSP02W'. mc_add 'TSP03'.
  mc_add 'TSP03A'. mc_add 'TSP03C'. mc_add 'TSP03D'. mc_add 'TSP03L'.
  mc_add 'TSP03POCCNF'. mc_add 'TSP03POCPRE'. mc_add 'TSP03T'.
  mc_add 'TSP04'. mc_add 'TSP05'. mc_add 'TSP06'. mc_add 'TSP06A'.
  mc_add 'TSP06POT'. mc_add 'TSP06T'. mc_add 'TSP07'. mc_add 'TSP08'.
  mc_add 'TSP09'. mc_add 'TSP09A'. mc_add 'TSP0A'. mc_add 'TSP0B'.
  mc_add 'TSP0C'. mc_add 'TSP0E'. mc_add 'TSP0F'. mc_add 'TSP0G'.
  mc_add 'TSP0H'. mc_add 'TSP0I'. mc_add 'TSP0K'. mc_add 'TSP0L'.
  mc_add 'TSP0P'. mc_add 'TSP0Q'. mc_add 'TSP0R'. mc_add 'TSP0S'.
  mc_add 'TSP0T'. mc_add 'TSP0U'. mc_add 'TSP1D'. mc_add 'TSP1T'.
  mc_add 'TSP2D'. mc_add 'TSP2T'. mc_add 'TSP3T'. mc_add 'TSP4D'.
  mc_add 'TSP4T'. mc_add 'TSP5D'. mc_add 'TSP5T'. mc_add 'TSP6D'.
  mc_add 'TSP6T'. mc_add 'TSP7D'. mc_add 'TSP7S'. mc_add 'TSP7T'.
  mc_add 'TSP8D'. mc_add 'TSP8T'. mc_add 'TSP9D'. mc_add 'TSP9T'.
  mc_add 'TSPCMDS'. mc_add 'TSPCONV'. mc_add 'TSPCPCDS'.
  mc_add 'TSPCPCDSE'. mc_add 'TSPCPCFAV'. mc_add 'TSPCPCPGC'.
  mc_add 'TSPCPCPGL'. mc_add 'TSPCPCPL'. mc_add 'TSPCPCTPR'.
  mc_add 'TSPCPCTSGC'. mc_add 'TSPCPCTSGL'. mc_add 'TSPCPCTSL'.
  mc_add 'TSPDD'. mc_add 'TSPDD2'. mc_add 'TSPDDCH'. mc_add 'TSPDEVCF'.
  mc_add 'TSPEVDEV'. mc_add 'TSPEVJOB'. mc_add 'TSPJSTAT'.
  mc_add 'TSPLOMS'. mc_add 'TSPOPIMAN'. mc_add 'TSPOPIP'.
  mc_add 'TSPOPIPRN'. mc_add 'TSPOPIPRN_WORK'. mc_add 'TSPOPTIONS'.
  mc_add 'TSPQSTAT'. mc_add 'TSPRF'. mc_add 'TSPRMGCF'.
  mc_add 'TSPROMS'. mc_add 'TSPSV'. mc_add 'TSPSVI'. mc_add 'TSRCG'.
  mc_add 'TSRVGRP'. mc_add 'TSRVLST'. mc_add 'TSST1'. mc_add 'TSST2'.
  mc_add 'TSST3'. mc_add 'TST_POOL_1'. mc_add 'TST02'. mc_add 'TST05'.
  mc_add 'TST07'. mc_add 'TSTAACTW'. mc_add 'TSTAFPR'. mc_add 'TSTAFTW'.
  mc_add 'TSTAHPR'. mc_add 'TSTAHTPR'. mc_add 'TSTAHTTW'.
  mc_add 'TSTAHTW'. mc_add 'TSTAKEPR'. mc_add 'TSTAM'. mc_add 'TSTAMK'.
  mc_add 'TSTAPRTW'. mc_add 'TSTAREPR'. mc_add 'TSTASETW'.
  mc_add 'TSTASTTW'. mc_add 'TSTATACT'. mc_add 'TSTATC'.
  mc_add 'TSTATCE'. mc_add 'TSTATCL'. mc_add 'TSTATCP'.
  mc_add 'TSTATCS'. mc_add 'TSTATCT'. mc_add 'TSTATEPR'.
  mc_add 'TSTATF'. mc_add 'TSTATH'. mc_add 'TSTATH4'. mc_add 'TSTATHT'.
  mc_add 'TSTATHT4'. mc_add 'TSTATIMG'. mc_add 'TSTATP'.
  mc_add 'TSTATPP'. mc_add 'TSTATR'. mc_add 'TSTATRE'. mc_add 'TSTATRP'.
  mc_add 'TSTATRS'. mc_add 'TSTATRT'. mc_add 'TSTATS'. mc_add 'TSTATSE'.
  mc_add 'TSTATST'. mc_add 'TSTATTPR'. mc_add 'TSTC'. mc_add 'TSTC_LOG'.
  mc_add 'TSTC_SM'. mc_add 'TSTC_SRT'. mc_add 'TSTCA'. mc_add 'TSTCC'.
  mc_add 'TSTCLUS1_E'. mc_add 'TSTCP'. mc_add 'TSTCT'. mc_add 'TSTPH'.
  mc_add 'TSTPOOL1_E'. mc_add 'TSYIMPSTAT'. mc_add 'TSYNC'.
  mc_add 'TSYPROJVER'. mc_add 'TSYREQUEST'. mc_add 'TSYS1'.
  mc_add 'TSYS2'. mc_add 'TSYSDEF'. mc_add 'TSYST'. mc_add 'TSYSTSTC'.
  mc_add 'TSYSTT'. mc_add 'TSYSYNCSYS'. mc_add 'TT3X40'. mc_add 'TTABP'.
  mc_add 'TTABSPLIT'. mc_add 'TTABSTRIPCL'. mc_add 'TTADIR'.
  mc_add 'TTASK'. mc_add 'TTASKT'. mc_add 'TTCDS'. mc_add 'TTCGR'.
  mc_add 'TTDCF'. mc_add 'TTDCW'. mc_add 'TTDTG'. mc_add 'TTHINFO'.
  mc_add 'TTHTEST'. mc_add 'TTOBJECTS'. mc_add 'TTOBJHIER'.
  mc_add 'TTODIR'. mc_add 'TTPDASUBFOLDER'. mc_add 'TTPDASUBFOLDERHI'.
  mc_add 'TTPDASUBFOLDERT'. mc_add 'TTPDATOOLCTRLAD'.
  mc_add 'TTPDATOOLID'. mc_add 'TTPDATOOLIDAD'. mc_add 'TTPDATOOLIDT'.
  mc_add 'TTPDATOOLIEXT'. mc_add 'TTRAL'. mc_add 'TTREE'.
  mc_add 'TTREE_APPL'. mc_add 'TTREE_EXT'. mc_add 'TTREE_EXTT'.
  mc_add 'TTREE_FLNK'. mc_add 'TTREE_FLP'. mc_add 'TTREE_GEN'.
  mc_add 'TTREE_JOB'. mc_add 'TTREE_SFW_NODES'. mc_add 'TTREEBRAEX'.
  mc_add 'TTREEBRAIN'. mc_add 'TTREECOUEX'. mc_add 'TTREECOUIN'.
  mc_add 'TTREED'. mc_add 'TTREEFLAEX'. mc_add 'TTREEFLAIN'.
  mc_add 'TTREEI'. mc_add 'TTREELOADS'. mc_add 'TTREELOADT'.
  mc_add 'TTREEN'. mc_add 'TTREEP'. mc_add 'TTREERELEX'.
  mc_add 'TTREERELIN'. mc_add 'TTREEROAEX'. mc_add 'TTREEROAIN'.
  mc_add 'TTREEROLEX'. mc_add 'TTREEROLIN'. mc_add 'TTREES'.
  mc_add 'TTREESRCH'. mc_add 'TTREESUBEX'. mc_add 'TTREESUBIN'.
  mc_add 'TTREET'. mc_add 'TTREETYPE'. mc_add 'TTREETYPEN'.
  mc_add 'TTREETYPET'. mc_add 'TTREV'. mc_add 'TTREV_APPL'.
  mc_add 'TTREV_COMP'. mc_add 'TTREV_DNUM'. mc_add 'TTREV_FLNK'.
  mc_add 'TTREV_HCHI'. mc_add 'TTREV_HEAD'. mc_add 'TTREV_NODE'.
  mc_add 'TTREV_REF'. mc_add 'TTREV_ROLE'. mc_add 'TTREV_TEXT'.
  mc_add 'TTREV_VLNK'. mc_add 'TTREVBRAEX'. mc_add 'TTREVBRAIN'.
  mc_add 'TTREVCOUEX'. mc_add 'TTREVCOUIN'. mc_add 'TTREVI'.
  mc_add 'TTREVN'. mc_add 'TTREVP'. mc_add 'TTREVROAEX'.
  mc_add 'TTREVROAIN'. mc_add 'TTREVROLEX'. mc_add 'TTREVROLIN'.
  mc_add 'TTREVS'. mc_add 'TTREVSUBEX'. mc_add 'TTREVSUBIN'.
  mc_add 'TTREVT'. mc_add 'TTREVT_COM'. mc_add 'TTREVTHEAD'.
  mc_add 'TTREVTROLE'. mc_add 'TTXFORMAT'. mc_add 'TTXFP'.
  mc_add 'TTXFPT'. mc_add 'TTXFT'. mc_add 'TTXGR'. mc_add 'TTXGRT'.
  mc_add 'TTXID'. mc_add 'TTXIE'. mc_add 'TTXIN'. mc_add 'TTXIT'.
  mc_add 'TTXOB'. mc_add 'TTXOT'. mc_add 'TTXPI'. mc_add 'TTXPS'.
  mc_add 'TTXSY'. mc_add 'TUAPP'. mc_add 'TUAPPT'.
  mc_add 'TUCC_TEST_TABLE'. mc_add 'TUCHK'. mc_add 'TUCHK1'.
  mc_add 'TUCHK2'. mc_add 'TUCHK3'. mc_add 'TUCON'. mc_add 'TUGRP'.
  mc_add 'TUJOB'. mc_add 'TUL_AC_UNIT'. mc_add 'TUL_AC_UNITT'.
  mc_add 'TUL_ACTTC'. mc_add 'TUL_AUCESS'. mc_add 'TUL_CONTROL'.
  mc_add 'TUL_CPU'. mc_add 'TUL_CPU_RAW'. mc_add 'TUL_ENG_PL'.
  mc_add 'TUL_ENGINES'. mc_add 'TUL_ENGINEST'. mc_add 'TUL_MEMOS'.
  mc_add 'TUL_TRANS'. mc_add 'TUL_TRSTAT'. mc_add 'TUL_UNIT'.
  mc_add 'TUL_UNITT'. mc_add 'TULAPP_PL'. mc_add 'TULTRANS'.
  mc_add 'TUMRES'. mc_add 'TUMSG'. mc_add 'TUNE1'. mc_add 'TUNEOPSGEN'.
  mc_add 'TUPL'. mc_add 'TUPLT'. mc_add 'TUPRO'. mc_add 'TUSTAT'.
  mc_add 'TUTYP'. mc_add 'TUTYPA'. mc_add 'TUTYPP'. mc_add 'TUTYPPL'.
  mc_add 'TUUNT'. mc_add 'TUUNTT'. mc_add 'TUZUS'. mc_add 'TVARH'.
  mc_add 'TVARIND'. mc_add 'TVART'. mc_add 'TVARV'. mc_add 'TVDIR'.
  mc_add 'TVDIR_TC'. mc_add 'TVIMF'. mc_add 'TVIMV'. mc_add 'TVIMVT'.
  mc_add 'TVIRTDOM'. mc_add 'TVIRTDOMT'. mc_add 'TWB_CUST_PRIO'.
  mc_add 'TWB_CUST_STAT'. mc_add 'TWB_CUSTOM'. mc_add 'TWB_EVAL'.
  mc_add 'TWB_PRODUCT'. mc_add 'TWB_PROFIL'. mc_add 'TWB_PROFILT'.
  mc_add 'TWB_RMP'. mc_add 'TWB_RMPT'. mc_add 'TWB_SEQU'.
  mc_add 'TWB_SEQUT'. mc_add 'TWB_TBTCO'. mc_add 'TWB_TPR'.
  mc_add 'TWBMAN_COMP'. mc_add 'TWBMAN_TEST_OBJ'.
  mc_add 'TWBMAN_TEST_OBT'. mc_add 'TWFFP'. mc_add 'TWFQU'.
  mc_add 'TWFRL'. mc_add 'TWFRT'. mc_add 'TWFSY'. mc_add 'TWFTX'.
  mc_add 'TWPCUSTOM'. mc_add 'TWPCUSTOMT'. mc_add 'TWPLGRPLNG'.
  mc_add 'TWPOBJOBJ'. mc_add 'TWPSRVCLSS'. mc_add 'TWPSRVDEVI'.
  mc_add 'TWPSRVLOGS'. mc_add 'TWPSRVOBJ'. mc_add 'TWPSRVPARM'.
  mc_add 'TWPSRVPART'. mc_add 'TWPSRVSFIN'. mc_add 'TWPSRVTXTS'.
  mc_add 'TWPURLPATH'. mc_add 'TWPURLSVR'. mc_add 'TWSYS'.
  mc_add 'TXBUF'. mc_add 'TXCOM'. mc_add 'TXCOMSECU'. mc_add 'TXDPCHKF'.
  mc_add 'TXDPCHKO'. mc_add 'TXDPIDXSTA'. mc_add 'TXDPLOIO'.
  mc_add 'TXDPLOIOT'. mc_add 'TXDPLOPR'. mc_add 'TXDPLORE'.
  mc_add 'TXDPLORI'. mc_add 'TXDPPHF'. mc_add 'TXDPPHHR'.
  mc_add 'TXDPPHHRPR'. mc_add 'TXDPPHIO'. mc_add 'TXDPPHNM'.
  mc_add 'TXDPPHNMPR'. mc_add 'TXDPPHPR'. mc_add 'TXDPPHRE'.
  mc_add 'TXDPPHREPR'. mc_add 'TXDPPHRI'. mc_add 'TXDPPHRIPR'.
  mc_add 'TXMI_USAGE'. mc_add 'TXMILOG'. mc_add 'TXMILOGRAW'.
  mc_add 'TXMIMSG'. mc_add 'TXML_NODE'. mc_add 'TXML_NODER'.
  mc_add 'TXML_NODET'. mc_add 'TXMLCHKF'. mc_add 'TXMLCHKO'.
  mc_add 'TXMLIDXSTA'. mc_add 'TXMLLOIO'. mc_add 'TXMLLOIOT'.
  mc_add 'TXMLLOPR'. mc_add 'TXMLLORE'. mc_add 'TXMLLORI'.
  mc_add 'TXMLPHF'. mc_add 'TXMLPHHR'. mc_add 'TXMLPHHRPR'.
  mc_add 'TXMLPHIO'. mc_add 'TXMLPHNM'. mc_add 'TXMLPHNMPR'.
  mc_add 'TXMLPHPR'. mc_add 'TXMLPHRE'. mc_add 'TXMLPHREPR'.
  mc_add 'TXMLPHRI'. mc_add 'TXMLPHRIPR'. mc_add 'TXSUP'.
  mc_add 'TXSUP2'. mc_add 'TZCUSTCALLTYPE'. mc_add 'TZCUSTCALLTYPE_T'.
  mc_add 'TZCUSTDEST'. mc_add 'TZCUSTTSTYPE'. mc_add 'TZDMPARAMETERS'.
  mc_add 'TZRR_ANNOT'. mc_add 'TZRR_DEP'. mc_add 'TZRR_DEP_ARTIF'.
  mc_add 'TZRR_DEP_LOCA'. mc_add 'TZRR_DEP_PROP'. mc_add 'TZRR_DEP_TST'.
  mc_add 'TZRR_EVARG'. mc_add 'TZRR_EVATTR'. mc_add 'TZRR_EVCMD'.
  mc_add 'TZRR_EVENT'. mc_add 'TZRR_EVPAR'. mc_add 'TZRR_EVTC'.
  mc_add 'TZRR_EXEC'. mc_add 'TZRR_EXPROP'. mc_add 'TZRR_HEAD'.
  mc_add 'TZRR_IFRAG'. mc_add 'TZRR_PCI'. mc_add 'TZRR_PROP'.
  mc_add 'TZRR_RPA'. mc_add 'TZRR_RPA_A'. mc_add 'TZRR_RSATTR'.
  mc_add 'TZRR_SDESC'. mc_add 'TZRR_SUT'. mc_add 'TZRR_VERDICT'.
  mc_add 'TZRR_XML'. mc_add 'UBEKL'. mc_add 'UBURO'.
  mc_add 'UCON_CA_CA'. mc_add 'UCON_CA_HTTPSERV'. mc_add 'UCON_CA_ICF'.
  mc_add 'UCON_CA_RFC'. mc_add 'UCON_CA_RFCFUNC'. mc_add 'UCON_CA_SMTP'.
  mc_add 'UCON_CHANGE_DOC'. mc_add 'UCON_LOAD_TEST'.
  mc_add 'UCON_MONI_TS'. mc_add 'UCONCACONFIGRT'.
  mc_add 'UCONCFGPROFILERT'. mc_add 'UCONFILLCUST'.
  mc_add 'UCONHTTPCLNTSRT'. mc_add 'UCONHTTPLOGGING'.
  mc_add 'UCONHTTPMETALOG'. mc_add 'UCONHTTPSERVTEXT'.
  mc_add 'UCONHTTPSTAPHASE'. mc_add 'UCONHTTPSTATHEAD'.
  mc_add 'UCONICFALIAS'. mc_add 'UCONINCLCART'.
  mc_add 'UCONPRODCLNTSRT'. mc_add 'UCONPROFAPPL'.
  mc_add 'UCONPROFHEAD'. mc_add 'UCONPROFILETEXT'.
  mc_add 'UCONPROFLOGON'. mc_add 'UCONPROFPAGE'. mc_add 'UCONREGISTRY'.
  mc_add 'UCONRFCCART'. mc_add 'UCONRFCPROFCFGRT'.
  mc_add 'UCONRFCSCENARIOS'. mc_add 'UCONRFCSERVHEAD'.
  mc_add 'UCONRFCSERVTEXT'. mc_add 'UCONRFCSRVFMRT'.
  mc_add 'UCONRFCSRVRT'. mc_add 'UCONRFCSTATEHEAD'.
  mc_add 'UCONRFCSTATEPHAS'. mc_add 'UCONRFCSTATERT'.
  mc_add 'UCONRFMCALLERATT'. mc_add 'UCONSERVASDEST'.
  mc_add 'UCONSERVASHEAD'. mc_add 'UCONSERVASTEXT'.
  mc_add 'UCONSERVHANDLER'. mc_add 'UCONSERVHEADER'.
  mc_add 'UCONSERVRT'. mc_add 'UCONSRVCART'. mc_add 'UCONSTATEHTTPWL'.
  mc_add 'UCONSTATETRANSWL'. mc_add 'UCONSTATINCIDENT'.
  mc_add 'UCONSTATMONITORA'. mc_add 'UCONSTATPROTOCOL'.
  mc_add 'UDDICL_CONF'. mc_add 'UFACH'. mc_add 'UG4CCTL'.
  mc_add 'UG4COMCHAR'. mc_add 'UG4CONTAIN'. mc_add 'UG4LANGCP'.
  mc_add 'UG4LNOCP'. mc_add 'UG4PMDIC'. mc_add 'UG4PMIG'.
  mc_add 'UG4POCNV'. mc_add 'UG4SEP'. mc_add 'UG4SEPCP'.
  mc_add 'UG4SETTNGS'. mc_add 'UG4STAT'. mc_add 'UKM_AGENCY'.
  mc_add 'UKM_CONTEXT'. mc_add 'UKM_CONTEXT_C'. mc_add 'UKM_SCHEME'.
  mc_add 'UKRSB'. mc_add 'UKRST'. mc_add 'UKRTB'. mc_add 'UKURS'.
  mc_add 'UM4PMST'. mc_add 'UMG_TEST_1'. mc_add 'UMG_TEST_10'.
  mc_add 'UMG_TEST_11'. mc_add 'UMG_TEST_12'. mc_add 'UMG_TEST_13'.
  mc_add 'UMG_TEST_14'. mc_add 'UMG_TEST_2'. mc_add 'UMG_TEST_3'.
  mc_add 'UMG_TEST_4'. mc_add 'UMG_TEST_5'. mc_add 'UMG_TEST_6'.
  mc_add 'UMG_TEST_6B'. mc_add 'UMG_TEST_7'. mc_add 'UMG_TEST_8'.
  mc_add 'UMG_TEST_9'. mc_add 'UMG_TEST_A'. mc_add 'UMG_TEST_B'.
  mc_add 'UMG_TEST_C'. mc_add 'UMG_TEST_D'. mc_add 'UMG_TEST_F'.
  mc_add 'UMG_TEST_G'. mc_add 'UMG_TEST_INDX'. mc_add 'UMG_TEST_INDX1'.
  mc_add 'UMG_TEST_MODE'. mc_add 'UMG_TEST_P'. mc_add 'UMG_TEST_Q'.
  mc_add 'UMG_TEST_R'. mc_add 'UMG_TEST_S'. mc_add 'UMG_TEST_T'.
  mc_add 'UMG_TEST_U'. mc_add 'UMGBATTAB'. mc_add 'UMGCCTL'.
  mc_add 'UMGCHAR1STAT'. mc_add 'UMGCHAR1STATR'. mc_add 'UMGCHAR2STAT'.
  mc_add 'UMGCHAR2STATR'. mc_add 'UMGCHAR3STAT'. mc_add 'UMGCHAR3STATR'.
  mc_add 'UMGCHAR4STAT'. mc_add 'UMGCHAR4STATR'. mc_add 'UMGCHARCOUNT'.
  mc_add 'UMGCHARCOUNT1'. mc_add 'UMGCHARCOUNTR'.
  mc_add 'UMGCHARSTATLOG'. mc_add 'UMGCHKP'. mc_add 'UMGCOMCHAR'.
  mc_add 'UMGCON'. mc_add 'UMGCONDITION'. mc_add 'UMGCONTAINER'.
  mc_add 'UMGEXCEP'. mc_add 'UMGEXCEPTIONS'. mc_add 'UMGHINT'.
  mc_add 'UMGHISTORY'. mc_add 'UMGINDX'. mc_add 'UMGINFO'.
  mc_add 'UMGINITCLUST'. mc_add 'UMGINITPOOL'. mc_add 'UMGJOBROWINFO'.
  mc_add 'UMGLANGCP'. mc_add 'UMGLNOCP'. mc_add 'UMGLOCK'.
  mc_add 'UMGMAINLOG'. mc_add 'UMGNTCD6'. mc_add 'UMGPLERR'.
  mc_add 'UMGPMBAT'. mc_add 'UMGPMCNV'. mc_add 'UMGPMCOLL'.
  mc_add 'UMGPMDIC'. mc_add 'UMGPMDII'. mc_add 'UMGPMDIT'.
  mc_add 'UMGPMIG'. mc_add 'UMGPMST'. mc_add 'UMGPOCNV'.
  mc_add 'UMGR3LLOG'. mc_add 'UMGR3LLOGINFO'. mc_add 'UMGRTCP'.
  mc_add 'UMGSEP'. mc_add 'UMGSEPCP'. mc_add 'UMGSETT'.
  mc_add 'UMGSETTING'. mc_add 'UMGSETTINGS'. mc_add 'UMGSTAT'.
  mc_add 'UMGTESTCASE'. mc_add 'UMGTESTER'. mc_add 'UMGTSKQ'.
  mc_add 'UMGTWININDX'. mc_add 'UMGTWINPOOL'. mc_add 'UMGTXFLAG'.
  mc_add 'UMGTXFLAG2'. mc_add 'UMGVOCABASSIGN'. mc_add 'UMGWORDLIST'.
  mc_add 'UMGXMLFILES'. mc_add 'UNAMA'. mc_add 'UPERS'.
  mc_add 'UPG_BF_CHANGE'. mc_add 'UPG_COCKPIT_LOG'. mc_add 'UPG_E070_H'.
  mc_add 'UPG_E071_H'. mc_add 'UPG_E071K_H'. mc_add 'UPG_MFLE_FIELD'.
  mc_add 'UPG_MFLE_STATUS'. mc_add 'UPG_MFLE_TABLE'.
  mc_add 'UPG_ST_JOB_INFO'. mc_add 'UPG_ST_JOB_TASK'.
  mc_add 'UPG_ST_RTO_IDX01'. mc_add 'UPG_ST_RTO_IDX02'.
  mc_add 'UPG_ST_RTO_IDX03'. mc_add 'UPG_ST_RTO_IDX04'.
  mc_add 'UPG_ST_RTO_IDX05'. mc_add 'UPG_ST_RTO_IDX06'.
  mc_add 'UPG_ST_RUNFLAG'. mc_add 'UPG_ST_TAB_INFO'.
  mc_add 'UPG_ST_TASK_INFO'. mc_add 'UPG_ST_TASK_RUNT'.
  mc_add 'UPG_ST_TASK_SYNC'. mc_add 'UPG_ST_TASK_TAB'.
  mc_add 'UPG_STATECHANGE'. mc_add 'UPG_SW_CHANGE'.
  mc_add 'UPG_TABLEEXCEPT'. mc_add 'UPGBALOG'. mc_add 'UPGCOLLPATTERN'.
  mc_add 'UPGCOLLTRACE'. mc_add 'UPGGENSET'. mc_add 'UPGJOBTAB'.
  mc_add 'UPGSERVTYPE'. mc_add 'UPGSHDFLD_PART'.
  mc_add 'UPGSHDFLD_STAT'. mc_add 'UPGSTAT'. mc_add 'UPGSTRANA'.
  mc_add 'UPGTDELTA'. mc_add 'UPGVIEWDEPS'. mc_add 'UPGVIEWREQ'.
  mc_add 'UPGXPRA'. mc_add 'UPROF'. mc_add 'URL_DEFINE'.
  mc_add 'URL_EXITS'. mc_add 'URL_EXTXT'. mc_add 'URL_TEXTS'.
  mc_add 'USER_CATT'. mc_add 'USER_COMP'. mc_add 'USER_DIR'.
  mc_add 'USER_KOMP'. mc_add 'USERINFO_STORAGE'. mc_add 'USEXIT'.
  mc_add 'USKRI'. mc_add 'USKRIA'. mc_add 'USKRIAT'.
  mc_add 'USOB_AUTHVALTRC'. mc_add 'USOB_CONTAINER'.
  mc_add 'USOB_CONTAINERT'. mc_add 'USOB_MOD'. mc_add 'USOB_SM'.
  mc_add 'USOB_TIMESTAMP'. mc_add 'USOBAUTHINACTIVE'.
  mc_add 'USOBAUTHSTART'. mc_add 'USOBHASH'. mc_add 'USOBJEXIT'.
  mc_add 'USOBRESP'. mc_add 'USOBT'. mc_add 'USOBT_BACK'.
  mc_add 'USOBT_C'. mc_add 'USOBT_C_BACK'. mc_add 'USOBT_CD'.
  mc_add 'USOBT_DUP'. mc_add 'USOBT_TSTMP'. mc_add 'USOBX'.
  mc_add 'USOBX_BACK'. mc_add 'USOBX_C'. mc_add 'USOBX_C_BACK'.
  mc_add 'USOBX_CD'. mc_add 'USOBX_DUP'. mc_add 'USOBX_MOD'.
  mc_add 'USOBX_REV'. mc_add 'USOBX_TSTMP'. mc_add 'USOBXFLAGS'.
  mc_add 'USOGR'. mc_add 'USOGT'. mc_add 'USORG'. mc_add 'USOTT'.
  mc_add 'USR_CUST_SYSTEM'. mc_add 'USR_DBMS_SYSTEM'.
  mc_add 'USR_FLAGS'. mc_add 'USR40'. mc_add 'USREXTIDT'.
  mc_add 'USREXTIDTT'. mc_add 'USRFLD'. mc_add 'USRFLDDEF'.
  mc_add 'USRFLDGRP'. mc_add 'USRFLDT'. mc_add 'USRFLDVAL'.
  mc_add 'USTUD'. mc_add 'USVAR'. mc_add 'USVART'. mc_add 'UTAB'.
  mc_add 'UTCMP'. mc_add 'UVERS_HIST'. mc_add 'UWLPARAMETERS'.
  mc_add 'UWLUSERDB'. mc_add 'VAL_AKH'. mc_add 'VAL_AKHT'.
  mc_add 'VARICONCI'. mc_add 'VARID_CI'. mc_add 'VARIDESCCI'.
  mc_add 'VARIS_CI'. mc_add 'VARIT_CI'. mc_add 'VBDATA'.
  mc_add 'VBERROR'. mc_add 'VBHDR'. mc_add 'VBIC_APP'.
  mc_add 'VBIC_APP_MS_ASGN'. mc_add 'VBIC_GC_SERV'.
  mc_add 'VBIC_MAP_PRODUCT'. mc_add 'VBIC_MAP_SERVER'.
  mc_add 'VBIC_MAP_STACK'. mc_add 'VBIC_MSTACK_ASGN'.
  mc_add 'VBIC_PROJECTIONS'. mc_add 'VBIC_PROP_CONT'.
  mc_add 'VBIC_REST_HNDLR'. mc_add 'VBIC_SERV_URL'. mc_add 'VBLOG'.
  mc_add 'VBMOD'. mc_add 'VBWRK'. mc_add 'VCLDIR'. mc_add 'VCLDIRT'.
  mc_add 'VCLMF'. mc_add 'VCLSTRUC'. mc_add 'VCLSTRUCT'.
  mc_add 'VCLSTRUDEP'. mc_add 'VEPADMINFLAG'. mc_add 'VEPCROSSREF'.
  mc_add 'VEPELEMTYPE'. mc_add 'VEPELETYPSOAP'. mc_add 'VEPENDPOINT'.
  mc_add 'VEPFAULT'. mc_add 'VEPFIELDREF'. mc_add 'VEPFUNCSOAPEXT'.
  mc_add 'VEPFUNCST'. mc_add 'VEPFUNCTION'. mc_add 'VEPGENCLASS'.
  mc_add 'VEPHEADER'. mc_add 'VEPPARAMETER'. mc_add 'VEPPARAMIDDEFS'.
  mc_add 'VEPPARAMIDS'. mc_add 'VEPPARASOAPEXT'. mc_add 'VEPPLUGINOBJS'.
  mc_add 'VEPPLUGINOBJST'. mc_add 'VEPSTRUTYPE'. mc_add 'VEPTABLETYPE'.
  mc_add 'VEPTABTYPSOAP'. mc_add 'VEPTYPE'. mc_add 'VEPTYPESOAPEXT'.
  mc_add 'VEPTYPEST'. mc_add 'VEPVISOAPEXT'. mc_add 'VERS_IFLDS'.
  mc_add 'VERS_TABS'. mc_add 'VERS_UDEFS'. mc_add 'VERSNTAB'.
  mc_add 'VERSOBJ'. mc_add 'VERSOBJ_ALIAS'. mc_add 'VERSOBJ_ALIAS_IN'.
  mc_add 'VERSOBJ_TMP'. mc_add 'VERSSUBOBJ'. mc_add 'VFS_FAV_OBJECTS'.
  mc_add 'VFS_FAV_USERS'. mc_add 'VFS_FAVORITES'. mc_add 'VFS_TEMP'.
  mc_add 'VGTST'. mc_add 'VGTX1'. mc_add 'VGTX2'. mc_add 'VIEWSYN'.
  mc_add 'VORINDX'. mc_add 'VORMT'. mc_add 'VORMZ'. mc_add 'VRS_AR_IDX'.
  mc_add 'VRS_CMP_RESULT'. mc_add 'VRS_SYST_COMP'. mc_add 'VRSD'.
  mc_add 'VRSMODISRC'. mc_add 'VRSSC_ATTR'. mc_add 'VRSSC_CTGRS'.
  mc_add 'VRSSC_OBJ_LIST'. mc_add 'VRSSC_PFL_HEAD'.
  mc_add 'VRSSC_PROFILE'. mc_add 'VRSSC_R_CLASHES'.
  mc_add 'VRSSC_TADIR'. mc_add 'VRSTABC'. mc_add 'VRSTC_CONFIGS'.
  mc_add 'VRSTC_IDS'. mc_add 'VRSTC_RES_INFO'. mc_add 'VRSTC_RESULTS'.
  mc_add 'VRSTC_TABLE_EXCL'. mc_add 'VRSX'. mc_add 'VRSX2'.
  mc_add 'VRSX3'. mc_add 'VRSX4'. mc_add 'VRSX5'. mc_add 'VSCAN_GROUP'.
  mc_add 'VSCAN_GROUP_P'. mc_add 'VSCAN_GROUPT'. mc_add 'VSCAN_PARAM'.
  mc_add 'VSCAN_PARAMT'. mc_add 'VSCAN_SERVER'. mc_add 'VSYNC'.
  mc_add 'VTEST10'. mc_add 'VTEST11'. mc_add 'W3BHTMCON'.
  mc_add 'W3BHTMCONT'. mc_add 'W3BHTMFUNC'. mc_add 'W3BHTMFUNT'.
  mc_add 'W3BHTMPARA'. mc_add 'W3BHTMPART'. mc_add 'W3BHTMVALT'.
  mc_add 'W3BHTMVALU'. mc_add 'W3BHTMWIZT'. mc_add 'W3BHTMWIZZ'.
  mc_add 'W3CONTYPES'. mc_add 'W3CREATPAT'. mc_add 'W3CROSS'.
  mc_add 'W3DEVAPP'. mc_add 'W3DEVTYPE'. mc_add 'W3DEVTYPT'.
  mc_add 'W3ERROBJECTS'. mc_add 'W3GENSTYLES'. mc_add 'W3HEXCONV'.
  mc_add 'W3HTMLT'. mc_add 'W3HTMLTAGS'. mc_add 'W3HTRCT'.
  mc_add 'W3JSCRT'. mc_add 'W3MIMET'. mc_add 'W3MODTYPES'.
  mc_add 'W3MODTYPET'. mc_add 'W3OBJLOINF'. mc_add 'W3PUBDIR'.
  mc_add 'W3SERINTP'. mc_add 'W3SERINTPT'. mc_add 'W3SERVICET'.
  mc_add 'W3SPAR'. mc_add 'W3SPART'. mc_add 'WBACTINDEX'.
  mc_add 'WBACTIONS'. mc_add 'WBACTIONST'. mc_add 'WBBGRFCTMP'.
  mc_add 'WBCROSSGT'. mc_add 'WBCROSSGTI'. mc_add 'WBCROSSGTX'.
  mc_add 'WBCROSSI'. mc_add 'WBEXTTYPES'. mc_add 'WBINDEX'.
  mc_add 'WBO_ADM'. mc_add 'WBO_ADMT'. mc_add 'WBOATTR'.
  mc_add 'WBOATTRT'. mc_add 'WBOBJECT_DATA'. mc_add 'WBOBJECTTYPES'.
  mc_add 'WBOBJECTTYPES_T'. mc_add 'WBOBJTPS_ACTIONS'.
  mc_add 'WBOBJTYPES'. mc_add 'WBOBJTYPES_SCOPE'.
  mc_add 'WBOBJTYPES_USERS'. mc_add 'WBOBJTYPT'. mc_add 'WBOSYSCNTRL'.
  mc_add 'WBOSYSCUST'. mc_add 'WBREGISTRY'. mc_add 'WBREGISTRY_AUNIT'.
  mc_add 'WBREGISTRY_P'. mc_add 'WBREGISTRY_PT'. mc_add 'WBTABLE'.
  mc_add 'WBTESTCROSS'. mc_add 'WBTOOLTYPE'. mc_add 'WBTT_CONTENT'.
  mc_add 'WBTT_METADATA'. mc_add 'WBWL_TAB'. mc_add 'WBXMLCUST'.
  mc_add 'WCMTXFLD'. mc_add 'WCMTXLOIOT'. mc_add 'WCMTXLOPR'.
  mc_add 'WCMTXLORE'. mc_add 'WCMTXLORI'. mc_add 'WDBOOK_ARTICLE'.
  mc_add 'WDELVERS'. mc_add 'WDG_GEN_LOG'. mc_add 'WDG_UR_CONTROL'.
  mc_add 'WDG_UR_ENUM'. mc_add 'WDG_UR_ENUM_VAL'. mc_add 'WDG_UR_EVENT'.
  mc_add 'WDG_UR_EVENT_PAR'. mc_add 'WDG_UR_PROPERTY'.
  mc_add 'WDG_UR_STYLE'. mc_add 'WDG_UR_WEB_ICOL'.
  mc_add 'WDG_UR_WEB_ICOLT'. mc_add 'WDG_UR_WEB_ICON'.
  mc_add 'WDG_UR_WEB_ICONT'. mc_add 'WDG_UR_WEBICONT'.
  mc_add 'WDG_URNW7_TXT'. mc_add 'WDR_ACF_WL_CTRL'.
  mc_add 'WDR_ADAPTER_PROP'. mc_add 'WDR_ADP_AGGR_MP'.
  mc_add 'WDR_ADP_AGR_P_MP'. mc_add 'WDR_ADP_CHNGLOG'.
  mc_add 'WDR_ADP_CONST_MP'. mc_add 'WDR_ADP_ENUM_MP'.
  mc_add 'WDR_ADP_EVENT_MP'. mc_add 'WDR_ADP_GET_MP'.
  mc_add 'WDR_ADP_UR_EVENT'. mc_add 'WDR_ADP_UR_EVPAR'.
  mc_add 'WDR_ELEM_APT_MAP'. mc_add 'WDR_ENUM_MP_DEF'.
  mc_add 'WDR_ENUM_MP_ITEM'. mc_add 'WDR_MIME_FILE'.
  mc_add 'WDR_MIME_PCKG'. mc_add 'WDR_MIME_PCKG_DP'.
  mc_add 'WDR_REC_PLG_CND'. mc_add 'WDR_REC_PLG_CNDT'.
  mc_add 'WDR_REC_PLUGIN'. mc_add 'WDR_REC_PLUGINT'.
  mc_add 'WDR_RR_LOAD'. mc_add 'WDR_TEST_DDSHLP'.
  mc_add 'WDR_TEST_SOALPHA'. mc_add 'WDR_TEST_UIELEM'.
  mc_add 'WDR_UIEL_ADAPTER'. mc_add 'WDR_USAGE_LOG'.
  mc_add 'WDRCSFCHECK'. mc_add 'WDRDEMOIFBAEMAIL'.
  mc_add 'WDY_APP_PROP_DEF'. mc_add 'WDY_APP_PROP_DFT'.
  mc_add 'WDY_APP_PROPERTY'. mc_add 'WDY_APPLICATION'.
  mc_add 'WDY_APPLICATIONT'. mc_add 'WDY_CFG_CHANGES'.
  mc_add 'WDY_CFG_CMP_WUL'. mc_add 'WDY_CFG_ENH_HIDE'.
  mc_add 'WDY_CHIP_DEF'. mc_add 'WDY_CHIP_DEFT'.
  mc_add 'WDY_CHIP_PARAM'. mc_add 'WDY_CHIP_PARAMT'.
  mc_add 'WDY_CHIP_PORT'. mc_add 'WDY_CHIP_PORTT'.
  mc_add 'WDY_CHIP_TAG'. mc_add 'WDY_COMPO_USAGE'.
  mc_add 'WDY_COMPONENT'. mc_add 'WDY_COMPONENTT'.
  mc_add 'WDY_CONF_DELA'. mc_add 'WDY_CONF_DELC'.
  mc_add 'WDY_CONFIG_APPL'. mc_add 'WDY_CONFIG_APPT'.
  mc_add 'WDY_CONFIG_COMP'. mc_add 'WDY_CONFIG_COMPT'.
  mc_add 'WDY_CONFIG_DATA'. mc_add 'WDY_CONFIG_DATT'.
  mc_add 'WDY_CONTROLLER'. mc_add 'WDY_CONTROLLERT'.
  mc_add 'WDY_CTLR_COMPO'. mc_add 'WDY_CTLR_COMPOT'.
  mc_add 'WDY_CTLR_EXC'. mc_add 'WDY_CTLR_EXCT'.
  mc_add 'WDY_CTLR_PARAM'. mc_add 'WDY_CTLR_PARAMT'.
  mc_add 'WDY_CTLR_TEXT'. mc_add 'WDY_CTLR_USAGE'.
  mc_add 'WDY_CTX_ATT_USE'. mc_add 'WDY_CTX_ATTRIB'.
  mc_add 'WDY_CTX_CHANGED'. mc_add 'WDY_CTX_MAPPING'.
  mc_add 'WDY_CTX_NODE'. mc_add 'WDY_DC_SETTINGS'.
  mc_add 'WDY_DEPR_OBJ_TXT'. mc_add 'WDY_EXT_CTLR_USE'.
  mc_add 'WDY_EXT_CTX_MAP'. mc_add 'WDY_FIELDGROUP'.
  mc_add 'WDY_IDE_INBOXES'. mc_add 'WDY_INTF_IMPLEM'.
  mc_add 'WDY_IOBOUND_PLGT'. mc_add 'WDY_IOBOUND_PLUG'.
  mc_add 'WDY_LIBRARY_USE'. mc_add 'WDY_MODEL'.
  mc_add 'WDY_MODEL_CLASS'. mc_add 'WDY_MODEL_REL'.
  mc_add 'WDY_MODEL_RELROL'. mc_add 'WDY_NAV_LINK'.
  mc_add 'WDY_NAV_TARGREF'. mc_add 'WDY_NEW_LS_OBJ'.
  mc_add 'WDY_P13N_AG_DEFT'. mc_add 'WDY_P13N_EL_DEFT'.
  mc_add 'WDY_PLUG_PARAM'. mc_add 'WDY_PLUG_PARAMT'.
  mc_add 'WDY_TEXTFILE_REF'. mc_add 'WDY_TRACE_ACTVTY'.
  mc_add 'WDY_UI_AGGR_DEF'. mc_add 'WDY_UI_AGGR_DEFT'.
  mc_add 'WDY_UI_AGGR_LD'. mc_add 'WDY_UI_AP13N_DEF'.
  mc_add 'WDY_UI_CTX_BIND'. mc_add 'WDY_UI_DDIC_BIND'.
  mc_add 'WDY_UI_ELEM_DEF'. mc_add 'WDY_UI_ELEM_DEFT'.
  mc_add 'WDY_UI_ELEM_DOC'. mc_add 'WDY_UI_ELEMENT'.
  mc_add 'WDY_UI_ENUM_DEF'. mc_add 'WDY_UI_ENUM_VAL'.
  mc_add 'WDY_UI_ENUM_VALT'. mc_add 'WDY_UI_EVENT_DEF'.
  mc_add 'WDY_UI_EVPAR_DEF'. mc_add 'WDY_UI_EVT_BIND'.
  mc_add 'WDY_UI_INTERFACE'. mc_add 'WDY_UI_INTF_IMPL'.
  mc_add 'WDY_UI_LIB_USAGE'. mc_add 'WDY_UI_LIBRARY'.
  mc_add 'WDY_UI_PP13N_DEF'. mc_add 'WDY_UI_PP13N_TVA'.
  mc_add 'WDY_UI_PROP_DEF'. mc_add 'WDY_UI_PROP_DEFT'.
  mc_add 'WDY_UI_PROP_RAL'. mc_add 'WDY_UI_PROPERTY'.
  mc_add 'WDY_URL_PROP_DEF'. mc_add 'WDY_VAREA_DEF'.
  mc_add 'WDY_VD_CONTEXT'. mc_add 'WDY_VD_CTX_NEW'.
  mc_add 'WDY_VD_DESDEFT'. mc_add 'WDY_VD_DESIGNDEF'.
  mc_add 'WDY_VD_GUIDDELTA'. mc_add 'WDY_VD_HTML'. mc_add 'WDY_VIEW'.
  mc_add 'WDY_VIEW_CNTR'. mc_add 'WDY_VIEW_CNTRT'.
  mc_add 'WDY_VIEW_TEXT'. mc_add 'WDY_VIEWT'. mc_add 'WDY_VS_PROP_DEF'.
  mc_add 'WDY_VS_PROPERTY'. mc_add 'WDY_VSET_DEF'.
  mc_add 'WDY_VSH_NODE'. mc_add 'WDY_VSH_PHOLDER'.
  mc_add 'WDY_WB_C_TEMPL'. mc_add 'WDY_WB_GENINF_VS'.
  mc_add 'WDY_WB_GENINFO'. mc_add 'WDY_WB_SOURCEMAP'.
  mc_add 'WDY_WB_VD_ACCCHK'. mc_add 'WDY_WB_VD_ADD_F'.
  mc_add 'WDY_WB_VD_CD'. mc_add 'WDY_WB_VD_CHECK'.
  mc_add 'WDY_WB_VD_CTX'. mc_add 'WDY_WB_VD_CTXPRO'.
  mc_add 'WDY_WB_VD_FAV'. mc_add 'WDY_WB_VD_IDE'.
  mc_add 'WDY_WB_VD_IMPLIC'. mc_add 'WDY_WB_VD_TXT'.
  mc_add 'WDY_WINDOW'. mc_add 'WDY_WINDOWT'. mc_add 'WDYADT_VE_DEFS'.
  mc_add 'WEBSERVICE_TAB'. mc_add 'WFAS'. mc_add 'WFERR'.
  mc_add 'WFIUT'. mc_add 'WFPRC'. mc_add 'WFQUE'. mc_add 'WFSTO'.
  mc_add 'WIZ_DEF'. mc_add 'WIZ_MODUL'. mc_add 'WIZ_STAT'.
  mc_add 'WIZ_TEXT'. mc_add 'WSHEADER'. mc_add 'WSHEADER_DARK'.
  mc_add 'WSRM_EH'. mc_add 'WSRM_EH_SID'. mc_add 'WSRM_TXH_WATCHER'.
  mc_add 'WSS_TEMPLATE'. mc_add 'WSS_TEMPLATE_T'. mc_add 'WSSOAPPROP'.
  mc_add 'WSSOAPPROP_DARK'. mc_add 'WSURLREF'. mc_add 'WVOBJECTTYPES'.
  mc_add 'WVOBJECTTYPES_T'. mc_add 'WVOBJTYPES_SCOPE'. mc_add 'WWWDATA'.
  mc_add 'WWWFUNC'. mc_add 'WWWLANGRES'. mc_add 'WWWPARAMS'.
  mc_add 'WWWREPS'.
  SORT lt_shadow BY size DESCENDING.
ENDFORM.                    "fill_shadow_list
*&---------------------------------------------------------------------*
*&      Form  get_anydb_size
*&---------------------------------------------------------------------*
*       Get used size on disk of analysed database
*----------------------------------------------------------------------*
FORM get_anydb_size CHANGING anydbsize "bytes
                             anydbsizedate.
  DATA: td110         LIKE td110_stru,
        lt_db6pmdb02  TYPE TABLE OF db6pmdb02,
        ls_db6pmdb02  TYPE db6pmdb02,
        db4size       TYPE db4size,
        l_db_sizeinfo TYPE mssdbsizeinfo.

  MESSAGE s090(sada) WITH 'Read analysed db disk size'.     "#EC NOTEXT
  TRY.
      CASE sy-dbsys(3).
        WHEN 'MSS'.
          CALL FUNCTION 'MSS_GET_DB_SIZE_INFO'
            IMPORTING
              db_sizeinfo          = l_db_sizeinfo "mb
            EXCEPTIONS
              not_running_on_mssql = 1
              db_error             = 2
              internal_error       = 3
              db_not_found         = 4
              no_db_access         = 5
              OTHERS               = 6.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
          anydbsize = l_db_sizeinfo-db_alloc.
          anydbsize = anydbsize * 1024 * 1024. "bytes
        WHEN 'DB2'.
        WHEN 'ORA'.
          CALL FUNCTION 'DB02_ORA_FILL_TD110'
            IMPORTING
              td110 = td110. "kb
          anydbsizedate = td110-datum.
          anydbsize = td110-tssi - td110-tsfr.
          anydbsize = anydbsize * 1024. "bytes
          anydbsizedate = td110-datum.
        WHEN 'SQL'.
        WHEN 'ADA'.
        WHEN 'INF'.
        WHEN 'DB4'.
          CALL FUNCTION 'GET_DB4_DBSIZE'
            IMPORTING
              db4_dbsize                    = db4size "kb
            EXCEPTIONS
              wrong_dbsys                   = 1
              dbstattdb4_empty              = 2
              api_qwcrssts_error            = 3
              dbstat_empty_api_error        = 4
              import_from_moni_error        = 5
              dbstat_empty_moni_error       = 6
              api_error_moni_error          = 7
              dbstat_empty_api_err_moni_err = 8
              consistency_check_error       = 9
              api_error                     = 10
              OTHERS                        = 11.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
          anydbsize = db4size-db_size.
          anydbsize = anydbsize * 1024.
        WHEN 'DB6'.
          CALL FUNCTION 'DB6_HIS_OVERVIEW'
            TABLES
              it_db6pmdb02     = lt_db6pmdb02 "kb
            EXCEPTIONS
              no_history_found = 1
              OTHERS           = 2.
          IF sy-subrc <> 0.
            RETURN.
          ELSE.
            READ TABLE lt_db6pmdb02 INTO ls_db6pmdb02 INDEX 1.
          ENDIF.
          anydbsize = ls_db6pmdb02-totalkb - ls_db6pmdb02-freekb.
          anydbsize = anydbsize * 1024.
        WHEN 'HDB'.
          EXEC SQL.
            select sum(used_size) into :anydbsize
              from M_DATA_VOLUME_STATISTICS
          ENDEXEC.                                      "#EC CI_EXECSQL
      ENDCASE.
    CATCH cx_sy_dyn_call_illegal_func
          cx_root.
      CLEAR anydbsize.
  ENDTRY.

ENDFORM.                    "get_anydb_size

*&---------------------------------------------------------------------*
*&      Form  calculate_subtotals_calib
*&---------------------------------------------------------------------*
*       Calculate detailled sub totals.
*----------------------------------------------------------------------*
FORM calculate_subtotals_calib.

  ls_cs_cal-column_name = 'trexexternalkey'.                "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "1
  ls_cs_cal-column_name = 'rowid'.                          "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "2
  ls_cs_cal-column_name = 'trex_udiv'.                      "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "3
  ls_cs_cal-column_name = 'Unique indexes'.                 "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal.  "4
  ls_cs_cal-column_name = 'Entreprise Search'.              "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "5
  ls_cs_cal-column_name = 'Concatenated JE indexes'.        "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "6
  ls_cs_cal-column_name = 'LOB Columns'.                    "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "7
  ls_cs_cal-column_name = 'Hybrid LOB Columns'.             "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "8
  ls_cs_cal-column_name = 'U Columns'.                      "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "9
  ls_cs_cal-column_name = 'N Columns'.                      "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "10
  ls_cs_cal-column_name = 'P Columns'.                      "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "11
  ls_cs_cal-column_name = 'Low opt. Columns'.               "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "12
  ls_cs_cal-column_name = 'Other Columns'.                  "#EC NOTEXT
  APPEND ls_cs_cal TO lt_cs_cal. "13

  LOOP AT gt_tables ASSIGNING <item>.
    IF <item>-store = 'CS'.
      LOOP AT <item>-cs_cols ASSIGNING <cs_size>.
        CASE <cs_size>-column_name.
          WHEN '$trexexternalkey$'.
            READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 1.
          WHEN '$rowid$'.
            READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 2.
          WHEN '$trex_udiv$'.
            READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 3.
          WHEN OTHERS.
            IF <cs_size>-column_name(3) = '$uc_'.
              READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 4.
            ELSEIF <cs_size>-column_name(13) = '$_SYS_SHADOW_'
                 OR <cs_size>-column_name(5) = '$esh:'.
              READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 5.
            ELSE.
              IF <cs_size>-column_name(1) = '$'.
                READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 6.
              ENDIF.
              IF <cs_size>-lob_size_byte > 0.
                IF <cs_size>-dlob_count = 0.
                  READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 7.
                ELSE.
                  READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 8.
                ENDIF.
              ELSE.
                CASE <cs_size>-has_index.
                  WHEN 'U'.
                    READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 9.
                  WHEN 'N'.
                    READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 10.
                  WHEN 'P'.
                    READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 11.
                  WHEN OTHERS.
                    IF <cs_size>-opt_size_byte = 0.
                      READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 12.
                    ELSE.
                      READ TABLE lt_cs_cal ASSIGNING <cscal> INDEX 13.
                    ENDIF.
                ENDCASE.
              ENDIF.
            ENDIF.
        ENDCASE.
        <cscal>-est_ms_main  = <cscal>-est_ms_main  +
<cs_size>-est_ms_main.
        <cscal>-est_ms_dict  = <cscal>-est_ms_dict  +
<cs_size>-est_ms_dict.
        <cscal>-est_ms_data  = <cscal>-est_ms_data  +
<cs_size>-est_ms_data.
        <cscal>-est_ms_index = <cscal>-est_ms_index +
<cs_size>-est_ms_index.
        <cscal>-est_ms_misc  = <cscal>-est_ms_misc  +
<cs_size>-est_ms_misc.
        <cscal>-ms_main  = <cscal>-ms_main  + <cs_size>-ms_main.
        <cscal>-ms_dict  = <cscal>-ms_dict  + <cs_size>-ms_dict.
        <cscal>-ms_data  = <cscal>-ms_data  + <cs_size>-ms_data.
        <cscal>-ms_index = <cscal>-ms_index + <cs_size>-ms_index.
        <cscal>-ms_misc  = <cscal>-ms_misc  + <cs_size>-ms_misc.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "calculate_subtotals_calib
*&---------------------------------------------------------------------*
*& FORM  collect_stats
*& Control the RFC calls, packaging and parallelism
*&
*&---------------------------------------------------------------------*

FORM collect_stats USING mode.
  DATA: nb_tables TYPE i,
        l_count   TYPE p DECIMALS 2.

  DESCRIBE TABLE gt_input LINES nb_tables.
  " Adapt parallel if bigger than # of tables.
  IF nb_tables < p_paral.
    p_paral = nb_tables.
  ENDIF.
  "Adapt bucket size if small selection of tables
  IF bucket_size > nb_tables.
    bucket_size =  nb_tables / p_paral + 1.
  ENDIF.
  " Create buckets. Cluster/pool tables must be grouped in the same.
  LOOP AT gt_input ASSIGNING <item>.
    IF p_db_table <> <item>-db_table.
      p_db_table = <item>-db_table.
      l_count = l_count + 1.
    ENDIF.
    <item>-bucket = l_count DIV bucket_size.
  ENDLOOP.
  nb_buckets = <item>-bucket + 1.
  CLEAR p_db_table.

  " call one RFC per bucket
  CONCATENATE '1ST' mode INTO mode.
  LOOP AT gt_input ASSIGNING <item>.
    IF <item>-bucket <> bucket.
      PERFORM do_rfc TABLES gt_bucket
                      USING bucket
                            wa_techd-prec
                            mode.
      bucket = <item>-bucket.
      CALL FUNCTION 'DB_COMMIT'.
      WAIT UNTIL l_act < p_paral.
      CLEAR gt_bucket.
    ENDIF.
    APPEND <item> TO gt_bucket.
  ENDLOOP.

  " perform last call.
  CALL FUNCTION 'DB_COMMIT'.
  WAIT UNTIL l_act < p_paral.
  PERFORM do_rfc TABLES gt_bucket
                  USING bucket
                        wa_techd-prec
                        mode.
  CALL FUNCTION 'DB_COMMIT'.
  WAIT UNTIL l_sent = l_recvd.

  CLEAR: gt_bucket, l_act, l_recvd, l_sent, bucket.

  " retry erroneous in single RFC.
  IF lt_retry IS NOT INITIAL.
    CONCATENATE 'RET' mode+3 INTO mode.
    MESSAGE s090(sada) WITH 'Warning: Error encountered. Check ST22.'.
                                                            "#EC NOTEXT
    DESCRIBE TABLE lt_retry LINES l_retrycnt.
    MESSAGE s090(sada) WITH 'Reprocessing of erroneous calls'
                           ' started'.                      "#EC NOTEXT
    LOOP AT lt_retry ASSIGNING <item>.
      IF p_db_table <> <item>-db_table AND p_db_table IS NOT INITIAL.
        PERFORM do_rfc TABLES gt_bucket
                        USING bucket
                              wa_techd-prec
                              mode.
        CALL FUNCTION 'DB_COMMIT'.
        WAIT UNTIL l_act < p_paral.
        CLEAR gt_bucket.
      ENDIF.
      p_db_table = <item>-db_table.
      APPEND <item> TO gt_bucket.
    ENDLOOP.

    " perform last call.
    CALL FUNCTION 'DB_COMMIT'.
    WAIT UNTIL l_act < p_paral.
    PERFORM do_rfc TABLES gt_bucket
                    USING bucket
                          wa_techd-prec
                          mode.
    CALL FUNCTION 'DB_COMMIT'.
    WAIT UNTIL l_sent = l_recvd.
    MESSAGE s090(sada) WITH 'Reprocessing of erroneous calls'
                           ' finished'.                     "#EC NOTEXT
  ENDIF.
  CLEAR nb_buckets.
  REFRESH: gt_input, gt_bucket, lt_retry.
  SORT gt_tables BY table_name.
  MESSAGE s090(sada) WITH 'End of statistics collection'.   "#EC NOTEXT
ENDFORM.                    "collect_stats

*&---------------------------------------------------------------------*
*&      Form  rs_changes_s4
*&---------------------------------------------------------------------*
*       Estimates memory reduction due to move to column store in S4
*----------------------------------------------------------------------*
FORM rs_changes_s4.
  DATA:
    lt_rs2cstab TYPE ty_t_hana_db_size,
    ls_rs2cstab TYPE ty_hana_db_size.
  FIELD-SYMBOLS:
   <rs2cs>     TYPE ty_hana_db_size.

  PERFORM fill_row_list.
  LOOP AT gt_tables ASSIGNING <item> WHERE store = 'RS'.
    tabix = sy-tabix.
    READ TABLE gt_rownots4_tab WITH KEY <item>-table_name
      TRANSPORTING NO FIELDS BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR <item>-bucket.
      APPEND <item> TO gt_input.
      DELETE gt_tables INDEX tabix.
    ENDIF.
  ENDLOOP.

  IF gt_input IS NOT INITIAL.
    MESSAGE s090(sada) WITH
       'Statistics collection for S4 stores changes'.       "#EC NOTEXT
    mode = 'CAL'.
    PERFORM collect_stats USING mode.

    LOOP AT gt_columns ASSIGNING <columns>.
      READ TABLE gt_tables ASSIGNING <item>
         WITH KEY table_name = <columns>-table_name
                            BINARY SEARCH.
      MOVE-CORRESPONDING <columns> TO gs_cs_size.
      APPEND gs_cs_size TO <item>-cs_cols.
      READ TABLE lt_rs2cstab ASSIGNING <rs2cs>
         WITH KEY table_name = <item>-table_name.
      IF sy-subrc <> 0.
        <item>-store = 'CS'.
        MOVE-CORRESPONDING <item> TO ls_rs2cstab.
        APPEND ls_rs2cstab TO lt_rs2cstab.
      ELSE.
        APPEND gs_cs_size TO <rs2cs>-cs_cols.
      ENDIF.
      CLEAR: gs_cs_size.
    ENDLOOP.
    CLEAR: gs_cs_size.
    REFRESH gt_columns.

    PERFORM sizing_calculation CHANGING lt_rs2cstab.

    "Now move lt_rs2cstab to back to gt_tables.
    LOOP AT lt_rs2cstab ASSIGNING <rs2cs>.
      READ TABLE gt_tables ASSIGNING <item>
        WITH KEY table_name = <rs2cs>-table_name.
      CLEAR <item>-cs_cols.
      "Move est. to real.
      LOOP AT <rs2cs>-cs_cols ASSIGNING <cs_size>.
        <cs_size>-ms_main  = <cs_size>-est_ms_main.
        <cs_size>-ms_dict  = <cs_size>-est_ms_dict.
        <cs_size>-ms_index = <cs_size>-est_ms_index.
        <cs_size>-ms_data  = <cs_size>-est_ms_data.
        <cs_size>-ms_misc  = <cs_size>-est_ms_misc.
        <cs_size>-ms_hl    = <cs_size>-est_ms_hl.
      ENDLOOP.
      <item>-cs_cols = <rs2cs>-cs_cols.
    ENDLOOP.
    SORT gt_tables BY table_name.
  ENDIF.

ENDFORM.                    "rs_changes_s4

*&---------------------------------------------------------------------*
*&      Form calculate_scaleout
*&---------------------------------------------------------------------*
*       Calculate Scale-Out group size
*----------------------------------------------------------------------*

FORM calculate_scaleout_group.

  DATA: ls_sogrp  TYPE ty_sogrp,
        sizefield TYPE fieldname.
  FIELD-SYMBOLS:
    <sizefs> TYPE ty_hana_db_size-est_size.

  DEFINE mc_add_s.
    READ TABLE gt_tables WITH KEY table_name = &1 ASSIGNING <item>.
    IF sy-subrc = 0.
      <item>-sogroup = &2.
    ENDIF.
  END-OF-DEFINITION.

  mc_add_s 'ACCTHD' 'ACC_DOC'.
  mc_add_s 'ACCTCR' 'ACC_DOC'.
  mc_add_s 'ACCTIT' 'ACC_DOC'.

  mc_add_s 'CDPOS' 'BC_CHDO'.
  mc_add_s 'CDHDR' 'BC_CHDO'.
  mc_add_s 'CDPOS_STR' 'BC_CHDO'.
  mc_add_s 'CDPOS_UID' 'BC_CHDO'.

  mc_add_s 'EDID4' 'BC_IDOC'.
  mc_add_s 'EDIDC' 'BC_IDOC'.
  mc_add_s 'EDIDS' 'BC_IDOC'.

  mc_add_s 'BALDAT' 'BC_SBAL'.
  mc_add_s 'BAL_INDX' 'BC_SBAL'.
  mc_add_s 'BALHDR' 'BC_SBAL'.

  mc_add_s 'SWWCNTP0' 'BC_WORKITEM'.
  mc_add_s 'SWP_JOIN' 'BC_WORKITEM'.
  mc_add_s 'SWP_NODEWI' 'BC_WORKITEM'.
  mc_add_s 'SWPNODE' 'BC_WORKITEM'.
  mc_add_s 'SWPNODELOG' 'BC_WORKITEM'.
  mc_add_s 'SWPSTEPLOG' 'BC_WORKITEM'.
  mc_add_s 'SWW_CONT' 'BC_WORKITEM'.
  mc_add_s 'SWW_CONTOB' 'BC_WORKITEM'.
  mc_add_s 'SWWLOGHIST' 'BC_WORKITEM'.
  mc_add_s 'SWWORGTASK' 'BC_WORKITEM'.
  mc_add_s 'SWWUSERWI' 'BC_WORKITEM'.
  mc_add_s 'SWWWIAGENT' 'BC_WORKITEM'.
  mc_add_s 'SWWWIHEAD' 'BC_WORKITEM'.

  mc_add_s 'ACDOCA_M_EXTRACT' 'CKML'.
  mc_add_s 'CKMLCR' 'CKML'.
  mc_add_s 'CKMLPP' 'CKML'.
  mc_add_s 'MBEW' 'CKML'.

  mc_add_s 'SNWD_SO' 'EPM_SO'.
  mc_add_s 'SNWD_GI_HEAD' 'EPM_SO'.
  mc_add_s 'SNWD_GI_I' 'EPM_SO'.
  mc_add_s 'SNWD_SO_I' 'EPM_SO'.
  mc_add_s 'SNWD_SO_INV_HEAD' 'EPM_SO'.
  mc_add_s 'SNWD_SO_INV_ITEM' 'EPM_SO'.
  mc_add_s 'SNWD_SO_SL' 'EPM_SO'.
  mc_add_s 'SNWD_TEXT_KEY' 'EPM_SO'.
  mc_add_s 'SNWD_TEXTS' 'EPM_SO'.

  mc_add_s 'ACDOCA' 'FI_DOCUMENT'.
  mc_add_s 'AGKO' 'FI_DOCUMENT'.
  mc_add_s 'ANEA' 'FI_DOCUMENT'.
  mc_add_s 'ANEK' 'FI_DOCUMENT'.
  mc_add_s 'ANEP' 'FI_DOCUMENT'.
  mc_add_s 'ANLC' 'FI_DOCUMENT'.
  mc_add_s 'ANLP' 'FI_DOCUMENT'.
  mc_add_s 'BFIT_A' 'FI_DOCUMENT'.
  mc_add_s 'BFIT_A0' 'FI_DOCUMENT'.
  mc_add_s 'BFO_A_RA' 'FI_DOCUMENT'.
  mc_add_s 'BFOD_A' 'FI_DOCUMENT'.
  mc_add_s 'BFOD_AB' 'FI_DOCUMENT'.
  mc_add_s 'BFOK_A' 'FI_DOCUMENT'.
  mc_add_s 'BFOK_AB' 'FI_DOCUMENT'.
  mc_add_s 'BKPF' 'FI_DOCUMENT'.
  mc_add_s 'BSAD' 'FI_DOCUMENT'.
  mc_add_s 'BSAD_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSAK' 'FI_DOCUMENT'.
  mc_add_s 'BSAK_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSAS' 'FI_DOCUMENT'.
  mc_add_s 'BSAS_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSBW' 'FI_DOCUMENT'.
  mc_add_s 'BSE_CLR' 'FI_DOCUMENT'.
  mc_add_s 'BSE_CLR_ASGMT' 'FI_DOCUMENT'.
  mc_add_s 'BSE_OIH' 'FI_DOCUMENT'.
  mc_add_s 'BSEC' 'FI_DOCUMENT'.
  mc_add_s 'BSED' 'FI_DOCUMENT'.
  mc_add_s 'BSEG' 'FI_DOCUMENT'.
  mc_add_s 'BSEG_ADD' 'FI_DOCUMENT'.
  mc_add_s 'BSEGC' 'FI_DOCUMENT'.
  mc_add_s 'BSET' 'FI_DOCUMENT'.
  mc_add_s 'BSID' 'FI_DOCUMENT'.
  mc_add_s 'BSID_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSIK' 'FI_DOCUMENT'.
  mc_add_s 'BSIK_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSIM' 'FI_DOCUMENT'.
  mc_add_s 'BSIM_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSIP' 'FI_DOCUMENT'.
  mc_add_s 'BSIS' 'FI_DOCUMENT'.
  mc_add_s 'BSIS_BCK' 'FI_DOCUMENT'.
  mc_add_s 'BSPL' 'FI_DOCUMENT'.
  mc_add_s 'BVOR' 'FI_DOCUMENT'.
  mc_add_s 'BWFI_AEDAT' 'FI_DOCUMENT'.
  mc_add_s 'BWFIAA_AEDAT_AS' 'FI_DOCUMENT'.
  mc_add_s 'BWFIAA_AEDAT_TR' 'FI_DOCUMENT'.
  mc_add_s 'CBPR' 'FI_DOCUMENT'.
  mc_add_s 'COBK' 'FI_DOCUMENT'.
  mc_add_s 'COEJ' 'FI_DOCUMENT'.
  mc_add_s 'COEJL' 'FI_DOCUMENT'.
  mc_add_s 'COEJT' 'FI_DOCUMENT'.
  mc_add_s 'COEP' 'FI_DOCUMENT'.
  mc_add_s 'COSP' 'FI_DOCUMENT'.
  mc_add_s 'COSP_BAK' 'FI_DOCUMENT'.
  mc_add_s 'COSS' 'FI_DOCUMENT'.
  mc_add_s 'COSS_BAK' 'FI_DOCUMENT'.
  mc_add_s 'ETXDCH' 'FI_DOCUMENT'.
  mc_add_s 'ETXDCI' 'FI_DOCUMENT'.
  mc_add_s 'ETXDCJ' 'FI_DOCUMENT'.
  mc_add_s 'FAAT_DOC_IT' 'FI_DOCUMENT'.
  mc_add_s 'FAAT_PLAN_VALUES' 'FI_DOCUMENT'.
  mc_add_s 'FAGL_BSBW_HISTRY' 'FI_DOCUMENT'.
  mc_add_s 'FAGL_R_SPL' 'FI_DOCUMENT'.
  mc_add_s 'FAGL_R_SPL_VAL' 'FI_DOCUMENT'.
  mc_add_s 'FAGL_SPLINFO' 'FI_DOCUMENT'.
  mc_add_s 'FAGL_SPLINFO_VAL' 'FI_DOCUMENT'.
  mc_add_s 'FAGLBSAS' 'FI_DOCUMENT'.
  mc_add_s 'FAGLBSAS_BCK' 'FI_DOCUMENT'.
  mc_add_s 'FAGLBSIS' 'FI_DOCUMENT'.
  mc_add_s 'FAGLBSIS_BCK' 'FI_DOCUMENT'.
  mc_add_s 'FAGLFLEXA' 'FI_DOCUMENT'.
  mc_add_s 'FAGLFLEXT' 'FI_DOCUMENT'.
  mc_add_s 'FAGLFLEXT_BCK' 'FI_DOCUMENT'.
  mc_add_s 'FIGLDOC' 'FI_DOCUMENT'.
  mc_add_s 'FMGLFLEXA' 'FI_DOCUMENT'.
  mc_add_s 'FMGLFLEXT' 'FI_DOCUMENT'.
  mc_add_s 'GLE_MCA_FDOCREF' 'FI_DOCUMENT'.
  mc_add_s 'GLFUNCT' 'FI_DOCUMENT'.
  mc_add_s 'GLPCA' 'FI_DOCUMENT'.
  mc_add_s 'GLPCC' 'FI_DOCUMENT'.
  mc_add_s 'GLPCO' 'FI_DOCUMENT'.
  mc_add_s 'GLPCP' 'FI_DOCUMENT'.
  mc_add_s 'GLPCT' 'FI_DOCUMENT'.
  mc_add_s 'GLT0' 'FI_DOCUMENT'.
  mc_add_s 'GLT0_BCK' 'FI_DOCUMENT'.
  mc_add_s 'GLT0_DIF' 'FI_DOCUMENT'.
  mc_add_s 'GLT1' 'FI_DOCUMENT'.
  mc_add_s 'GLT2' 'FI_DOCUMENT'.
  mc_add_s 'JVGLFLEXT' 'FI_DOCUMENT'.
  mc_add_s 'KNC1' 'FI_DOCUMENT'.
  mc_add_s 'KNC1_BCK' 'FI_DOCUMENT'.
  mc_add_s 'KNC3' 'FI_DOCUMENT'.
  mc_add_s 'KNC3_BCK' 'FI_DOCUMENT'.
  mc_add_s 'KNCX_DIF' 'FI_DOCUMENT'.
  mc_add_s 'KNKKF1' 'FI_DOCUMENT'.
  mc_add_s 'LFC1' 'FI_DOCUMENT'.
  mc_add_s 'LFC1_BCK' 'FI_DOCUMENT'.
  mc_add_s 'LFC3' 'FI_DOCUMENT'.
  mc_add_s 'LFC3_BCK' 'FI_DOCUMENT'.
  mc_add_s 'LFCX_DIF' 'FI_DOCUMENT'.
  mc_add_s 'PSGLFLEXT' 'FI_DOCUMENT'.
  mc_add_s 'REGUP' 'FI_DOCUMENT'.
  mc_add_s 'RF048' 'FI_DOCUMENT'.
  mc_add_s 'SIPT_BKPF' 'FI_DOCUMENT'.
  mc_add_s 'TTXI' 'FI_DOCUMENT'.
  mc_add_s 'TTXY' 'FI_DOCUMENT'.
  mc_add_s 'VBKPF' 'FI_DOCUMENT'.
  mc_add_s 'VBSEGD' 'FI_DOCUMENT'.
  mc_add_s 'VBSEGK' 'FI_DOCUMENT'.
  mc_add_s 'VBSEGS' 'FI_DOCUMENT'.
  mc_add_s 'WITH_ITEM' 'FI_DOCUMENT'.

  mc_add_s 'LEDSPD_FLOW' 'LE_LIKP'.
  mc_add_s '/SPE/CD_DOCFLOW' 'LE_LIKP'.
  mc_add_s '/SPE/CD_PROC_ITM' 'LE_LIKP'.
  mc_add_s '/SPE/INSPECRESC' 'LE_LIKP'.
  mc_add_s '/SPE/INSPECRESH' 'LE_LIKP'.
  mc_add_s '/SPE/INSPECRESP' 'LE_LIKP'.
  mc_add_s '/SPE/RET_CARRSEL' 'LE_LIKP'.
  mc_add_s '/SPE/RETPACKPROP' 'LE_LIKP'.
  mc_add_s 'LIKP' 'LE_LIKP'.
  mc_add_s 'LIPS' 'LE_LIKP'.
  mc_add_s 'LIPSDG' 'LE_LIKP'.
  mc_add_s 'LIPSPO' 'LE_LIKP'.
  mc_add_s 'LIPSRF' 'LE_LIKP'.
  mc_add_s 'SHP_IDX_GDSI' 'LE_LIKP'.
  mc_add_s 'TSEGE' 'LE_LIKP'.
  mc_add_s 'TSEGEADR' 'LE_LIKP'.
  mc_add_s 'TSEGH' 'LE_LIKP'.
  mc_add_s 'TSEGR' 'LE_LIKP'.
  mc_add_s 'TVPOD' 'LE_LIKP'.
  mc_add_s 'VBFS' 'LE_LIKP'.
  mc_add_s 'VKDFS' 'LE_LIKP'.
  mc_add_s 'VLBL' 'LE_LIKP'.
  mc_add_s 'VTDST' 'LE_LIKP'.
  mc_add_s 'VTRDI' 'LE_LIKP'.
  mc_add_s 'VTRKH' 'LE_LIKP'.
  mc_add_s 'VTRKP' 'LE_LIKP'.
  mc_add_s 'WAPPTV' 'LE_LIKP'.

  mc_add_s 'MLDOC' 'MLDOC'.
  mc_add_s 'MLDOC_EXTRACT' 'MLDOC'.
  mc_add_s 'MLDOCCCS' 'MLDOC'.
  mc_add_s 'MLDOCCCS_EXTRACT' 'MLDOC'.
  mc_add_s 'MLRUNLIST' 'MLDOC'.

  mc_add_s 'RESB' 'MM_EKKO'.
  mc_add_s 'EKAB' 'MM_EKKO'.
  mc_add_s 'EKBE' 'MM_EKKO'.
  mc_add_s 'EKBE_MA' 'MM_EKKO'.
  mc_add_s 'EKBE_SC' 'MM_EKKO'.
  mc_add_s 'EKBE_SRV_SUM' 'MM_EKKO'.
  mc_add_s 'EKBEH' 'MM_EKKO'.
  mc_add_s 'EKBEH_MA' 'MM_EKKO'.
  mc_add_s 'EKBZ' 'MM_EKKO'.
  mc_add_s 'EKBZ_MA' 'MM_EKKO'.
  mc_add_s 'EKBZH' 'MM_EKKO'.
  mc_add_s 'EKBZH_MA' 'MM_EKKO'.
  mc_add_s 'EKES' 'MM_EKKO'.
  mc_add_s 'EKET' 'MM_EKKO'.
  mc_add_s 'EKETH' 'MM_EKKO'.
  mc_add_s 'EKKI' 'MM_EKKO'.
  mc_add_s 'EKKN' 'MM_EKKO'.
  mc_add_s 'EKKO' 'MM_EKKO'.
  mc_add_s 'EKPA' 'MM_EKKO'.
  mc_add_s 'EKPO' 'MM_EKKO'.
  mc_add_s 'EKPV' 'MM_EKKO'.
  mc_add_s 'EKUB' 'MM_EKKO'.
  mc_add_s 'EREV' 'MM_EKKO'.
  mc_add_s 'ESKL' 'MM_EKKO'.
  mc_add_s 'ESKN' 'MM_EKKO'.
  mc_add_s 'ESLH' 'MM_EKKO'.
  mc_add_s 'ESLL' 'MM_EKKO'.
  mc_add_s 'ESSR' 'MM_EKKO'.
  mc_add_s 'ESUC' 'MM_EKKO'.
  mc_add_s 'ESUH' 'MM_EKKO'.
  mc_add_s 'MLBE' 'MM_EKKO'.
  mc_add_s 'MLBECR' 'MM_EKKO'.
  mc_add_s 'MLWERE' 'MM_EKKO'.
  mc_add_s 'POEXT' 'MM_EKKO'.

  mc_add_s 'MATDOC' 'MM_MATDOC'.
  mc_add_s 'MARC' 'MM_MATDOC'.
  mc_add_s 'MATDOC_EXTRACT' 'MM_MATDOC'.
  mc_add_s 'MCHBH' 'MM_MATDOC'.
  mc_add_s 'MKOL' 'MM_MATDOC'.
  mc_add_s 'MKOLH' 'MM_MATDOC'.
  mc_add_s 'MKPF' 'MM_MATDOC'.
  mc_add_s 'MSEG' 'MM_MATDOC'.
  mc_add_s 'MSKA' 'MM_MATDOC'.
  mc_add_s 'MSKAH' 'MM_MATDOC'.
  mc_add_s 'MSKU' 'MM_MATDOC'.
  mc_add_s 'MSKUH' 'MM_MATDOC'.
  mc_add_s 'MSLB' 'MM_MATDOC'.
  mc_add_s 'MSLBH' 'MM_MATDOC'.
  mc_add_s 'MSPR' 'MM_MATDOC'.
  mc_add_s 'MSPRH' 'MM_MATDOC'.
  mc_add_s 'MSSA' 'MM_MATDOC'.
  mc_add_s 'MSSAH' 'MM_MATDOC'.
  mc_add_s 'MSSL' 'MM_MATDOC'.
  mc_add_s 'MSSQ' 'MM_MATDOC'.
  mc_add_s 'MSSQH' 'MM_MATDOC'.
  mc_add_s 'MSTB' 'MM_MATDOC'.
  mc_add_s 'MSTBH' 'MM_MATDOC'.
  mc_add_s 'MSTE' 'MM_MATDOC'.
  mc_add_s 'MSTEH' 'MM_MATDOC'.
  mc_add_s 'MSTQ' 'MM_MATDOC'.
  mc_add_s 'MSTQH' 'MM_MATDOC'.

  mc_add_s 'VBAK' 'SD_VBAK'.
  mc_add_s 'DGMSD' 'SD_VBAK'.
  mc_add_s 'FPLA' 'SD_VBAK'.
  mc_add_s 'FPLT' 'SD_VBAK'.
  mc_add_s 'FPLTC' 'SD_VBAK'.
  mc_add_s 'KONV' 'SD_VBAK'.
  mc_add_s 'MVER' 'SD_VBAK'.
  mc_add_s 'NAST' 'SD_VBAK'.
  mc_add_s 'PRCD_ELEMENTS' 'SD_VBAK'.
  mc_add_s 'SALESDOC_CNT' 'SD_VBAK'.
  mc_add_s 'SER02' 'SD_VBAK'.
  mc_add_s 'VAKGU' 'SD_VBAK'.
  mc_add_s 'VAKPA' 'SD_VBAK'.
  mc_add_s 'VAPMA' 'SD_VBAK'.
  mc_add_s 'VBAP' 'SD_VBAK'.
  mc_add_s 'VBBE' 'SD_VBAK'.
  mc_add_s 'VBEH' 'SD_VBAK'.
  mc_add_s 'VBEP' 'SD_VBAK'.
  mc_add_s 'VBEX' 'SD_VBAK'.
  mc_add_s 'VBFA' 'SD_VBAK'.
  mc_add_s 'VBKD' 'SD_VBAK'.
  mc_add_s 'VBLB' 'SD_VBAK'.
  mc_add_s 'VBOX' 'SD_VBAK'.
  mc_add_s 'VBPA' 'SD_VBAK'.
  mc_add_s 'VBPA2' 'SD_VBAK'.
  mc_add_s 'VBPA3' 'SD_VBAK'.
  mc_add_s 'VBRK' 'SD_VBAK'.
  mc_add_s 'VBRL' 'SD_VBAK'.
  mc_add_s 'VBRP' 'SD_VBAK'.
  mc_add_s 'VBSK' 'SD_VBAK'.
  mc_add_s 'VBSN' 'SD_VBAK'.
  mc_add_s 'VBSS' 'SD_VBAK'.
  mc_add_s 'VBUK' 'SD_VBAK'.
  mc_add_s 'VBUP' 'SD_VBAK'.
  mc_add_s 'VBUV' 'SD_VBAK'.
  mc_add_s 'VEDA' 'SD_VBAK'.
  mc_add_s 'VEPVG' 'SD_VBAK'.
  mc_add_s 'VLKPA' 'SD_VBAK'.
  mc_add_s 'VLPKM' 'SD_VBAK'.
  mc_add_s 'VLPMA' 'SD_VBAK'.
  mc_add_s 'VRKPA' 'SD_VBAK'.
  mc_add_s 'VRPMA' 'SD_VBAK'.

  mc_add_s 'JCDS' 'STATUS'.
  mc_add_s 'DJEST' 'STATUS'.
  mc_add_s 'JCDO' 'STATUS'.
  mc_add_s 'JEST' 'STATUS'.
  mc_add_s 'JSTO' 'STATUS'.

  mc_add_s 'CEPC' 'OSTR'.
  mc_add_s 'CEPCT' 'OSTR'.
  mc_add_s 'CKMLHD' 'OSTR'.
  mc_add_s 'FAAT_YDDA' 'OSTR'.
  mc_add_s 'FAGL_TLDGRP_MAP' 'OSTR'.
  mc_add_s 'FINS_ML_ACCOUNT' 'OSTR'.
  mc_add_s 'FINSC_CMP_VERSND' 'OSTR'.
  mc_add_s 'FINSC_LD_CMP' 'OSTR'.
  mc_add_s 'FMLT_CURTP_ML' 'OSTR'.
  mc_add_s 'KNB5' 'OSTR'.
  mc_add_s 'KNBK' 'OSTR'.
  mc_add_s 'KNKK' 'OSTR'.
  mc_add_s 'KNVP' 'OSTR'.
  mc_add_s 'KNVV' 'OSTR'.
  mc_add_s 'MARCH' 'OSTR'.
  mc_add_s 'MARD' 'OSTR'.
  mc_add_s 'MARDH' 'OSTR'.
  mc_add_s 'ONRAO' 'OSTR'.
  mc_add_s 'PRPS' 'OSTR'.
  mc_add_s 'T000' 'OSTR'.
  mc_add_s 'T006' 'OSTR'.
  mc_add_s 'T093' 'OSTR'.
  mc_add_s 'T093A' 'OSTR'.
  mc_add_s 'T093C' 'OSTR'.
  mc_add_s 'T311A' 'OSTR'.
  mc_add_s 'TCURX' 'OSTR'.
  mc_add_s 'TJ01' 'OSTR'.
  mc_add_s 'TKA01' 'OSTR'.

  IF sy-dbsys(3) = 'HDB'.
    sizefield = 'REAL_SIZE'.
  ELSE.
    sizefield = 'EST_SIZE'.
  ENDIF.

  "Sum up scale-out group sizes.
  LOOP AT gt_tables ASSIGNING <item>.
    ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <sizefs>.
    READ TABLE gt_sogrp ASSIGNING <sogrp>
      WITH KEY group = <item>-sogroup.
    IF sy-subrc = 0.
      <sogrp>-size     = <sogrp>-size + <sizefs>.
      <sogrp>-opt_size = <sogrp>-opt_size + <item>-opt_size.
    ELSE.
      ls_sogrp-group = <item>-sogroup.
      ls_sogrp-size = <sizefs>.
      ls_sogrp-opt_size = <item>-opt_size.
      APPEND ls_sogrp TO gt_sogrp. CLEAR ls_sogrp.
    ENDIF.
  ENDLOOP.
  READ TABLE gt_sogrp ASSIGNING <sogrp> WITH KEY group = ''.
  IF sy-subrc = 0.
    <sogrp>-group = 'COL_TABLES_NOT_GROUPED'.
  ENDIF.

ENDFORM.                                     "calculate_scaleout_group.

*&---------------------------------------------------------------------*
*&      Form  get_dvm_info.
*&---------------------------------------------------------------------*
FORM get_dvm_info.
  TYPE-POOLS: adk.
  CONSTANTS: c_admi_run(8) VALUE 'admi_run'.

  "downport to 640 from cl_adk_ccms_get_objects
  TYPES:
    BEGIN OF ty_s_object,
      object(10)      TYPE c,
      object_text(50) TYPE c,
      sign(1)         TYPE c,
      tabnames        TYPE STANDARD TABLE OF tabname WITH DEFAULT KEY,
    END OF ty_s_object,
    ty_t_objects TYPE SORTED TABLE OF ty_s_object
                     WITH UNIQUE KEY object,
    BEGIN OF ty_s_objects_for_table,
      tabname TYPE tabname,
      objects TYPE ty_t_objects,
    END OF ty_s_objects_for_table,
    ty_t_objects_for_tables TYPE SORTED TABLE OF ty_s_objects_for_table
                                 WITH UNIQUE KEY tabname .

  DATA: ls_obj_arch_runs TYPE ty_obj_arch_runs,
        lt_obj_arch_runs TYPE TABLE OF ty_obj_arch_runs,
        ls_aging_runs    TYPE ty_aging_runs,
        ls_arch_runs     TYPE ty_arch_runs,
        lt_objects       TYPE ty_t_objects_for_tables,
        wa_objects       TYPE ty_s_objects_for_table,
        wa_objects_obj   TYPE ty_s_object,
        lt_tabnames      TYPE tttabname,
        archobj_wa       TYPE adk_ccms_object,
        archobj          TYPE adk_ccms_objects,
        nb_tables        TYPE i.
  FIELD-SYMBOLS:
    <objs> TYPE LINE OF ty_t_objects_for_tables,
    <obj>  TYPE ty_s_object.

  DATA: dbtab_join  TYPE string,
        aging_ratio TYPE p LENGTH 9 DECIMALS 3,
        ls_aging    TYPE ty_aging,
        ls_archdef  TYPE ty_obj_arch_def,
        ls_archruns TYPE ty_obj_arch_runs.

  FIELD-SYMBOLS: <dty> TYPE ty_aging_runs.
  CONSTANTS: c_daag_objects_act TYPE tabname VALUE 'DAAG_OBJECTS_ACT'.

  DEFINE mc_add_a.
    ls_aging_runs-table_name = &1. ls_aging_runs-agobject = &2.
    IF &1 = 'EDIDS'. ls_aging_runs-fspec = 'STATUS'. ENDIF.
    IF &1 = 'BALHDR'. ls_aging_runs-fspec = 'ALDATE'. ENDIF.
    IF &1 = 'CDHDR'. ls_aging_runs-fspec = 'UDATE'. ENDIF.
    IF &1 = 'SWWWIHEAD'. ls_aging_runs-fspec = 'WI_AED'. ENDIF.
    APPEND ls_aging_runs TO gt_aging_runs. CLEAR ls_aging_runs.
  END-OF-DEFINITION.

* We do not read DAAG_OBJECTS_DEF as it might not exist.
  mc_add_a 'ADR10' 'ADDRESS'.
  mc_add_a 'ADR11' 'ADDRESS'.
  mc_add_a 'ADR12' 'ADDRESS'.
  mc_add_a 'ADR13' 'ADDRESS'.
  mc_add_a 'ADR2' 'ADDRESS'.
  mc_add_a 'ADR3' 'ADDRESS'.
  mc_add_a 'ADR4' 'ADDRESS'.
  mc_add_a 'ADR5' 'ADDRESS'.
  mc_add_a 'ADR6' 'ADDRESS'.
  mc_add_a 'ADR7' 'ADDRESS'.
  mc_add_a 'ADR8' 'ADDRESS'.
  mc_add_a 'ADR9' 'ADDRESS'.
  mc_add_a 'ADRC' 'ADDRESS'.
  mc_add_a 'ADRCOMC' 'ADDRESS'.
  mc_add_a 'ADRCT' 'ADDRESS'.
  mc_add_a 'ADRT' 'ADDRESS'.
  mc_add_a 'ADRU' 'ADDRESS'.
  mc_add_a 'ADRV' 'ADDRESS'.
  mc_add_a 'CDHDR' 'BC_CHDO'.
  mc_add_a 'CDPOS' 'BC_CHDO'.
  mc_add_a 'CDPOS_STR' 'BC_CHDO'.
  mc_add_a 'CDPOS_UID' 'BC_CHDO'.
  mc_add_a 'EDID4' 'BC_IDOC'.
  mc_add_a 'EDIDS' 'BC_IDOC'.
  mc_add_a 'BAL_INDX' 'BC_SBAL'.
  mc_add_a 'BALDAT' 'BC_SBAL'.
  mc_add_a 'BALHDR' 'BC_SBAL'.
  mc_add_a 'SWP_JOIN' 'BC_WORKITEM'.
  mc_add_a 'SWP_NODEWI' 'BC_WORKITEM'.
  mc_add_a 'SWPNODE' 'BC_WORKITEM'.
  mc_add_a 'SWPNODELOG' 'BC_WORKITEM'.
  mc_add_a 'SWPSTEPLOG' 'BC_WORKITEM'.
  mc_add_a 'SWW_CONT' 'BC_WORKITEM'.
  mc_add_a 'SWW_CONTOB' 'BC_WORKITEM'.
  mc_add_a 'SWWCNTP0' 'BC_WORKITEM'.
  mc_add_a 'SWWLOGHIST' 'BC_WORKITEM'.
  mc_add_a 'SWWWIHEAD' 'BC_WORKITEM'.
  mc_add_a 'SNWD_TEXT_KEY' 'EPM_REUSE'.
  mc_add_a 'SNWD_TEXTS' 'EPM_REUSE'.
  mc_add_a 'SNWD_GI_HEAD' 'EPM_SO'.
  mc_add_a 'SNWD_GI_I' 'EPM_SO'.
  mc_add_a 'SNWD_SO' 'EPM_SO'.
  mc_add_a 'SNWD_SO_I' 'EPM_SO'.
  mc_add_a 'SNWD_SO_INV_HEAD' 'EPM_SO'.
  mc_add_a 'SNWD_SO_INV_ITEM' 'EPM_SO'.
  mc_add_a 'SNWD_SO_SL' 'EPM_SO'.
  mc_add_a 'FDM_DCOBJ' 'FDM_ENH'.
  mc_add_a 'ACDOCA' 'FI_DOCUMENT'.
  mc_add_a 'AGKO' 'FI_DOCUMENT'.
  mc_add_a 'BFIT_A' 'FI_DOCUMENT'.
  mc_add_a 'BFIT_A0' 'FI_DOCUMENT'.
  mc_add_a 'BFO_A_RA' 'FI_DOCUMENT'.
  mc_add_a 'BFOD_A' 'FI_DOCUMENT'.
  mc_add_a 'BFOD_AB' 'FI_DOCUMENT'.
  mc_add_a 'BFOK_A' 'FI_DOCUMENT'.
  mc_add_a 'BFOK_AB' 'FI_DOCUMENT'.
  mc_add_a 'BKPF' 'FI_DOCUMENT'.
  mc_add_a 'BSBW' 'FI_DOCUMENT'.
  mc_add_a 'BSE_CLR' 'FI_DOCUMENT'.
  mc_add_a 'BSE_CLR_ASGMT' 'FI_DOCUMENT'.
  mc_add_a 'BSEC' 'FI_DOCUMENT'.
  mc_add_a 'BSED' 'FI_DOCUMENT'.
  mc_add_a 'BSEG' 'FI_DOCUMENT'.
  mc_add_a 'BSEG_ADD' 'FI_DOCUMENT'.
  mc_add_a 'BSEGC' 'FI_DOCUMENT'.
  mc_add_a 'BSET' 'FI_DOCUMENT'.
  mc_add_a 'BSIP' 'FI_DOCUMENT'.
  mc_add_a 'BSPL' 'FI_DOCUMENT'.
  mc_add_a 'BVOR' 'FI_DOCUMENT'.
  mc_add_a 'ETXDCH' 'FI_DOCUMENT'.
  mc_add_a 'ETXDCI' 'FI_DOCUMENT'.
  mc_add_a 'ETXDCJ' 'FI_DOCUMENT'.
  mc_add_a 'FAGL_BSBW_HISTRY' 'FI_DOCUMENT'.
  mc_add_a 'FAGL_SPLINFO' 'FI_DOCUMENT'.
  mc_add_a 'FAGL_SPLINFO_VAL' 'FI_DOCUMENT'.
  mc_add_a 'FIGLDOC' 'FI_DOCUMENT'.
  mc_add_a 'GLE_MCA_FDOCREF' 'FI_DOCUMENT'.
  mc_add_a 'RF048' 'FI_DOCUMENT'.
  mc_add_a 'TTXI' 'FI_DOCUMENT'.
  mc_add_a 'TTXY' 'FI_DOCUMENT'.
  mc_add_a 'WITH_ITEM' 'FI_DOCUMENT'.
  mc_add_a 'SIPT_BKPF' 'FI_DOCUMENT_SIPT_BKPF'.
  mc_add_a 'EPICT_BRS' 'FI_EPIC_BRS'.
  mc_add_a 'EPICT_BRS_BSI' 'FI_EPIC_BRS'.
  mc_add_a 'EPICT_BRS_UJEI' 'FI_EPIC_BRS'.
  mc_add_a '/SPE/CD_DOCFLOW' 'LE_LIKP'.
  mc_add_a '/SPE/CD_PROC_ITM' 'LE_LIKP'.
  mc_add_a '/SPE/INSPECRESC' 'LE_LIKP'.
  mc_add_a '/SPE/INSPECRESH' 'LE_LIKP'.
  mc_add_a '/SPE/INSPECRESP' 'LE_LIKP'.
  mc_add_a '/SPE/RET_CARRSEL' 'LE_LIKP'.
  mc_add_a '/SPE/RETPACKPROP' 'LE_LIKP'.
  mc_add_a 'LEDSPD_FLOW' 'LE_LIKP'.
  mc_add_a 'LIKP' 'LE_LIKP'.
  mc_add_a 'LIPS' 'LE_LIKP'.
  mc_add_a 'LIPSDG' 'LE_LIKP'.
  mc_add_a 'LIPSPO' 'LE_LIKP'.
  mc_add_a 'LIPSRF' 'LE_LIKP'.
  mc_add_a 'TSEGE' 'LE_LIKP'.
  mc_add_a 'TSEGEADR' 'LE_LIKP'.
  mc_add_a 'TSEGH' 'LE_LIKP'.
  mc_add_a 'TSEGR' 'LE_LIKP'.
  mc_add_a 'TVPOD' 'LE_LIKP'.
  mc_add_a 'VLBL' 'LE_LIKP'.
  mc_add_a 'VTDST' 'LE_LIKP'.
  mc_add_a 'VTRDI' 'LE_LIKP'.
  mc_add_a 'VTRKH' 'LE_LIKP'.
  mc_add_a 'VTRKP' 'LE_LIKP'.
  mc_add_a 'WAPPTV' 'LE_LIKP'.
  mc_add_a 'EKAB' 'MM_EKKO'.
  mc_add_a 'EKBE' 'MM_EKKO'.
  mc_add_a 'EKBE_MA' 'MM_EKKO'.
  mc_add_a 'EKBE_SC' 'MM_EKKO'.
  mc_add_a 'EKBE_SRV_SUM' 'MM_EKKO'.
  mc_add_a 'EKBEH' 'MM_EKKO'.
  mc_add_a 'EKBEH_MA' 'MM_EKKO'.
  mc_add_a 'EKBZ' 'MM_EKKO'.
  mc_add_a 'EKBZ_MA' 'MM_EKKO'.
  mc_add_a 'EKBZH' 'MM_EKKO'.
  mc_add_a 'EKBZH_MA' 'MM_EKKO'.
  mc_add_a 'EKES' 'MM_EKKO'.
  mc_add_a 'EKET' 'MM_EKKO'.
  mc_add_a 'EKETH' 'MM_EKKO'.
  mc_add_a 'EKKI' 'MM_EKKO'.
  mc_add_a 'EKKN' 'MM_EKKO'.
  mc_add_a 'EKPA' 'MM_EKKO'.
  mc_add_a 'EKPO' 'MM_EKKO'.
  mc_add_a 'EKPV' 'MM_EKKO'.
  mc_add_a 'EKUB' 'MM_EKKO'.
  mc_add_a 'EREV' 'MM_EKKO'.
  mc_add_a 'ESKL' 'MM_EKKO'.
  mc_add_a 'ESKN' 'MM_EKKO'.
  mc_add_a 'ESLH' 'MM_EKKO'.
  mc_add_a 'ESLL' 'MM_EKKO'.
  mc_add_a 'ESSR' 'MM_EKKO'.
  mc_add_a 'ESUC' 'MM_EKKO'.
  mc_add_a 'ESUH' 'MM_EKKO'.
  mc_add_a 'MLBE' 'MM_EKKO'.
  mc_add_a 'MLBECR' 'MM_EKKO'.
  mc_add_a 'MLWERE' 'MM_EKKO'.
  mc_add_a 'POEXT' 'MM_EKKO'.
  mc_add_a 'RESB' 'MM_EKKO'.
  mc_add_a 'MATDOC' 'MM_MATDOC'.
  mc_add_a 'DGMSD' 'SD_VBAK'.
  mc_add_a 'FPLA' 'SD_VBAK'.
  mc_add_a 'FPLT' 'SD_VBAK'.
  mc_add_a 'FPLTC' 'SD_VBAK'.
  mc_add_a 'NAST' 'SD_VBAK'.
  mc_add_a 'PRCD_ELEMENTS' 'SD_VBAK'.
  mc_add_a 'VAKGU' 'SD_VBAK'.
  mc_add_a 'VBAK' 'SD_VBAK'.
  mc_add_a 'VBAP' 'SD_VBAK'.
  mc_add_a 'VBEH' 'SD_VBAK'.
  mc_add_a 'VBEP' 'SD_VBAK'.
  mc_add_a 'VBEX' 'SD_VBAK'.
  mc_add_a 'VBFA' 'SD_VBAK'.
  mc_add_a 'VBKD' 'SD_VBAK'.
  mc_add_a 'VBLB' 'SD_VBAK'.
  mc_add_a 'VBPA' 'SD_VBAK'.
  mc_add_a 'VBPA2' 'SD_VBAK'.
  mc_add_a 'VBPA3' 'SD_VBAK'.
  mc_add_a 'VBSN' 'SD_VBAK'.
  mc_add_a 'VBUV' 'SD_VBAK'.
  mc_add_a 'VEDA' 'SD_VBAK'.
  mc_add_a 'VLPKM' 'SD_VBAK'.
  mc_add_a 'VBRK' 'SD_VBRK'.
  mc_add_a 'VBRL' 'SD_VBRK'.
  mc_add_a 'VBRP' 'SD_VBRK'.
  mc_add_a 'FAGL_R_SPL' 'SPL_ENH'.
  mc_add_a 'FAGL_R_SPL_VAL' 'SPL_ENH'.
  mc_add_a 'DJEST' 'STATUS'.
  mc_add_a 'JCDO' 'STATUS'.
  mc_add_a 'JCDS' 'STATUS'.
  mc_add_a 'JEST' 'STATUS'.
  mc_add_a 'JSTO' 'STATUS'.
  mc_add_a 'WITH_ITEM_EXCL' 'WITH_ENH'.

  "New V74
  mc_add_a 'IBEXTINST' 'IBASE'.
  mc_add_a 'IBIB' 'IBASE'.
  mc_add_a 'IBIBT' 'IBASE'.
  mc_add_a 'IBIN' 'IBASE'.
  mc_add_a 'IBINOBS' 'IBASE'.
  mc_add_a 'IBINOWN' 'IBASE'.
  mc_add_a 'IBINTX' 'IBASE'.
  mc_add_a 'IBINVALUES' 'IBASE'.
  mc_add_a 'IBSP' 'IBASE'.
  mc_add_a 'IBST' 'IBASE'.
  mc_add_a 'IBSTREF' 'IBASE'.
  mc_add_a 'IBSYMBOL' 'IBASE'.

  mc_add_a 'LCM_DOC_PERMLOCK' 'LCM_DOC'.
  mc_add_a 'LCM_DOC_STAMP' 'LCM_DOC'.
  mc_add_a 'LCM_DOC_STS_CHGS' 'LCM_DOC'.
  mc_add_a 'LCM_DOC_TMPLINFO' 'LCM_DOC'.
  mc_add_a 'LCM_DOCUMENT' 'LCM_DOC'.
  mc_add_a 'LCM_LONGTEXT_VER' 'LCM_DOC'.
  mc_add_a 'LCM_NOTES' 'LCM_DOC'.

  mc_add_a 'LCM_DOCUMENT' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LEGALTR' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LONGTEXT_VER' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_CAT_GR' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_CATEG' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_DATE' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_DOCLINK' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_ENTITY' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_EXTCONT' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_INTCONT' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_LINKOBJ' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_PR_CAT' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_RELSHP' 'LCM_LEGALTR'.
  mc_add_a 'LCM_LT_REMINDER' 'LCM_LEGALTR'.
  mc_add_a 'LCM_NOTES' 'LCM_LEGALTR'.

  mc_add_a 'MLDOC' 'ML_MLDOC'.
  mc_add_a 'MLDOCCCS' 'ML_MLDOC'.

  mc_add_a 'EKBE_SRV_SUM' 'MM_SRV'.
  mc_add_a 'ESKL' 'MM_SRV'.
  mc_add_a 'ESKN' 'MM_SRV'.
  mc_add_a 'ESLH' 'MM_SRV'.
  mc_add_a 'ESLL' 'MM_SRV'.
  mc_add_a 'ESSR' 'MM_SRV'.
  mc_add_a 'ESUC' 'MM_SRV'.
  mc_add_a 'ESUH' 'MM_SRV'.

  mc_add_a 'PPH_DD_STPR_COMP' 'PP_DD_PROD_BUFLVL_PRPSL'.
  mc_add_a 'PPH_DD_STPR_DETS' 'PP_DD_PROD_BUFLVL_PRPSL'.
  mc_add_a 'PPH_DD_STPR_HDR' 'PP_DD_PROD_BUFLVL_PRPSL'.
  mc_add_a 'PPH_DD_STPR_ZADJ' 'PP_DD_PROD_BUFLVL_PRPSL'.

  mc_add_a 'DBERCHR' 'ISU_BILLZ'.
  mc_add_a 'DBERCHT' 'ISU_BILLZ'.
  mc_add_a 'DBERCHU' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ1' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ2' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ3' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ4' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ5' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ6' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ7' 'ISU_BILLZ'.
  mc_add_a 'DBERCHZ8' 'ISU_BILLZ'.
  mc_add_a 'DBERDTAX' 'ISU_BILLZ'.
  mc_add_a 'DBERDTAX_ADD' 'ISU_BILLZ'.

  mc_add_a 'DBERDL' 'ISU_PRDOCL'.
  mc_add_a 'DBERDLB' 'ISU_PRDOCL'.
  mc_add_a 'DBERDR' 'ISU_PRDOCL'.
  mc_add_a 'DBERDU' 'ISU_PRDOCL'.
  mc_add_a 'ERDTS' 'ISU_PRDOCL'.

  SORT gt_aging_runs BY table_name.

  "Search for last aging runs
  LOOP AT gt_aging_runs ASSIGNING <aging_runs>
      WHERE agobject IS NOT INITIAL.
    TRY.
        SELECT SINGLE active FROM (c_daag_objects_act)
          INTO <aging_runs>-agobact
          WHERE object = <aging_runs>-agobject.
      CATCH cx_sy_dynamic_osql_semantics.
        "table do not exist=> no data aging.
    ENDTRY.
    IF <aging_runs>-agobact = abap_true.
      CONCATENATE 'daag_run as r JOIN daag_run_obj as o'
                  'ON r~run_number = o~run_number'
                  INTO dbtab_join SEPARATED BY space.       "#EC NOTEXT
      SELECT MAX( r~run_date ) INTO <aging_runs>-aglrun
        FROM (dbtab_join)
        WHERE o~object = <aging_runs>-agobject
              AND o~active = abap_true AND r~run_status = 'S'
              AND r~status_details = 'A'.
    ENDIF.
  ENDLOOP.

*  IF p_dag = abap_true.
  "Calculate the age-able size of the tables
  LOOP AT gt_aging_runs ASSIGNING <aging_runs>
      WHERE agobject IS NOT INITIAL.
    READ TABLE gt_tables ASSIGNING <item>
      WITH KEY table_name = <aging_runs>-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT <item>-cs_cols ASSIGNING <cs_size>
        WHERE column_name <> '$rowid$'
          AND column_name <> '$trex_udiv$'
          AND column_name(5) <> '$esh:'.
        IF <cs_size>-column_name(1) = '$' "concat attr.
            AND <cs_size>-has_index = abap_false
            AND <cs_size>-column_name <> '$trexexternalkey$'.
          CONTINUE.
        ELSE.
          "anydb
          <item>-est_age_size  = <item>-est_age_size
                                + <cs_size>-est_ms_main.
          est_elaging = est_elaging + <cs_size>-est_ms_main.
          "HANA
          IF <cs_size>-column_name(1)    = '$'           OR
             <cs_size>-cs_data_type_name = 'STRING'      OR
             <cs_size>-cs_data_type_name = 'FIXEDSTRING' OR
             <cs_size>-cs_data_type_name = 'RAW'.
            "only add dict. if not fixed data type.
            <item>-real_age_size = <item>-real_age_size
                                   + <cs_size>-ms_dict.
            real_elaging = real_elaging + <cs_size>-ms_dict.
          ENDIF.
          "misc is pageable. "index pageable since SPS11
          <item>-real_age_size = <item>-real_age_size +
             <cs_size>-ms_data + <cs_size>-ms_index.
          real_elaging = real_elaging +
             <cs_size>-ms_data + <cs_size>-ms_index.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  "Multiply age-able size by ratio of age-able documents
  LOOP AT gt_aging_runs ASSIGNING <aging_runs>
      WHERE fspec IS NOT INITIAL.
    READ TABLE gt_tables ASSIGNING <item>
      WITH KEY table_name = <aging_runs>-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE <item>-cs_cols ASSIGNING <cs_size>
        WITH KEY column_name = <aging_runs>-fspec.
      IF sy-subrc = 0.
        aging_ratio = <cs_size>-spec_count / <item>-record_count.
        LOOP AT gt_aging_runs ASSIGNING <dty>
            WHERE agobject = <aging_runs>-agobject.
          READ TABLE gt_tables ASSIGNING <item>
            WITH KEY table_name = <dty>-table_name BINARY SEARCH.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <item> TO ls_aging.
            IF sy-dbsys(3) = 'HDB'.
              ls_aging-size = <item>-real_age_size.
            ELSE.
              ls_aging-size = <item>-est_age_size.
            ENDIF.
            ls_aging-hist_size = ls_aging-size * aging_ratio.
            APPEND ls_aging TO gt_aging.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_aging ASSIGNING <aging>.
    est_aged = est_aged + <aging>-hist_size.
  ENDLOOP.
  SORT gt_aging BY hist_size DESCENDING.

*  ENDIF.

  "Check archiving
  IF sy-dbsys(3) = 'HDB'.
    SORT gt_tables BY real_size DESCENDING.
  ELSE.
    SORT gt_tables BY est_size DESCENDING.
  ENDIF.
  CASE jobname.
    WHEN c_hrcjobname.
      nb_tables = 2000.
    WHEN OTHERS.
      nb_tables = 100.
  ENDCASE.
  LOOP AT gt_tables ASSIGNING <item> TO nb_tables.
    APPEND <item>-table_name TO lt_tabnames.
  ENDLOOP.
  TRY.
      CALL METHOD ('CL_ADK_CCMS_GET_OBJECTS')=>('GET_OBJECTS')
        EXPORTING
          it_tabnames            = lt_tabnames
          iv_delete_objects_only = abap_true
          iv_get_all_tables      = abap_false
        IMPORTING
          et_objects_for_tables  = lt_objects.
    CATCH cx_sy_dyn_call_illegal_class
          cx_sy_dyn_call_illegal_method
          cx_root.
      "read the minimum number of tables
      LOOP AT gt_tables ASSIGNING <item> TO nb_tables.
        CALL FUNCTION 'ADK_CCMS_GET_OBJECTS'
          EXPORTING
            table                      = <item>-db_table
            delete_objects_only        = 'X'
          TABLES
            objects                    = archobj
          EXCEPTIONS
            table_not_found            = 1
            pool_or_cluster_not_in_use = 2
            no_object_found            = 3
            table_is_not_transparent   = 4
            OTHERS                     = 5.
        IF sy-subrc <> 0.
          "ok no archiving information
        ENDIF.
        wa_objects-tabname = <item>-table_name.
        LOOP AT archobj INTO archobj_wa.
          wa_objects_obj-object = archobj_wa-object.
          MOVE archobj_wa-objtext TO wa_objects_obj-object_text.
          wa_objects_obj-sign = archobj_wa-sign.
          APPEND wa_objects_obj TO wa_objects-objects.
        ENDLOOP.
        IF wa_objects-objects[] IS NOT INITIAL.
          READ TABLE lt_objects WITH KEY tabname = wa_objects-tabname
            TRANSPORTING NO FIELDS BINARY SEARCH.
          INSERT wa_objects INTO lt_objects INDEX sy-tabix.
        ENDIF.
        CLEAR: wa_objects, wa_objects_obj.
      ENDLOOP.
  ENDTRY.
  CLEAR lt_tabnames[].

  "get 10 last archiving runs dates of each objects
  LOOP AT lt_objects ASSIGNING <objs>.
    ls_arch_runs-table_name = <objs>-tabname.
    LOOP AT <objs>-objects ASSIGNING <obj>.
      ls_archdef-object = <obj>-object.
      APPEND ls_archdef TO ls_arch_runs-objects.
      TRY.
          SELECT object creat_date AS arclrun FROM (c_admi_run)
            UP TO 10 ROWS
            INTO TABLE lt_obj_arch_runs
            WHERE object = <obj>-object AND
                  status = 1
            ORDER BY creat_date DESCENDING.
          LOOP AT lt_obj_arch_runs INTO ls_archruns.
            APPEND ls_archruns TO ls_arch_runs-runs.
          ENDLOOP.
        CATCH cx_sy_dynamic_osql_semantics.
      ENDTRY.
    ENDLOOP.
    APPEND ls_arch_runs TO gt_arch_runs. CLEAR ls_arch_runs.
  ENDLOOP.
  LOOP AT gt_arch_runs INTO ls_arch_runs.
    SORT ls_arch_runs-runs BY arclrun DESCENDING.
    SORT ls_arch_runs-objects BY object.

    "below code is only relevant for ful mode
    READ TABLE gt_tables ASSIGNING <item>
       WITH KEY table_name = ls_arch_runs-table_name BINARY SEARCH.
    IF sy-subrc = 0.
      IF sy-dbsys(3) = 'HDB'.
        real_elarch = real_elarch + <item>-real_size.
      ELSE.
        est_elarch = est_elarch + <item>-est_size.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "get_dvm_info
*&---------------------------------------------------------------------*
*&      Form get_topcs
*&---------------------------------------------------------------------*
*       estimate the size of the largest 2 partitions
*----------------------------------------------------------------------*

FORM get_topcs  USING    sizefield TYPE fieldname
                         l_topcs.

  FIELD-SYMBOLS: <fs> TYPE any.

  SORT gt_tables BY (sizefield) DESCENDING.
  CLEAR count.
  IF sy-dbsys(3) = 'HDB'.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      IF count > 3.
        EXIT.
      ENDIF.
      ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <fs>.
      IF <item>-part_id = 0. "cannot be 0 except cal mode.
        l_topcs = l_topcs + <fs>.
      ELSE.
        l_topcs = l_topcs + <fs> / <item>-part_id.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT gt_tables ASSIGNING <item> WHERE store = 'CS'.
      count = count + 1.
      IF count > 3.
        EXIT.
      ENDIF.
      ASSIGN COMPONENT sizefield OF STRUCTURE <item> TO <fs>.
      IF <item>-record_count >= max_partition_rc.
        IF l_topcs IS INITIAL.
          l_topcs = 3 * ( <fs> * max_partition_rc ) /
                                    <item>-record_count.
          EXIT. "if biggest table is partitioned then consider 3
          "partitions of this table as 3 largest objects.
        ELSE. "first table not partitioned but second is.
          l_topcs = l_topcs + ( <fs> * max_partition_rc ) /
                                    <item>-record_count.
        ENDIF.
      ELSE.
        l_topcs = l_topcs + <fs>.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "get_topcs

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_guid.

  PERFORM call_f4_help USING 'P_GUID'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_guid_c.

  PERFORM call_f4_help USING 'P_GUID_C'.

*&---------------------------------------------------------------------*
*&      Form call_F4_help
*&---------------------------------------------------------------------*
*       Call F4 help for Sizing GUID
*
*----------------------------------------------------------------------*
FORM call_f4_help USING f4_param_name.

  TYPES:
    BEGIN OF ty_sizing, "replace this by dynamic using l_dfies
      sizing_guid       TYPE guid_16,
      crdate            TYPE sydats,
      crtime            TYPE sytime,
      report_version(6) TYPE c,
      nb_tables         TYPE int4,
      nb_errors         TYPE int4,
      total_sizing1     TYPE dec20,
    END OF ty_sizing.

  DATA: l_sizing_guids TYPE TABLE OF ty_sizing,
        l_dfies        TYPE TABLE OF dfies,
        l_alldfies     TYPE TABLE OF dfies,
        wa_dfies       TYPE dfies,
        offset         LIKE dfies-offset.
  FIELD-SYMBOLS: <sizing> TYPE ty_sizing.
  CONSTANTS: c_sdf_hdbsizings TYPE tabname VALUE '/SDF/HDBSIZING'.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = c_sdf_hdbsizings
    TABLES
      dfies_tab      = l_alldfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  LOOP AT l_alldfies INTO wa_dfies.
    CASE wa_dfies-fieldname.
      WHEN 'SIZING_GUID'.
        wa_dfies-reptext   = 'Sizing GUID'.                 "#EC NOTEXT
      WHEN 'CRDATE'.
        wa_dfies-scrtext_s = 'Exec. date'.                  "#EC NOTEXT
      WHEN 'CRTIME'.
        wa_dfies-scrtext_s = 'Exec. time'.                  "#EC NOTEXT
      WHEN 'TOTAL_SIZING1'.
        wa_dfies-reptext = 'Memory sizing in MB'.           "#EC NOTEXT
      WHEN OTHERS.
    ENDCASE.
    IF   wa_dfies-fieldname = 'SIZING_GUID'
      OR wa_dfies-fieldname = 'CRDATE'
      OR wa_dfies-fieldname = 'CRTIME'
      OR wa_dfies-fieldname = 'REPORT_VERSION'
      OR wa_dfies-fieldname = 'NB_TABLES'
      OR wa_dfies-fieldname = 'NB_ERRORS'
      OR wa_dfies-fieldname = 'TOTAL_SIZING1'.
      wa_dfies-offset    = offset.
      offset = offset + wa_dfies-intlen.
      APPEND wa_dfies TO l_dfies.
    ENDIF.
  ENDLOOP.

  SELECT sizing_guid crdate crtime report_version nb_tables
         nb_errors total_sizing1
    FROM (c_sdf_hdbsizings)
    INTO CORRESPONDING FIELDS OF TABLE l_sizing_guids.

  LOOP AT l_sizing_guids ASSIGNING <sizing>.
    <sizing>-total_sizing1 = <sizing>-total_sizing1 / 1024 / 1024.
  ENDLOOP.
  SORT l_sizing_guids BY crdate DESCENDING crtime DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SIZING_GUID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = f4_param_name
      window_title    = 'Sizing results available'      "#EC NOTEXT
      value_org       = 'S'
    TABLES
      value_tab       = l_sizing_guids
      field_tab       = l_dfies
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form b2gb_subtotals
*&---------------------------------------------------------------------*
*        Convert all variables to GB
*
*----------------------------------------------------------------------*
FORM b2gb_subtotals.

  LOOP AT gt_tables ASSIGNING <item>.
    b2gb <item>-source_size. b2gb <item>-real_size.
    b2gb <item>-est_size. b2gb <item>-est_hl_size.
    b2gb <item>-real_hl_size. b2gb <item>-real_jec_size.
    b2gb <item>-real_esh_size.
    b2gb <item>-est_pk_size. b2gb <item>-real_pk_size.
    b2gb <item>-est_nuk_size. b2gb <item>-real_nuk_size.
    b2gb <item>-est_age_size. b2gb <item>-real_age_size.
    b2gb <item>-opt_size.
    b2gb <item>-real_rs_exsize. b2gb <item>-real_hlrs_exsize.
  ENDLOOP.
  LOOP AT gt_sfin2 ASSIGNING <sfin>.
    b2gb <sfin>-size. b2gb <sfin>-ssize. b2gb <sfin>-osize.
  ENDLOOP.
  LOOP AT gt_aging ASSIGNING <aging>.
    b2gb <aging>-hist_size. b2gb <aging>-size.
  ENDLOOP.
  LOOP AT gt_slog ASSIGNING <slog>.
    b2gb <slog>-size. b2gb <slog>-ssize. b2gb <slog>-osize.
  ENDLOOP.
  LOOP AT gt_inplace ASSIGNING <upgrade>.
    b2gb <upgrade>-size. b2gb <upgrade>-pksize. b2gb <upgrade>-disksize.
  ENDLOOP.
  LOOP AT gt_shadow ASSIGNING <upgrade>.
    b2gb <upgrade>-size. b2gb <upgrade>-pksize. b2gb <upgrade>-disksize.
  ENDLOOP.
  LOOP AT gt_sogrp ASSIGNING <sogrp>.
    b2gb <sogrp>-size.
    b2gb <sogrp>-opt_size.
  ENDLOOP.
  LOOP AT gt_ml ASSIGNING <ml>.
    b2gb <ml>-size. b2gb <ml>-ssize. b2gb <ml>-osize.
  ENDLOOP.

  b2gb diskadmin. b2gb codestack.
  "details - real
  b2gb cs_col. b2gb cs_pk. b2gb cs_rid. b2gb cs_udiv.
  b2gb cs_uk. b2gb cs_nuk. b2gb cs_jec. b2gb cs_aging. b2gb cs_ft.
  b2gb cs_hcard. b2gb cs_mcard.
  b2gb cs_lcard. b2gb cs_ucard.
  b2gb rs_col. b2gb rs_pk. b2gb rs_sk.
  b2gb cshl. b2gb rshl.
  b2gb cs_mlob. b2gb rs_mlob.
  b2gb cs_k. b2gb cs_total. b2gb rs_total. b2gb init_data.
  b2gb init_ws. b2gb hl. b2gb hlch.
  b2gb twocs. b2gb initdisk. b2gb init_size.
  b2gb opt_data. b2gb opt_size.
  b2gb optdisk. b2gb opt_ws.
  b2gb init_pers. b2gb init_dram.
  "details - est.
  b2gb est_cs_col. b2gb est_cs_pk. b2gb est_cs_rid. b2gb est_cs_udiv.
  b2gb est_cs_uk. b2gb est_cs_nuk.
  b2gb est_cs_hcard. b2gb est_cs_mcard.
  b2gb est_cs_lcard. b2gb est_cs_ucard.
  b2gb est_rs_col. b2gb est_rs_pk. b2gb est_rs_sk.
  b2gb est_cshl. b2gb est_rshl.
  b2gb est_rs_mlob. b2gb est_rs_mlob.
  b2gb est_cs_k. b2gb est_cs_total. b2gb est_rs_total.
  b2gb est_init_data.
  b2gb est_init_ws. b2gb est_hl. b2gb est_hlch.
  b2gb est_sfin_inv.
  b2gb est_twocs. b2gb est_initdisk. b2gb est_init_size.
  b2gb est_init_pers. b2gb est_init_dram.

  b2gb est_opt_data. b2gb est_opt_size.
  b2gb opt_twocs. b2gb est_optdisk. b2gb est_opt_ws.

  "Finance
  b2gb est_sfintab. b2gb est_sfincoldch. b2gb est_sfincold.
  b2gb est_fin_unl.
  b2gb est_sfincol. b2gb est_pca. b2gb est_delcoep. b2gb est_sl.
  b2gb est_obs_fin. b2gb est_sfinnew.
  "ml
  b2gb est_mlnew.  b2gb est_obs_ml.
  "aging
  b2gb est_aged. b2gb est_agedch.
  b2gb real_elarch. b2gb est_elarch.
  b2gb real_elaging. b2gb est_elaging.
  b2gb est_ageddisk.
  "Logistics
  b2gb est_slogtab. b2gb est_log_unl.
  b2gb est_slogcol. b2gb est_log_les.
  b2gb est_log_newvbfa. b2gb est_log_oldvbfa. b2gb est_log_chgvbfa.
  b2gb est_obs_log. b2gb est_slognew.
  b2gb est_s4_trans.
  b2gb est_rs2cs.
  "inplace
  b2gb est_updata. b2gb est_upchrec.
  b2gb est_updisk. b2gb est_uptotal.
  "standard conversion
  b2gb est_sha. b2gb est_shadisk. b2gb est_shatotal.
  "anydb
  b2gb wa_techd-anydbsize_n.
  "Additional components
  b2gb est_indx. b2gb est_lc.
  "Business data
  b2gb est_cl_lis.

ENDFORM.                    "b2gb_subtotals

*&---------------------------------------------------------------------*
*&      Form fill_tech_details
*&---------------------------------------------------------------------*
*        Collect technical details
*
*----------------------------------------------------------------------*

FORM fill_tech_details.

  DATA: s_cvers  TYPE cvers,
        l_dbinfo TYPE dbrelinfo,
        ls_so    LIKE LINE OF so_tab.

  "parameters
  wa_techd-daag        = p_daag.
  wa_techd-paral       = p_paral.
  wa_techd-hdbload     = p_load.
  wa_techd-hl          = p_hl.
  wa_techd-mac         = p_mac.
  wa_techd-calib       = p_calib.
  wa_techd-do_soh      = do_soh.
  wa_techd-do_sfin     = do_sfin.
  wa_techd-do_slog     = do_slog.
  wa_techd-in_sfin     = in_sfin.
  wa_techd-in_slog     = in_slog.
  wa_techd-hdb_empty_tables = p_empt.
  wa_techd-clustprec   = p_clus.

  IF p_bas2 = abap_true.
    wa_techd-basis_753   = abap_true.
  ELSE.
    wa_techd-basis_753   = abap_false.
  ENDIF.

  IF p_hdb1 IS NOT INITIAL.
    wa_techd-hdbv        = '1.0'.
  ELSE.
    wa_techd-hdbv        = '2.0'.
  ENDIF.

  LOOP AT so_tab INTO ls_so.
    MOVE-CORRESPONDING ls_so TO wa_nso.
    APPEND wa_nso TO gt_nso. CLEAR wa_nso.
  ENDLOOP.

  wa_techd-report_version = l_version.
  wa_techd-crdate      = sy-datum.
  wa_techd-report_name = sy-repid.
  wa_techd-sid         = sy-sysid.
  wa_techd-db_vendor   = sy-dbsys.

  IF cl_abap_char_utilities=>charsize = 1.
    wa_techd-unicode = 'No'.
  ELSE.
    wa_techd-unicode = 'Yes'.
  ENDIF.

  CALL FUNCTION 'DB_DBRELINFO'
    IMPORTING
      dbinfo = l_dbinfo.
  wa_techd-db_release = l_dbinfo-srvrel(15).

  SELECT SINGLE * FROM cvers INTO s_cvers WHERE component = 'SAP_BASIS'.
  SHIFT s_cvers-extrelease LEFT DELETING LEADING '0'.
  CONCATENATE s_cvers-release(5) 'SP' s_cvers-extrelease
         INTO wa_techd-sapversion SEPARATED BY space.

  CALL FUNCTION 'TH_SAPREL'
    IMPORTING
      kern_rel     = wa_techd-kernversion
      kern_comp_on = wa_techd-osversion.

  IF s_cvers-release(3) = '700'
      AND s_cvers-extrelease < 28 AND sy-dbsys(3) = 'MSS'.

    CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS_SIMPLE'
      EXPORTING
        iv_numm                    = '1691513'
      IMPORTING
        ev_status                  = lv_status
      EXCEPTIONS
        note_not_found             = 1
        inconsistent_delivery_data = 2
        undefined_component_state  = 3
        incomplete_note_data       = 4
        error                      = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      CASE lv_status.
        WHEN 'V' OR 'U' OR 'N' OR ' ' OR 'O'.
          wa_techd-mss1691513 = abap_true.
        WHEN OTHERS.
          "the note is implemented. Status E. OR not valid Status '-'
      ENDCASE.
    ENDIF.
  ENDIF.

  GET TIME STAMP FIELD endstamp.
  wa_techd-duration = cl_abap_tstmp=>subtract( tstmp1 = endstamp
                                               tstmp2 = startstamp ).

ENDFORM.                  "fill_tech_details

*&---------------------------------------------------------------------*
*&      Form read_selected_sizing
*&---------------------------------------------------------------------*
*        Select and display a sizing
*
*----------------------------------------------------------------------*

FORM read_selected_sizing USING guid.

  DATA: s_sizing         TYPE xstring,
        s_subtotals      TYPE xstring,
        s_aging_runs     TYPE xstring,
        s_archiving_runs TYPE xstring,
        s_techd          TYPE xstring,
        s_tables         TYPE xstring,
        s_aging          TYPE xstring,
        s_sogrp          TYPE xstring,
        s_cpu_per        TYPE xstring,
        s_cpu_x          TYPE xstring.

  FIELD-SYMBOLS:
    <subtotals>  TYPE ty_subtotals,
    <hdbtabsize> TYPE ty_hdbtabsizes.

  DEFINE mc_read_sub.
    IF gt_subtotals IS NOT INITIAL. "Old report versions
      READ TABLE gt_subtotals WITH KEY name = &1 ASSIGNING <subtotals>.
      IF sy-subrc = 0. "new variables not in old versions
        IF wa_sizing-db_vendor = 'HDB'.
          &3 = <subtotals>-value.
        ELSE.
          &2 = <subtotals>-value.
        ENDIF.
      ENDIF.
    ENDIF.
  END-OF-DEFINITION.

  IF guid IS INITIAL.
    MESSAGE e090(sada) WITH 'You must specify'              "#EC NOTEXT
                       'a valid sizing result guid.' ''.    "#EC NOTEXT
  ENDIF.

  TRY.
      CALL FUNCTION '/SDF/READ_HDB_SIZING_RESULTS_X'
        EXPORTING
          i_sizing_guid        = guid
        IMPORTING
          e_sizing             = s_sizing
          e_subtotals          = s_subtotals
          e_aging_runs         = s_aging_runs
          e_archiving_runs     = s_archiving_runs
          e_techd              = s_techd
          e_tables             = s_tables
          e_aging              = s_aging
          e_sogrp              = s_sogrp
          e_cpu_per            = s_cpu_per
          e_cpu_x              = s_cpu_x
        EXCEPTIONS
          no_results           = 1
          no_results_in_client = 2
          OTHERS               = 3.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e090(sada) WITH 'The selected sizing GUID' "#EC NOTEXT
                             'cannot be used.' ''.          "#EC NOTEXT
        WHEN 2.
          MESSAGE e090(sada) WITH 'The selected sizing GUID' "#EC NOTEXT
               'was created in a different client and '     "#EC NOTEXT
               'cannot be displayed in this client.'.       "#EC NOTEXT
        WHEN 3.
          MESSAGE e090(sada) WITH 'An error occurred when'  "#EC NOTEXT
                           'reading the selected GUID.' ''. "#EC NOTEXT
      ENDCASE.

    CATCH cx_sy_dyn_call_illegal_func.
      MESSAGE e090(sada) WITH 'The selected sizing GUID'    "#EC NOTEXT
                         'cannot be used.' ''.              "#EC NOTEXT
  ENDTRY.

  CALL TRANSFORMATION id SOURCE XML s_sizing
       RESULT source = wa_sizing.
  CALL TRANSFORMATION id SOURCE XML s_subtotals
        RESULT source = gt_subtotals.
  CALL TRANSFORMATION id SOURCE XML s_tables
        RESULT source = gt_hdbtabsizes.

  "Minimim V74 required
  CALL TRANSFORMATION id SOURCE XML s_aging_runs
      RESULT source = gt_aging_runs.
  CALL TRANSFORMATION id SOURCE XML s_archiving_runs
      RESULT source = gt_arch_runs.
  CALL TRANSFORMATION id SOURCE XML s_techd
      RESULT source = wa_techd.
  CALL TRANSFORMATION id SOURCE XML s_aging
      RESULT source = gt_aging.
  CALL TRANSFORMATION id SOURCE XML s_sogrp
      RESULT source = gt_sogrp.
  "Minimum V76 required
  IF p_disp = abap_true.
    CALL TRANSFORMATION id SOURCE XML s_cpu_per
        RESULT source = gt_cpu_per.
    CALL TRANSFORMATION id SOURCE XML s_cpu_x
            RESULT source = wa_cpu_x.
  ENDIF.

  "Compatibility adaptations
  IF wa_techd IS INITIAL. "old sizing report version
    wa_techd-success_cnt    = wa_sizing-nb_tables.
    wa_techd-error_cnt      = wa_sizing-nb_errors.
    MOVE-CORRESPONDING wa_sizing TO wa_techd.
  ENDIF.
  wa_techd-anydbsize_n = wa_sizing-anydb_size.

  IF wa_sizing-error_list IS NOT INITIAL.
    IMPORT p1 = gt_error FROM DATA BUFFER wa_sizing-error_list.
  ENDIF.
  IF wa_sizing-sizing_variant IS NOT INITIAL.
    IMPORT p1 = gt_nso   FROM DATA BUFFER wa_sizing-sizing_variant.
  ENDIF.

  "Subtotals - aligned with WRITE_TO_DB
  mc_read_sub 'l_cs_total' est_cs_total cs_total.
  mc_read_sub 'l_rs_total' est_rs_total rs_total.
  mc_read_sub 'l_hl' est_hl hl.
  mc_read_sub 'l_hlch' est_hlch hlch.
  mc_read_sub 'l_disk' est_initdisk initdisk.
  mc_read_sub 'l_init_data' est_init_data init_data.
  mc_read_sub 'l_init_ws' est_init_ws init_ws.
  mc_read_sub 'l_init_size' est_init_size init_size.
  mc_read_sub 'l_twocs' est_twocs twocs.
  mc_read_sub 'l_indx' est_indx est_indx.
  mc_read_sub 'l_lc' est_lc est_lc.
  mc_read_sub 'l_s4_tr' est_s4_trans est_s4_trans.
  mc_read_sub 'l_sfincold' est_sfincold est_sfincold.
  mc_read_sub 'l_coldsfinch' est_sfincoldch est_sfincoldch.
  mc_read_sub 'l_sfintab' est_sfintab est_sfintab.
  mc_read_sub 'l_sfincol' est_sfincol est_sfincol.
  mc_read_sub 'l_sfinnew' est_sfinnew est_sfinnew.
  mc_read_sub 'l_fin_unl' est_fin_unl est_fin_unl.
  mc_read_sub 'l_obs_fin' est_obs_fin est_obs_fin.
  mc_read_sub 'l_pca' est_pca est_pca.
  mc_read_sub 'l_delcoep' est_delcoep est_delcoep.
  mc_read_sub 'l_sl' est_sl est_sl.
  mc_read_sub 'l_slogtab' est_slogtab est_slogtab.
  mc_read_sub 'l_slogcol' est_slogcol est_slogcol.
  mc_read_sub 'l_log_chgvbfa' est_log_chgvbfa est_log_chgvbfa.
  mc_read_sub 'l_log_oldvbfa' est_log_oldvbfa est_log_oldvbfa.
  mc_read_sub 'l_log_newvbfa' est_log_newvbfa est_log_newvbfa.
  mc_read_sub 'l_slognew' est_slognew est_slognew.
  mc_read_sub 'l_log_unl' est_log_unl est_log_unl.
  mc_read_sub 'l_log_les' est_log_les est_log_les.
  mc_read_sub 'l_obs_log' est_obs_log est_obs_log.
  mc_read_sub 'l_agedch' est_agedch est_agedch.
  mc_read_sub 'l_aged' est_aged est_aged.
  mc_read_sub 'l_elaging' est_elaging real_elaging.
  mc_read_sub 'l_elarch' est_elarch real_elarch.
  mc_read_sub 'l_opt_data' est_opt_data opt_data.
  mc_read_sub 'l_opt_ws' est_opt_ws opt_ws.
  mc_read_sub 'l_optdisk' est_optdisk optdisk.
  mc_read_sub 'l_opt_size' est_opt_size opt_size.
  mc_read_sub 'l_opt_twocs' opt_twocs twocs.
  mc_read_sub 'l_cs_col' est_cs_col cs_col.
  mc_read_sub 'l_cs_mlob' est_cs_mlob cs_mlob.
  mc_read_sub 'l_cs_k' est_cs_k cs_k.
  mc_read_sub 'l_cs_pk' est_cs_pk cs_pk.
  mc_read_sub 'l_cs_hcard' est_cs_hcard cs_hcard.
  mc_read_sub 'l_cs_mcard' est_cs_mcard cs_mcard.
  mc_read_sub 'l_cs_lcard' est_cs_lcard cs_lcard.
  mc_read_sub 'l_cs_ucard' est_cs_ucard cs_ucard.
  mc_read_sub 'l_cs_rid' est_cs_rid cs_rid.
  mc_read_sub 'l_cs_udiv' est_cs_udiv cs_udiv.
  mc_read_sub 'l_cs_uk' est_cs_uk cs_uk.
  mc_read_sub 'l_cs_nuk' est_cs_nuk cs_nuk.
  mc_read_sub 'l_rs_col' est_rs_col rs_col.
  mc_read_sub 'l_rs_mlob' est_rs_mlob rs_mlob.
  mc_read_sub 'l_rs_pk' est_rs_pk rs_pk.
  mc_read_sub 'l_rs_sk' est_rs_sk rs_sk.
  mc_read_sub 'l_cshl ' est_cshl cshl.
  mc_read_sub 'l_rshl' est_rshl rshl.
  mc_read_sub 'l_cs_jec' est_cs_jec cs_jec.
  mc_read_sub 'l_cs_aging' est_cs_aging cs_aging.
  mc_read_sub 'l_cs_ft' est_cs_ft cs_ft.
  mc_read_sub 'l_updisk ' est_updisk est_updisk.
  mc_read_sub 'l_updata' est_updata est_updata.
  mc_read_sub 'l_upchrec' est_upchrec est_upchrec.
  mc_read_sub 'l_uptotal' est_uptotal est_uptotal.
  mc_read_sub 'l_shadisk' est_shadisk est_shadisk.
  mc_read_sub 'l_sha' est_sha est_sha.
  mc_read_sub 'l_shatotal' est_shatotal est_shatotal.
  mc_read_sub 'codestack' codestack codestack.
  mc_read_sub 'diskadmin' diskadmin diskadmin.
  mc_read_sub 'l_mlnew' est_mlnew est_mlnew.
  mc_read_sub 'l_obs_ml' est_obs_ml est_obs_ml.
  mc_read_sub 'l_init_pers' est_init_pers init_pers.
  mc_read_sub 'l_init_dram' est_init_dram init_dram.
  mc_read_sub 'l_opt_pers' est_opt_pers opt_pers.
  mc_read_sub 'l_opt_dram' est_opt_dram opt_dram.
  mc_read_sub 'l_error_ratio' error_ratio error_ratio.
  "end of alignment with WRITE_TO_DB

  "tables
  LOOP AT gt_hdbtabsizes ASSIGNING <hdbtabsize>.
    MOVE-CORRESPONDING <hdbtabsize> TO table_item.
    table_item-store         = <hdbtabsize>-store_type.
    IF wa_sizing-db_vendor = 'HDB'.
      table_item-real_size      = <hdbtabsize>-data_size.
      table_item-real_hl_size   = <hdbtabsize>-lob_size.
    ELSE.
      table_item-est_size      = <hdbtabsize>-data_size.
      table_item-est_hl_size   = <hdbtabsize>-lob_size.
    ENDIF.
    APPEND table_item TO gt_tables.
  ENDLOOP.

ENDFORM.                    "READ_SELECTED_SIZING

*&---------------------------------------------------------------------*
*&      Form  sizing_calculation_cpu
*&---------------------------------------------------------------------*
*        Perform the CPU sizing calculations
*
*----------------------------------------------------------------------*

FORM sizing_calculation_cpu USING cpu_err.

  TYPES: BEGIN OF ty_times,
           date       TYPE dats,
           time(8)    TYPE c,
           loadcnt    TYPE p LENGTH 12,
           loadcntbkg TYPE p LENGTH 12,
           lgth       TYPE i,
         END OF  ty_times.

  CONSTANTS: "downport 620
    c_d_st03 TYPE typename VALUE 'SWNC_T_SYSLOAD',
    c_h_st03 TYPE typename VALUE 'SWNCAGGTASKTIMES'.

  DATA: l_times  TYPE TABLE OF ty_times,
        wa_times TYPE ty_times,
        last_per TYPE dats,
        wa_avg   TYPE ty_cpu_avg,
        hloadcnt TYPE p LENGTH 12,
        c_sortfd TYPE fieldname,
        wa_dref  TYPE REF TO data,
        tab_dref TYPE REF TO data.

  FIELD-SYMBOLS: <times>     TYPE ty_times.

  FIELD-SYMBOLS: <l_d_st03>   TYPE STANDARD TABLE,
                 <lt_h_st03>  TYPE STANDARD TABLE,
                 <st03>       TYPE any,
                 <tasktimes>  TYPE any,
                 <wa_comp>    TYPE any,
                 <wa_time>    TYPE any,
                 <wa_phyread> TYPE any,
                 <wa_phychn>  TYPE any,
                 <wa_type>    TYPE any.

  MESSAGE s090(sada) WITH 'CPU Sizing calculations'.        "#EC NOTEXT
  TRY.
      CREATE DATA tab_dref TYPE (c_d_st03).
      ASSIGN tab_dref->* TO <l_d_st03>.
      CREATE DATA wa_dref LIKE LINE OF <l_d_st03>.
      ASSIGN wa_dref->* TO <st03>.
    CATCH cx_sy_create_data_error.
      cpu_err = abap_true.
      RETURN. "no CPU sizing.
  ENDTRY.

  TRY.
      CREATE DATA tab_dref TYPE STANDARD TABLE OF (c_h_st03).
      ASSIGN tab_dref->* TO <lt_h_st03>.
      CREATE DATA wa_dref LIKE LINE OF <lt_h_st03>.
    CATCH cx_sy_create_data_error.
      cpu_err = abap_true.
      RETURN. "no CPU sizing.
  ENDTRY.

  "start by calling day data to find out what days are stored
  CALL FUNCTION 'SWNC_COLLECTOR_GET_SYSTEMLOAD'
    EXPORTING
      component    = 'TOTAL'
      periodtype   = 'D'
      periodstrt   = sy-datum
    IMPORTING
      t_systemload = <l_d_st03>.

  "get available hour intervals from day data
  c_sortfd = 'PERIODSTRT'.
  SORT <l_d_st03> BY (c_sortfd).
  LOOP AT <l_d_st03> ASSIGNING <st03>.
    ASSIGN COMPONENT 'PERIODSTRT' OF STRUCTURE <st03> TO <wa_comp>.
    IF <wa_comp> <> last_per.
      last_per = <wa_comp>.
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          component     = 'TOTAL'
          periodtype    = 'D'
          periodstrt    = last_per
        TABLES
          tasktimes     = <lt_h_st03>
        EXCEPTIONS
          no_data_found = 1
          OTHERS        = 2.

      IF sy-subrc = 0.
        "sum tasks and separate bkg
        c_sortfd = 'TIME'.
        SORT <lt_h_st03> BY (c_sortfd).
        LOOP AT <lt_h_st03> ASSIGNING <tasktimes>.
          ASSIGN COMPONENT 'TIME'
            OF STRUCTURE <tasktimes> TO <wa_time>.
          ASSIGN COMPONENT 'PHYREADCNT'
            OF STRUCTURE <tasktimes> TO <wa_phyread>.
          ASSIGN COMPONENT 'PHYCHNGREC'
            OF STRUCTURE <tasktimes> TO <wa_phychn>.
          ASSIGN COMPONENT 'TASKTYPE'
            OF STRUCTURE <tasktimes> TO <wa_type>.

          IF <wa_time> <> wa_times-time.
            IF wa_times IS NOT INITIAL.
              APPEND wa_times TO l_times. CLEAR wa_times.
            ENDIF.
            wa_times-date = last_per.
            wa_times-time = <wa_time>.
          ENDIF.
          wa_times-loadcnt  = wa_times-loadcnt + <wa_phyread>
                           + <wa_phychn> * c_chg_cpu_factor.
          IF <wa_type> = 4. "bkg
            wa_times-loadcntbkg = wa_times-loadcntbkg + <wa_phyread>
                               + <wa_phychn> * c_chg_cpu_factor.
          ENDIF.
        ENDLOOP.
        APPEND wa_times TO l_times. CLEAR wa_times.
      ELSE. " no data found
      ENDIF.
    ENDIF.
    CLEAR <lt_h_st03>.
  ENDLOOP.
  CLEAR last_per.

  "Calculate average sql/h and peak sql/h on hour-based stats
  LOOP AT l_times ASSIGNING <times>.
    IF <times>-date <> last_per.
      IF last_per IS NOT INITIAL.
        APPEND wa_avg TO l_cpu_avg. CLEAR wa_avg.
      ENDIF.
      wa_avg-date = <times>-date.
      wa_avg-per = 'H'.
      last_per = <times>-date.
    ENDIF.
    <times>-lgth = <times>-time+4(2) - <times>-time(2).
    wa_avg-sum_ploadcnt  = wa_avg-sum_ploadcnt + <times>-loadcnt.
    wa_avg-sum_ploadcntbkg = wa_avg-sum_ploadcntbkg
                          + <times>-loadcntbkg.
    wa_avg-hcount        = wa_avg-hcount + <times>-lgth.
    hloadcnt = <times>-loadcnt / <times>-lgth.
    IF hloadcnt > wa_avg-peak_hloadcnt.
      wa_avg-peak_hloadcnt = hloadcnt.
    ENDIF.
  ENDLOOP.
  IF l_times IS NOT INITIAL.
    APPEND wa_avg TO l_cpu_avg. CLEAR wa_avg. CLEAR last_per.
  ENDIF.

  IF l_cpu_avg IS NOT INITIAL.
    " calculate average and peak factor
    LOOP AT l_cpu_avg ASSIGNING <hstat>.
      <hstat>-avg_hloadcnt = <hstat>-sum_ploadcnt / <hstat>-hcount.
      <hstat>-peak_factcnt = <hstat>-peak_hloadcnt
                        / <hstat>-avg_hloadcnt.
    ENDLOOP.

    "Now get the larger aggregates
    PERFORM add_to_avg USING 'D'.
    PERFORM add_to_avg USING 'W'.
    PERFORM add_to_avg USING 'M'.

    "Collects analysed periods
    SORT l_cpu_avg BY date.
    PERFORM get_cpu_periods USING 'H'.
    PERFORM get_cpu_periods USING 'D'.
    PERFORM get_cpu_periods USING 'W'.
    PERFORM get_cpu_periods USING 'M'.

    "Find the peak factor
    SORT l_cpu_avg BY peak_factcnt DESCENDING.
    READ TABLE l_cpu_avg INDEX 1 ASSIGNING <hstat>.
    wa_cpu_x-max_factcnt = <hstat>-peak_factcnt.
    wa_cpu_x-max_factcnt_date = <hstat>-date.
    "Multiply non-H data by peak factor
    LOOP AT l_cpu_avg ASSIGNING <hstat> WHERE per <> 'H'.
      <hstat>-peak_hloadcnt = <hstat>-avg_hloadcnt *
                              wa_cpu_x-max_factcnt.
    ENDLOOP.
    "Get peak
    SORT l_cpu_avg BY peak_hloadcnt DESCENDING.
    READ TABLE l_cpu_avg INDEX 1 ASSIGNING <hstat>.
    wa_cpu_x-datecnt = <hstat>-date.
    wa_cpu_x-percnt  = <hstat>-per.
    wa_cpu_x-max_hloadcnt = <hstat>-peak_hloadcnt.

    SORT l_cpu_avg BY date.
    SORT gt_cpu_per BY per.

    wa_cpu_x-est_sapscnt = wa_cpu_x-max_hloadcnt / c_sapscnt_factor
                            * 65 / 100. "Sizing at 65% CPU peak
    wa_cpu_x-est_sapscnt = ( wa_cpu_x-est_sapscnt + 10000 )
                            DIV 10000 * 10000.

    IF wa_cpu_x-est_sapscnt < 50000.
      wa_cpu_x-est_sapsts = 'XS'.
    ELSEIF wa_cpu_x-est_sapscnt < 90000.
      wa_cpu_x-est_sapsts = 'S'.
    ELSEIF wa_cpu_x-est_sapscnt < 140000.
      wa_cpu_x-est_sapsts = 'M'.
    ELSEIF wa_cpu_x-est_sapscnt < 200000.
      wa_cpu_x-est_sapsts = 'L'.
    ELSEIF wa_cpu_x-est_sapscnt < 300000.
      wa_cpu_x-est_sapsts = 'XL'.
    ELSEIF wa_cpu_x-est_sapscnt < 500000.
      wa_cpu_x-est_sapsts = 'XXL'.
    ELSEIF wa_cpu_x-est_sapscnt >= 500000.
      wa_cpu_x-est_sapsts = 'N/A'.
    ENDIF.

    "Check share of bkg at peak time
    READ TABLE l_cpu_avg INTO wa_avg
                          WITH KEY date = wa_cpu_x-datecnt
                                   per  = wa_cpu_x-percnt.
    wa_cpu_x-bkgratio = wa_avg-sum_ploadcntbkg
                      / wa_avg-sum_ploadcnt * 100.
  ELSE.
    wa_cpu_x-est_sapsts = 'N/A'.
  ENDIF.
ENDFORM.                    "cpu_sizing_calculation

*&---------------------------------------------------------------------*
*&      Form  get_cpu_periods
*&      get st03 periods for display
*&--------------------------------------------------------------------
FORM get_cpu_periods USING per.
  DATA: wa_per TYPE ty_cpu_per.
  LOOP AT l_cpu_avg ASSIGNING <hstat> WHERE per = per.
    IF wa_per-firstdate IS INITIAL.
      wa_per-firstdate = <hstat>-date.
      wa_per-per = <hstat>-per.
    ENDIF.
    wa_per-lastdate = <hstat>-date.
  ENDLOOP.
  IF wa_per IS NOT INITIAL.
    APPEND wa_per TO gt_cpu_per.
  ENDIF.
ENDFORM.                    "get_cpu_periods

*&---------------------------------------------------------------------*
*&      Form  add_to_avg
*&      add st03 aggregated statistics to average table
*&---------------------------------------------------------------------*

FORM add_to_avg USING l_per TYPE c.

  CONSTANTS: "downport 620
    c_d_st03     TYPE typename VALUE 'SWNC_T_SYSLOAD',
    c_periodstrt TYPE fieldname VALUE 'PERIODSTRT'.

  DATA: last_day TYPE dats,
        last_per TYPE dats,
        wa_avg   TYPE ty_cpu_avg,
        l_hour   TYPE i,
        wa_dref  TYPE REF TO data,
        tab_dref TYPE REF TO data,
        offset   TYPE i,
        i1       TYPE i,
        i2       TYPE i,
        i3       TYPE i.
  FIELD-SYMBOLS: <days>,
                 <l_d_st03>   TYPE STANDARD TABLE,
                 <st03>       TYPE any,
                 <wa_phyread> TYPE any,
                 <wa_phychn>  TYPE any,
                 <wa_type>    TYPE any,
                 <wa_time>    TYPE any.
  CONSTANTS: days(26) VALUE '00312831303130313130313031'.

  TRY.
      CREATE DATA tab_dref TYPE (c_d_st03).
      ASSIGN tab_dref->* TO <l_d_st03>.
      CREATE DATA wa_dref LIKE LINE OF <l_d_st03>.
      ASSIGN wa_dref->* TO <st03>.
    CATCH cx_sy_create_data_error.
      cpu_err = abap_true.
      RETURN. "no CPU sizing.
  ENDTRY.

  IF l_cpu_avg IS NOT INITIAL.
    SORT l_cpu_avg BY date.
    READ TABLE l_cpu_avg INDEX 1 INTO wa_avg.
    last_day = wa_avg-date. CLEAR wa_avg.
  ELSE.
    last_day = '31129999'.
  ENDIF.

  CALL FUNCTION 'SWNC_COLLECTOR_GET_SYSTEMLOAD'
    EXPORTING
      component    = 'TOTAL'
      periodtype   = l_per
      periodstrt   = sy-datum
    IMPORTING
      t_systemload = <l_d_st03>.

  IF <l_d_st03> IS NOT INITIAL.
    SORT <l_d_st03> BY (c_periodstrt).
    LOOP AT <l_d_st03> ASSIGNING <st03>." WHERE periodstrt < last_day.
      ASSIGN COMPONENT 'PERIODSTRT' OF STRUCTURE <st03> TO <wa_time>.
      ASSIGN COMPONENT 'PHYREADCNT' OF STRUCTURE <st03> TO <wa_phyread>.
      ASSIGN COMPONENT 'PHYCHNGREC' OF STRUCTURE <st03> TO <wa_phychn>.
      ASSIGN COMPONENT 'TASKTYPE' OF STRUCTURE <st03> TO <wa_type>.
      IF <wa_time> < last_day.
        IF <wa_time> <> last_per.
          IF last_per IS NOT INITIAL.
            APPEND wa_avg TO l_cpu_avg. CLEAR wa_avg.
          ENDIF.
          wa_avg-date = <wa_time>.
          wa_avg-per = l_per.
          last_per = <wa_time>.
        ENDIF.
        wa_avg-sum_ploadcnt = wa_avg-sum_ploadcnt + <wa_phyread>
                            + <wa_phychn> * c_chg_cpu_factor.
        IF <wa_type> = 4.
          wa_avg-sum_ploadcntbkg = wa_avg-sum_ploadcntbkg + <wa_phyread>
                            + <wa_phychn> * c_chg_cpu_factor.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND wa_avg TO l_cpu_avg. CLEAR: wa_avg, last_per.

    LOOP AT l_cpu_avg ASSIGNING <hstat> WHERE per = l_per.
      CASE l_per.
        WHEN 'D'.
          l_hour = 24.
        WHEN 'W'.
          l_hour = 7 * 24.
        WHEN 'M'.
          offset = 2 * <hstat>-date+4(2).
          ASSIGN days+offset(2) TO <days>.
          l_hour = <days> * 24.
          IF <hstat>-date+4(2) = '02'.
            i1 = <hstat>-date+4 MOD 4.
            i2 = <hstat>-date+4 MOD 100.
            i3 = <hstat>-date+4 MOD 400.
            IF i1 = 0 AND ( i2 <> 0 OR i3 = 0 ).
              l_hour = l_hour + 24.
            ENDIF.
          ENDIF.
      ENDCASE.
      <hstat>-hcount = l_hour.
      <hstat>-avg_hloadcnt = <hstat>-sum_ploadcnt / <hstat>-hcount.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "add_to_avg

*&---------------------------------------------------------------------*
*&      Form  call_output
*&---------------------------------------------------------------------*
*       output the result
*----------------------------------------------------------------------*

FORM call_output USING routine_name.
  DATA progname TYPE c LENGTH 20.
  progname = sy-repid.
  IF sy-dbsys(3) = 'HDB'.
    PERFORM (routine_name) IN PROGRAM (progname) USING 'HDB'
                      "top totals
                      cs_total rs_total hl hlch
                      initdisk init_data init_ws
                      init_size twocs
                      "Additional components
                      est_indx est_lc est_s4_trans
                      "Simple Finance
                      est_sfincold est_sfincoldch est_sfintab
                      est_sfincol
                      est_sfinnew est_fin_unl est_obs_fin
                      est_pca est_delcoep est_sl
                      "Simple Logistics
                      est_slogtab est_slogcol
                      est_log_chgvbfa est_log_oldvbfa est_log_newvbfa
                      est_slognew
                      est_log_unl est_log_les est_obs_log
                      est_rs2cs
                      "Material ledger actual costing
                      est_mlnew est_obs_ml
                      "Aging
                      est_agedch est_aged est_ageddisk
                      real_elaging real_elarch
                      "S/4 optimization
                      opt_data opt_ws optdisk opt_size
                      opt_twocs
                      "persistent memory
                      init_pers init_dram
                      opt_pers opt_dram
                      "Details
                      cs_col cs_mlob cs_k cs_pk
                      cs_hcard cs_mcard
                      cs_lcard cs_ucard
                      cs_rid cs_udiv cs_uk cs_nuk
                      rs_col rs_mlob rs_pk rs_sk
                      cshl rshl cs_jec
                      cs_aging cs_ft
                      "in-place upgrade
                      est_updisk est_updata est_upchrec est_uptotal
                      est_shadisk est_sha est_shatotal
                      "technical details
                      wa_techd
                      "business data
                      est_cl_lis
                      "errors
                      error_ratio.
  ENDIF.
  IF sy-dbsys(3) <> 'HDB' OR
    ( p_calib = abap_true AND routine_name = 'OUTPUT_ON_SCREEN' ).
    PERFORM (routine_name) IN PROGRAM (progname) USING 'ANY'
                      "top totals
                      est_cs_total est_rs_total est_hl est_hlch
                      est_initdisk est_init_data est_init_ws
                      est_init_size est_twocs
                      "Additional components
                      est_indx est_lc est_s4_trans
                      "Simple Finance
                      est_sfincold est_sfincoldch est_sfintab
                      est_sfincol
                      est_sfinnew  est_fin_unl est_obs_fin
                      est_pca est_delcoep est_sl
                      "Simple Logistics
                      est_slogtab est_slogcol
                      est_log_chgvbfa est_log_oldvbfa est_log_newvbfa
                      est_slognew
                      est_log_unl est_log_les est_obs_log
                      est_rs2cs
                      "Material ledger actual costing
                      est_mlnew est_obs_ml
                      "Aging
                      est_agedch est_aged est_ageddisk
                      est_elaging est_elarch
                      "S/4 optimization
                      est_opt_data est_opt_ws est_optdisk
                      est_opt_size
                      opt_twocs
                      "persistent memory
                      est_init_pers est_init_dram
                      est_opt_pers est_opt_dram
                      "Details
                      est_cs_col est_cs_mlob est_cs_k est_cs_pk
                      est_cs_hcard est_cs_mcard
                      est_cs_lcard est_cs_ucard
                      est_cs_rid est_cs_udiv est_cs_uk est_cs_nuk
                      est_rs_col est_rs_mlob est_rs_pk est_rs_sk
                      est_cshl est_rshl est_cs_jec
                      est_cs_aging est_cs_ft
                      "in-place upgrade
                      est_updisk est_updata est_upchrec est_uptotal
                      est_shadisk est_sha est_shatotal
                      "technical details
                      wa_techd
                      "business data
                      est_cl_lis
                      "errors
                      error_ratio.
  ENDIF.
ENDFORM.                    "call_output
*&---------------------------------------------------------------------*
*&      Form  calculate_business_data
*&---------------------------------------------------------------------*
*       get business data size for cloud license.
*----------------------------------------------------------------------*

FORM calculate_business_data CHANGING l_cl_lis.
  TYPES: BEGIN OF ty_cli,
           tabname TYPE tabname,
         END OF ty_cli.
  DATA: gt_cli TYPE TABLE OF ty_cli.
  FIELD-SYMBOLS: <cli> TYPE ty_cli.

  IF sy-dbsys(3) = 'HDB' OR p_calib = abap_true.
    IF gt_tables IS NOT INITIAL.
      SELECT tabname FROM dd03l INTO TABLE gt_cli
        FOR ALL ENTRIES IN gt_tables
        WHERE tabname = gt_tables-table_name
        AND datatype = 'CLNT'.
    ENDIF.
    LOOP AT gt_cli ASSIGNING <cli>.
      READ TABLE gt_tables ASSIGNING <item>
        WITH KEY table_name = <cli>-tabname BINARY SEARCH.
      IF sy-subrc = 0.
        <item>-clitab = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT gt_tables ASSIGNING <item>.
    READ TABLE <item>-cs_cols WITH KEY client = abap_true
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0 OR <item>-clitab = abap_true.
      "all new tables are flagged as client-dependent.
      IF sy-dbsys(3) = 'HDB'.
        l_cl_lis = l_cl_lis + <item>-real_size.
      ELSE.
        l_cl_lis = l_cl_lis + <item>-est_size.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "calculate_business_data

*&---------------------------------------------------------------------*
*&      Form  sml_sizing
*&---------------------------------------------------------------------*
*       Material ledger S/4 change
*----------------------------------------------------------------------*

FORM sml_sizing.

  CONSTANTS:
    c_t001w            TYPE tabname VALUE 'T001W',
    c_mldoc_rec        TYPE i VALUE 47,
    c_mldocccs_rec     TYPE i VALUE 50,
    c_mldoc_ext_rec    TYPE i VALUE 24,
    c_mldocccs_ext_rec TYPE i VALUE 82.
  DATA:
    l_actco   TYPE abap_bool,
    ls_ml     TYPE ty_ssol,
    lv_mlitrc TYPE dec20.

  SELECT SINGLE mgvupd FROM (c_t001w) INTO l_actco
    WHERE mgvupd = abap_true.

  IF l_actco IS NOT INITIAL.
    IF do_sml = abap_true.

      "MLDOC.
      READ TABLE gt_tables ASSIGNING <item>
       WITH KEY table_name = 'MLIT'.
      IF sy-subrc = 0. lv_mlitrc = <item>-record_count. ENDIF.
      READ TABLE gt_tables ASSIGNING <item>
       WITH KEY table_name = 'MLHD'.
      IF sy-subrc = 0.
        READ TABLE <item>-cs_cols ASSIGNING <cs_size>
         WITH KEY column_name = 'BLDAT'.
        ls_ml-table_name = 'MLDOC'.                         "#EC NOTEXT
        ls_ml-group = 'AC'.                                 "#EC NOTEXT
        ls_ml-record_count = lv_mlitrc * <cs_size>-spec_count
           / <item>-record_count * 120 / 100.
        ls_ml-ssize = ls_ml-record_count * c_mldoc_rec.
        PERFORM add2gt_tables USING ls_ml.
        APPEND ls_ml TO gt_ml. CLEAR ls_ml.

        "MLDOC_EXTRACT
        READ TABLE gt_ml ASSIGNING <ml> WITH KEY table_name = 'MLDOC'.
        ls_ml-table_name = 'MLDOC_EXTRACT'.                 "#EC NOTEXT
        ls_ml-group = 'AC'.                                 "#EC NOTEXT
        ls_ml-record_count = <ml>-record_count / 100.
        ls_ml-ssize = ls_ml-record_count * c_mldoc_ext_rec.
        PERFORM add2gt_tables USING ls_ml.
        APPEND ls_ml TO gt_ml. CLEAR ls_ml.
      ENDIF.

      "MLDOCCCS
      READ TABLE gt_tables ASSIGNING <item>
       WITH KEY table_name = 'CKMLPRKEPH'.
      tabix = sy-tabix.
      IF sy-subrc =  0.
        "if HDB we do not have the spec_count yet
        IF sy-dbsys(3) = 'HDB' AND <item>-record_count > 0.
          CLEAR gt_input. APPEND <item> TO gt_input.
          IF gt_input IS NOT INITIAL.
            DELETE gt_tables INDEX tabix.
            MESSAGE s090(sada) WITH
            'Start of statistics collection for CKMLPRKEPH'. "#EC NOTEXT
            mode = 'CAL'.
            PERFORM collect_stats USING mode.

            READ TABLE gt_columns ASSIGNING <columns>
              WITH KEY table_name = 'CKMLPRKEPH' column_name = 'KST001'.
            IF sy-subrc = 0.
              READ TABLE gt_tables ASSIGNING <item>
                 WITH KEY table_name = <columns>-table_name.
              READ TABLE <item>-cs_cols ASSIGNING <cs_size>
               WITH KEY column_name = <columns>-column_name.
              <cs_size>-spec_count = <columns>-spec_count.
            ENDIF.
            REFRESH gt_columns.
          ENDIF.
        ENDIF.

        READ TABLE gt_tables ASSIGNING <item>
         WITH KEY table_name = 'CKMLPRKEPH'.
        READ TABLE <item>-cs_cols ASSIGNING <cs_size>
         WITH KEY column_name = 'KST001'.
        IF sy-subrc = 0.
          ls_ml-table_name = 'MLDOCCCS'.                    "#EC NOTEXT
          ls_ml-group = 'AC'.                               "#EC NOTEXT
          ls_ml-record_count = lv_mlitrc * <cs_size>-spec_count.
          ls_ml-ssize = ls_ml-record_count * c_mldocccs_rec.
          PERFORM add2gt_tables USING ls_ml.
          APPEND ls_ml TO gt_ml. CLEAR ls_ml.
        ENDIF.

        "MLDOCCCS_EXTRACT
        READ TABLE gt_ml ASSIGNING <ml>
            WITH KEY table_name = 'MLDOCCCS'.
        IF subrc = 0.
          ls_ml-table_name = 'MLDOCCCS_EXTRACT'.            "#EC NOTEXT
          ls_ml-group = 'AC'.                               "#EC NOTEXT
          ls_ml-record_count = <ml>-record_count / 100.
          ls_ml-ssize = ls_ml-record_count * c_mldocccs_ext_rec.
          PERFORM add2gt_tables USING ls_ml.
          APPEND ls_ml TO gt_ml. CLEAR ls_ml.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT gt_tables ASSIGNING <item> WHERE
        table_name = 'MLCD' OR "table_name = 'CKMLPP' OR --not yet
        table_name = 'CKMLMV003' OR table_name = 'CKMLMV004'.
      MOVE-CORRESPONDING <item> TO ls_ml.
      ls_ml-group = 'OBS'.
      IF sy-dbsys(3) = 'HDB'.
        ls_ml-ssize = <item>-real_size.
      ELSE.
        ls_ml-ssize = <item>-est_size.
      ENDIF.
      APPEND ls_ml TO gt_ml. CLEAR ls_ml.
    ENDLOOP.

    LOOP AT gt_ml ASSIGNING <ml>.
      CASE <ml>-group.
        WHEN 'AC'.
          est_mlnew = est_mlnew + <ml>-ssize.
        WHEN 'OBS'.
          est_obs_ml = est_obs_ml + <ml>-ssize.
      ENDCASE.
    ENDLOOP.
  ENDIF.


ENDFORM.                    "sml_sizing

*&---------------------------------------------------------------------*
*& FORM  add_sep_line
*& Write a sum line inside a table.
*&
*&---------------------------------------------------------------------*
FORM add_sep_line.
  WRITE: / '|',
  AT 5  '---------------------------------------------------'.
  WRITE: AT 37 '-----------------------------------------', AT 80 '|'.
ENDFORM.                    "flush

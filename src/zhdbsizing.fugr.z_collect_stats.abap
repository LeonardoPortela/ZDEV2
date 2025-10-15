FUNCTION z_collect_stats.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PRECISION) TYPE  DTPRECINT DEFAULT 'M'
*"     VALUE(MODE) TYPE  STRING OPTIONAL
*"     VALUE(LOAD) TYPE  BOOLEAN OPTIONAL
*"     VALUE(HL) TYPE  BOOLEAN OPTIONAL
*"     VALUE(MAC) TYPE  BOOLEAN OPTIONAL
*"     VALUE(CLUSTPREC) TYPE  DTPRECINT OPTIONAL
*"     VALUE(MSHL) TYPE  BOOLEAN OPTIONAL
*"     VALUE(DAAG) TYPE  NUMC3 OPTIONAL
*"     VALUE(FORCE_COUNT) TYPE  BOOLEAN OPTIONAL
*"     VALUE(STAT_AGE) TYPE  INT4 DEFAULT 5
*"     VALUE(MIN_REC_CNT) TYPE  INT4 DEFAULT 100000
*"     VALUE(HDB_EMPTY_TABLES) TYPE  BOOLEAN OPTIONAL
*"     VALUE(LRAW) TYPE  INT4 DEFAULT 5000
*"  CHANGING
*"     VALUE(DATA) TYPE  XSTRING
*"  EXCEPTIONS
*"      SYSTEM_ERROR
*"----------------------------------------------------------------------

  DATA:
    e_tables      TYPE typ_t_hana_db_size,
    e_columns     TYPE typ_t_columns,
    e_realrsindex TYPE typ_t_realrsindex,
    e_realrs      TYPE typ_t_realrs,
    e_realcs      TYPE typ_t_realcs,
    lv_timeout    TYPE spfpflpar-pvalue,
    rt            TYPE typ_rt.
  FIELD-SYMBOLS: <lv_hex> TYPE x.
  CONSTANTS:
    class_name TYPE syrepid VALUE 'CL_RUNTIME'.

  IMPORT p1 = e_tables FROM DATA BUFFER data.

  PERFORM security_check.

  IF mode(3) = 'RET'.
    GET TIME FIELD rt-initime.
    TRY.
        CALL METHOD (class_name)=>get_my_max_runtime
          EXPORTING
            max_runtime = lv_timeout.
      CATCH cx_root.
    ENDTRY.
    IF lv_timeout IS INITIAL.
      TRY.
          CALL FUNCTION 'RSAN_SYSTEM_PARAMETER_READ'
            EXPORTING
              i_name  = 'rdisp/max_wprun_time'
            IMPORTING
              e_value = lv_timeout.
          rt-maxs = lv_timeout.
        CATCH cx_root.                                  "#EC NO_HANDLER
          "do nothing. timeout will occur if time is exceeded.
      ENDTRY.
    ENDIF.
  ENDIF.

  IF sy-dbsys(3) <> 'HDB'
      OR ( sy-dbsys(3) = 'HDB' AND mode+3(3) = 'CAL' ).
    ASSIGN en_quad TO <lv_hex> CASTING. <lv_hex> = '0020'.
    PERFORM calc_data TABLES   e_tables
                               e_columns
                      USING    precision
                               clustprec
                               mshl
                               daag
                               force_count
                               stat_age
                               min_rec_cnt
                               lraw
                    CHANGING rt.
  ENDIF.

  IF sy-dbsys(3) = 'HDB'.
    PERFORM read_real_sizes TABLES e_tables
                                   e_realrsindex
                                   e_realrs
                                   e_realcs
                             USING load
                                   hl
                                   mac
                                   daag
                                   hdb_empty_tables.
  ENDIF.

  EXPORT p1 = e_tables
         p2 = e_columns
         p3 = e_realrsindex
         p4 = e_realrs
         p5 = e_realcs
         TO DATA BUFFER data.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  calc_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_TABLES     text
*      -->SAMPLE_RATIO  text
*----------------------------------------------------------------------*
FORM calc_data TABLES    it_tables  TYPE typ_t_hana_db_size
                         e_columns  TYPE typ_t_columns
               USING     precision  TYPE c
                         clustprec  TYPE c
                         mshl       TYPE c
                         daag       TYPE numc3
                         force_count TYPE boolean
                         stat_age    TYPE i
                         min_rec_cnt TYPE i
                         lraw       TYPE i
               CHANGING  rt         TYPE typ_rt.
*
  FIELD-SYMBOLS:
    <table>   TYPE typ_hana_db_size.
  DATA:
    l_record_count  TYPE typ_hana_db_size-record_count,
    l_return_code   TYPE typ_hana_db_size-return_code,
    l_error_message TYPE typ_hana_db_size-error_message,
    l_doubt_reason  TYPE typ_hana_db_size-doubt_reason,
    l_count_method  TYPE typ_hana_db_size-count_method,
    l_stat_age      TYPE typ_hana_db_size-stat_age,
    lt_table        TYPE typ_t_hana_db_size,
    lt_table_new    TYPE typ_t_hana_db_size,
    l_db_table      TYPE tabname,
    l_sample_ratio  TYPE p LENGTH 10 DECIMALS 7,
    l_dbok          TYPE sy-subrc.

  SORT it_tables BY db_table.
* Preset result table
  LOOP AT it_tables ASSIGNING <table>.
    <table>-record_count          = -1.
    <table>-return_code = cx_undefined.
  ENDLOOP.

  "Identify Info Objects
  PERFORM determine_table_type TABLES it_tables.

  IF sy-dbsys(3) <> 'HDB' AND sy-dbsys(3) <> 'ADA'.
    l_count_method = 1.
    PERFORM calc_data_old TABLES it_tables.
  ENDIF.

  LOOP AT it_tables ASSIGNING <table>.
    IF l_db_table <> <table>-db_table.
      l_record_count = -1.
      CLEAR: l_return_code, l_error_message, l_doubt_reason, l_stat_age.
      CASE sy-dbsys(3).
        WHEN 'ORA'.
*         Do not trust NROWS,
*         but get cardinality freshly from system catalog
          l_count_method = 2.
          PERFORM get_cardinality_ora USING <table>-db_table
                                   CHANGING l_record_count
                                            l_return_code
                                            l_error_message
                                            l_doubt_reason
                                            l_stat_age.
        WHEN 'DB6'.
          l_count_method = 2.
          PERFORM get_cardinality_db6 USING <table>-db_table
                                   CHANGING l_record_count
                                            l_return_code
                                            l_error_message.
        WHEN 'HDB'.
          l_count_method = 2.
          PERFORM get_cardinality_hdb USING <table>-db_table
                                  CHANGING l_record_count
                                           l_return_code
                                           l_error_message.
        WHEN OTHERS.
*         By default, use cardinality provided by
*         DB_STATISTICS_DATA_READ.
          l_record_count = <table>-record_count.
      ENDCASE.

      IF l_stat_age IS NOT INITIAL "stat age was checked
        AND l_stat_age >= stat_age "stats older than accepted
        AND l_record_count > min_rec_cnt "Big enough not to ignore
        AND force_count = abap_true "User requested count
        AND l_doubt_reason <> 1. "Not obviously inconsistent
        l_doubt_reason = 2. "force count.
      ENDIF.
      IF ( <table>-record_count <= 0 AND l_record_count = -1 )
       OR l_record_count < -1 "case of volatile tables on Oracle.
       OR l_doubt_reason = 1  "case of obvious inconsistent statistics
       OR l_doubt_reason = 2. "case of forced count
*        In case of cluster count on db_table will work.
        CALL FUNCTION 'DB_EXISTS_TABLE'
          EXPORTING
            tabname = <table>-db_table
          IMPORTING
            subrc   = l_dbok.
        IF l_dbok = 0.
          l_count_method = 3.
          SELECT COUNT(*) INTO l_record_count
                  FROM (<table>-db_table) CLIENT SPECIFIED.
          <table>-record_count = l_record_count.
          CLEAR l_return_code.
        ELSE.
          l_return_code = 5.
          l_error_message = 'Table not found in database'.  "#EC NOTEXT
        ENDIF.
      ENDIF.

      IF l_db_table IS NOT INITIAL. "not the very first time
        PERFORM calc_data_sample_new USING  l_sample_ratio
                                            precision
                                            clustprec
                                            mshl
                                            daag
                                            lraw
                                   CHANGING lt_table[]
                                            e_columns[]
                                            rt.
        APPEND LINES OF lt_table TO lt_table_new.
        CLEAR lt_table[].
      ENDIF.

      l_db_table = <table>-db_table.
      PERFORM default_sampling USING l_record_count
                                     precision
                               CHANGING l_sample_ratio.
      IF l_sample_ratio = 0.
        l_sample_ratio = '0.000001'.
      ENDIF.
    ENDIF.
    IF l_record_count <> -1.
      "for ora,db6,hdb, trust l_record_count, not <table>-record_count.
      "For other db, this is redundant.
      <table>-record_count   = l_record_count.
    ENDIF.
    <table>-return_code   = l_return_code.
    <table>-error_message = l_error_message.
    <table>-doubt_reason  = l_doubt_reason.
    <table>-count_method  = l_count_method.
    <table>-stat_age      = l_stat_age.
    APPEND <table> TO lt_table.
  ENDLOOP.
* last execution after loop.
  PERFORM calc_data_sample_new USING  l_sample_ratio
                                      precision
                                      clustprec
                                      mshl
                                      daag
                                      lraw
                             CHANGING lt_table[]
                                      e_columns[]
                                      rt.
  APPEND LINES OF lt_table TO lt_table_new.
  it_tables[] = lt_table_new.

ENDFORM."calc_data

*&--------------------------------------------------------------------*
*&      Form  calc_data_sample_new
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->SYS_REF    text
*      -->SAMPLE_RATItext
*      -->PRECISION  text
*      -->T_TABLE    text
*      -->T_COLUMNS  text
*---------------------------------------------------------------------*
FORM calc_data_sample_new
                      USING     sample_ratio TYPE p
                                precision    TYPE c
                                clustprec    TYPE c
                                mshl         TYPE c
                                daag         TYPE numc3
                                lraw         TYPE i
                      CHANGING  t_table      TYPE typ_t_hana_db_size
                                e_columns    TYPE typ_t_columns
                                rt           TYPE typ_rt.

  TYPES:
    BEGIN OF typ_pool_key,
      tabname(10) TYPE c,
      varkey(50)  TYPE c,
    END OF typ_pool_key.

  STATICS:
      stmt_ref                 TYPE REF TO cl_sql_statement.

  FIELD-SYMBOLS:
    <wa_db>          TYPE any,
    <wa_abap>        TYPE any,
    <wa_abap_key>    TYPE any,
    <wa_comp>        TYPE any,
    <pool_tab>       TYPE typ_pool_key-tabname,
    <where_strg>     TYPE typ_where_strg,
    <table>          TYPE typ_hana_db_size,
    <dbfield>        TYPE dbfield,
    <abap_table_ref> TYPE typ_abap_table_ref,
    <d1field>        TYPE dfies,
    <d2field>        TYPE dfies,
    <col>            TYPE typ_columns,
    <h_col>          TYPE typ_h_columns,
    <h_hlcol>        TYPE typ_h_hlcolumns.

  DATA:
    is_lob                TYPE abap_bool,
    subrc                 TYPE sysubrc,
    len                   TYPE i,
    cnt_keyflds           TYPE i,
    l_rows                TYPE i,
    records_selected      TYPE i,
    abap_records_selected TYPE i,
    max_records           TYPE i,
    max_abap_records      TYPE i,
    dbfields              TYPE TABLE OF dbfield,
    ddfields              TYPE TABLE OF dfies,
    db_table_ddfields     TYPE TABLE OF dfies,
    ls_where_strg         TYPE typ_where_strg,
    where_tab             TYPE TABLE OF typ_where_strg,
    structdescr           TYPE REF TO cl_abap_structdescr,
    compdescr             TYPE REF TO cl_abap_typedescr,
    lv_tabix              TYPE sy-tabix,
    ref                   TYPE REF TO data,
    db_ref                TYPE REF TO data,
    abap_table_ref        TYPE typ_abap_table_ref,
    abap_table_refs       TYPE TABLE OF typ_abap_table_ref,
    rows_ret              TYPE i,
    sqlerr_ref            TYPE REF TO cx_sql_exception,
    sqlroot_ref           TYPE REF TO cx_root,
    osql_err_ref          TYPE REF TO cx_sy_dynamic_osql_semantics,
    rs_ref                TYPE REF TO cl_sql_result_set,
    sql_stmt              TYPE string,
    column_name           TYPE string,
    column_list           TYPE string,
    sample_ratio_char     TYPE string,
    desc_tab              TYPE tabname,
    fulltablename         TYPE string,
    l_dbok                TYPE sy-subrc,
    values                LIKE TABLE OF dd07v,
    funcname              TYPE rs38l_fnam,
    BEGIN OF db_table,
      db_table     TYPE tabname,
      tab_type     TYPE tabclass,
      record_count TYPE typ_hana_db_size-record_count,
    END OF db_table,
    db_table_curr  LIKE db_table,
    lt_h_columns   TYPE typ_ht_columns,
    lt_h_hlcolumns TYPE typ_ht_hlcolumns,
    ls_columns     TYPE typ_columns,
    lt_columns     TYPE typ_t_columns.

  CONSTANTS:
    op_and(5) TYPE c VALUE ''' AND',
    op_end(5) TYPE c VALUE ''''.

  ls_where_strg-op_and = op_end.
  ls_where_strg-op_eq  = '= '''.

  IF stmt_ref IS INITIAL.
    TRY.
        CREATE OBJECT stmt_ref.
      CATCH cx_sql_exception INTO sqlerr_ref.
        RAISE system_error.
    ENDTRY.
  ENDIF.

* prepare result structures for ABAP tables.
  LOOP AT t_table ASSIGNING <table>.
    MOVE-CORRESPONDING <table> TO db_table_curr.
    IF db_table_curr <> db_table.
      "make sure that the db table properties are filled correctly in
      "t_table
      IF db_table IS INITIAL.
        db_table = db_table_curr.
        IF db_table-record_count <= 0.
          " early exit in case no records are found
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE x000(00).
      ENDIF.
    ENDIF.
    CLEAR: ddfields, abap_table_ref.

    TRY.
        "specify the absolute type to avoid dump if inconsistencies
        CLEAR desc_tab.
        CONCATENATE '\TYPE=' <table>-table_name INTO desc_tab.
        CALL METHOD cl_abap_structdescr=>describe_by_name
          EXPORTING
            p_name         = desc_tab
          RECEIVING
            p_descr_ref    = compdescr
          EXCEPTIONS
            type_not_found = 1.
        IF sy-subrc <> 0.
          <table>-return_code = 2.
          RETURN.
        ENDIF.
        structdescr ?= compdescr.
        ddfields = structdescr->get_ddic_field_list( ).

        IF <table>-tab_type = 'TRANSP'.
          CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
            EXPORTING
              function_module = '/SDF/CONVERT_DECFLOAT'
            EXCEPTIONS
              not_existent    = 1
              OTHERS          = 2.
          IF sy-subrc = 0.
            funcname = '/SDF/CONVERT_DECFLOAT'.
          ELSE.
            CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
              EXPORTING
                function_module = 'Z_CONVERT_DECFLOAT'
              EXCEPTIONS
                not_existent    = 1
                OTHERS          = 2.
            IF sy-subrc = 0.
              funcname = 'Z_CONVERT_DECFLOAT'.
            ENDIF.
          ENDIF.
          IF funcname IS NOT INITIAL.
            TRY.
                CALL FUNCTION funcname
                  EXPORTING
                    table_name  = <table>-table_name
                  CHANGING
                    structdescr = structdescr
                  EXCEPTIONS
                    not_found   = 1
                    OTHERS      = 2.
                IF sy-subrc <> 0.
                  EXIT. "no conversion.
                ENDIF.
              CATCH cx_sy_dyn_call_illegal_func.
            ENDTRY.
          ENDIF.
        ENDIF.

        CREATE DATA abap_table_ref-ref TYPE HANDLE structdescr.

      CATCH cx_root INTO sqlroot_ref.
        <table>-return_code   = 2.
        <table>-error_message = sqlroot_ref->get_text( ).
        RETURN.
    ENDTRY.
    abap_table_ref-tabname  = <table>-table_name.
    abap_table_ref-ddfields = ddfields.

    LOOP AT ddfields ASSIGNING <d1field>  WHERE datatype = 'LRAW'.
*                                                datatype = 'LCHR'.
      lv_tabix = <d1field>-position - 1.
      " The number of characters of the types LRAW and LCHR is the
      " value of a preceding INT2 or INT4 field in a transparent
      " database table.
      " LCHR is commented out because of error described in SAP Note
      " 1770416 and STRLEN works with it anyway.
      IF lv_tabix > 0.
        READ TABLE ddfields INDEX lv_tabix ASSIGNING <d2field>.
        IF <d2field>-datatype = 'INT2'  OR <d2field>-datatype = 'INT4'.
          abap_table_ref-intfield = <d2field>-fieldname.
          abap_table_ref-rawfield = <d1field>-fieldname.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF <table>-tab_type = 'TRANSP'.
      " in case of transp. tables DB and ABAP structures are identical
      db_ref              = abap_table_ref-ref.
      db_table_ddfields   = abap_table_ref-ddfields.
    ELSEIF <table>-tab_type = 'POOL'.
      CLEAR: cnt_keyflds.
      LOOP AT ddfields ASSIGNING <d1field> WHERE keyflag = 'X'.
        cnt_keyflds = cnt_keyflds + 1.
        IF cnt_keyflds > 16.
          EXIT.
        ENDIF.
        IF ls_where_strg-op_and = op_end.
          ls_where_strg-op_and = op_and.
        ELSE.
          APPEND ls_where_strg TO abap_table_ref-where_tab.
        ENDIF.
        len = <d1field>-leng.
        abap_table_ref-keylen = len + abap_table_ref-keylen.
        ls_where_strg-fieldname = <d1field>-fieldname.
      ENDLOOP.
      ls_where_strg-op_and = op_end.
      APPEND ls_where_strg TO abap_table_ref-where_tab.

      IF cnt_keyflds > 16 OR <table>-table_name(5) = 'M_MTV'.
        " Table remains in Pool if number of keyfields is larger than 16
        " or if it's a matchcode table!
        <table>-table_name = <table>-db_table.
        FREE abap_table_ref-where_tab.
        TRY.
            "specify the absolute type to avoid dump if inconsistencies
            CLEAR desc_tab.
            CONCATENATE '\TYPE=' <table>-db_table INTO desc_tab.
            CALL METHOD cl_abap_structdescr=>describe_by_name
              EXPORTING
                p_name         = desc_tab
              RECEIVING
                p_descr_ref    = compdescr
              EXCEPTIONS
                type_not_found = 1.
            IF sy-subrc <> 0.
              <table>-return_code = 2.
            ENDIF.
            structdescr ?= compdescr.
            CREATE DATA abap_table_ref-ref TYPE HANDLE structdescr.
            abap_table_ref-ddfields =
            structdescr->get_ddic_field_list( ).
*           DB structure is identical to ABAP structure in this case.
            db_ref = abap_table_ref-ref.
            db_table_ddfields   = abap_table_ref-ddfields.

          CATCH cx_root INTO sqlroot_ref.
            <table>-return_code = 2.
            <table>-error_message = sqlroot_ref->get_text( ).
            CONTINUE.
        ENDTRY.
      ENDIF.
    ENDIF.
    abap_table_ref-table = <table>.
    APPEND abap_table_ref TO abap_table_refs.
  ENDLOOP.

  CALL FUNCTION 'DB_GET_TABLE_FIELDS'
    EXPORTING
      tabname  = db_table-db_table
    IMPORTING
      subrc    = subrc
    TABLES
      dbfields = dbfields.
  IF dbfields[] IS INITIAL OR subrc <> 0.
    LOOP AT t_table ASSIGNING <table>.
      <table>-return_code = 1.
    ENDLOOP.
    RETURN.
  ENDIF.

* prepare structures for DB select (in case not already prepared)
  IF db_ref IS NOT BOUND.
    " In case of TRANSP and POOL tables with tables remaining in pool
    " data is already prepared
    TRY.
        "specify the absolute type to avoid dump if inconsistencies
        CLEAR desc_tab.
        CONCATENATE '\TYPE=' <table>-db_table INTO desc_tab.
        CALL METHOD cl_abap_structdescr=>describe_by_name
          EXPORTING
            p_name         = desc_tab
          RECEIVING
            p_descr_ref    = compdescr
          EXCEPTIONS
            type_not_found = 1.
        IF sy-subrc <> 0.
          LOOP AT t_table ASSIGNING <table>.
            <table>-return_code =  2.
          ENDLOOP.
          RETURN.
        ENDIF.
        structdescr ?= compdescr.
        db_table_ddfields = structdescr->get_ddic_field_list( ).
      CATCH cx_root INTO sqlroot_ref.
        LOOP AT t_table ASSIGNING <table>.
          <table>-return_code =  2.
          <table>-error_message = sqlroot_ref->get_text( ).
        ENDLOOP.
        RETURN.
    ENDTRY.

    " remove all non key fields, because only key fields have to be
    " selected.

    IF db_table-tab_type = 'CLUSTER' .
      LOOP AT db_table_ddfields ASSIGNING <d1field>
           WHERE keyflag = 'X' AND fieldname <> 'PAGENO'.
        IF ls_where_strg-op_and = op_end.
          ls_where_strg-op_and = op_and.
        ELSE.
          APPEND ls_where_strg TO where_tab.
        ENDIF.
        ls_where_strg-fieldname = <d1field>-fieldname.
      ENDLOOP.
      ls_where_strg-op_and = op_end.
      APPEND ls_where_strg TO where_tab.
      TRY.
          CREATE DATA db_ref TYPE HANDLE structdescr.
        CATCH cx_root INTO sqlroot_ref.
          <table>-return_code =  3.
          <table>-error_message = sqlroot_ref->get_text( ).
      ENDTRY.

    ELSEIF db_table-tab_type = 'POOL'.
      DELETE db_table_ddfields WHERE keyflag <> 'X'.

      LOOP AT db_table_ddfields ASSIGNING <d1field>
          WHERE fieldname <> 'TABNAME' OR
                fieldname <> 'VARKEY'.
      ENDLOOP.
      IF sy-subrc <> 0.
        MESSAGE x000(00).
      ENDIF.

      TRY.
          CREATE DATA db_ref TYPE typ_pool_key .
        CATCH cx_root INTO sqlroot_ref.
          <table>-return_code =  3.
          <table>-error_message = sqlroot_ref->get_text( ).
      ENDTRY.
    ENDIF.
  ENDIF.

* calculate the max number of records from sample ratio.
  CASE precision.
    WHEN 'L'.
      max_records = 10000.
    WHEN 'M'.
      max_records = 100000.
    WHEN 'H'.
      max_records = 1000000.
  ENDCASE.

  CASE clustprec.
    WHEN 'S'.
      max_abap_records = max_records * 5.
    WHEN 'M'.
      max_abap_records = max_records * 50.
    WHEN 'L'.
      max_abap_records = max_records * 250.
    WHEN 'X'.
      max_abap_records = max_records * 500.
    WHEN OTHERS.
      max_abap_records = max_records * 5.
  ENDCASE.

* create SQL statement.
  LOOP AT db_table_ddfields ASSIGNING  <d1field>.
    CONCATENATE `"` <d1field>-fieldname `"` INTO column_name.
    IF column_list IS INITIAL.
      column_list = column_name.
    ELSE.
      CONCATENATE column_list `,` column_name INTO column_list
        SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  sample_ratio_char = sample_ratio.

  CONCATENATE '"' db_table-db_table '"' INTO fulltablename.

  CASE sy-dbsys(3).
    WHEN 'DB6' OR 'MSS'.
      CONCATENATE
        'SELECT' column_list 'FROM' fulltablename
        'TABLESAMPLE SYSTEM(' sample_ratio_char ')'
        INTO sql_stmt SEPARATED BY space.
    WHEN 'ORA'.
      IF sample_ratio = 100.
        CONCATENATE
          'SELECT' column_list 'FROM' fulltablename
          INTO sql_stmt SEPARATED BY space.
      ELSE.
        CONCATENATE
          'SELECT' column_list
          'FROM' fulltablename
          'SAMPLE(' sample_ratio_char ')'
          INTO sql_stmt SEPARATED BY space.
      ENDIF.
    WHEN 'DB2'.
      sample_ratio_char = sample_ratio / 100.
      CONCATENATE
        'SELECT' column_list 'FROM' fulltablename
        'WHERE RAND() < ' sample_ratio_char
        INTO sql_stmt SEPARATED BY space.
    WHEN 'SYB'.
      sample_ratio_char = sample_ratio / 100.
      CONCATENATE
        'SELECT' column_list 'FROM' fulltablename
        'WHERE RAND2() < ' sample_ratio_char
        INTO sql_stmt SEPARATED BY space.
    WHEN 'DB4'.
      sample_ratio_char = sample_ratio / 100.
      CONCATENATE
        'SELECT' column_list 'FROM ('
*        ' SELECT A.*, RAND() AS PSEUDORAND'
        'SELECT A.*, RAND( CAST( MOD( RRN( A ), 2147483647 ) '
        'AS INTEGER ) ) AS PSEUDORAND'
        ' FROM' fulltablename 'A'
        ' ) RANDTABLE'
        ' WHERE PSEUDORAND < ' sample_ratio_char
        '%_HINTS DB4 ''SINGLE_EXECUTION'''
        INTO sql_stmt SEPARATED BY space.
    WHEN 'HDB'.
      IF db_table-record_count <  max_records.
        "number of records below max sample=> Read all records.
        CONCATENATE
         'SELECT' column_list 'FROM' fulltablename
             INTO sql_stmt SEPARATED BY space.
      ELSE.
        sample_ratio_char = sample_ratio * db_table-record_count DIV 10.
* SQL is too expensive...
*        CONCATENATE 'SELECT' column_list 'FROM ( SELECT * FROM ('
*       'SELECT *, row_number( ) OVER(  ) as rownum  from' fulltablename
*         ') where rownum IN ( select top' sample_ratio_char
*         'round(rand() *' card_char ') from' fulltablename ') )'
*         INTO sql_stmt SEPARATED BY space.
        CONCATENATE
        'SELECT TOP' sample_ratio_char column_list 'FROM' fulltablename
             INTO sql_stmt SEPARATED BY space.
      ENDIF.
    WHEN OTHERS. "ADA
      IF db_table-record_count <= 10000.
        sample_ratio_char = db_table-record_count.
      ELSE.
        sample_ratio_char = sample_ratio * db_table-record_count DIV 10.
      ENDIF.
      CONCATENATE
        'SELECT TOP' sample_ratio_char column_list 'FROM' fulltablename
           INTO sql_stmt SEPARATED BY space.
  ENDCASE.

  IF db_table-tab_type = 'TRANSP'.
    "assign the first and only table line.
    READ TABLE abap_table_refs ASSIGNING <abap_table_ref> INDEX 1.
  ENDIF.

  TRY.
      rs_ref = stmt_ref->execute_query( sql_stmt ).
      ASSIGN db_ref->* TO <wa_db>.

      LOOP AT db_table_ddfields ASSIGNING <d1field>.
        ASSIGN COMPONENT <d1field>-fieldname OF STRUCTURE <wa_db>
          TO <wa_comp>.
        GET REFERENCE OF <wa_comp> INTO ref.
        READ TABLE dbfields ASSIGNING <dbfield>
           WITH KEY name = <d1field>-fieldname.
        IF ( ( sy-dbsys(3) = 'ORA' OR
               sy-dbsys(3) = 'DB2' OR
               sy-dbsys(3) = 'DB4' OR
               sy-dbsys(3) = 'DB6' OR
               sy-dbsys(3) = 'HDB'    ) AND
             ( <dbfield>-type CS 'LOB'            )     )  OR
           ( ( sy-dbsys(3) = 'SYB'    ) AND
             ( <dbfield>-type CS 'TEXT'        OR
               <dbfield>-type CS 'IMAGE'          )     )   OR
           ( ( sy-dbsys(3) = 'MSS'    ) AND
             ( <dbfield>-type CS 'varbinary(max)' OR
               <dbfield>-type CS 'varchar(max)'   OR
               <dbfield>-type CS 'nvarchar(max)'  OR
               <dbfield>-type CS 'text'           OR
               <dbfield>-type CS 'image'          ) ) OR
          ( ( sy-dbsys(3) = 'ADA' ) AND
              ( <dbfield>-type CS 'LONG VARCHAR' OR
                <dbfield>-type CS 'LONG RAW' )
             ).
          is_lob = abap_true.
        ELSE.
          is_lob = abap_false.
        ENDIF.
        rs_ref->set_param( data_ref = ref
                           is_lob   = is_lob ).
      ENDLOOP.

*     fetch loop to fetch one record of the db table
      DO.
        rows_ret = rs_ref->next( ).

        IF rt-maxs IS NOT INITIAL.
          GET TIME FIELD rt-currtime.
          rt-lefts = rt-maxs - ( rt-currtime - rt-initime ).
          IF rt-lefts < 60. "less than 60 secs left
            rs_ref->close( ).
            IF <abap_table_ref> IS ASSIGNED.
              <abap_table_ref>-table-return_code = 98.
              <abap_table_ref>-table-error_message
                                            = 'Time out'.   "#EC NOTEXT
            ELSE. "pool or cluster tables
              LOOP AT abap_table_refs ASSIGNING <abap_table_ref>.
                <abap_table_ref>-table-return_code = 98.
                <abap_table_ref>-table-error_message
                                            = 'Time out'.   "#EC NOTEXT
              ENDLOOP.
            ENDIF.
            EXIT.
          ENDIF.
        ENDIF.

        IF rows_ret = 0
           OR records_selected > max_records
           OR  ( abap_records_selected > max_abap_records AND
                 records_selected > 10000 ).
          rs_ref->close( ).
          EXIT.
        ENDIF.

        IF db_table-tab_type = 'POOL'.
          ASSIGN COMPONENT 1 OF STRUCTURE <wa_db> TO <pool_tab>.
* component 1 of pool tables always contains tabname of abap table
* search for the corresponging abap table
          READ TABLE abap_table_refs ASSIGNING <abap_table_ref>
            WITH KEY tabname = <pool_tab>.
          IF sy-subrc = 0.
            IF <abap_table_ref>-where_tab IS INITIAL.
              "table remains in pool.
              records_selected = records_selected + 1.
              PERFORM  calc_data_line  USING     mshl
                                                 daag
                                                 lraw
                                       CHANGING  <abap_table_ref>
                                                 lt_h_columns
                                                 lt_h_hlcolumns
                                                 l_rows.
            ELSE.
              ASSIGN COMPONENT 2 OF STRUCTURE <wa_db> TO <wa_comp>.
              "assign varkey (concatenated key fields of abap table)
              ASSIGN <abap_table_ref>-ref->* TO <wa_abap>.
              ASSIGN <wa_abap>(<abap_table_ref>-keylen)
                  TO <wa_abap_key>.
              "assign key of abap table to <wa_abap_key>
              <wa_abap_key> = <wa_comp>.
              "move unstructured varkey to structured key of abap table
              "fill where clause with actual values.
              LOOP AT <abap_table_ref>-where_tab ASSIGNING <where_strg>.
                ASSIGN COMPONENT sy-tabix OF STRUCTURE <wa_abap>
                                                    TO <wa_comp>.
                <where_strg>-fieldvalue = <wa_comp>.
                REPLACE ALL OCCURRENCES OF  ''''
                    IN <where_strg>-fieldvalue WITH  ''''''.
* In case of quotes(') in the field value this needs to be escaped in
* the where clause
              ENDLOOP.
              SELECT SINGLE * FROM (<pool_tab>) CLIENT SPECIFIED INTO
                 <wa_abap> WHERE (<abap_table_ref>-where_tab).
              IF sy-subrc = 0.
* There are a few exceptions, where data is not found ( data in the
* varkey does not have correct format for type (numc without leading 0
* or blank)
                records_selected = records_selected + 1.
                PERFORM  calc_data_line  USING     mshl
                                                   daag
                                                   lraw
                                         CHANGING  <abap_table_ref>
                                                   lt_h_columns
                                                   lt_h_hlcolumns
                                                   l_rows.
              ENDIF.
            ENDIF.
          ELSE.
            records_selected = records_selected + 1.
          ENDIF.
        ELSEIF db_table-tab_type = 'CLUSTER'.
* fill where clause with the actual values of the semantical fields
* (excluding pageno) of the cluster key
          LOOP AT where_tab ASSIGNING <where_strg>.
            ASSIGN COMPONENT sy-tabix OF STRUCTURE <wa_db> TO <wa_comp>.
            <where_strg>-fieldvalue = <wa_comp>.
            REPLACE ALL OCCURRENCES OF  '''' IN
                 <where_strg>-fieldvalue WITH  ''''''.
* In case of quotes(') in the field value this needs to be escaped in
* the where clause
          ENDLOOP.
          SELECT MAX( pageno ) INTO l_rows FROM (db_table-db_table)
            CLIENT SPECIFIED WHERE (where_tab).
          records_selected = records_selected + 1.
          " pageno start with 0 -> number of records = max( pagno + 1)
          " now loop over all ABAP tables of the cluster and select data
          " with cluster key.
          LOOP AT abap_table_refs ASSIGNING <abap_table_ref>.
            ASSIGN <abap_table_ref>-ref->* TO <wa_abap>.
            SELECT * FROM (<abap_table_ref>-tabname)
              CLIENT SPECIFIED INTO <wa_abap> WHERE (where_tab).
              PERFORM  calc_data_line  USING     mshl
                                                 daag
                                                 lraw
                                       CHANGING  <abap_table_ref>
                                                 lt_h_columns
                                                 lt_h_hlcolumns
                                                 l_rows.
              abap_records_selected = abap_records_selected + 1.
            ENDSELECT.
          ENDLOOP.
        ELSEIF db_table-tab_type = 'TRANSP'.
          records_selected = records_selected + 1.
          PERFORM  calc_data_line USING     mshl
                                            daag
                                            lraw
                                  CHANGING  <abap_table_ref>
                                            lt_h_columns
                                            lt_h_hlcolumns
                                            l_rows.
        ENDIF.
      ENDDO.
    CATCH cx_sql_exception INTO sqlerr_ref.
      IF <abap_table_ref> IS ASSIGNED.
        IF sqlerr_ref->sql_code <> 0.
          <abap_table_ref>-table-return_code =  sqlerr_ref->sql_code.
        ELSEIF sqlerr_ref->internal_error <> 0.
          <abap_table_ref>-table-return_code =
                          sqlerr_ref->internal_error.
        ELSE.
          <abap_table_ref>-table-return_code = '89'.
        ENDIF.
        IF sqlerr_ref->sql_message IS NOT INITIAL.
          <abap_table_ref>-table-error_message =
                            sqlerr_ref->sql_message.
        ELSE.
          <abap_table_ref>-table-error_message
            = 'Sampling error. Check SAP Note 1917032.'.    "#EC NOTEXT
        ENDIF.
      ELSE. "in case of pool or cluster tables and initial select fails
        LOOP AT abap_table_refs ASSIGNING <abap_table_ref>.
          IF sqlerr_ref->sql_code <> 0.
            <abap_table_ref>-table-return_code =  sqlerr_ref->sql_code.
          ELSEIF sqlerr_ref->internal_error <> 0.
            <abap_table_ref>-table-return_code =
                          sqlerr_ref->internal_error.
          ELSE.
            <abap_table_ref>-table-return_code = '89'.
          ENDIF.
          IF sqlerr_ref->sql_message IS NOT INITIAL.
            <abap_table_ref>-table-error_message =
                          sqlerr_ref->sql_message.
          ELSE.
            <abap_table_ref>-table-error_message
              = 'Sampling error. Check SAP Note 1917032.'.  "#EC NOTEXT
          ENDIF.
        ENDLOOP.
      ENDIF.
    CATCH cx_sy_dynamic_osql_semantics INTO osql_err_ref.
* open sql only used for pool/cluster, therefore <abap_table_ref> must
* be assigned.
      "error code of *osql_err_ref is 0
      LOOP AT abap_table_refs ASSIGNING <abap_table_ref>.
        <abap_table_ref>-table-return_code = 4.
        <abap_table_ref>-table-error_message =
                                 osql_err_ref->kernel_errid.
      ENDLOOP.
  ENDTRY.

* calculate/correct table cardinalities
  CLEAR t_table[].
  LOOP AT abap_table_refs ASSIGNING <abap_table_ref>.
    IF <abap_table_ref>-table-record_count > 0.
      IF <abap_table_ref>-table-tab_type = 'TRANSP'.
        IF <abap_table_ref>-table-records_sampled >
                <abap_table_ref>-table-record_count.
          <abap_table_ref>-table-record_count =
                <abap_table_ref>-table-records_sampled.
        ELSEIF <abap_table_ref>-table-records_sampled = 0
             AND <abap_table_ref>-table-record_count > 0
             AND <abap_table_ref>-table-return_code = 0.
          CALL FUNCTION 'DB_EXISTS_TABLE'
            EXPORTING
              tabname = <abap_table_ref>-table-db_table
            IMPORTING
              subrc   = l_dbok.
          IF l_dbok = 0.
            ASSIGN <abap_table_ref>-ref->* TO <wa_abap>.
            SELECT SINGLE * FROM (<abap_table_ref>-table-db_table)
                 CLIENT SPECIFIED INTO <wa_abap>.
            IF <wa_abap> IS NOT INITIAL.
              <abap_table_ref>-table-return_code = 97.
              <abap_table_ref>-table-error_message
                                 = 'Sampling unsuccessful'. "#EC NOTEXT
            ELSE. "realy empty
              <abap_table_ref>-table-record_count =
                              <abap_table_ref>-table-records_sampled.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
* records_selected is the actual number of db table lines which have
* been effectively selected and could be analyzed.
* logical records_sampled is the number of logical records that have
* been analyzed for a given abap table.
* 1 logical record = N physical records.
        <abap_table_ref>-table-record_count =
        <abap_table_ref>-table-record_count *
          <abap_table_ref>-table-documents_sampled / records_selected.
      ENDIF.
    ENDIF.
    IF <abap_table_ref>-tabname <> <abap_table_ref>-table-table_name.
      "restore original ABAP table name for table which remains in pool
      <abap_table_ref>-table-table_name = <abap_table_ref>-tabname.
    ENDIF.
    APPEND <abap_table_ref>-table TO t_table.
  ENDLOOP.

* Move out of hash into lt_columns
  LOOP AT lt_h_columns ASSIGNING <h_col>.
    MOVE-CORRESPONDING <h_col> TO ls_columns. "spec_count type conv.
    APPEND ls_columns TO lt_columns.
    CLEAR ls_columns.
  ENDLOOP.
  LOOP AT lt_h_hlcolumns ASSIGNING <h_hlcol>.
    MOVE-CORRESPONDING <h_hlcol> TO ls_columns.
    APPEND ls_columns TO lt_columns.
    CLEAR ls_columns.
  ENDLOOP.

* Collect metadata.
  LOOP AT ddfields ASSIGNING <d1field>.
    READ TABLE lt_columns ASSIGNING <col>
      WITH KEY column_name = <d1field>-fieldname.
    IF sy-subrc = 0. "empty tables have no column data.
      <col>-typekind = <d1field>-inttype.
      IF <d1field>-valexi = abap_true.
        <col>-low_distinct = 2.
      ENDIF.
      IF <d1field>-datatype = 'CLNT'.
        <col>-client = abap_true.
        <col>-low_distinct = '1.1'.
      ENDIF.
    ENDIF.
  ENDLOOP.

* calculate average field length and mark column as key field
  LOOP AT lt_columns ASSIGNING <col>.
    READ TABLE abap_table_refs ASSIGNING <abap_table_ref>
      WITH KEY tabname = <col>-table_name.
    "first calc. average sizes.
    <col>-size_byte = <col>-size_byte /
                            <abap_table_ref>-table-records_sampled.
    <col>-lob_size_byte = <col>-lob_size_byte /
                            <abap_table_ref>-table-records_sampled.
    <col>-opt_size_byte = <col>-opt_size_byte /
                            <abap_table_ref>-table-records_sampled.
    <col>-dlob_size_byte = <col>-dlob_size_byte / <col>-dlob_count.
    <col>-mlob_size_byte = <col>-mlob_size_byte / <col>-mlob_count.

    "change subcounts from sample count to total estimated counts
    <col>-dlob_count = <col>-dlob_count *
                        <abap_table_ref>-table-record_count /
                        <abap_table_ref>-table-records_sampled.
    <col>-mlob_count = <col>-mlob_count *
                        <abap_table_ref>-table-record_count /
                        <abap_table_ref>-table-records_sampled.
    IF <col>-table_name = 'CKMLPRKEPH'.
      "For CKMLPRKEPH spec_count is already weighted
    ELSE.
      <col>-spec_count = <col>-spec_count *
                          <abap_table_ref>-table-record_count /
                          <abap_table_ref>-table-records_sampled.
    ENDIF.
    "flag keyfields.
    READ TABLE <abap_table_ref>-ddfields
      WITH KEY fieldname = <col>-column_name keyflag = abap_true
                        TRANSPORTING NO FIELDS.             "#EC WARNOK
    IF sy-subrc = 0.
      <col>-keyflag = abap_true.
    ENDIF.
    APPEND <col> TO e_columns.
  ENDLOOP.

ENDFORM.                    "calc_data_sample_new


*&--------------------------------------------------------------------*
*&      Form  calc_data_line
*&--------------------------------------------------------------------*
*       sampling
*---------------------------------------------------------------------*

FORM calc_data_line USING      mshl         TYPE c
                               daag         TYPE numc3
                               lraw         TYPE i
                    CHANGING   table_ref    TYPE typ_abap_table_ref
                               lt_h_columns TYPE typ_ht_columns
                               lt_h_hlcolumns TYPE typ_ht_hlcolumns
                               l_rows      TYPE i.

  FIELD-SYMBOLS:
    <ddfield>   TYPE dfies,
    <wa>        TYPE any,
    <wa_comp>   TYPE any,
    <lv_hex>    TYPE x,
    <h_columns> TYPE typ_h_columns.
  DATA:
    ls_h_columns   TYPE typ_h_columns,
    ls_h_hlcolumns TYPE typ_h_hlcolumns,
    p_str          TYPE string,
    intvalue       TYPE i,
    lv_hybridlob   TYPE boolean,
    lv_res         TYPE sy-datum,
    lv_char        TYPE char1,
    lv_edids       TYPE abap_bool,
    lv_kst         TYPE i.
  CONSTANTS:
      cv_hex      TYPE x LENGTH 2 VALUE '0020'.
  ASSIGN table_ref-ref->* TO <wa>.
  " in case of cluster we need to weight the count so that big documents
  " are not "over-represented". If not cluster then l_rows = 0.
  table_ref-table-documents_sampled =
          table_ref-table-documents_sampled + ( 1 / ( l_rows + 1 ) ).
  table_ref-table-records_sampled = table_ref-table-records_sampled + 1.
  LOOP AT table_ref-ddfields ASSIGNING <ddfield>.
    ASSIGN COMPONENT <ddfield>-position OF STRUCTURE <wa>
      TO <wa_comp>.
    CLEAR: ls_h_columns, ls_h_hlcolumns, lv_hybridlob.
    IF table_ref-intfield = <ddfield>-fieldname.
      intvalue = <wa_comp>. " keep the value.
    ENDIF.

    IF table_ref-tabname = 'COEP'
      AND <ddfield>-fieldname = 'WRTTP' AND <wa_comp> = '04'.
      ls_h_columns-spec_count = 1.
    ENDIF.

    IF table_ref-tabname = 'BALHDR' AND <ddfield>-fieldname = 'ALDATE'.
      lv_res = sy-datum - daag.
      IF <wa_comp> < lv_res.
        ls_h_columns-spec_count = 1.
      ENDIF.
    ENDIF.

    IF table_ref-tabname = 'EDIDS'.
      IF <ddfield>-fieldname = 'CREDAT'.
        lv_res = sy-datum - daag.
        IF <wa_comp> < lv_res.
          lv_edids = abap_true.
        ENDIF.
      ENDIF.
      IF <ddfield>-fieldname = 'STATUS'.
        IF <wa_comp> <> '64' AND <wa_comp> <> '66' AND <wa_comp> <> '51'
          AND <wa_comp> <> '75' AND <wa_comp> <> '69'
          AND lv_edids = abap_true.
          ls_h_columns-spec_count = 1.
        ENDIF.
        CLEAR lv_edids.
      ENDIF.
    ENDIF.

    IF table_ref-tabname = 'CDHDR'
      AND <ddfield>-fieldname = 'UDATE'.
      lv_res = sy-datum - daag.
      IF <wa_comp> < lv_res.
        ls_h_columns-spec_count = 1.
      ENDIF.
    ENDIF.

    IF table_ref-tabname = 'SWWWIHEAD'
      AND <ddfield>-fieldname = 'WI_AED'.
      lv_res = sy-datum - daag.
      IF <wa_comp> < lv_res.
        ls_h_columns-spec_count = 1.
      ENDIF.
    ENDIF.

    IF table_ref-tabname = 'VBFA' AND <ddfield>-fieldname = 'STUFE'
      AND <wa_comp> > 0.
      ls_h_columns-spec_count = 1.
    ENDIF.

    "Material ledger with actual costing (S/4)
    IF table_ref-tabname = 'MLHD' AND <ddfield>-fieldname = 'BLDAT'.
      "on average the data migrated is max 18 months old.
      lv_res = sy-datum - 550.
      IF <wa_comp> > lv_res.
        ls_h_columns-spec_count = 1.
      ENDIF.
    ENDIF.

    IF table_ref-tabname = 'CKMLPRKEPH'
      AND <ddfield>-fieldname(3) = 'KST'.
      IF <wa_comp> IS NOT INITIAL.
        ADD 1 TO lv_kst.
      ENDIF.
    ENDIF.

    CASE <ddfield>-inttype.
      WHEN cl_abap_typedescr=>typekind_date       OR
           cl_abap_typedescr=>typekind_num        OR
           cl_abap_typedescr=>typekind_numeric    OR
           cl_abap_typedescr=>typekind_time.
        ls_h_columns-size_byte = strlen( <wa_comp> ) * 2.
      WHEN 'a' " cl_abap_typedescr=>typekind_decfloat16
        OR 'e'."cl_abap_typedescr=>typekind_decfloat34.
        ls_h_columns-size_byte = <ddfield>-intlen.
      WHEN cl_abap_typedescr=>typekind_packed.
        p_str = <wa_comp>."real size of decimal without leading zeroes
        ls_h_columns-size_byte = ( strlen( p_str ) + 1 ) / 2.
      WHEN cl_abap_typedescr=>typekind_float.
        ls_h_columns-size_byte = 8.
      WHEN cl_abap_typedescr=>typekind_int.
        ls_h_columns-size_byte = 4.
      WHEN cl_abap_typedescr=>typekind_int1.
        ls_h_columns-size_byte = 1.
      WHEN cl_abap_typedescr=>typekind_int2.
        ls_h_columns-size_byte = 2.

      WHEN cl_abap_typedescr=>typekind_char.
        IF <ddfield>-datatype = 'LCHR' "LOB type C
          AND table_ref-tabname <> 'EDID4'.
          ls_h_hlcolumns-lob_size_byte = strlen( <wa_comp> ) * 2.
        ELSE.
          IF <ddfield>-intlen < 5000.
            ls_h_columns-size_byte = strlen( <wa_comp> ) * 2.
          ELSE.
            ls_h_hlcolumns-lob_size_byte = strlen( <wa_comp> ) * 2.
          ENDIF.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_string.
        ls_h_hlcolumns-lob_size_byte = strlen( <wa_comp> ) * 2.

      WHEN cl_abap_typedescr=>typekind_xstring.
        ls_h_hlcolumns-lob_size_byte = xstrlen( <wa_comp> ).

      WHEN cl_abap_typedescr=>typekind_hex.
        IF <ddfield>-datatype = 'LRAW'. "LOB type X
          IF table_ref-rawfield = <ddfield>-fieldname.
            ls_h_hlcolumns-lob_size_byte = intvalue.
          ELSE.
            "should not be happening.There is always a preceding int
            ls_h_hlcolumns-lob_size_byte = xstrlen( <wa_comp> ).
          ENDIF.
        ELSE. "hex not LRAW...
          IF <ddfield>-intlen < 7000.
            ls_h_columns-size_byte = xstrlen( <wa_comp> ).
          ELSE.
            ls_h_hlcolumns-lob_size_byte = xstrlen( <wa_comp> ).
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        ls_h_columns-size_byte = <ddfield>-intlen.
    ENDCASE.

    "check for en quad
    IF <ddfield>-inttype = cl_abap_typedescr=>typekind_char.
      IF <wa_comp>(1) = en_quad AND en_quad IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF en_quad IN <wa_comp> WITH ''.
      ENDIF.
    ENDIF.

    IF <wa_comp> IS INITIAL.
      "Initial does not mean empty but there is a very high chance that
      "the dictionary compression is excellent on initial values.
      "however we store in a different field as this is only relevant
      "for column store and non-concatenated, non-LOB fields.
      ls_h_columns-opt_size_byte = 0.
    ELSE.
      ls_h_columns-opt_size_byte = ls_h_columns-size_byte.
    ENDIF.

    CASE <ddfield>-datatype. "Find hybrid LOB candidates.
      WHEN 'STRG' OR 'RSTR'.
        lv_hybridlob = abap_true.
      WHEN 'LRAW'.
        IF <ddfield>-leng > lraw. "1000 or 5000
          lv_hybridlob = abap_true.
        ENDIF.
      WHEN 'LCHR'.
        IF <ddfield>-leng > 5000.
          lv_hybridlob = abap_true.
        ENDIF.
    ENDCASE.


    IF lv_hybridlob = abap_true."hybrid LOBs
      IF ls_h_hlcolumns-lob_size_byte > 1000.
        IF mshl = abap_true.
          "multiple records stored in one page
          ls_h_hlcolumns-dlob_size_byte = ls_h_hlcolumns-lob_size_byte.
        ELSE.
          " round up to the next multiple of 4kb.
          ls_h_hlcolumns-dlob_size_byte =
               ceil( ls_h_hlcolumns-lob_size_byte / 4096 ) * 4096.
        ENDIF.
        ls_h_hlcolumns-dlob_count = 1.
      ELSE.
        ls_h_hlcolumns-mlob_size_byte = ls_h_hlcolumns-lob_size_byte.
        ls_h_hlcolumns-mlob_count = 1.
      ENDIF.
    ENDIF.

    "2 hash tables are used for better perf. always collect even if 0.
    IF ls_h_hlcolumns-lob_size_byte > 0
      OR ls_h_hlcolumns-mlob_count > 0.
      ls_h_hlcolumns-table_name = table_ref-tabname.
      ls_h_hlcolumns-column_name = <ddfield>-fieldname.
      COLLECT ls_h_hlcolumns INTO lt_h_hlcolumns.
    ELSE.
      ls_h_columns-table_name = table_ref-tabname.
      ls_h_columns-column_name = <ddfield>-fieldname.
      COLLECT ls_h_columns INTO lt_h_columns.
    ENDIF.
  ENDLOOP.
  IF lv_kst IS NOT INITIAL.
    READ TABLE lt_h_columns ASSIGNING <h_columns>
      WITH KEY table_name = 'CKMLPRKEPH'
               column_name = 'KST001'.
    "need to weight now or we lose the information
    <h_columns>-spec_count = ( <h_columns>-spec_count
                           * ( table_ref-table-records_sampled - 1 )
                           + lv_kst )
                           / table_ref-table-records_sampled.
  ENDIF.

ENDFORM.                    "calc_data_line


*&---------------------------------------------------------------------*
*&      Form  calc_data_old
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WITH_INDEX text
*----------------------------------------------------------------------*
FORM calc_data_old TABLES it_tables TYPE typ_t_hana_db_size.
  FIELD-SYMBOLS:
    <dbstatam> TYPE dbstatam,
    <table>    TYPE typ_hana_db_size.
  DATA:
    wa_dbstatam TYPE dbstatam,
    it_dbstatam TYPE TABLE OF dbstatam.

  LOOP AT it_tables ASSIGNING <table>.
    IF <table>-db_table <> wa_dbstatam-tname.
      wa_dbstatam-tname = <table>-db_table.
      APPEND wa_dbstatam TO it_dbstatam.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'DB_STATISTICS_DATA_READ'
    EXPORTING
      tableclass          = 'I'
      activ_data          = 'A'
    TABLES
      dbstat              = it_dbstatam
    EXCEPTIONS
      input_inconsistence = 1
      no_data_found       = 2
      OTHERS              = 3.

  CASE sy-subrc.
    WHEN 0.
      LOOP AT it_dbstatam ASSIGNING <dbstatam>.
        LOOP AT it_tables ASSIGNING <table>
            WHERE db_table = <dbstatam>-tname.
          <table>-record_count = <dbstatam>-nrows.
          <table>-source_size = <dbstatam>-occtb + <dbstatam>-occix.
          <table>-return_code = 0.
        ENDLOOP.
      ENDLOOP.
    WHEN 2.
*     ignore, table does not exist or does not have statistics.
*      <table>-return_code             = -1.
    WHEN OTHERS.
      RAISE system_error. "RAISE other_error.
  ENDCASE.
ENDFORM.                    "calc_data_old

*&---------------------------------------------------------------------*
*&      Form  get_cardinality_ora
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABLE      text
*----------------------------------------------------------------------*
FORM get_cardinality_ora
          USING    db_table TYPE tabname
          CHANGING cardinality   TYPE typ_hana_db_size-record_count
                   return_code   TYPE typ_hana_db_size-return_code
                   error_message TYPE typ_hana_db_size-error_message
                   doubt_reason  TYPE typ_hana_db_size-doubt_reason
                   stat_age      TYPE typ_hana_db_size-stat_age.

  TYPES:
    BEGIN OF ty_user_table,
      num_rows         TYPE p,
      last_analysed(9) TYPE c,
    END OF ty_user_table.

  CONSTANTS:
    class_name TYPE syrepid VALUE 'CL_SDB_ORA_UPDATE_STATS'.
  DATA:
    con_ref      TYPE REF TO cl_sql_connection,
    dref         TYPE REF TO data,
    sqlerr_ref   TYPE REF TO cx_sql_exception,
    prepstmt_ref TYPE REF TO cl_sql_prepared_statement,
    res_ref      TYPE REF TO cl_sql_result_set,
    sql_stmt     TYPE string,
    updstatreq   TYPE i,
    ora_y        TYPE i,
    aba_y        TYPE i,
    wa_user_tab  TYPE ty_user_table,
    num_rows_mod TYPE p.

  TRY.
      con_ref = cl_sql_connection=>get_connection( ).

      CONCATENATE
        `SELECT count(*)  FROM user_tab_modifications `
        `WHERE table_name = ?`
        `AND ( truncated = 'YES' OR drop_segments > 0 )`
        INTO sql_stmt SEPARATED BY space.                   "#EC NOTEXT
      prepstmt_ref = con_ref->prepare_statement( sql_stmt ).

      GET REFERENCE OF db_table INTO dref.
      prepstmt_ref->set_param( dref ).
      res_ref = prepstmt_ref->execute_query( sql_stmt ).
      GET REFERENCE OF updstatreq INTO dref.
      res_ref->set_param( dref ).
      res_ref->next( ).
      res_ref->close( ).
      prepstmt_ref->close( ).

      IF updstatreq > 0.
        TRY.
            CALL METHOD (class_name)=>update_stats
              EXPORTING
                i_tablnm = db_table
                i_force  = 'X'
*               i_connection =   sys_ref->sys_data-dbcname
              .
          CATCH cx_root.                                "#EC NO_HANDLER
*            cx_sy_dyn_call_illegal_class
            "If upd fails, continue with available information
        ENDTRY.
      ELSE.
        CONCATENATE
         `SELECT sum( inserts - deletes )  FROM user_tab_modifications `
         `WHERE table_name = ?`                             "#EC NOTEXT
         INTO sql_stmt SEPARATED BY space.
        prepstmt_ref = con_ref->prepare_statement( sql_stmt ).

        GET REFERENCE OF db_table INTO dref.
        prepstmt_ref->set_param( dref ).
        res_ref = prepstmt_ref->execute_query( sql_stmt ).

        GET REFERENCE OF num_rows_mod INTO dref.
        res_ref->set_param( dref ).
        res_ref->next( ).
        res_ref->close( ).
        prepstmt_ref->close( ).
      ENDIF.

      CONCATENATE
        `SELECT num_rows, last_analyzed FROM user_tables `
        `WHERE table_name = ?`                              "#EC NOTEXT
        INTO sql_stmt SEPARATED BY space.
      prepstmt_ref = con_ref->prepare_statement( sql_stmt ).

      GET REFERENCE OF db_table INTO dref.
      prepstmt_ref->set_param( dref ).
      res_ref = prepstmt_ref->execute_query( sql_stmt ).

      GET REFERENCE OF wa_user_tab INTO dref.
      res_ref->set_param_struct( dref ).
      res_ref->next( ).
      res_ref->close( ).
      prepstmt_ref->close( ).

      wa_user_tab-num_rows = wa_user_tab-num_rows + num_rows_mod.
      IF wa_user_tab-num_rows < 0.
        doubt_reason = 1.
      ENDIF.

      ora_y = wa_user_tab-last_analysed+7(2).
      aba_y = sy-datum+2(2).
      stat_age = aba_y - ora_y.
      IF stat_age < 0. stat_age = 99. ENDIF.

      cardinality = wa_user_tab-num_rows.

    CATCH cx_sql_exception INTO sqlerr_ref.
      return_code = cx_adbc_error.
      error_message = sqlerr_ref->sql_message.
  ENDTRY.
*
ENDFORM.                    "get_cardinality_ora

*&---------------------------------------------------------------------*
*&      Form  get_cardinality_db6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->SYS_REF    text
*      -->TABLE      text
*----------------------------------------------------------------------*
FORM get_cardinality_db6
        USING       db_table TYPE tabname
        CHANGING cardinality TYPE typ_hana_db_size-record_count
                 return_code TYPE typ_hana_db_size-return_code
               error_message TYPE typ_hana_db_size-error_message.
*
  TYPES:
    BEGIN OF ty_result,
      cardinality TYPE typ_hana_db_size-record_count,
      type        TYPE char0001,
    END OF ty_result.
  DATA:
    sqlerr_ref         TYPE REF TO cx_sy_native_sql_error,
    l_curr_schema(128) TYPE c,
    l_result           TYPE ty_result.

  CALL FUNCTION 'DB_DBSCHEMA'
    IMPORTING
      dbschema = l_curr_schema.

  TRY.
      EXEC SQL.
        SELECT CARD, TYPE into :l_result FROM SYSCAT.TABLES
          WHERE TABSCHEMA = :l_curr_schema AND TABNAME = :db_table
      ENDEXEC.                                          "#EC CI_EXECSQL

      IF l_result-type = 'V' AND l_result-cardinality = '-1'.
        " Deferred views => Table is empty.
        l_result-cardinality = 0.
      ENDIF.
      cardinality = l_result-cardinality.

    CATCH cx_sy_native_sql_error INTO sqlerr_ref.
      return_code   = -1.
      error_message = sqlerr_ref->get_text( ).
  ENDTRY.
*
ENDFORM.                    "get_cardinality_db6

*&---------------------------------------------------------------------*
*&      Form  get_cardinality_hdb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->SYS_REF    text
*      -->TABLE      text
*----------------------------------------------------------------------*
FORM get_cardinality_hdb
        USING    db_table      TYPE tabname
        CHANGING cardinality   TYPE typ_hana_db_size-record_count
                 return_code   TYPE typ_hana_db_size-return_code
                 error_message TYPE typ_hana_db_size-error_message.

  DATA: l_curr_schema(128) TYPE c,
        sqlerr_ref         TYPE REF TO cx_sy_native_sql_error.

  TRY.

      CALL FUNCTION 'DB_DBSCHEMA'  " Get current schema
        IMPORTING
          dbschema = l_curr_schema.

      EXEC SQL.
        select record_count into :cardinality
        from sys.m_tables where
              schema_name = :l_curr_schema
              and table_name = :db_table
      ENDEXEC.                                          "#EC CI_EXECSQL

    CATCH cx_sy_native_sql_error INTO sqlerr_ref.
      return_code   = -1.
      error_message = sqlerr_ref->get_text( ).
  ENDTRY.
*
ENDFORM.                    "get_cardinality_hdb

*&--------------------------------------------------------------------*
*&      Form  default_sampling
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->CARDINALITYtext
*      -->PRECISION  text
*      -->SAMPLE_RATItext
*---------------------------------------------------------------------*
FORM default_sampling USING cardinality TYPE p
                            precision   TYPE c
                      CHANGING sample_ratio TYPE p.

  IF cardinality = -1.
    sample_ratio = '0.1'.
  ELSEIF cardinality < 10000.
    sample_ratio = 100.
  ELSEIF cardinality < 100000.
    sample_ratio = 50.
  ELSEIF cardinality < 1000000.                             " 1 Million
    sample_ratio = 10.
  ELSEIF cardinality < 10000000.
    sample_ratio = 1.
  ELSEIF cardinality < 100000000.
    sample_ratio = '0.1'.
  ELSEIF cardinality < 1000000000.                          "1 Millarde
    sample_ratio = '0.01'.
  ELSEIF cardinality < 10000000000.
    sample_ratio = '0.001'.
  ELSEIF cardinality < 100000000000.
    sample_ratio = '0.00001'.
  ELSEIF cardinality < 1000000000000.                       "1 Billion
    sample_ratio = '0.000001'.
  ELSEIF cardinality < 10000000000000.
    sample_ratio = '0.0000001'.
  ELSE.
    sample_ratio = '0.00000001'.
  ENDIF.

*
* standard / 'M' precision: sample between 100K and 1M records
*
  IF precision = 'H'.
    sample_ratio = sample_ratio * 10.    " samples 1M ... 10M recs
  ELSEIF precision = 'L' AND cardinality > 10000.
    "keep 100% for very small tables otherwise, 0 rows are returned.
    sample_ratio = sample_ratio / 10.    " samples 10K ... 100K recs
  ENDIF.

  IF sample_ratio > 100.
    sample_ratio = 100.
  ENDIF.

  IF sy-dbsys(3) = 'ORA' AND sample_ratio < '0.000001'.
    sample_ratio = '0.000001'.
  ENDIF.
ENDFORM.                    "default_sampling

*&---------------------------------------------------------------------*
*& FORM  determine_table_type
*& Determine the type of the infoobjects
*&
*&---------------------------------------------------------------------*

FORM determine_table_type TABLES it_tables.
  TYPES: BEGIN OF ty_bw_dso,
           odsname_tech(16) TYPE c,
           userapp(10)      TYPE c,
           version(6)       TYPE n,
         END   OF ty_bw_dso.
  DATA: lv_result(1)     TYPE c,
*        lv_infocube(30)  TYPE c,
*        lv_odsobject(30) TYPE c,
        lv_identifier(3) TYPE c,
        lv_len           TYPE i,
        lt_bw_dso        TYPE TABLE OF ty_bw_dso,
        ods_table        TYPE tabname VALUE 'rstsods'.

  FIELD-SYMBOLS: <tab> TYPE typ_hana_db_size,
                 <dso> TYPE ty_bw_dso.

  LOOP AT it_tables ASSIGNING <tab>.
    TRY.
        CALL FUNCTION 'RSD_OBJECT_IS_GENERATED'
          EXPORTING
            i_objnm            = <tab>-table_name
          IMPORTING
            e_object_generated = lv_result
            e_identifier       = lv_identifier
*           e_infocube         = lv_infocube
*           e_odsobject        = lv_odsobject
          EXCEPTIONS
            nspacegen_invalid  = 1
            OTHERS             = 2.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF lv_result IS INITIAL. CONTINUE. ENDIF.

        CASE lv_identifier.
            "Cubes
          WHEN 'E'.
            "sap.bw.cube or sap.bw.aggr with 'FACT_E'
            <tab>-nopk_type = 'X'.
          WHEN 'F'.
            "sap.bw.cube or sap.bw.aggr with 'FACT_IMO' or 'FACT_F'
            <tab>-nopk_type = 'X'.
            "DSO
          WHEN 'A'.
            "sap.bw.dso subtype 'QUEUE'
            lv_len = strlen( <tab>-table_name ) - 2.
            IF <tab>-table_name+lv_len(2) = '40'.
              <tab>-nopk_type = 'X'.
            ENDIF.
            "sap.bw.dso subtype 'ACTIVE'
            IF <tab>-table_name+lv_len(2) = '00'.
              "sap.bw.dso subtype 'ACTIVE' or 'ACTIVE_IMO'...
              " we need to find out if we have a write opti. DSO!
            ENDIF.
            "PSA typed tables
          WHEN 'B'.
            TRY.
                SELECT odsname_tech userapp version FROM (ods_table)
                  INTO TABLE lt_bw_dso
                  WHERE odsname_tech = <tab>-table_name
                    AND objstat      = 'ACT'.
              CATCH cx_root.
                EXIT. " Should not happen.
            ENDTRY.
            IF lt_bw_dso IS NOT INITIAL.
              SORT lt_bw_dso BY version DESCENDING.
              READ TABLE lt_bw_dso INDEX 1 ASSIGNING <dso>.
              " sap.bw.dtp subtype 'ERROR_STACK'
              " or sap.bw.dso subtype 'CHANGE_LOG'.
              " or sap.bw.psa subtype 'PSA'.
              IF    <dso>-userapp = 'ERROR_STACK'
                 OR <dso>-userapp = 'CHANGE_LOG'
                 OR <dso>-userapp = 'NEW_DS'
                 OR <dso>-userapp = ''.
                <tab>-nopk_type = 'X'.
              ENDIF.
            ELSE.
              "unknown PSA type table (old 3.X PSAs with empty USERAPP)
              <tab>-nopk_type = 'X'.
            ENDIF.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      CATCH cx_root.
        EXIT.  " FM does not exist. No BW compoment.
    ENDTRY.
  ENDLOOP.

ENDFORM.  "determine_table_type

*&---------------------------------------------------------------------*
*& FORM  SECURITY_CHECK
*& Check if caller is in same user/system/client
*&
*&---------------------------------------------------------------------*
FORM security_check.

  DATA l_external_call(1)  TYPE c.

  TRY.
      CALL METHOD ('CL_RFC')=>('CHECK_RFC_EXTERNAL')
        RECEIVING
          external_call    = l_external_call
        EXCEPTIONS
          kernel_too_old   = 1
          unexpected_error = 2
          OTHERS           = 3.
      IF sy-subrc = 1.
*       Kernel prerequisite not fulfilled (see note 1882417)
*       -> ok (no check in this case)
        RETURN.
      ENDIF.
    CATCH cx_sy_dyn_call_illegal_class
          cx_sy_dyn_call_illegal_method.
*     Class or method does not exist. ABAP prerequisite not fulfilled *
*     (see note 1882417)  -> ok (no check in this case)
      RETURN.
  ENDTRY.

  IF l_external_call = abap_true.
*   Call was performed via external RFC (other system, client or user)
*   -> ERROR
    MESSAGE x000(00).                                       "#EC *
  ELSE.
*   Call was performed via internal RFC
    RETURN.
  ENDIF.


ENDFORM. "check_authorization

*&---------------------------------------------------------------------*
*& FORM  READ_REAL_SIZES
*& Read real HDB size
*&
*&---------------------------------------------------------------------*
FORM read_real_sizes TABLES lt_tables TYPE typ_t_hana_db_size
                            lt_realrsindex
                            lt_realrs TYPE typ_t_realrs
                            lt_realcs TYPE typ_t_realcs
                     USING  l_load    TYPE abap_bool
                            l_read_hl TYPE abap_bool
                            l_mac     TYPE abap_bool
                            l_daag    TYPE numc3
                            l_empt    TYPE boolean.

  TYPES:
    BEGIN OF typ_store,
      table_name      TYPE tabname,
      is_column_table TYPE c LENGTH 5,
      record_count    TYPE dec20,
    END OF typ_store,
    BEGIN OF typ_loaded,
      table_name     TYPE tabname,
      part_id        TYPE i,
      loaded(16)     TYPE c,
      record_count   TYPE dec20,
      delta_count    TYPE dec20,
      comp_rec_count TYPE dec20,
    END OF typ_loaded,
    BEGIN OF typ_parts,
      table_name TYPE tabname,
      parts      TYPE i,
    END OF typ_parts,
    typ_t_store TYPE STANDARD TABLE OF typ_store,
    BEGIN OF typ_hl,
      table_name  TYPE tabname,
      part_id     TYPE i,
      column_name TYPE ty_cs_size-column_name,
      ms_hl       TYPE dec20,
    END OF typ_hl,
    typ_t_hl TYPE STANDARD TABLE OF typ_hl,
    BEGIN OF typ_uncomp,
      table_name  TYPE tabname,
      column_name TYPE ty_cs_size-column_name,
    END OF typ_uncomp.

  CONSTANTS: c_coep      TYPE tabname VALUE 'COEP',
             c_coep_ori  TYPE tabname VALUE 'V_COEP_ORI',
             c_balhdr    TYPE tabname VALUE 'BALHDR',
             c_edids     TYPE tabname VALUE 'EDIDS',
             c_cdhdr     TYPE tabname VALUE 'CDHDR',
             c_swwwihead TYPE tabname VALUE 'SWWWIHEAD',
             c_vbfa      TYPE tabname VALUE 'VBFA',
             c_mlhd      TYPE tabname VALUE 'MLHD',
             c_min_rec   TYPE i VALUE '10000000'.

  DATA: r_connection       TYPE REF TO cl_sql_connection,
        sql                TYPE REF TO cl_sql_statement,
        sql_error          TYPE REF TO cx_sql_exception,
        all_tables         TYPE TABLE OF tabname,
        all_ne_tab         TYPE TABLE OF tabname,
        cs_tables          TYPE TABLE OF tabname,
        rs_tables          TYPE TABLE OF tabname,
        rs_indexes         TYPE TABLE OF tabname,
        loaded_tables      TYPE TABLE OF typ_loaded,
        part_tables        TYPE TABLE OF typ_parts,
        l_store            TYPE typ_t_store,
        l_hl               TYPE TABLE OF tabname,
        l_realy_hl         TYPE typ_t_hl,
        l_curr_schema(128) TYPE c,
        stmt_p             TYPE string,
        stmt_s             TYPE string,
        ddlstmt            TYPE string,
        lv_res             TYPE sy-datum,
        viewstate          TYPE ddgotstate,
        last_table         TYPE tabname,
        unload_ko          TYPE abap_bool,
        changed_percent    TYPE i,
        l_part_id          TYPE string,
        lt_uncomp          TYPE TABLE OF typ_uncomp,
        ls_uncomp          TYPE typ_uncomp,
        hlview             TYPE i,
        pmcol              TYPE i.

  FIELD-SYMBOLS: <item>   TYPE typ_hana_db_size,
                 <store>  TYPE typ_store,
                 <ddl>    TYPE typ_loaded,
                 <loadt>  TYPE typ_loaded,
                 <part>   TYPE typ_parts,
                 <realrs> TYPE typ_realrs,
                 <realcs> TYPE typ_realcs,
                 <hl>     TYPE typ_hl,
                 <comp>   TYPE typ_uncomp.

  TRY.
      r_connection = cl_sql_connection=>get_connection( ).
      sql = r_connection->create_statement( ).
    CATCH cx_sql_exception.
      RAISE system_error.
  ENDTRY.

  LOOP AT lt_tables ASSIGNING <item>.
    CLEAR <item>-record_count.
    "the clear is relevant for cal where <item>-record_count is not
    "empty.
    APPEND <item>-table_name TO all_tables.
  ENDLOOP.

*Check if installed HANA supports persistent memory
  EXEC SQL.
    SELECT COUNT(*) INTO :pmcol
      FROM SYS.M_MONITOR_COLUMNS
      WHERE VIEW_NAME = 'M_CS_ALL_COLUMNS'
      AND VIEW_COLUMN_NAME = 'PERSISTENT_MEMORY_SIZE_IN_TOTAL'
  ENDEXEC.                                              "#EC CI_EXECSQL

  CALL FUNCTION 'DB_DBSCHEMA'  " Get current schema
    IMPORTING
      dbschema = l_curr_schema.

  CONCATENATE 'select table_name, is_column_table, record_count'
              'from sys.m_tables'
              'where schema_name = ? and table_name IN'     "#EC NOTEXT
              INTO stmt_p SEPARATED BY space.

  PERFORM adbc_call   TABLES all_tables
                             l_store
                       USING l_curr_schema
                             sql
                    CHANGING stmt_p
                             stmt_s.

  LOOP AT l_store ASSIGNING <store>.
    IF <store>-is_column_table = 'TRUE'.
      APPEND <store>-table_name TO cs_tables.
    ELSE.
      APPEND <store>-table_name TO rs_tables.
    ENDIF.
    "update records
    IF <store>-record_count > 0.
      READ TABLE lt_tables ASSIGNING <item>
        WITH KEY table_name = <store>-table_name.
      <item>-record_count = <store>-record_count.
      APPEND <store>-table_name TO all_ne_tab.
    ENDIF.
  ENDLOOP.

  IF rs_tables IS NOT INITIAL.

    CONCATENATE 'SELECT TABLE_NAME, RECORD_COUNT,'
    'ALLOCATED_VARIABLE_PART_SIZE AS VARIABLE_SIZE,'
    'ALLOCATED_FIXED_PART_SIZE AS FIXED_SIZE, 0 AS MS_HL'
    'FROM M_RS_TABLES WHERE schema_name = ? AND'
    'record_count > 0 AND table_name IN'                    "#EC NOTEXT
    INTO stmt_p SEPARATED BY space.

    PERFORM adbc_call   TABLES rs_tables
                               lt_realrs
                         USING l_curr_schema
                               sql
                      CHANGING stmt_p
                               stmt_s.
    LOOP AT lt_realrs ASSIGNING <realrs>.
      READ TABLE lt_tables ASSIGNING <item>
                      WITH KEY table_name = <realrs>-table_name.
      <item>-record_count = <realrs>-record_count.
    ENDLOOP.

    CONCATENATE 'select table_name, index_name '
     ', index_size from M_RS_INDEXES where schema_name'
     ' = ? and table_name IN'                               "#EC NOTEXT
     INTO stmt_p SEPARATED BY space.

    LOOP AT lt_realrs ASSIGNING <realrs>.
      "only tables with records.
      APPEND <realrs>-table_name TO rs_indexes.
    ENDLOOP.

    PERFORM adbc_call   TABLES rs_indexes
                               lt_realrsindex
                         USING l_curr_schema
                               sql
                      CHANGING stmt_p
                               stmt_s.
  ENDIF.

  IF cs_tables IS NOT INITIAL.
    CONCATENATE 'SELECT TABLE_NAME, PART_ID, LOADED,'
    'RECORD_COUNT, RAW_RECORD_COUNT_IN_DELTA AS DELTA_COUNT'
    ', LAST_COMPRESSED_RECORD_COUNT AS COMP_REC_COUNT'
    'FROM M_CS_TABLES WHERE schema_name = ? AND record_count > 0'
    'AND table_name IN'                                     "#EC NOTEXT
    INTO stmt_p SEPARATED BY space.
    stmt_s = 'ORDER BY TABLE_NAME, PART_ID'.                "#EC NOTEXT
    PERFORM adbc_call   TABLES cs_tables
                               loaded_tables "only non empty
                         USING l_curr_schema
                               sql
                      CHANGING stmt_p
                               stmt_s.
    IF l_load = abap_true.
      LOOP AT loaded_tables ASSIGNING <ddl> WHERE loaded <> 'FULL'.
        IF <ddl>-table_name <> last_table.
          last_table = <ddl>-table_name.
          TRY.
              CONCATENATE 'load "' <ddl>-table_name         "#EC NOTEXT
                          '" all' INTO ddlstmt.             "#EC NOTEXT
              sql->execute_ddl( ddlstmt ).
            CATCH cx_sql_exception INTO sql_error.
              READ TABLE lt_tables ASSIGNING <item>
               WITH KEY table_name = <ddl>-table_name.
              <item>-return_code = 10.
              <item>-error_message = sql_error->sql_message.
          ENDTRY.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CONCATENATE 'SELECT TABLE_NAME, COUNT(*) AS PARTS'
                'FROM M_CS_TABLES WHERE schema_name = ?'
                'AND table_name IN'                         "#EC NOTEXT
                INTO stmt_p SEPARATED BY space.
    stmt_s = 'GROUP BY TABLE_NAME'.                         "#EC NOTEXT
    PERFORM adbc_call   TABLES cs_tables
                               part_tables
                         USING l_curr_schema
                               sql
                      CHANGING stmt_p
                               stmt_s.

    "update partitions count.
    LOOP AT part_tables ASSIGNING <part>.
      READ TABLE lt_tables ASSIGNING <item>
            WITH KEY table_name = <part>-table_name.
      <item>-part_id      = <part>-parts.
    ENDLOOP.

    IF l_mac = abap_true.
      "Merge large tables with large deltas
      LOOP AT loaded_tables ASSIGNING <ddl>
        WHERE record_count > c_min_rec.
        changed_percent = <ddl>-delta_count / <ddl>-record_count * 100.
        IF changed_percent > 5.
          TRY.
              l_part_id = <ddl>-part_id.
              CONCATENATE
                 'MERGE DELTA OF "' <ddl>-table_name        "#EC NOTEXT
                        '" PART' l_part_id INTO ddlstmt.    "#EC NOTEXT
              sql->execute_ddl( ddlstmt ).
            CATCH cx_sql_exception INTO sql_error.
              READ TABLE lt_tables ASSIGNING <item>
               WITH KEY table_name = <ddl>-table_name.
              <item>-return_code = 10.
              <item>-error_message = sql_error->sql_message.
          ENDTRY.
        ENDIF.
      ENDLOOP.
      "compress tables where a low distinct column is uncompressed.
      "compress large tables never compressed.
      CONCATENATE 'SELECT DISTINCT TABLE_NAME,'
                  ''''' AS COLUMN_NAME'
                  'FROM M_CS_COLUMNS WHERE schema_name = ?'
                  'AND COUNT > 10000000'
                  'AND COMPRESSION_TYPE = ''DEFAULT'''
                  'AND DISTINCT_COUNT <= COUNT * 0.05'
                  'AND MEMORY_SIZE_IN_TOTAL >= 500 * 1024 * 1024'
                  'AND table_name IN'                       "#EC NOTEXT
                  INTO stmt_p SEPARATED BY space.

      PERFORM adbc_call TABLES cs_tables
                               lt_uncomp
                         USING l_curr_schema
                               sql
                      CHANGING stmt_p
                               stmt_s.

      LOOP AT loaded_tables ASSIGNING <ddl> WHERE comp_rec_count = 0
                                             AND record_count > c_min_rec.
        ls_uncomp-table_name = <ddl>-table_name.
        APPEND ls_uncomp TO lt_uncomp.
      ENDLOOP.
      SORT lt_uncomp BY table_name.
      DELETE ADJACENT DUPLICATES FROM lt_uncomp COMPARING table_name.

      LOOP AT lt_uncomp ASSIGNING <comp>.
        TRY.
            CONCATENATE 'UPDATE "' <ddl>-table_name
                        '" WITH PARAMETERS ('''
                        'OPTIMIZE_COMPRESSION''=''FORCE'')'
                        INTO ddlstmt.                       "#EC NOTEXT
            sql->execute_ddl( ddlstmt ).
          CATCH cx_sql_exception INTO sql_error.
            READ TABLE lt_tables ASSIGNING <item>
             WITH KEY table_name = <ddl>-table_name.
            <item>-return_code = 10.
            <item>-error_message = sql_error->sql_message.
        ENDTRY.
      ENDLOOP.

    ENDIF.

    "PAGE_LOADABLE_MAIN is included in MEMORY_SIZE_IN_MAIN.
    "if a column uses page Attributes, the total maximum size cannot
    "be read by the report
    "Page attributes are not loaded to persistent memory

    CONCATENATE 'SELECT a.table_name, a.column_name, part_id,'
    'memory_size_in_total AS ms_total, '
    INTO stmt_p SEPARATED BY space.                         "#EC NOTEXT
    IF pmcol = 1.
      CONCATENATE stmt_p
        'memory_size_in_main+persistent_memory_size_in_total' "#EC NOTEXT
        'AS ms_main, ' INTO stmt_p SEPARATED BY space.
    ELSE.
      CONCATENATE stmt_p 'memory_size_in_main AS ms_main, '
        INTO stmt_p SEPARATED BY space.                     "#EC NOTEXT
    ENDIF.
    CONCATENATE stmt_p 'count AS record_count, distinct_count,'
    'loaded, index_loaded, a.compression_type, b.cs_data_type_name,'
    'b.load_unit,'
    'main_memory_size_in_dict AS ms_dict, main_memory_size_in_index'
    'AS ms_index, main_memory_size_in_data AS ms_data,'
    'main_memory_size_in_misc AS ms_misc, 0 as ms_hl, 0 as spec_count '
    'FROM sys.m_cs_all_columns as a LEFT JOIN public.table_columns as b'
    'ON a.schema_name = b.schema_name AND a.table_name = b.table_name'
    'AND a.column_name = b.column_name'
    'WHERE a.schema_name = ? '                              "#EC NOTEXT
    INTO stmt_p SEPARATED BY space.
    IF l_empt = abap_true.
      CONCATENATE stmt_p 'AND a.table_name IN'              "#EC NOTEXT
        INTO stmt_p SEPARATED BY space.
    ELSE.
      CONCATENATE stmt_p 'AND count > 0 AND a.table_name IN' "#EC NOTEXT
        INTO stmt_p SEPARATED BY space.
    ENDIF.
    stmt_s = 'ORDER BY table_name, column_name'.            "#EC NOTEXT

    PERFORM adbc_call   TABLES cs_tables
                               lt_realcs
                         USING l_curr_schema
                               sql
                      CHANGING stmt_p
                               stmt_s.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_coep column_name = 'WRTTP'
             BINARY SEARCH.
    IF sy-subrc = 0 AND <realcs>-load_unit <> 'PAGE'.
      CALL FUNCTION 'DDIF_VIEW_GET'
        EXPORTING
          name          = c_coep_ori
        IMPORTING
          gotstate      = viewstate
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      IF sy-subrc <> 0 OR viewstate IS INITIAL.
        SELECT COUNT(*) FROM (c_coep) CLIENT SPECIFIED
          INTO <realcs>-spec_count WHERE wrttp = '04'.
      ELSE.
        SELECT COUNT(*) FROM (c_coep_ori) CLIENT SPECIFIED
          INTO <realcs>-spec_count WHERE wrttp = '04'.
      ENDIF.
    ENDIF.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_balhdr column_name = 'ALDATE'
             BINARY SEARCH.
    IF sy-subrc = 0 AND <realcs>-load_unit <> 'PAGE'.
      lv_res = sy-datum - l_daag.
      SELECT COUNT(*) FROM (c_balhdr) CLIENT SPECIFIED
        INTO <realcs>-spec_count WHERE aldate < lv_res.
    ENDIF.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_edids column_name = 'STATUS'
             BINARY SEARCH.
    IF sy-subrc = 0 AND <realcs>-load_unit <> 'PAGE'.
      lv_res = sy-datum - l_daag.
      SELECT COUNT(*) FROM (c_edids) CLIENT SPECIFIED
        INTO <realcs>-spec_count
        WHERE credat < lv_res AND status <> '64' AND status <> '66'
          AND status <> '51' AND status <> '75' AND status <> '69'.
    ENDIF.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_cdhdr column_name = 'UDATE'
             BINARY SEARCH.
    IF sy-subrc = 0 AND <realcs>-load_unit <> 'PAGE'.
      lv_res = sy-datum - l_daag.
      SELECT COUNT(*) FROM (c_cdhdr) CLIENT SPECIFIED
        INTO <realcs>-spec_count WHERE udate < lv_res.
    ENDIF.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_swwwihead column_name = 'WI_AED'
             BINARY SEARCH.
    IF sy-subrc = 0 AND <realcs>-load_unit <> 'PAGE'.
      lv_res = sy-datum - l_daag.
      SELECT COUNT(*) FROM (c_swwwihead) CLIENT SPECIFIED
        INTO <realcs>-spec_count WHERE wi_aed < lv_res.
    ENDIF.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_vbfa column_name = 'STUFE'
             BINARY SEARCH.
    IF sy-subrc = 0.
      SELECT COUNT(*) FROM (c_vbfa) CLIENT SPECIFIED
        INTO <realcs>-spec_count WHERE stufe > 0.
    ENDIF.

    READ TABLE lt_realcs ASSIGNING <realcs>
             WITH KEY table_name = c_mlhd column_name = 'BLDAT'
             BINARY SEARCH.
    IF sy-subrc = 0.
      lv_res = sy-datum - 550. "18 months

*---> S4 Migration - 20/07/2023 - DG
*      SELECT COUNT(*) FROM (c_mlhd) CLIENT SPECIFIED
*        INTO <realcs>-spec_count WHERE bldat > lv_res.

      DATA: t_mlhd         TYPE STANDARD TABLE OF mlhd.
      DATA: l_awsys_client LIKE mlhd-awsys,
            l_belnr_init   TYPE ck_belnr,
            l_mes(3)       type n.

      CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
        IMPORTING
          own_logical_system             = l_awsys_client
        EXCEPTIONS
          own_logical_system_not_defined = 1.
      IF sy-subrc <> 0.
        CLEAR: l_awsys_client.
      ENDIF.

      l_mes           = lv_res+4(2).
      DATA(l_JAHRPER) =  |{ lv_res(4) }| & |{ l_mes }|.


      SELECT DISTINCT belnr kjahr vgart usnam tcode glvor FROM mldoc INTO CORRESPONDING FIELDS OF TABLE t_mlhd
             WHERE  JAHRPER > l_JAHRPER
             AND    belnr NE l_belnr_init
             AND   ( awtyp = 'PRCHG' OR vgart <> 'PC' )
             AND    ( awsys = l_awsys_client OR awsys = space ).

      SELECT DISTINCT belnr kjahr vgart usnam tcode glvor FROM mlhd APPENDING CORRESPONDING FIELDS OF TABLE t_mlhd "#EC CI_DB_OPERATION_OK[2354768]
          WHERE  bldat > lv_res                                                                                    "#EC CI_DB_OPERATION_OK[2332591]
          AND    belnr NE l_belnr_init
          AND    awtyp <> 'PRCHG' AND vgart = 'PC'
          AND    ( awsys = l_awsys_client OR awsys = space ).

      <realcs>-spec_count = lines( t_mlhd ).

*<--- S4 Migration - 20/07/2023 - DG
    ENDIF.

    IF l_load = abap_true.
      CLEAR last_table.
      SORT loaded_tables BY table_name loaded.
      DELETE ADJACENT DUPLICATES FROM loaded_tables
         COMPARING table_name loaded.
      LOOP AT loaded_tables ASSIGNING <ddl>.
        CLEAR unload_ko.
        IF <ddl>-loaded = 'NO' AND <ddl>-table_name <> last_table.
          last_table = <ddl>-table_name.
          LOOP AT loaded_tables ASSIGNING <loadt>
            WHERE table_name = <ddl>-table_name AND loaded <> 'NO'.
            unload_ko = abap_true.
            EXIT. "if one partition was loaded, do not unload any
          ENDLOOP.
          IF unload_ko = abap_false.
            TRY.
                CONCATENATE 'UNLOAD "' <ddl>-table_name '"' "#EC NOTEXT
                        INTO ddlstmt.
                sql->execute_ddl( ddlstmt ).
              CATCH cx_sql_exception INTO sql_error.
                READ TABLE lt_tables ASSIGNING <item>
                     WITH KEY table_name = <ddl>-table_name.
                <item>-return_code = 10.
                <item>-error_message = sql_error->sql_message.
            ENDTRY.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF l_read_hl = abap_true.
    CONCATENATE 'SELECT TABLE_NAME FROM PUBLIC.TABLE_COLUMNS WHERE'
    'CS_DATA_TYPE_NAME = ''LOB'' and MEMORY_THRESHOLD > 0'
    'AND schema_name = ? AND table_name IN'                 "#EC NOTEXT
     INTO stmt_p SEPARATED BY space.

    PERFORM adbc_call  TABLES all_ne_tab
                              l_hl
                        USING l_curr_schema
                              sql
                     CHANGING stmt_p
                              stmt_s.

    IF l_hl IS NOT INITIAL.
      EXEC SQL.
        SELECT COUNT(*) INTO :hlview
          FROM SYS.M_MONITORS
          WHERE VIEW_NAME = 'M_TABLE_LOB_STATISTICS'
      ENDEXEC.                                          "#EC CI_EXECSQL

      IF hlview = 1.
        CONCATENATE 'select table_name, part_id, column_name,'
          'SUM(to_bigint(DISK_SIZE)) as ms_hl '
          'from m_table_lob_statistics'
          'where schema_name = ? AND table_name IN'         "#EC NOTEXT
            INTO stmt_p SEPARATED BY space.
      ELSE.
        CONCATENATE 'select table_name, part_id, column_name,'
          'SUM(to_bigint(PHYSICAL_SIZE)) as ms_hl'
          'from m_table_lob_files'
          'where schema_name = ? AND table_name IN'         "#EC NOTEXT
            INTO stmt_p SEPARATED BY space.
      ENDIF.
      CONCATENATE 'group by table_name, part_id, column_name'
                'ORDER BY table_name, part_id, column_name' "#EC NOTEXT
                INTO stmt_s SEPARATED BY space.

      SORT l_hl.
      DELETE ADJACENT DUPLICATES FROM l_hl.

      PERFORM adbc_call   TABLES l_hl
                                 l_realy_hl
                           USING l_curr_schema
                                 sql
                        CHANGING stmt_p
                                 stmt_s.
      LOOP AT l_realy_hl ASSIGNING <hl>.
        READ TABLE lt_realcs  ASSIGNING <realcs> WITH KEY
           table_name  = <hl>-table_name
           column_name = <hl>-column_name
                           part_id     = <hl>-part_id.
        IF sy-subrc = 0.
          <realcs>-ms_hl = <hl>-ms_hl.
        ELSE.
          READ TABLE lt_realrs ASSIGNING <realrs> WITH KEY
            table_name = <hl>-table_name.
          IF sy-subrc = 0.
            <realrs>-ms_hl = <hl>-ms_hl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    "read_real_sizes

*&---------------------------------------------------------------------*
*&      Form  ADBC_CALL
*&---------------------------------------------------------------------*
*       do the native SQL
*----------------------------------------------------------------------*

FORM adbc_call TABLES table_list
                      into_table
                USING l_curr_schema
                      sql TYPE REF TO cl_sql_statement
             CHANGING stmt_p
                      stmt_s.

  DATA: res_ref   TYPE REF TO cl_sql_result_set,
        sql_error TYPE REF TO cx_sql_exception,
        data_ref  TYPE REF TO data,
        rows_ret  TYPE i,
        eow       TYPE boolean,
        next_in   TYPE sy-tabix,
        stmt      TYPE string,
        in_list   TYPE string.

  TRY.
      DO.
        CLEAR: in_list, stmt.
        PERFORM build_in_list TABLES table_list
                           CHANGING next_in
                                    in_list
                                    eow.
        IF in_list IS INITIAL.
          EXIT.
        ENDIF.
        CONCATENATE stmt_p '(' in_list ')' INTO stmt SEPARATED BY space.
        IF stmt_s IS NOT INITIAL.
          CONCATENATE stmt stmt_s INTO stmt SEPARATED BY space.
        ENDIF.
        GET REFERENCE OF l_curr_schema INTO data_ref.
        sql->set_param( data_ref ).
        res_ref = sql->execute_query( stmt ).
        GET REFERENCE OF into_table[] INTO data_ref.
        res_ref->set_param_table( data_ref ).
        rows_ret = res_ref->next_package( ).
        res_ref->close( ).
        IF eow = abap_true.
          EXIT.
        ENDIF.
      ENDDO.
    CATCH cx_sql_exception INTO sql_error.
      RAISE system_error.
  ENDTRY.
  CLEAR: stmt_p, stmt_s.
ENDFORM.                    " ADBC_CALL

*&---------------------------------------------------------------------*
*&      Form  BUILD_IN_LIST
*&---------------------------------------------------------------------*
*       build in list for adbc
*----------------------------------------------------------------------*

FORM build_in_list    TABLES table_list
                    CHANGING next_in TYPE sy-tabix
                             in_list TYPE string
                             eow TYPE boolean.
  DATA: curr_in_cnt TYPE i,
        max_in      TYPE i VALUE 100.
  FIELD-SYMBOLS: <table> TYPE tabname.

  LOOP AT table_list ASSIGNING <table> FROM next_in.    "#EC CI_NOORDER
    curr_in_cnt = curr_in_cnt + 1.
    IF in_list IS INITIAL.
      CONCATENATE '''' <table> '''' INTO in_list.
      CONTINUE.
    ENDIF.
    CONCATENATE in_list ' , ''' <table> '''' INTO in_list.
    IF curr_in_cnt = max_in.
      next_in = sy-tabix + 1.
      RETURN.
    ENDIF.
  ENDLOOP.
  eow = abap_true.
ENDFORM.                    " BUILD_IN_LIST

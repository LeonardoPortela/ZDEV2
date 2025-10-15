FUNCTION z_analise_logs_table.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CUSOBJ) TYPE  OBJS-OBJECTNAME
*"     REFERENCE(OBJFIRST) DEFAULT SPACE
*"     REFERENCE(TABFIRST) DEFAULT 'X'
*"     REFERENCE(ALV_GRID) DEFAULT 'X'
*"     REFERENCE(IGN_UNCH) DEFAULT ' '
*"     REFERENCE(ADDTBLGS) DEFAULT ' '
*"     REFERENCE(ESPECIFICO) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(MATNR) TYPE  MATNR OPTIONAL
*"----------------------------------------------------------------------

  DATA: lit_fields    TYPE TABLE OF sval WITH HEADER LINE,
        lva_date      TYPE sy-datum,
        lva_return    TYPE vbpok-charg,
        lva_date_from TYPE sy-datum,
        lva_date_to   TYPE sy-datum,
        lva_time_from TYPE sy-uzeit,
        lva_time_to   TYPE sy-uzeit.

*-US192246-06.10.2025-#1922468-JT-inicio
  TYPES: BEGIN OF ty_zdt0210,
           tlog_logdate     TYPE sy-datum,
           tlog_username    TYPE sy-uname,
           tlog_logtime     TYPE sy-uzeit,
           tcode            TYPE sy-tcode,
           img_activity     TYPE char20,
           project_id       TYPE char20,
           progname         TYPE char40,
           tlog_optype      TYPE char01,
           tlog_optype_text TYPE char20,
           matnr            TYPE matnr,
           id_matnr_idea    TYPE int4,
           id_matnr_agriq   TYPE char32.
  TYPES: END OF ty_zdt0210.

  DATA: t_zsdt0210 TYPE TABLE OF ty_zdt0210,
        w_zsdt0210 TYPE ty_zdt0210,
        obj_data   TYPE REF TO data.

  FIELD-SYMBOLS <fs_alv_data> TYPE ANY TABLE.
*-US192246-06.10.2025-#1922468-JT-fim

  lva_date = sy-datum .

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lva_date
      days      = 0
      months    = 1
      signum    = '-'    " Signum : "+" or "-" to add or remove
      years     = 0
    IMPORTING
      calc_date = lva_date.

  lit_fields-tabname = 'ZLEST0103'.
  lit_fields-fieldname = 'DT_CAD'.
  lit_fields-fieldtext = 'Data Inicio'.
  lit_fields-value = lva_date.
  APPEND lit_fields.

  lit_fields-tabname = 'ZLEST0103'.
  lit_fields-fieldname = 'HR_CAD'.
  lit_fields-fieldtext = 'Hora Inicio'.
  lit_fields-value = sy-uzeit.
  APPEND lit_fields.

  lit_fields-tabname = 'BKPF'.
  lit_fields-fieldname = 'BUDAT'.
  lit_fields-fieldtext = 'Data Fim'.
  lit_fields-value = sy-datum.
  APPEND lit_fields.

  lit_fields-tabname = 'SYST'.
  lit_fields-fieldname = 'UZEIT'.
  lit_fields-fieldtext = 'Hora Fim'.
  lit_fields-value = sy-uzeit.
  APPEND lit_fields.


  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Selecione Período de Análise'
    IMPORTING
      returncode      = lva_return
    TABLES
      fields          = lit_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF lva_return <> 'A' AND sy-subrc = 0.

    READ TABLE lit_fields  INDEX 1.
    lva_date_from = lit_fields-value.

    READ TABLE lit_fields  INDEX 2.
    lva_time_from = lit_fields-value.

    READ TABLE lit_fields  INDEX 3.
    lva_date_to = lit_fields-value.

    READ TABLE lit_fields  INDEX 4.
    lva_time_to = lit_fields-value.

*-US192246-06.10.2025-#1922468-JT-inicio
    IF especifico = abap_true.
      cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                              metadata = abap_true
                                              data     = abap_true ).
    ENDIF.
*-US192246-06.10.2025-#1922468-JT-fim

    SUBMIT rsvtprot
      WITH cusobj EQ  cusobj "'ZFIT0033'
      WITH objfirst EQ objfirst
      WITH tabfirst EQ tabfirst
      WITH alv_grid EQ alv_grid
      WITH ign_unch EQ ign_unch
      WITH addtblgs EQ addtblgs
      WITH dbeg     EQ lva_date_from
      WITH dend     EQ lva_date_to
      WITH tbeg     EQ lva_time_from
      WITH tend     EQ lva_time_to
       AND RETURN.

*-US192246-06.10.2025-#1922468-JT-inicio
    CHECK especifico = abap_true.

    FREE: it_fieldcat.

    TRY.
        CALL METHOD cl_salv_bs_runtime_info=>get_data_ref
          IMPORTING
            r_data = obj_data.
      CATCH cx_salv_bs_sc_runtime_info.
    ENDTRY.

    IF obj_data IS BOUND.
      ASSIGN obj_data->* TO <fs_alv_data>.
    ENDIF.

    IF cusobj = 'ZSDT0210' AND <fs_alv_data> IS ASSIGNED.
      t_zsdt0210[] = <fs_alv_data>.

      IF matnr IS NOT INITIAL.
        DELETE t_zsdt0210 WHERE matnr <> matnr.
      ENDIF.

      IF t_zsdt0210[] IS INITIAL.
        MESSAGE s024(sd) WITH 'Não há Log de Modificações a Exibir.'.
        RETURN.
      ENDIF.

      PERFORM f_preenche_fcat USING :
       '01' 'ZSDT0210'      'MATNR'            'T_ZSDT0210'  'MATNR'            'Material SAP'        '20'     ''    ''     ''    '' '' '',
       '02' 'DBTABLOG'      'TCODE'            'T_ZSDT0210'  'TCODE'            'Transação'           '10'     ''    ''     ''    '' '' '',
       '03' 'DBTABLOG'      'TLOG_OPTYPE_TEXT' 'T_ZSDT0210'  'TLOG_OPTYPE_TEXT' 'Modificação'         '12'     ''    ''     ''    '' '' '',
       '04' 'ZSDT0210'      'ID_MATNR_IDEA'    'T_ZSDT0210'  'ID_MATNR_IDEA'    'Id.Cód.Indea'        '15'     ''    ''     ''    '' '' '',
       '05' 'ZSDT0210'      'ID_MATNR_AGRIQ'   'T_ZSDT0210'  'ID_MATNR_AGRIQ'   'Id.Cód.Agriq'        '20'     ''    ''     ''    '' '' '',
       '06' 'TLOG_HEADER'   'TLOG_LOGDATE'     'T_ZSDT0210'  'TLOG_LOGDATE'     'Data'                '13'     ''    ''     ''    '' '' '',
       '07' 'TLOG_HEADER'   'TLOG_LOGTIME'     'T_ZSDT0210'  'TLOG_LOGTIME'     'Hora'                '10'     ''    ''     ''    '' '' '',
       '08' 'TLOG_HEADER'   'TLOG_USERNAME'    'T_ZSDT0210'  'TLOG_USERNAME'    'Nome usuário'        '12'     ''    ''     ''    '' '' ''.
*      '09' 'DBTABLOG'      'PROGNAME'         'T_ZSDT0210'  'PROGNAME'         'Programa'            '10'     ''    ''     ''    '' '' ''.
    ENDIF.

    ls_variant-report = sy-repid && 'XXX'.
    l_grid_title      = 'Log De/Para Material SAP x Produto Indea x Produto AgriQ'.

    SORT t_zsdt0210  BY tlog_logdate DESCENDING
                        tlog_logtime DESCENDING.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_true
                                            metadata = abap_true
                                            data     = abap_true ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        it_fieldcat           = it_fieldcat[]
*       it_sort               = t_sort[]
*       i_callback_user_command = 'USER_COMMAND_COMPRO'
        i_grid_title          = l_grid_title
        i_save                = 'X'
        is_variant            = ls_variant
        i_screen_start_column = 25
        i_screen_start_line   = 04
        i_screen_end_column   = 150
        i_screen_end_line     = 18
      TABLES
        t_outtab              = t_zsdt0210.
*-US192246-06.10.2025-#1922468-JT-fim

  ENDIF.
ENDFUNCTION.

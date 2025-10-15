FUNCTION z_zmail_logs_table.

*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CUSOBJ) TYPE  OBJS-OBJECTNAME
*"     REFERENCE(OBJFIRST) DEFAULT SPACE
*"     REFERENCE(TABFIRST) DEFAULT 'X'
*"     REFERENCE(ALV_GRID) DEFAULT 'X'
*"     REFERENCE(IGN_UNCH) DEFAULT ' '
*"     REFERENCE(ADDTBLGS) DEFAULT ' '
*"----------------------------------------------------------------------

  DATA: lit_fields    TYPE TABLE OF sval WITH HEADER LINE,
        lva_date      TYPE sy-datum,
        lva_return    TYPE vbpok-charg,
        lva_date_from TYPE sy-datum,
        lva_date_to   TYPE sy-datum,
        lva_time_from TYPE sy-uzeit,
        lva_time_to   TYPE sy-uzeit.

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

  lit_fields-tabname = 'ZMAIL'.
  lit_fields-fieldname = 'ZDT_ATUAL'.
  lit_fields-fieldtext = 'Data Inicio'.
  lit_fields-value = lva_date.
  APPEND lit_fields.

  lit_fields-tabname = 'ZMAIL'.
  lit_fields-fieldname = 'ZHR_ATUAL'.
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


    SUBMIT rsvtprot
      WITH cusobj   EQ  cusobj "'ZFIT0033'
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

  ENDIF.
ENDFUNCTION.

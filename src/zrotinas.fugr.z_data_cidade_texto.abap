FUNCTION z_data_cidade_texto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(LANGU) TYPE  SY-LANGU OPTIONAL
*"     REFERENCE(PAIS) TYPE  LAND1 DEFAULT 'BR'
*"     REFERENCE(DATE) TYPE  DATUM
*"     REFERENCE(TXJCD) TYPE  TXJCD OPTIONAL
*"  EXPORTING
*"     REFERENCE(TX_DATE) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: wa_j_1btxjurt TYPE j_1btxjurt,
        wa_data       TYPE c LENGTH 10,
        wa_t247       TYPE  t247,
        vg_month      TYPE  month.

  WRITE date TO wa_data.

  IF langu IS INITIAL.
    langu = sy-langu.
  ENDIF.

  IF NOT txjcd IS INITIAL.

    SELECT SINGLE * INTO wa_j_1btxjurt
      FROM j_1btxjurt
     WHERE spras      EQ langu
       AND country    EQ pais
       AND taxjurcode EQ txjcd.

    CONCATENATE wa_j_1btxjurt-text '-' wa_j_1btxjurt-taxjurcode(2) INTO tx_date SEPARATED BY space.
    CONCATENATE tx_date ',' INTO tx_date.
  ENDIF.

  WRITE wa_data+3(2) TO vg_month.

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      langu = langu
      month = vg_month
    IMPORTING
      t247  = wa_t247.

  CONCATENATE tx_date wa_data(2) wa_t247-ltx 'de' wa_data+6(4) INTO tx_date SEPARATED BY space.

ENDFUNCTION.

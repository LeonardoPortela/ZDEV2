*&---------------------------------------------------------------------*
*& Report ZLOG0002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlog0002.

TABLES: zlog1001.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_data  FOR zlog1001-erdat NO-EXTENSION OBLIGATORY.
  SELECT-OPTIONS: p_user  FOR zlog1001-usnam NO-EXTENSION no INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  DATA: it_datas  TYPE TABLE OF iscal_day.

  DATA: lva_data_processa TYPE zlog1001-erdat.

  DATA(_stop) = abap_false.

  lva_data_processa = p_data-low.

  WHILE _stop EQ abap_false.

    MESSAGE |Processando dia { lva_data_processa } | TYPE 'S'.

    "Check Dia Util
    CLEAR: it_datas[].

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar = 'BR'
        factory_calendar = 'ZF'
        date_from        = lva_data_processa
        date_to          = lva_data_processa
      TABLES
        holidays         = it_datas
      EXCEPTIONS
        OTHERS           = 1.

    IF it_datas[] IS INITIAL.
      CALL FUNCTION 'ZLOG_SM20N_001'
        EXPORTING
          i_erdat = lva_data_processa
          i_usnam = p_user-low.
    ENDIF.

    IF p_data-high IS INITIAL.
      EXIT.
    ENDIF.

    IF lva_data_processa >= p_data-high.
      EXIT.
    ENDIF.

    ADD 1 TO lva_data_processa.

  ENDWHILE.

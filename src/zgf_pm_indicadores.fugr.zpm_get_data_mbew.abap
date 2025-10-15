FUNCTION zpm_get_data_mbew.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_YEAR) TYPE  GJAHR OPTIONAL
*"     VALUE(I_DATE) TYPE  DATS OPTIONAL
*"  TABLES
*"      MBEW STRUCTURE  MBEW OPTIONAL
*"----------------------------------------------------------------------

  DATA t_mard TYPE TABLE OF mard.
  CALL FUNCTION 'ZPM_GET_DATA_MARD'
    EXPORTING
      i_year = i_year
      i_date = i_date
    TABLES
      mard   = t_mard.

  IF t_mard IS NOT INITIAL.
    SELECT * FROM mbew INTO TABLE mbew
      FOR ALL ENTRIES IN t_mard
      WHERE matnr EQ t_mard-matnr.
  ENDIF.


ENDFUNCTION.

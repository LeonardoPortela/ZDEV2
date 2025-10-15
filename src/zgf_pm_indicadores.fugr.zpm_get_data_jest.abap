FUNCTION zpm_get_data_jest.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"     VALUE(I_HOUR) TYPE  TIMS OPTIONAL
*"  TABLES
*"      JEST STRUCTURE  JEST OPTIONAL
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZPM_GET_DATA_AUFK'
    EXPORTING
      i_start = i_start
      i_end   = i_end
      i_hour  = i_hour
    TABLES
      aufk   = t_aufk.


  IF t_aufk IS NOT INITIAL.
    SELECT * FROM jest INTO TABLE jest
      FOR ALL ENTRIES IN t_aufk
      WHERE objnr EQ t_aufk-objnr.
  ENDIF.


ENDFUNCTION.

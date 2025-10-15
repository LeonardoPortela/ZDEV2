FUNCTION zpm_get_data_afru.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"     VALUE(I_HOUR) TYPE  TIMS OPTIONAL
*"  TABLES
*"      AFRU STRUCTURE  AFRU OPTIONAL
*"----------------------------------------------------------------------
  DATA t_aufk TYPE TABLE OF aufk.

  CALL FUNCTION 'ZPM_GET_DATA_AUFK'
    EXPORTING
      i_start = i_start
      i_end   = i_end
      i_hour  = i_hour
    TABLES
      aufk   = t_aufk.


  IF t_aufk IS NOT INITIAL.
    SELECT * FROM afru INTO TABLE afru
      FOR ALL ENTRIES IN t_aufk
      WHERE aufnr EQ t_aufk-aufnr.
  ENDIF.


ENDFUNCTION.

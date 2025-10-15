FUNCTION zpm_get_data_itob.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"     VALUE(I_HOUR) TYPE  TIMS OPTIONAL
*"  TABLES
*"      ITOB STRUCTURE  ITOB OPTIONAL
*"----------------------------------------------------------------------
DATA t_equi TYPE TABLE OF equi.

CALL FUNCTION 'ZPM_GET_DATA_EQUI'
 EXPORTING
   i_start        = i_start
   i_end          = i_end
   I_HOUR         = i_hour
 TABLES
   EQUI           = t_equi .

CHECK t_equi IS NOT INITIAL.

  SELECT * FROM itob INTO TABLE itob
  FOR ALL ENTRIES IN t_equi
  WHERE equnr EQ t_equi-equnr.



ENDFUNCTION.

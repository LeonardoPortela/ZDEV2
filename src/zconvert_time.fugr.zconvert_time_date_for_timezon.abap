FUNCTION ZCONVERT_TIME_DATE_FOR_TIMEZON.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATE) TYPE  DATS
*"     REFERENCE(I_TIME) TYPE  TIMS
*"     REFERENCE(I_TIMEZONE_FROM) TYPE  TZONREF-TZONE
*"     REFERENCE(I_TIMEZONE_TO) TYPE  TZONREF-TZONE
*"  EXPORTING
*"     REFERENCE(E_TIME) TYPE  TIMS
*"     REFERENCE(E_DATE) TYPE  DATS
*"----------------------------------------------------------------------

DATA:
           lv_timstamp_tmp     TYPE timestamp.


 CONVERT DATE i_date
         TIME i_time INTO TIME STAMP lv_timstamp_tmp TIME ZONE i_timezone_from.

 CONVERT TIME STAMP lv_timstamp_tmp TIME ZONE i_timezone_to INTO DATE e_date
                                                                 TIME e_time.




ENDFUNCTION.

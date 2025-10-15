FUNCTION ZPM_GET_DATA_IMRG.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"     VALUE(I_HOUR) TYPE  TIMS OPTIONAL
*"  TABLES
*"      IMRG STRUCTURE  IMRG OPTIONAL
*"----------------------------------------------------------------------
  DATA: r_dats TYPE RANGE OF dats,
        r_tims TYPE RANGE OF tims.

  PERFORM f_date_range USING i_start i_end i_hour
                       CHANGING r_dats r_tims.


SELECT * FROM imrg INTO TABLE imrg
  WHERE ( erdat IN r_dats
    AND   ERUHR IN r_tims )
     OR   AEDAT IN r_dats.



ENDFUNCTION.

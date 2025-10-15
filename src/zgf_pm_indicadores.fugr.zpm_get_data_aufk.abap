FUNCTION zpm_get_data_aufk.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"     VALUE(I_HOUR) TYPE  TIMS OPTIONAL
*"  TABLES
*"      AUFK STRUCTURE  AUFK OPTIONAL
*"----------------------------------------------------------------------
  DATA: r_dats TYPE RANGE OF dats,
        r_tims TYPE RANGE OF tims.

  PERFORM f_date_range USING i_start i_end i_hour
                       CHANGING r_dats r_tims.

  SELECT * FROM aufk INTO TABLE aufk
    WHERE ( erdat IN r_dats
      AND  ERFZEIT IN r_tims
      AND AUTYP EQ '30')
       OR (  aedat IN r_dats
      AND AEZEIT  IN r_tims
      AND AUTYP EQ '30' ).



ENDFUNCTION.

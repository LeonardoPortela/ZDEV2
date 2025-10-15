FUNCTION zpm_get_data_equi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"     VALUE(I_HOUR) TYPE  TIMS OPTIONAL
*"  TABLES
*"      EQUI STRUCTURE  EQUI OPTIONAL
*"----------------------------------------------------------------------
  DATA: r_timstamp TYPE RANGE OF char15,
        r_dats     TYPE RANGE OF dats.

  r_dats = VALUE #( sign = 'I' option = 'BT' ( low  = i_start
                                               high = i_end ) ).

  SELECT * FROM equi INTO TABLE equi
    WHERE erdat           IN r_dats.

  IF i_hour IS NOT INITIAL.
    r_timstamp = VALUE #( sign = 'I' option = 'BT' ( low  = |{ i_start }{ i_hour }|
                                                     high = |{ i_end }235959| ) ).

    SELECT * FROM equi APPENDING TABLE equi
      WHERE changeddatetime IN r_timstamp.
  ENDIF.

ENDFUNCTION.

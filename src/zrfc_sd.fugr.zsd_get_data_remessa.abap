FUNCTION zsd_get_data_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_AEDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_AEDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_VBELN STRUCTURE  ZSDDE0325
*"      T_SAIDA_LIKP STRUCTURE  LIKPVB
*"      T_SAIDA_LIPS STRUCTURE  LIPSVB
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_vbeln,
           vbeln TYPE likp-vbeln,
         END OF ty_vbeln.

  TYPES: lr_range_te TYPE RANGE OF erdat,
         lr_range_ta TYPE RANGE OF aedat.
  DATA: it_vbeln       TYPE STANDARD TABLE OF ty_vbeln,
        ls_vbeln       TYPE ty_vbeln,
        it_range_erdat TYPE RANGE OF likp-erdat,
        it_range_aedat TYPE RANGE OF likp-aedat.


  IF ( i_erdat_ini IS NOT INITIAL AND i_erdat_fim IS NOT INITIAL ) OR ( i_aedat_ini IS NOT INITIAL AND i_aedat_fim IS NOT INITIAL ).

    IF i_erdat_ini IS NOT INITIAL.
      it_range_erdat = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                              option = 'BT' ( low = i_erdat_ini high = i_erdat_fim )
                                                                            ).
    ENDIF.

    IF i_aedat_ini IS NOT INITIAL.
      it_range_aedat = VALUE lr_range_ta( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                                option = 'BT' ( low = i_aedat_ini high = i_aedat_fim )
                                                                              ).
    ENDIF.

    LOOP AT t_vbeln INTO DATA(lwr_vbeln).
      ls_vbeln-vbeln = lwr_vbeln-vbeln.
      APPEND ls_vbeln TO it_vbeln.
      CLEAR ls_vbeln.
    ENDLOOP.


    IF it_vbeln[] IS NOT INITIAL.
      SELECT *
      FROM likp
       INTO TABLE t_saida_likp
        FOR ALL ENTRIES IN it_vbeln
        WHERE erdat IN it_range_erdat
          AND aedat IN it_range_aedat
          AND vbeln EQ it_vbeln-vbeln.


      SELECT *
        FROM lips
        INTO TABLE t_saida_lips
        FOR ALL ENTRIES IN it_vbeln
         WHERE erdat IN it_range_erdat
           AND aedat IN it_range_aedat
           AND vbeln EQ it_vbeln-vbeln.

    ELSE.

      SELECT *
      FROM likp
      INTO TABLE t_saida_likp
      WHERE erdat IN it_range_erdat
        AND aedat IN it_range_aedat.


      SELECT *
        FROM lips
        INTO TABLE t_saida_lips
         WHERE erdat IN it_range_erdat
           AND aedat IN it_range_aedat.

    ENDIF.




  ENDIF.

ENDFUNCTION.

FUNCTION zfi_get_data_zlest0079.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_NRO_SOL_OV) TYPE  EBELN OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZDE_DATA_ZLEST0079_OUT
*"----------------------------------------------------------------------


  RANGES: lra_data_registro FOR zfit0087-data_registro.

  DATA:lra_ebeln TYPE RANGE OF zlest0105-ebeln,
       lra_augbl TYPE RANGE OF zlest0105-augbl,
       lRA_nr_sol_ov TYPE RANGE OF zfit0087-nr_sol.

  CLEAR: t_saida[].

*  lt_range_ebeln = VALUE #(
*FOR <ls_itab1> IN t_ebeln
*( sign = 'I'
*  option = 'EQ'
*  low = <ls_itab1> )
* ).
*
*  lt_range_augbl = VALUE #(
*  FOR <ls_itab2> IN t_augbl
*  ( sign = 'I'
*    option = 'EQ'
*    low = <ls_itab2> )
*  ).

  CHECK ( i_data_ini IS NOT INITIAL
          AND i_data_fim IS NOT INITIAL )
          OR I_NRO_SOL_OV IS NOT INITIAL.
          "OR i_augbl IS NOT INITIAL.


  IF i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL .
    APPEND VALUE #( sign = 'I'  option = 'BT' low = i_data_ini high = i_data_fim ) TO lra_data_registro.
  ENDIF.

*    IF i_ebeln IS NOT INITIAL .
*    APPEND VALUE #( sign = 'I'  option = 'EQ' low = i_ebeln ) TO lra_ebeln.
*  ENDIF.
*
*    IF i_augbl IS NOT INITIAL .
*    APPEND VALUE #( sign = 'I'  option = 'EQ' low = i_augbl ) TO lra_augbl.
*  ENDIF.

IF i_nro_sol_ov IS NOT INITIAL .
    APPEND VALUE #( sign = 'I'  option = 'EQ' low = i_nro_sol_ov ) TO lRA_nr_sol_ov.
  ENDIF.

  SELECT *
    FROM zlest0105 INTO TABLE @DATA(lit_zlest0105)
   WHERE dt_atual IN @lra_data_registro
         AND ( ebeln IN @lRA_nr_sol_ov
         or  augbl IN @lRA_nr_sol_ov ) .

  CHECK lit_zlest0105[] IS NOT INITIAL.

*--------------------------------------------------------------------------------------*
*  Montar Saida
*--------------------------------------------------------------------------------------*
  LOOP AT lit_zlest0105 ASSIGNING FIELD-SYMBOL(<fs_zlest0105>).
    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    MOVE-CORRESPONDING <fs_zlest0105> TO <fs_saida>.

  ENDLOOP.


ENDFUNCTION.

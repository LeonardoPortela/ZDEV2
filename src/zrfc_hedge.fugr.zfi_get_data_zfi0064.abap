FUNCTION zfi_get_data_zfi0064.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_NRO_SOL_OV) TYPE  EBELN OPTIONAL
*"  TABLES
*"      T_VBELN STRUCTURE  ZSDE_VBELN OPTIONAL
*"      T_SAIDA STRUCTURE  ZDE_DATA_ZFI0064_OUT
*"----------------------------------------------------------------------

  DATA: BEGIN OF lit_mara OCCURS 0,
          matnr TYPE mara-matnr,
          matkl TYPE mara-matkl,
        END OF lit_mara,

        BEGIN OF lit_kna1 OCCURS 0,
          kunnr TYPE kna1-kunnr,
          ktokd TYPE kna1-ktokd,
        END OF lit_kna1.

  RANGES: lra_data_registro FOR zfit0087-data_registro.
  DATA: lra_nr_sol_ov  TYPE RANGE OF zfit0087-nr_sol,
        lt_range_vbeln TYPE RANGE OF zfit0087-vbeln.

  CLEAR: t_saida[].

  lt_range_vbeln = VALUE #(
  FOR <ls_itab2> IN t_vbeln
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab2> )
  ).

  IF i_nro_sol_ov IS NOT INITIAL .
    APPEND VALUE #( sign = 'I'  option = 'EQ' low = i_nro_sol_ov ) TO lra_nr_sol_ov.
  ENDIF.

  CHECK ( i_data_ini IS NOT INITIAL AND
        i_data_fim IS NOT INITIAL ) OR
        lt_range_vbeln IS NOT INITIAL OR
        lra_nr_sol_ov IS NOT INITIAL.

  IF i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL .
    APPEND VALUE #( sign = 'I'  option = 'BT' low = i_data_ini high = i_data_fim ) TO lra_data_registro.
  ENDIF.

  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
  SELECT *
    FROM zfit0087 INTO TABLE @DATA(lit_zfit0087)
   WHERE data_registro IN @lra_data_registro.

  SELECT *
    FROM zfit0087 APPENDING TABLE @lit_zfit0087
   WHERE data_ajuste IN @lra_data_registro.

  DELETE lit_zfit0087 WHERE NOT ( nr_sol IN lra_nr_sol_ov AND vbeln IN lt_range_vbeln ).

  SORT lit_zfit0087 BY belnr buzei bukrs estorno.
  DELETE ADJACENT DUPLICATES FROM lit_zfit0087 COMPARING belnr buzei bukrs estorno.
  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<---

  CHECK lit_zfit0087[] IS NOT INITIAL.

  "Busca Material
  DATA(lit_zfit0087_aux) = lit_zfit0087[].
  SORT lit_zfit0087_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM lit_zfit0087_aux COMPARING matnr.

  IF lit_zfit0087_aux[] IS NOT INITIAL.
    SELECT matnr matkl
      FROM mara INTO TABLE lit_mara
       FOR ALL ENTRIES IN lit_zfit0087_aux
     WHERE matnr EQ lit_zfit0087_aux-matnr.
  ENDIF.

  "Busca Clientes
  lit_zfit0087_aux = lit_zfit0087[].
  SORT lit_zfit0087_aux BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lit_zfit0087_aux COMPARING kunnr.

  IF lit_zfit0087_aux[] IS NOT INITIAL.
    SELECT kunnr ktokd
      FROM kna1 INTO TABLE lit_kna1
       FOR ALL ENTRIES IN lit_zfit0087_aux
     WHERE kunnr EQ lit_zfit0087_aux-kunnr.
  ENDIF.

*--------------------------------------------------------------------------------------*
*  Sorts
*--------------------------------------------------------------------------------------*
  SORT: lit_mara  BY matnr,
        lit_kna1  BY kunnr.

*--------------------------------------------------------------------------------------*
*  Montar Saida
*--------------------------------------------------------------------------------------*
  LOOP AT lit_zfit0087 ASSIGNING FIELD-SYMBOL(<fs_zfit0087>).
    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    MOVE-CORRESPONDING <fs_zfit0087> TO <fs_saida>.

    CLEAR: <fs_saida>-matkl, <fs_saida>-ktokd.

    READ TABLE lit_mara INTO DATA(lwa_mara) WITH KEY matnr = <fs_saida>-matnr BINARY SEARCH.
    IF sy-subrc EQ 0 AND <fs_saida>-matnr IS NOT INITIAL.
      <fs_saida>-matkl = lwa_mara-matkl.
    ENDIF.

    READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = <fs_saida>-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0 AND <fs_saida>-kunnr IS NOT INITIAL.
      <fs_saida>-ktokd = lwa_kna1-ktokd.
    ENDIF.
  ENDLOOP.


ENDFUNCTION.

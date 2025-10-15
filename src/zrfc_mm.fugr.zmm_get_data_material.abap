FUNCTION zmm_get_data_material .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERSDA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERSDA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_LAEDA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_LAEDA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_MATNR STRUCTURE  ZMMST_MATNR
*"      ET_RET_MARA TYPE  ZSDST0040_MARA
*"      ET_RET_MAKT TYPE  ZSDST0040_MAKT
*"----------------------------------------------------------------------

  DATA: lt_range_ersda TYPE RANGE OF mara-ersda,
        lt_range_laeda TYPE RANGE OF mara-laeda,
        lra_matnr      TYPE RANGE OF mara-matnr.


  CHECK i_ersda_ini IS NOT INITIAL OR i_laeda_ini IS NOT INITIAL or t_matnr[] is NOT INITIAL.

  IF i_ersda_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_ersda_ini
                             high = i_ersda_fim ) TO lt_range_ersda.
  ENDIF.

  IF i_laeda_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_laeda_ini
                             high = i_laeda_fim ) TO lt_range_laeda.
  ENDIF.

  IF t_matnr[] IS NOT INITIAL.

    LOOP AT t_matnr ASSIGNING FIELD-SYMBOL(<fs_matnr>).

      APPEND VALUE #(  sign = 'I'
                       option = 'EQ'
                       low  = <fs_matnr>-matnr
                        ) TO lra_matnr.

    ENDLOOP.

  ENDIF.

  SELECT *
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE et_ret_mara
    WHERE matnr IN lra_matnr
      AND ersda IN lt_range_ersda
      AND laeda IN lt_range_laeda.
  IF sy-subrc IS INITIAL.
    SORT et_ret_mara BY matnr ersda.
  ENDIF.

  IF et_ret_mara[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO CORRESPONDING FIELDS OF TABLE et_ret_makt
      FOR ALL ENTRIES IN et_ret_mara
      WHERE matnr EQ et_ret_mara-matnr.
    IF sy-subrc IS INITIAL.
      SORT et_ret_makt BY matnr.
    ENDIF.
  ENDIF.


ENDFUNCTION.

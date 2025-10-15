FUNCTION zsd_get_dados_transp_manu_mdfe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  DATUM
*"     VALUE(I_DATA_FIM) TYPE  DATUM
*"  TABLES
*"      ET_ZSDT0118 STRUCTURE  ZSDT0118
*"----------------------------------------------------------------------

  DATA: lr_range_data TYPE RANGE OF datum.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #( sign = 'I'
                  option = 'BT'
                  low  = i_data_ini
                  high = i_data_fim ) TO lr_range_data.

  SELECT *
    FROM zsdt0102
    INTO TABLE @DATA(lt_102)
    WHERE data_emi IN @lr_range_data.
  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zsdt0118
      INTO TABLE et_zsdt0118
      FOR ALL ENTRIES IN lt_102
      WHERE docnum = lt_102-docnum.
  ENDIF.



ENDFUNCTION.

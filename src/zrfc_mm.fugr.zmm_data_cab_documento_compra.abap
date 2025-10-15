FUNCTION zmm_data_cab_documento_compra.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  AEDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  AEDAT OPTIONAL
*"  TABLES
*"      ET_EKKO STRUCTURE  EKKO
*"----------------------------------------------------------------------

  DATA: lt_range_data  TYPE RANGE OF ekko-aedat.
  DATA: lt_range_change TYPE RANGE OF ekko-lastchangedatetime,
        lv_dt_ini       TYPE ekko-lastchangedatetime,
        lv_dt_fim       TYPE ekko-lastchangedatetime,
        lv_hora_fim     TYPE sy-uzeit.

  CHECK i_data_ini IS NOT INITIAL.

  APPEND VALUE #(  sign = 'I'
                           option = 'BT'
                           low  = i_data_ini
                           high = i_data_fim ) TO lt_range_data.

  CALL FUNCTION 'RSSM_GET_TIME'
    EXPORTING
      i_datum_loc  = i_data_ini
    IMPORTING
      e_timestampl = lv_dt_ini
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  lv_hora_fim  = '235900'.

  CALL FUNCTION 'RSSM_GET_TIME'
    EXPORTING
      i_datum_loc  = i_data_fim
      i_uzeit_loc  = lv_hora_fim
    IMPORTING
      e_timestampl = lv_dt_fim
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  APPEND VALUE #(  sign = 'I'
                              option = 'BT'
                              low  = lv_dt_ini
                              high = lv_dt_fim ) TO lt_range_change.

  SELECT *
   FROM ekko
   INTO TABLE et_ekko
   WHERE aedat IN lt_range_data.

  SELECT *
  FROM ekko
  APPENDING TABLE et_ekko
  WHERE lastchangedatetime IN lt_range_change.

  SORT et_ekko BY ebeln.
  DELETE ADJACENT DUPLICATES FROM et_ekko COMPARING ebeln.

ENDFUNCTION.

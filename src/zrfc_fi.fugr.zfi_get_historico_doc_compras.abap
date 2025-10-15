FUNCTION ZFI_GET_HISTORICO_DOC_COMPRAS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BUDAT_INI) TYPE  BUDAT OPTIONAL
*"     VALUE(I_BUDAT_FIM) TYPE  BUDAT OPTIONAL
*"  TABLES
*"      ET_EKBE STRUCTURE  EKBE
*"----------------------------------------------------------------------


  DATA: lt_range_budat TYPE RANGE OF bsad-budat.

  CHECK i_budat_ini IS NOT INITIAL.

  IF i_budat_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_budat_ini
                             high = i_budat_fim ) TO lt_range_budat.
  ENDIF.

  SELECT *
    FROM ekbe
    INTO TABLE et_ekbe
    WHERE budat IN lt_range_budat.

ENDFUNCTION.

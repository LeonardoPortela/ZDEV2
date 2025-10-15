FUNCTION zfi_get_data_aging.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_TP_PROC) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_ZFBDT_INI) TYPE  DZFBDT OPTIONAL
*"     VALUE(I_ZFBDT_FIM) TYPE  DZFBDT OPTIONAL
*"  TABLES
*"      T_BUKRS TYPE  ZFIST0001 OPTIONAL
*"      T_KUNNR TYPE  ZFIST0001_1 OPTIONAL
*"      T_LIFNR TYPE  ZFIST0001_2 OPTIONAL
*"      T_DOC_SIMULACAO TYPE  ZFIST0001_3 OPTIONAL
*"      T_DOC_VENDA TYPE  ZFIST0001_4 OPTIONAL
*"      T_SAIDA TYPE  ZFIST0002
*"----------------------------------------------------------------------

  it_range_bukrs = VALUE #(
    FOR <ls_itab> IN t_bukrs
    ( sign = 'I'
      option = 'EQ'
      low = <ls_itab> )
  ).

  it_range_kunnr = VALUE #(
    FOR <ls_itab1> IN t_kunnr
    ( sign = 'I'
      option = 'EQ'
      low = <ls_itab1> )
  ).

  it_range_lifnr = VALUE #(
  FOR <ls_itab2> IN t_lifnr
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab2> )
).

  it_range_doc_simu = VALUE #(
  FOR <ls_itab3> IN t_doc_simulacao
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab3> )
).

  it_range_vbeln = VALUE #(
  FOR <ls_itab4> IN t_doc_venda
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab4> )
).

  append VALUE #(  sign = 'I' OPTION = 'BT' low = '20070101' high = sy-datum ) to it_range_budat.

  if I_ZFBDT_INI is NOT INITIAL.
    append VALUE #(  sign = 'I' OPTION = 'BT' low = i_zfbdt_ini high = i_zfbdt_fim ) to it_range_zfbdt.
  endif.

  CASE i_tp_proc.
    WHEN 'F'.
      PERFORM f_select_f CHANGING t_saida[].
    WHEN 'C'.
      PERFORM f_select_c CHANGING t_saida[].
    WHEN OTHERS.
      PERFORM f_select_c CHANGING t_saida[].
      PERFORM f_select_f CHANGING t_saida[].
  ENDCASE.




ENDFUNCTION.

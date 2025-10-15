*DATA : it_texto TYPE TABLE OF string.
*
*SPLIT gw_order_header-registro_mapa AT space INTO TABLE it_texto.
*
*TRY .
*    registro_mapa = it_texto[ 1 ].
*  CATCH cx_sy_itab_line_not_found.
*ENDTRY.

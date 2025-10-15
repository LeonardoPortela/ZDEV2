data: tl_mes type table of t247 with header line.

refresh: tl_mes.

call function 'MONTH_NAMES_GET'
  tables
    month_names           = tl_mes
  exceptions
    month_names_not_found = 1
    others                = 2.
if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.
read table tl_mes
  with key mnr = wg_header-data+4(2).

concatenate wg_header-cidade '-' wg_header-uf ',' wg_header-data+6(2) tl_mes-ltx 'de' wg_header-data(4)
into wg_descritivo separated by space.




















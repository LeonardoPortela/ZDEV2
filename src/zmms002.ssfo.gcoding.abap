
data:
      wl_table1,
      wl_table2,
      wl_table3,
      wl_table4.

**tg_fardos1[] = tg_fardos[].
**do 165 times.
**add 1 to wg_cont.
**
**move: '11111' to tg_fardos-tipo,
**      '10'    to tg_fardos-peso,
**      wg_cont to tg_fardos-fardo.
**append tg_fardos.
**
**enddo.
sort: tg_fardos by fardo ascending.

clear: wg_cont.
loop at tg_fardos.
  add 1 to wg_cont.

*  if wl_cont le 41.
  if wl_table1 is initial.
    append tg_fardos to tg_fardos1.
    if wg_cont eq 41.
      move 'X' to wl_table1.
      clear: wg_cont.
    endif.
  elseif wl_table2 is initial.
    append tg_fardos to tg_fardos2.
    if wg_cont eq 41.
      move 'X' to wl_table2.
      clear: wg_cont.
    endif.
  elseif wl_table3 is initial.
        append tg_fardos to tg_fardos3.
    if wg_cont eq 41.
      move 'X' to wl_table3.
      clear: wg_cont.
    endif.
  elseif wl_table4 is initial.
      append tg_fardos to tg_fardos4.
    if wg_cont eq 41.
      move 'X' to wl_table4.
      clear: wg_cont, wl_table1, wl_table2, wl_table3,
             wl_table4.
    endif.
  endif.


 endloop.

















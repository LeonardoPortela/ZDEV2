*&---------------------------------------------------------------------*
*& Report Z_357991
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_357991 .

PARAMETERS: TESTMODE AS CHECKBOX DEFAULT 'X'.
tables: iloa.
DATA: lv_pSIZE TYPE I VALUE 1000.
data: lv_cursor type cursor.
data: begin of lt_adrc occurs 0,
      addrnumber like adrc-addrnumber,
      end of lt_adrc.
data: lt_iloa like iloa occurs 0 with header line.

open cursor with hold lv_cursor for select * from iloa
                                    where adrnr <> space.
do.
  fetch next cursor lv_cursor into table lt_iloa package size lv_psize.
  if sy-subrc <> 0.
    close cursor lv_cursor.
    exit.
  endif.

  select addrnumber   from adrc into table lt_adrc
                      for all entries in lt_iloa
                      where addrnumber = lt_iloa-adrnr.
  sort lt_adrc by addrnumber.

  loop at lt_iloa.
    read table lt_adrc with key addrnumber = lt_iloa-adrnr
                                binary search.
    if sy-subrc <> 0.
      write: / 'ILOA-ILOAN:', lt_ILOA-ILOAN, ', address number',
                lt_iloa-adrnr, 'was cleared'.
      clear lt_iloa-adrnr.
      modify lt_iloa.
    endif.
  endloop.
  if testmode is initial.
    update iloa from table lt_iloa.
  endif.
  refresh: lt_iloa, lt_adrc.
enddo.

if testmode = 'X'.
   write: / 'Testmode only. No Updates.'.
endif.

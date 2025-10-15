report zpcon_051.
***********************************************************************
* Report zum Ermitteln von Vorgängen, deren Rückmeldezähler kleiner
* der Anzahl erfasster Rückmeldungen ist.
* Diese Aufträge verursachen bei der nächsten Rückmeldung einen
* Kurz Dump wegen DUPREC !!!!!!
* Im Update-Modus werden die Rückmeldzähler dieser problembehafteten
* Vorgänge angepasst.
*
* This report determines operations having a confirmation counter
* smaller than the number of confirmations on the operation.
* These kind of operations cause a shortdumps because of DUPRECs.
* If the update-flag is set, the confirmation counter is updated
* on the database.
***********************************************************************

tables: caufv, afvc, afru.

select-options: paaufnr for caufv-aufnr,
                paaplfl for afvc-aplfl,
                pavornr for afvc-vornr.
parameters: update as checkbox.

data: begin of gt_caufv occurs 0,
        aufnr like caufv-aufnr,
        aufpl like caufv-aufpl,
      end of gt_caufv.

data: begin of gt_afvc occurs 0,
        aufpl like afvc-aufpl,
        aplzl like afvc-aplzl,
        aplfl like afvc-aplfl,
        vornr like afvc-vornr,
        rueck like afvc-rueck,
        rmzhl like afvc-rmzhl,
      end of gt_afvc.

data: begin of gt_afru occurs 0,
        rueck like afru-rueck,
        rmzhl like afru-rmzhl,
      end of gt_afru.

data: t_tmp_rueck like afru-rueck.

  if paaufnr-low is initial.
    message i889(co) with 'Bitte auszuwählende Aufträge einschränken'.
    exit.
  endif.
* Aufträge lesen
  select aufnr aufpl from caufv into table gt_caufv
              where aufnr in paaufnr
              and ( autyp = '10'              "PP-Auftrag
                 or autyp = '40' )            "PI-Auftrag
              and   gstri > '00000000'.       "Rückmeldung erfasst
  check not gt_caufv[] is initial.
  sort gt_caufv by aufpl.
* Vorgänge lesen
  select aufpl aplzl aplfl vornr rueck rmzhl from afvc
         into table gt_afvc
         for all entries in gt_caufv
         where aufpl = gt_caufv-aufpl
         and   aplfl in paaplfl
         and   vornr in pavornr.
  check not gt_afvc[] is initial.
* Rückmeldungen lesen
  select rueck rmzhl from afru into table gt_afru
               for all entries in gt_afvc
               where rueck = gt_afvc-rueck.
  sort gt_afru by rueck rmzhl descending.
  clear t_tmp_rueck.
  loop at gt_afru.
    if gt_afru-rueck = t_tmp_rueck.
      delete gt_afru.
    else.
      t_tmp_rueck = gt_afru-rueck.
    endif.
  endloop.
* Ausgabe
  write: / 'Rückmeldezähler in folgenden Vorgängen nicht aktualisiert'.
  skip.
  write: /2 'Auftrag', 16 'Folge', 24 'Vorgang', 36 'RUECK',
         46 'Zähler Vorgang', 64 'Zähler Rückmeldung'.
  uline.
  loop at gt_afvc.
    read table gt_afru with key rueck = gt_afvc-rueck binary search.
    check sy-subrc = 0.
    if gt_afvc-rmzhl < gt_afru-rmzhl.
      read table gt_caufv with key aufpl = gt_afvc-aufpl binary search.
      write: /2 gt_caufv-aufnr, 16 gt_afvc-aplfl, 26 gt_afvc-vornr,
             35 gt_afvc-rueck, 50 gt_afvc-rmzhl, 68 gt_afru-rmzhl.
      check not update is initial.
*     Update !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      update afvc set rmzhl = gt_afru-rmzhl
             where aufpl = gt_afvc-aufpl
             and   aplzl = gt_afvc-aplzl.
    endif.
  endloop.

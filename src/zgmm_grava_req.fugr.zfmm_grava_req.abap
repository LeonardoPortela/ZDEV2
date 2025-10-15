function zfmm_grava_req.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BANFN) LIKE  EBAN-BANFN
*"----------------------------------------------------------------------


**** CESAR COELHO - 04.08.2008
  data: t_zmmt0004 like zmmt0004 occurs 0 with header line,
        t_eban     like eban     occurs 0 with header line.

  select *
    from eban
    into table t_eban
    where banfn eq banfn.

  if sy-subrc = 0.

    select *
      from zmmt0004
      into table t_zmmt0004
      for all entries in t_eban
      where matnr = t_eban-matnr and
            werks = t_eban-werks.

    if sy-subrc = 0.

      loop at t_eban.

        read table t_zmmt0004 with key matnr = t_eban-matnr
                                   werks = t_eban-werks.

        if sy-subrc = 0.

          if t_zmmt0004-kdatb ge sy-datum and
             t_zmmt0004-kdate le sy-datum.

            update eban set ebeln = t_eban-ebeln
                            ebelp = t_eban-ebelp
                        where matnr = t_eban-matnr and
                              werks = t_eban-werks.

          endif.

        endif.

      endloop.

    endif.

  endif.

endfunction.

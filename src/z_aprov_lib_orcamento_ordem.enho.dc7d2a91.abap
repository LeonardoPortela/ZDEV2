"Name: \PR:RIGENE10\FO:SELECTION_L\SE:END\EI
ENHANCEMENT 0 Z_APROV_LIB_ORCAMENTO_ORDEM.
*
  data: tl_caufv type TABLE OF caufv with header line.

  FIELD-SYMBOLS: <fs_object_tab> like object_tab.

  select *
    from caufv
    into table tl_caufv
    for all entries in object_tab
    where aufnr = object_tab-aufnr.

  LOOP AT object_tab ASSIGNING <fs_object_tab>.
    read table tl_caufv into tl_caufv with key aufnr = <fs_object_tab>-aufnr.
    if sy-subrc is initial.
      <fs_object_tab>-user4 = tl_caufv-user4.
    endif.
  ENDLOOP.
ENDENHANCEMENT.

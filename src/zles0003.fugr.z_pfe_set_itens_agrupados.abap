function z_pfe_set_itens_agrupados.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_ITEM) TYPE  ZPFE_LOTE_ITEM_ALV
*"  TABLES
*"      P_ITENS_ALV STRUCTURE  ZPFE_LOTE_ITEM_ALV OPTIONAL
*"----------------------------------------------------------------------


  data: it_zpfe_chvid_ag type table of zpfe_chvid_ag with header line,
        wa_item          type zpfe_lote_item_alv.

  select * into table it_zpfe_chvid_ag
    from zpfe_chvid_ag
   where chvid_ch eq p_item-chvid.

  loop at it_zpfe_chvid_ag.
    loop at p_itens_alv into wa_item
      where docnum eq p_item-docnum
        and chvid  eq it_zpfe_chvid_ag-chvid_ch_vinc.
      wa_item-ck_conferido = p_item-ck_conferido.
      modify p_itens_alv index sy-tabix from wa_item transporting ck_conferido.
    endloop.
  endloop.

endfunction.

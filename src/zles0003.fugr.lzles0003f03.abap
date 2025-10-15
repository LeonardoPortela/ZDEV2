*----------------------------------------------------------------------*
***INCLUDE LZLES0003F03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_ITENS
*&---------------------------------------------------------------------*
*       Confirmar quais ajustes serão lancados para ajustar financeiro
*----------------------------------------------------------------------*
form selecionar_itens  tables p_it_lotes_item structure zpfe_lote_item
                        using vg_cancelado type c.

  data: vg_tabix         type sy-tabix,
        p_apfe_lote_alv  type zpfe_lote_alv,
        p_zpfe_lote_item type zpfe_lote_item.

  clear: vg_cancelado, vg_cancelado_0002.

  vg_total_ajustar = 0.

  read table p_it_lotes_item index 1 into p_zpfe_lote_item.
  check sy-subrc is initial.
  p_apfe_lote_alv-nm_lote = p_zpfe_lote_item-nm_lote.

  clear: it_itens_alv[].
  move p_it_lotes_item[] to it_itens[].

  loop at it_itens.
    move-corresponding it_itens to it_itens_alv.
    append it_itens_alv.
  endloop.

  call function 'Z_PFE_PSQ_ITENS'
    exporting
      p_lote_alv  = p_apfe_lote_alv
      p_pesquisar = space
    tables
      p_itens     = it_itens
      p_itens_alv = it_itens_alv
    exceptions
      sem_itens   = 1
      others      = 2.

  loop at it_itens_alv.
    vg_tabix = sy-tabix.
    it_itens_alv-ck_conferido = c_x.
    modify it_itens_alv index vg_tabix transporting ck_conferido.
  endloop.

  call screen 0002 starting at 05 05.

  vg_cancelado = vg_cancelado_0002.

  if vg_cancelado is initial.
    delete it_itens_alv where ck_conferido eq c_x.
    loop at it_itens_alv.
      delete p_it_lotes_item where nm_lote      eq it_itens_alv-nm_lote
                               and nm_lote_item eq it_itens_alv-nm_lote_item.
    endloop.
  endif.

endform.                    " SELECIONAR_ITENS

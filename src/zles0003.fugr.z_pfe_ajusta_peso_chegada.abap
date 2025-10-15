function z_pfe_ajusta_peso_chegada.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(P_ITEM) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(P_PESO) TYPE  J_1BNETQTY
*"  TABLES
*"      IT_LOTE_ITEM STRUCTURE  ZPFE_LOTE_ITEM
*"      IT_LOTES_ITEM_ALV STRUCTURE  ZPFE_LOTE_ITEM_ALV
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  data: p_lote_alv        type  zpfe_lote_alv,
        wa_zpfe_lote_item type zpfe_lote_item,
        wa_lotes_item_alv type zpfe_lote_item_alv,
        it_zpfe_chvid_ag  type table of zpfe_chvid_ag with header line,
        wa_zpfe_item_q    type zpfe_lote_item,
        wa_zpfe_item_p    type zpfe_lote_item,
        wa_zpfe_item_aux  type zpfe_lote_item,
        wa_zcte_ciot      type zcte_ciot,
        vl_perda_calc     type zpfe_lote_item-vl_conferido,
        vl_quebra_calc    type zpfe_lote_item-vl_conferido,
        vg_diferenca      type zpfe_lote_item-peso_origem,
        vg_toleravel      type zpfe_lote_item-peso_origem,
        vg_nm_lote_item   type zpfe_lote_item-nm_lote_item,
        vg_tabix          type sy-tabix,
        vg_color          type char04.

  vl_perda_calc   = 0.
  vl_quebra_calc  = 0.
  vg_nm_lote_item = 0.

  read table it_lotes_item_alv into wa_lotes_item_alv with key nm_lote      = p_lote
                                                               nm_lote_item = p_item.
  vg_tabix = sy-tabix.
  vg_color = wa_lotes_item_alv-rowcolor.
  loop at it_lote_item into wa_zpfe_item_aux.
    if wa_zpfe_item_aux-nm_lote_item gt vg_nm_lote_item.
      vg_nm_lote_item = wa_zpfe_item_aux-nm_lote_item.
    endif.
  endloop.

  vg_nm_lote_item = vg_nm_lote_item + 1.

  clear: wa_zpfe_item_aux.

  select single * into wa_zpfe_lote_item
    from zpfe_lote_item
   where nm_lote      = p_lote
     and nm_lote_item = p_item.

  select single * into wa_zcte_ciot
    from zcte_ciot
   where docnum eq wa_zpfe_lote_item-docnum.

  check not wa_zcte_ciot-peso_chegada is initial.

  if wa_zpfe_lote_item-peso_origem gt p_peso.
    vg_diferenca   = wa_zpfe_lote_item-peso_origem - p_peso.
    vl_quebra_calc = vg_diferenca * ( wa_zcte_ciot-vlr_unit_frete / 1000 ).

    vg_toleravel = wa_zpfe_lote_item-peso_origem * ( wa_zcte_ciot-perc_tolerancia / 100 ).
    "Tira Valor da Perda
    if vg_diferenca gt vg_toleravel.
      vl_perda_calc = ( ( vg_diferenca - vg_toleravel ) * wa_zcte_ciot-vlr_unit_merc  ).
    else.
      vl_perda_calc = 0.
    endif.
  else.
    vl_quebra_calc = 0.
    vl_perda_calc  = 0.
  endif.

  vl_quebra_calc = vl_quebra_calc * ( -1 ).
  vl_perda_calc  = vl_perda_calc  * ( -1 ).

  select single * into wa_zpfe_item_q
    from zpfe_lote_item
   where nm_lote  = p_lote
     and docnum   = wa_zpfe_lote_item-docnum
     and chvid    = '30'.

  if not sy-subrc is initial.
    read table it_lote_item with key nm_lote = p_lote
                                     docnum  = wa_zpfe_lote_item-docnum
                                     chvid   = '30'.
  endif.

  if not sy-subrc is initial.
    if vl_quebra_calc lt 0.
      move-corresponding wa_zpfe_lote_item to wa_zpfe_item_q.
      wa_zpfe_item_q-ck_conferido = wa_lotes_item_alv-ck_conferido.
      wa_zpfe_item_q-chvid = '30'.
      clear:
      wa_zpfe_item_q-vl_transacao,
      wa_zpfe_item_q-vl_pago_lote,
      wa_zpfe_item_q-vl_conferido,
      wa_zpfe_item_q-vl_diferenca,
      wa_zpfe_item_q-peso_importado,
      wa_zpfe_item_q-peso_origem,
      wa_zpfe_item_q-peso_chegada,
      wa_zpfe_item_q-dt_chegada.

      wa_zpfe_item_q-nm_lote_item = vg_nm_lote_item.
      vg_nm_lote_item             = vg_nm_lote_item + 1.
      wa_zpfe_item_q-vl_conferido = vl_quebra_calc.
      wa_zpfe_item_q-vl_diferenca = vl_quebra_calc.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zpfe_item_q-nm_lote_item
        importing
          output = wa_zpfe_item_q-nm_lote_item.
    endif.
  else.
    wa_zpfe_item_q-vl_conferido = vl_quebra_calc.
    wa_zpfe_item_q-vl_diferenca = vl_quebra_calc - wa_zpfe_item_q-vl_pago_lote.
  endif.

  read table it_lote_item with key nm_lote = p_lote
                                   docnum  = wa_zpfe_lote_item-docnum
                                   chvid   = '30'
                                   into wa_zpfe_item_aux.
  if sy-subrc is initial.
    if ( vl_quebra_calc eq 0 ) and
         ( wa_zpfe_item_aux-vl_transacao eq 0 ) and
         ( wa_zpfe_item_aux-vl_pago_lote eq 0 ).
      delete it_lote_item where nm_lote = p_lote
                            and docnum  = wa_zpfe_lote_item-docnum
                            and chvid   = '30'.
      delete it_lotes_item_alv where nm_lote = p_lote
                                and docnum   = wa_zpfe_lote_item-docnum
                                and chvid    = '30'.
    else.
      modify it_lote_item index sy-tabix from wa_zpfe_item_q.
    endif.
  elseif vl_quebra_calc lt 0.
    append wa_zpfe_item_q to it_lote_item.
  endif.

  select single * into wa_zpfe_item_p
    from zpfe_lote_item
   where nm_lote  = p_lote
     and docnum   = wa_zpfe_lote_item-docnum
     and chvid    = '31'.

  if not sy-subrc is initial.
    if vl_perda_calc lt 0.
      move-corresponding wa_zpfe_lote_item to wa_zpfe_item_p.
      wa_zpfe_item_p-ck_conferido = wa_lotes_item_alv-ck_conferido.
      wa_zpfe_item_p-chvid = '31'.
      clear:
      wa_zpfe_item_p-vl_transacao,
      wa_zpfe_item_p-vl_pago_lote,
      wa_zpfe_item_p-vl_conferido,
      wa_zpfe_item_p-vl_diferenca,
      wa_zpfe_item_p-peso_importado,
      wa_zpfe_item_p-peso_origem,
      wa_zpfe_item_p-peso_chegada,
      wa_zpfe_item_p-dt_chegada.

      wa_zpfe_item_p-nm_lote_item = vg_nm_lote_item.
      vg_nm_lote_item             = vg_nm_lote_item + 1.
      wa_zpfe_item_p-vl_conferido = vl_perda_calc.
      wa_zpfe_item_p-vl_diferenca = vl_perda_calc.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zpfe_item_p-nm_lote_item
        importing
          output = wa_zpfe_item_p-nm_lote_item.

    endif.
  else.
    wa_zpfe_item_p-vl_conferido = vl_perda_calc.
    wa_zpfe_item_p-vl_diferenca = vl_perda_calc - wa_zpfe_item_p-vl_pago_lote.
  endif.

  read table it_lote_item with key nm_lote = p_lote
                                   docnum  = wa_zpfe_lote_item-docnum
                                   chvid   = '31'
                                   into wa_zpfe_item_aux.
  if sy-subrc is initial.
    if ( vl_perda_calc eq 0 ) and
         ( wa_zpfe_item_aux-vl_transacao eq 0 ) and
         ( wa_zpfe_item_aux-vl_pago_lote eq 0 ).
      delete it_lote_item where nm_lote = p_lote
                            and docnum  = wa_zpfe_lote_item-docnum
                            and chvid   = '31'.
      delete it_lotes_item_alv where nm_lote = p_lote
                                and docnum   = wa_zpfe_lote_item-docnum
                                and chvid    = '31'.
    else.
      modify it_lote_item index sy-tabix from wa_zpfe_item_p.
    endif.
  elseif ( vl_perda_calc lt 0 ).
    append wa_zpfe_item_p to it_lote_item.
  endif.

  select * into table it_zpfe_chvid_ag
    from zpfe_chvid_ag
   where chvid_ch eq wa_zpfe_lote_item-chvid
     and chvid_ch_vinc ne '30'
     and chvid_ch_vinc ne '31'.

  loop at it_zpfe_chvid_ag.
    read table it_lote_item with key nm_lote = p_lote
                                     docnum  = wa_zpfe_lote_item-docnum
                                     chvid   = it_zpfe_chvid_ag-chvid_ch_vinc
                                     into wa_zpfe_item_aux.
    if sy-subrc is initial.
      wa_zpfe_item_aux-vl_conferido = wa_zpfe_item_aux-vl_pago_lote.
      modify it_lote_item index sy-tabix from wa_zpfe_item_aux transporting vl_conferido.
    endif.
  endloop.

  read table it_lote_item with key nm_lote = p_lote
                                   docnum  = wa_zpfe_lote_item-docnum
                                   chvid   = wa_zpfe_lote_item-chvid
                                   into wa_zpfe_item_aux.
  if sy-subrc is initial.
    wa_zpfe_item_aux-vl_conferido = wa_zpfe_item_aux-vl_pago_lote.
    modify it_lote_item index sy-tabix from wa_zpfe_item_aux transporting vl_conferido.
  endif.

  call function 'Z_PFE_PSQ_ITENS'
    exporting
      p_lote_alv  = p_lote_alv
      p_pesquisar = space
    tables
      p_itens     = it_lote_item
      p_itens_alv = it_lotes_item_alv
    exceptions
      sem_itens   = 1
      others      = 2.

  if not sy-subrc is initial.
    message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 raising error.
  endif.

  read table it_lotes_item_alv into wa_lotes_item_alv with key nm_lote      = p_lote
                                                               nm_lote_item = p_item.
  vg_tabix = sy-tabix.
  wa_lotes_item_alv-peso_chegada = p_peso.
  wa_lotes_item_alv-rowcolor     = vg_color.
  modify it_lotes_item_alv index vg_tabix from wa_lotes_item_alv transporting peso_chegada rowcolor.


endfunction.

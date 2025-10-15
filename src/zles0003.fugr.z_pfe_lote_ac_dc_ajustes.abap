function z_pfe_lote_ac_dc_ajustes.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_NM_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"  EXPORTING
*"     REFERENCE(P_ZPFE_LOTE) TYPE  ZPFE_LOTE
*"  EXCEPTIONS
*"      ERRO
*"----------------------------------------------------------------------

  data: p_tipcontabil   type ztipcontabil,
        it_lotes_item   type table of zpfe_lote_item with header line,
        wa_lotes_item   type zpfe_lote_item,
        vg_nm_lote_item type zpfe_numero_lote,
        vg_cancelado    type c length 1,
        vg_zuonr        type dzuonr .

  select single * into p_zpfe_lote
    from zpfe_lote
   where nm_lote eq p_nm_lote.

  if not sy-subrc is initial.
    message e013 with p_nm_lote raising error.
  endif.

  vg_nm_lote_item  = 0.

  select max( nm_lote_item ) into vg_nm_lote_item
    from zpfe_lote_item
   where nm_lote eq p_nm_lote.

  call function 'Z_PFE_TIPO_CONTAB'
    exporting
      p_dt_posicao  = p_zpfe_lote-dt_posicao
    importing
      p_tipcontabil = p_tipcontabil.

  if ( p_tipcontabil eq 'FC' ) and
     ( p_zpfe_lote-cd_adiministra is not initial ) and
     ( p_zpfe_lote-status eq 'I' ).

    select * into table it_lotes_item
      from zpfe_lote_item as i
     where i~status       eq 'C'
       and i~vl_diferenca ne 0
       and i~ck_utilizado eq space
       and exists ( select * from zpfe_lote as l
                     where l~nm_lote eq i~nm_lote
                       and l~cd_adiministra eq p_zpfe_lote-cd_adiministra ).

    perform selecionar_itens tables it_lotes_item using vg_cancelado.

    check vg_cancelado is initial.

    loop at it_lotes_item.
      clear: wa_lotes_item.
      vg_nm_lote_item = vg_nm_lote_item + 1.
      move-corresponding it_lotes_item to wa_lotes_item.
      wa_lotes_item-ck_utilizado = 'X'.
      modify zpfe_lote_item from wa_lotes_item.

      wa_lotes_item-nm_lote_or      = wa_lotes_item-nm_lote.
      wa_lotes_item-nm_lote_item_or = wa_lotes_item-nm_lote_item.
      wa_lotes_item-nm_lote         = p_nm_lote.
      wa_lotes_item-nm_lote_item    = vg_nm_lote_item.
      wa_lotes_item-dt_transacao    = sy-datum.
      wa_lotes_item-ck_utilizado    = 'X'.
      wa_lotes_item-vl_transacao    = wa_lotes_item-vl_diferenca.
      wa_lotes_item-vl_pago_lote    = wa_lotes_item-vl_diferenca.
      wa_lotes_item-vl_conferido    = 0.
      wa_lotes_item-vl_diferenca    = 0.
      modify zpfe_lote_item from wa_lotes_item.

      if ( not wa_lotes_item-belnr is initial ) and ( not wa_lotes_item-gjahr is initial ) and
         ( not wa_lotes_item-gjahr is initial ) and ( not p_zpfe_lote-bukrs is initial ).
        concatenate 'FR-' p_nm_lote into vg_zuonr.
        update bseg
           set zuonr = vg_zuonr
         where bukrs eq p_zpfe_lote-bukrs
           and belnr eq wa_lotes_item-belnr
           and gjahr eq wa_lotes_item-gjahr
           and buzei eq wa_lotes_item-nm_item_ctb_or.
      endif.

      p_zpfe_lote-vl_ac_dc_lote     = p_zpfe_lote-vl_ac_dc_lote + it_lotes_item-vl_diferenca.
    endloop.

    p_zpfe_lote-vl_confi_lote = p_zpfe_lote-vl_confi_lote + p_zpfe_lote-vl_ac_dc_lote.
    modify zpfe_lote from p_zpfe_lote.

  endif.

endfunction.

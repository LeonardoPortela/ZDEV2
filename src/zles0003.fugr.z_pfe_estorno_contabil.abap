function z_pfe_estorno_contabil.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(BELNR) TYPE  BELNR_D
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"----------------------------------------------------------------------

  data: it_lote_item type table of zpfe_lote_item with header line,
        wa_lote      type zpfe_lote.

  select single * into wa_lote
    from zpfe_lote
   where bukrs eq bukrs
     and belnr eq belnr
     and gjahr eq gjahr.

  if sy-subrc is initial.

    clear: wa_lote-belnr,
           wa_lote-gjahr,
           wa_lote-dt_belnr,
           wa_lote-hr_belnr,
           wa_lote-ds_usuario,
           wa_lote-obj_key,
           wa_lote-augbl,
           wa_lote-augdt.

    wa_lote-status = 'I'.
    modify zpfe_lote from wa_lote.

  else.

    select * into table it_lote_item
      from zpfe_lote_item as i
     where belnr eq belnr
       and gjahr eq gjahr
       and exists ( select *
                      from zpfe_lote as l
                     where l~nm_lote eq i~nm_lote
                       and l~bukrs   eq bukrs ).

    loop at it_lote_item.

      select single * into wa_lote
        from zpfe_lote
       where nm_lote eq it_lote_item-nm_lote.

      if sy-subrc is initial.
        if wa_lote-status eq 'C'.
          wa_lote-status = 'F'.
          modify zpfe_lote from wa_lote.
        endif.
      endif.

      clear: it_lote_item-belnr,
             it_lote_item-gjahr,
             it_lote_item-dt_belnr,
             it_lote_item-hr_belnr,
             it_lote_item-ds_usuario_ctb,
             it_lote_item-obj_key,
             it_lote_item-ck_conferido.
      it_lote_item-status = 'I'.
      modify zpfe_lote_item from it_lote_item.
    endloop.
  endif.

endfunction.

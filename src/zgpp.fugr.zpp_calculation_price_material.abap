function zpp_calculation_price_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"  EXPORTING
*"     REFERENCE(E_PRICE_USD) TYPE  CK_PVPRS_1
*"     REFERENCE(E_PRICE_BRL) TYPE  CK_PVPRS_1
*"----------------------------------------------------------------------

  data: wa_ckmlhd type ckmlhd,
        it_ckmlcr TYPE table of ckmlcr,
        ws_ckmlcr TYPE ckmlcr.

  clear: wa_ckmlhd, ws_ckmlcr, e_price_brl, e_price_usd.
  free: it_ckmlcr.

  check i_matnr is not initial and i_werks is not initial.

  select single *
  from ckmlhd
  into wa_ckmlhd
  where matnr eq i_matnr
  and bwkey eq i_werks.
  if sy-subrc eq 0.
*    "Seleciona preço atualizado do material.
    select *
    from ckmlcr
    into table it_ckmlcr
    where kalnr eq wa_ckmlhd-kalnr.
    if sy-subrc eq 0.
      sort it_ckmlcr descending by bdatj poper.

      read table it_ckmlcr into ws_ckmlcr with key kalnr = wa_ckmlhd-kalnr
                                                   waers = 'BRL'.
      if sy-subrc eq 0.
        e_price_brl = ws_ckmlcr-pvprs.
      endif.

      read table it_ckmlcr into ws_ckmlcr with key kalnr = wa_ckmlhd-kalnr
                                                   waers = 'USD'.
      if sy-subrc eq 0.
        e_price_usd = ws_ckmlcr-pvprs.
      endif.
    endif.
  endif.
endfunction.

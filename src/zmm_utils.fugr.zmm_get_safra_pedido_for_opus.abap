function zmm_get_safra_pedido_for_opus .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EKKO) TYPE  EKKO OPTIONAL
*"     REFERENCE(I_EKPO) TYPE  EKPO OPTIONAL
*"     REFERENCE(I_EKET) TYPE  EKET OPTIONAL
*"     REFERENCE(I_EBELN) TYPE  EKKO-EBELN OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_SAFRA) TYPE  EKET-CHARG
*"----------------------------------------------------------------------

  constants: lc_numbers(13) value ' 1234567890'.
  data vnum type i.
  data: lt_split_safra  type table of char40.

  clear: e_safra.

  if i_ekko is initial.

    check i_ebeln is not initial.

    select single *
      from ekko into @data(lwa_ekko)
     where ebeln eq @i_ebeln.

    check sy-subrc eq 0.

  else.

    lwa_ekko = i_ekko.

  endif.

  if i_eket is initial.

    select single *
      from eket into @data(lwa_eket)
     where ebeln eq @lwa_ekko-ebeln.

    check sy-subrc eq 0.

  else.

    lwa_eket = i_eket.

  endif.

  if i_ekpo is initial.

    select single *
      from ekpo into @data(lwa_ekpo)
     where ebeln eq @lwa_ekko-ebeln.

    check sy-subrc eq 0.

  else.

    lwa_ekpo = i_ekpo.

  endif.

  data(_fertilizantes) = abap_false.


  select single *
    from tvarvc into @data(lwa_tvarvc)
   where name = 'MAGGI_GR_FERTILIZANTES'
     and low  = @lwa_ekpo-matkl.

  if sy-subrc eq 0.
    _fertilizantes = abap_true.
  endif.
*"BUG172998
  if ( 'ZNB_ZFTE_YFTE' cs lwa_ekko-bsart or  _fertilizantes eq abap_true  ) and lwa_ekko-unsez is not initial.
    lwa_eket-charg = lwa_ekko-unsez.
  endif.

  if lwa_ekko-bsart = 'YFTE' and lwa_eket-charg is not initial.
    if lwa_eket-charg co lc_numbers.
      vnum = lwa_eket-charg.
      if vnum between 2020 and 2050.
      else.
        clear lwa_eket-charg.
      endif.
    else.
      clear lwa_eket-charg.
    endif.
  endif.

  if ( lwa_ekko-bsart = 'YFTE'       and
       lwa_ekko-ihrez is not initial and
       lwa_eket-charg is initial ).
    lwa_eket-charg = lwa_ekko-ihrez.
    if lwa_eket-charg co lc_numbers.
      vnum = lwa_eket-charg.
      if vnum between 2020 and 2050.
      else.
        lwa_eket-charg = sy-datum+0(4). "ano corrente
      endif.
    else.
      lwa_eket-charg = sy-datum+0(4). "ano corrente
    endif.
  endif.
*"BUG172998

  "Ajuste Determinação Safra Para Fertilizantes
  if ( _fertilizantes eq abap_true ) and ( lwa_ekko-unsez is initial ) and lwa_ekko-bsart ne 'YFTE' .
    clear: lt_split_safra[].

    split lwa_eket-charg at '/' into table lt_split_safra.

    if lines( lt_split_safra ) eq 2.
      read table lt_split_safra into data(lwa_safra) index 2.
      if strlen( lwa_safra ) eq 4.
        lwa_eket-charg = lwa_safra.
      endif.
    endif.
  endif.
  "Ajuste Determinação Safra Para Fertilizantes

  if lwa_eket-charg is initial and 'ZNB_ZFTE' cs lwa_ekko-bsart.
    exit.
  endif.

  e_safra = lwa_eket-charg.


endfunction.

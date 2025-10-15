function zpp_get_value_ksl_acdoca.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_RLDNR) TYPE  FINS_LEDGER
*"     VALUE(I_RBUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(I_FISCYEARPER) TYPE  JAHRPER OPTIONAL
*"     VALUE(I_MATNR) TYPE  MATNR OPTIONAL
*"     VALUE(I_BWKEY) TYPE  BWKEY
*"     VALUE(I_BELNR) TYPE  BELNR_D
*"     VALUE(I_MJAHR) TYPE  MJAHR
*"     VALUE(I_POPER) TYPE  POPER OPTIONAL
*"  EXPORTING
*"     VALUE(E_KSL) TYPE  FINS_VKCUR12
*"     VALUE(E_HSL) TYPE  FINS_VKCUR12
*"----------------------------------------------------------------------

  data: zva_awkey type awkey.


  check i_rldnr is not initial
*  and   i_rbukrs is not initial
*  and   i_fiscyearper is not initial
  and   i_matnr is not initial
  and   i_bwkey is not initial
  and   i_belnr is not initial
  and   i_mjahr is not initial.

  clear: zva_awkey.
  zva_awkey = |{ i_belnr }{ i_mjahr }|.
  I_FISCYEARPER = |{ i_mjahr }{ i_poper }|.


  select single * from bkpf into @data(ws_bkpf)
    where awkey eq @zva_awkey.
  if sy-subrc eq 0.
    select single * from acdoca_m_extract into @data(wa_acdoca)
    where belnr eq @ws_bkpf-belnr
      and rldnr eq @i_rldnr
*      and rbukrs eq i_rbukrs
*      and fiscyearper eq @i_fiscyearper
      and ryear eq @i_mjahr
      and poper eq @i_poper
      and matnr eq @i_matnr
      and bwkey eq @i_bwkey.
    if sy-subrc eq 0.
      if wa_acdoca-ksl < 0.
        e_ksl = abs( wa_acdoca-ksl ).
      else.
        e_ksl = wa_acdoca-ksl.
      endif.

      if wa_acdoca-hsl < 0.
        e_hsl = abs( wa_acdoca-hsl ).
      else.
        e_hsl = wa_acdoca-hsl.
      endif.
    endif.
  endif.















endfunction.

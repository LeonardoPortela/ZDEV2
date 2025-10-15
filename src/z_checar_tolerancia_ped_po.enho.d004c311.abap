"Name: \PR:SAPLMEPO\FO:CUSTOMER_DATA_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 Z_CHECAR_TOLERANCIA_PED_PO.
*
*  data: wa_ekko    type ekko,
*        dif_rlwrt  type zmmt0113-vl_toler,
*        tot_rlwrt  type ekko-rlwrt,
*        w_zmmt0113 type zmmt0113,
*        v_toler    type zmmt0113-vl_toler.
*
*
*  select single *
*    from ekko
*    into wa_ekko
*    where ebeln = ekko-ebeln.
*
*  CHECK sy-subrc = 0.
*
*  select single *
*    from zmmt0113
*    into w_zmmt0113
*    where bukrs = ekko-bukrs
*    and   bsart = ekko-bsart.
*
*  if sy-subrc = 0.
*    tot_rlwrt = 0.
*    loop at pot.
*      add pot-netwr to tot_rlwrt.
*    endloop.
*    dif_rlwrt = ( tot_rlwrt - ekko-rlwrt ) * 100.
*    if  dif_rlwrt lt 0.
*      dif_rlwrt = dif_rlwrt * -1.
*    endif.
*    if ekko-waers = 'BRL'.
*      v_toler = w_zmmt0113-vl_toler.
*    else.
*      v_toler = w_zmmt0113-vl_toler_us.
*    endif.
*    if  dif_rlwrt le v_toler and dif_rlwrt > 0.
*      if wa_ekko-frgke = '2'.
*        ekko-frgke = '2'.
*        ekko-frgrl = ' '.
*        ekko-frgzu = wa_ekko-frgzu.
*      endif.
*    endif.
*  endif.

ENDENHANCEMENT.

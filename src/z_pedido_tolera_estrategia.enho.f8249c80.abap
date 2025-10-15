"Name: \FU:ME_REL_STRATEGIE_EKKO\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_TOLERA_ESTRATEGIA.
*
    data: dif_rlwrt  type zmmt0113-vl_toler,
          w_zmmt0113 type zmmt0113,
          w_zmmt0035 type zmmt0035,
          w_ekko     type ekko,
          vgnetw     type cekko-gnetw,
          v_toler    type zmmt0113-vl_toler.

    move i_cekko_new to cekko.
    move i_cekko_old to *cekko.
    move i_wkurs to bwkurs.
    move i_wkurs_old to bwkurs_old.
    move cekko-bedat to bbedat.
    clear s_kzfae.

    if i_lpot ne space.
      e_frgrl = space.
      exit.
    endif.

    select single *
      from zmmt0113
      into w_zmmt0113
      where bukrs = *cekko-bukrs
      and   bsart = *cekko-bsart.

    if sy-subrc = 0.
      clear v_toler.
      vgnetw = cekko-gnetw. "valor da aprovação (novo)

      if *cekko-waers = 'BRL'.
        v_toler = w_zmmt0113-vl_toler.
      else.
        v_toler = w_zmmt0113-vl_toler_us.
      endif.

      if cekko-submi is not initial.
        select single *
          from zmmt0035
          into  w_zmmt0035
          where nro_sol_cp = *cekko-submi.
        if sy-subrc eq 0.
          if w_zmmt0035-gnetw gt 0.
            vgnetw = w_zmmt0035-gnetw. "valor da ultima modificaçao do total do pedido + impostos na zmm0149
          endif.
          if w_zmmt0035-ebeln is not initial.
            select single *
              from ekko
              into w_ekko
              where ebeln = w_zmmt0035-ebeln.
          endif.
        endif.
      endif.

      if w_ekko-frgke = '2'. "Se esta aprovado ( pedido mãe ZMM0149), verifica se reinicia a estrategia
********************* Novo   - Antigo
        dif_rlwrt = ( vgnetw - *cekko-gnetw  ) * 100.

        dif_rlwrt = abs( dif_rlwrt ).

        if  dif_rlwrt le v_toler.
          clear xchange.
          if not *cekko is initial.
            e_frgst = i_frgst.
            e_frggr = i_frggr.
            e_frgkz = i_frgkz.
            e_frgrl = i_frgrl.
            e_frgzu = i_frgzu.
          endif.
          exit.
        endif.
      endif.
    endif.

ENDENHANCEMENT.

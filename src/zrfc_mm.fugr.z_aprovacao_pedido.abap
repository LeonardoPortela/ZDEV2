function z_aprovacao_pedido.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NUMERO_PEDIDO) TYPE  EBELN
*"     REFERENCE(I_VALOR_TOTAL_SOL) TYPE  GNETW
*"     REFERENCE(I_GRAVA) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(R_NOVA_APROVACAO) TYPE  BOOLEAN
*"----------------------------------------------------------------------
  data: wa_ekko    type ekko,
        dif_rlwrt  type zmmt0113-vl_toler,
        w_zmmt0113 type zmmt0113,
        w_zmmt0035 type zmmt0035,
        v_toler    type zmmt0113-vl_toler.

  r_nova_aprovacao = abap_false.

  select single *
     from zmmt0035
    into w_zmmt0035
    where ebeln = i_numero_pedido.

  check sy-subrc = 0.

  select single *
      from ekko
      into wa_ekko
      where ebeln = i_numero_pedido.

  check sy-subrc = 0.

  check  wa_ekko-frgke = '2'. "Se não esta aprovado não é necessario verificar

  clear v_toler.
  select single *
    from zmmt0113
    into w_zmmt0113
    where bukrs = wa_ekko-bukrs
    and   bsart = wa_ekko-bsart.

  if sy-subrc eq 0.
    if wa_ekko-waers = 'BRL'.
      v_toler = w_zmmt0113-vl_toler.
    else.
      v_toler = w_zmmt0113-vl_toler_us.
    endif.

    dif_rlwrt = i_valor_total_sol - wa_ekko-rlwrt.

    if  dif_rlwrt gt v_toler.
      r_nova_aprovacao = abap_true.
    endif.

    if i_grava = 'X'.
      update zmmt0035 set gnetw = i_valor_total_sol
      where nro_sol_cp = w_zmmt0035-nro_sol_cp.
      commit work.
    endif.
  endif.

endfunction.

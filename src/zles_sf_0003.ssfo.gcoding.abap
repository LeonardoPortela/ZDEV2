data: it_zlest0056 type table of zlest0056,
      wa_zlest0056 type zlest0056,
      it_zlest0057 type table of zlest0057,
      wa_zlest0057 type zlest0057,
      it_zlest0061 type table of zlest0061,
      it_zlest0061_aux type table of zlest0061,
      wa_zlest0061 type zlest0061,
      wa_zlest0061_aux type zlest0061,
      it_zlest0063     type table of zlest0063,
      wa_zlest0063 type zlest0063,
      it_zlest0060 type table of zlest0060,
      wa_zlest0060 type zlest0060,
      it_zlest0063_aux type table of zlest0063,
      wa_zlest0063_aux type zlest0063.

data: it_j_1bnfdoc      type table of j_1bnfdoc,
      wa_j_1bnfdoc      type j_1bnfdoc,
      it_j_1bnfe_active type table of j_1bnfe_active,
      wa_j_1bnfe_active type j_1bnfe_active,
      it_t001           type table of t001,
      wa_t001           type t001,
      it_lfa1           type table of lfa1,
      wa_lfa1           type lfa1,
      it_kna1           type table of kna1,
      wa_kna1           type kna1,
      it_adrc           type table of adrc,
      wa_adrc           type adrc,
      it_j_1bbranch     type table of j_1bbranch,
      wa_j_1bbranch     type j_1bbranch.


data: vl_lifnr      type lfa1-lifnr.

"Viagem.
select * from zlest0056
  into table it_zlest0056
where bukrs      = i_bukrs
  and werks      = i_werks
  and ano_viagem = i_ano_viagem
  and nr_viagem  = i_nr_viagem.

check not it_zlest0056[] is initial.

select * from zlest0063
  into table it_zlest0063
  for all entries in it_zlest0056
where bukrs      = it_zlest0056-bukrs
  and werks      = it_zlest0056-werks
  and ano_viagem = it_zlest0056-ano_viagem
  and nr_viagem  = it_zlest0056-nr_viagem.

select * from zlest0061
  into table it_zlest0061
  for all entries in it_zlest0063
where bukrs      = it_zlest0063-bukrs
  and werks      = it_zlest0063-werks
  and ano_viagem = it_zlest0063-ano_viagem
  and nr_viagem  = it_zlest0063-nr_viagem
  and embarcacao = it_zlest0063-embarcacao
  and nome_emb   = it_zlest0063-nome_emb
  and auart      in ('ZTAG','ZTAB','ZTAM','ZTAF').

select * from zlest0060
  into table it_zlest0060
  for all entries in it_zlest0061
where bukrs      = it_zlest0061-bukrs
  and werks      = it_zlest0061-werks
  and ano_viagem = it_zlest0061-ano_viagem
  and nr_viagem  = it_zlest0061-nr_viagem
  and embarcacao = it_zlest0061-embarcacao
  and nome_emb   = it_zlest0061-nome_emb.


select * from j_1bbranch
 into table it_j_1bbranch
  for all entries in it_zlest0056
where bukrs  eq it_zlest0056-bukrs
  and branch eq it_zlest0056-werks.

select * from zlest0061
  into table it_zlest0061_aux
  for all entries in it_zlest0063
where bukrs      = it_zlest0063-bukrs
  and werks      = it_zlest0063-werks
  and ano_viagem = it_zlest0063-ano_viagem
  and nr_viagem  = it_zlest0063-nr_viagem.

clear: wa_saida_cabecalho.

loop at it_zlest0056 into wa_zlest0056.

  wa_saida_cabecalho-nr_viagem  = wa_zlest0056-nr_viagem.
  wa_saida_cabecalho-ano_viagem = wa_zlest0056-ano_viagem.
  read table it_zlest0061_aux into wa_zlest0061_aux index 1.
  wa_saida_cabecalho-dt_prevista = wa_zlest0061_aux-dt_fatura.
  "WA_SAIDA_CABECALHO-DT_PREVISTA = WA_ZLEST0056-DT_PREVISTA.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = wa_zlest0056-werks
    importing
      output = vl_lifnr.

  select single * from lfa1 into wa_lfa1 where lifnr eq vl_lifnr.
  wa_saida_cabecalho-name1_emi = wa_lfa1-name1.
  wa_saida_cabecalho-stcd1     = wa_lfa1-stcd1.

  select single * from adrc into wa_adrc where addrnumber eq wa_lfa1-adrnr.
  wa_saida_cabecalho-street     = wa_adrc-street.
  wa_saida_cabecalho-house_num1 = wa_adrc-house_num1.
  wa_saida_cabecalho-city1      = wa_adrc-city1.
  wa_saida_cabecalho-region     = wa_adrc-region.

  read table it_j_1bbranch into wa_j_1bbranch with key bukrs = wa_zlest0056-bukrs
                                                       branch = wa_zlest0056-werks.
  "wa_saida_cabecalho-stcd1      = wa_j_1bbranch-stcd1.
  wa_saida_cabecalho-state_insc = wa_j_1bbranch-state_insc.

  clear: wa_lfa1.
  select single * from lfa1 into wa_lfa1 where lifnr eq wa_zlest0056-po_embarque.
  wa_saida_cabecalho-po_embarque = wa_lfa1-name1.
  wa_saida_cabecalho-ort01_emb   = wa_lfa1-ort01.
  select single * from kna1 into wa_kna1 where kunnr eq wa_zlest0056-po_destino.
  wa_saida_cabecalho-po_destino  = wa_kna1-name1.
  wa_saida_cabecalho-ort01_dest  = wa_kna1-ort01.

  read table it_zlest0063 into wa_zlest0063 with key bukrs      = wa_zlest0056-bukrs
                                                     werks      = wa_zlest0056-werks
                                                     ano_viagem = wa_zlest0056-ano_viagem
                                                     nr_viagem  = wa_zlest0056-nr_viagem
                                                     embarcacao = 'E'.



  wa_saida_cabecalho-nome_emb = wa_zlest0063-nome_emb.
endloop.


data: total_peso    type zlest0060-peso_fiscal,
      total_produto type zlest0060-netwr,
      total_frete   type zlest0061-vlr_brl,
      tabix         type sy-tabix.

data: total_peso_sum    type zlest0060-peso_fiscal,
      total_produto_sum type zlest0060-netwr,
      total_frete_sum   type zlest0061-vlr_brl.


clear: wa_zlest0063.


loop at it_zlest0063 into wa_zlest0063 where embarcacao ne 'E'.

 tabix = sy-tabix.

  loop at it_zlest0060 into wa_zlest0060 where nome_emb eq wa_zlest0063-nome_emb.

    if wa_zlest0060-peso_liq_ret is NOT INITIAL.
      total_peso    = total_peso    + wa_zlest0060-peso_liq_ret.
      total_produto = total_produto + wa_zlest0060-vlr_liq_ret.
    else.
      total_peso    = total_peso    + wa_zlest0060-peso_fiscal.
      total_produto = total_produto + wa_zlest0060-netwr.
    ENDIF.

  endloop.

 loop at it_zlest0061 into wa_zlest0061 where embarcacao = wa_zlest0060-embarcacao
                                          and  nome_emb   = wa_zlest0060-nome_emb.
    total_frete = total_frete + wa_zlest0061-vlr_brl.
 endloop.

if not ( total_peso is initial ) or not ( total_produto is initial ).

  wa_saida_balsa-nome_emb      = wa_zlest0063-nome_emb.
  wa_saida_balsa-total_peso    = total_peso.
  wa_saida_balsa-total_produto = total_produto.
  wa_saida_balsa-total_frete   = total_frete.

  append wa_saida_balsa to it_saida_balsa.

  total_peso_sum     =  total_peso_sum    + wa_saida_balsa-total_peso.
  total_produto_sum  =  total_produto_sum + wa_saida_balsa-total_produto.
  total_frete_sum    =  total_frete_sum   + wa_saida_balsa-total_frete.



  clear: total_peso, total_produto, total_frete.
endif.

 delete it_zlest0063 index tabix.

 clear: tabix, wa_saida_balsa.
endloop.

if ( not total_peso_sum   is initial ) or not ( total_produto_sum is initial ) and not ( total_frete_sum  is initial ).

wa_saida_total-total_peso_sum    = total_peso_sum.
wa_saida_total-total_produto_sum = total_produto_sum.
wa_saida_total-total_frete_sum   = total_frete_sum.


clear:
total_peso_sum,
total_produto_sum,
total_frete_sum.

endif.

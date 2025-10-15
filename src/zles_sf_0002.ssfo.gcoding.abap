data: it_zlest0056 type table of zlest0056,
      wa_zlest0056 type zlest0056,

      it_zlest0061 type table of zlest0061,
      wa_zlest0061 type zlest0061,

      it_zlest0060 type table of zlest0060,
      wa_zlest0060 type zlest0060,

      it_zlest0063     type table of zlest0063,
      wa_zlest0063 type zlest0063,
      it_zlest0063_aux type table of zlest0063,
      wa_zlest0063_aux type zlest0063,
      it_makt          type table of makt,
      wa_zlest0053     type zlest0053.


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
      wa_j_1bbranch     type j_1bbranch,
      wa_makt           type makt.


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

select * from j_1bbranch
  into table it_j_1bbranch
  for all entries in it_zlest0056
where bukrs  eq it_zlest0056-bukrs
  and branch eq it_zlest0056-werks.

clear: wa_saida_cabecalho.

select * from zlest0061
  into table it_zlest0061
where bukrs      = i_bukrs
  and werks      = i_werks
  and ano_viagem = i_ano_viagem
  and nr_viagem  = i_nr_viagem.


sort: it_zlest0061 by dt_fatura ascending.


loop at it_zlest0056 into wa_zlest0056.

  wa_saida_cabecalho-nr_viagem   = wa_zlest0056-nr_viagem.
  wa_saida_cabecalho-ano_viagem  = wa_zlest0056-ano_viagem.
  read table it_zlest0061 into wa_zlest0061 index 1.
  wa_saida_cabecalho-dt_prevista = wa_zlest0061-dt_fatura.

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

refresh: it_zlest0061[].
clear: wa_zlest0061.

select * from zlest0061
  into table it_zlest0061
where bukrs      = i_bukrs
  and werks      = i_werks
  and ano_viagem = i_ano_viagem
  and nr_viagem  = i_nr_viagem.

check not it_zlest0061[] is initial.

  select * from zlest0060
    into table it_zlest0060
    for all entries in it_zlest0061
  where bukrs       eq it_zlest0061-bukrs
    and werks       eq it_zlest0061-werks
    and nr_viagem   eq it_zlest0061-nr_viagem
    and ano_viagem  eq it_zlest0061-ano_viagem
    and embarcacao  eq it_zlest0061-embarcacao
    and nome_emb    eq it_zlest0061-nome_emb
    and cl_codigo   eq it_zlest0061-cl_codigo
    and nr_dco      eq it_zlest0061-nr_dco
    and safra       eq it_zlest0061-safra.

select * from makt
  into table it_makt
  for all entries in it_zlest0061
where matnr eq it_zlest0061-cod_material
  and spras = sy-langu.

select * from j_1bnfdoc
  into table it_j_1bnfdoc
  for all entries in it_zlest0061
where docnum eq it_zlest0061-docnum.

select * from j_1bnfe_active
  into table it_j_1bnfe_active
  for all entries in it_j_1bnfdoc
where docnum eq it_j_1bnfdoc-docnum.


clear: wa_zlest0056, wa_kna1, wa_lfa1.

loop at it_j_1bnfdoc into wa_j_1bnfdoc.
*
  read table it_j_1bnfe_active into wa_j_1bnfe_active with key docnum = wa_j_1bnfdoc-docnum.
  if ( sy-subrc ne 0 ).
    continue.
  else.

    read table it_zlest0061 into wa_zlest0061 with key docnum = wa_j_1bnfdoc-docnum.
    wa_saida-docnum    = wa_zlest0061-docnum.
    wa_saida-tax_dolar = wa_zlest0061-tax_dolar.
    wa_saida-nome_emb  = wa_zlest0061-nome_emb.
    wa_saida-class     = wa_zlest0061-tp_class.

    CASE wa_zlest0061-tp_class.
      WHEN 'CO'.
        wa_zlest0061-tp_class = 'CO-Convencional'.
      WHEN 'R1'.
        wa_zlest0061-tp_class = 'R1-RR'.
      WHEN 'R2'.
        wa_zlest0061-tp_class = 'R2-RR2'.
    ENDCASE.

    read table it_makt into wa_makt with key matnr = wa_zlest0061-cod_material.
    wa_saida-maktx = wa_makt-maktx(15).



    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_j_1bnfe_active-nfnum9
      importing
        output = wa_saida-nfenum.

    concatenate wa_saida-nfenum '-' wa_saida-series into wa_saida-nfe_serie.

    "Somar o Peso Vinculado / Valor Vinculado
    loop at it_zlest0060 into wa_zlest0060 where cl_codigo eq wa_zlest0061-cl_codigo
                                             and nr_dco    eq wa_zlest0061-nr_dco
                                             and safra     eq wa_zlest0061-safra
                                             and nome_emb  eq wa_zlest0061-nome_emb.


      IF wa_zlest0061-rm_codigo is NOT INITIAL.
        CHECK wa_zlest0060-rm_codigo EQ wa_zlest0061-rm_codigo.
      ENDIF.

      CHECK wa_zlest0060-operacao  EQ wa_zlest0061-operacao.

      wa_saida-eudr = wa_zlest0060-eudr. "// US-165460 WBARBOSA 04/02/2025

      if wa_zlest0060-peso_liq_ret is NOT INITIAL.
        wa_saida-peso     = wa_saida-peso      + wa_zlest0060-peso_liq_ret.
        wa_saida-vlr_vinc = wa_saida-vlr_vinc  + wa_zlest0060-vlr_liq_ret.
      else.
        wa_saida-peso     = wa_saida-peso      + wa_zlest0060-peso_fiscal.
        wa_saida-vlr_vinc = wa_saida-vlr_vinc  + wa_zlest0060-netwr.
      endif.

      clear: wa_zlest0060.

    endloop.

    wa_saida-vlr_unit  = ( wa_zlest0061-vlr_brl / wa_saida-peso ) * 1000.
    wa_saida-vlr_frete = wa_zlest0061-vlr_brl.

    clear: vl_lifnr.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zlest0061-werks
      importing
        output = vl_lifnr.

    select single * from lfa1 into wa_lfa1 where lifnr = vl_lifnr.
    wa_saida-name1 = wa_lfa1-name1.

    select single * from kna1 into wa_kna1 where kunnr = wa_zlest0061-cl_codigo.
    wa_saida-tomador = wa_kna1-name1.
    wa_saida-stcd1   = wa_kna1-stcd1.

    wa_saida_total-total_peso    = wa_saida_total-total_peso + wa_saida-peso.
    wa_saida_total-total_produto = wa_saida_total-total_produto +  wa_saida-vlr_vinc.
    wa_saida_total-total_frete   = wa_saida_total-total_frete + wa_saida-vlr_frete.

    append wa_saida to it_saida.

    clear: wa_saida.
  endif.

endloop.


data: total_peso    type zlest0060-peso_fiscal,
      total_produto type zlest0060-netwr,
      tabix         type sy-tabix.

clear: wa_zlest0063, wa_zlest0060.

loop at it_zlest0063 into wa_zlest0063 where embarcacao ne 'E'.
 tabix = sy-tabix.

  loop at it_zlest0060 into wa_zlest0060 where nome_emb eq wa_zlest0063-nome_emb.
      if wa_zlest0060-peso_liq_ret is NOT INITIAL.
        total_peso    = total_peso     + wa_zlest0060-peso_liq_ret.
        total_produto = total_produto  + wa_zlest0060-vlr_liq_ret.
      else.
        total_peso    = total_peso     + wa_zlest0060-peso_fiscal.
        total_produto = total_produto  + wa_zlest0060-netwr.
      endif.

      clear: wa_zlest0060.
  endloop.

  select single * from zlest0053 into wa_zlest0053 where nome eq wa_zlest0063-nome_emb.
  wa_saida_balsa-nome_emb      = wa_zlest0053-apelido.
  wa_saida_balsa-total_peso    = total_peso.
  wa_saida_balsa-total_produto = total_produto.

 if not ( total_peso is initial ) or not ( total_produto is initial ).
  append wa_saida_balsa to it_saida_balsa.

endif.

 delete it_zlest0063 index tabix.

 clear: tabix, wa_saida_balsa, total_peso, total_produto.
endloop.

sort: it_saida by nfe_serie ascending.

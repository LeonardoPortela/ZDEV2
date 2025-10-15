class ZCL_UTIL_INVOCE definition
  public
  final
  create public .

public section.

  class-methods GET_DATA_PROVISIONAL
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0089_T .
  class-methods GET_DATA_INVOICE
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0091_T .
  class-methods GET_DATA_NOTA_INVOICE
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0091_T .
  class-methods SAVE_MEMOR_DATA_PROVISIONAL
    importing
      value(I_DATA) type ZSDE0089_T optional
    returning
      value(E_RETURN) type CHAR01 .
  class-methods GET_DATA_INSTRUCAO
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0092_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UTIL_INVOCE IMPLEMENTATION.


  method GET_DATA_INSTRUCAO.
  endmethod.


  method GET_DATA_INVOICE.













  endmethod.


  method GET_DATA_NOTA_INVOICE.
  endmethod.


  method get_data_provisional.

    data: rg_bukrs  type range of bukrs,
          rg_kunnr  type range of kunnr,
          rg_safra  type range of gjahr,
          rg_CONTR  type range of bstkd,
          rg_PROV   type range of zde_provisinal,
          rg_LOTE   type range of CHARG_d,
          rg_DTTKUP type range of sy-datum,
          rg_DTPROV type range of sy-datum.

    data: wa_provisional    type zsde0089,
          vg_nr_provisional type zsdt0166-nr_provisional.


    free: it_result.

    if i_param is not initial.
      loop at i_param into data(wa_param).
        case wa_param-fieldnm.
          when 'BUKRS'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_bukrs.
          when 'KUNNR'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_kunnr.
          when 'SAFRA'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_safra.
          when 'CONTR'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_contr.
          when 'PROV'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_prov.
          when 'LOTE'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_lote.
          when 'DTTKUP'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_dttkup.
          when 'DTPROV'.
            append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to rg_dtprov.
        endcase.
      endloop.
    endif.

    select *
    from zsdt0166
    into table @data(it_zsdt0166)
    where empresa in @rg_bukrs
    and safra in @rg_safra
    and contrato in @rg_contr
    and kunnr    in @rg_kunnr
    and nr_provisional ne @space.

    if sy-subrc is initial.
      select *
      from kna1
      into table @data(it_kna1)
        for all entries in @it_zsdt0166
      where kunnr eq @it_zsdt0166-kunnr.

      if  sy-subrc = 0.
        select  *
          from adrc
          into table @data(it_ADRC)
          for all entries in @it_kna1
         where addrnumber eq @it_kna1-adrnr
           and date_to    >= @sy-datum.
      endif.

      "Seleção dados do contrato/solicição/dados adiantamento.
      select a~nro_sol_ov, a~vkorg, a~bstkd as contrato, b~adiant, b~dmbtr as valor, b~nr_provis_inv, c~belnr, c~dmbtr, c~wrbtr, d~bldat
      from zsdt0051 as a
      left join zsdt0054 as b on b~nro_sol_ov eq a~nro_sol_ov
      inner join bseg as c on c~belnr eq b~adiant and c~bukrs eq a~vkorg
      inner join bkpf as d on d~belnr eq c~belnr and d~gjahr eq c~gjahr
      into table @data(it_dados_adiant)
        for all entries in @it_zsdt0166
      where a~bstkd eq  @it_zsdt0166-contrato
        and d~stblg eq @space.
      sort it_dados_adiant by bldat.

      "Dados salvo rel.provisional.
      select nr_provisional, contrato, kunnr, bukrs, dt_ret_corretora, observacao
      from zsdt0409
      into table @data(it_zsdt0409)
        for all entries in @it_zsdt0166
        where nr_provisional eq @it_zsdt0166-nr_provisional
          and contrato eq @it_zsdt0166-contrato
          and bukrs    eq @it_zsdt0166-empresa.
    endif.


    if it_zsdt0166[] is not initial.
      data(it_zsdt0166_aux) = it_zsdt0166.
      sort it_zsdt0166 by contrato nr_provisional.
      delete adjacent duplicates from it_zsdt0166 comparing contrato nr_provisional.

      sort it_dados_adiant by vkorg contrato nr_provis_inv.

      loop at it_zsdt0166 into data(wa_zsdt0166).
        move-corresponding wa_zsdt0166 to wa_provisional.
        wa_provisional-dt_takeup =  wa_zsdt0166-data_takeup.
        wa_provisional-mes_takeup = |{ wa_zsdt0166-data_takeup+4(2) }|.
        wa_provisional-bukrs =  wa_zsdt0166-empresa.
        wa_provisional-nr_provisional =  wa_zsdt0166-nr_provisional.


        read table it_kna1 into data(w_kna1) with key kunnr = wa_zsdt0166-kunnr.
        if sy-subrc is initial.
          read table it_ADRC into data(w_ADRC) with key addrnumber = w_kna1-adrnr.
          if sy-subrc is initial.
            concatenate  w_adrc-name1 w_adrc-name2 into wa_provisional-desc_cliente separated by space.
          endif.
        endif.
        loop at it_zsdt0166_aux assigning field-symbol(<ws_zsdt0166>) where contrato eq wa_zsdt0166-contrato and nr_provisional eq wa_zsdt0166-nr_provisional.
          if <ws_zsdt0166>-peso_lote > 0.
            add <ws_zsdt0166>-peso_lote to wa_provisional-qtd_ton.
          endif.

          if <ws_zsdt0166>-valor_total > 0.
            add <ws_zsdt0166>-valor_total to wa_provisional-val_total.
          endif.

          if <ws_zsdt0166>-valor_antec > 0.
            add <ws_zsdt0166>-valor_antec to wa_provisional-val_adiant.
          endif.
        endloop.

        if wa_provisional-qtd_ton is not initial.
          "Converter em tonelada.
          wa_provisional-qtd_ton = ( wa_provisional-qtd_ton / 1000 ).
        endif.

        if wa_provisional-val_adiant is not initial.
          wa_provisional-per_adiant = ( wa_provisional-val_adiant / wa_provisional-val_total ) * 100.
        endif.

        if wa_provisional-val_adiant is not initial.
          wa_provisional-previsao_cad = ( wa_provisional-val_total - wa_provisional-val_adiant ).
        endif.

        wa_provisional-dt_emissao = sy-datum.
        wa_provisional-qdt_dia_takeup_e = ( wa_zsdt0166-data - wa_zsdt0166-data_takeup ).

        loop at it_dados_adiant into data(wa_dados_adiant) where contrato = wa_zsdt0166-contrato
                                                             and    vkorg = wa_zsdt0166-empresa.

          clear: vg_nr_provisional.
          vg_nr_provisional = wa_dados_adiant-nr_provis_inv+0(4).

          if vg_nr_provisional eq wa_zsdt0166-nr_provisional.

            if wa_provisional-doc_cont_pi is initial.
              wa_provisional-doc_cont_pi = wa_dados_adiant-belnr.
            else.
              wa_provisional-doc_cont_pi = |{ wa_provisional-doc_cont_pi }/{ wa_dados_adiant-belnr }|.
            endif.

            if wa_provisional-nro_sol_ov is initial and wa_dados_adiant-nro_sol_ov ne wa_provisional-nro_sol_ov.
              wa_provisional-nro_sol_ov = wa_dados_adiant-nro_sol_ov.
            else.
              wa_provisional-nro_sol_ov = |{ wa_provisional-nro_sol_ov }/{ wa_dados_adiant-nro_sol_ov }|.
            endif.

            if wa_dados_adiant-wrbtr is not initial.
              add wa_dados_adiant-wrbtr to wa_provisional-recebifo_usd.
            endif.

            wa_provisional-a_receber_usd = ( wa_provisional-val_adiant - wa_provisional-recebifo_usd ).

            wa_provisional-dt_pgto_pi = wa_dados_adiant-bldat.
            wa_provisional-ano = |{ wa_dados_adiant-bldat+0(4) }|.
          endif.
        endloop.

        if wa_provisional-dt_ret_corretora is not initial.
          wa_provisional-dia_emissao_ret = ( wa_provisional-dt_ret_corretora - wa_provisional-dt_emissao ).
          wa_provisional-dia_ret_pgto = ( wa_provisional-dt_pgto_pi - wa_provisional-dt_ret_corretora ).
        endif.

        wa_provisional-qtd_tons_rec = ( ( wa_provisional-per_adiant / 100 ) * wa_provisional-qtd_ton ).

        wa_provisional-qtd_receber =  ( ( wa_provisional-per_adiant / 100 ) * wa_provisional-qtd_ton ).

        read table it_zsdt0409 into data(wa_zsdt0409) with key nr_provisional = wa_zsdt0166-nr_provisional
                                                               contrato       = wa_zsdt0166-contrato
                                                               bukrs          = wa_zsdt0166-empresa.
        if sy-subrc eq 0.
          wa_provisional-dt_ret_corretora = wa_zsdt0409-dt_ret_corretora.
          wa_provisional-observacao       = wa_zsdt0409-observacao.
*          wa_provisional-dt_emissao       = wa_zsdt0409-dt_emissao.

          if wa_provisional-dt_ret_corretora is not initial.
            wa_provisional-dia_emissao_ret = ( wa_provisional-dt_ret_corretora - wa_provisional-dt_emissao ).
            wa_provisional-dia_ret_pgto = ( wa_provisional-dt_pgto_pi - wa_provisional-dt_ret_corretora ).
          endif.
        endif.
        append wa_provisional to it_result.
        clear: wa_provisional, wa_zsdt0409, wa_dados_adiant.
      endloop.
    endif.

  endmethod.


  method save_memor_data_provisional.
    data: it_ZSDT0409 type table of zsdt0409.
    clear: e_return.

    check i_data is not initial.

    move-corresponding i_data to it_ZSDT0409.
*    sort it_ZSDT0409 by dt_ret_corretora.
*    delete it_ZSDT0409 where dt_ret_corretora eq space.

    modify zsdt0409 from table it_ZSDT0409.
    commit work.
    if sy-subrc eq 0.
      e_return = abap_true.
    endif.
  endmethod.
ENDCLASS.

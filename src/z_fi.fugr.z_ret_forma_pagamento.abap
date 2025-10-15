function z_ret_forma_pagamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"     REFERENCE(P_LIFNR) TYPE  LIFNR
*"     REFERENCE(P_ZLSCH) TYPE  DZLSCH OPTIONAL
*"     REFERENCE(P_VALOR) TYPE  NETWR_FP OPTIONAL
*"     REFERENCE(P_COTACAO) TYPE  UKURSM DEFAULT 1
*"     REFERENCE(P_BVTYP) TYPE  BVTYP DEFAULT '0001'
*"     REFERENCE(P_WAERS) TYPE  WAERS OPTIONAL
*"  EXPORTING
*"     VALUE(P_FORMA_PAGAMENTO) TYPE  DZLSCH
*"     VALUE(P_PRINC_BNC_EMP) TYPE  HBKID
*"     VALUE(P_INFO_FORNECEDOR) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(P_BANCO_FORNECEDOR) LIKE  LFBK STRUCTURE  LFBK
*"  EXCEPTIONS
*"      NAO_FORNECEDOR
*"      FORNECEDOR_CONTA
*"      FORNECEDOR_BANCO
*"      FAIXA_VALOR
*"      BANCO_EMPRESA
*"----------------------------------------------------------------------

  data: lc_bankl type bankl.
  data: rg_hbkid type range of hbkid,
        wa_hbkid like line of rg_hbkid,
        lc_lifnr type lfa1-lifnr.

  select single * into @data(wa_zfit0143) from zfit0143.

  case sy-subrc.
    when 0.

      select single * into @data(wa_t001)
        from t001
       where bukrs eq @p_bukrs.

      check wa_t001-land1 eq 'BR'.
      lc_lifnr = p_lifnr.
      select single * into @data(wa_lfa1)
        from lfa1
       where lifnr eq @p_lifnr.
                                                            "BUG 192981
      if wa_lfa1-lnrza is not initial.
        lc_lifnr = wa_lfa1-lnrza.
        select single * into wa_lfa1
          from lfa1
         where lifnr eq lc_lifnr.
      endif.
                                                            "BUG 192981

      "Somente Validar Banco Fornecedor se for diferente de Boleto e Fatura de Consumo
      if p_zlsch ne zcl_miro=>st_forma_pagamento_boleto     and
         p_zlsch ne zcl_miro=>st_forma_pagamento_ft_consumo.
        if sy-subrc is initial.
          select single * into @data(wa_lfbk)
            from lfbk
           where lifnr eq @lc_lifnr
             and bvtyp eq @p_bvtyp.

          if not sy-subrc is initial.
            message e005 raising fornecedor_conta with lc_lifnr p_bvtyp 'Favor entrar com uma prioridade de banco!'.
          endif.
          vg_banco_fornecedor = wa_lfbk-bankl(3).
        else.
          message e004 raising nao_fornecedor with lc_lifnr.
        endif.
      endif.

      if p_zlsch eq zcl_miro=>st_forma_pagamento_boleto.

        "Verificar Banco para a Forma de Pagamento selecionada
        select single * into @wa_zfit0143
          from zfit0143
         where bukrs eq @p_bukrs
           and zlsch eq @p_zlsch.

        if sy-subrc is not initial.
          message e062(zfi) with p_bukrs raising banco_empresa.
        endif.

        p_forma_pagamento   = p_zlsch.
        p_princ_bnc_emp     = wa_zfit0143-hbkid.
        p_info_fornecedor   = wa_lfa1.
        p_banco_fornecedor  = wa_lfbk.

      elseif p_zlsch eq zcl_miro=>st_forma_pagamento_ft_consumo.

        "Verificar Banco para a Forma de Pagamento selecionada
        select single * into @wa_zfit0143
          from zfit0143
         where bukrs eq @p_bukrs
           and zlsch eq @p_zlsch.

        if sy-subrc is not initial.
          message e071(zfi) with p_bukrs raising banco_empresa.
        endif.

        p_forma_pagamento   = p_zlsch.
        p_princ_bnc_emp     = wa_zfit0143-hbkid.
        p_info_fornecedor   = wa_lfa1.
        p_banco_fornecedor  = wa_lfbk.

      elseif p_zlsch eq zcl_miro=>st_forma_pagamento_ted.

        "Verificar Banco para a Forma de Pagamento selecionada
        select single * into @wa_zfit0143
          from zfit0143
         where bukrs eq @p_bukrs
           and zlsch eq @p_zlsch.

        if sy-subrc is not initial.
          message e063(zfi) with p_bukrs raising banco_empresa.
        endif.

        p_forma_pagamento  = p_zlsch.
        p_princ_bnc_emp    = wa_zfit0143-hbkid.
        p_info_fornecedor  = wa_lfa1.
        p_banco_fornecedor = wa_lfbk.

      elseif p_zlsch eq zcl_miro=>st_forma_pagamento_tranf.

        lc_bankl = vg_banco_fornecedor && '%'.

        if vg_banco_fornecedor is not initial.

          select * into table @data(it_t012)
            from t012
           where bukrs eq @p_bukrs
             and bankl like @lc_bankl.

          if sy-subrc is initial.
            loop at it_t012 into data(wa_t012).
              clear: wa_hbkid.
              wa_hbkid-sign   = 'I'.
              wa_hbkid-option = 'EQ'.
              wa_hbkid-high   = wa_t012-hbkid.
              wa_hbkid-low    = wa_t012-hbkid.
              append wa_hbkid to rg_hbkid.
            endloop.

            "Verificar Banco para a Forma de Pagamento selecionada
            select single * into @wa_zfit0143
              from zfit0143
             where bukrs eq @p_bukrs
               and zlsch eq @p_zlsch
               and hbkid in @rg_hbkid.
          endif.
        else.
          sy-subrc = 1.
        endif.

        if sy-subrc is not initial.
          message e064(zfi) with p_bukrs vg_banco_fornecedor raising banco_empresa.
        endif.

        p_forma_pagamento   = p_zlsch.
        p_info_fornecedor   = wa_lfa1.
        p_princ_bnc_emp     = wa_zfit0143-hbkid.
        p_banco_fornecedor  = wa_lfbk.

      elseif p_zlsch is not initial.
        message e065(zfi) raising banco_empresa.
      endif.

      check p_zlsch is initial.

      if vg_banco_fornecedor is not initial.

        lc_bankl = vg_banco_fornecedor && '%'.

        select * into table @it_t012
          from t012
         where bukrs eq @p_bukrs
           and bankl like @lc_bankl.

        if sy-subrc is initial.

          loop at it_t012 into wa_t012.
            clear: wa_hbkid.
            wa_hbkid-sign   = 'I'.
            wa_hbkid-option = 'EQ'.
            wa_hbkid-high   = wa_t012-hbkid.
            wa_hbkid-low    = wa_t012-hbkid.
            append wa_hbkid to rg_hbkid.
          endloop.

          "Procurar Forma de Pagamento Transferência
          select single * into @wa_zfit0143
            from zfit0143
           where bukrs eq @p_bukrs
             and zlsch eq @zcl_miro=>st_forma_pagamento_tranf
             and hbkid in @rg_hbkid.

          if sy-subrc is initial.
            p_forma_pagamento   = wa_zfit0143-zlsch.
            p_info_fornecedor   = wa_lfa1.
            p_princ_bnc_emp     = wa_zfit0143-hbkid.
            p_banco_fornecedor  = wa_lfbk.
          endif.
        endif.

      else.
        sy-subrc = 1.
      endif.

      check sy-subrc is not initial.

      select single * into @wa_zfit0143
        from zfit0143
       where bukrs eq @p_bukrs
         and zlsch eq @zcl_miro=>st_forma_pagamento_ted.

      if sy-subrc is initial.
        p_info_fornecedor   = wa_lfa1.
        p_banco_fornecedor  = wa_lfbk.
        p_forma_pagamento   = wa_zfit0143-zlsch.
        p_princ_bnc_emp     = wa_zfit0143-hbkid.
      endif.

      check sy-subrc is not initial.

      message e066(zfi) raising banco_empresa.

    when others.

      clear: p_forma_pagamento, p_princ_bnc_emp, p_info_fornecedor, p_banco_fornecedor.

      if p_waers eq 'USD'.

        p_forma_pagamento = 'E'.

        select *
          into table it_zfit0011
          from zfit0011
         where bukrs eq p_bukrs.

        if sy-subrc is initial.
          read table it_zfit0011 into wa_zfit0011 index 1.
          p_princ_bnc_emp = wa_zfit0011-hbkid.
        else.
          message e009 raising banco_empresa with p_bukrs.
        endif.

      else.
        select single * into p_info_fornecedor
          from lfa1
         where lifnr eq p_lifnr.
        lc_lifnr = p_lifnr.
                                                            "BUG 192981
        if wa_lfa1-lnrza is not initial.
          lc_lifnr = wa_lfa1-lnrza.
          select single * into wa_lfa1
            from lfa1
           where lifnr eq lc_lifnr.
        endif.

        if sy-subrc is initial.

          select single * into p_banco_fornecedor
            from lfbk
           where lifnr eq lc_lifnr
             and bvtyp eq p_bvtyp.

          if not sy-subrc is initial.
            message e005 raising fornecedor_conta with lc_lifnr p_bvtyp 'Favor entrar com uma prioridade de banco!'.
          else.
            vg_banco_fornecedor = p_banco_fornecedor-bankl(4).
          endif.
        else.
          message e004 raising nao_fornecedor with lc_lifnr.
        endif.

        select *
          into table it_zfit0011
          from zfit0011
         where bukrs eq p_bukrs.

        "Se existe bancos da empresa
        if sy-subrc is initial.

          "Valor em Reais
          vg_valor_movimento = p_valor * p_cotacao.

          loop at it_zfit0011 into wa_zfit0011.
            if wa_zfit0011-banco eq vg_banco_fornecedor.
              p_forma_pagamento = c_u.
              p_princ_bnc_emp   = wa_zfit0011-hbkid.
            elseif p_forma_pagamento is initial and
                   vg_valor_movimento ge wa_zfit0011-netwr and
                   vg_valor_movimento le wa_zfit0011-netwm.
              p_forma_pagamento = wa_zfit0011-zlsch.
              p_princ_bnc_emp   = wa_zfit0011-hbkid.
            endif.
          endloop.

          if p_forma_pagamento is initial.
            message e008 raising faixa_valor with vg_valor_movimento vg_banco_fornecedor p_bukrs.
          endif.

        else.
          message e009 raising banco_empresa with p_bukrs.
        endif.
      endif.
  endcase.


endfunction.
